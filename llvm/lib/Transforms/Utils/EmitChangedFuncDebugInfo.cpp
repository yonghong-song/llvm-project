//==- EmitChangedFuncDebugInfoPass - Emit Additional Debug Info -*- C++ -*-==//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This pass synthesizes a "shadow" DISubprogram carrying a *possibly changed*
// signature for certain optimized functions. The new subprogram lives in a
// dedicated DICompileUnit whose file name is "<changed_signatures>", and is
// attached to a dummy AvailableExternally function so that the metadata forms
// a valid graph.
//
// When we can recover argument names/types from dbg records in the entry
// block, we do so; otherwise we conservatively fall back to pointer- or
// integer-typed parameters and mark the new subprogram's calling convention
// as DW_CC_nocall.
//
// We *only* run for C-family source languages, skip BPF targets (BTF is used
// there), skip varargs originals, and skip functions whose return type is a
// large by-value aggregate.
//
// Functionality is intentionally unchanged from the provided version; changes
// are limited to comments, naming, and small refactors for clarity.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/EmitChangedFuncDebugInfo.h"

#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h" // for cl::opt
#include "llvm/TargetParser/Triple.h"

using namespace llvm;

/// Command-line switch to completely disable this pass' behavior.
static cl::opt<bool> DisableChangedFuncDBInfo(
    "disable-changed-func-dbinfo", cl::Hidden, cl::init(false),
    cl::desc("Disable debuginfo emission for changed func signatures"));

/// ==========================================================================
/// Small helpers
/// ==========================================================================

/// Strip away qualifiers/typedefs/const/volatile/etc. through derived types
/// and stop at the first pointer (returning that pointer) or at the base
/// non-derived type if no pointers appear.
static DIType *stripToBaseOrFirstPointer(DIType *T) {
  while (auto *DT = dyn_cast_or_null<DIDerivedType>(T)) {
    if (DT->getTag() == dwarf::DW_TAG_pointer_type)
      return DT;
    T = DT->getBaseType();
  }
  return T;
}

/// ==========================================================================
/// Type utilities
/// ==========================================================================

/// Create a signed integer basic DIType with a canonical C-ish name based
/// solely on size. This mirrors the previous behavior exactly.
static DIType *createBasicType(DIBuilder &DIB, uint64_t SizeInBits) {
  switch (SizeInBits) {
  case 8:
    return DIB.createBasicType("char", 8, dwarf::DW_ATE_signed);
  case 16:
    return DIB.createBasicType("short", 16, dwarf::DW_ATE_signed);
  case 32:
    return DIB.createBasicType("int", 32, dwarf::DW_ATE_signed);
  case 64:
    return DIB.createBasicType("long long", 64, dwarf::DW_ATE_signed);
  default:
    return DIB.createBasicType("__int128", SizeInBits, dwarf::DW_ATE_signed);
  }
}

/// Create a floating-point basic DIType based on bit width.
static DIType *createFloatType(DIBuilder &DIB, uint64_t SizeInBits) {
  if (SizeInBits == 32)
    return DIB.createBasicType("float", 32, dwarf::DW_ATE_float);
  if (SizeInBits == 64)
    return DIB.createBasicType("double", 64, dwarf::DW_ATE_float);
  // 80/128 etc. are represented as "long double" here per the original code.
  return DIB.createBasicType("long double", SizeInBits, dwarf::DW_ATE_float);
}

/// For a struct/union parameter split into fragments, derive an integer DIType
/// for the fragment described by Expr. If a member with matching (offset,size)
/// exists, reuse its base type; otherwise synthesize an int type of BitSize.
static DIType *getIntTypeFromExpr(DIBuilder &DIB, DIExpression *Expr,
                                  DICompositeType *DTy) {
  for (auto Op : Expr->expr_ops()) {
    if (Op.getOp() != dwarf::DW_OP_LLVM_fragment)
      continue;

    const uint64_t BitOffset = Op.getArg(0);
    const uint64_t BitSize = Op.getArg(1);

    for (auto *Element : DTy->getElements()) {
      if (auto *Elem = dyn_cast<DIDerivedType>(Element)) {
        if (Elem->getSizeInBits() == BitSize &&
            Elem->getOffsetInBits() == BitOffset)
          return Elem->getBaseType();
      }
    }
    // No matching member; synthesize an integer of the fragment size.
    return createBasicType(DIB, BitSize);
  }
  return nullptr;
}

/// Compute the DIType to use for parameter with IR type `Ty` and original
/// debug type `Orig` (after stripping qualifiers / stopping at first pointer).
/// If we coerce a composite to a fragment or wrap it in a pointer, NeedSuffix
/// is set to true so the caller may decorate the argument name.
static DIType *computeParamDIType(DIBuilder &DIB, Type *Ty, DIType *Orig,
                                  unsigned PointerBitWidth, DIExpression *Expr,
                                  bool &NeedSuffix) {
  NeedSuffix = false;
  DIType *Stripped = stripToBaseOrFirstPointer(Orig);

  if (Ty->isIntegerTy()) {
    if (auto *Comp = dyn_cast_or_null<DICompositeType>(Stripped)) {
      if (!Ty->isIntegerTy(Comp->getSizeInBits())) {
        if (DIType *Frag = getIntTypeFromExpr(DIB, Expr, Comp)) {
          NeedSuffix = true; // composite coerced into integer fragment
          return Frag;
        }
      }
      // If sizes match, fall through and synthesize by width.
    }
    const unsigned W = cast<IntegerType>(Ty)->getBitWidth();
    return createBasicType(DIB, W);
  }

  if (Ty->isPointerTy()) {
    if (auto *Comp = dyn_cast_or_null<DICompositeType>(Stripped)) {
      NeedSuffix = true; // struct turned into pointer-to-struct
      return DIB.createPointerType(Comp, PointerBitWidth);
    }
    if (auto *Der = dyn_cast_or_null<DIDerivedType>(Stripped)) {
      if (Der->getTag() == dwarf::DW_TAG_pointer_type)
        return Der; // Already a pointer in DI.
    }
    // Generic pointer -> pointer to pointer-sized int.
    DIType *Base = createBasicType(DIB, PointerBitWidth);
    return DIB.createPointerType(Base, PointerBitWidth);
  }

  if (Ty->isFloatingPointTy())
    return createFloatType(DIB, Ty->getScalarSizeInBits());

  // Default: pointer-sized int (matches previous behavior).
  return createBasicType(DIB, PointerBitWidth);
}

/// Synthesize a DI type/name for a parameter if we cannot match via dbg
/// records. Integer stays integer; everything else becomes pointer to int.
static DIType *
fallbackParam(DIBuilder &DIB, Function *F, unsigned Idx, unsigned PtrW) {
  Type *Ty = F->getArg(Idx)->getType();
  const unsigned W =
      Ty->isIntegerTy() ? cast<IntegerType>(Ty)->getBitWidth() : 32;
  DIType *BaseInt = createBasicType(DIB, W);
  DIType *ParamTy =
      Ty->isIntegerTy() ? BaseInt : DIB.createPointerType(BaseInt, PtrW);
  return ParamTy;
}

/// Aggregate (struct/union) larger than pointer width?
static bool isLargeByValueAggregate(DIType *T, unsigned PtrW) {
  DIType *P = stripToBaseOrFirstPointer(T);
  if (auto *Comp = dyn_cast_or_null<DICompositeType>(P))
    return Comp->getSizeInBits() > PtrW;
  return false;
}

/// ==========================================================================
/// Argument collection
/// ==========================================================================

/// Try to recover one argument's DI name/type by scanning dbg records in the
/// entry block. Covers:
///   * direct SSA use of the argument,
///   * alloca-based byval lowering (matched by dbg name),
///   * zext(i1)->i8 adjacent-use pattern.
/// Falls back to a synthesized parameter if nothing matches.
static bool getOneArgDI(unsigned Idx, BasicBlock &Entry, DIBuilder &DIB,
                        Function *F, DISubprogram *OldSP,
                        SmallVectorImpl<Metadata *> &TypeList,
                        SmallVectorImpl<Metadata *> &ArgList,
                        unsigned PointerBitWidth) {
  Argument *Arg = F->getArg(Idx);
  StringRef ArgName = Arg->getName();
  Type *ArgTy = Arg->getType();

  for (Instruction &I : Entry) {
    for (DbgRecord &DR : I.getDbgRecordRange()) {
      auto *DVR = dyn_cast<DbgVariableRecord>(&DR);
      if (!DVR)
        continue;

      auto *VAM = dyn_cast_or_null<ValueAsMetadata>(DVR->getRawLocation());
      if (!VAM)
        continue;

      Value *LocV = VAM->getValue();
      if (!LocV)
        continue;

      auto *Var = DVR->getVariable();
      if (!Var || !Var->getArg())
        continue;

      // Strip qualifiers until we encounter a pointer (which we keep).
      DIType *DITy = Var->getType();
      while (auto *DTy = dyn_cast<DIDerivedType>(DITy)) {
        if (DTy->getTag() == dwarf::DW_TAG_pointer_type) {
          DITy = DTy;
          break;
        }
        DITy = DTy->getBaseType();
      }

      // (1) Direct SSA match, or
      // (2) alloca/byval (matched by dbg name), or
      // (3) zext(i1) -> i8 (adjacent predecessor instruction) pattern.
      bool Matched = (LocV == Arg);
      if (!Matched) {
        if (isa<AllocaInst>(LocV)) {
          if (Var->getName() != ArgName)
            continue;
          Matched = true;
        } else if (Instruction *Prev = I.getPrevNode()) {
          if (auto *ZExt = dyn_cast<ZExtInst>(Prev))
            Matched = (ZExt->getOperand(0) == Arg && LocV == Prev);
          if (!Matched)
            continue;
        } else {
          continue;
        }
      }

      bool NeedSuffix = false;
      DIType *ParamType =
          computeParamDIType(DIB, ArgTy, Var->getType(), PointerBitWidth,
                             DVR->getExpression(), NeedSuffix);
      if (!ParamType)
        return false;

      TypeList.push_back(ParamType);
      auto *NewVar = DIB.createParameterVariable(OldSP, ArgName,
                                                 Idx + 1, OldSP->getFile(),
                                                 OldSP->getLine(), ParamType);
      ArgList.push_back(NewVar);
      return true;
    }
  }

  // Fallback (unused/poison argument path).
  DIType *ParamTy = fallbackParam(DIB, F, Idx, PointerBitWidth);
  TypeList.push_back(ParamTy);
  auto *NewVar =
      DIB.createParameterVariable(OldSP, ArgName, Idx + 1,
                                  OldSP->getFile(), OldSP->getLine(), ParamTy);
  ArgList.push_back(NewVar);
  return true;
}

/// Collect return and all parameter DI types/variables for `F`. The return
/// type is copied from the original subprogram unless void.
static bool collectReturnAndArgs(DIBuilder &DIB, Function *F,
                                 DISubprogram *OldSP,
                                 SmallVectorImpl<Metadata *> &TypeList,
                                 SmallVectorImpl<Metadata *> &ArgList,
                                 unsigned PointerBitWidth) {
  FunctionType *FTy = F->getFunctionType();
  Type *RetTy = FTy->getReturnType();

  if (RetTy->isVoidTy()) {
    TypeList.push_back(nullptr);
  } else {
    // Non-void return type is assumed unchanged by optimization.
    DITypeRefArray TyArray = OldSP->getType()->getTypeArray();
    TypeList.push_back(TyArray[0]);
  }

  BasicBlock &Entry = F->getEntryBlock();
  for (unsigned i = 0, n = FTy->getNumParams(); i < n; ++i) {
    if (!getOneArgDI(i, Entry, DIB, F, OldSP, TypeList, ArgList,
                     PointerBitWidth))
      return false;
  }
  return true;
}

/// ==========================================================================
/// Per-function transform
/// ==========================================================================

/// Find an existing "<changed_signatures>" DICompileUnit if present.
static DICompileUnit *findChangedSigCU(Module &M) {
  if (NamedMDNode *CUs = M.getNamedMetadata("llvm.dbg.cu")) {
    for (MDNode *Node : CUs->operands()) {
      auto *CU = cast<DICompileUnit>(Node);
      if (CU->getFile()->getFilename() == "<changed_signatures>")
        return CU;
    }
  }
  return nullptr;
}

static void generateDebugInfo(Module &M, Function *F,
                              unsigned PointerBitWidth) {
  // A DIBuilder scoped to this function's work; we seed it with an existing
  // "<changed_signatures>" CU if one already exists.
  DICompileUnit *NewCU = findChangedSigCU(M);
  DIBuilder DIB(M, /*AllowUnresolved=*/false, NewCU);

  DISubprogram *OldSP = F->getSubprogram();
  DIFile *NewFile;

  if (NewCU) {
    NewFile = NewCU->getFile();
  } else {
    // Create the dedicated CU and its file on first use, mirroring fields
    // from the original CU (producer, flags, opt level, runtimeVersion, ...).
    DICompileUnit *OldCU = OldSP->getUnit();
    DIFile *OldFile = OldCU->getFile();
    NewFile = DIB.createFile("<changed_signatures>", OldFile->getDirectory());
    NewCU = DIB.createCompileUnit(
        OldCU->getSourceLanguage(), NewFile, OldCU->getProducer(),
        OldCU->isOptimized(), OldCU->getFlags(), OldCU->getRuntimeVersion());
  }

  SmallVector<Metadata *, 5> TypeList, ArgList;
  const bool Success =
      collectReturnAndArgs(DIB, F, OldSP, TypeList, ArgList, PointerBitWidth);

  // Build the new subroutine type + retained parameter variables.
  DITypeRefArray DITypeArray = DIB.getOrCreateTypeArray(
      TypeList.empty() ? ArrayRef<Metadata *>{nullptr} : TypeList);
  auto *SubroutineType = DIB.createSubroutineType(DITypeArray);
  DINodeArray ArgArray = DIB.getOrCreateArray(ArgList);

  // Create a dummy function to anchor the new DISubprogram.
  Function *DummyF = Function::Create(F->getFunctionType(),
                                      GlobalValue::AvailableExternallyLinkage,
                                      F->getName() + ".newsig", &M);

  DISubprogram *NewSP =
      DIB.createFunction(OldSP,                 // Scope
                         OldSP->getName(),      // Name
                         F->getName(),          // Linkage name
                         NewFile,               // File
                         OldSP->getLine(),      // Line
                         SubroutineType,        // Type
                         OldSP->getScopeLine(), // ScopeLine
                         DINode::FlagZero,      // Flags (unchanged)
                         DISubprogram::SPFlagDefinition);
  NewSP->replaceRetainedNodes(ArgArray);

  // If argument recovery failed, mark the new signature as nocall.
  if (!Success) {
    auto Temp = NewSP->getType()->cloneWithCC(llvm::dwarf::DW_CC_nocall);
    NewSP->replaceType(MDNode::replaceWithPermanent(std::move(Temp)));
  }

  DIB.finalizeSubprogram(NewSP);

  // Add a trivial body so the subprogram is "defined".
  BasicBlock *BB = BasicBlock::Create(M.getContext(), "entry", DummyF);
  IRBuilder<> IRB(BB);
  IRB.CreateUnreachable();

  DummyF->setSubprogram(NewSP);

  // Finalize DIBuilder emissions for this function.
  DIB.finalize();
}

/// ==========================================================================
/// Pass driver
/// ==========================================================================

PreservedAnalyses EmitChangedFuncDebugInfoPass::run(Module &M,
                                                    ModuleAnalysisManager &AM) {
  if (DisableChangedFuncDBInfo)
    return PreservedAnalyses::all();

  // C-family only.
  for (DICompileUnit *CU : M.debug_compile_units()) {
    auto L = CU->getSourceLanguage().getUnversionedName();
    if (L != dwarf::DW_LANG_C && L != dwarf::DW_LANG_C89 &&
        L != dwarf::DW_LANG_C99 && L != dwarf::DW_LANG_C11 &&
        L != dwarf::DW_LANG_C17)
      return PreservedAnalyses::all();
  }

  Triple T(M.getTargetTriple());
  if (T.isBPF()) // BPF: LLVM emits BTF; skip here.
    return PreservedAnalyses::all();

  const unsigned PointerBitWidth = T.getArchPointerBitWidth();

  SmallVector<Function *> ChangedFuncs;
  for (Function &F : M) {
    if (F.isIntrinsic() || F.isDeclaration())
      continue;

    DISubprogram *SP = F.getSubprogram();
    if (!SP)
      continue;

    DITypeRefArray TyArray = SP->getType()->getTypeArray();

    // Skip if return is a large aggregate (> pointer size).
    {
      DIType *RetDI = stripToBaseOrFirstPointer(TyArray[0]);
      if (auto *Comp = dyn_cast_or_null<DICompositeType>(RetDI))
        if (Comp->getSizeInBits() > PointerBitWidth)
          continue;
    }

    // Skip original varargs.
    if (TyArray.size() > 1 && TyArray[TyArray.size() - 1] == nullptr)
      continue;

    // Consider signature "changed" if any arg is a large by-value aggregate.
    bool SigChanged = false;
    if (!F.getName().contains('.')) {
      uint8_t CC = SP->getType()->getCC();
      if (CC != dwarf::DW_CC_nocall) {
        for (unsigned i = 1; i < TyArray.size(); ++i) {
          if (isLargeByValueAggregate(TyArray[i], PointerBitWidth)) {
            SigChanged = true;
            break;
          }
        }
        if (!SigChanged)
          continue;
      }
    }

    ChangedFuncs.push_back(&F);
  }

  for (Function *F : ChangedFuncs)
    generateDebugInfo(M, F, PointerBitWidth);

  return ChangedFuncs.empty() ? PreservedAnalyses::all()
                              : PreservedAnalyses::none();
}
