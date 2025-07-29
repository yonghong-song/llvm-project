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
// block, we do so; otherwise this particular function will be skipped.
// Remarks can show what functions to be skipped.
//
// We *only* run for C-family source languages, only support x86_64/arm64/risc64
// /s390), skip varargs originals, and skip functions whose return type is a
// large by-value aggregate, etc.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/EmitChangedFuncDebugInfo.h"

#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/TargetParser/Triple.h"

using namespace llvm;

#define DEBUG_TYPE "emit-changed-func-debuginfo"

// Disable/Enable switch.
static cl::opt<bool> EnableChangedFuncDBInfo(
    "enable-changed-func-dbinfo", cl::Hidden, cl::init(false),
    cl::desc("Enable debuginfo emission for changed func signatures"));

static cl::opt<bool> SkipDottedFunc("skip-dotted-func", cl::Hidden,
                                    cl::init(false),
                                    cl::desc("Skip functions with dot"));

// Replace all '.' with "__" (stable with opaque-lifetime inputs).
static std::string sanitizeDots(StringRef S) {
  std::string Out = S.str();
  for (size_t pos = 0; (pos = Out.find('.', pos)) != std::string::npos;
       pos += 2)
    Out.replace(pos, 1, "__");
  return Out;
}

// Return the "basename" (prefix before the first '.') of a name.
static StringRef baseBeforeDot(StringRef S) {
  return S.take_front(S.find('.'));
}

// Ensure a variable name is unique among previously recorded parameters.
// If collision, append "__<Idx>".
static std::string uniquifyParamName(StringRef Candidate,
                                     ArrayRef<Metadata *> Existing,
                                     unsigned Idx) {
  for (unsigned i = 0; i < Existing.size(); ++i)
    if (auto *LV = dyn_cast<DILocalVariable>(Existing[i]))
      if (LV->getName() == Candidate)
        return (Twine(Candidate) + "__" + Twine(Idx)).str();
  return Candidate.str();
}

// Walk backward in the current block to see whether LocV matches one of
// previous insn or some operand of those insns.
static bool comesFromArgViaCast(Value *LocV, Argument *Arg, Instruction &At) {
  if (!LocV)
    return false;
  for (Instruction *Prev = At.getPrevNode(); Prev; Prev = Prev->getPrevNode()) {
    // FIXME: maybe some other insns need check as well.
    if (auto *Z = dyn_cast<ZExtInst>(Prev))
      if (Z->getOperand(0) == Arg && LocV == Prev)
        return true;
    if (auto *T = dyn_cast<TruncInst>(Prev))
      if (T->getOperand(0) == Arg && LocV == Prev)
        return true;
    if (auto *T = dyn_cast<StoreInst>(Prev))
      if (T->getOperand(0) == Arg && LocV == T->getOperand(1))
        return true;
  }
  return false;
}

// Strip qualifiers/typedefs until the first pointer-type (which we keep), or
// to the base non-derived type if no pointer is found.
static DIType *stripToBaseOrFirstPointer(DIType *T) {
  while (auto *DT = dyn_cast_or_null<DIDerivedType>(T)) {
    if (DT->getTag() == dwarf::DW_TAG_pointer_type)
      return DT;
    T = DT->getBaseType();
  }
  return T;
}

static DIType *createBasicType(DIBuilder &DIB, uint64_t SizeInBits) {
  switch (SizeInBits) {
  case 8:
    return DIB.createBasicType("signed char", 8, dwarf::DW_ATE_signed_char);
  case 16:
    return DIB.createBasicType("short", 16, dwarf::DW_ATE_signed);
  case 32:
    return DIB.createBasicType("int", 32, dwarf::DW_ATE_signed);
  case 64:
    return DIB.createBasicType("long", 64, dwarf::DW_ATE_signed);
  default:
    return DIB.createBasicType("__int128", SizeInBits, dwarf::DW_ATE_signed);
  }
}

static DIType *getIntTypeFromExpr(DIBuilder &DIB, DIExpression *Expr,
                                  DICompositeType *DTy, unsigned W) {
  for (auto Op : Expr->expr_ops()) {
    if (Op.getOp() != dwarf::DW_OP_LLVM_fragment)
      break;

    uint64_t BitOffset = Op.getArg(0);
    uint64_t BitSize = Op.getArg(1);
    uint64_t BitUpLimit = BitOffset + BitSize;

    DINodeArray Elems = DTy->getElements();
    unsigned N = Elems.size();

    for (unsigned i = 0; i < N; ++i)
      if (auto *Elem = dyn_cast<DIDerivedType>(Elems[i])) {
        if (N >= 2 && i < N - 1) {
          if (Elem->getOffsetInBits() <= BitOffset &&
              BitUpLimit <= (Elem->getOffsetInBits() + Elem->getSizeInBits()))
            return Elem->getBaseType();
        } else {
          if (Elem->getOffsetInBits() <= BitOffset &&
              BitUpLimit <= DTy->getSizeInBits())
            return Elem->getBaseType();
        }
      }

    return createBasicType(DIB, BitSize);
  }
  return createBasicType(DIB, W);
}

static DIType *computeParamDIType(DIBuilder &DIB, Type *Ty, DIType *Orig,
                                  unsigned PointerBitWidth, DIExpression *Expr,
                                  bool Coerce16B, bool ByVal) {
  DIType *Stripped = stripToBaseOrFirstPointer(Orig);
  unsigned TyBitSize = Stripped->getSizeInBits();

  if (TyBitSize <= 64)
    return Stripped;

  if (TyBitSize > 128 || !Coerce16B || ByVal)
    return DIB.createPointerType(Stripped, PointerBitWidth);

  auto *Comp = cast<DICompositeType>(Stripped);
  if (Comp->getTag() == dwarf::DW_TAG_union_type)
    return nullptr;

  unsigned W = cast<IntegerType>(Ty)->getBitWidth();
  return getIntTypeFromExpr(DIB, Expr, Comp, W);
}

static bool isLargeByValueAggregate(DIType *T, unsigned PtrW) {
  DIType *P = stripToBaseOrFirstPointer(T);
  if (auto *Comp = dyn_cast_or_null<DICompositeType>(P))
    return Comp->getSizeInBits() > PtrW;
  return false;
}

static void pushParam(DIBuilder &DIB, DISubprogram *OldSP,
                      SmallVectorImpl<Metadata *> &TypeList,
                      SmallVectorImpl<Metadata *> &ArgList, DIType *Ty,
                      StringRef VarName, unsigned Idx) {
  TypeList.push_back(Ty);
  ArgList.push_back(DIB.createParameterVariable(
      OldSP, VarName, Idx + 1, OldSP->getFile(), OldSP->getLine(), Ty));
}

// Argument collection.
static bool getOneArgDI(unsigned Idx, BasicBlock &Entry, DIBuilder &DIB,
                        Function *F, DISubprogram *OldSP,
                        SmallVectorImpl<Metadata *> &TypeList,
                        SmallVectorImpl<Metadata *> &ArgList,
                        unsigned PointerBitWidth, bool Coerce16B) {
  Argument *Arg = F->getArg(Idx);
  StringRef ArgName = Arg->getName();
  Type *ArgTy = Arg->getType();

  // If byval struct, remember its identified-name and kind to match via dbg.
  StringRef ByValUserName;
  bool IsByValStruct = true;
  if (ArgTy->isPointerTy() && Arg->hasByValAttr()) {
    if (Type *ByValTy = F->getParamByValType(Idx))
      if (auto *ST = dyn_cast<StructType>(ByValTy)) {
        auto [Kind, Name] = ST->getName().split('.');
        ByValUserName = Name;
        IsByValStruct = (Kind == "struct");
      }
  }

  DILocalVariable *DIVar = nullptr;
  DIExpression *DIExpr = nullptr;

  // Scan the entry block for dbg records.
  for (Instruction &I : Entry) {
    bool Final = false;

    for (DbgRecord &DR : I.getDbgRecordRange()) {
      auto *DVR = dyn_cast<DbgVariableRecord>(&DR);
      if (!DVR)
        continue;

      auto *VAM = dyn_cast_or_null<ValueAsMetadata>(DVR->getRawLocation());
      if (!VAM)
        continue;

      Value *LocV = VAM->getValue();
      auto *Var = DVR->getVariable();
      if (!Var || !Var->getArg())
        continue;

      // Canonicalize through derived types stopping at first pointer.
      DIType *DITy = Var->getType();
      while (auto *DTy = dyn_cast<DIDerivedType>(DITy)) {
        if (DTy->getTag() == dwarf::DW_TAG_pointer_type) {
          DITy = DTy;
          break;
        }
        DITy = DTy->getBaseType();
      }

      if (LocV == Arg) {
        DIVar = Var;
        DIExpr = DVR->getExpression();
        Final = true;
        break;
      }

      // Compare base names (before dot) in several cases.
      StringRef ArgBase = baseBeforeDot(ArgName);
      StringRef VarBase = baseBeforeDot(Var->getName());

      if (ArgName.empty()) {
        if (!ByValUserName.empty()) {
          // Match by byval struct DI type’s name/kind.
          DIType *Stripped = stripToBaseOrFirstPointer(Var->getType());
          auto *Comp = dyn_cast<DICompositeType>(Stripped);
          if (!Comp)
            continue;
          bool IsStruct = Comp->getTag() == dwarf::DW_TAG_structure_type;
          if (Comp->getName() != ByValUserName || IsStruct != IsByValStruct)
            continue;
          DIVar = Var;
          DIExpr = DVR->getExpression();
          Final = true;
          break;
        }

        if (isa<PoisonValue>(LocV))
          continue;

        if (comesFromArgViaCast(LocV, Arg, I)) {
          DIVar = Var;
          DIExpr = DVR->getExpression();
          Final = true;
          break;
        }

        if (isa<AllocaInst>(LocV))
          continue;
      } else {
        // We do have an IR arg name.
        if (isa<PoisonValue>(LocV)) {
          if (Var->getName() != ArgBase)
            continue;
          DIVar = Var;
          DIExpr = DVR->getExpression();
          // Possibly we may find a non poison value later.
        } else if (isa<AllocaInst>(LocV)) {
          if (Var->getName() != ArgName)
            continue;
          DIVar = Var;
          DIExpr = DVR->getExpression();
          Final = true;
          break;
        } else if (ArgBase == VarBase) {
          DIVar = Var;
          DIExpr = DVR->getExpression();
          Final = true;
          break;
        } else if (comesFromArgViaCast(LocV, Arg, I)) {
          DIVar = Var;
          DIExpr = DVR->getExpression();
          Final = true;
          break;
        }
      }
    }

    if (Final)
      break;
  }

  OptimizationRemarkEmitter ORE(F);

  if (!DIVar) {
    if (ArgTy->isIntegerTy()) {
      // Probably due to argument promotion, e.g., a pointer argument becomes
      // an integer. The dbg does not give a clear mapping to the argument.
      // So create an int type.
      auto *Ty = createBasicType(DIB, cast<IntegerType>(ArgTy)->getBitWidth());
      pushParam(DIB, OldSP, TypeList, ArgList, Ty,
                (Twine("__") + Twine(Idx)).str(), Idx);

      ORE.emit([&]() {
        return OptimizationRemark(DEBUG_TYPE, "FindDIVariable", F)
               << "create a new int type " << ore::NV("ArgName", Arg->getName())
               << "(" << ore::NV("ArgIndex", Idx) << ")";
      });

      return true;
    }

    ORE.emit([&]() {
      return OptimizationRemark(DEBUG_TYPE, "FindDIVariable", F)
             << "not found ptr type " << ore::NV("ArgName", Arg->getName())
             << "(" << ore::NV("ArgIndex", Idx) << ")";
    });

    return false;
  }

  // Compute parameter DI type from IR type + original debug type.
  DIType *ParamType =
      computeParamDIType(DIB, ArgTy, DIVar->getType(), PointerBitWidth, DIExpr,
                         Coerce16B, !ByValUserName.empty());
  if (!ParamType) {
    ORE.emit([&]() {
      return OptimizationRemark(DEBUG_TYPE, "GetParamDIType", F)
             << "no use union type " << ore::NV("ArgName", Arg->getName())
             << "(" << ore::NV("ArgIndex", Idx) << ")";
    });

    return false;
  }

  // Decide the parameter name (sanitize + uniquify).
  std::string VarName;
  if (ArgName.empty()) {
    VarName = sanitizeDots(DIVar->getName());
    VarName = uniquifyParamName(VarName, ArgList, Idx);
  } else {
    VarName = sanitizeDots(ArgName);
  }

  pushParam(DIB, OldSP, TypeList, ArgList, ParamType, VarName, Idx);
  return true;
}

// Collect return and parameter DI information.
static bool collectReturnAndArgs(DIBuilder &DIB, Function *F,
                                 DISubprogram *OldSP,
                                 SmallVectorImpl<Metadata *> &TypeList,
                                 SmallVectorImpl<Metadata *> &ArgList,
                                 unsigned PointerBitWidth, bool Coerce16B) {
  FunctionType *FTy = F->getFunctionType();
  Type *RetTy = FTy->getReturnType();

  if (RetTy->isVoidTy())
    TypeList.push_back(nullptr);
  else
    TypeList.push_back(OldSP->getType()->getTypeArray()[0]);

  BasicBlock &Entry = F->getEntryBlock();
  for (unsigned i = 0, n = FTy->getNumParams(); i < n; ++i)
    if (!getOneArgDI(i, Entry, DIB, F, OldSP, TypeList, ArgList,
                     PointerBitWidth, Coerce16B))
      return false;
  return true;
}

static DICompileUnit *findChangedSigCU(Module &M) {
  if (NamedMDNode *CUs = M.getNamedMetadata("llvm.dbg.cu"))
    for (MDNode *Node : CUs->operands()) {
      auto *CU = cast<DICompileUnit>(Node);
      if (CU->getFile()->getFilename() == "<changed_signatures>")
        return CU;
    }
  return nullptr;
}

static bool buildArgsForChangedSignature(
    DIBuilder &DIB, Function *F, DISubprogram *OldSP, DITypeRefArray TyArray,
    SmallVectorImpl<Metadata *> &TypeList, SmallVectorImpl<Metadata *> &ArgList,
    unsigned PointerBitWidth, bool Coerce16B) {
  // Return type.
  TypeList.push_back(TyArray[0]);

  unsigned ArgIdx = 0;
  for (unsigned i = 1; i < TyArray.size(); ++i) {
    Argument *Arg = F->getArg(ArgIdx);
    StringRef ArgName = Arg->getName();
    Type *ArgTy = Arg->getType();

    // For x86_64, a 16-byte argument may become a pointer since only
    // limited (6) registers available to pass directly. The rest of
    // arguments will pass through stack.
    bool ByVal = Arg->hasByValAttr();

    DILocalVariable *DIVar;
    for (DINode *DN : OldSP->getRetainedNodes())
      if (auto *DV = dyn_cast<DILocalVariable>(DN))
        if (DV->getArg() == i)
          DIVar = DV;

    DIType *Ty = stripToBaseOrFirstPointer(TyArray[i]);
    unsigned TyBitSize = Ty->getSizeInBits();

    if (ArgName.empty())
      ArgName = DIVar->getName();

    if (TyBitSize <= 64) {
      pushParam(DIB, OldSP, TypeList, ArgList, Ty, ArgName, ArgIdx);
      ArgIdx++;
      continue;
    }

    if (TyBitSize > 128 || ByVal || !Coerce16B) {
      Ty = DIB.createPointerType(Ty, PointerBitWidth);
      pushParam(DIB, OldSP, TypeList, ArgList, Ty, ArgName, ArgIdx);
      ArgIdx++;
      continue;
    }

    // Two arguments (struct or union)
    DICompositeType *CompTy = cast<DICompositeType>(Ty);
    if (CompTy->getTag() == dwarf::DW_TAG_union_type)
      return false;

    for (auto Element : CompTy->getElements()) {
      // dwarf::DW_TAG_member
      auto *DDTy = cast<DIDerivedType>(Element);
      if (DDTy->getOffsetInBits() == 0) {
        auto *BaseTy = stripToBaseOrFirstPointer(DDTy->getBaseType());
        if (BaseTy->getSizeInBits() == 64)
          Ty = BaseTy;
        else
          Ty = createBasicType(DIB, 64);

        std::string Owned = (llvm::Twine(ArgName) + "__coerce0").str();
        ArgName = Owned;

        pushParam(DIB, OldSP, TypeList, ArgList, Ty, ArgName, ArgIdx);
        ArgIdx++;
      } else if (DDTy->getOffsetInBits() == 64) {
        Arg = F->getArg(ArgIdx);
        ArgName = Arg->getName();
        ArgTy = Arg->getType();

        if (ArgName.empty())
          ArgName = DIVar->getName();

        auto *BaseTy = stripToBaseOrFirstPointer(DDTy->getBaseType());
        if (BaseTy->getSizeInBits() == 64)
          Ty = BaseTy;
        else
          Ty = createBasicType(DIB, cast<IntegerType>(ArgTy)->getBitWidth());

        std::string Owned = (llvm::Twine(ArgName) + "__coerce1").str();
        ArgName = Owned;
        pushParam(DIB, OldSP, TypeList, ArgList, Ty, ArgName, ArgIdx);
        ArgIdx++;
        break;
      } else if (DDTy->getOffsetInBits() > 64) {
        Arg = F->getArg(ArgIdx);
        ArgName = Arg->getName();
        ArgTy = Arg->getType();

        if (ArgName.empty())
          ArgName = DIVar->getName();

        Ty = createBasicType(DIB, cast<IntegerType>(ArgTy)->getBitWidth());

        std::string Owned = (llvm::Twine(ArgName) + "__coerce1").str();
        ArgName = Owned;
        pushParam(DIB, OldSP, TypeList, ArgList, Ty, ArgName, ArgIdx);
        ArgIdx++;
        break;
      }
    }
  }

  return true;
}

static void generateDebugInfo(Module &M, Function *F, unsigned PointerBitWidth,
                              bool Coerce16B) {
  DICompileUnit *NewCU = findChangedSigCU(M);
  DIBuilder DIB(M, /*AllowUnresolved=*/false, NewCU);

  DISubprogram *OldSP = F->getSubprogram();
  DIFile *NewFile;

  if (NewCU) {
    NewFile = NewCU->getFile();
  } else {
    DICompileUnit *OldCU = OldSP->getUnit();
    DIFile *OldFile = OldCU->getFile();
    NewFile = DIB.createFile("<changed_signatures>", OldFile->getDirectory());
    NewCU = DIB.createCompileUnit(
        OldCU->getSourceLanguage(), NewFile, OldCU->getProducer(),
        OldCU->isOptimized(), OldCU->getFlags(), OldCU->getRuntimeVersion());
  }

  SmallVector<Metadata *, 5> TypeList, ArgList;

  bool doCollect = true;
  DITypeRefArray TyArray = OldSP->getType()->getTypeArray();
  uint8_t cc = OldSP->getType()->getCC();
  if (cc != dwarf::DW_CC_nocall) {
    unsigned i;
    unsigned n = TyArray.size();
    for (i = 1; i < n; ++i) {
      if (isLargeByValueAggregate(TyArray[i], PointerBitWidth))
        break;
    }
    if (i == n)
      doCollect = false;
  }

  // doCollect true means the OldSP signature can be reused for function F.

  DISubroutineType *SubTy;
  DINodeArray ArgArray;
  StringRef FuncName, LinkageName;

  // Three cases:
  //  . OldSP signature can be reused, e.g., foo.llvm.<hash>() has the same
  //    signature as source foo().
  //  . Signature changed (not-marked with dwarf::DW_CC_nocall) due to
  //    parameter have more than 8 byte size.
  //  . Signature changed (marked with dwarf::DW_CC_nocall).
  if (cc != dwarf::DW_CC_nocall) {
    if (!doCollect) {
      SubTy = OldSP->getType();
      ArgArray = OldSP->getRetainedNodes();
      FuncName = F->getName();
    } else {
      bool Success =
          buildArgsForChangedSignature(DIB, F, OldSP, TyArray, TypeList,
                                       ArgList, PointerBitWidth, Coerce16B);
      if (!Success) {
        DIB.finalize();
        return;
      }

      DITypeRefArray DITypeArray =
          DIB.getOrCreateTypeArray(ArrayRef<Metadata *>{TypeList});
      SubTy = DIB.createSubroutineType(DITypeArray);
      ArgArray = DIB.getOrCreateArray(ArgList);
      FuncName = OldSP->getName();
      if (FuncName != F->getName())
        LinkageName = F->getName();
    }
  } else {
    bool Success = collectReturnAndArgs(DIB, F, OldSP, TypeList, ArgList,
                                        PointerBitWidth, Coerce16B);
    if (!Success) {
      DIB.finalize();
      return;
    }

    DITypeRefArray DITypeArray =
        DIB.getOrCreateTypeArray(ArrayRef<Metadata *>{TypeList});
    SubTy = DIB.createSubroutineType(DITypeArray);
    ArgArray = DIB.getOrCreateArray(ArgList);
    FuncName = OldSP->getName();
    if (FuncName != F->getName())
      LinkageName = F->getName();
  }

  DISubprogram *NewSP = DIB.createFunction(
      OldSP, FuncName, LinkageName, NewFile, OldSP->getLine(), SubTy,
      OldSP->getScopeLine(), DINode::FlagZero, DISubprogram::SPFlagDefinition);
  NewSP->replaceRetainedNodes(ArgArray);

  DIB.finalizeSubprogram(NewSP);

  // Dummy anchor function
  Function *DummyF = Function::Create(F->getFunctionType(),
                                      GlobalValue::AvailableExternallyLinkage,
                                      F->getName() + ".newsig", &M);

  // Provide a trivial body so the SP is marked as "defined".
  BasicBlock *BB = BasicBlock::Create(M.getContext(), "entry", DummyF);
  IRBuilder<> IRB(BB);
  IRB.CreateUnreachable();
  DummyF->setSubprogram(NewSP);

  DIB.finalize();
}

PreservedAnalyses EmitChangedFuncDebugInfoPass::run(Module &M,
                                                    ModuleAnalysisManager &AM) {
  if (!EnableChangedFuncDBInfo)
    return PreservedAnalyses::all();

  // Only C-family
  for (DICompileUnit *CU : M.debug_compile_units()) {
    auto L = CU->getSourceLanguage().getUnversionedName();
    if (L != dwarf::DW_LANG_C && L != dwarf::DW_LANG_C89 &&
        L != dwarf::DW_LANG_C99 && L != dwarf::DW_LANG_C11 &&
        L != dwarf::DW_LANG_C17)
      return PreservedAnalyses::all();
  }

  Triple T(M.getTargetTriple());

  // Skip BPF for now.
  if (T.isBPF())
    return PreservedAnalyses::all();

  unsigned PointerBitWidth = T.getArchPointerBitWidth();

  // If Coerce16B is true, a 16B byte original parameter will become two
  // parameters in argument list. Otherwise, the 16B byte original
  // parameter will become a pointer (similar to e.g. 24B struct parameter).
  bool Coerce16B = true;
  if (T.isSystemZ())
    Coerce16B = false;

  SmallVector<Function *> ChangedFuncs;
  for (Function &F : M) {
    if (F.isIntrinsic() || F.isDeclaration())
      continue;

    DISubprogram *SP = F.getSubprogram();
    if (!SP)
      continue;

    DITypeRefArray TyArray = SP->getType()->getTypeArray();
    if (TyArray.size() == 0)
      continue;

    // Skip varargs
    if (TyArray.size() > 1 && TyArray[TyArray.size() - 1] == nullptr)
      continue;

    // For C language, only supports int/ptr types, no support for
    // floating_point/vector.
    unsigned i = 0;
    unsigned n = F.getFunctionType()->getNumParams();
    for (i = 0; i < n; ++i) {
      Type *ArgTy = F.getArg(i)->getType();
      if (ArgTy->isVectorTy() || ArgTy->isFloatingPointTy())
        break;
    }
    if (i != n)
      continue;

    // Skip if large by-value return
    DIType *RetDI = stripToBaseOrFirstPointer(TyArray[0]);
    if (auto *Comp = dyn_cast_or_null<DICompositeType>(RetDI))
      if (Comp->getSizeInBits() > PointerBitWidth)
        continue;

    if (SkipDottedFunc && F.getName().contains('.'))
      continue;

    // Signature consider not changed if
    //   . function does not '.' (e.g., foo.llvm.<hash>), and
    //   . function without '.'/dwarf::DW_CC_nocall, and
    //   . any argument size is less than 8 bytes (PointerBitWidth).
    if (!F.getName().contains('.') &&
        SP->getType()->getCC() != dwarf::DW_CC_nocall) {
      n = TyArray.size();
      for (i = 1; i < n; ++i)
        if (isLargeByValueAggregate(TyArray[i], PointerBitWidth)) {
          break;
        }
      if (i == n)
        continue;
    }

    ChangedFuncs.push_back(&F);
  }

  for (Function *F : ChangedFuncs)
    generateDebugInfo(M, F, PointerBitWidth, Coerce16B);

  return ChangedFuncs.empty() ? PreservedAnalyses::all()
                              : PreservedAnalyses::none();
}
