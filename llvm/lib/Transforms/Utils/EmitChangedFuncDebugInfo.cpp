//==- EmitChangedFuncDebugInfoPass - Emit Additional Debug Info -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements emitting debug info for functions with changed
// signatures.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/EmitChangedFuncDebugInfo.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

using namespace llvm;

static void generateDebugInfo(Module &M, Function *F) {
  // For this CU, we want generate the following:
  // DW_TAG_compile_unit
  //   ...
  //   DW_TAG_inlined_subroutine
  //     DW_AT_name      ("foo.1")
  //     DW_AT_type      (0x0000000000000091 "int")
  //     DW_AT_artificial (true)
  //     DW_AT_specificiation (original DW_TAG_subprogram)
  //
  //     DW_TAG_formal_parameter
  //       DW_AT_name    ("b")
  //       DW_AT_type    (0x0000000000000091 "int")
  //
  //     DW_TAG_formal_parameter
  //       DW_AT_name    ("c")
  //       DW_AT_type    (0x0000000000000095 "long")
  //   ...
  //   DW_TAG_inlined_subroutine
  //     DW_AT_name      ("bar")
  //     DW_AT_type      (0x0000000000000091 "int")
  //     DW_AT_calling_convention        (DW_CC_nocall)
  //     DW_AT_artificial (true)
  //     DW_AT_specificiation (original DW_TAG_subprogram)
  //
  //     DW_TAG_formal_parameter
  //       DW_AT_name    ("c")
  //       DW_AT_type    (0x0000000000000095 "unsigned int")
  //   ...
  //
  // The DW_AT_calling_convention indicates that the function
  // signature is different from the original program.

  DISubprogram *OldSP = F->getSubprogram();
  DIBuilder DIB(M, /*AllowUnresolved=*/false, OldSP->getUnit());

  // Create function SubroutineType.
  FunctionType *FTy = F->getFunctionType();

  SmallVector<Metadata *, 3> TypeList;
  SmallVector<Metadata *, 3> ArgList;

  Type *RetTy = FTy->getReturnType();
  if (RetTy->isVoidTy()) {
    // Void return type may be due to optimization.
    DIType *VoidDIType = DIB.createBasicType("void", 0, dwarf::DW_ATE_signed);
    TypeList.push_back(VoidDIType);
  } else {
    // Optimization should not change return type from one
    // non-void type to anothe non-void type.
    DISubroutineType *SubTy = OldSP->getType();
    DITypeRefArray TyArray = SubTy->getTypeArray();
    TypeList.push_back(TyArray[0]);
  }

  unsigned NumArgs = FTy->getNumParams();
  BasicBlock &FirstBB = F->getEntryBlock();
  for (unsigned i = 0; i < NumArgs; ++i) {
    bool Found = false;
    for (Instruction &I : FirstBB) {
      for (const DbgRecord &DR : I.getDbgRecordRange()) {
        auto *DVR = dyn_cast<DbgVariableRecord>(&DR);
        if (!DVR)
          continue;
        // All of DbgVariableRecord::LocationType::{Value,Assign,Declare}
        // are covered.
        Metadata *Loc = DVR->getRawLocation();
        auto *ValueMDN = dyn_cast<ValueAsMetadata>(Loc);
        if (!ValueMDN)
          continue;

        // It is possible that argument is not represented in dbg_*(...).
        // Example 1: argument %mastter is not represented in dbg_value, need more work.
        //   define internal i32 @__netdev_upper_dev_link(ptr noundef %dev,
        //       ptr noundef %upper_dev, i1 noundef zeroext %master,
        //       ptr noundef %upper_priv, ptr noundef %upper_info,
        //       ptr noundef %priv, ptr noundef %extack) {
        //     #dbg_assign(i1 poison, !38871, !DIExpression(), !38886, ptr %2, !DIExpression(), !38887)
        //     #dbg_assign(i1 poison, !38871, !DIExpression(), !38889, ptr %6, !DIExpression(), !38890)
        //     #dbg_assign(i1 poison, !38871, !DIExpression(), !38896, ptr %10, !DIExpression(), !38897)
        //     #dbg_assign(i1 poison, !20006, !DIExpression(), !38901, ptr %14, !DIExpression(), !38902)
        //     #dbg_value(ptr %dev, !19999, !DIExpression(), !38902)
        //     #dbg_value(ptr %upper_dev, !20000, !DIExpression(), !38902)
        //   %storedv = zext i1 %master to i8
        //     #dbg_value(i8 %storedv, !20001, !DIExpression(), !38902)
        //     #dbg_value(ptr %upper_priv, !20002, !DIExpression(), !38902)
        //     #dbg_value(ptr %upper_info, !20003, !DIExpression(), !38902)
        //     #dbg_value(ptr poison, !20004, !DIExpression(), !38902)
        //     #dbg_value(ptr %extack, !20005, !DIExpression(), !38902)
        // Example 2: argument%0 is poisoned (unused)
        //   define hidden range(i32 -22, 1) i32 @efivarfs_unlink.llvm.18064083376782323959
        //       (ptr readnone captures(none) %0, ptr noundef %1) #0 align 16 !dbg !8154 {
        //     #dbg_value(ptr poison, !8156, !DIExpression(), !8159)
        //     #dbg_value(ptr %1, !8157, !DIExpression(), !8159)
        // If any argument is not represented in dbg_*(...), this function will be skipped.
        if (ValueMDN->getValue() != F->getArg(i))
          continue;

        auto *Var = cast<DILocalVariable>(DVR->getRawVariable());

        // Even we get dbg_*(...) for arguments, we still need to ensure compatible types
        // between IR func argument types and debugInfo argument types.
        // TODO: Need to generate more complex DebugInfo.
        Type *Ty = ValueMDN->getType();
        DIType *DITy = Var->getType();
        while(auto *DTy = dyn_cast<DIDerivedType>(DITy)) {
          if (DTy->getTag() == dwarf::DW_TAG_pointer_type) {
            DITy = DTy;
            break;
          }
          DITy = DTy->getBaseType();
        }

        if (Ty->isIntegerTy()) {
          if (auto *DTy = dyn_cast<DICompositeType>(DITy)) {
            if (!Ty->isIntegerTy(DTy->getSizeInBits())) {
              // TODO: for cases like
              //    static int count(struct user_arg_ptr argv, int max)
              // in fs/exec.c, and the actual func signature:
              //    i32 @count(i8 range(i8 0, 2) %argv.coerce0, ptr %argv.coerce1)
              DIB.finalize();
              return;
            }
          } else if (auto *DTy = dyn_cast<DIBasicType>(DITy)) {
            auto *IntTy = cast<IntegerType>(Ty);
            unsigned IntBitWidth = IntTy->getBitWidth();
            unsigned DTyBitWidth = DTy->getSizeInBits();

            // TODO: for cases like
            //  @walk_zones_in_node(ptr noundef %m, ptr noundef %pgdat, i1 noundef zeroext %assert_populated,
            //                      ptr noundef readonly captures(none) %print)
            //  #dbg_value(i1 %assert_populated, !11157, !DIExpression(DW_OP_LLVM_convert, 1,
            //     DW_ATE_unsigned, DW_OP_LLVM_convert, 8, DW_ATE_unsigned, DW_OP_stack_value), !11172)
            //  DI: !1138 = !DIDerivedType(tag: DW_TAG_typedef, name: "bool", file: !175, line: 35, baseType: !1139)
            //      !1139 = !DIBasicType(name: "_Bool", size: 8, encoding: DW_ATE_boolean)
            if (IntBitWidth <= 8) {
              if (DTyBitWidth != 8) {
                DIB.finalize();
                return;
              }
              // If DTyBitWidth is 8, we are good.
            }
            // All other cases (e.g., DTyBitWidth is 16/32, etc. should be okay since
            // the corresponding argument int width should be similar.
          } else {
            // TODO: missing pieces.
            continue;
          }
        } else if (Ty->isPointerTy()) {
          // TODO: A struct turned into a pointer to struct.
          if (dyn_cast<DICompositeType>(DITy)) {
            DIB.finalize();
            return;
          }
          // FIXME: how can we ganrantee both pointee types the same?
          auto *DTy = dyn_cast<DIDerivedType>(DITy);
          if (!DTy) {
            continue;
          }
          if (DTy->getTag() != dwarf::DW_TAG_pointer_type) {
            continue;
          }
        } else {
          // TODO: Missing pieces
          continue;
        }

        TypeList.push_back(Var->getType());
        if (Var->getArg() != (i + 1) || Var->getName() != F->getArg(i)->getName()) {
          Var = DIB.createParameterVariable(OldSP, F->getArg(i)->getName(),
              i + 1, OldSP->getUnit()->getFile(), OldSP->getLine(),
              Var->getType());
        }
        ArgList.push_back(Var);
        Found = true;
        break;
      }
      if (Found)
        break;
    }
    if (!Found) { /* F->dump(); */ DIB.finalize(); return; }
  }

  DITypeRefArray DITypeArray = DIB.getOrCreateTypeArray(TypeList);
  auto *SubroutineType = DIB.createSubroutineType(DITypeArray);
  DINodeArray ArgArray = DIB.getOrCreateArray(ArgList);

  Function *DummyF = Function::Create(FTy,
    GlobalValue::AvailableExternallyLinkage, F->getName() + ".newsig", &M);

  DISubprogram *NewSP = DIB.createFunction(
    OldSP,                        // Scope
    F->getName(),                 // Name
    OldSP->getLinkageName(),      // Linkage name
    OldSP->getUnit()->getFile(),  // File
    OldSP->getLine(),             // Line
    SubroutineType,               // DISubroutineType
    OldSP->getScopeLine(),        // ScopeLine
    DINode::FlagZero,
    DISubprogram::SPFlagDefinition
  );
  NewSP->replaceRetainedNodes(ArgArray);

  if (OldSP->getType()->getCC() == llvm::dwarf::DW_CC_nocall) {
    auto Temp = NewSP->getType()->cloneWithCC(llvm::dwarf::DW_CC_nocall);
    NewSP->replaceType(MDNode::replaceWithPermanent(std::move(Temp)));
  }

  DIB.finalizeSubprogram(NewSP);

  auto *NMD = M.getOrInsertNamedMetadata("llvm.dbg.sp.extra");
  NMD->addOperand(NewSP);

  // Add dummy return block
  BasicBlock *BB = BasicBlock::Create(M.getContext(), "entry", DummyF);
  IRBuilder<> IRB(BB);
  IRB.CreateUnreachable();

  DummyF->setSubprogram(NewSP);

  DIB.finalize();
}

PreservedAnalyses EmitChangedFuncDebugInfoPass::run(Module &M, ModuleAnalysisManager &AM) {
  SmallVector<Function *> ChangedFuncs;
  for (auto &F : M) {
    // Function must already have DebugInfo.
    DISubprogram *SP = F.getSubprogram();
    if (!SP)
      continue;

    // Ignore all intrinsics functions.
    if (F.isIntrinsic())
      continue;

    StringRef FName = F.getName();
    if (!FName.contains('.')) {
      uint8_t cc = SP->getType()->getCC();
      if (cc != llvm::dwarf::DW_CC_nocall)
        continue;
    }

    ChangedFuncs.push_back(&F);
  }

  bool Changed = ChangedFuncs.size() != 0;
  for (auto *F : ChangedFuncs)
    generateDebugInfo(M, F);

  return Changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}
