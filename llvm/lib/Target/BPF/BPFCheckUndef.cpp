//===---------------- BPFAdjustOpt.cpp - Adjust Optimization --------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Adjust optimization to make the code more kernel verifier friendly.
//
//===----------------------------------------------------------------------===//

#include "BPF.h"
#include "BPFCORE.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicsBPF.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/User.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include <set>

#define DEBUG_TYPE "bpf-check-undef"

using namespace llvm;

namespace {

static std::set<const Function *> FuncsWithUndef;

static bool HandleReturnInsn(Function &F, ReturnInst *I) {
  Value *RetValue = I->getReturnValue();
  if (!RetValue || isa<PoisonValue>(RetValue) || !isa<UndefValue>(RetValue))
    return false;

  dbgs() << "WARNING: return undefined value in func "
         << F.getName() << ", uninitialized variable?\n";
  FuncsWithUndef.insert(&F);
  return true;
}

static bool HandlePHINode(Function &F, PHINode *I) {
  for (Value *IncValue : I->incoming_values()) {
    if (!isa<UndefValue>(IncValue))
      continue;

    dbgs() << "WARNING: undefined value in IR/PHI node in func "
           << F.getName() << ", uninitialized variable?\n";
    FuncsWithUndef.insert(&F);
    return true;
  }

  return false;
}

static bool BPFCheckInst(Function &F, BasicBlock &BB, Instruction &I) {
  bool FoundUndef = false;
  switch(I.getOpcode()) {
  case Instruction::Ret:
    FoundUndef = HandleReturnInsn(F, cast<ReturnInst>(&I));
    break;
  case Instruction::Unreachable:
    {
    // If predecessor's last insn is a switch statement, ignore for now.
    unsigned NumNoSwitches = 0, NumSwitches = 0;
    for (BasicBlock *Pred : predecessors(&BB)) {
      const Instruction *Term = Pred->getTerminator();
      if (Term && Term->getOpcode() == Instruction::Switch) {
        NumSwitches++;
        continue;
      }
      NumNoSwitches++;
    }
    if (NumSwitches > 0 && NumNoSwitches == 0)
      break;
    }

    {
    Instruction *PrevI = I.getPrevNonDebugInstruction();
    if (PrevI) {
      auto *CI = dyn_cast<CallInst>(PrevI);
      if (CI && CI->doesNotReturn())
        break;
    }
    }

    dbgs() << "WARNING: unreachable in func "
           << F.getName() << ", due to uninitialized variable?\n";
    FuncsWithUndef.insert(&F);
    break;
  default:
#if 0
    if (isa<PHINode>(&I))
      FoundUndef = HandlePHINode(F, cast<PHINode>(&I));
#endif
    break;
  }

  return FoundUndef;
}

static bool BPFCheckUndefImpl(Function &F) {
  if (FuncsWithUndef.find(&F) != FuncsWithUndef.end())
    return false;
  if (F.hasFnAttribute(Attribute::Naked))
    return false;

  // To handle bpf_tail_call_static, which has
  //   call void @llvm.trap()
  //   unreachable
  // if (F.getName() == "bpf_tail_call_static")
    // return false;

  for (auto &BB : F) {
    for (auto &I : BB) {
      if (BPFCheckInst(F, BB, I))
        return true;
    }
  }
  return false;
}

}

PreservedAnalyses BPFCheckUndefInFuncPass::run(Function &F,
                                               FunctionAnalysisManager &AM) {
  return BPFCheckUndefImpl(F) ? PreservedAnalyses::none()
                              : PreservedAnalyses::all();
}

PreservedAnalyses BPFCheckUndefInModulePass::run(Module &M,
                                                 ModuleAnalysisManager &AM) {
  bool Changed = false;
  for (Function &F : M)
    Changed |= BPFCheckUndefImpl(F);
  return Changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}
