//===-- PGOCtxProfLowering.h - Contextual PGO Instr. Lowering ---*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file declares the PGOCtxProfLoweringPass class.
//
//===----------------------------------------------------------------------===//
#ifndef LLVM_TRANSFORMS_INSTRUMENTATION_PGOCTXPROFLOWERING_H
#define LLVM_TRANSFORMS_INSTRUMENTATION_PGOCTXPROFLOWERING_H

#include "llvm/IR/PassManager.h"
namespace llvm {
class Type;

class PGOCtxProfLoweringPass : public PassInfoMixin<PGOCtxProfLoweringPass> {
public:
  explicit PGOCtxProfLoweringPass() = default;
  // True if contextual instrumentation is enabled.
  static bool isCtxIRPGOInstrEnabled();

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &MAM);
};

// Utility pass blocking inlining for any function that may be overridden during
// linking by a prevailing copy.
// This avoids confusingly collecting profiles for the same GUID corresponding
// to different variants of the function. We could do like PGO and identify
// functions by a (GUID, Hash) tuple, but since the ctxprof "use" waits for
// thinlto to happen before performing any further optimizations, it's
// unnecessary to collect profiles for non-prevailing copies.
class NoinlineNonPrevailing : public PassInfoMixin<NoinlineNonPrevailing> {
public:
  explicit NoinlineNonPrevailing() = default;

  PreservedAnalyses run(Module &M, ModuleAnalysisManager &MAM);
};

} // namespace llvm
#endif
