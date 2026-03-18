#ifndef LLVM_LIB_TARGET_BPF_RISCVMACHINEFUNCTIONINFO_H
#define LLVM_LIB_TARGET_BPF_RISCVMACHINEFUNCTIONINFO_H

#include "llvm/CodeGen/MachineFunction.h"

namespace llvm {

class BPFMachineFunctionInfo : public MachineFunctionInfo {
public:
  short IncomingExtDepth = 0;
  short MaxOutgoingExtDepth = 0;

  BPFMachineFunctionInfo(const Function &F, const BPFSubtarget *STI) {}
};

} // end namespace llvm

#endif
