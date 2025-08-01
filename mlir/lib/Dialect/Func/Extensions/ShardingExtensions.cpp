//===- ShardingExtensions.cpp - ---------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "mlir/Dialect/Func/Extensions/ShardingExtensions.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/Shard/Interfaces/ShardingInterfaceImpl.h"
#include "mlir/IR/MLIRContext.h"

namespace mlir::func {

void registerShardingInterfaceExternalModels(DialectRegistry &registry) {
  registry.addExtension(+[](MLIRContext *ctx, FuncDialect *dialect) {
    ReturnOp::attachInterface<
        shard::IndependentParallelIteratorDomainShardingInterface<ReturnOp>>(
        *ctx);
  });
}

} // namespace mlir::func
