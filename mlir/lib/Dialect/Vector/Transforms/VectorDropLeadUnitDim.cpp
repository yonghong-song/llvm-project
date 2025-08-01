//===- VectorDropLeadUnitDim.cpp - Conversion within the Vector dialect ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <numeric>

#include "mlir/Dialect/Utils/StructuredOpsUtils.h"
#include "mlir/Dialect/Vector/IR/VectorOps.h"
#include "mlir/Dialect/Vector/Transforms/VectorRewritePatterns.h"
#include "mlir/Dialect/Vector/Transforms/VectorTransforms.h"
#include "mlir/Dialect/Vector/Utils/VectorUtils.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/TypeUtilities.h"

#define DEBUG_TYPE "vector-drop-unit-dim"

using namespace mlir;
using namespace mlir::vector;

// Trims leading one dimensions from `oldType` and returns the result type.
// Returns `vector<1xT>` if `oldType` only has one element.
static VectorType trimLeadingOneDims(VectorType oldType) {
  ArrayRef<int64_t> oldShape = oldType.getShape();
  ArrayRef<int64_t> newShape = oldShape;

  ArrayRef<bool> oldScalableDims = oldType.getScalableDims();
  ArrayRef<bool> newScalableDims = oldScalableDims;

  while (!newShape.empty() && newShape.front() == 1 &&
         !newScalableDims.front()) {
    newShape = newShape.drop_front(1);
    newScalableDims = newScalableDims.drop_front(1);
  }

  // Make sure we have at least 1 dimension per vector type requirements.
  if (newShape.empty()) {
    newShape = oldShape.take_back();
    newScalableDims = oldType.getScalableDims().take_back();
  }
  return VectorType::get(newShape, oldType.getElementType(), newScalableDims);
}

/// Return a smallVector of size `rank` containing all zeros.
static SmallVector<int64_t> splatZero(int64_t rank) {
  return SmallVector<int64_t>(rank, 0);
}
namespace {

// Casts away leading one dimensions in vector.extract_strided_slice's vector
// input by inserting vector.broadcast.
struct CastAwayExtractStridedSliceLeadingOneDim
    : public OpRewritePattern<vector::ExtractStridedSliceOp> {
  using OpRewritePattern::OpRewritePattern;

  LogicalResult matchAndRewrite(vector::ExtractStridedSliceOp extractOp,
                                PatternRewriter &rewriter) const override {
    // vector.extract_strided_slice requires the input and output vector to have
    // the same rank. Here we drop leading one dimensions from the input vector
    // type to make sure we don't cause mismatch.
    VectorType oldSrcType = extractOp.getSourceVectorType();
    VectorType newSrcType = trimLeadingOneDims(oldSrcType);

    if (newSrcType.getRank() == oldSrcType.getRank())
      return failure();

    int64_t dropCount = oldSrcType.getRank() - newSrcType.getRank();

    VectorType oldDstType = extractOp.getType();
    VectorType newDstType =
        VectorType::get(oldDstType.getShape().drop_front(dropCount),
                        oldDstType.getElementType(),
                        oldDstType.getScalableDims().drop_front(dropCount));

    Location loc = extractOp.getLoc();

    Value newSrcVector = vector::ExtractOp::create(
        rewriter, loc, extractOp.getVector(), splatZero(dropCount));

    // The offsets/sizes/strides attribute can have a less number of elements
    // than the input vector's rank: it is meant for the leading dimensions.
    auto newOffsets = rewriter.getArrayAttr(
        extractOp.getOffsets().getValue().drop_front(dropCount));
    auto newSizes = rewriter.getArrayAttr(
        extractOp.getSizes().getValue().drop_front(dropCount));
    auto newStrides = rewriter.getArrayAttr(
        extractOp.getStrides().getValue().drop_front(dropCount));

    auto newExtractOp = vector::ExtractStridedSliceOp::create(
        rewriter, loc, newDstType, newSrcVector, newOffsets, newSizes,
        newStrides);

    rewriter.replaceOpWithNewOp<vector::BroadcastOp>(extractOp, oldDstType,
                                                     newExtractOp);

    return success();
  }
};

// Casts away leading one dimensions in vector.insert_strided_slice's vector
// inputs by inserting vector.broadcast.
struct CastAwayInsertStridedSliceLeadingOneDim
    : public OpRewritePattern<vector::InsertStridedSliceOp> {
  using OpRewritePattern::OpRewritePattern;

  LogicalResult matchAndRewrite(vector::InsertStridedSliceOp insertOp,
                                PatternRewriter &rewriter) const override {
    VectorType oldSrcType = insertOp.getSourceVectorType();
    VectorType newSrcType = trimLeadingOneDims(oldSrcType);
    VectorType oldDstType = insertOp.getDestVectorType();
    VectorType newDstType = trimLeadingOneDims(oldDstType);

    int64_t srcDropCount = oldSrcType.getRank() - newSrcType.getRank();
    int64_t dstDropCount = oldDstType.getRank() - newDstType.getRank();
    if (srcDropCount == 0 && dstDropCount == 0)
      return failure();

    // Trim leading one dimensions from both operands.
    Location loc = insertOp.getLoc();

    Value newSrcVector = vector::ExtractOp::create(
        rewriter, loc, insertOp.getValueToStore(), splatZero(srcDropCount));
    Value newDstVector = vector::ExtractOp::create(
        rewriter, loc, insertOp.getDest(), splatZero(dstDropCount));

    auto newOffsets = rewriter.getArrayAttr(
        insertOp.getOffsets().getValue().take_back(newDstType.getRank()));
    auto newStrides = rewriter.getArrayAttr(
        insertOp.getStrides().getValue().take_back(newSrcType.getRank()));

    auto newInsertOp = vector::InsertStridedSliceOp::create(
        rewriter, loc, newDstType, newSrcVector, newDstVector, newOffsets,
        newStrides);

    rewriter.replaceOpWithNewOp<vector::BroadcastOp>(insertOp, oldDstType,
                                                     newInsertOp);

    return success();
  }
};

// Casts away leading one dimensions in vector.insert's vector inputs by
// inserting vector.broadcast.
struct CastAwayInsertLeadingOneDim : public OpRewritePattern<vector::InsertOp> {
  using OpRewritePattern::OpRewritePattern;

  LogicalResult matchAndRewrite(vector::InsertOp insertOp,
                                PatternRewriter &rewriter) const override {
    Type oldSrcType = insertOp.getValueToStoreType();
    Type newSrcType = oldSrcType;
    int64_t oldSrcRank = 0, newSrcRank = 0;
    if (auto type = dyn_cast<VectorType>(oldSrcType)) {
      newSrcType = trimLeadingOneDims(type);
      oldSrcRank = type.getRank();
      newSrcRank = cast<VectorType>(newSrcType).getRank();
    }

    VectorType oldDstType = insertOp.getDestVectorType();
    VectorType newDstType = trimLeadingOneDims(oldDstType);

    int64_t srcDropCount = oldSrcRank - newSrcRank;
    int64_t dstDropCount = oldDstType.getRank() - newDstType.getRank();
    if (srcDropCount == 0 && dstDropCount == 0)
      return failure();

    // Trim leading one dimensions from both operands.
    Location loc = insertOp.getLoc();

    Value newSrcVector = insertOp.getValueToStore();
    if (oldSrcRank != 0) {
      newSrcVector = vector::ExtractOp::create(
          rewriter, loc, insertOp.getValueToStore(), splatZero(srcDropCount));
    }
    Value newDstVector = vector::ExtractOp::create(
        rewriter, loc, insertOp.getDest(), splatZero(dstDropCount));

    // New position rank needs to be computed in two steps: (1) if destination
    // type has leading unit dims, we also trim the position array accordingly,
    // then (2) if source type also has leading unit dims, we need to append
    // zeroes to the position array accordingly.
    unsigned oldPosRank = insertOp.getNumIndices();
    unsigned newPosRank = std::max<int64_t>(0, oldPosRank - dstDropCount);
    SmallVector<OpFoldResult> oldPosition = insertOp.getMixedPosition();
    SmallVector<OpFoldResult> newPosition =
        llvm::to_vector(ArrayRef(oldPosition).take_back(newPosRank));
    newPosition.resize(newDstType.getRank() - newSrcRank,
                       rewriter.getI64IntegerAttr(0));

    auto newInsertOp = vector::InsertOp::create(rewriter, loc, newSrcVector,
                                                newDstVector, newPosition);

    rewriter.replaceOpWithNewOp<vector::BroadcastOp>(insertOp, oldDstType,
                                                     newInsertOp);

    return success();
  }
};

static Value dropUnitDimsFromMask(OpBuilder &b, Location loc, Value mask,
                                  VectorType newType, AffineMap newMap,
                                  VectorType oldMaskType) {
  // Infer the type of the new mask from the new map.
  VectorType newMaskType = inferTransferOpMaskType(newType, newMap);

  // If the new mask is broadcastable to the old result type, we can safely
  // use a `vector.extract` to get the new mask. Otherwise the best we can
  // do is shape cast.
  if (vector::isBroadcastableTo(newMaskType, oldMaskType) ==
      BroadcastableToResult::Success) {
    int64_t dropDim = oldMaskType.getRank() - newMaskType.getRank();
    return vector::ExtractOp::create(b, loc, mask, splatZero(dropDim));
  }
  return vector::ShapeCastOp::create(b, loc, newMaskType, mask);
}

// Turns vector.transfer_read on vector with leading 1 dimensions into
// vector.shape_cast followed by vector.transfer_read on vector without leading
// 1 dimensions.
struct CastAwayTransferReadLeadingOneDim
    : public OpRewritePattern<vector::TransferReadOp> {
  using OpRewritePattern::OpRewritePattern;

  LogicalResult matchAndRewrite(vector::TransferReadOp read,
                                PatternRewriter &rewriter) const override {
    // TODO(#78787): Not supported masked op yet.
    if (cast<MaskableOpInterface>(read.getOperation()).isMasked())
      return failure();
    // TODO: support 0-d corner case.
    if (read.getTransferRank() == 0)
      return failure();

    auto shapedType = cast<ShapedType>(read.getBase().getType());
    if (shapedType.getElementType() != read.getVectorType().getElementType())
      return failure();

    VectorType oldType = read.getVectorType();
    VectorType newType = trimLeadingOneDims(oldType);

    if (newType == oldType)
      return failure();

    AffineMap oldMap = read.getPermutationMap();
    ArrayRef<AffineExpr> newResults =
        oldMap.getResults().take_back(newType.getRank());
    AffineMap newMap =
        AffineMap::get(oldMap.getNumDims(), oldMap.getNumSymbols(), newResults,
                       rewriter.getContext());

    ArrayAttr inBoundsAttr;
    if (read.getInBounds())
      inBoundsAttr = rewriter.getArrayAttr(
          read.getInBoundsAttr().getValue().take_back(newType.getRank()));

    Value mask = Value();
    if (read.getMask()) {
      VectorType maskType = read.getMaskType();
      mask = dropUnitDimsFromMask(rewriter, read.getLoc(), read.getMask(),
                                  newType, newMap, maskType);
    }

    auto newRead = vector::TransferReadOp::create(
        rewriter, read.getLoc(), newType, read.getBase(), read.getIndices(),
        AffineMapAttr::get(newMap), read.getPadding(), mask, inBoundsAttr);
    rewriter.replaceOpWithNewOp<vector::BroadcastOp>(read, oldType, newRead);

    return success();
  }
};

// Turns vector.transfer_write on vector with leading 1 dimensions into
// vector.shape_cast followed by vector.transfer_write on vector without leading
// 1 dimensions.
struct CastAwayTransferWriteLeadingOneDim
    : public OpRewritePattern<vector::TransferWriteOp> {
  using OpRewritePattern::OpRewritePattern;

  LogicalResult matchAndRewrite(vector::TransferWriteOp write,
                                PatternRewriter &rewriter) const override {
    // TODO(#78787): Not supported masked op yet.
    if (cast<MaskableOpInterface>(write.getOperation()).isMasked())
      return failure();
    // TODO: support 0-d corner case.
    if (write.getTransferRank() == 0)
      return failure();

    auto shapedType = dyn_cast<ShapedType>(write.getBase().getType());
    if (shapedType.getElementType() != write.getVectorType().getElementType())
      return failure();

    VectorType oldType = write.getVectorType();
    VectorType newType = trimLeadingOneDims(oldType);
    if (newType == oldType)
      return failure();
    int64_t dropDim = oldType.getRank() - newType.getRank();

    AffineMap oldMap = write.getPermutationMap();
    ArrayRef<AffineExpr> newResults =
        oldMap.getResults().take_back(newType.getRank());
    AffineMap newMap =
        AffineMap::get(oldMap.getNumDims(), oldMap.getNumSymbols(), newResults,
                       rewriter.getContext());

    ArrayAttr inBoundsAttr;
    if (write.getInBounds())
      inBoundsAttr = rewriter.getArrayAttr(
          write.getInBoundsAttr().getValue().take_back(newType.getRank()));

    auto newVector = vector::ExtractOp::create(
        rewriter, write.getLoc(), write.getVector(), splatZero(dropDim));

    if (write.getMask()) {
      VectorType maskType = write.getMaskType();
      Value newMask = dropUnitDimsFromMask(
          rewriter, write.getLoc(), write.getMask(), newType, newMap, maskType);
      rewriter.replaceOpWithNewOp<vector::TransferWriteOp>(
          write, newVector, write.getBase(), write.getIndices(),
          AffineMapAttr::get(newMap), newMask, inBoundsAttr);
      return success();
    }

    rewriter.replaceOpWithNewOp<vector::TransferWriteOp>(
        write, newVector, write.getBase(), write.getIndices(),
        AffineMapAttr::get(newMap), inBoundsAttr);
    return success();
  }
};

} // namespace

FailureOr<Value>
mlir::vector::castAwayContractionLeadingOneDim(vector::ContractionOp contractOp,
                                               MaskingOpInterface maskingOp,
                                               RewriterBase &rewriter) {
  VectorType oldAccType = dyn_cast<VectorType>(contractOp.getAccType());
  if (oldAccType == nullptr)
    return failure();
  if (oldAccType.getRank() < 2)
    return failure();
  if (oldAccType.getShape()[0] != 1)
    return failure();
  // currently we support only dropping one dim but the pattern can be applied
  // greedily to drop more.
  int64_t dropDim = 1;

  auto oldIndexingMaps = contractOp.getIndexingMapsArray();
  SmallVector<AffineMap> newIndexingMaps;

  auto oldIteratorTypes = contractOp.getIteratorTypes();
  SmallVector<Attribute> newIteratorTypes;

  int64_t dimToDrop = oldIndexingMaps[2].getDimPosition(0);

  if (!isParallelIterator(oldIteratorTypes[dimToDrop]))
    // only parallel type iterators can be dropped.
    return failure();

  for (const auto &it : llvm::enumerate(oldIteratorTypes)) {
    int64_t currDim = it.index();
    if (currDim == dimToDrop)
      continue;
    newIteratorTypes.push_back(it.value());
  }

  SmallVector<Value> operands = {contractOp.getLhs(), contractOp.getRhs(),
                                 contractOp.getAcc()};
  SmallVector<Value> newOperands;
  auto loc = contractOp.getLoc();

  for (const auto &it : llvm::enumerate(oldIndexingMaps)) {
    // Check if the dim to be dropped exists as a leading dim in the operand
    // if it does then we use vector.extract to drop it.
    bool validExtract = false;
    SmallVector<AffineExpr> results;
    auto map = it.value();
    int64_t orginalZeroDim = it.value().getDimPosition(0);
    if (orginalZeroDim != dimToDrop) {
      // There are two reasons to be in this path, 1. We need to
      // transpose the operand to make the dim to be dropped
      // leading. 2. The dim to be dropped does not exist and in
      // that case we dont want to add a unit transpose but we must
      // check all the indices to make sure this is the case.
      bool transposeNeeded = false;
      SmallVector<int64_t> perm;
      SmallVector<AffineExpr> transposeResults;

      for (int64_t i = 0, e = map.getNumResults(); i < e; ++i) {
        int64_t currDim = map.getDimPosition(i);
        if (currDim == dimToDrop) {
          transposeNeeded = true;
          perm.insert(perm.begin(), i);
          auto targetExpr = rewriter.getAffineDimExpr(currDim);
          transposeResults.insert(transposeResults.begin(), targetExpr);
        } else {
          perm.push_back(i);
          auto targetExpr = rewriter.getAffineDimExpr(currDim);
          transposeResults.push_back(targetExpr);
        }
      }

      // Checks if only the outer, unit dimensions (of size 1) are permuted.
      // Such transposes do not materially effect the underlying vector and can
      // be omitted. EG: perm [1, 0, 2] applied to vector<1x1x8xi32>
      bool transposeNonOuterUnitDims = false;
      auto operandShape = cast<ShapedType>(operands[it.index()].getType());
      for (auto [index, dim] :
           llvm::enumerate(ArrayRef<int64_t>(perm).drop_back(1))) {
        if (dim != static_cast<int64_t>(index) &&
            operandShape.getDimSize(index) != 1) {
          transposeNonOuterUnitDims = true;
          break;
        }
      }

      // Do the transpose now if needed so that we can drop the
      // correct dim using extract later.
      if (transposeNeeded) {
        map = AffineMap::get(map.getNumDims(), 0, transposeResults,
                             contractOp.getContext());
        if (transposeNonOuterUnitDims) {
          operands[it.index()] = rewriter.createOrFold<vector::TransposeOp>(
              loc, operands[it.index()], perm);
        }
      }
    }
    // We have taken care to have the dim to be dropped be
    // the leading dim. If its still not leading that means it
    // does not exist in this operand and hence we do not need
    // an extract.
    if (map.getDimPosition(0) == dimToDrop)
      validExtract = true;

    for (int64_t i = 0, e = map.getNumResults(); i < e; ++i) {
      int64_t currDim = map.getDimPosition(i);
      if (currDim == dimToDrop)
        // This is the dim we are dropping.
        continue;
      auto targetExpr = rewriter.getAffineDimExpr(
          currDim < dimToDrop ? currDim : currDim - 1);
      results.push_back(targetExpr);
    }
    newIndexingMaps.push_back(AffineMap::get(map.getNumDims() - 1, 0, results,
                                             contractOp.getContext()));
    // Extract if its a valid extraction, otherwise use the operand
    // without extraction.
    newOperands.push_back(validExtract
                              ? vector::ExtractOp::create(rewriter, loc,
                                                          operands[it.index()],
                                                          splatZero(dropDim))
                              : operands[it.index()]);
  }

  // Depending on whether this vector.contract is masked, the replacing Op
  // should either be a new vector.contract Op or vector.mask Op.
  Operation *newOp = vector::ContractionOp::create(
      rewriter, loc, newOperands[0], newOperands[1], newOperands[2],
      rewriter.getAffineMapArrayAttr(newIndexingMaps),
      rewriter.getArrayAttr(newIteratorTypes), contractOp.getKind());

  if (maskingOp) {
    auto newMask = vector::ExtractOp::create(rewriter, loc, maskingOp.getMask(),
                                             splatZero(dropDim));

    newOp = mlir::vector::maskOperation(rewriter, newOp, newMask);
  }

  return vector::BroadcastOp::create(rewriter, loc,
                                     contractOp->getResultTypes()[0],
                                     newOp->getResults()[0])
      .getResult();
}

namespace {

/// Turns vector.contract on vector with leading 1 dimensions into
/// vector.extract followed by vector.contract on vector without leading
/// 1 dimensions. Also performs transpose of lhs and rhs operands if required
/// prior to extract.
struct CastAwayContractionLeadingOneDim
    : public MaskableOpRewritePattern<vector::ContractionOp> {
  using MaskableOpRewritePattern::MaskableOpRewritePattern;

  FailureOr<Value>
  matchAndRewriteMaskableOp(vector::ContractionOp contractOp,
                            MaskingOpInterface maskingOp,
                            PatternRewriter &rewriter) const override {
    return castAwayContractionLeadingOneDim(contractOp, maskingOp, rewriter);
  }
};

/// Looks at elementwise operations on vectors with at least one leading
/// dimension equal 1, e.g. vector<1x[4]x1xf32> (but not vector<2x[4]x1xf32>),
/// and cast aways the leading one dimensions (_plural_) and then broadcasts
/// the results.
///
/// Example before:
///     %1 = arith.mulf %arg0, %arg1 : vector<1x4x1xf32>
/// Example after:
///    %2 = arith.mulf %0, %1 : vector<4x1xf32>
///    %3 = vector.broadcast %2 : vector<4x1xf32> to vector<1x4x1xf32>
///
/// Does support scalable vectors.
class CastAwayElementwiseLeadingOneDim : public RewritePattern {
public:
  CastAwayElementwiseLeadingOneDim(MLIRContext *context,
                                   PatternBenefit benefit = 1)
      : RewritePattern(MatchAnyOpTypeTag(), benefit, context) {}

  LogicalResult matchAndRewrite(Operation *op,
                                PatternRewriter &rewriter) const override {
    if (!OpTrait::hasElementwiseMappableTraits(op) || op->getNumResults() != 1)
      return failure();
    auto vecType = dyn_cast<VectorType>(op->getResultTypes()[0]);
    if (!vecType)
      return failure();
    VectorType newVecType = trimLeadingOneDims(vecType);
    if (newVecType == vecType)
      return failure();
    int64_t dropDim = vecType.getRank() - newVecType.getRank();
    SmallVector<Value, 4> newOperands;
    for (Value operand : op->getOperands()) {
      if (auto opVecType = dyn_cast<VectorType>(operand.getType())) {
        newOperands.push_back(vector::ExtractOp::create(
            rewriter, op->getLoc(), operand, splatZero(dropDim)));
      } else {
        newOperands.push_back(operand);
      }
    }
    Operation *newOp =
        rewriter.create(op->getLoc(), op->getName().getIdentifier(),
                        newOperands, newVecType, op->getAttrs());
    rewriter.replaceOpWithNewOp<vector::BroadcastOp>(op, vecType,
                                                     newOp->getResult(0));
    return success();
  }
};

// Drops leading 1 dimensions from vector.constant_mask and inserts a
// vector.broadcast back to the original shape.
struct CastAwayConstantMaskLeadingOneDim
    : public OpRewritePattern<vector::ConstantMaskOp> {
  using OpRewritePattern::OpRewritePattern;

  LogicalResult matchAndRewrite(vector::ConstantMaskOp mask,
                                PatternRewriter &rewriter) const override {
    VectorType oldType = mask.getType();
    VectorType newType = trimLeadingOneDims(oldType);

    if (newType == oldType)
      return failure();

    int64_t dropDim = oldType.getRank() - newType.getRank();
    ArrayRef<int64_t> dimSizes = mask.getMaskDimSizes();

    // If any of the dropped unit dims has a size of `0`, the entire mask is a
    // zero mask, else the unit dim has no effect on the mask.
    int64_t flatLeadingSize =
        std::accumulate(dimSizes.begin(), dimSizes.begin() + dropDim + 1,
                        static_cast<int64_t>(1), std::multiplies<int64_t>());
    SmallVector<int64_t> newDimSizes = {flatLeadingSize};
    newDimSizes.append(dimSizes.begin() + dropDim + 1, dimSizes.end());

    auto newMask = vector::ConstantMaskOp::create(rewriter, mask.getLoc(),
                                                  newType, newDimSizes);
    rewriter.replaceOpWithNewOp<vector::BroadcastOp>(mask, oldType, newMask);
    return success();
  }
};

} // namespace

void mlir::vector::populateCastAwayVectorLeadingOneDimPatterns(
    RewritePatternSet &patterns, PatternBenefit benefit) {
  patterns
      .add<CastAwayExtractStridedSliceLeadingOneDim,
           CastAwayInsertStridedSliceLeadingOneDim, CastAwayInsertLeadingOneDim,
           CastAwayConstantMaskLeadingOneDim, CastAwayTransferReadLeadingOneDim,
           CastAwayTransferWriteLeadingOneDim, CastAwayElementwiseLeadingOneDim,
           CastAwayContractionLeadingOneDim>(patterns.getContext(), benefit);
}
