//===- SymbolManager.h - Management of Symbolic Values --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
//  This file defines SymbolManager, a class that manages symbolic values
//  created for use by ExprEngine and related classes.
//
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Core/PathSensitive/SymbolManager.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/Analysis/Analyses/LiveVariables.h"
#include "clang/Analysis/AnalysisDeclContext.h"
#include "clang/Basic/LLVM.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/MemRegion.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SVals.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/Store.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/SymExpr.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>

using namespace clang;
using namespace ento;

void SymExpr::anchor() {}

StringRef SymbolConjured::getKindStr() const { return "conj_$"; }
StringRef SymbolDerived::getKindStr() const { return "derived_$"; }
StringRef SymbolExtent::getKindStr() const { return "extent_$"; }
StringRef SymbolMetadata::getKindStr() const { return "meta_$"; }
StringRef SymbolRegionValue::getKindStr() const { return "reg_$"; }

LLVM_DUMP_METHOD void SymExpr::dump() const { dumpToStream(llvm::errs()); }

void BinarySymExpr::dumpToStreamImpl(raw_ostream &OS, const SymExpr *Sym) {
  OS << '(';
  Sym->dumpToStream(OS);
  OS << ')';
}

void BinarySymExpr::dumpToStreamImpl(raw_ostream &OS,
                                     const llvm::APSInt &Value) {
  if (Value.isUnsigned())
    OS << Value.getZExtValue();
  else
    OS << Value.getSExtValue();
  if (Value.isUnsigned())
    OS << 'U';
}

void BinarySymExpr::dumpToStreamImpl(raw_ostream &OS,
                                     BinaryOperator::Opcode Op) {
  OS << ' ' << BinaryOperator::getOpcodeStr(Op) << ' ';
}

void SymbolCast::dumpToStream(raw_ostream &os) const {
  os << '(' << ToTy << ") (";
  Operand->dumpToStream(os);
  os << ')';
}

void UnarySymExpr::dumpToStream(raw_ostream &os) const {
  os << UnaryOperator::getOpcodeStr(Op);
  bool Binary = isa<BinarySymExpr>(Operand);
  if (Binary)
    os << '(';
  Operand->dumpToStream(os);
  if (Binary)
    os << ')';
}

const Stmt *SymbolConjured::getStmt() const {
  // Sometimes the CFG element is invalid, avoid dereferencing it.
  if (Elem.getParent() == nullptr ||
      Elem.getIndexInBlock() >= Elem.getParent()->size())
    return nullptr;
  switch (Elem->getKind()) {
  case CFGElement::Initializer:
    if (const auto *Init = Elem->castAs<CFGInitializer>().getInitializer()) {
      return Init->getInit();
    }
    return nullptr;
  case CFGElement::ScopeBegin:
    return Elem->castAs<CFGScopeBegin>().getTriggerStmt();
  case CFGElement::ScopeEnd:
    return Elem->castAs<CFGScopeEnd>().getTriggerStmt();
  case CFGElement::NewAllocator:
    return Elem->castAs<CFGNewAllocator>().getAllocatorExpr();
  case CFGElement::LifetimeEnds:
    return Elem->castAs<CFGLifetimeEnds>().getTriggerStmt();
  case CFGElement::LoopExit:
    return Elem->castAs<CFGLoopExit>().getLoopStmt();
  case CFGElement::Statement:
    return Elem->castAs<CFGStmt>().getStmt();
  case CFGElement::Constructor:
    return Elem->castAs<CFGConstructor>().getStmt();
  case CFGElement::CXXRecordTypedCall:
    return Elem->castAs<CFGCXXRecordTypedCall>().getStmt();
  case CFGElement::AutomaticObjectDtor:
    return Elem->castAs<CFGAutomaticObjDtor>().getTriggerStmt();
  case CFGElement::DeleteDtor:
    return Elem->castAs<CFGDeleteDtor>().getDeleteExpr();
  case CFGElement::BaseDtor:
    return nullptr;
  case CFGElement::MemberDtor:
    return nullptr;
  case CFGElement::TemporaryDtor:
    return Elem->castAs<CFGTemporaryDtor>().getBindTemporaryExpr();
  case CFGElement::CleanupFunction:
    return nullptr;
  }
  return nullptr;
}

void SymbolConjured::dumpToStream(raw_ostream &os) const {
  os << getKindStr() << getSymbolID() << '{' << T << ", LC" << LCtx->getID();
  if (auto *S = getStmt())
    os << ", S" << S->getID(LCtx->getDecl()->getASTContext());
  else
    os << ", no stmt";
  os << ", #" << Count << '}';
}

void SymbolDerived::dumpToStream(raw_ostream &os) const {
  os << getKindStr() << getSymbolID() << '{' << getParentSymbol() << ','
     << getRegion() << '}';
}

void SymbolExtent::dumpToStream(raw_ostream &os) const {
  os << getKindStr() << getSymbolID() << '{' << getRegion() << '}';
}

void SymbolMetadata::dumpToStream(raw_ostream &os) const {
  os << getKindStr() << getSymbolID() << '{' << getRegion() << ',' << T << '}';
}

void SymbolData::anchor() {}

void SymbolRegionValue::dumpToStream(raw_ostream &os) const {
  os << getKindStr() << getSymbolID() << '<' << getType() << ' ' << R << '>';
}

bool SymExpr::symbol_iterator::operator==(const symbol_iterator &X) const {
  return itr == X.itr;
}

bool SymExpr::symbol_iterator::operator!=(const symbol_iterator &X) const {
  return itr != X.itr;
}

SymExpr::symbol_iterator::symbol_iterator(const SymExpr *SE) {
  itr.push_back(SE);
}

SymExpr::symbol_iterator &SymExpr::symbol_iterator::operator++() {
  assert(!itr.empty() && "attempting to iterate on an 'end' iterator");
  expand();
  return *this;
}

SymbolRef SymExpr::symbol_iterator::operator*() {
  assert(!itr.empty() && "attempting to dereference an 'end' iterator");
  return itr.back();
}

void SymExpr::symbol_iterator::expand() {
  const SymExpr *SE = itr.pop_back_val();

  switch (SE->getKind()) {
    case SymExpr::SymbolRegionValueKind:
    case SymExpr::SymbolConjuredKind:
    case SymExpr::SymbolDerivedKind:
    case SymExpr::SymbolExtentKind:
    case SymExpr::SymbolMetadataKind:
      return;
    case SymExpr::SymbolCastKind:
      itr.push_back(cast<SymbolCast>(SE)->getOperand());
      return;
    case SymExpr::UnarySymExprKind:
      itr.push_back(cast<UnarySymExpr>(SE)->getOperand());
      return;
    case SymExpr::SymIntExprKind:
      itr.push_back(cast<SymIntExpr>(SE)->getLHS());
      return;
    case SymExpr::IntSymExprKind:
      itr.push_back(cast<IntSymExpr>(SE)->getRHS());
      return;
    case SymExpr::SymSymExprKind: {
      const auto *x = cast<SymSymExpr>(SE);
      itr.push_back(x->getLHS());
      itr.push_back(x->getRHS());
      return;
    }
  }
  llvm_unreachable("unhandled expansion case");
}

QualType SymbolConjured::getType() const {
  return T;
}

QualType SymbolDerived::getType() const {
  return R->getValueType();
}

QualType SymbolExtent::getType() const {
  ASTContext &Ctx = R->getMemRegionManager().getContext();
  return Ctx.getSizeType();
}

QualType SymbolMetadata::getType() const {
  return T;
}

QualType SymbolRegionValue::getType() const {
  return R->getValueType();
}

bool SymbolManager::canSymbolicate(QualType T) {
  T = T.getCanonicalType();

  if (Loc::isLocType(T))
    return true;

  if (T->isIntegralOrEnumerationType())
    return true;

  if (T->isRecordType() && !T->isUnionType())
    return true;

  return false;
}

void SymbolManager::addSymbolDependency(const SymbolRef Primary,
                                        const SymbolRef Dependent) {
  auto &dependencies = SymbolDependencies[Primary];
  if (!dependencies) {
    dependencies = std::make_unique<SymbolRefSmallVectorTy>();
  }
  dependencies->push_back(Dependent);
}

const SymbolRefSmallVectorTy *SymbolManager::getDependentSymbols(
                                                     const SymbolRef Primary) {
  SymbolDependTy::const_iterator I = SymbolDependencies.find(Primary);
  if (I == SymbolDependencies.end())
    return nullptr;
  return I->second.get();
}

void SymbolReaper::markDependentsLive(SymbolRef sym) {
  // Do not mark dependents more then once.
  SymbolMapTy::iterator LI = TheLiving.find(sym);
  assert(LI != TheLiving.end() && "The primary symbol is not live.");
  if (LI->second == HaveMarkedDependents)
    return;
  LI->second = HaveMarkedDependents;

  if (const SymbolRefSmallVectorTy *Deps = SymMgr.getDependentSymbols(sym)) {
    for (const auto I : *Deps) {
      if (TheLiving.contains(I))
        continue;
      markLive(I);
    }
  }
}

void SymbolReaper::markLive(SymbolRef sym) {
  TheLiving[sym] = NotProcessed;
  markDependentsLive(sym);
}

void SymbolReaper::markLive(const MemRegion *region) {
  LiveRegionRoots.insert(region->getBaseRegion());
  markElementIndicesLive(region);
}

void SymbolReaper::markLazilyCopied(const clang::ento::MemRegion *region) {
  LazilyCopiedRegionRoots.insert(region->getBaseRegion());
}

void SymbolReaper::markElementIndicesLive(const MemRegion *region) {
  for (auto SR = dyn_cast<SubRegion>(region); SR;
       SR = dyn_cast<SubRegion>(SR->getSuperRegion())) {
    if (const auto ER = dyn_cast<ElementRegion>(SR)) {
      SVal Idx = ER->getIndex();
      for (SymbolRef Sym : Idx.symbols())
        markLive(Sym);
    }
  }
}

void SymbolReaper::markInUse(SymbolRef sym) {
  if (isa<SymbolMetadata>(sym))
    MetadataInUse.insert(sym);
}

bool SymbolReaper::isLiveRegion(const MemRegion *MR) {
  // TODO: For now, liveness of a memory region is equivalent to liveness of its
  // base region. In fact we can do a bit better: say, if a particular FieldDecl
  // is not used later in the path, we can diagnose a leak of a value within
  // that field earlier than, say, the variable that contains the field dies.
  MR = MR->getBaseRegion();
  if (LiveRegionRoots.count(MR))
    return true;

  if (const auto *SR = dyn_cast<SymbolicRegion>(MR))
    return isLive(SR->getSymbol());

  if (const auto *VR = dyn_cast<VarRegion>(MR))
    return isLive(VR, true);

  // FIXME: This is a gross over-approximation. What we really need is a way to
  // tell if anything still refers to this region. Unlike SymbolicRegions,
  // AllocaRegions don't have associated symbols, though, so we don't actually
  // have a way to track their liveness.
  return isa<AllocaRegion, CXXThisRegion, MemSpaceRegion, CodeTextRegion>(MR);
}

bool SymbolReaper::isLazilyCopiedRegion(const MemRegion *MR) const {
  // TODO: See comment in isLiveRegion.
  return LazilyCopiedRegionRoots.count(MR->getBaseRegion());
}

bool SymbolReaper::isReadableRegion(const MemRegion *MR) {
  return isLiveRegion(MR) || isLazilyCopiedRegion(MR);
}

bool SymbolReaper::isLive(SymbolRef sym) {
  if (TheLiving.count(sym)) {
    markDependentsLive(sym);
    return true;
  }

  bool KnownLive;

  switch (sym->getKind()) {
  case SymExpr::SymbolRegionValueKind:
    KnownLive = isReadableRegion(cast<SymbolRegionValue>(sym)->getRegion());
    break;
  case SymExpr::SymbolConjuredKind:
    KnownLive = false;
    break;
  case SymExpr::SymbolDerivedKind:
    KnownLive = isLive(cast<SymbolDerived>(sym)->getParentSymbol());
    break;
  case SymExpr::SymbolExtentKind:
    KnownLive = isLiveRegion(cast<SymbolExtent>(sym)->getRegion());
    break;
  case SymExpr::SymbolMetadataKind:
    KnownLive = MetadataInUse.count(sym) &&
                isLiveRegion(cast<SymbolMetadata>(sym)->getRegion());
    if (KnownLive)
      MetadataInUse.erase(sym);
    break;
  case SymExpr::SymIntExprKind:
    KnownLive = isLive(cast<SymIntExpr>(sym)->getLHS());
    break;
  case SymExpr::IntSymExprKind:
    KnownLive = isLive(cast<IntSymExpr>(sym)->getRHS());
    break;
  case SymExpr::SymSymExprKind:
    KnownLive = isLive(cast<SymSymExpr>(sym)->getLHS()) &&
                isLive(cast<SymSymExpr>(sym)->getRHS());
    break;
  case SymExpr::SymbolCastKind:
    KnownLive = isLive(cast<SymbolCast>(sym)->getOperand());
    break;
  case SymExpr::UnarySymExprKind:
    KnownLive = isLive(cast<UnarySymExpr>(sym)->getOperand());
    break;
  }

  if (KnownLive)
    markLive(sym);

  return KnownLive;
}

bool
SymbolReaper::isLive(const Expr *ExprVal, const LocationContext *ELCtx) const {
  if (LCtx == nullptr)
    return false;

  if (LCtx != ELCtx) {
    // If the reaper's location context is a parent of the expression's
    // location context, then the expression value is now "out of scope".
    if (LCtx->isParentOf(ELCtx))
      return false;
    return true;
  }

  // If no statement is provided, everything in this and parent contexts is
  // live.
  if (!Loc)
    return true;

  return LCtx->getAnalysis<RelaxedLiveVariables>()->isLive(Loc, ExprVal);
}

bool SymbolReaper::isLive(const VarRegion *VR, bool includeStoreBindings) const{
  const StackFrameContext *VarContext = VR->getStackFrame();

  if (!VarContext)
    return true;

  if (!LCtx)
    return false;
  const StackFrameContext *CurrentContext = LCtx->getStackFrame();

  if (VarContext == CurrentContext) {
    // If no statement is provided, everything is live.
    if (!Loc)
      return true;

    // Anonymous parameters of an inheriting constructor are live for the entire
    // duration of the constructor.
    if (isa<CXXInheritedCtorInitExpr>(Loc))
      return true;

    if (LCtx->getAnalysis<RelaxedLiveVariables>()->isLive(Loc, VR->getDecl()))
      return true;

    if (!includeStoreBindings)
      return false;

    unsigned &cachedQuery =
      const_cast<SymbolReaper *>(this)->includedRegionCache[VR];

    if (cachedQuery) {
      return cachedQuery == 1;
    }

    // Query the store to see if the region occurs in any live bindings.
    if (Store store = reapedStore.getStore()) {
      bool hasRegion =
        reapedStore.getStoreManager().includedInBindings(store, VR);
      cachedQuery = hasRegion ? 1 : 2;
      return hasRegion;
    }

    return false;
  }

  return VarContext->isParentOf(CurrentContext);
}
