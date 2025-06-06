; NOTE: Assertions have been autogenerated by utils/update_test_checks.py UTC_ARGS: --version 2
; Test if the !invariant.load metadata is maintained by GVN.
; RUN: opt -passes=gvn -S < %s | FileCheck %s

define i32 @test1(ptr nocapture %p, ptr nocapture %q) {
; CHECK-LABEL: define i32 @test1
; CHECK-SAME: (ptr captures(none) [[P:%.*]], ptr captures(none) [[Q:%.*]]) {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[X:%.*]] = load i32, ptr [[P]], align 4, !invariant.load [[META0:![0-9]+]]
; CHECK-NEXT:    [[CONV:%.*]] = trunc i32 [[X]] to i8
; CHECK-NEXT:    store i8 [[CONV]], ptr [[Q]], align 1
; CHECK-NEXT:    [[ADD:%.*]] = add i32 [[X]], 1
; CHECK-NEXT:    ret i32 [[ADD]]
;
entry:
  %x = load i32, ptr %p, align 4, !invariant.load !0
  %conv = trunc i32 %x to i8
  store i8 %conv, ptr %q, align 1
  %y = load i32, ptr %p, align 4, !invariant.load !0
  %add = add i32 %y, 1
  ret i32 %add
}

define i32 @test2(ptr nocapture %p, ptr nocapture %q) {
; CHECK-LABEL: define i32 @test2
; CHECK-SAME: (ptr captures(none) [[P:%.*]], ptr captures(none) [[Q:%.*]]) {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[X:%.*]] = load i32, ptr [[P]], align 4
; CHECK-NEXT:    [[CONV:%.*]] = trunc i32 [[X]] to i8
; CHECK-NEXT:    store i8 [[CONV]], ptr [[Q]], align 1
; CHECK-NEXT:    [[ADD:%.*]] = add i32 [[X]], 1
; CHECK-NEXT:    ret i32 [[ADD]]
;
entry:
  %x = load i32, ptr %p, align 4
  %conv = trunc i32 %x to i8
  store i8 %conv, ptr %q, align 1
  %y = load i32, ptr %p, align 4, !invariant.load !0
  %add = add i32 %y, 1
  ret i32 %add
}

; With the invariant.load metadata, what would otherwise
; be a case for PRE becomes a full redundancy.
define i32 @test3(i1 %cnd, ptr %p, ptr %q) {
; CHECK-LABEL: define i32 @test3
; CHECK-SAME: (i1 [[CND:%.*]], ptr [[P:%.*]], ptr [[Q:%.*]]) {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    br i1 [[CND]], label [[BB1:%.*]], label [[BB2:%.*]]
; CHECK:       bb1:
; CHECK-NEXT:    store i32 5, ptr [[Q]], align 4
; CHECK-NEXT:    br label [[BB2]]
; CHECK:       bb2:
; CHECK-NEXT:    ret i32 0
;
entry:
  %v1 = load i32, ptr %p
  br i1 %cnd, label %bb1, label %bb2

bb1:
  store i32 5, ptr %q
  br label %bb2

bb2:
  %v2 = load i32, ptr %p, !invariant.load !0
  %res = sub i32 %v1, %v2
  ret i32 %res
}

; This test is here to document a case which doesn't optimize
; as well as it could.
define i32 @test4(i1 %cnd, ptr %p, ptr %q) {
; CHECK-LABEL: define i32 @test4
; CHECK-SAME: (i1 [[CND:%.*]], ptr [[P:%.*]], ptr [[Q:%.*]]) {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[V1:%.*]] = load i32, ptr [[P]], align 4, !invariant.load [[META0]]
; CHECK-NEXT:    br i1 [[CND]], label [[BB1:%.*]], label [[BB2:%.*]]
; CHECK:       bb1:
; CHECK-NEXT:    store i32 5, ptr [[Q]], align 4
; CHECK-NEXT:    [[V2_PRE:%.*]] = load i32, ptr [[P]], align 4
; CHECK-NEXT:    br label [[BB2]]
; CHECK:       bb2:
; CHECK-NEXT:    [[V2:%.*]] = phi i32 [ [[V2_PRE]], [[BB1]] ], [ [[V1]], [[ENTRY:%.*]] ]
; CHECK-NEXT:    [[RES:%.*]] = sub i32 [[V1]], [[V2]]
; CHECK-NEXT:    ret i32 [[RES]]
;
; %v2 is redundant, but GVN currently doesn't catch that
entry:
  %v1 = load i32, ptr %p, !invariant.load !0
  br i1 %cnd, label %bb1, label %bb2

bb1:
  store i32 5, ptr %q
  br label %bb2

bb2:
  %v2 = load i32, ptr %p
  %res = sub i32 %v1, %v2
  ret i32 %res
}

; Checks that we return the mustalias store as a def
; so that it contributes to value forwarding.  Note
; that we could and should remove the store too.
define i32 @test5(i1 %cnd, ptr %p) {
; CHECK-LABEL: define i32 @test5
; CHECK-SAME: (i1 [[CND:%.*]], ptr [[P:%.*]]) {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    store i32 5, ptr [[P]], align 4
; CHECK-NEXT:    ret i32 5
;
entry:
  %v1 = load i32, ptr %p, !invariant.load !0
  store i32 5, ptr %p ;; must alias store, want to exploit
  %v2 = load i32, ptr %p, !invariant.load !0
  ret i32 %v2
}


declare void @foo()

; Clobbering (mayalias) stores, even in function calls, can be ignored
define i32 @test6(i1 %cnd, ptr %p) {
; CHECK-LABEL: define i32 @test6
; CHECK-SAME: (i1 [[CND:%.*]], ptr [[P:%.*]]) {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    call void @foo()
; CHECK-NEXT:    ret i32 0
;
entry:
  %v1 = load i32, ptr %p, !invariant.load !0
  call void @foo()
  %v2 = load i32, ptr %p, !invariant.load !0
  %res = sub i32 %v1, %v2
  ret i32 %res
}

declare noalias ptr @bar(...)

; Same as previous, but a function with a noalias result (since they're handled
; differently in MDA)
define i32 @test7(i1 %cnd, ptr %p) {
; CHECK-LABEL: define i32 @test7
; CHECK-SAME: (i1 [[CND:%.*]], ptr [[P:%.*]]) {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[TMP0:%.*]] = call ptr (...) @bar(ptr [[P]])
; CHECK-NEXT:    ret i32 0
;
entry:
  %v1 = load i32, ptr %p, !invariant.load !0
  call ptr (...) @bar(ptr %p)
  %v2 = load i32, ptr %p, !invariant.load !0
  %res = sub i32 %v1, %v2
  ret i32 %res
}

define i32 @test8(i1 %cnd, ptr %p) {
; CHECK-LABEL: define i32 @test8
; CHECK-SAME: (i1 [[CND:%.*]], ptr [[P:%.*]]) {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[V1:%.*]] = load i32, ptr [[P]], align 4, !invariant.load [[META0]]
; CHECK-NEXT:    br i1 [[CND]], label [[TAKEN:%.*]], label [[MERGE:%.*]]
; CHECK:       taken:
; CHECK-NEXT:    [[P2:%.*]] = call ptr (...) @bar(ptr [[P]])
; CHECK-NEXT:    [[V2_PRE:%.*]] = load i32, ptr [[P2]], align 4, !invariant.load [[META0]]
; CHECK-NEXT:    br label [[MERGE]]
; CHECK:       merge:
; CHECK-NEXT:    [[V2:%.*]] = phi i32 [ [[V1]], [[ENTRY:%.*]] ], [ [[V2_PRE]], [[TAKEN]] ]
; CHECK-NEXT:    [[P3:%.*]] = phi ptr [ [[P]], [[ENTRY]] ], [ [[P2]], [[TAKEN]] ]
; CHECK-NEXT:    [[RES:%.*]] = sub i32 [[V1]], [[V2]]
; CHECK-NEXT:    ret i32 [[RES]]
;
entry:
  %v1 = load i32, ptr %p, !invariant.load !0
  br i1 %cnd, label %taken, label %merge
taken:
  %p2 = call ptr (...) @bar(ptr %p)
  br label %merge
merge:
  %p3 = phi ptr [%p, %entry], [%p2, %taken]
  %v2 = load i32, ptr %p3, !invariant.load !0
  %res = sub i32 %v1, %v2
  ret i32 %res
}

define i32 @metadata_preservation(ptr nocapture %p, ptr nocapture %q) {
; CHECK-LABEL: define i32 @metadata_preservation
; CHECK-SAME: (ptr captures(none) [[P:%.*]], ptr captures(none) [[Q:%.*]]) {
; CHECK-NEXT:  entry:
; CHECK-NEXT:    [[X:%.*]] = load i32, ptr [[P]], align 4, !invariant.load [[META0]]
; CHECK-NEXT:    [[ADD:%.*]] = add i32 [[X]], [[X]]
; CHECK-NEXT:    ret i32 [[ADD]]
;
entry:
  %x = load i32, ptr %p, align 4, !invariant.load !0
  %y = load i32, ptr %p, align 4
  %add = add i32 %x, %y
  ret i32 %add
}

!0 = !{ }

