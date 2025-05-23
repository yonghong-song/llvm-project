; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64 -mattr=+zu | FileCheck %s

define i16 @i8(i8 %x) nounwind {
; CHECK-LABEL: i8:
; CHECK:       # %bb.0:
; CHECK-NEXT:    cmpb $3, %dil
; CHECK-NEXT:    setzuae %al
; CHECK-NEXT:    # kill: def $ax killed $ax killed $eax
; CHECK-NEXT:    retq
  %t0 = icmp ugt i8 %x, 2
  %zext = zext i1 %t0 to i16
  ret i16 %zext
}

define i16 @i16(i16 %x) nounwind {
; CHECK-LABEL: i16:
; CHECK:       # %bb.0:
; CHECK-NEXT:    cmpw $2, %di
; CHECK-NEXT:    setzub %al
; CHECK-NEXT:    # kill: def $ax killed $ax killed $eax
; CHECK-NEXT:    retq
  %t0 = icmp ult i16 %x, 2
  %if = select i1 %t0, i16 1, i16 0
  ret i16 %if
}

define i32 @i32(i32 %x) nounwind {
; CHECK-LABEL: i32:
; CHECK:       # %bb.0:
; CHECK-NEXT:    cmpl $1, %edi
; CHECK-NEXT:    setzue %al
; CHECK-NEXT:    retq
  %t0 = icmp eq i32 %x, 1
  %if = select i1 %t0, i32 1, i32 0
  ret i32 %if
}

define i64 @i64(i64 %x) nounwind {
; CHECK-LABEL: i64:
; CHECK:       # %bb.0:
; CHECK-NEXT:    cmpq $1, %rdi
; CHECK-NEXT:    setzune %al
; CHECK-NEXT:    retq
  %t0 = icmp ne i64 %x, 1
  %if = select i1 %t0, i64 1, i64 0
  ret i64 %if
}

define i32 @flags_copy_lowering() nounwind {
; CHECK-LABEL: flags_copy_lowering:
; CHECK:       # %bb.0: # %bb
; CHECK-NEXT:    xorl %eax, %eax
; CHECK-NEXT:    xorl %edx, %edx
; CHECK-NEXT:    xorl %ecx, %ecx
; CHECK-NEXT:    .p2align 4
; CHECK-NEXT:  .LBB4_1: # %bb1
; CHECK-NEXT:    # =>This Inner Loop Header: Depth=1
; CHECK-NEXT:    addl %edx, 0
; CHECK-NEXT:    setb %sil
; CHECK-NEXT:    adcl $0, %ecx
; CHECK-NEXT:    testb %sil, %sil
; CHECK-NEXT:    je .LBB4_3
; CHECK-NEXT:  # %bb.2: # %bb1
; CHECK-NEXT:    # in Loop: Header=BB4_1 Depth=1
; CHECK-NEXT:    xorl %edx, %edx
; CHECK-NEXT:    movb %sil, %dl
; CHECK-NEXT:    testb %al, %al
; CHECK-NEXT:    jne .LBB4_1
; CHECK-NEXT:  .LBB4_3: # %bb2
; CHECK-NEXT:    xorl %eax, %eax
; CHECK-NEXT:    retq
bb:
  br label %bb1

bb1:
  %phi = phi i32 [ 0, %bb ], [ %zext, %bb1 ]
  %phi2 = phi i32 [ 0, %bb ], [ %add3, %bb1 ]
  %load = load i32, ptr null, align 4
  %add = add i32 %load, %phi
  store i32 %add, ptr null, align 4
  %icmp = icmp ugt i32 %phi, %add
  %zext = zext i1 %icmp to i32
  %add3 = add i32 %phi2, %zext
  %icmp4 = icmp ult i32 %phi2, 0
  %and = and i1 %icmp, false
  br i1 %and, label %bb1, label %bb2

bb2:
  ret i32 0
}
