; REQUIRES: x86
; RUN: mkdir -p %t.dir
; RUN: llvm-as -o %t.obj %s
; RUN: llvm-mc -triple=x86_64-pc-windows-msvc -filetype=obj -o %t.dir/lto-chkstk-foo.obj %S/Inputs/lto-chkstk-foo.s
; RUN: llvm-mc -triple=x86_64-pc-windows-msvc -filetype=obj -o %t.dir/lto-chkstk-chkstk.obj %S/Inputs/lto-chkstk-chkstk.s
; RUN: rm -f %t.lib
; RUN: llvm-ar cru %t.lib %t.dir/lto-chkstk-chkstk.obj
; RUN: lld-link /out:%t.exe /entry:main /subsystem:console %t.obj %t.dir/lto-chkstk-foo.obj %t.lib

target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-windows-msvc"

define void @main() {
entry:
  %array4096 = alloca [4096 x i8]
  call void @foo(ptr %array4096)
  ret void
}

declare void @foo(ptr)
