; REQUIRES: x86-registered-target
; RUN: opt -O2 -S < %s | FileCheck %s
;
; Source code:
;   int tar(int a);
;   __attribute__((noinline)) static int foo(int a, int b)
;   {
;     return tar(a) + tar(a + 1);
;   }
;   int bar(int a)
;   {
;     foo(a, 1);
;     return 0;
;   }
; Compilation flag:
;   clang -O2 -g -S -emit-llvm -Xclang -disable-llvm-passes test.c

; ModuleID = 'test.c'
source_filename = "test.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define dso_local i32 @bar(i32 noundef %0) #0 !dbg !9 {
  %2 = alloca i32, align 4
  store i32 %0, ptr %2, align 4, !tbaa !15
    #dbg_declare(ptr %2, !14, !DIExpression(), !19)
  %3 = load i32, ptr %2, align 4, !dbg !20, !tbaa !15
  %4 = call i32 @foo(i32 noundef %3, i32 noundef 1), !dbg !21
  ret i32 0, !dbg !22
}

; Function Attrs: noinline nounwind uwtable
define internal i32 @foo(i32 noundef %0, i32 noundef %1) #1 !dbg !23 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, ptr %3, align 4, !tbaa !15
    #dbg_declare(ptr %3, !27, !DIExpression(), !29)
  store i32 %1, ptr %4, align 4, !tbaa !15
    #dbg_declare(ptr %4, !28, !DIExpression(), !30)
  %5 = load i32, ptr %3, align 4, !dbg !31, !tbaa !15
  %6 = call i32 @tar(i32 noundef %5), !dbg !32
  %7 = load i32, ptr %3, align 4, !dbg !33, !tbaa !15
  %8 = add nsw i32 %7, 1, !dbg !34
  %9 = call i32 @tar(i32 noundef %8), !dbg !35
  %10 = add nsw i32 %6, %9, !dbg !36
  ret i32 %10, !dbg !37
}

; CHECK: define internal fastcc void @foo(i32 noundef %0)

declare !dbg !38 i32 @tar(i32 noundef) #2

attributes #0 = { nounwind uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { noinline nounwind uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5, !6, !7}
!llvm.ident = !{!8}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 21.0.0git (git@github.com:yonghong-song/llvm-project.git 95c942ca729f6f240c408ceeb39d2d5f10a3b0d5)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test.c", directory: "/tmp/tests/sig-change/deadret", checksumkind: CSK_MD5, checksum: "728d225e6425c104712ae21cee1db99b")
!2 = !{i32 7, !"Dwarf Version", i32 5}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !{i32 1, !"wchar_size", i32 4}
!5 = !{i32 8, !"PIC Level", i32 2}
!6 = !{i32 7, !"PIE Level", i32 2}
!7 = !{i32 7, !"uwtable", i32 2}
!8 = !{!"clang version 21.0.0git (git@github.com:yonghong-song/llvm-project.git 95c942ca729f6f240c408ceeb39d2d5f10a3b0d5)"}
!9 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 6, type: !10, scopeLine: 7, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !13)
!10 = !DISubroutineType(types: !11)
!11 = !{!12, !12}
!12 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!13 = !{!14}
!14 = !DILocalVariable(name: "a", arg: 1, scope: !9, file: !1, line: 6, type: !12)
!15 = !{!16, !16, i64 0}
!16 = !{!"int", !17, i64 0}
!17 = !{!"omnipotent char", !18, i64 0}
!18 = !{!"Simple C/C++ TBAA"}
!19 = !DILocation(line: 6, column: 13, scope: !9)
!20 = !DILocation(line: 8, column: 7, scope: !9)
!21 = !DILocation(line: 8, column: 3, scope: !9)
!22 = !DILocation(line: 9, column: 3, scope: !9)
!23 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 2, type: !24, scopeLine: 3, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !26)

; CHECK: distinct !DISubprogram(name: "foo", scope: ![[#]], file: ![[#]], line: [[#]], type: ![[#]], scopeLine: [[#]], flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition | DISPFlagOptimized | DISPFlagArgChanged | DISPFlagRetvalRemoved, unit: !0, retainedNodes: ![[#]])

!24 = !DISubroutineType(types: !25)
!25 = !{!12, !12, !12}
!26 = !{!27, !28}
!27 = !DILocalVariable(name: "a", arg: 1, scope: !23, file: !1, line: 2, type: !12)
!28 = !DILocalVariable(name: "b", arg: 2, scope: !23, file: !1, line: 2, type: !12)
!29 = !DILocation(line: 2, column: 46, scope: !23)
!30 = !DILocation(line: 2, column: 53, scope: !23)
!31 = !DILocation(line: 4, column: 14, scope: !23)
!32 = !DILocation(line: 4, column: 10, scope: !23)
!33 = !DILocation(line: 4, column: 23, scope: !23)
!34 = !DILocation(line: 4, column: 25, scope: !23)
!35 = !DILocation(line: 4, column: 19, scope: !23)
!36 = !DILocation(line: 4, column: 17, scope: !23)
!37 = !DILocation(line: 4, column: 3, scope: !23)
!38 = !DISubprogram(name: "tar", scope: !1, file: !1, line: 1, type: !10, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized)
