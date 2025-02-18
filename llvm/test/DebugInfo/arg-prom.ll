; REQUIRES: x86-registered-target
; RUN: opt -O3 -S < %s | FileCheck %s
;
; Source code:
;   __attribute__((noinline)) static int is_absolute_path(const char *path)
;   {
;     return path[0] == '/';
;   }
;
;   int scpy(char *, const char *, int);
;   int quit(void);
;   const char *make_nonrelative_path(char *buf, int sz, const char *path)
;   {
;     if (is_absolute_path(path)) {
;       if (scpy(buf, path, sz) >= sz)
;         quit();
;     }
;     return buf;
;   }
; Compilation flag:
;   clang -O2 -g -S -emit-llvm -Xclang -disable-llvm-passes test.c

; ModuleID = 'test.c'
source_filename = "test.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Function Attrs: nounwind uwtable
define dso_local ptr @make_nonrelative_path(ptr noundef %0, i32 noundef %1, ptr noundef %2) #0 !dbg !9 {
  %4 = alloca ptr, align 8
  %5 = alloca i32, align 4
  %6 = alloca ptr, align 8
  store ptr %0, ptr %4, align 8, !tbaa !21
    #dbg_declare(ptr %4, !18, !DIExpression(), !26)
  store i32 %1, ptr %5, align 4, !tbaa !27
    #dbg_declare(ptr %5, !19, !DIExpression(), !29)
  store ptr %2, ptr %6, align 8, !tbaa !21
    #dbg_declare(ptr %6, !20, !DIExpression(), !30)
  %7 = load ptr, ptr %6, align 8, !dbg !31, !tbaa !21
  %8 = call i32 @is_absolute_path(ptr noundef %7), !dbg !33
  %9 = icmp ne i32 %8, 0, !dbg !33
  br i1 %9, label %10, label %20, !dbg !33

10:                                               ; preds = %3
  %11 = load ptr, ptr %4, align 8, !dbg !34, !tbaa !21
  %12 = load ptr, ptr %6, align 8, !dbg !37, !tbaa !21
  %13 = load i32, ptr %5, align 4, !dbg !38, !tbaa !27
  %14 = call i32 @scpy(ptr noundef %11, ptr noundef %12, i32 noundef %13), !dbg !39
  %15 = load i32, ptr %5, align 4, !dbg !40, !tbaa !27
  %16 = icmp sge i32 %14, %15, !dbg !41
  br i1 %16, label %17, label %19, !dbg !41

17:                                               ; preds = %10
  %18 = call i32 @quit(), !dbg !42
  br label %19, !dbg !42

19:                                               ; preds = %17, %10
  br label %20, !dbg !43

20:                                               ; preds = %19, %3
  %21 = load ptr, ptr %4, align 8, !dbg !44, !tbaa !21
  ret ptr %21, !dbg !45
}

; Function Attrs: noinline nounwind uwtable
define internal i32 @is_absolute_path(ptr noundef %0) #1 !dbg !46 {
  %2 = alloca ptr, align 8
  store ptr %0, ptr %2, align 8, !tbaa !21
    #dbg_declare(ptr %2, !50, !DIExpression(), !51)
  %3 = load ptr, ptr %2, align 8, !dbg !52, !tbaa !21
  %4 = getelementptr inbounds i8, ptr %3, i64 0, !dbg !52
  %5 = load i8, ptr %4, align 1, !dbg !52, !tbaa !53
  %6 = sext i8 %5 to i32, !dbg !52
  %7 = icmp eq i32 %6, 47, !dbg !54
  %8 = zext i1 %7 to i32, !dbg !54
  ret i32 %8, !dbg !55
}

; CHECK: define internal fastcc range(i32 0, 2) i32 @is_absolute_path(i8 {{.*}})

declare !dbg !56 i32 @scpy(ptr noundef, ptr noundef, i32 noundef) #2

declare !dbg !59 i32 @quit() #2

attributes #0 = { nounwind uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { noinline nounwind uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3, !4, !5, !6, !7}
!llvm.ident = !{!8}

!0 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 21.0.0git (git@github.com:yonghong-song/llvm-project.git bbfd0a15ade80596f6d6dde8add7d50f4875dde1)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!1 = !DIFile(filename: "test.c", directory: "/tmp/tests/sig-change/prom", checksumkind: CSK_MD5, checksum: "1befb35eb4507489630adb56cb20fe09")
!2 = !{i32 7, !"Dwarf Version", i32 5}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !{i32 1, !"wchar_size", i32 4}
!5 = !{i32 8, !"PIC Level", i32 2}
!6 = !{i32 7, !"PIE Level", i32 2}
!7 = !{i32 7, !"uwtable", i32 2}
!8 = !{!"clang version 21.0.0git (git@github.com:yonghong-song/llvm-project.git bbfd0a15ade80596f6d6dde8add7d50f4875dde1)"}
!9 = distinct !DISubprogram(name: "make_nonrelative_path", scope: !1, file: !1, line: 8, type: !10, scopeLine: 9, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !17)
!10 = !DISubroutineType(types: !11)
!11 = !{!12, !15, !16, !12}
!12 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !13, size: 64)
!13 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !14)
!14 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!15 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !14, size: 64)
!16 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!17 = !{!18, !19, !20}
!18 = !DILocalVariable(name: "buf", arg: 1, scope: !9, file: !1, line: 8, type: !15)
!19 = !DILocalVariable(name: "sz", arg: 2, scope: !9, file: !1, line: 8, type: !16)
!20 = !DILocalVariable(name: "path", arg: 3, scope: !9, file: !1, line: 8, type: !12)
!21 = !{!22, !22, i64 0}
!22 = !{!"p1 omnipotent char", !23, i64 0}
!23 = !{!"any pointer", !24, i64 0}
!24 = !{!"omnipotent char", !25, i64 0}
!25 = !{!"Simple C/C++ TBAA"}
!26 = !DILocation(line: 8, column: 41, scope: !9)
!27 = !{!28, !28, i64 0}
!28 = !{!"int", !24, i64 0}
!29 = !DILocation(line: 8, column: 50, scope: !9)
!30 = !DILocation(line: 8, column: 66, scope: !9)
!31 = !DILocation(line: 10, column: 30, scope: !32)
!32 = distinct !DILexicalBlock(scope: !9, file: !1, line: 10, column: 13)
!33 = !DILocation(line: 10, column: 13, scope: !32)
!34 = !DILocation(line: 11, column: 26, scope: !35)
!35 = distinct !DILexicalBlock(scope: !36, file: !1, line: 11, column: 21)
!36 = distinct !DILexicalBlock(scope: !32, file: !1, line: 10, column: 37)
!37 = !DILocation(line: 11, column: 31, scope: !35)
!38 = !DILocation(line: 11, column: 37, scope: !35)
!39 = !DILocation(line: 11, column: 21, scope: !35)
!40 = !DILocation(line: 11, column: 44, scope: !35)
!41 = !DILocation(line: 11, column: 41, scope: !35)
!42 = !DILocation(line: 12, column: 4, scope: !35)
!43 = !DILocation(line: 13, column: 9, scope: !36)
!44 = !DILocation(line: 14, column: 16, scope: !9)
!45 = !DILocation(line: 14, column: 9, scope: !9)
!46 = distinct !DISubprogram(name: "is_absolute_path", scope: !1, file: !1, line: 1, type: !47, scopeLine: 2, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition | DISPFlagOptimized, unit: !0, retainedNodes: !49)

; CHECK: distinct !DISubprogram(name: "is_absolute_path", scope: ![[#]], file: ![[#]], line: [[#]], type: ![[#]], scopeLine: [[#]], flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagLocalToUnit | DISPFlagDefinition | DISPFlagOptimized | DISPFlagArgChanged, unit: ![[#]], retainedNodes: ![[#]])

!47 = !DISubroutineType(types: !48)
!48 = !{!16, !12}
!49 = !{!50}
!50 = !DILocalVariable(name: "path", arg: 1, scope: !46, file: !1, line: 1, type: !12)
!51 = !DILocation(line: 1, column: 67, scope: !46)
!52 = !DILocation(line: 3, column: 16, scope: !46)
!53 = !{!24, !24, i64 0}
!54 = !DILocation(line: 3, column: 24, scope: !46)
!55 = !DILocation(line: 3, column: 9, scope: !46)
!56 = !DISubprogram(name: "scpy", scope: !1, file: !1, line: 6, type: !57, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized)
!57 = !DISubroutineType(types: !58)
!58 = !{!16, !15, !12, !16}
!59 = !DISubprogram(name: "quit", scope: !1, file: !1, line: 7, type: !60, flags: DIFlagPrototyped, spFlags: DISPFlagOptimized)
!60 = !DISubroutineType(types: !61)
!61 = !{!16}
