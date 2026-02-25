; RUN: llc -mtriple=bpf -filetype=obj -mcpu=v3 %s -o %t
; RUN: llvm-objcopy --dump-section='.BTF'=%t2 %t
; RUN: %python %p/../BTF/print_btf.py %t2 | FileCheck -check-prefixes=CHECK-BTF %s
; RUN: llvm-objdump --no-print-imm-hex -dr --no-show-raw-insn %t | FileCheck --check-prefix=CHECK-DUMP %s

; Source:
;   struct t {
;     char c[40000];
;     int ub;
;   } __attribute__((preserve_access_index));
;   void foo(volatile struct t *t) {
;     t->ub = 1;
;   }
;   int bar(volatile struct t *t) {
;     return t->ub;
;   }
; Using the following command:
;   clang -g -O2 -S -emit-llvm --target=bpf t.c -o t.ll


@"llvm.t:0:40001$0:1" = external global i64, !llvm.preserve.access.index !0 #0

; Function Attrs: nofree nounwind memory(readwrite, target_mem0: none, target_mem1: none)
define dso_local void @foo(ptr noundef %0) local_unnamed_addr #1 !dbg !21 {
    #dbg_value(ptr %0, !27, !DIExpression(), !28)
  %2 = load i64, ptr @"llvm.t:0:40001$0:1", align 8
  %3 = getelementptr i8, ptr %0, i64 %2
  %4 = tail call ptr @llvm.bpf.passthrough.p0.p0(i32 0, ptr %3)
  store volatile i32 1, ptr %4, align 4, !dbg !29, !tbaa !30
  ret void, !dbg !32
}

; Function Attrs: nofree nounwind memory(readwrite, target_mem0: none, target_mem1: none)
define dso_local i32 @bar(ptr noundef %0) local_unnamed_addr #1 !dbg !33 {
    #dbg_value(ptr %0, !37, !DIExpression(), !38)
  %2 = load i64, ptr @"llvm.t:0:40001$0:1", align 8
  %3 = getelementptr i8, ptr %0, i64 %2
  %4 = tail call ptr @llvm.bpf.passthrough.p0.p0(i32 1, ptr %3)
  %5 = load volatile i32, ptr %4, align 4, !dbg !39, !tbaa !30
  ret i32 %5, !dbg !40
}

; CHECK-BTF:      [1] PTR '(anon)' type_id=2
; CHECK-BTF-NEXT: [2] VOLATILE '(anon)' type_id=3
; CHECK-BTF-NEXT: [3] STRUCT 't' size=65544 vlen=2
; CHECK-BTF-NEXT:         'c' type_id=5 bits_offset=0
; CHECK-BTF-NEXT:         'ub' type_id=7 bits_offset=524320
; CHECK-BTF-NEXT: [4] INT 'char' size=1 bits_offset=0 nr_bits=8 encoding=SIGNED
; CHECK-BTF-NEXT: [5] ARRAY '(anon)' type_id=4 index_type_id=6 nr_elems=40000
; CHECK-BTF-NEXT: [6] INT '__ARRAY_SIZE_TYPE__' size=4 bits_offset=0 nr_bits=32 encoding=(none)
; CHECK-BTF-NEXT: [7] INT 'int' size=4 bits_offset=0 nr_bits=32 encoding=SIGNED
; CHECK-BTF-NEXT: [8] FUNC_PROTO '(anon)' ret_type_id=0 vlen=1
; CHECK-BTF-NEXT:         't' type_id=1
; CHECK-BTF-NEXT: [9] FUNC 'foo' type_id=8 linkage=global
; CHECK-BTF-NEXT: [10] FUNC_PROTO '(anon)' ret_type_id=7 vlen=1
; CHECK-BTF-NEXT:          't' type_id=1
; CHECK-BTF-NEXT: [11] FUNC 'bar' type_id=10 linkage=global

; CHECK-DUMP:      <foo>:
; CHECK-DUMP-NEXT: 0:       r2 = 40001
; CHECK-DUMP-NEXT:          0000000000000000:  CO-RE <byte_off> [3] struct t::ub (0:1)
; CHECK-DUMP-NEXT: 1:       r1 += r2
; CHECK-DUMP-NEXT: 2:       w2 = 1
; CHECK-DUMP-NEXT: 3:       *(u32 *)(r1 + 0) = w2
; CHECK-DUMP-NEXT: 4:       exit
; CHECK-DUMP:      <bar>:
; CHECK-DUMP-NEXT: 5:       r2 = 40001
; CHECK-DUMP-NEXT:          0000000000000028:  CO-RE <byte_off> [3] struct t::ub (0:1)
; CHECK-DUMP-NEXT: 6:       r1 += r2
; CHECK-DUMP-NEXT: 7:       w0 = *(u32 *)(r1 + 0)
; CHECK-DUMP-NEXT: 8:       exit

; Function Attrs: nofree nosync nounwind memory(none)
declare ptr @llvm.bpf.passthrough.p0.p0(i32, ptr) #2

attributes #0 = { "btf_ama" }
attributes #1 = { nofree nounwind memory(readwrite, target_mem0: none, target_mem1: none) "frame-pointer"="all" "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
attributes #2 = { nofree nosync nounwind memory(none) }

!llvm.dbg.cu = !{!10}
!llvm.module.flags = !{!11, !12, !13, !14, !15}
!llvm.ident = !{!16}
!llvm.errno.tbaa = !{!17}

!0 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "t", file: !1, line: 1, size: 524352, elements: !2)
!1 = !DIFile(filename: "t.c", directory: "/tmp/home/yhs/tmp/tmp3", checksumkind: CSK_MD5, checksum: "caaf4a6cf60507ab36f683c70a9bebee")
!2 = !{!3, !8}
!3 = !DIDerivedType(tag: DW_TAG_member, name: "c", scope: !0, file: !1, line: 2, baseType: !4, size: 524312)
!4 = !DICompositeType(tag: DW_TAG_array_type, baseType: !5, size: 524312, elements: !6)
!5 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!6 = !{!7}
!7 = !DISubrange(count: 40000)
!8 = !DIDerivedType(tag: DW_TAG_member, name: "ub", scope: !0, file: !1, line: 3, baseType: !9, size: 32, offset: 524320)
!9 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!10 = distinct !DICompileUnit(language: DW_LANG_C11, file: !1, producer: "clang version 23.0.0git (https://github.com/llvm/llvm-project.git 8d2d8be58b8d599e3a2164041639ede33acab327)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
!11 = !{i32 7, !"Dwarf Version", i32 5}
!12 = !{i32 2, !"Debug Info Version", i32 3}
!13 = !{i32 1, !"wchar_size", i32 4}
!14 = !{i32 7, !"frame-pointer", i32 2}
!15 = !{i32 7, !"debug-info-assignment-tracking", i1 true}
!16 = !{!"clang version 23.0.0git (https://github.com/llvm/llvm-project.git 8d2d8be58b8d599e3a2164041639ede33acab327)"}
!17 = !{!18, !18, i64 0}
!18 = !{!"int", !19, i64 0}
!19 = !{!"omnipotent char", !20, i64 0}
!20 = !{!"Simple C/C++ TBAA"}
!21 = distinct !DISubprogram(name: "foo", scope: !1, file: !1, line: 6, type: !22, scopeLine: 6, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !10, retainedNodes: !26, keyInstructions: true)
!22 = !DISubroutineType(types: !23)
!23 = !{null, !24}
!24 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !25, size: 64)
!25 = !DIDerivedType(tag: DW_TAG_volatile_type, baseType: !0)
!26 = !{!27}
!27 = !DILocalVariable(name: "t", arg: 1, scope: !21, file: !1, line: 6, type: !24)
!28 = !DILocation(line: 0, scope: !21)
!29 = !DILocation(line: 7, column: 9, scope: !21, atomGroup: 1, atomRank: 1)
!30 = !{!31, !18, i64 40001}
!31 = !{!"t", !19, i64 0, !18, i64 40001}
!32 = !DILocation(line: 8, column: 1, scope: !21, atomGroup: 2, atomRank: 1)
!33 = distinct !DISubprogram(name: "bar", scope: !1, file: !1, line: 10, type: !34, scopeLine: 10, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !10, retainedNodes: !36, keyInstructions: true)
!34 = !DISubroutineType(types: !35)
!35 = !{!9, !24}
!36 = !{!37}
!37 = !DILocalVariable(name: "t", arg: 1, scope: !33, file: !1, line: 10, type: !24)
!38 = !DILocation(line: 0, scope: !33)
!39 = !DILocation(line: 11, column: 13, scope: !33, atomGroup: 1, atomRank: 2)
!40 = !DILocation(line: 11, column: 3, scope: !33, atomGroup: 1, atomRank: 1)
