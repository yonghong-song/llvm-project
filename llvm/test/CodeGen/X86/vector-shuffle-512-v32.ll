; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-apple-darwin -mcpu=knl | FileCheck --check-prefixes=ALL,KNL %s
; RUN: llc < %s -mtriple=x86_64-apple-darwin -mcpu=skx | FileCheck --check-prefixes=ALL,SKX %s

target triple = "x86_64-unknown-unknown"

define <32 x i16> @shuffle_v32i16(<32 x i16> %a)  {
; KNL-LABEL: shuffle_v32i16:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw %xmm0, %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw %xmm0, %zmm0
; SKX-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> poison, <32 x i32> zeroinitializer
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v32i16_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08(<32 x i16> %a)  {
; KNL-LABEL: shuffle_v32i16_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08:
; KNL:       ## %bb.0:
; KNL-NEXT:    vextracti128 $1, %ymm0, %xmm0
; KNL-NEXT:    vpbroadcastw %xmm0, %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08_08:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastd {{.*#+}} zmm1 = [8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8]
; SKX-NEXT:    vpermw %zmm0, %zmm1, %zmm0
; SKX-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> poison, <32 x i32> <i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v32i16_02_05_u_u_07_u_0a_01_00_05_u_04_07_u_0a_01_02_05_u_u_07_u_0a_01_00_05_u_04_07_u_0a_1f(<32 x i16> %a)  {
; KNL-LABEL: shuffle_v32i16_02_05_u_u_07_u_0a_01_00_05_u_04_07_u_0a_01_02_05_u_u_07_u_0a_01_00_05_u_04_07_u_0a_1f:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpshufb {{.*#+}} ymm1 = ymm0[4,5,10,11,4,5,6,7,14,15,2,3,4,5,2,3,20,21,26,27,20,21,22,23,30,31,18,19,20,21,18,19]
; KNL-NEXT:    vpermq {{.*#+}} ymm2 = ymm0[2,3,0,1]
; KNL-NEXT:    vpshufb {{.*#+}} ymm3 = ymm2[0,1,10,11,8,9,8,9,14,15,6,7,4,5,14,15,16,17,26,27,24,25,24,25,30,31,22,23,20,21,30,31]
; KNL-NEXT:    vmovdqa {{.*#+}} ymm4 = [255,255,255,255,u,u,u,u,255,255,u,u,0,0,255,255,0,0,0,0,u,u,0,0,0,0,u,u,255,255,u,u]
; KNL-NEXT:    vpblendvb %ymm4, %ymm1, %ymm3, %ymm3
; KNL-NEXT:    vextracti64x4 $1, %zmm0, %ymm0
; KNL-NEXT:    vpblendw {{.*#+}} ymm0 = ymm3[0,1,2,3,4,5,6],ymm0[7],ymm3[8,9,10,11,12,13,14],ymm0[15]
; KNL-NEXT:    vpblendd {{.*#+}} ymm0 = ymm3[0,1,2,3],ymm0[4,5,6,7]
; KNL-NEXT:    vpshufb {{.*#+}} ymm2 = ymm2[0,1,10,11,8,9,8,9,14,15,2,3,4,5,2,3,16,17,26,27,24,25,24,25,30,31,18,19,20,21,18,19]
; KNL-NEXT:    vmovdqa {{.*#+}} ymm3 = [0,0,0,0,u,u,u,u,0,0,u,u,255,255,0,0,255,255,255,255,u,u,255,255,255,255,u,u,0,0,255,255]
; KNL-NEXT:    vpblendvb %ymm3, %ymm2, %ymm1, %ymm1
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm1, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_02_05_u_u_07_u_0a_01_00_05_u_04_07_u_0a_01_02_05_u_u_07_u_0a_01_00_05_u_04_07_u_0a_1f:
; SKX:       ## %bb.0:
; SKX-NEXT:    vmovdqa64 {{.*#+}} zmm1 = [2,5,u,u,7,u,10,1,0,5,u,4,7,u,10,1,2,5,u,u,7,u,10,1,0,5,u,4,7,u,10,31]
; SKX-NEXT:    vpermw %zmm0, %zmm1, %zmm0
; SKX-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> poison, <32 x i32> <i32 2, i32 5, i32 poison, i32 poison, i32 7, i32 poison, i32 10, i32 1,  i32 0, i32 5, i32 poison, i32 4, i32 7, i32 poison, i32 10, i32 1, i32 2, i32 5, i32 poison, i32 poison, i32 7, i32 poison, i32 10, i32 1,  i32 0, i32 5, i32 poison, i32 4, i32 7, i32 poison, i32 10, i32 31>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v32i16_0f_1f_0e_16_0d_1d_04_1e_0b_1b_0a_1a_09_19_08_18_0f_1f_0e_16_0d_1d_04_1e_0b_1b_0a_1a_09_19_08_38(<32 x i16> %a, <32 x i16> %b)  {
; KNL-LABEL: shuffle_v32i16_0f_1f_0e_16_0d_1d_04_1e_0b_1b_0a_1a_09_19_08_18_0f_1f_0e_16_0d_1d_04_1e_0b_1b_0a_1a_09_19_08_38:
; KNL:       ## %bb.0:
; KNL-NEXT:    vextracti64x4 $1, %zmm0, %ymm2
; KNL-NEXT:    vpermq {{.*#+}} ymm2 = ymm2[3,1,2,3]
; KNL-NEXT:    vpshufb {{.*#+}} ymm3 = ymm2[6,7,12,13,2,3,0,1,u,u,u,u,u,u,u,u,22,23,20,21,18,19,u,u,u,u,u,u,u,u,u,u]
; KNL-NEXT:    vpermq {{.*#+}} ymm0 = ymm0[3,1,2,3]
; KNL-NEXT:    vpshufb {{.*#+}} ymm0 = ymm0[6,7,4,5,2,3,8,9,u,u,u,u,u,u,u,u,22,23,20,21,18,19,16,17,u,u,u,u,u,u,u,u]
; KNL-NEXT:    vpunpcklwd {{.*#+}} ymm3 = ymm0[0],ymm3[0],ymm0[1],ymm3[1],ymm0[2],ymm3[2],ymm0[3],ymm3[3],ymm0[8],ymm3[8],ymm0[9],ymm3[9],ymm0[10],ymm3[10],ymm0[11],ymm3[11]
; KNL-NEXT:    vextracti32x4 $3, %zmm1, %xmm1
; KNL-NEXT:    vpbroadcastw %xmm1, %ymm1
; KNL-NEXT:    vpblendw {{.*#+}} ymm1 = ymm3[0,1,2,3,4,5,6],ymm1[7],ymm3[8,9,10,11,12,13,14],ymm1[15]
; KNL-NEXT:    vpblendd {{.*#+}} ymm1 = ymm3[0,1,2,3],ymm1[4,5,6,7]
; KNL-NEXT:    vpshufb {{.*#+}} ymm2 = ymm2[6,7,12,13,2,3,0,1,u,u,u,u,u,u,u,u,22,23,20,21,18,19,16,17,u,u,u,u,u,u,u,u]
; KNL-NEXT:    vpunpcklwd {{.*#+}} ymm0 = ymm0[0],ymm2[0],ymm0[1],ymm2[1],ymm0[2],ymm2[2],ymm0[3],ymm2[3],ymm0[8],ymm2[8],ymm0[9],ymm2[9],ymm0[10],ymm2[10],ymm0[11],ymm2[11]
; KNL-NEXT:    vinserti64x4 $1, %ymm1, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_0f_1f_0e_16_0d_1d_04_1e_0b_1b_0a_1a_09_19_08_18_0f_1f_0e_16_0d_1d_04_1e_0b_1b_0a_1a_09_19_08_38:
; SKX:       ## %bb.0:
; SKX-NEXT:    vmovdqa64 {{.*#+}} zmm2 = [15,31,14,22,13,29,4,28,11,27,10,26,9,25,8,24,15,31,14,22,13,29,4,28,11,27,10,26,9,25,8,56]
; SKX-NEXT:    vpermt2w %zmm1, %zmm2, %zmm0
; SKX-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> %b, <32 x i32> <i32 15, i32 31, i32 14, i32 22, i32 13, i32 29, i32 4, i32 28, i32 11, i32 27, i32 10, i32 26, i32 9, i32 25, i32 8, i32 24, i32 15, i32 31, i32 14, i32 22, i32 13, i32 29, i32 4, i32 28, i32 11, i32 27, i32 10, i32 26, i32 9, i32 25, i32 8, i32 56>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v16i32_0_32_1_33_2_34_3_35_8_40_9_41_u_u_u_u(<32 x i16> %a, <32 x i16> %b)  {
; ALL-LABEL: shuffle_v16i32_0_32_1_33_2_34_3_35_8_40_9_41_u_u_u_u:
; ALL:       ## %bb.0:
; ALL-NEXT:    vpunpcklwd {{.*#+}} ymm0 = ymm0[0],ymm1[0],ymm0[1],ymm1[1],ymm0[2],ymm1[2],ymm0[3],ymm1[3],ymm0[8],ymm1[8],ymm0[9],ymm1[9],ymm0[10],ymm1[10],ymm0[11],ymm1[11]
; ALL-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> %b, <32 x i32> <i32 0, i32 32, i32 1, i32 33, i32 2, i32 34, i32 3, i32 35, i32 8, i32 40, i32 9, i32 41, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v16i32_4_36_5_37_6_38_7_39_12_44_13_45_u_u_u_u(<32 x i16> %a, <32 x i16> %b)  {
; ALL-LABEL: shuffle_v16i32_4_36_5_37_6_38_7_39_12_44_13_45_u_u_u_u:
; ALL:       ## %bb.0:
; ALL-NEXT:    vpunpckhwd {{.*#+}} ymm0 = ymm0[4],ymm1[4],ymm0[5],ymm1[5],ymm0[6],ymm1[6],ymm0[7],ymm1[7],ymm0[12],ymm1[12],ymm0[13],ymm1[13],ymm0[14],ymm1[14],ymm0[15],ymm1[15]
; ALL-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> %b, <32 x i32> <i32 4, i32 36, i32 5, i32 37, i32 6, i32 38, i32 7, i32 39, i32 12, i32 44, i32 13, i32 45, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison, i32 poison>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v32i16_1_z_3_z_5_z_7_z_9_z_11_z_13_z_15_z_17_z_19_z_21_z_23_z_25_z_27_z_29_z_31_z(<32 x i16> %a, <32 x i16> %b)  {
; ALL-LABEL: shuffle_v32i16_1_z_3_z_5_z_7_z_9_z_11_z_13_z_15_z_17_z_19_z_21_z_23_z_25_z_27_z_29_z_31_z:
; ALL:       ## %bb.0:
; ALL-NEXT:    vpsrld $16, %zmm0, %zmm0
; ALL-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> zeroinitializer, <32 x i32> <i32 1, i32 34, i32 3, i32 34, i32 5, i32 34, i32 7, i32 34, i32 9, i32 34, i32 11, i32 34, i32 13, i32 34, i32 15, i32 34, i32 17, i32 34, i32 19, i32 34, i32 21, i32 34, i32 23, i32 34, i32 25, i32 34, i32 27, i32 34, i32 29, i32 34, i32 31, i32 34>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v32i16_z_0_z_2_z_4_z_6_z_8_z_10_z_12_z_14_z_16_z_18_z_20_z_22_z_24_z_26_z_28_z_30(<32 x i16> %a, <32 x i16> %b)  {
; ALL-LABEL: shuffle_v32i16_z_0_z_2_z_4_z_6_z_8_z_10_z_12_z_14_z_16_z_18_z_20_z_22_z_24_z_26_z_28_z_30:
; ALL:       ## %bb.0:
; ALL-NEXT:    vpslld $16, %zmm0, %zmm0
; ALL-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> zeroinitializer, <32 x i32> <i32 34, i32 0, i32 34, i32 2, i32 34, i32 4, i32 34, i32 6, i32 34, i32 8, i32 34, i32 10, i32 34, i32 12, i32 34, i32 14, i32 34, i32 16, i32 34, i32 18, i32 34, i32 20, i32 34, i32 22, i32 34, i32 24, i32 34, i32 26, i32 34, i32 28, i32 34, i32 30>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v32i16_1_1_0_0_4_5_6_7_9_9_8_8_12_13_14_15_17_17_16_16_20_21_22_23_25_25_24_24_28_29_30_31(<32 x i16> %a, <32 x i16> %b)  {
; KNL-LABEL: shuffle_v32i16_1_1_0_0_4_5_6_7_9_9_8_8_12_13_14_15_17_17_16_16_20_21_22_23_25_25_24_24_28_29_30_31:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpshuflw {{.*#+}} ymm1 = ymm0[1,1,0,0,4,5,6,7,9,9,8,8,12,13,14,15]
; KNL-NEXT:    vextracti64x4 $1, %zmm0, %ymm0
; KNL-NEXT:    vpshuflw {{.*#+}} ymm0 = ymm0[1,1,0,0,4,5,6,7,9,9,8,8,12,13,14,15]
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm1, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_1_1_0_0_4_5_6_7_9_9_8_8_12_13_14_15_17_17_16_16_20_21_22_23_25_25_24_24_28_29_30_31:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpshuflw {{.*#+}} zmm0 = zmm0[1,1,0,0,4,5,6,7,9,9,8,8,12,13,14,15,17,17,16,16,20,21,22,23,25,25,24,24,28,29,30,31]
; SKX-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> zeroinitializer, <32 x i32> <i32 1, i32 1, i32 0, i32 0, i32 4, i32 5, i32 6, i32 7, i32 9, i32 9, i32 8, i32 8, i32 12, i32 13, i32 14, i32 15, i32 17, i32 17, i32 16, i32 16, i32 20, i32 21, i32 22, i32 23, i32 25, i32 25, i32 24, i32 24, i32 28, i32 29, i32 30, i32 31>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v32i16_0_1_2_3_5_5_4_4_8_9_10_11_13_13_12_12_16_17_18_19_21_21_20_20_24_25_26_27_29_29_28_28(<32 x i16> %a, <32 x i16> %b)  {
; KNL-LABEL: shuffle_v32i16_0_1_2_3_5_5_4_4_8_9_10_11_13_13_12_12_16_17_18_19_21_21_20_20_24_25_26_27_29_29_28_28:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpshufhw {{.*#+}} ymm1 = ymm0[0,1,2,3,5,5,4,4,8,9,10,11,13,13,12,12]
; KNL-NEXT:    vextracti64x4 $1, %zmm0, %ymm0
; KNL-NEXT:    vpshufhw {{.*#+}} ymm0 = ymm0[0,1,2,3,5,5,4,4,8,9,10,11,13,13,12,12]
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm1, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_0_1_2_3_5_5_4_4_8_9_10_11_13_13_12_12_16_17_18_19_21_21_20_20_24_25_26_27_29_29_28_28:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpshufhw {{.*#+}} zmm0 = zmm0[0,1,2,3,5,5,4,4,8,9,10,11,13,13,12,12,16,17,18,19,21,21,20,20,24,25,26,27,29,29,28,28]
; SKX-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> zeroinitializer, <32 x i32> <i32 0, i32 1, i32 2, i32 3, i32 5, i32 5, i32 4, i32 4, i32 8, i32 9, i32 10, i32 11, i32 13, i32 13, i32 12, i32 12, i32 16, i32 17, i32 18, i32 19, i32 21, i32 21, i32 20, i32 20, i32 24, i32 25, i32 26, i32 27, i32 29, i32 29, i32 28, i32 28>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v32i16_1_1_0_0_5_5_4_4_9_9_11_11_13_13_12_12_17_17_19_19_21_21_20_20_25_25_27_27_29_29_28_28(<32 x i16> %a, <32 x i16> %b)  {
; KNL-LABEL: shuffle_v32i16_1_1_0_0_5_5_4_4_9_9_11_11_13_13_12_12_17_17_19_19_21_21_20_20_25_25_27_27_29_29_28_28:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpshuflw {{.*#+}} ymm1 = ymm0[1,1,0,0,4,5,6,7,9,9,8,8,12,13,14,15]
; KNL-NEXT:    vpshufhw {{.*#+}} ymm1 = ymm1[0,1,2,3,5,5,4,4,8,9,10,11,13,13,12,12]
; KNL-NEXT:    vextracti64x4 $1, %zmm0, %ymm0
; KNL-NEXT:    vpshuflw {{.*#+}} ymm0 = ymm0[1,1,0,0,4,5,6,7,9,9,8,8,12,13,14,15]
; KNL-NEXT:    vpshufhw {{.*#+}} ymm0 = ymm0[0,1,2,3,5,5,4,4,8,9,10,11,13,13,12,12]
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm1, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_1_1_0_0_5_5_4_4_9_9_11_11_13_13_12_12_17_17_19_19_21_21_20_20_25_25_27_27_29_29_28_28:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpshufb {{.*#+}} zmm0 = zmm0[2,3,2,3,0,1,0,1,10,11,10,11,8,9,8,9,18,19,18,19,16,17,16,17,26,27,26,27,24,25,24,25,34,35,34,35,32,33,32,33,42,43,42,43,40,41,40,41,50,51,50,51,48,49,48,49,58,59,58,59,56,57,56,57]
; SKX-NEXT:    retq
  %c = shufflevector <32 x i16> %a, <32 x i16> zeroinitializer, <32 x i32> <i32 1, i32 1, i32 0, i32 0, i32 5, i32 5, i32 4, i32 4, i32 9, i32 9, i32 8, i32 8, i32 13, i32 13, i32 12, i32 12, i32 17, i32 17, i32 16, i32 16, i32 21, i32 21, i32 20, i32 20, i32 25, i32 25, i32 24, i32 24, i32 29, i32 29, i32 28, i32 28>
  ret <32 x i16> %c
}

define <32 x i16> @shuffle_v32i16_01_00_03_02_05_04_07_06_09_08_11_10_13_12_15_14_17_16_19_18_21_20_23_22_25_24_27_26_29_28_31_30(<32 x i16> %a) {
; ALL-LABEL: shuffle_v32i16_01_00_03_02_05_04_07_06_09_08_11_10_13_12_15_14_17_16_19_18_21_20_23_22_25_24_27_26_29_28_31_30:
; ALL:       ## %bb.0:
; ALL-NEXT:    vprold $16, %zmm0, %zmm0
; ALL-NEXT:    retq
  %shuffle = shufflevector <32 x i16> %a, <32 x i16> poison, <32 x i32> <i32 1, i32 0, i32 3, i32 2, i32 5, i32 4, i32 7, i32 6, i32 9, i32 8, i32 11, i32 10, i32 13, i32 12, i32 15, i32 14, i32 17, i32 16, i32 19, i32 18, i32 21, i32 20, i32 23, i32 22, i32 25, i32 24, i32 27, i32 26, i32 29, i32 28, i32 31, i32 30>
  ret <32 x i16> %shuffle
}

define <32 x i16> @shuffle_v32i16_03_00_01_02_07_04_05_06_11_08_09_10_15_12_13_14_19_16_17_18_23_20_21_22_27_24_25_26_31_28_29_30(<32 x i16> %a) {
; ALL-LABEL: shuffle_v32i16_03_00_01_02_07_04_05_06_11_08_09_10_15_12_13_14_19_16_17_18_23_20_21_22_27_24_25_26_31_28_29_30:
; ALL:       ## %bb.0:
; ALL-NEXT:    vprolq $16, %zmm0, %zmm0
; ALL-NEXT:    retq
  %shuffle = shufflevector <32 x i16> %a, <32 x i16> poison, <32 x i32> <i32 3, i32 0, i32 1, i32 2, i32 7, i32 4, i32 5, i32 6, i32 11, i32 8, i32 9, i32 10, i32 15, i32 12, i32 13, i32 14, i32 19, i32 16, i32 17, i32 18, i32 23, i32 20, i32 21, i32 22, i32 27, i32 24, i32 25, i32 26, i32 31, i32 28, i32 29, i32 30>
  ret <32 x i16> %shuffle
}

define <32 x i16> @shuffle_v32i16_0zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz(<32 x i16> %a) {
; KNL-LABEL: shuffle_v32i16_0zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:
; KNL:       ## %bb.0:
; KNL-NEXT:    vmovd {{.*#+}} xmm1 = [65535,0,0,0]
; KNL-NEXT:    vpandq %zmm1, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_0zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:
; SKX:       ## %bb.0:
; SKX-NEXT:    vmovaps {{.*#+}} xmm1 = [65535,0,0,0]
; SKX-NEXT:    vandps %zmm1, %zmm0, %zmm0
; SKX-NEXT:    retq
  %shuffle = shufflevector <32 x i16> %a, <32 x i16> zeroinitializer, <32 x i32> <i32 0, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32, i32 32>
  ret <32 x i16> %shuffle
}

define <32 x i16> @shuffle_v32i16_ashr_00_02_04_06_32_34_36_38_08_10_12_14_40_42_44_46_16_18_20_22_48_50_52_54_24_26_28_30_56_58_60_62(<16 x i32> %a0, <16 x i32> %a1) nounwind {
; KNL-LABEL: shuffle_v32i16_ashr_00_02_04_06_32_34_36_38_08_10_12_14_40_42_44_46_16_18_20_22_48_50_52_54_24_26_28_30_56_58_60_62:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpsrad $25, %zmm0, %zmm0
; KNL-NEXT:    vpsrad $25, %zmm1, %zmm1
; KNL-NEXT:    vextracti64x4 $1, %zmm1, %ymm2
; KNL-NEXT:    vextracti64x4 $1, %zmm0, %ymm3
; KNL-NEXT:    vpackssdw %ymm2, %ymm3, %ymm2
; KNL-NEXT:    vpackssdw %ymm1, %ymm0, %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm2, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_ashr_00_02_04_06_32_34_36_38_08_10_12_14_40_42_44_46_16_18_20_22_48_50_52_54_24_26_28_30_56_58_60_62:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpsrad $25, %zmm0, %zmm0
; SKX-NEXT:    vpsrad $25, %zmm1, %zmm1
; SKX-NEXT:    vpackssdw %zmm1, %zmm0, %zmm0
; SKX-NEXT:    retq
  %1 = ashr <16 x i32> %a0, <i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25>
  %2 = ashr <16 x i32> %a1, <i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25>
  %3 = bitcast <16 x i32> %1 to <32 x i16>
  %4 = bitcast <16 x i32> %2 to <32 x i16>
  %5 = shufflevector <32 x i16> %3, <32 x i16> %4, <32 x i32> <i32 0, i32 2, i32 4, i32 6, i32 32, i32 34, i32 36, i32 38, i32 8, i32 10, i32 12, i32 14, i32 40, i32 42, i32 44, i32 46, i32 16, i32 18, i32 20, i32 22, i32 48, i32 50, i32 52, i32 54, i32 24, i32 26, i32 28, i32 30, i32 56, i32 58, i32 60, i32 62>
  ret <32 x i16> %5
}

define <32 x i16> @shuffle_v32i16_lshr_00_02_04_06_32_34_36_38_08_10_12_14_40_42_44_46_16_18_20_22_48_50_52_54_24_26_28_30_56_58_60_62(<16 x i32> %a0, <16 x i32> %a1) nounwind {
; KNL-LABEL: shuffle_v32i16_lshr_00_02_04_06_32_34_36_38_08_10_12_14_40_42_44_46_16_18_20_22_48_50_52_54_24_26_28_30_56_58_60_62:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpsrld $25, %zmm0, %zmm0
; KNL-NEXT:    vpsrld $25, %zmm1, %zmm1
; KNL-NEXT:    vextracti64x4 $1, %zmm1, %ymm2
; KNL-NEXT:    vextracti64x4 $1, %zmm0, %ymm3
; KNL-NEXT:    vpackusdw %ymm2, %ymm3, %ymm2
; KNL-NEXT:    vpackusdw %ymm1, %ymm0, %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm2, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_lshr_00_02_04_06_32_34_36_38_08_10_12_14_40_42_44_46_16_18_20_22_48_50_52_54_24_26_28_30_56_58_60_62:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpsrld $25, %zmm0, %zmm0
; SKX-NEXT:    vpsrld $25, %zmm1, %zmm1
; SKX-NEXT:    vpackusdw %zmm1, %zmm0, %zmm0
; SKX-NEXT:    retq
  %1 = lshr <16 x i32> %a0, <i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25>
  %2 = lshr <16 x i32> %a1, <i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25, i32 25>
  %3 = bitcast <16 x i32> %1 to <32 x i16>
  %4 = bitcast <16 x i32> %2 to <32 x i16>
  %5 = shufflevector <32 x i16> %3, <32 x i16> %4, <32 x i32> <i32 0, i32 2, i32 4, i32 6, i32 32, i32 34, i32 36, i32 38, i32 8, i32 10, i32 12, i32 14, i32 40, i32 42, i32 44, i32 46, i32 16, i32 18, i32 20, i32 22, i32 48, i32 50, i32 52, i32 54, i32 24, i32 26, i32 28, i32 30, i32 56, i32 58, i32 60, i32 62>
  ret <32 x i16> %5
}

define <32 x i16> @shuffle_v32i16_concat_00_00_00_00_04_05_06_07_08_08_08_08_12_13_14_15_16_16_16_16_20_21_22_23_24_24_24_24_28_29_30_31(<16 x i16> %a0, <16 x i16> %a1) {
; KNL-LABEL: shuffle_v32i16_concat_00_00_00_00_04_05_06_07_08_08_08_08_12_13_14_15_16_16_16_16_20_21_22_23_24_24_24_24_28_29_30_31:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpshuflw {{.*#+}} ymm0 = ymm0[0,0,0,0,4,5,6,7,8,8,8,8,12,13,14,15]
; KNL-NEXT:    vpshuflw {{.*#+}} ymm1 = ymm1[0,0,0,0,4,5,6,7,8,8,8,8,12,13,14,15]
; KNL-NEXT:    vinserti64x4 $1, %ymm1, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_concat_00_00_00_00_04_05_06_07_08_08_08_08_12_13_14_15_16_16_16_16_20_21_22_23_24_24_24_24_28_29_30_31:
; SKX:       ## %bb.0:
; SKX-NEXT:    ## kill: def $ymm0 killed $ymm0 def $zmm0
; SKX-NEXT:    vinserti64x4 $1, %ymm1, %zmm0, %zmm0
; SKX-NEXT:    vpshuflw {{.*#+}} zmm0 = zmm0[0,0,0,0,4,5,6,7,8,8,8,8,12,13,14,15,16,16,16,16,20,21,22,23,24,24,24,24,28,29,30,31]
; SKX-NEXT:    retq
  %shuffle = shufflevector <16 x i16> %a0, <16 x i16> %a1, <32 x i32> <i32 0, i32 0, i32 0, i32 0, i32 4, i32 5, i32 6, i32 7, i32 8, i32 8, i32 8, i32 8, i32 12, i32 13, i32 14, i32 15, i32 16, i32 16, i32 16, i32 16, i32 20, i32 21, i32 22, i32 23, i32 24, i32 24, i32 24, i32 24, i32 28, i32 29, i32 30, i32 31>
  ret <32 x i16> %shuffle
}

define <32 x i16> @shuffle_v32i16_concat_00_01_02_03_04_07_06_04_08_09_10_11_12_15_14_12_16_17_18_19_20_23_22_20_24_25_26_27_28_31_30_28(<16 x i16> %a0, <16 x i16> %a1) {
; KNL-LABEL: shuffle_v32i16_concat_00_01_02_03_04_07_06_04_08_09_10_11_12_15_14_12_16_17_18_19_20_23_22_20_24_25_26_27_28_31_30_28:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpshufhw {{.*#+}} ymm0 = ymm0[0,1,2,3,4,7,6,4,8,9,10,11,12,15,14,12]
; KNL-NEXT:    vpshufhw {{.*#+}} ymm1 = ymm1[0,1,2,3,4,7,6,4,8,9,10,11,12,15,14,12]
; KNL-NEXT:    vinserti64x4 $1, %ymm1, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_concat_00_01_02_03_04_07_06_04_08_09_10_11_12_15_14_12_16_17_18_19_20_23_22_20_24_25_26_27_28_31_30_28:
; SKX:       ## %bb.0:
; SKX-NEXT:    ## kill: def $ymm0 killed $ymm0 def $zmm0
; SKX-NEXT:    vinserti64x4 $1, %ymm1, %zmm0, %zmm0
; SKX-NEXT:    vpshufhw {{.*#+}} zmm0 = zmm0[0,1,2,3,4,7,6,4,8,9,10,11,12,15,14,12,16,17,18,19,20,23,22,20,24,25,26,27,28,31,30,28]
; SKX-NEXT:    retq
  %shuffle = shufflevector <16 x i16> %a0, <16 x i16> %a1, <32 x i32> <i32 0, i32 1, i32 2, i32 3, i32 4, i32 7, i32 6, i32 4, i32 8, i32 9, i32 10, i32 11, i32 12, i32 15, i32 14, i32 12, i32 16, i32 17, i32 18, i32 19, i32 20, i32 23, i32 22, i32 20, i32 24, i32 25, i32 26, i32 27, i32 28, i32 31, i32 30, i32 28>
  ret <32 x i16> %shuffle
}

define <32 x i16> @insert_dup_mem_v32i16_i32(ptr %ptr) {
; KNL-LABEL: insert_dup_mem_v32i16_i32:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw (%rdi), %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: insert_dup_mem_v32i16_i32:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw (%rdi), %zmm0
; SKX-NEXT:    retq
  %tmp = load i32, ptr %ptr, align 4
  %tmp1 = insertelement <4 x i32> zeroinitializer, i32 %tmp, i32 0
  %tmp2 = bitcast <4 x i32> %tmp1 to <8 x i16>
  %tmp3 = shufflevector <8 x i16> %tmp2, <8 x i16> poison, <32 x i32> zeroinitializer
  ret <32 x i16> %tmp3
}

define <32 x i16> @insert_dup_mem_v32i16_sext_i16(ptr %ptr) {
; KNL-LABEL: insert_dup_mem_v32i16_sext_i16:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw (%rdi), %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: insert_dup_mem_v32i16_sext_i16:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw (%rdi), %zmm0
; SKX-NEXT:    retq
  %tmp = load i16, ptr %ptr, align 2
  %tmp1 = sext i16 %tmp to i32
  %tmp2 = insertelement <4 x i32> zeroinitializer, i32 %tmp1, i32 0
  %tmp3 = bitcast <4 x i32> %tmp2 to <8 x i16>
  %tmp4 = shufflevector <8 x i16> %tmp3, <8 x i16> poison, <32 x i32> zeroinitializer
  ret <32 x i16> %tmp4
}

define <32 x i16> @insert_dup_elt1_mem_v32i16_i32(ptr %ptr) #0 {
; KNL-LABEL: insert_dup_elt1_mem_v32i16_i32:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw 2(%rdi), %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: insert_dup_elt1_mem_v32i16_i32:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw 2(%rdi), %zmm0
; SKX-NEXT:    retq
  %tmp = load i32, ptr %ptr, align 4
  %tmp1 = insertelement <4 x i32> zeroinitializer, i32 %tmp, i32 0
  %tmp2 = bitcast <4 x i32> %tmp1 to <8 x i16>
  %tmp3 = shufflevector <8 x i16> %tmp2, <8 x i16> poison, <32 x i32> <i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1>
  ret <32 x i16> %tmp3
}

define <32 x i16> @insert_dup_elt3_mem_v32i16_i32(ptr %ptr) #0 {
; KNL-LABEL: insert_dup_elt3_mem_v32i16_i32:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw 2(%rdi), %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: insert_dup_elt3_mem_v32i16_i32:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw 2(%rdi), %zmm0
; SKX-NEXT:    retq
  %tmp = load i32, ptr %ptr, align 4
  %tmp1 = insertelement <4 x i32> zeroinitializer, i32 %tmp, i32 1
  %tmp2 = bitcast <4 x i32> %tmp1 to <8 x i16>
  %tmp3 = shufflevector <8 x i16> %tmp2, <8 x i16> poison, <32 x i32> <i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3>
  ret <32 x i16> %tmp3
}

define <32 x i16> @insert_dup_mem_v16i16_i64(ptr %ptr) {
; KNL-LABEL: insert_dup_mem_v16i16_i64:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw (%rdi), %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: insert_dup_mem_v16i16_i64:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw (%rdi), %zmm0
; SKX-NEXT:    retq
  %tmp = load i64, ptr %ptr, align 4
  %tmp1 = insertelement <2 x i64> zeroinitializer, i64 %tmp, i32 0
  %tmp2 = bitcast <2 x i64> %tmp1 to <8 x i16>
  %tmp3 = shufflevector <8 x i16> %tmp2, <8 x i16> poison, <32 x i32> zeroinitializer
  ret <32 x i16> %tmp3
}

define <32 x i16> @insert_dup_elt1_mem_v16i16_i64(ptr %ptr) {
; KNL-LABEL: insert_dup_elt1_mem_v16i16_i64:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw 2(%rdi), %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: insert_dup_elt1_mem_v16i16_i64:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw 2(%rdi), %zmm0
; SKX-NEXT:    retq
  %tmp = load i64, ptr %ptr, align 4
  %tmp1 = insertelement <2 x i64> zeroinitializer, i64 %tmp, i32 0
  %tmp2 = bitcast <2 x i64> %tmp1 to <8 x i16>
  %tmp3 = shufflevector <8 x i16> %tmp2, <8 x i16> poison, <32 x i32> <i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1, i32 1>
  ret <32 x i16> %tmp3
}

define <32 x i16> @insert_dup_elt3_mem_v16i16_i64(ptr %ptr) {
; KNL-LABEL: insert_dup_elt3_mem_v16i16_i64:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw 6(%rdi), %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: insert_dup_elt3_mem_v16i16_i64:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw 6(%rdi), %zmm0
; SKX-NEXT:    retq
  %tmp = load i64, ptr %ptr, align 4
  %tmp1 = insertelement <2 x i64> zeroinitializer, i64 %tmp, i32 0
  %tmp2 = bitcast <2 x i64> %tmp1 to <8 x i16>
  %tmp3 = shufflevector <8 x i16> %tmp2, <8 x i16> poison, <32 x i32> <i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3, i32 3>
  ret <32 x i16> %tmp3
}

define <32 x i16> @insert_dup_elt7_mem_v16i16_i64(ptr %ptr) {
; KNL-LABEL: insert_dup_elt7_mem_v16i16_i64:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw 6(%rdi), %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: insert_dup_elt7_mem_v16i16_i64:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw 6(%rdi), %zmm0
; SKX-NEXT:    retq
  %tmp = load i64, ptr %ptr, align 4
  %tmp1 = insertelement <2 x i64> zeroinitializer, i64 %tmp, i32 1
  %tmp2 = bitcast <2 x i64> %tmp1 to <8 x i16>
  %tmp3 = shufflevector <8 x i16> %tmp2, <8 x i16> poison, <32 x i32> <i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7, i32 7>
  ret <32 x i16> %tmp3
}

define <32 x i16> @insert_dup_mem_v16i16_sext_i16_i64(ptr %ptr) {
; KNL-LABEL: insert_dup_mem_v16i16_sext_i16_i64:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpbroadcastw (%rdi), %ymm0
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm0, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: insert_dup_mem_v16i16_sext_i16_i64:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpbroadcastw (%rdi), %zmm0
; SKX-NEXT:    retq
  %tmp = load i16, ptr %ptr, align 2
  %tmp1 = sext i16 %tmp to i64
  %tmp2 = insertelement <2 x i64> zeroinitializer, i64 %tmp1, i32 0
  %tmp3 = bitcast <2 x i64> %tmp2 to <8 x i16>
  %tmp4 = shufflevector <8 x i16> %tmp3, <8 x i16> poison, <32 x i32> zeroinitializer
  ret <32 x i16> %tmp4
}

define <32 x i16> @shuffle_v32i16_32_zz_zz_zz_33_zz_zz_zz_34_zz_zz_zz_35_zz_zz_zz_36_zz_zz_zz_37_zz_zz_zz_38_zz_zz_zz_39_zz_zz_zz(<32 x i16> %a) {
; ALL-LABEL: shuffle_v32i16_32_zz_zz_zz_33_zz_zz_zz_34_zz_zz_zz_35_zz_zz_zz_36_zz_zz_zz_37_zz_zz_zz_38_zz_zz_zz_39_zz_zz_zz:
; ALL:       ## %bb.0:
; ALL-NEXT:    vpmovzxwq {{.*#+}} zmm0 = xmm0[0],zero,zero,zero,xmm0[1],zero,zero,zero,xmm0[2],zero,zero,zero,xmm0[3],zero,zero,zero,xmm0[4],zero,zero,zero,xmm0[5],zero,zero,zero,xmm0[6],zero,zero,zero,xmm0[7],zero,zero,zero
; ALL-NEXT:    retq
  %shuffle = shufflevector <32 x i16> zeroinitializer, <32 x i16> %a, <32 x i32> <i32 32, i32 0, i32 0, i32 0, i32 33, i32 0, i32 0, i32 0, i32 34, i32 0, i32 0, i32 0, i32 35, i32 0, i32 0, i32 0, i32 36, i32 0, i32 0, i32 0, i32 37, i32 0, i32 0, i32 0, i32 38, i32 0, i32 0, i32 0, i32 39, i32 0, i32 0, i32 0>
  ret <32 x i16> %shuffle
}

define <32 x i16> @shuffle_v32i16_32_zz_33_zz_34_zz_35_zz_36_zz_37_zz_38_zz_39_zz_40_zz_41_zz_42_zz_43_zz_44_zz_45_zz_46_zz_47_zz(<32 x i16> %a) {
; ALL-LABEL: shuffle_v32i16_32_zz_33_zz_34_zz_35_zz_36_zz_37_zz_38_zz_39_zz_40_zz_41_zz_42_zz_43_zz_44_zz_45_zz_46_zz_47_zz:
; ALL:       ## %bb.0:
; ALL-NEXT:    vpmovzxwd {{.*#+}} zmm0 = ymm0[0],zero,ymm0[1],zero,ymm0[2],zero,ymm0[3],zero,ymm0[4],zero,ymm0[5],zero,ymm0[6],zero,ymm0[7],zero,ymm0[8],zero,ymm0[9],zero,ymm0[10],zero,ymm0[11],zero,ymm0[12],zero,ymm0[13],zero,ymm0[14],zero,ymm0[15],zero
; ALL-NEXT:    retq
  %shuffle = shufflevector <32 x i16> zeroinitializer, <32 x i16> %a, <32 x i32> <i32 32, i32 0, i32 33, i32 0, i32 34, i32 0, i32 35, i32 0, i32 36, i32 0, i32 37, i32 0, i32 38, i32 0, i32 39, i32 0, i32 40, i32 0, i32 41, i32 0, i32 42, i32 0, i32 43, i32 0, i32 44, i32 0, i32 45, i32 0, i32 46, i32 0, i32 47, i32 0>
  ret <32 x i16> %shuffle
}

define <8 x i16> @pr32967(<32 x i16> %v) {
; KNL-LABEL: pr32967:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpsrlq $16, %zmm0, %zmm0
; KNL-NEXT:    vpmovqw %zmm0, %xmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: pr32967:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpsrlq $16, %zmm0, %zmm0
; SKX-NEXT:    vpmovqw %zmm0, %xmm0
; SKX-NEXT:    vzeroupper
; SKX-NEXT:    retq
 %shuffle = shufflevector <32 x i16> %v, <32 x i16> poison, <8 x i32> <i32 1,i32 5,i32 9,i32 13,i32 17,i32 21,i32 25,i32 29>
 ret <8 x i16> %shuffle
}

define <32 x i16> @shuffle_v32i16_07_zz_05_zz_03_zz_01_zz_15_zz_13_zz_11_zz_09_zz_23_zz_21_zz_19_zz_17_zz_31_zz_29_zz_27_zz_25_zz(<32 x i16> %a) {
; KNL-LABEL: shuffle_v32i16_07_zz_05_zz_03_zz_01_zz_15_zz_13_zz_11_zz_09_zz_23_zz_21_zz_19_zz_17_zz_31_zz_29_zz_27_zz_25_zz:
; KNL:       ## %bb.0:
; KNL-NEXT:    vpshufb {{.*#+}} ymm1 = ymm0[14,15],zero,zero,ymm0[10,11],zero,zero,ymm0[6,7],zero,zero,ymm0[2,3],zero,zero,ymm0[30,31],zero,zero,ymm0[26,27],zero,zero,ymm0[22,23],zero,zero,ymm0[18,19],zero,zero
; KNL-NEXT:    vextracti64x4 $1, %zmm0, %ymm0
; KNL-NEXT:    vpshufb {{.*#+}} ymm0 = ymm0[14,15],zero,zero,ymm0[10,11],zero,zero,ymm0[6,7],zero,zero,ymm0[2,3],zero,zero,ymm0[30,31],zero,zero,ymm0[26,27],zero,zero,ymm0[22,23],zero,zero,ymm0[20,21],zero,zero
; KNL-NEXT:    vinserti64x4 $1, %ymm0, %zmm1, %zmm0
; KNL-NEXT:    retq
;
; SKX-LABEL: shuffle_v32i16_07_zz_05_zz_03_zz_01_zz_15_zz_13_zz_11_zz_09_zz_23_zz_21_zz_19_zz_17_zz_31_zz_29_zz_27_zz_25_zz:
; SKX:       ## %bb.0:
; SKX-NEXT:    vpshufb {{.*#+}} zmm0 = zmm0[14,15],zero,zero,zmm0[10,11],zero,zero,zmm0[6,7],zero,zero,zmm0[2,3],zero,zero,zmm0[30,31],zero,zero,zmm0[26,27],zero,zero,zmm0[22,23],zero,zero,zmm0[18,19],zero,zero,zmm0[46,47],zero,zero,zmm0[42,43],zero,zero,zmm0[38,39],zero,zero,zmm0[34,35],zero,zero,zmm0[62,63],zero,zero,zmm0[58,59],zero,zero,zmm0[54,55],zero,zero,zmm0[52,53],zero,zero
; SKX-NEXT:    retq
  %shuffle = shufflevector <32 x i16> zeroinitializer, <32 x i16> %a, <32 x i32> <i32 39, i32 0, i32 37, i32 0, i32 35, i32 0, i32 33, i32 0, i32 47, i32 0, i32 45, i32 0, i32 43, i32 0, i32 41, i32 0, i32 55, i32 0, i32 53, i32 0, i32 51, i32 0, i32 49, i32 0, i32 63, i32 0, i32 61, i32 0, i32 59, i32 0, i32 58, i32 0>
  ret <32 x i16> %shuffle
}
