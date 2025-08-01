! RUN: %python %S/test_folding.py %s %flang_fc1 -pedantic -triple x86_64-unknown-linux-gnu
! UNSUPPORTED: system-windows
! REQUIRES: target=x86_64{{.*}}
! REQUIRES: flang-supports-f128-math
! Tests folding of OUT_OF_RANGE().
module m
  integer(1),  parameter :: i1v(*)  = [ -huge(1_1)  - 1_1,  huge(1_1) ]
  integer(2),  parameter :: i2v(*)  = [ -huge(1_2)  - 1_2,  huge(1_2) ]
  integer(4),  parameter :: i4v(*)  = [ -huge(1_4)  - 1_4,  huge(1_4) ]
  integer(8),  parameter :: i8v(*)  = [ -huge(1_8)  - 1_8,  huge(1_8) ]
  integer(16), parameter :: i16v(*) = [ -huge(1_16) - 1_16, huge(1_16) ]
  !WARN: warning: division by zero [-Wfolding-exception]
  !WARN: warning: invalid argument on division [-Wfolding-exception]
  real(2),  parameter :: r2v(*)  = [ -huge(1._2),  huge(1._2),  1._2/0._2,   0._2/0._2 ]
  !WARN: warning: division by zero [-Wfolding-exception]
  !WARN: warning: invalid argument on division [-Wfolding-exception]
  real(3),  parameter :: r3v(*)  = [ -huge(1._3),  huge(1._3),  1._3/0._3,   0._3/0._3 ]
  !WARN: warning: division by zero [-Wfolding-exception]
  !WARN: warning: invalid argument on division [-Wfolding-exception]
  real(4),  parameter :: r4v(*)  = [ -huge(1._4),  huge(1._4),  1._4/0._4,   0._4/0._4 ]
  !WARN: warning: division by zero [-Wfolding-exception]
  !WARN: warning: invalid argument on division [-Wfolding-exception]
  real(8),  parameter :: r8v(*)  = [ -huge(1._8),  huge(1._8),  1._8/0._8,   0._8/0._8 ]
  !WARN: warning: division by zero [-Wfolding-exception]
  !WARN: warning: invalid argument on division [-Wfolding-exception]
  real(10), parameter :: r10v(*) = [ -huge(1._10), huge(1._10), 1._10/0._10, 0._10/0._10 ]
  !WARN: warning: division by zero [-Wfolding-exception]
  !WARN: warning: invalid argument on division [-Wfolding-exception]
  real(16), parameter :: r16v(*) = [ -huge(1._16), huge(1._16), 1._16/0._16, 0._16/0._16 ]
  logical, parameter :: finites(*) = [ .true., .true., .false., .false. ]

  logical, parameter :: test_i1i1   = .not. any(out_of_range(i1v,  1_1))
  logical, parameter :: test_i1i2   = .not. any(out_of_range(i1v,  1_2))
  logical, parameter :: test_i1i4   = .not. any(out_of_range(i1v,  1_4))
  logical, parameter :: test_i1i8   = .not. any(out_of_range(i1v,  1_8))
  logical, parameter :: test_i1i16  = .not. any(out_of_range(i1v,  1_16))
  logical, parameter :: test_i2i1   =       all(out_of_range(i2v,  1_1))
  logical, parameter :: test_i2i2   = .not. any(out_of_range(i2v,  1_2))
  logical, parameter :: test_i2i4   = .not. any(out_of_range(i2v,  1_4))
  logical, parameter :: test_i2i8   = .not. any(out_of_range(i2v,  1_8))
  logical, parameter :: test_i2i16  = .not. any(out_of_range(i2v,  1_16))
  logical, parameter :: test_i4i1   =       all(out_of_range(i4v,  1_1))
  logical, parameter :: test_i4i2   =       all(out_of_range(i4v,  1_2))
  logical, parameter :: test_i4i4   = .not. any(out_of_range(i4v,  1_4))
  logical, parameter :: test_i4i8   = .not. any(out_of_range(i4v,  1_8))
  logical, parameter :: test_i4i16  = .not. any(out_of_range(i4v,  1_16))
  logical, parameter :: test_i8i1   =       all(out_of_range(i8v,  1_1))
  logical, parameter :: test_i8i2   =       all(out_of_range(i8v,  1_2))
  logical, parameter :: test_i8i4   =       all(out_of_range(i8v,  1_4))
  logical, parameter :: test_i8i8   = .not. any(out_of_range(i8v,  1_8))
  logical, parameter :: test_i8i16  = .not. any(out_of_range(i8v,  1_16))
  logical, parameter :: test_i16i1  =       all(out_of_range(i16v, 1_1))
  logical, parameter :: test_i16i2  =       all(out_of_range(i16v, 1_2))
  logical, parameter :: test_i16i4  =       all(out_of_range(i16v, 1_4))
  logical, parameter :: test_i16i8  =       all(out_of_range(i16v, 1_8))
  logical, parameter :: test_i16i16 = .not. any(out_of_range(i16v, 1_16))

  logical, parameter :: test_i1r2   = .not. any(out_of_range(i1v,  1._2))
  logical, parameter :: test_i1r3   = .not. any(out_of_range(i1v,  1._3))
  logical, parameter :: test_i1r4   = .not. any(out_of_range(i1v,  1._4))
  logical, parameter :: test_i1r8   = .not. any(out_of_range(i1v,  1._8))
  logical, parameter :: test_i1r10  = .not. any(out_of_range(i1v,  1._10))
  logical, parameter :: test_i1r16  = .not. any(out_of_range(i1v,  1._16))
  logical, parameter :: test_i2r2   = .not. any(out_of_range(i2v,  1._2))
  logical, parameter :: test_i2r3   = .not. any(out_of_range(i2v,  1._3))
  logical, parameter :: test_i2r4   = .not. any(out_of_range(i2v,  1._4))
  logical, parameter :: test_i2r8   = .not. any(out_of_range(i2v,  1._8))
  logical, parameter :: test_i2r10  = .not. any(out_of_range(i2v,  1._10))
  logical, parameter :: test_i2r16  = .not. any(out_of_range(i2v,  1._16))
  logical, parameter :: test_i4r2   =       all(out_of_range(i4v,  1._2))
  logical, parameter :: test_i4r3   = .not. any(out_of_range(i4v,  1._3))
  logical, parameter :: test_i4r4   = .not. any(out_of_range(i4v,  1._4))
  logical, parameter :: test_i4r8   = .not. any(out_of_range(i4v,  1._8))
  logical, parameter :: test_i4r10  = .not. any(out_of_range(i4v,  1._10))
  logical, parameter :: test_i4r16  = .not. any(out_of_range(i4v,  1._16))
  logical, parameter :: test_i8r2   =       all(out_of_range(i8v,  1._2))
  logical, parameter :: test_i8r3   = .not. any(out_of_range(i8v,  1._3))
  logical, parameter :: test_i8r4   = .not. any(out_of_range(i8v,  1._4))
  logical, parameter :: test_i8r8   = .not. any(out_of_range(i8v,  1._8))
  logical, parameter :: test_i8r10  = .not. any(out_of_range(i8v,  1._10))
  logical, parameter :: test_i8r16  = .not. any(out_of_range(i8v,  1._16))
  logical, parameter :: test_i16r2  =       all(out_of_range(i16v, 1._2))
  logical, parameter :: test_i16r3  = .not. any(out_of_range(i16v, 1._3))
  logical, parameter :: test_i16r4  = .not. any(out_of_range(i16v, 1._4))
  logical, parameter :: test_i16r8  = .not. any(out_of_range(i16v, 1._8))
  logical, parameter :: test_i16r10 = .not. any(out_of_range(i16v, 1._10))
  logical, parameter :: test_i16r16 = .not. any(out_of_range(i16v, 1._16))

  logical, parameter :: test_r2r2   = .not. any(out_of_range(r2v,  1._2))
  logical, parameter :: test_r2r3   = .not. any(out_of_range(r2v,  1._3))
  logical, parameter :: test_r2r4   = .not. any(out_of_range(r2v,  1._4))
  logical, parameter :: test_r2r8   = .not. any(out_of_range(r2v,  1._8))
  logical, parameter :: test_r2r10  = .not. any(out_of_range(r2v,  1._10))
  logical, parameter :: test_r2r16  = .not. any(out_of_range(r2v,  1._16))
  logical, parameter :: test_r3r2   =       all(out_of_range(r3v,  1._2) .eqv. finites)
  !WARN: warning: invalid argument on REAL(2) to REAL(3) conversion [-Wfolding-exception]
  logical, parameter :: test_r3r2b  = .not. any(out_of_range(real(r2v, 3),  1._2))
  logical, parameter :: test_r3r3   = .not. any(out_of_range(r3v,  1._3))
  logical, parameter :: test_r3r4   = .not. any(out_of_range(r3v,  1._4))
  logical, parameter :: test_r3r8   = .not. any(out_of_range(r3v,  1._8))
  logical, parameter :: test_r3r10  = .not. any(out_of_range(r3v,  1._10))
  logical, parameter :: test_r3r16  = .not. any(out_of_range(r3v,  1._16))
  logical, parameter :: test_r4r2   =       all(out_of_range(r4v,  1._2) .eqv. finites)
  !WARN: warning: invalid argument on REAL(2) to REAL(4) conversion [-Wfolding-exception]
  logical, parameter :: test_r4r2b  = .not. any(out_of_range(real(r2v, 4),  1._2))
  logical, parameter :: test_r4r3   =       all(out_of_range(r4v,  1._3) .eqv. finites)
  !WARN: warning: invalid argument on REAL(3) to REAL(4) conversion [-Wfolding-exception]
  logical, parameter :: test_r4r3b  = .not. any(out_of_range(real(r3v, 4),  1._3))
  logical, parameter :: test_r4r4   = .not. any(out_of_range(r4v,  1._4))
  logical, parameter :: test_r4r8   = .not. any(out_of_range(r4v,  1._8))
  logical, parameter :: test_r4r10  = .not. any(out_of_range(r4v,  1._10))
  logical, parameter :: test_r4r16  = .not. any(out_of_range(r4v,  1._16))
  logical, parameter :: test_r8r2   =       all(out_of_range(r8v,  1._2) .eqv. finites)
  !WARN: warning: invalid argument on REAL(2) to REAL(8) conversion [-Wfolding-exception]
  logical, parameter :: test_r8r2b  = .not. any(out_of_range(real(r2v, 8),  1._2))
  logical, parameter :: test_r8r3   =       all(out_of_range(r8v,  1._3) .eqv. finites)
  !WARN: warning: invalid argument on REAL(3) to REAL(8) conversion [-Wfolding-exception]
  logical, parameter :: test_r8r3b  = .not. any(out_of_range(real(r3v, 8),  1._3))
  logical, parameter :: test_r8r4   =       all(out_of_range(r8v,  1._4) .eqv. finites)
  !WARN: warning: invalid argument on REAL(4) to REAL(8) conversion [-Wfolding-exception]
  logical, parameter :: test_r8r4b  = .not. any(out_of_range(real(r4v, 8),  1._4))
  logical, parameter :: test_r8r8   = .not. any(out_of_range(r8v,  1._8))
  logical, parameter :: test_r8r10  = .not. any(out_of_range(r8v,  1._10))
  logical, parameter :: test_r8r16  = .not. any(out_of_range(r8v,  1._16))
  logical, parameter :: test_r10r2  =       all(out_of_range(r10v, 1._2) .eqv. finites)
  !WARN: warning: invalid argument on REAL(2) to REAL(10) conversion [-Wfolding-exception]
  logical, parameter :: test_r10r2b = .not. any(out_of_range(real(r2v, 10),  1._2))
  logical, parameter :: test_r10r3  =       all(out_of_range(r10v, 1._3) .eqv. finites)
  !WARN: warning: invalid argument on REAL(3) to REAL(10) conversion [-Wfolding-exception]
  logical, parameter :: test_r10r3b = .not. any(out_of_range(real(r3v, 10),  1._3))
  logical, parameter :: test_r10r4  =       all(out_of_range(r10v, 1._4) .eqv. finites)
  !WARN: warning: invalid argument on REAL(4) to REAL(10) conversion [-Wfolding-exception]
  logical, parameter :: test_r10r4b = .not. any(out_of_range(real(r4v, 10),  1._4))
  logical, parameter :: test_r10r8  =       all(out_of_range(r10v, 1._8) .eqv. finites)
  !WARN: warning: invalid argument on REAL(8) to REAL(10) conversion [-Wfolding-exception]
  logical, parameter :: test_r10r8b = .not. any(out_of_range(real(r8v, 10),  1._8))
  logical, parameter :: test_r10r10 = .not. any(out_of_range(r10v, 1._10))
  logical, parameter :: test_r10r16 = .not. any(out_of_range(r10v, 1._16))
  logical, parameter :: test_r16r2  =       all(out_of_range(r16v, 1._2) .eqv. finites)
  !WARN: warning: invalid argument on REAL(2) to REAL(16) conversion [-Wfolding-exception]
  logical, parameter :: test_r16r2b = .not. any(out_of_range(real(r2v, 16), 1._2))
  logical, parameter :: test_r16r3  =       all(out_of_range(r16v, 1._3) .eqv. finites)
  !WARN: warning: invalid argument on REAL(3) to REAL(16) conversion [-Wfolding-exception]
  logical, parameter :: test_r16r3b = .not. any(out_of_range(real(r3v, 16), 1._3))
  logical, parameter :: test_r16r4  =       all(out_of_range(r16v, 1._4) .eqv. finites)
  !WARN: warning: invalid argument on REAL(4) to REAL(16) conversion [-Wfolding-exception]
  logical, parameter :: test_r16r4b = .not. any(out_of_range(real(r4v, 16), 1._4))
  logical, parameter :: test_r16r8  =       all(out_of_range(r16v, 1._8) .eqv. finites)
  !WARN: warning: invalid argument on REAL(8) to REAL(16) conversion [-Wfolding-exception]
  logical, parameter :: test_r16r8b = .not. any(out_of_range(real(r8v, 16), 1._8))
  logical, parameter :: test_r16r10 =       all(out_of_range(r16v, 1._10) .eqv. finites)
  !WARN: warning: invalid argument on REAL(10) to REAL(16) conversion [-Wfolding-exception]
  logical, parameter :: test_r16r10b= .not. any(out_of_range(real(r10v, 16), 1._10))
  logical, parameter :: test_r16r16 = .not. any(out_of_range(r16v, 1._16))

  logical, parameter :: test_r2i1   =       all(out_of_range(r2v,  1_1))
  logical, parameter :: test_r2i2   =       all(out_of_range(r2v,  1_2))
  logical, parameter :: test_r2i4   =       all(out_of_range(r2v,  1_4)  .neqv. finites)
  logical, parameter :: test_r2i8   =       all(out_of_range(r2v,  1_8)  .neqv. finites)
  logical, parameter :: test_r2i16  =       all(out_of_range(r2v,  1_16) .neqv. finites)
  logical, parameter :: test_r3i1   =       all(out_of_range(r3v,  1_1))
  logical, parameter :: test_r3i2   =       all(out_of_range(r3v,  1_2))
  logical, parameter :: test_r3i4   =       all(out_of_range(r3v,  1_4))
  logical, parameter :: test_r3i8   =       all(out_of_range(r3v,  1_8))
  logical, parameter :: test_r3i16  =       all(out_of_range(r3v,  1_16))
  logical, parameter :: test_r4i1   =       all(out_of_range(r4v,  1_1))
  logical, parameter :: test_r4i2   =       all(out_of_range(r4v,  1_2))
  logical, parameter :: test_r4i4   =       all(out_of_range(r4v,  1_4))
  logical, parameter :: test_r4i8   =       all(out_of_range(r4v,  1_8))
  logical, parameter :: test_r4i16  =       all(out_of_range(r4v,  1_16))
  logical, parameter :: test_r8i1   =       all(out_of_range(r8v,  1_1))
  logical, parameter :: test_r8i2   =       all(out_of_range(r8v,  1_2))
  logical, parameter :: test_r8i4   =       all(out_of_range(r8v,  1_4))
  logical, parameter :: test_r8i8   =       all(out_of_range(r8v,  1_8))
  logical, parameter :: test_r8i16  =       all(out_of_range(r8v,  1_16))
  logical, parameter :: test_r10i1  =       all(out_of_range(r10v, 1_1))
  logical, parameter :: test_r10i2  =       all(out_of_range(r10v, 1_2))
  logical, parameter :: test_r10i4  =       all(out_of_range(r10v, 1_4))
  logical, parameter :: test_r10i8  =       all(out_of_range(r10v, 1_8))
  logical, parameter :: test_r10i16 =       all(out_of_range(r10v, 1_16))
  logical, parameter :: test_r16i1  =       all(out_of_range(r16v, 1_1))
  logical, parameter :: test_r16i2  =       all(out_of_range(r16v, 1_2))
  logical, parameter :: test_r16i4  =       all(out_of_range(r16v, 1_4))
  logical, parameter :: test_r16i8  =       all(out_of_range(r16v, 1_8))
  logical, parameter :: test_r16i16 =       all(out_of_range(r16v, 1_16))

  logical, parameter :: test_r2i1r   =       all(out_of_range(r2v,  1_1,  .true.))
  logical, parameter :: test_r2i2r   =       all(out_of_range(r2v,  1_2,  .true.))
  logical, parameter :: test_r2i4r   =       all(out_of_range(r2v,  1_4,  .true.) .neqv. finites)
  logical, parameter :: test_r2i8r   =       all(out_of_range(r2v,  1_8,  .true.) .neqv. finites)
  logical, parameter :: test_r2i16r  =       all(out_of_range(r2v,  1_16, .true.) .neqv. finites)
  logical, parameter :: test_r3i1r   =       all(out_of_range(r3v,  1_1,  .true.))
  logical, parameter :: test_r3i2r   =       all(out_of_range(r3v,  1_2,  .true.))
  logical, parameter :: test_r3i4r   =       all(out_of_range(r3v,  1_4,  .true.))
  logical, parameter :: test_r3i8r   =       all(out_of_range(r3v,  1_8,  .true.))
  logical, parameter :: test_r3i16r  =       all(out_of_range(r3v,  1_16, .true.))
  logical, parameter :: test_r4i1r   =       all(out_of_range(r4v,  1_1,  .true.))
  logical, parameter :: test_r4i2r   =       all(out_of_range(r4v,  1_2,  .true.))
  logical, parameter :: test_r4i4r   =       all(out_of_range(r4v,  1_4,  .true.))
  logical, parameter :: test_r4i8r   =       all(out_of_range(r4v,  1_8,  .true.))
  logical, parameter :: test_r4i16r  =       all(out_of_range(r4v,  1_16, .true.))
  logical, parameter :: test_r8i1r   =       all(out_of_range(r8v,  1_1,  .true.))
  logical, parameter :: test_r8i2r   =       all(out_of_range(r8v,  1_2,  .true.))
  logical, parameter :: test_r8i4r   =       all(out_of_range(r8v,  1_4,  .true.))
  logical, parameter :: test_r8i8r   =       all(out_of_range(r8v,  1_8,  .true.))
  logical, parameter :: test_r8i16r  =       all(out_of_range(r8v,  1_16, .true.))
  logical, parameter :: test_r10i1r  =       all(out_of_range(r10v, 1_1,  .true.))
  logical, parameter :: test_r10i2r  =       all(out_of_range(r10v, 1_2,  .true.))
  logical, parameter :: test_r10i4r  =       all(out_of_range(r10v, 1_4,  .true.))
  logical, parameter :: test_r10i8r  =       all(out_of_range(r10v, 1_8,  .true.))
  logical, parameter :: test_r10i16r =       all(out_of_range(r10v, 1_16, .true.))
  logical, parameter :: test_r16i1r  =       all(out_of_range(r16v, 1_1,  .true.))
  logical, parameter :: test_r16i2r  =       all(out_of_range(r16v, 1_2,  .true.))
  logical, parameter :: test_r16i4r  =       all(out_of_range(r16v, 1_4,  .true.))
  logical, parameter :: test_r16i8r  =       all(out_of_range(r16v, 1_8,  .true.))
  logical, parameter :: test_r16i16r =       all(out_of_range(r16v, 1_16, .true.))

  logical, parameter :: test_r2i1u    = all(out_of_range(real(i1v,  kind=2)+.5_2, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r2i1ur   = all(out_of_range(real(i1v,  kind=2)+.5_2, 1_1,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r2i1d    = all(out_of_range(real(i1v,  kind=2)-.5_2, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r2i1dr   = all(out_of_range(real(i1v,  kind=2)-.5_2, 1_1,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r2i2u    = all(out_of_range(real(i2v,  kind=2)+.5_2, 1_2,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r2i2ur   = all(out_of_range(real(i2v,  kind=2)+.5_2, 1_2,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r2i2d    = all(out_of_range(real(i2v,  kind=2)-.5_2, 1_2,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r2i2dr   = all(out_of_range(real(i2v,  kind=2)-.5_2, 1_2,  .true.)  .eqv. [.false., .true.])
  !WARN: warning: overflow on INTEGER(4) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i4u    = all(out_of_range(real(i4v,  kind=2)+.5_2, 1_4,  .false.) .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(4) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i4ur   = all(out_of_range(real(i4v,  kind=2)+.5_2, 1_4,  .true.)  .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(4) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i4d    = all(out_of_range(real(i4v,  kind=2)-.5_2, 1_4,  .false.) .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(4) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i4dr   = all(out_of_range(real(i4v,  kind=2)-.5_2, 1_4,  .true.)  .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(8) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i8u    = all(out_of_range(real(i8v,  kind=2)+.5_2, 1_8,  .false.) .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(8) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i8ur   = all(out_of_range(real(i8v,  kind=2)+.5_2, 1_8,  .true.)  .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(8) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i8d    = all(out_of_range(real(i8v,  kind=2)-.5_2, 1_8,  .false.) .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(8) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i8dr   = all(out_of_range(real(i8v,  kind=2)-.5_2, 1_8,  .true.)  .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(16) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i16u   = all(out_of_range(real(i16v, kind=2)+.5_2, 1_16, .false.) .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(16) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i16ur  = all(out_of_range(real(i16v, kind=2)+.5_2, 1_16, .true.)  .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(16) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i16d   = all(out_of_range(real(i16v, kind=2)-.5_2, 1_16, .false.) .eqv. [.true.,  .true.])
  !WARN: warning: overflow on INTEGER(16) to REAL(2) conversion [-Wfolding-exception]
  logical, parameter :: test_r2i16dr  = all(out_of_range(real(i16v, kind=2)-.5_2, 1_16, .true.)  .eqv. [.true.,  .true.])

  logical, parameter :: test_r3i1u    = all(out_of_range(real(i1v,  kind=3)+.5_3, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r3i1ur   = all(out_of_range(real(i1v,  kind=3)+.5_3, 1_1,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r3i1d    = all(out_of_range(real(i1v,  kind=3)-.5_3, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r3i1dr   = all(out_of_range(real(i1v,  kind=3)-.5_3, 1_1,  .true.)  .eqv. [.false., .false.])
  logical, parameter :: test_r3i2u    = all(out_of_range(real(i2v,  kind=3)+.5_3, 1_2,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r3i2ur   = all(out_of_range(real(i2v,  kind=3)+.5_3, 1_2,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r3i2d    = all(out_of_range(real(i2v,  kind=3)-.5_3, 1_2,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r3i2dr   = all(out_of_range(real(i2v,  kind=3)-.5_3, 1_2,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r3i4u    = all(out_of_range(real(i4v,  kind=3)+.5_3, 1_4,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r3i4ur   = all(out_of_range(real(i4v,  kind=3)+.5_3, 1_4,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r3i4d    = all(out_of_range(real(i4v,  kind=3)-.5_3, 1_4,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r3i4dr   = all(out_of_range(real(i4v,  kind=3)-.5_3, 1_4,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r3i8u    = all(out_of_range(real(i8v,  kind=3)+.5_3, 1_8,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r3i8ur   = all(out_of_range(real(i8v,  kind=3)+.5_3, 1_8,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r3i8d    = all(out_of_range(real(i8v,  kind=3)-.5_3, 1_8,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r3i8dr   = all(out_of_range(real(i8v,  kind=3)-.5_3, 1_8,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r3i16u   = all(out_of_range(real(i16v, kind=3)+.5_3, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r3i16ur  = all(out_of_range(real(i16v, kind=3)+.5_3, 1_16, .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r3i16d   = all(out_of_range(real(i16v, kind=3)-.5_3, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r3i16dr  = all(out_of_range(real(i16v, kind=3)-.5_3, 1_16, .true.)  .eqv. [.false., .true.])

  logical, parameter :: test_r4i1u    = all(out_of_range(real(i1v,  kind=4)+.5_4, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r4i1ur   = all(out_of_range(real(i1v,  kind=4)+.5_4, 1_1,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r4i1d    = all(out_of_range(real(i1v,  kind=4)-.5_4, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r4i1dr   = all(out_of_range(real(i1v,  kind=4)-.5_4, 1_1,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r4i2u    = all(out_of_range(real(i2v,  kind=4)+.5_4, 1_2,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r4i2ur   = all(out_of_range(real(i2v,  kind=4)+.5_4, 1_2,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r4i2d    = all(out_of_range(real(i2v,  kind=4)-.5_4, 1_2,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r4i2dr   = all(out_of_range(real(i2v,  kind=4)-.5_4, 1_2,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r4i4u    = all(out_of_range(real(i4v,  kind=4)+.5_4, 1_4,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r4i4ur   = all(out_of_range(real(i4v,  kind=4)+.5_4, 1_4,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r4i4d    = all(out_of_range(real(i4v,  kind=4)-.5_4, 1_4,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r4i4dr   = all(out_of_range(real(i4v,  kind=4)-.5_4, 1_4,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r4i8u    = all(out_of_range(real(i8v,  kind=4)+.5_4, 1_8,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r4i8ur   = all(out_of_range(real(i8v,  kind=4)+.5_4, 1_8,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r4i8d    = all(out_of_range(real(i8v,  kind=4)-.5_4, 1_8,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r4i8dr   = all(out_of_range(real(i8v,  kind=4)-.5_4, 1_8,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r4i16u   = all(out_of_range(real(i16v, kind=4)+.5_4, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r4i16ur  = all(out_of_range(real(i16v, kind=4)+.5_4, 1_16, .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r4i16d   = all(out_of_range(real(i16v, kind=4)-.5_4, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r4i16dr  = all(out_of_range(real(i16v, kind=4)-.5_4, 1_16, .true.)  .eqv. [.false., .true.])

  logical, parameter :: test_r8i1u    = all(out_of_range(real(i1v,  kind=8)+.5_8, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r8i1ur   = all(out_of_range(real(i1v,  kind=8)+.5_8, 1_1,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r8i1d    = all(out_of_range(real(i1v,  kind=8)-.5_8, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r8i1dr   = all(out_of_range(real(i1v,  kind=8)-.5_8, 1_1,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r8i2u    = all(out_of_range(real(i2v,  kind=8)+.5_8, 1_2,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r8i2ur   = all(out_of_range(real(i2v,  kind=8)+.5_8, 1_2,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r8i2d    = all(out_of_range(real(i2v,  kind=8)-.5_8, 1_2,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r8i2dr   = all(out_of_range(real(i2v,  kind=8)-.5_8, 1_2,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r8i4u    = all(out_of_range(real(i4v,  kind=8)+.5_8, 1_4,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r8i4ur   = all(out_of_range(real(i4v,  kind=8)+.5_8, 1_4,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r8i4d    = all(out_of_range(real(i4v,  kind=8)-.5_8, 1_4,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r8i4dr   = all(out_of_range(real(i4v,  kind=8)-.5_8, 1_4,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r8i8u    = all(out_of_range(real(i8v,  kind=8)+.5_8, 1_8,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r8i8ur   = all(out_of_range(real(i8v,  kind=8)+.5_8, 1_8,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r8i8d    = all(out_of_range(real(i8v,  kind=8)-.5_8, 1_8,  .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r8i8dr   = all(out_of_range(real(i8v,  kind=8)-.5_8, 1_8,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r8i16u   = all(out_of_range(real(i16v, kind=8)+.5_8, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r8i16ur  = all(out_of_range(real(i16v, kind=8)+.5_8, 1_16, .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r8i16d   = all(out_of_range(real(i16v, kind=8)-.5_8, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r8i16dr  = all(out_of_range(real(i16v, kind=8)-.5_8, 1_16, .true.)  .eqv. [.false., .true.])

  logical, parameter :: test_r10i1u   = all(out_of_range(real(i1v,  kind=10)+.5_10, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r10i1ur  = all(out_of_range(real(i1v,  kind=10)+.5_10, 1_1,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r10i1d   = all(out_of_range(real(i1v,  kind=10)-.5_10, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r10i1dr  = all(out_of_range(real(i1v,  kind=10)-.5_10, 1_1,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r10i2u   = all(out_of_range(real(i2v,  kind=10)+.5_10, 1_2,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r10i2ur  = all(out_of_range(real(i2v,  kind=10)+.5_10, 1_2,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r10i2d   = all(out_of_range(real(i2v,  kind=10)-.5_10, 1_2,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r10i2dr  = all(out_of_range(real(i2v,  kind=10)-.5_10, 1_2,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r10i4u   = all(out_of_range(real(i4v,  kind=10)+.5_10, 1_4,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r10i4ur  = all(out_of_range(real(i4v,  kind=10)+.5_10, 1_4,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r10i4d   = all(out_of_range(real(i4v,  kind=10)-.5_10, 1_4,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r10i4dr  = all(out_of_range(real(i4v,  kind=10)-.5_10, 1_4,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r10i8u   = all(out_of_range(real(i8v,  kind=10)+.5_10, 1_8,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r10i8ur  = all(out_of_range(real(i8v,  kind=10)+.5_10, 1_8,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r10i8d   = all(out_of_range(real(i8v,  kind=10)-.5_10, 1_8,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r10i8dr  = all(out_of_range(real(i8v,  kind=10)-.5_10, 1_8,  .true.)  .eqv. [.false., .false.])
  logical, parameter :: test_r10i16u  = all(out_of_range(real(i16v, kind=10)+.5_10, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r10i16ur = all(out_of_range(real(i16v, kind=10)+.5_10, 1_16, .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r10i16d  = all(out_of_range(real(i16v, kind=10)-.5_10, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r10i16dr = all(out_of_range(real(i16v, kind=10)-.5_10, 1_16, .true.)  .eqv. [.false., .true.])

  logical, parameter :: test_r16i1u   = all(out_of_range(real(i1v,  kind=16)+.5_16, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r16i1ur  = all(out_of_range(real(i1v,  kind=16)+.5_16, 1_1,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r16i1d   = all(out_of_range(real(i1v,  kind=16)-.5_16, 1_1,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r16i1dr  = all(out_of_range(real(i1v,  kind=16)-.5_16, 1_1,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r16i2u   = all(out_of_range(real(i2v,  kind=16)+.5_16, 1_2,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r16i2ur  = all(out_of_range(real(i2v,  kind=16)+.5_16, 1_2,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r16i2d   = all(out_of_range(real(i2v,  kind=16)-.5_16, 1_2,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r16i2dr  = all(out_of_range(real(i2v,  kind=16)-.5_16, 1_2,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r16i4u   = all(out_of_range(real(i4v,  kind=16)+.5_16, 1_4,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r16i4ur  = all(out_of_range(real(i4v,  kind=16)+.5_16, 1_4,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r16i4d   = all(out_of_range(real(i4v,  kind=16)-.5_16, 1_4,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r16i4dr  = all(out_of_range(real(i4v,  kind=16)-.5_16, 1_4,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r16i8u   = all(out_of_range(real(i8v,  kind=16)+.5_16, 1_8,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r16i8ur  = all(out_of_range(real(i8v,  kind=16)+.5_16, 1_8,  .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r16i8d   = all(out_of_range(real(i8v,  kind=16)-.5_16, 1_8,  .false.) .eqv. [.false., .false.])
  logical, parameter :: test_r16i8dr  = all(out_of_range(real(i8v,  kind=16)-.5_16, 1_8,  .true.)  .eqv. [.true.,  .false.])
  logical, parameter :: test_r16i16u  = all(out_of_range(real(i16v, kind=16)+.5_16, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r16i16ur = all(out_of_range(real(i16v, kind=16)+.5_16, 1_16, .true.)  .eqv. [.false., .true.])
  logical, parameter :: test_r16i16d  = all(out_of_range(real(i16v, kind=16)-.5_16, 1_16, .false.) .eqv. [.false., .true.])
  logical, parameter :: test_r16i16dr = all(out_of_range(real(i16v, kind=16)-.5_16, 1_16, .true.)  .eqv. [.false., .true.])

 contains
  subroutine s(x, r)
    real(8), intent(in) :: x
    logical, intent(in), optional :: r
    !WARN: warning: ROUND= argument to OUT_OF_RANGE() is an optional dummy argument that must be present at execution [-Woptional-must-be-present]
    print *, out_of_range(x, 1, round=r)
  end
end
