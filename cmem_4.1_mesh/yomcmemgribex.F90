MODULE YOMCMEMGRIBEX

! Module containing GRIBEX file handlers

! ipack_factor : 
! gribfields : all output fields of gribfile
!---------------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM,JPRM

IMPLICIT NONE

INTEGER(KIND=JPIM) :: iunit,ounit
INTEGER(KIND=JPIM) :: iret,ilen,iword,igrib_len
INTEGER(KIND=JPIM) :: isec0(2),isec1(1024),isec2(1024),isec3(2),isec4(512)
INTEGER(KIND=JPIM) :: ilenp
INTEGER(KIND=JPIM),allocatable :: igrib(:)

! stored file configuration for outputs
INTEGER(KIND=JPIM), SAVE :: isec0_out(2),isec1_out(1024),isec2_out(1024),isec3_out(2),isec4_out(512)
INTEGER(KIND=JPIM), SAVE :: igrib_out_size
INTEGER(KIND=JPIM),PARAMETER :: regular_grid_flag = 128  ! equal 128 when regular grid

REAL(KIND = JPRM) :: zsec2(512)
REAL(KIND = JPRM) :: zsec3(2)
REAL(KIND = JPRM),allocatable :: zsec4(:)
REAL(KIND = JPRM),allocatable :: gribfields(:,:)

END MODULE YOMCMEMGRIBEX
