
MODULE MESH_CMEM_MODULE

USE PARKIND1 , ONLY : JPIM, JPRM
USE YOMCMEMPAR, ONLY : JPCMEMTILE
IMPLICIT NONE
!***********************************************************************
!     * CMEM VARIABLES
INTEGER(KIND=JPIM) :: JJPOL
REAL(KIND = JPRM) :: RSN(2), ESN(2)
REAL(KIND = JPRM) :: tfrac(JPCMEMTILE)
INTEGER(KIND=JPIM),ALLOCATABLE :: WATERROW(:,:)
INTEGER(KIND=JPIM),ALLOCATABLE :: ECHGRUROW(:,:) 
INTEGER(KIND=JPIM),ALLOCATABLE :: ECLGRUROW(:,:)
REAL(KIND=JPRM),ALLOCATABLE :: ELEVROW(:,:)
INTEGER(KIND=JPIM),ALLOCATABLE :: WATERGAT(:)
INTEGER(KIND=JPIM),ALLOCATABLE :: ECHGRUGAT(:)
INTEGER(KIND=JPIM),ALLOCATABLE :: ECLGRUGAT(:)
REAL(KIND=JPRM),ALLOCATABLE:: ELEVGAT(:)
REAL, DIMENSION(:,:), ALLOCATABLE :: AIL ! CLASSA VARIABLE REQUIRED FOR CMEM
REAL, DIMENSION(:,:), ALLOCATABLE :: TBH, TBV, TAUH, TAUV
INTEGER:: CMEMCOUNT, CMEMFLAG, CMEMR2COUTPUTFLAG
REAL, ALLOCATABLE, DIMENSION(:) :: CMEMTHETA
!* GRD, GAT and GRDGAT variable for writing R2C output file
INTEGER, ALLOCATABLE, DIMENSION(:) :: GRD_CMEM, GAT_CMEM, GRDGAT_CMEM
CHARACTER*50, ALLOCATABLE, DIMENSION(:,:) :: R2C_ATTRIBUTES_CMEM
INTEGER :: NR2C_CMEM, DELTR2C_CMEM, NR2CFILES_CMEM
INTEGER, PARAMETER :: R2CFILEUNITSTART_CMEM = 2000
INTEGER :: FRAME_NO_CMEM, NR2CSTATES_CMEM

!* ECHGRU is the ECOCLIMAP type for high vegetation for each gru
!* ECLGRU is the ECOCLIMAP type for low vegetation for each gru
!* TBH, TBV are brightness temperature in horizontal and vertical polarizations
!* TAUH, TAUV are vegetation optical depth of horizontal and vertical polarizations
!* TBH/V, TAUH/V are required only for printing. Since MESH GAT variables are of
!* size (ILG,CMEMFLAG) but only have NML useful values CMEM variables are of length NML to
!* avoid numerical complications. The CMEM variables are then read to the TB, TAU
!* arrays for printing/ integration to the standard MESH format

! FOR A DESCRIPTION OF THE CMEM VARIABLES SEE THE YOMCMEM MODULES
!=======================================================================

END MODULE MESH_CMEM_MODULE