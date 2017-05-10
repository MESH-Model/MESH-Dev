MODULE YOMCMEMNETCDF

! Module containing NC file handlers

!---------------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM,JPRM

IMPLICIT NONE

INTEGER(KIND=JPIM) ::  NDIMS,NTIMES, NLATS, NLONS, NLVLS 
INTEGER(KIND=JPIM) ::  NDIMS_SM,NTIMES_SM, NLATS_SM, NLONS_SM, NLVLS_SM 


REAL(KIND = JPRM), allocatable :: fncfield(:,:)
CHARACTER (len = *), parameter :: CCLVL_NAME = "LEV"
CHARACTER (len = *), parameter :: CCLAT_NAME = "LATITUDE"
CHARACTER (len = *), parameter :: CCLON_NAME = "LONGITUDE"
CHARACTER (len = *), parameter :: CCTIME_NAME = "TIME"

CHARACTER (len = *), parameter :: CCLAT_UNITS = "Degrees North"
CHARACTER (len = *), parameter :: CCLON_UNITS = "Degrees East"
CHARACTER (len = *), parameter :: CCTIME_UNITS = "DoY"
CHARACTER (len = *), parameter :: CCTEMP_UNITS = "K"
CHARACTER (len = *), parameter :: CCVWC_UNITS = "Kg/m2"
CHARACTER (len = *), parameter :: CCUNITS = "units"
CHARACTER (len = *), parameter :: CCPFILED_UNITS = "-"


REAL(KIND = JPRM), ALLOCATABLE :: xlats(:), xlons(:),xlvls(:),xtimes(:)
REAL(KIND = JPRM), ALLOCATABLE :: VAR_IN(:,:,:,:)
CHARACTER(LEN=10)  CCVARNC_NAME






END MODULE YOMCMEMNETCDF
