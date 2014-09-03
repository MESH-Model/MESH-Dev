MODULE YOMCMEMGRIBAPI

! Module containing GRIB API file handlers

!---------------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM,JPRM

IMPLICIT NONE


REAL(KIND = JPRM), ALLOCATABLE   ::  fapifield(:,:)
REAL(KIND = JPRM)                ::  latitudeOfFirstgridPointInDegrees
REAL(KIND = JPRM)                ::  longitudeOfFirstgridPointInDegrees
REAL(KIND = JPRM)                ::  latitudeOfLastgridPointInDegrees
REAL(KIND = JPRM)                ::  longitudeOfLastgridPointInDegrees
INTEGER(KIND=JPIM)               ::  numberOfPointsAlongAParallel
INTEGER(KIND=JPIM)               ::  numberOfPointsAlongAMeridian
INTEGER(KIND=JPIM)               ::  numberOfLevels
INTEGER(KIND=JPIM)               ::  numberOfPoints,numberOfValues
CHARACTER(LEN=10)  CCVARAPI_NAME






END MODULE YOMCMEMGRIBAPI
