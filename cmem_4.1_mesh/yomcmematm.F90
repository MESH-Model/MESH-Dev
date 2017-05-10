MODULE YOMCMEMATM

! Module containing the variables for the atmosphere and vegetation for a single cell

! Atmosphere variables :
!   tb_toa :: top-of-atmosphere brightness temperature (K)
!   tb_tov :: top-of-atmosphere brightness temperature (K)
!   tb_au :: upward atmospheric radiation
!   tb_ad :: downward atmospheric radiation
!   tau_atm :: optical thickness of atmosphere (zenith opacity / costheta)
! tair : 2m air temperature (K)
! spres : surface pressure (Pa)
! Z :  Average altitude above sea level for the pixel (km)
!   twv          scaling factor for water vapor absorption
!                  (1.0 = normal absorption)
!   to2          scaling factor for oxygen absorption
!                  (1.0 = normal absorption)
!  r_atm : density of air kg/m3
!  z_atm : observation layer height (m)
!  t_atm : observation layer temperature (K)
!  tc_atm : observation layer temperature (C)
!  p_atm : observation layer pressure (Pa)
!  rh_atm : observation layer relative humidity (%)
!  ah : observation layer absolute humidity (kg/m3)

! t_sky : cosmic background radiation (K)
! rw : gas constant for water vapor (J/(kg*K))
!---------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM,JPRM

IMPLICIT NONE

REAL(KIND = JPRM) :: tb_toa(2)
REAL(KIND = JPRM) :: tb_tov(2)
REAL(KIND = JPRM) :: tb_au
REAL(KIND = JPRM) :: tb_ad
REAL(KIND = JPRM) :: tau_atm
REAL(KIND = JPRM) :: tair
REAL(KIND = JPRM) :: spres
REAL(KIND = JPRM) :: Z

REAL(KIND = JPRM) :: twv
REAL(KIND = JPRM) :: to2

REAL(KIND = JPRM), allocatable, dimension (:) :: fZ 
REAL(KIND = JPRM), allocatable, dimension (:,:) :: fs_tatm 
REAL(KIND = JPRM), allocatable, dimension (:) :: fs_spres 
REAL(KIND = JPRM), allocatable, dimension (:,:) :: fs_R 
REAL(KIND = JPRM), allocatable, dimension (:) :: r_atm
REAL(KIND = JPRM), allocatable, dimension (:) :: z_atm
REAL(KIND = JPRM), allocatable, dimension (:) :: p_atm
REAL(KIND = JPRM), allocatable, dimension (:) :: t_atm
REAL(KIND = JPRM) :: tc_atm
REAL(KIND = JPRM), allocatable, dimension (:) :: rh_atm
REAL(KIND = JPRM), allocatable, dimension (:) :: ah

REAL(KIND = JPRM) :: t_sky
REAL(KIND = JPRM) :: rw
!---------------------------------------------------------------------------

END MODULE YOMCMEMATM
