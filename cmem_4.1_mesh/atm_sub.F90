! Atmospheric models
!  1 : L-band, Pellarin
!  2 : Layered atmosphere, Liebe, 1989
!  3 : Ulaby
!===========================================================================

SUBROUTINE ATMPELLARIN

! Purpose :
! -------
!   Calculate atmospheric opacity and up- and downwelling 
!   atmospheric radiation at L-band
!   For frequencies above 10 GHz, surface water vapor density should 
!      be accounted for
   
! Reference :
! ---------
!   Pellarin and Calvet, NOTE CNRM

! Author :
! ------
!   from lsmem

! Modifications :
! -------------
!   24-Sept-2006 Thomas Holmes, ECMWF
!   January 2008 Patricia de Rosnay, ECMWF
! End Modifications

! Internal variables :
!  GOSSAT : ATMOSPHERIC LOSS FACTOR: GOSSAT = EXP( -tau_atm / costheta)
!  TAEQ : equivalent layer temperature [K]
!---------------------------------------------------------------------------

USE PARKIND1, ONLY : JPRM
USE YOMCMEMPAR, ONLY : costheta, LGPRINT,LOFIELDEXP
USE YOMCMEMATM, ONLY : tair, Z,  t_sky, tau_atm, tb_ad, tb_au

IMPLICIT NONE

REAL(KIND=JPRM) :: GOSSAT, TAEQ
!---------------------------------------------------------------------------

! 1. Zenith atmospheric opacity

tau_atm = EXP( -3.926 - 0.2211 * Z - 0.00369 *tair)


! 2. Calculate up- and downward atmospheric radiation

GOSSAT = EXP(-tau_atm/ costheta)


TAEQ = EXP( 4.927 + 0.002195 * tair)

tb_ad = TAEQ*(1. - GOSSAT) + t_sky * GOSSAT


tb_au =  TAEQ*(1. - GOSSAT) 

SELECT CASE (LOFIELDEXP)

     CASE (.TRUE.)
      tb_au = 0.0
ENDSELECT

END SUBROUTINE ATMPELLARIN

!===========================================================================

SUBROUTINE ATMLIEBE

! Purpose :
! -------
!   Calculate atmospheric opacity and up- and downwelling atmospheric 
!   radiation 
   
! Author :
! ------
!   from lsmem

! Modifications :
! -------------
!   30-Aug-2006 Thomas Holmes   *ECMWF*
! End Modifications

! nlay : number of atmospheric layers
!---------------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRM
USE YOMCMEMPAR, ONLY : fghz , pi_cmem , costheta, nobs_atm , tfreeze, LGPRINT
USE YOMCMEMATM  ! z_atm, p_atm, t_atm, rh_atm, t_sky, rw, twv, t02

IMPLICIT NONE

INTEGER(KIND=JPIM) :: nlay, toplay, i, j
REAL(KIND=JPRM) :: c, e, awv, ao2
REAL, ALLOCATABLE, DIMENSION (:) :: cabsf
REAL, ALLOCATABLE, DIMENSION (:) :: dz_lay
REAL, ALLOCATABLE, DIMENSION (:) :: t_lay
REAL, ALLOCATABLE, DIMENSION (:) :: p_lay
REAL, ALLOCATABLE, DIMENSION (:) :: ah_lay
!---------------------------------------------------------------------------

nlay = nobs_atm - 1    

ALLOCATE (cabsf(nlay))
ALLOCATE (dz_lay(nlay))
ALLOCATE (t_lay(nlay))
ALLOCATE (p_lay(nlay))
ALLOCATE (ah_lay(nlay))

! 1 relative humidity (%) to absolute humidity (kg/m3)
!DO i = 1, nobs_atm
!  tc_atm = t_atm(i) - tfreeze
!  if (tc_atm > 0.) then
!    e = rh_atm(i) * 6.108 * exp( 17.1*tc_atm / (234.2+tc_atm) )
!  else
!    e = rh_atm(i) * 6.108 * exp( 17.8*tc_atm / (245.4+tc_atm) )
!  endif
!  ah(i) = 1000 * e / (rw * t_atm(i))
!ENDDO
! 1 specific humidity (kg/kg) to absolute humidity (kg/m3)
DO i = 1, nobs_atm
  ah(i) = r_atm(i) * rh_atm(i)
ENDDO

! Calculate layer averages

CALL avepro (z_atm, p_atm, t_atm, ah, dz_lay, p_lay, t_lay, ah_lay, nobs_atm)
! warning, avepro does not accept zero as input for pressure
!WRITE(6,*) 'z_atm ', z_atm , 'p ', p_lay

! Calculate layer absorption coefficients according to Liebe 89 for
!  - water vapor and oxygen

DO j = nlay, 1, -1
  CALL abwvl (ah_lay(j), t_lay(j), p_lay(j), fghz, awv)
  CALL abo2l (ah_lay(j), t_lay(j), p_lay(j), fghz, ao2)
  cabsf(j) = (twv * awv + to2 * ao2)
  IF (p_lay(j)<500) toplay = j
ENDDO
!WRITE(6,*) 'toplay', toplay , ' cabsf', cabsf

! Calculate zenith opacity of atmosphere

tau_atm = 0.
DO j = 1, toplay
  tau_atm = tau_atm + cabsf(j) * dz_lay(j)
ENDDO

! Calculate downward atmospheric radiation

tb_ad = t_sky
DO j = toplay,1, -1
  c = exp(-cabsf(j) * dz_lay(j) / costheta)
  tb_ad = c * tb_ad + (1.-c) * t_lay(j)
ENDDO

! Calculate upward atmospheric radiation

tb_au = 0.
DO j = 1,toplay
  c = exp(-cabsf(j) * dz_lay(j) / costheta)
  tb_au = c * tb_au + (1.-c) * t_lay(j)
ENDDO

! Clean

DEALLOCATE (cabsf)
DEALLOCATE (dz_lay)
DEALLOCATE (t_lay)
DEALLOCATE (p_lay)
DEALLOCATE (ah_lay)

END SUBROUTINE ATMLIEBE

!===========================================================================

SUBROUTINE ATMULABY

! Purpose :
! -------
!   Calculate atmospheric opacity and up- and downwelling atmospheric 
!   radiation for L-band
   
! Reference :
! ---------
! Ulaby, 1986 (UMF: 279-287)

! Author :
! ------
!   from lmeb

! Modifications :
! -------------
!   24-Sept-2006 Thomas Holmes   *ECMWF*
! End Modifications

! LOSSAT=ATMOSPHERIC LOSS FACTOR: LOSSAT(DB)=TAUAT(DB)*SEC(TETA0)
!---------------------------------------------------------------------------

USE PARKIND1, ONLY : JPRM
USE YOMCMEMPAR, ONLY : costheta, LGPRINT
USE YOMCMEMATM, ONLY : tair, t_sky, tau_atm, tb_ad, tb_au

IMPLICIT NONE

REAL(KIND=JPRM) :: LOSSAT
!---------------------------------------------------------------------------

! 1. Zenith atmospheric opacity

CALL ULABY_TAU_ATM

! 2. Calculate up- and downward atmospheric radiation

LOSSAT = EXP((tau_atm/costheta)/4.34)

tb_ad = tair*(1. -1./LOSSAT) + t_sky/LOSSAT

tb_au = tair*(1. -1./LOSSAT) 

END SUBROUTINE ATMULABY

!===========================================================================

SUBROUTINE ULABY_TAU_ATM

! Purpose :
! -------
!   Calculate zenith atmospheric opacity
   
! Reference :
! ---------
! Ulaby, 1986 (UMF: 279-287)

! Author :
! ------
!   from lmeb

! Modifications :
! -------------
!   24-Sept-2006 Thomas Holmes   *ECMWF*
! End Modifications

!  ROSWAT=SURFACE-WATER-VAPOR-DENSITY (ROSWAT=7.5G.M-3)
!---------------------------------------------------------------------------

USE PARKIND1, ONLY : JPRM
USE YOMCMEMPAR, ONLY : fghz, LGPRINT
USE YOMCMEMATM, ONLY : tau_atm

IMPLICIT NONE

REAL(KIND=JPRM), PARAMETER :: ROSWAT = 7.5
REAL(KIND=JPRM) :: X
!---------------------------------------------------------------------------

tau_atm = 0.

X=ABS((fghz-90.)/90.)
IF (X <= 0.1) THEN
    tau_atm=0.17+0.06*ROSWAT
ENDIF
X=ABS((fghz-35.)/35.)
IF (X <= 0.1) THEN
    tau_atm=0.17+0.013*ROSWAT
ENDIF
X=ABS((fghz-22.235)/22.235)
IF (X <= 0.1) THEN
    tau_atm=0.11+0.048*ROSWAT
ENDIF
X=ABS((fghz-15.)/15.)
IF (X <= 0.1) THEN
    tau_atm=0.055+0.004*ROSWAT
ENDIF
IF (fghz <= 11.) THEN
    tau_atm=0.04
ENDIF

IF (tau_atm == 0.) THEN
    CALL ABOR1('tau_atm == 0')
ENDIF

END SUBROUTINE ULABY_TAU_ATM

!===========================================================================

SUBROUTINE ec_p60l
        
! Purpose :
!     Computes the 60-level vertical pressure grid
!       associated to the input surface pressure

! p_atm  :: 61-level vertical pressure grid (Pa)
! p_lay  :: 60-level vertical pressure grid, layer average (Pa)
!---------------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRM
USE YOMCMEMPAR, ONLY : nobs_atm, LGPRINT
USE YOMCMEMATM, ONLY : p_atm, spres
        
IMPLICIT NONE

INTEGER(KIND=JPIM) :: i
INTEGER(KIND=JPIM), PARAMETER    :: nlev=60
REAL(KIND=JPRM)    :: aam(nlev+1), bbm(nlev+1)
!---------------------------------------------------------------------------
        
      data aam / &
     &     0.000000,    20.000000,    38.425343, &
     &    63.647804,    95.636963,   134.483307, &
     &   180.584351,   234.779053,   298.495789, &
     &   373.971924,   464.618134,   575.651001, &
     &   713.218079,   883.660522,  1094.834717, &
     &  1356.474609,  1680.640259,  2082.273926, &
     &  2579.888672,  3196.421631,  3960.291504, &
     &  4906.708496,  6018.019531,  7306.631348, &
     &  8765.053711, 10376.126953, 12077.446289, &
     & 13775.325195, 15379.805664, 16819.474609, &
     & 18045.183594, 19027.695313, 19755.109375, &
     & 20222.205078, 20429.863281, 20384.480469, &
     & 20097.402344, 19584.330078, 18864.750000, &
     & 17961.357422, 16899.468750, 15706.447266, &
     & 14411.124023, 13043.218750, 11632.758789, &
     & 10209.500977,  8802.356445,  7438.803223, &
     &  6144.314941,  4941.778320,  3850.913330, &
     &  2887.696533,  2063.779785,  1385.912598, &
     &   855.361755,   467.333588,   210.393890, &
     &    65.889244,     7.367743,     0.000000, &
     &     0.000000 &
     &         /
      data bbm / &
     & 0.0000000000, 0.0000000000, 0.0000000000, &
     & 0.0000000000, 0.0000000000, 0.0000000000, &
     & 0.0000000000, 0.0000000000, 0.0000000000, &
     & 0.0000000000, 0.0000000000, 0.0000000000, &
     & 0.0000000000, 0.0000000000, 0.0000000000, &
     & 0.0000000000, 0.0000000000, 0.0000000000, &
     & 0.0000000000, 0.0000000000, 0.0000000000, &
     & 0.0000000000, 0.0000000000, 0.0000000000, &
     & 0.0000758235, 0.0004613950, 0.0018151561, &
     & 0.0050811190, 0.0111429105, 0.0206778757, &
     & 0.0341211632, 0.0516904071, 0.0735338330, &
     & 0.0996746942, 0.1300225109, 0.1643843204, &
     & 0.2024759352, 0.2439331412, 0.2883229554, &
     & 0.3351548910, 0.3838921487, 0.4339629412, &
     & 0.4847715795, 0.5357099175, 0.5861684084, &
     & 0.6355474591, 0.6832686067, 0.7287858129, &
     & 0.7715966105, 0.8112534285, 0.8473749161, &
     & 0.8796569109, 0.9078838825, 0.9319403172, &
     & 0.9518215060, 0.9676452279, 0.9796627164, &
     & 0.9882701039, 0.9940194488, 0.9976301193, &
     & 1.0000000000 &
     &         /
        
DO i = 1, nobs_atm
  p_atm(nobs_atm-i+1)=aam(i)+bbm(i)*spres
ENDDO

!DO i = 1, nobs_atm-1
!  p_lay(i)=0.5*(paph(i)+paph(i+1))
!ENDDO
        
     ! return
END SUBROUTINE ec_p60l
