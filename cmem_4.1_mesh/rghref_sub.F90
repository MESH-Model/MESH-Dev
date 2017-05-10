!---------------------------------------------------------------------------
! Different models to calculate Rough Surface Emissivity
!  1,2,4,5) Choudhury et al., 1979
!  3) Wegmueller and Maetzler, 1999
!---------------------------------------------------------------------------

SUBROUTINE RGHCHOU

! Purpose :
!   Calculate Rough Surface Emissivity

! Reference:
!    Wang and Choudhury, 1981: Remote sensing of soil moisture
!     content over bare field at 1.4 GHz frequency, J.Geo.Res.
!     Vol.86, 5277-5287    
!    Choudhury et al., 1979: Effect of surface roughness on the
!     microwave emission from soils, J.Geo.Res. Vol.84, 5699-5706
! and what reference for the parameterization of h and Q?
  
!---------------------------------------------------------------------------

USE YOMCMEMPAR, ONLY : costheta, LGPRINT, ip_Q
USE YOMCMEMSOIL, ONLY : r_r, r_s, Nrv,Nrh,hrmodel 
USE YOMCMEMFIELDS, ONLY : JJ, N_CMEM

IMPLICIT NONE
!---------------------------------------------------------------------------

r_r(1) = (ip_Q * r_s(2) + (1.-ip_Q) * r_s(1)) * exp(-hrmodel * costheta**Nrh)
r_r(2) = (ip_Q * r_s(1) + (1.-ip_Q) * r_s(2)) * exp(-hrmodel * costheta**Nrv)



END SUBROUTINE RGHCHOU

!===========================================================================

SUBROUTINE RGHWEGM

! Purpose :
! calculate Rough Surface Emissivity based on smooth surface reflectivity (H only)

!  Reference:
!    Wegmueller and Maetzler, 1999: Rough bare soil reflectivity model,
!    IEEE Trans. Geosci. Remote Sensing, Vol.37, No.3, 1391-1395.
!---------------------------------------------------------------------------

USE PARKIND1  ,ONLY : JPIM     ,JPRM

USE YOMCMEMPAR ,ONLY : theta, costheta, LGPRINT
USE YOMCMEMSOIL, ONLY : r_r , r_s 
USE YOMCMEMFIELDS, ONLY : JJ, N_CMEM, fh

IMPLICIT NONE
!---------------------------------------------------------------------------

r_r(1) = r_s(1) * exp(-1.0 * fh(JJ) ** (sqrt(0.10 * costheta)))

IF (fh(JJ) == 0.) THEN
  r_r(2) = r_s(2) 
ELSE
  IF (theta <= 60.0) THEN
    r_r(2) = r_r(1) * (costheta ** 0.655)
  ELSEIF (theta <= 70.0) THEN
    r_r(2) = r_r(1) * (0.635_JPRM - 0.0014_JPRM*(theta-60.0_JPRM))
  ELSE
   CALL ABOR1('Incidence angle not in [0;70] degrees. not suitable for rough soil reflectivity')
  ENDIF
ENDIF

END SUBROUTINE RGHWEGM
