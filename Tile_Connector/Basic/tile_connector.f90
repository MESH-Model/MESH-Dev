SUBROUTINE tile_connector(runoff, recharge, leakages, ncount, rofogrd, rofsgrd, rofbgrd, DELT)
USE area_watflood
IMPLICIT NONE

!> ----------------------------------------------------------------------------
!> Parameters
!> ----------------------------------------------------------------------------
REAL*4, DIMENSION(ycount,xcount) :: runoff, recharge, leakages
REAL*4, DIMENSION(NA) :: rofogrd, rofsgrd, rofbgrd
REAL*4 :: DELT
INTEGER :: ncount


!> ----------------------------------------------------------------------------
!> Declarations
!> ----------------------------------------------------------------------------
INTEGER :: i


!> ----------------------------------------------------------------------------
!> mesh_driver code
!> ----------------------------------------------------------------------------
!> Calculate averages for Watroute
!> CDAN * Values are only reset on the hour and are cumulative on the
!> CDAN * half-hour: stand-alone RTE.exe (Watroute) reads hourly data.
!> CDAN * Output values are multiplies by delt to convert them from
!> CDAN * [kg m-2 s-1] TO [mm] (Mar 20/08)
DO i = 1, NA
  IF (MOD (ncount, 2) /= 0) THEN !Hourly time step
      runoff(YYY(i), XXX(i)) = (ROFOGRD(i) + ROFSGRD(i))*DELT
      recharge(YYY(i), XXX(i)) = ROFBGRD(i)*DELT
!+      leakages(YYY(i), XXX(i)) = 0.0*DELT !todo: determine what this should be
  ELSE !Cumulative half-hourly time step
      runoff(YYY(i), XXX(i)) = runoff(YYY(i), XXX(i)) + (ROFOGRD(i) + ROFSGRD(i))*DELT
      recharge(YYY(i), XXX(i)) = recharge(YYY(i), XXX(i)) + ROFBGRD(i)*DELT
!+      leakages(YYY(i), XXX(i) = leakages(YYY(i), XXX(i)) + 0.0*DELT !todo: determine what this should be
  END IF
END DO


END SUBROUTINE
