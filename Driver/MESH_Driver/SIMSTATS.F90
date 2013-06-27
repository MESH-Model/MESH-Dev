SUBROUTINE SIMSTATS(OBS,SIM,N,BIAS,NSD,NSW,TPD)
!>
!>       January 25, 2012 - M.A. Mekonnen
!>=======================================================================
!>
!>       The function computes model efficiency coefficients.
!>
!>=======================================================================
!>
!>       OBS        -   Observed values 
!>       SIM        -   Simulated values 
!>       N          -   Number of days
!>       NS         -   Number of stations
!>       NMIN       -   Minimum number of days for model spin-up
!>
!>       SAE        -   Absolute value error
!>       MAE        -   Mean absolute value error
!>       RMSE       -   Root mean squared error
!>       BIAS       -   
!>       NS         -   Nash-Sutcliffe coefficient
!>       NSLN       -   Nash-Sutcliffe coefficient 
!                       (with the natural logarithm of runoff)
!>=======================================================================

!INCOMING VARIABLES
!    IMPLICIT NONE
    INTEGER I, J, IW, N, NW
    REAL    OBS(N), SIM(N)
    REAL, ALLOCATABLE::  OBSW(:), SIMW(:), ERRW(:), ERRWM(:)

!OUTGOING VARIABLES
    REAL    BIAS, NSD, NSW, TPD, TPW

!LOCAL VARIABLES
    INTEGER NAD, NAW, IPO(1), IPS(1)
    REAL    OBSDM, OBSWM
    REAL    ERRD(N), ERRDM(N), ERRTP
    
!> Intrinsic Function
    INTRINSIC MAXLOC

!FOR WEEKLY CALCULATIONS
    NW     = CEILING(N/7.0)
    ALLOCATE(OBSW(NW), SIMW(NW), ERRW(NW), ERRWM(NW))

!INITIALIZE OUTPUT AND LOCAL VARIABLES
    BIAS   = 0.0
    NSD    = 0.0
    NSW    = 0.0
    TPD    = 0.0
    TPW    = 0.0
    OBSDM  = 0.0
    OBSW   = 0.0
    SIMW   = 0.0
    OBSWM  = 0.0
    ERRD   = 0.0
    ERRDM  = 0.0
    ERRW   = 0.0
    ERRWM  = 0.0
    ERRTP  = 0

!WEEKLY OBSERVED AND SIMULATED VALUES
    IW   = 0
    DO I = 1,N,7
       IW       = IW + 1
       J        = MIN(I+6,N)
       OBSW(IW) = SUM(OBS(I:J))
       SIMW(IW) = SUM(SIM(I:J))
    ENDDO


!MEAN OF OBSERVED RUNOFF
    NAD     = COUNT(OBS(1:N) .GE. 0.0)
    OBSDM   = SUM(OBS  (1:N), MASK = OBS(1:N) .GE. 0.0) / NAD

!MEAN OF WEEKLY RUNOFF
    NAW     = COUNT(OBSW(1:NW) .GE. 0.0)
    OBSWM   = SUM(OBSW  (1:NW), MASK = OBSW(1:NW) .GE. 0.0) / NAW

!CALCULATE ERRORS FOR RUNOFF GREATER THAN ZERO - DAILY
    WHERE(OBS(1:N).GE.0.0)
       ERRD  (1:N) = OBS(1:N) - SIM(1:N)
       ERRDM (1:N) = OBS(1:N) - OBSDM
    END WHERE

!CALCULATE ERRORS FOR RUNOFF GREATER THAN ZERO - WEEKLY
    WHERE(OBSW(1:NW).GE.0.0)
       ERRW  (1:NW) = OBSW(1:NW) - SIMW(1:NW)
       ERRWM (1:NW) = OBSW(1:NW) - OBSWM
    END WHERE

!CALCULATE THE STATISTICAL COEFFICIENTS
    BIAS = SUM(ERRD(1:N)) / (OBSDM * NAD)
    NSD  = 1.0 - SUM(ERRD*ERRD) / SUM(ERRDM*ERRDM)
    NSW  = 1.0 - SUM(ERRW*ERRW) / SUM(ERRWM*ERRWM)

!TIME TO PEAK - DAILY BASIS
    ERRTP = 0
    DO I = 1,N,365
       J = MIN(I+364,N)
       IF(OBS(MAX(I, J-182)) > 0.0)THEN
          IPO = MAXLOC(OBS(I:J))
          IPS = MAXLOC(SIM(I:J))
          ERRTP  = IPO(1) - IPS(1)
       ELSE
          ERRTP = 0
       ENDIF
          TPD = TPD + ABS(ERRTP)
    ENDDO

!TIME TO PEAK - WEAKLY BASIS
!ERRTP = 0
!DO I = 1,NW, 52
!   J = MIN(I+51,NW)
!   IF(OBSW(MAX(I, J-25)) > 0.0)THEN
!      IPO = MAXLOC(OBSW(I:J))
!      IPS = MAXLOC(SIMW(I:J))
!      ERRTP  = IPO(1) - IPS(1)
!   ELSE
!      ERRTP = 0
!   ENDIF
!   TPW = TPW + ABS(ERRTP)
!ENDDO

END
