FUNCTION SAESRT(OBS,SIM,N,NMIN)
!>
!>       June 17, 2010 - M.A. Mekonnen
!>=======================================================================
!>
!>       The function computes the sum of absolute value of errors.
!>
!>=======================================================================
!>
!>       OBS        -   Observed values (vector) 
!>       SIM        -   Simulated values (vector) 
!>       N          -   vector dimension
!>
!>       SAESRT     -   Sum of absolute value of erros (after sorting)
!>       NMIN       -   Minimum of number of days for model spin-up
!>=======================================================================

    INTEGER N, NMIN
    REAL    OBS(N), SIM(N)

    REAL     SAESRT
    EXTERNAL SLASRT

    SAESRT = 0.0
    NN = N - NMIN
    IF(NN > 0)THEN
       CALL SLASRT('D',NN+1,OBS(NMIN:N),IERR)
       CALL SLASRT('D',NN+1,SIM(NMIN:N),IERR)
       SAESRT = SUM(ABS(OBS(NMIN:N) - SIM(NMIN:N)))
    ENDIF

END