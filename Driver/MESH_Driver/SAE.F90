FUNCTION SAE(OBS,SIM,N,NMIN)
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
!>       SAE        -   Sum of absolute value of erros
!>       NMIN       -   Minimum of number of days for model spin-up
!>=======================================================================

    INTEGER N, NMIN
    REAL    OBS(N), SIM(N)

    REAL    SAE
    
    SAE = 0.0
    IF(N > NMIN)SAE = SUM(ABS(OBS(NMIN:N) - SIM(NMIN:N)))

END