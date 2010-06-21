FUNCTION SAE(OBS,SIM,N)
!>
!>       June 17, 2010 - M.A. Mekonnen/B. Davidson/M. MacDonald
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
!>=======================================================================

    INTEGER N
    REAL    OBS(N), SIM(N)

    REAL    SSE

    SSE = SUM(ABS(OBS - SIM))

END