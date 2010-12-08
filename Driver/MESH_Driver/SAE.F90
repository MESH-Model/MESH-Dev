FUNCTION SAE(OBS,SIM,N,NS,NMIN)
!>
!>       June 17, 2010 - M.A. Mekonnen
!>=======================================================================
!>
!>       The function computes the sum of absolute value of errors.
!>
!>=======================================================================
!>
!>       OBS        -   Observed values 
!>       SIM        -   Simulated values 
!>       N          -   Number of days
!>       NS         -   Number of stations
!>
!>       SAE        -   Sum of absolute value of erros
!>       NMIN       -   Minimum of number of days for model spin-up
!>=======================================================================

    INTEGER N,NS,NMIN
    REAL    OBS(N,NS), SIM(N,NS)

    REAL    SAE
    
    SAE = 0.0
    IF(N > NMIN)THEN
       DO J = 1, NS
          DO I = NMIN, N
             IF(OBS(I,J) .GE. 0.0)SAE = SAE + ABS(OBS(I,J) - SIM(I,J))
          ENDDO
       ENDDO
    ENDIF
END