FUNCTION SAESRT(OBS,SIM,N,NS,NMIN)
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
!>       SAESRT     -   Sum of absolute value of erros (after sorting)
!>       NMIN       -   Minimum of number of days for model spin-up
!>=======================================================================

    INTEGER N,NS,NMIN
    REAL    OBS(N,NS), SIM(N,NS)

    REAL     SAESRT,QO(N-NMIN+1),QS(N-NMIN+1)

    SAESRT = 0.0
    NN = N - NMIN + 1
    IF(NN > 1)THEN
       DO J = 1, NS
          QO = 0.0
          QS = 0.0
          DO I = NMIN, N
             IF(OBS(I,J) .GE. 0.0)THEN     !> Exclude missing streamflow data
                II     = I - NMIN + 1
                QO(II) = OBS(I,J)
                QS(II) = SIM(I,J)
             ENDIF
          ENDDO

!> Sort observed streamflow          
          CALL SORT(QO,NN)

!> Sort simulated streamflow          
          CALL SORT(QS,NN)

!> Compute the sum of errors after ranking          
          SAESRT = SAESRT + SUM(ABS(QO - QS))
       ENDDO
    ENDIF
RETURN
END