FUNCTION SAEMSRT(OBS,SIM,N,NS,NMIN,IW1,IW2)
!>
!>       DECEMBER 16, 2010 - M.A. Mekonnen & B. Davison
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

    REAL     SAEMSRT,QO(IW1,NS),QS(IW1,NS)
    
    !> Initialize objective function value
    SAEMSRT = 0.0
    
	!> Number of calibraton data - excluding model spin-up days.
	NN = N - NMIN + 1
    
	!> Sum of window size and window spacing
	IW1PW2 = IW1 + IW2
	
	IF(NN > IW1PW2)THEN

		!> Compute number of windows 
		NW = (N + IW2 - 1) / (IW1PW2)  !> !N = (W1+1)*NW + (W2-1)*(NW-1) = NW*(W1+W2)-W2+1
	    
		!Sum up the objective function values of each window 
        DO I = 1, NW
           ISTRT = NMIN + (I-1)*IW1PW2
           IEND  = ISTRT + IW1 - 1	
           QO    =  OBS(ISTRT:IEND,:)
           QS    = SIM(ISTRT:IEND,:)
           SAEMSRT = SAEMSRT + SAESRT(QO,QS,IW1,NS,1)
	    ENDDO
	ELSE
	    PRINT*, "CHECK THE SUM OF WINDOW SIZE AND WINDOW SPACING WITH ", &
		        "THE NUMBER OF CALIBRATION DAYS MINUS MODEL SPIN-UP DAYS"
		STOP
    ENDIF
	
RETURN
END