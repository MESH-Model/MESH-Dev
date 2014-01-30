FUNCTION NSE(OBS,SIM,N,NS,NMIN)
!>
!>        December 18, 2013 - K. C. Kornelsen
!>==============================================================================================
!>
!>		The function computes the Nash-Sutcliffe efficiency index
!>
!>==============================================================================================
!>
!>		OBS		- Observed values
!>		SIM		- Simulated values
!>		N		- Number of days
!>		NS		- Number of stations
!>		NMIN	- Minimum number of days for model spin up
!>		NSE		- Nash-Sutcliffe Efficiency Index 
!>
!>===============================================================================================

! FUNCTION IN/OUT
INTEGER N, NS, NMIN
REAL	OBS(N,NS), SIM(N,NS)
REAL	NSE

! OTHER ALLOCATIONS
INTEGER I, J ! Counters
REAL	QBAR(NS), NUM(NS), DENOM(NS)
REAL	WGT





! Mean flow
QBAR = 0.0

IF(N > NMIN) THEN
	! Get Sum
	DO J = 1,NS
		DO I= NMIN,N
			QBAR(J) = QBAR(J) + OBS(I,J)
		ENDDO
	ENDDO

	! Get Average
	DO J = 1, NS
		QBAR(J) = QBAR(J)/(N-NMIN)
	ENDDO

	

NUM = 0.0
DENOM = 0.0

	DO J = 1, NS
		DO I = NMIN,N
			NUM(J) = NUM(J) + (OBS(I,J)-SIM(I,J))**2.0
			DENOM(J) = DENOM(J) + (OBS(I,J) - QBAR(J))**2.0
		ENDDO
	ENDDO


NSE = 0.0

WGT  = 1.0/REAL(NS) ! Weight based on NS

	DO J = 1,NS
		NSE = NSE + (WGT*(1.0 - (NUM(J)/DENOM(J))))
	ENDDO





ENDIF

END FUNCTION

