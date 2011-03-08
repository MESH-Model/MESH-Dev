PROGRAM CLASS_Run_Completion
!>
!>	February 2, 2011 - B. Schweitzer
!>
!>=============================================================================
!>
!>	This program tests to see whether CLASS has run to completion
!>
!>=============================================================================
!>
!>	hr		- hour from CLASS output file
!>	hrt		- hour from true solution
!>	mn		- minute from CLASS output file
!>	mnt		- minute from true solution
!>	dt		- date from CLASS output file
!>	dtt		- date from true solution
!>	yr		- year from CLASS output file
!>	yrt		- year from true solution
!>
!>=============================================================================
	INTEGER :: hr,hrt,mn,mnt,dt,dtt,yr,yrt,inc
	CHARACTER :: temp1, temp2
	OPEN(UNIT=1,FILE='CLASS_benchmark.TS',STATUS='OLD')
	OPEN(UNIT=2,FILE='Test_Results\test.err',STATUS='REPLACE')
	OPEN(UNIT=14,FILE='CLASS_benchmark.of4',STATUS='OLD',ERR=1001)
	
	!>skips the 3 header lines at the beginning of the file
	DO inc = 1, 3
		READ(14,*) temp1
		READ(1,*) temp2
	END DO
	
	!>read the hours, minutes, days, and years into appropriate variables
	DO
		READ(14,*,END=104) hr,mn,dt,yr
		104 CONTINUE
		READ(1,*,END=113) hrt,mnt,dtt,yrt
	END DO
	
	113 CONTINUE
	!>Compare the variables to see if they are the same. If they are then the test passes 
	IF (hr /= hrt .OR. mn /= mnt .OR. dt /= dtt .OR. yr /= yrt) THEN
		WRITE(2,*) 'Model did not run to completion.'
		WRITE(2,*) 'test=Notok'
		STOP
	ELSE 
		WRITE(2,*) 'Model ran to completion.'
		WRITE(2,*) 'test=ok'
	ENDIF
	STOP
	
	1001 CONTINUE
	WRITE(2,*) 'No output files generated. Test failed'
	WRITE(2,*) 'test=Notok'
	
	CLOSE(UNIT=1)
	CLOSE(UNIT=2)
	CLOSE(UNIT=14)
	
END PROGRAM CLASS_Run_Completion