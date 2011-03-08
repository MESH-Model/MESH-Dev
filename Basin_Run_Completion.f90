PROGRAM Basin_Run_Completion
!>
!>	February 8, 2011 - B. Schweitzer
!>
!>=============================================================================
!>
!> 	This program tests to see whether the Basin has run to completion
!>
!>=============================================================================
	CHARACTER(LEN=33):: input
	
	OPEN(UNIT=11,FILE='Test_Results\test.err',STATUS='REPLACE')
	OPEN(UNIT=10,FILE='BASINAVG1\MESH_output_echo_print.txt',STATUS='OLD',ERR=1001)
	
	
	DO
		!> read the whole line into input
		READ(10,'(A)',END=100)input
		!> checks for termination line in file and immediately end the program if its found
		IF(input == 'Program has terminated normally.')THEN
			WRITE(11,*)'Model ran to completion'
			WRITE(11,*) 'test=ok'
			STOP
		END IF
	END DO
	
	100 CONTINUE
	
	WRITE(11,*)'Model did not run to completion'
	WRITE(11,*)'test=Notok'
	
	STOP
	1001 CONTINUE
	WRITE(11,*) 'No output files generated. Test failed'
	WRITE(11,*) 'test=Notok'
END PROGRAM Basin_Run_Completion