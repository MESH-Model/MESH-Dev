PROGRAM Grid_Run_Completion
!>        
!>	February 8, 2011 - B. Schweitzer
!>
!>=============================================================================
!>
!> 	This program tests to see whether MESH has run to completion
!>
!>=============================================================================
!>
!>	obs		- observed values from model output
!>	obst		- observed values from true solution
!>	dat		- days the model runs for
!>	datet	- days from the true solution
!>
!>=============================================================================
	CHARACTER (len=33):: input
	OPEN(UNIT=2,FILE='Test_Results\test.err',STATUS='REPLACE')
	OPEN(UNIT=1, FILE='BASINAVG1\MESH_output_echo_print.txt', STATUS='OLD',ERR=1001)
	
	DO
		READ(1,'(A)',END=100)input
		IF(input == 'Program has terminated normally.')THEN
			WRITE(2,*)'Model ran to completion'
			WRITE(2,*) 'test=ok'
			STOP
		END IF
	END DO
	100 CONTINUE
	
	WRITE(2,*)'Model did not run to completion'
	WRITE(2,*)'test=Notok'
	STOP
	
	1001 CONTINUE
	WRITE(2,*) 'No output files generated. Test failed'
	WRITE(2,*) 'test=Notok'
	
	CLOSE(UNIT=1)
	CLOSE(UNIT=2)
END PROGRAM Grid_Run_Completion