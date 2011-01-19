PROGRAM compare
!>        
!>	January 7, 2011 - B. Schweitzer
!>
!>=============================================================================
!>
!> 	This program tests to see whether MESH has failed and how it failed
!>
!>=============================================================================
!>
!>	obs 	- array of observed values from model output
!>	obst	- array of observed values from true solution
!>	mod	- array of model values from model output
!>	modt	- array of model values from true solution
!>	date	- array of days the model runs for
!>	datet	- array of days from the true solution
!>	ET	- Error Threshold
!>    RT	- Run Time
!>
!>=============================================================================

	REAL, DIMENSION(:), ALLOCATABLE :: obs, obst, mod, modt
	INTEGER, DIMENSION(:), ALLOCATABLE :: date, datet
	REAL :: ET
	INTEGER :: i,j,k, count = 1, RT
	CHARACTER(LEN=50) :: input1

	OPEN(UNIT=9, FILE='MESH_input_run_options.ini', STATUS='OLD')
	OPEN(UNIT=10, FILE='BASINAVG1\MESH_output_streamflow.csv', STATUS='OLD')
	OPEN(UNIT=11, FILE='BASINAVG1\True_Value_MESH_output_streamflow.csv', STATUS='OLD')
	OPEN(UNIT=12, FILE='BASINAVG1\Test_results.txt', STATUS='REPLACE')
	OPEN(UNIT=13, FILE='MESH_test.ini', STATUS='OLD')

!> Finds the error threshold and run time from the MESH_test.ini file
	DO
		READ(13,*, END = 100) input1
		i = INDEX(input1, "ErrorThreshold")
		j = INDEX(input1, "RunTime")
		IF (i /= 0) THEN
			i = INDEX(input1, '=')
			k = LEN(input1)
			READ(input1(i+1:k),*) ET
		ELSEIF (j /=0) THEN
			j = INDEX(input1, '=')
			k = LEN(input1)
			READ(input1(j+1:k),*) RT
		ENDIF
		
	END DO
	
	100 CONTINUE

	ALLOCATE(date(RT),datet(RT),obs(RT),obst(RT),mod(RT),modt(RT))

!> reads the days, observed values, and the model's outputs from the model output file 	
	DO 
		READ(10,*,END = 123) date(count),obs(count),mod(count)
		count = count + 1	
	END DO

	123 CONTINUE
	count = 1
	
!> reads the days, observed values and true model ouputs from the true solution file	
	DO 
		READ(11,*,END = 456) datet(count),obst(count),modt(count)
		count = count + 1	
	END DO

 	456 CONTINUE
 
	DO count = 1,RT
		!> Checks to see if the days of the model output and the 
		!> true value files match otherwise the test stops
		IF (date(count) - datet(count) /= 0) THEN
			WRITE(12,*)'Dates do not match. Model has crashed. Test failed'
			STOP
		ENDIF
		!> Checks to see if the observed values of the model output
		!> and the true value files match otherwise the test stops
		IF (obs(count) - obst(count) /= 0) THEN
			WRITE(12,*)'Observed values do not match. Model has crashed. Test failed'
			STOP
		ENDIF
	END DO
	count=1
	DO
		!> Checks to see if the test has reached the end of the array and 
		!> if the difference of the true model output and the created model output are
		!> within the error threshold and if it is the test passes 
		IF ((count == RT .AND. (mod(count) - modt(count)) < ET) .OR. &
		    (count == RT .AND. (mod(count) - modt(count)) > -ET) ) THEN
			WRITE(12,*)'Test Passed'
			EXIT
		!> Checks to see if the difference between the true 
		!> model output and the created model output are outside the error
		!> threshold and if it is then the test fails													
		ELSEIF ((mod(count) - modt(count)) >= ET .OR. &	
			(mod(count) - modt(count)) <= -ET) THEN 
			WRITE(12,*)'Test Failed on day ', date(count)
			EXIT
		ELSE
			count = count + 1
		ENDIF
	END DO
	
	CLOSE(UNIT=9)
	CLOSE(UNIT=10)
	CLOSE(UNIT=11)
	CLOSE(UNIT=12)
	CLOSE(UNIT=13)

END PROGRAM compare