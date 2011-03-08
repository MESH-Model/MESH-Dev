PROGRAM CLASS_Same_Results
!>        
!>	February 3, 2011 - B. Schweitzer
!>
!>=============================================================================
!>
!>	This program tests to see whether CLASS has produced Same results
!>	for the output files CLASS_benchmark.of# based on absolute difference
!>
!>=============================================================================
!>
!>	c#	- the #th column of the file
!>	l#	- the lowest value of column #
!>	h#	- the highest value of column #
!>	e#	- the error threshold for column # found by subtracting h# by l# 
!>	t#	- the #th colum of the true solution
!>	ok	- determines whether the test passes or fails
!>
!>=============================================================================
	INTEGER :: row , fail = 0,col
	CHARACTER :: temp1,temp2
	REAL, DIMENSION(:), ALLOCATABLE :: c,h,l,e,t
	LOGICAL :: ok = .true.
	
	OPEN(UNIT=32, FILE='Test_Results\test.err', STATUS='REPLACE')
	OPEN(UNIT=10, FILE='CLASS_benchmark.of1', STATUS='OLD',ERR=1001)
	OPEN(UNIT=20, FILE='Diana_benchmark_results\CLASS_benchmark.of1', STATUS='OLD')
	OPEN(UNIT=31, FILE='Test_Results\CLASS_Same_failures.txt', STATUS='REPLACE')
	
	
	ALLOCATE(c(15),h(13),l(13),e(13),t(15))
	
	!>skip the header lines
	DO row = 1, 3
		READ(20,*) temp1
	END DO
	row = 1
	
	DO
		!> read in the values from each column
		READ(20,*,END=100) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), &
						c(10),c(11),c(12),c(13),c(14),c(15)
		!> if this is the first read then intialize highest and lowest value as first number in column
		IF (row == 1) THEN
			DO col=3,15
				h(col-2)= c(col)
				l(col-2)=c(col)
			END DO
		!> else pass the column value and previous highest/lowest value to the function findmax/min
		ELSE
			DO col=3,15
				h(col-2)=findmax(c(col),h(col-2))
				l(col-2)=findmin(c(col),l(col-2))
			END DO
		ENDIF
		row = row + 1
	END DO
	100 CONTINUE
	!> initialize the error threshold using the the Same difference of the highest and lowest values multiplied by 0.0001%
	DO col=1,13
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(20)
	
	!> skip the headers
	DO row = 1, 3
		READ(20,*) temp1
		READ(10,*) temp2
	END DO
	DO
		!> read the values of the columns from the CLASS output file and the true solution
		READ(10,*,END=101) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15)
		101 CONTINUE
		READ(20,*,END=102) t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15)
		!> checks to see if the difference between the model output and the true solution exceeds the error threshold
		!> if it does the failure counter goes up by one
		DO col = 3,15
			IF ((c(col)-t(col)) > e(col-2) .OR. (c(col)-t(col)) < -e(col-2)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASS_benchmark.of1"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	102 CONTINUE
	!> check to see if the fail counter is 0. If it is then this portion passes
	!> else ok is set to false
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASS_benchmark.of1"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of1".' , &
			' Check CLASS_Same_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (10)
	CLOSE (20)
	DEALLOCATE(c,h,l,e,t)
	!> reset counter
	fail = 0
	
	OPEN(UNIT=11, FILE='CLASS_benchmark.of2', STATUS='OLD',ERR=1001)
	OPEN(UNIT=21, FILE='Diana_benchmark_results\CLASS_benchmark.of2', STATUS='OLD')
	ALLOCATE(c(16),h(14),l(14),e(14),t(16))
	
	DO row = 1, 3
		READ(21,*) temp1
	END DO
	row = 1
	DO
		READ(21,*,END=110) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16)
		IF (row == 1) THEN
			DO col=3,16
				h(col-2)= c(col)
				l(col-2)=c(col)
			END DO
		ELSE
			DO col=3,16
				h(col-2)=findmax(c(col),h(col-2))
				l(col-2)=findmin(c(col),l(col-2))
			END DO
		END IF
		row = row + 1
	END DO
	110 CONTINUE
	DO col=1,14
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(21)
	
	DO row = 1, 3
		READ(21,*) temp1
		READ(11,*) temp2
	END DO
	DO
		READ(11,*,END=111)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16)
		111 CONTINUE
		READ(21,*,END=112)t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16)
		DO col = 3,16
			IF ((c(col)-t(col)) > e(col-2) .OR. (c(col)-t(col)) < -e(col-2)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASS_benchmark.of2"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	112 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASS_benchmark.of2"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of2".' , &
			' Check CLASS_Same_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (11)
	CLOSE (21)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=12, FILE='CLASS_benchmark.of3', STATUS='OLD',ERR=1001)
	OPEN(UNIT=22, FILE='Diana_benchmark_results\CLASS_benchmark.of3', STATUS='OLD')
	ALLOCATE(c(10),h(8),l(8),e(8),t(10))
	
	DO row = 1, 3
		READ(22,*) temp1
	END DO
	row = 1
	DO
		READ(22,*,END=120)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		IF (row == 1) THEN
			DO col=3,10
				h(col-2)= c(col)
				l(col-2)=c(col)
			END DO
		ELSE
			DO col=3,10
				h(col-2)=findmax(c(col),h(col-2))
				l(col-2)=findmin(c(col),l(col-2))
			END DO
		END IF
		row = row + 1
	END DO
	120 CONTINUE
	DO col=1,8
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(22)
	
	DO row = 1, 3
		READ(22,*) temp1
		READ(12,*) temp2
	END DO
	DO
		READ(12,*,END=121) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		121 CONTINUE
		READ(22,*,END=122) t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9),t(10)
		DO col = 3,10
			IF ((c(col)-t(col)) > e(col-2) .OR. (c(col)-t(col)) < -e(col-2)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASS_benchmark.of3"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	122 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASS_benchmark.of3"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of3".' , &
			' Check CLASS_Same_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (12)
	CLOSE (22)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=13, FILE='CLASS_benchmark.of4', STATUS='OLD',ERR=1001)
	OPEN(UNIT=23, FILE='Diana_benchmark_results\CLASS_benchmark.of4', STATUS='OLD')
	ALLOCATE(c(18),h(14),l(14),e(14),t(18))
	
	DO row = 1, 3
		READ(23,*) temp1
	END DO
	row = 1
	DO
		READ(23,*,END=130)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18)
		IF (row == 1) THEN
			DO col=5,18
				h(col-4)= c(col)
				l(col-4)=c(col)
			END DO
		ELSE
			DO col=5,18
				h(col-4)=findmax(c(col),h(col-4))
				l(col-4)=findmin(c(col),l(col-4))
			END DO
		END IF
		row = row + 1
	END DO
	130 CONTINUE
	DO col=1,14
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(23)
	
	DO row = 1, 3
		READ(23,*) temp1
		READ(13,*) temp2
	END DO
	DO
		READ(13,*,END=131) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18)
		131 CONTINUE
		READ(23,*,END=132)t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17),t(18)
		DO col = 5,18
			IF ((c(col)-t(col)) > e(col-4) .OR. (c(col)-t(col)) < -e(col-4)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASS_benchmark.of4"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	132 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASS_benchmark.of4"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of4".' , &
			' Check CLASS_Same_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (13)
	CLOSE (23)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=14, FILE='CLASS_benchmark.of5', STATUS='OLD',ERR=1001)
	OPEN(UNIT=24, FILE='Diana_benchmark_results\CLASS_benchmark.of5', STATUS='OLD')
	ALLOCATE(c(19),h(15),l(15),e(15),t(19))
	
	DO row = 1, 3
		READ(24,*) temp1
	END DO
	row = 1
	DO
		READ(24,*,END=140) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18),c(19)
		IF (row == 1) THEN
			DO col=5,19
				h(col-4)= c(col)
				l(col-4)=c(col)
			END DO
		ELSE
			DO col=5,19
				h(col-4)=findmax(c(col),h(col-4))
				l(col-4)=findmin(c(col),l(col-4))
			END DO
		END IF
		row = row + 1
	END DO
	140 CONTINUE
	DO col=1,15
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(24)
	
	DO row = 1, 3
		READ(24,*) temp1
		READ(14,*) temp2
	END DO
	DO
		READ(14,*,END=141) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18),c(19)
		141 CONTINUE
		READ(24,*,END=142)  t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17),t(18),t(19)
		DO col = 5,19
			IF ((c(col)-t(col)) > e(col-4) .OR. (c(col)-t(col)) < -e(col-4)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASS_benchmark.of5"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	142 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASS_benchmark.of5"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of5".' , &
			' Check CLASS_Same_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (14)
	CLOSE (24)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=15, FILE='CLASS_benchmark.of6', STATUS='OLD',ERR=1001)
	OPEN(UNIT=25, FILE='Diana_benchmark_results\CLASS_benchmark.of6', STATUS='OLD')
	ALLOCATE(c(10),h(7),l(7),e(7),t(10))
	
	DO row = 1, 3
		READ(25,*) temp1
	END DO
	row = 1
	DO
		READ(25,*,END=150)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		IF (row == 1) THEN
			DO col=4,10
				h(col-3)= c(col)
				l(col-3)=c(col)
			END DO
		ELSE
			DO col=4,10
				h(col-3)=findmax(c(col),h(col-3))
				l(col-3)=findmin(c(col),l(col-3))
			END DO
		END IF
		row = row + 1
	END DO
	150 CONTINUE
	DO col=1,7
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(25)
	
	DO row = 1, 3
		READ(25,*) temp1
		READ(15,*) temp2
	END DO
	DO
		READ(15,*,END=151) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		151 CONTINUE
		READ(25,*,END=152) t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9),t(10)
		DO col = 4,10
			IF ((c(col)-t(col)) > e(col-3) .OR. (c(col)-t(col)) < -e(col-3)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASS_benchmark.of6"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	152 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASS_benchmark.of6"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of6".' , &
			' Check CLASS_Same_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (15)
	CLOSE (25)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
!	.of7 code goes here
	
	OPEN(UNIT=17, FILE='CLASS_benchmark.of8', STATUS='OLD',ERR=1001)
	OPEN(UNIT=27, FILE='Diana_benchmark_results\CLASS_benchmark.of8', STATUS='OLD')
	ALLOCATE(c(22),h(22),l(22),e(22),t(22))
	
	DO row = 1, 3
		READ(27,*) temp1
	END DO
	row = 1
	DO
		READ(27,*,END=170) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18), &
						c(19),c(20),c(21),c(22)
		IF (row == 1) THEN
			DO col=1,22
				h(col)= c(col)
				l(col)=c(col)
			END DO
		ELSE
			DO col=1,22
				h(col)=findmax(c(col),h(col))
				l(col)=findmin(c(col),l(col))
			END DO
		END IF
		row = row + 1
	END DO
	170 CONTINUE
	DO col=1,22
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(27)
	
	DO row = 1, 3
		READ(27,*) temp1
		READ(17,*) temp2
	END DO
	DO
		READ(17,*,END=171) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18), &
						c(19),c(20),c(21),c(22)
		171 CONTINUE
		READ(27,*,END=172)t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17),t(18), &
						t(19),t(20),t(21),t(22)
		DO col = 1,22
			IF ((c(col)-t(col)) > e(col) .OR. (c(col)-t(col)) < -e(col)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASS_benchmark.of8"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	172 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASS_benchmark.of8"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of8".' , &
			' Check CLASS_Same_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (17)
	CLOSE (27)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=18, FILE='CLASS_benchmark.of9', STATUS='OLD',ERR=1001)
	OPEN(UNIT=28, FILE='Diana_benchmark_results\CLASS_benchmark.of9', STATUS='OLD')
	ALLOCATE(c(18),h(18),l(18),e(18),t(18))
	
	DO row = 1, 3
		READ(28,*) temp1
	END DO
	row = 1
	DO
		READ(28,*,END=180) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18)
		IF (row == 1) THEN
			DO col=1,18
				h(col)= c(col)
				l(col)=c(col)
			END DO
		ELSE
			DO col=1,18
				h(col)=findmax(c(col),h(col))
				l(col)=findmin(c(col),l(col))
			END DO
		END IF
		row = row + 1
	END DO
	180 CONTINUE
	DO col=1,18
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(28)
	
	DO row = 1, 3
		READ(28,*) temp1
		READ(18,*) temp2
	END DO
	DO
		READ(18,*,END=181)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18)
		181 CONTINUE
		READ(28,*,END=182) t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17),t(18)
		DO col = 1,18
			IF ((c(col)-t(col)) > e(col) .OR. (c(col)-t(col)) < -e(col)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASS_benchmark.of9"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	182 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASS_benchmark.of9"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of9".', &
			' Check CLASS_Same_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (18)
	CLOSE (28)
	DEALLOCATE(c,h,l,e,t)
	
	IF (ok)THEN
		WRITE(32,*)'test=ok'
	ELSE
		WRITE(32,*)'test=Notok'
	END IF
	STOP
	
	1001 CONTINUE
	WRITE(32,*) 'No output files generated. Test failed'
	WRITE(32,*) 'test=Notok'
	
	CLOSE (31)
	CLOSE (32)
	
	CONTAINS
		FUNCTION findmax(c,h)
			REAL :: findmax
			REAL :: c, h
			IF(c > h) THEN
				h=c
			END IF
			findmax=h
		END FUNCTION findmax
		FUNCTION findmin(c,l)
			REAL :: findmin
			REAL :: c, l
			IF(c < l) THEN
				l=c
			END IF
			findmin=l
		END FUNCTION findmin
END PROGRAM CLASS_Same_Results