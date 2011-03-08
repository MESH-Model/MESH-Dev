PROGRAM Basin_Same_Results
!>        
!>	February 23, 2011 - B. Schweitzer
!>
!>=============================================================================
!>
!>	This program tests to see whether the Basin has produced the same results as previous
!>	runs
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
	INTEGER :: row , fail = 0, col
	CHARACTER :: temp1,temp2
	REAL, DIMENSION(:), ALLOCATABLE :: c,h,l,e,t
	LOGICAL :: ok=.true.
	
	OPEN(UNIT=32,FILE='Test_Results\test.err',STATUS='REPLACE')
	OPEN(UNIT=10,FILE='BASINAVG1\MESH_output_streamflow.csv',STATUS='OLD',ERR=1001)
	OPEN(UNIT=20,FILE='BASINAVG1\MESH_output_streamflow.TS',STATUS='OLD')
	OPEN(UNIT=31,FILE='Test_Results\Basin_Same_failures.txt',STATUS='REPLACE')
	
	
	ALLOCATE(c(3),h(1),l(1),e(1),t(3))
	
	!> find the max/min values in the MESH streamflow output file
	DO
		READ(20,*,END=100) c(1),c(2),c(3)
		IF(row==1)THEN
			h(1)=c(3)
			l(1)=c(3)
		ELSE
			h(1) = findmax(c(3),h(1))
			l(1) = findmin(c(3),l(1))
		END IF
		row = row + 1
	END DO
	100 CONTINUE
	
	e(1)=ABS(h(1)-l(1))*0.0001
	
	REWIND(20)
	DO
		READ(10,*,END=101) c(1),c(2),c(3)
		101 CONTINUE
		READ(20,*,END=102) t(1),t(2),t(3)
		IF ((c(3)-t(3)) > e(1) .OR. (c(3)-t(3)) < -e(1)) THEN
			WRITE(31,*)'Test failed in row ',row, ', column', & 
					col, 'of the file "MESH_output_streamflow.csv"'
			fail = fail + 1
		END IF
		row = row + 1
	END DO
	102 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "MESH_output_streamflow.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "MESH_output_streamflow.csv".' , &
			' Check Basin_Same_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (10)
	CLOSE (20)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=11,FILE='CLASSOUT1/CLASSOF1.csv',STATUS='OLD',ERR=1001)
	OPEN(UNIT=21,FILE='CLASSOUT1/CLASSOF1.TS',STATUS='OLD')
	ALLOCATE(c(17),h(15),l(15),e(15),t(17))
	
	DO row = 1, 4
		READ(21,*) temp1
	END DO
	row = 1
	DO
		READ(21,*,END=110)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17)
		IF(row==1)THEN
			DO col=3,17
				h(col-2)= c(col)
				l(col-2)=c(col)
			END DO
		ELSE
			DO col=3,17
				h(col-2)=findmax(c(col),h(col-2))
				l(col-2)=findmin(c(col),l(col-2))
			END DO
		END IF
		row = row + 1
	END DO
	110 CONTINUE
	DO col=1,17
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(21)
	DO row = 1, 4
		READ(21,*) temp1
		READ(11,*) temp2
	END DO
	DO
		READ(11,*,END=111)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17)
		111 CONTINUE
		READ(21,*,END=112) t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17)
		DO col = 3,17
			IF ((c(col)-t(col)) > e(col-2) .OR. (c(col)-t(col)) < -e(col-2)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASSOF1.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	112 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASSOF1.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF1.csv".' , &
			' Check Basin_Same_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (11)
	CLOSE (21)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	
	OPEN(UNIT=12, FILE='CLASSOUT1/CLASSOF2.csv', STATUS='OLD',ERR=1001)
	OPEN(UNIT=22, FILE='CLASSOUT1/CLASSOF2.TS', STATUS='OLD')
	ALLOCATE(c(25),h(23),l(23),e(23),t(25))
	
	DO row = 1, 4
		READ(22,*) temp1
	END DO
	row = 1
	DO
		READ(22,*,END=120) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21),c(22),c(23),c(24),c(25)
		IF (row == 1) THEN
			DO col=3,25
				h(col-2)= c(col)
				l(col-2)=c(col)
			END DO
		ELSE
			DO col=3,25
				h(col-2)=findmax(c(col),h(col-2))
				l(col-2)=findmin(c(col),l(col-2))
			END DO
		END IF
		row = row + 1
	END DO
	120 CONTINUE
	DO col=1,23
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(22)
	
	DO row = 1, 4
		READ(22,*) temp1
		READ(12,*) temp2
	END DO
	DO
		READ(12,*,END=121)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21),c(22),c(23),c(24),c(25)
		121 CONTINUE
		READ(22,*,END=122) t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17), &
						t(18),t(19),t(20),t(21),t(22),t(23),t(24),t(25)
		DO col = 3,25
			IF ((c(col)-t(col)) > e(col-2) .OR. (c(col)-t(col)) < -e(col-2)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASSOF2.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	122 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASSOF2.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF2.csv".' , &
			' Check Basin_Same_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (12)
	CLOSE (22)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=13, FILE='CLASSOUT1/CLASSOF3.csv', STATUS='OLD',ERR=1001)
	OPEN(UNIT=23, FILE='CLASSOUT1/CLASSOF3.TS', STATUS='OLD')
	ALLOCATE(c(10),h(8),l(8),e(8),t(10))
	
	DO row = 1, 4
		READ(23,*) temp1
	END DO
	row = 1
	DO
		READ(23,*,END=130) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
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
	130 CONTINUE
	DO col=1,8
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(23)
	
	DO row = 1, 4
		READ(23,*) temp1
		READ(13,*) temp2
	END DO
	DO
		READ(13,*,END=131)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		131 CONTINUE
		READ(23,*,END=132) t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), t(10)
		DO col = 3,10
			IF ((c(col)-t(col)) > e(col-2) .OR. (c(col)-t(col)) < -e(col-2)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASSOF3.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	132 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASSOF3.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF3.csv".' , &
			' Check Basin_Same_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (13)
	CLOSE (23)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=14, FILE='CLASSOUT1/CLASSOF4.csv', STATUS='OLD',ERR=1001)
	OPEN(UNIT=24, FILE='CLASSOUT1/CLASSOF4.TS', STATUS='OLD')
	ALLOCATE(c(18),h(14),l(14),e(14),t(18))
	
	DO row = 1, 4
		READ(24,*) temp1
	END DO
	row = 1
	DO
		READ(24,*,END=140) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
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
	140 CONTINUE
	DO col=1,14
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(24)
	
	DO row = 1, 4
		READ(24,*) temp1
		READ(14,*) temp2
	END DO
	DO
		READ(14,*,END=141)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18)
		141 CONTINUE
		READ(24,*,END=142)t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17),t(18)
		DO col = 5,18
			IF ((c(col)-t(col)) > e(col-4) .OR. (c(col)-t(col)) < -e(col-4)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASSOF4.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	142 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASSOF4.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF4.csv".' , &
			' Check Basin_Same_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (14)
	CLOSE (24)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=15, FILE='CLASSOUT1/CLASSOF5.csv', STATUS='OLD',ERR=1001)
	OPEN(UNIT=25, FILE='CLASSOUT1/CLASSOF5.TS', STATUS='OLD')
	ALLOCATE(c(27),h(23),l(23),e(23),t(27))
	
	DO row = 1, 4
		READ(25,*) temp1
	END DO
	row = 1
	DO
		READ(25,*,END=150)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21),c(22),c(23),c(24),c(25), &
						c(26),c(27)
		IF (row == 1) THEN
			DO col=5,27
				h(col-4)= c(col)
				l(col-4)=c(col)
			END DO
		ELSE
			DO col=5,27
				h(col-4)=findmax(c(col),h(col-4))
				l(col-4)=findmin(c(col),l(col-4))
			END DO
		END IF
		row = row + 1
	END DO
	150 CONTINUE
	DO col=1,23
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	REWIND(25)
	
	DO row = 1, 4
		READ(25,*) temp1
		READ(15,*) temp2
	END DO
	DO
		READ(15,*,END=151) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21),c(22),c(23),c(24),c(25), &
						c(26),c(27)
		151 CONTINUE
		READ(25,*,END=152) t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17), &
						t(18),t(19),t(20),t(21),t(22),t(23),t(24),t(25), &
						t(26),t(27)
		DO col = 5,27
			IF ((c(col)-t(col)) > e(col-4) .OR. (c(col)-t(col)) < -e(col-4)) THEN
				WRITE(31,*)'Test failed in row ', row, ', column', & 
					col, 'of the file "CLASSOF5.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	152 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASSOF5.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF5.csv".' , &
			' Check Basin_Same_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (15)
	CLOSE (25)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=16, FILE='CLASSOUT1/CLASSOF6.csv', STATUS='OLD',ERR=1001)
	OPEN(UNIT=26, FILE='CLASSOUT1/CLASSOF6.TS', STATUS='OLD')
	ALLOCATE(c(10),h(7),l(7),e(7),t(10))
	
	DO row = 1, 4
		READ(26,*) temp1
	END DO
	row = 1
	DO
		READ(26,*,END=160) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
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
	160 CONTINUE
	DO col=1,7
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(26)
	
	DO row = 1, 4
		READ(26,*) temp1
		READ(16,*) temp2
	END DO
	DO
		READ(16,*,END=161)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		161 CONTINUE
		READ(26,*,END=162)  t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9),t(10)
		DO col = 4,10
			IF ((c(col)-t(col)) > e(col-3) .OR. (c(col)-t(col)) < -e(col-3)) THEN
					WRITE(31,*)'Test failed in row ', row, ', column', & 
						col, 'of the file "CLASSOF6.csv"'
					fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	162 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASSOF6.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF6.csv".' , &
			' Check Basin_Same_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (16)
	CLOSE (26)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=17, FILE='CLASSOUT1/CLASSOF7.csv', STATUS='OLD',ERR=1001)
	OPEN(UNIT=27, FILE='CLASSOUT1/CLASSOF7.TS', STATUS='OLD')
	ALLOCATE(c(12),h(12),l(12),e(12),t(12))
	
	DO row = 1, 4
		READ(27,*) temp1
	END DO
	row = 1
	DO
		READ(27,*,END=170)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12)
		IF (row == 1) THEN
			DO col=1,12
				h(col)= c(col)
				l(col)=c(col)
			END DO
		ELSE
			DO col=1,12
				h(col)=findmax(c(col),h(col))
				l(col)=findmin(c(col),l(col))
			END DO
		END IF
		row = row + 1
	END DO
	170 CONTINUE
	DO col=1,12
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(27)
	
	DO row = 1, 4
		READ(27,*) temp1
		READ(17,*) temp2
	END DO
	DO
		READ(17,*,END=171) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12)
		171 CONTINUE
		READ(27,*,END=172)  t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12)
		DO col = 1,12
			IF ((c(col)-t(col)) > e(col) .OR. (c(col)-t(col)) < -e(col)) THEN
					WRITE(31,*)'Test failed in row ', row, ', column', & 
						col, 'of the file "CLASSOF7.csv"'
					fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	172 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASSOF7.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF7.csv".' , &
		' Check Basin_Same_failures.txt for details'
	ok = .false.
	END IF
	
	CLOSE (17)
	CLOSE (27)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=18, FILE='CLASSOUT1/CLASSOF8.csv', STATUS='OLD',ERR=1001)
	OPEN(UNIT=28, FILE='CLASSOUT1/CLASSOF8.TS', STATUS='OLD')
	ALLOCATE(c(22),h(22),l(22),e(22),t(22))
	
	DO row = 1, 4
		READ(28,*) temp1
	END DO
	row = 1
	DO
		READ(28,*,END=180) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21),c(22)
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
	180 CONTINUE
	DO col=1,22
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(28)
	
	DO row = 1, 4
		READ(28,*) temp1
		READ(18,*) temp2
	END DO
	DO
		READ(18,*,END=181) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21),c(22)
		181 CONTINUE
		READ(28,*,END=182)  t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17), &
						t(18),t(19),t(20),t(21),t(22)
		DO col = 1,22
			IF ((c(col)-t(col)) > e(col) .OR. (c(col)-t(col)) < -e(col)) THEN
					WRITE(31,*)'Test failed in row ', row, ', column', & 
						col, 'of the file "CLASSOF8.csv"'
					fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	182 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASSOF8.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF8.csv".' , &
			' Check Basin_Same_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (18)
	CLOSE (28)
	DEALLOCATE(c,h,l,e,t)
	fail = 0
	
	OPEN(UNIT=19, FILE='CLASSOUT1/CLASSOF9.csv', STATUS='OLD',ERR=1001)
	OPEN(UNIT=29, FILE='CLASSOUT1/CLASSOF9.TS', STATUS='OLD')
	ALLOCATE(c(21),h(21),l(21),e(21),t(21))
	
	DO row = 1, 4
		READ(29,*) temp1
	END DO
	row = 1
	DO
		READ(29,*,END=190)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21)
		IF (row == 1) THEN
			DO col=1,21
				h(col)= c(col)
				l(col)=c(col)
			END DO
		ELSE
			DO col=1,21
				h(col)=findmax(c(col),h(col))
				l(col)=findmin(c(col),l(col))
			END DO
		END IF
		row = row + 1
	END DO
	190 CONTINUE
	DO col=1,21
		e(col)= ABS(h(col)-l(col))*0.0001
	END DO
	
	REWIND(29)
	
	DO row = 1, 4
		READ(29,*) temp1
		READ(19,*) temp2
	END DO
	DO
		READ(19,*,END=191)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21)
		191 CONTINUE
		READ(29,*,END=192)  t(1),t(2),t(3),t(4),t(5),t(6),t(7),t(8),t(9), & 
						t(10),t(11),t(12),t(13),t(14),t(15),t(16),t(17), &
						t(18),t(19),t(20),t(21)
		DO col = 1,21
			IF ((c(col)-t(col)) > e(col) .OR. (c(col)-t(col)) < -e(col)) THEN
					WRITE(31,*)'Test failed in row ', row, ', column', & 
						col, 'of the file "CLASSOF9.csv"'
					fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	192 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Same results in file "CLASSOF9.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF9.csv".', &
			' Check Basin_Same_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (19)
	CLOSE (29)
	
	IF(ok)THEN
		WRITE(32,*)'test=ok'
	ELSE
		WRITE(32,*)'test=Notok'
	END IF
	CLOSE (30)
	CLOSE (31)
	STOP
	
	1001 CONTINUE
	WRITE(32,*) 'No output files generated. Test failed'
	WRITE(32,*) 'test=Notok'
	
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
END PROGRAM Basin_Same_Results