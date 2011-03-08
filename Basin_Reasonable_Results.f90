PROGRAM Basin_Reasonable_Results
!>        
!>	February 28, 2011 - B. Schweitzer
!>
!>=============================================================================
!>
!>	This program tests to see whether the basin has produced reasonable results
!>
!>=============================================================================
!>
!>	c	- an array of the values of the columns of the file
!>	rng	-array of high and low values for the MESH streamflow output
!>	rof#	-array of high and low values for each of the CLASS output files
!>
!>=============================================================================
	INTEGER :: row=1 , fail = 0,col =1
	CHARACTER :: temp
	REAL, DIMENSION(:),ALLOCATABLE :: c
	REAL, DIMENSION(:,:),ALLOCATABLE :: rng,rof1,rof2,rof3,rof4,rof5,rof6,rof7,rof8,rof9
	LOGICAL :: ok=.true.
	
	ALLOCATE(c(3),rng(2,1),rof1(2,15),rof2(2,23),rof3(2,8),rof4(2,14), &
		rof5(2,23),rof6(2,7),rof7(2,12),rof8(2,22),rof9(2,21))
	rng(1,1)= 3.824; rng(2,1)=0
	rof1(1,1)=151.64;	rof1(2,1)=12.41;	rof1(1,2)=-8.48;	rof1(2,2)=-62.64
	rof1(1,3)=78.07;	rof1(2,3)=-14.1;	rof1(1,4)=64.37;	rof1(2,4)=-3.45
	rof1(1,5)=36.18;	rof1(2,5)=-0.45;	rof1(1,6)=38.48;	rof1(2,6)=-10
	rof1(1,7)=-112.49;	rof1(2,7)=-133.96;	rof1(1,8)=68.16;	rof1(2,8)=0
	rof1(1,9)=600.68;	rof1(2,9)=0;		rof1(1,10)=0.434;	rof1(2,10)=0
	rof1(1,11)=0.788;	rof1(2,11)=0.153;	rof1(1,12)=4.3932;	rof1(2,12)=0
	rof1(1,13)=0.0038;	rof1(2,13)=0;		rof1(1,14)=4.3932;	rof1(2,14)=0
	rof1(1,15)=0;		rof1(2,15)=0
	
	rof2(1,1)=15.18;	rof2(2,1)=-10.6;	rof2(1,2)=0.237; 	rof2(2,2)=0.022
	rof2(1,3)=0.404;	rof2(2,3)=0;		rof2(1,4)=5.65;	rof2(2,4)=-10.32
	rof2(1,5)=0.153;	rof2(2,5)=0.022;	rof2(1,6)=0.141;	rof2(2,6)=0
	rof2(1,7)=1.82;	rof2(2,7)=-8.77;	rof2(1,8)=0.136;	rof2(2,8)=0.022
	rof2(1,9)=0.141;	rof2(2,9)=0.039;	rof2(1,10)=-0.35;	rof2(2,10)=-8.27
	rof2(1,11)=0.039;	rof2(2,11)=0.022;	rof2(1,12)=0.135;	rof2(2,12)=0.122
	rof2(1,13)=-0.9;	rof2(2,13)=-8.08;	rof2(1,14)=0.022;	rof2(2,14)=0.022
	rof2(1,15)=0.135;	rof2(2,15)=0.135;	rof2(1,16)=-1.21;	rof2(2,16)=-8.03
	rof2(1,17)=0.022;	rof2(2,17)=0.022;	rof2(1,18)=0.135;	rof2(2,18)=0.135
	rof2(1,19)=23.97;	rof2(2,19)=-5.37;	rof2(1,20)=0.1733;	rof2(2,20)=0
	rof2(1,21)=0.1389;	rof2(2,21)=0;		rof2(1,22)=0;		rof2(2,22)=-11.42
	rof2(1,23)=0.29;	rof2(2,23)=0
	
	rof3(1,1)=185.82;	rof3(2,1)=14.83;	rof3(1,2)=200.36; 	rof3(2,2)=102.29
	rof3(1,3)=-112.26;	rof3(2,3)=-132.96;	rof3(1,4)=6.85;	rof3(2,4)=0.61
	rof3(1,5)=55330.66;rof3(2,5)=51918.06;rof3(1,6)=0.00852;	rof3(2,6)=6.49E-4
	rof3(1,7)=5.4;	rof3(2,7)=0;		rof3(1,8)=2.224;	rof3(2,8)=-0.105
	
	rof4(1,1)=580.66;	rof4(2,1)=-5.26;	rof4(1,2)=9.25; 	rof4(2,2)=-200.17
	rof4(1,3)=388.64;	rof4(2,3)=-64.74;	rof4(1,4)=266.06;	rof4(2,4)=-47.49
	rof4(1,5)=195.02;	rof4(2,5)=-29.06;	rof4(1,6)=202.09;	rof4(2,6)=-69.25
	rof4(1,7)=39.9;	rof4(2,7)=-24.97;	rof4(1,8)=117.36;	rof4(2,8)=0
	rof4(1,9)=433.85;	rof4(2,9)=0;		rof4(1,10)=0.868;	rof4(2,10)=0
	rof4(1,11)=0.84;	rof4(2,11)=0;		rof4(1,12)=0.000738;rof4(2,12)=0
	rof4(1,13)=0;		rof4(2,13)=0;		rof4(1,14)=0;		rof4(2,14)=0
	
	rof5(1,1)=23.9;	rof5(2,1)=-10.97;	rof5(1,2)=0.535; 	rof5(2,2)=0.04
	rof5(1,3)=0.864;	rof5(2,3)=0;		rof5(1,4)=3.89;	rof5(2,4)=-10.92
	rof5(1,5)=0.297;	rof5(2,5)=0.04;	rof5(1,6)=0.275;	rof5(2,6)=0
	rof5(1,7)=0;		rof5(2,7)=-8.7;	rof5(1,8)=0.241;	rof5(2,8)=0.04
	rof5(1,9)=0.263;	rof5(2,9)=0.093;	rof5(1,10)=-0.48;	rof5(2,10)=-8.25
	rof5(1,11)=0.04;	rof5(2,11)=0.04;	rof5(1,12)=0.252;	rof5(2,12)=0.25
	rof5(1,13)=-1.08;	rof5(2,13)=-8.08;	rof5(1,14)=0.04;	rof5(2,14)=0.04
	rof5(1,15)=0.25;	rof5(2,15)=0.25;	rof5(1,16)=-1.4;	rof5(2,16)=-8.03
	rof5(1,17)=0.04;	rof5(2,17)=0.04;	rof5(1,18)=0.25;	rof5(2,18)=0.25
	rof5(1,19)=38.91;	rof5(2,19)=-14.63;	rof5(1,20)=0.4788;	rof5(2,20)=0
	rof5(1,21)=0.6359;	rof5(2,21)=0;		rof5(1,22)=0;		rof5(2,22)=-12.36
	rof5(1,23)=0.511;	rof5(2,23)=0
	
	rof6(1,1)=729.11;	rof6(2,1)=-5.26;	rof6(1,2)=438.1; 	rof6(2,2)=167.42
	rof6(1,3)=0.001361;rof6(2,3)=0;		rof6(1,4)=31.2;	rof6(2,4)=-19
	rof6(1,5)=14.94;	rof6(2,5)=0.1;	rof6(1,6)=102560;	rof6(2,6)=95840
	rof6(1,7)=0.022413;rof6(2,7)=0.00074
	
	rof7(1,1)=300.5;	rof7(2,1)=0;		rof7(1,2)=300.5; 	rof7(2,2)=0
	rof7(1,3)=0;		rof7(2,3)=0;		rof7(1,4)=0;		rof7(2,4)=0
	rof7(1,5)=7.384E-4;rof7(2,5)=0;		rof7(1,6)=5.492E-6;rof7(2,6)=0
	rof7(1,7)=7.384E-4;rof7(2,7)=0;		rof7(1,8)=0;		rof7(2,8)=0
	rof7(1,9)=1;		rof7(2,9)=0;		rof7(1,10)=0.9894;	rof7(2,10)=0
	rof7(1,11)=1;		rof7(2,11)=0;		rof7(1,12)=0;		rof7(2,12)=0
	
!	rof8(1,1)=1602.727;rof8(2,1)=-578.551;rof8(1,2)=204.3672;rof8(2,2)=-143.422
!	rof8(1,3)=55.6875;	rof8(2,3)=-14.3438;rof8(1,4)=4.9375;	rof8(2,4)=-0.7812
!	rof8(1,5)=2.0625;	rof8(2,5)=-5.0765;	rof8(1,6)=1.625;	rof8(2,6)=-0.125
!	rof8(1,7)=450.041;	rof8(2,7)=-5.26;	rof8(1,8)=214.4055;rof8(2,8)=-8.6227
!	rof8(1,9)=142.205;	rof8(2,9)=-0.9387;	rof8(1,10)=13.5248;rof8(2,10)=-197.3311
!	rof8(1,11)=37.789;	rof8(2,11)=-82.635;rof8(1,12)=0.252;	rof8(2,12)=0.25
!	rof8(1,13)=-1.08;	rof8(2,13)=-8.08;	rof8(1,14)=0.04;	rof8(2,14)=0.04
!	rof8(1,15)=0.25;	rof8(2,15)=0.25;	rof8(1,16)=-1.4;	rof8(2,16)=-8.03
!	rof8(1,17)=0.04;	rof8(2,17)=0.04;	rof8(1,18)=0.25;	rof8(2,18)=0.25
!	rof8(1,19)=38.91;	rof8(2,19)=-14.63;	rof8(1,20)=0.4788;	rof8(2,20)=0
!	rof8(1,21)=0.6359;	rof8(2,21)=0;		rof8(1,22)=0;		rof8(2,22)=-12.36
	
	rof9(1,1)=7.047E-5;rof9(2,1)=0;		rof9(1,2)=0.001151;rof9(2,2)=0
	rof9(1,3)=2.21E-4;	rof9(2,3)=0;		rof9(1,4)=0.00136;	rof9(2,4)=0
	rof9(1,5)=6.036E-6;rof9(2,5)=-3.047E-6;rof9(1,6)=9.759E-5;rof9(2,6)=-1.899E-5
	rof9(1,7)=2.061E-5;rof9(2,7)=-6.914E-6;rof9(1,8)=3.506E-5;rof9(2,8)=-9.363E-6
	rof9(1,9)=7.71E-5;	rof9(2,9)=0;		rof9(1,10)=0;		rof9(2,10)=0
	rof9(1,11)=0;		rof9(2,11)=0;		rof9(1,12)=0;		rof9(2,12)=0
	rof9(1,13)=0;		rof9(2,13)=0;		rof9(1,14)=0;		rof9(2,14)=0
	rof9(1,15)=1.15E-3;rof9(2,15)=0;		rof9(1,16)=5.839E-4;rof9(2,16)=0
	rof9(1,17)=5.492E-6;rof9(2,17)=0;	rof9(1,18)=7.384E-4;rof9(2,18)=0
	rof9(1,19)=0;		rof9(2,19)=-6.424E-6;rof9(1,20)=0;	rof9(2,20)=0
	rof9(1,21)=6.422E-6;rof9(2,21)=0
	
	OPEN(UNIT=32,FILE='Test_Results\test.err',STATUS='REPLACE')
	OPEN(UNIT=10,FILE='BASINAVG1\MESH_output_streamflow.csv',STATUS='OLD',ERR=1001)
	OPEN(UNIT=31,FILE='Test_Results\Basin_Reasonable_failures.txt',STATUS='REPLACE')
	
	DO
		READ(10,*,END=101) c(1),c(2),c(3)
		IF(c(3) >  rng(1,1).OR.  c(3) < rng(2,1))THEN
			WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "MESH_output_streamflow.csv"'
			fail = fail + 1
		END IF
		row = row + 1
	END DO
	101 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "MESH_output_streamflow.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "MESH_output_streamflow.csv".' , &
			' Check Basin_Reasonable_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (10)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=11,FILE='CLASSOUT1/CLASSOF1.csv',STATUS='OLD',ERR=1001)
	ALLOCATE(c(17))
	DO row = 1, 4
		READ(11,*) temp
	END DO
	DO
		READ(11,*,END=111) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17)
		DO col = 3, 17
			IF ((c(col) > rof1(1,(col-2))) .OR. (c(col) < rof1(2,(col-2)))) THEN
				WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "CLASSOF1.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	111 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASSOF1.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF1.csv".' , &
			' Check Basin_Reasonable_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (11)
	DEALLOCATE(c)
	fail = 0
	
	
	OPEN(UNIT=12, FILE='CLASSOUT1/CLASSOF2.csv', STATUS='OLD',ERR=1001)
	ALLOCATE(c(25))
	
	DO row = 1, 4
		READ(12,*) temp
	END DO
	DO
		READ(12,*,END=121)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21),c(22),c(23),c(24),c(25)
		DO col = 3, 25
			IF ((c(col) > rof2(1,(col-2))) .OR. (c(col) < rof2(2,(col-2)))) THEN
				WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "CLASSOF2.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	121 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASSOF2.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF2.csv".' , &
			' Check Basin_Reasonable_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (12)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=13, FILE='CLASSOUT1/CLASSOF3.csv', STATUS='OLD',ERR=1001)
	ALLOCATE(c(10))
	
	DO row = 1, 4
		READ(13,*) temp
	END DO
	DO
		READ(13,*,END=131)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		DO col = 3, 10
			IF ((c(col) > rof3(1,(col-2))) .OR. (c(col) < rof3(2,(col-2)))) THEN
				WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "CLASSOF3.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	131 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASSOF3.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF3.csv".' , &
			' Check Basin_Reasonable_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (13)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=14, FILE='CLASSOUT1/CLASSOF4.csv', STATUS='OLD',ERR=1001)
	ALLOCATE(c(18))
	
	DO row = 1, 4
		READ(14,*) temp
	END DO
	DO
		READ(14,*,END=141)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), c(18)
		DO col = 5, 18
			IF ((c(col) > rof4(1,(col-4))) .OR. (c(col) < rof4(2,(col-4)))) THEN
				WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "CLASSOF4.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	141 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASSOF4.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF4.csv".' , &
			' Check Basin_Reasonable_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (14)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=15, FILE='CLASSOUT1/CLASSOF5.csv', STATUS='OLD',ERR=1001)
	ALLOCATE(c(27))
	
	DO row = 1, 4
		READ(15,*) temp
	END DO
	DO
		READ(15,*,END=151) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21),c(22),c(23),c(24),c(25), &
						c(26),c(27)
		DO col = 5, 27
			IF ((c(col) > rof5(1,(col-4))) .OR. (c(col) < rof5(2,(col-4)))) THEN
				WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "CLASSOF5.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	151 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASSOF5.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF5.csv".' , &
			' Check Basin_Reasonable_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (15)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=16, FILE='CLASSOUT1/CLASSOF6.csv', STATUS='OLD',ERR=1001)
	ALLOCATE(c(10))
	
	DO row = 1, 4
		READ(16,*) temp
	END DO
	DO
		READ(16,*,END=161)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		DO col = 4, 10
			IF ((c(col) > rof6(1,(col-3))) .OR. (c(col) < rof6(2,(col-3)))) THEN
				WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "CLASSOF6.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	161 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASSOF6.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF6.csv".' , &
			' Check Basin_Reasonable_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (16)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=17, FILE='CLASSOUT1/CLASSOF7.csv', STATUS='OLD',ERR=1001)
	ALLOCATE(c(12))
	
	DO row = 1, 4
		READ(17,*) temp
	END DO
	DO
		READ(17,*,END=171) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12)
		DO col = 1, 12
			IF ((c(col) > rof7(1,(col))) .OR. (c(col) < rof7(2,(col)))) THEN
				WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "CLASSOF7.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	171 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASSOF7.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF7.csv".' , &
		' Check Basin_Reasonable_failures.txt for details'
	ok = .false.
	END IF
	
	CLOSE (17)
	DEALLOCATE(c)
	fail = 0
	
!	OPEN(UNIT=18, FILE='CLASSOUT1/CLASSOF8.csv', STATUS='OLD',ERR=1001)
!	ALLOCATE(c(22))
!	
!	DO row = 1, 4
!		READ(18,*) temp
!	END DO
!	DO
!		READ(18,*,END=181) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
!						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
!						c(18),c(19),c(20),c(21),c(22)
!		DO col = 1, 22
!			IF ((c(col) > rof8(1,(col))) .OR. (c(col) < rof8(2,(col)))) THEN
!				WRITE(31,*)'Test failed in row ', row, ', column ', &
!					col, ' of the file "CLASSOF8.csv"'
!				fail = fail + 1
!			END IF
!		END DO
!		row = row + 1
!	END DO
!	181 CONTINUE
!	IF (fail == 0)THEN
!		WRITE(32,*)'Reasonable results in file "CLASSOF8.csv"'
!	ELSE
!		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF8.csv".' , &
!			' Check Basin_Reasonable_failures.txt for details'
!		ok = .false.
!	END IF
!	
!	CLOSE (18)
!	DEALLOCATE(c)
!	fail = 0
	
	OPEN(UNIT=19, FILE='CLASSOUT1/CLASSOF9.csv', STATUS='OLD',ERR=1001)
	ALLOCATE(c(21))
	
	DO row = 1, 4
		READ(19,*) temp
	END DO
	DO
		READ(19,*,END=191)c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21)
		DO col = 1, 21
			IF ((c(col) > rof9(1,(col))) .OR. (c(col) < rof9(2,(col)))) THEN
				WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "CLASSOF9.csv"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	191 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASSOF9.csv"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASSOF9.csv".', &
			' Check Basin_Reasonable_failures.txt for details'
		ok = .false.
	END IF
	
	CLOSE (19)
	DEALLOCATE(c)
	
	IF(ok)THEN
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
END PROGRAM Basin_Reasonable_Results