PROGRAM CLASS_Reasonable_Results
!>        
!>	February 28, 2011 - B. Schweitzer
!>
!>=============================================================================
!>
!>	This program tests to see whether CLASS has produced reasonable results
!>
!>=============================================================================
!>
!>	c	- an array of the values of the columns of the file
!>	rof#	-array of high and low values for each of the CLASS output files
!>
!>=============================================================================
	INTEGER :: row=1 , fail = 0,col =1
	CHARACTER :: temp
	REAL, DIMENSION(:),ALLOCATABLE :: c
	REAL, DIMENSION(:,:),ALLOCATABLE :: rof1,rof2,rof3,rof4,rof5,rof6,rof7,rof8,rof9
	LOGICAL :: ok=.true.
	
	ALLOCATE(c(15),rof1(2,13),rof2(2,14),rof3(2,8),rof4(2,14), &
		rof5(2,15),rof6(2,7),rof7(2,12),rof8(2,22),rof9(2,18))
	
	rof1(1,1)=310.89;	rof1(2,1)=4.69;	rof1(1,2)=20.88;	rof1(2,2)=-158.16
	rof1(1,3)=157.78;	rof1(2,3)=-57.51;	rof1(1,4)=194.53;	rof1(2,4)=-6.34
	rof1(1,5)=37.62;	rof1(2,5)=-8.22;	rof1(1,6)=83.91;	rof1(2,6)=-44.85
	rof1(1,7)=28.62;	rof1(2,7)=-36.53;	rof1(1,8)=84.12;	rof1(2,8)=0
	rof1(1,9)=425.26;	rof1(2,9)=0;		rof1(1,10)=0.349;	rof1(2,10)=0
	rof1(1,11)=0.311;	rof1(2,11)=0.115;	rof1(1,12)=3.4385;	rof1(2,12)=0
	rof1(1,13)=535;	rof1(2,13)=0;
	
	rof2(1,1)=21.74;	rof2(2,1)=-20.8;	rof2(1,2)=0.859; 	rof2(2,2)=0.04
	rof2(1,3)=0.782;	rof2(2,3)=0;		rof2(1,4)=15.23;	rof2(2,4)=-14.15
	rof2(1,5)=0.361;	rof2(2,5)=0.04;	rof2(1,6)=0.402;	rof2(2,6)=0
	rof2(1,7)=9.43;	rof2(2,7)=0;		rof2(1,8)=0.285;	rof2(2,8)=0.105
	rof2(1,9)=0.027;	rof2(2,9)=0;		rof2(1,10)=28.61;	rof2(2,10)=-36.14
	rof2(1,11)=0.7496;	rof2(2,11)=0;		rof2(1,12)=3.5432;	rof2(2,12)=0
	rof2(1,13)=0;		rof2(2,13)=-31.7;	rof2(1,14)=0.415;	rof2(2,14)=0
	
	rof3(1,1)=362.38;	rof3(2,1)=6.07;	rof3(1,2)=384.72; 	rof3(2,2)=117.57
	rof3(1,3)=27.71;	rof3(2,3)=-35.94;	rof3(1,4)=8.24;	rof3(2,4)=0.29
	rof3(1,5)=96745.65;rof3(2,5)=92107.77;rof3(1,6)=0.0129;	rof3(2,6)=0.000121
	rof3(1,7)=52;		rof3(2,7)=0;		rof3(1,8)=6.72;	rof3(2,8)=-0.193
	
	rof4(1,1)=891.72;	rof4(2,1)=-0.85;	rof4(1,2)=32.23; 	rof4(2,2)=-285.53
	rof4(1,3)=1044.24;	rof4(2,3)=-643.33;	rof4(1,4)=1008.28;	rof4(2,4)=-101.24
	rof4(1,5)=149.3;	rof4(2,5)=-179.53;	rof4(1,6)=201.71;	rof4(2,6)=-124.41
	rof4(1,7)=43.21;	rof4(2,7)=-44.23;	rof4(1,8)=84.13;	rof4(2,8)=0
	rof4(1,9)=428.21;	rof4(2,9)=0;		rof4(1,10)=0.973;	rof4(2,10)=0
	rof4(1,11)=0.33;	rof4(2,11)=0;		rof4(1,12)=0.00041;rof4(2,12)=0
	rof4(1,13)=8.88;	rof4(2,13)=0;		rof4(1,14)=0.006;	rof4(2,14)=0
	
	rof5(1,1)=33.13;	rof5(2,1)=-22.27;	rof5(1,2)=0.861; 	rof5(2,2)=0.04
	rof5(1,3)=0.792;	rof5(2,3)=0;		rof5(1,4)=15.57;	rof5(2,4)=-14.23
	rof5(1,5)=0.373;	rof5(2,5)=0.04;	rof5(1,6)=0.402;	rof5(2,6)=0
	rof5(1,7)=9.43;	rof5(2,7)=0;		rof5(1,8)=0.285;	rof5(2,8)=0.105 !>(1,7) is causing failures
	rof5(1,9)=0.027;	rof5(2,9)=0;		rof5(1,10)=40.47;	rof5(2,10)=-45.17
	rof5(1,11)=0.9;	rof5(2,11)=0;		rof5(1,12)=3.9156;	rof5(2,12)=0
	rof5(1,13)=0;		rof5(2,13)=-34.14;	rof5(1,14)=0.425;	rof5(2,14)=0
	rof5(1,15)=56.423;	rof5(2,15)=-43.193;
	
	rof6(1,1)=1054.07;	rof6(2,1)=-0.85;	rof6(1,2)=432.61; 	rof6(2,2)=109.71
	rof6(1,3)=0.0133;	rof6(2,3)=0;		rof6(1,4)=35.04;	rof6(2,4)=-38.6
	rof6(1,5)=11;		rof6(2,5)=0.1;	rof6(1,6)=97113;	rof6(2,6)=91566
	rof6(1,7)=0.0158;	rof6(2,7)=8.86E-5
	
!	rof7(1,1)=300.5;	rof7(2,1)=0;		rof7(1,2)=300.5; 	rof7(2,2)=0
!	rof7(1,3)=0;		rof7(2,3)=0;		rof7(1,4)=0;		rof7(2,4)=0
!	rof7(1,5)=7.384E-4;rof7(2,5)=0;		rof7(1,6)=5.492E-6;rof7(2,6)=0
!	rof7(1,7)=7.384E-4;rof7(2,7)=0;		rof7(1,8)=0;		rof7(2,8)=0
!	rof7(1,9)=1;		rof7(2,9)=0;		rof7(1,10)=0.9894;	rof7(2,10)=0
!	rof7(1,11)=1;		rof7(2,11)=0;		rof7(1,12)=0;		rof7(2,12)=0
	
	rof8(1,1)=724.0422;rof8(2,1)=-0.85;	rof8(1,2)=123.758;	rof8(2,2)=0
	rof8(1,3)=496.683;	rof8(2,3)=-0.0255;	rof8(1,4)=93.4373;	rof8(2,4)=-175.426
	rof8(1,5)=41.786;	rof8(2,5)=-60.01;	rof8(1,6)=21.426;	rof8(2,6)=-247.703
	rof8(1,7)=1020.98;	rof8(2,7)=-643.243;rof8(1,8)=17.7989;	rof8(2,8)=-4.9994
	rof8(1,9)=176.771;	rof8(2,9)=-8.1629;	rof8(1,10)=1003.82;rof8(2,10)=-101.242
	rof8(1,11)=29.4406;rof8(2,11)=-10.362;rof8(1,12)=279.826;rof8(2,12)=-40.072498
	rof8(1,13)=152.0106;rof8(2,13)=-61.744;rof8(1,14)=149.302;rof8(2,14)=-179.534
	rof8(1,15)=310.436;rof8(2,15)=-201.99;rof8(1,16)=115.7;	rof8(2,16)=-123.8485
	rof8(1,17)=9.9265;	rof8(2,17)=-10.544;rof8(1,18)=8587.8;	rof8(2,18)=-520.34
	rof8(1,19)=871.449;rof8(2,19)=-256.422;rof8(1,20)=15821.52;rof8(2,20)=-1941.7253
	rof8(1,21)=2153.1013;rof8(2,21)=-335.2381;	rof8(1,22)=592.454;rof8(2,22)=-215.2746
	
	rof9(1,1)=1.173E-3;rof9(2,1)=0;		rof9(1,2)=0.01332;	rof9(2,2)=0
	rof9(1,3)=1.522E-3;rof9(2,3)=-3.721E-23;rof9(1,4)=0.01277;rof9(2,4)=-2.523E-23
	rof9(1,5)=1.345E-4;rof9(2,5)=-3.571E-5;rof9(1,6)=4.014E-4;rof9(2,6)=-2.386E-5
	rof9(1,7)=2.03E-4;	rof9(2,7)=-3.655E-6;rof9(1,8)=1.119E-4;rof9(2,8)=-1.602E-5
	rof9(1,9)=9.093E-5;rof9(2,9)=0;		rof9(1,10)=1.137E-4;rof9(2,10)=0
	rof9(1,11)=1.91E-4;rof9(2,11)=0;		rof9(1,12)=0.01275;rof9(2,12)=0
	rof9(1,13)=8.396E-4;rof9(2,13)=0;	rof9(1,14)=5.768E-6;rof9(2,14)=0
	rof9(1,15)=4.08E-5;rof9(2,15)=0;		rof9(1,16)=0		;rof9(2,16)=-2.103E-4
	rof9(1,17)=1.593E-4;rof9(2,17)=0;	rof9(1,18)=2.103E-4;rof9(2,18)=-1.593E-4
	
	OPEN(UNIT=32,FILE='Test_Results\test.err',STATUS='REPLACE')
	OPEN(UNIT=10,FILE='CLASS_benchmark.of1',STATUS='OLD',ERR=1001)
	OPEN(UNIT=31,FILE='Test_Results\CLASS_Reasonable_failures.txt',STATUS='REPLACE')
	
	
	DO row = 1, 3
		READ(10,*) temp
	END DO
	DO
		READ(10,*,END=100) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15)
		DO col = 3, 15
			IF ((c(col) > rof1(1,(col-2))) .OR. (c(col) < rof1(2,(col-2)))) THEN
				WRITE(31,*)'Test failed in row ', row,  ', column ', &
					col, ' of the file "CLASS_benchmark.of1"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	100 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASS_benchmark.of1"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of1".' , &
			' Check CLASS_Reasonable_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (10)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=11, FILE='CLASS_benchmark.of2', STATUS='OLD',ERR=1001)
	ALLOCATE(c(16))
	
	DO row = 1, 3
		READ(11,*) temp
	END DO
	DO
		READ(11,*,END=101) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16)
		DO col = 3, 16
			IF ((c(col) > rof2(1,(col-2))) .OR. (c(col) < rof2(2,(col-2)))) THEN
				WRITE(31,*)'Test failed in row ', row ,', column ', &
					col, ' of the file "CLASS_benchmark.of2"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	101 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASS_benchmark.of2"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of2".' , &
			' Check CLASS_Reasonable_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (11)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=12, FILE='CLASS_benchmark.of3', STATUS='OLD',ERR=1001)
	ALLOCATE(c(10))
	
	DO row = 1, 3
		READ(12,*) temp
	END DO
	DO
		READ(12,*,END=102) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		DO col = 3, 10
			IF ((c(col) > rof3(1,(col-2))) .OR. (c(col) < rof3(2,(col-2)))) THEN
				WRITE(31,*)'Test failed in row ', row,  ', column ', &
					col, 'of the file "CLASS_benchmark.of3"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	102 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASS_benchmark.of3"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of3".' , &
			' Check CLASS_Reasonable_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (12)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=13, FILE='CLASS_benchmark.of4', STATUS='OLD',ERR=1001)
	ALLOCATE(c(18))
	
	DO row = 1, 3
		READ(13,*) temp
	END DO
	DO
		READ(13,*,END=103) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18)
		DO col = 5, 18
			IF ((c(col) > rof4(1,(col-4))) .OR. (c(col) < rof4(2,(col-4)))) THEN
				WRITE(31,*)'Test failed in row ', row, ', column ', &
					col, ' of the file "CLASS_benchmark.of4"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	103 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASS_benchmark.of4"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of4".' , &
			' Check CLASS_Reasonable_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (13)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=14, FILE='CLASS_benchmark.of5', STATUS='OLD',ERR=1001)
	ALLOCATE(c(19))
	
	DO row = 1, 3
		READ(14,*) temp
	END DO
	DO
		READ(14,*,END=104) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19)
		DO col = 5, 19
			IF ((c(col) > rof5(1,(col-4))) .OR. (c(col) < rof5(2,(col-4)))) THEN
				WRITE(31,*)'Test failed in row', row, ', column ', &
					col, ' of the file "CLASS_benchmark.of5"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	104 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASS_benchmark.of5"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of5".' , &
			' Check CLASS_Reasonable_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (14)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=15, FILE='CLASS_benchmark.of6', STATUS='OLD',ERR=1001)
	ALLOCATE(c(10))
	
	DO row = 1, 3
		READ(15,*) temp
	END DO
	DO
		READ(15,*,END=105) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9),c(10)
		DO col = 4, 10
			IF ((c(col) > rof6(1,(col-3))) .OR. (c(col) < rof6(2,(col-3)))) THEN
				WRITE(31,*)'Test failed in row', row, ', column ', &
					col, ' of the file "CLASS_benchmark.of6"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	105 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASS_benchmark.of6"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of6".' , &
			' Check CLASS_Reasonable_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (15)
	DEALLOCATE(c)
	fail = 0
	
!	OPEN(UNIT=16, FILE='CLASS_benchmark.of7', STATUS='OLD',ERR=1001)
!	ALLOCATE(c(12))
!	
!	DO row = 1, 3
!		READ(16,*) temp
!	END DO
!	DO
!		READ(16,*,END=106) c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
!						c(10),c(11),c(12)
!		DO col = 4, 12
!			IF ((c(col) > rof7(1,(col-4))) .OR. (c(col) < rof7(2,(col-4)))) THEN
!				WRITE(31,*)'Test failed in row', row, ', column ', &
!					col, ' of the file "CLASS_benchmark.of7"'
!				fail = fail + 1
!			END IF
!		END DO
!		row = row + 1
!	END DO
!	106 CONTINUE
!	IF (fail == 0)THEN
!		WRITE(32,*)'Reasonable results in file "CLASS_benchmark.of7"'
!	ELSE
!		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of7".' , &
!			' Check CLASS_Reasonable_failures.txt for details'
!			ok = .false.
!	END IF
!	
!	CLOSE (16)
!	DEALLOCATE(c)
!	fail = 0
	
	OPEN(UNIT=17, FILE='CLASS_benchmark.of8', STATUS='OLD',ERR=1001)
	ALLOCATE(c(22))
	
	DO row = 1, 3
		READ(17,*) temp
	END DO
	DO
		READ(17,*,END=107)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17), &
						c(18),c(19),c(20),c(21),c(22)
		DO col = 1, 22
			IF ((c(col) > rof8(1,(col))) .OR. (c(col) < rof8(2,(col)))) THEN
				WRITE(31,*)'Test failed in row', row, ', column ', &
					col, ' of the file "CLASS_benchmark.of8"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	107 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASS_benchmark.of8"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of8".' , &
			' Check CLASS_Reasonable_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (17)
	DEALLOCATE(c)
	fail = 0
	
	OPEN(UNIT=18, FILE='CLASS_benchmark.of9', STATUS='OLD',ERR=1001)
	ALLOCATE(c(18))
	
	DO row = 1, 3
		READ(18,*) temp
	END DO
	DO
		READ(18,*,END=108)  c(1),c(2),c(3),c(4),c(5),c(6),c(7),c(8),c(9), & 
						c(10),c(11),c(12),c(13),c(14),c(15),c(16),c(17),c(18)
		DO col = 1, 18
			IF ((c(col) > rof9(1,(col))) .OR. (c(col) < rof9(2,(col)))) THEN
				WRITE(31,*)'Test failed in row', row, ', column ', &
					col, ' of the file "CLASS_benchmark.of9"'
				fail = fail + 1
			END IF
		END DO
		row = row + 1
	END DO
	108 CONTINUE
	IF (fail == 0)THEN
		WRITE(32,*)'Reasonable results in file "CLASS_benchmark.of9"'
	ELSE
		WRITE(32,*)'Test failed ', fail, 'times for file "CLASS_benchmark.of9".' , &
			' Check CLASS_Reasonable_failures.txt for details'
			ok = .false.
	END IF
	
	CLOSE (17)
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
END PROGRAM CLASS_Reasonable_Results