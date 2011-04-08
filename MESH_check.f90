PROGRAM MESH_check
!>
!>	March 9, 2011 - B. Schweitzer
!>
!>=============================================================================
!>
!>	This program tests to see whether MESH has the proper dependent files before running
!>	and if the files dont exist it creates them
!>
!>=============================================================================
!>
!>	chk1-11	-checks each row to see if each respective file exists or not
!>
!>=============================================================================
	CHARACTER :: chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9, &
				chk10,chk11
	CHARACTER(len=22):: flag
	CHARACTER(len=6):: min,max
	CHARACTER(len=15):: nm,filnam
	CHARACTER(len=10):: tmp1,tmp2,tmp3
	CHARACTER(len=20):: clssin,fname,lname
	INTEGER :: num, flgval, inc, grdnum(5),GRU(5),opt,y,m,d,mi,GRUnum,GHP
	INTEGER :: i,j,k,row=1,clss,i1,i2,i3,i4,xc,yc,hr,frmt
	integer*4 date_time(8)
	REAL :: crr1,crr2,crr3,crr4,crr5,input,IHP,sl1,sl2,c1,c2,c3,c4,c5,c6,c7,c8,c9
	REAL, DIMENSION(:,:),ALLOCATABLE :: arr
	INTEGER, DIMENSION(:,:),ALLOCATABLE:: arri
	OPEN(UNIT=1,FILE='test.ini',STATUS='OLD')
	
	READ(1,*)chk1
	!check if the user has specified that the first file(drainage database)exists or not
	IF(chk1 == 'n')THEN
		OPEN(10,FILE='MESH_drainage_database.r2c',STATUS='NEW')
		!Write header
		WRITE(10,'(A)')'########################################'
		WRITE(10,'(A)')':FileType r2c  ASCII  EnSim 1.0         '
		WRITE(10,'(A)')'#                                       '
		WRITE(10,'(A)')'# DataType               2D Rect Cell   '
		WRITE(10,'(A)')'#                                       '
		WRITE(10,'(A)')':Application             EnSimHydrologic'
		WRITE(10,'(A)')':Version                 2.1.23         '
		READ(1,*)nm
		WRITE(10,'(2A)')':WrittenBy          ',nm
		call date_and_time(tmp1, tmp2, tmp3, date_time)
		WRITE(10,FMT=9000)':CreationDate       ', date_time(1),'-', & 
			date_time(2),'-',date_time(3),'  ',date_time(5),':', &
			date_time(6)
		WRITE(10,'(A)')'#                                       '
		WRITE(10,'(A)')'#---------------------------------------'
		READ(1,'(A)')filnam
		WRITE(10,'(2A)')':SourceFileName               ',filnam
		READ(1,*)input
		WRITE(10,'(A,F13.3)')':NominalGridSize_AL', input
		READ(1,*)input
		WRITE(10,'(A,F8.3)')':ContourInterval        ',input
		READ(1,*)input
		WRITE(10,'(A,F8.3)')':ImperviousArea         ',input
		READ(1,*)clss
		WRITE(10,'(A,I8)')':ClassCount             ',clss
		READ(1,*)num
		WRITE(10,'(A,I8)')':NumRiverClasses        ',num
		READ(1,*)input
		WRITE(10,'(A,F8.3)')':ElevConversion         ',input
		READ(1,*)num
		WRITE(10,'(A,I8)')':TotalNumOfGrids        ',num
		READ(1,*)num
		WRITE(10,'(A,I8)')':numGridsInBasin        ',num
		READ(1,*)num
		WRITE(10,'(A,I8)')':DebugGridNo            ',num
		WRITE(10,'(A)')'#                                       '
		WRITE(10,'(A)')'#                                       '
		READ(1,*)nm
		WRITE(10,'(2A)')':Projection         ',nm
		IF(nm=='UTM')THEN
			READ(1,*)nm
			WRITE(10,'(2A)')':Zone               ',nm
		END IF
		READ(1,*)nm
		WRITE(10,'(2A)')':Ellipsoid          ',nm
		WRITE(10,'(A)')'#                                       '
		READ(1,*)input
		WRITE(10,'(A,F14.6)')':xOrigin             ',input
		READ(1,*)input
		WRITE(10,'(A,F14.6)')':yOrigin             ',input
		WRITE(10,'(A)')':AttributeName 1 Rank         '
		WRITE(10,'(A)')':AttributeName 2 Next         '
		WRITE(10,'(A)')':AttributeName 3 DA           '
		WRITE(10,'(A)')':AttributeName 4 Bankfull     '
		WRITE(10,'(A)')':AttributeName 5 ChnlSlope    '
		WRITE(10,'(A)')':AttributeName 6 Elev         '
		WRITE(10,'(A)')':AttributeName 7 ChnlLength   '
		WRITE(10,'(A)')':AttributeName 8 IAK          '
		WRITE(10,'(A)')':AttributeName 9 IntSlope     '
		WRITE(10,'(A)')':AttributeName 10 Chnl        '
		WRITE(10,'(A)')':AttributeName 11 Reach       '
		WRITE(10,'(A)')':AttributeName 12 GridArea    '
		DO inc=1, clss
			WRITE(10,'(A,I3,A,I3,A)')':AttributeName', inc+12,' Class', inc,'    '
		END DO
		WRITE(10,'(A)')'#                                       '
		READ(1,*)xc
		WRITE(10,'(A,I8)')':xCount                 ',xc
		READ(1,*)yc
		WRITE(10,'(A,I8)')':yCount                 ',yc
		READ(1,*)input
		WRITE(10,'(A,F12.6)')':xDelta                 ',input
		READ(1,*)input
		WRITE(10,'(A,F12.6)')':yDelta                 ',input
		WRITE(10,'(A)')'#                                       '
		WRITE(10,'(A)')':EndHeader                              '
		ALLOCATE(arri(yc,xc),arr(yc,xc))
		DO inc = 1,12+clss
			IF(inc==1 .OR. inc==2 .OR. inc==8 .OR. inc==10 .OR. inc==11)THEN
				DO i = 1,yc
					DO j=1,xc
						READ(1,'(I5)',ADVANCE='NO')arri(i,j)
						WRITE(10,'(I5)',ADVANCE='NO')arri(i,j)
					END DO
					READ(1,*)
					WRITE(10,*)
				END DO
			END IF
			IF(inc==3 .OR. inc==12)THEN
				DO i = 1,yc
					DO j=1,xc
						READ(1,'(E13.7)',ADVANCE='NO')arr(i,j)
						WRITE(10,'(1x,E12.7)',ADVANCE='NO')arr(i,j)
					END DO
					READ(1,*)
					WRITE(10,*)
				END DO
			END IF
			IF(inc==4)THEN
				DO i = 1,yc
					DO j=1,xc
						READ(1,'(F11.3)',ADVANCE='NO')arr(i,j)
						WRITE(10,'(F11.3)',ADVANCE='NO')arr(i,j)
					END DO
					READ(1,*)
					WRITE(10,*)
				END DO
			END IF
			IF(inc==13 .OR. inc==14 .OR. inc==15 .OR. inc==16 &
				.OR. inc==16.OR. inc==16)THEN
				DO i = 1,yc
					DO j=1,xc
						READ(1,'(F9.3)',ADVANCE='NO')arr(i,j)
						WRITE(10,'(F9.3)',ADVANCE='NO')arr(i,j)
					END DO
					READ(1,*)
					WRITE(10,*)
				END DO
			END IF
			IF(inc==5 .OR. inc==9)THEN
				DO i = 1,yc
					DO j=1,xc
						READ(1,'(F11.7)',ADVANCE='NO')arr(i,j)
						WRITE(10,'(F11.7)',ADVANCE='NO')arr(i,j)
					END DO
					READ(1,*)
					WRITE(10,*)
				END DO
			END IF
			IF(inc==6)THEN
				DO i = 1,yc
					DO j=1,xc
						READ(1,'(F9.1)',ADVANCE='NO')arr(i,j)
						WRITE(10,'(F9.1)',ADVANCE='NO')arr(i,j)
					END DO
					READ(1,*)
					WRITE(10,*)
				END DO
			END IF
			IF(inc==7)THEN
				DO i = 1,yc
					DO j=1,xc
						READ(1,'(F9.0)',ADVANCE='NO')arr(i,j)
						WRITE(10,'(F9.0)',ADVANCE='NO')arr(i,j)
					END DO
					READ(1,*)
					WRITE(10,*)
				END DO
			END IF
		END DO
		CLOSE(10)
	END IF
	READ(1,*)chk2
	IF(chk2 == 'n')THEN
		OPEN(20,FILE='MESH_input_reservoir.txt',STATUS='NEW')
		READ(1,*)num
		DO inc=1,num
			READ(1,*)i,j,k
			WRITE(20,'(3I5)')i,j,k
		END DO
		CLOSE(20)
	END IF
	READ(1,*)chk3
	IF(chk3 == 'n')THEN
		OPEN(30,FILE='MESH_input_run_options.ini',STATUS='NEW')
		WRITE(30,'(A)')'MESH Run Options input file'
		WRITE(30,'(A)')'##### Control Flags #####'
		WRITE(30,'(A)')'----#'
		READ(1,*)num
		WRITE(30,'(I5,A)')num, ' # Number of Control Flags'
		DO inc = 1,num
			READ(1,*)flag,flgval
			WRITE(30,'(A22,I2)')flag,flgval
		END DO
		WRITE(30,'(A)')'##### Output Grid Selection #####'
		WRITE(30,'(A)')'----#'
		READ(1,*)num
		WRITE(30,'(I5,A)')num,' # Number of Grid Output Points'
		DO inc = 1, num
			READ(1,*)grdnum(inc),GRU(inc)
		END DO
		IF(num == 1)THEN
			WRITE(30,'(A)')'---------#'
			WRITE(30,'(I10,A)')grdnum(1),' # Grid Number'
			WRITE(30,'(I10,A)')GRU(1),' # GRU'
			WRITE(30,'(A10,A)')'CLASSOUT1','  # Output Directory'
		END IF
		IF(num == 2)THEN
			WRITE(30,'(A)')'---------#---------#'
			WRITE(30,'(2I10,A)')grdnum(1), grdnum(2),' # Grid Number'
			WRITE(30,'(2I10,A)')GRU(1), GRU(2),' # GRU'
			WRITE(30,'(2A10,A)')'CLASSOUT1','CLASSOUT2','  # Output Directory'
		END IF
		IF(num == 3)THEN
			WRITE(30,'(A)')'---------#---------#---------#'
			WRITE(30,'(3I10,A)')grdnum(1), grdnum(2),grdnum(3),' # Grid Number'
			WRITE(30,'(3I10,A)')GRU(1), GRU(2), GRU(3),' # GRU'
			WRITE(30,'(3A10,A)')'CLASSOUT1','CLASSOUT2','CLASSOUT3', &
				'  # Output Directory'
		END IF
		IF(num == 4)THEN
			WRITE(30,'(A)')'---------#---------#---------#---------#'
			WRITE(30,'(4I10,A)')grdnum(1), grdnum(2),grdnum(3),grdnum(4), &
				' # Grid Number'
			WRITE(30,'(4I10,A)')GRU(1), GRU(2), GRU(3),GRU(4),' # GRU'
			WRITE(30,'(4A10,A)')'CLASSOUT1','CLASSOUT2','CLASSOUT3','CLASSOUT4', &
				'  # Output Directory'
		END IF
		IF(num == 5)THEN
			WRITE(30,'(A)')'---------#---------#---------#---------#---------#'
			WRITE(30,'(5I10,A)')grdnum(1), grdnum(2),grdnum(3),grdnum(4), &
				grdnum(5),' # Grid Number'
			WRITE(30,'(5I10,A)')GRU(1), GRU(2), GRU(3),GRU(4),GRU(5),' # GRU'
			WRITE(30,'(4A10,A)')'CLASSOUT1','CLASSOUT2','CLASSOUT3','CLASSOUT4', &
				'CLASSOUT5','  # Output Directory'
		END IF
		WRITE(30,'(A)')'##### Simulation Output Directory #####'
		WRITE(30,'(A)')'---------#'
		WRITE(30,'(A)')'BASINAVG1  # Simulation Output Directory'
		WRITE(30,'(A)')'##### Simulation Run Times #####'
		WRITE(30,'(A)')'---#---#---#---#'
		READ(1,*)y,d,hr,mi
		WRITE(30,'(4I4,A)')y,d,hr,mi,' # Sim. Start Time (Year, Julian Day, Hour, and Minute)'
		READ(1,*)y,d,hr,mi
		WRITE(30,'(4I4,A)')y,d,hr,mi,' # Sim. Stop Time (Year, Julian Day, Hour, and Minute)'
		CLOSE(30)
	END IF
	READ(1,*)chk4
	IF(chk4 == 'n')THEN
		OPEN(40,FILE='MESH_input_soil_levels.txt',STATUS='NEW')
		READ(1,*)num
		DO inc=1,num
			READ(1,*)sl1,sl2
			WRITE(40,'(F10.2,F8.2)')sl1,sl2
		END DO
		CLOSE(40)
	END IF
	READ(1,*)chk5
	IF(chk5 == 'n')THEN
		OPEN(50,FILE='MESH_input_streamflow.txt',STATUS='NEW')
		READ(1,*)num
		WRITE(50,'(I1)')num
		READ(1,*)i1,i2,i3,i4
		WRITE(50,'(I5,10x,3I5,A)')i1,i2,i3,i4,'   00'
		DO inc=1,i1
			READ(1,'(A)')clssin
			WRITE(50,'(A)')clssin
		END DO
		READ(1,*)num
		DO inc=1,num
			READ(1,*)c1
			WRITE(50,'(F8.3)')c1
		END DO
		CLOSE(50)
	END IF
	READ(1,*)chk6
	IF(chk6 == 'n')THEN
		OPEN(60,FILE='MESH_parameters_CLASS.ini',STATUS='NEW')
		READ(1,*)clssin
		WRITE(60,'(2x,A)')clssin
		READ(1,*)num
		DO inc =1, num
			READ(1,*)fname,lname
			WRITE(60,'(2x,A,1x,A)', ADVANCE='NO')TRIM(fname),TRIM(lname)
			IF (num > 1 .AND. num /= inc)THEN
				WRITE(60,'(A)',ADVANCE='NO')','
			END IF
		END DO
		WRITE(60,*)
		READ(1,*)clssin
		WRITE(60,'(2x,A)')clssin
		READ(1,*)c1,c2,c3,c4,c5,c6,i1,i2,i3
		WRITE(60,'(5F10.2,F7.1,3I5)')c1,c2,c3,c4,c5,c6,i1,i2,i3
		i = row
		READ(1,*)num
		DO inc =1, num
			row = i
			READ(1,*)c1,c2,c3,c4,c5,c6,c7,c8,c9
			WRITE(60,'(9F8.3,3x,A)')c1,c2,c3,c4,c5,c6,c7,c8, &
				c9,'05 Land class type/fcanrow/lamxrow'
			READ(1,*)c1,c2,c3,c4,c5,c6,c7,c8,c9
			WRITE(60,'(9F8.3,3x,A)')c1,c2,c3,c4,c5,c6,c7,c8, &
				c9,'06 lnz0row/lamnrow'
			READ(1,*)c1,c2,c3,c4,c5,c6,c7,c8,c9
			WRITE(60,'(9F8.3,3x,A)')c1,c2,c3,c4,c5,c6,c7,c8, &
				c9,'07 alvcrow/cmasrow'
			READ(1,*)c1,c2,c3,c4,c5,c6,c7,c8,c9
			WRITE(60,'(9F8.3,3x,A)')c1,c2,c3,c4,c5,c6,c7,c8, &
				c9,'08 alirow/rootrow'
			READ(1,*)c1,c2,c3,c4,c5,c6,c7,c8
			WRITE(60,'(4F8.3,8x,4F8.3,3x,A)')c1,c2,c3,c4,c5,c6,c7, &
				c8,'09 rsmnrow/qa50row'
			READ(1,*)c1,c2,c3,c4,c5,c6,c7,c8
			WRITE(60,'(4F8.3,8x,4F8.3,3x,A)')c1,c2,c3,c4,c5,c6,c7, &
				c8,'10 vpdarow/vpdprow'
			READ(1,*)c1,c2,c3,c4,c5,c6,c7,c8
			WRITE(60,'(4F8.3,8x,4F8.3,3x,A)')c1,c2,c3,c4,c5,c6,c7, &
				c8,'11 psgarow/psgbrow'
			READ(1,*)c1,c2,c3,c4
			WRITE(60,'(4F8.3,43x,A)')c1,c2,c3, &
				c4,'12 drnrow/sdeprow/farerow/ddenrow'
			READ(1,*)c1,c2,c3,c4,i
			WRITE(60,'(4F8.3,I8,35x,A)')c1,c2,c3,c4, &
				i, '13 xslprow/grkfrow/manrow/WFCIROW/midrow'
			READ(1,*)c1,c2,c3
			WRITE(60,'(3F10.2,45x,A)')c1,c2,c3,'14 sand'
			READ(1,*)c1,c2,c3
			WRITE(60,'(3F10.2,45x,A)')c1,c2,c3,'15 clay'
			READ(1,*)c1,c2,c3
			WRITE(60,'(3F10.2,45x,A)')c1,c2,c3,'16 org'
			READ(1,*)c1,c2,c3,c4,c5,c6
			WRITE(60,'(6F10.2,15x,A)')c1,c2,c3,c4,c5, &
				c6,'17 temperature-soil/can/sno/pnd'
			READ(1,*)c1,c2,c3,c4,c5,c6,c7
			WRITE(60,'(7F10.3,5x,A)')c1,c2,c3,c4,c5,c6, &
				c7,'18 soil moisture-soil/ice/pnd'
			READ(1,*)c1,c2,c3,c4,c5,c6
			WRITE(60,'(6F10.3,15x,A)')c1,c2,c3,c4,c5, &
				c6,'19 rcan/scan/sno/albs/rho/gro'
		END DO
		READ(1,*)i1,i2,i3,i4
		WRITE(60,'(4I10,35x,A)')i1,i2,i3,i4,'20'
		READ(1,*)i1,i2,i3,i4
		WRITE(60,'(4I10,35x,A)')i1,i2,i3,i4,'21'
		READ(1,*)i1,i2,i3,i4
		WRITE(60,'(4I10,35x,A)')i1,i2,i3,i4,'22'
		WRITE(60,'(A)')'1234567890123456789012345678901234567890123456789012345678901234567890123'
		CLOSE(60)
	END IF
	READ(1,*)chk7
	IF(chk7 == 'n')THEN
		OPEN(70,FILE='MESH_parameters_hydrology.ini',STATUS='NEW')
		WRITE(70,'(A)')'1.1.a04: MESH Hydrology Parameters input file'
		WRITE(70,'(A)')'##### Option Flags #####'
		WRITE(70,'(A)')'----#'
		READ(1,*)num
		WRITE(70,'(I5,A)')num,' # Number of Option Flags'
		DO inc=1, num
			READ(1,*)opt
			WRITE(70,'(I5,A,I1,A,I1,A)')opt,' #', inc, ' [reserved #', inc, ']'
		END DO
		WRITE(70,'(A)')'##### Channel River Roughness Factors (WF_R2) #####'
		WRITE(70,'(A)')'-----#-----#-----#-----#-----#'
		READ(1,*)crr1,crr2,crr3,crr4,crr5
		WRITE(70,'(5F6.3)')crr1,crr2,crr3,crr4,crr5
		WRITE(70,'(A)')'##### GRU Independent Hydrologic Parameters #####'
		WRITE(70,'(A)')'-------#'
		READ(1,*)num
		WRITE(70,'(I8,A)')num,' # Number of GRU-Independent Hydrologic Parameters'
		DO inc=1,num
			READ(1,*)IHP
			WRITE(70,'(F8.3,A,I1,A,I1,A)')IHP, ' #',inc, '[reserved #', inc, ']'
		END DO
		WRITE(70,'(A)')'##### GRU Dependent Hydrologic Parameters #####'
		WRITE(70,'(A)')'-------#'
		READ(1,*)GRUnum,GHP
		WRITE(70,'(I8,A)')GRUnum, ' # Number of GRUs'
		WRITE(70,'(I8,A)')GHP,' # Number of GRU-Dependent Hydrologic Parameters'
		IF(GRUnum == 1)THEN
			WRITE(70,'(A)')'---------#'
		END IF
		IF(GRUnum == 2)THEN
			WRITE(70,'(A)')'---------#---------#'
		END IF
		IF(GRUnum == 3)THEN
			WRITE(70,'(A)')'---------#---------#---------#'
		END IF
		IF(GRUnum == 4)THEN
			WRITE(70,'(A)')'---------#---------#---------#---------#'
		END IF
		IF(GRUnum == 5)THEN
			WRITE(70,'(A)')'---------#---------#---------#---------#---------#'
		END IF
		DO i=1,GHP
			DO j=1,GRUnum
				READ(1,'(F10.3)',ADVANCE='NO')input
				WRITE(70,'(F10.3)',ADVANCE='NO')input
			END DO
			READ(1,*)
			WRITE(70,*)
		END DO
		CLOSE(70)
	END IF
	READ(1,*)chk8
	IF(chk8 == 'n')THEN
		OPEN(80,FILE='minmax_parameters.txt',STATUS='NEW')
		WRITE(80,'(A,I2)')'Reserved 1 - THEXTRA                               !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(2A)')min,'                                             !min'
		READ(1,'(A)')max
		WRITE(80,'(2A)')max,'                                             !max'
		WRITE(80,'(A,I2)')'Reserved 2 - ICE_INDEX                             !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(2A)')min,'                                             !min'
		READ(1,'(A)')max
		WRITE(80,'(2A)')max,'                                             !max'
		WRITE(80,'(A,I2)')'Reserved 3 - GWSCALE                               !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(2A)')min,'                                             !min'
		READ(1,'(A)')max
		WRITE(80,'(2A)')max,'                                             !max'
		READ(1,*)num
		DO inc=1,num
			IF(inc==1)THEN
				WRITE(80,'(A,I2)')'River roughness factor (WF_R2) (5 classes maximum) !ROW ', row
				row = row +1
				READ(1,'(A)')min
				WRITE(80,'(2A)')min,'                                             !min'
				READ(1,'(A)')max
				WRITE(80,'(2A)')max,'                                             !max'
				CYCLE
			END IF
			WRITE(80,'(A,I1,A,I2)')'WF_R2 - CLASS ',inc,'                                    !ROW ', row
			row = row +1
			READ(1,'(A)')min
			WRITE(80,'(2A)')min,'                                             !min'
			READ(1,'(A)')max
			WRITE(80,'(2A)')max,'                                             !max'
		END DO
		WRITE(80,'(A,I2)')'Reserved 4 - Precipitation adjustment              !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(3A)')min,'                                             !min' , &
			' precip_rat - Amount of precipitation adjustment - Not Used'
		READ(1,'(A)')max
		WRITE(80,'(2A)')max,'                                             !max'
		WRITE(80,'(A,I2)')'Reserved 5 - Temperature adjustment                !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(3A)')min,'                                             !min' , &
			' temper_inc - Amount of temperature adjustment - Not Used'
		READ(1,'(A)')max
		WRITE(80,'(2A)')max,'                                             !max'
		WRITE(80,'(2A,I2)')'DRNROW - DRAINAGE INDEX, CALCULATED DRAINAGE', &
			' IS MULTIPLIED BY THIS VALUE                      !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min DRNROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'SDEPROW - THE PERMEABLE DEPTH OF THE SOIL COLUMN', &                                              !ROW 12'
			'                                              !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min SDEPROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'FAREROW - WHEN RUNNING A MOSAIC, THE FRACTIONAL', &
			' AREA THAT THIS TILE REPRESENTS IN A GRID CELL !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min FAREROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'DDENROW - THE DRAINAGE DENSITY OF THE', &
			' GRU IN KM/KM2                                           !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min DDENROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'XSLPROW - AVERAGE OVERLAND SLOPE OF A GIVEN GRU', &
			'                                               !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min XSLPROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'XDNROW - HORIZONTAL CONDUCTIVITY AT A DEPTH OF', &
			' h0 DIVIDED BY HORIZONTAL CONDUCTIVITY AT SURFACE !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min XDNROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'MANNROW - MANNING ROUGHNESS COEFFICIENT', &
			'                                                       !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min MANNROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'KSROW - HORIZONTAL CONDUCTIVITY AT SURFACE', &
			'                                                    !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min KSROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'SANDROW - PERCENTAGES OF SAND CONTENT OF SOIL', &
			' LAYER 1                                         !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min % OF SAND not organic in soil layer 1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'CLAYROW - PERCENTAGES OF CLAY CONTENT OF SOIL ', &
			'LAYER 1                                         !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min % OF CLAY not organic or sand in soil layer 1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ORGMROW - PERCENTAGES OF ORGANIC MATTER OF SOIL ', &
			'LAYER 1                                       !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min % OF ORGANIC in soil layer 1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'SANDROW - PERCENTAGES OF SAND CONTENT OF SOIL ', &
			'LAYER 2                                         !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min % OF SAND not organic in soil layer 2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'CLAYROW - PERCENTAGES OF CLAY CONTENT OF SOIL ', &
			'LAYER 2                                         !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min % OF CLAY not organic or sand in soil layer 2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ORGMROW - PERCENTAGES OF ORGANIC MATTER OF SOIL ', &
			'LAYER 2                                       !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min % OF ORGANIC in soil layer 2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'SANDROW - PERCENTAGES OF SAND CONTENT OF SOIL ', &
			'LAYER 3                                         !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min % OF SAND not organic in soil layer 3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'CLAYROW - PERCENTAGES OF CLAY CONTENT OF SOIL ', &
			'LAYER 3                                         !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min % OF CLAY not organic or sand in soil layer 3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ORGMROW - PERCENTAGES OF ORGANIC MATTER OF SOIL ', &
			'LAYER 3                                       !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, &
			min,min,min,min,'   !min % OF ORGANIC in soil layer 3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ZSNLROW - LIMITING SNOW DEPTH BELOW WHICH', &
			' COVERAGE IS < 100%                                  !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ZSNLROW    **From MESH_parameters_hydrology.ini'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ZPLSROW - MAXIMUM WATER PONDING DEPTH FOR', &
			' SNOW-COVERED AREAS                                  !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ZPLSROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ZPLGROW - MAXIMUM WATER PONDING DEPTH FOR', &
			' SNOW-FREE AREAS                                     !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ZPLGROW'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LNZ0ROW - NATURAL LOGARITHM OF THE', &
			' ROUGHNESS LENGTH FOR LAND COVER CATEGORY 1                 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LNZ0ROW1   Column 1 **Atmospheric parameters from MESH_parameters_CLASS.INI '
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALVCROW - VISIBLE ALBEDO FOR LAND COVER CATEGORY 1', &
			'                                            !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALVCROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALICROW - NEAR INFRARED ALBEDO FOR LAND COVER', &
			' CATEGORY 1                                      !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALICROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'RSMNROW - MINIMUM STOMATAL RESISTANCE FOR THE', &
			' VEGETATION TYPE 1                               !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min RSMNROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'VPDAROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO VAPOR PRESSURE DEFICIT - COMMON VALUE 0.5 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min VPDAROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'PSGAROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO SOIL WATER SUCTION -OMMON VALUE 100 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min PSGAROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LNZ0ROW - NATURAL LOGARITHM OF THE ROUGHNESS', &
			' LENGTH FOR LAND COVER CATEGORY 2                 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LNZ0ROW2   Column 2 **Atmospheric parameters from MESH_parameters_CLASS.INI '
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALVCROW - VISIBLE ALBEDO FOR LAND COVER CATEGORY 2', &
			'                                            !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALVCROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALICROW - NEAR INFRARED ALBEDO FOR LAND COVER', &
			' CATEGORY 2                                      !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALICROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'RSMNROW - MINIMUM STOMATAL RESISTANCE FOR THE', &
			' VEGETATION TYPE 2                               !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min RSMNROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'VPDAROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO VAPOR PRESSURE DEFICIT - COMMON VALUE 0.5 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min VPDAROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'PSGAROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO SOIL WATER SUCTION -OMMON VALUE 100 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min PSGAROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LNZ0ROW - NATURAL LOGARITHM OF THE ROUGHNESS', &
			' LENGTH FOR LAND COVER CATEGORY 3                 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LNZ0ROW3   Column 3 **Atmospheric parameters from MESH_parameters_CLASS.INI '
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALVCROW - VISIBLE ALBEDO FOR LAND COVER CATEGORY 3', &
			'                                            !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALVCROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALICROW - NEAR INFRARED ALBEDO FOR LAND COVER', &
			' CATEGORY 3                                      !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALICROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'RSMNROW - MINIMUM STOMATAL RESISTANCE FOR THE', &
			' VEGETATION TYPE 3                               !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min RSMNROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'VPDAROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO VAPOR PRESSURE DEFICIT - COMMON VALUE 0.5 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min VPDAROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'PSGAROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO SOIL WATER SUCTION -OMMON VALUE 100 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min PSGAROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LNZ0ROW - NATURAL LOGARITHM OF THE ROUGHNESS', &
			' LENGTH FOR LAND COVER CATEGORY 4                 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LNZ0ROW4   Column 4 **Atmospheric parameters from MESH_parameters_CLASS.INI '
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALVCROW - VISIBLE ALBEDO FOR LAND COVER CATEGORY 4', &
			'                                            !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALVCROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALICROW - NEAR INFRARED ALBEDO FOR LAND COVER', &
			' CATEGORY 4                                      !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALICROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'RSMNROW - MINIMUM STOMATAL RESISTANCE FOR THE', &
			' VEGETATION TYPE 4                               !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min RSMNROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'VPDAROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO VAPOR PRESSURE DEFICIT - COMMON VALUE 0.5 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min VPDAROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'PSGAROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO SOIL WATER SUCTION -OMMON VALUE 100 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min PSGAROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LNZ0ROW - NATURAL LOGARITHM OF THE ROUGHNESS', &
			' LENGTH FOR LAND COVER CATEGORY 5                 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LNZ0ROW5   Column 5 **Atmospheric parameters from MESH_parameters_CLASS.INI '
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALVCROW - VISIBLE ALBEDO FOR LAND COVER CATEGORY 5', &
			'                                            !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALVCROW5'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ALICROW - NEAR INFRARED ALBEDO FOR LAND COVER', &
			' CATEGORY 5                                      !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ALICROW5'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LAMXROW - MAXIMUM LEAF AREA INDEX FOR THE', &
			' VEGETATION TYPE 1                                   !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LAMXROW1   Column 6'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LAMNROW - MINIMUM LEAF AREA INDEX FOR THE', &
			' VEGETATION TYPE 1                                   !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LAMNROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'CMASROW - ANNUAL MAXIMUM CANOPY MASS FOR', &
			' VEGETATION TYPE 1 [kg m-2]                           !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min CMASROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ROOTROW - ROOTING DEPTH FOR VEGETATION TYPE 1', &
			'                                                 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ROOTROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'QA50ROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO LIGHT, COMMON VALUES - 30 TO 50 W/M2 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min QA50ROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'VPDBROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO VAPOR PRESSURE DEFICIT, COMMON VALUES - 1.0 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min VPDBROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'PSGBROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO SOIL WATER SUCTION, COMMON VALUES - 5 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min PSGBROW1'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LAMXROW - MAXIMUM LEAF AREA INDEX FOR THE', &
			' VEGETATION TYPE 2                                   !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LAMXROW2   Column 7'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LAMNROW - MINIMUM LEAF AREA INDEX FOR THE', &
			' VEGETATION TYPE 2                                   !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LAMNROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'CMASROW - ANNUAL MAXIMUM CANOPY MASS FOR', &
			' VEGETATION TYPE 2 [kg m-2]                           !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min CMASROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ROOTROW - ROOTING DEPTH FOR VEGETATION TYPE 2', &
			'                                                 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ROOTROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'QA50ROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO LIGHT, COMMON VALUES - 30 TO 50 W/M2 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min QA50ROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'VPDBROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO VAPOR PRESSURE DEFICIT, COMMON VALUES - 1.0 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min VPDBROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'PSGBROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO SOIL WATER SUCTION, COMMON VALUES - 5 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min PSGBROW2'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LAMXROW - MAXIMUM LEAF AREA INDEX FOR THE', &
			' VEGETATION TYPE 3                                   !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LAMXROW3   Column 8'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LAMNROW - MINIMUM LEAF AREA INDEX FOR THE', &
			' VEGETATION TYPE 3                                   !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LAMNROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'CMASROW - ANNUAL MAXIMUM CANOPY MASS FOR', &
			' VEGETATION TYPE 3 [kg m-2]                           !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min CMASROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ROOTROW - ROOTING DEPTH FOR VEGETATION TYPE 3', &
			'                                                 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ROOTROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'QA50ROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO LIGHT, COMMON VALUES - 30 TO 50 W/M2 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min QA50ROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'VPDBROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO VAPOR PRESSURE DEFICIT, COMMON VALUES - 1.0 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min VPDBROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'PSGBROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO SOIL WATER SUCTION, COMMON VALUES - 5 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min PSGBROW3'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LAMXROW - MAXIMUM LEAF AREA INDEX FOR THE', &
			' VEGETATION TYPE 4                                   !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LAMXROW4   Column 9'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'LAMNROW - MINIMUM LEAF AREA INDEX FOR THE', &
			' VEGETATION TYPE 4                                   !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min LAMNROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'CMASROW - ANNUAL MAXIMUM CANOPY MASS FOR', &
			' VEGETATION TYPE 4 [kg m-2]                           !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min CMASROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'ROOTROW - ROOTING DEPTH FOR VEGETATION TYPE 4', &
			'                                                 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min ROOTROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'QA50ROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO LIGHT, COMMON VALUES - 30 TO 50 W/M2 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min QA50ROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'VPDBROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO VAPOR PRESSURE DEFICIT, COMMON VALUES - 1.0 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min VPDBROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		WRITE(80,'(2A,I2)')'PSGBROW - COEFFICIENT GOVERNING THE RESPONSE OF', &
			' STOMATES TO SOIL WATER SUCTION, COMMON VALUES - 5 !ROW ', row
		row = row +1
		READ(1,'(A)')min
		WRITE(80,'(A,8A7,A)')min,min,min,min,min, min,min,min, &
			min,'   !min PSGBROW4'
		READ(1,'(A)')max
		WRITE(80,'(A,8A7,A)')max,max,max,max,max, &
			max,max,max,max,'   !max'
		CLOSE(80)
	END IF
	READ(1,*)chk9
	READ(1,*)chk10
	READ(1,*)chk11
	IF(chk9 == 'n' .AND. chk10 == 'n' .AND. chk11 == 'n')THEN
		OPEN(91,FILE='basin_humidity.csv',STATUS='NEW')
		OPEN(92,FILE='basin_longwave.csv',STATUS='NEW')
		OPEN(93,FILE='basin_pres.csv',STATUS='NEW')
		OPEN(94,FILE='basin_rain.csv',STATUS='NEW')
		OPEN(95,FILE='basin_shortwave.csv',STATUS='NEW')
		OPEN(96,FILE='basin_temperature.csv',STATUS='NEW')
		OPEN(97,FILE='basin_wind.csv',STATUS='NEW')
		DO inc = 91,97
			WRITE(inc,'(A)')'########################################'
			WRITE(inc,'(A)')':FileType r2c  ASCII  EnSim 1.0'
			WRITE(inc,'(A)')'#'
			WRITE(inc,'(A)')'# DataType               2D Rect Cell'
			WRITE(inc,'(A)')'#'
			WRITE(inc,'(A)')':Application             EnSimHydrologic'
			WRITE(inc,'(A)')':Version                 2.1.23'
			READ(1,*)nm
			WRITE(inc,'(2A)')':WrittenBy          ',nm
			call date_and_time(tmp1, tmp2, tmp3, date_time)
			WRITE(inc,FMT=9000)':CreationDate       ', date_time(1),'-', & 
				date_time(2),'-',date_time(3),'  ',date_time(5),':', &
				date_time(6)
			WRITE(inc,'(A)')'#'
			WRITE(inc,'(A)')'#---------------------------------------'
			WRITE(inc,'(A)')'#'
			WRITE(inc,'(A)')':Name               XXXXXXXXXXXXX'
			WRITE(inc,'(A)')'#'
			READ(1,*)nm
			WRITE(inc,'(2A)')':Projection         ',nm
			IF(nm=='UTM')THEN
				READ(1,*)nm
				WRITE(inc,'(2A)')':Zone               ',nm
			END IF
			READ(1,*)nm
			WRITE(inc,'(2A)')':Ellipsoid          ',nm
			WRITE(inc,'(A)')'#'
			READ(1,*)input
			WRITE(inc,'(A,F15.6)')':xOrigin            ',input
			READ(1,*)input
			WRITE(inc,'(A,F15.6)')':yOrigin            ',input
			WRITE(inc,'(A)')'#'
			READ(1,'(A)')fname
			WRITE(inc,'(2A)')':SourceFile         ',fname
			WRITE(inc,'(A)')'#'
			READ(1,*)num
			DO i = 1,num
				READ(1,*)nm
				WRITE(inc,'(A,I2,1x,A)')'  :AttributeName ',i,nm
				READ(1,'(A)')nm
				WRITE(inc,'(2A)')'  :AttributeUnits   ',nm
			END DO
			WRITE(inc,'(A)')'#'
			READ(1,*)xc
			WRITE(inc,'(A,I12)')':xCount             ',xc
			READ(1,*)yc
			WRITE(inc,'(A,I12)')':yCount             ',yc
			READ(1,*)input
			WRITE(inc,'(A,F12.5)')':xDelta             ',input
			READ(1,*)input
			WRITE(inc,'(A,F12.5)')':yDelta             ',input
			WRITE(inc,'(A)')'#'
			WRITE(inc,'(A)')'#'
			WRITE(inc,'(A)')':endHeader'
			READ(1,*)num
			READ(1,*)y,m,d
			hr=0;mi=0
			DO i = 1,num
				IF(mi >= 60)THEN
					hr = hr+1
					mi =0
					IF(hr >= 24)THEN
						d= d+1
						hr=0
						IF((d>31 .AND. (m==1 .OR. m==3 .OR. m==5 .OR. m==7 .OR. m==8 .OR. &
							m==10 .OR. m==12)).OR. (d>30 .AND.(m==4 .OR. m==6 .OR. m==9 .OR. &
							m==11)).OR. (d>28 .AND. m==2))THEN
							m=m+1
							d=1
							IF(m>12)THEN
								y=y+1
								m=1
							END IF
						END IF
					END IF
				END IF
				WRITE(inc,FMT=9001)':Frame ',i,i,'    "',y,'/',m,'/',d,hr, &
					':',mi,':00.000"'
				mi=mi+30
				IF(inc==91)THEN
					READ(1,*)input
					DO j=1,yc
						DO k=1,xc
							WRITE(inc,FMT=9100,ADVANCE='NO')input
						END DO
						WRITE(inc,*)
					END DO
				END IF
				IF(inc==92)THEN
					READ(1,*)input
					DO j=1,yc
						DO k=1,xc
							WRITE(inc,FMT=9200,ADVANCE='NO')input
						END DO
						WRITE(inc,*)
					END DO
				END IF
				IF(inc==93)THEN
					READ(1,*)input
					DO j=1,yc
						DO k=1,xc
							WRITE(inc,FMT=9300,ADVANCE='NO')input
						END DO
						WRITE(inc,*)
					END DO
				END IF
				IF(inc==94)THEN
					READ(1,*)input
					DO j=1,yc
						DO k=1,xc
							WRITE(inc,FMT=9400,ADVANCE='NO')input
						END DO
						WRITE(inc,*)
					END DO
				END IF
				IF(inc==95)THEN
					READ(1,*)input
					DO j=1,yc
						DO k=1,xc
							WRITE(inc,FMT=9500,ADVANCE='NO')input
						END DO
						WRITE(inc,*)
					END DO
				END IF
				IF(inc==96)THEN
					READ(1,*)input
					DO j=1,yc
						DO k=1,xc
							WRITE(inc,FMT=9600,ADVANCE='NO')input
						END DO
						WRITE(inc,*)
					END DO
				END IF
				IF(inc==97)THEN
					READ(1,*)input
					DO j=1,yc
						DO k=1,xc
							WRITE(inc,FMT=9700,ADVANCE='NO')input
						END DO
						WRITE(inc,*)
					END DO
				END IF
				WRITE(inc,'(A)')':EndFrame'
			END DO
		END DO
		CLOSE(91)
		CLOSE(92)
		CLOSE(93)
		CLOSE(94)
		CLOSE(95)
		CLOSE(96)
		CLOSE(97)
	END IF
	WRITE(*,*)chk1,chk2,chk3,chk4,chk5,chk6,chk7,chk8,chk9, &
				chk10,chk11
	CLOSE(1)
	
	9100 FORMAT (F7.5,3x)
	9200 FORMAT (F6.2,3x)
	9300 FORMAT (F7.1,3x)
	9400 FORMAT (F12.10,3x)
	9500 FORMAT (F4.2,3x)
	9600 FORMAT (F6.2,3x)
	9700 FORMAT (F6.4,3x)
	9000 FORMAT (A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2)
	9001 FORMAT (A,2I6,A,I4,A,I2.2,A,I2.2,1x,I2.2,A,I2.2,A)
END PROGRAM MESH_check
