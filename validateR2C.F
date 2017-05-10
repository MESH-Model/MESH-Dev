      PROGRAM validateR2C

	! Reads all time series R2C file(s) in the same directory and compares 
	! the timestamps against those in correct_times.txt. It prints out a 
	! file NAME_missing_values.txt. The file correct_times.txt is a tab 
	! separated ascii which can be created using Excel or similar. 
	! (TODO: Replace this with an actual date counter) 
	! If correct_times.txt is missing or incomplete the program operation will
	! not be hindered except for incorrect information in missing_values.txt and
	! is still useful for file concatentation/trimming
	!
	! The program will also write a new R2C file containing only dates between
	! those specified with frame numbers modified. 
	!
	! This program reads lines from the R2C as text and writes them as such, 
	! so no data is changed.
	!
	! If forcing data comes from CaPA the timestamps must be corrected or 
	! a time that is in the CaPA file used.
	!
	! Upon the first execution example files will be written, so just compile
	! and execute.
	!
	! PROGRAM USES:
	! 
	! FINDING MISSING VALUES 
	! The program will identify if times in correct_times.txt are missing from 
	! the R2C file(s) and print a report in the folder 'Missing_Values'. Users
	! should note the program does not infill missing values, it only alerts you
	! to their presence
	!
	! TRIMMING R2C TIMESERIES
	! MESH skips lines in forcing files based on times in input_run_options.ini
	! and MESH_parameters_CLASS.ini. This can take time. This program can be 
	! used to trim forcing files to only those times that are desired.
	!
	! REMOVE MULTIPLE HEADERS
	! If forcing/R2C files are recieved as multiple files much must be put 
	! together using a function like cat in Linux (i.e. cat file1 file2 > newfile)
	! there may be multiple headers. VALIDATER2C will detect and remove these
	! headers in the processed file.
	!
	! WARNING: Ensure that only time-series R2C files that you want processed
	!          are in the same directory with the executable. 

C     ***************************************************************************
C     * Original Program: by Kurt C. Kornelsen, McMaster University, Sept 2013  *
C     *                                                                         *
C     ***************************************************************************

	IMPLICIT NONE

C     DELCARATIONS **************************************************************
      INTEGER, PARAMETER:: linelen = 10000
      INTEGER, DIMENSION(:), ALLOCATABLE:: corr_year
	INTEGER, DIMENSION(:), ALLOCATABLE:: corr_month
	INTEGER, DIMENSION(:), ALLOCATABLE:: corr_day 
	INTEGER, DIMENSION(:), ALLOCATABLE:: corr_hour
	INTEGER, DIMENSION(:), ALLOCATABLE:: corr_minute ! Read from correct file
	INTEGER, DIMENSION(:), ALLOCATABLE:: year
	INTEGER, DIMENSION(:), ALLOCATABLE:: month
	INTEGER, DIMENSION(:), ALLOCATABLE:: day
	INTEGER, DIMENSION(:), ALLOCATABLE:: hour
	INTEGER, DIMENSION(:), ALLOCATABLE:: minute ! Read from R2C
	CHARACTER(100),DIMENSION(:),ALLOCATABLE:: FileNames
	INTEGER:: n, j, k, idx, len_line
	INTEGER:: n_corr, n_r2c, n_frame, n_files ! Counters
	INTEGER:: startYear, startMonth, startDay, startHour
	INTEGER:: stopYear, stopMonth, stopDay, stopHour
      CHARACTER(linelen):: line
      INTEGER:: endHeaderFlag, startCopyFlag
	INTEGER:: foundFlag, suffix_index, IOS
	CHARACTER(6):: dummy1
	INTEGER:: dummy2,dummy3
	CHARACTER(6):: fm
	CHARACTER(1):: sl,col,qu,und
      CHARACTER(8):: sec
      CHARACTER(4):: suffix
	CHARACTER(100):: OrigFile, Variable, MissingFile, NewFile
	CHARACTER(18):: miss
      CHARACTER(23):: timestamp
	CHARACTER(1):: delim
	CHARACTER(14)::missDir
	CHARACTER(13)::procDir
	LOGICAL:: secheadflag 
	
C *****************************************************************************
      
C     FIND THE R2C Files and get their names

      delim = '\' ! Uncomment for Windows
	!delim = '/'  ! Uncomment for Unix
	

      IF (delim.eq.'\') THEN ! System is on windows
	CALL SYSTEM('dir /b *.r2c > fileContents.txt')
	ELSEIF (delim.eq.'/') THEN ! System is on linux      
	CALL SYSTEM('ls *.r2c > fileContents.txt')
	ENDIF

	

	procDir = 'Processed_R2C'
	missDir = 'Missing_Values'

	CALL SYSTEM('mkdir '//  procDir)
	CALL SYSTEM('mkdir ' //missDir)

	OPEN(31,FILE = 'fileContents.txt')
	n_files = 0
	PRINT*, 'Finding File Names'
	
	DO
	READ(31,*,END = 12) 
	n_files = n_files + 1
	ENDDO
12    REWIND(31)

      PRINT*, 'There are ',n_files, ' file(s)'

      ALLOCATE(FileNames(n_files))
	DO n =1,n_files
	READ(31,'(A)') FileNames(n)
	ENDDO
	CLOSE(31,STATUS='DELETE')
		
C*****************************************************************************
C     DELCARE TEXT CONSTANTS
	fm = ':Frame'
	sl = '/'
	col = ':'
	qu = '"'
	sec = ':00.000"'
	suffix = '.r2c'
	und = '_'
	miss = 'missing_values.txt'

	
C *****************************************************************************
C     READ START AND STOP TIMES
      OPEN(77,File='times.txt',STATUS = 'OLD',IOSTAT=IOS)
	IF (IOS.ne.0) THEN
	PRINT*, 'ERROR! times.txt is a required file'
	PRINT*, 'This tells the program when to start and stop'
	PRINT*, 'the new R2C file'
	PRINT*, 'The format is : Header'
	PRINT*, 'Start : Year  Month  Day  Hour  '
	PRINT*, 'Stop : Year Month Day Hour '
	PRINT*, 'Generating an Example'
	
	OPEN(78,File ='times.txt',STATUS ='NEW')
	WRITE(78,'(A)') 'Year    Month    Day    Hour'
	WRITE(78,'(A)') '2010    03       02     00   | Start Times'
	WRITE(78,'(A)') '2010    06       04     23   | Stop  Times'
	CLOSE(78)
		
	STOP
	ENDIF

	READ(77,*) ! Skip header
	READ(77,*) startYear,startMonth,startDay,startHour
	READ(77,*) stopYear,stopMonth,stopDay,stopHour
	CLOSE(77)

C *****************************************************************************

C     FIND THE LENGTH OF THE CORRECT_TIME FILE
      PRINT*, 'Reading correct_times.txt'
	PRINT*, ' '
	n_corr = 0 ! Length of correct_times
	OPEN(66, File='correct_times.txt', STATUS = 'OLD',IOSTAT=IOS)
	
	IF (IOS.ne.0) THEN
	PRINT*, 'Error! File correct_times.txt is missing'
	PRINT*, 'Format is year month day hour min'
	PRINT*, 'Also check the file is not still open elsewhere!'
	PRINT*, ''
	PRINT*, 'Generating a DUMMY correct_times.txt to continue'
	PRINT*, ''
	CLOSE(66)
	OPEN(66, File='correct_times.txt')
	WRITE(66,'(A)') '2010    02    01    00    00'
	REWIND(66)
	
	ENDIF
	
	DO ! 1
	READ(66,*,END = 10)
	n_corr = n_corr + 1
	ENDDO !1
10    REWIND(66)
	
C     ALLOCATE CORRECT ARRAYS
	ALLOCATE(corr_year(n_corr))
	ALLOCATE(corr_month(n_corr))
	ALLOCATE(corr_day(n_corr))
	ALLOCATE(corr_hour(n_corr))
	ALLOCATE(corr_minute(n_corr))

C	READ CORRECT_TIMES.TXT
	DO k =1,n_corr
	READ(66,*) corr_year(k),corr_month(k),corr_day(k),
	1           corr_hour(k),corr_minute(k)
	ENDDO
      CLOSE(66)
      
C *****************************************************************************
	
	
	DO idx = 1,n_files ! Do for each file in the directory
	
C     GET FILE NAMES
      
      OrigFile = TRIM(FileNames(idx))
	
      suffix_index =INDEX(OrigFile,suffix)
	

	Variable = OrigFile(1:suffix_index-1)
      Variable = TRIM(Variable)
	

	
      WRITE(MissingFile,'(5A)') missDir,delim,TRIM(Variable), und, miss
      MissingFile = TRIM(MissingFile)
	

	WRITE(NewFile,FMT=102) procDir,delim,TRIM(Variable),und,startYear,
	1                      startMonth,startDay,startHour,und,stopYear,
     2                      stopMonth,stopDay,stopHour,suffix
102   FORMAT(4A,I4,I2.2,I2.2,I2.2,A,I4,I2.2,I2.2,I2.2,A)
	NewFile = TRIM(NewFile)
	

      PRINT*, 'Processing ', TRIM(Variable)
	

	OPEN(99,File = OrigFile)
	OPEN(999,File = MissingFile,STATUS = 'REPLACE')
	OPEN(100,File = NewFile ,STATUS = 'REPLACE')
      secheadflag = .false.


C     FIND THE NUMBER OF TIMESTAMPS AND LENGTH OF R2C
      n_frame = 0 ! Number of timestamps
      n_r2c = 0 ! Lenght of R2C
	DO
	READ(99,*,END = 11) line
	   IF(line(1:6).eq.':Frame') THEN
	   n_frame = n_frame + 1
	   ENDIF
      n_r2c = n_r2c + 1
	ENDDO
11	REWIND(99)
      

C     ALLOCATION ARRAYS

	ALLOCATE(year(n_frame))
	ALLOCATE(month(n_frame))
	ALLOCATE(day(n_frame))
	ALLOCATE(hour(n_frame))
	ALLOCATE(minute(n_frame))
	

      
C	READ FRAME VALUES

	endHeaderFlag = 0
	startCopyFlag = 0
      j = 1
	n = 1
	DO k=1,n_r2c !2
	READ(99,FMT = 333) line
      line = TRIM(line)
	
	  
        
	  IF (endHeaderFlag.eq.0) THEN ! Write all lines to end of end header
	  WRITE(100,'(A)') TRIM(line)
        ENDIF

	  IF(line(1:10).eq.':endHeader') THEN ! Stop auto writing after end of header
	  endHeaderFlag = 1
	
	  ENDIF  
      


	  IF(line(1:6).eq.':Frame') THEN
	 
		len_line = LEN(TRIM(line))
	    timestamp = line(len_line-23:len_line)
	
	
		READ(timestamp,fmt=100) year(j),month(j),day(j),hour(j),
	1	                        minute(j)

      ! Incase the start date is prior to dates in the R2C
	    IF(j .eq. 1) THEN
	    WRITE(*,*) 'Specified start date is: (YYYY MM DD HH)'
	    WRITE(*,*) startYear,startMonth,startDay,startHour
	    WRITE(*,*) ' '
		WRITE(*,*) 'First date in ',TRIM(Variable),
	1                ' is:(YYYY MM DD HH)'
	    WRITE(*,*) year(j),month(j),day(j),hour(j)
	    WRITE(*,*)
		  
	    ENDIF



	    IF (startCopyFlag.eq.0) THEN
            IF (year(j).eq.startYear.and.month(j).eq.startMonth
     1      .and.day(j).eq.startDay.and.hour(j).eq.startHour) THEN
            startCopyFlag = 1
	      ENDIF
	    ENDIF
	   
	    j = j + 1
	  ENDIF ! Line(1:6) .eq. :Frame

        
      
	  IF(startCopyFlag.eq.1) THEN
	    IF(line(1:6).eq.':Frame') THEN
	    
		WRITE(100,fmt=101) fm,n,n,qu,year(j-1),sl,month(j-1),sl,
     1                        day(j-1),hour(j-1),col,minute(j-1),sec
	    n = n + 1 ! Next Frame (Frames are renumbered starting at 1)
	    
		ELSE
	      IF ((line(1:1).eq.'#' .or. line(1:1).eq.':') ! Incase there is a second header
	1         .and.endHeaderFlag.eq.1 .and. 
     2          line(1:9).ne.':EndFrame') THEN
	            IF(secheadflag.eq. .false.) THEN
	            WRITE(*,*) ' Found second header in r2c file'
	            WRITE(*,*) ' Removing ...'
	            ENDIF
	            secheadflag = .true.
	          CYCLE
	      ELSE
	          WRITE(100,'(A)') TRIM(line)
	      ENDIF
	    ENDIF
	  ENDIF

        IF(startCopyFlag.eq.1) THEN
	    IF(year(j-1).eq.stopYear.and.month(j-1).eq.stopMonth
     1       .and.day(j-1).eq.stopDay.and.hour(j-1).eq.stopHour
     2        .and.line(1:9).eq.':EndFrame') THEN
	       startCopyFlag = 0
	    ENDIF
	  ENDIF

	ENDDO !2

C     CHECK WHICH VALUES ARE MISSING AND PRINT MISSING_VALUES.TXT      
      DO n = 1,n_corr
	      foundFlag = 0
	      j = 1
	      DO WHILE (foundFlag.eq.0) !4
      
	        IF(year(j).eq.corr_year(n).and.month(j).eq.corr_month(n)
	1		.and.day(j).eq.corr_day(n).and.hour(j).eq.corr_hour(n)
     2         .and.minute(j).eq.corr_minute(n)) THEN
     	         foundFlag = 1

	        ELSEIF (j.eq.n_frame) THEN
	  	    WRITE(999,*)corr_year(n),corr_month(n),corr_day(n),
	1                    corr_hour(n),corr_minute(n)
	        foundFlag = 1
	        ELSE
	        j = j+1
	        ENDIF
	
	      ENDDO ! WHILE (foundFlag) !4
	ENDDO



	CLOSE(100)
	CLOSE(99)
	CLOSE(999)

	DEALLOCATE(year)
	DEALLOCATE(month)
	DEALLOCATE(day)
	DEALLOCATE(hour)
	DEALLOCATE(minute)

	ENDDO ! For each file in directory


333   FORMAT(A)
100   FORMAT(I4,1X,I2,1X,I2,1X,I2,1X,I2)
101   FORMAT(A6,I10,I10,3X,A1,I4.4,A1,I2.2,A1,I2.2,1X,I2.2,A1,I2.2,A8)

      PRINT*, 'If first date is after start date R2C will be empty'

      PRINT*, 'Processing of R2C Files is Complete!'

      END PROGRAM ! ValidateR2C