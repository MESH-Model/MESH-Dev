          program R2C_mesh_data_conversion

C    1. This program reads the gridded ASCII "R2C" format data - ENSIM format?
C       and converts it to a binary file that is read by MESH (meteorological data)
C    2. Please take a look a the definition of the variables in order 
c       to determine if they are OK for the period to be converted
C                                         S. Marin Nov 22, 2007
C    3. February 22, 2008: a register containing the initial date for the data  
c       in the BIN file is added to the binary file. S.M

      IMPLICIT NONE
c
      REAL*4, ALLOCATABLE:: VALUE(:,:,:,:) 
      INTEGER IHOUR_BIN,IMIN_BIN,IDAY_BIN,IYEAR_BIN,ISTEP_BIN
      integer nf,nfiles,nlong,nlat,nt,it,ix,iy,i,j,ios
      character*40 fln(3650,20),FNF(20)         !fln(nfiles,nf), fnf(nf)
      character*20 header
c     !value(nlat,nlong,nt,nf)

c ******************************
c READ FILES WITH NAMES OF THE DATAFILES
c ******************************
C  FNF(1): FILE CONTAINING FILES WITH SHORT WAVE DATA  
C  FNF(2): FILE CONTAINING FILES WITH LONG WAVE DATA   
C  FNF(3): FILE CONTAINING FILES WITH RAIN DATA        
C  FNF(4): FILE CONTAINING FILES WITH TEMPERATURE DATA 
C  FNF(5): FILE CONTAINING FILES WITH WIND X DATA      
C  FNF(6): FILE CONTAINING FILES WITH PRESSION DATA    
C  FNF(7): FILE CONTAINING FILES WITH HUMIDITY DATA    
C  ...
      OPEN(UNIT=5,FILE='PARNAMES.TXT',STATUS='OLD',IOSTAT=IOS)
      READ(5,*)           ! COMMENT LINE
      READ(5,*)IHOUR_BIN  ! initial hour
      READ(5,*)IMIN_BIN   ! initial minute
      READ(5,*)IDAY_BIN   ! initial day (Julian day)
      READ(5,*)IYEAR_BIN  ! initial year
      READ(5,*)ISTEP_BIN  ! time-step 1: hourly; 2:half_hourly
      READ(5,*)           ! COMMENT LINE
      READ(5,*)NF         ! NUMBER OF FILES
      READ(5,*)NLONG      ! NUMBER OF LOGITUDE POINTS (X)
      READ(5,*)NLAT       ! NUMBER OF LATITUDE POINTS (Y)
      READ(5,*)           ! COMMENT LINE
      DO I=1,NF
         READ(5,*)FNF(i) 
      ENDDO 
      CLOSE(5)
c nt:# of timestep per file
c nf:# number of varaibles to read (# of files)           
      ALLOCATE(value(nlat,nlong,2000,7))             !value(nlat,nlong,nt,nf)

c OPEN AND READ FILES WITH NAME OF THE DATAFILES
c REMAINDER: In an open statement IOSTAT=ios, where ios is an integer variable
c            which is set to zero if the statement is executed successfully
c
       DO J=1,NF
         OPEN(UNIT=5,FILE=trim(FNF(J)),STATUS='OLD',IOSTAT=IOS)
           i=0
           DO WHILE (ios.eq.0)
              i=i+1
              read(5,*,iostat=ios)fln(i,j)
           ENDDO
           if(j.EQ.1)nfiles=i-1
           if(i-1.lt.nfiles)nfiles=i-1
         CLOSE(5)
       ENDDO
       PRINT*,'NUMBER OF FILES TO BE PRECESSED FOR EACH VARIABLE= ',NFILES
c OPENING READING AND WRITTING DATA... TIME STEPS PER FILE
      OPEN(10,file='watclass.bin',action='write',form='unformatted',access='sequential')
       WRITE(10)IHOUR_BIN,IMIN_BIN,IDAY_BIN,IYEAR_BIN,ISTEP_BIN
       DO I=1,NFILES
       PRINT*,'PROCCESSING GROUP OF FILES ',I,'  OF ',NFILES
          DO J=1,NF
            OPEN(UNIT=15,FILE='.\R2C\'//trim(FLN(I,J)),STATUS='OLD',IOSTAT=IOS)
            PRINT*,'READING FILE ......',fln(i,j)
              header=''
              do while((trim(header).ne.':EndHeader').and.
     1                 (trim(header).ne.':Endheader').and.
     2                 (trim(header).ne.':endHeader').and.
     3                 (trim(header).ne.':endheader').and.
     4                 (trim(header).ne.':ENDHEADER'))   !should be more options sorry!
              read(15,*)header
              end do
              it=0  !index for number of times in each file
                    !for example: monthly file - hourly data -->maximum 744 
              DO WHILE (ios.eq.0)
                 it=it+1
                 read(15,*,iostat=ios)header
                 do iy=1,nlat
                    read(15,*,iostat=ios)(value(iy,ix,it,j),ix=1,nlong)
                 enddo                    
                 read(15,*,iostat=ios)header
              ENDDO
            CLOSE(15)
            nt=it-1
          ENDDO
          do it=1,nt
            do j=1,nf
                WRITE(10)((value(iy,ix,it,j),ix=1,nlong),iy=1,nlat)
            enddo
          enddo   
       ENDDO 
       CLOSE (10)
      pause
      stop
	end
