!> "REDIST_NEW_CH_PAR"
!>
!> Program uses parts from Watroute to distribute channel
!> parameter values according the grid characteristics
!> defined in the drainage database (shed) file, and from
!> a single-array of channel parameter values.
!>
!> The 'nclasses' and an extra dimension are added to where
!> the channel parameter values are stored, in case
!> there is need to use multiple classes of parameter
!> values.

PROGRAM REDIST_NEW_CH_PAR

USE area_watflood
USE EF_Module
USE EF_ParseUtilities

INTEGER :: ii,i,j
INTEGER :: istatusfile,iinfounit,iunit,istat
LOGICAL :: iexists
INTEGER :: nchpars,nclasses
CHARACTER(10) :: time
CHARACTER(8) :: cday
CHARACTER(14),DIMENSION(:),ALLOCATABLE :: chpars
REAL,DIMENSION(:,:),ALLOCATABLE :: chparvalues

!> Allocate filename and book-keeping variables
ALLOCATE(fln(100),filename(100),infln(100),outfln(100),stat=istat)
IF(istat.NE.0) STOP 'ERROR: Error allocating filename variables'

!> Set author as program name
author='redist_new_ch_par.f90'

!> Set filenames
filename(20)='infiles_redist_new_ch_par.txt'
filename(21)='outfiles_redist_new_ch_par.txt'
filename(51)='redist_new_ch_par.txt'
filename(52)='import_new_ch_par.txt'
filename(53)='new_ch_par.r2c'
filename(98)='redist_new_ch_par_info.txt'
filename(99)='scratch5'

!> These constants are required to read the event file (adapted from rte.f)
id=1
ni=1

!> Open the status file
istatusfile=51
OPEN(UNIT=istatusfile,FILE=TRIM(ADJUSTL(filename(istatusfile))),STATUS='UNKNOWN',IOSTAT=istat)
IF (istat.NE.0) STOP 'ERROR: Error opening status file'

!> Open info file
iinfounit=98
OPEN(UNIT=iinfounit,FILE=TRIM(ADJUSTL(filename(iinfounit))),STATUS='UNKNOWN',IOSTAT=istat)
IF (istat.NE.0) STOP 'ERROR: Error opening info file'

!> Adapted from rte.f
CALL DATE_AND_TIME(cday,time)
WRITE(istatusfile,*)
WRITE(istatusfile,6016) time(1:2),time(3:4),time(5:6)
WRITE(istatusfile,6017) cday(1:4),cday(5:6),cday(7:8)
WRITE(istatusfile,*)
WRITE(istatusfile,5003) istatusfile,filename(istatusfile)

!> This block of code is taken from rte.f to read the filenames of input files
iunit=20
INQUIRE(FILE=TRIM(ADJUSTL(filename(iunit))),EXIST=iexists)
IF(iexists) THEN
  OPEN(UNIT=99,FILE=TRIM(ADJUSTL(filename(iunit))),STATUS='OLD',IOSTAT=istat)
  IF(istat.EQ.0) THEN
    DO i=1,50
      READ(UNIT=99,FMT=5001,IOSTAT=istat) infln(i)
      IF(istat.NE.0) STOP 'ERROR: Reading infiles.txt'
    END DO
    CLOSE(UNIT=99)
  ELSE
    PRINT*, 'Error opening ',TRIM(filename(iunit))
    PRINT*, 'Continuing using default input files'
  END IF
ELSE
  PRINT*, TRIM(filename(iunit)),' file not found, defaults used'
  PRINT*
END IF

1000 FORMAT(' ',2(' ',A))
1030 FORMAT(' ','Unit no. =',I3,' file no',I3,' = ',A)
5001 FORMAT(A)
5002 FORMAT(/' output files')
5003 FORMAT(' opened unit',I5,' file name ',A)
5005 FORMAT(' Files opened:')
6015 FORMAT(' runtime  ',A2,':',A2,':',A2,2X,A4,'-',A2,'-',A2)
6016 FORMAT('  runtime    ',2(A2,':'),A2)
6017 FORMAT('  rundate  ',A4,'-',A2,'-',A2)

!> This block of code is taken from rte.f to read the filenames of output files
iunit=21
INQUIRE(FILE=TRIM(ADJUSTL(filename(iunit))),EXIST=iexists)
IF(iexists) THEN
  OPEN(UNIT=99,FILE=TRIM(ADJUSTL(filename(iunit))),STATUS='OLD',IOSTAT=istat)
  IF(istat.EQ.0) THEN
    DO i=1,50
      READ(UNIT=99,FMT=5001,IOSTAT=istat) outfln(i)
      IF(istat.NE.0) STOP 'ERROR: Reading outfiles.txt'
    END DO
    CLOSE(UNIT=99)
  ELSE
    PRINT*, 'Error opening ',TRIM(filename(iunit))
    PRINT*, 'Continuing using default output files'
  END IF
ELSE
  PRINT*, TRIM(filename(iunit)),' file not found, defaults used'
  PRINT*
END IF

!> Read event file (required for watershed file name)
IF(LEN_TRIM(infln(1)).GT.2) THEN
  fln(99)=infln(1)
ELSE
  fln(99)='event/event.evt'
END IF
CALL rdevt(date,conv,scale,smc5,nhr,nhf)

!> Read watershed file
CALL read_shed_ef(31,1)

!> *************************************************************
!> Read attributes and attribute values
!> *************************************************************

!> The unit of the file is 52
iunit=52

!> Check if the filename was specified in the input files file
IF(LEN_TRIM(infln(2)).GT.2) filename(iunit)=infln(2)

!> Check if the file exists
INQUIRE(FILE=TRIM(ADJUSTL(filename(iunit))),EXIST=iexists)
IF(iexists) THEN

  !> Open the file
  OPEN(UNIT=99,FILE=TRIM(ADJUSTL(filename(iunit))),STATUS='OLD',IOSTAT=istat)
  IF(istat.EQ.0) THEN

    !> Read the first line: no. pars, no. classes
    READ(UNIT=99,FMT=*,IOSTAT=istat) nchpars,nclasses
    IF(istat.NE.0) THEN
      STOP 'ERROR: Reading number of parameters and values from file'
    ELSE
      WRITE(UNIT=istatusfile,FMT=*) 'nchpars',nchpars,'nclasses',nclasses
    END IF

    !> Allocate the channel parameter variables
    ALLOCATE(chpars(nchpars),chparvalues(nchpars,nclasses),stat=istat)
    IF(istat.NE.0) STOP 'ERROR: Allocating channel parameter arrays'

    !> Read attribute values
    DO ii=1,nchpars
      READ(UNIT=99,FMT=*,IOSTAT=istat) chpars(ii),(chparvalues(ii,j),j=1,nclasses)
      IF(istat.NE.0) THEN
        STOP 'ERROR: Reading attribute value (check status file)'
      ELSE
        WRITE(UNIT=istatusfile,FMT=*) 'attribute',ii,chpars(ii),(chparvalues(ii,j),j=1,nclasses)
      END IF
    END DO !DO i=1,nchpars
    CLOSE(UNIT=99)
  ELSE
    STOP 'ERROR: Opening channel parameter input file'
  END IF !IF(istat.EQ.0)
ELSE
  STOP 'ERROR: Channel parameter input file does not exist'
END IF !IF(iexists)

!> *************************************************************
!> Distribute values and write channel parameter R2C file
!> *************************************************************

!> The unit of the file is 53
iunit=53

!> Check if the filename was specified in the input files file
IF(LEN_TRIM(outfln(6)).GT.2) filename(iunit)=outfln(6)

!> Open the file
OPEN(UNIT=iunit,FILE=TRIM(ADJUSTL(filename(iunit))),STATUS='UNKNOWN',IOSTAT=istat)
IF (istat.NE.0) STOP 'ERROR: Error opening channel parameter output file'

!> Write header (EnSim ASCII R2C)
WRITE(iunit,3005) '########################################'
WRITE(iunit,3005) ':FileType r2c  ASCII  EnSim 1.0         '
WRITE(iunit,3005) '#                                       '
WRITE(iunit,3005) '# DataType               2D Rect Cell   '
WRITE(iunit,3005) '#                                       '
WRITE(iunit,3005) ':Application             EnSimHydrologic'
WRITE(iunit,3005) ':Version                 2.1.23         '
WRITE(iunit,3020) ':WrittenBy          ',author
CALL DATE_AND_TIME(cday,time)
WRITE(iunit,3010) ':CreationDate       ',cday(1:4),cday(5:6),cday(7:8),time(1:2),time(3:4)
WRITE(iunit,3005) '#                                       '
WRITE(iunit,3005) '#---------------------------------------'

!> Write source file name as single-array input file
WRITE(iunit,3005) '#                                       '
WRITE(iunit,3020) ':SourceFile         ',TRIM(filename(52))

!> Write projection (LatLong or UTM seem to be allowed in read_shed_ef.f)
WRITE(iunit,3005) '#                                       '
WRITE(iunit,3004) ':Projection         ',coordsys1
WRITE(iunit,3004) ':Ellipsoid          ',datum1
IF(llflg.EQ.'n') WRITE(iunit,3004) ':Zone               ',zone1

!> Write attribute list
WRITE(iunit,3005) '#                                       '
DO ii=1,nchpars
  WRITE(iunit,3008) ':AttributeName',ii,chpars(ii)
END DO

!> Write grid characteristics
WRITE(iunit,3005) '#                                       '
WRITE(iunit,3003) ':xOrigin            ',xorigin
WRITE(iunit,3003) ':yOrigin            ',yorigin
WRITE(iunit,3005) '#                                       '
WRITE(iunit,3001) ':xCount             ',xcount
WRITE(iunit,3001) ':yCount             ',ycount
WRITE(iunit,3003) ':xDelta             ',xdelta
WRITE(iunit,3003) ':yDelta             ',ydelta
WRITE(iunit,3005) '#                                       '
WRITE(iunit,3005) ':EndHeader                              '

3000 FORMAT(A10,I5)
3001 FORMAT(A20,I16)
3002 FORMAT(2A20)
3003 FORMAT(A20,F16.7)
3004 FORMAT(A20,A10,2X,A10)
3005 FORMAT(A40)
3006 FORMAT(A3,A10)
3007 FORMAT(A14,I5,A6,I5)
3008 FORMAT(A14,1X,I5,1X,A)
3010 FORMAT(A20,A4,'-',A2,'-',A2,2X,A2,':',A2)
3012 FORMAT(A9)
3020 FORMAT(A20,A40)

!> Write attributes
DO ii=1,nchpars
DO i=1,ycount
!DO j=1,xcount
!TODO: Tie into here, method to write vararible by CLASS
  WRITE(iunit,1309) (chparvalues(ii,nclasses),j=1,xcount)
!END DO !DO j=1,xcount
END DO !DO i=1,ycount
END DO !DO ii=1,nchpars

!> Close the file
CLOSE(UNIT=iunit)

1300 FORMAT(*(1X,F5.0))
1301 FORMAT(*(1X,F5.1))
1302 FORMAT(*(1X,F5.2))
1303 FORMAT(*(1X,F6.3))
1304 FORMAT(*(1X,F7.4))
1305 FORMAT(*(1X,F8.5))
1306 FORMAT(*(1X,F9.6))
1307 FORMAT(*(1X,F12.6))
1308 FORMAT(*(1X,F10.3))
1309 FORMAT(*(1X,E12.3E2))

!> Close the status and info files
CLOSE(UNIT=istatusfile)
CLOSE(UNIT=iinfounit)

PRINT*,'Program terminated normally'

END PROGRAM REDIST_NEW_CH_PAR
