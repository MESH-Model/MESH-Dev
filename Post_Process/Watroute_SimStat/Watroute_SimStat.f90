PROGRAM Watroute_SimStat

!> area_watflood contains a number of variables required for Watroute
USE area_watflood

!> ADAPTED FROM MESH ROUTINE FOR AUTOCALIBRATION USING PRE-EMPTION - A MAXIMUM OF 1 YEAR (365 DAYS)
!> DAILY STREAM FLOW IS SUPPOSED TO BE USED FOR AUTOCALIBRATION PURPOSE.
!* NCAL:        ACTUAL NUMBER OF CALIBRATION DATA
!* DAYCOUNT:    CURRENT DAY IN STREAMFLOW INPUT FILE (HOURLY DATA FROM WATROUTE)
!* NCALCOUNT:   CURRENT INDEX IN CALIBRATION DATA
!* EXISTS:      LOGICAL USED WHEN CHECKING IF A FILE EXISTS
!* SAE:         SUM OF ABSOLUTE VALUE OF ERRORS (DEVIATIONS BETWEEN OBSERVED
!*              AND SIMULATED STREAM FLOWS)
!* SAESRT:      SUM OF ABSOLUTE VALUE OF ERRORS (DEVIATIONS BETWEEN SORTED OBSERVED
!*              AND SORTED SIMULATED STREAM FLOWS)
!* NSE:         MEAN NASH-SUTCLIFFE EFFIECIENCY INDEX OF DAILY FLOWS
!* FBEST:       SAE AT PREVIOUS TIME STEP TRIAL
!* FTEST:       SAE AT CURRENT TIME STEP TRIAL
!* QHYDIN:      INPUT DATA FROM WATROUTE (HOURLY OBSERVED)
!* QOIN:        INPUT DATA FROM WATROUTE (HOURLY MODELLED/SIMULATED)
!* QOBS:        DAILY INPUT DATA USED BY SIMSTAT.F90 (DAILY OBSERVED)
!* QSIM:        DAILY INPUT DATA USED BY SIMSTAT.F90 (DAILY MODELLED/SIMULATED)
INTEGER :: NCAL,DAYCOUNT,NCALCOUNT
LOGICAL :: EXISTS
REAL :: SAE,SAESRT,SAEMSRT,FBEST,FTEST,NSE
REAL,DIMENSION(:),ALLOCATABLE :: QHYDIN,QOIN
REAL,DIMENSION(:,:),ALLOCATABLE :: QOBS,QSIM
REAL :: MAXNAN

!> Variables for SIMSTATS.F90
!* NSD:   NASH-SUTCLIFFE COEFFICIENT (DAILY)
!* NSW:   NASH-SUTCLIFFE COEFFICIENT (WEEKLY)
REAL :: MAE,RMSE,BIAS,NSD,NSW,TPD,TPW

!> Variables for Watroute
!* DATE:          DATE
!* IALLOCATE:     DIAGNOSTIC VARIABLE USED WHEN ALLOCATING VARIABLES
!* NHF:           NUMBER OF HOURS OF FLOW RECORD
!* LINECOUNT:     NUMBER OF LINES
!* IDEALLOCATE:   DIAGNOSTIC VARIABLE USED WHEN DE-ALLOCATING VARIABLES
!* I,J,II,L:      COUNT VARIABLES
CHARACTER*14 :: DATE
INTEGER :: IALLOCATE, &
    NHR,NHF,LINECOUNT,IDEALLOCATE,IENDARG, &
    I,J,II,L,ICASE
REAL*4 :: CONV,SCALE,SMC5(16)

!> Allocate file variables (Watroute)
ALLOCATE(FLN(999),FILENAME(999),OUTFLN(999),INFLN(999),STAT=IALLOCATE)
IF(IALLOCATE.NE.0) &
    STOP 'Error: On allocation of file name arrays'

!> SET DEFAULT FILENAMES FOR OUTPUT FILES:
!> these names may be replaced with the outfiles_rte.txt file in the working
!> directory to send the files to a designated place for output.
!> A default outfiles.new file is created in the working directory each
!> time this program is run.

!> The file names in the array have to be initialized
DO I=51,100
    FILENAME(I)='..'
END DO

!> This constant is required by the Watroute code
IENDARG=0

!> These constants are required to read the event file
ID=1              !not used like this now  nk Apr. 8/03
NI=1

!> Max NaN is a number to replace NaN values
MAXNAN=1.E+15

!> This block of code is taken from rte.f to read in the name of the event file
INQUIRE(FILE='infiles_rte.txt',EXIST=exists)
IF(EXISTS) THEN
    OPEN(UNIT=99,FILE='infiles_rte.txt',STATUS='OLD',IOSTAT=IOS)
    IF(IOS.EQ.0) THEN

        !> AN INFILES.TXT FILE EXISTS AND WE'LL READ IT:
!d        print*,'reading infile names from infiles_rte.txt'
        ISWITCH=0
        DO I=1,50
            READ(UNIT=99,FMT=5001,IOSTAT=IOS) INFLN(I)
            IF(IOS.EQ.0) THEN
                IF(ISWITCH.EQ.0 .AND. LEN_TRIM(INFLN(I)).LE.2) THEN
                    NREACHES=0
                    ISWITCH=1
                ELSE IF (ISWITCH.EQ.1 .AND. LEN_TRIM(INFLN(I)).GT.2) THEN
                    NREACHES=NREACHES+1
                ELSE IF (ISWITCH.EQ.1 .AND. LEN_TRIM(INFLN(I)).LE.2) THEN
                    ISWITCH=2
                END IF
            ELSE
!d                 print*,'Problems on unit 99'
!d                 print*,'Warning: error reading file name infiles_rte.txt'
!d                 print*,'possible cause: end of file is reached'
!d                 print*,' - need 50 lines'
!d                 print*,'iostat code =',ios
!d                 print*,'Got as far as line ',i-100

                STOP 'ERROR: Reading infiles_rte.txt'
            END IF !> (IOS.EQ.0)
            IOFLG=I
        END DO
        IF(NREACHES.GT.REACHESMAX) THEN
            WRITE(*,*) 'The current setup can have no more than ',NREACHESMAX,' reaches'
            WRITE(*,*) 'The calculated # reaches is: ',NREACHES
        END IF
        CLOSE(UNIT=99)
    ELSE
        PRINT*,'Error opening infiles_rte.txt'
        PRINT*,'Continuing using default input files'
    END IF
ELSE
    PRINT*,'infiles_rte.txt file not found, defaults used'
    PRINT*
END IF

5001 FORMAT(A)

!> Set the file name of the event file
IF(LEN_TRIM(INFLN(1)).GT.2) THEN
    FLN(99)=INFLN(1)
ELSE
    FLN(99)='event/event.evt'
END IF

!> Read the event file to collect the record length
CALL RDEVT(DATE,CONV,SCALE,SMC5,NHR,NHF)

!> Read the flow file to collect the number of streamflow guages
CALL READ_FLOW_EF()

!> Open the streamflow file
INQUIRE(FILE='spl_rpn.csv',EXIST=EXISTS)
IF(.NOT.EXISTS) THEN
    STOP 'ERROR: spl_rpn.csv does not exist'
ELSE
    OPEN(UNIT=54,FILE='spl_rpn.csv',STATUS="OLD")
END IF

!> Skip header information
READ(UNIT=54,FMT='(A)') !'Observed and simulated streamflows (m^3/s)'
READ(UNIT=54,FMT='(A10,'//TRIM(NOSTR)//'(",",A8,","))') !'Station,,,',(GAGE(L),L=1,NO)
READ(UNIT=54,FMT='(A12,'//TRIM(NOSTR)//'(",",F8.3,","))') !'Longitude,,,',(XSTR(L),L=1,NO)
READ(UNIT=54,FMT='(A11,'//TRIM(NOSTR)//'(",",F8.3,","))') !'Latitude,,,',(YSTR(L),L=1,NO)
READ(UNIT=54,FMT='(A14,'//TRIM(NOSTR)//'(",",I8,","))') !'Xcoord_grid,,,',(JX(L),L=1,NO)
READ(UNIT=54,FMT='(A14,'//TRIM(NOSTR)//'(",",I8,","))') !'Ycoord_grid,,,',(IY(L),L=1,NO)
READ(UNIT=54,FMT='(A)') !'YEAR,MONTH,DAY,HOUR'//REPEAT(',OBS,SIM',NO)

!d print*, year1,month1,day1,hour1
!d print*, nhf

ALLOCATE(QHYDIN(NO),QOIN(NO),STAT=IALLOCATE)
IF(IALLOCATE.NE.0) &
    STOP 'ERROR: On allocation of QHYDIN,QOIN on NO'

!> Set the current day
DAYCOUNT=DAY1

!> Calculate the number of daily records from the number of hours from the event file
NCAL=CEILING(NHF/24.0)
!d print*,nhf,ncal
ALLOCATE(QOBS(NCAL,NO),QSIM(NCAL,NO),STAT=IALLOCATE)
IF(IALLOCATE.NE.0) &
    STOP 'ERROR: On allocation of QOBS,QSIM on NCAL,NO'

!> Pre-set the record
QOBS(:,:)=0.
QSIM(:,:)=0.

!d open(unit=55,file='spl_csv_daily.csv',status='unknown')

NCALCOUNT=1
!linecount=8

!> Begin DO-LOOP (Read records)
!> The format of the read statement is adopted from route.f from Watroute
DO

!> Stop the loop if the expected number of records is exceeded
IF(NCALCOUNT.GT.NCAL) &
    GOTO 20

READ(UNIT=54,FMT=7010,IOSTAT=IOS) YEAR1,MONTH_NOW,DAY_NOW,HOUR_NOW,(QHYDIN(L),QOIN(L),L=1,NO)

!> Exit the loop if end-of-file (or on error)
IF (IOS.NE.0) GOTO 20

!d print*,linecount
!d print*,year1,month_now,day_now,hour_now
!d print*,(qhydin(l),qoin(l),l=1,no)

!> Update counter if it's a new day
IF(DAY_NOW.NE.DAYCOUNT) THEN
!d     write(unit=55,fmt=7000) year1,month_now,day_now,hour_now,(qobs(ncalcount,l),qsim(ncalcount,l),l=1,no)
    DAYCOUNT=DAY_NOW
    NCALCOUNT=NCALCOUNT+1
END IF !> (DAY_NOW.NE.DAYCOUNT)

!> Update daily totals (Daily totals are used by SIMSTATS.F90)
QOBS(NCALCOUNT,:)=QOBS(NCALCOUNT,:)+QHYDIN(:)
QSIM(NCALCOUNT,:)=QSIM(NCALCOUNT,:)+QOIN(:)

END DO

!d 7000 format(4(i5,','),f10.3,*(',',f10.3))
7010 FORMAT(4(I5,1X),F10.3,*(1X,F10.3))

!> At end of input
20 CONTINUE

!> Stop the program if the expected and actual number of records do not match
IF(NCALCOUNT.LT.NCAL) &
    STOP 'ERROR: Unexpected end of file in spl_rpn.csv'
CLOSE(54)
!d close(55)

!d print*,'finished reading file'
!d print*,'ncal',ncal,'ncalcount',ncalcount

!> Calculate simulation statistics (SIMSTATS.F90 from standalone MESH)
CALL SIMSTATS(QOBS(1:NCAL,1),QSIM(1:NCAL,1),NCAL,BIAS,NSD,NSW,TPD)

!> Check for NaN value
IF(isnan(NSD)) NSD=(-1)*MAXNAN

!> Print Nash-Sutcliffe coefficient (Daily)
OPEN(100,FILE='NS.txt',STATUS='UNKNOWN')
WRITE(100,*)NSD
CLOSE(100)

!> Check for NaN value
IF(isnan(NSW)) NSW=(-1)*MAXNAN

!> Print Nash-Sutcliffe coefficient (Weekly)
OPEN(100,FILE='NSW.txt',STATUS='UNKNOWN')
WRITE(100,*)NSW
CLOSE(100)

!d print*,'nsd',nsd

PRINT*,'Program terminated normally'

END PROGRAM Watroute_SimStat
