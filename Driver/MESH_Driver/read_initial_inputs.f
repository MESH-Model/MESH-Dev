      SUBROUTINE READ_INITIAL_INPUTS(
!> GENERIC VARIABLES
     +  RELEASE,
!> VARIABLES FOR READ_RUN_OPTIONS
     +  IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  ICTEMMOD, IOS, PAS, N, IROVAL, WF_NUM_POINTS,
     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START,
     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END,
     +  IRONAME, GENDIR_OUT,
!> variables for drainage database or new_shd
     + IGND, ILG, WF_IYMAX, WF_JXMAX,
     + WF_LAND_COUNT,
     + LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX,
     + LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX,
     + WF_LAND_MAX, WF_LAND_SUM,
!> variables for READ_CHECK_FORCING_FILES
     + NUM_CSV, NUM_R2C, NUM_SEQ,
!> variables for READ_PARAMETERS_CLASS
     +  TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6,
     +  NAME1, NAME2, NAME3, NAME4, NAME5, NAME6,
     +  PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6,
     +  ILW, NLTEST, NMTEST, JLAT, ICAN,
     +  DEGLAT, DEGLON,
     +  HOURLY_START_DAY,  HOURLY_STOP_DAY,
     +  DAILY_START_DAY,   DAILY_STOP_DAY,
     +  HOURLY_START_YEAR, HOURLY_STOP_YEAR,
     +  DAILY_START_YEAR,  DAILY_STOP_YEAR,
     +  IHOUR, IMIN, IDAY, IYEAR,
!> variables for READ_SOIL_INI
!> variables for READ_PARAMETERS_HYDROLOGY
     +  INDEPPAR, DEPPAR, WF_R2, M_C,
!> the types that are to be allocated and initialised
     +  op, sl, cp, sv, hp,ts,cm,
     +  SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, fls)

      USE MESH_INPUT_MODULE
      USE FLAGS
      USE AREA_WATFLOOD

      USE climate_forcing
      USE model_dates
      USE model_files

      IMPLICIT NONE
      
!> DECLARE THE READ IN VARIABLES.
!> ----------------------------
!> VALUES NEEDED FOR READ_RUN_OPTIONS
      INTEGER :: IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  ICTEMMOD, IOS, PAS, N, IROVAL, WF_NUM_POINTS,
     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START,
     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END
      CHARACTER(20) :: IRONAME
      CHARACTER*10 GENDIR_OUT
!> declared in area_watflood:
!>    ID
!> declared MESH_INPUT_MODULE:
!>     op%N_OUT, op%II_OUT, op%DIR_OUT

!> END OF VALUES FOR READ_RUN_OPTIONS
!> -----------------------------
!> VALUES NEEDED for drainage_database and/or new_shd.r2c
      INTEGER :: IGND, ILG,WF_IYMAX,WF_JXMAX,
     + WF_LAND_COUNT,
     + LATDEGMIN,LATMINMIN,LATDEGMAX,LATMINMAX,
     + LONDEGMIN,LONMINMIN,LONDEGMAX,LONMINMAX
      REAL :: WF_LAND_MAX, WF_LAND_SUM
      CHARACTER*500 fl_listMesh
!> already declared:
!>  SHDFILEFLAG, IOS, WF_NUM_POINTS,

!> declared in MESH_INPUT_MODULE
!>  WF_IBN, WF_IROUGH, WF_ICHNL, WF_NEXT, WF_ELEV, WF_IREACH,
!>  WF_DA, WF_BNKFLL, WF_CHANNELSLOPE, BASIN_FRACTION, WF_NHYD,
!>  WF_QR, WF_QBASE, WF_QI2, WF_QO1, WF_QO2, WF_STORE1, WF_STORE2,
!>  WF_QI1, SNOGRD, FSDOWN
!>  wc_algwet, wc_algdry, wc_thpor, wc_thlret,
!>  wc_thlmin, wc_bi, wc_psisat, wc_grksat, wc_hcps, wc_tcs

!> declared in area_watflood module
!>  NTYPE, NA, NAA, AL, NRVR, NTYPE, GRDN, GRDE, FLN
!>  ibn, ichnl, next, elev, ireach, da, bnkfll, slope
!>  IYMIN, JXMIN, IYMAX, JXMAX, YCOUNT, XCOUNT
!>  FRAC, ACLASS, YYY, XXX, MODELFLG,

!> local variables
      INTEGER :: I,J,K
!> END OF VALUES NEEDED for drainagedatabase of new_shd.r2c
!> -----------------------------
!> values needed for READ_CHECK_FORCING_FILES
      INTEGER :: NUM_CSV, NUM_R2C,NUM_SEQ
!> values that were declared earlier:
!>  BASINSHORTWAVEFLAG, BASINLONGWAVEFLAG,
!>  BASINTEMPERATUREFLAG, BASINRAINFLAG, BASINWINDFLAG,
!>  BASINPRESFLAG, BASINHUMIDITYFLAG
!> -----------------------------
!> values needed for READ_PARAMETERS_CLASS
      CHARACTER*4 ::
     +  TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6,
     +  NAME1, NAME2, NAME3, NAME4, NAME5, NAME6,
     +  PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
      INTEGER ::ILW, NLTEST, NMTEST,
     +        JLAT, ICAN,
     +        HOURLY_START_DAY,  HOURLY_STOP_DAY,
     +        DAILY_START_DAY,   DAILY_STOP_DAY,
     +        HOURLY_START_YEAR, HOURLY_STOP_YEAR,
     +        DAILY_START_YEAR,  DAILY_STOP_YEAR,
     +        IHOUR, IMIN, IDAY, IYEAR
      REAL :: DEGLAT, DEGLON
!> values declared in area watflood:
!>  NA, NTYPE
!> values declared above:
!>  IGND
!> values declared in mesh_input_module

!> -----------------------------
!> Values needed for READ_SOIL_INI :
!> values already declared above:
!>  NMTEST, IGND, NTYPE, NA, SOILINIFLAG
!>  values already declared in MESH_INPUT_MODULE
!>  wc_thpor,  wc_thlret, wc_thlmin, wc_bi,     wc_psisat,
!>  wc_grksat, wc_hcps,   wc_tcs,    wc_algwet, wc_algdry
!> -----------------------------
!> Values needed for READ_S_MOISTURE_TXT
!> already declared:
!>  IGND, YCOUNT, XCOUNT, na, NTYPE
!>  YYY, XXX, THLQROW, valuem
!> -----------------------------
!> Values needed for READ_PARAMETERS_HYDROLOGY :
      INTEGER INDEPPAR, DEPPAR, M_C
      REAL SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS
      REAL WF_R2(M_C)
!> Values already declared above:
!>  NTYPE, NA, RELFLG
!> -----------------------------
!> VALUES FOR MANY INPUT FILES
      CHARACTER*8 :: RELEASE(10)
      
      
!> The types that contain allocatable values
      TYPE(OutputPoints)        :: op
!+     TYPE(ShedInformation)     :: si
      TYPE(SoilLevels)          :: sl
      TYPE(ClassParameters)     :: cp
      TYPE(SoilValues)          :: sv
      TYPE(HydrologyParameters) :: hp

      TYPE(CLIM_INFO) :: cm
      TYPE(dates_model) :: ts
      type(fl_ids):: fls

!> DECLARE THE LOCAL VARIABLES

!> ====================================
!> read the RUN_OPTIONS input file called "MESH_input_run_options.ini"
!> and SET or RESET any CONTROL FLAGS
!> and READ the GRID OUTPUT DIRECTORIES.
      CALL READ_RUN_OPTIONS(
     +  IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  ICTEMMOD, IOS, PAS, N,
     +  IROVAL, WF_NUM_POINTS,
     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START,
     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END,
     +  ID, IRONAME, GENDIR_OUT, op, ts, cm, fls )

!> =====================================
!> DAN  * READ EVENT FILE
!> DAN  * GATHER FILE NAMES THAT ARE REQUIRED TO READ THE BASIN SHD FILE,
!> DAN  * AND WRITE THE RTE.EXE INPUT FILES (RUNOFF, RECHARGE, LEAKAGE).

!> DAN  * MUST ALLOCATE AND ADD "EVENT/EVENT.EVT" TO FLN.  FLN IS
!> DAN  * ALLOCATED TO THE SAME LIMIT AS IN RTE.EXE

!> And Open and read in values from new_shd.r2c file
!> *********************************************************************
!> dan * DRAINAGE DATABASE (BASIN SHD) (DRAINAGE_DATABASE.TXT):
!> dan * IS NO LONGER USED.  DRAINAGE_DATABASE.TXT HAS BEEN REPLACED WITH
!> dan * THE BASIN SHD FILE.  READ_SHED_EF, FROM STAND-ALONE RTE.EXE
!> dan * (WATROUTE), IS CALLED TO READ THE NEW FILE.
      IF (SHDFILEFLAG == 1) THEN
      ALLOCATE (FLN(999))  !declared in area_watflood module
      FLN(1) = 'MESH_drainage_database.r2c'

!+      OPEN(UNIT=12,FILE='event/event.evt',STATUS='OLD',IOSTAT=IOS_EVT)
      OPEN(UNIT=20,FILE='MESH_drainage_database.r2c',
     + STATUS='OLD',IOSTAT=IOS)
      IF (IOS == 0) THEN
        !CLOSE(12)
        CLOSE(20)


!> these settings are for producing input files for the new watroute
      FLN(31) = 'WR_runoff.r2c'
      FLN(32) = 'WR_recharge.r2c'


      PRINT*,"Reading Drainage Database from MESH_drainage_database.r2c"

        OPEN(UNIT=51,FILE='nul')
        OPEN(UNIT=98,FILE='1234500124572321.1265489')
        CALL READ_SHED_EF (31, 1)
        CLOSE(98,STATUS='delete')
        CLOSE(51)
        WRITE (6, *) " READ: SUCCESSFUL, FILE: CLOSED"
!+        ALLOCATE (FRAC(NA),
!>
!>*******************************************************************
!>
        ALLOCATE (WF_IBN(NA), WF_IROUGH(NA),
     +  WF_ICHNL(NA), WF_NEXT(NA), WF_ELEV(NA), WF_IREACH(NA),
     +  WF_DA(NA), WF_BNKFLL(NA), WF_CHANNELSLOPE(NA),
     +  BASIN_FRACTION(NA))
!+        ALLOCATE (WF_NHYD(NA), WF_QR(NA),
!+     +  WF_QBASE(NA), WF_QI2(NA), WF_QO1(NA), WF_QO2(NA),
!+     +  WF_STORE1(NA), WF_STORE2(NA), WF_QI1(NA), SNOGRD(NA),
!+     +  FSDOWN(NA))
        DO I =1, NA
          WF_IBN(I) = ibn(I)
          WF_IROUGH(I) = 1 !irough does not seem
                            !to be used in new WATROUTE
          WF_ICHNL(I) = ichnl(I)
          WF_NEXT(I) = next(I)
          WF_ELEV(I) = elev(I)
          WF_IREACH(I) = ireach(I)
          WF_DA(I) = da(I)
          WF_BNKFLL(I) = bnkfll(I)
          WF_CHANNELSLOPE(I) = slope(I)

        ENDDO

        WF_IYMAX = IYMAX
        WF_JXMAX = JXMAX
        BASIN_FRACTION(1) = -1

        ILG = NA*NTYPE
      ELSE
        PRINT *, 'ERROR with event.evt or new_shd.r2c'
        STOP
      ENDIF
      ELSE IF(SHDFILEFLAG == 0)THEN

!> *********************************************************************
!> Open and read in values from MESH_input_drainage_database.txt file
!>   if new_shd.r2c file was not found
!> *********************************************************************
        OPEN(UNIT=20,FILE='MESH_input_drainage_database.txt',
     +       STATUS='OLD', IOSTAT=IOS)
        IF (IOS == 0) THEN
          PRINT *, "Reading Drainage Database from ",
     +      "MESH_input_drainage_database.txt"
        ELSE
          PRINT *, 'MESH_input_drainage_database.txt not found'
          STOP
        ENDIF
        READ(20,'(I5,50X,I5)') NA,NAA
        READ(20,'(F10.0,5X,2I5)') AL,NRVR,NTYPE
        GRDN=0.0
        GRDE=0.0

!todo change the code or documentation on these variables.
!ANDY Set ILG from the read-in values
        ILG = NA*NTYPE

!> Using IOSTAT allows us to try to read input that may or may not exist.
!> If all of the values successfully get read, IOSTAT=VarName will set 
!> VarName to 0. If all of the values were not successfully read, 
!> VarName would be set to 1 or more. In this case, the VarName that
!> we are using is IOS.
        READ(20,'(12I5,2F5.0)',IOSTAT=IOS) IYMIN,WF_IYMAX,
     +    JXMIN,WF_JXMAX,LATDEGMIN,LATMINMIN,LATDEGMAX,LATMINMAX,
     +    LONDEGMIN,LONMINMIN,LONDEGMAX,LONMINMAX,GRDN,GRDE

!> Condition for Lat/Long by Frank S Sept/1999
        IF( GRDN>0.0 ) THEN
          IYMIN=LATDEGMIN*60+LATMINMIN
          WF_IYMAX=LATDEGMAX*60+LATMINMAX
          JXMIN=LONDEGMIN*60+LONMINMIN
          WF_JXMAX=LONDEGMAX*60+LONMINMAX

        ELSE
!> Define GRDN & GRDE for UTM
          GRDN=AL/1000.
          GRDE=AL/1000.
        ENDIF 
        READ(20,'(2I5)') YCOUNT, XCOUNT

!> check if we are going to get an "array bounds out of range" error
        IF(YCOUNT>100) THEN
        WRITE (6, *) 'WARNING: The height of the basin is very high. ',
     *    'This may negatively impact performance.'
!+          PRINT *, 'size of grid arrays in MESH: ',M_Y
!+          PRINT *, 'number up/down (north/south) ',
!+     +             'grids from MESH_drainage_database.txt'
!+	    PRINT *, ' file: ',YCOUNT
!+          PRINT *, 'Please adjust these values.'
!+          STOP
	  ENDIF

        IF(XCOUNT>100) THEN
        WRITE (6, *) 'WARNING: The width of the basin is very high. ',
     *    'This may negatively impact performance.'
!+          PRINT *, 'size of grid arrays in MESH: ',M_X
!+          PRINT *, 'no. of east/west (left/right) grids from ',
!+     +             'MESH_drainage_database.txt'
!+	    PRINT *, ' file: ',XCOUNT
!+          PRINT *, 'Please adjust these values.'
!+          STOP
	  ENDIF


!ANDY Allocation of variables that use NA and NTYPE
        ALLOCATE(WF_IBN(NA), WF_IROUGH(NA),
     +    WF_ICHNL(NA), WF_NEXT(NA), WF_ELEV(NA), WF_IREACH(NA),
     +    WF_DA(NA), WF_BNKFLL(NA), WF_CHANNELSLOPE(NA),
     +    FRAC(NA), BASIN_FRACTION(NA))
        ALLOCATE(ACLASS(NA,NTYPE))

!ANDY Zero everything we just allocated
      DO I=1,NA
        DO J=1,NTYPE
          ACLASS(I,J) = 0
        ENDDO
      ENDDO
        DO I=1,NA
          WF_IBN(I) = 0
          WF_IROUGH(I) = 0
          WF_ICHNL(I) = 0
          WF_NEXT(I) = 0
          WF_ELEV(I) = 0
          WF_IREACH(I) = 0
          WF_DA(I) = 0
          WF_BNKFLL(I) = 0
          WF_CHANNELSLOPE(I) = 0
          FRAC(I) = 0
          BASIN_FRACTION(I) = 0
        ENDDO

        !Set this to ensure basin fraction will be set later on
        BASIN_FRACTION(1) = -1


        ALLOCATE(YYY(NA),XXX(NA))



        DO I = 1, YCOUNT
          READ(20,*)
        ENDDO

        DO I = 1,NA
          READ(20,'(5X,2I5,3F10.5,I7,5I5,F5.2,15F5.2)')YYY(I),
     +      XXX(I),WF_DA(I),WF_BNKFLL(I),WF_CHANNELSLOPE(I),
     +      WF_ELEV(I),WF_IBN(I),WF_IROUGH(I),WF_ICHNL(I),
     +      WF_NEXT(I),WF_IREACH(I),FRAC(I), 
     +      (ACLASS(I,J),J=1,NTYPE)
!> check to make sure land cover areas sum to 100%
          WF_LAND_COUNT=1
          WF_LAND_MAX=0.0
          WF_LAND_SUM=0.0
          DO J=1,NTYPE
            WF_LAND_SUM=WF_LAND_SUM+ACLASS(I,J)
            IF(ACLASS(I,J)>WF_LAND_MAX) THEN
              WF_LAND_COUNT=J
    	        WF_LAND_MAX=ACLASS(I,J)
            ENDIF
          ENDDO
	    IF(WF_LAND_SUM/=1.0)THEN
	      ACLASS(I,WF_LAND_COUNT)=
     +      ACLASS(I,WF_LAND_COUNT)-(WF_LAND_SUM-1.0)
          ENDIF
        ENDDO

        CLOSE(UNIT=20)

      ENDIF ! IF SHDFILE...
	  
      
      !*   ACLASS: PERCENT-GRU FRACTION FOR EACH GRID SQUARE (WF_ACLASS)
!The following are used in read_soil_ini
!wc_thpor, wc_thlret,wc_thlmin,wc_bi,    wc_psisat,
!wc_grksat,wc_hcps,  wc_tcs,   wc_algwet,wc_algdry
       ALLOCATE(sv%wc_algwet(NA,NTYPE), sv%wc_algdry(NA,NTYPE))
       ALLOCATE(sv%wc_thpor(NA,NTYPE,IGND), sv%wc_thlret(NA,NTYPE,IGND),
     +    sv%wc_thlmin(NA,NTYPE,IGND), sv%wc_bi(NA,NTYPE,IGND),
     +    sv%wc_psisat(NA,NTYPE,IGND), sv%wc_grksat(NA,NTYPE,IGND),
     +    sv%wc_hcps(NA,NTYPE,IGND), sv%wc_tcs(NA,NTYPE,IGND))

!ANDY Zero everything we just allocated
        DO I=1,NA
          DO J=1,NTYPE
            DO K=1,IGND
              sv%wc_thpor(I,J,K) = 0
              sv%wc_thlret(I,J,K) = 0
              sv%wc_thlmin(I,J,K) = 0
              sv%wc_bi(I,J,K) = 0
              sv%wc_psisat(I,J,K) = 0
              sv%wc_grksat(I,J,K) = 0
              sv%wc_hcps(I,J,K) = 0
              sv%wc_tcs(I,J,K) = 0
            ENDDO
            sv%wc_algwet(I,J) = 0
            sv%wc_algdry(I,J) = 0
          ENDDO
        ENDDO
      
      
      

      IF (XCOUNT .GT. 100) THEN
        WRITE (6, *) 'WARNING: The width of the basin is very high. ',
     *    'This may negatively impact performance.'
      END IF
      IF (YCOUNT .GT. 100) THEN
        WRITE (6, *) 'WARNING: The height of the basin is very high. ',
     *    'This may negatively impact performance.'
      END IF
      IF (ILG > 1500) THEN
        WRITE (6, *) 'WARNING: The number of grid squares in the basin',
     *    ' is very high. This may negatively impact performance.'
      END IF

!> CHECK THAT GRID OUTPUT POINTS ARE IN THE BASIN
      DO I = 1, WF_NUM_POINTS
        IF (op%N_OUT(I) .GT. NA) THEN !IF EXISTS IN BASIN
            WRITE (6, *)
            WRITE (6, *)
            WRITE (6, *) "Grids from basin watershed file: ", NA
            WRITE (6, *) "Grid output point ", I, " is in Grid: ",
     1          op%N_OUT(I)
            WRITE (6, *) "Please adjust this grid output point in ",
     1          "MESH_input_run_options.ini"
            STOP
        END IF
      END DO
!>
!>*******************************************************************
!>
!> *********************************************************************
!> Open and read in values from MESH_input_soil_levels.txt file
!> *********************************************************************
      ALLOCATE (sl%DELZ(IGND), sl%ZBOT(IGND))
      CALL READ_SOIL_LEVELS(IGND, sl)

      CALL READ_CHECK_FORCING_FILES(NUM_CSV, NUM_R2C,NUM_SEQ,NA,cm,ts)

      ALLOCATE(
     + cp%ZRFMGRD(NA), cp%ZRFHGRD(NA), cp%ZBLDGRD(NA), cp%GCGRD(NA))

      ALLOCATE (cp%FCANROW(NA, NTYPE, ICAN+1),
     + cp%LNZ0ROW(NA, NTYPE, ICAN+1),
     + cp%ALVCROW(NA, NTYPE, ICAN+1),
     + cp%ALICROW(NA, NTYPE, ICAN+1))

      ALLOCATE (
     + cp%PAMXROW(NA, NTYPE, ICAN), cp%PAMNROW(NA, NTYPE, ICAN),
     + cp%CMASROW(NA, NTYPE, ICAN), cp%ROOTROW(NA, NTYPE, ICAN),
     + cp%RSMNROW(NA, NTYPE, ICAN), cp%QA50ROW(NA, NTYPE, ICAN),
     + cp%VPDAROW(NA, NTYPE, ICAN), cp%VPDBROW(NA, NTYPE, ICAN),
     + cp%PSGAROW(NA, NTYPE, ICAN), cp%PSGBROW(NA, NTYPE, ICAN))

      ALLOCATE (
     + cp%DRNROW(NA,NTYPE),  cp%SDEPROW(NA,NTYPE),
     + cp%FAREROW(NA,NTYPE), cp%DDROW(NA,NTYPE),
     + cp%XSLPROW(NA,NTYPE), cp%XDROW(NA,NTYPE),
     + cp%MANNROW(NA,NTYPE), cp%KSROW(NA,NTYPE),
     + cp%TCANROW(NA,NTYPE), cp%TSNOROW(NA,NTYPE),
     + cp%TPNDROW(NA,NTYPE), cp%ZPNDROW(NA,NTYPE),
     + cp%RCANROW(NA,NTYPE), cp%SCANROW(NA,NTYPE),
     + cp%SNOROW(NA,NTYPE),  cp%ALBSROW(NA,NTYPE),
     + cp%RHOSROW(NA,NTYPE), cp%GROROW(NA,NTYPE))

      ALLOCATE (cp%MIDROW(NA,NTYPE))

      ALLOCATE (
     + cp%SANDROW(NA, NTYPE, IGND), cp%CLAYROW(NA, NTYPE, IGND),
     + cp%ORGMROW(NA, NTYPE, IGND), cp%TBARROW(NA, NTYPE, IGND),
     + cp%THLQROW(NA, NTYPE, IGND), cp%THICROW(NA, NTYPE, IGND))
!>
!>*******************************************************************
!>
      CALL READ_PARAMETERS_CLASS(
     +  TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6,
     +  NAME1, NAME2, NAME3, NAME4, NAME5, NAME6,
     +  PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6,
     +  NA,ILW, NLTEST, NMTEST,
     +  IGND, JLAT, ICAN, NTYPE,
     +  DEGLAT, DEGLON,
     +  HOURLY_START_DAY,  HOURLY_STOP_DAY,
     +  DAILY_START_DAY,   DAILY_STOP_DAY,
     +  HOURLY_START_YEAR, HOURLY_STOP_YEAR,
     +  DAILY_START_YEAR,  DAILY_STOP_YEAR,
     +  IHOUR, IMIN, IDAY, IYEAR, cp, fls)
!>
!>*******************************************************************
!>
      CALL READ_SOIL_INI(NTYPE, IGND, NTYPE, NA, sv)
!>
!>*******************************************************************
!>
!>  ==============================
!> *********************************************************************
!> Open and read INITIAL SOIL MOISTURE AND SOIL TEMPERATURE values
!> when data is available
!> files: S_moisture.txt : soil moisture in layer 1, 2 and 3
!> files: T_temperature.txt : soil temperature in layer 1, 2 and 3
!> *********************************************************************
!>  FOR INITIAL SOIL MOISTURE AND SOIL TEMPERATURE
!>  Saul M. feb 26 2008

!todo - test this piece of code and make sure we understand how it works.
!todo - if we implement this, make it an option for the user to select GRU or grid initialization
      CALL READ_S_MOISTURE_TXT(
     + IGND, YCOUNT, XCOUNT, na, NTYPE,
     + YYY, XXX, cp%THLQROW )

      CALL READ_S_TEMPERATURE_TXT(
     + IGND, YCOUNT, XCOUNT, na, NTYPE,
     + YYY, XXX, cp%TBARROW )
!>
!>*******************************************************************
!>
!> Read the mesh_parameters_hydrology.ini file
      ALLOCATE(hp%ZSNLROW(NA, NTYPE), hp%ZPLGROW(NA, NTYPE),
     +         hp%ZPLSROW(NA, NTYPE), hp%FRZCROW(NA,NTYPE),
     +         hp%CMAXROW(NA, NTYPE), hp%CMINROW(NA, NTYPE),
     +         hp%BROW   (NA, NTYPE), hp%K1ROW  (NA, NTYPE),
     +         hp%K2ROW  (NA, NTYPE),
     +         hp%fetchROW(NA, NTYPE),hp%HtROW(NA, NTYPE),
     +         hp%N_SROW(NA, NTYPE),hp%A_SROW(NA, NTYPE),
     +         hp%DistribROW(NA, NTYPE))
      
      NYEARS = IYEAR_END - IYEAR_START + 1
      ALLOCATE (t0_ACC(NYEARS))
      t0_ACC = 0.0

      CALL READ_PARAMETERS_HYDROLOGY(INDEPPAR, DEPPAR,
     +    RELEASE, WF_R2, hp, M_C, NA, NTYPE,
     +    SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, fls)

      RETURN
      END SUBROUTINE READ_INITIAL_INPUTS
