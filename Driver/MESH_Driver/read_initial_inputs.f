      subroutine READ_INITIAL_INPUTS(
!> GENERIC VARIABLES
     +  RELEASE,
!> VARIABLES FOR READ_RUN_OPTIONS
     +  IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  ICTEMMOD, IOS, PAS, N, IROVAL, WF_NUM_POINTS,
!     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START,
!     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END,
     +  IRONAME, GENDIR_OUT,
!> variables for drainage database or new_shd
     + IGND, ILG, WF_IYMAX, WF_JXMAX,
     + WF_LAND_COUNT,
     + LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX,
     + LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX,
     + WF_LAND_MAX, WF_LAND_SUM,
!> variables for READ_CHECK_FORCING_FILES
!     + NUM_CSV, NUM_R2C, NUM_SEQ,
!> variables for READ_PARAMETERS_CLASS
     +  TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6,
     +  NAME1, NAME2, NAME3, NAME4, NAME5, NAME6,
     +  PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6,
     +  ILW, NLTEST, NMTEST, JLAT, ICAN,
     +  DEGLAT, DEGLON,
     +  HOURLY_START_DAY, HOURLY_STOP_DAY,
     +  DAILY_START_DAY, DAILY_STOP_DAY,
     +  HOURLY_START_YEAR, HOURLY_STOP_YEAR,
     +  DAILY_START_YEAR, DAILY_STOP_YEAR,
!     +  IHOUR, IMIN, IDAY, IYEAR,
!> variables for READ_SOIL_INI
!> variables for READ_PARAMETERS_HYDROLOGY
     +  INDEPPAR, DEPPAR, WF_R2, M_C,
!> the types that are to be allocated and initialised
     +  bi, op, sl, cp, sv, hp, ts, cm,
     +  SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, fls)

      use sa_mesh_shared_variabletypes
      use model_files_variabletypes
      use MESH_INPUT_MODULE
      use FLAGS
      use AREA_WATFLOOD
      use climate_forcing
      use model_dates
!      USE model_files

      implicit none

!> DECLARE THE READ IN VARIABLES.
!> ----------------------------
!> VALUES NEEDED FOR READ_RUN_OPTIONS
      integer IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  ICTEMMOD, IOS, PAS, N, IROVAL, WF_NUM_POINTS
!     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START,
!     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END
      character(20) IRONAME
      character(10) GENDIR_OUT
!> declared MESH_INPUT_MODULE:
!>     op%N_OUT, op%II_OUT, op%DIR_OUT

!> END OF VALUES FOR READ_RUN_OPTIONS
!> -----------------------------
!> VALUES NEEDED for drainage_database and/or new_shd.r2c
      integer IGND, ILG, WF_IYMAX, WF_JXMAX,
     +  WF_LAND_COUNT,
     +  LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX,
     +  LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX
      real WF_LAND_MAX, WF_LAND_SUM
      character(500) fl_listMesh
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
      integer i, j, k
!> END OF VALUES NEEDED for drainagedatabase of new_shd.r2c
!> -----------------------------
!> values needed for READ_CHECK_FORCING_FILES
!      INTEGER :: NUM_CSV, NUM_R2C,NUM_SEQ
!> values that were declared earlier:
!>  BASINSHORTWAVEFLAG, BASINLONGWAVEFLAG,
!>  BASINTEMPERATUREFLAG, BASINRAINFLAG, BASINWINDFLAG,
!>  BASINPRESFLAG, BASINHUMIDITYFLAG
!> -----------------------------
!> values needed for READ_PARAMETERS_CLASS
      character(4)
     +  TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6,
     +  NAME1, NAME2, NAME3, NAME4, NAME5, NAME6,
     +  PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
      integer ILW, NLTEST, NMTEST,
     +  JLAT, ICAN,
     +  HOURLY_START_DAY, HOURLY_STOP_DAY,
     +  DAILY_START_DAY, DAILY_STOP_DAY,
     +  HOURLY_START_YEAR, HOURLY_STOP_YEAR,
     +  DAILY_START_YEAR, DAILY_STOP_YEAR,
     +  IHOUR, IMIN, IDAY, IYEAR
      real DEGLAT, DEGLON
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
      integer INDEPPAR, DEPPAR, M_C
      real SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS
      real WF_R2(M_C)
!> Values already declared above:
!>  NTYPE, NA, RELFLG
!> -----------------------------
!> VALUES FOR MANY INPUT FILES
      character(8) RELEASE(10)

!> The types that contain allocatable values
      type(OutputPoints) :: op
!+     TYPE(ShedInformation) :: si
      type(SoilLevels) :: sl
      type(ClassParameters) :: cp
      type(SoilValues) :: sv
      type(HydrologyParameters) :: hp
      type(basin_info) :: bi
      type(CLIM_INFO) :: cm
      type(dates_model) :: ts
      type(fl_ids):: fls

!> DECLARE THE LOCAL VARIABLES

!> ====================================
!> read the RUN_OPTIONS input file called "MESH_input_run_options.ini"
!> and SET or RESET any CONTROL FLAGS
!> and READ the GRID OUTPUT DIRECTORIES.
      call READ_RUN_OPTIONS(
     +  IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  ICTEMMOD, IOS, PAS, N,
     +  IROVAL, WF_NUM_POINTS,
!     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START,
!     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END,
     +  IRONAME, GENDIR_OUT, op, ts, cm, fls)

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
      if (SHDFILEFLAG == 1) then
        allocate(FLN(999))  !declared in area_watflood module
        FLN(1) = 'MESH_drainage_database.r2c'

!+      OPEN(UNIT=12,FILE='event/event.evt',STATUS='OLD',IOSTAT=IOS_EVT)
        open(UNIT=20, FILE='MESH_drainage_database.r2c',
     +    STATUS='OLD', IOSTAT=IOS)
        if (IOS == 0) then
        !CLOSE(12)
          close(20)

!> these settings are for producing input files for the new watroute
          FLN(31) = 'WR_runoff.r2c'
          FLN(32) = 'WR_recharge.r2c'

          print *,
     +      'Reading Drainage Database from MESH_drainage_database.r2c'

          open(UNIT=98, FILE='1234500124572321.1265489')
          call READ_SHED_EF(31, 1)
          close(98, STATUS='delete')
          write(6, *) ' READ: SUCCESSFUL, FILE: CLOSED'
!+        ALLOCATE(FRAC(NA),

          !> Initialize basin information variable.
          bi%xorigin = xorigin
          bi%yorigin = yorigin
          bi%AL = al
          bi%xdelta = xdelta
          bi%ydelta = ydelta
          bi%NA = NA
          bi%xcount = xcount
          bi%ycount = ycount
          allocate(bi%XXX(size(XXX)), bi%YYY(size(YYY)))
          bi%XXX = XXX
          bi%YYY = YYY
          bi%NTYPE = NTYPE
          allocate(bi%ACLASS(size(ACLASS, 1), size(ACLASS, 2)))
          bi%ACLASS = ACLASS
          bi%ILG = NA*NTYPE

!>
!>*******************************************************************
!>
          allocate(WF_IBN(NA), WF_IROUGH(NA),
     +      WF_ICHNL(NA), WF_NEXT(NA), WF_ELEV(NA), WF_IREACH(NA),
     +      WF_DA(NA), WF_BNKFLL(NA), WF_CHANNELSLOPE(NA),
     +      BASIN_FRACTION(NA))
!+        ALLOCATE (WF_NHYD(NA), WF_QR(NA),
!+     +  WF_QBASE(NA), WF_QI2(NA), WF_QO1(NA), WF_QO2(NA),
!+     +  WF_STORE1(NA), WF_STORE2(NA), WF_QI1(NA), SNOGRD(NA),
!+     +  FSDOWN(NA))

          do i = 1, NA
            WF_IBN(i) = ibn(i)
            WF_IROUGH(i) = 1 !irough does not seem
                            !to be used in new WATROUTE
            WF_ICHNL(i) = ichnl(i)
            WF_NEXT(i) = next(i)
            WF_ELEV(i) = elev(i)
            WF_IREACH(i) = ireach(i)
            WF_DA(i) = da(i)
            WF_BNKFLL(i) = bnkfll(i)
            WF_CHANNELSLOPE(i) = slope(i)
          end do

          WF_IYMAX = IYMAX
          WF_JXMAX = JXMAX
          BASIN_FRACTION(1) = -1
          ILG = NA*NTYPE

        else
          print *, 'ERROR with event.evt or new_shd.r2c'
          stop
        end if

      else if (SHDFILEFLAG == 0) then

!> *********************************************************************
!> Open and read in values from MESH_input_drainage_database.txt file
!>   if new_shd.r2c file was not found
!> *********************************************************************
        open(UNIT=20, FILE='MESH_input_drainage_database.txt',
     +    STATUS='OLD', IOSTAT=IOS)
        if (IOS == 0) then
          print *, 'Reading Drainage Database from ',
     +      'MESH_input_drainage_database.txt'
        else
          print *, 'MESH_input_drainage_database.txt not found'
          stop
        end if
        read(20, '(i5, 50x, i5)') NA, NAA
        read(20, '(f10.0, 5x, 2i5)') AL, NRVR, NTYPE
        GRDN = 0.0
        GRDE = 0.0

!todo change the code or documentation on these variables.
!ANDY Set ILG from the read-in values
        ILG = NA*NTYPE

!> Using IOSTAT allows us to try to read input that may or may not exist.
!> If all of the values successfully get read, IOSTAT=VarName will set
!> VarName to 0. If all of the values were not successfully read,
!> VarName would be set to 1 or more. In this case, the VarName that
!> we are using is IOS.
        read(20, '(12i5, 2f5.0)', IOSTAT=IOS) IYMIN, WF_IYMAX,
     +    JXMIN, WF_JXMAX, LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX,
     +    LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX, GRDN, GRDE

!> Condition for Lat/Long by Frank S Sept/1999
        if (GRDN > 0.0) then
          IYMIN = LATDEGMIN*60 + LATMINMIN
          WF_IYMAX = LATDEGMAX*60 + LATMINMAX
          JXMIN = LONDEGMIN*60 + LONMINMIN
          WF_JXMAX = LONDEGMAX*60 + LONMINMAX

        else
!> Define GRDN & GRDE for UTM
          GRDN = AL/1000.0
          GRDE = AL/1000.0
        end if
        read(20, '(2i5)') YCOUNT, XCOUNT

!> check if we are going to get an "array bounds out of range" error
        if (YCOUNT > 100) then
          write(6, *) 'WARNING: The height of the basin is very high.',
     *      'This may negatively impact performance.'
!+          PRINT *, 'size of grid arrays in MESH: ',M_Y
!+          PRINT *, 'number up/down (north/south) ',
!+     +             'grids from MESH_drainage_database.txt'
!+     PRINT *, ' file: ',YCOUNT
!+          PRINT *, 'Please adjust these values.'
!+          STOP
        end if

        if (XCOUNT > 100) then
          write(6, *) 'WARNING: The width of the basin is very high. ',
     *     'This may negatively impact performance.'
!+          PRINT *, 'size of grid arrays in MESH: ',M_X
!+          PRINT *, 'no. of east/west (left/right) grids from ',
!+     +             'MESH_drainage_database.txt'
!+     PRINT *, ' file: ',XCOUNT
!+          PRINT *, 'Please adjust these values.'
!+          STOP
        end if

!ANDY Allocation of variables that use NA and NTYPE
        allocate(WF_IBN(NA), WF_IROUGH(NA),
     +    WF_ICHNL(NA), WF_NEXT(NA), WF_ELEV(NA), WF_IREACH(NA),
     +    WF_DA(NA), WF_BNKFLL(NA), WF_CHANNELSLOPE(NA),
     +    FRAC(NA), BASIN_FRACTION(NA))
        allocate(ACLASS(NA, NTYPE))

!ANDY Zero everything we just allocated
        do i = 1, NA
          do j = 1, NTYPE
            ACLASS(i, j) = 0
          end do
        end do
        do i = 1, NA
          WF_IBN(i) = 0
          WF_IROUGH(i) = 0
          WF_ICHNL(i) = 0
          WF_NEXT(i) = 0
          WF_ELEV(i) = 0
          WF_IREACH(i) = 0
          WF_DA(i) = 0
          WF_BNKFLL(i) = 0
          WF_CHANNELSLOPE(i) = 0
          FRAC(i) = 0
          BASIN_FRACTION(i) = 0
        end do

        !Set this to ensure basin fraction will be set later on
        BASIN_FRACTION(1) = -1

        allocate(YYY(NA), XXX(NA))

        do i = 1, YCOUNT
          read(20, *)
        end do

        do i = 1, NA
          read(20, '(5x, 2i5, 3f10.5, i7, 5i5, f5.2, 15f5.2)') YYY(i),
     +      XXX(i), WF_DA(i), WF_BNKFLL(i), WF_CHANNELSLOPE(i),
     +      WF_ELEV(i), WF_IBN(i), WF_IROUGH(i), WF_ICHNL(i),
     +      WF_NEXT(i), WF_IREACH(i), FRAC(i),
     +      (ACLASS(i, j), j = 1, NTYPE)
!> check to make sure land cover areas sum to 100%
          WF_LAND_COUNT = 1
          WF_LAND_MAX = 0.0
          WF_LAND_SUM = 0.0
          do j = 1, NTYPE
            WF_LAND_SUM = WF_LAND_SUM + ACLASS(i, j)
            if (ACLASS(i, j) > WF_LAND_MAX) then
              WF_LAND_COUNT = j
              WF_LAND_MAX = ACLASS(i, j)
            end if
          end do
          if (WF_LAND_SUM /= 1.0) THEN
            ACLASS(i, WF_LAND_COUNT) =
     +        ACLASS(i, WF_LAND_COUNT) - (WF_LAND_SUM - 1.0)
          end if
        end do

        close(20)

      end if ! IF SHDFILE...

      !*   ACLASS: PERCENT-GRU FRACTION FOR EACH GRID SQUARE (WF_ACLASS)
!The following are used in read_soil_ini
!wc_thpor, wc_thlret,wc_thlmin,wc_bi,    wc_psisat,
!wc_grksat,wc_hcps,  wc_tcs,   wc_algwet,wc_algdry
      allocate(sv%wc_algwet(NA, NTYPE), sv%wc_algdry(NA, NTYPE))
      allocate(sv%wc_thpor(NA, NTYPE, IGND),
     +  sv%wc_thlret(NA, NTYPE, IGND),
     +  sv%wc_thlmin(NA, NTYPE, IGND), sv%wc_bi(NA, NTYPE, IGND),
     +  sv%wc_psisat(NA, NTYPE, IGND), sv%wc_grksat(NA, NTYPE, IGND),
     +  sv%wc_hcps(NA, NTYPE, IGND), sv%wc_tcs(NA, NTYPE, IGND))

!ANDY Zero everything we just allocated
      do i = 1, NA
        do j = 1, NTYPE
          do k = 1, IGND
            sv%wc_thpor(i, j, k) = 0
            sv%wc_thlret(i, j, k) = 0
            sv%wc_thlmin(i, j, k) = 0
            sv%wc_bi(i, j, k) = 0
            sv%wc_psisat(i, j, k) = 0
            sv%wc_grksat(i, j, k) = 0
            sv%wc_hcps(i, j, k) = 0
            sv%wc_tcs(i, j, k) = 0
          end do
          sv%wc_algwet(i, j) = 0
          sv%wc_algdry(i, j) = 0
        end do
      end do

      if (XCOUNT > 100) then
        write(6, *) 'WARNING: The width of the basin is very high. ',
     *    'This may negatively impact performance.'
      end if
      if (YCOUNT > 100) then
        write(6, *) 'WARNING: The height of the basin is very high. ',
     *    'This may negatively impact performance.'
      end if
      if (ILG > 1500) then
        write(6, *) 'WARNING: The number of grid squares in the basin',
     *    ' is very high. This may negatively impact performance.'
      end if

!> CHECK THAT GRID OUTPUT POINTS ARE IN THE BASIN
      do i = 1, WF_NUM_POINTS
        if (op%N_OUT(i) > NA) then !IF EXISTS IN BASIN
          write(6, *)
          write(6, *)
          write(6, *) 'Grids from basin watershed file: ', NA
          write(6, *) 'Grid output point ', i, ' is in Grid: ',
     1      op%N_OUT(i)
          write(6, *) 'Please adjust this grid output point in ',
     1      'MESH_input_run_options.ini'
          stop
        end if
      end do
!>
!>*******************************************************************
!>
!> *********************************************************************
!> Open and read in values from MESH_input_soil_levels.txt file
!> *********************************************************************
      allocate(sl%DELZ(IGND), sl%ZBOT(IGND))
      call READ_SOIL_LEVELS(IGND, sl, fls)

      bi%ignd = IGND

      allocate(
     +  cp%ZRFMGRD(NA), cp%ZRFHGRD(NA), cp%ZBLDGRD(NA), cp%GCGRD(NA))

      allocate(cp%FCANROW(NA, NTYPE, ICAN + 1),
     +  cp%LNZ0ROW(NA, NTYPE, ICAN + 1),
     +  cp%ALVCROW(NA, NTYPE, ICAN + 1),
     +  cp%ALICROW(NA, NTYPE, ICAN + 1))

      allocate(
     +  cp%PAMXROW(NA, NTYPE, ICAN), cp%PAMNROW(NA, NTYPE, ICAN),
     +  cp%CMASROW(NA, NTYPE, ICAN), cp%ROOTROW(NA, NTYPE, ICAN),
     +  cp%RSMNROW(NA, NTYPE, ICAN), cp%QA50ROW(NA, NTYPE, ICAN),
     +  cp%VPDAROW(NA, NTYPE, ICAN), cp%VPDBROW(NA, NTYPE, ICAN),
     +  cp%PSGAROW(NA, NTYPE, ICAN), cp%PSGBROW(NA, NTYPE, ICAN))

      allocate(
     +  cp%DRNROW(NA, NTYPE),  cp%SDEPROW(NA, NTYPE),
     +  cp%FAREROW(NA, NTYPE), cp%DDROW(NA, NTYPE),
     +  cp%XSLPROW(NA, NTYPE), cp%XDROW(NA, NTYPE),
     +  cp%MANNROW(NA, NTYPE), cp%KSROW(NA, NTYPE),
     +  cp%TCANROW(NA, NTYPE), cp%TSNOROW(NA, NTYPE),
     +  cp%TPNDROW(NA, NTYPE), cp%ZPNDROW(NA, NTYPE),
     +  cp%RCANROW(NA, NTYPE), cp%SCANROW(NA, NTYPE),
     +  cp%SNOROW(NA, NTYPE),  cp%ALBSROW(NA, NTYPE),
     +  cp%RHOSROW(NA, NTYPE), cp%GROROW(NA, NTYPE))

      allocate(cp%MIDROW(NA, NTYPE))

      allocate(
     +  cp%SANDROW(NA, NTYPE, IGND), cp%CLAYROW(NA, NTYPE, IGND),
     +  cp%ORGMROW(NA, NTYPE, IGND), cp%TBARROW(NA, NTYPE, IGND),
     +  cp%THLQROW(NA, NTYPE, IGND), cp%THICROW(NA, NTYPE, IGND))
!>
!>*******************************************************************
!>
      call READ_PARAMETERS_CLASS(
     +  TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6,
     +  NAME1, NAME2, NAME3, NAME4, NAME5, NAME6,
     +  PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6,
     +  NA,ILW, NLTEST, NMTEST,
     +  IGND, JLAT, ICAN, NTYPE,
     +  DEGLAT, DEGLON,
     +  HOURLY_START_DAY, HOURLY_STOP_DAY,
     +  DAILY_START_DAY, DAILY_STOP_DAY,
     +  HOURLY_START_YEAR, HOURLY_STOP_YEAR,
     +  DAILY_START_YEAR, DAILY_STOP_YEAR,
     +  IHOUR, IMIN, IDAY, IYEAR,
     +  cp, fls)

!>    Copy the starting date of input forcing data from CLASS.ini
!>    to the climate variable.
      cm%start_date%year = IYEAR
      cm%start_date%jday = IDAY
      cm%start_date%hour = IHOUR
      cm%start_date%mins = IMIN

!>    Set the starting date to that of the forcing data if none is
!>    provided and intialize the current time-step.
      if (YEAR_START == 0 .and. JDAY_START == 0 .and.
     +  MINS_START == 0 .and. HOUR_START == 0) then
        YEAR_START = cm%start_date%year
        JDAY_START = cm%start_date%jday
        HOUR_START = cm%start_date%hour
        MINS_START = cm%start_date%mins
      end if
      YEAR_NOW = YEAR_START
      JDAY_NOW = JDAY_START
      HOUR_NOW = HOUR_START
      MINS_NOW = MINS_START
      TIME_STEP_NOW = MINS_START

!>
!>*******************************************************************
!>
      call READ_SOIL_INI(NTYPE, IGND, NTYPE, NA, sv)
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
      call READ_S_MOISTURE_TXT(
     + IGND, YCOUNT, XCOUNT, na, NTYPE,
     + YYY, XXX, cp%THLQROW)

      call READ_S_TEMPERATURE_TXT(
     + IGND, YCOUNT, XCOUNT, na, NTYPE,
     + YYY, XXX, cp%TBARROW)
!>
!>*******************************************************************
!>
!> Read the mesh_parameters_hydrology.ini file
      allocate(hp%ZSNLROW(NA, NTYPE), hp%ZPLGROW(NA, NTYPE),
     +  hp%ZPLSROW(NA, NTYPE), hp%FRZCROW(NA, NTYPE),
     +  hp%CMAXROW(NA, NTYPE), hp%CMINROW(NA, NTYPE),
     +  hp%BROW(NA, NTYPE), hp%K1ROW(NA, NTYPE),
     +  hp%K2ROW(NA, NTYPE),
     +  hp%fetchROW(NA, NTYPE), hp%HtROW(NA, NTYPE),
     +  hp%N_SROW(NA, NTYPE), hp%A_SROW(NA, NTYPE),
     +  hp%DistribROW(NA, NTYPE))

      NYEARS = YEAR_STOP - YEAR_START + 1
      allocate(t0_ACC(NYEARS))
      t0_ACC = 0.0

      call READ_PARAMETERS_HYDROLOGY(INDEPPAR, DEPPAR,
     +  RELEASE, WF_R2, hp, M_C, NA, NTYPE,
     +  SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, fls)

      return

      end subroutine
