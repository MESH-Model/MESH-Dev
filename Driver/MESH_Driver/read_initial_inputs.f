      subroutine READ_INITIAL_INPUTS(
     &  shd,
     &  ts, cm,
     &  fls)

      use sa_mesh_shared_variabletypes
      use model_files_variabletypes
      use model_files_variables
      use model_dates
      use FLAGS
      use climate_forcing

      use RUNCLASS36_constants
      use RUNCLASS36_variables
      use RUNCLASS36_save_output

      implicit none

!> DECLARE THE READ IN VARIABLES.
!> ----------------------------
!> VALUES NEEDED for drainage_database and/or new_shd.r2c
!      integer
!     +  WF_LAND_COUNT,
!     +  LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX,
!     +  LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX
!      real WF_LAND_MAX, WF_LAND_SUM
!> declared in MESH_INPUT_MODULE
!>  BASIN_FRACTION, WF_NHYD,
!>  WF_QR, WF_QBASE, WF_QI2, WF_QO1, WF_QO2, WF_STORE1, WF_STORE2,
!>  WF_QI1, SNOGRD, FSDOWN

!> -----------------------------
!> Values needed for READ_S_MOISTURE_TXT
!> already declared:
!>  THLQROW, valuem
!> -----------------------------
!> Values needed for READ_PARAMETERS_HYDROLOGY :
!-      integer INDEPPAR, DEPPAR, M_C
!-      real WF_R2(M_C)
!> Values already declared above:
!>  RELFLG
!> -----------------------------
!> VALUES FOR MANY INPUT FILES
      character(8) RELEASE

!> The types that contain allocatable values
      type(ShedGridParams) :: shd
      type(CLIM_INFO) :: cm
      type(dates_model) :: ts
      type(fl_ids):: fls

!> DECLARE THE LOCAL VARIABLES
      integer NA, NTYPE, IGND, ios, i, j, k

!> ====================================
!> read the RUN_OPTIONS input file called "MESH_input_run_options.ini"
!> and SET or RESET any CONTROL FLAGS
!> and READ the GRID OUTPUT DIRECTORIES.
      call READ_RUN_OPTIONS(ts, cm, fls)

!> And Open and read in values from new_shd.r2c file
!> *********************************************************************
!> DRAINAGE DATABASE (BASIN SHD) (DRAINAGE_DATABASE.TXT):
!> IS NO LONGER USED.  DRAINAGE_DATABASE.TXT HAS BEEN REPLACED WITH
!> THE BASIN SHD FILE.  READ_SHED_EF, FROM STAND-ALONE RTE.EXE
!> (WATROUTE), IS CALLED TO READ THE NEW FILE.
      if (SHDFILEFLAG == 1) then

        open(fls%fl(mfk%f20)%iun,
     &    file=adjustl(trim(fls%fl(mfk%f20)%fn)),
     &    status='old', iostat=ios)
        if (IOS == 0) then
          close(fls%fl(mfk%f20)%iun)
          print *,
     &      'Reading Drainage Database from MESH_drainage_database.r2c'
          call READ_SHED_EF(fls, mfk%f20, shd)
          write(6, *) ' READ: SUCCESSFUL, FILE: CLOSED'
!>
!>*******************************************************************
!>
!          allocate(
!     &      BASIN_FRACTION(NA))
!+        ALLOCATE (WF_NHYD(NA), WF_QR(NA),
!+     &  WF_QBASE(NA), WF_QI2(NA), WF_QO1(NA), WF_QO2(NA),
!+     &  WF_STORE1(NA), WF_STORE2(NA), WF_QI1(NA), SNOGRD(NA),
!+     &  )
!          BASIN_FRACTION(1) = -1
          shd%lc%ILG = shd%NA*shd%lc%NTYPE
        else
          print *, 'ERROR with event.evt or new_shd.r2c'
          stop
        end if

      else if (SHDFILEFLAG == 0) then

!> *********************************************************************
!> Open and read in values from MESH_input_drainage_database.txt file
!>   if new_shd.r2c file was not found
!> *********************************************************************
!        open(UNIT=20, FILE='MESH_input_drainage_database.txt',
!     &    STATUS='OLD', IOSTAT=IOS)
!        if (IOS == 0) then
!          print *, 'Reading Drainage Database from ',
!     &      'MESH_input_drainage_database.txt'
!        else
!          print *, 'MESH_input_drainage_database.txt not found'
!          stop
!        end if
!        read(20, '(i5, 50x, i5)') NA, NAA
!        read(20, '(f10.0, 5x, 2i5)') AL, NRVR, NTYPE
!        GRDN = 0.0
!        GRDE = 0.0

!todo change the code or documentation on these variables.
!ANDY Set ILG from the read-in values
!        ILG = NA*NTYPE

!> Using IOSTAT allows us to try to read input that may or may not exist.
!> If all of the values successfully get read, IOSTAT=VarName will set
!> VarName to 0. If all of the values were not successfully read,
!> VarName would be set to 1 or more. In this case, the VarName that
!> we are using is IOS.
!        read(20, '(12i5, 2f5.0)', IOSTAT=IOS) IYMIN, WF_IYMAX,
!     &    JXMIN, WF_JXMAX, LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX,
!     &    LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX, GRDN, GRDE

!> Condition for Lat/Long by Frank S Sept/1999
!        if (GRDN > 0.0) then
!          IYMIN = LATDEGMIN*60 + LATMINMIN
!          WF_IYMAX = LATDEGMAX*60 + LATMINMAX
!          JXMIN = LONDEGMIN*60 + LONMINMIN
!          WF_JXMAX = LONDEGMAX*60 + LONMINMAX

!        else
!> Define GRDN & GRDE for UTM
!          GRDN = AL/1000.0
!          GRDE = AL/1000.0
!        end if
!        read(20, '(2i5)') YCOUNT, XCOUNT

!> check if we are going to get an "array bounds out of range" error
!        if (YCOUNT > 100) then
!          write(6, *) 'WARNING: The height of the basin is very high.',
!     *      'This may negatively impact performance.'
!-+          PRINT *, 'size of grid arrays in MESH: ',M_Y
!-+          PRINT *, 'number up/down (north/south) ',
!-+     &             'grids from MESH_drainage_database.txt'
!-+     PRINT *, ' file: ',YCOUNT
!-+          PRINT *, 'Please adjust these values.'
!-+          STOP
!        end if

!        if (XCOUNT > 100) then
!          write(6, *) 'WARNING: The width of the basin is very high. ',
!     *     'This may negatively impact performance.'
!-+          PRINT *, 'size of grid arrays in MESH: ',M_X
!-+          PRINT *, 'no. of east/west (left/right) grids from ',
!-+     &             'MESH_drainage_database.txt'
!-+     PRINT *, ' file: ',XCOUNT
!-+          PRINT *, 'Please adjust these values.'
!-+          STOP
!        end if

!ANDY Allocation of variables that use NA and NTYPE
!        allocate(WF_IBN(NA), WF_IROUGH(NA),
!     &    WF_ICHNL(NA), WF_NEXT(NA), WF_ELEV(NA), WF_IREACH(NA),
!     &    WF_DA(NA), WF_BNKFLL(NA), WF_CHANNELSLOPE(NA),
!     &    FRAC(NA), BASIN_FRACTION(NA))
!        allocate(ACLASS(NA, NTYPE))

!ANDY Zero everything we just allocated
!        do i = 1, NA
!          do j = 1, NTYPE
!            ACLASS(i, j) = 0
!          end do
!        end do
!        do i = 1, NA
!          WF_IBN(i) = 0
!          WF_IROUGH(i) = 0
!          WF_ICHNL(i) = 0
!          WF_NEXT(i) = 0
!          WF_ELEV(i) = 0
!          WF_IREACH(i) = 0
!          WF_DA(i) = 0
!          WF_BNKFLL(i) = 0
!          WF_CHANNELSLOPE(i) = 0
!          FRAC(i) = 0
!          BASIN_FRACTION(i) = 0
!        end do

        !Set this to ensure basin fraction will be set later on
!        BASIN_FRACTION(1) = -1

!        allocate(YYY(NA), XXX(NA))

!        do i = 1, YCOUNT
!          read(20, *)
!        end do

!        do i = 1, NA
!          read(20, '(5x, 2i5, 3f10.5, i7, 5i5, f5.2, 15f5.2)') YYY(i),
!     &      XXX(i), WF_DA(i), WF_BNKFLL(i), WF_CHANNELSLOPE(i),
!     &      WF_ELEV(i), WF_IBN(i), WF_IROUGH(i), WF_ICHNL(i),
!     &      WF_NEXT(i), WF_IREACH(i), FRAC(i),
!     &      (ACLASS(i, j), j = 1, NTYPE)
!> check to make sure land cover areas sum to 100%
!          WF_LAND_COUNT = 1
!          WF_LAND_MAX = 0.0
!          WF_LAND_SUM = 0.0
!          do j = 1, NTYPE
!            WF_LAND_SUM = WF_LAND_SUM + ACLASS(i, j)
!            if (ACLASS(i, j) > WF_LAND_MAX) then
!              WF_LAND_COUNT = j
!              WF_LAND_MAX = ACLASS(i, j)
!            end if
!          end do
!          if (WF_LAND_SUM /= 1.0) THEN
!            ACLASS(i, WF_LAND_COUNT) =
!     &        ACLASS(i, WF_LAND_COUNT) - (WF_LAND_SUM - 1.0)
!          end if
!        end do

!        close(20)

      end if ! IF SHDFILE...

      !> Assign shed values to local variables.
      NA = shd%NA
      NTYPE = shd%lc%NTYPE

      if (shd%xCount > 100) then
        write(6, *) 'WARNING: The width of the basin is very high. ',
     &    'This may negatively impact performance.'
      end if
      if (shd%yCount > 100) then
        write(6, *) 'WARNING: The height of the basin is very high. ',
     &    'This may negatively impact performance.'
      end if
      if (shd%lc%ILG > 1500) then
        write(6, *) 'WARNING: The number of grid squares in the basin',
     &    ' is very high. This may negatively impact performance.'
      end if

!> *********************************************************************
!> Open and read in values from MESH_input_soil_levels.txt file
!> *********************************************************************
      call READ_SOIL_LEVELS(shd, fls)
      print *, 'IGND = ', shd%lc%IGND

      IGND = shd%lc%IGND

!> CHECK THAT GRID OUTPUT POINTS ARE IN THE BASIN
      do i = 1, WF_NUM_POINTS
        if (op%N_OUT(i) > shd%NA) then !IF EXISTS IN BASIN
          write(6, *)
          write(6, *)
          write(6, *) 'Grids from basin watershed file: ', shd%NA
          write(6, *) 'Grid output point ', i, ' is in Grid: ',
     &      op%N_OUT(i)
          write(6, *) 'Please adjust this grid output point in ',
     &      'MESH_input_run_options.ini'
          stop
        end if
      end do

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
     &  IGND, shd%yCount, shd%xCount, NA, NTYPE,
     &  shd%yyy, shd%xxx, cp%THLQROW)

      call READ_S_TEMPERATURE_TXT(
     &  IGND, shd%yCount, shd%xCount, NA, NTYPE,
     &  shd%yyy, shd%xxx, cp%TBARROW)
!>
!>*******************************************************************
!>
!> Read the mesh_parameters_hydrology.ini file
      allocate(hp%ZSNLROW(NA, NTYPE), hp%ZPLGROW(NA, NTYPE),
     &         hp%ZPLSROW(NA, NTYPE), hp%FRZCROW(NA, NTYPE),
     &         hp%CMAXROW(NA, NTYPE), hp%CMINROW(NA, NTYPE),
     &         hp%BROW(NA, NTYPE), hp%K1ROW(NA, NTYPE),
     &         hp%K2ROW(NA, NTYPE),
     &         hp%fetchROW(NA, NTYPE), hp%HtROW(NA, NTYPE),
     &         hp%N_SROW(NA, NTYPE), hp%A_SROW(NA, NTYPE),
     &         hp%DistribROW(NA, NTYPE))

      NYEARS = YEAR_STOP - YEAR_START + 1
      allocate(t0_ACC(NYEARS))
      t0_ACC = 0.0

      call READ_PARAMETERS_HYDROLOGY(shd, fls)

      return

      end subroutine
