subroutine READ_INITIAL_INPUTS(shd, ts, cm, fls)

    use mpi_shared_variables
    use mpi_utilities
    use sa_mesh_shared_parameters
    use sa_mesh_shared_variables
    use sa_mesh_shared_output_variables
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
!>  WF_QI1

    character(8) RELEASE

    !> The types that contain allocatable values
    type(ShedGridParams) :: shd
    type(CLIM_INFO) :: cm
    type(dates_model) :: ts
    type(fl_ids):: fls

    !> Local variables.
    integer NA, NTYPE, NML, NSL, ierr, k, i, m, j

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

    open(fls%fl(mfk%f20)%iun, file=adjustl(trim(fls%fl(mfk%f20)%fn)), status='old', iostat=ierr)
        if (ierr == 0) then
            close(fls%fl(mfk%f20)%iun)
            print *, 'Reading Drainage Database from MESH_drainage_database.r2c'
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
!     &    STATUS='OLD', IOSTAT=ierr)
!        if (ierr == 0) then
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
!> we are using is ierr.
!        read(20, '(12i5, 2f5.0)', IOSTAT=ierr) IYMIN, WF_IYMAX,
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

    !> Assign shd values to local variables.
    NA = shd%NA
    NTYPE = shd%lc%NTYPE

    if (shd%xCount > 100) then
        write(6, *) &
            'WARNING: The width of the basin is very high. ', &
            'This may negatively impact performance.'
    end if
    if (shd%yCount > 100) then
        write(6, *) &
            'WARNING: The height of the basin is very high. ', &
            'This may negatively impact performance.'
    end if
    if (shd%lc%ILG > 1500) then
        write(6, *) &
            'WARNING: The number of grid squares in the basin', &
            ' is very high. This may negatively impact performance.'
    end if

    !> Determine the number of active tile elements.
    allocate(shd%lc%ILMOS(shd%lc%ILG), shd%lc%JLMOS(shd%lc%ILG), &
             shd%wc%ILMOS(shd%wc%ILG), shd%wc%JLMOS(shd%wc%ILG))

    !> Count the number of tiles that are land 'lc' or water 'wc' and
    !> store the respective ID's of the grid and GRU in the 'ILMOS' and
    !> 'JLMOS' variables.
    shd%lc%NML = 0
    shd%wc%NML = 0
    do i = 1, NA
        if (-1.0 <= -0.5) then
            do m = 1, NTYPE

                !> Only count active GRUs (with > 0.0 contributing fraction).
                if (shd%lc%ACLASS(i, m) > 0.0) then
                    if (shd%IAK(i) > 0) then

                        !> Land.
                        shd%lc%NML = shd%lc%NML + 1
                        shd%lc%ILMOS(shd%lc%NML) = i
                        shd%lc%JLMOS(shd%lc%NML) = m

                    else

                        !> Water.
                        shd%wc%NML = shd%wc%NML + 1
                        shd%wc%ILMOS(shd%wc%NML) = i
                        shd%wc%JLMOS(shd%wc%NML) = m

                    end if
                end if
            end do
        end if
    end do

    !> Store the number of active tile elements to initialize variables.
    NML = shd%lc%NML

    !> Calculate the operational indices in the current node.
    call mpi_split_nml(inp, izero, ipid, NML, shd%lc%ILMOS, il1, il2, ilen)
    if (ro%DIAGNOSEMODE > 0) print 1062, ipid, NML, ilen, il1, il2

1062    format(/1x, 'Configuration and distribution of the domain', &
               /3x, 'Current process: ', i10, &
               /3x, 'Tile land elements: ', i10, &
               /3x, 'Length of single array: ', i10, &
               /3x, 'Starting index: ', i10, &
               /3x, 'Stopping index: ', i10, /)

    !> Open and read in soil depths from file.
    call READ_SOIL_LEVELS(shd, fls)
    print *, 'IGND = ', shd%lc%IGND

    !> Store the number of soil layers to initialize variables.
    NSL = shd%lc%IGND

    !> Initialize parameter values.
    allocate(pm%tp%gc(NML), pm%tp%fare(NML), pm%tp%xslp(NML), pm%tp%mid(NML), &
             pm%cp%fcan(NML, ICP1), pm%cp%z0or(NML, ICP1), pm%cp%lnz0(NML, ICP1), pm%cp%alvc(NML, ICP1), pm%cp%alic(NML, ICP1), &
             pm%cp%lamx(NML, ICAN), pm%cp%lamn(NML, ICAN), pm%cp%cmas(NML, ICAN), pm%cp%root(NML, ICAN), pm%cp%rsmn(NML, ICAN), &
             pm%cp%qa50(NML, ICAN), pm%cp%vpda(NML, ICAN), pm%cp%vpdb(NML, ICAN), pm%cp%psga(NML, ICAN), pm%cp%psgb(NML, ICAN), &
             pm%sfp%zbld(NML), pm%sfp%zrfh(NML), pm%sfp%zrfm(NML), pm%sfp%zplg(NML), pm%snp%zsnl(NML), pm%snp%zpls(NML), &
             pm%slp%sdep(NML), pm%slp%ggeo(NML), pm%slp%delz(NML), pm%slp%zbot(NML), &
             pm%slp%sand(NML, NSL), pm%slp%clay(NML, NSL), pm%slp%orgm(NML, NSL), &
             pm%hp%drn(NML), pm%hp%dd(NML), pm%hp%grkf(NML), pm%hp%mann(NML), pm%hp%ks(NML))

    !> Initialize states.

    !> Canopy.
    stas%cnpy%n = NML
    allocate(stas%cnpy%qac(NML), stas%cnpy%rcan(NML), stas%cnpy%sncan(NML), stas%cnpy%tac(NML), stas%cnpy%tcan(NML), &
             stas%cnpy%cmai(NML), stas%cnpy%gro(NML))
    stas%cnpy%qac = 0.0
    stas%cnpy%rcan = 0.0
    stas%cnpy%sncan = 0.0
    stas%cnpy%tac = 0.0
    stas%cnpy%tcan = 0.0
    stas%cnpy%cmai = 0.0
    stas%cnpy%gro = 0.0

    !> Snow.
    stas%sno%n = NML
    allocate(stas%sno%sno(NML), stas%sno%albs(NML), stas%sno%rhos(NML), stas%sno%tsno(NML), stas%sno%wsno(NML))
    stas%sno%sno = 0.0
    stas%sno%albs = 0.0
    stas%sno%rhos = 0.0
    stas%sno%tsno = 0.0
    stas%sno%wsno = 0.0

    !> Surface or at near surface.
    stas%sfc%n = NML
    allocate(stas%sfc%tpnd(NML), stas%sfc%zpnd(NML), stas%sfc%tsfs(NML, 4))
    stas%sfc%tpnd = 0.0
    stas%sfc%zpnd = 0.0
    stas%sfc%tsfs = 0.0

    !> Soil layers.
    stas%sl%n = NML
    allocate(stas%sl%tbas(NML), stas%sl%thic(NML, NSL), stas%sl%thlq(NML, NSL), stas%sl%tbar(NML, NSL))
    stas%sl%tbas = 0.0
    stas%sl%thic = 0.0
    stas%sl%thlq = 0.0
    stas%sl%tbar = 0.0

    !> Lower zone storage.
    stas%lzs%n = NML
    allocate(stas%lzs%zlw(NML), stas%lzs%tbas(NML))
    stas%lzs%zlw = 0.0
    stas%lzs%tbas = 0.0

    !> Deep zone storage.
    stas%dzs%n = NML
    allocate(stas%dzs%zlw(NML), stas%dzs%tbas(NML))
    stas%dzs%zlw = 0.0
    stas%dzs%tbas = 0.0

    !> Call 'CLASSD' to initialize constants.
!todo: replace this with a non-CLASS/generic version.
    call CLASSD

    !> Initialize output parameters ('ROW' format).
!todo: fix this.
    allocate(cp%ZRFMGRD(NA), cp%ZRFHGRD(NA), cp%ZBLDGRD(NA), cp%GCGRD(NA))
    allocate(cp%FCANROW(NA, NTYPE, ICAN + 1), cp%LNZ0ROW(NA, NTYPE, ICAN + 1), &
             cp%ALVCROW(NA, NTYPE, ICAN + 1), cp%ALICROW(NA, NTYPE, ICAN + 1))
    allocate(cp%PAMXROW(NA, NTYPE, ICAN), cp%PAMNROW(NA, NTYPE, ICAN), cp%CMASROW(NA, NTYPE, ICAN), cp%ROOTROW(NA, NTYPE, ICAN), &
             cp%RSMNROW(NA, NTYPE, ICAN), cp%QA50ROW(NA, NTYPE, ICAN), cp%VPDAROW(NA, NTYPE, ICAN), cp%VPDBROW(NA, NTYPE, ICAN), &
             cp%PSGAROW(NA, NTYPE, ICAN), cp%PSGBROW(NA, NTYPE, ICAN))
    allocate(cp%DRNROW(NA, NTYPE),  cp%SDEPROW(NA, NTYPE), cp%FAREROW(NA, NTYPE), cp%DDROW(NA, NTYPE), &
             cp%XSLPROW(NA, NTYPE), cp%XDROW(NA, NTYPE), cp%MANNROW(NA, NTYPE), cp%KSROW(NA, NTYPE), &
             cp%TCANROW(NA, NTYPE), cp%TSNOROW(NA, NTYPE), cp%TPNDROW(NA, NTYPE), cp%ZPNDROW(NA, NTYPE), &
             cp%RCANROW(NA, NTYPE), cp%SCANROW(NA, NTYPE), cp%SNOROW(NA, NTYPE),  cp%ALBSROW(NA, NTYPE), cp%RHOSROW(NA, NTYPE), &
             cp%GROROW(NA, NTYPE))
    allocate(cp%MIDROW(NA, NTYPE))
    allocate(cp%SANDROW(NA, NTYPE, NSL), cp%CLAYROW(NA, NTYPE, NSL), cp%ORGMROW(NA, NTYPE, NSL), &
             cp%TBARROW(NA, NTYPE, NSL), cp%THLQROW(NA, NTYPE, NSL), cp%THICROW(NA, NTYPE, NSL))

    !> Read parameters from file.
    call READ_PARAMETERS_CLASS(shd, fls, cm)

    !> Distribute the values.
    do k = il1, il2

        !> Grab the grid and GRU of the current tile.
        i = shd%lc%ILMOS(k)
        m = shd%lc%JLMOS(k)

        !> Grab the appropriate value.
        cp%ZRFMGRD(i) = pm%sfp%zrfm(k)
        cp%ZRFHGRD(i) = pm%sfp%zrfh(k)
        cp%ZBLDGRD(i) = pm%sfp%zbld(k)
        cp%GCGRD(i) = pm%tp%gc(k)
        cp%FCANROW(i, m, :) = pm%cp%fcan(k, :)
        cp%LNZ0ROW(i, m, :) = pm%cp%lnz0(k, :)
        cp%ALVCROW(i, m, :) = pm%cp%alvc(k, :)
        cp%ALICROW(i, m, :) = pm%cp%alic(k, :)
        cp%PAMXROW(i, m, :) = pm%cp%lamx(k, :)
        cp%PAMNROW(i, m, :) = pm%cp%lamn(k, :)
        cp%CMASROW(i, m, :) = pm%cp%cmas(k, :)
        cp%ROOTROW(i, m, :) = pm%cp%root(k, :)
        cp%RSMNROW(i, m, :) = pm%cp%rsmn(k, :)
        cp%QA50ROW(i, m, :) = pm%cp%qa50(k, :)
        cp%VPDAROW(i, m, :) = pm%cp%vpda(k, :)
        cp%VPDBROW(i, m, :) = pm%cp%vpdb(k, :)
        cp%PSGAROW(i, m, :) = pm%cp%psga(k, :)
        cp%PSGBROW(i, m, :) = pm%cp%psgb(k, :)
        cp%DRNROW(i, m) = pm%hp%drn(k)
        cp%SDEPROW(i, m) = pm%slp%sdep(k)
        cp%FAREROW(i, m) = pm%tp%fare(k)
        cp%DDROW(i, m) = pm%hp%dd(k)
        cp%XSLPROW(i, m) = pm%tp%xslp(k)
        cp%XDROW(i, m) = pm%hp%grkf(k)
        cp%MANNROW(i, m) = pm%hp%mann(k)
        cp%KSROW(i, m) = pm%hp%ks(k)
        cp%TCANROW(i, m) = stas%cnpy%tcan(k) - TFREZ
        cp%TSNOROW(i, m) = stas%sno%tsno(k) - TFREZ
        cp%TPNDROW(i, m) = stas%sfc%tpnd(k) - TFREZ
        cp%ZPNDROW(i, m) = stas%sfc%zpnd(k)
        cp%RCANROW(i, m) = stas%cnpy%rcan(k)
        cp%SCANROW(i, m) = stas%cnpy%sncan(k)
        cp%SNOROW(i, m) = stas%sno%sno(k)
        cp%ALBSROW(i, m) = stas%sno%albs(k)
        cp%RHOSROW(i, m) = stas%sno%rhos(k)
        cp%GROROW(i, m) = stas%cnpy%gro(k)
        cp%MIDROW(i, m) = pm%tp%mid(k)
        cp%SANDROW(i, m, :) = pm%slp%sand(k, :)
        cp%CLAYROW(i, m, :) = pm%slp%clay(k, :)
        cp%ORGMROW(i, m, :) = pm%slp%orgm(k, :)
        cp%TBARROW(i, m, :) = stas%sl%tbar(k, :) - TFREZ
        cp%THLQROW(i, m, :) = stas%sl%thlq(k, :)
        cp%THICROW(i, m, :) = stas%sl%thic(k, :)

    end do

    !> Check that grid output points are in the basin.
    do i = 1, WF_NUM_POINTS
        if (op%N_OUT(i) > shd%NA) then
            write(6, *)
            write(6, *)
            write(6, *) 'Grids from basin watershed file: ', shd%NA
            write(6, *) 'Grid output point ', i, ' is in Grid: ', op%N_OUT(i)
            write(6, *) 'Please adjust this grid output point in ', 'MESH_input_run_options.ini'
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
    call READ_S_MOISTURE_TXT(NSL, shd%yCount, shd%xCount, NA, NTYPE, shd%yyy, shd%xxx, cp%THLQROW)
    call READ_S_TEMPERATURE_TXT(NSL, shd%yCount, shd%xCount, NA, NTYPE, shd%yyy, shd%xxx, cp%TBARROW)

    !> Allocate additional parameters.
    allocate(hp%ZSNLROW(NA, NTYPE), hp%ZPLGROW(NA, NTYPE), hp%ZPLSROW(NA, NTYPE), hp%FRZCROW(NA, NTYPE), &
             hp%CMAXROW(NA, NTYPE), hp%CMINROW(NA, NTYPE), hp%BROW(NA, NTYPE), hp%K1ROW(NA, NTYPE), hp%K2ROW(NA, NTYPE), &
             hp%fetchROW(NA, NTYPE), hp%HtROW(NA, NTYPE), hp%N_SROW(NA, NTYPE), hp%A_SROW(NA, NTYPE), hp%DistribROW(NA, NTYPE))

    NYEARS = ic%stop%year - ic%start%year + 1
    allocate(t0_ACC(NYEARS))
    t0_ACC = 0.0

    !> Read parameters from file.
    call READ_PARAMETERS_HYDROLOGY(shd, fls)

    !> Distribute the values.
    do k = il1, il2

        !> Grab the grid and GRU of the current tile.
        i = shd%lc%ILMOS(k)
        m = shd%lc%JLMOS(k)

        !> Grad the appropriate value.
        hp%ZSNLROW(i, m) = pm%snp%zsnl(k)
        hp%ZPLGROW(i, m) = pm%sfp%zplg(k)
        hp%ZPLSROW(i, m) = pm%snp%zpls(k)

    end do !k = il1, il2

end subroutine
