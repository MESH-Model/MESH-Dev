subroutine READ_PARAMETERS_CLASS(shd, fls, cm)

    use mpi_shared_variables
    use sa_mesh_shared_parameters
    use sa_mesh_shared_variabletypes
    use sa_mesh_shared_variables
    use model_files_variabletypes
    use model_files_variables
    use climate_forcing

    use RUNCLASS36_constants
    use RUNCLASS36_variables, only: cp

    use FLAGS

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    type(fl_ids) :: fls
!todo: remove this.
    type(clim_info) :: cm

    !> Local variables.
    integer NA, NTYPE, NSL, iun, ierr, k, ignd, i, m, j

    !> Local variables (read from file).
    real DEGLAT, DEGLON, ZRFM, ZRFH, ZBLD, GC
    real, dimension(:, :), allocatable :: FCAN, LNZ0, ALVC, ALIC
    real, dimension(:, :), allocatable :: PAMX, PAMN, CMAS, ROOT, RSMN, QA50, VPDA, VPDB, PSGA, PSGB
    real, dimension(:, :), allocatable :: SAND, CLAY, ORGM
    real, dimension(:), allocatable :: FARE, DRN, XSLP, GRKF, SDEP, DD, KS, MANN
    integer, dimension(:), allocatable :: MID
    real, dimension(:), allocatable :: TCAN, TSNO, TPND, ZPND, RCAN, SNCAN, SNO, ALBS, RHOS, GRO
    real, dimension(:, :), allocatable :: TBAR, THLQ, THIC
    integer JOUT1, JOUT2, JAV1, JAV2, KOUT1, KOUT2, KAV1, KAV2
    integer IHOUR, IMIN, IDAY, IYEAR

    !> Open the file.
    iun = fls%fl(mfk%f50)%iun
    open(iun, &
         file = trim(adjustl(fls%fl(mfk%f50)%fn)), &
         status = 'old', &
         action = 'read', &
         iostat = ierr)

    !> Check for errors from opening the file.
    if (ierr /= 0) then
        print *
        print *, &
            'MESH_parameters_CLASS.ini could not be opened.', &
            'Ensure that the file exists and restart the program.'
        stop
    else if (ro%VERBOSEMODE > 0) then
        write(6, '(a)', advance = 'no') 'READING: MESH_parameters_CLASS.ini '
    end if

    NA = shd%NA
    NTYPE = shd%lc%NTYPE
    NSL = shd%lc%IGND

1000    format(2x, 6a4)

    !> Read constants from file.
    read(iun, 1000) TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6
    read(iun, 1000) NAME1, NAME2, NAME3, NAME4, NAME5, NAME6
    read(iun, 1000) PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
    read(iun, *) DEGLAT, DEGLON, ZRFM, ZRFH, ZBLD, GC, shd%wc%ILG, i, m

    !> Check that the number of GRUs matches the drainage database value.
    if (NTYPE /= m .and. NTYPE > 0) then
        print *
        print *, 'GRUs from MESH_parameters_CLASS.ini: ', m
        print *, 'GRUs from basin watershed file: ', NTYPE
        print *, 'These values must be equal.'
	    stop
    end if

    !> Check that the number of grid cells matches the drainage database value.
    if (i /= NA) then
        print *
        print *, &
            'ERROR: The number of grid squares in the class ', &
            'parameters file does not match the number of grid squares ', &
            'from the shed file.'
        stop
    end if

    JLAT = nint(DEGLAT)

    !> Allocate temporary variables.
    allocate(FCAN(NTYPE, ICP1), LNZ0(NTYPE, ICP1), ALVC(NTYPE, ICP1), ALIC(NTYPE, ICP1), &
             PAMX(NTYPE, ICAN), PAMN(NTYPE, ICAN), CMAS(NTYPE, ICAN), ROOT(NTYPE, ICAN), &
             RSMN(NTYPE, ICAN), QA50(NTYPE, ICAN), VPDA(NTYPE, ICAN), VPDB(NTYPE, ICAN), PSGA(NTYPE, ICAN), PSGB(NTYPE, ICAN), &
             SAND(NTYPE, NSL), CLAY(NTYPE, NSL), ORGM(NTYPE, NSL), &
             FARE(NTYPE), DRN(NTYPE), XSLP(NTYPE), GRKF(NTYPE), SDEP(NTYPE), DD(NTYPE), KS(NTYPE), MANN(NTYPE), &
             MID(NTYPE), &
             TCAN(NTYPE), TSNO(NTYPE), TPND(NTYPE), ZPND(NTYPE), RCAN(NTYPE), SNCAN(NTYPE), &
             SNO(NTYPE), ALBS(NTYPE), RHOS(NTYPE), GRO(NTYPE), &
             TBAR(NTYPE, NSL), THLQ(NTYPE, NSL), THIC(NTYPE, NSL))

    !> Determine the number of layers for soil parameters to read from file.
    if (NRSOILAYEREADFLAG == 1) then
        ignd = NSL
    else
        ignd = 3
    end if

    !> Populate temporary variables from file.
    do m = 1, NTYPE
        read(iun, *) (FCAN(m, j), j = 1, ICP1), (PAMX(m, j), j = 1, ICAN)
        read(iun, *) (LNZ0(m, j), j = 1, ICP1), (PAMN(m, j), j = 1, ICAN)
        read(iun, *) (ALVC(m, j), j = 1, ICP1), (CMAS(m, j), j = 1, ICAN)
        read(iun, *) (ALIC(m, j), j = 1, ICP1), (ROOT(m, j), j = 1, ICAN)
        read(iun, *) (RSMN(m, j), j = 1, ICAN), (QA50(m, j), j = 1, ICAN)
        read(iun, *) (VPDA(m, j), j = 1, ICAN), (VPDB(m, j), j = 1, ICAN)
        read(iun, *) (PSGA(m, j), j = 1, ICAN), (PSGB(m, j), j = 1, ICAN)
        read(iun, *) DRN(m), SDEP(m), FARE(m), DD(m)
        read(iun, *) XSLP(m), GRKF(m), MANN(m), KS(m), MID(m)
        read(iun, *) (SAND(m, j), j = 1, ignd)
        read(iun, *) (CLAY(m, j), j = 1, ignd)
        read(iun, *) (ORGM(m, j), j = 1, ignd)
        read(iun, *) (TBAR(m, j), j = 1, ignd), TCAN(m), TSNO(m), TPND(m)
        read(iun, *) (THLQ(m, j), j = 1, ignd), (THIC(m, j), j = 1, ignd), ZPND(m)
        read(iun, *) RCAN(m), SNCAN(m), SNO(m), ALBS(m), RHOS(m), GRO(m)
    end do

!todo: Make sure these variables are documented properly (for CLASS output, not currently used)
    read(iun, *) JOUT1, JOUT2, JAV1, JAV2
    read(iun, *) KOUT1, KOUT2, KAV1, KAV2

    !> Read in the starting date of the forcing files.
    read(iun, *) IHOUR, IMIN, IDAY, IYEAR

    !> Close the file.
    close(iun)
    if (ro%VERBOSEMODE > 0) print *, 'READ: SUCCESSFUL, FILE: CLOSED'

    !> Distribute soil variables to additional layers.
!todo: Change this so that soil.ini can take more than 3 layers.
    do j = 4, NSL
        do m = 1, NTYPE
            if (NRSOILAYEREADFLAG == 0) then
                TBAR(m, j) = TBAR(m, 3) !note333 see read_s_temperature_txt.f for more TBAR information
                THLQ(m, j) = THLQ(m, 3) !note444 see read_s_moisture_txt.f for more THLQ information
                THIC(m, j) = THIC(m, 3)
                if (SDEP(m) < (shd%lc%sl%ZBOT(j - 1) + 0.001) .and. SAND(m, 3) > -2.5) then
                    SAND(m, j) = -3.0
                    CLAY(m, j) = -3.0
                    ORGM(m, j) = -3.0
                else
                    SAND(m, j) = SAND(m, 3)
                    CLAY(m, j) = CLAY(m, 3)
                    ORGM(m, j) = ORGM(m, 3)
                end if
            else
                if (SDEP(m) < (shd%lc%sl%ZBOT(j - 1) + 0.001) .and. SAND(m, j) > -2.5) then
                    SAND(m, j) = -3.0
                    CLAY(m, j) = -3.0
                    ORGM(m, j) = -3.0
                end if
            end if !if (NRSOILAYEREADFLAG == 0) then
        end do
    end do

    !> Distribute the values for legacy parameter checks.
    cp%FCANROW(1, :, :) = FCAN(:, :)
    cp%PAMXROW(1, :, :) = PAMX(:, :)
    cp%LNZ0ROW(1, :, :) = LNZ0(:, :)
    cp%PAMNROW(1, :, :) = PAMN(:, :)
    cp%ALVCROW(1, :, :) = ALVC(:, :)
    cp%CMASROW(1, :, :) = CMAS(:, :)
    cp%ALICROW(1, :, :) = ALIC(:, :)
    cp%ROOTROW(1, :, :) = ROOT(:, :)
    cp%RSMNROW(1, :, :) = RSMN(:, :)
    cp%QA50ROW(1, :, :) = QA50(:, :)
    cp%VPDAROW(1, :, :) = VPDA(:, :)
    cp%VPDBROW(1, :, :) = VPDB(:, :)
    cp%PSGAROW(1, :, :) = PSGA(:, :)
    cp%PSGBROW(1, :, :) = PSGB(:, :)
    cp%DRNROW(1, :) = DRN(:)
    cp%SDEPROW(1, :) = SDEP(:)
    cp%FAREROW(1, :) = FARE(:)
    cp%DDROW(1, :) = DD(:)
    cp%XSLPROW(1, :) = XSLP(:)
    cp%XDROW(1, :) = GRKF(:)
    cp%MANNROW(1, :) = MANN(:)
    cp%KSROW(1, :) = KS(:)
    cp%MIDROW(1, :) = MID(:)
    cp%SANDROW(1, :, :) = SAND(:, :)
    cp%CLAYROW(1, :, :) = CLAY(:, :)
    cp%ORGMROW(1, :, :) = ORGM(:, :)
    cp%TBARROW(1, :, :) = TBAR(:, :)
    cp%TCANROW(1, :) = TCAN(:)
    cp%TSNOROW(1, :) = TSNO(:)
    cp%TPNDROW(1, :) = TPND(:)
    cp%THLQROW(1, :, :) = THLQ(:, :)
    cp%THICROW(1, :, :) = THIC(:, :)
    cp%ZPNDROW(1, :) = ZPND(:)
    cp%RCANROW(1, :) = RCAN(:)
    cp%SCANROW(1, :) = SNCAN(:)
    cp%SNOROW(1, :) = SNO(:)
    cp%ALBSROW(1, :) = ALBS(:)
    cp%RHOSROW(1, :) = RHOS(:)
    cp%GROROW(1, :) = GRO(:)

    !> Distribute the values.
    do k = il1, il2

        !> Grab the indeces of the grid cell and GRU.
        i = shd%lc%ILMOS(k)
        m = shd%lc%JLMOS(k)

        !> Distribute the parameter values.
        pm%sfp%zrfm(k) = ZRFM
        pm%sfp%zrfh(k) = ZRFH
        pm%sfp%zbld(k) = ZBLD
        pm%tp%gc(k) = GC
        pm%tp%fare(k) = FARE(m)
        pm%tp%mid(k) = max(1, MID(m))
        pm%cp%fcan(k, :) = FCAN(m, :)
        pm%cp%lnz0(k, :) = LNZ0(m, :)
        pm%cp%alvc(k, :) = ALVC(m, :)
        pm%cp%alic(k, :) = ALIC(m, :)
        pm%cp%lamx(k, :) = PAMX(m, :)
        pm%cp%lamn(k, :) = PAMN(m, :)
        pm%cp%cmas(k, :) = CMAS(m, :)
        pm%cp%root(k, :) = ROOT(m, :)
        pm%cp%rsmn(k, :) = RSMN(m, :)
        pm%cp%qa50(k, :) = QA50(m, :)
        pm%cp%vpda(k, :) = VPDA(m, :)
        pm%cp%vpdb(k, :) = VPDB(m, :)
        pm%cp%psga(k, :) = PSGA(m, :)
        pm%cp%psgb(k, :) = PSGB(m, :)
        pm%slp%sdep(k) = SDEP(m)
        pm%hp%drn(k) = DRN(m)
        if (allocated(shd%SLOPE_INT)) then
            pm%tp%xslp(k) = shd%SLOPE_INT(i) !taken from the drainage database.
        else
            pm%tp%xslp(k) = XSLP(m) !taken by GRU from CLASS.ini
        end if
        if (allocated(shd%DRDN)) then
            pm%hp%dd(k) = shd%DRDN(i) !taken from the drainage database.
        else
            pm%hp%dd(k) = DD(m)/1000.0 !taken from CLASS.ini and from km/km^2 to m/m^2 for WATROF.
        end if
        pm%hp%mann(k) = MANN(m)
        pm%hp%grkf(k) = GRKF(m)
        pm%hp%ks(k) = KS(m)
        pm%slp%sand(k, :) = SAND(m, :)
        pm%slp%clay(k, :) = CLAY(m, :)
        pm%slp%orgm(k, :) = ORGM(m, :)

        !> Distribute the initial prognostic variable values.
        stas%cnpy%cmai = 0.0
        stas%sno%wsno = 0.0
        stas%cnpy%qac = 0.5e-2
        stas%cnpy%tcan(k) = TCAN(m) + TFREZ
        stas%cnpy%tac(k) = TCAN(m) + TFREZ
        stas%sno%tsno(k) = TSNO(m) + TFREZ
        stas%sfc%tpnd(k) = TPND(m) + TFREZ
        stas%sfc%zpnd(k) = ZPND(m)
        stas%cnpy%rcan(k) = RCAN(m)
        stas%cnpy%sncan(k) = SNCAN(m)
        stas%sno%sno(k) = SNO(m)
        stas%sno%albs(k) = ALBS(m)
        stas%sno%rhos(k) = RHOS(m)
        stas%cnpy%gro(k) = GRO(m)
        stas%sfc%tsfs(k, 1) = TFREZ
        stas%sfc%tsfs(k, 2) = TFREZ
        stas%sfc%tsfs(k, 3) = TBAR(m, 1) + TFREZ
        stas%sfc%tsfs(k, 4) = TBAR(m, 1) + TFREZ
        stas%sl%tbar(k, :) = TBAR(m, :) + TFREZ
        stas%sl%thlq(k, :) = THLQ(m, :)
        stas%sl%thic(k, :) = THIC(m, :)
        stas%sl%tbas(k) = TBAR(m, NSL) + TFREZ

    end do !k = il1, il2

    !> Distribute the starting date of the forcing files.
    do i = 1, cm%nclim
        cm%dat(i)%start_date%year = IYEAR
        cm%dat(i)%start_date%jday = IDAY
        cm%dat(i)%start_date%hour = IHOUR
        cm%dat(i)%start_date%mins = IMIN
    end do

    !> Set the starting date from the forcing files if none is provided.
    if (ic%start%year == 0 .and. ic%start%jday == 0 .and. ic%start%hour == 0 .and. ic%start%mins == 0) then
        ic%start%year = IYEAR
        ic%start%jday = IDAY
        ic%start%hour = IHOUR
        ic%start%mins = IMIN
    end if

    !> Initialize the current time-step.
    ic%now%year = ic%start%year
    ic%now%jday = ic%start%jday
    call julian2monthday(ic%now%jday, ic%now%year, ic%now%month, ic%now%day)
    ic%now%hour = ic%start%hour
    ic%now%mins = ic%start%mins

    !> Deallocate temporary variables.
    deallocate(FCAN, LNZ0, ALVC, ALIC, &
               PAMX, PAMN, CMAS, ROOT, RSMN, QA50, VPDA, VPDB, PSGA, PSGB, &
               SAND, CLAY, ORGM, &
               FARE, DRN, XSLP, GRKF, SDEP, DD, KS, MANN, &
               MID, &
               TCAN, TSNO, TPND, ZPND, RCAN, SNCAN, SNO, ALBS, RHOS, GRO, &
               TBAR, THLQ, THIC)

    return

end subroutine
