subroutine READ_PARAMETERS_CLASS(shd, fls, cm)

    !> Required for 'ipid'.
    use mpi_module

    !> Required for file object and CLASS.ini file index.
    use model_files_variables

    !> For the 'ShedGridParams' type and SA_MESH parameters.
    use sa_mesh_variables

    !> Required for 'NRSOILAYEREADFLAG'.
    use FLAGS

    use RUNCLASS36_constants
    use RUNCLASS36_variables

    !> Used for starting date of climate forcing data.
    use climate_forcing

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    type(fl_ids) :: fls
    type(clim_info) :: cm

    !> Local variables.
    integer NA, NTYPE, NSL, iun, ierr, k, ignd, i, m, j

    !> Local variables (read from file).
    real DEGLAT, DEGLON
    integer JOUT1, JOUT2, JAV1, JAV2, KOUT1, KOUT2, KAV1, KAV2

    !> Open the file.
    iun = fls%fl(mfk%f50)%iun
    open(iun, &
         file = trim(adjustl(fls%fl(mfk%f50)%fn)), &
         status = 'old', &
         action = 'read', &
         iostat = ierr)

    !> Check for errors from opening the file.
    if (ierr /= 0 .and. ipid == 0) then
        print *
        print *, &
            'MESH_parameters_CLASS.ini could not be opened.', &
            'Ensure that the file exists and restart the program.'
        stop
    else if (ro%VERBOSEMODE > 0) then
        write(6, "(1x, 'READING: ', (a))", advance = 'no') trim(adjustl(fls%fl(mfk%f50)%fn))
    end if

    NA = shd%NA
    NTYPE = shd%lc%NTYPE
    NSL = shd%lc%IGND

1000    format(2x, 6a4)

    !> Read constants from file.
    read(iun, 1000) TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6
    read(iun, 1000) NAME1, NAME2, NAME3, NAME4, NAME5, NAME6
    read(iun, 1000) PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
    read(iun, *) DEGLAT, DEGLON, pm_gru%sfp%zrfm(1), pm_gru%sfp%zrfh(1), pm_gru%sfp%zbld(1), pm_gru%tp%gc(1), shd%wc%ILG, i, m

    !> Check that the number of GRUs matches the drainage database value.
    if (NTYPE /= m .and. NTYPE > 0 .and. ipid == 0) then
        print *
        print *, 'GRUs from MESH_parameters_CLASS.ini: ', m
        print *, 'GRUs from basin watershed file: ', NTYPE
        print *, 'These values must be equal.'
	    stop
    end if

    !> Check that the number of grid cells matches the drainage database value.
    if (i /= NA .and. ipid == 0) then
        print *
        print *, &
            'ERROR: The number of grid squares in the class ', &
            'parameters file does not match the number of grid squares ', &
            'from the shed file.'
        stop
    end if

    JLAT = nint(DEGLAT)

    !> Determine the number of layers for soil parameters to read from file.
    if (NRSOILAYEREADFLAG > 3) then
        ignd = min(NRSOILAYEREADFLAG, NSL)
    else if (NRSOILAYEREADFLAG == 1) then
        ignd = NSL
    else
        ignd = 3
    end if

    !> Populate temporary variables from file.
    do m = 1, NTYPE
        read(iun, *) (pm_gru%cp%fcan(m, j), j = 1, ICP1), (pm_gru%cp%lamx(m, j), j = 1, ICAN)
        read(iun, *) (pm_gru%cp%lnz0(m, j), j = 1, ICP1), (pm_gru%cp%lamn(m, j), j = 1, ICAN)
        read(iun, *) (pm_gru%cp%alvc(m, j), j = 1, ICP1), (pm_gru%cp%cmas(m, j), j = 1, ICAN)
        read(iun, *) (pm_gru%cp%alic(m, j), j = 1, ICP1), (pm_gru%cp%root(m, j), j = 1, ICAN)
        read(iun, *) (pm_gru%cp%rsmn(m, j), j = 1, ICAN), (pm_gru%cp%qa50(m, j), j = 1, ICAN)
        read(iun, *) (pm_gru%cp%vpda(m, j), j = 1, ICAN), (pm_gru%cp%vpdb(m, j), j = 1, ICAN)
        read(iun, *) (pm_gru%cp%psga(m, j), j = 1, ICAN), (pm_gru%cp%psgb(m, j), j = 1, ICAN)
        read(iun, *) pm_gru%hp%drn(m), pm_gru%slp%sdep(m), pm_gru%tp%fare(m), pm_gru%hp%dd(m)
        read(iun, *) pm_gru%tp%xslp(m), pm_gru%hp%grkf(m), pm_gru%hp%mann(m), pm_gru%hp%ks(m), pm_gru%tp%mid(m)
        read(iun, *) (pm_gru%slp%sand(m, j), j = 1, ignd)
        read(iun, *) (pm_gru%slp%clay(m, j), j = 1, ignd)
        read(iun, *) (pm_gru%slp%orgm(m, j), j = 1, ignd)
        read(iun, *) (stas_gru%sl%tbar(m, j), j = 1, ignd), stas_gru%cnpy%tcan(m), stas_gru%sno%tsno(m), stas_gru%sfc%tpnd(m)
        read(iun, *) (stas_gru%sl%thlq(m, j), j = 1, ignd), (stas_gru%sl%thic(m, j), j = 1, ignd), stas_gru%sfc%zpnd(m)
        read(iun, *) stas_gru%cnpy%rcan(m), stas_gru%cnpy%sncan(m), stas_gru%sno%sno(m), stas_gru%sno%albs(m), &
            stas_gru%sno%rhos(m), stas_gru%cnpy%gro(m)
    end do

!todo: Make sure these variables are documented properly (for CLASS output, not currently used)
    read(iun, *) JOUT1, JOUT2, JAV1, JAV2
    read(iun, *) KOUT1, KOUT2, KAV1, KAV2

    !> Read in the starting date of the forcing files.
    read(iun, *) cm%start_date%hour, cm%start_date%mins, cm%start_date%jday, cm%start_date%year

    !> Close the file.
    close(iun)
    if (ro%VERBOSEMODE > 0) print *, 'READ: SUCCESSFUL, FILE: CLOSED'

    !> Distribute soil variables to additional layers.
!todo: Change this so that soil.ini can take more than 3 layers.
    if (NRSOILAYEREADFLAG > 3) then
        ignd = min(NRSOILAYEREADFLAG, NSL)
    else if (NRSOILAYEREADFLAG == 1) then
        ignd = 0
    else
        ignd = 3
    end if
    do j = 4, NSL
        do m = 1, NTYPE

            !> Distribute parameters and initial states to lower layers whose values might not be defined.
            if (ignd > 0) then
                stas_gru%sl%tbar(m, j) = stas_gru%sl%tbar(m, ignd) !note333 see read_s_temperature_txt.f for more TBAR information
                stas_gru%sl%thlq(m, j) = stas_gru%sl%thlq(m, ignd) !note444 see read_s_moisture_txt.f for more THLQ information
                stas_gru%sl%thic(m, j) = stas_gru%sl%thic(m, ignd)
                pm_gru%slp%sand(m, j) = pm_gru%slp%sand(m, ignd)
                pm_gru%slp%clay(m, j) = pm_gru%slp%clay(m, ignd)
                pm_gru%slp%orgm(m, j) = pm_gru%slp%orgm(m, ignd)
            end if !if (NRSOILAYEREADFLAG == 0) then

            !> Impermeable soils.
            if (pm_gru%slp%sdep(m) < (shd%lc%sl%ZBOT(j - 1) + 0.001) .and. pm_gru%slp%sand(m, j) > -2.5) then
                pm_gru%slp%sand(m, j) = -3.0
                pm_gru%slp%clay(m, j) = -3.0
                pm_gru%slp%orgm(m, j) = -3.0
            end if
        end do
    end do

    !> Assign DEGLAT and DEGLON if running a point run where no shed file exists.
    if (SHDFILEFLAG == 2) then
        shd%ylat = DEGLAT
        shd%xlng = DEGLON
    end if

    return

end subroutine
