subroutine READ_PARAMETERS_CLASS(shd, fls, cm)

    !> For: 'ShedGridParams' type, run option flags.
    use sa_mesh_shared_variables

    !> Required for parameters ('ROW' indexing).
    use sa_mesh_shared_output_variables

    !> Required for file object and CLASS.ini file index.
    use model_files_variables

    !> Required for 'NRSOILAYEREADFLAG'.
    use FLAGS

    use RUNCLASS36_constants

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
    read(iun, *) DEGLAT, DEGLON, pmrow%sfp%zrfm, pmrow%sfp%zrfh, pmrow%sfp%zbld, pmrow%tp%gc, shd%wc%ILG, i, m

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

    !> Determine the number of layers for soil parameters to read from file.
    if (NRSOILAYEREADFLAG == 1) then
        ignd = NSL
    else
        ignd = 3
    end if

    !> Populate temporary variables from file.
    do m = 1, NTYPE
        read(iun, *) (pmrow%cp%fcan(m, j), j = 1, ICP1), (pmrow%cp%lamx(m, j), j = 1, ICAN)
        read(iun, *) (pmrow%cp%lnz0(m, j), j = 1, ICP1), (pmrow%cp%lamn(m, j), j = 1, ICAN)
        read(iun, *) (pmrow%cp%alvc(m, j), j = 1, ICP1), (pmrow%cp%cmas(m, j), j = 1, ICAN)
        read(iun, *) (pmrow%cp%alic(m, j), j = 1, ICP1), (pmrow%cp%root(m, j), j = 1, ICAN)
        read(iun, *) (pmrow%cp%rsmn(m, j), j = 1, ICAN), (pmrow%cp%qa50(m, j), j = 1, ICAN)
        read(iun, *) (pmrow%cp%vpda(m, j), j = 1, ICAN), (pmrow%cp%vpdb(m, j), j = 1, ICAN)
        read(iun, *) (pmrow%cp%psga(m, j), j = 1, ICAN), (pmrow%cp%psgb(m, j), j = 1, ICAN)
        read(iun, *) pmrow%hp%drn(m), pmrow%slp%sdep(m), pmrow%tp%fare(m), pmrow%hp%dd(m)
        read(iun, *) pmrow%tp%xslp(m), pmrow%hp%grkf(m), pmrow%hp%mann(m), pmrow%hp%ks(m), pmrow%tp%mid(m)
        read(iun, *) (pmrow%slp%sand(m, j), j = 1, ignd)
        read(iun, *) (pmrow%slp%clay(m, j), j = 1, ignd)
        read(iun, *) (pmrow%slp%orgm(m, j), j = 1, ignd)
        read(iun, *) (stasrow%sl%tbar(m, j), j = 1, ignd), stasrow%cnpy%tcan(m), stasrow%sno%tsno(m), stasrow%sfc%tpnd(m)
        read(iun, *) (stasrow%sl%thlq(m, j), j = 1, ignd), (stasrow%sl%thic(m, j), j = 1, ignd), stasrow%sfc%zpnd(m)
        read(iun, *) stasrow%cnpy%rcan(m), stasrow%cnpy%sncan(m), stasrow%sno%sno(m), stasrow%sno%albs(m), stasrow%sno%rhos(m), &
            stasrow%cnpy%gro(m)
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
    do j = 4, NSL
        do m = 1, NTYPE
            if (NRSOILAYEREADFLAG == 0) then
                stasrow%sl%tbar(m, j) = stasrow%sl%tbar(m, 3) !note333 see read_s_temperature_txt.f for more TBAR information
                stasrow%sl%thlq(m, j) = stasrow%sl%thlq(m, 3) !note444 see read_s_moisture_txt.f for more THLQ information
                stasrow%sl%thic(m, j) = stasrow%sl%thic(m, 3)
                if (pmrow%slp%sdep(m) < (shd%lc%sl%ZBOT(j - 1) + 0.001) .and. pmrow%slp%sand(m, 3) > -2.5) then
                    pmrow%slp%sand(m, j) = -3.0
                    pmrow%slp%clay(m, j) = -3.0
                    pmrow%slp%orgm(m, j) = -3.0
                else
                    pmrow%slp%sand(m, j) = pmrow%slp%sand(m, 3)
                    pmrow%slp%clay(m, j) = pmrow%slp%clay(m, 3)
                    pmrow%slp%orgm(m, j) = pmrow%slp%orgm(m, 3)
                end if
            else
                if (pmrow%slp%sdep(m) < (shd%lc%sl%ZBOT(j - 1) + 0.001) .and. pmrow%slp%sand(m, j) > -2.5) then
                    pmrow%slp%sand(m, j) = -3.0
                    pmrow%slp%clay(m, j) = -3.0
                    pmrow%slp%orgm(m, j) = -3.0
                end if
            end if !if (NRSOILAYEREADFLAG == 0) then
        end do
    end do

    return

end subroutine
