subroutine READ_PARAMETERS_CLASS(shd, fls)

    use sa_mesh_shared_variabletypes
    use sa_mesh_shared_variables
    use model_files_variabletypes
    use model_files_variables
!    use climate_forcing

    use RUNCLASS36_constants
    use RUNCLASS36_variables

    use FLAGS

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    type(fl_ids) :: fls

    !> Local variables.
    integer NLTEST, NMTEST
    integer NTYPE, ierr, iun, m, j, ig

    iun = fls%fl(mfk%f50)%iun
    open(iun, &
         file = trim(adjustl(fls%fl(mfk%f50)%fn)), &
         status = 'old', &
         action = 'read', &
         iostat = ierr)

    !> CHECK FILE FOR IOSTAT ERRORS
    if (ierr /= 0) then
        print *
        print *, &
            'MESH_parameters_CLASS.ini could not be opened.', &
            'Ensure that the file exists and restart the program.'
        stop
    else if (ro%VERBOSEMODE > 0) then
        write(6, '(a)', advance = 'no') 'READING: MESH_parameters_CLASS.ini '
    end if

1000    format(2x, 6a4)

    read(iun, 1000) TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6
    read(iun, 1000) NAME1, NAME2, NAME3, NAME4, NAME5, NAME6
    read(iun, 1000) PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
    read(iun, *) DEGLAT, DEGLON, cp%ZRFMGRD(1), cp%ZRFHGRD(1), cp%ZBLDGRD(1), cp%GCGRD(1), shd%wc%ILG, NLTEST, NMTEST

    if (shd%lc%NTYPE /= NMTEST .and. shd%lc%NTYPE > 0) then
        print *
        print *, 'GRUs from MESH_parameters_CLASS.ini: ', NMTEST
        print *, 'GRUs from basin watershed file: ', shd%lc%NTYPE
        print *, 'These values must be equal.'
	    stop
    end if

!todo - fix this so that we only use one of the variables (use NA and ignore NLTEST - doc)
    if (NLTEST /= shd%NA) then
        print *
        print *, &
            'ERROR: The number of grid squares in the class ', &
            'parameters file does not match the number of grid squares ', &
            'from the shed file.'
        stop
    end if

    if (NRSOILAYEREADFLAG == 1) then
        ig = shd%lc%IGND
    else
        ig = 3
    end if

    JLAT = nint(DEGLAT)

    do m = 1, NMTEST
        read(iun, *) (cp%FCANROW(1, m, j), j = 1, ICAN + 1), (cp%PAMXROW(1, m, j), j = 1, ICAN)
        read(iun, *) (cp%LNZ0ROW(1, m, j), j = 1, ICAN + 1), (cp%PAMNROW(1, m, j), j = 1, ICAN)
        read(iun, *) (cp%ALVCROW(1, m, j), j = 1, ICAN + 1), (cp%CMASROW(1, m, j), j = 1, ICAN)
        read(iun, *) (cp%ALICROW(1, m, j), j = 1, ICAN + 1), (cp%ROOTROW(1, m, j), j = 1, ICAN)
        read(iun, *) (cp%RSMNROW(1, m, j), j = 1, ICAN), (cp%QA50ROW(1, m, j), j = 1, ICAN)
        read(iun, *) (cp%VPDAROW(1, m, j), j = 1, ICAN), (cp%VPDBROW(1, m, j), j = 1, ICAN)
        read(iun, *) (cp%PSGAROW(1, m, j), j = 1, ICAN), (cp%PSGBROW(1, m, j), j = 1, ICAN)
        read(iun, *) cp%DRNROW(1, m), cp%SDEPROW(1, m), cp%FAREROW(1, m), cp%DDROW(1, m)
        read(iun, *) cp%XSLPROW(1, m), cp%XDROW(1, m), cp%MANNROW(1, m), cp%KSROW(1, m), cp%MIDROW(1, m)
        read(iun, *) (cp%SANDROW(1, m, j), j = 1, ig)
        read(iun, *) (cp%CLAYROW(1, m, j), j = 1, ig)
        read(iun, *) (cp%ORGMROW(1, m, j), j = 1, ig)
        read(iun, *) (cp%TBARROW(1, m, j), j = 1, ig), cp%TCANROW(1, m), cp%TSNOROW(1, m), cp%TPNDROW(1, m)
        read(iun, *) (cp%THLQROW(1, m, j), j = 1, ig), (cp%THICROW(1, m, j), j = 1, ig), cp%ZPNDROW(1, m)
        read(iun, *) cp%RCANROW(1, m), cp%SCANROW(1, m), cp%SNOROW(1, m), cp%ALBSROW(1, m), cp%RHOSROW(1, m), cp%GROROW(1, m)
    end do

!todo - make sure these variables are documented properly
    read(iun, *) HOURLY_START_DAY, HOURLY_STOP_DAY, DAILY_START_DAY, DAILY_STOP_DAY
    read(iun, *) HOURLY_START_YEAR, HOURLY_STOP_YEAR, DAILY_START_YEAR, DAILY_STOP_YEAR

    !> Read in hour, minute, day and year from class.ini file as it is
    !> not present in the forcing files
    read(iun, *) IHOUR, IMIN, IDAY, IYEAR

    !> Close the file.
    close(iun)
    if (ro%VERBOSEMODE > 0) print *, 'READ: SUCCESSFUL, FILE: CLOSED'

    !> Convert DD from km/km^2 to m/m^2
    !> The formulae in WATROF.f expect m/m^2
    do m = 1, NMTEST
        cp%DDROW(1, m) = cp%DDROW(1, m)/1000.0
    end do

    return

end subroutine
