subroutine READ_PARAMETERS_CLASS(shd, fls, cm)

    !> Required for file object and CLASS.ini file index.
    use model_files_variables

    !> For the 'ShedGridParams' type and SA_MESH parameters.
    use sa_mesh_variables
    use sa_mesh_utilities

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
    character(len = DEFAULT_LINE_LENGTH) line

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
        call print_error(trim(adjustl(fls%fl(mfk%f50)%fn)) // ' could not be opened.')
        call print_message('Ensure that the file exists and restart the program.')
        call stop_program()
    else
        call print_message('READING: ' // trim(adjustl(fls%fl(mfk%f50)%fn)))
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
    ierr = 0
    if (NTYPE /= m .and. NTYPE > 0) then
        call print_error('The number of GRUs does not match the drainage database.')
        write(line, 1001) NTYPE
        call print_message_detail('Drainage database: ' // trim(adjustl(line)))
        write(line, 1001) m
        call print_message_detail(trim(adjustl(fls%fl(mfk%f50)%fn)) // ': ' // trim(adjustl(line)))
        ierr = 1
    end if

    !> Check that the number of grid cells matches the drainage database value.
    if (i /= NA) then
        call print_error('The number of GRUs does not match the drainage database.')
        write(line, 1001) NA
        call print_message_detail('Drainage database: ' // trim(adjustl(line)))
        write(line, 1001) i
        call print_message_detail(trim(adjustl(fls%fl(mfk%f50)%fn)) // ': ' // trim(adjustl(line)))
        ierr = 1
    end if

    !> Stop if an error has occurred.
    if (ierr /= 0) call stop_program()

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
    if (SHDFILEFMT == 2) then
        shd%ylat = DEGLAT
        shd%xlng = DEGLON
    end if

    return

    !> Format statements.
1001    format(9999(g15.6, 1x))

end subroutine
