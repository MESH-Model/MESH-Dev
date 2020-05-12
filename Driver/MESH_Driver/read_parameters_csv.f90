!>
!> Description:
!>  Subroutine to read parameters from file, in delimited format.
!>  Parameter values are saved directly to the shared parameter object
!>  at the GRU and NRVR levels, accessible by 'sa_mesh_variables'.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
!>
subroutine read_parameters_csv(shd, iun, fname, ierr)

    !> strings: For 'readline', 'compact', 'parse', 'uppercase' and 'lowercase' functions.
    !> sa_mesh_common: For common MESH variables and routines.
    !> parse_utilities: For 'assign_line_args_vector' function.
    use strings
    use sa_mesh_common
    use parse_utilities
    use FLAGS

    !> Process modules: Required for process variables, parameters.
    use baseflow_module
    use rte_module
    use runsvs_mesh

    implicit none

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer nargs, p, n, j, i, ignd, istat, iconv, z
    real fval
    character(len = DEFAULT_LINE_LENGTH) line
    character(len = DEFAULT_FIELD_LENGTH) field1, field2
    character(len = DEFAULT_FIELD_LENGTH), dimension(50) :: args

    !> Initialize the return status.
    ierr = 0

    !> Open the file.
    call reset_tab()
    call print_message('READING: ' // trim(adjustl(fname)))
    call increase_tab()
    open(iun, file = fname, status = 'old', action = 'read', iostat = ierr)
    if (ierr /= 0) then
        call print_error('Unable to open the file. Check if the file exists.')
        return
    end if

    !> Read and parse each line.
    n = 0
    z = 0
    do while (z == 0)

        !> Compact and reduce the line to any instance of '#' or '!'.
        call readline(iun, line, z)
        if (z /= 0) exit
        if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
        if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
        call compact(line)

        !> Replace commas with spaces and parse the fields in the line.
        do i = 1, len_trim(line)
            if (line(i:i) == ',') line(i:i) = ' '
        end do
        call parse(line, ' ', args, nargs)

        !> Cycle if no arguments exist.
        if (nargs < 1) cycle

        !> Assign and distribute the field.
        if (DIAGNOSEMODE) call print_message('Reading parameter: ' // trim(adjustl(args(1))) // '.')
        istat = 0
        select case (uppercase(args(1)))

            !> SVS (unique variables).
            case (VN_SVS_DEGLAT)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%deglat)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%deglat(p))
                    end if
                    svs_mesh%vs%deglat = 0.0
                    call assign_line_args_vector(svs_mesh%vs%deglat, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_DEGLNG)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%deglng)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%deglng(p))
                    end if
                    svs_mesh%vs%deglng = 0.0
                    call assign_line_args_vector(svs_mesh%vs%deglng, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_OBSERVED_FORCING)
                p = 1
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (lowercase(args(2)) == 'height' .or. lowercase(args(2)) == 'on' .or. lowercase(args(2)) == '.true.') then
                        svs_mesh%vs%observed_forcing = .true.
                    else
                        svs_mesh%vs%observed_forcing = .false.
                    end if
                end if
            case (VN_SVS_ZUSL)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%zusl)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%zusl(p))
                    end if
                    svs_mesh%vs%zusl = 0.0
                    call assign_line_args_vector(svs_mesh%vs%zusl, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_ZTSL)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%ztsl)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%ztsl(p))
                    end if
                    svs_mesh%vs%ztsl = 0.0
                    call assign_line_args_vector(svs_mesh%vs%ztsl, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_SIGMA_U)
                p = 1
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call value(args(2), svs_mesh%vs%sigma_u, iconv)
                end if
            case (VN_SVS_SIGMA_T)
                p = 1
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call value(args(2), svs_mesh%vs%sigma_t, iconv)
                end if
            case (VN_SVS_SLOP)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%slop)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%slop(p))
                    end if
                    svs_mesh%vs%slop = 0.0
                    call assign_line_args_vector(svs_mesh%vs%slop, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_DRAINDENS)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%draindens)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%draindens(p))
                    end if
                    svs_mesh%vs%draindens = 0.0
                    call assign_line_args_vector(svs_mesh%vs%draindens, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_SOILTEXT)
                p = 1
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    svs_mesh%vs%soiltext = trim(adjustl(args(2)))
                end if
            case (VN_SVS_KHYD)
                p = 1
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call value(args(2), svs_mesh%vs%khyd, iconv)
                end if
            case (VN_SVS_SAND)
                if (NRSOILAYEREADFLAG > 3) then
                    p = min(NRSOILAYEREADFLAG, shd%lc%IGND)
                else if (NRSOILAYEREADFLAG == 1) then
                    p = shd%lc%IGND
                else
                    p = 3
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%sand)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%sand(1, shd%lc%IGND))
                    end if
                    svs_mesh%vs%sand = 0.0
                    call assign_line_args_vector(svs_mesh%vs%sand(1, 1:p), p, args(2:), nargs, iconv)
                    if (p < shd%lc%IGND) then
                        do j = (p + 1), shd%lc%IGND
                            svs_mesh%vs%sand(:, j) = svs_mesh%vs%sand(:, p)
                            svs_mesh%vs%sand(:, j) = svs_mesh%vs%sand(:, p)
                            svs_mesh%vs%sand(:, j) = svs_mesh%vs%sand(:, p)
                        end do
                    end if
                end if
            case (VN_SVS_CLAY)
                if (NRSOILAYEREADFLAG > 3) then
                    p = min(NRSOILAYEREADFLAG, shd%lc%IGND)
                else if (NRSOILAYEREADFLAG == 1) then
                    p = 0
                else
                    p = 3
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%clay)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%clay(1, shd%lc%IGND))
                    end if
                    svs_mesh%vs%clay = 0.0
                    call assign_line_args_vector(svs_mesh%vs%clay(1, 1:p), p, args(2:), nargs, iconv)
                    if (p < shd%lc%IGND) then
                        do j = (p + 1), shd%lc%IGND
                            svs_mesh%vs%clay(:, j) = svs_mesh%vs%clay(:, p)
                            svs_mesh%vs%clay(:, j) = svs_mesh%vs%clay(:, p)
                            svs_mesh%vs%clay(:, j) = svs_mesh%vs%clay(:, p)
                        end do
                    end if
                end if
            case (VN_SVS_WSOIL)
                if (NRSOILAYEREADFLAG > 3) then
                    p = min(NRSOILAYEREADFLAG, shd%lc%IGND)
                else if (NRSOILAYEREADFLAG == 1) then
                    p = 0
                else
                    p = 3
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%wsoil)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%wsoil(1, shd%lc%IGND))
                    end if
                    svs_mesh%vs%wsoil = 0.0
                    call assign_line_args_vector(svs_mesh%vs%wsoil(1, 1:p), p, args(2:), nargs, iconv)
                    if (p < shd%lc%IGND) then
                        do j = (p + 1), shd%lc%IGND
                            svs_mesh%vs%wsoil(:, j) = svs_mesh%vs%wsoil(:, p)
                            svs_mesh%vs%wsoil(:, j) = svs_mesh%vs%wsoil(:, p)
                            svs_mesh%vs%wsoil(:, j) = svs_mesh%vs%wsoil(:, p)
                        end do
                    end if
                end if
            case (VN_SVS_ISOIL)
                if (NRSOILAYEREADFLAG > 3) then
                    p = min(NRSOILAYEREADFLAG, shd%lc%IGND)
                else if (NRSOILAYEREADFLAG == 1) then
                    p = 0
                else
                    p = 3
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%isoil)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%isoil(1, shd%lc%IGND))
                    end if
                    svs_mesh%vs%isoil = 0.0
                    call assign_line_args_vector(svs_mesh%vs%isoil(1, 1:p), p, args(2:), nargs, iconv)
                    if (p < shd%lc%IGND) then
                        do j = (p + 1), shd%lc%IGND
                            svs_mesh%vs%isoil(:, j) = svs_mesh%vs%isoil(:, p)
                            svs_mesh%vs%isoil(:, j) = svs_mesh%vs%isoil(:, p)
                            svs_mesh%vs%isoil(:, j) = svs_mesh%vs%isoil(:, p)
                        end do
                    end if
                end if
            case (VN_SVS_TGROUND)
                p = svs_mesh%vs%kthermal
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%tground)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%tground(1, p))
                    end if
                    svs_mesh%vs%tground = 0.0
                    call assign_line_args_vector(svs_mesh%vs%tground(1, :), p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_VF)
                p = svs_mesh%c%NLANDCLASS
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%vf)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%vf(1, p))
                    end if
                    svs_mesh%vs%vf = 0.0
                    call assign_line_args_vector(svs_mesh%vs%vf(1, :), p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_Z0V, 'Z0')
                p = svs_mesh%c%NLANDCLASS
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%z0v)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%z0v(1, p))
                    end if
                    svs_mesh%vs%z0v = 0.0
                    call assign_line_args_vector(svs_mesh%vs%z0v(1, :), p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_TVEGE)
                p = 2
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%tvege)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%tvege(1, p))
                    end if
                    svs_mesh%vs%tvege = 0.0
                    call assign_line_args_vector(svs_mesh%vs%tvege(1, :), p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_WVEG)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%wveg)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%wveg(p))
                    end if
                    svs_mesh%vs%wveg = 0.0
                    call assign_line_args_vector(svs_mesh%vs%wveg, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_TSNOW)
                p = 2
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%tsnow)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%tsnow(1, p))
                    end if
                    svs_mesh%vs%tsnow = 0.0
                    call assign_line_args_vector(svs_mesh%vs%tsnow(1, :), p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_SNODPL)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%snodpl)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%snodpl(p))
                    end if
                    svs_mesh%vs%snodpl = 0.0
                    call assign_line_args_vector(svs_mesh%vs%snodpl, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_SNODEN)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%snoden)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%snoden(p))
                    end if
                    svs_mesh%vs%snoden = 0.0
                    call assign_line_args_vector(svs_mesh%vs%snoden, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_SNOAL)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%snoal)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%snoal(p))
                    end if
                    svs_mesh%vs%snoal = 0.0
                    call assign_line_args_vector(svs_mesh%vs%snoal, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_WSNOW)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%wsnow)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%wsnow(p))
                    end if
                    svs_mesh%vs%wsnow = 0.0
                    call assign_line_args_vector(svs_mesh%vs%wsnow, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_TSNOWVEG)
                p = 2
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%tsnowveg)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%tsnowveg(1, p))
                    end if
                    svs_mesh%vs%tsnowveg = 0.0
                    call assign_line_args_vector(svs_mesh%vs%tsnowveg(1, :), p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_SNVDP)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%snvdp)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%snvdp(p))
                    end if
                    svs_mesh%vs%snvdp = 0.0
                    call assign_line_args_vector(svs_mesh%vs%snvdp, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_SNVDEN)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%snvden)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%snvden(p))
                    end if
                    svs_mesh%vs%snvden = 0.0
                    call assign_line_args_vector(svs_mesh%vs%snvden, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_SNVAL)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%snval)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%snval(p))
                    end if
                    svs_mesh%vs%snval = 0.0
                    call assign_line_args_vector(svs_mesh%vs%snval, p, args(2:), nargs, iconv)
                end if
            case (VN_SVS_WSNV)
                if (SHDFILEFMT == 2) then
                    p = shd%lc%NML
                else
                    p = shd%lc%NTYPE
                end if
                if (.not. svs_mesh%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    if (allocated(svs_mesh%vs%wsnv)) then
                        istat = 3
                    else
                        allocate(svs_mesh%vs%wsnv(p))
                    end if
                    svs_mesh%vs%wsnv = 0.0
                    call assign_line_args_vector(svs_mesh%vs%wsnv, p, args(2:), nargs, iconv)
                end if

            !> BASEFLOWFLAG == 2 (lower zone storage).
            case ('PWR')
                p = shd%NRVR
                if (.not. bflm%BASEFLOWFLAG /= 2) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call assign_line_args_vector(bflm%pm_iak%pwr, p, args(2:), nargs, iconv)
                end if
            case ('FLZ')
                p = shd%NRVR
                if (.not. bflm%BASEFLOWFLAG /= 2) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call assign_line_args_vector(bflm%pm_iak%flz, p, args(2:), nargs, iconv)
                end if

            !> RPN RTE (Watflood, 2007).
            case ('R2N')
                p = shd%NRVR
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call assign_line_args_vector(rtepm_iak%r2n, p, args(2:), nargs, iconv)
                end if
            case ('R1N')
                p = shd%NRVR
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call assign_line_args_vector(rtepm_iak%r1n, p, args(2:), nargs, iconv)
                end if
            case ('MNDR')
                p = shd%NRVR
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call assign_line_args_vector(rtepm_iak%mndr, p, args(2:), nargs, iconv)
                end if
            case ('AA2')
                p = shd%NRVR
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call assign_line_args_vector(rtepm_iak%aa2, p, args(2:), nargs, iconv)
                end if
            case ('AA3')
                p = shd%NRVR
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call assign_line_args_vector(rtepm_iak%aa3, p, args(2:), nargs, iconv)
                end if
            case ('AA4')
                p = shd%NRVR
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call assign_line_args_vector(rtepm_iak%aa4, p, args(2:), nargs, iconv)
                end if
            case ('WIDEP')
                p = shd%NRVR
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) == p) then
                    call assign_line_args_vector(rtepm_iak%widep, p, args(2:), nargs, iconv)
                end if

            !> Unrecognized.
            case default
                istat = 2
        end select

        !> Status flags.
        if (istat == 1 .and. DIAGNOSEMODE) then
            call print_remark("'" // trim(adjustl(args(1))) // "' is present but inactive.")
        else if (istat == 2) then
            call print_warning("'" // trim(adjustl(args(1))) // "' is not recognized.")
        else if (istat == 3) then
            call print_remark("'" // trim(adjustl(args(1))) // &
                "' was already assigned but has been overwritten by a succeeding entry in the list.")
        else if ((nargs - 1) > p) then
            write(field1, FMT_GEN) (nargs - 1)
            write(field2, FMT_GEN) p
            call print_warning( &
                "'" // trim(adjustl(args(1))) // "' contains " // trim(adjustl(field1)) // " fields but only " // &
                trim(adjustl(field2)) // " are being used.")
        else if ((nargs - 1) /= p) then
            write(field1, FMT_GEN) (nargs - 1)
            write(field2, FMT_GEN) p
            call print_warning( &
                "'" // trim(adjustl(args(1))) // "' only contains " // trim(adjustl(field1)) // " fields when " // &
                trim(adjustl(field2)) // " are expected.")
        else if (istat /= 0) then
            call print_warning("An error occured reading '" // trim(adjustl(args(1))) // "' from the file.")
        else if (iconv /= 0) then
            call print_warning("An error occured assigning '" // trim(adjustl(args(1))) // "' values.")
        else if (istat == 0) then
            n = n + 1
        end if
    end do

    !> Print number of active parameters.
    write(line, FMT_GEN) n
    call print_message('Active parameters in file: ' // trim(adjustl(line)))

    !> Close the file to free the unit.
    close(iun)

    return

end subroutine
