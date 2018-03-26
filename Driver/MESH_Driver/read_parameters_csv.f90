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
subroutine read_parameters_csv(shd, iun, fname)

    !> strings: For 'readline', 'compact', 'parse', and 'lowercase' functions.
    !> sa_mesh_variables: Required for MESH variables, parameters.
    !> sa_mesh_utilities: Required for printing/writing messages, DIAGNOSEMODE.
    !> parse_utilities: For 'assign_line_args_vector' function.
    use strings
    use sa_mesh_variables
    use sa_mesh_utilities
    use parse_utilities

    !> Process modules: Required for process variables, parameters.
    use baseflow_module
    use rte_module

    implicit none

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Local variables.
    integer nargs, n, i, istat, ierr
    real fval
    character(len = DEFAULT_LINE_LENGTH) line
    character(len = DEFAULT_FIELD_LENGTH), dimension(50) :: args

    !> Open the file.
    call print_screen('READING: ' // trim(adjustl(fname)))
    call print_echo_txt(fname)
    open(iun, file = fname, status = 'old', action = 'read', iostat = ierr)
    if (ierr /= 0) then
        call print_error('Unable to open file. Check if the file exists or use an alternate parameter file format.')
        call stop_program()
    end if

    !> Read and parse each line.
    ierr = 0
    do while (ierr == 0)

        !> Compact and reduce the line to any instance of '#' or '!'.
        call readline(iun, line, ierr)
        if (ierr /= 0) exit
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
        n = 0
        select case (lowercase(args(1)))

            !> BASEFLOWFLAG == 2 (lower zone storage).
            case ('pwr')
                if (.not. bflm%BASEFLOWFLAG /= 2) then
                    istat = 1
                else if ((nargs - 1) /= shd%NRVR) then
                    istat = 3
                else
                    call assign_line_args_vector(bflm%pm_iak%pwr, shd%NRVR, args(2:), nargs, istat)
                end if
            case ('flz')
                if (.not. bflm%BASEFLOWFLAG /= 2) then
                    istat = 1
                else if ((nargs - 1) /= shd%NRVR) then
                    istat = 3
                else
                    call assign_line_args_vector(bflm%pm_iak%flz, shd%NRVR, args(2:), nargs, istat)
                end if

            !> RPN RTE (Watflood, 2007).
            case ('r2n')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) /= shd%NRVR) then
                    istat = 3
                else
                    call assign_line_args_vector(rtepm_iak%r2n, shd%NRVR, args(2:), nargs, istat)
                end if
            case ('r1n')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) /= shd%NRVR) then
                    istat = 3
                else
                    call assign_line_args_vector(rtepm_iak%r1n, shd%NRVR, args(2:), nargs, istat)
                end if
            case ('mndr')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) /= shd%NRVR) then
                    istat = 3
                else
                    call assign_line_args_vector(rtepm_iak%mndr, shd%NRVR, args(2:), nargs, istat)
                end if
            case ('aa2')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) /= shd%NRVR) then
                    istat = 3
                else
                    call assign_line_args_vector(rtepm_iak%aa2, shd%NRVR, args(2:), nargs, istat)
                end if
            case ('aa3')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) /= shd%NRVR) then
                    istat = 3
                else
                    call assign_line_args_vector(rtepm_iak%aa3, shd%NRVR, args(2:), nargs, istat)
                end if
            case ('aa4')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) /= shd%NRVR) then
                    istat = 3
                else
                    call assign_line_args_vector(rtepm_iak%aa4, shd%NRVR, args(2:), nargs, istat)
                end if
            case ('widep')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    istat = 1
                else if ((nargs - 1) /= shd%NRVR) then
                    istat = 3
                else
                    call assign_line_args_vector(rtepm_iak%widep, shd%NRVR, args(2:), nargs, istat)
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
            call print_warning("'" // trim(adjustl(args(1))) // "' does not contain the expected number of values.")
        else if (istat /= 0) then
            call print_warning("Error assigning '" // trim(adjustl(args(1))) // "' values.")
        else if (istat == 0) then
            n = n + 1
        end if
    end do

    !> Print number of active parameters.
    write(line, 1001) n
    call print_message_detail('Active parameters in file: ' // trim(adjustl(line)))

    !> Close the file to free the unit.
    close(iun)

    return

    !> Format statements.
1001    format(9999(g15.6, 1x))

end subroutine
