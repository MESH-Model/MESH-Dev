!>
!> Description:
!>  Subroutine to read parameters from file, in delimited format.
!>  Parameter values are saved directly to the shared parameter object
!>  at the GRU and NRVR levels, accessible by sa_mesh_shared_variables.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
!>
subroutine read_parameters_csv(shd, iun, fname)

    use strings
    use parse_utilities
    use mpi_module
    use sa_mesh_shared_variables

    use rte_module

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer :: iun
    character(len = *) :: fname

    !> Local variables.
    integer ierr, istat, iconvert, i, ival
    real fval

    !> Local variables for parsing lines.
    integer, parameter :: MaxLenField = 100, MaxArgs = 100, MaxLenLine = 2000
    character(MaxLenLine) in_line
    character(MaxLenField), dimension(MaxArgs) :: args
    integer nargs

    !> Open the file.
    if (ro%VERBOSEMODE > 0) print 1010, trim(adjustl(fname))
    open(iun, file = trim(adjustl(fname)), status = 'old', iostat = ierr)
    if (ierr /= 0) then
        if (ipid == 0) print 1020, trim(adjustl(fname))
        stop
    end if

    !> Read and parse each line.
    ierr = 0
    istat = 0
    do while (istat == 0)

        !> Compact and reduce the line to any instance of '#' or '!'.
        call readline(iun, in_line, istat)
        if (istat /= 0) exit
        if (index(in_line, '#') > 2) in_line = in_line(1:index(in_line, '#') - 1)
        if (index(in_line, '!') > 2) in_line = in_line(1:index(in_line, '!') - 1)
        call compact(in_line)

        !> Replace commas with spaces and parse the fields in the line.
        do i = 1, len_trim(in_line)
            if (in_line(i:i) == ',') in_line(i:i) = ' '
        end do
        call parse(in_line, ' ', args, nargs)

        !> Cycle if no arguments exist.
        if (nargs < 1) cycle

        !> Assign and distribute the field.
        iconvert = 0
        select case (lowercase(args(1)))

            !> RPN RTE (Watflood, 2007).
            case ('r2n')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) print 9160, trim(adjustl(args(1)))
                    cycle
                else
                    call assign_parameters(rtepm_iak%r2n, shd%NRVR, in_line, args, nargs, ipid, iconvert, ro%VERBOSEMODE > 0)
                end if
            case ('r1n')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) print 9160, trim(adjustl(args(1)))
                    cycle
                else
                    call assign_parameters(rtepm_iak%r1n, shd%NRVR, in_line, args, nargs, ipid, iconvert, ro%VERBOSEMODE > 0)
                end if
            case ('flz')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) print 9160, trim(adjustl(args(1)))
                    cycle
                else
                    call assign_parameters(rtepm_iak%flz, shd%NRVR, in_line, args, nargs, ipid, iconvert, ro%VERBOSEMODE > 0)
                end if
            case ('pwr')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) print 9160, trim(adjustl(args(1)))
                    cycle
                else
                    call assign_parameters(rtepm_iak%pwr, shd%NRVR, in_line, args, nargs, ipid, iconvert, ro%VERBOSEMODE > 0)
                end if
            case ('mndr')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) print 9160, trim(adjustl(args(1)))
                    cycle
                else
                    call assign_parameters(rtepm_iak%mndr, shd%NRVR, in_line, args, nargs, ipid, iconvert, ro%VERBOSEMODE > 0)
                end if
            case ('aa2')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) print 9160, trim(adjustl(args(1)))
                    cycle
                else
                    call assign_parameters(rtepm_iak%aa2, shd%NRVR, in_line, args, nargs, ipid, iconvert, ro%VERBOSEMODE > 0)
                end if
            case ('aa3')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) print 9160, trim(adjustl(args(1)))
                    cycle
                else
                    call assign_parameters(rtepm_iak%aa3, shd%NRVR, in_line, args, nargs, ipid, iconvert, ro%VERBOSEMODE > 0)
                end if
            case ('aa4')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) print 9160, trim(adjustl(args(1)))
                    cycle
                else
                    call assign_parameters(rtepm_iak%aa4, shd%NRVR, in_line, args, nargs, ipid, iconvert, ro%VERBOSEMODE > 0)
                end if
            case ('widep')
                if (.not. rteflg%PROCESS_ACTIVE) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) print 9160, trim(adjustl(args(1)))
                    cycle
                else
                    call assign_parameters(rtepm_iak%widep, shd%NRVR, in_line, args, nargs, ipid, iconvert, ro%VERBOSEMODE > 0)
                end if

            !> Unrecognized.
            case default
                if (ro%VERBOSEMODE > 0) print 9170, trim(adjustl(args(1)))
        end select

    end do

9160    format(3x, 'REMARK: Inactive parameter: ', (a))
9170    format(3x, 'WARNING: Unrecognized parameter: ', (a))
9180    format(3x, 'WARNING: The parameter ', (a), ' contains no values. NARGS =', i2)
9190    format(1x, 'ERROR: Assigning parameter ', (a))

    !> Stop if errors exist.
    if (ierr > 0) stop

    !> Close the file to free the unit.
    close(iun)

1010    format(1x, 'READING: ', (a))
1020    format(/1x, 'ERROR: Opening ', (a), &
               /3x, 'Check that the file exists or use an alternate parameter file form.')

    return

end subroutine
