!>
!> Description:
!>  Module that contains subroutines and functions for parsing lines
!>  read from file for simple text and CSV format.
!>
module parse_utilities

    implicit none

    private assign_parameters_fval, assign_parameters_ival

    !> Description:
    !>  Assign the arguments provided from a string to the given field.
    !>
    !> Arguments:
    !*  - field: Field of size 'nfield'.
    !*  - nfield: Size of 'field'.
    !*  - in_line: Original line parsed to 'args'.
    !*  - args: Fields extracted from 'in_line' of size 'nargs'.
    !*  - nargs: Size of 'args'.
    !*  - ipid: Index of current node, used in printing output (0: print; 1: don't print).
    !*  - istat: Status of conversion error(s) (0: no error; 1: error(s)).
    !*  - verbose: Verbosity of non-critical output (.true.: print warnings; .false.: ignore warnings).
    interface assign_parameters
        module procedure assign_parameters_fval
        module procedure assign_parameters_ival
    end interface

    contains

    subroutine assign_parameters_fval(field, nfield, in_line, args, nargs, ipid, istat, verbose)

        !> For: 'value'.
        use strings

        !> Arguments.
        integer, intent(in) :: nfield, nargs, ipid
        character(len = *), intent(in) :: in_line
        character(len = *), dimension(nargs), intent(in) :: args
        real, dimension(nfield) :: field
        integer istat
        logical, intent(in) :: verbose

        !> Local variables.
        integer i
        real fval

        !> Extract the fields.
        istat = 0
        if (nargs == 2) then

            !> Assign to all 'nfield' if only one value.
            call value(args(2), fval, istat)
            field = fval
        else if (nargs > nfield) then

            !> Assign values.
            do i = 1, nfield
                call value(args(i + 1), field(i), istat)
            end do
        else

            !> Bad number of arguments.
            if (verbose) print 1020, trim(adjustl(args(1))), nargs
        end if

        !> Check for conversion error.
        if (istat /= 0) then
            if (ipid == 0) print 1010, trim(adjustl(in_line))
        end if

        return

1010    format(1x, 'ERROR: Assigning parameter ', (a))
1020    format(3x, 'WARNING: The parameter ', (a), ' contains no values. NARGS =', i2)

    end subroutine

    subroutine assign_parameters_ival(field, nfield, in_line, args, nargs, ipid, istat, verbose)

        !> Arguments.
        integer, intent(in) :: nfield, nargs, ipid
        character(len = *), intent(in) :: in_line
        character(len = *), dimension(nargs), intent(in) :: args
        integer, dimension(nfield) :: field
        integer istat
        logical, intent(in) :: verbose

        !> Local variables.
        real, dimension(nfield) :: fval

        !> Call subroutine for type real.
        call assign_parameters_fval(fval, nfield, in_line, args, nargs, ipid, istat, verbose)

        !> Assign field.
        if (istat == 0) field = int(fval)

        return

    end subroutine

end module
