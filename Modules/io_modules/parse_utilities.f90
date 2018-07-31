!> Description:
!>  Module that contains subroutines and functions for parsing lines
!>  read from simple text and/or CSV format files.
module parse_utilities

    implicit none

    private assign_line_args_fval, assign_line_args_ival

    !> Description:
    !>  Assign the arguments provided from a string to the given field.
    !>
    !> Input/output variables:
    !*  field: Field of size 'nfield' (allocated), assigned values from 'args'.
    !*  nfield: Size of 'field'.
    !*  args: Values to assign to 'field'.
    !*  nargs: Size of 'args'.
    !*  ierr: Conversion/error status.
    interface assign_line_args_vector
        module procedure assign_line_args_fval
        module procedure assign_line_args_ival
    end interface

    !> Description:
    !>  Type for error keys (to be interpreted by called routines).
    !>
    !> Variables:
    !*  COUNT_MISMATCH: When the number of values in 'args' does not match the expected number.
    !*  BAD_ASSIGN: When an error occured converting the type of the variable to assign to the value.
    type error_keys
        integer :: COUNT_MISMATCH = 1
        integer :: BAD_ASSIGN = 2
    end type

    !* pserr: Instance of error keys.
    type(error_keys), save :: pserr

    contains

    subroutine assign_line_args_fval(field, nfield, args, nargs, ierr)

        !> strings: For 'value' function.
        use strings

        !> Input/output variables.
        integer, intent(in) :: nfield, nargs
        character(len = *), dimension(nargs), intent(in) :: args
        real, dimension(nfield) :: field
        integer ierr

        !> Local variables.
        integer i
        real fval

        !> Check dimensions.
        if (nargs < nfield) then
            ierr = pserr%COUNT_MISMATCH
            return
        end if

        !> Extract the fields.
        ierr = 0
        do i = 1, nfield
            call value(args(i), field(i), ierr)
            if (ierr /= 0) exit
        end do

        !> Check error status.
        if (ierr /= 0) ierr = pserr%BAD_ASSIGN

        return

    end subroutine

    subroutine assign_line_args_ival(field, nfield, args, nargs, ierr)

        !> Input/output variables.
        integer, intent(in) :: nfield, nargs
        character(len = *), dimension(nargs), intent(in) :: args
        integer, dimension(nfield) :: field
        integer ierr

        !> Local variables.
        real, dimension(nfield) :: fval

        !> Call subroutine for type real.
        call assign_line_args_fval(fval, nfield, args, nargs, ierr)

        !> Assign field.
        if (ierr == 0) field = int(fval)

        return

    end subroutine

end module
