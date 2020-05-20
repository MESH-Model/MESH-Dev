!> Description:
!>  Module that contains subroutines and functions for parsing lines
!>  read from simple text and/or CSV format files.
module parse_utilities

    !> 'strings': For 'compact', 'parse' and 'value' functions.
    use strings

    implicit none

    !> Description:
    !>  Check the provided field is allocated and assigned.
    !>
    !> Input/output variables:
    !*  field: Field to check if allocated and assigned.
    !*  size1: Expected size of field (if allocated, and if passing a vector).
    !*  istat: Return status (returns 'istat' as provided if the field is allocated and assigned).
    interface check_allocated
        module procedure check_assigned_real
        module procedure check_assigned_integer
        module procedure check_assigned_character
        module procedure check_allocated_real1d
        module procedure check_allocated_integer1d
        module procedure check_allocated_character1d
        module procedure check_allocated_real2d
        module procedure check_allocated_integer2d
    end interface

    !> Description:
    !>  Allocate and initialize the provided field.
    !>
    !> Input/output variables:
    !*  field: Field to check if allocated and assigned.
    !*  size1: Expected size of 'field' in the first dimension (if allocated, and if passing a vector or array).
    !*  size2: Expected size of 'field' in the second dimension (if allocated, and if passing an array).
    !*  istat: Return status (returns 'istat' as provided if allocation returns normal).
    interface allocate_variable
        module procedure allocate_variable_real1d
        module procedure allocate_variable_integer1d
        module procedure allocate_variable_character1d
        module procedure allocate_variable_real2d
        module procedure allocate_variable_integer2d
    end interface

    !> Description:
    !>  Assign the arguments provided from a string to the given field.
    !>
    !> Input/output variables:
    !*  field: Variable to assign values to (returns a warning if already allocated).
    !*  size1: Size of 'field' in the first dimension (if passing a vector or array).
    !*  size2: Size of 'field' in the second dimension (if passing an array).
    !*  args: Values to assign to 'field'.
    !*  nargs: Size of 'args'.
    !*  istat: Return status (0: normal).
    interface assign_line_args
        module procedure assign_line_args_real
        module procedure assign_line_args_integer
        module procedure assign_line_args_character
        module procedure assign_line_args_logical
        module procedure assign_line_args_real1d
        module procedure assign_line_args_integer1d
        module procedure assign_line_args_character1d
        module procedure assign_line_args_real2d
        module procedure assign_line_args_integer2d
    end interface

    !> Description:
    !>  Type for status keys (to be interpreted by called routines).
    !>
    !> Variables:
    !*  NORMAL_STATUS: Normal status (no message).
    !*  COUNT_MISMATCH: When the number of values in 'args' does not match the expected number.
    !*  CONVERSION_ERROR: When an error occured converting the type of the variable to assign to the value.
    !*  BAD_FORMAT: When the provided field is not in the expected format.
    !*  BAD_DATE_FORMAT: When the provided date string is not in the expected format.
    !*  BAD_TIME_FORMAT: When the provided time string is not in the expected format.
    !*  OVERWRITING_FIELD: When a field to be assigned already contains data.
    !*  ALLOCATION_ERROR: When an error occurred while allocating a variable.
    !*  INACTIVE: When the specified field is associated with an inactive process (e.g., may not be used).
    !*  MISMATCHED_PRECISION: When the precision or length of variables to be assigned exceed the precision or length of the field.
    !*  MISMATCHED_BOUNDS: When the size of variables to be assigned is not the same as the field.
    !*  NOT_ALLOCATED: When a variable is determined to be unallocated.
    !*  ASSIGNED: When a variable is determined to contain values (e.g., not still equal to the initialization value).
    !*  UNRECOGNIZED: When the field is not recognized.
    type parse_status_keys
        integer :: NORMAL_STATUS = 0
        integer :: COUNT_MISMATCH = 1
        integer :: CONVERSION_ERROR = 2
        integer :: BAD_FORMAT = 3
        integer :: BAD_DATE_FORMAT = 4
        integer :: BAD_TIME_FORMAT = 5
        integer :: OVERWRITING_FIELD = 6
        integer :: ALLOCATION_ERROR = 7
        integer :: INACTIVE = 8
        integer :: MISMATCHED_PRECISION = 9
        integer :: MISMATCHED_BOUNDS = 10
        integer :: NOT_ALLOCATED = 11
        integer :: ASSIGNED = 12
        integer :: UNRECOGNIZED = 13
    end type

    !* pstat: Instance of status keys.
    type(parse_status_keys), save :: pstat

    !> Description:
    !>  Type of option keys (interpreted by routines).
    !>
    !> Variables:
    !*  MAP_ASSIGN_ORDER1: Assign to first index when provided.
    !*  MAP_ASSIGN_ORDER2: Assign to second index when provided.
    !*  MAP_ASSIGN_ORDER3: Assign to third index when provided.
    type parse_option_keys
        integer :: MAP_ASSIGN_ORDER1 = 1
        integer :: MAP_ASSIGN_ORDER2 = 2
        integer :: MAP_ASSIGN_ORDER3 = 3
    end type

    !* pkey: Instance of option keys.
    type(parse_option_keys), save :: pkey

    contains

    subroutine check_assigned_real(field, istat)

        !> Input variables.
        real, intent(in) :: field

        !> Input/output variables.
        integer istat

        !> Check if the field is assigned.
        if (field /= huge(field)) then
            istat = istat + radix(istat)**pstat%ASSIGNED
        end if

    end subroutine

    subroutine check_assigned_integer(field, istat)

        !> Input variables.
        integer, intent(in) :: field

        !> Input/output variables.
        integer istat

        !> Check if the field is assigned.
        if (field /= huge(field)) then
            istat = istat + radix(istat)**pstat%ASSIGNED
        end if

    end subroutine

    subroutine check_assigned_character(field, istat)

        !> Input variables.
        character(len = *), intent(in) :: field

        !> Input/output variables.
        integer istat

        !> Check if the field is assigned.
        if (field /= achar(0)) then
            istat = istat + radix(istat)**pstat%ASSIGNED
        end if

    end subroutine

    subroutine check_allocated_real1d(field, size1, istat)

        !> Input variables.
        real, dimension(:), allocatable, intent(in) :: field
        integer, intent(in) :: size1

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= huge(field))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (size(field) /= size1) then
                istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    subroutine check_allocated_integer1d(field, size1, istat)

        !> Input variables.
        integer, dimension(:), allocatable, intent(in) :: field
        integer, intent(in) :: size1

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= huge(field))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (size(field) /= size1) then
                istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    subroutine check_allocated_character1d(field, size1, istat)

        !> Input variables.
        character(len = *), dimension(:), allocatable, intent(in) :: field
        integer, intent(in) :: size1

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= achar(0))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (size(field) /= size1) then
                istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    subroutine check_allocated_real2d(field, size1, size2, istat)

        !> Input variables.
        real, dimension(:, :), allocatable, intent(in) :: field
        integer, intent(in) :: size1, size2

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= huge(field))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (size(field, 1) /= size1 .or. size(field, 2) /= size2) then
                istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    subroutine check_allocated_integer2d(field, size1, size2, istat)

        !> Input variables.
        integer, dimension(:, :), allocatable, intent(in) :: field
        integer, intent(in) :: size1, size2

        !> Input/output variables.
        integer istat

        !> Check if the field is allocated.
        if (allocated(field)) then

            !> Check if the field is assigned.
            if (all(field /= huge(field))) then
                istat = istat + radix(istat)**pstat%ASSIGNED
            end if

            !> Check for mismatched bounds.
            if (size(field, 1) /= size1 .or. size(field, 2) /= size2) then
                istat = istat + radix(istat)**pstat%MISMATCHED_BOUNDS
            end if
        else
            istat = istat + radix(istat)**pstat%NOT_ALLOCATED
        end if

    end subroutine

    subroutine allocate_variable_real1d(field, size1, istat)

        !> Input variables.
        integer, intent(in) :: size1

        !> Input/output variables.
        real, dimension(:), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = huge(field)
        end if

    end subroutine

    subroutine allocate_variable_integer1d(field, size1, istat)

        !> Input variables.
        integer, intent(in) :: size1

        !> Input/output variables.
        integer, dimension(:), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = huge(field)
        end if

    end subroutine

    subroutine allocate_variable_character1d(field, size1, istat)

        !> Input variables.
        integer, intent(in) :: size1

        !> Input/output variables.
        character(len = *), dimension(:), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = achar(0)
        end if

    end subroutine

    subroutine allocate_variable_real2d(field, size1, size2, istat)

        !> Input variables.
        integer, intent(in) :: size1, size2

        !> Input/output variables.
        real, dimension(:, :), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1, size2), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = huge(field)
        end if

    end subroutine

    subroutine allocate_variable_integer2d(field, size1, size2, istat)

        !> Input variables.
        integer, intent(in) :: size1, size2

        !> Input/output variables.
        integer, dimension(:, :), allocatable :: field
        integer istat

        !> Local variables.
        integer z

        !> Allocate the variable.
        z = 0
        allocate(field(size1, size2), stat = z)
        if (z /= 0) then
            istat = istat + radix(istat)**pstat%ALLOCATION_ERROR
        else
            field = huge(field)
        end if

    end subroutine

    subroutine assign_line_args_real(field, args, nargs, istat)

        !> strings: For 'value' function.
        use strings

        !> Input variables.
        integer, intent(in) :: nargs
        character(len = *), dimension(nargs), intent(in) :: args

        !> Input/output variables.
        real field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer z

        !> Initialize return variable.
        istat = pstat%NORMAL_STATUS

        !> Check to see if variable allocate (issue warning if already allocated).
        call check_allocated(field, istat)
        if (btest(istat, pstat%ASSIGNED)) then
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if

        !> Check for consistency of values.
        if (nargs /= 1) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Extract the field.
        if (nargs >= 1) then
            z = 0
            call value(args(1), field, z)
            if (z /= 0) then
                istat = istat + radix(istat)**pstat%CONVERSION_ERROR
            end if
        end if

        return

    end subroutine

    subroutine assign_line_args_integer(field, args, nargs, istat)

        !> Input/output variables.
        integer, intent(in) :: nargs
        character(len = *), dimension(nargs), intent(in) :: args
        integer field
        integer, intent(out) :: istat

        !> Local variables.
        real fval
        integer z

        !> Pass the value for internal checks.
        fval = real(field)

        !> Call subroutine for type real.
        z = 0
        call assign_line_args(fval, args, nargs, z)

        !> Assign field.
        field = int(fval)

        !> Pass the status return.
        istat = z

        return

    end subroutine

    subroutine assign_line_args_character(field, args, nargs, istat)

        !> Input variables.
        integer, intent(in) :: nargs
        character(len = *), dimension(nargs), intent(in) :: args

        !> Input/output variables.
        character(len = *) field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer z

        !> Initialize return variable.
        istat = pstat%NORMAL_STATUS

        !> Check to see if variable allocate (issue warning if already allocated).
        call check_allocated(field, istat)
        if (btest(istat, pstat%ASSIGNED)) then
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if

        !> Check for consistency of values.
        if (nargs /= 1) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Check field length.
        if (.not. len(field) >= len(args)) then
            istat = istat + radix(istat)**pstat%MISMATCHED_PRECISION
        end if

        !> Extract the fields.
        if (nargs >= 1) then
            field = adjustl(args(1))
        end if

        return

    end subroutine

    subroutine assign_line_args_logical(field, args, nargs, istat)

        !> strings: For 'lowercase' function.
        use strings

        !> Input variables.
        integer, intent(in) :: nargs
        character(len = *), dimension(nargs), intent(in) :: args

        !> Input/output variables.
        logical field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer z

        !> Initialize return variable.
        istat = pstat%NORMAL_STATUS

        !> Check for consistency of values.
        if (nargs /= 1) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Extract the fields.
        if (nargs >= 1) then
            field = ( &
                args(1) == '1' .or. lowercase(args(1)) == 'on' .or. lowercase(args(1)) == '.true.' .or. &
                lowercase(args(1)) == 'true')
        end if

        return

    end subroutine

    subroutine assign_line_args_real1d(field, size1, args, nargs, istat)

        !> strings: For 'value' function.
        use strings

        !> Input variables.
        integer, intent(in) :: size1, nargs
        character(len = *), dimension(nargs), intent(in) :: args

        !> Input/output variables.
        real, dimension(:), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer i, z

        !> Initialize return variable.
        istat = pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, istat)
        end if

        !> Check for consistency of values.
        if (nargs /= size1) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Extract the fields.
        if (.not. btest(istat, pstat%ALLOCATION_ERROR) .and. nargs >= 1) then
            do i = 1, min(size1, size(field))
                z = 0
                if (i > nargs) then
                    call value(args(nargs), field(i), z)
                else
                    call value(args(i), field(i), z)
                end if
                if (.not. btest(istat, pstat%CONVERSION_ERROR) .and. z /= 0) then
                    istat = istat + radix(istat)**pstat%CONVERSION_ERROR
                end if
            end do
        end if

        return

    end subroutine

    subroutine assign_line_args_integer1d(field, size1, args, nargs, istat)

        !> Input/output variables.
        integer, intent(in) :: size1, nargs
        character(len = *), dimension(nargs), intent(in) :: args
        integer, dimension(:), allocatable :: field
        integer, intent(out) :: istat

        !> Local variables.
        integer i, z

        !> Initialize return variable.
        istat = pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, istat)
        end if

        !> Check for consistency of values.
        if (nargs /= size1) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Extract the fields.
        if (.not. btest(istat, pstat%ALLOCATION_ERROR) .and. nargs >= 1) then
            do i = 1, min(size1, size(field))
                z = 0
                if (i > nargs) then
                    call value(args(nargs), field(i), z)
                else
                    call value(args(i), field(i), z)
                end if
                if (.not. btest(istat, pstat%CONVERSION_ERROR) .and. z /= 0) then
                    istat = istat + radix(istat)**pstat%CONVERSION_ERROR
                end if
            end do
        end if

        return

    end subroutine

    subroutine assign_line_args_character1d(field, size1, args, nargs, istat)

        !> Input variables.
        integer, intent(in) :: size1, nargs
        character(len = *), dimension(nargs), intent(in) :: args

        !> Input/output variables.
        character(len = *), dimension(:), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer i, z

        !> Initialize return variable.
        istat = pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, istat)
        end if

        !> Check for consistency of values.
        if (nargs /= size1) then
            istat = istat + radix(istat)**pstat%COUNT_MISMATCH
        end if

        !> Check field length.
        if (.not. len(field) >= len(args)) then
            istat = istat + radix(istat)**pstat%MISMATCHED_PRECISION
        end if

        !> Extract the fields.
        if (.not. btest(istat, pstat%ALLOCATION_ERROR) .and. nargs >= 1) then
            do i = 1, min(size1, size(field))
                if (i > nargs) then
                    field(i) = adjustl(args(nargs))
                else
                    field(i) = adjustl(args(i))
                end if
            end do
        end if

        return

    end subroutine

    subroutine assign_line_args_real2d(field, size1, size2, args, nargs, map_order, istat, element_id)

        !> strings: For 'value' function.
        use strings

        !> Input variables.
        integer, intent(in) :: size1, size2, nargs, map_order
        character(len = *), dimension(nargs), intent(in) :: args
        integer, intent(in), optional :: element_id

        !> Input/output variables.
        real, dimension(:, :), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        real, dimension(:), allocatable :: fval
        integer i, z

        !> Initialize return variable.
        istat = pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, size2, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, size2, istat)
        end if

        !> Extract the fields.
        z = 0
        call allocate_variable(fval, nargs, z)
        if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
            do i = 1, nargs
                z = 0
                call value(args(i), fval(i), z)
                if (.not. btest(istat, pstat%CONVERSION_ERROR) .and. z /= 0) then
                    istat = istat + radix(istat)**pstat%CONVERSION_ERROR
                end if
            end do

            !> Assign background field.
            if (nargs >= 1) then
                field(:, :) = fval(nargs)
            end if

            !> Check for mapping.
            if (map_order == pkey%MAP_ASSIGN_ORDER1) then

                !> Check for consistency of values.
                if (nargs /= size1) then
                    istat = istat + radix(istat)**pstat%COUNT_MISMATCH
                end if

                !> Assign the values.
                do i = 1, nargs
                    if (present(element_id)) then
                        field(min(i, size1), min(max(element_id, 1), size2)) = fval(i)
                    else
                        field(min(i, size1), :) = fval(i)
                    end if
                end do
            else

                !> Check for consistency of values.
                if (nargs /= size2) then
                    istat = istat + radix(istat)**pstat%COUNT_MISMATCH
                end if

                !> Assign the values.
                do i = 1, nargs
                    if (present(element_id)) then
                        field(min(max(element_id, 1), size1), min(i, size2)) = fval(i)
                    else
                        field(:, min(i, size2)) = fval(i)
                    end if
                end do
            end if
        end if

        return

    end subroutine

    subroutine assign_line_args_integer2d(field, size1, size2, args, nargs, map_order, istat, element_id)

        !> strings: For 'value' function.
        use strings

        !> Input variables.
        integer, intent(in) :: size1, size2, nargs, map_order
        character(len = *), dimension(nargs), intent(in) :: args
        integer, intent(in), optional :: element_id

        !> Input/output variables.
        integer, dimension(:, :), allocatable :: field

        !> Output variables.
        integer, intent(out) :: istat

        !> Local variables.
        integer, dimension(:), allocatable :: ival
        integer i, z

        !> Initialize return variable.
        istat = pstat%NORMAL_STATUS

        !> Check to see if the variable is allocated and assigned.
        call check_allocated(field, size1, size2, istat)
        if (btest(istat, pstat%ASSIGNED)) then

            !> Issue a warning if the variable is already allocated.
            istat = istat + radix(istat)**pstat%OVERWRITING_FIELD
        end if
        if (btest(istat, pstat%NOT_ALLOCATED)) then

            !> Allocate the variable.
            call allocate_variable(field, size1, size2, istat)
        end if

        !> Extract the fields.
        z = 0
        call allocate_variable(ival, nargs, z)
        if (.not. btest(z, pstat%ALLOCATION_ERROR)) then
            do i = 1, nargs
                z = 0
                call value(args(i), ival(i), z)
                if (.not. btest(istat, pstat%CONVERSION_ERROR) .and. z /= 0) then
                    istat = istat + radix(istat)**pstat%CONVERSION_ERROR
                end if
            end do

            !> Assign background field.
            if (nargs >= 1) then
                field(:, :) = ival(nargs)
            end if

            !> Check for mapping.
            if (map_order == pkey%MAP_ASSIGN_ORDER1) then

                !> Check for consistency of values.
                if (nargs /= size1) then
                    istat = istat + radix(istat)**pstat%COUNT_MISMATCH
                end if

                !> Assign the values.
                do i = 1, nargs
                    if (present(element_id)) then
                        field(min(i, size1), min(max(element_id, 1), size2)) = ival(i)
                    else
                        field(min(i, size1), :) = ival(i)
                    end if
                end do
            else

                !> Check for consistency of values.
                if (nargs /= size2) then
                    istat = istat + radix(istat)**pstat%COUNT_MISMATCH
                end if

                !> Assign the values.
                do i = 1, nargs
                    if (present(element_id)) then
                        field(min(max(element_id, 1), size1), min(i, size2)) = ival(i)
                    else
                        field(:, min(i, size2)) = ival(i)
                    end if
                end do
            end if
        end if

        return

    end subroutine

    !> Description:
    !>  Parse the start time in the file header into individual components.
    !>
    !> Input:
    !*  datetime: Date/time structure to parse.
    !>
    !> Output:
    !*  year: Year component of date.
    !*  month: Month component of date.
    !*  day: Day in month component of date.
    !*  hour: Hour in day component of date.
    !*  minutes: Minutes in hour component of date.
    !*  seconds: Seconds in minutes component of date.
    !>
    !> Error return:
    !*  ierr: Return status (of 'parse_status_keys' type value).
    subroutine parse_datetime(datetime, year, month, day, hour, minutes, seconds, ierr)

        !> strings: For 'compact', 'parse' and 'value' functions.
        !> print_routines: For 'DEFAULT_LINE_LENGTH' and 'DEFAULT_FIELD_LENGTH' constants.
        use strings
        use print_routines

        !> Input variables.
        character(len = *), intent(in) :: datetime

        !> Output variables.
        integer, intent(out) :: year, month, day, hour, minutes, seconds, ierr

        !> Local variables.
        integer nargs, n, i, z
        character(len = DEFAULT_FIELD_LENGTH), dimension(DEFAULT_LINE_LENGTH/DEFAULT_FIELD_LENGTH) :: args
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) ctmp

        !> Initially set the values to zero.
        year = 0; month = 0; day = 0; hour = 0; minutes = 0

        !> Initialize the return status.
        ierr = 0

        !> Replace forward-slash with dash.
        line = adjustl(datetime)
        call compact(line)
        do i = 1, len_trim(line)
            if (line(i:i) == '/') line(i:i) = '-'
        end do

        !> Parse date/time text.
        call parse(line, ' ', args, nargs)
        if (.not. nargs > 0) then
            ierr = pstat%BAD_FORMAT
            return
        end if

        !> Find start time in the list of attributes.
        do n = 1, nargs

            !> Scan for a date signature (e.g., 2003-01-31).
            ctmp = adjustl(args(n))
            z = 0
            if (index(ctmp, '-') > 1 .and. index(ctmp, '-') /= index(ctmp, '-', back = .true.)) then
                call value(ctmp(1:(index(ctmp, '-') - 1)), year, z)
                if (z == 0) call value(ctmp((index(ctmp, '-') + 1):(index(ctmp, '-', back = .true.) - 1)), month, z)
                if (z == 0) call value(ctmp((index(ctmp, '-', back = .true.) + 1):len(ctmp)), day, z)
            end if
            if (z /= 0) then
                ierr = pstat%BAD_DATE_FORMAT
                return
            end if

            !> Scan for a time signature (e.g., 24:00:00.000; 24:00:00; 24:00).
            ctmp = adjustl(args(n))
            z = 0
            if (index(ctmp, ':') > 1) then
                call value(ctmp(1:(index(ctmp, ':') - 1)), hour, z)
                if (z == 0 .and. index(ctmp, ':') /= index(ctmp, ':', back = .true.)) then
                    call value(ctmp((index(ctmp, ':') + 1):(index(ctmp, ':', back = .true.) - 1)), minutes, z)
                    if (z == 0) call value(ctmp((index(ctmp, ':', back = .true.) + 1):len(ctmp)), seconds, z)
                end if
            end if
            if (z /= 0) then
                ierr = pstat%BAD_TIME_FORMAT
                return
            end if
        end do

        return

    end subroutine

end module
