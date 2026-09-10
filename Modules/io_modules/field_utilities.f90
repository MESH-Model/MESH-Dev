module field_utilities

    !> 'field_types': For field types, model variable types and I/O constants.
    use field_types

    implicit none

    interface reset_field
        module procedure reset_field_5d
        module procedure reset_field_4d
        module procedure reset_field_3d
        module procedure reset_field_2d
        module procedure reset_field_1d
        module procedure reset_field_scalar
    end interface

    interface allocate_field
        module procedure allocate_field_real5d
        module procedure allocate_field_real4d
        module procedure allocate_field_real3d
        module procedure allocate_field_real2d
        module procedure allocate_field_real1d
        module procedure allocate_field_int5d
        module procedure allocate_field_int4d
        module procedure allocate_field_int3d
        module procedure allocate_field_int2d
        module procedure allocate_field_int1d
        module procedure allocate_field_char1d
    end interface

    interface copy_field
        module procedure copy_field_5d_to_5d
        module procedure copy_field_4d_to_4d
        module procedure copy_field_3d_to_3d
        module procedure copy_field_2d_to_2d
        module procedure copy_field_1d_to_1d
        module procedure copy_field_scalar_to_scalar
    end interface

    interface map_dimensions
        module procedure map_dimensions_5d_to_real5d
        module procedure map_dimensions_5d_to_int5d
        module procedure map_dimensions_4d_to_real4d
        module procedure map_dimensions_4d_to_int4d
        module procedure map_dimensions_3d_to_real3d
        module procedure map_dimensions_3d_to_int3d
        module procedure map_dimensions_2d_to_real2d
        module procedure map_dimensions_2d_to_int2d
    end interface

    interface compact_field
        module procedure compact_5d_to_real4d
        module procedure compact_5d_to_int4d
        module procedure compact_5d_to_real3d
        module procedure compact_5d_to_int3d
        module procedure compact_5d_to_real2d
        module procedure compact_5d_to_int2d
        module procedure compact_5d_to_real1d
        module procedure compact_5d_to_int1d
        module procedure compact_4d_to_real3d
        module procedure compact_4d_to_int3d
        module procedure compact_4d_to_real2d
        module procedure compact_4d_to_int2d
        module procedure compact_4d_to_real1d
        module procedure compact_4d_to_int1d
        module procedure compact_3d_to_real2d
        module procedure compact_3d_to_int2d
        module procedure compact_3d_to_real1d
        module procedure compact_3d_to_int1d
        module procedure compact_2d_to_real1d
        module procedure compact_2d_to_int1d
    end interface

    interface explode_dimensions
        module procedure explode_dimensions_4d_to_5d
        module procedure explode_dimensions_3d_to_5d
        module procedure explode_dimensions_3d_to_4d
        module procedure explode_dimensions_2d_to_5d
        module procedure explode_dimensions_2d_to_4d
        module procedure explode_dimensions_2d_to_3d
        module procedure explode_dimensions_1d_to_5d
        module procedure explode_dimensions_1d_to_4d
        module procedure explode_dimensions_1d_to_3d
        module procedure explode_dimensions_1d_to_2d
    end interface

    interface check_field_dimensions
        module procedure check_field_dimensions_5d
        module procedure check_field_dimensions_4d
        module procedure check_field_dimensions_3d
        module procedure check_field_dimensions_2d
        module procedure check_field_dimensions_1d
    end interface

    interface map_field
        module procedure map_field_5d
        module procedure map_field_4d
        module procedure map_field_3d
        module procedure map_field_2d
        module procedure map_field_1d
    end interface

    interface assign_field
        module procedure assign_field_5d_to_real5d
        module procedure assign_field_5d_to_int5d
        module procedure assign_field_5d_to_real4d
        module procedure assign_field_5d_to_int4d
        module procedure assign_field_5d_to_real3d
        module procedure assign_field_5d_to_int3d
        module procedure assign_field_5d_to_real2d
        module procedure assign_field_5d_to_int2d
        module procedure assign_field_5d_to_real1d
        module procedure assign_field_5d_to_int1d
        module procedure assign_field_5d_to_scalar
        module procedure assign_field_4d_to_real4d
        module procedure assign_field_4d_to_int4d
        module procedure assign_field_4d_to_real3d
        module procedure assign_field_4d_to_int3d
        module procedure assign_field_4d_to_real2d
        module procedure assign_field_4d_to_int2d
        module procedure assign_field_4d_to_real1d
        module procedure assign_field_4d_to_int1d
        module procedure assign_field_4d_to_scalar
        module procedure assign_field_4d_to_real5d
        module procedure assign_field_4d_to_int5d
        module procedure assign_field_3d_to_real3d
        module procedure assign_field_3d_to_int3d
        module procedure assign_field_3d_to_real2d
        module procedure assign_field_3d_to_int2d
        module procedure assign_field_3d_to_real1d
        module procedure assign_field_3d_to_int1d
        module procedure assign_field_3d_to_scalar
        module procedure assign_field_3d_to_real5d
        module procedure assign_field_3d_to_int5d
        module procedure assign_field_3d_to_real4d
        module procedure assign_field_3d_to_int4d
        module procedure assign_field_2d_to_real2d
        module procedure assign_field_2d_to_int2d
        module procedure assign_field_2d_to_real1d
        module procedure assign_field_2d_to_int1d
        module procedure assign_field_2d_to_scalar
        module procedure assign_field_2d_to_real5d
        module procedure assign_field_2d_to_int5d
        module procedure assign_field_2d_to_real4d
        module procedure assign_field_2d_to_int4d
        module procedure assign_field_2d_to_real3d
        module procedure assign_field_2d_to_int3d
        module procedure assign_field_1d_to_real1d
        module procedure assign_field_1d_to_int1d
        module procedure assign_field_1d_to_char1d
        module procedure assign_field_1d_to_scalar
        module procedure assign_field_1d_to_real5d
        module procedure assign_field_1d_to_int5d
        module procedure assign_field_1d_to_real4d
        module procedure assign_field_1d_to_int4d
        module procedure assign_field_1d_to_real3d
        module procedure assign_field_1d_to_int3d
        module procedure assign_field_1d_to_real2d
        module procedure assign_field_1d_to_int2d
        module procedure assign_field_scalar_to_real5d
        module procedure assign_field_scalar_to_int5d
        module procedure assign_field_scalar_to_real4d
        module procedure assign_field_scalar_to_int4d
        module procedure assign_field_scalar_to_real3d
        module procedure assign_field_scalar_to_int3d
        module procedure assign_field_scalar_to_real2d
        module procedure assign_field_scalar_to_int2d
        module procedure assign_field_scalar_to_real1d
        module procedure assign_field_scalar_to_int1d
        module procedure assign_field_scalar_to_char1d
        module procedure assign_field_scalar_to_scalar

    end interface

    interface assign_mapped_value
        module procedure assign_mapped_value_5d_to_real1d
        module procedure assign_mapped_value_5d_to_int1d
        module procedure assign_mapped_value_4d_to_real1d
        module procedure assign_mapped_value_4d_to_int1d
        module procedure assign_mapped_value_3d_to_real1d
        module procedure assign_mapped_value_3d_to_int1d
        module procedure assign_mapped_value_2d_to_real1d
        module procedure assign_mapped_value_2d_to_int1d
        module procedure assign_mapped_value_1d_to_real1d
        module procedure assign_mapped_value_1d_to_int1d
        module procedure assign_mapped_value_1d_to_char1d
        module procedure assign_mapped_value_io_field_to_scalar
    end interface

    contains

    subroutine reset_field_5d(output_field, error_status)

        !> Input/output variables
        class(*) output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Reset the return status.
        error_status = 0

        !> Assign default value.
        select type (output_field)
            type is (real)
                output_field = NO_DATA_REAL
            type is (integer)
                output_field = NO_DATA_INT
            class default
                error_status = 1
        end select

    end subroutine

    subroutine reset_field_4d(output_field, error_status)

        !> Input/output variables
        class(*) output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Reset the return status.
        error_status = 0

        !> Assign default value.
        select type (output_field)
            type is (real)
                output_field = NO_DATA_REAL
            type is (integer)
                output_field = NO_DATA_INT
            class default
                error_status = 1
        end select

    end subroutine

    subroutine reset_field_3d(output_field, error_status)

        !> Input/output variables
        class(*) output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Reset the return status.
        error_status = 0

        !> Assign default value.
        select type (output_field)
            type is (real)
                output_field = NO_DATA_REAL
            type is (integer)
                output_field = NO_DATA_INT
            class default
                error_status = 1
        end select

    end subroutine

    subroutine reset_field_2d(output_field, error_status)

        !> Input/output variables
        class(*) output_field(:, :)
        integer, intent(out) :: error_status

        !> Reset the return status.
        error_status = 0

        !> Assign default value.
        select type (output_field)
            type is (real)
                output_field = NO_DATA_REAL
            type is (integer)
                output_field = NO_DATA_INT
            class default
                error_status = 1
        end select

    end subroutine

    subroutine reset_field_1d(output_field, error_status)

        !> Input/output variables
        class(*) output_field(:)
        integer, intent(out) :: error_status

        !> Reset the return status.
        error_status = 0

        !> Assign default value.
        select type (output_field)
            type is (real)
                output_field = NO_DATA_REAL
            type is (integer)
                output_field = NO_DATA_INT
            type is (character(len = *))
                output_field = NO_DATA_CHAR
            class default
                error_status = 1
        end select

    end subroutine

    subroutine reset_field_scalar(output_field, error_status)

        !> Input/output variables
        class(*) output_field
        integer, intent(out) :: error_status

        !> Reset the return status.
        error_status = 0

        !> Assign default value.
        select type (output_field)
            type is (real)
                output_field = NO_DATA_REAL
            type is (integer)
                output_field = NO_DATA_INT
            type is (character(len = *))
                output_field = NO_DATA_CHAR
            class default
                error_status = 1
        end select

    end subroutine

    subroutine allocate_field_real5d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        real, allocatable :: output_field(:, :, :, :, :)
        integer, intent(in) :: dim_sizes(5)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1), dim_sizes(2), dim_sizes(3), dim_sizes(4), dim_sizes(5)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_int5d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        integer, allocatable :: output_field(:, :, :, :, :)
        integer, intent(in) :: dim_sizes(5)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1), dim_sizes(2), dim_sizes(3), dim_sizes(4), dim_sizes(5)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_real4d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        real, allocatable :: output_field(:, :, :, :)
        integer, intent(in) :: dim_sizes(4)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1), dim_sizes(2), dim_sizes(3), dim_sizes(4)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_int4d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        integer, allocatable :: output_field(:, :, :, :)
        integer, intent(in) :: dim_sizes(4)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1), dim_sizes(2), dim_sizes(3), dim_sizes(4)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_real3d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        real, allocatable :: output_field(:, :, :)
        integer, intent(in) :: dim_sizes(3)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1), dim_sizes(2), dim_sizes(3)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_int3d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        integer, allocatable :: output_field(:, :, :)
        integer, intent(in) :: dim_sizes(3)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1), dim_sizes(2), dim_sizes(3)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_real2d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        real, allocatable :: output_field(:, :)
        integer, intent(in) :: dim_sizes(2)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1), dim_sizes(2)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_int2d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        integer, allocatable :: output_field(:, :)
        integer, intent(in) :: dim_sizes(2)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1), dim_sizes(2)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_real1d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        real, allocatable :: output_field(:)
        integer, intent(in) :: dim_sizes(1)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_int1d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        integer, allocatable :: output_field(:)
        integer, intent(in) :: dim_sizes(1)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine allocate_field_char1d(output_field, dim_sizes, error_status)

        !> Input/output variables.
        character(len = *), allocatable :: output_field(:)
        integer, intent(in) :: dim_sizes(1)
        integer, intent(out) :: error_status

        !> Allocate the field.
        allocate(output_field(dim_sizes(1)), stat = error_status)

        !> Initialize the field.
        if (error_status == 0) call reset_field(output_field, error_status)

    end subroutine

    subroutine copy_field_5d_to_5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        class(*) output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        real rhuge
        integer ihuge

        !> Compare the shapes of the input and output fields.
        if (any(shape(input_field) /= shape(output_field))) then
            error_status = 1
            return
        else

            !> Return status.
            error_status = 0
        end if

        !> Copy field.
        select type (input_field)
            type is (real)
                select type (output_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        ihuge = huge(output_field)
                        rhuge = real(ihuge)
                        where (input_field > rhuge)
                            output_field = ihuge
                        elsewhere (input_field < -rhuge)
                            output_field = -ihuge
                        elsewhere
                            output_field = int(input_field)
                        end where
                    class default
                        error_status = 1
                end select
            type is (integer)
                select type (output_field)
                    type is (real)
                        output_field = real(input_field)
                    type is (integer)
                        output_field = input_field
                    class default
                        error_status = 1
                end select
            class default
                error_status = 1
        end select

    end subroutine

    subroutine copy_field_4d_to_4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        class(*) output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        real rhuge
        integer ihuge

        !> Compare the shapes of the input and output fields.
        if (any(shape(input_field) /= shape(output_field))) then
            error_status = 1
            return
        else

            !> Return status.
            error_status = 0
        end if

        !> Copy field.
        select type (input_field)
            type is (real)
                select type (output_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        ihuge = huge(output_field)
                        rhuge = real(ihuge)
                        where (input_field > rhuge)
                            output_field = ihuge
                        elsewhere (input_field < -rhuge)
                            output_field = -ihuge
                        elsewhere
                            output_field = int(input_field)
                        end where
                    class default
                        error_status = 1
                end select
            type is (integer)
                select type (output_field)
                    type is (real)
                        output_field = real(input_field)
                    type is (integer)
                        output_field = input_field
                    class default
                        error_status = 1
                end select
            class default
                error_status = 1
        end select

    end subroutine

    subroutine copy_field_3d_to_3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        class(*) output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        real rhuge
        integer ihuge

        !> Compare the shapes of the input and output fields.
        if (any(shape(input_field) /= shape(output_field))) then
            error_status = 1
            return
        else

            !> Return status.
            error_status = 0
        end if

        !> Copy field.
        select type (input_field)
            type is (real)
                select type (output_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        ihuge = huge(output_field)
                        rhuge = real(ihuge)
                        where (input_field > rhuge)
                            output_field = ihuge
                        elsewhere (input_field < -rhuge)
                            output_field = -ihuge
                        elsewhere
                            output_field = int(input_field)
                        end where
                    class default
                        error_status = 1
                end select
            type is (integer)
                select type (output_field)
                    type is (real)
                        output_field = real(input_field)
                    type is (integer)
                        output_field = input_field
                    class default
                        error_status = 1
                end select
            class default
                error_status = 1
        end select

    end subroutine

    subroutine copy_field_2d_to_2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        class(*) output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        real rhuge
        integer ihuge

        !> Compare the shapes of the input and output fields.
        if (any(shape(input_field) /= shape(output_field))) then
            error_status = 1
            return
        else

            !> Return status.
            error_status = 0
        end if

        !> Copy field.
        select type (input_field)
            type is (real)
                select type (output_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        ihuge = huge(output_field)
                        rhuge = real(ihuge)
                        where (input_field > rhuge)
                            output_field = ihuge
                        elsewhere (input_field < -rhuge)
                            output_field = -ihuge
                        elsewhere
                            output_field = int(input_field)
                        end where
                    class default
                        error_status = 1
                end select
            type is (integer)
                select type (output_field)
                    type is (real)
                        output_field = real(input_field)
                    type is (integer)
                        output_field = input_field
                    class default
                        error_status = 1
                end select
            class default
                error_status = 1
        end select

    end subroutine

    subroutine copy_field_1d_to_1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        class(*) output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        real rhuge
        integer ihuge, i, ierr

        !> Compare the shapes of the input and output fields.
        if (any(shape(input_field) /= shape(output_field))) then
            error_status = 1
            return
        else

            !> Return status.
            error_status = 0
        end if

        !> Copy field.
        select type (input_field)
            type is (real)
                select type (output_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        ihuge = huge(output_field)
                        rhuge = real(ihuge)
                        where (input_field > rhuge)
                            output_field = ihuge
                        elsewhere (input_field < -rhuge)
                            output_field = -ihuge
                        elsewhere
                            output_field = int(input_field)
                        end where
                    type is (character(len = *))
                        do i = 1, size(output_field)
                            write(output_field(i), *, iostat = ierr) input_field(i)
                            if (ierr /= 0) then
                                error_status = 1
                            else
                                output_field(i) = trim(adjustl(output_field(i)))
                            end if
                        end do
                    class default
                        error_status = 1
                end select
            type is (integer)
                select type (output_field)
                    type is (real)
                        output_field = real(input_field)
                    type is (integer)
                        output_field = input_field
                    type is (character(len = *))
                        do i = 1, size(output_field)
                            write(output_field(i), *, iostat = ierr) input_field(i)
                            if (ierr /= 0) then
                                error_status = 1
                            else
                                output_field(i) = trim(adjustl(output_field(i)))
                            end if
                        end do
                    class default
                        error_status = 1
                end select
            type is (character(len = *))
                select type (output_field)
                    type is (real)
                        do i = 1, size(output_field)
                            read(input_field(i), *, iostat = ierr) output_field(i)
                            if (ierr /= 0) error_status = 1
                        end do
                    type is (integer)
                        do i = 1, size(output_field)
                            read(input_field(i), *, iostat = ierr) output_field(i)
                            if (ierr /= 0) error_status = 1
                        end do
                    type is (character(len = *))
                        do i = 1, size(input_field)
                            output_field(i) = trim(adjustl(input_field(i)))
                        end do
                    class default
                        error_status = 1
                end select
            class default
                error_status = 1
        end select

    end subroutine

    subroutine copy_field_scalar_to_scalar(input_field, output_field, error_status)

        !> Input/output variables.
        class(*) input_field
        class(*) output_field
        integer, intent(out) :: error_status

        !> Local variables.
        real rhuge
        integer ihuge, ierr

        !> Return status.
        error_status = 0

        !> Assign field.
        select type (input_field)
            type is (real)
                select type (output_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        ihuge = huge(output_field)
                        rhuge = real(ihuge)
                        if (input_field > rhuge) then
                            output_field = ihuge
                        else if (input_field < -rhuge) then
                            output_field = -ihuge
                        else
                            output_field = int(input_field)
                        end if
                    type is (character(len = *))
                        write(output_field, *, iostat = ierr) input_field
                        if (ierr /= 0) then
                            error_status = 1
                        else
                            output_field = trim(adjustl(output_field))
                        end if
                    class default
                        error_status = 1
                end select
            type is (integer)
                select type (output_field)
                    type is (real)
                        output_field = real(input_field)
                    type is (integer)
                        output_field = input_field
                    type is (character(len = *))
                        write(output_field, *, iostat = ierr) input_field
                        if (ierr /= 0) then
                            error_status = 1
                        else
                            output_field = trim(adjustl(output_field))
                        end if
                    class default
                        error_status = 1
                end select
            type is (character(len = *))
                select type (output_field)
                    type is (real)
                        read(input_field, *, iostat = ierr) output_field
                        if (ierr /= 0) error_status = 1
                    type is (integer)
                        read(input_field, *, iostat = ierr) output_field
                        if (ierr /= 0) error_status = 1
                    type is (character(len = *))
                        output_field = trim(adjustl(input_field))
                    class default
                        error_status = 1
                end select
            class default
                error_status = 1
        end select

    end subroutine

    subroutine map_dimensions_5d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, intent(in) :: desired_order(5)
        class(*) output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables
        integer order(5), d5, d4, d3, d2, d1, ierr

        !> Status.
        error_status = 0

        !> Update temporary variables.
        order = desired_order

        !> Map the field.
        if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d2, d3, d4, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d1, d3, d4, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d3, d2, d4, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d3, d1, d4, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d1, d2, d4, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d2, d1, d4, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d2, d4, d3, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d1, d4, d3, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d3, d4, d2, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d3, d4, d1, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d1, d4, d2, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d2, d4, d1, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d4, d3, d2, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d4, d3, d1, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d4, d2, d3, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d4, d1, d3, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d4, d2, d1, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d4, d1, d2, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d2, d3, d1, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d1, d3, d2, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d3, d2, d1, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d3, d1, d2, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d1, d2, d3, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 5) then
            do d5 = 1, size(input_field, order(5))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d2, d1, d3, d5), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d2, d3, d5, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d1, d3, d5, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d3, d2, d5, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d3, d1, d5, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d1, d2, d5, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d2, d1, d5, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d2, d4, d5, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d1, d4, d5, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d3, d4, d5, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d3, d4, d5, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d1, d4, d5, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d2, d4, d5, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d4, d3, d5, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d4, d3, d5, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d4, d2, d5, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d4, d1, d5, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d4, d2, d5, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d4, d1, d5, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d2, d3, d5, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d1, d3, d5, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 4) then
            do d1 = 1, size(input_field, order(1))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d3, d2, d5, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 4) then
            do d2 = 1, size(input_field, order(2))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d3, d1, d5, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d1, d2, d5, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 4) then
            do d3 = 1, size(input_field, order(3))
                do d5 = 1, size(input_field, order(5))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d2, d1, d5, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d2, d5, d4, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d1, d5, d4, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d3, d5, d4, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d3, d5, d4, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d1, d5, d4, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d2, d5, d4, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d2, d5, d3, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d1, d5, d3, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d3, d5, d2, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d3, d5, d1, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d1, d5, d2, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 3) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d2, d5, d1, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d4, d5, d2, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d4, d5, d1, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d4, d5, d3, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d4, d5, d3, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d4, d5, d1, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 2 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d4 = 1, size(input_field, order(4))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d4, d5, d2, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d2, d5, d1, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d1, d5, d2, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d3, d5, d1, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 1 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d5 = 1, size(input_field, order(5))
                        do d3 = 1, size(input_field, order(3))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d3, d5, d2, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d1 = 1, size(input_field, order(1))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d1, d5, d3, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d5 = 1, size(input_field, order(5))
                        do d2 = 1, size(input_field, order(2))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d2, d5, d3, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d5, d3, d4, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d5, d3, d4, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d5, d2, d4, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d5, d1, d4, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d5, d2, d4, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 4 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d5, d1, d4, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d5, d4, d3, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d5, d4, d3, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d5, d4, d2, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d5, d4, d1, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d5, d4, d2, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 1 .and. order(4) == 3 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d5, d4, d1, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d5, d3, d2, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d5, d3, d1, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d1 = 1, size(input_field, order(1))
                                call copy_field(input_field(d1, d5, d2, d3, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d2 = 1, size(input_field, order(2))
                                call copy_field(input_field(d2, d5, d1, d3, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d5, d2, d1, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 5 .and. order(5) == 2) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d3 = 1, size(input_field, order(3))
                                call copy_field(input_field(d3, d5, d1, d2, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d5, d3, d1, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 1 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d5, d3, d2, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d5, d2, d1, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 1 .and. order(5) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d5, d1, d2, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d5, d2, d3, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 1 .and. order(5) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d5 = 1, size(input_field, order(5))
                            do d4 = 1, size(input_field, order(4))
                                call copy_field(input_field(d4, d5, d1, d3, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d2, d3, d4, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 4 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d1, d3, d4, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d3, d2, d4, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 4 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d3, d1, d4, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d1, d2, d4, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 4 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d2, d1, d4, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d2, d4, d3, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 3 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d1, d4, d3, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d3, d4, d2, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 2 .and. order(4) == 3 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d3, d4, d1, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d1, d4, d2, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 5 .and. order(4) == 3 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d2, d4, d1, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d4, d3, d2, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 5 .and. order(3) == 3 .and. order(4) == 2 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d4, d3, d1, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 5 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d4, d2, d3, d1), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 5 .and. order(3) == 4 .and. order(4) == 2 .and. order(5) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d4, d1, d3, d2), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d4, d2, d1, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 5 .and. order(4) == 2 .and. order(5) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d4, d1, d2, d3), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d2, d3, d1, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d1, d3, d2, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d3, d2, d1, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d3, d1, d2, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d1, d2, d3, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 5 .and. order(5) == 1) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            do d5 = 1, size(input_field, order(5))
                                call copy_field(input_field(d5, d2, d1, d3, d4), output_field(d1, d2, d3, d4, d5), ierr)
                                if (ierr /= 0) error_status = ierr
                            end do
                        end do
                    end do
                end do
            end do
        end if

    end subroutine

    subroutine map_dimensions_5d_to_real5d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, intent(in) :: desired_order(size(shape(input_field)))
        real, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(size(desired_order)), i

        !> Allocate field.
        if (.not. allocated(output_field)) then
            do i = 1, size(desired_order)
                dim_size(i) = size(input_field, desired_order(i))
            end do
            call allocate_field(output_field, dim_size, error_status)
        end if

        !> Map field.
        call map_dimensions_5d(input_field, desired_order, output_field, error_status)

    end subroutine

    subroutine map_dimensions_5d_to_int5d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, intent(in) :: desired_order(size(shape(input_field)))
        integer, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(size(desired_order)), i

        !> Allocate field.
        if (.not. allocated(output_field)) then
            do i = 1, size(desired_order)
                dim_size(i) = size(input_field, desired_order(i))
            end do
            call allocate_field(output_field, dim_size, error_status)
        end if

        !> Map field.
        call map_dimensions_5d(input_field, desired_order, output_field, error_status)

    end subroutine

    subroutine map_dimensions_4d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, intent(in) :: desired_order(4)
        class(*) output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables
        integer order(4), d4, d3, d2, d1, ierr

        !> Status.
        error_status = 0

        !> Update temporary variables.
        order = desired_order

        !> Map the field.
        if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            call copy_field(input_field(d1, d2, d3, d4), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            call copy_field(input_field(d2, d1, d3, d4), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            call copy_field(input_field(d1, d3, d2, d4), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            call copy_field(input_field(d2, d3, d1, d4), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            call copy_field(input_field(d3, d1, d2, d4), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 4) then
            do d4 = 1, size(input_field, order(4))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            call copy_field(input_field(d3, d2, d1, d4), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d1 = 1, size(input_field, order(1))
                            call copy_field(input_field(d1, d2, d4, d3), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d2 = 1, size(input_field, order(2))
                            call copy_field(input_field(d2, d1, d4, d3), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d1 = 1, size(input_field, order(1))
                            call copy_field(input_field(d1, d3, d4, d2), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 2 .and. order(4) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d3 = 1, size(input_field, order(3))
                        do d2 = 1, size(input_field, order(2))
                            call copy_field(input_field(d2, d3, d4, d1), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 3) then
            do d2 = 1, size(input_field, order(2))
                do d4 = 1, size(input_field, order(4))
                    do d1 = 1, size(input_field, order(1))
                        do d3 = 1, size(input_field, order(3))
                            call copy_field(input_field(d3, d1, d4, d2), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 1 .and. order(4) == 3) then
            do d1 = 1, size(input_field, order(1))
                do d4 = 1, size(input_field, order(4))
                    do d2 = 1, size(input_field, order(2))
                        do d3 = 1, size(input_field, order(3))
                            call copy_field(input_field(d3, d2, d4, d1), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            call copy_field(input_field(d1, d4, d3, d2), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 1 .and. order(3) == 3 .and. order(4) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            call copy_field(input_field(d2, d4, d3, d1), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d1 = 1, size(input_field, order(1))
                            call copy_field(input_field(d1, d4, d2, d3), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 4 .and. order(4) == 2) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d2 = 1, size(input_field, order(2))
                            call copy_field(input_field(d2, d4, d1, d3), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 1 .and. order(4) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            call copy_field(input_field(d3, d4, d2, d1), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 1 .and. order(4) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d4 = 1, size(input_field, order(4))
                        do d3 = 1, size(input_field, order(3))
                            call copy_field(input_field(d3, d4, d1, d2), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 2 .and. order(3) == 3 .and. order(4) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            call copy_field(input_field(d4, d2, d3, d1), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 4 .and. order(3) == 3 .and. order(4) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            call copy_field(input_field(d4, d1, d3, d2), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 4 .and. order(2) == 3 .and. order(3) == 2 .and. order(4) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            call copy_field(input_field(d4, d3, d2, d1), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 4 .and. order(3) == 2 .and. order(4) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        do d4 = 1, size(input_field, order(4))
                            call copy_field(input_field(d4, d3, d1, d2), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 4 .and. order(4) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        do d4 = 1, size(input_field, order(4))
                            call copy_field(input_field(d4, d1, d2, d3), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 4 .and. order(4) == 1) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        do d4 = 1, size(input_field, order(4))
                            call copy_field(input_field(d4, d2, d1, d3), output_field(d1, d2, d3, d4), ierr)
                            if (ierr /= 0) error_status = ierr
                        end do
                    end do
                end do
            end do
        end if

    end subroutine

    subroutine map_dimensions_4d_to_real4d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, intent(in) :: desired_order(size(shape(input_field)))
        real, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(size(desired_order)), i

        !> Allocate field.
        if (.not. allocated(output_field)) then
            do i = 1, size(desired_order)
                dim_size(i) = size(input_field, desired_order(i))
            end do
            call allocate_field(output_field, dim_size, error_status)
        end if

        !> Map field.
        call map_dimensions_4d(input_field, desired_order, output_field, error_status)

    end subroutine

    subroutine map_dimensions_4d_to_int4d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, intent(in) :: desired_order(size(shape(input_field)))
        integer, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(size(desired_order)), i

        !> Allocate field.
        if (.not. allocated(output_field)) then
            do i = 1, size(desired_order)
                dim_size(i) = size(input_field, desired_order(i))
            end do
            call allocate_field(output_field, dim_size, error_status)
        end if

        !> Map field.
        call map_dimensions_4d(input_field, desired_order, output_field, error_status)

    end subroutine

    subroutine map_dimensions_3d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, intent(in) :: desired_order(3)
        class(*) output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables
        integer order(3), d3, d2, d1, ierr

        !> Status.
        error_status = 0

        !> Update temporary variables.
        order = desired_order

        !> Map the field.
        if (order(1) == 1 .and. order(2) == 2 .and. order(3) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d2 = 1, size(input_field, order(2))
                    do d1 = 1, size(input_field, order(1))
                        call copy_field(input_field(d1, d2, d3), output_field(d1, d2, d3), ierr)
                        if (ierr /= 0) error_status = ierr
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 1 .and. order(3) == 3) then
            do d3 = 1, size(input_field, order(3))
                do d1 = 1, size(input_field, order(1))
                    do d2 = 1, size(input_field, order(2))
                        call copy_field(input_field(d2, d1, d3), output_field(d1, d2, d3), ierr)
                        if (ierr /= 0) error_status = ierr
                    end do
                end do
            end do
        else if (order(1) == 1 .and. order(2) == 3 .and. order(3) == 2) then
            do d2 = 1, size(input_field, order(2))
                do d3 = 1, size(input_field, order(3))
                    do d1 = 1, size(input_field, order(1))
                        call copy_field(input_field(d1, d3, d2), output_field(d1, d2, d3), ierr)
                        if (ierr /= 0) error_status = ierr
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 1 .and. order(3) == 2) then
            do d1 = 1, size(input_field, order(1))
                do d3 = 1, size(input_field, order(3))
                    do d2 = 1, size(input_field, order(2))
                        call copy_field(input_field(d2, d3, d1), output_field(d1, d2, d3), ierr)
                        if (ierr /= 0) error_status = ierr
                    end do
                end do
            end do
        else if (order(1) == 2 .and. order(2) == 3 .and. order(3) == 1) then
            do d2 = 1, size(input_field, order(2))
                do d1 = 1, size(input_field, order(1))
                    do d3 = 1, size(input_field, order(3))
                        call copy_field(input_field(d3, d1, d2), output_field(d1, d2, d3), ierr)
                        if (ierr /= 0) error_status = ierr
                    end do
                end do
            end do
        else if (order(1) == 3 .and. order(2) == 2 .and. order(3) == 1) then
            do d1 = 1, size(input_field, order(1))
                do d2 = 1, size(input_field, order(2))
                    do d3 = 1, size(input_field, order(3))
                        call copy_field(input_field(d3, d2, d1), output_field(d1, d2, d3), ierr)
                        if (ierr /= 0) error_status = ierr
                    end do
                end do
            end do
        end if

    end subroutine

    subroutine map_dimensions_3d_to_real3d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, intent(in) :: desired_order(size(shape(input_field)))
        real, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(size(desired_order)), i

        !> Allocate field.
        if (.not. allocated(output_field)) then
            do i = 1, size(desired_order)
                dim_size(i) = size(input_field, desired_order(i))
            end do
            call allocate_field(output_field, dim_size, error_status)
        end if

        !> Map field.
        call map_dimensions_3d(input_field, desired_order, output_field, error_status)

    end subroutine

    subroutine map_dimensions_3d_to_int3d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, intent(in) :: desired_order(size(shape(input_field)))
        integer, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(size(desired_order)), i

        !> Allocate field.
        if (.not. allocated(output_field)) then
            do i = 1, size(desired_order)
                dim_size(i) = size(input_field, desired_order(i))
            end do
            call allocate_field(output_field, dim_size, error_status)
        end if

        !> Map field.
        call map_dimensions_3d(input_field, desired_order, output_field, error_status)

    end subroutine

    subroutine map_dimensions_2d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, intent(in) :: desired_order(2)
        class(*) output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer d2, d1, ierr

        !> Status.
        error_status = 0

        !> Map the field.
        if (desired_order(1) == 1 .and. desired_order(2) == 2) then
            do d2 = 1, size(input_field, desired_order(2))
                do d1 = 1, size(input_field, desired_order(1))
                    call copy_field(input_field(d1, d2), output_field(d1, d2), ierr)
                    if (ierr /= 0) error_status = ierr
                end do
            end do
        else
            do d1 = 1, size(input_field, desired_order(1))
                do d2 = 1, size(input_field, desired_order(2))
                    call copy_field(input_field(d2, d1), output_field(d1, d2), ierr)
                    if (ierr /= 0) error_status = ierr
                end do
            end do
        end if

    end subroutine

    subroutine map_dimensions_2d_to_real2d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, intent(in) :: desired_order(size(shape(input_field)))
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(size(desired_order)), i

        !> Allocate field.
        if (.not. allocated(output_field)) then
            do i = 1, size(desired_order)
                dim_size(i) = size(input_field, desired_order(i))
            end do
            call allocate_field(output_field, dim_size, error_status)
        end if

        !> Map field.
        call map_dimensions_2d(input_field, desired_order, output_field, error_status)

    end subroutine

    subroutine map_dimensions_2d_to_int2d(input_field, desired_order, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, intent(in) :: desired_order(size(shape(input_field)))
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer dim_size(size(desired_order)), i

        !> Allocate field.
        if (.not. allocated(output_field)) then
            do i = 1, size(desired_order)
                dim_size(i) = size(input_field, desired_order(i))
            end do
            call allocate_field(output_field, dim_size, error_status)
        end if

        !> Map field.
        call map_dimensions_2d(input_field, desired_order, output_field, error_status)

    end subroutine

    subroutine map_flattened_dimensions(field_shape, target_order, flattened_dims, targeted_dims, target_size, error_status)

        !> Input/output variables.
        integer, intent(in) :: field_shape(:)
        integer, intent(in) :: target_order
        integer, intent(out) :: flattened_dims(max(size(field_shape) - target_order, 1))
        integer, intent(out), optional :: targeted_dims(max(target_order, 1))
        integer, intent(out), optional :: target_size(max(target_order, 1))
        integer, intent(out) :: error_status

        !> Local variables.
        integer max_flattened_dims, k, j, i

        !> Return status.
        error_status = 0

        !> Transfer local variables.
        max_flattened_dims = size(field_shape) - target_order

        !> Check if the field can be compacted (any dimensions in excess of 'target_order' must have a size of one).
        flattened_dims = 0
        if (present(targeted_dims)) targeted_dims = 0
        if (present(target_size)) target_size = 0
        k = 1
        j = 1
        do i = 1, size(field_shape)
            if (j <= max_flattened_dims .and. field_shape(i) == 1) then
                flattened_dims(j) = i
                j = j + 1
            else
                if (present(targeted_dims)) then
                    if (k > size(targeted_dims)) then
                        flattened_dims = 0
                        exit
                    else
                        targeted_dims(k) = i
                    end if
                end if
                if (present(target_size)) then
                    if (k > size(targeted_dims)) then
                        flattened_dims = 0
                    else
                        target_size(k) = field_shape(i)
                        exit
                    end if
                end if
                k = k + 1
            end if
        end do

        !> Check if the field can be compacted.
        if (all(flattened_dims == 0)) then
            flattened_dims = 0
            if (present(targeted_dims)) targeted_dims = 0
            if (present(target_size)) target_size = 0
            error_status = 1
        end if

    end subroutine

    subroutine compact_dimensions(field_shape, target_order, target_size, bit_map, error_status)

        !> Input/output variables.
        integer, intent(in) :: field_shape(:)
        integer, intent(in) :: target_order
        integer, intent(out) :: target_size(max(target_order, 1))
        integer, intent(out) :: bit_map
        integer, intent(out) :: error_status

        !> Local variables.
        integer compacted_dims(max(size(field_shape) - target_order, 1)), targeted_dims(max(target_order, 1)), i

        !> Find flattened dimensions.
        call map_flattened_dimensions(field_shape, target_order, compacted_dims, targeted_dims, target_size, error_status)
        if (error_status /= 0) then
            return
        else

            !> Calculate map.
            bit_map = 0
            do i = 1, size(targeted_dims)
                bit_map = bit_map + 2**targeted_dims(i)
            end do
        end if

    end subroutine

    subroutine compact_5d_to_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 4, target_size(4), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2 + 2**3 + 2**4)
                call copy_field(input_field(:, :, :, :, 1), output_field, error_status)
            case (2**1 + 2**2 + 2**3 + 2**5)
                call copy_field(input_field(:, :, :, 1, :), output_field, error_status)
            case (2**1 + 2**2 + 2**4 + 2**5)
                call copy_field(input_field(:, :, 1, :, :), output_field, error_status)
            case (2**1 + 2**3 + 2**4 + 2**5)
                call copy_field(input_field(:, 1, :, :, :), output_field, error_status)
            case (2**2 + 2**3 + 2**4 + 2**5)
                call copy_field(input_field(1, :, :, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_5d_to_int4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 4, target_size(4), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2 + 2**3 + 2**4)
                call copy_field(input_field(:, :, :, :, 1), output_field, error_status)
            case (2**1 + 2**2 + 2**3 + 2**5)
                call copy_field(input_field(:, :, :, 1, :), output_field, error_status)
            case (2**1 + 2**2 + 2**4 + 2**5)
                call copy_field(input_field(:, :, 1, :, :), output_field, error_status)
            case (2**1 + 2**3 + 2**4 + 2**5)
                call copy_field(input_field(:, 1, :, :, :), output_field, error_status)
            case (2**2 + 2**3 + 2**4 + 2**5)
                call copy_field(input_field(1, :, :, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_5d_to_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 3, target_size(3), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2 + 2**3)
                call copy_field(input_field(:, :, :, 1, 1), output_field, error_status)
            case (2**1 + 2**2 + 2**4)
                call copy_field(input_field(:, :, 1, :, 1), output_field, error_status)
            case (2**1 + 2**2 + 2**5)
                call copy_field(input_field(:, :, 1, 1, :), output_field, error_status)
            case (2**1 + 2**3 + 2**4)
                call copy_field(input_field(:, 1, :, :, 1), output_field, error_status)
            case (2**1 + 2**3 + 2**5)
                call copy_field(input_field(:, 1, :, 1, :), output_field, error_status)
            case (2**1 + 2**4 + 2**5)
                call copy_field(input_field(:, 1, 1, :, :), output_field, error_status)
            case (2**2 + 2**3 + 2**4)
                call copy_field(input_field(1, :, :, :, 1), output_field, error_status)
            case (2**2 + 2**3 + 2**5)
                call copy_field(input_field(1, :, :, 1, :), output_field, error_status)
            case (2**2 + 2**4 + 2**5)
                call copy_field(input_field(1, :, 1, :, :), output_field, error_status)
            case (2**3 + 2**4 + 2**5)
                call copy_field(input_field(1, 1, :, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_5d_to_int3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 3, target_size(3), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2 + 2**3)
                call copy_field(input_field(:, :, :, 1, 1), output_field, error_status)
            case (2**1 + 2**2 + 2**4)
                call copy_field(input_field(:, :, 1, :, 1), output_field, error_status)
            case (2**1 + 2**2 + 2**5)
                call copy_field(input_field(:, :, 1, 1, :), output_field, error_status)
            case (2**1 + 2**3 + 2**4)
                call copy_field(input_field(:, 1, :, :, 1), output_field, error_status)
            case (2**1 + 2**3 + 2**5)
                call copy_field(input_field(:, 1, :, 1, :), output_field, error_status)
            case (2**1 + 2**4 + 2**5)
                call copy_field(input_field(:, 1, 1, :, :), output_field, error_status)
            case (2**2 + 2**3 + 2**4)
                call copy_field(input_field(1, :, :, :, 1), output_field, error_status)
            case (2**2 + 2**3 + 2**5)
                call copy_field(input_field(1, :, :, 1, :), output_field, error_status)
            case (2**2 + 2**4 + 2**5)
                call copy_field(input_field(1, :, 1, :, :), output_field, error_status)
            case (2**3 + 2**4 + 2**5)
                call copy_field(input_field(1, 1, :, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_5d_to_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 2, target_size(2), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2)
                call copy_field(input_field(:, :, 1, 1, 1), output_field, error_status)
            case (2**1 + 2**3)
                call copy_field(input_field(:, 1, :, 1, 1), output_field, error_status)
            case (2**1 + 2**4)
                call copy_field(input_field(:, 1, 1, :, 1), output_field, error_status)
            case (2**1 + 2**5)
                call copy_field(input_field(:, 1, 1, 1, :), output_field, error_status)
            case (2**2 + 2**3)
                call copy_field(input_field(1, :, :, 1, 1), output_field, error_status)
            case (2**2 + 2**4)
                call copy_field(input_field(1, :, 1, :, 1), output_field, error_status)
            case (2**2 + 2**5)
                call copy_field(input_field(1, :, 1, 1, :), output_field, error_status)
            case (2**3 + 2**4)
                call copy_field(input_field(1, 1, :, :, 1), output_field, error_status)
            case (2**3 + 2**5)
                call copy_field(input_field(1, 1, :, 1, :), output_field, error_status)
            case (2**4 + 2**5)
                call copy_field(input_field(1, 1, 1, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_5d_to_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 2, target_size(2), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2)
                call copy_field(input_field(:, :, 1, 1, 1), output_field, error_status)
            case (2**1 + 2**3)
                call copy_field(input_field(:, 1, :, 1, 1), output_field, error_status)
            case (2**1 + 2**4)
                call copy_field(input_field(:, 1, 1, :, 1), output_field, error_status)
            case (2**1 + 2**5)
                call copy_field(input_field(:, 1, 1, 1, :), output_field, error_status)
            case (2**2 + 2**3)
                call copy_field(input_field(1, :, :, 1, 1), output_field, error_status)
            case (2**2 + 2**4)
                call copy_field(input_field(1, :, 1, :, 1), output_field, error_status)
            case (2**2 + 2**5)
                call copy_field(input_field(1, :, 1, 1, :), output_field, error_status)
            case (2**3 + 2**4)
                call copy_field(input_field(1, 1, :, :, 1), output_field, error_status)
            case (2**3 + 2**5)
                call copy_field(input_field(1, 1, :, 1, :), output_field, error_status)
            case (2**4 + 2**5)
                call copy_field(input_field(1, 1, 1, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_5d_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 1, target_size(1), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1)
                call copy_field(input_field(:, 1, 1, 1, 1), output_field, error_status)
            case (2**2)
                call copy_field(input_field(1, :, 1, 1, 1), output_field, error_status)
            case (2**3)
                call copy_field(input_field(1, 1, :, 1, 1), output_field, error_status)
            case (2**4)
                call copy_field(input_field(1, 1, 1, :, 1), output_field, error_status)
            case (2**5)
                call copy_field(input_field(1, 1, 1, 1, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_5d_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 1, target_size(1), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1)
                call copy_field(input_field(:, 1, 1, 1, 1), output_field, error_status)
            case (2**2)
                call copy_field(input_field(1, :, 1, 1, 1), output_field, error_status)
            case (2**3)
                call copy_field(input_field(1, 1, :, 1, 1), output_field, error_status)
            case (2**4)
                call copy_field(input_field(1, 1, 1, :, 1), output_field, error_status)
            case (2**5)
                call copy_field(input_field(1, 1, 1, 1, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_4d_to_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        real, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 3, target_size(3), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2 + 2**3)
                call copy_field(input_field(:, :, :, 1), output_field, error_status)
            case (2**1 + 2**2 + 2**4)
                call copy_field(input_field(:, :, 1, :), output_field, error_status)
            case (2**1 + 2**3 + 2**4)
                call copy_field(input_field(:, 1, :, :), output_field, error_status)
            case (2**2 + 2**3 + 2**4)
                call copy_field(input_field(1, :, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_4d_to_int3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 3, target_size(3), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2 + 2**3)
                call copy_field(input_field(:, :, :, 1), output_field, error_status)
            case (2**1 + 2**2 + 2**4)
                call copy_field(input_field(:, :, 1, :), output_field, error_status)
            case (2**1 + 2**3 + 2**4)
                call copy_field(input_field(:, 1, :, :), output_field, error_status)
            case (2**2 + 2**3 + 2**4)
                call copy_field(input_field(1, :, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_4d_to_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 2, target_size(2), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2)
                call copy_field(input_field(:, :, 1, 1), output_field, error_status)
            case (2**1 + 2**3)
                call copy_field(input_field(:, 1, :, 1), output_field, error_status)
            case (2**1 + 2**4)
                call copy_field(input_field(:, 1, 1, :), output_field, error_status)
            case (2**2 + 2**3)
                call copy_field(input_field(1, :, :, 1), output_field, error_status)
            case (2**2 + 2**4)
                call copy_field(input_field(1, :, 1, :), output_field, error_status)
            case (2**3 + 2**4)
                call copy_field(input_field(1, 1, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_4d_to_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 2, target_size(2), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2)
                call copy_field(input_field(:, :, 1, 1), output_field, error_status)
            case (2**1 + 2**3)
                call copy_field(input_field(:, 1, :, 1), output_field, error_status)
            case (2**1 + 2**4)
                call copy_field(input_field(:, 1, 1, :), output_field, error_status)
            case (2**2 + 2**3)
                call copy_field(input_field(1, :, :, 1), output_field, error_status)
            case (2**2 + 2**4)
                call copy_field(input_field(1, :, 1, :), output_field, error_status)
            case (2**3 + 2**4)
                call copy_field(input_field(1, 1, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_4d_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 1, target_size(1), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1)
                call copy_field(input_field(:, 1, 1, 1), output_field, error_status)
            case (2**2)
                call copy_field(input_field(1, :, 1, 1), output_field, error_status)
            case (2**3)
                call copy_field(input_field(1, 1, :, 1), output_field, error_status)
            case (2**4)
                call copy_field(input_field(1, 1, 1, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_4d_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 1, target_size(1), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1)
                call copy_field(input_field(:, 1, 1, 1), output_field, error_status)
            case (2**2)
                call copy_field(input_field(1, :, 1, 1), output_field, error_status)
            case (2**3)
                call copy_field(input_field(1, 1, :, 1), output_field, error_status)
            case (2**4)
                call copy_field(input_field(1, 1, 1, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_3d_to_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 2, target_size(2), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2)
                call copy_field(input_field(:, :, 1), output_field, error_status)
            case (2**1 + 2**3)
                call copy_field(input_field(:, 1, :), output_field, error_status)
            case (2**2 + 2**3)
                call copy_field(input_field(1, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_3d_to_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 2, target_size(2), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1 + 2**2)
                call copy_field(input_field(:, :, 1), output_field, error_status)
            case (2**1 + 2**3)
                call copy_field(input_field(:, 1, :), output_field, error_status)
            case (2**2 + 2**3)
                call copy_field(input_field(1, :, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_3d_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 1, target_size(1), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1)
                call copy_field(input_field(:, 1, 1), output_field, error_status)
            case (2**2)
                call copy_field(input_field(1, :, 1), output_field, error_status)
            case (2**3)
                call copy_field(input_field(1, 1, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_3d_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 1, target_size(1), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1)
                call copy_field(input_field(:, 1, 1), output_field, error_status)
            case (2**2)
                call copy_field(input_field(1, :, 1), output_field, error_status)
            case (2**3)
                call copy_field(input_field(1, 1, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_2d_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 1, target_size(1), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1)
                call copy_field(input_field(:, 1), output_field, error_status)
            case (2**2)
                call copy_field(input_field(1, :), output_field, error_status)
        end select

    end subroutine

    subroutine compact_2d_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer :: target_order = 1, target_size(1), bit_map

        !> Check if the field can be compacted.
        call compact_dimensions(shape(input_field), target_order, target_size, bit_map, error_status)
        if (error_status /= 0) return

        !> Allocate an unallocated output field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, target_size, error_status)
            if (error_status /= 0) return
        end if

        !> Compact the field.
        select case (bit_map)
            case (2**1)
                call copy_field(input_field(:, 1), output_field, error_status)
            case (2**2)
                call copy_field(input_field(1, :), output_field, error_status)
        end select

    end subroutine

    subroutine expand_field_list(field_list, increment, error_status)

        !> Input/output variables.
        type(io_field), allocatable :: field_list(:)
        integer, intent(in), optional :: increment
        integer, intent(out) :: error_status

        !> Local variables.
        type(io_field), allocatable :: temp_list(:)
        integer increment_size

        !> Transfer the increment to a local variable (if present).
        increment_size = 1
        if (present(increment)) increment_size = max(increment, 1)

        !> Status.
        error_status = 0

        !> Transfer fields.
        if (.not. allocated(field_list)) then

            !> No need to transfer if 'field_list' is not allocated.
            allocate(field_list(increment_size))
        else
            !> Allocate temporary field.
            allocate(temp_list(size(field_list) + increment_size))

            !> Transfer fields.
            temp_list(1:size(field_list)) = field_list

            !> Deallocate source array.
            deallocate(field_list)

            !> Reallocate source array and transfer fields.
            call move_alloc(from = temp_list, to = field_list)
        end if

    end subroutine

    subroutine combine_field_list(field_list, buffer, error_status)

        !> Input/output variables.
        type(io_field), allocatable :: field_list(:)
        type(io_field), allocatable :: buffer(:)
        integer, intent(out) :: error_status

        !> Local variables.
        type(io_field), allocatable :: temp_list(:)
        integer n, j, i

        !> Status.
        error_status = 0

        !> Count allocated fields in 'buffer'.
        n = 0
        do i = 1, size(buffer)
            if (allocated(buffer(i)%field)) n = n + 1
        end do

        !> Combine fields.
        if (.not. allocated(field_list)) then

            !> No need to combine if 'field_list' is not allocated, just transfer.
            j = 1
            allocate(field_list(n))
            do i = 1, size(buffer)
                if (allocated(buffer(i)%field)) then
                    field_list(j) = buffer(i)
                    j = j + 1
                end if
            end do
            deallocate(buffer)
        else if (n > 0) then

            !> Count allocated fields in 'field_list'.
            do i = 1, size(field_list)
                if (allocated(field_list(i)%field)) n = n + 1
            end do

            !> Combine the arrays.
            allocate(temp_list(n))
            j = 1
            do i = 1, size(field_list)
                if (allocated(field_list(i)%field)) then
                    temp_list(j) = field_list(i)
                    j = j + 1
                end if
            end do
            do i = 1, size(buffer)
                if (allocated(buffer(i)%field)) then
                    temp_list(j) = buffer(i)
                    j = j + 1
                end if
            end do
            deallocate(field_list, buffer)

            !> Transfer back to 'field_list'.
            call move_alloc(from = temp_list, to = field_list)
        end if

    end subroutine

    subroutine copy_field_list(target_list, source_list, error_status)

        !> Input/output variables.
        type(io_field), allocatable :: target_list(:)
        type(io_field), intent(in) :: source_list(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer n, j, i

        !> Count allocated fields in 'buffer'.
        n = 0
        do i = 1, size(source_list)
            if (allocated(source_list(i)%field)) n = n + 1
        end do

        !> Expand field list.
        if (.not. allocated(target_list)) then
            j = 1
        else
            j = size(target_list) + 1
        end if
        call expand_field_list(target_list, n, error_status)
        if (error_status == 0) then

            !> Copy over fields.
            do i = 1, size(source_list)
                if (allocated(source_list(i)%field)) then
                    target_list(j) = source_list(i)
                    j = j + 1
                end if
            end do
        end if

    end subroutine

    subroutine cleanup_field_list(field_list, error_status)

        !> Input/output variables.
        type(io_field), allocatable :: field_list(:)
        integer, intent(out) :: error_status

        !> Local variables.
        type(io_field), allocatable :: temp_list(:)
        integer n, j, i

        !> Status.
        error_status = 0

        !> Count the number of valid fields.
        n = 0
        do i = 1, size(field_list)
            if (allocated(field_list(i)%field)) n = n + 1
        end do

        !> Compact the list.
        if (n > 0) then

            !> Transfer valid fields to a temporary list.
            allocate(temp_list(n))
            j = 1
            do i = 1, size(field_list)
                if (allocated(field_list(i)%field)) then
                    temp_list(j) = field_list(i)
                    j = j + 1
                end if
            end do

            !> Deallocate the source list.
            deallocate(field_list)

            !> Rebuild the source list with only the valid fields.
            call move_alloc(from = temp_list, to = field_list)
        else

            !> Deallocate the list.
            deallocate(field_list)
        end if

    end subroutine

    subroutine explode_dimensions_4d_to_5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        class(*) output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions(shape(output_field), size(shape(input_field)), flattened_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (flattened_dims(1))
            case (5)
                call copy_field(input_field, output_field(:, :, :, :, 1), error_status)
            case (4)
                call copy_field(input_field, output_field(:, :, :, 1, :), error_status)
            case (3)
                call copy_field(input_field, output_field(:, :, 1, :, :), error_status)
            case (2)
                call copy_field(input_field, output_field(:, 1, :, :, :), error_status)
            case (1)
                call copy_field(input_field, output_field(1, :, :, :, :), error_status)
        end select

    end subroutine

    subroutine explode_dimensions_3d_to_5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        class(*) output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions(shape(output_field), size(shape(input_field)), flattened_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (2**flattened_dims(1) + 2**flattened_dims(2))
            case (2**1 + 2**2)
                call copy_field(input_field, output_field(1, 1, :, :, :), error_status)
            case (2**1 + 2**3)
                call copy_field(input_field, output_field(1, :, 1, :, :), error_status)
            case (2**1 + 2**4)
                call copy_field(input_field, output_field(1, :, :, 1, :), error_status)
            case (2**1 + 2**5)
                call copy_field(input_field, output_field(1, :, :, :, 1), error_status)
            case (2**2 + 2**3)
                call copy_field(input_field, output_field(:, 1, 1, :, :), error_status)
            case (2**2 + 2**4)
                call copy_field(input_field, output_field(:, 1, :, 1, :), error_status)
            case (2**2 + 2**5)
                call copy_field(input_field, output_field(:, 1, :, :, 1), error_status)
            case (2**3 + 2**4)
                call copy_field(input_field, output_field(:, :, 1, 1, :), error_status)
            case (2**3 + 2**5)
                call copy_field(input_field, output_field(:, :, 1, :, 1), error_status)
            case (2**4 + 2**5)
                call copy_field(input_field, output_field(:, :, :, 1, 1), error_status)
        end select

    end subroutine

    subroutine explode_dimensions_3d_to_4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        class(*) output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions(shape(output_field), size(shape(input_field)), flattened_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (flattened_dims(1))
            case (4)
                call copy_field(input_field, output_field(:, :, :, 1), error_status)
            case (3)
                call copy_field(input_field, output_field(:, :, 1, :), error_status)
            case (2)
                call copy_field(input_field, output_field(:, 1, :, :), error_status)
            case (1)
                call copy_field(input_field, output_field(1, :, :, :), error_status)
        end select

    end subroutine

    subroutine explode_dimensions_2d_to_5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        class(*) output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions(shape(output_field), size(shape(input_field)), flattened_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (2**flattened_dims(1) + 2**flattened_dims(2) + 2**flattened_dims(3))
            case (2**1 + 2**2 + 2**3)
                call copy_field(input_field, output_field(1, 1, 1, :, :), error_status)
            case (2**1 + 2**2 + 2**4)
                call copy_field(input_field, output_field(1, 1, :, 1, :), error_status)
            case (2**1 + 2**2 + 2**5)
                call copy_field(input_field, output_field(1, 1, :, :, 1), error_status)
            case (2**1 + 2**3 + 2**4)
                call copy_field(input_field, output_field(1, :, 1, 1, :), error_status)
            case (2**1 + 2**3 + 2**5)
                call copy_field(input_field, output_field(1, :, 1, :, 1), error_status)
            case (2**1 + 2**4 + 2**5)
                call copy_field(input_field, output_field(1, :, :, 1, 1), error_status)
            case (2**2 + 2**3 + 2**4)
                call copy_field(input_field, output_field(:, 1, 1, 1, :), error_status)
            case (2**2 + 2**3 + 2**5)
                call copy_field(input_field, output_field(:, 1, 1, :, 1), error_status)
            case (2**2 + 2**4 + 2**5)
                call copy_field(input_field, output_field(:, 1, :, 1, 1), error_status)
            case (2**3 + 2**4 + 2**5)
                call copy_field(input_field, output_field(:, :, 1, 1, 1), error_status)
        end select

    end subroutine

    subroutine explode_dimensions_2d_to_4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        class(*) output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions(shape(output_field), size(shape(input_field)), flattened_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (2**flattened_dims(1) + 2**flattened_dims(2))
            case (2**1 + 2**2)
                call copy_field(input_field, output_field(1, 1, :, :), error_status)
            case (2**1 + 2**3)
                call copy_field(input_field, output_field(1, :, 1, :), error_status)
            case (2**1 + 2**4)
                call copy_field(input_field, output_field(1, :, :, 1), error_status)
            case (2**2 + 2**3)
                call copy_field(input_field, output_field(:, 1, 1, :), error_status)
            case (2**2 + 2**4)
                call copy_field(input_field, output_field(:, 1, :, 1), error_status)
            case (2**3 + 2**4)
                call copy_field(input_field, output_field(:, :, 1, 1), error_status)
        end select

    end subroutine

    subroutine explode_dimensions_2d_to_3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        class(*) output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions(shape(output_field), size(shape(input_field)), flattened_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (flattened_dims(1))
            case (3)
                call copy_field(input_field, output_field(:, :, 1), error_status)
            case (2)
                call copy_field(input_field, output_field(:, 1, :), error_status)
            case (1)
                call copy_field(input_field, output_field(1, :, :), error_status)
        end select

    end subroutine

    subroutine explode_dimensions_1d_to_5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        class(*) output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field))), targeted_dims(size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions( &
            shape(output_field), size(targeted_dims), flattened_dims, targeted_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (targeted_dims(1))
            case (5)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, 1, 1, 1, :), error_status)
                    class default
                        call copy_field(input_field, output_field(1, 1, 1, 1, :), error_status)
                end select
            case (4)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, 1, 1, :, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(1, 1, 1, :, 1), error_status)
                end select
            case (3)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, 1, :, 1, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(1, 1, :, 1, 1), error_status)
                end select
            case (2)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, :, 1, 1, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(1, :, 1, 1, 1), error_status)
                end select
            case (1)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(:, 1, 1, 1, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(:, 1, 1, 1, 1), error_status)
                end select
        end select

    end subroutine

    subroutine explode_dimensions_1d_to_4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        class(*) output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field))), targeted_dims(size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions( &
            shape(output_field), size(targeted_dims), flattened_dims, targeted_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (targeted_dims(1))
            case (4)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, 1, 1, :), error_status)
                    class default
                        call copy_field(input_field, output_field(1, 1, 1, :), error_status)
                end select
            case (3)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, 1, :, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(1, 1, :, 1), error_status)
                end select
            case (2)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, :, 1, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(1, :, 1, 1), error_status)
                end select
            case (1)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(:, 1, 1, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(:, 1, 1, 1), error_status)
                end select
        end select

    end subroutine

    subroutine explode_dimensions_1d_to_3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        class(*) output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field))), targeted_dims(size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions( &
            shape(output_field), size(targeted_dims), flattened_dims, targeted_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (targeted_dims(1))
            case (3)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, 1, :), error_status)
                    class default
                        call copy_field(input_field, output_field(1, 1, :), error_status)
                end select
            case (2)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, :, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(1, :, 1), error_status)
                end select
            case (1)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(:, 1, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(:, 1, 1), error_status)
                end select
        end select

    end subroutine

    subroutine explode_dimensions_1d_to_2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        class(*) output_field(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer flattened_dims(size(shape(output_field)) - size(shape(input_field))), targeted_dims(size(shape(input_field)))

        !> Check for a flattended dimension.
        call map_flattened_dimensions( &
            shape(output_field), size(targeted_dims), flattened_dims, targeted_dims, error_status = error_status)
        if (error_status /= 0) return

        !> Assign the field.
        select case (targeted_dims(1))
            case (2)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(1, :), error_status)
                    class default
                        call copy_field(input_field, output_field(1, :), error_status)
                end select
            case (1)
                select type (input_field)
                    type is (character(len = *))
                        call copy_field(input_field, output_field(:, 1), error_status)
                    class default
                        call copy_field(input_field, output_field(:, 1), error_status)
                end select
        end select

    end subroutine

    subroutine check_field_dimensions_5d(input_field_dat, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field_dat(:, :, :, :, :)
        integer, intent(in), optional :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Return status.
        error_status = 0

        !> Check map.
        if (present(ordered_map)) then
            if (.not. size(ordered_map, 1) == 5) error_status = 1
        end if

        !> Check dimensions.
        if (.not. all(shape(input_field_dat) > 0)) error_status = 1

    end subroutine

    subroutine check_field_dimensions_4d(input_field_dat, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field_dat(:, :, :, :)
        integer, intent(in), optional :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Return status.
        error_status = 0

        !> Check map.
        if (present(ordered_map)) then
            if (.not. size(ordered_map, 1) == 4) error_status = 1
        end if

        !> Check dimensions.
        if (.not. all(shape(input_field_dat) > 0)) error_status = 1

    end subroutine

    subroutine check_field_dimensions_3d(input_field_dat, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field_dat(:, :, :)
        integer, intent(in), optional :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Return status.
        error_status = 0

        !> Check map.
        if (present(ordered_map)) then
            if (.not. size(ordered_map, 1) == 3) error_status = 1
        end if

        !> Check dimensions.
        if (.not. all(shape(input_field_dat) > 0)) error_status = 1

    end subroutine

    subroutine check_field_dimensions_2d(input_field_dat, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field_dat(:, :)
        integer, intent(in), optional :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Return status.
        error_status = 0

        !> Check map.
        if (present(ordered_map)) then
            if (.not. size(ordered_map, 1) == 2) error_status = 1
        end if

        !> Check dimensions.
        if (.not. all(shape(input_field_dat) > 0)) error_status = 1

    end subroutine

    subroutine check_field_dimensions_1d(input_field_dat, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field_dat(:)
        integer, intent(in), optional :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Return status.
        error_status = 0

        !> Check map.
        if (present(ordered_map)) then
            if (.not. size(ordered_map, 1) == 1) error_status = 1
        end if

        !> Check dimensions.
        if (.not. all(shape(input_field_dat) > 0)) error_status = 1

    end subroutine

    subroutine map_field_5d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        class(*) output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, ierr

        !> Check dimensions.
        call check_field_dimensions(input_field, ordered_map, error_status)
        if (error_status /= 0) return

        !> Map field.
        do i = 1, size(output_field)
            call copy_field( &
                input_field(ordered_map(1, i), ordered_map(2, i), ordered_map(3, i), ordered_map(4, i), ordered_map(5, i)), &
                output_field(i), ierr)
            if (ierr /= 0) error_status = 1
        end do

    end subroutine

    subroutine map_field_4d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        class(*) output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, ierr

        !> Check dimensions.
        call check_field_dimensions(input_field, ordered_map, error_status)
        if (error_status /= 0) return

        !> Map field.
        do i = 1, size(output_field)
            call copy_field( &
                input_field(ordered_map(1, i), ordered_map(2, i), ordered_map(3, i), ordered_map(4, i)), output_field(i), ierr)
            if (ierr /= 0) error_status = 1
        end do

    end subroutine

    subroutine map_field_3d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        class(*) output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, ierr

        !> Check dimensions.
        call check_field_dimensions(input_field, ordered_map, error_status)
        if (error_status /= 0) return

        !> Map field.
        do i = 1, size(output_field)
            call copy_field(input_field(ordered_map(1, i), ordered_map(2, i), ordered_map(3, i)), output_field(i), ierr)
            if (ierr /= 0) error_status = 1
        end do

    end subroutine

    subroutine map_field_2d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        class(*) output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, ierr

        !> Check dimensions.
        call check_field_dimensions(input_field, ordered_map, error_status)
        if (error_status /= 0) return

        !> Map field.
        do i = 1, size(output_field)
            call copy_field(input_field(ordered_map(1, i), ordered_map(2, i)), output_field(i), ierr)
            if (ierr /= 0) error_status = 1
        end do

    end subroutine

    subroutine map_field_1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        class(*) output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, ierr

        !> Check dimensions.
        call check_field_dimensions(input_field, ordered_map, error_status)
        if (error_status /= 0) return

        !> Map field.
        do i = 1, size(output_field)
            select type (input_field)
                type is (character(len = *))
                    select type (output_field)
                        type is (character(len = *))
                            call copy_field(input_field(ordered_map(1, i)), output_field(i), ierr)
                        class default
                            call copy_field(input_field(ordered_map(1, i)), output_field(i), ierr)
                    end select
                class default
                    call copy_field(input_field(ordered_map(1, i)), output_field(i), ierr)
            end select
            if (ierr /= 0) error_status = 1
        end do

    end subroutine

    subroutine assign_field_5d_to_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        call copy_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_int5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        call copy_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_int4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_int3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_5d_to_scalar(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        class(*) output_field
        integer, intent(out) :: error_status

        !> Check dimensions.
        call check_field_dimensions(input_field, error_status = error_status)

        !> Assign field.
        call copy_field(input_field(1, 1, 1, 1, 1), output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        real, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        call copy_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_int4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        call copy_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        real, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_int3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_scalar(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        class(*) output_field
        integer, intent(out) :: error_status

        !> Check dimensions.
        call check_field_dimensions(input_field, error_status = error_status)

        !> Assign field.
        call copy_field(input_field(1, 1, 1, 1), output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        real, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/ &
                size(input_field, 1), size(input_field, 2), size(input_field, 3), size(input_field, 4), 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_4d_to_int5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/ &
                size(input_field, 1), size(input_field, 2), size(input_field, 3), size(input_field, 4), 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        real, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        call copy_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_int3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        call copy_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_scalar(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        class(*) output_field
        integer, intent(out) :: error_status

        !> Check dimensions.
        call check_field_dimensions(input_field, error_status = error_status)

        !> Assign field.
        call copy_field(input_field(1, 1, 1), output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        real, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/ &
                size(input_field, 1), size(input_field, 2), size(input_field, 3), 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_int5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/ &
                size(input_field, 1), size(input_field, 2), size(input_field, 3), 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        real, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), size(input_field, 2), size(input_field, 3), 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_3d_to_int4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), size(input_field, 2), size(input_field, 3), 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        call copy_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        call copy_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Compact field.
        call compact_field(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_scalar(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        class(*) output_field
        integer, intent(out) :: error_status

        !> Check dimensions.
        call check_field_dimensions(input_field, error_status = error_status)

        !> Assign field.
        call copy_field(input_field(1, 1), output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        real, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), size(input_field, 2), 1, 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_int5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), size(input_field, 2), 1, 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        real, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), size(input_field, 2), 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_int4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), size(input_field, 2), 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        real, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), size(input_field, 2), 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_2d_to_int3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), size(input_field, 2), 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        call explode_dimensions(input_field, output_field, error_status)

    end subroutine

    subroutine assign_field_1d_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        select type (input_field)
            type is (character(len = *))
                call copy_field(input_field, output_field, error_status)
            class default
                call copy_field(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        select type (input_field)
            type is (character(len = *))
                call copy_field(input_field, output_field, error_status)
            class default
                call copy_field(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_char1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        character(len = *), allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, shape(input_field), error_status)
            if (error_status /= 0) return
        end if

        !> Assign field.
        select type (input_field)
            type is (character(len = *))
                call copy_field(input_field, output_field, error_status)
            class default
                call copy_field(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_scalar(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        class(*) output_field
        integer, intent(out) :: error_status

        !> Check dimensions.
        call check_field_dimensions(input_field, error_status = error_status)

        !> Assign field.
        call copy_field(input_field(1), output_field, error_status)

    end subroutine

    subroutine assign_field_1d_to_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        real, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), 1, 1, 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        select type (input_field)
            type is (character(len = *))
                call explode_dimensions(input_field, output_field, error_status)
            class default
                call explode_dimensions(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_int5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        integer, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), 1, 1, 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        select type (input_field)
            type is (character(len = *))
                call explode_dimensions(input_field, output_field, error_status)
            class default
                call explode_dimensions(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        real, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), 1, 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        select type (input_field)
            type is (character(len = *))
                call explode_dimensions(input_field, output_field, error_status)
            class default
                call explode_dimensions(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_int4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        integer, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), 1, 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        select type (input_field)
            type is (character(len = *))
                call explode_dimensions(input_field, output_field, error_status)
            class default
                call explode_dimensions(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        real, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        select type (input_field)
            type is (character(len = *))
                call explode_dimensions(input_field, output_field, error_status)
            class default
                call explode_dimensions(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_int3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        integer, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), 1, 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        select type (input_field)
            type is (character(len = *))
                call explode_dimensions(input_field, output_field, error_status)
            class default
                call explode_dimensions(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        select type (input_field)
            type is (character(len = *))
                call explode_dimensions(input_field, output_field, error_status)
            class default
                call explode_dimensions(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_field_1d_to_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Allocate an unallocated field.
        if (.not. allocated(output_field)) then
            call allocate_field(output_field, (/size(input_field, 1), 1/), error_status)
            if (error_status /= 0) return
        end if

        !> Assign the field.
        select type (input_field)
            type is (character(len = *))
                call explode_dimensions(input_field, output_field, error_status)
            class default
                call explode_dimensions(input_field, output_field, error_status)
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_real5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        real, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1, 1, 1, 1, 1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                select type (input_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        output_field = real(input_field)
                    class default
                        error_status = 1
                end select
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_int5d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        integer, allocatable :: output_field(:, :, :, :, :)
        integer, intent(out) :: error_status

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1, 1, 1, 1, 1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                select type (input_field)
                    type is (real)
                        output_field = int(input_field)
                    type is (integer)
                        output_field = input_field
                    class default
                        error_status = 1
                end select
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_real4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        real, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1, 1, 1, 1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                select type (input_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        output_field = real(input_field)
                    class default
                        error_status = 1
                end select
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_int4d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        integer, allocatable :: output_field(:, :, :, :)
        integer, intent(out) :: error_status

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1, 1, 1, 1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                select type (input_field)
                    type is (real)
                        output_field = int(input_field)
                    type is (integer)
                        output_field = input_field
                    class default
                        error_status = 1
                end select
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_real3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        real, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1, 1, 1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                select type (input_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        output_field = real(input_field)
                    class default
                        error_status = 1
                end select
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_int3d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        integer, allocatable :: output_field(:, :, :)
        integer, intent(out) :: error_status

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1, 1, 1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                select type (input_field)
                    type is (real)
                        output_field = int(input_field)
                    type is (integer)
                        output_field = input_field
                    class default
                        error_status = 1
                end select
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_real2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        real, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1, 1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                select type (input_field)
                    type is (real)
                        output_field = input_field
                    type is (integer)
                        output_field = real(input_field)
                    class default
                        error_status = 1
                end select
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_int2d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        integer, allocatable :: output_field(:, :)
        integer, intent(out) :: error_status

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1, 1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                select type (input_field)
                    type is (real)
                        output_field = int(input_field)
                    type is (integer)
                        output_field = input_field
                    class default
                        error_status = 1
                end select
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_real1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        real, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer ierr

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                if (size(output_field) > 0) then
                    call copy_field(input_field, output_field(1), error_status)
                    if (error_status == 0) then
                        output_field = output_field(1)
                    end if
                end if
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_int1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        integer, allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer ierr

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                if (size(output_field) > 0) then
                    call copy_field(input_field, output_field(1), error_status)
                    if (error_status == 0) then
                        output_field = output_field(1)
                    end if
                end if
        end select

    end subroutine

    recursive subroutine assign_field_scalar_to_char1d(input_field, output_field, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field
        character(len = *), allocatable :: output_field(:)
        integer, intent(out) :: error_status

        !> Local variables.
        integer ierr

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Allocate an unallocated field.
                if (.not. allocated(output_field)) then
                    call allocate_field(output_field, (/1/), error_status)
                    if (error_status /= 0) return
                else
                    error_status = 0
                end if

                !> Assign field.
                if (size(output_field) > 0) then
                    call copy_field(input_field, output_field(1), error_status)
                    if (error_status == 0) then
                        output_field = output_field(1)
                    end if
                end if
        end select

    end subroutine

    subroutine assign_field_scalar_to_scalar(input_field, output_field, error_status)

        !> Input/output variables.
        class(*) input_field
        class(*) output_field
        integer, intent(out) :: error_status

        !> Check field type.
        select type (input_field)

            !> Model variable types.
            type is (model_variable_real5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_real)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int5d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int4d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int3d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int2d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_int)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char1d)
                call assign_field(input_field%dat, output_field, error_status)
            type is (model_variable_char)
                call assign_field(input_field%dat, output_field, error_status)

            !> I/O field.
            type is (io_field)

                !> Return if the field is empty.
                if (.not. allocated(input_field%field)) then
                    error_status = 1
                    return
                else

                    !> Assign field.
                    call assign_field(input_field%field, output_field, error_status)
                end if
            class default

                !> Assign field.
                call copy_field(input_field, output_field, error_status)
        end select

    end subroutine

    subroutine assign_mapped_value_5d_to_real1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        real, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        call map_field(input_field, output_field, ordered_map, error_status)

    end subroutine

    subroutine assign_mapped_value_5d_to_int1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :, :)
        integer, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        call map_field(input_field, output_field, ordered_map, error_status)

    end subroutine

    subroutine assign_mapped_value_4d_to_real1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        real, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        call map_field(input_field, output_field, ordered_map, error_status)

    end subroutine

    subroutine assign_mapped_value_4d_to_int1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :, :)
        integer, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        call map_field(input_field, output_field, ordered_map, error_status)

    end subroutine

    subroutine assign_mapped_value_3d_to_real1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        real, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        call map_field(input_field, output_field, ordered_map, error_status)

    end subroutine

    subroutine assign_mapped_value_3d_to_int1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :, :)
        integer, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        call map_field(input_field, output_field, ordered_map, error_status)

    end subroutine

    subroutine assign_mapped_value_2d_to_real1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        real, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        call map_field(input_field, output_field, ordered_map, error_status)

    end subroutine

    subroutine assign_mapped_value_2d_to_int1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:, :)
        integer, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        call map_field(input_field, output_field, ordered_map, error_status)

    end subroutine

    subroutine assign_mapped_value_1d_to_real1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        real, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        select type (input_field)
            type is (character(len = *))
                call map_field(input_field, output_field, ordered_map, error_status)
            class default
                call map_field(input_field, output_field, ordered_map, error_status)
        end select

    end subroutine

    subroutine assign_mapped_value_1d_to_int1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        integer, allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        select type (input_field)
            type is (character(len = *))
                call map_field(input_field, output_field, ordered_map, error_status)
            class default
                call map_field(input_field, output_field, ordered_map, error_status)
        end select

    end subroutine

    subroutine assign_mapped_value_1d_to_char1d(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        class(*), intent(in) :: input_field(:)
        character(len = *), allocatable :: output_field(:)
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Assign field.
        if (.not. allocated(output_field)) allocate(output_field(size(ordered_map, 2)))
        select type (input_field)
            type is (character(len = *))
                call map_field(input_field, output_field, ordered_map, error_status)
            class default
                call map_field(input_field, output_field, ordered_map, error_status)
        end select

    end subroutine

    subroutine assign_mapped_value_io_field_to_scalar(input_field, output_field, ordered_map, error_status)

        !> Input/output variables.
        type(io_field) input_field
        class(*) output_field
        integer, intent(in) :: ordered_map(:, :)
        integer, intent(out) :: error_status

        !> Return status.
        error_status = 0

        select type (this => input_field%field)
            type is (model_variable_real5d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_real4d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_real3d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_real2d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_real1d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_char1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_int5d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_int4d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_int3d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_int2d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_int1d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_char1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            type is (model_variable_char1d)
                select type (output_field)
                    type is (model_variable_real1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_int1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    type is (model_variable_char1d)
                        call assign_mapped_value(this%dat, output_field%dat, ordered_map, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
            class default
                select type (output_field)
                    type is (model_variable_real5d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_real4d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_real3d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_real2d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_real1d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_real)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_int5d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_int4d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_int3d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_int2d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_int1d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_int)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_char1d)
                        call assign_field(input_field, output_field%dat, error_status)
                    type is (model_variable_char)
                        call assign_field(input_field, output_field%dat, error_status)
                    class default
                        call assign_field(input_field, output_field, error_status)
                end select
        end select

    end subroutine

    subroutine assign_field_to_mapped_value(input_field, error_status)

        !> Input/output variables.
        type(io_field) input_field
        integer, intent(out) :: error_status

        !> Return status.
        error_status = 0

        !> Assign field.
        if (allocated(input_field%mapping%cell_map)) then
            call assign_mapped_value(input_field, input_field%mapping%mapped_to_cell, input_field%mapping%cell_map, error_status)
        end if
        if (allocated(input_field%mapping%tile_map)) then
            call assign_mapped_value(input_field, input_field%mapping%mapped_to_tile, input_field%mapping%tile_map, error_status)
        end if

    end subroutine

    subroutine get_dimension_order(field_dim_names, desired_dim_names, mapped_dim_order, error_status)

        !> 'strings': For 'uppercase' function.
        use strings, only: uppercase

        !> Input/output variables.
        character(len = *), dimension(:), intent(in) :: field_dim_names
        character(len = *), dimension(:), intent(in) :: desired_dim_names
        integer, dimension(:), allocatable, intent(out) :: mapped_dim_order
        integer, intent(out) :: error_status

        !> Local variables.
        integer j, i

        !> Status.
        error_status = 0

        !> Check dimension names.
        if (allocated(mapped_dim_order)) deallocate(mapped_dim_order)
        allocate(mapped_dim_order(size(desired_dim_names)))
        mapped_dim_order = 0
        do i = 1, size(desired_dim_names)
            if (any(DIM_NAMES_OF_Y == uppercase(desired_dim_names(i)))) then
                do j = 1, size(field_dim_names)
                    if (any(DIM_NAMES_OF_Y == uppercase(field_dim_names(j)))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            else if (any(DIM_NAMES_OF_X == uppercase(desired_dim_names(i)))) then
                do j = 1, size(field_dim_names)
                    if (any(DIM_NAMES_OF_X == uppercase(field_dim_names(j)))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            else if (any(DIM_NAMES_OF_T == uppercase(desired_dim_names(i)))) then
                do j = 1, size(field_dim_names)
                    if (any(DIM_NAMES_OF_T == uppercase(field_dim_names(j)))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            else if (any(DIM_NAMES_OF_N == uppercase(desired_dim_names(i)))) then
                do j = 1, size(field_dim_names)
                    if (any(DIM_NAMES_OF_N == uppercase(field_dim_names(j)))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            else if (any(DIM_NAMES_OF_M == uppercase(desired_dim_names(i)))) then
                do j = 1, size(field_dim_names)
                    if (any(DIM_NAMES_OF_M == uppercase(field_dim_names(j)))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            else if (any(DIM_NAMES_OF_K == uppercase(desired_dim_names(i)))) then
                do j = 1, size(field_dim_names)
                    if (any(DIM_NAMES_OF_K == uppercase(field_dim_names(j)))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            else if (any(DIM_NAMES_OF_B == uppercase(desired_dim_names(i)))) then
                do j = 1, size(field_dim_names)
                    if (any(DIM_NAMES_OF_B == uppercase(field_dim_names(j)))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            else if (any(DIM_NAMES_OF_G == uppercase(desired_dim_names(i)))) then
                do j = 1, size(field_dim_names)
                    if (any(DIM_NAMES_OF_G == uppercase(field_dim_names(j)))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            else if (any(DIM_NAMES_OF_L == uppercase(desired_dim_names(i)))) then
                do j = 1, size(field_dim_names)
                    if (any(DIM_NAMES_OF_L == uppercase(field_dim_names(j)))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            else
                do j = 1, size(field_dim_names)
                    if (uppercase(field_dim_names(j)) == uppercase(desired_dim_names(i))) then
                        mapped_dim_order(i) = j
                        exit
                    end if
                end do
            end if
        end do

        !> Check for missing dimensions.
        if (all(mapped_dim_order == 0)) error_status = 1

    end subroutine

    subroutine get_field_name_and_level(label, field_name, level, level_dim_name, level_id, error_status)

        !> 'strings': For 'uppercase' function.
        use strings, only: uppercase

        !> Input/output variables.
        character(len = *), intent(in) :: label
        character(len = *), intent(out), optional :: field_name
        character(len = *), intent(out), optional :: level
        character(len = *), intent(out), optional :: level_dim_name
        integer, intent(out), optional :: level_id
        integer, intent(out) :: error_status

        !> Local variables.
        character(len = SHORT_FIELD_LENGTH) fld_name, lvl, lvl_dim_name
        integer a1, a2, ilvl, ierr

        !> Status.
        error_status = 0

        !> Transfer the label to a temporary field and convert to uppercase.
        fld_name = trim(uppercase(label))

        !> Subset the field ID.
        a1 = index(trim(fld_name), ' ')
        a2 = index(trim(fld_name), ' ', back = .true.)
        lvl = ''
        lvl_dim_name = ''
        ilvl = 0
        if (a1 > 0) then

            !> Slice out the level ID.
            if (a2 /= a1) then
                a2 = index(fld_name((a1 + 1):), ' ')
                lvl = fld_name((a1 + 1):(a2 + a1))
            else
                lvl = fld_name((a1 + 1):)
            end if
            if (index(lvl, '=') > 0) then
                lvl_dim_name = lvl(1:(index(lvl, '=') - 1))
                lvl = lvl((index(lvl, '=') + 1):)
            end if
            fld_name = fld_name(1:a1)
            read(lvl, *, iostat = ierr) ilvl
            if (ierr /= 0) ilvl = 0
        end if

        !> Check the dimension.
        if (len_trim(lvl_dim_name) > 0) then
            if (any(DIM_NAMES_OF_Y == lvl_dim_name)) then
                lvl_dim_name = DIM_NAME_Y
            else if (any(DIM_NAMES_OF_X == lvl_dim_name)) then
                lvl_dim_name = DIM_NAME_X
            else if (any(DIM_NAMES_OF_T == lvl_dim_name)) then
                lvl_dim_name = DIM_NAME_T
            else if (any(DIM_NAMES_OF_N == lvl_dim_name)) then
                lvl_dim_name = DIM_NAME_N
            else if (any(DIM_NAMES_OF_M == lvl_dim_name)) then
                lvl_dim_name = DIM_NAME_M
            else if (any(DIM_NAMES_OF_K == lvl_dim_name)) then
                lvl_dim_name = DIM_NAME_K
            else if (any(DIM_NAMES_OF_B == lvl_dim_name)) then
                lvl_dim_name = DIM_NAME_B
            else if (any(DIM_NAMES_OF_G == lvl_dim_name)) then
                lvl_dim_name = DIM_NAME_G
            else if (any(DIM_NAMES_OF_L == lvl_dim_name)) then
                lvl_dim_name = DIM_NAME_L
            end if
        end if

        !> Assign to output variables.
        if (present(field_name)) field_name = trim(fld_name)
        if (present(level)) level = trim(lvl)
        if (present(level_dim_name)) level_dim_name = trim(lvl_dim_name)
        if (present(level_id)) level_id = ilvl

    end subroutine

end module
