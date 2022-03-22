!> Description:
!>  Module containing model variable types.
module variable_types

    !> 'mesh_io_constants' for character lengths and 'NODATA' values.
    use mesh_io_constants, only: SHORT_FIELD_LENGTH, LONG_FIELD_LENGTH, NO_DATA_REAL, NO_DATA_INT, NO_DATA_CHAR

    implicit none

    !> Description:
    !>  Model variable of type 'real' (base).
    type, abstract :: model_variable_real_base
        real :: const_mul = 1.0
        real :: const_add = 0.0
        real :: no_data_value = NO_DATA_REAL
        real :: valid_max = NO_DATA_REAL
        real :: valid_min = -NO_DATA_REAL
    end type

    !> Extension: real(:, :, :, :, :).
    type, extends(model_variable_real_base) :: model_variable_real5d
        real, dimension(:, :, :, :, :), allocatable :: dat
    end type

    !> Extension: real(:, :, :, :).
    type, extends(model_variable_real_base) :: model_variable_real4d
        real, dimension(:, :, :, :), allocatable :: dat
    end type

    !> Extension: real(:, :, :).
    type, extends(model_variable_real_base) :: model_variable_real3d
        real, dimension(:, :, :), allocatable :: dat
    end type

    !> Extension: real(:, :).
    type, extends(model_variable_real_base) :: model_variable_real2d
        real, dimension(:, :), allocatable :: dat
    end type

    !> Extension: real(:).
    type, extends(model_variable_real_base) :: model_variable_real1d
        real, dimension(:), allocatable :: dat
    end type

    !> Extension: real.
    type, extends(model_variable_real_base) :: model_variable_real
        real :: dat = NO_DATA_REAL
    end type

    !> Description:
    !>  Model variable of type 'int' (base).
    type, abstract :: model_variable_int_base
        integer :: no_data_value = NO_DATA_INT
        integer :: valid_max = NO_DATA_INT
        integer :: valid_min = -NO_DATA_INT
    end type

    !> Extension: int(:, :, :, :, :).
    type, extends(model_variable_int_base) :: model_variable_int5d
        integer, dimension(:, :, :, :, :), allocatable :: dat
    end type

    !> Extension: int(:, :, :, :).
    type, extends(model_variable_int_base) :: model_variable_int4d
        integer, dimension(:, :, :, :), allocatable :: dat
    end type

    !> Extension: int(:, :, :).
    type, extends(model_variable_int_base) :: model_variable_int3d
        integer, dimension(:, :, :), allocatable :: dat
    end type

    !> Extension: int(:, :).
    type, extends(model_variable_int_base) :: model_variable_int2d
        integer, dimension(:, :), allocatable :: dat
    end type

    !> Extension: int(:).
    type, extends(model_variable_int_base) :: model_variable_int1d
        integer, dimension(:), allocatable :: dat
    end type

    !> Extension: int.
    type, extends(model_variable_int_base) :: model_variable_int
        integer :: dat = NO_DATA_INT
    end type

    !> Description:
    !>  Model variable of type 'char' (base).
    type, abstract :: model_variable_char_base
        character(len = 1) :: no_data_value = NO_DATA_CHAR
    end type

    !> Extension: char(:).
    type, extends(model_variable_char_base) :: model_variable_char1d
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dat
    end type

    !> Extension: char.
    type, extends(model_variable_char_base) :: model_variable_char
        character(len = LONG_FIELD_LENGTH) :: dat = NO_DATA_CHAR
    end type

    !> Description:
    !>  Model variable pointer for type 'real'.
    !>
    !> Variables:
    !*  dat: Pointer.
    type model_variable_pointer_1d
        real, pointer :: dat(:) => null()
    end type

    !> Description:
    !>  Structure for model variable information.
    !>
    !> Variables:
    !*  label: Variable label/ID.
    !*  description: Description.
    !*  units: Units.
    type model_variable_info
        character(len = SHORT_FIELD_LENGTH) :: label = ''
        character(len = LONG_FIELD_LENGTH) :: description = ''
        character(len = SHORT_FIELD_LENGTH) :: units = ''
    end type

    !> Description:
    !>  Wrapper for model variable structures.
    !>
    !> Variables:
    !*  field: Instance of a model variable.
    type model_variable
        class(*), allocatable :: field
    end type

end module
