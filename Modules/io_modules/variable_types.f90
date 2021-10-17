!> Description:
!>  Module containing model variable types.
module variable_types

    !> 'mesh_io_options' for character lengths.
    use mesh_io_options, only: SHORT_FIELD_LENGTH, LONG_FIELD_LENGTH

    implicit none

    !> Description:
    !>  Model variable of type 'real' (base).
    type, abstract :: model_variable_real_base
        real :: const_mul = 1.0
        real :: const_add = 0.0
        real :: no_data_value = huge(0.0)
        real :: valid_max = huge(0.0)
        real :: valid_min = -huge(0.0)
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
        real :: dat = huge(0.0)
    end type

    !> Description:
    !>  Model variable of type 'int' (base).
    type, abstract :: model_variable_int_base
        integer :: no_data_value = huge(0)
        integer :: valid_max = huge(0)
        integer :: valid_min = -huge(0)
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
        integer :: dat = huge(0)
    end type

    !> Description:
    !>  Model variable of type 'char' (base).
    type, abstract :: model_variable_char_base
        character(len = 1) :: no_data_value = achar(0)
    end type

    !> Extension: char(:).
    type, extends(model_variable_char_base) :: model_variable_char1d
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dat
    end type

    !> Extension: char.
    type, extends(model_variable_char_base) :: model_variable_char
        character(len = LONG_FIELD_LENGTH) :: dat = ''
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
