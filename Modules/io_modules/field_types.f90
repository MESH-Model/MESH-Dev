!> Description:
!>  Module containing field types.
module field_types

    !> 'mesh_io_constants': For I/O constants.
    use mesh_io_constants

    !> 'variable_types': For model variable types.
    use variable_types

    implicit none

    !> Description:
    !>  Generic attributes for a mapped field (e.g., to dimensions of a model variable).
    !>
    !> Variables:
    !*  mapped_name: Name if mapped to an internal variable.
    !*  mapped_dim_order: Pre-derived mapping to spatial orders.
    !*  time_order: Index of the time dimension for multi-dimensional fields.
    !*  level_order: Index of the level dimension for multi-dimensional fields.
    !*  cell_map: Pre-derived mapping of the multi-dimensional field to 'cell' field.
    !*  mapped_to_cell: Mapped values by 'cell_map'.
    !*  mapped_dat_cell_interp: Previous mapped 'cell' values used for temporal interpolation.
    !*  tile_map: Pre-derived mapping of the multi-dimensional field to 'tile' field.
    !*  mapped_to_tile: Mapped values by 'tile_map'.
    !*  mapped_dat_tile_interp: Previous mapped 'tile' values used for temporal interpolation.
    type map_info
        character(len = SHORT_FIELD_LENGTH) :: mapped_name = ''
        integer, dimension(:), allocatable :: mapped_dim_order
        integer :: time_order = 0
        integer :: level_order = 0
        integer, dimension(:, :), allocatable :: cell_map
        class(*), allocatable :: mapped_to_cell
        class(*), allocatable :: mapped_to_cell_interp
        integer, dimension(:, :), allocatable :: tile_map
        class(*), allocatable :: mapped_to_tile
        class(*), allocatable :: mapped_to_tile_interp
    end type

    !> Description:
    !>  Generic structure for an I/O field.
    !>
    !> Variables:
    !*  label: Label or ID.
    !*  id: Variable ID (e.g., if from an indexed input file).
    !*  field_name: Field name derived from label or ID.
    !*  level: Level or category derived from label or ID.
    !*  level_id: Level ID for numeric level derived from label or ID.
    !*  field_type: Data frame.
    !*  var: Mapped model variable (and related indices).
    !*  dim_names: Names of the field dimensions.
    !*  dim_lengths: Lengths of each field dimension.
    type io_field
        character(len = SHORT_FIELD_LENGTH) :: label = ''
        integer :: id = -1
        character(len = SHORT_FIELD_LENGTH) :: field_name = ''
        character(len = SHORT_FIELD_LENGTH) :: level = ''
        type(model_variable_info) meta
        integer :: level_id = -1
        class(*), allocatable :: field
        type(map_info) mapping
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dim_names
        integer, dimension(:), allocatable :: dim_lengths
    end type

end module
