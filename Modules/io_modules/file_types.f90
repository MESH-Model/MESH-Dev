!> Description:
!>  Module containing file types.
module file_types

    !> 'mesh_io_options': For I/O constants.
    use mesh_io_options

    !> 'field_types': For field types.
    use field_types

    !> 'datetime_types': For 'io_datetime' structure.
    use datetime_types, only: io_datetime

    implicit none

    !> Description:
    !>  Virtual file type (i.e., without external I/O).
    type io_type_virtual

    end type

    !> Description:
    !>  Character-delimited file.
    !>
    !> Variables:
    !*  n_skip_rows: Number of rows to skip (default: 0).
    !*  n_skip_cols: Number of columns to skip (default: 0).
    !*  delimiter: Delimiter (default: ' ').
    type io_type_char_delimited
        integer :: n_skip_rows = 0
        integer :: n_skip_cols = 0
        character :: delimiter = ' '
    end type

    !> Description:
    !>  EnSim Hydrologic/Green Kenue rectangular-cell 'r2c' file.
    !>
    !> Variables:
    !*  binary: .true. if data is stored in binary format.
    type io_type_r2c
        logical :: binary = .false.
    end type

    !> Description:
    !>  Sequential binary (i.e., unformatted) file.
    !>
    !> Variables:
    !*  dim_names: Names of dimensions of fields in the file.
    type io_type_seq

    end type

    !> Description:
    !>  NetCDF file.
    !>
    !> Variables:
    !*  flatten_outputs: .true. to flatten outputs (default: .false.).
    !*      This means to compact dimensions, e.g.: SAND_N(y, x) instead of SAND(y, x, 1:N).
    !*  dim_name_x: Override for 'x' dimension.
    !*  dim_name_y: Override for 'y' dimension.
    !*  dim_name_t: Override for 't' dimension.
    !*  dim_name_m: Override for 'm' dimension.
    !*  dim_name_s: Override for 's' dimension.
    !*  dim_name_c: Override for 'c' dimension.
    !*  dim_name_g: Override for 'g' dimension.
    type io_type_nc
        logical :: flatten_output = .false.
        character(len = SHORT_FIELD_LENGTH) :: dim_name_x = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_y = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_t = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_m = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_s = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_c = ''
        character(len = SHORT_FIELD_LENGTH) :: dim_name_g = ''
    end type

    !> Description:
    !>  CLASS/RUNCLASS 'MET' format file.
    !>
    !> Variables:
    !*  rr_sr: .true. to read extra columns from the file for PRERN/PRESNO.
    type io_type_met
        logical :: rr_sr = .false.
    end type

    !> Description:
    !>  Generic attributes for a multi-frame (i.e., time-series) file.
    !>
    !> Variables:
    !*  multi_frame: .true. if a multi-frame file (default: .false.).
    !*  freq: Frame frequency type for multi-frame files (default: unknown type).
    !*  freq_interval: Frequency interval in units of 'freq' for multi-frame files (default: 1).
    !*  istep: Current step in 'freq_interval' (start: 1).
    !*  start: Date associated with the first frame in a multi-frame file ('io_datetime' type).
    !*  time_offset: Offset to apply in calculating 'start' in hours (default: 0.0).
    !*  block_interval: Number of blocks to read at-a-time for multi-frame files (default: 1).
    !*  iblock: Current block in 'block_interval' (start: 1).
    !*  ipos: Current I/O position in file for multi-frame file (start: 1).
    type series_info
        logical :: multi_frame = .false.
        integer :: freq = FREQ_NUL
        integer :: freq_interval = 0
        integer :: istep = 1
        type(io_datetime) start
        real :: time_offset = 0.0
        integer :: block_interval = 1
        integer :: iblock = 1
        integer :: ipos = 1
    end type

    !> Description:
    !> Field overrides.
    !>
    !> Variables.
    !*  dim_names: The ordered dimension names for all fields in the file.
    !*  dim_lengths: The ordered dimension lengths for all fields in the file.
    !*  const_mul: Multiplicative factor to apply to all fields in the file (default: 1.0).
    !*  const_add: Additive factor to apply to all fields in the file (default: 0.0).
    type field_overrides
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dim_names
        character(len = SHORT_FIELD_LENGTH), dimension(:), allocatable :: dim_lengths
        real :: const_mul = 1.0
        real :: const_add = 0.0
    end type

    !> Description:
    !>  Generic attributes for temporal interoplation.
    !>
    !> Variables:
    !*  scheme: Flag used for temporal interpolation of the field.
    !*  interp_weights: Weights used for interpolation.
    type temporal_interp
        integer :: scheme = 0
        real, dimension(:), allocatable :: interp_weights
    end type

    !> Description:
    !>  Variable structure for file information.
    !>
    !> Variables:
    !*  full_path: Full file path, including extension.
    !*  ext: Extension type (if cannot be determined from 'io_file' extension, default: unknown).
    !*  iunit: File handle/unit.
!todo: remove 'ext'.
    type file_info
        character(len = LONG_FIELD_LENGTH) :: full_path = ''
        integer :: ext = FILE_TYPE_NUL
        integer :: iunit = -1
    end type

    !> Description:
    !>  Generic file structure.
    !>
    !> Variables:
    !*  label: Unique file label or ID.
    !*  subset_ids: Active subset of indices to print for output files (default: no subset).
    !*  fields: List of fields in the file.
    !*  field_map: Map of file field name to internal field name (in list, 1: field name; 2: internal name).
    type, extends(file_info) :: io_file
        character(len = SHORT_FIELD_LENGTH) :: label = ''
        type(series_info) series
        integer, dimension(:), allocatable :: subset_ids
        type(temporal_interp) temporal_interp
        type(io_field), dimension(:), allocatable :: fields
        character(len = SHORT_FIELD_LENGTH), dimension(:, :), allocatable :: field_map
        type(field_overrides) overrides
        class(*), allocatable :: container
    end type

end module
