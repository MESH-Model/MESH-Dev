module climate_forcing_variabletypes

    use model_dates

    implicit none

    type clim_info_series

        integer                                   :: nattr
        character(20)                             :: attrtype
        character(200), dimension(:), allocatable :: attr

    end type

!-    type clim_info_transform_series

        !* nattr: Number of attributes in the transform.
        !* attrtype: Type of transform.
        !* attr: Attributes (e.g., coefficients) of the transform [-].
        !* tfs: Array for work or for the transformed series.
!-        integer :: nattr = 0
!-        integer attrtype
!-        real, dimension(:), allocatable :: attr
!-        real, dimension(:, :), allocatable :: tfs

!-    end type

    type clim_series

        !* id_var:  Climate variable name and ID.
        !* factive: Returns .true. if the variable is active.
        !* ffmt:    Input file format.
        !* fname:   Input file name.
        !* fpath:   Full path to the forcing input file, including extension.
        !* fiun:    Input file unit.
        !* fopen:   Returns .true. if an input file for the variable has been opened.
        character(20)  :: id_var = ''
        logical        :: factive = .false.
        integer        :: ffmt = 1
        character(200) :: fname = ''
        character(200) :: fpath = ''
        integer        :: fiun
        logical        :: fopen = .false.

        !* name_var:       Name of variable                in (netcdf) input file.
        !* name_lat:       Name of latitude  (y) dimension in (netcdf) input file.
        !* name_lon:       Name of longitude (x) dimension in (netcdf) input file.
        !* name_time:      Name of time      (t) dimension in (netcdf) input file.
        !* ncol_lat:       Position of latitude  dimension for variable, ie. if var(time,lat,lon) --> ncol_lat  = 2.
        !* ncol_lon:       Position of longitude dimension for variable, ie. if var(time,lat,lon) --> ncol_lon  = 3.
        !* ncol_time:      Position of time      dimension for variable, ie. if var(time,lat,lon) --> ncol_time = 1.
        !* dim_order_case: which order of dimensions: case 1 = (lon,lat,time), 2 = (lat,lon,time),
        !*                                                 3 = (lon,time,lat), 4 = (lat,time,lon),
        !*                                                 5 = (time,lon,lat), 6 = (time,lat,lon)
        !* time_shift:     time shift of forcing data relative to UTC, i.e
        !*                 forcing data in local Central Standard Time (CST  = UTC+3.5): Saskatoon  --> time_shift = -6.0
        !*                 forcing data in local Eastern Standard Time (EST  = UTC-5):   Toronto    --> time_shift = -5.0
        !*                 forcing data in local Iran Standard Time    (IRST = UTC+3.5): Teheran    --> time_shift = +3.5
        !* varid:          ID of variable in NetCDF (nf90_inq_varid)
        character(200) :: name_var       = ''
        character(200) :: name_lat       = ''
        integer        :: ncol_lat       = 0
        character(200) :: name_lon       = ''
        integer        :: ncol_lon       = 0
        character(200) :: name_time      = ''
        integer        :: ncol_time      = 0  
        integer        :: dim_order_case = 0
        real           :: time_shift     = 0.0
        integer        :: varid          = -9

        !* GRD: Values for forcing data (Bounds: 1: Grid).
        !>      Values are averaged to the grid-level for grid-based
        !>      processing and certain output. These gridded values are
        !>      not used to drive the model, as they are not compatible
        !>      with data at the GRU- or GAT-level.
        !* GRU: Values for forcing data (Bounds: 1: GRU).
        !* GAT: Values for forcing data (Bounds: 1: Land Element).
        real, dimension(:), pointer :: GRD
        real, dimension(:), pointer :: GRU
        real, dimension(:), pointer :: GAT

        !* nblocks:   Number of frames of blocks of data to read into memory.
        !* blocktype: Type of data being stored (1 = GRD; 2 = GRU; 3 = GAT).
        !* blocks:    Forcing data (Bounds: 1: Element; 2: nblocks).
        !* iblock:    Index of the current block in data to memory [-].
        !* skip:      Number of frames that have been skipped at the start
        integer                            :: nblocks = 1
        integer                            :: blocktype = 1
        real, dimension(:, :), allocatable :: blocks
        integer                            :: iblock = 1
        integer                            :: skip = 0

        !* unit conversion: new_value = cm * old_value + ca
        !* cm: Multiplicative conversion factor.
        !* ca: Additive conversion factor.
        real :: cm = 1.0
        real :: ca = 0.0

        !* start_date: Starting date of the data in the file.
        !* hf: Increment of minutes passed in each frame of data [mins].
        !* itimestep: Current time-step [mins].
        type(counter_date) :: start_date
        integer            :: hf = 30
        integer            :: itimestep = 0

        !* ipflg: INTERPOLATIONFLAG (0: none, 1: active).
        !* ipwgt: Interpolation type (1: arithmetic mean; 2: harmonic mean).
        !* ipdat: Array to store the states of the forcing data [-] (Bounds: 1: Element; 2: interpolation/previous time-step state).
        integer                            :: ipflg = 0
        integer                            :: ipwgt = 1
        real, dimension(:, :), allocatable :: ipdat

        !* nseries: Number of series in the definition.
        !* series: Definitions for the series.
        integer                                           :: nseries = 0
        type(clim_info_series), dimension(:), allocatable :: series

    end type

    type clim_info

        integer :: basefileunit = 89

        !* nclim: Number of climate variables.
        !* start_date: Starting date of the data (general).
        !* dat: Climate variables.
        integer            :: nclim = 10
        type(counter_date) :: start_date
        type(clim_series)  :: dat(10)

    end type !clim_info

end module
