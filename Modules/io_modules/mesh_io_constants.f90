!> Description:
!>  Module containing I/O (input/output) related constants.
module mesh_io_constants

    implicit none

    !> Constants for file formats.
    !* FILE_TYPE_NUL: None (no file format applicable or not set).
    !* FILE_TYPE_R2C: EnSim Hydrologic/GreenKenue R2C file in ASCII format
    !*  (multi-attribute single framed or single attribute multi-frame).
    !* FILE_TYPE_R2C_BIN: EnSim Hydrologic/GreenKenue R2C file in binary format
    !*  (single attribute multi-frame).
    !* FILE_TYPE_TXT: Space-delimited plain text format.
    !* FILE_TYPE_ASC: Space-delimited plain text format (Rank-order).
    !* FILE_TYPE_CSV: Comma-delimited plain text format.
    !* FILE_TYPE_TSI: Space-delimited plain text format (Rank-subset).
    !* FILE_TYPE_TSK: Space-delimited plain text format (Tile-subset).
    !* FILE_TYPE_SEQ: Binary sequential format (no predefined structure).
    !* FILE_TYPE_NC4: NetCDF format.
    !* FILE_TYPE_MET: CLASS 'MET' format forcing file.
    integer, parameter :: FILE_TYPE_NUL = 0
    integer, parameter :: FILE_TYPE_R2C = 1
    integer, parameter :: FILE_TYPE_R2C_BIN = 2
    integer, parameter :: FILE_TYPE_TXT = 3
    integer, parameter :: FILE_TYPE_ASC = 4
    integer, parameter :: FILE_TYPE_CSV = 5
    integer, parameter :: FILE_TYPE_TSI = 6
    integer, parameter :: FILE_TYPE_TSK = 7
    integer, parameter :: FILE_TYPE_SEQ = 8
    integer, parameter :: FILE_TYPE_NC4 = 9
    integer, parameter :: FILE_TYPE_MET = 10

    !> Constants for field lengths.
    !* SHORT_FIELD_LENGTH: Default length for short fields.
    !* LONG_FIELD_LENGTH: Default length for long fields.
    integer, parameter :: SHORT_FIELD_LENGTH = 200
    integer, parameter :: LONG_FIELD_LENGTH = 2000

    !> Constants for field scale.
    !* DATA_TYPE_GRID: Grid-based (e.g., aggregated from 'tile').
    !* DATA_TYPE_TILE: Tile-based.
    integer, parameter :: DATA_TYPE_GRID = 1
    integer, parameter :: DATA_TYPE_TILE = 2

    !> Constants for binary flag states.
    !* FLAG_OFF: Disabled/inactive.
    !* FLAG_ON: Enabled/active.
    !* FLAG_AUTO: Automatic (e.g., if dependent on other flags disabled or enabled).
    integer, parameter :: FLAG_OFF = 0
    integer, parameter :: FLAG_ON = 1
    integer, parameter :: FLAG_AUTO = 2

    !> Constants for time frequencies of inputs and outputs.
    !* FREQ_NUL: None/no frequency applicable or not set.
    !* FREQ_YEARLY: Yearly, before the beginning of the next year.
    !* FREQ_MONTHLY: Monthly, before the beginning of the next month.
    !* FREQ_SEASONAL: Seasonal monthly output (monthly average across years).
    !* FREQ_DAILY: Daily, before the beginning of the next day.
    !* FREQ_HOURLY: Hourly, before the beginning of the next hour.
    !* FREQ_PTS: Per model time-step (model configuration dependent).
    !* FREQ_START: Only at the start of a simulation.
    !* FREQ_END: Only at the end of a simulation.
    !* FREQ_NOW: In the current time-step.
    !* FREQ_SECONDS: At the end of a pre-defined increment in seconds.
    !* FREQ_MINUTES: At the end of a pre-defined increment in minutes.
    !* FREQ_HOURS: At the end of a pre-defined increment in hours.
    !* FREQ_DAYS: At the end of a pre-defined increment in days.
    !* FREQ_IC: A pre-defined 'ic' counter date, where values matched are those greater than zero.
    integer, parameter :: FREQ_NUL = 0
    integer, parameter :: FREQ_YEARLY = 1
    integer, parameter :: FREQ_MONTHLY = 2
    integer, parameter :: FREQ_SEASONAL = 3
    integer, parameter :: FREQ_DAILY = 4
    integer, parameter :: FREQ_HOURLY = 5
    integer, parameter :: FREQ_PTS = 6
    integer, parameter :: FREQ_START = 7
    integer, parameter :: FREQ_END = 8
    integer, parameter :: FREQ_NOW = 9
    integer, parameter :: FREQ_SECONDS = 10
    integer, parameter :: FREQ_MINUTES = 11
    integer, parameter :: FREQ_HOURS = 12
    integer, parameter :: FREQ_DAYS = 13
    integer, parameter :: FREQ_IC = 14

    !> Constants for 2-D/planar horizontal dimensions (group 1).
    !* DIM_NAME_LON: Short form geo-referenced longitude.
    !* DIM_NAME_LONGITUDE: Geo-referenced longitude.
    !* DIM_NAME_DEGLON: Short form geo-referenced longitude in decimal degrees.
    !* DIM_NAME_RLON: Short form geo-referenced rotated longitude.
    !* DIM_NAME_X: Cartesian 'x' (horizontal).
    !* DIM_NAMES_OF_X: List of all dimension names to describe the 2-D/planar horizontal dimension.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_LON = 'LON'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_LONGITUDE = 'LONGITUDE'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_DEGLON = 'DEGLON'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_RLON = 'RLON'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_X = 'X'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_X(5) = (/ &
        DIM_NAME_LON, DIM_NAME_LONGITUDE, DIM_NAME_DEGLON, DIM_NAME_RLON, DIM_NAME_X/)

    !> Constants for 2-D/planar vertical dimensions (group 2).
    !* DIM_NAME_LAT: Short form geo-referenced latitude.
    !* DIM_NAME_LATITUDE: Geo-referenced latitude.
    !* DIM_NAME_DEGLAT: Short form geo-referenced latitude in decimal degrees.
    !* DIM_NAME_RLAT: Short form geo-referenced rotated latitude.
    !* DIM_NAME_Y: Cartesian 'y' (vertical).
    !* DIM_NAMES_OF_Y: List of all dimension names to describe the 2-D/planar vertical dimension.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_LAT = 'LAT'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_LATITUDE = 'LATITUDE'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_DEGLAT = 'DEGLAT'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_RLAT = 'RLAT'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_Y = 'Y'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_Y(5) = (/ &
        DIM_NAME_LAT, DIM_NAME_LATITUDE, DIM_NAME_DEGLAT, DIM_NAME_RLAT, DIM_NAME_Y/)

    !> Constants for time dimensions (gruop 3).
    !* DIM_NAME_TIME: Time.
    !* DIM_NAME_T: 't'.
    !* DIM_NAMES_OF_T: List of all dimension names to describe the time dimension.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_TIME = 'TIME'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_T = 'T'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_T(2) = (/DIM_NAME_TIME, DIM_NAME_T/)

    !> Constants for GRU/land cover dimensions (group 4).
    !* DIM_NAME_GRU: Grouped response unit (GRU).
    !* DIM_NAME_NGRU: Number of grouped response units (GRUs).
    !* DIM_NAME_CLASSCOUNT: WATFLOOD-style land cover count (ClassCount).
    !* DIM_NAME_LANDCOVER: Land cover/type.
    !* DIM_NAME_NLANDCOVER: Number of land covers/types.
    !* DIM_NAME_M: 'm'.
    !* DIM_NAMES_OF_M: List of all dimension names to describe the GRU/land cover dimension.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_GRU = 'GRU'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NGRU = 'NGRU'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_CLASSCOUNT = 'CLASSCOUNT'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_LANDCOVER = 'LANDCOVER'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NLANDCOVER = 'NLANDCOVER'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_M = 'M'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_M(6) = (/ &
        DIM_NAME_GRU, DIM_NAME_NGRU, DIM_NAME_CLASSCOUNT, DIM_NAME_LANDCOVER, DIM_NAME_NLANDCOVER, DIM_NAME_M/)

    !> Constants for river class dimensions (group 5).
    !* DIM_NAME_RVR: Short form river class (RVR).
    !* DIM_NAME_NRVR: Count of river classes.
    !* DIM_NAME_IAK: WATFLOOD-style river class (IAK).
    !* DIM_NAME_RIVERCLASS: River class.
    !* DIM_NAME_NRIVERCLASS: Number of river classes.
    !* DIM_NAME_K: 'k'.
    !* DIM_NAMES_OF_K: List of all dimension names to describe the river class dimension.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_RVR = 'RVR'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NRVR = 'NRVR'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_IAK = 'IAK'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NUMRIVERCLASSES = 'NUMRIVERCLASSES'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_RIVERCLASS = 'RIVERCLASS'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NRIVERCLASS = 'NRIVERCLASS'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_K = 'K'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_K(7) = (/ &
        DIM_NAME_RVR, DIM_NAME_NRVR, DIM_NAME_IAK, DIM_NAME_NUMRIVERCLASSES, DIM_NAME_RIVERCLASS, DIM_NAME_NRIVERCLASS, &
        DIM_NAME_K/)

    !> Constants for subbasin/cell or grid dimensions (group 6).
    !* DIM_NAME_SUBBASIN: Subbasin.
    !* DIM_NAME_NSUBBASIN: Number of subbasins.
    !* DIM_NAME_CELL: Subbasin or grid cell.
    !* DIM_NAME_NCELL: Number of subbasin or grid cells.
    !* DIM_NAME_RANK: WATFLOOD-style grid order (Rank).
    !* DIM_NAME_N: 'n'.
    !* DIM_NAMES_OF_N: List of all dimension names to describe the subbasin/cell or grid dimension.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_SUBBASIN = 'SUBBASIN'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NSUBBASIN = 'NSUBBASIN'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_CELL = 'CELL'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NCELL = 'NCELL'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_RANK = 'RANK'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_N = 'N'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_N(6) = (/ &
        DIM_NAME_SUBBASIN, DIM_NAME_NSUBBASIN, DIM_NAME_CELL, DIM_NAME_NCELL, DIM_NAME_RANK, DIM_NAME_N/)

    !> Constants for spatially-uniform/basin-wide dimensions (group 7).
    !* DIM_NAME_BASIN: Spatially-uniform basin.
    !* DIM_NAME_UNIFORM: Spatially-uniform.
    !* DIM_NAME_B: 'b'.
    !* DIM_NAMES_OF_B: List of all dimension names to describe the spatially-uniform/basin-wide dimension.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_BASIN = 'BASIN'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_UNIFORM = 'UNIFORM'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_B = 'B'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_B(3) = (/DIM_NAME_BASIN, DIM_NAME_UNIFORM, DIM_NAME_B/)

    !> Constants for tile dimensions (group 8).
    !* DIM_NAME_LANDTILE: Land tile.
    !* DIM_NAME_NLANDTILE: Number of land tiles.
    !* DIM_NAME_NML: CLASS-style number of land-based tiles (NML).
    !* DIM_NAME_G: 'g'.
    !* DIM_NAMES_OF_G: List of all dimension names to describe tile dimensions.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_LANDTILE = 'LANDTILE'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NLANDTILE = 'NLANDTILE'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NML = 'NML'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_G = 'G'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_G(4) = (/ &
        DIM_NAME_LANDTILE, DIM_NAME_NLANDTILE, DIM_NAME_NML, DIM_NAME_G/)

    !> Constants for level/layer or sub-level dimensions (group 9).
    !* DIM_NAME_LEVEL: Level.
    !* DIM_NAME_LAYER: Layer.
    !* DIM_NAME_Z: Cartesian 'z' (depth).
    !* DIM_NAME_SOIL: Soil layer.
    !* DIM_NAME_NSOIL: Number of soil layers.
    !* DIM_NAME_SOL: Short form soil layer.
    !* DIM_NAME_NSOL: Number of soil layers (short form).
    !* DIM_NAME_NSL: Number of soil layers (NSL).
    !* DIM_NAME_IGND: CLASS-style number of soil layers (IGND).
    !* DIM_NAME_SURFACE: Surface type.
    !* DIM_NAME_NSURFACE: Number of surface types.
    !* DIM_NAME_SURF: Short form surface type.
    !* DIM_NAME_NSURF: Number of surface types.
    !* DIM_NAME_SUBTILETYPES: Sub-tile surface type.
    !* DIM_NAME_NSUBTILETYPES: Number of sub-tile surface types.
    !* DIM_NAME_SUBTYPE: Short form sub-tile surface type.
    !* DIM_NAME_NSUBTYPE: Number of sub-tile surface types (short form).
    !* DIM_NAME_SUBTYPE_TYPES: Number of surface types (legacy NetCDF-format resume file).
    !* DIM_NAME_CANOPY: Canopy.
    !* DIM_NAME_NCANOPY: Number of canopies.
    !* DIM_NAME_CAN: Short form canopy.
    !* DIM_NAME_NCAN: Number of canopies.
    !* DIM_NAME_ICAN: CLASS-style number of canopies excluding bare-ground and urban areas.
    !* DIM_NAME_ICP1: CLASS-style number of canopies excluding urban areas.
    !* DIM_NAME_VF: SVS-style short form for vegetation type or ID.
    !* DIM_NAME_NVF: Number of vegetation types of IDs (SVS-style short form).
    !* DIM_NAME_L: 'l'.
    !* DIM_NAMES_OF_L: List of all dimension names to describe level/layer or sub-level dimensions.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_LEVEL = 'LEVEL'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_LAYER = 'LAYER'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_Z = 'Z'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_SOIL = 'SOIL'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NSOIL = 'NSOIL'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_SOL = 'SOL'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NSOL = 'NSOL'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NSL = 'NSL'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_IGND = 'IGND'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_SURFACE = 'SURFACE'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NSURFACE = 'NSURFACE'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_SURF = 'SURF'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NSURF = 'NSURF'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_SUBTILETYPES = 'SUBTILETYPES'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NSUBTILETYPES = 'NSUBTILETYPES'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_SUBTYPE = 'SUBTYPE'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NSUBTYPE = 'NSUBTYPE'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_SUBTYPE_TYPES = 'SUBTILE_TYPES'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_CANOPY = 'CANOPY'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NCANOPY = 'NCANOPY'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_CAN = 'CAN'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NCAN = 'NCAN'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_ICAN = 'ICAN'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_ICP1 = 'ICP1'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_VF = 'VF'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NVF = 'NVF'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_L = 'L'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_L(27) = (/ &
        DIM_NAME_LEVEL, DIM_NAME_LAYER, DIM_NAME_Z, &
        DIM_NAME_SOIL, DIM_NAME_NSOIL, DIM_NAME_SOL, DIM_NAME_NSOL, DIM_NAME_NSL, DIM_NAME_IGND, &
        DIM_NAME_SURFACE, DIM_NAME_NSURFACE, DIM_NAME_SURF, DIM_NAME_NSURF, &
        DIM_NAME_SUBTILETYPES, DIM_NAME_NSUBTILETYPES, DIM_NAME_SUBTYPE, DIM_NAME_NSUBTYPE, DIM_NAME_SUBTYPE_TYPES, &
        DIM_NAME_CANOPY, DIM_NAME_NCANOPY, DIM_NAME_CAN, DIM_NAME_NCAN, DIM_NAME_ICAN, DIM_NAME_ICP1, &
        DIM_NAME_VF, DIM_NAME_NVF, &
        DIM_NAME_L/)
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_SOL(6) = (/ &
        DIM_NAME_SOIL, DIM_NAME_NSOIL, DIM_NAME_SOL, DIM_NAME_NSOL, DIM_NAME_NSL, DIM_NAME_IGND/)
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_SURF(4) = (/ &
        DIM_NAME_SURFACE, DIM_NAME_NSURFACE, DIM_NAME_SURF, DIM_NAME_NSURF/)
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_SUBTYPE(5) = (/ &
        DIM_NAME_SUBTILETYPES, DIM_NAME_NSUBTILETYPES, DIM_NAME_SUBTYPE, DIM_NAME_NSUBTYPE, DIM_NAME_SUBTYPE_TYPES/)
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_CANOPY(6) = (/ &
        DIM_NAME_CANOPY, DIM_NAME_NCANOPY, DIM_NAME_CAN, DIM_NAME_NCAN, DIM_NAME_ICAN, DIM_NAME_ICP1/)
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_VF(2) = (/DIM_NAME_VF, DIM_NAME_NVF/)

    !> Constants for generic and groups of dimension names.
    !* DIM_NAME_NUL: No dimension defined.
    !* DIM_NAMES_OF_ORDERS: List of generic DIM_NAME for all registered orders (matching 'MAP_ORDER').
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAME_NUL = ''
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_OF_ORDERS(9) = (/ &
        DIM_NAME_X, DIM_NAME_Y, DIM_NAME_T, DIM_NAME_M, DIM_NAME_K, DIM_NAME_N, DIM_NAME_B, DIM_NAME_G, DIM_NAME_L/)
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_SPATIAL_2D(2) = (/DIM_NAME_X, DIM_NAME_Y/)
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_NAMES_SPATIAL_1D(5) = (/ &
        DIM_NAME_M, DIM_NAME_K, DIM_NAME_N, DIM_NAME_B, DIM_NAME_G/)

    !> Constants for units.
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_UNITS_DEGREES = 'degrees'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_UNITS_DECIMAL_DEGREES = 'decimal_degrees'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_UNITS_DEGREES_EAST = 'degrees_east'
    character(len = SHORT_FIELD_LENGTH), parameter :: DIM_UNITS_DEGREES_NORTH = 'degrees_north'

    !> Constants for MESH variable dimension order (derived).
    !* MAP_ORDER_X: 2-D/planar horizontal dimension.
    !* MAP_ORDER_Y: 2-D/planar vertical dimension.
    !* MAP_ORDER_T: Time dimension.
    !* MAP_ORDER_M: GRU/land cover dimension.
    !* MAP_ORDER_K: River class dimension.
    !* MAP_ORDER_N: Subbasin/cell or grid dimension.
    !* MAP_ORDER_B: Spatially-uniform/basin-wide dimension.
    !* MAP_ORDER_G: Tile dimension.
    !* MAP_ORDER_L: Level/layer or sub-level dimension.
    integer, parameter :: MAP_ORDER_X = 1
    integer, parameter :: MAP_ORDER_Y = 2
    integer, parameter :: MAP_ORDER_T = 3
    integer, parameter :: MAP_ORDER_M = 4
    integer, parameter :: MAP_ORDER_K = 5
    integer, parameter :: MAP_ORDER_N = 6
    integer, parameter :: MAP_ORDER_B = 7
    integer, parameter :: MAP_ORDER_G = 8
    integer, parameter :: MAP_ORDER_L = 9

    !> Constants for 'NODATA' values.
    !* NO_DATA_REAL: No data value for type of real (float).
    !* NO_DATA_INT: No data value for type of integer (int).
    !* NO_DATA_CHAR: No data value for type of character.
    real, parameter :: NO_DATA_REAL = huge(0.0)
    integer, parameter :: NO_DATA_INT = huge(0)
    character(len = 1), parameter :: NO_DATA_CHAR = achar(0)

end module
