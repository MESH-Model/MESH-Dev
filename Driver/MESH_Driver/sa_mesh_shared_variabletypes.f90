!>
!> Module for storing common variable types used throughout the driver.
!>
module sa_mesh_shared_variabletypes

    implicit none

    !> *****************************************************************
    !* basin_info: Data type to store basin information.
    !> *****************************************************************
    type basin_info

        !* NA: Total number of grids. [-]
        !* NAA: Total number of grids in the basin. [-]
        integer :: &
            NA = 0, &
            NAA = 0

        !* NM: Number of GRUs. [-]
        integer :: NTYPE = 0

        !* NRVR: Number of river classes. [-]
        integer :: NRVR = 0

        !* IGND: Number of soil layers per grid. [-]
        integer :: IGND = 3

        !* ILG: Total potential number of land-based contributing units.
        integer :: ILG = 0

        !* NML: Number of active land-based contributing units.
        integer :: NML = 0

        real, dimension(:, :), allocatable :: ACLASS
        integer, dimension(:), allocatable :: ILMOS, JLMOS, IWMOS, JWMOS, RANK, NEXT

        !* NMW: Number of active water-based contributing units.
        integer :: NMW = 0
        integer :: ILW = 0

        character(10) CoordSys, Datum, Zone

        integer, dimension(:), allocatable :: xxx, yyy

        real :: AL = 0.0

        real :: &
            GRDN = 0.0, &
            GRDE = 0.0

        real :: &
            xOrigin = 0.0, &
            yOrigin = 0.0

        real :: &
            xDelta = 0.0, &
            yDelta = 0.0

        integer :: &
            xCount = 1, &
            yCount = 1

        integer :: &
            iyMax = 1, &
            iyMin = 1, &
            jxMax = 1, &
            jxMin = 1

        !* ELEV: Grid-cell elevation [m]
        !* SLOPE: Internal slope (may exist in other files as 'intslope' or 'sl1')
        !* AREA: Grid-cell area [m^2]
        !* DA: Drainage area [m^2]
        !* DRDN: Drainage density [m m^-2]
        !* FRAC:
        !* SLOPE_CHNL: Channel slope
        !* CHNL_LEN: Channel length (may exist in other files as 'chnllength', 'rl', or 'ch_length')
        !* BNKFLL:
        real, dimension(:), allocatable :: &
            ELEV, SLOPE, AREA, DA, DRDN, FRAC, SLOPE_CHNL, CHNL_LEN, BNKFLL

        integer, dimension(:), allocatable :: IAK, IROUGH, ICHNL, IREACH

    end type !basin_info

    !> *****************************************************************
    !* run_options: Data type to store run options for the driver.
    !> *****************************************************************
    type run_options

        !* VERBOSEMODE: Flag to suppress certain model output from the
        !*              console.
        !> VERBOSEMODE = 0 to disable certain output to the console.
        integer :: VERBOSEMODE = 1

        !* DIAGNOSEMODE: Flag to allow code to write extra diagnostic
        !*  output.
        !>  DIAGNOSEMODE = 1: to write extra output to the console.
        !>  DIAGNOSEMODE = 0: to disable.
        integer :: DIAGNOSEMODE = 0

    end type !run_options

end module
