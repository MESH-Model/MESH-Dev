!>
!> Module for storing common variable types used throughout the driver.
!>
module sa_mesh_shared_variabletypes

    implicit none

    !> *****************************************************************
    !* basin_info: Data type to store basin information.
    !> *****************************************************************
    type basin_info

        !* NA: Number of grid cells in the basin. [-]
        integer :: NA = 1

        !* NM: Number of GRUs. [-]
        integer :: NTYPE = 1

        !* IGND: Number of soil layers per grid. [-]
        integer :: IGND = 3

        !* ILG: Total potential number of land-based contributing units.
        integer :: ILG = 0

        !* NML: Number of active land-based contributing units.
        integer :: NML = 0

        real, dimension(:, :), allocatable :: ACLASS
        integer, dimension(:), allocatable :: ILMOS, JLMOS

        !* NMW: Number of active water-based contributing units.
        integer :: NMW = 0

        integer, dimension(:), allocatable :: XXX, YYY

        real AL
        real xorigin, yorigin, xdelta, ydelta
        integer xcount, ycount

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
