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
        integer :: NM = 1

        !* IGND: Number of soil layers per grid. [-]
        integer :: IGND = 3

    end type !basin_info

    !> *****************************************************************
    !* run_options: Data type to store run options for the driver.
    !> *****************************************************************
    type run_options

        !* VERBOSEMODE: Flag to suppress certain model output from the
        !*              console.
        !> VERBOSEMODE = 0 to disable certain output to the console.
        integer :: VERBOSEMODE = 1

    end type !run_options

end module
