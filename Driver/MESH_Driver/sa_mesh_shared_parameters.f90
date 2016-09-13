!>
!> Module for storing parameters shared throughout the driver.
!>
module sa_mesh_shared_parameters

    use input_parameters

    implicit none

    !> Type: parameters
    !>
    !> Description:
    !>  Collection of input parameters.
    type parameters
        type(tile_parameters) :: tp
        type(canopy_parameters) :: cp
        type(surface_parameters) :: sfp
        type(snow_parameters) :: snp
        type(soil_parameters) :: slp
        type(hydraulic_parameters) :: hp
    end type

    !* pm: Collection of input parameters.
    type(parameters), save :: pm

end module
