!>
!> Module for storing variables shared throughout the driver.
!>
module sa_mesh_shared_variables

    use control_variables
    use shd_variables
    use fm_variables
    use state_variables

    implicit none

    type(run_options), save :: ro
    type(control_options), save :: cops

    !> Type: forms
    !>
    !> Description:
    !>  Contains structures or 'forms' in the model, such
    !>  as landmark locations like streamflow gauge, irrigation demand,
    !>  lake, and reservoir locations.
    type forms
        type(streamflow_gauge) stmg
        type(lake_outlet) lk
        type(reservoir_outlet) rsvr
        type(abstraction_point) absp
    end type

    !* fms: Collection of structures in the basin.
    type(forms), save :: fms

    !> Type: states
    !>
    !> Description:
    !>  Contains variable types for states of variables in the model,
    !>  such as components of the water and energy balances, streamflow
    !>  channels, and reservoirs.
    type states
        type(river_flow) :: chnl
        type(lake_flow) :: lk, rsvr
        type(canopy) :: cnpy
        type(snow_balance) :: sno
        type(surface_interface) :: sfc
        type(soil_layer) :: sl
        type(deep_zone) :: lzs, dzs
    end type

    !* stas: Collection of the state of variables in the current time-step.
    type(states), save :: stas

end module
