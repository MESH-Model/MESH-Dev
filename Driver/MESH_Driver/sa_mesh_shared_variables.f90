!>
!> Module for storing variables shared throughout the driver.
!>
module sa_mesh_shared_variables

    use sa_mesh_shared_variabletypes

    implicit none

    type(run_options), save :: ro
    type(control_options), save :: cops

end module
