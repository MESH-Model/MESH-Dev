!>
!> Module for storing output variables shared throughout the driver.
!>
!> Output variables may store parameters and state variables in the alternate 'ROW' and 'GRD' formats.
!>
module sa_mesh_shared_output_variables

    use sa_mesh_shared_parameters

    implicit none

    !* pmrow: Input parameters stored in 'ROW' format.
    type(parameters), save :: pmrow

end module
