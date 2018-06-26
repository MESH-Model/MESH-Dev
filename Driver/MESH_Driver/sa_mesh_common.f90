!>
!> Common modules used throughout SA_MESH.
!>
module sa_mesh_common

  !> Parameters and variables.
  use control_variables, only:                 &
       run_options, ro
  use shd_variables,     only:                 &
       CoordSysParams, ContribElemsParams,     &
       SoilLayer, LandElemsParams, GridParams, &
       LandGridParams, ShedGridParams,         &       
       SHDFILEFLAG, SHDFILEFMT, SHDTOMAPFLAG
  use fm_variables,      only:                 &
       fm_config_file,                         &
       outlet_location,                        &
       time_series,                            &
       release_outlet,                         &
       streamflow_gauge_location,              &
       reservoir_outlet_location,              &
       abstraction_point_location,             &
       forms, fms,                             &
       allocate_outlet_location,               &
       allocate_time_series,                   &
       allocate_release_outlet,                &
       allocate_streamflow_gauge_location,     &
       allocate_reservoir_outlet_location,     &
       allocate_abstraction_point_location
  use input_parameters
  use model_variables
  use output_variables

  !> Utilities.
  use print_routines  

  implicit none

end module sa_mesh_common
