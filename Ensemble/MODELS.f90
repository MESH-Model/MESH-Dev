!-------------------------------------------------------------------------------
! Model numbers
!-------------------------------------------------------------------------------
module MODELS

integer*8, parameter :: &
 Nmod = 16384      ! Number of models

integer :: &
  ebalm(Nmod),    &! energy balance solution model
  tem(Nmod),      &! turbulent exchange model
  zmsm(Nmod),     &! momentum-scalar ratio model
  bsm(Nmod),      &! blowing snow model
  scfm(Nmod),     &! snow cover fraction model
  fsdm(Nmod),     &! fresh snow density model
  scm(Nmod),      &! snow compaction model
  sam(Nmod),      &! snow albedo decay model
  sntcm(Nmod),    &! snow thermal conductivity model
  betam(Nmod),    &! soil moisture beta model
  im(Nmod),       &! infiltration model
  sotcm(Nmod),    &! soil thermal conductivity model
  albslm(Nmod),   &! soil unfrozen water content model
  siim(Nmod),     &! soil ice impedance model
  lam(Nmod)        ! local advection model

end module MODELS
