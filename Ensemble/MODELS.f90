!-------------------------------------------------------------------------------
! Model numbers
!-------------------------------------------------------------------------------
module MODELS

integer, parameter :: &
 Nmod = 4096      ! Number of models

integer :: &
  tem(Nmod),      &! turbulent exchange model
  zmsm(Nmod),     &! momentum-scalar ratio model
  bsm(Nmod),      &! blowing snow model
  scfm(Nmod),     &! snow cover fraction model
  fsdm(Nmod),     &! fresh snow density model
  scm(Nmod),      &! snow compaction model
  sam(Nmod),      &! snow albedo decay model
  sntcm(Nmod),    &! snow thermal conductivity model
  slwm(Nmod),     &! snow liquid water content model
  im(Nmod),       &! infiltration model
  sotcm(Nmod),    &! soil thermal conductivity model
  ufcm(Nmod),     &! soil unfrozen water content model
  siim(Nmod)       ! soil ice impedance model

end module MODELS
