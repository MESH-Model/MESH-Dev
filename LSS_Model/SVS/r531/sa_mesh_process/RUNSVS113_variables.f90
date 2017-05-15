module RUNSVS113_variables

    implicit none

!    integer, parameter :: fid_ini = 50
!    integer, parameter :: fid_met = 51
!    integer, parameter :: fid_out = 52

!    integer nt      ! Number of time steps
!    integer dateo   ! Starting date of the run
!    integer houro   ! Starting hour of the run
!    real :: dt = 1800.0         ! Time step duration (sec)
!    real :: sigma_u = 0.995    ! Sigma level of momentum forcing
!    real :: sigma_t = 0.995    ! Sigma level of scalar forcings
!    integer xcount  ! Number of columns in the grid
!    integer ycount  ! Number of lines in the grid
!    logical :: observed_forcing = .false.
!    character(len = 255) :: inifile = ''
!    character(len = 255) :: interpfile = ''
!    character(len = 255) :: geofile = ''
!    character(len = 255) :: metfile = ''
!    character(len = 255) :: outfile = ''
!    character(len = 255) :: rtefile = ''

!    integer, parameter :: bussiz = runsvs_busdim
!    real bus(bussiz)
    integer bussiz
    real, dimension(:), allocatable :: bus
    integer datecmc_o, date_f, hour_f
!    integer datecmc_v, date_v, hour_v, istat, bidon
!    integer kount
!    real(kind = 8) kdt

    !> Type: SVS_Flags
    !* PROCESS_ACTIVE: Variable to enable SVS.
    type RUNSVS113_flags
        logical :: PROCESS_ACTIVE = .false.
    end type

    type(RUNSVS113_flags), save :: RUNSVS113_flgs

end module
