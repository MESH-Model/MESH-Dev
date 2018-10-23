module land_force_perturb

    use forcepert_types
    use random_fields_nr_ran2
    use random_fields2


    implicit none

    ! --------------------------------------------------------------------

    type :: grid_def_type
         integer          :: N_x        ! number of longitudinal nodes
         integer          :: N_y        ! number of latitudinal nodes
         real             :: llx        ! lower left latitude in deg
         real             :: lly        ! lower left longitude in deg
         real             :: dx         ! longitude spacing in deg
         real             :: dy         ! latitude spacing in deg
    end type grid_def_type

  ! **********************************************************************


    contains

    subroutine get_forcepert(                                &
       N_forcepert, N_ens, N_x, N_y,                       &
       dx, dy, dtstep,                                     &
       initialize,                                         &
       forcepert_param,                                    &
       ens_id,                                             &
       Forcepert_rseed,                                    &
       Forcepert_ntrmdt,                                   &
       Forcepert                )

    ! get forcing perturbations
    !
    ! reichle, 11 Feb 2005

    implicit none

    ! N_forcepert is the number independent forcing perturbations
    ! (e.g. generate N_forcepert=4 perturbations for precip,
    ! shortwave radiation, and longwave radiation that will then be
    ! applied to various precip fields (incl large-scale & convective precip
    ! and snow) and to radiation fields accordingly. I have added the other perturbation
    ! field in order to perturb SWE.

    integer, intent(in) :: N_forcepert   ! # different forcing perturb's

    integer, intent(in) :: N_ens  ! # ensemble members

    integer, intent(in) :: N_x    ! # grid cells in longitudinal direction
    integer, intent(in) :: N_y    ! # grid cells in latitudinal direction

    real, intent(in) :: dx   ! grid cell size in longitudinal direction [deg]
    real, intent(in) :: dy   ! grid cell size in latitudinal direction [deg]

    real, intent(in) :: dtstep        ! land model time step in seconds

    ! If initialize==.true., set initial random seed and generate
    ! initial Forcepert_ntrmdt (must be allocated!!!)
    ! If initialize==.false., the random seed and Forcepert_ntrmdt must
    ! be saved from the last time step (via restart file if model has been
    ! shut down).

    logical, intent(in) :: initialize

    ! Identify ensemble members via ensemble ID
    ! (necessary for random seed identification)

    integer, dimension(N_ens), intent(in) :: ens_id

    ! Parameter structure for forcing perturbations (see type definition
    ! for details).

    type(forcepert_param_type), dimension(:), allocatable :: forcepert_param

    ! Forcepert_ntrmdt are intermediate perturbation fields
    ! that need to be remembered between calls to this subroutine.
    ! In essence, they store N_forcepert mutually uncorrelated
    ! perturbation fields of standard-normal distribution.
    ! Forcepert_rseed is the random seed for the generation of
    ! Forcepert_ntrmdt and is treated similarly to a prognostic variable.
    ! Each ensemble member has its own random seed.

    integer, dimension(NRANDSEED,N_ens), intent(inout) :: Forcepert_rseed

    real, dimension(N_forcepert,N_x,N_y,N_ens), intent(inout) :: &
         Forcepert_ntrmdt

    ! Forcepert are N_forcepert cross-correlated perturbation
    ! fields that are rotated and scaled versions of Forcepert_ntrmdt
    ! so that Forcepert has the the mean values, standard deviations and
    ! cross-correlations specified in forcepert_param.
    ! The distribution is lognormal for multiplicative perturbations.
    ! Forcepert should be used as follows for field F (eg. large-scale
    ! precip, convective precip, lw radiation, ...)
    !
    ! F = F+Forcepert   for additive perturbations
    ! F = F*Forcepert   for multiplicative perturbations
    !
    ! Note that this subroutine does NOT ensure physically meaningful
    ! perturbed forcing fields.  This is best done outside this subroutine
    ! after the perturbations have been applied.

    real, dimension(N_forcepert,N_x,N_y,N_ens), intent(out) :: Forcepert

    ! --------------------------------------------------
    !
    ! local parameter values for initial random seed
    ! (one random seed value will be propagated for each ensemble member)
    !
    integer, parameter :: RSEED_CONST1 = -777    ! must be negative
    integer, parameter :: RSEED_CONST2 =  100    ! must be positive

    ! --------------------------------------------------
    !
    ! local variables

    type(grid_def_type) :: loc_grid

    integer :: i, j, k, m, mm, n

    real, dimension(N_x,N_y)  :: tmp_grid

    real :: tmpreal

    ! --------------------------------------------------------------------

    ! assemble structure for grid info

    loc_grid%N_x = N_x
    loc_grid%N_y = N_y
    loc_grid%llx = -9999.             ! lower left corner not needed
    loc_grid%lly = -9999.             ! lower left corner not needed
    loc_grid%dx  = dx
    loc_grid%dy  = dy

    ! ------------------------------------------------------------------
    !
    ! initialize random seed if necessary (one for each ensemble member,
    ! based on ID of ensemble member)

    if (initialize) then

       do n = 1 , N_ens

          Forcepert_rseed(1,n) = RSEED_CONST1 - ens_id(n) * RSEED_CONST2

          call init_randseed(Forcepert_rseed(:,n))

       end do

    end if

    ! ------------------------------------------------------------------
    !
    ! Forcepert_ntrmdt are standard-normal with desired
    ! temporal and spatial correlation structure.
    ! Cross-correlations between different fields and scaling to desired
    ! mean and variance is NOT included in Forcepert_ntrmdt.
    !
    ! Propagate forcing perturbation fields:
    ! on input, Forcepert_ntrmdt must contain fields from
    ! last time step
    ! (If initialize=.true. Forcepert_ntrmdt is initialized to a
    ! standard-normal field with the desired spatial correlation structure)
    !
    ! Forcepert_ntrmdt are perturbation of forcing data by considering the
    ! temporal correlation. In this stage only the evolution of desired field
    ! is considered. The spatial and cross correlation will be applied later.

    call propagate_forcepert(                             &
         N_forcepert, N_ens, loc_grid, dtstep,            &
         Forcepert_rseed,                                 &
         initialize,                                      &
         forcepert_param,                                 &
         Forcepert_ntrmdt )

    if (.not. initialize) then

       ! ensure that ensemble mean forcing perturbatino is zero
       ! (must have N_ens>2 for this to make sense).
       !
       ! NOTE: since the sample mean model error varies spatially,
       !       this adjustment slightly changes the *spatial* mean and
       !       covariance of the model error fields
       !       likely, the benefits of the adjustments for small
       !       ensemble sizes outweigh the disadvantages of altering
       !       the statistical properties

       do m = 1 , N_forcepert

          if ( (forcepert_param(m)%zeromean) .and. (N_ens > 2)) then

             do i = 1 , loc_grid%N_x

                call adjust_mean(loc_grid%N_y, N_ens, &
                     Forcepert_ntrmdt(m, i , : , :) )

             end do

          end if

       end do

       ! compute rotated fields to get desired cross-correlations between
       ! different fields, then scale to desired mean and std

       do m = 1 , N_forcepert

          do n = 1 , N_ens

             ! rotate to get desired multivariate correlations

             do i = 1 , loc_grid%N_x
                do j = 1 , loc_grid%N_y

                   tmp_grid(i,j) = 0.

                   do mm = 1 , N_forcepert

                      tmp_grid(i,j) = tmp_grid(i , j) + &
                           forcepert_param(m)%ccorr(mm , i , j) * &
                           Forcepert_ntrmdt(mm , i , j , n)

                   end do

                end do
             end do

             ! scale back freak outliers

             call truncate_std_normal( loc_grid%N_x, loc_grid%N_y, &
                  forcepert_param(m)%std_normal_max, tmp_grid )

             ! scale

             do i= 1 , loc_grid%N_x
                do j = 1 , loc_grid%N_y

                   tmpreal = forcepert_param(m)%mean(i,j) + &
                        forcepert_param(m)%std(i,j) * tmp_grid(i,j)

                   select case (forcepert_param(m)%typ)

                   case (0)        ! additive

                      Forcepert(m , i , j , n) = tmpreal

                   case (1)        ! multiplicative and lognormal

                      Forcepert(m , i , j, n) = exp(tmpreal)

                   case default

                      write (*,*) 'generate_forcepert(): encountered unknown'
                      write (*,*) 'typ_forcepert, stopping...'
                      stop

                   end select

                end do
             end do

          end do  ! end loop through ensemble members (1:N_ens)
       end do     ! end loop through different forcing fields (1:N_forcepert)

    end if

  end subroutine get_forcepert

  ! ************************************************************************

  subroutine propagate_forcepert(                       &
       N_forcepert, N_ens, loc_grid, dtstep,            &
       Forcepert_rseed,                                 &
       initialize,                                      &
       forcepert_param,                                 &
       Forcepert_ntrmdt )

    ! generate zero-mean, unit-variance (!!) time series
    !  of N_forcepert 2d perturbation fields
    !
    ! can also be used just to get a set of 2d random fields (set dtstep
    !  to arbitrary number and "initialize" to .true.)
    !
    ! on input, Forcepert_ntrmdt must contain the corresponding
    !  perturbations from the previous time step (or 0. if initialize==.true.)
    !
    ! accounts for temporal correlation with AR(1) approach
    !  (if forcepert_param%tcorr==0 then error is white in time)
    !
    ! adapted from off-line EnKF, subroutine propagate_err()
    !  from NCAT_59124_tskin in enkf_catchment.f90
    !
    ! reichle, 14 Feb 2005

    implicit none

    ! ---------------------------

    integer, intent(in) :: N_ens, N_forcepert

    type(grid_def_type), intent(in) :: loc_grid   ! local grid definition

    type(forcepert_param_type), dimension(N_forcepert), intent(in) :: &
         forcepert_param

    real, intent(in) :: dtstep  ! time step of generation of error fields [s]

    integer, dimension(NRANDSEED, N_ens), intent(inout) :: Forcepert_rseed

    logical, intent(in) :: initialize   ! switch

    real, dimension(N_forcepert,loc_grid%N_x,loc_grid%N_y,N_ens), &
         intent(inout) :: Forcepert_ntrmdt

    ! ---------------------------

    ! locals

    integer :: i, j, m, n

    real    :: cc, dd

    double precision, dimension(loc_grid%N_x,loc_grid%N_y) :: rfield, rfield2

    logical :: white_in_time, white_in_space, stored_field

    ! ---------------------------

    do m = 1 , N_forcepert

       ! get parameters for temporal correlation

       if ((.not. initialize) .and. (forcepert_param(m)%tcorr>0.0)) then

          white_in_time = .false.

          cc = exp( - dtstep / forcepert_param(m)%tcorr )

          dd = sqrt( 1 - cc**2 )

       else

          cc = 0.
          dd = 1.

          white_in_time = .true.

       end if

       ! find out whether there are spatial correlations

       if ( (forcepert_param(m)%xcorr>0.0) .or.            &
            (forcepert_param(m)%ycorr>0.0)        ) then

          white_in_space = .false.

       else

          white_in_space = .true.

       end if


       ! generate new random fields and propagate AR(1)
       !
       ! Note that rfg2d always produces a pair of random fields!
       !
       ! Use logical variable "stored_field" to figure out whether a second
       ! standard-normal random field is available for next ensemble member.
       ! (in other words, this subroutine is most efficient if N_ens=even
       ! number, and it is least efficient if N_ens=1)

       stored_field = .false.

       do n = 1 , N_ens

          ! generate a random field

          if (white_in_space) then

             call generate_white_field(loc_grid%N_x, loc_grid%N_y, &
                  Forcepert_rseed(:,n), rfield )

          else       ! spatially correlated random fields

             ! NOTE: rfg2d_fft() relies on CXML math library (22 Feb 05)

             if (.not. stored_field) then

                call rfg2d_fft( Forcepert_rseed(:,n), 1.,                &
                     loc_grid%N_x, loc_grid%N_y,                         &
                     loc_grid%dx, loc_grid%dy,                           &
                     forcepert_param(m)%xcorr, forcepert_param(m)%ycorr, &
                     rfield, rfield2 )

                stored_field = .true.

             else

                ! for the second ensemble
                rfield = rfield2

                stored_field = .false.

             end if

          end if


          !! -----------------------------------------------------------
          !!
          !! adjust std of fields to match exactly 1.0
          !!
          !! WARNING: before doing this should check that
          !!          N_x * dx>>xcorr .and. N_y*dy>>ycorr .and. N_x*N_y>>1
          !!
          !! Cannot use adjust_std if the field is small relative to
          !! its spatial correlation scales or if it contains only a few
          !! grid cells (even for white noise in space)
          !!
          !! reichle, 25 Jan 2005
          !!
          call adjust_std( loc_grid%N_x, loc_grid%N_y, rfield )
          !!
          !! -----------------------------------------------------------

          ! propagate AR(1)

          if (white_in_time) then

             Forcepert_ntrmdt(m,:,:,n) = real(rfield)

          else

             Forcepert_ntrmdt(m,:,:,n) = &
                  cc * Forcepert_ntrmdt(m,:,:,n) + dd * real(rfield)
          end if

       end do

    end do

  end subroutine propagate_forcepert

  ! ******************************************************************

  subroutine truncate_std_normal( N_x, N_y, std_normal_max, grid_data )

    ! truncate a realization of standard normal variables
    ! (scale back freak outliers)

    implicit none

    integer, intent(in) :: N_x, N_y

    real, intent(in) :: std_normal_max

    real, dimension(N_x,N_y), intent(inout) :: grid_data

    ! local variables

    integer :: i,j

    ! --------------------------------------------------------

    do i=1,N_x
       do j=1,N_y

          ! want: -std_normal_max < cat_data < std_normal_max

          grid_data(i,j) = &
               sign( min(abs(grid_data(i,j)),std_normal_max), grid_data(i,j) )

       end do
    end do

  end subroutine truncate_std_normal

  ! **************************************************************

  subroutine adjust_mean( N_row, N_col, A, M )

    ! adjust N_row by N_col matrix A such that
    ! mean over columns for each row is given by the
    ! corresponding element in vector M of length N_row
    !
    ! vector of mean values M is optional input, if not present
    ! zero mean is assumed

    implicit none

    integer, intent(in) :: N_row, N_col

    real, intent(inout), dimension(N_row , N_col)  :: A

    real, intent(in), optional, dimension(N_row) :: M

    ! ----------------------------

    ! locals

    integer i

    real, dimension(N_row) :: correction

    ! ------------------------------------------------------------

    if (present(M)) then
       correction = M - sum(A,2)/real(N_col)
    else
       correction = - sum(A,2)/real(N_col)
    end if

    do i= 1 , N_col
       A(:,i) = A(:,i) + correction
    end do

  end subroutine adjust_mean

  ! *************************************************************************


  subroutine adjust_std( N_row, N_col, A, std )

    ! adjust N_row by N_col matrix A such that (sample) standard deviation
    !  of all elements is exactly equal to std
    !
    ! std is optional input, if not present std=1 is assumed

    implicit none

    integer, intent(in) :: N_row, N_col

    double precision, intent(inout), dimension(N_row,N_col)  :: A

    real, intent(in), optional :: std

    ! ----------------------------

    ! locals

    integer :: i, j

    real :: correction, sample_std

    ! ------------------------------------------------------------

    ! compute sample std

    call matrix_std( N_row, N_col, A, sample_std )

    if (present(std)) then
       correction = std/sample_std
    else
       correction = 1./sample_std
    end if

    do i=1,N_row
       do j=1,N_col
          A(i,j) = correction*A(i,j)
       end do
    end do

  end subroutine adjust_std

  ! ***************************************************************************

  subroutine matrix_std( N_row, N_col, A, std )

    ! compute std of all elements of N_row by N_col matrix A

    implicit none

    integer, intent(in) :: N_row, N_col

    double precision, intent(inout), dimension(N_row,N_col)  :: A

    real, intent(out) :: std

    ! ----------------------------

    ! locals

    integer :: i, j

    real :: x2, m, N_real, N_real_minus_one

    ! ------------------------------------------------------------

    N_real = real(N_row)*real(N_col)

    N_real_minus_one = N_real - 1.

    ! compute sample std

    x2 = 0.0
    m  = 0.0

    do i=1,N_row
       do j=1,N_col
          m  = m  + A(i,j)
          x2 = x2 + A(i,j)*A(i,j)
       end do
    end do

    std = sqrt( ( x2 - m**2/N_real )/N_real_minus_one )

  end subroutine matrix_std


    subroutine assemble_forcepert_param( N_x, N_y,               &
       N_forcepert, forcepert_param )

        integer, intent(in) :: N_x, N_y

        integer, intent(out) :: N_forcepert

        type(forcepert_param_type), dimension(:), allocatable :: forcepert_param ! out

        ! local variables

        integer              :: i, j, k, l
        real                 :: tmpreal
        !real, dimension(3,3) :: tmpmat
        real, dimension(8,8) :: tmpmat

        ! -----------------------------------------------------------------

        ! forcing perturbation parameters

        ! # of forcing variables that are perturbed
        ! (currently pcp, sw, lw,sno)
        ! 11/08/2017:
        ! I have added the model states perturbation fields.
        ! In order to test how this perturbation field work,
        ! I have considered SWE and THLQ of four layers

        integer, parameter :: N_force   = 8   ! 3

        integer, parameter :: ind_pcp   = 1
        integer, parameter :: ind_sw    = 2
        integer, parameter :: ind_lw    = 3
        integer, parameter :: ind_swe   = 4
        integer, parameter :: ind_thlq1 = 5
        integer, parameter :: ind_thlq2 = 6
        integer, parameter :: ind_thlq3 = 7
        integer, parameter :: ind_thlq4 = 8




        character(40), parameter :: descr_pcp = 'pcp'
        character(40), parameter :: descr_sw  = 'sw'
        character(40), parameter :: descr_lw  = 'lw'
        character(40), parameter :: descr_swe   = 'swe'
        character(40), parameter :: descr_thlq1 = 'thlq1'
        character(40), parameter :: descr_thlq2 = 'thlq2'
        character(40), parameter :: descr_thlq3 = 'thlq3'
        character(40), parameter :: descr_thlq4 = 'thlq4'


        ! limit on range of random numbers:
        !  specify max absolute value allowed to be drawn from a standard
        !  normal distribution

        real, parameter :: std_normal_max =  2.5

        ! decide whether to ensure zero mean for synthetically generated errors
        ! (IMPORTANT: this will only have an effect for N_ens>2!!!)

        logical, parameter :: zeromean = .true.

        ! temporal correlation scale

        real, parameter :: tcorr = 86400               ! [s]

        ! horizontal correlation scales

        real, parameter :: xcorr = 176499.2
        real, parameter :: ycorr = 176499.2

        ! perturbations are either
        !
        !   typ=0: additive, mean=0
        !   typ=1: multiplicative and lognormal, mean=1

        integer, parameter :: typ_pcp    = 1
        real,    parameter :: std_pcp    = .5 ! .3, 0.5

        integer, parameter :: typ_sw     = 1
        real,    parameter :: std_sw     = .2 ! .2

        integer, parameter :: typ_lw     = 0
        real,    parameter :: std_lw     = 30 !40 ! .4 30

        integer, parameter :: typ_swe    = 1
        real,    parameter :: std_swe    = 0.0004

        integer, parameter :: typ_thlq1  = 0
        !real,    parameter :: std_thlq1  = 0.001
        real,    parameter :: std_thlq1  = 0.0001

        integer, parameter :: typ_thlq2  = 0
        !real,    parameter :: std_thlq2  = 0.0005
        real,    parameter :: std_thlq2  = 0.0001

        integer, parameter :: typ_thlq3  = 0
        real,    parameter :: std_thlq3  = 0.00005

        integer, parameter :: typ_thlq4  = 0
        real,    parameter :: std_thlq4  = 0.00005


        ! correlation coefficients -1 <= rho <= 1

        real, parameter :: rho_pcp_sw       = -.8
        real, parameter :: rho_pcp_lw       =  .5
        real, parameter :: rho_pcp_swe      =   0
        real, parameter :: rho_pcp_thlq1    =   0
        real, parameter :: rho_pcp_thlq2    =   0
        real, parameter :: rho_pcp_thlq3    =   0
        real, parameter :: rho_pcp_thlq4    =   0
        real, parameter :: rho_sw_lw        = -.5
        real, parameter :: rho_sw_swe       =   0
        real, parameter :: rho_sw_thlq1     =   0
        real, parameter :: rho_sw_thlq2     =   0
        real, parameter :: rho_sw_thlq3     =   0
        real, parameter :: rho_sw_thlq4     =   0
        real, parameter :: rho_lw_swe       =   0
        real, parameter :: rho_lw_thlq1     =   0
        real, parameter :: rho_lw_thlq2     =   0
        real, parameter :: rho_lw_thlq3     =   0
        real, parameter :: rho_lw_thlq4     =   0
        real, parameter :: rho_swe_thlq1    =   0
        real, parameter :: rho_swe_thlq2    =   0
        real, parameter :: rho_swe_thlq3    =   0
        real, parameter :: rho_swe_thlq4    =   0
        real, parameter :: rho_thlq1_thlq2  =   0
        real, parameter :: rho_thlq1_thlq3  =   0
        real, parameter :: rho_thlq1_thlq4  =   0
        real, parameter :: rho_thlq2_thlq3  =   0
        real, parameter :: rho_thlq2_thlq4  =   0
        real, parameter :: rho_thlq3_thlq4  =   0


        ! ---------------------------------------------------------------------
        !
        ! allocate forcepert_param (must not be associated at this time)

        ! if (associated(forcepert_param)) then
           ! write (*,*) 'assemble_forcepert_param(): this needs work...'
           ! write (*,*) 'stopping'
           ! stop
        ! end if

        N_forcepert = N_force

        call allocate_forcepert_param(N_forcepert, N_x, N_y, forcepert_param)

        ! ----------------------------------------
        !
        ! copy inputs into structure

        ! precip perturbations

        forcepert_param(ind_pcp)%descr          = descr_pcp
        forcepert_param(ind_pcp)%typ            = typ_pcp
        forcepert_param(ind_pcp)%std_normal_max = std_normal_max
        forcepert_param(ind_pcp)%zeromean       = zeromean
        forcepert_param(ind_pcp)%tcorr          = tcorr
        forcepert_param(ind_pcp)%xcorr          = xcorr
        forcepert_param(ind_pcp)%ycorr          = ycorr

        forcepert_param(ind_pcp)%std            = std_pcp

        forcepert_param(ind_pcp)%ccorr(1,:,:)   = 1.
        forcepert_param(ind_pcp)%ccorr(2,:,:)   = rho_pcp_sw
        forcepert_param(ind_pcp)%ccorr(3,:,:)   = rho_pcp_lw
        forcepert_param(ind_pcp)%ccorr(4,:,:)   = rho_pcp_swe
        forcepert_param(ind_pcp)%ccorr(5,:,:)   = rho_pcp_thlq1
        forcepert_param(ind_pcp)%ccorr(6,:,:)   = rho_pcp_thlq2
        forcepert_param(ind_pcp)%ccorr(7,:,:)   = rho_pcp_thlq3
        forcepert_param(ind_pcp)%ccorr(8,:,:)   = rho_pcp_thlq4



        ! shortwave perturbations

        forcepert_param(ind_sw)%descr          = descr_sw
        forcepert_param(ind_sw)%typ            = typ_sw
        forcepert_param(ind_sw)%std_normal_max = std_normal_max
        forcepert_param(ind_sw)%zeromean       = zeromean
        forcepert_param(ind_sw)%tcorr          = tcorr
        forcepert_param(ind_sw)%xcorr          = xcorr
        forcepert_param(ind_sw)%ycorr          = ycorr

        forcepert_param(ind_sw)%std            = std_sw

        forcepert_param(ind_sw)%ccorr(1,:,:)   = rho_pcp_sw
        forcepert_param(ind_sw)%ccorr(2,:,:)   = 1.
        forcepert_param(ind_sw)%ccorr(3,:,:)   = rho_sw_lw
        forcepert_param(ind_sw)%ccorr(4,:,:)   = rho_sw_swe
        forcepert_param(ind_sw)%ccorr(5,:,:)   = rho_sw_thlq1
        forcepert_param(ind_sw)%ccorr(6,:,:)   = rho_sw_thlq2
        forcepert_param(ind_sw)%ccorr(7,:,:)   = rho_sw_thlq3
        forcepert_param(ind_sw)%ccorr(8,:,:)   = rho_sw_thlq4


        ! longwave perturbations

        forcepert_param(ind_lw)%descr          = descr_lw
        forcepert_param(ind_lw)%typ            = typ_lw
        forcepert_param(ind_lw)%std_normal_max = std_normal_max
        forcepert_param(ind_lw)%zeromean       = zeromean
        forcepert_param(ind_lw)%tcorr          = tcorr
        forcepert_param(ind_lw)%xcorr          = xcorr
        forcepert_param(ind_lw)%ycorr          = ycorr

        forcepert_param(ind_lw)%std            = std_lw

        forcepert_param(ind_lw)%ccorr(1,:,:)   = rho_pcp_lw
        forcepert_param(ind_lw)%ccorr(2,:,:)   = rho_sw_lw
        forcepert_param(ind_lw)%ccorr(3,:,:)   = 1.
        forcepert_param(ind_lw)%ccorr(4,:,:)   = rho_lw_swe
        forcepert_param(ind_lw)%ccorr(5,:,:)   = rho_lw_thlq1
        forcepert_param(ind_lw)%ccorr(6,:,:)   = rho_lw_thlq2
        forcepert_param(ind_lw)%ccorr(7,:,:)   = rho_lw_thlq3
        forcepert_param(ind_lw)%ccorr(8,:,:)   = rho_lw_thlq4


        ! swe perturbations

        forcepert_param(ind_swe)%descr          = descr_swe
        forcepert_param(ind_swe)%typ            = typ_swe
        forcepert_param(ind_swe)%std_normal_max = std_normal_max
        forcepert_param(ind_swe)%zeromean       = zeromean
        forcepert_param(ind_swe)%tcorr          = tcorr
        forcepert_param(ind_swe)%xcorr          = xcorr
        forcepert_param(ind_swe)%ycorr          = ycorr

        forcepert_param(ind_swe)%std            = std_swe

        forcepert_param(ind_swe)%ccorr(1,:,:)   = rho_pcp_swe
        forcepert_param(ind_swe)%ccorr(2,:,:)   = rho_sw_swe
        forcepert_param(ind_swe)%ccorr(3,:,:)   = rho_lw_swe
        forcepert_param(ind_swe)%ccorr(4,:,:)   = 1.
        forcepert_param(ind_swe)%ccorr(5,:,:)   = rho_swe_thlq1
        forcepert_param(ind_swe)%ccorr(6,:,:)   = rho_swe_thlq2
        forcepert_param(ind_swe)%ccorr(7,:,:)   = rho_swe_thlq3
        forcepert_param(ind_swe)%ccorr(8,:,:)   = rho_swe_thlq4

        ! soil moisture layer1
        forcepert_param(ind_thlq1)%descr          = descr_thlq1
        forcepert_param(ind_thlq1)%typ            = typ_thlq1
        forcepert_param(ind_thlq1)%std_normal_max = std_normal_max
        forcepert_param(ind_thlq1)%zeromean       = zeromean
        forcepert_param(ind_thlq1)%tcorr          = tcorr
        forcepert_param(ind_thlq1)%xcorr          = xcorr
        forcepert_param(ind_thlq1)%ycorr          = ycorr

        forcepert_param(ind_thlq1)%std            = std_thlq1

        forcepert_param(ind_thlq1)%ccorr(1,:,:)   = rho_pcp_thlq1
        forcepert_param(ind_thlq1)%ccorr(2,:,:)   = rho_sw_thlq1
        forcepert_param(ind_thlq1)%ccorr(3,:,:)   = rho_lw_thlq1
        forcepert_param(ind_thlq1)%ccorr(4,:,:)   = rho_swe_thlq1
        forcepert_param(ind_thlq1)%ccorr(5,:,:)   = 1.
        forcepert_param(ind_thlq1)%ccorr(6,:,:)   = rho_thlq1_thlq2
        forcepert_param(ind_thlq1)%ccorr(7,:,:)   = rho_thlq1_thlq3
        forcepert_param(ind_thlq1)%ccorr(8,:,:)   = rho_thlq1_thlq4

        ! soil moisture layer2
        forcepert_param(ind_thlq2)%descr          = descr_thlq2
        forcepert_param(ind_thlq2)%typ            = typ_thlq2
        forcepert_param(ind_thlq2)%std_normal_max = std_normal_max
        forcepert_param(ind_thlq2)%zeromean       = zeromean
        forcepert_param(ind_thlq2)%tcorr          = tcorr
        forcepert_param(ind_thlq2)%xcorr          = xcorr
        forcepert_param(ind_thlq2)%ycorr          = ycorr

        forcepert_param(ind_thlq2)%std            = std_thlq2

        forcepert_param(ind_thlq2)%ccorr(1,:,:)   = rho_pcp_thlq2
        forcepert_param(ind_thlq2)%ccorr(2,:,:)   = rho_sw_thlq2
        forcepert_param(ind_thlq2)%ccorr(3,:,:)   = rho_lw_thlq2
        forcepert_param(ind_thlq2)%ccorr(4,:,:)   = rho_swe_thlq2
        forcepert_param(ind_thlq2)%ccorr(5,:,:)   = rho_thlq1_thlq2
        forcepert_param(ind_thlq2)%ccorr(6,:,:)   = 1.
        forcepert_param(ind_thlq2)%ccorr(7,:,:)   = rho_thlq2_thlq3
        forcepert_param(ind_thlq2)%ccorr(8,:,:)   = rho_thlq2_thlq4

        ! soil moisture layer3
        forcepert_param(ind_thlq3)%descr          = descr_thlq3
        forcepert_param(ind_thlq3)%typ            = typ_thlq3
        forcepert_param(ind_thlq3)%std_normal_max = std_normal_max
        forcepert_param(ind_thlq3)%zeromean       = zeromean
        forcepert_param(ind_thlq3)%tcorr          = tcorr
        forcepert_param(ind_thlq3)%xcorr          = xcorr
        forcepert_param(ind_thlq3)%ycorr          = ycorr

        forcepert_param(ind_thlq3)%std            = std_thlq3

        forcepert_param(ind_thlq3)%ccorr(1,:,:)   = rho_pcp_thlq3
        forcepert_param(ind_thlq3)%ccorr(2,:,:)   = rho_sw_thlq3
        forcepert_param(ind_thlq3)%ccorr(3,:,:)   = rho_lw_thlq3
        forcepert_param(ind_thlq3)%ccorr(4,:,:)   = rho_swe_thlq3
        forcepert_param(ind_thlq3)%ccorr(5,:,:)   = rho_thlq1_thlq3
        forcepert_param(ind_thlq3)%ccorr(6,:,:)   = rho_thlq2_thlq3
        forcepert_param(ind_thlq3)%ccorr(7,:,:)   = 1.
        forcepert_param(ind_thlq3)%ccorr(8,:,:)   = rho_thlq3_thlq4

        ! soil moisture layer4
        forcepert_param(ind_thlq4)%descr          = descr_thlq4
        forcepert_param(ind_thlq4)%typ            = typ_thlq4
        forcepert_param(ind_thlq4)%std_normal_max = std_normal_max
        forcepert_param(ind_thlq4)%zeromean       = zeromean
        forcepert_param(ind_thlq4)%tcorr          = tcorr
        forcepert_param(ind_thlq4)%xcorr          = xcorr
        forcepert_param(ind_thlq4)%ycorr          = ycorr

        forcepert_param(ind_thlq4)%std            = std_thlq4

        forcepert_param(ind_thlq4)%ccorr(1,:,:)   = rho_pcp_thlq4
        forcepert_param(ind_thlq4)%ccorr(2,:,:)   = rho_sw_thlq4
        forcepert_param(ind_thlq4)%ccorr(3,:,:)   = rho_lw_thlq4
        forcepert_param(ind_thlq4)%ccorr(4,:,:)   = rho_swe_thlq4
        forcepert_param(ind_thlq4)%ccorr(5,:,:)   = rho_thlq1_thlq4
        forcepert_param(ind_thlq4)%ccorr(6,:,:)   = rho_thlq2_thlq4
        forcepert_param(ind_thlq4)%ccorr(7,:,:)   = rho_thlq3_thlq4
        forcepert_param(ind_thlq4)%ccorr(8,:,:)   = 1.



        !--------------------------------------------------------------
        ! Constructing the correlation matrix
        ! Added by Ala Bahrami 14/08/2017

        do i = 1 , N_forcepert
            do j = 1 , N_forcepert
                tmpmat(i , j) = forcepert_param(i)%ccorr(j,1,1)
            end do
        end do

        ! -------------------------------------------------------------
        !
        ! set mean and (if needed) modify standard deviation according to 'typ'
        ! (additive or multiplicative perturbations)

        do k = 1 , N_forcepert

           select case (forcepert_param(k)%typ)

           case (0)         ! additive (mean=0, std as above)

              forcepert_param(k)%mean  = 0.

           ! Note : here the other fields which have the lognormal distributions, their
           ! mu and sigma values should be calulcated based on their mean and standard deviation



           case (1)         ! multiplicative and lognormal (mean = 1 )

              do i = 1 , N_x
                 do j = 1 , N_y

                    tmpreal = forcepert_param(k)%std(i,j)

                    tmpreal = log( 1. + tmpreal ** 2)

                    forcepert_param(k)%mean(i,j) = - .5 * tmpreal
                    forcepert_param(k)%std(i,j)  = sqrt(tmpreal)


                 end do
              end do

           case default

              write (*,*) 'assemble_forcepert_param(): encountered unknown'
              write (*,*) 'type of error, stopping...'
              stop

           end select

        end do

        ! -------------------------------------------------------------
        !
        ! compute sqrt of correlation matrix for each grid point
        ! here I modified the code to consider the correlation matrix
        ! which its size is more than 3*3

        do i = 1, N_x
           do j = 1 , N_y

              if (N_forcepert==1) then

                 forcepert_param(1)%ccorr(1,i,j) = 1.

              else

                 ! call get_sqrt_corr_matrix( N_forcepert, &
                      ! forcepert_param(1)%ccorr(2,i,j),   &
                      ! forcepert_param(1)%ccorr(3,i,j),   &
                      ! forcepert_param(2)%ccorr(3,i,j),   &
                      ! tmpmat )

                ! calling the cholesky_decomposition
                ! Added by Ala Bahrami on 14/08/2017

                call choldc(N_forcepert , tmpmat)


                 ! overwrite cross-correlations in forcepert_param with square
                 ! root of cross-correlation matrix

                 do k = 1 , N_forcepert
                    do l = 1 , N_forcepert

                       forcepert_param(k)%ccorr(l, i, j) = tmpmat(k , l)

                    end do
                 end do

              end if

           end do
        end do

    ! -------------------------------------------------------------
    end subroutine assemble_forcepert_param

    subroutine choldc(n,B)
    !
    ! Modified and added by Ala Bahrami: 14/08/2017
    !
    ! The purpsoe of this function is to generate lower triangular matrix
    ! based on the cholessky factorization (decomposition). We assume that the correlation matrix
    ! is (complex Hermitian) symmetric. .What I found that is that the rank of matrix
    ! be 3 if the size of the matrix is 3*3 or all the eigenvalues should be positive
    ! (positive definite). In Matlab we can use chol(R,'lower')
    ! The cholesky decomposition is about a factor of two faster than alternative method for
    ! solving equations.
    ! Reference : (Numerical Recipes, Cholesky decomposition , pp. 89)


            implicit none

            integer :: n
            integer :: i, j, k
            real :: sum
            !integer :: j

            real, dimension(n) :: p
            real, intent(inout), dimension(n , n) :: B

            do i = 1 , n
                do j = i , n

                    sum = B(i,j)

                    do k = i-1 , 1 , -1

                        sum = sum - B( i , k) * B( j , k)

                    end do

                    if(i.eq.j) then

                        if(sum.le.0.) pause 'choldc failed'
                        p(i) = sqrt(sum)

                    else

                        B(j,i) = sum/p(i)

                    endif

                end do
            end do

            ! Substitude the diagonal value with vector p
            do i = 1 , n

                B(i , i) = p(i)

            end do

            ! Substitude the upper diagonal values with zero
            do i = 1 , n

                if (i.lt.n) then

                    do j = i + 1 , n

                        B(i , j) = .0

                    end do

                else
                        return
                endif

            end do


    end subroutine choldc


    subroutine get_sqrt_corr_matrix( N, rho12, rho13, rho23, A )

    ! get sqrt of correlation matrix
    !
    ! correlation matrix has diagonal=variance=1
    !
    ! corr_matrix = [ 1 rho12 rho13; rho12 1 rho23; rho13 rho23 1 ]
    !
    ! A is lower tri-angular
    !
    ! A = sqrt_corr_matrix = [ a11 0 0; a12 a22 0; a13 a23 a33 ]
    !
    ! A*A' = corr_matrix
    !
    ! so far only implemented for 3-by-3 correlation matrices with all
    !  nonnegative eigenvalues (no check for eigenvalues!)
    !
    ! reichle, 25 Jan 2005
    !



    implicit none

    integer, intent(in) :: N
    real,    intent(in) :: rho12, rho13, rho23

    real, intent(out), dimension(N,N) :: A

    ! ------------------------------------------------------------------

    if (N/=3) then

       write (*,*) 'get_sqrt_corr_matrix() not implemented for N<=3'

    else

       A(3,3) = 1. - rho12 * rho12  ! temporary

       A(1,1) = 1.

       A(1,2) = 0.
       A(2,1) = rho12

       A(2,2) = sqrt(A(3,3))

       A(1,3) = 0.
       A(3,1) = rho13
       A(2,3) = 0.
       A(3,2) = (rho23 - rho12 * rho13)/ A(2,2)

       A(3,3) =                                                          &
            sqrt(                                                        &
            (A(3,3) - rho13 * rho13 - rho23 * rho23 + 2*rho12*rho13*rho23)   &
            /                                                            &
            A(3,3) )

    end if

  end subroutine get_sqrt_corr_matrix




end module