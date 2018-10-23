! +-======-+
!  Copyright (c) 2003-2007 United States Government as represented by
!  the Admistrator of the National Aeronautics and Space Administration.
!  All Rights Reserved.
!
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES,
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:
!           Dale Hithon, SRA Assistant, (301) 286-2691
!
! +-======-+
! =========================================================================
!
! random_fields.f90
!
! random field generator in 2d:
!  generate a pair of random fields in 2d with zero mean
!
! subroutines rfg2d_fft() and sqrt_gauss_spectrum are translated from
!  C++ code rfg2d.C written for MIT EnKF work by reichle
!  (see janus:~reichle/nasa/EnKF)
!
! covariance is specified through its spectrum, so far only Gaussian
!
! IMPORTANT: read comments for function rfg2d_fft()
!
! written for NSIPP - EnKF
! Type:   f90
! Author: Rolf Reichle
! Date:   2 Nov 2001
!
! reichle, 18 Feb 2005 - updated for use with module nr_ran2_gasdev
!                        deleted use of module select_kinds
!
! =========================================================================
!
! modified by : Ala Bahrami
!
! Bahrami, 21/03/2017 -   use nr_ran2_gasdev to use random_fields_nr_ran2
!                     -   I have added the file_variables  module
!                     -   Changing name of variables from N_x and N_y to N_x_fft , N_y_fft
!                     -   Changing the precision of variable sqrt_spectrum_2d from real to double complex
!                     -   Remove inclusion of the CXMLDEF.FOR
!                     -   Adding inclusion of the fftw3.f90
!                     -   Modifying the precision of the field1 and field2 from real to double complex
!                     -   Adding allocating of field1_fft_inv and field2_fft_inv variables
!                     -   Modifying using CXML 2d fft libs in the rfg2d_fft() subroutine
!                     -   Adding dfftw_plan_dft_2d_(), dfftw_execute_(), dfftw_destroy_plan_() functions.
!                     -   Removing float(N_x_fft)*float(N_y_fft) multiplier in the calculation of the field1 and field 2
!                     -   Adding the deallocation of the field1_fft_inv and  field2_fft_inv
!                     -   Modifying the precision of the rfield from real to double precision.
!                     -   Removing test_rfg2d program in the module
!
! =========================================================================

module random_fields2

    !use nr_ran2_gasdev
    ! Ala Bahrami changed use nr_ran2_gasdev to  random_fields_nr_ran2
    use random_fields_nr_ran2

    implicit none

    contains

    subroutine get_fft_grid( N_x, N_y, dx, dy, lambda_x, lambda_y, N_x_fft, N_y_fft)

        !
        ! get a grid that can be used for 2d fft random number generator
        ! (this fft grid must extend beyond the desired random field
        !  by about two correlations lengths, it must also have N_x_fft and
        !  N_y_fft that are powers of two)


        ! Ala Bahrami added this
        use file_variables, only : mult_of_xcorr , mult_of_ycorr

        ! ----------------------------------------------------------------
        integer, intent(in) :: N_x, N_y
        !integer :: N_x, N_y

        real, intent(in) :: dx, dy, lambda_x, lambda_y
        !real :: dx, dy, lambda_x, lambda_y

        integer, intent(out) :: N_x_fft, N_y_fft
        !integer :: N_x_fft, N_y_fft

        ! local variables
        !real, parameter :: mult_of_xcorr = 2.
        !real, parameter :: mult_of_ycorr = 2.


        ! ------------------------------

        ! local variables

        ! specify by how many correlation lengths the fft grid must be
        ! be larger than the grid2cat grid

        !real, parameter :: mult_of_xcorr = 2.
        !real, parameter :: mult_of_ycorr = 2.

        ! ----------------------------------------------------------------

        ! add minimum required correlation lengths

        N_x_fft = N_x + ceiling(mult_of_xcorr*lambda_x/dx)
        N_y_fft = N_y + ceiling(mult_of_ycorr*lambda_y/dy)

        ! make sure N_x_fft, N_y_fft are power of two

        N_x_fft = 2**ceiling( log(real(N_x_fft))/log(2.) )
        N_y_fft = 2**ceiling( log(real(N_y_fft))/log(2.) )

        ! echo findings

        ! write (*,*)
        ! write (*,*) 'desired random field:'
        ! write (*,*) 'Number of nodes in x direction = ', N_x, ' Number of nodes in y direction      = ', N_y
        ! write (*,*) ' dx = ', dx,           '  dy = ', dy
        ! write (*,*) 'Decorrelation length in x direction   = ', lambda_x, ' Decorrelation length in y direction = ', lambda_y
        ! write (*,*)
        ! write (*,*) 'grid used for fft: '
        ! write (*,*) ' N_x_fft = ', N_x_fft, ' N_y_fft = ', N_y_fft
        ! write (*,*)

        !return

    end subroutine get_fft_grid

  ! -------------------------------------------------------------------------
  !
  ! subroutine sqrt_gauss_spectrum_2d()
  !
  ! get SQUARE ROOT of 2d Gaussian spectrum (incl volume element)
  !
  ! 2d Gaussian spectrum:
  !
  ! S(kx,ky) = variance
  !            *
  !            lambda_x*lambda_y/(2*pi)
  !            *
  !            exp( -(kx^2*lambda_x^2 + ky^2*lambda_y^2)/2 )
  !
  ! return: sqrt( S*dkx*dky )
  !
  ! that is return the SQUARE ROOT of the spectrum multiplied with the
  !  square root of the volume element d2k=dkx*dky of the ifft integral
  !
  ! spectrum is returned in "wrap-around" order compatible with CXML and
  !  matlab FFT algorithms
  !
  ! inputs:
  !  variance : variance desired for complex field, if pair of real fields
  !             is used each field must eventually be multiplied with sqrt(2)
  !  N_x      : number of nodes in x direction
  !  N_y      : number of nodes in y direction
  !  dkx      : wave number spacing in x direction
  !  dky      : wave number spacing in y direction
  !  lambda_x : decorrelation length in x direction
  !  lambda_y : decorrelation length in y direction
  !
  !-------------------------------------------------------------

    subroutine sqrt_gauss_spectrum_2d( variance, N_x_fft, N_y_fft, dkx, dky, &
    lambda_x, lambda_y, sqrt_spectrum_2d )

        use file_variables, only : MY_PI
!DAN
        use, intrinsic :: iso_c_binding

        real, intent(in) :: variance, dkx, dky, lambda_x, lambda_y

        integer, intent(in) :: N_x_fft, N_y_fft

        ! Ala Bahrami changed this
!DAN        double complex, intent(out), dimension(N_x_fft , N_y_fft) :: sqrt_spectrum_2d
        complex(C_DOUBLE_COMPLEX), dimension(N_x_fft, N_y_fft) :: sqrt_spectrum_2d

        ! -------------------------------------
        ! local variables

        integer :: i, j

        !real, parameter :: MY_PI = 3.14159265

        real :: fac, lamx2dkx2, lamy2dky2

        real, dimension(N_x_fft) :: lx2kx2
        real, dimension(N_y_fft) :: ly2ky2

        ! ------------------------------------------------------------------

        ! factor includes sqrt of volume element of ifft integral

        fac       = sqrt( variance * lambda_x * lambda_y / (2. * MY_PI) * dkx * dky )

        lamx2dkx2 = lambda_x * lambda_x * dkx * dkx
        lamy2dky2 = lambda_y * lambda_y * dky * dky


        ! precompute (lambda_x*k_x)^2 in "wrap-around" order suitable for CXML fft
        ! Ref : page 389, McLaughlin
        do i = 1 , (N_x_fft / 2)
           lx2kx2(i) = lamx2dkx2 * (i-1) * (i-1)
        end do

        do i = (N_x_fft/2 + 1) , N_x_fft
           lx2kx2(i) = lamx2dkx2 * (N_x_fft - i + 1)*(N_x_fft - i + 1) ! minus drops out when squared
        end do

        ! precompute (lambda_y*k_y)^2 in "wrap-around" order suitable for CXML fft

        do j = 1 , (N_y_fft / 2)
           ly2ky2(j) = lamy2dky2 *(j - 1) * (j - 1)
        end do

        do j = (N_y_fft / 2 + 1), N_y_fft
           ly2ky2(j) = lamy2dky2 * (N_y_fft - j + 1) * (N_y_fft - j + 1) ! minus drops out when squared
        end do

        ! assemble spectrum in "wrap-around" order

        do i = 1 , N_x_fft
           do j = 1 , N_y_fft
              sqrt_spectrum_2d(i,j) = fac * exp(-.25 * (lx2kx2(i) + ly2ky2(j)) )
           end do
        end do

        return

      end subroutine sqrt_gauss_spectrum_2d


    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  ! ----------------------------------------------------------------------
  !
  ! subroutine rfg2d_fft()
  !
  ! generate a pair of 2d zero-mean random fields using FFT method
  ! (so far only Gaussian covariance implemented)
  !
  ! NOTE: implemented with index counters of type int, must have
  !
  !          N_x*N_y < maximum integer on given machine
  !
  !       on yama/alborz (at MIT) int varies from -2147483648...2147483647
  !       -> can handle up to N_x*N_y = (32768)^2
  !       if larger fields are needed, rewrite with type long int etc
  !
  ! NOTE: The fft method for generating random fields produces
  !       fields that are periodic with the size of the domain,
  !       that is the field is (almost) the same on each boundary.
  !       This introduces unwanted correlations at lags shorter than
  !       the domain size. Therefore, only a part of the generated
  !       field is usable. As a rule of thumb, the fields should be
  !       generated on a grid that is two correlation lenghts bigger
  !       than the field on which the grid is desired. Then cut out
  !       fields of the necessary size.
  !       This procedure is included in rfg2d_fft().
  !
  ! NOTE: The variance specified as input is the theoretical variance of
  !       the complex field that is obtained from the inverse fft of the
  !       realization dZ.
  !       The sample variance of this *complex* field is a FIXED (non-random)
  !       number which depends on the size of the domain, the grid spacing,
  !       and the correlation length (but not on the random seed!!).
  !       (This number is non-random because the variance is the integral
  !        of the absolute value of the spectrum. In this integral the
  !        randomness disappears because we only choose a random phase angle.)
  !       For vanishing discretization and spectral truncation error,
  !       this number converges to the theoretical (input) value.
  !
  !       This function is set up to use the pair of two real fields, where
  !
  !                field1 = sqrt(2)*real(ifft(dZ))
  !                field2 = sqrt(2)*imag(ifft(dZ)).
  !
  !       The factor sqrt(2) re-scales the variance of field1 and field2
  !       such that for vanishing discretization error and spectral
  !       truncation error each field converges to the specified theoretical
  !       (input) variance.
  !       NOTE: The sum of the sample variances of the two real fields
  !       is equal to the (FIXED) sample variance of the complex field
  !       (before re-scaling with sqrt(2)).
  !       The individual sample variances within each pair vary from
  !       realization to realization.
  !
  ! -------------------------------------------------------------------



    subroutine rfg2d_fft(rseed, variance, N_x, N_y, dx, dy, lambda_x, lambda_y, &
    field1, field2)

        use file_variables, only : MY_PI!, plan_forward

!DAN
        use, intrinsic :: iso_c_binding
        ! ----------------------------------------------------------------
        implicit none

        ! Changed by Ala Bahrami
        ! include 'CXMLDEF.FOR'

        ! Added by Ala Bahrami
!DAN        include "fftw3.f90"
        include 'fftw3.f03'

        integer, intent(in) :: N_x, N_y

        real, intent(in) :: variance ,dx, dy, lambda_x, lambda_y

        integer, dimension(NRANDSEED), intent(inout) :: rseed

        ! Ala Bahrami changed this
        !real, intent(out), dimension(N_x,N_y) :: field1, field2
        double precision, intent(out), dimension(N_x , N_y) :: field1, field2

        !-------------------------------------------------------------------
        ! local variables
        integer :: N_x_fft, N_y_fft, i, j

        real :: dkx, dky, theta, ran_num

        !real, parameter :: MY_PI = 3.14159265

        !integer (kind = 8) plan_forward

!DAN    Only way I could get the FFTW calls to work with Intel
!DAN    compiler/libraries on plato/ComputeCanada clusters was to
!DAN    switch the "modern" Fortran implementation
!DAN    (See FFTW manual for details).
        type(C_PTR) plan

        ! Ala Bahrami changed this
        !real, dimension(:,:), allocatable :: field1_fft, field2_fft
!DAN        double complex, dimension(:,:), allocatable :: field1_fft, field2_fft
        complex(C_DOUBLE_COMPLEX), dimension(:, :), allocatable :: field1_fft, field2_fft

        ! Ala Bahrami added this
!DAN        double complex, dimension(:,:), allocatable :: field1_fft_inv, field2_fft_inv
        complex(C_DOUBLE_COMPLEX), dimension(:, :), allocatable :: field1_fft_inv, field2_fft_inv
        !-------------------------------------------------------------------
        ! Calculation dkx and dky calculation

        call get_fft_grid (N_x, N_y, dx, dy, lambda_x, lambda_y, N_x_fft, N_y_fft)

        dkx = (2. * MY_PI) / (float(N_x_fft) * dx)
        dky = (2. * MY_PI) / (float(N_y_fft) * dy)

        !write (*,*) ' dkx = ' , dkx , ' dky = ', dky


        !--------------------------------------------------------------------------
        ! Calling the
        ! Allocating variables
        allocate(field1_fft(N_x_fft, N_y_fft))
        allocate(field2_fft(N_x_fft, N_y_fft))

        ! Added by Ala Bahrami
        allocate(field1_fft_inv(N_x_fft, N_y_fft))
        allocate(field2_fft_inv(N_x_fft, N_y_fft))



        !--------------------------------------------------------------------------
        call sqrt_gauss_spectrum_2d( &
         variance, N_x_fft, N_y_fft, dkx, dky, lambda_x, lambda_y, &
         field1_fft)


        !----------------------------------------------------------------------------
        ! multiply with random phase angle

        do i = 1 , N_x_fft

           do j = 1 , N_y_fft


              call nr_ran2(rseed, ran_num)

              theta = (2.* MY_PI) * ran_num          ! random phase angle

              field2_fft(i,j) = sin( theta ) * field1_fft(i,j)
              field1_fft(i,j) = cos( theta ) * field1_fft(i,j)

           end do

        end do


        ! force dZ(1,1) to zero (-> zero mean random field)

        field1_fft(1,1) = 0.
        field2_fft(1,1) = 0.

        ! Ala Bahrami modified this
        !
        ! ! -----------------------------------------------------------
        ! !
        ! ! invoke appropriate CXML 2d fft - SINGLE precision!
        ! !
        ! ! order of arguments is wrong in web-documentation!!!
        ! ! (compare 1d and 3d ffts)
        ! !
        ! ! Complex transform in real data format:
        ! ! status = {C,Z}FFT_2D (input_format, output_format, direction,
        ! !                       in_real, in_imag, out_real, out_imag, ni, nj,
        ! !                       lda, ni_stride, nj_stride)

        ! #ifdef CXML_FFT_AVAILABLE

        ! cxml_status = cfft_2d('R', 'R', 'B',                                    &
         ! field1_fft, field2_fft, field1_fft, field2_fft, N_x_fft, N_y_fft,  &
         ! N_x_fft, 1, 1)

        ! if (cxml_status .ne. 0) then
            ! write (*,*) 'error when using CXML cfft_2d in rfg2d_fft()'
            ! stop
        ! end if

        ! #else

        ! write (*,*)
        ! write (*,*) 'rfg2d_fft(): this routine relies on CXML fft!'
        ! write (*,*)
        ! write (*,*) 'If CXML math library is available, define cpp variable'
        ! write (*,*) 'CXML_FFT_AVAILABLE, and link CXML fft math library at'
        ! write (*,*) 'compilation.'
        ! write (*,*)
        ! write (*,*) 'If CXML math library is not available, undefine cpp variable'
        ! write (*,*) 'CXML_FFT_AVAILABLE and use your own fft, otherwise cannot'
        ! write (*,*) 'use spatially correlated forcing perturbations.'
        ! write (*,*)
        ! write (*,*) 'STOPPING.'
        ! stop

        ! #endif

        ! ! multiply with factor sqrt(2) to get correct variance
        ! ! (see above and p. 388 Ruan and McLaughlin, 1998),
        ! ! also multiply with N_x_fft*N_y_fft to get correct scaling,
        ! ! also retain only useable part of field?_fft

        !>
        ! Ala Bahrami added this part instead of using CXML fft libs
        ! Ala Bahrami added using dfftw_plan_dft_2d_  instead of  cfft_2d
        ! The dfftw_plan_dft_2d_ function could be called by installing the FFTW 3 libs in Cygwin
        !-------------------------------------------------------------------------------
        ! Applying FFT inverse in order to obtian the pseudo random fields
        !
        ! field 1
!DAN        call dfftw_plan_dft_2d_ ( plan_forward, N_x_fft, N_y_fft, field1_fft, &
!DAN        field1_fft_inv, FFTW_FORWARD ,FFTW_ESTIMATE )
        plan = fftw_plan_dft_2d(N_x_fft, N_y_fft, field1_fft, field1_fft_inv, FFTW_FORWARD, FFTW_ESTIMATE)

        ! if call dfftw_plan_dft_c2r_2d_ is used half of the first dimension is returned
        !(See Evensen pseudo random generation).
        ! !call dfftw_plan_dft_c2r_2d_( plan_backward, N_x_fft, N_y_fft, field1_fft, &
        ! !                             field1_fft_inv, FFTW_ESTIMATE )


!DAN    If "old" style is to be enabled, 'execute' call which
!DAN    passes the arguments is preferred, per notes from manual.
!!      call dfftw_execute_ ( plan_forward )
!DAN        call dfftw_execute_dft_ ( plan_forward, field1_fft, field1_fft_inv )
        call fftw_execute_dft(plan, field1_fft, field1_fft_inv)

        !
        ! Discard the information assosicated with the plans
        !
!DAN        call dfftw_destroy_plan_ ( plan_forward )
        call fftw_destroy_plan(plan)

        !
        ! field 2
!DAN        call dfftw_plan_dft_2d_ ( plan_forward, N_x_fft, N_y_fft, field2_fft, &
!DAN        field2_fft_inv, FFTW_FORWARD ,FFTW_ESTIMATE )
        plan = fftw_plan_dft_2d(N_x_fft, N_y_fft, field2_fft, field2_fft_inv, FFTW_FORWARD, FFTW_ESTIMATE)


!DAN        call dfftw_execute_dft_ ( plan_forward, field2_fft, field2_fft_inv )
!!      call dfftw_execute_ ( plan_forward )
        call fftw_execute_dft(plan, field2_fft, field2_fft_inv)

        !
        ! Discard the information assosicated with the plans
        !
!DAN        call dfftw_destroy_plan_ ( plan_forward )
        call fftw_destroy_plan(plan)




        !----------------------------------------------------------------------------
        ! Extract desired field1 and field2 (N_x*N_y)
        ! multiply with factor sqrt(2) to get correct variance
        ! (see above and p. 388 Ruan and McLaughlin, 1998),
        ! also multiply with N_x_fft*N_y_fft to get correct scaling,
        ! also retain only useable part of field?_fft
        !
        ! Note : It doesn't neccessary to mutiply field 1 with factor N_x_fft * N_y_fft

        ! Ala Bahrami changed this part
        ! field1 = sqrt(2.)*float(N_x_fft)*float(N_y_fft)*field1_fft(1:N_x,1:N_y)
        ! field2 = sqrt(2.)*float(N_x_fft)*float(N_y_fft)*field2_fft(1:N_x,1:N_y)

        field1 = sqrt(2.) *  real (field1_fft_inv (1 : N_x, 1 : N_y))
        field2 = sqrt(2.) *  real (field2_fft_inv (1 : N_x, 1 : N_y))


        ! Deallocating variables
        deallocate(field1_fft)
        deallocate(field1_fft_inv)
        deallocate(field2_fft)
        deallocate(field2_fft_inv)


    end subroutine rfg2d_fft



    subroutine generate_white_field(N_x, N_y, rseed, rfield )

        ! generate standard-normal random field that is white in space

        ! note that nr_gasdev always produces a pair of random numbers
        !
        ! do not store random numbers between subsequent calls to
        ! the random field generator - works best if fields are large
        ! (ie. avoid using this subroutine with N_x=N_y=1)

        implicit none

        integer, intent(in) :: N_x, N_y

        integer, dimension(NRANDSEED), intent(inout) :: rseed

        ! Ala Bahrami changed this
        !real, dimension(N_x,N_y), intent(out) :: rfield
        double precision, dimension(N_x,N_y), intent(out) :: rfield

        ! local variables

        logical :: gauss_stored

        integer :: i, j

        real, dimension(2) :: tmp_real

        ! -----------------------------------------------------

        gauss_stored = .false.

        do i=1,N_x
           do j=1,N_y

              if (.not. gauss_stored) then

                 call nr_gasdev(rseed, tmp_real)

                 rfield(i,j) = tmp_real(1)

                 gauss_stored = .true.

              else

                 rfield(i,j) = tmp_real(2)

                 gauss_stored = .false.

              end if

           end do
        end do

  end subroutine generate_white_field



end module