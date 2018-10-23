!>
!> Author: Ala Bahrami
!> Date of creation : 01 March 2017
!> last modified    : 02/02/2018
!                   - Adding data assimilation variables
!                   - Adding perturbaiton variables
!
!> written for Ph.D Thesis
!> Type:   f90
!>
module file_variables

    implicit none

    integer (kind = 4) :: N_x, N_y, n_e, N_forcepert, N_ens, n


    real (kind = 4) :: dx, dy, lambda_x, lambda_y, variance

    ! temporal variables
    real :: dtstep, tcorr

    !real (kind = 8), allocatable, dimension(:,:) :: field1, field2
    !integer, parameter :: N1= 49
    !integer, parameter :: N2= 113

    !double precision, dimension(N1 , N2) :: rfield, rfield2
    double precision, dimension(: , :), allocatable :: rfield, rfield2
    !double precision, dimension(:,:), allocatable :: field1, field2
    !double precision, dimension(:,:), allocatable :: rfield, rfield2


    ! local variables
    integer (kind = 4) :: N_x_fft, N_y_fft, N_x_fft2

    real :: dkx, dky, theta, ran_num

    real, parameter :: MY_PI = 3.14159265


    !include "fftw3.f90"


    ! Variables for FFT calculation
    !double precision , dimension(:,:), allocatable :: field1_fft, field2_fft
    double complex , dimension(:,:), allocatable :: field1_fft, field2_fft


    ! allocatble variable for FFT Inverse
    !complex (kind = 8), dimension(:,:), allocatable :: field1_fft_inv, field2_fft_inv
    double complex, dimension(:,:), allocatable :: field1_fft_inv, field2_fft_inv

    ! local variable used in the get_fft_grid

    ! specify by how many correlation lengths the fft grid must be
    ! be larger than the grid2cat grid
    real, parameter :: mult_of_xcorr = 2.
    real, parameter :: mult_of_ycorr = 2.

    ! random variables
    integer :: RSEEDCONST
    integer :: NRANDSEED2
    integer, parameter :: NRANDSEED = 35
    integer, dimension(NRANDSEED) :: rseed

    ! FFT variables
!   integer (kind = 8) plan_forward

    ! variable which are used in producing the forcing perturbation
    logical :: initialize

    integer, dimension(:),   allocatable  :: ens_id
    integer, dimension(:,:), allocatable  :: Forcepert_rseed
    ! restorable rseed varibale
    integer, dimension(:,:), allocatable  :: rseed_store
    integer, dimension(:,:), allocatable  :: Forcepert_rseed_store


    real, dimension(:,:,:,:), allocatable :: Forcepert
    real, dimension (:,:,:) ,allocatable  :: Forcepert_vect
    real, dimension(:,:,:,:), allocatable :: Forcepert_ntrmdt
    ! restorable ntrmdt variable
    real, dimension(:,:,:,:), allocatable :: ntrmdt_store
    real, dimension(:,:)    , allocatable :: Forcepert_ntrmdt_store


    ! Perturbed GRACE TWSA retrievals
    real, dimension(:,:), allocatable :: GR_Pert

    !type(forcepert_param_type), dimension(:), pointer :: forcepert_param
    ! Timing stamps variables

    !> ------------------------------------------------------------
    !> Forcing data and perturbation variables
    !> ------------------------------------------------------------
    ! lines 228 : 240
    !> Added by Ala Bahrami
    ! Variables for forcing data
    integer tt, N_t, n2

    !> Added by Ala Bahrami
    ! Perturbation_GAT
    real, dimension (:,:), allocatable :: precip_pert
    real, dimension (:,:), allocatable :: sw_pert
    real, dimension (:,:), allocatable :: lw_pert
    real, dimension (:,:), allocatable :: swe_pert
    real, dimension (:,:), allocatable :: thlq_pert

    !> ------------------------------------------------------------
    !> Data assimilation variables
    !> ------------------------------------------------------------

    ! Measurement Operator
    real, dimension(:,:), allocatable :: H
    real, dimension(:,:), allocatable :: HT

    ! Model States, dimension(13*NML , N_ens)
    real, dimension(:,:), allocatable :: X

    ! Model states average
    real, dimension(:), allocatable :: X2
    real, dimension(:,:), allocatable :: X_ave

    ! Ensemble Perturbations,  dimension (13*NML , N_ens)
    real, dimension(:,:), allocatable :: A

    ! other variables
    ! X3, dimension (20 , 1)
    real, dimension(:,:), allocatable :: X3

    ! X4, dimension (13*NML , 1)
    real, dimension(:,:), allocatable :: X4

    ! X5, dimension(1 , 1)
    real, dimension(:,:), allocatable :: X5

    real GR_er

    ! Kalman Gain, dimension(13*NML , 1)
    real, dimension(:,:), allocatable :: KG

    ! Analysis Increment (13*NML , N_ens)
    real, dimension(:,:), allocatable :: AI

    logical assim_mode

    integer it_counter, month_counter

    ! Innovation matrix, dimension (1, N_ens)
    real, dimension(:,:), allocatable :: D

    ! model predicted TWS, dimension(1, N_ens)
    real, dimension(:,:), allocatable :: HX

    !> Local variables, Jlulian calender.
    integer nmth, ndy

    ! Accumulated storage
    real, dimension(:,:), allocatable :: stg_accum

    ! Perturbation resume variable
    integer :: RESUMEFLAG_per
    integer :: SAVERESUMEFLAG_per

    ! variable to assign Ensemble memeber naming
    character(5) strens
    character(100) fl_ens

    integer :: days_n
    integer :: month_n
    save

end module