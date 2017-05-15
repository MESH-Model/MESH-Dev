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
! modified by : Ala Bahrami
!
! Bahrami, 21/03/2017 - changing allocation of the pointer fpp variable
!                     - added allocate(fpp(N_forcepert))
!                     - removed the program test in the module
! Bahrami, 03/05/2017 - I have added deallocate (fpp)
! =========================================================================

module forcepert_types

  implicit none

  save

  ! everything is private by default unless made public

  private

  public :: forcepert_param_type
  public :: allocate_forcepert_param

  ! --------------------------------------------------------------------
  !
  ! parameters for each kind of forcing perturbation (precip, radiation, etc)

  type :: forcepert_param_type

     character(40)    :: descr    ! 'precip', 'shortwave', etc
     integer          :: typ      ! add or multiply model error?

     ! max allowed normalized perturbation (relative to N(0,1))

     real             :: std_normal_max

     ! if .true. enforce zeromean across ensemble
     ! (implies mean=1 for multiplicative perturbations)
     ! (not applicable if only one ensemble member is done at a time)

     logical          :: zeromean        ! enforce zero mean across ensemble

     ! Mean and std are allowed to vary in space (dimension(N_x,N_y)).

     real, dimension(:,:), pointer :: mean      ! mean
     real, dimension(:,:), pointer :: std       ! standard deviation

     ! Cross-correlations between different kinds of forcing perturbations
     ! (eg. between precip and shortwave perturbations) are allowed to vary
     ! in space (dimension(N_forcepert_kind,N_x,N_y)).

     real, dimension(:,:,:), pointer :: ccorr

     ! Spatial and temporal correlation scales must be constant in space.
     ! For non-zero cross-correlations they must also be the same for
     ! all kinds for forcing perturbations (eg. if precip and radiation
     ! perturbations are cross-correlated, their xcorr, ycorr and tcorr
     ! must be the same).

     real             :: xcorr  ! correlation length along latitudes   [deg]
     real             :: ycorr  ! correlation length along longitudes  [deg]
     real             :: tcorr  ! temporal correlation length          [s]

  end type forcepert_param_type

  ! --------------------------------------------------------------------

contains

  subroutine allocate_forcepert_param(N_forcepert, N_x, N_y, fpp)

    implicit none

    integer, intent(in) :: N_forcepert, N_x, N_y

    !type(forcepert_param_type), dimension(:), pointer :: fpp
    type(forcepert_param_type), dimension(:), allocatable :: fpp


    ! local variables

    integer :: k

    ! --------------------------------------------------------

    !nullify(fpp)
    !nullify(fpp%mean)
    !nullify(fpp%std)
    !nullify(fpp%ccorr)

    ! Added by Ala Bahrami
    if (allocated (fpp)) deallocate (fpp)

    ! Added by Ala Bahrami
    allocate(fpp(N_forcepert))

    do k = 1 , N_forcepert

       allocate(fpp(k)%mean(N_x,N_y))
       allocate(fpp(k)%std(N_x,N_y))
       allocate(fpp(k)%ccorr(N_forcepert,N_x,N_y))

    end do

  end subroutine allocate_forcepert_param

  ! ------------------------------------------------------------------

end module forcepert_types
