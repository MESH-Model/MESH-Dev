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

!**********************************************************************
!
! nr_ran2_gasdev.f90
!
! adapted Numerical Recipes random number generator ran2() and gasdev()
!
! use ran2() instead of rand() and/or ran1() for longer period (~1e18)
!
! gasdev() uses ran2() instead of ran1()
!
! eliminate all save attributes, convert functions into subroutines,
! pass seed state vector into subroutines, enclose in module
!
! reichle, 16 Feb 2005
! reichle, 18 Feb 2005 - make nr_ran2() public, change call statement
!
! *********************************************************************

module random_fields_nr_ran2

    implicit none

    public :: NRANDSEED

    public :: nr_ran2

    integer, parameter :: NRANDSEED = 35

    integer, parameter :: NTAB = NRANDSEED-3
    integer, parameter :: IM1=2147483563
    integer, parameter :: IM2=2147483399
    real,    parameter :: AM=1./IM1
    integer, parameter :: IMM1=IM1-1
    integer, parameter :: IA1=40014
    integer, parameter :: IA2=40692
    integer, parameter :: IQ1=53668
    integer, parameter :: IQ2=52774
    integer, parameter :: IR1=12211
    integer, parameter :: IR2=3791
    integer, parameter :: NDIV=1+IMM1/NTAB
    real,    parameter :: EPS=1.2e-7
    real,    parameter :: RNMX=1.-EPS

contains

    subroutine nr_ran2(rseed, ran_num)

        implicit none

        integer, dimension(NRANDSEED), intent(inout) :: rseed

        real, intent(out) :: ran_num

        ! local variables

        integer :: idum, idum2, iy

        integer, dimension(NTAB) :: iv

        integer :: j, k

        ! -------------------------------------------------------
        ! Note : According to explanation on page 271 of Numerical Recipes, the The reason of using two different
        ! sequence with different periods, is to extends number of calls. The new generared period is product of
        ! two periods. When number of calls starts become order of m ~ m/20 the statistical test fails.
        ! Based on L'Ecuyer recommends, two moduli are slightly less than 2^31. The period of IM1-1 = 2*3*7*631*81031
        ! and the period of IM2-1 = 2*19*31*1019*1789. Both of them has the factor of 2, so the period of combined generator
        ! is ~ 2.3 * 10^18.


        idum  = rseed(1)
        idum2 = rseed(2)
        iy    = rseed(3)
        iv    = rseed(4:NRANDSEED)

        ! start here when not initializing
        k = idum / IQ1

        !Compute idum = mod (IA1*idum,IM1) without overflows by Scharge's method.
        !
        ! Note Explained by Ala Bahrami: As it was explained on page 269 of Numerical Recipes, in order to generate
        ! a squence of random number, the random number on time (j+1) is produced by this equation
        ! I(j+1) = aI(j) (mod m). (7.1.2)
        ! m is usually is close to the machine's largest representable integer, e.g., 2^31 - 1 and a = 7^5 = 16807
        ! In the implementation of the equation (7.1.2) it's is impossible to implement because of the fact that the
        ! the product a and m-1 exceeds the maximum value of a 32-bit integer. So, Scharge proposed a trick which can be
        ! implemented on any programming language on any machine. He propossed that if r < q then ,  0 < z < m-1, it can be
        ! be shown that both a(z mod q) and r[z/q] lie in the range of the 0,...,m-1.
        !
        ! Ala's  explanation :
        !
        ! m = aq + r
        ! in order to calcuate az mod m, here z means the idum, instead of using the mod fucntion we can use the :
        !
        ! az mod m equal : az -km = az - k (aq + r) = az - kaq -kr = a(z -kq) - kr which can  be translated to :
        ! a * (z mod q) - r[z/q]. Here it was supposed that if r < q then k = [z/q] = [az/m] , m = aq + r.
        ! az / m = az /(aq+r) = z / (q + r/q). Therefore, we can assume that k = [z/q] = [az/m]

        idum = IA1 * (idum - k*IQ1) - k*IR1
        ! preventing idum becomes zero
        if (idum.lt.0) idum = idum + IM1
        k = idum2/IQ2

        ! Compute idum2 = mod (IA2*idum2,IM2) likewise
        idum2 = IA2 * (idum2 - k*IQ2) - k*IR2
        if (idum2.lt.0) idum2 = idum2 + IM2

        ! Will be in range 1 : NTAB
        j = 1 + iy/NDIV

        ! here idum is shuffled, idum and and idum2 are combined to generate the output
        iy = iv(j) - idum2
        iv(j) = idum
        if(iy.lt.1)iy = iy + IMM1

        ! because users don't expect endpoint values.
        ran_num = min(AM*iy , RNMX)

        rseed(1)           = idum
        rseed(2)           = idum2
        rseed(3)           = iy
        rseed(4 : NRANDSEED) = iv

        return

      END subroutine nr_ran2

    subroutine init_randseed( rseed )

        implicit none

        integer, dimension(NRANDSEED), intent(inout) :: rseed

        ! local variables

        integer :: idum, idum2, iy

        integer, dimension(NTAB) :: iv

        integer :: j, k

        ! ------------------------------------------------------------

        idum = rseed(1)

        if (idum<=0) then
           ! be sure to prevent idum = 0.
           idum = max(-idum , 1)
           idum2 = idum

           ! load the shuffle table after 8 warm-ups
           do j = NTAB + 8, 1, -1
              k = idum / IQ1
              idum = IA1 * (idum - k*IQ1) -k*IR1
              if (idum.lt.0) idum = idum + IM1
              if (j.le.NTAB) iv(j)=idum
           end do

           iy=iv(1)
        else
           write (*,*) 'init_randseed(): initialize by calling with rseed(1)<0'
           write (*,*) 'STOPPING.'
           stop
        end if

        rseed(1)           = idum
        rseed(2)           = idum2
        rseed(3)           = iy
        rseed(4:NRANDSEED) = iv

  end subroutine init_randseed

  !******************************************************************
  !
  ! gasdev() adapted to use ran2()
  !
  ! Returns TWO normally distributed deviates with zero mean and unit
  ! variance, using ran2() as the source of uniform deviates.
  !
  ! Use init_randseed() to initialize.

    subroutine nr_gasdev(rseed, gasdev)

        implicit none

        integer, dimension(NRANDSEED), intent(inout) :: rseed

        real, dimension(2), intent(out) :: gasdev

        ! local variables

        real :: fac, rsq, v1, v2

        ! ---------------

    1   call nr_ran2(rseed, v1)
        call nr_ran2(rseed, v2)

        ! Note : pick two uniform numbers in the square extending from -1 to +1 in each direction
        v1=2.*v1-1.
        v2=2.*v2-1.
        ! see if they are in the unit circle

        rsq=v1**2+v2**2

        ! checking if it's inside the unit circle, if not generate again
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gasdev(1)=v1*fac
        gasdev(2)=v2*fac

        return

  end subroutine nr_gasdev

end module