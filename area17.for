      MODULE area17

c      real, dimension(:),   allocatable :: distance
c      real, dimension(:,:),   allocatable :: elv,da,slope,frac,bnkfll
c      real, dimension(:,:),   allocatable :: dummy,ch_length
c      real, dimension(:,:,:), allocatable :: aclass
c      integer, dimension(:,:), allocatable :: idummy,ijtemp
c      integer, dimension(:),  allocatable :: xxx,yyy,distflg,channel,
c     *         new,newxxx,newyyy,newrank,newnext
c      integer, dimension(:,:),allocatable :: rank,s,next,ielv,iak,
c     *                   irough,ichnl,ireach
c      real :: elevconv



      real, dimension(:),   allocatable :: distance
      real, dimension(:,:),   allocatable :: elv_2d,da_2d,slope_2d,
     *                        frac_2d,bnkfll_2d,dummy,ch_length_2d

      real, dimension(:,:,:), allocatable :: aclass_3d

      integer, dimension(:,:), allocatable :: idummy,ijtemp

      integer, dimension(:),  allocatable :: distflg,channel,
     *         new,newxxx,newyyy,newrank,newnext

      integer, dimension(:,:),allocatable :: rank_2d,s_2d,next_2d,
     *                   ielv,iak,irough_2d,ichnl_2d,ireach_2d

      real :: elevconv

      END MODULE area17


!     NOTE:  THIS MODULE CANNOT BE USED IN CONJUNCTION WITH AREA1A
!            DUE TO DUPLICATE ARRAY NAMES WITH DIFFERENT SIZES




