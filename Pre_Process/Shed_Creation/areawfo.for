      MODULE areawfo


!     note that outwfo is (column,row) & outarray is (row,column)

      real*4, dimension(:,:), allocatable:: outwfo,outarray,inarray

      real*4, dimension(:),   allocatable::wfo_sum_p,wfo_cum_p 



      real         :: xorigin,yorigin,xdelta,ydelta,angle
      integer      :: xcount,ycount,deltat
      character(10) ::  starttime,startdate

        CHARACTER(64), DIMENSION(:), ALLOCATABLE :: attname
        CHARACTER(32), DIMENSION(:), ALLOCATABLE :: attunits



      END MODULE areawfo
