      subroutine distr(nh,ixr,iyr,ng,w,p,hourly_var)

c - this subroutine distributes forcing variables according to weights.
      real*8 p(iyr,ixr,nh),w(iyr,ixr,ng)
      real hourly_var(nh,ng)
      integer hr,is

      do 100 i=1,iyr
        do 100 j=1,ixr
          do 100 is=1,ng
            do 100 hr=1,nh
                p(i,j,hr)= p(i,j,hr)+w(i,j,is)*hourly_var(hr,is)
100   continue

      return
      end
