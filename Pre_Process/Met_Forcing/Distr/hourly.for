      subroutine hourly(ih,nh,ng,var,var_hr)

c - this subroutine ensures the forcing data is hourly
c - variables
c var = forcing variable
c var_hr = hourly varaible
c nh = number of hours
c ih = interval hour
c ng = number of stations or guages

      real ih
      real var_hr(nh,ng)
      real var(int(nh/ih),ng)

cbjd average half-hourly to hourly
      if(ih.eq.0.5) then
        do m=1,ng
          do n=1,nh 
            var_hr(n,m)=(var(2*n-1,m)+var(2*n,m))/2
           continue
          enddo
        enddo
cbjd or keep the hourly
      elseif(ih.eq.1) then
        do m=1,ng
          do n=1,nh
            var_hr(n,m)=var(n,m)
           continue
          enddo
        enddo
      else
        print*, 'program aborting: ih ne 1 or 0.5'
        stop
      endif

      return
      end
