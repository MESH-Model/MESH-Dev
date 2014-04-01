!C-----------------------------------------------------------------------------------------
!C     The subroutine checks if the dds bounds lie within the global bounds
!C-----------------------------------------------------------------------------------------
      subroutine check_dds_bounds(inout,minlimit,maxlimit,nr,nc,invari,invard)
      
      implicit none
      
      integer nr,nc,invari,invard,inout(nr,nc)
      real    minlimit(nr,nc),maxlimit(nr,nc)
      
      real    minlimitglb(nr,nc),maxlimitglb(nr,nc)
      integer i,j,ncount
      
      write(*,*)
      write(*,*)
      write(*,*)'Checking if dds limits lie within global bounds'
      write(*,*)

!C-----------READ GLOBAL MINIMUM AND MAXIMUM LIMIT-----------------------
	  open(10,file='minmax_parameters.txt',status='old')
      
      do i=1,invari
         read(10,*)
         read(10,*)minlimitglb(i,1)
         read(10,*)maxlimitglb(i,1)
      enddo
      do i=invari+1,invari+invard
         read(10,*)
         read(10,*)(minlimitglb(i,j),j=1,nc)
         read(10,*)(maxlimitglb(i,j),j=1,nc)
      enddo
      close(10)
   
      ncount = 0
      
      do i = 1, nr
         do j = 1, nc
            if(inout(i,j) .eq. 1)then
               if(minlimit(i,j).lt.minlimitglb(i,j).or.maxlimit(i,j).gt.maxlimitglb(i,j)) then
                  ncount = ncount + 1
                  if(ncount == 1)then
                     write(*,*)"The following dds parameter limits are out of the global limits"
                     write(*,*)
                     write(*,10)"ROW","COLUMN","Minimum value","Global minimum","Maximum value","Global maximum"
                     write(*,*)"  -----------------------------------------------------------------------------"
                  endif
                  write(*,20)i,j,minlimit(i,j),minlimitglb(i,j),maxlimit(i,j),maxlimitglb(i,j)
               endif
             endif
         enddo
      enddo
      
      if(ncount > 0)then
         write(*,*)
         write(*,*)ncount, " dds parameter limit(s) out of global limits"
         write(*,*)
         write(*,*)'Adjust the dds limits or modify the global limits'
         write(*,*)
         pause
         stop
      else
        write(*,*)'All dds parameter limits lie within the global limits'
        write(*,*)
      endif
      
 10   format(2(a6,2x),4(a15,2x))
 20   format(2(i6,2x),4(f15.4,2x))
      
      end
