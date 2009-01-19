      SUBROUTINE arrange()


!*************************************************************************
! WRITTEN IN 1972
!
! ARRANGEA - ASSIGNMENT OF ELEMENT YYY, XXX AND CALCULATION OF DRAINAGE 
!            AREAS.  THE ELEMENT WITH THE HIGHEST ELEVATION WILL HAVE 
!            ORDER=1 AND THE ATIONS WILL OCCUR TO SUCCESSIVELY LOWER 
!            ELVATIONS HAVING HIGHER
!
! Modified by Tricia Stadnyk - September 2000
! Converted common blocks to modules and added dynamically allocated
! run-time arrays as part of Fortran 90 conversion.
!
!
!*************************************************************************

      use area1
      USE area2
	USE area3
      USE area17


!     REAL    :: dummy(999,999)  moved to area17 Apr. 10/02 nk.
      REAL(4) :: maxelv
	integer :: i,j,n


! TO CALCULATE STORAGE/DISCHARGE,MUST FIND DRAINAGE AREA ABOVE A
! DEFINE DA(I,J) AS INTEGER NO. OF SQUARE KILOMETERS
      do i=imin,imax
         do j=jmin,jmax
            dummy(i,j)=elv_2d(i,j)*1.0
         end do
      end do

! ELEVATIONS ARE ENTERED OAND STRED 
! THE FOLLOWING SEQUENCE WILL ORDER ARRAY  SUBROUTINE AMAXO FIND
! T INTEGER IN AN ARRAY OF INTEGERS
      n=0
      no=0


   12 maxelv=0.0

!       Find the highest elevation in the domain
        do i=imin,imax
           do j=jmin,jmax
              maxelv=max(dummy(i,j),maxelv)
           end do
        end do

        if(maxelv.eq.0.0) GO TO 13

        do i=imin,imax
          do j=jmin,jmax
            if(dummy(i,j).eq.maxelv)then
    5         n=n+1
!             add to total no of grids if there is an elv.
              yyy(n)=i
              xxx(n)=j
              rank_2d(i,j)=n
              dummy(i,j)=0
!             find the number of outlets
!             if a grid has zero area but has an elv, it is part of a watershed
              if(frac_2d(i,j).le.0.0)no=no+1
            endif
          end do
        end do

   10   CONTINUE


      GO TO 12


! INITIAL VALUE IS SET EQUAL ZERO,THEN START AT ORDER=1 AND SUM
! AREA GOING TO SUCCESSIVE LOWER ELVATIONS IN THE WATERSHED
! DA(I,J) IS THE DRAINAGE AREA ABOVE THE OUTLET OF ELEMENT(I,J)
! DA IS SQ. KM AND CAN BE  < 1.0
! DEFINE DA AND S =0 FOR ELEMENTS OUTSIDE THE WATERSHED
! S IS AN INDICATOR NE=1,E=2,SE=3,S=4,SW=5,W=6,NW=7,N=8. S IS TH
! ION OF FLOW OUTOF THE SQUARE GRID ELEMENT IN 8 POSSIBLE DIRECT


   13 na=n
      naa=n-no

      do n=1,naa
         i=yyy(n)
         j=xxx(n)
         da_2d(i,j)=step2*frac_2d(i,j)
!         if(da_2d(i,j).le.0.0.or.frac_2d(i,j).le.0.0)then
!           write(*,1012)i,j
!           write(39,1012)i,j
!         endif        
!         write(*,1011)n,i,j,da(i,j),i-1,i+1,j-1,j+1
        if(i.eq.1.or.j.eq.1.or.i.eq.imax.or.j.eq.jmax)then
!         this can happen if people did NOT leave a blank edge!!!
        else
          if(s_2d(i-1,j-1).eq.1)da_2d(i,j)=da_2d(i,j)+da_2d(i-1,j-1)
          if(s_2d(i  ,j-1).eq.2)da_2d(i,j)=da_2d(i,j)+da_2d(i  ,j-1)
          if(s_2d(i+1,j-1).eq.3)da_2d(i,j)=da_2d(i,j)+da_2d(i+1,j-1)
          if(s_2d(i+1,j  ).eq.4)da_2d(i,j)=da_2d(i,j)+da_2d(i+1,j  )
          if(s_2d(i+1,j+1).eq.5)da_2d(i,j)=da_2d(i,j)+da_2d(i+1,j+1)
          if(s_2d(i  ,j+1).eq.6)da_2d(i,j)=da_2d(i,j)+da_2d(i  ,j+1)
          if(s_2d(i-1,j+1).eq.7)da_2d(i,j)=da_2d(i,j)+da_2d(i-1,j+1)
          if(s_2d(i-1,j  ).eq.8)da_2d(i,j)=da_2d(i,j)+da_2d(i-1,j  )
        endif
      end do

! FORMATS:

 1001 format(20I4)
!1107 format(/' sampled elevations of the channel bottoms')
!1010 format(' MAXELV = ',I5/)
 1011 format(' ',3I5,F7.2,4I5)
 1012 format(' da or frac <= 0.0 in i,j=',2i5,'- please fix')


      RETURN

      END SUBROUTINE arrange
