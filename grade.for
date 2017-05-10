      SUBROUTINE grade(slopemin,no_errors)

!*********************************************************************
! WRITTEN IN 1972!
!
! GRADEA - THIS SUBROUTINE CALCULATES THE slope OF THE CHANNEL AT THE
!          OUTLET OF EACH SQUARE ELEMENT
!
! Modified by Tricia Stadnyk - September 2000
! Converted common blocks to modules and added dynamically allocated
! run-time arrays as part of Fortran 90 conversion.
!
!
!*********************************************************************

      USE area1
      USE area2
	USE area3
	USE area17

      real  ::   slopemin
	integer :: no_errors,n,i,j,ind

! FOR METER CONTOURS elvCONV WILL BE 1.0
! FOR FT CONTOURS elvCONV WILL BE 0.305


!     This section code upgrade NK 11/09/04 

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        ind=s_2d(i,j)
!       ind= drainage directin 1-8
        if(ind.eq.0) GO TO 28

      GO TO (20,21,22,23,24,25,26,27),ind
   20   ch_length_2d(i,j)=al*1.4142
        slope_2d(i,j)=-(elv_2d(i+1,j+1)-elv_2d(i,j))/ch_length_2d(i,j)
!        slope(i,j)=-slope(i,j)
        
        next_2d(i,j)=rank_2d(i+1,j+1)
      GO TO 28
   21   ch_length_2d(i,j)=al
        slope_2d(i,j)=-(elv_2d(i  ,j+1)-elv_2d(i,j))/ch_length_2d(i,j)
!        slope(i,j)=-slope(i,j)
        
        next_2d(i,j)=rank_2d(i,j+1)
      GO TO 28
   22   ch_length_2d(i,j)=al*1.4142
        slope_2d(i,j)=-(elv_2d(i-1,j+1)-elv_2d(i,j))/ch_length_2d(i,j)
!        slope(i,j)=-slope(i,j)
        
        next_2d(i,j)=rank_2d(i-1,j+1)
      GO TO 28
   23   ch_length_2d(i,j)=al
        slope_2d(i,j)=-(elv_2d(i-1,j  )-elv_2d(i,j))/ch_length_2d(i,j)
!        slope(i,j)=-slope(i,j)
        
        next_2d(i,j)=rank_2d(i-1,j)
      GO TO 28
   24   ch_length_2d(i,j)=al*1.4142
        slope_2d(i,j)=-(elv_2d(i-1,j-1)-elv_2d(i,j))/ch_length_2d(i,j)
!        slope(i,j)=-slope(i,j)
        
        next_2d(i,j)=rank_2d(i-1,j-1)
      GO TO 28
   25   ch_length_2d(i,j)=al
        slope_2d(i,j)=-(elv_2d(i  ,j-1)-elv_2d(i,j))/ch_length_2d(i,j)
!        slope(i,j)=-slope(i,j)
        
        next_2d(i,j)=rank_2d(i,j-1)
      GO TO 28
   26   ch_length_2d(i,j)=al*1.4142
        slope_2d(i,j)=-(elv_2d(i+1,j-1)-elv_2d(i,j))/ch_length_2d(i,j)
!        slope(i,j)=-slope(i,j)
        
        next_2d(i,j)=rank_2d(i+1,j-1)
      GO TO 28
   27   ch_length_2d(i,j)=al
        slope_2d(i,j)=-(elv_2d(i+1,j  )-elv_2d(i,j))/ch_length_2d(i,j)
!        slope(i,j)=-slope(i,j)
        
        next_2d(i,j)=rank_2d(i+1,j)

   28 CONTINUE

      
      slope_2d(i,j)=amax1(slopemin,slope_2d(i,j),0.0000001)


      if(next_2d(i,j).le.n)then
	   no_errors=no_errors+1
!         write(*,7771)n,i,j,next(i,j)
         if(no_errors.eq.1)then
           write(39,7771)n,i,j,next_2d(i,j)
         else
           write(39,7772)n,i,j,next_2d(i,j)
         endif
         next_2d(i,j)=n+1
	   slope_2d(i,j)=-slope_2d(i,j)
      endif  

      end do


! FORMATS:

 7771 format(' ERROR: grid does not drain to lower grid'/
     *  ' next(i,j) set to n+1 <<<<<<FIX FIX FIX'/ 
     *  ' please fix - location n,row,col,next(i,j)/',4i5)
 7772 format(' please fix - location n,row,col,next(i,j)/',4i5) 


      RETURN

      END SUBROUTINE grade
