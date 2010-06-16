subroutine check_parameters(wf_r2,m_c,nmtest,cp,hp)
!>
!>*******************************************************************
!> Check for parameter values to make sure that every parameter value 
!> lies within the specified limits at minmax_parameters.txt file
!>
!> March 23, 2010 - M.A. Mekonnen/B. Davidson/M. MacDonald
!>=======================================================================

use     mesh_input_module

implicit none     

integer,parameter :: nrow  = 100   ! maximum number of rows	
integer,parameter :: nsl   = 3     ! number of soil layers	

integer i,j,ib0,ib1,ib2,ib3,i4,i5,ir,m_c,nmtest
real wf_r2(m_c),total,percent
real parv(nrow,nmtest),minlimit(nrow,nmtest),maxlimit(nrow,nmtest)
type(ClassParameters)     :: cp
type(HydrologyParameters) :: hp

!>
!>*******************************************************************
!> Hydrology parameters
!>=======================================================================
!>
  parv = -999
  ir = 1   
!  parv(ir,1) = thextra
  ir = ir + 1
!  parv(ir,1) = ice_index
  ir = ir + 1
!  parv(ir,1) = gwscale
  do i = 1,m_c
     ir = ir + 1
     parv(ir,1) = wf_r2(i)
  enddo
  
  ir = ir + 1
 ! parv(ir,1) = par1
  
  ir = ir + 1
 ! parv(ir,1) = par2

!>
!>*******************************************************************
!> Class parameters
!>=======================================================================
!>
  ib0 = ir
  do j = 1, nmtest
     parv(ir+1,j) = cp%drnrow (1,j)
     parv(ir+2,j) = cp%sdeprow(1,j)   
     parv(ir+3,j) = cp%farerow(1,j)   
     parv(ir+4,j) = cp%ddrow(1,j)   
     parv(ir+5,j) = cp%xslprow(1,j)   
     parv(ir+6,j) = cp%xdrow(1,j)   
     parv(ir+7,j) = cp%mannrow(1,j)   
     parv(ir+8,j) = cp%ksrow(1,j)   
  enddo

!>
!>*******************************************************************
!> Check the soil fractions
!>=======================================================================
!>
  ib1 = ir + 8
  do i = 1,nsl
     ir   = ib1 + (i-1)*3
     do j = 1,nmtest
!        adjust sand and clay
        total   = cp%sandrow(1,j,i)/(100.0-cp%orgmrow(1,j,i))*100.0
        percent = cp%clayrow(1,j,i)/(100.0-cp%orgmrow(1,j,i)-cp%sandrow(1,j,i))*100.0
!           check that we didn't calculated any bad numbers during the soil calculation
        if(total.gt.100.0.or.percent.gt.100.0) then
           print *, 'one of the soil parameters are greater than'
         print *, '100% in row ',ir,' please adjust'
           pause
           stop
        endif
        parv(ir+1,j) = total
        parv(ir+2,j) = percent
        parv(ir+3,j) = cp%orgmrow(1,j,i)
     enddo
  enddo

!>
!>*******************************************************************
!> Hydrology parameters
!>=======================================================================
!>
  ir = ir + nsl
  do j = 1,nmtest
     parv(ir+1,j) = hp%zsnlrow(1,j)   
     parv(ir+2,j) = hp%zplsrow(1,j)   
     parv(ir+3,j) = hp%zplgrow(1,j)
  enddo
  
!>
!>*******************************************************************
!> Class parameters
!>=======================================================================
!>
  ib2 = ir+3
  do i5 = 1,5
     ir = ib2 + (i5-1)*6
     do j = 1,nmtest
        parv(ir+1,j) = cp%lnz0row(1,j,i5)   
        parv(ir+2,j) = cp%alvcrow(1,j,i5)   
        parv(ir+3,j) = cp%alicrow(1,j,i5)
        if(i5 .lt. 5)then   !urban areas
           parv(ir+4,j) = cp%rsmnrow(1,j,i5)   
           parv(ir+5,j) = cp%vpdarow(1,j,i5)   
           parv(ir+6,j) = cp%psgarow(1,j,i5)
        endif
     enddo
  enddo

!>
!>*******************************************************************
!> Class parameters
!>=======================================================================
!>
  ib3 = ir + 3 !57
  do i4 = 1,4
     ir = ib3 + (i4-1)*7
     do j = 1,nmtest
        parv(ir+1,j) = cp%pamxrow(1,j,i4)   
        parv(ir+2,j) = cp%pamnrow(1,j,i4)   
        parv(ir+3,j) = cp%cmasrow(1,j,i4)   
        parv(ir+4,j) = cp%rootrow(1,j,i4)   
        parv(ir+5,j) = cp%qa50row(1,j,i4)
        parv(ir+6,j) = cp%vpdbrow(1,j,i4)
        parv(ir+7,j) = cp%psgbrow(1,j,i4)
     enddo
  enddo
  ir = ir + 7
  
  open(10,file='minmax_parameters.txt',status='old')
  minlimit=-999
  maxlimit=-999
  do i=1,ir
     read(10,*)
     if(i .gt. ib0)then
          read(10,*)(minlimit(i,j),j=1,nmtest)
          read(10,*)(maxlimit(i,j),j=1,nmtest)
     else
          read(10,*)minlimit(i,1)
          read(10,*)maxlimit(i,1)
     endif
          
  enddo
  close(10)

  call checkbound(parv,minlimit,maxlimit,nrow,nmtest)

end

!C-----------------------------------------------------------------------------------------
!C     The subroutine checks if the parameter value is within the limits
!C-----------------------------------------------------------------------------------------
      subroutine checkbound(parv,minlimit,maxlimit,nr,nc)
      
      implicit none
      
      integer nr,nc
      real    parv(nr,nc),minlimit(nr,nc),maxlimit(nr,nc)
      
      integer i,j,ncount
      
      write(*,*)
      write(*,*)
      write(*,*)'Checking if parameter values lie within the specified ranges'
      write(*,*)
      write(*,10)"ROW","COLUMN","Specified value","Minimum value","Maximum value"
      write(*,*)"-----------------------------------------------------------------"
      
      ncount = 0
      
      do i = 1, nr
         do j = 1, nc
            if(parv(i,j) .gt. -990)then
               if(parv(i,j).lt.minlimit(i,j).or.parv(i,j).gt.maxlimit(i,j)) then
                  ncount = ncount + 1
                  write(*,20)i,j,parv(i,j),minlimit(i,j),maxlimit(i,j)
               endif
             endif
         enddo
      enddo
      
      if(ncount > 0)then
         write(*,*)
         write(*,*)ncount, " parameter(s) out of range"
         write(*,*)'Adjust the parameter value(s) or modify the parameter limits'
         pause
         stop
      endif
      
 10   format(2(a6,2x),3(a15,2x))
 20   format(2(i6,2x),3(f15.4,2x))
      
      end
