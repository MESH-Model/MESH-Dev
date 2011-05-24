subroutine check_parameters(wf_r2,m_c,nmtest,cp,hp,soil_por_max,soil_depth,s0,t_ice_lens)
!>
!>       March 23, 2010 - M.A. Mekonnen/B. Davidson/M. MacDonald
!>=======================================================================
!>
!>       The subroutine checks for parameter values specified in the 
!>       "MESH_parameters_hydrology.ini" and "MESH_parameters_CLASS.ini"  
!>       files to make sure that all the parameter values lie within the  
!>       specified limits.
!>
!>       The minimum and maximum values of each parameter are specified in 
!>       "minmax_parameters.txt" file. The parameters are stored as two 
!>       dimensional arrays (nrows x nmtest). nmtest represents the number 
!>       of GRUs.
!>
!>=======================================================================
!>
!>       wf_r2      -   River roughness factor 
!>       m_c        -   number of river classes
!>       nmtest     -   number of GRUs
!>       cp         -   CLASS parameters
!>       hp         -   hydrology parameters
!>
!>       parv       -   parameter values (two dimensional array)
!>       parflag    -   parameter flags (two dimensional array)
!>=======================================================================

use     mesh_input_module
use     flags

implicit none     

integer,parameter :: nrows  = 88   ! maximum number of rows	
integer,parameter :: nsl   = 3     ! number of soil layers	

integer     i,j,ib0,ib1,ib2,ib3,i4,i5,ir,m_c,nmtest
integer     parflag(nrows,nmtest)

real        total,percent
real        wf_r2(m_c),soil_por_max,soil_depth,s0,t_ice_lens
real        parv(nrows,nmtest),minlimit(nrows,nmtest),maxlimit(nrows,nmtest)

type(ClassParameters)     :: cp
type(HydrologyParameters) :: hp
!>=======================================================================

! Activate the parameters
parflag = 1

!>
!>***********************************************************************
!> Hydrology parameters
!>=======================================================================
!>

  ir = 1
  parv(ir,1) = 2             ! The parameter is currently not active
  parflag(ir,1:nmtest) = 0

  ir = ir + 1
  parv(ir,1) = 0             ! The parameter is currently not active
  parflag(ir,1:nmtest) = 0

  ir = ir + 1
  parv(ir,1) = 0             ! The parameter is currently not active
  parflag(ir,1:nmtest) = 0

! River roughness factor
  do i = 1,m_c
     ir = ir + 1
     parv(ir,1) = wf_r2(i)
     parflag(ir,2:nmtest) = 0 ! only wf_r2(1) is currently active
  enddo
  
  ir = ir + 1
  parv(ir,1) = soil_por_max
  parflag(ir,2:nmtest) = 0
  if(frozensoilinfilflag ==0)parflag(ir,1) = 0
  
  ir = ir + 1
  parv(ir,1) = soil_depth
  parflag(ir,2:nmtest) = 0
  if(frozensoilinfilflag ==0)parflag(ir,1) = 0

  ir = ir + 1
  parv(ir,1) = s0
  parflag(ir,2:nmtest) = 0
  if(frozensoilinfilflag ==0)parflag(ir,1) = 0

  ir = ir + 1
  parv(ir,1) = t_ice_lens
  parflag(ir,2:nmtest) = 0
  if(frozensoilinfilflag ==0)parflag(ir,1) = 0

!  do i = 5,indeppar
!     ir = ir + 1
!     parv(ir,1) = t0_acc(i-4)
!     parflag(ir,2:nmtest) = 0
!  enddo
  
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
!>***********************************************************************
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
!           check that we didn't calculate any bad numbers during the soil calculation
        if(total.gt.100.0.or.percent.gt.100.0) then
           print *, 'one of the soil parameters are greater than'
         print *, '100% in row ',ir,' please adjust'
           stop
        endif
        parv(ir+1,j) = total
        parv(ir+2,j) = percent
        parv(ir+3,j) = cp%orgmrow(1,j,i)
     enddo
  enddo

!>
!>***********************************************************************
!> Hydrology parameters
!>=======================================================================
!>
  ir = ir + nsl
  do j = 1,nmtest
     parv(ir+1,j) = hp%zsnlrow(1,j)   
     parv(ir+2,j) = hp%zplsrow(1,j)   
     parv(ir+3,j) = hp%zplgrow(1,j)
     parv(ir+4,j) = hp%frzcrow(1,j)
     if(frozensoilinfilflag ==0)parflag(ir+4,j) = 0
  enddo
  
!>
!>***********************************************************************
!> Class parameters
!>=======================================================================
!>
  ib2 = ir+4
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
!>***********************************************************************
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
  if(ir .lt. nrows)parflag(ir+1:nrows,:)= 0
  
  open(10,file='minmax_parameters.txt',status='old')
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

  call checkbound(parflag,parv,minlimit,maxlimit,nrows,nmtest)

end

!C-----------------------------------------------------------------------------------------
!C     The subroutine checks if the parameter value is within the limits
!C-----------------------------------------------------------------------------------------
      subroutine checkbound(parflag,parv,minlimit,maxlimit,nr,nc)
      
      implicit none
      
      integer nr,nc,parflag(nr,nc)
      real    parv(nr,nc),minlimit(nr,nc),maxlimit(nr,nc)
      
      integer i,j,ncount
      
      write(*,*)
      write(*,*)
      write(*,*)'Checking if parameter values lie within the specified ranges'
      write(*,*)
   
      ncount = 0
      
      do i = 1, nr
         do j = 1, nc
            if(parflag(i,j) .eq. 1)then
               if(parv(i,j).lt.minlimit(i,j).or.parv(i,j).gt.maxlimit(i,j)) then
                  ncount = ncount + 1
                  if(ncount == 1)then
                     write(*,*)"The following parameter values are out of range"
                     write(*,*)
                     write(*,10)"ROW","COLUMN","Specified value","Minimum value","Maximum value"
                     write(*,*)"-----------------------------------------------------------------"
                  endif
                  write(*,20)i,j,parv(i,j),minlimit(i,j),maxlimit(i,j)
               endif
             endif
         enddo
      enddo
      
      if(ncount > 0)then
         write(*,*)
         write(*,*)ncount, " parameter(s) out of range"
         write(*,*)
         write(*,*)'Adjust the parameter value(s) or modify the parameter limits'
         write(*,*)
         stop
      else
        write(*,*)'All parameter values lie within the specified ranges'
        write(*,*)
      endif
      
 10   format(2(a6,2x),3(a15,2x))
 20   format(2(i6,2x),3(f15.4,2x))
      
      end
