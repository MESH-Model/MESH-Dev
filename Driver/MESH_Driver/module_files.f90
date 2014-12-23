!     
! File:   module_files.f90
! Author: gonzalo
!
! Created on November 17, 2014, 4:12 PM
!

MODULE model_files
    
!>******************************************************************************
!>  Author: Gonzalo Sapriza Azuri
!>  Description: handled input and output files in mesh if needed
!>  Only the parameter files :

!
!    fls(1) -> MESH_input_run_options.ini
!    fls(2) -> MESH_parameters_CLASS.ini
!    fls(3) -> MESH_parameters_hydrology.ini
!    fls(4) -> Basin_average_water_balance.csv
!    fls(5) -> Metrics_Out.txt
!>    If flag VARIABLEFILESFLAG is activated    
!>******************************************************************************
    type file
        character*250 :: name
        logical       :: isInit
        integer       :: unit
    end type
    
    type fl_ids
        character*500 :: pthIn     !Input absolute path for the param files
        character*500 :: pthOut    !Output absolute path for the output files
        type(file),dimension(:),allocatable :: fl
    end type

    contains
    
    subroutine Init_fls(flg,fld)
    !>**************************************************************************
    !> Description: Initialize fl_ids type reading information from fld file    
    !> that is readed when VARIABLEFILEFLAG is activated (=1)
    !>**************************************************************************        
        implicit none
        !Input
        character(len=*),intent(in):: fld !Name file that contains files names
        !Input-Output
        type(fl_ids),intent(inout) :: flg 
        !Internal
        integer  :: ios,i
        character*500 :: str1,str2,phtfl
        
        !> 1- Read file fld
        open(UNIT   = 271                , &
             FILE   = trim(adjustl(fld)) , &
             STATUS = "OLD"              , &
             IOSTAT = IOS                )
             
        print*,    trim(adjustl(fld)) 
        allocate(flg%fl(6))       
        
        do i = 1, 6
            
            flg%fl(i)%name   = 'sapo'
            flg%fl(i)%isInit = .false.
            flg%fl(i)%unit   = 0
        
        enddo
             
        do while (IOS == 0)

            read(271,*,iostat = IOS) str1,str2
            
            if     (trim(adjustl(str1)).eq.'pthIn')then
                
                flg%pthIn = trim(adjustl(str2))
            
            elseif (trim(adjustl(str1)).eq.'pthOut')then   
                
                flg%pthOut = trim(adjustl(str2))
                
            elseif (trim(adjustl(str1)).eq.'class' )then   
                
                phtfl = trim(adjustl(flg%pthIn))//trim(adjustl(str2))
                flg%fl(2)%name   = phtfl
                flg%fl(2)%isInit = .true.
                flg%fl(2)%unit   = 50

            elseif (trim(adjustl(str1)).eq.'hydro' )then   
                
                phtfl = trim(adjustl(flg%pthIn))//trim(adjustl(str2))
                flg%fl(3)%name   = phtfl
                flg%fl(3)%isInit = .true.
                flg%fl(3)%unit   = 23                
                                   
            elseif (trim(adjustl(str1)).eq.'run_option' )then   
                
                phtfl = trim(adjustl(flg%pthIn))//trim(adjustl(str2))
                flg%fl(1)%name   = phtfl
                flg%fl(1)%isInit = .true.
                flg%fl(1)%unit   = 53                
                
            elseif (trim(adjustl(str1)).eq.'metrics_out')then   
                
                phtfl = trim(adjustl(flg%pthOut))//trim(adjustl(str2))
                flg%fl(5)%name   = phtfl
                flg%fl(5)%isInit = .true.
                flg%fl(5)%unit   = 100          
                
            elseif (trim(adjustl(str1)).eq.'basin_wb')then   
                
                phtfl = trim(adjustl(flg%pthOut))//trim(adjustl(str2))
                flg%fl(4)%name   = phtfl
                flg%fl(4)%isInit = .true.
                flg%fl(4)%unit   = 900       
                               
            elseif (trim(adjustl(str1)).eq.'stream_flow')then   
                
                phtfl = trim(adjustl(flg%pthOut))//trim(adjustl(str2))
                flg%fl(6)%name   = phtfl
                flg%fl(6)%isInit = .true.
                flg%fl(6)%unit   = 70                   
                
            endif
        enddo
      
        close(271)     
        
    end subroutine Init_fls
    
END MODULE model_files
