!     
! File:   module_files.f90
! Author: gonzalo
!
! Created on November 17, 2014, 4:12 PM
!

MODULE model_files
    
!>******************************************************************************
!>  Athor: Gonzalo Sapriza Azuri
!>  Description: handled input and output files in mesh if needed
!>  Only the parameter files :
!>    1- MESH_parameters_CLASS.ini
!>    2- MESH_parameters_hydrology.ini
!>  and output files are modified
!>    If flag VARIABLEFILESFLAG is activated
!>******************************************************************************
    type fl_ids
        character*500 :: pthIn    !Input absolute path for the param files
        character*500 :: pthOut   !Output absolute path for the output files
        character*150 :: pmclass  !file name of class parameter
        character*150 :: pmhydro  !file name of hydrological parameters
        character*150 :: flOut1   !file output 1  
        character*150 :: flOut2   !file output 2
    end type
    
    contains
    
    subroutine Init_fls(fls,fld)
    !>**************************************************************************
    !> Description: Initialize fl_ids type reading information from fld file    
    !> that is readed when VARIABLEFILEFLAG is activated (=1)
    !>**************************************************************************        
        implicit none
        !Input
        character(len=*),intent(in):: fld !Name file that contains files names
        !Input-Output
        type(fl_ids),intent(inout) :: fls 
        !Internal
        integer  :: ios
        character*500 :: str1,str2
        
        !> 1- Read file fld
        open(UNIT   = 271                , &
             FILE   = trim(adjustl(fld)) , &
             STATUS = "OLD"              , &
             IOSTAT = IOS                )
             
        do while (IOS == 0)

            read(271,*,iostat = IOS) str1,str2
            
            if     (trim(adjustl(str1)).eq.'pthIn')then
                
                fls%pthIn = trim(adjustl(str2))
            
            elseif (trim(adjustl(str1)).eq.'pthOut')then   
                
                fls%pthOut = trim(adjustl(str2))
                
            elseif (trim(adjustl(str1)).eq.'class' )then   
                
                fls%pmclass = trim(adjustl(str2))

            elseif (trim(adjustl(str1)).eq.'hydro' )then   
                
                fls%pmhydro = trim(adjustl(str2))      
                
            elseif (trim(adjustl(str1)).eq.'metrics_out')then   
                
                fls%flOut1 = trim(adjustl(str2))            
                
            elseif (trim(adjustl(str1)).eq.'flOut2')then   
                
                fls%flOut2 = trim(adjustl(str2))                  
                
            endif
        enddo
      
        close(271)     
        
    end subroutine Init_fls
    
END MODULE model_files
