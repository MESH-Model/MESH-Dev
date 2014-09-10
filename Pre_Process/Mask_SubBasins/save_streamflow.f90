module save_streamflow
contains

subroutine tb0(flin,vals,n,m)

    implicit none
    !Inputs    
    character*450 :: flin
    real          :: vals(n,m)
    integer       :: n,m
    !Internals
    integer :: i,ih,j



!f2py intent(in):: flin
!f2py intent(in):: vals
!f2py intent(hide):: n,m

    open(unit     =  23                 , &
         file     = trim(adjustl(flin)) , &
         status   = "old"               , &
         position = "append"            , &
         action   = "write"             )

    do i = 1, n
        do ih = 1, 24
            write(23,*)((vals(i,j)/24.),j=1,m)
        enddo
    enddo
    close(23)

end subroutine tb0

subroutine meshfmt(flin,vals,n,m)

    implicit none
    !Inputs    
    character*450 :: flin
    real          :: vals(n,m)
    integer       :: n,m
    !Internals
    integer :: i,j



!f2py intent(in):: flin
!f2py intent(in):: vals
!f2py intent(hide):: n,m

    open(unit     =  23                 , &
         file     = trim(adjustl(flin)) , &
         status   = "old"               , &
         position = "append"            , &
         action   = "write"             )

    do i = 1, n
        write(23,*)((vals(i,j)),j=1,m)
    enddo
    close(23)


end subroutine meshfmt

end module save_streamflow