module mask_subbasins


contains

subroutine get_subbasin(mskOut,  outlet, &
                        lon   ,  lat   , &
                        lon0  ,  lat0  , &
                        dylat ,  dxlon , &
                        rank  ,  next  , &
                        n     ,  m     )

    implicit none
    !Inputs
    integer :: rank(n,m),next(n,m)
    real    :: lon,lat,lon0,lat0,dylat,dxlon    
    integer :: n,m
    !Outputs
    integer,intent(out) :: mskOut(n,m)
    integer :: is,js,ip,jp
    integer :: outlet(4)

!f2py intent(in)   :: rank,next
!f2py intent(in)   :: lon,lat,lon0,lat0,dylat,dxlon 
!f2py intent(out)  :: mskOut
!f2py intent(out)  :: outlet
!f2py intent(hide) :: n,m

    mskOut = 0

    call get_ij(is,js,lon,lat,lon0,lat0,dylat,dxlon)
    !Output based on the lat lon
    mskOut(is,js) = 1
    !We add the contribution cell of the ouput based on the latlong

    if (next(is,js).eq.rank(is-1,js-1))then
        ip = is-1
        jp = js-1
    endif

    if(next(is,js).eq.rank(is-1,js))then
        ip = is-1
        jp = js
    endif
    
    if(next(is,js).eq.rank(is,js-1))then
        ip = is
        jp = js-1
    endif

    if(next(is,js).eq.rank(is+1,js-1))then
        ip = is+1
        jp = js-1
    endif

    if(next(is,js).eq.rank(is+1,js))then
        ip = is+1
        jp = js
    endif

    if(next(is,js).eq.rank(is-1,js+1))then
        ip = is-1
        jp = js+1
    endif

    if(next(is,js).eq.rank(is,js+1))then
        ip = is
        jp = js+1
    endif

    if(next(is,js).eq.rank(is+1,js+1))then
        ip = is+1
        jp = js+1
    endif

    outlet(1) = is
    outlet(2) = js
    outlet(3) = ip
    outlet(4) = jp

    mskOut(ip,jp) = 1
    
    call getloc(mskOut,is,js,rank,next,n,m)
    

end subroutine get_subbasin


subroutine get_ij(ii,jj,lon,lat,lon0,lat0,dylat,dxlon)

    implicit none
    !Inputs
    real,intent(in)     :: lon,lat,lon0,lat0,dylat,dxlon
    !Outputs 
    integer,intent(out) :: ii,jj    
    !Internals
    real    :: lx1, ncll1, ly1,mcll1
    integer :: ncllf_1,mcllf_1
    real    :: p11,q11

!f2py intent(in)  :: lon,lat,lon0,lat0,dylat,dxlon
!f2py intent(out) :: ii,jj
    
    lx1 = lat - lat0 
    ncll1 = lx1/dylat
    ncllf_1 = int(ncll1)
    p11 = 1.0e0 - (ncll1- ncllf_1)
    
    if (p11.gt.0.0e0)then                    
        ii = ncllf_1 + 1
    else 
        ii = ncllf_1                
    endif         

    ly1 = lon - lon0 
    mcll1 = ly1/dxlon
    mcllf_1 = int(mcll1)
    q11 = 1.0e0 - (mcll1- mcllf_1)

    if (q11.gt.0.0e0)then
        jj = mcllf_1 + 1
    else 
        jj = mcllf_1 
    endif  

end subroutine get_ij



recursive subroutine getloc(mskOut,is,js,rank,next,n,m)

    implicit none
    !Inputs
    integer :: rank(n,m),next(n,m)
    integer :: n,m
    integer :: is,js
    !Outputs
    integer,intent(out) :: mskOut(n,m)
    
    integer  :: nr_ctr,cnt
    logical  :: msk(n,m)    
    integer  :: ii,jj

    msk  = .false.

    where(next .eq. rank(is,js)) msk = .true.
    
    nr_ctr = count(msk)

    if (nr_ctr .gt. 0 ) then
        cnt = 0
        do ii = 1, n
            do jj = 1, m
                if (msk(ii,jj))then
                    mskOut(ii,jj) = 1 
                    call getloc(mskOut,ii,jj,rank,next,n,m)                    
                    cnt = cnt + 1
                    if (cnt .eq. nr_ctr)then
                        exit
                    endif
                endif
            enddo
        enddo
    endif

end subroutine getloc

subroutine writeMatrix(flIn,vals,n,m)

    implicit none
    !Inputs
    character*450, intent(in)  :: flIn
    real*8       , intent(in)  :: vals(n,m)

    !Internals
    integer   :: n,m
    integer   :: i,j
    character*30 :: rowfmt
!f2py intent(in)    :: flIn,vals
!f2py intent(hide)  :: n,m
    WRITE(rowfmt,'(A,I4,A)') '(',m,'(1X,F18.8))'

    open(unit     =  23                 , &
         file     = trim(adjustl(flIn)) , &
         status   = "old"               , &
         position = "append"            , &
         action   = "write"             )

    do i = 1, n
        write(23,fmt=rowfmt)((vals(i,j)),j=1,m)
    enddo
    close(23)


       
end subroutine writeMatrix

subroutine msk_climate_forcing(flIn,flOut,rnk,NA,n)

    implicit none
    !Inputs
    character*450, intent(in)  :: flIn,flOut
    integer, intent(in)        :: rnk(n),NA

    !Internals
    integer   :: n,timeM,i    
    real      :: vals(NA), vout(n)
    integer   :: IOS

!f2py intent(in)   :: flIn,flOut,rnk,NA
!f2py intent(hide) :: n

    open(unit   = 75                    , &
         file   = TRIM(ADJUSTL(flIn))   , &
         status = 'old'                 , &
         form   = 'unformatted'         , &
         action = 'read'                , &
         access = 'sequential'          , &
         IOSTAT =  IOS                  )
  
    open(unit   = 85                    , &
         file   = TRIM(ADJUSTL(flOut))  , &
         status = 'replace'             , &
         form   = 'unformatted'         , &
         action = 'write'               , &
         access = 'sequential'          )    



    do while (IOS == 0)

        read(75,iostat = IOS) timeM
        read(75,iostat = IOS) vals     
        

        if (IOS == 0)then
    
            do i = 1, n            
                 vout(i) = vals(rnk(i))            
            enddo
    
            write(85) timeM
            write(85) vout  

        endif
    enddo

    close(75)
    close(85)
    

end subroutine msk_climate_forcing

subroutine get_ntimesMeshSeq(flIn,tm)

        implicit none
        integer       , intent(out) :: tm
        character*450 , intent(in)  :: flIn
        integer :: timeM
        integer :: ios

!f2py intent(in)     :: flIn
!f2py intent(out)    :: tm

    open(unit   = 75                    , &
         file   = TRIM(ADJUSTL(flIn))   , &
         status = 'old'                 , &
         form   = 'unformatted'         , &
         action = 'read'                , &
         access = 'sequential'          , &
         IOSTAT =  IOS                  )



    tm = 0
    do while (IOS == 0)

        read(75,iostat = IOS) timeM
        read(75,iostat = IOS)      

        if (IOS == 0)then
            tm = tm + 1
        endif

    enddo
    
    close(75)
        
end subroutine get_ntimesMeshSeq

subroutine msk_initprogclass(flIn,flOut,rnk,NA,NTYPE,IGND,msk_lc,nlc,n)

    implicit none
    !Inputs
    character*450, intent(in)  :: flIn,flOut
    integer, intent(in)        :: rnk(n),msk_lc(nlc),NA
    integer, intent(in)        :: NTYPE,IGND
    !Internals
    integer   :: n,nlc,i,j,k
    integer   :: IOS

    real,dimension(:,:)  ,allocatable :: aux1
    real,dimension(:,:,:),allocatable :: aux2
    real,dimension(NA,NTYPE)  :: read1
    real, dimension(NA, NTYPE, 4) :: read2
    real, dimension(NA, NTYPE, IGND) :: read3



!f2py intent(in)   :: flIn,flOut,rnk,NA
!f2py intent(in)   :: NTYPE,IGND,msk_lc
!f2py intent(hide) :: n,nlc

    OPEN(UNIT   = 883                     , &
         FILE   = trim(adjustl(flIn))     , &
         STATUS = 'OLD'                   , &
         FORM   = 'unformatted'           , &
         ACTION = 'read'                  , &
         ACCESS = 'sequential'            , &
         IOSTAT = IOS                     )

    OPEN(UNIT   = 884                     , &
         FILE   = trim(adjustl(flOut))    , &
         STATUS = 'replace'               , &
         FORM   = 'unformatted'           , &
         ACTION = 'write'                 , &
         ACCESS = 'sequential'            , &
         IOSTAT = IOS                     )    

    do k = 1, 9
        read(883)  read1   !1
        allocate(aux1(n,nlc))
        do i = 1, nlc
            do j = 1, n            
                aux1(j,i) = read1(rnk(j),msk_lc(i))            
            enddo        
        enddo
        write(884) aux1   !1
        deallocate(aux1)
    enddo    



    read(883)  read3   !10
    allocate(aux2(n,nlc,IGND))
    do i = 1, nlc
        do j = 1, n 
            aux2(j,i,:) = read3(rnk(j),msk_lc(i),:)
        enddo
    enddo
    write(884) aux2   !1   
    deallocate(aux2)

    do k = 1, 2
        read(883)  read1   !1
        allocate(aux1(n,nlc))
        do i = 1, nlc
            do j = 1, n            
                aux1(j,i) = read1(rnk(j),msk_lc(i))            
            enddo        
        enddo
        write(884) aux1   !1
        deallocate(aux1)
    enddo    

    do k = 1, 2
        read(883)  read3   !10
        allocate(aux2(n,nlc,IGND))
        do i = 1, nlc
            do j = 1, n 
                aux2(j,i,:) = read3(rnk(j),msk_lc(i),:)
            enddo
        enddo
        write(884) aux2   !1   
        deallocate(aux2)
    enddo

    read(883) read1   !1
    allocate(aux1(n,nlc))
    do i = 1, nlc
        do j = 1, n            
            aux1(j,i) = read1(rnk(j),msk_lc(i))            
        enddo        
    enddo
    write(884) aux1   !1
    deallocate(aux1)


    read(883)  read2   !10
    allocate(aux2(n,nlc,4))
    do i = 1, nlc
        do j = 1, n 
            aux2(j,i,:) = read2(rnk(j),msk_lc(i),:)
        enddo
    enddo
    write(884) aux2   !1   
    deallocate(aux2)


    do k = 1, 3
        read(883)  read1   !1
        allocate(aux1(n,nlc))
        do i = 1, nlc
            do j = 1, n            
                aux1(j,i) = read1(rnk(j),msk_lc(i))            
            enddo        
        enddo
        write(884) aux1   !1
        deallocate(aux1)
    enddo  


    close(883)
    close(884)
    

end subroutine msk_initprogclass

subroutine renumerate(rnk_c,nxt_c,rnk,nxt,n,m)
    !Renumerate rank or nxt
    
    implicit none
    !Inputs
    integer, intent(in) :: rnk(n,m),nxt(n,m)    
    !Outputs
    integer, intent(out) :: rnk_c(n,m),nxt_c(n,m)

    !Internals
    integer  :: n,m,nn
    integer  :: i,j,k,mcount
    integer  :: ycount_new, xcount_new
    integer,dimension(n*m)::rank_1d,rank_1d_bis,next_1d,next_1d_bis

!f2py intent(in)   :: rnk,nxt
!f2py intent(out)  :: rnk_c,nxt_c
!f2py intent(hide) :: n,m

    nn = ycount_new*xcount_new
    ycount_new = n
    xcount_new = m
    
    rank_1d = 0    
    next_1d  = 0
    
    do j=1,ycount_new
        do i=1,xcount_new
            rank_1d((j-1)*xcount_new+i) = rnk(j,i)
            next_1d((j-1)*xcount_new+i) = nxt(j,i)
        end do
    end do
    
    
    !!!!  know the number of active points in the square
    mcount=0

    rank_1d_bis=0
    do i=1,ycount_new*xcount_new
        if(rank_1d(i)>0)then
            mcount = mcount + 1
        end if
    end do
    
    
    
    !!!reorder the ranks
    do i = 1, ycount_new*xcount_new
        k=0
        if (rank_1d(i)>0)then            
            do j = 1, ycount_new*xcount_new                
                if ((rank_1d(j)>0).and.(rank_1d(j)>rank_1d(i)))then
                    k=k+1
                end if

            end do
            
            rank_1d_bis(i) = mcount - k
            
        end if
        
    end do
    
    
    next_1d_bis = 0
    
    do i = 1, ycount_new*xcount_new
        if (rank_1d(i)>0) then
            do j = 1, ycount_new*xcount_new
                if(next_1d(i).eq.rank_1d(j))then
                    next_1d_bis(i) = rank_1d_bis(j)                    
                endif
            enddo
        endif
        write(12,*)rank_1d_bis(i),rank_1d(i),next_1d_bis(i),next_1d(i)
    enddo
    close(12)

    do j=1,ycount_new
        do i=1,xcount_new
            rnk_c(j,i) = rank_1d_bis((j-1)*xcount_new+i)
            nxt_c(j,i) = next_1d_bis((j-1)*xcount_new+i)
        end do
    end do    

    

end subroutine renumerate

end module mask_subbasins