      module ntcdf2BinMesh
      
      use netcdf
                 
      contains
      
!************************************************************
!************************************************************      
      subroutine readNetCDF(fname     , &
                            var_name  , &    
                            vals      , &
                            lat       , &
                            long      )
      !------------------------------------------------------                             
      ! This subroutine open and read the values from a netcdf
      ! file format. Need the file name, the name variable
      ! and return three arrays. 1) a matrix of values of 
      ! dimension, lat, long,time 2)arrays of lats, 
      !3) arrays of long. Only works for netcdf files with no
      !heights levels. 
      !------------------------------------------------------                              
      
      implicit none
    ! Inputs      
      character(len=*),intent(in) :: fname                   !ntcdf filename
      character(len=*),intent(in) :: var_name                !variable name 
    ! Outputs
      real,dimension(:,:,:),allocatable,intent(out) :: vals  !matrix of lat,lon,time output
      real,dimension(:),allocatable, intent(out):: lat,long  !lat and long arrays

    ! declare local variables
      INTEGER :: nc_id,var_id,ndim,nvar,nattr,unlim_id,fmt
      CHARACTER(LEN=15)            :: dname,longName,latName
      INTEGER                      :: dlength
      
      INTEGER :: ii,status,lo,la,le,ti,tstep
      REAL :: sf,ofs
      integer :: var_id1,var_id2
      
      !-----------------------------------------------
      
      CALL check(nf90_open(fname,nf90_nowrite,nc_id))
      CALL check(nf90_inquire(nc_id,ndim,nvar))

    ! take the dimension names and lengths
      DO ii=1,ndim
       CALL check(NF90_INQUIRE_DIMENSION(nc_id,ii,dname,len=dlength))
       !print*, dname
       SELECT CASE (TRIM(dname))
        CASE ('lon','LON','Lon','Longitude','longitude','LONGITUDE')
         lo=dlength
         longName = dname
        CASE ('lat','LAT','Lat','Latitude','latitude','LATITUDE')
         la=dlength
         latName = dname
        CASE ('time','Time','TIME')
        ti=dlength
        CASE ('tstep')
        tstep = dlength
        CASE DEFAULT
        PRINT*,' Error while reading dimensions....'
        PRINT*,' Some dimensions are missing.   '
        PRINT*,' The program is terminating....';STOP
       END SELECT
      END DO
      !print*, lo,la,ti,tstep
    ! allocate the matrix for reading data. The definition is
    ! var_dummy(nlon,lat,nlev,timesteps)
      ALLOCATE(vals(lo,la,tstep))
      allocate(lat(la),long(lo))
    
    ! Get Lats and longs
      CALL check(nf90_inq_varid(nc_id,TRIM(latName),var_id1))
      CALL check(nf90_get_var(nc_id   , &
                              var_id1 , &
                              lat       ))

      CALL check(nf90_inq_varid(nc_id,TRIM(longName),var_id2))
      CALL check(nf90_get_var(nc_id   , &
                              var_id2 , &
                              long      ))   
    ! Read all data
      CALL check(nf90_inq_varid(nc_id,TRIM(var_name),var_id))
      CALL check(nf90_get_var(nc_id              , &
                              var_id             , &
                              vals          ))!, &
                              !start=(/1,1,1/)     ,&
                              !count=(/lo,la,ti/) ))

    ! asking if there are the scale_factor and add_offset attributes
      status = nf90_get_att(nc_id,var_id,"scale_factor",sf)
      IF (status == -43) sf=1.0
      status = nf90_get_att(nc_id,var_id,"add_offset",ofs)
      IF (status == -43) ofs = 0.0
      !print*, 'Max=',maxval(vals),'Min:',minval(vals)
      vals=sf*vals+ofs
      !print*, 'Max=',maxval(vals),'Min:',minval(vals)
      call check(nf90_close(nc_id))
      
      !PRINT*,'Reading step ',ist
      !PRINT*,'Shape of netcdf file:',shape(vals)
      !PRINT*,'Shape of lats file:',shape(lat)
      !PRINT*,'Shape of longs file:',shape(long)
      
      END SUBROUTINE readNetCDF
!************************************************************
!************************************************************         
      subroutine check(status)
        
        integer, intent ( in) :: status

        if(status /= nf90_noerr) then 
          print *, trim(nf90_strerror(status))
          stop 2
        end if
      end subroutine check 
!************************************************************
!************************************************************      
      subroutine readR2C(vals    , fname     , &
                         rnk     , headread  , &
                         enddata , ntgt      , &
                         n       , m         )    
                         
      !------------------------------------------------------                             
      ! Read climate forcing data in r2c file  format
      ! Read in two steps first the header and then the values 
      ! per time step
      !------------------------------------------------------                               
      implicit none
      
      !Input variables  
      character(len=*),intent(in) :: fname        
      integer, intent(in)         :: rnk(n,m)
      logical   :: headread
      integer   :: ntgt,n,m
      
      !Outputs
      real,intent(out) :: vals(ntgt)   
      logical :: enddata
      
      !Internal variables
      integer      :: i,j, XCOUNT,YCOUNT
      character*15 :: dummy
      real         :: val_aux(n,m)
      !------------------------------------------------------
      
      enddata = .false.
      
      if (headread)then
        open(1501, file=fname, status ='old', action='read')
      
        do i=1,25
            read(1501,*)
        enddo
      
        read(1501,*) dummy,XCOUNT
        read(1501,*) dummy,YCOUNT
        
        do i=1,5
            read(1501,*) dummy
        enddo
        
      else        
        read(1501, *, END=999) 
        
        do i = 1,n            
            READ (1501, *, END=999) (val_aux(I,J),J=1,m)
        end do                       
        
        read(1501, *, END=999) !:EndFrame line
            
        
        do j =1, m
            do i = 1, n
                if (rnk(i,j).gt.0)then
                    vals(rnk(i,j))= val_aux(i,j)
                endif
            enddo 
        enddo
      endif
      
      return
      
999 enddata = .TRUE.
      if (enddata)then
        close(1501)
      endif
      
      end subroutine readR2C

!************************************************************
!************************************************************
      subroutine writefl(unitt  ,  closee  , &
                         flOut  ,  openn   , &
                         vall   ,  itime   , &
                         ntgt   ,  ntime   , &
                         frmtOut           )
                         
      !------------------------------------------------------                             
      !This subroutine carry out the file writing in several 
      !outputs formats.       
      !------------------------------------------------------                               
      implicit none
      !Input variables
      real           :: vall(ntime,ntgt)      
      integer        :: unitt
      logical        :: closee,openn
      character*450  :: flOut      
      integer        :: ntgt,ntime
      !Internal variables
      integer :: i,itime,j
      character*15  :: frmtOut
      
!f2py inent(in)    :: unitt,closee
!f2py inent(in)    :: nameP,openn ,vall
!f2py intent(hide) :: ntgt,ntime
      
      
      !--------------------------------------------       
       if (openn)then 
          select case(TRIM(ADJUSTL(frmtOut)))
            case ('seq','sequential')
              
              open(UNIT   = unitt                , &
                   FILE   = TRIM(ADJUSTL(flOut)) , &
                   STATUS = 'REPLACE'            , &
                   FORM   = 'UNFORMATTED'        , &
                   ACCESS = 'SEQUENTIAL'           ) 
               
            case('direct')
            
              open(UNIT   = unitt                , &
                   FILE   = TRIM(ADJUSTL(flOut)) , &
                   STATUS = 'REPLACE'            , &
                   FORM   = 'UNFORMATTED'        , &
                   ACCESS = 'DIRECT'             ) 
            
            case('ascii')
            
              open(UNIT   = unitt                , &
                   FILE   = TRIM(ADJUSTL(flOut)) , &
                   STATUS = 'REPLACE'            )

            case('netcdf')
                print*, 'Not implemented yet'
            
            case default
                print*, 'bad format output'
                stop
          end select 
            
       endif       
       
       if (closee)then            
           close(unitt)                   
       endif
              
       if (.not.(openn) .and. .not.(closee))then
            
            select case(TRIM(ADJUSTL(frmtOut)))
                case('seq','sequential')
                    
                    do i=1, ntime
                        
                        write(unitt) itime + i
                        write(unitt) vall(i,:)
                    enddo
                    
                case('direct')
                    print*, 'Not implemented yet'
                case('ascii')
                    do j =1,ntime
                        write(unitt,*)(vall(j,i),i=1,ntgt)
                    enddo
                    
                case('netcdf')
                
                    print*, 'Not implemented yet'
                case default
                    print*, 'bad format output'
                    stop
            end select                
           
       endif
        
       end subroutine writefl
!************************************************************
!************************************************************              
      subroutine search_index(vin1,vin2,vout,n1,n2)
      
      implicit none
      
      real    :: vin1(n1),vin2(n2)
      integer :: vout(n2)
      integer :: n1,n2,i,j,k
      
!f2py intent(in)   :: vin1
!f2py intent(in)   :: vin2
!f2py intent(out)  :: vout      
!f2py intent(hide) :: n1
!f2py intent(hide) :: n2      

      do i=1,n2
        do j=1,n1
          if (vin2(i).eq.vin1(j))then
            vout(i) = j
            exit
          endif
        enddo
      enddo
      
      
      end subroutine search_index
!************************************************************
!************************************************************
      subroutine msk_latlon(lats   ,  lons   , &
                            lat0   ,  lat1   , &
                            lon0   ,  lon1   , &
                            ilat   ,  ilon   , &
                            latMsk ,  lonMsk , &
                            n      ,  m      )
       !------------------------------------------------------                             
       !
       !       
       !------------------------------------------------------
       implicit none
       !Inputs
       real, intent(in)  :: lats(n),lons(m)
       real, intent(in)  :: lat0,lat1,lon0,lon1
       !Outputs       
       real, intent(out),allocatable :: latMsk(:),lonMsk(:)
       integer, intent(out),allocatable :: ilat(:),ilon(:) 
       !Internals
       logical  :: v_lat(n),v_lon(m)
       integer  :: nlat, nlon
       integer  :: n,m
       
       integer ::i
       !------------------------------------------------------
                
     
        do i=1,n
            if (((lats(i).le.lat1) .and. &
                 (lats(i).ge.lat0)))then
                
                v_lat(i) = .true.
            
            else
                v_lat(i) = .false.
            endif
        enddo        
        nlat = count(v_lat)   
        


        do i=1,m
            if (((lons(i).le.lon1) .and. &
                 (lons(i).ge.lon0)))then
                v_lon(i) = .true.
            else
                v_lon(i) = .false.
            endif
            
        enddo        
        nlon = count(v_lon)                
        
        allocate(ilat(nlat)   ,&
                 ilon(nlon)   ,&
                 latMsk(nlat) ,&                 
                 lonMsk(nlon) )
        
        latMsk = pack(lats,v_lat)
        lonMsk = pack(lons,v_lon)

        call search_index(lats, latMsk, ilat, n, nlat)
        call search_index(lons, lonMsk, ilon, m, nlon)    
       
                             
       end subroutine msk_latlon
!************************************************************
!************************************************************          
      subroutine GetValLoc(valOut , valIn  , & 
                           lat    , lon    , &
                           lat0   , lon0   , &
                           dx     , dy     , &
                           msk    , rnk    , &
                           nlat   , nlon   , &
                           ntime  , ntgt   , &
                           n      , m      )
        
        implicit none     
       !------------------------------------------------------                             
       !
       !       
       !------------------------------------------------------          
        
        !Input variables
        real, intent(in)      :: valIn(nlon,nlat,ntime)
        real, intent(in)      :: lat(nlat),lon(nlon)
        real, intent(in)      :: lat0,lon0,dx,dy
        logical, intent(in)   :: msk(n,m)
        integer, intent(in)   :: rnk(n,m) 
        integer, intent(in)   :: ntgt
        
        integer :: nlat,nlon,n,m,ntime
        
        !Output variables
        real,dimension(:,:),allocatable, intent (out) :: valOut
                            
        !Internal variables
        integer   :: i,j,tgt
        real      :: x0,x1,y0,y1,xx,yy
        
        real      :: wlat0,wlon0
        
        real      :: lx1,ly1,dxlat,dylon
        real      :: ncll1,p11,mcll1,q11
       
        integer   :: ncllf_1,mcllf_1
        integer   :: ii,jj,k
        
        real      :: lat_w0,lat_w1,lon_w0,lon_w1
        real      :: a1,a2,a3,a4

!f2py inent(in)    :: valIn,lat,lon
!f2py inent(in)    :: lat0,lon0,dx,msk,ntgt
!f2py intent(hide) :: ntime,n,m,nlat,nlon
!f2py inent(out)   :: valOut
       
       !------------------------------------------------------  

        allocate(valOut(ntime,ntgt))
        !print*, lat0,lon0,dx,n,m
        
        dxlat = abs(lat(1)-lat(2))
        dylon = abs(lon(1)-lon(2))
        
        wlat0 = lat(1) - dxlat*0.5
        wlon0 = lon(1) - dylon*0.5
        
        !write(11,*)wlat0,wlon0,dxlat,dylon
        
        !tgt = 1

        do i = 1,n
            do j = 1,m
                if (msk(i,j))then
                    
                    !write(11,*)i,j,msk(i,j)

                    x0 = lat0 +(i-1)*dx 
                    x1 = lat0 + i*dx
                    xx = (x0+x1)*0.5e0
                    y0 = lon0 +(j-1)*dy
                    y1 = lon0 + j*dy 
                    yy = (y0+y1)*0.5e0
                    
                    lx1 = xx - wlat0 
                    ncll1 = lx1/dxlat
                    ncllf_1 = int(ncll1)
                    p11 = 1.0e0 - (ncll1- ncllf_1)
                    
                    if (p11.gt.0.0e0)then                    
                        ii = ncllf_1 + 1
                    else 
                        ii = ncllf_1                
                    endif 
                    
                    !write(11,*) 'lat',xx,wlat0,ncll1,ii

                    ly1 = yy - wlon0 
                    mcll1 = ly1/dylon
                    mcllf_1 = int(mcll1)
                    q11 = 1.0e0 - (mcll1- mcllf_1)

                    if (q11.gt.0.0e0)then
                        jj = mcllf_1 + 1
                    else 
                        jj = mcllf_1 
                    endif  
                    
                    !write(11,*) 'long',yy,wlon0,mcll1,jj

                    ! check relative position of the small
                    ! grid related to the big grid
                    
                    lat_w0 = lat(ii) - dxlat*0.5e0
                    lat_w1 = lat(ii) + dxlat*0.5e0
                    
                    lon_w0 = lon(jj) - dylon*0.5e0
                    lon_w1 = lon(jj) + dylon*0.5e0
                    !write(11,*)lat(ii),lon(jj)
                    !write(11,*) lat_w0,x0,lon_w0,y0,lat_w1,x1,lon_w1,y1
                    
                    tgt = rnk(i,j)
                    
                    ! 1) Condition 1 Inside
                    if ((lat_w0 .le. x0 ).and. &
                        (lon_w0 .le. y0 ).and. & 
                        (lat_w1 .ge. x1 ).and. &
                        (lon_w1 .ge. y1 ))  then
                        
                        !write(11,*) '1) Condition 1 Inside'  
                        !write(11,*) 'long',yy,lon(jj),jj
                        !write(11,*) 'lat',xx,lat(ii),ii                        
                        !write(11,*) 'mask',i,j,tgt
                        
                        do k =1,ntime
                            
                            
                            valOut(k,tgt) = valIn(jj,ii,k)
                            
                            ! if(valOut(k,tgt).gt.1.e4)then
                                
                              !write(27,*)valOut(k,tgt)
                              !write(27,*) jj,ii,k,tgt,i,j
                              !write(27,*)lat(ii),lon(jj)
                                 !write(27,*) lat_w0,x0,lon_w0,y0, &
 !                                            lat_w1,x1,lon_w1,y1
                                 !close(27)
                                ! STOP
                                
                            ! endif
                            
                        enddo
                    ! 2) Condition 2  out North side
                    elseif ((lat_w0 .lt. x0 ).and. &
                            (lon_w0 .le. y0 ).and. & 
                            (lat_w1 .lt. x1 ).and. &
                            (lon_w1 .ge. y1 ).and. &
                            (lat_w1 .gt. x0 ))  then
                            
                        !write(11,*) '2) Condition 2  out North side'    
                        
                        a1 = dy*(x1-lat_w1)
                        a2 = dy*dx - a1
                        
                        !write(11,*) 'lat North',lon(jj),lat(ii-1)
                                                
                        
                        do k =1,ntime
                            
                            valOut(k,tgt) = (valIn(jj  ,ii  ,k)*a2   + &
                                             valIn(jj  ,ii+1,k)*a1) / & 
                                            (a1+a2)
                        enddo                
                    ! 3) Condition 3  out NorthWest corner 
                    elseif ((lat_w0 .lt. x0 ).and. &
                            (lon_w0 .gt. y0 ).and. & 
                            (lat_w1 .lt. x1 ).and. &
                            (lon_w1 .gt. y1 ).and. &
                            (lon_w0 .lt. y1 ).and. &
                            (lat_w1 .gt. x0 ))  then
                            
                        !write(11,*) '3) Condition 3  out NorthWest side'     
                        
                        a1 = (lon_w0-y0)*(lat_w1-x0)
                        a2 = (lon_w0-y0)*(x1-lat_w1)
                        a3 = (y1-lon_w0)*(x1-lat_w1)
                        a4 = (y1-lon_w0)*(lat_w1-x0)
                        
                        do k =1,ntime
                            valOut(k,tgt) = (valIn(jj  ,ii  ,k)*a4   + &
                                             valIn(jj  ,ii+1,k)*a3   + & 
                                             valIn(jj-1,ii+1,k)*a2   + & 
                                             valIn(jj-1,ii  ,k)*a1)  / & 
                                            (a1+a2+a3+a4)  
                        enddo                 
                    ! 4) Condition 4  out NorthEast corner 
                    elseif ((lat_w0 .lt. x0 ).and. &
                            (lon_w0 .lt. y0 ).and. & 
                            (lat_w1 .lt. x1 ).and. &
                            (lon_w1 .lt. y1 ).and. &
                            (lon_w1 .gt. y0 ).and. &
                            (lat_w1 .gt. x0 ))  then
                        
                        !write(11,*)'4) Condition 4  out NorthEast corner'      
                        
                        a1 = (lon_w1-y0)*(x1-lat_w1)
                        a2 = (y1-lon_w1)*(x1-lat_w1)
                        a3 = (y1-lon_w1)*(lat_w1-x0)
                        a4 = (lon_w1-y0)*(lat_w1-x0)
                        
                        do k =1,ntime
                            valOut(k,tgt) = (valIn(jj  ,ii  ,k)*a4   + &
                                             valIn(jj+1,ii  ,k)*a3   + & 
                                             valIn(jj+1,ii+1,k)*a2   + & 
                                             valIn(jj  ,ii+1,k)*a1)  / & 
                                            (a1+a2+a3+a4)                                         
                        enddo                 
                    ! 5) Condition 5  out East side
                    elseif ((lat_w0 .le. x0 ).and. &
                            (lon_w0 .lt. y0 ).and. & 
                            (lat_w1 .ge. x1 ).and. &
                            (lon_w1 .lt. y1 ).and. &
                            (lon_w1 .gt. y0 ))  then
                        
                        !write(11,*)'! 5) Condition 5  out East side'     
                        
                        a1 = (y1-lon_w1)*dx
                        a2 = dx*dy -a1
                        
                        do k =1,ntime
                            valOut(k,tgt) = (valIn(jj+1,ii  ,k)*a1   + &
                                             valIn(jj  ,ii  ,k)*a2)  / & 
                                            (a1+a2)         
                        enddo
                    ! 6) Condition 6  out SouthEast corner 
                    elseif ((lat_w0 .gt. x0 ).and. &
                            (lon_w0 .lt. y0 ).and. & 
                            (lat_w1 .gt. x1 ).and. &
                            (lon_w1 .lt. y1 ).and. &
                            (lat_w0 .lt. x1 ).and. &
                            (lon_w1 .gt. y0 ))  then
                        
                        !write(11,*)'6)Condition 6  out SouthEast corner'       
                        
                        a1 = (y1-lon_w1)*(x1-lat_w0 )
                        a2 = (y1-lon_w1)*(lat_w0-x0)
                        a3 = (lon_w1-y0)*(lat_w0-x0)
                        a4 = (lon_w1-y0)*(x1-lat_w0)
                        
                        do k =1,ntime                        
                            valOut(k,tgt) = (valIn(jj  ,ii  ,k)*a4   + &
                                             valIn(jj  ,ii-1,k)*a3   + & 
                                             valIn(jj+1,ii-1,k)*a2   + & 
                                             valIn(jj+1,ii  ,k)*a1)  / & 
                                             (a1+a2+a3+a4)      
                        enddo
                    ! 7) Condition 7  out South side
                    elseif ((lat_w0 .gt. x0 ).and. &
                            (lon_w0 .le. y0 ).and. & 
                            (lat_w1 .gt. x1 ).and. &
                            (lon_w1 .ge. y1 ).and. &
                            (lat_w0 .lt. x1 ))  then
                            
                        !write(11,*)'Condition 7  out South side'    
                        
                        a1 = (lat_w0-x0)*dy
                        a2 = dx*dy -a1

                        do k =1,ntime 
                            valOut(k,tgt) = (valIn(jj  ,ii-1,k)*a1   + &
                                             valIn(jj  ,ii  ,k)*a2)  / & 
                                            (a1+a2)    
                        enddo
                    ! 8) Condition 8  out SouthWest corner 
                    elseif ((lat_w0 .gt. x0 ).and. &
                            (lon_w0 .gt. y0 ).and. & 
                            (lat_w1 .gt. x1 ).and. &
                            (lon_w1 .gt. y1 ).and. &
                            (lon_w0 .lt. y1 ).and. &
                            (lat_w0 .lt. x1 ))  then
                            
                        !write(11,*)'Condition 8  out SouthWest corner'    
                        
                        a1 = (lon_w0-y0)*(x1-lat_w0)
                        a2 = (lon_w0-y0)*(lat_w0-x0)
                        a3 = (y1-lon_w0)*(lat_w0-x0)
                        a4 = (y1-lon_w0)*(x1-lat_w0)
                        
                        do k =1,ntime
                            valOut(k,tgt) = (valIn(jj  ,ii  ,k)*a4   + &
                                             valIn(jj  ,ii-1,k)*a3   + & 
                                             valIn(jj-1,ii-1,k)*a2   + & 
                                             valIn(jj-1,ii  ,k)*a1)  / & 
                                             (a1+a2+a3+a4)   
                        enddo
                    ! 9) Condition 9  out West side
                    elseif ((lat_w0 .le. x0 ).and. &
                            (lon_w0 .gt. y0 ).and. & 
                            (lat_w1 .ge. x1 ).and. &
                            (lon_w1 .gt. y1 ).and. &
                            (lon_w0 .lt. y1))   then
                            
                        !write(11,*)'Condition 9 out West corner'    
                        
                        a1 = (lon_w0-y0)*dx
                        a2 = dx*dy -a1
                        
                        do k =1,ntime                        
                            valOut(k,tgt) = (valIn(jj-1,ii  ,k)*a1   + &
                                             valIn(jj  ,ii  ,k)*a2)  / & 
                                            (a1+a2)                                            
                        enddo
                    
                    else
                        !write(43,*) 'something wrong no condition', &
                        !            'no condition reached', ii,jj,i,j
                        stop
                    endif

                    !write(43,*) 'tgt:' ,tgt
                endif
                
            enddo
        enddo
        
       !close(27)                     
       !close(43)
       !close(11)
       end subroutine GetValLoc
!************************************************************
!************************************************************
      
       end module ntcdf2BinMesh