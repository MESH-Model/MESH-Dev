           program watclass_forcing_distribution

c    This program takes point data and distributes it across the basin.
c
c    version 1.0, by Bruce Davison - June 2003
c
c notes:
c Version 1.0
c 1. This program takes half-hourly or hourly data.
c 1.1) Precip must be hourly due to the fact that "hourly.for" takes an average
c      of the half-hourly data, which is inappropriate for precip in mm.
c 2. Half-hourly data is averaged to hourly values.
c 3. More than one station can be used to distribute.
c 4. Currently distributes all WATCLASS forcing variables.
c 5. When changing basins, change the format statements.
c        
c Necessary files: 
c watflood.shd - shed file for rows and columns in the basin, used to set arrays and loops
c start_day.txt - text file to read in the start day, used for file names
c forcing files
c
c Variables
c ih = interval hour (only works for 1 or 0.5)
c nh = number of hours
c ng = number of guages or stations
c gs = grid size (km)
c b_row = basin rows
c b_col = basin columns
c xsta = station location (longitude and column)
c ysta = station location (latitude and row)
c
      integer i,j,n,hour,gru,nh,gs,b_row,b_col
      integer flnumber,ALLOC_ERR

c measured point data variables
      real, allocatable::insw_pt_meas  (:,:) ! hourly or half-hourly
      real, allocatable::hourly_insw   (:,:) ! hourly
      real, allocatable::inlw_pt_meas  (:,:)
      real, allocatable::hourly_inlw   (:,:)
      real, allocatable::wind_pt_meas  (:,:)
      real, allocatable::hourly_wind   (:,:)
      real, allocatable::pres_pt_meas  (:,:)
      real, allocatable::hourly_pres   (:,:)
      real, allocatable::hum_pt_meas   (:,:)
      real, allocatable::hourly_hum    (:,:)
      real, allocatable::tempr_pt_meas (:,:)
      real, allocatable::hourly_tempr  (:,:)
      real, allocatable::precip_pt_meas(:,:)
      real, allocatable::hourly_precip (:,:)

c distributed data variables
      real*8, allocatable::insw_dist  (:,:,:)
      real*8, allocatable::inlw_dist  (:,:,:)
      real*8, allocatable::wind_dist  (:,:,:)
      real*8, allocatable::pres_dist  (:,:,:)
      real*8, allocatable::hum_dist   (:,:,:)
      real*8, allocatable::tempr_dist (:,:,:)
      real*8, allocatable::precip_dist(:,:,:)

      character cjunk*30,start_day*6
      real rjunk,ih,total_precip
      real, allocatable::xsta(:)
      real, allocatable::ysta(:)
      real*8, allocatable::w(:,:,:)

c ***************************************
c take point measurements and distribute
c ***************************************

cbjd open start_day.txt to read in the start date	
	open(40,file='start_day.txt',
     +     status='old',form='formatted',access='sequential')

      read(40,*) start_day

cbjd open the shed file to read # of basin rows and columns	
	open(50,file='watflood.shd',
     +     status='old',form='formatted',access='sequential')

      read(50,*) cjunk
      read(50,*) cjunk
      read(50,*) cjunk
      read(50,*) b_row,b_col

      close(50)

cbjd open the point data files	
	open(51,file=start_day//'.sag',
     +     status='old',form='formatted',access='sequential')
	open(52,file=start_day//'.lag',
     +     status='old',form='formatted',access='sequential')
	open(53,file=start_day//'.wag',
     +     status='old',form='formatted',access='sequential')
	open(54,file=start_day//'.pag',
     +     status='old',form='formatted',access='sequential')
	open(55,file=start_day//'.hag',
     +     status='old',form='formatted',access='sequential')
      open(56,file=start_day//'.tag',
     +     status='old',form='formatted',access='sequential')
      open(57,file=start_day//'.rag',
     +     status='old',form='formatted',access='sequential')

c**********************************************************
c INCOMING SHORTWAVE RADIATION DISTRIBUTION
c**********************************************************
cbjd read station specific information
      read(51,*) gs,iymin,iymax,jxmin,jxmax
      read(51,*) ng,nh,ih

      allocate(hourly_insw(nh,ng))
      allocate(insw_pt_meas(int(nh/ih),ng))
      allocate(insw_dist(b_row,b_col,nh))
      allocate(xsta(ng))
      allocate(ysta(ng))
      allocate(w(b_row,b_col,ng))

      do i=1,ng
         read(51,'(2f5.0)') ysta(i),xsta(i)
         ysta(i)=int((real(ysta(i))-real(iymin))/gs+1.0)
         xsta(i)=int((real(xsta(i))-real(jxmin))/gs+1.0)
      end do

      do m=1,int(nh/ih) !hour
        read (51,*,END=100) (insw_pt_meas(m,n), n=1,ng)
      continue
      enddo

100   continue

cbjd close point data file
      close(51)

c     get weightings
      call weight(ng,xsta,ysta,b_col,b_row,w)

c     make sure the data is hourly
      call hourly(ih,nh,ng,insw_pt_meas,hourly_insw)

c     distribute based on weightings   
      call distr(nh,b_col,b_row,ng,w,insw_dist,hourly_insw) 

      deallocate(hourly_insw,insw_pt_meas,
     +           xsta,ysta,w, STAT = ALLOC_ERR)

c**********************************************************
c INCOMING LONGWAVE DISTRIBUTION
c**********************************************************
cbjd read station specific information
      read(52,*) gs,iymin,iymax,jxmin,jxmax
      read(52,*) ng,nh,ih

      allocate(hourly_inlw(nh,ng))
      allocate(inlw_pt_meas(int(nh/ih),ng))
      allocate(inlw_dist(b_row,b_col,nh))
      allocate(xsta(ng))
      allocate(ysta(ng))
      allocate(w(b_row,b_col,ng))

      do i=1,ng
         read(52,'(2f5.0)') ysta(i),xsta(i)
         ysta(i)=int((real(ysta(i))-real(iymin))/gs+1.0)
         xsta(i)=int((real(xsta(i))-real(jxmin))/gs+1.0)
      end do

      do m=1,int(nh/ih) !hour
        read (52,*,END=200) (inlw_pt_meas(m,n), n=1,ng)
      continue
      enddo

200   continue

cbjd close point data file
      close(52)

c     get weightings
      call weight(ng,xsta,ysta,b_col,b_row,w)

c     make sure the data is hourly
      call hourly(ih,nh,ng,inlw_pt_meas,hourly_inlw)

c     distribute based on weightings   
      call distr(nh,b_col,b_row,ng,w,inlw_dist,hourly_inlw) 

      deallocate(hourly_inlw,inlw_pt_meas,
     +           xsta,ysta,w, STAT = ALLOC_ERR)

c**********************************************************
c WIND DISTRIBUTION
c**********************************************************
cbjd read station specific information
      read(53,*) gs,iymin,iymax,jxmin,jxmax
      read(53,*) ng,nh,ih

      allocate(hourly_wind(nh,ng))
      allocate(wind_pt_meas(int(nh/ih),ng))
      allocate(wind_dist(b_row,b_col,nh))
      allocate(xsta(ng))
      allocate(ysta(ng))
      allocate(w(b_row,b_col,ng))

      do i=1,ng
         read(53,'(2f5.0)') ysta(i),xsta(i)
         ysta(i)=int((real(ysta(i))-real(iymin))/gs+1.0)
         xsta(i)=int((real(xsta(i))-real(jxmin))/gs+1.0)
      end do

      do m=1,int(nh/ih) !hour
        read (53,*,END=300) (wind_pt_meas(m,n), n=1,ng)
      continue
      enddo

300   continue

cbjd close point data file
      close(53)

c     get weightings
      call weight(ng,xsta,ysta,b_col,b_row,w)

c     make sure the data is hourly
      call hourly(ih,nh,ng,wind_pt_meas,hourly_wind)

c     distribute based on weightings   
      call distr(nh,b_col,b_row,ng,w,wind_dist,hourly_wind) 

      deallocate(hourly_wind,wind_pt_meas,
     +           xsta,ysta,w, STAT = ALLOC_ERR)

c**********************************************************
c PRESSURE DISTRIBUTION
c**********************************************************
cbjd read station specific information
      read(54,*) gs,iymin,iymax,jxmin,jxmax
      read(54,*) ng,nh,ih

      allocate(hourly_pres(nh,ng))
      allocate(pres_pt_meas(int(nh/ih),ng))
      allocate(pres_dist(b_row,b_col,nh))
      allocate(xsta(ng))
      allocate(ysta(ng))
      allocate(w(b_row,b_col,ng))

      do i=1,ng
         read(54,'(2f5.0)') ysta(i),xsta(i)
         ysta(i)=int((real(ysta(i))-real(iymin))/gs+1.0)
         xsta(i)=int((real(xsta(i))-real(jxmin))/gs+1.0)
      end do

      do m=1,int(nh/ih) !hour
        read (54,*,END=400) (pres_pt_meas(m,n), n=1,ng)
      continue
      enddo

400   continue

cbjd close point data file
      close(54)

c     get weightings
      call weight(ng,xsta,ysta,b_col,b_row,w)

c     make sure the data is hourly
      call hourly(ih,nh,ng,pres_pt_meas,hourly_pres)

c     distribute based on weightings   
      call distr(nh,b_col,b_row,ng,w,pres_dist,hourly_pres) 

      deallocate(hourly_pres,pres_pt_meas,
     +           xsta,ysta,w, STAT = ALLOC_ERR)

c**********************************************************
c HUMIDITY DISTRIBUTION
c**********************************************************
cbjd read station specific information
      read(55,*) gs,iymin,iymax,jxmin,jxmax
      read(55,*) ng,nh,ih

      allocate(hourly_hum(nh,ng))
      allocate(hum_pt_meas(int(nh/ih),ng))
      allocate(hum_dist(b_row,b_col,nh))
      allocate(xsta(ng))
      allocate(ysta(ng))
      allocate(w(b_row,b_col,ng))

      do i=1,ng
         read(55,'(2f5.0)') ysta(i),xsta(i)
         ysta(i)=int((real(ysta(i))-real(iymin))/gs+1.0)
         xsta(i)=int((real(xsta(i))-real(jxmin))/gs+1.0)
      end do

      do m=1,int(nh/ih) !hour
        read (55,*,END=500) (hum_pt_meas(m,n), n=1,ng)
      continue
      enddo

500   continue

cbjd close point data file
      close(55)

c     get weightings
      call weight(ng,xsta,ysta,b_col,b_row,w)

c     make sure the data is hourly
      call hourly(ih,nh,ng,hum_pt_meas,hourly_hum)

c     distribute based on weightings   
      call distr(nh,b_col,b_row,ng,w,hum_dist,hourly_hum) 

      deallocate(hourly_hum,hum_pt_meas,
     +           xsta,ysta,w, STAT = ALLOC_ERR)

c**********************************************************
c TEMPERATURE DISTRIBUTION
c**********************************************************
cbjd read station specific information
      read(56,*) gs,iymin,iymax,jxmin,jxmax
      read(56,*) ng,nh,ih

      allocate(hourly_tempr(nh,ng))
      allocate(tempr_pt_meas(int(nh/ih),ng))
      allocate(tempr_dist(b_row,b_col,nh))
      allocate(xsta(ng))
      allocate(ysta(ng))
      allocate(w(b_row,b_col,ng))

      do i=1,ng
         read(56,'(2f5.0)') ysta(i),xsta(i)
         ysta(i)=int((real(ysta(i))-real(iymin))/gs+1.0)
         xsta(i)=int((real(xsta(i))-real(jxmin))/gs+1.0)
      end do

      do m=1,int(nh/ih) !hour
        read (56,*,END=600) (tempr_pt_meas(m,n), n=1,ng)
      continue
      enddo

600   continue

cbjd close point data file
      close(56)

c     get weightings
      call weight(ng,xsta,ysta,b_col,b_row,w)

c     make sure the data is hourly
      call hourly(ih,nh,ng,tempr_pt_meas,hourly_tempr)

c     distribute based on weightings   
      call distr(nh,b_col,b_row,ng,w,tempr_dist,hourly_tempr) 

      deallocate(hourly_tempr,tempr_pt_meas,
     +           xsta,ysta,w, STAT = ALLOC_ERR)

c**********************************************************
c PRECIPITATION DISTRIBUTION
c**********************************************************
cbjd read station specific information
      read(57,*) gs,iymin,iymax,jxmin,jxmax
      read(57,*) ng,nh,ih

      allocate(hourly_precip(nh,ng))
      allocate(precip_pt_meas(int(nh/ih),ng))
      allocate(precip_dist(b_row,b_col,nh))
      allocate(xsta(ng))
      allocate(ysta(ng))
      allocate(w(b_row,b_col,ng))

      do i=1,ng
         read(57,'(2f5.0)') ysta(i),xsta(i)
         ysta(i)=int((real(ysta(i))-real(iymin))/gs+1.0)
         xsta(i)=int((real(xsta(i))-real(jxmin))/gs+1.0)
      end do

      read (57,*)

      do m=1,int(nh/ih) !hour
        read (57,*,END=700) (precip_pt_meas(m,n), n=1,ng)
      continue
      enddo

700   continue

cbjd close point data file
      close(57)

c     get weightings
      call weight(ng,xsta,ysta,b_col,b_row,w)

c     make sure the data is hourly
      call hourly(ih,nh,ng,precip_pt_meas,hourly_precip)

c     distribute based on weightings   
      call distr(nh,b_col,b_row,ng,w,precip_dist,hourly_precip) 

      deallocate(hourly_precip,precip_pt_meas,
     +           xsta,ysta,w, STAT = ALLOC_ERR)

c**********************************************************
c WRITE THE FORCING ASCII INPUT FILES
c**********************************************************
  	print *,'Writing out information for this event'

c	opening file to write shrtw data to file
      open(61,file=start_day//'.shw',action='write',status='unknown',
     +form='formatted',access='sequential')
	
c	opening file to write longw data to file
      open (62, file=start_day//'.lgw',action='write',status='unknown',
     +form='formatted',access='sequential')

c	opening file to write wind speed data to file
      open (63, file=start_day//'.wnd',action='write',status='unknown',
     +form='formatted',access='sequential')

c	opening file to write pressure data to file
      open (64, file=start_day//'.prs',action='write',status='unknown',
     +form='formatted',access='sequential')

c	opening file to write humidity data to file
      open (65, file=start_day//'.hum',action='write',status='unknown',
     +form='formatted',access='sequential')
	
c	opening file to write temperature data to file
      open (66, file=start_day//'.tem',action='write',status='unknown',
     +form='formatted',access='sequential')
	
c	opening file to write precipitation data to file
      open (67, file=start_day//'.met',action='write',status='unknown',
     +form='formatted',access='sequential')

      write (66,1001),start_day//' 01'
      write (66,1002),nh,'1','1.00'

      write (67,1067),'a'
      write (67,1067),'b'
      write (67,1067),'c'
      write (67,1067),'d'
      write (67,1001),start_day//' 01'
      write (67,1002),nh,'1','1.00'

1001	format(15x,a12)
1002	format(2x,i4,4x,a1,1x,a4)
1067	format(a1)
	
      do flnumber =61,65
          write(flnumber,*),'blankline:  1'
          write(flnumber,*),'blankline:  2'
          write(flnumber,*),'blankline:  3'
          write(flnumber,*),'blankline:  4'
          write(flnumber,1004),start_day,' 00','starting time'
          write(flnumber,1005),nh,'0.30','1   1.0',
     +'= #hrs,smc,month,conv'
      enddo
1004	format(11x,a10,3x,a2,1x,a13)
1005	format(9x,i4,1x,a4,6x,a7,a23)

	
      do n=1,nh 
        do flnumber=61,66
          write(flnumber,1003),'HOUR=',n
        enddo
          write(61,1000)((insw_dist  (j,i,n),i=1,b_col),j=b_row,1,-1)
          write(62,1000)((inlw_dist  (j,i,n),i=1,b_col),j=b_row,1,-1)
          write(63,1000)((wind_dist  (j,i,n),i=1,b_col),j=b_row,1,-1)
          write(64,1000)((pres_dist  (j,i,n),i=1,b_col),j=b_row,1,-1)
          write(65,1000)((hum_dist   (j,i,n),i=1,b_col),j=b_row,1,-1)
          write(66,2000)((tempr_dist (j,i,n),i=1,b_col),j=b_row,1,-1)
          if(flnumber.eq.67) then ! find out if we have any precipitation
            total_precip=0.0
            do i=1,b_col
              do j=1,b_row
                total_precip = total_precip+precip_dist(j,i,n)
              enddo
            enddo
            if(total_precip.eq.0.0) then
             write(67,2002)' HOUR=',-n,'0     Gauge'
            else
             write(67,2002),' HOUR=',n,'0     Gauge'
             write(67,2000)((precip_dist(j,i,n),i=1,b_col),j=b_row,1,-1)
            endif
        endif
      enddo
      deallocate(insw_dist,inlw_dist,wind_dist,pres_dist,
     +           hum_dist,tempr_dist,precip_dist,STAT = ALLOC_ERR)

      close(61)
      close(62)
      close(63)
      close(64)
      close(65)
      close(66)
      close(67)

c1000	format (29ES12.4) ! Wolf Creek
1000	format (16ES12.4) ! TVC

2003	format(a5)
1003	format(a5,i5,4x,a20)
c2000	format(29f5.1) ! Wolf Creek
2000	format(16f5.1) ! TVC
2002  format(a6,i5,4x,a11)
	
      stop
	end

