        PROGRAM ragmet

!***********************************************************************
! RAGMETA - This program generates a precipitation data file from
!           rain gauge data.
!
! Modified by Tricia Stadnyk - January 2000
!  Converted common blocks to modules and added dynamically allocated
!  run-time arrays as part of Fortran 90 conversion.
!
!        subroutines called:
!                rdevt
!                inpraga
!                inpgrda
!                weighta
!                distra
!                outmeta
!
!  VERSION 2      Dec. 2000  - TS: Added dynamic memory allocation
!
!        variable list:
!     NG = NUMBER OF RAIN GAGE STATIONS
!     nhg = NUMBER OF HOURS OF RECORDED DATA
!
!   I - R( , )  REAL*4     radar precipitation data
!   I - nhg     INT        number of hours of data
!   I - ixr     INT        number of east-west grid squares
!   I - iyr     INT        number of north-south grid squares
!   I - SMCRAG  REAL*4     initial soil moisture content
!   I - MO      INT        month of year
!   I - CONV    REAL*4     conversion factor
!   I - DATE    CHAR*12    event identifier
!   I - fln     CHAR*30    file names

!***********************************************************************

c        USE area2
c        USE area12
c        USE area16
c        use areawfo

      use area_watflood

!        include 'debug.for'
        
      INTEGER         :: iAllocateStatus,iDeallocateStatus
      INTEGER         :: ng,nhg,pmax,iyr,ixr,month_no,day_no
	REAL            :: conv,max_precip
      CHARACTER(12)   :: date
      DIMENSION       :: smc5(5)
      INTEGER(kind=2) :: nrad,I
      INTEGER(2)      :: status1
      CHARACTER(2)    :: buf
      CHARACTER(1)    :: stopflg,dataflg,local_smear_flg
      character(10)   ::  fileformat
      integer, dimension(:,:), allocatable :: no_hrs_precip
      real,    dimension(:,:), allocatable :: precip_rate,
     *                                        unused

      INTEGER        :: mohours(24),ju_mon(24)

      DATA mohours/744,672,744,720,744,720,744,744,720,744,720,744,
     *             744,672,744,720,744,720,744,744,720,744,720,744/

      DATA ju_mon/1, 32, 60, 91,121,152,182,213,244,274,305,335,
     *          366,397,425,456,486,517,547,578,609,639,670,700/

      iopt=1
	id=1   ! needed in rdevt

        iall=0

! GET THE DISTANCE WEIGHTING PARAMETER NW FROM THE PROGRAM ARGUMENT
! USED IN WEIGHT

!     USE DFLIB


      CALL GETARG(1, buf, status1)
      print*,status1
      print*, buf
      if(buf.eq.'-2')then
        stopflg='y'
      else
        stopflg='n'
      endif
      if(status1.ne.1)buf=' '
      print*, buf
      if(buf.ne.' ')then
        open(unit=99,file='junk',status='unknown')
        write(99,9901)buf
9901    format(a1)
        rewind 99
        read(99,9902)nw
9902    format(i2)
      else
        nw=2
      endif

      rdr='Gauge'

	print*,'********************************************************'
	print*,'*                                                      *'
	print*,'* RRRRRR         A       GGGG  MM     MM EEEEEE TTTTTT *'
	print*,'* RRRRRRR       AAA     GGGGGG MMM   MMM EEEEEE TTTTTT *'
	print*,'* RR   RR      AA AA    GG     MMMMMMMMM EE       TT   *'
	print*,'* RRRRRRR     AA   AA   GG     MM MMM MM EEEEE    TT   *'
	print*,'* RRRRRR     AAAAAAAAA  GG GGG MM  M  MM EEP      TT   *'
	print*,'* RR   RR   AA       AA GG  GG MM     MM EEEEEE   TT   *'
	print*,'* RR    RR AA         AA GGGG  MM     MM EEEEEE   TT   *'
	print*,'*                                                      *'
	print*,'*                  WATFLOOD (TM)                       *'
	print*,'*          Version 9.4   Sept. 23, 2008                *'
	print*,'*                                                      *'
	print*,'*      >>>>>>>> read tb0 or rag files <<<<<<<<         *'
	print*,'*                                                      *'
	print*,'*             (c) N. Kouwen, 1972-2008                 *'
	print*,'*                                                      *'
	print*,'********************************************************'
      print*
 

!      print*,'stopflg=    ',stopflg
!      print*,'nw=',nw
!      print*
!      pause


!         include 'fsublib.fi'          
!         character(128) :: arg
!         integer        :: argc,arglen
!         argc=iargc()
!         arglen=igetarg(0,arg)
!         arglen=igetarg(1,arg)
!         i=1
!         if(arglen.le.0)arglen=2

c        data ioflag/2/itogo/0/

      ioflag=2

! PRINT EVENT TITLE

      write(6,1000)

      open(unit=51,file='ragmet_info.txt',recl=flen,status='unknown',
     *       iostat=ios)
	if(ios.ne.0)then
	  print*,'Problem opening ragmet_info.txt in working directory'
	  print*
	  stop 'Program aborted in ragmet @ 110'
	endif       

! INPUT EVENT PARTICULARS

! TS - ALLOCATION STATEMENT TO DYNAMICALLY ALLOCATE ARRAY'S FOR AREA12

      allocate(fln(601),outfln(100),stat=iAllocateStatus)
      if (iAllocateStatus .ne. 0) STOP 
     *    '**Allocation failed for area12**' 

! INPUT EVENT PARTICULARS

      fln(99)='event/event.evt'
c      call rdevt(date,conv1,scale,smc5,nhg,nhf)
      call read_evt(date,conv1,scale,smc5,nhg,nhf)

      mo=mo1   ! mo1 = in area 2 - used to be in arg list

      call find_filetype(5)

      print*
      print*,'filetype=',filetype
      if(filetype.eq.'rag')then
        print*
        print*,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
        print*,'The input file found is ',fln(5)
        print*,'Check if'
        print*,'The first data line in the .rag file is the '
        print*,'initial soil moisture at selected locations.'
        print*
        print*,'This is no longer allowed as the initial soil'
        print*,'moisture is now entered with the .psm and .gsm '
        print*,'files.'
        print*
        print*,'Please note also that the initial soil moisture'
        print*,'is now required for each land cover class.'
        print*,'Please create the .psm file and run MOIST.exe'
        print*,'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
        print*
!        stop 'Program aborted in RAGMET @ 282'
      endif

! INPUT RAIN GAUGE FILE DATA

      ng=ioflag

! IOFLAG IS HARDWIRED IN RAGMET DATA STATEMENT
! FOR IOFLAG=2 GRID IS READ FROM basin\nsnm.rag FILE
! STATION LOCATIONS ARE READ FROM THE raing\yymmdd.rag FILE

! FOR IOFLAG=1 GRID IS READ FROM basin\bsnm.rag FILE
! STATION LOCATIONS ARE ALSO READ FROM THE basin\bsnm.rag FILE

! TS - ALLOCATIONS FOR SMCRAG,XSTA,YSTA,SNG,SNGMIN,EWG,EWGMIN,GNAME
!      OCCUR WITHIN SUBROUTINE INPGRDA.

!     For the met file, use the domain specified by the pdl file
!     Reason is that we may want a larger domain for grid shifting practice.
!     Note that snw & mosit use the shed file as these fields should 
!     never need to be shifted.

!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
      call inpgrd(ixr,iyr,ng)
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`

! TS - ALLOCATION FOR RAIN() OCCURS WITHIN SUBROUTINE INPRAGA.

      if(filetype.eq.'tb0')then
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
        call read_rag_ef(35,5,nhg,conv,ng)
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
      else
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
        call rdrag(nhg,conv,ng,fileformat)
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
	endif


! TS - ADDED ALLOCATION STATEMENT TO DYNAMICALLY ALLOCATE ARRAYS IN 
!      AREA16
!        Variables used in array allocations are defined in the 
!        following file(s):
!                    ixr,iyr  - inpgrda
!                    ng       - inpraga
!                    nhg      - inpevta, inpraga

      allocate(temp(ng),p(iyr,ixr),psum(iyr,ixr),
     *         pmin(iyr,ixr),w(iyr,ixr,ng),
     *         ntogo(nhg),dst(ng),no_hrs_precip(iyr,ixr),
     *         precip_rate(iyr,ixr),unused(iyr,ixr),
     *        stat=iAllocateStatus)
      if (iAllocateStatus .ne. 0) STOP  
     *    '**Allocation failed for AREA16A arrays in ragmet @ 153**'  

!     outarray is in areawfo
!     this has to be allocated before calling write_r2c
      allocate(outarray(iyr,ixr),stat=iAllocate)
        if(iAllocate.ne.0) STOP
     *    'Error with allocation of outarray in moist'      

! TS - MOVED DO LOOP FROM BEFORE LINE53 TO HERE BECAUSE PRECEEDED
!      ALLOCATION.
              
      print*,'grid allocations done: ycount xcount',iyr,ixr            

!     write the header (frame_no = 0)

!      SUBROUTINE write_r2c(un,fn,
!     *            no_frames,no_classes,frame_no,class_no,
!     *            no_signf)

      author='ragmet.exe                              '     
      name='Precipitation                           '
      coordsys_temp=coordsys1
      zone_temp=zone1
	datum_temp=datum1
	xorigin_temp=xorigin
	yorigin_temp=yorigin
	xcount_temp=xcount
	ycount_temp=ycount
	xdelta_temp=xdelta
	ydelta_temp=ydelta
	attribute_name='precipitation                           '
	attribute_units='mm                                      ' 
      attribute_type='Gauge                                   '  
      source_file_name=fln(5)
      unit_conversion=conv                                    

!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        call write_r2c(40,10,nhg,1,0,1,1)   
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 
! Taken out of data statement in area16 for F90
      do i=1,iyr
        do j=1,ixr
          psum(i,j)=0.0
          pmin(i,j)=999.0
        end do
      end do

!     FIND HOW MANY HOURS OF RAINFALL HAVE BEEN RECORDED:

      if(conv1.ne.conv)then
        write(6,1111)conv1,conv
 1111   format(' ','WARNING: event and rain file values of conv', 
     *      '     do not match. From event',f6.2,' from raing',f6.2, 
     *      '     The value from the .RAG file is used')
        print*
        pause ' In ragmet @ line ~141. Hit enter to continue'
      endif

      write(*,1112)nhg,ng
 1112 format(' file length =',i5,'hours    # gauges =',i5)

c      do nh=1,nhg
c        do is=1,ng
c          if(rrain(is,nh).gt.0.0)then
c            new=nh
c          endif
c        end do
c      end do

c      write(6,1113)nhg,new
c 1113 format(' file length =',i5,'hours - record length =',i5,'hours')

c      nhg=new          

!     MET FILES ARE PRODUCED FOR LENGTH OF RAINFALL RECORD
!     CONVERT RAINGAGE VALUES TO MM
!     ONLY RAIN GAGE FILE CAN BE IN NON-MM AMOUNTS
      
      if(conv.ne.1.0)then
        do nh=1,nhg
          do is=1,ng
            rrain(is,nh)=rrain(is,nh)*conv
          end do
        end do
        conv=1.0
      endif

!     SET THE NEXT RAIN TIME TO GO

c      lastnh=nhg
c      do nh=nhg,1,-1
c        do is=1,ng
c          if(rrain(is,nh).gt.0.0)then
c            lastnh=nh
c          end if
c        end do
c        ntogo(nh)=lastnh-nh
c      end do
c
c      print *,' finished checking for last hour of precip'

!     DISTRIBUTE RAIN ACCORDING TO WEIGHTS AND GENERATE XXXX.MET FILE 
      time=0.0

      write(6,1110)


!     LOOP THROUGH TIME
!     LOOP THROUGH TIME
!     LOOP THROUGH TIME
!     LOOP THROUGH TIME

      do nh=deltat,nhg,deltat

!       find julian day of first day
        day_no=(nh)/24
        ju=ju_mon(mo1)+(nh-deltat)/24
        i=12
        do while(i.ge.1)
          if(ju.lt.ju_mon(i+1))then 
            mo=i
          endif
          i=i-1
        end do
        if(mo.gt.12)mo=mo-12

        day_now=ju-ju_mon(mo)+1
        hour_now=mod(nh-deltat,24)+deltat
        ju=max(ju,1)
        month_now=mo

        write(6,1100)nh,nhg
            do i=1,iyr
              do j=1,ixr
            p(i,j)=0.0
          end do
        end do

        iflag=0
!       iflag = 1 if weights have to be recalculated
!       this happens when there is missing data at one or more 
!       of the stations and stations have to be ignored 
!       temporarily
        lflag=0
!       lflag = 1 when there is data at at least one gage
!       this can be zero rainfall 
        kflag=-1
!       kflag = -1 when there is non-zero rainfall data
!       when kflag = -1, the rainfall grid is not printed and 
!       nh is set -nh & SPL will assume 0 rainfall for the hour

        if(nh.eq.1)then

!         CHECK IF THERE IS ANY RECORDED DATA IN 1st HOUR

          do is=1,ng
            if(rrain(is,nh).ge.0.0) lflag=1
            if(rrain(is,nh).gt.0.0) kflag=1
          end do

        else
          do is=1,ng

!           RECALCULATE WEIGHTS IF AVAILABILITY OF GAUGES HAS
!           CHANGED

            if(rrain(is,nh).ge.0.0.and.  
     *         rrain(is,nh-1).lt.0.0)iflag=1
            if(rrain(is,nh).lt.0.0.and.  
     *             rrain(is,nh-1).ge.0.0)iflag=1

!           CHECK IF THERE IS ANY RECORDED DATA THIS HOUR

            if(rrain(is,nh).ge.0.0)lflag=1
            if(rrain(is,nh).gt.0.0)kflag=1
           end do
        endif

!       find the max recorded precip this delta t
        local_smear_flg=smrflg
        max_precip=0.0
        do is=1,ng
           max_precip=amax1(rrain(is,nh),max_precip)
        end do

        if(max_precip.lt.1.0.and.deltat.le.1)local_smear_flg='n'

!       THIS SECTION IS BYPASSED IF THERE IS NO RECORDED DATA

        if(lflag.eq.1)then
!         THIS SECTION IS BYPASSED IF THE STATIONS ARE THE SAME THIS HOUR
          if(iflag.eq.1.or.nh.eq.1)then
            call weight(nh,ng,ixr,iyr,0.0)
          endif
          call distr(nh,ixr,iyr,ng)
        endif


!       SMEARING  added Apr. 22/08  nk
!       only if the deltat > 1 and flag is 'y'
        if(local_smear_flg.eq.'y')then


!       find no of hours of precip & rate in each grid
          do i=1,iyr
            do j=1,ixr
              no_hrs_precip(i,j)=min(int(p(i,j)+1),deltat)  
              unused(i,j)=p(i,j)
            end do
          end do

c      print*,nh,p(1,1),unused(1,1),no_hrs_precip(1,1)

          do nhdt=1,deltat
            dataflg='n'
            hour_now=mod(nh-deltat,24)+nhdt
            do i=1,iyr
              do j=1,ixr
	          if(p(i,j).le.0.0)then
                  outarray(i,j)=0.0
                elseif(p(i,j).le.1.0)then
                  if(unused(i,j).ge.0.0)then
                   outarray(i,j)=unused(i,j)
                   unused(i,j)=0.0
                   if(outarray(i,j).gt.0.0)dataflg='y'
                  endif
                elseif(p(i,j).le.24.)then
                  if(unused(i,j).gt.0.000001)then
                    outarray(i,j)=amin1(1.0,unused(i,j))
                    unused(i,j)=unused(i,j)-outarray(i,j)
                    dataflg='y'
                  else
                    outarray(i,j)=0.0
                  endif
                elseif(p(i,j).gt.24.)then
                  outarray(i,j)=p(i,j)/deltat
                  dataflg='y'
                else
                  print*,'nhdt,i,j,p(i,j)/',nhdt,i,j,p(i,j)
                  print*,'This should never happen!!!'
                  stop
                endif

              end do
            end do    

c            dataflg='n'
            do i=1,iyr
              do j=1,ixr
                 if(dataflg.eq.'n'.and.outarray(i,j).gt.0.0)dataflg='y'
c               outarray(i,j)=amin1(outarray(i,j),999.9)  ! prevents ****.* in file
              end do
            end do     

!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            if(dataflg.eq.'y')then
              call write_r2c(40,10,nhg,1,nh+nhdt-1,1,9)
	      endif
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c            hour_now=hour_now+1

          end do  ! nhdt=...
        else

! OUTPUT PRECIPITATION DATA  -  no smearing

!         convert local variables to write module variables


          dataflg='n'
            do i=1,iyr
              do j=1,ixr
              outarray(i,j)=amin1(p(i,j),999.9)  ! prevents ****.* in file
              if(dataflg.eq.'n'.and.p(i,j).gt.0.0)dataflg='y'
            end do
          end do     

!      SUBROUTINE write_r2c(conv,un,fn,data_name,
!     *            no_frames,no_classes,frame_no,class_no,
!     *            hour_no,hours_togo,data_source,no_signf)

!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c          if(dataflg.eq.'y')then
            call write_r2c(40,10,nhg,1,nh,1,9)
c	    endif
!         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        endif



c        if(kflag.le.0)then
c!         IN THIS CASE THERE IS NO RAINFALL THIS HOUR AND PRECIP IS
c!         NOT NEEDED IN THE .MET FILE
c          call write_met  
c     *     (nhg,-1*nh,itogo,ixr,iyr,mo,conv,40,date,ioflag,fileformat)
c            ioflag=0
c        else
c          call write_met  
c     *     (nhg,nh,itogo,ixr,iyr,mo,conv,40,date,ioflag,fileformat)
c            ioflag=0
            do i=1,iyr
              do j=1,ixr
c            do i=1,12
c              do j=1,9
              psum(i,j) = psum(i,j)+p(i,j)
	        pmin(i,j) = amax1(pmin(i,j),p(i,j))
            end do
          end do
c        endif

      end do

!     WRITE THE RAINFALL SUMMARY: TOTAL RAINFALL

c      write(6,6001)
c      do i=iyr,1,-1
c        write(6,6000)(psum(i,j),j=1,ixr)
c      end do

      write(51,6001)
      do i=iyr,1,-1
        write(51,6000)(psum(i,j),j=1,ixr)
      end do
      write(51,6002)
      do i=iyr,1,-1
        write(51,6000)(pmin(i,j),j=1,ixr)
      end do

      close(unit=40,status='keep')
	print*
      print*,'Closed Unit 40  file name ',fln(10)

      close(unit=98,status='keep')

! TS - ADDED DEALLOCATION STATEMENT FOR AREA12 AND AREA16 ARRAYS


! FORMATS

1001  format(a30)
1002  format(10x,i5,1x,f5.2,1x,i6,1x,f5.1)
1003  format(' HOUR=',2i5)
1000  format(' RAIN GAUGE RAINFALL DISTRIBUTION APPLICATION')
1100  format('+',' DISTRIBUTING RAINFALL FOR HOUR ',i5,' OF ',i5,
     +' HOURS OF PRECIPITATION DATA')
1110  format(' ')
1300  format(9999f5.1)
1301  format(9999f5.2)
6000  format(9999f7.0)
6001  format(' Hour= -999                    Summary of rainfall:')
6002  format(' Hour= -999                    Maximum rainfall:')

      if(stopflg.eq.'y')then
        print*,' normal ending'
        print*
        stop 
      else
        stop ' normal ending'
      endif
      
      END PROGRAM ragmet




