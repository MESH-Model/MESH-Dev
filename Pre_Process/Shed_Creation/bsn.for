! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! *
! *             WATFLOOD : data preparation
! *
! *           revised watershed input (basin file)
! *
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

      PROGRAM bsn

!**********************************************************************
! BSNA - 
!
! Modified by FS - July 1999 variables for lan/long coordinates
! Modified by NK - June 8/00 for free format
!
! Modified by Tricia Stadnyk - September 2000
! Converted common blocks to modules and added dynamically allocated
! run-time arrays as part of Fortran 90 conversion.
!
!	subroutines called:
!               arrangea
!               gradea
!
!  VERSION 9      Dec. 2000  - TS: Added dynamic memory allocation
!  Version 9.07 - Jun. 28/02 - 
!  Version 9.08 - Nov. 22/02 - added profiles for sub-basins
!  Version 9.46 - Jul. 31/03 - fixed bug in converting areas
!  rev. 9.1.50  Jan.  14/04  - NK: version number added to the wfo_spec.txt file
!
!
! - list of arguments:
!
!   latsdeg = lattitude of south boundary in degrees
!   latsmin = lattitude of south boundary in minutes
!   latndeg = lattitude of north boundary in degrees
!   latnmin = lattitude of north boundary in minutes
!   lonwdeg = longitude of west boundary in degrees
!   lonwmin = longitude of west boundary in minutes
!   lonedeg = longitude of east boundary in degrees
!   lonemin = longitude of east boundary in minutes
!   yint    = north/south height of gru in minutes
!   xint    = west/east width of gru in minutes
!   al      = grid length in meters 
!   cintvl  = contour interval in meters 
!   impr    = percent of urban area that is impervious 
!           if impervious area is classified as such, imp will be 100% 
!           if urban area is classified as such, imp will 
!           range from about 25% to 50% depending on popn. density etc. 
!           default for inpr = 100% 
!           - this class is impervious area, not urban
!   itype   = channel type. Entered in parameter file.del fort.
!           itype = 0 then channel has flood plains
!           itype = 1 flood plains are infinite width
!   ntype   = number of land cover classes including 
!             urban/impervious   (changed Nov. 6/06  nk)
!   elvconv = conversion factor from imp. units to si
!
!************************************************************************

      USE area1
      USE area2
      USE area3
	use area4
	use area4
	use area16
      USE area17   ! see note
	use areawfo
	use areawet
	use area12

!     note:  some variable in area1 and area17 have the same name 
!     but have different dimensinos. 
!     So here they are called   rank_2d instead of say rank



        CHARACTER(10) :: time
        CHARACTER(8)  :: cday

      CHARACTER(80) :: comment
      CHARACTER(79) :: hdrcomment(100)
      CHARACTER(20) :: junk,filenames(20)
      CHARACTER(30) :: fn1
      CHARACTER(10) :: coordsys,datum,zone
      INTEGER(2)    :: nrad,ssquery,n_hdr_lines
      INTEGER       :: chksum(999)
      INTEGER       :: latsdeg,latsmin,latndeg,latnmin,lonwdeg,
     *                 lonwmin,lonedeg,lonemin,errflg,lastn
     *                 longest,maxlength,tempsum,startn,m,k,
     *                 lastgrid,nextgrid,nogrids,nnn,nullgrids,frame
      REAL          :: yint,xint,areamet,latrad,fudge,slopemin,split,
     *                 sngrid,ewgrid,cintv,aimpr,antype,elvconv,perv,
     *                 sumclass,e1,aimp 
	integer       :: no_errors,error_count,impr,newformat,longest,
     *                 lastn,iiiii,
     *                 newversion,local,i,j,n,ii,l,ix,nn,iallocate,
     *                 iallocatestatus,ios,iDeallocateStatus


      INTEGER(2)   :: status1
      CHARACTER(1) :: buf,update_flg,junk1,reply
      integer(2)   :: nrows,srows,wcols,ecols,
     *                i1flg,i2flg,i3flg,j1flg,j2flg
      logical      :: exists
!     USE DFLIB
      allocate(fln(601),stat=iAllocateStatus)
      if (iAllocateStatus.ne.0) STOP 
     *    '**Allocation failed for area12**' 

      CALL GETARG(1, buf, status1)
      if(status1.ne.1)iopt=status1

      no_errors=0
      iopt=2
      nrvr=0
      coordsys='unknown   '
	datum='unknown   '
	zone='unknown   '


! TS - INITIALIZATIONS MOVED TO LINE308 DUE TO ALLOCATIONS OF ARRAYS


	print*,'********************************************************'
	print*,'*                                                      *'
	print*,'*                  WATFLOOD (TM)                       *'
	print*,'*                                                      *'
	print*,'*     Program BSN Version 10      April 1, 2007        *'
	print*,'*                                                      *'
	print*,'*           (c) N. Kouwen, 1972-2006                   *'
	print*,'*                                                      *'
	print*,'********************************************************'
      print*
      print*,'Please see file bsn_info.txt for information re: this run'
      print*
	print*,'VERY IMPORTANT CHANGE:'
	print*
	print*,'In the bsnm.map file'
	print*,'the impervious area is now the LAST class - not the first'
	Print*,'The no of classes is now the TOTAL number - including the '
	print*,'impervious class'
	print*
	print*,'Please change the .map file accordingly if you have not'
	print*,'yet done so.  Sorry for the inconvenience  NK'
	print*
	pause 'Hit enter to continue - Ctrl C to abort'
	print*


      print*,' input the basin (map) file name:'
      read(*,1)fn1
      open(5,file=fn1,status='old',iostat=ios)
      if(ios.ne.0)then
         print*,' Unable to open the file ',fn1
         print*,' Are you in the proper directory?'
	   print*,' Did you enter the proper map file name?'
	   print*,' Does the file you named exist?'
         print*
         STOP ' Program aborted'
      endif


      print*,' input the parameter (par) file name:'
      read(*,1)fln(2)
    1 format(a16)
!     basin/bsnm.par
      open(unit=32 ,file=fln(2),status='old',iostat=ios)
      if(ios.ne.0)then
        print*,'Problems on unit 32'
        write(*,99172)fln(2)
        write(98,99172)fln(2)
99172   format(' Warning: Error opening or reading fln:',a30/
     *  ' Probable cause: missing basin/bsnm.par input file')
        print*,'iostat code =',ios
        STOP 'program aborted in bsn.for @ 139'
      endif



      print*,'Enter your name'
	read(*,2)author
    2 format(a40)

c      fn1='gr10k.map'
c	fln(2)='gr10k.par'

      print*,fln(2)



!#########################################################
!     grid size reduction:     9/05/2002   NK
      lastgrid=0
      print*
      print*,' Enter the grid you would like included'
      print*,' in the simulation'
      print*
      print*,' This should NOT be the receiving grid!!!!'
      print*
      print*,' There can only be one (1) outlet with this option'
      print*
      print*,' example:  6639   Hit Return to use whole dataset '
      print*
      read(*,5)lastgrid
5     format(i5)

!     get rid of the existing new.map file  nk. Jul. 29/04
      INQUIRE(FILE='new.map',EXIST=exists)
	IF(exists)THEN
        open(unit=5,file='new.map',status='old',iostat=ios)
	  close(unit=5,status='delete')
	  print*,'existing new.map file deleted'
	  print*,'replaced by new_format.map '
	  print*
	  pause 'hit enter to continue'
	endif


      open(36,file='new_format.shd',form='formatted',
     *     access='sequential',recl=2048,status='unknown',
     *     iostat=ios)
      if(ios.ne.0)then
         print*,' Problems opening the NEW1.SHD file '
         print*,' Possible cause: file open or read only'
         print*
         STOP ' Program BSN aborted'
      endif

!     create a file with the CHANNEL PARAMETERS INCLUDED
!       thurns out, this format will not be used

c      open(41,file='channel.shd',form='formatted',
c     *     access='sequential',recl=2048,status='unknown',
c     *     iostat=ios)
c      if(ios.ne.0)then
c        print*,' Problems opening the channel.shd file '
c        print*,' Possible cause: file open or read only'
c        print*
c        STOP ' Program BSN aborted @ 579'
c      endif


!     get rid of the existing new.shd file  nk. Jul. 8/04
      INQUIRE(FILE='new.shd',EXIST=exists)
	IF(exists)THEN
        open(unit=37,file='new.shd',status='old',iostat=ios)
	  close(unit=37,status='delete')
	  print*,'existing new.txt file deleted'
	  print*,'replaced by new_format.map and old_format.map'
	  print*
	  pause 'hit enter to continue'
	endif

      open(37,file='old_format.shd',form='formatted',
     *     access='sequential',recl=2048,status='unknown',
     *     iostat=ios)
      if(ios.ne.0)then
         print*,' Problems opening the NEW.SHD file '
         print*,' Possible cause: file open or read only'
         print*
         STOP ' Program BSN aborted'
      endif

      open(38,file='ssdata.out',form='formatted',
     *     access='sequential',recl=2048,status='unknown',
     *     iostat=ios)
      if(ios.ne.0)then
         print*,' Problems opening the ssdata.out file '
         print*,' Possible cause: file open or read only'
         print*
         STOP ' Program BSN aborted'
      endif

      open(39,file='bsn_info.txt',form='formatted',
     *     access='sequential',recl=2048,status='unknown',
     *     iostat=ios)
      if(ios.ne.0)then
         print*,' Problems opening the bsn.err file '
         print*,' Possible cause: file open or read only'
         print*
         STOP ' Program BSN aborted'
      endif

!     Undocumented feature
!     Undocumented feature
!     Undocumented feature
!     Undocumented feature
!     Wetlands can be either coupled or not coupled to the river
!     To have both present, there should be two wetland classes. 
!     Only the last one will be coupled to the river.
!     Create a map file with two wetland classes, the last one = coupled
!     If they can not be differentiated from a land cover map, first
!     create two identical wetland % grids. 
!     Next divide them up according to the desired split, which is entered 
!     on the second line of the old format map file.
!     The dual wetland percentages are computed in the write statement
!     

!     Changed to accept any number of comments at the top of the file
!         or  -- none at all   NK  Sept. 11/05

      newversion=0
      i=0
      read(5,5007,iostat=ios)junk1,hdrcomment(i+1)
      if(junk1.eq.'#'.or.junk1.eq.':')then
	  i=i+1
        print*
        print*, 'Ensim compatible free format map file expected'
        print*
        newversion=1
!       read the New ENSIM format
!       read the New ENSIM format
!       read the New ENSIM format
!       read the New ENSIM format
!       read the New ENSIM format
!          file can start with # or : for first data line
        do while(junk1.eq.'#')
          i=i+1
          read(5,5007,iostat=ios)junk1,hdrcomment(i)
          write(51,5007)junk1,hdrcomment(i)
          write(*,5007)junk1,hdrcomment(i)
          if(ios.ne.0)then
	      print*,'error on line ',i,' of file',fn1
	      print*,'Please update BSN.EXE'
	      print*
	      stop 'Program aborted in BSN @ 237'
	    endif
	  end do
        backspace 5
	  n_hdr_lines=i-1

        read(5,5004,iostat=ios)junk,coordsys
        print*,junk,coordsys
        if(ios.ne.0)then
          print*,'Error reading unit 5 fln=',fn1
	    print*,'Please update BSN.EXE'
          print*
          stop 'Program aborted in bsn @ 368'
        endif

        if(coordsys.eq.'UTM       ')then
          read(5,5004,iostat=ios)junk,datum
          print*,junk,datum
	    if(ios.ne.0)then
	      print*,'Error reading unit 5 fln=',fn1
	      print*
	      stop 'Program aborted in bsn @ 382'
	    endif
          read(5,5004,iostat=ios)junk,zone
          print*,junk,zone
	    if(ios.ne.0)then
	      print*,'Error reading unit 5 fln=',fn1
	      print*
	      stop 'Program aborted in bsn @ 389'
	    endif
        elseif(coordsys.eq.'CARTESIAN ')then
!         datum and zone are undefined & not needed
          datum='unknown'
          zone='unknown'
        elseif(coordsys.eq.'LATLONG   ')then
          read(5,5004,iostat=ios)junk,datum
          print*,junk,datum
	    if(ios.ne.0)then
	      print*,'Error reading unit 5 fln=',fn1
	      print*
	      stop 'Program aborted in bsn @ 401'
	    endif
        else
          print*,' Valid format (UTM, CARTESIAN, or LATLONG) not found'
	    print*,'format found is ***',coordsys,'***'
          print*
          stop 'Program aborted @ 407'
        end if

!       READ NEW FORMAT FOR HEADER
        if(coordsys.eq.'UTM       '.or.coordsys.eq.'CARTESIAN ')then
!         New ENSIM format
          read(5,*)junk    ! reads a line starting with #
          print*,junk
          read(5,5003)junk,xorigin
          print*,junk,xorigin
          read(5,5003)junk,yorigin
          print*,junk,yorigin
          read(5,5003)
          read(5,5006)junk,xcount
          print*,junk,xcount
          read(5,5006)junk,ycount
          print*,junk,ycount


          imax=int(ycount)
          jmax=int(xcount)      


          read(5,5003)junk,xdelta
          print*,junk,xdelta
          read(5,5003)junk,ydelta
          print*,junk,ydelta
          read(5,5003)junk
          read(5,5003)junk,cintv
          print*,junk,cintv
          read(5,5003)junk,aimpr
          impr=int(aimpr)
          print*,junk,impr
          read(5,5003)junk,antype

!     fix fix
!     for the ensim format header, the no of classes 
!     now INCLUDES the impervious class
!     later all ntype+1 indecies need to change to ntype
!     for now we subtract 1 here and add 1 later for the r2c file.

          ntype=int(antype)-1
!          ntype=int(antype)


          print*,junk,ntype+1
          if(ntype.eq.0)then
             print*,' No. of land classes not entered on the map file'
             print*,' Map file assigned 100% to class 1'
             print*,' in the bsnm.map file'
             print*
             PAUSE ' Press Enter to continue'
          endif
          read(5,5003)junk,elvconv
          print*,junk,elvconv
!         look for  :endHeader
          do while(junk.ne.':endHeader         ')
            read(5,5003)junk
            print*,junk
          end do
!         CONVERT TO OLD VARIABLES: 
          al=sqrt(xdelta*ydelta)
          sstep=al/1000.0
          step2=sstep**2
          istep=int(sstep)
          print*,' Computed nominal grid size= ',al
          jxmin=int(xorigin/1000.)
          iymin=int(yorigin/1000.)
!          jxmax=jxmin+int(xcount*xdelta/1000.)-1
!          iymax=iymin+int(ycount*ydelta/1000.)-1
          jxmax=jxmin+int((xcount-1)*xdelta/1000.)
          iymax=iymin+int((ycount-1)*ydelta/1000.)
          imin=1
          imax=ycount
          jmin=1
          jmax=xcount
        else   ! cordsys = LATLONG
!         New ENSIM format

          read(5,5003)junk   ! reads a line starting with #
          print*,junk
!         New ENSIM format
!          read(5,5003)junk,xoriginDegree
!          print*,junk,xoriginDegree
!          read(5,5003)junk,yoriginDegree
!          print*,junk,yoriginDegree
          read(5,5003)junk,xorigin
          print*,junk,xorigin
          read(5,5003)junk,yorigin
          print*,junk,yorigin
          read(5,5003)junk
          print*,junk
          read(5,5006)junk,xcount
          print*,junk,xcount
          read(5,5006)junk,ycount
          print*,junk,ycount
          read(5,5003)junk,xdelta
          print*,junk,xdelta
          read(5,5003)junk,ydelta
          print*,junk,ydelta
          read(5,5003)junk
          print*,junk
          read(5,5003)junk,cintv
          print*,junk,cintv
          read(5,5003)junk,aimpr
          print*,junk,aimpr
          impr=int(aimpr)
          read(5,5003)junk,antype
          print*,junk,antype

!     fix fix
!     for the ensim format header, the no of classes 
!     now INCLUDES the impervious class
!     later all ntype+1 indecies need to change to ntype
!     for now we subtract 1 here and add 1 later.

          ntype=int(antype)-1
!          ntype=int(antype)

          read(5,5003)junk,elvconv
          print*,junk,elvconv

!         look for  :endHeader
          i=1
          do while(junk.ne.':endHeader         ')
            read(5,5003)junk
            print*,junk
            i=i+1
          end do

!         end reading header

!         CONVERT TO OLD VARIABLES: 
!     *   iymin,iymax,jxmin,jxmax,latsdeg,latsmin,
!     *   latndeg,latnmin,lonwdeg,lonwmin,lonedeg,lonemin,yint,xint
          write(*,*) 'lat/long coordinates'


          yint=ydelta*60.0      !  divide ???
          xint=xdelta*60.0


!          iymin=xoriginDegree*60
!          jxmin=yoriginDegree*60

          jxmin=xorigin*60.0     !changed Mar. 3/04  nk
          iymin=yorigin*60.0

!          iymax=iymin+ycount        !changed Mar. 3/04  nk
!          jxmax=jxmin+xcount

          iymax=iymin+ycount*yint
          jxmax=jxmin+xcount*xint

          fudge=0.00001
!          latsdeg=int(yoriginDegree)
!          latsmin=int(amod(yoriginDegree,float(latsdeg))*60.0)
!          latndeg=int(yoriginDegree+ycount*ydelta)
!          latnmin=int(amod((yoriginDegree+ycount*ydelta)
!     *                       ,float(latndeg))*60.0)-1
!
!          lonwdeg=int(xoriginDegree)
!          lonwmin=int(amod(xoriginDegree,float(lonwdeg))*60.0)
!          lonedeg=int(xoriginDegree+xcount*xdelta)
!          lonemin=int(amod((xoriginDegree+xcount*xdelta)


          latsdeg=int(yorigin)
          latsmin=int(amod(yorigin,float(latsdeg))*60.0)
          latndeg=int(yorigin+ycount*ydelta)
          latnmin=int(amod((yorigin+ycount*ydelta)
     *                       ,float(latndeg))*60.0)-1

          lonwdeg=int(xorigin)
          lonwmin=int(amod(xorigin,float(lonwdeg))*60.0)
          lonedeg=int(xorigin+xcount*xdelta)
          lonemin=int(amod((xorigin+xcount*xdelta)
     *                           ,float(lonedeg))*60.0)-1

          imin=1
          jmin=1
!         this was commented out - don't know why.
          imax=ycount
          jmax=xcount      

          latrad=(yorigin+ydelta*ycount/2.0)/180.*2.*acos(0.0)

          areamet=yint*(111.1360-0.5623*cos(2.0*latrad)
     *        +0.0011*cos(4.0*latrad))/60.0*xint*(111.4172*cos(latrad)
     *        -0.094*cos(3.0*latrad)+0.0002*cos(5.0*latrad))/60.0
     *        *1000.0*1000.0
          al=sqrt(areamet)
          sstep=al/1000.0
          step2=sstep**2
          istep=int(sstep)

        endif
        pause 'please check above numbers & hit enter to continue'
        
	  print*
        print*,'Enter the split: % of wetland coupled to channel'
	  print*,'only if you have two identical sets of wetland '
	  print*,'land cover gridsas the 2 classes before the '
	  print*,'water class in the land use section of the map file'
	  print*,'Enter 0 if you have just 1 block of wetland land cover'
        print*
	  print*,'Split ='
	  read*,split
	  split=split/100.0
	  if(split.gt.1.0.or.split.lt.0.0)then
	    print*,'value for split is outside acceptable range'
	    print*
	    stop 'Program aborted @ 483'
	  endif

      else
!       READ IN UNDER THE OLD FORMAT
!       READ IN UNDER THE OLD FORMAT
!       READ IN UNDER THE OLD FORMAT
!       READ IN UNDER THE OLD FORMAT
!       READ IN UNDER THE OLD FORMAT
        print*, 'Old style, fixed format map file expected'
        print*
        REWIND 5
        write(*,1101)
        read(5,7001,iostat=ios)ls,ks,js,ih,local
        write(*,7001)ls,ks,js,ih,local
        read(5,1020,iostat=ios)al,cintv,impr,ntype,elvconv,split  !split
        aimpr=float(impr)
        write(*,1013)
        write(*,1022)al,cintv,impr,ntype,elvconv
        if(ios.ne.0)then
           print*, 'unable to read line 2'
           print*
           STOP ' Program aborted'
        endif
        if(impr.le.0)impr=100

        if(elvconv.eq.0.0)then
           elvconv=1.0
!          S.I. UNITS IS DEFAULT 
        endif

        read(5,1005,iostat=ios)
     *  iymin,iymax,jxmin,jxmax,latsdeg,latsmin,
     *  latndeg,latnmin,lonwdeg,lonwmin,lonedeg,lonemin,yint,xint
        write(*,1007)
     *  iymin,iymax,jxmin,jxmax,latsdeg,latsmin,
     *  latndeg,latnmin,lonwdeg,lonwmin,lonedeg,lonemin,yint,xint
        if(ios.ne.0)then
            print*, ' Unable to read line 3'
            print*
            STOP ' Program aborted'
        endif

        sstep=al/1000.
        step2=sstep**2
        istep=int(sstep)

!       create a file with the new ensim format
        open(40,file='new_format.map',form='formatted',
     *     access='sequential',recl=2048,status='unknown',
     *     iostat=ios)
        if(ios.ne.0)then
           print*,' Problems opening the map.new file '
           print*,' Possible cause: file open or read only'
           print*
           STOP ' Program BSN aborted @ 125'
        endif

        if(iymin.lt.0)then
!         read OLD FORMAT
          coordsys='LATLONG   '
!          datum='          '   !set at top
!         CALCULATION FOR LAT/LONG COORDINATES
          write(*,*) 'lat/long coordinates'
          write(*,*) iymin,iymax,jxmin,jxmax,latsdeg,latsmin,latndeg,
     *    latnmin,lonwdeg,lonwmin,lonedeg,lonemin,yint,xint
          iymin=real(latsdeg*60+latsmin)
          jxmin=real(lonwdeg*60+lonwmin)
          iymax=real(latndeg*60+latnmin)
          jxmax=real(lonedeg*60+lonemin)
!          imax=int(real((latndeg-latsdeg)*60+(latnmin-latsmin))/yint)       
!          jmax=int(real((lonedeg-lonwdeg)*60+(lonemin-lonwmin))/xint)       
          imin=1
          jmin=1
          imax=int(real((latndeg-latsdeg)*60+(latnmin-latsmin))/yint)+1
          jmax=int(real((lonedeg-lonwdeg)*60+(lonemin-lonwmin))/xint)+1       
          write(*,1104)
          write(*,1105)
          write(*,1024)imin,imax,iymin,iymax,yint
          write(*,1106)
          write(*,1024)jmin,jmax,jxmin,jxmax,xint
          print*
          xorigin=float(jxmin)/60.0
          yorigin=float(iymin)/60.0
          xcount=jmax
          ycount=imax
          xdelta=xint
          ydelta=yint
!          cintv=
!          aimp=
          antype=float(ntype) 
!          elvconv         
        else
!         rad OLD FORMAT
          coordsys='UTM       '
!          datum='          '   ! set at top
!          zone='          '
!         CALCULATION FOR UTM (OR ANY SQUARE GRID) COORDINATES
!          imax=int(real(iymax-iymin)/sstep)
          imax=int(real(iymax-iymin)/sstep)+1
          imin=1
!          jmax=int(real(jxmax-jxmin)/sstep)
          jmax=int(real(jxmax-jxmin)/sstep)+1
          jmin=1
          yint=sstep
          xint=sstep
          write(*,1104)
          write(*,1105)
          write(*,1024)imin,imax,iymin,iymax,yint
          write(*,1106)
          write(*,1024)jmin,jmax,jxmin,jxmax,xint
          print*
          xorigin=float(jxmin)*1000.0
          yorigin=float(iymin)*1000.0
          xcount=jmax
          ycount=imax
          print*,'al=',al

          xdelta=al
          ydelta=al
!          cintv=    already defined
!          aimp=     already defined
          antype=float(ntype) 
!          elvconv=   already defined         
        endif
        pause 'please check above numbers & hit enter to continue'
!       end of old format read header 
      endif

      print*,'Number of classes now includes the impervious class'
      print*,'Number of classes stipulated = ',ntype+1
	print*
	print*,'Is this correct?  y or n'
	read*,reply

      reply='y'

	if(reply.ne.'y')then
	  print*,'Please fix the .map file'
	  print*
	  stop 'Program aborted in bsn @ 713'
	endif

c      pause 100
c      print*,'newversion=',newversion

      if(newversion.eq.0)then
!       write the new_version.map file - conversion from old format
!       write the new_version.map file - conversion from old format
        write(40,5012) 
        write(40,5009)coordsys
        if(coordsys.eq.'UTM       ')then
          write(40,5010)datum
          write(40,5011)zone
        endif
	pause 1
        if(coordsys.eq.'LATLONG   ')then
          write(40,5010)datum
        endif
        write(40,5012)
        if(coordsys.eq.'LATLONG   ')then
          write(40,5013)xorigin
          write(40,5014)yorigin
        else
          write(40,50131)xorigin         !   *1000.0
          write(40,50141)yorigin         !   *1000.0
        endif
	pause 2
        write(40,5012)
        write(40,5015)xcount
        write(40,5016)ycount
        if(coordsys.eq.'LATLONG   ')then
          write(40,5017)xdelta/60.0
          write(40,5018)ydelta/60.0
        else
          write(40,50171)xdelta
          write(40,50181)ydelta
        endif
	pause 3
        write(40,5012)
        write(40,5019)cintv
        write(40,5020)aimpr

        write(40,5021)int(antype)+1  ! changed nov 6/06
!        write(40,5021)int(antype)

      pause 4
        write(40,5022)elvconv   !units not converted for updated format
        write(40,5024)'          File converted from ',fn1
        write(40,5032)                !  :endHeader
      endif

      print*,'before allocating area17'

! TS - ALLOCATE AREA17A ARRAYS
      allocate(elv_2d(imax,jmax),da_2d(imax,jmax),slope_2d(imax,jmax),
     *frac_2d(imax,jmax),rank_2d(imax+1,jmax+1),
     *s_2d(imax,jmax),next_2d(imax,jmax),ielv(imax,jmax),
     *iak(imax,jmax),irough_2d(imax,jmax),ichnl_2d(imax,jmax),
     *bnkfll_2d(imax,jmax),ireach_2d(imax,jmax),
     *yyy(imax*jmax),xxx(imax*jmax),ibn(imax*jmax),
     *distance(imax*jmax),channel(imax*jmax),dummy(imax,jmax),
     *idummy(imax,jmax),new(imax*jmax),ch_length_2d(imax,jmax),
     *newxxx(jmax*imax),newyyy(imax*jmax),newnext(imax*jmax),
     *newrank(imax*jmax),sl1(imax*jmax),stat=iAllocateStatus)
      if (iAllocateStatus .ne. 0) STOP 
     *    '**Allocation failed for area17 in bsna l303**' 

      print*,'area17 allocated'



! INITIALIZATION - MOVED SEPT/2000 DUE TO ALLOCATIONS:
      do i=1,imax
         do j=1,jmax
            rank_2d(i,j)=0
            next_2d(i,j)=0
            da_2d(i,j)=0.0
            bnkfll_2d(i,j)=0.0
            slope_2d(i,j)=0.0
            elv_2d(i,j)=0.0
	      ch_length_2d(i,j)=0.0
	      iak(i,j)=0.0
	      ichnl_2d(i,j)=0.0
            ireach_2d(i,j)=0
	      frac_2d(i,j)=0.0
         end do
      end do

      if(impr.le.0)impr=100
      if(elvconv.eq.0.0)elvconv=1.0

      ijk=imax*jmax 

! ELEVATION(i,j) IS THE ELEVATION OF THE STREAMBED AT A POINT M
! TWEENTHE ENTRANCE AND THE EXIT OF THE STREAM AT A GIVEN STORAGE
! ELEMENT

      read(5,5000)comment
      write(40,5000)comment
!      write(*,5000)comment
!      print*

      do i=imax,imin,-1
         if(newversion.eq.0)then
           read(5,1001,iostat=ios)(elv_2d(i,j),j=jmin,jmax)
           write(40,4001,iostat=ios)(elv_2d(i,j),j=jmin,jmax)
         else
           read(5,*,iostat=ios)(elv_2d(i,j),j=jmin,jmax)
         endif
         if(ios.ne.0)then
           print*,' Problems reading the elevations on line',imax-i+1
           print*
           STOP ' Program aborted'
         endif
      end do

! S(I,J) IS THE DIRECTION OF DRAINAGE OUT OF AN ELEMENT  
!        1=ne  3=se  4=s  5=sw  6=w  7=nw  8=n

!      write(*,1107)
!      write(*,1108)
!      do i=imax,imin,-1
!         write(*,*)(elv_2d(i,j),j=jmin,jmax)
!      end do
!      print*
!      pause '3 - Hit enter to continue'


      write(39,1107)
      write(39,1108)
      write(39,1006)j,j,(j,j=jmin,jmax)
!      write(39,1006)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
      write(39,1006)j,j,(int(jxmin+xint*(j-1)),j=jmin,jmax) 
      do i=imax,imin,-1
         do j=jmin,jmax
            ielv(i,j)=int(elv_2d(i,j))
         enddo
!        write(39,1006)iymin+istep*(i-1),i,(ielv(i,j),j=jmin,jmax)
         write(39,1006)iymin+int(yint)*(i-1),i,(ielv(i,j),j=jmin,jmax)
      end do

!     CONVERT TO METRIC 
      do i=imin,imax
         do j=jmin,jmax
            elv_2d(i,j)=elv_2d(i,j)*elvconv
         end do
      end do
	elvconv=1.0  !added 11/09/04  NK

! FRAC IS THE FRACTION OF A SQUARE IN THE BASIN 
      read(5,5000)comment
      write(40,5000)comment
!      write(*,5000)comment
      do i=imax,imin,-1
         if(newversion.eq.0)then
           read(5,1052,iostat=ios)(frac_2d(i,j),j=jmin,jmax)
           write(40,4050)(int(frac_2d(i,j)),j=jmin,jmax)
!          units not converted for updated format
         else
           read(5,*,iostat=ios)(frac_2d(i,j),j=jmin,jmax)
         endif
         if(ios.ne.0)then
           print*,' Problems reading the fractions in row',i
           print*
           STOP ' Program aborted'
         endif
      end do

!      do i=imax,imin,-1
!         write(*,1056)(frac_2d(i,j),j=jmin,jmax)
!      end do
!      print*

!      pause '4 - hit enter to continue'

!      write(*,1109)

      write(39,1109)
      write(39,1057)j,j,(j,j=jmin,jmax)
      write(39,1057)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
      do i=imax,imin,-1
         write(39,1054)iymin+istep*(i-1),i,(frac_2d(i,j),j=jmin,jmax)
      end do

!      pause 41

      if(frac_2d(imax,1).lt.0.0)then
         do i=imax,imin,-1
            do j=jmin,jmax
               frac_2d(i,j)=frac_2d(i,j)/(sstep*sstep)
            end do
         end do
      else
!        frac is given in percent so divide by 100
         do i=imax,imin,-1
            do j=jmin,jmax
               frac_2d(i,j)=frac_2d(i,j)/100.
            end do
         end do
      end if

!     DRAINAGE DIRECTIONS:

      read(5,5000)comment
      write(40,5000)comment
!      write(*,5000)comment

      do i=imax,imin,-1
         if(newversion.eq.0)then
           read(5,1003,iostat=ios)(s_2d(i,j),j=jmin,jmax)
           write(40,1003,iostat=ios)(s_2d(i,j),j=jmin,jmax)
         else
           read(5,*,iostat=ios)(s_2d(i,j),j=jmin,jmax)
         endif
         if(ios.ne.0)then
           print*,' Problems reading the directions on line',i
           print*
           STOP ' Program aborted'
         endif
      end do

!      write(*,1110)
!      write(*,1111)
!      do i=imax,imin,-1
!         write(*,1003) (s_2d(i,j),j=jmin,jmax)
!      end do
!      print*

!      pause '6 - Hit enter to continue' 

      write(39,1110)
      write(39,1111)
      write(39,1006)j,j,(j,j=jmin,jmax)
      write(39,1006)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
      do i=imax,imin,-1
         write(39,1006)iymin+istep*(i-1),i,(s_2d(i,j),j=jmin,jmax)
      end do

! IOPT=1  WILL PRINT ALL THE DATA FOR EACH ELEMENT
! IOPT=2  WILL GIVE ONLY THE FLOWS AT THE SELECTED POINTS
      iopt=1
      iopt=2
      ib=imin+1
      it=imax-1
!      write(*,1112)
!      write(*,1113)
!      write(*,1005) ib,it
!      print*

      if(iopt.ge.1)print*,'gone to arrange'

!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
      call arrange()    
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``

      if(iopt.ge.1)print*,'back from arrange'

!     added for erasmo Feb. 16/03 nk

      print*
      print*,' Often DEM have flat spots filled and you end up with'
      print*,' unwanted flat spots in your river profile'
      print*,' It causes severe flattening of the hydrographs'
      print*,' Enter the minimum allowable river slope'
      print*,' that you have in your sustem - e.g. 0.0001'
      print*,' Min accepted value = 0.0000001'
      print*,' Max value accepted is 1.0 (45 degrees!)'
	print*
!	This value is needed in grade.for
      read*,slopemin
      slopemin=amax1(slopemin,0.0000001)
      slopemin=amin1(slopemin,1.0)

! THE SUBROUTINE GRADEA CALCULATES THE CHANNEL SLOPE AT THE
! OUTLET OF EACH ELEMENT 

      if(iopt.ge.1)print*,'gone to grade'

!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
      call grade(slopemin,no_errors)  
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
 
      if(iopt.ge.1)print*,'back from grade'

      if(lastgrid.eq.0)then
        lastgrid=na    ! <<<<<<<<do not use naa here - shd file gets truncated
      else
!       the user picks the last grid in the watershed. 
!       here we assign the receiving grid which is the next one
        i=yyy(lastgrid)
        j=xxx(lastgrid)
        lastgrid=next_2d(i,j)
        if(lastgrid.gt.na)then
          print*,'It looks like you have picked a lastgrid outside'
          print*,'the original modelled watershed - in one of the '
          print*,'receiving grids. Please reselect more upstream'
          print*,'na,naa,lastgrid',na,naa,lastgrid
          print*
          stop 'Program aborted'
        endif
      endif

! RANK WILL BE READ BY THE VARIABLE 'S' IN PROGRAM
! SIMPLE. NOTE THAT IN THIS PROGRAM S IS THE DRAINAGE
! DIRECTION!

! IF NTYPE .GE.1 THEN ALL VALUES ON IAK(I,J) MUST BE ZERO 
! BUT DATA IS STILL READ IN TO PRESERVE DATA STRUCTURE
! ANY VALUE OF IAK MAY BE ENTERED BUT WHEN NTYPE>0,
! ALL IAK VALUES ARE CHANGED TO 0

! RIVER CLASS:
      read(5,5000)comment
      write(40,5000)comment
!      write(*,5000)comment
      do i=imax,imin,-1
         if(newversion.eq.0)then
           read(5,1003,iostat=ios)(iak(i,j),j=jmin,jmax)
           write(40,1003,iostat=ios)(iak(i,j),j=jmin,jmax)
         else
           read(5,*,iostat=ios)(iak(i,j),j=jmin,jmax)
         endif
         if(ios.ne.0)then
           print*,' Problems reading the river class on line',i
           print*
           STOP ' Program aborted'
         endif
      end do
!     Find number of river classes in the map file:
      do i=imin,imax
        do j=jmin,jmax
          nrvr=max0(nrvr,iak(i,j))
        end do
      end do

!     iak = ibn 
!     ibn needed in rdpar later
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        ibn(n)=iak(i,j)
      end do

!     This added April 1/07  nk
!     was undefined earlier
      do n=naa+1,na
	  ibn(n)=0
	end do



!     TS - ALLOCATIONS OF AREA4A ARRAYS
!     ntype for the number of land cover classes
!     nrivertype for the number of channel or basin types
!     moved here from spl9  nk 06/07/00
!     then moved from rdpar nk 28/12/04
!     moved back from rdshed 27/07/06 because needed for bsn.for  nk


      if(iopt.ge.1)print*,'nrvr=',nrvr


c      allocate(mndr(nrvr),
c     *r1(nrvr),r2(nrvr),r2low(nrvr),r2hgh(nrvr),r2dlt(nrvr),
c     *aa2(nrvr),aa3(nrvr),aa4(nrvr),rivtype(nrvr),
c     *theta(nrvr),thetadlt(nrvr),thetalow(nrvr),thetahgh(nrvr),
c     *widep(nrvr),widepdlt(nrvr),wideplow(nrvr),widephgh(nrvr),
c     *kcond(nrvr),kconddlt(nrvr),kcondlow(nrvr),kcondhgh(nrvr),
c     *flz(nrvr),flz2(nrvr),flzlow(nrvr),flzhgh(nrvr),flzdlt(nrvr),
c     *pwr(nrvr),pwr2(nrvr),pwrlow(nrvr),pwrhgh(nrvr),pwrdlt(nrvr),
c     *stat=iAllocate)
c      if(iAllocate.ne.0) STOP
c     *   'Error with allocation of area4 arrays in bsn'


!     in spl these allocations are done in dear_shed_ef
!     but in this creates this file so it has to be done here

      allocate(mndr(na),r1(na),r2(na),r1n(na),r2n(na),
     *aa2(na),aa3(na),aa4(na),rivtype(na),theta(na),widep(na),
     *kcond(na),flz(na),pwr(na),pwr2(nrvr),stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *   'Error with allocation of area4 arrays in bsn'


      allocate(r2low(nrvr),r2hgh(nrvr),r2dlt(nrvr),
     *thetadlt(nrvr),thetalow(nrvr),thetahgh(nrvr),
     *widepdlt(nrvr),wideplow(nrvr),widephgh(nrvr),
     *kconddlt(nrvr),kcondlow(nrvr),kcondhgh(nrvr),
     *flzlow(nrvr),flzhgh(nrvr),flzdlt(nrvr),
     *pwrlow(nrvr),pwrhgh(nrvr),pwrdlt(nrvr),
     *stat=iAllocate)
      if(iAllocate.ne.0) STOP
     *   'Error with allocation of area4 (part 2)arrays in bsn'


      write(39,5000)comment
      write(39,1006)j,j,(j,j=jmin,jmax)
      write(39,1006)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
      do i=imax,imin,-1
         write(39,1006)iymin+istep*(i-1),i,(iak(i,j),j=jmin,jmax)
      end do

! CONTOUR COUNT FOR INTERNAL SLOPE:
      read(5,5000)comment
      write(40,5000)comment
!      write(*,5000)comment
      do i=imax,imin,-1
         if(newversion.eq.0)then
           read(5,1003,iostat=ios)(irough_2d(i,j),j=jmin,jmax)
           write(40,4003,iostat=ios)(irough_2d(i,j),j=jmin,jmax)
         else
           read(5,*,iostat=ios)(irough_2d(i,j),j=jmin,jmax)
         endif
         if(ios.ne.0)then
           print*,' Problems reading the no countours on line',i
           print*
           STOP ' Program aborted'
         endif
      end do

!      write(*,1125)
!      do i=imax,imin,-1
!         write(*,1003)(irough_2d(i,j),j=jmin,jmax)
!      end do

      write(39,5000)comment
      write(39,1006)j,j,(j,j=jmin,jmax)
      write(39,1006)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
      do i=imax,imin,-1
         write(39,1006)iymin+istep*(i-1),i,(irough_2d(i,j),j=jmin,jmax)
      end do

! NO OF CHANNELS
      read(5,5000)comment
      write(40,5000)comment
!      write(*,5000)comment
      do i=imax,imin,-1
         if(newversion.eq.0)then
           read(5,1003,iostat=ios)(ichnl_2d(i,j),j=jmin,jmax)
           write(40,1003,iostat=ios)(ichnl_2d(i,j),j=jmin,jmax)
         else
           read(5,*,iostat=ios)( ichnl_2d(i,j),j=jmin,jmax)
         endif
      end do
         if(ios.ne.0)then
           print*,' Problems reading the no channels on line',i
           print*
           STOP ' Program aborted'
         endif

!      write(*,1126)
!      do i=imax,imin,-1
!         write(*,1003)(ichnl_2d(i,j),j=jmin,jmax)
!      end do

      write(39,5000)comment
      write(39,1006)j,j,(j,j=jmin,jmax)
      write(39,1006)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
      do i=imax,imin,-1
         write(39,1006)iymin+istep*(i-1),i,(ichnl_2d(i,j),j=jmin,jmax)
      end do

! DWOPER INPUT NODE
      read(5,5000)comment
      write(40,5000)comment
!      write(*,5000)comment

      do i=imax,imin,-1
         if(newversion.eq.0)then
           read(5,1003,iostat=ios)(ireach_2d(i,j),j=jmin,jmax)
           write(40,4003,iostat=ios)(ireach_2d(i,j),j=jmin,jmax)
         else
           read(5,*,iostat=ios)(ireach_2d(i,j),j=jmin,jmax)
         endif
         if(ios.ne.0)then
           print*,' Problems reading the reach no on line',i
           print*
           STOP ' Program aborted'
         endif
      end do

!      write(*,1126)
!      do i=imax,imin,-1
!         write(*,1003)(ireach_2d(i,j),j=jmin,jmax)
!      end do

      write(39,5000)comment
      write(39,1006)j,j,(j,j=jmin,jmax)
      write(39,1006)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
      do i=imax,imin,-1
         write(39,1006)iymin+istep*(i-1),i,(ireach_2d(i,j),j=jmin,jmax)
      end do

!     pause 7

c! IMPERVIOUS AREA
c      read(5,5000) comment
c      write(40,5000)comment
c!      write(*,5000)comment

c! THERE HAS TO BE AT LEAST ONE MORE CLASS THAN IMPERVIOUS
c      ntemp=max0(2,ntype+1)

! TS - ALLOCATE AREA17 ARRAY ACLASS
c      allocate(aclass_3d(imax,jmax,ntemp+1),ijtemp(imax,jmax),
c     *        stat=iAllocateStatus)
c      if (iAllocateStatus .ne. 0) STOP 
c     *    '**Allocation failed for area17 array in bsna l632**' 


c!     read the impervious class percentages
c!     and write the new_format.map file
c      do i=imax,imin,-1
c        if(newversion.eq.0)then
c!         originally, class was input as a floating point number 
c!         but files were always integer. Time to make the read integer
c!         nk  Sept. 11/04
c          read(5,4050,iostat=ios)(ijtemp(i,j),j=jmin,jmax)
c          write(40,4050)(ijtemp(i,j),j=jmin,jmax)
c          do j=jmin,jmax
c            aclass_3d(i,j,ntype+1)=float(ijtemp(i,j))
c          end do
c        else
c!         new (ensim) format. Integer % only
c          read(5,*,iostat=ios)(ijtemp(i,j),j=jmin,jmax)
c          do j=jmin,jmax
c!           convert to floating point for computations
c            aclass_3d(i,j,ntype+1)=float(ijtemp(i,j))
c          end do
c        endif
c        if(ios.ne.0)then
c          print*,' Problems reading the impervious % on line',i
c          print*
c          STOP ' Program aborted'
c        endif
c      end do
c
c      write(39,5000)comment
c      write(39,1057)j,j,(j,j=jmin,jmax)
c      write(39,1057)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
c      do i=imax,imin,-1
c         write(39,1055)iymin+istep*(i-1),i,
c     *                (aclass_3d(i,j,ntemp+1),j=jmin,jmax)
c      end do


      allocate(aclass_3d(imax,jmax,ntype+1),ijtemp(imax,jmax),
     *        stat=iAllocateStatus)
      if (iAllocateStatus .ne. 0) STOP 
     *    '**Allocation failed for area17 array in bsn @1197**' 


! PERCENTAGES OF CLASSES WHEN NTYPE > 0
      if(ntype.ge.1)then
!        do ii=1,ntype
        do ii=1,ntype+1
          read(5,5000)comment
          write(40,5000)comment
          write(*,5000)comment
          do i=imax,imin,-1
            if(newversion.eq.0)then
              read(5,4050,iostat=ios)(ijtemp(i,j),j=jmin,jmax)
              write(40,4050)(ijtemp(i,j),j=jmin,jmax)
              do j=jmin,jmax
                aclass_3d(i,j,ii)=float(ijtemp(i,j))
              end do
            else
              read(5,*,iostat=ios)(ijtemp(i,j),j=jmin,jmax)
              do j=jmin,jmax
                aclass_3d(i,j,ii)=float(ijtemp(i,j))
              end do
            endif
            if(ios.ne.0)then
              print*,
     *            ' Problems reading the class % on line',i
              print*,' in class= ',comment
              print*
              STOP ' Program aborted'
            endif
          end do
          write(39,5000)comment
          write(39,1057)j,j,(j,j=jmin,jmax)
          write(39,1057)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
          do i=imax,imin,-1  
            write(39,1055)iymin+istep*(i-1),i,
     *           (aclass_3d(i,j,ii),j=jmin,jmax)
 	    end do
	  end do

        print*,' end of map file reached'
          
      else
!     NTYPE=0 ... NO LAND COVER AMOUNTS READ IN
!     SET ALL VALUES FOR ACLASS 1 TO 100%
        ntype=1
        do i=imin,imax
          do j=jmin,jmax
            aclass_3d(i,j,1)=100.0
          end do
        end do
      endif

! MODIFY THE BARREN AREAS BY ADDING 100-IMP % OF THE URBAN AREA
! THAT IS PERVIOUS AND TAKE IMP % OF THE URBAN AREA AS IMPERVIOUS:
! IMPERVIOUS PERCENTAGE:    
	aimp=float(impr)/100.
      if(impr.gt.0)then
!       move part of the URBAN area to class 1
!       PERVIOUS PERCENTAGE:
	  perv=1.-aimp
        do i=imin,imax
          do j=jmin,jmax
!           BUG FIXED HERE JUL 19/00 NK
	      aclass_3d(i,j,1)=aclass_3d(i,j,1)+aclass_3d(i,j,ntype+1)*perv
	      aclass_3d(i,j,ntype+1)=aclass_3d(i,j,ntype+1)*aimp
	    end do
	  end do
	  print*,'Note:    impervious area > 0 in the header'
	  print*,int(perv*100),'% of the impervious class (urban)' 
	  print*,'has been subtracted from class',ntype+1
	  print*,'and added to class 1'
	  print*,'Class 1 should be a land cover compatible with'
        print*,'the pervious areas in urban areas (eg. grass)'
	  print*
      endif

! MODIFY THE PERCENTAGES TO MAKE SURE THEY ADD UP TO 100%
! This is where percentages are converted to fractions !!!!!

!     subdivide the wetlands for coupled & uncoupled 
      if(split.gt.0.0)then
        do i=imin,imax
          do j=jmin,jmax
            aclass_3d(i,j,ntype-2)=aclass_3d(i,j,ntype-2)*(1.0-split)
            aclass_3d(i,j,ntype-1)=aclass_3d(i,j,ntype-1)*split
          end do
	  end do
	endif

      do i=imin,imax
        do j=jmin,jmax
          sumclass=0.0
          do ii=1,ntype+1
	      sumclass=sumclass+aclass_3d(i,j,ii)
          end do
!            if(sumclass.gt.0.0)then 
          if(sumclass.ne.0.0)then   ! changed Mar. 23/06 nk
            do ii=1,ntype+1
              aclass_3d(i,j,ii)=aclass_3d(i,j,ii)/sumclass
            end do
          endif
	  end do
	end do

!     check that grids with 100% water have been assigned a reach number
!     this check added 24/10/05  nk
      error_count=0      
      do n=1,na
        i=yyy(n)
        j=xxx(n)
        if(aclass_3d(i,j,ntype).ge.0.99999)then
          if(ireach_2d(i,j).eq.0)then
	      error_count=error_count+1
            if(error_count.eq.1)then
      	      print*,'A grid with 100% water has not been assigned '
      	      print*,'a reach number. Program will crash if you try'
	        print*,'to use a resume file'
      	      write(51,*)'A grid with 100% water has not been assigned'
      	      write(51,*)'(a) reach number. Program will crash '
	        write(51,*)'if you try to use a resume file'
	        pause 'Hit enter to continue but you have been warned'
	      endif
            write(51,*)'grid,row,col',n,i,j
            print*,'grid,row,col',n,i,j
	      print*
	      aclass_3d(i,j,1)=0.01
	      aclass_3d(i,j,ntype)=0.99
	    endif
	  endif
      end do
      if(error_count.ge.1)then
      	print*,error_count,' grid(s) with 100% water has(ve)'
        print*,' not been assigned '
      	print*,'a reach number(s). The water class has been changed'
        print*,'99% and class 1 has been changed to 1%'
	  pause 'Hit enter to continue but you have been warned AGAIN!'
	endif




! READ THE CHANNEL BANKFULL CAPACITIES IF SET UP IN THE MAP FILE:
	read(5,5000,iostat=ios)comment
!      write(40,5000)comment
!	write(*,5000)comment
	do i=imax,imin,-1
         read(5,*,iostat=ios)(bnkfll_2d(i,j),j=jmin,jmax)
!         write(40,*,iostat=ios)(bnkfll_2d(i,j),j=jmin,jmax)
!         write(*,1053)(bnkfll_2d(i,j),j=jmin,jmax)
      end do


      if(ios.ne.0)then
         print*,'ios=',ios
	   print*
         print*,' No bankfull values found'
         print*,' Default assumed'
!        WE HAVE NOT FOUND BANKFULL DISCHARGES IN THE MAP FILE
!        SO WE CALCULATE THE DEFAULT BANKFULL CAPACITIES INSTEAD
!        FOR NOW, JUST THE DRAINAGE AREA DIVIDED BY 6
!         write(*,6012)
         do i=imax,imin,-1
            do j=jmin,jmax
              bnkfll_2d(i,j)=da_2d(i,j)/6.0+0.1
! 	        if(da_2d(i,j).gt.0.0)print*,i,j,da_2d(i,j),bnkfll_2d(i,j)
            end do
         end do
      endif



! THESE VALUES ARE NO LONGER USED IN SPL BUT ARE EMBEDDED 
! SHD FILE FORMAT SO ARE KEPT FOR COMPATIBILITY WITH OLD FILES.
      ls=0
      ks=0
      js=0
      ih=0
      local=0

      comment=' Grid ranking - highest =1'
      write(39,1006)
      write(39,5000)comment
      write(39,1006)j,j,(j,j=jmin,jmax)
      write(39,1006)j,j,(jxmin+istep*(j-1),j=jmin,jmax) 
      
      do i=imax,1,-1
	  write(39,1058)(rank_2d(i,j),j=1,jmax)
	end do

      do i=1,50
	   chksum(i)=0.0
	end do

      do i=imin,imax
         do j=jmin,jmax
            chksum(i)=chksum(i)+rank_2d(i,j)
	   end do
	end do

!      pause 1

! WRITE THE USER CHECK SECURITY CODE
      write(38,8002)
      write(38,8003) (chksum(i),i=1,50)

      do n=1,na
	   errflg=0	
         i=yyy(n)
         j=xxx(n)
	   if(frac_2d(i,j).le.0.0)then
		 write(*,1129)i,j,frac_2d(i,j)
		 write(39,1129)i,j,frac_2d(i,j)
		 errflg=1
	   endif

	   if(iak(i,j).le.0.or.iak(i,j).gt.16)then
	     write(*,1014)n,i,j,elv_2d(i,j)
	     write(39,1014)n,i,j,elv_2d(i,j)
           errflg=1
	   endif

	   if(irough_2d(i,j).eq.0)then
          write(*,1015)n,i,j,elv_2d(i,j)
           write(39,1015)n,i,j,elv_2d(i,j)
           errflg=1
	   endif

	   if(ichnl_2d(i,j).le.0.or.ichnl_2d(i,j).gt.5)then
           write(*,1016)n,i,j,elv_2d(i,j)
           write(39,1016)n,i,j,elv_2d(i,j)
       		errflg=1
	  endif

	  if(next_2d(i,j).eq.0)then
          write(*,1017)n,i,j,elv_2d(i,j)
          write(39,1017)n,i,j,elv_2d(i,j)
          write(*,1018)
          write(39,1018)
      		write(*,1019)
      		write(39,1019)
          errflg=1
	  endif

	  if(na.le.naa)then
          write(*,*)'fatal error - outlet elements have not been'
          write(*,*)'properly designated'
          write(*,*)'set frac of the recieving element = 0'
	    write(*,*)'elevation of the receiving grid must be > 0.0'
          write(*,*)'location of outlet:'
          write(*,*)rank_2d(xxx(n),yyy(n))
          write(*,*)yyy(n),yorigin+yyy(n)*ydelta
          write(*,*)xxx(n),xorigin+xxx(n)*xdelta
          write(39,6001)
          write(39,6002)
          write(39,6003)
	    write(39,*)'elevation of the receiving grid must be > 0.0'
          write(39,6004)
          write(39,6005)rank_2d(xxx(n),yyy(n))
          write(39,6006)yyy(n)
          write(39,6007)xxx(n)
          print*,' Please see the bsn.err file for error messages'
          print*
          STOP ' program aborted'
	  endif

	  if(next_2d(i,j).le.rank_2d(i,j))then
      		write(39,6015)i,j,rank_2d(i,j),elv_2d(i,j)
	  	errflg=1		
	  endif
      end do

      nullgrids=na-naa

      if(iopt.eq.1)pause 2

!     set all next_2d() values =
      do n=na,naa+1,-1                   ! nk nov 20/02
        next_2d(yyy(n),xxx(n))=0
      end do

!     MAPPING THE SUB-BASIN
!     a sub-basin can only have one outlet!!!!
      
      i=yyy(lastgrid)
      j=xxx(lastgrid)

!     set all rank_2d(i,j) except last grid to -ve
      do n=1,na
        i=yyy(n)
        j=xxx(n)
        if(n.ne.lastgrid)then
          rank_2d(i,j)=rank_2d(i,j)*-1
          next_2d(i,j)=next_2d(i,j)*-1
        endif
      end do
!     leave all rank_2d() and next_2d() outside basin -ve  
!     work from the lastgrid up the sub-watershed to turn 
!     rank_2d() and next_2d() positive

      if(iopt.eq.1)pause 32

!     now set rank_2d() & next_2d() back to +ve for grids in watershed(s)
!     BUT LAST GRID IS ALREADY +VE !!!!!
      do n=lastgrid,1,-1
        i=yyy(n)
        j=xxx(n)
!       next conditional took a while to figure out Jul. 11/05
	  if(lastgrid.le.naa)then   
!         this treats the receiving grids too
          nextgrid=abs(next_2d(i,j))
          k=yyy(nextgrid)
          l=xxx(nextgrid)
!         this puts the upstream grid in the watershed
          if(rank_2d(k,l).gt.0)then
            next_2d(i,j)=next_2d(i,j)*-1    
            rank_2d(i,j)=rank_2d(i,j)*-1     
          endif
	  else
!         for the case where the whole watershed is included
!         all receiving grids need to be included
!         but the last grid rank is already +ve
          next_2d(i,j)=next_2d(i,j)*-1    
!         added this conditional - took 2 days!  Sept. 13/05 NK
          if(rank_2d(i,j).lt.0)then
            rank_2d(i,j)=rank_2d(i,j)*-1     
	    endif
	  endif
      end do                             !<<<<<<ok to here<<<<<<<<<<<<<<<<<<<<<

      if(iopt.eq.1)pause 33

!     count the number of grids in the sub-watershed (could be all)
!     and create a new coordinate matrix
      nogrids=0
      do n=1,na
        i=yyy(n)
        j=xxx(n)
        if(rank_2d(i,j).gt.0)then
          nogrids=nogrids+1
	    newyyy(nogrids)=yyy(n)
	    newxxx(nogrids)=xxx(n)
	    newrank(n)=nogrids
        endif
      end do

      if(iopt.eq.1)pause 4

!     reassign the next grid numbering
      do n=1,na
        i=yyy(n)
        j=xxx(n)
        if(rank_2d(i,j).gt.0)then
          if(n.lt.lastgrid)then
            newnext(n)=newrank(next_2d(i,j))
          else
	      newnext(n)=0
	    endif
	  endif
	end do

      if(iopt.eq.1)pause 5

!     reassign the rank number to remaning grids
      do n=1,na
        i=yyy(n)
        j=xxx(n)
        if(rank_2d(i,j).gt.0)then
	    rank_2d(i,j)=newrank(n)
	  else
	    rank_2d(i,j)=0
        endif
      end do
  
      if(iopt.eq.1)pause 6

!     Remove the blank rows and colums
!     Check for number of blank rows and columns:
      nrows=0
      srows=0
      wcols=0
      ecols=0
      srows=0

      i1flg=0   ! change to 1 as soon as we find an entry
!     scan the rows until we find the first entry     
      do i=1,imax
        do j=1,jmax
          if(rank_2d(i,j).gt.0.and.i1flg.eq.0)then
!           we have found the first line with data
            i1flg=1
            srows=i-1
          endif
        end do
      end do

      if(iopt.eq.1)pause 7

!     do it in reverse from the top (north) down
      i1flg=0
      do i=imax,1,-1
        do j=1,jmax
          if(rank_2d(i,j).gt.0.and.i1flg.eq.0)then
!           we have found the first line with data
            i1flg=1
            nrows=imax-i
          endif
        end do
      end do

      if(iopt.eq.1)pause 8

      j1flg=0   ! change to 1 as soon as we find an entry
!     scan the rows until we find the first entry     
      do j=1,jmax
        do i=1,imax
          if(rank_2d(i,j).gt.0.and.j1flg.eq.0)then
!           we have found the first line with data
            j1flg=1
            wcols=j-1
          endif
        end do
      end do

      if(iopt.eq.1)pause 9

!     do it in reverse from the top (north) down
      j1flg=0
      do j=jmax,1,-1
        do i=1,imax
          if(rank_2d(i,j).gt.0.and.j1flg.eq.0)then
!           we have found the first line with data
            j1flg=1
            ecols=jmax-j
          endif
        end do
      end do

      if(iopt.eq.1)pause 10

!     This will not reduce the border to less than 2 grids
      nrows=max(0,nrows-2)
      srows=max(0,srows-2)
      wcols=max(0,wcols-2)
      ecols=max(0,ecols-2)

      if(nrows.gt.0.or.srows.gt.0.or.wcols.gt.0.or.ecols.gt.0)then
        print*
        print*
        print*,'Grid size for the .shd file has been reduced by'
        print*
        print*,'No. of rows removed north side = ',nrows
        print*,'No. of rows removed south side = ',srows
	  print*,'No. of columns removed west side = ',wcols
	  print*,'No. of columns removed east side = ',ecols
	  print*
        imax=imax-srows-nrows
        jmax=jmax-wcols-ecols
        print*,'There will now be ',imax,' rows and',jmax,' columns'
	  print*,'This is done to reduce the size of the output files'
	  print*
      endif

      if(iopt.eq.1)pause 11

!     incorporate the river par in the shd file  27/07/06 nk

!**********************************************************************
      call rdpar(1,ix,e1)
!**********************************************************************



      print*,'parameter file read'




! WRITE THE SHD FILE TO DISK:
! WRITE THE SHD FILE TO DISK:
! WRITE THE SHD FILE TO DISK:
! WRITE THE SHD FILE TO DISK:
! WRITE THE SHD FILE TO DISK:
! * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

c!     New column channel format channel.shd file:
c
c!     transfer the comment from the map to the shd file
c      do i=1,n_hdr_lines
c        write(41,5007)'#',hdrcomment(i)
c      end do
c      write(41,5012)
c      call date_and_time(cday,time)
c      write(41,5026)time(1:2),time(3:4),time(5:6),
c     *              cday(7:8),cday(5:6),cday(1:4)
c      write(41,5025)fn1  
c      write(41,5012)
c      write(41,5009)coordsys
c      if(coordsys.eq.'LATLONG   ')then
c        write(41,5010)datum
c      elseif(coordsys.eq.'UTM       ')then
c        write(41,5010)datum
c        write(41,5011)zone
c      endif
c      write(41,5012)
c      if(coordsys.eq.'LATLONG   ')then
c!        write(41,5013)xoriginDegree+wcols*xdelta
c!        write(41,5014)yoriginDegree+srows*ydelta
c        write(41,5013)xorigin+wcols*xdelta
c        write(41,5014)yorigin+srows*ydelta
c      else
c        write(41,50131)xorigin+wcols*xdelta
c        write(41,50141)yorigin+srows*ydelta
c	endif
c      write(41,5012)
c      write(41,5015)xcount-wcols-ecols
c      write(41,5016)ycount-srows-nrows
c      if(coordsys.eq.'LATLONG   ')then
c        write(41,5017)xdelta
c        write(41,5018)ydelta
c	else
c        write(41,50171)xdelta
c        write(41,50181)ydelta
c	endif
c      write(41,5012)
c      write(41,50182)al
c      write(41,5019)cintv
c      write(41,5020)aimpr
c      write(41,5021)int(antype)
c      write(41,5027)nrvr
c      write(41,5022)elvconv
c      write(41,5023)
c
c!     fix fix  do these write(37 's need to be moved down?
c
c      if(na.eq.nogrids)then
c        write(37,7001)na,imax,jmax,ls,ks,js,ijk,ih,local,
c     *                       imax/2,jmax/2,naa
c       write(41,5028)na
c        write(41,5029)naa
c      else
c!       for this case there can only be one outlet
cc        write(37,7001)
cc     *     nogrids,imax,jmax,ls,ks,js,ijk,ih,local,
cc     *                         imax/2,jmax/2,nogrids-1
c        write(41,5028)nogrids
c        write(41,5029)nogrids-1
cendif
c
c     write(41,5030)na/2
c     write(41,5033)     !  #
c     write(41,5032)     !  enddheader


!     New format bsnm.shd file:
!     New format bsnm.shd file:
!     New format bsnm.shd file:

!     transfer the comment from the map to the shd file
      do i=1,n_hdr_lines
        write(36,5007)'#',hdrcomment(i)
      end do
      write(36,5012)
      call date_and_time(cday,time)
      write(36,5026)time(1:2),time(3:4),time(5:6),
     *              cday(7:8),cday(5:6),cday(1:4)
      write(36,5025)fn1  
      write(36,5012)
      write(36,5009)coordsys
      if(coordsys.eq.'LATLONG   ')then
        write(36,5010)datum
      elseif(coordsys.eq.'UTM       ')then
        write(36,5010)datum
        write(36,5011)zone
      endif
      write(36,5012)
      if(coordsys.eq.'LATLONG   ')then
!        write(36,5013)xoriginDegree+wcols*xdelta
!        write(36,5014)yoriginDegree+srows*ydelta
        write(36,5013)xorigin+wcols*xdelta
        write(36,5014)yorigin+srows*ydelta
      else
        write(36,50131)xorigin+wcols*xdelta
        write(36,50141)yorigin+srows*ydelta
	endif
      write(36,5012)
      write(36,5015)xcount-wcols-ecols
      write(36,5016)ycount-srows-nrows
      if(coordsys.eq.'LATLONG   ')then
        write(36,5017)xdelta
        write(36,5018)ydelta
	else
        write(36,50171)xdelta
        write(36,50181)ydelta
	endif
      write(36,5012)
      write(36,50182)al
      write(36,5019)cintv
      write(36,5020)aimpr
      write(36,5021)ntype
      write(36,5027)nrvr
      write(36,5022)elvconv
      write(36,5023)

!     fix fix  do these write(37 's need to be moved down?

      if(na.eq.nogrids)then
        write(37,7001)na,imax,jmax,ls,ks,js,ijk,ih,local,
     *                       imax/2,jmax/2,naa
        write(36,5028)na
        write(36,5029)naa
      else
!       for this case there can only be one outlet
        write(37,7001)
     *     nogrids,imax,jmax,ls,ks,js,ijk,ih,local,
     *                         imax/2,jmax/2,nogrids-1
        write(36,5028)nogrids
        write(36,5029)nogrids-1
	endif

      write(36,5030)na/2
      write(36,5033)     !  #
      write(36,5032)     !  enddheader





      if(iopt.eq.2)pause 12

!     Old format shed file
!     Old format shed file
!     Old format shed file

      write(37,7021) al,cintv,nrvr,ntype,fn1
!      write(37,7021) al,cintv,impr,ntype,fn1
!     impr is used only in bsn.for so not needed in spl
!     use space of nrvr - Oct. 16/01 NK

! CHECK FOR COORDINATE SYSTEM
      if(coordsys.eq.'UTM      ') then
!        FOR UTM COORDINATES
         write(37,7005)iymin+srows,iymax-nrows,jxmin+wcols,jxmax-ecols
      elseif(coordsys.eq.'CARTESIAN ')then
!        for CARTESIAN coordinates
         write(37,7005)iymin+srows,iymax-nrows,jxmin+wcols,jxmax-ecols
      else
!        FOR LAT-LONG COORDINATES
         iiiii=-1

!     the jx and iy's are reversed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        if(nrows.gt.0.or.srows.gt.0.or.wcols.gt.0.or.ecols.gt.0)then
          print*,' Old extent of the grid in minutes:'
          print*,iymin,iymax,jxmin,jxmax
        endif

!     reduce the watflood domain by removing blank rows & colums
        iymin=iymin+int(real(srows)*yint)
        iymax=iymax-int(real(nrows)*yint)
        jxmin=jxmin+int(real(wcols)*xint)
        jxmax=jxmax-int(real(ecols)*xint)

        if(nrows.gt.0.or.srows.gt.0.or.wcols.gt.0.or.ecols.gt.0)then
          print*,' New extent of the grid in minutes:'
          print*,iymin,iymax,jxmin,jxmax
        endif

        latsdeg=int(real(iymin)/60.0)
        latsmin=iymin-int(real(latsdeg)*60.0)
        latndeg=int(real(iymax)/60.0) 
        latnmin=iymax-int(real(latndeg)*60.0)

        lonwdeg=int(real(jxmin)/60.0)
        lonwmin=jxmin-int(real(lonwdeg)*60.0)
        lonedeg=int(real(jxmax)/60.0)
        lonemin=jxmax-int(real(lonedeg)*60.0)

        if(nrows.gt.0.or.srows.gt.0.or.wcols.gt.0.or.ecols.gt.0)then
          print*,' New extent of the grid in degrees & minutes:'
          print*,latsdeg,latsmin,latndeg,latnmin
          print*,lonwdeg,lonwmin,lonedeg,lonemin  
          print*,' x interval =',xint,'y interval =',yint
          print*
        endif

        write(37,1005)iiiii,iiiii,iiiii,iiiii,latsdeg,latsmin,latndeg,
     *                 latnmin,lonwdeg,lonwmin,lonedeg,lonemin,yint,xint
      endif
      


      if(iopt.eq.2)pause 13

!     this is already printed above
	write(37,7006) imax,jmax
!      write(37,7006) (jl(i),jr(i),i=ib,it)


      write(36,*)'The ranks of each grid: highest = 1'
	do i=imax,imin,-1
        if(na.le.9999)then
	    write(36,1006)(rank_2d(i+srows,j+wcols),j=jmin,jmax)
c	    write(41,1006)(rank_2d(i+srows,j+wcols),j=jmin,jmax)
	    write(37,1006)(rank_2d(i+srows,j+wcols),j=jmin,jmax)
        else
          write(36,1058)(rank_2d(i+srows,j+wcols),j=jmin,jmax)
c          write(41,1058)(rank_2d(i+srows,j+wcols),j=jmin,jmax)
          write(37,1058)(rank_2d(i+srows,j+wcols),j=jmin,jmax)
      endif
	   write(39,1006)iymin+istep*(i-1),i,(rank_2d(i,j),j=jmin,jmax)
 	end do


!     splice from move of section above.

      if(iopt.eq.2)pause 14

      write(36,*)'   n, next, row, col,   da,    bankfull, cha_slope,',
     *'  elv, ch_lenth,iak,int_slope,chnl,reach,frac,imperv',
     *' classes 1 -',ntype 


c     write(41,*)'   n, next, row, col,   da,    bankfull, cha_slope,',
c    *'  elv, ch_lenth,iak,   flz,      pwr,      R1n,',
c    *'      R2n,     mndr,',
c    *'      aa2,      aa3,      aa4,    theta,    widep,',
c    *'    kcond,int_slope,chnl,reach,frac,',
c    *' classes 1 -',ntype+1  


!     added Jun. 1/04  NK
!     changed sl1(n) to the internal slope. 
!     Was sqrt if int. slope but that was misleading when 
!     looking at the shd file.  nk jul 27/04

c      write(1000,*)'contour interval =',cintv,'  in bsn @ 1946'
      do n=1,na
        i=yyy(n)
        j=xxx(n)
        sl1(n)=cintv*(float(irough_2d(i,j))+.0001)/al
c	  write(1000,*)n,i,j,irough_2d(i,j),sl1(n)
      end do

      if(split.le.0.0)then

!      wetlands are not split into the stream coupled and upland type
!      value for split is read in the old format file 2nd line.
!      FIX:  needs to be added to the new format file.
       print*,'na,naa/',na,naa

       do n=1,na
        i=yyy(n)
        j=xxx(n)
        nn=newrank(n)
        m=iak(i,j)
        if(n.gt.naa)newnext(n)=0
        if(n.eq.lastgrid)da_2d(i,j)=0.0
        if(rank_2d(i,j).gt.0)then
          if(da_2d(i,j).gt.10.0)then
!           new format
           write(36,1025)nn,newnext(n),yyy(n)-srows,xxx(n)-wcols,
     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),
     *        elv_2d(i,j),ch_length_2d(i,j),
     *        iak(i,j),sl1(n),ichnl_2d(i,j),ireach_2d(i,j),
     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),
     *        (aclass_3d(i,j,ii),ii=1,ntype)
!     *        frac_2d(i,j),(aclass_3d(i,j,ii),ii=1,ntype+1)
!          channels included format
c           write(41,1027)nn,newnext(n),yyy(n)-srows,xxx(n)-wcols,
c     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),
c     *        elv_2d(i,j),ch_length_2d(i,j),
c     *        iak(i,j),
c     *        flz(m),pwr(m),R1(m),R2(m),mndr(m),aa2(m),aa3(m),aa4(m),
c     *        theta(m),widep(m),kcond(m),
c     *        sl1(n),ichnl_2d(i,j),ireach_2d(i,j),
c     *        frac_2d(i,j),(aclass_3d(i,j,ii),ii=1,ntype+1)
c!     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),(aclass_3d(i,j,ii),ii=1,ntype)
!           old format
            write(37,1010)nn,yyy(n)-srows,xxx(n)-wcols,
     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),elv_2d(i,j),
     *        iak(i,j),irough_2d(i,j),ichnl_2d(i,j),
     *        newnext(n),ireach_2d(i,j),
     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),
     *        (aclass_3d(i,j,ii),ii=1,ntype)
!     *        frac_2d(i,j),(aclass_3d(i,j,ii),ii=1,ntype+1)
	    else
!           new format
            write(36,1026)nn,newnext(n),yyy(n)-srows,xxx(n)-wcols,
     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),
     *        elv_2d(i,j),ch_length_2d(i,j),
     *        iak(i,j),sl1(n),ichnl_2d(i,j),ireach_2d(i,j),
     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),
     *        (aclass_3d(i,j,ii),ii=1,ntype)
!     *        frac_2d(i,j),(aclass_3d(i,j,ii),ii=1,ntype+1)
!           channels included format
c            write(41,1028)nn,newnext(n),yyy(n)-srows,xxx(n)-wcols,
c     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),
c     *        elv_2d(i,j),ch_length_2d(i,j),
c     *        iak(i,j),
c     *        flz(m),pwr(m),R1(m),R2(m),mndr(m),aa2(m),aa3(m),aa4(m),
c     *        theta(m),widep(m),kcond(m),
c     *        sl1(n),ichnl_2d(i,j),ireach_2d(i,j),
c     *        frac_2d(i,j),(aclass_3d(i,j,ii),ii=1,ntype+1)
c!     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),(aclass_3d(i,j,ii),ii=1,ntype)
!           old format
            write(37,1011)nn,yyy(n)-srows,xxx(n)-wcols,
     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),elv_2d(i,j),
     *        iak(i,j),irough_2d(i,j),ichnl_2d(i,j),
     *        newnext(n),ireach_2d(i,j),
     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),
     *        (aclass_3d(i,j,ii),ii=1,ntype)
!     *        frac_2d(i,j),(aclass_3d(i,j,ii),ii=1,ntype+1)
	    endif
        endif
       end do

      else    ! if(split.gt.0.0)

!      split > 0
!      wetlands are split into the stream coupled and upland type
!      according to the split

!      continue writing the .shd file
       do n=1,na
        i=yyy(n)
        j=xxx(n)
        nn=newrank(n)
        if(n.gt.naa)newnext(n)=0
        if(n.eq.lastgrid)da_2d(i,j)=0.0
        if(rank_2d(i,j).gt.0)then
          if(da_2d(i,j).gt.10.0)then
!           new format
            write(36,1025)nn,newnext(n),yyy(n)-srows,xxx(n)-wcols,
     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),
     *        elv_2d(i,j),ch_length_2d(i,j),
     *        iak(i,j),sl1(n),ichnl_2d(i,j),ireach_2d(i,j),
     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),
     *        (aclass_3d(i,j,ii),ii=1,ntype-3),
     *        aclass_3d(i,j,ntype-2)*(1.0-split),
     *        aclass_3d(i,j,ntype-1)*split,
     *        aclass_3d(i,j,ntype)
!           old format
            write(37,1010)nn,yyy(n)-srows,xxx(n)-wcols,
     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),elv_2d(i,j),
     *        iak(i,j),irough_2d(i,j),ichnl_2d(i,j),
     *        newnext(n),ireach_2d(i,j),
     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),
     *        (aclass_3d(i,j,ii),ii=1,ntype-3),
     *        aclass_3d(i,j,ntype-2)*(1.0-split),
     *        aclass_3d(i,j,ntype-1)*split,
     *        aclass_3d(i,j,ntype)
	    else
!           new format
            write(36,1026)nn,newnext(n),yyy(n)-srows,xxx(n)-wcols,
     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),
     *        elv_2d(i,j),ch_length_2d(i,j),
     *        iak(i,j),sl1(n),ichnl_2d(i,j),ireach_2d(i,j),
     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),
     *        (aclass_3d(i,j,ii),ii=1,ntype-3),
     *        aclass_3d(i,j,ntype-2)*(1.0-split),
     *        aclass_3d(i,j,ntype-1)*split,
     *        aclass_3d(i,j,ntype)
!           old format
            write(37,1011)nn,yyy(n)-srows,xxx(n)-wcols,
     *        da_2d(i,j),bnkfll_2d(i,j),slope_2d(i,j),elv_2d(i,j),
     *        iak(i,j),irough_2d(i,j),ichnl_2d(i,j),
     *        newnext(n),ireach_2d(i,j),
     *        frac_2d(i,j),aclass_3d(i,j,ntype+1),
     *        (aclass_3d(i,j,ii),ii=1,ntype-3),
     *        aclass_3d(i,j,ntype-2)*(1.0-split),
     *        aclass_3d(i,j,ntype-1)*split,
     *        aclass_3d(i,j,ntype)
	    endif
        endif
	  if(errflg.eq.1)then
!!!		  write(*,1019)
	    write(39,1019)
!     	  PAUSE '7 - Hit key to continue'
	  endif
       end do
      endif    !  if(split.le.0.0)


      close(unit=36,status='keep')
      close(unit=37,status='keep')
c     close(unit=41,status='keep')

!     pause 9



!     write the shd.r2c file for watroute in r2c format
!     write the shd.r2c file for watroute in r2c format
!     write the shd.r2c file for watroute in r2c format
!     write the shd.r2c file for watroute in r2c format
!     write the shd.r2c file for watroute in r2c format

      open(41,file='new_shd.r2c',status='unknown',iostat=ios)
      if(ios.ne.0)then
         print*,' Problems opening the new_shd.r2c file '
         print*,' Possible cause: file open or read only'
         print*
         STOP ' Program BSN aborted @ 1966'
      endif

      write(41,3005)'########################################'
      write(41,3005)':FileType r2c  ASCII  EnSim 1.0         '
      write(41,3005)'#                                       '
	write(41,3005)'# DataType               2D Rect Cell   '
      write(41,3005)'#                                       '
      write(41,3005)':Application             EnSimHydrologic'
	write(41,3005)':Version                 2.1.23         '
	write(41,3020)':WrittenBy          ',author
      call date_and_time(cday,time)
	write(41,3010)':CreationDate       ',
     *       cday(1:4),cday(5:6),cday(7:8),time(1:2),time(3:4)
3010  format(a20,a4,'-',a2,'-',a2,2x,a2,':',a2)
      write(41,3005)'#                                       '
	write(41,3005)'#---------------------------------------'
      write(41,3020)':SourceFileName     ',fn1
      write(41,50182)al
      write(41,5019)cintv
      write(41,5020)aimpr
      write(41,5021)ntype+1
      write(41,5027)nrvr
      write(41,5022)elvconv
      if(na.eq.nogrids)then
        write(41,5028)na
        write(41,5029)naa
      else
!       for this case there can only be one outlet
        write(41,5028)nogrids
        write(41,5029)nogrids-1
	endif
      write(41,5030)na/2
      write(41,3005)'#                                       '
      write(41,3005)'#                                       '
	write(41,3004)':Projection         ',coordsys
	if(coordsys.eq.'UTM       ')then
          write(41,3004)':Zone               ',zone
	endif
	write(41,3004)':Ellipsoid          ',datum
      write(41,3005)'#                                       '
      write(41,3003)':xOrigin            ',xorigin+wcols*xdelta
      write(41,3003)':yOrigin            ',yorigin+srows*ydelta
      write(41,3005)'#                                       '
      write(41,3008)':AttributeName 1 Rank         ' 
      write(41,3008)':AttributeName 2 Next         '  
      write(41,3008)':AttributeName 3 DA           '
      write(41,3008)':AttributeName 4 Bankfull     ' 
      write(41,3008)':AttributeName 5 ChnlSlope    ' 
      write(41,3008)':AttributeName 6 Elev         '
      write(41,3008)':AttributeName 7 ChnlLength   ' 
      write(41,3008)':AttributeName 8 IAK          '
      write(41,3008)':AttributeName 9 IntSlope     '
      write(41,3008)':AttributeName 10 Chnl        '
      write(41,3008)':AttributeName 11 Reach       '
!     removed these Jan. 29/07 NK after talk with Al.
!      write(41,3008)':AttributeName 12 Flz         '
!      write(41,3008)':AttributeName 13 Pwr         '
!      write(41,3008)':AttributeName 14 R1n         '
!      write(41,3008)':AttributeName 15 R2n         '
!      write(41,3008)':AttributeName 16 mndr        '
!      write(41,3008)':AttributeName 17 aa2         '
!      write(41,3008)':AttributeName 18 aa3         '
!      write(41,3008)':AttributeName 19 aa4         '
!      write(41,3008)':AttributeName 20 widep       '
!      write(41,3008)':AttributeName 21 Frac        '
c      write(41,3008)':AttributeName 21 GridArea    '
      write(41,3008)':AttributeName 12 GridArea    '
      do i=1,ntype+1
        write(41,3009)':AttributeName',12+i,' Class',i
	end do

      write(41,3005)'#                                       '
      write(41,3001)':xCount             ',xcount-wcols-ecols
      write(41,3001)':yCount             ',ycount-srows-nrows
      write(41,3003)':xDelta             ',xdelta
      write(41,3003)':yDelta             ',ydelta
      write(41,3005)'#                                       '
      write(41,3005)':EndHeader                              '

      do i=1,ycount
	  do j=1,xcount
	    dummy(i,j)=0.0
	  end do
	end do
      frame=1
c      rank_2d(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,1058)(rank_2d(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      next_2d(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,1058)(next_2d(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      da_2d(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,4012)(da_2d(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      bnkfll_2d(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,4002)(bnkfll_2d(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      slope_2d(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,4005)(slope_2d(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      elv_2d(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,4006)(elv_2d(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      ch_length_2d(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,4007)(ch_length_2d(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      iak(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,1058)(iak(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'


      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=sl1(n)
      end do
	frame=frame+1
c      dummy(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,4005)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      ichnl_2d(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,1058)(ichnl_2d(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      ireach_2d(1,1)=frame
	do i=1+srows,ycount-nrows
        write(41,1058)(ireach_2d(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

	frame=frame+1
c      frac_2d(1,1)=frame
!     write 'gridarea'
	do i=1+srows,ycount-nrows
        write(41,4012)(frac_2d(i,j)*al*al,j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'

c      if(split.eq.0.0)then
        do ii=1,ntype+1
          frame=frame+1
c         aclass_3d(1,1,ii)=frame
          do i=1+srows,ycount-nrows
            write(41,4008)(aclass_3d(i,j,ii),j=1+wcols,xcount-ecols)
          end do
          print*,' frame= ',frame,' written'
	  end do
c	else   ! split > 0.0
c        do ii=1,ntype-3
c          frame=frame+1
cc         aclass_3d(1,1,ii)=frame
c          do i=1+srows,ycount-nrows
c            write(41,4008)(aclass_3d(i,j,ii),j=1+wcols,xcount-ecols)
c          end do
c          print*,' frame= ',frame,' written'
c	  end do
c!       write the % uncoupled wetland
c	  ii=ntype-2
c        do i=1+srows,ycount-nrows
c          write(41,4008)(aclass_3d(i,j,ii)*(1.0-split),
c     *                            j=1+wcols,xcount-ecols)
c        end do
c        print*,' frame= ',frame,' written'
c!       write the % coupled wetland
c	  ii=ntype-1    
c        do i=1+srows,ycount-nrows
c          write(41,4008)(aclass_3d(i,j,ii)*split,
c     *                            j=1+wcols,xcount-ecols)
c        end do
c        print*,' frame= ',frame,' written'
c!       write the water & impervious classes
c        do ii=ntype,ntype+1
c          frame=frame+1
cc         aclass_3d(1,1,ii)=frame
c          do i=1+srows,ycount-nrows
c            write(41,4008)(aclass_3d(i,j,ii),j=1+wcols,xcount-ecols)
c          end do
c          print*,' frame= ',frame,' written'
c	  end do
c	endif

      close(unit=41,status='keep')

      print*,'new_shd.r2c written'



!     write the par.r2c file for watroute in r2c format
!     write the par.r2c file for watroute in r2c format
!     write the par.r2c file for watroute in r2c format
!     write the par.r2c file for watroute in r2c format

      open(41,file='new_ch_par.r2c',status='unknown',iostat=ios)
      if(ios.ne.0)then
         print*,' Problems opening the new_par.r2c file '
         print*,' Possible cause: file open or read only'
         print*
         STOP ' Program BSN aborted @ 2358'
      endif

      write(41,3005)'########################################'
      write(41,3005)':FileType r2c  ASCII  EnSim 1.0         '
      write(41,3005)'#                                       '
	write(41,3005)'# DataType               2D Rect Cell   '
      write(41,3005)'#                                       '
      write(41,3005)':Application             EnSimHydrologic'
	write(41,3005)':Version                 2.1.23         '
	write(41,3020)':WrittenBy          ',author
      call date_and_time(cday,time)
	write(41,3010)':CreationDate       ',
     *       cday(1:4),cday(5:6),cday(7:8),time(1:2),time(3:4)
      write(41,3005)'#                                       '
	write(41,3005)'#---------------------------------------'
      write(41,3020)':SourceFileName     ',fln(2)
      write(41,50182)al
      write(41,5019)cintv
      write(41,5020)aimpr
      write(41,5021)ntype+1
      write(41,5027)nrvr
      write(41,5022)elvconv
      if(na.eq.nogrids)then
        write(41,5028)na
        write(41,5029)naa
      else
!       for this case there can only be one outlet
        write(41,5028)nogrids
        write(41,5029)nogrids-1
	endif
      write(41,5030)na/2
      write(41,3005)'#                                       '
      write(41,3005)'#                                       '
	write(41,3004)':Projection         ',coordsys
	if(coordsys.eq.'UTM       ')then
          write(41,3004)':Zone               ',zone
	endif
	write(41,3004)':Ellipsoid          ',datum
      write(41,3005)'#                                       '
      write(41,3003)':xOrigin            ',xorigin+wcols*xdelta
      write(41,3003)':yOrigin            ',yorigin+srows*ydelta
      write(41,3005)'#                                       '
      write(41,3008)':AttributeName 1 Flz         '
      write(41,3008)':AttributeName 2 Pwr         '
      write(41,3008)':AttributeName 3 R1n         '
      write(41,3008)':AttributeName 4 R2n         '
      write(41,3008)':AttributeName 5 mndr        '
      write(41,3008)':AttributeName 6 aa2         '
      write(41,3008)':AttributeName 7 aa3         '
      write(41,3008)':AttributeName 8 aa4         '
      write(41,3008)':AttributeName 9 theta       '
      write(41,3008)':AttributeName 10 widep      '
      write(41,3008)':AttributeName 11 kcond      '


      write(41,3005)'#          5                             '
      write(41,3001)':xCount             ',xcount-wcols-ecols
      write(41,3001)':yCount             ',ycount-srows-nrows
      write(41,3003)':xDelta             ',xdelta
      write(41,3003)':yDelta             ',ydelta
      write(41,3005)'#                                       '
      write(41,3005)':EndHeader                              '

      do i=1,ycount
	  do j=1,xcount
	    dummy(i,j)=0.0
	  end do
	end do
      frame=1
!     note:  iak(i,j)=ibn(n)=basin number
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=flz(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4009)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
      print*,' frame= ',frame,' written'
      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=pwr(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4011)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=R1n(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4010)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=R2n(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4010)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=mndr(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4011)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=aa2(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4004)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=aa3(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4004)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=aa4(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4004)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=theta(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4001)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=widep(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4001)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'

      do n=1,naa
        i=yyy(n)
        j=xxx(n)
        dummy(i,j)=kcond(iak(i,j))
      end do
	do i=1+srows,ycount-nrows
        write(41,4001)(dummy(i,j),j=1+wcols,xcount-ecols)
      end do
	frame=frame+1
      print*,' frame= ',frame,' written'
	print*
      close(unit=41,status='keep')
      print*,'new_ch_par.r2c written'
	print*

!     end of writing the par file for watroute in r2c format




!     Write the wfo_spec.new file
!     Write the wfo_spec.new file
!     Write the wfo_spec.new file
!     Write the wfo_spec.new file
!     Write the wfo_spec.new file
!     Write the wfo_spec.new file

!     This section is set up so new items can be easily added
!     by the auto-numbering sequence.
!     Don't forget to change the attribute count

      open(unit=99,file='wfo_spec.new',status='unknown',iostat=ios)
      if(ios.ne.0)then
        print*,' unable to open a new wfo_spec.new file'
        print*,' program not aborted but file not written'
        print*
      endif
!     rev. 9.1.50  Jan.  14/04  - NK: version number added to the wfo_spec.txt file
      write(99,99000)       
!     Attribute Count
      write(99,99001)12+15*(ntype+1)
!     Reporting time step - can be changed by user in wfo_spec.txt
      write(99,99002)            
!     Start Reporting       Added Jan. 14/04 NK
      write(99,99902)
!     End Reporting       Added Jan. 14/04 NK
      write(99,99903)
!     Temperature
      j=1                   !  j is the attribute number
      write(99,99003)j
!     Precipitation
      j=j+1
      write(99,99004)j
!     CummulativePrecipitation    Added June 11/03 NK
      j=j+1
      write(99,99104)j
!     lower zone storage
      j=j+1
      write(99,99005)j
!     ground water discharge
      j=j+1
      write(99,99006)j
!     grid runoff
      j=j+1
      write(99,99007)j
!     grid outflow      
      j=j+1
      write(99,99008)j
!     weighted swe
      j=j+1
      write(99,99009)j
!     wetland depth
      j=j+1
      write(99,99010)j
!     channel depth
      j=j+1
      write(99,99011)j
!     wetland storage
      j=j+1
      write(99,99012)j
!     wetland outflow
      j=j+1
      write(99,99013)j
!     depression storage
      do i=1,(ntype+1)
        j=j+1
        write(99,99014)j,i
      end do
!     depression storage (snow)
      do i=1,(ntype+1)
        j=j+1
        write(99,99015)j,i
      end do
! snow water equivalent
      do i=1,(ntype+1)
        j=j+1
        write(99,99016)j,i
      end do
!     snow covered area
      do i=1,(ntype+1)
        j=j+1
        write(99,99017)j,i
      end do
!     upper zone storage
      do i=1,(ntype+1)
        j=j+1
        write(99,99018)j,i
      end do
!     upper zone storage (snow)
      do i=1,(ntype+1)
        j=j+1
        write(99,99019)j,i
      end do
!     surface flow from bare area
      do i=1,(ntype+1)
        j=j+1
        write(99,99020)j,i
      end do
!     surface flow from snow covered ground
      do i=1,(ntype+1)
        j=j+1
        write(99,99021)j,i
      end do
!     interflow from bare ground
      do i=1,(ntype+1)
        j=j+1
        write(99,99022)j,i
      end do
!     interflow from snow covered ground
      do i=1,(ntype+1)
        j=j+1
        write(99,99023)j,i
      end do
!     drainage from bare ground
      do i=1,(ntype+1)
        j=j+1
        write(99,99024)j,i
      end do
!     drainage from snow covered ground
      do i=1,(ntype+1)
        j=j+1
        write(99,99025)j,i
      end do
!     PET 
      do i=1,(ntype+1)
        j=j+1
        write(99,99026)j,i
      end do
!     ET 
      do i=1,(ntype+1)
        j=j+1
        write(99,99027)j,i
      end do
!     Sublimation from snow 
      do i=1,(ntype+1)
        j=j+1
        write(99,99028)j,i
      end do
    
      close(unit=99)

      print*,'wfo_spec.new  written'
	print*
!     finished writing wfo_spec.new 



!	write the new.pdl file
!	write the new.pdl file
!	write the new.pdl file


      open(unit=33,file='new.pdl',recl=flen,status='unknown')
      if(ios.ne.0)then
        print*,' problems opening file new.pdl   ios=',ios
        stop 'program aborted in inpgrd.for @ 44'
      endif

      nnprint=0
      newformat=0
!     rev. 9.1.58  Jul.  12/04  - NK: New header for the .shd file
      write(33,5012)
      write(33,33001)':FileType             bsnm.pdl'
      write(33,5009)coordsys
      write(33,5010)datum
      write(33,5011)zone
      write(33,5012)

      if(coordsys.eq.'LATLONG   ')then
        write(33,5013)xorigin+wcols*xdelta
        write(33,5014)yorigin+srows*ydelta
      else
        write(33,50131)xorigin+wcols*xdelta
        write(33,50141)yorigin+srows*ydelta
	endif
      write(33,5012)
      write(33,5015)xcount-wcols-ecols
      write(33,5016)ycount-srows-nrows
      if(coordsys.eq.'LATLONG   ')then
        write(33,5017)xdelta
        write(33,5018)ydelta
	else
        write(33,50171)xdelta
        write(33,50181)ydelta
	endif

 

	ewgrid=xorigin+float(wcols)*xdelta+
     *           (float(xcount-wcols-ecols)/2.)*xdelta
	sngrid=yorigin+float(srows)*ydelta+
     *           (float(ycount-srows-nrows)/2.)*ydelta

      write(33,5012)
      write(33,33000)':NoPrecipStations     1'
      write(33,5012)
      if(llflg.ne.'y')then
!       FOR UTM 
          write(33,*)ewgrid,sngrid,'  centerville'
      else
!       FOR LAT-LONG
          write(33,*)ewgrid,sngrid,'  centerville'
      endif

      write(33,5012)
      write(33,33000)':NoSnowCourses        1'
      write(33,5012)
      if(llflg.ne.'y')then
!       FOR UTM 
          write(33,*)ewgrid,sngrid,'  centerville'
      else
!       FOR LAT-LONG
          write(33,*)ewgrid,sngrid,'  centervill'
      endif

      write(33,5012)
      write(33,33000)':NoTempStations       1'
      write(33,5012)
      if(llflg.ne.'y')then
!       FOR UTM 
          write(33,*)ewgrid,sngrid,'  centerville'
      else
!       FOR LAT-LONG
        write(33,*)ewgrid,sngrid,'  centerville'
      endif

      write(33,5012)
      write(33,33000)':NoFlowStations       1'
      write(33,5012)
      if(llflg.ne.'y')then
!       FOR UTM 
        write(33,*)ewgrid,sngrid,'  centerville'
      else
!       FOR LAT-LONG
        write(33,*)ewgrid,sngrid,'  centerville'
      endif

      write(33,5012)
      write(33,33000)':NoReservoirs         1'
      write(33,5012)
      if(llflg.ne.'y')then
!       FOR UTM 
        write(33,*)ewgrid,sngrid,'  centerville'
      else
!       FOR LAT-LONG
        write(33,*)ewgrid,sngrid,'  centerville'
      endif

      write(33,5012)
      write(33,33000)':NoDamageSites        1'
      write(33,5012)

33000  format(a23,i5)
33001  format(a30)

      close(unit=33, status='keep')
      print*,'new.pdl  written'
	print*


!     pause 10

      if(na-naa.le.1000)then

!     PROFILES
!     this section only works for a single outlet
      filenames(1)='profil01.dat'
      filenames(2)='profil02.dat'
      filenames(3)='profil03.dat'
      filenames(4)='profil04.dat'
      filenames(5)='profil05.dat'
      filenames(6)='profil06.dat'
      filenames(7)='profil07.dat'
      filenames(8)='profil08.dat'
      filenames(9)='profil09.dat'
      filenames(10)='profil10.dat'
      filenames(11)='river01.dat'
      filenames(12)='river02.dat'
      filenames(13)='river03.dat'
      filenames(14)='river04.dat'
      filenames(15)='river05.dat'
      filenames(16)='river06.dat'
      filenames(17)='river07.dat'
      filenames(18)='river08.dat'
      filenames(19)='river09.dat'
      filenames(20)='river10.dat'

!     pause 11

!     compute the distance of each grid to the outlet
      do n=1,na
        distance(n)=0.0
        maxlength=0.0
        longest=0
        channel(n)=0
      end do

!     pause 12

!     work up the watershed and compute the distance to outlet for each grid
      do n=naa,1,-1
        i=yyy(n)
        j=xxx(n)
!       for grids outside the sub-watershed, rank = 0
        if(rank_2d(i,j).gt.0)then  !new
          distance(n)=distance(abs(newnext(n)))+al
!         find the grid with the longest path
          if(distance(n).gt.maxlength)then
            maxlength=distance(n)
            longest=n          
          endif
        endif                     !new
      end do

!     pause 13

!     mark off the longest channel
      n=longest
      do while(n.le.naa)
        channel(n)=1
        i=yyy(n)
        j=xxx(n)
        n=abs(next_2d(i,j))
      end do

!     pause 14

      open(unit=98,file=filenames(1),status='unknown',iostat=ios)
      open(unit=99,file=filenames(11),status='unknown',iostat=ios)
      if(ios.ne.0)then
        stop 'Program aborted @ 992, can`t open distance.dat file'
      endif
!     write stuff for the main stem
      do n=1,na
        if(channel(n).eq.1)then
          i=yyy(n)
          j=xxx(n)
          nn=nn+1
          write(98,*)n,distance(n),elv_2d(i,j),channel(n)
          write(99,*)j,i,elv_2d(i,j)
!          print*,n,distance(n),elv_2d(i,j),channel(n)
        endif
      end do
      close(unit=98,status='keep')
      close(unit=99,status='keep')
        print*,'finished writing ',filenames(1)
        print*,'finished writing ',filenames(11)

!     pause 15

!     write stuff for the tributaries
      do k=2,10
!     check the length of each tributary and find longest unmarked:
        longest=0
        maxlength=0.0
        do n=1,naa
          tempsum=al
          m=n
          do while(channel(m).eq.0)
            tempsum=tempsum+al
            i=yyy(m)
            j=xxx(m)
            m=abs(next_2d(i,j))
	      if(m.eq.0)then
	        print*,'Did you leave blank columns & rows around the'
              print*,'edges of the data? Maybe not'
	        print*,'river profile data not printed'
	        print*
	        stop 'Program aborted in bsn @ 2861'
	      endif
          end do
          if(tempsum.gt.maxlength)then
            longest=n
            maxlength=tempsum
          endif          
        end do
!       mark the channel until a previously marked channel is found:
        n=longest
        do while(channel(n).eq.0)
          channel(n)=k
          i=yyy(n)
          j=xxx(n)
          n=abs(next_2d(i,j))  ! still done on the next, not newnext????? 21/11/02
        end do

        open(unit=98,file=filenames(k),status='unknown',iostat=ios)
        open(unit=99,file=filenames(k+10),status='unknown',iostat=ios)
        if(ios.ne.0)then
          stop 'Program aborted @ 992, can`t open distance.dat file'
        endif
        do n=1,na    !new
          if(rank_2d(i,j).gt.0)then
          if(channel(n).eq.k)then
            i=yyy(n)
            j=xxx(n)
            write(98,*)n,distance(n),elv_2d(i,j),channel(n)
            write(99,*)j,i,elv_2d(i,j)
!            print*,n,distance(n),elv_2d(i,j),channel(n)
            lastn=n
          endif
          endif    !new
        end do

!I DON'T KNOW WHAT THE NEXT FEW LINES DO  NK nOV. 21/02
!        n=abs(newnext(i,j))   !changed
!        i=yyy(n)
!        j=xxx(n)
!        if(rank_2d(i,j).gt.0)then   !new
!        write(98,*)n,distance(n),elv_2d(i,j),channel(n)
!        write(99,*)j,i,elv_2d(i,j)
! 
!       print*,n,distance(n),elv_2d(i,j),channel(n)
!        endif      !new



        close(unit=98)
        close(unit=99)
        print*,'finished writing ',filenames(k)
        print*,'finished writing ',filenames(k+10)

      end do        

      endif     ! na-naa.le.1


!     pause 16
      print*
	print*,'No. of errors found in the map file = ',no_errors
	print*,'No. of errors found in the map file = ',no_errors
	print*,'No. of errors found in the map file = ',no_errors
	print*
      if(no_errors.ne.0)then
       print*,' ********* please check the bsn_info.txt file **********'
       print*,' ********* please check the bsn_info.txt file **********'
       print*,' ********* please check the bsn_info.txt file **********'
       print*
      else
        print*,'new_shd.r2c has been written'
	  print*,'Please rename new_shd.r2c or replace the bsnm_shd.r2c '
	  print*
      endif
      STOP ' Normal ending'

! TS - ADDED DEALLOCATION STATEMENT FOR AREA17 ARRAYS
c      deallocate(elv_2d,da,slope,frac,aclass,xxx,yyy,rank,s,next,ielv,iak,
c     *irough,ichnl,bnkfll,ireach,dummy,stat=iDeallocateStatus)
      if (iDeallocateStatus.ne.0) STOP   
     *    '**Deallocation for AREA17A arrays in bsna failed**' 

! FORMATS

 1001 format(999f4.0)
 1002 format(10f8.4)
 1003 format(999i2)
 1004 format(999f6.1)
 1005 format(12i5,2f5.1)
 1006 format(999i4)
 1007 format(12i5,2f5.1)
 1008 format(20f5.3/)
 1009 format(2i5,4f10.5)
 1010 format(3i5,f10.0,f10.2,f10.7,f7.1,5i5,17f5.2)
 1011 format(3i5,f10.3,f10.5,f10.7,f7.1,5i5,17f5.2)
 1012 format(' ','istep is set to 1 - local coordinates are used')
 1013 format('  al cintvl impr ntype elvconv')
 1014	format('   Basin # not coded @ grid #',i6,' @ ',2i6,' elv=',f7.3)
 1015	format('# contours not coded @ grid #',i6,' @ ',2i6,' elv=',f7.3)
 1016	format('# channels not coded @ grid #',i6,' @ ',2i6,' elv=',f7.3)
 1017	format('       next grid = 0 @ grid #',i6,' @ ',2i6,' elv=',f7.3)
 1018	format('Possible cause: wrong drainage direction')
 1019	format('Errors OK if last receiving grid !!!!!!!!!!!!!'/)
 1020 format(f10.0,  f5.0,2i5,2f10.0)
 1021 format(f10.0,  f5.1,2i5,f5.3)
 1022 format(f10.0,  f5.1,2i5,2f10.3)
 1024 format(4i5,f10.3)
 1025 format(4i5,f10.0,f10.2,f10.7,f7.1,f7.0,i5,f10.5,2i5,3x,17f5.2)
 1026 format(4i5,f10.3,f10.5,f10.7,f7.1,f7.0,i5,f10.5,2i5,3x,17f5.2)
 1027 format(4i5,f10.0,f10.2,f10.7,f7.1,f7.0,i5,11e10.3,
     *               f10.5,2i5,3x,17f5.2)
 1028 format(4i5,f10.3,f10.5,f10.7,f7.1,f7.0,i5,11e10.3,
     *               f10.5,2i5,3x,17f5.2)
 1050 format(999f4.2)
 1051 format(999f5.2)
 1052 format(999f4.0)
 1053 format(999f6.0)
 1054 format(2i4,999f5.0)
 1055 format(2i4,999f5.2)
 1056 format(999f6.2)
 1057 format(2i4,999i5)
 1058 format(999i5)
 1101 format(/'   ls   ks   js   ih  local')
 1102 format(/'   l  istep cintv local')
 1103 format(/' iymin iymax jxmin jxmax')
 1104 format(/' converted to local coordinates')
 1105 format( ' imin, imax,iymin,iymax, yint')
 1106 format(/' jmin, jmax,jxmin,jxmax, xint')
 1107 format(/' elevations of the channel bottoms')
 1108 format( ' half way along the square')
 1109 format(/' fraction of the elemement within the basin')
 1110 format(/' drainage directions')
 1111 format( ' 1=ne,2=e,3=se,4=s,5=sw,6=w,7=nw,8=n')
 1112 format(/' ib & it - the bottom and top rows for computations')
 1113 format( '           in local coordinates')
 1116 format(/' na - the total number of elements within the basin')
 1117 format( '      including the element accepting the last flow')
 1118 format(/' pairs of local coordinates for each element in order')
 1119 format( ' of descending elevations - ie according to rank')
 1120 format(/' rank - the order in which the calculations are carried')
 1121 format( '        out, from highest to lowest element')
 1122 format(/' drainage area above the outlet of each element')
 1123 format(/' slope of the river from this to the next square')
 1124 format(/' permeability & land use class')
 1125 format(/' number of contours in the element')
 1126 format(/' number of channels traversing the element')
 1127 format(/' % impervious area in each element')
 1128 format(/' the next element - ie the element recieving the flow') 
 1129	format(/' frac_2d(',2i5,')=',f10.3,' - please check')

 3000 format(a10,i5)
 3001 format(a20,i12)
 3002 format(2a20)
 3003 format(a20,f15.6)
 3004 format(a20,a10,2x,a10)
 3005 format(a40)
 3006 format(a3,a10)
 3007 format(a14,i5,a6,i5)
 3008 format(a30)
 3009 format(a14,i3,a6,i3)
 3012 format(a9)
 3020 format(a20,a40)

 4001 format(999(' ',f5.0))
 4002 format(999(' ',f10.3))
 4003 format(999(' ',i2))
 4004 format(999(' ',f10.5))
 4005 format(999(' ',f10.7))
 4006 format(999(' ',f8.1))
 4007 format(999(' ',f8.0))
 4008 format(999(' ',f8.3))
 4009 format(999(' ',e10.3))
 4010 format(999(' ',f5.3))
 4011 format(999(' ',f5.1))
 4012 format(999(' ',e12.7))

 4050 format(999(i4))
 4052 format(999(' ',i3))

 5000 format(a80)
 5001 format(a2)
 5002 format(a12,i3,a65)
 5003 format(a20,f12.0)
 5004 format(a20,a10)
 5005 format(a1,a80)
 5006 format(a20,i12)
5007  format(a1,a79)
5009  format(':CoordSys           ',a10)
5010  format(':Datum              ',a10)
5011  format(':Zone               ',a10)
5012  format('#',a80)
5013  format(':xOrigin            ',f12.7)
5014  format(':yOrigin            ',f12.7)
50131 format(':xOrigin            ',f12.3)
50141 format(':yOrigin            ',f12.3)
5015  format(':xCount             ',i12)
5016  format(':yCount             ',i12)
5017  format(':xDelta             ',f12.7)
5018  format(':yDelta             ',f12.7)
50171 format(':xDelta             ',f12.3)
50181 format(':yDelta             ',f12.3)
50182 format(':NominalGridSize_AL ',f12.3)
5019  format(':ContourInterval    ',f12.3)
5020  format(':ImperviousArea     ',f12.3)
5021  format(':ClassCount         ',i12)
5022  format(':ElevConversion     ',f12.3)
5023  format('#          ')
5024  format('#            ',2a30)
5025  format(':InputFileName      ',a30)
5026  format(':Created     :      ',
     *        2(a2,':'),a2,2x,a2,'-',a2,'-',a4)
5027  format(':NumRiverClasses    ',i12)
5028  format(':TotalNumOfGrids    ',i12)
5029  format(':numGridsInBasin    ',i12)
5030  format(':DebugGridNo        ',i12)
5032  format(':endHeader          ')
5033  format('#                   ')

 6000 format(5x,'assembled basin data file')
 6001	format(' fatal error - outlet elements have not been')
 6002 format(' properly designated')
 6003 format(' set frac of the recieving element = 0'//)
6004  format(' location of outlet:')
6005  format(' grid number  ',i5)
6006  format(' row number   ',i5,f10.3)
6007  format(' column number',i5,f10.3)
 6011 format(' area ratios do not add to 1.00 @ i=',i2,' j=',i2)
 6012 format(' bankfull discharges:')
 6013 format(' zero bankfull discharge encountered')
 6014 format(' probable cause: blank lines at the end of the map file')
 6015	format(' receiving grid higher in grid',3i5,' elv=',f10.3)
 7001 format(12i5)
 7005 format(4i5)
 7006 format(2i5,'             # rows & columns resp.')
 7021 format(f10.0,f5.1,2i5,'     map file used =  ',a30)
 8002 format('      data chksum/')
 8003 format('     * ',10(i6,'#'))
99000 format('  3.0 Version Number')   ! added a class
99001 format(i5,' AttributeCount')
99002 format('    1 ReportingTimeStep Hours')
99902 format('    0 Start Reporting Time for ENSIM (hr)')
99903 format(' 8784 End Reporting Time for ENSIM (hr)')
99003 format('0',i4,' Temperature')
99004 format('1',i4,' Precipitation')
99104 format('1',i4,' Cumulative Precipitation ')
99005 format('1',i4,' Lower Zone Storage Class ')  
99006 format('0',i4,' Ground Water Discharge m^3/s ',i2)     
99007 format('1',i4,' Grid Runoff')
99008 format('1',i4,' Grid Outflow')
99009 format('1',i4,' Weighted SWE')
99010 format('0',i4,' Wetland Depth')
99011 format('0',i4,' Channel Depth')
99012 format('0',i4,' Wetland Storage in m^3')
99013 format('0',i4,' Wetland Outflow in m^3/s')
99014 format('0',i4,' Depression Storage Class ',i2)   
99015 format('0',i4,' Depression Storage (Snow) Class ',i2)
99016 format('0',i4,' Snow Water Equivalent Class ',i2)
99017 format('0',i4,' Snow Covered Area Class ',i2)
99018 format('0',i4,' Upper Zone Storage Class ',i2)
99019 format('0',i4,' Upper Zone Storage (Snow) Class ',i2)
99020 format('0',i4,' Surface Flow m^3/s Class ',i2)
99021 format('0',i4,' Surface Flow (snow) m^3/s Class ',i2)
99022 format('0',i4,' Interflow m^3/s Class ',i2)
99023 format('0',i4,' Interflow (snow) m^3/s Class',i2)
99024 format('0',i4,' Recharge mm Class ',i2)
99025 format('0',i4,' Recharge mm (snow) Class ',i2)
99026 format('0',i4,' PET (average) mm Class ',i2)
99027 format('0',i4,' ET (cummulative) mm Class ',i2)
99028 format('0',i4,' Sublimation Cummulative) mm (snow) Class ',i2)
      
      END PROGRAM bsn
