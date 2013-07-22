      program main

!***********************************************************************
!     This program:
!        -reads FST file format, 
!        -interpolates on a lat/lon grid and 
!        -writes into R2C file format.
!
!     Written by Muluneh Admass Mekonnen, September 15, 2010
!***********************************************************************

!***********************************************************************
!     A "makefile" is available to compile the main program and the
!     associated subroutines. To compile the program, first clean the 
!     existing objects and the executable using the "make clean" command.
!     Then build the fst2r2c executable using the "make" command. 
!***********************************************************************

!     MESH forcing data attributes
      use gem_variable_attributes_module

!     Variables for R2C grid parameters 
      
      implicit none

!     Unit conversion for Temperature
      real, parameter ::    t0 = 273.16

!     Declare variables - related to FST file
      integer      ip1,ip2,ip3
      integer      ig1_src,ig2_src,ig3_src,ig4_src 
      integer      ig1_des,ig2_des,ig3_des,ig4_des
      integer      deet,npas,dateo,datev
      character*1  grtyp

!     Declare other variables used by the program
      integer       ier,key,keylat,keylon,nrecs,nfiles,FileCount
      integer       pas,istart,iend
      integer       i,j,k,iunfst,iunr2c,gdfst,gdr2c
      integer       ni,nj,nk
      character*100 fstname,arg

!     Forcing data variable
      real, dimension(:,:), allocatable :: fldfst
      real fldr2c,uu,vv,lat(1),lon(1)

!     Temporary variables
      integer jk
      character*8 cjk

!     External functions
      integer, external :: iargc
      integer, external :: fnom,fstouv,fclos,fstfrm
      integer, external :: fstinf,fstluk,fstprm
      integer, external :: ezqkdef,ezdefset,ezsetopt,gdllsval

!     External subroutine
      external :: incdatr

!     get command line arguments
      if(iargc() .ne. 3)then
         print*,'Error: three arguments (file count, number of files ,',&
                'and file name) are needed.'
         call exit(1)
      else
         call getarg(1, arg)
         read(arg,*) FileCount
         call getarg(2, arg)
         read(arg,*) nfiles
         call getarg(3, fstname)
      endif
      
!     Open FST data file in random mode
      iunfst = 11
      ier    = fnom(iunfst, trim(fstname), 'STD+RND+R/O', 0)
      if(ier .lt. 0)then
         print*
         print *,'ERROR: Cannot open file ', trim(fstname)
         stop
      endif
      ier = fstouv(iunfst, trim(fstname), 'RND')
      if(ier .lt. 0)then
       print*
       print *,'ERROR: Cannot open file ',trim(fstname),'in random mode'
       stop
      endif
      
!     FST grid information
      keylon  = fstinf(iunfst,ni,jk,jk,-1,' ',-1,-1,-1,'','>>')
      keylat  = fstinf(iunfst,jk,nj,jk,-1,' ',-1,-1,-1,'','^^')

!     Allocate the two dimensional data
      allocate(fldfst(ni,nj), stat=pas)
      if (pas .ne. 0) then
         print*
         print*,'Error in allocating FST two dimensional array.'
         print*,'File ', trim(fstname)
         stop
      endif

!***   Define latlon values
      lat(1)  =   53.46
      lon(1)  =  -111.26 + 360.0

      istart = 6
      iend   = 17
      if(FileCount == 1)istart = 7
!      if(FileCount == nfiles)iend = 18

      do ip2 = istart,iend  !GEM hourly records
      do iunr2c = 6,7
         key    = fstinf(iunfst,ni,nj,nk,-1,' ',-1,ip2,-1,'',varNm(iunr2c))
         if(key > 0) ier    = fstluk(fldfst,key,ni,nj,nk)
         if(ier > 0) then
!            ier    = fstluk(fldfst,key,ni,nj,nk)
         else
           print*
           print*,'no ',varname(iunr2c),' record found.'
           print*,'File ', trim(fstname)
           print*,'IP2 ', ip2
           stop
         endif       

!        Read date, grid type and grid parameters of FST data
         ier    = fstprm(key,dateo,deet,npas,ni,nj,nk, &
                  jk,jk,jk,jk,jk,cjk,cjk,cjk, &
                  grtyp,ig1_src,ig2_src,ig3_src,ig4_src, &
                  jk,jk,jk,jk,jk,jk,jk)
                  
!        Define FST grid
         gdfst = ezqkdef(ni,nj,grtyp,ig1_src,ig2_src,ig3_src,ig4_src, &
                         iunfst)

!        Set interpolation option
!         ier = ezsetopt('INTERP_DEGREE', 'NEAREST')

!       Interpolate
        ier = gdllsval(gdfst,fldr2c,fldfst,lat,lon,1)

!       Change the temperature unit to K        
        if(iunr2c == 3)fldr2c = fldr2c + t0

!       Compute the wind speed
        if(iunr2c == 6)then !x-component of wind speed
           uu = fldr2c
        endif
        if(iunr2c == 7)then !y-component of wind speed
           vv = fldr2c
           fldr2c = sqrt(uu*uu + vv*vv)
        endif

!       Limit the values to be non-negative            
        fldr2c = max(fldr2c,0.0)

!       Write to R2C file
        if(iunr2c .ne. 6)then
           call write_r2c(iunr2c,fldr2c,FileCount,ip2,dateo)
        endif   
        
      enddo
      enddo 

!     Close the FST file
      ier = fstfrm(iunfst)

!     Unlink the file unit number from the FST file
      ier = fclos(iunfst)
      
      end  

      
      subroutine write_r2c(iunr2c,fldr2c,FileCount,ip2,dateo)

!     MESH forcing data attributes
      use gem_variable_attributes_module

!     Variables for R2C grid parameters
      
      integer       ip2,dateo,FileCount
      real          fldr2c

      integer       nrecs
      character*10  strymd
      character*12  strhms
      
!     Write header on the forcing files
      if(ip2 == 7 .and. FileCount == 1)then
	 open(iunr2c,file=trim(FileName(iunr2c)),status='new')
      endif
      if(ip2 == 6 .and. FileCount > 1)then
         open(iunr2c,file=trim(FileName(iunr2c)),POSITION='APPEND')
      endif
      
      call date(dateo,FileCount,delt,strymd,strhms,nrecs,ip2)
      
      write(iunr2c,'(A,A,A,22F12.6)')strymd,' ',strhms,fldr2c

!      do j = 1, yCount
!         write(iunr2c,'(100(F12.6))')(fldr2c(i,j),i=1,xCount)
!      enddo
!      write(iunr2c,'(A)')':EndFrame'

!     Close r2c file when all records of the current FST file are written
      if(ip2 == 17)close(iunr2c)
            
      return
      end
      
      subroutine date(dateo,FileCount,delt,strymd,strhms,nrecs,ip2)
      integer       dateo,FileCount,nrecs,ip2
      character(*)  strymd
      character(*)  strhms
      
      integer              yyyymmdd,hhmmssHH
      integer, external :: newdate
      
      ier=newdate(dateo,yyyymmdd,hhmmssHH,-3)
      write(strymd,'(I8)'),yyyymmdd
      strymd=strymd(1:4)//'/'//strymd(5:6)//'/'//strymd(7:8)
      
      write(strhms,'(I8)'),hhmmssHH+int((ip2-6)*8*450*1e+6/3600.)
      do i=1,8
        if(strhms(i:i).EQ.' ')then
          strhms(i:i)='0'
        endif
      enddo
      strhms=strhms(1:2)//':'//strhms(3:4)//':'//strhms(5:6)//'.000'

      nrecs = int(hhmmssHH/1e6)+(ip2-6)!(FileCount-1)*delt
      if(nrecs == 0) nrecs = 24
      
      return
      end
