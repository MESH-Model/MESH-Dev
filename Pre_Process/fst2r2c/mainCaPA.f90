      program mainCaPA

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
      use variable_attributes_module

!     Variables for R2C grid parameters 
      use grid_parameters_module

      implicit none

!     Specify time step 
      integer, parameter :: delt = 1.0

!     Interpolation scheme
!      character*7, parameter :: interoption = 'NEAREST'
      character*7, parameter :: interoption = 'LINEAIR'

!     External functions
      integer, external :: iargc

!     External Functions from ARMNLIB to deal with FST files
      integer, external :: fnom,fstouv,fclos,fstfrm
      integer, external :: fstinf,fstluk,fstprm,fstecr
      integer, external :: ezqkdef,ezdefset,gdll,gdllsval,ezsetopt,ezsint

!     Declare variables used by the RPN standard file library
      character*2 nomvar
      character*1 typvar, grtyp, grref
      character*8 etiket
  
      integer gdfst, nifst, njfst, nkfst 
      integer ip1, ip2, ip3
      integer ig1, ig2, ig3, ig4
      integer ex1, ex2, ex3, deet,npas,npak,nbits,datyp,dateo,datev
      integer swa, lng, dltf, ubc
      integer ip1z,ip2z,ip3z
      integer npts,nnpas,idt,ipos

      integer newdate,nrecs
      external newdate,incdatr
      INTEGER yyyymmdd,hhmmssHH ! date et heure d'emission lue au fichier
      CHARACTER*10 strymd ! date d'emission convertie en chaine
      CHARACTER*12 strhms ! heure d'emission convertie en chaine

!     xlat0 and xlon0 are the location of SW corner of the grid
!     dlat and dlon are the grid spacing
!     ni and nj are number of grids in the longitudinal and in the 
!     latitudinal directions respectively.
      integer nir2c, njr2c, nkr2c, gdr2c,keypr

!     Declare other variables used by the program
      integer      ier, nfiles, FileCount
      integer      i,j,k,ii,jj,nv,nh,iunfst,iunr2c
      integer      iunstationcsv,iunstationtxt
      integer      ni, nj, nk, ns
      character*100 fstname,arg
      logical       I_EXIST

!     Forcing data dimension in the FST file
      real, dimension(:,:), allocatable ::latr2c,lonr2c,latfst,lonfst
      real, dimension(:,:), allocatable ::PREGRD,fldfst
      real, dimension(:), allocatable   ::lat,lon,val
      
!     get command line arguments
      if(iargc() .ne. 3)then
         print*,'Error: five arguments (file counts, number of files,',&
               'file name, variable name and variable type) are needed.'
         call exit(1)
      else
         call getarg(1, arg)
         read(arg,*) FileCount
         call getarg(2, arg)
         read(arg,*) nfiles
         call getarg(3, fstname)
      endif
      
      write(*,'(A,A)')'Input FST file name:  ', adjustl(trim(fstname))

!     R2C grid parameters - read from MESH_drainage_database.r2c file
      call read_grid_parameters()
      nkr2c = 1
      allocate (latr2c(xCount,yCount))
      allocate (lonr2c(xCount,yCount))
      allocate (PREGRD(xCount,yCount))

!     R2C file !Precipitation id = 3 
      iunr2c = 3
      iunstationcsv = 11
      iunstationtxt = 12

!     Observation stations
!      open(13,file='station_location.txt',status='old')
!      read(13,*)ns
!      allocate(lat(ns),lon(ns),val(ns))
!      do i = 1, ns
!         read(13,*)lat(i),lon(i)
!      enddo
!      close(13)

!     Write header on the forcing files
      inquire(file=trim(FileName(iunr2c)), EXIST=I_EXIST)
      if(I_EXIST)then
          open(iunr2c,file=trim(FileName(iunr2c)),position='append',&
               status='old')
      else
          open(iunr2c,file=trim(FileName(iunr2c)),status='unknown')
          call write_header(iunr2c)
      endif 

!     Open FST data file
      iunfst = 20
      ier   = fnom(iunfst, trim(fstname), 'STD+RND+R/O', 0)
      if(ier .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: Cannot open file ', trim(fstname)
         print*,'-------------------------------------------------'
         stop
      endif
      nrecs = fstouv(iunfst, trim(fstname), 'RND')
      if(nrecs .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: Cannot open file ', trim(fstname),         &
                 ' in random mode'
         print*,'-------------------------------------------------'
         stop
      endif
      
!     Index for precipiation records, analysis type
      keypr = fstinf(iunfst,ni,nj,nk,-1,' ',-1,-1,-1,'','PR0')
      if(keypr .gt. 0)then
         ier    = fstprm(keypr,dateo,deet,npas,nifst,njfst,nkfst,      &
                         nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket, &
                         grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,   &
                         ex2,ex3)

         CALL incdatr(datev,dateo,1)

          ier=newdate(datev,yyyymmdd,hhmmssHH,-3)
          WRITE(strymd,'(I8)'),yyyymmdd
          strymd=strymd(1:4)//'/'//strymd(5:6)//'/'//strymd(7:8)
          WRITE(strhms,'(I8)'),hhmmssHH
          DO i=1,8
            IF(strhms(i:i).EQ.' ')THEN
              strhms(i:i)='0'
            ENDIF
          ENDDO
          strhms=strhms(1:2)//':'//strhms(3:4)//':'//strhms(5:6)//'.000'


!        Allocate the two dimensional data
         allocate(fldfst(nifst,njfst))
         allocate(latfst(nifst,njfst))
         allocate(lonfst(nifst,njfst))

         ier = fstluk(fldfst,keypr,ni,nj,nk)

!         call statfld('PR','F',0,0,0,0,'',fldfst,nifst,njfst,1)
!        Define FST grid
         gdfst = ezqkdef(nifst,njfst,grtyp,ig1,ig2,ig3,ig4,iunfst)

!         ier   = gdll(gdfst, latfst, lonfst)
!         print*,latfst(1,1),lonfst(1,1) 
!         call statfld('LA','F',0,0,0,0,'',latfst,nifst,njfst,1) 
!         call statfld('LO','F',0,0,0,0,'',lonfst,nifst,njfst,1)
         
!     Computation of ig parameters
          if (Projection .eq. "LATLONG")then

!     At this time the 'L' grid type is selected to represent the R@C domain
               call cxgaig('L',ig1,ig2,ig3,ig4,yOrigin,xOrigin,yDelta,xDelta)
!               if(FileCount==1)then
!                  print*,'R2C grid parameters'
!                  print*,'xlat0 = ',yOrigin
!                  print*,'xlon0 = ',xOrigin
!                  print*,'dlat = ',yDelta
!                  print*,'dlon = ',xDelta
!                  print*,'yCount = ',yCount
!                  print*,'xCount = ',xCount
!                  print*,'ig1 = ',ig1
!                  print*,'ig2 = ',ig2
!                  print*,'ig3 = ',ig3
!                  print*,'ig4 = ',ig4
!                  print*,'---------------------------------------------'
!               endif

!              Define the R2C grid to be used for interpolation
               gdr2c = ezqkdef(xCount, yCount, 'L', ig1,ig2,ig3,ig4,0)

!              ier   = gdll(gdr2c, latr2c, lonr2c)
!              print*,latr2c(1,1),lonr2c(1,1)
!              Print grid statistics
!              print*,'R2C grid statistics'
!              call statfld('LA','R',0,0,0,0,'',latr2c,xCount,yCount,1)
!              call statfld('LO','R',0,0,0,0,'',lonr2c,xCount,yCount,1)

            else
               print*,'------------------------------------------------'
               print*,'Currently the LATLONG projection of MESH grid is'
               print*,'functional. The MERCATOR projection will soon be'
               print*,'included - contact muluneh.mekonnen@ec.gc.ca'
               print*,'------------------------------------------------'
               stop
            endif
            ier = ezdefset(gdr2c, gdfst)
            ier = ezsetopt('INTERP_DEGREE', trim(interoption))

!           Interpolation
            ier = ezsint(PREGRD, fldfst)
            PREGRD = PREGRD * 1000.0/(delt*3600.0) !Change into mm/s

!           call statfld('PR','R',0,0,0,0,'',PREGRD,xCount,yCount,1)

!           Write to R2C file
            nrecs = mod(FileCount-1,24)
            if(nrecs == 0 .and. FileCount > 1)nrecs=24
            write(iunr2c,'(A,I8,I8,A,A,A,A,A)'), &
               ':Frame ',nrecs,nrecs,' "',strymd,' ',strhms,'"'

            do j = 1, yCount
               write(iunr2c,'(100(F12.7,1X))')(PREGRD(i,j),i=1,xCount)
            enddo
            write(iunr2c,908)
      else
         write(iunr2c,*)'file ', FileCount
         write(iunr2c,*)'no record is found for precipitation'
         write(iunr2c,911)
      endif

!     Close the FST file
      ier = fstfrm(iunfst)

!     Unlink the file unit number from the FST file
      ier = fclos(iunfst)

!    Close the r2c file
      close(iunr2c)
 
900   format(':Frame        ',                                         &
              I2,8X,I2,1X,'"',I10,'"')
908   format(':EndFrame')
909   format(100(f12.7,','))
910   format(100(f12.7,1X))
911   format('Use the voir command to open the FST file',              &
             'table of contents and check the variable',               &
             'name (NOMVAR) convention used in the',                   &
             'variable_attributes_module.f90 module.')
      end  
