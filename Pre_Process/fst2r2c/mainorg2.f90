      program main

!***********************************************************************
!*     This program reads the RPN standard file format, interpolates 
!*     on a lat/lon grid and writes into the R2C file format used by 
!*     GreenKenue and the MESH hydrological model.
!***********************************************************************

!***********************************************************************
!*     A "makefile" is available to compile and run this program. To 
!*     compile the program, first clean the existing object and 
!*     executable files using the "make clean" command, then build the 
!*     fst2r2c executable using the "make" command. To run the program, 
!*     redirect the input FST file and the output R2C file to the 
!*     fst2r2c executable as "fst2r2c < input.fst". For now I am assuming
!*     24 FST files containing hourly data and saved as 20100727_000 ...
!*     20100727_023. So you need to provide the file name before the 
!*     underscore.
!***********************************************************************

!     MESH forcing data attributes
      use variable_attributes_module

!     MESH grid parameters
      use grid_parameters_module

      implicit none

!     External functions
      integer, external :: iargc
      integer, external :: fnom, fstouv, fclos, fstfrm, fstinf, &
                           fstluk, fstprm, fstecr, newdate
      integer, external :: ezqkdef, ezdefset, ezsint, gdll

!     Declare variables used by the RPN standard file library
      character*2 nomvar
      character*1 typvar, grtyp, grref
      character*8 etiket
  
!     The ikeys are identifiers for the IPs and IGs        
      integer key(nvr2c + 2), keylat, keylon, gdfst 
      integer dateo, deet, npas, nifst, njfst, nkfst, npak, datyp 
      integer ip1, ip2, ip3
      integer ig1, ig2, ig3, ig4
      integer ex1, ex2, ex3, deet, npas, nbits, datyp, dateo
      integer deltat, datev
      integer swa, lng, dltf, ubc
      integer ip1z,ip2z,ip3z
      integer yyyymmdd,hhmmssss
      integer npts,nnpas,idt,ipos
      logical rewrit

!     xlat0 and xlon0 are the location of SW corner of the MESH grid
!     dlat and dlon are the grid spacing
!     ni and nj are number of grids in the longitudinal and in the 
!     latitudinal directions respectively.
      integer nir2c, njr2c, nkr2c, gdr2c
      real    xlat0, xlon0, dlat, dlon

!     Declare other variables used by the program
      integer      ier, nrecs, nfiles, FileCount
      integer      i,j, ii, jj, nf, nv, nk, nh, iun, iungrd, ni, nj, nk
      character*100 fstroot,fstname,cnf1,cnf2

      character*10 ci
!     Temporary storage for non-important readings
      integer jk
      real    cjk

!     Forcing data dimension in the FST file
      real, dimension(:,:), allocatable ::  fldfst

!     Forcing data dimension in the FST file
      real, dimension(:), allocatable ::      latr2c, lonr2c, work
      real, dimension(:), allocatable ::      latfst, lonfst
      real, dimension(:,:), allocatable ::    zlat, zlon, PREGRD

      real swlatref, swlonref, dlonref, dlatref

!     get command line arguments
      if(iargc() .ne. 3)then
         print*,'Error: three arguments (number of files, file counts',&
                ' and file name) are needed.'
         call exit(1)
      else
         call getarg(1, cnf1)
         print*,cnf1
         read(cnf1,*) nfiles
         call getarg(2, cnf2)
         print*,cnf2
         read(cnf2,*) FileCount
         call getarg(3, fstname)
      endif
      print*,nfiles
      write(*,'(A,A)')'Input FST file name:  ', adjustl(trim(fstname))

!     MESH grid
      call read_grid_parameters()
      xlat0 = yOrigin
      xlon0 = xOrigin
      if(xOrigin < 0.0)xlon0 = xOrigin + 360.0
      dlat  = yDelta
      dlon  = xDelta
      nir2c = yCount
      njr2c = xCount
      nkr2c = 1

      allocate (latr2c(nir2c))
      allocate (lonr2c(njr2c))
      allocate (work(njr2c))
      allocate (zlat(njr2c,nir2c))
      allocate (zlon(njr2c,nir2c))
      allocate (PREGRD(njr2c,nir2c))

!     Initialisation of the axes values 
      do i=1,njr2c
        lonr2c(i) = xlon0 + dlon * (i-1)
      enddo
  
      do j=1,nir2c
         latr2c(j) = xlat0 + dlat * (j-1)
      enddo

!-    computation of ig parameters for defining a z-grid in a latlon space
!     The values are such that the lat-lon can be encoded "as is" in the navigational records
!     without supplemental conversion

      swlatref = xlat0 !0.0
      swlonref = xlon0 !0.0
      dlonref = dlon !1.0
      dlatref = dlat !1.0

      call cxgaig('L', ig1, ig2, ig3, ig4, swlatref, swlonref, & 
                  dlatref, dlonref)

      write(*,*)(lonr2c(i),i=1,njr2c)
      write(*,*)(latr2c(i),i=1,nir2c)

!     Write header on the forcing files
      nf = 3 !Precipitation
      if(FileCount==1)then
         open(nf,file=trim(FileName(nf)))
         call write_header(nf)
      endif
 

!     Computation of ig parameters
      if (Projection .eq. "LATLONG")then

!     At this time the 'L' grid type is selected - this needs 
!     further discussion  and refinement
!         call cxgaig('L',ig1,ig2,ig3,ig4,xlat0,xlon0,dlat,dlon)
         print*,'-------------------------------------------------'
         print *,'R2C grid parameters:'
         print*,'xlat0 = ',xlat0
         print*,'xlon0 = ',xlon0
         print*,'dlat = ',dlat
         print*,'dlon = ',dlon
         print*,'ig1 = ',ig1
         print*,'ig2 = ',ig2
         print*,'ig3 = ',ig3
         print*,'ig4 = ',ig4
         print*,'-------------------------------------------------'
      else
         print*,'---------------------------------------------------'
         print*,'Currently the LATLONG projection of MESH grid is '
         print*,'functional. The MERCATOR projection will soon be '
         print*,'included - contact muluneh.mekonnen@ec.gc.ca' 
         print*,'---------------------------------------------------'
         stop
      endif

!     Initialization of the necessary standard file parameters required to write a record

      npak = -16
      deet = 0
      npas = 0
      ip1z  = 2000
      ip2z  = 2001
      ip3z  = 2002
      dateo = 0
      typvar = 'P'
      grref  = 'L'
      etiket = 'ZLATLON'
      datyp  = 1
      rewrit = .true.

!     definition of dateo

      yyyymmdd = 20001102
      hhmmssss = 00000000
      ier = newdate(dateo,yyyymmdd,hhmmssss,3)

      if(ier .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: dateo not defined'
         print*,'-------------------------------------------------'
         stop
      endif

!     Write the navigational records into the standard file
      iungrd = 1
      ier = fnom(iungrd, 'grd.fst','STD+RND',0)
      if(ier .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: Cannot open file,  grd.fst'
         print*,'-------------------------------------------------'
         stop
      endif
      
      nrecs = fstouv(iungrd, 'STD+RND')
      if(nrecs .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: Cannot open file, grd.fst, in random mode'
         print*,'-------------------------------------------------'
         stop
      endif
      
      nomvar = '>>'
      ier = fstecr(lonr2c, lonr2c, npak, iungrd, dateo, deet, npas, &
                   njr2c,1, 1, ip1z, ip2z, ip3z, typvar, nomvar,    &
                   etiket, grref, ig1, ig2, ig3, ig4, datyp, .false.)
  
      if(ier .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: longitude construction not OK'
         print*,'-------------------------------------------------'
         stop
      endif

      nomvar = '^^'
      ier=fstecr(latr2c, latr2c, npak, iungrd, dateo, deet, npas, 1, &
                 nir2c, 1, ip1z, ip2z, ip3z, typvar, nomvar, etiket,  &
                 grref, ig1, ig2, ig3, ig4, datyp, rewrit)

      if(ier .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: latitude construction not OK'
         print*,'-------------------------------------------------'
         stop
      endif

!     define the grid

      grtyp   = 'Z'
      gdr2c    = ezqkdef(njr2c,nir2c,grtyp, ip1z, ip2z, ip3z, 0, iungrd)

!      get the latlon of the grid we just defined

      ier     = gdll(gdr2c, zlat, zlon)
      if(ier .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: grid construction not OK'
         print*,'-------------------------------------------------'
         stop
      endif

      write(*,*)((zlat(i,j),i=1,njr2c),j=1,nir2c)
      write(*,*)((zlon(i,j),i=1,njr2c),j=1,nir2c)

!     Close the FST file
      ier = fstfrm(iungrd)

!     Unlink the file unit number from the FST file
      ier = fclos(iungrd)

!     Define output grid
!      gdr2c = ezqkdef(nir2c,njr2c,'L',ig1,ig2,ig3,ig4,lonr2c,latr2c)
!      ier   = gdll(gdr2c, latr2c, lonr2c)

      write(*,*)(latr2c(i),i=1,njr2c)
!     Open FST data file
      ier   = fnom(iun, trim(fstname), 'STD+RND+R/O', 0)
      if(ier .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: Cannot open file ', trim(fstname)
         print*,'-------------------------------------------------'
         stop
      endif
      nrecs = fstouv(iun, trim(fstname), 'RND')
      if(nrecs .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: Cannot open file ', trim(fstname),         &
                 ' in random mode'
         print*,'-------------------------------------------------'
         stop
      endif

!     Get grid dimensions and specifications from FST file
      keylon = fstinf(iun,nifst,jk,nkfst,-1,' ',-1,-1,-1,' ','>>')
      allocate (lonfst(nifst))
      ier = fstluk(lonfst,keylon,ni,nj,nk)
      write(*,*)(lonfst(i),i=1,nifst)

      keylat = fstinf(iun,jk,njfst,nkfst,-1,' ',-1,-1,-1,' ','^^')
      allocate (latfst(njfst))
      ier = fstluk(latfst,keylat,ni,nj,nk)
      write(*,*)(latfst(i),i=1,njfst)

      ier    = fstprm(keylat,dateo,deet,npas,jk,njfst,nkfst,           &
                      nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket,    &
                      grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,      &
                      ex2,ex3)

!     Allocate the two dimensional data
      allocate(fldfst(nifst,njfst))

!     Define FST grid
      gdfst = ezqkdef(nifst,njfst,grtyp,ig1,ig2,ig3,ig4,iun)

!     Define output grid
      ier   = ezdefset(gdr2c,gdfst)

!     Check records of the FST file
      nv = 3
      key(nv) = fstinf(iun,ni,nj,nk,-1,' ',-1,-1,-1,'A',varNm(nv))

!     Precipitation
      nv = 3
      nf = 3
      if(key(nv) .gt. 0)then
         ier = fstluk(fldfst,key(nv),ni,nj,nk)

!        Interpolate on MESH grid
         ier = ezsint(PREGRD,fldfst)
         ier = fstprm(key(nv),dateo,deet,npas,ni,nj,nk,                &
                      nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket,    &
                      grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,      &
                      ex2,ex3)

!        Compute date of validation
         deltat = (deet*npas+1800)/3600
         call incdat(datev,dateo,deltat)

!        Write to R2C file
         write(nf,900)deltat,deltat,datev
!         do i = 1, njr2c
!            write(nf,'(100(F16.7,2X))')(PREGRD(i,j),j=1,nir2c)
!         enddo
         do i = 1, nj
            write(nf,'(100(F16.7,2X))')(PREGRD(i,j),j=1,ni)
         enddo
         write(nf,908)
      else
         write(nf,*)'file ', nh
         write(nf,*)'no record is found for precipitation'
         write(nf,909)
      endif

!     Close the FST file
      ier = fstfrm(iun)

!     Unlink the file unit number from the FST file
      ier = fclos(iun)

!    Close the r2c file
      if(FileCount==nfiles)close(nf)
  
900   format(':Frame        ',                                         &
              I2,8X,I2,1X,'"',I10,'"')
908   format(':EndFrame')

909   format('Use the voir command to open the FST file',              &
             'table of contents and check the variable',               &
             'name (NOMVAR) convention used in the',                   &
             'variable_attributes_module.f90 module.')
      end  
