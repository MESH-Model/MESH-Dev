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
                           fstluk, fstprm
      integer, external :: ezqkdef, ezdefset, ezsint

!     Declare variables used by the RPN standard file library
      character*2 nomvar
      character*1 typvar, grtyp
      character*8 etiket
  
!     The ikeys are identifiers for the IPs and IGs        
      integer key(nvr2c + 2), keylat, keylon, gdfst 
      integer dateo, deet, npas, nifst, njfst, nkfst, npak, datyp 
      integer ip1, ip2, ip3
      integer ig1, ig2, ig3, ig4
      integer ex1, ex2, ex3, deet, npas, nbits, datyp, dateo
      integer deltat, datev
      integer swa, lng, dltf, ubc

!     xlat0 and xlon0 are the location of SW corner of the MESH grid
!     dlat and dlon are the grid spacing
!     ni and nj are number of grids in the longitudinal and in the 
!     latitudinal directions respectively.
      integer nir2c, njr2c, nkr2c, gdr2c
      real    xlat0, xlon0, dlat, dlon

!     Declare other variables used by the program
      integer      ier, nrecs, nfiles
      integer      i,j, ii, jj, nf, nv, nk, nh, iun, ni, nj, nk
      character*50 fstroot,fstname,cnf

      character*10 ci
!     Temporary storage for non-important readings
      integer jk
      real    cjk
      logical fldok(nvr2c)

!     Forcing data dimension in the FST file
      real, dimension(:,:), allocatable ::  fldfst1, fldfst2, fldfst

!     Forcing data dimension in the FST file
      real, dimension(:), allocatable ::      latr2c, lonr2c
      real, dimension(:,:), allocatable ::    FSDOWN, FDLGRD, PREGRD, &
                                              TAGRD,QAGRD,UVGRD,PRESGRD

!     get command line arguments
      if(iargc() .ne. 2)then
         print*,'Error: two arguments (number of files', & 
                ' and file root name) are needed.'
         call exit(1)
      else
         call getarg(1, cnf)
         read(cnf,*) nfiles
         call getarg(2, fstroot)
      endif
      print*,nfiles
      write(*,'(A,A)')'Input FST root name:  ', adjustl(trim(fstroot))

!     MESH grid
      call read_grid_parameters()
      xlat0 = yOrigin
      xlon0 = xOrigin
      dlat  = yDelta
      dlon  = xDelta
      nir2c = yCount
      njr2c = xCount
      nkr2c = 1

      allocate (latr2c(njr2c))
      allocate (lonr2c(nir2c))
      allocate (FSDOWN(nir2c,njr2c))
      allocate (FDLGRD(nir2c,njr2c))
      allocate (PREGRD(nir2c,njr2c))
      allocate (TAGRD(nir2c,njr2c))
      allocate (QAGRD(nir2c,njr2c))
      allocate (UVGRD(nir2c,njr2c))
      allocate (PRESGRD(nir2c,njr2c))

!     Write header on the forcing files
      do nf = 1, nvr2c
         open(nf,file=trim(FileName(nf)))
         call write_header(nf)
      enddo 

!     Computation of ig parameters
      if (Projection .eq. "LATLONG")then

!     At this time the 'L' grid type is selected - this needs 
!     further discussion  and refinement
         call cxgaig('L',ig1,ig2,ig3,ig4,xlat0,xlon0,dlat,dlon)
         print*,'-------------------------------------------------'
         print *,'R2C grid parameters:'
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

!     Define output grid
      gdr2c = ezqkdef(nir2c,njr2c,'L',ig1,ig2,ig3,ig4,lonr2c,latr2c)

      fldok = .true.
  
      do nh = 0, nfiles
!        Open FST data file
         write(ci,'(i3.3)')nh
         iun     = 100
         fstname = trim(fstroot) // '_' //adjustl(trim(ci))

         ier     = fnom(iun, trim(fstname), 'STD+RND+R/O', 0)
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

!        Get grid dimensions and specifications from FST file
         if(nh == 0)then
            keylon = fstinf(iun,nifst,jk,nkfst,-1,' ',-1,-1,-1,' ','>>')
            keylat = fstinf(iun,jk,njfst,nkfst,-1,' ',-1,-1,-1,' ','^^')
            ier    = fstprm(keylat,dateo,deet,npas,jk,njfst,nkfst,     &
                     nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket,     &
                     grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,       &
                     ex2,ex3)

!           Allocate the two dimensional data
            allocate(fldfst1(nifst,njfst))
            allocate(fldfst2(nifst,njfst))
            allocate(fldfst(nifst,njfst))

!           Define FST grid
            gdfst = ezqkdef(nifst,njfst,grtyp,ig1,ig2,ig3,ig4,iun)

!           Define output grid
            ier   = ezdefset(gdr2c,gdfst)
         endif

!        Check records of the FST file
         do nv = 1, nvr2c + 2
            key(nv) = fstinf(iun,ni,nj,nk,-1,' ',-1,-1,-1,' ',varNm(nv))
         enddo

!        Short wave radiation
         nv   = 1 
         nf   = 1
         if(key(nv) .gt. 0 .and. fldok(nf))then
            ier = fstluk(fldfst,key(nv),ni,nj,nk)

!           Interpolate on MESH grid
            ier     = ezsint(FSDOWN,fldfst)
            ier     = fstprm(key(nv),dateo,deet,npas,ni,nj,nk,      &
                      nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket, &
                      grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,   &
                      ex2,ex3)

!           Compute date of validation
            deltat = (deet*npas+1800)/3600
            call incdat(datev,dateo,deltat)

!           Write to R2C file
            write(nf,900)deltat,deltat,datev
            do i = 1, njr2c
               write(nf,'(100(F16.7,2X))')(FSDOWN(i,j),j=1,nir2c)
            enddo
            write(nf,908)
         else
            fldok(nf) = .false.
            write(nf,*)'file ', nh
            write(nf,*)'no record is found for short wave radiation'
            write(nf,909)
         endif

!        Long wave radiation
         nv = 2
         nf = 2
         if(key(nv) .gt. 0 .and. fldok(nf))then
            ier = fstluk(fldfst,key(nv),ni,nj,nk)

!           Interpolate on MESH grid
            ier     = ezsint(FDLGRD,fldfst)
            ier     = fstprm(key(nv),dateo,deet,npas,ni,nj,nk,      &
                      nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket, &
                      grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,   &
                      ex2,ex3)

!           Compute date of validation
            deltat = (deet*npas+1800)/3600
            call incdat(datev,dateo,deltat)
!           Write to R2C file
            write(nf,900)deltat,deltat,datev
            do i = 1, njr2c
               write(nf,'(100(F16.7,2X))')(FDLGRD(i,j),j=1,nir2c)
            enddo
            write(nf,908)
         else
            fldok(nf) = .false.
            write(nf,*)'file ', nh
            write(nf,*)'no record is found for long wave radiation'
         endif           

!        Precipitation
         nv = 3
         nf = 3
         if(key(nv) .gt. 0 .and. key(nv+1) .gt. 0 .and. fldok(nf))then
            ier = fstluk(fldfst1,key(nv),ni,nj,nk)
            ier = fstluk(fldfst2,key(nv+1),ni,nj,nk)

!           Interpolate on MESH grid
            fldfst  = fldfst1 + fldfst2
            ier     = ezsint(PREGRD,fldfst)
            ier     = fstprm(key(nv),dateo,deet,npas,ni,nj,nk,      &
                      nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket, &
                      grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,   &
                      ex2,ex3)

!           Compute date of validation
            deltat = (deet*npas+1800)/3600
            call incdat(datev,dateo,deltat)
!           Write to R2C file
            write(nf,900)deltat,deltat,datev
            do i = 1, njr2c
               write(nf,'(100(F16.7,2X))')(PREGRD(i,j),j=1,nir2c)
            enddo
            write(nf,908)
         else
            fldok(nf) = .false.
            write(nf,*)'file ', nh
            write(nf,*)'no record is found for precipitation'
            write(nf,909)
         endif

!        Air Temperature
         nv = 5
         nf = 4
         if(key(nv).gt.0 .and. fldok(nf))then
            ier = fstluk(fldfst,key(nv),ni,nj,nk)

!           Interpolate on MESH grid
            ier     = ezsint(TAGRD,fldfst)
            ier     = fstprm(key(nv),dateo,deet,npas,ni,nj,nk,      &
                      nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket, &
                      grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,   &
                      ex2,ex3)

!           Compute date of validation
            deltat = (deet*npas+1800)/3600
            call incdat(datev,dateo,deltat)
!           Write to R2C file
            write(nv,900)deltat,deltat,datev
            do i = 1, njr2c
               write(nf,'(100(F16.7,2X))')(TAGRD(i,j),j=1,nir2c)
            enddo
            write(nf,908)
         else
            fldok(nf) = .false.
            write(nf,*)'file ', nh
            write(nf,*)'no record is found for air temperature'
            write(nf,909)
         endif

!        Relative humidity
         nv = 6
         nf = 5
         if(key(nv).gt.0 .and. fldok(nf))then
            ier = fstluk(fldfst,key(nv),ni,nj,nk)

!           Interpolate on MESH grid
            ier     = ezsint(QAGRD,fldfst)
            ier     = fstprm(key(nv),dateo,deet,npas,ni,nj,nk,      &
                      nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket, &
                      grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,   &
                      ex2,ex3)

!           Compute date of validation
            deltat = (deet*npas+1800)/3600
            call incdat(datev,dateo,deltat)

!           Write to R2C file
            write(nv,900)deltat,deltat,datev
            do i = 1, njr2c
               write(nf,'(100(F16.7,2X))')(QAGRD(i,j),j=1,nir2c)
            enddo
            write(nf,908)
         else
            fldok(nf) = .false.
            write(nf,*)'file ', nh
            write(nf,*)'no record is found for specific humidity'
            write(nf,909)
         endif

!        Wind speed
         nv = 7
         nf = 6
         if(key(nv).gt.0 .and. key(nv+1).gt.0 .and. fldok(nf))then
            ier = fstluk(fldfst1,key(nv),ni,nj,nk)
            ier = fstluk(fldfst2,key(nv+1),ni,nj,nk)

!           Interpolate on MESH grid
            fldfst  = sqrt(fldfst1**2 + fldfst2**2)
            ier     = ezsint(UVGRD,fldfst)
            ier     = fstprm(key(nv),dateo,deet,npas,ni,nj,nk,      &
                      nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket, &
                      grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,   &
                      ex2,ex3)

!           Compute date of validation
            deltat = (deet*npas+1800)/3600
            call incdat(datev,dateo,deltat)
!           Write to R2C file
            write(nf,900)deltat,deltat,datev
            do i = 1, njr2c
               write(nf,'(100(F16.7,2X))')(UVGRD(i,j),j=1,nir2c)
            enddo
            write(nf,908)
         else
            fldok(nf) = .false.
            write(nf,*)'file ', nh
            write(nf,*)'no record is found for Wind speed'
            write(nf,909)
         endif

!        Air pressure
         nv = 9
         nf = 7
         if(key(nv).gt.0 .and. fldok(nf))then
            ier = fstluk(fldfst,key(nv),ni,nj,nk)

!           Interpolate on MESH grid
            ier     = ezsint(PRESGRD,fldfst)
            ier     = fstprm(key(nv),dateo,deet,npas,ni,nj,nk,      &
                      nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket, &
                      grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,   &
                      ex2,ex3)

!           Compute date of validation
            deltat = (deet*npas+1800)/3600
            call incdat(datev,dateo,deltat)
!           Write to R2C file
            write(nf,900)deltat,deltat,datev
            do i = 1, njr2c
               write(nf,'(100(F16.7,2X))')(PRESGRD(i,j),j=1,nir2c)
            enddo
            write(nf,908)
         else
            fldok(nf) = .false.
            write(nf,*)'file ', nh
            write(nf,*)'no record is found for air pressur'
            write(nf,909)
         endif

!        Close the FST file
         ier = fstfrm(iun)

!        Unlink the file unit number from the FST file
         ier = fclos(iun)
      enddo
      do nf = 1,nvr2c
         close(nf)
      enddo

900   format(':Frame        ',                                         &
              I2,8X,I2,1X,'"',I10,'"')
908   format(':EndFrame')

909   format('Use the voir command to open the FST file',              &
             'table of contents and check the variable',               &
             'name (NOMVAR) convention used in the',                   &
             'variable_attributes_module.f90 module.')
      end  
