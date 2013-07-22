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
!*     fst2r2c executable as "fst2r2c < input.fst output.r2c"
!***********************************************************************

!     MESH forcing data attributes
      use variable_attributes_module

!     MESH grid parameters
      use grid_parameters_module

      implicit none

!     External functions
      integer, external :: iargc
      integer, external :: fnom, fstouv, fclos, fstfrm, fstinf, fstinl,&
                           fstluk, fstprm
      integer, external :: ezqkdef, ezdefset, ezsint

!     Declare variables used by the RPN standard file library
      character*2 nomvar
      character*1 typvar, grtyp
      character*8 etiket
  
!     The ikeys are identifiers for the IPs and IGs        
      integer, parameter :: nmax = 1500
      integer keylat, keylon, keys(nmax),nkeys,gdfst 
      integer key, dateo, deet, npas, nifst, njfst, nkfst, npak, datyp 
      integer ip1, ip2, ip3
      integer ig1, ig2, ig3, ig4
      integer ex1, ex2, ex3, deet, npas, nbits, datyp, dateo
      integer swa, lng, dltf, ubc

!     xlat0 and xlon0 are the location of SW corner of the MESH grid
!     dlat and dlon are the grid spacing
!     ni and nj are number of grids in the longitudinal and in the 
!     latitudinal directions respectively.
      integer nir2c, njr2c, nkr2c, gdr2c
      real    xlat0, xlon0, dlat, dlon
      real    xg1, xg2, xg3, xg4

!     Declare other variables used by the program
      integer      ier, nrecs
      integer      i,j, ii, jj, nv, nk, iun
      integer      year, month, day, hour, minute
      real         second
      character*20 fstname

!     Temporary storage for non-important readings
      integer jk
      real    cjk

!     Forcing data dimension in the FST file
      real, dimension(:,:), allocatable ::  fldfst, workfst
      real, dimension(:), allocatable ::    latfst, lonfst

!     Forcing data dimension in the FST file
      real, dimension(:,:), allocatable ::  fldr2c
      real, dimension(:), allocatable ::    latr2c, lonr2c

!     get command line arguments
      if(iargc() .ne. 1)then
         print*,'Error: one file name argument is needed.'
         call exit(1)
      else
         call getarg(1, fstname)
      endif
      print 10, 'Input FST file name:  ', fstname
  
!     Open FST data file
      iun = 100
      ier = fnom(iun, trim(fstname), 'STD+RND+R/O', 0)
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
         print *,'ERROR: Cannot open file ', trim(fstname), &
                 ' in random mode'
         print*,'-------------------------------------------------'
        stop
      endif

!     Get grid dimensions and specifications from FST file
      keylon = fstinf(iun,nifst,jk,nkfst,-1,' ',-1,-1,-1,' ','>>')
      ier = fstprm(keylon,dateo,deet,npas,nifst,jk,nkfst,nbits,        &
                   datyp,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,       &
                   ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,ex2,ex3)
      write(*,20)dateo,deet,npas,nifst,jk,nkfst,nbits,datyp,           &
                 ip1,ip2,ip3,typvar,trim(nomvar),trim(etiket),grtyp,   &
                 ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,ex2,ex3

      keylat = fstinf(iun,jk,njfst,nkfst,-1,' ',-1,-1,-1,' ','^^')
      ier = fstprm(keylat,dateo,deet,npas,jk,njfst,nkfst,nbits,        &
                   datyp,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp,       &
                   ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,ex2,ex3)
      write(*,20)dateo,deet,npas,jk,njfst,nkfst,nbits,datyp,           &
                 ip1,ip2,ip3,typvar,trim(nomvar),trim(etiket),grtyp,   &
                 ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,ex2,ex3

      allocate(fldfst(nifst,njfst))

!     Define FST grid
      gdfst = ezqkdef(nifst,njfst,grtyp,ig1,ig2,ig3,ig4,iun)

!     MESH grid
      call read_grid_parameters()
      xlat0 = yOrigin
      xlon0 = xOrigin
      dlat  = yDelta
      dlon  = xDelta
      nir2c = yCount
      njr2c = xCount
      nkr2c = 1
 
      allocate (fldr2c(nir2c,njr2c))
      allocate (latr2c(njr2c))
      allocate (lonr2c(nir2c))

      fldr2c = 0.0
      year = 2000
      month = 1
      day = 1
      hour = 0
      minute = 0
      second = 0.0
      
!     Computation of ig parameters
      call cxgaig('L',ig1,ig2,ig3,ig4,xlat0,xlon0,dlat,dlon)
      print*,'-------------------------------------------------'
      print *,'R2C grid parameters:'
      print*,'ig1 = ',ig1
      print*,'ig2 = ',ig2
      print*,'ig3 = ',ig3
      print*,'ig4 = ',ig4
      print*,'-------------------------------------------------'

!     Define output grid
      gdr2c = ezqkdef(nir2c,njr2c,'L',ig1,ig2,ig3,ig4,lonr2c,latr2c)
      ier   = ezdefset(gdr2c,gdfst)

!     Read fld info and data
      do nv = 1, 1 !nvar

!      rm(tmpiun)

         open(nv,file=trim(FileName(nv)))
         call write_header(nv)
!     do t=timestart,timeend
         write(nv,900)hour,hour,year,month,day,hour,minute,second

         ier = fstinl(iun,nifst,njfst,nkfst,-1,' ',-1,-1,-1,' ',       &
                      varNm(nv),keys,nkeys,nmax)
         print*,'nkeys = ',nkeys
         do nk = 1, nkeys
             ier = fstluk(fldfst,keys(nk),nifst,njfst,nkfst)
             ier = fstprm(keys(nk),dateo,deet,npas,nifst,njfst,nkfst,  &
                          nbits,datyp,ip1,ip2,ip3,typvar,nomvar,etiket,&
                          grtyp,ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,  &
                          ex2,ex3)
             write(*,20)dateo,deet,npas,nifst,njfst,nkfst,nbits,datyp, &
                 ip1,ip2,ip3,typvar,trim(nomvar),trim(etiket),grtyp,   &
                 ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,ex2,ex3

!            Interpolate on MESH grid
             ier   = ezsint(fldr2c,fldfst)

!            do t=timestart,timeend
             write(nv,900)hour,hour,year,month,day,hour,minute,second
             do i=1,njr2c
                write(nv,'(100(F16.7,4X))')(fldr2c(i,j),j=1,nir2c)
             enddo
             write(nv,908)
         enddo
         close(nv)
      enddo

!     Close the FST file
      ier = fstfrm(iun)

!     Unlink the file unit number from the FST file
      ier = fclos(iun)

10    format(A,A)

20    format('-------------------------------------------------',/,&
             'FST record attributes:',/,                           &
             'dateo    = ',i10,/,                                  &
             'deet     = ',i10,/,                                  &
             'npas     = ',i10,/,                                  &
             'nifst    = ',i10,/,                                  &
             'njfst    = ',i10,/,                                  &
             'nkfst    = ',i10,/,                                  &
             'nbits    = ',i10,/,                                  &
             'datyp    = ',i10,/,                                  &
             'ip1      = ',i10,/,                                  &
             'ip2      = ',i10,/,                                  &
             'ip3      = ',i10,/,                                  &
             'typvar   = ',a10,/,                                  &
             'nomvar   = ',a10,/,                                  &
             'etiket   = ',a10,/,                                  &
             'grtyp    = ',a10,/,                                  &
             'ig1      = ',i10,/,                                  &
             'ig2      = ',i10,/,                                  &
             'ig3      = ',i10,/,                                  &
             'ig4      = ',i10,/,                                  &
             'swa      = ',i10,/,                                  &
             'lng      = ',i10,/,                                  &
             'dltf     = ',i10,/,                                  &
             'ubc      = ',i10,/,                                  &
             'ex1      = ',i10,/,                                  &
             'ex2      = ',i10,/,                                  &
             'ex3      = ',i10,/,                                  &
             '-------------------------------------------------')

900   format(':Frame        ', &
              I2,8X,I2,1X,'"',I4,'/',I2.2,'/',I2.2,1X, &
              I2.2,':',I2.2,':',f6.3,'"')
908   format(':EndFrame')

      end  
