c234567
      PROGRAM stn2class 

c  The following code runs the in house program named feseri
c  this code converts the station output file from GEM to standard
c  format.  The code below loops over days containing station file
c  and directs the output to a tmp directory.

      IMPLICIT NONE
      
c     Declarations
      integer       nkey, nvar, dirlen, nvpar, ICAN 
      integer       nspar, nsics, nvics
      parameter   ( nkey = 9 )
      parameter   ( nvar = 9, nvpar = 14, ICAN = 4)
      parameter   ( nspar = 3, nsics = 9, nvics = 10)
      parameter   ( dirlen = 24 )

      CHARACTER*8   cles(nkey), cdate
      CHARACTER*24  TITLE, NAME, PLACE
      character*128 nam(nkey),def(nkey)
      CHARACTER*128 exec,ifile,ofile
      CHARACTER*4   fcNm(ICAN)
      CHARACTER*2   varNm(nvar),vpNm(nvpar),spNm(nspar)
      CHARACTER*2   sicNm(nsics),vicNm(nvics)

      external  jjulien
      real      jjulien
      external  ccard, newdate, incdatr,fnom,fclos
      external  fstouv,fstfrm,fstinf,fstprm,fstinfx,fstluk
      integer   newdate, fnom, fclos, system
      integer   fstouv,fstfrm,fstinf,fstprm,fstinfx,fstluk
      integer   ier, npos, ii, topkey
      integer   start_stamp, end_stamp, stamp, old_stamp
      integer   stamp_hr
      integer   istart, iend, idate, itime
      integer   num_steps 
      integer   IHOUR,IMIN,IDAY,IYEAR
      integer   J, MIDROW
      real      FSDOWN,FDLGRD,PREGRD,TAGRD,QAGRD,UVGRD,PRESGRD
      real      DEGLAT,DEGLON
      real      DRNROW,SDEPROW,FAREROW
      real      XSLPROW,GRKFROW,WFSFROW,WFCIROW
      real      t0
      
      integer   key,iun,ni,nj,nk,ip1,ip2,ip3,ig1,ig2,ig3,ig4
      integer   dateo,deet,npas,nbits,datyp
      integer   swa,lng,dltf,ubc,extra1,extra2,extra3
      character etiket*12, nomvar*4, typvar*2, grtyp*1

      REAL,   dimension (:,:,:),       allocatable :: val
      REAL,   dimension (:,:),         allocatable :: var1D,vic1D
      REAL,   dimension (:),           allocatable :: hh

      DATA cles /'S.' ,  'D.' , 
     1           'Sdir.','Ddir.',
     2           'START','END', 
     3           'TITLE.','NAME.','PLACE.'/
      DATA def  /'   ', '   ',
     1           './output_',       './', 
     2           '20040601', '20040603', 
     3           'Great Lakes', 'Ken Snelgrove', 'RPN Dorval'/
      DATA nam  /'time_series.bin', 'CLASS',
     1           '   ', '   ', 
     2           '0',    '99999999',
     3           '   ', '   ', '   '/

c                  Surface Atmospheric Names
      Data varNm /'FI','FU','G6','G7','H8','P8','T8','U8','V8'/
      
c                  Vegetation Parameters
      Data fcNm /'Y2C1','Y2C2','Y2C3','Y2C4'/
      Data vpNm /'??','LX','X9','LN','C4','MC','C2','D2',
     1           'RS','Q5','VA','VB','PA','PB'/
     
c                  Soil Parameters
      Data spNm /'SA','CL','OR'/

c                  Soil Initial Conditions
c                   temperature      moisture          ice
      Data sicNm /'I0','TP','TB', 'WG','WR','WB', 'G4','GX','GY'/
      
c                  Veg/Surface Initial Conditions
c                   
      Data vicNm /'TE','TN','Q4','M9','C5','SK','G5','C7','DN','GR'/

      Data    t0 / 273.16 /   

c     Read input parameters
      npos = -1
      CALL CCARD(CLES,DEF,NAM,nkey,npos)

c     open .met output file 
      OPEN(unit=51,file=TRIM(nam(2))//'.met')

c     Start & End dates to integers and then CMC stamps
      READ(nam(5),'(i8)') istart
      READ(nam(6),'(i8)') iend
      
      ier = newdate(start_stamp,istart,0,3)
      ier = newdate(end_stamp,iend,0,3)
            
      stamp = start_stamp
      do
c       convert binary files to standard files using feseri           
        if(stamp .gt. end_stamp) exit 
        ier = newdate(stamp,idate,itime,-3)
        WRITE(cdate,'(i8)') idate 
	 
c       Note: executable feseri must be in your PATH      
        exec = 'feseri_2000.Abs '
        ifile ='-iserial '//TRIM(nam(3))//
     *                         cdate//'00'//'/'//nam(1)
        ofile ='-omsorti '//TRIM(nam(4))//
     *                        'ts'//cdate//'.fst'

c       run executable and dump screen output & error
        ier = system(exec//ifile//ofile//' &> /dev/null')
      
c       increment daily time step	
        old_stamp = stamp
        call incdatr(stamp,old_stamp,dble(dirlen))
      
      enddo

c     Read the standard files just written and write them into 
c     .met format.  This could be included in the pervious loop 
c     but is separated for readability.  Daily time loop is the same.

      stamp = start_stamp
      do
c       converts standard files to CLASS.met format

c       construct file name to open
        if(stamp .gt. end_stamp) exit 
        ier = newdate(stamp,idate,itime,-3)
	WRITE(cdate,'(i8)') idate 
         
	ifile = TRIM(nam(4))//'ts'//cdate//'.fst'
	 
c       Open one input file
        iun = 11
        ier = fnom(iun, ifile, 'STD+RND+R/O', 0)
        ier = fstouv(iun,'STD+RND')
	
c       Get times steps from HH record
c       ------------------------
	topkey = fstinf(iun,ni,nj,nk,-1,' ',-1,-1,-1,' ','HH')
	ier = fstprm(topkey,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,
     1        ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,swa,
     2        lng,dltf,ubc,extra1,extra2,extra3)

	allocate (hh(ni))                
        allocate (val(ni,nj,nk))
	ier = fstluk(val,topkey,ni,nj,nk)
	hh(:)=val(:,1,1)
        deallocate (val)	
c	WRITE(*,'(48f5.1)') hh
	
	num_steps = ni
        allocate (var1D(ni,nvar))
	       
        do ii = 1, nvar
c         read the variable data indexed from the HH record
c         ------------------------
	  key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ',varNm(ii))
	  allocate (val(ni,nj,nk))
          ier = fstluk(val,key,ni,nj,nk)
	  var1D(:,ii) = val(1,:,1)
	  deallocate (val)
c	  WRITE(*,'(a2,48(f12.6)') varNm(ii),var1D(:,ii)

	end do 
	
        do ii = 1,num_steps
c         loop over hh time steps and write .met file format          
c         ------------------------
          call incdatr(stamp_hr,dateo,dble(hh(ii)))
          ier = newdate(stamp_hr,idate,itime,-3)

          IHOUR = itime/1000000
	  IMIN  = (itime-(IHOUR*1000000))/10000
          IDAY  = INT(jjulien(0.0, 0, stamp_hr))
	  IYEAR = idate/10000
	  FSDOWN = var1d(ii,2)
	  FDLGRD = var1d(ii,1)
	  PREGRD = var1d(ii,3)+var1d(ii,4)
	  TAGRD  = var1d(ii,7)-t0
	  QAGRD  = var1d(ii,5)
	  UVGRD  = SQRT(var1d(ii,8)**2+var1d(ii,9)**2)
	  PRESGRD= var1d(ii,6)
	 
	  WRITE(51,5300)IHOUR,IMIN,IDAY,IYEAR,FSDOWN,FDLGRD,
     1         PREGRD,TAGRD,QAGRD,UVGRD,PRESGRD
           	        
        enddo

c       clean up files and work arrays
        deallocate (var1d)
	deallocate (hh)
        ier = fstfrm(iun)
	ier = fclos (iun)

c       increment daily time step	
        old_stamp = stamp
        call incdatr(stamp,old_stamp,dble(dirlen))

      end do

5300  FORMAT(1X,I2,I3,I5,I6,2F9.2,E14.4,F9.2,E12.3,F8.2,F12.2,3F9.2,
     1       F9.4)
      CLOSE(51)
      
      
c=======================================            
c     open .ini output file 
      OPEN(unit=50,file=TRIM(nam(2))//'.ini')
      
      WRITE(50,5010) nam(7)
      WRITE(50,5010) nam(8)
      WRITE(50,5010) nam(9)
      
      stamp = start_stamp
c     converts first standard files to CLASS.ini format
c     construct file name to open
      ier = newdate(stamp,idate,itime,-3)
      WRITE(cdate,'(i8)') idate 
      ifile = TRIM(nam(4))//'ts'//cdate//'.fst'
      
c     Open one input file
      iun = 11
      ier = fnom(iun, ifile, 'STD+RND+R/O', 0)
      ier = fstouv(iun,'STD+RND')
	
c     Get the index for the top of the file from HH record
c     ------------------------
      topkey = fstinf(iun,ni,nj,nk,-1,' ',-1,-1,-1,' ','HH')
      ier = fstprm(topkey,dateo,deet,npas,ni,nj,nk,nbits,datyp,ip1,
     1      ip2,ip3,typvar,nomvar,etiket,grtyp,ig1,ig2,ig3,ig4,swa,
     2      lng,dltf,ubc,extra1,extra2,extra3)

c     Read the TIC TAK data to determine the grid cell location
c     ------------------------
      key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ','^^')
      allocate (val(ni,nj,nk))
      ier = fstluk(val,key,ni,nj,nk)
      DEGLAT = val(1,1,1)
      deallocate (val)
      key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ','>>')
      allocate (val(ni,nj,nk))
      ier = fstluk(val,key,ni,nj,nk)
      DEGLON = val(1,1,1)
      deallocate (val)
      
c     Read heights 
c     ------------------------
      key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ','SV')
      allocate (val(ni,nj,nk))
      ier = fstluk(val,key,ni,nj,nk)
c      WRITE(*,*) val
      deallocate (val)
      key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ','SH')
      allocate (val(ni,nj,nk))
      ier = fstluk(val,key,ni,nj,nk)
c      WRITE(*,*) val
      deallocate (val)
      
      WRITE(50,5020) DEGLAT, DEGLON, 50., 50., 50., -1., 1, 1, 1

      allocate (var1D(ICAN,nvpar))
c       fix to allow for 4-character fraction name in the first veg field
      do ii = 1, ICAN
        key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ',fcNm(ii))
	allocate (val(ni,nj,nk))
        ier = fstluk(val,key,ni,nj,nk)
	var1D(ii,1) = val(1,1,1)
	WRITE(*,'(a4,4(f20.6)') fcNm(ii),var1D(ii,1)
	deallocate (val)

      end do       
      do ii = 2, nvpar
c       read the vegetation parameters indexed from the HH record
c       ------------------------
        key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ',vpNm(ii))
	allocate (val(ni,nj,nk))
        ier = fstluk(val,key,ni,nj,nk)
	var1D(:,ii) = val(:,1,1)
c	deallocate (val)
	WRITE(*,'(a2,4(f12.6)') vpNm(ii),var1D(:,ii)
        deallocate (val)
      end do 


      WRITE(50,5040) (var1D(J,1),J=1,ICAN),0.,(var1D(J,2),
     1                J=1,ICAN)
      WRITE(50,5040) (var1D(J,3),J=1,ICAN),0.,(var1D(J,4),
     1                J=1,ICAN)
      WRITE(50,5040) (var1D(J,5),J=1,ICAN),0.,(var1D(J,6),
     1                J=1,ICAN)
      WRITE(50,5040) (var1D(J,7),J=1,ICAN),0.,(var1D(J,8),
     1                J=1,ICAN)
      WRITE(50,5030) (var1D(J,9), J=1,ICAN),
     1               (var1D(J,10),J=1,ICAN)
      WRITE(50,5030) (var1D(J,11),J=1,ICAN),
     1               (var1D(J,12),J=1,ICAN)
      WRITE(50,5030) (var1D(J,13),J=1,ICAN),
     1               (var1D(J,14),J=1,ICAN)

      deallocate (var1d)      
      
c     read the variable data indexed from the HH record
c     ------------------------
      key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ','DB')
      allocate (val(ni,nj,nk))
      ier = fstluk(val,key,ni,nj,nk)
      SDEPROW = val(1,1,1)
      deallocate (val)
      DRNROW = 1.
      FAREROW = 1.

      WRITE(50,5040) DRNROW,SDEPROW,FAREROW

c     read lateral drainage parameters; currently not used in CLASS
c     ------------------------
      XSLPROW = .99
      GRKFROW = .99
      WFSFROW = .99
      WFCIROW = .99
      MIDROW = 1
      
      WRITE(50,5090) XSLPROW,GRKFROW,WFSFROW,WFCIROW,MIDROW
      
      allocate (var1D(3,nspar))
      do ii = 1, nspar
c       read the soil parameters indexed from the HH record
c       ------------------------ 
        key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ',spNm(ii))
	allocate (val(ni,nj,nk))
        ier = fstluk(val,key,ni,nj,nk)
	var1D(:,ii) = val(:,1,1)
	deallocate (val)
c	WRITE(*,'(a2,48(f12.6)') varNm(ii),var1D(:,ii)

      end do 

      WRITE(50,5080) (var1D(J,1),J=1,3)
      WRITE(50,5080) (var1D(J,2),J=1,3)
      WRITE(50,5080) (var1D(J,3),J=1,3)

      deallocate (var1d)
       
      allocate (var1D(1,nsics))
      do ii = 1, nsics
c       read the soil initial conditions indexed from the HH record
c       ------------------------ 
        key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ',sicNm(ii))
	allocate (val(ni,nj,nk))
        ier = fstluk(val,key,ni,nj,nk)
	var1D(1,ii) = val(1,1,1)
	deallocate (val)
c	WRITE(*,'(a2,48(f12.6)') varNm(ii),var1D(:,ii)
	
      end do
      
      allocate (vic1D(1,nvics))
      do ii = 1, nvics
c       read the vegetation initial conditions indexed from the HH record
c       ------------------------ 
        key = fstinfx(topkey,iun,ni,nj,nk,-1,' ',-1,-1,-1,' ',vicNm(ii))
	allocate (val(ni,nj,nk))
        ier = fstluk(val,key,ni,nj,nk)
	vic1D(1,ii) = val(1,1,1)
	deallocate (val)
c	WRITE(*,'(a2,48(f12.6)') varNm(ii),var1D(:,ii)
	
      end do
      

      WRITE(50,5050) var1D(1,1)-t0,var1D(1,2)-t0,var1D(1,3)-t0,
     1               vic1D(1,1)-t0,vic1D(1,2),vic1D(1,3)
      WRITE(50,5060) var1D(1,4),var1D(1,5),var1D(1,6),
     1               var1D(1,7),var1D(1,8),var1D(1,9),vic1D(1,4)
      WRITE(50,5070) vic1D(1,5),vic1D(1,6),vic1D(1,7),
     1               vic1D(1,8),vic1D(1,9),vic1D(1,10)
     
      deallocate(var1D,vic1D)

c     clean up files and work arrays
      ier = fstfrm(iun)
      ier = fclos (iun)


5010  FORMAT(2X,A24)
5020  FORMAT(5F10.2,F7.1,3I5)
5030  FORMAT(4F8.3,8X,4F8.3)
5040  FORMAT(9F8.3)
5050  FORMAT(6F10.2)
5060  FORMAT(7F10.3)
5070  FORMAT(2F10.4,F10.2,F10.3,F10.4,F10.3)
5080  FORMAT(3F10.1)
5090  FORMAT(4E8.1,I8)      
      CLOSE(50)
      
      END PROGRAM stn2class
