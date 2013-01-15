      program main
!***********************************************************************
!     This program:
!        -reads 'header.r2c' file and generates a LAT/LON grid, 
!        -reads values for attributes of a given record from specified
!         FST file, 
!        -interpolates values on generated LAT/LON grid, and
!        -writes interpolated values into a multi-frame r2c file.
!
!     Written by Muluneh A. Mekonnen, September 15, 2010
!***********************************************************************

      implicit none

!     External function to read input arguments of the FST2R2C program
      integer, external :: iargc

!     External Functions from ARMNLIB
      integer, external :: fnom      !Associate Fortran unit number to a standard file name
      integer, external :: fclos     !Diassociate a Fortran unit number from a standard file name
      integer, external :: fstouv    !Open a standar file
      integer, external :: fstfrm    !Close a standard file

      integer, external :: fstinf    !Retrieve the key of the first record meeting selection criteria
      integer, external :: fstprm    !Retrieve all the attributes of a record

      integer, external :: fstluk    !Read a field using a key pointing to a record

!     Functions defined in the EZSCINT package (also available from ARMNLIB). For details:
!     http://collaboration.cmc.ec.gc.ca/science/si/eng/si/libraries/rmnlib/ezscint/functions.html
      integer, external :: ezqkdef   !Universal grid defïnition
      integer, external :: ezdefset  !Defines a set of grids for interpolation
      integer, external :: ezsetopt  !Sets an option (ex. interpolation option) for the package
      integer, external :: ezsint    !Scalar interpolation

!     Attributes of an FST record. For the details of these attributes refer to:
!     http://collaboration.cmc.ec.gc.ca/science/si/eng/si/libraries/rmnlib/fstd/fstd.pdf
      integer       ip1,ip2,ip3,ig1,ig2,ig3,ig4
      integer       ex1,ex2,ex3,npas,npak,nbits,datyp,deet,dateo,datev
      integer       swa,lng,dltf,ubc,npts,nnpas,idt,ipos
      character*1   typvar,grtyp,grref
      character*2   nomvar
      character*8   etiket

!     Date and time information of the record to be stamped in the R2C frame
      integer deltat                 !time lapse defined by deet*npas (in hours)
      integer yyyymmdd,hhmmssHH      !date and hour
      character*10 strymd            !date as a string of characters
      character*12 strhms            !hour as a string of characters
      external           :: incdat   !subroutine which computes date of validity (DATEV) from date of
                                     !original analysis and a time lapse defined by deet*npas (in hours)
      integer, external  :: newdate  !extracts date and hour from DATEV

!     Keywords for the R2C header file
      integer      xCount            !number of points, in each row of the grid, along the x-direction 
      integer      yCount            !number of points, in each column of the grid, along the y-direction
      real         xOrigin           !the x-coordinate of the point in the bottom left corner of the grid
      real         yOrigin           !the y-coordinate of the point in the bottom left corner of the grid
      real         xDelta            !distance between two adjacent points in a row
      real         yDelta            !distance between two adjacent points in a column
      character*20 Projection        !The coordinate system of the data 
                                     !Valid value for the FST2R2C program in its current state is 'LATLONG'
      character*20 Ellipsoid         !The ellipsoid used by the coordinate system
                                     !Possible values are 'Clark66', 'GRS80', 'WGS72', 'WGS74', or 'Sphere'

!     Arguements used by the FST2R2C program
      integer       ip2start         !Forecast starting hour
      integer       ip2end           !Forecast ending hour
      real*8        cfactorm         !Multiplier - Conversion factor to extract the R2C records with a
                                     !different unit. Provide a value of 1.0 if the R2C records are
                                     !required to have the same unit than the corresponding FST record.
      real*8        cfactora         !Additive - Conversion factor to extract the R2C records with a
                                     !different unit. Provide a value of 0.0 if the R2C records are
                                     !required to have the same unit than the corresponding FST record.
      character*3   varNameX         !Name of the FST record to be extracted - x component
      character*3   varNameY         !Name of the FST record to be extracted - y component
      character*70  attrName         !Variable attribute name to be written on the R2C header file
      character*70  attrUnit         !Variable unit to be written on the R2C header file
      character*100 fstName          !FST file name (Input file)
      character*100 r2cName          !R2C file name (Output file)
      character*100 r2cFormat        !Format for the data (body) section of the R2C file

!     Local variables
      integer       i,j,ier,iunfst,iunr2c,ifh,ni,nj,nk
      integer       keyX,keyY,nifst,njfst,nkfst 
      integer       gdfst,gdr2c
      logical       I_EXIST
      character*100 arg              

      real, dimension(:,:), allocatable ::fldr2c,fldr2cX,fldr2cY,      &
                                          fldfstX,fldfstY
    
!     get command line arguments
      if(iargc() .ne. 11)then
         print*,'ERROR: 11 arguments needed: '
         print*,'1 - FST file name (Input file)',                      &
                '2 - FST variable name to be extracted - x component ',&
                '3 - FST variable name to be extracted - y component ',&
                '4 - Forecast starting hour',                          &
                '5 - Forecast ending hour',                            &
                '6 - R2C file name (output file)',                     &
                '7 - Conversion factor - Multiplier',                  &
                '8 - Conversion factor - For addition',                &
                '9 - Variable attribute name for the R2C header file', &
                '10- Variable unit (eg. mm/s) for the R2C header file',&
                '11- Format for data (body) section of the R2C file.'
         call exit(1)
      else
         call getarg(1, fstName)
         call getarg(2, varNameX)
         call getarg(3, varNameY)
         call getarg(4, arg)
         read(arg,*) ip2start
         call getarg(5, arg)
         read(arg,*) ip2end
         call getarg(6, r2cName)
         call getarg(7, arg)
         read(arg,*) cfactorm
         call getarg(8, arg)
         read(arg,*) cfactora
         call getarg(9, attrName)
         call getarg(10, attrUnit)
         call getarg(11, r2cFormat)
      endif

!     Screen output - FST file name
      write(*,'(A,A)')'Input FST file name:  ', adjustl(trim(fstName))

!     R2C grid parameters - read from header.r2c file
      call read_r2c_header(Projection,                                 &
                           Ellipsoid,                                  &
                           xOrigin,                                    &
                           yOrigin,                                    &
                           xCount,                                     &
                           yCount,                                     &
                           xDelta,                                     &
                           yDelta)

      if (Projection .ne. "LATLONG")then
          print*,'------------------------------------------------'
          print*,'Currently the LATLONG projection of MESH grid is'
          print*,'functional.'
          print*,'------------------------------------------------'
          stop
      endif

!     Allocate R2C 2D field data
      allocate (fldr2c(xCount,yCount))
      allocate (fldr2cX(xCount,yCount))
      allocate (fldr2cY(xCount,yCount))

!     Write header on the forcing file
      inquire(file=trim(r2cName), EXIST=I_EXIST)
      iunr2c = 10
      if(I_EXIST)then
         open(iunr2c,file=trim(r2cName),position='append',status='old')
      else
         open(iunr2c,file=trim(r2cName),status='unknown')
         call write_r2c_header(iunr2c,                                 &
                               varNameX(1:1)//varNameY(1:1),           &
                               attrName,                               &
                               attrUnit,                               &
                               Projection,                             &
                               Ellipsoid,                              &
                               xOrigin,                                &
                               yOrigin,                                &
                               xCount,                                 &
                               yCount,                                 &
                               xDelta,                                 &
                               yDelta)
      endif 

!     Associate unit number of 20 and open the FST data file
      iunfst = 20
      ier    = fnom(iunfst, trim(fstName), 'STD+RND+R/O', 0)
      if(ier .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: Cannot associate a Fortran unit number to',   &
                  trim(fstName)
         print*,'-------------------------------------------------'
         stop
      endif
      ier = fstouv(iunfst, trim(fstName), 'RND')
      if(ier .lt. 0)then
         print*
         print*,'-------------------------------------------------'
         print *,'ERROR: Cannot open file ', trim(fstName),            &
                 ' in random mode'
         print*,'-------------------------------------------------'
         stop
      endif
      
!     Loop through starting forecast hour to ending forecast hour
      do ifh = ip2start, ip2end

!        Search for a record of nomvar = varName and ip2 = ifh
       keyX = fstinf(iunfst,ni,nj,nk,-1,' ',-1,ifh,-1,'',trim(varNameX))
       keyY = fstinf(iunfst,ni,nj,nk,-1,' ',-1,ifh,-1,'',trim(varNameY))

!        Extract the attributes if the record exists
         if(keyX .gt. 0 .and. keyY .gt. 0)then
            ier = fstprm(keyX,dateo,deet,npas,nifst,njfst,nkfst,nbits, &
                         datyp,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp, &
                         ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,ex2,ex3)


            ier = fstprm(keyY,dateo,deet,npas,nifst,njfst,nkfst,nbits, &
                         datyp,ip1,ip2,ip3,typvar,nomvar,etiket,grtyp, &
                         ig1,ig2,ig3,ig4,swa,lng,dltf,ubc,ex1,ex2,ex3)

!            Allocate variable for the two dimensional FST record
             allocate(fldfstX(nifst,njfst))
             allocate(fldfstY(nifst,njfst))

!            Read the record
             ier = fstluk(fldfstX,keyX,ni,nj,nk)
             ier = fstluk(fldfstY,keyY,ni,nj,nk)

!            Define FST grid
             gdfst = ezqkdef(nifst,njfst,grtyp,ig1,ig2,ig3,ig4,iunfst)

!            Computation of ig parameters
!            'L' grid type is selected to represent the R2C domain
             call cxgaig('L',ig1,ig2,ig3,ig4,                          &
                         yOrigin,xOrigin,yDelta,xDelta)

!            Define the R2C grid to be used for interpolation
             gdr2c = ezqkdef(xCount,yCount,'L',ig1,ig2,ig3,ig4,0)

!            Define a set of grids for interpolation
             ier    = ezdefset(gdr2c, gdfst)

!            Set interpolation option
             ier    = ezsetopt('INTERP_DEGREE','LINEAIR')

!            X - component interpolation into the R2C grid
             ier    = ezsint(fldr2cX, fldfstX)

!            Y - component interpolation into the R2C grid
             ier    = ezsint(fldr2cY, fldfstY)

!            Compute the resultant
             fldr2c  = sqrt(fldr2cX * fldr2cX + fldr2cY * fldr2cY)

!            Apply unit conversion if necessary
             fldr2c = fldr2c * cfactorm + cfactora

!            Compute time elapsed since date of origin of forecast
             deltat = (deet * npas + 1800) / 3600

!            Calculate date of validity
             CALL incdat(datev,dateo,deltat)

!            Extract date and hour information - to be used for time stamp in R2C frame
             ier = newdate(datev,yyyymmdd,hhmmssHH,-3)
             write(strymd,'(I8)'),yyyymmdd
             strymd = strymd(1:4)//'/'//strymd(5:6)//'/'//strymd(7:8)
             write(strhms,'(I8)')hhmmssHH
             do i=1,8
               if(strhms(i:i).eq.' ')strhms(i:i)='0'
             enddo
        strhms = strhms(1:2)//':'//strhms(3:4)//':'//strhms(5:6)//'.000'

!           Write time stamp to R2C file
            write(iunr2c,'(A,I8,I8,A,A,A,A,A)'), &
               ':Frame ',ip2,ip2,' "',strymd,' ',strhms,'"'

!           Write data to R2C file
            do j = 1, yCount
               write(iunr2c,trim(r2cFormat))(fldr2c(i,j),i=1,xCount)
            enddo
            write(iunr2c,901)
         else
            write(iunr2c,*)'File: ', fstName
            write(iunr2c,*)'no FST record is found'
            write(iunr2c,902)
         endif
      enddo

!     Close the FST file
      ier = fstfrm(iunfst)

!     Unlink the file unit number from the FST file
      ier = fclos(iunfst)

!    Close the r2c file
      close(iunr2c)
 
900   format(':Frame        ',I2,8X,I2,1X,'"',I10,'"')
901   format(':EndFrame')
902   format('Use the voir command to open the FST file',              &
             'table of contents and check the variable',               &
             'name (NOMVAR) convention used in the',                   &
             'variable_attributes_module.f90 module.')
      end  
      subroutine write_r2c_header(iunr2c,                              &
                                  varName,                             &
                                  attrName,                            &
                                  attrUnit,                            &
                                  Projection,                          &
                                  Ellipsoid,                           &
                                  xOrigin,                             &
                                  yOrigin,                             &
                                  xCount,                              &
                                  yCount,                              &
                                  xDelta,                              &
                                  yDelta)
!     Writes the header of R2C file
    
      implicit none

      character*3  varName
      character*70 attrName, attrUnit
      character*20 Projection,Ellipsoid
      integer      iunr2c,xCount,yCount
      real         xOrigin,yOrigin,xDelta,yDelta

      integer       varid,today(3),now(3)
      external      idate, itime
    
!     Get today's data and time
      call idate(today)
      call itime(now)
    
!     Write header
      write(iunr2c,10)today,now,                                       &
                      varName,                                         &
                      Projection,                                      &
                      Ellipsoid,                                       &
                      xOrigin,                                         &
                      yOrigin,                                         &
                      trim(attrName),                                  &
                      trim(attrUnit),                                  &
                      xCount,                                          &
                      yCount,                                          &
                      xDelta,                                          &
                      yDelta
                 
10    format('########################################',/              &
             ':FileType r2c  ASCII  EnSim 1.0',/                       &
             '#',/                                                     &
             '# DataType               2D Rect Cell',/                 &
             '#',/                                                     &
             ':Application             FORTRAN',/                      &
             ':Version                 1.0.0',/                        &
             ':WrittenBy               MSC/HAL/GIWS',/                 &
             ':CreationDate            ',                              &
             i2.2,'/',i2.2,'/',i4,x,i2.2,':',i2.2,':',i2.2,/           &
             '#',/                                                     &
             '#---------------------------------------',/              &
             '#',/                                                     &
             ':Name                    ',A,/                           &
             '#',/                                                     &
             ':Projection              ',A,/                           &
             ':Ellipsoid               ',A,/                           &
             '#',/                                                     &
             ':xOrigin                 ',f9.4,/                        &
             ':yOrigin                 ',f9.4,/                        &
             '#',/                                                     &
             ':SourceFile              XXXX/           X',/            &
             '#',/                                                     &
             ':AttributeName           ',A,/                           &
             ':AttributeUnit           ',A,/                           &
             '#',/                                                     &
             ':xCount                 ',i3,/                           &
             ':yCount                 ',i3,/                           &
             ':xDelta                  ',f9.6,/                        &
             ':yDelta                  ',f9.6,/                        &
             '#',/                                                     &
             '#',/                                                     &
             ':endHeader')
      return
      end

      subroutine read_r2c_header(Projection,                           &
                                 Ellipsoid,                            &
                                 xOrigin,                              &
                                 yOrigin,                              &
                                 xCount,                               &
                                 yCount,                               &
                                 xDelta,                               &
                                 yDelta)
!**********************************************************************
!    Reads the R2C header file to get values for the following 8 
!    parameters:
!    1. projection
!    2. ellipsoid
!    3. xorigin
!    4. yorigin
!    5. xcount
!    6. ycount
!    7. xdelta
!    8. ydelta
!
!    This subroutine and the associated functions are based on the work 
!    by Nick Kouwen and Dave Watson 2007
!***********************************************************************

     implicit none

!    Outgoing variables
     character*20 Projection,Ellipsoid
     integer      xCount,yCount
     real         xOrigin,yOrigin,xDelta,yDelta

!     Local variables
      integer*4      ios
      character*1024 line, subString, tmpString
      character*128  keyword, value
      integer        lineLen, keyLen, wordCount, ncount, npar
      logical        exists, foundEndHeader, rStat

!     Functions
      integer, external :: splitLine
      logical, external :: Detab, ToLowerCase 

!     Check if the r2c header file exists and is readable
      inquire(file='header.r2c',exist=exists)
      if(exists)then
         open(1,file='header.r2c',status='old',iostat=ios)
         if(ios.ne.0)then
            print*,'ERROR: Could not open the header.r2c file'
            stop
         endif
      else
         print*,'ERROR: header.r2c file not found '
         stop 
      endif
  
!     Initialize the search for the end of the header
      foundEndHeader = .false.

!     Initialize the counter for grid parameters. 
      ncount = 0
      npar = 8

!     Search for grid parameters from the r2c file header
      line(1:1) = '#'
      do while((.NOT.foundEndHeader) .AND.((line(1:1) .eq. '#') .OR. &
                (line(1:1) .eq. ':') .OR. (len_trim(line) .eq. 0)))

!        read a line
         read(1, '((A))', iostat=ios) line
         if(ios .eq. -1)then
            print*,'Premature EndOfFile encountered in reading', &
                   'header.r2c header'
            stop
         end if

!        Replace tabs with spaces
         rStat = Detab(line)

!        Get rid of leading white space
         line = adjustl(line)

!        Find the length excluding trailing spaces
         lineLen = len_trim(line) 
         if(line(1:1) .eq. ':')then

!           Find the keyword
            wordCount = SplitLine(line, keyword, subString)

!           Change to lower case
            rStat = ToLowerCase(keyword)
            KeyLen = len_trim(keyword)
            if(keyword(1:KeyLen) .eq. ':endheader')then
               foundEndHeader = .TRUE.
            else

!              If keyword is a grid parameter
!              Projection - 1st grid parameter
               if(keyword(1:keyLen) .eq. ':projection')then
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)Projection
                     ncount = ncount + 1
                  else
                     print*,'Couldnot read Projection type'
                  endif

!              Ellipsoid - 2nd grid parameter
               elseif(keyword(1:keyLen) .eq. ':ellipsoid')then
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)Ellipsoid
                     ncount = ncount + 1
                  else
                     print*,'Couldnot read Ellipsoid type'
                  endif

!              xOrigin - 3rd grid parameter
               elseif(keyword(1:keyLen) .eq. ':xorigin')then
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)xOrigin
                     ncount = ncount + 1
                  else
                     print*,'Couldnot read xOrigin value'
                  endif

!              yOrigin - 4th grid parameter
               elseif(keyword(1:keyLen) .eq. ':yorigin')then
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)yOrigin
                     ncount = ncount + 1
                  else
                     print*,'Couldnot read yOrigin value'
                  endif

!              xCount - 5th grid parameter
               elseif(keyword(1:keyLen) .eq. ':xcount')then
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)xCount
                     ncount = ncount + 1
                  else
                     print*,'Couldnot read xCount value'
                  endif

!              yCount - 6th grid parameter
               elseif(keyword(1:keyLen) .eq. ':ycount')then
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)yCount
                     ncount = ncount + 1
                  else
                     print*,'Couldnot read yCount value'
                  endif

!              xDelta - 7th grid parameter
               elseif(keyword(1:keyLen) .eq. ':xdelta')then
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)xDelta
                     ncount = ncount + 1
                  else
                     print*,'Couldnot read xDelta value'
                  endif

!              yDelta - 8th grid parameter
               elseif(keyword(1:keyLen) .eq. ':ydelta')then
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)yDelta
                     ncount = ncount + 1
                  else
                     print*,'Couldnot read yDelta value'
                  endif
               endif
            endif
         endif
      enddo
      close(1)
      if(ncount < npar)then
          print*,'Not all required parameters are read'
          stop
      endif
      return
      end

!C***********************************************************************
        integer FUNCTION SplitLine(line, keyword, value)
!C***********************************************************************
!C Extract the first white space delimited substring from line and place
!C it into keyword. Put the rest of line into value.
!C
!C Do NOT change the incoming line
!C
!C Return:       0 = the line is empty
!C               1 = there is ONLY a keyword
!C               2 = there is a keyword and a value
!C***********************************************************************

        character*(*) line
        character*(*) keyword, value
        character*4096 localLine
        integer I, DQ, lineLen
        logical rStat

        logical, external :: Detab
 
        SplitLine = 0
        localLine = line                        ! work on local copy of the string

        rStat = Detab(localLine)                                ! change any tabs to spaces
        localLine = ADJUSTL(localLine)                  ! get rid of leading spaces
        lineLen = LEN_TRIM(localLine)   ! find the new length, excluding trailing spaces

        if(lineLen .eq. 0) then ! we have an empty line
                SplitLine = 0
                return
        end if


        I = INDEX(localLine(1:lineLen),' ')     ! Find the first space character

! Deal with possibility of quotes (strings)
        if(localLine(1:1) .eq. '"')then
                DQ = INDEX(localLine(2:lineLen),'"')+1
                localLine(1:1) = CHAR(32) !replace first occurance of quote with a space
                if(DQ.GT.1) then
                        localLine(DQ:DQ) = CHAR(32) !replace 2nd occurance of quote with a space

!C                       localLine = ADJUSTL(localLine)                  ! get rid of leading spaces
!C                       lineLen = LEN_TRIM(localLine)
                        if(DQ.EQ.lineLen) then
                                keyword = localLine(2:lineLen-1)
                                value = ''
                                SplitLine = 1
                                return
                        else
                                keyword = localLine(2:DQ-1)
                                value = localLine(DQ+1:lineLen)
                                value = ADJUSTL(value)
                                SplitLine = 2
                        end if
                        return
                end if
        end if


        if((I.EQ.0) .AND. (lineLen.GT.0))then   ! Single word only
                keyword = localLine(1:lineLen)
                value = ''
                SplitLine = 1
        else
                keyword = localLine(1:I)
                value = localLine(I+1:lineLen)
                value = ADJUSTL(value)
                SplitLine = 2
        end if

        return
        end FUNCTION

!C***********************************************************************
        logical FUNCTION Detab(line)
!C***********************************************************************
!C Replace all tabs in line with space characters.
!C
!C Return:       = .false. no change occured
!C               = .true. the string was changed
!C***********************************************************************

        character*(*) line
        character*1 tab, space
        integer I

        tab = CHAR(9)           ! What we are looking for
        space = CHAR(32)        ! the substitution value

        Detab = .false.
        do I=1,LEN_TRIM(line)
                if(line(I:I) .eq. tab)then
                        line(I:I) = space
                        Detab = .true.
                end if
        end do

        return
        end FUNCTION

!***********************************************************************
        logical FUNCTION ToLowerCase(line)
!C***********************************************************************
!C Replace all uppercase character in line with lowercase characters.
!C
!C Return:       = .false. no change occured
!C               = .true. the string was changed
!C***********************************************************************

        character*(*) line
        integer I

        ToLowerCase = .false.
        do I=1,LEN_TRIM(line)
                if((line(I:I) .GE. 'A') .AND. (line(I:I) .LE. 'Z'))then
                        line(I:I) = CHAR(ICHAR(line(I:I)) + 32)
                        ToLowerCase = .true.
                end if
        end do

        return
        end FUNCTION
