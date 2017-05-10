      subroutine read_grid_parameters()
!**********************************************************************
!*    Reads the MESH grid parameters from the header of the drainage 
!*    database file. The subroutine and the associated functions are 
!*    based on the work by Nick Kouwen and Dave Watson 2007
!***********************************************************************

      use grid_parameters_module

      implicit none

!     Local variables
      integer*4      ios
      character*1024 line, subString, tmpString
      character*128  keyword, value
      integer        lineLen, keyLen, wordCount, ncount
      logical        exists, foundEndHeader, rStat

!     Functions
      integer, external :: splitLine
      logical, external :: Detab, ToLowerCase 

!     Check if the drainage database file exists
      inquire(file='MESH_drainage_database.r2c',exist=exists)
      if(exists)then

!        Open the drainage database file
         open(1,file='MESH_drainage_database.r2c',status='old',&
               iostat=ios)
         if(ios.ne.0)then
            print*,'Could not open the MESH_drainage_database.r2c file'
            stop   'Program aborted in read_grid_parameters, line 24'
         endif
 
      else
         print*,'ERROR: MESH_drainage_database.r2c file not found '
         stop '  Program aborted in read_grid_parameters, line 20'
      endif
  
!     Initialize the search for the end of the header
      foundEndHeader = .false.

!     Initialize the counter for grid parameters
      ncount = 0

!     Search for grid parameters from the r2c file header
      line(1:1) = '#'
      do while((.NOT.foundEndHeader) .AND.((line(1:1) .eq. '#') .OR. &
                (line(1:1) .eq. ':') .OR. (len_trim(line) .eq. 0)))

!        read a line
         read(1, '((A))', iostat=ios) line
         if(ios .eq. -1)then
            print*,'Premature EndOfFile encountered in reading', &
                   'MESH_drainage_database.r2c header'
            stop   'Program aborted in read_grid_parameters, line 44'
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
                  ncount = ncount + 1
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)Projection
                  else
                     print*,'Couldnot read Projection type'
                  endif

!              Ellipsoid - 2nd grid parameter
               elseif(keyword(1:keyLen) .eq. ':ellipsoid')then
                  ncount = ncount + 1
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)Ellipsoid
                  else
                     print*,'Couldnot read Ellipsoid type'
                  endif

!              xOrigin - 3rd grid parameter
               elseif(keyword(1:keyLen) .eq. ':xorigin')then
                  ncount = ncount + 1
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)xOrigin
                  else
                     print*,'Couldnot read xOrigin value'
                  endif

!              yOrigin - 4th grid parameter
               elseif(keyword(1:keyLen) .eq. ':yorigin')then
                  ncount = ncount + 1
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)yOrigin
                  else
                     print*,'Couldnot read yOrigin value'
                  endif

!              xCount - 5th grid parameter
               elseif(keyword(1:keyLen) .eq. ':xcount')then
                  ncount = ncount + 1
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)xCount
                  else
                     print*,'Couldnot read xCount value'
                  endif

!              yCount - 6th grid parameter
               elseif(keyword(1:keyLen) .eq. ':ycount')then
                  ncount = ncount + 1
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)yCount
                  else
                     print*,'Couldnot read yCount value'
                  endif

!              xDelta - 7th grid parameter
               elseif(keyword(1:keyLen) .eq. ':xdelta')then
                  ncount = ncount + 1
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)xDelta
                  else
                     print*,'Couldnot read xDelta value'
                  endif

!              yDelta - 8th grid parameter
               elseif(keyword(1:keyLen) .eq. ':ydelta')then
                  ncount = ncount + 1
                  wordCount = SplitLine(subString, value, tmpString)
                  if(wordCount == 1)then
                     read(value,*)yDelta
                  else
                     print*,'Couldnot read yDelta value'
                  endif
               endif
            endif
         endif
      enddo
      close(1) !MAM
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

