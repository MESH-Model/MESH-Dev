C    This file is part of WATROUTE.
C
C    WATROUTE is free software: you can redistribute it and/or modify
C    it under the terms of the GNU Lesser General Public License as published by
C    the Free Software Foundation, either version 3 of the License, or
C    (at your option) any later version.
C
C    WATROUTE is distributed in the hope that it will be useful,
C    but WITHOUT ANY WARRANTY; without even the implied warranty of
C    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C    GNU Lesser General Public License for more details.
C
C    You should have received a copy of the GNU Lesser General Public License
C    along with WATROUTE.  If not, see <http://www.gnu.org/licenses/>.

!***********************************************************************
!       copyright (c) by Nick Kouwen and Dave Watson 2007
!***********************************************************************

      SUBROUTINE read_flowinit_ef()
	      
!     borrowed swe reader for this s/r
!     nk  Oct. 9/06

C***********************************************************************
C  based on:  READ_SWE_EF -  written Mar/06 by Dave Watson, CHC
C     - Derived from rdswe written by Nick Kouwen
C     - This subroutine reads the ensim compatible gridded SWE file (r2c format)
C***********************************************************************

!     rev.   Sep. 2008  - Vincent Fortin:  print FLOWINIT CONTENT
   
c      USE area1
c      USE area2
c      USE area3
c      USE area6
c      USE area12
c      USE areawfo
c      USE areamelt
c      USE area_rte

      use area_watflood

C R2C data module
	USE EF_Module

	implicit none
	TYPE(SWEParam) :: header

C SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

cx      integer       :: nclasses,ios,i,j,ii,n
cx      real*4        :: deffactor,p(100,100)
      integer       :: nclasses,ios
      real*4        :: deffactor

      LOGICAL exists
 
C parameter type definitions
	integer*4 unitNum, flnNum, iStat
	integer*4 n,i,j

C Local variables
	character*1024 line, subString, tmpString
	character*128 keyword, value
	integer lineLen, keyLen, wordCount
	logical rStat, lineType, foundEndHeader
	integer xCountLoc, yCountLoc, attCountLoc
	integer ai, xi, yi, vi, error

C Set unit and fln number
	unitNum = 99
	flnNum = 99
 
C Open the file
	INQUIRE(FILE=fln(flnNum),EXIST=exists)
	IF(exists)then
		open(unit=unitNum,file=fln(flnNum),status='old',iostat=ios)
 		if(ios.ne.0)then
			print*,'Problems opening ',fln(flnNum)
			print*
			STOP ' Stopped in read_swe_ef'
		endif
	else
		print*,'ERROR: the SWE file: ',	fln(flnNum)
		print*,'is NOT found'
		STOP ' Program STOPPED in read_swe_ef'
	endif

C Initialize default values
	CALL InitSWEParam(header)	

C Search for and read r2c file header
	line(1:1) = '#'
	foundEndHeader = .false.

	do WHILE((.NOT.foundEndHeader) .AND.
     &	    ((line(1:1) .eq. '#') .OR.
     &		(line(1:1) .eq. ':') .OR.
     &		(LEN_TRIM(line) .eq. 0))) 	

		read(UNIT=unitNum, FMT='((A))', iostat=ios) line	! read a line
		if(ios .eq. -1)then
			write(6,'((A))') 'ERROR: Premature EndOfFile encountered'
			STOP ' Stopped in read_swe_ef'
		end if

		rStat = Detab(line)				! replace tabs with spaces
		line = ADJUSTL(line)		! Get rid of leading white space
		lineLen = LEN_TRIM(line)		! Find the length excluding trailing spaces

		if(line(1:1) .eq. ':')then
			wordCount = SplitLine(line, keyword, subString)	! find the keyword
			rStat = ToLowerCase(keyword)
			KeyLen = LEN_TRIM(keyword)

			if(keyword(1:KeyLen) .eq. ':endheader')then
				foundEndHeader = .TRUE.

			else
				iStat = ParseSWEParam(header,keyword,keyLen,
     &													subString)
				if(iStat .lt. 0) then
					write(*,'(2(A))') 'ERROR parsing ', fln(flnNum)
					write(*,'(2(A))') '   in line: ',line					
					STOP ' Stopped in read_swe_ef'
					return
				else if(iStat .eq. 0) then
C					write(*,'((A), (A))')  'Unrecognized keyword line: ',
C     &										line
				endif
			end if
		end if
	end do
C***************************************
C	Finished reading the header
C***************************************

C Assign the parsed parameters to the model variables		
	xCountLoc = header%r2cp%xCount
	yCountLoc = header%r2cp%yCount
	attCountLoc =header%r2cp%ep%attCount
	imax=header%r2cp%yCount
	jmax=header%r2cp%xCount 
	deffactor = header%initHeatDeficit !Set InitHeatDeficit

C Read the data section
	CALL LoadAttributeData(header%r2cp%ep, xCountLoc,
     &						yCountLoc, unitNum)
      
c      print*,xCountLoc,yCountLoc,unitNum
            	
C Validate parameters
	if(header%r2cp%xOrigin.ne.xorigin)print*,'xorig_rte.ne.xorigin'
	if(header%r2cp%yOrigin.ne.yorigin)print*,'yorig_rte.ne.yorigin'
        if(header%r2cp%xDelta.ne.xdelta)print*,'xdelta_rte.ne.xdelta'
	if(header%r2cp%yDelta.ne.ydelta)print*,'ydelta_rte.ne.ydelta'
        if(header%r2cp%xCount.ne.xcount)print*,'xcount_rte.ne.xcount'
        if(header%r2cp%yCount.ne.ycount)print*,'ycount_rte.ne.ycount'
        if(header%r2cp%xOrigin.ne.xorigin.or.
     &	header%r2cp%yOrigin.ne.yorigin.or.
     &	header%r2cp%xDelta.ne.xdelta.or.
     &	header%r2cp%yDelta.ne.ydelta.or.
     &	header%r2cp%xCount.ne.xcount.or.
     &	header%r2cp%yCount.ne.ycount) then
            print*
	    PRINT*,'Mismatch between ',fln(flnNum)
            print*,'    and SHD files'
            print*,'Check files for origins, deltas and counts'
            print*,'Could be due to # significant digits in header' 
	    STOP 'Program aborted in read_flowinit_ef @ 159'
	endif

	nclasses = header%r2cp%ep%attCount

        imax=ycount
	jmax=xcount

C Copy Attribute data over to Nick's global variables
	do ai=1,attCountLoc
	   vi = 0
c	    do yi=yCountLoc,1,-1      <<<<<<<top to bottom reversed !!!
	   do yi=1,yCountLoc
	     do xi=1,xCountLoc
	        vi = vi+1
		p(yi,xi) = header%r2cp%ep%attList(ai)%val(vi)
c	print*,yi,xi,header%r2cp%ep%attList(ai)%val(vi)
	     end do
             write(55,55555)(p(yi,xi),xi=1,xCountLoc)
	   end do
	   write(55,*)
55555      format(999e10.3)
	   do n=1,naa
	        i=yyy(n)
	        j=xxx(n)
	        if(ai.eq.1)then
                  qi1(n)=p(i,j)
	          qi2(n)=p(i,j)
c       print*,n,qi1(n)
	        elseif(ai.eq.2)then
	            qo1(n)=p(i,j)
	            qo2(n)=p(i,j)
c       print*,n,qo2(n)
      	        elseif(ai.eq.3)then
	            store1(n)=p(i,j)
	            store2(n)=p(i,j)
c       print*,n,store2(n)
	        elseif(ai.eq.4)then
	            over(n)=p(i,j)
c       print*,n,over(n)
	        elseif(ai.eq.5)then
	            lzs(n)=p(i,j)
	        endif
           end do
        end do

c vfo 
c       print*,"FLOWINIT CONTENT rank 18:"
c     1 ,qi1(18),qo1(18),store1(18),over(18),lzs(18) 
c vfo

C Deallocate the attribute data now that global attributes have been set
	do ai=1,attCountLoc
		deallocate ( header%r2cp%ep%attList(ai)%val, STAT = error )
		if (error.ne.0) STOP 'deallocation error in read_gsm_ef()' 
	end do

      close (unit=unitNum)

      RETURN

      END SUBROUTINE read_flowinit_ef



