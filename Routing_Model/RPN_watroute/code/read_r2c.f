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

      SUBROUTINE read_r2c(unitNum,flnNum,hdrflg)

C***********************************************************************
C  read_r2c - written May/06 by Dave Watson
C       - Adapted for general r2c read by NK  Jul. 24/06
C	- Derived from rdtemp written by Nick Kouwen
C	- This subroutine reads the ensim compatible gridded r2c format
C***********************************************************************

!     rev. Sep. 2008 - Vincent Fortin/Isabelle Dore
!     EOF by ios line 194 and line 422

      use area_watflood

C R2C data module
	use EF_Module
	implicit none
	type(TempParam) :: header
	type(FrameRecord) :: frameRec


!     SAVES THE LOCAL VARIABLES FROM ONE RUN TO NEXT
      SAVE

      DIMENSION :: ntoc(9000)

      real*4  :: x1,deltatmp,teltatmp
      integer :: idlast,mhlast,jan,ntocorrect,ios,ios1,
     *           ntoc,i,iend,j,jz,n,
     *           ndeltatt,tem_hr,iostat,iAllocate,iDeallocate,
     *           nh_count,un,fn,xcount_local,ycount_local,
     *           nolines,xcount_max,ycount_max
      character(10) ::  fileformat
      real*4 timeHrs

      character*20  :: junk
      character*1   :: junk2,newfmtflg,firstpass,lineflg,hdrflg
      character*6   :: junk0
      DATA idlast/0/
      data firstpass/'y'/
      LOGICAL      :: exists
     
C Local variables
	integer*4 unitNum, flnNum, iStat
	character*1024 line, subString
	character*128 keyword, value 
	integer lineLen, keyLen, wordCount
	logical rStat, foundEndHeader,foundFirstFrame,foundFrame
	integer frameCount

	integer modelHour !used to be k
	integer modelHourLast !used to be klast
	integer dataHour !used to be nr_count

C Dave's debug variables
C	integer daveInt
C	real daveReal

C Initialize default values within frame module
	CALL InitFrameRecord(frameRec)

	foundEndHeader = .false.

C Debug line
C	print*, 'Inside read_r2c at time = ',timeHrs, 'hrs'

C If hdrflg==1 ,then it's the first time through for this event
	if(hdrflg.eq.'1')then
		inquire(FILE=trim(adjustl(fln(flnNum))),EXIST=exists)
		if(exists) then
		  open(unit=unitNum,file=trim(adjustl(fln(flnNum))),
     *              status='old',iostat=ios)
	print*,'Opened unit',unitNum,' filename  ',fln(flnNum)
			if(ios.ne.0)then
				print*, 'Problems opening the tem file',fln(flnNum)
				STOP ' Stopped in read_r2c'
			endif
		else
			print*, 'Attempting to open the met file ',fln(flnNum)
			print*, 'but it is not found (in this location)'
			STOP 'Program aborted in read_r2c @89'
		endif

! Dave:  this needs to be fixed for this s/r  NK

C Initialize default values within XXXXXXXX module
		call InitTempParam(header)	

C Search for and read r2c file header
		line(1:1) = '#'
		do while((.NOT.foundEndHeader) .AND.
     &	        ((line(1:1) .eq. '#') .OR.
     &		(line(1:1) .eq. ':') .OR.
     &		(len_trim(line) .eq. 0))) 	

			read(unit=unitNum, FMT='((A))', iostat=ios) line	! read a line
			if(ios .eq. -1)then
			   print*, 'ERROR: Premature EndOfFile encountered line 119' !ido line 119 added
			   STOP ' Stopped in read_r2c'
			end if
c                        if(iopt.eq.2)print*,line
			rStat = Detab(line)				! replace tabs with spaces
			line = adjustl(line)		! Get rid of leading white space
			lineLen = len_trim(line)		! Find the length excluding trailing spaces

			if(line(1:1) .eq. ':')then
				wordCount = SplitLine(line, keyword, subString)	! find the keyword
				rStat = ToLowerCase(keyword)
				KeyLen = len_trim(keyword)

				if(keyword(1:KeyLen) .eq. ':endheader')then
					foundEndHeader = .TRUE.
				else
					! parse the header
					iStat = ParseTempParam(header,keyword,keyLen,
     &													subString)
					if(iStat .lt. 0) then
						write(*,'(2(A))') 'ERROR parsing ', fln(flnNum)
						write(*,'(2(A))') '   in line: ',line					
						STOP ' Stopped in read_r2c'
						return
					else if(iStat .eq. 0) then
C						write(*,'((A), (A))')
C     &								'Unrecognized keyword line: ',line
					endif
				end if
			end if
		end do

C Assign the parsed parameters to the model variables		
		xcount_temp = header%r2cp%xCount
		ycount_temp = header%r2cp%yCount
		xorigin_temp = header%r2cp%xOrigin
		yorigin_temp = header%r2cp%yOrigin
		xdelta_temp = header%r2cp%xDelta
		ydelta_temp = header%r2cp%yDelta
		
!     This section copied from read_swe_ef by NK  Jul. 5/07
C Validate parameters
c        print*,header%r2cp%xOrigin,xorigin     !ido
c        print*,header%r2cp%yOrigin,yorigin     !ido
c        print*,header%r2cp%xDelta,xdelta       !ido
c        print*,header%r2cp%yDelta,ydelta       !ido
c        print*,header%r2cp%xCount,xcount       !ido
c        print*,header%r2cp%yCount,ycount       !ido
	if(header%r2cp%xOrigin.ne.xorigin)print*,'xorig_swe.ne.xorigin'
	if(header%r2cp%yOrigin.ne.yorigin)print*,'yorig_swe.ne.yorigin'
        if(header%r2cp%xDelta.ne.xdelta)print*,'xdelta_swe.ne.xdelta'
	if(header%r2cp%yDelta.ne.ydelta)print*,'ydelta_swe.ne.ydelta'
        if(header%r2cp%xCount.ne.xcount)print*,'xcount_swe.ne.xcount'
        if(header%r2cp%yCount.ne.ycount)print*,'ycount_swe.ne.ycount'
        if(header%r2cp%xOrigin.ne.xorigin.or.
     &	header%r2cp%yOrigin.ne.yorigin.or.
     &	header%r2cp%xDelta.ne.xdelta.or.
     &	header%r2cp%yDelta.ne.ydelta.or.
     &	header%r2cp%xCount.ne.xcount.or.
     &	header%r2cp%yCount.ne.ycount) then
            print*
	    print*,'Mismatch between ',fln(flnNum)
            print*,'    and SHD files'
            print*,'Check files for origins, deltas and counts'
            print*,'Could be due to # significant digits in header' 
		STOP 'Program aborted in read_r2c_ef @ 158'
	endif

c Scan data frames
	       frameCount = 0
c                do while((.NOT.EOF(unitNum)))  !ido
               ios=0                           !ido
	        do while(ios.eq.0)	       !ido
			read(unit=unitNum, FMT='((A))', iostat=ios) line	! read a line
c      write(654,*)line
			if(ios.gt.0)then      !ido
			 write(6,'((A))') 'ERROR: Premature EndOfFile encountered line 195' !ido line 195 added
				STOP ' Stopped in read_r2c'
                        elseif(ios.eq.0)then  !ido      
c      if(iopt.eq.2)print*,line
			      rStat = Detab(line)		! replace tabs with spaces
			      line = adjustl(line)		! Get rid of leading white space
			      lineLen = len_trim(line)		! Find the length excluding trailing spaces

			if(line(1:1) .eq. ':')then
				wordCount = SplitLine(line, keyword, subString)	! find the keyword
				rStat = ToLowerCase(keyword)
				KeyLen = len_trim(keyword)
				
				if(keyword(1:KeyLen).eq.':frame')then
					iStat = ParseFrameLine(frameRec,keyword,keyLen,
     &													subString)
					
C	Identify the first and last frame's timestamp 
					if(iStat .lt. 0) then
					   write(*,'(2(A))') 'ERROR parsing ', fln(flnNum)
					   write(*,'(2(A))') '   in line: ',line					
					   STOP ' Stopped in read_r2c'
					   return
					else if(iStat .eq. 0) then
C						write(*,'((A), (A))')
C     &								'Unrecognized keyword line: ', line
					else if(frameRec%frame.EQ.1) then
						header%startJulianDay =
     &						JDATE(frameRec%tStamp%year,
     &							frameRec%tStamp%month,
     &							frameRec%tStamp%day)
						header%startHour = frameRec%tStamp%hour
					else
						header%endJulianDay =
     &						JDATE(frameRec%tStamp%year,
     &							frameRec%tStamp%month,
     &							frameRec%tStamp%day)
						header%endHour = frameRec%tStamp%hour
					end if
					header%r2cp%frameCount = header%r2cp%frameCount+1
				end if
			end if
                      end if
		enddo	
	
C	deltat2 = model timestep in hours 
		deltat2 = 1	
C	nr = number of hours spanned by this tem file
C	convert to julian days to properly calculate hours spanned
		nr = (header%endJulianDay - header%startJulianDay)*24
     &				 + (header%endHour - header%startHour) + 1

c      pause 20		
C	Position to start of data (immediately after first frame record but before first frame data)
		REWIND (unitNum)
		foundFirstFrame = .false.
		do WHILE(.NOT.foundFirstFrame)
			read(unit=unitNum, FMT='((A))', iostat=ios) line	! read a line
			if(line(1:1) .eq. ':')then
				wordCount = SplitLine(line, keyword, subString)	! find the keyword
				rStat = ToLowerCase(keyword)
				KeyLen = len_trim(keyword)
				if(keyword(1:KeyLen).eq.':frame')then
					iStat = ParseFrameLine(frameRec,keyword,
     &							keyLen,subString)
					foundFirstFrame = .true.
				end if
			end if		      
		enddo	
		
		modelHour=1 ! set model hour to first hour
		dataHour = 0 ! set data hour to just before first hour

c      pause 30
C firstpass means this is the first tem file of the first event...which is the only time it needs to be done
		if(firstpass.eq.'y')then
		  firstpass='n'        
c		  allocate(ttemp(ycount3,xcount3),tempv(na),tempvmin(na),
c     *						rh(na),stat=iAllocate)
              if(.NOT.allocated(inarray))then    
!               inarray not previously allocated
		    allocate(inarray(ycount_temp,xcount_temp),
     *                                         stat=iAllocate)
			if(iAllocate.ne.0) STOP
     *	   'Error with allocation in read_r2c'
			xcount_max=xcount_temp
			ycount_max=ycount_temp
			xcount_local=xcount_temp
			ycount_local=ycount_temp
                  
              else
!               inarray previously allocated but check to see this 
!               data fits in the allocated memory
 		    if(xcount_temp.gt.xcount_max.or.ycount_temp.gt.ycount_max)then
		      deallocate(inarray,stat=iDeallocate)
			if(iDeallocate.ne.0)then
			  print*,'Warning: error with deallocation of inarray'
			endif

			xcount_max=xcount_temp
			ycount_max=ycount_temp
			xcount_local=xcount_temp
			ycount_local=ycount_temp
C       outarray is in areawfo
C       this has to be allocated before calling write_r2c
		      allocate(inarray(ycount_max,xcount_max),
     *                             stat=iAllocate)
			if(iAllocate.ne.0)then
			  STOP 'Error with allocation of inarray in read_qlz'      
			end if
	          endif
              endif
		endif
 
C Nick initializes stuff here...may want to move this to process_temp()
c		do n=1,na
c			tempvmin(n)=99.99
c			rh(n)=.50
c		enddo

c		if(jan.eq.1)then     
C       CALCULATE THE GRID OFFSETS:
c			if(iymin.ge.rads.and.jxmin.ge.radw)then
C         COMPARE THE SOUTH-WEST CORNER COORDINATES
c				if(IsLatLong(header%r2cp%csp).eq.(.true.))then
c					iyoffset=int(float(iymin-rads)/rgrdn)+iyshift
c					jxoffset=int(float(jxmin-radw)/rgrde)+jxshift
c				else
c					iyoffset=int(float(iymin-rads)/grdn)+iyshift
c					jxoffset=int(float(jxmin-radw)/grde)+jxshift
c				endif
c			elseif(iymin.lt.rads.or.jxmin.lt.radw)then
!         WATERSHED GRID IS NOT WITHIN TEM GRID
c				print*
c				print*,' In read_r2c:'
c				print*,' Watershed outline is not within tem grid'
c				print*,'iymin,jxmin,rads,radw/',iymin,jxmin,rads,radw
c				print*                     
c				STOP 'Program aborted in read_r2c'
c			else
c				iyoffset=0
c				jxoffset=0
c				! DO NOTHING
c			endif
c
c			call precip_adjust()
c		endif   ! END OF JAN=1

! Check for change of grid size (could happen for next event)
            if(allocated(inarray))then    ! check that inarray is allocated
		  if(xcount_temp.gt.xcount_max.
     *                          or.ycount_temp.gt.ycount_max)then
			deallocate(inarray,stat=iDeallocate)
			if(iDeallocate.ne.0) then
			  print*,'Warning: error with deallocation of intarray'
			endif

			xcount_max=xcount_temp
			ycount_max=ycount_temp
C       outarray is in areawfo
C       this has to be allocated before calling write_r2c
		  allocate(inarray(ycount_max,xcount_max),stat=iAllocate)
			if(iAllocate.ne.0) then
			STOP 'Error with allocation of inarray in read_r2c'      
			end if
		  endif
	      endif

		xcount_temp=xcount_local
		ycount_temp=ycount_local

		return
      endif          ! (hdrflg.eq.'1')

      if(iopt.eq.2)print*,'Finished reading the header      '

C***********************************************************
C***********************************************************
	
C     Finished reading the header      

C***********************************************************
C***********************************************************

C     We are looking for temp data for this step
	dataHour = dataHour + 1

c	if(modelHour.eq.modelHourLast)then
c		print*,'Error in ',fln(flnNum),' data repeated??'
c	endif    
 
C	If there is a record for this hour than modelHour = dataHour
C	print*,	'modelHour:',modelHour, 'dataHour:',dataHour
c	if(modelHour.eq.dataHour)then
C	Go ahead and read the data for this frame 

c      print*

		do i=1, ycount_local
		  read(unitNum,*,iostat=ios)(inarray(i,j),j=1,xcount_local)
c	        write(*,*)(inarray(i,j),j=1,xcount_local)
		  if(ios.ne.0)then
		    write(*,9993)modelHour,i
	            print*,' unit number =',unitNum
	            print*,' filename =',fln(flnNum)
		    print*,' last data read:'
		    print*,' ycount_local = ',i
		    print*,' xcount_local = ',j
	            backspace unitNum
	            read(unitNum,*)line
	            print*,line
			STOP ' program aborted - read_r2c @ 363'
		  endif
		end do

C	Read the next frame. If we are at the end of the file...close
		
        foundFrame = .false.
	do WHILE(.NOT.foundFrame)
	  read(unit=unitNum, FMT='((A))', iostat=ios) line	! read a line
c      print*,line

c	  if(EOF(unitNum)) then !ido			
	 if(ios.ne.0) then     !ido

!     NK  Oct. 29/06
!     Added global flag to end event
!     end of event is when eof file found in first file
!     other files may have more data but we can only run shortest

	   found_data_end=.true.

           close(unit=unitNum,status='keep',iostat=ios)

           write(51,*)'Closed unit ',unitNum,' Filename=  ',fln(flnNum)
c	   write(*,*) 'Closed unit ',unitNum,' Filename=  ',fln(flnNum)

	   RETURN
	  
         endif

			if(line(1:1) .eq. ':')then
				wordCount = SplitLine(line, keyword, subString)	! find the keyword
				rStat = ToLowerCase(keyword)
				KeyLen = len_trim(keyword)
				if(keyword(1:KeyLen).eq.':frame')then
					iStat = ParseFrameLine(frameRec,keyword,
     &												keyLen,subString)
					foundFrame = .true.
				end if
			end if		      
		enddo	

!           so the grid size can be checked in sub at any time
		xcount_temp=xcount_local
		ycount_temp=ycount_local

		modelHourLast=modelHour

C	Determine the next modelhour		
		modelHour =	(JDATE(frameRec%tStamp%year,frameRec%tStamp%month,
     &				frameRec%tStamp%day) - header%startJulianDay) * 24
     &				+ (frameRec%tStamp%hour - header%startHour) + 1


C	else
C		do nothing...keep last temperature values...

ccccccccccccccc	endif

	RETURN

!     FORMATS

 9993 format(' error reading inarray at hour/line ',2i10/)
	

      END SUBROUTINE read_r2c

