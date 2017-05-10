c real start of program
          program watclass_data_conversion

c    This program takes data from a pseudo WATFLOOD format (ie. gridded, ASCII)
c    and coverts it to a binary file to be read by WATCLASS (ie. gridded,binary)
c
c    version 3.0, by Frank Seglenieks
c
c        version 1.0 - October 2000
c        version 2.0 - January 2001 - other ways to write data
c        version 3.0 - October 2001 - now can change time step of output file
c
      integer no_hours,no_grus,no_grid
      parameter (no_hours=2000,no_grus=4700,no_grid=100)


      integer ijunk,nch,i,j,n,o,hour,gru,imax,jmax,val
      integer event,rhour,hourstep

      integer yy(no_grus),xx(no_grus)
      real radcl(no_grid,no_grid,no_hours)
      real addradcl(no_grid,no_grid)
      real tempr(no_grid,no_grid,no_hours)
      real humid(no_grid,no_grid,no_hours)
      real wndsp(no_grid,no_grid,no_hours)
      real longw(no_grid,no_grid,no_hours)
      real shrtw(no_grid,no_grid,no_hours)
      real press(no_grid,no_grid,no_hours)
      real oldtempr(no_grid,no_grid)
      real oldhumid(no_grid,no_grid)
      real oldwndsp(no_grid,no_grid)
      real oldlongw(no_grid,no_grid)
      real oldshrtw(no_grid,no_grid)
      real oldpress(no_grid,no_grid)
      real junk(no_grid,no_grid)
      real value(no_grid,no_grid)

      character fln(600)*30,cjunk*30
      character*80 tag
      logical   equals
c ******************************
c get time step for the hour
c ******************************

      print *, 'What do you want to the hourly time step to be?'
	print *, '(recommended values: 1,2,3,4,6 or 12)'
	read *, tempstep

	if(tempstep.eq.1.or.tempstep.eq.2.or.tempstep.eq.3.or.tempstep.
     +eq.4.or.tempstep.eq.6.or.tempstep.eq.12) then
      hourstep=tempstep
	else
	hourstep=1
	endif

c ******************************
c set up arrays
c ******************************

c precipitation  data - radcl(hour, GRU)
c temperature    data - tempr(hour, GRU)
c humidity       data - humid(hour, GRU)
c wind speed     data - wndsp(hour, GRU)
c long wave rad  data - longw(hour, GRU)
c short wave rad data - shrtw(hour, GRU)
c pressure       data - press(hour, GRU)


c ******************************
c read in event file
c ******************************

c open the event file:
      open(unit=99,file='event/event.evt',status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening event.evt file'
      stop
      endif

c input event particulars
      read(99,*) 
      read(99,1200) ijunk,nch
	


c force nch to have a minimum value of 1
      if(nch.eq.0) then
      print *, 'This is a single event'
      nch=1
      else
      print *, 'There are the following number of chained events: ',nch
      endif


      if(nch.ge.2)then
c         read the second and subsequent event names to modelled:
          do n=2,nch
            read(99,1300)fln(100+n)
          enddo
      endif


c     read in data file names (doesn't matter if chained or not)
      read(99,*)
      read(99,*)
      read(99,*)
      read(99,*)

c     use only the first value:
      
        i=0
        do while (ios.eq.0)
          i=i+1
          read(99,1000,iostat=ios) fln(i)
        end do

      close(99)

1000  format(26x,a30)
c1000  format(27x,a30)
1200  format(27x,3i5)
1300  format(a30)

c *****************************************************
c read in *.shd file to get number and position of GRUs
c *****************************************************

c read in data from shed file to get values to translate grid data to GRUs
      open(31, file=fln(1),status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening shed file file'
      stop
      endif

c read shed file until EOF 
	do while (.true.)
		read (31,'(A)',end=999) tag
		if (INDEX(tag,'xCount').gt.0) then
			read (tag(8:80),'(i72)') jmax
			print *,"xCount==",jmax
		endif		
		if (INDEX(tag,'yCount').gt.0) then
			read (tag(8:80),'(i72)') imax
			print *,"yCount==",imax
		endif		
	enddo

 999  close(31)

 2000 format(5x,2i5,3f10.5,f7.0,5i5,17f5.2)
 2001 format(160i4)
 2005 format(12i5,2f5.0)
 2006 format(1x,a,1x,1i10)


      
c *****************************************************
c set up loop to go through events and get filenames 
c *****************************************************

c  open output file
c      open(15,file='watclass.out',status='unknown')
c      open(15,file='watclass.bin',action='write',form='unformatted', 
c     +access='direct',recl=imax*jmax*4)

c      open(15,file='s:\spl\mackclass\watclass.bin',action='write',
c     +form='unformatted',access='sequential')

c  This is the real one
      open(15,file='watclass.bin',action='write',form='unformatted',
     +access='sequential')


      open(16,file='wat_debug.txt')
      
c start of BIG event loop
      do event = 1, nch

      if (nch.gt.1) then
        if (event.eq.1) then
          print *, 'Working on first event'
        else
          print *
          print *, 'Working on event: ',fln(100+event)
        endif
        else
        print *, 'Working on single event'
      endif


c     if first event don't read in filename (they are already there)

      if (event.gt.1) then
        open(99,file=fln(100+event),status='old',iostat=ios)
        if(ios.ne.0) then
          print *, 'error opening event file'
          print *, 'event= ',event
          stop
        endif

        do i=1,6
          read(99,*) cjunk
      continue
        enddo

        i=0        
        do while (ios.eq.0)
          i=i+1
          read(99,3000,iostat=ios) fln(i)
        end do
        close(99)
      endif

 3000  format(26x,a30)


c *****************************************************
c read in data from streamflow (strfw) files to get total number of hours of simulation
c *****************************************************

      open (31,file=fln(6),status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening streamflow file'
      stop
      endif

      print *, 'reading from streamflow file: ',fln(6)

      read (31,*)
      read (31,3507) ijunk,hour
 3507 format(2i5)

      close(31)


c *****************************************************
c read in precipitation data and put into proper array
c *****************************************************

      open(31, file=fln(10),status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening precip file'
      print *, 'hour= ',hour,' event= ',event
      stop
      endif

      print *, 'reading from precipitation file: ',fln(10)


      do i=1,6
      read(31,*)
      enddo

      do n=1,hour
      if(rhour.ne.-999) then
        read(31,4018)rhour
      endif

        if(rhour.eq.0)then
            do j=jmax,1,-1
               read(31,4012)(junk(i,j),i=1,imax)
            end do
          read(31,4018)rhour
        endif 

        if(rhour.gt.0) then
          do j=jmax,1,-1
c             read(31,4012)(value(i,j),i=1,imax)
             read(31,4012)(radcl(i,j,n),i=1,imax)
          end do
        else
          do j=jmax,1,-1
          do i=1,imax
             radcl(i,j,n)=0.0
          enddo
          end do
        endif
        
c end of hour loop
      enddo 
      
      close(31)
           
 4012 format(160f5.1)
 4018 format(6x,2i5,5x,a5)

c now look at the precip file with relation to the time step
c in WATCLASS with a 3 hour time step it assumes that the 3 hour precip falls
c every hour of the run, hence you have to divide the precip by the number of hours

      do n=1,int(real(hour)/real(hourstep))

c do loop for number of hours in each timestep

          do j=jmax,1,-1
           do i=1,imax
             addradcl(i,j)=0.0
           enddo
	    enddo

        do k=(n-1)*hourstep+1, n*hourstep

          do j=jmax,1,-1
           do i=1,imax
             addradcl(i,j)=addradcl(i,j)+radcl(i,j,k)
           enddo
	    enddo

        enddo

          do j=jmax,1,-1
           do i=1,imax
             radcl(i,j,(n-1)*hourstep+1)=addradcl(i,j)/real(hourstep)
           enddo
	    enddo
        
c end of hour loop
      enddo 


c *****************************************************
c read in temperature data and put into proper array
c *****************************************************

      open(31, file=fln(15),status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening temperature file'
      print *, 'hour= ',hour,' event= ',event
      stop
      endif

      print *, 'reading from temperature file: ',fln(15)

      do i=1,2
      read(31,*)
      enddo

      do n=1,hour
        read(31,4518)rhour
        if(rhour.gt.0) then
          do j=jmax,1,-1
c             read(31,4512)(value(i,j),i=1,imax)
             read(31,4512)(tempr(i,j,n),i=1,imax)
          end do

          do j=jmax,1,-1
          do i=1,imax
         oldtempr(i,j)=tempr(i,j,n)
          enddo
	    enddo

        else
          do j=jmax,1,-1
          do i=1,imax
             tempr(i,j,n)=oldtempr(i,j)
          enddo
          end do
        endif
        
c end of hour loop
      enddo 
      
      close(31)
           
 4512 format(160f5.1)
 4518 format(6x,2i5,5x,a5)

c *****************************************************
c read in humidity data and put into proper array
c *****************************************************

      open(31, file=fln(21),status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening humidity file'
      print *, 'hour= ',hour,' event= ',event
      stop
      endif

      print *, 'reading from humidity file: ',fln(21)

      do i=1,6
      read(31,*)
      enddo

      do n=1,hour
        read(31,5018)rhour

        if(rhour.gt.0) then
          do j=jmax,1,-1
c             read(31,5012)(value(i,j),i=1,imax)
             read(31,5012)(humid(i,j,n),i=1,imax)
          end do

          do j=jmax,1,-1
          do i=1,imax
         oldhumid(i,j)=humid(i,j,n)
          enddo
	    enddo

        else
          do j=jmax,1,-1
          do i=1,imax
             humid(i,j,n)=oldhumid(i,j)
          enddo
          end do
        endif
         
c end of hour loop
      enddo 
      
      close(31)
           
 5012 format(160es12.4)
 5018 format(6x,2i5,5x,a5)


c *****************************************************
c read in wind speed data and put into proper array
c *****************************************************

      open(31, file=fln(22),status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening wind speed file'
      print *, 'hour= ',hour,' event= ',event
      stop
      endif

      print *, 'reading from wind speed file: ',fln(22)

      do i=1,6
      read(31,*)
      enddo

      do n=1,hour
        read(31,5518)rhour

        if(rhour.gt.0) then
          do j=jmax,1,-1
c             read(31,5512)(value(i,j),i=1,imax)
             read(31,5512)(wndsp(i,j,n),i=1,imax)
          end do

          do j=jmax,1,-1
          do i=1,imax
         oldwndsp(i,j)=wndsp(i,j,n)
          enddo
	    enddo

        else
          do j=jmax,1,-1
          do i=1,imax
             wndsp(i,j,n)=oldwndsp(i,j)
          enddo
          end do
        endif
        
c end of hour loop
      enddo 
      
      close(31)
           
 5512 format(160es12.4)
 5518 format(6x,2i5,5x,a5)


c *****************************************************
c read in longwave radiation data and put into proper array
c *****************************************************

      open(31, file=fln(23),status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening longwave radiation file'
      print *, 'hour= ',hour,' event= ',event
      stop
      endif

      print *, 'reading from longwave radiation file: ',fln(23)

      do i=1,6
      read(31,*)
      enddo

      do n=1,hour
        read(31,6018)rhour

        if(rhour.gt.0) then
          do j=jmax,1,-1
c             read(31,6012)(value(i,j),i=1,imax)
             read(31,6012)(longw(i,j,n),i=1,imax)
          end do

          do j=jmax,1,-1
          do i=1,imax
         oldlongw(i,j)=longw(i,j,n)
          enddo
	    enddo

        else
          do j=jmax,1,-1
          do i=1,imax
             longw(i,j,n)=oldlongw(i,j)
          enddo
          end do
        endif
         
c end of hour loop
      enddo 
      
      close(31)
           
 6012 format(160es12.4)
 6018 format(6x,2i5,5x,a5)


c *****************************************************
c read in shortwave radiation data and put into proper array
c *****************************************************

      open(31, file=fln(24),status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening shortwave radiation file'
      print *, 'hour= ',hour,' event= ',event
      stop
      endif

      print *, 'reading from shortwave radiation file: ',fln(24)

      do i=1,6
      read(31,*)
      enddo

      do n=1,hour
          read(31,6518)rhour

        if(rhour.gt.0) then
          do j=jmax,1,-1
c             read(31,6512)(value(i,j),i=1,imax)
             read(31,6512)(shrtw(i,j,n),i=1,imax)
          end do

          do j=jmax,1,-1
          do i=1,imax
         oldshrtw(i,j)=shrtw(i,j,n)
          enddo
	    enddo

        else
          do j=jmax,1,-1
          do i=1,imax
c zero out the missing shortwave data for Nathalie
c             shrtw(i,j,n)=0.0
             shrtw(i,j,n)=oldshrtw(i,j)
          enddo
          end do
        endif
         
c end of hour loop
      enddo 

      
      close(31)
           
 6512 format(160es12.4)
 6518 format(6x,2i5,5x,a5)


c *****************************************************
c read in pressure data and put into proper array
c *****************************************************

      open(31, file=fln(25),status='old',iostat=ios)
      if(ios.ne.0) then
      print *, 'error opening absolute pressure file'
      print *, 'hour= ',hour,' event= ',event
      stop
      endif

      print *, 'reading from pressure file: ',fln(25)

      do i=1,6
      read(31,*)
      enddo

      do n=1,hour
	  read(31,6518)rhour

        if(rhour.gt.0) then
          do j=jmax,1,-1
c             read(31,6512)(value(i,j),i=1,imax)
             read(31,6512)(press(i,j,n),i=1,imax)
          end do

          do j=jmax,1,-1
          do i=1,imax
         oldpress(i,j)=press(i,j,n)
          enddo
	    enddo

        else
          do j=jmax,1,-1
          do i=1,imax
             press(i,j,n)=oldpress(i,j)
          enddo
          end do
        endif
         
c end of hour loop
      enddo 
      
      close(31)
           
 7012 format(160es12.4)
 7018 format(6x,2i5,5x,a5)



c *****************************************************
c write out final file from array
c *****************************************************


c write it out in vector form

c      do n=1, hour
c	do o=1,GRU

c	write out all parameters
c      write(15,*) press(n,o)
c	enddo
c      write(15,*)
c	enddo

      print *, 'Printing out WATCLASS file for this event'


c write it out in grid format
      do n=1, hour, hourstep !hour
             write(15)((shrtw(i,j,n),i=1,imax),j=1,jmax)
             write(15)((longw(i,j,n),i=1,imax),j=1,jmax)
c When using GEM data
             write(15)((radcl(i,j,n)/3600.0,i=1,imax),j=1,jmax)
             write(15)((tempr(i,j,n)+273.16,i=1,imax),j=1,jmax)
c When using station data
c             write(15)((radcl(i,j,n),i=1,imax),j=1,jmax)
c             write(15)((tempr(i,j,n),i=1,imax),j=1,jmax)
             write(15)((wndsp(i,j,n),i=1,imax),j=1,jmax)
             write(15)((press(i,j,n),i=1,imax),j=1,jmax)
             write(15)((humid(i,j,n),i=1,imax),j=1,jmax)
      enddo


c end of big event loop
      enddo

c close output file
      close(15)
      close(16)

      stop
	end







