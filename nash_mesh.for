      program nash_mesh
c
c  Program nash_spl.exe written by Frank Seglenieks, starting in 2004
c
c  The program nash_spl.exe will read in an spl.csv file (the main streamflow output file
c  created by WATFLOOD) and create a file called nash.csv.  The program nash_spl.exe should
c  be run in the directory that contains the spl.csv file.  In most cases the program should
c  be able to determine the number of streamflow gauges from the spl.csv file, in the rare 
c  case that it cannot the program will stop.
c
c  This version of the program is limited to 49 streamflow gauges and 20 000 streamflow
c  readings, and it assumes that the spl.csv contains daily streamflow output (ie. each 
c  line represents the daily average streamflow). Please contact the authours if this 
c  is a problem.
c
c  The Nash-Sutcliffe values will be output to file called nash.csv.  There will be two columns
c  in the file, the first is the Nash-Sutcliffe value determined on a monthly basis the second
c  is the Nash-Sutcliffe value based on daily values.  Each row in the nash.csv file represents
c  a different streamflow gauge, the order of these gauges will be the same as in the streamflow
c  file used for the WATFLOOD run.
c
c  For more information on the Nash-Sutcliffe coefficient see this paper:
c  Nash, J. E., and J. V. Sutcliffe, 1970.  River flow forecasting through conceptual models, I, A 
c  discussion of principles, Journal of Hydrology, Vol. 10, pp. 282-290.
c
c Update Oct 2007
c
c For MESH specific runs, the program now checks the MESH_output_echo_print.txt file to make sure that
c the program finished properly
c
c Update May 2008
c
c Add a WATFLOOD flag so the program can be used for an spl.csv file
c Added feature to average the NASH for all of the basins
c Default is now to average all of the basins
c Now will also output total volume comparison
c


c Note that the maximum number of streamflow gauges is NG
c And the maximum number of readings is NS
      parameter(NG=99,NS=20000)
c Note minimum value of NG is 99

      integer i,j,k,l,num,year,month,gnum,syear,errflag
	integer daynum(12),dayi(NS)
      real mflow(NS,NG),sflow(NS,NG),msum(NG),acount(NG)
      real mave(NG),nash(NG),ssum(NG)

	integer monnewcount,day,point
      real monmflow(NS,NG),monsflow(NS,NG),monmsum(NG)
	real monacount(NG),avemon,avedaily,avevolume
      real monmave(NG),monnash(NG),monssum(NG)
	real mvolume(NG), svolume(NG)
      real gnumdaily,gnummonthly,gnumvolume
      integer start1, start2, start3, endflag
      character junk40*40
      logical exists

      data daynum/31,28,31,30,31,30,31,31,30,31,30,31/

c set watflood flag, if watflood=1 read spl.csv, otherwise read MESH file
      watflood=0
c set gauge that will be put at top of output files
c if value is set to -1 then take average of all the gauges
	point=-1

c print out header and program limits

      print *, 'This program reads in a MESH streamflow file'
	print *, 'called MESH_output_streamflow.csv and will'
	print *, 'output a file containing the Nash-Sutcliffe'
	print *, 'coefficient for each streamflow gauge.'
	print *
	print *, 'Please report any bugs to Frank (frseglen@uwaterloo.ca)'
	print *
	print *, 'This version of the program has the following limits:'
	print *, 'Number of streamflow gauges: ', NG
	print *, 'Number of streamflow values: ', NS
	print *

c skip over echo_print_check for watflood
      if(watflood.ne.1) then

c check for MESH_output_echo_print.txt
      endflag=0     
	open(2,file='MESH_output_echo_print.txt')
  5    read(2,'(A40)', end=10) junk40                
      if(verify(junk40,'Total Baseflow').gt.30) endflag=1 
c      print *, junk40, verify(junk40,'Total Baseflow')
      goto 5
  10  close(2)
      if(endflag.eq.0) then
	print *, 'MESH did not finish properly'
	print *, 'Value of -999.9 will be written to nash.csv'
	print *
      open(2,file='nash_monthly.csv')
      write(2,1000) -999.9
      close(2)
      open(2,file='nash_daily.csv')
      write(2,1000) -999.9
      close(2)
      stop     
	endif
	
      endif

c hard wire in the beginning year, this is used
c to figure out the leap year in the monthly NASH calculations
      syear=1961

      do j=1,NG
	do k=1,NS
	mflow(k,j)=-99.9
	sflow(k,j)=-99.9
      enddo
	enddo

      gnum=-99
      i=1
      errflag=0

c determine the number of streamflow gauges

      if(watflood.ne.1) then
	open(1,file='MESH_output_streamflow.csv')
      else
	open(1,file='spl.csv')
      endif
c just in case the run doesn't start at 1
      read(1,*,end=25) start1
	rewind 1
	start2=start1+1
 	start3=start2+1

      if(start1.eq.365) then
	start2=1
      start3=2
	endif

      if(start1.eq.364) then
	start2=365
      start3=1
	endif

c start out by reading the first 20 elements of spl.csv
      read(1,*,end=25) dayi(1),(mflow(1,j),sflow(1,j),j=1,20)
      do j=1,20
      if(mflow(1,j).eq.start2.and.2*(j-1)+1.le.NG) then
      if(sflow(1,2*(j-1)+1).eq.start3) then
      gnum=j-1
	endif
      endif
	enddo

c now read in the first 50 elements
      if(gnum.eq.-99) then
      rewind 1
      read(1,*,end=25) dayi(1),(mflow(1,j),sflow(1,j),j=1,50)
      do j=1,50
      if(mflow(1,j).eq.start2.and.2*(j-1)+1.le.NG) then
      if(sflow(1,2*(j-1)+1).eq.start3) then
      gnum=j-1
	endif
      endif
	enddo
      endif

c now read in the first 99 elements
      if(gnum.eq.-99) then
      rewind 1
      read(1,*,end=25) dayi(1),(mflow(1,j),sflow(1,j),j=1,99)
      do j=1,99
      if(mflow(1,j).eq.start2.and.2*(j-1)+1.le.NG) then
      if(sflow(1,2*(j-1)+1).eq.start3) then
      gnum=j-1
	endif
      endif
	enddo
      endif

      goto 30

 25   errflag=1

 30   if(gnum.eq.-99.or.errflag.eq.1) then
      print *, 'Unable to determine number of streamflow gauges'
	pause
	stop
	endif

      print *, 'Number of streamflow gauges found:', gnum

c check if we have too many streamflow gauges
      if (gnum.gt.NG) then
	print *, 'Too many streamflow gauges'
	print *, 'Maximum is: ',NG
	pause
	stop
	endif

      rewind 1

 50   read(1,*,end=100) dayi(i),(mflow(i,j),sflow(i,j),j=1,gnum)
      i=i + 1
      goto 50

 100  close(1)

      print *, 'Number of total streamflow readings found:', i
      print *

c check if we have too many streamflow readings
      if (i.gt.NS) then
	print *, 'Too many streamflow readings'
	print *, 'Maximum is: ',NS
	pause
	stop
	endif

c check for NASH_input.txt file, if not stick with default values
	inquire(file='NASH_input.txt',EXIST = exists)
      if(exists) then
      open(22,file='NASH_input.txt')
	read(22,"(I5)",iostat=ios) point
      close(22)	
	endif 
      if(point.le.0.or.point.gt.gnum) point=-1

      print *
      if(point.eq.-1) then
	print *,'NASH will be calculated by averaging all gauges'
      else
	print *,'NASH will be calculated for the following gauge: ',point
	endif
      print *

c daily nash sutcliffe

	do j=1,gnum
      msum(j)=0.0
      acount(j)=0.0
      svolume(j)=0.0
      mvolume(j)=0.0
      enddo

      do k=1,i
	do j=1,gnum
      if(mflow(k,j).gt.0.001) then
	msum(j)=msum(j)+mflow(k,j)
      acount(j)=acount(j)+1.0
      svolume(j)=svolume(j)+sflow(k,j)
      mvolume(j)=mvolume(j)+mflow(k,j)
      endif
	enddo
	enddo

	do j=1,gnum
      print *, 'Usable streamflow readings found in gauge:', j,acount(j)
      enddo
      print *

      do j=1,gnum
      if(acount(j).gt.0) then
      mave(j)=msum(j)/acount(j)
      else
      mave(j)=0.0
      endif
      enddo

	do j=1,gnum
      msum(j)=0.0
      ssum(j)=0.0
      enddo

      do k=1,i
	do j=1,gnum
      if(mflow(k,j).gt.0.0) then
	msum(j)=msum(j)+(sflow(k,j)-mflow(k,j))**2
	ssum(j)=ssum(j)+(mflow(k,j)-mave(j))**2
      endif
	enddo
	enddo

      do j=1,gnum
      if(ssum(j).gt.0) then
      nash(j)=1-(msum(j)/ssum(j))
      else
      nash(j)=-9999.9
      endif
      enddo

c monthly nash sutcliffe

      year=syear
      month=1
      k=0
      monnewcount=0

 200  if(mod(year,4).eq.0) then
	daynum(2)=29
      else
	daynum(2)=28
	endif

	do j=1,gnum
      monmsum(j)=0.0
      monssum(j)=0.0
      monacount(j)=0.0
      enddo

      monnewcount=monnewcount+1

      do day=1,daynum(month)
      k=k+1

	do j=1,gnum

      if(mflow(k,j).gt.0.0) then
	monmsum(j)=monmsum(j)+mflow(k,j)
	monssum(j)=monssum(j)+sflow(k,j)
      monacount(j)=monacount(j)+1.0
      endif

	enddo

      enddo

	do j=1,gnum
      if(monacount(j).gt.0.0) then
	monmflow(monnewcount,j)=monmsum(j)/monacount(j)
	monsflow(monnewcount,j)=monssum(j)/monacount(j)
      endif
	enddo

      month=month+1

	if(month.eq.13) then
	month=1
	year=year+1
	endif

      if(k.lt.i) goto 200


	do j=1,gnum
      monmsum(j)=0.0
      monacount(j)=0.0
      enddo

      do k=1,monnewcount
	do j=1,gnum
      if(monmflow(k,j).gt.0.0) then
	monmsum(j)=monmsum(j)+monmflow(k,j)
      monacount(j)=monacount(j)+1.0
      endif
	enddo
	enddo

      do j=1,gnum
      monmave(j)=monmsum(j)/monacount(j)
      enddo

	do j=1,gnum
      monmsum(j)=0.0
      monacount(j)=0.0
      enddo

      do k=1,monnewcount
	do j=1,gnum
      if(monmflow(k,j).gt.0.0) then
	monmsum(j)=monmsum(j)+(monsflow(k,j)-monmflow(k,j))**2
	monssum(j)=monssum(j)+(monmflow(k,j)-monmave(j))**2
c	monmsum(j)=monmsum(j)+abs(monsflow(k,j)-monmflow(k,j))
c	monssum(j)=monssum(j)+abs(monmflow(k,j)-monmave(j))
      endif
	enddo
	enddo

      do j=1,gnum
      if(monssum(j).gt.0) then
      monnash(j)=1-(monmsum(j)/monssum(j))
      else
      monnash(j)=-9999.9
      endif
      enddo

c calculate average values of NASH
      avemon=0.0
      avedaily=0.0
      avevolume=0.0
      gnummonthly=0.0
	gnumdaily=0.0
	gnumvolume=0.0


	do j=1,gnum
      if(monnash(j).gt.-9990.0) then
      avemon=avemon+monnash(j)
      gnummonthly=gnummonthly+1.0
      endif
      if(nash(j).gt.-9990.0) then
      avedaily=avedaily+nash(j)
      gnumdaily=gnumdaily+1.0
      endif
        if(mvolume(j).gt.0.0) then
	    avevolume=avevolume+abs( (svolume(j)-mvolume(j))/mvolume(j) )
      gnumvolume=gnumvolume+1.0
        endif
      enddo

	if(gnummonthly.gt.0.0)then
        avemon=avemon/gnummonthly
      endif
	if(gnumdaily.gt.0.0)then
        avedaily=avedaily/gnumdaily
      endif
	if(gnumvolume.gt.0.0)then
	  avevolume=avevolume/gnumvolume
      endif

c print out final results monthly


      open(2,file='nash_monthly.csv')

      if(point.eq.-1)then
      if(avemon.lt.-9999.9) avemon=-9999.9
      write(2,1000) avemon
	else
      if(monnash(point).lt.-9999.9)monnash(point)=-9999.9
      write(2,1000) monnash(point)
	endif
      write(2,1000) 
      do j=1,gnum
      if(monnash(j).lt.-9999.9)monnash(j)=-9999.9
      write(2,1000) monnash(j)
      enddo

      write(2,*)
	close(2)

c print out final results daily

      open(2,file='nash_daily.csv')

      if(point.eq.-1)then
      if(avedaily.lt.-9999.9) avedaily=-9999.9
      write(2,1000) avedaily
      else
      if(nash(point).lt.-9999.9)nash(point)=-9999.9
	write(2,1000) nash(point)
	endif
      write(2,1000) 

      do j=1,gnum
      if(nash(j).lt.-9999.9)nash(j)=-9999.9
      write(2,1000) nash(j)
      enddo

      write(2,*)
	close(2)

c print out final results total volume

      open(2,file='volume_total.csv')

      if(point.eq.-1)then
      write(2,1000) avevolume
      else
      if(mvolume(point).gt.0.0) then
	write(2,1000) (svolume(point)-mvolume(point))/mvolume(point)
      else
	write(2,1000) -9999.99
      endif

	endif
      write(2,1000) 

      do j=1,gnum

      if(mvolume(j).gt.0.0) then
      write(2,1000) (svolume(j)-mvolume(j))/mvolume(j)
      else
	write(2,1000) -9999.99
      endif

      enddo

      write(2,*)
	close(2)


 1000 format(F25.6,999(',',F25.6))


      print *, 'Nash-Sutcliffe coefficients have been written'
	print *, 'Normal program termination'
	print *

	stop
	end

