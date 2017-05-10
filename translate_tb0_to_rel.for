      program translate_tb0_to_rel

      character num(0:99)*2,cjunk*2,header*70
      integer daynum(12),syear,eyear,monnum(12),bmonth,emonth
      integer day,i,j,year,month,hour,headercount
      character name_long(999)*20
      real x(999),y(999),flow(999),coeffa(999),coeffb(999)
      integer no_of_stations

      data daynum/31,28,31,30,31,30,31,31,30,31,30,31/
      data monnum/31,59,90,120,151,181,212,243,273,304,334,365/
      data num/'00','01','02','03','04','05','06','07','08','09',
     +'10','11','12','13','14','15','16','17','18','19','20',
     +'21','22','23','24','25','26','27','28','29','30',
     +'31','32','33','34','35','36','37','38','39','40',
     +'41','42','43','44','45','46','47','48','49','50',
     +'51','52','53','54','55','56','57','58','59','60',
     +'61','62','63','64','65','66','67','68','69','70',
     +'71','72','73','74','75','76','77','78','79','80',
     +'81','82','83','84','85','86','87','88','89','90',
     +'91','92','93','94','95','96','97','98','99'/

c what years are you running
c     stating year
      syear=1997
c     ending year
      eyear=2008
c     number of stations
      no_of_stations=7

c read header from other file
      open(37,file="tb0_header.txt")
 20   read(37,"(A70)") header
c       print *, index(header(i),":endHeader"), header(i)
     
       if(index(header,":ColumnType").eq.0) goto 20

      read(37,*) cjunk,( name_long(i),i=1,no_of_stations)
      read(37,*) cjunk,( x(i),i=1,no_of_stations)
      read(37,*) cjunk,( y(i),i=1,no_of_stations)
      read(37,*) cjunk,( coeffa(i),i=1,no_of_stations)
      read(37,*) cjunk,( coeffb(i),i=1,no_of_stations)

       close(37)

      open(32,file='MESH_input_reservoir.txt')

c write header for old format streamflow file
      write(32, "(4I5)") no_of_stations,9999,24
      do i=1,no_of_stations
      write(32, "(2I5,2E10.2,26X,A12,I2)") 
     +int(y(i)*60),int(x(i)*60.0),coeffa(i),coeffb(i),name_long(i),i
      enddo

 	do year= syear, eyear

c check for leap year **No leap year in the RCM data**
c      if(mod(real(year),4.0).lt.0.5) then
c	daynum(2)=29
c	else
c	daynum(2)=28
c	endif

      if(year.eq.1997) then
      bmonth=4
      emonth=12
      elseif(year.eq.2008) then
      bmonth=1
      emonth=10
      else
      bmonth=1
      emonth=12
      endif

	do month = bmonth, emonth

      print *, 'on year: ',year,' month: ', month

c open new format *.tb0 file
      if(year.lt.2000) then
      open(31,file='19'//
     +num(year-1900)//num(month)//'01_REL.tb0')
      else
      open(31,file='20'//
     +num(year-2000)//num(month)//'01_REL.tb0')
      endif

 50   read(31,"(A70)") header
       if(index(header,":EndHeader").eq.0) goto 50


      do day=1,daynum(month)
      read(31,*) (flow(i),i=1,no_of_stations)
      write(32,"(999F10.3)") (flow(i),i=1,no_of_stations)
      enddo

      close(31)
     
      enddo !month loop
	enddo !year loop

      close(32)

	stop
	end


