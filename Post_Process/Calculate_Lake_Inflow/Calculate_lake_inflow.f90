
REAL, allocatable :: input(:,:),month_sum(:),month_final(:)
!REAL, allocatable :: input(:,:)
!REAL :: month_sum(250),month_final(250)
INTEGER :: days_in_month(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
CHARACTER (LEN=2) :: num(0:99)= (/'00','01','02','03','04','05','06','07','08','09',&
     '10','11','12','13','14','15','16','17','18','19','20',&
     '21','22','23','24','25','26','27','28','29','30',&
     '31','32','33','34','35','36','37','38','39','40',&
     '41','42','43','44','45','46','47','48','49','50',&
     '51','52','53','54','55','56','57','58','59','60',&
     '61','62','63','64','65','66','67','68','69','70',&
     '71','72','73','74','75','76','77','78','79','80',&
     '81','82','83','84','85','86','87','88','89','90',&
     '91','92','93','94','95','96','97','98','99'/)
CHARACTER (LEN=4) :: year_char(1997:2010) = (/'1997','1998','1999','2000','2001', &
     &  '2002','2003','2004','2005','2006','2007','2008','2009','2010' /)
CHARACTER :: junk*50
INTEGER :: imax,jmax,i,j,lake_temp,x_temp,y_temp,hour_count
INTEGER :: lake(250),x(250),y(250),cur_day,cur_month_cur_year
REAL :: seq,step,yy,mm,dd,hh,mi,ss,ms,month_count
LOGICAL :: WATFLOOD

imax=120
jmax=66
WATFLOOD=.TRUE.
month_count=0

IF(WATFLOOD) THEN
PRINT *, 'Model chosen: WATFLOOD'
cur_day=1
cur_month=1
cur_year=2004
ELSE
PRINT *, 'Model chosen: MESH'
cur_day=1
cur_month=6
cur_year=2004
ENDIF

ALLOCATE ( input(imax,jmax)  )

! First read in inflow points

OPEN(UNIT=20,file='glake_inflow.csv')
inflow_count=1
DO WHILE (I /= 9999)
READ(20,*,iostat=ios) lake_temp,x_temp,y_temp
if(ios /= 0) EXIT

lake(inflow_count)=lake_temp
x(inflow_count)=x_temp
y(inflow_count)=y_temp

inflow_count=inflow_count+1

END DO
CLOSE(20)

ALLOCATE ( month_sum(inflow_count),month_final(inflow_count) )
month_sum = 0.0

IF(WATFLOOD) THEN
  open(unit=65,file='watflood_mod.wfo',status='old',form='binary' )
ELSE
  open(unit=65,file='basin_streamflow_daily.csv',status='old')
ENDIF

OPEN(11,file='inflow_daily.csv')

WRITE(11,"(I15,9999(',',I15))") (lake(i),i=1,inflow_count-1)
WRITE(11,"(I15,9999(',',I15))") (x(i),i=1,inflow_count-1)
WRITE(11,"(I15,9999(',',I15))") (y(i),i=1,inflow_count-1)
WRITE(11,*) 

OPEN(12,file='inflow_monthly.csv')

WRITE(12,"(I15,9999(',',I15))") (lake(i),i=1,inflow_count-1)
WRITE(12,"(I15,9999(',',I15))") (x(i),i=1,inflow_count-1)
WRITE(12,"(I15,9999(',',I15))") (y(i),i=1,inflow_count-1)
WRITE(12,*)


IF(WATFLOOD) THEN

  DO WHILE (I /= 9999)
    print *, "here 1"
!    read(65,*,iostat=ios)
    read(65,iostat=ios) seq,step,yy,mm,dd,hh,mi,ss,ms
    print *, "here 2",ios
    if(ios /= 0) EXIT
    read(65,iostat=ios) ((input(i,j),i=1,imax),j=jmax,1,-1)
!      DO j=jmax,1,-1
!      read(65, *, iostat=ios) (input(i,j),i=1,imax)
!      ENDDO
    print *, "here 3",input(1,1),ios
    if(ios /= 0) EXIT
    WRITE(11,"(E15.8,9999(',',E15.8))") (input(x(i),y(i)),i=1,inflow_count-1)
  END DO

ELSE

  hour_count=0
  DO WHILE (I /= 9999)
    hour_count=hour_count+1
    if(mod(REAL(hour_count),100.0)==0.0) print *, 'Working on hour: ',hour_count
    read(65,*,iostat=ios)
    if(ios /= 0) EXIT
!    IF(WATFLOOD) THEN
!      DO j=jmax,1,-1
!      read(65, *, iostat=ios) (input(i,j),i=1,imax)
!      ENDDO
!    if(ios /= 0) EXIT
!  ELSE ! end of WATFLOOD section
    !print *, cur_day, cur_month, cur_year

    DO j=1,jmax
      read(65, *, iostat=ios) (input(i,j),i=1,imax)
    ENDDO
  if(ios /= 0) EXIT
!  ENDIF
  read(65,*,iostat=ios)

  WRITE(11,"(E15.8,9999(',',E15.8))") (input(x(i),y(i)),i=1,inflow_count-1)
! put in monthly averages
  DO i=1,inflow_count-1
    month_sum(i) = month_sum(i) + input( x(i),y(i) )
  END DO
  month_count = month_count + 1.0

  IF( cur_day == days_in_month(cur_month)) THEN
! first calculate final average for the month
    DO i=1,inflow_count-1
      month_final(i) = month_sum(i)/month_count
    END DO
    WRITE(12,"(E15.8,9999(',',E15.8))") (month_final(i),i=1,inflow_count-1)
    month_sum = 0.0
    month_count = 0.0
    cur_day=1
    cur_month=cur_month+1
    IF( cur_month == 13) THEN
      cur_year=cur_year+1
      cur_month=1
      IF(mod(real(cur_year),4.0) == 0) THEN
        days_in_month(2)=29
      ELSE
        days_in_month(2)=28
      ENDIF
    ENDIF
  ELSE
    cur_day=cur_day+1
  ENDIF

  END DO

ENDIF ! end of MESH section

CLOSE (65)


!WRITE(11,"(E15.8,9999(',',E15.8))") seq,step,yy,mm,dd,hh,mi,ss,ms


!DO J=1,JMAX
!WRITE(11,"(E15.8,9999(',',E15.8))") (input(i,j),i=1,imax)
!END DO

CLOSE(11)
CLOSE(12)

PRINT *, 'Program terminated normally'

STOP
END
