!  lake_inflow_streamflow_files.f90 
!
!  FUNCTIONS:
!  lake_inflow_streamflow_files      - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: lake_inflow_streamflow_files
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program lake_inflow_streamflow_files

    implicit none

    ! Variables

CHARACTER (LEN=80) :: header
INTEGER :: i,j,imax, jmax, loop_count, current_lake
INTEGER, DIMENSION(120,66) :: dir, lake
INTEGER, DIMENSION(500) :: inflowi,inflowj
REAL, DIMENSION(120,66) :: drainage

    ! Body of lake_inflow_streamflow_files

imax=120
jmax=66

OPEN(1,file='glake.map')

DO 
  READ(1,"(a80)") header

    IF(header(1:18) /= "Drainage direction" ) CYCLE
    DO j=1,jmax
      READ(1,*) (dir(i,j),i=1,imax)
    END DO
    EXIT
END DO

!DO 
!  READ(1,"(a80)") header
!    IF(header(1:11) /= "River Class" ) CYCLE
!    DO j=jmax,1,-1
!      READ(1,*) (drainage(i,j),i=1,imax)
!    END DO
!    EXIT
!END DO


DO 
  READ(1,"(a80)") header
    IF(header(1:12) /= "Reach Number" ) CYCLE
    DO j=1,jmax
      READ(1,*) (lake(i,j),i=1,imax)
    END DO
    EXIT
END DO

CLOSE(1)

OPEN(12,file='glake_inflow.csv')

DO current_lake=1,7

DO i=1,500
inflowi(i)=0
inflowj(i)=0
END DO

loop_count=0

DO i=1,imax
  DO j=1,jmax

IF(lake(i,j) /= 0 ) CYCLE

SELECT CASE(dir(i,j))
CASE(1)
IF( lake(i+1,j-1) == current_lake ) THEN
loop_count=loop_count+1
inflowi(loop_count)=i
inflowj(loop_count)=j
END IF
CASE(2)
IF( lake(i+1,j) == current_lake ) THEN
loop_count=loop_count+1
inflowi(loop_count)=i
inflowj(loop_count)=j
END IF
CASE(3)
IF( lake(i+1,j+1) == current_lake ) THEN
loop_count=loop_count+1
inflowi(loop_count)=i
inflowj(loop_count)=j
END IF
CASE(4)
IF( lake(i,j+1) == current_lake ) THEN
loop_count=loop_count+1
inflowi(loop_count)=i
inflowj(loop_count)=j
END IF
CASE(5)
IF( lake(i-1,j+1) == current_lake ) THEN
loop_count=loop_count+1
inflowi(loop_count)=i
inflowj(loop_count)=j
END IF
CASE(6)
IF( lake(i-1,j) == current_lake ) THEN
loop_count=loop_count+1
inflowi(loop_count)=i
inflowj(loop_count)=j
END IF
CASE(7)
IF( lake(i-1,j-1) == current_lake ) THEN
loop_count=loop_count+1
inflowi(loop_count)=i
inflowj(loop_count)=j
END IF
CASE(8)
IF( lake(i,j-1) == current_lake ) THEN
loop_count=loop_count+1
inflowi(loop_count)=i
inflowj(loop_count)=j
END IF
END SELECT
  
  END DO
END DO


DO i=1,loop_count
WRITE(12,"(I4,',',I8,',',I8,',',F10.3)") current_lake, inflowi(i),inflowj(i) 
!  & , drainage(inflowi(i),inflowj(i))
END DO

END DO

CLOSE(12)


    end program lake_inflow_streamflow_files

