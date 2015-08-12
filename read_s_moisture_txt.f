      SUBROUTINE READ_S_MOISTURE_TXT(
     + IGND, YCOUNT, XCOUNT, na, NTYPE,
     + YYY, XXX, THLQROW )
!> local variables
      INTEGER :: i,j,k,M,s_ios
      REAL*4, ALLOCATABLE, DIMENSION(:,:,:) :: valuem
!> read in variables
      INTEGER :: IGND,YCOUNT,XCOUNT
      integer*4 :: na,NTYPE
      integer*4 :: YYY(NA),XXX(NA)
      REAL :: THLQROW(NA, NTYPE, IGND)

!> SOIL MOISTURE

!ANDY - This function is for future development. Currently doesn't work.
      RETURN

      ALLOCATE (valuem(YCOUNT,XCOUNT,ignd))

      OPEN(UNIT=59,FILE='s_moisture.txt',STATUS='old',IOSTAT=s_ios)
!> IOSTAT returns 0 on successful file open so
!> s_ios will be 0 if the file opened properly.
      IF(s_ios==0)THEN
        DO J=1,IGND
          READ(59,*)
          DO i=1,YCOUNT
             READ(59,*)(valuem(i,k,j),k=1,XCOUNT)
          enddo
        enddo
        DO I=1,NA     !> number of cells
          DO M=1,NMTEST   !> number of classes
            DO J=1,IGND   !> soil layers
              THLQROW(I,M,J)= valuem(YYY(I),XXX(I),J)
            enddo
          enddo
        enddo
      ELSE
         PRINT*,'S_MOISTURE.TXT file not found'
         PRINT*,'  Running without gridded initial soil moisture'
      ENDIF
      CLOSE(59)
!> note444. search for !note444 in mesh driver.
!> you will see that the values in THLQROW are reset.
!> that occures after this is called, so the values
!> which are written here will be written over.
      RETURN
      END SUBROUTINE READ_S_MOISTURE_TXT