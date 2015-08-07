      SUBROUTINE READ_S_TEMPERATURE_TXT(
     + IGND, YCOUNT, XCOUNT, na, NTYPE,
     + YYY, XXX, TBARROW )
!> local variables
      INTEGER :: i,j,k,M,s_ios
      REAL*4, DIMENSION(:, :, :), ALLOCATABLE :: valuet
!> read in variables
      INTEGER :: IGND,YCOUNT,XCOUNT
      integer*4 :: na,NTYPE
      integer*4 :: YYY(NA),XXX(NA)
      REAL :: TBARROW(NA, NTYPE, IGND)
!> SOIL TEMPERATURE
!ANDY - This function is for future development. Currently doesn't work.
      RETURN

      ALLOCATE (valuet(YCOUNT,XCOUNT,ignd))

      OPEN(UNIT=59,FILE='s_temperature.txt',STATUS='old',IOSTAT=s_ios)
!> IOS returns 0 on successful file open so,
!> s_ios will equal 0 if file opened successfully.
      IF(s_ios==0)THEN
        DO J=1,IGND
          READ(59,*)
          DO i=1,YCOUNT
             READ(59,*)(valuet(i,k,j),k=1,XCOUNT)
          enddo
        enddo
        DO I=1,NA     !> number of cells
          DO M=1,NMTEST   !> number of classes
            DO J=1,IGND   !> soil layers
              TBARROW(I,M,J)=   valuet(YYY(I),XXX(I),J)
            enddo
          enddo
        enddo
      ELSE
         PRINT*,'S_TEMPERATURE.TXT file not found'
         PRINT*,'  Running without gridded initial soil temperature'
      ENDIF
      CLOSE(59)
!> note333. search for !note333 in mesh driver.
!> you will see that the values in TBARROW are reset.
!> that occures after this is called, so the values
!> which are written here will be written over.
      RETURN
      END SUBROUTINE READ_S_TEMPERATURE_TXT