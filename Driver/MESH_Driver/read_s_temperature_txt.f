      SUBROUTINE READ_S_TEMPERATURE_TXT(
     + YCOUNT, XCOUNT, NA, NTYPE, NML, NSL,
     + YYY, XXX, ILMOS, JLMOS,
     + TBAR,
     + il1, il2)
!> SOIL TEMPERATURE

      implicit none

!> local variables
      INTEGER :: i, j, k, s_ios
      REAL*4, DIMENSION(:, :, :), ALLOCATABLE :: valuet
!> read in variables
      INTEGER :: YCOUNT, XCOUNT
      integer*4 :: NA, NTYPE, NML, NSL, il1, il2
      integer*4 :: YYY(NA), XXX(NA), ILMOS(NML), JLMOS(NML)
      REAL :: TBAR(NML, NSL)

!ANDY - This function is for future development. Currently doesn't work.
      RETURN

      ALLOCATE (valuet(YCOUNT, XCOUNT, NSL))

      OPEN(UNIT = 59, FILE = 's_temperature.txt', STATUS = 'old',
     +     IOSTAT = s_ios)
!> IOS returns 0 on successful file open so,
!> s_ios will equal 0 if file opened successfully.
      IF (s_ios == 0) THEN
        DO j = 1, NSL
          READ(59, *)
          DO i = 1, YCOUNT
             READ(59, *) (valuet(i, k, j), k = 1, XCOUNT)
          END DO
        END DO
        DO k = il1, il2
          i = ILMOS(k)   !> number of cells
            DO j = 1, NSL   !> soil layers
              TBAR(k, j) = valuet(YYY(i), XXX(i), j)
            END DO
        END DO
      ELSE
         PRINT *, 'S_TEMPERATURE.TXT file not found'
         PRINT *, '  Running without gridded initial soil temperature'
      END IF
      CLOSE(59)
!> note333. search for !note333 in mesh driver.
!> you will see that the values in TBARROW are reset.
!> that occures after this is called, so the values
!> which are written here will be written over.
      RETURN
      END SUBROUTINE READ_S_TEMPERATURE_TXT
