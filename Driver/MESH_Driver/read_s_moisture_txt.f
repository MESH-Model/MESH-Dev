      SUBROUTINE READ_S_MOISTURE_TXT(
     + YCOUNT, XCOUNT, NA, NTYPE, NML, NSL,
     + YYY, XXX, ILMOS, JLMOS,
     + THLQ,
     + il1, il2)
!> SOIL MOISTURE

      implicit none

!> local variables
      INTEGER :: i, j, k, s_ios
      REAL*4, DIMENSION(:, :, :), ALLOCATABLE :: valuem
!> read in variables
      INTEGER :: YCOUNT, XCOUNT
      integer*4 :: NA, NTYPE, NML, NSL, il1, il2
      integer*4 :: YYY(NA), XXX(NA), ILMOS(NML), JLMOS(NML)
      REAL :: THLQ(NML, NSL)

!ANDY - This function is for future development. Currently doesn't work.
      RETURN

      ALLOCATE (valuem(YCOUNT, XCOUNT, NSL))

      OPEN(UNIT = 59, FILE = 's_moisture.txt', STATUS = 'old',
     +     IOSTAT = s_ios)
!> IOSTAT returns 0 on successful file open so
!> s_ios will be 0 if the file opened properly.
      IF (s_ios == 0) THEN
        DO j = 1, NSL
          READ(59, *)
          DO i = 1, YCOUNT
             READ(59, *) (valuem(i, k, j), k = 1, XCOUNT)
          END DO
        END DO
        DO k = il1, il2
          i = ILMOS(k)   !> number of cells
            DO j = 1, NSL   !> soil layers
              THLQ(k, j) = valuem(YYY(i), XXX(i), j)
            END DO
        END DO
      ELSE
         PRINT *, 'S_MOISTURE.TXT file not found'
         PRINT *, '  Running without gridded initial soil moisture'
      END IF
      CLOSE(59)
!> note444. search for !note444 in mesh driver.
!> you will see that the values in THLQROW are reset.
!> that occures after this is called, so the values
!> which are written here will be written over.
      RETURN
      END SUBROUTINE READ_S_MOISTURE_TXT
