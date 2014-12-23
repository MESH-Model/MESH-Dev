      SUBROUTINE READ_SOIL_LEVELS(IGND, sl)

      USE MESH_INPUT_MODULE

!> passed in variables
      INTEGER :: IGND
      TYPE(SoilLevels) :: sl
!> local variables
      INTEGER :: IOS


      OPEN (52, FILE="MESH_input_soil_levels.txt", STATUS="OLD",
     1  IOSTAT=IOS)
      IF (IOS .NE. 0)THEN !CHECK FILE FOR IOSTAT ERRORS
        WRITE (6, *)
        WRITE (6, *)
        WRITE (6, *) "MESH_input_soil_levels.txt could not be ",
     1      "opened.  Ensure that the file exists and restart the ",
     2      "program."
        STOP
      ELSE
        WRITE (6, '(A)', ADVANCE="NO")
     +          "READING: MESH_input_soil_levels.txt"
      END IF

      DO I=1,IGND
!todo change documentation to reflect that we read in delz only (and not zbot)
!todo check other variables read-in from other files
!todo put in a warning that at least 3 layers are needed.
        READ(52, *) sl%DELZ(I)
      ENDDO

      sl%ZBOT(1) = sl%DELZ(1)
      DO I=2,IGND
         sl%ZBOT(I) = sl%ZBOT(I-1) + sl%DELZ(I)
      ENDDO

	CLOSE(UNIT=52)
      WRITE (6, *) " READ: SUCCESSFUL, FILE: CLOSED"

      RETURN
      END SUBROUTINE READ_SOIL_LEVELS
