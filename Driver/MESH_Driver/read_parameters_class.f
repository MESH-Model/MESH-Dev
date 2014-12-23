      SUBROUTINE READ_PARAMETERS_CLASS(
     +  TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6,
     +  NAME1, NAME2, NAME3, NAME4, NAME5, NAME6,
     +  PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6,
     +  NA,ILW, NLTEST, NMTEST,
     +  IGND, JLAT, ICAN, NTYPE,
     +  DEGLAT, DEGLON,
     +  HOURLY_START_DAY,  HOURLY_STOP_DAY,
     +  DAILY_START_DAY,   DAILY_STOP_DAY,
     +  HOURLY_START_YEAR, HOURLY_STOP_YEAR,
     +  DAILY_START_YEAR,  DAILY_STOP_YEAR,
     +  IHOUR, IMIN, IDAY, IYEAR, cp, fls)

      USE MESH_INPUT_MODULE
      use model_files
      USE FLAGS

      CHARACTER*4 ::
     +  TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6,
     +  NAME1, NAME2, NAME3, NAME4, NAME5, NAME6,
     +  PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
!> passed in variables
      INTEGER :: NA, ILW, NLTEST, NMTEST,
     +        IGND, JLAT, ICAN,
     +        HOURLY_START_DAY,  HOURLY_STOP_DAY,
     +        DAILY_START_DAY,   DAILY_STOP_DAY,
     +        HOURLY_START_YEAR, HOURLY_STOP_YEAR,
     +        DAILY_START_YEAR,  DAILY_STOP_YEAR,
     +        IHOUR, IMIN, IDAY, IYEAR

      integer*4 :: NTYPE
      REAL :: DEGLAT, DEGLON
      TYPE(ClassParameters) :: cp
      
      !file handled
      type(fl_ids)              :: fls 

!> local variables
      integer IOS,I,M,J

      if ((VARIABLEFILESFLAG .eq. 1) .and. (fls%fl(2)%isInit)) then
        OPEN(fls%fl(2)%unit,FILE=trim(adjustl(fls%fl(2)%name)),
     1  STATUS="OLD", IOSTAT=IOS)
      else   
        OPEN (50, FILE="MESH_parameters_CLASS.ini", STATUS="OLD",
     1  IOSTAT=IOS)
      endif
!> CHECK FILE FOR IOSTAT ERRORS
!> if IOS is set to zero, then the file was opened successfully.
      IF (IOS .NE. 0)THEN 
        WRITE (6, *)
        WRITE (6, *)
        WRITE (6, *) "MESH_parameters_CLASS.ini could not be ",
     1      "opened.  Ensure that the file exists and restart the ",
     2      "program."
        STOP
      ELSE
        WRITE (6, '(A)', ADVANCE="NO")
     +          "READING: MESH_parameters_CLASS.ini"
      ENDIF

      READ (50,'(2X,6A4)') TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      READ (50,'(2X,6A4)') NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      READ (50,'(2X,6A4)') PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      READ(50,*) DEGLAT,DEGLON,cp%ZRFMGRD(1),
     +  cp%ZRFHGRD(1),cp%ZBLDGRD(1),cp%GCGRD(1),ILW,NLTEST,NMTEST

      IF(NTYPE.NE.NMTEST .AND. NTYPE.GT.0) THEN
        WRITE (6, *)
        WRITE (6, *)
        WRITE (6, *) "GRUs from MESH_parameters_CLASS.ini: ", NMTEST
        WRITE (6, *) "GRUs from basin watershed file: ", NTYPE
        WRITE (6, *) "These values must be equal."
        CLOSE (50)
	  STOP
	END IF


!todo - fix this so that we only use one of the variables (use NA and ignore NLTEST - doc)
      IF (NLTEST /= NA) THEN
        WRITE (6, *)
        WRITE (6, *) "ERROR: The number of grid squares in the class",
     1  " parameters file does not match the number of grid squares ",
     2  "from the shed file."
        STOP
      END IF

      JLAT=NINT(DEGLAT)

      I=1
      DO M=1,NMTEST
        READ(50,*) (cp%FCANROW(1,M,J),J=1,ICAN+1),
     1                  (cp%PAMXROW(1,M,J),J=1,ICAN)
        READ(50,*) (cp%LNZ0ROW(1,M,J),J=1,ICAN+1),
     1                  (cp%PAMNROW(1,M,J),J=1,ICAN)
        READ(50,*) (cp%ALVCROW(1,M,J),J=1,ICAN+1),
     1                  (cp%CMASROW(1,M,J),J=1,ICAN)
        READ(50,*) (cp%ALICROW(1,M,J),J=1,ICAN+1),
     1                  (cp%ROOTROW(1,M,J),J=1,ICAN)
        READ(50,*) (cp%RSMNROW(1,M,J),J=1,ICAN),
     1                  (cp%QA50ROW(1,M,J),J=1,ICAN)
        READ(50,*) (cp%VPDAROW(1,M,J),J=1,ICAN),
     1                  (cp%VPDBROW(1,M,J),J=1,ICAN)
        READ(50,*) (cp%PSGAROW(1,M,J),J=1,ICAN),
     1                  (cp%PSGBROW(1,M,J),J=1,ICAN)
        READ(50,*) cp%DRNROW(1,M),cp%SDEPROW(1,M),
     1                  cp%FAREROW(1,M),cp%DDROW(1,M)
        READ(50,*) cp%XSLPROW(1,M),cp%XDROW(1,M),
     1                  cp%MANNROW(1,M),cp%KSROW(1,M),cp%MIDROW(1,M)
!        This requires mesh_parameters_class.ini file to include info on layer 4 to ignd
!        READ(50,'(3F10.1)') (cp%SANDROW(1,M,J),J=1,IGND) !soil layers
!        READ(50,'(3F10.1)') (cp%CLAYROW(1,M,J),J=1,IGND)
!        READ(50,'(3F10.1)') (cp%ORGMROW(1,M,J),J=1,IGND)
!        READ(50,'(6F10.2)') (cp%TBARROW(1,M,J),J=1,IGND),
!     1                  cp%TCANROW(1,M),cp%TSNOROW(1,M),cp%TPNDROW(1,M)
!        READ(50,'(7F10.3)') (cp%THLQROW(1,M,J),J=1,IGND),
!     1                  (cp%THICROW(1,M,J),J=1,IGND),cp%ZPNDROW(1,M)
!     1                  cp%MANNROW(1,M),cp%KSROW(1,M),cp%MIDROW(1,M)
!        This requires mesh_parameters_class.ini file to include info on 3 layers only
        READ(50,*) (cp%SANDROW(1,M,J),J=1,3) !soil layers
        READ(50,*) (cp%CLAYROW(1,M,J),J=1,3)
        READ(50,*) (cp%ORGMROW(1,M,J),J=1,3)
        READ(50,*) (cp%TBARROW(1,M,J),J=1,3),
     1                  cp%TCANROW(1,M),cp%TSNOROW(1,M),cp%TPNDROW(1,M)
        READ(50,*) (cp%THLQROW(1,M,J),J=1,3),
     1                  (cp%THICROW(1,M,J),J=1,3),cp%ZPNDROW(1,M)
        READ(50,*)
     +                  cp%RCANROW(1,M),cp%SCANROW(1,M),cp%SNOROW(1,M),
     1                  cp%ALBSROW(1,M),cp%RHOSROW(1,M),cp%GROROW(1,M)
      ENDDO

!todo - Document the !P variables better. Ignore for now.
!P
!todo - make sure these variables are documented properly
      READ(50,*) HOURLY_START_DAY, HOURLY_STOP_DAY, DAILY_START_DAY,
     1           DAILY_STOP_DAY !P, IDAY_START, IDAY_END
      READ(50,*) HOURLY_START_YEAR, HOURLY_STOP_YEAR, DAILY_START_YEAR,
     1           DAILY_STOP_YEAR !P, IYEAR_START, IYEAR_END
!> Read in hour, minute, day and year from class.ini file as it is
!> not present in the forcing files
      READ(50,*) IHOUR,IMIN,IDAY,IYEAR

!> Close unit 50 as we don't need anything else from the class.ini file
      CLOSE(UNIT=50)
      WRITE (6, *) " READ: SUCCESSFUL, FILE: CLOSED"

c     Convert DD from km/km^2 to m/m^2
c     The formulae in WATROF.f expect m/m^2
      DO M=1,NMTEST
        cp%DDROW(1,M)= cp%DDROW(1,M)/1000.0
      ENDDO

      RETURN
      END SUBROUTINE READ_PARAMETERS_CLASS
