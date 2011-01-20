       SUBROUTINE READ_RUN_OPTIONS(
     +  IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  IOS, PAS, N,
     +  IROVAL, WF_NUM_POINTS,
     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START,
     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END,
     +  ID, IRONAME, GENDIR_OUT, op )

      USE MESH_INPUT_MODULE
      USE FLAGS
      IMPLICIT NONE

!> the following variables were passed in from the subroutine call
      INTEGER :: IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  IOS, PAS, N,
     +  IROVAL, WF_NUM_POINTS,
     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START, !P
     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END !P

      INTEGER :: ID !didn't find the declaration in the driver
      INTEGER:: M_G  ! PARAMETER :: M_G = 5
      CHARACTER(20) :: IRONAME
      CHARACTER*10 GENDIR_OUT
      
      TYPE(OutputPoints) :: op



!> The following variables are all local to this subroutine
      INTEGER :: J,I


!>=======================================================================
!>    * SET RUN OPTIONS
!>    * READ THE RUN_OPTIONS.INI INPUT FILE TO SET OR RESET ANY CONTROL
!>    * FLAGS AND READ THE GRID OUTPUT DIRECTORIES.

!>    * IF IDISP = 0, VEGETATION DISPLACEMENT HEIGHTS ARE IGNORED,
!>    * BECAUSE THE ATMOSPHERIC MODEL CONSIDERS THESE TO BE PART OF THE
!>    * "TERRAIN".
!>    * IF IDISP = 1, VEGETATION DISPLACEMENT HEIGHTS ARE CALCULATED.
      IDISP = 1

!>    * IF IZREF = 1, THE BOTTOM OF THE ATMOSPHERIC MODEL IS TAKEN TO
!>    * LIE AT THE GROUND SURFACE.
!>    * IF IZREF = 2, THE BOTTOM OF THE ATMOSPHERIC MODEL IS TAKEN TO
!>    * LIE AT THE LOCAL ROUGHNESS HEIGHT.
      IZREF = 1

!>    * IF ISLFD = 0, DRCOEF IS CALLED FOR SURFACE STABILITY
!>    * CORRECTIONS AND THE ORIGINAL GCM SET OF SCREEN-LEVEL DIAGNOSTIC
!>    * CALCULATIONS IS DONE.
!>    * IF ISLFD = 1, DRCOEF IS CALLED FOR SURFACE STABILITY
!>    * CORRECTIONS AND SLDIAG IS CALLED FOR SCREEN-LEVEL DIAGNOSTIC
!>    * CALCULATIONS.
!>    * IF ISLFD = 2, FLXSURFZ IS CALLED FOR SURFACE STABILITY
!>    * CORRECTIONS AND DIASURF IS CALLED FOR SCREEN-LEVEL DIAGNOSTIC
!>    * CALCULATIONS.
      ISLFD = 2

!>    * IF IPCP = 1, THE RAINFALL-SNOWFALL CUTOFF IS TAKEN TO LIE AT
!>    * 0 dC.
!>    * IF IPCP = 2, A LINEAR PARTITIONING OF PRECIPITATION BETWEEEN
!>    * RAINFALL AND SNOWFALL IS DONE BETWEEN 0 dC AND 2 dC.
!>    * IF IPCP = 3, RAINFALL AND SNOWFALL ARE PARTITIONED ACCORDING TO
!>    * A POLYNOMIAL CURVE BETWEEN 0 dC AND 6 dC.
      IPCP = 1

!>    * ITC, ITCG AND ITG ARE SWITCHES TO CHOOSE THE ITERATION SCHEME
!>    * TO BE USED IN CALCULATING THE CANOPY OR GROUND SURFACE
!>    * TEMPERATURE RESPECTIVELY.  IF THE SWITCH IS SET TO 1, A
!>    * COMBINATION OF SECANT AND BISECTION METHODS IS USED; IF TO 2,
!>    * THE NEWTON-RAPHSON METHOD IS USED.
      ITC = 2
      ITCG = 2
      ITG = 2

!>    * IF IWF = 0, ONLY OVERLAND FLOW AND BASEFLOW ARE MODELLED, AND
!>    * THE GROUND SURFACE SLOPE IS NOT MODELLED.
!>    * IF IWF = n (0 < n < 4), THE MODIFIED CALCULATIONS OF OVERLAND
!>    * FLOW AND INTERFLOW ARE PERFORMED; INTERFLOW IS DRAWN FROM THE
!>    * TOP n SOIL LAYERS.
      IWF = 3

!>    * IF IPAI, IHGT, IALC, IALS AND IALG ARE ZERO, THE VALUES OF
!>    * LEAF ARE INDEX, VEGETATION HEIGHT, CANOPY ALBEDO, SNOW ALBEDO
!>    * AND SOIL ALBEDO RESPECTIVELY CALCULATED BY CLASS ARE USED.
!>    * IF ANY OF THESE SWITCHES IS SET TO 1, THE VALUE OF THE
!>    * CORRESPONDING PARAMETER CALCULATED BY CLASS IS OVERRIDDEN BY
!>    * A USER-SUPPLIED INPUT VALUE.
      IPAI = 0
      IHGT = 0
      IALC = 0
      IALS = 0
      IALG = 0



!> DAN * IF RELFLG = 0, ANY CONFIGURATION FILE IS READ THAT MATCHES
!> DAN * THE FILE NAME IN THE OPEN STATEMENT.
!> DAN * IF RELFLG = 1, ONLY CONFIGURATION FILES WHOSE VERSION MATCHES
!> DAN * THE RELEASE OF MESH_DRIVER ARE READ.  THE PROGRAM STOPS IF THE
!> DAN * TWO STRINGS DO NOT MATCH.
!> DAN * THIS FLAG IS NOT APPLICABLE TO RUN_OPTIONS.INI, WHERE THIS FLAG
!> DAN * MAY BE RESET).
      RELFLG = 1

!> DAN * IF ID = 1, RDEVT WILL READ THE NEW FORMAT EVENT FILE (VERSION
!> DAN * 9.7 OF THE EVENT FILE WAS USED IN TESTING).
! TODO: is ID really a flag? should not be set here.
      ID = 1


!> Set HOURLYFLAG to forcing data time step (in minutes).
!> 30 minute forcing data should be the default
      HOURLYFLAG = 30

!> set SHDFILEFLAG to 0 to use drainage_database.txt
!> set SHDFILEFLAG to 1 to use new_shd.r2c
      SHDFILEFLAG = 1

!> set RESUMEFLAG to 0 just as a default
!> this will be set later by reading the run_options file
!* if RESUMEFLAG is 0, the user doesn't want to use the resume file
!* if RESUMEFLAG is 1, the user wants to run the resume file.
      RESUMEFLAG = 0

!* if SAVERESUMEFLAG is 0, the user doesn't want to make the resume file.
!* if SAVERESUMEFLAG is 1, the user wants to make the resume file.
      SAVERESUMEFLAG = 0
      
!> FORCING DATA FILES:
!>  0 = read forcing data from .bin file
!>  1 = read forcing data from .r2c
!>  2 = read forcing data from .csv
      BASINSHORTWAVEFLAG = 0
      BASINLONGWAVEFLAG = 0
      BASINRAINFLAG = 0
      BASINTEMPERATUREFLAG = 0
      BASINWINDFLAG = 0
      BASINPRESFLAG = 0
      BASINHUMIDITYFLAG = 0

!* If SOILINIFLAG is 1, the user wants to use the soil.ini file. If SOILINIFLAG
!*  is 0, the user does not want to use the soil.ini file.
      SOILINIFLAG = 0

!> PRE-EMPTIONFLAG FLAG - DEFAULT = NO PRE-EMPTION
      PREEMPTIONFLAG = 0

!> OBJECTIVE FUNCTION FLAG - DEFAULT = SAE - SUM OF ABSOLUTE VALUE OF ERRORS
      OBJFNFLAG = 0

!> AUTOCALIBRATION FLAG - DEFAULT = NO AUTOCALIBRATION
      AUTOCALIBRATIONFLAG = 0
      WINDOWSIZEFLAG = 1
      WINDOWSPACINGFLAG = 1

!> If FROZENSOILINFILFLAG is 0, all snow melt infiltrates.
!> If FROZENSOILINFILFLAG is 1, snow melt is partitioned to frozen soil infiltration 
!> and direct runoff based on the parameteric equation developed by Gray et al, 2001.
      FROZENSOILINFILFLAG = 0

!> FORCIND DATA INTERPOLATION AT INTERMEDIATE TIME STEPS (WHEN THE TIME 
!> INTERVAL OF THE FORCING DATA IS GREATER THAN 30 MINUTE) 
!> DEFAULT = NO INTERPOLATION
      INTERPOLATIONFLAG = 0      

!* If STREAMFLOWFLAG is 1, the user wants to output streamflow values for each 
!* timestep. If STREAMFLOWFLAG is 0, the user will get the default daily file.
      STREAMFLOWFLAG = 0

!* If SUBBASINFLAG is 1, calculations will only be done for grid squares that are 
!* in the watersheds of the locations listed in the streamflow files. 
!* If SUBBASINFLAG is 0, calculations will be made for all grid squares.
      SUBBASINFLAG = 0

!* If TESTCSVFLAG is 1, proper distribution of csv forcing data will be checked. 
      TESTCSVFLAG = 0

!* If R2COUTPUTFLAG is 1, R2C ascii file will be written for user specified 
!* variables.
!* If R2COUTPUTFLAG is 2, R2C binary will be written for user specified 
!* variables (list of variables will be read from r2c_output.txt file).
 
      R2COUTPUTFLAG = 0

!* If FROZENSOILINFILFLAG is 0, all snow melt infiltrates.
!* If FROZENSOILINFILFLAG is 1, snow melt is partitioned to frozen soil infiltration 
!* and direct runoff based on the parameteric equation developed by Gray et al, 2001.
      FROZENSOILINFILFLAG = 0

!> SET N = 0 RESETS THE CLASS COUNTER.
!TODO: N is not a flag, move it somewhere else
      N = 0

!> The above parameter values are defaults, to change to a different
!> value, use the MESH_input_run_options.ini file


!todo make this more clear for the user
!todo at the top, make a brief discription about the changes between
!todo  the different versions

!> *********************************************************************
!> Open and read in values from MESH_input_run_options.ini file
!> *********************************************************************

      OPEN (53, FILE="MESH_input_run_options.ini", STATUS="OLD",
     1  IOSTAT=IOS)
      IF (IOS .NE. 0) THEN !CHECK FILE FOR IOSTAT ERRORS
        WRITE (6, *)
        WRITE (6, *)
        WRITE (6, *) "MESH_input_run_options.ini ",
     1      "could not be opened.  Ensure that the file exists and ",
     2      "restart the program."
        STOP
      ELSE
        WRITE (6, '(A)', ADVANCE="NO"),
     +   "READING: MESH_input_run_options.ini"
      END IF

      DO I=1,3
        READ(53,*)
      ENDDO

      READ(53,"(I5)") CONFLAGS

!> Set flag values based on given input
      IF(CONFLAGS>0) THEN
        DO I=1,CONFLAGS
          READ(53,"(A20, I4)") IRONAME, IROVAL
          IF (IRONAME == "IDISP") THEN
            IDISP = IROVAL
          ELSE IF (IRONAME == "IZREF") THEN
            IZREF = IROVAL
          ELSE IF (IRONAME == "ISLFD") THEN
            ISLFD = IROVAL
          ELSE IF (IRONAME == "IPCP") THEN
            IPCP = IROVAL
          ELSE IF (IRONAME == "ITC") THEN
            ITC = IROVAL
          ELSE IF (IRONAME == "ITCG") THEN
            ITCG = IROVAL
          ELSE IF (IRONAME == "ITG") THEN
            ITG = IROVAL
          ELSE IF (IRONAME == "IWF") THEN
            IWF = IROVAL
          ELSE IF (IRONAME == "IPAI") THEN
            IPAI = IROVAL
          ELSE IF (IRONAME == "IHGT") THEN
            IHGT = IROVAL
          ELSE IF (IRONAME == "IALC") THEN
            IALC = IROVAL
          ELSE IF (IRONAME == "IALS") THEN
            IALS = IROVAL
          ELSE IF (IRONAME == "IALG") THEN
            IALG = IROVAL
          ELSE IF (IRONAME == "RESUMEFLAG") THEN
            RESUMEFLAG = IROVAL
          ELSE IF (IRONAME == "SAVERESUMEFLAG") THEN
            SAVERESUMEFLAG = IROVAL
          ELSE IF (IRONAME == "HOURLYFLAG") THEN
            HOURLYFLAG = IROVAL
          ELSE IF (IRONAME == "RELFLG") THEN
            RELFLG = IROVAL
          ELSE IF (IRONAME == "BASINSHORTWAVEFLAG") THEN
            BASINSHORTWAVEFLAG = IROVAL
          ELSE IF (IRONAME == "BASINLONGWAVEFLAG") THEN
            BASINLONGWAVEFLAG = IROVAL
          ELSE IF (IRONAME == "BASINRAINFLAG") THEN
            BASINRAINFLAG = IROVAL
          ELSE IF (IRONAME == "BASINTEMPERATUREFLAG") THEN
            BASINTEMPERATUREFLAG = IROVAL
          ELSE IF (IRONAME == "BASINWINDFLAG") THEN
            BASINWINDFLAG = IROVAL
          ELSE IF (IRONAME == "BASINPRESFLAG") THEN
            BASINPRESFLAG = IROVAL
          ELSE IF (IRONAME == "BASINHUMIDITYFLAG") THEN
            BASINHUMIDITYFLAG = IROVAL
          ELSE IF (IRONAME == "SHDFILEFLAG") THEN
            SHDFILEFLAG = IROVAL
          ELSE IF (IRONAME == "SOILINIFLAG") THEN
            SOILINIFLAG = IROVAL
          ELSE IF (IRONAME == "STREAMFLOWFLAG") THEN
            STREAMFLOWFLAG = IROVAL
          ELSE IF (IRONAME == "PREEMPTIONFLAG") THEN
            PREEMPTIONFLAG = IROVAL
          ELSE IF (IRONAME == "INTERPOLATIONFLAG") THEN
            INTERPOLATIONFLAG = IROVAL
          ELSE IF (IRONAME == "SUBBASINFLAG") THEN
            SUBBASINFLAG = IROVAL
          ELSE IF (IRONAME == "TESTCSVFLAG") THEN
            TESTCSVFLAG = IROVAL
          ELSE IF (IRONAME == "R2COUTPUTFLAG") THEN
            R2COUTPUTFLAG = IROVAL
          ELSE IF (IRONAME == "OBJFNFLAG") THEN
            OBJFNFLAG = IROVAL
          ELSE IF (IRONAME == "AUTOCALIBRATIONFLAG") THEN
            AUTOCALIBRATIONFLAG = IROVAL                        
          ELSE IF (IRONAME == "WINDOWSIZEFLAG") THEN
            WINDOWSIZEFLAG = IROVAL                        
          ELSE IF (IRONAME == "WINDOWSPACINGFLAG") THEN
            WINDOWSPACINGFLAG = IROVAL
          ELSE IF (IRONAME == "FROZENSOILINFILFLAG") THEN
            FROZENSOILINFILFLAG = IROVAL                        
          ELSE
            !> Error when reading the input file
            WRITE(6, *) "The flag '", IRONAME, "' was found in the",
     +          " run options file. This program does not recognise",
     +          " that flag. Please make sure you have the correct",
     +          " flag name."
            STOP
          END IF
        ENDDO
      ENDIF

      DO I=1,2
        READ(53,*)
      ENDDO

      READ (53, "(I5)") WF_NUM_POINTS !> READ GRID OUTPUT POINTS
!todo prompt the user, asking them if they want to continue.
      IF (WF_NUM_POINTS .GT. 10) THEN
        WRITE (6, *) "WARNING: The number of grid output points is ",
     +     "greater than ten. This may cause performance or ",
     +     "stability issues."
      END IF

      READ (53, *) !BLANK LINES: FIELD HEADER

!> DIMENSION GRID OUTPUT POINT VARIABLES
      IF (WF_NUM_POINTS .GT. 0) THEN
        ALLOCATE (op%DIR_OUT(WF_NUM_POINTS), op%N_OUT(WF_NUM_POINTS),
     1      op%II_OUT(WF_NUM_POINTS), STAT=PAS)
        IF (PAS. NE. 0) THEN
            WRITE (6, *)
            WRITE (6, *)
            WRITE (6, *) "Error allocating grid output point ",
     1          "variables.  Check that these bounds are within an ",
     1          "acceptable range."
            WRITE (6, *) "Bound 1 (grid output points): ", WF_NUM_POINTS
            CLOSE (53)
            STOP
        END IF
        READ (53, "(5I10)") (op%N_OUT(I), I=1, WF_NUM_POINTS)
        READ (53, "(5I10)") (op%II_OUT(I), I=1, WF_NUM_POINTS)
        READ (53, "(5A10)") (op%DIR_OUT(I), I=1, WF_NUM_POINTS)
      ELSE !FOR GENERAL PURPOSES
        READ(53,*)
        READ(53,*)
        READ(53,*)
        ALLOCATE (op%DIR_OUT(1), op%N_OUT(1), op%II_OUT(1))
      END IF !(WF_NUM_POINTS .GT. 0)

      DO I = 1, WF_NUM_POINTS
        IF (I .LT. WF_NUM_POINTS) THEN !IF IS REPEATED
          DO J = I + 1, WF_NUM_POINTS
            IF (op%N_OUT(I) == op%N_OUT(J) .AND.
     1        op%II_OUT(I) == op%II_OUT(J)) THEN
              WRITE (6, *)
              WRITE (6, *)
	        WRITE (6, *) "Output for Grid ", op%N_OUT(I), " and GRU ",
     1          op%II_OUT(I)
	        WRITE (6, *) "is repeated in grid output point ", J, "."
              WRITE (6, *) "Please adjust this grid output ",
     1          "point in MESH_input_run_options.ini"
	        STOP
	      END IF
          END DO
        ELSE !> IF DIRECTORY EXISTS
          OPEN (17, FILE="./"// TRIM (ADJUSTL (op%DIR_OUT(I)))//
     1      "/fort.17", STATUS="UNKNOWN", IOSTAT=IOS)
          IF (IOS .NE. 0) THEN !TRY WRITING DUMMY FILE TO DIRECTORY
            WRITE (6, *)
            WRITE (6, *)
            WRITE (6, *) "Grid output point ", I
            WRITE (6, *) "The output directory does not exist: ",
     1        TRIM (ADJUSTL (op%DIR_OUT(I)))
            WRITE (6, *) "Please adjust this grid output point ",
     1        "in MESH_input_run_options.ini or create the ",
     2        "folder."
            STOP
          ELSE
            CLOSE (17, STATUS="DELETE") !DELETE DUMMY FILE
          END IF
	  END IF
	END DO

!> This is the directory to output the mesh_output* files and the basin_swe/sca files
      READ(53,*)
      READ(53,*)
      READ(53,'(A10)') GENDIR_OUT

!todo describe these !P comments better. For now, they mean nothing.
!P
      !This section is used to start part way through the bin file
      READ(53,*) !P
      READ(53,*) !P
      READ(53,'(I4, I4, I4, I4)') IYEAR_START, IDAY_START, !P
     +  IHOUR_START, IMIN_START !P
      READ(53,'(I4, I4, I4, I4)') IYEAR_END, IDAY_END, !P
     +  IHOUR_END, IMIN_END !P

      CLOSE(UNIT=53)
      WRITE (6, FMT=*) " READ: SUCCESSFUL, FILE: CLOSED"

      RETURN
      END
