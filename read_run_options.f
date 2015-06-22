       SUBROUTINE READ_RUN_OPTIONS(
     +  IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  ICTEMMOD, IOS, PAS, N,
     +  IROVAL, WF_NUM_POINTS,
     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START,
     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END,
     +  ID, IRONAME, GENDIR_OUT, op, ts, cm, fls)

      USE MESH_INPUT_MODULE
      USE FLAGS
      USE climate_forcing
      USE model_dates
      use model_files

      IMPLICIT NONE

!> the following variables were passed in from the subroutine call
      INTEGER :: IDISP, IZREF, ISLFD, IPCP, IWF,
     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
     +  ICTEMMOD, IOS, PAS, N,
     +  IROVAL, WF_NUM_POINTS,
     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START, !P
     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END !P

      INTEGER :: ID !didn't find the declaration in the driver
      INTEGER:: M_G  ! PARAMETER :: M_G = 5
      CHARACTER(20) :: IRONAME
      CHARACTER*10 GENDIR_OUT
      
      TYPE(OutputPoints) :: op

      TYPE(CLIM_INFO) :: cm
      type(dates_model) :: ts

      !file handled
      type(fl_ids)              :: fls

!> The following variables are all local to this subroutine
      INTEGER :: J,I
      INTEGER :: START_DATE(4),END_DATE(4)

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
!>    * IF IPCP=4, THE RAINFALL, SNOWFALL AND TOTAL PRECIPITATION RATES
!>    * ARE READ IN DIRECTLY.
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
!>    * IF IWF = 1, THE MODIFIED CALCULATIONS OF OVERLAND
!>    * FLOW AND INTERFLOW ARE PERFORMED.
!>    * IF IWF = 2, SAME AS IWF = 0 EXCEPT THAT OVERLAND FLOW IS
!>    * MODELLED AS FILL AND SPILL PROCESS FROM A SERIES OF POTHOLES.
!>    * DEFAULT VALUE IS 1.
      IWF = 1

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

!>    * ICTEMMOD IS SET TO 1 IF CLASS IS BEING RUN IN CONJUNCTION WITH
!>    * THE CANADIAN TERRESTRIAL ECOSYSTEM MODEL "CTEM"; OTHERWISE
!>    * ICTEMMOD IS SET TO 0.
      ICTEMMOD=0

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
!* if RESUMEFLAG is 2, the user wants to run the r2c resume file.
      RESUMEFLAG = 0

!* if SAVERESUMEFLAG is 0, the user doesn't want to make the resume file.
!* if SAVERESUMEFLAG is 1, the user wants to make the resume file.
!* if SAVERESUMEFLAG is 2, the user wants to make the r2c resume file.
!* if SAVERESUMEFLAG is 3, the user wants to make the only class prognostic variables resume file.
      SAVERESUMEFLAG = 0
      
!> FORCING DATA FILES:
!>  0 = read forcing data from .bin file
!>  1 = read forcing data from .r2c
!>  2 = read forcing data from .csv
!>  3 = read forcing data from .seq binary sequential files      
!>  3 = read forcing data from .seq ascii sequential files
!>  5 = read forcing data from load buffer in memory
      BASINSHORTWAVEFLAG = 0
      BASINLONGWAVEFLAG = 0
      BASINRAINFLAG = 0
      BASINTEMPERATUREFLAG = 0
      BASINWINDFLAG = 0
      BASINPRESFLAG = 0
      BASINHUMIDITYFLAG = 0

!> SOIL INITIALIZATION  FLAG - DEFAULT = STOP SIMULATION IF SUM OF SOIL PERCENTAGES EXCEEDS 100%
!> If SOILINIFLAG is 0, stop simulation if the sum of soil percentages is greater than 100%
!> If SOILINIFLAG is 1, no adjustment to soil percentages even if the sum is greater than 100%
!> If SOILINIFLAG is 2, adjust soil percentages in favor of sand
!> If SOILINIFLAG is 3, adjust soil percentages in favor of clay
!> If SOILINIFLAG is 4, adjust soil percentages proportionally
!> If SOILINIFLAG is 5, directly read soil parameter values from soil.ini file.
      SOILINIFLAG = 0

!> PRE-EMPTIONFLAG FLAG - DEFAULT = NO PRE-EMPTION
      PREEMPTIONFLAG = 0

!> If OBJFNFLAG is 0 {DEFAULT} = SAE - SUM OF ABSOLUTE VALUE OF ERRORS
!> If OBJFNFLAG is 1, SAESRT - SUM OF ABSOLUTE VALUE OF ERRORS AFTER SORTING
!> If OBJFNFLAG is 2, SAEMSRT - SUM OF ABSOLUTE VALUE OF MOVING ERRORS AFTER SORTING 
!> If OBJFNFLAG is 3, NSE - MEAN NASH-SUTCLIFFE MODEL EFFICIENCY INDEX (+ve FOR MAXIMIZATION)
!> IF OBJFNFLAG is 4, NSE - MEAN NASH-SUTFLIFFE MODEL EFFICIENCY INDEX (-ve FOR MINIMIZATION)
      OBJFNFLAG = 0

!> AUTOCALIBRATION FLAG - DEFAULT = NO AUTOCALIBRATION
      AUTOCALIBRATIONFLAG = 0
      WINDOWSIZEFLAG = 1
      WINDOWSPACINGFLAG = 1

      METRICSSTATSOUTFLAG = 1
      METRICSFILTEROBSFLAG = 1

!> METRICSSPINUP specifies the starting day from which to calculate metrics.
!> The starting day is relative to the beginning of the simulation; Day 1 is
!> the first day of the simulation, regardless of the date or its Julian date
!> in the year. If METRICSINCLUDESPINUP is set to 1, METRICSSPINUP is not used.
      METRICSSPINUP = 30

!> If METRICSINCLUDESPINUP is set to 1 then metrics are calculated from the
!> first day of the simulation (1:ndsim).
!> If METRICSINCLUDESPINUP is set to 0 then metrics are calculated from
!> METRICSSPINUP (METRICSSPINUP:ndsim).
      METRICSINCLUDESPINUP = 1

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

!* If WD3 is 0, existing WATDRN is used.
!* If WD3 is 1, WATDRN by Ric (May, 2011) is used.
      WD3 = 0

!* If WD3NEWFILE is 0, an existing "soil_out.txt" for MAPLE is used.
!* If WD3NEWFILE is 1, "soil_out.txt" for MAPLE is created or overwritten.
      WD3NEWFILE = 1

!* If WD3FLOW is 0, SUBFLW=SUBFLW,BASFLW=BASFLW.
!* If WD3FLOW is 1, SUBFLW=SUBFLW+BASFLW,BASFLW=0.
!* If WD3FLOW is 2, SUBFLW=SUBFLW,BASFLW=0.
      WD3FLOW = 0

!* If WD3BKFC is 0, BULK_FC (WATROF)=0.
!* If WD3BKFC is 1, BULK_FC remains unchanged in WATROF.
      WD3BKFC = 1

!* set PBSMFLAG = 0 so by default blowing snow calculations are not made
!* 1 =  blowing snow transport, sublimation & inter-GRU redistribution calculations are made
      PBSMFLAG = 0

!* If LOCATIONFLAG is 0, gauge coordinates are read using 2I5 (Minutes) {Default}
!* If LOCATIONFLAG is 1, gauge coordinates for BOTH MESH_input_streamflow.txt AND
!*                       MESH_input_reservoir.txt are read using 2F7.1 (Minutes with 1 decimal)
      LOCATIONFLAG = 0

!> SET N = 0 RESETS THE CLASS COUNTER.
!TODO: N is not a flag, move it somewhere else
      N = 0

!> FLAGS FOR WATROUTE (MODIFIED RPN CODE)
!> Format is PRINT(VARIABLE_TERM)R2CFILEFLAG:
!> If PRINT(VARIABLE_TERM)R2CFILEFLAG is 0, no file is saved.
!> If PRINT(VARIABLE_TERM)R2CFILEFLAG is 1, the file is saved.
      PRINTRFFR2CFILEFLAG = 0
      PRINTRCHR2CFILEFLAG = 0
!      PRINTLKGR2CFILEFLAG = 0

!> FLAGS FOR GEOTHERMAL FLUX FOR THE BOTTOM OF THE LAST SOIL LAYER 
!* If GGEOFLAG is GT 0,  READ UNIQUE VALUE FROM MESH_ggeo.INI FILE
      GGEOFLAG = 0

!> BASIN WATER AND ENERGY BALANCES OUTPUT FLAG
!> If enabled, saves the water and energy balance output files.
!>     0 = Create no output.
!>     1 = Save the basin water and energy balance CSV files.
      BASINBALANCEOUTFLAG = 1

!> BASIN CSV-FORMAT STREAMFLOW OUTPUT FLAG
!> If enabled, saves the observed versus simulated streamflow output
!> file. The flag can also enable the cumulative and every time-step
!> streamflow files written by past model configurations.
!>     0 = Create no output.
!>     1 = Save the observed versus simulated streamflow output file.
!>     2 = Save the observed versus simulated, as well as the
!>         cumulative and every time-step streamflow files.
      STREAMFLOWOUTFLAG = 2

!> BASIN SWE OUTPUT FLAG
!> If enabled, saves the SCA and SWE output files.
!>     0 = Create no output.
!>     1 = Save the SCA and SWE output files.
      BASINSWEOUTFLAG = 1

!> MODEL INFO OUTPUT FLAG
!> If enabled, saves model configuration and run information to the
!> echo_print.txt file.
!>     0 = Create no output.
!>     1 = Save the model configuration and run information to the
!>         echo_print.txt file.
      MODELINFOOUTFLAG = 1

!> The above parameter values are defaults, to change to a different
!> value, use the MESH_input_run_options.ini file

!todo make this more clear for the user
!todo at the top, make a brief discription about the changes between
!todo  the different versions

!> *********************************************************************
!> Open and read in values from MESH_input_run_options.ini file
!> *********************************************************************

      if ((VARIABLEFILESFLAG .eq. 1) .and. (fls%fl(1)%isInit)) then
        open(fls%fl(1)%unit, file=trim(adjustl(fls%fl(1)%name)),
     1  STATUS="OLD", IOSTAT=IOS) 
      else
        OPEN (53, FILE="MESH_input_run_options.ini", STATUS="OLD",
     1  IOSTAT=IOS)
      end if
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
          ELSE IF (IRONAME == "METRICSSTATSOUTFLAG") THEN
            METRICSSTATSOUTFLAG = IROVAL
          ELSE IF (IRONAME == "METRICSFILTEROBSFLAG") THEN
            METRICSFILTEROBSFLAG = IROVAL
          ELSE IF (IRONAME == "METRICSSPINUP") THEN
            METRICSSPINUP = IROVAL
          ELSE IF (IRONAME == "METRICSINCLUDESPINUP") THEN
            METRICSINCLUDESPINUP = IROVAL
          ELSE IF (IRONAME == "FROZENSOILINFILFLAG") THEN
            FROZENSOILINFILFLAG = IROVAL
          ELSE IF (IRONAME == "PRINTRFFR2CFILEFLAG") THEN
            PRINTRFFR2CFILEFLAG = IROVAL
          ELSE IF (IRONAME == "PRINTRCHR2CFILEFLAG") THEN
            PRINTRCHR2CFILEFLAG = IROVAL
!          ELSE IF (IRONAME == "PRINTLKGR2CFILEFLAG") THEN
!            PRINTLKGR2CFILEFLAG = IROVAL
          ELSE IF (IRONAME == "WD3") THEN
            WD3 = IROVAL
          ELSE IF (IRONAME == "WD3NEWFILE") THEN
            WD3NEWFILE = IROVAL
          ELSE IF (IRONAME == "WD3FLOW") THEN
            WD3FLOW = IROVAL
          ELSE IF (IRONAME == "WD3BKFC") THEN
            WD3BKFC = IROVAL
          ELSE IF (IRONAME == "ICTEMMOD") THEN
            ICTEMMOD = IROVAL
          ELSE IF (IRONAME == "PBSMFLAG") THEN
            PBSMFLAG = IROVAL
	      ELSE IF (IRONAME == "LOCATIONFLAG") THEN
	        LOCATIONFLAG = IROVAL
          ELSE IF (IRONAME == "OUTFIELDSFLAG") THEN
            OUTFIELDSFLAG = IROVAL
          ELSE IF (IRONAME == "GGEOFLAG") THEN
            GGEOFLAG = IROVAL
          ELSE IF (IRONAME == "BASINBALANCEOUTFLAG") THEN
            BASINBALANCEOUTFLAG = IROVAL
          ELSE IF (IRONAME == "MODELINFOOUTFLAG") THEN
            MODELINFOOUTFLAG = IROVAL
          ELSE IF (IRONAME == "STREAMFLOWOUTFLAG") THEN
            STREAMFLOWOUTFLAG = IROVAL
          ELSE IF (IRONAME == "BASINSWEOUTFLAG") THEN
            BASINSWEOUTFLAG = IROVAL
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

!> Initialization of model  dates
        start_date(1) = IYEAR_START
        start_date(2) = IDAY_START
        start_date(3) = IHOUR_START
        start_date(4) = IMIN_START

        end_date(1) = IYEAR_END
        end_date(2) = IDAY_END
        end_date(3) = IHOUR_END
        end_date(4) = IMIN_END

        CALL GET_DATES(ts,start_date,end_date,HOURLYFLAG)


!> rewind file to get extra information in the climate forcing data
!> in case that we load a part of the file in memory
      IF ((BASINSHORTWAVEFLAG   .EQ. 5) .OR.
     +    (BASINLONGWAVEFLAG    .EQ. 5) .OR.
     +    (BASINRAINFLAG        .EQ. 5) .OR.
     +    (BASINTEMPERATUREFLAG .EQ. 5) .OR.
     +    (BASINWINDFLAG        .EQ. 5) .OR.
     +    (BASINPRESFLAG        .EQ. 5) .OR.
     +    (BASINHUMIDITYFLAG    .EQ. 5))THEN

        REWIND(UNIT=53)

        DO I=1,4
            READ(53,*)
        ENDDO


        DO I=1,CONFLAGS
          READ(53,"(A20, I4)") IRONAME, IROVAL

          IF (IRONAME .EQ. "BASINSHORTWAVEFLAG") THEN
              IF (IROVAL .EQ. 5) THEN
                  BACKSPACE(53)
                  READ(53,*)IRONAME,cm%clin(1)%flagId,
     +                     cm%clin(1)%flagRead, cm%clin(1)%timeSize
                  cm%clin(1)%id_var = 'SHORTWAVE'
              ENDIF
          ENDIF

          IF (IRONAME .EQ. "BASINLONGWAVEFLAG") THEN
              IF (IROVAL .EQ. 5)THEN
                  BACKSPACE(53)
                  READ(53,*)IRONAME,cm%clin(2)%flagId,
     +                      cm%clin(2)%flagRead,cm%clin(2)%timeSize
                  cm%clin(2)%id_var = 'LONGWAVE'
              ENDIF
          ENDIF

          IF (IRONAME .EQ. "BASINRAINFLAG") THEN
              IF (IROVAL .EQ. 5)THEN
                  BACKSPACE(53)
                  READ(53,*)IRONAME, cm%clin(3)%flagId,
     +                      cm%clin(3)%flagRead, cm%clin(3)%timeSize
                  cm%clin(3)%id_var = 'RAIN'



              ENDIF
          ENDIF

          IF (IRONAME .EQ. "BASINTEMPERATUREFLAG") THEN
              IF (IROVAL .EQ. 5)THEN
                  BACKSPACE(53)
                  READ(53,*)IRONAME,cm%clin(4)%flagId,
     +                      cm%clin(4)%flagRead,cm%clin(4)%timeSize
                  cm%clin(4)%id_var = 'TEMPERATURE'
              ENDIF
          ENDIF

          IF (IRONAME .EQ. "BASINWINDFLAG") THEN
              IF (IROVAL .EQ. 5)THEN
                  BACKSPACE(53)
                  READ(53,*)IRONAME,cm%clin(5)%flagId,
     +                      cm%clin(5)%flagRead, cm%clin(5)%timeSize
                  cm%clin(5)%id_var = 'WIND'
              ENDIF
          ENDIF

          IF (IRONAME .EQ. "BASINPRESFLAG") THEN
              IF (IROVAL .EQ. 5)THEN
                  BACKSPACE(53)
                  READ(53,*)IRONAME,cm%clin(6)%flagId,
     +                      cm%clin(6)%flagRead, cm%clin(6)%timeSize
                  cm%clin(6)%id_var = 'PRESSURE'
              ENDIF
          ENDIF

          IF (IRONAME .EQ. "BASINHUMIDITYFLAG") THEN
              IF (IROVAL .EQ. 5)THEN
                  BACKSPACE(53)
                  READ(53,*)IRONAME,cm%clin(7)%flagId,
     +                      cm%clin(7)%flagRead,cm%clin(7)%timeSize
                  cm%clin(7)%id_var = 'HUMIDITY'
              ENDIF
          ENDIF

        ENDDO

      END IF

      CLOSE(UNIT=53)
      WRITE (6, FMT=*) " READ: SUCCESSFUL, FILE: CLOSED"

      RETURN
      END
