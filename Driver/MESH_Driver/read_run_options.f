      SUBROUTINE READ_RUN_OPTIONS(
!     +  IDISP, IZREF, ISLFD, IPCP, IWF,
!     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
!     +  ICTEMMOD,
!     +  IOS, PAS, N,
!     +  IROVAL,
!     +  WF_NUM_POINTS,
!     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START,
!     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END,
!     +  IRONAME,
!-     +  GENDIR_OUT,
!     +  op,
     +  ts, cm, fls)

      use module_mpi_flags

      use sa_mesh_shared_variables
      USE MESH_INPUT_MODULE
      USE strings

      use model_files_variabletypes
      use model_files_variables

      USE FLAGS
      USE climate_forcing
      USE model_dates

      use SIMSTATS_config, only: mtsflg

      use process_CLASS_constants
      use process_CLASS_save_output

      use process_SA_RTE, only: SA_RTE_flgs

      IMPLICIT NONE

!> the following variables were passed in from the subroutine call
      INTEGER ::
!     +  IDISP, IZREF, ISLFD, IPCP, IWF,
!     +  IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG,
!     +  ICTEMMOD,
!     +  IOS, PAS, N,
!     +  IROVAL,
!     +  WF_NUM_POINTS,
     +  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START, !P
     +  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END !P

      INTEGER, PARAMETER :: MaxLenField=20, MaxArgs=20, MaxLenLine=100
      CHARACTER delim*1, in_line*(MaxLenLine)
      INTEGER nargs
      CHARACTER(MaxLenField), DIMENSION(MaxArgs) :: out_args

      INTEGER:: M_G  ! PARAMETER :: M_G = 5
!      CHARACTER(20) :: IRONAME
!-      CHARACTER*10 GENDIR_OUT

      !> Local variables.
      character(20) IRONAME
      character(10) GENDIR_OUT

!      TYPE(OutputPoints) :: op

      TYPE(CLIM_INFO) :: cm
      type(dates_model) :: ts

      !file handled
      type(fl_ids)              :: fls

!> The following variables are all local to this subroutine
      integer CONFLAGS, IROVAL, iun, ierr, j, i

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
!-      BASINSHORTWAVEFLAG = 0
!-      BASINLONGWAVEFLAG = 0
!-      BASINRAINFLAG = 0
!-      BASINTEMPERATUREFLAG = 0
!-      BASINWINDFLAG = 0
!-      BASINPRESFLAG = 0
!-      BASINHUMIDITYFLAG = 0

!> SOIL INITIALIZATION  FLAG - DEFAULT = STOP SIMULATION IF SUM OF SOIL PERCENTAGES EXCEEDS 100%
!> If SOILINIFLAG is 0, stop simulation if the sum of soil percentages is greater than 100%
!> If SOILINIFLAG is 1, no adjustment to soil percentages even if the sum is greater than 100%
!> If SOILINIFLAG is 2, adjust soil percentages in favor of sand
!> If SOILINIFLAG is 3, adjust soil percentages in favor of clay
!> If SOILINIFLAG is 4, adjust soil percentages proportionally
!> If SOILINIFLAG is 5, directly read soil parameter values from soil.ini file.
      SOILINIFLAG = 0

!> If OBJFNFLAG is 0 {DEFAULT} = SAE - SUM OF ABSOLUTE VALUE OF ERRORS
!> If OBJFNFLAG is 1, SAESRT - SUM OF ABSOLUTE VALUE OF ERRORS AFTER SORTING
!> If OBJFNFLAG is 2, SAEMSRT - SUM OF ABSOLUTE VALUE OF MOVING ERRORS AFTER SORTING 
!> If OBJFNFLAG is 3, NSE - MEAN NASH-SUTCLIFFE MODEL EFFICIENCY INDEX (+ve FOR MAXIMIZATION)
!> IF OBJFNFLAG is 4, NSE - MEAN NASH-SUTFLIFFE MODEL EFFICIENCY INDEX (-ve FOR MINIMIZATION)
      OBJFNFLAG = 0

      WINDOWSIZEFLAG = 1
      WINDOWSPACINGFLAG = 1

      METRICSSTATSOUTFLAG = 1
      METRICSFILTEROBSFLAG = 1

!> METRICSSPINUP specifies the starting day from which to calculate metrics.
!> The starting day is relative to the beginning of the simulation; Day 1 is
!> the first day of the simulation, regardless of the date or its Julian date
!> in the year. If METRICSINCLUDESPINUP is set to 1, METRICSSPINUP is not used.
      METRICSSPINUP = 1

!> If METRICSINCLUDESPINUP is set to 1 then metrics are calculated from the
!> first day of the simulation (1:ndsim).
!> If METRICSINCLUDESPINUP is set to 0 then metrics are calculated from
!> METRICSSPINUP (METRICSSPINUP:ndsim).
      METRICSINCLUDESPINUP = 0

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
!      N = 0

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

      iun = fls%fl(mfk%f53)%iun
      open(iun,
     +     file=trim(adjustl(fls%fl(mfk%f53)%fn)),
     +     action='read',
     +     status='old', iostat=ierr)

      IF (ierr .NE. 0) THEN !CHECK FILE FOR IOSTAT ERRORS
        WRITE (6, *)
        WRITE (6, *)
        WRITE (6, *) "MESH_input_run_options.ini ",
     1      "could not be opened.  Ensure that the file exists and ",
     2      "restart the program."
        STOP
      ELSEIF (ro%VERBOSEMODE > 0) THEN
        WRITE (6, '(A)', ADVANCE="NO"),
     +   "READING: MESH_input_run_options.ini"
      END IF

      DO I=1,3
        READ(iun,*)
      ENDDO

      READ(iun,"(I5)") CONFLAGS

      !> Read and parse the control flags.
      IF (CONFLAGS > 0) THEN

        !> Control flags are parsed by space.
        delim = ' '
        DO I=1,CONFLAGS

          !> Read and parse the entire line.
          READ(iun,'(A)') in_line
          CALL parse(in_line, delim, out_args, nargs)
          IF (.NOT. nargs > 0) THEN
            PRINT 9358, I
            STOP
          END IF

          !> Determine the control flag and parse additional arguments.
          SELECT CASE (trim(adjustl(out_args(1))))

!>    ******************************************************************
!>     PARSE CONTROL FLAG BEGINS
!>    ******************************************************************

            CASE ('IDISP')
              CALL value(out_args(2), IDISP, ierr)
            CASE ('IZREF')
              CALL value(out_args(2), IZREF, ierr)
            CASE ('ISLFD')
              CALL value(out_args(2), ISLFD, ierr)
            CASE ('IPCP')
              CALL value(out_args(2), IPCP, ierr)
            CASE ('ITC')
              CALL value(out_args(2), ITC, ierr)
            CASE ('ITCG')
              CALL value(out_args(2), ITCG, ierr)
            CASE ('ITG')
              CALL value(out_args(2), ITG, ierr)
            CASE ('IWF')
              CALL value(out_args(2), IWF, ierr)
            CASE ('IPAI')
              CALL value(out_args(2), IPAI, ierr)
            CASE ('IHGT')
              CALL value(out_args(2), IHGT, ierr)
            CASE ('IALC')
              CALL value(out_args(2), IALC, ierr)
            CASE ('IALS')
              CALL value(out_args(2), IALS, ierr)
            CASE ('IALG')
              CALL value(out_args(2), IALG, ierr)
            CASE ('RESUMEFLAG')
              CALL value(out_args(2), RESUMEFLAG, ierr)
            CASE ('SAVERESUMEFLAG')
              CALL value(out_args(2), SAVERESUMEFLAG, ierr)

            !> Basin forcing time-step flag.
            CASE ('HOURLYFLAG')
              CALL value(out_args(2), HOURLYFLAG, ierr)
              IF (ierr == 0) THEN
                DO j = 1,size(cm%clin)
                  cm%clin(j)%hf = HOURLYFLAG
                END DO
              END IF

            !> Model time-step.
            CASE ('TIMESTEPFLAG')
              CALL value(out_args(2), TIME_STEP_MINS, ierr)

            CASE ('RELFLG')
              CALL value(out_args(2), RELFLG, ierr)

      !> CONSOLE OUTPUT OPTIONS
            CASE ('VERBOSEMODE')
              CALL value(out_args(2), ro%VERBOSEMODE, ierr)
            case ('DIAGNOSEMODE')
              call value(out_args(2), ro%DIAGNOSEMODE, ierr)

      !> MPI OPTIONS
            CASE ('MPIUSEBARRIER')
              CALL value(out_args(2), MPIUSEBARRIER, ierr)

      !> BASIN FORCING DATA OPTIONS
            !> Basin forcing data.
            CASE ('BASINSHORTWAVEFLAG')
              CALL value(out_args(2), cm%clin(cfk%FB)%filefmt, ierr)
              cm%clin(cfk%FB)%id_var = 'FB'
              IF (cm%clin(cfk%FB)%filefmt == 5) THEN
                CALL value(out_args(3), cm%clin(cfk%FB)%filefmt, ierr)
                CALL value(out_args(4), cm%clin(cfk%FB)%timeSize, ierr)
              END IF
              DO j = 3,nargs
                IF (len_trim(out_args(j)) > 3) THEN
                  IF (out_args(j)(1:3) == 'hf=') THEN
                    CALL value(out_args(j)(4:),
     +                         cm%clin(cfk%FB)%hf, ierr)
                  END IF
                END IF
                IF (len_trim(out_args(j)) > 4) THEN
                  IF (out_args(j)(1:4) == 'nts=') THEN
                    CALL value(out_args(j)(5:),
     +                         cm%clin(cfk%FB)%timeSize, ierr)
                  END IF
                END IF
              END DO
            CASE ('BASINLONGWAVEFLAG')
              CALL value(out_args(2), cm%clin(cfk%FI)%filefmt, ierr)
              cm%clin(cfk%FI)%id_var = 'FI'
              IF (cm%clin(cfk%FI)%filefmt == 5) THEN
                CALL value(out_args(3), cm%clin(cfk%FI)%filefmt, ierr)
                CALL value(out_args(4), cm%clin(cfk%FI)%timeSize, ierr)
              END IF
              DO j = 3,nargs
                IF (len_trim(out_args(j)) > 3) THEN
                  IF (out_args(j)(1:3) == 'hf=') THEN
                    CALL value(out_args(j)(4:),
     +                         cm%clin(cfk%FI)%hf, ierr)
                  END IF
                END IF
                IF (len_trim(out_args(j)) > 4) THEN
                  IF (out_args(j)(1:4) == 'nts=') THEN
                    CALL value(out_args(j)(5:),
     +                         cm%clin(cfk%FI)%timeSize, ierr)
                  END IF
                END IF
              END DO
            CASE ('BASINRAINFLAG')
              CALL value(out_args(2), cm%clin(cfk%PR)%filefmt, ierr)
              cm%clin(cfk%PR)%id_var = 'PR'
              IF (cm%clin(cfk%PR)%filefmt == 5) THEN
                CALL value(out_args(3), cm%clin(cfk%PR)%filefmt, ierr)
                CALL value(out_args(4), cm%clin(cfk%PR)%timeSize, ierr)
              END IF
              DO j = 3,nargs
                IF (len_trim(out_args(j)) > 3) THEN
                  IF (out_args(j)(1:3) == 'hf=') THEN
                    CALL value(out_args(j)(4:),
     +                         cm%clin(cfk%PR)%hf, ierr)
                  END IF
                END IF
                IF (len_trim(out_args(j)) > 4) THEN
                  IF (out_args(j)(1:4) == 'nts=') THEN
                    CALL value(out_args(j)(5:),
     +                         cm%clin(cfk%PR)%timeSize, ierr)
                  END IF
                END IF
              END DO
            CASE ('BASINTEMPERATUREFLAG')
              CALL value(out_args(2), cm%clin(cfk%TT)%filefmt, ierr)
              cm%clin(cfk%TT)%id_var = 'TT'
              IF (cm%clin(cfk%TT)%filefmt == 5) THEN
                CALL value(out_args(3), cm%clin(cfk%TT)%filefmt, ierr)
                CALL value(out_args(4), cm%clin(cfk%TT)%timeSize, ierr)
              END IF
              DO j = 3,nargs
                IF (len_trim(out_args(j)) > 3) THEN
                  IF (out_args(j)(1:3) == 'hf=') THEN
                    CALL value(out_args(j)(4:),
     +                         cm%clin(cfk%TT)%hf, ierr)
                  END IF
                END IF
                IF (len_trim(out_args(j)) > 4) THEN
                  IF (out_args(j)(1:4) == 'nts=') THEN
                    CALL value(out_args(j)(5:),
     +                         cm%clin(cfk%TT)%timeSize, ierr)
                  END IF
                END IF
              END DO
            CASE ('BASINWINDFLAG')
              CALL value(out_args(2), cm%clin(cfk%UV)%filefmt, ierr)
              cm%clin(cfk%UV)%id_var = 'UV'
              IF (cm%clin(cfk%UV)%filefmt == 5) THEN
                CALL value(out_args(3), cm%clin(cfk%UV)%filefmt, ierr)
                CALL value(out_args(4), cm%clin(cfk%UV)%timeSize, ierr)
              END IF
              DO j = 3,nargs
                IF (len_trim(out_args(j)) > 3) THEN
                  IF (out_args(j)(1:3) == 'hf=') THEN
                    CALL value(out_args(j)(4:),
     +                         cm%clin(cfk%UV)%hf, ierr)
                  END IF
                END IF
                IF (len_trim(out_args(j)) > 4) THEN
                  IF (out_args(j)(1:4) == 'nts=') THEN
                    CALL value(out_args(j)(5:),
     +                         cm%clin(cfk%UV)%timeSize, ierr)
                  END IF
                END IF
              END DO
            CASE ('BASINPRESFLAG')
              CALL value(out_args(2), cm%clin(cfk%P0)%filefmt, ierr)
              cm%clin(cfk%P0)%id_var = 'P0'
              IF (cm%clin(cfk%P0)%filefmt == 5) THEN
                CALL value(out_args(3), cm%clin(cfk%P0)%filefmt, ierr)
                CALL value(out_args(4), cm%clin(cfk%P0)%timeSize, ierr)
              END IF
              DO j = 3,nargs
                IF (len_trim(out_args(j)) > 3) THEN
                  IF (out_args(j)(1:3) == 'hf=') THEN
                    CALL value(out_args(j)(4:),
     +                         cm%clin(cfk%P0)%hf, ierr)
                  END IF
                END IF
                IF (len_trim(out_args(j)) > 4) THEN
                  IF (out_args(j)(1:4) == 'nts=') THEN
                    CALL value(out_args(j)(5:),
     +                         cm%clin(cfk%P0)%timeSize, ierr)
                  END IF
                END IF
              END DO
            CASE ('BASINHUMIDITYFLAG')
              CALL value(out_args(2), cm%clin(cfk%HU)%filefmt, ierr)
              cm%clin(cfk%HU)%id_var = 'HU'
              IF (cm%clin(cfk%HU)%filefmt == 5) THEN
                CALL value(out_args(3), cm%clin(cfk%HU)%filefmt, ierr)
                CALL value(out_args(4), cm%clin(cfk%HU)%timeSize, ierr)
              END IF
              DO j = 3,nargs
                IF (len_trim(out_args(j)) > 3) THEN
                  IF (out_args(j)(1:3) == 'hf=') THEN
                    CALL value(out_args(j)(4:),
     +                         cm%clin(cfk%HU)%hf, ierr)
                  END IF
                END IF
                IF (len_trim(out_args(j)) > 4) THEN
                  IF (out_args(j)(1:4) == 'nts=') THEN
                    CALL value(out_args(j)(5:),
     +                         cm%clin(cfk%HU)%timeSize, ierr)
                  END IF
                END IF
              END DO

            CASE ('SHDFILEFLAG')
              CALL value(out_args(2), SHDFILEFLAG, ierr)
            CASE ('SOILINIFLAG')
              CALL value(out_args(2), SOILINIFLAG, ierr)
            CASE ('NRSOILAYEREADFLAG')
              CALL value(out_args(2), NRSOILAYEREADFLAG, ierr)
            CASE ('STREAMFLOWFLAG')
              CALL value(out_args(2), STREAMFLOWFLAG, ierr)
            CASE ('PREEMPTIONFLAG')
              CALL value(out_args(2), mtsflg%PREEMPTIONFLAG, ierr)
            CASE ('INTERPOLATIONFLAG')
              CALL value(out_args(2), INTERPOLATIONFLAG, ierr)
            CASE ('SUBBASINFLAG')
              CALL value(out_args(2), SUBBASINFLAG, ierr)
            CASE ('R2COUTPUTFLAG')
              CALL value(out_args(2), R2COUTPUTFLAG, ierr)
            CASE ('OBJFNFLAG')
              CALL value(out_args(2), OBJFNFLAG, ierr)
            CASE ('AUTOCALIBRATIONFLAG')
              CALL value(out_args(2), mtsflg%AUTOCALIBRATIONFLAG, ierr)
            CASE ('WINDOWSIZEFLAG')
              CALL value(out_args(2), WINDOWSIZEFLAG, ierr)
            CASE ('WINDOWSPACINGFLAG')
              CALL value(out_args(2), WINDOWSPACINGFLAG, ierr)
            CASE ('METRICSSTATSOUTFLAG')
              CALL value(out_args(2), METRICSSTATSOUTFLAG, ierr)
            CASE ('METRICSFILTEROBSFLAG')
              CALL value(out_args(2), METRICSFILTEROBSFLAG, ierr)
            CASE ('METRICSSPINUP')
              CALL value(out_args(2), METRICSSPINUP, ierr)
            CASE ('METRICSINCLUDESPINUP')
              CALL value(out_args(2), METRICSINCLUDESPINUP, ierr)
            CASE ('FROZENSOILINFILFLAG')
              CALL value(out_args(2), FROZENSOILINFILFLAG, ierr)
            case ('PRINTRFFR2CFILEFLAG')
              call value(out_args(2),
     +                   SA_RTE_flgs%PRINTRFFR2CFILEFLAG, ierr)
              SA_RTE_flgs%PROCESS_ACTIVE = .true.
            case ('PRINTRCHR2CFILEFLAG')
              call value(out_args(2),
     +                   SA_RTE_flgs%PRINTRCHR2CFILEFLAG, ierr)
              SA_RTE_flgs%PROCESS_ACTIVE = .true.
!+            case ('PRINTLKGR2CFILEFLAG')
!+              call value(out_args(2),
!+    +                    SA_RTE_flgs%PRINTLKGR2CFILEFLAG, ierr)
!+              SA_RTE_flgs%PROCESS_ACTIVE = .true.
            CASE ('WD3')
              CALL value(out_args(2), WD3, ierr)
            CASE ('WD3NEWFILE')
              CALL value(out_args(2), WD3NEWFILE, ierr)
            CASE ('WD3FLOW')
              CALL value(out_args(2), WD3FLOW, ierr)
            CASE ('WD3BKFC')
              CALL value(out_args(2), WD3BKFC, ierr)
            CASE ('ICTEMMOD')
              CALL value(out_args(2), ICTEMMOD, ierr)
            CASE ('PBSMFLAG')
              CALL value(out_args(2), PBSMFLAG, ierr)
            CASE ('LOCATIONFLAG')
              CALL value(out_args(2), LOCATIONFLAG, ierr)
            CASE ('OUTFIELDSFLAG')
              CALL value(out_args(2), OUTFIELDSFLAG, ierr)
            CASE ('GGEOFLAG')
              CALL value(out_args(2), GGEOFLAG, ierr)
            CASE ('BASINBALANCEOUTFLAG')
              CALL value(out_args(2), BASINBALANCEOUTFLAG, ierr)
            CASE ('MODELINFOOUTFLAG')
              CALL value(out_args(2), MODELINFOOUTFLAG, ierr)
            CASE ('STREAMFLOWOUTFLAG')
              CALL value(out_args(2), STREAMFLOWOUTFLAG, ierr)
            CASE ('BASINSWEOUTFLAG')
              CALL value(out_args(2), BASINSWEOUTFLAG, ierr)

!>    ******************************************************************
!>     PARSE CONTROL FLAG ENDS
!>    ******************************************************************

            !> Unrecognized flag.
            CASE DEFAULT
              PRINT 9373, trim(out_args(1))
              STOP

          END SELECT !CASE (trim(adjustl(out_args(1))))

          !> Error check.
          IF (ierr /= 0) THEN
            PRINT 9484, trim(out_args(1)), I, trim(in_line)
            STOP
          END IF

        END DO !I=1,CONFLAGS
      END IF !(CONFLAGS > 0) THEN

9373  FORMAT(/1X "WARNING: An unrecognized flag '" (A)
     +  "' was found in the run options file."
     +  /1X
     +  'The flag may not be supported by this version of the model.'
     +  /1X 'Please ensure that the flag is properly named.')
9358  FORMAT(/1X 'WARNING: An error occurred parsing Control Flag #' I5
     +  /1X 'The flag or its arguments could not be parsed.')
9484  FORMAT(/1X "WARNING: An error occurred parsing '" (A) "'."
     +  /1X 'Flag #' I5 ': ' (A))

      DO I=1,2
        READ(iun,*)
      ENDDO

      READ (iun, "(I5)") WF_NUM_POINTS !> READ GRID OUTPUT POINTS
!todo prompt the user, asking them if they want to continue.
      IF (WF_NUM_POINTS .GT. 10) THEN
        WRITE (6, *) "WARNING: The number of grid output points is ",
     +     "greater than ten. This may cause performance or ",
     +     "stability issues."
      END IF


      READ (iun, *) !BLANK LINES: FIELD HEADER

!> DIMENSION GRID OUTPUT POINT VARIABLES
      IF (WF_NUM_POINTS .GT. 0) THEN
        ALLOCATE(op%DIR_OUT(WF_NUM_POINTS), op%N_OUT(WF_NUM_POINTS),
     +           op%II_OUT(WF_NUM_POINTS), op%K_OUT(WF_NUM_POINTS),
     +           STAT=ierr)
        IF (ierr. NE. 0) THEN
            WRITE (6, *)
            WRITE (6, *)
            WRITE (6, *) "Error allocating grid output point ",
     1          "variables.  Check that these bounds are within an ",
     1          "acceptable range."
            WRITE (6, *) "Bound 1 (grid output points): ", WF_NUM_POINTS
            CLOSE (iun)
            STOP
        END IF
        READ (iun, "(5I10)") (op%N_OUT(I), I=1, WF_NUM_POINTS)
        READ (iun, "(5I10)") (op%II_OUT(I), I=1, WF_NUM_POINTS)
        READ (iun, "(5A10)") (op%DIR_OUT(I), I=1, WF_NUM_POINTS)
      ELSE !FOR GENERAL PURPOSES
        READ(iun,*)
        READ(iun,*)
        READ(iun,*)
        ALLOCATE (op%DIR_OUT(1), op%N_OUT(1), op%II_OUT(1), op%K_OUT(1))
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
     1      "/fort.17", STATUS="UNKNOWN", IOSTAT=ierr)
          IF (ierr .NE. 0) THEN !TRY WRITING DUMMY FILE TO DIRECTORY
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
      read(iun, *)
      read(iun, *)
      read(iun, '(a10)') GENDIR_OUT
      call removesp(GENDIR_OUT)
      fls%GENDIR_OUT = adjustl(GENDIR_OUT)

!todo describe these !P comments better. For now, they mean nothing.
!P
      !This section is used to start part way through the bin file
      READ(iun,*) !P
      READ(iun,*) !P
      READ(iun, '(4I4)') YEAR_START, JDAY_START, HOUR_START, MINS_START
      READ(iun, '(4I4)') YEAR_STOP, JDAY_STOP, HOUR_STOP, MINS_STOP

      CALL GET_DATES(ts)

      CLOSE(iun)
      WRITE (6, FMT=*) " READ: SUCCESSFUL, FILE: CLOSED"

      RETURN
      END
