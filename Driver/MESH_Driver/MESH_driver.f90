program RUNMESH

!>       MESH DRIVER
!>
!>       JAN 2013 - K.C.KORNELSEN
!>                - INCORPORATED LOCATION FLAG FOR INCREASING PRECISION
!>                - OF STREAMFLOW AND RESERVOIR INPUTS
!>                - INCLUDED NSE AND NEGATIVE NSE AS OBJFN'S
!>       JAN 2014 - M. MACDONALD.  INCORPORATED BLOWING SNOW ALGORITHMS
!>       AUG 2013 - M. MACDONALD
!>                - INCORPORATE OPTIONAL COUPLING OF CLASS WITH CTEM
!>                - MOVE SOME INITIALIZATION AND SCATTER OF CLASS
!>                  DIAGNOSTIC VARIABLES IN TO MESH_DRIVER
!>       JUN 2010 - F. SEGLENIEKS. 
!>                - ADDED CODE TO HAVE MESH ONLY RUN ON BASINS LISTED IN 
!>                  THE STREAMFLOW FILE, CALLED THE SUBBASIN FEATURE
!>       JUN 2010 - M.A.MEKONNEN/B.DAVIDSON/M.MacDONALD. 
!>                - BUG FIX FOR READING FORCING DATA IN CSV FORMAT 
!>                  WITH 1 HOUR INTERVAL
!>                - READING FORCING DATA WITH VARIOUS TIME STEPS
!>                - FORCING DATA INTERPOLATION TO 30 MINUTE INTERVALS
!>                  (CLASS MODEL TIME STEP)
!>                - PRE-EMPTION OPTION FOR AUTOCALIBRATION
!>                - CHECKING FOR PARAMETER MINIMUM AND MAXIMUM LIMITS
!>                - PATH SPECIFICATION THAT WORKS FOR BOTH WINDOWS AND 
!>                  UNIX SYSTEMS
!>
!>       AUG 2009 - B.DAVISON. CHANGES TO UPDATE TO SA_MESH 1.3
!>       APL 2009 - CLEAN COMMENTS AND REFINE STRUCTURE AFTER CODE REVIEW
!>       FEB 2009 - MESH12-01 BUG FIX AND ADDING NEW FEATURES
!>       AUG 28/07 - F.SEGLENIEKS. CHANGED FILENAMES AND REARRANGED THE CODE
!>       MAY 21/07 - B.DAVISON.    INITIAL VERSION BASED ON WORK OF E.D. SOULIS
!>       AND F. SEGLENIEKS AT THE UNIVERSITY OF WATERLOO
!>
!>=======================================================================
!>       DIMENSION STATEMENTS.
!>
!>       FIRST SET OF DEFINITIONS:
!>       BACKGROUND VARIABLES, AND PROGNOSTIC AND DIAGNOSTIC
!>       VARIABLES NORMALLY PROVIDED BY AND/OR USED BY THE GCM.
!>       THE SUFFIX "ROW" REFERS TO VARIABLES EXISTING ON THE
!>       MOSAIC GRID ON THE CURRENT LATITUDE CIRCLE.  THE SUFFIX
!>       "GAT" REFERS TO THE SAME VARIABLES AFTER THEY HAVE UNDERGONE
!>       A "GATHER" OPERATION IN WHICH THE TWO MOSAIC DIMENSIONS
!>       ARE COLLAPSED INTO ONE.  THE SUFFIX "GRD" REFERS BOTH TO
!>       GRID-CONSTANT INPUT VARIABLES. AND TO GRID-AVERAGED
!>       DIAGNOSTIC VARIABLES.
!>
!>       THE FIRST DIMENSION ELEMENT OF THE "ROW" VARIABLES
!>       REFERS TO THE NUMBER OF GRID CELLS ON THE CURRENT
!>       LATITUDE CIRCLE.  IN THIS STAND-ALONE VERSION, THIS
!>       NUMBER IS ARBITRARILY SET TO THREE, TO ALLOW UP TO THREE
!>       SIMULTANEOUS TESTS TO BE RUN.  THE SECOND DIMENSION
!>       ELEMENT OF THE "ROW" VARIABLES REFERS TO THE MAXIMUM
!>       NUMBER OF TILES IN THE MOSAIC.  IN THIS STAND-ALONE
!>       VERSION, THIS NUMBER IS SET TO EIGHT.  THE FIRST
!>       DIMENSION ELEMENT IN THE "GAT" VARIABLES IS GIVEN BY
!>       THE PRODUCT OF THE FIRST TWO DIMENSION ELEMENTS IN THE
!>       "ROW" VARIABLES.

!> Note, the internal comments are to be organised with 
!> the following symbols:
!>  -the symbols "!>" at the beginning of the line means that the 
!>  following comments are descriptive documentation.
!>  -the symbols "!*" means that the following comment is a variable
!>  definition.
!>  -the symbols "!+" means that the following comment contains code 
!>  that may be useful in the future and should not be deleted.
!>  -the symbols "!-" means that the following comment contains code
!>  that is basically garbage, and can be deleted safely at any time.
!>  -the symbol "!" or any number of exclamation marks can be used
!>  by the developers for various temporary code commenting.
!>  -the symbol "!todo" refers to places where the developers would 
!>  like to work on.
!>  -the symbol "!futuredo" refers to places where the developers
!>  would like to work on with a low priority.

    use sa_mesh_shared_variabletypes
    use sa_mesh_shared_variables

    use EF_MODULE
    use MESH_INPUT_MODULE
    use FLAGS

    use module_mpi_flags
    use module_mpi

    use MODEL_OUTPUT
    use climate_forcing
    use model_dates
    use SIMSTATS_config
    use SIMSTATS
    use model_files_variables
    use model_files
    use strings

    implicit none

    !> ierr: For status return from MPI
    !> istop: To stop all MPI process
    !* inp: Number of active tasks.
    !* ipid: Current process ID.
    integer :: ierr = 0, inp = 1, ipid = 0
    integer ipid_recv, itag, izero, ierrcode, istop
    logical lstat

    integer iun, u, invars

    !+ For split-vector approach
    integer il1, il2, ilen
    integer i1, i2

    integer, dimension(:), allocatable :: irqst
    integer, dimension(:, :), allocatable :: imstat

    type CLASSOUT_VARS
        real, dimension(:), allocatable :: &
            PREACC, GTACC, QEVPACC, EVAPACC, HFSACC, HMFNACC, &
            ROFACC, ROFOACC, ROFSACC, ROFBACC, WTBLACC, ALVSACC, ALIRACC, &
            RHOSACC, TSNOACC, WSNOACC, SNOARE, TCANACC, CANARE, SNOACC, &
            RCANACC, SCANACC, GROACC, FSINACC, FLINACC, FLUTACC, &
            TAACC, UVACC, PRESACC, QAACC
        real, dimension(:, :), allocatable :: &
            TBARACC, THLQACC, THICACC, THALACC, GFLXACC
    end type !CLASSOUT_VARS

!todo: Investigate what this is
    integer ireport

!> DAN  USE RTE SUBROUTINES FOR READING EVENT FILE AND SHD FILE, AND
!> DAN  WRITING R2C-FORMAT OUTPUT FILES      
!>  INTEGER CONSTANTS.
!INTEGER ILG
!INTEGER,PARAMETER :: ICAN=4, IGND=6, ICP1=ICAN+1
    integer, parameter :: ICAN = 4, ICP1 = ICAN + 1, ICTEM = 1 !Number of CTEM vegetation categories (set to 1 if not using CTEM)
    integer M_S, M_R
    integer, parameter :: M_C = 5
!INTEGER,PARAMETER :: M_S=290, M_R=7, M_C=5
!M_S and M_R are now read in and used to allocate the appropriate arrays - Frank S Jul 2013
!todo it should be read in from the shd file
!todo M_S could be removed as it is now just a surrogate of WF_NO (KCK)

!INTEGER IGND
    real IGND_TEST, IGND_DEEP

!> WATERSHED RELATED VARIABLES
!    integer LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX, LONDEGMIN, &
!        LONMINMIN, LONDEGMAX, LONMINMAX
!    integer WF_IYMAX, WF_JXMAX
!> note, there are more watershed related variables declared in mesh_input_module.f

!    real(kind = 8) LATLENGTH, LONGLENGTH
!    real(kind = 8) WF_AL
!    real WF_LAND_MAX, WF_LAND_SUM
!    integer WF_LAND_COUNT

!> IOSTAT VARIABLE
    integer IOS, IOS_EVT

!> FOR OUTPUT
    character(450) GENDIR_OUT

    !> For R2C-format out
    integer rte_year_now, rte_month_now, rte_day_now, rte_hour_now

!todo clean up commets and arrange variables a bit better

!> SCA variables

!todo clean up comments and make sure the variables
!todo are in groups that make sense
    real basin_SCA
    real basin_SWE
!> STREAMFLOW VARIABLES
!* WF_GAGE: GAUGE IDENTIFIER (8 CHARACTER STRING)
!* WF_NO: NUMBER OF STREAMFLOW GAUGES
!* WF_NL: NUMBER OF DATA POINTS
!* WF_MHRD: NUMBER OF HOURS OF DATA PER MONTH
!* WF_KT: HOURLY INCREMENT FOR STREAMFLOW INPUT (24 = DAILY)
!* WF_IY: Y-DIRECTION GAUGE CO-ORDINATE (UTM OR LATLONG)
!* WF_JX: X-DIRECTION GAUGE CO-ORDINATE (UTM OR LATLONG)
!* WF_S: GAUGE'S PARENT GRID SQUARE
!* WF_QHYD: STREAMFLOW VALUE (_AVG = DAILY AVERAGE)
!* WF_QSYN: SIMULATED STREAFLOW VALUE (_AVG = DAILY AVERAGE)
!* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
!* WF_START_DAY OBSERVED STREAMFLOW START DAY
!* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
    integer WF_NO, WF_NL, WF_MHRD, WF_KT, WF_START_YEAR, &
        WF_START_DAY, WF_START_HOUR
    integer, dimension(:), allocatable :: WF_IY, WF_JX, WF_S
    real, dimension(:), allocatable :: WF_QHYD, WF_QHYD_AVG, WF_QHYD_CUM
    real, dimension(:), allocatable :: WF_QSYN, WF_QSYN_AVG, WF_QSYN_CUM
    character(8), dimension(:), allocatable :: WF_GAGE

!> RESERVOIR VARIABLES
    integer, dimension(:), allocatable :: WF_IRES, WF_JRES, WF_RES, WF_R
    real, dimension(:), allocatable :: WF_B1, WF_B2, WF_QREL, WF_RESSTORE
    character(8), dimension(:), allocatable :: WF_RESNAME

!> FOR BASEFLOW INITIALIZATION
    integer JAN
    integer imonth_now, imonth_old

!>     FOR ROUTING
!* WF_R1: MANNING'S N FOR RIVER CHANNEL
!* WF_R2: OPTIMIZED RIVER ROUGHNESS FACTOR
!* WF_QO2: SIMULATED STREAMFLOW VALUE
    real WF_R1(M_C), WF_R2(M_C)
    real, dimension(:), allocatable :: WF_NHYD, WF_QBASE, WF_QI2, &
        WF_QO1, WF_QO2, WF_QR, WF_STORE1, WF_STORE2, WF_QI1

! Saul=======
!* HOURLY_START_*: Start day/year for recording hourly averaged data
!* HOURLY_STOP_*: Stop day/year for recording hourly averaged data
!* DAILY_START_*: Start day/year for recording daily averaged data
!* DAILY_STOP_*: Stop day/year for recording daily averaged data
    integer HOURLY_START_DAY, HOURLY_STOP_DAY, DAILY_START_DAY, &
        DAILY_STOP_DAY
    integer HOURLY_START_YEAR, HOURLY_STOP_YEAR, DAILY_START_YEAR, &
        DAILY_STOP_YEAR
    integer JDAY_IND_STRM, JDAY_IND1, JDAY_IND2, JDAY_IND3
!*******************************************************************************

!> LAND SURFACE DIAGNOSTIC VARIABLES.

    real, dimension(:), allocatable :: SNOGRD

!>==========
!>
!> START ENSIM == FOR ENSIM == FOR ENSIM == FOR ENSIM ==
    character(10) wf_landclassname(10)
    integer(kind = 4) wfo_yy, wfo_mm, wfo_dd, wfo_hh, wfo_mi, wfo_ss, &
        wfo_ms, nj, ensim_month, ensim_day
    integer(kind = 4) WFO_SEQ, ENSIM_IOS
    integer(kind = 4) CURREC
!> End of ENSIM Changes 
!>== ENSIM == ENSIM == ENSIM == ENSIM == ENSIM ==

!>  CONSTANTS AND TEMPORARY VARIABLES.
    real DEGLAT, DEGLON, FSDOWN1, FSDOWN2, FSDOWN3, RDAY, &
        DECL, HOUR, COSZ, &
        ALTOT, FSSTAR, FLSTAR, QH, QE, BEG, SNOMLT, ZSN, TCN, TSN, TPN, GTOUT
    integer JLAT

!> *************************************************************
!> For reading in options information from MESH_run_options.ini
!> *************************************************************
    character(20) IRONAME
    integer IROVAL

!> *******************************************************************
!> For reading in the last information in mesh_paramters_hydrology.ini
!> *******************************************************************
    character(30) NMTESTFORMAT

!>=======================================================================
!>     * DIMENSION STATEMENTS

!> FIRST SET OF DEFINITIONS:

!> BACKGROUND VARIABLES, AND PROGNOSTIC AND DIAGNOSTIC
!> VARIABLES NORMALLY PROVIDED BY AND/OR USED BY THE GCM.
!> THE SUFFIX "ROW" REFERS TO VARIABLES EXISTING ON THE
!> MOSAIC GRID ON THE CURRENT LATITUDE CIRCLE.  THE SUFFIX
!> "GAT" REFERS TO THE SAME VARIABLES AFTER THEY HAVE UNDERGONE
!> A "GATHER" OPERATION IN WHICH THE TWO MOSAIC DIMENSIONS
!> ARE COLLAPSED INTO ONE.  THE SUFFIX "GRD" REFERS BOTH TO
!> GRID-CONSTANT INPUT VARIABLES. AND TO GRID-AVERAGED
!> DIAGNOSTIC VARIABLES.

!> THE FIRST DIMENSION ELEMENT OF THE "ROW" VARIABLES
!> REFERS TO THE NUMBER OF GRID CELLS ON THE CURRENT
!> LATITUDE CIRCLE.  IN THIS STAND-ALONE VERSION, THIS
!> NUMBER IS ARBITRARILY SET TO THREE, TO ALLOW UP TO THREE
!> SIMULTANEOUS TESTS TO BE RUN.  THE SECOND DIMENSION
!> ELEMENT OF THE "ROW" VARIABLES REFERS TO THE MAXIMUM
!> NUMBER OF TILES IN THE MOSAIC.  IN THIS STAND-ALONE
!> VERSION, THIS NUMBER IS SET TO EIGHT.  THE FIRST
!> DIMENSION ELEMENT IN THE "GAT" VARIABLES IS GIVEN BY
!> THE PRODUCT OF THE FIRST TWO DIMENSION ELEMENTS IN THE
!> "ROW" VARIABLES.

!>     * CONSTANTS (PARAMETER DEFINITIONS):

!* bi%NA: MAXIMUM ALLOWABLE NUMBER OF GRID SQUARES
!* bi%NTYPE: MAXIMUM ALLOWABLE NUMBER OF GRUS
!* bi%ILG: MAXIMUM ALLOWABLE SINGLE-DIMENSION ARRAY LENGTH
!* ICAN: MAXIMUM ALLOWABLE NUMBER OF LAND COVER TYPES
!* ICP1: MAXIMUM ALLOWABLE NUMBER OF LAND COVER TYPES INCLUDING
!*       URBAN AREAS
!* bi%IGND: MAXIMUM ALLOWABLE NUMBER OF SOIL LAYERS
!* M_X: MAXIMUM ALLOWABLE NUMBER OF GRID COLUMNS IN SHD FILE
!* M_Y: MAXIMUM ALLOWABLE NUMBER OF GRID ROWS IN SHD FILE
!* M_S: MAXIMUM ALLOWABLE NUMBER OF STREAMFLOW GAUGES
!* M_R: MAXIMUM ALLOWABLE NUMBER OF RESERVOIRS
!* M_C: MAXIMUM ALLOWABLE NUMBER OF RIVER CHANNELS
!* M_G: MAXIMUM ALLOWABLE NUMBER OF GRID OUTPUTS

!> DAN  * VERSION: MESH_DRIVER VERSION
!> DAN  * RELEASE: PROGRAM RELEASE VERSIONS
!> ANDY * VER_OK: IF INPUT FILES ARE CORRECT VERSION FOR PROGRAM
!> ANDY *    INTEGER, PARAMETER :: M_G = 5
    character(24) :: VERSION = 'TRUNK (893)'
!+CHARACTER :: VERSION*24 = 'TAG'
    character(8) RELEASE(7)
    logical VER_OK
!>
!>*******************************************************************
!>
!> OPERATIONAL VARIABLES:

!* IOS: IOSTAT (ERROR) RETURN ON READ EXTERNAL FILE
!* IY: Y-DIRECTION GRID CO-ORDINATE, USED TO READ FORCING DATA
!* JX: X-DIRECTION GRID CO-ORDINATE, USED TO READ FORCING DATA
!* NN: GRID SQUARE, USED TO READ DRAINAGE DATABASE
!* II: GRU, USED TO READ DRAINAGE DATABASE
!* JAN: IS USED TO INITIALISE BASEFLOW (WHEN JAN = 1)
!* N: COUNTER USED BY CLASS
!* NCOUNT: HALF-HOURLY BASED TIME STEP (200 LOOP)
!* NSUM: NUMBER OF ITERATIONS, TIME STEPS PASSED (200 LOOP)
!* NSUM_TOTAL: total number of iterations
!* i: COUNTER
!* j: COUNTER
!* k: COUNTER
!* l: COUNTER
!* m: COUNTER
!* CONFLAGS: NUMBER OF CONTROL FLAGS
!* OPTFLAGS: NUMBER OF OPTFLAGS
!* INDEPPAR: NUMBER OF GRU-INDEPENDENT VARIABLES
!* DEPPAR: NUMBER OF GRU-DEPENDENT VARIABLES
!* PAS: STAT (ERROR) RETURN ON ALLOCATE VARIABLE
!* OPN: OPENED RETURN ON INQUIRE STATEMENT (USED TO CHECK IF AN
!*      EXTERNAL FILE HAS BEEN OPENED BY THE PROGRAM)
!* FILE_VER: FILE VERSION USED TO SEEK INPUT FILE COMPATIBILITY
!*           (COMPARED TO "RELEASE")
    character(8) FILE_VER
    integer N, NCOUNT, NSUM, i, j, k, l, m, &
        INDEPPAR, DEPPAR, PAS, NSUM_TOTAL
!  CONFLAGS, OPTFLAGS, INDEPPAR, DEPPAR, PAS
    logical OPN
!>
!>*******************************************************************
!>
!>  BASIN INFORMATION AND COUNTS:
!* WF_NA: NUMBER OF GRID SQUARES
!* bi%NAA: NUMBER OF GRID OUTLETS
!* WF_NTYPE: NUMBER OF GRUS
!* bi%NRVR: NUMBER OF RIVER CLASSES
!* WF_IMAX: NUMBER OF GRID COLUMNS IN BASIN
!* WF_JMAX: NUMBER OF GRID ROWNS IN BASIN
!* bi%AL: SINGLE-DIMENSION GRID SQUARE LENGTH
!* LAT/LONG, SITE LOCATION INFORMATION:
!* bi%iyMin: MINIMUM Y-DIRECTION GRID CO-ORDINATE (UTM)
!* bi%iyMax: MAXIMUM Y-DIRECTION GRID CO-ORDINATE (UTM)
!* bi%jxMin: MINIMUM X-DIRECTION GRID CO-ORDINATE (UTM)
!* bi%jxMax: MAXIMUM X-DIRECTION GRID CO-ORDINATE (UTM)
!* bi%GRDN: GRID NORTHING
!* bi%GRDE: GRID EASTING
!* LATLENGTH: SINGLE SIDE LENGTH OF GRID SQUARE IN DEGREES
!*            LATITUDE
!* LONGLENGTH: SINGLE SIDE LENGTH OF GRID SQUARE IN DEGREES
!*             LONGITUDE
!>************************************************************
!>
!> RESERVOIR MEASUREMENTS:
!* WF_RESNAME: RESERVOIR IDENTIFIER (8 CHARACTER STRING)
!* WF_NORESV: NUMBER OF RESERVOIRS
!* WR_NREL: NUMBER OF DATA POINTS
!* WF_KTR: HOURLY INCREMENT FOR RESERVOIR INPUR (24 = DAILY)
!* WF_IRES: Y-DIRECTION GAUGE CO-ORDINATE
!* WF_JRES: X-DIRECTION GAUGE CO-ORDINATE
!* WF_R: RESERVOIR'S PARENT GRID SQUARE
!* WF_QREL: RESERVOIR VALUE

    integer WF_NORESV, WF_NREL, WF_KTR, WF_NORESV_CTRL
    integer WF_ROUTETIMESTEP, WF_TIMECOUNT, DRIVERTIMESTEP
!>
    real I_G, J_G
!* I_G: REAL TEMPORARY IY COORDINATE FOR STREAM AND RESERVOIR GAUGES
!* J_G: REAL TEMPORARY JX COORDINATE FOR STREAM AND RESERVOIR GAUGES
!>*******************************************************************
!>
!* rte_frames_now: FRAME NUMBER BEING WRITTEN TO R2C-FORMAT FILE
!* rte_frames_total: TOTAL NUMBER OF FRAMES IN R2C-FORMAT FILE (TOTAL
!*            NUMBER OF FRAMES IS NEVER KNOWN, IS ALWAYS SET TO
!*            rte_frames_total + 1)
    integer rte_frames_now, rte_frames_total
    integer FRAME_NO_NEW
!* rte_runoff: HOURLY SIMULATED RUNOFF
!* rte_recharge: HOURLY SIMULATED RECHARGE
!* rte_leakage: UNKNOWN, BUT MAY BE USED IN THE FUTURE
    real, dimension(:, :), allocatable :: rte_runoff, rte_recharge, rte_leakage
!-* LEAKAGE: UNKNOWN, BUT MAY BE USED IN THE FUTURE

!> GRID OUTPUT POINTS
!* BNAM: TEMPORARY HOLD FOR OUTPUT DIRECTORY (12 CHARACTER STRING)
    character(12) BNAM
!* WF_NUM_POINTS: NUMBER OF GRID OUTPUTS
!* I_OUT: OUTPUT GRID SQUARE TEMPORARY STORE
    integer WF_NUM_POINTS, I_OUT
!>
!>*******************************************************************
!>
!>*******************************************************************
!>
!> LIMITING TIME STEPS (CLASS.INI):
!> DAN  NOT USED RIGHT NOW.  CONSIDER USING THEM TO LIMIT RUN INSTEAD
!> DAN  OF END OF FORCING.BIN FILE (IS ESPECIALLY USEFUL WHEN DEBUGGING).
!* JOUT1: DAILY-AVERAGED OUTPUT START DAY (JULIAN FROM YEAR START)
!* JOUT2: DAILY-AVERAGED OUTPUT STOP DAY (JULIAN FROM YEAR START)
!* JAV1: DAILY-AVERAGED OUTPUT START YEAR
!* JAV2: DAILY-AVERAGED OUTPUT STOP YEAR
!* KOUT1: YEARLY-AVERAGED OUTPUT START DAY (JULIAN FROM YEAR START)
!* KOUT2: YEARLY-AVERAGED OUTPUT STOP DAY (JULIAN FROM YEAR START)
!* KAV1: YEARLY-AVERAGED OUTPUT START YEAR
!* KAV2: YEARLY-AVERAGED OUTPUT STOP YEAR
    integer JOUT1, JOUT2, JAV1, JAV2, KOUT1, KOUT2, KAV1, KAV2
!>
!>*******************************************************************
!>
!> CLASS CONTROL FLAGS:
!> DAN  CONSIDER INCLUDING AS CONTROL FLAGS IN RUN_OPTIONS.INI FILE SO
!> DAN  THAT THEY ARE NO LONGER HARD-CODED.
!* ALL: DESCRIPTIONS ARE WRITTEN WHERE RUN_OPTIONS.INI IS READ
    integer IDISP, IZREF, ISLFD, IPCP, IWF, IPAI, IHGT, IALC, &
        IALS, IALG, ITG, ITC, ITCG

!> GRID SQUARE COUNTS:
!* NLTEST: NUMBER OF GRID SQUARES (CLASS.INI)
!* NMTEST: NUMBER OF GRUS (CLASS.INI)
!* IHOUR: CURRENT HOUR OF MET. FORCING DATA (0 TO 23) (CLASS.INI)
!* IMIN: CURRENT MINUTE OF MET. FORCING DATA (0 OR 30) (CLASS.INI)
!* IDAY: CURRENT DAY OF MET. FORCING DATA (JULIAN FROM YEAR START)
!*       (CLASS.INI)
!* IYEAR: CURRENT YEAR OF MET. FORCING DATA (CLASS.INI)
!* bi%NML: NUMBER OF LAND-ORIENTED GRID SQUARES
!* bi%NMW: NUMBER OF WATER-ORIENTED GRID SQUARES
!INTEGER :: ILW
    integer NLTEST, NMTEST, NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI

!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
!* TBAR: INITIAL SOIL LAYER TEMPERATURE
!* THLQ: INITIAL SOIL LAYER LIQUID WATER CONTENT
!* THIC: INITIAL SOIL LAYER ICE WATER CONTENT
    real, dimension(:, :), allocatable :: TBARGAT, THLQGAT, THICGAT, &
        SANDGAT, CLAYGAT
    real, dimension(:, :), allocatable :: TBASROW, &
        CMAIROW, TACROW, QACROW, WSNOROW
     
!>PBSM VARIABLES (GRU)
!* DrySnow: 0 = air temperature above 0 degC
!*          1 = air temperature below 0 degC
!* SnowAge: hours since last snowfall
!* Drift: blowing snow transport (kg/m^2)
!* Subl: blowing snow sublimation (kg/m^2)
    real, dimension(:), allocatable :: DrySnowGAT, SnowAgeGAT, &
        TSNOdsGAT, RHOSdsGAT, DriftGAT, SublGAT, DepositionGAT
    real, dimension(:, :), allocatable :: DrySnowROW, SnowAgeROW, &
        TSNOdsROW, RHOSdsROW, DriftROW, SublROW, DepositionROW
!>CLASS SUBAREA VARIABLES NEEDED FOR PBSM
    real, dimension(:), allocatable :: ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
        HCPSCS, HCPSGS, HCPSC, HCPSG, TSNOWC, TSNOWG, &
        RHOSC, RHOSG, XSNOWC, XSNOWG, XSNOCS, XSNOGS
!* TPND: INITIAL PONDING TEMPERATURE (CLASS.INI)
!* ZPND: INITIAL PONDING DEPTH (CLASS.INI)
!* ALBS: ALBEDO OF SNOWPACK (CLASS.INI)
!* TSNO: INITIAL SNOWPACK TEMPERATURE (CLASS.INI)
!* RHOS: DENSITY OF SNOWPACK (CLASS.INI)
!* SNO: SNOWPACK ON CANOPY LAYER (CLASS.INI)
!* TCAN: INITIAL CANOPY TEMPERATURE (CLASS.INI)
!* GRO: VEGETATION GROWTH INDEX (CLASS.INI)
    real, dimension(:), allocatable :: TPNDGAT, ZPNDGAT, TBASGAT, &
        ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, TCANGAT, RCANGAT, SCANGAT, &
        GROGAT, FRZCGAT, CMAIGAT, TACGAT, QACGAT, WSNOGAT
     
    real, dimension(:, :, :), allocatable :: TSFSROW
    real, dimension(:, :), allocatable :: TSFSGAT

!> GATHER-SCATTER COUNTS:
!-INTEGER, DIMENSION(:), ALLOCATABLE :: ILMOS, JLMOS, IWMOS, JWMOS
!>
!>*******************************************************************
!>
!> CANOPY AND SOIL INFORMATION (CLASS):
!> THE LENGTH OF THESE ARRAYS IS DETERMINED BY THE NUMBER
!> OF SOIL LAYERS (3) AND THE NUMBER OF BROAD VEGETATION
!> CATEGORIES (4, OR 5 INCLUDING URBAN AREAS).
!* ALL: DEFINITIONS IN CLASS DOCUMENTATION (CLASS.INI)
    real, dimension(:, :), allocatable :: FCANGAT, LNZ0GAT, &
        ALVCGAT, ALICGAT
    real, dimension(:, :, :), allocatable :: &
        PAIDROW, HGTDROW, ACVDROW, ACIDROW
    real, dimension(:, :), allocatable :: PAMXGAT, PAMNGAT, &
        CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, VPDAGAT, VPDBGAT, PSGAGAT, &
        PSGBGAT, PAIDGAT, HGTDGAT, ACVDGAT, ACIDGAT
    real, dimension(:, :, :), allocatable :: THPROW, THRROW, THMROW, &
        BIROW, PSISROW, GRKSROW, THRAROW, HCPSROW, TCSROW, THFCROW, &
        PSIWROW, DLZWROW, ZBTWROW
    real, dimension(:, :), allocatable :: THPGAT, THRGAT, THMGAT, &
        BIGAT, PSISGAT, GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, THFCGAT, &
        PSIWGAT, DLZWGAT, ZBTWGAT, GFLXGAT
    real, dimension(:, :), allocatable :: &
        WFSFROW, ALGWROW, ALGDROW, ASVDROW, ASIDROW, AGVDROW, &
        AGIDROW
    real, dimension(:), allocatable :: DRNGAT, XSLPGAT, XDGAT, &
        WFSFGAT, KSGAT, ALGWGAT, ALGDGAT, ASVDGAT, ASIDGAT, AGVDGAT, &
        AGIDGAT, ZSNLGAT, ZPLGGAT, ZPLSGAT, SDEPGAT, FAREGAT
!* PBSM parameters
!  fetch: fetch distance (m)
!  Ht: vegetation height (m)
!  N_S:vegetation density (number/m^2)
!  A_S: vegetation width (m)
!  Distrib: Inter-GRU snow redistribution factor
    real, dimension(:), allocatable :: &
        fetchGAT, HtGAT, N_SGAT, A_SGAT, DistribGAT

!* SAND: PERCENT-CONTENT OF SAND IN SOIL LAYER (CLASS.INI)
!* CLAY: PERCENT-CONTENT OF CLAY IN SOIL LAYER (CLASS.INI)
!* ORGM: PERCENT-CONTENT OF ORGANIC MATTER IN SOIL LAYER (CLASS.INI)

!* MIDROW: DEFINITION IN CLASS DOCUMENTATION (CLASS.INI)

    integer, dimension(:, :, :), allocatable :: ISNDROW, IORG
    integer, dimension(:, :), allocatable :: ISNDGAT
    integer, dimension(:,:), allocatable :: IGDRROW
    integer, dimension(:), allocatable :: IGDRGAT
!>
!>*******************************************************************
!>
!> WATROF FLAGS AND VARIABLES:
!* VICEFLG: VERTICAL ICE FLAG OR LIMIT
!* HICEFLG: HORIZONTAL ICE FLAG OR LIMIT
    integer LZFFLG, EXTFLG, IWFICE, ERRFLG, IWFOFLW
    real VICEFLG, PSI_LIMIT, HICEFLG
!* DD (DDEN): DRAINAGE DENSITY (CLASS.INI)
!* MANN (WFSF): MANNING'S n (CLASS.INI)
    real, dimension(:), allocatable :: DDGAT, MANNGAT
    real, dimension(:, :), allocatable :: BTC, BCAP, DCOEFF, BFCAP, &
        BFCOEFF, BFMIN, BQMAX

!> CONTROL FLAGS
!* ALL: DEFINITIONS ARE WRITTEN JUST BEFORE RUN_OPTIONS.INI IS
!*      OPENED
!* RELFLG: RELEASE-MATCH STRICTNESS
!INTEGER :: RELFLG
!>
!>*******************************************************************
!>
!> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
    real, dimension(:), allocatable :: ZDMGRD, &
        ZDHGRD, RADJGRD, CSZGRD, &
        PADRGRD, VPDGRD, &
        TADPGRD, RHOAGRD, RPCPGRD, TRPCGRD, SPCPGRD, TSPCGRD, RHSIGRD, &
        FCLOGRD, DLONGRD, Z0ORGRD, GGEOGRD, UVGRD, XDIFFUS, &
        RPREGRD, SPREGRD, VMODGRD

!> MAM - logical variables to control simulation runs:
    logical ENDDATE, ENDDATA

    real, dimension(:), allocatable :: ZRFMGAT, ZRFHGAT, ZDMGAT, &
        ZDHGAT, ZBLDGAT, RADJGAT, CSZGAT, &
        RPREGAT, SPREGAT, &
        PADRGAT, VPDGAT, TADPGAT, RHOAGAT, RPCPGAT, TRPCGAT, SPCPGAT, &
        TSPCGAT, RHSIGAT, FCLOGAT, DLONGAT, Z0ORGAT, GGEOGAT, VMODGAT
!>
!>*******************************************************************
!>
!> LAND SURFACE DIAGNOSTIC VARIABLES:
    real, dimension(:, :), allocatable :: CDHROW, CDMROW, HFSROW, &
        TFXROW, QEVPROW, QFSROW, QFXROW, PETROW, GAROW, EFROW, GTROW, &
        QGROW, TSFROW, ALVSROW, ALIRROW, FSNOROW, SFCTROW, SFCUROW, &
        SFCVROW, SFCQROW, FSGVROW, FSGSROW, FSGGROW, FLGVROW, FLGSROW, &
        FLGGROW, HFSCROW, HFSSROW, HFSGROW, HEVCROW, HEVSROW, HEVGROW, &
        HMFCROW, HMFNROW, HTCCROW, HTCSROW, PCFCROW, PCLCROW, PCPNROW, &
        PCPGROW, QFGROW, QFNROW, QFCLROW, QFCFROW, ROFROW, ROFOROW, &
        ROFSROW, ROFBROW, ROFCROW, ROFNROW, ROVGROW, WTRCROW, WTRSROW, &
        WTRGROW, DRROW, WTABROW, ILMOROW, UEROW, HBLROW, TROFROW, &
        TROOROW, TROSROW, TROBROW
    real, dimension(:), allocatable :: CDHGAT, CDMGAT, HFSGAT, &
        TFXGAT, QEVPGAT, QFSGAT, QFXGAT, PETGAT, GAGAT, EFGAT, GTGAT, &
        QGGAT, ALVSGAT, ALIRGAT, FSNOGAT, SFRHGAT, SFCTGAT, SFCUGAT, &
        SFCVGAT, SFCQGAT, FSGVGAT, FSGSGAT, FSGGGAT, FLGVGAT, FLGSGAT, &
        FLGGGAT, HFSCGAT, HFSSGAT, HFSGGAT, HEVCGAT, HEVSGAT, HEVGGAT, &
        HMFCGAT, HMFNGAT, HTCCGAT, HTCSGAT, PCFCGAT, PCLCGAT, PCPNGAT, &
        PCPGGAT, QFGGAT, QFNGAT, QFCLGAT, QFCFGAT, ROFGAT, ROFOGAT, &
        ROFSGAT, ROFBGAT, ROFCGAT, ROFNGAT, ROVGGAT, WTRCGAT, WTRSGAT, &
        WTRGGAT, DRGAT, WTABGAT, ILMOGAT, UEGAT, HBLGAT, QLWOGAT, FTEMP, &
        FVAP, RIB, TROFGAT, TROOGAT, TROSGAT, TROBGAT
    real, dimension(:), allocatable :: CDHGRD, CDMGRD, HFSGRD, &
        TFXGRD, QEVPGRD, QFSGRD, QFXGRD, PETGRD, GAGRD, EFGRD, GTGRD, &
        QGGRD, TSFGRD, ALVSGRD, ALIRGRD, FSNOGRD, SFCTGRD, SFCUGRD, &
        SFCVGRD, SFCQGRD, FSGVGRD, FSGSGRD, FSGGGRD, FLGVGRD, FLGSGRD, &
        FLGGGRD, HFSCGRD, HFSSGRD, HFSGGRD, HEVCGRD, HEVSGRD, HEVGGRD, &
        HMFCGRD, HMFNGRD, HTCCGRD, HTCSGRD, PCFCGRD, PCLCGRD, PCPNGRD, &
        PCPGGRD, QFGGRD, QFNGRD, QFCLGRD, QFCFGRD, ROFGRD, ROFOGRD, &
        ROFSGRD, ROFBGRD, ROFCGRD, ROFNGRD, ROVGGRD, WTRCGRD, WTRSGRD, &
        WTRGGRD, DRGRD, WTABGRD, ILMOGRD, UEGRD, HBLGRD

    real, dimension(:, :, :), allocatable :: HMFGROW, HTCROW, QFCROW, &
        GFLXROW
    real, dimension(:, :), allocatable :: HMFGGAT, HTCGAT, QFCGAT
    real, dimension(:, :), allocatable :: HMFGGRD, HTCGRD, QFCGRD, GFLXGRD
    integer, dimension(:, :, :, :), allocatable :: ITCTROW
    integer, dimension(:, :, :), allocatable :: ITCTGAT

!* TITLE: PROJECT DESCRIPTOR (6 COLUMNS: 4 CHARACTER STRINGS)
!* NAME: AUTHOR, RESEARCHER (6 COLUMNS: 4 CHARACTER STRINGS)
!* PLACE: SITE LOCATION, BASIN (6 COLUMNS: 4 CHARACTER STRINGS)
    character(4) TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, &
        TITLE6, NAME1, NAME2, NAME3, NAME4, NAME5, NAME6, &
        PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
!>
!>*******************************************************************
!>*******************************************************************
!>
!> OUTPUT VARIABLES:
!> THE SUFFIX "ACC" REFERS TO THE ACCUMULATOR ARRAYS USED IN
!> CALCULATING TIME AVERAGES.
!* ALL: DEFINITIONS IN CLASS DOCUMENTATION
    real, dimension(:), allocatable :: PREACC, GTACC, QEVPACC, &
        HFSACC, ROFACC, SNOACC, ALVSACC, ALIRACC, FSINACC, FLINACC, &
        TAACC, UVACC, PRESACC, QAACC, EVAPACC, FLUTACC, ROFOACC, &
        ROFSACC, ROFBACC, HMFNACC, WTBLACC, WSNOACC, RHOSACC, TSNOACC, &
        TCANACC, RCANACC, SCANACC, GROACC, CANARE, SNOARE, ZPNDACC

!> FIELD OF DELTA STORAGE AND INITIAL STORAGE
    real, dimension(:), allocatable :: DSTG, STG_I

    real, dimension(:, :), allocatable :: TBARACC, THLQACC, THICACC, &
        THALACC , THLQ_FLD, THIC_FLD, GFLXACC

!* TOTAL_ROFACC: TOTAL RUNOFF
!* TOTAL_EVAPACC: TOTAL EVAPORATION
!* TOTAL_PREACC: TOTAL PRECIPITATION
!* INIT_STORE: INITIAL STORAGE
!* FINAL_STORE: FINAL STORAGE
!* TOTAL_AREA: TOTAL FRACTIONED DRAINAGE AREA
    real TOTAL_ROFACC, TOTAL_ROFOACC, TOTAL_ROFSACC, &
        TOTAL_ROFBACC, TOTAL_EVAPACC, TOTAL_PREACC, INIT_STORE, &
        FINAL_STORE, TOTAL_AREA, &
        TOTAL_PRE_ACC_M, TOTAL_EVAP_ACC_M, TOTAL_ROF_ACC_M, &
        TOTAL_ROFO_ACC_M, TOTAL_ROFS_ACC_M, TOTAL_ROFB_ACC_M, &
        TOTAL_PRE_M, TOTAL_EVAP_M, TOTAL_ROF_M, &
        TOTAL_ROFO_M, TOTAL_ROFS_M, TOTAL_ROFB_M, &
        TOTAL_SCAN_M, TOTAL_RCAN_M, &
        TOTAL_SNO_M, TOTAL_WSNO_M, &
        TOTAL_ZPND_M, &
        TOTAL_STORE_M, TOTAL_STORE_2_M, &
        TOTAL_STORE_ACC_M
  
!* TOTAL_HFS = TOTAL SENSIBLE HEAT FLUX
!* TOTAL_QEVP = TOTAL LATENT HEAT FLUX
    real TOTAL_HFSACC, TOTAL_QEVPACC

    real TOTAL_STORE, TOTAL_STORE_2, TOTAL_RCAN, TOTAL_SCAN, TOTAL_SNO, TOTAL_WSNO, TOTAL_ZPND
    real TOTAL_PRE, TOTAL_EVAP, TOTAL_ROF, TOTAL_ROFO, TOTAL_ROFS, TOTAL_ROFB
    real, dimension(:), allocatable :: TOTAL_THLQ, TOTAL_THIC, &
        TOTAL_THLQ_M, TOTAL_THIC_M

!> CROSS-CLASS VARIABLES (CLASS):
!> ARRAYS DEFINED TO PASS INFORMATION BETWEEN THE THREE MAJOR
!> SUBSECTIONS OF CLASS ("CLASSA", "CLASST" AND "CLASSW").
    real, dimension(:, :), allocatable :: TBARC, TBARG, TBARCS, &
        TBARGS, THLIQC, THLIQG, THICEC, THICEG, FROOT, HCPC, HCPG, &
        TCTOPC, TCBOTC, TCTOPG, TCBOTG

    real, dimension(:), allocatable :: FC, FG, FCS, FGS, RBCOEF, &
        ZSNOW, FSVF, FSVFS, ALVSCN, ALIRCN, ALVSG, &
        ALIRG, ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, &
        ALIRSC, TRVSCN, TRIRCN, TRVSCS, TRIRCS, RC, RCS, FRAINC, &
        FSNOWC, FRAICS, FSNOCS, CMASSC, CMASCS, DISP, DISPS, ZOMLNC, &
        ZOELNC, ZOMLNG, &
        ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, TRSNOW, CHCAP, CHCAPS, &
        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, G12CS, G12GS, G23C, &
        G23G, G23CS, G23GS, QFREZC, QFREZG, QMELTC, QMELTG, EVAPC, &
        EVAPCG, EVAPG, EVAPCS, EVPCSG, EVAPGS, TCANO, TCANS, RAICAN, &
        SNOCAN, RAICNS, SNOCNS, CWLCAP, CWFCAP, CWLCPS, CWFCPS, TSNOCS, &
        TSNOGS, RHOSCS, RHOSGS, WSNOCS, WSNOGS, TPONDC, TPONDG, TPNDCS, &
        TPNDGS, ZPLMCS, ZPLMGS, ZPLIMC, ZPLIMG

!> BALANCE ERRORS (CLASS):
!> DIAGNOSTIC ARRAYS USED FOR CHECKING ENERGY AND WATER
!> BALANCES.
    real, dimension(:), allocatable :: CTVSTP, CTSSTP, CT1STP, &
        CT2STP, CT3STP, WTVSTP, WTSSTP, WTGSTP

!> CTEM-RELATED FIELDS (NOT USED IN STANDARD OFFLINE CLASS RUNS).
    real, dimension(:), allocatable :: &
        CO2CONC, COSZS, XDIFFUSC, CFLUXCG, CFLUXCS
    real, dimension(:, :), allocatable :: &
        AILCG, AILCGS, FCANC, FCANCS, CO2I1CG, CO2I1CS, CO2I2CG, CO2I2CS, &
        SLAI, FCANCMX, ANCSVEG, ANCGVEG, RMLCSVEG, RMLCGVEG, &
        AILC, PAIC, &
        FIELDSM, WILTSM
    real, dimension(:, :, :), allocatable :: &
        RMATCTEM, RMATC
    integer, dimension(:), allocatable :: NOL2PFTS
    integer ICTEMMOD, L2MAX

!> COMMON BLOCK PARAMETERS (CLASS):
    integer K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11
    real X1, X2, X3, X4, G, GAS, X5, X6, CPRES, GASV, X7, CPI, X8, &
        CELZRO, X9, X10, X11, X12, X13, X14, X15, SIGMA, X16, DELTIM, &
        DELT, TFREZ, RGAS, RGASV, GRAV, SBC, VKC, CT, VMIN, TCW, TCICE, &
        TCSAND, TCCLAY, TCOM, TCDRYS, RHOSOL, RHOOM, HCPW, HCPICE, &
        HCPSOL, HCPOM, HCPSND, HCPCLY, SPHW, SPHICE, SPHVEG, SPHAIR, &
        RHOW, RHOICE, TCGLAC, CLHMLT, CLHVAP, PI, ZOLNG, ZOLNS, ZOLNI, &
        ZORATG, ALVSI, ALIRI, ALVSO, ALIRO, ALBRCK, DELTA, CGRAV, &
        CKARM, CPD, AS, ASX, CI, BS, BETA, FACTN, HMIN, ANGMAX

!> DAN * CONFLICTS WITH COMMON BLOCK DEFINITIONS (APR 20/08)
    real, dimension(ICAN) :: CANEXT, XLEAF, ZORAT

    real, dimension(3) :: THPORG, THRORG, THMORG, BORG, PSISORG, &
        GRKSORG
    real, dimension(18, 4, 2) :: GROWYR

!-    integer found

!-    character(10) time
!-    character(8) cday

!> **********************************************************************
!>  For cacluating the subbasin grids
!> **********************************************************************

    integer SUBBASINCOUNT
    integer, dimension(:), allocatable :: SUBBASIN

!>=======================================================================
!> DAN * GLOBAL SUBROUTINES AND VARIABLES

!> DAN * SUBROUTINES AND MODULES:
!
!> READ_SHED_EF: SUBROUTINE USED TO READ THE BASIN SHD FILE
!> WRITE_R2C: SUBROUTINE USED TO WRITE R2C-FORMAT RTE.EXE INPUT
!>            FILES (RUNOFF, RECHARGE, AND LEAKAGE VALUES)
!> EF_MODULE: MODULE CONTAINING FORMATTING FUNCTIONS AND
!>            SUBROUTINES
!> EF_PARSEUTILITIES: MODULE CONTAINING FORMATTING FUNCTIONS AND
!>                    SUBROUTINES CALLED BY EF_MODULE
!> FIND_MONTH: SUBROUTINE USED TO CONVERT JULIAN DAY FROM YEAR
!>             START INTO MONTH FROM YEAR START (1 TO 12)
!> FIND_DAY: SUBROUTINE USED TO CONVERT JULIAN DAY FROM YEAR START
!>           INTO DAY FROM MONTH START (1 TO 31)

!> DAN * VARIABLES:

!* xCount: NUMBER OF GRID SQUARES IN X-DIRECTION OF
!*                      BASIN (COLUMNS) (JMAX)
!* yCount: NUMBER OF GRID SQUARES IN Y-DIRECTION OF
!*                      BASIN (ROWS) (IMAX)
!* AL: SINGLE GRID SIDE LENGTH IN METRES (AL)
!* NA: NUMBER OF GRID SQUARES IN BASIN (WF_NA)
!* NAA: NUMBER OF GRID SQUARE OUTLETS IN BASIN (NAA)
!* NTYPE: NUMBER OF GRUS (WF_NTYPE)
!* FRAC: GRID FRACTION (previously WF_FRAC)
!* ACLASS: PERCENT-GRU FRACTION FOR EACH GRID SQUARE
!* CoordSys: CO-ORDINATE SYSTEM (FROM BASIN SHD
!*                           FILE)
!* Zone: CO-ORDINATE SYSTEM (FROM BASIN SHD FILE)
!* Datum: CO-ORDINATE SYSTEM (FROM BASIN SHD FILE)
!* xOrigin: X-DIRECTION CO-ORDINATE OF BASIN GRID
!*                        (FROM BASIN SHD FILE)
!* yOrigin: Y-DIRECTION CO-ORDINATE OF BASIN GRID
!*                        (FROM BASIN SHD FILE)
!* xDelta: AVERAGE DIFFERENCE BETWEEN TWO X-DIRECTION
!*                      SIDES OF GRID SQUARE (FROM BASIN SHD FILE)
!* yDelta: AVERAGE DIFFERENCE BETWEEN TWO Y-DIRECTION
!*                      SIDES OF GRID SQUARE (FROM BASIN SHD FILE)
!* yyy: Y-DIRECTION GRID SQUARE CO-ORDINATE (YYY), aka column coordinate
!* xxx: X-DIRECTION GRID SQUARE CO-ORDIANTE (XXX), aka row coordinate

!> These are the types defined in mesh_input_module.f that contain arrays
!> that need to be allocated in read_initial_inputs.f.
    type(OutputPoints) :: op
!+    type(ShedInformation) :: si
    type(SoilLevels) :: sl
    type(ClassParameters) :: cp
    type(SoilValues) :: sv
    type(HydrologyParameters) :: hp

    type(fl_ids) :: fls

!>THESE ARE THTE TYPES DEFINED IN MODEL_OUTPUT.F95 NEED TO WRITE OUTPUT FIELD ACCUMULATED
!> OR AVERAGE FOR THE WATER BALANCE AND SOME OTHER STATES VARIABLES
    type(OUT_FLDS) :: VR
    type(basin_info) :: bi
    type(DATES_MODEL) :: TS
    type(INFO_OUT) :: IOF
    type(CLIM_INFO) :: cm
    type(met_data) :: md
    type(CLASSOUT_VARS) :: co
    type(water_balance) :: wb, wb_h
    type(energy_balance) :: eng
    type(soil_statevars) :: sov

    logical R2COUTPUT
    integer, parameter :: R2CFILEUNITSTART = 500
    integer NR2C, DELTR2C, NR2CFILES, NR2CSTATES, NR2C_R, DELTR2C_R, NR2C_S, DELTR2C_S
    integer, allocatable, dimension(:) :: GRD, GAT, GRDGAT, GRD_R, GAT_R, GRDGAT_R, GRD_S, GAT_S, GRDGAT_S
    character(50), allocatable, dimension(:, :) :: R2C_ATTRIBUTES, R2C_ATTRIBUTES_R, R2C_ATTRIBUTES_S

    integer NMELT
    real SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS
    integer, dimension(:), allocatable :: INFILTYPE
    real, dimension(:), allocatable :: SI, TSI, SNOWMELTD, SNOWMELTD_LAST, &
        SNOWINFIL, CUMSNOWINFILCS, MELTRUNOFF, CUMSNOWINFILGS

!* PDMROF
    real ZPND, FSTR
    real, dimension(:), allocatable   :: CMINPDM, CMAXPDM, BPDM, K1PDM, K2PDM, &
        ZPNDPRECS, ZPONDPREC, ZPONDPREG, ZPNDPREGS, &
        UM1CS, UM1C, UM1G, UM1GS, &
        QM1CS, QM1C, QM1G, QM1GS, &
        QM2CS, QM2C, QM2G, QM2GS, UMQ, &
        FSTRCS, FSTRC, FSTRG, FSTRGS

! To use with variable format expressions in writing some output files
    character(20) IGND_CHAR
    character(2000) FMT

    character(500) WRT_900_1, WRT_900_2, WRT_900_3, WRT_900_4, WRT_900_f
    character(500) fl_listMesh
    character(5) strInt
!=======================================================================
!     * SET PHYSICAL CONSTANTS AND COMMON BLOCKS

    common /PARAMS/ X1, X2, X3, X4, G, GAS, X5, X6, CPRES, &
        GASV, X7
    common /PARAM1/ CPI, X8, CELZRO, X9, X10, X11
    common /PARAM3/ X12, X13, X14, X15, SIGMA, X16
    common /TIMES/ DELTIM, K1, K2, K3, K4, K5, K6, K7, K8, K9, &
        K10, K11

!> THE FOLLOWING COMMON BLOCKS ARE DEFINED SPECIFICALLY FOR USE
!> IN CLASS, VIA BLOCK DATA AND THE SUBROUTINE "CLASSD".
    common /CLASS1/ DELT, TFREZ
    common /CLASS2/ RGAS, RGASV, GRAV, SBC, VKC, CT, VMIN
    common /CLASS3/ TCW, TCICE, TCSAND, TCCLAY, TCOM, TCDRYS, &
        RHOSOL, RHOOM
    common /CLASS4/ HCPW, HCPICE, HCPSOL, HCPOM, HCPSND, &
        HCPCLY, SPHW, SPHICE, SPHVEG, SPHAIR, RHOW, &
        RHOICE, TCGLAC, CLHMLT, CLHVAP
    common /CLASS5/ THPORG, THRORG, THMORG, BORG, PSISORG, &
        GRKSORG
    common /CLASS6/ PI, GROWYR, ZOLNG, ZOLNS, ZOLNI, ZORAT, &
        ZORATG
    common /CLASS7/ CANEXT, XLEAF
    common /CLASS8/ ALVSI, ALIRI, ALVSO, ALIRO, ALBRCK
    common /PHYCON/ DELTA, CGRAV, CKARM, CPD
    common /CLASSD2/ AS, ASX, CI, BS, BETA, FACTN, HMIN, ANGMAX

!> THE FOLLOWING COMMON BLOCKS ARE DEFINED FOR WATROF
!-COMMON    /WATFLGS/   VICEFLG, PSI_LIMIT, HICEFLG, LZFFLG, &
!-                      EXTFLG, IWFICE, ERRFLG, IMIN, IHOUR, IDAY, &
!-                      IYEAR

    data VICEFLG/3.0/, PSI_LIMIT/1.0/, HICEFLG/1.0/, LZFFLG/0/, &
        EXTFLG/0/, IWFICE/3/, ERRFLG/1/

    real :: startprog, endprog
    integer :: narg
!real :: alpharain
!character*50 :: alphCh

!> ((((((((((((((((((((((((((((((((((
!> Set the acceptable version numbers
!> ))))))))))))))))))))))))))))))))))
!> todo this should be input file dependant,
!>  because different files will work with different releases
!>  so, make them local variables inside each read subroutine.
    RELEASE(1) = '1.1.a01'
    RELEASE(2) = '1.1.a02'
    RELEASE(3) = '1.1.a04'
    RELEASE(4) = '1.2.000'
    RELEASE(5) = '1.2.a01'
    RELEASE(6) = '1.3.000'
    RELEASE(7) = '1.3.1'

    call cpu_time(startprog)
!>=======================================================================
!>      PROGRAM START

    !> Initialize MPI.
    call mpi_init(ierr)
    if (ierr /= mpi_success) then
        print *, 'Failed to initialize MPI.'
        call mpi_abort(mpi_comm_world, ierrcode, ierr)
        print *, 'ierrcode ', ierrcode, 'ierr ', ierr
    end if

    !> Grab number of total processes and current process ID.
    call mpi_comm_size(mpi_comm_world, inp, ierr)
    call mpi_comm_rank(mpi_comm_world, ipid, ierr)

    !> izero is active if the head node is used for booking and lateral flow
    !> processes.
    if (inp > 1) then
        izero = 1
    else
        izero = 0
    end if

    !> Reset verbose flag for worker nodes.
    if (ipid > 0) ro%VERBOSEMODE = 0

!>!TODO: UPDATE THIS (RELEASE(*)) WITH VERSION CHANGE
    if (ro%VERBOSEMODE > 0) print 951, trim(RELEASE(7)), trim(VERSION)

951 format(1x, 'MESH ', a, ' ---  (', a, ')', /)

!File handled for variable in/out names
!At the moment only class,hydro parameters and some outputs

    !> Check if any command line arguments are found.
    narg = command_argument_count()
    !print *, narg
    if (narg > 0) then
        VARIABLEFILESFLAG = 1
        if (narg >= 1) then
            call get_command_argument(1, fl_listMesh)
!            print *, fl_listMesh
!        else if (narg == 2) then
!            call get_command_argument(1, fl_listMesh)
!            print *, fl_listMesh
!todo: re-instate alpha
!            call get_command_argument(2, alphCh)
!            call value(alphCh, alpharain, ios)
!            cm%clin(8)%alpharain = alpharain
!            print *, cm%clin(8)%alpharain
        end if
        call Init_fls(fls, trim(adjustl(fl_listMesh)))
    else
!todo: Call this anyway, make loading values from file an alternate subroutine of module_files
        call Init_fls(fls)
    end if !(narg > 0) then

    !> Determine the value of IGND from MESH_input_soil_levels.txt
!todo: Move this to read_soil_levels
    bi%IGND = 0

    !> Open soil levels file and check for IOSTAT errors.
!    if ((VARIABLEFILESFLAG == 1) .and. (fls%fl(10)%isInit)) then
    iun = fls%fl(mfk%f52)%iun
    open(iun, file = trim(adjustl(fls%fl(mfk%f52)%fn)), status = 'old', action = 'read', iostat = ios)
!    else
!        open(52, file = 'MESH_input_soil_levels.txt', status = 'old', iostat = IOS)
!    end if
    if (ios /= 0) then
        print 1002
        stop
    end if

    !> Count the number of soil layers.
    IGND_TEST = 1.0
    do while (IGND_TEST /= 0.0 .and. ios == 0)
        read(52, *, iostat = ios) IGND_TEST, IGND_DEEP
        bi%IGND = bi%IGND + 1
    end do

    !> because IGND increments the first time that IGND_TEST = 0.0
    bi%IGND = bi%IGND - 1
    print *, 'IGND = ', bi%IGND
    close(iun)

1002 format(/1x, 'MESH_input_soil_levels.txt could not be opened.', &
            /1x, 'Ensure that the file exists and restart the program.', /)

!>=======================================================================
!> INITIALIZE CLASS VARIABLES
!> SET COMMON CLASS PARAMETERS.
    call CLASSD
!>
!>*******************************************************************
!>
    call READ_INITIAL_INPUTS( &
!>GENERIC VARIABLES
                             RELEASE, &
!>VARIABLES FOR READ_RUN_OPTIONS
                             IDISP, IZREF, ISLFD, IPCP, IWF, &
                             IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG, &
                             ICTEMMOD, IOS, PAS, N, IROVAL, WF_NUM_POINTS, &
!  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START, &
!  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END, &
                             IRONAME, GENDIR_OUT, &
!>variables for drainage database or new_shd
!                             bi%IGND, bi%ILG, WF_IYMAX, WF_JXMAX, &
!                             WF_LAND_COUNT, &
!                             LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX, &
!                             LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX, &
!                             WF_LAND_MAX, WF_LAND_SUM, &
!>variables for READ_CHECK_FORCING_FILES
! NUM_CSV, NUM_R2C, NUM_SEQ, &
!>variables for READ_PARAMETERS_CLASS
                             TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6, &
                             NAME1, NAME2, NAME3, NAME4, NAME5, NAME6, &
                             PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6, &
                             bi%ILW, NLTEST, NMTEST, JLAT, ICAN, &
                             DEGLAT, DEGLON, &
                             HOURLY_START_DAY, HOURLY_STOP_DAY, &
                             DAILY_START_DAY, DAILY_STOP_DAY, &
                             HOURLY_START_YEAR, HOURLY_STOP_YEAR, &
                             DAILY_START_YEAR, DAILY_STOP_YEAR, &
!  IHOUR, IMIN, IDAY, IYEAR, &
 !>variables for READ_SOIL_INI
 !>variables for READ_PARAMETERS_HYDROLOGY
                             INDEPPAR, DEPPAR, WF_R2, M_C, &
 !>the types that are to be allocated and initialised
                             bi, op, sl, cp, sv, hp, ts, cm, &
                             SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, fls)

!>
!>***********************************************************************
!> Forcing data time step should not be less than 30 min - there is no 
!> any increase in accuracy as delt (CLASS model time step) is 30 min.
!>=======================================================================

    if (HOURLYFLAG < 30) then
        print 1028
        stop
    end if

1028 format(/1x, 'FORCING DATA TIME STEP IS LESS THAN 30 MIN', &
            /1x, 'AGGREGATE THE FORCING DATA TO 30 MIN INTERVAL AND TRY AGAIN', /)

!>
!>***********************************************************************
!> MAM - Check for parameter values - all parameters should lie within the 
!> specified ranges in the "minmax_parameters.txt" file.
!>=======================================================================
!>
    call check_parameters(WF_R2, M_C, NMTEST, cp, hp, soil_por_max, soil_depth, s0, t_ice_lens)

!>
!>*******************************************************************
!>
!>=======================================================================
!> ALLOCATE ALL VARIABLES
!> DAN * IGND, ICAN, AND ICP1 HAVE BEEN INCLUDED IN CASE THEY WILL BE
!> DAN * CONFIGURABLE IN THE FUTURE (IF IN THE RUN_OPTIONS.INI FILE)
!> DAN * (APR 20/08).

!> ANDY * Allocate some variables
    allocate(WF_NHYD(bi%NA), WF_QR(bi%NA), &
             WF_QBASE(bi%NA), WF_QI2(bi%NA), WF_QO1(bi%NA), WF_QO2(bi%NA), &
             WF_STORE1(bi%NA), WF_STORE2(bi%NA), WF_QI1(bi%NA), SNOGRD(bi%NA))

    !> ANDY * Zero everything we just allocated
    WF_NHYD = 0.0
    WF_QBASE = 0.0
    WF_QI2 = 0.0
    WF_QO1 = 0.0
    WF_QO2 = 0.0
    WF_QR = 0.0
    WF_STORE1 = 0.0
    WF_STORE2 = 0.0
    WF_QI1 = 0.0

!!> WATROUTE INPUT FILES:
!-ALLOCATE (RUNOFF(YCOUNT, XCOUNT), &
!-  RECHARGE(YCOUNT, XCOUNT), STAT=PAS)
    if (ipid == 0) then
        allocate(rte_runoff(bi%yCount, bi%xCount), &
                 rte_recharge(bi%yCount, bi%xCount), rte_leakage(bi%yCount, bi%xCount), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'Standalone RTE input'
            print 1118, 'Grid square rows', bi%yCount
            print 1118, 'Grid square columns', bi%xCount
            stop
        end if
    end if

1114 format(/1x, 'Error allocating ', a, ' variables.', &
            /1x, 'Check that these bounds are within an acceptable range.', /)
1118 format(3x, a, ': ', i6)

!> MET. FORCING DATA:

!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
    allocate(TBARGAT(bi%ILG, bi%IGND), &
             THLQGAT(bi%ILG, bi%IGND), THICGAT(bi%ILG, bi%IGND), &
             SANDGAT(bi%ILG, bi%IGND), CLAYGAT(bi%ILG, bi%IGND), &
             TBASROW(bi%NA, bi%NTYPE), &
             CMAIROW(bi%NA, bi%NTYPE), TACROW(bi%NA, bi%NTYPE), &
             QACROW(bi%NA, bi%NTYPE), WSNOROW(bi%NA, bi%NTYPE), &
             TPNDGAT(bi%ILG), ZPNDGAT(bi%ILG), TBASGAT(bi%ILG), &
             ALBSGAT(bi%ILG), TSNOGAT(bi%ILG), RHOSGAT(bi%ILG), &
             SNOGAT(bi%ILG), TCANGAT(bi%ILG), RCANGAT(bi%ILG), &
             SCANGAT(bi%ILG), &
             GROGAT(bi%ILG), FRZCGAT(bi%ILG), CMAIGAT(bi%ILG), TACGAT(bi%ILG), &
             QACGAT(bi%ILG), WSNOGAT(bi%ILG), &
             TSFSROW(bi%NA, bi%NTYPE, 4), &
             TSFSGAT(bi%ILG, 4), stat = PAS)

!> PBSM PROGNOSTIC VARIABLES
    allocate(DrySnowROW(bi%NA, bi%NTYPE), SnowAgeROW(bi%NA, bi%NTYPE), &
             DrySnowGAT(bi%ILG), SnowAgeGAT(bi%ILG), &
             TSNOdsROW(bi%NA, bi%NTYPE), RHOSdsROW(bi%NA, bi%NTYPE), &
             TSNOdsGAT(bi%ILG), RHOSdsGAT(bi%ILG), &
             DriftROW(bi%NA, bi%NTYPE), SublROW(bi%NA, bi%NTYPE), DepositionROW(bi%NA, bi%NTYPE), &
             DriftGAT(bi%ILG), SublGAT(bi%ILG), DepositionGAT(bi%ILG), &
             ZSNOCS(bi%ILG), ZSNOGS(bi%ILG), &
             ZSNOWC(bi%ILG), ZSNOWG(bi%ILG), &
             HCPSCS(bi%ILG), HCPSGS(bi%ILG), &
             HCPSC(bi%ILG), HCPSG(bi%ILG), &
             TSNOWC(bi%ILG), TSNOWG(bi%ILG), &
             RHOSC(bi%ILG), RHOSG(bi%ILG), &
             XSNOWC(bi%ILG), XSNOWG(bi%ILG), &
             XSNOCS(bi%ILG), XSNOGS(bi%ILG), stat = PAS)

!> LAND SURFACE PROGNOSTIC VARIABLES (for Basin_average_water_balance.csv):
    allocate(TOTAL_THLQ(bi%IGND), TOTAL_THIC(bi%IGND), &
             TOTAL_THLQ_M(bi%IGND), TOTAL_THIC_M(bi%IGND), stat = PAS)

    if (PAS /= 0) then
        print 1114, 'land surface prognostic'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Soil layers', bi%IGND
        stop
    end if

!> **********************************************************************
!>  For cacluating the subbasin grids
!> **********************************************************************

    allocate(SUBBASIN(bi%ILG), stat = PAS)

    if (PAS /= 0) then
        print 1114, 'subbasin grid'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Total tile elements', bi%ILG
        stop
    end if

!     * GATHER-SCATTER COUNTS:
    allocate(bi%ILMOS(bi%ILG), bi%JLMOS(bi%ILG), bi%IWMOS(bi%ILG), &
             bi%JWMOS(bi%ILG), stat = PAS)
    if (PAS /= 0) then
        print 1114, 'gather-scatter count'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Total tile elements', bi%ILG
        stop
    end if

    allocate(FCANGAT(bi%ILG, ICP1), LNZ0GAT(bi%ILG, ICP1), &
             ALVCGAT(bi%ILG, ICP1), ALICGAT(bi%ILG, ICP1), &
             PAIDROW(bi%NA, bi%NTYPE, ICAN), &
             HGTDROW(bi%NA, bi%NTYPE, ICAN), ACVDROW(bi%NA, bi%NTYPE, ICAN), &
             ACIDROW(bi%NA, bi%NTYPE, ICAN), &
             PAMXGAT(bi%ILG, ICAN), PAMNGAT(bi%ILG, ICAN), &
             CMASGAT(bi%ILG, ICAN), ROOTGAT(bi%ILG, ICAN), &
             RSMNGAT(bi%ILG, ICAN), QA50GAT(bi%ILG, ICAN), &
             VPDAGAT(bi%ILG, ICAN), VPDBGAT(bi%ILG, ICAN), &
             PSGAGAT(bi%ILG, ICAN), &
             PSGBGAT(bi%ILG, ICAN), PAIDGAT(bi%ILG, ICAN), &
             HGTDGAT(bi%ILG, ICAN), ACVDGAT(bi%ILG, ICAN), &
             ACIDGAT(bi%ILG, ICAN), &
             THPROW(bi%NA, bi%NTYPE, bi%IGND), THRROW(bi%NA, bi%NTYPE, bi%IGND), &
             THMROW(bi%NA, bi%NTYPE, bi%IGND), &
             BIROW(bi%NA, bi%NTYPE, bi%IGND), PSISROW(bi%NA, bi%NTYPE, bi%IGND), &
             GRKSROW(bi%NA, bi%NTYPE, bi%IGND), THRAROW(bi%NA, bi%NTYPE, bi%IGND), &
             HCPSROW(bi%NA, bi%NTYPE, bi%IGND), TCSROW(bi%NA, bi%NTYPE, bi%IGND), &
             THFCROW(bi%NA, bi%NTYPE, bi%IGND), &
             PSIWROW(bi%NA, bi%NTYPE, bi%IGND), DLZWROW(bi%NA, bi%NTYPE, bi%IGND), &
             ZBTWROW(bi%NA, bi%NTYPE, bi%IGND), &
             THPGAT(bi%ILG, bi%IGND), THRGAT(bi%ILG, bi%IGND), &
             THMGAT(bi%ILG, bi%IGND), &
             BIGAT(bi%ILG, bi%IGND), PSISGAT(bi%ILG, bi%IGND), &
             GRKSGAT(bi%ILG, bi%IGND), THRAGAT(bi%ILG, bi%IGND), &
             HCPSGAT(bi%ILG, bi%IGND), TCSGAT(bi%ILG, bi%IGND), &
             THFCGAT(bi%ILG, bi%IGND), &
             PSIWGAT(bi%ILG, bi%IGND), DLZWGAT(bi%ILG, bi%IGND), &
             ZBTWGAT(bi%ILG, bi%IGND), GFLXGAT(bi%ILG, bi%IGND), &
             WFSFROW(bi%NA, bi%NTYPE),  ALGWROW(bi%NA, bi%NTYPE), &
             ALGDROW(bi%NA, bi%NTYPE), ASVDROW(bi%NA, bi%NTYPE), ASIDROW(bi%NA, bi%NTYPE), &
             AGVDROW(bi%NA, bi%NTYPE), &
             AGIDROW(bi%NA, bi%NTYPE), &
             DRNGAT(bi%ILG), XSLPGAT(bi%ILG), XDGAT(bi%ILG), &
             WFSFGAT(bi%ILG), KSGAT(bi%ILG), ALGWGAT(bi%ILG), &
             ALGDGAT(bi%ILG), ASVDGAT(bi%ILG), ASIDGAT(bi%ILG), &
             AGVDGAT(bi%ILG), &
             AGIDGAT(bi%ILG), ZSNLGAT(bi%ILG), ZPLGGAT(bi%ILG), &
             ZPLSGAT(bi%ILG), SDEPGAT(bi%ILG), FAREGAT(bi%ILG), &
             ISNDROW(bi%NA, bi%NTYPE, bi%IGND), IORG(bi%NA, bi%NTYPE, bi%IGND), &
             ISNDGAT(bi%ILG, bi%IGND), IGDRROW(bi%NA,bi%NTYPE), &
             IGDRGAT(bi%ILG), &
             fetchGAT(bi%ILG),HtGAT(bi%ILG),N_SGAT(bi%ILG),A_SGAT(bi%ILG), &
             DistribGAT(bi%ILG),stat = PAS)

    if (PAS /= 0) then
        print 1114, 'canopy and soil info.'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Total tile elements', bi%ILG
        print 1118, 'Canopy types with urban areas', ICP1
        print 1118, 'Canopy types', ICAN
        print 1118, 'Soil layers', bi%IGND
        stop
    end if

!> WATROF FLAGS AND VARIABLES:
    allocate(DDGAT(bi%ILG), MANNGAT(bi%ILG), stat = PAS)
    if (PAS /= 0) then
        print 1114, 'WATROF'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Total tile elements', bi%ILG
        stop
    end if

!> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
    allocate(ZDMGRD(bi%NA), &
             ZDHGRD(bi%NA), RADJGRD(bi%NA), &
             CSZGRD(bi%NA), &
             PADRGRD(bi%NA), VPDGRD(bi%NA), &
             TADPGRD(bi%NA), RHOAGRD(bi%NA), RPCPGRD(bi%NA), TRPCGRD(bi%NA), &
             SPCPGRD(bi%NA), TSPCGRD(bi%NA), RHSIGRD(bi%NA), &
             FCLOGRD(bi%NA), DLONGRD(bi%NA), Z0ORGRD(bi%NA), GGEOGRD(bi%NA), UVGRD(bi%NA), &
             XDIFFUS(bi%NA), &
             RPREGRD(bi%NA), SPREGRD(bi%NA), VMODGRD(bi%NA), &
             ZRFMGAT(bi%ILG), ZRFHGAT(bi%ILG), ZDMGAT(bi%ILG), &
             ZDHGAT(bi%ILG), ZBLDGAT(bi%ILG), &
             RADJGAT(bi%ILG), CSZGAT(bi%ILG), &
             RPREGAT(bi%ILG), SPREGAT(bi%ILG), &
             PADRGAT(bi%ILG), VPDGAT(bi%ILG), &
             TADPGAT(bi%ILG), RHOAGAT(bi%ILG), RPCPGAT(bi%ILG), &
             TRPCGAT(bi%ILG), SPCPGAT(bi%ILG), TSPCGAT(bi%ILG), &
             RHSIGAT(bi%ILG), &
             FCLOGAT(bi%ILG), DLONGAT(bi%ILG), Z0ORGAT(bi%ILG), &
             GGEOGAT(bi%ILG), VMODGAT(bi%ILG), stat = PAS)
    if (PAS /= 0) then
        print 1114, 'atmospheric and grid-cst.'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Total tile elements', bi%ILG
        stop
    end if

!> LAND SURFACE DIAGNOSTIC VARIABLES:
    allocate(CDHROW(bi%NA, bi%NTYPE), CDMROW(bi%NA, bi%NTYPE), &
             HFSROW(bi%NA, bi%NTYPE), &
             TFXROW(bi%NA, bi%NTYPE), QEVPROW(bi%NA, bi%NTYPE), QFSROW(bi%NA, bi%NTYPE), &
             QFXROW(bi%NA, bi%NTYPE), PETROW(bi%NA, bi%NTYPE), GAROW(bi%NA, bi%NTYPE), &
             EFROW(bi%NA, bi%NTYPE), GTROW(bi%NA, bi%NTYPE), &
             QGROW(bi%NA, bi%NTYPE), TSFROW(bi%NA, bi%NTYPE), ALVSROW(bi%NA, bi%NTYPE), &
             ALIRROW(bi%NA, bi%NTYPE), FSNOROW(bi%NA, bi%NTYPE), SFCTROW(bi%NA, bi%NTYPE), &
             SFCUROW(bi%NA, bi%NTYPE), &
             SFCVROW(bi%NA, bi%NTYPE), SFCQROW(bi%NA, bi%NTYPE), FSGVROW(bi%NA, bi%NTYPE), &
             FSGSROW(bi%NA, bi%NTYPE), FSGGROW(bi%NA, bi%NTYPE), FLGVROW(bi%NA, bi%NTYPE), &
             FLGSROW(bi%NA, bi%NTYPE), &
             FLGGROW(bi%NA, bi%NTYPE), HFSCROW(bi%NA, bi%NTYPE), HFSSROW(bi%NA, bi%NTYPE), &
             HFSGROW(bi%NA, bi%NTYPE), HEVCROW(bi%NA, bi%NTYPE), HEVSROW(bi%NA, bi%NTYPE), &
             HEVGROW(bi%NA, bi%NTYPE), &
             HMFCROW(bi%NA, bi%NTYPE), HMFNROW(bi%NA, bi%NTYPE), HTCCROW(bi%NA, bi%NTYPE), &
             HTCSROW(bi%NA, bi%NTYPE), PCFCROW(bi%NA, bi%NTYPE), PCLCROW(bi%NA, bi%NTYPE), &
             PCPNROW(bi%NA, bi%NTYPE), &
             PCPGROW(bi%NA, bi%NTYPE), QFGROW(bi%NA, bi%NTYPE), QFNROW(bi%NA, bi%NTYPE), &
             QFCLROW(bi%NA, bi%NTYPE), QFCFROW(bi%NA, bi%NTYPE), ROFROW(bi%NA, bi%NTYPE), &
             ROFOROW(bi%NA, bi%NTYPE), &
             ROFSROW(bi%NA, bi%NTYPE), ROFBROW(bi%NA, bi%NTYPE), ROFCROW(bi%NA, bi%NTYPE), &
             ROFNROW(bi%NA, bi%NTYPE), ROVGROW(bi%NA, bi%NTYPE), WTRCROW(bi%NA, bi%NTYPE), &
             WTRSROW(bi%NA, bi%NTYPE), &
             WTRGROW(bi%NA, bi%NTYPE), DRROW(bi%NA, bi%NTYPE), WTABROW(bi%NA, bi%NTYPE), &
             ILMOROW(bi%NA, bi%NTYPE), UEROW(bi%NA, bi%NTYPE), HBLROW(bi%NA, bi%NTYPE), &
             TROFROW(bi%NA, bi%NTYPE), &
             TROOROW(bi%NA, bi%NTYPE), TROSROW(bi%NA, bi%NTYPE), TROBROW(bi%NA, bi%NTYPE), &
             CDHGAT(bi%ILG), CDMGAT(bi%ILG), HFSGAT(bi%ILG), &
             TFXGAT(bi%ILG), QEVPGAT(bi%ILG), QFSGAT(bi%ILG), &
             QFXGAT(bi%ILG), PETGAT(bi%ILG), GAGAT(bi%ILG), &
             EFGAT(bi%ILG), GTGAT(bi%ILG), &
             QGGAT(bi%ILG), ALVSGAT(bi%ILG), &
             ALIRGAT(bi%ILG), FSNOGAT(bi%ILG), SFRHGAT(bi%ILG), SFCTGAT(bi%ILG), &
             SFCUGAT(bi%ILG), &
             SFCVGAT(bi%ILG), SFCQGAT(bi%ILG), FSGVGAT(bi%ILG), &
             FSGSGAT(bi%ILG), FSGGGAT(bi%ILG), FLGVGAT(bi%ILG), &
             FLGSGAT(bi%ILG), &
             FLGGGAT(bi%ILG), HFSCGAT(bi%ILG), HFSSGAT(bi%ILG), &
             HFSGGAT(bi%ILG), HEVCGAT(bi%ILG), HEVSGAT(bi%ILG), &
             HEVGGAT(bi%ILG), &
             HMFCGAT(bi%ILG), HMFNGAT(bi%ILG), HTCCGAT(bi%ILG), &
             HTCSGAT(bi%ILG), PCFCGAT(bi%ILG), PCLCGAT(bi%ILG), &
             PCPNGAT(bi%ILG), &
             PCPGGAT(bi%ILG), QFGGAT(bi%ILG), QFNGAT(bi%ILG), &
             QFCLGAT(bi%ILG), QFCFGAT(bi%ILG), ROFGAT(bi%ILG), &
             ROFOGAT(bi%ILG), &
             ROFSGAT(bi%ILG), ROFBGAT(bi%ILG), ROFCGAT(bi%ILG), &
             ROFNGAT(bi%ILG), ROVGGAT(bi%ILG), WTRCGAT(bi%ILG), &
             WTRSGAT(bi%ILG), &
             WTRGGAT(bi%ILG), DRGAT(bi%ILG), WTABGAT(bi%ILG), &
             ILMOGAT(bi%ILG), UEGAT(bi%ILG), HBLGAT(bi%ILG), QLWOGAT(bi%ILG), &
             FTEMP(bi%ILG),   FVAP(bi%ILG),  RIB(bi%ILG), TROFGAT(bi%ILG), &
             TROOGAT(bi%ILG), TROSGAT(bi%ILG), TROBGAT(bi%ILG), &
             CDHGRD(bi%NA), CDMGRD(bi%NA), HFSGRD(bi%NA), &
             TFXGRD(bi%NA), QEVPGRD(bi%NA), QFSGRD(bi%NA), QFXGRD(bi%NA), PETGRD(bi%NA), &
             GAGRD(bi%NA), EFGRD(bi%NA), GTGRD(bi%NA), &
             QGGRD(bi%NA), TSFGRD(bi%NA), ALVSGRD(bi%NA), ALIRGRD(bi%NA), FSNOGRD(bi%NA), &
             SFCTGRD(bi%NA), SFCUGRD(bi%NA), &
             SFCVGRD(bi%NA), SFCQGRD(bi%NA), FSGVGRD(bi%NA), FSGSGRD(bi%NA), &
             FSGGGRD(bi%NA), FLGVGRD(bi%NA), FLGSGRD(bi%NA), &
             FLGGGRD(bi%NA), HFSCGRD(bi%NA), HFSSGRD(bi%NA), HFSGGRD(bi%NA), &
             HEVCGRD(bi%NA), HEVSGRD(bi%NA), HEVGGRD(bi%NA), &
             HMFCGRD(bi%NA), HMFNGRD(bi%NA), HTCCGRD(bi%NA), HTCSGRD(bi%NA), &
             PCFCGRD(bi%NA), PCLCGRD(bi%NA), PCPNGRD(bi%NA), &
             PCPGGRD(bi%NA), QFGGRD(bi%NA), QFNGRD(bi%NA), QFCLGRD(bi%NA), QFCFGRD(bi%NA), &
             ROFGRD(bi%NA), ROFOGRD(bi%NA), &
             ROFSGRD(bi%NA), ROFBGRD(bi%NA), ROFCGRD(bi%NA), ROFNGRD(bi%NA), &
             ROVGGRD(bi%NA), WTRCGRD(bi%NA), WTRSGRD(bi%NA), &
             WTRGGRD(bi%NA), DRGRD(bi%NA), WTABGRD(bi%NA), ILMOGRD(bi%NA), UEGRD(bi%NA), &
             HBLGRD(bi%NA), &
             HMFGROW(bi%NA, bi%NTYPE, bi%IGND), HTCROW(bi%NA, bi%NTYPE, bi%IGND), &
             QFCROW(bi%NA, bi%NTYPE, bi%IGND), GFLXROW(bi%NA, bi%NTYPE, bi%IGND), &
             HMFGGAT(bi%ILG, bi%IGND), HTCGAT(bi%ILG, bi%IGND), &
             QFCGAT(bi%ILG, bi%IGND), &
             HMFGGRD(bi%NA, bi%IGND), HTCGRD(bi%NA, bi%IGND), QFCGRD(bi%NA, bi%IGND), &
             GFLXGRD(bi%NA, bi%IGND), &
             ITCTROW(bi%NA, bi%NTYPE, 6, 50), &
             ITCTGAT(bi%ILG, 6, 50), stat = PAS)
    if (PAS /= 0) then
        print 1114, 'land surface diagnostic'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Total tile elements', bi%ILG
        print 1118, 'Soil layers', bi%IGND
        stop
    end if

!> OUTPUT VARIABLES:
    allocate(PREACC(bi%NA), GTACC(bi%NA), QEVPACC(bi%NA), &
             HFSACC(bi%NA), ROFACC(bi%NA), SNOACC(bi%NA), ALVSACC(bi%NA), ALIRACC(bi%NA), &
             FSINACC(bi%NA), FLINACC(bi%NA), &
             TAACC(bi%NA), UVACC(bi%NA), PRESACC(bi%NA), QAACC(bi%NA), EVAPACC(bi%NA), &
             FLUTACC(bi%NA), ROFOACC(bi%NA), &
             ROFSACC(bi%NA), ROFBACC(bi%NA), HMFNACC(bi%NA), WTBLACC(bi%NA), ZPNDACC(bi%NA), &
             WSNOACC(bi%NA), RHOSACC(bi%NA), TSNOACC(bi%NA), &
             TCANACC(bi%NA), RCANACC(bi%NA), SCANACC(bi%NA), GROACC(bi%NA), CANARE(bi%NA), &
             SNOARE(bi%NA), &
             TBARACC(bi%NA, bi%IGND), THLQACC(bi%NA, bi%IGND), THICACC(bi%NA, bi%IGND), &
             THALACC(bi%NA, bi%IGND), GFLXACC(bi%NA, bi%IGND), &
             STG_I(bi%NA), DSTG(bi%NA), THLQ_FLD(bi%NA, bi%IGND), THIC_FLD(bi%NA, bi%IGND), &
             stat = PAS)
    if (PAS /= 0) then
        print 1114, 'accumulator'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'Soil layers', bi%IGND
        stop
    end if

!> CROSS-CLASS VARIABLES (CLASS):
    allocate(TBARC(bi%ILG, bi%IGND), TBARG(bi%ILG, bi%IGND), &
             TBARCS(bi%ILG, bi%IGND), &
             TBARGS(bi%ILG, bi%IGND), THLIQC(bi%ILG, bi%IGND), &
             THLIQG(bi%ILG, bi%IGND), THICEC(bi%ILG, bi%IGND), &
             THICEG(bi%ILG, bi%IGND), FROOT(bi%ILG, bi%IGND), &
             HCPC(bi%ILG, bi%IGND), HCPG(bi%ILG, bi%IGND), &
             TCTOPC(bi%ILG, bi%IGND), TCBOTC(bi%ILG, bi%IGND), &
             TCTOPG(bi%ILG, bi%IGND), TCBOTG(bi%ILG, bi%IGND), &
             FC(bi%ILG), FG(bi%ILG), FCS(bi%ILG), &
             FGS(bi%ILG), RBCOEF(bi%ILG), &
             ZSNOW(bi%ILG), &
             FSVF(bi%ILG), FSVFS(bi%ILG), ALVSCN(bi%ILG), &
             ALIRCN(bi%ILG), ALVSG(bi%ILG), &
             ALIRG(bi%ILG), ALVSCS(bi%ILG), ALIRCS(bi%ILG), &
             ALVSSN(bi%ILG), ALIRSN(bi%ILG), ALVSGC(bi%ILG), &
             ALIRGC(bi%ILG), ALVSSC(bi%ILG), &
             ALIRSC(bi%ILG), TRVSCN(bi%ILG), TRIRCN(bi%ILG), &
             TRVSCS(bi%ILG), TRIRCS(bi%ILG), RC(bi%ILG), &
             RCS(bi%ILG), FRAINC(bi%ILG), &
             FSNOWC(bi%ILG),FRAICS(bi%ILG),FSNOCS(bi%ILG), &
             CMASSC(bi%ILG), CMASCS(bi%ILG), &
             DISP(bi%ILG), DISPS(bi%ILG), ZOMLNC(bi%ILG), &
             ZOELNC(bi%ILG), ZOMLNG(bi%ILG), &
             ZOELNG(bi%ILG), ZOMLCS(bi%ILG), ZOELCS(bi%ILG), &
             ZOMLNS(bi%ILG), ZOELNS(bi%ILG), TRSNOW(bi%ILG), &
             CHCAP(bi%ILG), CHCAPS(bi%ILG), &
             GZEROC(bi%ILG), GZEROG(bi%ILG), GZROCS(bi%ILG), &
             GZROGS(bi%ILG), G12C(bi%ILG), G12G(bi%ILG), &
             G12CS(bi%ILG), G12GS(bi%ILG), G23C(bi%ILG), &
             G23G(bi%ILG), G23CS(bi%ILG), G23GS(bi%ILG), &
             QFREZC(bi%ILG), QFREZG(bi%ILG), QMELTC(bi%ILG), &
             QMELTG(bi%ILG), EVAPC(bi%ILG), &
             EVAPCG(bi%ILG), EVAPG(bi%ILG), EVAPCS(bi%ILG), &
             EVPCSG(bi%ILG), EVAPGS(bi%ILG), TCANO(bi%ILG), &
             TCANS(bi%ILG), RAICAN(bi%ILG), &
             SNOCAN(bi%ILG), RAICNS(bi%ILG), SNOCNS(bi%ILG), &
             CWLCAP(bi%ILG), CWFCAP(bi%ILG), CWLCPS(bi%ILG), &
             CWFCPS(bi%ILG), TSNOCS(bi%ILG), &
             TSNOGS(bi%ILG), RHOSCS(bi%ILG), RHOSGS(bi%ILG), &
             WSNOCS(bi%ILG), WSNOGS(bi%ILG), TPONDC(bi%ILG), &
             TPONDG(bi%ILG), TPNDCS(bi%ILG), &
             TPNDGS(bi%ILG), ZPLMCS(bi%ILG), ZPLMGS(bi%ILG), &
             ZPLIMC(bi%ILG), ZPLIMG(bi%ILG), stat = PAS)
    if (PAS /= 0) then
        print 1114, 'cross-CLASS'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Total tile elements', bi%ILG
        print 1118, 'Soil layers', bi%IGND
        stop
    end if

!> BALANCE ERRORS (CLASS):
    allocate(CTVSTP(bi%ILG), CTSSTP(bi%ILG), &
             CT1STP(bi%ILG), &
             CT2STP(bi%ILG), CT3STP(bi%ILG), WTVSTP(bi%ILG), &
             WTSSTP(bi%ILG), WTGSTP(bi%ILG), stat = PAS)
    if (PAS /= 0) then
        print 1114, 'balance error diagnostic'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Total tile elements', bi%ILG
        stop
    end if

!> CTEM ERRORS (CLASS):
    allocate(CO2CONC(bi%ILG), COSZS(bi%ILG), XDIFFUSC(bi%ILG), CFLUXCG(bi%ILG), CFLUXCS(bi%ILG), &
             AILCG(bi%ILG, ICTEM), AILCGS(bi%ILG, ICTEM), FCANC(bi%ILG, ICTEM), FCANCS(bi%ILG, ICTEM), &
             CO2I1CG(bi%ILG, ICTEM), CO2I1CS(bi%ILG, ICTEM), CO2I2CG(bi%ILG, ICTEM), CO2I2CS(bi%ILG, ICTEM), &
             SLAI(bi%ILG, ICTEM), FCANCMX(bi%ILG, ICTEM), ANCSVEG(bi%ILG, ICTEM), ANCGVEG(bi%ILG, ICTEM), &
             RMLCSVEG(bi%ILG, ICTEM), RMLCGVEG(bi%ILG, ICTEM), &
             AILC(bi%ILG, ICAN), PAIC(bi%ILG, ICAN), FIELDSM(bi%ILG, bi%IGND), WILTSM(bi%ILG, bi%IGND), &
             RMATCTEM(bi%ILG, ICTEM, bi%IGND), RMATC(bi%ILG, ICAN, bi%IGND), NOL2PFTS(ICAN), stat = PAS)
    if (PAS /= 0) then
        print 1114, 'CTEM'
        print 1118, 'Grid squares', bi%NA
        print 1118, 'GRUs', bi%NTYPE
        print 1118, 'Total tile elements', bi%ILG
        print 1118, 'Canopy types', ICAN
        print 1118, 'Soil layers', bi%IGND
        print 1118, 'CTEM flag', ICTEM
        stop
    end if
!>
!>*******************************************************************
!>
!> *********************************************************************
!>  Open additional output files
!> *********************************************************************
    if (ipid == 0 .and. BASINSWEOUTFLAG > 0) then
        open(85, file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/basin_SCA_alldays.csv')
        open(86, file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/basin_SWE_alldays.csv')
    end if !(BASINSWEOUTFLAG > 0) then

!> CLASS requires that each GRU for each grid square has its own parameter value,
!> for MESH the value read in from the parameter file is assumed to be valid for
!> all grid squares in the study area - Frank Seglenieks Aug 2007

!> bjd - This would be a good spot for setting pre-distributed values

    do i = 2, bi%NA
        do m = 1, NMTEST
            do j = 1, ICP1
                cp%FCANROW(i, m, j) = cp%FCANROW(1, m, j)
                cp%LNZ0ROW(i, m, j) = cp%LNZ0ROW(1, m, j)
                cp%ALVCROW(i, m, j) = cp%ALVCROW(1, m, j)
                cp%ALICROW(i, m, j) = cp%ALICROW(1, m, j)
            end do
            do j = 1, ICAN
                cp%PAMXROW(i, m, j) = cp%PAMXROW(1, m, j)
                cp%PAMNROW(i, m, j) = cp%PAMNROW(1, m, j)
                cp%CMASROW(i, m, j) = cp%CMASROW(1, m, j)
                cp%ROOTROW(i, m, j) = cp%ROOTROW(1, m, j)
                cp%RSMNROW(i, m, j) = cp%RSMNROW(1, m, j)
                cp%QA50ROW(i, m, j) = cp%QA50ROW(1, m, j)
                cp%VPDAROW(i, m, j) = cp%VPDAROW(1, m, j)
                cp%VPDBROW(i, m, j) = cp%VPDBROW(1, m, j)
                cp%PSGAROW(i, m, j) = cp%PSGAROW(1, m, j)
                cp%PSGBROW(i, m, j) = cp%PSGBROW(1, m, j)
            end do
            do j = 1, bi%IGND
                cp%SANDROW(i, m, j) = cp%SANDROW(1, m, j)
                cp%CLAYROW(i, m, j) = cp%CLAYROW(1, m, j)
                cp%ORGMROW(i, m, j) = cp%ORGMROW(1, m, j)
!> note333 see read_s_temperature_txt.f for more TBARROW information
                cp%TBARROW(i, m, j) = cp%TBARROW(1, m, j)
!> note444 see read_s_moisture_txt.f for more THLQROW information
                cp%THLQROW(i, m, j) = cp%THLQROW(1, m, j)
                cp%THICROW(i, m, j) = cp%THICROW(1, m, j)
            end do
            cp%TCANROW(i, m) = cp%TCANROW(1, m)
            cp%TSNOROW(i, m) = cp%TSNOROW(1, m)
            cp%DRNROW(i, m) = cp%DRNROW(1, m)
            cp%SDEPROW(i, m) = cp%SDEPROW(1, m)
            cp%FAREROW(i, m) = cp%FAREROW(1, m)
            cp%MANNROW(i, m) = cp%MANNROW(1, m)
!> note, if drdn (drainage density) is provided from the Mesh_drainage_database.r2c
!> we give the same value for all the GRU that are in one cell    
            if (allocated(bi%SLOPE)) then
		        cp%XSLPROW(i, m) = bi%SLOPE(i)
		        if (i == 2) then
                    cp%XSLPROW(i - 1, m) = bi%SLOPE(i - 1)
                end if
            else
                cp%XSLPROW(i, m) = cp%XSLPROW(1, m)
            end if
            cp%XDROW(i, m) = cp%XDROW(1, m)
!> note, if drdn (drainage density) is provided from the Mesh_drainage_database.r2c
!> we give the same value for all the GRU that are in one cell
            if (allocated(bi%DRDN)) then
                if (i == 2) then
                    cp%DDROW(i - 1, m) = bi%DRDN(i - 1)
                end if
                cp%DDROW(i, m) = bi%DRDN(i)
            else
                cp%DDROW(i, m) = cp%DDROW(1, m)
            end if
            WFSFROW(i, m) = WFSFROW(1, m)
            cp%KSROW(i, m) = cp%KSROW(1, m)
            cp%MIDROW(i, m) = cp%MIDROW(1, m)
            cp%TPNDROW(i, m) = cp%TPNDROW(1, m)
            cp%ZPNDROW(i, m) = cp%ZPNDROW(1, m)
            cp%RCANROW(i, m) = cp%RCANROW(1, m)
            cp%SCANROW(i, m) = cp%SCANROW(1, m)
            cp%SNOROW(i, m) = cp%SNOROW(1, m)
            cp%ALBSROW(i, m) = cp%ALBSROW(1, m)
            cp%RHOSROW(i, m) = cp%RHOSROW(1, m)
            cp%GROROW(i, m) = cp%GROROW(1, m)
        end do !m = 1, NMTEST
    end do !i = 2, bi%NA

!> *********************************************************************
!>  Open and read in values from MESH_input_reservoir.txt file
!> *********************************************************************

    open(21, file = 'MESH_input_reservoir.txt', status = 'old', action = 'read')
    read(21, '(3i5)') WF_NORESV, WF_NREL, WF_KTR
    WF_NORESV_CTRL = 0

! allocate reservoir arrays
    M_R = WF_NORESV
    allocate(WF_IRES(M_R), WF_JRES(M_R), WF_RES(M_R), WF_R(M_R), WF_B1(M_R), WF_B2(M_R), &
             WF_QREL(M_R), WF_RESSTORE(M_R), WF_RESNAME(M_R))

    if (WF_NORESV > 0) then
        do i = 1, WF_NORESV
! KCK Added to allow higher precision gauge sites    
            if (LOCATIONFLAG == 1) then
                read(21, '(2f7.1, 2g10.3, 25x, a12, i2)') I_G, J_G, WF_B1(i), WF_B2(i), WF_RESNAME(i), WF_RES(i)
                WF_IRES(i) = nint((I_G - bi%yOrigin*60.0)/bi%GRDN)
                WF_JRES(i) = nint((J_G - bi%xOrigin*60.0)/bi%GRDE)
            else
                read(21, '(2i5, 2g10.3, 25x, a12, i2)') WF_IRES(i), WF_JRES(i), WF_B1(i), WF_B2(i), WF_RESNAME(i), WF_RES(i)
                WF_IRES(i) = int((real(WF_IRES(i)) - real(bi%iyMin))/bi%GRDN + 1.0)
                WF_JRES(i) = int((real(WF_JRES(i)) - real(bi%jxMin))/bi%GRDE + 1.0)
            end if
!> check if point is in watershed and in river reaches
            WF_R(i) = 0
            do j = 1, bi%NA
                if (WF_IRES(i) == bi%yyy(j) .and. WF_JRES(i) == bi%xxx(j)) then
                    WF_R(i) = j
                end if
            end do
            if (WF_R(i) == 0) then
                print *, 'Reservoir Station: ', i, ' is not in the basin'
                print *, 'Up/Down Coordinate: ', wf_ires(i), bi%iyMin
                print *, 'Left/Right Coordinate: ', wf_jres(i), bi%jxMin
                stop
            end if
            if (bi%IREACH(WF_R(i)) /= i) then
                print *, 'Reservoir Station: ', i, ' is not in the correct reach'
                print *, 'Up/Down Coordinate: ', wf_ires(i)
                print *, 'Left/Right Coordinate: ', wf_jres(i)
                print *, 'ireach value at station: ', wf_iy(i)
                stop
            end if
            if (WF_B1(i) == 0.0) then
                WF_NORESV_CTRL = WF_NORESV_CTRL + 1
            end if
        end do
    end if
!> leave file open and read in the reservoir files when needed

!> *********************************************************************
!> Open and read in values from MESH_input_streamflow.txt file
!> *********************************************************************
    open(22, file = 'MESH_input_streamflow.txt', status = 'old', action = 'read')
    read(22, *)
    read(22, *) WF_NO, WF_NL, WF_MHRD, WF_KT, WF_START_YEAR, WF_START_DAY, WF_START_HOUR

! Allocate variable based on value from streamflow file
    M_S = WF_NO !todo M_S is same as WF_NO and could be removed.

    allocate(WF_IY(M_S), WF_JX(M_S), WF_S(M_S), WF_QHYD(M_S), WF_QHYD_AVG(M_S), WF_QHYD_CUM(M_S), &
             WF_QSYN(M_S), WF_QSYN_AVG(M_S), WF_QSYN_CUM(M_S), WF_GAGE(M_S))

    do i = 1, WF_NO
        if (LOCATIONFLAG == 1) then
            read(22, *) I_G, J_G, WF_GAGE(i)
            WF_IY(i) = nint((I_G - bi%yOrigin*60.0)/bi%GRDN)
            WF_JX(i) = nint((J_G - bi%xOrigin*60.0)/bi%GRDE)
        else
            read(22, *) WF_IY(i), WF_JX(i), WF_GAGE(i)
            WF_IY(i) = int((real(WF_IY(i)) - real(bi%iyMin))/bi%GRDN + 1.0)
            WF_JX(i) = int((real(WF_JX(i)) - real(bi%jxMin))/bi%GRDE + 1.0)
        end if
    end do
    do i = 1, WF_NO
        WF_S(i) = 0
        do j = 1, bi%NA
            if (WF_JX(i) == bi%xxx(j) .and. WF_IY(i) == bi%yyy(j)) then
                WF_S(i) = j
            end if
        end do
        if (WF_S(i) == 0) then
            print *, 'STREAMFLOW GAUGE: ', i, ' IS NOT IN THE BASIN'
            print *, 'UP/DOWN', WF_IY(i), bi%iyMin, bi%yyy(j), bi%yCount
            print *, 'LEFT/RIGHT', WF_JX(i), bi%jxMin, bi%xxx(j), bi%xCount
            stop
        end if
    end do

!> ric     initialise smoothed variables
    wf_qsyn = 0.0
    WF_QSYN_AVG = 0.0
    wf_qhyd_avg = 0.0
    wf_qsyn_cum = 0.0
    wf_qhyd_cum = 0.0

!>MAM - The first stream flow record is used for flow initialization
    read(22, *, iostat = IOS) (WF_QHYD(i), i = 1, WF_NO)

      ! fixed streamflow start time bug. add in function to enable the
      ! correct start time. Feb2009 aliu.
    call Julian_Day_ID(WF_START_YEAR, WF_START_day, Jday_IND1)
    call Julian_Day_ID(YEAR_START, JDAY_START, Jday_IND2)
!    print *, WF_START_YEAR, WF_START_day, Jday_IND1
    if (YEAR_START == 0) then
        Jday_IND2 = Jday_IND1
    end if
    if (Jday_IND2 < Jday_IND1) then
        print *, 'ERROR: Simulation start date too early, check ', &
            ' MESH_input_streamflow.txt, The start date in ', &
            ' MESH_input_run_options.ini may be out of range'
        stop
    end if
    jday_ind_strm = (jday_ind2 - jday_ind1)*24/WF_KT

         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !skip the unused streamflow records in streamflow.txt .
    do j = 1, jday_ind_strm
        read(22, *, iostat = IOS)
        if (IOS < 0) then
            print *, 'ERROR: end of file reached when reading ', &
                ' MESH_input_streamflow.txt, The start date in ', &
                ' MESH_input_run_options.ini may be out of range'
            stop
        end if
    end do
          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    print *, 'Skipping', jday_ind_strm, 'Registers in streamflow file'
!> leave unit open and read new streamflow each hour

!todo - verify that all checks are needed and in the right spot
!> *********************************************************************
!> Check to make sure input values are consistent
!> *********************************************************************
!> compare land classes in class.ini and drainage database files
    if (bi%NTYPE /= NMTEST .and. bi%NTYPE > 0) then
        print *, 'land classes from MESH_parameters_CLASS.ini: ', NMTEST
        print *, 'land classes from MESH_drainage_database.txt:', bi%NTYPE
        print *, 'Please adjust these values.'
        stop
    end if

!> check that run points are in the basin and that there are no repeats
    do i = 1, WF_NUM_POINTS
        if (op%N_OUT(i) > bi%NA) then
            print *, 'No. of grids from MESH_drainage_database.txt:', bi%NA
            print *, 'out point ', i, ' is: ', op%N_OUT(i)
            print *, 'please adjust MESH_run_options.ini file'
            stop
        end if
        if (i < WF_NUM_POINTS) then
            do j = i + 1, WF_NUM_POINTS
                if (op%N_OUT(i) == op%N_OUT(j) .and. op%II_OUT(i) == op%II_OUT(j)) then
                    print *, 'grid number ', op%N_OUT(i)
                    print *, 'is repeated in MESH_run_options.ini file'
                    print *, 'please adjust MESH_run_options.ini file'
                    stop
                end if
            end do
        end if
    end do

!> *********************************************************************
!> Set some more intial values and clear accumulators
!> *********************************************************************

!> ASSIGN VALUES OF LAT/LONG TO EACH SQUARE:
!> NOTE FROM FRANK
!> I got the equations to determine the actual length of a 
!> degree of latitude and longitude from this paper, thank you 
!> Geoff Kite (I have attached it):
!> http://www.agu.org/pubs/crossref/1994/94WR00231.shtml
!> This chunk of code is a way to put the actual values of 
!> longitude and latitude for each cell in a large basin.  
!> The original CLASS code just put in the same value for each cell.  
!> The problem is that the class.ini file only has a single value 
!> of long and lat (as it was only designed for a point).  So in order 
!> to get the values across the basin I assumed that the single value 
!> from the class.ini file is in the centre of the basin and then use 
!> information from the watflow.shd file to figure out the long/lat 
!> varies across the basin.  However, the watflod.shd file only gives 
!> information in kilometers not degrees of long/lat so I had 
!> to use the formulas from the above paper to go between the two.
!
!> The only value of DEGLAT is the one read in from the class.ini file, 
!> after that Diana uses RADJGRD (the value of latitude in radians) so 
!> after DEGLAT is used to calculate RADJGRD is it no longer used.  This 
!> is how it was in the original CLASS code.

    !> Read an intial value for geothermal flux from file.
    if (GGEOFLAG == 1) then
!        if ((VARIABLEFILESFLAG == 1) .and. (fls%fl(7)%isInit)) then
!            open(fls%fl(7)%unit, file = trim(adjustl(fls%fl(7)%name)))
        iun = fls%fl(mfk%f18)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f18)%fn)), status = 'old', action = 'read', iostat = ios)
!        else
!            open(18, file = 'MESH_ggeo.ini', status = 'old')
!        end if
        read(iun, *) GGEOGRD(1)
        close(iun)
    else
        GGEOGRD(1) = 0.0
    end if

	do i = 1, bi%NA
        !LATLENGTH = bi%AL/1000.0/(111.136 - 0.5623*cos(2*(DEGLAT*PI/180.0)) + 0.0011*cos(4*(DEGLAT*PI/180.0)))
        !LONGLENGTH = bi%AL/1000.0/(111.4172*cos((DEGLAT*PI/180.0)) - 0.094*cos(3*(DEGLAT*PI/180.0)) + 0.0002*cos(5*(DEGLAT*PI/180.0)))
        RADJGRD(i) = ((bi%yOrigin + bi%yDelta*bi%yyy(i)) - bi%yDelta/2.0)*PI/180.0
        DLONGRD(i) = (bi%xOrigin + bi%xDelta*bi%xxx(i)) - bi%xDelta/2.0
        cp%ZRFMGRD(i) = cp%ZRFMGRD(1)
        cp%ZRFHGRD(i) = cp%ZRFHGRD(1)
        cp%ZBLDGRD(i) = cp%ZBLDGRD(1)
        cp%GCGRD(i) = cp%GCGRD(1)
        Z0ORGRD(i) = 0.0
        GGEOGRD(i) = GGEOGRD(1)
        ZDMGRD(i) = 10.0
        ZDHGRD(i) = 2.0
	end do

!> adjust NAA to the be number of outlet squares, as currently it is the
!> number of squares with outlets into other squares in the basin, and
!> we want it to be the number of squares with outlets to outside the
!> basin.
!todo - look into the logic of this and suggest how it could be changed
    bi%NAA = bi%NA - bi%NAA

!> set initial values of ncount and nsum
! NCOUNT = which half-hour period the current time is:
! The first period (0:00-0:30) is #1, the last period (23:30-0:00) is #48
    NCOUNT = HOUR_NOW*2 + MINS_NOW/TIME_STEP_MINS + 1
    NSUM = 1
    NSUM_TOTAL = 1

!> **********************************************************************
!>  Start of section to only run on squares that make up the watersheds
!>  that are listed in the streamflow file (subbasin)
!> **********************************************************************

    if (SUBBASINFLAG > 0) then
        do i = 1, bi%NA
            SUBBASIN(i) = 0
        end do

!> Set values at guages to 1
        do i = 1, WF_NO
            SUBBASIN(WF_S(i)) = 1
        end do

!> Set values of subbasin to 1 for all upstream grids
        SUBBASINCOUNT = 1
        do while (SUBBASINCOUNT > 0)
            SUBBASINCOUNT = 0
            do i = 1, bi%NA - 1
                if (SUBBASIN(bi%NEXT(i)) == 1 .and. SUBBASIN(i) == 0) then
                    SUBBASIN(i) = 1
                    SUBBASINCOUNT = SUBBASINCOUNT + 1
                end if
            end do
        end do !while (SUBBASINCOUNT > 0)

!> Set values of frac to 0 for all grids non-upstream grids
        SUBBASINCOUNT = 0
        do i = 1, bi%NA
            if (SUBBASIN(i) == 0) then
                bi%FRAC(i) = 0.0
            else
                SUBBASINCOUNT = SUBBASINCOUNT + 1
            end if
        end do

  !> MAM - Write grid number, grid fractional area and percentage of GRUs in each grid
        open(10, file = 'subbasin_info.txt')
        write(10, '(a7, 3x, a18, 3x, a58)') &
            'GRID NO', 'GRID AREA FRACTION', 'GRU FRACTIONS, GRU 1, GRU 2, GRU 3,... IN INCREASING ORDER'
        do i = 1, bi%NA
            if (SUBBASIN(i) == 0) then
            else
                write(10, '(i5, 3x, f10.3, 8x, 50(f10.3, 3x))') i, bi%FRAC(i), (bi%ACLASS(i, m), m = 1, NMTEST)
            end if
        end do
        close(10)

    end if !(SUBBASINFLAG > 0) then

!> **********************************************************************
!>  End of subbasin section
!> **********************************************************************

!> Set value of FAREROW:
!todo - flag this as an issue to explore later and hide basin average code
!todo - document the problem
    TOTAL_AREA = 0.0
    do i = 1, bi%NA
        do m = 1, NMTEST
            cp%FAREROW(i, m) = bi%ACLASS(i, m)*bi%FRAC(i)
            TOTAL_AREA = TOTAL_AREA + cp%FAREROW(i, m)
    !FUTUREDO: Bruce, FRAC is calculated by EnSim
    ! using Dan Princz's instructions for EnSim
    ! FRAC can be greater than 1.00
    ! So, we cannot use FAREROW in place of BASIN_FRACTION
        end do
    end do

    call GATPREP(bi%ILMOS, bi%JLMOS, bi%IWMOS, bi%JWMOS, &
                 bi%NML, bi%NMW, cp%GCGRD, cp%FAREROW, cp%MIDROW, &
                 bi%NA, bi%NTYPE, bi%ILG, 1, bi%NA, NMTEST)

    !> Update basin information.
!-    bi%NML = NML
!-    bi%NMW = NMW
!-    allocate(bi%ILMOS(size(ILMOS)), bi%JLMOS(size(JLMOS)))
!-    bi%ILMOS = ILMOS
!-    bi%JLMOS = JLMOS

!> Initialize output variables.
    call init_water_balance(wb, bi)
    wb%grid_area = 0.0
    wb%basin_area = 0.0
    do i = 1, bi%NA
        do m = 1, NMTEST
            wb%grid_area(i) = wb%grid_area(i) + cp%FAREROW(i, m)
        end do
        wb%basin_area = wb%basin_area + wb%grid_area(i)
    end do

    call climate_module_init(bi, ts, cm, ENDDATA)
    if (ENDDATA) goto 999

!> *********************************************************************
!> Initialize water balance output fields
!> *********************************************************************

    if (ipid == 0) then
        call init_energy_balance(eng, bi)
        call init_soil_statevars(sov, bi)
        call init_met_data(md, bi)
        call init_water_balance(wb_h, bi)
        if (OUTFIELDSFLAG == 1) call init_out(vr, ts, iof, bi)
    end if !(ipid == 0) then

!> routing parameters
    WF_ROUTETIMESTEP = 900
    WF_TIMECOUNT = 0
    DRIVERTIMESTEP = DELT    ! Be sure it's REAL*8

!* JAN: The first time throught he loop, jan = 1. Jan will equal 2 after that.
    JAN = 1

!todo - check that this is compatible with Saul's pre-distributed soil moisture and soil temp.
    do i = 1, bi%NA
        do m = 1, NMTEST
            do j = 1, bi%IGND
                cp%TBARROW(i, m, j) = cp%TBARROW(i, m, j) + TFREZ
            end do
            cp%TSNOROW(i, m) = cp%TSNOROW(i, m) + TFREZ
            cp%TCANROW(i, m) = cp%TCANROW(i, m) + TFREZ
            cp%TPNDROW(i, m) = cp%TPNDROW(i, m) + TFREZ
            TBASROW(i, m) = cp%TBARROW(i, m, bi%IGND)
            CMAIROW(i, m) = 0.0
            WSNOROW(i, m) = 0.0
            TSFSROW(i, m, 1) = TFREZ
            TSFSROW(i, m, 2) = TFREZ
            TSFSROW(i, m, 3) = cp%TBARROW(i, m, 1)
            TSFSROW(i, m, 4) = cp%TBARROW(i, m, 1)
            TACROW(i, m) = cp%TCANROW(i, m)
            QACROW(i, m) = 0.5e-2
            if (bi%IGND > 3) then ! should stay this way to work with class

                !todo - if we have time, change this so that soil.ini can take more than 3 layers.
                if (NRSOILAYEREADFLAG == 0) then
                    do j = 4, bi%IGND
                        cp%THLQROW(i, m, j) = cp%THLQROW(i, m, 3)
                        cp%THICROW(i, m, j) = cp%THICROW(i, m, 3)
                        cp%TBARROW(i, m, j) = cp%TBARROW(i, m, 3)
                        if (cp%SDEPROW(i, m) < (sl%ZBOT(j - 1) + 0.001) .and. cp%SANDROW(i, m, 3) > -2.5) then
                            cp%SANDROW(i, m, j) = -3.0
                            cp%CLAYROW(i, m, j) = -3.0
                            cp%ORGMROW(i, m, j) = -3.0
                        else
                            cp%SANDROW(i, m, j) = cp%SANDROW(i, m, 3)
                            cp%CLAYROW(i, m, j) = cp%CLAYROW(i, m, 3)
                            cp%ORGMROW(i, m, j) = cp%ORGMROW(i, m, 3)
                        end if
                    end do
                else
                    do j = 4, bi%IGND
                        if (cp%SDEPROW(i, m) < (sl%ZBOT(j - 1) + 0.001) .and. cp%SANDROW(i, m, 3) > -2.5) then
                            cp%SANDROW(i, m, j) = -3.0
                            cp%CLAYROW(i, m, j) = -3.0
                            cp%ORGMROW(i, m, j) = -3.0
                        end if
                    end do
                end if !if (NRSOILAYEREADFLAG == 0) then
            end if !(bi%IGND > 3) then
            do k = 1, 6
                do l = 1, 50
                    ITCTROW(i, m, k, l) = 0
                end do
            end do
        end do !m = 1, NMTEST
    end do !i = 1, bi%NA

!> clear accumulating variables
    TOTAL_ROF = 0.0
    TOTAL_ROFO = 0.0
    TOTAL_ROFS = 0.0
    TOTAL_ROFB = 0.0
    TOTAL_EVAP = 0.0
    TOTAL_PRE = 0.0
    TOTAL_ROFACC = 0.0
    TOTAL_ROFOACC = 0.0
    TOTAL_ROFSACC = 0.0
    TOTAL_ROFBACC = 0.0
    TOTAL_EVAPACC = 0.0
    TOTAL_PREACC = 0.0
    TOTAL_HFSACC = 0.0
    TOTAL_QEVPACC = 0.0

    ! For monthly totals.
    TOTAL_ROF_M = 0.0
    TOTAL_ROFO_M = 0.0
    TOTAL_ROFS_M = 0.0
    TOTAL_ROFB_M = 0.0
    TOTAL_EVAP_M = 0.0
    TOTAL_PRE_M = 0.0
    TOTAL_ROF_ACC_M = 0.0
    TOTAL_ROFO_ACC_M = 0.0
    TOTAL_ROFS_ACC_M = 0.0
    TOTAL_ROFB_ACC_M = 0.0
    TOTAL_EVAP_ACC_M = 0.0
    TOTAL_PRE_ACC_M = 0.0

!> *********************************************************************
!> Set accumulation variables to zero.
!> *********************************************************************

  !> Grid Variables
    PREACC = 0.0
    GTACC = 0.0
    QEVPACC = 0.0
    EVAPACC = 0.0
    HFSACC = 0.0
    HMFNACC = 0.0
    ROFACC = 0.0
    ROFOACC = 0.0
    ROFSACC = 0.0
    ROFBACC = 0.0
    WTBLACC = 0.0
    ALVSACC = 0.0
    ALIRACC = 0.0
    RHOSACC = 0.0
    SNOACC = 0.0
    WSNOACC = 0.0
    CANARE = 0.0
    SNOARE = 0.0
    TSNOACC = 0.0
    TCANACC = 0.0
    RCANACC = 0.0
    SCANACC = 0.0
    GROACC = 0.0
    FSINACC = 0.0
    FLINACC = 0.0
    FLUTACC = 0.0
    TAACC = 0.0
    UVACC = 0.0
    PRESACC = 0.0
    QAACC = 0.0

    !> Soil variables
    TBARACC = 0.0
    THLQACC = 0.0
    THICACC = 0.0
    THALACC = 0.0
    GFLXACC = 0.0

    STG_I = 0.0
    DSTG = 0.0
    THLQ_FLD = 0.0
    THIC_FLD = 0.0

    FRAME_NO_NEW = 1

    if (ipid == 0) then

!> SET GRID-FORMAT WATROUTE OUTPUT           !
!-DO I = 1, YCOUNT                            !
!-  DO J = 1, XCOUNT                          !
        rte_runoff = 0.0                    !
        rte_recharge = 0.0                  !
        rte_leakage = 0.0
!-> CDAN            LEAKAGE(I, J) = 0.0       !
!-   END DO                                   !
!-END DO                                      !

!>  SET FRAME COUNT FOR WRITE_R2C
        rte_frames_now = 1
        rte_frames_total = 1

!> ******************************************************
!> echo print information to MESH_output_echo_print.txt
!> ******************************************************

        if (MODELINFOOUTFLAG > 0) then
            open(58, file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/MESH_output_echo_print.txt')
            write(58, "('Number of Soil Layers (IGND) = ', i5)") bi%IGND
            write(58, *)
            write(58, "('MESH_input_run_options.ini')")
            write(58, *)
            write(58, "('Configuration flags - specified by user or default values')")

        !todo: this list should be updated (dgp: 2015-01-09)
            write(58, *) 'BASINSHORTWAVEFLAG   = ', cm%clin(cfk%FB)%filefmt
            write(58, *) 'BASINLONGWAVEFLAG    = ', cm%clin(cfk%FI)%filefmt
            write(58, *) 'BASINRAINFLAG        = ', cm%clin(cfk%PR)%filefmt
            write(58, *) 'BASINTEMPERATUREFLAG = ', cm%clin(cfk%TT)%filefmt
            write(58, *) 'BASINWINDFLAG        = ', cm%clin(cfk%UV)%filefmt
            write(58, *) 'BASINPRESFLAG        = ', cm%clin(cfk%P0)%filefmt
            write(58, *) 'BASINHUMIDITYFLAG    = ', cm%clin(cfk%HU)%filefmt
            write(58, *) 'HOURLYFLAG           = ', HOURLYFLAG
            write(58, *) 'RESUMEFLAG           = ', RESUMEFLAG
            write(58, *) 'SAVERESUMEFLAG       = ', SAVERESUMEFLAG
            write(58, *) 'SHDFILEFLAG          = ', SHDFILEFLAG
            write(58, *) 'SOILINIFLAG          = ', SOILINIFLAG
            write(58, *) 'STREAMFLOWFLAG       = ', STREAMFLOWFLAG
            write(58, *) 'CONFLAGS             = ', CONFLAGS
            write(58, *) 'RELFLG               = ', RELFLG
            write(58, *) 'OPTFLAGS             = ', OPTFLAGS
            write(58, *) 'PREEMPTIONFLAG       = ', mtsflg%PREEMPTIONFLAG
            write(58, *) 'INTERPOLATIONFLAG    = ', INTERPOLATIONFLAG
            write(58, *) 'SUBBASINFLAG         = ', SUBBASINFLAG
            write(58, *) 'TESTCSVFLAG          = ', 'NOTSUPPORTED'
            write(58, *) 'R2COUTPUTFLAG        = ', R2COUTPUTFLAG
            write(58, *) 'OBJFNFLAG            = ', OBJFNFLAG
            write(58, *) 'AUTOCALIBRATIONFLAG  = ', mtsflg%AUTOCALIBRATIONFLAG
            write(58, *) 'WINDOWSIZEFLAG       = ', WINDOWSIZEFLAG
            write(58, *) 'WINDOWSPACINGFLAG    = ', WINDOWSPACINGFLAG
            write(58, *) 'FROZENSOILINFILFLAG  = ', FROZENSOILINFILFLAG
            write(58, *) 'LOCATIONFLAG         = ', LOCATIONFLAG

        !> MAM - ALLOCATE AND INITIALIZE INTERPOLATION VARIABLES:
        !> For 30 minute forcing data there is no need for interpolation and
        !> hence no need to assign PRE and PST variables
            if (INTERPOLATIONFLAG > 1 .or. (INTERPOLATIONFLAG == 1 .and. sum(cm%clin(:)%hf) == 210)) then
                print 9000
                write(58, 9000)
                INTERPOLATIONFLAG = 0
            end if !(INTERPOLATIONFLAG > 1 .or. (INTERPOLATIONFLAG == 1 .and. sum(cm%clin(:)%hf) == 210)) then
            write(58, "('WF_NUM_POINTS: ', i5)") WF_NUM_POINTS
            write(58, "('Out directory:', 5a10)") (op%DIR_OUT(i), i = 1, WF_NUM_POINTS)
            write(58, "('Grid number:  ', 5i10)") (op%N_OUT(i), i = 1, WF_NUM_POINTS)
            write(58, "('Land class:   ', 5i10)") (op%II_OUT(i), i = 1, WF_NUM_POINTS)
            write(58, *)
            write(58, "('MESH_parameters_hydrology.ini')")
            write(58, *)
            write(58, "('Option flags:')")
            if (OPTFLAGS > 0) then
                do i = 1, OPTFLAGS
                    write(58, '(a11, i2, a19)') 'PARAMETER ', i, ' NOT CURRENTLY USED'
                end do
            end if
            write(58, "('River roughnesses:')")
!todo: change this to use NRVR
            write(58, '(5f6.3)') (WF_R2(i), i = 1, 5)
            write(58, "('Land class independent hydrologic parameters:')")
            if (FROZENSOILINFILFLAG == 1) then
                write(58, *) 'SOIL_POR_MAX = ', SOIL_POR_MAX
                write(58, *) 'SOIL_DEPTH   = ', SOIL_DEPTH
                write(58, *) 'S0           = ', S0
                write(58, *) 'T_ICE_LENS   = ', T_ICE_LENS
                do i = 5, INDEPPAR
                    j = i - 4
                    write(58, '(a38, i2, a3, f6.2)') 'OPPORTUNITY TIME FOR SIMULATION YEAR ', j, ' = ', t0_ACC(j)
                end do
            else
                do i = 1, INDEPPAR
                    write(58, '(a36, i2, a19)') 'FROZEN SOIL INFILTRATION PARAMETER ', i, ' READ BUT NOT USED'
                end do
            end if !(FROZENSOILINFILFLAG == 1) then
            write(58, "('Land class dependent hydrologic parameters:')")
            write(NMTESTFORMAT, "(a10, i3, 'f10.2)')") "('ZSNLROW'", NMTEST
            write(58, NMTESTFORMAT) (hp%ZSNLROW(1, m), m = 1, NMTEST)
            write(NMTESTFORMAT, "(a10, i3, 'f10.2)')") "('ZPLSROW'", NMTEST
            write(58, NMTESTFORMAT) (hp%ZPLSROW(1, m), m = 1, NMTEST)
            write(NMTESTFORMAT, "(a10, i3, 'f10.2)')") "('ZPLGROW'", NMTEST
            write(58, NMTESTFORMAT) (hp%ZPLGROW(1, m), m = 1, NMTEST)
            if (DEPPAR >= 4) then
                write(NMTESTFORMAT, "(a10, i3, 'f10.2)')") "('FRZCROW'", NMTEST
                write(58, NMTESTFORMAT) (hp%FRZCROW(1, m), m = 1, NMTEST)
            end if
            write(58, *)
            write(58, "('MESH_parameters_CLASS.ini')")
            write(58, *)
            write(58, '(2x, 6a4)') TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6
            write(58, '(2x, 6a4)') NAME1, NAME2, NAME3, NAME4, NAME5, NAME6
            write(58, '(2x, 6a4)') PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
            i = 1
            write(58, '(5f10.2, f7.1, 3i5)') &
                DEGLAT, DEGLON, cp%ZRFMGRD(i), cp%ZRFHGRD(i), cp%ZBLDGRD(i), cp%GCGRD(i), bi%ILW, bi%NA, NMTEST
            do m = 1, NMTEST
                write(58, '(9f8.3)') (cp%FCANROW(i, m, j), j = 1, ICAN + 1), (cp%PAMXROW(i, m, j), j = 1, ICAN)
                write(58, '(9f8.3)') (cp%LNZ0ROW(i, m, j), j = 1, ICAN + 1), (cp%PAMNROW(i, m, j), j = 1, ICAN)
                write(58, '(9f8.3)') (cp%ALVCROW(i, m, j), j = 1, ICAN + 1), (cp%CMASROW(i, m, j), j = 1, ICAN)
                write(58, '(9f8.3)') (cp%ALICROW(i, m, j), j = 1, ICAN + 1), (cp%ROOTROW(i, m, j), j = 1, ICAN)
                write(58, '(4f8.3, 8x, 4f8.3)') (cp%RSMNROW(i, m, j), j = 1, ICAN), (cp%QA50ROW(i, m, j), j = 1, ICAN)
                write(58, '(4f8.3, 8x, 4f8.3)') (cp%VPDAROW(i, m, j), j = 1, ICAN), (cp%VPDBROW(i, m, j), j = 1, ICAN)
                write(58, '(4f8.3, 8x, 4f8.3)') (cp%PSGAROW(i, m, j), j = 1, ICAN), (cp%PSGBROW(i, m, j), j = 1, ICAN)
                write(58, '(3f8.3, f8.4)') cp%DRNROW(i, m), cp%SDEPROW(i, m), cp%FAREROW(i, m), cp%DDROW(i, m)
                write(58, '(4e8.1, i8)') cp%XSLPROW(i, m), cp%XDROW(i, m), cp%MANNROW(i, m), cp%KSROW(i, m), cp%MIDROW(i, m)
                write(58, '(6f10.1)') (cp%SANDROW(i, m, j), j = 1, bi%IGND)
                write(58, '(6f10.1)') (cp%CLAYROW(i, m, j), j = 1, bi%IGND)
                write(58, '(6f10.1)') (cp%ORGMROW(i, m, j), j = 1, bi%IGND)
                write(58, '(9f10.2)') (cp%TBARROW(i, m, j), j = 1, bi%IGND), cp%TCANROW(i, m), cp%TSNOROW(i, m), cp%TPNDROW(i, m)
                write(58, '(10f10.3)') &
                    (cp%THLQROW(i, m, j), j = 1, bi%IGND), (cp%THICROW(i, m, j), j = 1, bi%IGND), cp%ZPNDROW(i, m)
                write(58, '(2f10.4, f10.2, f10.3, f10.4, f10.3, f10.3)') &
                    cp%RCANROW(i, m), cp%SCANROW(i, m), cp%SNOROW(i, m), cp%ALBSROW(i, m), cp%RHOSROW(i, m), cp%GROROW(i, m)
                write(58, *)
            end do !m = 1, NMTEST
        end if !(MODELINFOOUTFLAG > 0) then
    end if !(ipid == 0) then

    allocate(INFILTYPE(bi%ILG), SI(bi%ILG), TSI(bi%ILG), &
             SNOWMELTD(bi%ILG), SNOWMELTD_LAST(bi%ILG), SNOWINFIL(bi%ILG), &
             CUMSNOWINFILCS(bi%ILG), MELTRUNOFF(bi%ILG), CUMSNOWINFILGS(bi%ILG))
             
    NMELT = 1
    INFILTYPE = 2 !> INITIALIZED WITH UNLIMITED INFILTRATION
    SNOWMELTD = 0.0
    SNOWINFIL = 0.0
    CUMSNOWINFILCS = 0.0
    CUMSNOWINFILGS = 0.0
    MELTRUNOFF = 0.0
    SI = 0.20
    TSI = -0.10

!* PDMROF
    allocate(CMINPDM(bi%ILG), CMAXPDM(bi%ILG), BPDM(bi%ILG), K1PDM(bi%ILG), &
             K2PDM(bi%ILG), ZPNDPRECS(bi%ILG), ZPONDPREC(bi%ILG), ZPONDPREG(bi%ILG), &
             ZPNDPREGS(bi%ILG), &
             UM1CS(bi%ILG), UM1C(bi%ILG), UM1G(bi%ILG), UM1GS(bi%ILG), &
             QM1CS(bi%ILG), QM1C(bi%ILG), QM1G(bi%ILG), QM1GS(bi%ILG), &
             QM2CS(bi%ILG), QM2C(bi%ILG), QM2G(bi%ILG), QM2GS(bi%ILG), &
             UMQ(bi%ILG), &
             FSTRCS(bi%ILG), FSTRC(bi%ILG), FSTRG(bi%ILG), FSTRGS(bi%ILG))

!* PDMROF: INITIALIZE VARIABLES
    ZPNDPRECS = 0.0
    ZPONDPREC = 0.0
    ZPONDPREG = 0.0
    ZPNDPREGS = 0.0
    ZPND = 0.0
    UM1CS = 0.0
    UM1C = 0.0
    UM1G = 0.0
    UM1GS = 0.0
    QM1CS = 0.0
    QM1C = 0.0
    QM1G = 0.0
    QM1GS = 0.0
    QM2CS = 0.0
    QM2C = 0.0
    QM2G = 0.0
    QM2GS = 0.0
    UMQ = 0.0
    FSTRCS = 0.0
    FSTRC = 0.0
    FSTRG = 0.0
    FSTRGS = 0.0
    FSTR = 0.0

!>
!>****************CHECK RESUME FILE***************************************************
!>
    if (RESUMEFLAG == 1) then
!todo: can do this using inquire statement
        open(88, file = 'class_resume.txt', status = 'old', action = 'read', iostat = IOS)
        if (IOS /= 0) then
            if (ipid == 0 .and. MODELINFOOUTFLAG > 0) then
                write(58, *) "WARNING: You've specified a start time", ' without having a resume file. Now ending run.'
            end if
            print *, 'No class_resume.txt found.'
            print *, 'The RESUMEFLAG in MESH_input_run_options.ini is', &
                ' set to 1, which means that class_resume.txt should be here,', &
                ' but it is not here.'
            print *, 'Ending Run'
            stop
        end if
        close(88)
    end if
!>
!> *******************************************************************
!> FOR SPL WATROUTE (MODIFIED RPN CODE)
!> *******************************************************************
!>

    if (ipid == 0) then

!> R2C-FORMAT OUTPUT FILES (RUNOFF, RECHARGE, AND LEAKAGE VALUES)
!> CALL WRITE_R2C TO WRITE R2C-FORMAT FILES
!>
!> IF (MODELFLG .EQ. "i") AUTHOR="MESH_DRIVER (rte -i)"
!>   Requires _RFF (RUNOFF) file
!> IF (MODELFLG .EQ. "r") AUTHOR="MESH_DRIVER (rte -r)"
!>   Requires _RFF (RUNOFF) &
!>            _RCH (RECHARGE) files
!> IF (MODELFLG .EQ. "l") AUTHOR="MESH_DRIVER (rte -l)"
!>   Requires _RFF (RUNOFF) &
!>            _LKG (LEAKAGE) files (not currently supported)
!>
!> HEADER INFORMATION
!>
!-AUTHOR = "MESH_DRIVER"
!-COORDSYS_TEMP = COORDSYS1
!-ZONE_TEMP = ZONE1
!-DATUM_TEMP = DATUM1
!-XORIGIN_TEMP = XORIGIN
!-YORIGIN_TEMP = YORIGIN
!-XCOUNT_TEMP = XCOUNT
!-YCOUNT_TEMP = YCOUNT
!-XDELTA_TEMP = XDELTA
!-YDELTA_TEMP = YDELTA
!-SOURCE_FILE_NAME = "CLASS"
!>
!> OPEN RTE.EXE INPUT FILES (UNIT 261, UNIT 262)
!> (RTE.EXE MIGHT ALSO BE CALLED WATROUTE.EXE)
!> UNIT NUMBERS HAVE BEEN PULLED FROM RTE.EXE SUBROUTINES, THEIR
!> FILE NAMES (FLN(31), FLN(32)) ARE READ FROM THE EVENT FILE
!> FILES ARE OPENED ACCORDING TO MODELFLG IN THE EVENT FILE
!>
!> RUNOFF (MODELFLG .EQ. 'r', 'l', or 'i' (ALL))
!>
        if (PRINTRFFR2CFILEFLAG == 1) then
!-  NAME = "Gridded Channel Inflow"
!-  ATTRIBUTE_NAME = "channel_inflow"
!-  ATTRIBUTE_UNITS = "mm"
!-  ATTRIBUTE_TYPE = "flow"
            call write_r2c(fls, mfk%f31, bi, &
                           1, 0, 1, 1, &
                           rte_year_now, rte_month_now, rte_day_now, rte_hour_now, &
                           rte_runoff, &
!todo: replace source with LSS flag
                           'channel_inflow', 'mm', 'flow', 'CLASS', 'SA_MESH_DRIVER')
        end if
!>
!> RECHARGE (MODELFLG .EQ. 'r')
!>
        if (PRINTRCHR2CFILEFLAG == 1) then
!-  NAME = "Gridded Recharge"
!-  ATTRIBUTE_NAME = "recharge"
!-  ATTRIBUTE_UNITS = "mm"
!-  ATTRIBUTE_TYPE = "flow"
            call write_r2c(fls, mfk%f32, bi, &
                           0, 1, 0, 1, 1, &
                           rte_year_now, rte_month_now, rte_day_now, rte_hour_now, &
                           rte_recharge, &
!todo: replace source with LSS flag
                           'recharge', 'mm', 'flow', 'CLASS', 'SA_MESH_DRIVER')
        end if
!>
!> LEAKAGE (MODELFLG .EQ. 'l' (NOT SUPPORTED))
!>
!!+IF (PRINTLKGR2CFILEFLAG == 1) THEN
!!+  NAME = "Gridded Leakage"
!!+  ATTRIBUTE_NAME = "leakage"
!!+  ATTRIBUTE_UNITS = "cms"
!!+  ATTRIBUTE_TYPE = " "
!!+  CALL WRITE_R2C(263, 33, 0, 1, 0, 1, 1)
!!+END IF

    end if !(ipid == 0) then

!> *********************************************************************
!> Open and print header information to the output files
!> *********************************************************************

    if (ipid == 0) then

    !> Streamflow output files.
        if (STREAMFLOWOUTFLAG > 0) then

        !> Daily streamflow file.
!        if ((VARIABLEFILESFLAG .eq. 1) .and. (fls%fl(6)%isInit)) then
            open(fls%fl(mfk%f70)%iun, &
!todo: This creates a bug if a space doesn't exist in the name of the folder!
                 file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/' // trim(adjustl(fls%fl(mfk%f70)%fn)), &
                 iostat = ios)
!        else
!            open(70, file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/MESH_output_streamflow.csv')
!        end if

        !> Hourly and cumulative daily streamflow files.
            if (STREAMFLOWOUTFLAG >= 2) then
                open(71, file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/MESH_output_streamflow_all.csv')
                open(72, file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/MESH_output_streamflow_cumulative.csv')
            end if

        end if !(STREAMFLOWOUTFLAG > 0) then

!> *********************************************************************
!> Open and read in values from r2c_output.txt file
!> *********************************************************************
        NR2CFILES = 0
        if (R2COUTPUTFLAG >= 1) then
            inquire(file = 'r2c_output.txt', exist = R2COUTPUT)
            if (R2COUTPUT) then
                open(56, file = 'r2c_output.txt', action = 'read')
                read(56, *, iostat = IOS) NR2C, DELTR2C
                if (IOS == 0) then
                    allocate(GRD(NR2C), GAT(NR2C), GRDGAT(NR2C), R2C_ATTRIBUTES(NR2C, 3), stat = PAS)
                    if (PAS /= 0) then
                        print *, 'ALLOCATION ERROR: CHECK THE VALUE OF THE FIRST ', &
                            'RECORD AT THE FIRST LINE IN THE r2c_output.txt FILE. ', &
                            'IT SHOULD BE AN INTEGER VALUE (GREATER THAN 0).'
                        stop
                    end if
                end if
                if (IOS /= 0 .or. mod(DELTR2C, 30) /= 0) then
                    print 9002
                    stop
                end if
                print *
                print *, 'THE FOLLOWING R2C OUTPUT FILES WILL BE WRITTEN:'
                do i = 1, NR2C
                    read(56, *, iostat = IOS) GRD(i), GAT(i), GRDGAT(i), (R2C_ATTRIBUTES(i, j), j = 1, 3)
                    if (IOS /= 0) then
                        print *, 'ERROR READING r2c_output.txt FILE AT LINE ', i + 1
                        stop
                    else
                        if (GRD(i) == 1) then
                            NR2CFILES = NR2CFILES + 1
                            print *, NR2CFILES, ' (GRD)    : ', R2C_ATTRIBUTES(i, 3)
                        end if
                        if (GAT(i) == 1) then
                            NR2CFILES = NR2CFILES + 1
                            print *, NR2CFILES, ' (GAT)    : ', R2C_ATTRIBUTES(i, 3)
                        end if
                        if (GRDGAT(i) == 1) then
                            NR2CFILES = NR2CFILES + 1
                            print *, NR2CFILES, ' (GRDGAT) : ', R2C_ATTRIBUTES(i, 3)
                        end if
                    end if
                end do
                close(56)
            else
                print *
                print *, "r2c_output.txt FILE DOESN'T EXIST. ", &
                    'R2COUTPUTFLAG SHOULD BE SET TO ZERO IF R2C OUTPUTS ARE NOT NEEDED.'
                print *
                stop
            end if
        end if

!> WRITE THE HEADER FOR R2C FILES:
        if (NR2CFILES > 0) then
            call WRITE_R2C_HEADER(NMTEST, NR2C, NR2CFILES, GRD, GAT, GRDGAT, R2C_ATTRIBUTES, &
                                  R2CFILEUNITSTART, NR2CSTATES, bi%CoordSys, bi%Datum, bi%Zone, &
                                  bi%xOrigin, bi%yOrigin, bi%xDelta, bi%yDelta, bi%xCount, bi%yCount)
        end if

!> For the ENSIM timestamp
        wfo_seq = 0

    end if !(ipid == 0) then

!> End of ENSIM Changes

!> *********************************************************************
!> Output information to screen
!> *********************************************************************

    if (ro%VERBOSEMODE > 0) then
        print *, 'NUMBER OF GRID SQUARES: ', bi%NA
        print *, 'NUMBER OF LAND CLASSES (WITH IMPERVIOUS): ', NMTEST
        print *, 'NUMBER OF RIVER CLASSES: ', bi%NRVR
        print *, 'MINIMUM NUMBER FOR ILG: ', bi%NA*NMTEST
        print *, 'NUMBER OF GRID SQUARES IN West-East DIRECTION: ', bi%xCount
        print *, 'NUMBER OF GRID SQUARES IN South-North DIRECTION: ', bi%yCount
        print *, 'LENGTH OF SIDE OF GRID SQUARE IN M: ', bi%AL
        print *, 'NUMBER OF DRAINAGE OUTLETS: ', bi%NAA
        print *, 'NUMBER OF STREAMFLOW GUAGES: ', WF_NO
        do i = 1, WF_NO
            print *, 'STREAMFLOW STATION: ', i, 'I: ', WF_IY(i), 'J: ', WF_JX(i)
        end do
        print *, 'NUMBER OF RESERVOIR STATIONS: ', WF_NORESV
        if (WF_NORESV > 0) then
            do i = 1, WF_NORESV
                print *, 'RESERVOIR STATION: ', i, 'I: ', WF_IRES(i), 'J: ', WF_JRES(i)
            end do
        end if
        print *
        print *, 'Found these output locations:'
        print *, 'Output Directory, grid number, land class number'
        do i = 1, WF_NUM_POINTS
            print *, op%DIR_OUT(i), op%N_OUT(i), op%II_OUT(i)
        end do
        print *
        print *
        print *
    end if !(ro%VERBOSEMODE > 0) then

    if (ipid == 0 .and. mtsflg%AUTOCALIBRATIONFLAG > 0) call stats_init(ts, wf_no)

!>
!>*******************************************************************
!>
!> Check if we are reading in a resume file
    if (RESUMEFLAG == 1) then
        print *, 'Reading saved state variables'
        call resume_state(HOURLYFLAG, MINS_NOW, TIME_STEP_NOW, &
                          cm%clin(cfk%FB)%filefmt, cm%clin(cfk%FI)%filefmt, &
                          cm%clin(cfk%PR)%filefmt, cm%clin(cfk%TT)%filefmt, &
                          cm%clin(cfk%UV)%filefmt, cm%clin(cfk%P0)%filefmt, cm%clin(cfk%HU)%filefmt, &
                          cm%clin(cfk%FB)%climvGrd, FSVHGRD, FSIHGRD, cm%clin(cfk%FI)%climvGrd, &
                          i, j, bi%xCount, bi%yCount, jan, &
                          VPDGRD, TADPGRD, PADRGRD, RHOAGRD, RHSIGRD, &
                          RPCPGRD, TRPCGRD, SPCPGRD, TSPCGRD, cm%clin(cfk%TT)%climvGrd, &
                          cm%clin(cfk%HU)%climvGrd, cm%clin(cfk%PR)%climvGrd, RPREGRD, SPREGRD, cm%clin(cfk%P0)%climvGrd, &

!> MAM - FOR FORCING DATA INTERPOLATION
                          FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE, &
                          TAGATPRE, ULGATPRE, PRESGATPRE, QAGATPRE, &
                          IPCP, bi%NA, bi%NA, bi%ILMOS, bi%JLMOS, bi%IWMOS, bi%JWMOS, &
                          bi%NML, bi%NMW, &
                          cp%GCGRD, cp%FAREROW, cp%MIDROW, bi%NTYPE, bi%ILG, NMTEST, &
                          TBARGAT, THLQGAT, THICGAT, TPNDGAT, ZPNDGAT, &
                          TBASGAT, ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, &
                          TCANGAT, RCANGAT, SCANGAT, GROGAT, FRZCGAT, CMAIGAT, &
                          FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT, PAMXGAT, &
                          PAMNGAT, CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, &
                          VPDAGAT, VPDBGAT, PSGAGAT, PSGBGAT, PAIDGAT, &
                          HGTDGAT, ACVDGAT, ACIDGAT, TSFSGAT, WSNOGAT, &
                          THPGAT, THRGAT, THMGAT, BIGAT, PSISGAT, &
                          GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, THFCGAT, &
                          PSIWGAT, DLZWGAT, ZBTWGAT, ZSNLGAT, ZPLGGAT, &
                          ZPLSGAT, TACGAT, QACGAT, DRNGAT, XSLPGAT, &
                          XDGAT, WFSFGAT, KSGAT, ALGWGAT, ALGDGAT, &
                          ASVDGAT, ASIDGAT, AGVDGAT, AGIDGAT, ISNDGAT, &
                          RADJGAT, ZBLDGAT, Z0ORGAT, ZRFMGAT, ZRFHGAT, &
                          ZDMGAT, ZDHGAT, FSVHGAT, FSIHGAT, CSZGAT, &
                          FDLGAT, ULGAT, VLGAT, TAGAT, QAGAT, PRESGAT, &
                          PREGAT, PADRGAT, VPDGAT, TADPGAT, RHOAGAT, &
                          RPCPGAT, TRPCGAT, SPCPGAT, TSPCGAT, RHSIGAT, &
                          FCLOGAT, DLONGAT, GGEOGAT, CDHGAT, CDMGAT, &
                          HFSGAT, TFXGAT, QEVPGAT, QFSGAT, QFXGAT, &
                          PETGAT, GAGAT, EFGAT, GTGAT, QGGAT, &
                          ALVSGAT, ALIRGAT, SFCTGAT, SFCUGAT, SFCVGAT, &
                          SFCQGAT, FSNOGAT, FSGVGAT, FSGSGAT, FSGGGAT, &
                          FLGVGAT, FLGSGAT, FLGGGAT, HFSCGAT, HFSSGAT, &
                          HFSGGAT, HEVCGAT, HEVSGAT, HEVGGAT, HMFCGAT, &
                          HMFNGAT, HTCCGAT, HTCSGAT, PCFCGAT, PCLCGAT, &
                          PCPNGAT, PCPGGAT, QFGGAT, QFNGAT, QFCLGAT, &
                          QFCFGAT, ROFGAT, ROFOGAT, ROFSGAT, ROFBGAT, &
                          TROFGAT, TROOGAT, TROSGAT, TROBGAT, ROFCGAT, &
                          ROFNGAT, ROVGGAT, WTRCGAT, WTRSGAT, WTRGGAT, &
                          DRGAT, HMFGGAT, HTCGAT, QFCGAT, ITCTGAT, &
                          bi%IGND, ICAN, ICP1, &
                          cp%TBARROW, cp%THLQROW, cp%THICROW, cp%TPNDROW, cp%ZPNDROW, &
                          TBASROW, cp%ALBSROW, cp%TSNOROW, cp%RHOSROW, cp%SNOROW, &
                          cp%TCANROW, cp%RCANROW, cp%SCANROW, cp%GROROW, CMAIROW, &
                          cp%FCANROW, cp%LNZ0ROW, cp%ALVCROW, cp%ALICROW, cp%PAMXROW, &
                          cp%PAMNROW, cp%CMASROW, cp%ROOTROW, cp%RSMNROW, cp%QA50ROW, &
                          cp%VPDAROW, cp%VPDBROW, cp%PSGAROW, cp%PSGBROW, PAIDROW, &
                          HGTDROW, ACVDROW, ACIDROW, TSFSROW, WSNOROW, &
                          THPROW, THRROW, THMROW, BIROW, PSISROW, &
                          GRKSROW, THRAROW, HCPSROW, TCSROW, THFCROW, &
                          PSIWROW, DLZWROW, ZBTWROW, hp%ZSNLROW, hp%ZPLGROW, &
                          hp%ZPLSROW, hp%FRZCROW, TACROW, QACROW, cp%DRNROW, cp%XSLPROW, &
                          cp%XDROW, WFSFROW, cp%KSROW, ALGWROW, ALGDROW, &
                          ASVDROW, ASIDROW, AGVDROW, AGIDROW, &
                          ISNDROW, RADJGRD, cp%ZBLDGRD, Z0ORGRD, &
                          cp%ZRFMGRD, cp%ZRFHGRD, ZDMGRD, ZDHGRD, CSZGRD, &
                          cm%clin(cfk%UV)%climvGrd, VLGRD, FCLOGRD, DLONGRD, GGEOGRD, &
                          cp%MANNROW, MANNGAT, cp%DDROW, DDGAT, &
                          IGDRROW, IGDRGAT, VMODGRD, VMODGAT, QLWOGAT, &
                          CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, &
                          WTVSTP, WTSSTP, WTGSTP, &
                          sl%DELZ, FCS, FGS, FC, FG, N, &
                          ALVSCN, ALIRCN, ALVSG, ALIRG, ALVSCS, &
                          ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, &
                          ALVSSC, ALIRSC, TRVSCN, TRIRCN, TRVSCS, &
                          TRIRCS, FSVF, FSVFS, &
                          RAICAN, RAICNS, SNOCAN, SNOCNS, &
                          FRAINC, FSNOWC, FRAICS, FSNOCS, &
                          DISP, DISPS, ZOMLNC, ZOMLCS, ZOELNC, ZOELCS, &
                          ZOMLNG, ZOMLNS, ZOELNG, ZOELNS, &
                          CHCAP, CHCAPS, CMASSC, CMASCS, CWLCAP, &
                          CWFCAP, CWLCPS, CWFCPS, RC, RCS, RBCOEF, &
                          FROOT, ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, &
                          TRSNOW, ZSNOW, JDAY_NOW, JLAT, IDISP, &
                          IZREF, IWF, IPAI, IHGT, IALC, IALS, IALG, &
                          TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG, &
                          THICEC, THICEG, HCPC, HCPG, TCTOPC, TCBOTC, &
                          TCTOPG, TCBOTG, &
                          GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, &
                          G12CS, G12GS, G23C, G23G, G23CS, G23GS, &
                          QFREZC, QFREZG, QMELTC, QMELTG, &
                          EVAPC, EVAPCG, EVAPG, EVAPCS, EVPCSG, EVAPGS, &
                          TCANO, TCANS, TPONDC, TPONDG, TPNDCS, TPNDGS, &
                          TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, &
                          WTABGAT, &
                          ILMOGAT, UEGAT, HBLGAT, &
                          bi%ILW, ITC, ITCG, ITG, ISLFD, &
                          NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI, &
                          GFLXGAT, CDHROW, CDMROW, HFSROW, TFXROW, &
                          QEVPROW, QFSROW, QFXROW, PETROW, GAROW, &
                          EFROW, GTROW, QGROW, TSFROW, ALVSROW, &
                          ALIRROW, SFCTROW, SFCUROW, SFCVROW, SFCQROW, &
                          FSGVROW, FSGSROW, FSGGROW, FLGVROW, FLGSROW, &
                          FLGGROW, HFSCROW, HFSSROW, HFSGROW, HEVCROW, &
                          HEVSROW, HEVGROW, HMFCROW, HMFNROW, HTCCROW, &
                          HTCSROW, PCFCROW, PCLCROW, PCPNROW, PCPGROW, &
                          QFGROW, QFNROW, QFCLROW, QFCFROW, ROFROW, &
                          ROFOROW, ROFSROW, ROFBROW, TROFROW, TROOROW, &
                          TROSROW, TROBROW, ROFCROW, ROFNROW, ROVGROW, &
                          WTRCROW, WTRSROW, WTRGROW, DRROW, WTABROW, &
                          ILMOROW, UEROW, HBLROW, HMFGROW, HTCROW, &
                          QFCROW, FSNOROW, ITCTROW, NCOUNT, ireport, &
                          wfo_seq, YEAR_NOW, ensim_MONTH, ensim_DAY, &
                          HOUR_NOW, bi%xxx, bi%yyy, bi%NA, &
                          bi%NTYPE, DELT, TFREZ, UVGRD, SBC, RHOW, CURREC, &
                          M_C, M_S, M_R, &
                          WF_ROUTETIMESTEP, WF_R1, WF_R2, bi%NAA, bi%iyMin, &
                          bi%iyMax, bi%jxMin, bi%jxMax, bi%IAK, bi%IROUGH, &
                          bi%ICHNL, bi%NEXT, bi%IREACH, bi%AL, bi%GRDN, bi%GRDE, &
                          bi%DA, bi%BNKFLL, bi%SLOPE_CHNL, bi%ELEV, bi%FRAC, &
                          WF_NO, WF_NL, WF_MHRD, WF_KT, WF_IY, WF_JX, &
                          WF_QHYD, WF_RES, WF_RESSTORE, WF_NORESV_CTRL, WF_R, &
                          WF_NORESV, WF_NREL, WF_KTR, WF_IRES, WF_JRES, WF_RESNAME, &
                          WF_B1, WF_B2, WF_QREL, WF_QR, &
                          WF_TIMECOUNT, WF_NHYD, WF_QBASE, WF_QI1, WF_QI2, WF_QO1, WF_QO2, &
                          WF_STORE1, WF_STORE2, &
                          DRIVERTIMESTEP, ROFGRD, &
                          WF_S, &
                          TOTAL_ROFACC, TOTAL_ROFOACC, TOTAL_ROFSACC, &
                          TOTAL_ROFBACC, TOTAL_EVAPACC, TOTAL_PREACC, INIT_STORE, &
                          FINAL_STORE, TOTAL_AREA, TOTAL_HFSACC, TOTAL_QEVPACC, &
                          SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, NMELT, t0_ACC, &
                          CO2CONC, COSZS, XDIFFUSC, CFLUXCG, CFLUXCS, &
                          AILCG, AILCGS, FCANC, FCANCS, CO2I1CG, CO2I1CS, CO2I2CG, CO2I2CS, &
                          SLAI, FCANCMX, ANCSVEG, ANCGVEG, RMLCSVEG, RMLCGVEG, &
                          AILC, PAIC, FIELDSM, WILTSM, &
                          RMATCTEM, RMATC, NOL2PFTS, ICTEMMOD, L2MAX, ICTEM, &
                          hp%fetchROW, hp%HtROW, hp%N_SROW, hp%A_SROW, hp%DistribROW, &
                          fetchGAT, HtGAT, N_SGAT, A_SGAT, DistribGAT)
    end if !(RESUMEFLAG == 1) then

!>
!>*******************************************************************
!>
!> Check if we are reading in a resume_state.r2c file
    if (RESUMEFLAG == 2) then
        print *, 'Reading saved state variables'

! Allocate arrays for resume_state_r2c
        open(54, file = 'resume_state_r2c.txt', action = 'read')
        read(54, *, iostat = IOS) NR2C_R, DELTR2C_R
        if (IOS == 0) then
            allocate(GRD_R(NR2C_R), GAT_R(NR2C_R), GRDGAT_R(NR2C_R), R2C_ATTRIBUTES_R(NR2C_R, 3), stat = PAS)
            if (PAS /= 0) then
                print *, 'ALLOCATION ERROR: CHECK THE VALUE OF THE FIRST ', &
                    'RECORD AT THE FIRST LINE IN THE resume_state_r2c.txt FILE. ', &
                    'IT SHOULD BE AN INTEGER VALUE (GREATER THAN 0).'
                stop
            end if
        end if
        close(54)

! start by gathering from ROW to GAT so as not to mess-up with CLASSS after call to save_state_r2c
        call CLASSG (TBARGAT, THLQGAT, THICGAT, TPNDGAT, ZPNDGAT, &
                     TBASGAT, ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, &
                     TCANGAT, RCANGAT, SCANGAT, GROGAT, FRZCGAT, CMAIGAT, &
                     FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT, PAMXGAT, &
                     PAMNGAT, CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, &
                     VPDAGAT, VPDBGAT, PSGAGAT, PSGBGAT, PAIDGAT, &
                     HGTDGAT, ACVDGAT, ACIDGAT, TSFSGAT, WSNOGAT, &
                     THPGAT, THRGAT, THMGAT, BIGAT, PSISGAT, &
                     GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, IGDRGAT, &
                     THFCGAT, PSIWGAT, DLZWGAT, ZBTWGAT, VMODGAT, &
                     ZSNLGAT, ZPLGGAT, ZPLSGAT, TACGAT, QACGAT, &
                     DRNGAT, XSLPGAT, XDGAT, WFSFGAT, KSGAT, &
                     ALGWGAT, ALGDGAT, ASVDGAT, ASIDGAT, AGVDGAT, &
                     AGIDGAT, ISNDGAT, RADJGAT, ZBLDGAT, Z0ORGAT, &
                     ZRFMGAT, ZRFHGAT, ZDMGAT, ZDHGAT, FSVHGAT, &
                     FSIHGAT, CSZGAT, FDLGAT, ULGAT, VLGAT, &
                     TAGAT, QAGAT, PRESGAT, PREGAT, PADRGAT, &
                     VPDGAT, TADPGAT, RHOAGAT, RPCPGAT, TRPCGAT, &
                     SPCPGAT, TSPCGAT, RHSIGAT, FCLOGAT, DLONGAT, &
                     GGEOGAT, &
                     CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT, &
                     QFSGAT, QFXGAT, PETGAT, GAGAT, EFGAT, &
                     GTGAT, QGGAT, ALVSGAT, ALIRGAT, &
                     SFCTGAT, SFCUGAT, SFCVGAT, SFCQGAT, FSNOGAT, &
                     FSGVGAT, FSGSGAT, FSGGGAT, FLGVGAT, FLGSGAT, &
                     FLGGGAT, HFSCGAT, HFSSGAT, HFSGGAT, HEVCGAT, &
                     HEVSGAT, HEVGGAT, HMFCGAT, HMFNGAT, HTCCGAT, &
                     HTCSGAT, PCFCGAT, PCLCGAT, PCPNGAT, PCPGGAT, &
                     QFGGAT, QFNGAT, QFCLGAT, QFCFGAT, ROFGAT, &
                     ROFOGAT, ROFSGAT, ROFBGAT, TROFGAT, TROOGAT, &
                     TROSGAT, TROBGAT, ROFCGAT, ROFNGAT, ROVGGAT, &
                     WTRCGAT, WTRSGAT, WTRGGAT, DRGAT, GFLXGAT, &
                     HMFGGAT, HTCGAT, QFCGAT, ITCTGAT, &
!BEGIN: PDMROF
                     CMINPDM, CMAXPDM, BPDM, K1PDM, K2PDM, &
!END: PDMROF
                     bi%ILMOS, bi%JLMOS, bi%IWMOS, bi%JWMOS, bi%NML, bi%NA, bi%NTYPE, &
                     bi%NA*bi%NTYPE, bi%IGND, ICAN, ICP1, cp%TBARROW, cp%THLQROW, &
                     cp%THICROW, cp%TPNDROW, cp%ZPNDROW, TBASROW, cp%ALBSROW, &
                     cp%TSNOROW, cp%RHOSROW, cp%SNOROW, cp%TCANROW, &
                     cp%RCANROW, cp%SCANROW, cp%GROROW, CMAIROW, cp%FCANROW, &
                     cp%LNZ0ROW, cp%ALVCROW, cp%ALICROW, cp%PAMXROW, &
                     cp%PAMNROW, cp%CMASROW, cp%ROOTROW, cp%RSMNROW, &
                     cp%QA50ROW, cp%VPDAROW, cp%VPDBROW, cp%PSGAROW, &
                     cp%PSGBROW, PAIDROW, HGTDROW, ACVDROW, ACIDROW, TSFSROW, &
                     WSNOROW, THPROW, THRROW, THMROW, BIROW, PSISROW, &
                     GRKSROW, THRAROW, HCPSROW, TCSROW, IGDRROW, &
                     THFCROW, PSIWROW, DLZWROW, ZBTWROW, VMODGRD, &
                     hp%ZSNLROW, hp%ZPLGROW, hp%ZPLSROW, hp%FRZCROW, TACROW, QACROW, &
                     cp%DRNROW, cp%XSLPROW, cp%XDROW, WFSFROW, cp%KSROW, &
                     ALGWROW, ALGDROW, ASVDROW, ASIDROW, AGVDROW, &
                     AGIDROW, ISNDROW, RADJGRD, cp%ZBLDGRD, Z0ORGRD, &
                     cp%ZRFMGRD, cp%ZRFHGRD, ZDMGRD, ZDHGRD, FSVHGRD, &
                     FSIHGRD, CSZGRD, cm%clin(cfk%FI)%climvGrd, cm%clin(cfk%UV)%climvGrd, VLGRD, &
                     cm%clin(cfk%TT)%climvGrd, cm%clin(cfk%HU)%climvGrd, cm%clin(cfk%P0)%climvGrd, &
                     cm%clin(cfk%PR)%climvGrd, PADRGRD, &
                     VPDGRD, TADPGRD, RHOAGRD, RPCPGRD, TRPCGRD, &
                     SPCPGRD, TSPCGRD, RHSIGRD, FCLOGRD, DLONGRD, &
                     GGEOGRD, cp%MANNROW, MANNGAT, cp%DDROW, DDGAT, &
                     cp%SANDROW, SANDGAT, cp%CLAYROW, CLAYGAT, &
!BEGIN: PDMROF
                     hp%CMINROW, hp%CMAXROW, hp%BROW, hp%K1ROW, hp%K2ROW, &
!END: PDMROF
                     cp%FAREROW, FAREGAT, &
                     hp%fetchROW, hp%HtROW, hp%N_SROW, hp%A_SROW, hp%DistribROW, &
                     fetchGAT, HtGAT, N_SGAT, A_SGAT, DistribGAT, &
                     DrySnowRow, SnowAgeROW, DrySnowGAT, SnowAgeGAT, &
                     TSNOdsROW, RHOSdsROW, TSNOdsGAT, RHOSdsGAT, &
                     DriftROW, SublROW, DepositionROW, &
                     DriftGAT, SublGAT, DepositionGAT)
!>
!>   * INITIALIZATION OF DIAGNOSTIC VARIABLES SPLIT OUT OF CLASSG
!>   * FOR CONSISTENCY WITH GCM APPLICATIONS.
!>

!> *********************************************************************
!> Set variables arrays to zero.
!> *********************************************************************

        CDHGAT = 0.0
        CDMGAT = 0.0
        HFSGAT = 0.0
        TFXGAT = 0.0
        QEVPGAT = 0.0
        QFSGAT = 0.0
        QFXGAT = 0.0
        PETGAT = 0.0
        GAGAT = 0.0
        EFGAT = 0.0
        GTGAT = 0.0
        QGGAT = 0.0
        ALVSGAT = 0.0
        ALIRGAT = 0.0
        SFCTGAT = 0.0
        SFCUGAT = 0.0
        SFCVGAT = 0.0
        SFCQGAT = 0.0
        FSNOGAT = 0.0
        FSGVGAT = 0.0
        FSGSGAT = 0.0
        FSGGGAT = 0.0
        FLGVGAT = 0.0
        FLGSGAT = 0.0
        FLGGGAT = 0.0
        HFSCGAT = 0.0
        HFSSGAT = 0.0
        HFSGGAT = 0.0
        HEVCGAT = 0.0
        HEVSGAT = 0.0
        HEVGGAT = 0.0
        HMFCGAT = 0.0
        HMFNGAT = 0.0
        HTCCGAT = 0.0
        HTCSGAT = 0.0
        PCFCGAT = 0.0
        PCLCGAT = 0.0
        PCPNGAT = 0.0
        PCPGGAT = 0.0
        QFGGAT = 0.0
        QFNGAT = 0.0
        QFCFGAT = 0.0
        QFCLGAT = 0.0
        ROFGAT = 0.0
        ROFOGAT = 0.0
        ROFSGAT = 0.0
        ROFBGAT = 0.0
        TROFGAT = 0.0
        TROOGAT = 0.0
        TROSGAT = 0.0
        TROBGAT = 0.0
        ROFCGAT = 0.0
        ROFNGAT = 0.0
        ROVGGAT = 0.0
        WTRCGAT = 0.0
        WTRSGAT = 0.0
        WTRGGAT = 0.0
        DRGAT = 0.0
120 continue
!>
        HMFGGAT = 0.0
        HTCGAT = 0.0
        QFCGAT = 0.0
        GFLXGAT = 0.0
130 continue
140 continue
!>
        ITCTGAT = 0
150 continue
160 continue
170 continue
!>
        call resume_state_r2c(bi%NML, NLTEST, NMTEST, NCOUNT, &
                              MINS_NOW, bi%ACLASS, NR2C_R, GRD_R, GAT_R, GRDGAT_R, R2C_ATTRIBUTES_R, &
                              bi%NA, bi%xxx, bi%yyy, bi%xCount, bi%yCount, bi%ILMOS, bi%JLMOS, bi%ILG, ICAN, ICP1, bi%IGND, &
                              TBARGAT, THLQGAT, THICGAT, TPNDGAT, ZPNDGAT, &
                              TBASGAT, ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, &
                              TCANGAT, RCANGAT, SCANGAT, GROGAT, CMAIGAT, &
                              FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT, PAMXGAT, &
                              PAMNGAT, CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, &
                              VPDAGAT, VPDBGAT, PSGAGAT, PSGBGAT, PAIDGAT, &
                              HGTDGAT, ACVDGAT, ACIDGAT, TSFSGAT, WSNOGAT, &
                              THPGAT, THRGAT, THMGAT, BIGAT, PSISGAT, &
                              GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, &
                              THFCGAT, PSIWGAT, DLZWGAT, ZBTWGAT, &
                              ZSNLGAT, ZPLGGAT, ZPLSGAT, TACGAT, QACGAT, &
                              DRNGAT, XSLPGAT, XDGAT, WFSFGAT, KSGAT, &
                              ALGWGAT, ALGDGAT, ASVDGAT, ASIDGAT, AGVDGAT, &
                              AGIDGAT, ISNDGAT, RADJGAT, ZBLDGAT, Z0ORGAT, &
                              ZRFMGAT, ZRFHGAT, ZDMGAT, ZDHGAT, FSVHGAT, &
                              FSIHGAT, CSZGAT, FDLGAT, ULGAT, VLGAT, &
                              TAGAT, QAGAT, PRESGAT, PREGAT, PADRGAT, &
                              VPDGAT, TADPGAT, RHOAGAT, RPCPGAT, TRPCGAT, &
                              SPCPGAT, TSPCGAT, RHSIGAT, FCLOGAT, DLONGAT, &
                              GGEOGAT, &
                              CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT, &
                              QFSGAT, QFXGAT, PETGAT, GAGAT, EFGAT, &
                              GTGAT, QGGAT, ALVSGAT, ALIRGAT, &
                              SFCTGAT, SFCUGAT, SFCVGAT, SFCQGAT, FSNOGAT, &
                              FSGVGAT, FSGSGAT, FSGGGAT, FLGVGAT, FLGSGAT, &
                              FLGGGAT, HFSCGAT, HFSSGAT, HFSGGAT, HEVCGAT, &
                              HEVSGAT, HEVGGAT, HMFCGAT, HMFNGAT, HTCCGAT, &
                              HTCSGAT, PCFCGAT, PCLCGAT, PCPNGAT, PCPGGAT, &
                              QFGGAT, QFNGAT, QFCLGAT, QFCFGAT, ROFGAT, &
                              ROFOGAT, ROFSGAT, ROFBGAT, TROFGAT, TROOGAT, &
                              TROSGAT, TROBGAT, ROFCGAT, ROFNGAT, ROVGGAT, &
                              WTRCGAT, WTRSGAT, WTRGGAT, DRGAT, GFLXGAT, &
                              HMFGGAT, HTCGAT, QFCGAT, MANNGAT, DDGAT, &
                              SANDGAT, CLAYGAT, IGDRGAT, VMODGAT, QLWOGAT, &
                              bi%CoordSys, bi%Datum, bi%Zone, bi%xOrigin, bi%yOrigin, bi%xDelta, bi%yDelta)
!>
! now scatter the variables so that the GATs don't get overwritten incorrectly
        call CLASSS(cp%TBARROW, cp%THLQROW, cp%THICROW, GFLXROW, TSFSROW, &
                    cp%TPNDROW, cp%ZPNDROW, TBASROW, cp%ALBSROW, cp%TSNOROW, &
                    cp%RHOSROW, cp%SNOROW, cp%TCANROW, cp%RCANROW, cp%SCANROW, &
                    cp%GROROW, CMAIROW, TACROW, QACROW, WSNOROW, &
                    bi%ILMOS, bi%JLMOS, bi%IWMOS, bi%JWMOS, &
                    bi%NML, bi%NA, bi%NTYPE, bi%ILG, bi%IGND, ICAN, ICAN + 1, &
                    TBARGAT, THLQGAT, THICGAT, GFLXGAT, TSFSGAT, &
                    TPNDGAT, ZPNDGAT, TBASGAT, ALBSGAT, TSNOGAT, &
                    RHOSGAT, SNOGAT, TCANGAT, RCANGAT, SCANGAT, &
                    GROGAT, CMAIGAT, TACGAT, QACGAT, WSNOGAT, &
                    cp%MANNROW, MANNGAT, cp%DDROW, DDGAT, &
                    cp%SANDROW, SANDGAT, cp%CLAYROW, CLAYGAT, cp%XSLPROW, XSLPGAT, &
                    DrySnowRow, SnowAgeROW, DrySnowGAT, SnowAgeGAT, &
                    TSNOdsROW, RHOSdsROW, TSNOdsGAT, RHOSdsGAT, &
                    DriftROW, SublROW, DepositionROW, &
                    DriftGAT, SublGAT, DepositionGAT)
!>
!>   * SCATTER OPERATION ON DIAGNOSTIC VARIABLES SPLIT OUT OF
!>   * CLASSS FOR CONSISTENCY WITH GCM APPLICATIONS.
!>
        do 180 k = 1, bi%NML
            CDHROW(bi%ILMOS(k), bi%JLMOS(k)) = CDHGAT(k)
            CDMROW(bi%ILMOS(k), bi%JLMOS(k)) = CDMGAT(k)
            HFSROW(bi%ILMOS(k), bi%JLMOS(k)) = HFSGAT(k)
            TFXROW(bi%ILMOS(k), bi%JLMOS(k)) = TFXGAT(k)
            QEVPROW(bi%ILMOS(k), bi%JLMOS(k)) = QEVPGAT(k)
            QFSROW(bi%ILMOS(k), bi%JLMOS(k)) = QFSGAT(k)
            QFXROW(bi%ILMOS(k), bi%JLMOS(k)) = QFXGAT(k)
            PETROW(bi%ILMOS(k), bi%JLMOS(k)) = PETGAT(k)
            GAROW(bi%ILMOS(k), bi%JLMOS(k)) = GAGAT(k)
            EFROW(bi%ILMOS(k), bi%JLMOS(k)) = EFGAT(k)
            GTROW(bi%ILMOS(k), bi%JLMOS(k)) = GTGAT(k)
            QGROW(bi%ILMOS(k), bi%JLMOS(k)) = QGGAT(k)
            ALVSROW(bi%ILMOS(k), bi%JLMOS(k)) = ALVSGAT(k)
            ALIRROW(bi%ILMOS(k), bi%JLMOS(k)) = ALIRGAT(k)
            SFCTROW(bi%ILMOS(k), bi%JLMOS(k)) = SFCTGAT(k)
            SFCUROW(bi%ILMOS(k), bi%JLMOS(k)) = SFCUGAT(k)
            SFCVROW(bi%ILMOS(k), bi%JLMOS(k)) = SFCVGAT(k)
            SFCQROW(bi%ILMOS(k), bi%JLMOS(k)) = SFCQGAT(k)
            FSNOROW(bi%ILMOS(k), bi%JLMOS(k)) = FSNOGAT(k)
            FSGVROW(bi%ILMOS(k), bi%JLMOS(k)) = FSGVGAT(k)
            FSGSROW(bi%ILMOS(k), bi%JLMOS(k)) = FSGSGAT(k)
            FSGGROW(bi%ILMOS(k), bi%JLMOS(k)) = FSGGGAT(k)
            FLGVROW(bi%ILMOS(k), bi%JLMOS(k)) = FLGVGAT(k)
            FLGSROW(bi%ILMOS(k), bi%JLMOS(k)) = FLGSGAT(k)
            FLGGROW(bi%ILMOS(k), bi%JLMOS(k)) = FLGGGAT(k)
            HFSCROW(bi%ILMOS(k), bi%JLMOS(k)) = HFSCGAT(k)
            HFSSROW(bi%ILMOS(k), bi%JLMOS(k)) = HFSSGAT(k)
            HFSGROW(bi%ILMOS(k), bi%JLMOS(k)) = HFSGGAT(k)
            HEVCROW(bi%ILMOS(k), bi%JLMOS(k)) = HEVCGAT(k)
            HEVSROW(bi%ILMOS(k), bi%JLMOS(k)) = HEVSGAT(k)
            HEVGROW(bi%ILMOS(k), bi%JLMOS(k)) = HEVGGAT(k)
            HMFCROW(bi%ILMOS(k), bi%JLMOS(k)) = HMFCGAT(k)
            HMFNROW(bi%ILMOS(k), bi%JLMOS(k)) = HMFNGAT(k)
            HTCCROW(bi%ILMOS(k), bi%JLMOS(k)) = HTCCGAT(k)
            HTCSROW(bi%ILMOS(k), bi%JLMOS(k)) = HTCSGAT(k)
            PCFCROW(bi%ILMOS(k), bi%JLMOS(k)) = PCFCGAT(k)
            PCLCROW(bi%ILMOS(k), bi%JLMOS(k)) = PCLCGAT(k)
            PCPNROW(bi%ILMOS(k), bi%JLMOS(k)) = PCPNGAT(k)
            PCPGROW(bi%ILMOS(k), bi%JLMOS(k)) = PCPGGAT(k)
            QFGROW(bi%ILMOS(k), bi%JLMOS(k)) = QFGGAT(k)
            QFNROW(bi%ILMOS(k), bi%JLMOS(k)) = QFNGAT(k)
            QFCLROW(bi%ILMOS(k), bi%JLMOS(k)) = QFCLGAT(k)
            QFCFROW(bi%ILMOS(k), bi%JLMOS(k)) = QFCFGAT(k)
            ROFROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFGAT(k)
            ROFOROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFOGAT(k)
            ROFSROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFSGAT(k)
            ROFBROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFBGAT(k)
            TROFROW(bi%ILMOS(k), bi%JLMOS(k)) = TROFGAT(k)
            TROOROW(bi%ILMOS(k), bi%JLMOS(k)) = TROOGAT(k)
            TROSROW(bi%ILMOS(k), bi%JLMOS(k)) = TROSGAT(k)
            TROBROW(bi%ILMOS(k), bi%JLMOS(k)) = TROBGAT(k)
            ROFCROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFCGAT(k)
            ROFNROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFNGAT(k)
            ROVGROW(bi%ILMOS(k), bi%JLMOS(k)) = ROVGGAT(k)
            WTRCROW(bi%ILMOS(k), bi%JLMOS(k)) = WTRCGAT(k)
            WTRSROW(bi%ILMOS(k), bi%JLMOS(k)) = WTRSGAT(k)
            WTRGROW(bi%ILMOS(k), bi%JLMOS(k)) = WTRGGAT(k)
            DRROW(bi%ILMOS(k), bi%JLMOS(k)) = DRGAT(k)
            WTABROW(bi%ILMOS(k), bi%JLMOS(k)) = WTABGAT(k)
            ILMOROW(bi%ILMOS(k), bi%JLMOS(k)) = ILMOGAT(k)
            UEROW(bi%ILMOS(k), bi%JLMOS(k)) = UEGAT(k)
            HBLROW(bi%ILMOS(k), bi%JLMOS(k)) = HBLGAT(k)
180     continue
!>
        do 190 l = 1, bi%IGND
            do 190 k = 1, bi%NML
                HMFGROW(bi%ILMOS(k), bi%JLMOS(k), l) = HMFGGAT(k, l)
                HTCROW(bi%ILMOS(k), bi%JLMOS(k), l) = HTCGAT(k, l)
                QFCROW(bi%ILMOS(k), bi%JLMOS(k), l) = QFCGAT(k, l)
190     continue
!>
        do 230 m = 1, 50
            do 220 l = 1, 6
                do 210 k = 1, bi%NML
                    ITCTROW(bi%ILMOS(k), bi%JLMOS(k), l, m) = ITCTGAT(k, l, m)
210     continue
220     continue
230     continue
    end if !(RESUMEFLAG == 2) then

!> *********************************************************************
!> Call read_init_prog_variables.f90 for initi prognostic variables by
!> by fields needd by classas as initial conditions
!> *********************************************************************
!> bjd - July 14, 2014: Gonzalo Sapriza
    if (RESUMEFLAG == 3) then
        call read_init_prog_variables_class(CMAIROW, QACROW, TACROW, &
                                            TBASROW, TSFSROW, WSNOROW, &
                                            cp, bi%NA, bi%NTYPE, &
                                            bi%IGND, fls)
    end if !(RESUMEFLAG == 3) then

!> *********************************************************************
!> Call CLASSB to set more CLASS variables
!> *********************************************************************
!> bjd - July 25, 2005: For inputting field measured soil properties.

    call CLASSB(THPROW, THRROW, THMROW, BIROW, PSISROW, &
                GRKSROW, THRAROW, HCPSROW, TCSROW, THFCROW, &
                PSIWROW, DLZWROW, ZBTWROW, ALGWROW, ALGDROW, &
                cp%SANDROW, cp%CLAYROW , cp%ORGMROW, sl%DELZ, sl%ZBOT, &
                cp%SDEPROW, ISNDROW, IGDRROW, bi%NA, bi%NTYPE, &
                1, bi%NA, NMTEST, bi%IGND, ICTEMMOD, &
                SV%WC_THPOR, SV%WC_THLRET, SV%WC_THLMIN, SV%WC_BI, SV%WC_PSISAT, &
                SV%WC_GRKSAT, SV%WC_HCPS, SV%WC_TCS)

!> Allocate variables for WATDRN3
!> ******************************************************************
!> DGP - June 3, 2011: Now that variables are shared, moved from WD3
!> flag to ensure allocation.
    allocate(BTC(bi%NTYPE, bi%IGND), BCAP(bi%NTYPE, bi%IGND), DCOEFF(bi%NTYPE, bi%IGND), &
             BFCAP(bi%NTYPE, bi%IGND), BFCOEFF(bi%NTYPE, bi%IGND), BFMIN(bi%NTYPE, bi%IGND), &
             BQMAX(bi%NTYPE, bi%IGND), stat = PAS)

!> Call WATDRN3B to set WATDRN (Ric) variables
!> ******************************************************************
!> DGP - May 5, 2011: Added.
    if (PAS /= 0) print *, 'Error allocating on WD3 for new WATDRN.'
    call WATDRN3B(PSISROW, THPROW, GRKSROW, BIROW, cp%XSLPROW, cp%DDROW, &
                  bi%NA, bi%NTYPE, bi%IGND, &
                  BTC, BCAP, DCOEFF, BFCAP, BFCOEFF, BFMIN, BQMAX, &
                  cp%SANDROW, cp%CLAYROW)

!> *********************************************************************
!> MAM - Initialize ENDDATE and ENDDATA
!> *********************************************************************
    ENDDATE = .false.
    ENDDATA = .false.

    call climate_module_loaddata(bi, .true., cm, ENDDATA)

    if (ipid == 0) then
        TOTAL_STORE = 0.0
        TOTAL_STORE_2 = 0.0
        TOTAL_RCAN = 0.0
        TOTAL_SCAN = 0.0
        TOTAL_SNO = 0.0
        TOTAL_WSNO = 0.0
        TOTAL_ZPND = 0.0
        TOTAL_THLQ = 0.0
        TOTAL_THIC = 0.0
        TOTAL_STORE_M = 0.0
        TOTAL_STORE_2_M = 0.0
        TOTAL_STORE_ACC_M = 0.0
        TOTAL_RCAN_M = 0.0
        TOTAL_SCAN_M = 0.0
        TOTAL_SNO_M = 0.0
        TOTAL_WSNO_M = 0.0
        TOTAL_ZPND_M = 0.0
        TOTAL_THLQ_M = 0.0
        TOTAL_THIC_M = 0.0

    !> Open CSV output files.
        if (BASINBALANCEOUTFLAG > 0) then

        !> Water balance.
!        if ((VARIABLEFILESFLAG == 1) .and. (fls%fl(4)%isInit)) then
            open(fls%fl(mfk%f900)%iun, &
!todo: This creates a bug if a space doesn't exist in the name of the folder!
                 file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/' // trim(adjustl(fls%fl(mfk%f900)%fn)), &
                 iostat = ios)
!        else
!            open(900, file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/Basin_average_water_balance.csv')
            open(902, file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/Basin_average_water_balance_Monthly.csv')
!        end if

            wrt_900_1 = 'DAY,YEAR,PREACC' // ',EVAPACC,ROFACC,ROFOACC,' // &
                'ROFSACC,ROFBACC,PRE,EVAP,ROF,ROFO,ROFS,ROFB,SCAN,RCAN,SNO,WSNO,ZPND,'

            wrt_900_2 = 'THLQ'
            wrt_900_3 = 'THIC'
            wrt_900_4 = 'THLQIC'

            do i = 1, bi%IGND
                write(strInt, '(i1)') i
                if (i < bi%IGND) then
                    wrt_900_2 = trim(adjustl(wrt_900_2)) // trim(adjustl(strInt)) // ',THLQ'
                    wrt_900_3 = trim(adjustl(wrt_900_3)) // trim(adjustl(strInt)) // ',THIC'
                    wrt_900_4 = trim(adjustl(wrt_900_4)) // trim(adjustl(strInt)) // ',THLQIC'
                else
                    wrt_900_2 = trim(adjustl(wrt_900_2)) // trim(adjustl(strInt)) // ','
                    wrt_900_3 = trim(adjustl(wrt_900_3)) // trim(adjustl(strInt)) // ','
                    wrt_900_4 = trim(adjustl(wrt_900_4)) // trim(adjustl(strInt)) // ','
                end if
            end do !> i = 1, bi%IGND

            wrt_900_f = trim(adjustl(wrt_900_1)) // &
                trim(adjustl(wrt_900_2)) // &
                trim(adjustl(wrt_900_3)) // &
                trim(adjustl(wrt_900_4)) // &
                'THLQ,THLIC,THLQIC,STORAGE,DELTA_STORAGE,DSTOR_ACC'

            write(fls%fl(mfk%f900)%iun, '(a)') trim(adjustl(wrt_900_f))
            write(902, '(a)') trim(adjustl(wrt_900_f))

        !> Energy balance.
            open(901, file = './' // GENDIR_OUT(1:index(GENDIR_OUT, ' ') - 1) // '/Basin_average_energy_balance.csv')

            write(901, '(a)') 'DAY,YEAR,HFSACC,QEVPACC'

        end if !(BASINBALANCEOUTFLAG > 0) then

!>**********************************************************************
!> Set initial SnowAge & DrySnow values for PBSM calculations
!> (MK MacDonald, Sept 2010)
!>**********************************************************************
        if (PBSMFLAG == 1) then
            do i = 1, bi%NA  !i = 2, bi%NA
                do m = 1, NMTEST
                    if (cp%SNOROW(i, m) <= 0.0) then
                        DrySnowROW(i, m) = 0.0 !1 = snowpack is dry (i.e. cold)
                        SnowAgeROW(i, m) = 0.0 !hours since last snowfall
       !todo: this can use the TFREZ parameter instead of a hard-coded value. (dgp: 2015-01-09)
                        if (cm%clin(cfk%TT)%climvGrd(i) >= 273.16) then
                            DrySnowROW(i, m) = 0.0
                            SnowAgeROW(i, m) = 48.0 !assume 48 hours since last snowfall
                        else
                            DrySnowROW(i, m) = 1.0
                            SnowAgeROW(i, m) = 48.0
                        end if
                    end if
                end do
            end do
        end if !PBSMFLAG == 1

    end if !(ipid == 0) then

    call CLASSG(TBARGAT, THLQGAT, THICGAT, TPNDGAT, ZPNDGAT, &
                TBASGAT, ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, &
                TCANGAT, RCANGAT, SCANGAT, GROGAT, FRZCGAT, CMAIGAT, &
                FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT, PAMXGAT, &
                PAMNGAT, CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, &
                VPDAGAT, VPDBGAT, PSGAGAT, PSGBGAT, PAIDGAT, &
                HGTDGAT, ACVDGAT, ACIDGAT, TSFSGAT, WSNOGAT, &
                THPGAT, THRGAT, THMGAT, BIGAT, PSISGAT, &
                GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, IGDRGAT, &
                THFCGAT, PSIWGAT, DLZWGAT, ZBTWGAT, VMODGAT, &
                ZSNLGAT, ZPLGGAT, ZPLSGAT, TACGAT, QACGAT, &
                DRNGAT, XSLPGAT, XDGAT, WFSFGAT, KSGAT, &
                ALGWGAT, ALGDGAT, ASVDGAT, ASIDGAT, AGVDGAT, &
                AGIDGAT, ISNDGAT, RADJGAT, ZBLDGAT, Z0ORGAT, &
                ZRFMGAT, ZRFHGAT, ZDMGAT, ZDHGAT, FSVHGAT, &
                FSIHGAT, CSZGAT, FDLGAT, ULGAT, VLGAT, &
                TAGAT, QAGAT, PRESGAT, PREGAT, PADRGAT, &
                VPDGAT, TADPGAT, RHOAGAT, RPCPGAT, TRPCGAT, &
                SPCPGAT, TSPCGAT, RHSIGAT, FCLOGAT, DLONGAT, &
                GGEOGAT, &
                CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT, &
                QFSGAT, QFXGAT, PETGAT, GAGAT, EFGAT, &
                GTGAT, QGGAT, ALVSGAT, ALIRGAT, &
                SFCTGAT, SFCUGAT, SFCVGAT, SFCQGAT, FSNOGAT, &
                FSGVGAT, FSGSGAT, FSGGGAT, FLGVGAT, FLGSGAT, &
                FLGGGAT, HFSCGAT, HFSSGAT, HFSGGAT, HEVCGAT, &
                HEVSGAT, HEVGGAT, HMFCGAT, HMFNGAT, HTCCGAT, &
                HTCSGAT, PCFCGAT, PCLCGAT, PCPNGAT, PCPGGAT, &
                QFGGAT, QFNGAT, QFCLGAT, QFCFGAT, ROFGAT, &
                ROFOGAT, ROFSGAT, ROFBGAT, TROFGAT, TROOGAT, &
                TROSGAT, TROBGAT, ROFCGAT, ROFNGAT, ROVGGAT, &
                WTRCGAT, WTRSGAT, WTRGGAT, DRGAT, GFLXGAT, &
                HMFGGAT, HTCGAT, QFCGAT, ITCTGAT, &
!BEGIN: PDMROF
                CMINPDM, CMAXPDM, BPDM, K1PDM, K2PDM,  &
!END: PDMROF
                bi%ILMOS, bi%JLMOS, bi%IWMOS, bi%JWMOS, bi%NML, bi%NA, bi%NTYPE, &
                bi%NA*bi%NTYPE, bi%IGND, ICAN, ICP1, cp%TBARROW, cp%THLQROW, &
                cp%THICROW, cp%TPNDROW, cp%ZPNDROW, TBASROW, cp%ALBSROW, &
                cp%TSNOROW, cp%RHOSROW, cp%SNOROW, cp%TCANROW, &
                cp%RCANROW, cp%SCANROW, cp%GROROW, CMAIROW, cp%FCANROW, &
                cp%LNZ0ROW, cp%ALVCROW, cp%ALICROW, cp%PAMXROW, &
                cp%PAMNROW, cp%CMASROW, cp%ROOTROW, cp%RSMNROW, &
                cp%QA50ROW, cp%VPDAROW, cp%VPDBROW, cp%PSGAROW, &
                cp%PSGBROW, PAIDROW, HGTDROW, ACVDROW, ACIDROW, TSFSROW, &
                WSNOROW, THPROW, THRROW, THMROW, BIROW, PSISROW, &
                GRKSROW, THRAROW, HCPSROW, TCSROW, IGDRROW, &
                THFCROW, PSIWROW, DLZWROW, ZBTWROW, VMODGRD, &
                hp%ZSNLROW, hp%ZPLGROW, hp%ZPLSROW, hp%FRZCROW, TACROW, QACROW, &
                cp%DRNROW, cp%XSLPROW, cp%XDROW, WFSFROW, cp%KSROW, &
                ALGWROW, ALGDROW, ASVDROW, ASIDROW, AGVDROW, &
                AGIDROW, ISNDROW, RADJGRD, cp%ZBLDGRD, Z0ORGRD, &
                cp%ZRFMGRD, cp%ZRFHGRD, ZDMGRD, ZDHGRD, FSVHGRD, &
                FSIHGRD, CSZGRD, cm%clin(cfk%FI)%climvGrd, cm%clin(cfk%UV)%climvGrd, VLGRD, &
                cm%clin(cfk%TT)%climvGrd, cm%clin(cfk%HU)%climvGrd, cm%clin(cfk%P0)%climvGrd, cm%clin(cfk%PR)%climvGrd, PADRGRD, &
                VPDGRD, TADPGRD, RHOAGRD, RPCPGRD, TRPCGRD, &
                SPCPGRD, TSPCGRD, RHSIGRD, FCLOGRD, DLONGRD, &
                GGEOGRD, cp%MANNROW, MANNGAT, cp%DDROW, DDGAT, &
                cp%SANDROW, SANDGAT, cp%CLAYROW, CLAYGAT, &
!BEGIN: PDMROF
                hp%CMINROW, hp%CMAXROW, hp%BROW, hp%K1ROW, hp%K2ROW, &
!END: PDMROF
                cp%FAREROW, FAREGAT, &
                hp%fetchROW, hp%HtROW, hp%N_SROW, hp%A_SROW, hp%DistribROW, &
                fetchGAT, HtGAT, N_SGAT, A_SGAT, DistribGAT, &
                DrySnowRow, SnowAgeROW, DrySnowGAT, SnowAgeGAT, &
                TSNOdsROW, RHOSdsROW, TSNOdsGAT, RHOSdsGAT, &
                DriftROW, SublROW, DepositionROW, &
                DriftGAT, SublGAT, DepositionGAT)

!todo+++: Perhaps land-unit indexing can be done prior in the sequence
!todo+++: of initialization, after reading the drainage database.
!todo+++: Then, variables could be allocated (il1:il2) instead of
!todo+++: (1:ILG) to reduce the memory footprint of the model per node.
!> *********************************************************************
!> Calculate Indices
!> *********************************************************************

    call GetIndices(inp, izero, ipid, bi%NML, bi%ILMOS, il1, il2, ilen)

    !> Initialize and open files for CLASS output.
    if (WF_NUM_POINTS > 0) then

        !> After GATPREP. Determine the GAT-index of the output point.
        do k = il1, il2
            do i = 1, WF_NUM_POINTS
                if (op%N_OUT(i) == bi%ILMOS(k) .and. op%II_OUT(i) == bi%JLMOS(k)) op%K_OUT(i) = k
            end do
        end do

        !> Allocate the CLASS output variables.
        allocate(co%PREACC(WF_NUM_POINTS), co%GTACC(WF_NUM_POINTS), co%QEVPACC(WF_NUM_POINTS), co%EVAPACC(WF_NUM_POINTS), &
                 co%HFSACC(WF_NUM_POINTS), co%HMFNACC(WF_NUM_POINTS), &
                 co%ROFACC(WF_NUM_POINTS), co%ROFOACC(WF_NUM_POINTS), co%ROFSACC(WF_NUM_POINTS), co%ROFBACC(WF_NUM_POINTS), &
                 co%WTBLACC(WF_NUM_POINTS), co%ALVSACC(WF_NUM_POINTS), co%ALIRACC(WF_NUM_POINTS), &
                 co%RHOSACC(WF_NUM_POINTS), co%TSNOACC(WF_NUM_POINTS), co%WSNOACC(WF_NUM_POINTS), co%SNOARE(WF_NUM_POINTS), &
                 co%TCANACC(WF_NUM_POINTS), co%CANARE(WF_NUM_POINTS), co%SNOACC(WF_NUM_POINTS), &
                 co%RCANACC(WF_NUM_POINTS), co%SCANACC(WF_NUM_POINTS), co%GROACC(WF_NUM_POINTS), co%FSINACC(WF_NUM_POINTS), &
                 co%FLINACC(WF_NUM_POINTS), co%FLUTACC(WF_NUM_POINTS), &
                 co%TAACC(WF_NUM_POINTS), co%UVACC(WF_NUM_POINTS), co%PRESACC(WF_NUM_POINTS), co%QAACC(WF_NUM_POINTS))
        allocate(co%TBARACC(WF_NUM_POINTS, bi%IGND), co%THLQACC(WF_NUM_POINTS, bi%IGND), co%THICACC(WF_NUM_POINTS, bi%IGND), &
                 co%THALACC(WF_NUM_POINTS, bi%IGND), co%GFLXACC(WF_NUM_POINTS, bi%IGND))

        !> Initialize the CLASS output variables.
        co%PREACC = 0.0
        co%GTACC = 0.0
        co%QEVPACC = 0.0
        co%EVAPACC = 0.0
        co%HFSACC = 0.0
        co%HMFNACC = 0.0
        co%ROFACC = 0.0
        co%ROFOACC = 0.0
        co%ROFSACC = 0.0
        co%ROFBACC = 0.0
        co%WTBLACC = 0.0
        co%TBARACC = 0.0
        co%THLQACC = 0.0
        co%THICACC = 0.0
        co%THALACC = 0.0
        co%GFLXACC = 0.0
        co%ALVSACC = 0.0
        co%ALIRACC = 0.0
        co%RHOSACC = 0.0
        co%TSNOACC = 0.0
        co%WSNOACC = 0.0
        co%SNOARE = 0.0
        co%TCANACC = 0.0
        co%CANARE = 0.0
        co%SNOACC = 0.0
        co%RCANACC = 0.0
        co%SCANACC = 0.0
        co%GROACC = 0.0
        co%FSINACC = 0.0
        co%FLINACC = 0.0
        co%FLUTACC = 0.0
        co%TAACC = 0.0
        co%UVACC = 0.0
        co%PRESACC = 0.0
        co%QAACC = 0.0

        !> Open the files if the GAT-index of the output point resides on this node.
        do i = 1, WF_NUM_POINTS
            if (op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2) then

                !> Open the files in the appropriate directory.
                BNAM = op%DIR_OUT(i)
                j = 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF1.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF2.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF3.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF4.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF5.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF6.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF7.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF8.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/CLASSOF9.csv'); j = j + 1
                open(150 + i*10 + j, file = './' // trim(adjustl(BNAM)) // '/GRU_water_balance.csv')

                !> Write project header information.
                do j = 1, 9
                    write(150 + i*10 + j, "('CLASS TEST RUN:     ', 6a4)") TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6
                    write(150 + i*10 + j, "('RESEARCHER:         ', 6a4)") NAME1, NAME2, NAME3, NAME4, NAME5, NAME6
                    write(150 + i*10 + j, "('INSTITUTION:        ', 6a4)") PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
                end do

                !> CLASSOF1.
                write(150 + i*10 + 1, "('IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,BEG," // &
                    'GTOUT,SNOACC(I),RHOSACC(I),WSNOACC(I),ALTOT,ROFACC(I),' // &
                    "ROFOACC(I),ROFSACC(I),ROFBACC(I)')")

                !> CLASSOF2.
                write(FMT, *) ''
                do j = 1, bi%IGND
                    write(IGND_CHAR, *) j
                    IGND_CHAR = adjustl(IGND_CHAR)
                    FMT = trim(adjustl(FMT)) // 'TBARACC(I ' // trim(IGND_CHAR) // ')-TFREZ,THLQACC(I ' // &
                        trim(IGND_CHAR) // '),THICACC(I ' // trim(IGND_CHAR) // '),'
                end do
                write(150 + i*10 + 2, "('IDAY,IYEAR," // trim(FMT) // "TCN,RCANACC(I),SCANACC(I),TSN,ZSN')")

                !> CLASSOF3.
                write(150 + i*10 + 3, "('IDAY,IYEAR,FSINACC(I),FLINACC(I)," // &
                    'TAACC(I)-TFREZ,UVACC(I),PRESACC(I),QAACC(I),PREACC(I),' // &
                    "EVAPACC(I)')")

                !> CLASSOF4.
                write(150 + i*10 + 4, "('IHOUR,IMIN,IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE," // &
                    'SNOMLT,BEG,GTOUT,SNOROW(I M),RHOSROW(I M),WSNOROW(I M),ALTOT,' // &
                    "ROFROW(I M),TPN,ZPNDROW(I M),ZPND,FSTR')")

                !> CLASSOF5.
                write(FMT, *) ''
                do j = 1, bi%IGND
                    write(IGND_CHAR, *) j
                    IGND_CHAR = adjustl(IGND_CHAR)
                    FMT = trim(adjustl(FMT)) // 'TBARROW(I ' // trim(IGND_CHAR) // ')-TFREZ,THLQROW(I ' // &
                        trim(IGND_CHAR) // '),THICROW(I ' // trim(IGND_CHAR) // '),'
                end do
                write(150 + i*10 + 5, "('IHOUR,IMIN,IDAY,IYEAR," // trim(FMT) // "TCN,RCANROW(I M),SCANROW(I M),TSN,ZSN')")

                !> CLASSOF6.
                write(150 + i*10 + 6, "('IHOUR,IMIN,IDAY,FSDOWN(I),FDLGRD(I)," // &
                    "PREGRD(I),TAGRD(I)-TFREZ,UVGRD(I),PRESGRD(I),QAGRD(I)')")

                !> CLASSOF7.
                write(150 + i*10 + 7,"('TROFROW(I M),TROOROW(I M),TROSROW(I M)," // &
                    'TROBROW(I M),ROFROW(I M),ROFOROW(I M),ROFSROW(I M),' // &
                    "ROFBROW(I M),FCS(I),FGS(I),FC(I),FG(I)')")

                !> CLASSOF8.
                write(FMT, *) ''
                do j = 1, bi%IGND
                    write(IGND_CHAR, *) j
                    IGND_CHAR = adjustl(IGND_CHAR)
                    FMT = trim(adjustl(FMT)) // ',HMFGROW(I M ' // trim(IGND_CHAR) // ')'
                end do
                FMT = trim(adjustl(FMT)) // ',HTCCROW(I M),HTCSROW(I M)'
                do j = 1, bi%IGND
                    write(IGND_CHAR, *) j
                    IGND_CHAR = adjustl(IGND_CHAR)
                    FMT = trim(adjustl(FMT)) // ',HTCROW(I M ' // trim(IGND_CHAR) // ')'
                end do
                write(150 + i*10 + 8, "('FSGVROW(I M),FSGSROW(I M),FSGGROW(I M)," // &
                    'FLGVROW(I M),FLGSROW(I M),FLGGROW(I M),HFSCROW(I M),' // &
                    'HFSSROW(I M),HFSGROW(I M),HEVCROW(I M),HEVSROW(I M),' // &
                    'HEVGROW(I M),HMFCROW(I M),HMFNROW(I M)' // trim(FMT) // "')")

                !> CLASSOF9.
                write(FMT, *) ''
                do j = 1, bi%IGND
                    write(IGND_CHAR, *) j
                    IGND_CHAR = adjustl(IGND_CHAR)
                    FMT = trim(adjustl(FMT)) // 'QFCROW(I M ' // trim(IGND_CHAR) // '),'
                end do
                write(150 + i*10 + 9, "('PCFCROW(I M),PCLCROW(I M),PCPNROW(I M)," // &
                    'PCPGROW(I M),QFCFROW(I M),QFCLROW(I M),QFNROW(I M),QFGROW(I M),' // trim(FMT) // 'ROFCROW(I M),' // &
                    'ROFNROW(I M),ROFOROW(I M),ROFROW(I M),WTRCROW(I M),' // &
                    "WTRSROW(I M),WTRGROW(I M)')")

                !> GRU water balance file.
                write(FMT, *) ''
                do j = 1, bi%IGND
                    write(IGND_CHAR, *) j
                    IGND_CHAR = adjustl(IGND_CHAR)
                    FMT = trim(adjustl(FMT)) // 'THLQ' // trim(IGND_CHAR) // ','
                end do
                do j = 1, bi%IGND
                    write(IGND_CHAR, *) j
                    IGND_CHAR = adjustl(IGND_CHAR)
                    FMT = trim(adjustl(FMT)) // 'THIC' // trim(IGND_CHAR) // ','
                end do
                write(150 + i*10 + 10, "('IHOUR,IMIN,IDAY,IYEAR," // &
                    'PRE,EVAP,ROF,ROFO,ROFS,ROFB,' // &
                    'SCAN,RCAN,SNO,WSNO,ZPND,' // trim(FMT) // "')")

            end if !(op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2) then
        end do !i = 1, wf_num_points
    end if !(WF_NUM_POINTS > 0) then

!> *********************************************************************
!> End of Initialization
!> *********************************************************************

    if (ro%VERBOSEMODE > 0) then
        print *
        print 2836
        print 2835
    end if !(ro%VERBOSEMODE > 0) then

2836    format(/1x, 'DONE INTITIALIZATION')
2835    format(/1x, 'STARTING MESH')

!> *********************************************************************
!> Start of main loop that is run each half hour
!> *********************************************************************
    do while (.not. ENDDATE .and. .not. ENDDATA)

!* N: is only used for debugging purposes.
!> N is incremented at the beginning of each loop. so you can tell which
!> iteration of the loop you are on by what the value of N is.
!> N is printed out with each of the error messages in CLASSZ.
        N = N + 1

    !> MAM - Linearly interpolate forcing data for intermediate time steps
        if (INTERPOLATIONFLAG == 1) then
            call climate_module_interpolatedata(bi, FAREGAT, cm)
        end if
        UVGRD = max(VMIN, cm%clin(cfk%UV)%climvGrd)
        VMODGRD = UVGRD
        VMODGAT = max(VMIN, ULGAT)

!> *********************************************************************
!> Read in current reservoir release value
!> *********************************************************************

!> only read in current value if we are on the correct time step
!> however put in an exception if this is the first time through (ie. jan = 1),
!> otherwise depending on the hour of the first time step
!> there might not be any data in wf_qrel, wf_qhyd
!> make sure we have a controlled reservoir (if not the mod(HOUR_NOW, wf_ktr)
!> may give an error. Frank S Jun 2007
        if (WF_NORESV_CTRL > 0) then
            if (mod(HOUR_NOW, WF_KTR) == 0 .and. MINS_NOW == 0) then
!>        READ in current reservoir value
                read(21, '(100f10.3)', iostat = IOS) (WF_QREL(i), i = 1, WF_NORESV_CTRL)
                if (IOS /= 0) then
                    print *, 'ran out of reservoir data before met data'
                    stop
                end if
            else
                if (JAN == 1 .and. WF_NORESV_CTRL > 0) then
                    read(21, '(100f10.3)', iostat = IOS) (WF_QREL(i), i = 1, WF_NORESV_CTRL)
                    rewind 21
                    read(21, *)
                    do i = 1, WF_NORESV
                        read(21, *)
                    end do
                end if
            end if
        end if

! *********************************************************************
!> Read in current streamflow value
!> *********************************************************************

!> only read in current value if we are on the correct time step
!> also read in the first value if this is the first time through
        if (mod(HOUR_NOW, WF_KT) == 0 .and. MINS_NOW == 0 .and. JAN > 1) then
!>       read in current streamflow value
            read(22, *, iostat = IOS) (WF_QHYD(i), i = 1, WF_NO)
            if (IOS /= 0) then
                print *, 'ran out of streamflow data before met data'
                stop
            end if
        end if

!> *********************************************************************
!> Set some more CLASS parameters
!> *********************************************************************

!> This estimates the fractional cloud cover (FCLOGRD) by the basis
!>  of the solar zenith angle and the occurrence of precipitation.
!>  Assumed to be 1 (100%) when precipitation occurs and somewhere
!>  in the range of [0.1, 1] based on the location of the sun in the
!>  sky when precipitation is not occuring. (0.1 when the sun is at
!>  the zenith, 1 when the sun is at the horizon).
        RDAY = real(JDAY_NOW) + (real(HOUR_NOW) + real(MINS_NOW)/60.0)/24.0
        DECL = sin(2.0*PI*(284.0 + RDAY)/365.0)*23.45*PI/180.0
        HOUR = (real(HOUR_NOW) + real(MINS_NOW)/60.0)*PI/12.0 - PI

        do i = il1, il2
            COSZ = sin(RADJGAT(i))*sin(DECL) + cos(RADJGAT(i))*cos(DECL)*cos(HOUR)
            CSZGAT(i) = sign(max(abs(COSZ), 1.0e-3), COSZ)
            CSZGRD(bi%ILMOS(i)) = CSZGAT(i)
            if (PREGAT(i) > 0.0) then
    !todo: there isn't a GAT variable for this (although, there might be for the canopy)?
                XDIFFUS(bi%ILMOS(i)) = 1.0
            else
                XDIFFUS(bi%ILMOS(i)) = max(0.0, min(1.0 - 0.9*COSZ, 1.0))
            end if
            FCLOGAT(i) = XDIFFUS(bi%ILMOS(i))
            FCLOGRD(bi%ILMOS(i)) = FCLOGAT(i)
        end do

!> *********************************************************************
!> Start of calls to CLASS subroutines
!> *********************************************************************

        !> Were initialized in CLASSG and so have been extracted.
        DriftGAT = 0.0 !DriftROW (ILMOS(k), JLMOS(k))
        SublGAT = 0.0 !SublROW (ILMOS(k), JLMOS(k))
        DepositionGAT = 0.0

!>
!>   * INITIALIZATION OF DIAGNOSTIC VARIABLES SPLIT OUT OF CLASSG
!>   * FOR CONSISTENCY WITH GCM APPLICATIONS.
!>

        CDHGAT = 0.0
        CDMGAT = 0.0
        HFSGAT = 0.0
        TFXGAT = 0.0
        QEVPGAT = 0.0
        QFSGAT = 0.0
        QFXGAT = 0.0
        PETGAT = 0.0
        GAGAT = 0.0
        EFGAT = 0.0
        GTGAT = 0.0
        QGGAT = 0.0
        ALVSGAT = 0.0
        ALIRGAT = 0.0
        SFCTGAT = 0.0
        SFCUGAT = 0.0
        SFCVGAT = 0.0
        SFCQGAT = 0.0
        FSNOGAT = 0.0
        FSGVGAT = 0.0
        FSGSGAT = 0.0
        FSGGGAT = 0.0
        FLGVGAT = 0.0
        FLGSGAT = 0.0
        FLGGGAT = 0.0
        HFSCGAT = 0.0
        HFSSGAT = 0.0
        HFSGGAT = 0.0
        HEVCGAT = 0.0
        HEVSGAT = 0.0
        HEVGGAT = 0.0
        HMFCGAT = 0.0
        HMFNGAT = 0.0
        HTCCGAT = 0.0
        HTCSGAT = 0.0
        PCFCGAT = 0.0
        PCLCGAT = 0.0
        PCPNGAT = 0.0
        PCPGGAT = 0.0
        QFGGAT = 0.0
        QFNGAT = 0.0
        QFCFGAT = 0.0
        QFCLGAT = 0.0
        ROFGAT = 0.0
        ROFOGAT = 0.0
        ROFSGAT = 0.0
        ROFBGAT = 0.0
        TROFGAT = 0.0
        TROOGAT = 0.0
        TROSGAT = 0.0
        TROBGAT = 0.0
        ROFCGAT = 0.0
        ROFNGAT = 0.0
        ROVGGAT = 0.0
        WTRCGAT = 0.0
        WTRSGAT = 0.0
        WTRGGAT = 0.0
        DRGAT = 0.0
320     continue
!>
        HMFGGAT = 0.0
        HTCGAT = 0.0
        QFCGAT = 0.0
        GFLXGAT = 0.0
330     continue
340     continue
!>
        ITCTGAT = 0
350     continue
360     continue
370     continue
!>
        call CLASSI(VPDGAT, TADPGAT, PADRGAT, RHOAGAT, RHSIGAT, &
                    RPCPGAT, TRPCGAT, SPCPGAT, TSPCGAT, TAGAT, QAGAT, &
                    PREGAT, RPREGAT, SPREGAT, PRESGAT, &
                    IPCP, bi%ILG, il1, il2)

        if (ipid == 0) then

!> Calculate initial storage (after reading in resume.txt file if applicable)
            if (JAN == 1) then
                INIT_STORE = 0.0
                do i = 1, bi%NA
                    if (bi%FRAC(i) >= 0.0) then
                        do m = 1, NMTEST
                            INIT_STORE = INIT_STORE + cp%FAREROW(i, m)* &
                                (cp%RCANROW(i, m) + cp%SCANROW(i, m) + cp%SNOROW(i, m) + WSNOROW(i, m) + cp%ZPNDROW(i, m)*RHOW)
                            wb%stg(i) = cp%FAREROW(i, m)* &
                                (cp%RCANROW(i, m) + cp%SCANROW(i, m) + cp%SNOROW(i, m) + WSNOROW(i, m) + cp%ZPNDROW(i, m)*RHOW)
                            do j = 1, bi%IGND
                                INIT_STORE = INIT_STORE + cp%FAREROW(i, m)* &
                                    (cp%THLQROW(i, m, j)*RHOW + cp%THICROW(i, m, j)*RHOICE)*DLZWROW(i, m, j)
                                wb%stg(i) = cp%FAREROW(i, m)* &
                                    (cp%THLQROW(i, m, j)*RHOW + cp%THICROW(i, m, j)*RHOICE)*DLZWROW(i, m, j)
                            end do
                        end do
                        wb%dstg(i) = wb%stg(i)
                    end if
                end do
                TOTAL_STORE_2 = INIT_STORE

    ! For monthly totals.
                call FIND_MONTH(JDAY_NOW, YEAR_NOW, imonth_old)
                TOTAL_STORE_2_M = INIT_STORE
            end if

!>=========================================================================
!> Initialization of the Storage field
            if (JAN == 1) then
                do m = 1, NMTEST
                    STG_I(:) = STG_I(:) + cp%FAREROW(:, m)*(cp%RCANROW(:, m) + &
                                                            cp%SCANROW(:, m) + &
                                                            cp%SNOROW(:, m)  + &
                                                            cp%ZPNDROW(:, m)*RHOW)
                    do j = 1, bi%IGND
                        STG_I(:) = STG_I(:) + cp%FAREROW(:, m)*(cp%THLQROW(:, m, j)*RHOW + &
                                                                cp%THICROW(:, m, j)*RHOICE)*DLZWROW(:, m, j)
                    end do
                end do
            end if

        end if !(ipid == 0) then

!> *********************************************************************
!> Start of the NML-based LSS loop.
!> *********************************************************************

!> ========================================================================
        if (ipid /= 0 .or. izero == 0) then

            call CLASSZ(0, CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, &
                        WTVSTP, WTSSTP, WTGSTP, &
                        FSGVGAT, FLGVGAT, HFSCGAT, HEVCGAT, HMFCGAT, HTCCGAT, &
                        FSGSGAT, FLGSGAT, HFSSGAT, HEVSGAT, HMFNGAT, HTCSGAT, &
                        FSGGGAT, FLGGGAT, HFSGGAT, HEVGGAT, HMFGGAT, HTCGAT, &
                        PCFCGAT, PCLCGAT, QFCFGAT, QFCLGAT, ROFCGAT, WTRCGAT, &
                        PCPNGAT, QFNGAT, ROFNGAT, WTRSGAT, PCPGGAT, QFGGAT, &
                        QFCGAT, ROFGAT, WTRGGAT, CMAIGAT, RCANGAT, SCANGAT, &
                        TCANGAT, SNOGAT, WSNOGAT, TSNOGAT, THLQGAT, THICGAT, &
                        HCPSGAT, THPGAT, DLZWGAT, TBARGAT, ZPNDGAT, TPNDGAT, &
                        sl%DELZ, FCS, FGS, FC, FG, &
                        il1, il2, bi%ILG, bi%IGND, N, &
                        DriftGAT, SublGAT)

!> ========================================================================
!> ALBEDO AND TRANSMISSIVITY CALCULATIONS; GENERAL VEGETATION
!> CHARACTERISTICS.
            call CLASSA(FC, FG, FCS, FGS, ALVSCN, ALIRCN, &
                        ALVSG, ALIRG, ALVSCS, ALIRCS, ALVSSN, ALIRSN, &
                        ALVSGC, ALIRGC, ALVSSC, ALIRSC, TRVSCN, TRIRCN, &
                        TRVSCS, TRIRCS, FSVF, FSVFS, &
                        RAICAN, RAICNS, SNOCAN, SNOCNS, FRAINC, FSNOWC, &
                        FRAICS, FSNOCS, &
                        DISP, DISPS, ZOMLNC, ZOMLCS, &
                        ZOELNC, ZOELCS, ZOMLNG, ZOMLNS, ZOELNG, ZOELNS, &
                        CHCAP, CHCAPS, CMASSC, CMASCS, CWLCAP, CWFCAP, &
                        CWLCPS, CWFCPS, RC, RCS, RBCOEF, FROOT, &
                        ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, TRSNOW, ZSNOW, &
                        WSNOGAT, ALVSGAT, ALIRGAT, HTCCGAT, HTCSGAT, HTCGAT, &
                        WTRCGAT, WTRSGAT, WTRGGAT, CMAIGAT, FSNOGAT, &
                        FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT, PAMXGAT, PAMNGAT, &
                        CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, VPDAGAT, VPDBGAT, &
                        PSGAGAT, PSGBGAT, PAIDGAT, HGTDGAT, ACVDGAT, ACIDGAT, &
                        ASVDGAT, ASIDGAT, AGVDGAT, AGIDGAT, ALGWGAT, ALGDGAT, &
                        THLQGAT, THICGAT, TBARGAT, RCANGAT, SCANGAT, TCANGAT, &
                        GROGAT, SNOGAT, TSNOGAT, RHOSGAT, ALBSGAT, ZBLDGAT, &
                        Z0ORGAT, ZSNLGAT, ZPLGGAT, ZPLSGAT, &
                        FCLOGAT, TAGAT, VPDGAT, RHOAGAT, CSZGAT, &
                        FSVHGAT, RADJGAT, DLONGAT, RHSIGAT, sl%DELZ, DLZWGAT, &
                        ZBTWGAT, THPGAT, THMGAT, PSISGAT, BIGAT, PSIWGAT, &
                        HCPSGAT, ISNDGAT, &
                        FCANCMX, ICTEM, ICTEMMOD, RMATC, &
                        AILC, PAIC, L2MAX, NOL2PFTS, &
                        AILCG, AILCGS, FCANC, FCANCS, &
                        JDAY_NOW, bi%ILG, il1, il2, &
                        JLAT, N, ICAN, ICAN + 1, bi%IGND, IDISP, IZREF, &
                        IWF, IPAI, IHGT, IALC, IALS, IALG)
!
!-----------------------------------------------------------------------
!          * SURFACE TEMPERATURE AND FLUX CALCULATIONS.
!
            call CLASST(TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG, &
                        THICEC, THICEG, HCPC, HCPG, TCTOPC, TCBOTC, TCTOPG, TCBOTG, &
                        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, G12CS, G12GS, &
                        G23C, G23G, G23CS, G23GS, QFREZC, QFREZG, QMELTC, QMELTG, &
                        EVAPC, EVAPCG, EVAPG, EVAPCS, EVPCSG, EVAPGS, TCANO, TCANS, &
                        RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP, CHCAPS, TPONDC, TPONDG, &
                        TPNDCS, TPNDGS, TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, &
                        ITCTGAT, CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT, QFSGAT, QFXGAT, &
                        PETGAT, GAGAT, EFGAT, GTGAT, QGGAT, SFCTGAT, SFCUGAT, SFCVGAT, &
                        SFCQGAT, SFRHGAT, FSGVGAT, FSGSGAT, FSGGGAT, FLGVGAT, FLGSGAT, FLGGGAT, &
                        HFSCGAT, HFSSGAT, HFSGGAT, HEVCGAT, HEVSGAT, HEVGGAT, HMFCGAT, HMFNGAT, &
                        HTCCGAT, HTCSGAT, HTCGAT, QFCFGAT, QFCLGAT, DRGAT, WTABGAT, ILMOGAT, &
                        UEGAT, HBLGAT, TACGAT, QACGAT, ZRFMGAT, ZRFHGAT, ZDMGAT, ZDHGAT, &
                        VPDGAT, TADPGAT, RHOAGAT, FSVHGAT, FSIHGAT, FDLGAT, ULGAT, VLGAT, &
                        TAGAT, QAGAT, PADRGAT, FC, FG, FCS, FGS, RBCOEF, &
                        FSVF, FSVFS, PRESGAT, VMODGAT, ALVSCN, ALIRCN, ALVSG, ALIRG, &
                        ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, ALIRSC, &
                        TRVSCN, TRIRCN, TRVSCS, TRIRCS, RC, RCS, WTRGGAT, QLWOGAT, &
                        FRAINC, FSNOWC, FRAICS, FSNOCS, CMASSC, CMASCS, DISP, DISPS, &
                        ZOMLNC, ZOELNC, ZOMLNG, ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, &
                        TBARGAT, THLQGAT, THICGAT, TPNDGAT, ZPNDGAT, TBASGAT, TCANGAT, TSNOGAT, &
                        ZSNOW, TRSNOW, RHOSGAT, WSNOGAT, THPGAT, THRGAT, THMGAT, THFCGAT, &
                        RADJGAT, PREGAT, HCPSGAT, TCSGAT, TSFSGAT, sl%DELZ, DLZWGAT, ZBTWGAT, &
                        FTEMP, FVAP, RIB, ISNDGAT, &
                        AILCG, AILCGS, FCANC, FCANCS, CO2CONC, CO2I1CG, CO2I1CS, CO2I2CG, &
                        CO2I2CS, COSZS, XDIFFUSC, SLAI, ICTEM, ICTEMMOD, RMATCTEM, &
                        FCANCMX, L2MAX, NOL2PFTS, CFLUXCG, CFLUXCS, ANCSVEG, ANCGVEG, &
                        RMLCSVEG, RMLCGVEG, FIELDSM, WILTSM, &
                        ITC, ITCG, ITG, bi%ILG, il1, il2, JLAT, N, ICAN, &
                        bi%IGND, IZREF, ISLFD, NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI)
!
!-----------------------------------------------------------------------
!          * WATER BUDGET CALCULATIONS.
!
            if (JDAY_NOW == 1 .and. NCOUNT == 48) then
       ! bruce davison - only increase NMELT if we don't start the run on January 1st, otherwise t0_ACC allocation is too large
       ! and the model crashes if the compiler is checking for array bounds when t0_ACC is passed into CLASSW with size NMELT
                if (JDAY_START == 1 .and. NSUM_TOTAL < 49) then
                    continue ! NMELT should stay = 1
                else
                    NMELT = NMELT + 1
                end if
                CUMSNOWINFILCS = 0.0
                CUMSNOWINFILGS = 0.0
                INFILTYPE = 2
            end if

            call CLASSW(THLQGAT, THICGAT, TBARGAT, TCANGAT, RCANGAT, SCANGAT, &
                        ROFGAT, TROFGAT, SNOGAT, TSNOGAT, RHOSGAT, ALBSGAT, &
                        WSNOGAT, ZPNDGAT, TPNDGAT, GROGAT, FRZCGAT, TBASGAT, GFLXGAT, &
                        PCFCGAT, PCLCGAT, PCPNGAT, PCPGGAT, QFCFGAT, QFCLGAT, &
                        QFNGAT, QFGGAT, QFCGAT, HMFCGAT, HMFGGAT, HMFNGAT, &
                        HTCCGAT, HTCSGAT, HTCGAT, ROFCGAT, ROFNGAT, ROVGGAT, &
                        WTRSGAT, WTRGGAT, ROFOGAT, ROFSGAT, ROFBGAT, &
                        TROOGAT, TROSGAT, TROBGAT, QFSGAT, &
                        TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG, &
                        THICEC, THICEG, HCPC, HCPG, RPCPGAT, TRPCGAT, &
                        SPCPGAT, TSPCGAT, PREGAT, TAGAT, RHSIGAT, GGEOGAT, &
                        FC, FG, FCS, FGS, TPONDC, TPONDG, &
                        TPNDCS, TPNDGS, EVAPC, EVAPCG, EVAPG, EVAPCS, &
                        EVPCSG, EVAPGS, QFREZC, QFREZG, QMELTC, QMELTG, &
                        RAICAN, SNOCAN, RAICNS, SNOCNS, FROOT, FSVF, &
                        FSVFS, CWLCAP, CWFCAP, CWLCPS, CWFCPS, TCANO, &
                        TCANS, CHCAP, CHCAPS, CMASSC, CMASCS, ZSNOW, &
                        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, &
                        G12CS, G12GS, G23C, G23G, G23CS, G23GS, &
                        TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, &
                        ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, TSFSGAT, &
                        TCTOPC, TCBOTC, TCTOPG, TCBOTG, &
                        THPGAT, THRGAT, THMGAT, BIGAT, PSISGAT, GRKSGAT, &
                        THRAGAT, THFCGAT, DRNGAT, HCPSGAT, sl%DELZ, &
                        DLZWGAT, ZBTWGAT, XSLPGAT, XDGAT, WFSFGAT, KSGAT, &
                        ISNDGAT, IGDRGAT, IWF, bi%NA*bi%NTYPE, il1, il2, N, &
                        JLAT, ICAN, bi%IGND, bi%IGND + 1, bi%IGND + 2, &
                        NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI, &
                        MANNGAT, DDGAT, NCOUNT, &
                        t0_ACC(NMELT), SI, TSI, INFILTYPE, SNOWMELTD, SNOWMELTD_LAST, &
                        MELTRUNOFF, SNOWINFIL, CUMSNOWINFILCS, CUMSNOWINFILGS, &
                        SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, &
                        bi%NA, bi%NTYPE, bi%ILMOS, bi%JLMOS, &
                        BTC, BCAP, DCOEFF, BFCAP, BFCOEFF, BFMIN, BQMAX, &
!FOR PDMROF
                        CMINPDM, CMAXPDM, BPDM, K1PDM, K2PDM, &
                        ZPNDPRECS, ZPONDPREC, ZPONDPREG, ZPNDPREGS, &
                        UM1CS, UM1C, UM1G, UM1GS, &
                        QM1CS, QM1C, QM1G, QM1GS, &
                        QM2CS, QM2C, QM2G, QM2GS, UMQ, &
                        FSTRCS, FSTRC, FSTRG, FSTRGS, &
                        ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
                        HCPSCS, HCPSGS, HCPSC, HCPSG, &
                        TSNOWC, TSNOWG, RHOSC, RHOSG, &
                        XSNOWC, XSNOWG, XSNOCS, XSNOGS)
!
!========================================================================
!          * SINGLE COLUMN BLOWING SNOW CALCULATIONS.
!
            if (PBSMFLAG == 1) then
                call PBSMrun(ZSNOW, WSNOGAT, SNOGAT, RHOSGAT, TSNOGAT, HTCSGAT, &
                             ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
                             HCPSCS, HCPSGS, HCPSC, HCPSG, &
                             TSNOWC, TSNOWG, TSNOCS, TSNOGS, &
                             RHOSC, RHOSG, RHOSCS, RHOSGS,&
                             XSNOWC, XSNOWG, XSNOCS, XSNOGS, &
                             WSNOCS, WSNOGS, &
                             FC, FG, FCS, FGS, &
                             fetchGAT, N_SGAT, A_SGAT, HtGAT, &
                             SFCTGAT, SFCUGAT, SFCQGAT, PRESGAT, PREGAT, &
                             DrySnowGAT, SnowAgeGAT, DriftGAT, SublGAT, &
                             TSNOdsGAT, &
                             bi%NA*bi%NTYPE, il1, il2, N, ZRFMGAT, ZOMLCS, ZOMLNS)
            end if
!========================================================================
!
            call CLASSZ(1, CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, &
                        WTVSTP, WTSSTP, WTGSTP, &
                        FSGVGAT, FLGVGAT, HFSCGAT, HEVCGAT, HMFCGAT, HTCCGAT, &
                        FSGSGAT, FLGSGAT, HFSSGAT, HEVSGAT, HMFNGAT, HTCSGAT, &
                        FSGGGAT, FLGGGAT, HFSGGAT, HEVGGAT, HMFGGAT, HTCGAT, &
                        PCFCGAT, PCLCGAT, QFCFGAT, QFCLGAT, ROFCGAT, WTRCGAT, &
                        PCPNGAT, QFNGAT, ROFNGAT, WTRSGAT, PCPGGAT, QFGGAT, &
                        QFCGAT, ROFGAT, WTRGGAT, CMAIGAT, RCANGAT, SCANGAT, &
                        TCANGAT, SNOGAT, WSNOGAT, TSNOGAT, THLQGAT, THICGAT, &
                        HCPSGAT, THPGAT, DLZWGAT, TBARGAT, ZPNDGAT, TPNDGAT, &
                        sl%DELZ, FCS, FGS, FC, FG, &
                        il1, il2, bi%ILG, bi%IGND, N, &
                        DriftGAT, SublGAT)
!
!=======================================================================
!
!          *Redistribute blowing snow mass between GRUs
!
            call REDISTRIB_SNOW(bi%ILG, 1, bi%NA, bi%NTYPE, bi%NML, TSNOGAT, ZSNOW, &
                                RHOSGAT, SNOGAT, TSNOCS, ZSNOCS, HCPSCS, RHOSCS, TSNOGS, &
                                ZSNOGS, HCPSGS, RHOSGS, TSNOWC, ZSNOWC, HCPSC, RHOSC, TSNOWG, &
                                ZSNOWG, HCPSG, RHOSG, cp%GCGRD, bi%ILMOS, DriftGAT, FAREGAT, &
                                TSNOdsGAT, DistribGAT, WSNOCS, WSNOGS, FCS, FGS, FC, FG, DepositionGAT, &
                                TROOGAT, ROFOGAT, TROFGAT, ROFGAT, ROFNGAT, PCPGGAT, HTCSGAT, WSNOGAT, N)
!
!=======================================================================
            ROFGAT = ROFGAT - UMQ

        end if !(ipid /= 0 .or. izero == 0) then

!> *********************************************************************
!> End of the NML-based LSS loop.
!> *********************************************************************

! *********************************************************************
! Calculate values for output files and print them out
! *********************************************************************

    !> Send/receive process.
        itag = NSUM_TOTAL*1000
        invars = 14 + 4*bi%IGND

    !> Update the variable count per the active control flags.
        if (SAVERESUMEFLAG == 3) invars = invars + 10 + 4

        if (inp > 1 .and. ipid /= 0) then

        !> Send data back to head-node.

            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))
            irqst = mpi_request_null

            i = 1
            call mpi_isend(PREGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(QFSGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(ROFGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(ROFOGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(ROFSGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(ROFBGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(SCANGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(RCANGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(ZPNDGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(SNOGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(FSNOGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(WSNOGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(HFSGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(QEVPGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            do j = 1, bi%IGND
                call mpi_isend(THLQGAT(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(THICGAT(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(GFLXGAT(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(TBARGAT(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            end do

        !> Send optional variables per the active control flags.
            if (SAVERESUMEFLAG == 3) then
                call mpi_isend(ALBSGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(CMAIGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(GROGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(QACGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(RHOSGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(TACGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(TBASGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(TCANGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(TPNDGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(TSNOGAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                do j = 1, 4
                    call mpi_isend(TSFSGAT(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                end do
            end if !(SAVERESUMEFLAG == 3) then

            lstat = .false.
            do while (.not. lstat)
                call mpi_testall(invars, irqst, lstat, imstat, ierr)
            end do

!            print *, ipid, ' done sending'

        else if (inp > 1) then

        !> Receive data from worker nodes.
            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))

        !> Receive and assign variables.
            do u = 1, (inp - 1)

!                print *, 'initiating irecv for:', u, ' with ', itag

                irqst = mpi_request_null
                imstat = 0

                call GetIndices(inp, izero, u, bi%NML, bi%ILMOS, il1, il2, ilen)

                i = 1
                call mpi_irecv(PREGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(QFSGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(ROFGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(ROFOGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(ROFSGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(ROFBGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(SCANGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(RCANGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(ZPNDGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(SNOGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(FSNOGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(WSNOGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(HFSGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(QEVPGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                do j = 1, bi%IGND
                    call mpi_irecv(THLQGAT(il1:il2, j), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(THICGAT(il1:il2, j), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(GFLXGAT(il1:il2, j), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(TBARGAT(il1:il2, j), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                end do

            !> Send optional variables per the active control flags.
                if (SAVERESUMEFLAG == 3) then
                    call mpi_irecv(ALBSGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(CMAIGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(GROGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(QACGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(RHOSGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(TACGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(TBASGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(TCANGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(TPNDGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(TSNOGAT(il1:il2), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    do j = 1, 4
                        call mpi_irecv(TSFSGAT(il1:il2, j), ilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    end do
                end if !(SAVERESUMEFLAG == 3) then

                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(invars, irqst, lstat, imstat, ierr)
                end do

            end do !u = 1, (inp - 1)
!            print *, 'done receiving'

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. NCOUNT == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, ierr)

!> *********************************************************************
!> Start of book-keeping and grid accumulation.
!> *********************************************************************

!
!=======================================================================
!     * WRITE FIELDS FROM CURRENT TIME STEP TO OUTPUT FILES.

        !> Write to CLASSOF* output files.
        do k = 1, WF_NUM_POINTS
            if (op%K_OUT(k) >= il1 .and. op%K_OUT(k) <= il2) then

            !> Update variables.
                i = op%K_OUT(k)
                if (2.0*FSVHGAT(i) > 0.0) then
                    ALTOT = (ALVSGAT(i) + ALIRGAT(i))/2.0
                else
                    ALTOT = 0.0
                end if
                FSSTAR = 2.0*FSVHGAT(i)*(1.0 - ALTOT)
                FLSTAR = FDLGAT(i) - SBC*GTGAT(i)**4
                QH = HFSGAT(i)
                QE = QEVPGAT(i)
                BEG = FSSTAR + FLSTAR - QH - QE
                SNOMLT = HMFNGAT(i)
                if (RHOSGAT(i) > 0.0) then
                    ZSN = SNOGAT(i)/RHOSGAT(i)
                else
                    ZSN = 0.0
                end if
                if (TCANGAT(i) > 0.01) then
                    TCN = TCANGAT(i) - TFREZ
                else
                    TCN = 0.0
                end if
                if (TSNOGAT(i) > 0.01) then
                    TSN = TSNOGAT(i) - TFREZ
                else
                    TSN = 0.0
                end if
                if (TPNDGAT(i) > 0.01) then
                    TPN = TPNDGAT(i) - TFREZ
                else
                    TPN = 0.0
                end if
                if (bi%ILW == 1) then
                    GTOUT = GTGAT(i) - TFREZ
                else
                    GTOUT = 0.0
                end if
                ZPND = ZPNDPRECS(i)*FCS(i) + ZPONDPREC(i)*FC(i) + ZPONDPREG(i)*FG(i) + ZPNDPREGS(i)*FGS(i)
                FSTR = FSTRCS(i)*FCS(i) + FSTRC(i)*FC(i) + FSTRG(i)*FG(i) + FSTRGS(i)*FGS(i)

            !> Write to the CLASSOF* output files for sub-hourly output.
                write(150 + k*10 + 4, &
                      "(i2,',', i3,',', i5,',', i6,',', 9(f8.2,','), 2(f7.3,','), e11.3,',', f8.2,',', 3(f12.4,','))") &
                    HOUR_NOW, MINS_NOW, JDAY_NOW, YEAR_NOW, FSSTAR, FLSTAR, QH, &
                    QE, SNOMLT, BEG, GTOUT, SNOGAT(i), &
                    RHOSGAT(i), WSNOGAT(i), ALTOT, ROFGAT(i), &
                    TPN, ZPNDGAT(i), ZPND, FSTR
                write(150 + k*10 + 5, "(i2,',', i3,',', i5,',', i6,',', " // trim(adjustl(IGND_CHAR)) // &
                      "(f7.2,',', 2(f6.3,',')), f8.2,',', 2(f8.4,','), f8.2,',', f8.3,',')") &
                    HOUR_NOW, MINS_NOW, JDAY_NOW, YEAR_NOW, &
                    (TBARGAT(i, j) - TFREZ, THLQGAT(i, j), &
                    THICGAT(i, j), j = 1, bi%IGND), TCN, &
                    RCANGAT(i), SCANGAT(i), TSN, ZSN
                write(150 + k*10 + 6, &
                      "(i2,',', i3,',', i5,',', 2(f10.2,','), f12.6,',', f10.2,',', f8.2,',', f10.2,',', f15.9,',')") &
                    HOUR_NOW, MINS_NOW, JDAY_NOW, 2.0*FSVHGAT(i), FDLGAT(i), &
                    PREGAT(i), TAGAT(i) - TFREZ, VMODGAT(i), PRESGAT(i), &
                    QAGAT(i)
                write(150 + k*10 + 7, "(999(e11.4,','))") &
                    TROFGAT(i), TROOGAT(i), TROSGAT(i), &
                    TROBGAT(i), ROFGAT(i), ROFOGAT(i), &
                    ROFSGAT(i), ROFBGAT(i), &
                    FCS(i), FGS(i), FC(i), FG(i)
                write(150 + k*10 + 8, "(999(f12.4,','))") &
                    FSGVGAT(i), FSGSGAT(i), FSGGGAT(i), &
                    FLGVGAT(i), FLGSGAT(i), FLGGGAT(i), &
                    HFSCGAT(i), HFSSGAT(i), HFSGGAT(i), &
                    HEVCGAT(i), HEVSGAT(i), HEVGGAT(i), &
                    HMFCGAT(i), HMFNGAT(i), &
                    (HMFGGAT(i, j), j = 1, bi%IGND), &
                    HTCCGAT(i), HTCSGAT(i), &
                    (HTCGAT(i, j), j = 1, bi%IGND)
                write(150 + k*10 + 9, "(999(e12.4,','))") &
                    PCFCGAT(i), PCLCGAT(i), PCPNGAT(i), &
                    PCPGGAT(i), QFCFGAT(i), QFCLGAT(i), &
                    QFNGAT(i), QFGGAT(i), (QFCGAT(i, j), j = 1, bi%IGND), &
                    ROFCGAT(i), ROFNGAT(i), &
                    ROFOGAT(i), ROFGAT(i), WTRCGAT(i), &
                    WTRSGAT(i), WTRGGAT(i)
                write(150 + k*10 + 10, "(i2,',', i3,',', i5,',', i6,',', 999(f14.6,','))") &
                    HOUR_NOW, MINS_NOW, JDAY_NOW, YEAR_NOW, PREGAT(i)*DELT, QFSGAT(i)*DELT, &
                    ROFGAT(i)*DELT, ROFOGAT(i)*DELT, ROFSGAT(i)*DELT, ROFBGAT(i)*DELT, &
                    SCANGAT(i), RCANGAT(i), SNOGAT(i), WSNOGAT(i), &
                    ZPNDGAT(i)*RHOW, (THLQGAT(i, j)*RHOW*DLZWGAT(i, j), j = 1, bi%IGND), &
                    (THICGAT(i, j)*RHOICE*DLZWGAT(i, j), j = 1, bi%IGND)

            !> Calculate accumulated grid variables.
                do i = il1, il2
                    if (bi%ILMOS(i) == op%N_OUT(k)) then
                        co%PREACC(k) = co%PREACC(k) + PREGAT(i)*FAREGAT(i)*DELT
                        co%GTACC(k) = co%GTACC(k) + GTGAT(i)*FAREGAT(i)
                        co%QEVPACC(k) = co%QEVPACC(k) + QEVPGAT(i)*FAREGAT(i)
                        co%EVAPACC(k) = co%EVAPACC(k) + QFSGAT(i)*FAREGAT(i)*DELT
                        co%HFSACC(k) = co%HFSACC(k) + HFSGAT(i)*FAREGAT(i)
                        co%HMFNACC(k) = co%HMFNACC(k) + HMFNGAT(i)*FAREGAT(i)
                        co%ROFACC(k) = co%ROFACC(k) + ROFGAT(i)*FAREGAT(i)*DELT
                        co%ROFOACC(k) = co%ROFOACC(k) + ROFOGAT(i)*FAREGAT(i)*DELT
                        co%ROFSACC(k) = co%ROFSACC(k) + ROFSGAT(i)*FAREGAT(i)*DELT
                        co%ROFBACC(k) = co%ROFBACC(k) + ROFBGAT(i)*FAREGAT(i)*DELT
                        co%WTBLACC(k) = co%WTBLACC(k) + WTABGAT(i)*FAREGAT(i)
                        do j = 1, bi%IGND
                            co%TBARACC(k, j) = co%TBARACC(k, j) + TBARGAT(i, j)*bi%ACLASS(bi%ILMOS(i), bi%JLMOS(i))
                            co%THLQACC(k, j) = co%THLQACC(k, j) + THLQGAT(i, j)*FAREGAT(i)
                            co%THICACC(k, j) = co%THICACC(k, j) + THICGAT(i, j)*FAREGAT(i)
                            co%THALACC(k, j) = co%THALACC(k, j) + (THLQGAT(i, j) + THICGAT(i, j))*FAREGAT(i)
                            co%GFLXACC(k, j) = co%GFLXACC(k, j) + GFLXGAT(i, j)*FAREGAT(i)
                        end do
                        co%ALVSACC(k) = co%ALVSACC(k) + ALVSGAT(i)*FSVHGAT(i)*FAREGAT(i)
                        co%ALIRACC(k) = co%ALIRACC(k) + ALIRGAT(i)*FSIHGAT(i)*FAREGAT(i)
                        if (SNOGAT(i) > 0.0) then
                            co%RHOSACC(k) = co%RHOSACC(k) + RHOSGAT(i)*FAREGAT(i)
                            co%TSNOACC(k) = co%TSNOACC(k) + TSNOGAT(i)*FAREGAT(i)
                            co%WSNOACC(k) = co%WSNOACC(k) + WSNOGAT(i)*FAREGAT(i)
                            co%SNOARE(k) = co%SNOARE(k) + FAREGAT(i)
                        end if
                        if (TCANGAT(i) > 0.5) then
                            co%TCANACC(k) = co%TCANACC(k) + TCANGAT(i)*FAREGAT(i)
                            co%CANARE(k) = co%CANARE(k) + FAREGAT(i)
                        end if
                        co%SNOACC(k) = co%SNOACC(k) + SNOGAT(i)*FAREGAT(i)
                        co%RCANACC(k) = co%RCANACC(k) + RCANGAT(i)*FAREGAT(i)
                        co%SCANACC(k) = co%SCANACC(k) + SCANGAT(i)*FAREGAT(i)
                        co%GROACC(k) = co%GROACC(k) + GROGAT(i)*FAREGAT(i)
                        co%FSINACC(k) = co%FSINACC(k) + 2.0*FSVHGAT(i)*FAREGAT(i)
                        co%FLINACC(k) = co%FLINACC(k) + FDLGAT(i)*FAREGAT(i)
                        co%FLUTACC(k) = co%FLUTACC(k) + SBC*GTGAT(i)**4*FAREGAT(i)
                        co%TAACC(k) = co%TAACC(k) + TAGAT(i)*FAREGAT(i)
                        co%UVACC(k) = co%UVACC(k) + VMODGAT(i)*FAREGAT(i)
                        co%PRESACC(k) = co%PRESACC(k) + PRESGAT(i)*FAREGAT(i)
                        co%QAACC(k) = co%QAACC(k) + QAGAT(i)*FAREGAT(i)
                    end if
                end do

            !> Write to the CLASSOF* output files for daily output.
                if (NCOUNT == 48) then

                !> Calculate grid averages.
                    co%GTACC(k) = co%GTACC(k)/real(NSUM)
                    co%QEVPACC(k) = co%QEVPACC(k)/real(NSUM)
                    co%HFSACC(k) = co%HFSACC(k)/real(NSUM)
                    co%HMFNACC(k) = co%HMFNACC(k)/real(NSUM)
                    co%WTBLACC(k) = co%WTBLACC(k)/real(NSUM)
                    co%TBARACC(k, :) = co%TBARACC(k, :)/real(NSUM)
                    co%THLQACC(k, :) = co%THLQACC(k, :)/real(NSUM)
                    co%THICACC(k, :) = co%THICACC(k, :)/real(NSUM)
                    co%THALACC(k, :) = co%THALACC(k, :)/real(NSUM)
                    if (co%FSINACC(k) > 0.0) then
                        co%ALVSACC(k) = co%ALVSACC(k)/(co%FSINACC(k)*0.5)
                        co%ALIRACC(k) = co%ALIRACC(k)/(co%FSINACC(k)*0.5)
                    else
                        co%ALVSACC(k) = 0.0
                        co%ALIRACC(k) = 0.0
                    end if
                    if (co%SNOARE(k) > 0.0) then
                        co%RHOSACC(k) = co%RHOSACC(k)/co%SNOARE(k)
                        co%TSNOACC(k) = co%TSNOACC(k)/co%SNOARE(k)
                        co%WSNOACC(k) = co%WSNOACC(k)/co%SNOARE(k)
                    end if
                    if (co%CANARE(k) > 0.0) then
                        co%TCANACC(k) = co%TCANACC(k)/co%CANARE(k)
                    end if
                    co%SNOACC(k) = co%SNOACC(k)/real(NSUM)
                    co%RCANACC(k) = co%RCANACC(k)/real(NSUM)
                    co%SCANACC(k) = co%SCANACC(k)/real(NSUM)
                    co%GROACC(k) = co%GROACC(k)/real(NSUM)
                    co%FSINACC(k) = co%FSINACC(k)/real(NSUM)
                    co%FLINACC(k) = co%FLINACC(k)/real(NSUM)
                    co%FLUTACC(k) = co%FLUTACC(k)/real(NSUM)
                    co%TAACC(k) = co%TAACC(k)/real(NSUM)
                    co%UVACC(k) = co%UVACC(k)/real(NSUM)
                    co%PRESACC(k) = co%PRESACC(k)/real(NSUM)
                    co%QAACC(k) = co%QAACC(k)/real(NSUM)
                    ALTOT = (co%ALVSACC(k) + co%ALIRACC(k))/2.0
                    FSSTAR = co%FSINACC(k)*(1.0 - ALTOT)
                    FLSTAR = co%FLINACC(k) - co%FLUTACC(k)
                    QH = co%HFSACC(k)
                    QE = co%QEVPACC(k)
                    BEG = FSSTAR + FLSTAR - QH - QE
                    SNOMLT = co%HMFNACC(k)
                    if (co%RHOSACC(k) > 0.0) then
                        ZSN = co%SNOACC(k)/co%RHOSACC(k)
                    else
                        ZSN = 0.0
                    end if
                    if (co%TCANACC(k) > 0.01) then
                        TCN = co%TCANACC(k) - TFREZ
                    else
                        TCN = 0.0
                    end if
                    if (co%TSNOACC(k) > 0.01) then
                        TSN = co%TSNOACC(k) - TFREZ
                    else
                        TSN = 0.0
                    end if
                    if (bi%ILW == 1) then
                        GTOUT = co%GTACC(k) - TFREZ
                    else
                        GTOUT = 0.0
                    end if

                !> Write to the CLASSOF* output files for daily accumulated output.
                    write(150 + k*10 + 1, "(i4,',', i5,',', 9(f8.2,','), 2(f8.3,','), 999(f12.4,','))") &
                        JDAY_NOW, YEAR_NOW, FSSTAR, FLSTAR, QH, QE, SNOMLT, &
                        BEG, GTOUT, co%SNOACC(k), co%RHOSACC(k), &
                        co%WSNOACC(k), ALTOT, co%ROFACC(k), co%ROFOACC(k), &
                        co%ROFSACC(k), co%ROFBACC(k)
                    write(150 + k*10 + 2, "(i4,',', i5,',', " // adjustl(IGND_CHAR) // "((f8.2,','), " // &
                          "2(f6.3,',')), f8.2,',', 2(f7.4,','), 2(f8.2,','))") &
                        JDAY_NOW, YEAR_NOW, (co%TBARACC(k, j) - TFREZ, &
                        co%THLQACC(k, j), co%THICACC(k, j), j = 1, bi%IGND), &
                        TCN, co%RCANACC(k), co%SCANACC(k), TSN, ZSN
                    write(150 + k*10 + 3, "(i4,',', i5,',', 3(f9.2,','), f8.2,',', " // &
                          "f10.2,',', e12.3,',', 2(f12.3,','))") &
                        JDAY_NOW, YEAR_NOW, co%FSINACC(k), co%FLINACC(k), &
                        co%TAACC(k) - TFREZ, co%UVACC(k), co%PRESACC(k), &
                        co%QAACC(k), co%PREACC(k), co%EVAPACC(k)

                !> Reset the CLASS output variables.
                    co%PREACC = 0.0
                    co%GTACC = 0.0
                    co%QEVPACC = 0.0
                    co%EVAPACC = 0.0
                    co%HFSACC = 0.0
                    co%HMFNACC = 0.0
                    co%ROFACC = 0.0
                    co%ROFOACC = 0.0
                    co%ROFSACC = 0.0
                    co%ROFBACC = 0.0
                    co%WTBLACC = 0.0
                    co%TBARACC = 0.0
                    co%THLQACC = 0.0
                    co%THICACC = 0.0
                    co%THALACC = 0.0
                    co%GFLXACC = 0.0
                    co%ALVSACC = 0.0
                    co%ALIRACC = 0.0
                    co%RHOSACC = 0.0
                    co%TSNOACC = 0.0
                    co%WSNOACC = 0.0
                    co%SNOARE = 0.0
                    co%TCANACC = 0.0
                    co%CANARE = 0.0
                    co%SNOACC = 0.0
                    co%RCANACC = 0.0
                    co%SCANACC = 0.0
                    co%GROACC = 0.0
                    co%FSINACC = 0.0
                    co%FLINACC = 0.0
                    co%FLUTACC = 0.0
                    co%TAACC = 0.0
                    co%UVACC = 0.0
                    co%PRESACC = 0.0
                    co%QAACC = 0.0
                end if !(NCOUNT == 48) then
            end if !(op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2) then
        end do !i = 1, WF_NUM_POINTS

        if (ipid == 0) then

!> Write ENSIM output
!> -----------------------------------------------------c
!>
            if (NR2CFILES > 0 .and. mod(NCOUNT*30, DELTR2C) == 0) then
                call FIND_MONTH (JDAY_NOW, YEAR_NOW, ensim_month)
                call FIND_DAY (JDAY_NOW, YEAR_NOW, ensim_day)
                call WRITE_R2C_DATA(bi%NML, NLTEST, NMTEST, NCOUNT, MINS_NOW, bi%ACLASS, &
                                    bi%NA, bi%xxx, bi%yyy, bi%xCount, bi%yCount, bi%ILMOS, bi%JLMOS, bi%ILG, &
                                    NR2C, NR2CFILES, R2CFILEUNITSTART, GRD, GAT, &
                                    GRDGAT, NR2CSTATES, R2C_ATTRIBUTES, FRAME_NO_NEW, YEAR_NOW, &
                                    ensim_MONTH, ensim_DAY, HOUR_NOW, MINS_NOW, ICAN, &
                                    ICAN + 1, bi%IGND, &
                                    TBARGAT, THLQGAT, THICGAT, TPNDGAT, ZPNDGAT, &
                                    TBASGAT, ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, &
                                    TCANGAT, RCANGAT, SCANGAT, GROGAT, CMAIGAT, &
                                    FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT, PAMXGAT, &
                                    PAMNGAT, CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, &
                                    VPDAGAT, VPDBGAT, PSGAGAT, PSGBGAT, PAIDGAT, &
                                    HGTDGAT, ACVDGAT, ACIDGAT, TSFSGAT, WSNOGAT, &
                                    THPGAT, THRGAT, THMGAT, BIGAT, PSISGAT, &
                                    GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, &
                                    THFCGAT, PSIWGAT, DLZWGAT, ZBTWGAT, &
                                    ZSNLGAT, ZPLGGAT, ZPLSGAT, TACGAT, QACGAT, &
                                    DRNGAT, XSLPGAT, XDGAT, WFSFGAT, KSGAT, &
                                    ALGWGAT, ALGDGAT, ASVDGAT, ASIDGAT, AGVDGAT, &
                                    AGIDGAT, ISNDGAT, RADJGAT, ZBLDGAT, Z0ORGAT, &
                                    ZRFMGAT, ZRFHGAT, ZDMGAT, ZDHGAT, FSVHGAT, &
                                    FSIHGAT, CSZGAT, FDLGAT, ULGAT, VLGAT, &
                                    TAGAT, QAGAT, PRESGAT, PREGAT, PADRGAT, &
                                    VPDGAT, TADPGAT, RHOAGAT, RPCPGAT, TRPCGAT, &
                                    SPCPGAT, TSPCGAT, RHSIGAT, FCLOGAT, DLONGAT, &
                                    GGEOGAT, &
                                    CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT, &
                                    QFSGAT, QFXGAT, PETGAT, GAGAT, EFGAT, &
                                    GTGAT, QGGAT, ALVSGAT, ALIRGAT, &
                                    SFCTGAT, SFCUGAT, SFCVGAT, SFCQGAT, FSNOGAT, &
                                    FSGVGAT, FSGSGAT, FSGGGAT, FLGVGAT, FLGSGAT, &
                                    FLGGGAT, HFSCGAT, HFSSGAT, HFSGGAT, HEVCGAT, &
                                    HEVSGAT, HEVGGAT, HMFCGAT, HMFNGAT, HTCCGAT, &
                                    HTCSGAT, PCFCGAT, PCLCGAT, PCPNGAT, PCPGGAT, &
                                    QFGGAT, QFNGAT, QFCLGAT, QFCFGAT, ROFGAT, &
                                    ROFOGAT, ROFSGAT, ROFBGAT, TROFGAT, TROOGAT, &
                                    TROSGAT, TROBGAT, ROFCGAT, ROFNGAT, ROVGGAT, &
                                    WTRCGAT, WTRSGAT, WTRGGAT, DRGAT, GFLXGAT, &
                                    HMFGGAT, HTCGAT, QFCGAT, MANNGAT, DDGAT, &
                                    IGDRGAT, VMODGAT, QLWOGAT)
                FRAME_NO_NEW = FRAME_NO_NEW + 1 !UPDATE COUNTERS
            end if

!> =======================================================================
!>     * CALCULATE GRID CELL AVERAGE DIAGNOSTIC FIELDS.

!> many of these varibles are currently not being used for anything,
!> but we want to keep them because they may be useful in the future.
!> these variables hold the grid cell averages. 
!> In the future, someone will need to use them.

            CDHGRD = 0.0
            CDMGRD = 0.0
            HFSGRD = 0.0
            TFXGRD = 0.0
            QEVPGRD = 0.0
            QFSGRD = 0.0
            QFXGRD = 0.0
            PETGRD = 0.0
            GAGRD = 0.0
            EFGRD = 0.0
            GTGRD = 0.0
            QGGRD = 0.0
            TSFGRD = 0.0
            ALVSGRD = 0.0
            ALIRGRD = 0.0
            SFCTGRD = 0.0
            SFCUGRD = 0.0
            SFCVGRD = 0.0
            SFCQGRD = 0.0
            FSNOGRD = 0.0
            FSGVGRD = 0.0
            FSGSGRD = 0.0
            FSGGGRD = 0.0
            SNOGRD = 0.0
            FLGVGRD = 0.0
            FLGSGRD = 0.0
            FLGGGRD = 0.0
            HFSCGRD = 0.0
            HFSSGRD = 0.0
            HFSGGRD = 0.0
            HEVCGRD = 0.0
            HEVSGRD = 0.0
            HEVGGRD = 0.0
            HMFCGRD = 0.0
            HMFNGRD = 0.0
            HTCCGRD = 0.0
            HTCSGRD = 0.0
            PCFCGRD = 0.0
            PCLCGRD = 0.0
            PCPNGRD = 0.0
            PCPGGRD = 0.0
            QFGGRD = 0.0
            QFNGRD = 0.0
            QFCLGRD = 0.0
            QFCFGRD = 0.0
            ROFGRD = 0.0
            ROFOGRD = 0.0
            ROFSGRD = 0.0
            ROFBGRD = 0.0
            ROFCGRD = 0.0
            ROFNGRD = 0.0
            ROVGGRD = 0.0
            WTRCGRD = 0.0
            WTRSGRD = 0.0
            WTRGGRD = 0.0
            DRGRD = 0.0
            WTABGRD = 0.0
            ILMOGRD = 0.0
            UEGRD = 0.0
            HBLGRD = 0.0
            HMFGGRD = 0.0
            HTCGRD = 0.0
            QFCGRD = 0.0
            GFLXGRD = 0.0
!>
!>*******************************************************************
!>

    !> Grid data for output.
            md%fsdown = cm%clin(cfk%FB)%climvGrd
            md%fsvh = fsvhgrd
            md%fsih = fsihgrd
            md%fdl = cm%clin(cfk%FI)%climvGrd
            md%ul = cm%clin(cfk%UV)%climvGrd
            md%ta = cm%clin(cfk%TT)%climvGrd
            md%qa = cm%clin(cfk%HU)%climvGrd
            md%pres = cm%clin(cfk%P0)%climvGrd
            md%pre = cm%clin(cfk%PR)%climvGrd

!> GRU-distributed data for output.
            wb_h%pre = 0.0
            wb_h%evap = 0.0
            wb_h%rof = 0.0
            wb_h%rofo = 0.0
            wb_h%rofs = 0.0
            wb_h%rofb = 0.0
            wb_h%rcan = 0.0
            wb_h%sncan = 0.0
            wb_h%pndw = 0.0
            wb_h%sno = 0.0
            wb_h%wsno = 0.0
            wb_h%lqws = 0.0
            wb_h%frws = 0.0

            !$omp parallel do
            do i = 1, bi%NML
                CDHGRD(bi%ILMOS(i)) = CDHGRD(bi%ILMOS(i)) + CDHGAT(i)*FAREGAT(i)
                CDMGRD(bi%ILMOS(i)) = CDMGRD(bi%ILMOS(i)) + CDMGAT(i)*FAREGAT(i)
                HFSGRD(bi%ILMOS(i)) = HFSGRD(bi%ILMOS(i)) + HFSGAT(i)*FAREGAT(i)
                TFXGRD(bi%ILMOS(i)) = TFXGRD(bi%ILMOS(i)) + TFXGAT(i)*FAREGAT(i)
                QEVPGRD(bi%ILMOS(i)) = QEVPGRD(bi%ILMOS(i)) + QEVPGAT(i)*FAREGAT(i)
                QFSGRD(bi%ILMOS(i)) = QFSGRD(bi%ILMOS(i)) + QFSGAT(i)*FAREGAT(i)
                QFXGRD(bi%ILMOS(i)) = QFXGRD(bi%ILMOS(i)) + QFXGAT(i)*FAREGAT(i)
                PETGRD(bi%ILMOS(i)) = PETGRD(bi%ILMOS(i)) + PETGAT(i)*FAREGAT(i)
                GAGRD(bi%ILMOS(i)) = GAGRD(bi%ILMOS(i)) + GAGAT(i)*FAREGAT(i)
                EFGRD(bi%ILMOS(i)) = EFGRD(bi%ILMOS(i)) + EFGAT(i)*FAREGAT(i)
                GTGRD(bi%ILMOS(i)) = GTGRD(bi%ILMOS(i)) + GTGAT(i)*FAREGAT(i)
                QGGRD(bi%ILMOS(i)) = QGGRD(bi%ILMOS(i)) + QGGAT(i)*FAREGAT(i)
!                TSFGRD(bi%ILMOS(i)) = TSFGRD(bi%ILMOS(i)) + TSFGAT(i)*FAREGAT(i)
                ALVSGRD(bi%ILMOS(i)) = ALVSGRD(bi%ILMOS(i)) + ALVSGAT(i)*FAREGAT(i)
                ALIRGRD(bi%ILMOS(i)) = ALIRGRD(bi%ILMOS(i)) + ALIRGAT(i)*FAREGAT(i)
                SFCTGRD(bi%ILMOS(i)) = SFCTGRD(bi%ILMOS(i)) + SFCTGAT(i)*FAREGAT(i)
                SFCUGRD(bi%ILMOS(i)) = SFCUGRD(bi%ILMOS(i)) + SFCUGAT(i)*FAREGAT(i)
                SFCVGRD(bi%ILMOS(i)) = SFCVGRD(bi%ILMOS(i)) + SFCVGAT(i)*FAREGAT(i)
                SFCQGRD(bi%ILMOS(i)) = SFCQGRD(bi%ILMOS(i)) + SFCQGAT(i)*FAREGAT(i)
                FSNOGRD(bi%ILMOS(i)) = FSNOGRD(bi%ILMOS(i)) + FSNOGAT(i)*FAREGAT(i)
                FSGVGRD(bi%ILMOS(i)) = FSGVGRD(bi%ILMOS(i)) + FSGVGAT(i)*FAREGAT(i)
                FSGSGRD(bi%ILMOS(i)) = FSGSGRD(bi%ILMOS(i)) + FSGSGAT(i)*FAREGAT(i)
                FSGGGRD(bi%ILMOS(i)) = FSGGGRD(bi%ILMOS(i)) + FSGGGAT(i)*FAREGAT(i)
                SNOGRD(bi%ILMOS(i)) = SNOGRD(bi%ILMOS(i)) + SNOGAT(i)*FAREGAT(i)
                FLGVGRD(bi%ILMOS(i)) = FLGVGRD(bi%ILMOS(i)) + FLGVGAT(i)*FAREGAT(i)
                FLGSGRD(bi%ILMOS(i)) = FLGSGRD(bi%ILMOS(i)) + FLGSGAT(i)*FAREGAT(i)
                FLGGGRD(bi%ILMOS(i)) = FLGGGRD(bi%ILMOS(i)) + FLGGGAT(i)*FAREGAT(i)
                HFSCGRD(bi%ILMOS(i)) = HFSCGRD(bi%ILMOS(i)) + HFSCGAT(i)*FAREGAT(i)
                HFSSGRD(bi%ILMOS(i)) = HFSSGRD(bi%ILMOS(i)) + HFSSGAT(i)*FAREGAT(i)
                HFSGGRD(bi%ILMOS(i)) = HFSGGRD(bi%ILMOS(i)) + HFSGGAT(i)*FAREGAT(i)
                HEVCGRD(bi%ILMOS(i)) = HEVCGRD(bi%ILMOS(i)) + HEVCGAT(i)*FAREGAT(i)
                HEVSGRD(bi%ILMOS(i)) = HEVSGRD(bi%ILMOS(i)) + HEVSGAT(i)*FAREGAT(i)
                HEVGGRD(bi%ILMOS(i)) = HEVGGRD(bi%ILMOS(i)) + HEVGGAT(i)*FAREGAT(i)
                HMFCGRD(bi%ILMOS(i)) = HMFCGRD(bi%ILMOS(i)) + HMFCGAT(i)*FAREGAT(i)
                HMFNGRD(bi%ILMOS(i)) = HMFNGRD(bi%ILMOS(i)) + HMFNGAT(i)*FAREGAT(i)
                HTCCGRD(bi%ILMOS(i)) = HTCCGRD(bi%ILMOS(i)) + HTCCGAT(i)*FAREGAT(i)
                HTCSGRD(bi%ILMOS(i)) = HTCSGRD(bi%ILMOS(i)) + HTCSGAT(i)*FAREGAT(i)
                PCFCGRD(bi%ILMOS(i)) = PCFCGRD(bi%ILMOS(i)) + PCFCGAT(i)*FAREGAT(i)
                PCLCGRD(bi%ILMOS(i)) = PCLCGRD(bi%ILMOS(i)) + PCLCGAT(i)*FAREGAT(i)
                PCPNGRD(bi%ILMOS(i)) = PCPNGRD(bi%ILMOS(i)) + PCPNGAT(i)*FAREGAT(i)
                PCPGGRD(bi%ILMOS(i)) = PCPGGRD(bi%ILMOS(i)) + PCPGGAT(i)*FAREGAT(i)
                QFGGRD(bi%ILMOS(i)) = QFGGRD(bi%ILMOS(i)) + QFGGAT(i)*FAREGAT(i)
                QFNGRD(bi%ILMOS(i)) = QFNGRD(bi%ILMOS(i)) + QFNGAT(i)*FAREGAT(i)
                QFCLGRD(bi%ILMOS(i)) = QFCLGRD(bi%ILMOS(i)) + QFCLGAT(i)*FAREGAT(i)
                QFCFGRD(bi%ILMOS(i)) = QFCFGRD(bi%ILMOS(i)) + QFCFGAT(i)*FAREGAT(i)
                ROFGRD(bi%ILMOS(i)) = ROFGRD(bi%ILMOS(i)) + ROFGAT(i)*FAREGAT(i)
                ROFOGRD(bi%ILMOS(i)) = ROFOGRD(bi%ILMOS(i)) + ROFOGAT(i)*FAREGAT(i)
                ROFSGRD(bi%ILMOS(i)) = ROFSGRD(bi%ILMOS(i)) + ROFSGAT(i)*FAREGAT(i)
                ROFBGRD(bi%ILMOS(i)) = ROFBGRD(bi%ILMOS(i)) + ROFBGAT(i)*FAREGAT(i)
                ROFCGRD(bi%ILMOS(i)) = ROFCGRD(bi%ILMOS(i)) + ROFCGAT(i)*FAREGAT(i)
                ROFNGRD(bi%ILMOS(i)) = ROFNGRD(bi%ILMOS(i)) + ROFNGAT(i)*FAREGAT(i)
                ROVGGRD(bi%ILMOS(i)) = ROVGGRD(bi%ILMOS(i)) + ROVGGAT(i)*FAREGAT(i)
                WTRCGRD(bi%ILMOS(i)) = WTRCGRD(bi%ILMOS(i)) + WTRCGAT(i)*FAREGAT(i)
                WTRSGRD(bi%ILMOS(i)) = WTRSGRD(bi%ILMOS(i)) + WTRSGAT(i)*FAREGAT(i)
                WTRGGRD(bi%ILMOS(i)) = WTRGGRD(bi%ILMOS(i)) + WTRGGAT(i)*FAREGAT(i)
                DRGRD(bi%ILMOS(i)) = DRGRD(bi%ILMOS(i)) + DRGAT(i)*FAREGAT(i)
                WTABGRD(bi%ILMOS(i)) = WTABGRD(bi%ILMOS(i)) + WTABGAT(i)*FAREGAT(i)
                ILMOGRD(bi%ILMOS(i)) = ILMOGRD(bi%ILMOS(i)) + ILMOGAT(i)*FAREGAT(i)
                UEGRD(bi%ILMOS(i)) = UEGRD(bi%ILMOS(i)) + UEGAT(i)*FAREGAT(i)
                HBLGRD(bi%ILMOS(i)) = HBLGRD(bi%ILMOS(i)) + HBLGAT(i)*FAREGAT(i)
                wb_h%pre(bi%ILMOS(i)) = wb_h%pre(bi%ILMOS(i)) + FAREGAT(i)*PREGAT(i)*DELT
                wb_h%evap(bi%ILMOS(i)) = wb_h%evap(bi%ILMOS(i)) + FAREGAT(i)*QFSGAT(i)*DELT
                wb_h%rof(bi%ILMOS(i)) = wb_h%rof(bi%ILMOS(i)) + FAREGAT(i)*ROFGAT(i)*DELT
                wb_h%rofo(bi%ILMOS(i)) = wb_h%rofo(bi%ILMOS(i)) + FAREGAT(i)*ROFOGAT(i)*DELT
                wb_h%rofs(bi%ILMOS(i)) = wb_h%rofs(bi%ILMOS(i)) + FAREGAT(i)*ROFSGAT(i)*DELT
                wb_h%rofb(bi%ILMOS(i)) = wb_h%rofb(bi%ILMOS(i)) + FAREGAT(i)*ROFBGAT(i)*DELT
                wb_h%rcan(bi%ILMOS(i)) = wb_h%rcan(bi%ILMOS(i)) + FAREGAT(i)*RCANGAT(i)
                            wb_h%sncan(bi%ILMOS(i)) = wb_h%sncan(bi%ILMOS(i)) + FAREGAT(i)*SCANGAT(i)
                wb_h%pndw(bi%ILMOS(i)) = wb_h%pndw(bi%ILMOS(i)) + FAREGAT(i)*ZPNDGAT(i)*RHOW
                wb_h%sno(bi%ILMOS(i)) = wb_h%sno(bi%ILMOS(i)) + FAREGAT(i)*SNOGAT(i)
                wb_h%wsno(bi%ILMOS(i)) = wb_h%wsno(bi%ILMOS(i)) + FAREGAT(i)*WSNOGAT(i)
                do j = 1, bi%IGND
                    HMFGGRD(bi%ILMOS(i), j) = HMFGGRD(bi%ILMOS(i), j) + HMFGGAT(i, j)*FAREGAT(i)
                    HTCGRD(bi%ILMOS(i), j) = HTCGRD(bi%ILMOS(i), j) + HTCGAT(i, j)*FAREGAT(i)
                    QFCGRD(bi%ILMOS(i), j) = QFCGRD(bi%ILMOS(i), j) + QFCGAT(i, j)*FAREGAT(i)
                    GFLXGRD(bi%ILMOS(i), j) = GFLXGRD(bi%ILMOS(i), j) + GFLXGAT(i, j)*FAREGAT(i)
                    wb_h%lqws(bi%ILMOS(i), j) = wb_h%lqws(bi%ILMOS(i), j) + FAREGAT(i)*THLQGAT(i, j)*DLZWGAT(i, j)*RHOW
                    wb_h%frws(bi%ILMOS(i), j) = wb_h%frws(bi%ILMOS(i), j) + FAREGAT(i)*THICGAT(i, j)*DLZWGAT(i, j)*RHOICE
                end do
                wb_h%stg(bi%ILMOS(i)) = wb%rcan(bi%ILMOS(i)) + wb%sncan(bi%ILMOS(i)) + wb%pndw(bi%ILMOS(i)) + &
                    wb%sno(bi%ILMOS(i)) + wb%wsno(bi%ILMOS(i)) + &
                    sum(wb%lqws(bi%ILMOS(i), :)) + sum(wb%frws(bi%ILMOS(i), :))
            end do !i = 1, bi%NML

!>
!> *******************************************************************
!> FOR SPL WATROUTE (MODIFIED RPN CODE)
!> *******************************************************************
!>
            call tile_connector(bi, rte_runoff, rte_recharge, rte_leakage, NCOUNT, ROFOGRD, ROFSGRD, ROFBGRD, DELT)

!> FILES ARE ONLY WRITTEN ON THE HOUR (WATROUTE READS HOURLY DATA).
!> HOURLY TIME STEPS ARE ODD-NUMBERED INTEGERS.
!>
!todo: these can be removed at some point, as they've been added
!todo: as flags as a part of the model_output module.
            if (mod(real(NCOUNT), 2.0) == 0.0) then !HOURLY TIME STEP
                rte_year_now = YEAR_NOW
                call FIND_MONTH(JDAY_NOW, YEAR_NOW, rte_month_now)
                call FIND_DAY(JDAY_NOW, YEAR_NOW, rte_day_now)
                rte_hour_now = HOUR_NOW + 1 !ROUTING USES 1-24 RANGE, MESH USES 0-23
!>
!> WRITE OUTPUT FOR RTE.EXE (RUNOFF)
!>
                if (PRINTRFFR2CFILEFLAG == 1) then
                    !PASS RUNOFF TO WRITE_R2C
                    call write_r2c(fls, mfk%f31, bi, &
                                   rte_frames_total, 1, rte_frames_now, 1, 6, &
                                   rte_year_now, rte_month_now, rte_day_now, rte_hour_now, &
                                   rte_runoff)
                end if
!>
!> WRITE OUTPUT FOR RTE.EXE (RECHARGE)
!>
                if (PRINTRCHR2CFILEFLAG == 1) then !WRITE RECHARGE DATA
                    !PASS RECHARGE TO WRITE_R2C
                    call write_r2c(fls, mfk%f32, bi, &
                                   rte_frames_total, 1, rte_frames_now, 1, 6, &
                                   rte_year_now, rte_month_now, rte_day_now, rte_hour_now, &
                                   rte_recharge)
                end if
!>
!> UPDATE COUNTERS
!>
                rte_frames_now = rte_frames_now + 1
                rte_frames_total = rte_frames_total + 1
            end if !(mod(real(NCOUNT), 2.0) == 0.0) then

!> calculate and write the basin avg SCA similar to watclass3.0f5
!> Same code than in wf_ensim.f subrutine of watclass3.0f8
!> Especially for version MESH_Prototype 3.3.1.7b (not to be incorporated in future versions)
!> calculate and write the basin avg SWE using the similar fudge factor!!!

!            if (BASIN_FRACTION(1) == -1) then
!                do i = 1, bi%NA ! NA = number of grid squares
!>         BASIN_FRACTION is the basin snow cover
!>         (portions of the grids outside the basin are not included)
!>         for a given day - JDAY_NOW in the if statement
!                    BASIN_FRACTION(i) = bi%FRAC(i)
    !TODO: FRAC is not actually the fraction of the grid square
    !within the basin, we should be using some other value, but I'm
    !not sure what.
    !todo: calculate frac and write document to send to someone else.
!                end do
!            end if

            if (HOUR_NOW == 12 .and. MINS_NOW == 0) then
                basin_SCA = 0.0
                basin_SWE = 0.0
!                do i = 1, bi%NA
!                    if (BASIN_FRACTION(i) /= 0.0) then
!                        basin_SCA = basin_SCA + FSNOGRD(i)/BASIN_FRACTION(i)
!                        basin_SWE = basin_SWE + SNOGRD(i)/BASIN_FRACTION(i)
!                    end if
!                end do
!                basin_SCA = basin_SCA/NA
!                basin_SWE = basin_SWE/NA

! BRUCE DAVISON - AUG 17, 2009 (see notes in my notebook for this day)
! Fixed calculation of basin averages. Needs documenting and testing.
                do i = 1, bi%NML
                    basin_SCA = basin_SCA + FSNOGAT(i)*FAREGAT(i)
                    basin_SWE = basin_SWE + SNOGAT(i)*FAREGAT(i)
                end do
                basin_SCA = basin_SCA/TOTAL_AREA
                basin_SWE = basin_SWE/TOTAL_AREA
                if (BASINSWEOUTFLAG > 0) then
                    write(85, "(i5,',', f10.3)") JDAY_NOW, basin_SCA
                    write(86, "(i5,',', f10.3)") JDAY_NOW, basin_SWE
                end if
            end if

!> =======================================================================
!> ACCUMULATE OUTPUT DATA FOR DIURNALLY AVERAGED FIELDS.

            !$omp parallel do
            do i = 1, bi%NML
                if (bi%FRAC(bi%ILMOS(i)) /= 0.0) then
                    PREACC(bi%ILMOS(i)) = PREACC(bi%ILMOS(i)) + PREGAT(i)*FAREGAT(i)*DELT
                    GTACC(bi%ILMOS(i)) = GTACC(bi%ILMOS(i)) + GTGAT(i)*FAREGAT(i)
                    QEVPACC(bi%ILMOS(i)) = QEVPACC(bi%ILMOS(i)) + QEVPGAT(i)*FAREGAT(i)
                    EVAPACC(bi%ILMOS(i)) = EVAPACC(bi%ILMOS(i)) + QFSGAT(i)*FAREGAT(i)*DELT
                    HFSACC(bi%ILMOS(i))  = HFSACC(bi%ILMOS(i)) + HFSGAT(i)*FAREGAT(i)
                    HMFNACC(bi%ILMOS(i)) = HMFNACC(bi%ILMOS(i)) + HMFNGAT(i)*FAREGAT(i)
                    ROFACC(bi%ILMOS(i)) = ROFACC(bi%ILMOS(i)) + ROFGAT(i)*FAREGAT(i)*DELT
                    ROFOACC(bi%ILMOS(i)) = ROFOACC(bi%ILMOS(i)) + ROFOGAT(i)*FAREGAT(i)*DELT
                    ROFSACC(bi%ILMOS(i)) = ROFSACC(bi%ILMOS(i)) + ROFSGAT(i)*FAREGAT(i)*DELT
                    ROFBACC(bi%ILMOS(i)) = ROFBACC(bi%ILMOS(i)) + ROFBGAT(i)*FAREGAT(i)*DELT
                    WTBLACC(bi%ILMOS(i)) = WTBLACC(bi%ILMOS(i)) + WTABGAT(i)*FAREGAT(i)
                    do j = 1, bi%IGND
                        TBARACC(bi%ILMOS(i), j) = TBARACC(bi%ILMOS(i), j) + TBARGAT(i, j)*bi%ACLASS(bi%ILMOS(i), bi%JLMOS(i))
                        THLQACC(bi%ILMOS(i), j) = THLQACC(bi%ILMOS(i), j) + THLQGAT(i, j)*FAREGAT(i)
                        THICACC(bi%ILMOS(i), j) = THICACC(bi%ILMOS(i), j) + THICGAT(i, j)*FAREGAT(i)
                        THALACC(bi%ILMOS(i), j) = THALACC(bi%ILMOS(i), j) + (THLQGAT(i, j) + THICGAT(i, j))*FAREGAT(i)
            !Added by GSA compute daily heat conduction flux between layers
                        GFLXACC(bi%ILMOS(i), j) = GFLXACC(bi%ILMOS(i), j) + GFLXGAT(i, j)*FAREGAT(i)
!                        (i) = THALACC_STG(i) + THALACC(i, j)
                        THLQ_FLD(bi%ILMOS(i), j) =  THLQ_FLD(bi%ILMOS(i), j) + THLQGAT(i, j)*RHOW*FAREGAT(i)*DLZWGAT(i, j)
                        THIC_FLD(bi%ILMOS(i), j) =  THIC_FLD(bi%ILMOS(i), j) + THICGAT(i, j)*RHOICE*FAREGAT(i)*DLZWGAT(i, j)
                    end do
                    ALVSACC(bi%ILMOS(i)) = ALVSACC(bi%ILMOS(i)) + ALVSGAT(i)*FAREGAT(i)*FSVHGRD(bi%ILMOS(i))
                    ALIRACC(bi%ILMOS(i)) = ALIRACC(bi%ILMOS(i)) + ALIRGAT(i)*FAREGAT(i)*FSIHGRD(bi%ILMOS(i))
                    if (SNOGAT(i) > 0.0) then
                        RHOSACC(bi%ILMOS(i)) = RHOSACC(bi%ILMOS(i)) + RHOSGAT(i)*FAREGAT(i)
                        TSNOACC(bi%ILMOS(i)) = TSNOACC(bi%ILMOS(i)) + TSNOGAT(i)*FAREGAT(i)
                        WSNOACC(bi%ILMOS(i)) = WSNOACC(bi%ILMOS(i)) + WSNOGAT(i)*FAREGAT(i)
                        SNOARE(bi%ILMOS(i)) = SNOARE(bi%ILMOS(i)) + FAREGAT(i)
                    end if
                    if (TCANGAT(i) > 0.5) then
                        TCANACC(bi%ILMOS(i)) = TCANACC(bi%ILMOS(i)) + TCANGAT(i)*FAREGAT(i)
                        CANARE(bi%ILMOS(i)) = CANARE(bi%ILMOS(i)) + FAREGAT(i)
                    end if
                    SNOACC(bi%ILMOS(i)) = SNOACC(bi%ILMOS(i)) + SNOGAT(i)*FAREGAT(i)
                    RCANACC(bi%ILMOS(i)) = RCANACC(bi%ILMOS(i)) + RCANGAT(i)*FAREGAT(i)
                    SCANACC(bi%ILMOS(i)) = SCANACC(bi%ILMOS(i)) + SCANGAT(i)*FAREGAT(i)
                    GROACC(bi%ILMOS(i)) = GROACC(bi%ILMOS(i)) + GROGAT(i)*FAREGAT(i)
                    FSINACC(bi%ILMOS(i)) = FSINACC(bi%ILMOS(i)) + cm%clin(cfk%FB)%climvGrd(bi%ILMOS(i))*FAREGAT(i)
                    FLINACC(bi%ILMOS(i)) = FLINACC(bi%ILMOS(i)) + cm%clin(cfk%FI)%climvGrd(bi%ILMOS(i))*FAREGAT(i)
                    FLUTACC(bi%ILMOS(i)) = FLUTACC(bi%ILMOS(i)) + SBC*GTGAT(i)**4*FAREGAT(i)
                    TAACC(bi%ILMOS(i)) = TAACC(bi%ILMOS(i)) + cm%clin(cfk%TT)%climvGrd(bi%ILMOS(i))*FAREGAT(i)
                    UVACC(bi%ILMOS(i)) = UVACC(bi%ILMOS(i)) + UVGRD(bi%ILMOS(i))*FAREGAT(i)
                    PRESACC(bi%ILMOS(i)) = PRESACC(bi%ILMOS(i)) + cm%clin(cfk%P0)%climvGrd(bi%ILMOS(i))*FAREGAT(i)
                    QAACC(bi%ILMOS(i)) = QAACC(bi%ILMOS(i)) + cm%clin(cfk%HU)%climvGrd(bi%ILMOS(i))*FAREGAT(i)
                end if
            end do !i = 1, bi%NML

    !> Update output data.
            call updatefieldsout_temp(vr, ts, iof, bi, &
                                      md, wb_h, &
                                      YEAR_NOW, JDAY_NOW, ceiling(NCOUNT/2.0), TIME_STEP_DELT - mod(NCOUNT, 2)*TIME_STEP_DELT)

!> CALCULATE AND PRINT DAILY AVERAGES.

!todo: use delta t here
            if (NCOUNT == 48) then !48 is the last half-hour period of the day
                      ! when they're numbered 1-48

    !no omp b/c of file IO
                do i = 1, bi%NA
                    if (bi%FRAC(i) /= 0.0) then
                        PREACC(i) = PREACC(i)
                        GTACC(i) = GTACC(i)/real(NSUM)
                        QEVPACC(i) = QEVPACC(i)/real(NSUM)
                        EVAPACC(i) = EVAPACC(i)
                        HFSACC(i) = HFSACC(i)/real(NSUM)
                        HMFNACC(i) = HMFNACC(i)/real(NSUM)
                        ROFACC(i) = ROFACC(i)
                        ROFOACC(i) = ROFOACC(i)
                        ROFSACC(i) = ROFSACC(i)
                        ROFBACC(i) = ROFBACC(i)
                        WTBLACC(i) = WTBLACC(i)/real(NSUM)
                        do j = 1, bi%IGND
                            TBARACC(i, j) = TBARACC(i, j)/real(NSUM)
                            THLQACC(i, j) = THLQACC(i, j)/real(NSUM)
                            THICACC(i, j) = THICACC(i, j)/real(NSUM)
                            THALACC(i, j) = THALACC(i, j)/real(NSUM)
                        end do
                        if (FSINACC(i) > 0.0) then
                            ALVSACC(i) = ALVSACC(i)/(FSINACC(i)*0.5)
                            ALIRACC(i) = ALIRACC(i)/(FSINACC(i)*0.5)
                        else
                            ALVSACC(i) = 0.0
                            ALIRACC(i) = 0.0
                        end if
                        if (SNOARE(i) > 0.0) then
                            RHOSACC(i) = RHOSACC(i)/SNOARE(i)
                            TSNOACC(i) = TSNOACC(i)/SNOARE(i)
                            WSNOACC(i) = WSNOACC(i)/SNOARE(i)
                        end if
                        if (CANARE(i) > 0.0) then
                            TCANACC(i) = TCANACC(i)/CANARE(i)
                        end if
                        SNOACC(i) = SNOACC(i)/real(NSUM)
                        RCANACC(i) = RCANACC(i)/real(NSUM)
                        SCANACC(i) = SCANACC(i)/real(NSUM)
                        GROACC(i) = GROACC(i)/real(NSUM)
                        FSINACC(i) = FSINACC(i)/real(NSUM)
                        FLINACC(i) = FLINACC(i)/real(NSUM)
                        FLUTACC(i) = FLUTACC(i)/real(NSUM)
                        TAACC(i) = TAACC(i)/real(NSUM)
                        UVACC(i) = UVACC(i)/real(NSUM)
                        PRESACC(i) = PRESACC(i)/real(NSUM)
                        QAACC(i) = QAACC(i)/real(NSUM)
!* ALTOT: the average of the visible spectrum and infrared spectrum
                        ALTOT = (ALVSACC(i) + ALIRACC(i))/2.0
                        FSSTAR = FSINACC(i)*(1.0 - ALTOT)
                        FLSTAR = FLINACC(i) - FLUTACC(i)
                        QH = HFSACC(i)
                        QE = QEVPACC(i)
                        BEG = FSSTAR + FLSTAR - QH - QE
                        SNOMLT = HMFNACC(i)
                        if (RHOSACC(i) > 0.0) then
                            ZSN = SNOACC(i)/RHOSACC(i)
                        else
                            ZSN = 0.0
                        end if
                        if (TCANACC(i) > 0.01) then
                            TCN = TCANACC(i) - TFREZ
                        else
                            TCN = 0.0
                        end if
                        if (TSNOACC(i) > 0.01) then
                            TSN = TSNOACC(i) - TFREZ
                        else
                            TSN = 0.0
                        end if
                        if (bi%ILW == 1) then
                            GTOUT = GTACC(i) - TFREZ
                        else
                            GTOUT = 0.0
                        end if

!> update components for final water balance tally
                        TOTAL_PRE = TOTAL_PRE + PREACC(i)
                        TOTAL_EVAP = TOTAL_EVAP + EVAPACC(i)
                        TOTAL_ROF = TOTAL_ROF + ROFACC(i)
                        TOTAL_ROFO = TOTAL_ROFO + ROFOACC(i)
                        TOTAL_ROFS = TOTAL_ROFS + ROFSACC(i)
                        TOTAL_ROFB = TOTAL_ROFB + ROFBACC(i)
                        TOTAL_PREACC = TOTAL_PREACC + PREACC(i)
                        TOTAL_EVAPACC = TOTAL_EVAPACC + EVAPACC(i)
                        TOTAL_ROFACC = TOTAL_ROFACC + ROFACC(i)
                        TOTAL_ROFOACC = TOTAL_ROFOACC + ROFOACC(i)
                        TOTAL_ROFSACC = TOTAL_ROFSACC + ROFSACC(i)
                        TOTAL_ROFBACC = TOTAL_ROFBACC + ROFBACC(i)
                        wb%pre(i) = wb%pre(i) + PREACC(i)
                        wb%evap(i) = wb%evap(i) + EVAPACC(i)
                        wb%rof(i) = wb%rof(i) + ROFACC(i)
                        wb%rofo(i) = wb%rofo(i) + ROFOACC(i)
                        wb%rofs(i) =  wb%rofs(i) + ROFSACC(i)
                        wb%rofb(i) = wb%rofb(i) + ROFBACC(i)

!> update components for final energy balance tally
                        TOTAL_HFSACC  = TOTAL_HFSACC  + HFSACC(i)
                        TOTAL_QEVPACC = TOTAL_QEVPACC + QEVPACC(i)
                        eng%hfs(i) = eng%hfs(i) + HFSACC(i)
                        eng%qevp(i) = eng%qevp(i) + QEVPACC(i)
                        do j = 1, bi%IGND
                            eng%gflx(i, j) = eng%gflx(i, j) + GFLXACC(i, j)
                        end do
                    end if
                end do

    !> update components for final water balance tally
                wb%rcan = 0.0
                wb%sncan = 0.0
                wb%pndw = 0.0
                wb%sno = 0.0
                wb%wsno = 0.0
                wb%lqws = 0.0
                wb%frws = 0.0
                sov%tbar = 0.0
                sov%thic = 0.0
                sov%thic = 0.0

                do i = 1, bi%NML
                    if (bi%FRAC(bi%ILMOS(i)) >= 0.0) then
!-            DO M = 1, NMTEST
                        TOTAL_SCAN = TOTAL_SCAN + FAREGAT(i)*SCANGAT(i)
                        TOTAL_RCAN = TOTAL_RCAN + FAREGAT(i)*RCANGAT(i)
                        TOTAL_SNO = TOTAL_SNO + FAREGAT(i)*SNOGAT(i)
                        TOTAL_WSNO = TOTAL_WSNO + FAREGAT(i)*WSNOGAT(i)
                        TOTAL_ZPND = TOTAL_ZPND + FAREGAT(i)*ZPNDGAT(i)*RHOW
                        wb%rcan(bi%ILMOS(i)) = wb%rcan(bi%ILMOS(i)) + FAREGAT(i)*SCANGAT(i)
                        wb%sncan(bi%ILMOS(i)) = wb%sncan(bi%ILMOS(i)) + FAREGAT(i)*RCANGAT(i)
                        wb%pndw(bi%ILMOS(i)) = wb%pndw(bi%ILMOS(i)) + FAREGAT(i)*ZPNDGAT(i)*RHOW
                        wb%sno(bi%ILMOS(i)) = wb%sno(bi%ILMOS(i)) + FAREGAT(i)*SNOGAT(i)
                        wb%wsno(bi%ILMOS(i)) = wb%wsno(bi%ILMOS(i)) + FAREGAT(i)*WSNOGAT(i)
                        do j = 1, bi%IGND
                            TOTAL_THLQ(j) = TOTAL_THLQ(j) + FAREGAT(i)*THLQGAT(i, j)*RHOW*DLZWGAT(i, j)
                            TOTAL_THIC(j) = TOTAL_THIC(j) + FAREGAT(i)*THICGAT(i, j)*RHOICE*DLZWGAT(i, j)
                            wb%lqws(bi%ILMOS(i), j) = wb%lqws(bi%ILMOS(i), j) + FAREGAT(i)*THLQGAT(i, j)*RHOW*DLZWGAT(i, j)
                            wb%frws(bi%ILMOS(i), j) = wb%frws(bi%ILMOS(i), j) + FAREGAT(i)*THICGAT(i, j)*RHOICE*DLZWGAT(i, j)
                            sov%tbar(bi%ILMOS(i), j) = sov%tbar(bi%ILMOS(i), j) + TBARGAT(i, j)*bi%ACLASS(bi%ILMOS(i), bi%JLMOS(i))
                            sov%thic(bi%ILMOS(i), j) = sov%thic(bi%ILMOS(i), j) + FAREGAT(i)*THICGAT(i, j)
                            sov%thlq(bi%ILMOS(i), j) = sov%thlq(bi%ILMOS(i), j) + FAREGAT(i)*THLQGAT(i, j)
                        end do
!-            END DO
                    end if !(bi%FRAC(i) >= 0.0) then
                end do !i = 1, bi%NML

    !> Calculate storage
                wb%stg = wb%rcan + wb%sncan + wb%pndw + wb%sno + wb%wsno + sum(wb%lqws, 2) + sum(wb%frws, 2)
                wb%dstg = wb%stg - wb%dstg
                TOTAL_STORE = TOTAL_SCAN + TOTAL_RCAN + TOTAL_SNO + TOTAL_WSNO + TOTAL_ZPND + sum(TOTAL_THLQ) + sum(TOTAL_THIC)

    !> Write output CSV files.
                if (BASINBALANCEOUTFLAG > 0) then

        !> Water balance.
                    write(fls%fl(mfk%f900)%iun, "(i4,',', i5,',', 999(e14.6,','))") &
                          JDAY_NOW, YEAR_NOW, &
                          TOTAL_PREACC/TOTAL_AREA, &
                          TOTAL_EVAPACC/TOTAL_AREA, &
                          TOTAL_ROFACC/TOTAL_AREA, &
                          TOTAL_ROFOACC/TOTAL_AREA, &
                          TOTAL_ROFSACC/TOTAL_AREA, &
                          TOTAL_ROFBACC/TOTAL_AREA, &
                          TOTAL_PRE/TOTAL_AREA, &
                          TOTAL_EVAP/TOTAL_AREA, &
                          TOTAL_ROF/TOTAL_AREA, &
                          TOTAL_ROFO/TOTAL_AREA, &
                          TOTAL_ROFS/TOTAL_AREA, &
                          TOTAL_ROFB/TOTAL_AREA, &
                          TOTAL_SCAN/TOTAL_AREA, &
                          TOTAL_RCAN/TOTAL_AREA, &
                          TOTAL_SNO/TOTAL_AREA, &
                          TOTAL_WSNO/TOTAL_AREA, &
                          TOTAL_ZPND/TOTAL_AREA, &
                          (TOTAL_THLQ(j)/TOTAL_AREA, j = 1, bi%IGND), &
                          (TOTAL_THIC(j)/TOTAL_AREA, j = 1, bi%IGND), &
                          ((TOTAL_THLQ(j) + TOTAL_THIC(j))/TOTAL_AREA, j = 1, bi%IGND), &
                          SUM(TOTAL_THLQ(1:bi%IGND))/TOTAL_AREA, &
                          SUM(TOTAL_THIC(1:bi%IGND))/TOTAL_AREA, &
                          (SUM(TOTAL_THLQ(1:bi%IGND)) + SUM(TOTAL_THIC(1:bi%IGND)))/TOTAL_AREA, &
                          TOTAL_STORE/TOTAL_AREA, &
                          (TOTAL_STORE - TOTAL_STORE_2)/TOTAL_AREA, &
                          (TOTAL_STORE - INIT_STORE)/TOTAL_AREA

        !> Energy balance.
                    write(901, "(i4,',', i5,',', 999(e12.5,','))") &
                          JDAY_NOW, YEAR_NOW, &
                          TOTAL_HFSACC/TOTAL_AREA, &
                          TOTAL_QEVPACC/TOTAL_AREA

        ! Monthly totals.
                    TOTAL_PRE_ACC_M = TOTAL_PRE_ACC_M + TOTAL_PRE
                    TOTAL_EVAP_ACC_M = TOTAL_EVAP_ACC_M + TOTAL_EVAP
                    TOTAL_ROF_ACC_M = TOTAL_ROF_ACC_M + TOTAL_ROF
                    TOTAL_ROFO_ACC_M = TOTAL_ROFO_ACC_M + TOTAL_ROFO
                    TOTAL_ROFS_ACC_M = TOTAL_ROFS_ACC_M + TOTAL_ROFS
                    TOTAL_ROFB_ACC_M = TOTAL_ROFB_ACC_M + TOTAL_ROFB
                    TOTAL_PRE_M = TOTAL_PRE_M + TOTAL_PRE
                    TOTAL_EVAP_M = TOTAL_EVAP_M + TOTAL_EVAP
                    TOTAL_ROF_M = TOTAL_ROF_M + TOTAL_ROF
                    TOTAL_ROFO_M = TOTAL_ROFO_M + TOTAL_ROFO
                    TOTAL_ROFS_M = TOTAL_ROFS_M + TOTAL_ROFS
                    TOTAL_ROFB_M = TOTAL_ROFB_M + TOTAL_ROFB
                    TOTAL_SCAN_M = TOTAL_SCAN_M + TOTAL_SCAN
                    TOTAL_RCAN_M = TOTAL_RCAN_M + TOTAL_RCAN
                    TOTAL_SNO_M = TOTAL_SNO_M + TOTAL_SNO
                    TOTAL_WSNO_M = TOTAL_WSNO_M + TOTAL_WSNO
                    TOTAL_ZPND_M = TOTAL_ZPND_M + TOTAL_ZPND
                    TOTAL_THLQ_M = TOTAL_THLQ_M + TOTAL_THLQ
                    TOTAL_THIC_M = TOTAL_THIC_M + TOTAL_THIC
                    TOTAL_STORE_M = TOTAL_STORE
                    TOTAL_STORE_ACC_M = TOTAL_STORE

        ! Write out monthly totals.
                    call FIND_MONTH(JDAY_NOW, YEAR_NOW, imonth_now)
                    if (imonth_now /= imonth_old) then
                        write(902, "(i4,',', i5,',', 999(e14.6,','))") &
                              JDAY_NOW, YEAR_NOW, &
                              TOTAL_PRE_ACC_M/TOTAL_AREA, &
                              TOTAL_EVAP_ACC_M/TOTAL_AREA, &
                              TOTAL_ROF_ACC_M/TOTAL_AREA, &
                              TOTAL_ROFO_ACC_M/TOTAL_AREA, &
                              TOTAL_ROFS_ACC_M/TOTAL_AREA, &
                              TOTAL_ROFB_ACC_M/TOTAL_AREA, &
                              TOTAL_PRE_M/TOTAL_AREA, &
                              TOTAL_EVAP_M/TOTAL_AREA, &
                              TOTAL_ROF_M/TOTAL_AREA, &
                              TOTAL_ROFO_M/TOTAL_AREA, &
                              TOTAL_ROFS_M/TOTAL_AREA, &
                              TOTAL_ROFB_M/TOTAL_AREA, &
                              TOTAL_SCAN_M/TOTAL_AREA, &
                              TOTAL_RCAN_M/TOTAL_AREA, &
                              TOTAL_SNO_M/TOTAL_AREA, &
                              TOTAL_WSNO_M/TOTAL_AREA, &
                              TOTAL_ZPND_M/TOTAL_AREA, &
                              (TOTAL_THLQ_M(j)/TOTAL_AREA, j = 1, bi%IGND), &
                              (TOTAL_THIC_M(j)/TOTAL_AREA, j = 1, bi%IGND), &
                              ((TOTAL_THLQ_M(j) + TOTAL_THIC_M(j))/TOTAL_AREA, j = 1, bi%IGND), &
                              sum(TOTAL_THLQ_M(1:bi%IGND))/TOTAL_AREA, &
                              sum(TOTAL_THIC_M(1:bi%IGND))/TOTAL_AREA, &
                              (sum(TOTAL_THLQ_M(1:bi%IGND)) + sum(TOTAL_THIC_M(1:bi%IGND)))/TOTAL_AREA, &
                              TOTAL_STORE_M/TOTAL_AREA, &
                              (TOTAL_STORE_M - TOTAL_STORE_2_M)/TOTAL_AREA, &
                              (TOTAL_STORE_ACC_M - INIT_STORE)/TOTAL_AREA
                        TOTAL_PRE_M = 0.0
                        TOTAL_EVAP_M = 0.0
                        TOTAL_ROF_M = 0.0
                        TOTAL_ROFO_M = 0.0
                        TOTAL_ROFS_M = 0.0
                        TOTAL_ROFB_M = 0.0
                        TOTAL_SCAN_M = 0.0
                        TOTAL_RCAN_M = 0.0
                        TOTAL_SNO_M = 0.0
                        TOTAL_WSNO_M = 0.0
                        TOTAL_ZPND_M = 0.0
                        TOTAL_THLQ_M = 0.0
                        TOTAL_THIC_M = 0.0
                        TOTAL_STORE_2_M = TOTAL_STORE_M
                        TOTAL_STORE_M = 0.0
                        imonth_old = imonth_now
                    end if
                end if !(BASINBALANCEOUTFLAG > 0) then

!>  Added by Gonzalo Sapriza
    !DELTA STORAGE
                do i = 1, bi%IGND
                    DSTG = DSTG + THLQ_FLD(:, i) + THIC_FLD(:, i)
                end do
                DSTG = DSTG + RCANACC + SCANACC + SNOACC - STG_I

                if (OUTFIELDSFLAG == 1) then
                    call UpdateFIELDSOUT(vr, ts, iof, &
                                         wb%pre, wb%evap, wb%rof, wb%dstg, &
                                         sov%tbar, wb%lqws, wb%frws, &
                                         wb%rcan, wb%sncan, &
                                         wb%pndw, wb%sno, wb%wsno, &
                                         eng%gflx, eng%hfs, eng%qevp, &
                                         sov%thlq, sov%thic, &
                                         bi%IGND, &
                                         JDAY_NOW, YEAR_NOW)
                end if
                STG_I = DSTG + STG_I

!RESET ACCUMULATION VARIABLES TO ZERO
!> RESET ACCUMULATOR ARRAYS.
                PREACC = 0.0
                GTACC = 0.0
                QEVPACC = 0.0
                HFSACC = 0.0
                HMFNACC = 0.0
                ROFACC = 0.0
                SNOACC = 0.0
                CANARE = 0.0
                SNOARE = 0.0
                ROFOACC = 0.0
                ROFSACC = 0.0
                ROFBACC = 0.0
                WTBLACC = 0.0
                TBARACC = 0.0
                THLQACC = 0.0
                THICACC = 0.0
                THALACC = 0.0
                GFLXACC = 0.0
                ALVSACC = 0.0
                ALIRACC = 0.0
                RHOSACC = 0.0
                TSNOACC = 0.0
                WSNOACC = 0.0
                TCANACC = 0.0
                RCANACC = 0.0
                SCANACC = 0.0
                GROACC = 0.0
                FSINACC = 0.0
                FLINACC = 0.0
                TAACC = 0.0
                UVACC = 0.0
                PRESACC = 0.0
                QAACC = 0.0
                EVAPACC = 0.0
                FLUTACC = 0.0
                TOTAL_STORE_2 = TOTAL_STORE
                TOTAL_STORE = 0.0
                TOTAL_RCAN = 0.0
                TOTAL_SCAN = 0.0
                TOTAL_SNO = 0.0
                TOTAL_WSNO = 0.0
                TOTAL_ZPND = 0.0
                TOTAL_THLQ = 0.0
                TOTAL_THIC = 0.0
                TOTAL_PRE = 0.0
                TOTAL_EVAP = 0.0
                TOTAL_ROF = 0.0
                TOTAL_ROFO = 0.0
                TOTAL_ROFS = 0.0
                TOTAL_ROFB = 0.0
                TOTAL_HFSACC = 0.0
                TOTAL_QEVPACC = 0.0
                eng%gflx = 0.0
                eng%hfs = 0.0
                eng%qevp = 0.0
                sov%tbar = 0.0
                sov%thic = 0.0
                sov%thlq = 0.0
                THIC_FLD = 0.0
                THLQ_FLD = 0.0
                DSTG = 0.0
            end if !(NCOUNT == 48) then
        end if !(ipid == 0) then

        NCOUNT = NCOUNT + 1 !todo: does this work with hourly forcing data?
        NSUM = NSUM + 1
        NSUM_TOTAL = NSUM_TOTAL + 1
        if (NCOUNT > 48) then !48 is the last half-hour period of the day
                      ! when they're numbered 1-48
            NCOUNT = 1
            NSUM = 1
        end if

!> *********************************************************************
!> Call routing routine
!> *********************************************************************
        if (ipid == 0) then
            call WF_ROUTE(WF_ROUTETIMESTEP, WF_R1, WF_R2, &
                          bi%NA, bi%NAA, bi%NTYPE, bi%yCount, bi%xCount, bi%iyMin, &
                          bi%iyMax, bi%jxMin, bi%jxMax, bi%yyy, bi%xxx, bi%IAK, bi%IROUGH, &
                          bi%ICHNL, bi%NEXT, bi%IREACH, bi%AL, bi%GRDN, bi%GRDE, &
                          bi%DA, bi%BNKFLL, bi%SLOPE_CHNL, bi%ELEV, bi%FRAC, &
                          WF_NO, WF_NL, WF_MHRD, WF_KT, WF_IY, WF_JX, &
                          WF_QHYD, WF_RES, WF_RESSTORE, WF_NORESV_CTRL, WF_R, &
                          WF_NORESV, WF_NREL, WF_KTR, WF_IRES, WF_JRES, WF_RESNAME, &
                          WF_B1, WF_B2, WF_QREL, WF_QR, &
                          WF_TIMECOUNT, WF_NHYD, WF_QBASE, WF_QI1, WF_QI2, WF_QO1, WF_QO2, &
                          WF_STORE1, WF_STORE2, &
                          DRIVERTIMESTEP, ROFGRD, bi%NA, M_C, M_R, M_S, bi%NA, &
                          WF_S, JAN, JDAY_NOW, HOUR_NOW, MINS_NOW)
            do i = 1, WF_NO
                WF_QSYN(i) = WF_QO2(WF_S(i))
                WF_QSYN_AVG(i) = WF_QSYN_AVG(i) + WF_QO2(WF_S(i))
                WF_QSYN_CUM(i) = WF_QSYN_CUM(i) + WF_QO2(WF_S(i))
                WF_QHYD_AVG(i) = WF_QHYD(i) !(MAM)THIS SEEMS WORKING OKAY (AS IS THE CASE IN THE READING) FOR A DAILY STREAM FLOW DATA.
            end do
            if (JAN == 1) then
!>     this is done so that INIT_STORE is not recalculated for
!>     each iteration when wf_route is not used
                JAN = 2
            end if

!> *********************************************************************
!> Write measured and simulated streamflow to file and screen
!> Also write daily summary (pre, evap, rof)
!> *********************************************************************

    !> Write output for hourly streamflow.
            if (STREAMFLOWFLAG == 1 .and. STREAMFLOWOUTFLAG >= 2) then
                write(71, 5085) JDAY_NOW, HOUR_NOW, MINS_NOW, (WF_QHYD(i), WF_QSYN(i), i = 1, WF_NO)
            end if

            if (NCOUNT == 48) then !48 is the last half-hour period of the day
                      ! when they're numbered 1-48

                do i = 1, WF_NO
                    WF_QHYD_CUM(i) = WF_QHYD_CUM(i) + WF_QHYD_AVG(i)
                end do

    !> Write output for daily and cumulative daily streamflow.
                if (STREAMFLOWOUTFLAG > 0) then
                    write(fls%fl(mfk%f70)%iun, 5084) JDAY_NOW, (WF_QHYD_AVG(i), WF_QSYN_AVG(i)/NCOUNT, i = 1, WF_NO)
                    if (STREAMFLOWOUTFLAG >= 2) then
                        write(72, 5084) JDAY_NOW, (WF_QHYD_CUM(i), WF_QSYN_CUM(i)/NCOUNT, i = 1, WF_NO)
                    end if
                end if

5084    format(i5, ',', f10.3, 999(',', f10.3))
5085    format(3(i5, ','), f10.3, 999(',', f10.3))

                if (ro%VERBOSEMODE > 0) then
                    if (WF_NUM_POINTS > 1) then !FOR MORE THAN ONE OUTPUT
                        print 5176, YEAR_NOW, JDAY_NOW, (WF_QHYD_AVG(i), WF_QSYN_AVG(i)/NCOUNT, i = 1, WF_NO)
                    else !FOR GENERAL CASE OR SINGLE GRID OUTPUT POINT
    !todo: Update or remove this altogether. If to update, take NA-1 or something more akin
    !to an outlet, than the average middle-elevation grid (as it's coded now).
    !Should there be a choice to print point-process (pre, evap, rof) vs flow-process (wf_qo2)?
                        j = ceiling(real(bi%NA)/2); if (WF_NUM_POINTS > 0) j = op%N_OUT(1)
                        print 5176, YEAR_NOW, JDAY_NOW, (WF_QHYD_AVG(i), WF_QSYN_AVG(i)/NCOUNT, i = 1, WF_NO), &
                            wb%pre(j), wb%evap(j), wb%rof(j)
                    end if
                end if !(ro%VERBOSEMODE > 0) then
                if (mtsflg%AUTOCALIBRATIONFLAG > 0) then
                    call stats_update_daily(WF_QHYD_AVG, WF_QSYN_AVG, NCOUNT)
                    if (mtsflg%PREEMPTIONFLAG > 1) then
                        if (FTEST > FBEST) goto 199
                    end if
                end if
                WF_QSYN_AVG = 0.0
                wb%pre = 0.0
                wb%evap = 0.0
                wb%rof = 0.0
                wb%rofo = 0.0
                wb%rofs =  0.0
                wb%rofb = 0.0
            end if !(NCOUNT == 48) then
        end if !(ipid == 0) then

5176    format(2i5, 999(f10.3))

! *********************************************************************
! Update time counters and return to beginning of main loop
! *********************************************************************
        MINS_NOW = MINS_NOW + TIME_STEP_MINS ! increment the current time by 30 minutes
        if (MINS_NOW == 60) then
            MINS_NOW = 0
            HOUR_NOW = HOUR_NOW + 1
            if (HOUR_NOW == 24) then
                HOUR_NOW = 0
!    IF(mtsflg%AUTOCALIBRATIONFLAG .GE. 1 .AND. mtsflg%PREEMPTIONFLAG == 1)THEN
!      IF(FTEST > FBEST) GOTO 199
!    ENDIF
                JDAY_NOW = JDAY_NOW + 1
                if (JDAY_NOW >= 366) then
                    if (mod(YEAR_NOW, 400) == 0) then !LEAP YEAR
                        if (JDAY_NOW == 367) then
                            JDAY_NOW = 1
                            YEAR_NOW = YEAR_NOW + 1
                        end if
                    else if (mod(YEAR_NOW, 100) == 0) then !NOT A LEAP YEAR
                        JDAY_NOW = 1
                        YEAR_NOW = YEAR_NOW + 1
                    else if (mod(YEAR_NOW, 4) == 0) then !LEAP YEAR
                        if (JDAY_NOW == 367) then
                            JDAY_NOW = 1
                            YEAR_NOW = YEAR_NOW + 1
                        end if
                    else !NOT A LEAP YEAR
                        JDAY_NOW = 1
                        YEAR_NOW = YEAR_NOW + 1
                    end if
                end if
            end if
        end if

!> check if we should terminate the run yet
        if (YEAR_NOW >= YEAR_STOP .and. YEAR_STOP > 0) then
            if (YEAR_NOW > YEAR_STOP) then
                ENDDATE = .true.
            else if (YEAR_NOW == YEAR_STOP .and. JDAY_NOW >= JDAY_STOP) then
                if (JDAY_NOW > JDAY_STOP) then
                    ENDDATE = .true.
                else if (JDAY_NOW == JDAY_STOP .and. HOUR_NOW >= HOUR_STOP) then
                    if (HOUR_NOW > HOUR_STOP) then
                        ENDDATE = .true.
                    else if (HOUR_NOW == HOUR_STOP .and. MINS_NOW >= MINS_STOP) then
                        ENDDATE = .true.
                    end if
                end if
            end if
        end if
        TIME_STEP_NOW = TIME_STEP_NOW + TIME_STEP_MINS
        if (TIME_STEP_NOW == HOURLYFLAG) TIME_STEP_NOW = 0

    !> *********************************************************************
    !> Read in meteorological forcing data
    !> *********************************************************************
        call climate_module_loaddata(bi, .false., cm, ENDDATA)

    end do !while (.not. ENDDATE .and. .not. ENDDATA)

    !> End program if not the head node.
    if (ipid /= 0) then
!        print 4696, ipid
        goto 999

!4696 format (1x, 'Node ', i4, ' is exiting...')

    end if !(ipid /= 0) then

    call CLASSS(cp%TBARROW, cp%THLQROW, cp%THICROW, GFLXROW, TSFSROW, &
                cp%TPNDROW, cp%ZPNDROW, TBASROW, cp%ALBSROW, cp%TSNOROW, &
                cp%RHOSROW, cp%SNOROW, cp%TCANROW, cp%RCANROW, cp%SCANROW, &
                cp%GROROW, CMAIROW, TACROW, QACROW, WSNOROW, &
                bi%ILMOS, bi%JLMOS, bi%IWMOS, bi%JWMOS, &
                bi%NML, bi%NA, bi%NTYPE, bi%ILG, bi%IGND, ICAN, ICAN + 1, &
                TBARGAT, THLQGAT, THICGAT, GFLXGAT, TSFSGAT, &
                TPNDGAT, ZPNDGAT, TBASGAT, ALBSGAT, TSNOGAT, &
                RHOSGAT, SNOGAT, TCANGAT, RCANGAT, SCANGAT, &
                GROGAT, CMAIGAT, TACGAT, QACGAT, WSNOGAT, &
                cp%MANNROW, MANNGAT, cp%DDROW, DDGAT, &
                cp%SANDROW, SANDGAT, cp%CLAYROW, CLAYGAT, cp%XSLPROW, XSLPGAT, &
                DrySnowRow, SnowAgeROW, DrySnowGAT, SnowAgeGAT, &
                TSNOdsROW, RHOSdsROW, TSNOdsGAT, RHOSdsGAT, &
                DriftROW, SublROW, DepositionROW, &
                DriftGAT, SublGAT, DepositionGAT)
!>
!>   * SCATTER OPERATION ON DIAGNOSTIC VARIABLES SPLIT OUT OF
!>   * CLASSS FOR CONSISTENCY WITH GCM APPLICATIONS.
!>
    do 380 k = 1, bi%NML
        CDHROW(bi%ILMOS(k), bi%JLMOS(k)) = CDHGAT(k)
        CDMROW(bi%ILMOS(k), bi%JLMOS(k)) = CDMGAT(k)
        HFSROW(bi%ILMOS(k), bi%JLMOS(k)) = HFSGAT(k)
        TFXROW(bi%ILMOS(k), bi%JLMOS(k)) = TFXGAT(k)
        QEVPROW(bi%ILMOS(k), bi%JLMOS(k)) = QEVPGAT(k)
        QFSROW(bi%ILMOS(k), bi%JLMOS(k)) = QFSGAT(k)
        QFXROW(bi%ILMOS(k), bi%JLMOS(k)) = QFXGAT(k)
        PETROW(bi%ILMOS(k), bi%JLMOS(k)) = PETGAT(k)
        GAROW(bi%ILMOS(k), bi%JLMOS(k)) = GAGAT(k)
        EFROW(bi%ILMOS(k), bi%JLMOS(k)) = EFGAT(k)
        GTROW(bi%ILMOS(k), bi%JLMOS(k)) = GTGAT(k)
        QGROW(bi%ILMOS(k), bi%JLMOS(k)) = QGGAT(k)
        ALVSROW(bi%ILMOS(k), bi%JLMOS(k)) = ALVSGAT(k)
        ALIRROW(bi%ILMOS(k), bi%JLMOS(k)) = ALIRGAT(k)
        SFCTROW(bi%ILMOS(k), bi%JLMOS(k)) = SFCTGAT(k)
        SFCUROW(bi%ILMOS(k), bi%JLMOS(k)) = SFCUGAT(k)
        SFCVROW(bi%ILMOS(k), bi%JLMOS(k)) = SFCVGAT(k)
        SFCQROW(bi%ILMOS(k), bi%JLMOS(k)) = SFCQGAT(k)
        FSNOROW(bi%ILMOS(k), bi%JLMOS(k)) = FSNOGAT(k)
        FSGVROW(bi%ILMOS(k), bi%JLMOS(k)) = FSGVGAT(k)
        FSGSROW(bi%ILMOS(k), bi%JLMOS(k)) = FSGSGAT(k)
        FSGGROW(bi%ILMOS(k), bi%JLMOS(k)) = FSGGGAT(k)
        FLGVROW(bi%ILMOS(k), bi%JLMOS(k)) = FLGVGAT(k)
        FLGSROW(bi%ILMOS(k), bi%JLMOS(k)) = FLGSGAT(k)
        FLGGROW(bi%ILMOS(k), bi%JLMOS(k)) = FLGGGAT(k)
        HFSCROW(bi%ILMOS(k), bi%JLMOS(k)) = HFSCGAT(k)
        HFSSROW(bi%ILMOS(k), bi%JLMOS(k)) = HFSSGAT(k)
        HFSGROW(bi%ILMOS(k), bi%JLMOS(k)) = HFSGGAT(k)
        HEVCROW(bi%ILMOS(k), bi%JLMOS(k)) = HEVCGAT(k)
        HEVSROW(bi%ILMOS(k), bi%JLMOS(k)) = HEVSGAT(k)
        HEVGROW(bi%ILMOS(k), bi%JLMOS(k)) = HEVGGAT(k)
        HMFCROW(bi%ILMOS(k), bi%JLMOS(k)) = HMFCGAT(k)
        HMFNROW(bi%ILMOS(k), bi%JLMOS(k)) = HMFNGAT(k)
        HTCCROW(bi%ILMOS(k), bi%JLMOS(k)) = HTCCGAT(k)
        HTCSROW(bi%ILMOS(k), bi%JLMOS(k)) = HTCSGAT(k)
        PCFCROW(bi%ILMOS(k), bi%JLMOS(k)) = PCFCGAT(k)
        PCLCROW(bi%ILMOS(k), bi%JLMOS(k)) = PCLCGAT(k)
        PCPNROW(bi%ILMOS(k), bi%JLMOS(k)) = PCPNGAT(k)
        PCPGROW(bi%ILMOS(k), bi%JLMOS(k)) = PCPGGAT(k)
        QFGROW(bi%ILMOS(k), bi%JLMOS(k)) = QFGGAT(k)
        QFNROW(bi%ILMOS(k), bi%JLMOS(k)) = QFNGAT(k)
        QFCLROW(bi%ILMOS(k), bi%JLMOS(k)) = QFCLGAT(k)
        QFCFROW(bi%ILMOS(k), bi%JLMOS(k)) = QFCFGAT(k)
        ROFROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFGAT(k)
        ROFOROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFOGAT(k)
        ROFSROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFSGAT(k)
        ROFBROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFBGAT(k)
        TROFROW(bi%ILMOS(k), bi%JLMOS(k)) = TROFGAT(k)
        TROOROW(bi%ILMOS(k), bi%JLMOS(k)) = TROOGAT(k)
        TROSROW(bi%ILMOS(k), bi%JLMOS(k)) = TROSGAT(k)
        TROBROW(bi%ILMOS(k), bi%JLMOS(k)) = TROBGAT(k)
        ROFCROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFCGAT(k)
        ROFNROW(bi%ILMOS(k), bi%JLMOS(k)) = ROFNGAT(k)
        ROVGROW(bi%ILMOS(k), bi%JLMOS(k)) = ROVGGAT(k)
        WTRCROW(bi%ILMOS(k), bi%JLMOS(k)) = WTRCGAT(k)
        WTRSROW(bi%ILMOS(k), bi%JLMOS(k)) = WTRSGAT(k)
        WTRGROW(bi%ILMOS(k), bi%JLMOS(k)) = WTRGGAT(k)
        DRROW(bi%ILMOS(k), bi%JLMOS(k)) = DRGAT(k)
        WTABROW(bi%ILMOS(k), bi%JLMOS(k)) = WTABGAT(k)
        ILMOROW(bi%ILMOS(k), bi%JLMOS(k)) = ILMOGAT(k)
        UEROW(bi%ILMOS(k), bi%JLMOS(k)) = UEGAT(k)
        HBLROW(bi%ILMOS(k), bi%JLMOS(k)) = HBLGAT(k)
380     continue
!>
    do 390 l = 1, bi%IGND
        do 390 k = 1, bi%NML
            HMFGROW(bi%ILMOS(k), bi%JLMOS(k), l) = HMFGGAT(k, l)
            HTCROW(bi%ILMOS(k), bi%JLMOS(k), l) = HTCGAT(k, l)
            QFCROW(bi%ILMOS(k), bi%JLMOS(k), l) = QFCGAT(k, l)
390     continue
!>
    do 430 m = 1, 50
        do 420 l = 1, 6
            do 410 k = 1, bi%NML
                ITCTROW(bi%ILMOS(k), bi%JLMOS(k), l, m) = ITCTGAT(k, l, m)
410     continue
420     continue
430     continue

!> *********************************************************************
!> Run is now over, print final results to the screen and close files
!> *********************************************************************

!> *********************************************************************
!> Save the state of the basin in r2c file format
!> *********************************************************************

!> Write the resume file
    if (SAVERESUMEFLAG == 2) then !todo: done: use a flag
        print *, 'Saving state variables in r2c file format'

! Allocate arrays for save_state_r2c
        open(55, file = 'save_state_r2c.txt', action = 'read')
        read(55, *, iostat = IOS) NR2C_S, DELTR2C_S
        if (IOS == 0) then
            allocate(GRD_S(NR2C_S), GAT_S(NR2C_S), GRDGAT_S(NR2C_S), R2C_ATTRIBUTES_S(NR2C_S, 3), stat = PAS)
            if (PAS /= 0) then
                print *, 'ALLOCATION ERROR: CHECK THE VALUE OF THE FIRST ', &
                    'RECORD AT THE FIRST LINE IN THE save_state_r2c.txt FILE. ', &
                    'IT SHOULD BE AN INTEGER VALUE (GREATER THAN 0).'
                stop
            end if
        end if
        close(55)

        call SAVE_STATE_R2C(bi%NML, NLTEST, NMTEST, NCOUNT, &
                            MINS_NOW, bi%ACLASS, NR2C_S, GRD_S, GAT_S, GRDGAT_S, R2C_ATTRIBUTES_S, &
                            bi%NA, bi%xxx, bi%yyy, bi%xCount, bi%yCount, bi%ILMOS, bi%JLMOS, bi%ILG, ICAN, ICP1, bi%IGND, &
                            TBARGAT, THLQGAT, THICGAT, TPNDGAT, ZPNDGAT, &
                            TBASGAT, ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, &
                            TCANGAT, RCANGAT, SCANGAT, GROGAT, CMAIGAT, &
                            FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT, PAMXGAT, &
                            PAMNGAT, CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, &
                            VPDAGAT, VPDBGAT, PSGAGAT, PSGBGAT, PAIDGAT, &
                            HGTDGAT, ACVDGAT, ACIDGAT, TSFSGAT, WSNOGAT, &
                            THPGAT, THRGAT, THMGAT, BIGAT, PSISGAT, &
                            GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, &
                            THFCGAT, PSIWGAT, DLZWGAT, ZBTWGAT, &
                            ZSNLGAT, ZPLGGAT, ZPLSGAT, TACGAT, QACGAT, &
                            DRNGAT, XSLPGAT, XDGAT, WFSFGAT, KSGAT, &
                            ALGWGAT, ALGDGAT, ASVDGAT, ASIDGAT, AGVDGAT, &
                            AGIDGAT, ISNDGAT, RADJGAT, ZBLDGAT, Z0ORGAT, &
                            ZRFMGAT, ZRFHGAT, ZDMGAT, ZDHGAT, FSVHGAT, &
                            FSIHGAT, CSZGAT, FDLGAT, ULGAT, VLGAT, &
                            TAGAT, QAGAT, PRESGAT, PREGAT, PADRGAT, &
                            VPDGAT, TADPGAT, RHOAGAT, RPCPGAT, TRPCGAT, &
                            SPCPGAT, TSPCGAT, RHSIGAT, FCLOGAT, DLONGAT, &
                            GGEOGAT, &
                            CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT, &
                            QFSGAT, QFXGAT, PETGAT, GAGAT, EFGAT, &
                            GTGAT, QGGAT, ALVSGAT, ALIRGAT, &
                            SFCTGAT, SFCUGAT, SFCVGAT, SFCQGAT, FSNOGAT, &
                            FSGVGAT, FSGSGAT, FSGGGAT, FLGVGAT, FLGSGAT, &
                            FLGGGAT, HFSCGAT, HFSSGAT, HFSGGAT, HEVCGAT, &
                            HEVSGAT, HEVGGAT, HMFCGAT, HMFNGAT, HTCCGAT, &
                            HTCSGAT, PCFCGAT, PCLCGAT, PCPNGAT, PCPGGAT, &
                            QFGGAT, QFNGAT, QFCLGAT, QFCFGAT, ROFGAT, &
                            ROFOGAT, ROFSGAT, ROFBGAT, TROFGAT, TROOGAT, &
                            TROSGAT, TROBGAT, ROFCGAT, ROFNGAT, ROVGGAT, &
                            WTRCGAT, WTRSGAT, WTRGGAT, DRGAT, GFLXGAT, &
                            HMFGGAT, HTCGAT, QFCGAT, MANNGAT, DDGAT, &
                            SANDGAT, CLAYGAT, IGDRGAT, VMODGAT, QLWOGAT, &
                            bi%CoordSys, bi%Datum, bi%Zone, bi%xOrigin, bi%yOrigin, bi%xDelta, bi%yDelta)
    end if !(SAVERESUMEFLAG == 2) then

!> Write the resume file
    if (SAVERESUMEFLAG == 1) then !todo: done: use a flag
        print *, 'Saving state variables'
        call SAVE_STATE(HOURLYFLAG, MINS_NOW, TIME_STEP_NOW, &
                        cm%clin(cfk%FB)%filefmt, cm%clin(cfk%FI)%filefmt, &
                        cm%clin(cfk%PR)%filefmt, cm%clin(cfk%TT)%filefmt, &
                        cm%clin(cfk%UV)%filefmt, cm%clin(cfk%P0)%filefmt, cm%clin(cfk%HU)%filefmt, &
                        cm%clin(cfk%FB)%climvGrd, FSVHGRD, FSIHGRD, cm%clin(cfk%FI)%climvGrd, &
                        i, j, bi%xCount, bi%yCount, jan, &
                        VPDGRD, TADPGRD, PADRGRD, RHOAGRD, RHSIGRD, &
                        RPCPGRD, TRPCGRD, SPCPGRD, TSPCGRD, cm%clin(cfk%TT)%climvGrd, &
                        cm%clin(cfk%HU)%climvGrd, cm%clin(cfk%PR)%climvGrd, RPREGRD, SPREGRD, cm%clin(cfk%P0)%climvGrd, &
!MAM - FOR FORCING DATA INTERPOLATION
                        FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE, &
                        TAGATPRE, ULGATPRE, PRESGATPRE, QAGATPRE, &
                        IPCP, bi%NA, bi%NA, bi%ILMOS, bi%JLMOS, bi%IWMOS, bi%JWMOS, &
                        bi%NML, bi%NMW, &
                        cp%GCGRD, cp%FAREROW, cp%MIDROW, bi%NTYPE, bi%ILG, NMTEST, &
                        TBARGAT, THLQGAT, THICGAT, TPNDGAT, ZPNDGAT, &
                        TBASGAT, ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, &
                        TCANGAT, RCANGAT, SCANGAT, GROGAT, FRZCGAT, CMAIGAT, &
                        FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT, PAMXGAT, &
                        PAMNGAT, CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, &
                        VPDAGAT, VPDBGAT, PSGAGAT, PSGBGAT, PAIDGAT, &
                        HGTDGAT, ACVDGAT, ACIDGAT, TSFSGAT, WSNOGAT, &
                        THPGAT, THRGAT, THMGAT, BIGAT, PSISGAT, &
                        GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, THFCGAT, &
                        PSIWGAT, DLZWGAT, ZBTWGAT, ZSNLGAT, ZPLGGAT, &
                        ZPLSGAT, TACGAT, QACGAT, DRNGAT, XSLPGAT, &
                        XDGAT, WFSFGAT, KSGAT, ALGWGAT, ALGDGAT, &
                        ASVDGAT, ASIDGAT, AGVDGAT, AGIDGAT, ISNDGAT, &
                        RADJGAT, ZBLDGAT, Z0ORGAT, ZRFMGAT, ZRFHGAT, &
                        ZDMGAT, ZDHGAT, FSVHGAT, FSIHGAT, CSZGAT, &
                        FDLGAT, ULGAT, VLGAT, TAGAT, QAGAT, PRESGAT, &
                        PREGAT, PADRGAT, VPDGAT, TADPGAT, RHOAGAT, &
                        RPCPGAT, TRPCGAT, SPCPGAT, TSPCGAT, RHSIGAT, &
                        FCLOGAT, DLONGAT, GGEOGAT, CDHGAT, CDMGAT, &
                        HFSGAT, TFXGAT, QEVPGAT, QFSGAT, QFXGAT, &
                        PETGAT, GAGAT, EFGAT, GTGAT, QGGAT, &
                        ALVSGAT, ALIRGAT, SFCTGAT, SFCUGAT, SFCVGAT, &
                        SFCQGAT, FSNOGAT, FSGVGAT, FSGSGAT, FSGGGAT, &
                        FLGVGAT, FLGSGAT, FLGGGAT, HFSCGAT, HFSSGAT, &
                        HFSGGAT, HEVCGAT, HEVSGAT, HEVGGAT, HMFCGAT, &
                        HMFNGAT, HTCCGAT, HTCSGAT, PCFCGAT, PCLCGAT, &
                        PCPNGAT, PCPGGAT, QFGGAT, QFNGAT, QFCLGAT, &
                        QFCFGAT, ROFGAT, ROFOGAT, ROFSGAT, ROFBGAT, &
                        TROFGAT, TROOGAT, TROSGAT, TROBGAT, ROFCGAT, &
                        ROFNGAT, ROVGGAT, WTRCGAT, WTRSGAT, WTRGGAT, &
                        DRGAT, HMFGGAT, HTCGAT, QFCGAT, ITCTGAT, &
                        bi%IGND, ICAN, ICP1, &
                        cp%TBARROW, cp%THLQROW, cp%THICROW, cp%TPNDROW, cp%ZPNDROW, &
                        TBASROW, cp%ALBSROW, cp%TSNOROW, cp%RHOSROW, cp%SNOROW, &
                        cp%TCANROW, cp%RCANROW, cp%SCANROW, cp%GROROW, CMAIROW, &
                        cp%FCANROW, cp%LNZ0ROW, cp%ALVCROW, cp%ALICROW, cp%PAMXROW, &
                        cp%PAMNROW, cp%CMASROW, cp%ROOTROW, cp%RSMNROW, cp%QA50ROW, &
                        cp%VPDAROW, cp%VPDBROW, cp%PSGAROW, cp%PSGBROW, PAIDROW, &
                        HGTDROW, ACVDROW, ACIDROW, TSFSROW, WSNOROW, &
                        THPROW, THRROW, THMROW, BIROW, PSISROW, &
                        GRKSROW, THRAROW, HCPSROW, TCSROW, THFCROW, &
                        PSIWROW, DLZWROW, ZBTWROW, hp%ZSNLROW, hp%ZPLGROW, &
                        hp%ZPLSROW, hp%FRZCROW, TACROW, QACROW, cp%DRNROW, cp%XSLPROW, &
                        cp%XDROW, WFSFROW, cp%KSROW, ALGWROW, ALGDROW, &
                        ASVDROW, ASIDROW, AGVDROW, AGIDROW, &
                        ISNDROW, RADJGRD, cp%ZBLDGRD, Z0ORGRD, &
                        cp%ZRFMGRD, cp%ZRFHGRD, ZDMGRD, ZDHGRD, CSZGRD, &
                        cm%clin(cfk%UV)%climvGrd, VLGRD, FCLOGRD, DLONGRD, GGEOGRD, &
                        cp%MANNROW, MANNGAT, cp%DDROW, DDGAT, &
                        IGDRROW, IGDRGAT, VMODGRD, VMODGAT, QLWOGAT, &
                        CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, &
                        WTVSTP, WTSSTP, WTGSTP, &
                        sl%DELZ, FCS, FGS, FC, FG, N, &
                        ALVSCN, ALIRCN, ALVSG, ALIRG, ALVSCS, &
                        ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, &
                        ALVSSC, ALIRSC, TRVSCN, TRIRCN, TRVSCS, &
                        TRIRCS, FSVF, FSVFS, &
                        RAICAN, RAICNS, SNOCAN, SNOCNS, &
                        FRAINC, FSNOWC, FRAICS, FSNOCS, &
                        DISP, DISPS, ZOMLNC, ZOMLCS, ZOELNC, ZOELCS, &
                        ZOMLNG, ZOMLNS, ZOELNG, ZOELNS, &
                        CHCAP, CHCAPS, CMASSC, CMASCS, CWLCAP, &
                        CWFCAP, CWLCPS, CWFCPS, RC, RCS, RBCOEF, &
                        FROOT, ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, &
                        TRSNOW, ZSNOW, JDAY_NOW, JLAT, IDISP, &
                        IZREF, IWF, IPAI, IHGT, IALC, IALS, IALG, &
                        TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG, &
                        THICEC, THICEG, HCPC, HCPG, TCTOPC, TCBOTC, &
                        TCTOPG, TCBOTG, &
                        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, &
                        G12CS, G12GS, G23C, G23G, G23CS, G23GS, &
                        QFREZC, QFREZG, QMELTC, QMELTG, &
                        EVAPC, EVAPCG,EVAPG, EVAPCS, EVPCSG, EVAPGS, &
                        TCANO, TCANS, TPONDC, TPONDG, TPNDCS, TPNDGS, &
                        TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, &
                        WTABGAT, &
                        ILMOGAT, UEGAT, HBLGAT, &
                        bi%ILW, ITC, ITCG, ITG, ISLFD, &
                        NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI, &
                        GFLXGAT, CDHROW, CDMROW, HFSROW, TFXROW, &
                        QEVPROW, QFSROW, QFXROW, PETROW, GAROW, &
                        EFROW, GTROW, QGROW, TSFROW, ALVSROW, &
                        ALIRROW, SFCTROW, SFCUROW, SFCVROW, SFCQROW, &
                        FSGVROW, FSGSROW, FSGGROW, FLGVROW, FLGSROW, &
                        FLGGROW, HFSCROW, HFSSROW, HFSGROW, HEVCROW, &
                        HEVSROW, HEVGROW, HMFCROW, HMFNROW, HTCCROW, &
                        HTCSROW, PCFCROW, PCLCROW, PCPNROW, PCPGROW, &
                        QFGROW, QFNROW, QFCLROW, QFCFROW, ROFROW, &
                        ROFOROW, ROFSROW, ROFBROW, TROFROW, TROOROW, &
                        TROSROW, TROBROW, ROFCROW, ROFNROW, ROVGROW, &
                        WTRCROW, WTRSROW, WTRGROW, DRROW, WTABROW, &
                        ILMOROW, UEROW, HBLROW, HMFGROW, HTCROW, &
                        QFCROW, FSNOROW, ITCTROW, NCOUNT, ireport, &
                        wfo_seq, YEAR_NOW, ensim_MONTH, ensim_DAY, &
                        HOUR_NOW, bi%xxx, bi%yyy, bi%NA, &
                        bi%NTYPE, DELT, TFREZ, UVGRD, SBC, RHOW, CURREC, &
                        M_C, M_S, M_R, &
                        WF_ROUTETIMESTEP, WF_R1, WF_R2, bi%NAA, bi%iyMin, &
                        bi%iyMax, bi%jxMin, bi%jxMax, bi%IAK, bi%IROUGH, &
                        bi%ICHNL, bi%NEXT, bi%IREACH, bi%AL, bi%GRDN, bi%GRDE, &
                        bi%DA, bi%BNKFLL, bi%SLOPE_CHNL, bi%ELEV, bi%FRAC, &
                        WF_NO, WF_NL, WF_MHRD, WF_KT, WF_IY, WF_JX, &
                        WF_QHYD, WF_RES, WF_RESSTORE, WF_NORESV_CTRL, WF_R, &
                        WF_NORESV, WF_NREL, WF_KTR, WF_IRES, WF_JRES, WF_RESNAME, &
                        WF_B1, WF_B2, WF_QREL, WF_QR, &
                        WF_TIMECOUNT, WF_NHYD, WF_QBASE, WF_QI1, WF_QI2, WF_QO1, WF_QO2, &
                        WF_STORE1, WF_STORE2, &
                        DRIVERTIMESTEP, ROFGRD, &
                        WF_S, &
                        TOTAL_ROFACC, TOTAL_ROFOACC, TOTAL_ROFSACC, &
                        TOTAL_ROFBACC, TOTAL_EVAPACC, TOTAL_PREACC, INIT_STORE, &
                        FINAL_STORE, TOTAL_AREA, TOTAL_HFSACC, TOTAL_QEVPACC, &
                        SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, NMELT, t0_ACC, &
                        CO2CONC, COSZS, XDIFFUSC, CFLUXCG, CFLUXCS, &
                        AILCG, AILCGS, FCANC, FCANCS, CO2I1CG, CO2I1CS, CO2I2CG, CO2I2CS, &
                        SLAI, FCANCMX, ANCSVEG, ANCGVEG, RMLCSVEG, RMLCGVEG, &
                        AILC, PAIC, FIELDSM, WILTSM, &
                        RMATCTEM, RMATC, NOL2PFTS, ICTEMMOD, L2MAX, ICTEM, &
                        hp%fetchROW, hp%HtROW, hp%N_SROW, hp%A_SROW, hp%DistribROW, &
                        fetchGAT, HtGAT, N_SGAT, A_SGAT, DistribGAT)
    end if !(SAVERESUMEFLAG == 1) then

!> *********************************************************************
!> Call save_init_prog_variables_class.f90 to save initi prognostic variables by
!> by fields needd by classas as initial conditions
!> *********************************************************************
!> bjd - July 14, 2014: Gonzalo Sapriza
    if (SAVERESUMEFLAG == 3) then
!> Save the last time step
        call save_init_prog_variables_class(CMAIROW, QACROW, TACROW, &
                                            TBASROW, TSFSROW, WSNOROW, &
                                            cp%ALBSROW, cp%GROROW, cp%RCANROW, &
                                            cp%RHOSROW, cp%SCANROW, cp%SNOROW, &
                                            cp%TBARROW, cp%TCANROW, cp%THICROW, &
                                            cp%THLQROW, cp%TPNDROW, cp%TSNOROW, &
                                            cp%ZPNDROW, &
                                            bi%NA, bi%NTYPE, bi%IGND, &
                                            fls)
    end if !(SAVERESUMEFLAG == 3) then

    if (OUTFIELDSFLAG == 1) call write_outputs(vr, ts, iof, bi, fls)

    if (ENDDATA) print *, 'Reached end of forcing data'
    if (ENDDATE) print *, 'Reached end of simulation date'

!> Calculate final storage
    FINAL_STORE = 0.0
    do i = 1, bi%NML
        if (bi%FRAC(bi%ILMOS(i)) >= 0.0) then
!-        DO M = 1, NMTEST
            FINAL_STORE = FINAL_STORE + FAREGAT(i)*(RCANGAT(i) + SCANGAT(i) + SNOGAT(i) + WSNOGAT(i) + ZPNDGAT(i)*RHOW)
            do j = 1, bi%IGND
                FINAL_STORE = FINAL_STORE + FAREGAT(i)*(THLQGAT(i, j)*RHOW + THICGAT(i, j)*RHOICE)*DLZWGAT(i, j)
            end do
!-        END DO
        end if
    end do

    !> write out final totals to screen
    if (ro%VERBOSEMODE > 0) then

        print *
        print 5641, 'Total Precipitation         (mm) =', TOTAL_PREACC/TOTAL_AREA
        print 5641, 'Total Evaporation           (mm) =', TOTAL_EVAPACC/TOTAL_AREA
        print 5641, 'Total Runoff                (mm) =', TOTAL_ROFACC/TOTAL_AREA
        print 5641, 'Storage (Change/Init/Final) (mm) =', (FINAL_STORE - INIT_STORE)/TOTAL_AREA, INIT_STORE/TOTAL_AREA, &
            FINAL_STORE/TOTAL_AREA
        print *
        print 5641, 'Total Overland flow         (mm) =', TOTAL_ROFOACC/TOTAL_AREA
        print 5641, 'Total Interflow             (mm) =', TOTAL_ROFSACC/TOTAL_AREA
        print 5641, 'Total Baseflow              (mm) =', TOTAL_ROFBACC/TOTAL_AREA
        print *

5641    format(3x, a34, 999(f11.3))
5635    format(1x, 'Program has terminated normally.'/)

    end if !(ro%VERBOSEMODE > 0) then

    print 5635

    !> Write final totals to output file.
    if (MODELINFOOUTFLAG > 0) then

        write(58, *)
        write(58, '(a, f11.3)') '  Total Precipitation         (mm) = ', TOTAL_PREACC/TOTAL_AREA
        write(58, '(a, f11.3)') '  Total Evaporation           (mm) = ', TOTAL_EVAPACC/TOTAL_AREA
        write(58, '(a, f11.3)') '  Total Runoff                (mm) = ', TOTAL_ROFACC/TOTAL_AREA
        write(58, '(a, 3f11.3)') '  Storage(Change/Init/Final)  (mm) = ', &
            (FINAL_STORE - INIT_STORE)/TOTAL_AREA, &
            INIT_STORE/TOTAL_AREA, &
            FINAL_STORE/TOTAL_AREA
        write(58, '(a, f11.3)') '  Total Overland flow         (mm) = ', TOTAL_ROFOACC/TOTAL_AREA
        write(58, '(a, f11.3)') '  Total Interflow             (mm) = ', TOTAL_ROFSACC/TOTAL_AREA
        write(58, '(a, f11.3)') '  Total Baseflow              (mm) = ', TOTAL_ROFBACC/TOTAL_AREA
        write(58, *)
        write(58, *)
        write(58, '(a)') 'Program has terminated normally.'
        write(58, *)

        call cpu_time(endprog)
        write(58, "('Time = ', e14.6, ' seconds.')") (endprog - startprog)

    end if !(MODELINFOOUTFLAG > 0) then

199 continue

    if (mtsflg%AUTOCALIBRATIONFLAG > 0) call stats_write()

999 continue
!> Diane      close(21)
!>      close(22)
    close(51)

    !todo++:
    !todo++: CUT OUT CLASS ACCUMULATION AND OUTPUT FILES TO APPROPRIATE NODE
    !todo++:
    !> Close the CLASS output files if the GAT-index of the output point resides on this node.
    do i = 1, WF_NUM_POINTS
        if (op%K_OUT(i) >= il1 .and. op%K_OUT(i) <= il2) then
            close(150 + i*10 + 1)
            close(150 + i*10 + 2)
            close(150 + i*10 + 3)
            close(150 + i*10 + 4)
            close(150 + i*10 + 5)
            close(150 + i*10 + 6)
            close(150 + i*10 + 7)
            close(150 + i*10 + 8)
            close(150 + i*10 + 9)
            close(150 + i*10 + 10)
        end if
    end do

    !> Close model output file.
    close(58)

    !> Close CSV streamflow files.
    close(fls%fl(mfk%f70)%iun)
    close(71)
    close(72)

    !> Close the SWE CSV files.
    close(85)
    close(86)

    !> Close the legacy binary format forcing files.
    close(90)
    close(91)
    close(92)
    close(93)
    close(94)
    close(95)
    close(96)

    !> Close the CSV energy and water balance output files.
    close(fls%fl(mfk%f900)%iun)
    close(901)
    close(902)

9000    format(/1x, 'INTERPOLATIONFLAG IS NOT SPECIFIED CORRECTLY AND IS SET TO 0 BY THE MODEL.', &
               /1x, '0: NO INTERPOLATION OF FORCING DATA.', &
               /1x, '1: LINEARLY INTERPOLATES FORCING DATA FOR INTERMEDIATE TIME STEPS.', &
               /1x, 'NOTE: INTERPOLATIONFLAG SHOULD BE SET TO 0 FOR 30 MINUTE FORCING DATA.', /)

9002    format(/1x, 'ERROR IN READING r2c_output.txt FILE.', &
               /1x, 'THE FIRST RECORD AT THE FIRST LINE IS FOR THE NUMBER OF ALL THE', &
               /1x, 'VARIABLES LISTED IN THE r2c_output.txt FILE.',&
               /1x, 'THE SECOND RECORD AT THE FIRST LINE IS TIME STEP FOR R2C OUTPUT.', &
               /1x, 'IT SHOULD BE AN INTEGER MULTIPLE OF 30.',&
               /1x, 'THE REMAINING RECORDS SHOULD CONTAIN 3 COLUMNS FOR EACH VARIABLE WITH', &
               /1x, 'INTEGER VALUES OF EITHER 0 OR 1,', &
               /1x, 'AND 3 COLUMNS CONTAINING INFORMATION ABOUT THE VARIABLES.', /)

    call mpi_finalize(ierr)

    stop

end program
