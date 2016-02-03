module process_CLASS_config

    use process_CLASS_constants
    use process_CLASS_variables

    implicit none

!>  INTEGER CONSTANTS.
!INTEGER,PARAMETER :: ICAN=4, IGND=6, ICP1=ICAN+1
!    integer, parameter :: ICAN = 4, ICP1 = ICAN + 1, ICTEM = 1 !Number of CTEM vegetation categories (set to 1 if not using CTEM)

!* HOURLY_START_*: Start day/year for recording hourly averaged data
!* HOURLY_STOP_*: Stop day/year for recording hourly averaged data
!* DAILY_START_*: Start day/year for recording daily averaged data
!* DAILY_STOP_*: Stop day/year for recording daily averaged data
!    integer HOURLY_START_DAY, HOURLY_STOP_DAY, DAILY_START_DAY, &
!        DAILY_STOP_DAY
!    integer HOURLY_START_YEAR, HOURLY_STOP_YEAR, DAILY_START_YEAR, &
!        DAILY_STOP_YEAR

!> LAND SURFACE DIAGNOSTIC VARIABLES.

    real, dimension(:), allocatable :: SNOGRD

!>  CONSTANTS AND TEMPORARY VARIABLES.
!    real DEGLAT, DEGLON
    real FSDOWN1, FSDOWN2, FSDOWN3, RDAY, &
        DECL, HOUR, COSZ
!        ALTOT, FSSTAR, FLSTAR, QH, QE, BEG, SNOMLT, ZSN, TCN, TSN, TPN, GTOUT
!    integer JLAT

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

!* ICAN: MAXIMUM ALLOWABLE NUMBER OF LAND COVER TYPES
!* ICP1: MAXIMUM ALLOWABLE NUMBER OF LAND COVER TYPES INCLUDING
!*       URBAN AREAS

!* N: COUNTER USED BY CLASS
!* NCOUNT: HALF-HOURLY BASED TIME STEP (200 LOOP)
!* NSUM: NUMBER OF ITERATIONS, TIME STEPS PASSED (200 LOOP)
!* NSUM_TOTAL: total number of iterations
!    integer N, NCOUNT, NSUM

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
!    integer JOUT1, JOUT2, JAV1, JAV2, KOUT1, KOUT2, KAV1, KAV2

!> CLASS CONTROL FLAGS:
!> DAN  CONSIDER INCLUDING AS CONTROL FLAGS IN RUN_OPTIONS.INI FILE SO
!> DAN  THAT THEY ARE NO LONGER HARD-CODED.
!* ALL: DESCRIPTIONS ARE WRITTEN WHERE RUN_OPTIONS.INI IS READ
!    integer IDISP, IZREF, ISLFD, IPCP, IWF, IPAI, IHGT, IALC, &
!        IALS, IALG, ITG, ITC, ITCG

    integer NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI

!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
!* TBAR: INITIAL SOIL LAYER TEMPERATURE
!* THLQ: INITIAL SOIL LAYER LIQUID WATER CONTENT
!* THIC: INITIAL SOIL LAYER ICE WATER CONTENT
    real, dimension(:, :), allocatable :: TBARGAT, THLQGAT, THICGAT, &
        SANDGAT, CLAYGAT
    real, dimension(:, :), allocatable :: TBASROW, &
        CMAIROW, TACROW, QACROW, WSNOROW

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

!* SAND: PERCENT-CONTENT OF SAND IN SOIL LAYER (CLASS.INI)
!* CLAY: PERCENT-CONTENT OF CLAY IN SOIL LAYER (CLASS.INI)
!* ORGM: PERCENT-CONTENT OF ORGANIC MATTER IN SOIL LAYER (CLASS.INI)

!* MIDROW: DEFINITION IN CLASS DOCUMENTATION (CLASS.INI)

    integer, dimension(:, :, :), allocatable :: ISNDROW, IORG
    integer, dimension(:, :), allocatable :: ISNDGAT
    integer, dimension(:,:), allocatable :: IGDRROW
    integer, dimension(:), allocatable :: IGDRGAT

!> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
    real, dimension(:), allocatable :: ZDMGRD, &
        ZDHGRD, RADJGRD, CSZGRD, &
        PADRGRD, VPDGRD, &
        TADPGRD, RHOAGRD, RPCPGRD, TRPCGRD, SPCPGRD, TSPCGRD, RHSIGRD, &
        FCLOGRD, DLONGRD, Z0ORGRD, GGEOGRD, UVGRD, XDIFFUS, &
        RPREGRD, SPREGRD, VMODGRD

    real, dimension(:), allocatable :: ZRFMGAT, ZRFHGAT, ZDMGAT, &
        ZDHGAT, ZBLDGAT, RADJGAT, CSZGAT, &
        RPREGAT, SPREGAT, &
        PADRGAT, VPDGAT, TADPGAT, RHOAGAT, RPCPGAT, TRPCGAT, SPCPGAT, &
        TSPCGAT, RHSIGAT, FCLOGAT, DLONGAT, Z0ORGAT, GGEOGAT, VMODGAT

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
!    character(4) TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, &
!        TITLE6, NAME1, NAME2, NAME3, NAME4, NAME5, NAME6, &
!        PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6

!> OUTPUT VARIABLES:
!> THE SUFFIX "ACC" REFERS TO THE ACCUMULATOR ARRAYS USED IN
!> CALCULATING TIME AVERAGES.
!* ALL: DEFINITIONS IN CLASS DOCUMENTATION
    real, dimension(:), allocatable :: PREACC, GTACC, QEVPACC, &
        HFSACC, ROFACC, SNOACC, ALVSACC, ALIRACC, FSINACC, FLINACC, &
        TAACC, UVACC, PRESACC, QAACC, EVAPACC, FLUTACC, ROFOACC, &
        ROFSACC, ROFBACC, HMFNACC, WTBLACC, WSNOACC, RHOSACC, TSNOACC, &
        TCANACC, RCANACC, SCANACC, GROACC, CANARE, SNOARE, ZPNDACC
    real, dimension(:, :), allocatable :: TBARACC, THLQACC, THICACC, &
        THALACC, THLQ_FLD, THIC_FLD, GFLXACC

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
!    integer ICTEMMOD
    integer L2MAX

!> COMMON BLOCK PARAMETERS (CLASS):
!    integer K1, K2, K3, K4, K5, K6, K7, K8, K9, K10, K11
!    real X1, X2, X3, X4, G, GAS, X5, X6, CPRES, GASV, X7, CPI, X8, &
!        CELZRO, X9, X10, X11, X12, X13, X14, X15, SIGMA, X16, DELTIM, &
!        DELT, TFREZ, RGAS, RGASV, GRAV, SBC, VKC, CT, VMIN, TCW, TCICE, &
!        TCSAND, TCCLAY, TCOM, TCDRYS, RHOSOL, RHOOM, HCPW, HCPICE, &
!        HCPSOL, HCPOM, HCPSND, HCPCLY, SPHW, SPHICE, SPHVEG, SPHAIR, &
!        RHOW, RHOICE, TCGLAC, CLHMLT, CLHVAP, PI, ZOLNG, ZOLNS, ZOLNI, &
!        ZORATG, ALVSI, ALIRI, ALVSO, ALIRO, ALBRCK, DELTA, CGRAV, &
!        CKARM, CPD, AS, ASX, CI, BS, BETA, FACTN, HMIN, ANGMAX
!    real, dimension(ICAN) :: CANEXT, XLEAF, ZORAT
!    real, dimension(3) :: THPORG, THRORG, THMORG, BORG, PSISORG, &
!        GRKSORG
!    real, dimension(18, 4, 2) :: GROWYR

!> WATROF FLAGS AND VARIABLES:
!* VICEFLG: VERTICAL ICE FLAG OR LIMIT
!* HICEFLG: HORIZONTAL ICE FLAG OR LIMIT
!    integer LZFFLG, EXTFLG, IWFICE, ERRFLG, IWFOFLW
!    real VICEFLG, PSI_LIMIT, HICEFLG
!* DD (DDEN): DRAINAGE DENSITY (CLASS.INI)
!* MANN (WFSF): MANNING'S n (CLASS.INI)
    real, dimension(:), allocatable :: DDGAT, MANNGAT
    real, dimension(:, :), allocatable :: BTC, BCAP, DCOEFF, BFCAP, &
        BFCOEFF, BFMIN, BQMAX

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
!* PBSM parameters
!  fetch: fetch distance (m)
!  Ht: vegetation height (m)
!  N_S:vegetation density (number/m^2)
!  A_S: vegetation width (m)
!  Distrib: Inter-GRU snow redistribution factor
    real, dimension(:), allocatable :: &
        fetchGAT, HtGAT, N_SGAT, A_SGAT, DistribGAT

    integer NMELT
    real SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS
    integer, dimension(:), allocatable :: INFILTYPE
    real, dimension(:), allocatable :: SI, TSI, SNOWMELTD, SNOWMELTD_LAST, &
        SNOWINFIL, CUMSNOWINFILCS, MELTRUNOFF, CUMSNOWINFILGS

!* PDMROF
    real ZPND, FSTR
    real, dimension(:), allocatable :: &
        CMINPDM, CMAXPDM, BPDM, K1PDM, K2PDM, &
        ZPNDPRECS, ZPONDPREC, ZPONDPREG, ZPNDPREGS, &
        UM1CS, UM1C, UM1G, UM1GS, &
        QM1CS, QM1C, QM1G, QM1GS, &
        QM2CS, QM2C, QM2G, QM2GS, UMQ, &
        FSTRCS, FSTRC, FSTRG, FSTRGS

!> GRID OUTPUT POINTS
!* BNAM: TEMPORARY HOLD FOR OUTPUT DIRECTORY (12 CHARACTER STRING)
!    character(12) BNAM
!* WF_NUM_POINTS: NUMBER OF GRID OUTPUTS
!* I_OUT: OUTPUT GRID SQUARE TEMPORARY STORE
!    integer WF_NUM_POINTS, I_OUT

!    type CLASSOUT_VARS
!        real, dimension(:), allocatable :: &
!            PREACC, GTACC, QEVPACC, EVAPACC, HFSACC, HMFNACC, &
!            ROFACC, ROFOACC, ROFSACC, ROFBACC, WTBLACC, ALVSACC, ALIRACC, &
!            RHOSACC, TSNOACC, WSNOACC, SNOARE, TCANACC, CANARE, SNOACC, &
!            RCANACC, SCANACC, GROACC, FSINACC, FLINACC, FLUTACC, &
!            TAACC, UVACC, PRESACC, QAACC
!        real, dimension(:, :), allocatable :: &
!            TBARACC, THLQACC, THICACC, THALACC, GFLXACC
!    end type !CLASSOUT_VARS

!    type CLASS_prognostic_variables
!        real, dimension(:), allocatable :: ALBS, CMAI, GRO, QAC, RCAN, RHOS, SNCAN, SNO, TAC, TBAR, TBAS, &
!                                           TCAN, THIC, THLQ, TPND, TSFS, TSNO, WSNO, ZPND
!    end type

!    type CLASS_averaged_variables
!        real, dimension(:), allocatable :: ALIR, ALVS, EVAP, FSIN, FLIN, FLUT, GRO, BBGT, HFS, HMFN, OVR, &
!                                           PRE, PRES, QA, QEVP, RCAN, RHOS, ROF, SNCAN, SNO, TA, TBAR, &
!                                           THAL, THIC, THLQ, TCAN, TSNO, UV, WSNO, WTBL
!    end type

!!variables for reading parameters_class.ini
!    type ClassParameters
       !> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
!        real, dimension(:), allocatable :: ZRFMGRD, &
!                                           ZRFHGRD, ZBLDGRD, GCGRD
!> CANOPY AND SOIL INFORMATION (CLASS):
!        real, dimension(:, :, :), allocatable :: &
!            FCANROW, LNZ0ROW, ALVCROW, ALICROW, PAMXROW, PAMNROW, &
!            CMASROW, ROOTROW, RSMNROW, QA50ROW, VPDAROW, VPDBROW, PSGAROW, &
!            PSGBROW
!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
!* DD (DDEN): DRAINAGE DENSITY (CLASS.INI)
!* MANN (WFSF): MANNING'S n (CLASS.INI)
!        real, dimension(:, :), allocatable :: DRNROW, XSLPROW, &
!                                              XDROW, SDEPROW, FAREROW, DDROW, MANNROW, TCANROW, &
!                                              TSNOROW, TPNDROW, ZPNDROW, RCANROW, SCANROW, SNOROW, &
!                                              ALBSROW, RHOSROW, GROROW, KSROW
!* MIDROW: DEFINITION IN CLASS DOCUMENTATION (CLASS.INI)
!        integer, dimension(:, :), allocatable :: MIDROW
!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
!* TBAR: INITIAL SOIL LAYER TEMPERATURE
!* THLQ: INITIAL SOIL LAYER LIQUID WATER CONTENT
!* THIC: INITIAL SOIL LAYER ICE WATER CONTENT
!* SAND: PERCENT-CONTENT OF SAND IN SOIL LAYER (CLASS.INI)
!* CLAY: PERCENT-CONTENT OF CLAY IN SOIL LAYER (CLASS.INI)
!* ORGM: PERCENT-CONTENT OF ORGANIC MATTER IN SOIL LAYER (CLASS.INI)
!        real, dimension(:, :, :), allocatable :: &
!            SANDROW, CLAYROW, ORGMROW, &
!            TBARROW, THLQROW, THICROW
!    end type

!> These are the types defined in mesh_input_module.f that contain arrays
!> that need to be allocated in read_initial_inputs.f.
!    type(OutputPoints), save :: op
!    type(ClassParameters), save :: cp
!    type(CLASSOUT_VARS), save :: co

!    common /PARAMS/ X1, X2, X3, X4, G, GAS, X5, X6, CPRES, &
!        GASV, X7
!    common /PARAM1/ CPI, X8, CELZRO, X9, X10, X11
!    common /PARAM3/ X12, X13, X14, X15, SIGMA, X16
!    common /TIMES/ DELTIM, K1, K2, K3, K4, K5, K6, K7, K8, K9, &
!        K10, K11

!> THE FOLLOWING COMMON BLOCKS ARE DEFINED SPECIFICALLY FOR USE
!> IN CLASS, VIA BLOCK DATA AND THE SUBROUTINE "CLASSD".
!    common /CLASS1/ DELT, TFREZ
!    common /CLASS2/ RGAS, RGASV, GRAV, SBC, VKC, CT, VMIN
!    common /CLASS3/ TCW, TCICE, TCSAND, TCCLAY, TCOM, TCDRYS, &
!        RHOSOL, RHOOM
!    common /CLASS4/ HCPW, HCPICE, HCPSOL, HCPOM, HCPSND, &
!        HCPCLY, SPHW, SPHICE, SPHVEG, SPHAIR, RHOW, &
!        RHOICE, TCGLAC, CLHMLT, CLHVAP
!    common /CLASS5/ THPORG, THRORG, THMORG, BORG, PSISORG, &
!        GRKSORG
!    common /CLASS6/ PI, GROWYR, ZOLNG, ZOLNS, ZOLNI, ZORAT, &
!        ZORATG
!    common /CLASS7/ CANEXT, XLEAF
!    common /CLASS8/ ALVSI, ALIRI, ALVSO, ALIRO, ALBRCK
!    common /PHYCON/ DELTA, CGRAV, CKARM, CPD
!    common /CLASSD2/ AS, ASX, CI, BS, BETA, FACTN, HMIN, ANGMAX

!> THE FOLLOWING COMMON BLOCKS ARE DEFINED FOR WATROF
!    data VICEFLG/3.0/, PSI_LIMIT/1.0/, HICEFLG/1.0/, LZFFLG/0/, &
!        EXTFLG/0/, IWFICE/3/, ERRFLG/1/

    contains

    subroutine RUNCLASS_ini(shd, fls, ts, ic, cm)

        use module_mpi_flags
        use module_mpi_shared_variables
        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_dates
        use climate_forcing
        use MESH_INPUT_MODULE, only: GetIndices

        !> For CLASS output.
        use process_CLASS_save_output

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(iter_counter) :: ic
        type(clim_info) :: cm

        integer NA, NTYPE, NML, IGND, m, j, i, ierr

        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        IGND = shd%lc%IGND

!>=======================================================================
!> INITIALIZE CLASS VARIABLES
!> SET COMMON CLASS PARAMETERS.
        call CLASSD
!>
!>*******************************************************************
!>
!    call READ_INITIAL_INPUTS( &
!>GENERIC VARIABLES
!                             RELEASE, &
!>VARIABLES FOR READ_RUN_OPTIONS
!                             IDISP, IZREF, ISLFD, IPCP, IWF, &
!                             IPAI, IHGT, IALC, IALS, IALG, ITG, ITC, ITCG, &
!                             ICTEMMOD, IOS, PAS, N, IROVAL, WF_NUM_POINTS, &
!  IYEAR_START, IDAY_START, IHOUR_START, IMIN_START, &
!  IYEAR_END,IDAY_END, IHOUR_END, IMIN_END, &
!                             IRONAME, GENDIR_OUT, &
!>variables for READ_PARAMETERS_CLASS
!                             TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6, &
!                             NAME1, NAME2, NAME3, NAME4, NAME5, NAME6, &
!                             PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6, &
!                             shd%wc%ILG, NLTEST, NMTEST, JLAT, ICAN, &
!                             DEGLAT, DEGLON, &
!                             HOURLY_START_DAY, HOURLY_STOP_DAY, &
!                             DAILY_START_DAY, DAILY_STOP_DAY, &
!                             HOURLY_START_YEAR, HOURLY_STOP_YEAR, &
!                             DAILY_START_YEAR, DAILY_STOP_YEAR, &
 !>variables for READ_SOIL_INI
 !>variables for READ_PARAMETERS_HYDROLOGY
!                             INDEPPAR, DEPPAR, WF_R2, M_C, &
 !>the types that are to be allocated and initialised
!                             shd, op, sl, cp, sv, hp, ts, cm, &
!                             SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, fls)

        allocate(cp%ZRFMGRD(NA), cp%ZRFHGRD(NA), cp%ZBLDGRD(NA), &
                 cp%GCGRD(NA))

        allocate(cp%FCANROW(NA, NTYPE, ICAN + 1), cp%LNZ0ROW(NA, NTYPE, ICAN + 1), &
                 cp%ALVCROW(NA, NTYPE, ICAN + 1), cp%ALICROW(NA, NTYPE, ICAN + 1))

        allocate(cp%PAMXROW(NA, NTYPE, ICAN), cp%PAMNROW(NA, NTYPE, ICAN), &
                 cp%CMASROW(NA, NTYPE, ICAN), cp%ROOTROW(NA, NTYPE, ICAN), &
                 cp%RSMNROW(NA, NTYPE, ICAN), cp%QA50ROW(NA, NTYPE, ICAN), &
                 cp%VPDAROW(NA, NTYPE, ICAN), cp%VPDBROW(NA, NTYPE, ICAN), &
                 cp%PSGAROW(NA, NTYPE, ICAN), cp%PSGBROW(NA, NTYPE, ICAN))

        allocate(cp%DRNROW(NA, NTYPE),  cp%SDEPROW(NA, NTYPE), cp%FAREROW(NA, NTYPE), cp%DDROW(NA, NTYPE), &
                 cp%XSLPROW(NA, NTYPE), cp%XDROW(NA, NTYPE), cp%MANNROW(NA, NTYPE), cp%KSROW(NA, NTYPE), &
                 cp%TCANROW(NA, NTYPE), cp%TSNOROW(NA, NTYPE), cp%TPNDROW(NA, NTYPE), cp%ZPNDROW(NA, NTYPE), &
                 cp%RCANROW(NA, NTYPE), cp%SCANROW(NA, NTYPE), cp%SNOROW(NA, NTYPE),  cp%ALBSROW(NA, NTYPE), &
                 cp%RHOSROW(NA, NTYPE), cp%GROROW(NA, NTYPE))

        allocate(cp%MIDROW(NA, NTYPE))

        allocate(cp%SANDROW(NA, NTYPE, IGND), cp%CLAYROW(NA, NTYPE, IGND), cp%ORGMROW(NA, NTYPE, IGND), &
                 cp%TBARROW(NA, NTYPE, IGND), cp%THLQROW(NA, NTYPE, IGND), cp%THICROW(NA, NTYPE, IGND))
!>
!>*******************************************************************
!>
        call READ_PARAMETERS_CLASS(shd, fls)

!>
!>***********************************************************************
!> MAM - Check for parameter values - all parameters should lie within the
!> specified ranges in the "minmax_parameters.txt" file.
!>=======================================================================
!>
!    call check_parameters(WF_R2, M_C, NMTEST, cp, hp, soil_por_max, soil_depth, s0, t_ice_lens)

!> CLASS requires that each GRU for each grid square has its own parameter value,
!> for MESH the value read in from the parameter file is assumed to be valid for
!> all grid squares in the study area - Frank Seglenieks Aug 2007

!> bjd - This would be a good spot for setting pre-distributed values

        do i = 2, NA
            cp%ZRFMGRD(i) = cp%ZRFMGRD(1)
            cp%ZRFHGRD(i) = cp%ZRFHGRD(1)
            cp%ZBLDGRD(i) = cp%ZBLDGRD(1)
            cp%GCGRD(i) = cp%GCGRD(1)
            do m = 1, NTYPE
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
                do j = 1, IGND
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
!- FARE is set using
!-            cp%FAREROW(i, m) = cp%FAREROW(1, m)
                cp%MANNROW(i, m) = cp%MANNROW(1, m)
!> note, if drdn (drainage density) is provided from the Mesh_drainage_database.r2c
!> we give the same value for all the GRU that are in one cell
                if (allocated(shd%SLOPE_INT)) then
                    cp%XSLPROW(i, m) = shd%SLOPE_INT(i)
                    if (i == 2) then
                        cp%XSLPROW(i - 1, m) = shd%SLOPE_INT(i - 1)
                    end if
                else
                    cp%XSLPROW(i, m) = cp%XSLPROW(1, m)
                end if
                cp%XDROW(i, m) = cp%XDROW(1, m)
!> note, if drdn (drainage density) is provided from the Mesh_drainage_database.r2c
!> we give the same value for all the GRU that are in one cell
                if (allocated(shd%DRDN)) then
                    if (i == 2) then
                        cp%DDROW(i - 1, m) = shd%DRDN(i - 1)
                    end if
                    cp%DDROW(i, m) = shd%DRDN(i)
                else
                    cp%DDROW(i, m) = cp%DDROW(1, m)
                end if
!-            WFSFROW(i, m) = WFSFROW(1, m)
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
        end do !i = 2, NA

!     * GATHER-SCATTER COUNTS:
        allocate(shd%lc%ILMOS(shd%lc%ILG), shd%lc%JLMOS(shd%lc%ILG), shd%wc%ILMOS(shd%wc%ILG), &
                 shd%wc%JLMOS(shd%wc%ILG), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'gather-scatter count'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            stop
        end if

!> Set value of FAREROW:
!todo - flag this as an issue to explore later and hide basin average code
!todo - document the problem
!    TOTAL_AREA = 0.0
    cp%FAREROW = 0.0
    do i = 1, NA
        do m = 1, NTYPE
            cp%FAREROW(i, m) = shd%lc%ACLASS(i, m)*shd%FRAC(i)
!            TOTAL_AREA = TOTAL_AREA + cp%FAREROW(i, m)
    !FUTUREDO: Bruce, FRAC is calculated by EnSim
    ! using Dan Princz's instructions for EnSim
    ! FRAC can be greater than 1.00
    ! So, we cannot use FAREROW in place of BASIN_FRACTION
        end do
    end do

    call GATPREP(shd%lc%ILMOS, shd%lc%JLMOS, shd%wc%ILMOS, shd%wc%JLMOS, &
                 shd%lc%NML, shd%wc%NML, cp%GCGRD, cp%FAREROW, cp%MIDROW, &
                 NA, NTYPE, shd%lc%ILG, 1, NA, NTYPE)

    NML = shd%lc%NML

!todo+++: Perhaps land-unit indexing can be done prior in the sequence
!todo+++: of initialization, after reading the drainage database.
!todo+++: Then, variables could be allocated (il1:il2) instead of
!todo+++: (1:ILG) to reduce the memory footprint of the model per node.
!> *********************************************************************
!> Calculate Indices
!> *********************************************************************

    call GetIndices(inp, izero, ipid, NML, shd%lc%ILMOS, il1, il2, ilen)
    if (ro%DIAGNOSEMODE > 0) print 1062, ipid, NML, ilen, il1, il2

1062 format(/1x, 'Configuration and distribution of the domain', &
            /3x, 'Current process: ', i10, &
            /3x, 'Tile land elements: ', i10, &
            /3x, 'Length of single array: ', i10, &
            /3x, 'Starting index: ', i10, &
            /3x, 'Stopping index: ', i10, /)

!>=======================================================================
!> ALLOCATE ALL VARIABLES

!> ANDY * Allocate some variables
        allocate(SNOGRD(NA))

1114 format(/1x, 'Error allocating ', a, ' variables.', &
            /1x, 'Check that these bounds are within an acceptable range.', /)
1118 format(3x, a, ': ', i6)

!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
        allocate(TBARGAT(NML, IGND), &
                 THLQGAT(NML, IGND), THICGAT(NML, IGND), &
                 SANDGAT(NML, IGND), CLAYGAT(NML, IGND), &
                 TBASROW(NA, NTYPE), &
                 CMAIROW(NA, NTYPE), TACROW(NA, NTYPE), &
                 QACROW(NA, NTYPE), WSNOROW(NA, NTYPE), &
                 TPNDGAT(NML), ZPNDGAT(NML), TBASGAT(NML), &
                 ALBSGAT(NML), TSNOGAT(NML), RHOSGAT(NML), &
                 SNOGAT(NML), TCANGAT(NML), RCANGAT(NML), &
                 SCANGAT(NML), &
                 GROGAT(NML), FRZCGAT(NML), CMAIGAT(NML), TACGAT(NML), &
                 QACGAT(NML), WSNOGAT(NML), &
                 TSFSROW(NA, NTYPE, 4), &
                 TSFSGAT(NML, 4), stat = ierr)

!> PBSM PROGNOSTIC VARIABLES
        allocate(DrySnowROW(NA, NTYPE), SnowAgeROW(NA, NTYPE), &
                 DrySnowGAT(NML), SnowAgeGAT(NML), &
                 TSNOdsROW(NA, NTYPE), RHOSdsROW(NA, NTYPE), &
                 TSNOdsGAT(NML), RHOSdsGAT(NML), &
                 DriftROW(NA, NTYPE), SublROW(NA, NTYPE), DepositionROW(NA, NTYPE), &
                 DriftGAT(NML), SublGAT(NML), DepositionGAT(NML), &
                 ZSNOCS(NML), ZSNOGS(NML), &
                 ZSNOWC(NML), ZSNOWG(NML), &
                 HCPSCS(NML), HCPSGS(NML), &
                 HCPSC(NML), HCPSG(NML), &
                 TSNOWC(NML), TSNOWG(NML), &
                 RHOSC(NML), RHOSG(NML), &
                 XSNOWC(NML), XSNOWG(NML), &
                 XSNOCS(NML), XSNOGS(NML), stat = ierr)

        allocate(FCANGAT(NML, ICP1), LNZ0GAT(NML, ICP1), &
                 ALVCGAT(NML, ICP1), ALICGAT(NML, ICP1), &
                 PAIDROW(NA, NTYPE, ICAN), &
                 HGTDROW(NA, NTYPE, ICAN), ACVDROW(NA, NTYPE, ICAN), &
                 ACIDROW(NA, NTYPE, ICAN), &
                 PAMXGAT(NML, ICAN), PAMNGAT(NML, ICAN), &
                 CMASGAT(NML, ICAN), ROOTGAT(NML, ICAN), &
                 RSMNGAT(NML, ICAN), QA50GAT(NML, ICAN), &
                 VPDAGAT(NML, ICAN), VPDBGAT(NML, ICAN), &
                 PSGAGAT(NML, ICAN), &
                 PSGBGAT(NML, ICAN), PAIDGAT(NML, ICAN), &
                 HGTDGAT(NML, ICAN), ACVDGAT(NML, ICAN), &
                 ACIDGAT(NML, ICAN), &
                 THPROW(NA, NTYPE, IGND), THRROW(NA, NTYPE, IGND), &
                 THMROW(NA, NTYPE, IGND), &
                 BIROW(NA, NTYPE, IGND), PSISROW(NA, NTYPE, IGND), &
                 GRKSROW(NA, NTYPE, IGND), THRAROW(NA, NTYPE, IGND), &
                 HCPSROW(NA, NTYPE, IGND), TCSROW(NA, NTYPE, IGND), &
                 THFCROW(NA, NTYPE, IGND), &
                 PSIWROW(NA, NTYPE, IGND), DLZWROW(NA, NTYPE, IGND), &
                 ZBTWROW(NA, NTYPE, IGND), &
                 THPGAT(NML, IGND), THRGAT(NML, IGND), &
                 THMGAT(NML, IGND), &
                 BIGAT(NML, IGND), PSISGAT(NML, IGND), &
                 GRKSGAT(NML, IGND), THRAGAT(NML, IGND), &
                 HCPSGAT(NML, IGND), TCSGAT(NML, IGND), &
                 THFCGAT(NML, IGND), &
                 PSIWGAT(NML, IGND), DLZWGAT(NML, IGND), &
                 ZBTWGAT(NML, IGND), GFLXGAT(NML, IGND), &
                 WFSFROW(NA, NTYPE),  ALGWROW(NA, NTYPE), &
                 ALGDROW(NA, NTYPE), ASVDROW(NA, NTYPE), ASIDROW(NA, NTYPE), &
                 AGVDROW(NA, NTYPE), &
                 AGIDROW(NA, NTYPE), &
                 DRNGAT(NML), XSLPGAT(NML), XDGAT(NML), &
                 WFSFGAT(NML), KSGAT(NML), ALGWGAT(NML), &
                 ALGDGAT(NML), ASVDGAT(NML), ASIDGAT(NML), &
                 AGVDGAT(NML), &
                 AGIDGAT(NML), ZSNLGAT(NML), ZPLGGAT(NML), &
                 ZPLSGAT(NML), SDEPGAT(NML), FAREGAT(NML), &
                 ISNDROW(NA, NTYPE, IGND), IORG(NA, NTYPE, IGND), &
                 ISNDGAT(NML, IGND), IGDRROW(NA,NTYPE), &
                 IGDRGAT(NML), &
                 fetchGAT(NML), HtGAT(NML), N_SGAT(NML), A_SGAT(NML), &
                 DistribGAT(NML), stat = ierr)

        if (ierr /= 0) then
            print 1114, 'canopy and soil info.'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Canopy types with urban areas', ICP1
            print 1118, 'Canopy types', ICAN
            print 1118, 'Soil layers', IGND
            stop
        end if

!> WATROF FLAGS AND VARIABLES:
        allocate(DDGAT(NML), MANNGAT(NML), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'WATROF'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            stop
        end if

!> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
        allocate(ZDMGRD(NA), &
                 ZDHGRD(NA), RADJGRD(NA), &
                 CSZGRD(NA), &
                 PADRGRD(NA), VPDGRD(NA), &
                 TADPGRD(NA), RHOAGRD(NA), RPCPGRD(NA), TRPCGRD(NA), &
                 SPCPGRD(NA), TSPCGRD(NA), RHSIGRD(NA), &
                 FCLOGRD(NA), DLONGRD(NA), Z0ORGRD(NA), GGEOGRD(NA), UVGRD(NA), &
                 XDIFFUS(NA), &
                 RPREGRD(NA), SPREGRD(NA), VMODGRD(NA), &
                 ZRFMGAT(NML), ZRFHGAT(NML), ZDMGAT(NML), &
                 ZDHGAT(NML), ZBLDGAT(NML), &
                 RADJGAT(NML), CSZGAT(NML), &
                 RPREGAT(NML), SPREGAT(NML), &
                 PADRGAT(NML), VPDGAT(NML), &
                 TADPGAT(NML), RHOAGAT(NML), RPCPGAT(NML), &
                 TRPCGAT(NML), SPCPGAT(NML), TSPCGAT(NML), &
                 RHSIGAT(NML), &
                 FCLOGAT(NML), DLONGAT(NML), Z0ORGAT(NML), &
                 GGEOGAT(NML), VMODGAT(NML), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'atmospheric and grid-cst.'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            stop
        end if

!> LAND SURFACE DIAGNOSTIC VARIABLES:
        allocate(CDHROW(NA, NTYPE), CDMROW(NA, NTYPE), &
                 HFSROW(NA, NTYPE), &
                 TFXROW(NA, NTYPE), QEVPROW(NA, NTYPE), QFSROW(NA, NTYPE), &
                 QFXROW(NA, NTYPE), PETROW(NA, NTYPE), GAROW(NA, NTYPE), &
                 EFROW(NA, NTYPE), GTROW(NA, NTYPE), &
                 QGROW(NA, NTYPE), TSFROW(NA, NTYPE), ALVSROW(NA, NTYPE), &
                 ALIRROW(NA, NTYPE), FSNOROW(NA, NTYPE), SFCTROW(NA, NTYPE), &
                 SFCUROW(NA, NTYPE), &
                 SFCVROW(NA, NTYPE), SFCQROW(NA, NTYPE), FSGVROW(NA, NTYPE), &
                 FSGSROW(NA, NTYPE), FSGGROW(NA, NTYPE), FLGVROW(NA, NTYPE), &
                 FLGSROW(NA, NTYPE), &
                 FLGGROW(NA, NTYPE), HFSCROW(NA, NTYPE), HFSSROW(NA, NTYPE), &
                 HFSGROW(NA, NTYPE), HEVCROW(NA, NTYPE), HEVSROW(NA, NTYPE), &
                 HEVGROW(NA, NTYPE), &
                 HMFCROW(NA, NTYPE), HMFNROW(NA, NTYPE), HTCCROW(NA, NTYPE), &
                 HTCSROW(NA, NTYPE), PCFCROW(NA, NTYPE), PCLCROW(NA, NTYPE), &
                 PCPNROW(NA, NTYPE), &
                 PCPGROW(NA, NTYPE), QFGROW(NA, NTYPE), QFNROW(NA, NTYPE), &
                 QFCLROW(NA, NTYPE), QFCFROW(NA, NTYPE), ROFROW(NA, NTYPE), &
                 ROFOROW(NA, NTYPE), &
                 ROFSROW(NA, NTYPE), ROFBROW(NA, NTYPE), ROFCROW(NA, NTYPE), &
                 ROFNROW(NA, NTYPE), ROVGROW(NA, NTYPE), WTRCROW(NA, NTYPE), &
                 WTRSROW(NA, NTYPE), &
                 WTRGROW(NA, NTYPE), DRROW(NA, NTYPE), WTABROW(NA, NTYPE), &
                 ILMOROW(NA, NTYPE), UEROW(NA, NTYPE), HBLROW(NA, NTYPE), &
                 TROFROW(NA, NTYPE), &
                 TROOROW(NA, NTYPE), TROSROW(NA, NTYPE), TROBROW(NA, NTYPE), &
                 CDHGAT(NML), CDMGAT(NML), HFSGAT(NML), &
                 TFXGAT(NML), QEVPGAT(NML), QFSGAT(NML), &
                 QFXGAT(NML), PETGAT(NML), GAGAT(NML), &
                 EFGAT(NML), GTGAT(NML), &
                 QGGAT(NML), ALVSGAT(NML), &
                 ALIRGAT(NML), FSNOGAT(NML), SFRHGAT(NML), SFCTGAT(NML), &
                 SFCUGAT(NML), &
                 SFCVGAT(NML), SFCQGAT(NML), FSGVGAT(NML), &
                 FSGSGAT(NML), FSGGGAT(NML), FLGVGAT(NML), &
                 FLGSGAT(NML), &
                 FLGGGAT(NML), HFSCGAT(NML), HFSSGAT(NML), &
                 HFSGGAT(NML), HEVCGAT(NML), HEVSGAT(NML), &
                 HEVGGAT(NML), &
                 HMFCGAT(NML), HMFNGAT(NML), HTCCGAT(NML), &
                 HTCSGAT(NML), PCFCGAT(NML), PCLCGAT(NML), &
                 PCPNGAT(NML), &
                 PCPGGAT(NML), QFGGAT(NML), QFNGAT(NML), &
                 QFCLGAT(NML), QFCFGAT(NML), ROFGAT(NML), &
                 ROFOGAT(NML), &
                 ROFSGAT(NML), ROFBGAT(NML), ROFCGAT(NML), &
                 ROFNGAT(NML), ROVGGAT(NML), WTRCGAT(NML), &
                 WTRSGAT(NML), &
                 WTRGGAT(NML), DRGAT(NML), WTABGAT(NML), &
                 ILMOGAT(NML), UEGAT(NML), HBLGAT(NML), QLWOGAT(NML), &
                 FTEMP(NML),   FVAP(NML),  RIB(NML), TROFGAT(NML), &
                 TROOGAT(NML), TROSGAT(NML), TROBGAT(NML), &
                 CDHGRD(NA), CDMGRD(NA), HFSGRD(NA), &
                 TFXGRD(NA), QEVPGRD(NA), QFSGRD(NA), QFXGRD(NA), PETGRD(NA), &
                 GAGRD(NA), EFGRD(NA), GTGRD(NA), &
                 QGGRD(NA), TSFGRD(NA), ALVSGRD(NA), ALIRGRD(NA), FSNOGRD(NA), &
                 SFCTGRD(NA), SFCUGRD(NA), &
                 SFCVGRD(NA), SFCQGRD(NA), FSGVGRD(NA), FSGSGRD(NA), &
                 FSGGGRD(NA), FLGVGRD(NA), FLGSGRD(NA), &
                 FLGGGRD(NA), HFSCGRD(NA), HFSSGRD(NA), HFSGGRD(NA), &
                 HEVCGRD(NA), HEVSGRD(NA), HEVGGRD(NA), &
                 HMFCGRD(NA), HMFNGRD(NA), HTCCGRD(NA), HTCSGRD(NA), &
                 PCFCGRD(NA), PCLCGRD(NA), PCPNGRD(NA), &
                 PCPGGRD(NA), QFGGRD(NA), QFNGRD(NA), QFCLGRD(NA), QFCFGRD(NA), &
                 ROFGRD(NA), ROFOGRD(NA), &
                 ROFSGRD(NA), ROFBGRD(NA), ROFCGRD(NA), ROFNGRD(NA), &
                 ROVGGRD(NA), WTRCGRD(NA), WTRSGRD(NA), &
                 WTRGGRD(NA), DRGRD(NA), WTABGRD(NA), ILMOGRD(NA), UEGRD(NA), &
                 HBLGRD(NA), &
                 HMFGROW(NA, NTYPE, IGND), HTCROW(NA, NTYPE, IGND), &
                 QFCROW(NA, NTYPE, IGND), GFLXROW(NA, NTYPE, IGND), &
                 HMFGGAT(NML, IGND), HTCGAT(NML, IGND), &
                 QFCGAT(NML, IGND), &
                 HMFGGRD(NA, IGND), HTCGRD(NA, IGND), QFCGRD(NA, IGND), &
                 GFLXGRD(NA, IGND), &
                 ITCTROW(NA, NTYPE, 6, 50), &
                 ITCTGAT(NML, 6, 50), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'land surface diagnostic'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Soil layers', IGND
            stop
        end if

!> CROSS-CLASS VARIABLES (CLASS):
        allocate(TBARC(NML, IGND), TBARG(NML, IGND), &
                 TBARCS(NML, IGND), &
                 TBARGS(NML, IGND), THLIQC(NML, IGND), &
                 THLIQG(NML, IGND), THICEC(NML, IGND), &
                 THICEG(NML, IGND), FROOT(NML, IGND), &
                 HCPC(NML, IGND), HCPG(NML, IGND), &
                 TCTOPC(NML, IGND), TCBOTC(NML, IGND), &
                 TCTOPG(NML, IGND), TCBOTG(NML, IGND), &
                 FC(NML), FG(NML), FCS(NML), &
                 FGS(NML), RBCOEF(NML), &
                 ZSNOW(NML), &
                 FSVF(NML), FSVFS(NML), ALVSCN(NML), &
                 ALIRCN(NML), ALVSG(NML), &
                 ALIRG(NML), ALVSCS(NML), ALIRCS(NML), &
                 ALVSSN(NML), ALIRSN(NML), ALVSGC(NML), &
                 ALIRGC(NML), ALVSSC(NML), &
                 ALIRSC(NML), TRVSCN(NML), TRIRCN(NML), &
                 TRVSCS(NML), TRIRCS(NML), RC(NML), &
                 RCS(NML), FRAINC(NML), &
                 FSNOWC(NML),FRAICS(NML),FSNOCS(NML), &
                 CMASSC(NML), CMASCS(NML), &
                 DISP(NML), DISPS(NML), ZOMLNC(NML), &
                 ZOELNC(NML), ZOMLNG(NML), &
                 ZOELNG(NML), ZOMLCS(NML), ZOELCS(NML), &
                 ZOMLNS(NML), ZOELNS(NML), TRSNOW(NML), &
                 CHCAP(NML), CHCAPS(NML), &
                 GZEROC(NML), GZEROG(NML), GZROCS(NML), &
                 GZROGS(NML), G12C(NML), G12G(NML), &
                 G12CS(NML), G12GS(NML), G23C(NML), &
                 G23G(NML), G23CS(NML), G23GS(NML), &
                 QFREZC(NML), QFREZG(NML), QMELTC(NML), &
                 QMELTG(NML), EVAPC(NML), &
                 EVAPCG(NML), EVAPG(NML), EVAPCS(NML), &
                 EVPCSG(NML), EVAPGS(NML), TCANO(NML), &
                 TCANS(NML), RAICAN(NML), &
                 SNOCAN(NML), RAICNS(NML), SNOCNS(NML), &
                 CWLCAP(NML), CWFCAP(NML), CWLCPS(NML), &
                 CWFCPS(NML), TSNOCS(NML), &
                 TSNOGS(NML), RHOSCS(NML), RHOSGS(NML), &
                 WSNOCS(NML), WSNOGS(NML), TPONDC(NML), &
                 TPONDG(NML), TPNDCS(NML), &
                 TPNDGS(NML), ZPLMCS(NML), ZPLMGS(NML), &
                 ZPLIMC(NML), ZPLIMG(NML), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'cross-CLASS'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Soil layers', IGND
            stop
        end if

!> BALANCE ERRORS (CLASS):
        allocate(CTVSTP(NML), CTSSTP(NML), &
                 CT1STP(NML), &
                 CT2STP(NML), CT3STP(NML), WTVSTP(NML), &
                 WTSSTP(NML), WTGSTP(NML), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'balance error diagnostic'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            stop
        end if

!> CTEM ERRORS (CLASS):
        allocate(CO2CONC(NML), COSZS(NML), XDIFFUSC(NML), CFLUXCG(NML), CFLUXCS(NML), &
                 AILCG(NML, ICTEM), AILCGS(NML, ICTEM), FCANC(NML, ICTEM), FCANCS(NML, ICTEM), &
                 CO2I1CG(NML, ICTEM), CO2I1CS(NML, ICTEM), CO2I2CG(NML, ICTEM), CO2I2CS(NML, ICTEM), &
                 SLAI(NML, ICTEM), FCANCMX(NML, ICTEM), ANCSVEG(NML, ICTEM), ANCGVEG(NML, ICTEM), &
                 RMLCSVEG(NML, ICTEM), RMLCGVEG(NML, ICTEM), &
                 AILC(NML, ICAN), PAIC(NML, ICAN), FIELDSM(NML, IGND), WILTSM(NML, IGND), &
                 RMATCTEM(NML, ICTEM, IGND), RMATC(NML, ICAN, IGND), NOL2PFTS(ICAN), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'CTEM'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Canopy types', ICAN
            print 1118, 'Soil layers', IGND
            print 1118, 'CTEM flag', ICTEM
            stop
        end if

!>    Copy the starting date of input forcing data from CLASS.ini
!>    to the climate variable.
        cm%start_date%year = IYEAR
        cm%start_date%jday = IDAY
        cm%start_date%hour = IHOUR
        cm%start_date%mins = IMIN

!>    Set the starting date to that of the forcing data if none is
!>    provided and intialize the current time-step.
        if (YEAR_START == 0 .and. JDAY_START == 0 .and. MINS_START == 0 .and. HOUR_START == 0) then
            YEAR_START = cm%start_date%year
            JDAY_START = cm%start_date%jday
            HOUR_START = cm%start_date%hour
            MINS_START = cm%start_date%mins
        end if
        YEAR_NOW = YEAR_START
        JDAY_NOW = JDAY_START
        HOUR_NOW = HOUR_START
        MINS_NOW = MINS_START
        TIME_STEP_NOW = MINS_START

        if (WF_NUM_POINTS > 0) then

            allocate(cf%FCLO(NML), cf%FDL(NML), cf%FSIH(NML), cf%FSVH(NML), cf%PRE(NML), cf%PRES(NML), cf%QA(NML), cf%TA(NML), &
                     cf%UL(NML), cf%VL(NML), cf%VMOD(NML), cf%ZBLD(NML), cf%ZRFH(NML), cf%ZRFM(NML))

            allocate(cpv%ALBS(NML), cpv%CMAI(NML), cpv%GRO(NML), cpv%QAC(NML), cpv%RCAN(NML), cpv%RHOS(NML), cpv%SNCAN(NML), &
                     cpv%SNO(NML), cpv%TAC(NML), cpv%TBAS(NML), cpv%TCAN(NML), cpv%TPND(NML), cpv%TSFS(NML), cpv%TSNO(NML), &
                     cpv%WSNO(NML), cpv%ZPND(NML))
            allocate(cpv%TBAR(NML, IGND), cpv%THIC(NML, IGND), cpv%THLQ(NML, IGND))

!            ISUM, ITCT
            allocate(cdv%ALIR(NML), cdv%ALVS(NML), cdv%CDH(NML), cdv%CDM(NML), cdv%DR(NML), cdv%EF(NML), cdv%FCS(NML), &
                     cdv%FGS(NML), cdv%FC(NML), cdv%FG(NML), cdv%FLGG(NML), cdv%FLGS(NML), cdv%FLGV(NML), cdv%FSGG(NML), &
                     cdv%FSGS(NML), cdv%FSGV(NML), cdv%FSNO(NML), cdv%GA(NML), cdv%GTE(NML), cdv%HBL(NML), cdv%HEVC(NML), &
                     cdv%HEVG(NML), cdv%HEVS(NML), cdv%HFS(NML), cdv%HFSC(NML), cdv%HFSG(NML), cdv%HFSS(NML), cdv%HMFC(NML), &
                     cdv%HMFN(NML), cdv%HTCC(NML), cdv%HTCS(NML), cdv%ILMO(NML), cdv%PCFC(NML), cdv%PCLC(NML), cdv%PCPG(NML), &
                     cdv%PCPN(NML), cdv%PET(NML), cdv%QEVP(NML), cdv%QFCF(NML), cdv%QFCL(NML), cdv%QFG(NML), cdv%QFN(NML), &
                     cdv%QFS(NML), cdv%QFX(NML), cdv%QG(NML), cdv%ROF(NML), cdv%ROFB(NML), cdv%ROFC(NML), cdv%ROFN(NML), &
                     cdv%ROFO(NML), cdv%ROFS(NML), cdv%ROVG(NML), cdv%SFCQ(NML), cdv%SFCT(NML), cdv%SFCU(NML), cdv%SFCV(NML), &
                     cdv%TFX(NML), cdv%TROB(NML), cdv%TROF(NML), cdv%TROO(NML), cdv%TROS(NML), cdv%TSF(NML), cdv%UE(NML), &
                     cdv%WTAB(NML), cdv%WTRC(NML), cdv%WTRG(NML), cdv%WTRS(NML))
            allocate(cdv%GFLX(NML, IGND), cdv%HMFG(NML, IGND), cdv%HTC(NML, IGND), cdv%QFC(NML, IGND))

            call CLASSOUT_open_files(shd)
        end if

    end subroutine

end module
