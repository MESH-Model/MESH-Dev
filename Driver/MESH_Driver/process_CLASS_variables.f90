module process_CLASS_variables

    implicit none

!>  INTEGER CONSTANTS.
!INTEGER,PARAMETER :: ICAN=4, IGND=6, ICP1=ICAN+1
!    integer, parameter :: ICAN = 4, ICP1 = ICAN + 1, ICTEM = 1 !Number of CTEM vegetation categories (set to 1 if not using CTEM)

!* HOURLY_START_*: Start day/year for recording hourly averaged data
!* HOURLY_STOP_*: Stop day/year for recording hourly averaged data
!* DAILY_START_*: Start day/year for recording daily averaged data
!* DAILY_STOP_*: Stop day/year for recording daily averaged data
    integer HOURLY_START_DAY, HOURLY_STOP_DAY, DAILY_START_DAY, &
        DAILY_STOP_DAY
    integer HOURLY_START_YEAR, HOURLY_STOP_YEAR, DAILY_START_YEAR, &
        DAILY_STOP_YEAR

!> LAND SURFACE DIAGNOSTIC VARIABLES.

!    real, dimension(:), allocatable :: SNOGRD

!>  CONSTANTS AND TEMPORARY VARIABLES.
    real DEGLAT, DEGLON
!    real FSDOWN1, FSDOWN2, FSDOWN3, RDAY, &
!        DECL, HOUR, COSZ
!        ALTOT, FSSTAR, FLSTAR, QH, QE, BEG, SNOMLT, ZSN, TCN, TSN, TPN, GTOUT
    integer JLAT

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
    integer JOUT1, JOUT2, JAV1, JAV2, KOUT1, KOUT2, KAV1, KAV2
    integer IHOUR, IMIN, IDAY, IYEAR

!> CLASS CONTROL FLAGS:
!> DAN  CONSIDER INCLUDING AS CONTROL FLAGS IN RUN_OPTIONS.INI FILE SO
!> DAN  THAT THEY ARE NO LONGER HARD-CODED.
!* ALL: DESCRIPTIONS ARE WRITTEN WHERE RUN_OPTIONS.INI IS READ
!    integer IDISP, IZREF, ISLFD, IPCP, IWF, IPAI, IHGT, IALC, &
!        IALS, IALG, ITG, ITC, ITCG

!    integer NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI

!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
!* TBAR: INITIAL SOIL LAYER TEMPERATURE
!* THLQ: INITIAL SOIL LAYER LIQUID WATER CONTENT
!* THIC: INITIAL SOIL LAYER ICE WATER CONTENT
!    real, dimension(:, :), allocatable :: TBARGAT, THLQGAT, THICGAT, &
!        SANDGAT, CLAYGAT
!    real, dimension(:, :), allocatable :: TBASROW, &
!        CMAIROW, TACROW, QACROW, WSNOROW

!* TPND: INITIAL PONDING TEMPERATURE (CLASS.INI)
!* ZPND: INITIAL PONDING DEPTH (CLASS.INI)
!* ALBS: ALBEDO OF SNOWPACK (CLASS.INI)
!* TSNO: INITIAL SNOWPACK TEMPERATURE (CLASS.INI)
!* RHOS: DENSITY OF SNOWPACK (CLASS.INI)
!* SNO: SNOWPACK ON CANOPY LAYER (CLASS.INI)
!* TCAN: INITIAL CANOPY TEMPERATURE (CLASS.INI)
!* GRO: VEGETATION GROWTH INDEX (CLASS.INI)
!    real, dimension(:), allocatable :: TPNDGAT, ZPNDGAT, TBASGAT, &
!        ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, TCANGAT, RCANGAT, SCANGAT, &
!        GROGAT, FRZCGAT, CMAIGAT, TACGAT, QACGAT, WSNOGAT

!    real, dimension(:, :, :), allocatable :: TSFSROW
!    real, dimension(:, :), allocatable :: TSFSGAT

!> CANOPY AND SOIL INFORMATION (CLASS):
!> THE LENGTH OF THESE ARRAYS IS DETERMINED BY THE NUMBER
!> OF SOIL LAYERS (3) AND THE NUMBER OF BROAD VEGETATION
!> CATEGORIES (4, OR 5 INCLUDING URBAN AREAS).
!* ALL: DEFINITIONS IN CLASS DOCUMENTATION (CLASS.INI)
!    real, dimension(:, :), allocatable :: FCANGAT, LNZ0GAT, &
!        ALVCGAT, ALICGAT
!    real, dimension(:, :, :), allocatable :: &
!        PAIDROW, HGTDROW, ACVDROW, ACIDROW
!    real, dimension(:, :), allocatable :: PAMXGAT, PAMNGAT, &
!        CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, VPDAGAT, VPDBGAT, PSGAGAT, &
!        PSGBGAT, PAIDGAT, HGTDGAT, ACVDGAT, ACIDGAT
!    real, dimension(:, :, :), allocatable :: THPROW, THRROW, THMROW, &
!        BIROW, PSISROW, GRKSROW, THRAROW, HCPSROW, TCSROW, THFCROW, &
!        PSIWROW, DLZWROW, ZBTWROW
!    real, dimension(:, :), allocatable :: THPGAT, THRGAT, THMGAT, &
!        BIGAT, PSISGAT, GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, THFCGAT, &
!        PSIWGAT, DLZWGAT, ZBTWGAT, GFLXGAT
!    real, dimension(:, :), allocatable :: &
!        WFSFROW, ALGWROW, ALGDROW, ASVDROW, ASIDROW, AGVDROW, &
!        AGIDROW
!    real, dimension(:), allocatable :: DRNGAT, XSLPGAT, XDGAT, &
!        WFSFGAT, KSGAT, ALGWGAT, ALGDGAT, ASVDGAT, ASIDGAT, AGVDGAT, &
!        AGIDGAT, ZSNLGAT, ZPLGGAT, ZPLSGAT, SDEPGAT, FAREGAT

!* SAND: PERCENT-CONTENT OF SAND IN SOIL LAYER (CLASS.INI)
!* CLAY: PERCENT-CONTENT OF CLAY IN SOIL LAYER (CLASS.INI)
!* ORGM: PERCENT-CONTENT OF ORGANIC MATTER IN SOIL LAYER (CLASS.INI)

!* MIDROW: DEFINITION IN CLASS DOCUMENTATION (CLASS.INI)

!    integer, dimension(:, :, :), allocatable :: ISNDROW, IORG
!    integer, dimension(:, :), allocatable :: ISNDGAT
!    integer, dimension(:,:), allocatable :: IGDRROW
!    integer, dimension(:), allocatable :: IGDRGAT

!> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
!    real, dimension(:), allocatable :: ZDMGRD, &
!        ZDHGRD, RADJGRD, CSZGRD, &
!        PADRGRD, VPDGRD, &
!        TADPGRD, RHOAGRD, RPCPGRD, TRPCGRD, SPCPGRD, TSPCGRD, RHSIGRD, &
!        FCLOGRD, DLONGRD, Z0ORGRD, GGEOGRD, UVGRD, XDIFFUS, &
!        RPREGRD, SPREGRD, VMODGRD

!    real, dimension(:), allocatable :: ZRFMGAT, ZRFHGAT, ZDMGAT, &
!        ZDHGAT, ZBLDGAT, RADJGAT, CSZGAT, &
!        RPREGAT, SPREGAT, &
!        PADRGAT, VPDGAT, TADPGAT, RHOAGAT, RPCPGAT, TRPCGAT, SPCPGAT, &
!        TSPCGAT, RHSIGAT, FCLOGAT, DLONGAT, Z0ORGAT, GGEOGAT, VMODGAT

!> LAND SURFACE DIAGNOSTIC VARIABLES:
!    real, dimension(:, :), allocatable :: CDHROW, CDMROW, HFSROW, &
!        TFXROW, QEVPROW, QFSROW, QFXROW, PETROW, GAROW, EFROW, GTROW, &
!        QGROW, TSFROW, ALVSROW, ALIRROW, FSNOROW, SFCTROW, SFCUROW, &
!        SFCVROW, SFCQROW, FSGVROW, FSGSROW, FSGGROW, FLGVROW, FLGSROW, &
!        FLGGROW, HFSCROW, HFSSROW, HFSGROW, HEVCROW, HEVSROW, HEVGROW, &
!        HMFCROW, HMFNROW, HTCCROW, HTCSROW, PCFCROW, PCLCROW, PCPNROW, &
!        PCPGROW, QFGROW, QFNROW, QFCLROW, QFCFROW, ROFROW, ROFOROW, &
!        ROFSROW, ROFBROW, ROFCROW, ROFNROW, ROVGROW, WTRCROW, WTRSROW, &
!        WTRGROW, DRROW, WTABROW, ILMOROW, UEROW, HBLROW, TROFROW, &
!        TROOROW, TROSROW, TROBROW
!    real, dimension(:), allocatable :: CDHGAT, CDMGAT, HFSGAT, &
!        TFXGAT, QEVPGAT, QFSGAT, QFXGAT, PETGAT, GAGAT, EFGAT, GTGAT, &
!        QGGAT, ALVSGAT, ALIRGAT, FSNOGAT, SFRHGAT, SFCTGAT, SFCUGAT, &
!        SFCVGAT, SFCQGAT, FSGVGAT, FSGSGAT, FSGGGAT, FLGVGAT, FLGSGAT, &
!        FLGGGAT, HFSCGAT, HFSSGAT, HFSGGAT, HEVCGAT, HEVSGAT, HEVGGAT, &
!        HMFCGAT, HMFNGAT, HTCCGAT, HTCSGAT, PCFCGAT, PCLCGAT, PCPNGAT, &
!        PCPGGAT, QFGGAT, QFNGAT, QFCLGAT, QFCFGAT, ROFGAT, ROFOGAT, &
!        ROFSGAT, ROFBGAT, ROFCGAT, ROFNGAT, ROVGGAT, WTRCGAT, WTRSGAT, &
!        WTRGGAT, DRGAT, WTABGAT, ILMOGAT, UEGAT, HBLGAT, QLWOGAT, FTEMP, &
!        FVAP, RIB, TROFGAT, TROOGAT, TROSGAT, TROBGAT
!    real, dimension(:), allocatable :: CDHGRD, CDMGRD, HFSGRD, &
!        TFXGRD, QEVPGRD, QFSGRD, QFXGRD, PETGRD, GAGRD, EFGRD, GTGRD, &
!        QGGRD, TSFGRD, ALVSGRD, ALIRGRD, FSNOGRD, SFCTGRD, SFCUGRD, &
!        SFCVGRD, SFCQGRD, FSGVGRD, FSGSGRD, FSGGGRD, FLGVGRD, FLGSGRD, &
!        FLGGGRD, HFSCGRD, HFSSGRD, HFSGGRD, HEVCGRD, HEVSGRD, HEVGGRD, &
!        HMFCGRD, HMFNGRD, HTCCGRD, HTCSGRD, PCFCGRD, PCLCGRD, PCPNGRD, &
!        PCPGGRD, QFGGRD, QFNGRD, QFCLGRD, QFCFGRD, ROFGRD, ROFOGRD, &
!        ROFSGRD, ROFBGRD, ROFCGRD, ROFNGRD, ROVGGRD, WTRCGRD, WTRSGRD, &
!        WTRGGRD, DRGRD, WTABGRD, ILMOGRD, UEGRD, HBLGRD

!    real, dimension(:, :, :), allocatable :: HMFGROW, HTCROW, QFCROW, &
!        GFLXROW
!    real, dimension(:, :), allocatable :: HMFGGAT, HTCGAT, QFCGAT
!    real, dimension(:, :), allocatable :: HMFGGRD, HTCGRD, QFCGRD, GFLXGRD
!    integer, dimension(:, :, :, :), allocatable :: ITCTROW
!    integer, dimension(:, :, :), allocatable :: ITCTGAT

!* TITLE: PROJECT DESCRIPTOR (6 COLUMNS: 4 CHARACTER STRINGS)
!* NAME: AUTHOR, RESEARCHER (6 COLUMNS: 4 CHARACTER STRINGS)
!* PLACE: SITE LOCATION, BASIN (6 COLUMNS: 4 CHARACTER STRINGS)
    character(4) TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, &
        TITLE6, NAME1, NAME2, NAME3, NAME4, NAME5, NAME6, &
        PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6

!> OUTPUT VARIABLES:
!> THE SUFFIX "ACC" REFERS TO THE ACCUMULATOR ARRAYS USED IN
!> CALCULATING TIME AVERAGES.
!* ALL: DEFINITIONS IN CLASS DOCUMENTATION
!    real, dimension(:), allocatable :: PREACC, GTACC, QEVPACC, &
!        HFSACC, ROFACC, SNOACC, ALVSACC, ALIRACC, FSINACC, FLINACC, &
!        TAACC, UVACC, PRESACC, QAACC, EVAPACC, FLUTACC, ROFOACC, &
!        ROFSACC, ROFBACC, HMFNACC, WTBLACC, WSNOACC, RHOSACC, TSNOACC, &
!        TCANACC, RCANACC, SCANACC, GROACC, CANARE, SNOARE, ZPNDACC
!    real, dimension(:, :), allocatable :: TBARACC, THLQACC, THICACC, &
!        THALACC, THLQ_FLD, THIC_FLD, GFLXACC

!> CROSS-CLASS VARIABLES (CLASS):
!> ARRAYS DEFINED TO PASS INFORMATION BETWEEN THE THREE MAJOR
!> SUBSECTIONS OF CLASS ("CLASSA", "CLASST" AND "CLASSW").
!    real, dimension(:, :), allocatable :: TBARC, TBARG, TBARCS, &
!        TBARGS, THLIQC, THLIQG, THICEC, THICEG, FROOT, HCPC, HCPG, &
!        TCTOPC, TCBOTC, TCTOPG, TCBOTG

!    real, dimension(:), allocatable :: FC, FG, FCS, FGS, RBCOEF, &
!        ZSNOW, FSVF, FSVFS, ALVSCN, ALIRCN, ALVSG, &
!        ALIRG, ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, &
!        ALIRSC, TRVSCN, TRIRCN, TRVSCS, TRIRCS, RC, RCS, FRAINC, &
!        FSNOWC, FRAICS, FSNOCS, CMASSC, CMASCS, DISP, DISPS, ZOMLNC, &
!        ZOELNC, ZOMLNG, &
!        ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, TRSNOW, CHCAP, CHCAPS, &
!        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, G12CS, G12GS, G23C, &
!        G23G, G23CS, G23GS, QFREZC, QFREZG, QMELTC, QMELTG, EVAPC, &
!        EVAPCG, EVAPG, EVAPCS, EVPCSG, EVAPGS, TCANO, TCANS, RAICAN, &
!        SNOCAN, RAICNS, SNOCNS, CWLCAP, CWFCAP, CWLCPS, CWFCPS, TSNOCS, &
!        TSNOGS, RHOSCS, RHOSGS, WSNOCS, WSNOGS, TPONDC, TPONDG, TPNDCS, &
!        TPNDGS, ZPLMCS, ZPLMGS, ZPLIMC, ZPLIMG

!> BALANCE ERRORS (CLASS):
!> DIAGNOSTIC ARRAYS USED FOR CHECKING ENERGY AND WATER
!> BALANCES.
!    real, dimension(:), allocatable :: CTVSTP, CTSSTP, CT1STP, &
!        CT2STP, CT3STP, WTVSTP, WTSSTP, WTGSTP

!> CTEM-RELATED FIELDS (NOT USED IN STANDARD OFFLINE CLASS RUNS).
!    real, dimension(:), allocatable :: &
!        CO2CONC, COSZS, XDIFFUSC, CFLUXCG, CFLUXCS
!    real, dimension(:, :), allocatable :: &
!        AILCG, AILCGS, FCANC, FCANCS, CO2I1CG, CO2I1CS, CO2I2CG, CO2I2CS, &
!        SLAI, FCANCMX, ANCSVEG, ANCGVEG, RMLCSVEG, RMLCGVEG, &
!        AILC, PAIC, &
!        FIELDSM, WILTSM
!    real, dimension(:, :, :), allocatable :: &
!        RMATCTEM, RMATC
!    integer, dimension(:), allocatable :: NOL2PFTS
!    integer ICTEMMOD, L2MAX

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

!> DAN * CONFLICTS WITH COMMON BLOCK DEFINITIONS (APR 20/08)
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
!    real, dimension(:), allocatable :: DDGAT, MANNGAT
!    real, dimension(:, :), allocatable :: BTC, BCAP, DCOEFF, BFCAP, &
!        BFCOEFF, BFMIN, BQMAX

!>PBSM VARIABLES (GRU)
!* DrySnow: 0 = air temperature above 0 degC
!*          1 = air temperature below 0 degC
!* SnowAge: hours since last snowfall
!* Drift: blowing snow transport (kg/m^2)
!* Subl: blowing snow sublimation (kg/m^2)
!    real, dimension(:), allocatable :: DrySnowGAT, SnowAgeGAT, &
!        TSNOdsGAT, RHOSdsGAT, DriftGAT, SublGAT, DepositionGAT
!    real, dimension(:, :), allocatable :: DrySnowROW, SnowAgeROW, &
!        TSNOdsROW, RHOSdsROW, DriftROW, SublROW, DepositionROW
!>CLASS SUBAREA VARIABLES NEEDED FOR PBSM
!    real, dimension(:), allocatable :: ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
!        HCPSCS, HCPSGS, HCPSC, HCPSG, TSNOWC, TSNOWG, &
!        RHOSC, RHOSG, XSNOWC, XSNOWG, XSNOCS, XSNOGS
!* PBSM parameters
!  fetch: fetch distance (m)
!  Ht: vegetation height (m)
!  N_S:vegetation density (number/m^2)
!  A_S: vegetation width (m)
!  Distrib: Inter-GRU snow redistribution factor
!    real, dimension(:), allocatable :: &
!        fetchGAT, HtGAT, N_SGAT, A_SGAT, DistribGAT

!    integer NMELT
!    real SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS
!    integer, dimension(:), allocatable :: INFILTYPE
!    real, dimension(:), allocatable :: SI, TSI, SNOWMELTD, SNOWMELTD_LAST, &
!        SNOWINFIL, CUMSNOWINFILCS, MELTRUNOFF, CUMSNOWINFILGS

!* PDMROF
!    real ZPND, FSTR
!    real, dimension(:), allocatable   :: CMINPDM, CMAXPDM, BPDM, K1PDM, K2PDM, &
!        ZPNDPRECS, ZPONDPREC, ZPONDPREG, ZPNDPREGS, &
!        UM1CS, UM1C, UM1G, UM1GS, &
!        QM1CS, QM1C, QM1G, QM1GS, &
!        QM2CS, QM2C, QM2G, QM2GS, UMQ, &
!        FSTRCS, FSTRC, FSTRG, FSTRGS

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

    type CLASS_forcing_input
        real, dimension(:), allocatable :: &
            FDL, FSIH, FSVH, PRE, PRES, QA, TA, UL, VL, VMOD
!            FCLO, ZBLD, ZRFH, ZRFM
    end type

    type CLASS_prognostic_variables
        real, dimension(:), allocatable :: &
            ALBS, CMAI, GRO, QAC, RCAN, RHOS, SNCAN, SNO, TAC, TBAS, &
            TCAN, TPND, TSNO, WSNO, ZPND
        real, dimension(:, :), allocatable :: &
            TBAR, THIC, THLQ, TSFS
    end type

    type CLASS_surface_variables

!WATROF GRKF
!WATROF WFCI
!WATROF WFSF
!WATROF XSLP

        !> Dimension: NML
        real, dimension(:), allocatable :: &
            AGID, AGVD, ALGD, ALGW, ASID, ASVD, DRN, FARE, GRKF, MID, &
            SDEP, WFCI, WFSF, XSLP, ZPLG, ZPLS, ZSNL

        !> Dimension: NML, IGND
        real, dimension(:, :), allocatable :: &
            BI, CLAY, DELZW, GRKS, HCPS, IORG, ISND, ORGM, PSIS, PSIW, &
            SAND, TCS, THFC, THM, THP, THR, THRA, ZBTW

        !> Dimension: NML, ICAN
        real, dimension(:, :), allocatable :: &
            ACID, ACVD, CMAS, HGTD, PAID, PAMN, PAMX, PSGA, PSGB, &
            QA50, ROOT, RSMN, VPDA, VPDB

        !> Dimension: NML, ICP1
        real, dimension(:, :), allocatable :: &
            ALIC, ALVC, FCAN, LNZ0
    end type

    type CLASS_atmospheric_variables
        integer GC
        real, dimension(:), allocatable :: &
            CSZ, DLON, FCLO, GGEO, PADR, RADJ, RHOA, RHSI, RPCP, RPRE, &
            SPCP, SPRE, TADP, TRPC, TSPC, VPD, Z0OR, ZBLD, ZDH, ZDM, &
            ZRFH, ZRFM
!            FDL, FSIH, FSVH, PRE, PRES, QA, TA, UL, UV, VL
    end type

    type CLASS_diagnostic_variables
        integer ISUM, ITCT
        real, dimension(:), allocatable :: &
            ALIR, ALVS, CDH, CDM, DR, EF, FCS, FGS, FC, FG, FLGG, &
            FLGS, FLGV, FSGG, FSGS, FSGV, FSNO, GA, GTE, HBL, HEVC, &
            HEVG, HEVS, HFS, HFSC, HFSG, HFSS, HMFC, HMFN, HTCC, HTCS, &
            ILMO, PCFC, PCLC, PCPG, PCPN, PET, QEVP, QFCF, QFCL, QFG, &
            QFN, QFS, QFX, QG, ROF, ROFB, ROFC, ROFN, ROFO, ROFS, &
            ROVG, SFCQ, SFCT, SFCU, SFCV, TFX, TROB, TROF, TROO, TROS, &
            TSF, UE, WTAB, WTRC, WTRG, WTRS
        real, dimension(:, :), allocatable :: &
            GFLX, HMFG, HTC, QFC
    end type

    type CLASS_averaged_variables
        real, dimension(:), allocatable :: &
            ALIR, ALVS, EVAP, FSIN, FLIN, FLUT, GRO, BBGT, HFS, HMFN, &
            OVR, PRE, PRES, QA, QEVP, RCAN, RHOS, ROF, SNCAN, SNO, TA, &
            TBAR, THAL, THIC, THLQ, TCAN, TSNO, UV, WSNO, WTBL
    end type

!!variables for reading parameters_class.ini
    type ClassParameters
       !> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
        real, dimension(:), allocatable :: ZRFMGRD, &
                                           ZRFHGRD, ZBLDGRD, GCGRD
!> CANOPY AND SOIL INFORMATION (CLASS):
        real, dimension(:, :, :), allocatable :: &
            FCANROW, LNZ0ROW, ALVCROW, ALICROW, PAMXROW, PAMNROW, &
            CMASROW, ROOTROW, RSMNROW, QA50ROW, VPDAROW, VPDBROW, PSGAROW, &
            PSGBROW
!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
!* DD (DDEN): DRAINAGE DENSITY (CLASS.INI)
!* MANN (WFSF): MANNING'S n (CLASS.INI)
        real, dimension(:, :), allocatable :: DRNROW, XSLPROW, &
                                              XDROW, SDEPROW, FAREROW, DDROW, MANNROW, TCANROW, &
                                              TSNOROW, TPNDROW, ZPNDROW, RCANROW, SCANROW, SNOROW, &
                                              ALBSROW, RHOSROW, GROROW, KSROW
!* MIDROW: DEFINITION IN CLASS DOCUMENTATION (CLASS.INI)
        integer, dimension(:, :), allocatable :: MIDROW
!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
!* TBAR: INITIAL SOIL LAYER TEMPERATURE
!* THLQ: INITIAL SOIL LAYER LIQUID WATER CONTENT
!* THIC: INITIAL SOIL LAYER ICE WATER CONTENT
!* SAND: PERCENT-CONTENT OF SAND IN SOIL LAYER (CLASS.INI)
!* CLAY: PERCENT-CONTENT OF CLAY IN SOIL LAYER (CLASS.INI)
!* ORGM: PERCENT-CONTENT OF ORGANIC MATTER IN SOIL LAYER (CLASS.INI)
        real, dimension(:, :, :), allocatable :: &
            SANDROW, CLAYROW, ORGMROW, &
            TBARROW, THLQROW, THICROW
    end type

!> These are the types defined in mesh_input_module.f that contain arrays
!> that need to be allocated in read_initial_inputs.f.
!    type(OutputPoints), save :: op
    type(ClassParameters), save :: cp
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

    type(CLASS_forcing_input), save :: cfi
    type(CLASS_prognostic_variables), save :: cpv
    type(CLASS_surface_variables), save :: csfv
    type(CLASS_atmospheric_variables), save :: catv
    type(CLASS_diagnostic_variables), save :: cdv

end module
