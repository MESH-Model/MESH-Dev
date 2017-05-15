module RUNCLASS36_variables

    implicit none

    !* HOURLY_START_*: Start day/year for recording hourly averaged data
    !* HOURLY_STOP_*: Stop day/year for recording hourly averaged data
    !* DAILY_START_*: Start day/year for recording daily averaged data
    !* DAILY_STOP_*: Stop day/year for recording daily averaged data
!-    integer HOURLY_START_DAY, HOURLY_STOP_DAY, DAILY_START_DAY, &
!-        DAILY_STOP_DAY
!-    integer HOURLY_START_YEAR, HOURLY_STOP_YEAR, DAILY_START_YEAR, &
!-        DAILY_STOP_YEAR

    !> Temporary variables.
!todo: remove these.
!-    integer JLAT
!-    real DEGLAT, DEGLON

    !> CLASS diagnostic output starting and stopping dates.
    !* JOUT1: DAILY-AVERAGED OUTPUT START DAY (JULIAN FROM YEAR START)
    !* JOUT2: DAILY-AVERAGED OUTPUT STOP DAY (JULIAN FROM YEAR START)
    !* JAV1: DAILY-AVERAGED OUTPUT START YEAR
    !* JAV2: DAILY-AVERAGED OUTPUT STOP YEAR
    !* KOUT1: YEARLY-AVERAGED OUTPUT START DAY (JULIAN FROM YEAR START)
    !* KOUT2: YEARLY-AVERAGED OUTPUT STOP DAY (JULIAN FROM YEAR START)
    !* KAV1: YEARLY-AVERAGED OUTPUT START YEAR
    !* KAV2: YEARLY-AVERAGED OUTPUT STOP YEAR
!-    integer JOUT1, JOUT2, JAV1, JAV2, KOUT1, KOUT2, KAV1, KAV2
!-    integer IHOUR, IMIN, IDAY, IYEAR

    !> CTEM-related fields (inactive in current version of CLASS).
!todo: isolate CTEM from CLASS code (if possible).
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

    !> WATROF variables.
!todo: isolate WATROF from CLASS code.
    !* DD (DDEN): Drainage density.
    !* MANN (WFSF): Manning's n.
    real, dimension(:), allocatable :: DDGAT, MANNGAT
!todo: remove these.
    real, dimension(:, :), allocatable :: BTC, BCAP, DCOEFF, BFCAP, &
        BFCOEFF, BFMIN, BQMAX

    !>PBSM parameters.
!todo: isolate PBSM from CLASS code.
    !* fetch: fetch distance (m)
    !* Ht: vegetation height (m)
    !* N_S: vegetation density (number/m^2)
    !* A_S: vegetation width (m)
    !* Distrib: Inter-GRU snow redistribution factor
    real, dimension(:), allocatable :: &
        fetchGAT, HtGAT, N_SGAT, A_SGAT, DistribGAT

    !> PBSM variables.
    !* DrySnow: 0 = air temperature above 0 degC
    !*          1 = air temperature below 0 degC
    !* SnowAge: hours since last snowfall
    !* Drift: blowing snow transport (kg/m^2)
    !* Subl: blowing snow sublimation (kg/m^2)
    real, dimension(:), allocatable :: DrySnowGAT, SnowAgeGAT, &
        TSNOdsGAT, RHOSdsGAT, DriftGAT, SublGAT, DepositionGAT
    real, dimension(:, :), allocatable :: DrySnowROW, SnowAgeROW, &
        TSNOdsROW, RHOSdsROW, DriftROW, SublROW, DepositionROW

    !> PBSM internal/local variables.
    real, dimension(:), allocatable :: ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
        HCPSCS, HCPSGS, HCPSC, HCPSG, TSNOWC, TSNOWG, &
        RHOSC, RHOSG, XSNOWC, XSNOWG, XSNOCS, XSNOGS

    !> FROZENSOILINIFLAG variables.
!todo: isolate from CLASS code if possible.
    integer NMELT, NYEARS
    real SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS
    integer, dimension(:), allocatable :: INFILTYPE
    real, dimension(:), allocatable :: SI, TSI, SNOWMELTD, SNOWMELTD_LAST, &
        SNOWINFIL, CUMSNOWINFILCS, MELTRUNOFF, CUMSNOWINFILGS, t0_ACC

    !> PDMROF.
!todo: isolate PDMROF from CLASS code
    real ZPND, FSTR
    real, dimension(:), allocatable   :: CMINPDM, CMAXPDM, BPDM, K1PDM, K2PDM, &
        ZPNDPRECS, ZPONDPREC, ZPONDPREG, ZPNDPREGS, &
        UM1CS, UM1C, UM1G, UM1GS, &
        QM1CS, QM1C, QM1G, QM1GS, &
        QM2CS, QM2C, QM2G, QM2GS, UMQ, &
        FSTRCS, FSTRC, FSTRG, FSTRGS

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
        integer, dimension(:), allocatable :: IGDR
        integer, dimension(:, :), allocatable :: &
            IORG, ISND
        real, dimension(:, :), allocatable :: &
            BI, CLAY, DELZW, GRKS, HCPS, ORGM, PSIS, PSIW, SAND, TCS, &
            THFC, THM, THP, THR, THRA, ZBTW

        !> Dimension: NML, ICAN
        real, dimension(:, :), allocatable :: &
            ACID, ACVD, CMAS, HGTD, PAID, PAMN, PAMX, PSGA, PSGB, &
            QA50, ROOT, RSMN, VPDA, VPDB

        !> Dimension: NML, ICP1
        real, dimension(:, :), allocatable :: &
            ALIC, ALVC, FCAN, LNZ0
    end type

    type CLASS_atmospheric_variables
        real, dimension(:), allocatable :: &
            CSZ, DLON, FCLO, GC, GGEO, PADR, RADJ, RHOA, RHSI, RPCP, &
            RPRE, SPCP, SPRE, TADP, TRPC, TSPC, VPD, Z0OR, ZBLD, ZDH, &
            ZDM, ZRFH, ZRFM
!            FDL, FSIH, FSVH, PRE, PRES, QA, TA, UL, UV, VL
    end type

    type CLASS_diagnostic_variables
        integer ISUM
        integer, dimension(:, :, :), allocatable :: ITCT
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

!> variables for reading parameters_class.ini
!-    type ClassParameters

        !> Atmospheric and grid-constant variables.
!-        real, dimension(:), allocatable :: &
!-            ZRFMGRD, ZRFHGRD, ZBLDGRD, GCGRD

        !> Vegetation canopy.
!-        real, dimension(:, :, :), allocatable :: &
!-            FCANROW, LNZ0ROW, ALVCROW, ALICROW, PAMXROW, PAMNROW, &
!-            CMASROW, ROOTROW, RSMNROW, QA50ROW, VPDAROW, VPDBROW, &
!-            PSGAROW, PSGBROW

        !> Soil texture (for default hydraulic parameters).
!-        real, dimension(:, :, :), allocatable :: &
!-            SANDROW, CLAYROW, ORGMROW

        !> Hydraulic parameters.
!WATROF: DDROW
!WATROF: MANNROW
!-        real, dimension(:, :), allocatable :: &
!-            DRNROW, XSLPROW, XDROW, SDEPROW, DDROW, MANNROW

        !> Land-surface variables.
!-        integer, dimension(:, :), allocatable :: MIDROW
!-        real, dimension(:, :), allocatable :: FAREROW

        !> Initial prognostic variables.
!-        real, dimension(:, :), allocatable :: &
!-            TCANROW, TSNOROW, TPNDROW, ZPNDROW, RCANROW, SCANROW, &
!-            SNOROW, ALBSROW, RHOSROW, GROROW, KSROW
!-        real, dimension(:, :, :), allocatable :: &
!-            TBARROW, THLQROW, THICROW
!-    end type

    !> For hydraulic parameters read from soil.ini.
!-    type SoilValues
!-        real, dimension(:, :), allocatable :: &
!-            wc_algwet, wc_algdry
!-        real, dimension(:, :, :), allocatable :: &
!-            wc_thpor, wc_thlret, &
!-            wc_thlmin, wc_bi, wc_psisat, wc_grksat, wc_hcps, wc_tcs
!-    end type

!todo: Move this?
    type HydrologyParameters
        real, dimension(:,:), allocatable :: &
!-            ZSNLROW, ZPLSROW, ZPLGROW, &
            FRZCROW, &
            CMAXROW, CMINROW, BROW, K1ROW, K2ROW, &
            fetchROW, HtROW, N_SROW, A_SROW, DistribROW
    end type

    !> Type: RUNCLASS36_Flags
    !* PROCESS_ACTIVE: Flag to enable CLASS.
    type RUNCLASS36_flags
        logical :: PROCESS_ACTIVE = .true.
    end type

!-    type(ClassParameters), save :: cp
!-    type(SoilValues), save :: sv
!todo: may need to move these.
    type(HydrologyParameters), save :: hp
    type(RUNCLASS36_flags), save :: RUNCLASS36_flgs

!todo: may need to move these.
    type(CLASS_forcing_input), save :: cfi
    type(CLASS_prognostic_variables), save :: cpv
    type(CLASS_surface_variables), save :: csfv
    type(CLASS_atmospheric_variables), save :: catv
    type(CLASS_diagnostic_variables), save :: cdv

end module
