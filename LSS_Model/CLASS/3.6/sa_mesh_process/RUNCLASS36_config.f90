module RUNCLASS36_config

    use RUNCLASS36_constants
    use RUNCLASS36_variables

    implicit none

    !>  CONSTANTS AND TEMPORARY VARIABLES.
    real FSDOWN1, FSDOWN2, FSDOWN3, RDAY, &
        DECL, HOUR, COSZ

    integer NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI
    real, dimension(:), allocatable :: &
        FRZCGAT

    !> CANOPY AND SOIL INFORMATION (CLASS):
    !> THE LENGTH OF THESE ARRAYS IS DETERMINED BY THE NUMBER
    !> OF SOIL LAYERS (3) AND THE NUMBER OF BROAD VEGETATION
    !> CATEGORIES (4, OR 5 INCLUDING URBAN AREAS).
    !* ALL: DEFINITIONS IN CLASS DOCUMENTATION (CLASS.INI)
    real, dimension(:), allocatable :: XDGAT, &
        KSGAT

    !> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
    real, dimension(:), allocatable :: ZDMGRD, &
        ZDHGRD, RADJGRD, CSZGRD, &
        PADRGRD, VPDGRD, &
        TADPGRD, RHOAGRD, RPCPGRD, TRPCGRD, SPCPGRD, TSPCGRD, RHSIGRD, &
        FCLOGRD, DLONGRD, Z0ORGRD, GGEOGRD, UVGRD, XDIFFUS, &
        RPREGRD, SPREGRD, VMODGRD

    !> LAND SURFACE DIAGNOSTIC VARIABLES:
    real, dimension(:), allocatable :: &
        SFRHGAT, &
        QLWOGAT, FTEMP, &
        FVAP, RIB
    real, dimension(:), allocatable :: CDHGRD, CDMGRD, HFSGRD, &
        TFXGRD, QEVPGRD, QFSGRD, QFXGRD, PETGRD, GAGRD, EFGRD, GTGRD, &
        QGGRD, TSFGRD, ALVSGRD, ALIRGRD, FSNOGRD, SFCTGRD, SFCUGRD, &
        SFCVGRD, SFCQGRD, FSGVGRD, FSGSGRD, FSGGGRD, FLGVGRD, FLGSGRD, &
        FLGGGRD, HFSCGRD, HFSSGRD, HFSGGRD, HEVCGRD, HEVSGRD, HEVGGRD, &
        HMFCGRD, HMFNGRD, HTCCGRD, HTCSGRD, PCFCGRD, PCLCGRD, PCPNGRD, &
        PCPGGRD, QFGGRD, QFNGRD, QFCLGRD, QFCFGRD, ROFGRD, ROFOGRD, &
        ROFSGRD, ROFBGRD, ROFCGRD, ROFNGRD, ROVGGRD, WTRCGRD, WTRSGRD, &
        WTRGGRD, DRGRD, WTABGRD, ILMOGRD, UEGRD, HBLGRD

    real, dimension(:, :), allocatable :: HMFGGRD, HTCGRD, QFCGRD, GFLXGRD

    !> CROSS-CLASS VARIABLES (CLASS):
    !> ARRAYS DEFINED TO PASS INFORMATION BETWEEN THE THREE MAJOR
    !> SUBSECTIONS OF CLASS ("CLASSA", "CLASST" AND "CLASSW").
    real, dimension(:), allocatable :: RBCOEF, &
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
    real, dimension(:, :), allocatable :: TBARC, TBARG, TBARCS, &
        TBARGS, THLIQC, THLIQG, THICEC, THICEG, FROOT, HCPC, HCPG, &
        TCTOPC, TCBOTC, TCTOPG, TCBOTG

    !> BALANCE ERRORS (CLASS):
    !> DIAGNOSTIC ARRAYS USED FOR CHECKING ENERGY AND WATER
    !> BALANCES.
    real, dimension(:), allocatable :: CTVSTP, CTSSTP, CT1STP, &
        CT2STP, CT3STP, WTVSTP, WTSSTP, WTGSTP

    contains

    subroutine RUNCLASS36_init(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing
        use FLAGS

        !> For CLASS output.
        use RUNCLASS36_save_output

        use PBSM_module

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        integer NA, NTYPE, NML, NSL, l, k, ik, jk, m, j, i, iun, ierr
        real FRAC

        !> For RESUMEFLAG 3
        real(kind = 4), dimension(:, :), allocatable :: ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
            RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
            TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW
        real(kind = 4), dimension(:, :, :), allocatable :: TBARROW, THICROW, THLQROW, TSFSROW

        !> Return if the process is not marked active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NSL = shd%lc%IGND
        NML = shd%lc%NML
        DELT = ic%dts

        !> ALLOCATE ALL VARIABLES

1114 format(/1x, 'Error allocating ', a, ' variables.', &
            /1x, 'Check that these bounds are within an acceptable range.', /)
1118 format(3x, a, ': ', i6)

        allocate(XDGAT(NML), KSGAT(NML), stat = ierr)

        if (ierr /= 0) then
            print 1114, 'canopy and soil info.'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Canopy types with urban areas', ICP1
            print 1118, 'Canopy types', ICAN
            print 1118, 'Soil layers', NSL
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
                 stat = ierr)
        if (ierr /= 0) then
            print 1114, 'atmospheric and grid-cst.'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            stop
        end if

        !> LAND SURFACE DIAGNOSTIC VARIABLES:
        allocate(SFRHGAT(NML), &
                 QLWOGAT(NML), &
                 FTEMP(NML), FVAP(NML), RIB(NML), &
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
                 HMFGGRD(NA, NSL), HTCGRD(NA, NSL), QFCGRD(NA, NSL), &
                 GFLXGRD(NA, NSL), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'land surface diagnostic'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Soil layers', NSL
            stop
        end if

        !> CROSS-CLASS VARIABLES (CLASS):
        allocate(TBARC(NML, NSL), TBARG(NML, NSL), &
                 TBARCS(NML, NSL), &
                 TBARGS(NML, NSL), THLIQC(NML, NSL), &
                 THLIQG(NML, NSL), THICEC(NML, NSL), &
                 THICEG(NML, NSL), FROOT(NML, NSL), &
                 HCPC(NML, NSL), HCPG(NML, NSL), &
                 TCTOPC(NML, NSL), TCBOTC(NML, NSL), &
                 TCTOPG(NML, NSL), TCBOTG(NML, NSL), &
                 RBCOEF(NML), &
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
            print 1118, 'Soil layers', NSL
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
                 AILC(NML, ICAN), PAIC(NML, ICAN), FIELDSM(NML, NSL), WILTSM(NML, NSL), &
                 RMATCTEM(NML, ICTEM, NSL), RMATC(NML, ICAN, NSL), NOL2PFTS(ICAN), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'CTEM'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            print 1118, 'Total tile elements', NML
            print 1118, 'Canopy types', ICAN
            print 1118, 'Soil layers', NSL
            print 1118, 'CTEM flag', ICTEM
            stop
        end if

        !> Forcing input.
        allocate(cfi%FDL(NML), cfi%FSIH(NML), cfi%FSVH(NML), cfi%PRE(NML), cfi%PRES(NML), cfi%QA(NML), cfi%TA(NML), cfi%UL(NML), &
                 cfi%VL(NML), cfi%VMOD(NML))

        !> Prognostic variables.
        allocate(cpv%ALBS(NML), cpv%CMAI(NML), cpv%GRO(NML), cpv%QAC(NML), cpv%RCAN(NML), cpv%RHOS(NML), cpv%SNCAN(NML), &
                 cpv%SNO(NML), cpv%TAC(NML), cpv%TBAS(NML), cpv%TCAN(NML), cpv%TPND(NML), cpv%TSNO(NML), cpv%WSNO(NML), &
                 cpv%ZPND(NML))
        allocate(cpv%TBAR(NML, NSL), cpv%THIC(NML, NSL), cpv%THLQ(NML, NSL))
        allocate(cpv%TSFS(NML, 4))

        !> Land-surface variables.
        allocate(csfv%AGID(NML), csfv%AGVD(NML), csfv%ALGD(NML), csfv%ALGW(NML), csfv%ASID(NML), csfv%ASVD(NML), csfv%DRN(NML), &
                 csfv%FARE(NML), csfv%GRKF(NML), csfv%MID(NML), csfv%IWF(NML), csfv%SDEP(NML), csfv%WFCI(NML), csfv%WFSF(NML), &
                 csfv%XSLP(NML), csfv%ZPLG(NML), csfv%ZPLS(NML), csfv%ZSNL(NML))
        allocate(csfv%IGDR(NML))
        allocate(csfv%IORG(NML, NSL), csfv%ISND(NML, NSL))
        allocate(csfv%BI(NML, NSL), csfv%CLAY(NML, NSL), csfv%DELZW(NML, NSL), csfv%GRKS(NML, NSL), csfv%HCPS(NML, NSL), &
                 csfv%ORGM(NML, NSL), csfv%PSIS(NML, NSL), csfv%PSIW(NML, NSL), csfv%SAND(NML, NSL), csfv%TCS(NML, NSL), &
                 csfv%THFC(NML, NSL), csfv%THM(NML, NSL), csfv%THP(NML, NSL), csfv%THR(NML, NSL), csfv%THRA(NML, NSL), &
                 csfv%ZBTW(NML, NSL))
        allocate(csfv%ACID(NML, ICAN), csfv%ACVD(NML, ICAN), csfv%CMAS(NML, ICAN), csfv%HGTD(NML, ICAN), csfv%PAID(NML, ICAN), &
                 csfv%PAMN(NML, ICAN), csfv%PAMX(NML, ICAN), csfv%PSGA(NML, ICAN), csfv%PSGB(NML, ICAN), csfv%QA50(NML, ICAN), &
                 csfv%ROOT(NML, ICAN), csfv%RSMN(NML, ICAN), csfv%VPDA(NML, ICAN), csfv%VPDB(NML, ICAN))
        allocate(csfv%ALIC(NML, ICP1), csfv%ALVC(NML, ICP1), csfv%FCAN(NML, ICP1), csfv%LNZ0(NML, ICP1))

        !> Atmospheric variables.
        allocate(catv%CSZ(NML), catv%DLON(NML), catv%FCLO(NML), catv%GC(NML), catv%GGEO(NML), catv%PADR(NML), catv%RADJ(NML), &
                 catv%RHOA(NML), catv%RHSI(NML), catv%RPCP(NML), catv%RPRE(NML), catv%SPCP(NML), catv%SPRE(NML), catv%TADP(NML), &
                 catv%TRPC(NML), catv%TSPC(NML), catv%VPD(NML), catv%Z0OR(NML), catv%ZBLD(NML), catv%ZDH(NML), catv%ZDM(NML), &
                 catv%ZRFH(NML), catv%ZRFM(NML))

        !> Diagnostic variables.
        allocate(cdv%ITCT(NML, 6, 50))
        allocate(cdv%ALIR(NML), cdv%ALVS(NML), cdv%CDH(NML), cdv%CDM(NML), cdv%DR(NML), cdv%EF(NML), cdv%FCS(NML), cdv%FGS(NML), &
                 cdv%FC(NML), cdv%FG(NML), cdv%FLGG(NML), cdv%FLGS(NML), cdv%FLGV(NML), cdv%FSGG(NML), cdv%FSGS(NML), &
                 cdv%FSGV(NML), cdv%FSNO(NML), cdv%GA(NML), cdv%GTE(NML), cdv%HBL(NML), cdv%HEVC(NML), cdv%HEVG(NML), &
                 cdv%HEVS(NML), cdv%HFS(NML), cdv%HFSC(NML), cdv%HFSG(NML), cdv%HFSS(NML), cdv%HMFC(NML), cdv%HMFN(NML), &
                 cdv%HTCC(NML), cdv%HTCS(NML), cdv%ILMO(NML), cdv%PCFC(NML), cdv%PCLC(NML), cdv%PCPG(NML), cdv%PCPN(NML), &
                 cdv%PET(NML), cdv%QEVP(NML), cdv%QFCF(NML), cdv%QFCL(NML), cdv%QFG(NML), cdv%QFN(NML), cdv%QFS(NML), &
                 cdv%QFX(NML), cdv%QG(NML), cdv%ROF(NML), cdv%ROFB(NML), cdv%ROFC(NML), cdv%ROFN(NML), cdv%ROFO(NML), &
                 cdv%ROFS(NML), cdv%ROVG(NML), cdv%SFCQ(NML), cdv%SFCT(NML), cdv%SFCU(NML), cdv%SFCV(NML), cdv%TFX(NML), &
                 cdv%TROB(NML), cdv%TROF(NML), cdv%TROO(NML), cdv%TROS(NML), cdv%TSF(NML), cdv%UE(NML), cdv%WTAB(NML), &
                 cdv%WTRC(NML), cdv%WTRG(NML), cdv%WTRS(NML))
        allocate(cdv%GFLX(NML, NSL), cdv%HMFG(NML, NSL), cdv%HTC(NML, NSL), cdv%QFC(NML, NSL))

        !> Read an initial value for geothermal flux from file.
        if (GGEOFLAG == 1) then
            iun = fls%fl(mfk%f18)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f18)%fn)), status = 'old', action = 'read', iostat = ierr)
            read(iun, *) GGEOGRD(1)
            close(iun)
        else
            GGEOGRD(1) = 0.0
        end if

        !> Initialize PBSM or allocate and initialize variables used in CLASS even if PBSM is not enabled.
        if (pbsm%PROCESS_ACTIVE) then
            call PBSM_init(fls, shd, cm)
        else

            !> Variables used in CLASSZ.
            allocate(pbsm%vs%Drift(NML), pbsm%vs%Subl(NML))
            pbsm%vs%Drift = 0.0; pbsm%vs%Subl = 0.0

            !> Variables used in CLASSW.
            !> These are initialized in WPREP.
            allocate(ZSNOCS(NML), ZSNOGS(NML), ZSNOWC(NML), ZSNOWG(NML), &
                     HCPSCS(NML), HCPSGS(NML), HCPSC(NML), HCPSG(NML), &
                     TSNOWC(NML), TSNOWG(NML), &
                     RHOSC(NML), RHOSG(NML), &
                     XSNOWC(NML), XSNOWG(NML), XSNOCS(NML), XSNOGS(NML))
        end if

        !> Resume the state of prognostic variables from file.
        select case (RESUMEFLAG)

            !> RESUMEFLAG 3.
            case (3)

                !> Open the resume state file.
                iun = fls%fl(mfk%f883)%iun
                open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)), status = 'old', action = 'read', &
                     form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

                !> Allocate temporary variables.
                allocate(ALBSROW(NA, NTYPE), CMAIROW(NA, NTYPE), GROROW(NA, NTYPE), QACROW(NA, NTYPE), RCANROW(NA, NTYPE), &
                         RHOSROW(NA, NTYPE), SCANROW(NA, NTYPE), SNOROW(NA, NTYPE), TACROW(NA, NTYPE), TBASROW(NA, NTYPE), &
                         TCANROW(NA, NTYPE), TPNDROW(NA, NTYPE), TSNOROW(NA, NTYPE), WSNOROW(NA, NTYPE), ZPNDROW(NA, NTYPE), &
                         TBARROW(NA, NTYPE, NSL), THICROW(NA, NTYPE, NSL), THLQROW(NA, NTYPE, NSL), TSFSROW(NA, NTYPE, 4))

                !> Read inital values from the file.
                read(iun) ALBSROW
                read(iun) CMAIROW
                read(iun) GROROW
                read(iun) QACROW
                read(iun) RCANROW
                read(iun) RHOSROW
                read(iun) SCANROW
                read(iun) SNOROW
                read(iun) TACROW
                read(iun) TBARROW
                read(iun) TBASROW
                read(iun) TCANROW
                read(iun) THICROW
                read(iun) THLQROW
                read(iun) TPNDROW
                read(iun) TSFSROW
                read(iun) TSNOROW
                read(iun) WSNOROW
                read(iun) ZPNDROW

                !> Close the file to free the unit.
                close(iun)

                !> Scatter the temporary variables.
                do k = il1, il2

                    !> Grab the grid and GRU of the current tile.
                    ik = shd%lc%ILMOS(k)
                    jk = shd%lc%JLMOS(k)

                    !> Assign values.
                    vs%tile%albs(k) = ALBSROW(ik, jk)
                    vs%tile%cmas(k) = CMAIROW(ik, jk)
                    vs%tile%gro(k) = GROROW(ik, jk)
                    vs%tile%qac(k) = QACROW(ik, jk)
                    vs%tile%rcan(k) = RCANROW(ik, jk)
                    vs%tile%rhos(k) = RHOSROW(ik, jk)
                    vs%tile%sncan(k) = SCANROW(ik, jk)
                    vs%tile%sno(k) = SNOROW(ik, jk)
                    vs%tile%tac(k) = TACROW(ik, jk)
                    vs%tile%tbar(k, :) = TBARROW(ik, jk, :)
                    vs%tile%tbas(k) = TBASROW(ik, jk)
                    vs%tile%tcan(k) = TCANROW(ik, jk)
                    vs%tile%thic(k, :) = THICROW(ik, jk, :)
                    vs%tile%thlq(k, :) = THLQROW(ik, jk, :)
                    vs%tile%tpnd(k) = TPNDROW(ik, jk)
                    vs%tile%tsfs(k, :) = TSFSROW(ik, jk, :)
                    vs%tile%tsno(k) = TSNOROW(ik, jk)
                    vs%tile%wsno(k) = WSNOROW(ik, jk)
                    vs%tile%zpnd(k) = ZPNDROW(ik, jk)

                end do

                !> Deallocate temporary variables.
                deallocate(ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
                           RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
                           TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW, &
                           TBARROW, THICROW, THLQROW, TSFSROW)

            !> RESUMEFLAG 4.
            case (4)
                call read_init_prog_variables_class(fls)

            !> RESUMEFLAG 5.
            case (5)
                call read_init_prog_variables_class(fls)

        end select !case (RESUMEFLAG)

        !> Distribute variables.
        catv%ZRFM(il1:il2) = pm%sfp%zrfm(il1:il2)
        catv%ZRFH(il1:il2) = pm%sfp%zrfh(il1:il2)
        catv%ZBLD(il1:il2) = pm%sfp%zbld(il1:il2)
        catv%GC(il1:il2) = pm%tp%gc(il1:il2)
        csfv%FARE(il1:il2) = pm%tp%fare(il1:il2)
        csfv%MID(il1:il2) = pm%tp%mid(il1:il2)
        csfv%IWF(il1:il2) = pm%tp%iwf(il1:il2)
        csfv%FCAN(il1:il2, :) = pm%cp%fcan(il1:il2, :)
        csfv%LNZ0(il1:il2, :) = pm%cp%lnz0(il1:il2, :)
        csfv%ALVC(il1:il2, :) = pm%cp%alvc(il1:il2, :)
        csfv%ALIC(il1:il2, :) = pm%cp%alic(il1:il2, :)
        csfv%PAMX(il1:il2, :) = pm%cp%lamx(il1:il2, :)
        csfv%PAMN(il1:il2, :) = pm%cp%lamn(il1:il2, :)
        csfv%CMAS(il1:il2, :) = pm%cp%cmas(il1:il2, :)
        csfv%ROOT(il1:il2, :) = pm%cp%root(il1:il2, :)
        csfv%RSMN(il1:il2, :) = pm%cp%rsmn(il1:il2, :)
        csfv%QA50(il1:il2, :) = pm%cp%qa50(il1:il2, :)
        csfv%VPDA(il1:il2, :) = pm%cp%vpda(il1:il2, :)
        csfv%VPDB(il1:il2, :) = pm%cp%vpdb(il1:il2, :)
        csfv%PSGA(il1:il2, :) = pm%cp%psga(il1:il2, :)
        csfv%PSGB(il1:il2, :) = pm%cp%psgb(il1:il2, :)
        csfv%DRN(il1:il2) = pm%hp%drn(il1:il2)
        csfv%SDEP(il1:il2) = pm%slp%sdep(il1:il2)
        csfv%XSLP(il1:il2) = pm%tp%xslp(il1:il2)
        DDGAT(il1:il2) = pm%hp%dd(il1:il2)
        MANNGAT(il1:il2) = pm%hp%mann(il1:il2)
        XDGAT(il1:il2) = pm%hp%grkf(il1:il2)
        KSGAT(il1:il2) = pm%hp%ks(il1:il2)
        csfv%SAND(il1:il2, :) = pm%slp%sand(il1:il2, :)
        csfv%CLAY(il1:il2, :) = pm%slp%clay(il1:il2, :)
        csfv%ORGM(il1:il2, :) = pm%slp%orgm(il1:il2, :)
        cpv%CMAI(il1:il2) = vs%tile%cmas(il1:il2)
        cpv%WSNO(il1:il2) = vs%tile%wsno(il1:il2)
        cpv%QAC(il1:il2) = vs%tile%qac(il1:il2)
        cpv%TCAN(il1:il2) = vs%tile%tcan(il1:il2)
        cpv%TAC(il1:il2) = vs%tile%tac(il1:il2)
        cpv%TSNO(il1:il2) = vs%tile%tsno(il1:il2)
        cpv%TPND(il1:il2) = vs%tile%tpnd(il1:il2)
        cpv%ZPND(il1:il2) = vs%tile%zpnd(il1:il2)
        cpv%RCAN(il1:il2) = vs%tile%rcan(il1:il2)
        cpv%SNCAN(il1:il2) = vs%tile%sncan(il1:il2)
        cpv%SNO(il1:il2) = vs%tile%sno(il1:il2)
        cpv%ALBS(il1:il2) = vs%tile%albs(il1:il2)
        cpv%RHOS(il1:il2) = vs%tile%rhos(il1:il2)
        cpv%GRO(il1:il2) = vs%tile%gro(il1:il2)
        cpv%TSFS(il1:il2, :) = vs%tile%tsfs(il1:il2, :)
        cpv%TBAR(il1:il2, :) = vs%tile%tbar(il1:il2, :)
        cpv%THLQ(il1:il2, :) = vs%tile%thlq(il1:il2, :)
        cpv%THIC(il1:il2, :) = vs%tile%thic(il1:il2, :)
        cpv%TBAS(il1:il2) = vs%tile%tbas(il1:il2)
        csfv%ZSNL(il1:il2) = pm%snp%zsnl(il1:il2)
        csfv%ZPLG(il1:il2) = pm%sfp%zplg(il1:il2)
        csfv%ZPLS(il1:il2) = pm%snp%zpls(il1:il2)

        cdv%ITCT = 0

        !> FROZENSOILINFILFLAG 1.
        allocate(FRZCGAT(NML), stat = ierr)
        allocate(INFILTYPE(NML), SI(NML), TSI(NML), &
                 SNOWMELTD(NML), SNOWMELTD_LAST(NML), SNOWINFIL(NML), &
                 CUMSNOWINFILCS(NML), MELTRUNOFF(NML), CUMSNOWINFILGS(NML))
!todo: move to read_parameters
        if (FROZENSOILINFILFLAG == 1) then
            NMELT = 1
            INFILTYPE = 2 !> INITIALIZED WITH UNLIMITED INFILTRATION
            SNOWMELTD = 0.0
            SNOWINFIL = 0.0
            CUMSNOWINFILCS = 0.0
            CUMSNOWINFILGS = 0.0
            MELTRUNOFF = 0.0
            SI = 0.20
            TSI = -0.10
            do k = il2, il2
                FRZCGAT(k) = hp%FRZCROW(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
            end do
        end if

        !> IWF 2 (PDMROF) and IWF 3 (LATFLOW).
        allocate(&
            CMINPDM(NML), CMAXPDM(NML), BPDM(NML), K1PDM(NML), K2PDM(NML), &
            ZPNDPRECS(NML), ZPONDPREC(NML), ZPONDPREG(NML), ZPNDPREGS(NML), &
            UM1CS(NML), UM1C(NML), UM1G(NML), UM1GS(NML), &
            QM1CS(NML), QM1C(NML), QM1G(NML), QM1GS(NML), &
            QM2CS(NML), QM2C(NML), QM2G(NML), QM2GS(NML), &
            UMQ(NML), &
            FSTRCS(NML), FSTRC(NML), FSTRG(NML), FSTRGS(NML))
        ZPNDPRECS = 0.0; ZPONDPREC = 0.0; ZPONDPREG = 0.0; ZPNDPREGS = 0.0
        UM1CS = 0.0; UM1C = 0.0; UM1G = 0.0; UM1GS = 0.0
        QM1CS = 0.0; QM1C = 0.0; QM1G = 0.0; QM1GS = 0.0
        QM2CS = 0.0; QM2C = 0.0; QM2G = 0.0; QM2GS = 0.0
        UMQ = 0.0
        FSTRCS = 0.0; FSTRC = 0.0; FSTRG = 0.0; FSTRGS = 0.0
        if (any(csfv%IWF == 2) .or. any(csfv%IWF == 3)) then
            do k = il1, il2
                ik = shd%lc%ILMOS(k)
                jk = shd%lc%JLMOS(k)
                CMINPDM(k) = hp%CMINROW(ik, jk)
                CMAXPDM(k) = hp%CMAXROW(ik, jk)
                BPDM(k) = hp%BROW(ik, jk)
                K1PDM(k) = hp%K1ROW(ik, jk)
                K2PDM(k) = hp%K2ROW(ik, jk)
            end do
        end if

        !> *********************************************************************
        !> Call CLASSBG to set more CLASS variables
        !> *********************************************************************
        !> bjd - July 25, 2005: For inputting field measured soil properties.
        call CLASSBG(csfv%THP, csfv%THR, csfv%THM, csfv%BI, csfv%PSIS, csfv%GRKS, &
                     csfv%THRA, csfv%HCPS, csfv%TCS, csfv%THFC, csfv%PSIW, &
                     csfv%DELZW, csfv%ZBTW, csfv%ALGW, csfv%ALGD, &
                     csfv%SAND, csfv%CLAY, csfv%ORGM, shd%lc%sl%DELZ, shd%lc%sl%ZBOT, csfv%SDEP, &
                     csfv%ISND, csfv%IGDR, NML, il1, il2, NSL, ICTEMMOD, &
                     pm_gru%slp%thpor, pm_gru%slp%thlret, pm_gru%slp%thlmin, pm_gru%slp%bi, &
                     pm_gru%slp%psisat, pm_gru%slp%grksat, pm_gru%slp%hcps, pm_gru%slp%tcs, &
                     NA, NTYPE, shd%lc%ILG, shd%lc%ILMOS, shd%lc%JLMOS)

        pm%slp%alwet(il1:il2) = csfv%ALGW(il1:il2)
        pm%slp%aldry(il1:il2) = csfv%ALGD(il1:il2)
        pm%slp%thpor(il1:il2, :) = csfv%THP(il1:il2, :)
        pm%slp%thlret(il1:il2, :) = csfv%THR(il1:il2, :)
        pm%slp%thlmin(il1:il2, :) = csfv%THM(il1:il2, :)
        pm%slp%bi(il1:il2, :) = csfv%BI(il1:il2, :)
        pm%slp%psisat(il1:il2, :) = csfv%PSIS(il1:il2, :)
        pm%slp%grksat(il1:il2, :) = csfv%GRKS(il1:il2, :)
        pm%slp%thlrat(il1:il2, :) = csfv%THRA(il1:il2, :)
        pm%slp%hcps(il1:il2, :) = csfv%HCPS(il1:il2, :)
        pm%slp%tcs(il1:il2, :) = csfv%TCS(il1:il2, :)
        pm%slp%thfc(il1:il2, :) = csfv%THFC(il1:il2, :)
        pm%slp%psiwlt(il1:il2, :) = csfv%PSIW(il1:il2, :)
        vs%tile%delzw(il1:il2, :) = csfv%DELZW(il1:il2, :)
        vs%tile%zbotw(il1:il2, :) = csfv%ZBTW(il1:il2, :)

        !> CLASS output files.
        if (WF_NUM_POINTS > 0) call CLASSOUT_open_files(shd)

        do k = il1, il2
            ik = shd%lc%ILMOS(k)
            catv%RADJ(k) = shd%ylat(ik)*PI/180.0
            catv%DLON(k) = shd%xlng(ik)
        end do
        catv%Z0OR = 0.0
        catv%GGEO(:) = GGEOGRD(1)
        catv%ZDM = 10.0
        catv%ZDH = 2.0

    end subroutine

    subroutine RUNCLASS36_finalize(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use model_dates
        use climate_forcing
        use FLAGS

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> For SAVERESUMEFLAG 3
        real(kind = 4), dimension(:, :), allocatable :: ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
            RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
            TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW
        real(kind = 4), dimension(:, :, :), allocatable :: TBARROW, THICROW, THLQROW, TSFSROW

        integer NA, NTYPE, NSL, k, ik, jk, iun, ierr

        !> Return if the process is not marked active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

        !> Only the head node writes CLASS output.
        if (.not. ipid == 0) return

        !> Local indices.
        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NSL = shd%lc%IGND

        !> Save the state of prognostic variables to file.
        select case (SAVERESUMEFLAG)

            !> SAVERESUMEFLAG 3.
            case (3)

                !> Open the resume state file.
                iun = fls%fl(mfk%f883)%iun
                open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)), status = 'replace', action = 'write', &
                     form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

                !> Allocate and initialize temporary variables.
                allocate(ALBSROW(NA, NTYPE), CMAIROW(NA, NTYPE), GROROW(NA, NTYPE), QACROW(NA, NTYPE), RCANROW(NA, NTYPE), &
                         RHOSROW(NA, NTYPE), SCANROW(NA, NTYPE), SNOROW(NA, NTYPE), TACROW(NA, NTYPE), TBASROW(NA, NTYPE), &
                         TCANROW(NA, NTYPE), TPNDROW(NA, NTYPE), TSNOROW(NA, NTYPE), WSNOROW(NA, NTYPE), ZPNDROW(NA, NTYPE), &
                         TBARROW(NA, NTYPE, NSL), THICROW(NA, NTYPE, NSL), THLQROW(NA, NTYPE, NSL), TSFSROW(NA, NTYPE, 4))
                ALBSROW = 0.0; CMAIROW = 0.0; GROROW = 0.0; QACROW = 0.0; RCANROW = 0.0; RHOSROW = 0.0
                SCANROW = 0.0; SNOROW = 0.0; TACROW = 0.0; TBASROW = 0.0; TCANROW = 0.0; TPNDROW = 0.0
                TSNOROW = 0.0; WSNOROW = 0.0; ZPNDROW = 0.0
                TBARROW = 0.0; THICROW = 0.0; THLQROW = 0.0; TSFSROW = 0.0

                !> Gather the temporary variables.
                do k = 1, shd%lc%NML

                    !> Grab the grid and GRU of the current tile.
                    ik = shd%lc%ILMOS(k)
                    jk = shd%lc%JLMOS(k)

                    !> Assign values.
                    ALBSROW(ik, jk) = vs%tile%albs(k)
                    CMAIROW(ik, jk) = vs%tile%cmas(k)
                    GROROW(ik, jk) = vs%tile%gro(k)
                    QACROW(ik, jk) = vs%tile%qac(k)
                    RCANROW(ik, jk) = vs%tile%rcan(k)
                    RHOSROW(ik, jk) = vs%tile%rhos(k)
                    SCANROW(ik, jk) = vs%tile%sncan(k)
                    SNOROW(ik, jk) = vs%tile%sno(k)
                    TACROW(ik, jk) = vs%tile%tac(k)
                    TBARROW(ik, jk, :) = vs%tile%tbar(k, :)
                    TBASROW(ik, jk) = vs%tile%tbas(k)
                    TCANROW(ik, jk) = vs%tile%tcan(k)
                    THICROW(ik, jk, :) = vs%tile%thic(k, :)
                    THLQROW(ik, jk, :) = vs%tile%thlq(k, :)
                    TPNDROW(ik, jk) = vs%tile%tpnd(k)
                    TSFSROW(ik, jk, :) = vs%tile%tsfs(k, :)
                    TSNOROW(ik, jk) = vs%tile%tsno(k)
                    WSNOROW(ik, jk) = vs%tile%wsno(k)
                    ZPNDROW(ik, jk) = vs%tile%zpnd(k)

                end do

                !> Read inital values from the file.
                write(iun) ALBSROW
                write(iun) CMAIROW
                write(iun) GROROW
                write(iun) QACROW
                write(iun) RCANROW
                write(iun) RHOSROW
                write(iun) SCANROW
                write(iun) SNOROW
                write(iun) TACROW
                write(iun) TBARROW
                write(iun) TBASROW
                write(iun) TCANROW
                write(iun) THICROW
                write(iun) THLQROW
                write(iun) TPNDROW
                write(iun) TSFSROW
                write(iun) TSNOROW
                write(iun) WSNOROW
                write(iun) ZPNDROW

                !> Close the file to free the unit.
                close(iun)

                !> Deallocate temporary variables.
                deallocate(ALBSROW, CMAIROW, GROROW, QACROW, RCANROW, &
                           RHOSROW, SCANROW, SNOROW, TACROW, TBASROW, &
                           TCANROW, TPNDROW, TSNOROW, WSNOROW, ZPNDROW, &
                           TBARROW, THICROW, THLQROW, TSFSROW)

            !> SAVERESUMEFLAG 4.
            case (4)
                call save_init_prog_variables_class(fls)

            !> RESUMEFLAG 5.
            case (5)
                call save_init_prog_variables_class(fls)

        end select !case (SAVERESUMEFLAG)

    end subroutine

end module
