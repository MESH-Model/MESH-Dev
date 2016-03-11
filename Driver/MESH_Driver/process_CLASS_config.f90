module process_CLASS_config

    use process_CLASS_constants
    use process_CLASS_variables

    implicit none

    !> LAND SURFACE DIAGNOSTIC VARIABLES.
    real, dimension(:), allocatable :: SNOGRD

    !>  CONSTANTS AND TEMPORARY VARIABLES.
    real FSDOWN1, FSDOWN2, FSDOWN3, RDAY, &
        DECL, HOUR, COSZ

    integer NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI
!    real, dimension(:, :), allocatable :: TBASROW, &
!        CMAIROW, TACROW, QACROW, WSNOROW
    real, dimension(:), allocatable :: &
        FRZCGAT
!    real, dimension(:, :, :), allocatable :: TSFSROW

    !> CANOPY AND SOIL INFORMATION (CLASS):
    !> THE LENGTH OF THESE ARRAYS IS DETERMINED BY THE NUMBER
    !> OF SOIL LAYERS (3) AND THE NUMBER OF BROAD VEGETATION
    !> CATEGORIES (4, OR 5 INCLUDING URBAN AREAS).
    !* ALL: DEFINITIONS IN CLASS DOCUMENTATION (CLASS.INI)
!    real, dimension(:, :, :), allocatable :: &
!        PAIDROW, HGTDROW, ACVDROW, ACIDROW
!    real, dimension(:, :, :), allocatable :: THPROW, THRROW, THMROW, &
!        BIROW, PSISROW, GRKSROW, THRAROW, HCPSROW, TCSROW, THFCROW, &
!        PSIWROW, DLZWROW, ZBTWROW
!    real, dimension(:, :), allocatable :: &
!        WFSFROW, ALGWROW, ALGDROW, ASVDROW, ASIDROW, AGVDROW, &
!        AGIDROW
    real, dimension(:), allocatable :: XDGAT, &
        KSGAT

!    integer, dimension(:, :, :), allocatable :: ISNDROW, IORG
!    integer, dimension(:,:), allocatable :: IGDRROW
!-    integer, dimension(:), allocatable :: IGDRGAT

    !> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
    real, dimension(:), allocatable :: ZDMGRD, &
        ZDHGRD, RADJGRD, CSZGRD, &
        PADRGRD, VPDGRD, &
        TADPGRD, RHOAGRD, RPCPGRD, TRPCGRD, SPCPGRD, TSPCGRD, RHSIGRD, &
        FCLOGRD, DLONGRD, Z0ORGRD, GGEOGRD, UVGRD, XDIFFUS, &
        RPREGRD, SPREGRD, VMODGRD

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

!    real, dimension(:, :, :), allocatable :: HMFGROW, HTCROW, QFCROW, &
!        GFLXROW
    real, dimension(:, :), allocatable :: HMFGGRD, HTCGRD, QFCGRD, GFLXGRD
!    integer, dimension(:, :, :, :), allocatable :: ITCTROW
!    integer, dimension(:, :, :), allocatable :: ITCTGAT

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

    subroutine RUNCLASS_ini(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

        use module_mpi_flags
        use module_mpi_shared_variables
        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        !> For CLASS output.
        use process_CLASS_save_output
        use MESH_INPUT_MODULE, only: GetIndices

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(iter_counter) :: ic
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        integer NA, NTYPE, NML, IGND, l, k, ik, jk, m, j, i, iun, ierr
        real FRAC

        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        IGND = shd%lc%IGND

        !> INITIALIZE CLASS VARIABLES.
        !> SET COMMON CLASS PARAMETERS.
        call CLASSD

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

        cp%ZRFMGRD = 0.0
        cp%ZRFHGRD = 0.0
        cp%ZBLDGRD = 0.0
        cp%GCGRD = 0.0

        cp%FCANROW = 0.0
        cp%LNZ0ROW = 0.0
        cp%ALVCROW = 0.0
        cp%ALICROW = 0.0
        cp%PAMXROW = 0.0
        cp%PAMNROW = 0.0
        cp%CMASROW = 0.0
        cp%ROOTROW = 0.0
        cp%RSMNROW = 0.0
        cp%QA50ROW = 0.0
        cp%VPDAROW = 0.0
        cp%VPDBROW = 0.0
        cp%PSGAROW = 0.0
        cp%PSGBROW = 0.0
        cp%DRNROW = 0.0
        cp%SDEPROW = 0.0
        cp%FAREROW = 0.0
        cp%DDROW = 0.0
        cp%XSLPROW = 0.0
        cp%XDROW = 0.0
        cp%MANNROW = 0.0
        cp%KSROW = 0.0
        cp%TCANROW = 0.0
        cp%TSNOROW = 0.0
        cp%TPNDROW = 0.0
        cp%ZPNDROW = 0.0
        cp%RCANROW = 0.0
        cp%SCANROW = 0.0
        cp%SNOROW = 0.0
        cp%ALBSROW = 0.0
        cp%RHOSROW = 0.0
        cp%GROROW = 0.0
        cp%MIDROW = 0
        cp%SANDROW = 0.0
        cp%CLAYROW = 0.0
        cp%ORGMROW = 0.0
        cp%TBARROW = 0.0
        cp%THLQROW = 0.0
        cp%THICROW = 0.0

        call READ_PARAMETERS_CLASS(shd, fls)

        !> MAM - Check for parameter values - all parameters should lie within the
        !> specified ranges in the "minmax_parameters.txt" file.
!        call check_parameters(WF_R2, M_C, NMTEST, cp, hp, soil_por_max, soil_depth, s0, t_ice_lens)

        !> GATHER-SCATTER COUNTS:
        allocate(shd%lc%ILMOS(shd%lc%ILG), shd%lc%JLMOS(shd%lc%ILG), shd%wc%ILMOS(shd%wc%ILG), &
                 shd%wc%JLMOS(shd%wc%ILG), stat = ierr)
        if (ierr /= 0) then
            print 1114, 'gather-scatter count'
            print 1118, 'Grid squares', NA
            print 1118, 'GRUs', NTYPE
            stop
        end if

        !> CLASS requires that each GRU for each grid square has its own parameter value,
        !> for MESH the value read in from the parameter file is assumed to be valid for
        !> all grid squares in the study area - Frank Seglenieks Aug 2007
        !> bjd - This would be a good spot for setting pre-distributed values
        cp%GCGRD(:) = cp%GCGRD(1)
        do m = 1, NTYPE
            cp%MIDROW(:, m) = cp%MIDROW(1, m)
        end do

        !> Set value of FAREROW:
!todo - flag this as an issue to explore later and hide basin average code
!todo - document the problem
!        TOTAL_AREA = 0.0
        cp%FAREROW = 0.0
        do i = 1, NA
            do m = 1, NTYPE
                cp%FAREROW(i, m) = shd%lc%ACLASS(i, m)*shd%FRAC(i)
!                TOTAL_AREA = TOTAL_AREA + cp%FAREROW(i, m)
!FUTUREDO: Bruce, FRAC is calculated by EnSim
! using Dan Princz's instructions for EnSim
! FRAC can be greater than 1.00
! So, we cannot use FAREROW in place of BASIN_FRACTION
            end do
        end do

        !> The following are used to read from soil.ini:
        !> wc_thpor, wc_thlret, wc_thlmin, wc_bi, wc_psisat,
        !> wc_grksat, wc_hcps, wc_tcs, wc_algwet, wc_algdry
        allocate(sv%wc_algwet(NA, NTYPE), sv%wc_algdry(NA, NTYPE))
        allocate(sv%wc_thpor(NA, NTYPE, IGND), sv%wc_thlret(NA, NTYPE, IGND), sv%wc_thlmin(NA, NTYPE, IGND), &
                 sv%wc_bi(NA, NTYPE, IGND), sv%wc_psisat(NA, NTYPE, IGND), sv%wc_grksat(NA, NTYPE, IGND), &
                 sv%wc_hcps(NA, NTYPE, IGND), sv%wc_tcs(NA, NTYPE, IGND))

        !> Zero everything we just allocated.
        sv%wc_algwet = 0.0
        sv%wc_algdry = 0.0
        sv%wc_thpor = 0.0
        sv%wc_thlret = 0.0
        sv%wc_thlmin = 0.0
        sv%wc_bi = 0.0
        sv%wc_psisat = 0.0
        sv%wc_grksat = 0.0
        sv%wc_hcps = 0.0
        sv%wc_tcs = 0.0

        !> Call to read from soil.ini.
        call READ_SOIL_INI(shd, fls)

        call GATPREP(shd%lc%ILMOS, shd%lc%JLMOS, shd%wc%ILMOS, shd%wc%JLMOS, &
                     shd%lc%NML, shd%wc%NML, cp%GCGRD, cp%FAREROW, cp%MIDROW, &
                     NA, NTYPE, shd%lc%ILG, 1, NA, NTYPE)

        NML = shd%lc%NML

!todo+++: Perhaps land-unit indexing can be done prior in the sequence
!todo+++: of initialization, after reading the drainage database.
!todo+++: Then, variables could be allocated (il1:il2) instead of
!todo+++: (1:ILG) to reduce the memory footprint of the model per node.

        !> Calculate Indices.
        call GetIndices(inp, izero, ipid, NML, shd%lc%ILMOS, il1, il2, ilen)
        if (ro%DIAGNOSEMODE > 0) print 1062, ipid, NML, ilen, il1, il2

1062 format(/1x, 'Configuration and distribution of the domain', &
            /3x, 'Current process: ', i10, &
            /3x, 'Tile land elements: ', i10, &
            /3x, 'Length of single array: ', i10, &
            /3x, 'Starting index: ', i10, &
            /3x, 'Stopping index: ', i10, /)

        !> ALLOCATE ALL VARIABLES
        allocate(SNOGRD(NA))

1114 format(/1x, 'Error allocating ', a, ' variables.', &
            /1x, 'Check that these bounds are within an acceptable range.', /)
1118 format(3x, a, ': ', i6)

        !> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
        allocate(FRZCGAT(NML), stat = ierr)

        !> PBSM PROGNOSTIC VARIABLES
        allocate(DrySnowGAT(NML), SnowAgeGAT(NML), &
                 TSNOdsGAT(NML), RHOSdsGAT(NML), &
                 DriftGAT(NML), SublGAT(NML), DepositionGAT(NML), &
                 ZSNOCS(NML), ZSNOGS(NML), &
                 ZSNOWC(NML), ZSNOWG(NML), &
                 HCPSCS(NML), HCPSGS(NML), &
                 HCPSC(NML), HCPSG(NML), &
                 TSNOWC(NML), TSNOWG(NML), &
                 RHOSC(NML), RHOSG(NML), &
                 XSNOWC(NML), XSNOWG(NML), &
                 XSNOCS(NML), XSNOGS(NML), stat = ierr)

        allocate(XDGAT(NML), &
                 KSGAT(NML), &
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
                 HMFGGRD(NA, IGND), HTCGRD(NA, IGND), QFCGRD(NA, IGND), &
                 GFLXGRD(NA, IGND), stat = ierr)
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

        !> Copy the starting date of input forcing data from CLASS.ini
        !> to the climate variable.
        cm%start_date%year = IYEAR
        cm%start_date%jday = IDAY
        cm%start_date%hour = IHOUR
        cm%start_date%mins = IMIN

        !> Set the starting date to that of the forcing data if none is
        !> provided and intialize the current time-step.
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

        !> Forcing input.
        allocate(cfi%FDL(NML), cfi%FSIH(NML), cfi%FSVH(NML), cfi%PRE(NML), cfi%PRES(NML), cfi%QA(NML), cfi%TA(NML), cfi%UL(NML), &
                 cfi%VL(NML), cfi%VMOD(NML))

        !> Prognostic variables.
        allocate(cpv%ALBS(NML), cpv%CMAI(NML), cpv%GRO(NML), cpv%QAC(NML), cpv%RCAN(NML), cpv%RHOS(NML), cpv%SNCAN(NML), &
                 cpv%SNO(NML), cpv%TAC(NML), cpv%TBAS(NML), cpv%TCAN(NML), cpv%TPND(NML), cpv%TSNO(NML), cpv%WSNO(NML), &
                 cpv%ZPND(NML))
        allocate(cpv%TBAR(NML, IGND), cpv%THIC(NML, IGND), cpv%THLQ(NML, IGND))
        allocate(cpv%TSFS(NML, 4))

        !> Land-surface variables.
        allocate(csfv%AGID(NML), csfv%AGVD(NML), csfv%ALGD(NML), csfv%ALGW(NML), csfv%ASID(NML), csfv%ASVD(NML), csfv%DRN(NML), &
                 csfv%FARE(NML), csfv%GRKF(NML), csfv%MID(NML), csfv%SDEP(NML), csfv%WFCI(NML), csfv%WFSF(NML), csfv%XSLP(NML), &
                 csfv%ZPLG(NML), csfv%ZPLS(NML), csfv%ZSNL(NML))
        allocate(csfv%IGDR(NML))
        allocate(csfv%IORG(NML, IGND), csfv%ISND(NML, IGND))
        allocate(csfv%BI(NML, IGND), csfv%CLAY(NML, IGND), csfv%DELZW(NML, IGND), csfv%GRKS(NML, IGND), csfv%HCPS(NML, IGND), &
                 csfv%ORGM(NML, IGND), csfv%PSIS(NML, IGND), csfv%PSIW(NML, IGND), csfv%SAND(NML, IGND), csfv%TCS(NML, IGND), &
                 csfv%THFC(NML, IGND), csfv%THM(NML, IGND), csfv%THP(NML, IGND), csfv%THR(NML, IGND), csfv%THRA(NML, IGND), &
                 csfv%ZBTW(NML, IGND))
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
        allocate(cdv%GFLX(NML, IGND), cdv%HMFG(NML, IGND), cdv%HTC(NML, IGND), cdv%QFC(NML, IGND))

        !> Read an initial value for geothermal flux from file.
        if (GGEOFLAG == 1) then
            iun = fls%fl(mfk%f18)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f18)%fn)), status = 'old', action = 'read', iostat = ierr)
            read(iun, *) GGEOGRD(1)
            close(iun)
        else
            GGEOGRD(1) = 0.0
        end if

        do k = il1, il2

            !> Extract the grid and GRU index values.
            ik = shd%lc%ILMOS(k)
            jk = shd%lc%JLMOS(k)

            !> Distribute variables.
            catv%ZRFM(k) = cp%ZRFMGRD(1)
            catv%ZRFH(k) = cp%ZRFHGRD(1)
            catv%ZBLD(k) = cp%ZBLDGRD(1)
            catv%GC(k) = cp%GCGRD(ik)
            csfv%FARE(k) = cp%FAREROW(ik, jk)
            csfv%MID(k) = cp%MIDROW(1, jk)
            do j = 1, ICP1
                csfv%FCAN(k, j) = cp%FCANROW(1, jk, j)
                csfv%LNZ0(k, j) = cp%LNZ0ROW(1, jk, j)
                csfv%ALVC(k, j) = cp%ALVCROW(1, jk, j)
                csfv%ALIC(k, j) = cp%ALICROW(1, jk, j)
            end do
            do j = 1, ICAN
                csfv%PAMX(k, j) = cp%PAMXROW(1, jk, j)
                csfv%PAMN(k, j) = cp%PAMNROW(1, jk, j)
                csfv%CMAS(k, j) = cp%CMASROW(1, jk, j)
                csfv%ROOT(k, j) = cp%ROOTROW(1, jk, j)
                csfv%RSMN(k, j) = cp%RSMNROW(1, jk, j)
                csfv%QA50(k, j) = cp%QA50ROW(1, jk, j)
                csfv%VPDA(k, j) = cp%VPDAROW(1, jk, j)
                csfv%VPDB(k, j) = cp%VPDBROW(1, jk, j)
                csfv%PSGA(k, j) = cp%PSGAROW(1, jk, j)
                csfv%PSGB(k, j) = cp%PSGBROW(1, jk, j)
            end do
            cpv%CMAI = 0.0
            cpv%WSNO = 0.0
            cpv%QAC = 0.5e-2
            cdv%ITCT = 0
            csfv%DRN(k) = cp%DRNROW(1, jk)
            csfv%SDEP(k) = cp%SDEPROW(1, jk)
            if (allocated(shd%SLOPE_INT)) then
                csfv%XSLP(k) = shd%SLOPE_INT(ik)
            else
                csfv%XSLP(k) = cp%XSLPROW(1, jk)
            end if
            !> note, if drdn (drainage density) is provided from the Mesh_drainage_database.r2c
            !> we give the same value for all the GRU that are in one cell
            if (allocated(shd%DRDN)) then
                DDGAT(k) = shd%DRDN(ik)
            else
                DDGAT(k) = cp%DDROW(1, jk)
            end if
            MANNGAT(k) = cp%MANNROW(1, jk)
            !> note, if drdn (drainage density) is provided from the Mesh_drainage_database.r2c
            !> we give the same value for all the GRU that are in one cell
            XDGAT(k) = cp%XDROW(1, jk)
            KSGAT(k) = cp%KSROW(1, jk)
            cpv%TCAN(k) = cp%TCANROW(1, jk) + TFREZ
            cpv%TAC(k) = cp%TCANROW(1, jk) + TFREZ
            cpv%TSNO(k) = cp%TSNOROW(1, jk) + TFREZ
            cpv%TPND(k) = cp%TPNDROW(1, jk) + TFREZ
            cpv%ZPND(k) = cp%ZPNDROW(1, jk)
            cpv%RCAN(k) = cp%RCANROW(1, jk)
            cpv%SNCAN(k) = cp%SCANROW(1, jk)
            cpv%SNO(k) = cp%SNOROW(1, jk)
            cpv%ALBS(k) = cp%ALBSROW(1, jk)
            cpv%RHOS(k) = cp%RHOSROW(1, jk)
            csfv%ZSNL(k) = hp%ZSNLROW(ik, jk)
            csfv%ZPLG(k) = hp%ZPLGROW(ik, jk)
            csfv%ZPLS(k) = hp%ZPLSROW(ik, jk)
            cpv%GRO(k) = cp%GROROW(1, jk)

            !> note333 see read_s_temperature_txt.f for more TBARROW information
            do j = 1, IGND
                cpv%TBAR(k, j) = cp%TBARROW(1, jk, j) + TFREZ
            end do
            cpv%TBAS(k) = cpv%TBAR(k, IGND)
            cpv%TSFS(k, 1) = TFREZ
            cpv%TSFS(k, 2) = TFREZ
            cpv%TSFS(k, 3) = cpv%TBAR(k, 1)
            cpv%TSFS(k, 4) = cpv%TBAR(k, 1)
            csfv%SDEP(k) = cp%SDEPROW(1, jk)

            !> note444 see read_s_moisture_txt.f for more THLQROW information
            do j = 1, 3
                cpv%THLQ(k, j) = cp%THLQROW(1, jk, j)
                cpv%THIC(k, j) = cp%THICROW(1, jk, j)
                csfv%SAND(k, j) = cp%SANDROW(1, jk, j)
                csfv%CLAY(k, j) = cp%CLAYROW(1, jk, j)
                csfv%ORGM(k, j) = cp%ORGMROW(1, jk, j)
            end do
            if (IGND > 3) then ! should stay this way to work with class

                !todo - if we have time, change this so that soil.ini can take more than 3 layers.
                if (NRSOILAYEREADFLAG == 0) then
                    do j = 4, IGND
                        cpv%THLQ(k, j) = cpv%THLQ(k, 3)
                        cpv%THIC(k, j) = cpv%THIC(k, 3)
                        cpv%TBAR(k, j) = cpv%TBAR(k, 3)
                        if (csfv%SDEP(k) < (shd%lc%sl%ZBOT(j - 1) + 0.001) .and. csfv%SAND(k, 3) > -2.5) then
                            csfv%SAND(k, j) = -3.0
                            csfv%CLAY(k, j) = -3.0
                            csfv%ORGM(k, j) = -3.0
                        else
                            csfv%SAND(k, j) = csfv%SAND(k, 3)
                            csfv%CLAY(k, j) = csfv%CLAY(k, 3)
                            csfv%ORGM(k, j) = csfv%ORGM(k, 3)
                        end if
                    end do
                else
                    do j = 4, IGND
                        if (csfv%SDEP(k) < (shd%lc%sl%ZBOT(j - 1) + 0.001) .and. csfv%SAND(k, 3) > -2.5) then
                            csfv%SAND(k, j) = -3.0
                            csfv%CLAY(k, j) = -3.0
                            csfv%ORGM(k, j) = -3.0
                        end if
                    end do
                end if !if (NRSOILAYEREADFLAG == 0) then
            end if !(IGND > 3) then
        end do !i = 1, NML

        !> FROZENSOILINFILFLAG
        allocate(INFILTYPE(NML), SI(NML), TSI(NML), &
                 SNOWMELTD(NML), SNOWMELTD_LAST(NML), SNOWINFIL(NML), &
                 CUMSNOWINFILCS(NML), MELTRUNOFF(NML), CUMSNOWINFILGS(NML))
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

        !> PDMROF
        allocate(CMINPDM(NML), CMAXPDM(NML), BPDM(NML), K1PDM(NML), &
                 K2PDM(NML), ZPNDPRECS(NML), ZPONDPREC(NML), ZPONDPREG(NML), &
                 ZPNDPREGS(NML), &
                 UM1CS(NML), UM1C(NML), UM1G(NML), UM1GS(NML), &
                 QM1CS(NML), QM1C(NML), QM1G(NML), QM1GS(NML), &
                 QM2CS(NML), QM2C(NML), QM2G(NML), QM2GS(NML), &
                 UMQ(NML), &
                 FSTRCS(NML), FSTRC(NML), FSTRG(NML), FSTRGS(NML))
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
        do k = il1, il2
            ik = shd%lc%ILMOS(k)
            jk = shd%lc%JLMOS(k)
            CMINPDM(k) = hp%CMAXROW(ik, jk)
            CMAXPDM(k) = hp%CMAXROW(ik, jk)
            BPDM(k) = hp%BROW(ik, jk)
            K1PDM(k) = hp%K1ROW(ik, jk)
            K2PDM(k) = hp%K2ROW(ik, jk)
        end do

        !> Allocate variables for WATDRN3
        !> ******************************************************************
        !> DGP - June 3, 2011: Now that variables are shared, moved from WD3
        !> flag to ensure allocation.
!-        allocate(BTC(NTYPE, IGND), BCAP(NTYPE, IGND), DCOEFF(NTYPE, IGND), &
!-                 BFCAP(NTYPE, IGND), BFCOEFF(NTYPE, IGND), BFMIN(NTYPE, IGND), &
!-                 BQMAX(NTYPE, IGND), stat = ierr)
!-        if (ierr /= 0) print *, 'Error allocating on WD3 for new WATDRN.'

        !> Call WATDRN3B to set WATDRN (Ric) variables
        !> ******************************************************************
        !> DGP - May 5, 2011: Added.
!-        call WATDRN3B(PSISROW, THPROW, GRKSROW, BIROW, cp%XSLPROW, cp%DDROW, &
!-                      NA, NTYPE, IGND, &
!-                      BTC, BCAP, DCOEFF, BFCAP, BFCOEFF, BFMIN, BQMAX, &
!-                      cp%SANDROW, cp%CLAYROW)

        !>**********************************************************************
        !> Set initial SnowAge & DrySnow values for PBSM calculations
        !> (MK MacDonald, Sept 2010)
        !>**********************************************************************
        if (PBSMFLAG == 1) then
            do k = il1, il2
                if (cpv%SNO(k) <= 0.0) then
                    DrySnowGAT(k) = 0.0 !1 = snowpack is dry (i.e. cold)
                    SnowAgeGAT(k) = 0.0 !hours since last snowfall
                    if (cm%clin(cfk%TT)%GAT(k) >= TFREZ) then
                        DrySnowGAT(k) = 0.0
                        SnowAgeGAT(k) = 48.0 !assume 48 hours since last snowfall
                    else
                        DrySnowGAT(k) = 1.0
                        SnowAgeGAT(k) = 48.0
                    end if
                end if
                ik = shd%lc%ILMOS(k)
                jk = shd%lc%JLMOS(k)
                fetchGAT(k) = hp%fetchROW(ik, jk)
                HtGAT(k) = hp%HtROW(ik, jk)
                N_SGAT(k) = hp%N_SROW(ik, jk)
                A_SGAT(k) = hp%A_SROW(ik, jk)
                DistribGAT(k) = hp%DistribROW(ik, jk)
            end do
        end if !PBSMFLAG == 1
        TSNOdsGAT = 0.0
        RHOSdsGAT = 0.0

        !> *********************************************************************
        !> Call CLASSBG to set more CLASS variables
        !> *********************************************************************
        !> bjd - July 25, 2005: For inputting field measured soil properties.
        call CLASSBG(csfv%THP, csfv%THR, csfv%THM, csfv%BI, csfv%PSIS, csfv%GRKS, &
                     csfv%THRA, csfv%HCPS, csfv%TCS, csfv%THFC, csfv%PSIW, &
                     csfv%DELZW, csfv%ZBTW, csfv%ALGW, csfv%ALGD, &
                     csfv%SAND, csfv%CLAY, csfv%ORGM, shd%lc%sl%DELZ, shd%lc%sl%ZBOT, csfv%SDEP, &
                     csfv%ISND, csfv%IGDR, NML, il1, il2, IGND, ICTEMMOD, &
                     sv%WC_THPOR, sv%WC_THLRET, sv%WC_THLMIN, sv%WC_BI, &
                     sv%WC_PSISAT, sv%WC_GRKSAT, sv%WC_HCPS, sv%WC_TCS, &
                     NA, NTYPE, shd%lc%ILG, shd%lc%ILMOS, shd%lc%JLMOS)

        if (WF_NUM_POINTS > 0) then

            print *, 'Found these output locations:'
            print *, 'Output Directory, grid number, land class number'
            do i = 1, WF_NUM_POINTS
                print *, op%DIR_OUT(i), op%N_OUT(i), op%II_OUT(i)
            end do
            print *

            call CLASSOUT_open_files(shd)
        end if

        if (ipid == 0 .and. BASINSWEOUTFLAG > 0) then
            open(85, file = './' // trim(fls%GENDIR_OUT) // '/basin_SCA_alldays.csv')
            open(86, file = './' // trim(fls%GENDIR_OUT) // '/basin_SWE_alldays.csv')
        end if !(BASINSWEOUTFLAG > 0) then

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
        !> The only value of DEGLAT is the one read in from the class.ini file,
        !> after that Diana uses RADJGRD (the value of latitude in radians) so
        !> after DEGLAT is used to calculate RADJGRD is it no longer used.  This
        !> is how it was in the original CLASS code.
        do k = il1, il2
            ik = shd%lc%ILMOS(k)
            !LATLENGTH = shd%AL/1000.0/(111.136 - 0.5623*cos(2*(DEGLAT*PI/180.0)) + 0.0011*cos(4*(DEGLAT*PI/180.0)))
            !LONGLENGTH = shd%AL/1000.0/(111.4172*cos((DEGLAT*PI/180.0)) - 0.094*cos(3*(DEGLAT*PI/180.0)) + 0.0002*cos(5*(DEGLAT*PI/180.0)))
            catv%RADJ(k) = ((shd%yOrigin + shd%yDelta*shd%yyy(ik)) - shd%yDelta/2.0)*PI/180.0
            catv%DLON(k) = (shd%xOrigin + shd%xDelta*shd%xxx(ik)) - shd%xDelta/2.0
        end do
        catv%Z0OR = 0.0
        catv%GGEO(:) = GGEOGRD(1)
        catv%ZDM = 10.0
        catv%ZDH = 2.0

        !> Initialize state prognostic variables.
        wb%LQWS = 0.0
        wb%FRWS = 0.0
        wb%RCAN = 0.0
        wb%SNCAN = 0.0
        wb%SNO = 0.0
        wb%WSNO = 0.0
        wb%PNDW = 0.0
        do k = il1, il2
            ik = shd%lc%ILMOS(k)
            FRAC = shd%lc%ACLASS(ik, shd%lc%JLMOS(k))*shd%FRAC(ik)
            if (FRAC > 0.0) then
                wb%LQWS(ik, :) = wb%LQWS(ik, :) + cpv%THLQ(k, :)*RHOW*csfv%DELZW(k, :)*FRAC
                wb%FRWS(ik, :) = wb%FRWS(ik, :) + cpv%THIC(k, :)*RHOICE*csfv%DELZW(k, :)*FRAC
                wb%RCAN(ik) = wb%RCAN(ik) + cpv%RCAN(k)*FRAC
                wb%SNCAN(ik) = wb%SNCAN(ik) + cpv%SNCAN(k)*FRAC
                wb%SNO(ik) = wb%SNO(ik) + cpv%SNO(k)*FRAC
                if (cpv%SNO(k) > 0.0) wb%WSNO(ik) = wb%WSNO(ik) + cpv%WSNO(k)*FRAC
                wb%PNDW(ik) = wb%PNDW(ik) + cpv%ZPND(k)*RHOW*FRAC
            end if
        end do

    end subroutine

end module
