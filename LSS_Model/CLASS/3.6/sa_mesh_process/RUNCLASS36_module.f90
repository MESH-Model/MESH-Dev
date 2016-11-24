module RUNCLASS36_module

    use RUNCLASS36_constants
    use RUNCLASS36_variables

    implicit none

    contains

    subroutine RUNCLASS36_within_tile(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_flags
        use mpi_shared_variables
        use mpi_module
        use mpi_utilities
        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        !> For internal variables.
        use RUNCLASS36_config

        !> For CLASS output.
        use RUNCLASS36_save_output

        !> For BASEFLOWFLAG.
!todo: Isolate this.
        use baseflow_module

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> istop: To stop all MPI process
        !* inp: Number of active tasks.
        !* ipid: Current process ID.
        integer ipid_recv, itag, ierrcode, istop, u, invars, iilen, ii1, ii2
        logical lstat

        integer NA, NTYPE, NML, IGND, k, ik, j, i
        real FRAC

        !* ierr: Diagnostic error/status return from various subroutines.
        integer :: ierr = 0

        integer, dimension(:), allocatable :: irqst
        integer, dimension(:, :), allocatable :: imstat

        !> SCA variables
        real TOTAL_AREA, basin_SCA, basin_SWE

        !> Return if the process is not marked active.
        if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE) return

!> *********************************************************************
!> Start of the NML-based LSS loop.
!> *********************************************************************

        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NML = shd%lc%NML
        IGND = shd%lc%IGND

!* N: is only used for debugging purposes.
!> N is incremented at the beginning of each loop. so you can tell which
!> iteration of the loop you are on by what the value of N is.
!> N is printed out with each of the error messages in CLASSZ.
!> N is replaced with ic%ts_count
!        N = N + 1

        if (ipid /= 0 .or. izero == 0) then

            !> Grab climate data.
            cfi%FSVH(il1:il2) = cm%dat(ck%FB)%GAT(il1:il2)/2.0
            cfi%FSIH(il1:il2) = cm%dat(ck%FB)%GAT(il1:il2)/2.0
            cfi%FDL(il1:il2) = cm%dat(ck%FI)%GAT(il1:il2)
            cfi%PRE(il1:il2) = cm%dat(ck%RT)%GAT(il1:il2)
            cfi%TA(il1:il2) = cm%dat(ck%TT)%GAT(il1:il2)
            cfi%UL(il1:il2) = cm%dat(ck%UV)%GAT(il1:il2)
            cfi%PRES(il1:il2) = cm%dat(ck%P0)%GAT(il1:il2)
            cfi%QA(il1:il2) = cm%dat(ck%HU)%GAT(il1:il2)

            cfi%VMOD = max(VMIN, cfi%UL)

            !> This estimates the fractional cloud cover (FCLOGRD) by the basis
            !> of the solar zenith angle and the occurrence of precipitation.
            !> Assumed to be 1 (100%) when precipitation occurs and somewhere
            !> in the range of [0.1, 1] based on the location of the sun in the
            !> sky when precipitation is not occuring. (0.1 when the sun is at
            !> the zenith, 1 when the sun is at the horizon).
            RDAY = real(ic%now%jday) + (real(ic%now%hour) + real(ic%now%mins)/60.0)/24.0
            DECL = sin(2.0*PI*(284.0 + RDAY)/365.0)*23.45*PI/180.0
            HOUR = (real(ic%now%hour) + real(ic%now%mins)/60.0)*PI/12.0 - PI

            do k = il1, il2
!                ik = shd%lc%ILMOS(k)
                COSZ = sin(catv%RADJ(k))*sin(DECL) + cos(catv%RADJ(k))*cos(DECL)*cos(HOUR)
                catv%CSZ(k) = sign(max(abs(COSZ), 1.0e-3), COSZ)
!                CSZGRD(ik) = catv%CSZ(k)
                if (cfi%PRE(k) > 0.0) then
                    catv%FCLO(k) = 1.0
                else
                    catv%FCLO(k) = max(0.0, min(1.0 - 0.9*COSZ, 1.0))
                end if
!-                FCLOGRD(ik) = catv%FCLO(k)
            end do

            !> Grab states.
            cpv%QAC(il1:il2) = stas%cnpy%qac(il1:il2)
            cpv%RCAN(il1:il2) = stas%cnpy%rcan(il1:il2)
            cpv%SNCAN(il1:il2) = stas%cnpy%sncan(il1:il2)
            cpv%TAC(il1:il2) = stas%cnpy%tac(il1:il2)
            cpv%TCAN(il1:il2) = stas%cnpy%tcan(il1:il2)
            cpv%CMAI(il1:il2) = stas%cnpy%cmai(il1:il2)
            cpv%GRO(il1:il2) = stas%cnpy%gro(il1:il2)
            cpv%SNO(il1:il2) = stas%sno%sno(il1:il2)
            cpv%ALBS(il1:il2) = stas%sno%albs(il1:il2)
            cpv%RHOS(il1:il2) = stas%sno%rhos(il1:il2)
            cpv%TSNO(il1:il2) = stas%sno%tsno(il1:il2)
            cpv%WSNO(il1:il2) = stas%sno%wsno(il1:il2)
            cpv%TPND(il1:il2) = stas%sfc%tpnd(il1:il2)
            cpv%ZPND(il1:il2) = stas%sfc%zpnd(il1:il2)
            cpv%TSFS(il1:il2, :) = stas%sfc%tsfs(il1:il2, :)
            cpv%TBAS(il1:il2) = stas%sl%tbas(il1:il2)
            cpv%THIC(il1:il2, :) = stas%sl%thic(il1:il2, :)
            cpv%THLQ(il1:il2, :) = stas%sl%thlq(il1:il2, :)
            cpv%TBAR(il1:il2, :) = stas%sl%tbar(il1:il2, :)

            !> Were initialized in CLASSG and so have been extracted.
            DriftGAT = 0.0 !DriftROW (ILMOS(k), JLMOS(k))
            SublGAT = 0.0 !SublROW (ILMOS(k), JLMOS(k))
            DepositionGAT = 0.0

            !> INITIALIZATION OF DIAGNOSTIC VARIABLES SPLIT OUT OF CLASSG
            !> FOR CONSISTENCY WITH GCM APPLICATIONS.
            cdv%CDH = 0.0
            cdv%CDM = 0.0
            cdv%HFS = 0.0
            cdv%TFX = 0.0
            cdv%QEVP = 0.0
            cdv%QFS = 0.0
            cdv%QFX = 0.0
            cdv%PET = 0.0
            cdv%GA = 0.0
            cdv%EF = 0.0
            cdv%GTE = 0.0
            cdv%QG = 0.0
            cdv%ALVS = 0.0
            cdv%ALIR = 0.0
            cdv%SFCT = 0.0
            cdv%SFCU = 0.0
            cdv%SFCV = 0.0
            cdv%SFCQ = 0.0
            cdv%FSNO = 0.0
            cdv%FSGV = 0.0
            cdv%FSGS = 0.0
            cdv%FSGG = 0.0
            cdv%FLGV = 0.0
            cdv%FLGS = 0.0
            cdv%FLGG = 0.0
            cdv%HFSC = 0.0
            cdv%HFSS = 0.0
            cdv%HFSG = 0.0
            cdv%HEVC = 0.0
            cdv%HEVS = 0.0
            cdv%HEVG = 0.0
            cdv%HMFC = 0.0
            cdv%HMFN = 0.0
            cdv%HTCC = 0.0
            cdv%HTCS = 0.0
            cdv%PCFC = 0.0
            cdv%PCLC = 0.0
            cdv%PCPN = 0.0
            cdv%PCPG = 0.0
            cdv%QFG = 0.0
            cdv%QFN = 0.0
            cdv%QFCF = 0.0
            cdv%QFCL = 0.0
            cdv%ROF = 0.0
            cdv%ROFO = 0.0
            cdv%ROFS = 0.0
            cdv%ROFB = 0.0
            cdv%TROF = 0.0
            cdv%TROO = 0.0
            cdv%TROS = 0.0
            cdv%TROB = 0.0
            cdv%ROFC = 0.0
            cdv%ROFN = 0.0
            cdv%ROVG = 0.0
            cdv%WTRC = 0.0
            cdv%WTRS = 0.0
            cdv%WTRG = 0.0
            cdv%DR = 0.0
            cdv%HMFG = 0.0
            cdv%HTC = 0.0
            cdv%QFC = 0.0
            cdv%GFLX = 0.0
            cdv%ITCT = 0

            call CLASSI(catv%VPD, catv%TADP, catv%PADR, catv%RHOA, catv%RHSI, &
                        catv%RPCP, catv%TRPC, catv%SPCP, catv%TSPC, cfi%TA, cfi%QA, &
                        cfi%PRE, catv%RPRE, catv%SPRE, cfi%PRES, &
                        IPCP, NML, il1, il2)

            call CLASSZ(0, CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, &
                        WTVSTP, WTSSTP, WTGSTP, &
                        cdv%FSGV, cdv%FLGV, cdv%HFSC, cdv%HEVC, cdv%HMFC, cdv%HTCC, &
                        cdv%FSGS, cdv%FLGS, cdv%HFSS, cdv%HEVS, cdv%HMFN, cdv%HTCS, &
                        cdv%FSGG, cdv%FLGG, cdv%HFSG, cdv%HEVG, cdv%HMFG, cdv%HTC, &
                        cdv%PCFC, cdv%PCLC, cdv%QFCF, cdv%QFCL, cdv%ROFC, cdv%WTRC, &
                        cdv%PCPN, cdv%QFN, cdv%ROFN, cdv%WTRS, cdv%PCPG, cdv%QFG, &
                        cdv%QFC, cdv%ROF, cdv%WTRG, cpv%CMAI, cpv%RCAN, cpv%SNCAN, &
                        cpv%TCAN, cpv%SNO, cpv%WSNO, cpv%TSNO, cpv%THLQ, cpv%THIC, &
                        csfv%HCPS, csfv%THP, csfv%DELZW, cpv%TBAR, cpv%ZPND, cpv%TPND, &
                        shd%lc%sl%DELZ, cdv%FCS, cdv%FGS, cdv%FC, cdv%FG, &
                        il1, il2, NML, IGND, ic%ts_count, &
                        DriftGAT, SublGAT)

            !> ALBEDO AND TRANSMISSIVITY CALCULATIONS; GENERAL VEGETATION
            !> CHARACTERISTICS.
            call CLASSA(cdv%FC, cdv%FG, cdv%FCS, cdv%FGS, ALVSCN, ALIRCN, &
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
                        cpv%WSNO, cdv%ALVS, cdv%ALIR, cdv%HTCC, cdv%HTCS, cdv%HTC, &
                        cdv%WTRC, cdv%WTRS, cdv%WTRG, cpv%CMAI, cdv%FSNO, &
                        csfv%FCAN, csfv%LNZ0, csfv%ALVC, csfv%ALIC, csfv%PAMX, csfv%PAMN, &
                        csfv%CMAS, csfv%ROOT, csfv%RSMN, csfv%QA50, csfv%VPDA, csfv%VPDB, &
                        csfv%PSGA, csfv%PSGB, csfv%PAID, csfv%HGTD, csfv%ACVD, csfv%ACID, &
                        csfv%ASVD, csfv%ASID, csfv%AGVD, csfv%AGID, csfv%ALGW, csfv%ALGD, &
                        cpv%THLQ, cpv%THIC, cpv%TBAR, cpv%RCAN, cpv%SNCAN, cpv%TCAN, &
                        cpv%GRO, cpv%SNO, cpv%TSNO, cpv%RHOS, cpv%ALBS, catv%ZBLD, &
                        catv%Z0OR, csfv%ZSNL, csfv%ZPLG, csfv%ZPLS, &
                        catv%FCLO, cfi%TA, catv%VPD, catv%RHOA, catv%CSZ, &
                        cfi%FSVH, catv%RADJ, catv%DLON, catv%RHSI, shd%lc%sl%DELZ, csfv%DELZW, &
                        csfv%ZBTW, csfv%THP, csfv%THM, csfv%PSIS, csfv%BI, csfv%PSIW, &
                        csfv%HCPS, csfv%ISND, &
                        FCANCMX, ICTEM, ICTEMMOD, RMATC, &
                        AILC, PAIC, L2MAX, NOL2PFTS, &
                        AILCG, AILCGS, FCANC, FCANCS, &
                        ic%now%jday, NML, il1, il2, &
                        JLAT, ic%ts_count, ICAN, ICAN + 1, IGND, IDISP, IZREF, &
                        IWF, IPAI, IHGT, IALC, IALS, IALG)

            !> SURFACE TEMPERATURE AND FLUX CALCULATIONS.
            call CLASST(TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG, &
                        THICEC, THICEG, HCPC, HCPG, TCTOPC, TCBOTC, TCTOPG, TCBOTG, &
                        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, G12CS, G12GS, &
                        G23C, G23G, G23CS, G23GS, QFREZC, QFREZG, QMELTC, QMELTG, &
                        EVAPC, EVAPCG, EVAPG, EVAPCS, EVPCSG, EVAPGS, TCANO, TCANS, &
                        RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP, CHCAPS, TPONDC, TPONDG, &
                        TPNDCS, TPNDGS, TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, &
                        cdv%ITCT, cdv%CDH, cdv%CDM, cdv%HFS, cdv%TFX, cdv%QEVP, cdv%QFS, cdv%QFX, &
                        cdv%PET, cdv%GA, cdv%EF, cdv%GTE, cdv%QG, cdv%SFCT, cdv%SFCU, cdv%SFCV, &
                        cdv%SFCQ, SFRHGAT, cdv%FSGV, cdv%FSGS, cdv%FSGG, cdv%FLGV, cdv%FLGS, cdv%FLGG, &
                        cdv%HFSC, cdv%HFSS, cdv%HFSG, cdv%HEVC, cdv%HEVS, cdv%HEVG, cdv%HMFC, cdv%HMFN, &
                        cdv%HTCC, cdv%HTCS, cdv%HTC, cdv%QFCF, cdv%QFCL, cdv%DR, cdv%WTAB, cdv%ILMO, &
                        cdv%UE, cdv%HBL, cpv%TAC, cpv%QAC, catv%ZRFM, catv%ZRFH, catv%ZDM, catv%ZDH, &
                        catv%VPD, catv%TADP, catv%RHOA, cfi%FSVH, cfi%FSIH, cfi%FDL, cfi%UL, cfi%VL, &
                        cfi%TA, cfi%QA, catv%PADR, cdv%FC, cdv%FG, cdv%FCS, cdv%FGS, RBCOEF, &
                        FSVF, FSVFS, cfi%PRES, cfi%VMOD, ALVSCN, ALIRCN, ALVSG, ALIRG, &
                        ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, ALIRSC, &
                        TRVSCN, TRIRCN, TRVSCS, TRIRCS, RC, RCS, cdv%WTRG, QLWOGAT, &
                        FRAINC, FSNOWC, FRAICS, FSNOCS, CMASSC, CMASCS, DISP, DISPS, &
                        ZOMLNC, ZOELNC, ZOMLNG, ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, &
                        cpv%TBAR, cpv%THLQ, cpv%THIC, cpv%TPND, cpv%ZPND, cpv%TBAS, cpv%TCAN, cpv%TSNO, &
                        ZSNOW, TRSNOW, cpv%RHOS, cpv%WSNO, csfv%THP, csfv%THR, csfv%THM, csfv%THFC, &
                        catv%RADJ, cfi%PRE, csfv%HCPS, csfv%TCS, cpv%TSFS, shd%lc%sl%DELZ, csfv%DELZW, csfv%ZBTW, &
                        FTEMP, FVAP, RIB, csfv%ISND, &
                        AILCG, AILCGS, FCANC, FCANCS, CO2CONC, CO2I1CG, CO2I1CS, CO2I2CG, &
                        CO2I2CS, COSZS, XDIFFUSC, SLAI, ICTEM, ICTEMMOD, RMATCTEM, &
                        FCANCMX, L2MAX, NOL2PFTS, CFLUXCG, CFLUXCS, ANCSVEG, ANCGVEG, &
                        RMLCSVEG, RMLCGVEG, FIELDSM, WILTSM, &
                        ITC, ITCG, ITG, NML, il1, il2, JLAT, ic%ts_count, ICAN, &
                        IGND, IZREF, ISLFD, NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI)

            if (ic%now%jday == 1 .and. ic%ts_daily == 48) then
       ! bruce davison - only increase NMELT if we don't start the run on January 1st, otherwise t0_ACC allocation is too large
       ! and the model crashes if the compiler is checking for array bounds when t0_ACC is passed into CLASSW with size NMELT
                if (ic%start%jday == 1 .and. ic%ts_count < 49) then
                    continue ! NMELT should stay = 1
                else
                    NMELT = NMELT + 1
                end if
                CUMSNOWINFILCS = 0.0
                CUMSNOWINFILGS = 0.0
                INFILTYPE = 2
            end if

            !> WATER BUDGET CALCULATIONS.
            call CLASSW(cpv%THLQ, cpv%THIC, cpv%TBAR, cpv%TCAN, cpv%RCAN, cpv%SNCAN, &
                        cdv%ROF, cdv%TROF, cpv%SNO, cpv%TSNO, cpv%RHOS, cpv%ALBS, &
                        cpv%WSNO, cpv%ZPND, cpv%TPND, cpv%GRO, FRZCGAT, cpv%TBAS, cdv%GFLX, &
                        cdv%PCFC, cdv%PCLC, cdv%PCPN, cdv%PCPG, cdv%QFCF, cdv%QFCL, &
                        cdv%QFN, cdv%QFG, cdv%QFC, cdv%HMFC, cdv%HMFG, cdv%HMFN, &
                        cdv%HTCC, cdv%HTCS, cdv%HTC, cdv%ROFC, cdv%ROFN, cdv%ROVG, &
                        cdv%WTRS, cdv%WTRG, cdv%ROFO, cdv%ROFS, cdv%ROFB, &
                        cdv%TROO, cdv%TROS, cdv%TROB, cdv%QFS, &
                        TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG, &
                        THICEC, THICEG, HCPC, HCPG, catv%RPCP, catv%TRPC, &
                        catv%SPCP, catv%TSPC, cfi%PRE, cfi%TA, catv%RHSI, catv%GGEO, &
                        cdv%FC, cdv%FG, cdv%FCS, cdv%FGS, TPONDC, TPONDG, &
                        TPNDCS, TPNDGS, EVAPC, EVAPCG, EVAPG, EVAPCS, &
                        EVPCSG, EVAPGS, QFREZC, QFREZG, QMELTC, QMELTG, &
                        RAICAN, SNOCAN, RAICNS, SNOCNS, FROOT, FSVF, &
                        FSVFS, CWLCAP, CWFCAP, CWLCPS, CWFCPS, TCANO, &
                        TCANS, CHCAP, CHCAPS, CMASSC, CMASCS, ZSNOW, &
                        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, &
                        G12CS, G12GS, G23C, G23G, G23CS, G23GS, &
                        TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, &
                        ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, cpv%TSFS, &
                        TCTOPC, TCBOTC, TCTOPG, TCBOTG, &
                        csfv%THP, csfv%THR, csfv%THM, csfv%BI, csfv%PSIS, csfv%GRKS, &
                        csfv%THRA, csfv%THFC, csfv%DRN, csfv%HCPS, shd%lc%sl%DELZ, &
                        csfv%DELZW, csfv%ZBTW, csfv%XSLP, XDGAT, csfv%WFSF, KSGAT, &
                        csfv%ISND, csfv%IGDR, IWF, NML, il1, il2, ic%ts_count, &
                        JLAT, ICAN, IGND, IGND + 1, IGND + 2, &
                        NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI, &
                        MANNGAT, DDGAT, ic%ts_daily, &
                        t0_ACC(NMELT), SI, TSI, INFILTYPE, SNOWMELTD, SNOWMELTD_LAST, &
                        MELTRUNOFF, SNOWINFIL, CUMSNOWINFILCS, CUMSNOWINFILGS, &
                        SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS, &
                        NA, NTYPE, shd%lc%ILMOS, shd%lc%JLMOS, &
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

            !> SINGLE COLUMN BLOWING SNOW CALCULATIONS.
!+            if (PBSMFLAG == 1) then
!+                call PBSMrun(ZSNOW, cpv%WSNO, cpv%SNO, cpv%RHOS, cpv%TSNO, cdv%HTCS, &
!+                             ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
!+                             HCPSCS, HCPSGS, HCPSC, HCPSG, &
!+                             TSNOWC, TSNOWG, TSNOCS, TSNOGS, &
!+                             RHOSC, RHOSG, RHOSCS, RHOSGS,&
!+                             XSNOWC, XSNOWG, XSNOCS, XSNOGS, &
!+                             WSNOCS, WSNOGS, &
!+                             cdv%FC, cdv%FG, cdv%FCS, cdv%FGS, &
!+                             fetchGAT, N_SGAT, A_SGAT, HtGAT, &
!+                             cdv%SFCT, cdv%SFCU, cdv%SFCQ, cfi%PRES, cfi%PRE, &
!+                             DrySnowGAT, SnowAgeGAT, DriftGAT, SublGAT, &
!+                             TSNOdsGAT, &
!+                             NML, il1, il2, ic%ts_count, catv%ZRFM, ZOMLCS, ZOMLNS)
!+            end if

            call CLASSZ(1, CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, &
                        WTVSTP, WTSSTP, WTGSTP, &
                        cdv%FSGV, cdv%FLGV, cdv%HFSC, cdv%HEVC, cdv%HMFC, cdv%HTCC, &
                        cdv%FSGS, cdv%FLGS, cdv%HFSS, cdv%HEVS, cdv%HMFN, cdv%HTCS, &
                        cdv%FSGG, cdv%FLGG, cdv%HFSG, cdv%HEVG, cdv%HMFG, cdv%HTC, &
                        cdv%PCFC, cdv%PCLC, cdv%QFCF, cdv%QFCL, cdv%ROFC, cdv%WTRC, &
                        cdv%PCPN, cdv%QFN, cdv%ROFN, cdv%WTRS, cdv%PCPG, cdv%QFG, &
                        cdv%QFC, cdv%ROF, cdv%WTRG, cpv%CMAI, cpv%RCAN, cpv%SNCAN, &
                        cpv%TCAN, cpv%SNO, cpv%WSNO, cpv%TSNO, cpv%THLQ, cpv%THIC, &
                        csfv%HCPS, csfv%THP, csfv%DELZW, cpv%TBAR, cpv%ZPND, cpv%TPND, &
                        shd%lc%sl%DELZ, cdv%FCS, cdv%FGS, cdv%FC, cdv%FG, &
                        il1, il2, NML, IGND, ic%ts_count, &
                        DriftGAT, SublGAT)

            !> Redistribute blowing snow mass between GRUs.
!todo: This has a dependency on cp%GCGRD.
!+            call REDISTRIB_SNOW(NML, 1, NA, NTYPE, NML, cpv%TSNO, ZSNOW, &
!+                                cpv%RHOS, cpv%SNO, TSNOCS, ZSNOCS, HCPSCS, RHOSCS, TSNOGS, &
!+                                ZSNOGS, HCPSGS, RHOSGS, TSNOWC, ZSNOWC, HCPSC, RHOSC, TSNOWG, &
!+                                ZSNOWG, HCPSG, RHOSG, cp%GCGRD, shd%lc%ILMOS, DriftGAT, csfv%FARE, &
!+                                TSNOdsGAT, DistribGAT, WSNOCS, WSNOGS, cdv%FCS, cdv%FGS, cdv%FC, cdv%FG, DepositionGAT, &
!+                                cdv%TROO, cdv%ROFO, cdv%TROF, cdv%ROF, cdv%ROFN, cdv%PCPG, cdv%HTCS, cpv%WSNO, ic%ts_count)
            cdv%ROF = cdv%ROF - UMQ

            !> BASEFLOWFLAG
            if (lzsp%BASEFLOWFLAG > 0) then
                select case (lzsp%BASEFLOWFLAG)
                    case (1)
                        cdv%ROF = cdv%ROF - cdv%ROFB
                        Wseep = cdv%ROFB*3600.0
                        do k = il1, il2
                            call baseFlow(Wseep(k), dgw(k), Wrchrg(k), agw(k), Qb(k), 1.0, Wrchrg_new, Qb_new)
                            cdv%ROFB(k) = Qb_new/3600.0
                            Qb(k) = Qb_new
                            Wrchrg(k) = Wrchrg_new
                        end do
                        cdv%ROF = cdv%ROF + cdv%ROFB
                        cdv%WTRG = cdv%WTRG - (Wseep/3600.0 - cdv%ROFB)
                    case (2)
                        cdv%ROF = cdv%ROF - cdv%ROFB
                        Wseep = cdv%ROFB
                        Wrchrg = Wrchrg + cdv%ROFB
                        call baseflow_wfqlz(WF_LZFA, WF_LZFPWR, Wrchrg, cdv%ROFB, NML, il1, il1)
                        cdv%ROF = cdv%ROF + cdv%ROFB
                        cdv%WTRG = cdv%WTRG - (Wseep - cdv%ROFB)
                end select
            end if

        end if !(ipid /= 0 .or. izero == 0) then

        !> WRITE FIELDS FROM CURRENT TIME STEP TO OUTPUT FILES.
        if (WF_NUM_POINTS > 0) then
            call CLASSOUT_update_files(shd)
        end if

        !> Gather variables from parallel nodes.
!todo: move this.

        !> Send/receive process.
        itag = ic%ts_count*1000
        invars = 15 + 4*IGND

        !> Update the variable count per the active control flags.
        if (SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) invars = invars + 10 + 4

        !> BASEFLOWFLAG.
        if (lzsp%BASEFLOWFLAG > 0) then
            invars = invars + 1
            if (lzsp%BASEFLOWFLAG == 1) then
                invars = invars + 1
            end if
        end if

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))
            irqst = mpi_request_null

            i = 1
            call mpi_isend(cfi%PRE(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cdv%QFS(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cdv%PET(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cdv%ROF(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cdv%ROFO(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cdv%ROFS(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cdv%ROFB(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cpv%SNCAN(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cpv%RCAN(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cpv%ZPND(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cpv%SNO(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cdv%FSNO(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cpv%WSNO(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cdv%HFS(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cdv%QEVP(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            do j = 1, IGND
                call mpi_isend(cpv%THLQ(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%THIC(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cdv%GFLX(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%TBAR(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            end do

            !> Send optional variables per the active control flags.
            if (SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) then
                call mpi_isend(cpv%ALBS(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%CMAI(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%GRO(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%QAC(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%RHOS(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%TAC(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%TBAS(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%TCAN(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%TPND(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(cpv%TSNO(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                do j = 1, 4
                    call mpi_isend(cpv%TSFS(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                end do
            end if !(SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) then

            !> BASEFLOWFLAG.
            if (lzsp%BASEFLOWFLAG > 0) then
                call mpi_isend(Wrchrg(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                if (lzsp%BASEFLOWFLAG == 1) then
                    call mpi_isend(Qb(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                end if
            end if

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

                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iilen)

                i = 1
                call mpi_irecv(cfi%PRE(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cdv%QFS(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cdv%PET(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cdv%ROF(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cdv%ROFO(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cdv%ROFS(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cdv%ROFB(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cpv%SNCAN(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cpv%RCAN(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cpv%ZPND(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cpv%SNO(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cdv%FSNO(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cpv%WSNO(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cdv%HFS(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cdv%QEVP(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                do j = 1, IGND
                    call mpi_irecv(cpv%THLQ(ii1:ii2, j), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%THIC(ii1:ii2, j), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cdv%GFLX(ii1:ii2, j), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%TBAR(ii1:ii2, j), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                end do

                !> Send optional variables per the active control flags.
                if (SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) then
                    call mpi_irecv(cpv%ALBS(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%CMAI(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%GRO(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%QAC(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%RHOS(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%TAC(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%TBAS(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%TCAN(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%TPND(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(cpv%TSNO(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    do j = 1, 4
                        call mpi_irecv(cpv%TSFS(ii1:ii2, j), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr)
                        i = i + 1
                    end do
                end if !(SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) then

                !> BASEFLOWFLAG.
                if (lzsp%BASEFLOWFLAG > 0) then
                    call mpi_irecv(Wrchrg(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    if (lzsp%BASEFLOWFLAG == 1) then
                        call mpi_irecv(Qb(ii1:ii2), iilen, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    end if
                end if

                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(invars, irqst, lstat, imstat, ierr)
                end do

            end do !u = 1, (inp - 1)
!            print *, 'done receiving'

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, ierr)

        !> calculate and write the basin avg SCA similar to watclass3.0f5
        !> Same code than in wf_ensim.f subrutine of watclass3.0f8
        !> Especially for version MESH_Prototype 3.3.1.7b (not to be incorporated in future versions)
        !> calculate and write the basin avg SWE using the similar fudge factor!!!
        if (ipid == 0 .and. BASINSWEOUTFLAG > 0) then

            !> BASIN_FRACTION is the basin snow cover
            !> (portions of the grids outside the basin are not included)
            !> for a given day - JDAY_NOW in the if statement
!            if (BASIN_FRACTION(1) == -1) then
!todo: FRAC is not actually the fraction of the grid square
!within the basin, we should be using some other value, but I'm
!not sure what.
!todo: calculate frac and write document to send to someone else.
!                do i = 1, NA ! NA = number of grid squares
!                    BASIN_FRACTION(i) = shd%FRAC(i)
!                end do
!            end if

            if (ic%now%hour == 12 .and. ic%now%mins == 0) then
                basin_SCA = 0.0
                basin_SWE = 0.0
!                do i = 1, NA
!                    if (BASIN_FRACTION(i) /= 0.0) then
!                        basin_SCA = basin_SCA + FSNOGRD(i)/BASIN_FRACTION(i)
!                        basin_SWE = basin_SWE + SNOGRD(i)/BASIN_FRACTION(i)
!                    end if
!                end do
!                basin_SCA = basin_SCA/NA
!                basin_SWE = basin_SWE/NA
                TOTAL_AREA = wb%basin_area

                !> BRUCE DAVISON - AUG 17, 2009 (see notes in my notebook for this day)
                !> Fixed calculation of basin averages. Needs documenting and testing.
                do k = il1, il2
                    ik = shd%lc%ILMOS(k)
                    FRAC = shd%lc%ACLASS(ik, shd%lc%JLMOS(k))*shd%FRAC(ik)
                    basin_SCA = basin_SCA + cdv%FSNO(k)*FRAC
                    basin_SWE = basin_SWE + cpv%SNO(k)*FRAC
                end do
                basin_SCA = basin_SCA/TOTAL_AREA
                basin_SWE = basin_SWE/TOTAL_AREA
                if (BASINSWEOUTFLAG > 0) then
                    write(85, "(i5,',', f10.3)") ic%now%jday, basin_SCA
                    write(86, "(i5,',', f10.3)") ic%now%jday, basin_SWE
                end if
            end if

        end if !(ipid == 0) then

        if (ipid == 0) then

            !> Copy over state variables.
            stas%cnpy%qac = cpv%QAC
            stas%cnpy%rcan = cpv%RCAN
            stas%cnpy%sncan = cpv%SNCAN
            stas%cnpy%tac = cpv%TAC
            stas%cnpy%tcan = cpv%TCAN
            stas%cnpy%cmai = cpv%CMAI
            stas%cnpy%gro = cpv%GRO
            stas%sno%sno = cpv%SNO
            stas%sno%albs = cpv%ALBS
            stas%sno%rhos = cpv%RHOS
            stas%sno%tsno = cpv%TSNO
            stas%sno%wsno = cpv%WSNO
            stas%sfc%tpnd = cpv%TPND
            stas%sfc%zpnd = cpv%ZPND
            stas%sfc%tsfs = cpv%TSFS
            stas%sl%tbas = cpv%TBAS
            stas%sl%thic = cpv%THIC
            stas%sl%fzws = cpv%THIC*csfv%DELZW*RHOICE
            stas%sl%thlq = cpv%THLQ
            stas%sl%lqws = cpv%THLQ*csfv%DELZW*RHOW
            stas%sl%tbar = cpv%TBAR
            stas%cnpy%evp = cdv%QFS
            stas%cnpy%pevp = cdv%PET

            do k = il1, il2
                ik = shd%lc%ILMOS(k)
                FRAC = shd%lc%ACLASS(ik, shd%lc%JLMOS(k))*shd%FRAC(ik)
                if (FRAC > 0.0) then
                    wb%PRE(ik) = wb%PRE(ik) + cfi%PRE(k)*FRAC*ic%dts
                    eb%QEVP(ik) = eb%QEVP(ik) + cdv%QEVP(k)*FRAC
                    wb%EVAP(ik) = wb%EVAP(ik) + cdv%QFS(k)*FRAC*ic%dts
                    eb%HFS(ik)  = eb%HFS(ik) + cdv%HFS(k)*FRAC
                    wb%ROF(ik) = wb%ROF(ik) + cdv%ROF(k)*FRAC*ic%dts
                    wb%ROFO(ik) = wb%ROFO(ik) + cdv%ROFO(k)*FRAC*ic%dts
                    wb%ROFS(ik) = wb%ROFS(ik) + cdv%ROFS(k)*FRAC*ic%dts
                    wb%ROFB(ik) = wb%ROFB(ik) + cdv%ROFB(k)*FRAC*ic%dts
                    do j = 1, IGND
                        sp%TBAR(ik, j) = sp%TBAR(ik, j) + cpv%TBAR(k, j)*shd%lc%ACLASS(ik, shd%lc%JLMOS(k))
                        sp%THLQ(ik, j) = sp%THLQ(ik, j) + cpv%THLQ(k, j)*FRAC
                        wb%LQWS(ik, j) = wb%LQWS(ik, j) + cpv%THLQ(k, j)*csfv%DELZW(k, j)*FRAC*RHOW
                        sp%THIC(ik, j) = sp%THIC(ik, j) + cpv%THIC(k, j)*FRAC
                        wb%FRWS(ik, j) = wb%FRWS(ik, j) + cpv%THIC(k, j)*csfv%DELZW(k, j)*FRAC*RHOICE
                        eb%GFLX(ik, j) = eb%GFLX(ik, j) + cdv%GFLX(k, j)*FRAC
                    end do
                    wb%RCAN(ik) = wb%RCAN(ik) + cpv%RCAN(k)*FRAC
                    wb%SNCAN(ik) = wb%SNCAN(ik) + cpv%SNCAN(k)*FRAC
                    wb%SNO(ik) = wb%SNO(ik) + cpv%SNO(k)*FRAC
                    if (cpv%SNO(k) > 0.0) then
                        wb%WSNO(ik) = wb%WSNO(ik) + cpv%WSNO(k)*FRAC
                    end if
                    wb%PNDW(ik) = wb%PNDW(ik) + cpv%ZPND(k)*FRAC*RHOW
                end if
            end do !k = il1, il2

            wb%DSTG = wb%RCAN + wb%SNCAN + wb%SNO + wb%WSNO + wb%PNDW + &
                sum(wb%LQWS, 2) + sum(wb%FRWS, 2) - wb%STG
            wb%STG = wb%DSTG + wb%STG

        else

            !> Copy over state variables.
            stas%cnpy%qac(il1:il2) = cpv%QAC(il1:il2)
            stas%cnpy%rcan(il1:il2) = cpv%RCAN(il1:il2)
            stas%cnpy%sncan(il1:il2) = cpv%SNCAN(il1:il2)
            stas%cnpy%tac(il1:il2) = cpv%TAC(il1:il2)
            stas%cnpy%tcan(il1:il2) = cpv%TCAN(il1:il2)
            stas%cnpy%cmai(il1:il2) = cpv%CMAI(il1:il2)
            stas%cnpy%gro(il1:il2) = cpv%GRO(il1:il2)
            stas%sno%sno(il1:il2) = cpv%SNO(il1:il2)
            stas%sno%albs(il1:il2) = cpv%ALBS(il1:il2)
            stas%sno%rhos(il1:il2) = cpv%RHOS(il1:il2)
            stas%sno%tsno(il1:il2) = cpv%TSNO(il1:il2)
            stas%sno%wsno(il1:il2) = cpv%WSNO(il1:il2)
            stas%sfc%tpnd(il1:il2) = cpv%TPND(il1:il2)
            stas%sfc%zpnd(il1:il2) = cpv%ZPND(il1:il2)
            stas%sfc%tsfs(il1:il2, :) = cpv%TSFS(il1:il2, :)
            stas%sl%tbas(il1:il2) = cpv%TBAS(il1:il2)
            stas%sl%thic(il1:il2, :) = cpv%THIC(il1:il2, :)
            stas%sl%fzws(il1:il2, :) = cpv%THIC(il1:il2, :)*csfv%DELZW(il1:il2, :)*RHOICE
            stas%sl%thlq(il1:il2, :) = cpv%THLQ(il1:il2, :)
            stas%sl%lqws(il1:il2, :) = cpv%THLQ(il1:il2, :)*csfv%DELZW(il1:il2, :)*RHOW
            stas%sl%tbar(il1:il2, :) = cpv%TBAR(il1:il2, :)
            stas%cnpy%evp(il1:il2) = cdv%QFS(il1:il2)
            stas%cnpy%pevp(il1:il2) = cdv%PET(il1:il2)

        end if

    end subroutine

end module
