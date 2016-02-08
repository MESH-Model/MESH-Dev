module process_CLASS

    use process_CLASS_constants
    use process_CLASS_variables

    implicit none

    contains

    subroutine RUNCLASS_within_tile(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

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

        !> For internal variables.
        use process_CLASS_config

        !> For CLASS output.
        use process_CLASS_save_output

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

        integer NA, NTYPE, NML, IGND

!> *********************************************************************
!> Start of the NML-based LSS loop.
!> *********************************************************************

        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NML = shd%lc%NML
        IGND = shd%lc%IGND

        if (ipid /= 0 .or. izero == 0) then

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
                        ic%now_jday, NML, il1, il2, &
                        JLAT, ic%ts_count, ICAN, ICAN + 1, IGND, IDISP, IZREF, &
                        IWF, IPAI, IHGT, IALC, IALS, IALG)

!          * SURFACE TEMPERATURE AND FLUX CALCULATIONS.
            call CLASST(TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG, &
                        THICEC, THICEG, HCPC, HCPG, TCTOPC, TCBOTC, TCTOPG, TCBOTG, &
                        GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, G12CS, G12GS, &
                        G23C, G23G, G23CS, G23GS, QFREZC, QFREZG, QMELTC, QMELTG, &
                        EVAPC, EVAPCG, EVAPG, EVAPCS, EVPCSG, EVAPGS, TCANO, TCANS, &
                        RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP, CHCAPS, TPONDC, TPONDG, &
                        TPNDCS, TPNDGS, TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, &
                        ITCTGAT, cdv%CDH, cdv%CDM, cdv%HFS, cdv%TFX, cdv%QEVP, cdv%QFS, cdv%QFX, &
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

!          * WATER BUDGET CALCULATIONS.
            if (ic%now_jday == 1 .and. ic%ts_daily == 48) then
       ! bruce davison - only increase NMELT if we don't start the run on January 1st, otherwise t0_ACC allocation is too large
       ! and the model crashes if the compiler is checking for array bounds when t0_ACC is passed into CLASSW with size NMELT
                if (JDAY_START == 1 .and. ic%ts_count < 49) then
                    continue ! NMELT should stay = 1
                else
                    NMELT = NMELT + 1
                end if
                CUMSNOWINFILCS = 0.0
                CUMSNOWINFILGS = 0.0
                INFILTYPE = 2
            end if

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
                        csfv%ISND, IGDRGAT, IWF, NML, il1, il2, ic%ts_count, &
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

!          * SINGLE COLUMN BLOWING SNOW CALCULATIONS.
            if (PBSMFLAG == 1) then
                call PBSMrun(ZSNOW, cpv%WSNO, cpv%SNO, cpv%RHOS, cpv%TSNO, cdv%HTCS, &
                             ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
                             HCPSCS, HCPSGS, HCPSC, HCPSG, &
                             TSNOWC, TSNOWG, TSNOCS, TSNOGS, &
                             RHOSC, RHOSG, RHOSCS, RHOSGS,&
                             XSNOWC, XSNOWG, XSNOCS, XSNOGS, &
                             WSNOCS, WSNOGS, &
                             cdv%FC, cdv%FG, cdv%FCS, cdv%FGS, &
                             fetchGAT, N_SGAT, A_SGAT, HtGAT, &
                             cdv%SFCT, cdv%SFCU, cdv%SFCQ, cfi%PRES, cfi%PRE, &
                             DrySnowGAT, SnowAgeGAT, DriftGAT, SublGAT, &
                             TSNOdsGAT, &
                             NML, il1, il2, ic%ts_count, catv%ZRFM, ZOMLCS, ZOMLNS)
            end if

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

!          *Redistribute blowing snow mass between GRUs
            call REDISTRIB_SNOW(NML, 1, NA, NTYPE, NML, cpv%TSNO, ZSNOW, &
                                cpv%RHOS, cpv%SNO, TSNOCS, ZSNOCS, HCPSCS, RHOSCS, TSNOGS, &
                                ZSNOGS, HCPSGS, RHOSGS, TSNOWC, ZSNOWC, HCPSC, RHOSC, TSNOWG, &
                                ZSNOWG, HCPSG, RHOSG, cp%GCGRD, shd%lc%ILMOS, DriftGAT, csfv%FARE, &
                                TSNOdsGAT, DistribGAT, WSNOCS, WSNOGS, cdv%FCS, cdv%FGS, cdv%FC, cdv%FG, DepositionGAT, &
                                cdv%TROO, cdv%ROFO, cdv%TROF, cdv%ROF, cdv%ROFN, cdv%PCPG, cdv%HTCS, cpv%WSNO, ic%ts_count)
            cdv%ROF = cdv%ROF - UMQ

        end if !(ipid /= 0 .or. izero == 0) then

        !> WRITE FIELDS FROM CURRENT TIME STEP TO OUTPUT FILES.
        if (WF_NUM_POINTS > 0) then
            call CLASSOUT_update_files(shd, ic)
        end if

    end subroutine

end module
