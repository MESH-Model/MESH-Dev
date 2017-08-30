module RUNLAKE_config

    use RUNLAKE_variables

    implicit none

    contains

    subroutine RUNLAKE_init(shd)

        use sa_mesh_shared_variables

        use RUNCLASS36_variables

        implicit none

        !> Input variables.
        type(ShedGridParams) :: shd

        !> Local variables.
        integer NMW, NBS, NLYRMAX, i

        !> Local variables.
        NBS = lm%op%NBS
        NLYRMAX = lm%op%NLYRMAX
        NMW = shd%wc%NML

        !> Option flags.
        lm%op%ISLFD = ISLFD
        lm%op%IZREF = IZREF
        lm%op%ITG = ITG
        lm%op%IPCP = IPCP
        lm%op%IALS = IALS

        !> Driving variables.
        allocate( &
            lfv%QSWINV(NMW), lfv%QSWINI(NMW), lfv%QLWIN(NMW), lfv%PRES(NMW), &
            lfv%ZREFH(NMW), lfv%QA(NMW), lfv%TA(NMW), lfv%ZREFM(NMW), lfv%UWIND(NMW), lfv%VWIND(NMW), &
            lfv%RHOAIR(NMW), lfv%TADP(NMW), lfv%PADRY(NMW), lfv%VPD(NMW), lfv%RHOSNI(NMW), &
            lfv%RPRE(NMW), lfv%SPRE(NMW), &
            lfv%PCPR(NMW), lfv%RPCP(NMW), lfv%TRPCP(NMW), lfv%SPCP(NMW), lfv%TSPCP(NMW), &
            lfv%CSZ(NMW), lfv%RADJ(NMW))
        lfv%RPRE = 0.0; lfv%SPRE = 0.0

        !> Diagnostic variables.
        allocate(ZDH(NMW), ZDM(NMW), CDH(NMW), CDM(NMW))

        !> Lake tile parameters.
        allocate( &
            lm%pm%HLAK(NMW), lm%pm%LLAK(NMW), lm%pm%BLAK(NMW), lm%pm%NLAK(NMW), lm%pm%TLAK(NMW, NLYRMAX))
        allocate( &
            ASVL(NMW), ASIL(NMW), BCSNL(NMW), REFL(NMW))

        !> Lake prognostic variables.
        allocate( &
            lpv%EXPW(NMW), lpv%DTEMP(NMW), lpv%HDPTH(NMW), lpv%DELU(NMW), &
            lpv%GRED(NMW), lpv%TKELAK(NMW), lpv%T0LAK(NMW), lpv%LKICEH(NMW), &
            lpv%RHOMIX(NMW), lpv%TSED(NMW), lpv%SNICEH(NMW), lpv%ROFICEH(NMW))

        !> Lake prognostic variables (integrated over DELT_L).
        allocate( &
            lpvi%EXPW(NMW), lpvi%DTEMP(NMW), lpvi%HDPTH(NMW), lpvi%DELU(NMW), &
            lpvi%GRED(NMW), lpvi%TKELAK(NMW), lpvi%T0LAK(NMW), lpvi%LKICEH(NMW), &
            lpvi%RHOMIX(NMW), lpvi%TSED(NMW), lpvi%SNICEH(NMW), lpvi%ROFICEH(NMW))

        !> Lake diagnostic variables.
        allocate( &
            ldv%CTLSTP(NMW), ldv%HFSL(NMW), ldv%HEVL(NMW), ldv%FSGL(NMW), &
            ldv%FLGL(NMW), ldv%HMFL(NMW), ldv%HTCL(NMW), ldv%FICE(NMW), &
            ldv%FLS(NMW), ldv%G0SL(NMW), ldv%FSGSL(NMW), ldv%FLGSL(NMW), &
            ldv%HFSSL(NMW), ldv%HEVSL(NMW), ldv%HMFNL(NMW), ldv%HTCSL(NMW), &
            ldv%PCPL(NMW), ldv%PCPNL(NMW), ldv%QFL(NMW), ldv%QFNL(NMW), &
            ldv%ROFNL(NMW), ldv%SFTL(NMW), ldv%SFUL(NMW), ldv%SFVL(NMW), &
            ldv%SFQL(NMW), ldv%SFHL(NMW), ldv%QLWOL(NMW), ldv%ALVL(NMW), &
            ldv%ALIL(NMW), ldv%EFL(NMW), ldv%GTL(NMW), ldv%QGL(NMW), ldv%DRL(NMW), &
            ldv%PETL(NMW), ldv%QSENL(NMW), ldv%TFXL(NMW), ldv%QEVPL(NMW), &
            ldv%QFSL(NMW), ldv%QFXL(NMW), ldv%SNOL(NMW), ldv%RHOSL(NMW), &
            ldv%TSNOL(NMW), ldv%ALBSL(NMW), ldv%WSNOL(NMW))
        allocate(ldv%FSDBL(NMW, NBS), ldv%FSFBL(NMW, NBS), ldv%FSSBL(NMW, NBS))

        !> Lake diagnostic variables (integrated over DELT_L).
        allocate( &
            ldvi%CTLSTP(NMW), ldvi%HFSL(NMW), ldvi%HEVL(NMW), ldvi%FSGL(NMW), &
            ldvi%FLGL(NMW), ldvi%HMFL(NMW), ldvi%HTCL(NMW), ldvi%FICE(NMW), &
            ldvi%FLS(NMW), ldvi%G0SL(NMW), ldvi%FSGSL(NMW), ldvi%FLGSL(NMW), &
            ldvi%HFSSL(NMW), ldvi%HEVSL(NMW), ldvi%HMFNL(NMW), ldvi%HTCSL(NMW), &
            ldvi%PCPL(NMW), ldvi%PCPNL(NMW), ldvi%QFL(NMW), ldvi%QFNL(NMW), &
            ldvi%ROFNL(NMW), ldvi%SFTL(NMW), ldvi%SFUL(NMW), ldvi%SFVL(NMW), &
            ldvi%SFQL(NMW), ldvi%SFHL(NMW), ldvi%QLWOL(NMW), ldvi%ALVL(NMW), &
            ldvi%ALIL(NMW), ldvi%EFL(NMW), ldvi%GTL(NMW), ldvi%QGL(NMW), ldvi%DRL(NMW), &
            ldvi%PETL(NMW), ldvi%QSENL(NMW), ldvi%TFXL(NMW), ldvi%QEVPL(NMW), &
            ldvi%QFSL(NMW), ldvi%QFXL(NMW), ldvi%SNOL(NMW), ldvi%RHOSL(NMW), &
            ldvi%TSNOL(NMW), ldvi%ALBSL(NMW), ldvi%WSNOL(NMW))

!        call OPEN_LAKE_OUTPUT_FILES()

    end subroutine

    subroutine OPEN_LAKE_OUTPUT_FILES()

        use mpi_module
        use RUNLAKE_variables
        use RUNCLASS36_variables

        implicit none

        integer j, ierr, k

        character(len = 1) fmt

        open(117, file = './' // trim(adjustl(LAKEOUTDIR)) // '/fort.117', status = 'unknown', iostat = ierr)
        if (ierr /= 0) then
            print *
            print *, 'The output directory does not exist: ' // trim(adjustl(LAKEOUTDIR))
            print *, 'Create the folder or update the path in MESH_parameters_LAKE.ini.'
            stop
        else
            close(117, status = 'delete')
        end if

        !>
        !> LAKE DIAGNOSTIC OUTPUT FILES.
        !>

        !> Write project header information.
        do j = 1, 9
            if (j /= 8) then
                k = j
                write(fmt, '(i1)') k
                open(70 + j, file = './' // trim(adjustl(LAKEOUTDIR)) // '/LAKE.of' // trim(adjustl(fmt)))
                write(70 + j, 6001) TITLE1, TITLE2, TITLE3, TITLE4, TITLE5, TITLE6
                write(70 + j, 6002) NAME1, NAME2, NAME3, NAME4, NAME5, NAME6
!                write(70 + j, 6003) PLACE1, PLACE2, PLACE3, PLACE4, PLACE5, PLACE6
            end if
        end do

        write(71, 7011)
        write(72, 7012)
        write(73, 7013)
        write(74, 7014)
        write(75, 7015)
        write(76, 7016)
        write(77, 7017)
        write(79, 7019)

        !> RUNLAKE format statements for diagnostic output files.

7011    format('YEAR DAY  HR MIN     E0     F0      Q*      Q0      L*     Hs      He     T0      Lake Ice')
7012    format('YEAR DAY  HR MIN     TEMP ')
7013    format('YEAR DAY  HR MIN     FQU      BFLX      DISS    TKE DELU HDPTH JMIX ZMIX  TMIX    DTEMP    FSHEAR FENTRA')
7014    format('YEAR DAY  HR MIN     U*     GRED      WEDB        HDPTH DELTHRM DELU')
7015    format('YEAR DAY  HR MIN     TSNOW   WSNOW    RHOSNO   ZSNOW')
7016    format('YEAR DAY  HR MIN     QSWNS   QTRANSL  QLNET    QSENSS  QEVAPS   QMELT   GZEROSL')
7017    format('YEAR DAY  HR MIN     QSWIN   FSGL    QFLX(1m)')
7019    format('YEAR DAY  HR MIN     EnergyBalance')

6001    format('CLASS TEST RUN:     ', 6a4)
6002    format('RESEARCHER:         ', 6a4)
6003    format('INSTITUTION:        ', 6a4)

    end subroutine

end module
