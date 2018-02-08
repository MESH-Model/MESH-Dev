module PBSM_module

    implicit none

    !> PBSM parameters.
    !*  fetch: fetch distance (m)
    !*  Ht: vegetation height (m)
    !*  N_S: vegetation density (number/m^2)
    !*  A_S: vegetation width (m)
    !*  Distrib: Inter-GRU snow redistribution factor
    type pbsm_parameters
        real, dimension(:), allocatable :: &
            fetch, Ht, N_S, A_S, Distrib
    end type

    !> PBSM variables.
    !*  DrySnow:
    !*      0 = air temperature above 0 degC
    !*      1 = air temperature below 0 degC
    !*  SnowAge: hours since last snowfall
    !*  Drift: blowing snow transport (kg/m^2)
    !*  Subl: blowing snow sublimation (kg/m^2)
    type pbsm_variables
        real, dimension(:), allocatable :: &
            DrySnow, SnowAge, &
            TSNOds, &
            Drift, Subl, Deposition
    end type

    !> Internal CLASS variables pulled from CLASSW for PBSM.
    real, dimension(:), allocatable, save :: &
        ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
        HCPSCS, HCPSGS, HCPSC, HCPSG, &
        TSNOWC, TSNOWG, &
        RHOSC, RHOSG, &
        XSNOWC, XSNOWG, XSNOCS, XSNOGS

    type pbsm_container
        type(pbsm_parameters) :: pm_gru, pm_grid, pm
        type(pbsm_variables) :: vs
        logical :: PROCESS_ACTIVE = .false.
    end type

    type(pbsm_container), save :: pbsm

    contains

    !> Parse PBSM flag from run_options.ini/
    !*  0 = Blowing snow calculations are not made (default).
    !*  1 = Blowing snow transport, sublimation & inter-GRU redistribution calculations are made.
    subroutine PBSM_parse_flag(in_line)

        use strings

        !> Input is PBSMFLAG as read from file.
        character(len = *), intent(in) :: in_line

        !> Local variables.
        character(len = len(in_line)), dimension(10) :: out_args
        integer nargs

        !> Parse the arguments of the file and assign options.
        pbsm%PROCESS_ACTIVE = .false.
        call parse(in_line, ' ', out_args, nargs)
        if (.not. nargs > 1) return
        select case (lowercase(out_args(2)))
            case ('1')
                pbsm%PROCESS_ACTIVE = .true.
            case ('on')
                pbsm%PROCESS_ACTIVE = .true.
        end select

        return

    end subroutine

    !> Set initial SnowAge & DrySnow values for PBSM calculations
    !> (MK MacDonald, Sept 2010).
    subroutine PBSM_init(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_variables
        use sa_mesh_utilities
        use model_dates
        use climate_forcing

        use RUNCLASS36_constants

        !> Input variables.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> Local variables.
        integer NML, k, m

        !> Return if PBSM is not active.
        if (.not. pbsm%PROCESS_ACTIVE) return

        !> Allocate variables.
        NML = shd%lc%NML
        allocate(pbsm%vs%DrySnow(NML), pbsm%vs%SnowAge(NML), &
                 pbsm%vs%TSNOds(NML), &
                 pbsm%vs%Drift(NML), pbsm%vs%Subl(NML), pbsm%vs%Deposition(NML))
        pbsm%vs%DrySnow = 0.0; pbsm%vs%SnowAge = 0.0;
        pbsm%vs%TSNOds = 0.0
        pbsm%vs%Drift = 0.0; pbsm%vs%Subl = 0.0; pbsm%vs%Deposition = 0.0;

        !> Allocate variables for CLASSW.
        !> These are initialized in WPREP.
        allocate(ZSNOCS(NML), ZSNOGS(NML), ZSNOWC(NML), ZSNOWG(NML), &
                 HCPSCS(NML), HCPSGS(NML), HCPSC(NML), HCPSG(NML), &
                 TSNOWC(NML), TSNOWG(NML), &
                 RHOSC(NML), RHOSG(NML), &
                 XSNOWC(NML), XSNOWG(NML), XSNOCS(NML), XSNOGS(NML))

        !> Set initial SnowAge & DrySnow values for PBSM calculations.
        do k = il1, il2
            if (stas%sno%sno(k) <= 0.0) then
                pbsm%vs%DrySnow(k) = 0.0   !1 = snowpack is dry (i.e. cold)
                pbsm%vs%SnowAge(k) = 0.0   !hours since last snowfall
            else if (cm%dat(ck%TT)%GAT(k) >= TFREZ) then
                pbsm%vs%DrySnow(k) = 0.0
                pbsm%vs%SnowAge(k) = 48.0   !assume 48 hours since last snowfall
            else
                pbsm%vs%DrySnow(k) = 1.0
                pbsm%vs%SnowAge(k) = 48.0
            end if
        end do

        !> Write summary to output file.
!todo: proper file unit.
        if (ipid == 0) then
            write(ECHO_TXT_IUN, 1000) 'PBSM (blowing snow) component ACTIVE.'
            write(ECHO_TXT_IUN, 1110) 'PBSMFLAG', 'on'
            write(ECHO_TXT_IUN, 1010) 'GRUs ->', (m, m = 1, shd%lc%NTYPE)
            write(ECHO_TXT_IUN, 1010) repeat('-', 17), (repeat('-', 16), m = 1, shd%lc%NTYPE)
            write(ECHO_TXT_IUN, 1110) 'fetch', (pbsm%pm_gru%fetch(m), m = 1, shd%lc%NTYPE)
            write(ECHO_TXT_IUN, 1110) 'Ht', (pbsm%pm_gru%Ht(m), m = 1, shd%lc%NTYPE)
            write(ECHO_TXT_IUN, 1110) 'N_S', (pbsm%pm_gru%N_S(m), m = 1, shd%lc%NTYPE)
            write(ECHO_TXT_IUN, 1110) 'A_S', (pbsm%pm_gru%A_S(m), m = 1, shd%lc%NTYPE)
            write(ECHO_TXT_IUN, 1110) 'Distrib', (pbsm%pm_gru%Distrib(m), m = 1, shd%lc%NTYPE)
            write(ECHO_TXT_IUN, 1000)
        end if

1000    format('!> ', (a))
1010    format('!> ', a17, 99(1x, g16.9))
1110    format(a20, 99(1x, g16.9))

    end subroutine

    !> Single column blowing snow calculations.
    subroutine PBSM_within_tile( &
        ZSNOW, WSNO, SNO, RHOS, TSNO, HTCS, &
        TSNOCS, TSNOGS, RHOSCS, RHOSGS, WSNOCS, WSNOGS, &
        FC, FG, FCS, FGS, &
        SFCT, SFCU, SFCQ, ZRFM, ZOMLCS, ZOMLNS, &
        NML, &
        shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_variables
        use model_dates
        use climate_forcing

        !> Input variables from driver.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> Input/output variables.
        integer, intent(in) :: NML
        real, dimension(NML) :: &
            ZSNOW, WSNO, SNO, RHOS, TSNO, HTCS, &
            TSNOCS, TSNOGS, RHOSCS, RHOSGS, WSNOCS, WSNOGS, &
            FC, FG, FCS, FGS, &
            SFCT, SFCU, SFCQ, ZRFM, ZOMLCS, ZOMLNS

        !> Return if PBSM is not active.
        if (.not. pbsm%PROCESS_ACTIVE) return

        !> Initialize diagnostic variables for PBSM.
!+        pbsm%vs%Drift(il1:il2) = 0.0
!+        pbsm%vs%Subl(il1:il2) = 0.0
!+        pbsm%vs%Deposition(il1:il2) = 0.0

        !> Single column blowing snow calculations.
        call PBSMrun(ZSNOW, WSNO, SNO, RHOS, TSNO, HTCS, &
                     ZSNOCS, ZSNOGS, ZSNOWC, ZSNOWG, &
                     HCPSCS, HCPSGS, HCPSC, HCPSG, &
                     TSNOWC, TSNOWG, TSNOCS, TSNOGS, &
                     RHOSC, RHOSG, RHOSCS, RHOSGS,&
                     XSNOWC, XSNOWG, XSNOCS, XSNOGS, &
                     WSNOCS, WSNOGS, &
                     FC, FG, FCS, FGS, &
                     pbsm%pm%fetch, pbsm%pm%N_S, pbsm%pm%A_S, pbsm%pm%Ht, &
                     SFCT, SFCU, SFCQ, cm%dat(ck%P0)%GAT, cm%dat(ck%RT)%GAT, &
                     pbsm%vs%DrySnow, pbsm%vs%SnowAge, pbsm%vs%Drift, pbsm%vs%Subl, &
                     pbsm%vs%TSNOds, &
                     NML, il1, il2, ic%ts_count, ZRFM, ZOMLCS, ZOMLNS)

    end subroutine

    !> Distribute blowing snow mass between GRUs.
    subroutine PBSM_within_grid( &
        TSNO, ZSNOW, RHOS, SNO, TSNOCS, RHOSCS, TSNOGS, RHOSGS, &
        GC, FARE, WSNOCS, WSNOGS, FCS, FGS, FC, FG, &
        TROO, ROFO, TROF, ROF, ROFN, PCPG, HTCS, WSNO, &
        NML, &
        shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_variables
        use model_dates
        use climate_forcing

        !> Input variables from driver.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> Input/output variables.
        integer, intent(in) :: NML
        real, dimension(NML) :: &
            TSNO, ZSNOW, RHOS, SNO, TSNOCS, RHOSCS, TSNOGS, RHOSGS, &
            GC, FARE, WSNOCS, WSNOGS, FCS, FGS, FC, FG, &
            TROO, ROFO, TROF, ROF, ROFN, PCPG, HTCS, WSNO

        !> Return if PBSM is not active.
        if (.not. pbsm%PROCESS_ACTIVE) return

        !> Distribute blowing snow mass between GRUs.
        call REDISTRIB_SNOW(shd%NA, shd%lc%NTYPE, shd%NA*shd%lc%NTYPE, NML, il1, il2, TSNO, ZSNOW, &
                            RHOS, SNO, TSNOCS, ZSNOCS, HCPSCS, RHOSCS, TSNOGS, &
                            ZSNOGS, HCPSGS, RHOSGS, TSNOWC, ZSNOWC, HCPSC, RHOSC, TSNOWG, &
                            ZSNOWG, HCPSG, RHOSG, GC, shd%lc%ILMOS, pbsm%vs%Drift, FARE, &
                            pbsm%vs%TSNOds, pbsm%pm%Distrib, WSNOCS, WSNOGS, FCS, FGS, FC, FG, pbsm%vs%Deposition, &
                            TROO, ROFO, TROF, ROF, ROFN, PCPG, HTCS, WSNO, ic%ts_count)

    end subroutine

    subroutine PSBM_finalize()

    end subroutine

end module
