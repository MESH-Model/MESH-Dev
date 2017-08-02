!>
!> Description:
!>  Subroutine to read input parameters from file. Parameters shared
!>  by SA_MESH are accessible by sa_mesh_shared_variables module. Other
!>  parameters are accessible by their respecitve process module(s).
!>
subroutine read_parameters(fls, shd, cm, ierr)

    use strings
    use mpi_module
    use model_files_variables
    use sa_mesh_shared_variables
    use FLAGS
    use climate_forcing_variabletypes

    use RUNCLASS36_variables
    use RUNSVS113_variables
    use WF_ROUTE_config
    use rte_module
    use baseflow_module
    use cropland_irrigation_variables
    use PBSM_module

    implicit none

    !> Input variables.
    type(fl_ids):: fls
    type(ShedGridParams) :: shd
    type(CLIM_INFO) :: cm

    !> Output variables.
    integer, intent(out), optional :: ierr

    !> Local variables for parsing INPUTPARAMSFORM.
    character(len = 20), dimension(100) :: out_args
    integer nargs
    character(1) :: delim = ' '

    !> Local variables.
    integer NA, NAA, NTYPE, NRVR, NML, NSL, k, i, n

    !> Assign commonly used indices to local variables.
    NA = shd%NA
    NAA = shd%NAA
    NTYPE = shd%lc%NTYPE
    NML = shd%lc%NML
    NSL = shd%lc%IGND
    NRVR = shd%NRVR

    !>
    !> ALLOCATE AND INITIALIZE VARIABLES.
    !>

    !> Allocate instances of SA_MESH parameters.
    call pm_init(pm, 'tile', NML, NSL, 4, 5, ierr)
    call pm_init(pm_grid, 'grid', NA, NSL, 4, 5, ierr)
    call pm_init(pm_gru, 'gru', NTYPE, NSL, 4, 5, ierr)

    !> CLASS interflow flag.
    if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
        pm%tp%iwf(il1:il2) = RUNCLASS36_flgs%INTERFLOWFLAG
    end if

    !> WF_ROUTE (Watflood, 1988).
    if (WF_RTE_flgs%PROCESS_ACTIVE) then
        allocate(wfp%r1(NRVR), wfp%r2(NRVR), wfp%aa1(NRVR), wfp%aa2(NRVR), wfp%aa3(NRVR), wfp%aa4(NRVR), stat = ierr)
        wfp%r1 = 2.0; wfp%r2 = 0.0; wfp%aa1 = 1.0; wfp%aa2 = 11.0; wfp%aa3 = 0.43; wfp%aa4 = 1.0
    end if

    !> RPN RTE (Watflood, 2007).
    if (rteflg%PROCESS_ACTIVE) then
        allocate(rtepm%r1n(NA), rtepm%r2n(NA), rtepm%mndr(NA), rtepm%widep(NA), &
                 rtepm%aa2(NA), rtepm%aa3(NA), rtepm%aa4(NA), &
                 rtepm_iak%r1n(NRVR), rtepm_iak%r2n(NRVR), rtepm_iak%mndr(NRVR), rtepm_iak%widep(NRVR), &
                 rtepm_iak%aa2(NRVR), rtepm_iak%aa3(NRVR), rtepm_iak%aa4(NRVR), &
                 stat = ierr)
        rtepm%r1n = 0.0; rtepm%r2n = 0.0; rtepm%mndr = 1.0; rtepm%widep = 10.0
        rtepm%aa2 = 1.1; rtepm%aa3 = 0.043; rtepm%aa4 = 1.0
        rtepm_iak%r1n = 0.0; rtepm_iak%r2n = 0.0; rtepm_iak%mndr = 0.0; rtepm_iak%widep = 0.0
        rtepm_iak%aa2 = 0.0; rtepm_iak%aa3 = 0.0; rtepm_iak%aa4 = 0.0
    end if

    !> PBSM (blowing snow).
    if (pbsm%PROCESS_ACTIVE) then
        allocate( &
            pbsm%pm_gru%fetch(NTYPE), pbsm%pm_gru%Ht(NTYPE), pbsm%pm_gru%N_S(NTYPE), pbsm%pm_gru%A_S(NTYPE), &
            pbsm%pm_gru%Distrib(NTYPE), &
            pbsm%pm_grid%fetch(NA), pbsm%pm_grid%Ht(NA), pbsm%pm_grid%N_S(NA), pbsm%pm_grid%A_S(NA), pbsm%pm_grid%Distrib(NA), &
            pbsm%pm%fetch(NML), pbsm%pm%Ht(NML), pbsm%pm%N_S(NML), pbsm%pm%A_S(NML), pbsm%pm%Distrib(NML))
        pbsm%pm_gru%fetch = 0.0; pbsm%pm_gru%Ht = 0.0; pbsm%pm_gru%N_S = 0.0; pbsm%pm_gru%A_S = 0.0
        pbsm%pm_gru%Distrib = 0.0
        pbsm%pm_grid%fetch = 0.0; pbsm%pm_grid%Ht = 0.0; pbsm%pm_grid%N_S = 0.0; pbsm%pm_grid%A_S = 0.0; pbsm%pm_grid%Distrib = 0.0
        pbsm%pm%fetch = 0.0; pbsm%pm%Ht = 0.0; pbsm%pm%N_S = 0.0; pbsm%pm%A_S = 0.0; pbsm%pm%Distrib = 0.0
    end if

    !> FROZENSOILINFILFLAG 1.
    if (FROZENSOILINFILFLAG == 1) then
        allocate(hp%FRZCROW(NA, NTYPE), stat = ierr)
        hp%FRZCROW = 0.0
        NYEARS = max(ic%stop%year - ic%start%year + 1, 1)
        allocate(t0_ACC(NYEARS))
        t0_ACC = 0.0
    end if

    !> IWF 2 (PDMROF) and IWF 3 (LATFLOW).
!temp: in case IWF is overwritten in hydrology.ini or parameters.csv
!    if (any(pm%tp%iwf == 2) .or. any(pm%tp%iwf == 3)) then
        allocate( &
            hp%CMAXROW(NA, NTYPE), hp%CMINROW(NA, NTYPE), hp%BROW(NA, NTYPE), hp%K1ROW(NA, NTYPE), hp%K2ROW(NA, NTYPE), stat = ierr)
        hp%CMAXROW = 0.0; hp%CMINROW = 0.0; hp%BROW = 0.0; hp%K1ROW = 0.0; hp%K2ROW = 0.0
!    end if

    !> BASEFLOWFLAG 1 (Luo, 2012).
    if (bflm%BASEFLOWFLAG == 1) then
        allocate(bflm%pm%dgw(NML), bflm%pm%agw(NML), bflm%pm_gru%dgw(NTYPE), bflm%pm_gru%agw(NTYPE))
        bflm%pm%dgw = 0.0; bflm%pm%agw = 0.0; bflm%pm_gru%dgw = 0.0; bflm%pm_gru%agw = 0.0
    end if

    !> BASEFLOWFLAG == 2 (lower zone storage).
    if (bflm%BASEFLOWFLAG == 2) then
        allocate(bflm%pm%pwr(NML), bflm%pm%flz(NML), &
                 bflm%pm_iak%pwr(NRVR), bflm%pm_iak%flz(NRVR), bflm%pm_gru%pwr(NTYPE), bflm%pm_gru%flz(NTYPE), &
                 bflm%pm_grid%pwr(NA), bflm%pm_grid%flz(NA))
        bflm%pm%pwr = 0.0; bflm%pm%flz = 0.0
        bflm%pm_iak%pwr = 0.0; bflm%pm_iak%flz = 0.0; bflm%pm_gru%pwr = 0.0; bflm%pm_gru%flz = 0.0
        bflm%pm_grid%pwr = 0.0; bflm%pm_grid%flz = 0.0
    end if

    !> Cropland irrigation module (CROPLANDIRRIGATION > 0).
    if (cifg%PROCESS_ACTIVE) then
        allocate( &
            ciprot%jdsow(NTYPE), ciprot%ldini(NTYPE), ciprot%lddev(NTYPE), ciprot%ldmid(NTYPE), ciprot%ldlate(NTYPE), &
            ciprot%Kcini(NTYPE), ciprot%Kcdev(NTYPE), ciprot%Kcmid(NTYPE), ciprot%Kclate(NTYPE))
        ciprot%jdsow = 0; ciprot%ldini = 0; ciprot%lddev = 0; ciprot%ldmid = 0; ciprot%ldlate = 0
        ciprot%Kcini = 0.0; ciprot%Kcdev = 0.0; ciprot%Kcmid = 0.0; ciprot%Kclate = 0.0
        allocate( &
            cip%jdsow(NML), cip%ldini(NML), cip%lddev(NML), cip%ldmid(NML), cip%ldlate(NML), &
            cip%Kcini(NML), cip%Kcdev(NML), cip%Kcmid(NML), cip%Kclate(NML))
        cip%jdsow = 0; cip%ldini = 0; cip%lddev = 0; cip%ldmid = 0; cip%ldlate = 0
        cip%Kcini = 0.0; cip%Kcdev = 0.0; cip%Kcmid = 0.0; cip%Kclate = 0.0
    end if

    !>
    !> READ FROM FILE.
    !>

    !> Parse the INPUTPARAMSFORM control flag to get INPUTPARAMSFORMFLAG.
    !> Default behaviour is to read the 'ini' format files.
    INPUTPARAMSFORMFLAG = radix(INPUTPARAMSFORMFLAG)**0
    call parse(INPUTPARAMSFORM, delim, out_args, nargs)
    do n = 2, nargs
        select case (out_args(n))
            case ('only')
                INPUTPARAMSFORMFLAG = 0
            case ('r2c')
                INPUTPARAMSFORMFLAG = INPUTPARAMSFORMFLAG + radix(INPUTPARAMSFORMFLAG)**1
            case ('csv')
                INPUTPARAMSFORMFLAG = INPUTPARAMSFORMFLAG + radix(INPUTPARAMSFORMFLAG)**2
        end select
    end do

    !> Check for a bad value of INPUTPARAMSFORMFLAG.
    if (INPUTPARAMSFORMFLAG == 0) then
        if (ipid == 0) then
            print "(1x, 'ERROR: Bad or unsupported input parameter file format.')"
            print "(3x, 'Revise INPUTPARAMSFORMFLAG in ', (a), '.')", trim(adjustl(fls%fl(mfk%f53)%fn))
        end if
        stop
    end if

    !> Read from the 'ini' files.
    if (btest(INPUTPARAMSFORMFLAG, 0)) then
        call READ_PARAMETERS_CLASS(shd, fls, cm)
        call READ_PARAMETERS_HYDROLOGY(shd, fls)
        call READ_SOIL_INI(shd, fls)
    end if

    !> Read from the 'r2c' file.
    if (btest(INPUTPARAMSFORMFLAG, 1)) then
        call read_parameters_r2c(shd, 100, 'MESH_parameters.r2c')
    end if

    !> Read from the 'csv' file.
    if (btest(INPUTPARAMSFORMFLAG, 2)) then
        call read_parameters_csv(shd, 100, 'MESH_parameters.csv')
    end if

    !>
    !> DISTRIBUTE.
    !>

    !> Constants.

    !> RUNCLASS36 and RUNSVS113.
    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
        pm%sfp%zrfm(il1:il2) = pm_gru%sfp%zrfm(1)
        pm%sfp%zrfh(il1:il2) = pm_gru%sfp%zrfh(1)
    end if

    !> RUNCLASS36.
    if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
        pm%sfp%zbld(il1:il2) = pm_gru%sfp%zbld(1)
        pm%tp%gc(il1:il2) = pm_gru%tp%gc(1)
    end if

    !> Parameters.

    !> From GRU.
    if (btest(INPUTPARAMSFORMFLAG, 0) .or. btest(INPUTPARAMSFORMFLAG, 2)) then
        do k = il1, il2

            !> GRU index.
            i = shd%lc%JLMOS(k)

            !> RUNCLASS36 and RUNSVS113.
            if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                pm%cp%fcan(k, :) = pm_gru%cp%fcan(i, :)
                pm%cp%lnz0(k, :) = pm_gru%cp%lnz0(i, :)
                pm%slp%sdep(k) = pm_gru%slp%sdep(i)
                pm%tp%xslp(k) = pm_gru%tp%xslp(i)
                pm%hp%dd(k) = pm_gru%hp%dd(i)/1000.0
                pm%slp%sand(k, :) = pm_gru%slp%sand(i, :)
                pm%slp%clay(k, :) = pm_gru%slp%clay(i, :)
            end if

            !> RUNCLASS36.
            if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
                pm%tp%fare(k) = pm_gru%tp%fare(i)
                pm%tp%mid(k) = max(1, pm_gru%tp%mid(i))
                if (pm_gru%tp%iwf(i) /= -1) pm%tp%iwf(k) = pm_gru%tp%iwf(i)
                pm%cp%alvc(k, :) = pm_gru%cp%alvc(i, :)
                pm%cp%alic(k, :) = pm_gru%cp%alic(i, :)
                pm%cp%lamx(k, :) = pm_gru%cp%lamx(i, :)
                pm%cp%lamn(k, :) = pm_gru%cp%lamn(i, :)
                pm%cp%cmas(k, :) = pm_gru%cp%cmas(i, :)
                pm%cp%root(k, :) = pm_gru%cp%root(i, :)
                pm%cp%rsmn(k, :) = pm_gru%cp%rsmn(i, :)
                pm%cp%qa50(k, :) = pm_gru%cp%qa50(i, :)
                pm%cp%vpda(k, :) = pm_gru%cp%vpda(i, :)
                pm%cp%vpdb(k, :) = pm_gru%cp%vpdb(i, :)
                pm%cp%psga(k, :) = pm_gru%cp%psga(i, :)
                pm%cp%psgb(k, :) = pm_gru%cp%psgb(i, :)
                pm%hp%drn(k) = pm_gru%hp%drn(i)
                pm%hp%mann(k) = pm_gru%hp%mann(i)
                pm%hp%grkf(k) = pm_gru%hp%grkf(i)
                pm%hp%ks(k) = pm_gru%hp%ks(i)
                pm%slp%orgm(k, :) = pm_gru%slp%orgm(i, :)
                pm%snp%zsnl(k) = pm_gru%snp%zsnl(i)
                pm%sfp%zplg(k) = pm_gru%sfp%zplg(i)
                pm%snp%zpls(k) = pm_gru%snp%zpls(i)
            end if

            !> BASEFLOWFLAG 1 (Luo, 2012).
            if (bflm%BASEFLOWFLAG == 1) then
                bflm%pm%dgw(k) = bflm%pm_gru%dgw(i)
                bflm%pm%agw(k) = bflm%pm_gru%agw(i)
            end if

            !> BASEFLOWFLAG == 2 (lower zone storage).
            if (bflm%BASEFLOWFLAG == 2) then
                bflm%pm%pwr(k) = bflm%pm_gru%pwr(i)
                bflm%pm%flz(k) = bflm%pm_gru%flz(i)
            end if

            !> Cropland irrigation module.
            if (cifg%PROCESS_ACTIVE) then
                cip%jdsow(k) = ciprot%jdsow(i)
                cip%ldini(k) = ciprot%ldini(i)
                cip%lddev(k) = ciprot%lddev(i)
                cip%ldmid(k) = ciprot%ldmid(i)
                cip%ldlate(k) = ciprot%ldlate(i)
                cip%Kcini(k) = ciprot%Kcini(i)
                cip%Kcdev(k) = ciprot%Kcdev(i)
                cip%Kcmid(k) = ciprot%Kcmid(i)
                cip%Kclate(k) = ciprot%Kclate(i)
            end if

            !> PBSM (blowing snow).
            if (pbsm%PROCESS_ACTIVE) then
                pbsm%pm%fetch(k) = pbsm%pm_gru%fetch(i)
                pbsm%pm%Ht(k) = pbsm%pm_gru%Ht(i)
                pbsm%pm%N_S(k) = pbsm%pm_gru%N_S(i)
                pbsm%pm%A_S(k) = pbsm%pm_gru%A_S(i)
                pbsm%pm%Distrib(k) = pbsm%pm_gru%Distrib(i)
            end if

        end do !k = il1, il2
    end if

    !> From river class (IAK) if not read by grid.
    if (NRVR > 0) then
        do k = 1, NAA

            !> River class index (IAK).
            i = shd%IAK(k)

            !> BASEFLOWFLAG == 2 (lower zone storage).
            if (bflm%BASEFLOWFLAG == 2) then
                if (bflm%pm_iak%pwr(i) /= 0.0) bflm%pm_grid%pwr(k) = bflm%pm_iak%pwr(i)
                if (bflm%pm_iak%flz(i) /= 0.0) bflm%pm_grid%flz(k) = bflm%pm_iak%flz(i)
            end if

            !> RPN RTE (Watflood, 2007).
            if (rteflg%PROCESS_ACTIVE) then
                if (rtepm_iak%r1n(i) /= 0.0) rtepm%r1n(k) = rtepm_iak%r1n(i)
                if (rtepm_iak%r2n(i) /= 0.0) rtepm%r2n(k) = rtepm_iak%r2n(i)
                if (rtepm_iak%mndr(i) /= 0.0) rtepm%mndr(k) = rtepm_iak%mndr(i)
                if (rtepm_iak%widep(i) /= 0.0) rtepm%widep(k) = rtepm_iak%widep(i)
                if (rtepm_iak%aa2(i) /= 0.0) rtepm%aa2(k) = rtepm_iak%aa2(i)
                if (rtepm_iak%aa3(i) /= 0.0) rtepm%aa3(k) = rtepm_iak%aa3(i)
                if (rtepm_iak%aa4(i) /= 0.0) rtepm%aa4(k) = rtepm_iak%aa4(i)
            end if

        end do !k = 1, NAA
        do k = il1, il2

            !> Grid index.
            i = shd%lc%ILMOS(k)

            !> BASEFLOWFLAG == 2 (lower zone storage).
            if (bflm%BASEFLOWFLAG == 2) then
                if (bflm%pm_iak%pwr(shd%IAK(i)) /= 0.0) bflm%pm%pwr(k) = bflm%pm_iak%pwr(shd%IAK(i))
                if (bflm%pm_iak%flz(shd%IAK(i)) /= 0.0) bflm%pm%flz(k) = bflm%pm_iak%flz(shd%IAK(i))
            end if

        end do !k = il1, il2
    end if

!todo: Formally change these to grid parameters, remove from shd
    !> RUNCLASS36 and RUNSVS113.
    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
        if (allocated(shd%SLOPE_INT)) then
            do k = il1, il2
                pm%tp%xslp(k) = shd%SLOPE_INT(shd%lc%ILMOS(k))
            end do
        end if
        if (allocated(shd%DRDN)) then
            do k = il1, il2
                pm%hp%dd(k) = shd%DRDN(shd%lc%ILMOS(k))
            end do
        end if
    end if

    !> From grid.
    if (btest(INPUTPARAMSFORMFLAG, 1)) then
        do k = il1, il2

            !> Grid index.
            i = shd%lc%ILMOS(k)

            !> RUNCLASS36 and RUNSVS113.
            if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                if (any(pm_grid%cp%fcan(i, :) /= 0.0)) pm%cp%fcan(k, :) = pm_grid%cp%fcan(i, :)
                if (any(pm_grid%cp%lnz0(i, :) /= 0.0)) pm%cp%lnz0(k, :) = pm_grid%cp%lnz0(i, :)
                if (pm_grid%slp%sdep(i) /= 0.0) pm%slp%sdep(k) = pm_grid%slp%sdep(i)
                if (pm_grid%tp%xslp(i) /= 0.0) pm%tp%xslp(k) = pm_grid%tp%xslp(i)
                if (pm_grid%hp%dd(i) /= 0.0) pm%hp%dd(k) = pm_grid%hp%dd(i)
                if (any(pm_grid%slp%sand(i, :) /= 0.0)) pm%slp%sand(k, :) = pm_grid%slp%sand(i, :)
                if (any(pm_grid%slp%clay(i, :) /= 0.0)) pm%slp%clay(k, :) = pm_grid%slp%clay(i, :)
            end if

            !> RUNCLASS36.
            if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
                if (pm_grid%tp%iwf(i) /= -1) pm%tp%iwf(k) = pm_grid%tp%iwf(i)
            end if

            !> BASEFLOWFLAG == 2 (lower zone storage).
            if (bflm%BASEFLOWFLAG == 2) then
                if (bflm%pm_grid%pwr(i) /= 0.0) bflm%pm%pwr(k) = bflm%pm_grid%pwr(i)
                if (bflm%pm_grid%flz(i) /= 0.0) bflm%pm%flz(k) = bflm%pm_grid%flz(i)
            end if

            !> PBSM (blowing snow).
            if (pbsm%PROCESS_ACTIVE) then
                if (pbsm%pm_grid%fetch(i) /= 0.0) pbsm%pm%fetch(k) = pbsm%pm_grid%fetch(i)
                if (pbsm%pm_grid%Ht(i) /= 0.0) pbsm%pm%Ht(k) = pbsm%pm_grid%Ht(i)
                if (pbsm%pm_grid%N_S(i) /= 0.0) pbsm%pm%N_S(k) = pbsm%pm_grid%N_S(i)
                if (pbsm%pm_grid%A_S(i) /= 0.0) pbsm%pm%A_S(k) = pbsm%pm_grid%A_S(i)
                if (pbsm%pm_grid%Distrib(i) /= 0.0) pbsm%pm%Distrib(k) = pbsm%pm_grid%Distrib(i)
            end if

        end do !k = il1, il2
    end if

end subroutine
