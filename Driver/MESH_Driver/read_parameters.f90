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
    use WF_ROUTE_config
    use rte_module
    use baseflow_module
    use cropland_irrigation_variables

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
    integer NA, NAA, NTYPE, NRVR, NML, NSL, k, i

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

    !> WF_ROUTE (Watflood, 1988).
    if (WF_RTE_flgs%PROCESS_ACTIVE) then
        allocate(wfp%r1(NRVR), wfp%r2(NRVR), wfp%aa1(NRVR), wfp%aa2(NRVR), wfp%aa3(NRVR), wfp%aa4(NRVR), stat = ierr)
        wfp%r1 = 2.0; wfp%r2 = 0.0; wfp%aa1 = 1.0; wfp%aa2 = 11.0; wfp%aa3 = 0.43; wfp%aa4 = 1.0
    end if

    !> RPN RTE (Watflood, 2007).
    if (rteflg%PROCESS_ACTIVE) then
        allocate(rtepm%r1n(NA), rtepm%r2n(NA), rtepm%mndr(NA), rtepm%widep(NA), &
                 rtepm%flz(NA), rtepm%pwr(NA), &
                 rtepm%aa2(NA), rtepm%aa3(NA), rtepm%aa4(NA), &
                 rtepm_iak%r1n(NRVR), rtepm_iak%r2n(NRVR), rtepm_iak%mndr(NRVR), rtepm_iak%widep(NRVR), &
                 rtepm_iak%flz(NRVR), rtepm_iak%pwr(NRVR), &
                 rtepm_iak%aa2(NRVR), rtepm_iak%aa3(NRVR), rtepm_iak%aa4(NRVR), &
                 stat = ierr)
        rtepm%r1n = 0.0; rtepm%r2n = 0.0; rtepm%mndr = 1.0; rtepm%widep = 10.0
        rtepm%flz = 1.0E-06; rtepm%pwr = 3.0
        rtepm%aa2 = 1.1; rtepm%aa3 = 0.043; rtepm%aa4 = 1.0
        rtepm_iak%r1n = 0.0; rtepm_iak%r2n = 0.0; rtepm_iak%mndr = 1.0; rtepm_iak%widep = 10.0
        rtepm_iak%flz = 1.0E-06; rtepm_iak%pwr = 3.0
        rtepm_iak%aa2 = 1.1; rtepm_iak%aa3 = 0.043; rtepm_iak%aa4 = 1.0
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
    if (IWF == 2 .or. IWF == 3) then
        allocate( &
            hp%CMAXROW(NA, NTYPE), hp%CMINROW(NA, NTYPE), hp%BROW(NA, NTYPE), hp%K1ROW(NA, NTYPE), hp%K2ROW(NA, NTYPE), stat = ierr)
        hp%CMAXROW = 0.0; hp%CMINROW = 0.0; hp%BROW = 0.0; hp%K1ROW = 0.0; hp%K2ROW = 0.0
    end if

    !> PBSMFLAG 1.
    if (PBSMFLAG == 1) then
        allocate( &
            hp%fetchROW(NA, NTYPE), hp%HtROW(NA, NTYPE), hp%N_SROW(NA, NTYPE), hp%A_SROW(NA, NTYPE), hp%DistribROW(NA, NTYPE), &
            stat = ierr)
        hp%fetchROW = 0.0; hp%HtROW = 0.0; hp%N_SROW = 0.0; hp%A_SROW = 0.0; hp%DistribROW = 0.0
    end if

    !> BASEFLOWFLAG 1 (Luo, 2012).
    if (lzsp%BASEFLOWFLAG == 1) then
        allocate(lzsp%dgwsh(NA, NTYPE), lzsp%agwsh(NA, NTYPE))
        lzsp%dgwsh = 0.0; lzsp%agwsh = 0.0
    end if

    !> BASEFLOWFLAG 2 (Watflood manual).
    if (lzsp%BASEFLOWFLAG == 2) then
        allocate(lzsp%WF_LZFA(NA, NTYPE), lzsp%WF_LZFPWR(NA, NTYPE))
        lzsp%WF_LZFA = 0.0; lzsp%WF_LZFPWR = 0.0
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

    !> Parse the INPUTPARAMSFORM to get INPUTPARAMSFORMFLAG.
    call parse(INPUTPARAMSFORM, delim, out_args, nargs)
    if (index(lowercase(INPUTPARAMSFORM), 'only') > 0) INPUTPARAMSFORMFLAG = 0
    if (index(lowercase(INPUTPARAMSFORM), 'ini') > 0) INPUTPARAMSFORMFLAG = INPUTPARAMSFORMFLAG + radix(2)**0
    if (index(lowercase(INPUTPARAMSFORM), 'r2c') > 0) INPUTPARAMSFORMFLAG = INPUTPARAMSFORMFLAG + radix(2)**1
    if (index(lowercase(INPUTPARAMSFORM), 'csv') > 0) INPUTPARAMSFORMFLAG = INPUTPARAMSFORMFLAG + radix(2)**2

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
        call read_parameters_r2c(shd, 100, 'MESH_input_parameters.r2c')
    end if

    !>
    !> DISTRIBUTE.
    !>

    !> Constants.
    pm%sfp%zrfm(il1:il2) = pm_gru%sfp%zrfm(1)
    pm%sfp%zrfh(il1:il2) = pm_gru%sfp%zrfh(1)
    pm%sfp%zbld(il1:il2) = pm_gru%sfp%zbld(1)
    pm%tp%gc(il1:il2) = pm_gru%tp%gc(1)

    !> From GRU.
    if (btest(INPUTPARAMSFORMFLAG, 0) .or. btest(INPUTPARAMSFORMFLAG, 2)) then
        do k = il1, il2

            !> GRU index.
            i = shd%lc%JLMOS(k)

            !> SA_MESH.
            pm%tp%fare(k) = pm_gru%tp%fare(i)
            pm%tp%mid(k) = max(1, pm_gru%tp%mid(i))
            pm%cp%fcan(k, :) = pm_gru%cp%fcan(i, :)
            pm%cp%lnz0(k, :) = pm_gru%cp%lnz0(i, :)
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
            pm%slp%sdep(k) = pm_gru%slp%sdep(i)
            pm%hp%drn(k) = pm_gru%hp%drn(i)
            pm%tp%xslp(k) = pm_gru%tp%xslp(i)
            pm%hp%dd(k) = pm_gru%hp%dd(i)/1000.0
            pm%hp%mann(k) = pm_gru%hp%mann(i)
            pm%hp%grkf(k) = pm_gru%hp%grkf(i)
            pm%hp%ks(k) = pm_gru%hp%ks(i)
            pm%slp%sand(k, :) = pm_gru%slp%sand(i, :)
            pm%slp%clay(k, :) = pm_gru%slp%clay(i, :)
            pm%slp%orgm(k, :) = pm_gru%slp%orgm(i, :)
            pm%snp%zsnl(k) = pm_gru%snp%zsnl(i)
            pm%sfp%zplg(k) = pm_gru%sfp%zplg(i)
            pm%snp%zpls(k) = pm_gru%snp%zpls(i)

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

        end do !k = il1, il2
    end if

    !> From river class (IAK) if not read by grid.
    if (NRVR > 0 .and. .not. btest(INPUTPARAMSFORMFLAG, 1)) then
        do k = 1, NAA

            !> River class index (IAK).
            i = shd%IAK(k)

            !> RPN RTE (Watflood, 2007).
            if (rteflg%PROCESS_ACTIVE) then
                rtepm%r1n(k) = rtepm_iak%r1n(i)
                rtepm%r2n(k) = rtepm_iak%r2n(i)
                rtepm%mndr(k) = rtepm_iak%mndr(i)
                rtepm%widep(k) = rtepm_iak%widep(i)
                rtepm%flz(k) = rtepm_iak%flz(i)
                rtepm%pwr(k) = rtepm_iak%pwr(i)
                rtepm%aa2(k) = rtepm_iak%aa2(i)
                rtepm%aa3(k) = rtepm_iak%aa3(i)
                rtepm%aa4(k) = rtepm_iak%aa4(i)
            end if

        end do !k = il1, il2
    end if

    !> From grid.
    if (btest(INPUTPARAMSFORMFLAG, 1)) then
        do k = il1, il2

            !> Grid index.
            i = shd%lc%ILMOS(k)

            !> SA_MESH.
            if (allocated(shd%SLOPE_INT)) pm%tp%xslp(k) = shd%SLOPE_INT(i)
            if (allocated(shd%DRDN)) pm%hp%dd(k) = shd%DRDN(i)

        end do !k = il1, il2
    end if

end subroutine
