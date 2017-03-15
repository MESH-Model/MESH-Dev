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
    integer NA, NTYPE, NRVR, NML, NSL, k, i, m

    !> Assign commonly used indices to local variables.
    NA = shd%NA
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

    !> Parse the INPUTPARAMSFORM.
    call parse(INPUTPARAMSFORM, delim, out_args, nargs)

    !> Read the parameter values.
    select case (lowercase(out_args(1)))

        !> r2c: From r2c by grid.

        !> csv: From CSV by GRU.

        !> ini: From CLASS.ini and Hydrology.ini (default).
        case default
            call READ_PARAMETERS_CLASS(shd, fls, cm)
            call READ_PARAMETERS_HYDROLOGY(shd, fls)
            call READ_SOIL_INI(shd, fls)

    end select

    !>
    !> DISTRIBUTE.
    !>

    !> Distribute the values.
    select case (lowercase(out_args(1)))

        !> From grid.
        !>  r2c: From r2c by grid.

        !> From GRU.
        !>  csv: From CSV by GRU.
        !>  ini: From CLASS.ini and Hydrology.ini (default).
        case default

            !> Distribute the parameter values to the tile (GAT) level.
            do k = il1, il2

                !> Grab the indices of the grid and GRU.
                i = shd%lc%ILMOS(k)
                m = shd%lc%JLMOS(k)

                !> SA_MESH.
                pm%sfp%zrfm(k) = pm_gru%sfp%zrfm(1)
                pm%sfp%zrfh(k) = pm_gru%sfp%zrfh(1)
                pm%sfp%zbld(k) = pm_gru%sfp%zbld(1)
                pm%tp%gc(k) = pm_gru%tp%gc(1)
                pm%tp%fare(k) = pm_gru%tp%fare(m)
                pm%tp%mid(k) = max(1, pm_gru%tp%mid(m))
                pm%cp%fcan(k, :) = pm_gru%cp%fcan(m, :)
                pm%cp%lnz0(k, :) = pm_gru%cp%lnz0(m, :)
                pm%cp%alvc(k, :) = pm_gru%cp%alvc(m, :)
                pm%cp%alic(k, :) = pm_gru%cp%alic(m, :)
                pm%cp%lamx(k, :) = pm_gru%cp%lamx(m, :)
                pm%cp%lamn(k, :) = pm_gru%cp%lamn(m, :)
                pm%cp%cmas(k, :) = pm_gru%cp%cmas(m, :)
                pm%cp%root(k, :) = pm_gru%cp%root(m, :)
                pm%cp%rsmn(k, :) = pm_gru%cp%rsmn(m, :)
                pm%cp%qa50(k, :) = pm_gru%cp%qa50(m, :)
                pm%cp%vpda(k, :) = pm_gru%cp%vpda(m, :)
                pm%cp%vpdb(k, :) = pm_gru%cp%vpdb(m, :)
                pm%cp%psga(k, :) = pm_gru%cp%psga(m, :)
                pm%cp%psgb(k, :) = pm_gru%cp%psgb(m, :)
                pm%slp%sdep(k) = pm_gru%slp%sdep(m)
                pm%hp%drn(k) = pm_gru%hp%drn(m)
                if (allocated(shd%SLOPE_INT)) then
                    pm%tp%xslp(k) = shd%SLOPE_INT(i) !taken from the drainage database.
                else
                    pm%tp%xslp(k) = pm_gru%tp%xslp(m) !taken by GRU from CLASS.ini
                end if
                if (allocated(shd%DRDN)) then
                    pm%hp%dd(k) = shd%DRDN(i) !taken from the drainage database.
                else
                    pm%hp%dd(k) = pm_gru%hp%dd(m)/1000.0 !taken from CLASS.ini and from km/km^2 to m/m^2 for WATROF.
                end if
                pm%hp%mann(k) = pm_gru%hp%mann(m)
                pm%hp%grkf(k) = pm_gru%hp%grkf(m)
                pm%hp%ks(k) = pm_gru%hp%ks(m)
                pm%slp%sand(k, :) = pm_gru%slp%sand(m, :)
                pm%slp%clay(k, :) = pm_gru%slp%clay(m, :)
                pm%slp%orgm(k, :) = pm_gru%slp%orgm(m, :)
                pm%snp%zsnl(k) = pm_gru%snp%zsnl(m)
                pm%sfp%zplg(k) = pm_gru%sfp%zplg(m)
                pm%snp%zpls(k) = pm_gru%snp%zpls(m)

                !> Cropland irrigation module.
                if (cifg%PROCESS_ACTIVE) then
                    cip%jdsow(k) = ciprot%jdsow(m)
                    cip%ldini(k) = ciprot%ldini(m)
                    cip%lddev(k) = ciprot%lddev(m)
                    cip%ldmid(k) = ciprot%ldmid(m)
                    cip%ldlate(k) = ciprot%ldlate(m)
                    cip%Kcini(k) = ciprot%Kcini(m)
                    cip%Kcdev(k) = ciprot%Kcdev(m)
                    cip%Kcmid(k) = ciprot%Kcmid(m)
                    cip%Kclate(k) = ciprot%Kclate(m)
                end if

            end do !k = il1, il2

    end select

end subroutine
