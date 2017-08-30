module sa_mesh_run_within_grid

    implicit none

    contains

    subroutine run_within_grid_init(shd, fls, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for 'il1:il2' and 'i1:i2' indexing.
        use mpi_module

!+todo: There's a dependency on CLASSBD.f.
        use RUNCLASS36_constants, only: RHOW, RHOICE

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        integer k, ki, kj
        real frac

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Initialize grid-based accumulators.
        stas_grid%sl%tbar(i1:i2, :) = 0.0
        stas_grid%sl%thic(i1:i2, :) = 0.0
        stas_grid%sl%fzws(i1:i2, :) = 0.0
        stas_grid%sl%thlq(i1:i2, :) = 0.0
        stas_grid%sl%lqws(i1:i2, :) = 0.0
        stas_grid%cnpy%rcan(i1:i2) = 0.0
        stas_grid%cnpy%sncan(i1:i2) = 0.0
        stas_grid%sno%sno(i1:i2) = 0.0
        stas_grid%sno%wsno(i1:i2) = 0.0
        stas_grid%sfc%zpnd(i1:i2) = 0.0
        stas_grid%sfc%pndw(i1:i2) = 0.0
        stas_grid%lzs%lqws(i1:i2) = 0.0
        stas_grid%dzs%lqws(i1:i2) = 0.0

        !> Aggregate grid-based accumulators.
        do k = il1, il2
            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)
            frac = shd%lc%ACLASS(ki, kj)
            stas_grid%sl%tbar(ki, :) = stas_grid%sl%tbar(ki, :) + stas%sl%tbar(k, :)*frac
            stas_grid%sl%thic(ki, :) = stas_grid%sl%thic(ki, :) + stas%sl%thic(k, :)*frac
            stas_grid%sl%fzws(ki, :) = stas_grid%sl%fzws(ki, :) + stas%sl%thic(k, :)*frac*stas%sl%delzw(k, :)*RHOICE
            stas_grid%sl%thlq(ki, :) = stas_grid%sl%thlq(ki, :) + stas%sl%thlq(k, :)*frac
            stas_grid%sl%lqws(ki, :) = stas_grid%sl%lqws(ki, :) + stas%sl%thlq(k, :)*frac*stas%sl%delzw(k, :)*RHOW
            stas_grid%cnpy%rcan(ki) = stas_grid%cnpy%rcan(ki) + stas%cnpy%rcan(k)*frac
            stas_grid%cnpy%sncan(ki) = stas_grid%cnpy%sncan(ki) + stas%cnpy%sncan(k)*frac
            stas_grid%sno%sno(ki) = stas_grid%sno%sno(ki) + stas%sno%sno(k)*frac
            if (stas%sno%sno(k) > 0.0) then
                stas_grid%sno%wsno(ki) = stas_grid%sno%wsno(ki) + stas%sno%wsno(k)*frac
            end if
            stas_grid%sfc%zpnd(ki) = stas_grid%sfc%zpnd(ki) + stas%sfc%zpnd(k)*frac
            stas_grid%lzs%lqws(ki) = stas_grid%lzs%lqws(ki) + stas%lzs%lqws(k)*frac
            stas_grid%dzs%lqws(ki) = stas_grid%dzs%lqws(ki) + stas%dzs%lqws(k)*frac
        end do
        stas_grid%sfc%pndw(i1:i2) = stas_grid%sfc%zpnd(i1:i2)*RHOW

    end subroutine

    subroutine run_within_grid(shd, fls, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for 'il1:il2' and 'i1:i2' indexing.
        use mpi_module

!+todo: There's a dependency on CLASSBD.f.
        use RUNCLASS36_constants, only: RHOW, RHOICE

        !> Required for calls to processes.
        use baseflow_module
        use RUNLAKE_module

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        integer k, ki, kj
        real frac

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Initialize grid-based accumulators.
        stas_grid%sfc%evap(i1:i2) = 0.0
        stas_grid%sfc%rofo(i1:i2) = 0.0
        stas_grid%sl%rofs(i1:i2) = 0.0
        stas_grid%lzs%rofb(i1:i2) = 0.0
        stas_grid%lzs%lqws(i1:i2) = 0.0
        stas_grid%dzs%rofb(i1:i2) = 0.0
        stas_grid%dzs%lqws(i1:i2) = 0.0
        stas_grid%cnpy%pevp(i1:i2) = 0.0
        stas_grid%cnpy%evpb(i1:i2) = 0.0
        stas_grid%cnpy%arrd(i1:i2) = 0.0
        stas_grid%sfc%qevp(i1:i2) = 0.0
        stas_grid%sfc%hfs(i1:i2)  = 0.0
        stas_grid%sl%tbar(i1:i2, :) = 0.0
        stas_grid%sl%thic(i1:i2, :) = 0.0
        stas_grid%sl%fzws(i1:i2, :) = 0.0
        stas_grid%sl%thlq(i1:i2, :) = 0.0
        stas_grid%sl%lqws(i1:i2, :) = 0.0
        stas_grid%sl%gflx(i1:i2, :) = 0.0
        stas_grid%cnpy%rcan(i1:i2) = 0.0
        stas_grid%cnpy%sncan(i1:i2) = 0.0
        stas_grid%sno%sno(i1:i2) = 0.0
        stas_grid%sno%wsno(i1:i2) = 0.0
        stas_grid%sfc%zpnd(i1:i2) = 0.0
        stas_grid%sfc%pndw(i1:i2) = 0.0

        !> Aggregate grid-based accumulators.
        do k = il1, il2
            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)
            frac = shd%lc%ACLASS(ki, kj)
            stas_grid%sfc%evap(ki) = stas_grid%sfc%evap(ki) + stas%sfc%evap(k)*frac
            stas_grid%sfc%rofo(ki) = stas_grid%sfc%rofo(ki) + stas%sfc%rofo(k)*frac
            stas_grid%sl%rofs(ki) = stas_grid%sl%rofs(ki) + stas%sl%rofs(k)*frac
            stas_grid%lzs%rofb(ki) = stas_grid%lzs%rofb(ki) + stas%lzs%rofb(k)*frac
            stas_grid%lzs%lqws(ki) = stas_grid%lzs%lqws(ki) + stas%lzs%lqws(k)*frac
            stas_grid%dzs%rofb(ki) = stas_grid%dzs%rofb(ki) + stas%dzs%rofb(k)*frac
            stas_grid%dzs%lqws(ki) = stas_grid%dzs%lqws(ki) + stas%dzs%lqws(k)*frac
            stas_grid%cnpy%pevp(ki) = stas_grid%cnpy%pevp(ki) + stas%cnpy%pevp(k)*frac
            stas_grid%cnpy%evpb(ki) = stas_grid%cnpy%evpb(ki) + stas%cnpy%evpb(k)*frac
            stas_grid%cnpy%arrd(ki) = stas_grid%cnpy%arrd(ki) + stas%cnpy%arrd(k)*frac
            stas_grid%sfc%qevp(ki) = stas_grid%sfc%qevp(ki) + stas%sfc%qevp(k)*frac
            stas_grid%sfc%hfs(ki)  = stas_grid%sfc%hfs(ki) + stas%sfc%hfs(k)*frac
            stas_grid%sl%tbar(ki, :) = stas_grid%sl%tbar(ki, :) + stas%sl%tbar(k, :)*frac
            stas_grid%sl%thic(ki, :) = stas_grid%sl%thic(ki, :) + stas%sl%thic(k, :)*frac
            stas_grid%sl%fzws(ki, :) = stas_grid%sl%fzws(ki, :) + stas%sl%thic(k, :)*stas%sl%delzw(k, :)*frac*RHOICE
            stas_grid%sl%thlq(ki, :) = stas_grid%sl%thlq(ki, :) + stas%sl%thlq(k, :)*frac
            stas_grid%sl%lqws(ki, :) = stas_grid%sl%lqws(ki, :) + stas%sl%thlq(k, :)*stas%sl%delzw(k, :)*frac*RHOW
            stas_grid%sl%gflx(ki, :) = stas_grid%sl%gflx(ki, :) + stas%sl%gflx(k, :)*frac
            stas_grid%cnpy%rcan(ki) = stas_grid%cnpy%rcan(ki) + stas%cnpy%rcan(k)*frac
            stas_grid%cnpy%sncan(ki) = stas_grid%cnpy%sncan(ki) + stas%cnpy%sncan(k)*frac
            stas_grid%sno%sno(ki) = stas_grid%sno%sno(ki) + stas%sno%sno(k)*frac
            if (stas%sno%sno(k) > 0.0) then
                stas_grid%sno%wsno(ki) = stas_grid%sno%wsno(ki) + stas%sno%wsno(k)*frac
            end if
            stas_grid%sfc%zpnd(ki) = stas_grid%sfc%zpnd(ki) + stas%sfc%zpnd(k)*frac
        end do
        stas_grid%sfc%pndw(i1:i2) = stas_grid%sfc%zpnd(i1:i2)*RHOW

        !> Update grid based states.
        do k = 1, NMW

            ki = shd%wc%ILMOS(k)
            kj = shd%wc%JLMOS(k)

            FRAC = lm%pm_nlak%FRAC(kj, ki)               !new code (1105)

            if (FRAC > 0.0) then
                stas_grid%sfc%evap(ki) = stas_grid%sfc%evap(ki) + ldvi%QFSL(k)*FRAC
                if (lfv%PCPR(k)*FRAC > ldvi%QFSL(k)*FRAC) then
                    stas_grid%sfc%rofo(ki) = stas_grid%sfc%rofo(ki) + lfv%PCPR(k)*FRAC-ldvi%QFSL(k)*FRAC
                end if
                stas_grid%sfc%qevp(ki) = stas_grid%sfc%qevp(ki) + ldvi%QEVPL(k)*FRAC
                stas_grid%sfc%hfs(ki)  = stas_grid%sfc%hfs(ki) + ldvi%HFSL(k)*FRAC

                stas_grid%sno%sno(ki) = stas_grid%sno%sno(ki) + ldvi%SNOL(k)*FRAC
                if (ldvi%SNOL(k)*FRAC > 0.0) then
                    stas_grid%sno%wsno(ki) = stas_grid%sno%wsno(ki) + ldvi%WSNOL(k)*FRAC
                end if
            end if

        end do

        !> Call processes.
        call bflm_within_grid(fls, shd, cm)

    end subroutine

    subroutine run_within_grid_finalize(fls, shd, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE .and. .not. ro%RUNGRID) return

    end subroutine

end module
