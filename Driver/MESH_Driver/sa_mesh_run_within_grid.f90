module sa_mesh_run_within_grid

    implicit none

    contains

    subroutine run_within_grid_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

!+todo: There's a dependency on CLASSBD.f (block data, though described below as module)
        use RUNCLASS36_constants, only: RHOW, RHOICE

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        integer k, ki, kj, i1, i2
        real FRAC

        i1 = shd%lc%ILMOS(il1)
        i2 = shd%lc%ILMOS(il2)

        !> Initialize grid-based states.
        do k = il1, il2

            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)

            FRAC = shd%lc%ACLASS(ki, kj)*shd%FRAC(ki)

            if (FRAC > 0.0) then
                sp%TBAR(ki, :) = sp%TBAR(ki, :) + stas%sl%tbar(k, :)*shd%lc%ACLASS(ki, kj)
                sp%THIC(ki, :) = sp%THIC(ki, :) + stas%sl%thic(k, :)*FRAC
                wb%FRWS(ki, :) = wb%FRWS(ki, :) + stas%sl%thic(k, :)*stas%sl%delzw(k, :)*FRAC*RHOICE
                sp%THLQ(ki, :) = sp%THLQ(ki, :) + stas%sl%thlq(k, :)*FRAC
                wb%LQWS(ki, :) = wb%LQWS(ki, :) + stas%sl%thlq(k, :)*stas%sl%delzw(k, :)*FRAC*RHOW
                wb%RCAN(ki) = wb%RCAN(ki) + stas%cnpy%rcan(k)*FRAC
                wb%SNCAN(ki) = wb%SNCAN(ki) + stas%cnpy%sncan(k)*FRAC
                wb%SNO(ki) = wb%SNO(ki) + stas%sno%sno(k)*FRAC
                if (stas%sno%sno(k) > 0.0) then
                    wb%WSNO(ki) = wb%WSNO(ki) + stas%sno%wsno(k)*FRAC
                end if
                wb%PNDW(ki) = wb%PNDW(ki) + stas%sfc%zpnd(k)*FRAC*RHOW
            end if

        end do

        wb%STG(i1:i2) = &
            wb%RCAN(i1:i2) + wb%SNCAN(i1:i2) + wb%SNO(i1:i2) + wb%WSNO(i1:i2) + wb%PNDW(i1:i2) + &
            sum(wb%LQWS(i1:i2, :), 2) + sum(wb%FRWS(i1:i2, :), 2)

    end subroutine

    subroutine run_within_grid(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_parameters
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

!+todo: There's a dependency on CLASSBD.f (block data, though described below as module)
        use RUNCLASS36_constants, only: RHOW, RHOICE

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        integer k, ki, kj, i1, i2
        real FRAC

        i1 = shd%lc%ILMOS(il1)
        i2 = shd%lc%ILMOS(il2)

        !> Update grid based states.
        do k = il1, il2

            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)

            FRAC = shd%lc%ACLASS(ki, kj)*shd%FRAC(ki)

            if (FRAC > 0.0) then
                wb%PRE(ki) = wb%PRE(ki) + cm%dat(ck%RT)%GAT(k)*FRAC*ic%dts
                wb%EVAP(ki) = wb%EVAP(ki) + stas%sfc%evap(k)*FRAC*ic%dts
                wb%ROF(ki) = wb%ROF(ki) + (stas%sfc%rofo(k) + stas%sl%rofs(k) + stas%lzs%rofb(k) + stas%dzs%rofb(k))*FRAC*ic%dts
                wb%ROFO(ki) = wb%ROFO(ki) + stas%sfc%rofo(k)*FRAC*ic%dts
                wb%ROFS(ki) = wb%ROFS(ki) + stas%sl%rofs(k)*FRAC*ic%dts
                wb%ROFB(ki) = wb%ROFB(ki) + (stas%lzs%rofb(k) + stas%dzs%rofb(k))*FRAC*ic%dts
                wb%pevp(ki) = wb%pevp(ki) + stas%cnpy%pevp(k)*FRAC*ic%dts
                wb%evpb(ki) = wb%evpb(ki) + stas%cnpy%evpb(k)*FRAC
                wb%arrd(ki) = wb%arrd(ki) + stas%cnpy%arrd(k)*FRAC
                eb%QEVP(ki) = eb%QEVP(ki) + stas%sfc%qevp(k)*FRAC
                eb%HFS(ki)  = eb%HFS(ki) + stas%sfc%hfs(k)*FRAC
                sp%TBAR(ki, :) = sp%TBAR(ki, :) + stas%sl%tbar(k, :)*shd%lc%ACLASS(ki, kj)
                sp%THIC(ki, :) = sp%THIC(ki, :) + stas%sl%thic(k, :)*FRAC
                wb%FRWS(ki, :) = wb%FRWS(ki, :) + stas%sl%thic(k, :)*stas%sl%delzw(k, :)*FRAC*RHOICE
                sp%THLQ(ki, :) = sp%THLQ(ki, :) + stas%sl%thlq(k, :)*FRAC
                wb%LQWS(ki, :) = wb%LQWS(ki, :) + stas%sl%thlq(k, :)*stas%sl%delzw(k, :)*FRAC*RHOW
                eb%GFLX(ki, :) = eb%GFLX(ki, :) + stas%sl%gflx(k, :)*FRAC
                wb%RCAN(ki) = wb%RCAN(ki) + stas%cnpy%rcan(k)*FRAC
                wb%SNCAN(ki) = wb%SNCAN(ki) + stas%cnpy%sncan(k)*FRAC
                wb%SNO(ki) = wb%SNO(ki) + stas%sno%sno(k)*FRAC
                if (stas%sno%sno(k) > 0.0) then
                    wb%WSNO(ki) = wb%WSNO(ki) + stas%sno%wsno(k)*FRAC
                end if
                wb%PNDW(ki) = wb%PNDW(ki) + stas%sfc%zpnd(k)*FRAC*RHOW
            end if

        end do

        wb%DSTG(i1:i2) = &
            wb%RCAN(i1:i2) + wb%SNCAN(i1:i2) + wb%SNO(i1:i2) + wb%WSNO(i1:i2) + wb%PNDW(i1:i2) + &
            sum(wb%LQWS(i1:i2, :), 2) + sum(wb%FRWS(i1:i2, :), 2) - &
            wb%STG(i1:i2)
        wb%STG(i1:i2) = wb%DSTG(i1:i2) + wb%STG(i1:i2)

    end subroutine

    subroutine run_within_grid_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        use model_files_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

    end subroutine

end module
