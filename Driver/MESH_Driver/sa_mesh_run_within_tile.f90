module sa_mesh_run_within_tile

    implicit none

    contains

    subroutine run_within_tile_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_parameters
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use RUNCLASS36_config
        use RUNSVS113_config
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

        call RUNCLASS36_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        call RUNSVS113_init(shd, fls, ts, cm, wb, eb, sp)

        call LZS_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

    end subroutine

    function run_within_tile(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use RUNCLASS36_module, only: RUNCLASS36_within_tile
        use RUNSVS113_module, only: RUNSVS113
        use WF_ROUTE_module, only: WF_ROUTE_within_tile
        use baseflow_module

        character(100) run_within_tile

        !> Internal variables for accumulation.
        integer k, ik
        real FRAC

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        run_within_tile = ''

        call RUNCLASS36_within_tile(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        call RUNSVS113(shd, fls, ts, cm, wb, eb, sp)

!+        call LZS_within_tile(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        run_within_tile = WF_ROUTE_within_tile(shd, stfl, rrls)
        if (len_Trim(run_within_tile) > 0) return

        stas%cnpy%evpb(il1:il2) = 0.0
        where (stas%cnpy%pevp(il1:il2) /= 0.0)
            stas%cnpy%evpb(il1:il2) = stas%cnpy%evp(il1:il2)/stas%cnpy%pevp(il1:il2)
            stas%cnpy%arrd(il1:il2) = cm%dat(ck%RT)%GAT(il1:il2)/stas%cnpy%pevp(il1:il2)
        end where

        if (ipid == 0) then
            do k = il1, il2
                ik = shd%lc%ILMOS(k)
                FRAC = shd%lc%ACLASS(ik, shd%lc%JLMOS(k))*shd%FRAC(ik)
                if (FRAC > 0.0) then
                    wb%pevp(ik) = wb%pevp(ik) + stas%cnpy%pevp(k)*FRAC*ic%dts
                    wb%evpb(ik) = wb%evpb(ik) + stas%cnpy%evpb(k)*FRAC
                    wb%arrd(ik) = wb%arrd(ik) + stas%cnpy%arrd(k)*FRAC
                end if
            end do
        end if

        return

    end function

    subroutine run_within_tile_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        use model_files_variabletypes
        use sa_mesh_shared_variabletypes
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use RUNCLASS36_config, only: RUNCLASS36_finalize
        use baseflow_module

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        call RUNCLASS36_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        call LZS_finalize(fls, shd)

    end subroutine

end module
