module sa_mesh_run_within_tile

    implicit none

    contains

    subroutine run_within_tile_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_parameters
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use RUNCLASS36_config
        use RUNSVS113_config
        use baseflow_module

        !> Cropland irrigation module.
        use cropland_irrigation_init, only: runci_init

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

        !> Cropland irrigation module.
        call runci_init(shd, fls)

    end subroutine

    function run_within_tile(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use cropland_irrigation_within_tile, only: runci_within_tile
        use RUNCLASS36_module, only: RUNCLASS36_within_tile
        use RUNSVS113_module, only: RUNSVS113
        use WF_ROUTE_module, only: WF_ROUTE_within_tile
        use baseflow_module

        character(100) run_within_tile

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        stas%cnpy%pevp(il1:il2) = 0.0
        stas%sfc%evap(il1:il2) = 0.0
        stas%cnpy%evpb(il1:il2) = 0.0
        stas%sfc%qevp(il1:il2) = 0.0
        stas%sfc%hfs(il1:il2) = 0.0
        stas%sfc%rofo(il1:il2) = 0.0
        stas%sl%rofs(il1:il2) = 0.0
        stas%lzs%rofb(il1:il2) = 0.0

        run_within_tile = ''

        call RUNCLASS36_within_tile(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        call RUNSVS113(shd, fls, ts, cm, wb, eb, sp)

!+        call LZS_within_tile(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        run_within_tile = WF_ROUTE_within_tile(shd, stfl, rrls)
        if (len_Trim(run_within_tile) > 0) return

        !> Cropland irrigation module (PEVP).
        call runci_within_tile(shd, fls, cm)

        where (stas%cnpy%pevp(il1:il2) /= 0.0)
            stas%cnpy%evpb(il1:il2) = stas%sfc%evap(il1:il2)/stas%cnpy%pevp(il1:il2)
            stas%cnpy%arrd(il1:il2) = cm%dat(ck%RT)%GAT(il1:il2)/stas%cnpy%pevp(il1:il2)
        end where

        return

    end function

    subroutine run_within_tile_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        use model_files_variabletypes
        use sa_mesh_shared_variables
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
