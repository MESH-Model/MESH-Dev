module sa_mesh_run_within_grid

    implicit none

    contains

    subroutine run_within_grid_init(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT
        use SVS_module_config, only: RUNSVS_config

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

        call RUNSVS_config(shd, fls, ts, ic, cm, wb, eb, sp)

    end subroutine

    subroutine run_within_grid(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT
        use SVS_module, only: RUNSVS

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

        call RUNSVS(shd, fls, ts, ic, cm, wb, eb, sp)

    end subroutine

    subroutine run_within_grid_finalize(fls, shd, ic, cm, wb, eb, sv, stfl, rrls)

        use model_files_variabletypes
        use sa_mesh_shared_variabletypes
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(iter_counter) :: ic
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

    end subroutine

end module
