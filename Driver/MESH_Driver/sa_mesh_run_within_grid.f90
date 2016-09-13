module sa_mesh_run_within_grid

    implicit none

    contains

    subroutine run_within_grid_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer NA

        !> Initialiation of states.
        NA = shd%NA

        !> Water balance.
        stas%wb%n = NA
        allocate(stas%wb%cnpy(1:NA), stas%wb%sfc(1:NA), stas%wb%sl(1:NA), stas%wb%lz(1:NA), stas%wb%dz(1:NA), stas%wb%lost(1:NA), &
                 stas%wb%s(1:NA), stas%wb%ds(1:NA))
        stas%wb%cnpy(1:NA) = 0.0
        stas%wb%sfc(1:NA) = 0.0
        stas%wb%sl(1:NA) = 0.0
        stas%wb%lz(1:NA) = 0.0
        stas%wb%dz(1:NA) = 0.0
        stas%wb%lost(1:NA) = 0.0
        stas%wb%s(1:NA) = 0.0
        stas%wb%ds(1:NA) = 0.0

    end subroutine

    subroutine run_within_grid(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

    end subroutine

    subroutine run_within_grid_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        use model_files_variabletypes
        use sa_mesh_shared_variabletypes
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
