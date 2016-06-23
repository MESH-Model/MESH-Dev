module sa_mesh_run_between_grid

    implicit none

    contains

    subroutine run_between_grid_init(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use SA_RTE_module, only: SA_RTE_init
        use WF_ROUTE_config, only: WF_ROUTE_init
        use save_basin_output, only: run_save_basin_output_init

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

        !> Local variables.
        !* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
        !* WF_START_DAY OBSERVED STREAMFLOW START DAY
        !* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
        integer WF_START_YEAR, WF_START_DAY, WF_START_HOUR
        integer JDAY_IND_STRM, JDAY_IND1, JDAY_IND2
        real I_G, J_G

!todo: switch
        call SA_RTE_init(shd, ic)
        call WF_ROUTE_init(shd, fls, ic, stfl, rrls)
        call run_save_basin_output_init(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

    end subroutine

    subroutine run_between_grid(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls, &
                                WF_R1, WF_R2, M_C)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use SA_RTE_module, only: SA_RTE
        use WF_ROUTE_module, only: WF_ROUTE_between_grid
        use save_basin_output, only: run_save_basin_output

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

        !> Temporary variables.
        integer M_C
        real WF_R1(M_C), WF_R2(M_C)

!todo: Switch
        call SA_RTE(shd, ic, wb)
        call WF_ROUTE_between_grid(shd, ic, wb, stfl, rrls, &
                                   WF_R1, WF_R2, M_C)
        call run_save_basin_output(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

    end subroutine

    subroutine run_between_grid_finalize(fls, shd, ic, cm, wb, eb, sv, stfl, rrls)

        use model_files_variabletypes
        use sa_mesh_shared_variabletypes
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use WF_ROUTE_config, only: WF_ROUTE_finalize
        use save_basin_output, only: run_save_basin_output_finalize

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(iter_counter) :: ic
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        call WF_ROUTE_finalize(fls, shd, ic, cm, wb, eb, sv, stfl, rrls)
        call run_save_basin_output_finalize(fls, shd, ic, cm, wb, eb, sv, stfl, rrls)

    end subroutine

end module
