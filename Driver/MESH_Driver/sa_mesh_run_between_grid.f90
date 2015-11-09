module sa_mesh_run_between_grid

    implicit none

    contains

    subroutine run_between_grid_config(shd, ts, ic)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates

        use process_SA_RTE, only: configure_SA_RTE
        use process_WF_ROUTE_config, only: config_WF_ROUTE

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic

!todo: switch
        call configure_SA_RTE(shd, ic)
        call config_WF_ROUTE(shd)

    end subroutine

    subroutine run_between_grid(shd, ts, ic, cm, wb, eb, sov)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use MODEL_OUTPUT

        use process_SA_RTE, only: run_SA_RTE
        use process_WF_ROUTE, only: run_WF_ROUTE

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic
        type(clim_info), intent(in) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sov

!todo: Switch
        call run_SA_RTE(shd, ic, wb)
        call run_WF_ROUTE(shd, wb)

    end subroutine

end module
