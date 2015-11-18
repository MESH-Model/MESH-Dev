module sa_mesh_run_within_tile

    implicit none

    contains

    subroutine run_within_tile_ini(shd, ts, ic, wb, eg, sp, stfl, rrls)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use MODEL_OUTPUT
        use model_output_variabletypes

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic
        type(water_balance) :: wb
        type(energy_balance) :: eg
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

    end subroutine

    subroutine run_within_tile(shd, ts, ic, cm, wb, eg, sp, stfl, rrls, &
                               WF_NORESV_CTRL, &
                               WF_KTR, &
                               WF_QREL, &
                               JAN, &
                               WF_NORESV, &
                               WF_QHYD, WF_KT, WF_NO)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use MODEL_OUTPUT
        use model_output_variabletypes

        use process_WF_ROUTE, only: run_WF_ROUTE_within_tile

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic
        type(clim_info), intent(in) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eg
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        integer WF_NO, WF_KT
        real, dimension(:), allocatable :: WF_QHYD
        real, dimension(:), allocatable :: WF_QREL
        integer JAN
        integer WF_NORESV, WF_KTR, WF_NORESV_CTRL

        call run_WF_ROUTE_within_tile(shd, ic, stfl, rrls, &
                                      WF_NORESV_CTRL, &
                                      WF_KTR, &
                                      WF_QREL, &
                                      JAN, &
                                      WF_NORESV, &
                                      WF_QHYD, WF_KT, WF_NO)

    end subroutine

end module
