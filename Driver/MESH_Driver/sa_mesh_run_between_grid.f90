module sa_mesh_run_between_grid

    implicit none

    contains

    subroutine run_between_grid_ini(shd, ts, ic, stfl, rrls, &
!todo: remove these
                                LOCATIONFLAG, STREAMFLOWOUTFLAG, &
                                GENDIR_OUT)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use model_output_variabletypes

        use process_SA_RTE, only: configure_SA_RTE
        use process_WF_ROUTE_config, only: run_WF_ROUTE_ini

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Temporary variables.
!todo: remove these
        integer LOCATIONFLAG, STREAMFLOWOUTFLAG
        character(450) GENDIR_OUT

        !> Local variables.
        !* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
        !* WF_START_DAY OBSERVED STREAMFLOW START DAY
        !* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
        integer WF_START_YEAR, WF_START_DAY, WF_START_HOUR
        integer JDAY_IND_STRM, JDAY_IND1, JDAY_IND2
        real I_G, J_G

!todo: switch
        call configure_SA_RTE(shd, ic)
        call run_WF_ROUTE_ini(shd, ic, stfl, rrls, &
                              LOCATIONFLAG, STREAMFLOWOUTFLAG, &
                              GENDIR_OUT)

    end subroutine

    subroutine run_between_grid(shd, ts, ic, cm, wb, eg, sp, stfl, rrls, &
                                WF_R1, WF_R2, M_C)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use MODEL_OUTPUT
        use model_output_variabletypes

        use process_SA_RTE, only: run_SA_RTE
        use process_WF_ROUTE, only: run_WF_ROUTE_between_grid

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic
        type(clim_info), intent(in) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eg
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Temporary variables.
        integer M_C
        real WF_R1(M_C), WF_R2(M_C)

!todo: Switch
        call run_SA_RTE(shd, ic, wb)
        call run_WF_ROUTE_between_grid(shd, ic, wb, stfl, rrls, &
                                       WF_R1, WF_R2, M_C)

    end subroutine

end module
