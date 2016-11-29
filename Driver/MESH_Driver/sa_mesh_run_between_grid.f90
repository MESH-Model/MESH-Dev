module sa_mesh_run_between_grid

    implicit none

    contains

    subroutine run_between_grid_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use SA_RTE_module, only: SA_RTE_init
        use WF_ROUTE_config, only: WF_ROUTE_init
        use save_basin_output, only: run_save_basin_output_init

        !> Cropland irrigation module.
        use cropland_irrigation_between_grid, only: runci_between_grid_init

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
        !* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
        !* WF_START_DAY OBSERVED STREAMFLOW START DAY
        !* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
!-        integer WF_START_YEAR, WF_START_DAY, WF_START_HOUR
!-        integer JDAY_IND_STRM, JDAY_IND1, JDAY_IND2
!-        real I_G, J_G
        integer NA

        !> Initialiation of states.
        NA = shd%NA

        !> Stream channel.
        stas%chnl%n = NA
        allocate(stas%chnl%qi(1:NA), stas%chnl%qo(1:NA), stas%chnl%s(1:NA))
        stas%chnl%qi(1:NA) = 0.0
        stas%chnl%qo(1:NA) = 0.0
        stas%chnl%s(1:NA) = 0.0

        !> Lake.
!+        stas%lk%n = NLK
!+        allocate(stas%lk%ab(1:NLK), stas%lk%qi(1:NLK), stas%lk%qo(1:NLK), stas%lk%s(1:NLK))
!+        stas%lk%ab(1:NLK) = 0.0
!+        stas%lk%qi(1:NLK) = 0.0
!+        stas%lk%qo(1:NLK) = 0.0
!+        stas%lk%s(1:NLK) = 0.0

        !> Reservoir.
!+        stas%rsvr%n = NRSVR
!+        allocate(stas%rsvr%ab(1:NRSVR), stas%rsvr%qi(1:NRSVR), stas%rsvr%qo(1:NRSVR), stas%rsvr%s(1:NRSVR))
!+        stas%rsvr%ab(1:NRSVR) = 0.0
!+        stas%rsvr%qi(1:NRSVR) = 0.0
!+        stas%rsvr%qo(1:NRSVR) = 0.0
!+        stas%rsvr%s(1:NRSVR) = 0.0

!todo: switch
        call SA_RTE_init(shd)
        call WF_ROUTE_init(shd, fls, stfl, rrls)
        call run_save_basin_output_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        !> Cropland irrigation module (ICU).
        call runci_between_grid_init(shd, fls)

    end subroutine

    subroutine run_between_grid(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use SA_RTE_module, only: SA_RTE
        use WF_ROUTE_module, only: WF_ROUTE_between_grid
        use save_basin_output, only: run_save_basin_output

        !> Cropland irrigation module.
        use cropland_irrigation_between_grid, only: runci_between_grid

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

!todo: Switch
        call SA_RTE(shd, wb)
        call WF_ROUTE_between_grid(shd, wb, stfl, rrls)

        !> Cropland irrigation module (ICU).
        call runci_between_grid(shd, fls, cm)

        call run_save_basin_output(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

    end subroutine

    subroutine run_between_grid_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        use model_files_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use WF_ROUTE_config, only: WF_ROUTE_finalize
        use save_basin_output, only: run_save_basin_output_finalize

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        call WF_ROUTE_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)
        call run_save_basin_output_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

    end subroutine

end module
