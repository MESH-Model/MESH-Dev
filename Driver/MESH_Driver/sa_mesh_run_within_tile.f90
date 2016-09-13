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

        !> Local variables.
        integer NA, NTYPE, NML, NSL

        NA = shd%NA
        NTYPE = shd%lc%NTYPE
        NSL = shd%lc%IGND
        NML = shd%lc%NML

        call RUNCLASS36_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

!>
!>***********************************************************************
!> MAM - Check for parameter values - all parameters should lie within the
!> specified ranges in the "minmax_parameters.txt" file.
!>=======================================================================
!>
!    call check_parameters(WF_R2, M_C, NMTEST, &
!                           cp, &
!                           hp, soil_por_max, soil_depth, s0, t_ice_lens)

        call RUNSVS113_init(shd, fls, ts, cm, wb, eb, sp)

        call LZS_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

    end subroutine

    function run_within_tile(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

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

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        call RUNCLASS36_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

    end subroutine

end module
