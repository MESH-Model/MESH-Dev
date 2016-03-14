module sa_mesh_run_within_tile

    implicit none

    contains

    subroutine run_within_tile_ini(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

        use module_mpi_flags
        use module_mpi_shared_variables
        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use process_CLASS_config, only: RUNCLASS_ini

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

        call RUNCLASS_ini(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

!>
!>***********************************************************************
!> MAM - Check for parameter values - all parameters should lie within the
!> specified ranges in the "minmax_parameters.txt" file.
!>=======================================================================
!>
!    call check_parameters(WF_R2, M_C, NMTEST, &
!                           cp, &
!                           hp, soil_por_max, soil_depth, s0, t_ice_lens)

    end subroutine

    function run_within_tile(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

        use module_mpi_flags
        use module_mpi_shared_variables
        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use process_CLASS, only: RUNCLASS_within_tile
        use process_WF_ROUTE, only: run_WF_ROUTE_within_tile

        character(100) run_within_tile

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

        run_within_tile = ''

        call RUNCLASS_within_tile(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)
        run_within_tile = run_WF_ROUTE_within_tile(shd, ic, stfl, rrls)
        if (len_Trim(run_within_tile) > 0) return

        return

    end function

end module
