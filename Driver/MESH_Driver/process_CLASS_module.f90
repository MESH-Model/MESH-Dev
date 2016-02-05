module process_CLASS

    use process_CLASS_constants
    use process_CLASS_variables

    implicit none

    contains

    subroutine RUNCLASS_within_tile(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

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

        !> For CLASS output.
        use process_CLASS_save_output

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

        !> WRITE FIELDS FROM CURRENT TIME STEP TO OUTPUT FILES.
        if (WF_NUM_POINTS > 0) then
            call CLASSOUT_update_files(shd, ic)
        end if

    end subroutine

end module
