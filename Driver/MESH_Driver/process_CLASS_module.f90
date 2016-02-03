module process_CLASS

    use process_CLASS_constants
    use process_CLASS_variables

    implicit none

    contains

    subroutine RUNCLASS_within_tile(shd, ic)

        use sa_mesh_shared_variabletypes
        use model_dates

        !> For CLASS output.
        use process_CLASS_save_output

        type(ShedGridParams), intent(in) :: shd
        type(iter_counter), intent(in) :: ic

        !> WRITE FIELDS FROM CURRENT TIME STEP TO OUTPUT FILES.
        if (WF_NUM_POINTS > 0) then
            call CLASSOUT_update_files(shd, ic)
        end if

    end subroutine

end module
