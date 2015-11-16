module process_WF_ROUTE

    use process_WF_ROUTE_config

    implicit none

    contains

    subroutine run_WF_ROUTE(shd, ic, wb)

        use sa_mesh_shared_variabletypes
        use model_dates
        use MODEL_OUTPUT

        type(ShedGridParams), intent(in) :: shd
        type(iter_counter), intent(in) :: ic
        type(water_balance), intent(in) :: wb

        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

    end subroutine

end module
