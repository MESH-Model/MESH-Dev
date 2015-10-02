module module_routing

    contains

    subroutine run_routing()
        use module_wf_route
        call run_wf_route()
    end subroutine

end module
