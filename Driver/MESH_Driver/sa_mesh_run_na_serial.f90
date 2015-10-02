module sa_mesh_run_na_serial

    contains

    subroutine run_na_serial
        use module_routing
        call run_routing()
    end subroutine

end module
