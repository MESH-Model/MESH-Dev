module sa_mesh_run_nml_parallel

    contains

    subroutine run_nml_parallel
        use module_lss
        call run_lss()
    end subroutine

end module
