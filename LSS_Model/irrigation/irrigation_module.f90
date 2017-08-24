module irrigation_module

    implicit none

    !*  irflg: 1 for irrigated GRU; 0 otherwise (default: 0).
    !*  thlmin: Fraction of field capacity used to determine irrigation demand (default: 0.5). [--].
    !*  t1: Start hour in day for irrigation (default: 0 -- from beginning of day). [h].
    !*  t2: Stop hour in day for irrigation (default: 24 -- to end of day). [h].
    type irrigation_parameters
        integer, dimension(:), allocatable :: irflg, t1, t2
        real, dimension(:), allocatable :: thlmin
    end type

    !*  dmnd: Calculated irrigation demand. [kg m-2 s-1].
    type irrigation_variables
        real, dimension(:), allocatable :: dmnd
    end type

    type irrigation_container
        type(irrigation_parameters) pm, pm_gru, pm_grid
        type(irrigation_variables) va
        logical :: PROCESS_ACTIVE = .false.
    end type

    type(irrigation_container), save :: irrm

    contains

    subroutine irrigation_parameters_allocate(pm, n, ierr)

        type(irrigation_parameters) pm
        integer n, ierr

        allocate(pm%irflg(n), pm%t1(n), pm%t2(n), pm%thlmin(n), stat = ierr)
        pm%irflg = 0; pm%t1 = 0; pm%t2 = 0; pm%thlmin = 0.0

    end subroutine

    subroutine irrigation_parameters_deallocate(pm, ierr)

        type(irrigation_parameters) pm
        integer ierr

        deallocate(pm%irflg, pm%t1, pm%t2, pm%thlmin, stat = ierr)

    end subroutine

    subroutine irrigation_init(fls, shd, cm)

        use model_files_variabletypes
        use sa_mesh_shared_variables
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        integer ierr

        irrm%PROCESS_ACTIVE = any(irrm%pm%irflg == 1)

        !> Return if the irrigation module is not active.
        if (.not. irrm%PROCESS_ACTIVE) then

            !> Deallocate NNML-based parameters allocated in other parts of the code.
            call irrigation_parameters_deallocate(irrm%pm, ierr)
            return
        end if

        !> Allocate and initialize variables.
        allocate(irrm%va%dmnd(shd%lc%NML))
        irrm%va%dmnd = 0.0

        !> Deallocate non-NML based parameters allocated in other parts of the code.
        call irrigation_parameters_deallocate(irrm%pm_grid, ierr)
        call irrigation_parameters_deallocate(irrm%pm_gru, ierr)

        !> Assign default parameterization in the case of no parameterization.
        if (all(irrm%pm%thlmin == 0.0)) irrm%pm%thlmin = 0.5
        if (all(irrm%pm%t1 == 0)) irrm%pm%t1 = 0
        if (all(irrm%pm%t2 == 0)) irrm%pm%t2 = 24

    end subroutine

    subroutine irrigation_within_tile(fls, shd, cm)

        use model_files_variabletypes
        use sa_mesh_shared_variables
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Return if the irrigation module is not active.
        if (.not. irrm%PROCESS_ACTIVE) return

    end subroutine

end module
