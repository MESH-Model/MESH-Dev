module permafrost_outputs_module

    !> 'sa_mesh_common' required for common SA_MESH variables and routines.
    !> 'model_dates' required for 'ic' counter.
    !> 'model_files_variables' required for 'fls' object and file keys.
    !> 'mpi_module' required for MPI variables, tile/grid parsing utility, barrier flag.
    use sa_mesh_common
    use model_dates
    use model_files_variables
    use mpi_module

    implicit none

    !> Variable names.
    character(len = 10), parameter :: PMFRSTVN_ALD = 'ALD'
    character(len = 10), parameter :: PMFRSTVN_ALDDOY = 'ALD_DOY'
    character(len = 10), parameter :: PMFRSTVN_ALDENV = 'ALD_ENV'
    character(len = 10), parameter :: PMFRSTVN_TAVG = 'TAVG'
    character(len = 10), parameter :: PMFRSTVN_TMAX = 'TMAX'
    character(len = 10), parameter :: PMFRSTVN_TMIN = 'TMIN'
    character(len = 10), parameter :: PMFRSTVN_TRNG = 'TRNG'
    character(len = 10), parameter :: PMFRSTVN_ZOD = 'ZOD'

    !> Description:
    !>  Data type for parameters.
    !>
    !> Variables:
    !*  zod_ttol: Temperature threshold for zero oscillation depth. [K].
    type permafrost_outputs_parameters
        real, dimension(:), allocatable :: zod_ttol
    end type

    !> Description:
    !>  Data type for variables.
    !>
    !> Variables:
    !*  ald: Active layer depth calculated using daily average temperature (1: Tile index). [m].
    !*  alddoy: Day of year when ALD is observed (1: Tile index). [--].
    !*  aldenv: Active layer depth calculated using the annual temperature envelope (1: Tile index). [m].
    !*  tavg: Average daily soil temperature (1: Tile index; 2: Soil layer). [K].
    !*  tmax: Annual maximum of daily soil temperature (1: Tile index; 2: Soil layer). [K].
    !*  tmin: Annual minimum of daily soil temperature (1: Tile index; 2: Soil layer). [K].
    !*  trng: Range/envelope of the annual maximum and minimum soil temperatures (1: Tile index; 2: Soil layer). [K].
    !*  zod: Zero oscillation depth, where the range/envelope of the annual maximum and minimum soil temperatures in within the threshold (1: Tile index; 2: TTOL). [m].
    type permafrost_outputs_fields
        type(output_fields_surrogate) ald, alddoy, aldenv
        type(output_fields_surrogate), dimension(:), allocatable :: tavg, tmax, tmin, trng
        type(output_fields_surrogate), dimension(:), allocatable :: zod
    end type

    !> Description:
    !>  Container for flags, parameters, and variables.
    !>
    !> Variables:
    !*  pm: Parameter group.
    !*  y, m, d: Output interval of variables. [--].
    type permafrost_outputs_container
        logical :: PROCESS_ACTIVE = .false.
        type(permafrost_outputs_parameters) pm
        type(permafrost_outputs_fields) out
    end type

    type(permafrost_outputs_container), save :: prmfst

    contains

    subroutine permafrost_outputs_init(fls, shd, vname)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        character(len = *), intent(in) :: vname

        !> Local variables.
        integer na, nml, nsl, n, j, ierr

        !> Local variables.
        na = shd%NA; nml = shd%lc%NML; nsl = shd%lc%IGND

        !> TAVG and temperature statistics (for all outputs).
        if (.not. allocated(prmfst%out%tavg)) then
            allocate(prmfst%out%tavg(nsl), prmfst%out%tmax(nsl), prmfst%out%tmin(nsl), prmfst%out%trng(nsl))
            do j = 1, shd%lc%IGND
                allocate( &
                    prmfst%out%tavg(j)%y_tile(nml), prmfst%out%tavg(j)%d_tile(nml), &
                    prmfst%out%tavg(j)%y_grid(na), prmfst%out%tavg(j)%d_grid(na))
                prmfst%out%tavg(j)%y_tile = 0.0; prmfst%out%tavg(j)%d_tile = 0.0
                prmfst%out%tavg(j)%y_grid = 0.0; prmfst%out%tavg(j)%d_grid = 0.0

                !> TMAX.
                allocate(prmfst%out%tmax(j)%y_tile(nml), prmfst%out%tmax(j)%y_grid(na))
                prmfst%out%tmax(j)%y_tile = 100.0; prmfst%out%tmax(j)%y_grid = 100.0

                !> TMIN.
                allocate(prmfst%out%tmin(j)%y_tile(nml), prmfst%out%tmin(j)%y_grid(na))
                prmfst%out%tmin(j)%y_tile = 900.0; prmfst%out%tmin(j)%y_grid = 900.0

                !> TRNG.
                allocate(prmfst%out%trng(j)%y_tile(nml), prmfst%out%trng(j)%y_grid(na))
                prmfst%out%trng(j)%y_tile = 0.0; prmfst%out%trng(j)%y_grid = 0.0
            end do
        end if

        !> ALD and ALD_DOY.
        if (vname == PMFRSTVN_ALD .or. vname == PMFRSTVN_ALDDOY) then
            if (.not. associated(prmfst%out%ald%d_tile)) then
                allocate( &
                    prmfst%out%ald%y_tile(nml), prmfst%out%ald%d_tile(nml), &
                    prmfst%out%ald%y_grid(na), prmfst%out%ald%d_grid(na))
                prmfst%out%ald%y_tile = 0.0; prmfst%out%ald%d_tile = 0.0
                prmfst%out%ald%y_grid = 0.0; prmfst%out%ald%d_grid = 0.0
                allocate(prmfst%out%alddoy%y_tile(nml), prmfst%out%alddoy%y_grid(na))
                prmfst%out%alddoy%y_tile = 0.0; prmfst%out%alddoy%y_grid = 0.0
            end if
        end if

        !> ALD_ENV.
        if (vname == PMFRSTVN_ALDENV) then
            if (.not. associated(prmfst%out%aldenv%y_tile)) then
                allocate(prmfst%out%aldenv%y_tile(nml), prmfst%out%aldenv%y_grid(na))
                prmfst%out%aldenv%y_tile = 0.0; prmfst%out%aldenv%y_grid = 0.0
            end if
        end if

        !> ZOD.
        if (vname == PMFRSTVN_ZOD) then

            !> Set zero tolerance if none were specified.
            if (.not. allocated(prmfst%pm%zod_ttol)) then
                allocate(prmfst%pm%zod_ttol(1))
                prmfst%pm%zod_ttol(1) = 0.1
            end if
            if (.not. allocated(prmfst%out%zod)) then
                allocate(prmfst%out%zod(size(prmfst%pm%zod_ttol)))
                do j = 1, size(prmfst%pm%zod_ttol)
                    if (.not. associated(prmfst%out%zod(j)%y_tile)) then
                        allocate(prmfst%out%zod(j)%y_tile(nml), prmfst%out%zod(j)%y_grid(na))
                        prmfst%out%zod(j)%y_tile = 0.0; prmfst%out%zod(j)%y_grid = 0.0
                    end if
                end do
            end if
        end if

        !> Enable the routine.
        prmfst%PROCESS_ACTIVE = .true.

    end subroutine

    subroutine permafrost_outputs_update(fls, shd)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer j, k, n
        real zbot(shd%lc%IGND)
        real tavg_tile(shd%lc%NML, shd%lc%IGND), tmax_tile(shd%lc%NML, shd%lc%IGND), tmin_tile(shd%lc%NML, shd%lc%IGND)
        real tavg_grid(shd%NA, shd%lc%IGND), tmax_grid(shd%NA, shd%lc%IGND), tmin_grid(shd%NA, shd%lc%IGND)

        !> Return if the process is not enabled.
        if (.not. prmfst%PROCESS_ACTIVE .or. ipid /= 0) return

        !> Local variables.
        zbot = shd%lc%sl%zbot

        !> Update daily temperature values.
        do j = 1, shd%lc%IGND

            !> Reset daily and yearly values if the first time-step in the day and year.
            if (ic%ts_daily == 1) then
                prmfst%out%tavg(j)%d_tile = 0.0
                prmfst%out%tavg(j)%d_grid = 0.0
            end if
            if (ic%ts_yearly == 1) then
                prmfst%out%tavg(j)%y_tile = 0.0; prmfst%out%tmax(j)%y_tile = 100.0; prmfst%out%tmin(j)%y_tile = 900.0
                prmfst%out%tavg(j)%y_grid = 0.0; prmfst%out%tmax(j)%y_grid = 100.0; prmfst%out%tmin(j)%y_grid = 900.0
                if (associated(prmfst%out%ald%d_tile)) then
                    prmfst%out%ald%y_tile = 0.0
                    prmfst%out%ald%y_grid = 0.0
                    prmfst%out%alddoy%y_tile = 0.0
                    prmfst%out%alddoy%y_grid = 0.0
                end if
                if (associated(prmfst%out%aldenv%y_tile)) then
                    prmfst%out%aldenv%y_tile = 0.0
                    prmfst%out%aldenv%y_grid = 0.0
                end if
            end if

            !> Tile-based.
            where (vs%tile%tbar(:, j) > 173.16 .and. vs%tile%tbar(:, j) < 373.16)
                prmfst%out%tavg(j)%d_tile = prmfst%out%tavg(j)%d_tile + vs%tile%tbar(:, j)
            elsewhere
                prmfst%out%tavg(j)%d_tile = out%NO_DATA
            end where

            !> Grid-based.
            where (vs%grid%tbar(:, j) > 173.16 .and. vs%grid%tbar(:, j) < 373.16)
                prmfst%out%tavg(j)%d_grid = prmfst%out%tavg(j)%d_grid + vs%grid%tbar(:, j)
            elsewhere
                prmfst%out%tavg(j)%d_grid = out%NO_DATA
            end where
        end do

        !> End of day outputs (daily).
        if (ic%now%day /= ic%next%day) then

            !> Calculate statistics and transform the variables to an array compatible with the function call.
            do j = 1, shd%lc%IGND

                !> Tile-based.
                where (prmfst%out%tavg(j)%d_tile /= out%NO_DATA)
                    prmfst%out%tavg(j)%d_tile = prmfst%out%tavg(j)%d_tile/ic%ts_daily
                    tavg_tile(:, j) = prmfst%out%tavg(j)%d_tile
                elsewhere
                    tavg_tile(:, j) = 0.0
                    prmfst%out%tavg(j)%d_tile = out%NO_DATA
                end where

                !> Grid-based.
                where (prmfst%out%tavg(j)%d_grid /= out%NO_DATA)
                    prmfst%out%tavg(j)%d_grid = prmfst%out%tavg(j)%d_grid/ic%ts_daily
                    tavg_grid(:, j) = prmfst%out%tavg(j)%d_grid
                elsewhere
                    tavg_grid(:, j) = 0.0
                    prmfst%out%tavg(j)%d_grid = out%NO_DATA
                end where
            end do

            !> Calculate ALD using daily average temperature (assign NO_DATA value if ALD == 0.0).
            if (associated(prmfst%out%ald%d_tile)) then
                call permafrost_ald(tavg_tile, zbot, prmfst%out%ald%d_tile, shd%lc%NML, shd%lc%IGND, 1, shd%lc%NML)
                where (.not. prmfst%out%ald%d_tile > 0.0) prmfst%out%ald%d_tile = out%NO_DATA
                call permafrost_ald(tavg_grid, zbot, prmfst%out%ald%d_grid, shd%NA, shd%lc%IGND, 1, shd%NA)
                where (.not. prmfst%out%ald%d_grid > 0.0) prmfst%out%ald%d_grid = out%NO_DATA

                !> Store the day when ALD occurs for yearly output.
                where (prmfst%out%ald%d_tile > prmfst%out%ald%y_tile)
                    prmfst%out%ald%y_tile = prmfst%out%ald%d_tile
                    prmfst%out%alddoy%y_tile = ic%now%jday
                end where
                where (prmfst%out%ald%d_grid > prmfst%out%ald%y_grid)
                    prmfst%out%ald%y_grid = prmfst%out%ald%d_grid
                    prmfst%out%alddoy%y_grid = ic%now%jday
                end where
            end if

            !> Yearly statistics (based on daily values).
            do j = 1, shd%lc%IGND

                !> Tile-based.
                where (prmfst%out%tavg(j)%d_tile /= out%NO_DATA)
                    prmfst%out%tavg(j)%y_tile = prmfst%out%tavg(j)%y_tile + prmfst%out%tavg(j)%d_tile*ic%ts_daily
                    prmfst%out%tmax(j)%y_tile = max(prmfst%out%tmax(j)%y_tile, prmfst%out%tavg(j)%d_tile)
                    prmfst%out%tmin(j)%y_tile = min(prmfst%out%tmin(j)%y_tile, prmfst%out%tavg(j)%d_tile)
                elsewhere
                    prmfst%out%tavg(j)%y_tile = out%NO_DATA
                end where

                !> Grid-based.
                where (prmfst%out%tavg(j)%d_grid /= out%NO_DATA)
                    prmfst%out%tavg(j)%y_grid = prmfst%out%tavg(j)%y_grid + prmfst%out%tavg(j)%d_grid*ic%ts_daily
                    prmfst%out%tmax(j)%y_grid = max(prmfst%out%tmax(j)%y_grid, prmfst%out%tavg(j)%d_grid)
                    prmfst%out%tmin(j)%y_grid = min(prmfst%out%tmin(j)%y_grid, prmfst%out%tavg(j)%d_grid)
                elsewhere
                    prmfst%out%tavg(j)%y_grid = out%NO_DATA
                end where
            end do

            !> End of year outputs (yearly).
            if (ic%now%year /= ic%next%year) then

                !> Calculate statistics and transform the variables to an array compatible with the function call.
                do j = 1, shd%lc%IGND

                    !> Tile-based.
                    where (prmfst%out%tavg(j)%y_tile /= out%NO_DATA)
                        prmfst%out%tavg(j)%y_tile = prmfst%out%tavg(j)%y_tile/ic%ts_yearly
                        prmfst%out%trng(j)%y_tile = prmfst%out%tmax(j)%y_tile - prmfst%out%tmin(j)%y_tile
                        tavg_tile(:, j) = prmfst%out%tavg(j)%y_tile
                        tmax_tile(:, j) = prmfst%out%tmax(j)%y_tile
                        tmin_tile(:, j) = prmfst%out%tmin(j)%y_tile
                    elsewhere
                        tavg_tile(:, j) = 0.0
                        tmax_tile(:, j) = 0.0
                        tmin_tile(:, j) = 0.0
                        prmfst%out%tavg(j)%y_tile = out%NO_DATA
                        prmfst%out%tmax(j)%y_tile = out%NO_DATA
                        prmfst%out%tmin(j)%y_tile = out%NO_DATA
                        prmfst%out%trng(j)%y_tile = out%NO_DATA
                    end where

                    !> Grid-based.
                    where (prmfst%out%tavg(j)%y_grid /= out%NO_DATA)
                        prmfst%out%tavg(j)%y_grid = prmfst%out%tavg(j)%y_grid/ic%ts_yearly
                        prmfst%out%trng(j)%y_grid = prmfst%out%tmax(j)%y_grid - prmfst%out%tmin(j)%y_grid
                        tavg_grid(:, j) = prmfst%out%tavg(j)%y_grid
                        tmax_grid(:, j) = prmfst%out%tmax(j)%y_grid
                        tmin_grid(:, j) = prmfst%out%tmin(j)%y_grid
                    elsewhere
                        tavg_grid(:, j) = 0.0
                        tmax_grid(:, j) = 0.0
                        tmin_grid(:, j) = 0.0
                        prmfst%out%tavg(j)%y_grid = out%NO_DATA
                        prmfst%out%tmax(j)%y_grid = out%NO_DATA
                        prmfst%out%tmin(j)%y_grid = out%NO_DATA
                        prmfst%out%trng(j)%y_grid = out%NO_DATA
                    end where
                end do

                !> Calculate ALD using annual temperature envelope (assign NO_DATA value if ALD == 0.0).
                if (associated(prmfst%out%aldenv%y_tile)) then
                    call permafrost_ald(tmax_tile, zbot, prmfst%out%aldenv%y_tile, shd%lc%NML, shd%lc%IGND, 1, shd%lc%NML)
                    where (.not. prmfst%out%aldenv%y_tile > 0.0) prmfst%out%aldenv%y_tile = out%NO_DATA
                    call permafrost_ald(tmax_grid, zbot, prmfst%out%aldenv%y_grid, shd%NA, shd%lc%IGND, 1, shd%NA)
                    where (.not. prmfst%out%aldenv%y_grid > 0.0) prmfst%out%aldenv%y_grid = out%NO_DATA
                end if

                !> Assign NO_DATA value where ALD fields based on daily temperature equal zero.
                if (associated(prmfst%out%ald%d_tile)) then
                    where (.not. prmfst%out%ald%y_tile > 0.0) prmfst%out%ald%y_tile = out%NO_DATA
                    where (.not. prmfst%out%ald%y_grid > 0.0) prmfst%out%ald%y_grid = out%NO_DATA
                    where (.not. prmfst%out%alddoy%y_tile > 0.0) prmfst%out%alddoy%y_tile = out%NO_DATA
                    where (.not. prmfst%out%alddoy%y_grid > 0.0) prmfst%out%alddoy%y_grid = out%NO_DATA
                end if

                !> Calculate ZOD using annual temperature envelope (assign NO_DATA value if ZOD == 0.0).
                if (allocated(prmfst%out%zod)) then
                    do j = 1, size(prmfst%pm%zod_ttol)

                        !> Tile-based.
                        call permafrost_zod( &
                            tmax_tile, tmin_tile, zbot, prmfst%pm%zod_ttol(j), prmfst%out%zod(j)%y_tile, &
                            shd%lc%NML, shd%lc%IGND, 1, shd%lc%NML)
                        where (.not. prmfst%out%zod(j)%y_tile > 0.0) prmfst%out%zod(j)%y_tile = out%NO_DATA

                        !> Grid-based.
                        call permafrost_zod( &
                            tmax_grid, tmin_grid, zbot, prmfst%pm%zod_ttol(j), prmfst%out%zod(j)%y_grid, &
                            shd%NA, shd%lc%IGND, 1, shd%NA)
                        where (.not. prmfst%out%zod(j)%y_grid > 0.0) prmfst%out%zod(j)%y_grid = out%NO_DATA
                    end do
                end if
            end if
        end if

    end subroutine

end module
