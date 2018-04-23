module permafrost_outputs_module

    !> 'sa_mesh_variables' required for SA_MESH variables, parameters, and counter.
    !> 'sa_mesh_utilities' required for I/O to screen and 'echo_print.txt', and for character defaults.
    !> 'model_dates' required for 'ic' counter.
    !> 'model_files_variables' required for 'fls' object and file keys.
    !> 'mpi_module' required for MPI variables, tile/grid parsing utility, barrier flag.
    use sa_mesh_variables
    use sa_mesh_utilities
    use model_dates
    use model_files_variables
    use mpi_module

    implicit none

    !> Variable names.
    character(len = 10), parameter :: PMFRSTVN_ALDD = 'ALD'
    character(len = 10), parameter :: PMFRSTVN_ALDE = 'ALDE'
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
    !*  aldd: Active layer depth calculated using daily average temperature (1: Tile index). [m].
    !*  alde: Active layer depth calculated using temperature envelope (1: Tile index). [m].
    !*  ald(d/e)_jday: Day of maximum ALD for yearly output (1: Tile index). [--].
    !*  tavg: Average soil temperature of each layer (1: Tile index; 2: Soil layer). [K].
    !*  tmax: Maximum soil temperature of each layer (1: Tile index; 2: Soil layer). [K].
    !*  tmin: Minimum soil temperature of each layer (1: Tile index; 2: Soil layer). [K].
    !*  trng: Range/envlope of soil temperature of each layer (1: Tile index; 2: Soil layer). [K].
    !*  zod: Zero oscillation depths for each temperature threshold (1: Tile index; 2: TTOL). [m].
    type permafrost_outputs_fields
        type(output_fields_surrogate) aldd, aldd_jday
        type(output_fields_surrogate) alde, alde_jday
        type(output_fields_surrogate), dimension(:), allocatable :: tavg, tmax, tmin, trng, zod
    end type

    !> Description:
    !>  Container for flags, parameters, and variables.
    !>
    !> Variables:
    !*  pm: Parameter group.
    !*  y, m, d: Output interval of variables. [--].
    type permafrost_outputs_container
        logical :: PROCESS_ACTIVE = .false.
        character(len = DEFAULT_LINE_LENGTH) :: PERMAFROSTOUTFLAG = ''
        type(permafrost_outputs_parameters) pm
        type(permafrost_outputs_fields) out
    end type

    type(permafrost_outputs_container), save :: prmfst

    contains

    subroutine permafrost_outputs_init(fls, shd)

        !> strings: For 'is_letter' and 'value' functions.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer na, nml, nsl, nttol, nargs, i, j, n, z
        character(len = DEFAULT_FIELD_LENGTH), dimension(50) :: args
        character(len = DEFAULT_LINE_LENGTH) line

        !> Return if the process is not enabled.
        if (len_trim(prmfst%PERMAFROSTOUTFLAG) == 0 .or. ipid /= 0) return

        !> Parse the options of 'PERMAFROSTOUTFLAG'.
        call parse(prmfst%PERMAFROSTOUTFLAG, ' ', args, nargs)

        !> Return if no arguments
        if (.not. nargs > 1) return

        !> Interpret options.
        do i = 2, nargs
            select case (args(i))

                !> Disabled.
                case ('off', '0')
                    prmfst%PROCESS_ACTIVE = .false.
                    return

                !> User-defined temperature threshold(s)/tolerance(s) for ZOD.
                case ('ttol')
                    n = 0
                    do j = i + 1, nargs
                        if (is_letter(args(j))) exit
                        n = n + 1
                    end do
                    z = 0
                    if (n > 0) then
                        allocate(prmfst%pm%zod_ttol(n))
                        do j = 1, n
                            call value(args(i + j), prmfst%pm%zod_ttol(j), z)
                        end do
                    end if
            end select
        end do

        !> Enable the routine.
        prmfst%PROCESS_ACTIVE = .true.

        !> Set zero tolerance if none were specified.
        if (.not. allocated(prmfst%pm%zod_ttol)) then
            allocate(prmfst%pm%zod_ttol(1))
            prmfst%pm%zod_ttol(1) = 0.0
        end if

        !> Summarize current PERMAFROSTOUTFLAG configuration to output.
        call print_message('PERMAFROSTOUT component ACTIVATED')
        write(line, FMT_GEN) prmfst%pm%zod_ttol
        call print_echo_txt('ZOD_TTOL ' // trim(adjustl(line)), PAD_3)

        !> Print an error and stop if 'OUTFIELDSFLAG' is not enabled.
!?        call print_error('OUTFIELDSFLAG is required for permafrost outputs (PERMAFROSTOUTFLAG).')
!?        call print_message('Enable and configure OUTFIELDSFLAG or disable PERMAFROSTOUTFLAG in the list of control flags.')

        !> Print an error and stop if 'TBAR' is not active in the 'stas' variables.
        if (.not. allocated(stas%sl%tbar)) then
            call print_warning( &
                "The model variable 'TBAR' is not active. Permafrost outputs (PERMAFROSTOUTFLAG) will not be created.", PAD_3)
            prmfst%PROCESS_ACTIVE = .false.
            return
        end if

        !> Allocate output variables.
        na = shd%NA
        nml = shd%lc%NML
        nsl = shd%lc%IGND
        nttol = size(prmfst%pm%zod_ttol)
        allocate( &
            prmfst%out%aldd%y_tile(nml), prmfst%out%aldd_jday%y_tile(nml), prmfst%out%aldd%d_tile(nml), &
            prmfst%out%aldd%y_grid(na), prmfst%out%aldd_jday%y_grid(na), prmfst%out%aldd%d_grid(na), &
            prmfst%out%alde%y_tile(nml), prmfst%out%alde_jday%y_tile(nml), prmfst%out%alde%d_tile(nml), &
            prmfst%out%alde%y_grid(na), prmfst%out%alde_jday%y_grid(na), prmfst%out%alde%d_grid(na))
        prmfst%out%aldd%y_tile = 0.0; prmfst%out%aldd_jday%y_tile = 0; prmfst%out%aldd%d_tile = 0.0
        prmfst%out%aldd%y_grid = 0.0; prmfst%out%aldd_jday%y_grid = 0; prmfst%out%aldd%d_grid = 0.0
        prmfst%out%alde%y_tile = 0.0; prmfst%out%alde_jday%y_tile = 0; prmfst%out%alde%d_tile = 0.0
        prmfst%out%alde%y_grid = 0.0; prmfst%out%alde_jday%y_grid = 0; prmfst%out%alde%d_grid = 0.0
        allocate(prmfst%out%tavg(nsl), prmfst%out%tmax(nsl), prmfst%out%tmin(nsl), prmfst%out%trng(nsl))
        do j = 1, nsl
            allocate( &
                prmfst%out%tavg(j)%y_tile(nml), prmfst%out%tavg(j)%d_tile(nml), &
                prmfst%out%tmax(j)%y_tile(nml), prmfst%out%tmax(j)%d_tile(nml), &
                prmfst%out%tmin(j)%y_tile(nml), prmfst%out%tmin(j)%d_tile(nml), &
                prmfst%out%trng(j)%y_tile(nml), prmfst%out%trng(j)%d_tile(nml), &
                prmfst%out%tavg(j)%y_grid(na), prmfst%out%tavg(j)%d_grid(na), &
                prmfst%out%tmax(j)%y_grid(na), prmfst%out%tmax(j)%d_grid(na), &
                prmfst%out%tmin(j)%y_grid(na), prmfst%out%tmin(j)%d_grid(na), &
                prmfst%out%trng(j)%y_grid(na), prmfst%out%trng(j)%d_grid(na))
            prmfst%out%tavg(j)%y_tile = 0.0; prmfst%out%tavg(j)%d_tile = 0.0
            prmfst%out%tmax(j)%y_tile = 100.0; prmfst%out%tmax(j)%d_tile = 100.0
            prmfst%out%tmin(j)%y_tile = 900.0; prmfst%out%tmin(j)%d_tile = 900.0
            prmfst%out%trng(j)%y_tile = 0.0; prmfst%out%trng(j)%d_tile = 0.0
            prmfst%out%tavg(j)%y_grid = 0.0; prmfst%out%tavg(j)%d_grid = 0.0
            prmfst%out%tmax(j)%y_grid = 100.0; prmfst%out%tmax(j)%d_grid = 100.0
            prmfst%out%tmin(j)%y_grid = 900.0; prmfst%out%tmin(j)%d_grid = 900.0
            prmfst%out%trng(j)%y_grid = 0.0; prmfst%out%trng(j)%d_grid = 0.0
        end do
        allocate(prmfst%out%zod(nttol))
        do j = 1, nttol
            allocate( &
                prmfst%out%zod(j)%y_tile(nml), prmfst%out%zod(j)%d_tile(nml), &
                prmfst%out%zod(j)%y_grid(na), prmfst%out%zod(j)%d_grid(na))
            prmfst%out%zod(j)%y_tile = 0.0; prmfst%out%zod(j)%d_tile = 0.0
            prmfst%out%zod(j)%y_grid = 0.0; prmfst%out%zod(j)%d_grid = 0.0
        end do

    end subroutine

    subroutine permafrost_outputs_within_tile(fls, shd)

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
                prmfst%out%tavg(j)%d_tile = 0.0; prmfst%out%tmax(j)%d_tile = 100.0; prmfst%out%tmin(j)%d_tile = 900.0
                prmfst%out%tavg(j)%d_grid = 0.0; prmfst%out%tmax(j)%d_grid = 100.0; prmfst%out%tmin(j)%d_grid = 900.0
            end if
            if (ic%ts_yearly == 1) then
                prmfst%out%tavg(j)%y_tile = 0.0; prmfst%out%tmax(j)%y_tile = 100.0; prmfst%out%tmin(j)%y_tile = 900.0
                prmfst%out%tavg(j)%y_grid = 0.0; prmfst%out%tmax(j)%y_grid = 100.0; prmfst%out%tmin(j)%y_grid = 900.0
                prmfst%out%aldd%y_tile = 0.0; prmfst%out%aldd_jday%y_tile = 0
                prmfst%out%aldd%y_grid = 0.0; prmfst%out%aldd_jday%y_grid = 0
                prmfst%out%alde%y_tile = 0.0; prmfst%out%alde_jday%y_tile = 0
                prmfst%out%alde%y_grid = 0.0; prmfst%out%alde_jday%y_grid = 0
            end if

            !> Tile-based.
            where (stas%sl%tbar(:, j) > 173.16 .and. stas%sl%tbar(:, j) < 373.16)
                prmfst%out%tavg(j)%d_tile = prmfst%out%tavg(j)%d_tile + stas%sl%tbar(:, j)
                prmfst%out%tmax(j)%d_tile = max(prmfst%out%tmax(j)%d_tile, stas%sl%tbar(:, j))
                prmfst%out%tmin(j)%d_tile = min(prmfst%out%tmin(j)%d_tile, stas%sl%tbar(:, j))
            elsewhere
                prmfst%out%tavg(j)%d_tile = out%NO_DATA
            end where

            !> Grid-based.
            where (stas_grid%sl%tbar(:, j) > 173.16 .and. stas_grid%sl%tbar(:, j) < 373.16)
                prmfst%out%tavg(j)%d_grid = prmfst%out%tavg(j)%d_grid + stas_grid%sl%tbar(:, j)
                prmfst%out%tmax(j)%d_grid = max(prmfst%out%tmax(j)%d_grid, stas_grid%sl%tbar(:, j))
                prmfst%out%tmin(j)%d_grid = min(prmfst%out%tmin(j)%d_grid, stas_grid%sl%tbar(:, j))
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
                    prmfst%out%trng(j)%d_tile = prmfst%out%tmax(j)%d_tile - prmfst%out%tmin(j)%d_tile
                    tavg_tile(:, j) = prmfst%out%tavg(j)%d_tile
                    tmax_tile(:, j) = prmfst%out%tmax(j)%d_tile
                    tmin_tile(:, j) = prmfst%out%tmin(j)%d_tile
                elsewhere
                    tavg_tile(:, j) = 0.0
                    tmax_tile(:, j) = 0.0
                    tmin_tile(:, j) = 0.0
                    prmfst%out%tavg(j)%d_tile = out%NO_DATA
                    prmfst%out%tmax(j)%d_tile = out%NO_DATA
                    prmfst%out%tmin(j)%d_tile = out%NO_DATA
                    prmfst%out%trng(j)%d_tile = out%NO_DATA
                end where

                !> Grid-based.
                where (prmfst%out%tavg(j)%d_grid /= out%NO_DATA)
                    prmfst%out%tavg(j)%d_grid = prmfst%out%tavg(j)%d_grid/ic%ts_daily
                    prmfst%out%trng(j)%d_grid = prmfst%out%tmax(j)%d_grid - prmfst%out%tmin(j)%d_grid
                    tavg_grid(:, j) = prmfst%out%tavg(j)%d_grid
                    tmax_grid(:, j) = prmfst%out%tmax(j)%d_grid
                    tmin_grid(:, j) = prmfst%out%tmin(j)%d_grid
                elsewhere
                    tavg_grid(:, j) = 0.0
                    tmax_grid(:, j) = 0.0
                    tmin_grid(:, j) = 0.0
                    prmfst%out%tavg(j)%d_grid = out%NO_DATA
                    prmfst%out%tmax(j)%d_grid = out%NO_DATA
                    prmfst%out%tmin(j)%d_grid = out%NO_DATA
                    prmfst%out%trng(j)%d_grid = out%NO_DATA
                end where
            end do

            !> Calculate ALD (assign NO_DATA value if ALD == 0.0).
            call permafrost_ald(tavg_tile, zbot, prmfst%out%aldd%d_tile, shd%lc%NML, shd%lc%IGND, 1, shd%lc%NML)
            where (.not. prmfst%out%aldd%d_tile > 0.0) prmfst%out%aldd%d_tile = out%NO_DATA
            call permafrost_ald(tavg_grid, zbot, prmfst%out%aldd%d_grid, shd%NA, shd%lc%IGND, 1, shd%NA)
            where (.not. prmfst%out%aldd%d_grid > 0.0) prmfst%out%aldd%d_grid = out%NO_DATA

            !> Store day when ALD occurs for yearly output.
            where (prmfst%out%aldd%d_tile > prmfst%out%aldd%y_tile)
                prmfst%out%aldd%y_tile = prmfst%out%aldd%d_tile
                prmfst%out%aldd_jday%y_tile = ic%now%jday
            end where
            where (prmfst%out%aldd%d_grid > prmfst%out%aldd%y_grid)
                prmfst%out%aldd%y_grid = prmfst%out%aldd%d_grid
                prmfst%out%aldd_jday%y_grid = ic%now%jday
            end where

            !> Calculate ZOD (assign NO_DATA value if ZOD == 0.0).
            do j = 1, size(prmfst%pm%zod_ttol)

                !> Tile-based.
                call permafrost_zod( &
                    tmax_tile, tmin_tile, zbot, prmfst%pm%zod_ttol(j), prmfst%out%zod(j)%d_tile, &
                    shd%lc%NML, shd%lc%IGND, 1, shd%lc%NML)
                where (.not. prmfst%out%zod(j)%d_tile > 0.0) prmfst%out%zod(j)%d_tile = out%NO_DATA

                !> Grid-based.
                call permafrost_zod( &
                    tmax_grid, tmin_grid, zbot, prmfst%pm%zod_ttol(j), prmfst%out%zod(j)%d_grid, &
                    shd%NA, shd%lc%IGND, 1, shd%NA)
                where (.not. prmfst%out%zod(j)%d_grid > 0.0) prmfst%out%zod(j)%d_grid = out%NO_DATA
            end do

            !> Yearly statistics (based on daily values).
            do j = 1, shd%lc%IGND

                !> Tile-based.
                where (prmfst%out%tavg(j)%d_tile /= out%NO_DATA)
                    prmfst%out%tavg(j)%y_tile = prmfst%out%tavg(j)%y_tile + prmfst%out%tavg(j)%d_tile
                    prmfst%out%tmax(j)%y_tile = max(prmfst%out%tmax(j)%y_tile, prmfst%out%tmax(j)%d_tile)
                    prmfst%out%tmin(j)%y_tile = min(prmfst%out%tmin(j)%y_tile, prmfst%out%tmin(j)%d_tile)
                elsewhere
                    prmfst%out%tavg(j)%y_tile = out%NO_DATA
                end where

                !> Grid-based.
                where (prmfst%out%tavg(j)%d_grid /= out%NO_DATA)
                    prmfst%out%tavg(j)%y_grid = prmfst%out%tavg(j)%y_grid + prmfst%out%tavg(j)%d_grid
                    prmfst%out%tmax(j)%y_grid = max(prmfst%out%tmax(j)%y_grid, prmfst%out%tmax(j)%d_grid)
                    prmfst%out%tmin(j)%y_grid = min(prmfst%out%tmin(j)%y_grid, prmfst%out%tmin(j)%d_grid)
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

                !> Calculate ZOD (assign NO_DATA value if ZOD == 0.0).
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

    end subroutine

end module
