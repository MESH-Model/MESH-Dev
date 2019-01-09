!> Description:
!>  Subroutine to resume the run state from file.
subroutine resumerun_read(fls, shd, cm)

    use model_files_variables
    use sa_mesh_common
    use climate_forcing
    use sa_mesh_run_within_tile
    use sa_mesh_run_within_grid
!+    use sa_mesh_run_between_grid

    !> Process modules.
    use FLAGS, only: NRSOILAYEREADFLAG
    use RUNCLASS36_constants
    use RUNCLASS36_variables
    use RUNCLASS36_config
    use RUNSVS113_variables
    use WF_ROUTE_config
    use area_watflood, only: fhr
    use rte_module
    use baseflow_module
    use save_basin_output
    use SIMSTATS

    implicit none

    !> Input variables.
    type(fl_ids) fls
    type(ShedGridParams) shd
    type(clim_info) cm

    !> Local variables.
    integer iun, ignd, k, j, i, m, z
    character(len = DEFAULT_LINE_LENGTH) args(100), line
    character(len = DEFAULT_FIELD_LENGTH) fname
    logical lstate

    !> Distribute GRU-based values.
    do k = 1, shd%lc%NML

        !> Grab the indices of the grid cell and GRU.
        i = shd%lc%ILMOS(k)
        m = shd%lc%JLMOS(k)

        !> RUNCLASS36 and RUNSVS113.
        if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
            stas%cnpy%tcan(k) = stas_gru%cnpy%tcan(m) + TFREZ
            stas%sno%tsno(k) = stas_gru%sno%tsno(m) + TFREZ
            stas%sno%rhos(k) = stas_gru%sno%rhos(m)
            stas%sno%albs(k) = stas_gru%sno%albs(m)
            stas%sl%tbar(k, :) = stas_gru%sl%tbar(m, :) + TFREZ
            stas%sl%thlq(k, :) = stas_gru%sl%thlq(m, :)
        end if

        !> RUNCLASS36.
        if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
            stas%cnpy%tac(k) = stas_gru%cnpy%tcan(m) + TFREZ
            stas%cnpy%qac = 0.5e-2
            stas%sfc%tpnd(k) = stas_gru%sfc%tpnd(m) + TFREZ
            stas%sfc%zpnd(k) = stas_gru%sfc%zpnd(m)
            stas%cnpy%rcan(k) = stas_gru%cnpy%rcan(m)
            stas%cnpy%sncan(k) = stas_gru%cnpy%sncan(m)
            stas%sno%sno(k) = stas_gru%sno%sno(m)
            stas%cnpy%gro(k) = stas_gru%cnpy%gro(m)
            stas%sfc%tsfs(k, 1) = TFREZ
            stas%sfc%tsfs(k, 2) = TFREZ
            stas%sfc%tsfs(k, 3) = stas_gru%sl%tbar(m, 1) + TFREZ
            stas%sfc%tsfs(k, 4) = stas_gru%sl%tbar(m, 1) + TFREZ
            stas%sl%tbas(k) = stas_gru%sl%tbar(m, shd%lc%IGND) + TFREZ
            stas%sl%thic(k, :) = stas_gru%sl%thic(m, :)
        end if
    end do !k = il1, il2

    !> Distribute soil states to layers lower than the "last configured layer".
    if (RUNCLASS36_flgs%PROCESS_ACTIVE) then

        !> Determine the "last configured layer" read from file (CLASS default: 3).
        if (NRSOILAYEREADFLAG > 3) then
            ignd = min(NRSOILAYEREADFLAG, shd%lc%IGND)
        else if (NRSOILAYEREADFLAG == 1) then
            ignd = 0
        else
            ignd = 3
        end if

        !> Assign states to layers lower than the "last configured layer" read from file.
        if (ignd > 0) then
            do j = (ignd + 1), shd%lc%IGND
                stas%sl%tbar(:, j) = stas%sl%tbar(:, ignd)
                stas%sl%thlq(:, j) = stas%sl%thlq(:, ignd)
                stas%sl%thic(:, j) = stas%sl%thic(:, ignd)
            end do
        end if
    end if

!?    !> Check for auto resume file.
!?    if (vs%flgs%resume%state == FLAG_AUTO) then
!?        fname = 'auto_resume.ini'
!?!+        call reset_tab()
!?        call print_message('READING: ' // trim(fname))
!?!+        call increase_tab()
!?        inquire(file = fname, exist = lstate)
!?        if (lstate) then
!?
!?            !> Open the file.
!?            iun = 100
!?            open(iun, file = fname, status = 'old', action = 'read', iostat = z)
!?            if (z /= 0) then
!?                call print_error('Unable to open the file.')
!?                call program_abort()
!?            end if
!?
!?            !> Read the simulation start date from the file.
!?            read(iun, *, iostat = z) ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
!?            if (z /= 0) then
!?                call print_error('An error occurred reading the simulation resume date from the file.')
!?                call program_abort()
!?            end if
!?            write(line, "(i5, '/', i3.3, ' ', i2.2, ':', i2.2)") ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
!?            call print_message( &
!?                'Simulation start revised to: ' // trim(adjustl(line)) // '. The previous run state will be resumed.')
!?            close(iun)
!?        else
!?
!?            !> Print a warning if the resume file does not exist.
!?            call print_warning( &
!?                'Auto-resume is active but ' // trim(fname) // ' cannot be found. No previous run state is resumed.')
!?
!?            !> Override the resume functionality.
!?            vs%flgs%resume%state = FLAG_OFF
!?        end if
!?    end if

    !> Read files.
    if (.not. vs%flgs%resume%state == FLAG_OFF) then

        !> txt: In text format.

        !> seq: Sequential binary format.
        if (btest(vs%flgs%resume%flo%ffmt, FFMT_SEQ)) then
            if (index(vs%flgs%resume%bin, '+STASONLY') == 0 .and. index(vs%flgs%resume%bin, '+CLASSPROG') == 0) then
                lstate = climate_module_resume_read(fls, shd, cm)
                call read_init_prog_variables_class(fls)
                call bflm_resume_read(fls, shd)
                call WF_ROUTE_resume_read(fls, shd)
                call run_rte_resume_read(fls, shd)
                call run_save_basin_output_resume_read(fls, shd)
                call stats_state_resume(fls)
            else if (index(vs%flgs%resume%bin, '+CLASSPROG') == 0) then
                call read_init_prog_variables_class(fls)
                call bflm_resume_read(fls, shd)
                call WF_ROUTE_resume_read_nots(fls, shd)
                call run_rte_resume_read_nots(fls, shd)
            else
                call read_init_prog_variables_class_row(fls, shd)
            end if
        end if

        !> r2c: From r2c by grid.

        !> csv: From CSV by GRU.
    end if

    !> Update derived values.
    call run_within_tile_stas_update(fls, shd, cm)
    call run_within_grid_stas_update(fls, shd, cm)

end subroutine
