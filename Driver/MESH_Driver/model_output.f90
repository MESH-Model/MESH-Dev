module model_output

    use sa_mesh_shared_variables
    use model_dates
    use model_files_variables

    implicit none

    real, allocatable :: TBAR_dly(:, :, :), ALD_dly(:, :), TMAX_ann(:, :, :), TMIN_ann(:, :, :), ALD_ann(:, :), ZOD_ann(:, :, :)
    real, allocatable :: ZOD_TTOL(:)

    !> Description:
    !>  Data type for output file information for a specific format.
    !>
    !> Variables:
    !*  active: .true. if the file is active (default: .false.).
    !*  iun: Unit of the file (must be unique; default: -1).
    !*  path: Path or file name (default: none).
    type output_file_format
        logical :: active = .false.
        integer :: iun = -1
        character(len = 1000) :: path = ''
    end type

    !> Description:
    !>  Data type for output file format information.
    !>
    !> Variables:
    !*  active: .true. if output is active (default: .false.).
    !*  delim: Field delimiter in supported formats (default: space-delimited).
    !*  print_date: Option to print the leading date stamp in supported formats (default: .false.).
    !*  order: Print order of elements in the field in supported formats (default: 'gridorder').
    !*  grids, tiles, grus: Indices of the specific grids/tiles/GRUs for output in supported formats (text format).
    !*  dat: Output data (1: Tile or grid index; 2: Index in series).
    !*  seq, r2c, txt, csv: File information for outputs in various formats.
    type output_file
        logical :: active = .false.
        character(len = 20) :: delim = '', order = 'unknown'
        logical :: print_date = .false.
        integer, dimension(:), allocatable :: grids, tiles, grus
        real, dimension(:, :), allocatable :: dat
        type(output_file_format) seq, r2c, txt, csv
    end type

    !> Description:
    !>  Data type for output.
    !>
    !> Variables:
    !*  apply_frac: .true. to multiply grid values by fractional cell area (default: .false.).
    !*  y, m, s, d, h, ts: Output files of the variable at various time intervals.
    type output_field
        logical :: apply_frac = .false.
        type(output_file) y, m, s, d, h, ts
    end type

    !> Description:
    !>  Data type for storing series of dates.
    !>
    !> Variables:
    !*  y, m, s, d, h, ts: Arrays of dates at various time intervals.
    type output_dates
        integer, dimension(:, :), allocatable :: y, m, s, d, h
        integer, dimension(:), allocatable :: ts
    end type

    !> Description:
    !>  Data type for storing series and variables for output.
    !>
    !> Variables:
    !*  dates: Array to store frame count and date (1: Frame count and date; 2: Index in series).
    !*  iun: Base file unit (increments as fields are activated for output).
    !*  in_mem: .true. to store in memory; .false. to write output continuously during the run (default: .false.).
    type output_fields_container
        type(output_dates) dates
        integer :: iun = 882100
        logical :: in_mem = .false.
        type(output_field) &
            pre, fsin, fsvh, fsih, fsdr, fsdf, flin, ta, qa, pres, uu, vv, uv, wdir, &
            prec, gro, evap, pevp, evpb, arrd, rof, rofo, rofs, rofb, &
            rcan, sncan, sno, fsno, wsno, zpnd, pndw, lzs, dzs, stgw, &
            cmas, tcan, tsno, tpnd, &
            alvs, alir, albt, fsout, flout, gte, qh, qe, gzero, stge, &
            ald, zod, &
            rff, rchg, qi, stgch, qo, zlvl
        type(output_field), dimension(:), allocatable :: &
            thlq, lqws, thic, fzws, alws, &
            gflx, tbar, tmax, tmin
    end type

    !*  flds: Instance of variables for output.
    type(output_fields_container), save :: flds

    contains

!>>>>temporarily_here
    !> Description:
    !>  Open an existing text or CSV format file for input (the routine
    !>  implements the 'read' action). Stop the program if the routine
    !>  fails.
    subroutine open_txt_input(iun, fname, verbose, ierr)

        !> Input variables.
        !*  iun: Unit of the file (stream persists).
        !*  fname: Name of the file.
        !*  verbose: .true. to print messages to the screen.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname
        logical, intent(in) :: verbose

        !> Output variables.
        !*  ierr: Returns '0' if the routine is successful; program stops otherwise.
        integer, intent(out) :: ierr

        !> Open the file.
        !> Print status to the screen if verbose.
        if (verbose) print 1000, trim(fname)
        open(iun, file = adjustl(fname), status = 'old', action = 'read', iostat = ierr)
        if (ierr /= 0) goto 999

        !> Return if the file successfully opened.
        return

        !> Stop if the routine failed.
999     if (verbose) print 1110, trim(fname)
        stop

1000    format(1x, 'READING: ', (a))
1110    format(/1x, 'ERROR: Error opening ', (a), &
               /3x, 'Check that the file exists or use an alternate format of file.'/)

    end subroutine

    !> Description:
    !>  Return the first line of data from a text or CSV format file.
    !>  The routine skips lines that lead with '#' or '!', and clip the
    !>  line to the first instance of '#' or '!' if one exists. The
    !>  returned line is de-tabbed and compacted of excess whitespace.
    subroutine read_txt_line(iun, line, ierr)

        !> Required for the 'compact' function.
        use strings

        !> Input variables.
        !*  iun: Unit of the file (of persistent stream).
        integer, intent(in) :: iun

        !> Output variables.
        !*  line: First data line read from file.
        !*  ierr: Returns '0' if the routine is successful.
        character(len = *), intent(out) :: line
        integer, intent(out) :: ierr

        !> Loop until the first line of data is read.
        !> Skip blank lines and lines that lead with '#' or '!'.
        !> Clip the line to the first instance of '#' or '!' if one exists.
        line = ''
        do while (ierr == 0)
            read(iun, '(a)', iostat = ierr) line
            if (ierr /= 0) exit
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#' .or. line(1:1) == '!') cycle
            if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
            if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
            call compact(line)
            if (len_trim(line) > 0) exit
        end do

        !> Return when a line that satisfies the constraints is read.
        return

    end subroutine
!<<<<<temporarily_here

    subroutine output_file_parse_indices(shd, file, args, nargs, indices, startindex, ierr)

        !> Required for 'is_letter' and 'value' functions.
        use strings

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        character(len = *), dimension(:), intent(in) :: args
        integer, intent(in) :: nargs, startindex

        !> Input/output variables.
        type(output_file) file

        !> Output variables.
        integer, dimension(:), allocatable, intent(out) :: indices
        integer, intent(out) :: ierr

        !> Local variables.
        integer n, j

        !> Count the number of indices.
        n = 0
        do j = startindex + 1, nargs
            if (is_letter(args(j))) exit
            n = n + 1
        end do

        !> Allocate the vector and store the indices.
        ierr = 0
        if (n > 0) then
            allocate(indices(n))
            do j = 1, n
                call value(args(startindex + j), indices(j), ierr)
            end do
        end if

    end subroutine

    subroutine output_file_parse_options(shd, file, file_path, t, args, nargs)

        !> Required for 'is_letter', 'lowercase', and 'value' functions.
        use strings

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        character(len = *), intent(in) :: file_path, args(:)
        integer, intent(in) :: t, nargs

        !> Input/output variables.
        type(output_file) file

        !> Local variables.
        integer n, j, i, ierr
        character(len = len(args)) opts

        !> Mark the field as active.
        file%active = .true.

        !> Condition output based on 'args' flags.
        do i = 2, nargs
            if (is_letter(args(i))) then
                opts = lowercase(args(i))
                select case (opts)

                    !> Already read; skip to avoid 'REMARK' below.
                    case ('y', 'm', 's', 'd', 'h', 'ts')
                        cycle

                    !> Generic output formats.
                    case ('r2c')
                        file%r2c%active = .true.
                        flds%iun = flds%iun + 1
                        file%r2c%iun = flds%iun
                        file%r2c%path = trim(file_path) // '.r2c'
                    case ('seq', 'binseq')
                        file%seq%active = .true.
                        flds%iun = flds%iun + 1
                        file%seq%iun = flds%iun
                        file%seq%path = trim(file_path) // '.seq'
                    case ('txt')
                        file%txt%active = .true.
                        file%delim = ''
                        file%txt%path = trim(file_path) // '.txt'
                    case ('csv')
                        file%txt%active = .true.
                        file%delim = ','
                        file%txt%path = trim(file_path) // '.csv'

                    !> Order of the selection being saved.
                    case ('gridorder', 'shedorder')
                        file%order = opts

                    !> Print leading date-stamp.
                    case ('printdate')
                        file%print_date = .true.

                    !> Specialized output (grid, tile, and GRU point output).
                    case('tsi')
                        file%txt%active = .true.
                        file%order = opts
                        call output_file_parse_indices(shd, file, args, nargs, file%grids, i, ierr)
                        file%txt%path = trim(file_path) // '.ts'
                    case('tsk')
                        file%txt%active = .true.
                        file%order = opts
                        call output_file_parse_indices(shd, file, args, nargs, file%tiles, i, ierr)
                    case('gru')
                        file%txt%active = .true.
                        file%order = opts
                        call output_file_parse_indices(shd, file, args, nargs, file%grus, i, ierr)

                    !> Function.
                    case ('cum', 'acc', 'avg', 'max', 'min')
                        cycle

                    !> TTOL for ZOD.
                    case ('ttol')
                        n = 0
                        do j = i + 1, nargs
                            if (is_letter(args(j))) exit
                            n = n + 1
                        end do
                        if (n > 0) then
                            if (allocated(ZOD_TTOL)) deallocate(ZOD_TTOL)
                            allocate(ZOD_TTOL(n))
                            do j = 1, n
                                call value(args(i + j), ZOD_TTOL(j), ierr)
                            end do
                        end if

                    case default
                        if (ro%DIAGNOSEMODE > 0) print 1010, trim(args(i)), trim(args(1))
                end select
            end if
        end do

        !> Update options for output in 'txt' format.
        if (file%txt%active) then
            flds%iun = flds%iun + 1
            file%txt%iun = flds%iun
        end if

        !> Allocate output variable.
        if (.not. allocated(file%dat)) then
            if (flds%in_mem) then
                allocate(file%dat(shd%NA, t))
            else
                allocate(file%dat(shd%NA, 1))
            end if
            file%dat = 0.0
        end if

1010    format(3x, 'REMARK: ', (a), ' (Variable ', (a), ') is an unrecognized argument for output.')

    end subroutine

    subroutine output_file_parse_freq(fls, shd, ts, field, args, nargs, igndx)

        !> Required for 'is_letter' and 'lowercase' functions.
        use strings
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts
        integer, intent(in) :: nargs, igndx
        character(len = *), intent(in) :: args(:)

        !> Input/output variables.
        type(output_field) field

        !> Local variables.
        integer i
        character(len = 500) file_path
        character(len = 10) str

        !> Generic beginning of file path.
        file_path = trim(adjustl(fls%GENDIR_OUT)) // '/' // trim(adjustl(args(1)))

        !> Check for optional 'igndx' variable.
        if (igndx > 0) then
            write(str, '(i10)') igndx
            str = '_' // trim(adjustl(str))
        else
            str = ''
        end if

        !> Activate output based on 'args' flags.
        do i = 2, nargs
            if (is_letter(args(i)))then
                select case (lowercase(args(i)))

                    !> Yearly.
                    case ('y')
                        call output_file_parse_options( &
                            shd, field%y, trim(adjustl(file_path)) // '_Y' // adjustl(str), &
                            ts%nyears, args, nargs)

                    !> Monthly.
                    case ('m')
                        call output_file_parse_options( &
                            shd, field%m, trim(adjustl(file_path)) // '_M' // adjustl(str), &
                            ts%nmonths, args, nargs)

                    !> Seasonal (monthly average).
                    case ('s')
                        call output_file_parse_options( &
                            shd, field%s, trim(adjustl(file_path)) // '_S' // adjustl(str), &
                            12, args, nargs)

                    !> Daily.
                    case ('d')
                        call output_file_parse_options( &
                            shd, field%d, trim(adjustl(file_path)) // '_D' // adjustl(str), &
                            ts%nr_days, args, nargs)

                    !> Hourly.
                    case ('h')
                        call output_file_parse_options( &
                            shd, field%h, trim(adjustl(file_path)) // '_H' // adjustl(str), &
                            ts%nr_days*24, args, nargs)

                    !> Per time-step.
                    case ('ts')
                        call output_file_parse_options( &
                            shd, field%ts, trim(adjustl(file_path)) // '_TS' // adjustl(str), &
                            ts%nr_days*24*(3600/ic%dts), args, nargs)

                    !> Option to apply fractional cell area.
                    case ('frac', 'apply_frac')
                        field%apply_frac = .true.
                end select
            end if
        end do

    end subroutine

    subroutine output_field_init(fls, shd, ts, field, out_var, args, nargs, igndx)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts
        integer, intent(in) :: nargs
        character(len = *), intent(in) :: args(:)

        !> Input variables (optional).
        integer, intent(in), optional :: igndx

        !> Input/output variables.
        type(output_field) field
        type(output_variables_field) out_var

        !> Local variables.
        integer j

        !> Layer for file path (optional).
        if (present(igndx)) then
            j = igndx
        else
            j = out%NO_DATA
        end if

        !> Parse arguments and initialize field.
        call output_file_parse_freq(fls, shd, ts, field, args, nargs, j)

        !> Activate output variables for output at specified time intervals.
        if (field%y%active) call output_variables_allocate(out_var%y, shd%NA)
        if (field%m%active) call output_variables_allocate(out_var%m, shd%NA)
        if (field%d%active) call output_variables_allocate(out_var%d, shd%NA)
        if (field%h%active) call output_variables_allocate(out_var%h, shd%NA)

    end subroutine

    subroutine init_out(fls, shd, ts)

        !> 'control_variables' required for 'ro' options.
        !> 'strings' required for 'is_letter' function.
        use control_variables
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts

        !> Local variables.
        integer iun, n, j, nargs, ierr
        character(len = 20) args(50)
        character(len = 1000) line

        !> Allocate output variable for time-stamps based on switch to store variables in-memory.
        if (flds%in_mem) then
            allocate(flds%dates%y(6, ts%nyears))
            allocate(flds%dates%m(6, ts%nmonths))
            allocate(flds%dates%s(6, 12))
            allocate(flds%dates%d(6, ts%nr_days))
            allocate(flds%dates%h(6, ts%nr_days*24))
        else
            allocate(flds%dates%y(6, 1))
            allocate(flds%dates%m(6, 1))
            allocate(flds%dates%s(6, 1))
            allocate(flds%dates%d(6, 1))
            allocate(flds%dates%h(6, 1))
        end if
        allocate(flds%dates%ts(6))

        !> Open output fields configuration file.
        iun = 100
        call open_txt_input(iun, 'outputs_balance.txt', .true., ierr)

        !> Allocate and initialize output fields.
        n = 0
        do while (ierr == 0)

            !> Read the line.
            call read_txt_line(iun, line, ierr)

            !> Check for output field signature: character string followed by at least one space.
            if (len_trim(line) == 0 .or. scan(line, ' ') == 0) cycle
            if (.not. is_letter(line(1:index(line, ' ') - 1))) cycle

            !> Split the line.
            call parse(line, ' ', args, nargs)

            !> Output field.
            if (ro%DIAGNOSEMODE > 0) print 1000, trim(args(1))
            select case (args(1))

                !> Meteorological forcing.
                case ('FSIN', 'FSDOWN')
                    if (ro%RUNCLIM) call output_field_init(fls, shd, ts, flds%fsin, out%grid%fsin, args, nargs)
                case ('FSVH')
                    if (ro%RUNCLIM) call output_field_init(fls, shd, ts, flds%fsvh, out%grid%fsvh, args, nargs)
                case ('FSIH')
                    if (ro%RUNCLIM) call output_field_init(fls, shd, ts, flds%fsih, out%grid%fsih, args, nargs)
                case ('FLIN', 'FDL')
                    if (ro%RUNCLIM) call output_field_init(fls, shd, ts, flds%flin, out%grid%flin, args, nargs)
                case ('UV', 'UL')
                    if (ro%RUNCLIM) call output_field_init(fls, shd, ts, flds%uv, out%grid%uv, args, nargs)
                case ('TA')
                    if (ro%RUNCLIM) call output_field_init(fls, shd, ts, flds%ta, out%grid%ta, args, nargs)
                case ('QA', 'HU')
                    if (ro%RUNCLIM) call output_field_init(fls, shd, ts, flds%qa, out%grid%qa, args, nargs)
                case ('PRES')
                    if (ro%RUNCLIM) call output_field_init(fls, shd, ts, flds%pres, out%grid%pres, args, nargs)
                case ('PRE')
                    if (ro%RUNCLIM) call output_field_init(fls, shd, ts, flds%pre, out%grid%pre, args, nargs)

                !> Water balance.
                case ('PREC', 'Rainfall', 'Rain', 'Precipitation')
                    if (ro%RUNBALWB) call output_field_init(fls, shd, ts, flds%prec, out%grid%pre, args, nargs)
                case ('EVAP', 'Evapotranspiration')
                    if (ro%RUNBALWB) call output_field_init(fls, shd, ts, flds%evap, out%grid%evap, args, nargs)
                case ('ROF', 'Runoff')
                    if (ro%RUNBALWB) call output_field_init(fls, shd, ts, flds%rof, out%grid%rof, args, nargs)
                case ('RCAN')
                    if (ro%RUNBALWB) call output_field_init(fls, shd, ts, flds%rcan, out%grid%rcan, args, nargs)
                case ('SNCAN', 'SCAN')
                    if (ro%RUNBALWB) call output_field_init(fls, shd, ts, flds%sncan, out%grid%sncan, args, nargs)
                case ('PNDW')
                    if (ro%RUNBALWB) call output_field_init(fls, shd, ts, flds%pndw, out%grid%pndw, args, nargs)
                case ('SNO')
                    if (ro%RUNBALWB) call output_field_init(fls, shd, ts, flds%sno, out%grid%sno, args, nargs)
                case ('WSNO')
                    if (ro%RUNBALWB) call output_field_init(fls, shd, ts, flds%wsno, out%grid%wsno, args, nargs)
                case ('STGW', 'STG', 'DSTG')
                    if (ro%RUNBALWB) call output_field_init(fls, shd, ts, flds%stgw, out%grid%stgw, args, nargs)
                case ('THLQ')
                    if (ro%RUNBALWB) then
                        if (.not. allocated(flds%thlq)) allocate(flds%thlq(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            call output_field_init(fls, shd, ts, flds%thlq(j), out%grid%thlq(j), args, nargs, j)
                        end do
                    end if
                case ('LQWS')
                    if (ro%RUNBALWB) then
                        if (.not. allocated(flds%lqws)) allocate(flds%lqws(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            call output_field_init(fls, shd, ts, flds%lqws(j), out%grid%lqws(j), args, nargs, j)
                        end do
                    end if
                case ('THIC')
                    if (ro%RUNBALWB) then
                        if (.not. allocated(flds%thic)) allocate(flds%thic(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            call output_field_init(fls, shd, ts, flds%thic(j), out%grid%thic(j), args, nargs, j)
                        end do
                    end if
                case ('FZWS', 'FRWS')
                    if (ro%RUNBALWB) then
                        if (.not. allocated(flds%fzws)) allocate(flds%fzws(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            call output_field_init(fls, shd, ts, flds%fzws(j), out%grid%fzws(j), args, nargs, j)
                        end do
                    end if

                !> Energy balance.
                case ('QH', 'HFS', 'SensibleHeat')
                    if (ro%RUNBALEB) call output_field_init(fls, shd, ts, flds%qh, out%grid%qh, args, nargs)
                case ('QE', 'QEVP', 'LatentHeat')
                    if (ro%RUNBALEB) call output_field_init(fls, shd, ts, flds%qe, out%grid%qe, args, nargs)
                case ('GFLX', 'HeatConduction')
                    if (ro%RUNBALEB) then
                        if (.not. allocated(flds%gflx)) allocate(flds%gflx(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            call output_field_init(fls, shd, ts, flds%gflx(j), out%grid%gflx(j), args, nargs, j)
                        end do
                    end if
                case ('TBAR', 'TempSoil', 'Temperature_soil_layers')
                    if (ro%RUNBALEB) then
                        if (.not. allocated(flds%tbar)) allocate(flds%tbar(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            call output_field_init(fls, shd, ts, flds%tbar(j), out%grid%tbar(j), args, nargs, j)
                        end do
                    end if

                case ('ZOD', 'TMAX', 'TMIN', 'ALD')
!                    call output_field_init(fls, shd, ts, flds%ald, out%grid%ald, args, nargs)
!                    flds%ald%m%active = .false.
!                    flds%ald%s%active = .false.
!                    flds%ald%d%active = .false.
!                    flds%ald%h%active = .false.
!                    flds%ald%ts%active = .false.
!                    call output_field_allocate(shd, ts, flds%ald)
!                    if (flds%ald%y%active) then
!                        call output_variables_allocate(out%grid%tbar, shd%NA)
!                        if (.not. allocated(TBAR_dly)) allocate(TBAR_dly(nts_d, shd%NA, shd%lc%IGND))
!                        if (.not. allocated(ALD_dly)) allocate(ALD_dly(nts_d, shd%NA))
!                        if (.not. allocated(TMAX_ann)) allocate(TMAX_ann(nts_y, shd%NA, shd%lc%IGND))
!                        if (.not. allocated(TMIN_ann)) allocate(TMIN_ann(nts_y, shd%NA, shd%lc%IGND))
!                        if (.not. allocated(ALD_ann)) allocate(ALD_ann(nts_y, shd%NA))
!                        if (.not. allocated(ZOD_TTOL)) then
!                            allocate(ZOD_TTOL(1)); ZOD_TTOL = 0.0
!                        end if
!                        if (.not. allocated(ZOD_ann)) allocate(ZOD_ann(nts_y, shd%NA, size(ZOD_TTOL)))
!                        TBAR_dly = 0.0; ALD_dly = -1.0; TMAX_ann = 0.0; TMIN_ann = 1000.0; ALD_ann = -1.0; ZOD_ann = -1.0
!                    end if

                !> Channels and routing.
                case ('RFF', 'WR_RUNOFF')
                    if (ro%RUNCHNL) call output_field_init(fls, shd, ts, flds%rff, out%grid%rff, args, nargs)
                case ('RCHG', 'WR_RECHARGE')
                    if (ro%RUNCHNL) call output_field_init(fls, shd, ts, flds%rchg, out%grid%rchg, args, nargs)

                case default
                    n = n - 1
                    print 1010, trim(args(1))
            end select
            n = n + 1
        end do
        close(iun)
        print 1110, n

1000    format(3x, 'Reading output variable: ', (a))
1010    format(3x, 'REMARK: Variable ', (a), ' is not recognized for output.')
1110    format(3x, 'Output variables: ', i3)

    end subroutine

    subroutine UpdateFIELDSOUT(fls, shd)

        use permafrost_active_layer

        type(fl_ids) fls
        type(ShedGridParams) shd

        call update_output_variables(fls, shd)

!        if (flds%ald%y%active .and. ic%now%day /= ic%next%day) then
!            TBAR_dly(ic%iter%day, : , :) = out%grid%tbar
!            call active_layer_depth( &
!                TBAR_dly(ic%iter%day, :, :), shd%lc%sl%ZBOT, &
!                ALD_dly(ic%iter%day, :), &
!                shd%lc%IGND, shd%NA, 1, shd%NA)
!            ALD_ann(ic%iter%year, :) = max(ALD_ann(ic%iter%year, :), ALD_dly(id, :))
!            TMAX_ann(ic%iter%year, :, :) = max(TMAX_ann(ic%iter%year, :, :), TBAR_dly(ic%iter%day, :, :))
!            TMIN_ann(ic%iter%year, :, :) = min(TMIN_ann(ic%iter%year, :, :), TBAR_dly(ic%iter%day, :, :))
!        end if

    end subroutine

    subroutine Write_Outputs(fls, shd)

        use permafrost_active_layer

        type(fl_ids) fls
        type(ShedGridParams) shd

        integer k, j

        if (flds%ald%y%active .and. ic%now%year /= ic%next%year) then

            !> ALD.
!            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

            !> ZOD.
!            do j = 1, size(ZOD_TTOL)
!                do k = 1, ts%nyears
!                    call zero_oscillation_depth( &
!                        TMAX_ann(k, :, :), TMIN_ann(k, :, :), shd%lc%sl%ZBOT, ZOD_TTOL(j), &
!                        ZOD_ann(k, :, j), &
!                        shd%lc%IGND, shd%NA, 1, shd%NA)
!                end do
!                call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
!            end do

            !> TMAX.
!            do j = 1, shd%lc%IGND
!                call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
!            end do

            !> TMIN.
!            do j = 1, shd%lc%IGND
!                call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
!            end do
        end if

    end subroutine

    subroutine flush_output(fls, shd, file, field_name, freq, dates, igndx)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_file), intent(in) :: file
        character(len = *), intent(in) :: field_name, freq
        integer, intent(in) :: dates(:, :)

        !> Input variables (optional).
        integer, intent(in), optional :: igndx

        !> Local variables.
        character(len = 25) str

        !> Append 'igndx' if provided.
        if (present(igndx)) then
            write(str, '(i10)') igndx
            str = trim(adjustl(field_name)) // '_' // trim(adjustl(freq)) // '_' // trim(adjustl(str))
        else
            str = trim(adjustl(field_name)) // '_' // trim(adjustl(freq))
        end if

        !> Write output.
        if (file%seq%active) call write_seq(fls, shd, file, str, dates, file%seq)
        if (file%r2c%active) call write_r2c(fls, shd, file, str, dates, file%r2c)
        if (file%txt%active) call write_txt(fls, shd, file, str, dates, file%txt)

    end subroutine

    !> Description:
    !>  Update the output field from the 'out_var' variable.
    subroutine update_output_variable(file, out_var, t, its, dnts, fn)

        !> Input variables.
        real, dimension(:), intent(in) :: out_var
        integer, intent(in) :: t, its, dnts
        character(len = *), intent(in) :: fn

        !> Input/output variables.
        type(output_file) file

        !> Update value.
        call output_variables_update_values(file%dat(:, t), out_var, its, dnts, fn)

    end subroutine

    subroutine update_output_field(fls, shd, field, out_var, field_name, cfactorm, cfactora, igndx)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_variables_field), intent(in) :: out_var
        character(len = *), intent(in) :: field_name

        !> Input variables (optional).
        integer, intent(in), optional :: igndx
        real, intent(in), optional :: cfactorm, cfactora

        !> Input/output variables.
        type(output_field) field

        !> Local variables.
        integer t
        real frac(shd%NA), m, a

        !> Optional factors.
        if (present(cfactorm)) then
            m = cfactorm
        else
            m = 1.0
        end if
        if (present(cfactora)) then
            a = cfactora
        else
            a = 0.0
        end if

        !> Set 't = 1' if not storing values in memory.
        t = 1

        !> Set 'frac' to 'shd%FRAC' if multiplying 'field' by fractional cell area.
        if (field%apply_frac) then
            frac = shd%FRAC
        else
            frac = 1.0
        end if

        !> Yearly.
        if (field%y%active .and. ic%now%year /= ic%next%year) then
            if (flds%in_mem) t = ic%iter%year
            call update_output_variable(field%y, (m*out_var%y + a)*frac, t, ic%ts_yearly, 0, 'val')
            if (.not. flds%in_mem) then
                if (present(igndx)) then
                    call flush_output(fls, shd, field%y, field_name, 'Y', flds%dates%y, igndx)
                else
                    call flush_output(fls, shd, field%y, field_name, 'Y', flds%dates%y)
                end if
            end if
        end if

        !> Monthly.
        if (field%m%active .and. ic%now%month /= ic%next%month) then
            if (flds%in_mem) t = ic%iter%month
            call update_output_variable(field%m, (m*out_var%m + a)*frac, t, ic%ts_monthly, 0, 'val')
            if (.not. flds%in_mem) then
                if (present(igndx)) then
                    call flush_output(fls, shd, field%m, field_name, 'M', flds%dates%m, igndx)
                else
                    call flush_output(fls, shd, field%m, field_name, 'M', flds%dates%m)
                end if
            end if
        end if

        !> Daily.
        if (field%d%active .and. ic%now%day /= ic%next%day) then
            if (flds%in_mem) t = ic%iter%day
            call update_output_variable(field%d, (m*out_var%d + a)*frac, t, ic%ts_daily, 0, 'val')
            if (.not. flds%in_mem) then
                if (present(igndx)) then
                    call flush_output(fls, shd, field%d, field_name, 'D', flds%dates%d, igndx)
                else
                    call flush_output(fls, shd, field%d, field_name, 'D', flds%dates%d)
                end if
            end if
        end if

        !> Seasonal.
        if (field%h%active .and. ic%now%hour /= ic%next%hour) then
            if (flds%in_mem) t = ic%iter%hour
            call update_output_variable(field%h, (m*out_var%h + a)*frac, t, ic%ts_hourly, 0, 'val')
            if (.not. flds%in_mem) then
                if (present(igndx)) then
                    call flush_output(fls, shd, field%h, field_name, 'H', flds%dates%h, igndx)
                else
                    call flush_output(fls, shd, field%h, field_name, 'H', flds%dates%h)
                end if
            end if
        end if

    end subroutine

    !> Description:
    !>  Update the 'dates' variable from the 'ic' counter.
    subroutine update_output_dates(dates, iter, t)

        !> Input variables.
        integer, intent(in) :: iter, t

        !> Input/output variables.
        integer dates(:, :)

        !> Save the time-step using 'next' date.
        dates(1, t) = iter
        dates(2, t) = ic%next%year
        dates(3, t) = ic%next%month
        dates(4, t) = ic%next%day
        dates(5, t) = ic%next%hour
        dates(6, t) = ic%next%mins

        !> Check 'hour' for the 24th hour.
        !> Output is written 01-24h while the model runs 00-23h.
        if (dates(5, t) == 0) dates(5, t) = 24

    end subroutine

    subroutine update_output_variables(fls, shd)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer t, j

        !> Update time-steps.
        !> Set 't = 1' if not storing values in memory.
        t = 1
        if (ic%now%year /= ic%next%year) then
            if (flds%in_mem) t = ic%iter%year
            call update_output_dates(flds%dates%y, ic%iter%year, t)
        end if
        if (ic%now%month /= ic%next%month) then
            if (flds%in_mem) t = ic%iter%month
            call update_output_dates(flds%dates%m, ic%iter%month, t)
        end if
        if (ic%now%day /= ic%next%day) then
            if (flds%in_mem) t = ic%iter%day
            call update_output_dates(flds%dates%d, ic%iter%day, t)
        end if
        if (ic%now%hour /= ic%next%hour) then
            if (flds%in_mem) t = ic%iter%hour
            call update_output_dates(flds%dates%h, ic%iter%hour, t)
        end if

        !> Update variables.
        call update_output_field(fls, shd, flds%fsin, out%grid%fsin, 'FSDOWN')
        call update_output_field(fls, shd, flds%fsvh, out%grid%fsin, 'FSVH', 0.5)
        call update_output_field(fls, shd, flds%fsih, out%grid%fsin, 'FSIH', 0.5)
        call update_output_field(fls, shd, flds%flin, out%grid%flin, 'FDL')
        call update_output_field(fls, shd, flds%uv, out%grid%uv, 'UL')
        call update_output_field(fls, shd, flds%ta, out%grid%ta, 'TA')
        call update_output_field(fls, shd, flds%qa, out%grid%qa, 'QA')
        call update_output_field(fls, shd, flds%pres, out%grid%pres, 'PRES')
        call update_output_field(fls, shd, flds%pre, out%grid%pre, 'PRE')
        call update_output_field(fls, shd, flds%prec, out%grid%pre, 'PREC', real(ic%dts))
        call update_output_field(fls, shd, flds%evap, out%grid%evap, 'EVAP', real(ic%dts))
        call update_output_field(fls, shd, flds%rof, out%grid%rof, 'ROF', real(ic%dts))
        call update_output_field(fls, shd, flds%rcan, out%grid%rcan, 'RCAN')
        call update_output_field(fls, shd, flds%sncan, out%grid%sncan, 'SNCAN')
        call update_output_field(fls, shd, flds%pndw, out%grid%pndw, 'PNDW')
        call update_output_field(fls, shd, flds%sno, out%grid%sno, 'SNO')
        call update_output_field(fls, shd, flds%wsno, out%grid%wsno, 'WSNO')
        call update_output_field(fls, shd, flds%stgw, out%grid%stgw, 'STG')
        call update_output_field(fls, shd, flds%qh, out%grid%qh, 'HFS')
        call update_output_field(fls, shd, flds%qe, out%grid%qe, 'QEVP')
        call update_output_field(fls, shd, flds%rff, out%grid%rff,  'WR_RUNOFF', real(ic%dts))
        call update_output_field(fls, shd, flds%rchg, out%grid%rchg, 'WR_RECHARGE', real(ic%dts))

        !> Variables with multiple layers.
        !> Must check if the multi-layer variable is allocated.
        if (allocated(flds%thlq)) then
            do j = 1, shd%lc%IGND
                call update_output_field(fls, shd, flds%thlq(j), out%grid%thlq(j), 'THLQ', 1.0, 0.0, j)
            end do
        end if
        if (allocated(flds%lqws)) then
            do j = 1, shd%lc%IGND
                call update_output_field(fls, shd, flds%lqws(j), out%grid%lqws(j), 'LQWS', 1.0, 0.0, j)
            end do
        end if
        if (allocated(flds%thic)) then
            do j = 1, shd%lc%IGND
                call update_output_field(fls, shd, flds%thic(j), out%grid%thic(j), 'THIC', 1.0, 0.0, j)
            end do
        end if
        if (allocated(flds%fzws)) then
            do j = 1, shd%lc%IGND
                call update_output_field(fls, shd, flds%fzws(j), out%grid%fzws(j), 'FRWS', 1.0, 0.0, j)
            end do
        end if
        if (allocated(flds%gflx)) then
            do j = 1, shd%lc%IGND
                call update_output_field(fls, shd, flds%gflx(j), out%grid%gflx(j), 'GFLX', 1.0, 0.0, j)
            end do
        end if
        if (allocated(flds%tbar)) then
            do j = 1, shd%lc%IGND
                call update_output_field(fls, shd, flds%tbar(j), out%grid%tbar(j), 'TBAR', 1.0, 0.0, j)
            end do
        end if

        !> ZOD, TMAX, TMIN, ALD.

    end subroutine

    subroutine write_seq(fls, shd, file, field_name, dates, file_fmt)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_file), intent(in) :: file
        character(len = *), intent(in) :: field_name
        integer, intent(in) :: dates(:, :)
        type(output_file_format), intent(in) :: file_fmt

        !> Local variables.
        integer iun, t, ierr
        logical opened_status

        !> Check if the file is opened.
        iun = file_fmt%iun
        inquire(iun, opened = opened_status)

        !> Open the file (if not opened).
        if (.not. opened_status) then
            open( &
                iun, file = file_fmt%path, &
                status = 'replace', form = 'unformatted', action = 'write', access = 'sequential', &
                iostat = ierr)
        end if

        !> Write series.
        do t = 1, size(file%dat, 2)

            !> Write frame number (N) and time-stamp.
            !> Extended format for 'seq': N N yyyy MM dd HH mm ss SSS
            write(iun) dates(1, t), dates(1, t), dates(2, t), dates(3, t), dates(4, t), dates(5, t), dates(6, t)

            !> Write data.
            write(iun) file%dat(:, t)
        end do

    end subroutine

    subroutine write_r2c(fls, shd, file, field_name, dates, file_fmt)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_file), intent(in) :: file
        character(len = *), intent(in) :: field_name
        integer, intent(in) :: dates(:, :)
        type(output_file_format), intent(in) :: file_fmt

        !> Local variables.
        integer iun, t, j, i, ierr
        real, dimension(shd%yCount, shd%xCount) :: data_aux
        logical opened_status
        character(len = 10) str10
        character(len = 8) str8

        !> Check if the file is opened.
        iun = file_fmt%iun
        inquire(iun, opened = opened_status)

        !> Open the file (if not opened) and write 'r2c' header.
        if (.not. opened_status) then
            open( &
                iun, file = file_fmt%path, &
                status = 'replace', form = 'formatted', action = 'write', &
                iostat = ierr)
            write(iun, 3005) '########################################'
            write(iun, 3005) ':FileType r2c  ASCII  EnSim 1.0         '
            write(iun, 3005) '#                                       '
            write(iun, 3005) '# DataType               2D Rect Cell   '
            write(iun, 3005) '#                                       '
            write(iun, 3005) ':Application               MeshOutput   '
            write(iun, 3005) ':Version                 1.0.00         '
            write(iun, 3005) ':WrittenBy          MESH_DRIVER         '
            call date_and_time(str8, str10)
            write(iun, 3010) ':CreationDate       ', str8(1:4), str8(5:6), str8(7:8), str10(1:2), str10(3:4)
            write(iun, 3005) '#                                       '
            write(iun, 3005) '#---------------------------------------'
            write(iun, 3005) '#                                       '
            i = len_trim(field_name)
            if (index(field_name, '_') > 1) i = index(field_name, '_') - 1
            write(iun, 3002) ':Name               ', field_name(1:i)
            write(iun, 3005) '#                                       '
            write(iun, 3004) ':Projection         ', shd%CoordSys%Proj
            if (shd%CoordSys%Proj == 'LATLONG   ') &
                write(iun, 3004) ':Ellipsoid          ', shd%CoordSys%Ellips
            if (shd%CoordSys%Proj == 'UTM       ') then
                write(iun, 3004) ':Ellipsoid          ', shd%CoordSys%Ellips
                write(iun, 3004) ':Zone               ', shd%CoordSys%Zone
            end if
            write(iun, 3005) '#                                       '
            write(iun, 3003) ':xOrigin            ', shd%xOrigin
            write(iun, 3003) ':yOrigin            ', shd%yOrigin
            write(iun, 3005) '#                                       '
            write(iun, 3005) ':SourceFile            MESH_DRIVER      '
            write(iun, 3005) '#                                       '
            write(iun, 3002) ':AttributeName      ', field_name(1:i)
            write(iun, 3002) ':AttributeUnits     ', ''
            write(iun, 3005) '#                                       '
            write(iun, 3001) ':xCount             ', shd%xCount
            write(iun, 3001) ':yCount             ', shd%yCount
            write(iun, 3003) ':xDelta             ', shd%xDelta
            write(iun, 3003) ':yDelta             ', shd%yDelta
            write(iun, 3005) '#                                       '
            write(iun, 3005) '#                                       '
            write(iun, 3005) ':endHeader                              '
        end if

        !> Write series.
        do t = 1, size(file%dat, 2)

            !> Transfer data to temporary variable.
            data_aux = 0.0
            do i = 1, shd%NA
                data_aux(shd%yyy(i), shd%xxx(i)) = file%dat(i, t)
            end do

            !> Write frame number and time-stamp.
            !> Standard format for 'r2c': "yyyy/MM/dd HH:mm:ss.SSS"
            write(iun, 9000) dates(1, t), dates(1, t), dates(2, t), dates(3, t), dates(4, t), dates(5, t), dates(6, t)

            !> Write data.
            do j = 1, shd%yCount
                write(iun, 9001) (data_aux(j, i), i = 1, shd%xCount)
            end do

            !> Write end frame.
            write(iun, 9002)
        end do

3001    format((a), i8)
3002    format((a), 3x, (a))
3003    format((a), f16.7)
3004    format((a), 1x, (a))
3005    format((a))
3010    format(a20, a4, '-', a2, '-', a2, 2x, a2, ':', a2)
9000    format(':Frame', 2i10, 3x, """", i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ":00.000""")
9001    format(999(e12.6, ' '))
9002    format(':EndFrame')

    end subroutine

    subroutine write_txt(fls, shd, file, field_name, dates, file_fmt)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_file), intent(in) :: file
        character(len = *), intent(in) :: field_name
        integer, intent(in) :: dates(:, :)
        type(output_file_format), intent(in) :: file_fmt

        !> Local variables.
        integer iun, t, j, i, ierr
        real, dimension(shd%yCount, shd%xCount) :: data_aux
        logical opened_status
        character(len = 25) str

        !> Check if the file is opened.
        iun = file_fmt%iun
        inquire(iun, opened = opened_status)

        !> Open the file (if not opened).
        if (.not. opened_status) then
            open( &
                iun, file = file_fmt%path, &
                status = 'replace', form = 'formatted', action = 'write', &
                iostat = ierr)
        end if

        !> Assign the delimiter.
        select case (file%delim)
            case (',')
                write(str, '(a)') "(9999(e12.6, ','))"
            case default
                write(str, '(a)') "(9999(e12.6, ' '))"
        end select

        !> Write series.
        do t = 1, size(file%dat, 2)

            !> Lead line with date (optional).
            if (file%print_date) then
                write(iun, 9000, advance = 'no') dates(2, t), dates(3, t), dates(4, t), dates(5, t), dates(6, t)
            end if

            !> Determine order of cells for output.
            select case (file%order)

                !> Write for specific grids.
                case ('tsi')
                    write(iun, str) (file%dat(file%grids(i), t), i = 1, size(file%grids))

                !> Write by order of 'r2c' grid (e.g., by shd%yCount, then by shd%xCount in a single line).
                case ('shedorder')

                    !> Transfer data to temporary array.
                    data_aux = 0.0
                    do i = 1, shd%NA
                        data_aux(shd%yyy(i), shd%xxx(i)) = file%dat(i, t)
                    end do

                    !> Write data (in a single line).
                    do j = 1, shd%yCount
                        write(iun, str, advance = 'no') (data_aux(j, i), i = 1, shd%xCount)
                    end do
                    write(iun, *)

                !> Write by order of RANK (e.g., 1:shd%NA).
                case default
                    write(iun, str) file%dat(:, t)
            end select
        end do

9000    format("""", i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ":00.000""", 2x)

    end subroutine

end module
