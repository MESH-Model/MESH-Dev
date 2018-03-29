module model_output

    use sa_mesh_variables
    use sa_mesh_utilities
    use model_dates
    use model_files_variables
    use variablename_constants

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
    !*  name: Variable name (default: none).
    !*  apply_frac: .true. to multiply grid values by fractional cell area (default: .false.).
    !*  y, m, s, d, h, ts: Output files of the variable at various time intervals.
    type output_field
        character(len = 10) :: name = ''
        logical :: apply_frac = .false.
        type(output_file) y, m, s, d, h, ts
    end type

    !> Description:
    !>  Data type for storing series of dates.
    !>
    !> Variables:
    !*  y, m, s, d, h, ts: Arrays of dates at various time intervals.
    !*  iter_s: Iteration for seasonal counter (from beginning of run instead of calendar year).
    type output_dates
        integer, dimension(:, :), allocatable :: y, m, s, d, h
        integer, dimension(:), allocatable :: ts
        integer iter_s(12)
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
            rcan, sncan, sno, fsno, wsno, zpnd, pndw, lzs, dzs, stgw, dstgw, &
            cmas, tcan, tsno, tpnd, &
            alvs, alir, albt, fsout, flout, gte, qh, qe, gzero, stge, dstge, &
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
        open(iun, file = adjustl(fname), action = 'read', status = 'old', iostat = ierr)

        !> Return if the file successfully opened.
        return

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
                        call print_remark( &
                            "'" // trim(adjustl(args(i))) // "' (Variable '" // trim(adjustl(args(1))) // &
                            "') is an unrecognized option for output.", PAD_3)
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
            allocate(file%dat(shd%NA, t))
            file%dat = 0.0
        end if

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
        integer t, i
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

                !> Set 't = 1' if not storing values in memory.
                t = 1

                !> Parse options.
                select case (lowercase(args(i)))

                    !> Yearly.
                    case ('y')
                        if (flds%in_mem) t = ts%nyears
                        call output_file_parse_options( &
                            shd, field%y, trim(adjustl(file_path)) // '_Y' // adjustl(str), &
                            t, args, nargs)

                    !> Monthly.
                    case ('m')
                        if (flds%in_mem) t = ts%nmonths
                        call output_file_parse_options( &
                            shd, field%m, trim(adjustl(file_path)) // '_M' // adjustl(str), &
                            t, args, nargs)

                    !> Daily.
                    case ('d')
                        if (flds%in_mem) t = ts%nr_days
                        call output_file_parse_options( &
                            shd, field%d, trim(adjustl(file_path)) // '_D' // adjustl(str), &
                            t, args, nargs)

                    !> Hourly.
                    case ('h')
                        if (flds%in_mem) t = ts%nr_days*24
                        call output_file_parse_options( &
                            shd, field%h, trim(adjustl(file_path)) // '_H' // adjustl(str), &
                            t, args, nargs)

                    !> Per time-step.
                    case ('ts')
                        if (flds%in_mem) t = ts%nr_days*24*(3600/ic%dts)
                        call output_file_parse_options( &
                            shd, field%ts, trim(adjustl(file_path)) // '_TS' // adjustl(str), &
                            t, args, nargs)

                    !> Seasonally (monthly average).
                    case ('s')
                        call output_file_parse_options( &
                            shd, field%s, trim(adjustl(file_path)) // '_S' // adjustl(str), &
                            12, args, nargs)

                    !> Option to apply fractional cell area.
                    case ('frac', 'apply_frac')
                        field%apply_frac = .true.
                end select
            end if
        end do

    end subroutine

    subroutine output_field_init(fls, shd, ts, field, args, nargs, igndx)

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
!        if (field%y%active) call output_variables_allocate(out_var%y, shd%NA)
!        if (field%m%active) call output_variables_allocate(out_var%m, shd%NA)
!        if (field%s%active) call output_variables_allocate(out_var%m, shd%NA)
!        if (field%d%active) call output_variables_allocate(out_var%d, shd%NA)
!        if (field%h%active) call output_variables_allocate(out_var%h, shd%NA)

    end subroutine

    subroutine init_out(fls, shd)

        !> 'strings' required for 'is_letter' function.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        type(dates_model) ts
        integer iun, n, j, nargs, ierr
        character(len = DEFAULT_FIELD_LENGTH) args(50)
        character(len = DEFAULT_LINE_LENGTH) line

        !> Initialize 'ts' variable.
        call GET_DATES(ts)

        !> Allocate output variable for time-stamps based on switch to store variables in-memory.
        if (flds%in_mem) then
            allocate(flds%dates%y(6, ts%nyears))
            allocate(flds%dates%m(6, ts%nmonths))
            allocate(flds%dates%d(6, ts%nr_days))
            allocate(flds%dates%h(6, ts%nr_days*24))
        else
            allocate(flds%dates%y(6, 1))
            allocate(flds%dates%m(6, 1))
            allocate(flds%dates%d(6, 1))
            allocate(flds%dates%h(6, 1))
        end if
        allocate(flds%dates%ts(6))
        allocate(flds%dates%s(6, 12))
        flds%dates%iter_s = 0

        !> Open output fields configuration file.
        call print_screen('READING: outputs_balance.txt')
        call print_echo_txt('outputs_balance.txt')
        iun = 100
        call open_txt_input(iun, 'outputs_balance.txt', .true., ierr)

        !> Stop if the routine failed.
        if (ierr /= 0) then
            call print_error('Unable to open file.')
            call print_message('Check that outputs_balance.txt exists or disabled OUTFIELDSFLAG.')
            call stop_program()
        end if

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
            if (DIAGNOSEMODE) call print_message_detail('Reading output variable: ' // args(1))
            select case (args(1))

                !> Meteorological forcing.
                case (VN_FSIN, 'FSDOWN')
                    if (ro%RUNCLIM) then
                        flds%fsin%name = VN_FSIN
                        call output_field_init(fls, shd, ts, flds%fsin, args, nargs)
                        if (flds%fsin%y%active) call output_variables_allocate(out%y%grid%fsin, shd%NA)
                        if (flds%fsin%m%active .or. flds%fsin%s%active) call output_variables_allocate(out%m%grid%fsin, shd%NA)
                        if (flds%fsin%d%active) call output_variables_allocate(out%d%grid%fsin, shd%NA)
                        if (flds%fsin%h%active) call output_variables_allocate(out%h%grid%fsin, shd%NA)
                    end if
                case (VN_FSVH)
                    if (ro%RUNCLIM) then
                        flds%fsvh%name = VN_FSVH
                        call output_field_init(fls, shd, ts, flds%fsvh, args, nargs)
                        if (flds%fsvh%y%active) call output_variables_allocate(out%y%grid%fsvh, shd%NA)
                        if (flds%fsvh%m%active .or. flds%fsvh%s%active) call output_variables_allocate(out%m%grid%fsvh, shd%NA)
                        if (flds%fsvh%d%active) call output_variables_allocate(out%d%grid%fsvh, shd%NA)
                        if (flds%fsvh%h%active) call output_variables_allocate(out%h%grid%fsvh, shd%NA)
                    end if
                case (VN_FSIH)
                    if (ro%RUNCLIM) then
                        flds%fsih%name = VN_FSIH
                        call output_field_init(fls, shd, ts, flds%fsih, args, nargs)
                        if (flds%fsih%y%active) call output_variables_allocate(out%y%grid%fsih, shd%NA)
                        if (flds%fsih%m%active .or. flds%fsih%s%active) call output_variables_allocate(out%m%grid%fsih, shd%NA)
                        if (flds%fsih%d%active) call output_variables_allocate(out%d%grid%fsih, shd%NA)
                        if (flds%fsih%h%active) call output_variables_allocate(out%h%grid%fsih, shd%NA)
                    end if
                case (VN_FLIN, 'FDL')
                    if (ro%RUNCLIM) then
                        flds%flin%name = VN_FLIN
                        call output_field_init(fls, shd, ts, flds%flin, args, nargs)
                        if (flds%flin%y%active) call output_variables_allocate(out%y%grid%flin, shd%NA)
                        if (flds%flin%m%active .or. flds%flin%s%active) call output_variables_allocate(out%m%grid%flin, shd%NA)
                        if (flds%flin%d%active) call output_variables_allocate(out%d%grid%flin, shd%NA)
                        if (flds%flin%h%active) call output_variables_allocate(out%h%grid%flin, shd%NA)
                    end if
                case (VN_UV, 'UL')
                    if (ro%RUNCLIM) then
                        flds%uv%name = VN_UV
                        call output_field_init(fls, shd, ts, flds%uv, args, nargs)
                        if (flds%uv%y%active) call output_variables_allocate(out%y%grid%uv, shd%NA)
                        if (flds%uv%m%active .or. flds%uv%s%active) call output_variables_allocate(out%m%grid%uv, shd%NA)
                        if (flds%uv%d%active) call output_variables_allocate(out%d%grid%uv, shd%NA)
                        if (flds%uv%h%active) call output_variables_allocate(out%h%grid%uv, shd%NA)
                    end if
                case (VN_TA)
                    if (ro%RUNCLIM) then
                        flds%ta%name = VN_TA
                        call output_field_init(fls, shd, ts, flds%ta, args, nargs)
                        if (flds%ta%y%active) call output_variables_allocate(out%y%grid%ta, shd%NA)
                        if (flds%ta%m%active .or. flds%ta%s%active) call output_variables_allocate(out%m%grid%ta, shd%NA)
                        if (flds%ta%d%active) call output_variables_allocate(out%d%grid%ta, shd%NA)
                        if (flds%ta%h%active) call output_variables_allocate(out%h%grid%ta, shd%NA)
                    end if
                case (VN_QA, 'HU')
                    if (ro%RUNCLIM) then
                        flds%qa%name = VN_QA
                        call output_field_init(fls, shd, ts, flds%qa, args, nargs)
                        if (flds%qa%y%active) call output_variables_allocate(out%y%grid%qa, shd%NA)
                        if (flds%qa%m%active .or. flds%qa%s%active) call output_variables_allocate(out%m%grid%qa, shd%NA)
                        if (flds%qa%d%active) call output_variables_allocate(out%d%grid%qa, shd%NA)
                        if (flds%qa%h%active) call output_variables_allocate(out%h%grid%qa, shd%NA)
                    end if
                case (VN_PRES)
                    if (ro%RUNCLIM) then
                        flds%pres%name = VN_PRES
                        call output_field_init(fls, shd, ts, flds%pres, args, nargs)
                        if (flds%pres%y%active) call output_variables_allocate(out%y%grid%pres, shd%NA)
                        if (flds%pres%m%active .or. flds%pres%s%active) call output_variables_allocate(out%m%grid%pres, shd%NA)
                        if (flds%pres%d%active) call output_variables_allocate(out%d%grid%pres, shd%NA)
                        if (flds%pres%h%active) call output_variables_allocate(out%h%grid%pres, shd%NA)
                    end if
                case (VN_PRE)
                    if (ro%RUNCLIM) then
                        flds%pre%name = VN_PRE
                        call output_field_init(fls, shd, ts, flds%pre, args, nargs)
                        if (flds%pre%y%active) call output_variables_allocate(out%y%grid%pre, shd%NA)
                        if (flds%pre%m%active .or. flds%pre%s%active) call output_variables_allocate(out%m%grid%pre, shd%NA)
                        if (flds%pre%d%active) call output_variables_allocate(out%d%grid%pre, shd%NA)
                        if (flds%pre%h%active) call output_variables_allocate(out%h%grid%pre, shd%NA)
                    end if

                !> Water balance.
                case (VN_PREC, 'Rainfall', 'Rain', 'Precipitation')
                    if (ro%RUNBALWB) then
                        flds%prec%name = VN_PREC
                        call output_field_init(fls, shd, ts, flds%prec, args, nargs)
                        if (flds%prec%y%active) call output_variables_allocate(out%y%grid%prec, shd%NA)
                        if (flds%prec%m%active .or. flds%prec%s%active) call output_variables_allocate(out%m%grid%prec, shd%NA)
                        if (flds%prec%d%active) call output_variables_allocate(out%d%grid%prec, shd%NA)
                        if (flds%prec%h%active) call output_variables_allocate(out%h%grid%prec, shd%NA)
                    end if
                case (VN_EVAP, 'Evapotranspiration')
                    if (ro%RUNBALWB) then
                        flds%evap%name = VN_EVAP
                        call output_field_init(fls, shd, ts, flds%evap, args, nargs)
                        if (flds%evap%y%active) call output_variables_allocate(out%y%grid%evap, shd%NA)
                        if (flds%evap%m%active .or. flds%evap%s%active) call output_variables_allocate(out%m%grid%evap, shd%NA)
                        if (flds%evap%d%active) call output_variables_allocate(out%d%grid%evap, shd%NA)
                        if (flds%evap%h%active) call output_variables_allocate(out%h%grid%evap, shd%NA)
                    end if
                case (VN_ROF, 'Runoff')
                    if (ro%RUNBALWB) then
                        flds%rof%name = VN_ROF
                        call output_field_init(fls, shd, ts, flds%rof, args, nargs)
                        if (flds%rof%y%active) call output_variables_allocate(out%y%grid%rof, shd%NA)
                        if (flds%rof%m%active .or. flds%rof%s%active) call output_variables_allocate(out%m%grid%rof, shd%NA)
                        if (flds%rof%d%active) call output_variables_allocate(out%d%grid%rof, shd%NA)
                        if (flds%rof%h%active) call output_variables_allocate(out%h%grid%rof, shd%NA)
                    end if
                case (VN_RCAN)
                    if (ro%RUNBALWB) then
                        flds%rcan%name = VN_RCAN
                        call output_field_init(fls, shd, ts, flds%rcan, args, nargs)
                        if (flds%rcan%y%active) call output_variables_allocate(out%y%grid%rcan, shd%NA)
                        if (flds%rcan%m%active .or. flds%rcan%s%active) call output_variables_allocate(out%m%grid%rcan, shd%NA)
                        if (flds%rcan%d%active) call output_variables_allocate(out%d%grid%rcan, shd%NA)
                        if (flds%rcan%h%active) call output_variables_allocate(out%h%grid%rcan, shd%NA)
                    end if
                case (VN_SNCAN, 'SCAN')
                    if (ro%RUNBALWB) then
                        flds%sncan%name = VN_SNCAN
                        call output_field_init(fls, shd, ts, flds%sncan, args, nargs)
                        if (flds%sncan%y%active) call output_variables_allocate(out%y%grid%sncan, shd%NA)
                        if (flds%sncan%m%active .or. flds%sncan%s%active) call output_variables_allocate(out%m%grid%sncan, shd%NA)
                        if (flds%sncan%d%active) call output_variables_allocate(out%d%grid%sncan, shd%NA)
                        if (flds%sncan%h%active) call output_variables_allocate(out%h%grid%sncan, shd%NA)
                    end if
                case (VN_PNDW)
                    if (ro%RUNBALWB) then
                        flds%pndw%name = VN_PNDW
                        call output_field_init(fls, shd, ts, flds%pndw, args, nargs)
                        if (flds%pndw%y%active) call output_variables_allocate(out%y%grid%pndw, shd%NA)
                        if (flds%pndw%m%active .or. flds%pndw%s%active) call output_variables_allocate(out%m%grid%pndw, shd%NA)
                        if (flds%pndw%d%active) call output_variables_allocate(out%d%grid%pndw, shd%NA)
                        if (flds%pndw%h%active) call output_variables_allocate(out%h%grid%pndw, shd%NA)
                    end if
                case (VN_SNO)
                    if (ro%RUNBALWB) then
                        flds%sno%name = VN_SNO
                        call output_field_init(fls, shd, ts, flds%sno, args, nargs)
                        if (flds%sno%y%active) call output_variables_allocate(out%y%grid%sno, shd%NA)
                        if (flds%sno%m%active .or. flds%sno%s%active) call output_variables_allocate(out%m%grid%sno, shd%NA)
                        if (flds%sno%d%active) call output_variables_allocate(out%d%grid%sno, shd%NA)
                        if (flds%sno%h%active) call output_variables_allocate(out%h%grid%sno, shd%NA)
                    end if
                case (VN_WSNO)
                    if (ro%RUNBALWB) then
                        flds%wsno%name = VN_WSNO
                        call output_field_init(fls, shd, ts, flds%wsno, args, nargs)
                        if (flds%wsno%y%active) call output_variables_allocate(out%y%grid%wsno, shd%NA)
                        if (flds%wsno%m%active .or. flds%wsno%s%active) call output_variables_allocate(out%m%grid%wsno, shd%NA)
                        if (flds%wsno%d%active) call output_variables_allocate(out%d%grid%wsno, shd%NA)
                        if (flds%wsno%h%active) call output_variables_allocate(out%h%grid%wsno, shd%NA)
                    end if
                case (VN_STGW, 'STG')
                    if (ro%RUNBALWB) then
                        flds%stgw%name = VN_STGW
                        call output_field_init(fls, shd, ts, flds%stgw, args, nargs)
                        if (flds%stgw%y%active) call output_variables_allocate(out%y%grid%stgw, shd%NA)
                        if (flds%stgw%m%active .or. flds%stgw%s%active) call output_variables_allocate(out%m%grid%stgw, shd%NA)
                        if (flds%stgw%d%active) call output_variables_allocate(out%d%grid%stgw, shd%NA)
                        if (flds%stgw%h%active) call output_variables_allocate(out%h%grid%stgw, shd%NA)
                    end if
                case (VN_DSTGW, 'DSTG')
                    if (ro%RUNBALWB) then
                        flds%dstgw%name = VN_DSTGW
                    end if
                case (VN_THLQ)
                    if (ro%RUNBALWB) then
                        if (.not. allocated(flds%thlq)) allocate(flds%thlq(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            flds%thlq(j)%name = VN_THLQ
                            call output_field_init(fls, shd, ts, flds%thlq(j), args, nargs, j)
                            if (flds%thlq(j)%y%active) call output_variables_allocate(out%y%grid%thlq, shd%NA, shd%lc%IGND)
                            if (flds%thlq(j)%m%active .or. flds%thlq(j)%s%active) then
                                call output_variables_allocate(out%m%grid%thlq, shd%NA, shd%lc%IGND)
                            end if
                            if (flds%thlq(j)%d%active) call output_variables_allocate(out%d%grid%thlq, shd%NA, shd%lc%IGND)
                            if (flds%thlq(j)%h%active) call output_variables_allocate(out%h%grid%thlq, shd%NA, shd%lc%IGND)
                        end do
                    end if
                case (VN_LQWS)
                    if (ro%RUNBALWB) then
                        if (.not. allocated(flds%lqws)) allocate(flds%lqws(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            flds%lqws(j)%name = VN_LQWS
                            call output_field_init(fls, shd, ts, flds%lqws(j), args, nargs, j)
                            if (flds%lqws(j)%y%active) call output_variables_allocate(out%y%grid%lqws, shd%NA, shd%lc%IGND)
                            if (flds%lqws(j)%m%active .or. flds%lqws(j)%s%active) then
                                call output_variables_allocate(out%m%grid%lqws, shd%NA, shd%lc%IGND)
                            end if
                            if (flds%lqws(j)%d%active) call output_variables_allocate(out%d%grid%lqws, shd%NA, shd%lc%IGND)
                            if (flds%lqws(j)%h%active) call output_variables_allocate(out%h%grid%lqws, shd%NA, shd%lc%IGND)
                        end do
                    end if
                case (VN_THIC)
                    if (ro%RUNBALWB) then
                        if (.not. allocated(flds%thic)) allocate(flds%thic(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            flds%thic(j)%name = VN_THIC
                            call output_field_init(fls, shd, ts, flds%thic(j), args, nargs, j)
                            if (flds%thic(j)%y%active) call output_variables_allocate(out%y%grid%thic, shd%NA, shd%lc%IGND)
                            if (flds%thic(j)%m%active .or. flds%thic(j)%s%active) then
                                call output_variables_allocate(out%m%grid%thic, shd%NA, shd%lc%IGND)
                            end if
                            if (flds%thic(j)%d%active) call output_variables_allocate(out%d%grid%thic, shd%NA, shd%lc%IGND)
                            if (flds%thic(j)%h%active) call output_variables_allocate(out%h%grid%thic, shd%NA, shd%lc%IGND)
                        end do
                    end if
                case (VN_FZWS, 'FRWS')
                    if (ro%RUNBALWB) then
                        if (.not. allocated(flds%fzws)) allocate(flds%fzws(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            flds%fzws(j)%name = VN_FZWS
                            call output_field_init(fls, shd, ts, flds%fzws(j), args, nargs, j)
                            if (flds%fzws(j)%y%active) call output_variables_allocate(out%y%grid%fzws, shd%NA, shd%lc%IGND)
                            if (flds%fzws(j)%m%active .or. flds%fzws(j)%s%active) then
                                call output_variables_allocate(out%m%grid%fzws, shd%NA, shd%lc%IGND)
                            end if
                            if (flds%fzws(j)%d%active) call output_variables_allocate(out%d%grid%fzws, shd%NA, shd%lc%IGND)
                            if (flds%fzws(j)%h%active) call output_variables_allocate(out%h%grid%fzws, shd%NA, shd%lc%IGND)
                        end do
                    end if

                !> Energy balance.
                case (VN_QH, 'HFS', 'SensibleHeat')
                    if (ro%RUNBALEB) then
                        flds%qh%name = VN_QH
                        call output_field_init(fls, shd, ts, flds%qh, args, nargs)
                        if (flds%qh%y%active) call output_variables_allocate(out%y%grid%qh, shd%NA)
                        if (flds%qh%m%active .or. flds%qh%s%active) call output_variables_allocate(out%m%grid%qh, shd%NA)
                        if (flds%qh%d%active) call output_variables_allocate(out%d%grid%qh, shd%NA)
                        if (flds%qh%h%active) call output_variables_allocate(out%h%grid%qh, shd%NA)
                    end if
                case (VN_QE, 'QEVP', 'LatentHeat')
                    if (ro%RUNBALEB) then
                        flds%qe%name = VN_QE
                        call output_field_init(fls, shd, ts, flds%qe, args, nargs)
                        if (flds%qe%y%active) call output_variables_allocate(out%y%grid%qe, shd%NA)
                        if (flds%qe%m%active .or. flds%qe%s%active) call output_variables_allocate(out%m%grid%qe, shd%NA)
                        if (flds%qe%d%active) call output_variables_allocate(out%d%grid%qe, shd%NA)
                        if (flds%qe%h%active) call output_variables_allocate(out%h%grid%qe, shd%NA)
                    end if
                case (VN_GFLX, 'HeatConduction')
                    if (ro%RUNBALEB) then
                        if (.not. allocated(flds%gflx)) allocate(flds%gflx(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            flds%gflx(j)%name = VN_GFLX
                            call output_field_init(fls, shd, ts, flds%gflx(j), args, nargs, j)
                            if (flds%gflx(j)%y%active) call output_variables_allocate(out%y%grid%gflx, shd%NA, shd%lc%IGND)
                            if (flds%gflx(j)%m%active .or. flds%gflx(j)%s%active) then
                                call output_variables_allocate(out%m%grid%gflx, shd%NA, shd%lc%IGND)
                            end if
                            if (flds%gflx(j)%d%active) call output_variables_allocate(out%d%grid%gflx, shd%NA, shd%lc%IGND)
                            if (flds%gflx(j)%h%active) call output_variables_allocate(out%h%grid%gflx, shd%NA, shd%lc%IGND)
                        end do
                    end if
                case (VN_TBAR, 'TempSoil', 'Temperature_soil_layers')
                    if (ro%RUNBALEB) then
                        if (.not. allocated(flds%tbar)) allocate(flds%tbar(shd%lc%IGND))
                        do j = 1, shd%lc%IGND
                            flds%tbar(j)%name = VN_TBAR
                            call output_field_init(fls, shd, ts, flds%tbar(j), args, nargs, j)
                            if (flds%tbar(j)%y%active) call output_variables_allocate(out%y%grid%tbar, shd%NA, shd%lc%IGND)
                            if (flds%tbar(j)%m%active .or. flds%tbar(j)%s%active) then
                                call output_variables_allocate(out%m%grid%tbar, shd%NA, shd%lc%IGND)
                            end if
                            if (flds%tbar(j)%d%active) call output_variables_allocate(out%d%grid%tbar, shd%NA, shd%lc%IGND)
                            if (flds%tbar(j)%h%active) call output_variables_allocate(out%h%grid%tbar, shd%NA, shd%lc%IGND)
                        end do
                    end if

                case ('ZOD', 'TMAX', 'TMIN')
                case (VN_ALD)
!                    flds%ald%name = VN_ALD
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
                case (VN_RFF, 'WR_RUNOFF')
                    if (ro%RUNCHNL) then
                        flds%rff%name = VN_RFF
                        call output_field_init(fls, shd, ts, flds%rff, args, nargs)
                        if (flds%rff%y%active) call output_variables_allocate(out%y%grid%rff, shd%NA)
                        if (flds%rff%m%active .or. flds%rff%s%active) call output_variables_allocate(out%m%grid%rff, shd%NA)
                        if (flds%rff%d%active) call output_variables_allocate(out%d%grid%rff, shd%NA)
                        if (flds%rff%h%active) call output_variables_allocate(out%h%grid%rff, shd%NA)
                    end if
                case (VN_RCHG, 'WR_RECHARGE')
                    if (ro%RUNCHNL) then
                        flds%rchg%name = VN_RCHG
                        call output_field_init(fls, shd, ts, flds%rchg, args, nargs)
                        if (flds%rchg%y%active) call output_variables_allocate(out%y%grid%rchg, shd%NA)
                        if (flds%rchg%m%active .or. flds%rchg%s%active) call output_variables_allocate(out%m%grid%rchg, shd%NA)
                        if (flds%rchg%d%active) call output_variables_allocate(out%d%grid%rchg, shd%NA)
                        if (flds%rchg%h%active) call output_variables_allocate(out%h%grid%rchg, shd%NA)
                    end if

                case default
                    n = n - 1
                    call print_warning("'" // trim(args(1)) // "' is not recognized for output.", PAD_3)
            end select
            n = n + 1
        end do
        close(iun)
        write(line, 1001) n
        call print_message_detail('Output variables: ' // trim(adjustl(line)))

        !> Format statements.
1001    format(9999(g15.6, 1x))

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

    subroutine flush_output(fls, shd, field, file, dates)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_field), intent(in) :: field
        type(output_file), intent(in) :: file
        integer, intent(in) :: dates(:, :)

        !> Write output.
        if (file%seq%active) call write_seq(fls, shd, field, file, dates)
        if (file%r2c%active) call write_r2c(fls, shd, field, file, dates)
        if (file%txt%active) call write_txt(fls, shd, field, file, dates)

    end subroutine

    !> Description:
    !>  Update the output field from 'val'.
    subroutine update_output_field(fls, shd, field, dates, file, t, val, cfactorm, cfactora)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, dimension(:, :), intent(in) :: dates
        integer, intent(in) :: t
        real, dimension(:), intent(in) :: val

        !> Input variables (optional).
        real, intent(in), optional :: cfactorm, cfactora

        !> Input/output variables.
        type(output_field) field
        type(output_file) file

        !> Local variables.
!        integer t
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
!        t = 1

        !> Set 'frac' to 'shd%FRAC' if multiplying 'field' by fractional cell area.
        if (field%apply_frac) then
            frac = shd%FRAC
        else
            frac = 1.0
        end if

        !> Update output.
        call output_variables_update_values(file%dat(:, t), (m*val + a)*frac, 0, 0, 'val')

        !> Write output.
        if (.not. flds%in_mem) call flush_output(fls, shd, field, file, dates)

        !> Yearly.
!        if (field%y%active .and. ic%now%year /= ic%next%year) then
!            if (flds%in_mem) t = ic%iter%year
!            call update_output_variable(field%y, (m*out_var%y + a)*frac, t, 'val')
!            if (.not. flds%in_mem) call flush_output(fls, shd, field, field%y, flds%dates%y)
!        end if

        !> Monthly.
!        if (field%m%active .and. ic%now%month /= ic%next%month) then
!            if (flds%in_mem) t = ic%iter%month
!            call update_output_variable(field%m, (m*out_var%m + a)*frac, t, 'val')
!            if (.not. flds%in_mem) call flush_output(fls, shd, field, field%m, flds%dates%m)
!        end if

        !> Daily.
!        if (field%d%active .and. ic%now%day /= ic%next%day) then
!            if (flds%in_mem) t = ic%iter%day
!            call update_output_variable(field%d, (m*out_var%d + a)*frac, t, 'val')
!            if (.not. flds%in_mem) call flush_output(fls, shd, field, field%d, flds%dates%d)
!        end if

        !> Hourly.
!        if (field%h%active .and. ic%now%hour /= ic%next%hour) then
!            if (flds%in_mem) t = ic%iter%hour
!            call update_output_variable(field%h, (m*out_var%h + a)*frac, t, 'val')
!            if (.not. flds%in_mem) call flush_output(fls, shd, field, field%h, flds%dates%h)
!        end if

        !> Seasonally.
!        if (field%s%active .and. ic%now%month /= ic%next%month) then

            !> Update variable from monthly value.
!            t = ic%now%month
!            field%s%dat(:, t) = field%s%dat(:, t)*max(flds%dates%iter_s(t) - 1, 1)
!            call update_output_variable(field%s, (m*out_var%m + a)*frac, t, 'sum')
!            field%s%dat(:, t) = field%s%dat(:, t)/flds%dates%iter_s(t)

            !> Overwrite existing output (for average output).
!            if (.not. flds%in_mem) then
!                call flush_output(fls, shd, field, field%s, flds%dates%s)
!                if (field%s%seq%active) close(field%s%seq%iun)
!                if (field%s%r2c%active) close(field%s%r2c%iun)
!                if (field%s%txt%active) close(field%s%txt%iun)
!            end if
!        end if

    end subroutine

    !> Description:
    !>  Update the 'dates' variable from the 'ic' counter.
    subroutine update_output_dates(dates, t, iter, year, month, day, hour, mins)

        !> Input variables.
        integer, intent(in) :: t, iter

        !> Input variables (optional).
        integer, intent(in), optional :: year, month, day, hour, mins

        !> Input/output variables.
        integer dates(:, :)

        !> Initialize the vector to zero (for missing fields).
        dates(:, t) = 0

        !> Save the time-step using 'now' date.
        dates(1, t) = iter
        if (present(year)) dates(2, t) = year
        if (present(month)) dates(3, t) = month
        if (present(day)) dates(4, t) = day
        if (present(hour)) dates(5, t) = hour
        if (present(mins)) dates(6, t) = mins

    end subroutine

    subroutine update_output_variables(fls, shd)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer iy, im, idy, ih, j
        real frac(shd%NA)
        logical fly, flm, fldy, flh

        !> Update times for output.
        !> Increment the hour to print 01-24 instead of 00-23.
        !> Set 't = 1' if not storing values in memory.
        if (ic%now%year /= ic%next%year) then
            if (flds%in_mem) then
                iy = ic%iter%year
            else
                iy = 1
            end if
            call update_output_dates(flds%dates%y, iy, ic%iter%year, ic%now%year, 1, 1)
            fly = .true.
        else
            fly = .false.
        end if
        if (ic%now%month /= ic%next%month) then
            if (flds%in_mem) then
                im = ic%iter%month
            else
                im = 1
            end if
            call update_output_dates(flds%dates%m, im, ic%iter%month, ic%now%year, ic%now%month, 1)
            flm = .true.
        else
            flm = .false.
        end if
        if (ic%now%day /= ic%next%day) then
            if (flds%in_mem) then
                idy = ic%iter%day
            else
                idy = 1
            end if
            call update_output_dates(flds%dates%d, idy, ic%iter%day, ic%now%year, ic%now%month, ic%now%day)
            fldy = .true.
        else
            fldy = .false.
        end if
        if (ic%now%hour /= ic%next%hour) then
            if (flds%in_mem) then
                ih = ic%iter%hour
            else
                ih = 1
            end if
            call update_output_dates(flds%dates%h, ih, ic%iter%hour, ic%now%year, ic%now%month, ic%now%day, ic%now%hour)
            flh = .true.
        else
            flh = .false.
        end if

        !> Seasonally.
        !> Iterate counter for current month.
        if (ic%now%month /= ic%next%month) then
            call update_output_dates(flds%dates%s, im, im, ic%now%year, ic%now%month, 1)
            flds%dates%iter_s(im) = flds%dates%iter_s(im) + 1
        end if

        !> Update variables.
!        call update_output_field(fls, shd, flds%fsin, out%grid%fsin)
        if (flds%fsin%y%active .and. fly) then
            call update_output_field(fls, shd, flds%fsin, flds%dates%y, flds%fsin%y, iy, out%y%grid%fsin)
        end if
        if (flds%fsin%m%active .and. flm) then
            call update_output_field(fls, shd, flds%fsin, flds%dates%m, flds%fsin%m, im, out%m%grid%fsin)
        end if
        if (flds%fsin%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%fsin, flds%dates%d, flds%fsin%d, idy, out%d%grid%fsin)
        end if
        if (flds%fsin%h%active .and. flh) then
            call update_output_field(fls, shd, flds%fsin, flds%dates%h, flds%fsin%h, ih, out%h%grid%fsin)
        end if
        if (flds%fsin%s%active .and. flm) then
            if (flds%fsin%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%fsin%s%dat(:, im) = flds%fsin%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%fsin%s%dat(:, im), out%m%grid%fsin*frac, 0, 0, 'sum')
            flds%fsin%s%dat(:, im) = flds%fsin%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%fsin, flds%fsin%s, flds%dates%s)
                if (flds%fsin%s%seq%active) close(flds%fsin%s%seq%iun)
                if (flds%fsin%s%r2c%active) close(flds%fsin%s%r2c%iun)
                if (flds%fsin%s%txt%active) close(flds%fsin%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%fsvh, out%grid%fsin, 0.5)
        if (flds%fsvh%y%active .and. fly) then
            call update_output_field(fls, shd, flds%fsvh, flds%dates%y, flds%fsvh%y, iy, out%y%grid%fsin, 0.5)
        end if
        if (flds%fsvh%m%active .and. flm) then
            call update_output_field(fls, shd, flds%fsvh, flds%dates%m, flds%fsvh%m, im, out%m%grid%fsin, 0.5)
        end if
        if (flds%fsvh%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%fsvh, flds%dates%d, flds%fsvh%d, idy, out%d%grid%fsin, 0.5)
        end if
        if (flds%fsvh%h%active .and. flh) then
            call update_output_field(fls, shd, flds%fsvh, flds%dates%h, flds%fsvh%h, ih, out%h%grid%fsin, 0.5)
        end if
        if (flds%fsvh%s%active .and. flm) then
            if (flds%fsvh%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%fsvh%s%dat(:, im) = flds%fsvh%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%fsvh%s%dat(:, im), 0.5*out%m%grid%fsin*frac, 0, 0, 'sum')
            flds%fsvh%s%dat(:, im) = flds%fsvh%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%fsvh, flds%fsvh%s, flds%dates%s)
                if (flds%fsvh%s%seq%active) close(flds%fsvh%s%seq%iun)
                if (flds%fsvh%s%r2c%active) close(flds%fsvh%s%r2c%iun)
                if (flds%fsvh%s%txt%active) close(flds%fsvh%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%fsih, out%grid%fsin, 0.5)
        if (flds%fsih%y%active .and. fly) then
            call update_output_field(fls, shd, flds%fsih, flds%dates%y, flds%fsih%y, iy, out%y%grid%fsin, 0.5)
        end if
        if (flds%fsih%m%active .and. flm) then
            call update_output_field(fls, shd, flds%fsih, flds%dates%m, flds%fsih%m, im, out%m%grid%fsin, 0.5)
        end if
        if (flds%fsih%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%fsih, flds%dates%d, flds%fsih%d, idy, out%d%grid%fsin, 0.5)
        end if
        if (flds%fsih%h%active .and. flh) then
            call update_output_field(fls, shd, flds%fsih, flds%dates%h, flds%fsih%h, ih, out%h%grid%fsin, 0.5)
        end if
        if (flds%fsih%s%active .and. flm) then
            if (flds%fsih%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%fsih%s%dat(:, im) = flds%fsih%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%fsih%s%dat(:, im), 0.5*out%m%grid%fsin*frac, 0, 0, 'sum')
            flds%fsih%s%dat(:, im) = flds%fsih%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%fsih, flds%fsih%s, flds%dates%s)
                if (flds%fsih%s%seq%active) close(flds%fsih%s%seq%iun)
                if (flds%fsih%s%r2c%active) close(flds%fsih%s%r2c%iun)
                if (flds%fsih%s%txt%active) close(flds%fsih%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%flin, out%grid%flin)
        if (flds%flin%y%active .and. fly) then
            call update_output_field(fls, shd, flds%flin, flds%dates%y, flds%flin%y, iy, out%y%grid%flin)
        end if
        if (flds%flin%m%active .and. flm) then
            call update_output_field(fls, shd, flds%flin, flds%dates%m, flds%flin%m, im, out%m%grid%flin)
        end if
        if (flds%flin%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%flin, flds%dates%d, flds%flin%d, idy, out%d%grid%flin)
        end if
        if (flds%flin%h%active .and. flh) then
            call update_output_field(fls, shd, flds%flin, flds%dates%h, flds%flin%h, ih, out%h%grid%flin)
        end if
        if (flds%flin%s%active .and. flm) then
            if (flds%flin%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%flin%s%dat(:, im) = flds%flin%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%flin%s%dat(:, im), out%m%grid%flin*frac, 0, 0, 'sum')
            flds%flin%s%dat(:, im) = flds%flin%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%flin, flds%flin%s, flds%dates%s)
                if (flds%flin%s%seq%active) close(flds%flin%s%seq%iun)
                if (flds%flin%s%r2c%active) close(flds%flin%s%r2c%iun)
                if (flds%flin%s%txt%active) close(flds%flin%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%uv, out%grid%uv)
        if (flds%uv%y%active .and. fly) then
            call update_output_field(fls, shd, flds%uv, flds%dates%y, flds%uv%y, iy, out%y%grid%uv)
        end if
        if (flds%uv%m%active .and. flm) then
            call update_output_field(fls, shd, flds%uv, flds%dates%m, flds%uv%m, im, out%m%grid%uv)
        end if
        if (flds%uv%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%uv, flds%dates%d, flds%uv%d, idy, out%d%grid%uv)
        end if
        if (flds%uv%h%active .and. flh) then
            call update_output_field(fls, shd, flds%uv, flds%dates%h, flds%uv%h, ih, out%h%grid%uv)
        end if
        if (flds%uv%s%active .and. flm) then
            if (flds%uv%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%uv%s%dat(:, im) = flds%uv%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%uv%s%dat(:, im), out%m%grid%uv*frac, 0, 0, 'sum')
            flds%uv%s%dat(:, im) = flds%uv%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%uv, flds%uv%s, flds%dates%s)
                if (flds%uv%s%seq%active) close(flds%uv%s%seq%iun)
                if (flds%uv%s%r2c%active) close(flds%uv%s%r2c%iun)
                if (flds%uv%s%txt%active) close(flds%uv%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%ta, out%grid%ta)
        if (flds%ta%y%active .and. fly) then
            call update_output_field(fls, shd, flds%ta, flds%dates%y, flds%ta%y, iy, out%y%grid%ta)
        end if
        if (flds%ta%m%active .and. flm) then
            call update_output_field(fls, shd, flds%ta, flds%dates%m, flds%ta%m, im, out%m%grid%ta)
        end if
        if (flds%ta%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%ta, flds%dates%d, flds%ta%d, idy, out%d%grid%ta)
        end if
        if (flds%ta%h%active .and. flh) then
            call update_output_field(fls, shd, flds%ta, flds%dates%h, flds%ta%h, ih, out%h%grid%ta)
        end if
        if (flds%ta%s%active .and. flm) then
            if (flds%ta%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%ta%s%dat(:, im) = flds%ta%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%ta%s%dat(:, im), out%m%grid%ta*frac, 0, 0, 'sum')
            flds%ta%s%dat(:, im) = flds%ta%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%ta, flds%ta%s, flds%dates%s)
                if (flds%ta%s%seq%active) close(flds%ta%s%seq%iun)
                if (flds%ta%s%r2c%active) close(flds%ta%s%r2c%iun)
                if (flds%ta%s%txt%active) close(flds%ta%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%qa, out%grid%qa)
        if (flds%qa%y%active .and. fly) then
            call update_output_field(fls, shd, flds%qa, flds%dates%y, flds%qa%y, iy, out%y%grid%qa)
        end if
        if (flds%qa%m%active .and. flm) then
            call update_output_field(fls, shd, flds%qa, flds%dates%m, flds%qa%m, im, out%m%grid%qa)
        end if
        if (flds%qa%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%qa, flds%dates%d, flds%qa%d, idy, out%d%grid%qa)
        end if
        if (flds%qa%h%active .and. flh) then
            call update_output_field(fls, shd, flds%qa, flds%dates%h, flds%qa%h, ih, out%h%grid%qa)
        end if
        if (flds%qa%s%active .and. flm) then
            if (flds%qa%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%qa%s%dat(:, im) = flds%qa%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%qa%s%dat(:, im), out%m%grid%qa*frac, 0, 0, 'sum')
            flds%qa%s%dat(:, im) = flds%qa%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%qa, flds%qa%s, flds%dates%s)
                if (flds%qa%s%seq%active) close(flds%qa%s%seq%iun)
                if (flds%qa%s%r2c%active) close(flds%qa%s%r2c%iun)
                if (flds%qa%s%txt%active) close(flds%qa%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%pres, out%grid%pres)
        if (flds%pres%y%active .and. fly) then
            call update_output_field(fls, shd, flds%pres, flds%dates%y, flds%pres%y, iy, out%y%grid%pres)
        end if
        if (flds%pres%m%active .and. flm) then
            call update_output_field(fls, shd, flds%pres, flds%dates%m, flds%pres%m, im, out%m%grid%pres)
        end if
        if (flds%pres%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%pres, flds%dates%d, flds%pres%d, idy, out%d%grid%pres)
        end if
        if (flds%pres%h%active .and. flh) then
            call update_output_field(fls, shd, flds%pres, flds%dates%h, flds%pres%h, ih, out%h%grid%pres)
        end if
        if (flds%pres%s%active .and. flm) then
            if (flds%pres%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%pres%s%dat(:, im) = flds%pres%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%pres%s%dat(:, im), out%m%grid%pres*frac, 0, 0, 'sum')
            flds%pres%s%dat(:, im) = flds%pres%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%pres, flds%pres%s, flds%dates%s)
                if (flds%pres%s%seq%active) close(flds%pres%s%seq%iun)
                if (flds%pres%s%r2c%active) close(flds%pres%s%r2c%iun)
                if (flds%pres%s%txt%active) close(flds%pres%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%pre, out%grid%pre)
        if (flds%pre%y%active .and. fly) then
            call update_output_field(fls, shd, flds%pre, flds%dates%y, flds%pre%y, iy, out%y%grid%pre)
        end if
        if (flds%pre%m%active .and. flm) then
            call update_output_field(fls, shd, flds%pre, flds%dates%m, flds%pre%m, im, out%m%grid%pre)
        end if
        if (flds%pre%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%pre, flds%dates%d, flds%pre%d, idy, out%d%grid%pre)
        end if
        if (flds%pre%h%active .and. flh) then
            call update_output_field(fls, shd, flds%pre, flds%dates%h, flds%pre%h, ih, out%h%grid%pre)
        end if
        if (flds%pre%s%active .and. flm) then
            if (flds%pre%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%pre%s%dat(:, im) = flds%pre%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%pre%s%dat(:, im), out%m%grid%pre*frac, 0, 0, 'sum')
            flds%pre%s%dat(:, im) = flds%pre%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%pre, flds%pre%s, flds%dates%s)
                if (flds%pre%s%seq%active) close(flds%pre%s%seq%iun)
                if (flds%pre%s%r2c%active) close(flds%pre%s%r2c%iun)
                if (flds%pre%s%txt%active) close(flds%pre%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%prec, out%grid%pre, real(ic%dts))
        if (flds%prec%y%active .and. fly) then
            call update_output_field(fls, shd, flds%prec, flds%dates%y, flds%prec%y, iy, out%y%grid%prec, real(ic%dts))
        end if
        if (flds%prec%m%active .and. flm) then
            call update_output_field(fls, shd, flds%prec, flds%dates%m, flds%prec%m, im, out%m%grid%prec, real(ic%dts))
        end if
        if (flds%prec%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%prec, flds%dates%d, flds%prec%d, idy, out%d%grid%prec, real(ic%dts))
        end if
        if (flds%prec%h%active .and. flh) then
            call update_output_field(fls, shd, flds%prec, flds%dates%h, flds%prec%h, ih, out%h%grid%prec, real(ic%dts))
        end if
        if (flds%prec%s%active .and. flm) then
            if (flds%prec%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%prec%s%dat(:, im) = flds%prec%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%prec%s%dat(:, im), real(ic%dts)*out%m%grid%prec*frac, 0, 0, 'sum')
            flds%prec%s%dat(:, im) = flds%prec%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%prec, flds%prec%s, flds%dates%s)
                if (flds%prec%s%seq%active) close(flds%prec%s%seq%iun)
                if (flds%prec%s%r2c%active) close(flds%prec%s%r2c%iun)
                if (flds%prec%s%txt%active) close(flds%prec%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%evap, out%grid%evap, real(ic%dts))
        if (flds%evap%y%active .and. fly) then
            call update_output_field(fls, shd, flds%evap, flds%dates%y, flds%evap%y, iy, out%y%grid%evap, real(ic%dts))
        end if
        if (flds%evap%m%active .and. flm) then
            call update_output_field(fls, shd, flds%evap, flds%dates%m, flds%evap%m, im, out%m%grid%evap, real(ic%dts))
        end if
        if (flds%evap%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%evap, flds%dates%d, flds%evap%d, idy, out%d%grid%evap, real(ic%dts))
        end if
        if (flds%evap%h%active .and. flh) then
            call update_output_field(fls, shd, flds%evap, flds%dates%h, flds%evap%h, ih, out%h%grid%evap, real(ic%dts))
        end if
        if (flds%evap%s%active .and. flm) then
            if (flds%evap%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%evap%s%dat(:, im) = flds%evap%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%evap%s%dat(:, im), real(ic%dts)*out%m%grid%evap*frac, 0, 0, 'sum')
            flds%evap%s%dat(:, im) = flds%evap%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%evap, flds%evap%s, flds%dates%s)
                if (flds%evap%s%seq%active) close(flds%evap%s%seq%iun)
                if (flds%evap%s%r2c%active) close(flds%evap%s%r2c%iun)
                if (flds%evap%s%txt%active) close(flds%evap%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%rof, out%grid%rof, real(ic%dts))
        if (flds%rof%y%active .and. fly) then
            call update_output_field(fls, shd, flds%rof, flds%dates%y, flds%rof%y, iy, out%y%grid%rof, real(ic%dts))
        end if
        if (flds%rof%m%active .and. flm) then
            call update_output_field(fls, shd, flds%rof, flds%dates%m, flds%rof%m, im, out%m%grid%rof, real(ic%dts))
        end if
        if (flds%rof%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%rof, flds%dates%d, flds%rof%d, idy, out%d%grid%rof, real(ic%dts))
        end if
        if (flds%rof%h%active .and. flh) then
            call update_output_field(fls, shd, flds%rof, flds%dates%h, flds%rof%h, ih, out%h%grid%rof, real(ic%dts))
        end if
        if (flds%rof%s%active .and. flm) then
            if (flds%rof%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%rof%s%dat(:, im) = flds%rof%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%rof%s%dat(:, im), real(ic%dts)*out%m%grid%rof*frac, 0, 0, 'sum')
            flds%rof%s%dat(:, im) = flds%rof%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%rof, flds%rof%s, flds%dates%s)
                if (flds%rof%s%seq%active) close(flds%rof%s%seq%iun)
                if (flds%rof%s%r2c%active) close(flds%rof%s%r2c%iun)
                if (flds%rof%s%txt%active) close(flds%rof%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%rcan, out%grid%rcan)
        if (flds%rcan%y%active .and. fly) then
            call update_output_field(fls, shd, flds%rcan, flds%dates%y, flds%rcan%y, iy, out%y%grid%rcan)
        end if
        if (flds%rcan%m%active .and. flm) then
            call update_output_field(fls, shd, flds%rcan, flds%dates%m, flds%rcan%m, im, out%m%grid%rcan)
        end if
        if (flds%rcan%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%rcan, flds%dates%d, flds%rcan%d, idy, out%d%grid%rcan)
        end if
        if (flds%rcan%h%active .and. flh) then
            call update_output_field(fls, shd, flds%rcan, flds%dates%h, flds%rcan%h, ih, out%h%grid%rcan)
        end if
        if (flds%rcan%s%active .and. flm) then
            if (flds%rcan%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%rcan%s%dat(:, im) = flds%rcan%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%rcan%s%dat(:, im), out%m%grid%rcan*frac, 0, 0, 'sum')
            flds%rcan%s%dat(:, im) = flds%rcan%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%rcan, flds%rcan%s, flds%dates%s)
                if (flds%rcan%s%seq%active) close(flds%rcan%s%seq%iun)
                if (flds%rcan%s%r2c%active) close(flds%rcan%s%r2c%iun)
                if (flds%rcan%s%txt%active) close(flds%rcan%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%sncan, out%grid%sncan)
        if (flds%sncan%y%active .and. fly) then
            call update_output_field(fls, shd, flds%sncan, flds%dates%y, flds%sncan%y, iy, out%y%grid%sncan)
        end if
        if (flds%sncan%m%active .and. flm) then
            call update_output_field(fls, shd, flds%sncan, flds%dates%m, flds%sncan%m, im, out%m%grid%sncan)
        end if
        if (flds%sncan%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%sncan, flds%dates%d, flds%sncan%d, idy, out%d%grid%sncan)
        end if
        if (flds%sncan%h%active .and. flh) then
            call update_output_field(fls, shd, flds%sncan, flds%dates%h, flds%sncan%h, ih, out%h%grid%sncan)
        end if
        if (flds%sncan%s%active .and. flm) then
            if (flds%sncan%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%sncan%s%dat(:, im) = flds%sncan%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%sncan%s%dat(:, im), out%m%grid%sncan*frac, 0, 0, 'sum')
            flds%sncan%s%dat(:, im) = flds%sncan%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%sncan, flds%sncan%s, flds%dates%s)
                if (flds%sncan%s%seq%active) close(flds%sncan%s%seq%iun)
                if (flds%sncan%s%r2c%active) close(flds%sncan%s%r2c%iun)
                if (flds%sncan%s%txt%active) close(flds%sncan%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%pndw, out%grid%pndw)
        if (flds%pndw%y%active .and. fly) then
            call update_output_field(fls, shd, flds%pndw, flds%dates%y, flds%pndw%y, iy, out%y%grid%pndw)
        end if
        if (flds%pndw%m%active .and. flm) then
            call update_output_field(fls, shd, flds%pndw, flds%dates%m, flds%pndw%m, im, out%m%grid%pndw)
        end if
        if (flds%pndw%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%pndw, flds%dates%d, flds%pndw%d, idy, out%d%grid%pndw)
        end if
        if (flds%pndw%h%active .and. flh) then
            call update_output_field(fls, shd, flds%pndw, flds%dates%h, flds%pndw%h, ih, out%h%grid%pndw)
        end if
        if (flds%pndw%s%active .and. flm) then
            if (flds%pndw%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%pndw%s%dat(:, im) = flds%pndw%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%pndw%s%dat(:, im), out%m%grid%pndw*frac, 0, 0, 'sum')
            flds%pndw%s%dat(:, im) = flds%pndw%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%pndw, flds%pndw%s, flds%dates%s)
                if (flds%pndw%s%seq%active) close(flds%pndw%s%seq%iun)
                if (flds%pndw%s%r2c%active) close(flds%pndw%s%r2c%iun)
                if (flds%pndw%s%txt%active) close(flds%pndw%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%sno, out%grid%sno)
        if (flds%sno%y%active .and. fly) then
            call update_output_field(fls, shd, flds%sno, flds%dates%y, flds%sno%y, iy, out%y%grid%sno)
        end if
        if (flds%sno%m%active .and. flm) then
            call update_output_field(fls, shd, flds%sno, flds%dates%m, flds%sno%m, im, out%m%grid%sno)
        end if
        if (flds%sno%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%sno, flds%dates%d, flds%sno%d, idy, out%d%grid%sno)
        end if
        if (flds%sno%h%active .and. flh) then
            call update_output_field(fls, shd, flds%sno, flds%dates%h, flds%sno%h, ih, out%h%grid%sno)
        end if
        if (flds%sno%s%active .and. flm) then
            if (flds%sno%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%sno%s%dat(:, im) = flds%sno%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%sno%s%dat(:, im), out%m%grid%sno*frac, 0, 0, 'sum')
            flds%sno%s%dat(:, im) = flds%sno%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%sno, flds%sno%s, flds%dates%s)
                if (flds%sno%s%seq%active) close(flds%sno%s%seq%iun)
                if (flds%sno%s%r2c%active) close(flds%sno%s%r2c%iun)
                if (flds%sno%s%txt%active) close(flds%sno%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%wsno, out%grid%wsno)
        if (flds%wsno%y%active .and. fly) then
            call update_output_field(fls, shd, flds%wsno, flds%dates%y, flds%wsno%y, iy, out%y%grid%wsno)
        end if
        if (flds%wsno%m%active .and. flm) then
            call update_output_field(fls, shd, flds%wsno, flds%dates%m, flds%wsno%m, im, out%m%grid%wsno)
        end if
        if (flds%wsno%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%wsno, flds%dates%d, flds%wsno%d, idy, out%d%grid%wsno)
        end if
        if (flds%wsno%h%active .and. flh) then
            call update_output_field(fls, shd, flds%wsno, flds%dates%h, flds%wsno%h, ih, out%h%grid%wsno)
        end if
        if (flds%wsno%s%active .and. flm) then
            if (flds%wsno%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%wsno%s%dat(:, im) = flds%wsno%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%wsno%s%dat(:, im), out%m%grid%wsno*frac, 0, 0, 'sum')
            flds%wsno%s%dat(:, im) = flds%wsno%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%wsno, flds%wsno%s, flds%dates%s)
                if (flds%wsno%s%seq%active) close(flds%wsno%s%seq%iun)
                if (flds%wsno%s%r2c%active) close(flds%wsno%s%r2c%iun)
                if (flds%wsno%s%txt%active) close(flds%wsno%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%stgw, out%grid%stgw)
        if (flds%stgw%y%active .and. fly) then
            call update_output_field(fls, shd, flds%stgw, flds%dates%y, flds%stgw%y, iy, out%y%grid%stgw)
        end if
        if (flds%stgw%m%active .and. flm) then
            call update_output_field(fls, shd, flds%stgw, flds%dates%m, flds%stgw%m, im, out%m%grid%stgw)
        end if
        if (flds%stgw%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%stgw, flds%dates%d, flds%stgw%d, idy, out%d%grid%stgw)
        end if
        if (flds%stgw%h%active .and. flh) then
            call update_output_field(fls, shd, flds%stgw, flds%dates%h, flds%stgw%h, ih, out%h%grid%stgw)
        end if
        if (flds%stgw%s%active .and. flm) then
            if (flds%stgw%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%stgw%s%dat(:, im) = flds%stgw%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%stgw%s%dat(:, im), out%m%grid%stgw*frac, 0, 0, 'sum')
            flds%stgw%s%dat(:, im) = flds%stgw%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%stgw, flds%stgw%s, flds%dates%s)
                if (flds%stgw%s%seq%active) close(flds%stgw%s%seq%iun)
                if (flds%stgw%s%r2c%active) close(flds%stgw%s%r2c%iun)
                if (flds%stgw%s%txt%active) close(flds%stgw%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%qh, out%grid%qh)
        if (flds%qh%y%active .and. fly) then
            call update_output_field(fls, shd, flds%qh, flds%dates%y, flds%qh%y, iy, out%y%grid%qh)
        end if
        if (flds%qh%m%active .and. flm) then
            call update_output_field(fls, shd, flds%qh, flds%dates%m, flds%qh%m, im, out%m%grid%qh)
        end if
        if (flds%qh%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%qh, flds%dates%d, flds%qh%d, idy, out%d%grid%qh)
        end if
        if (flds%qh%h%active .and. flh) then
            call update_output_field(fls, shd, flds%qh, flds%dates%h, flds%qh%h, ih, out%h%grid%qh)
        end if
        if (flds%qh%s%active .and. flm) then
            if (flds%qh%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%qh%s%dat(:, im) = flds%qh%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%qh%s%dat(:, im), out%m%grid%qh*frac, 0, 0, 'sum')
            flds%qh%s%dat(:, im) = flds%qh%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%qh, flds%qh%s, flds%dates%s)
                if (flds%qh%s%seq%active) close(flds%qh%s%seq%iun)
                if (flds%qh%s%r2c%active) close(flds%qh%s%r2c%iun)
                if (flds%qh%s%txt%active) close(flds%qh%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%qe, out%grid%qe)
        if (flds%qe%y%active .and. fly) then
            call update_output_field(fls, shd, flds%qe, flds%dates%y, flds%qe%y, iy, out%y%grid%qe)
        end if
        if (flds%qe%m%active .and. flm) then
            call update_output_field(fls, shd, flds%qe, flds%dates%m, flds%qe%m, im, out%m%grid%qe)
        end if
        if (flds%qe%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%qe, flds%dates%d, flds%qe%d, idy, out%d%grid%qe)
        end if
        if (flds%qe%h%active .and. flh) then
            call update_output_field(fls, shd, flds%qe, flds%dates%h, flds%qe%h, ih, out%h%grid%qe)
        end if
        if (flds%qe%s%active .and. flm) then
            if (flds%qe%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%qe%s%dat(:, im) = flds%qe%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%qe%s%dat(:, im), out%m%grid%qe*frac, 0, 0, 'sum')
            flds%qe%s%dat(:, im) = flds%qe%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%qe, flds%qe%s, flds%dates%s)
                if (flds%qe%s%seq%active) close(flds%qe%s%seq%iun)
                if (flds%qe%s%r2c%active) close(flds%qe%s%r2c%iun)
                if (flds%qe%s%txt%active) close(flds%qe%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%rff, out%grid%rff, real(ic%dts))
        if (flds%rff%y%active .and. fly) then
            call update_output_field(fls, shd, flds%rff, flds%dates%y, flds%rff%y, iy, out%y%grid%rff, real(ic%dts))
        end if
        if (flds%rff%m%active .and. flm) then
            call update_output_field(fls, shd, flds%rff, flds%dates%m, flds%rff%m, im, out%m%grid%rff, real(ic%dts))
        end if
        if (flds%rff%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%rff, flds%dates%d, flds%rff%d, idy, out%d%grid%rff, real(ic%dts))
        end if
        if (flds%rff%h%active .and. flh) then
            call update_output_field(fls, shd, flds%rff, flds%dates%h, flds%rff%h, ih, out%h%grid%rff, real(ic%dts))
        end if
        if (flds%rff%s%active .and. flm) then
            if (flds%rff%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%rff%s%dat(:, im) = flds%rff%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%rff%s%dat(:, im), real(ic%dts)*out%m%grid%rff*frac, 0, 0, 'sum')
            flds%rff%s%dat(:, im) = flds%rff%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%rff, flds%rff%s, flds%dates%s)
                if (flds%rff%s%seq%active) close(flds%rff%s%seq%iun)
                if (flds%rff%s%r2c%active) close(flds%rff%s%r2c%iun)
                if (flds%rff%s%txt%active) close(flds%rff%s%txt%iun)
            end if
        end if

!        call update_output_field(fls, shd, flds%rchg, out%grid%rchg, real(ic%dts))
        if (flds%rchg%y%active .and. fly) then
            call update_output_field(fls, shd, flds%rchg, flds%dates%y, flds%rchg%y, iy, out%y%grid%rchg, real(ic%dts))
        end if
        if (flds%rchg%m%active .and. flm) then
            call update_output_field(fls, shd, flds%rchg, flds%dates%m, flds%rchg%m, im, out%m%grid%rchg, real(ic%dts))
        end if
        if (flds%rchg%d%active .and. fldy) then
            call update_output_field(fls, shd, flds%rchg, flds%dates%d, flds%rchg%d, idy, out%d%grid%rchg, real(ic%dts))
        end if
        if (flds%rchg%h%active .and. flh) then
            call update_output_field(fls, shd, flds%rchg, flds%dates%h, flds%rchg%h, ih, out%h%grid%rchg, real(ic%dts))
        end if
        if (flds%rchg%s%active .and. flm) then
            if (flds%rchg%apply_frac) then
                frac = shd%FRAC
            else
                frac = 1.0
            end if
            flds%rchg%s%dat(:, im) = flds%rchg%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
            call output_variables_update_values(flds%rchg%s%dat(:, im), real(ic%dts)*out%h%grid%rchg*frac, 0, 0, 'sum')
            flds%rchg%s%dat(:, im) = flds%rchg%s%dat(:, im)/flds%dates%iter_s(im)
            if (.not. flds%in_mem) then
                call flush_output(fls, shd, flds%rchg, flds%rchg%s, flds%dates%s)
                if (flds%rchg%s%seq%active) close(flds%rchg%s%seq%iun)
                if (flds%rchg%s%r2c%active) close(flds%rchg%s%r2c%iun)
                if (flds%rchg%s%txt%active) close(flds%rchg%s%txt%iun)
            end if
        end if

        !> Variables with multiple layers.
        !> Must check if the multi-layer variable is allocated.
        if (allocated(flds%thlq)) then
            do j = 1, shd%lc%IGND
!                call update_output_field(fls, shd, flds%thlq(j), out%grid%thlq(j))
                if (flds%thlq(j)%y%active .and. fly) then
                    call update_output_field(fls, shd, flds%thlq(j), flds%dates%y, flds%thlq(j)%y, iy, out%y%grid%thlq(:, j))
                end if
                if (flds%thlq(j)%m%active .and. flm) then
                    call update_output_field(fls, shd, flds%thlq(j), flds%dates%m, flds%thlq(j)%m, im, out%m%grid%thlq(:, j))
                end if
                if (flds%thlq(j)%d%active .and. fldy) then
                    call update_output_field(fls, shd, flds%thlq(j), flds%dates%d, flds%thlq(j)%d, idy, out%d%grid%thlq(:, j))
                end if
                if (flds%thlq(j)%h%active .and. flh) then
                    call update_output_field(fls, shd, flds%thlq(j), flds%dates%h, flds%thlq(j)%h, ih, out%h%grid%thlq(:, j))
                end if
                if (flds%thlq(j)%s%active .and. flm) then
                    if (flds%thlq(j)%apply_frac) then
                        frac = shd%FRAC
                    else
                        frac = 1.0
                    end if
                    flds%thlq(j)%s%dat(:, im) = flds%thlq(j)%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
                    call output_variables_update_values(flds%thlq(j)%s%dat(:, im), out%m%grid%thlq(:, j)*frac, 0, 0, 'sum')
                    flds%thlq(j)%s%dat(:, im) = flds%thlq(j)%s%dat(:, im)/flds%dates%iter_s(im)
                    if (.not. flds%in_mem) then
                        call flush_output(fls, shd, flds%thlq(j), flds%thlq(j)%s, flds%dates%s)
                        if (flds%thlq(j)%s%seq%active) close(flds%thlq(j)%s%seq%iun)
                        if (flds%thlq(j)%s%r2c%active) close(flds%thlq(j)%s%r2c%iun)
                        if (flds%thlq(j)%s%txt%active) close(flds%thlq(j)%s%txt%iun)
                    end if
                end if
            end do
        end if
        if (allocated(flds%lqws)) then
            do j = 1, shd%lc%IGND
!                call update_output_field(fls, shd, flds%lqws(j), out%grid%lqws(j))
                if (flds%lqws(j)%y%active .and. fly) then
                    call update_output_field(fls, shd, flds%lqws(j), flds%dates%y, flds%lqws(j)%y, iy, out%y%grid%lqws(:, j))
                end if
                if (flds%lqws(j)%m%active .and. flm) then
                    call update_output_field(fls, shd, flds%lqws(j), flds%dates%m, flds%lqws(j)%m, im, out%m%grid%lqws(:, j))
                end if
                if (flds%lqws(j)%d%active .and. fldy) then
                    call update_output_field(fls, shd, flds%lqws(j), flds%dates%d, flds%lqws(j)%d, idy, out%d%grid%lqws(:, j))
                end if
                if (flds%lqws(j)%h%active .and. flh) then
                    call update_output_field(fls, shd, flds%lqws(j), flds%dates%h, flds%lqws(j)%h, ih, out%h%grid%lqws(:, j))
                end if
                if (flds%lqws(j)%s%active .and. flm) then
                    if (flds%lqws(j)%apply_frac) then
                        frac = shd%FRAC
                    else
                        frac = 1.0
                    end if
                    flds%lqws(j)%s%dat(:, im) = flds%lqws(j)%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
                    call output_variables_update_values(flds%lqws(j)%s%dat(:, im), out%m%grid%lqws(:, j)*frac, 0, 0, 'sum')
                    flds%lqws(j)%s%dat(:, im) = flds%lqws(j)%s%dat(:, im)/flds%dates%iter_s(im)
                    if (.not. flds%in_mem) then
                        call flush_output(fls, shd, flds%lqws(j), flds%lqws(j)%s, flds%dates%s)
                        if (flds%lqws(j)%s%seq%active) close(flds%lqws(j)%s%seq%iun)
                        if (flds%lqws(j)%s%r2c%active) close(flds%lqws(j)%s%r2c%iun)
                        if (flds%lqws(j)%s%txt%active) close(flds%lqws(j)%s%txt%iun)
                    end if
                end if
            end do
        end if
        if (allocated(flds%thic)) then
            do j = 1, shd%lc%IGND
!                call update_output_field(fls, shd, flds%thic(j), out%grid%thic(j))
                if (flds%thic(j)%y%active .and. fly) then
                    call update_output_field(fls, shd, flds%thic(j), flds%dates%y, flds%thic(j)%y, iy, out%y%grid%thic(:, j))
                end if
                if (flds%thic(j)%m%active .and. flm) then
                    call update_output_field(fls, shd, flds%thic(j), flds%dates%m, flds%thic(j)%m, im, out%m%grid%thic(:, j))
                end if
                if (flds%thic(j)%d%active .and. fldy) then
                    call update_output_field(fls, shd, flds%thic(j), flds%dates%d, flds%thic(j)%d, idy, out%d%grid%thic(:, j))
                end if
                if (flds%thic(j)%h%active .and. flh) then
                    call update_output_field(fls, shd, flds%thic(j), flds%dates%h, flds%thic(j)%h, ih, out%h%grid%thic(:, j))
                end if
                if (flds%thic(j)%s%active .and. flm) then
                    if (flds%thic(j)%apply_frac) then
                        frac = shd%FRAC
                    else
                        frac = 1.0
                    end if
                    flds%thic(j)%s%dat(:, im) = flds%thic(j)%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
                    call output_variables_update_values(flds%thic(j)%s%dat(:, im), out%m%grid%thic(:, j)*frac, 0, 0, 'sum')
                    flds%thic(j)%s%dat(:, im) = flds%thic(j)%s%dat(:, im)/flds%dates%iter_s(im)
                    if (.not. flds%in_mem) then
                        call flush_output(fls, shd, flds%thic(j), flds%thic(j)%s, flds%dates%s)
                        if (flds%thic(j)%s%seq%active) close(flds%thic(j)%s%seq%iun)
                        if (flds%thic(j)%s%r2c%active) close(flds%thic(j)%s%r2c%iun)
                        if (flds%thic(j)%s%txt%active) close(flds%thic(j)%s%txt%iun)
                    end if
                end if
            end do
        end if
        if (allocated(flds%fzws)) then
            do j = 1, shd%lc%IGND
!                call update_output_field(fls, shd, flds%fzws(j), out%grid%fzws(j))
                if (flds%fzws(j)%y%active .and. fly) then
                    call update_output_field(fls, shd, flds%fzws(j), flds%dates%y, flds%fzws(j)%y, iy, out%y%grid%fzws(:, j))
                end if
                if (flds%fzws(j)%m%active .and. flm) then
                    call update_output_field(fls, shd, flds%fzws(j), flds%dates%m, flds%fzws(j)%m, im, out%m%grid%fzws(:, j))
                end if
                if (flds%fzws(j)%d%active .and. fldy) then
                    call update_output_field(fls, shd, flds%fzws(j), flds%dates%d, flds%fzws(j)%d, idy, out%d%grid%fzws(:, j))
                end if
                if (flds%fzws(j)%h%active .and. flh) then
                    call update_output_field(fls, shd, flds%fzws(j), flds%dates%h, flds%fzws(j)%h, ih, out%h%grid%fzws(:, j))
                end if
                if (flds%fzws(j)%s%active .and. flm) then
                    if (flds%fzws(j)%apply_frac) then
                        frac = shd%FRAC
                    else
                        frac = 1.0
                    end if
                    flds%fzws(j)%s%dat(:, im) = flds%fzws(j)%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
                    call output_variables_update_values(flds%fzws(j)%s%dat(:, im), out%m%grid%fzws(:, j)*frac, 0, 0, 'sum')
                    flds%fzws(j)%s%dat(:, im) = flds%fzws(j)%s%dat(:, im)/flds%dates%iter_s(im)
                    if (.not. flds%in_mem) then
                        call flush_output(fls, shd, flds%fzws(j), flds%fzws(j)%s, flds%dates%s)
                        if (flds%fzws(j)%s%seq%active) close(flds%fzws(j)%s%seq%iun)
                        if (flds%fzws(j)%s%r2c%active) close(flds%fzws(j)%s%r2c%iun)
                        if (flds%fzws(j)%s%txt%active) close(flds%fzws(j)%s%txt%iun)
                    end if
                end if
            end do
        end if
        if (allocated(flds%gflx)) then
            do j = 1, shd%lc%IGND
!                call update_output_field(fls, shd, flds%gflx(j), out%grid%gflx(j))
                if (flds%gflx(j)%y%active .and. fly) then
                    call update_output_field(fls, shd, flds%gflx(j), flds%dates%y, flds%gflx(j)%y, iy, out%y%grid%gflx(:, j))
                end if
                if (flds%gflx(j)%m%active .and. flm) then
                    call update_output_field(fls, shd, flds%gflx(j), flds%dates%m, flds%gflx(j)%m, im, out%m%grid%gflx(:, j))
                end if
                if (flds%gflx(j)%d%active .and. fldy) then
                    call update_output_field(fls, shd, flds%gflx(j), flds%dates%d, flds%gflx(j)%d, idy, out%d%grid%gflx(:, j))
                end if
                if (flds%gflx(j)%h%active .and. flh) then
                    call update_output_field(fls, shd, flds%gflx(j), flds%dates%h, flds%gflx(j)%h, ih, out%h%grid%gflx(:, j))
                end if
                if (flds%gflx(j)%s%active .and. flm) then
                    if (flds%gflx(j)%apply_frac) then
                        frac = shd%FRAC
                    else
                        frac = 1.0
                    end if
                    flds%gflx(j)%s%dat(:, im) = flds%gflx(j)%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
                    call output_variables_update_values(flds%gflx(j)%s%dat(:, im), out%m%grid%gflx(:, j)*frac, 0, 0, 'sum')
                    flds%gflx(j)%s%dat(:, im) = flds%gflx(j)%s%dat(:, im)/flds%dates%iter_s(im)
                    if (.not. flds%in_mem) then
                        call flush_output(fls, shd, flds%gflx(j), flds%gflx(j)%s, flds%dates%s)
                        if (flds%gflx(j)%s%seq%active) close(flds%gflx(j)%s%seq%iun)
                        if (flds%gflx(j)%s%r2c%active) close(flds%gflx(j)%s%r2c%iun)
                        if (flds%gflx(j)%s%txt%active) close(flds%gflx(j)%s%txt%iun)
                    end if
                end if
            end do
        end if
        if (allocated(flds%tbar)) then
            do j = 1, shd%lc%IGND
!                call update_output_field(fls, shd, flds%tbar(j), out%grid%tbar(j))
                if (flds%tbar(j)%y%active .and. fly) then
                    call update_output_field(fls, shd, flds%tbar(j), flds%dates%y, flds%tbar(j)%y, iy, out%y%grid%tbar(:, j))
                end if
                if (flds%tbar(j)%m%active .and. flm) then
                    call update_output_field(fls, shd, flds%tbar(j), flds%dates%m, flds%tbar(j)%m, im, out%m%grid%tbar(:, j))
                end if
                if (flds%tbar(j)%d%active .and. fldy) then
                    call update_output_field(fls, shd, flds%tbar(j), flds%dates%d, flds%tbar(j)%d, idy, out%d%grid%tbar(:, j))
                end if
                if (flds%tbar(j)%h%active .and. flh) then
                    call update_output_field(fls, shd, flds%tbar(j), flds%dates%h, flds%tbar(j)%h, ih, out%h%grid%tbar(:, j))
                end if
                if (flds%tbar(j)%s%active .and. flm) then
                    if (flds%tbar(j)%apply_frac) then
                        frac = shd%FRAC
                    else
                        frac = 1.0
                    end if
                    flds%tbar(j)%s%dat(:, im) = flds%tbar(j)%s%dat(:, im)*max(flds%dates%iter_s(im) - 1, 1)
                    call output_variables_update_values(flds%tbar(j)%s%dat(:, im), out%m%grid%tbar(:, j)*frac, 0, 0, 'sum')
                    flds%tbar(j)%s%dat(:, im) = flds%tbar(j)%s%dat(:, im)/flds%dates%iter_s(im)
                    if (.not. flds%in_mem) then
                        call flush_output(fls, shd, flds%tbar(j), flds%tbar(j)%s, flds%dates%s)
                        if (flds%tbar(j)%s%seq%active) close(flds%tbar(j)%s%seq%iun)
                        if (flds%tbar(j)%s%r2c%active) close(flds%tbar(j)%s%r2c%iun)
                        if (flds%tbar(j)%s%txt%active) close(flds%tbar(j)%s%txt%iun)
                    end if
                end if
            end do
        end if

        !> ZOD, TMAX, TMIN, ALD.

    end subroutine

    subroutine write_seq(fls, shd, field, file, dates)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_field), intent(in) :: field
        type(output_file), intent(in) :: file
        integer, intent(in) :: dates(:, :)

        !> Local variables.
        integer iun, t, ierr
        logical opened_status

        !> Check if the file is opened.
        iun = file%seq%iun
        inquire(iun, opened = opened_status)

        !> Open the file (if not opened).
        if (.not. opened_status) then
            open( &
                iun, file = file%seq%path, &
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

    subroutine write_r2c(fls, shd, field, file, dates)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_field), intent(in) :: field
        type(output_file), intent(in) :: file
        integer, intent(in) :: dates(:, :)

        !> Local variables.
        integer iun, t, j, i, ierr
        real, dimension(shd%yCount, shd%xCount) :: data_aux
        logical opened_status
        character(len = 10) str10
        character(len = 8) str8

        !> Check if the file is opened.
        iun = file%r2c%iun
        inquire(iun, opened = opened_status)

        !> Open the file (if not opened) and write 'r2c' header.
        if (.not. opened_status) then
            open( &
                iun, file = file%r2c%path, &
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
            write(iun, 3002) ':Name               ', trim(field%name)
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
            write(iun, 3002) ':AttributeName      ', trim(field%name)
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

    subroutine write_txt(fls, shd, field, file, dates)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_field), intent(in) :: field
        type(output_file), intent(in) :: file
        integer, intent(in) :: dates(:, :)

        !> Local variables.
        integer iun, t, j, i, ierr
        real, dimension(shd%yCount, shd%xCount) :: data_aux
        logical opened_status
        character(len = 25) str

        !> Check if the file is opened.
        iun = file%txt%iun
        inquire(iun, opened = opened_status)

        !> Open the file (if not opened).
        if (.not. opened_status) then
            open( &
                iun, file = file%txt%path, &
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
