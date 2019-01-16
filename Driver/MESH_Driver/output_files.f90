module output_files

    use sa_mesh_common
    use model_dates
    use model_files_variables
    use variable_names

    implicit none

    !> Description:
    !>  Keys for output file formats.
    !>      To assign: += radix(key)**key
    !>      To test: btest(val, key)
    type output_file_formats
        integer :: R2C = 2
        integer :: SEQ = 3
        integer :: TXT = 4
        integer :: CSV = 6
        integer :: TSI = 7
        integer :: TSK = 8
    end type

    !> Description:
    !>  Keys for output file scale.
    type output_file_groups
        integer :: GRID = 1
        integer :: TILE = 2
    end type

    !> Description:
    !>  Keys for output file frequency.
    !* STA: Static (once; for fields that do not change in time).
    !* TOT: Accrued over the simulation.
    !* DLY: Daily.
    !* MLY: Monthly.
    !* HLY: Hourly.
    !* PTS: Per time-step.
    !* YLY: Yearly.
    !* SSL: Seasonal.
    type output_file_freqs
        integer :: STA = 0
        integer :: TOT = 1
        integer :: DLY = 2
        integer :: MLY = 3
        integer :: HLY = 4
        integer :: PTS = 5
        integer :: YLY = 6
        integer :: SSL = 7
    end type

    interface write_r2c_grid
        module procedure write_r2c_grid_real
        module procedure write_r2c_grid_int
    end interface

    interface write_seq_frame
        module procedure write_seq_frame_real
        module procedure write_seq_frame_int
    end interface

    !> Description:
    !>  Data type for an output file.
    !>
    !> Variables:
    !*  iun: Unit of the file (must be unique; default: -1).
    !*  fname: Base file name (default: none).
    !*  src: Source data (1: Index).
    !*  dat: Data (1: Index; 2: Time-series index).
    type output_file
        integer :: iun = -1
        character(len = DEFAULT_LINE_LENGTH) :: fname = ''
        real, dimension(:), pointer :: src => null()
        real, dimension(:, :), allocatable :: dat
    end type

    !> Description:
    !>  Data type for a group of output variables (e.g., spatial scale).
    !>
    !> Variables:
    !*  fname: Base file name (default: none).
    !*  tile, grid: Output file for the group.
    type output_group
        character(len = DEFAULT_LINE_LENGTH) :: fname = ''
        type(output_file) tile, grid
    end type

    !> Description:
    !>  Data type for an output field.
    !>
    !> Variables:
    !*  vname: Variable name (default: none).
    !*  ilvl: Variable layer and/or level (default: none).
    !*  cfactorm: Multiplicative transform to apply when updating the field (default: 1.0).
    !*  cfactora: Additive transform to apply when updating the field (default: 0.0).
    !*  fn: Function to use when updating the field (e.g., 'val', 'sum', 'min', 'max'; default: none).
    !*  ffmt: Output file formats (default: none).
    !*  fgroup: Variable groups (default: none).
    !*  ffreq: Output file frequencies (default: none).
    !*  delim: Field delimiter in supported formats (default: space-delimited).
    !*  order: Print order of elements in the field in supported formats (default: 'unknown').
    !*  gru_mask: Function to use when conditioning grid outputs using GRUs (default: '').
    !*  in_mem: .true. to store in memory; .false. to write output continuously during the run (default: .false.).
    !*  apply_frac: .true. to multiply grid values by fractional cell area (default: .true.).
    !*  print_date: Option to print the leading date stamp in supported formats (default: .false.).
    !*  tsi, tsk: Indices of the specific grids and tiles for output in supported formats (text format).
    !*  gru: Indices of GRUs to filter grid based outputs.
    !*  tile, grid, basin: Output data at various scales.
    !*  y, m, s, d, h: Output groups at various output frequencies.
    type output_field
        character(len = DEFAULT_FIELD_LENGTH) :: vname = ''
        integer :: ilvl = 0
        real :: cfactorm = 1.0
        real :: cfactora = 0.0
        character(len = DEFAULT_FIELD_LENGTH) :: fn = ''
        integer :: ffmt = 0
        integer :: fgroup = 0
        integer :: ffreq = 0
        character(len = DEFAULT_FIELD_LENGTH) :: delim = ''
        character(len = DEFAULT_FIELD_LENGTH) :: order = 'unknown'
        character(len = DEFAULT_FIELD_LENGTH) :: gru_mask = ''
        logical :: in_mem = .false.
        logical :: apply_frac = .false.
        logical :: print_date = .true.
        integer, dimension(:), allocatable :: tsi, tsk
        integer, dimension(:), allocatable :: gru
        type(output_group) y, m, s, d, h
    end type

    !> Description:
    !>  Data type for storing dates for various output frequencies.
    !>
    !> Variables:
    !*  y, m, d, h: Dates (1: Index; 2: Time-series index).
    !*  iter_s: Numbers of iterations passed for seasonal output.
    type output_dates
        integer, dimension(:, :), allocatable :: y, m, s, d, h
        integer iter_s(12)
    end type

    !> Description:
    !>  Data type for storing series and variables for output.
    !>
    !> Variables:
    !*  PROCESS_ACTIVE: .true. if output files are enabled.
    !*  ffmt: File format keys.
    !*  fgroup: Output group keys.
    !*  ffreq: Output frequency keys.
    !*  iun: Base file unit (increments as fields are activated for output).
    !*  dates: Array to store frame count and date (1: Frame count and date; 2: Index in series).
    !*  in_mem: .true. to store in memory; .false. to write output continuously during the run (default: .false.).
    !*  fclose: .true. to force writing output, regardless of the state of 'in_mem' (default: .false.).
    !*  fls: Output files.
    type output_fields_container
        logical :: PROCESS_ACTIVE = .false.
        type(output_file_formats) ffmt
        type(output_file_groups) fgroup
        type(output_file_freqs) ffreq
        integer :: iun = 882100
        type(output_dates) dates
        logical :: in_mem = .false.
        logical :: fclose = .false.
        type(output_field), dimension(:), allocatable :: fls
    end type

    !> Instances of data types.
    !>
    !> Variables:
    !*  fls_out: Instance of variables for output.
    type(output_fields_container), save :: fls_out

    !> Description:
    !>  Interface for 'output_files_parse_indices'.
    interface output_files_parse_indices
        module procedure output_files_parse_indices_real
        module procedure output_files_parse_indices_int
    end interface

    !> Description:
    !>  Interface for 'output_files_append_field'.
    interface output_files_append_field
        module procedure output_files_append_field
        module procedure output_files_append_pfield
    end interface

    contains

!>>>>temporarily_here
    !> Description:
    !>  Open an existing text or CSV format file for input (the routine
    !>  implements the 'read' action). Returns 'iostat' of the open
    !>  statement.
    !>
    !> Input variables:
    !*  iun: Unit of the file (stream persists).
    !*  fname: Name of the file.
    !>
    !> Output variables:
    !*  ierr: Returns '0' if the routine is successful.
    subroutine open_txt_input(iun, fname, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: ierr

        !> Open the file.
        !> Print status to the screen if verbose.
        open(iun, file = adjustl(fname), action = 'read', status = 'old', iostat = ierr)

        return

    end subroutine

    !> Description:
    !>  Return the first line of data from a text or CSV format file.
    !>  The routine skips lines that lead with '#' or '!', and clip the
    !>  line to the first instance of '#' or '!' if one exists. The
    !>  returned line is de-tabbed and compacted of excess whitespace.
    !>
    !> Input variables.
    !*  iun: Unit of the file (of persistent stream).
    !>
    !> Output variables.
    !*  line: First data line read from file.
    !*  ierr: Returns '0' if the routine is successful.
    !*  i: Lines passed in file.
    integer function read_txt_line(iun, line, ierr) result(i)

        !> Required for the 'compact' function.
        use strings

        !> Input variables.
        integer, intent(in) :: iun

        !> Output variables.
        character(len = *), intent(out) :: line
        integer, intent(out) :: ierr

        !> Loop until the first line of data is read.
        !> Skip blank lines and lines that lead with '#' or '!'.
        !> Clip the line to the first instance of '#' or '!' if one exists.
        line = ''
        i = 0
        do while (ierr == 0)
            read(iun, '(a)', iostat = ierr) line
            i = i + 1
            if (ierr /= 0) exit
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#' .or. line(1:1) == '!') cycle
            if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
            if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
            call compact(line)
            if (len_trim(line) > 0) exit
        end do

        return

    end function

    subroutine open_r2c_output(fls, shd, iun, fname, attname, attunits, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attname, attunits

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = 10) str10
        character(len = 8) str8

        !> Open the file (write access).
        ierr = 0
        open(iun, file = fname, status = 'replace', form = 'formatted', action = 'write', iostat = ierr)
        if (ierr /= 0) return

        !> Write header.
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
        write(iun, 3002) ':Name               ', trim(attname)
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
        write(iun, 3002) ':AttributeName      ', trim(attname)
        write(iun, 3002) ':AttributeUnits     ', ''
        write(iun, 3005) '#                                       '
        write(iun, 3001) ':xCount             ', shd%xCount
        write(iun, 3001) ':yCount             ', shd%yCount
        write(iun, 3003) ':xDelta             ', shd%xDelta
        write(iun, 3003) ':yDelta             ', shd%yDelta
        write(iun, 3005) '#                                       '
        write(iun, 3005) '#                                       '
        write(iun, 3005) ':endHeader                              '

3001    format((a), i8)
3002    format((a), 3x, (a))
3003    format((a), f16.7)
3004    format((a), 1x, (a))
3005    format((a))
3010    format(a20, a4, '-', a2, '-', a2, 2x, a2, ':', a2)

    end subroutine

    subroutine open_seq_output(fls, iun, fname, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: ierr

        !> Open the file (write access).
        ierr = 0
        open(iun, file = fname, status = 'replace', form = 'unformatted', action = 'write', access = 'sequential', iostat = ierr)

    end subroutine

    subroutine open_txt_output(fls, iun, fname, ierr, cols)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname
        character(len = *), dimension(:), intent(in), optional :: cols

        !> Output variables.
        integer, intent(out) :: ierr

        !> Open the file (write access).
        ierr = 0
        open(iun, file = fname, status = 'replace', form = 'formatted', action = 'write', iostat = ierr)

    end subroutine

    subroutine write_r2c_frame_start(iun, number, year, month, day, hour, mins, ierr)

        !> Input variables.
        integer, intent(in) :: iun, number, year, month, day, hour, mins

        !> Output variables.
        integer, intent(out) :: ierr

        !> Write frame number and time-stamp.
        !> Standard format for 'r2c': "yyyy/MM/dd HH:mm:ss.SSS"
        ierr = 0
        write(iun, "(':Frame', 2i10, 3x, (a), i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ':00.000', (a))", iostat = ierr) &
            number, number, """", year, month, day, hour, mins, """"

    end subroutine

    subroutine write_r2c_frame_end(iun, ierr)

        !> Input variables.
        integer, intent(in) :: iun

        !> Output variables.
        integer, intent(out) :: ierr

        !> Write frame end.
        ierr = 0
        write(iun, "(':EndFrame')", iostat = ierr)

    end subroutine

    subroutine write_r2c_grid_real(iun, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer j, i

        !> Write data.
        do j = 1, size(dat, 1)
            write(iun, *, iostat = ierr) (dat(j, i), i = 1, size(dat, 2))
            if (ierr /= 0) return
        end do

    end subroutine

    subroutine write_r2c_grid_int(iun, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        integer, dimension(:, :), intent(in) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer j, i

        !> Write data.
        do j = 1, size(dat, 1)
            write(iun, *, iostat = ierr) (dat(j, i), i = 1, size(dat, 2))
            if (ierr /= 0) return
        end do

    end subroutine

    subroutine write_seq_frame_real(iun, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        real, dimension(:), intent(in) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Write output.
        ierr = 0
        write(iun, iostat = ierr) dat

    end subroutine

    subroutine write_seq_frame_int(iun, dat, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        integer, dimension(:), intent(in) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Write output.
        ierr = 0
        write(iun, iostat = ierr) dat

    end subroutine

    subroutine output_files_write_r2c(fls, shd, iun, dat, dates, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat
        integer, dimension(:, :), intent(in) :: dates

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, j, i
        real, dimension(shd%yCount, shd%xCount) :: r2c_grid

        !> Write series.
        ierr = 0
        do t = 1, size(dat, 2)

            !> Transfer data to temporary variable.
            r2c_grid = 0.0
            do i = 1, shd%NA
                r2c_grid(shd%yyy(i), shd%xxx(i)) = dat(i, t)
            end do

            !> Write data.
            call write_r2c_frame_start(iun, dates(1, t), dates(2, t), dates(3, t), dates(4, t), dates(5, t), dates(6, t), ierr)
            if (ierr /= 0) return
            call write_r2c_grid(iun, r2c_grid, ierr)
            if (ierr /= 0) return
            call write_r2c_frame_end(iun, ierr)
            if (ierr /= 0) return
        end do

    end subroutine

    subroutine output_files_write_seq(fls, iun, dat, dates, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat
        integer, dimension(:, :), intent(in) :: dates

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, seq_dates(7)

        !> Write series.
        ierr = 0
        do t = 1, size(dat, 2)

            !> Write frame number (N) and time-stamp.
            !> Extended format for 'seq': N N yyyy MM dd HH mm ss SSS
            seq_dates(1) = dates(1, t)
            seq_dates(2) = dates(1, t)
            seq_dates(3) = dates(2, t)
            seq_dates(4) = dates(3, t)
            seq_dates(5) = dates(4, t)
            seq_dates(6) = dates(5, t)
            seq_dates(7) = dates(6, t)
            call write_seq_frame(iun, seq_dates, ierr)
            if (ierr /= 0) return

            !> Write data.
            call write_seq_frame(iun, dat(:, t), ierr)
            if (ierr /= 0) return
        end do

    end subroutine

    subroutine output_files_write_txt(fls, shd, field, iun, dat, dates, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_field), intent(in) :: field
        integer, intent(in) :: iun
        real, dimension(:, :), intent(in) :: dat
        integer, dimension(:, :), intent(in) :: dates

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, j, i
        real dat_grid(shd%yCount*shd%xCount), dat_tsi(size(field%tsi)), dat_tsk(size(field%tsk))
        character(len = DEFAULT_FIELD_LENGTH) fmt

        !> Assign the delimiter.
        select case (field%delim)
            case (',')
                fmt = FMT_CSV
            case default
                fmt = FMT_GEN
        end select

        !> Write series.
        do t = 1, size(dat, 2)

            !> Lead line with date if 'print_date' option is enabled.
            if (field%print_date) then
                ierr = 0
                write(iun, "((a), i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ':00.000', (a))", advance = 'no', iostat = ierr) &
                    """", dates(2:6, t), """" // trim(field%delim)
                if (ierr /= 0) return
            end if

            !> Determine order of cells for output.
            ierr = 0
            select case (field%order)

                !> Write for specific grids.
                case ('tsi')
                    do i = 1, size(field%tsi)
                        if (field%tsi(i) > 0 .and. field%tsi(i) <= size(dat, 1)) then
                            dat_tsi(i) = dat(field%tsi(i), t)
                        else
                            dat_tsi(i) = out%NO_DATA
                        end if
                    end do
                    write(iun, fmt, iostat = ierr) dat_tsi
                    if (ierr /= 0) return

                !> Write for specific tiles.
                case ('tsk')
                    do i = 1, size(field%tsk)
                        if (field%tsk(i) > 0 .and. field%tsk(i) <= size(dat, 1)) then
                            dat_tsk(i) = dat(field%tsk(i), t)
                        else
                            dat_tsk(i) = out%NO_DATA
                        end if
                    end do
                    write(iun, fmt, iostat = ierr) dat_tsk
                    if (ierr /= 0) return

                !> Write by order of 'r2c' grid (e.g., by shd%yCount, then by shd%xCount in a single line).
                case ('shedorder')

                    !> Transfer data to temporary array.
                    dat_grid = 0.0
                    do i = 1, shd%NA
                        dat_grid((shd%yyy(i) - 1)*shd%yCount + shd%xxx(i)) = dat(i, t)
                    end do

                    !> Write data (in a single line).
                    write(iun, fmt, iostat = ierr) dat_grid
                    if (ierr /= 0) return

                !> Write by order of RANK (e.g., 1:shd%NA).
                case default
                    write(iun, fmt, iostat = ierr) dat(:, t)
                    if (ierr /= 0) return
            end select
        end do

    end subroutine
!<<<<<temporarily_here

    subroutine output_files_allocate_group(fls, shd, out_group, field, group, t, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_series), intent(in) :: out_group
        integer, intent(in) :: t

        !> Input/output variables.
        type(output_field) field
        type(output_group) group

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer iun, j, n, z
        character(len = DEFAULT_LINE_LENGTH) fname
        character(len = DEFAULT_FIELD_LENGTH) str
        logical lopen

        !> Create base file name for group.
        fname = trim(group%fname)
        if (field%ilvl > 0) then
            write(str, '(i6)') field%ilvl
            fname = trim(fname) // '_IG' // trim(adjustl(str))
        end if
        ierr = 0

        !> Grid-based.
        if (btest(field%fgroup, fls_out%fgroup%grid)) then

            !> Allocate 'dat' and assign 'src'.
            allocate(group%grid%dat(shd%NA, t))
            group%grid%dat = 0.0
            if (field%ilvl > 0) then
                call output_variables_allocate_pntr( &
                    group%grid%src, out_group%grid, field%vname, shd%NA, shd%lc%IGND, field%ilvl)
            else
                call output_variables_allocate_pntr(group%grid%src, out_group%grid, field%vname, shd%NA)
            end if

            !> File name.
            if (len_trim(field%gru_mask) > 0) then
                n = 0
                select case (field%gru_mask)
                    case ('gru_include')
                        if (size(field%gru) >= 1) n = size(field%gru)
                    case ('gru_exclude')
                        if (size(field%gru) >= 1) n = size(field%gru)
                        fname = trim(fname) // '_not'
                    case default
                        if (size(field%gru) >= 1) n = 1
                end select
                if (n > 0) then
                    do j = 1, n
                        write(str, '(i6)') field%gru(j)
                        fname = trim(fname) // '_GRU' // trim(adjustl(str))
                    end do
                end if
            end if
            group%grid%fname = fname

            !> Assign file unit and open files.
            fls_out%iun = fls_out%iun + 1
            group%grid%iun = fls_out%iun
            iun = 0
            if (btest(field%ffmt, fls_out%ffmt%r2c)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '.r2c', opened = lopen)
                if (.not. lopen) then
                    call open_r2c_output(fls, shd, group%grid%iun + iun, trim(fname) // '.r2c', field%vname, '', z)
                    if (z /= 0) then
                        call print_message_detail('ERROR: Unable to open file for output: ' // trim(fname) // '.r2c')
                        ierr = z
                    end if
                else
                    call print_message_detail( &
                        'ERROR: Another output variable has already opened the file: ' // trim(fname) // '.r2c')
                    z = 1
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, fls_out%ffmt%seq)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '.seq', opened = lopen)
                if (.not. lopen) then
                    call open_seq_output(fls, group%grid%iun + iun, trim(fname) // '.seq', z)
                    if (z /= 0) then
                        call print_message_detail('ERROR: Unable to open file for output: ' // trim(fname) // '.seq')
                        ierr = z
                    end if
                else
                    call print_message_detail( &
                        'ERROR: Another output variable has already opened the file: ' // trim(fname) // '.seq')
                    z = 1
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, fls_out%ffmt%txt)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '.txt', opened = lopen)
                if (.not. lopen) then
                    call open_txt_output(fls, group%grid%iun + iun, trim(fname) // '.txt', z)
                    if (z /= 0) then
                        call print_message_detail('ERROR: Unable to open file for output: ' // trim(fname) // '.txt')
                        ierr = z
                    end if
                else
                    call print_message_detail( &
                        'ERROR: Another output variable has already opened the file: ' // trim(fname) // '.txt')
                    z = 1
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, fls_out%ffmt%csv)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '.csv', opened = lopen)
                if (.not. lopen) then
                    call open_txt_output(fls, group%grid%iun + iun, trim(fname) // '.csv', z)
                    if (z /= 0) then
                        call print_message_detail('ERROR: Unable to open file for output: ' // trim(fname) // '.csv')
                        ierr = z
                    end if
                else
                    call print_message_detail( &
                        'ERROR: Another output variable has already opened the file: ' // trim(fname) // '.csv')
                    z = 1
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, fls_out%ffmt%tsi)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '_GRD.ts', opened = lopen)
                if (.not. lopen) then
                    call open_txt_output(fls, group%grid%iun + iun, trim(fname) // '_GRD.ts', z)
                    if (z /= 0) then
                        call print_message_detail('ERROR: Unable to open file for output: ' // trim(fname) // '_GRD.ts')
                        ierr = z
                    end if
                else
                    call print_message_detail( &
                        'ERROR: Another output variable has already opened the file: ' // trim(fname) // '_GRD.ts')
                    z = 1
                end if
                iun = iun + 1
            end if
            if (z /= 0) ierr = z

            !> Update shared file unit.
            fls_out%iun = fls_out%iun + iun
        end if

        !> Tile-based.
        if (btest(field%fgroup, fls_out%fgroup%tile)) then

            !> Base file unit.

            !> Allocate 'dat' and assign 'src'.
            allocate(group%tile%dat(shd%lc%NML, t))
            group%tile%dat = 0.0
            if (field%ilvl > 0) then
                call output_variables_allocate_pntr( &
                    group%tile%src, out_group%tile, field%vname, shd%lc%NML, shd%lc%IGND, field%ilvl)
            else
                call output_variables_allocate_pntr(group%tile%src, out_group%tile, field%vname, shd%lc%NML)
            end if

            !> File name.
            group%tile%fname = fname

            !> Assign file unit and open files.
            fls_out%iun = fls_out%iun + 1
            group%tile%iun = fls_out%iun
            iun = 0
            if (btest(field%ffmt, fls_out%ffmt%tsk)) then
                z = 0
                lopen = .false.
                inquire(file = trim(fname) // '_NML.ts', opened = lopen)
                if (.not. lopen) then
                    call open_txt_output(fls, group%tile%iun + iun, trim(fname) // '_NML.ts', z)
                    if (z /= 0) then
                        call print_message_detail('ERROR: Unable to open file for output: ' // trim(fname) // '_NML.ts')
                        ierr = z
                    end if
                else
                    call print_message_detail( &
                        'ERROR: Another output variable has already opened the file: ' // trim(fname) // '_NML.ts')
                    z = 1
                end if
                iun = iun + 1
            end if
            if (z /= 0) ierr = z

            !> Update shared file unit.
            fls_out%iun = fls_out%iun + iun
        end if

    end subroutine

    subroutine output_files_allocate_field(fls, shd, ts, field, ierr)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts

        !> Input/output variables.
        type(output_field) field

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, z
        character(len = DEFAULT_LINE_LENGTH) fname

        !> Set 't = 1' for the case when 'in_mem' is not active.
        t = 1

        !> Create base file name for field.
        fname = trim(fls%GENDIR_OUT) // '/' // trim(field%vname)

        !> Allocate data fields.
        ierr = 0
        if (btest(field%ffreq, fls_out%ffreq%yly)) then
            if (field%in_mem) t = ts%nyears
            field%y%fname = trim(fname) // '_Y'
            call output_files_allocate_group(fls, shd, out%y, field, field%y, t, z)
            if (z /= 0) ierr = z
        end if
        if (btest(field%ffreq, fls_out%ffreq%mly)) then
            if (field%in_mem) t = ts%nmonths
            field%m%fname = trim(fname) // '_M'
            call output_files_allocate_group(fls, shd, out%m, field, field%m, t, z)
            if (z /= 0) ierr = z
        end if
        if (btest(field%ffreq, fls_out%ffreq%dly)) then
            if (field%in_mem) t = ts%nr_days
            field%d%fname = trim(fname) // '_D'
            call output_files_allocate_group(fls, shd, out%d, field, field%d, t, z)
            if (z /= 0) ierr = z
        end if
        if (btest(field%ffreq, fls_out%ffreq%hly)) then
            if (field%in_mem) t = ts%nr_days*24
            field%h%fname = trim(fname) // '_H'
            call output_files_allocate_group(fls, shd, out%h, field, field%h, t, z)
            if (z /= 0) ierr = z
        end if

        !> 'Seasonal' must go last because it changes 't' regardless of the state of 'in_mem'.
        if (btest(field%ffreq, fls_out%ffreq%ssl)) then
            t = 12
            field%s%fname = trim(fname) // '_S'
            call output_files_allocate_group(fls, shd, out%m, field, field%s, t, z)
            if (z /= 0) ierr = z
        end if

    end subroutine

    subroutine output_files_parse_indices_real(args, nargs, indices, startindex, ierr)

        !> strings: For 'is_letter' and 'value' functions.
        use strings

        !> Input variables.
        character(len = *), dimension(:), intent(in) :: args
        integer, intent(in) :: nargs, startindex

        !> Output variables.
        real, dimension(:), allocatable, intent(out) :: indices
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
            if (allocated(indices)) deallocate(indices)
            allocate(indices(n))
            do j = 1, n
                call value(args(startindex + j), indices(j), ierr)
            end do
        end if

    end subroutine

    subroutine output_files_parse_indices_int(args, nargs, indices, startindex, ierr)

        !> strings: For 'is_letter' and 'value' functions.
        use strings

        !> Input variables.
        character(len = *), dimension(:), intent(in) :: args
        integer, intent(in) :: nargs, startindex

        !> Output variables.
        integer, dimension(:), allocatable, intent(out) :: indices
        integer, intent(out) :: ierr

        !> Local variables.
        real, dimension(:), allocatable :: r

        !> Call 'real' function.
        call output_files_parse_indices_real(args, nargs, r, startindex, ierr)

        !> Convert 'r' to integer 'indices'.
        if (allocated(indices)) deallocate(indices)
        allocate(indices(size(r)))
        indices = int(r)

    end subroutine

    subroutine output_files_parse_options(fls, shd, ts, field, args, nargs, ierr)

        !> strings: For 'is_letter', 'lowercase', and 'value' functions.
        use strings

        !> Process modules.
        use permafrost_outputs_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts
        character(len = *), intent(in) :: args(:)
        integer, intent(in) :: nargs

        !> Input/output variables.
        type(output_field) field

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer n, j, i
        character(len = DEFAULT_FIELD_LENGTH) str

        !> Mark the field as active and assign default attributes.
        field%ffmt = 0
        field%ffreq = 0
        field%fgroup = 0

        !> Condition output based on 'args' flags.
        ierr = 0
        do i = 2, nargs
            if (.not. is_letter(args(i))) cycle

            !> Parse options.
            str = lowercase(args(i))
            select case (str)

                !> File output frequencies.
                case ('y')
                    if (.not. btest(field%ffreq, fls_out%ffreq%yly)) then
                        field%ffreq = field%ffreq + radix(fls_out%ffreq%yly)**fls_out%ffreq%yly
                    end if
                case ('m')
                    if (.not. btest(field%ffreq, fls_out%ffreq%mly)) then
                        field%ffreq = field%ffreq + radix(fls_out%ffreq%mly)**fls_out%ffreq%mly
                    end if
                case ('s')
                    if (.not. btest(field%ffreq, fls_out%ffreq%mly)) then
                        field%ffreq = field%ffreq + radix(fls_out%ffreq%mly)**fls_out%ffreq%mly
                    end if
                    if (.not. btest(field%ffreq, fls_out%ffreq%ssl)) then
                        field%ffreq = field%ffreq + radix(fls_out%ffreq%ssl)**fls_out%ffreq%ssl
                    end if
                case ('d')
                    if (.not. btest(field%ffreq, fls_out%ffreq%dly)) then
                        field%ffreq = field%ffreq + radix(fls_out%ffreq%dly)**fls_out%ffreq%dly
                    end if
                case ('h')
                    if (.not. btest(field%ffreq, fls_out%ffreq%hly)) then
                        field%ffreq = field%ffreq + radix(fls_out%ffreq%hly)**fls_out%ffreq%hly
                    end if
                case ('ts')
                    if (.not. btest(field%ffreq, fls_out%ffreq%pts)) then
                        field%ffreq = field%ffreq + radix(fls_out%ffreq%pts)**fls_out%ffreq%pts
                    end if

                !> File formats.
                case ('r2c')
                    if (.not. btest(field%ffmt, fls_out%ffmt%r2c)) then
                        field%ffmt = field%ffmt + radix(fls_out%ffmt%r2c)**fls_out%ffmt%r2c
                    end if
                    if (.not. btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%grid)**fls_out%fgroup%grid
                    end if
                case ('seq', 'binseq')
                    if (.not. btest(field%ffmt, fls_out%ffmt%seq)) then
                        field%ffmt = field%ffmt + radix(fls_out%ffmt%seq)**fls_out%ffmt%seq
                    end if
                    if (.not. btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%grid)**fls_out%fgroup%grid
                    end if
                case ('txt')
                    if (.not. btest(field%ffmt, fls_out%ffmt%txt)) then
                        field%ffmt = field%ffmt + radix(fls_out%ffmt%txt)**fls_out%ffmt%txt
                    end if
                    if (.not. btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%grid)**fls_out%fgroup%grid
                    end if
                case ('csv')
                    if (.not. btest(field%ffmt, fls_out%ffmt%csv)) then
                        field%ffmt = field%ffmt + radix(fls_out%ffmt%csv)**fls_out%ffmt%csv
                    end if
                    if (.not. btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%grid)**fls_out%fgroup%grid
                    end if

                !> Order of the selection being saved.
                case ('gridorder', 'shedorder')
                    field%order = adjustl(str)
                    if (.not. btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%grid)**fls_out%fgroup%grid
                    end if

                !> Point outputs for by grid or NML.
                case ('tsi')
                    field%order = adjustl(str)
                    if (.not. btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%grid)**fls_out%fgroup%grid
                    end if
                    if (.not. btest(field%ffmt, fls_out%ffmt%tsi)) then
                        field%ffmt = field%ffmt + radix(fls_out%ffmt%tsi)**fls_out%ffmt%tsi
                    end if
                    call output_files_parse_indices(args, nargs, field%tsi, i, ierr)
                case ('tsk')
                    field%order = adjustl(str)
                    if (.not. btest(field%fgroup, fls_out%fgroup%tile)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%tile)**fls_out%fgroup%tile
                    end if
                    if (.not. btest(field%ffmt, fls_out%ffmt%tsk)) then
                        field%ffmt = field%ffmt + radix(fls_out%ffmt%tsk)**fls_out%ffmt%tsk
                    end if
                    call output_files_parse_indices(args, nargs, field%tsk, i, ierr)

                !> Grid outputs conditioned by GRU.
                case ('gru')
                    field%gru_mask = adjustl(str)
                    if (.not. btest(field%fgroup, fls_out%fgroup%tile)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%tile)**fls_out%fgroup%tile
                    end if
                    if (.not. btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%grid)**fls_out%fgroup%grid
                    end if
                    call output_files_parse_indices(args, nargs, field%gru, i, ierr)
                case ('gru_include')
                    field%gru_mask = adjustl(str)
                    if (.not. btest(field%fgroup, fls_out%fgroup%tile)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%tile)**fls_out%fgroup%tile
                    end if
                    if (.not. btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%grid)**fls_out%fgroup%grid
                    end if
                    call output_files_parse_indices(args, nargs, field%gru, i, ierr)
                case ('gru_exclude')
                    field%gru_mask = adjustl(str)
                    if (.not. btest(field%fgroup, fls_out%fgroup%tile)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%tile)**fls_out%fgroup%tile
                    end if
                    if (.not. btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%fgroup = field%fgroup + radix(fls_out%fgroup%grid)**fls_out%fgroup%grid
                    end if
                    call output_files_parse_indices(args, nargs, field%gru, i, ierr)

                !> Read into memory.
                case ('in_mem')
                    field%in_mem = .true.

                !> Option to apply fractional cell area.
                case ('frac', 'apply_frac')
                    field%apply_frac = .true.

                !> Print leading date-stamp.
                case ('print_date', 'printdate')
                    field%print_date = .true.
                case ('no_date')
                    field%print_date = .false.

                !> Function.
                case ('cum', 'acc', 'avg')
                    call print_remark( &
                        "The option '" // trim(adjustl(args(i))) // "' has been depreciated and has no effect" // &
                        " (Variable '" // trim(field%vname) // "').", PAD_3)
                case ('max', 'min')
                    field%fn = adjustl(str)

                !> Permafrost option (dealt with elsewhere).
                case ('ttol')

                !> Not recognized.
                case default
                    call print_remark( &
                        "The option '" // trim(adjustl(args(i))) // "' is not recognized for output" // &
                        " (Variable '" // trim(field%vname) // "').", PAD_3)
            end select
        end do

        !> Check for 'value' conversion error.
        if (ierr /= 0) then
            call print_warning("Errors occurred parsing options of '" // trim(field%vname) // "'.", PAD_3)
        end if

        !> Validate the configuration.
        if (field%ffmt == 0) then
            call print_warning("No supported output file formats are active for '" // trim(field%vname) // "'.", PAD_3)
        end if
        if (field%ffreq == 0) then
            call print_warning("No supported output file frequencies are active for '" // trim(field%vname) // "'.", PAD_3)
        end if
        if (allocated(field%tsi)) then
            if (size(field%tsi) == 0) then
                call print_warning( &
                    "The 'tsi' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but no grids are listed or an error occurred parsing the values.', PAD_3)
                deallocate(field%tsi)
                field%order = ''
            else if (maxval(field%tsi) > shd%NA .or. minval(field%tsi) < 1) then
                call print_warning( &
                    "The 'tsi' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but contains an invalid grid number' // &
                    ' or exceeds the number of grids identified in the basin.', PAD_3)
            end if
        end if
        if (allocated(field%tsk)) then
            if (size(field%tsk) == 0) then
                call print_warning( &
                    "The 'tsk' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but no tiles are listed or an error occurred parsing the values.', PAD_3)
                deallocate(field%tsk)
                field%order = ''
            else if (maxval(field%tsk) > shd%lc%NML .or. minval(field%tsk) < 1) then
                call print_warning( &
                    "The 'tsk' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but contains an invalid tile number' // &
                    ' or exceeds the number of tiles identified in the basin.', PAD_3)
            end if
        end if
        if (allocated(field%gru)) then
            if (field%gru_mask == 'gru' .and. size(field%gru) > 1) then
                call print_warning( &
                    "The option '" // trim(field%gru_mask) // "' (Variable '" // trim(field%vname) // "')" // &
                    ' supports filtering grid output using 1 GRU at a time.' // &
                    ' Multiple GRUs are listed. Only the first GRU in the list is used.', PAD_3)
            else if (size(field%gru) == 0) then
                call print_warning( &
                    "The '" // trim(field%gru_mask) // "' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but no GRUs are listed or an error occurred parsing the values.', PAD_3)
                deallocate(field%gru)
                field%order = ''
            else if (field%gru(1) > shd%lc%NTYPE .or. field%gru(1) < 1) then
                call print_warning( &
                    "The '" // trim(field%gru_mask) // "' option (Variable '" // trim(field%vname) // "')" // &
                    ' is active but contains an invalid GRU number' // &
                    ' or exceeds the number of GRUs identified in the basin.', PAD_3)
            end if
        end if

        !> Allocate output fields and open output files.
        ierr = 0
        call output_files_allocate_field(fls, shd, ts, field, ierr)

    end subroutine

    subroutine output_files_append_field(fls, shd, ts, vname, args, nargs, ierr, ignd, cfactorm, cfactora, fn)

        !> strings: For 'is_letter', 'lowercase', and 'uppercase' functions.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts
        character(len = *), intent(in) :: vname, args(:)
        integer, intent(in) :: nargs
        integer, intent(in), optional :: ignd
        real, intent(in), optional :: cfactorm, cfactora
        character(len = *), intent(in), optional :: fn

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer n, i
        type(output_field), dimension(:), allocatable :: tmp

        !> Count attributes that require separate files.
        n = 0
        do i = 2, nargs
            if (.not. is_letter(args(i))) cycle
            select case (lowercase(args(i)))
                case ('y', 'm', 's', 'd', 'h', 'ts', 'r2c', 'seq', 'binseq', 'txt', 'csv', 'tsi', 'tsk')
                    n = 1
            end select
        end do

        !> Exit if no files exist to create.
        if (n == 0) then
            call print_warning("'" // trim(args(1)) // "' contains no options that will result in output files.", PAD_3)
            return
        end if

        !> Expand the vector of file attributes.
        if (allocated(fls_out%fls)) then
            allocate(tmp(size(fls_out%fls) + n))
            tmp(1:size(fls_out%fls)) = fls_out%fls
            deallocate(fls_out%fls)
            allocate(fls_out%fls(size(tmp)))
            fls_out%fls = tmp
            deallocate(tmp)
        else
            allocate(fls_out%fls(n))
        end if

        !> Set the index to the last field (just created).
        n = size(fls_out%fls)

        !> Assign the variable name.
        fls_out%fls(n)%vname = adjustl(vname)
        fls_out%fls(n)%ilvl = 0
        if (present(ignd)) then
            if (ignd > 0 .and. ignd <= shd%lc%IGND) fls_out%fls(n)%ilvl = ignd
        end if
        if (present(cfactorm)) fls_out%fls(n)%cfactorm = cfactorm
        if (present(cfactora)) fls_out%fls(n)%cfactora = cfactora
        if (present(fn)) fls_out%fls(n)%fn = adjustl(fn)

        !> Assign inherited options.
        fls_out%fls(n)%in_mem = fls_out%in_mem

        !> Parse and interpret remaining options.
        ierr = 0
        call output_files_parse_options(fls, shd, ts, fls_out%fls(n), args, nargs, ierr)

    end subroutine

    subroutine output_files_append_pfield(fls, shd, ts, vname, pfld, args, nargs, ierr, ignd, cfactorm, cfactora, fn)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts
        character(len = *), intent(in) :: vname, args(:)
        type(output_fields_surrogate) pfld
        integer, intent(in) :: nargs
        integer, intent(in), optional :: ignd
        real, intent(in), optional :: cfactorm, cfactora
        character(len = *), intent(in), optional :: fn

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer n
        type (output_field) field

        !> Call base routine.
        ierr = 0
        call output_files_append_field(fls, shd, ts, vname, args, nargs, ierr, ignd, cfactorm, cfactora, fn)
        if (ierr /= 0) return

        !> Check to see if the field was appended.
        n = size(fls_out%fls)
        if (fls_out%fls(n)%vname /= adjustl(vname)) return
        field = fls_out%fls(n)

        !> Assign 'process_group' fields to 'src' variables.
        if (btest(fls_out%fls(n)%ffreq, fls_out%ffreq%yly)) then
            if (btest(field%fgroup, fls_out%fgroup%tile) .and. associated(pfld%y_tile)) fls_out%fls(n)%y%tile%src => pfld%y_tile
            if (btest(field%fgroup, fls_out%fgroup%grid) .and. associated(pfld%y_grid)) fls_out%fls(n)%y%grid%src => pfld%y_grid
        end if
        if (btest(field%ffreq, fls_out%ffreq%mly)) then
            if (btest(field%fgroup, fls_out%fgroup%tile) .and. associated(pfld%m_tile)) fls_out%fls(n)%m%tile%src => pfld%m_tile
            if (btest(field%fgroup, fls_out%fgroup%grid) .and. associated(pfld%m_grid)) fls_out%fls(n)%m%grid%src => pfld%m_grid
        end if
        if (btest(field%ffreq, fls_out%ffreq%dly)) then
            if (btest(field%fgroup, fls_out%fgroup%tile) .and. associated(pfld%d_tile)) fls_out%fls(n)%d%tile%src => pfld%d_tile
            if (btest(field%fgroup, fls_out%fgroup%grid) .and. associated(pfld%d_grid)) fls_out%fls(n)%d%grid%src => pfld%d_grid
        end if
        if (btest(field%ffreq, fls_out%ffreq%hly)) then
            if (btest(field%fgroup, fls_out%fgroup%tile) .and. associated(pfld%h_tile)) fls_out%fls(n)%h%tile%src => pfld%h_tile
            if (btest(field%fgroup, fls_out%fgroup%grid) .and. associated(pfld%h_grid)) fls_out%fls(n)%h%grid%src => pfld%h_grid
        end if
        if (btest(field%ffreq, fls_out%ffreq%ssl)) then
            if (btest(field%fgroup, fls_out%fgroup%tile) .and. associated(pfld%m_tile)) fls_out%fls(n)%s%tile%src => pfld%m_tile
            if (btest(field%fgroup, fls_out%fgroup%grid) .and. associated(pfld%m_grid)) fls_out%fls(n)%s%grid%src => pfld%m_grid
        end if

    end subroutine

    subroutine output_files_init(fls, shd)

        !> strings: For 'is_letter' function.
        use strings

        !> Process modules.
        use permafrost_outputs_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        type(dates_model) ts
        integer iun, n, i, j, nargs, ios, ierr, z
        character(len = DEFAULT_FIELD_LENGTH) args(50)
        character(len = DEFAULT_LINE_LENGTH) line

        !> Return if not active.
        if (.not. fls_out%PROCESS_ACTIVE) return

        !> Initialize 'ts' variable.
        call GET_DATES(ts)

        !> Allocate output variable for time-stamps based on switch to store variables in-memory.
        if (fls_out%in_mem) then
            allocate(fls_out%dates%y(6, ts%nyears))
            allocate(fls_out%dates%m(6, ts%nmonths))
            allocate(fls_out%dates%d(6, ts%nr_days))
            allocate(fls_out%dates%h(6, ts%nr_days*24))
        else
            allocate(fls_out%dates%y(6, 1))
            allocate(fls_out%dates%m(6, 1))
            allocate(fls_out%dates%d(6, 1))
            allocate(fls_out%dates%h(6, 1))
        end if
        allocate(fls_out%dates%s(6, 12))
        fls_out%dates%iter_s = 0

        !> Open output fields configuration file.
        call print_screen('READING: outputs_balance.txt')
        call print_echo_txt('outputs_balance.txt')
        iun = 100
        call open_txt_input(iun, 'outputs_balance.txt', ierr)
        fls_out%fclose = .false.

        !> Stop if the routine failed.
        if (ierr /= 0) then
            call print_error('Unable to open file.')
            call print_message('Check that outputs_balance.txt exists or disabled OUTFIELDSFLAG.')
            call program_abort()
        end if

        !> Count the number of output files.
        ierr = 0
        ios = 0
        n = 0
        i = 0
        do while (ios == 0)

            !> Read the line and increment the line count.
            i = i + read_txt_line(iun, line, ios)

            !> Check for output field signature: character string followed by at least one space.
            if (len_trim(line) == 0 .or. scan(line, ' ') == 0) cycle
            if (.not. is_letter(line(1:index(line, ' ') - 1))) cycle

            !> Split the line.
            call parse(line, ' ', args, nargs)

            !> Output field.
            if (DIAGNOSEMODE) call print_message_detail('Reading output variable: ' // args(1))
            z = 0
            select case (args(1))

                !> Meteorological forcing.
                case (VN_FSIN, 'FSDOWN')
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_FSIN, args, nargs, z)
                case (VN_FSVH)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_FSVH, args, nargs, z, -1, 0.5)
                case (VN_FSIH)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_FSIH, args, nargs, z, -1, 0.5)
                case (VN_FSDIR)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_FSDIR, args, nargs, z)
                case (VN_FSDFF)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_FSDFF, args, nargs, z)
                case (VN_FLIN, 'FDL')
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_FLIN, args, nargs, z)
                case (VN_TA)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_TA, args, nargs, z)
                case (VN_QA, 'HU')
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_QA, args, nargs, z)
                case (VN_PRES)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_PRES, args, nargs, z)
                case (VN_UU)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_UU, args, nargs, z)
                case (VN_VV)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_VV, args, nargs, z)
                case (VN_UV, 'UL')
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_UV, args, nargs, z)
                case (VN_WDIR)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_WDIR, args, nargs, z)
                case (VN_PRE)
                    if (ro%RUNCLIM) call output_files_append_field(fls, shd, ts, VN_PRE, args, nargs, z)

                !> Water balance.
                case (VN_PREC, 'Rainfall', 'Rain', 'Precipitation')
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_PREC, args, nargs, z)
                case (VN_EVAP, 'Evapotranspiration')
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_EVAP, args, nargs, z, -1, real(ic%dts))
                case (VN_PEVP)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_PEVP, args, nargs, z)
                case (VN_EVPB)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_EVPB, args, nargs, z)
                case (VN_ARRD)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_ARRD, args, nargs, z)
                case (VN_GRO)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_GRO, args, nargs, z)
                case (VN_ROF, 'Runoff')
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_ROF, args, nargs, z, -1, real(ic%dts))
                case (VN_ROFO)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_ROFO, args, nargs, z, -1, real(ic%dts))
                case (VN_ROFS)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_ROFS, args, nargs, z, -1, real(ic%dts))
                case (VN_ROFB)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_ROFB, args, nargs, z, -1, real(ic%dts))
                case (VN_RCAN)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_RCAN, args, nargs, z)
                case (VN_SNCAN, 'SCAN')
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_SNCAN, args, nargs, z)
                case (VN_ZSNO)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_ZSNO, args, nargs, z)
                case (VN_RHOSNO)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_RHOSNO, args, nargs, z)
                case (VN_SNO)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_SNO, args, nargs, z)
                case (VN_FSNO)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_FSNO, args, nargs, z)
                case (VN_WSNO)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_WSNO, args, nargs, z)
                case (VN_ZPND)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_ZPND, args, nargs, z)
                case (VN_PNDW)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_PNDW, args, nargs, z)
                case (VN_LZS)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_LZS, args, nargs, z)
                case (VN_DZS)
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_DZS, args, nargs, z)
                case (VN_THLQ)
                    if (ro%RUNBALWB) then
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, VN_THLQ, args, nargs, z, j)
                        end do
                    end if
                case (VN_LQWS)
                    if (ro%RUNBALWB) then
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, VN_LQWS, args, nargs, z, j)
                        end do
                    end if
                case (VN_THIC)
                    if (ro%RUNBALWB) then
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, VN_THIC, args, nargs, z, j)
                        end do
                    end if
                case (VN_FZWS, 'FRWS')
                    if (ro%RUNBALWB) then
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, VN_FZWS, args, nargs, z, j)
                        end do
                    end if
                case (VN_ALWS)
                    if (ro%RUNBALWB) then
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, VN_ALWS, args, nargs, z, j)
                        end do
                    end if
                case (VN_STGW, 'STG')
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_STGW, args, nargs, z)
                case (VN_DSTGW, 'DSTG')
                    if (ro%RUNBALWB) call output_files_append_field(fls, shd, ts, VN_DSTGW, args, nargs, z)

                !> Energy balance.
                case (VN_CMAS)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_CMAS, args, nargs, z)
                case (VN_TCAN)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_TCAN, args, nargs, z)
                case (VN_TSNO)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_TSNO, args, nargs, z)
                case (VN_TPND)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_TPND, args, nargs, z)
                case (VN_ALVS)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_ALVS, args, nargs, z)
                case (VN_ALIR)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_ALIR, args, nargs, z)
                case (VN_ALBT)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_ALBT, args, nargs, z)
                case (VN_FSOUT)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_FSOUT, args, nargs, z)
                case (VN_FLOUT)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_FLOUT, args, nargs, z)
                case (VN_GTE)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_GTE, args, nargs, z)
                case (VN_QH, 'HFS', 'SensibleHeat')
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_QH, args, nargs, z)
                case (VN_QE, 'QEVP', 'LatentHeat')
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_QE, args, nargs, z)
                case (VN_GZERO)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_GZERO, args, nargs, z)
                case (VN_GFLX, 'HeatConduction')
                    if (ro%RUNBALEB) then
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, VN_GFLX, args, nargs, z, j)
                        end do
                    end if
                case (VN_TBAR, 'TempSoil', 'Temperature_soil_layers')
                    if (ro%RUNBALEB) then
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, VN_TBAR, args, nargs, z, j)
                        end do
                    end if
                case (VN_STGE)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_STGE, args, nargs, z)
                case (VN_DSTGE)
                    if (ro%RUNBALEB) call output_files_append_field(fls, shd, ts, VN_DSTGE, args, nargs, z)

                !> Channels and routing.
                case (VN_RFF, 'WR_RUNOFF')
                    if (ro%RUNCHNL) call output_files_append_field(fls, shd, ts, VN_RFF, args, nargs, z, -1, real(ic%dts))
                case (VN_RCHG, 'WR_RECHARGE')
                    if (ro%RUNCHNL) call output_files_append_field(fls, shd, ts, VN_RCHG, args, nargs, z, -1, real(ic%dts))
                case (VN_QI)
                    if (ro%RUNCHNL) call output_files_append_field(fls, shd, ts, VN_QI, args, nargs, z)
                case (VN_STGCH)
                    if (ro%RUNCHNL) call output_files_append_field(fls, shd, ts, VN_STGCH, args, nargs, z)
                case (VN_QO)
                    if (ro%RUNCHNL) call output_files_append_field(fls, shd, ts, VN_QO, args, nargs, z)
                case (VN_ZLVL)
                    if (ro%RUNCHNL) call output_files_append_field(fls, shd, ts, VN_ZLVL, args, nargs, z)

                !> Permafrost outputs (PERMAFROSTOUTFLAG).
                case (PMFRSTVN_ALD)
                    if (ro%RUNBALEB) then
                        call permafrost_outputs_init(fls, shd, PMFRSTVN_ALD)
                        call output_files_append_field(fls, shd, ts, PMFRSTVN_ALD, prmfst%out%ald, args, nargs, z)
                    end if
                case (PMFRSTVN_ALDDOY, 'ALD_JDAY')
                    if (ro%RUNBALEB) then
                        call permafrost_outputs_init(fls, shd, PMFRSTVN_ALDDOY)
                        call output_files_append_field(fls, shd, ts, PMFRSTVN_ALDDOY, prmfst%out%alddoy, args, nargs, z)
                    end if
                case (PMFRSTVN_ALDENV)
                    if (ro%RUNBALEB) then
                        call permafrost_outputs_init(fls, shd, PMFRSTVN_ALDENV)
                        call output_files_append_field(fls, shd, ts, PMFRSTVN_ALDENV, prmfst%out%aldenv, args, nargs, z)
                    end if
                case (PMFRSTVN_TAVG)
                    if (ro%RUNBALEB) then
                        call permafrost_outputs_init(fls, shd, PMFRSTVN_TAVG)
                        line = trim(VN_TBAR) // '_AVG'
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, line, prmfst%out%tavg(j), args, nargs, z, j)
                        end do
                    end if
                case (PMFRSTVN_TMAX)
                    if (ro%RUNBALEB) then
                        call permafrost_outputs_init(fls, shd, PMFRSTVN_TMAX)
                        line = trim(VN_TBAR) // '_MAX'
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, line, prmfst%out%tmax(j), args, nargs, z, j)
                        end do
                    end if
                case (PMFRSTVN_TMIN)
                    if (ro%RUNBALEB) then
                        call permafrost_outputs_init(fls, shd, PMFRSTVN_TMIN)
                        line = trim(VN_TBAR) // '_MIN'
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, line, prmfst%out%tmin(j), args, nargs, z, j)
                        end do
                    end if
                case (PMFRSTVN_TRNG, 'TENV')
                    if (ro%RUNBALEB) then
                        call permafrost_outputs_init(fls, shd, PMFRSTVN_TRNG)
                        line = trim(VN_TBAR) // '_RNG'
                        do j = 1, shd%lc%IGND
                            call output_files_append_field(fls, shd, ts, line, prmfst%out%trng(j), args, nargs, z, j)
                        end do
                    end if
                case (PMFRSTVN_ZOD)
                    if (ro%RUNBALEB) then

                        !> User-defined temperature threshold(s)/tolerance(s) for ZOD.
                        if (nargs > 1) then
                            do j = 2, nargs
                                if (lowercase(args(j)) == 'ttol') then
                                    if (allocated(prmfst%pm%zod_ttol)) then
                                        call print_message_detail("ERROR:" // &
                                            " Multiple instances of the 'ttol' option exist in outputs_balance.txt" // &
                                            " or a previous entry of 'ZOD' without the 'ttol' option" // &
                                            " has activated the default value ('ttol 0.1')." // &
                                            " Only one instance of the 'ttol' option can exist." // &
                                            " Combine multiple instances 'ttol' into a single option and add it to the first" // &
                                            " entry of 'ZOD' in the list.")
                                        z = 1
                                        exit
                                    else
                                        call output_files_parse_indices(args, nargs, prmfst%pm%zod_ttol, j, z)
                                        exit
                                    end if
                                end if
                            end do
                        end if
                        if (z == 0) then
                            call permafrost_outputs_init(fls, shd, PMFRSTVN_ZOD)
                            do j = 1, size(prmfst%pm%zod_ttol)
                                write(line, FMT_GEN) prmfst%pm%zod_ttol(j)
                                call trimzero(line)
                                line(index(line, '.'):index(line, '.')) = 'p'
                                line = trim(PMFRSTVN_ZOD) // '_TTOL_' // trim(adjustl(line))
                                call output_files_append_field(fls, shd, ts, line, prmfst%out%zod(j), args, nargs, z)
                            end do
                        end if
                    end if

                case default
                    n = n - 1
                    call print_warning("'" // trim(args(1)) // "' is not a recognized variable for output.", PAD_3)
            end select
            n = n + 1

            !> Check for errors.
            if (z /= 0) then
                ierr = z
                write(line, FMT_GEN) i
                call print_message_detail( &
                    "ERROR: Errors occurred while reading '" // trim(args(1)) // "' (Line " // trim(adjustl(line)) // ').')
            end if
        end do
        close(iun)

        !> Stop if errors exist.
        if (ierr /= 0) then
            call print_error('Errors occurred while reading outputs_balance.txt.')
            call program_abort()
        end if

        !> Echo the number of active fields read from file.
        write(line, FMT_GEN) n
        call print_message_detail('Output variables: ' // trim(adjustl(line)))

    end subroutine

    subroutine output_files_update_file(fls, shd, field, group, dates)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        type(output_field), intent(in) :: field
        type(output_group), intent(in) :: group
        integer, dimension(:, :), intent(in) :: dates

        !> Local variables.
        integer iun, z

        !> Grid-based.
        if (btest(field%fgroup, fls_out%fgroup%grid)) then

            !> Write output.
            iun = 0
            if (btest(field%ffmt, fls_out%ffmt%r2c)) then
                z = 0
                call output_files_write_r2c(fls, shd, group%grid%iun + iun, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_message_detail('ERROR: Unable to write to output file: ' // trim(group%grid%fname) // '.r2c')
                    call program_abort()
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, fls_out%ffmt%seq)) then
                z = 0
                call output_files_write_seq(fls, group%grid%iun + iun, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_message_detail('ERROR: Unable to write to output file: ' // trim(group%grid%fname) // '.seq')
                    call program_abort()
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, fls_out%ffmt%txt)) then
                z = 0
                call output_files_write_txt(fls, shd, field, group%grid%iun + iun, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_message_detail('ERROR: Unable to write to output file: ' // trim(group%grid%fname) // '.txt')
                    call program_abort()
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, fls_out%ffmt%csv)) then
                z = 0
                call output_files_write_txt(fls, shd, field, group%grid%iun + iun, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_message_detail('ERROR: Unable to write to output file: ' // trim(group%grid%fname) // '.csv')
                    call program_abort()
                end if
                iun = iun + 1
            end if
            if (btest(field%ffmt, fls_out%ffmt%tsi)) then
                z = 0
                call output_files_write_txt(fls, shd, field, group%grid%iun + iun, group%grid%dat, dates, z)
                if (z /= 0) then
                    call print_message_detail('ERROR: Unable to write to output file: ' // trim(group%grid%fname) // '_GRD.ts')
                    call program_abort()
                end if
                iun = iun + 1
            end if
        end if

        !> Tile-based.
        if (btest(field%fgroup, fls_out%fgroup%tile)) then

            !> Write output.
            iun = 0
            if (btest(field%ffmt, fls_out%ffmt%tsk)) then
                z = 0
                call output_files_write_txt(fls, shd, field, group%tile%iun + iun, group%tile%dat, dates, z)
                if (z /= 0) then
                    call print_message_detail('ERROR: Unable to write to output file: ' // trim(group%tile%fname) // '_NML.ts')
                    call program_abort()
                end if
                iun = iun + 1
            end if
        end if

    end subroutine

    subroutine output_files_filter_group(fls, shd, field, group, t)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: t

        !> Input/output variables.
        type(output_field) field
        type(output_group) group

        !> Local variables.
        integer k
        real frac(shd%NA)

        !> Return if either of the 'tile' or 'grid' groups are not activated.
        !> Return if the 'gru' attributes of the field is not allocated.
        if (.not. (btest(field%fgroup, fls_out%fgroup%tile) .and. btest(field%fgroup, fls_out%fgroup%grid)) .or. &
            .not. allocated(field%gru)) return

        !> Filter grid outputs by GRU (requires pulling from tile output variables).
        group%grid%dat = 0.0
        select case (field%gru_mask)
            case ('gru_include')

                !> Include only the GRUs in the list.
                frac = 0.0
                do k = 1, shd%lc%NML
                    if (group%tile%dat(k, t) /= out%NO_DATA .and. any(field%gru == shd%lc%JLMOS(k))) then
                        group%grid%dat(shd%lc%ILMOS(k), t) = group%grid%dat(shd%lc%ILMOS(k), t) + &
                            group%tile%dat(k, t)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                        frac(shd%lc%ILMOS(k)) = frac(shd%lc%ILMOS(k)) + shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                    end if
                end do
                where (frac > 0.0)
                    group%grid%dat(:, t) = group%grid%dat(:, t)/frac
                elsewhere
                    group%grid%dat(:, t) = out%NO_DATA
                end where
            case ('gru_exclude')

                !> Exclude only the GRUs in the list.
                frac = 0.0
                do k = 1, shd%lc%NML
                    if (group%tile%dat(k, t) /= out%NO_DATA .and. .not. any(field%gru == shd%lc%JLMOS(k))) then
                        group%grid%dat(shd%lc%ILMOS(k), t) = group%grid%dat(shd%lc%ILMOS(k), t) + &
                            group%tile%dat(k, t)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                        frac(shd%lc%ILMOS(k)) = frac(shd%lc%ILMOS(k)) + shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))
                    end if
                end do
                where (frac > 0.0)
                    group%grid%dat(:, t) = group%grid%dat(:, t)/frac
                elsewhere
                    group%grid%dat(:, t) = out%NO_DATA
                end where
            case default

                !> Only the specified GRU.
                do k = 1, shd%lc%NML
                    if (field%gru(1) == shd%lc%JLMOS(k)) group%grid%dat(shd%lc%ILMOS(k), t) = group%tile%dat(k, t)
                end do
        end select

    end subroutine

    !> Description:
    !>  Update the value using transforms (if provided).
    subroutine output_files_update_dat(fls, shd, field, file, t, cfactorm, cfactora, fn)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: t
        real, intent(in) :: cfactorm, cfactora
        character(len = *), intent(in) :: fn

        !> Input/output variables.
        type(output_field) field
        type(output_file) file

        !> Set 'NO_DATA' value is no 'src' is defined.
        if (.not. associated(file%src)) then
            file%dat(:, t) = out%NO_DATA
            return
        end if

        !> Apply transforms and update values.
        select case (fn)
            case ('max')
                file%dat(:, t) = max(file%dat(:, t), (cfactorm*file%src + cfactora))
            case ('min')
                file%dat(:, t) = min(file%dat(:, t), (cfactorm*file%src + cfactora))
            case ('sum')
                file%dat(:, t) = file%dat(:, t) + (cfactorm*file%src + cfactora)
            case default
                file%dat(:, t) = (cfactorm*file%src + cfactora)
        end select

    end subroutine

    subroutine output_files_update_group(fls, shd, field, group, t, cfactorm, cfactora, fn)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: t
        real, intent(in) :: cfactorm, cfactora
        character(len = *), intent(in) :: fn

        !> Input/output variables.
        type(output_field) field
        type(output_group) group

        !> Local variables.
        real frac(shd%NA)

        !> Update tile variables.
        if (btest(field%fgroup, fls_out%fgroup%tile)) then
            call output_files_update_dat(fls, shd, field, group%tile, t, cfactorm, cfactora, fn)
        end if

        !> Update grid variables.
        if (btest(field%fgroup, fls_out%fgroup%grid)) then
            call output_files_update_dat(fls, shd, field, group%grid, t, cfactorm, cfactora, fn)

            !> Filter grid outputs by GRU (requires pulling from tile output variables).
            if (allocated(field%gru)) call output_files_filter_group(fls, shd, field, group, t)

            !> Apply frac.
            if (field%apply_frac) group%grid%dat(:, t) = group%grid%dat(:, t)*shd%FRAC
        end if

    end subroutine

    !> Description:
    !>  Update the 'dates' variable from the 'ic' counter.
    subroutine output_files_update_dates(dates, t, iter, year, month, day, hour, mins)

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

    subroutine output_files_update_field(fls, shd, field)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_field) field

        !> Local variables.
        integer t, ierr
        real s_grid(shd%NA), s_tile(shd%lc%NML)

        !> Set 't = 1' for the case when 'in_mem' is not active.
        t = 1

        !> Update data fields.
        if (btest(field%ffreq, fls_out%ffreq%yly)) then
            if (ic%now%year /= ic%next%year) then
                if (field%in_mem) t = ic%iter%year
                call output_files_update_dates(fls_out%dates%y, t, ic%iter%year, ic%now%year, 1, 1)
                call output_files_update_group(fls, shd, field, field%y, t, field%cfactorm, field%cfactora, field%fn)
            end if
            if ((ic%now%year /= ic%next%year .and. .not. field%in_mem) .or. (field%in_mem .and. fls_out%fclose)) then
                call output_files_update_file(fls, shd, field, field%y, fls_out%dates%y)
            end if
        end if
        if (btest(field%ffreq, fls_out%ffreq%mly)) then
            if (ic%now%month /= ic%next%month) then
                if (field%in_mem) t = ic%iter%month
                call output_files_update_dates(fls_out%dates%m, t, ic%iter%month, ic%now%year, ic%now%month, 1)
                call output_files_update_group(fls, shd, field, field%m, t, field%cfactorm, field%cfactora, field%fn)
            end if
            if ((ic%now%month /= ic%next%month .and. .not. field%in_mem) .or. (field%in_mem .and. fls_out%fclose)) then
                call output_files_update_file(fls, shd, field, field%m, fls_out%dates%m)
            end if
        end if
        if (btest(field%ffreq, fls_out%ffreq%dly)) then
            if (ic%now%day /= ic%next%day) then
                if (field%in_mem) t = ic%iter%day
                call output_files_update_dates(fls_out%dates%d, t, ic%iter%day, ic%now%year, ic%now%month, ic%now%day)
                call output_files_update_group(fls, shd, field, field%d, t, field%cfactorm, field%cfactora, field%fn)
            end if
            if ((ic%now%day /= ic%next%day .and. .not. field%in_mem) .or. (field%in_mem .and. fls_out%fclose)) then
                call output_files_update_file(fls, shd, field, field%d, fls_out%dates%d)
            end if
        end if
        if (btest(field%ffreq, fls_out%ffreq%hly)) then
            if (ic%now%hour /= ic%next%hour) then
                if (field%in_mem) t = ic%iter%hour
                call output_files_update_dates(fls_out%dates%h, t, ic%iter%hour, ic%now%year, ic%now%month, ic%now%day, ic%now%hour)
                call output_files_update_group(fls, shd, field, field%h, t, field%cfactorm, field%cfactora, field%fn)
            end if
            if ((ic%now%hour /= ic%next%hour .and. .not. field%in_mem) .or. (field%in_mem .and. fls_out%fclose)) then
                call output_files_update_file(fls, shd, field, field%h, fls_out%dates%h)
            end if
        end if

        !> 'Seasonal' must go last because it changes 't' regardless of the state of 'in_mem'.
        if (btest(field%ffreq, fls_out%ffreq%ssl)) then
            if (ic%now%month /= ic%next%month) then
                t = ic%now%month
                call output_files_update_dates(fls_out%dates%s, t, t, ic%now%year, ic%now%month, 1)
                call output_files_update_group(fls, shd, field, field%s, t, field%cfactorm, field%cfactora, 'sum')
            end if
            if (fls_out%fclose) then

                !> Calculate average values.
                do t = 1, size(fls_out%dates%s, 2)
                    if (btest(field%fgroup, fls_out%fgroup%grid)) then
                        field%s%grid%dat(:, t) = field%s%grid%dat(:, t)/fls_out%dates%iter_s(t)
                    end if
                    if (btest(field%fgroup, fls_out%fgroup%tile)) then
                        field%s%tile%dat(:, t) = field%s%tile%dat(:, t)/fls_out%dates%iter_s(t)
                    end if
                end do
                call output_files_update_file(fls, shd, field, field%s, fls_out%dates%s)
            end if
        end if

    end subroutine

    subroutine output_files_update(fls, shd)

        !> Process modules.
        use permafrost_outputs_module

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Local variables.
        integer i

        !> Return if not active.
        if (.not. fls_out%PROCESS_ACTIVE) return

        !> Update counter for seasonal output.
        if (ic%now%month /= ic%next%month) fls_out%dates%iter_s(ic%now%month) = fls_out%dates%iter_s(ic%now%month) + 1

        !> Update external outputs.
        call permafrost_outputs_update(fls, shd)

        !> Update fields and output files.
        do i = 1, size(fls_out%fls)
            if (fls_out%fls(i)%ffreq /= fls_out%ffreq%sta) call output_files_update_field(fls, shd, fls_out%fls(i))
        end do

    end subroutine

    subroutine output_files_finalize(fls, shd)

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Return if not active.
        if (.not. fls_out%PROCESS_ACTIVE) return

        !> Write final outputs.
        fls_out%fclose = .true.
        call output_files_update(fls, shd)

    end subroutine

end module
