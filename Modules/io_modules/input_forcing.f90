module input_forcing

    !> 'mesh_io': For I/O field and file types, options and routines.
    use mesh_io

    implicit none

    !* forcing_files: List of forcing files (of type 'io_file', private).
    type(io_file), dimension(:), allocatable, private, save :: forcing_files

    !* forcing_file_start_date_override: File start date read from other configuration files (override).
    type(io_datetime), save :: forcing_file_start_date_override

    !* forcing_file_hourly_flag_override: Record length read from other configuration files.
    integer, save :: forcing_file_hourly_flag_override = 30

    !* forcing_file_preset_options: Options read from 'BASINFORCINGFLAG' to apply to other fields (private).
    character(len = :), allocatable, private, save :: forcing_file_preset_options

    !* forcing_file_temporal_interpolation: Global flag to activate temporal interpolation.
    integer, save :: forcing_file_temporal_interpolation = 0

    contains

    subroutine expand_forcing_files_list(error_status)

        !> Input/output variables.
        integer, intent(out) :: error_status

        !> Local variables.
        type(io_file), dimension(:), allocatable :: temp_list
        integer, parameter :: increment = 1

        !> Status.
        error_status = 0

        !> Transfer fields.
        if (.not. allocated(forcing_files)) then

            !> No need to transfer if 'forcing_files' is not allocated.
            allocate(forcing_files(increment))
        else
            !> Allocate temporary field.
            allocate(temp_list(size(forcing_files) + increment))

            !> Transfer fields.
            temp_list(1:size(forcing_files)) = forcing_files

            !> Deallocate source array.
            deallocate(forcing_files)

            !> Reallocate source array and transfer fields.
            call move_alloc(from = temp_list, to = forcing_files)
        end if

    end subroutine

    recursive subroutine parse_basinforcingflag(basinforcingflag, quiet, error_status)

        !> Modules.
        use parse_utilities, only: parse_line_values
        use date_utilities, only: date_to_jday
        use strings, only: lowercase
        use variable_names

        !> Input/output variables.
        character(len = *), intent(in) :: basinforcingflag
        logical, intent(in), optional :: quiet
        integer, intent(out) :: error_status

        !> Local variables.
        type(io_field), allocatable :: field_list(:)
        character(len = :), allocatable :: base_file_name, input_flag, flag_name, file_label
        character(len = SHORT_FIELD_LENGTH) :: field_map(1,2), code
        character(len = SHORT_FIELD_LENGTH), allocatable :: dim_names(:), values(:)
        integer j, i, block_interval
        logical v, no_clim_flag

        !> Check verbosity (override if 'DIAGNOSEMODE' is enabled).
        v = .true.
        if (present(quiet)) v = .not. quiet
        if (DIAGNOSEMODE) v = .true.

        !> Transfer the input forcing flag to a temporary variables.
        input_flag = trim(basinforcingflag)

        !> Append any pre-existing options.
        if (allocated(forcing_file_preset_options)) input_flag = input_flag // ' ' // forcing_file_preset_options

        !> Split the string.
        call parse_line_values(input_flag, values = values, error_status = error_status)
        if (error_status /= 0) then
            if (v) call print_remark("An error occurred parsing the options of the flag.")
            return
        end if

        !> Return if no arguments exist.
        if (.not. size(values) > 1) then
            if (v) call print_remark("The flag does not contain any space-delimited fields.")
            return
        else
            flag_name = trim(values(1))
        end if

        !> Create variable map where provided one of the variable-override flags.
        base_file_name = 'basin_forcing'
        field_map = ''
        select case (flag_name)
            case ('BASINFORCINGFLAG')

                !> Save options to apply to other fields.
                forcing_file_preset_options = input_flag((len(flag_name) + 1):)
                if (index(input_flag, ' met') > 0) then

                    !> Continue to read the flag if a CLASS 'MET' format file.
                    field_map(1, 2) = 'CLASSMET'
                else

                    !> Call other flags recursively (if not disabled with 'no_clim').
                    if (index(input_flag, ' no_clim') == 0) then
                        call parse_basinforcingflag('BASINSHORTWAVEFLAG', quiet, error_status)
                        call parse_basinforcingflag('BASINLONGWAVEFLAG', quiet, error_status)
                        call parse_basinforcingflag('BASINRAINFLAG', quiet, error_status)
                        call parse_basinforcingflag('BASINTEMPERATUREFLAG', quiet, error_status)
                        call parse_basinforcingflag('BASINWINDFLAG', quiet, error_status)
                        call parse_basinforcingflag('BASINPRESFLAG', quiet, error_status)
                        call parse_basinforcingflag('BASINHUMIDITYFLAG', quiet, error_status)
                    end if

                    !> Return since the options are saved for other flags.
                    return
                end if
            case ('BASINSHORTWAVEFLAG')
                base_file_name = 'basin_shortwave'
                field_map(1, 1) = VN_FSIN
                field_map(1, 2) = VN_FSIN
            case ('BASINLONGWAVEFLAG')
                base_file_name = 'basin_longwave'
                field_map(1, 1) = VN_FLIN
                field_map(1, 2) = VN_FLIN
            case ('BASINTOTPRECFLAG', 'BASINRAINFLAG')
                base_file_name = 'basin_rain'
                field_map(1, 1) = VN_PRE
                field_map(1, 2) = VN_PRE
            case ('BASINLIQUIDPRECFLAG')
                base_file_name = 'basin_liquid_precip'
                field_map(1, 1) = VN_PRERN
                field_map(1, 2) = VN_PRERN
            case ('BASINSOLIDPRECFLAG')
                base_file_name = 'basin_solid_precip'
                field_map(1, 1) = VN_PRESNO
                field_map(1, 2) = VN_PRESNO
            case ('BASINTEMPERATUREFLAG')
                base_file_name = 'basin_temperature'
                field_map(1, 1) = VN_TA
                field_map(1, 2) = VN_TA
            case ('BASINUWINDFLAG')
                base_file_name = 'basin_uwind'
                field_map(1, 1) = VN_UU
                field_map(1, 2) = VN_UU
            case ('BASINVWINDFLAG')
                base_file_name = 'basin_vwind'
                field_map(1, 1) = VN_VV
                field_map(1, 2) = VN_VV
            case ('BASINWINDFLAG')
                base_file_name = 'basin_wind'
                field_map(1, 1) = VN_UV
                field_map(1, 2) = VN_UV
            case ('BASINWINDDIRFLAG')
                base_file_name = 'basin_winddir'
                field_map(1, 1) = VN_WDIR
                field_map(1, 2) = VN_WDIR
            case ('BASINPRESFLAG')
                base_file_name = 'basin_pres'
                field_map(1, 1) = VN_PRES
                field_map(1, 2) = VN_PRES
            case ('BASINHUMIDITYFLAG')
                base_file_name = 'basin_humidity'
                field_map(1, 1) = VN_QA
                field_map(1, 2) = VN_QA
            case ('BASINRUNOFFFLAG')
                base_file_name = 'WR_runoff'
                field_map(1, 1) = VN_RFF
                field_map(1, 2) = VN_RFF
            case ('BASINRECHARGEFLAG')
                base_file_name = 'WR_recharge'
                field_map(1, 1) = VN_RCHG
                field_map(1, 2) = VN_RCHG
            case default
                call print_error( &
                    "Unrecognized control flag '" // flag_name // "' or the flag is not associated with a forcing file.")
                error_status = 1
                return
        end select

        !> Identify the file type and assign the file label.
        file_label = trim(field_map(1, 2)) // '_'
        block_interval = 1
        do j = 2, size(values)

            !> Determine the file format by the old-style numeric format or from recognized word options.
            select case (values(j))
                case ('1', 'r2c')
                    file_label = file_label // 'r2c'
                    exit
                case ('2', 'csv')
                    file_label = file_label // 'csv'
                    exit
                case ('3', 'seq', 'bin_seq')
                    file_label = file_label // 'seq'
                    exit
                case ('4', 'asc')
                    file_label = file_label // 'asc'
                    exit
                case ('5')
                    if (size(values) >= j + 2) then

                        !> Store the block count and cycle to read the format (and initialize the file).
                        read(values(j + 2), *, iostat = error_status) block_interval
                        if (error_status /= 0) then
                            block_interval = 1
                        else if (v) then
                            call print_warning( &
                                "An error occurred parsing the second option ('nts') of file format '5' of the '" // flag_name // &
                                "' control flag.")
                            error_status = 0
                        end if

                        !> Change the inline value to skip the argument in subsequent cycles.
                        values(j + 2) = ''
                    end if
                case ('6', 'met')
                    file_label = file_label // 'met'
                    exit
                case ('7', 'nc', 'nc_subbasin', 'nc_hru')
                    file_label = file_label // 'nc'
                    exit
            end select
        end do

        !> Check for errors.
        if (len(file_label) == len_trim(field_map(1, 2)) + 1) then
            call print_error("The '" // flag_name // "' control flag is not recognized or specifies an unsupported file format.")
            error_status = 1
            return
        else

            !> Check if the file already exists.
            if (allocated(forcing_files)) then
                i = 0
                do j = 1, size(forcing_files)
                    if (forcing_files(j)%label == file_label) then
                        i = j
                        exit
                    end if
                end do
                if (i == 0) i = size(forcing_files) + 1
            else
                i = 1
            end if

            !> Or else register the file.
            if (.not. allocated(forcing_files) .or. i > size(forcing_files)) then

                !> Expand the list of forcing files.
                call expand_forcing_files_list(error_status)

                !> Specify the file type/container.
                select case (file_label((index(file_label, '_') + 1):))
                    case ('r2c')

                        !> ASCII R2C format.
                        allocate(forcing_files(i)%container, source = io_type_r2c())
                    case ('csv')

                        !> CSV format.
                        allocate(dim_names(2))
                        dim_names = (/DIM_NAME_M, DIM_NAME_T/)
                        allocate(forcing_files(i)%container, source = io_type_char_delimited(delimiter = ','))
                    case ('seq')

                        !> Rank-ordered binary sequential format.
                        allocate(dim_names(2))
                        dim_names = (/DIM_NAME_N, DIM_NAME_T/)
                        allocate(forcing_files(i)%container, source = io_type_seq())
                    case ('asc')

                        !> Rank-ordered text (ASCII) format.
                        allocate(dim_names(2))
                        dim_names = (/DIM_NAME_N, DIM_NAME_T/)
                        allocate(forcing_files(i)%container, source = io_type_char_delimited())
                    case ('met')

                        !> CLASS 'MET' file.
                        allocate(dim_names(1))
                        dim_names = (/DIM_NAME_T/)
                        allocate(forcing_files(i)%container, source = io_type_met())
                    case ('nc')

                        !> netCDF format.
                        allocate(forcing_files(i)%container, source = io_type_nc())
                    case default

                        !> Unknown or unsupported format.
                        error_status = 1
                end select

                !> Transfer field map.
                allocate(forcing_files(i)%field_map(1, 2), source = field_map)

                !> Assign other attributes.
                forcing_files(i)%label = file_label
                forcing_files(i)%full_path = base_file_name // '.' // file_label((index(file_label, '_') + 1):)
                forcing_files(i)%series%multi_frame = .true.
                forcing_files(i)%series%freq = FREQ_MINUTES
                forcing_files(i)%series%freq_interval = forcing_file_hourly_flag_override
                forcing_files(i)%series%block_interval = block_interval
                if (allocated(dim_names)) then
                    allocate(forcing_files(i)%overrides%dim_names(size(dim_names)), source = dim_names)
                    deallocate(dim_names)
                end if
            end if
        end if

        !> Check for errors.
        if (error_status /= 0) then
            call print_error("A file could not be associated with the '" // flag_name // "' control flag.")
            return
        end if

        !> Parse the other options to update the file definition.
        no_clim_flag = .false.
        do j = 2, size(values)
            if (values(j)(1:3) == 'hf=') then

                !> Frame length/file time-stepping.
                read(values(j)(4:), *, iostat = error_status) forcing_files(i)%series%freq_interval
            else if (values(j)(1:11) == 'start_date=') then

                !> First date of record.
                if (len_trim(values(j)) >= 15) then
                    read(values(j)(12:15), *, iostat = error_status) forcing_files(i)%series%start%year
                    if (error_status == 0) then
                        if (len_trim(values(j)) >= 17) then
                            read(values(j)(16:17), *, iostat = error_status) forcing_files(i)%series%start%month
                            if (error_status == 0) then
                                if (len_trim(values(j)) >= 19) then
                                    read(values(j)(18:19), *, iostat = error_status) forcing_files(i)%series%start%day
                                    if (error_status == 0) then
                                        if (len_trim(values(j)) >= 21) then
                                            read(values(j)(20:21), *, iostat = error_status) forcing_files(i)%series%start%hour
                                            if (error_status == 0) then
                                                if (len_trim(values(j)) >= 23) then
                                                    read(values(j)(22:23), *, iostat = error_status) &
                                                        forcing_files(i)%series%start%minutes
                                                else
                                                    forcing_files(i)%series%start%minutes = 0
                                                end if
                                            end if
                                        else
                                            forcing_files(i)%series%start%hour = 0
                                        end if
                                    end if
                                else
                                    forcing_files(i)%series%start%day = 1
                                end if
                                forcing_files(i)%series%start%jday = date_to_jday( &
                                        forcing_files(i)%series%start%year, forcing_files(i)%series%start%month, &
                                        forcing_files(i)%series%start%day)
                            end if
                        else
                            forcing_files(i)%series%start%month = 1
                        end if
                    end if
                end if
            else if (values(j)(1:4) == 'nts=') then

                !> Number of frames to read in to memory.
                read(values(j)(5:), *, iostat = error_status) forcing_files(i)%series%block_interval
            else if (values(j)(1:6) == 'fname=') then

                !> Base file name (without extension).
                forcing_files(i)%full_path = trim(values(j)(7:)) // &
                    forcing_files(i)%full_path(index(forcing_files(i)%full_path, '.', back = .true.):)
            else if (values(j)(1:6) == 'fpath=') then

                !> Full path including file name and extension.
                forcing_files(i)%full_path = trim(values(j)(7:))
            else if (values(j)(1:9) == 'name_var=') then

                !> Variable name.
                forcing_files(i)%field_map(1, 1) = trim(values(j)(10:))
            else if (values(j)(1:9) == 'name_lat=') then

                !> Name of latitude dimension (for specific formats).
                select type (file_type => forcing_files(i)%container)
                    type is (io_type_nc)
                        file_type%dim_name_y = trim(values(j)(10:))
                    class default
                        call print_warning( &
                            "The option 'name_lat=' on '" // flag_name // "' has no effect on '" // &
                            trim(forcing_files(i)%full_path) // "'.")
                end select
            else if (values(j)(1:9) == 'name_lon=') then

                !> Name of longitude dimension (for specific formats).
                select type (file_type => forcing_files(i)%container)
                    type is (io_type_nc)
                        file_type%dim_name_x = trim(values(j)(10:))
                    class default
                        call print_warning( &
                            "The option 'name_lon=' on '" // flag_name // "' has no effect on '" // &
                            trim(forcing_files(i)%full_path) // "'.")
                end select
            else if (values(j)(1:10) == 'name_time=') then

                !> Name of time dimension (for specific formats).
                select type (file_type => forcing_files(i)%container)
                    type is (io_type_nc)
                        file_type%dim_name_t = trim(values(j)(11:))
                    class default
                        call print_warning( &
                            "The option 'name_time=' on '" // flag_name // "' has no effect on '" // &
                            trim(forcing_files(i)%full_path) // "'.")
                end select
            else if (values(j)(1:3) == 'cm=') then

                !> Data multiplier.
                read(values(j)(4:), *, iostat = error_status) forcing_files(i)%overrides%const_mul
            else if (values(j)(1:3) == 'ca=') then

                !> Data additive factor.
                read(values(j)(4:), *, iostat = error_status) forcing_files(i)%overrides%const_add
            else if (values(j)(1:12) == 'n_skip_cols=') then

                !> Number of leading columns (to skip).
                select type (file_type => forcing_files(i)%container)
                    type is (io_type_char_delimited)
                        read(values(j)(13:), *, iostat = error_status) file_type%n_skip_cols
                    class default
                        call print_warning( &
                            "The option 'n_skip_cols=' on '" // flag_name // "' has no effect on '" // &
                            trim(forcing_files(i)%full_path) // "'.")
                end select
            else if (values(j)(1:12) == 'n_skip_rows=') then

                !> Number of leading rows (to skip).
                select type (file_type => forcing_files(i)%container)
                    type is (io_type_char_delimited)
                        read(values(j)(13:), *, iostat = error_status) file_type%n_skip_rows
                    class default
                        call print_warning( &
                            "The option 'n_skip_rows=' on '" // flag_name // "' has no effect on '" // &
                            trim(forcing_files(i)%full_path) // "'.")
                end select
            else if (values(j)(1:11) == 'time_shift=') then

                !> Time shift to apply to time-stamps (for specific formats).
                read(values(j)(12:), *, iostat = error_status) forcing_files(i)%series%time_offset
            else if (values(j) == 'rr_sr') then

                !> Separate liquid/solid precipitation fields (for CLASS 'MET' format).
                select type (file_type => forcing_files(i)%container)
                    type is (io_type_met)
                        file_type%rr_sr = .true.
                    class default
                        call print_warning( &
                            "The option 'rr_sr=' on '" // flag_name // "' has no effect on '" // &
                            trim(forcing_files(i)%full_path) // "'.")
                end select
            else if (values(j) == 'no_clim') then

                !> Deactivate climate variables.
                no_clim_flag = .true.
            end if

            !> Check for errors.
            if (error_status /= 0) then
                write(code, FMT_GEN) j
                if (v) call print_warning( &
                    "An error occurred parsing argument " // trim(adjustl(code)) // " '" // trim(values(j)) // "' of the '" // &
                    flag_name // "' control flag.")
                error_status = 0
            end if
        end do

        !> Add fields (for formats that will not read them from file).
        if (.not. allocated(forcing_files(i)%fields)) then
            select type (file_type => forcing_files(i)%container)
                type is (io_type_met)

                    !> Allocate temporary variables.
                    if (file_type%rr_sr) then
                        allocate(field_list(9))
                    else
                        allocate(field_list(7))
                    end if

                    !> Add the standard fields of the CLASS 'MET' file.
                    field_list(1)%label = VN_FSIN
                    field_list(2)%label = VN_FLIN
                    field_list(3)%label = VN_PRE
                    field_list(4)%label = VN_TA
                    allocate(field_list(4)%field, source = model_variable_real1d(dat = null(), const_add = 273.16))
                    field_list(5)%label = VN_QA
                    field_list(6)%label = VN_UV
                    field_list(7)%label = VN_PRES

                    !> Add the extended fields by the 'sr_rr' option.
                    if (file_type%rr_sr) then
                        field_list(8)%label = VN_PRERN
                        allocate(field_list(8)%field, source = model_variable_real1d(dat = null()))
                        field_list(9)%label = VN_PRESNO
                        allocate(field_list(9)%field, source = model_variable_real1d(dat = null()))
                    end if

                    !> Update common attributes.
                    do j = 1, size(field_list)
                        field_list(j)%id = j
                        field_list(j)%mapping%time_order = 1
                        if (.not. allocated(field_list(j)%field)) then
                            allocate(field_list(j)%field, source = model_variable_real1d(dat = null()))
                        end if
                        allocate( &
                            field_list(j)%dim_names(size(forcing_files(i)%overrides%dim_names)), &
                            source = forcing_files(i)%overrides%dim_names)
                    end do

                    !> Attach the field list to the file.
                    call combine_field_list(forcing_files(i)%fields, field_list, error_status)
                type is (io_type_char_delimited)

                    !> Allocate temporary variables.
                    allocate(field_list(1))

                    !> Add the field.
                    field_list(1)%label = trim(forcing_files(i)%field_map(1, 2))
                    field_list(1)%id = 1
                    field_list(1)%mapping%time_order = 2
                    allocate(field_list(1)%field, source = model_variable_real2d(dat = null(), &
                        const_mul = forcing_files(i)%overrides%const_mul, const_add = forcing_files(i)%overrides%const_add))
                    allocate(field_list(1)%dim_names(size(forcing_files(i)%overrides%dim_names)), source = forcing_files(i)%overrides%dim_names)
                    call combine_field_list(forcing_files(i)%fields, field_list, error_status)
                type is (io_type_seq)

                    !> Allocate temporary variables.
                    allocate(field_list(1))

                    !> Add the field.
                    field_list(1)%label = trim(forcing_files(i)%field_map(1, 2))
                    field_list(1)%id = 1
                    field_list(1)%mapping%time_order = 2
                    allocate(field_list(1)%field, source = model_variable_real2d(dat = null(), &
                        const_mul = forcing_files(i)%overrides%const_mul, const_add = forcing_files(i)%overrides%const_add))
                    allocate( &
                        field_list(1)%dim_names(size(forcing_files(i)%overrides%dim_names)), &
                        source = forcing_files(i)%overrides%dim_names)
                    call combine_field_list(forcing_files(i)%fields, field_list, error_status)
            end select
        end if

        !> Check for errors.
        if (error_status /= 0) then
            if (v) call print_warning("An error occurred assigning fields for the '" // flag_name // "' control flag.")
            error_status = 0
        end if

    end subroutine

    subroutine open_input_forcing_files(error_status)

        !> Modules.
        use model_dates, only: ic
        use model_variables, only: vs

        !> Input/output variables.
        integer, intent(out) :: error_status

        !> Local variables.
        real, allocatable :: dat_cell_interp_r(:, :), dat_tile_interp_r(:, :)
        character(len = DEFAULT_FIELD_LENGTH) code
        integer j, i, iwarn

        !> Status.
        error_status = 0

        !> Open files.
        do i = 1, size(forcing_files)

            !> Pre-allocate the data field for file formats that do not read the information from file.
            select type (file_type => forcing_files(i)%container)
                type is (io_type_met)
                    do j = 1, size(forcing_files(i)%fields)
                        select type (field => forcing_files(i)%fields(j)%field)

                            !> Assume field type from known file format.
                            type is (model_variable_real1d)
                                allocate(field%dat(forcing_files(i)%series%block_interval))
                                field%dat = huge(field%dat)
                        end select
                    end do
                type is (io_type_char_delimited)
                    do j = 1, size(forcing_files(i)%fields)
                        select type (field => forcing_files(i)%fields(j)%field)

                            !> Assume number of dimensions and field type from known file format.
                            type is (model_variable_real2d)
                                if (allocated(forcing_files(i)%fields(j)%dim_names)) then
                                    select case (forcing_files(i)%fields(j)%dim_names(1))
                                        case (DIM_NAME_M)
                                            allocate(field%dat(maxval(vs%tile%from_gru), forcing_files(i)%series%block_interval))
                                        case (DIM_NAME_N)
                                            allocate(field%dat(vs%grid%dim_length, forcing_files(i)%series%block_interval))
                                    end select
                                    if (allocated(field%dat)) field%dat = huge(field%dat)
                                end if
                        end select
                    end do
                type is (io_type_seq)
                    do j = 1, size(forcing_files(i)%fields)
                        select type (field => forcing_files(i)%fields(j)%field)

                            !> Assume number of dimensions and field type from known file format.
                            type is (model_variable_real2d)
                                allocate(field%dat(vs%grid%dim_length, forcing_files(i)%series%block_interval))
                                field%dat = huge(field%dat)
                        end select
                    end do
            end select

            !> Open the file.
            call open_input_file(forcing_files(i), error_status = error_status)
            if (error_status /= 0) exit

            !> Validate spatial reference.
            if (.true.) then
                call validate_input_file_spatial_reference(forcing_files(i), error_status = error_status)
                if (error_status /= 0) exit
            end if

            !> Identify fields in the file.
            call read_file_fields_to_buffer(forcing_files(i), error_status = error_status)
            if (error_status /= 0) exit

            !> Deactivate non-temporal fields.
            do j = 1, size(forcing_files(i)%fields)
                if (.not. forcing_files(i)%fields(j)%mapping%time_order > 0) then
                    deallocate(forcing_files(i)%fields(j)%field)
                end if
            end do

            !> Compact the list.
            call cleanup_field_list(forcing_files(i)%fields, error_status)

            !> Check if the file still contains fields.
            if (.not. allocated(forcing_files(i)%fields)) then
                call print_error("The file contains no active temporal fields.")
                error_status = 1
            else

                !> Activate fields and create field maps.
                call activate_variables_from_field_list(forcing_files(i)%fields, error_status)
                call create_mapped_output_from_field_list(forcing_files(i)%fields, error_status = error_status)
                if (error_status /= 0) return

                !> Allocate fields for temporal interpolation if enabled.
                if (forcing_file_temporal_interpolation > 0) then
                    if (forcing_file_temporal_interpolation == 1) then

                        !> Set the flag for the file.
                        forcing_files(i)%temporal_interp%scheme = forcing_file_temporal_interpolation
                        call print_remark("INTERPOLATIONFLAG is enabled.")

                        !> Determine the frequency interval.
                        j = 0
                        select case (forcing_files(i)%series%freq)
                            case (FREQ_MINUTES)
                                j = forcing_files(i)%series%freq_interval/ic%dtmins
                            case (FREQ_HOURS)
                                j = forcing_files(i)%series%freq_interval*60/ic%dtmins
                            case (FREQ_DAYS)
                                j = forcing_files(i)%series%freq_interval*24*60/ic%dtmins
                        end select
                        if (j > 0) then

                            !> Pre-calculate weights.
                            allocate(forcing_files(i)%temporal_interp%interp_weights(j))
                            do j = 1, size(forcing_files(i)%temporal_interp%interp_weights)
                                forcing_files(i)%temporal_interp%interp_weights(j) = &
                                    real(j - 1)/size(forcing_files(i)%temporal_interp%interp_weights)
                            end do

                            !> Allocate field variables (temporal interpolation is only applied to 'real' type fields).
                            do j = 1, size(forcing_files(i)%fields)
                                iwarn = 0
                                if (allocated(forcing_files(i)%fields(j)%mapping%mapped_to_cell)) then
                                    select type (field => forcing_files(i)%fields(j)%mapping%mapped_to_cell)
                                        type is (model_variable_real1d)
                                            allocate(dat_cell_interp_r(vs%grid%dim_length, 2))
                                            dat_cell_interp_r = NO_DATA_REAL
                                            allocate(forcing_files(i)%fields(j)%mapping%mapped_to_cell_interp, &
                                                source = model_variable_real2d(dat = dat_cell_interp_r))
                                            deallocate(dat_cell_interp_r)
                                        class default
                                            iwarn = 1
                                    end select
                                end if
                                if (allocated(forcing_files(i)%fields(j)%mapping%mapped_to_tile)) then
                                    select type (field => forcing_files(i)%fields(j)%mapping%mapped_to_tile)
                                        type is (model_variable_real1d)
                                            allocate(dat_tile_interp_r(vs%tile%dim_length, 2))
                                            dat_tile_interp_r = NO_DATA_REAL
                                            allocate(forcing_files(i)%fields(j)%mapping%mapped_to_tile_interp, &
                                                source = model_variable_real2d(dat = dat_tile_interp_r))
                                            deallocate(dat_tile_interp_r)
                                        class default
                                            iwarn = 1
                                    end select
                                end if
                                if (iwarn /= 0) then
                                    call print_warning( &
                                        "INTERPOLATIONFLAG cannot be applied for the data type of the '" // &
                                        trim(forcing_files(i)%fields(j)%label) // &
                                        "' variable. The option has no effect for this field.")
                                end if
                            end do
                        end if
                    else

                        !> Print a warning if the flag is not recognized or unsupported.
                        write(code, *) forcing_file_temporal_interpolation
                        call print_warning( &
                            "The value of INTERPOLATIONFLAG (" // trim(adjustl(code)) // ") is not recognized or unsupported. " // &
                            "The option has no effect.")
                    end if
                end if
            end if
        end do

    end subroutine

    subroutine get_start_date_components_from_forcing_files(year, month, day, jday, hour, minutes, error_status)

        !> Modules.
        use date_utilities, only: jday_to_date

        !> Input/output variables.
        integer year, month, day, jday, hour, minutes
        integer, intent(out) :: error_status

        !> Local variables.
        integer i

        !> Status.
        error_status = 0

        !> Loop through the files to find the latest start date.
        do i = 1, size(forcing_files)

            !> Check forcing file start date.
            if ( &
                forcing_files(i)%series%start%year == 0 .and. forcing_files(i)%series%start%jday == 0 .and. &
                forcing_files(i)%series%start%hour == 0 .and. forcing_files(i)%series%start%minutes == 0) then
                forcing_files(i)%series%start%year = forcing_file_start_date_override%year
                forcing_files(i)%series%start%month = forcing_file_start_date_override%month
                forcing_files(i)%series%start%day = forcing_file_start_date_override%day
                forcing_files(i)%series%start%jday = forcing_file_start_date_override%jday
                forcing_files(i)%series%start%hour = forcing_file_start_date_override%hour
                forcing_files(i)%series%start%minutes = forcing_file_start_date_override%minutes
            end if

            !> Check if forcing file start date is later than the provided components.
            if (( &
                forcing_files(i)%series%start%year*10000 + forcing_files(i)%series%start%month*100 + &
                forcing_files(i)%series%start%day) > (year*10000 + month*100 + day) .or. (&
                forcing_files(i)%series%start%year*1000 + forcing_files(i)%series%start%jday) > (year*1000 + jday)) then
                year = forcing_files(i)%series%start%year
                month = forcing_files(i)%series%start%month
                day = forcing_files(i)%series%start%day
                jday = forcing_files(i)%series%start%jday
                hour = forcing_files(i)%series%start%hour
                minutes = forcing_files(i)%series%start%minutes
            end if
        end do

    end subroutine

    subroutine read_input_forcing_frame_from_file(input_file, error_status)

        !> Modules.
        use model_dates, only: ic

        !> Input/output variables.
        !*  error_status: Status returned by the operation (optional; 0: normal).
        type(io_file) input_file
        integer, intent(out) :: error_status

        !> Local variables.
        integer j

        !> Status.
        error_status = 0

        !> Read data if at the beginning of a new interval.
        if (input_file%series%istep == 1) then

            !> Read frame.
            if (input_file%series%iblock == 1) call read_frame_from_file(input_file, error_status = error_status)
            if (error_status /= 0) then
                call print_remark("Reached the of the file in '" // trim(input_file%full_path) // "'.")
                return
            end if

            !> Update mapped output.
            call create_mapped_output_from_field_list(input_file%fields, input_file%series%iblock, error_status = error_status)
            if (error_status == 0 .and. input_file%temporal_interp%scheme == 1) then

                !> Update fields for temporal linear interpolation if enabled.
                do j = 1, size(input_file%fields)
                    if (allocated(input_file%fields(j)%mapping%mapped_to_cell_interp)) then
                        select type (interp => input_file%fields(j)%mapping%mapped_to_cell_interp)
                            type is (model_variable_real2d)
                                select type (field => input_file%fields(j)%mapping%mapped_to_cell)
                                    type is (model_variable_real1d)
                                        interp%dat(:, 1) = interp%dat(:, 2)
                                        interp%dat(:, 2) = field%dat
                                end select
                        end select
                    end if
                    if (allocated(input_file%fields(j)%mapping%mapped_to_tile_interp)) then
                        select type (interp => input_file%fields(j)%mapping%mapped_to_tile_interp)
                            type is (model_variable_real2d)
                                select type (field => input_file%fields(j)%mapping%mapped_to_tile)
                                    type is (model_variable_real1d)
                                        interp%dat(:, 1) = interp%dat(:, 2)
                                        interp%dat(:, 2) = field%dat
                                end select
                        end select
                    end if
                end do
            end if
        end if

        !> Override the mapped 'dat' values if temporal interpolation is enabled.
        if (input_file%temporal_interp%scheme == 1) then
            do j = 1, size(input_file%fields)

                !> Calculate if only enough frames have been read that index '1' of the 'interp' field is assigned.
                if (allocated(input_file%fields(j)%mapping%mapped_to_cell_interp)) then
                    select type (interp => input_file%fields(j)%mapping%mapped_to_cell_interp)
                        type is (model_variable_real2d)
                            select type (field => input_file%fields(j)%mapping%mapped_to_cell)
                                type is (model_variable_real1d)
                                    if (all(interp%dat(:, 1) /= NO_DATA_REAL)) then
                                        field%dat = interp%dat(:, 1) + &
                                            input_file%temporal_interp%interp_weights(input_file%series%istep)* &
                                            (interp%dat(:, 2) - interp%dat(:, 1))
                                    end if
                            end select
                    end select
                end if
                if (allocated(input_file%fields(j)%mapping%mapped_to_tile_interp)) then
                    select type (interp => input_file%fields(j)%mapping%mapped_to_tile_interp)
                        type is (model_variable_real2d)
                            select type (field => input_file%fields(j)%mapping%mapped_to_tile)
                                type is (model_variable_real1d)
                                    if (all(interp%dat(:, 1) /= NO_DATA_REAL)) then
                                        field%dat = interp%dat(:, 1) + &
                                            input_file%temporal_interp%interp_weights(input_file%series%istep)* &
                                            (interp%dat(:, 2) - interp%dat(:, 1))
                                    end if
                            end select
                    end select
                end if
            end do
        end if

        !> Assign values.
        if (error_status == 0) then
            call assign_variables_from_field_list(input_file%fields, error_status)
        end if
        if (error_status /= 0) then
            call print_remark("An error occurred assigning the values from '" // trim(input_file%full_path) // "'.")
            return
        else

            !> Increment block counter.
            if (input_file%series%block_interval > 1) then
                input_file%series%iblock = input_file%series%iblock + 1
                if (input_file%series%iblock > input_file%series%block_interval) then

                    !> Reset the block counter.
                    input_file%series%iblock = 1
                end if
            end if
        end if

        !> Increment the interval timer.
        input_file%series%istep = input_file%series%istep + 1

        !> Check if the interval should be reset.
        select case (input_file%series%freq)
            case (FREQ_MINUTES)
                if (input_file%series%istep*ic%dtmins > input_file%series%freq_interval) then
                    input_file%series%istep = 1
                end if
            case (FREQ_HOURS)
                if (input_file%series%istep*ic%dtmins > input_file%series%freq_interval*60) then
                    input_file%series%istep = 1
                end if
            case (FREQ_DAYS)
                if (input_file%series%istep*ic%dtmins > input_file%series%freq_interval*24*60) then
                    input_file%series%istep = 1
                end if
        end select

    end subroutine

    subroutine skip_to_first_forcing_record(error_status)

        !> Modules.
        use date_utilities, only: date_components_to_time

        !> Variables.
        use model_dates

        !> Input/output variables.
        !*  error_status: Status returned by the operation (optional; 0: normal).
        integer, intent(out) :: error_status

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line, code1, code2
        real(kind = kind(0.0d0)) t0, t1
        integer n, m, j, i, skip_count, frame_length_minutes, interp_count

        !> Status.
        error_status = 0

        !> Get the simulation start time.
        call print_new_section('')
        t1 = date_components_to_time( &
            ic%start%year, ic%start%month, ic%start%day, ic%start%hour, ic%start%mins, error_status = error_status)
        if (error_status /= 0) then
            call print_error("An error occurred getting the start time of the simulation.")
            return
        else

            !> Convert from days to minutes.
            t1 = t1*24.0*60.0
        end if

        !> Check and skip records if the simulation starts after the start of the forcing file.
        write(line, FMT_DATETIME_SLASHES_YMD) ic%start%year, ic%start%month, ic%start%day, ic%start%hour, ic%start%mins, 0
        call print_message("The simulation starts at: " // trim(adjustl(line)))
        call print_message("Checking the start dates of the input files...")
        call increase_tab()
        n = 0
        do i = 1, size(forcing_files)

            !> Get the file start time.
            t0 = date_components_to_time( &
                forcing_files(i)%series%start%year, &
                forcing_files(i)%series%start%month, &
                forcing_files(i)%series%start%day, &
                forcing_files(i)%series%start%hour, &
                forcing_files(i)%series%start%minutes, error_status = error_status)
            if (error_status /= 0) then
                call print_error( &
                    "An error occurred getting the start time from the '" // trim(forcing_files(i)%full_path) // "' file.")
                return
            else

                !> Convert from days to minutes.
                t0 = t0*24.0*60.0
            end if

            !> Reset variables.
            skip_count = 0
            frame_length_minutes = 0

            !> Check the file time-stepping.
            select case (forcing_files(i)%series%freq)
                case (FREQ_MINUTES)
                    frame_length_minutes = forcing_files(i)%series%freq_interval
                case (FREQ_HOURS)
                    frame_length_minutes = forcing_files(i)%series%freq_interval*60
                case (FREQ_DAYS)
                    frame_length_minutes = forcing_files(i)%series%freq_interval*24*60
                case default
                    call print_warning("The units of the file frame length are unknown or unsupported.")
            end select
            if (frame_length_minutes < ic%dtmins) then

                !> Print an error if the frame length is less than the model time-step.
!todo: Could probably find a way to accommodate this (e.g., accumulating/averaging/etc...).
                write(code1, *) ic%dtmins
                call print_error( &
                    "The frame length in the file is less than the model time-step (" // trim(adjustl(code1)) // " minutes). " // &
                    "The data must be resampled to the model time-step.")
                error_status = 1
                return
            else if (mod(frame_length_minutes, ic%dtmins) /= 0) then

                !> Print an error if the frame length is not evenly divisibly by the model time-step.
                write(code1, *) frame_length_minutes
                write(code2, *) ic%dtmins
                call print_error( &
                    "The frame length in the file (" // trim(adjustl(code1)) // &
                    " minutes) must be divisible by the model time-step (" // trim(adjustl(code2)) // " minutes). " // &
                    "The model time-step must be changed or the data should be resampled.")
                error_status = 1
                return
            end if

            !> Forcing record.
            if (DIAGNOSEMODE) then
                write(line, FMT_DATETIME_SLASHES_YMD) &
                    forcing_files(i)%series%start%year, forcing_files(i)%series%start%month, forcing_files(i)%series%start%day, &
                    forcing_files(i)%series%start%hour, forcing_files(i)%series%start%minutes, 0
                call print_info( &
                    "The records in '" // trim(forcing_files(i)%full_path) // "' start at: " // trim(adjustl(line)))
            end if

            !> Check start time.
            if (t0 < t1) then

                !> Calculate the difference.
                select case (forcing_files(i)%series%freq)
                    case (FREQ_MINUTES)
                        skip_count = int(t1 - t0)
                    case (FREQ_HOURS)
                        skip_count = int(t1 - t0)/60
                    case (FREQ_DAYS)
                        skip_count = int(t1 - t0)/60/24
                end select

                !> Divide by the frame-length to determine the number of records.
                skip_count = skip_count/forcing_files(i)%series%freq_interval

                !> Skip records.
                if (skip_count > 0) then
                    write(line, *) skip_count
                    call print_message( &
                        "Skipping " // trim(adjustl(line)) // " records in '" // trim(forcing_files(i)%full_path) // "'.")
                    do j = 1, skip_count
                        call read_frame_from_file(forcing_files(i), skip_data = .true., error_status = error_status)
                        if (error_status /= 0) exit
                    end do
                    n = n + 1
                else if (DIAGNOSEMODE) then
                    call print_info("Skipping no records in '" // trim(forcing_files(i)%full_path) // "'.")
                end if
            else if (t0 > t1) then
                call print_error("The start time of the forcing file occurs after the simulation start time.")
                error_status = 1
                return
            end if

            !> Compare the frame length to model time-step for temporal interpolation.
            if (forcing_files(i)%temporal_interp%scheme > 0 .and. frame_length_minutes == ic%dtmins) then
                call print_remark( &
                    "INTERPOLATIONFLAG is enabled but the frame length in the file is the same as the model time-step. " // &
                    "The option has no effect.")
            end if

            !> Read and map the first frame if temporal interpolation is enabled.
            if (forcing_files(i)%temporal_interp%scheme == 1) then
                call read_input_forcing_frame_from_file(forcing_files(i), error_status)

                !> Reset the file stepping so data is still be read in the first model time-step.
                forcing_files(i)%series%istep = 1
            end if
        end do

        !> Print message if no records to skip.
        if (.not. DIAGNOSEMODE .and. n == 0) then
            call print_message("All forcing files start from the simulation start date. No records to skip.")
        end if

    end subroutine

    subroutine read_input_forcing_frame(error_status)

        !> Input/output variables.
        !*  error_status: Status returned by the operation (optional; 0: normal).
        integer, intent(out) :: error_status

        !> Local variables.
        integer i, ierr

        !> Status.
        error_status = 0

        !> Read frames in all files.
        ierr = 0
        do i = 1, size(forcing_files)
            call read_input_forcing_frame_from_file(forcing_files(i), ierr)
            if (ierr /= 0) error_status = ierr
        end do

    end subroutine

end module
