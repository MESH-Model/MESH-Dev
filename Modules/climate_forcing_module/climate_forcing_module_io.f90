!> Description:
!>  Module to read climate forcing data from file.
module climate_forcing_io

    use climate_forcing_constants
    use climate_forcing_variabletypes
    use print_routines

    implicit none

    real, parameter, private :: NO_DATA = -999.999

    contains

    !> Description:
    !>  Open the climate forcing input file.
    !>
    !> Input/output variables:
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and its unit.
    !*  vid: Index of the climate forcing variable.
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error opening the file.
    logical function open_data(shd, cm, vid) result(ENDDATA)

        !> 'shd_variables': For 'shd' variable.
        !> 'parse_utilities': For 'parse_datetime' and 'precision' (via 'strings').
#ifdef NETCDF
        !> 'netcdf': For netCDF library.
#endif
        use shd_variables
        use parse_utilities
#ifdef NETCDF
        use netcdf
#endif

        !> 'strings': For 'lowercase' function.
        use strings, only: lowercase

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: vid

        !> Input/Output variables.
        type(clim_info) cm

        !> Local variables.
        integer z, ierr
        character(len = DEFAULT_LINE_LENGTH) line
#ifdef NETCDF
        character(len = DEFAULT_LINE_LENGTH) time_attribute, time_units, time_calendar
        integer, dimension(:), allocatable :: dimids
        integer d, dtype, t0_year, t0_month, t0_day, t0_hour, t0_mins, t0_seconds, jday, ncol_lon, ncol_lat
        integer(kind = ki4) tt_i4(2)
        real(kind = kr4) tt_r4(2)
        real(kind = kr8) tt_r8(2), t0_r8, t1_r8, t2_r8, dt_r8
#endif

        !> Initialize the return variable.
        ENDDATA = .false.

        !> Return if the variable is not marked active.
        if (.not. cm%dat(vid)%factive) return

        !> Open file depending on the format type of the climate data.
        select case (cm%dat(vid)%ffmt)

            !> ASCII R2C format.
            case (1)

                !> Update the path if none exists.
                if (len_trim(cm%dat(vid)%fpath) == 0) then
                    cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.r2c'
                end if

                !> Open the file.
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)

                !> Return on an error.
                if (ierr /= 0) goto 999

                !> Skip the header of the 'r2c' format file.
                line = ''
                do while (lowercase(line) /= ':endheader')
                    read(cm%dat(vid)%fiun, '(a10)', end = 998) line
                end do

                !> Set the block type.
                cm%dat(vid)%blocktype = cbk%GRD

            !> CSV format.
            case (2)
                if (len_trim(cm%dat(vid)%fpath) == 0) then
                    cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.csv'
                end if
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRU

            !> Binary sequential format.
            case (3)
                if (len_trim(cm%dat(vid)%fpath) == 0) then
                    cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.seq'
                end if
                open( &
                    cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', &
                    form = 'unformatted', access = 'sequential', iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRD

            !> ASCII format.
            case (4)
                if (len_trim(cm%dat(vid)%fpath) == 0) then
                    cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.asc'
                end if
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRD

            !> CLASS 'MET' file.
            case (6)
                if (len_trim(cm%dat(vid)%fname) == 0) then
                    cm%dat(vid)%fname = 'basin_forcing'
                end if
                if (len_trim(cm%dat(vid)%fpath) == 0) then
                    cm%dat(vid)%fpath = 'basin_forcing.met'
                end if
                cm%dat(vid)%blocktype = cbk%GRD
                if (vid == ck%MET) then
                    open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)
                    if (ierr /= 0) goto 999
                end if

            !> netCDF format.
            case(7)

                !> Update the path if none exists.
                if (len_trim(cm%dat(vid)%fpath) == 0) then
                    cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.nc'
                end if
#ifdef NETCDF

                !> Open the file.
                ierr = nf90_open(cm%dat(vid)%fpath, NF90_NOWRITE, cm%dat(vid)%fiun)
                if (ierr /= NF90_NOERR) then
                    call print_error('Unable to open file: ' // trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if

                !> Check that the variable 'id_var' exists in the file.
                ierr = nf90_inq_varid(cm%dat(vid)%fiun, cm%dat(vid)%id_var, cm%dat(vid)%vid)
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "The variable '" // trim(cm%dat(vid)%id_var) // "' cound not be found in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if

                !> Get the dimensions of the variable.
                ierr = nf90_inquire_variable(cm%dat(vid)%fiun, cm%dat(vid)%vid, ndims = cm%dat(vid)%ndims)
                if (ierr == NF90_NOERR) then
                    allocate(dimids(cm%dat(vid)%ndims), cm%dat(vid)%dim_length(cm%dat(vid)%ndims), stat = z)
                    if (z == 0) ierr = nf90_inquire_variable(cm%dat(vid)%fiun, cm%dat(vid)%vid, dimids = dimids)
                end if
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "Unable to read dimensions of '" // trim(cm%dat(vid)%id_var) // "' in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if

                !> Check the dimensions.
                !> Only variables with three dimensions are supported (combination of lat, lon, time).
                !> Of the respective dimensions, compare to the expected length.
                z = 0
                do d = 1, cm%dat(vid)%ndims
                    ierr = nf90_inquire_dimension(cm%dat(vid)%fiun, dimids(d), len = cm%dat(vid)%dim_length(d))
                    if (ierr /= NF90_NOERR) exit
                    ierr = nf90_inquire_dimension(cm%dat(vid)%fiun, dimids(d), name = line)
                    if (ierr /= NF90_NOERR) exit
                    if (line == cm%dat(vid)%name_lon) then

                        !> Longitude.
                        if (cm%dat(vid)%dim_length(d) /= shd%xCount) then
                            call print_error( &
                                "The model configuration contains a different number of '" // trim(cm%dat(vid)%name_lon) // &
                                "' elements than in file: " // trim(cm%dat(vid)%fpath))
                            call program_abort()
                        end if
                        ncol_lon = d
                        z = z + radix(1)**1
                    else if (line == cm%dat(vid)%name_lat) then

                        !> Latitude.
                        if (cm%dat(vid)%dim_length(d) /= shd%yCount) then
                            call print_error( &
                                "The model configuration contains a different number of '" // trim(cm%dat(vid)%name_lat) // &
                                "' elements than in file: " // trim(cm%dat(vid)%fpath))
                            call program_abort()
                        end if
                        ncol_lat = d
                        z = z + radix(1)**2
                    else if (line == cm%dat(vid)%name_time) then

                        !> Time.
                        !> Store the column order (used to update start position when reading).
                        cm%dat(vid)%ncol_time = d
                        cm%dat(vid)%tid = dimids(d)
                        z = z + radix(1)**3

                        !> Override the length to 'nblocks' (to control the number of time-steps read).
                        cm%dat(vid)%dim_length(d) = cm%dat(vid)%nblocks
                    end if
                end do
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "Unable to read dimensions of '" // trim(cm%dat(vid)%id_var) // "' in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
                if (.not. btest(z, 1)) then
                    call print_warning( &
                        "A required attribute '" // trim(cm%dat(vid)%name_lon) // "' cound not be found in file: " // &
                        trim(cm%dat(vid)%fpath))
                else
                    z = z - radix(1)**1
                end if
                if (.not. btest(z, 2)) then
                    call print_warning( &
                        "A required attribute '" // trim(cm%dat(vid)%name_lat) // "' cound not be found in file: " // &
                        trim(cm%dat(vid)%fpath))
                else
                    z = z - radix(1)**2
                end if
                if (.not. btest(z, 3)) then
                    call print_warning( &
                        "A required attribute '" // trim(cm%dat(vid)%name_time) // "' cound not be found in file: " // &
                        trim(cm%dat(vid)%fpath))
                else
                    z = z - radix(1)**3
                end if
                if (z /= 0 .or. cm%dat(vid)%ndims /= 3) then
                    call print_error( &
                        "The variable '" // trim(cm%dat(vid)%name_time) // "' must contain only three dimensions for" // &
                        ' longitude, latitude, and time (in any order). The file must be pre-processed to remove' // &
                        ' dimensions in excess of this number.')
                    call print_message( &
                        'If the file is compatible with this structure, the dimensions must be mapped' // &
                        " using the 'name_lon', 'name_lat', and 'name_time' options where the names of these dimensions" // &
                        " are different from the default names of 'lon', 'lat', and 'time', respectively" // &
                        ' (via the input configuration files).')
                    call program_abort()
                end if

                !> Check the units of the time dimension.
                !> Only dates of the Gregorian calendar type are supported.
                ierr = nf90_get_att(cm%dat(vid)%fiun, cm%dat(vid)%tid, 'units', time_attribute)
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "The units are missing for the '" // trim(cm%dat(vid)%name_time) // "' attribute in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
                call parse_datetime(time_attribute, t0_year, t0_month, t0_day, t0_hour, t0_mins, t0_seconds, z)
                ierr = nf90_get_att(cm%dat(vid)%fiun, cm%dat(vid)%tid, 'calendar', time_calendar)
                if (ierr /= NF90_NOERR .or. lowercase(time_calendar) /= 'gregorian') then
                    call print_warning( &
                        "The reference calendar for '" // trim(cm%dat(vid)%name_time) // "' is not set or not equal to '" // &
                        'Gregorian' // "' in file: " // trim(cm%dat(vid)%fpath))
                end if
                if (z /= 0) then
                    call print_error( &
                        "The format of the units of '" // trim(cm%dat(vid)%name_time) // "' is unsupported in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call print_message('Expected format: [seconds/minutes/hours/days] since yyyy/MM/dd HH:mm:ss[.SSS]')
                    call program_abort()
                else if (t0_year < 1601) then
                    write(line, FMT_GEN) t0_year
                    call print_error( &
                        'The reference year (' // trim(adjustl(line)) // ') is less than 1601.' // &
                        ' The reference calendar does not correpond to the Gregorian calendar.')
                    call print_message( &
                        " The time-series of '" // trim(cm%dat(vid)%name_time) // "' cannot be processed in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if

                !> Get the data type of the variable.
                !> Only integer, float, and double types are supported.
                ierr = nf90_inquire_variable(cm%dat(vid)%fiun, cm%dat(vid)%tid, xtype = dtype)
                if (ierr == NF90_NOERR) then
                    select case (dtype)
                        case (NF90_INT)
                            ierr = nf90_get_var(cm%dat(vid)%fiun, cm%dat(vid)%tid, tt_i4, start = (/1/), count = (/2/))
                            if (ierr == NF90_NOERR) then
                                t1_r8 = real(tt_i4(1), kind = 8)
                                t2_r8 = real(tt_i4(2), kind = 8)
                            end if
                        case (NF90_FLOAT)
                            ierr = nf90_get_var(cm%dat(vid)%fiun, cm%dat(vid)%tid, tt_r4, start = (/1/), count = (/2/))
                            if (ierr == NF90_NOERR) then
                                t1_r8 = real(tt_r4(1), kind = 8)
                                t2_r8 = real(tt_r4(2), kind = 8)
                            end if
                        case (NF90_DOUBLE)
                            ierr = nf90_get_var(cm%dat(vid)%fiun, cm%dat(vid)%tid, tt_r8, start = (/1/), count = (/2/))
                            if (ierr == NF90_NOERR) then
                                t1_r8 = tt_r8(1)
                                t2_r8 = tt_r8(2)
                            end if
                        case default
                            ierr = NF90_EBADTYPE
                    end select
                end if
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "Unsupported data type for the '" // trim(cm%dat(vid)%name_time) // "' attribute in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if

                !> Calculate the reference date from the units of the time dimension.
                !> Only units of seconds, minutes, hours, and days are supported.
                jday = get_jday(t0_month, t0_day, t0_year)
                t0_r8 = real(jday_to_tsteps(t0_year, jday, t0_hour, t0_mins, (60*24)) + t0_seconds/60.0/60.0/24.0, kind = 8)
                dt_r8 = t0_r8 + cm%dat(vid)%time_shift/24.0
                read(time_attribute, *) time_units
                select case (time_units)
                    case ('seconds')
                        dt_r8 = dt_r8 + t1_r8/60.0/60.0/24.0
                        cm%dat(vid)%hf = int((t2_r8 - t1_r8)/60.0 + 0.5)        ! 0.5 takes care of correct rounding
                    case ('minutes')
                        dt_r8 = dt_r8 + t1_r8/60.0/24.0
                        cm%dat(vid)%hf = int(t2_r8 - t1_r8 + 0.5)               ! 0.5 takes care of correct rounding
                    case ('hours')
                        dt_r8 = dt_r8 + t1_r8/24.0
                        cm%dat(vid)%hf = int((t2_r8 - t1_r8)*60.0 + 0.5)        ! 0.5 takes care of correct rounding
                    case ('days')
                        dt_r8 = dt_r8 + t1_r8
                        cm%dat(vid)%hf = int((t2_r8 - t1_r8)*24.0*60.0 + 0.5)   ! 0.5 takes care of correct rounding
                    case default
                        ierr = NF90_EINVAL
                end select
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "The units of '" // trim(adjustl(time_units)) // "' are unsupported for '" // &
                        trim(cm%dat(vid)%name_time) // "' in file: " // trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if

                !> Calculate the date of the first record in the file.
                !> Assumes dates increase along the time dimension.
                cm%dat(vid)%start_date%year = floor(dt_r8/365.25) + 1601
                cm%dat(vid)%start_date%jday = floor(dt_r8) - floor((cm%dat(vid)%start_date%year - 1601)*365.25)
                cm%dat(vid)%start_date%hour = floor(dt_r8 - floor(dt_r8))*24
                cm%dat(vid)%start_date%mins = int(floor(dt_r8 - floor(dt_r8))*60.0*24.0 - cm%dat(vid)%start_date%hour*60.0 + 0.5)

                !> Assign an ID based on the order (for mapping when reading from the file).
                if (ncol_lon == 1 .and. ncol_lat == 2 .and. cm%dat(vid)%ncol_time == 3) then
                    if (DIAGNOSEMODE) call print_message('   dim order case #1 --> (lon,lat,time)')
                    cm%dat(vid)%dim_order_case = 1
                else if (ncol_lon == 2 .and. ncol_lat == 1 .and. cm%dat(vid)%ncol_time == 3) then
                    if (DIAGNOSEMODE) call print_message('   dim order case #2 --> (lat,lon,time)')
                    cm%dat(vid)%dim_order_case = 2
                else if (ncol_lon == 1 .and. ncol_lat == 3 .and. cm%dat(vid)%ncol_time == 2) then
                    if (DIAGNOSEMODE) call print_message('   dim order case #3 --> (lon,time,lat)')
                    cm%dat(vid)%dim_order_case = 3
                else if (ncol_lon == 3 .and. ncol_lat == 1 .and. cm%dat(vid)%ncol_time == 2) then
                    if (DIAGNOSEMODE) call print_message('   dim order case #4 --> (lat,time,lon)')
                    cm%dat(vid)%dim_order_case = 4
                else if (ncol_lon == 2 .and. ncol_lat == 3 .and. cm%dat(vid)%ncol_time == 1) then
                    if (DIAGNOSEMODE) call print_message('   dim order case #5 --> (time,lon,lat)')
                    cm%dat(vid)%dim_order_case = 5
                else if (ncol_lon == 3 .and. ncol_lat == 2 .and. cm%dat(vid)%ncol_time == 1) then
                    if (DIAGNOSEMODE) call print_message('   dim order case #6 --> (time,lat,lon)')
                    cm%dat(vid)%dim_order_case = 6
                else
                    call print_error( &
                        "Unable to determine the order of dimensions of '" // trim(cm%dat(vid)%id_var) // "' in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if

                !> Set the block type.
                cm%dat(vid)%blocktype = cbk%GRD
#else
                call print_error( &
                    'NetCDF format is specified for ' // trim(cm%dat(vid)%fpath) // ' (' // trim(cm%dat(vid)%id_var) // &
                    ') but the module is not active.' // &
                    ' A version of MESH compiled with the NetCDF library must be used to read files in this format.')
                call program_abort()
#endif

            !> Unknown file format.
            case default
                call print_error(trim(cm%dat(vid)%fname) // ' (' // trim(cm%dat(vid)%id_var) // '): Unsupported file format.')
                call program_abort()

        end select

        !> Allocate the block variable.
        if (allocated(cm%dat(vid)%blocks)) deallocate(cm%dat(vid)%blocks)
        select case (cm%dat(vid)%blocktype)
            case (1)

                !> Block type: GRD (Grid).
                allocate(cm%dat(vid)%blocks(shd%NA, cm%dat(vid)%nblocks), stat = ierr)
            case (2)

                !> Block type: GRU.
                allocate(cm%dat(vid)%blocks(shd%lc%NTYPE, cm%dat(vid)%nblocks), stat = ierr)
            case (3)

                !> Block type: GAT (Land element).
                allocate(cm%dat(vid)%blocks(shd%lc%NML, cm%dat(vid)%nblocks), stat = ierr)
        end select
        if (ierr /= 0) goto 997

        !> Flag that the file has been opened.
        cm%dat(vid)%fopen = .true.

        return

999     call print_error('Unable to open ' // trim(cm%dat(vid)%fpath) // ' or file not found.')
        call program_abort()

998     call print_error('Unable to read ' // trim(cm%dat(vid)%fpath) // ' or end of file.')
        call program_abort()

997     call print_error('Unable to allocate blocks for reading ' // trim(cm%dat(vid)%fpath) // ' data into memory.')
        call program_abort()

    end function

    !> Description:
    !>  Load data for the climate forcing variable from file.
    !>
    !> Input/output variables:
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and its unit.
    !*  vid: Index of the climate forcing variable.
    !*  iskip: Number of records to skip (default: 0).
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error reading from the file.
    logical function load_data(shd, cm, vid, iskip) result(ENDDATA)

        !> 'shd_variables': For 'shd' variable.
#ifdef NETCDF
        !> 'netcdf': For netCDF library.
#endif
        use shd_variables
#ifdef NETCDF
        use netcdf
#endif

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: vid, iskip

        !> Input/Output variables.
        type(clim_info) cm

        !> Local variables.
        integer t, n, j, i, z
        real GRD(shd%yCount, shd%xCount), MET(9)
        character(len = DEFAULT_LINE_LENGTH) line
#ifdef NETCDF
        real, allocatable :: dat3(:, :, :)
        integer, dimension(3) :: start
#endif

        !> Initialize the return variable.
        ENDDATA = .false.

        !> Return if the file is not open or if it is not time to read new blocks.
        if (.not. cm%dat(vid)%fopen .or. cm%dat(vid)%iblock /= 1) return

        !> Set 't' to the number of blocks to read or skip.
        if (iskip > 0) then
            n = iskip
        else if (cm%dat(vid)%nblocks > 1) then
            n = cm%dat(vid)%nblocks
        else
            n = 1
        end if

        !> Reset the blocks to the NO_DATA value.
        cm%dat(vid)%blocks = NO_DATA

        !> Read data according to the format of the file.
        select case (cm%dat(vid)%ffmt)

            !> ASCII R2C format.
            case (1)
                do t = 1, n
                    read(cm%dat(vid)%fiun, *, end = 999) !':Frame'
                    read(cm%dat(vid)%fiun, *, end = 999) ((GRD(i, j), j = 1, shd%xCount), i = 1, shd%yCount)
                    read(cm%dat(vid)%fiun, *, end = 999) !':EndFrame'
                    if (iskip == 0) then
                        do i = 1, shd%NA
                            cm%dat(vid)%blocks(i, t) = GRD(shd%yyy(i), shd%xxx(i))
                        end do
                    end if
                end do

            !> CSV format.
            case (2)
                do t = 1, n
                    if (iskip == 0) then
                        read(cm%dat(vid)%fiun, *, end = 999) (cm%dat(vid)%blocks(j, t), j = 1, shd%lc%NTYPE)
                    else
                        read(cm%dat(vid)%fiun, *, end = 999)
                    end if
                end do

            !> Binary sequential format.
            case (3)
                do t = 1, n
                    if (iskip == 0) then
                        read(cm%dat(vid)%fiun, end = 999) !NTIME
                        read(cm%dat(vid)%fiun, end = 999) (cm%dat(vid)%blocks(i, t), i = 1, shd%NA)
                    else
                        read(cm%dat(vid)%fiun, end = 999)
                        read(cm%dat(vid)%fiun, end = 999)
                    end if
                end do

            !> ASCII format.
            case (4)
                do t = 1, n
                    if (iskip == 0) then
                        read(cm%dat(vid)%fiun, *, end = 999) (cm%dat(vid)%blocks(i, t), i = 1, shd%NA)
                    else
                        read(cm%dat(vid)%fiun, *, end = 999)
                    end if
                end do

            !> CLASS format MET file.
            case (6)
                do t = 1, n
                    if (iskip == 0) then

                        !> Read from the 'MET' file (to a generic array).
                        if (vid == ck%RR) then
                            read(cm%dat(ck%MET)%fiun, *, end = 999) i, i, i, i, MET(1:9)
                            cm%dat(ck%RR)%blocks(:, t) = MET(8)
                        else if (vid == ck%SR) then
                            read(cm%dat(ck%MET)%fiun, *, end = 999) i, i, i, i, MET(1:9)
                            cm%dat(ck%SR)%blocks(:, t) = MET(9)
                        else
                            read(cm%dat(ck%MET)%fiun, *, end = 999) i, i, i, i, MET(1:7)
                        end if

                        !> Backspace the record as other variables read independently.
                        backspace(cm%dat(ck%MET)%fiun)

                        !> Assign the appropriate field to the variable.
                        if (vid == ck%FB) then
                            cm%dat(ck%FB)%blocks(:, t) = MET(1)
                        else if (vid == ck%FI) then
                            cm%dat(ck%FI)%blocks(:, t) = MET(2)
                        else if (vid == ck%RT) then
                            cm%dat(ck%RT)%blocks(:, t) = MET(3)
                        else if (vid == ck%TT) then
                            cm%dat(ck%TT)%blocks(:, t) = MET(4) + 273.16
                        else if (vid == ck%HU) then
                            cm%dat(ck%HU)%blocks(:, t) = MET(5)
                        else if (vid == ck%UV) then
                            cm%dat(ck%UV)%blocks(:, t) = MET(6)
                        else if (vid == ck%P0) then
                            cm%dat(ck%P0)%blocks(:, t) = MET(7)
                        else if (vid == ck%MET) then

                            !> Put something in the 'MET' field to pass the ENDDATA check.
                            cm%dat(vid)%blocks(1, t) = cm%dat(ck%TT)%blocks(1, t)
                        end if
                    else
                        if (vid == ck%MET) then
                            read(cm%dat(vid)%fiun, *, end = 999)
                        end if
                    end if
                end do

            !> netCDF format.
            case(7)
#ifdef NETCDF
                if (iskip == 0) then

                    !> Allocate the temporary array considering the order of the dimensions in the variable.
                    !> Case default handled with exit when opening the file.
                    allocate(dat3(cm%dat(vid)%dim_length(1), cm%dat(vid)%dim_length(2), cm%dat(vid)%dim_length(3)))

                    !> Set the starting position in each dimension (all at '1' except time).
                    start = 1
                    start(cm%dat(vid)%ncol_time) = cm%dat(vid)%iskip + 1

                    !> Read data.
                    z = nf90_get_var(cm%dat(vid)%fiun, cm%dat(vid)%vid, dat3, start = start)
                    if (z == NF90_EINVALCOORDS) then
                        goto 999
                    else if (z /= NF90_NOERR) then
                        write(line, FMT_GEN) z
                        call print_error( &
                            'Error reading from file (code ' // trim(adjustl(line)) // '): ' // trim(cm%dat(vid)%fpath))
                        call program_abort()
                    end if

                    !> Map and save values from the temporary array.
                    !> Case default handled with exit when opening the file.
                    select case(cm%dat(vid)%dim_order_case)

                        !> GRD = transpose(dat3(y:y, x:x, t)).
                        case(1)
                            do i = 1, shd%NA
                                cm%dat(vid)%blocks(i, :) = dat3(shd%xxx(i), shd%yyy(i), :)
                            end do

                        !> GRD = dat3(y, x, t).
                        case(2)
                            do i = 1, shd%NA
                                cm%dat(vid)%blocks(i, :) = dat3(shd%yyy(i), shd%xxx(i), :)
                            end do

                        !> GRD = transpose(dat3(y:y, t, x:x)).
                        case(3)
                            do i = 1, shd%NA
                                cm%dat(vid)%blocks(i, :) = dat3(shd%xxx(i), :, shd%yyy(i))
                            end do

                        !> GRD = dat3(y:y, t, x:x).
                        case(4)
                            do i = 1, shd%NA
                                cm%dat(vid)%blocks(i, :) = dat3(shd%yyy(i), :, shd%xxx(i))
                            end do

                        !> GRD = transpose(dat3(t, y:y, x:x)).
                        case(5)
                            do i = 1, shd%NA
                                cm%dat(vid)%blocks(i, :) = dat3(:, shd%xxx(i), shd%yyy(i))
                            end do

                        !> GRD = dat3(t, y:y, x:x).
                        case(6)
                            do i = 1, shd%NA
                                cm%dat(vid)%blocks(i, :) = dat3(:, shd%yyy(i), shd%xxx(i))
                            end do
                    end select
                end if

                !> Save the number of records read to save for the next update.
                cm%dat(vid)%iskip = cm%dat(vid)%iskip + cm%dat(vid)%nblocks
#else
                call print_error( &
                    'NetCDF format is specified for ' // trim(cm%dat(vid)%fpath) // ' (' // trim(cm%dat(vid)%id_var) // &
                    ') but the module is not active.' // &
                    ' A version of MESH compiled with the NetCDF library must be used to read files in this format.')
                call program_abort()
#endif

            !> Unknown file format.
            case default
                call print_error(trim(cm%dat(vid)%fname) // ' (' // trim(cm%dat(vid)%id_var) // '): Unsupported file format.')
                call program_abort()

        end select

        return

999     continue

    end function

    !> Description:
    !>  Load data for the climate forcing variable from file.
    !>
    !> Input/output variables:
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and its unit.
    !*  vid: Index of the climate forcing variable.
    !*  iskip: Number of records to skip (default: 0).
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error updating the climate input forcing data.
    logical function update_data(shd, cm, vid, iskip) result(ENDDATA)

        !> 'shd_variables': For 'shd' variable.
        use shd_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: vid, iskip

        !> Input/Output variables.
        type(clim_info) cm

        !> Initialize the return variable.
        ENDDATA = .false.

        !> Return if the file is not open.
        if (.not. cm%dat(vid)%fopen) return

        !> Read data (returns if the current block has already been read to memory).
        ENDDATA = load_data(shd, cm, vid, iskip)

        !> Return an error if the block contains the NO_DATA value.
        if (any(cm%dat(vid)%blocks(:, cm%dat(vid)%iblock) == NO_DATA) .and. iskip == 0) goto 999

        return

999     ENDDATA = .true.

    end function

end module
