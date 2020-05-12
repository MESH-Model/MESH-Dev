module nc_io

    !* 'sa_mesh_common': For 'print_routines' and 'shd_variables'.
    !* 'model_files_variables': For I/O constants.
    !* 'model_dates': For 'ic' counter.
    !* 'netcdf': For netCDF library.
    !* 'typesizes': For data types used by netCDF library.
    use sa_mesh_common
    use parse_utilities
    use model_files_variables
    use model_dates
    use netcdf
    use typesizes
    use strings

    implicit none

    interface nc4_get_start_time
        module procedure nc4_get_time_nf90_int
    end interface

    interface nc4_get_attribute
        module procedure nc4_get_attribute_nf90_int
        module procedure nc4_get_attribute_nf90_float
        module procedure nc4_get_attribute_nf90_double
        module procedure nc4_get_attribute_nf90_char
    end interface

    interface nc4_get_variable_scalar
        module procedure nc4_get_variable_scalar_nf90_int
        module procedure nc4_get_variable_scalar_nf90_float
        module procedure nc4_get_variable_scalar_nf90_double
        module procedure nc4_get_variable_scalar_nf90_char
    end interface

    interface nc4_get_variable_n
        module procedure nc4_get_variable_n_nf90_int
        module procedure nc4_get_variable_n_nf90_float
        module procedure nc4_get_variable_n_nf90_double
        module procedure nc4_get_variable_n_nf90_char
    end interface

    interface nc4_get_variable_xy
        module procedure nc4_get_variable_xy_nf90_int
        module procedure nc4_get_variable_xy_nf90_float
        module procedure nc4_get_variable_xy_nf90_double
    end interface

    interface nc4_get_variable_xym
        module procedure nc4_get_variable_xym_nf90_int
        module procedure nc4_get_variable_xym_nf90_float
        module procedure nc4_get_variable_xym_nf90_double
    end interface

    interface nc4_get_variable_xylm
        module procedure nc4_get_variable_xylm_nf90_int
        module procedure nc4_get_variable_xylm_nf90_float
        module procedure nc4_get_variable_xylm_nf90_double
    end interface

    interface nc4_get_variable_xyt
        module procedure nc4_get_variable_xyt_nf90_int
        module procedure nc4_get_variable_xyt_nf90_float
        module procedure nc4_get_variable_xyt_nf90_double
    end interface

    interface nc4_add_time
        module procedure nc4_add_time_nf90_int
        module procedure nc4_add_time_nf90_float
        module procedure nc4_add_time_nf90_double
    end interface

    interface nc4_add_attribute
        module procedure nc4_add_attribute_nf90_int
        module procedure nc4_add_attribute_nf90_float
        module procedure nc4_add_attribute_nf90_double
        module procedure nc4_add_attribute_nf90_char
    end interface

    interface nc4_add_dimension
        module procedure nc4_define_dimension
    end interface

    interface nc4_add_variable_scalar
        module procedure nc4_add_variable_scalar_nf90_int
        module procedure nc4_add_variable_scalar_nf90_float
        module procedure nc4_add_variable_scalar_nf90_double
        module procedure nc4_add_variable_scalar_nf90_char
    end interface

    interface nc4_add_variable_n
        module procedure nc4_define_variable_n_nf90_int
        module procedure nc4_define_variable_n_nf90_float
        module procedure nc4_define_variable_n_nf90_double
        module procedure nc4_define_variable_n_nf90_char
        module procedure nc4_add_variable_n_nf90_int
        module procedure nc4_add_variable_n_nf90_float
        module procedure nc4_add_variable_n_nf90_double
        module procedure nc4_add_variable_n_nf90_char
    end interface

    interface nc4_add_variable_xy
        module procedure nc4_define_variable_xy_nf90_int
        module procedure nc4_define_variable_xy_nf90_float
        module procedure nc4_define_variable_xy_nf90_double
        module procedure nc4_add_variable_xy_nf90_int
        module procedure nc4_add_variable_xy_nf90_float
        module procedure nc4_add_variable_xy_nf90_double
    end interface

    interface nc4_add_variable_xym
        module procedure nc4_define_variable_xym_nf90_int
        module procedure nc4_define_variable_xym_nf90_float
        module procedure nc4_define_variable_xym_nf90_double
        module procedure nc4_add_variable_xym_nf90_int
        module procedure nc4_add_variable_xym_nf90_float
        module procedure nc4_add_variable_xym_nf90_double
    end interface

    interface nc4_add_variable_xylm
        module procedure nc4_define_variable_xylm_nf90_int
        module procedure nc4_define_variable_xylm_nf90_float
        module procedure nc4_define_variable_xylm_nf90_double
        module procedure nc4_add_variable_xylm_nf90_int
        module procedure nc4_add_variable_xylm_nf90_float
        module procedure nc4_add_variable_xylm_nf90_double
    end interface

    interface nc4_add_variable_xyt
        module procedure nc4_define_variable_xyt_nf90_int
        module procedure nc4_define_variable_xyt_nf90_float
        module procedure nc4_define_variable_xyt_nf90_double
        module procedure nc4_add_variable_xyt_nf90_int
        module procedure nc4_add_variable_xyt_nf90_float
        module procedure nc4_add_variable_xyt_nf90_double
    end interface

    interface nc4_add_vname
        module procedure nc4_add_vname_xy_nf90_float
        module procedure nc4_add_vname_xyt_nf90_float
    end interface

    interface nc4_write_field
        module procedure nc4_write_field_xy_nf90_float
        module procedure nc4_write_field_xyt_nf90_float
    end interface

    contains

    subroutine nc4_open_input(fname, iun, ierr)

        !> Input variables.
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: iun, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Open the file with read access.
        ierr = nf90_open(fname, NF90_NOWRITE, iun)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning('Unable to open file (Code ' // trim(adjustl(code)) // '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_proj(iun, fname, proj_name, datum, zone_id, ierr)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname

        !> Output variables.
        character(len = *), intent(out) :: proj_name, datum
        character(len = *), intent(out), optional :: zone_id
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) field, code
        real val1, val2
        integer vid, z

        !> Initialize output variables.
        ierr = NF90_NOERR
        proj_name = 'UNKNOWN'
        datum = 'UNKNOWN'
        zone_id = 'UNKNOWN'

        !> Create variable.
        ierr = nf90_inq_varid(iun, 'crs', vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                'File does not contain CRS projection (Code ' // trim(adjustl(code)) // '): ' // trim(fname))
            return
        end if

        !> Get projection.
        ierr = nf90_get_att(iun, vid, 'grid_mapping_name', field)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "The file is missing '" // 'grid_mapping_name' // "' in file (Code: " // trim(adjustl(code)) // "): " // &
                trim(fname))
        else
            select case (lowercase(field))
                case ('latitude_longitude')
                    proj_name = 'LATLONG'
                case default
                    call print_warning("Unknown or unsupported projection '" // trim(field) // "' in file: " // trim(fname))
            end select
        end if

        !> Get datum.
        ierr = NF90_NOERR
        z = nf90_get_att(iun, vid, 'semi_major_axis', val1)
        if (z /= NF90_NOERR) then
            write(code, FMT_GEN) z
            call print_warning( &
                "The file is missing '" // 'semi_major_axis' // "' in file (Code: " // trim(adjustl(code)) // "): " // &
                trim(fname))
            ierr = z
        end if
        z = nf90_get_att(iun, vid, 'inverse_flattening', val2)
        if (z /= NF90_NOERR) then
            write(code, FMT_GEN) z
            call print_warning( &
                "The file is missing '" // 'inverse_flattening' // "' in file (Code: " // trim(adjustl(code)) // "): " // &
                trim(fname))
            ierr = z
        end if
        if (ierr == NF90_NOERR) then

            !> Ellipsoid/datum specification (from EnSim/GK manual; version: September, 2010).
            if (val1 == 6378137.0 .and. val2 == 298.257223563) then
                datum = 'WGS84'
            else if (val1 == 6378135.0 .and. val2 == 298.26) then
                datum = 'WGS72'
            else if (val1 == 6378137.0 .and. val2 == 298.257222101) then
                datum = 'NAD83'
            else if (val1 == 6378206.4 .and. val2 == 294.9786982) then
                datum = 'NAD27'
            else if (val1 == 6371000.0 .and. val2 == 0.0) then
                datum = 'SPHERE'
            else
                write(line, FMT_CSV) val1, val2
                call print_warning("Unknown or unsupported datum (" // trim(adjustl(line)) // ") in file: " // trim(fname))
            end if
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_time_nf90_int( &
        iun, fname, year, jday, month, day, hour, minutes, seconds, ierr, &
        time_shift, name_time)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: time_shift
        character(len = *), intent(in), optional :: name_time

        !> Output variables.
        integer(kind = FourByteInt), intent(out) :: year, jday, month, day, hour, minutes, seconds
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) time_calendar, time_units, vname, field, code
        character(len = DEFAULT_LINE_LENGTH) units_attribute
        integer t0_year, t0_month, t0_day, t0_hour, t0_mins, t0_seconds, dtype, vid, z
        integer(kind = FourByteInt) tt_i4(2)
        real(kind = FourByteReal) tt_r4(2)
        real(kind = EightByteReal) tt_r8(2), t0_r8, t1_r8

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the ID of the variable.
        if (present(name_time)) then
            vname = trim(name_time)
        else
            vname = 'time'
        end if
        ierr = nf90_inq_varid(iun, vname, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(vname) // "' cound not be found in file (Code: " // trim(adjustl(code)) // "): " // &
                trim(fname))
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units_attribute)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The units are missing for '" // trim(vname) // "' in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Check the units of the time dimension.
        !> Only dates of the Gregorian calendar type are supported.
        call parse_datetime(units_attribute, t0_year, t0_month, t0_day, t0_hour, t0_mins, t0_seconds, z)
        ierr = nf90_get_att(iun, vid, 'calendar', time_calendar)
        if (ierr /= NF90_NOERR .or. lowercase(time_calendar) /= 'gregorian') then
            call print_warning( &
                "The reference calendar for '" // trim(vname) // "' is not set or not equal to '" // &
                'Gregorian' // "' in file: " // trim(fname))
        end if
        if (z /= 0) then
            call print_error("The format of the units of '" // trim(vname) // "' is unsupported in file: " // trim(fname))
            call print_message('Expected format: [seconds/minutes/hours/days] since yyyy/MM/dd HH:mm:ss[.SSS]')
            ierr = 1
            return
        else if (t0_year < 1601) then
            write(field, FMT_GEN) t0_year
            call print_error( &
                'The reference year (' // trim(adjustl(field)) // ') is less than 1601.' // &
                ' The reference calendar does not correpond to the Gregorian calendar.')
            call print_message( &
                " The time-series of '" // trim(vname) // "' cannot be processed in file: " // &
                trim(fname))
            ierr = 1
            return
        end if

        !> Get the data type of the variable.
        !> Only integer, float, and double types are supported.
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr == NF90_NOERR) then
            select case (dtype)
                case (NF90_INT)
                    ierr = nf90_get_var(iun, vid, tt_i4, start = (/1/), count = (/1/))
                    if (ierr == NF90_NOERR) then
                        t1_r8 = real(tt_i4(1), kind = EightByteReal)
                    end if
                case (NF90_FLOAT)
                    ierr = nf90_get_var(iun, vid, tt_r4, start = (/1/), count = (/1/))
                    if (ierr == NF90_NOERR) then
                        t1_r8 = real(tt_r4(1), kind = EightByteReal)
                    end if
                case (NF90_DOUBLE)
                    ierr = nf90_get_var(iun, vid, tt_r8, start = (/1/), count = (/1/))
                    if (ierr == NF90_NOERR) then
                        t1_r8 = tt_r8(1)
                    end if
                case default
                    ierr = NF90_EBADTYPE
            end select
        end if
        if (ierr /= NF90_NOERR) then
            call print_error("Unsupported data type for '" // trim(vname) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Calculate the reference date from the units of the time dimension.
        !> Only units of seconds, minutes, hours, and days are supported.
        jday = get_jday(t0_month, t0_day, t0_year)
        t0_r8 = get_jdate(t0_year, jday)*24.0 + t0_hour + t0_mins/60.0 + t0_seconds/60.0/60.0
        if (present(time_shift)) then
            t0_r8 = t0_r8 + time_shift
        end if
        read(units_attribute, *) time_units
        select case (time_units)
            case ('seconds')
                t0_r8 = t0_r8 + t1_r8/60.0/60.0
            case ('minutes')
                t0_r8 = t0_r8 + t1_r8/60.0
            case ('hours')
                t0_r8 = t0_r8 + t1_r8
            case ('days')
                t0_r8 = t0_r8 + t1_r8*24.0
            case default
                ierr = NF90_EINVAL
        end select
        if (ierr /= NF90_NOERR) then
            call print_error( &
                "The units of '" // trim(adjustl(time_units)) // "' are unsupported for '" // trim(vname) // "' in file: " // &
                trim(fname))
            ierr = 1
            return
        end if

        !> Calculate the date of the first record in the file.
        !> Assumes dates increase along the time dimension.
        year = floor(t0_r8/24.0/365.25) + 1601
        jday = floor(t0_r8/24.0) - floor((year - 1601)*365.25)
        call Julian2MonthDay(jday, year, month, day)
        hour = floor(t0_r8) - get_jdate(year, jday)*24
        minutes = floor((t0_r8 - floor(t0_r8))*60.0)
        seconds = floor((t0_r8 - floor(t0_r8))*60.0*60.0 - minutes*60.0)

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_attribute_nf90_int(iun, fname, attribute_name, attribute_value, ierr, vid)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attribute_name
        integer, intent(in), optional :: vid

        !> Output variables.
        integer(kind = FourByteInt), intent(out) :: attribute_value
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer v

        !> Initialize output variables.
        ierr = NF90_NOERR

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_get_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the attribute '" // trim(attribute_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            attribute_value = NF90_FILL_INT
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_attribute_nf90_float(iun, fname, attribute_name, attribute_value, ierr, vid)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attribute_name
        integer, intent(in), optional :: vid

        !> Output variables.
        real(kind = FourByteReal), intent(out) :: attribute_value
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer v

        !> Initialize output variables.
        ierr = NF90_NOERR

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_get_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the attribute '" // trim(attribute_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            attribute_value = NF90_FILL_FLOAT
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_attribute_nf90_double(iun, fname, attribute_name, attribute_value, ierr, vid)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attribute_name
        integer, intent(in), optional :: vid

        !> Output variables.
        real(kind = EightByteReal), intent(out) :: attribute_value
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer v

        !> Initialize output variables.
        ierr = NF90_NOERR

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_get_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the attribute '" // trim(attribute_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            attribute_value = NF90_FILL_DOUBLE
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_attribute_nf90_char(iun, fname, attribute_name, attribute_value, ierr, vid)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attribute_name
        integer, intent(in), optional :: vid

        !> Output variables.
        character(len = *), intent(out) :: attribute_value
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer v

        !> Initialize output variables.
        ierr = NF90_NOERR

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_get_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the attribute '" // trim(attribute_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            attribute_value = NF90_FILL_CHAR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_dimension(iun, fname, dim_name, did, ierr, dim_length)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, dim_name

        !> Output variables.
        integer, intent(out) :: did, ierr

        !> Output variables (optional).
        integer, intent(out), optional :: dim_length

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname, field, code
        integer, dimension(:), allocatable :: dimids
        integer dim_len, ndims, dtype, vid

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the ID of the dimension.
        ierr = nf90_inq_dimid(iun, dim_name, did)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The dimension '" // trim(dim_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // "): " // &
                trim(fname))
            ierr = 1
            return
        end if

        !> Get the size of the dimension.
        if (present(dim_length)) then
            ierr = nf90_inquire_dimension(iun, did, len = dim_length)
            if (ierr /= NF90_NOERR) then
                call print_error( &
                    "Unable to get the size of the dimension '" // trim(dim_name) // "' in file (Code: " // trim(adjustl(code)) // &
                    "): " // trim(fname))
                ierr = 1
                return
            end if
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_scalar_nf90_int(iun, fname, standard_name, dat, ierr, vid, units, fill)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Output variables.
        integer(kind = FourByteInt), intent(out) :: dat
        integer, intent(out) :: ierr

        !> Output variables (optional).
        integer, intent(out), optional :: vid
        integer(kind = FourByteInt), intent(out), optional :: fill
        character(len = DEFAULT_LINE_LENGTH), intent(out), optional :: units

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer dtype, v

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the ID of the variable.
        ierr = nf90_inq_varid(iun, standard_name, v)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if
        if (present(vid)) vid = v

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, v, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_INT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'int' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        if (present(units)) then
            ierr = nf90_get_att(iun, v, 'units', units)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                    trim(adjustl(code)) // "): " // trim(fname))
                units = ''
            end if
        end if

        !> Get the fill value of the variable.
        if (present(fill)) then
            ierr = nf90_get_att(iun, v, '_FillValue', fill)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                    // trim(adjustl(code)) // "): " // trim(fname))
                fill = NF90_FILL_INT
            end if
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, v, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_scalar_nf90_float(iun, fname, standard_name, dat, ierr, vid, units, fill)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Output variables.
        real(kind = FourByteReal), intent(out) :: dat
        integer, intent(out) :: ierr

        !> Output variables (optional).
        integer, intent(out), optional :: vid
        real(kind = FourByteReal), intent(out), optional :: fill
        character(len = DEFAULT_LINE_LENGTH), intent(out), optional :: units

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer dtype, v

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the ID of the variable.
        ierr = nf90_inq_varid(iun, standard_name, v)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if
        if (present(vid)) vid = v

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, v, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_FLOAT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'float' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        if (present(units)) then
            ierr = nf90_get_att(iun, v, 'units', units)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                    trim(adjustl(code)) // "): " // trim(fname))
                units = ''
            end if
        end if

        !> Get the fill value of the variable.
        if (present(fill)) then
            ierr = nf90_get_att(iun, v, '_FillValue', fill)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                    // trim(adjustl(code)) // "): " // trim(fname))
                fill = NF90_FILL_FLOAT
            end if
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, v, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_scalar_nf90_double(iun, fname, standard_name, dat, ierr, vid, units, fill)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Output variables.
        real(kind = EightByteReal), intent(out) :: dat
        integer, intent(out) :: ierr

        !> Output variables (optional).
        integer, intent(out), optional :: vid
        real(kind = EightByteReal), intent(out), optional :: fill
        character(len = DEFAULT_LINE_LENGTH), intent(out), optional :: units

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer dtype, v

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the ID of the variable.
        ierr = nf90_inq_varid(iun, standard_name, v)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if
        if (present(vid)) vid = v

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, v, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_DOUBLE) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'double' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        if (present(units)) then
            ierr = nf90_get_att(iun, v, 'units', units)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                    trim(adjustl(code)) // "): " // trim(fname))
                units = ''
            end if
        end if

        !> Get the fill value of the variable.
        if (present(fill)) then
            ierr = nf90_get_att(iun, v, '_FillValue', fill)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                    // trim(adjustl(code)) // "): " // trim(fname))
                fill = NF90_FILL_FLOAT
            end if
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, v, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_scalar_nf90_char(iun, fname, standard_name, dat, ierr, vid, units, fill, name_dim_char_length)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim_char_length

        !> Output variables.
        character(len = *), intent(out) :: dat
        integer, intent(out) :: ierr

        !> Output variables (optional).
        integer, intent(out), optional :: vid
        character(len = 1), intent(out), optional :: fill
        character(len = DEFAULT_LINE_LENGTH), intent(out), optional :: units

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname_c, field, code
        integer, dimension(:), allocatable :: dimids
        integer ndims, dim_len, dtype, v

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, v)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if
        if (present(vid)) vid = v

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, v, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_CHAR) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'char' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        if (present(units)) then
            ierr = nf90_get_att(iun, v, 'units', units)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                    trim(adjustl(code)) // "): " // trim(fname))
                units = ''
            end if
        end if

        !> Get the fill value of the variable.
        if (present(fill)) then
            ierr = nf90_get_att(iun, v, '_FillValue', fill)
            if (ierr /= NF90_NOERR) then
                write(code, FMT_GEN) ierr
                call print_warning( &
                    "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                    // trim(adjustl(code)) // "): " // trim(fname))
                fill = NF90_FILL_CHAR
            end if
        end if

        !> Get the dimensions of the variable.
        ndims = 0
        ierr = nf90_inquire_variable(iun, v, ndims = ndims)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (ndims /= 1 .and. ndims /= 0) then
            write(field, FMT_GEN) ndims
            call print_error( &
                "The number of dimensions (" // trim(adjustl(field)) // ") is not supported for the scalar data type of '" // &
                "char" // "' for '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Multi-character string.
        if (ndims > 0) then

            !> Check the dimension name.
            if (present(name_dim_char_length)) then
                dname_c = trim(name_dim_char_length)
            else
                dname_c = 'DEFAULT_FIELD_LENGTH'
            end if
            allocate(dimids(ndims), stat = ierr)
            if (ierr == 0) ierr = nf90_inquire_variable(iun, v, dimids = dimids)
            if (ierr == 0) ierr = nf90_inquire_dimension(iun, dimids(1), name = field)
            if (ierr == NF90_NOERR) then
                if (lowercase(field) /= lowercase(dname_c)) then
                    call print_warning( &
                        "The name of the dimension '" // trim(field) // "' does not match the expected name '" // trim(dname_c) // &
                        " for '" // trim(standard_name) // "' in file: " // trim(fname))
                end if
            end if
            if (ierr /= NF90_NOERR) then
                ierr = 1
                return
            end if

            !> Get the size of the dimension.
            ierr = nf90_inquire_dimension(iun, dimids(1), len = dim_len)
            if (ierr /= NF90_NOERR) then
                call print_error( &
                    "Unable to get the size of the dimension '" // trim(field) // "' for '" // trim(standard_name) // &
                    "' in file (Code: " // trim(adjustl(code)) // "): " // trim(fname))
                ierr = 1
                return
            end if

            !> Check the string length.
            if (len(dat) /= dim_len) then
                write(code, FMT_GEN) len(dat)
                write(field, FMT_GEN) dim_len
                call print_error( &
                    "The length of the character-string (" // trim(adjustl(field)) // ") for '" // trim(standard_name) // &
                    "' is different from the expected length of (" // trim(adjustl(code)) // ") in file: " // trim(fname))
                ierr = 1
                return
            end if
        end if

        !> Read variable.
        if (ierr == 0) ierr = nf90_get_var(iun, v, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_n_nf90_int(iun, fname, standard_name, dat, units, fill, ierr, name_dim)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim

        !> Output variables.
        integer(kind = FourByteInt), dimension(:), allocatable, intent(out) :: dat
        integer(kind = FourByteInt), intent(out) :: fill
        character(len = DEFAULT_LINE_LENGTH), intent(out) :: units
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname, field, code
        integer, dimension(:), allocatable :: dimids
        integer dim_len, ndims, dtype, vid

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the ID of the variable.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_INT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'int' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_INT
        end if

        !> Get the dimensions of the variable.
        ndims = 1
        ierr = nf90_inquire_variable(iun, vid, ndims = ndims)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (ndims /= 1) then
            write(field, FMT_GEN) ndims
            call print_error( &
                "The number of dimensions (" // trim(adjustl(field)) // ") does not match the expected number of dimensions " // &
                " of 1 for '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Allocate the variable.
        if (present(name_dim)) then
            dname = trim(name_dim)
        else
            dname = trim(standard_name)
        end if
        allocate(dimids(ndims), stat = ierr)
        if (ierr == 0) ierr = nf90_inquire_variable(iun, vid, dimids = dimids)
        if (ierr == 0) ierr = nf90_inquire_dimension(iun, dimids(1), name = field)
        if (ierr == NF90_NOERR) then
            if (lowercase(field) /= lowercase(dname)) then
                call print_warning( &
                    "The name of the dimension '" // trim(field) // "' does not match the expected name '" // trim(dname) // &
                    " for '" // trim(standard_name) // "' in file: " // trim(fname))
            end if
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the size of the dimension.
        ierr = nf90_inquire_dimension(iun, dimids(1), len = dim_len)
        if (ierr /= NF90_NOERR) then
            call print_error( &
                "Unable to get the size of the dimension '" // trim(field) // "' for '" // trim(standard_name) // &
                "' in file (Code: " // trim(adjustl(code)) // "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Allocate the local variable.
        if (allocated(dat)) deallocate(dat, stat = ierr)
        if (ierr == 0) allocate(dat(dim_len), stat = ierr)
        if (ierr /= 0) then
            call print_error("Unable to allocate a local variable for '" // trim(standard_name) // "'.")
            ierr = 1
            return
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_n_nf90_float(iun, fname, standard_name, dat, units, fill, ierr, name_dim)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim

        !> Output variables.
        real(kind = FourByteReal), dimension(:), allocatable, intent(out) :: dat
        real(kind = FourByteReal), intent(out) :: fill
        character(len = DEFAULT_LINE_LENGTH), intent(out) :: units
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname, field, code
        integer, dimension(:), allocatable :: dimids
        integer dim_len, ndims, dtype, vid

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the ID of the variable.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_FLOAT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'float' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_FLOAT
        end if

        !> Get the dimensions of the variable.
        ndims = 1
        ierr = nf90_inquire_variable(iun, vid, ndims = ndims)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (ndims /= 1) then
            write(field, FMT_GEN) ndims
            call print_error( &
                "The number of dimensions (" // trim(adjustl(field)) // ") does not match the expected number of dimensions " // &
                " of 1 for '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Allocate the variable.
        if (present(name_dim)) then
            dname = trim(name_dim)
        else
            dname = trim(standard_name)
        end if
        allocate(dimids(ndims), stat = ierr)
        if (ierr == 0) ierr = nf90_inquire_variable(iun, vid, dimids = dimids)
        if (ierr == 0) ierr = nf90_inquire_dimension(iun, dimids(1), name = field)
        if (ierr == NF90_NOERR) then
            if (lowercase(field) /= lowercase(dname)) then
                call print_warning( &
                    "The name of the dimension '" // trim(field) // "' does not match the expected name '" // trim(dname) // &
                    " for '" // trim(standard_name) // "' in file: " // trim(fname))
            end if
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the size of the dimension.
        ierr = nf90_inquire_dimension(iun, dimids(1), len = dim_len)
        if (ierr /= NF90_NOERR) then
            call print_error( &
                "Unable to get the size of the dimension '" // trim(field) // "' for '" // trim(standard_name) // &
                "' in file (Code: " // trim(adjustl(code)) // "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Allocate the local variable.
        if (allocated(dat)) deallocate(dat, stat = ierr)
        if (ierr == 0) allocate(dat(dim_len), stat = ierr)
        if (ierr /= 0) then
            call print_error("Unable to allocate a local variable for '" // trim(standard_name) // "'.")
            ierr = 1
            return
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_n_nf90_double(iun, fname, standard_name, dat, units, fill, ierr, name_dim)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim

        !> Output variables.
        real(kind = EightByteReal), dimension(:), allocatable, intent(out) :: dat
        real(kind = EightByteReal), intent(out) :: fill
        character(len = DEFAULT_LINE_LENGTH), intent(out) :: units
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname, field, code
        integer, dimension(:), allocatable :: dimids
        integer dim_len, ndims, dtype, vid

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the ID of the variable.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_DOUBLE) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'double' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_FLOAT
        end if

        !> Get the dimensions of the variable.
        ndims = 1
        ierr = nf90_inquire_variable(iun, vid, ndims = ndims)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (ndims /= 1) then
            write(field, FMT_GEN) ndims
            call print_error( &
                "The number of dimensions (" // trim(adjustl(field)) // ") does not match the expected number of dimensions " // &
                " of 1 for '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Allocate the variable.
        if (present(name_dim)) then
            dname = trim(name_dim)
        else
            dname = trim(standard_name)
        end if
        allocate(dimids(ndims), stat = ierr)
        if (ierr == 0) ierr = nf90_inquire_variable(iun, vid, dimids = dimids)
        if (ierr == 0) ierr = nf90_inquire_dimension(iun, dimids(1), name = field)
        if (ierr == NF90_NOERR) then
            if (lowercase(field) /= lowercase(dname)) then
                call print_warning( &
                    "The name of the dimension '" // trim(field) // "' does not match the expected name '" // trim(dname) // &
                    " for '" // trim(standard_name) // "' in file: " // trim(fname))
            end if
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the size of the dimension.
        ierr = nf90_inquire_dimension(iun, dimids(1), len = dim_len)
        if (ierr /= NF90_NOERR) then
            call print_error( &
                "Unable to get the size of the dimension '" // trim(field) // "' for '" // trim(standard_name) // &
                "' in file (Code: " // trim(adjustl(code)) // "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Allocate the local variable.
        if (allocated(dat)) deallocate(dat, stat = ierr)
        if (ierr == 0) allocate(dat(dim_len), stat = ierr)
        if (ierr /= 0) then
            call print_error("Unable to allocate a local variable for '" // trim(standard_name) // "'.")
            ierr = 1
            return
        end if

        !> Read variable.
        ierr = nf90_get_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_n_nf90_char(iun, fname, standard_name, dat, units, fill, ierr, name_dim, name_dim_char_length)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_dim, name_dim_char_length

        !> Output variables.
        character(len = *), dimension(:), allocatable, intent(out) :: dat
        character(len = 1), intent(out) :: fill
        character(len = DEFAULT_LINE_LENGTH), intent(out) :: units
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname_c, dname_n, field, code
        integer, dimension(:), allocatable :: dimids, dim_lengths
        integer dim_len, ndims, dtype, dsort_c, dsort_n, vid

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        if (present(name_dim)) then
            dname_n = trim(name_dim)
        else
            dname_n = trim(standard_name)
        end if
        if (present(name_dim_char_length)) then
            dname_c = trim(name_dim_char_length)
        else
            dname_c = 'DEFAULT_FIELD_LENGTH'
        end if
        call nc4_get_xy_order(iun, fname, standard_name, vid, dsort_c, dsort_n, dim_lengths, ierr, dname_c, dname_n)
        if (ierr /= 0) return

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_CHAR) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'char' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_CHAR
        end if

        !> Check the string length.
        if (len(dat) /= dim_lengths(dsort_c)) then
            write(code, FMT_GEN) len(dat)
            write(field, FMT_GEN) dim_lengths(dsort_c)
            call print_error( &
                "The length of the character-string (" // trim(adjustl(field)) // ") for '" // trim(standard_name) // &
                "' is different from the expected length of (" // trim(adjustl(code)) // ") in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Read variable.
        allocate(dat(dim_lengths(dsort_n)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_xy_order(iun, fname, standard_name, vid, dim_x, dim_y, dim_lengths, ierr, name_x, name_y)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables.
        character(len = *), intent(in), optional :: name_x, name_y

        !> Output variables.
        integer, intent(out) :: dim_x, dim_y, ierr
        integer, dimension(:), allocatable, intent(out) :: dim_lengths

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname_x, dname_y, field, code
        integer, dimension(:), allocatable :: dimids
        integer ndims, i, z

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the dimensions of the variable.
        ndims = 2
        ierr = nf90_inquire_variable(iun, vid, ndims = ndims)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (ndims /= 2) then
            write(field, FMT_GEN) ndims
            call print_error( &
                "The number of dimensions (" // trim(adjustl(field)) // ") does not match the expected number of dimensions " // &
                " of 2 for '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get information about the dimensions.
        if (present(name_x)) then
            dname_x = trim(name_x)
        else
            dname_x = 'lon'
        end if
        dim_x = 0
        if (present(name_y)) then
            dname_y = trim(name_y)
        else
            dname_y = 'lat'
        end if
        dim_y = 0
        if (allocated(dim_lengths)) deallocate(dim_lengths, stat = ierr)
        if (ierr == 0) allocate(dimids(ndims), dim_lengths(ndims), stat = ierr)
        if (ierr == 0) ierr = nf90_inquire_variable(iun, vid, dimids = dimids)
        do i = 1, ndims
            if (ierr == 0) ierr = nf90_inquire_dimension(iun, dimids(i), name = field, len = dim_lengths(i))
            if (ierr == NF90_NOERR) then
                if (lowercase(field) == lowercase(dname_x)) then
                    dim_x = i
                else if (lowercase(field) == lowercase(dname_y)) then
                    dim_y = i
                end if
            else
                exit
            end if
        end do
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading properties of the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            ierr = 1
            return
        end if
        z = 0
        if (dim_x == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'x' dimension ('" // trim(dname_x) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (dim_y == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'y' dimension ('" // trim(dname_y) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (z /= 0) then
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xy_nf90_int( &
        iun, fname, standard_name, dat, units, fill, ierr, &
        dim_length_x, dim_length_y, dim_x, dim_y)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, optional :: dim_length_x, dim_length_y, dim_x, dim_y

        !> Output variables.
        character(len = DEFAULT_LINE_LENGTH), intent(out) :: units
        integer(kind = FourByteInt), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        integer(kind = FourByteInt) :: dat(:, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer(kind = FourByteInt), allocatable :: dat2(:, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xy_order(iun, fname, standard_name, vid, x, y, dim_lengths, ierr)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_INT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'int' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_INT
        end if

        !> Read variable.
        allocate(dat2(dim_lengths(1), dim_lengths(2)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat2)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2) then
            dat = dat2
        else
            dat = transpose(dat2)
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xy_nf90_float(iun, fname, standard_name, dat, units, fill, ierr, dim_x, dim_y)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y

        !> Output variables.
        character(len = DEFAULT_LINE_LENGTH), intent(out) :: units
        real(kind = FourByteReal), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        real(kind = FourByteReal) :: dat(:, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real(kind = FourByteReal), allocatable :: dat2(:, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xy_order(iun, fname, standard_name, vid, x, y, dim_lengths, ierr)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_FLOAT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'float' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_FLOAT
        end if

        !> Read variable.
        allocate(dat2(dim_lengths(1), dim_lengths(2)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat2)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2) then
            dat = dat2
        else
            dat = transpose(dat2)
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xy_nf90_double(iun, fname, standard_name, dat, units, fill, ierr, dim_x, dim_y)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y

        !> Output variables.
        character(len = DEFAULT_LINE_LENGTH), intent(out) :: units
        real(kind = EightByteReal), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        real(kind = EightByteReal) :: dat(:, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real(kind = EightByteReal), allocatable :: dat2(:, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xy_order(iun, fname, standard_name, vid, x, y, dim_lengths, ierr)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_DOUBLE) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'double' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_DOUBLE
        end if

        !> Read variable.
        allocate(dat2(dim_lengths(1), dim_lengths(2)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat2)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2) then
            dat = dat2
        else
            dat = transpose(dat2)
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_xym_order(iun, fname, standard_name, vid, dim_x, dim_y, dim_m, dim_lengths, ierr, name_x, name_y, name_m)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables.
        character(len = *), intent(in), optional :: name_x, name_y, name_m

        !> Output variables.
        integer, intent(out) :: dim_x, dim_y, dim_m, ierr
        integer, dimension(:), allocatable, intent(out) :: dim_lengths

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname_x, dname_y, dname_m, field, code
        integer, dimension(:), allocatable :: dimids
        integer ndims, i, z

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the dimensions of the variable.
        ndims = 3
        ierr = nf90_inquire_variable(iun, vid, ndims = ndims)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (ndims /= 3) then
            write(field, FMT_GEN) ndims
            call print_error( &
                "The number of dimensions (" // trim(adjustl(field)) // ") does not match the expected number of dimensions " // &
                " of 3 for '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get information about the dimensions.
        if (present(name_x)) then
            dname_x = trim(name_x)
        else
            dname_x = 'lon'
        end if
        dim_x = 0
        if (present(name_y)) then
            dname_y = trim(name_y)
        else
            dname_y = 'lat'
        end if
        dim_y = 0
        if (present(name_m)) then
            dname_m = trim(name_m)
        else
            dname_m = 'gru'
        end if
        dim_m = 0
        if (allocated(dim_lengths)) deallocate(dim_lengths, stat = ierr)
        if (ierr == 0) allocate(dimids(ndims), dim_lengths(ndims), stat = ierr)
        if (ierr == 0) ierr = nf90_inquire_variable(iun, vid, dimids = dimids)
        do i = 1, ndims
            if (ierr == 0) ierr = nf90_inquire_dimension(iun, dimids(i), name = field, len = dim_lengths(i))
            if (ierr == NF90_NOERR) then
                if (lowercase(field) == lowercase(dname_x)) then
                    dim_x = i
                else if (lowercase(field) == lowercase(dname_y)) then
                    dim_y = i
                else if (lowercase(field) == lowercase(dname_m)) then
                    dim_m = i
                end if
            else
                exit
            end if
        end do
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading properties of the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            ierr = 1
            return
        end if
        z = 0
        if (dim_x == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'x' dimension ('" // trim(dname_x) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (dim_y == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'y' dimension ('" // trim(dname_y) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (dim_m == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'm' dimension ('" // trim(dname_m) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (z /= 0) then
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xym_nf90_int(iun, fname, standard_name, dat, units, fill, ierr, dim_x, dim_y, dim_m)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y, dim_m

        !> Output variables.
        character(len = *), intent(out) :: units
        integer(kind = FourByteInt), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        integer(kind = FourByteInt) :: dat(:, :, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer(kind = FourByteInt), allocatable :: dat3(:, :, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, m, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xym_order(iun, fname, standard_name, vid, x, y, m, dim_lengths, ierr)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y
        if (present(dim_m)) m = dim_m

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_INT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'int' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_INT
        end if

        !> Read variable.
        allocate(dat3(dim_lengths(1), dim_lengths(2), dim_lengths(3)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat3)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(1)
                        dat(x, y, m) = dat3(x, y, m)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(2)
                        dat(x, y, m) = dat3(y, x, m)
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(1)
                        dat(x, y, m) = dat3(x, m, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(3)
                        dat(x, y, m) = dat3(y, m, x)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(2)
                        dat(x, y, m) = dat3(m, x, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(3)
                        dat(x, y, m) = dat3(m, y, x)
                    end do
                end do
            end do
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xym_nf90_float(iun, fname, standard_name, dat, units, fill, ierr, dim_x, dim_y, dim_m)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y, dim_m

        !> Output variables.
        character(len = *), intent(out) :: units
        real(kind = FourByteReal), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        real(kind = FourByteReal) :: dat(:, :, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real(kind = FourByteReal), allocatable :: dat3(:, :, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, m, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xym_order(iun, fname, standard_name, vid, x, y, m, dim_lengths, ierr)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y
        if (present(dim_m)) m = dim_m

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_FLOAT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'float' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_FLOAT
        end if

        !> Read variable.
        allocate(dat3(dim_lengths(1), dim_lengths(2), dim_lengths(3)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat3)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(1)
                        dat(x, y, m) = dat3(x, y, m)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(2)
                        dat(x, y, m) = dat3(y, x, m)
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(1)
                        dat(x, y, m) = dat3(x, m, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(3)
                        dat(x, y, m) = dat3(y, m, x)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(2)
                        dat(x, y, m) = dat3(m, x, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(3)
                        dat(x, y, m) = dat3(m, y, x)
                    end do
                end do
            end do
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xym_nf90_double(iun, fname, standard_name, dat, units, fill, ierr, dim_x, dim_y, dim_m)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y, dim_m

        !> Output variables.
        character(len = *), intent(out) :: units
        real(kind = EightByteReal), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        real(kind = EightByteReal) :: dat(:, :, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real(kind = EightByteReal), allocatable :: dat3(:, :, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, m, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xym_order(iun, fname, standard_name, vid, x, y, m, dim_lengths, ierr)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y
        if (present(dim_m)) m = dim_m

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_DOUBLE) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'double' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_DOUBLE
        end if

        !> Read variable.
        allocate(dat3(dim_lengths(1), dim_lengths(2), dim_lengths(3)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat3)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(1)
                        dat(x, y, m) = dat3(x, y, m)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(2)
                        dat(x, y, m) = dat3(y, x, m)
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(1)
                        dat(x, y, m) = dat3(x, m, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(3)
                        dat(x, y, m) = dat3(y, m, x)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(2)
                        dat(x, y, m) = dat3(m, x, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(3)
                        dat(x, y, m) = dat3(m, y, x)
                    end do
                end do
            end do
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_xylm_order( &
        iun, fname, standard_name, vid, dim_x, dim_y, dim_l, dim_m, dim_lengths, ierr, &
        name_x, name_y, name_l, name_m)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        character(len = *), intent(in), optional :: name_x, name_y, name_l, name_m

        !> Output variables.
        integer, intent(out) :: dim_x, dim_y, dim_l, dim_m, ierr
        integer, dimension(:), allocatable, intent(out) :: dim_lengths

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname_x, dname_y, dname_l, dname_m, field, code
        integer, dimension(:), allocatable :: dimids
        integer ndims, i, z

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the dimensions of the variable.
        ndims = 3
        ierr = nf90_inquire_variable(iun, vid, ndims = ndims)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (ndims /= 4) then
            write(field, FMT_GEN) ndims
            call print_error( &
                "The number of dimensions (" // trim(adjustl(field)) // ") does not match the expected number of dimensions " // &
                "of 4 for '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get information about the dimensions.
        if (present(name_x)) then
            dname_x = trim(name_x)
        else
            dname_x = 'lon'
        end if
        dim_x = 0
        if (present(name_y)) then
            dname_y = trim(name_y)
        else
            dname_y = 'lat'
        end if
        dim_y = 0
        if (present(name_l)) then
            dname_l = trim(name_l)
        else
            dname_l = 'level'
        end if
        dim_l = 0
        if (present(name_m)) then
            dname_m = trim(name_m)
        else
            dname_m = 'gru'
        end if
        dim_m = 0
        if (allocated(dim_lengths)) deallocate(dim_lengths, stat = ierr)
        if (ierr == 0) allocate(dimids(ndims), dim_lengths(ndims), stat = ierr)
        if (ierr == 0) ierr = nf90_inquire_variable(iun, vid, dimids = dimids)
        do i = 1, ndims
            if (ierr == 0) ierr = nf90_inquire_dimension(iun, dimids(i), name = field, len = dim_lengths(i))
            if (ierr == NF90_NOERR) then
                if (lowercase(field) == lowercase(dname_x)) then
                    dim_x = i
                else if (lowercase(field) == lowercase(dname_y)) then
                    dim_y = i
                else if (lowercase(field) == lowercase(dname_l)) then
                    dim_l = i
                else if (lowercase(field) == lowercase(dname_m)) then
                    dim_m = i
                end if
            else
                exit
            end if
        end do
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading properties of the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            ierr = 1
            return
        end if
        z = 0
        if (dim_x == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'x' dimension ('" // trim(dname_x) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (dim_y == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'y' dimension ('" // trim(dname_y) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (dim_l == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'l' dimension ('" // trim(dname_l) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (dim_m == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'm' dimension ('" // trim(dname_m) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (z /= 0) then
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xylm_nf90_int( &
        iun, fname, standard_name, dat, units, fill, ierr, &
        dim_x, dim_y, dim_l, dim_m, name_x, name_y, name_l, name_m)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y, dim_l, dim_m
        character(len = *), intent(in), optional :: name_x, name_y, name_l, name_m

        !> Output variables.
        character(len = *), intent(out) :: units
        integer(kind = FourByteInt), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        integer(kind = FourByteInt) :: dat(:, :, :, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer(kind = FourByteInt), allocatable :: dat4(:, :, :, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, l, m, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xylm_order(iun, fname, standard_name, vid, x, y, l, m, dim_lengths, ierr, name_x, name_y, name_l, name_m)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y
        if (present(dim_l)) l = dim_l
        if (present(dim_m)) m = dim_m

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_INT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'int' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_INT
        end if

        !> Read variable.
        allocate(dat4(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat4)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2 .and. l == 4 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, y, m, l)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. l == 4 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(y, x, m, l)
                        end do
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. l == 4 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, m, y, l)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. l == 4 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(y, m, x, l)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. l == 4 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(m, x, y, l)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. l == 4 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(m, y, x, l)
                        end do
                    end do
                end do
            end do
        end if
        if (x == 4 .and. y == 2 .and. l == 1 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(l, x, y, m)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 1 .and. l == 2 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(y, l, x, m)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 3 .and. l == 1 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(l, x, m, y)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 1 .and. l == 3 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(y, m, l, x)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 3 .and. l == 2 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(m, l, x, y)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 2 .and. l == 3 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(m, y, l, x)
                        end do
                    end do
                end do
            end do
        end if
        if (x == 1 .and. y == 4 .and. l == 2 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, l, y, m)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 4 .and. l == 1 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(l, y, x, m)
                        end do
                    end do
                end do
            end do
        else if (x == 1 .and. y == 4 .and. l == 3 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, m, l, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 4 .and. l == 1 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(l, y, m, x)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 4 .and. l == 3 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(m, x, l, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 4 .and. l == 2 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(m, l, y, x)
                        end do
                    end do
                end do
            end do
        end if
        if (x == 1 .and. y == 2 .and. l == 3 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, y, l, m)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. l == 3 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(y, x, l, m)
                        end do
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. l == 2 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, l, m, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. l == 2 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(y, l, m, x)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. l == 1 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(l, m, x, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. l == 1 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(l, m, y, x)
                        end do
                    end do
                end do
            end do
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xylm_nf90_float( &
        iun, fname, standard_name, dat, units, fill, ierr, &
        dim_x, dim_y, dim_l, dim_m, name_x, name_y, name_l, name_m)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y, dim_l, dim_m
        character(len = *), intent(in), optional :: name_x, name_y, name_l, name_m

        !> Output variables.
        character(len = *), intent(out) :: units
        real(kind = FourByteReal), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        real(kind = FourByteReal) :: dat(:, :, :, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real(kind = FourByteReal), allocatable :: dat4(:, :, :, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, l, m, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xylm_order(iun, fname, standard_name, vid, x, y, l, m, dim_lengths, ierr, name_x, name_y, name_l, name_m)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y
        if (present(dim_l)) l = dim_l
        if (present(dim_m)) m = dim_m

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_FLOAT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'float' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_FLOAT
        end if

        !> Read variable.
        allocate(dat4(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat4)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2 .and. l == 4 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, y, m, l)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. l == 4 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(y, x, m, l)
                        end do
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. l == 4 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, m, y, l)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. l == 4 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(y, m, x, l)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. l == 4 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(m, x, y, l)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. l == 4 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(m, y, x, l)
                        end do
                    end do
                end do
            end do
        end if
        if (x == 4 .and. y == 2 .and. l == 1 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(l, x, y, m)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 1 .and. l == 2 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(y, l, x, m)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 3 .and. l == 1 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(l, x, m, y)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 1 .and. l == 3 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(y, m, l, x)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 3 .and. l == 2 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(m, l, x, y)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 2 .and. l == 3 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(m, y, l, x)
                        end do
                    end do
                end do
            end do
        end if
        if (x == 1 .and. y == 4 .and. l == 2 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, l, y, m)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 4 .and. l == 1 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(l, y, x, m)
                        end do
                    end do
                end do
            end do
        else if (x == 1 .and. y == 4 .and. l == 3 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, m, l, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 4 .and. l == 1 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(l, y, m, x)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 4 .and. l == 3 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(m, x, l, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 4 .and. l == 2 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(m, l, y, x)
                        end do
                    end do
                end do
            end do
        end if
        if (x == 1 .and. y == 2 .and. l == 3 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, y, l, m)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. l == 3 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(y, x, l, m)
                        end do
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. l == 2 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, l, m, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. l == 2 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(y, l, m, x)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. l == 1 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(l, m, x, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. l == 1 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(l, m, y, x)
                        end do
                    end do
                end do
            end do
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xylm_nf90_double( &
        iun, fname, standard_name, dat, units, fill, ierr, &
        dim_x, dim_y, dim_l, dim_m, name_x, name_y, name_l, name_m)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y, dim_l, dim_m
        character(len = *), intent(in), optional :: name_x, name_y, name_l, name_m

        !> Output variables.
        character(len = *), intent(out) :: units
        real(kind = EightByteReal), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        real(kind = EightByteReal) :: dat(:, :, :, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real(kind = EightByteReal), allocatable :: dat4(:, :, :, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, l, m, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xylm_order(iun, fname, standard_name, vid, x, y, l, m, dim_lengths, ierr, name_x, name_y, name_l, name_m)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y
        if (present(dim_l)) l = dim_l
        if (present(dim_m)) m = dim_m

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_DOUBLE) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'double' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_DOUBLE
        end if

        !> Read variable.
        allocate(dat4(dim_lengths(1), dim_lengths(2), dim_lengths(3), dim_lengths(4)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat4)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2 .and. l == 4 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, y, m, l)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. l == 4 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(y, x, m, l)
                        end do
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. l == 4 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, m, y, l)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. l == 4 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(y, m, x, l)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. l == 4 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(m, x, y, l)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. l == 4 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(4)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(m, y, x, l)
                        end do
                    end do
                end do
            end do
        end if
        if (x == 4 .and. y == 2 .and. l == 1 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(l, x, y, m)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 1 .and. l == 2 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(y, l, x, m)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 3 .and. l == 1 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(l, x, m, y)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 1 .and. l == 3 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(y, m, l, x)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 3 .and. l == 2 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(m, l, x, y)
                        end do
                    end do
                end do
            end do
        else if (x == 4 .and. y == 2 .and. l == 3 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(4)
                            dat(x, y, l, m) = dat4(m, y, l, x)
                        end do
                    end do
                end do
            end do
        end if
        if (x == 1 .and. y == 4 .and. l == 2 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, l, y, m)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 4 .and. l == 1 .and. m == 3) then
            do m = 1, dim_lengths(3)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(l, y, x, m)
                        end do
                    end do
                end do
            end do
        else if (x == 1 .and. y == 4 .and. l == 3 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, m, l, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 4 .and. l == 1 .and. m == 2) then
            do m = 1, dim_lengths(2)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(l, y, m, x)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 4 .and. l == 3 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(m, x, l, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 4 .and. l == 2 .and. m == 1) then
            do m = 1, dim_lengths(1)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(4)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(m, l, y, x)
                        end do
                    end do
                end do
            end do
        end if
        if (x == 1 .and. y == 2 .and. l == 3 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, y, l, m)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. l == 3 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(3)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(y, x, l, m)
                        end do
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. l == 2 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(1)
                            dat(x, y, l, m) = dat4(x, l, m, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. l == 2 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(2)
                    do y = 1, dim_lengths(1)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(y, l, m, x)
                        end do
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. l == 1 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(3)
                        do x = 1, dim_lengths(2)
                            dat(x, y, l, m) = dat4(l, m, x, y)
                        end do
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. l == 1 .and. m == 4) then
            do m = 1, dim_lengths(4)
                do l = 1, dim_lengths(1)
                    do y = 1, dim_lengths(2)
                        do x = 1, dim_lengths(3)
                            dat(x, y, l, m) = dat4(l, m, y, x)
                        end do
                    end do
                end do
            end do
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_xyt_order(iun, fname, standard_name, vid, dim_x, dim_y, dim_t, dim_lengths, ierr, name_x, name_y, name_t)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables.
        character(len = *), intent(in), optional :: name_x, name_y, name_t

        !> Output variables.
        integer, intent(out) :: dim_x, dim_y, dim_t, ierr
        integer, dimension(:), allocatable, intent(out) :: dim_lengths

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) dname_x, dname_y, dname_t, field, code
        integer, dimension(:), allocatable :: dimids
        integer ndims, i, z

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the dimensions of the variable.
        ndims = 3
        ierr = nf90_inquire_variable(iun, vid, ndims = ndims)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (ndims /= 3) then
            write(field, FMT_GEN) ndims
            call print_error( &
                "The number of dimensions (" // trim(adjustl(field)) // ") does not match the expected number of dimensions " // &
                " of 3 for '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get information about the dimensions.
        if (present(name_x)) then
            dname_x = trim(name_x)
        else
            dname_x = 'lon'
        end if
        dim_x = 0
        if (present(name_y)) then
            dname_y = trim(name_y)
        else
            dname_y = 'lat'
        end if
        dim_y = 0
        if (present(name_t)) then
            dname_t = trim(name_t)
        else
            dname_t = 'time'
        end if
        dim_t = 0
        if (allocated(dim_lengths)) deallocate(dim_lengths, stat = ierr)
        if (ierr == 0) allocate(dimids(ndims), dim_lengths(ndims), stat = ierr)
        if (ierr == 0) ierr = nf90_inquire_variable(iun, vid, dimids = dimids)
        do i = 1, ndims
            if (ierr == 0) ierr = nf90_inquire_dimension(iun, dimids(i), name = field, len = dim_lengths(i))
            if (ierr == NF90_NOERR) then
                if (lowercase(field) == lowercase(dname_x)) then
                    dim_x = i
                else if (lowercase(field) == lowercase(dname_y)) then
                    dim_y = i
                else if (lowercase(field) == lowercase(dname_t)) then
                    dim_t = i
                end if
            else
                exit
            end if
        end do
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading properties of the dimensions of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            ierr = 1
            return
        end if
        z = 0
        if (dim_x == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'x' dimension ('" // trim(dname_x) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (dim_y == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 'y' dimension ('" // trim(dname_y) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (dim_t == 0) then
            call print_error( &
                "A dimension of the variable is not mapped to the expected 't' dimension ('" // trim(dname_t) // "') in file: " // &
                trim(fname))
            z = 1
        end if
        if (z /= 0) then
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xyt_nf90_int(iun, fname, standard_name, dat, units, fill, ierr, dim_x, dim_y, dim_t)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y, dim_t

        !> Output variables.
        character(len = *), intent(out) :: units
        integer(kind = FourByteInt), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        integer(kind = FourByteInt) :: dat(:, :, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer(kind = FourByteInt), allocatable :: dat3(:, :, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, t, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xyt_order(iun, fname, standard_name, vid, x, y, t, dim_lengths, ierr)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y
        if (present(dim_t)) t = dim_t

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_INT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'int' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_INT
        end if

        !> Read variable.
        allocate(dat3(dim_lengths(1), dim_lengths(2), dim_lengths(3)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat3)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2 .and. t == 3) then
            do t = 1, dim_lengths(3)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(1)
                        dat(x, y, t) = dat3(x, y, t)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. t == 3) then
            do t = 1, dim_lengths(3)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(2)
                        dat(x, y, t) = dat3(y, x, t)
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. t == 2) then
            do t = 1, dim_lengths(2)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(1)
                        dat(x, y, t) = dat3(x, t, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. t == 2) then
            do t = 1, dim_lengths(2)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(3)
                        dat(x, y, t) = dat3(y, t, x)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. t == 1) then
            do t = 1, dim_lengths(1)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(2)
                        dat(x, y, t) = dat3(t, x, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. t == 1) then
            do t = 1, dim_lengths(1)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(3)
                        dat(x, y, t) = dat3(t, y, x)
                    end do
                end do
            end do
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xyt_nf90_float(iun, fname, standard_name, dat, units, fill, ierr, dim_x, dim_y, dim_t)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y, dim_t

        !> Output variables.
        character(len = *), intent(out) :: units
        real(kind = FourByteReal), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        real(kind = FourByteReal) :: dat(:, :, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real(kind = FourByteReal), allocatable :: dat3(:, :, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, t, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xyt_order(iun, fname, standard_name, vid, x, y, t, dim_lengths, ierr)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y
        if (present(dim_t)) t = dim_t

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_FLOAT) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'float' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_FLOAT
        end if

        !> Read variable.
        allocate(dat3(dim_lengths(1), dim_lengths(2), dim_lengths(3)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat3)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2 .and. t == 3) then
            do t = 1, dim_lengths(3)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(1)
                        dat(x, y, t) = dat3(x, y, t)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. t == 3) then
            do t = 1, dim_lengths(3)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(2)
                        dat(x, y, t) = dat3(y, x, t)
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. t == 2) then
            do t = 1, dim_lengths(2)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(1)
                        dat(x, y, t) = dat3(x, t, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. t == 2) then
            do t = 1, dim_lengths(2)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(3)
                        dat(x, y, t) = dat3(y, t, x)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. t == 1) then
            do t = 1, dim_lengths(1)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(2)
                        dat(x, y, t) = dat3(t, x, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. t == 1) then
            do t = 1, dim_lengths(1)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(3)
                        dat(x, y, t) = dat3(t, y, x)
                    end do
                end do
            end do
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_get_variable_xyt_nf90_double(iun, fname, standard_name, dat, units, fill, ierr, dim_x, dim_y, dim_t)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_x, dim_y, dim_t

        !> Output variables.
        character(len = *), intent(out) :: units
        real(kind = EightByteReal), intent(out) :: fill
        integer, intent(out) :: ierr

        !> Input/output variable.
        real(kind = EightByteReal) :: dat(:, :, :)

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        real(kind = EightByteReal), allocatable :: dat3(:, :, :)
        integer, dimension(:), allocatable :: dim_lengths
        integer vid, dtype, x, y, t, i

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Check that the variable 'id_var' exists in the file.
        ierr = nf90_inq_varid(iun, standard_name, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "The variable '" // trim(standard_name) // "' cound not be found in file (Code: " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Get the order of the dimensions.
        call nc4_get_xyt_order(iun, fname, standard_name, vid, x, y, t, dim_lengths, ierr)
        if (ierr /= 0) return
        if (present(dim_x)) x = dim_x
        if (present(dim_y)) y = dim_y
        if (present(dim_t)) t = dim_t

        !> Check the data type of the variable.
        dtype = 0
        ierr = nf90_inquire_variable(iun, vid, xtype = dtype)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred reading the data type of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
        else if (dtype /= NF90_DOUBLE) then
            call print_error( &
                "The data type of '" // trim(standard_name) // "' is different from the expected data type of 'double' " // &
                "in file: " // trim(fname))
            ierr = 1
        end if
        if (ierr /= NF90_NOERR) then
            ierr = 1
            return
        end if

        !> Get the units of the variable.
        ierr = nf90_get_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the units of '" // trim(standard_name) // "' in file (Code: " // &
                trim(adjustl(code)) // "): " // trim(fname))
            units = ''
        end if

        !> Get the fill value of the variable.
        ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred reading the fill value of '" // trim(standard_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
            fill = NF90_FILL_DOUBLE
        end if

        !> Read variable.
        allocate(dat3(dim_lengths(1), dim_lengths(2), dim_lengths(3)), stat = ierr)
        if (ierr == 0) ierr = nf90_get_var(iun, vid, dat3)
        if (ierr /= NF90_NOERR) then
            call print_error("An error occurred reading '" // trim(standard_name) // "' in file: " // trim(fname))
            ierr = 1
            return
        end if
        if (x == 1 .and. y == 2 .and. t == 3) then
            do t = 1, dim_lengths(3)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(1)
                        dat(x, y, t) = dat3(x, y, t)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 1 .and. t == 3) then
            do t = 1, dim_lengths(3)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(2)
                        dat(x, y, t) = dat3(y, x, t)
                    end do
                end do
            end do
        else if (x == 1 .and. y == 3 .and. t == 2) then
            do t = 1, dim_lengths(2)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(1)
                        dat(x, y, t) = dat3(x, t, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 1 .and. t == 2) then
            do t = 1, dim_lengths(2)
                do y = 1, dim_lengths(1)
                    do x = 1, dim_lengths(3)
                        dat(x, y, t) = dat3(y, t, x)
                    end do
                end do
            end do
        else if (x == 2 .and. y == 3 .and. t == 1) then
            do t = 1, dim_lengths(1)
                do y = 1, dim_lengths(3)
                    do x = 1, dim_lengths(2)
                        dat(x, y, t) = dat3(t, x, y)
                    end do
                end do
            end do
        else if (x == 3 .and. y == 2 .and. t == 1) then
            do t = 1, dim_lengths(1)
                do y = 1, dim_lengths(2)
                    do x = 1, dim_lengths(3)
                        dat(x, y, t) = dat3(t, y, x)
                    end do
                end do
            end do
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_open_output(fname, iun, ierr)

        !> Input variables.
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: iun, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        character(len = 10) str10
        character(len = 8) str8
        character(len = 20) line

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Open the file with write access.
        ierr = nf90_create(fname, NF90_NETCDF4, iun)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning('Unable to open file (Code ' // trim(adjustl(code)) // '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Assign global meta attributes.
        ierr = NF90_NOERR
        call date_and_time(str8, str10)
        write(line, "(a4, '-', a2, '-', a2, 1x, a2, ':', a2, ':', a2)") &
            str8(1:4), str8(5:6), str8(7:8), str10(1:2), str10(3:4), '00'
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'title', 'SA_MESH model outputs')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'source', 'SA_MESH') !((version info, but somehow also configuration CLASS/SVS/etc..))
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'history', trim(adjustl(line)) // ' - Created.')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'references', 'SA_MESH') !(('https://wiki.usask.ca/display/MESH/'))

        !> Add coding convention.
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, NF90_GLOBAL, 'Conventions', 'CF-1.6')
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning('Errors occurred writing attributes to file (Code ' // trim(adjustl(code)) // '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_proj(iun, fname, proj_name, datum, zone_id, ierr)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, proj_name, datum
        character(len = *), intent(in), optional :: zone_id

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        integer vid

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, 'crs', NF90_INT, vid)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                'An error occurred creating the projection in file (Code ' // trim(adjustl(code)) // '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Assign projection.
        ierr = NF90_NOERR
        select case (lowercase(proj_name))

            !> Regular lat/lon.
            case ('latlon', 'latlong')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'grid_mapping_name', 'latitude_longitude')
                if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'longitude_of_prime_meridian', 0.0)

                !> Ellipsoid/datum specification (from EnSim/GK manual; version: September, 2010).
                select case (lowercase(datum))
                    case ('wgs84')
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6378137.0)
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 298.257223563)
                    case ('wgs72')
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6378135.0)
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 298.26)
                    case ('grs80', 'nad83')
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6378137.0)
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 298.257222101)
                    case ('clarke1866', 'nad27')
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6378206.4)
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 294.9786982)
                    case ('sphere')
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'semi_major_axis', 6371000.0)
                        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'inverse_flattening', 0.0)
                end select
        end select
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                'Errors occurred assigning a projection to file (Code ' // trim(adjustl(code)) // '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_dimension(iun, fname, dim_name, did, ierr, dim_length)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, dim_name

        !> Input variables (optional).
        integer, intent(in), optional :: dim_length

        !> Output variables.
        integer, intent(out) :: did, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create dimension.
        if (present(dim_length)) then
            ierr = nf90_def_dim(iun, dim_name, dim_length, did)
        else
            ierr = nf90_def_dim(iun, dim_name, NF90_UNLIMITED, did)
        end if
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred adding the '" // trim(dim_name) // "' dimension in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_set_time_units(iun, fname, vid_t, ts_freq, ierr)

        !> Input variables.
        integer, intent(in) :: iun, ts_freq, vid_t
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code
        character(len = DEFAULT_LINE_LENGTH) line

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Set units based on 'ts_freq'.
        write(line, "(i4.4, '-', i2.2, '-', i2.2, 1x, i2.2, ':', i2.2, ':', i2.2)") &
            ic%now%year, ic%now%month, ic%now%day, ic%now%hour, ic%now%mins, 0
        if (btest(ts_freq, IO_FREQ_MLY) .or. btest(ts_freq, IO_FREQ_SSL) .or. btest(ts_freq, IO_FREQ_DLY)) then
            ierr = nf90_put_att(iun, vid_t, 'units', 'days since ' // trim(adjustl(line)))
        else if (btest(ts_freq, IO_FREQ_YLY)) then
            ierr = nf90_put_att(iun, vid_t, 'units', 'years since ' // trim(adjustl(line)))
        else if (btest(ts_freq, IO_FREQ_HLY)) then
            ierr = nf90_put_att(iun, vid_t, 'units', 'hours since ' // trim(adjustl(line)))
        else if (btest(ts_freq, IO_FREQ_PTS)) then
            ierr = nf90_put_att(iun, vid_t, 'units', 'minutes since ' // trim(adjustl(line)))
        else
            ierr = nf90_put_att(iun, vid_t, 'units', 'seconds since ' // trim(adjustl(line)))
        end if
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_t, 'long_name', 'time')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_t, 'standard_name', 'time')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_t, 'axis', 'T')
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_t, 'calendar', 'gregorian')
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                'Errors occurred defining the time attribute in file (Code ' // trim(adjustl(code)) // '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_time_nf90_int(iun, fname, ts_freq, start_year, did_t, vid_t, ierr)

        !> Input variables.
        integer, intent(in) :: iun, ts_freq
        integer(kind = FourByteInt), intent(in) :: start_year
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: did_t, vid_t, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create dimension for 'time'.
        call nc4_define_dimension(iun, fname, 'time', did_t, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, 'time', NF90_INT, (/did_t/), vid_t)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred creating the time attribute in file (Code ' // trim(adjustl(code)) // '): " // trim(fname))
            ierr = 1
            return
        end if

        !> Set units based on 'ts_freq'.
        call nc4_set_time_units(iun, fname, vid_t, ts_freq, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_time_nf90_float(iun, fname, ts_freq, start_year, did_t, vid_t, ierr)

        !> Input variables.
        integer, intent(in) :: iun, ts_freq
        real(kind = FourByteReal), intent(in) :: start_year
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: did_t, vid_t, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create dimension for 'time'.
        call nc4_define_dimension(iun, fname, 'time', did_t, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, 'time', NF90_FLOAT, (/did_t/), vid_t)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred creating the time attribute in file (Code ' // trim(adjustl(code)) // '): " // trim(fname))
            ierr = 1
            return
        end if

        !> Set units based on 'ts_freq'.
        call nc4_set_time_units(iun, fname, vid_t, ts_freq, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_time_nf90_double(iun, fname, ts_freq, start_year, did_t, vid_t, ierr)

        !> Input variables.
        integer, intent(in) :: iun, ts_freq
        real(kind = EightByteReal), intent(in) :: start_year
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: did_t, vid_t, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create dimension for 'time'.
        call nc4_define_dimension(iun, fname, 'time', did_t, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, 'time', NF90_DOUBLE, (/did_t/), vid_t)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "An error occurred creating the time attribute in file (Code ' // trim(adjustl(code)) // '): " // trim(fname))
            ierr = 1
            return
        end if

        !> Set units based on 'ts_freq'.
        call nc4_set_time_units(iun, fname, vid_t, ts_freq, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

!    subroutine nc4_add_lonlat_float(iun, fname, xlon, ylat, did_x, did_y, ierr)

        !> Input variables.
!        integer, intent(in) :: iun
!        character(len = *), intent(in) :: fname
!        real(kind = FourByteReal), dimension(:), intent(in) :: xlon, ylat

        !> Output variables.
!        integer, intent(out) :: did_x, did_y, ierr

        !> Local variables.
!        character(len = DEFAULT_FIELD_LENGTH) code
!        integer vid_x, vid_y

        !> Initialize output variable.
!        ierr = NF90_NOERR

        !> Create necessary dimensions for fields.
!        ierr = nf90_def_dim(iun, 'lon', size(xlon), did_x)
!        ierr = nf90_def_dim(iun, 'lat', size(ylat), did_y)
!        if (ierr /= NF90_NOERR) then
!            write(code, FMT_GEN) ierr
!            call print_warning( &
!                "An error occurred creating location dimensions in file (Code ' // trim(adjustl(code)) // '): " // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Reference variables.
!        if (ierr == NF90_NOERR) ierr = nf90_def_var(iun, 'lon', NF90_FLOAT, (/did_x/), vid_x)
!        if (ierr == NF90_NOERR) ierr = nf90_def_var(iun, 'lat', NF90_FLOAT, (/did_y/), vid_y)
!        if (ierr /= NF90_NOERR) then
!            write(code, FMT_GEN) ierr
!            call print_warning( &
!                "An error occurred creating location variables in file (Code ' // trim(adjustl(code)) // '): " // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Coordinates.
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_x, 'standard_name', 'longitude')
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_x, 'long_name', 'longitude')
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_x, 'units', 'degrees_east')
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_y, 'standard_name', 'latitude')
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_y, 'long_name', 'latitude')
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid_y, 'units', 'degrees_north')
!        if (ierr /= NF90_NOERR) then
!            write(code, FMT_GEN) ierr
!            call print_warning( &
!                "An error occurred creating location attributes in file (Code ' // trim(adjustl(code)) // '): " // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Populate reference variables.
!        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid_x, xlon)
!        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid_y, ylat)
!        if (ierr /= NF90_NOERR) then
!            write(code, FMT_GEN) ierr
!            call print_warning( &
!                'Errors occurred saving location information to file (Code ' // trim(adjustl(code)) // '): ' // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Reset 'ierr.'
!        if (ierr == NF90_NOERR) ierr = 0

!    end subroutine

    subroutine nc4_add_attribute_nf90_int(iun, fname, attribute_name, attribute_value, ierr, vid)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attribute_name
        integer(kind = FourByteInt), intent(in) :: attribute_value
        integer, intent(in), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer v

        !> Initialize output variables.
        ierr = NF90_NOERR

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_put_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred writing the attribute '" // trim(attribute_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_attribute_nf90_float(iun, fname, attribute_name, attribute_value, ierr, vid)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attribute_name
        real(kind = FourByteReal), intent(in) :: attribute_value
        integer, intent(in), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer v

        !> Initialize output variables.
        ierr = NF90_NOERR

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_put_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred writing the attribute '" // trim(attribute_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_attribute_nf90_double(iun, fname, attribute_name, attribute_value, ierr, vid)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attribute_name
        real(kind = EightByteReal), intent(in) :: attribute_value
        integer, intent(in), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer v

        !> Initialize output variables.
        ierr = NF90_NOERR

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_put_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred writing the attribute '" // trim(attribute_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_attribute_nf90_char(iun, fname, attribute_name, attribute_value, ierr, vid)

        !* 'strings': lower
        use strings, only: lowercase

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, attribute_name
        character(len = *), intent(in) :: attribute_value
        integer, intent(in), optional :: vid

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) field, code
        integer v

        !> Initialize output variables.
        ierr = NF90_NOERR

        !> Assign the variable ID.
        if (present(vid)) then
            v = vid
        else
            v = NF90_GLOBAL
        end if

        !> Get the attribute value.
        ierr = nf90_put_att(iun, v, attribute_name, attribute_value)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_error( &
                "An error occurred writing the attribute '" // trim(attribute_name) // "' in file (Code: " &
                // trim(adjustl(code)) // "): " // trim(fname))
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_set_variable_standard_name(iun, fname, vid, standard_name, long_name, units, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname
        character(len = *), intent(in) :: standard_name, long_name, units

        !> Output variables.
        integer, intent(out) :: ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Assign the attributes.
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'standard_name', standard_name)
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'long_name', long_name)
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'units', units)
        if (ierr /= NF90_NOERR) then
            call print_warning('Errors occurred assigning standard name for ' // trim(standard_name) // ' in file: ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_set_crs(iun, fname, vid, standard_name, crs_coordinates, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname, standard_name, crs_coordinates

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Set CRS (reference).
        ierr = nf90_put_att(iun, vid, 'coordinates', trim(crs_coordinates))
        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'grid_mapping', 'crs')
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                'An error occurred assigning the CRS for ' // trim(standard_name) // ' in file (Code ' // trim(adjustl(code)) // &
                '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_set_variable_attributes_nf90_int( &
        iun, fname, vid, standard_name, long_name, units, fill, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Assign attributes.
        ierr = nf90_put_att(iun, vid, '_FillValue', fill)
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_standard_name(iun, fname, vid, standard_name, long_name, units, ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if
!        if (ierr == NF90_NOERR) then
!            call nc4_set_crs(iun, fname, vid, standard_name, 'lon lat', ierr)
!            if (ierr == 0) ierr = NF90_NOERR
!        end if
        if (present(constmul)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'scale_factor', constmul)
        end if
        if (present(constadd)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'add_offset', constadd)
        end if
        if (present(constrmin)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_min', constrmin)
        end if
        if (present(constrmax)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_max', constrmax)
        end if
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                'Errors occurred setting attributes for ' // trim(standard_name) // ' in file (Code ' // trim(adjustl(code)) // &
                '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_set_variable_attributes_nf90_float( &
        iun, fname, vid, standard_name, long_name, units, fill, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Assign attributes.
        ierr = nf90_put_att(iun, vid, '_FillValue', fill)
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_standard_name(iun, fname, vid, standard_name, long_name, units, ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if
!        if (ierr == NF90_NOERR) then
!            call nc4_set_crs(iun, fname, vid, standard_name, 'lon lat', ierr)
!            if (ierr == 0) ierr = NF90_NOERR
!        end if
        if (present(constmul)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'scale_factor', constmul)
        end if
        if (present(constadd)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'add_offset', constadd)
        end if
        if (present(constrmin)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_min', constrmin)
        end if
        if (present(constrmax)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_max', constrmax)
        end if
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                'Errors occurred setting attributes for ' // trim(standard_name) // ' in file (Code ' // trim(adjustl(code)) // &
                '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_set_variable_attributes_nf90_double( &
        iun, fname, vid, standard_name, long_name, units, fill, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Assign attributes.
        ierr = nf90_put_att(iun, vid, '_FillValue', fill)
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_standard_name(iun, fname, vid, standard_name, long_name, units, ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if
!        if (ierr == NF90_NOERR) then
!            call nc4_set_crs(iun, fname, vid, standard_name, 'lon lat', ierr)
!            if (ierr == 0) ierr = NF90_NOERR
!        end if
        if (present(constmul)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'scale_factor', constmul)
        end if
        if (present(constadd)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'add_offset', constadd)
        end if
        if (present(constrmin)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_min', constrmin)
        end if
        if (present(constrmax)) then
            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_max', constrmax)
        end if
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                'Errors occurred setting attributes for ' // trim(standard_name) // ' in file (Code ' // trim(adjustl(code)) // &
                '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_set_variable_attributes_nf90_char( &
        iun, fname, vid, standard_name, long_name, units, fill, ierr)

        !> Input variables.
        integer, intent(in) :: iun, vid
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        character(len = *), intent(in) :: fill

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Assign attributes.
        ierr = nf90_put_att(iun, vid, '_FillValue', fill)
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_standard_name(iun, fname, vid, standard_name, long_name, units, ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                'Errors occurred setting attributes for ' // trim(standard_name) // ' in file (Code ' // trim(adjustl(code)) // &
                '): ' // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_scalar_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_INT, (/0/), vid)

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_int( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_scalar_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_FLOAT, (/0/), vid)

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_float( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_scalar_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_DOUBLE, (/0/), vid)

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_double( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_scalar_nf90_char( &
        iun, fname, standard_name, long_name, units, fill, did_c, vid, ierr)

        !> Input variables.
        integer, intent(in) :: iun, did_c
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        character(len = *), intent(in) :: fill

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_CHAR, (/did_c/), vid)

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_char( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_scalar_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, dat, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: dat, fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_scalar_nf90_int(iun, fname, standard_name, long_name, units, fill, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_scalar_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, dat, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: dat, fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_scalar_nf90_float(iun, fname, standard_name, long_name, units, fill, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_scalar_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, dat, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: dat, fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_scalar_nf90_double(iun, fname, standard_name, long_name, units, fill, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_scalar_nf90_char( &
        iun, fname, standard_name, long_name, units, fill, dat, did_c, vid, ierr)

        !> Input variables.
        integer, intent(in) :: iun, did_c
        character(len = *), intent(in) :: fname, standard_name, long_name, units, dat
        character(len = 1), intent(in) :: fill

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_scalar_nf90_char(iun, fname, standard_name, long_name, units, fill, did_c, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_n_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, did, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_INT, (/did/), vid)

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_int( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_n_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, did, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_FLOAT, (/did/), vid)

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_float( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_n_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, did, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_DOUBLE, (/did/), vid)

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_double( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_n_nf90_char( &
        iun, fname, standard_name, long_name, units, fill, did_c, did_n, vid, ierr)

        !> Input variables.
        integer, intent(in) :: iun, did_c, did_n
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        character(len = *), intent(in) :: fill

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_CHAR, (/did_c, did_n/), vid)

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_char( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_n_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, dat, did, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: dat(:), fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_n_nf90_int(iun, fname, standard_name, long_name, units, fill, did, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_n_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, dat, did, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: dat(:), fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_n_nf90_float(iun, fname, standard_name, long_name, units, fill, did, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_n_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, dat, did, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: dat(:), fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_n_nf90_double(iun, fname, standard_name, long_name, units, fill, did, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_n_nf90_char( &
        iun, fname, standard_name, long_name, units, fill, dat, did_c, did_n, vid, ierr)

        !> Input variables.
        integer, intent(in) :: iun, did_c, did_n
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        character(len = *), intent(in) :: dat(:)
        character(len = 1), intent(in) :: fill

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_n_nf90_char(iun, fname, standard_name, long_name, units, fill, did_c, did_n, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xy_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_INT, (/did_x, did_y/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_int( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xy_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_FLOAT, (/did_x, did_y/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_float( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xy_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_DOUBLE, (/did_x, did_y/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_double( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xy_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: dat(:, :), fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xy_nf90_int(iun, fname, standard_name, long_name, units, fill, did_x, did_y, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xy_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: dat(:, :), fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xy_nf90_float(iun, fname, standard_name, long_name, units, fill, did_x, did_y, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xy_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: dat(:, :), fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xy_nf90_double(iun, fname, standard_name, long_name, units, fill, did_x, did_y, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xym_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_INT, (/did_x, did_y, did_m/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_int( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xym_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_FLOAT, (/did_x, did_y, did_m/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_float( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xym_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_DOUBLE, (/did_x, did_y, did_m/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_double( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xym_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: dat(:, :, :), fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xym_nf90_int(iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_m, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xym_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: dat(:, :, :), fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xym_nf90_float(iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_m, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xym_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: dat(:, :, :), fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xym_nf90_double(iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_m, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xylm_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_l, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_l, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_INT, (/did_x, did_y, did_l, did_m/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_int( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xylm_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_l, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_l, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_FLOAT, (/did_x, did_y, did_l, did_m/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_float( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xylm_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_l, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_l, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_DOUBLE, (/did_x, did_y, did_l, did_m/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_double( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xylm_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, did_l, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_l, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: dat(:, :, :, :), fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xylm_nf90_int( &
            iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_l, did_m, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xylm_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, did_l, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_l, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: dat(:, :, :, :), fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xylm_nf90_float( &
            iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_l, did_m, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xylm_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, did_l, did_m, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_l, did_m
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: dat(:, :, :, :), fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xylm_nf90_double( &
            iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_l, did_m, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xyt_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_t, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_t
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_INT, (/did_x, did_y, did_t/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_int( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xyt_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_t, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_t
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_FLOAT, (/did_x, did_y, did_t/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_float( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_define_variable_xyt_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_t, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_t
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Create variable.
        ierr = nf90_def_var(iun, standard_name, NF90_DOUBLE, (/did_x, did_y, did_t/), vid)

        !> Coordinate reference (longitude latitude).
        if (ierr == NF90_NOERR) then
            call nc4_set_crs(iun, fname, vid, standard_name, 'longitude latitude', ierr)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Assign attributes.
        if (ierr == NF90_NOERR) then
            call nc4_set_variable_attributes_nf90_double( &
                iun, fname, vid, standard_name, long_name, units, fill, ierr, &
                constmul, constadd, constrmax, constrmin)
            if (ierr == 0) ierr = NF90_NOERR
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xyt_nf90_int( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, did_t, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_t
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        integer(kind = FourByteInt), intent(in) :: dat(:, :, :), fill

        !> Input variables (optional).
        integer(kind = FourByteInt), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xyt_nf90_int(iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_t, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xyt_nf90_float( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, did_t, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_t
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = FourByteReal), intent(in) :: dat(:, :, :), fill

        !> Input variables (optional).
        real(kind = FourByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xyt_nf90_float(iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_t, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_variable_xyt_nf90_double( &
        iun, fname, standard_name, long_name, units, fill, dat, did_x, did_y, did_t, vid, ierr, &
        constmul, constadd, constrmax, constrmin)

        !> Input variables.
        integer, intent(in) :: iun, did_x, did_y, did_t
        character(len = *), intent(in) :: fname, standard_name, long_name, units
        real(kind = EightByteReal), intent(in) :: dat(:, :, :), fill

        !> Input variables (optional).
        real(kind = EightByteReal), intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: vid, ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Define variable.
        call nc4_define_variable_xyt_nf90_double(iun, fname, standard_name, long_name, units, fill, did_x, did_y, did_t, vid, ierr)
        if (ierr == 0) ierr = NF90_NOERR

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, dat)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning( &
                "Errors occurred writing data for '" // trim(standard_name) // "' in file (Code " // trim(adjustl(code)) // &
                "): " // trim(fname))
            ierr = 1
            return
        end if

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_vname_xy_nf90_float( &
        shd, vname, long_name, units, fill, fname, iun, vid, &
        constmul, constadd, constrmax, constrmin, ierr)

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        real, intent(in) :: fill
        character(len = *), intent(in) :: vname, long_name, units, fname

        !> Input variables (optional).
        real, intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: iun, vid, ierr

        !> Local variables.
!        integer did_x, did_y, vid_x, vid_y, n, z
        integer did_x, did_y, n
        real lon(shd%xCount), lat(shd%yCount)

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Open the file (write access).
!        z = 0
        call nc4_open_output(fname, iun, ierr)
!        if (z /= 0) return

        !> Create necessary dimensions for fields.
!        ierr = nf90_def_dim(iun, 'lon', shd%xCount, did_x)
!        ierr = nf90_def_dim(iun, 'lat', shd%yCount, did_y)
!        if (ierr /= NF90_NOERR) then
!            call print_warning("An error occurred dimensioning locations in file: " // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Reference variables.
!        if (ierr == NF90_NOERR) ierr = nf90_def_var(iun, 'lon', NF90_FLOAT, (/did_x/), vid_x)
!        if (ierr == NF90_NOERR) ierr = nf90_def_var(iun, 'lat', NF90_FLOAT, (/did_y/), vid_y)
!        if (ierr /= NF90_NOERR) then
!            call print_warning("An error occurred creating location attributes in file: " // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Projection.
!        z = 0
        if (ierr == 0) call nc4_add_proj(iun, fname, shd%CoordSys%Proj, shd%CoordSys%Ellips, shd%CoordSys%Zone, ierr)
!        if (z /= 0) return

        !> Populate reference variables.
        do n = 1, shd%xCount
            lon(n) = (shd%xOrigin + shd%xDelta*n) - shd%xDelta/2.0
        end do
        do n = 1, shd%yCount
            lat(n) = (shd%yOrigin + shd%yDelta*n) - shd%yDelta/2.0
        end do

        !> Coordinates.
!        z = 0
!        if (z == 0) call nc4_set_attr(iun, fname, vid_x, 'longitude', 'longitude', 'degrees_east', z)
!        if (z == 0) call nc4_set_attr(iun, fname, vid_y, 'latitude', 'latitude', 'degrees_north', z)
!        if (ierr == 0) call nc4_add_lonlat(iun, fname, lon, lat, did_x, did_y, ierr)
        if (ierr == 0) call nc4_define_dimension(iun, fname, 'lon', did_x, ierr, dim_length = size(lon))
        if (ierr == 0) call nc4_add_variable_n_nf90_float( &
            iun, fname, 'longitude', 'longitude', 'degrees_east', fill, lon, did_x, n, ierr)
        if (ierr == 0) call nc4_define_dimension(iun, fname, 'lat', did_y, ierr, dim_length = size(lat))
        if (ierr == 0) call nc4_add_variable_n_nf90_float( &
            iun, fname, 'latitude', 'latitude', 'degrees_north', fill, lat, did_y, n, ierr)
!        if (z /= 0) return

        !> Data.
!        ierr = nf90_def_var(iun, vname, NF90_FLOAT, (/did_x, did_y/), vid)
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, '_FillValue', fill)
!        z = 0
!        call nc4_set_attr(iun, fname, vid, vname, long_name, units, z)
!        if (z /= 0) return
!        if (present(constmul)) then
!            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'scale_factor', constmul)
!        end if
!        if (present(constadd)) then
!            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'add_offset', constadd)
!        end if
!        if (present(constrmin)) then
!            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_min', constrmin)
!        end if
!        if (present(constrmax)) then
!            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_max', constrmax)
!        end if
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'coordinates', 'lon lat')
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'grid_mapping', 'crs')
!        if (ierr /= NF90_NOERR) then
!            call print_warning('Errors occurred assigning attributes for ' // trim(vname) // ' in file: ' // trim(fname))
!            ierr = 1
!            return
!        end if
!        z = 0
        if (ierr == 0) then
            call nc4_define_variable_xy_nf90_float( &
                iun, fname, vname, long_name, units, fill, did_x, did_y, vid, ierr, &
                constmul, constadd, constrmax, constrmin)
        end if
!        if (z /= 0) return

        !> Populate reference variables.
!        do n = 1, shd%xCount
!            lon(n) = (shd%xOrigin + shd%xDelta*n) - shd%xDelta/2.0
!        end do
!        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid_x, lon)
!        do n = 1, shd%yCount
!            lat(n) = (shd%yOrigin + shd%yDelta*n) - shd%yDelta/2.0
!        end do
!        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid_y, lat)
!        if (ierr /= NF90_NOERR) then
!            call print_warning('Errors occurred adding locations to file: ' // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Reset 'ierr.'
!        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_add_vname_xyt_nf90_float( &
        shd, vname, long_name, units, ffreq, start_year, fill, fname, iun, tid, vid, &
        constmul, constadd, constrmax, constrmin, ierr)

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: ffreq, start_year
        real, intent(in) :: fill
        character(len = *), intent(in) :: vname, long_name, units, fname

        !> Input variables (optional).
        real, intent(in), optional :: constmul, constadd, constrmax, constrmin

        !> Output variables.
        integer, intent(out) :: iun, tid, vid, ierr

        !> Local variables.
!        integer did_x, did_y, did_t, vid_x, vid_y, n, z
        integer did_x, did_y, did_t, n
        real lon(shd%xCount), lat(shd%yCount)

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Open the file (write access).
!        z = 0
        call nc4_open_output(fname, iun, ierr)
!        if (z /= 0) return

        !> Create necessary dimensions for fields.
!        ierr = nf90_def_dim(iun, 'lon', shd%xCount, did_x)
!        ierr = nf90_def_dim(iun, 'lat', shd%yCount, did_y)
!        if (ierr /= NF90_NOERR) then
!            call print_warning("An error occurred dimensioning locations in file: " // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Reference variables.
!        if (ierr == NF90_NOERR) ierr = nf90_def_var(iun, 'lon', NF90_FLOAT, (/did_x/), vid_x)
!        if (ierr == NF90_NOERR) ierr = nf90_def_var(iun, 'lat', NF90_FLOAT, (/did_y/), vid_y)
!        if (ierr /= NF90_NOERR) then
!            call print_warning("An error occurred creating location attributes in file: " // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Projection.
!        z = 0
        if (ierr == 0) call nc4_add_proj(iun, fname, shd%CoordSys%Proj, shd%CoordSys%Ellips, shd%CoordSys%Zone, ierr)
!        if (z /= 0) return

        !> Populate reference variables.
        do n = 1, shd%xCount
            lon(n) = (shd%xOrigin + shd%xDelta*n) - shd%xDelta/2.0
        end do
        do n = 1, shd%yCount
            lat(n) = (shd%yOrigin + shd%yDelta*n) - shd%yDelta/2.0
        end do

        !> Coordinates.
!        z = 0
!        if (z == 0) call nc4_set_attr(iun, fname, vid_x, 'longitude', 'longitude', 'degrees_east', z)
!        if (z == 0) call nc4_set_attr(iun, fname, vid_y, 'latitude', 'latitude', 'degrees_north', z)
!        if (ierr == 0) call nc4_add_lonlat(iun, fname, lon, lat, did_x, did_y, ierr)
        if (ierr == 0) call nc4_define_dimension(iun, fname, 'lon', did_x, ierr, dim_length = size(lon))
        if (ierr == 0) call nc4_add_variable_n_nf90_float( &
            iun, fname, 'longitude', 'longitude', 'degrees_east', fill, lon, did_x, n, ierr)
        if (ierr == 0) call nc4_define_dimension(iun, fname, 'lat', did_y, ierr, dim_length = size(lat))
        if (ierr == 0) call nc4_add_variable_n_nf90_float( &
            iun, fname, 'latitude', 'latitude', 'degrees_north', fill, lat, did_y, n, ierr)
!        if (z /= 0) return

        !> Time.
!        z = 0
        if (ierr == 0) call nc4_add_time(iun, fname, ffreq, start_year, did_t, tid, ierr)
!        if (z /= 0) return

        !> Data.
!        ierr = nf90_def_var(iun, vname, NF90_FLOAT, (/did_x, did_y, did_t/), vid)
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, '_FillValue', fill)
!        z = 0
!        call nc4_set_attr(iun, fname, vid, vname, long_name, units, z)
!        if (z /= 0) return
!        if (present(constmul)) then
!            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'scale_factor', constmul)
!        end if
!        if (present(constadd)) then
!            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'add_offset', constadd)
!        end if
!        if (present(constrmin)) then
!            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_min', constrmin)
!        end if
!        if (present(constrmax)) then
!            if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'valid_max', constrmax)
!        end if
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'coordinates', 'lon lat')
!        if (ierr == NF90_NOERR) ierr = nf90_put_att(iun, vid, 'grid_mapping', 'crs')
!        if (ierr /= NF90_NOERR) then
!            call print_warning('Errors occurred assigning attributes for ' // trim(vname) // ' in file: ' // trim(fname))
!            ierr = 1
!            return
!        end if
        if (ierr == 0) then
            call nc4_define_variable_xyt_nf90_float( &
                iun, fname, vname, long_name, units, fill, did_x, did_y, did_t, vid, ierr, &
                constmul, constadd, constrmax, constrmin)
        end if
!        if (z /= 0) return

        !> Populate reference variables.
!        do n = 1, shd%xCount
!            lon(n) = (shd%xOrigin + shd%xDelta*n) - shd%xDelta/2.0
!        end do
!        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid_x, lon)
!        do n = 1, shd%yCount
!            lat(n) = (shd%yOrigin + shd%yDelta*n) - shd%yDelta/2.0
!        end do
!        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid_y, lat)
!        if (ierr /= NF90_NOERR) then
!            call print_warning('Errors occurred adding locations to file: ' // trim(fname))
!            ierr = 1
!            return
!        end if

        !> Reset 'ierr.'
!        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_write_field_xy_nf90_float(shd, iun, vid, dat, ierr)

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iun, vid
        real(kind = FourByteReal), dimension(:), intent(in) :: dat

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, n, z
        real, dimension(shd%xCount, shd%yCount) :: r2c_grid
        real fill

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the 'fill' value.
        z = nf90_inquire_attribute(iun, vid, '_FillValue')
        if (z == NF90_NOERR) ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (z /= NF90_NOERR .or. ierr /= NF90_NOERR) then
            fill = NF90_FILL_FLOAT
        end if

        !> Transfer data to temporary variable.
        r2c_grid = fill
        do n = 1, shd%NA
            r2c_grid(shd%xxx(n), shd%yyy(n)) = dat(n)
        end do

        !> Write data.
        if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, r2c_grid)

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_write_field_xyt_nf90_float(shd, iun, ffreq, tid, vid, dat, dates, ierr)

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iun, ffreq, tid, vid
        real(kind = FourByteReal), dimension(:, :), intent(in) :: dat
        integer, dimension(:, :), intent(in) :: dates

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer t, n, z
        real, dimension(shd%xCount, shd%yCount) :: r2c_grid
        real(kind = EightByteReal) jdate_r8, t0_r8, t1_r8
        real fill

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Get the 'fill' value.
        z = nf90_inquire_attribute(iun, vid, '_FillValue')
        if (z == NF90_NOERR) ierr = nf90_get_att(iun, vid, '_FillValue', fill)
        if (z /= NF90_NOERR .or. ierr /= NF90_NOERR) then
            fill = NF90_FILL_FLOAT
        end if

        !> Calculate reference time.
        jdate_r8 = ((ic%start%year - 1) - 1601)*365.25 + get_jday(ic%start%month, ic%start%day, ic%start%year)
        if (btest(ffreq, IO_FREQ_MLY) .or. btest(ffreq, IO_FREQ_SSL) .or. btest(ffreq, IO_FREQ_DLY)) then
            t0_r8 = jdate_r8
        else if (btest(ffreq, IO_FREQ_YLY)) then
            t0_r8 = ic%start%year
        else if (btest(ffreq, IO_FREQ_HLY)) then
            t0_r8 = jdate_r8*24.0 + ic%start%hour + ic%start%mins/60.0
        else if (btest(ffreq, IO_FREQ_PTS)) then
            t0_r8 = jdate_r8*24.0*60.0 + ic%start%hour*60.0 + ic%start%mins
        else
            t0_r8 = jdate_r8*24.0*60.0*60.0 + ic%start%hour*60.0*60.0 + ic%start%mins*60.0
        end if

        !> Loop for time.
        do t = 1, size(dat, 2)

            !> Transfer data to temporary variable.
            r2c_grid = fill
            do n = 1, shd%NA
                r2c_grid(shd%xxx(n), shd%yyy(n)) = dat(n, t)
            end do

            !> Calculate offset from reference time.
            !>  dates(2, t) -> year
            !>  dates(3, t) -> month
            !>  dates(4, t) -> day
            !>  dates(5, t) -> hour
            !>  dates(6, t) -> mins
            jdate_r8 = ((dates(2, t) - 1) - 1601)*365.25 + get_jday(dates(3, t), dates(4, t), dates(2, t))
            if (btest(ffreq, IO_FREQ_MLY) .or. btest(ffreq, IO_FREQ_SSL) .or. btest(ffreq, IO_FREQ_DLY)) then
                t1_r8 = jdate_r8
            else if (btest(ffreq, IO_FREQ_YLY)) then
                t1_r8 = dates(2, t)
            else if (btest(ffreq, IO_FREQ_HLY)) then
                t1_r8 = jdate_r8*24.0 + dates(5, t) + dates(6, t)/60.0
            else if (btest(ffreq, IO_FREQ_PTS)) then
                t1_r8 = jdate_r8*24.0*60.0 + dates(5, t)*60.0 + dates(6, t)
            else
                t1_r8 = jdate_r8*24.0*60.0*60.0 + dates(5, t)*60.0*60.0 + dates(6, t)*60.0
            end if

            !> Write time.
            if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, tid, int(t1_r8 - t0_r8), start = (/ dates(1, t) /))

            !> Write data.
            if (ierr == NF90_NOERR) ierr = nf90_put_var(iun, vid, r2c_grid, start = (/ 1, 1, dates(1, t) /))
        end do

        !> Reset 'ierr.'
        if (ierr == NF90_NOERR) ierr = 0

    end subroutine

    subroutine nc4_close_file(iun, fname, ierr)

        !> Input variables.
        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        character(len = DEFAULT_FIELD_LENGTH) code

        !> Initialize output variable.
        ierr = NF90_NOERR

        !> Open the file with read access.
        ierr = nf90_close(iun)
        if (ierr /= NF90_NOERR) then
            write(code, FMT_GEN) ierr
            call print_warning('An error occurred closing the file (Code ' // trim(adjustl(code)) // '): ' // trim(fname))
            ierr = 1
            return
        end if

    end subroutine

end module
