!> Description:
!>  Module to read climate forcing data from file.
module climate_forcing_io

    use climate_forcing_constants
    use climate_forcing_variabletypes
    use print_routines

    implicit none

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
    function open_data(shd, cm, vid) result(ENDDATA)

        !> 'shd_variables': For 'shd' variable.
        !> 'parse_utilities': For 'parse_datetime' and 'precision' (via 'strings').
        !> 'netcdf': For netCDF library.
        use shd_variables
        use parse_utilities
!        use mo_julian, only: &
!             date2dec, & ! converts date into fractional julian date
!             dec2date    ! converts fractional julian date into date
#ifdef NETCDF
!        use mo_ncread, only: &
!             NcOpen,         & ! Open NetCDF
!             Get_NcDimAtt,   & ! Get dimension name and length
!             Get_NcVarAtt,   & ! Get attributes of a variable
!             Get_NcVarType,  & ! get type of a variable
!             Get_NcVar,      & ! Get variable
!             check             ! check if NetCDF call was feasible
!        use mo_netcdf
!        use netcdf,  only:   &
!           nf90_inq_varid      ! request variable ID in NetCDF file
        use netcdf
#endif

        !> Input variables.
        type(ShedGridParams) shd
        integer, intent(in) :: vid

        !> Input/Output variables.
        type(clim_info) cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer z, ierr
        character(len = DEFAULT_LINE_LENGTH) line

#ifdef NETCDF
        !> Local variables for NetCDF support
!        integer                                   :: ncid        ! netcdf file unit
!        character(200), dimension(:), allocatable :: nc_dimname  ! names of dimensions of a variable
        integer,        dimension(:), allocatable :: nc_dimlen   ! length of dimension of a variable
!        logical                                   :: dim_found   ! true if dimension is listed in options.ini
!        character(200)                            :: time_unit   ! units attribute of time,
        character(len = DEFAULT_LINE_LENGTH) time_attribute, time_units, time_calendar
        !                                                        ! e.g. 'seconds since 1970-01-01 00:00:00.0'
        integer                                   :: dtype       ! type of variable 'time'
!        character(10)                             :: tunit, tdummy, tdate, thour  ! splits of time_unit
!        integer                                   :: yy, mm, dd  ! year, minute, day of time_unit
!        integer                                   :: hh, mi, ss  ! hour, minute, second of time_unit
!        real(8)                                   :: jdate       ! fractional julian date
!        real(8)                                   :: jdate_jan1  ! fractional julian date of Jan 1st
        integer                                   :: ii          ! counter
        integer t0_year, t0_month, t0_day, t0_hour, t0_mins, t0_seconds
        integer(kind = ki4) tt_i4(2)
        real(kind = kr4) tt_r4(2)
        real(kind = kr8) tt_r8(2), t0_r8, t1_r8, t2_r8, dt_r8
!        integer, dimension(1)                     :: start, a_count
!        integer                                   :: tstep       ! time step in netcdf file
        integer                                   :: jday        ! julian day of first timestep in NetCDF
!        type(NcDimension) dim_x, dim_y, dim_t
!        type(NcDimension), allocatable :: dims(:)
#endif

        ENDDATA = .false.

        !> Return if the variable is not marked active.
        if (.not. cm%dat(vid)%factive) return

        !> Open file depending on the format type of the climate data.
        select case (cm%dat(vid)%ffmt)

            !> ASCII R2C format.
            case (1)

                !> Open the file.
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.r2c'
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)

                !> Return on an error.
                if (ierr /= 0) goto 999

                !> Skip the header of the 'r2c' format file.
                line = ''
                do while (line /= ':endHeader')
                    read(cm%dat(vid)%fiun, '(a10)', end = 998) line
                end do

                !> Set the block type.
                cm%dat(vid)%blocktype = cbk%GRD

            !> CSV format.
            case (2)
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.csv'
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRU

            !> Binary sequential format.
            case (3)
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.seq'
                open( &
                    cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', &
                    form = 'unformatted', access = 'sequential', iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRD

            !> ASCII format.
            case (4)
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.asc'
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRD

            !> CLASS format MET file.
            case (6)
                if (vid /= ck%MET) return
                cm%dat(vid)%fname = 'basin_forcing'
                cm%dat(vid)%fpath = 'basin_forcing.met'
                cm%dat(vid)%blocktype = cbk%GRD
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)
                if (ierr /= 0) goto 999

            !> netCDF format.
            case(7)
#ifdef NETCDF
!-                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname))   ! no file ending (*.nc) needs to be appended
                if (len_trim(cm%dat(vid)%fpath) == 0) then
                    cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.nc'
                end if
                !> open and close file to see if it exists
!                ncid = NcOpen(cm%dat(vid)%fpath)
                !> overwrite pre-assigned unit number of file.
!                cm%dat(vid)%fiun = ncid
                ! call NcClose(ncid)

                !> get dimensions (name and length)
!                if ( allocated(nc_dimname) ) deallocate(nc_dimname)
!                if ( allocated(nc_dimlen) )  deallocate(nc_dimlen)
!                call Get_NcDimAtt(cm%dat(vid)%fpath, cm%dat(vid)%name_var, nc_dimname, dimlen=nc_dimlen, fid=cm%dat(vid)%fiun)

                !> check that there are only three dimensions (lon, lat, time) present
!                if (size(nc_dimname,1) /= 3) then
!                   call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // '): '// &
!                        ' Does not have three dimensions.'//&
!                        ' Dimensions must be latitude, longitude and time.')
!                   call program_abort()
!                end if
!                cm%dat(vid)%nc%f = NcDataset(cm%dat(vid)%fpath, 'r')
                ierr = nf90_open(cm%dat(vid)%fpath, NF90_NOWRITE, cm%dat(vid)%fiun)
                if (ierr /= NF90_NOERR) then
                    call print_error('Unable to open file: ' // trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
!                cm%dat(vid)%nc%v = cm%dat(vid)%nc%f%getVariable(cm%dat(vid)%name_var)
                ierr = nf90_inq_varid(cm%dat(vid)%fiun, cm%dat(vid)%name_var, cm%dat(vid)%vid)
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "The variable '" // trim(cm%dat(vid)%name_var) // "' cound not be found in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if

!                do ii=1,size(nc_dimname,1)

!                   dim_found = .false.

!                   if ( trim(nc_dimname(ii)) == trim(cm%dat(vid)%name_lon) ) then
                      !> set position of longitude dimension
!                      dim_found = cm%dat(vid)%nc%f%hasDimension(cm%dat(vid)%name_lon)
!                      dim_x = cm%dat(vid)%nc%f%getDimension(cm%dat(vid)%name_lon)
!                      cm%dat(vid)%ncol_lon = dim_x%id
                ierr = nf90_inq_dimid(cm%dat(vid)%fiun, cm%dat(vid)%name_lon, cm%dat(vid)%ncol_lon)
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "A required attribute '" // trim(cm%dat(vid)%name_lon) // "' cound not be found in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
                      !
                      !> check if longitude dimension is same as xCount
!                      if ( dim_x%getLength() /= shd%xCount ) then
!                         call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // &
!                              '): Longitude dimension ('//trim(cm%dat(vid)%name_lon)//') does not match '// &
!                              'shed files xCount .')
!                         call program_abort()
!                      end if
                ierr = nf90_inquire_dimension(cm%dat(vid)%fiun, cm%dat(vid)%ncol_lon, len = z)
                if (ierr /= NF90_NOERR .or. z /= shd%xCount) then
                    call print_error( &
                        "The model configuration contains a different number of '" // trim(cm%dat(vid)%name_lon) // &
                        "' elements than in file: " // trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
!                   end if

!                   if ( trim(nc_dimname(ii)) == trim(cm%dat(vid)%name_lat) ) then
                      !> set position of longitude dimension
!                      dim_found = cm%dat(vid)%nc%f%hasDimension(cm%dat(vid)%name_lat)
!                      dim_y = cm%dat(vid)%nc%f%getDimension(cm%dat(vid)%name_lat)
!                      cm%dat(vid)%ncol_lat = dim_y%id
                ierr = nf90_inq_dimid(cm%dat(vid)%fiun, cm%dat(vid)%name_lat, cm%dat(vid)%ncol_lat)
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "A required attribute '" // trim(cm%dat(vid)%name_lat) // "' cound not be found in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
                      !
                      !> check if latitude dimension is same as yCount
!                      if ( dim_y%getLength() /= shd%yCount ) then
!                         call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // &
!                              '): Latitude dimension ('//trim(cm%dat(vid)%name_lat)//') does not match '// &
!                              'shed files yCount .')
!                         call program_abort()
!                      end if
                ierr = nf90_inquire_dimension(cm%dat(vid)%fiun, cm%dat(vid)%ncol_lat, len = z)
                if (ierr /= NF90_NOERR .or. z /= shd%yCount) then
                    call print_error( &
                        "The model configuration contains a different number of '" // trim(cm%dat(vid)%name_lat) // &
                        "' elements than in file: " // trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
!                   end if

!                   if ( trim(nc_dimname(ii)) == trim(cm%dat(vid)%name_time) ) then
                      !> set position of time dimension
!                      dim_found = cm%dat(vid)%nc%f%hasDimension(cm%dat(vid)%name_time)
!                      dim_t = cm%dat(vid)%nc%f%getDimension(cm%dat(vid)%name_time)
!                      cm%dat(vid)%ncol_time = dim_t%id
                ierr = nf90_inq_dimid(cm%dat(vid)%fiun, cm%dat(vid)%name_time, cm%dat(vid)%ncol_time)
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "A required attribute '" // trim(cm%dat(vid)%name_time) // "' cound not be found in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if

                      ! check if consistent with settings in
                      ! - "MESH_parameters_CLASS.ini" (start date) and
                      ! - "MESH_input_run_options.ini" (time step)

                      ! time unit, e.g. 'seconds since 1970-01-01 00:00:00.0'
                      ! dtype      1 = NF90_BYTE
                      !            2 = NF90_CHAR
                      !            3 = NF90_SHORT
                      !            4 = NF90_INT      integer
                      !            5 = NF90_FLOAT    real(4)
                      !            6 = NF90_DOUBLE   real(8)
!                      call Get_NcVarAtt(cm%dat(vid)%fpath, 'time', 'units', time_unit, fid=cm%dat(vid)%fiun)
!                      cm%dat(vid)%nc%ts = cm%dat(vid)%nc%f%getVariable(cm%dat(vid)%name_time)
!                      if (cm%dat(vid)%nc%ts%hasAttribute('units')) then
!                         call cm%dat(vid)%nc%ts%getAttribute('units', time_unit)
!                      else
!                         call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // &
!                              '): Missing units for '//trim(cm%dat(vid)%name_time)//'.')
!                         call program_abort()
!                      end if
                ierr = nf90_inq_varid(cm%dat(vid)%fiun, cm%dat(vid)%name_time, cm%dat(vid)%tid)
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "The variable '" // trim(cm%dat(vid)%name_time) // "' cound not be found in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
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
!                      call Get_NcVarType(cm%dat(vid)%fpath, 'time', dtype, fid=cm%dat(vid)%fiun)

                      ! grep information from time unit string
!                      read(time_unit, *) tunit, tdummy, tdate, thour
!                      read(tdate(1:4), *) yy
!                      read(tdate(6:7), *) mm
!                      read(tdate(9:10), *) dd
!                      read(thour(1:2), *) hh
!                      read(thour(4:5), *) mi
!                      read(thour(7:8), *) ss

                      ! convert date in time unit string to julian date
!                      jdate = date2dec(dd = dd, mm = mm, yy = yy, hh = hh, nn = mi, ss = ss)

                      ! apply time shift
!                      jdate = jdate + cm%dat(vid)%time_shift/24.0

                      ! read first two time values (for initial day and timestep)
!                      start(1)   = 1
!                      a_count(1) = 2
                ierr = nf90_inquire_variable(cm%dat(vid)%fiun, cm%dat(vid)%tid, xtype = dtype)
                if (ierr == NF90_NOERR) then
                    select case (dtype)
!                      case(1:3)
!                         call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // &
!                              '): Datatype of variable is not supported (NF90_BYTE, NF90_CHAR, or NF90_SHORT).')
!                         call program_abort()
                        case (NF90_INT)
!                         call Get_NcVar(cm%dat(vid)%fpath, 'time', tt_i4, start=start, a_count=a_count, fid=cm%dat(vid)%fiun)  ! read only first value
!                         call cm%dat(vid)%nc%ts%getData(tt_i4, start, a_count)
                            ierr = nf90_get_var(cm%dat(vid)%fiun, cm%dat(vid)%tid, tt_i4, start = (/1/), count = (/2/))
                            if (ierr == NF90_NOERR) then
                                t1_r8 = real(tt_i4(1), kind = 8)
                                t2_r8 = real(tt_i4(2), kind = 8)
                            end if
                        case (NF90_FLOAT)
!                         call Get_NcVar(cm%dat(vid)%fpath, 'time', tt_sp, start=start, a_count=a_count, fid=cm%dat(vid)%fiun)  ! read only first value
!                         call cm%dat(vid)%nc%ts%getData(tt_sp, start, a_count)
                            ierr = nf90_get_var(cm%dat(vid)%fiun, cm%dat(vid)%tid, tt_r4, start = (/1/), count = (/2/))
                            if (ierr == NF90_NOERR) then
                                t1_r8 = real(tt_r4(1), kind = 8)
                                t2_r8 = real(tt_r4(2), kind = 8)
                            end if
                        case (NF90_DOUBLE)
!                         call Get_NcVar(cm%dat(vid)%fpath, 'time', tt_dp, start=start, a_count=a_count, fid=cm%dat(vid)%fiun)  ! read only first value
!                         call cm%dat(vid)%nc%ts%getData(tt_dp, start, a_count)
                            ierr = nf90_get_var(cm%dat(vid)%fiun, cm%dat(vid)%tid, tt_r8, start = (/1/), count = (/2/))
                            if (ierr == NF90_NOERR) then
                                t1_r8 = tt_r8(1)
                                t2_r8 = tt_r8(2)
                            end if
                        case default
                            ierr = NF90_EBADTYPE
!                         call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // &
!                              '): Datatype of variable is not supported.')
!                         call program_abort()
                    end select
                end if
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "Unsupported data type for the '" // trim(cm%dat(vid)%name_time) // "' attribute in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
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

                      ! convert julian day back to year, day, month, hour, minutes, seconds
!                      call dec2date(jdate, dd, mm, yy, hh, mi, ss)
                cm%dat(vid)%start_date%year = floor(dt_r8/365.25) + 1601
                cm%dat(vid)%start_date%jday = floor(dt_r8) - floor((cm%dat(vid)%start_date%year - 1601)*365.25)
                cm%dat(vid)%start_date%hour = floor(dt_r8 - floor(dt_r8))*24
                cm%dat(vid)%start_date%mins = int(floor(dt_r8 - floor(dt_r8))*60.0*24.0 - cm%dat(vid)%start_date%hour*60.0 + 0.5)
!                      write(*,*) "  "
!                      write(*,'(A,A,A2,I4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A6)') &
!                              "   first timestep in ",trim(cm%dat(vid)%fpath),": ",yy,"-",mm,"-",dd," ",hh,":",mi,":",ss," (UTC)"

                      ! julian date of first day of that year (to determine julian day)
!                      jdate_jan1 = date2dec(dd=1, mm=1, yy=yy)
!                      jday = int(jdate - (jdate_jan1 - 1.))

                      ! check if consistent with settings
!                      if ( yy   /= cm%dat(vid)%start_date%year .or. &
!                           jday /= cm%dat(vid)%start_date%jday .or. &
!                           hh   /= cm%dat(vid)%start_date%hour .or. &
!                           mi   /= cm%dat(vid)%start_date%mins) then
                         ! write(*,'(A40,I4,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2,A1,I2.2)') &
                         !      "first timestep in NetCDF: ",yy,"-",mm,"-",dd," ",hh,":",mi,":",ss
!                         write(*,*)          "  "
!                         write(*,'(A40,I4)') "year       of first time step in NetCDF: ",yy
!                         write(*,'(A40,I4)') "year       of first time step in setup:  ",cm%dat(vid)%start_date%year
!                         write(*,'(A40,I3)') "julian day of first time step in NetCDF: ",jday
!                         write(*,'(A40,I3)') "julian day of first time step in setup:  ",cm%dat(vid)%start_date%jday
!                         write(*,'(A40,I2)') "hour       of first time step in NetCDF: ",hh
!                         write(*,'(A40,I2)') "hour       of first time step in setup:  ",cm%dat(vid)%start_date%hour
!                         write(*,'(A40,I2)') "minutes    of first time step in NetCDF: ",mi
!                         write(*,'(A40,I2)') "minutes    of first time step in setup:  ",cm%dat(vid)%start_date%mins
!                         call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // &
!                              '): The first timestep in NetCDF file does not match '// &
!                              'the first timestep specified in MESH_parameters_CLASS.ini.')
!                         call program_abort()
!                      end if

!                      if ( tstep /= cm%dat(vid)%hf) then
!                         call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // &
!                              '): Time step in forcing file does not match HOURLYFLAG given in '// &
!                              'MESH_input_run_options.ini.')
!                         call program_abort()
!                      end if
!                   end if

                   !> check if dimension name is existing
!                   if (.not.(dim_found)) then
!                      call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // &
!                              '): Dimension ('//trim(nc_dimname(ii))//') not known. '//&
!                              'Must match names given in MESH_input_run_options.ini')
!                      call program_abort()
!                   end if

!                end do

                ! determine the case of the dimension order
!                dims = cm%dat(vid)%nc%v%getDimensions()
                ierr = nf90_inquire_variable(cm%dat(vid)%fiun, cm%dat(vid)%vid, ndims = ii)
                if (ierr == NF90_NOERR) then
                    allocate(nc_dimlen(ii), stat = z)
                    if (z == 0) ierr = nf90_inquire_variable(cm%dat(vid)%fiun, cm%dat(vid)%vid, dimids = nc_dimlen)
                end if
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "Unable to read dimensions of '" // trim(cm%dat(vid)%name_var) // "' in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
                if (ierr == NF90_NOERR) then
                    do ii = 1, size(nc_dimlen)
                        ierr = nf90_inquire_dimension(cm%dat(vid)%fiun, nc_dimlen(ii), name = line)
                        if (ierr == NF90_NOERR) then
                            if (line == cm%dat(vid)%name_lon) then
                                cm%dat(vid)%ncol_lon = ii
                            else if (line == cm%dat(vid)%name_lat) then
                                cm%dat(vid)%ncol_lat = ii
                            else if (line == cm%dat(vid)%name_time) then
                                cm%dat(vid)%ncol_time = ii
                            else
                                ierr = NF90_EBADDIM
!                      call print_error(trim(cm%dat(vid)%fpath) // ' (var=' // trim(cm%dat(vid)%name_var) // &
!                              '): Dimension ('//trim(nc_dimlen(ii)%getName())//') not known. '//&
!                              'Must match names given in MESH_input_run_options.ini')
!                      call program_abort()
                            end if
                        else
                            exit
                        end if
                    end do
                end if
                if (ierr /= NF90_NOERR) then
                    call print_error( &
                        "Unsupported data type for the '" // trim(cm%dat(vid)%name_time) // "' attribute in file: " // &
                        trim(cm%dat(vid)%fpath))
                    call program_abort()
                end if
                if (cm%dat(vid)%ncol_lon == 1 .and. cm%dat(vid)%ncol_lat == 2 .and. cm%dat(vid)%ncol_time == 3) then
                   call print_message('   dim order case #1 --> (lon,lat,time)')
                   cm%dat(vid)%dim_order_case = 1
                else if (cm%dat(vid)%ncol_lon == 2 .and. cm%dat(vid)%ncol_lat == 1 .and. cm%dat(vid)%ncol_time == 3) then
                   call print_message('   dim order case #2 --> (lat,lon,time)')
                   cm%dat(vid)%dim_order_case = 2
                else if (cm%dat(vid)%ncol_lon == 1 .and. cm%dat(vid)%ncol_lat == 3 .and. cm%dat(vid)%ncol_time == 2) then
                   call print_message('   dim order case #3 --> (lon,time,lat)')
                   cm%dat(vid)%dim_order_case = 3
                else if (cm%dat(vid)%ncol_lon == 3 .and. cm%dat(vid)%ncol_lat == 1 .and. cm%dat(vid)%ncol_time == 2) then
                   call print_message('   dim order case #4 --> (lat,time,lon)')
                   cm%dat(vid)%dim_order_case = 4
                else if (cm%dat(vid)%ncol_lon == 2 .and. cm%dat(vid)%ncol_lat == 3 .and. cm%dat(vid)%ncol_time == 1) then
                   call print_message('   dim order case #5 --> (time,lon,lat)')
                   cm%dat(vid)%dim_order_case = 5
                else if (cm%dat(vid)%ncol_lon == 3 .and. cm%dat(vid)%ncol_lat == 2 .and. cm%dat(vid)%ncol_time == 1) then
                   call print_message('   dim order case #6 --> (time,lat,lon)')
                   cm%dat(vid)%dim_order_case = 6
                else
                   call print_error(trim(cm%dat(vid)%fname) // ' (' // trim(cm%dat(vid)%name_var) // &
                        '): Weird order of dimensions.')
                   call program_abort()
                end if

                !> retrieve variable ID
!                call check(nf90_inq_varid(cm%dat(vid)%fiun, & ! in:  file handle of opened file
!                     trim(cm%dat(vid)%name_var),            & ! in:  variable name
!                     cm%dat(vid)%varid))                      ! out: variable ID
!                cm%dat(vid)%varid = cm%dat(vid)%nc%v%id

                cm%dat(vid)%blocktype = cbk%GRD
#else
                call print_error( &
                    "NetCDF format is specified for '" // trim(field%vname) // "' but the module is not active." // &
                    " A version of MESH compiled with the NetCDF library must be used to create files in this format.")
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
    !*  skip_data: .true. to skip data; .false. to store data.
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error reading from the file.
    function load_data(shd, cm, vid, skip_data) result(ENDDATA)

        !> 'shd_variables': For 'shd' variable.
        !> 'netcdf': For netCDF library.
        use shd_variables
#ifdef NETCDF
!      use mo_ncread, only:        &
!           NcOpen,                & ! Open NetCDF
           ! NcClose,             & ! Close NetCDF
!           Get_NcDim,             & ! Get length of dimensions for variable
!           Get_NcDimAtt,          & ! Get dimension name and length
!           Get_NcVarAtt,          & ! Get attributes of a variable
!           Get_NcVarType,         & ! get type of a variable
!           Get_NcVar,             & ! Get variable
!           check                    ! check if NetCDF call was feasible
!      use mo_netcdf
!      use netcdf,  only:          &
!           nf90_get_var             ! read data from NetCDF
        use netcdf
#endif

        !> Input variables.
        type(ShedGridParams) shd
        integer, intent(in) :: vid
        logical, intent(in) :: skip_data

        !> Input/Output variables.
        type(clim_info) cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer t, j, i, ierr
        real GRD(shd%yCount, shd%xCount)
        character(len = DEFAULT_LINE_LENGTH) line
        logical storedata

        !> Local variables for NetCDF
        real, dimension(:,:,:), allocatable  :: GRD_tmp  ! same as GRD but with mixed order of three dimensions
        integer, dimension(3)                :: start    ! where to start reading NetCDF variable
        integer, dimension(3)                :: a_count  ! how much to read from NetCDF variable
        integer                              :: tt

        !> Return if the file is not open or if it is not time to read new blocks.
        if (.not. cm%dat(vid)%fopen .or. cm%dat(vid)%iblock > 1) return

        !> Store data is 'skip_data' is not .true..
        storedata = (.not. skip_data)

        ENDDATA = .false.

        !> Reset the blocks.
        if (storedata) cm%dat(vid)%blocks = 0.0

        !> The outer loop is the number of time-steps read into memory at once.
        do t = 1, cm%dat(vid)%nblocks

            !> Read data according to the format of the file.
            select case (cm%dat(vid)%ffmt)

                !> ASCII R2C format.
                case (1)
                    read(cm%dat(vid)%fiun, *, end = 999) !':Frame'
                    read(cm%dat(vid)%fiun, *, end = 999) ((GRD(i, j), j = 1, shd%xCount), i = 1, shd%yCount)
                    read(cm%dat(vid)%fiun, *, end = 999) !':EndFrame'
                    if (storedata) then
                        do i = 1, shd%NA
                            cm%dat(vid)%blocks(i, t) = GRD(shd%yyy(i), shd%xxx(i))
                        end do
                    end if

                !> CSV format.
                case (2)
                    if (storedata) then
                        read(cm%dat(vid)%fiun, *, end = 999) (cm%dat(vid)%blocks(j, t), j = 1, shd%lc%NTYPE)
                    else
                        read(cm%dat(vid)%fiun, *, end = 999)
                    end if

                !> Binary sequential format.
                case (3)
                    if (storedata) then
                        read(cm%dat(vid)%fiun, end = 999) !NTIME
                        read(cm%dat(vid)%fiun, end = 999) (cm%dat(vid)%blocks(i, t), i = 1, shd%NA)
                    else
                        read(cm%dat(vid)%fiun, end = 999)
                        read(cm%dat(vid)%fiun, end = 999)
                    end if

                !> ASCII format.
                case (4)
                    read(cm%dat(vid)%fiun, *, end = 999) (cm%dat(vid)%blocks(i, t), i = 1, shd%NA)

                !> CLASS format MET file.
                case (6)
                    if (vid /= ck%MET) return
                    if (storedata) then
                        read(cm%dat(vid)%fiun, *, end = 999) i, i, i, i, &
                            cm%dat(ck%FB)%blocks(1, t), cm%dat(ck%FI)%blocks(1, t), cm%dat(ck%RT)%blocks(1, t), &
                            cm%dat(ck%TT)%blocks(1, t), cm%dat(ck%HU)%blocks(1, t), cm%dat(ck%UV)%blocks(1, t), &
                            cm%dat(ck%P0)%blocks(1, t)
                        cm%dat(ck%TT)%blocks(1, t) = cm%dat(ck%TT)%blocks(1, t) + 273.16
                    else
                        read(cm%dat(vid)%fiun, *, end = 999)
                    end if

                !> netCDF format.
                case(7)
#ifdef NETCDF
                   if (storedata) then

                      ! make order of dimensions flexible
                      select case(cm%dat(vid)%dim_order_case)
                      case(1)
                         allocate( GRD_tmp(shd%xCount,shd%yCount,cm%dat(vid)%nblocks) )
                         start = (/ 1,1,cm%dat(vid)%skip+1 /)
                      case(2)
                         allocate( GRD_tmp(shd%yCount,shd%xCount,cm%dat(vid)%nblocks) )
                         start = (/ 1,1,cm%dat(vid)%skip+1 /)
                      case(3)
                         allocate( GRD_tmp(shd%xCount,cm%dat(vid)%nblocks,shd%yCount) )
                         start = (/ 1,cm%dat(vid)%skip+1,1 /)
                      case(4)
                         allocate( GRD_tmp(shd%yCount,cm%dat(vid)%nblocks,shd%xCount) )
                         start = (/ 1,cm%dat(vid)%skip+1,1 /)
                      case(5)
                         allocate( GRD_tmp(cm%dat(vid)%nblocks,shd%xCount,shd%yCount) )
                         start = (/ cm%dat(vid)%skip+1,1,1 /)
                      case(6)
                         allocate( GRD_tmp(cm%dat(vid)%nblocks,shd%yCount,shd%xCount) )
                         start = (/ cm%dat(vid)%skip+1,1,1 /)
                      case default
                         call print_error(trim(cm%dat(vid)%fname) // ' (' // trim(cm%dat(vid)%name_var) // &
                              '): Weird order of dimensions.')
                         call program_abort()
                      end select

                      !> set how much data will be read in each dimension
                      a_count = (/ size(GRD_tmp,1), size(GRD_tmp,2), size(GRD_tmp,3) /)

                      !> read <cm%dat(vid)%nblocks> timesteps of whole domain
!                      call Get_NcVar(                  &
!                           trim(cm%dat(vid)%fpath),    &  !          in:  filename
!                           trim(cm%dat(vid)%name_var), &  !          in:  variable name
!                           GRD_tmp,                    &  !          out: data
!                           start,                      &  ! optional in:  where to start reading
!                           a_count,                    &  ! optional in:  how much to read
!                           fid=cm%dat(vid)%fiun)          ! optional in:  file handle of opened file
!                      call cm%dat(vid)%nc%v%getData(GRD_tmp, start, a_count)
                        ierr = nf90_get_var(cm%dat(vid)%fiun, cm%dat(vid)%vid, GRD_tmp, start, a_count)
                        if (ierr /= NF90_NOERR) then
                            print *, ierr
                            goto 999
                        end if

                      !> retrieve data
                      ! call check(nf90_get_var( &
                      !      cm%dat(vid)%fiun,   & ! in:           file handle of opened file
                      !      cm%dat(vid)%varid,  & ! in:           variable id
                      !      GRD_tmp,            & ! out:          data
                      !      start=start,        & ! optional in:  where to start reading
                      !      count=a_count))       ! optional in:  how much to read

                      !> increase skip for next read
                      cm%dat(vid)%skip = cm%dat(vid)%skip + cm%dat(vid)%nblocks

                      !> bring GRD_tmp with flexible dimension order to GRD(nlat,nlon)
                      do tt=1,cm%dat(vid)%nblocks

                         select case(cm%dat(vid)%dim_order_case)
                         case(1)
                            ! GRD = transpose( GRD_tmp(:,:,tt) )
                            do i = 1, shd%NA
                               cm%dat(vid)%blocks(i, tt) = GRD_tmp(shd%xxx(i), shd%yyy(i), tt)
                            end do
                         case(2)
                            ! GRD = GRD_tmp(:,:,tt)
                            do i = 1, shd%NA
                               cm%dat(vid)%blocks(i, tt) = GRD_tmp(shd%yyy(i), shd%xxx(i), tt)
                            end do
                         case(3)
                            ! GRD = transpose( GRD_tmp(:,tt,:) )
                            do i = 1, shd%NA
                               cm%dat(vid)%blocks(i, tt) = GRD_tmp(shd%xxx(i), tt, shd%yyy(i))
                            end do
                         case(4)
                            ! GRD = GRD_tmp(:,tt,:)
                            do i = 1, shd%NA
                               cm%dat(vid)%blocks(i, tt) = GRD_tmp(shd%yyy(i), tt, shd%xxx(i))
                            end do
                         case(5)
                            ! GRD = transpose( GRD_tmp(tt,:,:) )
                            do i = 1, shd%NA
                               cm%dat(vid)%blocks(i, tt) = GRD_tmp(tt, shd%xxx(i), shd%yyy(i))
                            end do
                         case(6)
                            ! GRD = GRD_tmp(tt,:,:)
                            do i = 1, shd%NA
                               cm%dat(vid)%blocks(i, tt) = GRD_tmp(tt, shd%yyy(i), shd%xxx(i))
                            end do
                         case default
                            call print_error(trim(cm%dat(vid)%fname) // ' (' // trim(cm%dat(vid)%name_var) // &
                                 '): Weird order of dimensions.')
                            call program_abort()
                         end select

                      end do

                   end if ! end if store_data

                   exit   ! don't loop over time steps (NetCDF reads all blocks at once)
#else
                   call print_error('NetCDF files can only be read when MESH is compiled with "make netcdf"')
                   call program_abort()
#endif
                !> Unknown file format.
                case default
                    call print_error(trim(cm%dat(vid)%fname) // ' (' // trim(cm%dat(vid)%id_var) // '): Unsupported file format.')
                    call program_abort()

            end select
        end do

        return

999     ENDDATA = .true.

    end function

    !> Description:
    !>  Load data for the climate forcing variable from file.
    !>
    !> Input/output variables:
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and its unit.
    !*  vid: Index of the climate forcing variable.
    !*  skip_data: .true. to skip data; .false. to store data.
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error updating the climate input forcing data.
    function update_data(shd, cm, vid, skip_data) result(ENDDATA)

        !> 'shd_variables': For 'shd' variable.
        use shd_variables

        !> Input variables.
        type(ShedGridParams) shd
        integer, intent(in) :: vid
        logical, intent(in) :: skip_data

        !> Input/Output variables.
        type(clim_info) cm

        !> Ouput variables.
        logical ENDDATA

        !> Local variables.
        logical storedata

        !> Return if the file is not open or if it is not time to read new blocks.
        if (.not. cm%dat(vid)%fopen .or. cm%dat(vid)%iblock > 1) return

        !> Store data is 'skip_data' is not .true..
        storedata = .not. skip_data

        ENDDATA = .false.

        !> Read data (if needed).
        if (load_data(shd, cm, vid, .not. storedata)) goto 999

        !> Update the counter of the current time-step.
        if (cm%dat(vid)%nblocks > 1) then
            cm%dat(vid)%iblock = cm%dat(vid)%iblock + 1
            if (cm%dat(vid)%iblock > cm%dat(vid)%nblocks) then
                cm%dat(vid)%iblock = 1
            end if
        end if

        return

999     ENDDATA = .true.

    end function

end module
