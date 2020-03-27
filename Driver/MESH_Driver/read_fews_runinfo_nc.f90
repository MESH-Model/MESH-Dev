!> Description:
!>  Subroutine to read confirguration input from a netCDF format
!>  Delft-FEWS 'runinfo' file.
!>
!> Input variables:
!*  fname: Full path to the file (default: 'runinfo.nc').
!*
!> Output variables:
!*  ierr: Return status.
subroutine read_fews_runinfo_nc(fname, ierr)

    !> 'strings': For 'lowercase' function.
    !> 'sa_mesh_common': For common MESH variables, constants, and routines.
    !> 'nc_io': For routines to read 'r2c' format file.
    use strings
    use sa_mesh_common
    use nc_io

    implicit none

    !> Input variables.
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    character(len = DEFAULT_LINE_LENGTH) line
    character(len = DEFAULT_FIELD_LENGTH) field
    integer iun, v, t, s, z

    !> Initialize the return status.
    ierr = 0

    !> Open the file.
    call reset_tab()
    call print_message('READING: ' // trim(fname))
    call increase_tab()
    call nc4_open_input(fname, iun, ierr)
    if (ierr /= 0) return

    !> Get attributes.
    z = 0
    if (z == 0) call nc4_get_variable_scalar(iun, fname, 'properties', field, z, vid = v)
    t = 0
    if (z == 0) then
        call nc4_get_attribute(iun, fname, 'time_shift', t, z, v)
        if (z == 0) then
            write(line, FMT_GEN) t
            call print_message("Applying a shift of " // trim(adjustl(line)) // " hours to all times.")
        else
            call print_warning("An error occurred reading 'time_shift' or the attribute does not exist.")
            t = 0
        end if
    end if
    if (z == 0) then
        call nc4_get_time_nf90_int( &
            iun, fname, &
            ic%start%year, ic%start%jday, ic%start%month, ic%start%day, ic%start%hour, ic%start%mins, s, &
            z, &
            time_shift = t, name_time = 'start_time')
        if (z == 0) then
            write(line, "(i4, '/', i2.2, '/', i2.2, ' ', i2.2, ':', i2.2, ' (', 4i4, ')')") &
                ic%start%year, ic%start%month, ic%start%day, ic%start%hour, ic%start%mins, &
                ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins
            call print_message('Revised simulation start time: ' // trim(adjustl(line)))
        else
            call print_warning("An error occurred reading 'start_time' or the variable does not exist.")
        end if
    end if
    if (z == 0) then
        call nc4_get_time_nf90_int( &
            iun, fname, &
            ic%stop%year, ic%stop%jday, ic%stop%month, ic%stop%day, ic%stop%hour, ic%stop%mins, s, &
            z, &
            time_shift = t, name_time = 'end_time')
        if (z == 0) then
            write(line, "(i4, '/', i2.2, '/', i2.2, ' ', i2.2, ':', i2.2, ' (', 4i4, ')')") &
                ic%stop%year, ic%stop%month, ic%stop%day, ic%stop%hour, ic%stop%mins, &
                ic%stop%year, ic%stop%jday, ic%stop%hour, ic%stop%mins
            call print_message('Revised simulation stop time: ' // trim(adjustl(line)))
        else
            call print_warning("An error occurred reading 'end_time' or the variable does not exist.")
        end if
    end if
    if (z /= 0) then
        call print_warning('Errors occured reading from the file: ' // trim(fname))
    end if

    !> Close the file to free the unit.
    call nc4_close_file(iun, fname, ierr)

end subroutine
