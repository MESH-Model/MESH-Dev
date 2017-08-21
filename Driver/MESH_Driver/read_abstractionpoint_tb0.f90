!>
!> Description:
!>  Subroutine to read abstraction location information from
!>  MESH_input_abstractionpoint.tb0.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: 'MESH_input_abstractionpoint.tb0').
!>
subroutine read_abstractionpoint_tb0(shd, iun, fname)

    use strings
    use mpi_module
    use model_dates
    use sa_mesh_shared_variables
    use ensim_io

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer :: iun
    character(len = *) :: fname

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    integer nkeyword, ierr
    logical verbose

    !> Local variables.
    verbose = (ro%VERBOSEMODE > 0)

    !> Open the file and read the header.
    call open_ensim_file(iun, fname, ierr, verbose)
    call parse_header_ensim(iun, fname, vkeyword, nkeyword, ierr)

    !> Get the number of locations (i.e., columns) in the file.
    call count_columns_tb0(iun, fname, vkeyword, nkeyword, fms%absp%n, ierr)

    !> Return if no locations are defined.
    if (fms%absp%n == 0) return

    !> Allocate attributes for the driver.
    call allocate_abstraction_point_location(fms%absp, fms%absp%n, ierr)
    if (ierr /= 0) goto 998

    !> Get the time-step of the records.
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':DeltaT', fms%absp%sabst%dts, ierr, verbose)

    !> Populate other attributes.
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':ColumnName', fms%absp%meta%name, fms%absp%n, ierr, verbose)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':ColumnLocationY', fms%absp%meta%y, fms%absp%n, ierr, verbose)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':ColumnLocationX', fms%absp%meta%x, fms%absp%n, ierr, verbose)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':MinStorage', fms%absp%meta%x, fms%absp%n, ierr, verbose)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':MinfracStorage', fms%absp%meta%x, fms%absp%n, ierr, verbose)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':MinLevel', fms%absp%meta%x, fms%absp%n, ierr, verbose)

    !> Get the start time of the first record in the file.
    call parse_starttime( &
        iun, fname, vkeyword, nkeyword, &
        fms%absp%sabst%iyear, fms%absp%sabst%imonth, fms%absp%sabst%iday, fms%absp%sabst%ihour, fms%absp%sabst%imins, &
        ierr, verbose)
    if (fms%absp%sabst%iyear > 0 .and. fms%absp%sabst%imonth > 0 .and. fms%absp%sabst%iday > 0) then
        fms%absp%sabst%ijday = get_jday(fms%absp%sabst%imonth, fms%absp%sabst%iday, fms%absp%sabst%iyear)
    end if

    !> Position the file to the first record.
    call advance_past_header(iun, fname, verbose, ierr)

    return

    !> File errors.
997 if (ipid == 0) print "(1x, 'ERROR: ', (a), ' may not exist.')", trim(adjustl(fname))
998 if (ipid == 0) print "(3x, 'ERROR allocating values based on ', (a), '.')", trim(adjustl(fname))
999 if (ipid == 0) print "(3x, 'ERROR reading from ', (a), '.')", trim(adjustl(fname))

    stop

1000    format(1x, 'READING: ', (a))

end subroutine
