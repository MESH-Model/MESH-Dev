!>
!> Description:
!>  Subroutine to read streamflow gauge information from
!>  MESH_input_streamflow.tb0.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: 'MESH_input_streamflow.tb0').
!>
subroutine read_streamflow_tb0(shd, iun, fname)

    use strings
    use mpi_module
    use model_dates
    use sa_mesh_variables
    use sa_mesh_utilities
    use ensim_io

    implicit none

    !> Input variables.
    type(ShedGridParams) shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    integer nkeyword, ierr

    !> Open the file and read the header.
    call print_screen('READING: ' // trim(adjustl(fname)))
    call print_echo_txt(fname)
    call open_ensim_file(iun, fname, ierr)
    if (ierr /= 0) then
        call print_error('Unable to open file. Check if the file exists.')
        call stop_program()
    end if
    call parse_header_ensim(iun, fname, vkeyword, nkeyword, ierr)

    !> Get the number of gauge locations (i.e., columns) from the file.
    call count_columns_tb0(iun, fname, vkeyword, nkeyword, fms%stmg%n, ierr)

    !> Return if no gauge locations are defined.
    if (fms%stmg%n == 0) return

    !> Allocate attributes for the driver.
    call allocate_streamflow_gauge_location(fms%stmg, fms%stmg%n, ierr)
    if (ierr /= 0) goto 998

    !> Get the time-step of the records.
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':DeltaT', fms%stmg%qomeas%dts, ierr, VERBOSEMODE)

    !> Populate other attributes.
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':ColumnName', fms%stmg%meta%name, fms%stmg%n, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':ColumnLocationY', fms%stmg%meta%y, fms%stmg%n, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':ColumnLocationX', fms%stmg%meta%x, fms%stmg%n, ierr, VERBOSEMODE)

    !> Get the start time of the first record in the file.
    call parse_starttime( &
        iun, fname, vkeyword, nkeyword, &
        fms%stmg%qomeas%iyear, fms%stmg%qomeas%imonth, fms%stmg%qomeas%iday, fms%stmg%qomeas%ihour, fms%stmg%qomeas%imins, &
        ierr, VERBOSEMODE)
    if (fms%stmg%qomeas%iyear > 0 .and. fms%stmg%qomeas%imonth > 0 .and. fms%stmg%qomeas%iday > 0) then
        fms%stmg%qomeas%ijday = get_jday(fms%stmg%qomeas%imonth, fms%stmg%qomeas%iday, fms%stmg%qomeas%iyear)
    end if

    !> Position the file to the first record.
    call advance_past_header(iun, fname, VERBOSEMODE, ierr)

    return

    !> Stop: Error allocating variables.
998 call print_error('Unable to allocate variables.')
    call stop_program()

end subroutine
