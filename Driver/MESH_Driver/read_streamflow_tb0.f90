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
    use model_dates
    use sa_mesh_common
    use ensim_io

    implicit none

    !> Input variables.
    type(ShedGridParams) shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    integer nkeyword, ierr, z

    !> Open the file and read the header.
    call print_message('READING: ' // trim(adjustl(fname)))
    call open_ensim_input(iun, fname, ierr)
    if (ierr /= 0) call program_abort()
    call parse_header_ensim(iun, vkeyword, nkeyword, ierr)
    if (ierr /= 0) call program_abort()

    !> Check the spatial definition in the header.
    call validate_header_spatial(vkeyword, nkeyword, shd%CoordSys%Proj, ierr)
    if (ierr /= 0) call program_abort()

    !> Get the number of gauge locations (i.e., columns) from the file.
    call count_columns_tb0(iun, vkeyword, nkeyword, fms%stmg%n, ierr)
    if (ierr /= 0) call program_abort()

    !> Print warning and return if no gauge locations are defined.
    if (fms%stmg%n == 0) then
        call print_warning('No streamflow gauge locations were found.', PAD_3)
        return
    end if

    !> Allocate attributes for the driver.
    call allocate_streamflow_gauge_location(fms%stmg, fms%stmg%n, ierr)
    if (ierr /= 0) then
        call print_error('Unable to allocate variables.')
        call program_abort()
    end if

    !> Parse attributes.
    ierr = 0

    !> Get the time-step of the records.
    call get_keyword_value(iun, vkeyword, nkeyword, ':DeltaT', fms%stmg%qomeas%dts, ierr); if (z /= 0) ierr = z

    !> Populate other attributes.
    call get_keyword_value(iun, vkeyword, nkeyword, ':ColumnName', fms%stmg%meta%name, fms%stmg%n, ierr); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':ColumnLocationY', fms%stmg%meta%y, fms%stmg%n, ierr); if (z /= 0) ierr = z
    call get_keyword_value(iun, vkeyword, nkeyword, ':ColumnLocationX', fms%stmg%meta%x, fms%stmg%n, ierr); if (z /= 0) ierr = z

    !> Get the start time of the first record in the file.
    call parse_starttime( &
        iun, vkeyword, nkeyword, &
        fms%stmg%qomeas%iyear, fms%stmg%qomeas%imonth, fms%stmg%qomeas%iday, fms%stmg%qomeas%ihour, fms%stmg%qomeas%imins, &
        ierr)
    if (z /= 0) ierr = z
    if (fms%stmg%qomeas%iyear > 0 .and. fms%stmg%qomeas%imonth > 0 .and. fms%stmg%qomeas%iday > 0) then
        fms%stmg%qomeas%ijday = get_jday(fms%stmg%qomeas%imonth, fms%stmg%qomeas%iday, fms%stmg%qomeas%iyear)
    end if

    !> Check for errors.
    if (ierr /= 0) call print_warning('Errors occured parsing attributes in the file.', PAD_3)

    !> Position the file to the first record.
    call advance_past_header(iun, fname, ierr)

    return

end subroutine
