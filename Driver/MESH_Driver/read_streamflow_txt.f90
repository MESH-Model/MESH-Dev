!>
!> Description:
!>  Subroutine to read streamflow gauge information from
!>  MESH_input_streamflow.txt.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
!>
subroutine read_streamflow_txt(shd, iun, fname)

    use mpi_module
    use sa_mesh_variables
    use sa_mesh_utilities

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Local variables.
    integer i, ierr
    character(len = DEFAULT_LINE_LENGTH) line

    !> Streamflow attributes pulled from 'fms':
    !*  stmg: Streamflow gauge structure
    !*  -   n: Number of elements dimensioned.
    !*  -   name(n): ID printed to output files.
    !*  -   y(n): Y-coordinate of outlet location.
    !*  -   x(n): X-coordinate of outlet location.
    !*  -   iy(n): Vertical index of the grid-cell containing the location.
    !*  -   jx(n): Horizontal index of the grid-cell containing the location.
    !*  -   n(n): Rank or index of the grid-cell containing the location.
    !*  -   DA(n): Drainage area.

    !> Open the file.
    call print_screen('READING: ' // trim(fname))
    open(iun, file = fname, status = 'old', action = 'read', err = 997)

    !> Read the number of locations.
    read(iun, *, err = 999)
    read(iun, *, err = 999) &
        fms%stmg%n, i, i, fms%stmg%qomeas%dts, fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday, fms%stmg%qomeas%ihour

    !> Return if no locations were defined.
    if (fms%stmg%n == 0) return

    !> Allocate configuration variables for the driver.
    call allocate_streamflow_gauge_location(fms%stmg, fms%stmg%n, ierr)
    if (ierr /= 0) goto 998

    !> Read gauge location and name.
    do i = 1, fms%stmg%n
        read(iun, *, err = 999) fms%stmg%meta%y(i), fms%stmg%meta%x(i), fms%stmg%meta%name(i)
    end do
    fms%stmg%meta%y = fms%stmg%meta%y/60.0
    fms%stmg%meta%x = fms%stmg%meta%x/60.0

    return

    !> Stop: File not found.
997 call print_error('Unable to open file.')
    call stop_program()

    !> Stop: Error allocating variables.
998 call print_error('Unable to allocate variables.')
    call stop_program()

    !> Stop: Premature end of file.
999 call print_error('Unable to read from file.')
    write(line, 1001) fms%rsvr%n
    call print_message('Number of gauge locations expected: ' // trim(adjustl(line)))
    write(line, 1001) i
    call print_message('Number found: ' // trim(adjustl(line)))
    call stop_program()

    !> Format statements.
1001    format(9999(g15.6, 1x))

end subroutine
