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

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer :: iun
    character(len = *) :: fname

    !> Local variables.
    integer NS, l, ierr

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

    if (ro%VERBOSEMODE > 0) print 1000, trim(fname)
    open(iun, file = fname, status = 'old', action = 'read', err = 997)
    read(iun, *, err = 999)
    read(iun, *, err = 999) &
        fms%stmg%n, NS, NS, fms%stmg%qomeas%dts, fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday, fms%stmg%qomeas%ihour
    NS = fms%stmg%n

    !> Return if there are no gauge locations.
    if (NS == 0) return

    !> Allocate configuration variables for the driver.
    call allocate_streamflow_gauge_location(fms%stmg, NS, ierr)
    if (ierr /= 0) goto 998

    !> Read gauge location and name.
    do l = 1, NS
        read(iun, *, err = 999) fms%stmg%meta%y(l), fms%stmg%meta%x(l), fms%stmg%meta%name(l)
    end do
    fms%stmg%meta%y = fms%stmg%meta%y/60.0
    fms%stmg%meta%x = fms%stmg%meta%x/60.0

    return

    !> File errors.
997 if (ipid == 0) print "(1x, 'ERROR: ', (a), ' may not exist.')", trim(fname)
998 if (ipid == 0) print "(3x, 'ERROR allocating values based on ', (a), '.')", trim(fname)
999 if (ipid == 0) print "(3x, 'ERROR reading from ', (a), '.')", trim(fname)

    stop

1000    format(1x, 'READING: ', (a))

end subroutine
