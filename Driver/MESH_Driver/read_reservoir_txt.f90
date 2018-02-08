!>
!> Description:
!>  Subroutine to read reservoir information from
!>  MESH_input_reservoir.txt.
!>
!> Input:
!*  shd: Basin shed object containing grid and drainage properties.
!*  iun: Unit of the input file.
!*  fname: Full path to the file.
!*  nb: Number of 'b' coefficients to read.
!>
subroutine read_reservoir_txt(shd, iun, fname, nb)

    use mpi_module
    use sa_mesh_variables
    use sa_mesh_utilities

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer :: iun, nb
    character(len = *) :: fname

    !> Local variables.
    integer NR, l, ierr

    !> Reservoir attributes pulled from 'fms':
    !*  rsvr: Streamflow gauge structure
    !*  -   n: Number of elements dimensioned.
    !*  -   name(n): ID printed to output files.
    !*  -   y(n): Y-coordinate of outlet location.
    !*  -   x(n): X-coordinate of outlet location.
    !*  -   iy(n): Vertical index of the grid-cell containing the location.
    !*  -   jx(n): Horizontal index of the grid-cell containing the location.
    !*  -   n(n): Rank or index of the grid-cell containing the location.
    !*  -   cfn(n): Type of release curve function.
    !*  -   b(n, :): Release curve coefficients.

    if (VERBOSEMODE) print 1000, trim(fname)
    open(iun, file = fname, status = 'old', action = 'read', err = 997)
    read(iun, *, err = 999) fms%rsvr%n, NR, fms%rsvr%rlsmeas%dts
    NR = fms%rsvr%n

    !> Return if there are no reservoirs.
    if (NR == 0) return

    !> Allocate configuration variables for the driver.
    call allocate_reservoir_outlet_location(fms%rsvr, NR, ierr)
    if (ierr /= 0) goto 998

    !> Read information.
    do l = 1, NR
        read(iun, *, err = 999) &
            fms%rsvr%meta%y(l), fms%rsvr%meta%x(l), fms%rsvr%rls%b1(l), fms%rsvr%rls%b2(l), fms%rsvr%meta%name(l)
    end do
    fms%rsvr%meta%y = fms%rsvr%meta%y/60.0
    fms%rsvr%meta%x = fms%rsvr%meta%x/60.0

    return

    !> File errors.
997 if (ipid == 0) print "(1x, 'ERROR: ', (a), ' may not exist.')", trim(fname)
998 if (ipid == 0) print "(3x, 'ERROR allocating values based on ', (a), '.')", trim(fname)
999 if (ipid == 0) print "(3x, 'ERROR reading from ', (a), '.')", trim(fname)

    stop

1000    format(1x, 'READING: ', (a))

end subroutine
