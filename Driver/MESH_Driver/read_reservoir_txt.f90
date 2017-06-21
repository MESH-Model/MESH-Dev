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
    use sa_mesh_shared_variables

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

    if (ro%VERBOSEMODE > 0) print 1000, fname
    open(iun, file = fname, status = 'old', action = 'read', err = 997)
    read(iun, *, err = 999) fms%rsvr%n, NR, fms%rsvr%qorls%dts
    NR = fms%rsvr%n

    !> Return if there are no reservoirs.
    if (NR == 0) return

    !> Allocate configuration variables for the driver.
    allocate(fms%rsvr%name(NR), &
             fms%rsvr%y(NR), fms%rsvr%x(NR), &
             fms%rsvr%iy(NR), fms%rsvr%jx(NR), fms%rsvr%rnk(NR), &
             fms%rsvr%cfn(NR), &
             fms%rsvr%b1(NR), fms%rsvr%b2(NR), fms%rsvr%b3(NR), fms%rsvr%b4(NR), fms%rsvr%b5(NR), &
             stat = ierr)
    if (ierr /= 0) goto 998
    fms%rsvr%cfn = 0
    fms%rsvr%b1 = 0.0; fms%rsvr%b2 = 0.0; fms%rsvr%b3 = 0.0; fms%rsvr%b4 = 0.0; fms%rsvr%b5 = 0.0

    !> Read information.
    do l = 1, NR
        read(iun, *, err = 999) fms%rsvr%y(l), fms%rsvr%x(l), fms%rsvr%b1(l), fms%rsvr%b2(l), fms%rsvr%name(l)
        fms%rsvr%y(l) = fms%rsvr%y(l)/60.0
        fms%rsvr%x(l) = fms%rsvr%x(l)/60.0
    end do

    return

    !> File errors.
997 if (ro%VERBOSEMODE > 0) print "(1x, 'ERROR: ', (a), ' may not exist.')", fname
998 if (ro%VERBOSEMODE > 0) print "(3x, 'ERROR allocating values based on ', (a), '.')", fname
999 if (ro%VERBOSEMODE > 0) print "(3x, 'ERROR reading from ', (a), '.')", fname

    stop

1000    format(1x, 'READING: ', (a))

end subroutine
