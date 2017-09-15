!>
!> Description:
!>  Subroutine to read abstraction location information from
!>  MESH_input_abstractionpoint.txt.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: 'MESH_input_abstractionpoint.txt').
!>
subroutine read_abstractionpoint_txt(shd, iun, fname)

    use mpi_module
    use sa_mesh_shared_variables

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer :: iun
    character(len = *) :: fname

    !> Local variables.
    integer l, ierr
    logical verbose

    !> Local variables.
    verbose = (ro%VERBOSEMODE > 0)

    !> Open the file and read the number of locations to read from file.
    if (verbose) print 1000, trim(fname)
    open(iun, file = fname, status = 'old', action = 'read', err = 997)
    read(iun, *, err = 999) fms%absp%n

    !> Return if no locations are defined.
    if (fms%absp%n == 0) return

    !> Allocate attributes for the driver.
    call allocate_abstraction_point_location(fms%absp, fms%absp%n, ierr)
    if (ierr /= 0) goto 998

    !> Read information.
    do l = 1, fms%absp%n
        read(iun, *, err = 999) fms%absp%meta%y(l), fms%absp%meta%x(l), fms%absp%meta%name(l)
    end do
    fms%absp%meta%y = fms%absp%meta%y/60.0
    fms%absp%meta%x = fms%absp%meta%x/60.0

    return

    !> File errors.
997 if (ipid == 0) print "(1x, 'ERROR: ', (a), ' may not exist.')", trim(fname)
998 if (ipid == 0) print "(3x, 'ERROR allocating values based on ', (a), '.')", trim(fname)
999 if (ipid == 0) print "(3x, 'ERROR reading from ', (a), '.')", trim(fname)

    stop

1000    format(1x, 'READING: ', (a))

end subroutine
