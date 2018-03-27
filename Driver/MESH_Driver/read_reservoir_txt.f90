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
    integer, intent(in) :: iun, nb
    character(len = *), intent(in) :: fname

    !> Local variables.
    integer i, ierr
    character(len = DEFAULT_LINE_LENGTH) line

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

    !> Open the file.
    call print_screen('READING: ' // trim(adjustl(fname)))
    call print_echo_txt(fname)
    open(iun, file = fname, status = 'old', action = 'read', iostat = ierr)
    if (ierr /= 0) then
        call print_error('Unable to open file. Check if the file exists.')
        call stop_program()
    end if

    !> Read the number of locations.
    read(iun, *, err = 999) fms%rsvr%n, i, fms%rsvr%rlsmeas%dts

    !> Return if no locations were defined.
    if (fms%rsvr%n == 0) return

    !> Allocate configuration variables for the driver.
    call allocate_reservoir_outlet_location(fms%rsvr, fms%rsvr%n, ierr)
    if (ierr /= 0) goto 998

    !> Read information.
    do i = 1, fms%rsvr%n
        read(iun, *, err = 999) &
            fms%rsvr%meta%y(i), fms%rsvr%meta%x(i), fms%rsvr%rls%b1(i), fms%rsvr%rls%b2(i), fms%rsvr%meta%name(i)
    end do
    fms%rsvr%meta%y = fms%rsvr%meta%y/60.0
    fms%rsvr%meta%x = fms%rsvr%meta%x/60.0

    return

    !> Stop: Error allocating variables.
998 call print_error('Unable to allocate variables.')
    call stop_program()

    !> Stop: Premature end of file.
999 call print_error('Unable to read from file.')
    write(line, FMT_GEN) fms%rsvr%n
    call print_message('Number of reservoirs expected: ' // trim(adjustl(line)))
    write(line, FMT_GEN) i
    call print_message('Number found: ' // trim(adjustl(line)))
    call stop_program()

end subroutine
