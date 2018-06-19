!>
!> Description:
!>  Subroutine to read abstraction location information from
!>  MESH_input_abstractionpoint.txt.
!>
!> Input:
!*  shd: Basin shed object containing grid and drainage properties.
!*  iun: Unit of the input file.
!*  fname: Full path to the file.
!>
subroutine read_abstractionpoint_txt(shd, iun, fname)

    use mpi_module
    use sa_mesh_common

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Local variables.
    integer i, ierr
    character(len = DEFAULT_LINE_LENGTH) line

    !> Open the file and read the number of locations to read from file.
    call print_screen('READING: ' // trim(adjustl(fname)))
    call print_echo_txt(fname)
    open(iun, file = fname, status = 'old', action = 'read', iostat = ierr)
    if (ierr /= 0) then
        call print_error('Unable to open file. Check if the file exists.')
        call program_abort()
    end if

    !> Read the number of locations.
    read(iun, *, err = 999) fms%absp%n

    !> Return if no locations are defined.
    if (fms%absp%n == 0) return

    !> Allocate attributes for the driver.
    call allocate_abstraction_point_location(fms%absp, fms%absp%n, ierr)
    if (ierr /= 0) goto 998

    !> Read information.
    do i = 1, fms%absp%n
        read(iun, *, err = 999) fms%absp%meta%y(i), fms%absp%meta%x(i), fms%absp%meta%name(i)
    end do
    fms%absp%meta%y = fms%absp%meta%y/60.0
    fms%absp%meta%x = fms%absp%meta%x/60.0

    return

    !> Stop: Error allocating variables.
998 call print_error('Unable to allocate variables.')
    call program_abort()

    !> Stop: Premature end of file.
999 call print_error('Unable to read from file.')
    write(line, FMT_GEN) fms%absp%n
    call print_message('Number of locations expected: ' // trim(adjustl(line)))
    write(line, FMT_GEN) i
    call print_message('Number found: ' // trim(adjustl(line)))
    call program_abort()

end subroutine
