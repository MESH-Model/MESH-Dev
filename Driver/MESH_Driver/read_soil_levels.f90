subroutine READ_SOIL_LEVELS(fls, shd)

    use sa_mesh_variables
    use sa_mesh_utilities
    use model_files_variables

    implicit none

    !> Input variables.
    type(fl_ids) :: fls
    type(ShedGridParams) :: shd

    !> Local variables.
    integer iun, i, ierr
    character(len = DEFAULT_LINE_LENGTH) line
    real DELZ_TEST, ZBOT_TEST

    !> Reset the number of levels.
    shd%lc%IGND = 0

    !> Open the file.
    call print_screen('READING: ' // trim(fls%fl(mfk%f52)%fn))
    iun = fls%fl(mfk%f52)%iun
    open(iun, file = fls%fl(mfk%f52)%fn, status = 'old', action = 'read', err = 997)

    !> Count the number of levels.
    DELZ_TEST = 1.0
    i = 0
    ierr = 0
    do while (DELZ_TEST /= 0.0 .and. ierr == 0)
        read(iun, *, iostat = ierr) DELZ_TEST
        i = i + 1
    end do

    !> 'i' increments an extra time when 'ierr' /= 0.
    shd%lc%IGND = i - 1
    if (shd%lc%IGND < 3) then
        call print_error('The number of soil layers is less than 3. At least 3 layers are required.')
        call stop_program()
    end if

    !> Allocate the level variables.
    !> Rewind the file and read the level definition.
    allocate(shd%lc%sl%DELZ(shd%lc%IGND), shd%lc%sl%ZBOT(shd%lc%IGND), stat = ierr)
    if (ierr /= 0) goto 998
    rewind(iun)
    do i = 1, shd%lc%IGND
        read(iun, *) DELZ_TEST
        shd%lc%sl%DELZ(i) = DELZ_TEST
        if (i > 1) then
            shd%lc%sl%ZBOT(i) = shd%lc%sl%ZBOT(i - 1) + DELZ_TEST
        else
            shd%lc%sl%ZBOT(i) = DELZ_TEST
        end if
    end do
    close(iun)

    return

    !> Stop: File not found.
997 call print_error('Unable to open file.')
    call stop_program()

    !> Stop: Error allocating variables.
998 call print_error('Unable to allocate variables.')
    call stop_program()

    !> Format statements.
1001    format(9999(g15.6, 1x))

end subroutine
