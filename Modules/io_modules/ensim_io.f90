module ensim_io

    implicit none

    integer, parameter :: MAX_WORDS = 200, MAX_WORD_LENGTH = 20, MAX_LINE_LENGTH = 1000

    interface r2c_to_rank
        module procedure r2c_to_rank_ffield
        module procedure r2c_to_rank_ifield
    end interface

    type ensim_keyword
        character(len = MAX_WORD_LENGTH) :: keyword = ''
        character(len = MAX_WORD_LENGTH), dimension(:), allocatable :: words
    end type

    type ensim_date
        integer :: year = 0, month = 0, day = 0, hour = 0, mins = 0, secs = 0
    end type

    type ensim_attr
        character(len = MAX_WORD_LENGTH) :: attr = '', units = ''
        character(len = MAX_LINE_LENGTH) :: frame_string = ''
        type(ensim_date) frame_date
        real, dimension(:, :), allocatable :: val
    end type

    contains

    subroutine open_ensim_file(iun, fname, ierr, verbose)

        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname
        logical, intent(in) :: verbose
        integer, intent(out) :: ierr

        if (verbose) print 1000, fname
        open(iun, file = adjustl(fname), status = 'old', iostat = ierr)
        if (ierr /= 0) goto 999

        return

999     if (verbose) print 1110, adjustl(fname)
        stop

1000    format(1x, 'READING: ', (a))
1110    format(/1x, 'ERROR: Error opening ', (a), &
               /3x, 'Check that the file exists or use an alternate format of file.'/)

    end subroutine

    subroutine read_ensim_line(iun, line, ierr)

        use strings

        integer, intent(in) :: iun
        character(len = *), intent(out) :: line
        integer, intent(out) :: ierr

        do while (ierr == 0)
            read(iun, '(a)', iostat = ierr) line
            if (ierr /= 0) exit
            call compact(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#') cycle
            if (len_trim(line) > 0) exit
        end do

        return

    end subroutine

    logical function is_header(line)

        use strings

        character(len = *), intent(in) :: line

        is_header = .false.

        if (len_trim(line) < 10) return
        if (lowercase(line(1:10)) == ':endheader') is_header = .true.

    end function

    subroutine parse_header_ensim(iun, fname, vkeyword, nkeyword, ierr)

        use strings

        integer, intent(in) :: iun
        character(len = *), intent(in) :: fname
        type(ensim_keyword), dimension(:), allocatable, intent(out) :: vkeyword
        integer, intent(out) :: nkeyword, ierr

        character(len = MAX_LINE_LENGTH) line
        character(len = MAX_WORD_LENGTH), dimension(MAX_WORDS) :: args
        integer n, nargs

        nkeyword = 0
        ierr = 0

        do while (ierr == 0)
            call read_ensim_line(iun, line, ierr)
            if (ierr /= 0) exit
            if (line(1:1) == ':' .and. index(line, ' ') > 0) nkeyword = nkeyword + 1
            if (is_header(line)) exit
        end do
        if (ierr /= 0) goto 999
        rewind(iun)

        allocate(vkeyword(nkeyword), stat = ierr)
        if (ierr /= 0) goto 999

        n = 0
        do while (ierr == 0)
            call read_ensim_line(iun, line, ierr)
            if (ierr /= 0) exit
            if (line(1:1) == ':' .and. index(line, ' ') > 0) then
                n = n + 1
                call parse(line, ' ', args, nargs)
                vkeyword(n)%keyword = args(1)
                if (nargs > 1) then
                    allocate(vkeyword(n)%words(nargs - 1), stat = ierr)
                    if (ierr /= 0) exit
                    vkeyword(n)%words = args(2:nargs)
                end if
            end if
            if (is_header(line)) exit
        end do
        if (ierr /= 0) goto 999

        return

999     nkeyword = 0
        if (allocated(vkeyword)) then
            do n = 1, size(vkeyword)
                if (allocated(vkeyword(n)%words)) deallocate(vkeyword(n)%words)
            end do
            deallocate(vkeyword)
        end if
        return

    end subroutine

    subroutine parse_header_attribute_ensim(iun, fname, vkeyword, nkeyword, vattr, nattr, ierr)

        use strings

        character(len = *), intent(in) :: fname
        integer, intent(in) :: iun, nkeyword
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword
        type(ensim_attr), dimension(:), allocatable, intent(out) :: vattr
        integer, intent(out) :: nattr, ierr

        integer n, j, i

        nattr = 0
        do n = 1, nkeyword
            if (lowercase(vkeyword(n)%keyword) == ':attributename' .and. size(vkeyword(n)%words) > 1) nattr = nattr + 1
        end do
        if (nattr == 0) goto 999

        allocate(vattr(nattr), stat = ierr)
        if (ierr /= 0) goto 999

        do n = 1, nkeyword
            if (.not. allocated(vkeyword(n)%words) .or. size(vkeyword(n)%words) < 2) cycle
            call value(vkeyword(n)%words(1), j, ierr)
            if (ierr /= 0) cycle
            select case (lowercase(vkeyword(n)%keyword))
                case (':attributename')
                    vattr(j)%attr = ''
                    do i = 2, size(vkeyword(n)%words)
                        vattr(j)%attr = trim(adjustl(vattr(j)%attr)) // ' ' // trim(adjustl(vkeyword(n)%words(i)))
                    end do
                case (':attributeunits')
                    vattr(j)%units = ''
                    do i = 2, size(vkeyword(n)%words)
                        vattr(j)%units = trim(adjustl(vattr(j)%units)) // ' ' // trim(adjustl(vkeyword(n)%words(i)))
                    end do
            end select
        end do

        return

999     nattr = 0
        if (allocated(vattr)) deallocate(vattr)
        return

    end subroutine

    subroutine validate_header_spatial(fname, vkeyword, nkeyword, xcount, xdelta, xorigin, ycount, ydelta, yorigin, verbose)

        use strings

        character(len = *), intent(in) :: fname
        integer, intent(in) :: nkeyword, xcount, ycount
        type(ensim_keyword), dimension(nkeyword), intent(in) :: vkeyword
        real, intent(in) :: xdelta, xorigin, ydelta, yorigin
        logical, intent(in) :: verbose

        integer ix, iy, n, ierr
        real dx, x, dy, y

        ierr = 0
        do n = 1, nkeyword
            select case (lowercase(vkeyword(n)%keyword))
                case (':xcount')
                    call value(vkeyword(n)%words(1), ix, ierr)
                case (':xdelta')
                    call value(vkeyword(n)%words(1), dx, ierr)
                case (':xorigin')
                    call value(vkeyword(n)%words(1), x, ierr)
                case (':ycount')
                    call value(vkeyword(n)%words(1), iy, ierr)
                case (':ydelta')
                    call value(vkeyword(n)%words(1), dy, ierr)
                case (':yorigin')
                    call value(vkeyword(n)%words(1), y, ierr)
            end select
        end do

        if (ix /= xcount .or. iy /= ycount .or. x /= xorigin .or. y /= yorigin .or. dx /= xdelta .or. dy /= ydelta) goto 999

        return

999     if (verbose) then
            print 1120, adjustl(fname), 'Drainage Database'
            print 1110, repeat('-', 16), repeat('-', 16), repeat ('-', 16)
            print 1110, 'Attribute', 'File 1', 'File 2'
            print 1110, repeat('-', 16), repeat('-', 16), repeat ('-', 16)
            print 1110, 'xCount', ix, xcount
            print 1110, 'xDelta', dx, xdelta
            print 1110, 'xOrigin', x, xorigin
            print 1110, 'yCount', iy, ycount
            print 1110, 'yDelta', dy, ydelta
            print 1110, 'yOrigin', y, yorigin
        end if
        stop

1110    format(3x, 999(1x, g16.9))
1120    format(/1x, 'ERROR: Mismatch in spatial attributes between: ', &
               /3x, 'File 1: ', (a), &
               /3x, 'File 2: ', (a)/)

    end subroutine

    subroutine load_data_r2c(iun, fname, vattr, nattr, xcount, ycount, frame, ierr)

        use strings

        character(len = *), intent(in) :: fname
        integer, intent(in) :: iun, nattr, xcount, ycount
        type(ensim_attr), dimension(:), allocatable :: vattr
        logical, intent(in) :: frame
        integer, intent(out) :: ierr

        character(len = MAX_LINE_LENGTH) line
        integer n, j, i

        rewind(iun)
        ierr = 0
        do while (ierr == 0)
            call read_ensim_line(iun, line, ierr)
            if (ierr /= 0) exit
            if (is_header(line)) exit
        end do
        if (ierr /= 0) goto 999

        do n = 1, nattr
            if (frame) then
                call readline(iun, vattr(n)%frame_string, ierr)
                if (ierr /= 0) exit
            end if
            if (allocated(vattr(n)%val)) deallocate(vattr(n)%val)
            allocate(vattr(n)%val(xcount, ycount), stat = ierr)
            if (ierr /= 0) exit
            do i = 1, ycount
                read(iun, *, iostat = ierr) (vattr(n)%val(j, i), j = 1, xcount)
                if (ierr /= 0) goto 999
            end do
            if (frame) then
                call readline(iun, line, ierr)
                if (ierr /= 0) exit
            end if
        end do
        if (ierr /= 0) goto 999

        return

999     do n = 1, nattr
            if (allocated(vattr(n)%val)) deallocate(vattr(n)%val)
        end do
        return

    end subroutine

    subroutine r2c_to_rank_ffield(iun, fname, vattr, nattr, iattr, xxx, yyy, na, ffield, nfield, verbose)

        character(len = *), intent(in) :: fname
        integer, intent(in) :: iun, nattr, iattr, na, nfield
        type(ensim_attr), dimension(nattr), intent(in) :: vattr
        integer, dimension(na), intent(in) :: xxx, yyy
        real, dimension(:), allocatable, intent(out) :: ffield
        logical, intent(in) :: verbose

        integer n, ierr

        if (allocated(ffield)) deallocate(ffield)
        if (iattr > nattr) goto 999
        if (.not. allocated(vattr(iattr)%val)) goto 999

        allocate(ffield(nfield), stat = ierr)
        if (ierr /= 0) goto 999
        ffield = 0.0

        do n = 1, nfield
            ffield(n) = vattr(iattr)%val(xxx(n), yyy(n))
        end do

        return

999     if (verbose) print 1120, iattr, adjustl(fname)
        stop

1110    format(3x, 999(g16.9))
1120    format(/1x, 'ERROR: Bad data in Attribute ', i3, ' of ', (a), '.')

    end subroutine

    subroutine r2c_to_rank_ifield(iun, fname, vattr, nattr, iattr, xxx, yyy, na, ifield, nfield, verbose)

        character(len = *), intent(in) :: fname
        integer, intent(in) :: iun, nattr, iattr, na, nfield
        type(ensim_attr), dimension(nattr) :: vattr
        integer, dimension(na), intent(in) :: xxx, yyy
        integer, dimension(:), allocatable, intent(out) :: ifield
        logical, intent(in) :: verbose

        real, dimension(:), allocatable :: ffield
        call r2c_to_rank_ffield(iun, fname, vattr, nattr, iattr, xxx, yyy, na, ffield, nfield, verbose)

        ifield = int(ffield)

    end subroutine

end module
