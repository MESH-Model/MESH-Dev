!> Module to read configuration information for the climate
!> forcing module from file.
!>
!> This module aligns with a file that has :1.0 in its header.
!>
module climate_forcing_config

    use climate_forcing_variabletypes
    use climate_forcing_variables

    implicit none

    contains

    subroutine open_config(cm)

        use strings
        use EF_ParseUtilities
        use sa_mesh_shared_variables

        type(clim_info) :: cm

        integer i, un, ios
        character*256 fname, key, attr, ver_in
        character*4096 ln_in
        logical is_exist, is_climInfo

        !> Open the file if one exists.
        fname = 'climate_info.ini'
        un = cm%basefileunit
        inquire(file = fname, exist = is_exist)
        if (is_exist) then

            !> Open the file.
            open(un, file = fname, status ='old', action = 'read')

            !> First uncommented line contains the tag and version of the file.
            !> :clim_info 1.0
            call readline(un, ln_in, ios)
            ios = SplitLine(ln_in, key, attr)
            if (lowercase(key) == ':clim_info') then
                is_climInfo = .true.
                ver_in = lowercase(attr)
            end if

            !> Call subroutine to parse the content of the file.
            if (is_climInfo) then
                if (ver_in == '1.0') call parse_config_1_0(un, cm)
            end if

            !> Close the file.
            close(un)

        end if

        !> Print summary of climate file information.
        if (ro%VERBOSEMODE > 0 .and. ro%DIAGNOSEMODE > 0) then
            do i = 1, size(cm%clin)
                print 1071, cm%clin(i)%id_var, cm%clin(i)%name, cm%clin(i)%filefmt, cm%clin(i)%unitR, &
                    cm%clin(i)%hf, cm%clin(i)%timeSize, cm%clin(i)%nseries
            end do
        end if

1071    format( &
            1x, 'Climate Variable: ', (a), /, &
            3x, 'Name: ', (a), /, &
            3x, 'File format: ', i5, /, &
            3x, 'File unit (input): ', i8, /, &
            3x, 'Record interval (mins): ', i5, /, &
            3x, 'Timesteps to read into memory: ', i5, /, &
            3x, 'Series count: ', i5)

    end subroutine

    subroutine parse_config_1_0(un, cm)

        use strings

        integer, intent(in) :: un

        type(clim_info) :: cm

        integer ios, i, j, jj, n, nn, indx, nlines, nargs
        character*4096, dimension(:), allocatable :: lines, args
        character*256 key, attr, mark

        !> Extract the sections of the file.
!        call parse_seek_section(un, 'filenames', lines, nlines)

        !> Section of series
        !* Key: Variable name (must match a recognized key)
        !* Attributes:
        !* > 'n': Number of series for the variable
        !* > 'per': Element of discretization for a weighting scheme (e.g., gru) (':i': from 1 to n)
        !* > 'attr': Qualifying attribute (':i': from 1 to n)
        !*           > 'per=gru': attr1 is the order of the gru
        !*           > 'per=gru': attr2 is the weight assigned to the value
        !> :StartSeries
        !> FB, n=2, per:1=gru, per:2=gru, attr1:1=1, attr2:1=0.5, attr1:2=2, attr2:2=1.0
        !> :EndSeries
        call parse_seek_section(un, 'series', lines, nlines)

        !> The lines returned include the :start and :end lines.
        do i = 2, (nlines - 1)
            call parse_split_fields(lines(i), ',', args, nargs)
            if (nargs > 2) then

                !> Extract the climate variable.
                select case (trim(args(1)))
                    case ('fb'); indx = cfk%FB
                    case ('fi'); indx = cfk%FI
                    case ('pr'); indx = cfk%PR
                    case ('tt'); indx = cfk%TT
                    case ('uv'); indx = cfk%UV
                    case ('p0'); indx = cfk%P0
                    case ('hu'); indx = cfk%HU
                end select

                !> Extract the number of series.
                do j = 2, nargs
                    if (args(j)(1:2) == 'n=' .and. len_trim(args(j)) > 2) then
                        call value(args(j)(3:), n, ios)
                        if (ios /= 0) then
!todo: fix this
!print *, 'bad n value'
!stop
                        end if

                        !> Update the number of series.
                        if (n > 0) then
                            cm%clin(indx)%nseries = n
                            if (allocated(cm%clin(indx)%series)) deallocate(cm%clin(indx)%series)
                            allocate(cm%clin(indx)%series(cm%clin(indx)%nseries))
                        end if
                        exit
                    end if
                end do

                !> Extract the attributes of the series.
                do j = 2, nargs
                    call parse_split_attr(args(j), key, mark, attr)
                    select case (key)
                        case ('per')

                            !> Series of weighted data by GRU.
                            if (attr == 'gru') then

                                !> Extract the value of the series.
                                call value(mark, n, ios)

                                !> Assigned the type of the attribute as 'gru'.
                                cm%clin(indx)%series(n)%attrtype = attr

                                !> Populate the variables for this type of series.
                                if (allocated(cm%clin(indx)%series(n)%attr)) deallocate(cm%clin(indx)%series(n)%attr)
                                allocate(cm%clin(indx)%series(n)%attr(2))
                                do jj = 2, nargs
                                    call parse_split_attr(args(jj), key, mark, attr)
                                    call value(mark, nn, ios)
                                    if (n == nn) then
                                        select case (key)
                                            case ('per1')
                                                cm%clin(indx)%series(n)%attr(1) = attr
                                            case ('per2')
                                                cm%clin(indx)%series(n)%attr(2) = attr
                                        end select
                                    end if
                                end do
!print *, 'n: ', n
!print *, 'attrtype: ', trim(cm%clin(indx)%series(n)%attrtype)
!print *, 'per1: ', trim(cm%clin(indx)%series(n)%attr(1))
!print *, 'per2: ', trim(cm%clin(indx)%series(n)%attr(2))
                            end if
                    end select
                end do
            end if
        end do

    end subroutine

    subroutine parse_seek_section(un, name, lines, nlines, preserveCasing)

        use strings

        integer, intent(in) :: un
        character(len=*), intent(in) :: name
        character*4096, intent(out), dimension(:), allocatable :: lines
        integer, intent(out) :: nlines
        logical, intent(in), optional :: preserveCasing

        integer :: ios = 0
        integer i, posln_end, posln_start
        character(len=len_trim(name)) lname
        character*4096 ln_in
        logical :: lpreserveCasing = .false.

        !> Remove casing from the name.
        lname = lowercase(name)
        if (present(preserveCasing)) lpreserveCasing = preserveCasing

        !> Rewind the file.
        rewind(un)

        !> Identify the end of the section using the header.
        posln_end = 1
        do while (ios == 0)
            call readline(un, ln_in, ios)
!            if (ios /= 0) then
!todo: Fix this.
!print *, 'error: end of file before end token', lname
!stop
!            end if
            if (lowercase(ln_in) == ':end' // lname) exit
            posln_end = posln_end + 1
        end do

        !> Rewind the file.
        rewind(un)

        !> Identify the start of the section using the header.
        posln_start = 1
        do while (ios == 0)
            call readline(un, ln_in, ios)
!            if (ios /= 0) then
!todo: Fix this.
!print *, 'error: end of file before start token', lname
!stop
!            end if
            if (lowercase(ln_in) == ':start' // lname) then
                nlines = posln_end - posln_start + 1
                allocate(lines(nlines))
                if (.not. lpreserveCasing) ln_in = lowercase(ln_in)
                lines(1) = ln_in
                do i = 2, nlines
                    call readline(un, ln_in, ios)
                    if (.not. lpreserveCasing) ln_in = lowercase(ln_in)
                    lines(i) = ln_in
                end do
                exit
            end if
            posln_start = posln_start + 1
        end do

!todo: Account for end of line, missing section headers, no section lines.

    end subroutine

    subroutine parse_split_fields(line, delim, args, nargs)

        use strings

        character(len=*), intent(in) :: line
        character, intent(in) :: delim
        character(len=len(line)), dimension(:), allocatable, intent(out) :: args
        integer, intent(out) :: nargs

        integer i
        character(len=len_trim(adjustl(line))) lline

        lline = adjustl(line)
        nargs = 0
        do i = 1, len(lline)
            if (lline(i:i) == delim) nargs = nargs + 1
        end do

        allocate(args(nargs + 1))
        call parse(line, delim, args, nargs)

        do i = 1, nargs
            call removesp(args(i))
        end do

    end subroutine

    subroutine parse_split_attr(arg, key, mark, attr)

        use strings

        character(len=*), intent(in) :: arg
        character*256, intent(out) :: key, mark, attr

        character(len=len(arg)) :: llead, ltail

        llead = arg
        call split(llead, '=', ltail)
        attr = llead
        llead = ltail
        call split(llead, ':', ltail)
        key = ltail
        mark = llead

    end subroutine

end module
