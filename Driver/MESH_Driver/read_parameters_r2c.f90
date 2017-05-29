!>
!> Description:
!>  Subroutine to read parameters from file, in r2c format. Parameter
!>  values are saved directly to the shared parameter object at the
!>  grid level, accessible by sa_mesh_shared_variables.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
!>
subroutine read_parameters_r2c(shd, iun, fname)

    use strings
    use mpi_module
    use sa_mesh_shared_variables
    use EF_Module

    use rte_module
    use PBSM_module

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer :: iun
    character(len = *) :: fname

    !> Local variables.
    integer ierr

    !> Local variables copied and modified from read_shed_ef.f.
    type(ShedParam) :: header
    character(len = 1000) line, subString
    character(len = 128) keyword
    integer keyLen, wordCount
    logical foundEndHeader
    integer n, l, i

    !> Initialization copied from read_shed_ef.f.
    call InitShedParam(header)
    foundEndHeader = .false.

    !> Open the file and read the header.
    open(iun, file = trim(adjustl(fname)), status = 'old', iostat = ierr)
    if (ierr /= 0) then
        if (ipid == 0) then
            print "(/1x, 'ERROR: Error opening the r2c parameter file: ', (a))", trim(adjustl(fname))
            print "(3x, 'Check that the file exists or use an alternate parameter file form.')"
        end if
        stop
    end if

    !> Copied and modified from read_shed_ef.f.
    line(1:1) = '#'
    do while ((.not. foundEndHeader) .and. ((line(1:1) == '#') .or. (line(1:1) == ':') .or. (len_trim(line) == 0)))

        !> Read the line.
        read(iun, "((a))", iostat = ierr) line
        if (ierr == -1) then
            if (ipid == 0) print "(/1x, 'ERROR: Premature end of file (EOF) encountered in: ', (a))", trim(adjustl(fname))
            stop
        end if

        !> Replace tabs with spaces, get rid of leading whitespace, and determine length of words in the line
        call compact(line)
        line = adjustl(line)

        !> Parse attributes (lines that start with ':').
        if (line(1:1) == ':') then
            wordCount = SplitLine(line, keyword, subString)
            keyword = lowercase(keyword)
            KeyLen = len_trim(keyword)

            !> ':endheader' is the end of the header, otherwise this is an attribute.
            if (keyword(1:KeyLen) == ':endheader') then
                foundEndHeader = .true.
            else
                ierr = ParseShedParam(header, keyword, keyLen, subString)
                if (ierr < 0) then
                    if (ipid == 0) print "(1x, 'ERROR: Error parsing an attribute of the header in ', (a), ':'/, (a))", &
                        trim(adjustl(fname)), trim(adjustl(line))
                    stop
                else if (ierr == 0) then
                    if (ipid == 0) print "(1x, 'WARNING: Unrecognized attribute of header in ', (a), ': ', (a))", &
                        trim(adjustl(fname)), trim(adjustl(keyword))
                end if
            end if
        end if
    end do

    !> Compare grid definiton to that from MESH_drainage_database.r2c.
    if (header%r2cp%xCount /= shd%xCount .or. header%r2cp%yCount /= shd%yCount .or. &
        header%r2cp%xOrigin /= shd%xOrigin .or. header%r2cp%yOrigin /= shd%yOrigin .or. &
        header%r2cp%xDelta /= shd%xDelta .or. header%r2cp%yDelta /= shd%yDelta) then
        if (ipid == 0) then
            print "(/1x, 'ERROR: Mismatch between ', (a), ' and MESH_drainage_database.r2c')", trim(adjustl(fname))
            print 1110, repeat('-', 16), repeat('-', 16), repeat ('-', 16)
            print 1110, 'ATTRIBUTE', 'INPUT FILE', 'DRAINAGE DB'
            print 1110, repeat('-', 16), repeat('-', 16), repeat ('-', 16)
            print 1110, 'xCount', header%r2cp%xCount, shd%xCount
            print 1110, 'yCount', header%r2cp%yCount, shd%yCount
            print 1110, 'xOrigin', header%r2cp%xOrigin, shd%xOrigin
            print 1110, 'yOrigin', header%r2cp%yOrigin, shd%yOrigin
            print 1110, 'xDelta', header%r2cp%xDelta, shd%xDelta
            print 1110, 'yDelta', header%r2cp%yDelta, shd%yDelta
        end if
        stop
    end if

    !> Read the attribute data.
    call LoadAttributeData(header%r2cp%ep, shd%xCount, shd%yCount, iun)

    !> Distribute the data to the grid array (GRD) by RANK.
    do n = 1, shd%NAA

        !> i is the index of the field in the r2c grid, by priority of row.
        i = (shd%yyy(n) - 1)*shd%xCount + shd%xxx(n)
        do l = 1, header%r2cp%ep%attCount
            select case (lowercase(header%r2cp%ep%attList(l)%name))

                !> SA_MESH parameters.
!-                case ('rank')
!-                    print "(' RANK ', i8, ' AttList%Val ', i8)", n, int(header%r2cp%ep%attList(l)%val(i))

                !> RPN RTE (Watflood, 2007).
                case ('r1n')
                    if (rteflg%PROCESS_ACTIVE) rtepm%r1n(n) = header%r2cp%ep%attList(l)%val(i)
                case ('r2n')
                    if (rteflg%PROCESS_ACTIVE) rtepm%r2n(n) = header%r2cp%ep%attList(l)%val(i)
                case ('mndr')
                    if (rteflg%PROCESS_ACTIVE) rtepm%mndr(n) = header%r2cp%ep%attList(l)%val(i)
                case ('widep')
                    if (rteflg%PROCESS_ACTIVE) rtepm%widep(n) = header%r2cp%ep%attList(l)%val(i)
                case ('flz')
                    if (rteflg%PROCESS_ACTIVE) rtepm%flz(n) = header%r2cp%ep%attList(l)%val(i)
                case ('pwr')
                    if (rteflg%PROCESS_ACTIVE) rtepm%pwr(n) = header%r2cp%ep%attList(l)%val(i)
                case ('aa2')
                    if (rteflg%PROCESS_ACTIVE) rtepm%aa2(n) = header%r2cp%ep%attList(l)%val(i)
                case ('aa3')
                    if (rteflg%PROCESS_ACTIVE) rtepm%aa3(n) = header%r2cp%ep%attList(l)%val(i)
                case ('aa4')
                    if (rteflg%PROCESS_ACTIVE) rtepm%aa4(n) = header%r2cp%ep%attList(l)%val(i)
!                case ('theta')
!                case ('kcond')

                !> PBSM (blowing snow).
                case ('fetch')
                    if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%fetch(n) = header%r2cp%ep%attList(l)%val(i)
                case ('ht')
                    if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%Ht(n) = header%r2cp%ep%attList(l)%val(i)
                case ('n_s')
                    if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%N_S(n) = header%r2cp%ep%attList(l)%val(i)
                case ('a_s')
                    if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%A_S(n) = header%r2cp%ep%attList(l)%val(i)
                case ('distrib')
                    if (pbsm%PROCESS_ACTIVE) pbsm%pm_grid%Distrib(n) = header%r2cp%ep%attList(l)%val(i)

                !> Unrecognized name.
                case default

                    !> Only print the message when RANK = 1 (not multiple times in the loop).
                    if (ipid == 0 .and. n == 1) print "(1x, 'WARNING: Unrecognized attribute in ', (a), ': ', (a))", &
                        trim(adjustl(fname)), trim(adjustl(header%r2cp%ep%attList(l)%name))

            end select
        end do
    end do

    !> Close the file to free the unit.
    close(iun)

1110    format(3x, g16.9, 1x, g16.9)

end subroutine
