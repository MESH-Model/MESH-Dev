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

    use RUNCLASS36_variables
    use RUNSVS113_variables
    use rte_module
    use PBSM_module

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer :: iun
    character(len = *) :: fname

    !> EnSim variables.
    type(ShedParam) :: header
    character(len = 1000) in_line, subString
    character(len = 128) keyword
    integer keyLen, wordCount
    logical foundEndHeader

    !> Location variables.
    integer ierr, n, l, i

    !> Open the file and read the header.
    if (ro%VERBOSEMODE > 0) print 1000, fname
    open(iun, file = trim(adjustl(fname)), status = 'old', iostat = ierr)
    if (ierr /= 0) then
        if (ipid == 0) then
            print "(/1x, 'ERROR: Error opening the r2c parameter file: ', (a))", trim(adjustl(fname))
            print "(3x, 'Check that the file exists or use an alternate parameter file form.')"
        end if
        stop
    end if

    !> Read and parse the header.
    call InitShedParam(header)
    foundEndHeader = .false.
    in_line(1:1) = '#'
    do while ((.not. foundEndHeader) .and. ((in_line(1:1) == '#') .or. (in_line(1:1) == ':') .or. (len_trim(in_line) == 0)))

        !> Read the line.
        read(iun, "((a))", iostat = ierr) in_line
        if (ierr == -1) then
            if (ipid == 0) print "(/1x, 'ERROR: Premature end of file (EOF) encountered in: ', (a))", trim(adjustl(fname))
            stop
        end if

        !> Replace tabs with spaces and get rid of leading whitespace.
        call compact(in_line)
        in_line = adjustl(in_line)

        !> Parse attributes (lines that start with ':').
        if (in_line(1:1) == ':') then
            wordCount = SplitLine(in_line, keyword, subString)
            keyword = lowercase(keyword)
            KeyLen = len_trim(keyword)

            !> ':endheader' is the end of the header, otherwise this is an attribute.
            if (keyword(1:KeyLen) == ':endheader') then
                foundEndHeader = .true.
            else
                ierr = ParseShedParam(header, keyword, keyLen, subString)
                if (ierr < 0) then
                    if (ipid == 0) print "(1x, 'ERROR: Error parsing an attribute of the header in ', (a), ':'/, (a))", &
                        trim(adjustl(fname)), trim(adjustl(in_line))
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

                !> RUNCLASS36 and RUNSVS113.
                case ('fcan nl')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%fcan(n, 1) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('fcan bl')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%fcan(n, 2) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('fcan cr')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%fcan(n, 3) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('fcan gr')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%fcan(n, 4) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('fcan ur')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%fcan(n, 5) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('lnz0 nl')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%lnz0(n, 1) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('lnz0 bl')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%lnz0(n, 2) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('lnz0 cr')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%lnz0(n, 3) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('lnz0 gr')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%lnz0(n, 4) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('lnz0 ur')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%cp%lnz0(n, 5) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('sdep')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%slp%sdep(n) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('xslp')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%tp%xslp(n) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('dd')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%hp%dd(n) = header%r2cp%ep%attList(l)%val(i)

                        !> Unit conversion if units are km km-2.
                        if (index(lowercase(header%r2cp%ep%attList(l)%units), 'km') > 0) then
                            pm_grid%hp%dd(n) = pm_grid%hp%dd(n)/1000.0
                        end if
                    end if
                case ('sand')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%slp%sand(n, :) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('sand 1')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%slp%sand(n, 1) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('sand 2')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%slp%sand(n, 2) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('sand 3')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%slp%sand(n, 3) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('clay')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%slp%clay(n, :) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('clay 1')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%slp%clay(n, 1) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('clay 2')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%slp%clay(n, 2) = header%r2cp%ep%attList(l)%val(i)
                    end if
                case ('clay 3')
                    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
                        pm_grid%slp%clay(n, 3) = header%r2cp%ep%attList(l)%val(i)
                    end if

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

1000    format(1x, 'READING: ', (a))
1110    format(3x, g16.9, 1x, g16.9)

end subroutine
