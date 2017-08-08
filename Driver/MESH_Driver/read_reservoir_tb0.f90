!>
!> Description:
!>  Subroutine to read reservoir outlet information from
!>  MESH_input_reservoir.tb0. Adapted from read_resv_ef.f.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: './MESH_input_reservoir.tb0').
!>
subroutine read_reservoir_tb0(shd, iun, fname)

    use strings
    use mpi_module
    use model_dates
    use sa_mesh_shared_variables
    use EF_Module

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd
    integer :: iun
    character(len = *) :: fname

    !> EnSim variables.
    type(ResvParam) :: header
    type(ResvColumnMetaData) :: colHeader
    character(len = 10000) in_line, subString
    character(len = 128) keyword
    integer KeyLen, wordCount
    logical foundEndHeader, insideColMetaData

    !> Local variables.
    integer NR, l, ierr

    !> Open the file.
    if (ro%VERBOSEMODE > 0) print 1000, trim(adjustl(fname))
    open(iun, file = fname, status = 'old', action = 'read', err = 997)

    !> Read and parse the header.
    call InitResvParam(header)
    foundEndHeader = .false.
    insideColMetaData = .false.
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
            else if (keyword(1:KeyLen) == ':columnmetadata') then
                insideColMetaData = .true.
            else if (keyword(1:KeyLen) == ':endcolumnmetadata') then
                insideColMetaData = .false.
            else if (insideColMetaData) then
                ierr = ParseResvColumnMetaData(colHeader, keyword, keyLen, subString)
                if (ierr < 0) then
                    if (ipid == 0) print "(1x, 'ERROR: Error parsing an attribute of the header in ', (a), ':'/, (a))", &
                        trim(adjustl(fname)), trim(adjustl(in_line))
                    stop
                end if
            else
                ierr = ParseResvParam(header, keyword, keyLen, subString)
                if (ierr < 0) then
                    if (ipid == 0) print "(1x, 'ERROR: Error parsing an attribute of the header in ', (a), ':'/, (a))", &
                        trim(adjustl(fname)), trim(adjustl(in_line))
                    stop
                else if (keyword(1:KeyLen) == ':starttime') then

                    !> Start date of measured/observed values.
                    call value(subString(1:4), fms%rsvr%qorls%iyear, ierr)
                    call value(subString(6:7), fms%rsvr%qorls%imonth, ierr)
                    call value(subString(9:10), fms%rsvr%qorls%iday, ierr)
                    fms%rsvr%qorls%ijday = get_jday(fms%rsvr%qorls%imonth, fms%rsvr%qorls%iday, fms%rsvr%qorls%iyear)
                    call value(subString(12:13), fms%rsvr%qorls%ihour, ierr)
                else if (ierr == 0) then
                    if (ro%DIAGNOSEMODE > 0 .and. ro%VERBOSEMODE > 0) then
                        print "(1x, 'WARNING: Unrecognized attribute of header in ', (a), ': ', (a))", &
                        trim(adjustl(fname)), trim(adjustl(keyword))
                    end if
                end if
            end if
        end if
    end do

    !> Return if no gauge locations are defined.
    fms%rsvr%n = colHeader%tb0cmd%colCount
    NR = fms%rsvr%n

    !> Return if there are no reservoirs.
    if (NR == 0) return

    !> Reservoir attributes pulled from 'fms':
    !*  rsvr: Reservoir outlet structure
    !*  -   n: Number of elements dimensioned.
    !*  -   name(n): ID printed to output files.
    !*  -   y(n): Y-coordinate of outlet location.
    !*  -   x(n): X-coordinate of outlet location.
    !*  -   iy(n): Vertical index of the grid-cell containing the location.
    !*  -   jx(n): Horizontal index of the grid-cell containing the location.
    !*  -   n(n): Rank or index of the grid-cell containing the location.
    !*  -   cfn(n): Type of release curve function.
    !*  -   b(n, :): Release curve coefficients.

    !> Allocate configuration variables for the driver.
    call allocate_reservoir_outlet_location(fms%rsvr, NR, ierr)
    if (ierr /= 0) goto 998

    !> Transfer values from the tb0 header, including ones which may optionally be omitted from the file.
    fms%rsvr%qorls%dts = header%tb0p%deltaT
    if (allocated(colHeader%tb0cmd%colName)) fms%rsvr%meta%name = colHeader%tb0cmd%colName
    fms%rsvr%meta%y = colHeader%tb0cmd%colLocY
    fms%rsvr%meta%x = colHeader%tb0cmd%colLocX
    if (allocated(colHeader%colCoeff1)) fms%rsvr%rls%b1 = colHeader%colCoeff1
    if (allocated(colHeader%colCoeff2)) fms%rsvr%rls%b2 = colHeader%colCoeff2
    if (allocated(colHeader%colCoeff3)) fms%rsvr%rls%b3 = colHeader%colCoeff3
    if (allocated(colHeader%colCoeff4)) fms%rsvr%rls%b4 = colHeader%colCoeff4
    if (allocated(colHeader%colCoeff5)) fms%rsvr%rls%b5 = colHeader%colCoeff5
    if (allocated(colHeader%colCoeff6)) fms%rsvr%rls%area = colHeader%colCoeff6
    if (allocated(colHeader%colCoeff7)) fms%rsvr%rls%lvlz0 = colHeader%colCoeff7

    !> Position the file to the first record.
    rewind iun
    call GoToStartOfData(iun)

    return

    !> File errors.
997 if (ipid == 0) print "(1x, 'ERROR: ', (a), ' may not exist.')", trim(adjustl(fname))
998 if (ipid == 0) print "(3x, 'ERROR allocating values based on ', (a), '.')", trim(adjustl(fname))
999 if (ipid == 0) print "(3x, 'ERROR reading from ', (a), '.')", trim(adjustl(fname))

    stop

1000    format(1x, 'READING: ', (a))

end subroutine
