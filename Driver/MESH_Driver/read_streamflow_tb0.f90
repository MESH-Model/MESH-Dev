!>
!> Description:
!>  Subroutine to read streamflow gauge information from
!>  MESH_input_streamflow.tb0. Adapted from read_flow_ef.f.
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!*  iun: Unit of the input file (default: 100).
!*  fname: Full path to the file (default: './MESH_input_parameters.r2c').
!>
subroutine read_streamflow_tb0(shd, iun, fname)

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
    type(FlowParam) :: header
    type(FlowColumnMetaData) :: colHeader
    character(len = 10000) in_line, subString
    character(len = 128) keyword
    integer KeyLen, wordCount
    logical foundEndHeader, insideColMetaData

    !> Local variables.
    integer NS, l, ierr

    !> Open the file.
    if (ro%VERBOSEMODE > 0) print 1000, fname
    open(iun, file = fname, status = 'old', action = 'read', err = 997)

    !> Read and parse the header.
    call InitFlowParam(header)
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
                ierr = ParseFlowColumnMetaData(colHeader, keyword, keyLen, subString)
                if (ierr < 0) then
                    if (ipid == 0) print "(1x, 'ERROR: Error parsing an attribute of the header in ', (a), ':'/, (a))", &
                        trim(adjustl(fname)), trim(adjustl(in_line))
                    stop
                end if
            else
                ierr = ParseFlowParam(header, keyword, keyLen, subString)
                if (ierr < 0) then
                    if (ipid == 0) print "(1x, 'ERROR: Error parsing an attribute of the header in ', (a), ':'/, (a))", &
                        trim(adjustl(fname)), trim(adjustl(in_line))
                    stop
                else if (keyword(1:KeyLen) == ':starttime') then

                    !> Start date of measured/observed values.
                    call value(subString(1:4), fms%stmg%qomeas%iyear, ierr)
                    call value(subString(6:7), fms%stmg%qomeas%imonth, ierr)
                    call value(subString(9:10), fms%stmg%qomeas%iday, ierr)
                    fms%stmg%qomeas%ijday = get_jday(fms%stmg%qomeas%imonth, fms%stmg%qomeas%iday, fms%stmg%qomeas%iyear)
                    call value(subString(12:13), fms%stmg%qomeas%ihour, ierr)
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
    fms%stmg%n = colHeader%tb0cmd%colCount
    NS = fms%stmg%n
    if (NS == 0) return

    !> Streamflow attributes pulled from 'fms':
    !*  stmg: Streamflow gauge structure
    !*  -   n: Number of elements dimensioned.
    !*  -   name(n): ID printed to output files.
    !*  -   y(n): Y-coordinate of outlet location.
    !*  -   x(n): X-coordinate of outlet location.
    !*  -   iy(n): Vertical index of the grid-cell containing the location.
    !*  -   jx(n): Horizontal index of the grid-cell containing the location.
    !*  -   n(n): Rank or index of the grid-cell containing the location.
    !*  -   DA(n): Drainage area.

    !> Allocate and assign attributes for the driver.
    allocate(fms%stmg%name(NS), &
             fms%stmg%y(NS), fms%stmg%x(NS), &
             fms%stmg%iy(NS), fms%stmg%jx(NS), fms%stmg%rnk(NS), &
             fms%stmg%DA(NS), stat = ierr)
    if (ierr /= 0) goto 998
    fms%stmg%qomeas%dts = header%tb0p%deltaT
    fms%stmg%name = colHeader%tb0cmd%colName
    fms%stmg%y = colHeader%tb0cmd%colLocY
    fms%stmg%x = colHeader%tb0cmd%colLocX

    !> Position the file to the first record.
    rewind iun
    call GoToStartOfData(iun)

    return

    !> File errors.
997 if (ipid == 0) print "(1x, 'ERROR: ', (a), ' may not exist.')", fname
998 if (ipid == 0) print "(3x, 'ERROR allocating values based on ', (a), '.')", fname
999 if (ipid == 0) print "(3x, 'ERROR reading from ', (a), '.')", fname

    stop

1000    format(1x, 'READING: ', (a))

end subroutine
