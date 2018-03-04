!> Description:
!>  Subroutine to read basin properties from a single-frame 'r2c' format
!>  file. Values are parsed by order of RANK and stored in variables
!>  that must be allocated 1:NA.
!>
!> Input variables:
!*  iun: Unit of the input file.
!*  fname: Full path to the file (default: './MESH_drainage_database.r2c').
!>
!> Input/output variables:
!*  shd: Basin 'shed' object (properties).
subroutine read_shed_r2c(shd, iun, fname)

    !> strings: For 'lowercase' function.
    !> sa_mesh_variables: Required for MESH variables, parameters.
    !> sa_mesh_utilities: Required for printing/writing messages, VERBOSEMODE, DIAGNOSEMODE.
    !> ensim_io: Required for read 'r2c' format file.
    use strings
    use sa_mesh_variables
    use sa_mesh_utilities
    use ensim_io

    implicit none

    !> Input variables.
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Input/output variables.
    type(ShedGridParams) shd

    !> Local variables.
    type(ensim_keyword), dimension(:), allocatable :: vkeyword
    type(ensim_attr), dimension(:), allocatable :: vattr
    integer nkeyword, nattr, l, x, y, ierr
    real, dimension(:, :), allocatable :: dat
    real, dimension(:), allocatable :: ffield
    character(len = DEFAULT_LINE_LENGTH) line

    !> Open the file and read the header.
    call open_ensim_file(iun, fname, ierr, VERBOSEMODE)
    call parse_header_ensim(iun, fname, vkeyword, nkeyword, ierr)

    !> Get keywords.
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':projection', shd%CoordSys%Proj, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':ellipsoid', shd%CoordSys%Ellips, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':zone', shd%CoordSys%Zone, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':xcount', shd%xCount, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':xorigin', shd%xOrigin, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':xdelta', shd%xDelta, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':ycount', shd%yCount, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':yorigin', shd%yOrigin, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':ydelta', shd%yDelta, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':nominalgridsize_AL', shd%AL, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':totalnumofgrids', shd%NA, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':numgridsinbasin', shd%NAA, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':numriverclasses', shd%NRVR, ierr, VERBOSEMODE)
    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':classcount', shd%lc%NTYPE, ierr, VERBOSEMODE)
!+    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':debuggridno', l, ierr, VERBOSEMODE)

    !> Adjust GRU count (exclude impervious).
    shd%lc%NTYPE = shd%lc%NTYPE - 1

    !> Calculate grid statistics.
    select case (lowercase(shd%CoordSys%Proj))
        case ('latlong')
            shd%iyMin = int(shd%yOrigin*60.0)
            shd%iyMax = int((shd%yOrigin + shd%yCount*shd%yDelta)*60.0)
            shd%jxMin = int(shd%xOrigin*60.0)
            shd%jxMax = int((shd%xOrigin + shd%xCount*shd%xDelta)*60.0)
            shd%GRDE = shd%xDelta*60.0
            shd%GRDN = shd%yDelta*60.0
        case ('utm')
            shd%GRDE = shd%xDelta/1000.0
            shd%GRDN = shd%yDelta/1000.0
            shd%jxMin = int(shd%xOrigin/1000.0)
            shd%jxMax = shd%jxMin + shd%GRDE*(shd%xCount - 1)
            shd%iyMin = int(shd%yOrigin/1000.0)
            shd%iyMax = shd%iyMin + shd%GRDN*(shd%yCount - 1)
        case default
            call print_error('Unsupported coordinate system: ' // trim(shd%CoordSys%Proj))
            call stop_program()
    end select

    !> Check grid dimension.
    if (shd%NA < 1 .or. shd%NAA < 1) then
        call print_error('Bad grid configuration. At least one grid must exist inside the basin.')
        call print_message('The number of outlets should be at least 1 greater than the number of grids inside the basin.')
        write(line, 1001) shd%NA
        call print_message_detail('Number of grids (from file): ' // trim(line))
        write(line, 1001) (shd%NAA - shd%NA)
        call print_message_detail('Number of outlets (from file): ' // trim(line))
        call stop_program()
    end if

    !> Allocate and initialize variables.
    allocate( &
        shd%RNKGRD(shd%yCount, shd%xCount), shd%xxx(shd%NA), shd%yyy(shd%NA), &
        shd%IROUGH(shd%NA), &
        shd%lc%ACLASS(shd%NA, shd%lc%NTYPE + 1), stat = ierr)
    if (ierr /= 0) then
        write(line, 1001) ierr
        call print_error('Unable to allocate grid variables (error code: ' // trim(line) // ').')
        call stop_program()
    end if
    shd%RNKGRD = 0; shd%xxx = 0; shd%yyy = 0
    shd%IROUGH = 0
    shd%lc%ACLASS = 0.0

    !> Get the list of attributes.
    call parse_header_attribute_ensim(iun, fname, vkeyword, nkeyword, vattr, nattr, ierr)

    !> Read and parse the attribute data.
    call load_data_r2c(iun, fname, vattr, nattr, shd%xCount, shd%yCount, .false., ierr)

    !> Find and assign 'RANK'.
    do l = 1, nattr
        if (lowercase(vattr(l)%attr) == 'rank') then
            shd%RNKGRD = transpose(vattr(l)%val)
            exit
        end if
    end do

    !> Create the 'xxx' and 'yyy' reference tables.
    do x = 1, shd%xCount
        do y = 1, shd%yCount
            if (shd%RNKGRD(y, x) > 0) then
                shd%xxx(shd%RNKGRD(y, x)) = x
                shd%yyy(shd%RNKGRD(y, x)) = y
            end if
        end do
    end do

    !> Allocate and initialize variables.
    allocate( &
        shd%NEXT(shd%NA), &
        shd%IAK(shd%NA), shd%SLOPE_CHNL(shd%NA), shd%CHNL_LEN(shd%NA), shd%ICHNL(shd%NA), shd%IREACH(shd%NA), &
        shd%DA(shd%NA), shd%AREA(shd%NA), shd%FRAC(shd%NA), &
        shd%BNKFLL(shd%NA), &
        shd%ELEV(shd%NA), shd%SLOPE_INT(shd%NA), &
        shd%DRDN(shd%NA), &
        stat = ierr)
    if (ierr /= 0) then
        write(line, 1001) ierr
        call print_error('Unable to allocate grid variables (error code: ' // trim(line) // ').')
        call stop_program()
    end if
    shd%NEXT = 0
    shd%IAK = 0; shd%SLOPE_CHNL = 0.0; shd%CHNL_LEN = 0.0; shd%ICHNL = 0; shd%IREACH = 0
    shd%DA = 0.0; shd%AREA = 0.0; shd%FRAC = 0.0
    shd%BNKFLL = 0.0
    shd%ELEV = 0.0; shd%SLOPE_INT = 0.0
    shd%DRDN = 0.0

    !> Scan and assign remaining variables.
    !> Cycle to where land cover (GRU) classes are expected.
    do l = 1, (nattr - (shd%lc%NTYPE + 1))

        !> Assign the data to a vector.
        call r2c_to_rank(iun, fname, vattr, nattr, l, shd%xxx, shd%yyy, shd%NA, ffield, shd%NA, VERBOSEMODE)
        if (.not. allocated(ffield)) then
            call print_error("Could not find attribute 'RANK' in the file.")
            call stop_program()
        end if

        !> Determine and assign to the variable.
        select case (lowercase(vattr(l)%attr))
            case ('next')
                shd%NEXT = int(ffield)
            case ('iak')
                shd%IAK = int(ffield)
            case ('chnlslope')
                shd%SLOPE_CHNL = ffield
            case ('chnllength')
                shd%CHNL_LEN = ffield
            case ('chnl')
                shd%ICHNL = int(ffield)
            case ('reach')
                shd%IREACH = int(ffield)
            case ('da')
                shd%DA = ffield
            case ('gridarea')
                shd%AREA = ffield
                shd%FRAC = ffield/shd%AL/shd%AL
            case ('bankfull')
                shd%BNKFLL = ffield
            case ('elev')
                shd%ELEV = ffield
!?            case ('intslope')
            case ('demslope')
                shd%SLOPE_INT = ffield
            case ('drdn')
                shd%DRDN = ffield

                !> Convert DD from km/km^2 to m/m^2; WATROF expects m/m^2.
!todo: At some point these units were keyed to the first grid square; whether or not it's -1.0 (or maybe 1.0)
                shd%DRDN = ffield/1000.0
        end select
    end do

    !> Scan and assign land cover (GRU) classes.
    do l = (nattr - shd%lc%NTYPE), nattr

        !> Assign the data to a vector.
        call r2c_to_rank(iun, fname, vattr, nattr, l, shd%xxx, shd%yyy, shd%NA, ffield, shd%NA, VERBOSEMODE)
        if (.not. allocated(ffield)) then
            write(line, 1001) l
            call print_warning('Unable to read Attribute ' // trim(adjustl(line)) // '.')
            cycle
        end if

        !> Assign to the variable.
        shd%lc%ACLASS(:, l - (nattr - shd%lc%NTYPE) + 1) = ffield
    end do

    !> Close the file to free the unit.
    close(iun)

    !> Format statements.
1001    format(9999(g15.6, 1x))

end subroutine
