!> Description:
!>  Subroutine to read basin properties from a netCDF format
!>  file. Values are parsed by order of RANK and stored in variables
!>  that must be allocated 1:NA.
!>
!> Input variables:
!*  fname: Full path to the file (default: 'MESH_drainage_database.nc').
!*
!> Output variables:
!*  ierr: Return status.
!>
!> Input/output variables:
!*  shd: Basin 'shed' object (properties).
subroutine read_shed_nc(shd, fname, ierr)

    !> 'strings': For 'lowercase' function.
    !> 'sa_mesh_common': For common MESH variables, constants, and routines.
    !> 'nc_io': For routines to read 'r2c' format file.
    use strings
    use sa_mesh_common
    use nc_io

    implicit none

    !> Input variables.
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Input/output variables.
    type(ShedGridParams) shd

    !> Local variables.
    character(len = DEFAULT_LINE_LENGTH) line, units
    character(len = DEFAULT_FIELD_LENGTH) field
    character(len = DEFAULT_FIELD_LENGTH), allocatable :: dat_c(:)
    character(len = 1) fill_c
    real, allocatable :: lon(:), lat(:), dat2_r(:, :), dat3_r(:, :, :)
    real fill_r
    real, parameter :: deg_threshold = 1.0E-4
    integer, allocatable :: dat2_i(:, :)
    integer fill_i, iun, x, y, m, n, i, z

    !> Initialize the return status.
    ierr = 0

    !> Open the file.
    call reset_tab()
    call print_message('READING: ' // trim(fname))
    call increase_tab()
    call nc4_open_input(fname, iun, ierr)
    if (ierr /= 0) return

    !> Get the projection.
    call nc4_get_proj(iun, fname, shd%CoordSys%Proj, shd%CoordSys%Ellips, shd%CoordSys%Zone, z)

    !> Get longitude and latitude and set respective counts.
    call nc4_get_variable_n(iun, fname, 'longitude', lon, units, fill_r, z, name_dim = 'lon')
    if (z /= 0) then
        call print_message("ERROR: An error occurred reading the lontigude 'lon' variable.")
        ierr = z
    else if ( &
        lowercase(units) /= 'degrees' .and. lowercase(units) /= 'decimal_degrees' .and. lowercase(units) /= 'degrees_east') then
        call print_warning( &
            "The units '" // trim(units) // "' of the longitude variable are different from the expected units of " // &
            "'degrees', 'decimal_degrees', or 'degrees_east'.")
    else
        shd%xCount = size(lon)
        shd%xDelta = 0.0
        n = 0
        do i = 2, shd%xCount
            if (i < shd%xCount) then
                if (((lon(i) - lon(i - 1)) - (lon(i + 1) - lon(i))) > deg_threshold) n = n + 1
            end if
            shd%xDelta = shd%xDelta + (lon(i) - lon(i - 1))
        end do
        shd%xDelta = shd%xDelta/(shd%xCount - 1)
        shd%xOrigin = minval(lon) - shd%xDelta/2.0
        if (n > 0) then
            call print_warning( &
                "The spacing of longitudes is not regular inside this file. Conversion or writing outputs to file formats " // &
                "that implicitly use regular grids is not recommended.")
        end if
    end if
    call nc4_get_variable_n(iun, fname, 'latitude', lat, units, fill_r, z, name_dim = 'lat')
    if (z /= 0) then
        call print_message("ERROR: An error occurred reading the latitude 'lat' variable.")
        ierr = z
    else if ( &
        lowercase(units) /= 'degrees' .and. lowercase(units) /= 'decimal_degrees' .and. lowercase(units) /= 'degrees_north') then
        call print_warning( &
            "The units '" // trim(units) // "' of the latitude variable are different from the expected units of " // &
            "'degrees', 'decimal_degrees', or 'degrees_north'.")
    else
        shd%yCount = size(lat)
        shd%yDelta = 0.0
        n = 0
        do i = 2, shd%yCount
            if (i < shd%yCount) then
                if (((lat(i) - lat(i - 1)) - (lat(i + 1) - lat(i))) > deg_threshold) n = n + 1
            end if
            shd%yDelta = shd%yDelta + (lat(i) - lat(i - 1))
        end do
        shd%yDelta = shd%yDelta/(shd%yCount - 1)
        shd%yOrigin = minval(lat) - shd%yDelta/2.0
    end if
    if (ierr /= 0) goto 999

    !> Get GRUs.
    call nc4_get_dimension(iun, fname, 'gru', m, ierr, dim_length = shd%lc%NTYPE)
    if (ierr /= 0) goto 999

    !> Get GRU names.
    call nc4_get_variable_n(iun, fname, 'gru', dat_c, units, fill_c, ierr)
    if (ierr /= 0) then
        call print_warning('Unable to read GRU names.')
    end if

    !> Get 'Rank'.
    allocate( &
        dat2_r(shd%xCount, shd%yCount), dat3_r(shd%xCount, shd%yCount, shd%lc%NTYPE), dat2_i(shd%xCount, shd%yCount), &
        shd%RNKGRD(shd%yCount, shd%xCount), &
        stat = ierr)
    if (ierr /= 0) then
        write(line, FMT_GEN) ierr
        call print_error("Unable to allocate the 'RNKGRD' variable (error code: " // trim(adjustl(line)) // ").")
        goto 999
    end if
    shd%RNKGRD = 0
    call nc4_get_variable_xy(iun, fname, 'Rank', dat2_i, units, fill_i, ierr)
    if (ierr /= 0) goto 999
    shd%RNKGRD = transpose(dat2_i)
    if (all(shd%RNKGRD == 0) .or. all(shd%RNKGRD == fill_i)) then
        write(field, FMT_GEN) fill_i
        call print_error("All values in the 'RANK' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
        ierr = 1
        goto 999
    end if

    !> Determine 'NA'.
    shd%NA = count(shd%RNKGRD /= fill_i .or. shd%RNKGRD > 0)

    !> Create the 'xxx' and 'yyy' reference tables.
    allocate(shd%xxx(shd%NA), shd%yyy(shd%NA), stat = ierr)
    if (ierr == 0) then
        do x = 1, shd%xCount
            do y = 1, shd%yCount
                if (shd%RNKGRD(y, x) > 0) then
                    shd%xxx(shd%RNKGRD(y, x)) = x
                    shd%yyy(shd%RNKGRD(y, x)) = y
                end if
            end do
        end do
    else
        call print_error("Unable to allocate 'xxx' and 'yyy' variables (error code: " // trim(adjustl(line)) // ").")
        goto 999
    end if

    !> Map coordinates.
    allocate(shd%xlng(shd%NA), shd%ylat(shd%NA), stat = ierr)
    if (ierr == 0) then
        do n = 1, shd%NA
            shd%xlng(n) = lon(shd%xxx(n))
            shd%ylat(n) = lat(shd%yyy(n))
        end do
    else
        call print_error("Unable to allocate 'xlng' and 'ylat' variables (error code: " // trim(adjustl(line)) // ").")
        goto 999
    end if

    !> Allocate and initialize variables.
    allocate( &
        shd%NEXT(shd%NA), &
        shd%IAK(shd%NA), shd%SLOPE_CHNL(shd%NA), shd%CHNL_LEN(shd%NA), shd%ICHNL(shd%NA), shd%IREACH(shd%NA), &
        shd%IROUGH(shd%NA), &
        shd%DA(shd%NA), shd%AREA(shd%NA), shd%FRAC(shd%NA), &
        shd%BNKFLL(shd%NA), &
        shd%ELEV(shd%NA), shd%SLOPE_INT(shd%NA), &
        shd%DRDN(shd%NA), &
        stat = ierr)
    if (ierr /= 0) then
        write(line, FMT_GEN) ierr
        call print_error("Unable to allocate the 'basin attribute' variables (error code: " // trim(adjustl(line)) // ").")
        goto 999
    end if
    shd%NEXT = 0
    shd%IAK = 0; shd%SLOPE_CHNL = 0.0; shd%CHNL_LEN = 0.0; shd%ICHNL = 0; shd%IREACH = 0
    shd%IROUGH = 0
    shd%DA = 0.0; shd%AREA = 0.0; shd%FRAC = 0.0
    shd%BNKFLL = 0.0
    shd%ELEV = 0.0; shd%SLOPE_INT = 0.0
    shd%DRDN = 0.0

    !> Get 'Next'.
    shd%NEXT = 0
    call nc4_get_variable_xy(iun, fname, 'Next', dat2_i, units, fill_i, ierr)
    if (ierr /= 0) goto 999
    if (all(dat2_i == 0) .or. all(dat2_i == fill_i)) then
        write(field, FMT_GEN) fill_i
        call print_error("All values in the 'NEXT' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
    else
        do n = 1, shd%NA
            shd%NEXT(n) = dat2_i(shd%xxx(n), shd%yyy(n))
        end do
    end if

    !> Determine 'NAA'.
    shd%NAA = count(shd%NEXT /= 0)

!?    call get_keyword_value(iun, vkeyword, nkeyword, ':NominalGridSize_AL', shd%AL, z); if (z /= 0) ierr = z
!+    call get_keyword_value(iun, fname, vkeyword, nkeyword, ':DebugGridNo', l, z); if (z /= 0) ierr = z

    !> Adjust GRU count (exclude impervious).
    shd%lc%NTYPE = shd%lc%NTYPE - 1

    !> Calculate additional grid attributes.
    select case (lowercase(shd%CoordSys%Proj))
        case ('latlong')
            shd%iyMin = int(shd%yOrigin*60.0)
            shd%iyMax = int((shd%yOrigin + shd%yCount*shd%yDelta)*60.0)
            shd%jxMin = int(shd%xOrigin*60.0)
            shd%jxMax = int((shd%xOrigin + shd%xCount*shd%xDelta)*60.0)
            shd%GRDE = shd%xDelta*60.0
            shd%GRDN = shd%yDelta*60.0
!+        case ('utm')
!+            shd%GRDE = shd%xDelta/1000.0
!+            shd%GRDN = shd%yDelta/1000.0
!+            shd%jxMin = int(shd%xOrigin/1000.0)
!+            shd%jxMax = shd%jxMin + shd%GRDE*(shd%xCount - 1)
!+            shd%iyMin = int(shd%yOrigin/1000.0)
!+            shd%iyMax = shd%iyMin + shd%GRDN*(shd%yCount - 1)
        case default
            call print_error('Unsupported coordinate system: ' // trim(shd%CoordSys%Proj))
            ierr = 1
            goto 999
    end select

    !> Check grid dimension.
    if (shd%NA < 1 .or. shd%NAA < 1) then
        call print_error('No grids are defined inside the basin.')
        write(line, FMT_GEN) shd%NA
        call print_message('Number of grids read from file: ' // trim(adjustl(line)))
        ierr = 1
        goto 999
    end if
    if (shd%NAA >= shd%NA) then
        call print_warning('No outlets exist in the basin.')
        write(line, FMT_GEN) shd%NA
        call print_message('Total number of grids: ' // trim(adjustl(line)))
        write(line, FMT_GEN) shd%NAA
        call print_message('Total number of grids inside the basin: ' // trim(adjustl(line)))
    end if

    !> Get 'ACLASS'.
    allocate(shd%lc%ACLASS(shd%NA, shd%lc%NTYPE + 1), stat = ierr)
    if (ierr /= 0) then
        write(line, FMT_GEN) ierr
        call print_error("Unable to allocate the 'ACLASS' variables (error code: " // trim(adjustl(line)) // ").")
        ierr = 1
        goto 999
    end if
    shd%lc%ACLASS = 0.0
    call nc4_get_variable_xym(iun, fname, 'LandClass', dat3_r, units, fill_r, ierr)
    if (ierr /= 0) goto 999
    if (all(dat3_r == fill_r)) then
        write(field, FMT_GEN) fill_r
        call print_error("All values in the 'LandClass' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
        ierr = 1
        goto 999
    else
        do m = 1, (shd%lc%NTYPE + 1)
            do n = 1, shd%NA
                shd%lc%ACLASS(n, m) = dat3_r(shd%xxx(n), shd%yyy(n), m)
            end do
        end do
    end if

    !> Get other attributes.
    shd%IAK = 0
    call nc4_get_variable_xy(iun, fname, 'IAK', dat2_i, units, fill_i, z)
    if (z == 0) then
        if (all(dat2_i == fill_i)) then
            write(field, FMT_GEN) fill_i
            call print_error("All values in the 'IAK' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            z = 1
        else
            do n = 1, shd%NA
                shd%IAK(n) = dat2_i(shd%xxx(n), shd%yyy(n))
            end do
        end if
    end if
    if (z /= 0) ierr = 1
    shd%NRVR = maxval(shd%IAK)
    shd%SLOPE_CHNL = 0.0
    call nc4_get_variable_xy(iun, fname, 'ChnlSlope', dat2_r, units, fill_r, z)
    if (z == 0) then
        if (all(dat2_r == fill_r)) then
            write(field, FMT_GEN) fill_r
            call print_error("All values in the 'ChnlSlope' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            z = 1
        else
            do n = 1, shd%NA
                shd%SLOPE_CHNL(n) = dat2_r(shd%xxx(n), shd%yyy(n))
            end do
        end if
    end if
    if (z /= 0) ierr = 1
    shd%CHNL_LEN = 0.0
    call nc4_get_variable_xy(iun, fname, 'ChnlLength', dat2_r, units, fill_r, z)
    if (z == 0) then
        if (all(dat2_r == fill_r)) then
            write(field, FMT_GEN) fill_r
            call print_error("All values in the 'ChnlLength' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            z = 1
        else
            do n = 1, shd%NA
                shd%CHNL_LEN(n) = dat2_r(shd%xxx(n), shd%yyy(n))
            end do
        end if
    end if
    if (z /= 0) ierr = 1
    shd%ICHNL = 0
    call nc4_get_variable_xy(iun, fname, 'Chnl', dat2_i, units, fill_i, z)
    if (z == 0) then
        if (all(dat2_i == fill_i)) then
            write(field, FMT_GEN) fill_i
            call print_error("All values in the 'Chnl' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            z = 1
        else
            do n = 1, shd%NA
                shd%ICHNL(n) = dat2_i(shd%xxx(n), shd%yyy(n))
            end do
        end if
    end if
    if (z /= 0) ierr = 1
    shd%IREACH = 0
    call nc4_get_variable_xy(iun, fname, 'Reach', dat2_i, units, fill_i, z)
    if (z == 0) then
        if (all(dat2_i == fill_i)) then
            write(field, FMT_GEN) fill_i
            call print_error("All values in the 'Reach' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            z = 1
        else
            do n = 1, shd%NA
                shd%IREACH(n) = dat2_i(shd%xxx(n), shd%yyy(n))
            end do
        end if
    end if
    if (z /= 0) ierr = 1
    shd%DA = 0.0
    call nc4_get_variable_xy(iun, fname, 'DA', dat2_r, units, fill_r, z)
    if (z == 0) then
        if (all(dat2_r == fill_r)) then
            write(field, FMT_GEN) fill_r
            call print_error("All values in the 'DA' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            z = 1
        else
            do n = 1, shd%NA
                shd%DA(n) = dat2_r(shd%xxx(n), shd%yyy(n))
            end do
        end if
    end if
    if (z /= 0) ierr = 1
    shd%AREA = 0.0
    call nc4_get_variable_xy(iun, fname, 'GridArea', dat2_r, units, fill_r, z)
    if (z == 0) then
        if (all(dat2_r == fill_r)) then
            write(field, FMT_GEN) fill_r
            call print_error("All values in the 'GridArea' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            z = 1
        else
            do n = 1, shd%NA
                shd%AREA(n) = dat2_r(shd%xxx(n), shd%yyy(n))
            end do
        end if
    end if
    if (z /= 0) ierr = 1
    if (shd%AL == 0) shd%AL = 1.0
    shd%FRAC = shd%AREA/shd%AL/shd%AL
    shd%BNKFLL = 0.0
    call nc4_get_variable_xy(iun, fname, 'Bankfull', dat2_r, units, fill_r, z)
    if (z == 0) then
        if (all(dat2_r == fill_r)) then
            write(field, FMT_GEN) fill_r
            call print_error("All values in the 'Bankfull' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            z = 1
        else
            do n = 1, shd%NA
                shd%BNKFLL(n) = dat2_r(shd%xxx(n), shd%yyy(n))
            end do
        end if
    end if
    if (z /= 0) ierr = 1
    shd%ELEV = 0.0
    call nc4_get_variable_xy(iun, fname, 'Elev', dat2_r, units, fill_r, z)
    if (z == 0) then
        if (all(dat2_r == fill_r)) then
            write(field, FMT_GEN) fill_r
            call print_error("All values in the 'Elev' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
            z = 1
        else
            do n = 1, shd%NA
                shd%ELEV(n) = dat2_r(shd%xxx(n), shd%yyy(n))
            end do
        end if
    end if
    if (z /= 0) ierr = 1
!?    shd%SLOPE_TOPO = 0.0
!?    call nc4_get_variable_xy(iun, fname, 'IntSlope', dat2_r, units, fill_r, z)
!?    if (z == 0) then
!?        if (all(dat2_r == fill_r)) then
!?            write(field, FMT_GEN) fill_r
!?            call print_error("All values in the 'IntSlope' attribute are the 'NO_DATA' value (" // trim(adjustl(field)) // ").")
!?            z = 1
!?        else
!?            do n = 1, shd%NA
!?                shd%SLOPE_TOPO(n) = dat2_r(shd%xxx(n), shd%yyy(n))
!?            end do
!?        end if
!?    end if
!?    if (z /= 0) ierr = 1

    !> Calculate 'AL' (modified from bsn.f).
    if (ierr == 0) then
        if (shd%CoordSys%Proj == 'UTM' .or. shd%CoordSys%Proj == 'CARTESIAN') then
            shd%AL = sqrt(shd%yDelta*shd%xDelta)
        else if (shd%CoordSys%Proj == 'LATLONG') then
            shd%AL = 0.0
            do y = 1, shd%yCount
                do x = 1, shd%xCount

                    !> Calculate the latitude in radians.
                    fill_r = (shd%yOrigin + shd%yDelta*y)/180.0*2.0*acos(0.0)

                    !> Calculate the grid area.
                    dat2_r(x, y) = &
                        shd%yDelta*(111.1360 - 0.5623*cos(2.0*fill_r) + &
                        0.0011*cos(4.0*fill_r))*shd%xDelta*(111.4172*cos(fill_r) - 0.094*cos(3.0*fill_r) + &
                        0.0002*cos(5.0*fill_r))*1000.0*1000.0
                    if (shd%RNKGRD(y, x) > 0) then
                        shd%AL = shd%AL + sqrt(dat2_r(x, y))
                    end if
                end do
            end do

            !> 'AL' at centre grid from origin.
            shd%AL = shd%AL/shd%NA !sqrt(dat2_r(shd%xCount/2, shd%yCount/2))
        else
            call print_message("ERROR: Unknown projection type '" // trim(shd%CoordSys%Proj) // "'.")
            ierr = 1
            goto 999
        end if
    end if

    !> Calculate 'FRAC'.
    shd%FRAC = shd%AREA/shd%AL/shd%AL

    !> Close the file to free the unit.
999 continue
    call nc4_close_file(iun, fname, z)

end subroutine
