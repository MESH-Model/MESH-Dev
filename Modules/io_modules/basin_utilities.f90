module basin_utilities

    !> 'mesh_io': For I/O field types and functions.
    use mesh_io

    !> 'print_routines': For print routines, format statements, and line lengths and limits.
    use print_routines

    implicit none

    contains

    subroutine basin_info_from_field_list(field_list, shd, error_status)

        !> Variables.
        use projection_variables, only: pj
        use model_variables
        use control_variables, only: ro
        use sa_mesh_common
        use FLAGS, only: SUBBASINFLAG

        !> Parsing utilities.
        use strings, only: uppercase, lowercase
        use field_utilities

        !> Input/output variables.
        !* field_list: List of fields (read from file).
        !* error_status: Status returned by the operation (optional; 0: normal).
        type(io_field), dimension(:), intent(in) :: field_list
        integer, intent(out) :: error_status
!>>temp
        type(ShedGridParams) shd
!<<temp

        !> Local variables.
        character(len = SHORT_FIELD_LENGTH) field, level, code
        character(len = LONG_FIELD_LENGTH) line
        character(len = :), allocatable :: message
        real, allocatable :: dat1_r(:), lat_xy(:, :), lon_xy(:, :), gru_n(:), gru_nm(:, :)
        integer, allocatable :: SUBBASIN(:), dat1_i(:), dat2_i(:, :)
        integer y, x, n, m, k, l, i, ierr

        !> Status.
        error_status = 0

        !> Find and assign the 'RANK' field from the list of fields.
        vs%cell_count = 0
        vs%active_cell_count = 0
        vs%grid_x = 0
        vs%grid_y = 0
        do i = 1, size(field_list)
            select case (lowercase(field_list(i)%label))

                !> Distributed 'Rank' field.
                case ('rank')

                    !> Check if 'cell' variables are already activated (existing 'Rank').
                    if (associated(vs%grid)) then
                        call print_error("The 'Rank' field has been read and is already initialized.")
                        error_status = 1
                        return
                    end if

                    !> Extract the field.
                    call assign_field(field_list(i), dat2_i, ierr)
                    if (ierr /= 0) then
                        call print_error("Unknown or incompatible input variable format for 'Rank'.")
                        error_status = 1
                        return
                    end if

                    !> Check the order of the X/Y dimensions.
                    ierr = 0
                    allocate(dat1_i(2))
                    dat1_i(1) = field_list(i)%mapping%mapped_dim_order(MAP_ORDER_X)
                    dat1_i(2) = field_list(i)%mapping%mapped_dim_order(MAP_ORDER_Y)
                    if (dat1_i(1) == 0 .and. dat1_i(2) == 0 .and. field_list(i)%mapping%mapped_dim_order(MAP_ORDER_N) > 0) then

                        !> Map the 1-D basin definition to the structure of the 2-D array.
                        dat1_i = (/1, 2/)
                    else if (dat1_i(1) == 0 .or. dat1_i(2) == 0) then
                        ierr = 1
                    end if

                    !> Print a warning if no appropriate spatial dimensions were found.
                    if (ierr /= 0) then
                        call print_error("The 'Rank' field does not contain the necessary spatial dimensions.")
                        error_status = 1
                        return
                    else

                        !> Allocate the 'cell' group of variables.
                        allocate(vs%grid)
                        vs%grid%dim_name = 'cell'
                        ro%RUNGRID = .true.
                    end if

                    !> Map the array of 'Rank' in the desired X/Y order.
                    call map_dimensions(dat2_i, dat1_i, vs%grid%from_grid_xy, ierr)

                    !> Count the number of active cells.
                    vs%cell_count = count(vs%grid%from_grid_xy > 0)
                    vs%grid%dim_length = vs%cell_count
                    if (.not. vs%cell_count > 0) then
                        call print_error("The basin contains no active cells. All 'Rank' values are zero.")
                        error_status = 1
                        return
                    else
                        call allocate_field(lat_xy, shape(vs%grid%from_grid_xy), ierr)
                        call allocate_field(lon_xy, shape(vs%grid%from_grid_xy), ierr)
                    end if

                    !> Assign maps.
                    vs%grid_x = size(vs%grid%from_grid_xy, 1)
                    vs%grid_y = size(vs%grid%from_grid_xy, 2)
                    allocate(vs%grid%from_grid_x(vs%cell_count), vs%grid%from_grid_y(vs%cell_count))
                    vs%grid%from_grid_x = 0
                    vs%grid%from_grid_y = 0
                    do x = 1, vs%grid_x
                        do y = 1, vs%grid_y
                            if (vs%grid%from_grid_xy(x, y) > 0) then
                                vs%grid%from_grid_x(vs%grid%from_grid_xy(x, y)) = x
                                vs%grid%from_grid_y(vs%grid%from_grid_xy(x, y)) = y
                            end if
                        end do
                    end do

                    !> Check for errors.
                    if (any(vs%grid%from_grid_x == 0) .or. any(vs%grid%from_grid_y == 0)) then
                        do n = 1, vs%cell_count
                            if (vs%grid%from_grid_x(n) == 0 .or. vs%grid%from_grid_y(n) == 0) then
                                write(code, *) n
                                call print_error("'Rank' " // trim(adjustl(code)) // " has no X/Y location inside the basin.")
                            end if
                        end do
                        error_status = 1
                        return
                    end if
            end select
        end do

        !> Check the 'Rank' field was assigned.
        if (.not. associated(vs%grid)) then
            call print_error("The 'Rank' field was not found.")
            error_status = 1
            return
        end if

        !> Find and assign reference attributes and variables from the list of fields.
        vs%gru_count = 0
        vs%riverclass_count = 0
        do i = 1, size(field_list)
            ierr = 0
            select case (lowercase(field_list(i)%label))

                !> EnSim reference fields.
                case ('xorigin')
                    call assign_field(field_list(i), pj%llc_x, ierr)
                case ('yorigin')
                    call assign_field(field_list(i), pj%llc_y, ierr)
                case ('xdelta')
                    call assign_field(field_list(i), pj%dx, ierr)
                case ('ydelta')
                    call assign_field(field_list(i), pj%dy, ierr)

                !> Projection.
                case ('projection', 'grid_mapping_name')
                    call assign_field(field_list(i), pj%projection, ierr)
                    if (ierr == 0) then
                        if (pj%projection == 'latitude_longitude') pj%projection = 'LATLONG'
                    end if
                case ('ellipsoid', 'datum')
                    call assign_field(field_list(i), pj%ellipsoid, ierr)
                case ('semi_major_axis')
                    call assign_field(field_list(i), pj%semi_major_axis, ierr)
                case ('inverse_flattening')
                    call assign_field(field_list(i), pj%inverse_flattening, ierr)
                case ('zone')
                    call assign_field(field_list(i), pj%zone, ierr)
                case ('centrelatitude')
                    call assign_field(field_list(i), pj%centre_latitude, ierr)
                case ('centrelongitude')
                    call assign_field(field_list(i), pj%centre_longitude, ierr)
                case ('rotationlatitude')
                    call assign_field(field_list(i), pj%rotation_latitude, ierr)
                case ('rotationlongitude')
                    call assign_field(field_list(i), pj%rotation_longitude, ierr)
                case ('earth_radius')
                    call assign_field(field_list(i), pj%earth_radius, ierr)
                case ('grid_north_pole_latitude')
                    call assign_field(field_list(i), pj%grid_north_pole_latitude, ierr)
                case ('grid_north_pole_longitude')
                    call assign_field(field_list(i), pj%grid_north_pole_longitude, ierr)

                !> Coordinates.
                case ('classcount', 'ngru')
                    call assign_field(field_list(i), vs%gru_count, ierr)
                case ('latitude', 'lat')
                    if (any(field_list(i)%dim_names == DIM_NAME_X) .and. any(field_list(i)%dim_names == DIM_NAME_Y)) then
                        call assign_field(field_list(i), pj%lat_xy, ierr)
                        if (ierr == 0) lat_xy = pj%lat_xy
                    else
                        call assign_field(field_list(i), pj%lat, ierr)
                        if (ierr == 0) then
                            if (any(field_list(i)%dim_names == DIM_NAME_N)) then
                                lat_xy(:, 1) = pj%lat
                            else if (size(pj%lat) == vs%grid_y) then
                                do y = 1, size(pj%lat)
                                    lat_xy(:, y) = pj%lat
                                end do
                            end if
                        end if
                    end if
                    if (any(lat_xy == NO_DATA_REAL)) ierr = 1

                    !> Check type.
!-                    select case (size(field_list(i)%dim_names))
!-                        case (1)
!-                            call assign_field(field_list(i), pj%lat, ierr)
!-                        case (2)
!-                            call assign_field(field_list(i), pj%lat_xy, ierr)
!-                    end select
                case ('rlat')
                    call assign_field(field_list(i), pj%lat, ierr)
                case ('longitude', 'lon')
                    if (any(field_list(i)%dim_names == DIM_NAME_X) .and. any(field_list(i)%dim_names == DIM_NAME_Y)) then
                        call assign_field(field_list(i), pj%lon_xy, ierr)
                        if (ierr == 0) lon_xy = pj%lon_xy
                    else
                        call assign_field(field_list(i), pj%lon, ierr)
                        if (ierr == 0) then
                            if (any(field_list(i)%dim_names == DIM_NAME_N)) then
                                lon_xy(:, 1) = pj%lon
                            else if (size(pj%lon) == vs%grid_x) then
                                do x = 1, size(pj%lon)
                                    lon_xy(x, :) = pj%lon
                                end do
                            end if
                        end if
                    end if
                    if (any(lon_xy == NO_DATA_REAL)) ierr = 1

                    !> Check type.
!-                    select case (size(field_list(i)%dim_names))
!-                        case (1)
!-                            call assign_field(field_list(i), pj%lon, ierr)
!-                        case (2)
!-                            call assign_field(field_list(i), pj%lon_xy, ierr)
!-                    end select
                case ('rlon')
                    call assign_field(field_list(i), pj%lon, ierr)
                case ('numriverclasses', 'nrvr')
                    call assign_field(field_list(i), vs%riverclass_count, ierr)

                !> Attributes.
                case ('nominalgridsize_al', 'size_length')
                    call assign_field(field_list(i), pj%nominal_side_length, ierr)
            end select

            !> Check for errors.
            if (ierr /= 0) then
                call print_warning("An error occurred parsing the variable '" // trim(field_list(i)%label) // "'.")
            end if
        end do

        !> Validate the spatial reference.
        select case (lowercase(pj%projection))

            !> Regular lat/lon projection.
            case ('latlong')

                !> Try to derive missing coordinates (where inconsistent by file specification).
                if (pj%llc_y /= 0.0 .and. pj%dy /= 0.0 .and. pj%llc_x /= 0.0 .and. pj%dx /= 0.0) then

                    !> From EnSim Hydrologic/Green Kenue geometry (for outputs in alternate file formats).
                    if (allocated(pj%lat_xy)) deallocate(pj%lat_xy)
                    if (allocated(pj%lat)) deallocate(pj%lat)
                    if (allocated(pj%lon_xy)) deallocate(pj%lon_xy)
                    if (allocated(pj%lon)) deallocate(pj%lon)
                    allocate( &
                        pj%lat_xy(vs%grid_x, vs%grid_y), pj%lat(vs%grid_y), &
                        pj%lon_xy(vs%grid_x, vs%grid_y), pj%lon(vs%grid_x))
                    pj%lat_xy = 0.0
                    do y = 1, vs%grid_y
                        pj%lat_xy(:, y) = pj%llc_y + pj%dy*y - pj%dy/2.0
                        pj%lat(y) = pj%lat_xy(1, y)
                    end do
                    lat_xy = pj%lat_xy
                    pj%lon_xy = 0.0
                    do x = 1, vs%grid_x
                        pj%lon_xy(x, :) = pj%llc_x + pj%dx*x - pj%dx/2.0
                        pj%lon(x) = pj%lon_xy(x, 1)
                    end do
                    lon_xy = pj%lon_xy
                else

                    !> From alternate file formats.
                    if (pj%dy == 0.0 .and. allocated(pj%lat)) then
                        do i = 2, size(pj%lat)
                            pj%dy = pj%dy + (pj%lat(i) - pj%lat(i - 1))
                        end do
                        pj%dy = pj%dy/(size(pj%lat) - 1)
                        pj%llc_y = pj%lat(1) - pj%dy/2.0
                    end if
                    if (pj%dx == 0.0 .and. allocated(pj%lon)) then
                        do i = 2, size(pj%lon)
                            pj%dx = pj%dx + (pj%lon(i) - pj%lon(i - 1))
                        end do
                        pj%dx = pj%dx/(size(pj%lon) - 1)
                        pj%llc_x = pj%lon(1) - pj%dx/2.0
                    end if
                end if

                !> Determine the ellipsoid/datum reference (NetCDF; reference: GK Manual, Sep 2010).
                if (pj%semi_major_axis > 0.0 .and. pj%inverse_flattening > 0.0) then
                    if (pj%semi_major_axis == 6378137.0 .and. pj%inverse_flattening == 298.257223563) then
                        pj%ellipsoid = 'WGS84'
                    else if (pj%semi_major_axis == 6378135.0 .and. pj%inverse_flattening == 298.26) then
                        pj%ellipsoid = 'WGS72'
                    else if (pj%semi_major_axis == 6378137.0 .and. pj%inverse_flattening == 298.257222101) then
                        pj%ellipsoid = 'NAD83'
                    else if (pj%semi_major_axis == 6378206.4 .and. pj%inverse_flattening == 294.9786982) then
                        pj%ellipsoid = 'NAD27'
                    else if (pj%semi_major_axis == 6371000.0 .and. pj%inverse_flattening == 0.0) then
                        pj%ellipsoid = 'SPHERE'
                    else
                        message = "Unknown datum using the 'semi_major_axis' and 'inverse_flattening' attributes ("
                        write(code, *) pj%semi_major_axis
                        message = message // trim(adjustl(code)) // ", "
                        write(code, *) pj%inverse_flattening
                        message = message // trim(adjustl(code)) // ")."
                        call print_warning(message)
                    end if
                end if

            !> Rotated lat/lon projection.
            case ('rotlatlong', 'rotated_latitude_longitude')

                !> Regular lat/lon conversion must be read from file, check if the fields exist.
                if (.not. allocated(pj%lat_xy)) then
                    call print_error( &
                        "The current projection '" // trim(pj%projection) // "' is projected. Latitudes in regular degrees " // &
                        "must be provided for each cell in the basin but none were found in the file.")
                    error_status = 1
                end if
                if (.not. allocated(pj%lon_xy)) then
                    call print_error( &
                        "The current projection '" // trim(pj%projection) // "' is projected. Longitudes in regular degrees " // &
                        "must be provided for each cell in the basin but none were found in the file.")
                    error_status = 1
                end if

            !> Unknown or unsupported projection.
            case default
                call print_error("Unknown or unsupported coordinate system '" // trim(pj%projection) // "'.")
                error_status = 1
        end select
        if (error_status /= 0) return

        !> Assign the lat/lon variables.
        allocate(vs%grid%lat(vs%cell_count))
        if (allocated(lat_xy)) then
            do n = 1, vs%cell_count
                vs%grid%lat(n) = lat_xy(vs%grid%from_grid_x(n), vs%grid%from_grid_y(n))
            end do
        else
            call print_error("A latitudinal reference 'lat' was not found.")
            error_status = 1
        end if
        allocate(vs%grid%lon(vs%cell_count))
        if (allocated(lon_xy)) then
            do n = 1, vs%cell_count
                vs%grid%lon(n) = lon_xy(vs%grid%from_grid_x(n), vs%grid%from_grid_y(n))
            end do
        else
            call print_error("A longitudinal reference 'lon' was not found.")
            error_status = 1
        end if
        if (error_status /= 0) return

        !> Convert longitudes from (0:360) to (-180:180).
        where (vs%grid%lon > 180.0)
            vs%grid%lon = vs%grid%lon - 360.0
        end where

        !> Check for fields used for mapping.
        do i = 1, size(field_list)

            !> Assign the field.
            ierr = 0
            select case (field_list(i)%field_name)

                !> River class.
                case ('IRVR', 'IAK')
                    call create_ranked_field_and_maps(field_list(i), ierr)
                    if (ierr == 0) call map_field_to_ranked_output(field_list(i), error_status = ierr)
                    if (ierr == 0) call assign_cell_values(field_list(i), vs%grid%from_riverclass, ierr)

                !> GRUs.
                case ('GRU', 'LANDCOVER')
                    if (vs%gru_count > 0 .and. field_list(i)%level_id > 0) then
                        if (.not. allocated(gru_nm)) then
                            allocate(gru_nm(vs%cell_count, vs%gru_count))
                            gru_nm = 0.0
                        end if
                        call create_ranked_field_and_maps(field_list(i), ierr)
                        if (ierr == 0) call map_field_to_ranked_output(field_list(i), error_status = ierr)
                        if (ierr == 0) call assign_cell_values(field_list(i), gru_n, ierr)
                        if (ierr == 0) gru_nm(:, field_list(i)%level_id) = gru_n
                        if (allocated(gru_n)) deallocate(gru_n)
                    end if
            end select

            !> Check for errors.
            if (ierr /= 0) then
                call print_warning("An error occurred assigning the '" // trim(field_list(i)%label) // "' variable.")
            end if
        end do

        !> Check if river classes were found (used for mapping).
        if (ro%RUNCHNL) then
            if (vs%riverclass_count == 0 .and. .not. allocated(vs%grid%from_riverclass)) then

                !> Define a single river class if none were found and channel routing is enabled.
                call print_remark( &
                    "At least one river class is required when channel routing is enabled. " // &
                    "The number of river classes is not defined or the 'IAK' or 'IRVR' map is missing in the file. " // &
                    "Assuming one river class.")
                vs%riverclass_count = 1
                allocate(vs%grid%from_riverclass(vs%cell_count))
                vs%grid%from_riverclass = 1
            else if (vs%riverclass_count /= maxval(vs%grid%from_riverclass)) then

                !> Check the number of river classes.
                call print_remark( &
                    "The number of river classes is adjusted to the maximum 'IAK' or 'IRVR' value. " // &
                    "Consider adjusting the input files.")
                vs%riverclass_count = maxval(vs%grid%from_riverclass)
            end if
        end if

        !> Assign remaining variables.
        call create_mapped_output_from_field_list(field_list, error_status = error_status)
        if (error_status /= 0) return
        call assign_variables_from_field_list(field_list, error_status)
        if (error_status /= 0) return

        !> Activate 'basin' variables if 'Next' was found.
        if (allocated(vs%grid%next_id)) then
            allocate(vs%basin)
            vs%basin%dim_name = 'basin'
            vs%basin%dim_length = vs%cell_count

            !> Derive the number of points inside the basin.
            vs%active_cell_count = count(vs%grid%next_id /= 0)

            !> Check the maximum number of cells and outlets, and print a warning if an adjustment is made.
            if (vs%cell_count /= maxval(vs%grid%next_id)) then
                call print_remark( &
                    "The total number of active cells is adjusted to the maximum 'Next' value. Consider adjusting the input files.")
                vs%grid%dim_length = maxval(vs%grid%next_id)
            end if
            if (vs%active_cell_count /= (maxval(vs%grid%next_id) - count(vs%grid%next_id == 0))) then
                call print_remark( &
                    "The number of outlets is adjusted to the number of cells where 'Next' is zero. " // &
                    "Consider adjusting the input files.")
                vs%active_cell_count = maxval(vs%grid%next_id) - count(vs%grid%next_id == 0)
            end if
        end if

        !> Validate other fields.
        ierr = 0
        if (ro%RUNCHNL) then
            if (.not. allocated(vs%grid%next_id)) then
                call print_error("The 'Next' variable was not found.")
                ierr = 0
            end if
            if (.not. allocated(vs%grid%drainage_area) .and. .not. allocated(vs%grid%surface_area)) then
                call print_error( &
                    "Neither a field for drainage area 'DA' nor surface area 'GridArea' were found. " // &
                    "At least one of these fields is required.")
                ierr = 1
            end if
            if (.not. allocated(vs%grid%chnl_slope)) then
                call print_error("The 'ChnlSlope' variable was not found.")
                ierr = 1
            end if
            if (.not. allocated(vs%grid%chnl_length)) then
                call print_error("The 'ChnlLength' variable was not found.")
                ierr = 1
            end if
            if (ierr /= 0) then
                error_status = 1
                return
            end if

            !> Calculate 'DA' from 'AREA' if not provided.
            if (.not. allocated(vs%grid%drainage_area)) then
                call print_remark( &
                    "No drainage area 'DA' variable found. Accumulating areas from surface area 'GridArea' by " // &
                    "flow direction 'Next'.")
                allocate(vs%grid%drainage_area(vs%cell_count))
                vs%grid%drainage_area = vs%grid%surface_area/1000000.0
                do n = 1, vs%active_cell_count
                    vs%grid%drainage_area(vs%grid%next_id(n)) = vs%grid%drainage_area(vs%grid%next_id(n)) + vs%grid%drainage_area(n)
                end do
            end if

            !> Assume no reaches if 'REACH' was not provided.
            if (.not. allocated(vs%grid%ireach)) then
                call print_remark( &
                    "No 'Reach' or 'IREACH' variable found. Assuming no reservoirs or routed lakes exist inside the basin.")
                allocate(vs%grid%ireach(vs%cell_count))
                vs%grid%ireach = 0
            end if

            !> Set 'BNKFLL' to a dummy value if not provided.
            if (.not. allocated(vs%grid%bankfull)) then
                call print_remark("No 'Bankfull' variable found. Setting the background field to zero.")
                allocate(vs%grid%bankfull(vs%cell_count))
                vs%grid%bankfull = 0.0
            end if

            !> Set 'ICHNL' to a dummy value if not provided (not used).
            if (.not. allocated(vs%grid%ichnl)) then
                call print_remark("No 'Chnl' or 'ICHNL' variable found. Assuming a single channel class.")
                allocate(vs%grid%ichnl(vs%cell_count))
                vs%grid%ichnl = 1
            end if
        end if

        !> Assume a unit area weighting if 'GridArea' was not found.
        if (.not. allocated(vs%grid%surface_area)) then
            call print_remark("No 'GridArea' variable found. Assuming nominal 1.0 unit area.")
            allocate(vs%grid%surface_area(vs%cell_count))
            vs%grid%surface_area = 1.0
        end if

        !> Assume a nominal side-length of 1.0 if 'NominalGridSize_AL' was not found.
        if (.not. pj%nominal_side_length > 0.0) pj%nominal_side_length = 1.0

        !> Calculate 'FRAC' for each cell.
        allocate(dat1_r(vs%cell_count))
        dat1_r = vs%grid%surface_area/pj%nominal_side_length/pj%nominal_side_length
        call move_alloc(dat1_r, vs%grid%area_weight)

        !> Set to unit weight 1.0 where 'area_weight' is zero.
!        where (vs%grid%area_weight == 0.0) vs%grid%area_weight = 1.0

        !> Validate individual values.
        ierr = 0
        do n = 1, vs%active_cell_count

            !> Prepare 'Rank' ID for output.
            write(code, *) n
            message = "'RANK " // trim(adjustl(code)) // "'"

            !> If channel routing is enabled.
            if (ro%RUNCHNL) then

                !> Check for invalid channel slope.
                if (vs%grid%chnl_slope(n) <= 0) then
                    call print_error("Invalid or negative channel slope 'ChnlSlope' at " // message // ".")
                    ierr = 1
                end if

                !> Check for invalid channel length.
                if (vs%grid%chnl_length(n) <= 0.0) then
                    call print_error("Invalid or negative channel length 'ChnlLength' at " // message // ".")
                    ierr = 1
                end if

                !> Check for invalid drainage area.
                if (vs%grid%drainage_area(n) <= 0.0) then
                    call print_error("Invalid or negative drainage area 'DA' at " // message // ".")
                    ierr = 1
                end if
            end if

            !> Check for invalid 'Next'.
            if (allocated(vs%grid%next_id)) then
                if (vs%grid%next_id(n) <= n) then
                    write(code, *) vs%grid%next_id(n)
                    call print_warning( &
                        "The 'Next' (" // trim(adjustl(code)) // ") might be upstream of 'Rank' ('Next' <= 'Rank') at " // &
                        message // ".")
                end if
            end if

            !> Check for invalid area.
            if (vs%grid%surface_area(n) <= 0.0) then
                call print_error("Invalid or negative grid area 'GridArea' at " // message // ".")
                ierr = 1
            end if
        end do
        if (ierr /= 0) then
            error_status = 1
            return
        end if

!>>temp
        shd%CoordSys%Proj = trim(pj%projection)
        shd%CoordSys%Ellips = trim(pj%ellipsoid)
        allocate(shd%CoordSys%lon(size(pj%lon)), source = pj%lon)
        shd%xCount = size(pj%lon)
        shd%xOrigin = pj%llc_x
        shd%xDelta = pj%dx
        allocate(shd%CoordSys%lat(size(pj%lat)), source = pj%lat)
        shd%yCount = size(pj%lat)
        shd%yOrigin = pj%llc_y
        shd%yDelta = pj%dy
        shd%NA = vs%grid%dim_length
        if (allocated(vs%grid%next_id)) then
            shd%NAA = count(vs%grid%next_id > 0)
            allocate(shd%NEXT(vs%grid%dim_length), source = vs%grid%next_id)
        else
            shd%NAA = 1
        end if
        allocate(shd%AREA(vs%grid%dim_length), source = vs%grid%surface_area)
        if (ro%RUNCHNL) then
            allocate(shd%IAK(vs%grid%dim_length), source = vs%grid%from_riverclass)
            shd%NRVR = maxval(vs%grid%from_riverclass)
            allocate(shd%SLOPE_CHNL(vs%grid%dim_length), source = vs%grid%chnl_slope)
            allocate(shd%CHNL_LEN(vs%grid%dim_length), source = vs%grid%chnl_length)
            allocate(shd%ICHNL(vs%grid%dim_length), source = vs%grid%ichnl)
            allocate(shd%IREACH(vs%grid%dim_length), source = vs%grid%ireach)
            allocate(shd%DA(vs%grid%dim_length), source = vs%grid%drainage_area)
            allocate(shd%BNKFLL(vs%grid%dim_length), source = vs%grid%bankfull)
            allocate(shd%IROUGH(vs%grid%dim_length))
            shd%IROUGH = 0
        end if
        allocate(shd%ELEV(vs%grid%dim_length))
        shd%ELEV = 0.0
        shd%AL = pj%nominal_side_length
        allocate(shd%xlng(vs%grid%dim_length), source = vs%grid%lon)
        allocate(shd%ylat(vs%grid%dim_length), source = vs%grid%lat)
        allocate(shd%xxx(vs%grid%dim_length), source = vs%grid%from_grid_x)
        allocate(shd%yyy(vs%grid%dim_length), source = vs%grid%from_grid_y)
        allocate(shd%FRAC(vs%grid%dim_length), source = vs%grid%area_weight)
!<<temp

!>>>
        !> Check for basin structures.
        if (ro%RUNCHNL) then
            call read_basin_structures(shd, error_status)
            if (error_status /= 0) return

            !> Deactivate land cover in cells outside the range of subbasins to
            !>  exclude them from NML. The original method used the CLASS 'FARE'
            !>  parameter to do this via GATPREP when CLASS was still integrated
            !>  in 'MESH_driver' directly. This approach doesn't work when the
            !>  'tile' variables were moved to the 'MESH_driver' level to
            !>  accommodate removal of the CLASS GATHER/SCATTER routines in r733
            !>  and the addition of the MPI exchanges in r1022.
            if (SUBBASINFLAG > 0 .and. allocated(gru_nm)) then

                !> Print message to screen.
                call reset_tab()
                call print_message('SUBBASIN mask is ACTIVE.')
                call increase_tab()

                !> Allocate and initialize local variables.
                allocate(SUBBASIN(vs%cell_count))
                SUBBASIN = 0

                !> Set gauge locations to 1.
                do l = 1, fms%stmg%n
                    SUBBASIN(fms%stmg%meta%rnk(l)) = l
                end do
                if (DIAGNOSEMODE) then
                    write(code, FMT_GEN) fms%stmg%n
                    call print_message('Masking domains for ' // trim(adjustl(code)) // ' subbasins.')
                end if

                !> Mask grids upstream of gauge locations.
                i = 1
                do while (i > 0)
                    i = 0
                    do n = 1, vs%active_cell_count
                        if (SUBBASIN(vs%grid%next_id(n)) > 0 .and. SUBBASIN(n) == 0) then
                            SUBBASIN(n) = SUBBASIN(vs%grid%next_id(n))
                            i = 1
                        end if
                    end do
                end do

                !> Set GRU fraction 'ACLASS' to zero for all cells in the grid.
                do m = 1, vs%gru_count
                    where (SUBBASIN == 0) gru_nm(:, m) = 0.0
                end do

                !> Print diagnostic information to screen.
                if (DIAGNOSEMODE) then
                    write(line, FMT_GEN) 'SUBBASIN', 'GRIDS'
                    call print_message(line)
                    do l = 1, fms%stmg%n
                        write(line, FMT_GEN) l, count(SUBBASIN == l)
                        call print_message(line)
                    end do
                end if
            end if
        end if
!<<<

        !> Print a summary.
        write(code, *) vs%cell_count
        call print_message("Total number of grids: " // trim(adjustl(code)))
        write(code, *) vs%active_cell_count
        call print_message("Total number of grids inside the basin: " // trim(adjustl(code)))
        write(code, *) pj%nominal_side_length
        call print_message("Side length of grid: " // trim(adjustl(code)) // " m")
        if (ro%RUNCHNL) then
            write(code, *) vs%riverclass_count
            call print_message("Number of river classes: " // trim(adjustl(code)))
        end if

        !> Check if GRUs (land cover) were found (used for mapping).
        ro%RUNTILE = .false.
        if (ro%RUNLSS) then
            if (vs%gru_count == 0 .or. .not. allocated(gru_nm)) then

                !> Define a single active GRU (and one for the inactive impervious cover) if none were found.
                call print_remark( &
                    "At least one GRU (land cover) is required when tile-processing (HLSS) is enabled. " // &
                    "The number of GRUs is not defined or the 'GRU' or 'LandCover' map is missing in the file. " // &
                    "Assuming one active GRU.")
                vs%gru_count = 2
                allocate(gru_nm(vs%cell_count, vs%gru_count))
                gru_nm(:, 1) = 1.0
                gru_nm(:, 2) = 0.0
            end if

            !> Adjust the land cover count to exclude impervious areas (the last GRU).
            vs%gru_count = vs%gru_count - 1

            !> Derive the 'landtile' map if tile-processing (HLSS) is enabled.
            vs%landtile_count = 0
            do m = 1, vs%gru_count

                !> Count the number of tiles.
                do n = 1, vs%cell_count
                    if (gru_nm(n, m) > 0.0) vs%landtile_count = vs%landtile_count + 1
                end do

                !> Print a remark if the land cover is not active.
                if (sum(gru_nm(:, m)) == 0.0) then
                    write(code, *) m
                    call print_remark("'GRU " // trim(adjustl(code)) // "' has zero coverage in the basin and is not active.")
                end if
            end do
            if (vs%landtile_count == 0) then
                call print_error( &
                    "The basin is configured to contain GRUs (land cover) but the fraction of cover of all GRUs is zero.")
                error_status = 1
                return
            end if

            !> Allocate 'tile' variables.
            allocate(vs%tile)
            vs%tile%dim_name = 'landtile'
            vs%tile%dim_length = vs%landtile_count
            ro%RUNTILE = .true.

            !> Allocate and assign maps.
            allocate( &
                vs%tile%from_cell(vs%landtile_count), vs%tile%from_gru(vs%landtile_count), vs%tile%from_grid_x(vs%landtile_count), &
                vs%tile%from_grid_y(vs%landtile_count), vs%tile%from_riverclass(vs%landtile_count), &
                vs%tile%lat(vs%landtile_count), vs%tile%lon(vs%landtile_count), vs%tile%area_weight(vs%landtile_count), &
                vs%tile%surface_area(vs%landtile_count))
            k = 1
            do n = 1, size(gru_nm, 1)

                !> Re-adjust GRU-fraction that does not add to 1.0.
                if (abs(sum(gru_nm(n, :)) - 1.0) > 0.0 .and. sum(gru_nm(n, :)) > 0.0) then

                    !> Print a warning if the missing fraction is significant (> 1%).
                    if (abs(sum(gru_nm(n, :)) - 1.0) > 0.1) then
                        write(code, *) n
                        message = "'Rank " // trim(adjustl(code)) // "'"
                        write(code, *) sum(gru_nm(n, :))
                        call print_remark( &
                            "The total fraction of GRUs (land cover) at " // message // " is adjusted from " // &
                            trim(adjustl(code)) // " to 1.0.")
                    end if
                    gru_nm(n, :) = gru_nm(n, :)/sum(gru_nm(n, :))
                end if

                !> Assign the maps and GRU-fraction.
                do m = 1, size(gru_nm, 2)
                    if (gru_nm(n, m) > 0.0) then

                        !> Update the maps.
                        vs%tile%from_cell(k) = n
                        vs%tile%from_gru(k) = m
                        vs%tile%from_grid_x(k) = vs%grid%from_grid_x(n)
                        vs%tile%from_grid_y(k) = vs%grid%from_grid_y(n)
                        if (ro%RUNCHNL) vs%tile%from_riverclass(k) = vs%grid%from_riverclass(n)

                        !> Transfer fields.
                        vs%tile%lon(k) = vs%grid%lon(n)
                        vs%tile%lat(k) = vs%grid%lat(n)
                        vs%tile%area_weight(k) = gru_nm(n, m)
                        vs%tile%surface_area(k) = vs%grid%surface_area(n)*gru_nm(n, m)

                        !> Increment the tile ID.
                        k = k + 1
                    end if
                end do
            end do

!>>temp
            shd%lc%NTYPE = vs%gru_count
            shd%lc%NML = vs%tile%dim_length
            shd%lc%ILG = vs%tile%dim_length
            allocate(shd%lc%ILMOS(vs%tile%dim_length), source = vs%tile%from_cell)
            allocate(shd%lc%JLMOS(vs%tile%dim_length), source = vs%tile%from_gru)
            allocate(shd%lc%ACLASS(vs%grid%dim_length, vs%gru_count + 1))
            shd%lc%ACLASS = 0.0
            do k = 1, vs%tile%dim_length
                shd%lc%ACLASS(vs%tile%from_cell(k), vs%tile%from_gru(k)) = vs%tile%area_weight(k)
            end do
!<<temp

            !> Print a summary.
            write(code, *) vs%gru_count
            call print_message("Number of GRUs: " // trim(adjustl(code)))
            write(code, *) vs%landtile_count
            call print_message("Number of land-based tiles: " // trim(adjustl(code)))
        end if

    end subroutine

end module
