subroutine READ_INITIAL_INPUTS(fls, shd, release, ierr)

    use mpi_module
    use strings
    use sa_mesh_common
    use projection_variables
    use model_files_variables
    use FLAGS
    use parse_utilities
    use mesh_io
    use basin_utilities

    implicit none

    !> Input variables.
    type(fl_ids) fls
    type(ShedGridParams) shd
    character(len = *), intent(in) :: release

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer iun, n, m, l, x, y, k, i, j, z
    real, dimension(:, :), allocatable :: grid
    character(len = DEFAULT_LINE_LENGTH) line, field
    character(len = DEFAULT_FIELD_LENGTH), dimension(50) :: args
!>>fews
    logical ltest
!<<fews
!>>
    type(io_file) :: basin_database
    real, allocatable :: dat1_r(:)
    integer, allocatable :: dat1_i(:)
    character(len = SHORT_FIELD_LENGTH), allocatable :: dim_names(:)
!<<

    !> SUBBASINFLAG.
    integer, dimension(:), allocatable :: SUBBASIN

    !> Initialize the return status.
    ierr = 0

    !> Reset spacing for screen output.
    call reset_tab()

    !>
    !> RUN OPTIONS.
    !>  Run options are read at the beginning of the run from
    !>  MESH_input_run_options.ini.
    !>
    call READ_RUN_OPTIONS(fls, shd, ierr)
    if (ierr /= 0) return

    !> Check that the output folder exists.
    write(line, FMT_GEN) ipid
    open(100, file = './' // trim(adjustl(fls%GENDIR_OUT)) // '/tmp' // trim(adjustl(line)), status = 'unknown', iostat = ierr)
    if (ierr /= 0) then
        ECHOTXTMODE = .false.
        call print_error('The output folder does not exist: ' // trim(adjustl(fls%GENDIR_OUT)))
        return
    else
        close(100, status = 'delete')
    end if

    !> Open the status file.
    call open_echo_txt('./' // trim(fls%GENDIR_OUT) // '/MESH_output_echo_print.txt', ierr)
    if (ierr /= 0) return

    !> Write MESH version to file.
    call reset_tab()
    call print_echo_txt(trim(release))
    call print_echo_txt('')

    !> Assign default configuration
    SHDFILEFMT = 1
    SHDTOMAPFLAG = .false.

    !> Parse 'SHDFILEFLAG'.
    call parse(SHDFILEFLAG, ' ', args, n)
    if (n > 1) then
        do i = 2, n
            select case (lowercase(args(i)))
                case ('1', 'r2c')
                    SHDFILEFMT = 1
                case ('2', 'class_ini')
                    SHDFILEFMT = 2
                case ('netcdf', 'nc')
                    SHDFILEFMT = 3
                case ('asc')
                    SHDFILEFMT = 4
                case ('nc_subbasin', 'nc_hru')
                    SHDFILEFMT = 5
                case ('to_map')
                    SHDTOMAPFLAG = .true.
            end select
        end do
    end if

    !> Read drainage database.
    select case (SHDFILEFMT)
        case (1)

            !> r2c ASCII format.
            basin_database%full_path = 'MESH_drainage_database.r2c'
            allocate(basin_database%container, source = io_type_r2c())

!> *********************************************************************
!> Open and read in values from MESH_input_drainage_database.txt file
!>   if new_shd.r2c file was not found
!> *********************************************************************
!        open(UNIT=20, FILE='MESH_input_drainage_database.txt',
!     &    STATUS='OLD', IOSTAT=ierr)
!        if (ierr == 0) then
!          print *, 'Reading Drainage Database from ',
!     &      'MESH_input_drainage_database.txt'
!        else
!          print *, 'MESH_input_drainage_database.txt not found'
!          stop
!        end if
!        read(20, '(i5, 50x, i5)') shd%NA, shd%NAA
!        read(20, '(f10.0, 5x, 2i5)') shd%AL, shd%NRVR, shd%lc%NTYPE
!        GRDN = 0.0
!        GRDE = 0.0

!todo change the code or documentation on these variables.
!ANDY Set ILG from the read-in values
!        ILG = shd%NA*shd%lc%NTYPE

!> Using IOSTAT allows us to try to read input that may or may not exist.
!> If all of the values successfully get read, IOSTAT=VarName will set
!> VarName to 0. If all of the values were not successfully read,
!> VarName would be set to 1 or more. In this case, the VarName that
!> we are using is ierr.
!        read(20, '(12i5, 2f5.0)', IOSTAT=ierr) IYMIN, WF_IYMAX,
!     &    JXMIN, WF_JXMAX, LATDEGMIN, LATMINMIN, LATDEGMAX, LATMINMAX,
!     &    LONDEGMIN, LONMINMIN, LONDEGMAX, LONMINMAX, GRDN, GRDE

!> Condition for Lat/Long by Frank S Sept/1999
!        if (GRDN > 0.0) then
!          IYMIN = LATDEGMIN*60 + LATMINMIN
!          WF_IYMAX = LATDEGMAX*60 + LATMINMAX
!          JXMIN = LONDEGMIN*60 + LONMINMIN
!          WF_JXMAX = LONDEGMAX*60 + LONMINMAX

!        else
!> Define GRDN & GRDE for UTM
!          GRDN = shd%AL/1000.0
!          GRDE = shd%AL/1000.0
!        end if
!        read(20, '(2i5)') YCOUNT, XCOUNT

!        do i = 1, YCOUNT
!          read(20, *)
!        end do

!        do i = 1, shd%NA
!          read(20, '(5x, 2i5, 3f10.5, i7, 5i5, f5.2, 15f5.2)') YYY(i),
!     &      XXX(i), WF_DA(i), WF_BNKFLL(i), WF_CHANNELSLOPE(i),
!     &      WF_ELEV(i), WF_IBN(i), WF_IROUGH(i), WF_ICHNL(i),
!     &      WF_NEXT(i), WF_IREACH(i), FRAC(i),
!     &      (ACLASS(i, j), j = 1, shd%lc%NTYPE)
!> check to make sure land cover areas sum to 100%
!          WF_LAND_COUNT = 1
!          WF_LAND_MAX = 0.0
!          WF_LAND_SUM = 0.0
!          do j = 1, shd%lc%NTYPE
!            WF_LAND_SUM = WF_LAND_SUM + ACLASS(i, j)
!            if (ACLASS(i, j) > WF_LAND_MAX) then
!              WF_LAND_COUNT = j
!              WF_LAND_MAX = ACLASS(i, j)
!            end if
!          end do
!          if (WF_LAND_SUM /= 1.0) THEN
!            ACLASS(i, WF_LAND_COUNT) =
!     &        ACLASS(i, WF_LAND_COUNT) - (WF_LAND_SUM - 1.0)
!          end if
!        end do

!        close(20)
        case (2)

            !> Point mode (location read from CLASS.ini).
            call reset_tab()
            call print_message("READING: (creating basin for point mode)")
            call increase_tab()
            allocate(dim_names(1), dat1_r(2), dat1_i(2))
            dim_names = (/DIM_NAME_SUBBASIN/)
            allocate(basin_database%container, source = io_type_virtual())
            allocate(basin_database%fields(10))
            basin_database%fields(1)%label = 'projection'
            allocate(basin_database%fields(1)%field, source = model_variable_char(dat = 'LATLONG'))
            basin_database%fields(2)%label = 'ellipsoid'
            allocate(basin_database%fields(2)%field, source = model_variable_char(dat = 'GRS80'))
            basin_database%fields(3)%label = 'ngru'
            allocate(basin_database%fields(3)%field, source = model_variable_int(dat = 2))
            dat1_i = (/1, 2/)
            basin_database%fields(4)%label = 'Rank'
            allocate(basin_database%fields(4)%field, source = model_variable_int1d(dat = dat1_i))
            dat1_i = (/2, 0/)
            basin_database%fields(5)%label = 'Next'
            allocate(basin_database%fields(5)%field, source = model_variable_int1d(dat = dat1_i))
            dat1_r = (/1.0, 0.0/)
            basin_database%fields(6)%label = 'GridArea'
            allocate(basin_database%fields(6)%field, source = model_variable_real1d(dat = dat1_r))
            basin_database%fields(7)%label = 'gru 1'
            allocate(basin_database%fields(7)%field, source = model_variable_real1d(dat = dat1_r))
            dat1_r = (/0.0, 0.0/)
            basin_database%fields(8)%label = 'gru 2'
            allocate(basin_database%fields(8)%field, source = model_variable_real1d(dat = dat1_r))
            basin_database%fields(9)%label = 'lat'
            allocate(basin_database%fields(9)%field, source = model_variable_real1d(dat = dat1_r))
            basin_database%fields(10)%label = 'lon'
            allocate(basin_database%fields(10)%field, source = model_variable_real1d(dat = dat1_r))
            do i = 4, 10
                allocate(basin_database%fields(i)%dim_names(size(dim_names)), source = dim_names)
            end do
            deallocate(dim_names)
        case (3, 5)

            !> NetCDF format.
#ifdef NETCDF
            basin_database%full_path = 'MESH_drainage_database.nc'
            allocate(basin_database%container, source = io_type_nc())
#else
            call print_error( &
                "The format of the drainage database input file is specified as NetCDF but the module is not active. " // &
                "A version of MESH compiled with the NetCDF library must be used to read files in this format.")
            ierr = 1
            return
#endif
        case (4)

            !> Simple text format (asc).
            basin_database%full_path = 'MESH_drainage_database.asc'
            allocate(dim_names(1))
            dim_names = (/DIM_NAME_N/)
            allocate(basin_database%overrides%dim_names(size(dim_names)), source = dim_names)
            allocate(basin_database%container, source = io_type_char_delimited())
        case default

            !> Unknown or unsupported format.
            write(line, FMT_GEN) SHDFILEFMT
            call print_error('Unrecognized drainage database format: ' // trim(adjustl(line)))
            ierr = 1
            return
    end select
!>>

    !> Read fields from file.
    if (.not. allocated(basin_database%container)) then
        call print_error("Unrecognized basin information file format.")
        ierr = 1
    else if (len_trim(basin_database%full_path) > 0) then
        call open_input_file(basin_database, error_status = ierr)
        if (ierr /= 0) return
    end if
    call read_file_fields_to_buffer(basin_database, error_status = ierr)
    if (ierr /= 0) return

    !> Assign fields.
    call basin_info_from_field_list(basin_database%fields, ierr)
    if (ierr /= 0) return
!<<

    !> Check maximum number of cells and outlets, and print a warning if an adjustment is made.
!-    if (ro%RUNCHNL) then
!-        if (shd%NA /= maxval(shd%NEXT)) then
!-            line = 'Total number of grids adjusted to maximum RANK. Consider adjusting the input files.'
!-            call print_remark(line)
!-            shd%NA = maxval(shd%NEXT)
!-        end if
!-        if (shd%NAA /= (maxval(shd%NEXT) - count(shd%NEXT == 0))) then
!-            line = 'The number of outlets adjusted to the number of cells where NEXT is zero. Consider adjusting the input files.'
!-            call print_remark(line)
!-            shd%NAA = maxval(shd%NEXT) - count(shd%NEXT == 0)
!-        end if
!-    end if

    !> Check for errors in the basin configuration.
!-    z = 0

    !> Print messages to screen (including non-critical warnings).
!-    do n = 1, shd%NAA

        !> Prepare 'RANK' ID for output.
!-        write(line, FMT_GEN) n
!-        line = 'RANK ' // trim(adjustl(line))

        !> If channel routing is enabled.
        !>  Errors: Invalid channel slope; invalid channel length; invalid grid area; invalid drainage area.
        !>  Warnings: NEXT <= RANK.
!-        if (ro%RUNCHNL) then
!-            if (shd%SLOPE_CHNL(n) <= 0) then
!-                z = 1
!-                call print_message('ERROR: Invalid or negative channel slope at ' // trim(adjustl(line)) // '.')
!-            end if
!-            if (shd%CHNL_LEN(n) <= 0.0) then
!-                z = 1
!-                call print_message('ERROR: Invalid or negative channel length at ' // trim(adjustl(line)) // '.')
!-            end if
!-            if (shd%AREA(n) <= 0.0) then
!-                z = 1
!-                call print_message('ERROR: Invalid or negative grid area at ' // trim(adjustl(line)) // '.')
!-            end if
!-            if (shd%DA(n) <= 0.0) then
!-                z = 1
!-                call print_message('ERROR: Invalid or negative drainage area at ' // trim(adjustl(line)) // '.')
!-            end if
!-            if (shd%NEXT(n) <= n) then
!-                call print_warning('NEXT might be upstream of RANK (NEXT <= RANK) at ' // trim(adjustl(line)) // '.')
!-            end if
!-        end if

        !> If tile processes (i.e., LSS) is enabled.
        !>  Errors: Sum of land covers is zero.
        !>  Warnings: Sum of land covers not equal to one.
        !>  Adjustments: Sum of land covers not equal to one.
!-        if (ro%RUNTILE) then
!-            if (sum(shd%lc%ACLASS(n, :)) == 0.0) then
!-                z = 1
!-                call print_message('ERROR: Total fraction of land covers (GRUs) is zero at ' // trim(adjustl(line)) // '.')
!-            else if (abs(sum(shd%lc%ACLASS(n, :)) - 1.0) > 0.0) then

                !> Print a warning if the missing fraction is significant (> 1%).
!-                if (abs(sum(shd%lc%ACLASS(n, :)) - 1.0) > 0.1) then
!-                    write(field, FMT_GEN) sum(shd%lc%ACLASS(n, :))
!-                    line = &
!-                        'Total fraction of land covers (GRUs) at ' // trim(adjustl(line)) // ' adjusted from ' // &
!-                        trim(adjustl(field)) // ' to 1.0.'
!-                    call print_warning(line)
!-                end if
!-                shd%lc%ACLASS(n, :) = shd%lc%ACLASS(n, :)/sum(shd%lc%ACLASS(n, :))
!-            end if
!-        end if

        !> General.
        !>  Errors: Cell has no x/y coordinate value (e.g., outside basin).
!-        if (shd%xxx(n) == 0 .or. shd%yyy(n) == 0) then
!-            z = 1
!-            call print_message(trim(line) // ' is assigned but outside the basin.')
!-        end if
!-    end do

    !> If tile processes (i.e., LSS) is enabled.
    !>  Remark: Land cover has no active coverage in any cells.
!-    if (ro%RUNTILE) then
!-        do m = 1, shd%lc%NTYPE
!-            if (sum(shd%lc%ACLASS(:, m)) == 0.0) then
!-                write(line, FMT_GEN) m
!-                call print_remark('GRU ' // trim(adjustl(line)) // ' has no coverage and is zero across the domain.')
!-            end if
!-        end do
!-    end if

    !> If routing is enabled.
    !>  Error: The number of river classes is zero.
!-    if (ro%RUNCHNL) then
!-        if (maxval(shd%IAK) == 0) then
!-            z = 1
!-            line = 'The number of river classes (IAK) is zero. At least one river class must exist when channel routing is enabled.'
!-            call print_error(line)
!-        end if
!-    end if

    !> Return if errors exist.
!-    if (z /= 0) then
!-        ierr = z
!-        call print_error('Errors exist in the drainage database.')
!-        return
!-    end if

    !> Check the number of river classes.
!-    if (ro%RUNCHNL) then
!-        if (shd%NRVR /= maxval(shd%IAK)) then
!-            call print_remark('The number of river classes is adjusted to match IAK. Consider adjusting the input files.')
!-            shd%NRVR = maxval(shd%IAK)
!-        end if
!-    end if

    !> Determine coordinates for intermediate grid locations.
    !> NOTE FROM FRANK
    !> I got the equations to determine the actual length of a
    !> degree of latitude and longitude from this paper, thank you
    !> Geoff Kite (I have attached it):
    !> http://www.agu.org/pubs/crossref/1994/94WR00231.shtml
    !> This chunk of code is a way to put the actual values of
    !> longitude and latitude for each cell in a large basin.
    !> The original CLASS code just put in the same value for each cell.
    !> The problem is that the class.ini file only has a single value
    !> of long and lat (as it was only designed for a point).  So in order
    !> to get the values across the basin I assumed that the single value
    !> from the class.ini file is in the centre of the basin and then use
    !> information from the watflow.shd file to figure out the long/lat
    !> varies across the basin.  However, the watflod.shd file only gives
    !> information in kilometers not degrees of long/lat so I had
    !> to use the formulas from the above paper to go between the two.
    !> The only value of DEGLAT is the one read in from the class.ini file,
    !> after that Diana uses RADJGRD (the value of latitude in radians) so
    !> after DEGLAT is used to calculate RADJGRD is it no longer used.  This
    !> is how it was in the original CLASS code.
!-    allocate(shd%ylat(shd%NA), shd%xlng(shd%NA))
!-    do i = 1, shd%NA
        !LATLENGTH = shd%AL/1000.0/(111.136 - 0.5623*cos(2*(DEGLAT*PI/180.0)) + 0.0011*cos(4*(DEGLAT*PI/180.0)))
        !LONGLENGTH = shd%AL/1000.0/(111.4172*cos((DEGLAT*PI/180.0)) - 0.094*cos(3*(DEGLAT*PI/180.0)) + 0.0002*cos(5*(DEGLAT*PI/180.0)))
!-        shd%ylat(i) = (shd%yOrigin + shd%yDelta*shd%yyy(i)) - shd%yDelta/2.0
!-        shd%xlng(i) = (shd%xOrigin + shd%xDelta*shd%xxx(i)) - shd%xDelta/2.0
!-    end do
!-    if (.not. allocated(shd%ylat)) then
!-        z = 0
!-        call allocate_variable(shd%ylat, shd%NA, z)
!-        if (btest(z, pstat%ALLOCATION_ERROR)) then
!-            call print_message("ERROR: An error occurred allocating the 'ylat' variable.")
!-            ierr = z
!-        end if
!-    end if
!-    if (.not. allocated(shd%xlng)) then
!-        z = 0
!-        call allocate_variable(shd%xlng, shd%NA, z)
!-        if (btest(z, pstat%ALLOCATION_ERROR)) then
!-            call print_message("ERROR: An error occurred allocating the 'xlng' variable.")
!-            ierr = z
!-        end if
!-    end if
!-    if (ierr /= 0) return
!-    select case (lowercase(shd%CoordSys%Proj))
!-        case ('latlong')
!-            z = 0
!-            call check_allocated(shd%ylat, shd%NA, z)
!-            if (.not. btest(z, pstat%ASSIGNED)) then
!-                shd%ylat = (shd%yOrigin + shd%yDelta*shd%yyy) - shd%yDelta/2.0
!-            end if
!-            z = 0
!-            call check_allocated(shd%xlng, shd%NA, z)
!-            if (.not. btest(z, pstat%ASSIGNED)) then
!-                shd%xlng = (shd%xOrigin + shd%xDelta*shd%xxx) - shd%xDelta/2.0
!-            end if
!-            shd%iyMin = int(shd%yOrigin*60.0)
!-            shd%iyMax = int((shd%yOrigin + shd%yCount*shd%yDelta)*60.0)
!-            shd%jxMin = int(shd%xOrigin*60.0)
!-            shd%jxMax = int((shd%xOrigin + shd%xCount*shd%xDelta)*60.0)
!-            shd%GRDE = shd%xDelta*60.0
!-            shd%GRDN = shd%yDelta*60.0
!-            if (.not. allocated(shd%CoordSys%lat)) then
!-                allocate(shd%CoordSys%lat(shd%yCount))
!-                do i = 1, shd%yCount
!-                    shd%CoordSys%lat(i) = (shd%yOrigin + shd%yDelta*i) - shd%yDelta/2.0
!-                end do
!-            end if
!-            if (.not. allocated(shd%CoordSys%lon)) then
!-                allocate(shd%CoordSys%lon(shd%xCount))
!-                do i = 1, shd%xCount
!-                    shd%CoordSys%lon(i) = (shd%xOrigin + shd%xDelta*i) - shd%xDelta/2.0
!-                end do
!-            end if
!?        case ('utm')
!?            shd%GRDE = shd%xDelta/1000.0
!?            shd%GRDN = shd%yDelta/1000.0
!?            shd%jxMin = int(shd%xOrigin/1000.0)
!?            shd%jxMax = shd%jxMin + shd%GRDE*(shd%xCount - 1)
!?            shd%iyMin = int(shd%yOrigin/1000.0)
!?            shd%iyMax = shd%iyMin + shd%GRDN*(shd%yCount - 1)
!-        case ('rotlatlong', 'rotated_latitude_longitude')
!-            z = 0
!-            call check_allocated(shd%ylat, shd%NA, z)
!-            if (.not. btest(z, pstat%ASSIGNED)) then
!-                call print_message( &
!-                    "ERROR: Latitudes in regular degrees have not been defined for the domain in '" // trim(shd%CoordSys%Proj) // &
!-                    "' projection. Coordinates are not automatically calculated for domains in '" // trim(shd%CoordSys%Proj) // &
!-                    "' projection, and must be provided as an attribute in the drainage database file.")
!-                ierr = 1
!-            end if
!-            z = 0
!-            call check_allocated(shd%xlng, shd%NA, z)
!-            if (.not. btest(z, pstat%ASSIGNED)) then
!-                call print_message( &
!-                    "ERROR: Longitudes in regular degrees have not been defined for the domain in '" // trim(shd%CoordSys%Proj) // &
!-                    "' projection. Coordinates are not automatically calculated for domains in '" // trim(shd%CoordSys%Proj) // &
!-                    "' projection, and must be provided as an attribute in the drainage database file.")
!-                ierr = 1
!-            end if
!-        case default
!-            call print_message('ERROR: Unsupported coordinate system: ' // trim(shd%CoordSys%Proj))
!-            ierr = 1
!-    end select
!-    if (ierr /= 0) return

    !> Convert longitudes from (0:360) to (-180:180).
!-    where (shd%xlng > 180.0)
!-        shd%xlng = shd%xlng - 360.0
!-    end where

    !> If no sub-grid variability is active.
!-    if (.not. ro%RUNTILE) then
!-        shd%lc%NTYPE = 1
!-        if (allocated(shd%lc%ACLASS)) deallocate(shd%lc%ACLASS)
!-        allocate(shd%lc%ACLASS(shd%NA, shd%lc%NTYPE + 1))
!-        shd%lc%ACLASS(:, shd%lc%NTYPE) = 1.0
!-        shd%lc%ACLASS(:, shd%lc%NTYPE + 1) = 0.0
!-    end if

    !> Compute the maximum number of tile elements.
!-    shd%lc%ILG = shd%NA*shd%lc%NTYPE
!-    shd%wc%ILG = shd%NA*shd%lc%NTYPE

    !> Determine the number of active tiles.
    !> Store callback indices in the 'IxMOS' and 'JxMOS' variables.
!todo: Fix this for water tiles.
!-    allocate(shd%lc%ILMOS(shd%lc%ILG), shd%lc%JLMOS(shd%lc%ILG), &
!-             shd%wc%ILMOS(shd%wc%ILG), shd%wc%JLMOS(shd%wc%ILG))
!-    shd%lc%NML = 0
!-    shd%wc%NML = 0
!-    do i = 1, shd%NA

        !> Only count active GRUs (with > 0.0 contributing fraction).
!-        if (shd%FRAC(i) > 0.0) then
!-            do m = 1, shd%lc%NTYPE

                !> Land.
!-                if (shd%lc%ACLASS(i, m) > 0.0) then
!-                    shd%lc%NML = shd%lc%NML + 1
!-                    shd%lc%ILMOS(shd%lc%NML) = i
!-                    shd%lc%JLMOS(shd%lc%NML) = m

                !> Water.
!                else
!                    shd%wc%NML = shd%wc%NML + 1
!                    shd%wc%ILMOS(shd%wc%NML) = i
!                    shd%wc%JLMOS(shd%wc%NML) = m
!-                end if
!-            end do
!-        end if
!-    end do
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
    if (ro%RUNLSS) then
        shd%lc%NTYPE = maxval(vs%tile%from_gru)
        shd%lc%NML = vs%tile%dim_length
        shd%lc%ILG = vs%tile%dim_length
        allocate(shd%lc%ILMOS(vs%tile%dim_length), source = vs%tile%from_cell)
        allocate(shd%lc%JLMOS(vs%tile%dim_length), source = vs%tile%from_gru)
        allocate(shd%lc%ACLASS(vs%grid%dim_length, maxval(vs%tile%from_gru) + 1))
        shd%lc%ACLASS = 0.0
        do k = 1, vs%tile%dim_length
            shd%lc%ACLASS(vs%tile%from_cell(k), vs%tile%from_gru(k)) = vs%tile%area_weight(k)
        end do
    end if
!<<temp

    !> Map file (diagnostic).
    if (SHDTOMAPFLAG .and. ISHEADNODE) then
        iun = 100
        open(iun, file = 'MESH_basin.map', status = 'replace', action = 'write')
        write(iun, '(a)') '#'
        write(iun, '(a)') ':Projection         ' // uppercase(trim(adjustl(shd%CoordSys%Proj)))
        write(iun, '(a)') ':Ellipsoid          ' // uppercase(trim(adjustl(shd%CoordSys%Ellips)))
        write(iun, '(a)') '#'
        write(line, '(f13.6)') shd%xOrigin
        write(iun, '(a)') ':xOrigin            ' // trim(adjustl(line))
        write(line, '(f13.6)') shd%yOrigin
        write(iun, '(a)') ':yOrigin            ' // trim(adjustl(line))
        write(iun, '(a)') '#'
        write(line, FMT_GEN) shd%xCount
        write(iun, '(a)') ':xCount             ' // trim(adjustl(line))
        write(line, FMT_GEN) shd%yCount
        write(iun, '(a)') ':yCount             ' // trim(adjustl(line))
        write(line, '(f13.6)') shd%xDelta
        write(iun, '(a)') ':xDelta             ' // trim(adjustl(line))
        write(line, '(f13.6)') shd%yDelta
        write(iun, '(a)') ':yDelta             ' // trim(adjustl(line))
        write(iun, '(a)') '#'
        write(iun, '(a)') ':contourInterval    1.000000'
        write(iun, '(a)') ':imperviousArea     0'
        write(line, FMT_GEN) (shd%lc%NTYPE + 1)
        write(iun, '(a)') ':classCount         ' // trim(adjustl(line))
        write(iun, '(a)') ':elevConversion     1.000000'
        write(iun, '(a)') '#------------------------------------------------------------------------'
        write(iun, '(a)') ':endHeader'
        write(iun, '(a)') 'Channel Elevation (ELV)'
        allocate(grid(shd%yCount, shd%xCount))
        grid = 0.0
        do n = 1, shd%NA
            grid(shd%yyy(n), shd%xxx(n)) = shd%ELEV(n)
        end do
        do y = shd%yCount, 1, -1
            write(iun, *) (grid(y, x), x = 1, shd%xCount)
        end do
        write(iun, '(a)') 'Drainage Area (FRAC)'
        grid = 0.0
        do n = 1, shd%NA
            grid(shd%yyy(n), shd%xxx(n)) = (shd%AREA(n)/shd%AL/shd%AL)*100.0
        end do
        do y = shd%yCount, 1, -1
            write(iun, *) (grid(y, x), x = 1, shd%xCount)
        end do
        write(iun, '(a)') 'Drainage direction (S)'
        grid = 0.0
        do y = 1, shd%yCount
            do x = 1, shd%xCount
                if (shd%RNKGRD(y, x) > 0) then
                    if (shd%NEXT(shd%RNKGRD(y, x)) > 0) then
                        grid(y, x) = -1
                        if (y > 1) then
                            if (x > 1) then
                                !>
                                if (shd%RNKGRD(y - 1, x - 1) == shd%NEXT(shd%RNKGRD(y, x))) grid(y, x) = 5
                            end if
                            if (x < shd%xCount) then
                                if (shd%RNKGRD(y - 1, x + 1) == shd%NEXT(shd%RNKGRD(y, x))) grid(y, x) = 3
                            end if
                            if (shd%RNKGRD(y - 1, x) == shd%NEXT(shd%RNKGRD(y, x))) grid(y, x) = 4
                        end if
                        if (y < shd%yCount) then
                            if (x > 1) then
                                if (shd%RNKGRD(y + 1, x - 1) == shd%NEXT(shd%RNKGRD(y, x))) grid(y, x) = 7
                            end if
                            if (x < shd%xCount) then
                                if (shd%RNKGRD(y + 1, x + 1) == shd%NEXT(shd%RNKGRD(y, x))) grid(y, x) = 1
                            end if
                            if (shd%RNKGRD(y + 1, x) == shd%NEXT(shd%RNKGRD(y, x))) grid(y, x) = 8
                        end if
                        if (x > 1) then
                            if (shd%RNKGRD(y, x - 1) == shd%NEXT(shd%RNKGRD(y, x))) grid(y, x) = 6
                        end if
                        if (x < shd%xCount) then
                            if (shd%RNKGRD(y, x + 1) == shd%NEXT(shd%RNKGRD(y, x))) grid(y, x) = 2
                        end if
                    end if
                end if
            end do
        end do
        do y = shd%yCount, 1, -1
            write(iun, *) (int(grid(y, x)), x = 1, shd%xCount)
        end do
        write(iun, '(a)') 'River Class (IBN)'
        grid = 0.0
        do n = 1, shd%NA
            grid(shd%yyy(n), shd%xxx(n)) = shd%IAK(n)
        end do
        do y = shd%yCount, 1, -1
            write(iun, *) (int(grid(y, x)), x = 1, shd%xCount)
        end do
        write(iun, '(a)') 'Contour Density (IROUGH)'
        grid = 0.0
        do n = 1, shd%NA
            grid(shd%yyy(n), shd%xxx(n)) = -1.0 !((intslope(n)*shd%AL)/([cintv = ]1.0) - 0.0001)
        end do
        do y = shd%yCount, 1, -1
            write(iun, *) (int(grid(y, x)), x = 1, shd%xCount)
        end do
        write(iun, '(a)') 'Channel Density (ICHNL)'
        grid = 0.0
        do n = 1, shd%NA
            grid(shd%yyy(n), shd%xxx(n)) = shd%ICHNL(n)
        end do
        do y = shd%yCount, 1, -1
            write(iun, *) (int(grid(y, x)), x = 1, shd%xCount)
        end do
        write(iun, '(a)') 'Reach Number (IREACH)'
        grid = 0.0
        do n = 1, shd%NA
            grid(shd%yyy(n), shd%xxx(n)) = shd%IREACH(n)
        end do
        do y = shd%yCount, 1, -1
            write(iun, *) (int(grid(y, x)), x = 1, shd%xCount)
        end do
        do m = 1, shd%lc%NTYPE + 1
            write(line, FMT_GEN) m
            write(iun, '(a)') 'gru' // trim(adjustl(line))
            grid = 0.0
            do n = 1, shd%NA
                grid(shd%yyy(n), shd%xxx(n)) = shd%lc%ACLASS(n, m)*100.0
            end do
            do y = shd%yCount, 1, -1
                write(iun, *) (grid(y, x), x = 1, shd%xCount)
            end do
        end do
        close(iun)
        deallocate(grid)
    end if

    !> Write information about tile configuration to file.
    if (ro%RUNLSS) then
        if (DIAGNOSEMODE) then

            !> Land tiles.
            write(line, FMT_GEN) shd%lc%NML
            call print_echo_txt('Number of land tiles (NML): ' // trim(adjustl(line)))
            if (shd%lc%NML > 0) then
                write(line, FMT_GEN) 'Tile ID', 'Grid', 'GRU', 'GRU Fraction'
                call print_echo_txt(trim(line))
                do k = 1, shd%lc%NML
                    write(line, FMT_GEN) k, shd%lc%ILMOS(k), shd%lc%JLMOS(k), vs%tile%area_weight(k)
                    call print_echo_txt(trim(line))
                end do
            end if

            !> Water tiles.
            write(line, FMT_GEN) shd%wc%NML
            call print_echo_txt('Number of water tiles (NMW): ' // trim(adjustl(line)))
            if (shd%wc%NML > 0) then
                write(line, FMT_GEN) 'Tile ID', 'Grid', 'GRU'
                call print_echo_txt(trim(line))
                do k = 1, shd%wc%NML
                    write(line, FMT_GEN) k, shd%wc%ILMOS(k), shd%wc%JLMOS(k)
                    call print_echo_txt(trim(line))
                end do
            end if
        end if

        !> Calculate active tiles in the current node.
        !> Update grid indices.
        call mpi_split_nml(inp, izero, ipid, shd%lc%NML, shd%lc%ILMOS, il1, il2, iln)
        i1 = shd%lc%ILMOS(il1)
        i2 = shd%lc%ILMOS(il2)
    end if

!todo: Update for head node to print all.
!?    if (DIAGNOSEMODE) then
!?        write(line, FMT_GEN) ipid
!?        call print_screen('Node ' // trim(adjustl(line)))
!?        write(line, FMT_GEN) il1
!?        call print_screen('First tile: ' // trim(adjustl(line)))
!?        write(line, FMT_GEN) il2
!?        call print_screen('Last tile: ' // trim(adjustl(line)))
!?        write(line, FMT_GEN) iln
!?        call print_screen('Stride: ' // trim(adjustl(line)))
!?    end if

    !> Print summary.
!-    write(line, FMT_GEN) shd%NA
!-    call print_message('Total number of grids: ' // trim(adjustl(line)))
!-    write(line, FMT_GEN) shd%NAA
!-    call print_message('Total number of grids inside the basin: ' // trim(adjustl(line)))
!-    write(line, FMT_GEN) shd%AL
!-    call print_message('Side length of grid: ' // trim(adjustl(line)) // ' m')
!-    write(line, FMT_GEN) shd%lc%NTYPE
!-    call print_message('Number of GRUs: ' // trim(adjustl(line)))
!-    write(line, FMT_GEN) shd%lc%NML
!-    call print_message('Number of land-based tiles: ' // trim(adjustl(line)))
!-    write(line, FMT_GEN) shd%NRVR
!-    call print_message('Number of river classes: ' // trim(adjustl(line)))

    !> Open and read in soil depths from file.
    if (ro%RUNLSS) then
        call READ_SOIL_LEVELS(fls, shd, ierr)
    else
        shd%lc%IGND = 1
        allocate(shd%lc%sl%DELZ(shd%lc%IGND), shd%lc%sl%ZBOT(shd%lc%IGND))
        shd%lc%sl%DELZ = 0.0
        shd%lc%sl%ZBOT = 0.0
    end if
    if (ierr /= 0) return

    !> Print a summary of levels to file.
    write(line, FMT_GEN) shd%lc%IGND
    call print_message('Number of soil layers: ' // trim(adjustl(line)))
    if (DIAGNOSEMODE) then
        write(line, FMT_GEN) 'Level', 'Thickness (m)', 'Bottom (m)'
        call print_message(line)
        do i = 1, shd%lc%IGND
            write(line, FMT_GEN) i, shd%lc%sl%DELZ(i), shd%lc%sl%ZBOT(i)
            call print_message(line)
        end do
    end if

    !> Allocate and initialize SA_MESH states.
    call model_variables_init(shd, ierr)
    if (ierr /= 0) then
        call print_error('Unable to allocate variables.')
        return
    end if

    !> Read resume configuration.
!-    call resumerun_config(fls, shd, ierr)
!-    if (ierr /= 0) then
!-        call program_abort()
!-    end if

    !> Call 'CLASSD' to initialize constants.
!todo: replace this with a non-CLASS/generic version.
    call CLASSD

    !> Read parameters from file.
    call read_parameters(fls, shd, ierr)
    if (ierr /= 0) return

!>>fews
    !> Read 'FEWS' configuration file if one exists.
    inquire(file = 'runinfo.nc', exist = ltest)
    if (ltest) then
        call reset_tab()
        call print_remark("A 'FEWS' configuration file 'runinfo.nc' exists and will override user-provided configuration.")
        call read_fews_runinfo_nc('runinfo.nc', ierr)
    end if
!<<fews

    !> Read resume configuration.
    call resumerun_config(fls, shd, ierr)
    if (ierr /= 0) then
        call program_abort()
    end if

    !> Initialize the current time-step.
!-    ic%now%year = ic%start%year
!-    ic%now%jday = ic%start%jday
!-    call julian2monthday(ic%start%jday, ic%start%year, ic%start%month, ic%start%day)
!-    ic%now%month = ic%start%month
!-    ic%now%day = ic%start%day
!-    ic%now%hour = ic%start%hour
!-    ic%now%mins = ic%start%mins

    !>
    !> READ BASIN STRUCTURES.
    !>

    if (ro%RUNGRID) then

        !> Basin structures.
        call read_basin_structures(shd, ierr)
        if (ierr /= 0) return

        !> SUBBASINFLAG.
        !>  Run only on squares that make up the watersheds listed in
        !>  the streamflow file.
        if (SUBBASINFLAG > 0) then

            !> Print message to screen.
            call reset_tab()
            call print_message('SUBBASIN mask is ACTIVE.')
            call increase_tab()

            !> Allocate and initialize local variables.
            allocate(SUBBASIN(shd%NA))
            SUBBASIN = 0

            !> Set gauge locations to 1.
            do l = 1, fms%stmg%n
                SUBBASIN(fms%stmg%meta%rnk(l)) = l
            end do
            if (DIAGNOSEMODE) then
                write(line, FMT_GEN) fms%stmg%n
                call print_message('Masking domains for ' // trim(adjustl(line)) // ' subbasins.')
            end if

            !> Mask grids upstream of gauge locations.
            i = 1
            do while (i > 0)
                i = 0
                do n = 1, shd%NAA
                    if (SUBBASIN(shd%NEXT(n)) > 0 .and. SUBBASIN(n) == 0) then
                        SUBBASIN(n) = SUBBASIN(shd%NEXT(n))
                        i = 1
                    end if
                end do
            end do

!temp
!            allocate(grid(shd%yCount, shd%xCount))
!            grid = 0
!            do n = 1, shd%NA
!                grid(shd%yyy(n), shd%xxx(n)) = SUBBASIN(n)
!            end do
!            do y = 1, shd%yCount
!                write(10, *) (grid(y, x), x = 1, shd%xCount)
!            end do
!            deallocate(grid)

            !> Set 'FRAC' to zero at inactive grids.
!?            where (SUBBASIN > 0) shd%FRAC = 0.0

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

end subroutine
