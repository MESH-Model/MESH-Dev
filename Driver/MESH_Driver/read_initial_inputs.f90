subroutine READ_INITIAL_INPUTS(shd, ts, cm, fls)

    use mpi_module
    use sa_mesh_shared_variables
    use model_files_variables
    use FLAGS
    use climate_forcing

    use RUNCLASS36_save_output
    use RUNSVS113_variables
    use RUNLAKE_module ! included by LAM, 12-JUL-2017

    implicit none

    !> The types that contain allocatable values
    type(ShedGridParams) :: shd
    type(CLIM_INFO) :: cm
    type(dates_model) :: ts
    type(fl_ids):: fls

    !> Local variables.
    integer NA, NAA, NTYPE, NML, NSL, ierr, k, n, i, m, j
    character(len = 100), dimension(:), allocatable :: list_errors, list_warnings

    !>
    !> RUN OPTIONS.
    !>  Run options are read at the beginning of the run from
    !>  MESH_input_run_options.ini.
    !>
    call READ_RUN_OPTIONS(ts, cm, fls)

    !> Open the status file after reading run options, in case MODELINFOOUTFLAG
    !> has been set to zero.
    if (ipid == 0 .and. MODELINFOOUTFLAG > 0) then
        open(58, file = './' // trim(fls%GENDIR_OUT) // '/MESH_output_echo_print.txt')
    end if

    !>
    !> DRAINAGE DATABASE.
    !>

    if (SHDFILEFLAG == 1) then

        open(fls%fl(mfk%f20)%iun, file=adjustl(trim(fls%fl(mfk%f20)%fn)), status='old', iostat=ierr)
            if (ierr == 0) then
                close(fls%fl(mfk%f20)%iun)
                if (ro%VERBOSEMODE > 0) then
                    print *, 'Reading Drainage Database from MESH_drainage_database.r2c'
                end if
                call READ_SHED_EF(fls, mfk%f20, shd)
                if (ro%VERBOSEMODE > 0) then
                    print *, ' READ: SUCCESSFUL, FILE: CLOSED'
                end if
            else
                print "(1x, 'ERROR: Opening ', (a))", adjustl(trim(fls%fl(mfk%f20)%fn))
                stop
            end if

    else if (SHDFILEFLAG == 0) then

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
!        read(20, '(i5, 50x, i5)') NA, NAA
!        read(20, '(f10.0, 5x, 2i5)') AL, NRVR, NTYPE
!        GRDN = 0.0
!        GRDE = 0.0

!todo change the code or documentation on these variables.
!ANDY Set ILG from the read-in values
!        ILG = NA*NTYPE

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
!          GRDN = AL/1000.0
!          GRDE = AL/1000.0
!        end if
!        read(20, '(2i5)') YCOUNT, XCOUNT

!        do i = 1, YCOUNT
!          read(20, *)
!        end do

!        do i = 1, NA
!          read(20, '(5x, 2i5, 3f10.5, i7, 5i5, f5.2, 15f5.2)') YYY(i),
!     &      XXX(i), WF_DA(i), WF_BNKFLL(i), WF_CHANNELSLOPE(i),
!     &      WF_ELEV(i), WF_IBN(i), WF_IROUGH(i), WF_ICHNL(i),
!     &      WF_NEXT(i), WF_IREACH(i), FRAC(i),
!     &      (ACLASS(i, j), j = 1, NTYPE)
!> check to make sure land cover areas sum to 100%
!          WF_LAND_COUNT = 1
!          WF_LAND_MAX = 0.0
!          WF_LAND_SUM = 0.0
!          do j = 1, NTYPE
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

    !> Point run with no routing.
    else if (SHDFILEFLAG == 2) then

        !> Assign no projection or grid properties.
        shd%CoordSys%Proj = 'none'; shd%CoordSys%Ellips = 'none'; shd%CoordSys%Zone = 'none'
        shd%xOrigin = 0.0; shd%xDelta = 1.0; shd%xCount = 1; shd%jxMin = 0; shd%jxMax = 1; shd%GRDE = 1.0
        shd%yOrigin = 0.0; shd%yDelta = 1.0; shd%yCount = 1; shd%iyMin = 0; shd%iyMax = 1; shd%GRDN = 1.0
        shd%AL = 1.0
        shd%NA = 1; shd%NAA = 1; shd%lc%NTYPE = 1; shd%NRVR = 0

        !> Allocate and initialize grid variables.
        allocate( &
            shd%xxx(shd%NA), shd%yyy(shd%NA), &
            shd%NEXT(shd%NA), &
            shd%SLOPE_INT(shd%NA), &
            shd%AREA(shd%NA), shd%FRAC(shd%NA), &
            shd%lc%ACLASS(shd%NA, shd%lc%NTYPE + 1), stat=ierr)
        shd%xxx = 1; shd%yyy = 1
        shd%NEXT = 0
        shd%SLOPE_INT = 1.0E-5
        shd%AREA = 1.0; shd%FRAC=shd%AREA/shd%AL/shd%AL
        shd%lc%ACLASS(:, shd%lc%NTYPE) = 1.0; shd%lc%ACLASS(:, shd%lc%NTYPE + 1) = 0.0

        !> Force 'RUNMODE noroute' (overrides the run option).
        ro%RUNCHNL = .false.
        ro%RUNGRID = .false.

    end if

    !> Check maximum number of cells and outlets, and print a warning if an adjustment is made
    if (shd%NA /= maxval(shd%NEXT)) then
        print 172
        shd%NA = maxval(shd%NEXT)
    end if
    if (shd%NAA /= (maxval(shd%NEXT) - count(shd%NEXT == 0))) then
        print 173
        shd%NAA = maxval(shd%NEXT) - count(shd%NEXT == 0)
    end if

172     format(1x, 'WARNING: Total number of grid adjusted to maximum of RANK. Consider adjusting the input files.')
173     format(1x, 'WARNING: The number of outlets is adjusted to the number of grids where NEXT is zero.', &
               /3x, 'Consider adjusting the input files.')

    !> Assign shd values to local variables.
    NA = shd%NA
    NAA = shd%NAA

    !> Allocate temporary message variables.
    allocate(list_errors(6*NAA), list_warnings(1*NAA))
    list_errors = ''; list_warnings = ''

    !> Check for values that might be incorrect, but are unlikely to stop the model.
    if (ro%RUNCHNL) then
        forall (n = 1:NAA, shd%NEXT(n) <= n) list_warnings(n) = 'NEXT might be upstream of RANK'
    end if

    !> Write warning messages to screen.
    if (any(len_trim(list_warnings) > 0) .and. ipid == 0) then
        print "(/1x, 'WARNING: Errors might exist in the drainage database: ', (a))", adjustl(trim(fls%fl(mfk%f20)%fn))
        do i = 1, size(list_warnings)
            if (len_trim(list_warnings(i)) > 0) print "(3x, 'WARNING: ', (a), ' at RANK ', i8)", &
                adjustl(trim(list_warnings(i))), (i - int(i/NAA)*NAA)
        end do
    end if

    !> Check for values that will likely stop the model.
    if (ro%RUNCHNL) then
        forall (n = 1:NAA, shd%SLOPE_CHNL(n) <= 0) list_errors(n) = 'Invalid or negative channel slope'
        forall (n = 1:NAA, shd%CHNL_LEN(n) <= 0.0) list_errors(NAA + n) = 'Invalid or negative channel length'
        forall (n = 1:NAA, shd%AREA(n) <= 0.0) list_errors(2*NAA + n) = 'Invalid or negative grid area'
        forall (n = 1:NAA, shd%DA(n) <= 0.0) list_errors(3*NAA + n) = 'Invalid or negative drainage area'
    end if
    forall (n = 1:NA, shd%xxx(n) == 0) list_errors(4*NAA + n) = 'Invalid x-direction placement'
    forall (n = 1:NA, shd%yyy(n) == 0) list_errors(5*NAA + n) = 'Invalid y-direction placement'
!+   forall (n = 1:NAA, shd%SLOPE_INT(n) <= 0.0) list_errors(4*NAA + n) = 'Invalid or negative interior slope'

    !> Write error messages to screen.
    if (any(len_trim(list_errors) > 0) .and. ipid == 0) then
        print "(/1x, 'ERROR: Errors have been found in the drainage database: ', (a))", adjustl(trim(fls%fl(mfk%f20)%fn))
        do i = 1, size(list_errors)
            if (len_trim(list_errors(i)) > 0) print "(3x, 'ERROR: ', (a), ' at RANK ', i8)", &
                adjustl(trim(list_errors(i))), (i - int(i/NAA)*NAA)
        end do
        stop
    end if

    !> Deallocate temporary message variables.
    deallocate(list_errors, list_warnings)

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
    allocate(shd%ylat(NA), shd%xlng(NA))
    do i = 1, NA
        !LATLENGTH = shd%AL/1000.0/(111.136 - 0.5623*cos(2*(DEGLAT*PI/180.0)) + 0.0011*cos(4*(DEGLAT*PI/180.0)))
        !LONGLENGTH = shd%AL/1000.0/(111.4172*cos((DEGLAT*PI/180.0)) - 0.094*cos(3*(DEGLAT*PI/180.0)) + 0.0002*cos(5*(DEGLAT*PI/180.0)))
        shd%ylat(i) = (shd%yOrigin + shd%yDelta*shd%yyy(i)) - shd%yDelta/2.0
        shd%xlng(i) = (shd%xOrigin + shd%xDelta*shd%xxx(i)) - shd%xDelta/2.0
    end do

    !> If no land surface scheme active.
    if (.not. RUNCLASS36_flgs%PROCESS_ACTIVE .and. .not. RUNSVS113_flgs%PROCESS_ACTIVE) then
        shd%lc%NTYPE = 1
        if (allocated(shd%lc%ACLASS)) deallocate(shd%lc%ACLASS)
        allocate(shd%lc%ACLASS(shd%NA, shd%lc%NTYPE + 1))
        shd%lc%ACLASS(:, shd%lc%NTYPE) = 1.0
        shd%lc%ACLASS(:, shd%lc%NTYPE + 1) = 0.0
    end if

    !> Compute the maximum number of tile elements.
    shd%lc%ILG = shd%NA*shd%lc%NTYPE
    shd%wc%ILG = shd%NA*shd%lc%NTYPE

    !> Determine the number of active tile elements.
!todo: fix this.
    NTYPE = shd%lc%NTYPE
!    allocate(shd%lc%ILMOS(shd%lc%ILG), shd%lc%JLMOS(shd%lc%ILG), &
!             shd%wc%ILMOS(shd%wc%ILG), shd%wc%JLMOS(shd%wc%ILG))
    allocate(shd%lc%ILMOS(shd%lc%ILG), shd%lc%JLMOS(shd%lc%ILG))

    !> Count the number of tiles that are land 'lc' or water 'wc' and
    !> store the respective ID's of the grid and GRU in the 'ILMOS' and
    !> 'JLMOS' variables.
    shd%lc%NML = 0
!    shd%wc%NML = 0
    do i = 1, NA
        if (shd%FRAC(i) > 0.0) then
            do m = 1, NTYPE

                !> Only count active GRUs (with > 0.0 contributing fraction).
!todo: fix this.
                if (shd%lc%ACLASS(i, m) > 0.0) then
!                    if (shd%IAK(i) > 0) then

                        !> Land.
                        shd%lc%NML = shd%lc%NML + 1
                        shd%lc%ILMOS(shd%lc%NML) = i
                        shd%lc%JLMOS(shd%lc%NML) = m

!                    else

                        !> Water.
!                        shd%wc%NML = shd%wc%NML + 1
!                        shd%wc%ILMOS(shd%wc%NML) = i
!                        shd%wc%JLMOS(shd%wc%NML) = m

!                    end if
                end if

            end do
        end if
    end do

    !> Write information about tile configuration to file.
    if (ipid == 0 .and. MODELINFOOUTFLAG > 0 .and. ro%DIAGNOSEMODE > 0) then

        !> Land tiles.
        write(58, 1210) 'land', 'NML', shd%lc%NML
        if (shd%lc%NML > 0) then
            write(58, 1910) 'Index', 'Grid', 'GRU'
            do k = 1, shd%lc%NML
                write(58, 1910) k, shd%lc%ILMOS(k), shd%lc%JLMOS(k)
            end do
        end if

        !> Water tiles.
        write(58, 1210) 'water', 'NMW', shd%wc%NML
        if (shd%wc%NML > 0) then
            write(58, 1910) 'Index', 'Grid', 'GRU'
            do k = 1, shd%wc%NML
                write(58, 1910) k, shd%wc%ILMOS(k), shd%wc%JLMOS(k)
            end do
        end if

    end if

1210    format(/1x, 'Configuration of ', (a), ' tiles', &
               /1x, 'Total number (', (a), '): ', g16.9)
1910    format(3(g16.9))

    !> Store the number of active tile elements to initialize variables.
    NML = shd%lc%NML

    !> Calculate the operational indices in the current node.
    call mpi_split_nml(inp, izero, ipid, NML, shd%lc%ILMOS, il1, il2)
    if (ro%DIAGNOSEMODE > 0) print 1062, ipid, NML, ((il2 - il1) + 1), il1, il2

1062    format(/1x, 'Configuration and distribution of the domain', &
               /3x, 'Current process: ', i10, &
               /3x, 'Tile land elements: ', i10, &
               /3x, 'Length of single array: ', i10, &
               /3x, 'Starting index: ', i10, &
               /3x, 'Stopping index: ', i10, /)

    !> Assign grid-based indices.
    i1 = shd%lc%ILMOS(il1)
    i2 = shd%lc%ILMOS(il2)

    !> Open and read in soil depths from file.
    call READ_SOIL_LEVELS(shd, fls)
    if (ro%VERBOSEMODE > 0) then
        print *, 'IGND = ', shd%lc%IGND
    end if

    !> Store the number of soil layers to initialize variables.
    NSL = shd%lc%IGND

    !> Allocate and initialize SA_MESH states.
    call stas_init(stas, 'tile', NML, NSL, ierr)
    call stas_init(stas_grid, 'grid', NA, NSL, ierr)
    call stas_init(stas_gru, 'gru', NTYPE, NSL, ierr)

    !> Call 'CLASSD' to initialize constants.
!todo: replace this with a non-CLASS/generic version.
    call CLASSD

    !> Read parameters from file.
    call read_parameters(fls, shd, cm, ierr)

    call READ_LAKE_PROPORTION(shd) ! included by LAM, 12-JUL-2017

    !> Read variable states from file.
    call read_initial_states(fls, shd, ierr)

    !> Check the grid output points.
!todo: fix this.
    if (ipid == 0) then
        do i = 1, WF_NUM_POINTS

            !> Check that output grid points aren't repeated and that the
            !> output directories exist.
            if (i < WF_NUM_POINTS) then
                do j = i + 1, WF_NUM_POINTS
                    if (op%N_OUT(i) == op%N_OUT(j) .and. op%II_OUT(i) == op%II_OUT(j)) then
                        print *
                        print *, 'Output for Grid ', op%N_OUT(i), ' and GRU ', &
                                 op%II_OUT(i), ' is repeated in grid output point: ', j
                        print *, 'Please adjust this grid output ', &
                                 'point in MESH_input_run_options.ini.'
                        stop
                    end if
                end do
            else
                open(17, file = './' // trim(adjustl(op%DIR_OUT(i))) // '/fort.17', status = 'unknown', iostat = ierr)
                if (ierr /= 0) then
                    print *
                    print *, 'Grid output point ', i
                    print *, 'The output directory does not exist: ' // trim(adjustl(op%DIR_OUT(i)))
                    print *, 'Please adjust this grid output point ', &
                             'in MESH_input_run_options.ini or create the ', &
                             'folder.'
                    stop
                else
                    close(17, status = 'delete')
                end if
            end if

            !> Check that grid output points are in the basin.
            if (op%N_OUT(i) > shd%NA) then
                write(6, *)
                write(6, *)
                write(6, *) 'Grids from basin watershed file: ', shd%NA
                write(6, *) 'Grid output point ', i, ' is in Grid: ', op%N_OUT(i)
                write(6, *) 'Please adjust this grid output point in ', 'MESH_input_run_options.ini'
                stop
            end if
        end do
    end if

    !> Distribute the starting date of the forcing files.
    do n = 1, cm%nclim
        cm%dat(n)%start_date%year = cm%start_date%year
        cm%dat(n)%start_date%jday = cm%start_date%jday
        cm%dat(n)%start_date%hour = cm%start_date%hour
        cm%dat(n)%start_date%mins = cm%start_date%mins
    end do

    !> Set the starting date from the forcing files if none is provided.
    if (ic%start%year == 0 .and. ic%start%jday == 0 .and. ic%start%hour == 0 .and. ic%start%mins == 0) then
        ic%start%year = cm%start_date%year
        ic%start%jday = cm%start_date%jday
        ic%start%hour = cm%start_date%hour
        ic%start%mins = cm%start_date%mins
    end if

    !> Initialize the current time-step.
    ic%now%year = ic%start%year
    ic%now%jday = ic%start%jday
    call julian2monthday(ic%now%jday, ic%now%year, ic%now%month, ic%now%day)
    ic%now%hour = ic%start%hour
    ic%now%mins = ic%start%mins

    !>
    !> READ BASIN STRUCTURES.
    !>

!-    call read_basin_structures(shd)

end subroutine
