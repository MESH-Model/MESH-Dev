!>
!> Description:
!>  Subroutine to read structure locations and configurations from
!>  file. Structures shared by SA_MESH are accessible by
!>  sa_mesh_shared_variables module. Other structures are accessible
!>  by their respecitve process module(s).
!>
!> Input:
!*  shd: Basin shed object, containing information about the grid
!*      definition read from MESH_drainage_database.r2c.
!>
subroutine read_basin_structures(shd)

    use strings
    use mpi_module
    use sa_mesh_shared_variables
    use model_dates
    use txt_io

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd

    !> Local variables.
    integer iun, istop, ierr, iskip, ijday1, ijday2, r, l, n, i
    character(len = 200) fname

    !> Initialize 'istop'
    istop = 0

    !>
    !> STREAMFLOW GAUGE LOCATION.
    !>

    !> File unit and name.
    fname = fms%stmg%qomeas%fls%fname
    iun = fms%stmg%qomeas%fls%iun

    !> Read location from file if channel routing is enabled.
    if (ro%RUNCHNL) then

        !> Initialize time-series.
        fms%stmg%qomeas%iyear = ic%start%year
        fms%stmg%qomeas%ijday = ic%start%jday
        fms%stmg%qomeas%ihour = ic%start%hour
        fms%stmg%qomeas%imins = ic%start%mins

        !> Read from file.
        select case (lowercase(fms%stmg%qomeas%fls%ffmt))
            case ('tb0')
                fname = trim(adjustl(fname)) // '.tb0'
                call read_streamflow_tb0(shd, iun, fname)
            case default
                fname = trim(adjustl(fname)) // '.txt'
                call read_streamflow_txt(shd, iun, fname)
        end select
    else
        fms%stmg%n = 0
    end if

    !> If locations exist.
    r = fms%stmg%n
    if (r > 0) then

        !> Find the x-y cell coordinate of the location.
        fms%stmg%meta%iy = int((fms%stmg%meta%y - shd%yOrigin)/shd%yDelta) + 1
        fms%stmg%meta%jx = int((fms%stmg%meta%x - shd%xOrigin)/shd%xDelta) + 1

        !> Find the RANK of the location.
        fms%stmg%meta%rnk = 0
        do l = 1, r
            do n = 1, shd%NA
                if (fms%stmg%meta%jx(l) == shd%xxx(n) .and. fms%stmg%meta%iy(l) == shd%yyy(n)) fms%stmg%meta%rnk(l) = n
            end do
        end do

        !> Print an error if any location has no RANK (is outside the basin).
        if (minval(fms%stmg%meta%rnk) == 0) then
            if (ipid == 0) then
                print 1010, 'Streamflow gauge(s) are outside the basin'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                print 1020, 'GAUGE', 'Y', 'IY', 'X', 'JX'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                do l = 1, r
                    if (fms%stmg%meta%rnk(l) == 0) then
                        print 1020, l, fms%stmg%meta%y(l), fms%stmg%meta%iy(l), fms%stmg%meta%x(l), fms%stmg%meta%jx(l)
                    end if
                end do
            end if
            istop = 1
        end if

        !> Skips records to present in file.
        call Julian_Day_ID(fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday, ijday1)
        call Julian_Day_ID(ic%start%year, ic%start%jday, ijday2)
        if (ijday2 < ijday1) then
            if (ipid == 0) then
                print 9994, trim(adjustl(fname)), trim(adjustl(fname)), &
                    fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday, ic%start%year, ic%start%jday
            end if
        end if
        iskip = (ijday2 - ijday1)*24/fms%stmg%qomeas%dts
        if (iskip > 0) then
            if (ipid == 0) print 9993, iskip
            ierr = read_records_txt(iun, fms%stmg%qomeas%val, iskip)
            if (ierr /= 0) then
                if (ipid == 0) print 9995, trim(adjustl(fname))
            end if
!-            do i = 1, iskip
!-                read(iun, *, iostat = ierr)
!-                if (ierr /= 0) then
!-                    if (ipid == 0) print 9995, trim(adjustl(fname))
!-                    exit
!-                end if
!-            end do
        end if

        !> Read the first record, then reposition to the first record.
!-        read(iun, *, iostat = ierr) (stas_grid%chnl%qo(fms%stmg%rnk(l)), l = 1, r)
        ierr = read_records_txt(iun, fms%stmg%qomeas%val)
        if (ierr /= 0) fms%stmg%qomeas%val = 0.0
        backspace(iun)

        !> Warn if the initial value is zero.
        if (any(fms%stmg%qomeas%val == 0.0)) then
            if (ipid == 0) print 9996, trim(adjustl(fname))
        end if
    end if

    !> Print a summary of locations to file.
    if (ipid == 0 .and. fms%stmg%n > 0) then
        if (ro%VERBOSEMODE > 0) print 9997, 'streamflow gauges', fms%stmg%n
        if (ro%DIAGNOSEMODE > 0) then
!todo: Change to write to summary file.
            print 1020, 'GAUGE', 'IY', 'JX', 'DA (km/km2)', 'RANK'
            do l = 1, fms%stmg%n
                print 1020, l, fms%stmg%meta%iy(l), fms%stmg%meta%jx(l), shd%DA(fms%stmg%meta%rnk(l)), fms%stmg%meta%rnk(l)
            end do
        end if
    end if

    !>
    !> RESERVOIR OUTLET LOCATION.
    !>

    !> File unit and name.
    fname = fms%rsvr%qorls%fls%fname
    iun = fms%rsvr%qorls%fls%iun

    !> Read location from file if reaches exist.
    if (any(shd%IREACH > 0)) then

        !> Initialize time-series.
        fms%rsvr%qorls%iyear = ic%start%year
        fms%rsvr%qorls%ijday = ic%start%jday
        fms%rsvr%qorls%ihour = ic%start%hour
        fms%rsvr%qorls%imins = ic%start%mins

        !> Read from file.
        select case (lowercase(fms%rsvr%qorls%fls%ffmt))
            case ('tb0')
                fname = trim(adjustl(fname)) // '.tb0'
                call read_reservoir_tb0(shd, iun, fname)
            case default
                fname = trim(adjustl(fname)) // '.txt'
                call read_reservoir_txt(shd, iun, fname, 2)
        end select
    else
        fms%rsvr%n = 0
    end if

    !> Print an error if no reservoirs are defined but reaches exist from the drainage database file.
    if (fms%rsvr%n == 0 .and. maxval(shd%IREACH) > 0) then
        if (ipid == 0) print 9992, trim(adjustl(fname))
        istop = 1
    end if

    !> If locations exist.
    r = fms%rsvr%n
    if (r > 0) then

        !> Find the x-y cell coordinate of the location.
        fms%rsvr%meta%iy = int((fms%rsvr%meta%y - shd%yOrigin)/shd%yDelta) + 1
        fms%rsvr%meta%jx = int((fms%rsvr%meta%x - shd%xOrigin)/shd%xDelta) + 1

        !> Find the RANK of the location.
        fms%rsvr%meta%rnk = 0
        do l = 1, r
            do n = 1, shd%NAA
                if (fms%rsvr%meta%jx(l) == shd%xxx(n) .and. fms%rsvr%meta%iy(l) == shd%yyy(n)) fms%rsvr%meta%rnk(l) = n
            end do
        end do

        !> Print an error if any location has no RANK (is outside the basin).
        if (minval(fms%rsvr%meta%rnk) == 0) then
            if (ipid == 0) then
                print 1010, 'Reservoir outlet(s) are outside the basin'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                print 1020, 'OUTLET', 'Y', 'IY', 'X', 'JX'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                do l = 1, r
                    if (fms%rsvr%meta%rnk(l) == 0) then
                        print 1020, l, fms%rsvr%meta%y(l), fms%rsvr%meta%iy(l), fms%rsvr%meta%x(l), fms%rsvr%meta%jx(l)
                    end if
                end do
            end if
            istop = 1
        end if

        !> Print an error if any outlet location has no REACH.
        do l = 1, r
            if (shd%IREACH(fms%rsvr%meta%rnk(l)) /= l) then
                if (ipid == 0) print 9991, l, fms%rsvr%meta%rnk(l), shd%IREACH(fms%rsvr%meta%rnk(l)), l
                istop = 1
            end if
        end do

        !> Initialize reservoir release values if such a type of reservoir has been defined.
        if (count(fms%rsvr%rls%b1 == 0.0) > 0) then

            !> Skips records to present in file.
            call Julian_Day_ID(fms%rsvr%qorls%iyear, fms%rsvr%qorls%ijday, ijday1)
            call Julian_Day_ID(ic%start%year, ic%start%jday, ijday2)
            if (ijday2 < ijday1) then
                if (ipid == 0) then
                    print 9994, trim(adjustl(fname)), trim(adjustl(fname)), &
                        fms%rsvr%qorls%iyear, fms%rsvr%qorls%ijday, ic%start%year, ic%start%jday
                end if
            end if
            iskip = (ijday2 - ijday1)*24/fms%rsvr%qorls%dts
            if (iskip > 0) then
                if (ipid == 0) print 9993, iskip
                ierr = read_records_txt(iun, fms%rsvr%qorls%val, iskip)
                if (ierr /= 0) then
                    if (ipid == 0) print 9995, trim(adjustl(fname))
                end if
            end if

            !> Read the first record, then reposition to the first record.
            ierr = read_records_txt(iun, fms%rsvr%qorls%val)

            !> Stop if no releases exist.
            if (ierr /= 0) then
                print 9990, trim(adjustl(fname))
                stop
            end if

            !> Reposition to the first record in the file.
            backspace(iun)
        end if
    end if

    !> Print a summary of locations to file.
    if (ipid == 0 .and. fms%rsvr%n > 0) then
        if (ro%VERBOSEMODE > 0) print 9997, 'reservoir outlets', fms%rsvr%n
        if (ro%DIAGNOSEMODE > 0) then
!todo: Change to write to summary file.
            print 1020, 'OUTLET', 'IY', 'JX', 'RANK'
            do l = 1, fms%rsvr%n
                print 1020, l, fms%rsvr%meta%iy(l), fms%rsvr%meta%jx(l), fms%rsvr%meta%rnk(l)
            end do
        end if
    end if

    !>
    !> ABSTRACTION POINT LOCATION.
    !>

    !> File unit and name.
    fname = fms%absp%sabst%fls%fname
    iun = fms%absp%sabst%fls%iun

    !> Read location from file if reaches exist.
    if (any(pm%tp%iabsp > 0)) then

        !> Initialize time-series.
        fms%absp%sabst%iyear = ic%start%year
        fms%absp%sabst%ijday = ic%start%jday
        fms%absp%sabst%ihour = ic%start%hour
        fms%absp%sabst%imins = ic%start%mins

        !> Read from file.
        select case (lowercase(fms%absp%sabst%fls%ffmt))
            case ('tb0')
                fname = trim(adjustl(fname)) // '.tb0'
                call read_abstractionpoint_tb0(shd, iun, fname)
            case default
                fname = trim(adjustl(fname)) // '.txt'
                call read_abstractionpoint_txt(shd, iun, fname)
        end select
    else
        fms%absp%n = 0
    end if

    !> Print an error if no reservoirs are defined but reaches exist from the drainage database file.
    if (fms%absp%n == 0 .and. maxval(pm%tp%iabsp) > 0) then
        if (ipid == 0) print 9989, trim(adjustl(fname))
        istop = 1
    end if

    !> If locations exist.
    r = fms%absp%n
    if (r > 0) then

        !> Find the x-y cell coordinate of the location.
        fms%absp%meta%iy = int((fms%absp%meta%y - shd%yOrigin)/shd%yDelta) + 1
        fms%absp%meta%jx = int((fms%absp%meta%x - shd%xOrigin)/shd%xDelta) + 1

        !> Find the RANK of the location.
        fms%absp%meta%rnk = 0
        do l = 1, r
            do n = 1, shd%NAA
                if (fms%absp%meta%jx(l) == shd%xxx(n) .and. fms%absp%meta%iy(l) == shd%yyy(n)) fms%absp%meta%rnk(l) = n
            end do
        end do

        !> Print an error if any location has no RANK (is outside the basin).
        if (minval(fms%absp%meta%rnk) == 0) then
            if (ipid == 0) then
                print 1010, 'Abstraction location(s) are outside the basin'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                print 1020, 'ABST. POINT', 'Y', 'IY', 'X', 'JX'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                do l = 1, r
                    if (fms%absp%meta%rnk(l) == 0) then
                        print 1020, l, fms%absp%meta%y(l), fms%absp%meta%iy(l), fms%absp%meta%x(l), fms%absp%meta%jx(l)
                    end if
                end do
            end if
            istop = 1
        end if
    end if

    !> Print a summary of locations to file.
    if (ipid == 0 .and. fms%absp%n > 0) then
        if (ro%VERBOSEMODE > 0) print 9997, 'abstraction points', fms%absp%n
        if (ro%DIAGNOSEMODE > 0) then
!todo: Change to write to summary file.
            print 1020, 'ABST. POINT', 'IY', 'JX', 'RANK'
            do l = 1, fms%absp%n
                print 1020, l, fms%absp%meta%iy(l), fms%absp%meta%jx(l), fms%absp%meta%rnk(l)
            end do
        end if
    end if

9997    format(3x, 'Number of ', (a), ': ', i5)
9996    format( &
            /3x, 'WARNING: The record at the simulation start date in ', (a), ' is zero.', &
            /8x, 'This may cause a no flow error if the channels are initialized using the observed value.')
9995    format(3x, 'WARNING: End of file reached when reading from ', (a), '.')
9994    format( &
            /3x, 'WARNING: The start date in ', (a), ' occurs after the simulation start date.', &
            /8x, 'This may cause a no flow error if the channels are initialized using the observed value.', &
            /8x, (a), ' start date:', i5, i4, &
            /8x, 'Simulation start date:', i5, i4)
9993    format(3x, 'Skipping ', i8, ' registers in the file.')
9992    format( &
            /3x, 'ERROR: Reaches exist in the drainage database file but no reservoirs are listed in ', (a), '.', &
            /8x, 'The numbers of reaches and reservoirs must match.')
9991    format( &
            /3x, 'ERROR: Reservoir ', i4, ' is not in the correct reach.', &
            /8x, 'REACH at RANK ', i8, ' is ', i4, ' but should be ', i4)
9990    format(3x, 'ERROR: End of file reached when reading from ', (a), '.')
9989    format(/3x, 'ERROR: Abstraction points are mapped in the basin but no locations are listed in ', (a), '.')

    !> Stop if there have been configuration errors.
    if (istop /= 0) stop

    !>
    !> FORMAT STATEMENTS.
    !>

1010    format(/1x, 'ERROR: ', (a))
1020    format(3x, 9(g16.9, 1x))
1030    format(3x, (a))

end subroutine
