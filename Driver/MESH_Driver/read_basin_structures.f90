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

    use mpi_module
    use sa_mesh_shared_variables
    use FLAGS
    use model_dates

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd

    !> Local variables.
    integer NS, NR, iskip, ijday1, ijday2
    integer iun, ierr, l, n, i
    character(len = 200) fname

    !>
    !> STREAMFLOW GAUGE LOCATION.
    !>

    !> File unit and name.
!todo: replace file unit and name from fls object.
    iun = 22
    fname = 'MESH_input_streamflow'

    !> Read location from file.
    select case (STREAMFLOWFILEFLAG)
        case ('tb0')
            fname = trim(adjustl(fname)) // '.tb0'
            call read_streamflow_tb0(shd, iun, fname)
        case default
            fname = trim(adjustl(fname)) // '.txt'
            call read_streamflow_txt(shd, iun, fname)
    end select

    !> If locations exist.
    NS = fms%stmg%n
    if (NS > 0) then

        !> Find the x-y cell coordinate of the location.
        fms%stmg%iy = int((fms%stmg%y - shd%yOrigin)/shd%yDelta) + 1
        fms%stmg%jx = int((fms%stmg%x - shd%xOrigin)/shd%xDelta) + 1

        !> Find the RANK of the location.
        fms%stmg%rnk = 0
        do l = 1, NS
            do n = 1, shd%NA
                if (fms%stmg%jx(l) == shd%xxx(n) .and. fms%stmg%iy(l) == shd%yyy(n)) fms%stmg%rnk(l) = n
            end do
        end do

        !> Print an error if any location has no RANK (is outside the basin).
        if (minval(fms%stmg%rnk) == 0) then
            if (ipid == 0) then
                print 1010, 'Streamflow gauge(s) are outside the basin'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                print 1020, 'GAUGE', 'Y', 'IY', 'X', 'JX'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                do l = 1, NS
                    if (fms%stmg%rnk(l) == 0) print 1020, l, fms%stmg%y(l), fms%stmg%iy(l), fms%stmg%x(l), fms%stmg%jx(l)
                end do
            end if
            ierr = 1
        end if

        !> Print a summary of locations to file.
        if (ipid == 0 .and. ro%VERBOSEMODE > 0) then
            print 9997, 'streamflow gauges', NS
            if (ro%DIAGNOSEMODE > 0) then
!todo: Change to write to summary file.
                print 1020, 'GAUGE', 'IY', 'JX', 'RANK'
                do l = 1, NS
                    print 1020, l, fms%stmg%iy(l), fms%stmg%jx(l), fms%stmg%rnk(l)
                end do
            end if
        end if

        !> Skips records to present in file.
        call Julian_Day_ID(fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday, ijday1)
        call Julian_Day_ID(ic%start%year, ic%start%jday, ijday2)
        if (ijday2 < ijday1) then
            if (ipid == 0) print 9994, fname, fname, fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday, ic%start%year, ic%start%jday
        end if
        iskip = (ijday2 - ijday1)*24/fms%stmg%qomeas%dts
        if (iskip > 0) then
            if (ipid == 0) print 9993, iskip
            do i = 1, iskip
                read(iun, *, iostat = ierr)
                if (ierr /= 0) then
                    if (ipid == 0) print 9995, fname
                    exit
                end if
            end do
        end if

        !> Read the first record, then reposition to the first record.
        read(iun, *, iostat = ierr) (stas_grid%chnl%qo(fms%stmg%rnk(l)), l = 1, NS)
        backspace(iun)

        !> Warn if the initial value is zero.
        do l = 1, NS
            if (stas_grid%chnl%qo(fms%stmg%rnk(l)) == 0.0) then
                if (ipid == 0) print 9996, fname
                exit
            end if
        end do
    end if

    !>
    !> RESERVOIR OUTLET LOCATION.
    !>

    !> File unit and name.
!todo: replace file unit and name from fls object.
    iun = 21
    fname = 'MESH_input_reservoir'

    !> Read location from file.
!todo: switch
    fname = trim(adjustl(fname)) // '.txt'
    call read_reservoir_txt(shd, iun, fname, 2)

    !> If locations exist.
    NR = fms%rsvr%n
    if (NR > 0) then

        !> Find the x-y cell coordinate of the location.
        fms%rsvr%iy = int((fms%rsvr%y - shd%yOrigin)/shd%yDelta) + 1
        fms%rsvr%jx = int((fms%rsvr%x - shd%xOrigin)/shd%xDelta) + 1

        !> Find the RANK of the location.
        fms%rsvr%rnk = 0
        do l = 1, NR
            do n = 1, shd%NAA
                if (fms%rsvr%jx(l) == shd%xxx(n) .and. fms%rsvr%iy(l) == shd%yyy(n)) fms%rsvr%rnk(l) = n
            end do
        end do

        !> Print an error if any location has no RANK (is outside the basin).
        if (minval(fms%rsvr%rnk) == 0) then
            if (ipid == 0) then
                print 1010, 'Reservoir outlet(s) are outside the basin'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                print 1020, 'OUTLET', 'Y', 'IY', 'X', 'JX'
                print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
                do l = 1, NR
                    if (fms%rsvr%rnk(l) == 0) print 1020, l, fms%rsvr%y(l), fms%rsvr%iy(l), fms%rsvr%x(l), fms%rsvr%jx(l)
                end do
            end if
            ierr = 1
        end if

        !> Print a summary of locations to file.
        if (ipid == 0 .and. ro%VERBOSEMODE > 0) then
            print 9997, 'reservoir outlets', NR
            if (ro%DIAGNOSEMODE > 0) then
!todo: Change to write to summary file.
                print 1020, 'OUTLET', 'IY', 'JX', 'RANK'
                do l = 1, NR
                    print 1020, l, fms%rsvr%iy(l), fms%rsvr%jx(l), fms%rsvr%rnk(l)
                end do
            end if
        end if

        !> Print an error if no reservoirs are defined but reaches exist from the drainage database file.
        if (fms%rsvr%n == 0 .and. maxval(shd%IREACH) > 0) then
            if (ipid == 0) print 9992, fname
            ierr = 1
        end if

        !> Print an error if any outlet location has no REACH.
        do l = 1, NR
            if (shd%IREACH(fms%rsvr%rnk(l)) /= l) then
                if (ipid == 0) print 9991, l, fms%rsvr%rnk(l), shd%IREACH(fms%rsvr%rnk(l)), l
                ierr = 1
            end if
        end do
    end if

9997    format(3x, 'Number of ', (a), ': ', i5)
9996    format( &
            /3x, 'WARNING: The record at the simulation start date in ', (a), ' is zero.', &
            8x, 'This may cause a no flow error if the channels are initialized using the observed value.')
9995    format(3x, 'WARNING: End of file reached when reading from ', (a), '.')
9994    format( &
            /3x, 'WARNING: The start date in ', (a), ' occurs after the simulation start date.', &
            8x, 'This may cause a no flow error if the channels are initialized using the observed value.', &
            8x, (a), ' start date:', i5, i4, &
            8x, 'Simulation start date:', i5, i4)
9993    format(3x, 'Skipping ', i8, ' registers in the file.')
9992    format( &
            /3x, 'ERROR: Reaches exist in the drainage database file but no reservoirs are listed in ', (a), '.', &
            8x, 'The numbers of reaches and reservoirs must match.')
9991    format( &
            /3x, 'ERROR: Reservoir ', i4, ' is not in the correct reach.', &
            8x, 'REACH at RANK ', i8, ' is ', i4, ' but should be ', i4)

    !> Stop if there have been configuration errors.
    if (ierr /= 0) stop

    !>
    !> FORMAT STATEMENTS.
    !>

1010    format(/1x, 'ERROR: ', (a))
1020    format(3x, 9(g16.9, 1x))
1030    format(3x, (a))

end subroutine
