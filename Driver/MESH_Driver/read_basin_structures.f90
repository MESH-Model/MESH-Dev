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
    integer NS, iun, ierr, l, n, i, iskip, ijday1, ijday2
    character(len = 200) fname

    !>
    !> STREAMFLOW GAUGE OUTLETS.
    !>

    !> File unit and name.
!todo: replace file unit and name from fls object.
    iun = 22
    fname = 'MESH_input_streamflow'

    !> Read streamflow gauge location from file.
    select case (STREAMFLOWFILEFLAG)
        case ('tb0')
            fname = trim(adjustl(fname)) // '.tb0'
            call read_streamflow_tb0(shd, iun, fname)
        case default
            fname = trim(adjustl(fname)) // '.txt'
            call read_streamflow_txt(shd, iun, fname)
    end select

    !> Find the x-y cell coordinate of the gauge location.
    fms%stmg%iy = int((fms%stmg%y - shd%yOrigin)/shd%yDelta) + 1
    fms%stmg%jx = int((fms%stmg%x - shd%xOrigin)/shd%xDelta) + 1

    !> Find the RANK of the gauge location.
    NS = fms%stmg%n
    fms%stmg%rnk = 0
    do l = 1, NS
        do n = 1, shd%NA
            if (fms%stmg%jx(l) == shd%xxx(n) .and. fms%stmg%iy(l) == shd%yyy(n)) fms%stmg%rnk(l) = n
        end do
    end do

    !> Print an error if any gauge location has no RANK.
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
        stop
    end if

    !> Print a summary of gauge locations to file.
    if (ipid == 0 .and. ro%VERBOSEMODE > 0) then
        print 9997, NS
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
        print 9994, fname, fname, fms%stmg%qomeas%iyear, fms%stmg%qomeas%ijday, ic%start%year, ic%start%jday
    end if
    iskip = (ijday2 - ijday1)*24/fms%stmg%qomeas%dts
    print 9993, iskip
    do i = 1, iskip
        read(iun, *, iostat = ierr)
        if (ierr /= 0) then
            print 9995, fname
            exit
        end if
    end do

    !> Read the first record, then reposition to the first record.
    read(iun, *, iostat = ierr) (stas_grid%chnl%qo(fms%stmg%rnk(l)), l = 1, NS)
    backspace(iun)

    !> Warn if the initial value is zero.
    do l = 1, NS
        if (stas_grid%chnl%qo(fms%stmg%rnk(l)) == 0.0) print 9996, fname
        exit
    end do

9997    format(3x, 'Number of streamflow gauges: ', i5)
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

    !>
    !> FORMAT STATEMENTS.
    !>

1010    format(/1x, 'ERROR: ', (a))
1020    format(3x, 9(g16.9, 1x))
1030    format(3x, (a))

end subroutine
