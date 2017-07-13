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

    use sa_mesh_shared_variables

    implicit none

    !> Input variables.
    type(ShedGridParams) :: shd

    !> Local variables.
    integer NS, l, n

    !> Read streamflow gauge location from file.
!todo: switch
    call read_streamflow_txt(shd, 22, 'MESH_input_streamflow.txt')

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
        print 1010, 'Streamflow gauge(s) are outside the basin'
        print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
        print 1020, 'GAUGE', 'Y', 'IY', 'X', 'JX'
        print 1020, repeat('-', 16), repeat('-', 16), repeat ('-', 16), repeat('-', 16), repeat ('-', 16)
        do l = 1, NS
            if (fms%stmg%rnk(l) == 0) print 1020, l, fms%stmg%y(l), fms%stmg%iy(l), fms%stmg%x(l), fms%stmg%jx(l)
        end do
        stop
    end if

    if (ro%VERBOSEMODE > 0) then
        print "(3x, 'Number of streamflow gauges: ', i3)", NS
        if (ro%DIAGNOSEMODE > 0) then
            print 1020, 'GAUGE', 'IY', 'JX', 'RANK'
            do l = 1, NS
                print 1020, l, fms%stmg%iy(l), fms%stmg%jx(l), fms%stmg%rnk(l)
            end do
        end if
    end if

1010    format(/1x, 'ERROR: ')
1020    format(3x, 9(g16.9, 1x))

end subroutine
