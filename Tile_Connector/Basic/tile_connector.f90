!>
!> Description: Calculate daily averages from runoff for Standalone
!>              Watroute.
!>
!> Author: D.G. Princz
!>
!> Updates:
!>  Mar. 20, 2008   DGP Values are only reset on the hour and are
!>                      accumulated on the half-hour. Standalone
!>                      Watroute reads hourly input. Output values are
!>                      multiplied by DELT to convert them from
!>                      [kg m-2 s-1] to [mm] water.
!>
subroutine tile_connector(shd, ic, runoff, recharge, leakage, rofogrd, rofsgrd, rofbgrd)

    use sa_mesh_shared_variabletypes
    use model_dates

    implicit none

    !> ----------------------------------------------------------------------------
    !> Parameters
    !> ----------------------------------------------------------------------------

    !> Input variables.
    type(GridParams), intent(in) :: shd
    type(iter_counter), intent(in) :: ic
    real, dimension(shd%NA), intent(in) :: rofogrd, rofsgrd, rofbgrd

    !> Input-output variables.
    real, dimension(shd%yCount, shd%xCount) :: runoff, recharge, leakage

    !> Local variables.
    integer i

    !> Accumulate runoff from GRD to R2C grid format.
    do i = 1, shd%NA

        !> Hourly time-step.
        if (ic%now_mins == 0) then
            runoff(shd%yyy(i), shd%xxx(i)) = (rofogrd(i) + rofsgrd(i))*ic%dts
            recharge(shd%yyy(i), shd%xxx(i)) = rofbgrd(i)*ic%dts
!todo: determine what this should be
!+           leakage(shd%yyy(i), shd%xxx(i)) = roflgrd*ic%dts

        !> Sub-hourly time-step.
        else
            runoff(shd%yyy(i), shd%xxx(i)) = runoff(shd%yyy(i), shd%xxx(i)) + (rofogrd(i) + rofsgrd(i))*ic%dts
            recharge(shd%yyy(i), shd%xxx(i)) = recharge(shd%yyy(i), shd%xxx(i)) + rofbgrd(i)*ic%dts
!todo: determine what this should be
!+           leakage(shd%yyy(i), shd%xxx(i) = leakage(shd%yyy(i), shd%xxx(i)) + roflgrd*ic%dts
        end if

    end do

end subroutine
