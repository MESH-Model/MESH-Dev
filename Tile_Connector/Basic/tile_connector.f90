subroutine tile_connector(shd, runoff, recharge, leakage, ncount, rofogrd, rofsgrd, rofbgrd, delt)

    use sa_mesh_shared_variabletypes

    implicit none

    !> ----------------------------------------------------------------------------
    !> Parameters
    !> ----------------------------------------------------------------------------

    !> Input.
    type(GridParams), intent(in) :: shd
    integer, intent(in) :: ncount
    real, dimension(shd%NA), intent(in) :: rofogrd, rofsgrd, rofbgrd
    real, intent(in) :: delt

    !> Input-Output.
    real, dimension(shd%yCount, shd%xCount) :: runoff, recharge, leakage

    !> ----------------------------------------------------------------------------
    !> Declarations
    !> ----------------------------------------------------------------------------
    integer i

    !> ----------------------------------------------------------------------------
    !> Calculate averages for Watroute
    !> ----------------------------------------------------------------------------
    !> CDAN * Values are only reset on the hour and are cumulative on the
    !> CDAN * half-hour: stand-alone RTE.exe (Watroute) reads hourly data.
    !> CDAN * Output values are multiplies by delt to convert them from
    !> CDAN * [kg m-2 s-1] TO [mm] (Mar 20/08)
    do i = 1, shd%NA
        if (mod(ncount, 2) /= 0) then !Hourly time step
            runoff(shd%yyy(i), shd%xxx(i)) = (rofogrd(i) + rofsgrd(i))*delt
            recharge(shd%yyy(i), shd%xxx(i)) = rofbgrd(i)*delt
!+           leakage(shd%yyy(i), shd%xxx(i)) = roflgrd*delt !todo: determine what this should be
        else !Cumulative half-hourly time step
            runoff(shd%yyy(i), shd%xxx(i)) = runoff(shd%yyy(i), shd%xxx(i)) + (rofogrd(i) + rofsgrd(i))*delt
            recharge(shd%yyy(i), shd%xxx(i)) = recharge(shd%yyy(i), shd%xxx(i)) + rofbgrd(i)*delt
!+           leakage(shd%yyy(i), shd%xxx(i) = leakage(shd%yyy(i), shd%xxx(i)) + roflgrd*delt !todo: determine what this should be
        end if
    end do

end subroutine
