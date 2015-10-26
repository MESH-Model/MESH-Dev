subroutine tile_connector(bi, runoff, recharge, leakage, ncount, rofogrd, rofsgrd, rofbgrd, delt)

!    use area_watflood
    use sa_mesh_shared_variabletypes

    implicit none

    !> ----------------------------------------------------------------------------
    !> Parameters
    !> ----------------------------------------------------------------------------

    !> Input.
    type(basin_info), intent(in) :: bi
    integer, intent(in) :: ncount
    real, dimension(bi%NA), intent(in) :: rofogrd, rofsgrd, rofbgrd
    real, intent(in) :: delt

    !> Input-Output.
    real, dimension(bi%yCount, bi%xCount) :: runoff, recharge, leakage

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
    do i = 1, bi%NA
        if (mod(ncount, 2) /= 0) then !Hourly time step
            runoff(bi%yyy(i), bi%xxx(i)) = (rofogrd(i) + rofsgrd(i))*delt
            recharge(bi%yyy(i), bi%xxx(i)) = rofbgrd(i)*delt
!+           leakage(bi%yyy(i), bi%xxx(i)) = roflgrd*delt !todo: determine what this should be
        else !Cumulative half-hourly time step
            runoff(bi%yyy(i), bi%xxx(i)) = runoff(bi%yyy(i), bi%xxx(i)) + (rofogrd(i) + rofsgrd(i))*delt
            recharge(bi%yyy(i), bi%xxx(i)) = recharge(bi%yyy(i), bi%xxx(i)) + rofbgrd(i)*delt
!+           leakage(bi%yyy(i), bi%xxx(i) = leakage(bi%yyy(i), bi%xxx(i)) + roflgrd*delt !todo: determine what this should be
        end if
    end do

end subroutine
