subroutine tile_connector(runoff, recharge, leakages, ncount, rofogrd, rofsgrd, rofbgrd, DELT)

    use area_watflood

    implicit none

    !> ----------------------------------------------------------------------------
    !> Parameters
    !> ----------------------------------------------------------------------------
    real(kind=4), dimension(ycount, xcount) :: runoff, recharge, leakages
    integer ncount
    real(kind=4), dimension(NA) :: rofogrd, rofsgrd, rofbgrd
    real(kind=4) DELT

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
    do i = 1, NA
        if (mod(ncount, 2) /= 0) then !Hourly time step
            runoff(YYY(i), XXX(i)) = (ROFOGRD(i) + ROFSGRD(i))*DELT
            recharge(YYY(i), XXX(i)) = ROFBGRD(i)*DELT
!+           leakages(YYY(i), XXX(i)) = 0.0*DELT !todo: determine what this should be
        else !Cumulative half-hourly time step
            runoff(YYY(i), XXX(i)) = runoff(YYY(i), XXX(i)) + (ROFOGRD(i) + ROFSGRD(i))*DELT
            recharge(YYY(i), XXX(i)) = recharge(YYY(i), XXX(i)) + ROFBGRD(i)*DELT
!+           leakages(YYY(i), XXX(i) = leakages(YYY(i), XXX(i)) + 0.0*DELT !todo: determine what this should be
        end if
    end do

end subroutine
