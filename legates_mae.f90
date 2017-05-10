!
! Legates, R. L., and McCabe Jr., G. J. (1999)
! Evaluating the use of "goodness-of-fit" measures in hydrologic
! and hydroclimatic model validation
!
! Water Resources Research, Vol. 35, No. 1, pp. 233-241
! 1998WR90001
! 0043-1397/99/1998WR900018$09.00
!
! MAE = N**(-1)*sum(abs(Observed-[Model simulated]))
!

module legates_mae

    implicit none

    contains

    subroutine calc_mae(obs, sim, out_coeff)

        real, dimension(:), intent(in) :: obs, sim
        real, intent(out) :: out_coeff

        out_coeff = sum(abs(obs - sim))/size(obs)

    end subroutine !calc_mae

end module !legates_mae
