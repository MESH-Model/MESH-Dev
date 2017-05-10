!
! Legates, R. L., and McCabe Jr., G. J. (1999)
! Evaluating the use of "goodness-of-fit" measures in hydrologic
! and hydroclimatic model validation
!
! Water Resources Research, Vol. 35, No. 1, pp. 233-241
! 1998WR90001
! 0043-1397/99/1998WR900018$09.00
!
! MSE = N**(-1)*sum((Observed-[Model simulated])**2)
!

module legates_mse

    implicit none

    contains

    subroutine calc_mse(obs, sim, out_coeff)

        real, dimension(:), intent(in) :: obs, sim
        real, intent(out) :: out_coeff

        out_coeff = sum((obs - sim)**2)/size(obs)

    end subroutine !calc_mse

end module !legates_mse
