!
! Krause, P., Boyle, D. P., and Base, F. (2005)
! Comparison of different efficiency criteria for hydrological model
! assessment
!
! Advances in Geosciences, 5, pp. 89-97
! SRef-ID: 1680-7359/adgeo/2005-5-89
!
! Nash-Sutcliffe efficiency E
!
! E = 1 - (sum((Obs-Sim)**2))/(sum((Obs-ObsMean)**2))
!

module krause_nse

    implicit none

    contains

    subroutine calc_nse(obs, sim, out_coeff)

        real, dimension(:), intent(in) :: obs, sim
        real, intent(out) :: out_coeff

    end subroutine !calc_nse

end module !krause_nse
