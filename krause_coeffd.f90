!
! Krause, P., Boyle, D. P., and Base, F. (2005)
! Comparison of different efficiency criteria for hydrological model
! assessment
!
! Advances in Geosciences, 5, pp. 89-97
! SRef-ID: 1680-7359/adgeo/2005-5-89
!
! Coefficient of determination r**2
! r2 = 
!       ((sum(
!           (Obs-ObsMean)*(Sim-SimMean)))/
!               (sqrt(sum((Obs-ObsMean)**2))*
!                   sqrt(sum((Sim-SimMean)**2))))**2
!

module krause_coeffd

    implicit none

    contains

    subroutine calc_coeffd(obs, sim, out_coeff)

        real, dimension(:), intent(in) :: obs, sim
        real, intent(out) :: out_coeff

    end subroutine !calc_coeffd_weighted

end module !krause_coeffd
