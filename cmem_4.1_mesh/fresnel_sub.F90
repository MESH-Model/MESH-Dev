SUBROUTINE FRESNEL (eps_surf)

! Purpose :
!  Fresnel Law to computes the reflectivities of flat surfaces

!  Reference:
!  Njoku and Kong, 1977: Theory for passive microwave remote sensing
!    of near-surface soil moisture.
!    Journal of Geophysical Research, Vol. 82, No. 20, 3108-3118.

! eps_surf : dielectric constant of the surface
!---------------------------------------------------------------------------

USE YOMCMEMPAR ,ONLY : sintheta, costheta, LGPRINT
USE YOMCMEMSOIL, ONLY : r_s

IMPLICIT NONE

COMPLEX :: g
COMPLEX :: eps_surf
!---------------------------------------------------------------------------

g = sqrt( eps_surf - sintheta*sintheta )

r_s(1) = abs((costheta-g)/(costheta+g)) ** 2.
r_s(2) = abs((costheta*eps_surf-g)/(costheta*eps_surf+g)) ** 2.

END SUBROUTINE FRESNEL
