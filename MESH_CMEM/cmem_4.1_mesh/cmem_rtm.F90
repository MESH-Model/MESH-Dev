SUBROUTINE CMEM_RTM

! Purpose :
! -------
!   Compute top-of-vegetation brightness temperatures

! Author :
! ------
!     T. Holmes   ECMWF     2/10/06
!------------------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM
USE YOMLUN   , ONLY : NULOUT
USE YOMCMEMSOIL, ONLY : r_r, tb_soil
USE YOMCMEMVEG, ONLY : tb_veg, tau_veg 
USE YOMCMEMATM, ONLY : tb_ad, tb_tov
USE YOMCMEMPAR ,ONLY : LGPRINT

IMPLICIT NONE

INTEGER(KIND=JPIM) :: i
!------------------------------------------------------------------------------

DO i = 1,2    ! h- and v-polarization
  tb_tov(i) =  tb_soil(i) * exp(-tau_veg(i))           &
             + tb_ad * r_r(i) * exp(-2.*tau_veg(i))    &
             + tb_veg(i)                               &
             + tb_veg(i) * r_r(i) * exp(-tau_veg(i)) 
ENDDO

IF (LGPRINT) WRITE(NULOUT,*) '--- Veg TBH:',tb_veg(1)+tb_veg(1) * r_r(1) * exp(-tau_veg(1))

IF (LGPRINT) WRITE(NULOUT,*) '--- TB tov:',tb_tov

END SUBROUTINE CMEM_RTM
