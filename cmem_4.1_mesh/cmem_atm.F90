SUBROUTINE CMEM_ATM

! Purpose :
! -------
! Compute Atmospheric contribution to the microwave emission
   
! Interface :
! ---------

! Method :
! ------
! Externals :
! ---------
!  IO_CMEMGRIBEX

! Internal variables
! ------------------
! Authors :
!     Patricia de Rosnay  Feb 2008, January 2009

!------------------------------------------------------------------------------

USE PARKIND1, ONLY : JPIM, JPRM
USE YOMLUN , ONLY : NULOUT

USE YOMCMEMPAR, ONLY : CIATM, LGPRINT,nobs_atm
USE YOMCMEMFIELDS, ONLY: N_CMEM, JJ, ftair, ftau_atm, ftb_au,ftb_ad
USE YOMCMEMATM, ONLY : fZ, fs_tatm,fs_spres,fs_R,t_atm,p_atm,z_atm,r_atm,rh_atm &
                     & ,tb_au,tb_ad,tau_atm,tair,spres, twv,to2,Z


IMPLICIT NONE
INTEGER(KIND=JPIM) :: i


!------------------------------------------------------------------------------

  DO JJ = 1, N_CMEM
    
    tau_atm = 0._JPRM
    tb_au = 0._JPRM
    tb_ad = 0._JPRM
    tair = ftair (JJ)

    SELECT CASE (CIATM)

      CASE ( 'Pellarin' )

        Z = fZ(JJ)
        CALL ATMPELLARIN

      CASE ( 'Liebe' )

        DO i = 1, nobs_atm
          t_atm(i) = fs_tatm(JJ,i)
          rh_atm(i) = fs_R(JJ,i)
          !z_atm(i) = 5._JPRM + REAL(i-1.,JPRM) * 100._JPRM
        ENDDO

        ! surface pressure to pressure profile
        spres = 2.716_JPRM ** (fs_spres(JJ))
        CALL ec_p60l
        twv = 1._JPRM
        to2 = 1._JPRM
        CALL ATMLIEBE


      CASE ( 'Ulaby' )

        CALL ATMULABY

    END SELECT

    ftau_atm(JJ) = tau_atm
    ftb_au(JJ) = tb_au
    ftb_ad(JJ) = tb_ad


  ENDDO


IF (LGPRINT) WRITE(NULOUT,*) 'End of CMEM Atmospheric Module'

       
END SUBROUTINE CMEM_ATM

