!     #########
!     NEED TO DECLARE WITHIN A MODULE CALLED BY SNOWES FOR THE assumed shape array to work i.e., arrays with : instead of hard-coded dimension
!     By using a module, both snowES and this subroutine are compiled at the same time and the info about dimensions is passed
!
module surface_aero_cond_snowES_mod

  private


  public :: surface_aero_cond_snowES

contains



!   ######################################################################
    SUBROUTINE SURFACE_AERO_COND_SNOWES(PRI, PZREF, PUREF, PVMOD, PZ0,&
                                     PZ0H, PAC, PRA, PCH           ) 
!   ######################################################################
!
!!****  *SURFACE_AERO_COND*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the drag coefficients for heat and momentum near the ground
!         
!     
!!**  METHOD
!!    ------
!
!
!
!    1 and 2 : computation of relative humidity near the ground
!
!    3 : richardson number
!
!    4 : the aerodynamical resistance for heat transfers is deduced
!
!    5 : the drag coefficient for momentum ZCD is computed
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/01/98 
!!                  02/04/01 (P Jabouille) limitation of Z0 with 0.5 PUREF
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS_SNOWES,ONLY : XKARMAN
!
USE MODE_THERMOS_SNOW
!
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)    :: PRI      ! Richardson number
REAL, DIMENSION(:), INTENT(IN)    :: PVMOD    ! module of the horizontal wind
REAL, DIMENSION(:), INTENT(IN)    :: PZREF    ! reference height of the first
                                              ! atmospheric level
REAL, DIMENSION(:), INTENT(IN)    :: PUREF    ! reference height of the wind
                                              ! NOTE this is different from ZZREF
                                              ! ONLY in stand-alone/forced mode,
                                              ! NOT when coupled to a model (MesoNH)
REAL, DIMENSION(:), INTENT(IN)    :: PZ0      ! roughness length for momentum
REAL, DIMENSION(:), INTENT(IN)    :: PZ0H     ! roughness length for heat
!
REAL, DIMENSION(:), INTENT(OUT)   :: PAC      ! aerodynamical conductance
REAL, DIMENSION(:), INTENT(OUT)   :: PRA      ! aerodynamical resistance
REAL, DIMENSION(:), INTENT(OUT)   :: PCH      ! drag coefficient for heat
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PRI)) :: ZZ0, ZZ0H, ZMU,          &
                               ZFH, ZCHSTAR, ZPH, ZCDN, &
                               ZSTA, ZDI, ZWORK1, ZWORK2, ZWORK3 
REAL, DIMENSION(SIZE(PRI)) :: ZVMOD
!
INTEGER                    :: JJ
!
! Functions:
REAL :: X, CHSTAR, PH
CHSTAR(X) = 3.2165 + 4.3431*X + 0.5360*X*X - 0.0781*X*X*X
PH    (X) = 0.5802 - 0.1571*X + 0.0327*X*X - 0.0026*X*X*X
!
!-------------------------------------------------------------------------------
!
!*       4.     Surface aerodynamic resistance for heat transfers
!               -------------------------------------------------
!
!  minimum value for exchange coefficients computations : 1m/s / 10m
ZVMOD = MAX(PVMOD , 0.1 * MIN(10.,PUREF) )
!
DO JJ=1,SIZE(PRI)

  ZZ0(JJ)  = MIN(PZ0(JJ),PUREF(JJ)*0.5)
  ZZ0H(JJ) = MIN(ZZ0(JJ),PZ0H(JJ))
  ZZ0H(JJ) = MIN(ZZ0H(JJ),PZREF(JJ)*0.5)
!
  ZWORK1(JJ)=LOG( PUREF(JJ)/ZZ0(JJ) )
  ZWORK2(JJ)=PZREF(JJ)/ZZ0H(JJ)
  ZWORK3(JJ)=ZVMOD(JJ)*ZVMOD(JJ)

  ZMU(JJ) = MAX( LOG( ZZ0(JJ)/ZZ0H(JJ) ), 0.0 )
  ZFH(JJ) = ZWORK1(JJ) / LOG(ZWORK2(JJ))
!
  ZCHSTAR(JJ) = CHSTAR(ZMU(JJ))
  ZPH(JJ)     = PH(ZMU(JJ))
!
! 
  ZCDN(JJ) = (XKARMAN/ZWORK1(JJ))**2.

 !print *, 'Karman',XKARMAN
 !print *, 'Work 1',ZWORK1(JJ)
!
!
  ZSTA(JJ) = PRI(JJ)*ZWORK3(JJ)
!
!
  IF ( PRI(JJ) < 0.0 ) THEN
    ZDI(JJ) = 1. / ( ZVMOD(JJ)                                  &
                   +ZCHSTAR(JJ)*ZCDN(JJ)*15.                         &
                                *ZWORK2(JJ)**ZPH(JJ)  &
                                *ZFH(JJ) * SQRT(-ZSTA(JJ))           &
                  ) 
    PAC(JJ) = ZCDN(JJ)*(ZVMOD(JJ)-15.* ZSTA(JJ)*ZDI(JJ))*ZFH(JJ)
    !if(pac(JJ).EQ.0.0) THEN
  !     print *, '----------PRI < 0.0 -------------------------'
  !     print *, 'for jj=',jj,' pac=',pac(jj!)
   !    print *,'zcdn',zcdn(jj),' zvmod',zvmod(jj),' zsta',zsta(jj)
   !    print *, 'zdi',zdi(jj),'  zfh',zfh(jj)
    !endif

  ELSE
    ZDI(JJ) = SQRT(ZWORK3(JJ) + 5. * ZSTA(JJ) )
    PAC(JJ) = ZCDN(JJ)*ZVMOD(JJ)/(1.+15.*ZSTA(JJ)*ZDI(JJ)  &
             / ZWORK3(JJ) /ZVMOD(JJ) )*ZFH(JJ)   

   ! print *, '-------------PRI > 0.0 ----------------------'
   !    print *, 'for jj=',jj,' pac=',pac(jj)
   !    print *,'zcdn',zcdn(jj),' zvmod',zvmod(jj),' zsta',zsta(jj)
   !    print *, 'zdi',zdi(jj),'  zfh',zfh(jj),' zwork3',zwork3(jj)

 
  ENDIF
!
  
!  print *, 'PZREF',PZREF,' PUREF',PUREF
!  print *, 'PVMOD',PVMOD,' PZ0H', PZ0H, 'PZ0', PZ0
!  print *,'PRI',PRI,' PAC', PAC
  PRA(JJ) = 1. / PAC(JJ)
!
  PCH(JJ) = 1. / (PRA(JJ) * ZVMOD(JJ))
!
ENDDO
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURFACE_AERO_COND_SNOWES
end module surface_aero_cond_snowES_mod
