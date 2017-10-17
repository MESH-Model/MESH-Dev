!     #########
!     NEED TO DECLARE WITHIN A MODULE CALLED BY SNOWES FOR THE assumed shape array to work i.e., arrays with : instead of hard-coded dimension
!     By using a module, both snowES and this subroutine are compiled at the same time and the info about dimensions is passed
!
module surface_ri_snowES_mod

  private


  public :: surface_ri_snowES

contains


    SUBROUTINE SURFACE_RI_SNOWES(PTG, PQS, PEXNS, PEXNA, PTA, PQA,   &
                               PZREF, PUREF, PDIRCOSZW, PVMOD, PRI )  
!   ######################################################################
!
!!****  *SURFACE_RI*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the richardson number near the ground
!         
!     
!!**  METHOD
!!    ------
!
!
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
!!    MODD_GROUND_PAR
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
!!      Original    22/09/98 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS_SNOWES,     ONLY : XRV, XRD, XG
!
!
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)    :: PTG      ! surface temperature
REAL, DIMENSION(:), INTENT(IN)    :: PQS      ! surface specific humidity
REAL, DIMENSION(:), INTENT(IN)    :: PEXNS    ! surface exner function
REAL, DIMENSION(:), INTENT(IN)    :: PTA      ! temperature at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PQA      ! specific humidity
                                              ! at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PEXNA    ! exner function
                                              ! at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PVMOD    ! module of the horizontal wind
!
REAL, DIMENSION(:), INTENT(IN)    :: PZREF    ! reference height of the first
                                              ! atmospheric level
REAL, DIMENSION(:), INTENT(IN)    :: PUREF    ! reference height of the wind
!                                             ! NOTE this is different from ZZREF
!                                             ! ONLY in stand-alone/forced mode,
!                                             ! NOT when coupled to a model (MesoNH)
REAL, DIMENSION(:), INTENT(IN)    :: PDIRCOSZW! Cosine of the angle between
!                                             ! the normal to the surface and
!                                             ! the vertical
!
REAL, DIMENSION(:), INTENT(OUT)   :: PRI      ! Richardson number
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PTG))   :: ZTHVA, ZTHVS
REAL, DIMENSION(SIZE(PVMOD)) :: ZVMOD

REAL   :: ZRIMAX

ZRIMAX = 0.2
!-------------------------------------------------------------------------------
!
!       1.     Richardson number
!              -----------------
!                
!                                                 virtual potential        
!                                                 temperature at the 
!                                                 first atmospheric level and
!                                                 at the surface
!
!
ZTHVA(:)=PTA(:)/PEXNA(:)*( 1.+(XRV/XRD-1.)*PQA(:) )   
ZTHVS(:)=PTG(:)/PEXNS(:)*( 1.+(XRV/XRD-1.)*PQS(:) )
!                                                 
ZVMOD(:) = MAX(PVMOD(:) , 0.1 * MIN(10.,PUREF(:)) )
!
                                                ! Richardson's number
PRI(:) = XG * PDIRCOSZW(:) * PUREF(:) * PUREF(:)              &
          * (ZTHVA(:)-ZTHVS(:)) / (0.5 * (ZTHVA(:)+ZTHVS(:)) )  &
          / (ZVMOD(:)*ZVMOD(:)) /PZREF(:)  
!
PRI(:) = MIN(PRI(:),ZRIMAX)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURFACE_RI_SNOWES

end module surface_ri_snowES_mod
