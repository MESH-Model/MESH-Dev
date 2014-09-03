!***********************************************************************
!                                                                      *
!   A V E P R O                                                        *
!                                                                      *
!   Computes layer averages from profile data on boundaries. Also      *
!   water vapor pressure is computed                                   *
!                                                                      *
!   Input:   Parameters on layer boundaries.                           *
!            ZB    height                                              *
!            PB    pressure                                            *
!            TB    temperature                                         *
!            FB    absolute humidity                                   *
!            NMLAY number of boundaries = number of layers + 1         *
!                                                                      *
!   Output : Parameters averaged for layers                            *
!            DZ    layer extention                                     *
!            PL    pressure                                            *
!            TL    temperature                                         *
!            FL    absolute humidity                                   *
!                                                                      *
!***********************************************************************

SUBROUTINE AVEPRO(ZB, PB, TB, FB, DZ, PL, TL, FL, NMLAY)

USE PARKIND1, ONLY : JPIM, JPRM

IMPLICIT NONE

INTEGER(KIND=JPIM) :: NMLAY,I,NZL 
REAL(KIND=JPRM) :: ZB(NMLAY), PB(NMLAY), TB(NMLAY), FB(NMLAY)
REAL(KIND=JPRM) :: DZ(NMLAY-1), PL(NMLAY-1), TL(NMLAY-1), FL(NMLAY-1)
REAL(KIND=JPRM) :: xp
!---------------------------------------------------------------------------

NZL = NMLAY - 1

DO I = 1,NZL
   DZ(I) = ZB(I+1) - ZB(I)
   IF ( PB(I) == PB(I+1) ) THEN
      PL(I) = PB(I)
   ELSE
      IF (PB(I) == 0) PB(I)=1
      IF (PB(I+1) == 0) PB(I+1)=1
      XP = -LOG(PB(I+1)/PB(I)) / DZ(I)
      PL(I) = -PB(I) / XP * (EXP(-XP*DZ(I)) - 1.0) / DZ(I)
   END IF
   TL(I) = (TB(I) + TB(I+1)) / 2.0
   FL(I) = (FB(I) + FB(I+1)) / 2.0
ENDDO

END SUBROUTINE AVEPRO
