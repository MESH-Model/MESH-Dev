!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer,
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer
!version 3 or (at your option) any later version that should be found at:
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software;
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec),
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
!**S/P PHASE_CHANGES
!
      SUBROUTINE AGG_PHASES ( TRNCH, VEGH, VEGL, PSNVH, PSNG, &
                              WF, WDTT, WFT,  &
                              WDTTG, WDTTV, WFTG, WFTV, &
                              TPSOIL, TPSOILV, CAP, & 
                              N )
!
!-----------------------------------------------------------------------------------------

      use sfc_options
      use svs_configs
      implicit none
#include <arch_specific.hf>
!
!    declarations of arguments
      INTEGER N,TRNCH
      REAL VEGH(N), VEGL(N), PSNVH(N), PSNG(N)
      REAL WDTTG(N,NL_SVS), WDTTV(N,NL_SVS)
      REAL WFTG(N,NL_SVS),  WFTV(N,NL_SVS)
      REAL WF(N,NL_SVS), WDTT(N,NL_SVS), WFT(N,NL_SVS)
      REAL TPSOIL(N,NL_SVS),TPSOILV(N,NL_SVS), CAP(N,NL_SVS)
!
!--------------------------------------------------------
!Author
!          N. Gauthier (November 2016)
!
!Object
!          Aggregate soil freeze/thaw variable over 3 different types of surfaces
!          (Snow, Vegetation & Bare Ground)
!
!Arguments
!
!            - Input -
! VEGH       High vege fraction
! VEGL       Low vege fraction
! PSNVH      fraction of HIGH vegetation covered by snow
! PSNG       fraction of bare ground or low veg. covered by snow
! WF         soil volumetric ice content (m3/m3)
! WDT        soil volumetric water content in soil layers (m3/m3)
! WFT        soil volumetric ice content (m3/m3)
! DELWATGR
! DELWATVG
! DELICEGR
! DELICEVG
!
!            - Output -
! WDTT       Aggregated Variable
!
!--------------------------------------------------------

include "thermoconsts.inc"

!     declarations of local variables 
      INTEGER I, K

      REAL, DIMENSION(N,NL_SVS) :: EXCES

      REAL RHOW
      DATA RHOW  / 1000. /          ! (kg m-3)     Water density

      EXCES = 0.


      DO I=1,N
         DO K=1,NL_SVS  

            WDTT(I,K) = ( 1. - VEGH(I) - VEGL(I) + PSNG(I)*VEGL(I) ) * WDTTG(I,K)  & 
                      + (      VEGH(I) + VEGL(I) - PSNG(I)*VEGL(I) ) * WDTTV(I,K)   
!
            WFT(I,K)  = ( 1. - VEGH(I) - VEGL(I) + PSNG(I)*VEGL(I) )  * WFTG(I,K)  &
                      + (      VEGH(I) + VEGL(I) - PSNG(I)*VEGL(I) )  * WFTV(I,K)
            
            IF(WFT(I,K)>0.0.AND.WFT(I,K)<1.0E-6)THEN
               WDTT  (I,K) = WDTT(I,K) + WFT(I,K)
               EXCES(I,K)  = EXCES(I,K) + WFT(I,K)
               WFT  (I,K)  = 0.0
               TPSOIL (I,K) = TPSOIL(I,K) - EXCES(I,K)*CHLF*RHOW/CAP(I,K) 
               TPSOILV (I,K) = TPSOILV(I,K) - EXCES(I,K)*CHLF*RHOW/CAP(I,K) 
            ENDIF
            
         ENDDO
      ENDDO
!

RETURN
END SUBROUTINE AGG_PHASES
