      SUBROUTINE DIASURF(UZ,VZ,TZ,QZ,NI,U,V,TG,QG,Z0,Z0T,ILMO,ZA,
     1                  H,UE,FTEMP,FVAP,ZU,ZT,LAT,F,IL1,IL2,JL)

      IMPLICIT NONE
      INTEGER NI
      REAL ZT(NI),ZU(NI)
      REAL UZ(NI),VZ(NI),TZ(NI),QZ(NI),ZA(NI),U(NI),V(NI)
      REAL TG(NI),QG(NI),UE(NI),FTEMP(NI),FVAP(NI)
      REAL ILMO(NI),Z0T(NI),Z0(NI),H(NI),F(NI)
      REAL LAT(NI)
*Author
*          Yves Delage  (Aug1990)
*
*Revision
* 001      G. Pellerin(JUN94)
*          Adaptation to new surface formulation
* 002      B. Bilodeau (Nov 95) - Replace VK by KARMAN
* 003      R. Sarrazin (Jan 96) - Prevent problems if zu < za
* 004      G. Pellerin (Feb 96) - Rewrite stable formulation
* 005      Y. Delage and B. Bilodeau (Jul 97) - Cleanup
* 006      Y. Delage (Feb 98) - Addition of HMIN
* 007      Y. Delage (Sept 00) - Change UE2 by UE
*                              - Introduce log-linear profile for near-
*                                 neutral cases
* 008      D. Verseghy (Nov 02) - Remove unused constant CLM 
*                                 from common block SURFCON
* 009      M. Mackay (Nov 04) - Change all occurrences of ALOG
*                               to LOG for greater portability.
* 010      F. SeglenieKs (Mar 05) - Declare LAT as REAL*8 for
*                                   consistency
* 011      P.Bartlett (Mar 06) - Set HI to zero for unstable case
* 012      E.Chan (Nov 06) - Bracket entire subroutine loop with 
*                            IF(F(J).GT.0.)
* 013      D.Verseghy (Nov 06) - Convert LAT to regular precision
*
*Object
*          to calculate the diagnostic values of U, V, T, Q
*          near the surface (ZU and ZT)
*
*Arguments
*
*          - Output -
* UZ       U component of the wind at Z=ZU
* VZ       V component of the wind at Z=ZU
* TZ       temperature in kelvins at Z=ZT
* QZ       specific humidity at Z=ZT
*
*          - Input -
* NI       number of points to process
* U        U component of wind at Z=ZA
* V        V component of wind at Z=ZA
* TG       temperature at the surface (Z=0) in Kelvins
* QG       specific humidity
* PS       surface pressure at the surface
* ILMO     inverse of MONIN-OBUKHOV lenth
* H        height of boundary layer
* UE       friction velocity
* Z0       roughness lenth for winds
* Z0T      roughness lenth for temperature and moisture
* FTEMP    temperature flux at surface
* FVAP     vapor flux at surface
* ZA       heights of first model level above ground
* ZU       heights for computation of wind components
* ZT       heights for computation of temperature and moisture
* LAT      LATITUDE
* F        Fraction of surface type being studied 

      REAL ANG,ANGI,VITS,LZZ0,LZZ0T
      REAL CT,DANG,CM,ANGMAX
      REAL X,X0,Y,Y0,FH,FM
      REAL RAC3
      SAVE ANGMAX
      INTEGER J,IL1,IL2,JL
*

      REAL AS,ASX,CI,BS,BETA,FACTN,HMIN
      COMMON / SURFCON / AS,ASX,CI,BS,BETA,FACTN,HMIN
      REAL DELTA,GRAV,KARMAN,CPD
      COMMON / PHYCON / DELTA,GRAV,KARMAN,CPD

      real a,b,c,d,psi,z
      real unsl,hi


************************************************************************
**  fonctions de couche de surface pour le cas stable                 **
************************************************************************
*
      d  (unsl) = 4*AS*BETA*unsl
      c  (hi)   = d(unsl)*hi - hi**2
      b  (hi)   = d(unsl) - 2*hi
      a  (z,hi) = sqrt(1 + b(hi)*z - c(hi)*z**2)
      psi(z,hi) = 0.5 * (a(z,hi)-z*hi-log(1+b(hi)*z*0.5+a(z,hi))-
     +            b(hi)/(2*sqrt(c(hi)))*asin((b(hi)-2*c(hi)*z)/d(unsl)))
*
*   Limites de validite: unsl >= 0 (cas stable ou neutre)
*                        c > 0 (hi < d)
*                        z*hi < 1
*   Ces 2 dernieres conditions imposees a l'aide du facteur 'factn'
*
*   Reference :  Y. Delage, BLM, 82 (p23-48) (Eq.33-37)
************************************************************************

      DATA ANGMAX /0.85/
      RAC3=SQRT(3.)

      DO 10 J=IL1,IL2
      IF(F(J).GT.0.0)                                     THEN

      LZZ0T=LOG((ZT(J)+Z0(J))/Z0T(J))
      LZZ0=LOG(ZU(J)/Z0(J)+1)
      IF(ILMO(J).LE.0.) THEN
*---------------------------------------------------------------------
*                      UNSTABLE CASE
           Y=(1-BETA*CI*(ZT(J)+Z0(J))*ILMO(J))**(1./3)
           Y0=(1-BETA*CI*Z0T(J)*ILMO(J))**(1./3)
           FH=BETA*(LZZ0T+1.5*LOG((Y0**2+Y0+1)/(Y**2+Y+1))+RAC3*
     1        ATAN(RAC3*2*(Y-Y0)/((2*Y0+1)*(2*Y+1)+3)))
           X=(1-BETA*CI*(ZU(J)+Z0(J))*ILMO(J))**(1./6)
           X0=(1-BETA*CI*Z0(J)*ILMO(J))**(1./6)
           FM=LZZ0+LOG((X0+1)**2*SQRT(X0**2-X0+1)*(X0**2+X0+1)**1.5
     1               /((X+1)**2*SQRT(X**2-X+1)*(X**2+X+1)**1.5))
     2              +RAC3*ATAN(RAC3*((X**2-1)*X0-(X0**2-1)*X)/
     3              ((X0**2-1)*(X**2-1)+3*X*X0))
           HI=0.
      ELSE
*---------------------------------------------------------------------
*                        STABLE CASE
           unsl=ilmo(j)
        hi=1/MAX(HMIN,H(J),(ZA(J)+10*Z0(J))*factn,factn/d(ILMO(J)))
           fh=BETA*(LZZ0T+min(psi(ZT(J)+Z0(J),hi)-psi(Z0T(J),hi),
     1                        ASX*ILMO(J)*(ZT(J)+Z0(J)-Z0T(J))))
           fm=LZZ0+min(psi(zu(J)+Z0(J),hi)-psi(Z0(J),hi),
     1                 ASX*ILMO(J)*ZU(J))
      ENDIF
*---------------------------------------------------------------------
      CT=KARMAN/FH
      CM=KARMAN/FM
      TZ(J)=TZ(J)+F(J)*(TG(J)-FTEMP(J)/(CT*UE(J))-GRAV/CPD*ZT(J))
      QZ(J)=QZ(J)+F(J)*(QG(J)-FVAP(J)/(CT*UE(J)))
      VITS=UE(J)/CM

* CALCULATE WIND DIRECTION CHANGE FROM TOP OF SURFACE LAYER
      DANG= (ZA(J)-ZU(J))*HI*ANGMAX*SIN(LAT(J))
      ANGI=ATAN2(V(J),SIGN(ABS(U(J))+1.e-05,U(J)))
      IF(ILMO(J).GT.0.)    THEN
         ANG=ANGI+DANG
      ELSE
         ANG=ANGI
      ENDIF

      UZ(J)=UZ(J)+F(J)*VITS*COS(ANG)
      VZ(J)=VZ(J)+F(J)*VITS*SIN(ANG)

      ENDIF
   10 CONTINUE

      RETURN
      END
