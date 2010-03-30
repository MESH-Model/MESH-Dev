***S/P  FLXSURFZ
*
      SUBROUTINE FLXSURFZ(CDM, CDH, CTU, RIB, FTEMP, FVAP, ILMO,
     X                    UE, FCOR, TA , QA , ZU, ZT, VA,
     Y                    TG , QG , H , Z0 , Z0T,
     %                    LZZ0, LZZ0T, FM, FH,N,IL1,IL2,FI,ITER,JL )
      IMPLICIT NONE
      INTEGER N,IL1,IL2,ITER(N),JL
      REAL CDM(N),CDH(N),CTU(N),RIB(N),FCOR(N),ILMO(N)
      REAL FTEMP(N),FVAP(N),TA(N),QA(N),ZU(N),VA(N)
      REAL TG(N),QG(N),H(N),Z0(N),UE(N),ZT(N)
      REAL Z0T(N),LZZ0(N),LZZ0T(N)
      REAL fm(N),fh(N)
      REAL FI(N)
*
*Author
*          Y.Delage (Jul 1990)
*Revision
* 001      G. Pellerin (Jun 94) New function for unstable case
* 002      G. Pellerin (Jui 94) New formulation for stable case
* 003      B. Bilodeau (Nov 95) Replace VK by KARMAN
* 004      M. Desgagne (Dec 95) Add safety code in function ff
*                               and ensures that RIB is non zero
* 005      R. Sarrazin (Jan 96) Correction for H
* 006      C. Girard (Nov 95) - Diffuse T instead of Tv
* 007      G. Pellerin (Feb 96) Revised calculation for H (stable)
* 008      G. Pellerin (Feb 96) Remove corrective terms to CTU
* 009      Y. Delage and B. Bilodeau (Jul 97) - Cleanup
* 010      Y. Delage (Feb 98) - Addition of HMIN
* 011      Y. Delage (Sept 00) - Set top of surface layer at ZU +Z0
*                              - Output UE instead of UE**2
*                              - Initialise ILMO and H
*                              - Change iteration scheme for stable case
*                              - Introduce log-linear profile for near-
*                                 neutral stable cases
*                              - set VAMIN inside flxsurf
* 012      Y. Delage (Oct 00) - Input of wind and temperatur/humidity
*                                at different levels
* 013      D. Verseghy (Nov 02) - Add calculation of CDH
* 014      D. Verseghy (Nov 04) - Pass in JL for troubleshooting
* 015      V. Fortin (Jun 06) - Modify calculations of LZZ0 and LZZ0T
*                               to avoid numerical problems with log
* 016      D. Verseghy (Jun 06) - Loop over IL1-IL2 instead of 1-N
* 017      J.P. Paquin (Aug 08) - "Synchronization" with flxsurf3
*                               - Insert code from stabfunc2.cdk (v4.5)
*                               - VAMIN=2.5 m/s to be coherent with ISBA  
*                                 (temporary measure VAMIN should be added
*                                  to physics constants)
*                                 (changes implemented by L.Duarte on Oct 08)
* 018      R. Harvey   (Oct 08) - Add fractional subregion cover FI
*                                 (in addition to ITER) to control
*                                 calculations over relevant points.
*
*Object
*          to calculate surface layer transfer coefficients and fluxes
*          FLXSURFZ is a variant of FLXSURF3 that permits to input
*          wind and temperature (humidity) at different levels
*
*Arguments
*
*          - Output -
* CDM      transfer coefficient of momentum squared
* CTU      transfer coefficient of temperature times UE
* RIB      bulk Richardson number
* FTEMP    temperature flux
* FVAP     vapor flux
* ILMO     (1/length of Monin-Obukov)
* UE       friction velocity 
* H        height of the boundary layer
* FM       momentum stability function
* FH       heat stability function
* LZZ0     log ((zu+z0)/z0)
* LZZ0T    log ((zt+z0)/z0t)
*          - Input -
* FCOR     Coriolis factor
* ZU       height of wind input
* ZT       height of temperature and humidity input
* TA       potential temperature at first predictive level above surface
* QA       specific humidity     "    "      "        "      "     "
* VA       wind speed            "    "      "        "      "     "
* TG       surface temperature
* QG       specific humidity at the surface
* Z0       roughness length for momentum      flux calculations
* Z0T      roughness length for heat/moisture flux calculations
* N        horizontal dimension
*
*Notes
*          SEE DELAGE AND GIRARD BLM 58 (19-31)
*                "       BLM 82 (23-48)
*
*     DIVERSES CONSTANTES PHYSIQUES 
*
      REAL AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX,CLM
      REAL DELTA,GRAV,KARMAN,CPD
      COMMON / PHYCON / DELTA,GRAV,KARMAN,CPD
      COMMON / CLASSD2 / AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
      INTEGER J
      INTEGER IT,ITMAX
      REAL HMAX,CORMIN,EPSLN
      REAL RAC3,CM,CT,ZP
      REAL F,G,DG,X,X0,Y,Y0
      REAL UNSH,HE,HS
      REAL DTHV,TVA,TVS
      REAL HL,VAMIN,U
      REAL CS
      REAL ZB,DD,ILMOX
      REAL DF,ZZ
      SAVE HMAX,CORMIN,EPSLN
      SAVE ITMAX
      SAVE VAMIN
*
      DATA CORMIN, HMAX /0.7E-4 ,  1500.0/
      DATA ITMAX / 3 /
* Modification de la valeur de VAMIN de 0.1 a
* 2.5 pour fitter avec ce qui est dans ISBA
*      DATA VAMIN /0.1/
      DATA VAMIN / 2.5 /
* 
      DATA EPSLN / 1.0e-05 /
*
c      real a,b,c,d,psi,z
      real unsl,hi
************************************************************************
**  fonctions de couche de surface pour le cas stable                 **
************************************************************************
*
c      d  (unsl) = 4*AS*BETA*unsl
c      c  (hi)   = d(unsl)*hi - hi**2
c      b  (hi)   = d(unsl) - 2*hi
c      a  (z,hi) = sqrt(1 + b(hi)*z - c(hi)*z**2)
c      psi(z,hi) = 0.5 * (a(z,hi)-z*hi-log(1+b(hi)*z*0.5+a(z,hi))-
c     +            b(hi)/(2*sqrt(c(hi)))*asin((b(hi)-2*c(hi)*z)/d(unsl)))
*
*   Limites de validite: unsl >= 0 (cas stable ou neutre)
*                        c > 0 (hi < d)
*                        z*hi < 1
*   Ces 2 dernieres conditions imposees a l'aide du facteur 'factn'
*
*   Reference :  Y. Delage, BLM, 82 (p23-48) (Eq.33-37)
************************************************************************
      DF(ZZ)=(1-ZZ*UNSH)*sqrt(1+(4*AS*BETA*ilmo(j))*ZZ/(1-ZZ*UNSH))
*
      RAC3=sqrt(3.0)
      CS=AS*2.5
*
      DO 1 J=IL1,IL2
      IF(FI(J).GT.0. .AND. ITER(J).EQ.1) THEN
*
*  CALCULATE THE RICHARDSON NUMBER
        ZP=ZU(J)**2/(ZT(J)+Z0(J)-Z0T(J))
        u=max(vamin,va(j))
        tva=(1+DELTA*QA(J))*TA(J)
        tvs=(1+DELTA*QG(J))*TG(J)
        dthv=tva-tvs
        RIB(J)=GRAV/(tvs+0.5*dthv)*ZP*dthv/u**2
        if (rib(j).ge.0.0) rib(j) = max(rib(j), EPSLN)
        if (rib(j).lt.0.0) rib(j) = min(rib(j),-EPSLN)
*
*  FIRST APPROXIMATION TO ILMO
        LZZ0(J)=LOG(Z0(J)+ZU(J))-LOG(Z0(J))
        LZZ0T(J)=LOG(ZT(J)+Z0(J))-LOG(Z0T(J))
        IF(RIB(J).GT.0.)  THEN
           FM(J)=LZZ0(J)+CS*RIB(J)/max(2*z0(j),1.0)
           FH(J)=BETA*(LZZ0T(J)+CS*RIB(J))/
     1           max(sqrt(z0(j)*z0t(j)),1.0)
           ILMO(J)=RIB(J)*FM(J)**2/(ZP*FH(J))
           F=MAX(ABS(FCOR(J)),CORMIN)
           H(J)=BS*sqrt(KARMAN*u/(ILMO(J)*F*fm(j)))
        ELSE
           FM(J)=LZZ0(J)-min(0.7+log(1-rib(j)),LZZ0(J)-1)
           FH(J)=BETA*(LZZ0T(J)-min(0.7+log(1-rib(j)),LZZ0T(J)-1))
        ENDIF
        ILMO(J)=RIB(J)*FM(J)**2/(ZP*FH(J))
      ENDIF
    1 CONTINUE
* - - - - - - - - -  BEGINNING OF ITERATION LOOP - - - - - - - - - - - 
      DO 35 IT=1,ITMAX
      DO 35 J=IL1,IL2
      IF(FI(J).GT.0. .AND. ITER(J).EQ.1) THEN
        u=max(vamin,va(j))
        ZP=ZU(J)**2/(ZT(J)+Z0(J)-Z0T(J))
        IF(RIB(J).GT.0.)  THEN
*----------------------------------------------------------------------
*  STABLE CASE
       	ILMO(J)=max(EPSLN,ILMO(J))
        hl=(ZU(J)+10*Z0(J))*FACTN
        F=MAX(ABS(FCOR(J)),CORMIN)
        hs=BS*sqrt(KARMAN*u/(ILMO(J)*F*fm(j)))
        H(J)=MAX(HMIN,hs,hl,factn/(4*AS*BETA*ILMO(J)))
        UNSH=1/H(J)
        unsl=ILMO(J)
*CDIR IEXPAND
        fm(J)=LZZ0(J)+psi(ZU(J)+Z0(J),unsh,unsl)-psi(Z0(J),unsh,unsl)
*CDIR IEXPAND
        fh(J)=BETA*(LZZ0T(J)+psi(ZT(J)+Z0(J),unsh,unsl)-psi(Z0T(J),unsh,
     %unsl))
        DG=-ZP*FH(J)/FM(J)**2*(1+beta*(DF(ZT(J)+Z0(J))-DF(Z0T(J)))/
     1          (2*FH(J))-(DF(ZU(J)+Z0(J))-DF(Z0(J)))/FM(J))
*----------------------------------------------------------------------
*  UNSTABLE CASE
      ELSE
        ILMO(J)=MIN(0.,ILMO(J))
*CDIR IEXPAND
c         X=(1-CI*(ZU(J)+Z0(J))*BETA*ILMO(J))**(1./6)
c         X0=(1-CI*Z0(J)*BETA*ILMO(J))**(1./6.)
c         FM(J)=LZZ0(J)+LOG((X0+1)**2*SQRT(X0**2-X0+1)*(X0**2+X0+1)**1.5
c     %               /((X+1)**2*SQRT(X**2-X+1)*(X**2+X+1)**1.5))
c     %              +RAC3*ATAN(RAC3*((X**2-1)*X0-(X0**2-1)*X)/
c     %              ((X0**2-1)*(X**2-1)+3*X*X0))
         FM(J)=fmi(zu(j)+z0(j),z0 (j),lzz0 (j),ilmo(j),x,x0)
*CDIR IEXPAND
c         Y=(1-CI*(ZT(J)+Z0(J))*BETA*ILMO(J))**(1./3)
c         Y0=(1-CI*Z0T(J)*BETA*ILMO(J))**(1./3)
c         FH(J)=BETA*(LZZ0T(J)+1.5*LOG((Y0**2+Y0+1)/(Y**2+Y+1))+RAC3*
c     %        ATAN(RAC3*2*(Y-Y0)/((2*Y0+1)*(2*Y+1)+3)))
         FH(J)=fhi(zt(j)+z0(j),z0t(j),lzz0t(j),ilmo(j),y,y0)
         DG=-ZP*FH(J)/FM(J)**2*(1+beta/FH(J)*(1/Y-1/Y0)-2/FM(J)*
     %                (1/X-1/X0))
      ENDIF
*----------------------------------------------------------------------
      IF(IT.LT.ITMAX) THEN
             G=RIB(J)-FH(J)/FM(J)**2*ZP*ILMO(J)
             ILMO(J)=ILMO(J)-G/DG
      ENDIF
      ENDIF
   35 CONTINUE
* - - - - - -  - - - END OF ITERATION LOOP - - - - - - - - - - - - - -
      DO 80 J=IL1,IL2
      IF(FI(J).GT.0. .AND. ITER(J).EQ.1) THEN
        u=max(vamin,va(j))
*----------------------------------------------------------------------
*  CALCULATE ILMO AND STABILITY FUNCTIONS FROM LOG-LINEAR PROFILE
        zb=zu(j)/(zt(j)+z0(j)-z0t(j))
        dd=(beta*lzz0t(j)*zb)**2-4*rib(j)*asx*lzz0(j)*beta*
     1        (lzz0t(j)*zb-lzz0(j))
        if(rib(j).gt.0..and.rib(j).lt.beta/asx.and.dd.ge.0.) then
         ilmox=(beta*lzz0t(j)*zb-2*rib(j)*asx*lzz0(j)-sqrt(dd))
     1          /(2*zu(j)*(asx**2*rib(j)-beta*asx))
         if(ilmox.lt.ilmo(j)) then
            ilmo(j)=ilmox
            fm(j)=lzz0(j)+asx*zu(j)*ilmox
            fh(j)=beta*(lzz0t(j)+asx*(zt(j)+z0(j)-z0t(j))*ilmox)
         endif
        endif
*----------------------------------------------------------------------
        CM=KARMAN/FM(J)
        CT=KARMAN/FH(J)
        UE(J)=u*CM
        CDM(J)=CM**2
        CTU(J)=CT*UE(J)
        CDH(J)=CM*CT
        if (rib(j).gt.0.0) then
*             cas stable
              H(J)=MIN(H(J),hmax)
        else
*             cas instable
              F=MAX(ABS(FCOR(J)),CORMIN)
              he=max(HMIN,0.3*UE(J)/F)
              H(J)=MIN(he,hmax)
        endif
        FTEMP(J)=-CTU(J)*(TA(J)-TG(J))
        FVAP(J)=-CTU(J)*(QA(J)-QG(J))
      ENDIF
   80 CONTINUE
      RETURN
      CONTAINS

C   The following code is taken from the RPN/CMC physics library file
C   /usr/local/env/armnlib/modeles/PHY_shared/ops/v_4.5/RCS/stabfunc2.cdk,v

C   Internal function FMI
C   Stability function for momentum in the unstable regime (ilmo<0)
c   Reference: Delage Y. and Girard C. BLM 58 (19-31) Eq. 19
c
      REAL FUNCTION FMI(Z2,Z02,LZZ02,ILMO2,X,X0)
      implicit none
C
      REAL, INTENT(IN ) :: Z2,Z02,LZZ02,ILMO2
      REAL, INTENT(OUT) :: X,X0
c
      X =(1-CI*Z2 *BETA*ILMO2)**(0.16666666)
      X0=(1-CI*Z02*BETA*ILMO2)**(0.16666666)
      FMI=LZZ02+LOG((X0+1)**2*SQRT(X0**2-X0+1)*(X0**2+X0+1)**1.5
     %               /((X+1)**2*SQRT(X**2-X+1)*(X**2+X+1)**1.5))
     %              +RAC3*ATAN(RAC3*((X**2-1)*X0-(X0**2-1)*X)/
     %              ((X0**2-1)*(X**2-1)+3*X*X0))
c
      RETURN
      END FUNCTION FMI
c
C   Internal function FHI
C   Stability function for heat and moisture in the unstable regime (ilmo<0)
c   Reference: Delage Y. and Girard C. BLM 58 (19-31) Eq. 17
c
      REAL FUNCTION FHI(Z2,Z0T2,LZZ0T2,ILMO2,Y,Y0)
      implicit none
C
      REAL, INTENT(IN ) :: Z2,Z0T2,LZZ0T2,ILMO2
      REAL, INTENT(OUT) :: Y,Y0
c
      Y =(1-CI*Z2  *BETA*ILMO2)**(0.33333333)
      Y0=(1-CI*Z0T2*BETA*ILMO2)**(0.33333333)
      FHI=BETA*(LZZ0T2+1.5*LOG((Y0**2+Y0+1)/(Y**2+Y+1))+RAC3*
     %        ATAN(RAC3*2*(Y-Y0)/((2*Y0+1)*(2*Y+1)+3)))
c
      RETURN
      END FUNCTION FHI
C
C   Internal function psi
C   Stability function for momentum in the stable regime (unsl>0)
c   Reference :  Y. Delage, BLM, 82 (p23-48) (Eqs.33-37)
c
      REAL FUNCTION PSI(Z2,HI2,ILMO2)
      implicit none
C
      REAL a,b,c,d
      REAL, INTENT(IN ) :: ILMO2,Z2,HI2
c
      d = 4*AS*BETA*ILMO2
      c = d*hi2 - hi2**2
      b = d - 2*hi2
      a = sqrt(1 + b*z2 - c*z2**2)
      psi = 0.5 * (a-z2*hi2-log(1+b*z2*0.5+a)-
     +            b/(2*sqrt(c))*asin((b-2*c*z2)/d))
c
      RETURN
      END FUNCTION PSI
      END
