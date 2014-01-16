      SUBROUTINE REDISTRIB_SNOW(ILG,IL1,IL2,NMOS,NML,TSNOW,ZSNOW,
     1     RHOSNO,SNO,TSNOCS,ZSNOCS,HCPSCS,RHOSCS,TSNOGS,
     2     ZSNOGS,HCPSGS,RHOSGS,TSNOWC,ZSNOWC,HCPSC,RHOSC,TSNOWG,
     3     ZSNOWG,HCPSG,RHOSG,GCGRD,GRID_SQUARE,Drift,RHOSNOds,FARE,
     4     TSNOWds,distrib,WSNOCS,WSNOGS,FCS,FGS,FC,FG,Deposition,
     5     TOVRFL,OVRFLW,TRUNOF,RUNOFF,ROFN,PCPG,HTCS,WSNOW,N,q)
C
      use MODELS, only : bsm,Nmod
      
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,K,I,NML,J,nn,jj,GRUsInGS,NMOS,N,q
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL TSNOW(ILG,Nmod),ZSNOW(ILG,Nmod),RHOSNO(ILG,Nmod),
     1     SNO(ILG,Nmod),TSNOCS(ILG),ZSNOCS(ILG),
     2     HCPSCS(ILG),RHOSCS(ILG),TSNOGS(ILG),ZSNOGS(ILG),
     3     HCPSGS(ILG),RHOSGS(ILG),TSNOWC(ILG),ZSNOWC(ILG),
     4     HCPSC(ILG),RHOSC(ILG),TSNOWG(ILG),ZSNOWG(ILG),
     5     HCPSG(ILG),RHOSG(ILG),TOVRFL(ILG),OVRFLW(ILG),TRUNOF(ILG),
     6     RUNOFF(ILG),ROFN(ILG),PCPG(ILG),HTCS(ILG),
     +     WSNOW(ILG,Nmod)
C
C     * INPUT ARRAYS.
C
      REAL GCGRD(ILG),Drift(ILG,Nmod),RHOSNOds(ILG,Nmod),
     1     FARE(ILG),TSNOWds(ILG,Nmod),distrib(ILG),WSNOCS(ILG),
     2     WSNOGS(ILG),FCS(ILG,Nmod),FGS(ILG,Nmod),FC(ILG,Nmod),
     3     FG(ILG,Nmod),DistribLoss(ILG,Nmod)
      INTEGER GRID_SQUARE(ILG)
C
C     * OUTPUT ARRAYS
      REAL Deposition(ILG,Nmod)
C
C     * TEMPORARY VARIABLES.
C
      REAL TSNOWSumDrift,RHOSNOSumDrift,RHOSNOSumDriftPREV,
     1     HCPSNOSumDriftPREV,TSNOWSumDriftPREV,
     2     HCPSNOSumDrift,SumDrift,HCPSNOds,total,
     3     XSNOCS,XSNOGS,XSNOWC,XSNOWG, transport
      INTEGER PrevNumTiles
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,
     1     SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C      
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C
C--------------------------------------------------------------
C
!> distribute drift amongst GRUs within grid squares
      PrevNumTiles=0

      !> Set grid square drift mass, temperature, density
      !  as weighted average of GRus in grid square
      DO 100 I=IL1,IL2 !LOOP GRID SQUARES(IL2=NA)
        IF(GCGRD(I).LE.-0.5) THEN
          TSNOWSumDrift=TFREZ 
          RHOSNOSumDrift=0.0
          RHOSNOSumDriftPREV=0.0
          HCPSNOSumDriftPREV=0.0
          TSNOWSumDriftPREV=TFREZ
          RHOSNOSumDrift=0.0
          HCPSNOSumDrift=0.0
          SumDrift=0.0
          GRUsInGS=0
          XSNOCS=0.0
          XSNOGS=0.0
          XSNOWC=0.0
          XSNOWG=0.0
          DO 200 K=1,NML !LOOP land-based GRUs x grid squares(NML)
            IF(GRID_SQUARE(K).EQ.I) THEN
             HTCS(K)=0.
             IF(Drift(K,q).GT.0.) THEN
              RHOSNOSumDriftPREV=RHOSNOSumDrift
              HCPSNOSumDriftPREV=HCPSNOSumDrift
              TSNOWSumDriftPREV=TSNOWSumDrift
              ! set density of drifting snow in grid square
              RHOSNOSumDrift=(RHOSNOds(K,q)*Drift(K,q)*FARE(K)
     1                 +RHOSNOSumDrift*SumDrift)/(Drift(K,q)+SumDrift)
              HCPSNOds=HCPICE*RHOSNOds(K,q)/RHOICE
              HCPSNOSumDrift=HCPICE*RHOSNOSumDrift/RHOICE
              ! TSNOWSumDrift: Kelvin following below calculation
              IF(RHOSNOSumDriftPREV.GT.0.) THEN
                ! set temperature of drifting snow in grid square
                TSNOWSumDrift=(TSNOWSumDriftPREV*(SumDrift
     1                     /RHOSNOSumDriftPREV)*HCPSNOSumDriftPREV 
     2                +TSNOWds(K,q)*(Drift(K,q)*FARE(K)/RHOSNOds(K,q))*
     3                    HCPSNOds)/((SumDrift/RHOSNOSumDriftPREV) 
     4                    *HCPSNOSumDriftPREV +
     5                 (Drift(K,q)*FARE(K)/RHOSNOds(K,q))*HCPSNOds)
              ELSE ! RHOSNOSumDriftPREV = 0.
                TSNOWSumDrift=(TSNOWds(K,q)*(Drift(K,q)*FARE(K)/
     1                    RHOSNOds(K,q))*HCPSNOds)/
     2                    ((Drift(K,q)*FARE(K)/RHOSNOds(K,q))*HCPSNOds)
              ENDIF
              ! total snow drift in grid square
              SumDrift=SumDrift+Drift(K,q)*FARE(K)
             ENDIF
            GRUsInGS=GRUsInGS+1 !number of GRUs in grid square
            ENDIF
  200     CONTINUE  
        ENDIF

        total=0.0

        DO 400 nn=1,GRUsInGS
          !First GRU
          IF(nn.EQ.1) THEN
            IF(distrib(nn).GT.0.) THEN
             IF(Drift(nn,q).GT.0.) THEN
              transport=Drift(nn,q)*distrib(nn)
              !Redistribute transport within first GRU and calculate snowpack properties at subarea-level
                 IF(FCS(nn,q).GT.0.) THEN
                   HTCS(nn)=HTCS(nn)-FCS(nn,q)*HCPSCS(nn)*(TSNOCS(nn)
     1                  +TFREZ)*ZSNOCS(nn)/DELT
                   ZSNOCS(nn)=ZSNOCS(nn)+transport/200.!RHOSCS(nn)
                   HCPSCS(nn)=HCPICE*RHOSCS(nn)/RHOICE+HCPW*WSNOCS(nn)/
     1                  (RHOW*ZSNOCS(nn))
                   HTCS(nn)=HTCS(nn)+FCS(nn,q)*HCPSCS(nn)*(TSNOCS(nn)
     1                  +TFREZ)*ZSNOCS(nn)/DELT
                   IF(FCS(nn,q).GT.0. .AND. ZSNOCS(nn).GT.0.) XSNOCS=1.0
                 ENDIF
                 IF(FGS(nn,q).GT.0.) THEN
                   HTCS(nn)=HTCS(nn)-FGS(nn,q)*HCPSGS(nn)*(TSNOGS(nn)
     1                  +TFREZ)*ZSNOGS(nn)/DELT
                   ZSNOGS(nn)=ZSNOGS(nn)+transport/200.!RHOSGS(nn)
                   HCPSGS(nn)=HCPICE*RHOSGS(nn)/RHOICE+HCPW*WSNOGS(nn)/
     1                  (RHOW*ZSNOGS(nn))
                   HTCS(nn)=HTCS(nn)+FGS(nn,q)*HCPSGS(nn)*(TSNOGS(nn)
     1                  +TFREZ)*ZSNOGS(nn)/DELT
                   IF(FGS(nn,q).GT.0. .AND. ZSNOGS(nn).GT.0.) XSNOGS=1.0
                 ENDIF
                 IF(FC(nn,q).GT.0.) THEN
                   HTCS(nn)=HTCS(nn)-FC(nn,q)*HCPSC(nn)*(TSNOWC(nn)
     1                  +TFREZ)*ZSNOWC(nn)/DELT
                   ZSNOWC(nn)=ZSNOWC(nn)+transport/200.!RHOSC(nn)
                   HCPSC(nn)=HCPICE*200./RHOICE!RHOSC(nn)/RHOICE
                   HTCS(nn)=HTCS(nn)+FC(nn,q)*HCPSC(nn)*(TSNOWC(nn)
     1                  +TFREZ)*ZSNOWC(nn)/DELT
                   IF(FC(nn,q).GT.0. .AND. ZSNOWC(nn).GT.0.) XSNOWC=1.0
                 ENDIF
                 IF(FG(nn,q).GT.0.) THEN
                   HTCS(nn)=HTCS(nn)-FG(nn,q)*HCPSG(nn)*(TSNOWG(nn)
     1                  +TFREZ)*ZSNOWG(nn)/DELT
                   ZSNOWG(nn)=ZSNOWG(nn)+transport/200.!RHOSG(nn)
                   HCPSG(nn)=HCPICE*200./RHOICE!RHOSG(nn)/RHOICE
                   HTCS(nn)=HTCS(nn)+FG(nn,q)*HCPSG(nn)*(TSNOWG(nn)
     1                  +TFREZ)*ZSNOWG(nn)/DELT
                   IF(FG(nn,q).GT.0. .AND. ZSNOWG(nn).GT.0.) XSNOWG=1.0
                 ENDIF
	    	     !> Calculate snowpack properties & add drift at GRU-level
                 TSNOW(nn,q)=(FCS(nn,q)*(TSNOCS(nn)+TFREZ)*HCPSCS(nn)*
     1                  ZSNOCS(nn)*XSNOCS +
     2                  FGS(nn,q)*(TSNOGS(nn)+TFREZ)*HCPSGS(nn)*
     3                  ZSNOGS(nn)*XSNOGS +
     4                  FC (nn,q)*(TSNOWC(nn)+TFREZ)*HCPSC(nn)*
     5                  ZSNOWC(nn)*XSNOWC +
     6                  FG (nn,q)*(TSNOWG(nn)+TFREZ)*HCPSG(nn)*
     7                  ZSNOWG(nn)*XSNOWG)/
     8                 (FCS(nn,q)*HCPSCS(nn)*ZSNOCS(nn)*XSNOCS +
     9                  FGS(nn,q)*HCPSGS(nn)*ZSNOGS(nn)*XSNOGS +
     A                  FC (nn,q)*HCPSC(nn)*ZSNOWC(nn)*XSNOWC +
     B                  FG (nn,q)*HCPSG(nn)*ZSNOWG(nn)*XSNOWG)
                 RHOSNO(nn,q)=(FCS(nn,q)*RHOSCS(nn)*ZSNOCS(nn)*XSNOCS +   
     1                   FGS(nn,q)*RHOSGS(nn)*ZSNOGS(nn)*XSNOGS +   
     2                   FC (nn,q)*RHOSC(nn)*ZSNOWC(nn)*XSNOWC +   
     3                   FG (nn,q)*RHOSG(nn)*ZSNOWG(nn)*XSNOWG)/    
     4                  (FCS(nn,q)*ZSNOCS(nn)*XSNOCS +
     5                   FGS(nn,q)*ZSNOGS(nn)*XSNOGS +                 
     6                   FC (nn,q)*ZSNOWC(nn)*XSNOWC +
     7                   FG (nn,q)*ZSNOWG(nn)*XSNOWG)
                 ZSNOW(nn,q)=FCS(nn,q)*ZSNOCS(nn) + FGS(nn,q)*ZSNOGS(nn)
     1                  +FC (nn,q)*ZSNOWC(nn) + FG (nn,q)*ZSNOWG(nn)
                 SNO(nn,q)=ZSNOW(nn,q)*RHOSNO(nn,q)
                 Deposition(nn,q)=transport
             ELSE
              Deposition(nn,q)=0.0
             ENDIF !(Drift(nn).GT.0.)
            ELSE
              Deposition(nn,q)=0.0
            ENDIF !(distrib(nn).GT.0.)
          !Not first GRU
          ELSE
          J=nn+PrevNumTiles
          total=0.
           IF(SumDrift.GT.0.0) THEN
	    	   IF(distrib(J).GT.0.) THEN
	    	     DO 500 jj=nn,GRUsInGS  !> calculate denominator
                   total=total+distrib(jj+PrevNumTiles)
  500            CONTINUE
	    	     !> determine contribution and scale
	    	     transport=SumDrift*distrib(J)/total/FARE(J)
                 !Redistribute transport and calculate snowpack properties at subarea-level
                 IF(FCS(J,q).GT.0.) THEN
                   HTCS(J)=HTCS(J)-FCS(J,q)*HCPSCS(J)*(TSNOCS(J)+TFREZ)*
     1                  ZSNOCS(J)/DELT
                   TSNOCS(J)=((TSNOCS(J)+TFREZ)*ZSNOCS(J)*HCPSCS(J) +
     1                  TSNOWSumDrift*(transport/RHOSNOSumDrift)*
     2                  HCPSNOSumDrift)/(ZSNOCS(J)*HCPSCS(J) + 
     3                  (transport/RHOSNOSumDrift)*HCPSNOSumDrift)
     4                  - TFREZ
                   RHOSCS(J)=(ZSNOCS(J)*RHOSCS(J) + 
     1                  (transport/RHOSNOSumDrift)*RHOSNOSumDrift)/
     2                  (ZSNOCS(J)+(transport/RHOSNOSumDrift))
                   ZSNOCS(J)=ZSNOCS(J)+transport/RHOSNOSumDrift
                   HCPSCS(J)=HCPICE*RHOSCS(J)/RHOICE+HCPW*WSNOCS(J)/
     1                  (RHOW*ZSNOCS(J))
                   HTCS(J)=HTCS(J)+FCS(J,q)*HCPSCS(J)*(TSNOCS(J)+TFREZ)*
     1                  ZSNOCS(J)/DELT
                   IF(FCS(J,q).GT.0. .AND. ZSNOCS(J).GT.0.) XSNOCS=1.0
                 ENDIF
                 IF(FGS(J,q).GT.0.) THEN
                   HTCS(J)=HTCS(J)-FGS(J,q)*HCPSGS(J)*(TSNOGS(J)+TFREZ)*
     1                  ZSNOGS(J)/DELT
                   TSNOGS(J)=((TSNOGS(J)+TFREZ)*ZSNOGS(J)*HCPSGS(J) +
     1                  TSNOWSumDrift*(transport/RHOSNOSumDrift)*
     2                  HCPSNOSumDrift)/(ZSNOGS(J)*HCPSGS(J) + 
     3                  (transport/RHOSNOSumDrift)*HCPSNOSumDrift)
     4                  - TFREZ
                   RHOSGS(J)=(ZSNOGS(J)*RHOSGS(J) + 
     1                  (transport/RHOSNOSumDrift)*RHOSNOSumDrift)/
     2                  (ZSNOGS(J)+(transport/RHOSNOSumDrift))
                   ZSNOGS(J)=ZSNOGS(J)+transport/RHOSNOSumDrift
                   HCPSGS(J)=HCPICE*RHOSGS(J)/RHOICE+HCPW*WSNOGS(J)/
     1                  (RHOW*ZSNOGS(J))
                   HTCS(J)=HTCS(J)+FGS(J,q)*HCPSGS(J)*(TSNOGS(J)+TFREZ)*
     1                  ZSNOGS(J)/DELT
                   IF(FGS(J,q).GT.0. .AND. ZSNOGS(J).GT.0.) XSNOGS=1.0
                 ENDIF
                 IF(FC(J,q).GT.0.) THEN
                   HTCS(J)=HTCS(J)-FC(J,q)*HCPSC(J)*(TSNOWC(J)+TFREZ)*
     1                  ZSNOWC(J)/DELT
                   TSNOWC(J)=((TSNOWC(J)+TFREZ)*ZSNOWC(J)*HCPSC(J) +
     1                  TSNOWSumDrift*(transport/RHOSNOSumDrift)*
     2                  HCPSNOSumDrift)/(ZSNOWC(J)*HCPSC(J) + 
     3                  (transport/RHOSNOSumDrift)*HCPSNOSumDrift)
     4                  - TFREZ
                   RHOSC(J)=(ZSNOWC(J)*RHOSC(J) + 
     1                  (transport/RHOSNOSumDrift)*RHOSNOSumDrift)/
     2                  (ZSNOWC(J)+(transport/RHOSNOSumDrift))
                   ZSNOWC(J)=ZSNOWC(J)+transport/RHOSNOSumDrift
                   HCPSC(J)=HCPICE*RHOSC(J)/RHOICE
                   HTCS(J)=HTCS(J)+FC(J,q)*HCPSC(J)*(TSNOWC(J)+TFREZ)*
     1                  ZSNOWC(J)/DELT
                   IF(FC(J,q).GT.0. .AND. ZSNOWC(J).GT.0.) XSNOWC=1.0
                 ENDIF
                 IF(FG(J,q).GT.0.) THEN
                   HTCS(J)=HTCS(J)-FG(J,q)*HCPSG(J)*(TSNOWG(J)+TFREZ)*
     1                  ZSNOWG(J)/DELT
                   TSNOWG(J)=((TSNOWG(J)+TFREZ)*ZSNOWG(J)*HCPSG(J) +
     1                  TSNOWSumDrift*(transport/RHOSNOSumDrift)*
     2                  HCPSNOSumDrift)/(ZSNOWG(J)*HCPSG(J) + 
     3                  (transport/RHOSNOSumDrift)*HCPSNOSumDrift)
     4                  - TFREZ
                   RHOSG(J)=(ZSNOWG(J)*RHOSG(J) + 
     1                  (transport/RHOSNOSumDrift)*RHOSNOSumDrift)/
     2                  (ZSNOWG(J)+(transport/RHOSNOSumDrift))
                   ZSNOWG(J)=ZSNOWG(J)+transport/RHOSNOSumDrift
                   HCPSG(J)=HCPICE*RHOSG(J)/RHOICE
                   HTCS(J)=HTCS(J)+FG(J,q)*HCPSG(J)*(TSNOWG(J)+TFREZ)*
     1                  ZSNOWG(J)/DELT
                   IF(FG(J,q).GT.0. .AND. ZSNOWG(J).GT.0.) XSNOWG=1.0
                 ENDIF
	    		 !> Calculate snowpack properties & add drift at GRU-level
                 TSNOW(J,q)=(FCS(J,q)*(TSNOCS(J)+TFREZ)*HCPSCS(J)*
     1                  ZSNOCS(J)*XSNOCS +
     2                  FGS(J,q)*(TSNOGS(J)+TFREZ)*HCPSGS(J)*
     3                  ZSNOGS(J)*XSNOGS +
     4                  FC (J,q)*(TSNOWC(J)+TFREZ)*HCPSC(J)*
     5                  ZSNOWC(J)*XSNOWC +
     6                  FG (J,q)*(TSNOWG(J)+TFREZ)*HCPSG(J)*
     7                  ZSNOWG(J)*XSNOWG)/
     8                 (FCS(J,q)*HCPSCS(J)*ZSNOCS(J)*XSNOCS +
     9                  FGS(J,q)*HCPSGS(J)*ZSNOGS(J)*XSNOGS +
     A                  FC (J,q)*HCPSC(J)*ZSNOWC(J)*XSNOWC +
     B                  FG (J,q)*HCPSG(J)*ZSNOWG(J)*XSNOWG)
                 RHOSNO(J,q)=(FCS(J,q)*RHOSCS(J)*ZSNOCS(J)*XSNOCS +   
     1                   FGS(J,q)*RHOSGS(J)*ZSNOGS(J)*XSNOGS +   
     2                   FC (J,q)*RHOSC(J)*ZSNOWC(J)*XSNOWC +   
     3                   FG (J,q)*RHOSG(J)*ZSNOWG(J)*XSNOWG)/    
     4                  (FCS(J,q)*ZSNOCS(J)*XSNOCS +
     5                   FGS(J,q)*ZSNOGS(J)*XSNOGS +                 
     6                   FC (J,q)*ZSNOWC(J)*XSNOWC +
     7                   FG (J,q)*ZSNOWG(J)*XSNOWG)
                 ZSNOW(J,q)=FCS(J,q)*ZSNOCS(J) + FGS(J,q)*ZSNOGS(J) +
     1                 FC (J,q)*ZSNOWC(J) + FG (J,q)*ZSNOWG(J)
                 SNO(J,q)=ZSNOW(J,q)*RHOSNO(J,q)
                 !> remove drift used from total available
                 Deposition(J,q)=transport
                 SumDrift=SumDrift-transport*FARE(J)
	    	   ELSE
	    	     Deposition(J,q)=0.
	    	   ENDIF
	     ELSE
	       Deposition(J,q)=0.
	     ENDIF !>(SumDrift.GT.0.0)
	    ENDIF !(nn.EQ.1) THEN
	   !ELSE
	   !  Deposition(J)=0.
	   !ENDIF !>(distrib(J).GE.0.0 AND nn+1.LT.nhru)
  400   CONTINUE
        !> used to track gathered tiles that have already had snow redistributed within
        PrevNumTiles=PrevNumTiles+GRUsInGS
  100 CONTINUE
  
      DO 600 K=1,NML !LOOP TILES(NML)
        !DEAL WITH VANISHINGLY SMALL SNOWPACK (ANALOGUOUS TO WHAT WAS DONE IN CLASSW)
         IF(SNO(K,q).LT.1.0E-2 .AND. SNO(K,q).GT.0.0) THEN
          TOVRFL(K)=(TOVRFL(K)*OVRFLW(K)+TSNOW(K,q)*(SNO(K,q)+
     1        WSNOW(K,q))/DELT)/(OVRFLW(K)+(SNO(K,q)+WSNOW(K,q))/
     2        DELT)
          OVRFLW(K)=OVRFLW(K)+(SNO(K,q)+WSNOW(K,q))/DELT
          TRUNOF(K)=(TRUNOF(K)*RUNOFF(K)+TSNOW(K,q)*(SNO(K,q)+
     1        WSNOW(K,q))/DELT)/(RUNOFF(K)+(SNO(K,q)+WSNOW(K,q))/
     2        DELT)
          RUNOFF(K)=RUNOFF(K)+(SNO(K,q)+WSNOW(K,q))/DELT
          ROFN(K)=ROFN(K)+(SNO(K,q)+WSNOW(K,q))/DELT
          PCPG(K)=PCPG(K)+(SNO(K,q)+WSNOW(K,q))/DELT
          HTCS(K)=HTCS(K)-TSNOW(K,q)*(SPHICE*SNO(K,q)+SPHW*
     1        WSNOW(K,q))/DELT
          TSNOW(K,q)=TFREZ
          RHOSNO(K,q)=0.0
          SNO(K,q)=0.0                            
          WSNOW(K,q)=0.0
         ENDIF
  600 CONTINUE
C
      RETURN
      END