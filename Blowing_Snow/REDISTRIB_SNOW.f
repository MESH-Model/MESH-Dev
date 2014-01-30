      SUBROUTINE REDISTRIB_SNOW(ILG,IL1,IL2,NMOS,NML,TSNOW,ZSNOW,
     1     RHOSNO,SNO,TSNOCS,ZSNOCS,HCPSCS,RHOSCS,TSNOGS,
     2     ZSNOGS,HCPSGS,RHOSGS,TSNOWC,ZSNOWC,HCPSC,RHOSC,TSNOWG,
     3     ZSNOWG,HCPSG,RHOSG,GCGRD,GRID_SQUARE,Drift,RHOSNOds,FARE,
     4     TSNOWds,distrib,WSNOCS,WSNOGS,FCS,FGS,FC,FG,Deposition,
     5     TOVRFL,OVRFLW,TRUNOF,RUNOFF,ROFN,PCPG,HTCS,WSNOW,N)
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,K,I,NML,J,nn,jj,GRUsInGS,NMOS,N
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL TSNOW(ILG),ZSNOW(ILG),RHOSNO(ILG),
     1     SNO(ILG),TSNOCS(ILG),ZSNOCS(ILG),
     2     HCPSCS(ILG),RHOSCS(ILG),TSNOGS(ILG),ZSNOGS(ILG),
     3     HCPSGS(ILG),RHOSGS(ILG),TSNOWC(ILG),ZSNOWC(ILG),
     4     HCPSC(ILG),RHOSC(ILG),TSNOWG(ILG),ZSNOWG(ILG),
     5     HCPSG(ILG),RHOSG(ILG),TOVRFL(ILG),OVRFLW(ILG),TRUNOF(ILG),
     6     RUNOFF(ILG),ROFN(ILG),PCPG(ILG),HTCS(ILG),WSNOW(ILG)
C
C     * INPUT ARRAYS.
C
      REAL GCGRD(ILG),Drift(ILG),RHOSNOds(ILG),
     1     FARE(ILG),TSNOWds(ILG),distrib(ILG),WSNOCS(ILG),
     2     WSNOGS(ILG),FCS(ILG),FGS(ILG),FC(ILG),FG(ILG),
     3     DistribLoss(ILG)
      INTEGER GRID_SQUARE(ILG)
C
C     * OUTPUT ARRAYS
      REAL Deposition(ILG)
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
             IF(Drift(K).GT.0.) THEN
              RHOSNOSumDriftPREV=RHOSNOSumDrift
              HCPSNOSumDriftPREV=HCPSNOSumDrift
              TSNOWSumDriftPREV=TSNOWSumDrift
              ! set density of drifting snow in grid square
              RHOSNOSumDrift=(RHOSNOds(K)*Drift(K)*FARE(K)
     1                     +RHOSNOSumDrift*SumDrift)/(Drift(K)+SumDrift)
              HCPSNOds=HCPICE*RHOSNOds(K)/RHOICE
              HCPSNOSumDrift=HCPICE*RHOSNOSumDrift/RHOICE
              ! TSNOWSumDrift: Kelvin following below calculation
              IF(RHOSNOSumDriftPREV.GT.0.) THEN
                ! set temperature of drifting snow in grid square
                TSNOWSumDrift=(TSNOWSumDriftPREV*(SumDrift
     1                     /RHOSNOSumDriftPREV)*HCPSNOSumDriftPREV 
     2                  + TSNOWds(K)*(Drift(K)*FARE(K)/RHOSNOds(K))*
     3                    HCPSNOds)/((SumDrift/RHOSNOSumDriftPREV) 
     4                    *HCPSNOSumDriftPREV +
     5                 (Drift(K)*FARE(K)/RHOSNOds(K))*HCPSNOds)
              ELSE ! RHOSNOSumDriftPREV = 0.
                TSNOWSumDrift=(TSNOWds(K)*(Drift(K)*FARE(K)/
     1                    RHOSNOds(K))*HCPSNOds)/
     2                    ((Drift(K)*FARE(K)/RHOSNOds(K))*HCPSNOds)
              ENDIF
              ! total snow drift in grid square
              SumDrift=SumDrift+Drift(K)*FARE(K)
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
             IF(Drift(nn).GT.0.) THEN
              transport=Drift(nn)*distrib(nn)
              !Redistribute transport within first GRU and calculate snowpack properties at subarea-level
                 IF(FCS(nn).GT.0.) THEN
                   HTCS(nn)=HTCS(nn)-FCS(nn)*HCPSCS(nn)*(TSNOCS(nn)
     1                  +TFREZ)*ZSNOCS(nn)/DELT
                   ZSNOCS(nn)=ZSNOCS(nn)+transport/200.
                   HCPSCS(nn)=HCPICE*RHOSCS(nn)/RHOICE+HCPW*WSNOCS(nn)/
     1                  (RHOW*ZSNOCS(nn))
                   HTCS(nn)=HTCS(nn)+FCS(nn)*HCPSCS(nn)*(TSNOCS(nn)
     1                  +TFREZ)*ZSNOCS(nn)/DELT
                   IF(FCS(nn).GT.0. .AND. ZSNOCS(nn).GT.0.) XSNOCS=1.0
                 ENDIF
                 IF(FGS(nn).GT.0.) THEN
                   HTCS(nn)=HTCS(nn)-FGS(nn)*HCPSGS(nn)*(TSNOGS(nn)
     1                  +TFREZ)*ZSNOGS(nn)/DELT
                   ZSNOGS(nn)=ZSNOGS(nn)+transport/200.
                   HCPSGS(nn)=HCPICE*RHOSGS(nn)/RHOICE+HCPW*WSNOGS(nn)/
     1                  (RHOW*ZSNOGS(nn))
                   HTCS(nn)=HTCS(nn)+FGS(nn)*HCPSGS(nn)*(TSNOGS(nn)
     1                  +TFREZ)*ZSNOGS(nn)/DELT
                   IF(FGS(nn).GT.0. .AND. ZSNOGS(nn).GT.0.) XSNOGS=1.0
                 ENDIF
                 IF(FC(nn).GT.0.) THEN
                   HTCS(nn)=HTCS(nn)-FC(nn)*HCPSC(nn)*(TSNOWC(nn)+TFREZ)
     1                  *ZSNOWC(nn)/DELT
                   ZSNOWC(nn)=ZSNOWC(nn)+transport/200.
                   HCPSC(nn)=HCPICE*RHOSC(nn)/RHOICE
                   HTCS(nn)=HTCS(nn)+FC(nn)*HCPSC(nn)*(TSNOWC(nn)+TFREZ)
     1                  *ZSNOWC(nn)/DELT
                   IF(FC(nn).GT.0. .AND. ZSNOWC(nn).GT.0.) XSNOWC=1.0
                 ENDIF
                 IF(FG(nn).GT.0.) THEN
                   HTCS(nn)=HTCS(nn)-FG(nn)*HCPSG(nn)*(TSNOWG(nn)+TFREZ)
     1                  *ZSNOWG(nn)/DELT
                   ZSNOWG(nn)=ZSNOWG(nn)+transport/200.
                   HCPSG(nn)=HCPICE*RHOSG(nn)/RHOICE
                   HTCS(nn)=HTCS(nn)+FG(nn)*HCPSG(nn)*(TSNOWG(nn)+TFREZ)
     1                  *ZSNOWG(nn)/DELT
                   IF(FG(nn).GT.0. .AND. ZSNOWG(nn).GT.0.) XSNOWG=1.0
                 ENDIF
	    	     !> Calculate snowpack properties & add drift at GRU-level
                 TSNOW(nn)=(FCS(nn)*(TSNOCS(nn)+TFREZ)*HCPSCS(nn)*
     1                  ZSNOCS(nn)*XSNOCS +
     2                  FGS(nn)*(TSNOGS(nn)+TFREZ)*HCPSGS(nn)*
     3                  ZSNOGS(nn)*XSNOGS +
     4                  FC (nn)*(TSNOWC(nn)+TFREZ)*HCPSC(nn)*
     5                  ZSNOWC(nn)*XSNOWC +
     6                  FG (nn)*(TSNOWG(nn)+TFREZ)*HCPSG(nn)*
     7                  ZSNOWG(nn)*XSNOWG)/
     8                 (FCS(nn)*HCPSCS(nn)*ZSNOCS(nn)*XSNOCS +
     9                  FGS(nn)*HCPSGS(nn)*ZSNOGS(nn)*XSNOGS +
     A                  FC (nn)*HCPSC(nn)*ZSNOWC(nn)*XSNOWC +
     B                  FG (nn)*HCPSG(nn)*ZSNOWG(nn)*XSNOWG)
                 RHOSNO(nn)=(FCS(nn)*RHOSCS(nn)*ZSNOCS(nn)*XSNOCS +   
     1                   FGS(nn)*RHOSGS(nn)*ZSNOGS(nn)*XSNOGS +   
     2                   FC (nn)*RHOSC(nn)*ZSNOWC(nn)*XSNOWC +   
     3                   FG (nn)*RHOSG(nn)*ZSNOWG(nn)*XSNOWG)/    
     4                  (FCS(nn)*ZSNOCS(nn)*XSNOCS +
     5                   FGS(nn)*ZSNOGS(nn)*XSNOGS +                 
     6                   FC (nn)*ZSNOWC(nn)*XSNOWC +
     7                   FG (nn)*ZSNOWG(nn)*XSNOWG)
                 ZSNOW(nn)=FCS(nn)*ZSNOCS(nn) + FGS(nn)*ZSNOGS(nn) +
     1                 FC (nn)*ZSNOWC(nn) + FG (nn)*ZSNOWG(nn)
                 SNO(nn)=ZSNOW(nn)*RHOSNO(nn)
                 Deposition(nn)=transport
             ELSE
              Deposition(nn)=0.0
             ENDIF !(Drift(nn).GT.0.)
            ELSE
              Deposition(nn)=0.0
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
                 IF(FCS(J).GT.0.) THEN
                   HTCS(J)=HTCS(J)-FCS(J)*HCPSCS(J)*(TSNOCS(J)+TFREZ)*
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
                   HTCS(J)=HTCS(J)+FCS(J)*HCPSCS(J)*(TSNOCS(J)+TFREZ)*
     1                  ZSNOCS(J)/DELT
                   IF(FCS(J).GT.0. .AND. ZSNOCS(J).GT.0.) XSNOCS=1.0
                 ENDIF
                 IF(FGS(J).GT.0.) THEN
                   HTCS(J)=HTCS(J)-FGS(J)*HCPSGS(J)*(TSNOGS(J)+TFREZ)*
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
                   HTCS(J)=HTCS(J)+FGS(J)*HCPSGS(J)*(TSNOGS(J)+TFREZ)*
     1                  ZSNOGS(J)/DELT
                   IF(FGS(J).GT.0. .AND. ZSNOGS(J).GT.0.) XSNOGS=1.0
                 ENDIF
                 IF(FC(J).GT.0.) THEN
                   HTCS(J)=HTCS(J)-FC(J)*HCPSC(J)*(TSNOWC(J)+TFREZ)*
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
                   HTCS(J)=HTCS(J)+FC(J)*HCPSC(J)*(TSNOWC(J)+TFREZ)*
     1                  ZSNOWC(J)/DELT
                   IF(FC(J).GT.0. .AND. ZSNOWC(J).GT.0.) XSNOWC=1.0
                 ENDIF
                 IF(FG(J).GT.0.) THEN
                   HTCS(J)=HTCS(J)-FG(J)*HCPSG(J)*(TSNOWG(J)+TFREZ)*
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
                   HTCS(J)=HTCS(J)+FG(J)*HCPSG(J)*(TSNOWG(J)+TFREZ)*
     1                  ZSNOWG(J)/DELT
                   IF(FG(J).GT.0. .AND. ZSNOWG(J).GT.0.) XSNOWG=1.0
                 ENDIF
	    		 !> Calculate snowpack properties & add drift at GRU-level
                 TSNOW(J)=(FCS(J)*(TSNOCS(J)+TFREZ)*HCPSCS(J)*
     1                  ZSNOCS(J)*XSNOCS +
     2                  FGS(J)*(TSNOGS(J)+TFREZ)*HCPSGS(J)*
     3                  ZSNOGS(J)*XSNOGS +
     4                  FC (J)*(TSNOWC(J)+TFREZ)*HCPSC(J)*
     5                  ZSNOWC(J)*XSNOWC +
     6                  FG (J)*(TSNOWG(J)+TFREZ)*HCPSG(J)*
     7                  ZSNOWG(J)*XSNOWG)/
     8                 (FCS(J)*HCPSCS(J)*ZSNOCS(J)*XSNOCS +
     9                  FGS(J)*HCPSGS(J)*ZSNOGS(J)*XSNOGS +
     A                  FC (J)*HCPSC(J)*ZSNOWC(J)*XSNOWC +
     B                  FG (J)*HCPSG(J)*ZSNOWG(J)*XSNOWG)
                 RHOSNO(J)=(FCS(J)*RHOSCS(J)*ZSNOCS(J)*XSNOCS +   
     1                   FGS(J)*RHOSGS(J)*ZSNOGS(J)*XSNOGS +   
     2                   FC (J)*RHOSC(J)*ZSNOWC(J)*XSNOWC +   
     3                   FG (J)*RHOSG(J)*ZSNOWG(J)*XSNOWG)/    
     4                  (FCS(J)*ZSNOCS(J)*XSNOCS +
     5                   FGS(J)*ZSNOGS(J)*XSNOGS +                 
     6                   FC (J)*ZSNOWC(J)*XSNOWC +
     7                   FG (J)*ZSNOWG(J)*XSNOWG)
                 ZSNOW(J)=FCS(J)*ZSNOCS(J) + FGS(J)*ZSNOGS(J) +
     1                 FC (J)*ZSNOWC(J) + FG (J)*ZSNOWG(J)
                 SNO(J)=ZSNOW(J)*RHOSNO(J)
                 !> remove drift used from total available
                 Deposition(J)=transport
                 SumDrift=SumDrift-transport*FARE(J)
	    	   ELSE
	    	     Deposition(J)=0.
	    	   ENDIF
	     ELSE
	       Deposition(J)=0.
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
         IF(SNO(K).LT.1.0E-2 .AND. SNO(K).GT.0.0) THEN
          TOVRFL(K)=(TOVRFL(K)*OVRFLW(K)+TSNOW(K)*(SNO(K)+
     1        WSNOW(K))/DELT)/(OVRFLW(K)+(SNO(K)+WSNOW(K))/
     2        DELT)
          OVRFLW(K)=OVRFLW(K)+(SNO(K)+WSNOW(K))/DELT
          TRUNOF(K)=(TRUNOF(K)*RUNOFF(K)+TSNOW(K)*(SNO(K)+
     1        WSNOW(K))/DELT)/(RUNOFF(K)+(SNO(K)+WSNOW(K))/
     2        DELT)
          RUNOFF(K)=RUNOFF(K)+(SNO(K)+WSNOW(K))/DELT
          ROFN(K)=ROFN(K)+(SNO(K)+WSNOW(K))/DELT
          PCPG(K)=PCPG(K)+(SNO(K)+WSNOW(K))/DELT
          HTCS(K)=HTCS(K)-TSNOW(K)*(SPHICE*SNO(K)+SPHW*
     1        WSNOW(K))/DELT
          TSNOW(K)=TFREZ
          RHOSNO(K)=0.0
          SNO(K)=0.0                            
          WSNOW(K)=0.0
         ENDIF
  600 CONTINUE
C
      RETURN
      END