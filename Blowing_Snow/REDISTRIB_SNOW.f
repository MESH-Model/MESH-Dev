      SUBROUTINE REDISTRIB_SNOW(NA,NTYPE,ILG,NML,IL1,IL2,TSNOW,ZSNOW,
     1     RHOSNO,SNO,TSNOCS,ZSNOCS,HCPSCS,RHOSCS,TSNOGS,
     2     ZSNOGS,HCPSGS,RHOSGS,TSNOWC,ZSNOWC,HCPSC,RHOSC,TSNOWG,
     3     ZSNOWG,HCPSG,RHOSG,GC,GRID_SQUARE,Drift,Area_Weight,
     4     TSNOWds,distrib,WSNOCS,WSNOGS,FCS,FGS,FC,FG,Deposition,
     5     TOVRFL,OVRFLW,TRUNOF,RUNOFF,ROFN,PCPG,HTCS,WSNOW,N)
C
C     * DEC 2021 - M.ELSHAMY Replaced FARE with Area_Weight as FARE has 
C     *                      been changed from its original definition    
C     * MAY 2017 - D.PRINCZ. Changed order of initial variables in
C     *                      subroutine call because not all should be
C     *                      allocated to ILG or NML. Most variables
C     *                      should be allocated to NML. ILMOS/GRID_SQUARE
C     *                      should be allocated to ILG.
C     *                      Also changed GC to use the NML/GAT
C     *                      variant instead of the GRD variant.
C     *
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER NA,NTYPE,ILG,NML,IL1,IL2,K,I,J,nn,jj,GRUsInGS,N
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL TSNOW(NML),ZSNOW(NML),RHOSNO(NML),
     1     SNO(NML),TSNOCS(NML),ZSNOCS(NML),
     2     HCPSCS(NML),RHOSCS(NML),TSNOGS(NML),ZSNOGS(NML),
     3     HCPSGS(NML),RHOSGS(NML),TSNOWC(NML),ZSNOWC(NML),
     4     HCPSC(NML),RHOSC(NML),TSNOWG(NML),ZSNOWG(NML),
     5     HCPSG(NML),RHOSG(NML),TOVRFL(NML),OVRFLW(NML),TRUNOF(NML),
     6     RUNOFF(NML),ROFN(NML),PCPG(NML),HTCS(NML),WSNOW(NML)
C
C     * INPUT ARRAYS.
C
      REAL GC(NML),Drift(NML),
     1     Area_Weight(NML),TSNOWds(NML),distrib(NML),WSNOCS(NML),
     2     WSNOGS(NML),FCS(NML),FGS(NML),FC(NML),FG(NML),
     3     DistribLoss(NML)
      INTEGER GRID_SQUARE(ILG)
C
C     * OUTPUT ARRAYS
      REAL Deposition(NML)
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
      PrevNumTiles=IL1-1

      !> Set grid square drift mass, temperature, density
      !  as weighted average of GRus in grid square
      DO 100 I=GRID_SQUARE(IL1),GRID_SQUARE(IL2) !LOOP GRID SQUARES
          TSNOWSumDrift=TFREZ 
          RHOSNOSumDrift=300.!mm 0.0
          HCPSNOSumDrift=HCPICE*RHOSNOSumDrift/RHOICE
          TSNOWSumDriftPREV=TFREZ
          SumDrift=0.0
          GRUsInGS=0
          XSNOCS=0.0
          XSNOGS=0.0
          XSNOWC=0.0
          XSNOWG=0.0
          DO 200 K=IL1,IL2 !LOOP land-based GRUs x grid squares
           IF(GC(K).LE.-0.5) THEN
            IF(GRID_SQUARE(K).EQ.I) THEN
             HTCS(K)=0.
             IF(Drift(K).GT.0.) THEN
              TSNOWSumDriftPREV=TSNOWSumDrift
              ! TSNOWSumDrift: Kelvin following below calculation
                ! set temperature of drifting snow in grid square
                TSNOWSumDrift=(TSNOWSumDriftPREV*(SumDrift
     1                     /RHOSNOSumDrift)*HCPSNOSumDrift +
     2           TSNOWds(K)*(Drift(K)*Area_Weight(K)/RHOSNOSumDrift)*
     3           HCPSNOSumDrift)/((SumDrift/RHOSNOSumDrift) 
     4           *HCPSNOSumDrift +
     5           (Drift(K)*Area_Weight(K)/
     6            RHOSNOSumDrift)*HCPSNOSumDrift)
              ! total snow drift in grid square
              SumDrift=SumDrift+Drift(K)*Area_Weight(K)
             ENDIF !(Drift(K).GT.0.)
            GRUsInGS=GRUsInGS+1 !number of GRUs in grid square
            ENDIF !(GRID_SQUARE(K).EQ.I)
           ENDIF
  200     CONTINUE  

        total=0.0

        DO 400 nn=1,GRUsInGS
          !> FY found nn to be wrongly checked against 1 (first tile on first grid not 1st tile in each grid)
          !> Additionaly he found no need to distinguish the first tile anyway - commented the whole block
C          !First GRU
C          IF(nn.EQ.1) THEN
C            IF(distrib(nn).GT.0.) THEN
C             IF(Drift(nn).GT.0.) THEN
C              transport=Drift(nn)*distrib(nn)
C              !Redistribute transport within first GRU and calculate snowpack properties at subarea-level
C                 IF(FCS(nn).GT.0.) THEN
C                   HTCS(nn)=HTCS(nn)-FCS(nn)*HCPSCS(nn)*(TSNOCS(nn)
C     1                  +TFREZ)*ZSNOCS(nn)/DELT
C                   ZSNOCS(nn)=ZSNOCS(nn)+transport/RHOSNOSumDrift
C                   HCPSCS(nn)=HCPICE*RHOSCS(nn)/RHOICE+HCPW*WSNOCS(nn)/
C     1                  (RHOW*ZSNOCS(nn))
C                   HTCS(nn)=HTCS(nn)+FCS(nn)*HCPSCS(nn)*(TSNOCS(nn)
C     1                  +TFREZ)*ZSNOCS(nn)/DELT
C                   IF(FCS(nn).GT.0. .AND. ZSNOCS(nn).GT.0.) XSNOCS=1.0
C                 ENDIF
C                 IF(FGS(nn).GT.0.) THEN
C                   HTCS(nn)=HTCS(nn)-FGS(nn)*HCPSGS(nn)*(TSNOGS(nn)
C     1                  +TFREZ)*ZSNOGS(nn)/DELT
C                   ZSNOGS(nn)=ZSNOGS(nn)+transport/RHOSNOSumDrift
C                   HCPSGS(nn)=HCPICE*RHOSGS(nn)/RHOICE+HCPW*WSNOGS(nn)/
C     1                  (RHOW*ZSNOGS(nn))
C                   HTCS(nn)=HTCS(nn)+FGS(nn)*HCPSGS(nn)*(TSNOGS(nn)
C     1                  +TFREZ)*ZSNOGS(nn)/DELT
C                   IF(FGS(nn).GT.0. .AND. ZSNOGS(nn).GT.0.) XSNOGS=1.0
C                 ENDIF
C                 IF(FC(nn).GT.0.) THEN
C                   HTCS(nn)=HTCS(nn)-FC(nn)*HCPSC(nn)*(TSNOWC(nn)+TFREZ)
C     1                  *ZSNOWC(nn)/DELT
C                   ZSNOWC(nn)=ZSNOWC(nn)+transport/RHOSNOSumDrift
C                   HCPSC(nn)=HCPICE*RHOSC(nn)/RHOICE
C                   HTCS(nn)=HTCS(nn)+FC(nn)*HCPSC(nn)*(TSNOWC(nn)+TFREZ)
C     1                  *ZSNOWC(nn)/DELT
C                   IF(FC(nn).GT.0. .AND. ZSNOWC(nn).GT.0.) XSNOWC=1.0
C                 ENDIF
C                 IF(FG(nn).GT.0.) THEN
C                   HTCS(nn)=HTCS(nn)-FG(nn)*HCPSG(nn)*(TSNOWG(nn)+TFREZ)
C     1                  *ZSNOWG(nn)/DELT
C                   ZSNOWG(nn)=ZSNOWG(nn)+transport/RHOSNOSumDrift
C                   HCPSG(nn)=HCPICE*RHOSG(nn)/RHOICE
C                   HTCS(nn)=HTCS(nn)+FG(nn)*HCPSG(nn)*(TSNOWG(nn)+TFREZ)
C     1                  *ZSNOWG(nn)/DELT
C                   IF(FG(nn).GT.0. .AND. ZSNOWG(nn).GT.0.) XSNOWG=1.0
C                 ENDIF
C	    	     !> Calculate snowpack properties & add drift at GRU-level
C                 TSNOW(nn)=(FCS(nn)*(TSNOCS(nn)+TFREZ)*HCPSCS(nn)*
C     1                  ZSNOCS(nn)*XSNOCS +
C     2                  FGS(nn)*(TSNOGS(nn)+TFREZ)*HCPSGS(nn)*
C     3                  ZSNOGS(nn)*XSNOGS +
C     4                  FC (nn)*(TSNOWC(nn)+TFREZ)*HCPSC(nn)*
C     5                  ZSNOWC(nn)*XSNOWC +
C     6                  FG (nn)*(TSNOWG(nn)+TFREZ)*HCPSG(nn)*
C     7                  ZSNOWG(nn)*XSNOWG)/
C     8                 (FCS(nn)*HCPSCS(nn)*ZSNOCS(nn)*XSNOCS +
C     9                  FGS(nn)*HCPSGS(nn)*ZSNOGS(nn)*XSNOGS +
C     A                  FC (nn)*HCPSC(nn)*ZSNOWC(nn)*XSNOWC +
C     B                  FG (nn)*HCPSG(nn)*ZSNOWG(nn)*XSNOWG)
C                 RHOSNO(nn)=(FCS(nn)*RHOSCS(nn)*ZSNOCS(nn)*XSNOCS +   
C     1                   FGS(nn)*RHOSGS(nn)*ZSNOGS(nn)*XSNOGS +   
C     2                   FC (nn)*RHOSC(nn)*ZSNOWC(nn)*XSNOWC +   
C     3                   FG (nn)*RHOSG(nn)*ZSNOWG(nn)*XSNOWG)/    
C     4                  (FCS(nn)*ZSNOCS(nn)*XSNOCS +
C     5                   FGS(nn)*ZSNOGS(nn)*XSNOGS +                 
C     6                   FC (nn)*ZSNOWC(nn)*XSNOWC +
C     7                   FG (nn)*ZSNOWG(nn)*XSNOWG)
C                 ZSNOW(nn)=FCS(nn)*ZSNOCS(nn) + FGS(nn)*ZSNOGS(nn) +
C     1                 FC (nn)*ZSNOWC(nn) + FG (nn)*ZSNOWG(nn)
C                 SNO(nn)=ZSNOW(nn)*RHOSNO(nn)
C                 Deposition(nn)=transport
C             ELSE
C              Deposition(nn)=0.0
C             ENDIF !(Drift(nn).GT.0.)
C            ELSE
C              Deposition(nn)=0.0
C            ENDIF !(distrib(nn).GT.0.)
C          !Not first GRU
C          ELSE
          J=nn+PrevNumTiles
          total=0.
           IF(SumDrift.GT.0.0) THEN
	    	   IF(distrib(J).GT.0.) THEN
	    	     DO 500 jj=nn,GRUsInGS  !> calculate denominator
                   total=total+distrib(jj+PrevNumTiles)
  500            CONTINUE
                 !> determine contribution and scale
                 transport=SumDrift*distrib(J)/total/Area_Weight(J)
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
                 SumDrift=SumDrift-transport*Area_Weight(J)
              ELSE
                  Deposition(J)=0.
              ENDIF !>(distrib(J).GT.0)
          ELSE
              Deposition(J)=0.
          ENDIF !>(SumDrift.GT.0.0)
C	    ENDIF !(nn.EQ.1) THEN
	   !ELSE
	   !  Deposition(J)=0.
	   !ENDIF !>(distrib(J).GE.0.0 AND nn+1.LT.nhru)
  400   CONTINUE
        !> used to track gathered tiles that have already had snow redistributed within
        PrevNumTiles=PrevNumTiles+GRUsInGS
  100 CONTINUE
  
      DO 600 K=IL1,IL2 !LOOP TILES
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
