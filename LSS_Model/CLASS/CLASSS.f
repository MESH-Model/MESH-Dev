      SUBROUTINE CLASSS(TBARROW,THLQROW,THICROW,GFLXROW,TSFSROW,
     1                  TPNDROW,ZPNDROW,TBASROW,ALBSROW,TSNOROW, 
     2                  RHOSROW,SNOROW, TCANROW,RCANROW,SCANROW, 
     3                  GROROW, CMAIROW,TACROW, QACROW, WSNOROW,
     4                  ILMOS,JLMOS,IWMOS,JWMOS,
     5                  NML,NL,NM,ILG,IG,IC,ICP1,
     6                  TBARGAT,THLQGAT,THICGAT,GFLXGAT,TSFSGAT,
     7                  TPNDGAT,ZPNDGAT,TBASGAT,ALBSGAT,TSNOGAT,
     8                  RHOSGAT,SNOGAT, TCANGAT,RCANGAT,SCANGAT,
     9                  GROGAT, CMAIGAT,TACGAT, QACGAT, WSNOGAT,
     A                  MANNROW,MANNGAT,DDROW,DDGAT,
     B                  SANDROW,SANDGAT,CLAYROW,CLAYGAT,XSLPROW,XSLPGAT,
     +                  DrySnowRow,SnowAgeROW,DrySnowGAT,SnowAgeGAT,
     +                  TSNOdsROW, RHOSdsROW, TSNOdsGAT, RHOSdsGAT,
     +                  DriftROW, SublROW, DepositionROW,
     +                  DriftGAT, SublGAT, DepositionGAT,q)
C
C     * OCT 25/11 - M.LAZARE.   REMOVE OPERATIONS ON INTERNAL
C     *                         ROT ARRAYS (NOW DONE DIRECTLY
C     *                         GAT->ROW IN SFCPROC).
C     * OCT 07/11 - M.LAZARE.   REMOVE TSF.
C     * OCT 05/11 - M.LAZARE.   ADD SFCH.
C     * OCT 04/11 - M.LAZARE.   REMOVE ITCT.
C     * MAR 23/06 - D.VERSEGHY. ADD WSNO,FSNO.
C     * MAR 18/05 - D.VERSEGHY. ADDITIONAL VARIABLES.
C     * FEB 18/05 - D.VERSEGHY. ADD "TSFS" VARIABLES.
C     * AUG 05/04 - D.VERSEGHY. ADD NEW DIAGNOSTIC VARIABLES
C     *                         ILMO, UE AND HBL.
C     * AUG 15/02 - D.VERSEGHY. SCATTER OPERATION ON CLASS 
C     *                         VARIABLES.
C
      use MODELS, only : Nmod
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER NML,NL,NM,ILG,IG,IC,ICP1,K,L,M,q
C
C     * LAND SURFACE PROGNOSTIC VARIABLES.
C
      REAL    TBARROW(NL,NM,IG,Nmod), THLQROW(NL,NM,IG,Nmod),  
     1        THICROW(NL,NM,IG,Nmod), GFLXROW(NL,NM,IG)

      REAL    TSFSROW(NL,NM,4,Nmod)

      REAL    TPNDROW(NL,NM,Nmod),    ZPNDROW(NL,NM,Nmod),    
     +        TBASROW(NL,NM,Nmod),    ALBSROW(NL,NM,Nmod),   
     +        TSNOROW(NL,NM,Nmod),    RHOSROW(NL,NM,Nmod),   
     2        SNOROW (NL,NM,Nmod),    TCANROW(NL,NM,Nmod),
     +        RCANROW(NL,NM,Nmod),    SCANROW(NL,NM,Nmod),    
     +        GROROW (NL,NM,Nmod),    TACROW (NL,NM,Nmod),  
     4        QACROW (NL,NM,Nmod),    WSNOROW(NL,NM,Nmod),
     +        CMAIROW(NL,NM) 
C
      REAL    TBARGAT(ILG,IG,Nmod),   THLQGAT(ILG,IG,Nmod),
     +        THICGAT(ILG,IG,Nmod),   GFLXGAT(ILG,IG)

      REAL    TSFSGAT(ILG,4,Nmod)

      REAL    TPNDGAT(ILG,Nmod),      ZPNDGAT(ILG,Nmod),      
     +        TBASGAT(ILG,Nmod),      ALBSGAT(ILG,Nmod),     
     +        TSNOGAT(ILG,Nmod),      RHOSGAT(ILG,Nmod),   
     2        SNOGAT (ILG,Nmod),      TCANGAT(ILG,Nmod),    
     +        RCANGAT(ILG,Nmod),      SCANGAT(ILG,Nmod),     
     +        GROGAT (ILG,Nmod),      TACGAT (ILG,Nmod),  
     4        QACGAT (ILG,Nmod),      WSNOGAT(ILG,Nmod),      
     +        CMAIGAT(ILG)

C     * SAND AND CLAY
      REAL    SANDROW(NL,NM,IG), CLAYROW(NL,NM,IG)
      REAL    SANDGAT(ILG,IG),   CLAYGAT(ILG,IG)
C
C     * GATHER-SCATTER INDEX ARRAYS.
C
      INTEGER  ILMOS (ILG),  JLMOS  (ILG),  IWMOS  (ILG),  JWMOS (ILG)
C
C     * PBSM VARIABLES.
C
      REAL  DrySnowRow(NL,NM,Nmod),SnowAgeROW(NL,NM,Nmod),
     1      TSNOdsROW(NL,NM,Nmod),  RHOSdsROW(NL,NM,Nmod),
     2      DriftROW(NL,NM,Nmod), SublROW(NL,NM,Nmod), 
     +      DepositionROW(NL,NM,Nmod)
C
      REAL  DrySnowGAT(ILG,Nmod), SnowAgeGAT(ILG,Nmod),
     H      TSNOdsGAT (ILG,Nmod), RHOSdsGAT(ILG,Nmod),
     I      DriftGAT  (ILG,Nmod), SublGAT(ILG,Nmod), 
     +      DepositionGAT(ILG,Nmod)

C * WATROF DECLARATIONS
      REAL  DDROW(NL,NM),MANNROW(NL,NM),DDGAT(ILG),MANNGAT(ILG)
      REAL  XSLPROW(NL,NM),XSLPGAT(ILG)
C
C----------------------------------------------------------------------
!$omp parallel do
      DO 100 K=1,NML
          TPNDROW(ILMOS(K),JLMOS(K),q)=TPNDGAT(K,q)  
          ZPNDROW(ILMOS(K),JLMOS(K),q)=ZPNDGAT(K,q)  
          TBASROW(ILMOS(K),JLMOS(K),q)=TBASGAT(K,q)  
          ALBSROW(ILMOS(K),JLMOS(K),q)=ALBSGAT(K,q)  
          TSNOROW(ILMOS(K),JLMOS(K),q)=TSNOGAT(K,q)  
          RHOSROW(ILMOS(K),JLMOS(K),q)=RHOSGAT(K,q)  
          SNOROW (ILMOS(K),JLMOS(K),q)=SNOGAT (K,q)  
          WSNOROW(ILMOS(K),JLMOS(K),q)=WSNOGAT(K,q)  
          TCANROW(ILMOS(K),JLMOS(K),q)=TCANGAT(K,q)  
          RCANROW(ILMOS(K),JLMOS(K),q)=RCANGAT(K,q)  
          SCANROW(ILMOS(K),JLMOS(K),q)=SCANGAT(K,q)  
          GROROW (ILMOS(K),JLMOS(K),q)=GROGAT (K,q)  
          TACROW (ILMOS(K),JLMOS(K),q)=TACGAT (K,q)  
          QACROW (ILMOS(K),JLMOS(K),q)=QACGAT (K,q)  
          CMAIROW(ILMOS(K),JLMOS(K))=CMAIGAT(K)  
          DDROW (ILMOS(K),JLMOS(K))=DDGAT(K)  
          MANNROW (ILMOS(K),JLMOS(K))=MANNGAT(K)
          DrySnowRow(ILMOS(K),JLMOS(K),q)=DrySnowGAT(K,q)
          SnowAgeROW(ILMOS(K),JLMOS(K),q)=SnowAgeGAT(K,q)
          TSNOdsROW(ILMOS(K),JLMOS(K),q)=TSNOdsGAT(K,q)
          RHOSdsROW(ILMOS(K),JLMOS(K),q)=RHOSdsGAT(K,q)
          DriftROW(ILMOS(K),JLMOS(K),q)=DriftGAT(K,q)
          SublROW(ILMOS(K),JLMOS(K),q)=SublGAT(K,q)
          DepositionROW(ILMOS(K),JLMOS(K),q)=DepositionGAT(K,q)
          XSLPROW (ILMOS(K),JLMOS(K))=XSLPGAT(K)
  100 CONTINUE
C
      DO 200 L=1,IG
      !$omp parallel do
      DO 200 K=1,NML
          TBARROW(ILMOS(K),JLMOS(K),L,q)=TBARGAT(K,L,q)
          THLQROW(ILMOS(K),JLMOS(K),L,q)=THLQGAT(K,L,q)
          THICROW(ILMOS(K),JLMOS(K),L,q)=THICGAT(K,L,q)
          GFLXROW(ILMOS(K),JLMOS(K),L)=GFLXGAT(K,L)
          SANDROW(ILMOS(K),JLMOS(K),L)=SANDGAT(K,L)
          CLAYROW(ILMOS(K),JLMOS(K),L)=CLAYGAT(K,L)
  200 CONTINUE
C
      DO 300 L=1,4
      !$omp parallel do
      DO 300 K=1,NML
          TSFSROW(ILMOS(K),JLMOS(K),L,q)=TSFSGAT(K,L,q)
  300 CONTINUE
C
      RETURN
      END
