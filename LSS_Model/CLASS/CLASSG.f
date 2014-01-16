      SUBROUTINE CLASSG(TBARGAT,THLQGAT,THICGAT,TPNDGAT,ZPNDGAT,
     1                  TBASGAT,ALBSGAT,TSNOGAT,RHOSGAT,SNOGAT, 
     2                  TCANGAT,RCANGAT,SCANGAT,GROGAT, FRZCGAT,
     2                  CMAIGAT,
     3                  FCANGAT,LNZ0GAT,ALVCGAT,ALICGAT,PAMXGAT,
     4                  PAMNGAT,CMASGAT,ROOTGAT,RSMNGAT,QA50GAT,
     5                  VPDAGAT,VPDBGAT,PSGAGAT,PSGBGAT,PAIDGAT,
     6                  HGTDGAT,ACVDGAT,ACIDGAT,TSFSGAT,WSNOGAT,
     7                  THPGAT, THRGAT, THMGAT, BIGAT,  PSISGAT,
     8                  GRKSGAT,THRAGAT,HCPSGAT,TCSGAT, IGDRGAT,
     9                  THFCGAT,PSIWGAT,DLZWGAT,ZBTWGAT,VMODGAT,
     A                  ZSNLGAT,ZPLGGAT,ZPLSGAT,TACGAT, QACGAT,
     B                  DRNGAT, XSLPGAT,XDGAT,WFSFGAT,KSGAT,
     C                  ALGWGAT,ALGDGAT,ASVDGAT,ASIDGAT,AGVDGAT,
     D                  AGIDGAT,ISNDGAT,RADJGAT,ZBLDGAT,Z0ORGAT,
     E                  ZRFMGAT,ZRFHGAT,ZDMGAT, ZDHGAT, FSVHGAT,
     F                  FSIHGAT,CSZGAT, FDLGAT, ULGAT,  VLGAT,  
     G                  TAGAT,  QAGAT,  PRESGAT,PREGAT, PADRGAT,
     H                  VPDGAT, TADPGAT,RHOAGAT,RPCPGAT,TRPCGAT,
     I                  SPCPGAT,TSPCGAT,RHSIGAT,FCLOGAT,DLONGAT,
     J                  GGEOGAT,
     K                  CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT,
     L                  QFSGAT, QFXGAT, PETGAT, GAGAT,  EFGAT,
     M                  GTGAT,  QGGAT,  ALVSGAT,ALIRGAT,
     N                  SFCTGAT,SFCUGAT,SFCVGAT,SFCQGAT,FSNOGAT,
     O                  FSGVGAT,FSGSGAT,FSGGGAT,FLGVGAT,FLGSGAT,
     P                  FLGGGAT,HFSCGAT,HFSSGAT,HFSGGAT,HEVCGAT,
     Q                  HEVSGAT,HEVGGAT,HMFCGAT,HMFNGAT,HTCCGAT,
     R                  HTCSGAT,PCFCGAT,PCLCGAT,PCPNGAT,PCPGGAT,
     S                  QFGGAT, QFNGAT, QFCLGAT,QFCFGAT,ROFGAT,
     T                  ROFOGAT,ROFSGAT,ROFBGAT,TROFGAT,TROOGAT,
     U                  TROSGAT,TROBGAT,ROFCGAT,ROFNGAT,ROVGGAT,
     V                  WTRCGAT,WTRSGAT,WTRGGAT,DRGAT,  GFLXGAT,
     W                  HMFGGAT,HTCGAT, QFCGAT, ITCTGAT,
C BEGIN: PDMROF
     1                  CMINGAT,CMAXGAT,BGAT,   K1GAT,  K2GAT,      
C END: PDMROF
     X                  ILMOS,JLMOS,IWMOS,JWMOS,
     Y                  NML,NL,NM,ILG,IG,IC,ICP1,
     Z                  TBARROW,THLQROW,THICROW,TPNDROW,ZPNDROW,
     +                  TBASROW,ALBSROW,TSNOROW,RHOSROW,SNOROW, 
     +                  TCANROW,RCANROW,SCANROW,GROROW,CMAIROW,
     +                  FCANROW,LNZ0ROW,ALVCROW,ALICROW,PAMXROW,
     +                  PAMNROW,CMASROW,ROOTROW,RSMNROW,QA50ROW,
     +                  VPDAROW,VPDBROW,PSGAROW,PSGBROW,PAIDROW,
     +                  HGTDROW,ACVDROW,ACIDROW,TSFSROW,WSNOROW,
     +                  THPROW, THRROW, THMROW, BIROW,  PSISROW,
     +                  GRKSROW,THRAROW,HCPSROW,TCSROW, IGDRROW,
     +                  THFCROW,PSIWROW,DLZWROW,ZBTWROW,VMODL,
     +                  ZSNLROW,ZPLGROW,ZPLSROW,FRZCROW,TACROW,
     + QACROW,
     +                  DRNROW, XSLPROW,XDROW,WFSFROW,KSROW,
     +                  ALGWROW,ALGDROW,ASVDROW,ASIDROW,AGVDROW,
     +                  AGIDROW,ISNDROW,RADJGRD,ZBLDGRD,Z0ORGRD,
     +                  ZRFMGRD,ZRFHGRD,ZDMGRD, ZDHGRD, FSVHGRD,
     +                  FSIHGRD,CSZGRD, FDLGRD, ULGRD,  VLGRD,  
     +                  TAGRD,  QAGRD,  PRESGRD,PREGRD, PADRGRD,
     +                  VPDGRD, TADPGRD,RHOAGRD,RPCPGRD,TRPCGRD,
     +                  SPCPGRD,TSPCGRD,RHSIGRD,FCLOGRD,DLONGRD,
     +                  GGEOGRD, MANNROW, MANNGAT, DDROW, DDGAT,
     +                  SANDROW,SANDGAT,CLAYROW,CLAYGAT,
C BEGIN: PDMROF
     1                  CMINROW, CMAXROW, BROW, K1ROW, K2ROW,
C END: PDMROF
     2                  FAREROW,FAREGAT,
     +                  fetchROW,HtROW,N_SROW,A_SROW,DistribROW,
     +                  fetchGAT,HtGAT,N_SGAT,A_SGAT,DistribGAT,
     +                  DrySnowRow,SnowAgeROW,DrySnowGAT,SnowAgeGAT,
     +                  TSNOdsROW, RHOSdsROW, TSNOdsGAT, RHOSdsGAT,
     +                  DriftROW, SublROW, DepositionROW,
     +                  DriftGAT, SublGAT,DepositionGAT,q)

C
C     * OCT 18/11 - M.LAZARE.  ADD IGDR.
C     * OCT 07/11 - M.LAZARE.  ADD VMODL->VMODGAT.
C     * OCT 05/11 - M.LAZARE.  PUT BACK IN PRESGROW->PRESGAT
C     *                        REQUIRED FOR ADDED SURFACE RH 
C     *                        CALCULATION.
C     * OCT 03/11 - M.LAZARE.  REMOVE ALL INITIALIZATION TO
C     *                        ZERO OF GAT ARRAYS (NOW DONE
C     *                        IN CLASS DRIVER).
C     * SEP 16/11 - M.LAZARE.  - ROW->ROT AND GRD->ROW.
C     *                        - REMOVE INITIALIZATION OF
C     *                          {ALVS,ALIR} TO ZERO.
C     *                        - REMOVE PRESGROW->PRESGAT 
C     *                          (OCEAN-ONLY NOW).
C     *                        - RADJROW (64-BIT) NOW RADJ
C     *                          (32-BIT).
C     * JUN 17/08 - D.HOLMAN. ADD MANNROW, MANNGAT, DDROW, DDGAT.
C     * MAR 23/06 - D.VERSEGHY. ADD WSNO,FSNO,GGEO.
C     * MAR 18/05 - D.VERSEGHY. ADDITIONAL VARIABLES.
C     * FEB 18/05 - D.VERSEGHY. ADD "TSFS" VARIABLES.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 15/02 - D.VERSEGHY. GATHER OPERATION ON CLASS 
C     *                         VARIABLES.
C 
      use MODELS, only : Nmod
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER  NML,NL,NM,ILG,IG,IC,ICP1,K,L,M,q
C
C     * LAND SURFACE PROGNOSTIC VARIABLES.
C
      REAL    TBARROW(NL,NM,IG,Nmod), THLQROW(NL,NM,IG,Nmod),
     +        THICROW(NL,NM,IG,Nmod), TPNDROW(NL,NM,Nmod),
     +        ZPNDROW(NL,NM,Nmod),    TBASROW(NL,NM,Nmod),   
     2        ALBSROW(NL,NM,Nmod),    TSNOROW(NL,NM,Nmod),
     +        RHOSROW(NL,NM,Nmod),    SNOROW (NL,NM,Nmod),
     +        TCANROW(NL,NM,Nmod),    RCANROW(NL,NM,Nmod),   
     4        SCANROW(NL,NM,Nmod),    GROROW (NL,NM,Nmod),
     +        CMAIROW(NL,NM),    TSFSROW(NL,NM,4,Nmod),
     +        TACROW (NL,NM,Nmod),    QACROW (NL,NM,Nmod),
     6        WSNOROW(NL,NM,Nmod) 
C
      REAL    TBARGAT(ILG,IG,Nmod),   THLQGAT(ILG,IG,Nmod),
     +        THICGAT(ILG,IG,Nmod),   TPNDGAT(ILG,Nmod),   
     +        ZPNDGAT(ILG,Nmod),      TBASGAT(ILG,Nmod),   
     2        ALBSGAT(ILG,Nmod),      TSNOGAT(ILG,Nmod),  
     +        RHOSGAT(ILG,Nmod),      SNOGAT (ILG,Nmod),  
     +        TCANGAT(ILG,Nmod),      RCANGAT(ILG,Nmod),   
     4        SCANGAT(ILG,Nmod),      GROGAT (ILG,Nmod),  
     +        CMAIGAT(ILG),      TSFSGAT(ILG,4,Nmod),   
     +        TACGAT (ILG,Nmod),      QACGAT (ILG,Nmod),
     6        WSNOGAT(ILG,Nmod)
C
C     * SAND AND CLAY
      REAL    SANDROW(NL,NM,IG), CLAYROW(NL,NM,IG)
      REAL    SANDGAT(ILG,IG),   CLAYGAT(ILG,IG)
C
C     * GATHER-SCATTER INDEX ARRAYS.
C
      INTEGER  ILMOS (ILG),  JLMOS  (ILG),  IWMOS  (ILG),  JWMOS (ILG)
C
C     * CANOPY AND SOIL INFORMATION ARRAYS.
C     * (THE LENGTH OF THESE ARRAYS IS DETERMINED BY THE NUMBER
C     * OF SOIL LAYERS (3) AND THE NUMBER OF BROAD VEGETATION
C     * CATEGORIES (4, OR 5 INCLUDING URBAN AREAS).)
C
      REAL          FCANROW(NL,NM,ICP1), LNZ0ROW(NL,NM,ICP1),
     1              ALVCROW(NL,NM,ICP1), ALICROW(NL,NM,ICP1),
     2              PAMXROW(NL,NM,IC),   PAMNROW(NL,NM,IC),
     3              CMASROW(NL,NM,IC),   ROOTROW(NL,NM,IC),
     4              RSMNROW(NL,NM,IC),   QA50ROW(NL,NM,IC),
     5              VPDAROW(NL,NM,IC),   VPDBROW(NL,NM,IC),
     6              PSGAROW(NL,NM,IC),   PSGBROW(NL,NM,IC),
     7              PAIDROW(NL,NM,IC),   HGTDROW(NL,NM,IC),
     8              ACVDROW(NL,NM,IC),   ACIDROW(NL,NM,IC)
C
      REAL          FCANGAT(ILG,ICP1),   LNZ0GAT(ILG,ICP1),
     1              ALVCGAT(ILG,ICP1),   ALICGAT(ILG,ICP1),
     2              PAMXGAT(ILG,IC),     PAMNGAT(ILG,IC),
     3              CMASGAT(ILG,IC),     ROOTGAT(ILG,IC),
     4              RSMNGAT(ILG,IC),     QA50GAT(ILG,IC),
     5              VPDAGAT(ILG,IC),     VPDBGAT(ILG,IC),
     6              PSGAGAT(ILG,IC),     PSGBGAT(ILG,IC),
     7              PAIDGAT(ILG,IC),     HGTDGAT(ILG,IC),
     8              ACVDGAT(ILG,IC),     ACIDGAT(ILG,IC)
C
      REAL    THPROW (NL,NM,IG), THRROW (NL,NM,IG), THMROW (NL,NM,IG),
     1        BIROW  (NL,NM,IG), PSISROW(NL,NM,IG), GRKSROW(NL,NM,IG),   
     2        THRAROW(NL,NM,IG), HCPSROW(NL,NM,IG), 
     3        TCSROW (NL,NM,IG), THFCROW(NL,NM,IG), PSIWROW(NL,NM,IG),  
     4        DLZWROW(NL,NM,IG), ZBTWROW(NL,NM,IG), 
     5        DRNROW (NL,NM),    XSLPROW(NL,NM),    XDROW(NL,NM),
     6        WFSFROW(NL,NM),    KSROW(NL,NM),    ALGWROW(NL,NM),   
     7        ALGDROW(NL,NM),    ASVDROW(NL,NM),    ASIDROW(NL,NM),   
     8        AGVDROW(NL,NM),    AGIDROW(NL,NM),    ZSNLROW(NL,NM),
     9        ZPLGROW(NL,NM),    ZPLSROW(NL,NM),    FRZCROW(NL,NM),
     +        fetchROW(NL,NM),   HtROW(NL,NM),     N_SROW(NL,NM),
     +        A_SROW(NL,NM),     DistribROW(NL,NM),
     +        DrySnowRow(NL,NM,Nmod), SnowAgeROW(NL,NM,Nmod),
     +        TSNOdsROW(NL,NM,Nmod),  RHOSdsROW(NL,NM,Nmod),
     +        DriftROW(NL,NM,Nmod),   SublROW(NL,NM,Nmod), 
     +        DepositionROW(NL,NM,Nmod), FAREROW(NL,NM)
C

      REAL    THPGAT (ILG,IG),   THRGAT (ILG,IG),   THMGAT (ILG,IG),
     1        BIGAT  (ILG,IG),   PSISGAT(ILG,IG),   GRKSGAT(ILG,IG),   
     2        THRAGAT(ILG,IG),   HCPSGAT(ILG,IG), 
     3        TCSGAT (ILG,IG),   THFCGAT(ILG,IG),   PSIWGAT(ILG,IG),  
     4        DLZWGAT(ILG,IG),   ZBTWGAT(ILG,IG),   
     5        DRNGAT (ILG),      XSLPGAT(ILG),      XDGAT(ILG),
     6        WFSFGAT(ILG),      KSGAT(ILG),      ALGWGAT(ILG),     
     7        ALGDGAT(ILG),      ASVDGAT(ILG),      ASIDGAT(ILG),     
     8        AGVDGAT(ILG),      AGIDGAT(ILG),      ZSNLGAT(ILG),
     9        ZPLGGAT(ILG),      ZPLSGAT(ILG),      FRZCGAT(ILG),
     +        fetchGAT(ILG),     HtGAT(ILG),        N_SGAT(ILG),
     +        A_SGAT(ILG),       DistribGAT(ILG),
     +        DrySnowGAT(ILG,Nmod),   SnowAgeGAT(ILG,Nmod),
     +        TSNOdsGAT (ILG,Nmod),   RHOSdsGAT(ILG,Nmod),
     +        DriftGAT(ILG,Nmod),     SublGAT(ILG,Nmod), 
     +        DepositionGAT(ILG,Nmod),FAREGAT(ILG)
C
      INTEGER ISNDROW(NL,NM,IG), ISNDGAT(ILG,IG)
      INTEGER IGDRROW(NL,NM),    IGDRGAT(ILG)

C     * ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES.
C
      REAL  ZRFMGRD( NL), ZRFHGRD( NL), ZDMGRD ( NL), ZDHGRD ( NL),
     1      FSVHGRD( NL), FSIHGRD( NL), CSZGRD ( NL), FDLGRD ( NL), 
     2      ULGRD  ( NL), VLGRD  ( NL), TAGRD  ( NL), QAGRD  ( NL), 
     3      PRESGRD( NL), PREGRD ( NL), PADRGRD( NL), VPDGRD ( NL), 
     4      TADPGRD( NL), RHOAGRD( NL), ZBLDGRD( NL), Z0ORGRD( NL),
     5      RPCPGRD( NL), TRPCGRD( NL), SPCPGRD( NL), TSPCGRD( NL),
     6      RHSIGRD( NL,Nmod), FCLOGRD( NL), DLONGRD( NL), GGEOGRD( NL),
     7      RADJGRD( NL), VMODL  ( NL)
C
      REAL  ZRFMGAT(ILG), ZRFHGAT(ILG), ZDMGAT (ILG), ZDHGAT (ILG),
     1      FSVHGAT(ILG), FSIHGAT(ILG), CSZGAT (ILG), FDLGAT (ILG), 
     2      ULGAT  (ILG), VLGAT  (ILG), TAGAT  (ILG), QAGAT  (ILG), 
     3      PRESGAT(ILG), PREGAT (ILG), PADRGAT(ILG), VPDGAT (ILG), 
     4      TADPGAT(ILG), RHOAGAT(ILG), ZBLDGAT(ILG), Z0ORGAT(ILG),
     5      RPCPGAT(ILG), TRPCGAT(ILG), SPCPGAT(ILG), TSPCGAT(ILG),
     6      RHSIGAT(ILG,Nmod), FCLOGAT(ILG), DLONGAT(ILG), GGEOGAT(ILG),
     7      RADJGAT(ILG), VMODGAT(ILG)
C
C     * LAND SURFACE DIAGNOSTIC VARIABLES.
C
      REAL  CDHGAT (ILG),  CDMGAT (ILG,Nmod),  HFSGAT (ILG), 
     +      TFXGAT (ILG),  QEVPGAT(ILG),  QFSGAT (ILG),
     +      QFXGAT (ILG),  PETGAT (ILG),  GAGAT  (ILG),
     +      EFGAT  (ILG),  GTGAT  (ILG),  QGGAT  (ILG), 
     3      ALVSGAT(ILG),  ALIRGAT(ILG),  FSNOGAT(ILG),
     4      SFCTGAT(ILG),  SFCUGAT(ILG),  SFCVGAT(ILG),
     +      SFCQGAT(ILG),  FSGVGAT(ILG),  FSGSGAT(ILG), 
     +      FSGGGAT(ILG),  FLGVGAT(ILG),  FLGSGAT(ILG),  
     +      FLGGGAT(ILG),  HFSCGAT(ILG),  HFSSGAT(ILG),
     7      HFSGGAT(ILG),  HEVCGAT(ILG),  HEVSGAT(ILG), 
     +      HEVGGAT(ILG),  HMFCGAT(ILG),  HMFNGAT(ILG),
     +      HTCCGAT(ILG),  HTCSGAT(ILG),  PCFCGAT(ILG),
     +      PCLCGAT(ILG),  PCPNGAT(ILG),  PCPGGAT(ILG), 
     A      QFGGAT (ILG),  QFNGAT (ILG),  QFCLGAT(ILG), 
     +      QFCFGAT(ILG),  ROFGAT (ILG),  ROFOGAT(ILG),
     +      ROFSGAT(ILG),  ROFBGAT(ILG),  TROFGAT(ILG),
     +      TROOGAT(ILG),  TROSGAT(ILG),  TROBGAT(ILG),
     D      ROFCGAT(ILG),  ROFNGAT(ILG),  ROVGGAT(ILG),
     +      WTRCGAT(ILG), 
     E      WTRSGAT(ILG),  WTRGGAT(ILG),  DRGAT  (ILG)
C
      REAL    HMFGGAT(ILG,IG),  HTCGAT (ILG,IG),  
     1        QFCGAT (ILG,IG),  GFLXGAT(ILG,IG)
C
      INTEGER        ITCTGAT(ILG,6,50)
      
C * WATROF DECLARATIONS
      REAL  DDROW(NL,NM),MANNROW(NL,NM),DDGAT(ILG),MANNGAT(ILG)

C * PDMROF DECLARATIONS
      REAL  CMAXROW(NL,NM), CMINROW(NL,NM), BROW(NL,NM),
     1      K1ROW(NL,NM),   K2ROW(NL,NM)
      REAL  CMINGAT(ILG),   CMAXGAT(ILG),   BGAT(ILG),
     1      K1GAT(ILG),     K2GAT(ILG)
C
C----------------------------------------------------------------------
!$omp parallel do
      DO 100 K=1,NML
          DDGAT(K)=DDROW(ILMOS(K),JLMOS(K))
          MANNGAT(K)=MANNROW(ILMOS(K),JLMOS(K))
          TPNDGAT(K,q)=TPNDROW(ILMOS(K),JLMOS(K),q)  
          ZPNDGAT(K,q)=ZPNDROW(ILMOS(K),JLMOS(K),q)  
          TBASGAT(K,q)=TBASROW(ILMOS(K),JLMOS(K),q)  
          ALBSGAT(K,q)=ALBSROW(ILMOS(K),JLMOS(K),q)  
          TSNOGAT(K,q)=TSNOROW(ILMOS(K),JLMOS(K),q)  
          RHOSGAT(K,q)=RHOSROW(ILMOS(K),JLMOS(K),q)  
          SNOGAT (K,q)=SNOROW (ILMOS(K),JLMOS(K),q)  
          WSNOGAT(K,q)=WSNOROW(ILMOS(K),JLMOS(K),q)  
          TCANGAT(K,q)=TCANROW(ILMOS(K),JLMOS(K),q)  
          RCANGAT(K,q)=RCANROW(ILMOS(K),JLMOS(K),q)  
          SCANGAT(K,q)=SCANROW(ILMOS(K),JLMOS(K),q)  
          GROGAT (K,q)=GROROW (ILMOS(K),JLMOS(K),q)  
          CMAIGAT(K)=CMAIROW(ILMOS(K),JLMOS(K))  
          DRNGAT (K)=DRNROW (ILMOS(K),JLMOS(K))  
          XSLPGAT(K)=XSLPROW(ILMOS(K),JLMOS(K))  
          XDGAT(K)=XDROW(ILMOS(K),JLMOS(K))  
c         WFSFGAT(K)=WFSFROW(ILMOS(K),JLMOS(K))  
          KSGAT(K)=KSROW(ILMOS(K),JLMOS(K))  
          ALGWGAT(K)=ALGWROW(ILMOS(K),JLMOS(K))  
          ALGDGAT(K)=ALGDROW(ILMOS(K),JLMOS(K))  
c         ASVDGAT(K)=ASVDROW(ILMOS(K),JLMOS(K))  
c         ASIDGAT(K)=ASIDROW(ILMOS(K),JLMOS(K))  
c         AGVDGAT(K)=AGVDROW(ILMOS(K),JLMOS(K))  
c         AGIDGAT(K)=AGIDROW(ILMOS(K),JLMOS(K))  
          ZSNLGAT(K)=ZSNLROW(ILMOS(K),JLMOS(K))  
          ZPLGGAT(K)=ZPLGROW(ILMOS(K),JLMOS(K))  
          ZPLSGAT(K)=ZPLSROW(ILMOS(K),JLMOS(K))  
          FRZCGAT (K)=FRZCROW (ILMOS(K),JLMOS(K))            
          TACGAT (K,q)=TACROW (ILMOS(K),JLMOS(K),q)  
          QACGAT (K,q)=QACROW (ILMOS(K),JLMOS(K),q)  
          FAREGAT(K)=FAREROW(ILMOS(K),JLMOS(K))
          IGDRGAT(K)=IGDRROW(ILMOS(K),JLMOS(K))
          ZBLDGAT(K)=ZBLDGRD(ILMOS(K))
          Z0ORGAT(K)=Z0ORGRD(ILMOS(K))
          ZRFMGAT(K)=ZRFMGRD(ILMOS(K))
          ZRFHGAT(K)=ZRFHGRD(ILMOS(K))
          ZDMGAT (K)=ZDMGRD(ILMOS(K))
          ZDHGAT (K)=ZDHGRD(ILMOS(K))
!          FSVHGAT(K)=FSVHGRD(ILMOS(K))
!          FSIHGAT(K)=FSIHGRD(ILMOS(K))
          CSZGAT (K)=CSZGRD (ILMOS(K))
!          FDLGAT (K)=FDLGRD (ILMOS(K))
!          ULGAT  (K)=ULGRD  (ILMOS(K))
!          VLGAT  (K)=VLGRD  (ILMOS(K))
!          TAGAT  (K)=TAGRD  (ILMOS(K))
!          QAGAT  (K)=QAGRD  (ILMOS(K))
!          PRESGAT(K)=PRESGRD(ILMOS(K))
!          PREGAT (K)=PREGRD (ILMOS(K))
!          PADRGAT(K)=PADRGRD(ILMOS(K))
!          VPDGAT (K)=VPDGRD (ILMOS(K))
!          TADPGAT(K)=TADPGRD(ILMOS(K))
!          RHOAGAT(K)=RHOAGRD(ILMOS(K))
!          RPCPGAT(K)=RPCPGRD(ILMOS(K))
!          TRPCGAT(K)=TRPCGRD(ILMOS(K))
!          SPCPGAT(K)=SPCPGRD(ILMOS(K))
!          TSPCGAT(K)=TSPCGRD(ILMOS(K))
!          RHSIGAT(K)=RHSIGRD(ILMOS(K))
          FCLOGAT(K)=FCLOGRD(ILMOS(K))
          DLONGAT(K)=DLONGRD(ILMOS(K))
          GGEOGAT(K)=GGEOGRD(ILMOS(K))
          RADJGAT(K)=RADJGRD(ILMOS(K))
          fetchGAT  (K)=fetchROW  (ILMOS(K),JLMOS(K))
          HtGAT     (K)=HtROW     (ILMOS(K),JLMOS(K))
          N_SGAT    (K)=N_SROW    (ILMOS(K),JLMOS(K))
          A_SGAT    (K)=A_SROW    (ILMOS(K),JLMOS(K))
          DistribGAT(K)=DistribROW(ILMOS(K),JLMOS(K))
          DrySnowGAT(K,q)=DrySnowRow(ILMOS(K),JLMOS(K),q)
          SnowAgeGAT(K,q)=SnowAgeROW(ILMOS(K),JLMOS(K),q)
          TSNOdsGAT (K,q)=TSNOdsROW (ILMOS(K),JLMOS(K),q)
          RHOSdsGAT (K,q)=RHOSdsROW (ILMOS(K),JLMOS(K),q)
          DriftGAT (K,q)=0.0 !DriftROW (ILMOS(K),JLMOS(K))
          SublGAT (K,q)=0.0 !SublROW (ILMOS(K),JLMOS(K))
          DepositionGAT(K,q)=0.0
          VMODGAT(K)=VMODL  (ILMOS(K))
          CMINGAT (K) = CMINROW (ILMOS(K),JLMOS(K))            
          CMAXGAT (K) = CMAXROW (ILMOS(K),JLMOS(K))            
          BGAT    (K) = BROW    (ILMOS(K),JLMOS(K))            
          K1GAT   (K) = K1ROW   (ILMOS(K),JLMOS(K))            
          K2GAT   (K) = K2ROW   (ILMOS(K),JLMOS(K))            
100   CONTINUE
C
      DO 250 L=1,IG
      !$omp parallel do
      DO 200 K=1,NML
          TBARGAT(K,L,q)=TBARROW(ILMOS(K),JLMOS(K),L,q)
          THLQGAT(K,L,q)=THLQROW(ILMOS(K),JLMOS(K),L,q)
          THICGAT(K,L,q)=THICROW(ILMOS(K),JLMOS(K),L,q)
          SANDGAT(K,L)=SANDROW(ILMOS(K),JLMOS(K),L)
          CLAYGAT(K,L)=CLAYROW(ILMOS(K),JLMOS(K),L)
          THPGAT (K,L)=THPROW (ILMOS(K),JLMOS(K),L)
          THRGAT (K,L)=THRROW (ILMOS(K),JLMOS(K),L)
          THMGAT (K,L)=THMROW (ILMOS(K),JLMOS(K),L)
          BIGAT  (K,L)=BIROW  (ILMOS(K),JLMOS(K),L)
          PSISGAT(K,L)=PSISROW(ILMOS(K),JLMOS(K),L)
          GRKSGAT(K,L)=GRKSROW(ILMOS(K),JLMOS(K),L)
          THRAGAT(K,L)=THRAROW(ILMOS(K),JLMOS(K),L)
          HCPSGAT(K,L)=HCPSROW(ILMOS(K),JLMOS(K),L)
          TCSGAT (K,L)=TCSROW (ILMOS(K),JLMOS(K),L)
          THFCGAT(K,L)=THFCROW(ILMOS(K),JLMOS(K),L)
          PSIWGAT(K,L)=PSIWROW(ILMOS(K),JLMOS(K),L)
          DLZWGAT(K,L)=DLZWROW(ILMOS(K),JLMOS(K),L)
          ZBTWGAT(K,L)=ZBTWROW(ILMOS(K),JLMOS(K),L)
          ISNDGAT(K,L)=ISNDROW(ILMOS(K),JLMOS(K),L)
  200 CONTINUE
  250 CONTINUE
C
      DO 300 L=1,ICP1
      !$omp parallel do
      DO 300 K=1,NML
          FCANGAT(K,L)=FCANROW(ILMOS(K),JLMOS(K),L)
          LNZ0GAT(K,L)=LNZ0ROW(ILMOS(K),JLMOS(K),L)
          ALVCGAT(K,L)=ALVCROW(ILMOS(K),JLMOS(K),L)
          ALICGAT(K,L)=ALICROW(ILMOS(K),JLMOS(K),L)
300   CONTINUE
C
      DO 400 L=1,IC
      !$omp parallel do
      DO 400 K=1,NML
          PAMXGAT(K,L)=PAMXROW(ILMOS(K),JLMOS(K),L)
          PAMNGAT(K,L)=PAMNROW(ILMOS(K),JLMOS(K),L)
          CMASGAT(K,L)=CMASROW(ILMOS(K),JLMOS(K),L)
          ROOTGAT(K,L)=ROOTROW(ILMOS(K),JLMOS(K),L)
          RSMNGAT(K,L)=RSMNROW(ILMOS(K),JLMOS(K),L)
          QA50GAT(K,L)=QA50ROW(ILMOS(K),JLMOS(K),L)
          VPDAGAT(K,L)=VPDAROW(ILMOS(K),JLMOS(K),L)
          VPDBGAT(K,L)=VPDBROW(ILMOS(K),JLMOS(K),L)
          PSGAGAT(K,L)=PSGAROW(ILMOS(K),JLMOS(K),L)
          PSGBGAT(K,L)=PSGBROW(ILMOS(K),JLMOS(K),L)
c         PAIDGAT(K,L)=PAIDROW(ILMOS(K),JLMOS(K),L)
c         HGTDGAT(K,L)=HGTDROW(ILMOS(K),JLMOS(K),L)
c         ACVDGAT(K,L)=ACVDROW(ILMOS(K),JLMOS(K),L)
c         ACIDGAT(K,L)=ACIDROW(ILMOS(K),JLMOS(K),L)
          TSFSGAT(K,L,q)=TSFSROW(ILMOS(K),JLMOS(K),L,q)
400   CONTINUE

      RETURN
      END
