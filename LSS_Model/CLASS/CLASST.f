      SUBROUTINE CLASST (TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG,
     1   THICEC, THICEG, HCPC,   HCPG,   TCTOPC, TCBOTC, TCTOPG, TCBOTG,
     2   GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,   G12CS,  G12GS,  
     3   G23C,   G23G,   G23CS,  G23GS,  QFREZC, QFREZG, QMELTC, QMELTG, 
     4   EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS, TCANO,  TCANS,  
     5   RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP,  CHCAPS, TPONDC, TPONDG,  
     6   TPNDCS, TPNDGS, TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     7   ITERCT, CDH,    CDM,    QSENS,  TFLUX,  QEVAP,  EVAP,   QFLUX,  
     8   EVPPOT, ACOND,  EVAPB,  GT,     QG, 
     9   ST,     SU,     SV,     SQ,     SRH,
     A   FSGV,   FSGS,   FSGG,   FLGV,   FLGS,   FLGG,   
     B   HFSC,   HFSS,   HFSG,   HEVC,   HEVS,   HEVG,   HMFC,   HMFN, 
     C   HTCC,   HTCS,   HTC,    QFCF,   QFCL,   DRAG,   WTABLE, ILMO,    
     D   UE,     HBL,    TAC,    QAC,    ZREFM,  ZREFH,  ZDIAGM, ZDIAGH, 
     E   VPD,    TADP,   RHOAIR, QSWINV, QSWINI, QLWIN,  UWIND,  VWIND,   
     F   TA,     QA,     PADRY,  FC,     FG,     FCS,    FGS,    RBCOEF,
     G   FSVF,   FSVFS,  PRESSG, VMOD,   ALVSCN, ALIRCN, ALVSG,  ALIRG,  
     H   ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, ALIRSC,
     I   TRVSCN, TRIRCN, TRVSCS, TRIRCS, RC,     RCS,    WTRG,   QLWAVG,
     J   FRAINC, FSNOWC, FRAICS, FSNOCS, CMASSC, CMASCS, DISP,   DISPS,  
     K   ZOMLNC, ZOELNC, ZOMLNG, ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, 
     L   TBAR,   THLIQ,  THICE,  TPOND,  ZPOND,  TBASE,  TCAN,   TSNOW,  
     M   ZSNOW,  TRSNOW, RHOSNO, WSNOW,  THPOR,  THLRET, THLMIN, THFC,   
     N   RADJ,   PCPR,   HCPS,   TCS,    TSFSAV, DELZ,   DELZW,  ZBOTW,  
     O   FTEMP,  FVAP,   RIB,   
     P   ISAND,
     Q   AILCG,          AILCGS,         FCANC,          FCANCS,
     R   CO2CONC,        CO2I1CG,        CO2I1CS,        CO2I2CG,
     S   CO2I2CS,        COSZS,          XDIFFUS,        SLAI,
     T   ICTEM,          ICTEMMOD,       RMATCTEM,       FCANCMX,
     U   L2MAX,          NOL2PFTS,       CFLUXCG,        CFLUXCS,
     V   ANCSVEG,        ANCGVEG,        RMLCSVEG,       RMLCGVEG,
     W   FIELDSM,        WILTSM,
     X   ITC,    ITCG,   ITG,    ILG,    IL1,IL2,JL,N,   IC,     
     Y   IG,     IZREF,  ISLFD,  NLANDCS,NLANDGS,NLANDC, NLANDG, NLANDI,
     Z   q,FCANMX,ICP1,GROWTH) 
C
C     * NOV 11/11 - M.LAZARE.   IMPLEMENT CTEM (INITIALIZATION OF FIELDS
C     *                         NEAR BEGINNING AND TWO REVISED CALLS TO 
C     *                         TSOLVC).                 
C     * OCT 12/11 - M.LAZARE.   REMOVED "TSURF" (REQUIRED CHANGE
C     *                         TO TPREP INITIALIZATION AS WELL).
C     * OCT 07/11 - M.LAZARE.   - CHANGE QLWAVG FROM AN INTERNAL WORK
C     *                           ARRAY TO ONE PASSED OUT TO THE CLASS
C     *                           DRIVER, TO ACCOMODATE RPN.
C     *                         - WIND SPEED NOW PASSED IN (POSSIBLY
C     *                           CONTAINING GUSTINESS FACTOR) AS "VMOD",
C     *                           INSTEAD OF CALCULATING IT LOCALLY.
C     * OCT 05/11 - M.LAZARE.   ADD CALCULATION OF SRH (REQUIRES PASSING
C     *                         IN OF PRESSG PLUS ADDITIONAL INTERNAL
C     *                         WORK ARRAYS).  
C     * APR 28/10 - D.VERSEGHY. REVISE CALCULATION OF QG.
C     * APR 28/10 - M.MACDONALD/D.VERSEGHY. CORRECT CALCULATIONS OF
C     *                         CRIB, DRAG AND VAC FOR ISLFD=1.
C     * APR 28/10 - E.CHAN/D/VERSEGHY. CORRECT CALCULATIONS OF ST AND
C     *                         SQ FOR ISLFD=0.
C     * DEC 21/09 - D.VERSEGHY. CORRECT BUG IN CALL TO TSOLVC IN CS
C     *                         SUBAREA (CALL WITH FSNOCS AND RAICNS).
C     * DEC 07/09 - D.VERSEGHY. ADD EVAP TO TSOLVC CALL.
C     * JAN 06/09 - D.VERSEGHY. INSERT UPDATES TO HTC AND WTRG 
C     *                         BRACKETTING LOOP 60; CORRECT TPOTA,
C     *                         ZRSLDM AND ZRSLDH CALCULATIONS; USE
C     *                         TPOTA IN SLDIAG CALL; ASSUME THAT TA IS 
C     *                         ADIABATIC EXTRAPOLATE TO SURFACE FOR 
C     *                         ATMOSPHERIC MODELS.
C     * NOV 03/08 - L.DUARTE    CORRECTED CALL TO TSOLVC.
C     * AUG    08 - JP PAQUIN   OUTPUT FTEMP, FVAP AND RIB FOR GEM
C                               (IMPLEMENTED BY L.DUARTE ON OCT 28/08)
C     * FEB 25/08 - D.VERSEGHY. MODIFICATIONS REFLECTING CHANGES 
C     *                         ELSEWHERE IN CODE.
C     * NOV 24/06 - D.VERSEGHY. REMOVE CALL TO TZTHRM; MAKE RADJ REAL.
C     * AUG 16/06 - D.VERSEGHY. NEW CALLS TO TSPREP AND TSPOST.
C     * APR 13/06 - D.VERSEGHY. SEPARATE GROUND AND SNOW ALBEDOS FOR 
C     *                         OPEN AND CANOPY-COVERED AREAS.
C     * MAR 23/06 - D.VERSEGHY. CHANGES TO ADD MODELLING OF WSNOW.
C     * MAR 21/06 - P.BARTLETT. PASS ADDITIONAL VARIABLES TO TPREP.
C     * OCT 04/05 - D.VERSEGHY. MODIFICATIONS TO ALLOW OPTION OF SUB-
C     *                         DIVIDING THIRD SOIL LAYER.
C     * APR 12/05 - D.VERSEGHY. VARIOUS NEW FIELDS; ADD CALL TO NEW
C     *                         SUBROUTINE TZTHRM; MOVE CALCULATION
C     *                         OF CPHCHC INTO TSOLVC.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 05/04 - Y.DELAGE/D.VERSEGHY. NEW DIAGNOSTIC VARIABLES
C     *                         ILMO, UE AND HBL.
C     * NOV 07/02 - Y.DELAGE/D.VERSEGHY. CALLS TO NEW DIAGNOSTIC
C     *                         SUBROUTINES "SLDIAG" AND "DIASURF";
C     *                         MODIFICATIONS TO ACCOMMODATE DIFFERENT
C     *                         SURFACE REFERENCE HEIGHT CONVENTIONS.
C     * JUL 31/02 - D.VERSEGHY. MOVE CALCULATION OF VEGETATION STOMATAL
C     *                         RESISTANCE FROM TPREP INTO APREP AND
C     *                         CANALB; SHORTENED CLASS3 COMMON BLOCK.
C     * JUL 23/02 - D.VERSEGHY. MOVE ADDITION OF AIR TO CANOPY MASS
C     *                         INTO CLASSA; SHORTENED CLASS4
C     *                         COMMON BLOCK.
C     * MAR 28/02 - D.VERSEGHY. STREAMLINED SUBROUTINE CALLS.
C     * MAR 22/02 - D.VERSEGHY. MOVE CALCULATION OF BACKGROUND SOIL 
C     *                         PROPERTIES INTO "CLASSB"; ADD NEW
C     *                         DIAGNOSTIC VARIABLES "EVPPOT", "ACOND" 
C     *                         AND "TSURF"; MODIFY CALCULATIONS OF VAC,
C     *                         EVAPB AND QG.
C     * JAN 18/02 - D.VERSEGHY. CHANGES TO INCORPORATE NEW BARE SOIL
C     *                         EVAPORATION FORMULATION.
C     * APR 11/01 - M.LAZARE.   SHORTENED "CLASS2" COMMON BLOCK.
C     * SEP 19/00 - D.VERSEGHY. PASS VEGETATION-VARYING COEFFICIENTS
C     *                         TO TPREP FOR CALCULATION OF STOMATAL
C     *                         RESISTANCE.
C     * DEC 16/99 - A.WU/D.VERSEGHY. CHANGES MADE TO INCORPORATE NEW SOIL
C     *                              EVAPORATION ALGORITHMS AND NEW CANOPY
C     *                              TURBULENT FLUX FORMULATION.  MODIFY
C     *                              CALCULATION OF BULK RICHARDSON NUMBER
C     *                              AND CANOPY MASS.
C     * APR 15/99 - M.LAZARE.   CORRECT SCREEN-LEVEL CALCULATION FOR WINDS
C     *                         TO HOLD AT ANEMOMETER LEVEL (10M) INSTEAD
C     *                         OF SCREEN LEVEL (2M).
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     *                         ALSO, APPLY UPPER BOUND ON "RATFC1"). 
C     * OCT 11/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         REVISE CALCULATION OF SLTHKEF AND 
C     *                         DEFINITION OF ZREF FOR INTERNAL 
C     *                         CONSISTENCY.
C     * SEP 27/96 - D.VERSEGHY. FIX BUG IN CALCULATION OF FLUXES
C     *                         BETWEEN SOIL LAYERS (PRESENT SINCE 
C     *                         RELEASE OF CLASS VERSION 2.5).
C     * MAY 21/96 - K.ABDELLA.  CORRECT EXPRESSION FOR ZOSCLH (4 PLACES).
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS; ALSO, PASS IN ZREF AND
C     *                         ILW THROUGH SUBROUTINE CALL.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL 
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 16/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         ADD THREE NEW DIAGNOSTIC FIELDS;
C     *                         REVISE CALCULATION OF HTCS, HTC.
C     * DEC 06/94 - M.LAZARE. - PASS "CFLUX" TO TSOLVE INSTEAD OF
C     *                         "CLIMIT" IN CONJUNCTION WITH CHANGES
C     *                         TO THAT ROUTINE.
C     *                       - REVISE CALCULATION OF "ZREF" TO INCLUDE
C     *                         VIRTUAL TEMPERATURE EFFECTS.
C     *                       - REVISE CALCULATION OF "SLTHKEF".
C     * NOV 28/94 - M.LAZARE.   FORM DRAG "CDOM" MODIFICATION REMOVED.
C     * NOV 18/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         LOCAL VERSION WITH INTERNAL WORK ARRAYS
C     *                         HARD-CODED FOR USE ON PCS.
C     * NOV 05/93 - M.LAZARE.   ADD NEW DIAGNOSTIC OUTPUT FIELD: DRAG.
C     * JUL 27/93 - D.VERSEGHY/M.LAZARE. PREVIOUS VERSION CLASSTO.
C
      use MODELS, only : Nmod, ebalm, lam
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER NLANDCS,NLANDGS,NLANDC,NLANDG,NLANDI,ISNOW,N,q
C
      INTEGER ITC,ITCG,ITG,ILG,IL1,IL2,JL,IC,IG,IZREF,ISLFD,I,J,ICP1
C
C     * OUTPUT FIELDS.
C                                                                              
      REAL TBARC (ILG,IG),TBARG (ILG,IG),TBARCS(ILG,IG),TBARGS(ILG,IG),
     1     THLIQC(ILG,IG),THLIQG(ILG,IG),THICEC(ILG,IG),THICEG(ILG,IG),
     2     HCPC  (ILG,IG),HCPG  (ILG,IG),TCTOPC(ILG,IG),TCBOTC(ILG,IG),
     3     TCTOPG(ILG,IG),TCBOTG(ILG,IG),HTC(ILG,IG),
     +     TSFSAV(ILG,4,Nmod)             
C   
      REAL GZEROC(ILG),   GZEROG(ILG),   GZROCS(ILG),   GZROGS(ILG),              
     1     G12C  (ILG),   G12G  (ILG),   G12CS (ILG),   G12GS (ILG),               
     2     G23C  (ILG),   G23G  (ILG),   G23CS (ILG),   G23GS (ILG),               
     3     QFREZC(ILG),   QFREZG(ILG),   QMELTC(ILG),   QMELTG(ILG),              
     4     EVAPC (ILG),   EVAPCG(ILG),   EVAPG (ILG),   EVAPCS(ILG),
     5     EVPCSG(ILG),   EVAPGS(ILG),   TCANO (ILG),   TCANS (ILG), 
     6     RAICAN(ILG),   SNOCAN(ILG,Nmod),   RAICNS(ILG),
     +     SNOCNS(ILG,Nmod),   CHCAP (ILG),   CHCAPS(ILG),
     7     TPONDC(ILG),   TPONDG(ILG),   TPNDCS(ILG),   TPNDGS(ILG),  
     8     TSNOCS(ILG),   TSNOGS(ILG),
     9     WSNOCS(ILG),   WSNOGS(ILG),   RHOSCS(ILG),   RHOSGS(ILG)   
C
      REAL CDH   (ILG),   CDM   (ILG),   QSENS (ILG),   TFLUX (ILG),   
     1     QEVAP (ILG),   EVAP  (ILG),   QFLUX (ILG),   
     2     EVPPOT(ILG),   ACOND (ILG),   EVAPB (ILG),   WTRG  (ILG),
     3     QLWAVG(ILG),   GT    (ILG),   QG    (ILG), 
     4     WTABLE(ILG),   ST    (ILG),   SU    (ILG),   SV    (ILG),  
     5     SQ    (ILG),   SRH   (ILG),
     6     FSGV  (ILG),   FSGS  (ILG),   FSGG  (ILG),   FLGV  (ILG),
     7     FLGS  (ILG),   FLGG  (ILG),   HFSC  (ILG),   HFSS  (ILG),
     8     HFSG  (ILG),   HEVC  (ILG),   HEVS  (ILG),   HEVG  (ILG),  
     9     HMFC  (ILG), HMFN  (ILG), HTCC  (ILG), HTCS  (ILG),
     A     DRAG  (ILG),   ILMO  (ILG),   UE    (ILG),   HBL   (ILG),   
     B     TAC   (ILG,Nmod), QAC   (ILG,Nmod), QFCF  (ILG), QFCL  (ILG),
     C     FTEMP (ILG),   FVAP  (ILG),   RIB   (ILG)
C
      INTEGER  ITERCT(ILG,6,50)
C
C     * INPUT FIELDS.
C
      REAL ZREFM (ILG),   ZREFH (ILG),   ZDIAGM(ILG),   ZDIAGH(ILG),
     1     VPD   (ILG),   TADP  (ILG),   RHOAIR(ILG),   QSWINV(ILG),   
     2     QSWINI(ILG),   QLWIN (ILG),   UWIND (ILG),   VWIND (ILG),   
     3     TA    (ILG),   QA    (ILG),   PADRY (ILG),   FC(ILG,Nmod),   
     4     FG (ILG,Nmod),   FCS(ILG,Nmod),   FGS(ILG,Nmod),RBCOEF(ILG), 
     5     FSVF  (ILG),   FSVFS (ILG),   PRESSG(ILG),   VMOD  (ILG),
     6     ALVSCN(ILG),   ALIRCN(ILG),   ALVSG (ILG),   ALIRG (ILG),              
     7     ALVSCS(ILG),   ALIRCS(ILG),   ALVSSN(ILG),   ALIRSN(ILG),  
     8     ALVSGC(ILG),   ALIRGC(ILG),   ALVSSC(ILG),   ALIRSC(ILG),
     9     TRVSCN(ILG),   TRIRCN(ILG),   TRVSCS(ILG),   TRIRCS(ILG),  
     A     RC    (ILG),   RCS   (ILG),   FRAINC(ILG),   FSNOWC(ILG),  
     B     FRAICS(ILG),   FSNOCS(ILG),   CMASSC(ILG),   CMASCS(ILG),   
     C     DISP  (ILG),   DISPS (ILG),   
     D     ZOMLNC(ILG),   ZOELNC(ILG),   ZOMLNG(ILG),   ZOELNG(ILG),   
     E     ZOMLCS(ILG),   ZOELCS(ILG),   ZOMLNS(ILG),   ZOELNS(ILG),   
     F     TPOND (ILG,Nmod),ZPOND (ILG,Nmod),TBASE (ILG,Nmod),   
     G     TCAN(ILG,Nmod),TSNOW(ILG,Nmod),ZSNOW(ILG,Nmod),TRSNOW(ILG),
     H     RHOSNO(ILG,Nmod),WSNOW (ILG,Nmod),   RADJ  (ILG),  PCPR(ILG),
     +     FCANMX(ILG,ICP1),GROWTH(ILG,Nmod)
C     
      REAL TBAR  (ILG,IG,Nmod),THLIQ (ILG,IG,Nmod),THICE (ILG,IG,Nmod)
C
C     * SOIL PROPERTY ARRAYS.
C
      REAL THPOR (ILG,IG),THLRET(ILG,IG),THLMIN(ILG,IG),
     1     THFC  (ILG,IG),HCPS  (ILG,IG),TCS   (ILG,IG),
     1     DELZ  (IG),    DELZW (ILG,IG),ZBOTW (ILG,IG)
C
      INTEGER  ISAND (ILG,IG)
C
C     * CTEM-RELATED I/O FIELDS.
C
      REAL AILCG(ILG,ICTEM),    AILCGS(ILG,ICTEM),    FCANC(ILG,ICTEM),
     1    FCANCS(ILG,ICTEM),         CO2CONC(ILG),  CO2I1CG(ILG,ICTEM),
     2   CO2I1CS(ILG,ICTEM),   CO2I2CG(ILG,ICTEM),  CO2I2CS(ILG,ICTEM),
     3           COSZS(ILG),         XDIFFUS(ILG),     SLAI(ILG,ICTEM),
     4                     RMATCTEM(ILG,ICTEM,IG),  FCANCMX(ILG,ICTEM),
     5   ANCSVEG(ILG,ICTEM),   ANCGVEG(ILG,ICTEM), RMLCSVEG(ILG,ICTEM),
     6  RMLCGVEG(ILG,ICTEM),         CFLUXCG(ILG),        CFLUXCS(ILG)
C
      REAL FIELDSM(ILG,IG), WILTSM(ILG,IG)
C
      INTEGER ICTEM, ICTEMMOD, L2MAX, NOL2PFTS(IC)
C
C     * INTERNAL WORK ARRAYS FOR THIS ROUTINE.
C
      REAL VA    (ILG),   ZRSLDM(ILG),   ZRSLDH(ILG),   ZRSLFM(ILG),   
     1     ZRSLFH(ILG),   ZDSLM (ILG),   ZDSLH (ILG),   TPOTA (ILG),   
     2     TVIRTA(ILG),   CRIB  (ILG),   CPHCHC(ILG),   CPHCHG(ILG),   
     3     HCPSCS(ILG),   HCPSGS(ILG),   TCSNOW(ILG),   CEVAP (ILG),   
     4     TBAR1P(ILG),   GSNOWC(ILG),   GSNOWG(ILG),   
     5     GDENOM(ILG),   GCOEFF(ILG),   GCONST(ILG),   
     6     TSNBOT(ILG),   GCOEFFS(ILG),  GCONSTS(ILG),
     7     A1    (ILG),   A2    (ILG),   B1    (ILG),   B2    (ILG),   
     8     C2    (ILG),   ZOM   (ILG),   ZOH   (ILG),   ZOSCLM(ILG),   
     9     ZOSCLH(ILG),   VAC   (ILG),                  FCOR  (ILG),
     A     CFLUX (ILG),   CDHX  (ILG),   CDMX  (ILG),   
     B     QSWX  (ILG),   QSWNC (ILG),   QSWNG (ILG),   QLWX  (ILG),
     C     QLWOC (ILG),   QLWOG (ILG),   QTRANS(ILG),    
     D     QSENSX(ILG),   QSENSC(ILG),   QSENSG(ILG),   QEVAPX(ILG),   
     E     QEVAPC(ILG),   QEVAPG(ILG),   QPHCHC(ILG),   QCANX (ILG),
     F     TSURX (ILG),   QSURX (ILG),                  
     G     TACCS (ILG),   QACCS (ILG),   TACCO (ILG),   QACCO (ILG),
     H     ILMOX (ILG),   UEX   (ILG),   HBLX  (ILG),   ZERO  (ILG),
     I     STT   (ILG),   SQT   (ILG),   SHT   (ILG)
      DOUBLE PRECISION    PSC   (ILG),   PSG   (ILG),   FSC   (ILG),   
     +     FSG   (ILG),   HBG   (ILG),   HBC   (ILG)
C
      INTEGER             IEVAP (ILG),   IWATER(ILG)
C
C     * INTERNAL WORK ARRAYS FOR TPREP.
C
      REAL FVEG  (ILG),    TCSATU(ILG),    TCSATF(ILG)
C
C     * INTERNAL WORK ARRAYS FOR TSOLVC/TSOLVE.
C   
      REAL TSTEP (ILG),    TVIRTC(ILG),    TVIRTG(ILG),    TVIRTS(ILG),    
     1     EVBETA(ILG),    XEVAP (ILG),    EVPWET(ILG),    Q0SAT (ILG),
     2     RA    (ILG),    RB    (ILG),    RAGINV(ILG),    RBINV (ILG),    
     3     RBTINV(ILG),    RBCINV(ILG),    
     4     TVRTAC(ILG),    TPOTG (ILG),    RESID (ILG),    
     5     TCANP (ILG),    TRTOP (ILG),    QSTOR (ILG),    
     6     AC    (ILG),    BC    (ILG), 
     7     LZZ0  (ILG),    LZZ0T (ILG),    FM    (ILG),    FH    (ILG),
     8     DCFLXM(ILG),    CFLUXM(ILG),    WZERO (ILG),    XEVAPM(ILG),
     9     WC    (ILG),    DRAGIN(ILG),    CFSENS(ILG),    CFEVAP(ILG),
     A     QSGADD(ILG),    CFLX  (ILG),
     B     FTEMPX(ILG),    FVAPX (ILG),    RIBX  (ILG)
C
      INTEGER              ITER  (ILG),    NITER (ILG),    JEVAP (ILG),
     1                     KF    (ILG),    KF1   (ILG),    KF2   (ILG),
     2                     IEVAPC(ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL THTOT,ZRUF,ZSCRN,ZANNOM,RATFC,RATFC1,RATFCA,RATFCA1,RATIO,
     1     CA,CB,WACSAT,QACSAT
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,TCW,TCICE,
     1     TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,HCPW,HCPICE,HCPSOL,
     2     HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     3     TCGLAC,CLHMLT,CLHVAP,DELTA,CGRAV,CKARM,CPD,AS,ASX,CI,BS,
     4     BETA,FACTN,HMIN,ANGMAX
C
      COMMON /CLASS1/ DELT,TFREZ                                       
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /PHYCON/ DELTA,CGRAV,CKARM,CPD
      COMMON /CLASSD2/ AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
C
C----------------------------------------------------------------------
C
C     * CALCULATION OF ATMOSPHERIC INPUT FIELDS REQUIRED BY CLASS FROM 
C     * VARIABLES SUPPLIED BY GCM.
C
      DO 50 I=IL1,IL2                                                            
          VA(I)=MAX(VMIN,VMOD(I)) 
          FCOR(I)=2.0*7.29E-5*SIN(RADJ(I))
C
C     * CHECK DEPTH OF PONDED WATER FOR UNPHYSICAL VALUES.
C
          IF(ZPOND(I,q).LT.1.0E-8) ZPOND(I,q)=0.0
          QG(I)=0.0
          
          !LOCAL ADVECTION INITIALIZATION
          PSC(I)=0.
          PSG(I)=0.
          FSC(I)=0.
          FSG(I)=0.
          HBC(I)=0.
          HBG(I)=0.
   50 CONTINUE
C
C     * CHECK LIQUID AND FROZEN SOIL MOISTURE CONTENTS FOR SMALL
C     * ABERRATIONS CAUSED BY PACKING/UNPACKING.
C
      DO 60 J=1,IG
      DO 60 I=IL1,IL2
          IF(ISAND(I,1).GT.-4)                                   THEN
              HTC(I,J)=HTC(I,J)-TBAR(I,J,q)*(HCPW*THLIQ(I,J,q)+
     1                 HCPICE*THICE(I,J,q))*DELZW(I,J)/DELT
              WTRG(I)=WTRG(I)-(RHOW*THLIQ(I,J,q)+RHOICE*THICE(I,J,q))*
     1                DELZW(I,J)/DELT
              IF(THLIQ(I,J,q).LT.THLMIN(I,J)) 
     1            THLIQ(I,J,q)=THLMIN(I,J)
              IF(THICE(I,J,q).LT.0.0) THICE(I,J,q)=0.0                        
              THTOT=THLIQ(I,J,q)+THICE(I,J,q)*RHOICE/RHOW                     
              IF(THTOT.GT.THPOR(I,J))           THEN                              
                  THLIQ(I,J,q)=MAX(THLIQ(I,J,q)*THPOR(I,J)/               
     1                       THTOT,THLMIN(I,J))                                       
                  THICE(I,J,q)=(THPOR(I,J)-THLIQ(I,J,q))*
     1                           RHOW/RHOICE
                  IF(THICE(I,J,q).LT.0.0) THICE(I,J,q)=0.0                        
              ENDIF
              HTC(I,J)=HTC(I,J)+TBAR(I,J,q)*(HCPW*THLIQ(I,J,q)+
     1                 HCPICE*THICE(I,J,q))*DELZW(I,J)/DELT
              WTRG(I)=WTRG(I)+(RHOW*THLIQ(I,J,q)+RHOICE*THICE(I,J,q))*
     1                DELZW(I,J)/DELT
          ENDIF
   60 CONTINUE                                                        
C
      IF (ICTEMMOD.EQ.1) THEN
C
C       * INITIALIZE VARIABLES ESTIMATED BY THE PHOTOSYNTHESIS SUBROUTINE
C       * CALLED FROM WITHIN TSOLVC.
C
        DO 65 J=1,ICTEM
        DO 65 I=IL1,IL2
          ANCSVEG(I,J)=0.0
          ANCGVEG(I,J)=0.0
          RMLCSVEG(I,J)=0.0
          RMLCGVEG(I,J)=0.0
   65   CONTINUE
      ENDIF
C
C     * PREPARATION.
C
      CALL  TPREP     (THLIQC, THLIQG, THICEC, THICEG, TBARC,  TBARG,
     1                 TBARCS, TBARGS, HCPC,   HCPG,   TCTOPC, TCBOTC,
     2                 TCTOPG, TCBOTG, HCPSCS, HCPSGS, TCSNOW, TSNOCS, 
     3                 TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, TCANO,  
     4                 TCANS,  CEVAP,  IEVAP,  TBAR1P, WTABLE, ZERO,
     5                 EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS,
     6                 GSNOWC, GSNOWG, GZEROC, GZEROG, GZROCS, GZROGS,
     7                 QMELTC, QMELTG, EVAP,
     8                 TPONDC, TPONDG, TPNDCS, TPNDGS, QSENSC, QSENSG, 
     9                 QEVAPC, QEVAPG, TACCO,  QACCO,  TACCS,  QACCS,  
     A                 ILMOX,  UEX,    HBLX,
     B                 ILMO,   UE,     HBL,   
     C                 ST,     SU,     SV,     SQ,     CDH,    CDM,
     D                 QSENS,  QEVAP,  QLWAVG, 
     E                 FSGV,   FSGS,   FSGG,   FLGV,   FLGS,   FLGG,   
     F                 HFSC,   HFSS,   HFSG,   HEVC,   HEVS,   HEVG,   
     G                 HMFC,   HMFN,   QFCF,   QFCL,   EVPPOT, ACOND,  
     H                 DRAG,   THLIQ,  THICE,  TBAR,   ZPOND,  TPOND,  
     I                 THPOR,  THLMIN, THLRET, THFC,   HCPS,   TCS,    
     J                 TA,     RHOSNO, TSNOW,  ZSNOW,  WSNOW,  TCAN,
     K                 FC,     FCS,    DELZ,   DELZW,  ZBOTW,
     L                 ISAND,  ILG,    IL1,    IL2,    JL,     IG,  
     M                 FVEG,   TCSATU, TCSATF, FTEMP,  FTEMPX, FVAP,
     N                 FVAPX,  RIB,    RIBX  ,q)           
C
C     * DEFINE NUMBER OF PIXELS OF EACH LAND SURFACE SUBAREA 
C     * (CANOPY-COVERED, CANOPY-AND-SNOW-COVERED, BARE SOIL, AND 
C     * SNOW OVER BARE SOIL) AND NUMBER OF LAND ICE PIXELS FOR 
C     * CALCULATIONS IN CLASST/CLASSW.

      NLANDC =0
      NLANDCS=0
      NLANDG =0
      NLANDGS=0
      NLANDI =0

      DO 70 I=IL1,IL2
          IF(FC (I,q).GT.0.)            NLANDC =NLANDC +1
          IF(FCS(I,q).GT.0.)            NLANDCS=NLANDCS+1
          IF(FG (I,q).GT.0.)            NLANDG =NLANDG +1
          IF(FGS(I,q).GT.0.)            NLANDGS=NLANDGS+1
          IF(ISAND(I,1).EQ.-4)        NLANDI =NLANDI +1
   70     CONTINUE
          
C
C     * CALCULATIONS FOR CANOPY OVER SNOW.
C                                                                                  
      IF(NLANDCS.GT.0)                                              THEN
          DO 100 I=IL1,IL2                                    
              IF(FCS(I,q).GT.0.)                                   THEN
                  ZOM(I)=EXP(ZOMLCS(I))
                  ZOH(I)=EXP(ZOELCS(I))
                  IF(IZREF.EQ.1) THEN
                      ZRSLDM(I)=ZREFM(I)-DISPS(I)
                      ZRSLDH(I)=ZREFH(I)-DISPS(I)
                      ZRSLFM(I)=ZREFM(I)-ZOM(I)-DISPS(I)
                      ZRSLFH(I)=ZREFH(I)-ZOM(I)-DISPS(I)
                      ZDSLM(I)=ZDIAGM(I)-ZOM(I)
                      ZDSLH(I)=ZDIAGH(I)-ZOM(I)
                      TPOTA(I)=TA(I)+ZRSLFH(I)*GRAV/CPD
                  ELSE
                      ZRSLDM(I)=ZREFM(I)+ZOM(I)
                      ZRSLDH(I)=ZREFH(I)+ZOM(I)
                      ZRSLFM(I)=ZREFM(I)-DISPS(I)
                      ZRSLFH(I)=ZREFH(I)-DISPS(I)
                      ZDSLM(I)=ZDIAGM(I)
                      ZDSLH(I)=ZDIAGH(I)
                      TPOTA(I)=TA(I)
                  ENDIF    
                  ZOSCLM(I)=ZOM(I)/ZRSLDM(I)
                  ZOSCLH(I)=ZOH(I)/ZRSLDH(I)
                  TVIRTA(I)=TPOTA(I)*(1.0+0.61*QA(I))
                  CRIB(I)=-GRAV*ZRSLDM(I)/(TVIRTA(I)*
     1                    VA(I)**2)
                  DRAG(I)=DRAG(I)+FCS(I,q)*(VKC/(LOG(ZRSLDM(I))-
     1                    ZOMLCS(I)))**2
                  VAC(I)=VA(I)*(LOG(10.0*ZOM(I)-DISPS(I))-ZOMLCS(I))/
     1                   (LOG(ZRSLDM(I))-ZOMLCS(I))
                  TACCS(I)=TAC(I,q)
                  QACCS(I)=QAC(I,q)
              ENDIF
  100     CONTINUE
C                                     
          CALL CWCALC(TCANS,RAICNS,SNOCNS,FRAICS,FSNOCS,CHCAPS,
     1                HMFC,HTCC,FCS,CMASCS,ILG,IL1,IL2,JL,q)
          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,IWATER, 
     2                TBAR,TCTOPC,TCBOTC,
     +                FCS,ZPOND,TBAR1P,DELZ,TCSNOW,ZSNOW,
     3                ISAND,ILG,IL1,IL2,JL,IG,q         )
          CALL TSPREP(GCOEFFS,GCONSTS,CPHCHG,IWATER,
     1                FCS,ZSNOW,TSNOW,TCSNOW,
     2                ILG,IL1,IL2,JL,q)
          ISNOW=1
        select case(ebalm(q))
         case(0) ! CLASS: iterative
          CALL TSOLVC(ISNOW,FCS,
     1                QSWX,QSWNC,QSWNG,QLWX,QLWOC,QLWOG,QTRANS,
     2                QSENSX,QSENSC,QSENSG,QEVAPX,QEVAPC,QEVAPG,EVAPCS,
     3                EVPCSG,EVAP,TCANS,QCANX,TSURX,QSURX,GSNOWC,QPHCHC,
     4                QMELTC,RAICNS,SNOCNS,CDHX,CDMX,RIBX,TACCS,QACCS,
     5                CFLUX,FTEMPX,FVAPX,ILMOX,UEX,HBLX,QFCF,QFCL,HTCC,
     6                QSWINV,QSWINI,QLWIN,TPOTA,TA,QA,VA,VAC,PADRY,
     7                RHOAIR,ALVSCS,ALIRCS,ALVSSC,ALIRSC,TRVSCS,TRIRCS,
     8                FSVFS,CRIB,CPHCHC,CPHCHG,CEVAP,TADP,TVIRTA,RCS,
     9                RBCOEF,ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,
     A                FCOR,GCONSTS,GCOEFFS,TSFSAV(1,1,q),TRSNOW,FSNOCS,
     B                FRAICS,CHCAPS,CMASCS,PCPR,IWATER,IEVAP,ITERCT,
     C                ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,N,  
     D                TSTEP,TVIRTC,TVIRTG,EVBETA,XEVAP,EVPWET,Q0SAT,
     E                RA,RB,RAGINV,RBINV,RBTINV,RBCINV,TVRTAC,TPOTG,
     F                RESID,TCANP,
     G                WZERO,XEVAPM,DCFLXM,WC,DRAGIN,CFLUXM,CFLX,IEVAPC,
     H                TRTOP,QSTOR,CFSENS,CFEVAP,QSGADD,AC,BC,
     I                LZZ0,LZZ0T,FM,FH,ITER,NITER,KF1,KF2,
     J                AILCGS,FCANCS,CO2CONC,RMATCTEM,
     K                THLIQC,FIELDSM,WILTSM,ISAND,IG,COSZS,PRESSG,
     L                XDIFFUS,ICTEM,IC,CO2I1CS,CO2I2CS,
     M                ICTEMMOD,SLAI,FCANCMX,L2MAX,
     N                NOL2PFTS,CFLUXCS,ANCSVEG,RMLCSVEG,FCANMX,ICP1,q,
     O                GROWTH)
          CALL TSPOST(GSNOWC,TSNOCS,WSNOCS,RHOSCS,QMELTC,
     1                GZROCS,TSNBOT,HTCS,HMFN,
     2                GCONSTS,GCOEFFS,GCONST,GCOEFF,TBAR,
     3                TSURX,ZSNOW,TCSNOW,HCPSCS,QTRANS,
     4                FCS,DELZ,ILG,IL1,IL2,JL,IG,q   )
          CALL TNPOST(TBARCS,G12CS,G23CS,TPNDCS,GZROCS,ZERO,GCONST,
     1                GCOEFF,TBAR,TCTOPC,TCBOTC,HCPC,ZPOND,TSNBOT,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FCS,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG,q)
         case(1) ! JULES: linear
          CALL TSOLVClinear(ISNOW,FCS,
     1                QSWX,QSWNC,QSWNG,QLWX,QLWOC,QLWOG,QTRANS,
     2                QSENSX,QSENSC,QSENSG,QEVAPX,QEVAPC,QEVAPG,EVAPCS,
     3                EVPCSG,EVAP,TCANS,QCANX,TSURX,QSURX,GSNOWC,QPHCHC,
     4                QMELTC,RAICNS,SNOCNS,CDHX,CDMX,RIBX,TACCS,QACCS,
     5                CFLUX,FTEMPX,FVAPX,ILMOX,UEX,HBLX,QFCF,QFCL,HTCC,
     6                QSWINV,QSWINI,QLWIN,TPOTA,TA,QA,VA,VAC,PADRY,
     7                RHOAIR,ALVSCS,ALIRCS,ALVSSC,ALIRSC,TRVSCS,TRIRCS,
     8                FSVFS,CRIB,CPHCHC,CPHCHG,CEVAP,TADP,TVIRTA,RCS,
     9                RBCOEF,ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,
     A                FCOR,GCONSTS,GCOEFFS,TSFSAV(1,1,q),TRSNOW,FSNOCS,
     B                FRAICS,CHCAPS,CMASCS,PCPR,IWATER,IEVAP,ITERCT,
     C                ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,N,  
     D                TSTEP,TVIRTC,TVIRTG,EVBETA,XEVAP,EVPWET,Q0SAT,
     E                RA,RB,RAGINV,RBINV,RBTINV,RBCINV,TVRTAC,TPOTG,
     F                RESID,TCANP,
     G                WZERO,XEVAPM,DCFLXM,WC,DRAGIN,CFLUXM,CFLX,IEVAPC,
     H                TRTOP,QSTOR,CFSENS,CFEVAP,QSGADD,AC,BC,
     I                LZZ0,LZZ0T,FM,FH,ITER,NITER,KF1,KF2,
     J                AILCGS,FCANCS,CO2CONC,RMATCTEM,
     K                THLIQC,FIELDSM,WILTSM,ISAND,IG,COSZS,PRESSG,
     L                XDIFFUS,ICTEM,IC,CO2I1CS,CO2I2CS,
     M                ICTEMMOD,SLAI,FCANCMX,L2MAX,
     N                NOL2PFTS,CFLUXCS,ANCSVEG,RMLCSVEG,FCANMX,ICP1,
     O                ZSNOW,TSNOW,TCSNOW,ZPOND,TCTOPC,DELZ,TBAR1P,q,
     P                GROWTH)
          CALL TSPOSTlinear(GSNOWC,TSNOCS,WSNOCS,RHOSCS,QMELTC,
     1                GZROCS,TSNBOT,HTCS,HMFN,
     2                GCONSTS,GCOEFFS,GCONST,GCOEFF,TBAR,
     3                TSURX,ZSNOW,TCSNOW,HCPSCS,QTRANS,
     4                FCS,DELZ,ILG,IL1,IL2,JL,IG,q,TCTOPC   )
          CALL TNPOSTlinear(TBARCS,G12CS,G23CS,TPNDCS,GZROCS,ZERO,GCONST
     1                ,GCOEFF,TBAR,TCTOPC,TCBOTC,HCPC,ZPOND,TSNBOT,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FCS,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG,q)
        end select
C
C     * DIAGNOSTICS.
C
          IF(ISLFD.EQ.0)                                         THEN
            DO 150 I=IL1,IL2
              IF(FCS(I,q).GT.0.)                THEN
                  ZRUF=ZOH(I)
                  ZSCRN=MAX(ZRUF,2.0)                         
                  ZANNOM=MAX(ZRUF,10.0)                         
                  RATFC=LOG(ZSCRN/ZRUF)/VKC                              
                  RATFC1=RATFC*SQRT(CDMX(I))                          
                  RATFC1=MIN(RATFC1,1.)
                  RATFCA=LOG(ZANNOM/ZRUF)/VKC                              
                  RATFCA1=RATFCA*SQRT(CDMX(I))                          
                  RATFCA1=MIN(RATFCA1,1.)
                  IF(RIBX(I).GE.0.)  THEN                               
                      RATIO=RATFC1                                             
                  ELSE                                                    
                      RATIO=RATFC1*CDHX(I)/CDMX(I)                   
                      RATIO=MIN(RATIO,(ZSCRN/ZRSLDM(I))**(1./3.))             
                  ENDIF                                                    
                  STT(I)=TCANS(I)-(MIN(RATIO,1.))*(TCANS(I)-TA(I))       
                  ST(I)=ST(I)+FCS(I,q)*STT(I)
                  SU(I)=SU(I)+FCS(I,q)*RATFCA1*UWIND(I)                                     
                  SV(I)=SV(I)+FCS(I,q)*RATFCA1*VWIND(I)                                     
                  SQT(I)=QA(I)+(QCANX(I)-QA(I))*MIN(RATIO,1.)      
                  SQ(I)=SQ(I)+FCS(I,q)*SQT(I)
              ENDIF
  150       CONTINUE
C
            SHT=0.
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FCS,ILG,IL1,IL2,q)
            DO 160 I=IL1,IL2
                SRH(I)=SRH(I)+FCS(I,q)*SHT(I)
  160       CONTINUE    
          ELSEIF(ISLFD.EQ.1)                                        THEN
              CALL SLDIAG(SU,SV,ST,SQ,
     1                    CDMX,CDHX,UWIND,VWIND,TPOTA,QA,
     2                    TACCS,QACCS,ZOM,ZOH,FCS,ZRSLDM,
     3                    ZDSLM,ZDSLH,ILG,IL1,IL2,JL,q)
          ELSEIF(ISLFD.EQ.2)                                        THEN
              CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TACCS,QACCS,
     1                    ZOM,ZOH,ILMOX,ZRSLFM,HBLX,UEX,FTEMPX,FVAPX,
     2                    ZDSLM,ZDSLH,RADJ,FCS,IL1,IL2,JL,q)
!              SHT=0.
!              CALL SCREENRH(SHT,ST,SQ,PRESSG,FCS,ILG,IL1,IL2,q)
!              DO 960 I=IL1,IL2
!                SRH(I)=SRH(I)+FCS(I,q)*SHT(I)
!  960         CONTINUE
          ENDIF
C
          DO 175 I=IL1,IL2
              IF(FCS(I,q).GT.0.)                                   THEN
                  IF(TACCS(I).GE.TFREZ)                      THEN
                      CA=17.269       
                      CB=35.86       
                  ELSE                
                      CA=21.874    
                      CB=7.66     
                  ENDIF                       
                  WACSAT=0.622*611.0*EXP(CA*(TACCS(I)-TFREZ)/
     1                   (TACCS(I)-CB))/PADRY(I)           
                  QACSAT=WACSAT/(1.0+WACSAT)    
                  EVPPOT(I)=EVPPOT(I)+FCS(I,q)*RHOAIR(I)*CFLUX(I)*
     1                     (QACSAT-QA(I))
                  ACOND(I)=ACOND(I)+FCS(I,q)*CFLUX(I)
                  ILMO(I) =ILMO(I)+FCS(I,q)*ILMOX(I)
                  UE(I)   =UE(I)+FCS(I,q)*UEX(I)
                  HBL(I)  =HBL(I)+FCS(I,q)*HBLX(I)
                  CDH (I) =CDH(I)+FCS(I,q)*CDHX(I)
                  CDM (I) =CDM(I)+FCS(I,q)*CDMX(I)
                  TSFSAV(I,1,q)=TSURX(I)
                  QG(I)=QG(I)+FCS(I,q)*QACCS(I)
                  QSENS(I)=QSENS(I)+FCS(I,q)*QSENSX(I)
                  QEVAP(I)=QEVAP(I)+FCS(I,q)*QEVAPX(I)
                  QLWAVG(I)=QLWAVG(I)+FCS(I,q)*QLWX(I)
                  FSGV(I) =FSGV(I)+FCS(I,q)*QSWNC(I)
                  FSGS(I) =FSGS(I)+FCS(I,q)*QSWNG(I)
                  FSGG(I) =FSGG(I)+FCS(I,q)*QTRANS(I)
                  FLGV(I) =FLGV(I)+FCS(I,q)*(QLWIN(I)+QLWOG(I)-2.0*
     1                     QLWOC(I))*(1.0-FSVFS(I))
                  FLGS(I) =FLGS(I)+FCS(I,q)*(QLWOC(I)*(1.0-FSVFS(I))+
     1                     QLWIN(I)*FSVFS(I)-QLWOG(I))
                  IF(ITC.EQ.1) THEN
                      HFSC(I) =HFSC(I)+FCS(I,q)*QSENSC(I)
                  ELSE
                      HFSC(I) =HFSC(I)+FCS(I,q)*(QSENSC(I)-QSENSG(I))
                  ENDIF
                  HFSS(I) =HFSS(I)+FCS(I,q)*QSENSG(I)
                  HEVC(I) =HEVC(I)+FCS(I,q)*QEVAPC(I)
                  HEVS(I) =HEVS(I)+FCS(I,q)*QEVAPG(I)
                  HMFC(I) =HMFC(I)+FCS(I,q)*QPHCHC(I)
                  HTCS(I) =HTCS(I)+FCS(I,q)*(-GZROCS(I)+
     1                     QTRANS(I))
                  HTC(I,1)=HTC(I,1)+FCS(I,q)*(GZROCS(I)-QTRANS(I)-
     1                     G12CS(I))
                  HTC(I,2)=HTC(I,2)+FCS(I,q)*(G12CS(I)-G23CS(I))
                  HTC(I,3)=HTC(I,3)+FCS(I,q)*G23CS(I)
                  FTEMP(I)= FTEMP(I) + FCS(I,q) * FTEMPX(I)
                  FVAP (I)= FVAP (I) + FCS(I,q) * FVAPX (I)
                  RIB  (I)= RIB  (I) + FCS(I,q) * RIBX  (I)
              ENDIF
  175     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR SNOW-COVERED GROUND.
C
      IF(NLANDGS.GT.0)                                              THEN
          DO 200 I=IL1,IL2                                    
              IF(FGS(I,q).GT.0.)                                   THEN
                  ZOM(I)=EXP(ZOMLNS(I))
                  ZOH(I)=EXP(ZOELNS(I))
                  IF(IZREF.EQ.1) THEN
                      ZRSLDM(I)=ZREFM(I)
                      ZRSLDH(I)=ZREFH(I)
                      ZRSLFM(I)=ZREFM(I)-ZOM(I)
                      ZRSLFH(I)=ZREFH(I)-ZOM(I)
                      ZDSLM(I)=ZDIAGM(I)-ZOM(I)
                      ZDSLH(I)=ZDIAGH(I)-ZOM(I)
                      TPOTA(I)=TA(I)+ZRSLFH(I)*GRAV/CPD
                  ELSE
                      ZRSLDM(I)=ZREFM(I)+ZOM(I)
                      ZRSLDH(I)=ZREFH(I)+ZOM(I)
                      ZRSLFM(I)=ZREFM(I)
                      ZRSLFH(I)=ZREFH(I)
                      ZDSLM(I)=ZDIAGM(I)
                      ZDSLH(I)=ZDIAGH(I)
                      TPOTA(I)=TA(I)
                  ENDIF    
                  ZOSCLM(I)=ZOM(I)/ZRSLDM(I)
                  ZOSCLH(I)=ZOH(I)/ZRSLDH(I)
                  TVIRTA(I)=TPOTA(I)*(1.0+0.61*QA(I))
                  CRIB(I)=-GRAV*ZRSLDM(I)/(TVIRTA(I)*VA(I)**2)
                  DRAG(I)=DRAG(I)+FGS(I,q)*(VKC/(LOG(ZRSLDM(I))-
     1                    ZOMLNS(I)))**2
              ENDIF
  200     CONTINUE
C
          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,IWATER, 
     2                TBAR,TCTOPG,TCBOTG,
     +                FGS,ZPOND,TBAR1P,DELZ,TCSNOW,ZSNOW,
     3                ISAND,ILG,IL1,IL2,JL,IG,q   )
          CALL TSPREP(GCOEFFS,GCONSTS,CPHCHG,IWATER,
     1                FGS,ZSNOW,TSNOW,TCSNOW,
     2                ILG,IL1,IL2,JL,q      )
          ISNOW=1 
        select case(ebalm(q))
         case(0) ! CLASS: iterative
          CALL TSOLVE(ISNOW,FGS,
     1                QSWX,QLWX,QTRANS,QSENSX,QEVAPX,EVAPGS,
     2                TSURX,QSURX,GSNOWG,QMELTG,CDHX,CDMX,RIBX,CFLUX,
     3                FTEMPX,FVAPX,ILMOX,UEX,HBLX, 
     4                QSWINV,QSWINI,QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,
     5                ALVSSN,ALIRSN,CRIB,CPHCHG,CEVAP,TVIRTA,
     6                ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,
     7                GCONSTS,GCOEFFS,TSFSAV(1,2,q),TRSNOW,PCPR,
     8                IWATER,IEVAP,ITERCT,ISAND,
     9                ISLFD,ITG,ILG,IG,IL1,IL2,JL,  
     A                TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,
     B                DCFLXM,CFLUXM,WZERO,TRTOP,AC,BC,
     C                LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF,q,N)
          CALL TSPOST(GSNOWG,TSNOGS,WSNOGS,RHOSGS,QMELTG,
     1                GZROGS,TSNBOT,HTCS,HMFN,
     2                GCONSTS,GCOEFFS,GCONST,GCOEFF,TBAR,
     3                TSURX,ZSNOW,TCSNOW,HCPSGS,QTRANS,
     4                FGS,DELZ,ILG,IL1,IL2,JL,IG,q     )
          CALL TNPOST(TBARGS,G12GS,G23GS,TPNDGS,GZROGS,ZERO,GCONST,
     1                GCOEFF,TBAR,TCTOPG,TCBOTG,HCPG,ZPOND,TSNBOT,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FGS,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG,q  )
         case(1) ! JULES: linear
          CALL TSOLVElinear(ISNOW,FGS,
     1                QSWX,QLWX,QTRANS,QSENSX,QEVAPX,EVAPGS,
     2                TSURX,QSURX,GSNOWG,QMELTG,CDHX,CDMX,RIBX,CFLUX,
     3                FTEMPX,FVAPX,ILMOX,UEX,HBLX, 
     4                QSWINV,QSWINI,QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,
     5                ALVSSN,ALIRSN,CRIB,CPHCHG,CEVAP,TVIRTA,
     6                ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,
     7                GCONSTS,GCOEFFS,TSFSAV(1,2,q),TRSNOW,PCPR,
     8                IWATER,IEVAP,ITERCT,ISAND,
     9                ISLFD,ITG,ILG,IG,IL1,IL2,JL,  
     A                TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,
     B                DCFLXM,CFLUXM,WZERO,TRTOP,AC,BC,
     C                LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF,
     D                ZSNOW,TSNOW,TCSNOW,ZPOND,TCTOPG,DELZ,TBAR1P,q,N)
          CALL TSPOSTlinear(GSNOWG,TSNOGS,WSNOGS,RHOSGS,QMELTG,
     1                GZROGS,TSNBOT,HTCS,HMFN,
     2                GCONSTS,GCOEFFS,GCONST,GCOEFF,TBAR,
     3                TSURX,ZSNOW,TCSNOW,HCPSGS,QTRANS,
     4                FGS,DELZ,ILG,IL1,IL2,JL,IG,q,TCTOPG     )
          CALL TNPOSTlinear(TBARGS,G12GS,G23GS,TPNDGS,GZROGS,ZERO,GCONST
     1                ,GCOEFF,TBAR,TCTOPG,TCBOTG,HCPG,ZPOND,TSNBOT,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FGS,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG,q  )
        end select
C
C     * DIAGNOSTICS.
C
          IF(ISLFD.EQ.0)                                         THEN
            DO 250 I=IL1,IL2
              IF(FGS(I,q).GT.0.)                THEN
                  ZRUF=ZOH(I)
                  ZSCRN=MAX(ZRUF,2.0)                         
                  ZANNOM=MAX(ZRUF,10.0)                         
                  RATFC=LOG(ZSCRN/ZRUF)/VKC                              
                  RATFC1=RATFC*SQRT(CDMX(I))                                
                  RATFC1=MIN(RATFC1,1.)
                  RATFCA=LOG(ZANNOM/ZRUF)/VKC                              
                  RATFCA1=RATFCA*SQRT(CDMX(I))                           
                  RATFCA1=MIN(RATFCA1,1.)
                  IF(RIBX(I).GE.0.)  THEN                              
                      RATIO=RATFC1                                             
                  ELSE                                                    
                      RATIO=RATFC1*CDHX(I)/CDMX(I)                       
                      RATIO=MIN(RATIO,(ZSCRN/ZRSLDM(I))**(1./3.))          
                  ENDIF                                                      
                  STT(I)=TSURX(I)-(MIN(RATIO,1.))*(TSURX(I)-TA(I))     
                  ST(I)=ST(I)+FGS(I,q)*STT(I)
                  SU(I)=SU(I)+FGS(I,q)*RATFCA1*UWIND(I)                                     
                  SV(I)=SV(I)+FGS(I,q)*RATFCA1*VWIND(I)                                     
                  SQT(I)=QA(I)+(QSURX(I)-QA(I))*MIN(RATIO,1.)      
                  SQ(I)=SQ(I)+FGS(I,q)*SQT(I)
              ENDIF
  250       CONTINUE
C
            SHT=0.
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FGS,ILG,IL1,IL2,q)
            DO 260 I=IL1,IL2
                SRH(I)=SRH(I)+FGS(I,q)*SHT(I)
  260       CONTINUE    
          ELSEIF(ISLFD.EQ.1)                                        THEN
              CALL SLDIAG(SU,SV,ST,SQ,
     1                    CDMX,CDHX,UWIND,VWIND,TPOTA,QA,
     2                    TSURX,QSURX,ZOM,ZOH,FGS,ZRSLDM,
     3                    ZDSLM,ZDSLH,ILG,IL1,IL2,JL,q)
          ELSEIF(ISLFD.EQ.2)                                        THEN  
              CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TSURX,QSURX,
     1                    ZOM,ZOH,ILMOX,ZRSLFM,HBLX,UEX,FTEMPX,FVAPX,
     2                    ZDSLM,ZDSLH,RADJ,FGS,IL1,IL2,JL,q)
!              SHT=0.
!              CALL SCREENRH(SHT,ST,SQ,PRESSG,FGS,ILG,IL1,IL2,q)
!              DO 961 I=IL1,IL2
!                SRH(I)=SRH(I)+FGS(I,q)*SHT(I)
!  961         CONTINUE
          ENDIF
C
          DO 275 I=IL1,IL2
              IF(FGS(I,q).GT.0.)                                    THEN
                  EVPPOT(I)=EVPPOT(I)+FGS(I,q)*RHOAIR(I)*CFLUX(I)*
     1                     (Q0SAT(I)-QA(I))
                  ACOND(I)=ACOND(I)+FGS(I,q)*CFLUX(I)
                  ILMO(I) =ILMO(I)+FGS(I,q)*ILMOX(I)
                  UE(I)   =UE(I)+FGS(I,q)*UEX(I)
                  HBL(I)  =HBL(I)+FGS(I,q)*HBLX(I)
                  CDH (I) =CDH(I)+FGS(I,q)*CDHX(I)
                  CDM (I) =CDM(I)+FGS(I,q)*CDMX(I)
                  TSFSAV(I,2,q)=TSURX(I)
                  QG(I)=QG(I)+FGS(I,q)*QSURX(I)
                  QSENS(I)=QSENS(I)+FGS(I,q)*QSENSX(I)
                  QEVAP(I)=QEVAP(I)+FGS(I,q)*QEVAPX(I)
                  QLWAVG(I)=QLWAVG(I)+FGS(I,q)*QLWX(I)
                  FSGS(I) =FSGS(I)+FGS(I,q)*(QSWX(I)-QTRANS(I))
                  FSGG(I) =FSGG(I)+FGS(I,q)*QTRANS(I)
                  FLGS(I) =FLGS(I)+FGS(I,q)*(QLWIN(I)-QLWX(I))
                  HFSS(I) =HFSS(I)+FGS(I,q)*QSENSX(I)
                  HEVS(I) =HEVS(I)+FGS(I,q)*QEVAPX(I)
                  HTCS(I) =HTCS(I)+FGS(I,q)*(-GZROGS(I)+
     1                     QTRANS(I))
                  HTC(I,1)=HTC(I,1)+FGS(I,q)*(GZROGS(I)-QTRANS(I)-
     1                     G12GS(I))
                  HTC(I,2)=HTC(I,2)+FGS(I,q)*(G12GS(I)-G23GS(I))
                  HTC(I,3)=HTC(I,3)+FGS(I,q)*G23GS(I)
                  FTEMP(I)= FTEMP(I) + FGS(I,q) * FTEMPX(I)
                  FVAP (I)= FVAP (I) + FGS(I,q) * FVAPX (I)
                  RIB  (I)= RIB  (I) + FGS(I,q) * RIBX  (I)
              ENDIF
  275     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR CANOPY OVER BARE GROUND.
C                                                                                  
      IF(NLANDC.GT.0)                                               THEN
          DO 300 I=IL1,IL2                                    
              IF(FC(I,q).GT.0.)                                    THEN
                  ZOM(I)=EXP(ZOMLNC(I))
                  ZOH(I)=EXP(ZOELNC(I))
                  IF(IZREF.EQ.1) THEN
                      ZRSLDM(I)=ZREFM(I)-DISP(I)
                      ZRSLDH(I)=ZREFH(I)-DISP(I)
                      ZRSLFM(I)=ZREFM(I)-ZOM(I)-DISP(I)
                      ZRSLFH(I)=ZREFH(I)-ZOM(I)-DISP(I)
                      ZDSLM(I)=ZDIAGM(I)-ZOM(I)
                      ZDSLH(I)=ZDIAGH(I)-ZOM(I)
                      TPOTA(I)=TA(I)+ZRSLFH(I)*GRAV/CPD
                  ELSE
                      ZRSLDM(I)=ZREFM(I)+ZOM(I)
                      ZRSLDH(I)=ZREFH(I)+ZOM(I)
                      ZRSLFM(I)=ZREFM(I)-DISP(I)
                      ZRSLFH(I)=ZREFH(I)-DISP(I)
                      ZDSLM(I)=ZDIAGM(I)
                      ZDSLH(I)=ZDIAGH(I)
                      TPOTA(I)=TA(I)
                  ENDIF    
                  ZOSCLM(I)=ZOM(I)/ZRSLDM(I)
                  ZOSCLH(I)=ZOH(I)/ZRSLDH(I)
                  TVIRTA(I)=TPOTA(I)*(1.0+0.61*QA(I))
                  CRIB(I)=-GRAV*ZRSLDM(I)/(TVIRTA(I)*VA(I)**2)
                  DRAG(I)=DRAG(I)+FC(I,q)*(VKC/(LOG(ZRSLDM(I))-
     1                    ZOMLNC(I)))**2
                  VAC(I)=VA(I)*(LOG(10.0*ZOM(I)-DISP(I))-ZOMLNC(I))/
     1                (LOG(ZRSLDM(I))-ZOMLNC(I))
                  TACCO(I)=TAC(I,q)
                  QACCO(I)=QAC(I,q)
              ENDIF
  300     CONTINUE
C
          CALL CWCALC(TCANO,RAICAN,SNOCAN,FRAINC,FSNOWC,CHCAP,
     1                HMFC,HTCC,FC,CMASSC,ILG,IL1,IL2,JL,q)
          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,IWATER, 
     2                TBAR,TCTOPC,TCBOTC,
     +                FC,ZPOND,TBAR1P,DELZ,TCSNOW,ZSNOW,
     3                ISAND,ILG,IL1,IL2,JL,IG,q         )
          ISNOW=0
        select case(ebalm(q))
         case(0) ! CLASS: iterative
          CALL TSOLVC(ISNOW,FC,
     1                QSWX,QSWNC,QSWNG,QLWX,QLWOC,QLWOG,QTRANS,
     2                QSENSX,QSENSC,QSENSG,QEVAPX,QEVAPC,QEVAPG,EVAPC,
     3                EVAPCG,EVAP,TCANO,QCANX,TSURX,QSURX,GZEROC,QPHCHC,
     4                QFREZC,RAICAN,SNOCAN,CDHX,CDMX,RIBX,TACCO,QACCO,
     5                CFLUX,FTEMPX,FVAPX,ILMOX,UEX,HBLX,QFCF,QFCL,HTCC, 
     6                QSWINV,QSWINI,QLWIN,TPOTA,TA,QA,VA,VAC,PADRY,
     7                RHOAIR,ALVSCN,ALIRCN,ALVSGC,ALIRGC,TRVSCN,TRIRCN,
     8                FSVF,CRIB,CPHCHC,CPHCHG,CEVAP,TADP,TVIRTA,RC,
     9                RBCOEF,ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,
     A                FCOR,GCONST,GCOEFF,TSFSAV(1,3,q),TRSNOW,FSNOWC,
     B                FRAINC,CHCAP,CMASSC,PCPR,IWATER,IEVAP,ITERCT,
     C                ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,N,  
     D                TSTEP,TVIRTC,TVIRTG,EVBETA,XEVAP,EVPWET,Q0SAT,
     E                RA,RB,RAGINV,RBINV,RBTINV,RBCINV,TVRTAC,TPOTG,
     F                RESID,TCANP,
     G                WZERO,XEVAPM,DCFLXM,WC,DRAGIN,CFLUXM,CFLX,IEVAPC,
     H                TRTOP,QSTOR,CFSENS,CFEVAP,QSGADD,AC,BC,
     I                LZZ0,LZZ0T,FM,FH,ITER,NITER,KF1,KF2,
     J                AILCG,FCANC,CO2CONC,RMATCTEM,
     K                THLIQC,FIELDSM,WILTSM,ISAND,IG,COSZS,PRESSG,
     L                XDIFFUS,ICTEM,IC,CO2I1CG,CO2I2CG,
     M                ICTEMMOD,SLAI,FCANCMX,L2MAX,
     N                NOL2PFTS,CFLUXCG,ANCGVEG,RMLCGVEG,FCANMX,ICP1,q,
     O                GROWTH)
          CALL TNPOST(TBARC,G12C,G23C,TPONDC,GZEROC,QFREZC,GCONST,
     1                GCOEFF,TBAR,TCTOPC,TCBOTC,HCPC,ZPOND,TSURX,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FC,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG,q)
         case(1) ! JULES: linear
          CALL TSOLVClinear(ISNOW,FC,
     1                QSWX,QSWNC,QSWNG,QLWX,QLWOC,QLWOG,QTRANS,
     2                QSENSX,QSENSC,QSENSG,QEVAPX,QEVAPC,QEVAPG,EVAPC,
     3                EVAPCG,EVAP,TCANO,QCANX,TSURX,QSURX,GZEROC,QPHCHC,
     4                QFREZC,RAICAN,SNOCAN,CDHX,CDMX,RIBX,TACCO,QACCO,
     5                CFLUX,FTEMPX,FVAPX,ILMOX,UEX,HBLX,QFCF,QFCL,HTCC, 
     6                QSWINV,QSWINI,QLWIN,TPOTA,TA,QA,VA,VAC,PADRY,
     7                RHOAIR,ALVSCN,ALIRCN,ALVSGC,ALIRGC,TRVSCN,TRIRCN,
     8                FSVF,CRIB,CPHCHC,CPHCHG,CEVAP,TADP,TVIRTA,RC,
     9                RBCOEF,ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,
     A                FCOR,GCONST,GCOEFF,TSFSAV(1,3,q),TRSNOW,FSNOWC,
     B                FRAINC,CHCAP,CMASSC,PCPR,IWATER,IEVAP,ITERCT,
     C                ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,N,  
     D                TSTEP,TVIRTC,TVIRTG,EVBETA,XEVAP,EVPWET,Q0SAT,
     E                RA,RB,RAGINV,RBINV,RBTINV,RBCINV,TVRTAC,TPOTG,
     F                RESID,TCANP,
     G                WZERO,XEVAPM,DCFLXM,WC,DRAGIN,CFLUXM,CFLX,IEVAPC,
     H                TRTOP,QSTOR,CFSENS,CFEVAP,QSGADD,AC,BC,
     I                LZZ0,LZZ0T,FM,FH,ITER,NITER,KF1,KF2,
     J                AILCG,FCANC,CO2CONC,RMATCTEM,
     K                THLIQC,FIELDSM,WILTSM,ISAND,IG,COSZS,PRESSG,
     L                XDIFFUS,ICTEM,IC,CO2I1CG,CO2I2CG,
     M                ICTEMMOD,SLAI,FCANCMX,L2MAX,
     N                NOL2PFTS,CFLUXCG,ANCGVEG,RMLCGVEG,FCANMX,ICP1,
     O                ZSNOW,TSNOW,TCSNOW,ZPOND,TCTOPC,DELZ,TBAR1P,q,
     P                GROWTH)
          CALL TNPOSTlinear(TBARC,G12C,G23C,TPONDC,GZEROC,QFREZC,GCONST,
     1                GCOEFF,TBAR,TCTOPC,TCBOTC,HCPC,ZPOND,TSURX,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FC,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG,q)
        end select
C
C     * DIAGNOSTICS.
C
          IF(ISLFD.EQ.0)                                         THEN
            DO 350 I=IL1,IL2
              IF(FC(I,q).GT.0.)                 THEN
                  ZRUF=ZOH(I)
                  ZSCRN=MAX(ZRUF,2.0)                         
                  ZANNOM=MAX(ZRUF,10.0)                         
                  RATFC=LOG(ZSCRN/ZRUF)/VKC                              
                  RATFC1=RATFC*SQRT(CDMX(I))                               
                  RATFC1=MIN(RATFC1,1.)
                  RATFCA=LOG(ZANNOM/ZRUF)/VKC                              
                  RATFCA1=RATFCA*SQRT(CDMX(I))                           
                  RATFCA1=MIN(RATFCA1,1.)
                  IF(RIBX(I).GE.0.)  THEN                             
                      RATIO=RATFC1                                             
                  ELSE                                                     
                      RATIO=RATFC1*CDHX(I)/CDMX(I)                       
                      RATIO=MIN(RATIO,(ZSCRN/ZRSLDM(I))**(1./3.))           
                  ENDIF                                                     
                  STT(I)=TCANO(I)-(MIN(RATIO,1.))*(TCANO(I)-TA(I))        
                  ST(I)=ST(I)+FC(I,q)*STT(I)
                  SU(I)=SU(I)+FC(I,q)*RATFCA1*UWIND(I)                                     
                  SV(I)=SV(I)+FC(I,q)*RATFCA1*VWIND(I)                                     
                  SQT(I)=QA(I)+(QCANX(I)-QA(I))*MIN(RATIO,1.)      
                  SQ(I)=SQ(I)+FC(I,q)*SQT(I)
              ENDIF
  350       CONTINUE
C
            SHT=0.
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FC,ILG,IL1,IL2,q)
            DO 360 I=IL1,IL2
                SRH(I)=SRH(I)+FC(I,q)*SHT(I)
  360       CONTINUE    
          ELSEIF(ISLFD.EQ.1)                                        THEN
              CALL SLDIAG(SU,SV,ST,SQ,
     1                    CDMX,CDHX,UWIND,VWIND,TPOTA,QA,
     2                    TACCO,QACCO,ZOM,ZOH,FC,ZRSLDM,
     3                    ZDSLM,ZDSLH,ILG,IL1,IL2,JL,q)
          ELSEIF(ISLFD.EQ.2)                                        THEN    
              CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TACCO,QACCO,
     1                    ZOM,ZOH,ILMOX,ZRSLFM,HBLX,UEX,FTEMPX,FVAPX,
     2                    ZDSLM,ZDSLH,RADJ,FC,IL1,IL2,JL,q)
!              SHT=0.
!              CALL SCREENRH(SHT,ST,SQ,PRESSG,FC,ILG,IL1,IL2,q)
!              DO 962 I=IL1,IL2
!                SRH(I)=SRH(I)+FC(I,q)*SHT(I)
!  962         CONTINUE
          ENDIF
C
          DO 375 I=IL1,IL2
              IF(FC(I,q).GT.0.)                                     THEN
                  IF(TACCO(I).GE.TFREZ)                      THEN
                      CA=17.269       
                      CB=35.86       
                  ELSE                
                      CA=21.874    
                      CB=7.66     
                  ENDIF                       
                  WACSAT=0.622*611.0*EXP(CA*(TACCO(I)-TFREZ)/
     1                   (TACCO(I)-CB))/PADRY(I)           
                  QACSAT=WACSAT/(1.0+WACSAT)    
                  EVPPOT(I)=EVPPOT(I)+FC(I,q)*RHOAIR(I)*CFLUX(I)*
     1                     (QACSAT-QA(I))
                  ACOND(I)=ACOND(I)+FC(I,q)*CFLUX(I)
                  ILMO(I) =ILMO(I)+FC(I,q)*ILMOX(I)
                  UE(I)   =UE(I)+FC(I,q)*UEX(I)
                  HBL(I)  =HBL(I)+FC(I,q)*HBLX(I)
                  CDH (I) =CDH(I)+FC(I,q)*CDHX(I)
                  CDM (I) =CDM(I)+FC(I,q)*CDMX(I)
                  TSFSAV(I,3,q)=TSURX(I)
                  QG(I)=QG(I)+FC(I,q)*QACCO(I)
                  QSENS(I)=QSENS(I)+FC(I,q)*QSENSX(I)
                  QEVAP(I)=QEVAP(I)+FC(I,q)*QEVAPX(I)
                  QLWAVG(I)=QLWAVG(I)+FC(I,q)*QLWX(I)
                  FSGV(I) =FSGV(I)+FC(I,q)*QSWNC(I)
                  FSGG(I) =FSGG(I)+FC(I,q)*QSWNG(I)
                  FLGV(I) =FLGV(I)+FC(I,q)*(QLWIN(I)+QLWOG(I)-2.0*
     1                     QLWOC(I))*(1.0-FSVF(I))
                  FLGG(I) =FLGG(I)+FC(I,q)*(FSVF(I)*QLWIN(I)+
     1                     (1.0-FSVF(I))*QLWOC(I)-QLWOG(I))
                  IF(ITC.EQ.1) THEN
                      HFSC(I) =HFSC(I)+FC(I,q)*QSENSC(I)
                  ELSE
                      HFSC(I) =HFSC(I)+FC(I,q)*(QSENSC(I)-QSENSG(I))
                  ENDIF
                  HFSG(I) =HFSG(I)+FC(I,q)*QSENSG(I)
                  HEVC(I) =HEVC(I)+FC(I,q)*QEVAPC(I)
                  HEVG(I) =HEVG(I)+FC(I,q)*QEVAPG(I)
                  HMFC(I) =HMFC(I)+FC(I,q)*QPHCHC(I)
                  HTC(I,1)=HTC(I,1)+FC(I,q)*(-G12C(I))
                  HTC(I,2)=HTC(I,2)+FC(I,q)*(G12C(I)-G23C(I))
                  HTC(I,3)=HTC(I,3)+FC(I,q)*G23C(I)
                  FTEMP(I)= FTEMP(I) + FC(I,q) * FTEMPX(I)
                  FVAP (I)= FVAP (I) + FC(I,q) * FVAPX (I)
                  RIB  (I)= RIB  (I) + FC(I,q) * RIBX   (I)
                  HBC  (I)=1*QSENSG(I)
              ENDIF
  375     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR BARE GROUND.
C                                                                                  
      IF(NLANDG.GT.0)                                               THEN
          DO 400 I=IL1,IL2                                    
              IF(FG(I,q).GT.0.)                                   THEN
                  ZOM(I)=EXP(ZOMLNG(I))
                  ZOH(I)=EXP(ZOELNG(I))
                  IF(IZREF.EQ.1) THEN
                      ZRSLDM(I)=ZREFM(I)
                      ZRSLDH(I)=ZREFH(I)
                      ZRSLFM(I)=ZREFM(I)-ZOM(I)
                      ZRSLFH(I)=ZREFH(I)-ZOM(I)
                      ZDSLM(I)=ZDIAGM(I)-ZOM(I)
                      ZDSLH(I)=ZDIAGH(I)-ZOM(I)
                      TPOTA(I)=TA(I)+ZRSLFH(I)*GRAV/CPD
                  ELSE
                      ZRSLDM(I)=ZREFM(I)+ZOM(I)
                      ZRSLDH(I)=ZREFH(I)+ZOM(I)
                      ZRSLFM(I)=ZREFM(I)
                      ZRSLFH(I)=ZREFH(I)
                      ZDSLM(I)=ZDIAGM(I)
                      ZDSLH(I)=ZDIAGH(I)
                      TPOTA(I)=TA(I)
                  ENDIF    
                  ZOSCLM(I)=ZOM(I)/ZRSLDM(I)
                  ZOSCLH(I)=ZOH(I)/ZRSLDH(I)
                  TVIRTA(I)=TPOTA(I)*(1.0+0.61*QA(I))
                  CRIB(I)=-GRAV*ZRSLDM(I)/(TVIRTA(I)*VA(I)**2)
                  DRAG(I)=DRAG(I)+FG(I,q)*(VKC/(LOG(ZRSLDM(I))-
     1                    ZOMLNG(I)))**2
              ENDIF
  400     CONTINUE
C
          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,IWATER, 
     2                TBAR,TCTOPG,TCBOTG,
     +                FG,ZPOND,TBAR1P,DELZ,TCSNOW,ZSNOW,
     3                ISAND,ILG,IL1,IL2,JL,IG,q        )
          ISNOW=0 
        select case(ebalm(q))
         case(0) ! CLASS: iterative
          CALL TSOLVE(ISNOW,FG, ! MM: output from FLXSURFZ: CDHX,CDMX,RIBX,CFLUX,FTEMPX,FVAPX,ILMOX,UEX,HBLX,LZZ0,LZZ0T,FM,FH
     1                QSWX,QLWX,QTRANS,QSENSX,QEVAPX,EVAPG,
     2                TSURX,QSURX,GZEROG,QFREZG,CDHX,CDMX,RIBX,CFLUX,
     3                FTEMPX,FVAPX,ILMOX,UEX,HBLX, 
     4                QSWINV,QSWINI,QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,
     5                ALVSG,ALIRG,CRIB,CPHCHG,CEVAP,TVIRTA,
     6                ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,
     7                GCONST,GCOEFF,TSFSAV(1,4,q),TRSNOW,PCPR,
     8                IWATER,IEVAP,ITERCT,ISAND,
     9                ISLFD,ITG,ILG,IG,IL1,IL2,JL,  
     A                TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,
     B                DCFLXM,CFLUXM,WZERO,TRTOP,AC,BC,
     C                LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF,q,N)
          CALL TNPOST(TBARG,G12G,G23G,TPONDG,GZEROG,QFREZG,GCONST,
     1                GCOEFF,TBAR,TCTOPG,TCBOTG,HCPG,ZPOND,TSURX,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FG,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG ,q )
         case(1) ! JULES: linear
          CALL TSOLVElinear(ISNOW,FG, ! MM: output from FLXSURFZ: CDHX,CDMX,RIBX,CFLUX,FTEMPX,FVAPX,ILMOX,UEX,HBLX,LZZ0,LZZ0T,FM,FH
     1                QSWX,QLWX,QTRANS,QSENSX,QEVAPX,EVAPG,
     2                TSURX,QSURX,GZEROG,QFREZG,CDHX,CDMX,RIBX,CFLUX,
     3                FTEMPX,FVAPX,ILMOX,UEX,HBLX, 
     4                QSWINV,QSWINI,QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,
     5                ALVSG,ALIRG,CRIB,CPHCHG,CEVAP,TVIRTA,
     6                ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,
     7                GCONST,GCOEFF,TSFSAV(1,4,q),TRSNOW,PCPR,
     8                IWATER,IEVAP,ITERCT,ISAND,
     9                ISLFD,ITG,ILG,IG,IL1,IL2,JL,  
     A                TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,
     B                DCFLXM,CFLUXM,WZERO,TRTOP,AC,BC,
     C                LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF,
     D                ZSNOW,TSNOW,TCSNOW,ZPOND,TCTOPG,DELZ,TBAR1P,q,N)
          CALL TNPOSTlinear(TBARG,G12G,G23G,TPONDG,GZEROG,QFREZG,GCONST,
     1                GCOEFF,TBAR,TCTOPG,TCBOTG,HCPG,ZPOND,TSURX,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FG,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG ,q )
        end select
C
C     * DIAGNOSTICS.
C
          IF(ISLFD.EQ.0)                                         THEN
            DO 450 I=IL1,IL2
              IF(FG(I,q).GT.0.)                 THEN
                  ZRUF=ZOH(I)
                  ZSCRN=MAX(ZRUF,2.0)                         
                  ZANNOM=MAX(ZRUF,10.0)                         
                  RATFC=LOG(ZSCRN/ZRUF)/VKC                              
                  RATFC1=RATFC*SQRT(CDMX(I))                              
                  RATFC1=MIN(RATFC1,1.)
                  RATFCA=LOG(ZANNOM/ZRUF)/VKC                              
                  RATFCA1=RATFCA*SQRT(CDMX(I))                               
                  RATFCA1=MIN(RATFCA1,1.)
                  IF(RIBX(I).GE.0.)  THEN                                
                      RATIO=RATFC1                                             
                  ELSE                                                    
                      RATIO=RATFC1*CDHX(I)/CDMX(I)                       
                      RATIO=MIN(RATIO,(ZSCRN/ZRSLDM(I))**(1./3.))             
                  ENDIF                                                        
                  STT(I)=TSURX(I)-(MIN(RATIO,1.))*(TSURX(I)-TA(I))    
                  ST(I)=ST(I)+FG(I,q)*STT(I)
                  SU(I)=SU(I)+FG(I,q)*RATFCA1*UWIND(I)                                     
                  SV(I)=SV(I)+FG(I,q)*RATFCA1*VWIND(I)                                     
                  SQT(I)=QA(I)+(QSURX(I)-QA(I))*MIN(RATIO,1.)      
                  SQ(I)=SQ(I)+FG(I,q)*SQT(I)
              ENDIF
  450       CONTINUE
C
            SHT=0.
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FG,ILG,IL1,IL2,q)
            DO 460 I=IL1,IL2
                SRH(I)=SRH(I)+FG(I,q)*SHT(I)
  460       CONTINUE    
          ELSEIF(ISLFD.EQ.1)                                        THEN
              CALL SLDIAG(SU,SV,ST,SQ,
     1                    CDMX,CDHX,UWIND,VWIND,TPOTA,QA,
     2                    TSURX,QSURX,ZOM,ZOH,FG,ZRSLDM,
     3                    ZDSLM,ZDSLH,ILG,IL1,IL2,JL,q)
          ELSEIF(ISLFD.EQ.2)                                        THEN  ! from FLXSURFZ: ILMOX,HBLX,UEX,FTEMPX,FVAPX
              CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TSURX,QSURX,
     1                    ZOM,ZOH,ILMOX,ZRSLFM,HBLX,UEX,FTEMPX,FVAPX,
     2                    ZDSLM,ZDSLH,RADJ,FG,IL1,IL2,JL,q)
!              SHT=0.
!              CALL SCREENRH(SHT,ST,SQ,PRESSG,FG,ILG,IL1,IL2,q)
!              DO 963 I=IL1,IL2
!                SRH(I)=SRH(I)+FG(I,q)*SHT(I)
!  963         CONTINUE
          ENDIF
C
          DO 475 I=IL1,IL2
              IF(FG(I,q).GT.0.)                                     THEN
                  EVPPOT(I)=EVPPOT(I)+FG(I,q)*RHOAIR(I)*CFLUX(I)*
     1                     (Q0SAT(I)-QA(I))
                  ACOND(I)=ACOND(I)+FG(I,q)*CFLUX(I)
                  ILMO(I) =ILMO(I)+FG(I,q)*ILMOX(I)
                  UE(I)   =UE(I)+FG(I,q)*UEX(I)
                  HBL(I)  =HBL(I)+FG(I,q)*HBLX(I)
                  CDH (I) =CDH(I)+FG(I,q)*CDHX(I)
                  CDM (I) =CDM(I)+FG(I,q)*CDMX(I)
                  TSFSAV(I,4,q)=TSURX(I)
                  QG(I)=QG(I)+FG(I,q)*QSURX(I)
                  QSENS(I)=QSENS(I)+FG(I,q)*QSENSX(I)
                  QEVAP(I)=QEVAP(I)+FG(I,q)*QEVAPX(I)
                  QLWAVG(I)=QLWAVG(I)+FG(I,q)*QLWX(I)
                  FSGG(I) =FSGG(I)+FG(I,q)*QSWX(I)
                  FLGG(I) =FLGG(I)+FG(I,q)*(QLWIN(I)-QLWX(I))
                  HFSG(I) =HFSG(I)+FG(I,q)*QSENSX(I)
                  HEVG(I) =HEVG(I)+FG(I,q)*QEVAPX(I)
                  HTC(I,1)=HTC(I,1)+FG(I,q)*(-G12G(I))
                  HTC(I,2)=HTC(I,2)+FG(I,q)*(G12G(I)-G23G(I))
                  HTC(I,3)=HTC(I,3)+FG(I,q)*G23G(I)
                  FTEMP(I)= FTEMP(I) + FG(I,q) * FTEMPX(I)
                  FVAP (I)= FVAP (I) + FG(I,q) * FVAPX (I)
                  RIB  (I)= RIB  (I) + FG(I,q) * RIBX  (I)
                  HBG  (I)=1*QSENSX(I)
              ENDIF
  475     CONTINUE
      ENDIF                                                               
C
C******LOCAL ADVECTION CALCULATIONS (Matt MacDonald)******
C
      select case(lam(q))
      case(1) !Cherkauer & Lettenmaier (2003)
       DO 600 I=IL1,IL2
           PSC(I)=0.
           PSG(I)=0.
           FSC(I)=0.
           FSG(I)=0.
          IF(FCS(I,q).GT.0)THEN
            PSC(I)=FCS(I,q)/(FC(I,q)+FCS(I,q))
          ELSE
            PSC(I)=0.
          ENDIF
          IF(FGS(I,q).GT.0)THEN
            PSG(I)=FGS(I,q)/(FG(I,q)+FGS(I,q))
          ELSE
            PSG(I)=0.
          ENDIF
          IF(PSC(I).GT.0.6)THEN
              FSC(I)=1.0
          ELSEIF(PSC(I).GT.0.5)THEN
              FSC(I)=10**(3*PSC(I)-1.8)
          ELSEIF(PSC(I).GT.0.2)THEN
              FSC(I)=10**(5.667*PSC(I)-3.133)
          ELSEIF(PSC(I).GT.0.013)THEN
              FSC(I)=0.01
          ENDIF
          IF(PSG(I).GT.0.6)THEN
              FSG(I)=1.0
          ELSEIF(PSG(I).GT.0.5)THEN
              FSG(I)=10**(3*PSG(I)-1.8)
          ELSEIF(PSG(I).GT.0.2)THEN
              FSG(I)=10**(5.667*PSG(I)-3.133)
          ELSEIF(PSG(I).GT.0.013)THEN
              FSG(I)=0.01
          ENDIF
          IF(HBC(I).GT.999.) FSC(I)=0.
          IF(HBG(I).GT.999.) FSG(I)=0.
  600 CONTINUE
      
      DO 601 I=IL1,IL2
          IF(FCS(I,q).GT.0.)THEN
              CALL LOCALADVECTION(TSNOCS,WSNOCS,RHOSCS,QMELTC,
     1                HTCS,HMFN,ZSNOW,TCSNOW,HCPSCS,PSC,FSC,HBC,
     2                HFSS,HEVS,FCS,ILG,IL1,IL2,JL,q   )
          ENDIF
          IF(FGS(I,q).GT.0.)THEN
              CALL LOCALADVECTION(TSNOGS,WSNOGS,RHOSGS,QMELTG,
     1                HTCS,HMFN,ZSNOW,TCSNOW,HCPSGS,PSG,FSG,HBG,
     2                HFSS,HEVS,FGS,ILG,IL1,IL2,JL,q   )
          ENDIF
  601 CONTINUE
      end select
C
C*********************************************************
C
C     * ADDITIONAL DIAGNOSTIC VARIABLES. 
C
      DO 500 I=IL1,IL2
          GT(I)=(QLWAVG(I)/SBC)**0.25                                            
          TFLUX(I)=-QSENS(I)/(RHOAIR(I)*SPHAIR)                                  
          EVAP(I)=EVAP(I)+RHOW*
     1           (FCS(I,q)*(EVAPCS(I)+EVPCSG(I)) + FGS(I,q)*EVAPGS(I) +              
     2            FC (I,q)*(EVAPC (I)+EVAPCG(I)) + FG (I,q)*EVAPG(I))
          QFLUX(I)=-EVAP(I)/RHOAIR(I)                                            
          IF(EVPPOT(I).NE.0.0) THEN
              EVAPB(I)=EVAP(I)/EVPPOT(I)
          ELSE
              EVAPB(I)=0.0
          ENDIF
          IF((FCS(I,q)+FC(I,q)).GT.1.0E-5) THEN
              TAC(I,q)=(FCS(I,q)*TACCS(I)+FC(I,q)*TACCO(I))/
     +                 (FCS(I,q)+FC(I,q))
              QAC(I,q)=(FCS(I,q)*QACCS(I)+FC(I,q)*QACCO(I))/
     +                 (FCS(I,q)+FC(I,q))
          ELSE
              TAC(I,q)=TA(I)
              QAC(I,q)=QA(I)
          ENDIF
  500 CONTINUE
C                                                                                  
      RETURN                                                                      
      END        
