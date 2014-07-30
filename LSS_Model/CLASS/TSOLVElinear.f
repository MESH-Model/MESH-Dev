      SUBROUTINE TSOLVElinear(ISNOW,FI,
     1                  QSWNET,QLWOUT,QTRANS,QSENS,QEVAP,EVAP,
     2                  TZERO,QZERO,GZERO,QMELT,CDH,CDM,RIB,CFLUX,
     3                  FTEMP,FVAP,ILMO,UE,H,
     4                  QSWINV,QSWINI,QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,
     5                  ALVISG,ALNIRG,CRIB,CPHCH,CEVAP,TVIRTA,
     6                  ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,
     7                  GCONST,GCOEFF,TSTART,TRSNOW,PCPR,
     8                  IWATER,IEVAP,ITERCT,ISAND,
     9                  ISLFD,ITG,ILG,IG,IL1,IL2,JL,
     A                  TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,
     B                  DCFLXM,CFLUXM,WZERO,TRTOP,A,B,
     C                  LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF,
     D                  ZSNOW,TSNOW,TCSNOW,ZPOND,TCTOP,DELZ,TBAR1P,q,N)
C
C     * JAN 16/14 - M.MACDONALD.ALGEBRAIC SOLUTION OF LINEARIZED ENERGY
C     *                         BALANCE EQUATION FOR ENSEMBLE MODEL.
C     * OCT 14/11 - D.VERSEGHY. FOR POST-ITERATION CLEANUP WITH N-R SCHEME,
C     *                         REMOVE CONDITION INVOLVING LAST ITERATION
C     *                         TEMPERATURE.
C     * DEC 07/09 - D.VERSEGHY. RESTORE EVAPORATION WHEN PRECIPITATION
C     *                         IS OCCURRING.
C     * MAR 13/09 - D.VERSEGHY. REPLACE SURFCON COMMON BLOCK WITH CLASSD2;
C     *                         REVISED CALL TO FLXSURFZ.
C     * JAN 06/09 - D.VERSEGHY/M.LAZARE. SPLIT IF CONDITIONS FRAMING
C     *                         300 LOOP.
C     * FEB 25/08 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS; REMOVE
C     *                         "ILW" SWITCH; SUPPRESS WATER VAPOUR FLUX
C     *                         IF PRECIPITATION IS OCCURRING.
C     * MAY 17/06 - D.VERSEGHY. SUPPRESS EVAPORATION WHEN PONDED WATER
C     *                         IS FREEZING; ADD IL1 AND IL2 TO CALL TO
C     *                         FLXSURFZ; REMOVE JL FROM CALL TO DRCOEF.
C     * APR 13/05 - R.BROWN. ADD WINDLESS TRANFER COEFFICIENT TO QSENS
C     *                         CALCULATION FOR SNOW PACKS.
C     * DEC 17/04 - Y.DELAGE/D.VERSEGHY. ADD SWITCH TO USE EITHER SECANT/
C     *                         BISECTION OR NEWTON-RAPHSON ITERATION
C     *                         SCHEME (WITH NUMBER OF ITERATIONS LIMITED
C     *                         TO FIVE AND CORRECTION FOR REMAINING
C     *                         RESIDUAL).
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 06/04 - Y.DELAGE/D.VERSEGHY. PROTECT SENSITIVE CALCULATIONS
C     *                         FROM ROUNDOFF ERRORS.
C     * NOV 07/02 - Y.DELAGE/D.VERSEGHY. NEW CALL TO FLXSURFZ.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * MAR 28/02 - D.VERSEGHY. STREAMLINED SUBROUTINE CALL.
C     *                         BYPASS EVAPORATION EFFICIENCY PARAMETER 
C     *                         IN CASES OF CONDENSATION.
C     * JAN 18/02 - P.BARTLETT/D.VERSEGHY. NEW "BETA" FORMULATION FOR 
C     *                         BARE SOIL EVAPORATION BASED ON LEE AND
C     *                         PIELKE.
C     * APR 11/01 - M.LAZARE.   SHORTENED "CLASS2" COMMON BLOCK.
C     * OCT 06/00 - D.VERSEGHY. CONDITIONAL "IF" IN ITERATION SEQUENCE
C     *                         TO AVOID DIVIDE BY ZERO.
C     * DEC 07/99 - A.WU/D.VERSEGHY. NEW SOIL EVAPORATION FORMULATION.
C     * JUL 24/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         REPLACE BISECTION METHOD IN SURFACE 
C     *                         TEMPERATURE ITERATION SCHEME WITH 
C     *                         SECANT METHOD FOR FIRST TEN ITERATIONS.
C     *                         PASS QZERO,QA,ZOMS,ZOHS TO REVISED
C     *                         DRCOEF (ZOMS AND ZOHS ALSO NEW WORK ARRAYS
C     *                         PASSED TO THIS ROUTINE).
C     * JUN 20/97 - D.VERSEGHY. PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.  ALSO, PASS SWITCH "ILW"
C     *                         THROUGH SUBROUTINE CALL, SPECIFYING 
C     *                         WHETHER QLWIN REPRESENTS INCOMING
C     *                         (ILW=1) OR NET (ILW=2) LONGWAVE
C     *                         RADIATION ABOVE THE GROUND.
C     * NOV 30/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         NEW DRAG COEFFICIENT AND RELATED FIELDS,
C     *                         NOW DETERMINED IN ROUTINE "DRCOEF"
C     *                         "CFLUX" NOW WORK FIELD INSTEAD OF "CLIMIT".
C     * OCT 04/94 - D.VERSEGHY. CHANGE "CALL ABORT" TO "CALL XIT" TO
C     *                         ENABLE RUNNING ON PCS.
C     * JAN 24/94 - M.LAZARE.   UNFORMATTED I/O COMMENTED OUT IN LOOP 200.
C     * JUL 29/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REMOVE RE-DEFINITION OF QMELT NEAR END
C     *                         (SINCE DONE ELSEWHERE ALREADY) AND
C     *                         REDEFINE QSWNET FOR DIAGNOSTIC PURPOSES
C     *                         TO INCLUDE TRANSMISSION THROUGH 
C     *                         SNOWPACK.
C     * OCT 15/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.                  
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. ITERATIVE SURFACE TEMPERATURE 
C     *                         CALCULATIONS FOR SNOW/SOIL.
C
      use MODELS, only : Nmod
      IMPLICIT NONE

C     * INTEGER CONSTANTS.
C
      INTEGER ISNOW,ISLFD,ITG,ILG,IG,IL1,IL2,JL,I,q,N
C
      INTEGER NUMIT,NIT,IBAD,ITERMX
C
C     * OUTPUT ARRAYS.
C
      REAL QSWNET(ILG),    QLWOUT(ILG),    QTRANS(ILG),    QSENS (ILG),    
     1     QEVAP (ILG),    EVAP  (ILG),    TZERO (ILG),    QZERO (ILG),    
     2     GZERO (ILG),    QMELT (ILG),    CDH   (ILG),    CDM   (ILG),    
     3     RIB   (ILG),    CFLUX (ILG),    FTEMP (ILG),    FVAP  (ILG),    
     4     ILMO  (ILG),    UE    (ILG),    H     (ILG)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG,Nmod),    QSWINV(ILG),    QSWINI(ILG),QLWIN (ILG),   
     1     TPOTA (ILG),    QA    (ILG),    VA    (ILG),    PADRY (ILG),    
     2     RHOAIR(ILG),    ALVISG(ILG),    ALNIRG(ILG),    CRIB  (ILG),    
     3     CPHCH (ILG),    CEVAP (ILG),    TVIRTA(ILG),    
     4     ZOSCLH(ILG),    ZOSCLM(ILG),    ZRSLFH(ILG),    ZRSLFM(ILG),
     5     ZOH   (ILG),    ZOM   (ILG),    GCONST(ILG),    GCOEFF(ILG),
     6     TSTART(ILG),    TRSNOW(ILG),    FCOR  (ILG),    PCPR  (ILG),
     +     ZSNOW(ILG,Nmod),TSNOW(ILG,Nmod),TCSNOW(ILG),ZPOND (ILG,Nmod),
     +     TCTOP (ILG,IG), DELZ  (IG),     TBAR1P(ILG)

C
      INTEGER          IWATER(ILG),        IEVAP (ILG)   
      INTEGER          ITERCT(ILG,6,50),   ISAND(ILG,IG)
C
C     * INTERNAL WORK ARRAYS.
C
      REAL TSTEP (ILG),    TVIRTS(ILG),    EVBETA(ILG),    Q0SAT (ILG),
     1     RESID (ILG),    DCFLXM(ILG),    CFLUXM(ILG),    TRTOP (ILG),    
     2     A     (ILG),    B     (ILG),
     3     LZZ0  (ILG),    LZZ0T (ILG),    FM    (ILG),    FH    (ILG),
     4     WZERO (ILG)
C
      INTEGER              ITER  (ILG),    NITER (ILG),    JEVAP (ILG),
     1                     KF    (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL QSWNV,QSWNI,DCFLUX,DRDT0,TZEROT,QEVAPT,BOWEN,EZERO,
     1     D,DTS,TCZERO
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,HCPW,HCPICE,
     1     HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,
     2     RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP,DELTA,CGRAV,CKARM,CPD,
     3     AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX,X1,X2,X3,X4,G,GAS,X5,
     1     X6,CPRES,GASV,X7
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /PHYCON/ DELTA,CGRAV,CKARM,CPD
      COMMON /CLASSD2/ AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
      COMMON /PARAMS/ X1,    X2,    X3,    X4,   G,GAS,   X5,
     1                X6,    CPRES, GASV,  X7
C-----------------------------------------------------------------------
C
C      IF(ISNOW.EQ.0) THEN
C          EZERO=0.0
C      ELSE
C          EZERO=2.0
C      ENDIF
       EZERO=0.0
C
      DO 50 I=IL1,IL2
          IF(FI(I,q).GT.0.)                                         THEN
              ITER(I)=1
              QMELT(I)=0.
              CFLUX(I)=0.
              TZERO(I)=TSTART(I)
              IF(TZERO(I).GE.TFREZ)                        THEN
                  A(I)=17.269       
                  B(I)=35.86       
              ELSE                
                  A(I)=21.874    
                  B(I)=7.66     
              ENDIF                       
              WZERO(I)=0.622*611.0*EXP(A(I)*(TZERO(I)-TFREZ)/
     1              (TZERO(I)-B(I)))/PADRY(I)           
              Q0SAT(I)=WZERO(I)/(1.0+WZERO(I))
              IF(IWATER(I).GT.0)                              THEN
                  EVBETA(I)=1.0
                  QZERO(I)=Q0SAT(I)
              ELSE
                  EVBETA(I)=CEVAP(I)
                  QZERO(I)=EVBETA(I)*Q0SAT(I)+(1.0-EVBETA(I))*QA(I)
                  IF(QZERO(I).GT.QA(I) .AND. IEVAP(I).EQ.0) THEN
                      EVBETA(I)=0.0
                      QZERO(I)=QA(I)
                  ENDIF
              ENDIF
              TVIRTS(I)=TZERO(I)*(1.0+0.61*QZERO(I))
C             * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT) AND
C             * OTHER RELATED QUANTITIES.
              IF(ISLFD.LT.2) THEN
                  CALL DRCOEF (CDM,CDH,RIB,CFLUX,QZERO,QA,ZOSCLM,ZOSCLH,
     1                   CRIB,TVIRTS,TVIRTA,VA,FI,ITER,
     2                   ILG,IL1,IL2,q)
              ELSE
                  CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TZERO,QZERO,H,ZOM,ZOH,
     3                LZZ0,LZZ0T,FM,FH,ILG,I,I,FI,ITER,JL,q,N,0)!IL1,IL2,FI,ITER,JL,q,N,0)
              ENDIF
              ! MM: into FLXSURFZ: FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,TZERO,QZERO,ZOM,ZOH,ILG,IL1,IL2,FI,ITER,JL
              ! MM: output from FLXSURFZ: CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,UE,H,LZZ0,LZZ0T,FM,FH (all back out to CLASST)
              ! MM: used subsequently in TSOLVE: CFLUX (CTU in FLXSURFZ)
              IF(ISNOW.EQ.0)                      THEN
                  TRTOP(I)=0.
              ELSE
                  TRTOP(I)=TRSNOW(I)
              ENDIF
              !D=0.622*CPHCH(I)*Q0SAT(I)/(GAS*TZERO(I)**2)
              D=0.622*CPHCH(I)*QZERO(I)/(GAS*TZERO(I)**2) !use QZERO rather than Q0SAT to account for EVBETA
              QSWNV=QSWINV(I)*(1.0-ALVISG(I))   
              QSWNI=QSWINI(I)*(1.0-ALNIRG(I))  
              QSWNET(I)=QSWNV+QSWNI           
              QTRANS(I)=QSWNET(I)*TRTOP(I)   
              QSWNET(I)=QSWNET(I)-QTRANS(I) 
              QLWOUT(I)=SBC*TZERO(I)**4
              IF(TZERO(I).LT.TPOTA(I))                        THEN
                  QSENS(I)=(RHOAIR(I)*SPHAIR*CFLUX(I)+EZERO)*(TZERO(I)-
     1                TPOTA(I))
              ELSE
                  QSENS(I)=RHOAIR(I)*SPHAIR*CFLUX(I)*(TZERO(I)-
     1                TPOTA(I))
              ENDIF
              EVAP(I)=RHOAIR(I)*CFLUX(I)*(QZERO(I)-QA(I)) 
              QEVAP(I)=CPHCH(I)*EVAP(I)      
              IF(ISNOW.EQ.0.) THEN
               IF(ZSNOW(I,q).LE.0.0) THEN
                TCZERO=TCTOP(I,1)
                GZERO(I)=2*TCZERO*(TZERO(I)-TBAR1P(I))/
     1                   (DELZ(1)+ZPOND(I,q))
                DTS=(QSWNET(I)+QLWIN(I)-QLWOUT(I)-QSENS(I)-QEVAP(I)-
     1              GZERO(I)) / ((SPHAIR+EZERO + CPHCH(I)*D)*RHOAIR(I)*
     2              CFLUX(I) + 2*TCZERO/(DELZ(1)+ZPOND(I,q)) +
     3              4*SBC*TZERO(I)**3)
               ELSE
                TCZERO=1.0/(0.5/TCSNOW(I)+0.5/TCTOP(I,1))
                GZERO(I)=2*TCZERO*(TZERO(I)-TSNOW(I,q))/ZSNOW(I,q)
                DTS=(QSWNET(I)+QLWIN(I)-QLWOUT(I)-QSENS(I)-QEVAP(I)-
     1              GZERO(I)) / ((SPHAIR + CPHCH(I)*D)*RHOAIR(I)
     2              *CFLUX(I) + 2*TCZERO/ZSNOW(I,q) + 4*SBC*TZERO(I)**3)
               ENDIF
              ELSE
                TCZERO=TCSNOW(I)
                GZERO(I)=2*TCZERO*(TZERO(I)-TSNOW(I,q))/ZSNOW(I,q)
                DTS=(QSWNET(I)+QLWIN(I)-QLWOUT(I)-QSENS(I)-QEVAP(I)-
     1              GZERO(I)) / ((SPHAIR + CPHCH(I)*D)*RHOAIR(I)
     2              *CFLUX(I) + 2*TCZERO/ZSNOW(I,q) + 4*SBC*TZERO(I)**3)
              ENDIF
              EVAP(I)=EVAP(I) + RHOAIR(I)*CFLUX(I)*D*DTS
              QEVAP(I)=CPHCH(I)*EVAP(I)
              QSENS(I)=QSENS(I) + RHOAIR(I)*SPHAIR*CFLUX(I)*DTS
              GZERO(I)= QSWNET(I)+QLWIN(I)-QLWOUT(I)
     1                  - 4*SBC*TZERO(I)**3*DTS - QEVAP(I) - QSENS(I)
              QLWOUT(I)=QLWOUT(I)+4*SBC*TZERO(I)**3*DTS
              TZERO(I)=TZERO(I)+DTS
              IF(ISNOW.EQ.1)    THEN
                IF(TZERO(I).GT.TFREZ) THEN
                  DTS=TFREZ-TZERO(I)
                  TZERO(I)=TFREZ
                  A(I)=17.269       
                  B(I)=35.86                    
                  WZERO(I)=0.622*611.0*EXP(A(I)*(TZERO(I)-TFREZ)/
     1              (TZERO(I)-B(I)))/PADRY(I)           
                  Q0SAT(I)=WZERO(I)/(1.0+WZERO(I))
                  EVBETA(I)=1.0
                  QZERO(I)=Q0SAT(I)
                  QLWOUT(I)=SBC*TZERO(I)**4 !+4*SBC*TZERO(I)**3*DTS !QLWOUT(I)++SBC*TZERO(I)**4
                  EVAP(I)=RHOAIR(I)*CFLUX(I)*(QZERO(I)-QA(I)) 
                  QEVAP(I)=CPHCH(I)*EVAP(I)
                  IF(TZERO(I).LT.TPOTA(I))                        THEN
                    QSENS(I)=(RHOAIR(I)*SPHAIR*CFLUX(I)+EZERO)*(TZERO(I)
     1                  -TPOTA(I))
                  ELSE
                    QSENS(I)=RHOAIR(I)*SPHAIR*CFLUX(I)*(TZERO(I)
     1                  -TPOTA(I))
                  ENDIF
                  GZERO(I)=2*TCSNOW(I)*(TZERO(I)-TSNOW(I,q))/ZSNOW(I,q)
                  QMELT(I)=QSWNET(I)+QLWIN(I)-QLWOUT(I)
     1                 -QSENS(I)-QEVAP(I)-GZERO(I)
                  if(QMELT(I).lt.0.01)then
                      GZERO(I)=GZERO(I)+QMELT(I)
                      QMELT(I)=0.0
                  endif
                  !- IF(QMELT(I).LT.0.) THEN
                  !-   GZERO(I)=GZERO(I)+QMELT(I)
                  !-   QMELT(I)=0.
                  !- ENDIF
                  !IF(QMELT(I).LT.0.0) THEN
                  !  QMELT(I)=QMELT(I)+QEVAP(I)
                  !  QEVAP(I)=0.0
                  !  EVAP(I) =0.0
                  !ENDIF
                ENDIF
              ENDIF
              IF((ISNOW.EQ.1 .AND. QMELT(I).LT.0.0) .OR.
     1            (ISNOW.EQ.0 .AND. QMELT(I).GT.0.0))     THEN
                  GZERO(I)=GZERO(I)+QMELT(I)
                  QMELT(I)=0.0
              ENDIF
              QSWNET(I)=QSWNET(I)+QTRANS(I)     
              EVAP(I)=EVAP(I)/RHOW
          ENDIF
   50 CONTINUE
      RETURN                                                                      
      END  
