      SUBROUTINE TSOLVL(TLAK,T0,LKICEH,KLAK,GLAK,Q0SAT,
     1                  KSTAR,LSTAR,QSENS,QEVAP,EVAP,ALVS,ALIR,
     2                  QSWIN,QLWIN,CSZ,TA,QA,VA,PRES,RHOAIR,CDH,
     3                  GZEROL,QTRANSL,HTCL,FLS,FICE,
     4                  CQ1A,CQ1B,CQ2A,CQ2B,CQ3A,CQ3B,
     5                  CQ1BI,CQ2BI,CQ3BI,G0,
     6                  NLAKMAX,ILG,IL1,IL2  )
C======================================================================
C     * APR 11/16 - M.MACKAY.   THERMAL EXPANSION OF ICE BUG CORRECTED
C     *                         ICE DRAFT,FREEBOARD NOW INCLUDED
C     *                         SW ATTENUATION THROUGH LEADS INCLUDED
C     * SEP 02/15 - D.VERSEGHY. ADD EFFECTS OF SNOW COVERAGE ON SURFACE
C     *                         FLUXES; ADDITIONAL DIAGNOSTIC VARIABLES; 
C     *                         COSMETIC CHANGES TO CODE AND NAMING.
C     * SEP  2/11 - M.MACKAY.  	ICE COMPUTED IN SKIN LAYER
C     * SEP 28/07 - M.MACKAY.  	SFC ENERGY BALANCE NOW COMPUTED OVER
C     *                         SKIN OF FINITE WIDTH
C     * MAR 15/07 - M.MACKAY.   COMPUTES LAKE SURFACE ENERGY BALANCE
C     *  
C
      IMPLICIT NONE
C
C ----* GLOBAL LAKE VARIABLES *---------------------------------------
C
      INTEGER NLAKMAX
      REAL,DIMENSION(ILG) :: T0,LKICEH,KLAK,GLAK,Q0SAT
      REAL,DIMENSION(ILG,NLAKMAX) :: TLAK
C
C ----* OUTPUT FIELDS *------------------------------------------------
C
      REAL,DIMENSION(ILG) :: KSTAR,LSTAR,QSENS,QEVAP,EVAP,ALVS,ALIR
C
C ----* INPUT FIELDS *------------------------------------------------
C
      REAL,DIMENSION(ILG) :: QSWIN,QLWIN,CSZ,TA,QA,VA,PRES,RHOAIR,CDH,
     1                       GZEROL,QTRANSL,HTCL,FLS,FICE,
     2                       CQ1A,CQ1B,CQ2A,CQ2B,CQ3A,CQ3B,
     3                       CQ1BI,CQ2BI,CQ3BI
      INTEGER ILG,IL1,IL2
C
C ----* INTERNAL ARRAYS *----------------------------------------------
C
      REAL,DIMENSION(ILG) :: G0
C
C ----* COMMON BLOCKS *------------------------------------------------
C
      REAL DELT,TFREZ,RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,TCW,TCICE,
     1     TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,HCPW,HCPICE,HCPSOL,
     2     HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     3     TCGLAC,CLHMLT,CLHVAP,CGRAV,CKARM,CPD,AS,ASX,CI,BS,
     4     BETA,FACTN,HMIN
      REAL TKECN,TKECF,TKECE,TKECS,HDPTHMIN,
     1     TKEMIN,DELMAX,DELMIN,EMSW,DELZLK,DELSKIN,DHMAX,
     2     TKECL,DUMAX
      COMMON /CLASS1/ DELT,TFREZ                                       
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /LAKECON/ TKECN,TKECF,TKECE,TKECS,HDPTHMIN,          
     1                 TKEMIN,DELMAX,DELMIN,EMSW,DELZLK,DELSKIN,DHMAX,
     2                 TKECL,DUMAX
C
C ----* LOCAL VARIABLES *--------------------------------------------
C
      INTEGER I,J,ITMAX
      REAL DZ,CA,CB,E0SAT,DS,COSZ
      REAL ALBW,ALBI,ALBTOT,T0old,NEWICE,ESKIN,ECOOL
      REAL EAVAIL,ATTEN1,ATTEN2,ATTEN3,EHEAT,TC,CPHCH
      REAL RHOIW,ICETOP,ICEBOT
C
C ----* LOCAL PARAMETER DEFINITIONS *-------------------------------
C
      DZ=DELZLK
      DS=DELSKIN
      RHOIW=RHOICE/RHOW
C----------------------------------------------------------------------
      DO 100 I=IL1,IL2

C======================================================================
C COMPUTE SURFACE ENERGY BALANCE FOR CURRENT TIMESTEP
C======================================================================
C COMPUTE SW FLUXES --------------------------------------------------
C   KSTAR is net SW at surface of skin
C   KLAK is penetrating SW at base of skin
C
        COSZ=MAX(CSZ(I),0.1)
        ALBW=0.045/COSZ                       !std value for water
        ALBI=0.08+0.44*(LKICEH(I))**0.28      !thin ice albedo (Vavrus et al 1996)
        ALBI=MIN(ALBI,0.44)
Cmdm    ALBI=0.65        !mdm ice albedo, test
Cmdm    ALBI=0.45        !ice albedo, Greene and Outcalt, 1985, CRST
CCCCC    ALBI=0.25      !ice albedo, Patterson and Hamblin 1988 L&O
CCCCC    ALBS=0.85      !snow albedo, Patterson and Hamblin 1988 L&O
        ALBTOT=FICE(I)*ALBI+(1.-FICE(I))*ALBW
        KSTAR(I)=(1.-FLS(I))*(1.-ALBTOT)*QSWIN(I)+FLS(I)*QTRANSL(I)
        ALVS(I)=ALBTOT
        ALIR(I)=ALBTOT

C--- Attenuation through ice (now includes attenuation through leads)
C---
          ICEBOT=RHOIW*LKICEH(I)                !ICE DRAFT
          ICETOP=LKICEH(I)-ICEBOT               !ICE FREEBOARD
          IF ( LKICEH(I) .LE. 0.0) THEN         !NO ICE 
            ATTEN1=CQ1B(I)*DS
            ATTEN2=CQ2B(I)*DS
            ATTEN3=CQ3B(I)*DS
          ELSE 
             IF (ICEBOT .GT. DS) THEN        !Z inside ice
               ATTEN1=FICE(I)*(CQ1BI(I)*(DS+ICETOP)) + 
     >                (1.-FICE(I))*CQ1B(I)*DS
               ATTEN2=FICE(I)*(CQ2BI(I)*(DS+ICETOP)) + 
     >                (1.-FICE(I))*CQ2B(I)*DS
               ATTEN3=FICE(I)*(CQ3BI(I)*(DS+ICETOP)) + 
     >                (1.-FICE(I))*CQ3B(I)*DS
             ELSE
               ATTEN1=FICE(I)*(CQ1BI(I)*LKICEH(I) + CQ1B(I)*(DS-ICEBOT))
     >              + (1.-FICE(I))*CQ1B(I)*DS
               ATTEN2=FICE(I)*(CQ2BI(I)*LKICEH(I) + CQ2B(I)*(DS-ICEBOT))
     >              + (1.-FICE(I))*CQ2B(I)*DS
               ATTEN3=FICE(I)*(CQ3BI(I)*LKICEH(I) + CQ3B(I)*(DS-ICEBOT))
     >              + (1.-FICE(I))*CQ3B(I)*DS
             ENDIF
          ENDIF
          KLAK(I)=KSTAR(I)*(CQ1A(I)*EXP(-ATTEN1) +
     >                      CQ2A(I)*EXP(-ATTEN2) +
     >                      CQ3A(I)*EXP(-ATTEN3) )
C
C COMPUTE TURBULENT FLUXES -------------------------------------------
C     * CALCULATION OF E0SAT CONSISTENT WITH CLASSI
C     * BUT CONSTANTS DIFFER FROM ROGERS&YAU
C     * Rogers and Yau values
C         CA=17.67
C         CB=29.65
C
          IF(T0(I).GE.TFREZ) THEN                              
              CA=17.269                                       
              CB=35.86                                       
          ELSE                                              
              CA=21.874                                    
              CB=7.66                                     
          ENDIF                                          
      
        CPHCH=FICE(I)*(CLHVAP+CLHMLT)+(1.0-FICE(I))*CLHVAP
        QSENS(I)=(1.-FLS(I))*RHOAIR(I)*SPHAIR*CDH(I)*VA(I)*(T0(I)-TA(I))
        E0SAT=611.0*EXP(CA*(T0(I)-TFREZ)/(T0(I)-CB))
        Q0SAT(I)=0.622*E0SAT/(PRES(I)-0.378*E0SAT)
        EVAP(I)=(1.-FLS(I))*RHOAIR(I)*CDH(I)*VA(I)*(Q0SAT(I)-QA(I))
Cmdm    QEVAP(I)=CPHCH*EVAP(I)
        QEVAP(I)=(1.-FLS(I))*RHOAIR(I)*CPHCH*CDH(I)*VA(I)*
     >                      (Q0SAT(I)-QA(I))
C
C COMPUTE NET LW AND NET SFC ENERGY -------------------------------
C GLAK IS THERMAL FLUX AT BASE OF SKIN.  THERMAL CONDUCTIVITY BASED
C ON WEIGHTED AVERAGE FOR WATER AND ICE IF ICE PRESENT IN LAYER
C
        LSTAR(I)=(1.-FLS(I))*EMSW*(QLWIN(I)-SBC*T0(I)*T0(I)*T0(I)*T0(I))
        G0(I)=KSTAR(I)-KLAK(I)+LSTAR(I)-QSENS(I)-QEVAP(I)+HTCL(I)+
     1        FLS(I)*GZEROL(I)
        IF (ICEBOT .GE. DS) THEN 
          GLAK(I)=(-2.0*TCICE/(DZ+DS))*(TLAK(I,1)-T0(I))
        ELSE IF (ICEBOT .LT. DS .AND. LKICEH(I) .GT. 0.0) THEN
          TC=(ICEBOT*TCICE + (DS-ICEBOT)*TCW)/DS
Cmdm      TC=20.0          !mdm test
          GLAK(I)=(-2.0*TC/(DZ+DS))*(TLAK(I,1)-T0(I))
        ELSE
          GLAK(I)=(-2.0*TCW/(DZ+DS))*(TLAK(I,1)-T0(I))
        ENDIF
C-----NET ENERGY FLUX INTO SKIN (W/M2)
        ESKIN= G0(I) - GLAK(I)
C
C STEP FORWARD SKIN TEMP T0
C
        T0old=T0(I)
        IF (LKICEH(I) .LE. 0.0) THEN
          T0(I) = T0(I) + (DELT/(DS*HCPW))*ESKIN
        ELSE IF (LKICEH(I) .GT. 0.0 .AND. ICEBOT .LE. DS) THEN
          T0(I) = T0(I) + (DELT/((LKICEH(I)*HCPICE)+
     >                              (DS-ICEBOT)*HCPW))*ESKIN
        ELSE
          T0(I) = T0(I) + (DELT/((DS+ICETOP)*HCPICE))*ESKIN
        ENDIF
C
C ICE GROWTH OR DECAY
C
        IF (ESKIN .LT. 0.0 .AND. ICEBOT .LT. DS) THEN
C-----NET ENERGY FLUX USED TO LOWER T0 TO TFREZ 
          IF (T0old .GT. TFREZ) THEN
            ECOOL=(DS-ICEBOT)*HCPW*(T0old-TFREZ)/DELT
          ELSE
            ECOOL=0.0
          ENDIF
C-----REMAINING ENERGY FLUX (IF ANY) USED TO FREEZE ICE
          EAVAIL=ESKIN+ECOOL
          IF (EAVAIL .LT. 0.0) THEN
           NEWICE=-(DELT/(RHOICE*CLHMLT))*EAVAIL
           LKICEH(I)=LKICEH(I)+NEWICE
           ICEBOT=RHOIW*LKICEH(I) 
           T0(I)=TFREZ
C-----LIMIT ICE GROWTH TO THE CURRENT LAYER
           IF (ICEBOT .GT. DS) THEN
             EHEAT=(RHOICE*CLHMLT*(ICEBOT-DS))/DELT
             T0(I)=TFREZ - (EHEAT*DELT)/(DS*HCPICE)
             LKICEH(I)=DS/RHOIW
           ENDIF
          ENDIF
        ENDIF

        IF (ESKIN .GT. 0.0 .AND. LKICEH(I) .GT. 0.0) THEN
C-----NET ENERGY FLUX USED FIRST TO RAISE T TO ZERO
            IF (ICEBOT .LE. DS) THEN 
             EHEAT=LKICEH(I)*HCPICE*(TFREZ-T0old)/DELT
            ELSE
             EHEAT=(DS+ICETOP)*HCPICE*(TFREZ-T0old)/DELT
            ENDIF

C-----NET ENERGY FLUX USED TO MELT ICE
          IF (ESKIN .GT. EHEAT) THEN
           NEWICE=-(DELT/(RHOICE*CLHMLT))*(ESKIN-EHEAT)
           LKICEH(I)=LKICEH(I)+NEWICE
           T0(I)=TFREZ
C-----LIMIT ICE MELT TO THE CURRENT LAYER
           IF (LKICEH(I) .LT. 0.0) THEN
             EHEAT=-(RHOICE*CLHMLT*LKICEH(I))/DELT
             T0(I)=TFREZ + (EHEAT*DELT)/(DS*HCPW)
             LKICEH(I)=0.0
           ENDIF
          ENDIF
        ENDIF
C -----------------
100     CONTINUE

      RETURN
      END
