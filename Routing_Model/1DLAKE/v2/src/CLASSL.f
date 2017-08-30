!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer,
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer
!version 3 or (at your option) any later version that should be found at:
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software;
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec),
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!
!-------------------------------------- LICENCE END --------------------------------------
      SUBROUTINE CLASSL (HLAK, LLAK, BLAK, NLAK, TLAK,   
     1                   T0, HDPTH, LKICEH, SNICEH,ROFICEH,
     2                   SNO, RHOSNO, TSNOW, ALBSNO, WSNOW,
     3                   CDH, CDM, QSENS, TFLUX, QEVAP, EVAP, QFLUX,
     4                   EVPPOT, EVAPB, GT, QSURF, DRAG,
     5                   ST, SU, SV, SQ, SH, QLWAVG, ALVS, ALIR, 
     6                   FSGL, FLGL, HFSL, HEVL, HMFL, HTCL,
     7                   FSGS, FLGS, HFSS, HEVS, HMFN, HTCS,
     8                   PCPL, PCPN, QFL, QFN, ROFN, FICE, FLS, GZEROSL,
     9                   EXPW, DTEMP, TKE, DELU, GRED, RHOMIX,   
     A                   QSWINV, QSWINI, QLWIN, UWIND, VWIND, TA, QA,
     B                   RHOAIR, PADRY, PRES, CSZ, ZREFM, ZREFH, 
     C                   ZDIAGM, ZDIAGH, R, TR, S, TS, RHOSNI, RADJ,
     D                   ASVDAT, ASIDAT, FSDB, FSFB, FSSB, REFSN, BCSN,
     E                   ILG, IL1, IL2, JL, NLAKMAX, ISLFD, IZREF, ITG,
     F                   IALS, NBS, ISNOALB, IGL, IRSTRT,
     G                   NSTEP, IYEAR, IDAY, IHOUR, IMIN, TSED  )
C
C=======================================================================
C     * APR 11/16 - M.MACKAY.   THERMAL EXPANSION OF ICE BUG CORRECTED
C     *                         ICE DRAFT,FREEBOARD NOW INCLUDED
C     *                         SW ATTENUATION THROUGH LEADS INCLUDED
C     * JAN  25/16 - M.MACKAY.  FRACTIONAL ICE COVER INCLUDED BUT SET
C     *                         TO 0 OR 100% FOR NOW
C     * SEP 09/15 - D.VERSEGHY. ADD CALLS TO CLASS SUBROUTINES AND
C     *                         SUPPLEMENTARY CODE FOR MODELLING OF
C     *                         SNOW ON LAKE ICE; ADDITIONAL DIAGNOSTIC 
C     *                         CALCULATIONS.
C     * APR 13/15 - D.VERSEGHY. RE-ORDERING OF SUBROUTINE CALL;
C     *                         COSMETIC CHANGES TO CODE AND NAMING.
C     * FEB 1/2013 - M.MACKAY.  SOLVES FOR LAKE TEMPERATURE PROFILES,
C     *                         MIXED LAYER DEPTH, AND FLUXES.
C     *                        	SFC ENERGY BALANCE COMPUTED OVER
C     *                         SKIN OF FINITE WIDTH
C=======================================================================
      IMPLICIT NONE
C
C ----* LAKE MODEL VARIABLES *----------------------------------------
C
      INTEGER NLAKMAX
      INTEGER,DIMENSION(ILG) :: NLAK
      REAL,DIMENSION(ILG) :: HLAK, LLAK, BLAK, T0, HDPTH,LKICEH,SNICEH,
     1                      EXPW,DTEMP,TKE,DELU,GRED,RHOMIX,TSED,ROFICEH
      REAL,DIMENSION(ILG) :: SNO, RHOSNO, TSNOW, ALBSNO, WSNOW
      REAL,DIMENSION(ILG,NLAKMAX) :: TLAK, QFLX, FFLX
C
C ----* INTEGER CONSTANTS *-------------------------------------------
C
      INTEGER ISNOW,ISLFD,IZREF,ITG,ILG,IL1,IL2,JL,IALS,NBS,ISNOALB,IGL 
      INTEGER IRSTRT,NSTEP,IYEAR,IDAY,IHOUR,IMIN
C
C ----* INPUT FIELDS *------------------------------------------------
C
      REAL,DIMENSION(ILG) :: QSWINV, QSWINI, QLWIN, UWIND, VWIND, TA, 
     1                       QA, RHOAIR, PADRY, PRES, CSZ, ZREFM, ZREFH,
     2                       ZDIAGM, ZDIAGH, R, TR, S, TS, RHOSNI, RADJ
C
      REAL   ASVDAT(ILG),  ASIDAT(ILG),  REFSN (ILG),  BCSN  (ILG)     
C
      REAL   FSDB(ILG,NBS), FSFB(ILG,NBS), FSSB(ILG,NBS)
C
C ----* DIAGNOSTIC OUTPUT FIELDS *-------------------------------------
C
      REAL,DIMENSION(ILG) :: FSGL, FLGL, HFSL, HEVL, HMFL, HTCL, DRAGL,
     1                       FSGS, FLGS, HFSS, HEVS, HMFN, HTCS, DRAGS,
     2                       PCPL, QFL, PCPN, QFN, ROFN,
     3                       CDH, CDM, QSENS, TFLUX, QEVAP, EVAP, QFLUX,
     4                       EVPPOT, EVAPB, GT, QSURF, DRAG,
     5                       ST, SU, SV, SQ, SH, QLWAVG, ALVS, ALIR,
     6                       ILMO, HBL, UE, FTEMP, FVAP, OSW 
C
C ----* INTERNAL ARRAYS *----------------------------------------------
C
      REAL,DIMENSION(ILG) :: Q0,DISS,BFLX,FQU,FSHEAR,FENTRA,TRAN,
     1                       CQ1A, CQ1B, CQ2A, CQ2B, CQ3A, CQ3B,
     2                       CQ1BI, CQ2BI, CQ3BI, FICE, CDHL, CDML,
     3                       HRPCPL,HSPCPL,HCONV
      REAL,DIMENSION(ILG) :: TCMIX, VA, QSWIN, G0, F0, USTAR, LKICEH0
      REAL,DIMENSION(NLAKMAX) :: TLAK0
C
      REAL   FLS   (ILG),  HCPSNO(ILG),  ZSNOW (ILG)   
C
      REAL   ALVSSN(ILG),  ALIRSN(ILG),  ALVSSC(ILG),  ALIRSC(ILG),     
     1       ALVSG (ILG),  ALIRG (ILG),  TRSNOWC(ILG),
     2       ALVSL (ILG),  ALIRL (ILG)
C                                                                       
      REAL   GCONSTS(ILG), GCOEFFS(ILG), CPHCHS(ILG),  TCSNOW(ILG),
     1       ZRSLDM(ILG),  ZRSLDH(ILG),  ZRSLFM(ILG),  ZRSLFH(ILG),
     2       ZDSLM (ILG),  ZDSLH (ILG),  ZOSCLM(ILG),  ZOSCLH(ILG),
     3       ZOMLNS(ILG),  ZOELNS(ILG),  ZOM   (ILG),  ZOH   (ILG),
     4       TVIRTA(ILG),  TPOTA (ILG),  CRIBS (ILG),  CEVAP (ILG), 
     5       TSTART(ILG),  FCOR  (ILG),  PCP   (ILG),  FTOT  (ILG),
     6       TSURF (ILG),  QSURFL(ILG),  TSNBOT(ILG),  FSCALL(ILG),
     7       RPCN  (ILG),  TRPCN (ILG),  SPCN  (ILG),  TSPCN (ILG)
C
      REAL   RMELT (ILG),  QMELT (ILG),  RHOMAX(ILG),
     1       RALB  (ILG),  WLOST (ILG),  SN    (ILG),  SL    (ILG),
     2       RN    (ILG),  RL    (ILG),
     2       TRUNOF(ILG),  RUNOFF(ILG),  TOVRFL(ILG),  OVRFLW(ILG)
C
      REAL   HTC   (ILG,IGL)
C
      INTEGER             IWATER(ILG),   ISAND (ILG,IGL)
C                                                                                 
C     * TSOLVE OUTPUT ARRAYS.                                                  
C                                                                       
      REAL QSWNS (ILG),    QLWOS (ILG),    QTRANSL(ILG),   QSENSS(ILG), 
     1     QEVAPS(ILG),    EVAPS (ILG),    TZEROS(ILG),    QZEROS(ILG), 
     2     GSNOW (ILG),    GZEROSL(ILG),   QMELTS(ILG),    CDHS  (ILG), 
     3     CDMS  (ILG),    RIBS  (ILG),    CFLUXS(ILG),    FTEMPS(ILG), 
     4     FVAPS (ILG),    ILMOS (ILG),    UES   (ILG),    HS    (ILG)  
C                                                                       
      INTEGER          ITERCT(ILG,6,50),   IEVAP (ILG)                  
C                                                                       
C     * RADIATION BAND-DEPENDENT ARRAYS.                                          
C                                                                       
      REAL TRSNOWG(ILG,NBS), ALSNO(ILG,NBS), 
     1     TRTOP  (ILG,NBS)                                             
C                                                                       
C     * INTERNAL WORK ARRAYS FOR TSOLVE.                                           
C                                                                       
      REAL TSTEP (ILG),    TVIRTS(ILG),    EVBETA(ILG),    Q0SAT (ILG), 
     1     RESID (ILG),    DCFLXM(ILG),    CFLUXM(ILG),    WZERO (ILG),
     2     A     (ILG),    B     (ILG),                                 
     3     LZZ0  (ILG),    LZZ0T (ILG),    FM    (ILG),    FH    (ILG) 
C                                                                       
      INTEGER              ITER  (ILG),    NITER (ILG),    JEVAP (ILG), 
     1                     KF    (ILG)                                  
C                                                                       
C ----* COMMON BLOCK PARAMETERS *--------------------------------------
C
      REAL DELT,TFREZ,RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,TCW,TCICE,
     1     TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,HCPW,HCPICE,HCPSOL,
     2     HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     3     TCGLAC,CLHMLT,CLHVAP,DELTA,CGRAV,CKARM,CPD,AS,ASX,CI,BS,
     4     BETA,FACTN,HMIN,ANGMAX                          
      REAL TKECN,TKECF,TKECE,TKECS,TKECL,HDPTHMIN,
     1     TKEMIN,DELMAX,DELMIN,EMSW,DELZLK,DELSKIN,DHMAX,DUMAX
C
C ----* CLASS COMMON BLOCKS *------------------------------------------
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

      COMMON /LAKECON/ TKECN,TKECF,TKECE,TKECS,HDPTHMIN,        
     2                 TKEMIN,DELMAX,DELMIN,EMSW,DELZLK,DELSKIN,
     3                 DHMAX,TKECL,DUMAX
C
C ----* LOCAL VARIABLES *---------------------------------------------
C
      INTEGER I,J,K,JBOT,JTOP,NLEV,JMIX,JMIXP1
      REAL Z,DAY,DECL,HOUR,TKE_OLD,ZBOT,TBOT,WEDB,
     1     Z2,TMIX,ZMIX,HTOP,HBOT,ZTOP,TTOP,
     2     RHO1,RHO2,TC1,TC2,TBAR,ATTEN1,ATTEN2,ATTEN3,DELTHRM,HCAP,
     3     EAVAIL,EHEAT,ECOOL,EFLUX,ZICE,NEWICE,TC,SNOLIM,ICELIM,
     4     FACTM,FACTH,RATIOM,RATIOH
      REAL TSEDT,TSEDB,TCSED,HCPSED,DSED,FFLXSED,ICEMASS
      REAL ICECAP,PORE,MASSL,SNO0,ALPHA,ETA,ZSNOW0,ICE0,BBLFAC
      REAL MASS,TC0,RHO0,RHOIW,ICETOP,ICEBOT,ICETOP0,ICEBOT0,ROFICE,
     1     HFREZ,HWARM,HFLX,HFLXI,HFLXS,HCPRAT,QMLTS,QSED,XXX
      INTEGER, PARAMETER :: DP=SELECTED_REAL_KIND(15,307)
Ctst  REAL(dp) :: TBAR,RHO1,RHO2,TC1,TC2
C
C SEDIMENT PROPERTIES  ------------------------------------------------
C     DSED=5.0       !THICKNESS OF SEDIMENT LAYER (m)  (STD)
C     TSEDB=TFREZ+6.0 !CONSTANT TEMP AT BOTTOM OF SEDIMENT (K)  (STD)
C     HCPSED=2.13E6  !VOLUMETRIC HEAT CAPACITY OF SEDIMENT      (STD)
C     TCSED=2.5      !THERMAL CONDUCTIVITY OF SEDIMENT (0 FOR ADIABATIC LOWER BC)
C----------------------------------------------------------------------
      DSED=5.0       !THICKNESS OF SEDIMENT LAYER (m)  test
      TSEDB=TFREZ+4.0 !CONSTANT TEMP AT BOTTOM OF SEDIMENT (K)  (STD)
      HCPSED=2.13E6  !VOLUMETRIC HEAT CAPACITY OF SEDIMENT 
      TCSED=0.0      !THERMAL CONDUCTIVITY OF SEDIMENT (0 FOR ADIABATIC LOWER BC)
C     QSED=3208.8
      QSED=0.0       !SEDIMENT HEAT FLUX MULTIPLIER
C ---------------------------------------------------------------------
      ISNOW=1
      SNOLIM=0.10
      RHOIW=RHOICE/RHOW
C======================================================================
C    * CLASS SMALL LAKE MODULE
C-----------------------------------------------------------------------
C    * INITIAL CONDITIONS (NSTEP=1)
C    * EQUATION OF STATE FROM FARMER AND CARMACK (1981,JPO), BUT 
C    * NEGLECTS SALINITY AND PRESSURE EFFECTS
C
      IF (NSTEP.EQ.1 .OR. IRSTRT.EQ.1) THEN
          DO 100 I=IL1,IL2
              LKICEH(I)=0.0     !INITIAL ICE COVER SET TO ZERO
              SNICEH(I)=0.0     !INITIAL SNOW ICE  SET TO ZERO
              ROFICEH(I)=0.0     !INITIAL runoff ICE  SET TO ZERO
              T0(I)=TLAK(I,1)   !INITIAL SKIN TEMP SET TO FIRST LAYER TEMP
              NLEV=NLAK(I)
              TSED(I)=TLAK(I,NLEV)   !INITIAL SEDIMENT TEMP SET TO LOWEST LAYER TEMP
              TKE(I)=TKEMIN
              DELU(I)=0.0
C ---* 
C ---* INITIAL MIXED LAYER DEPTH ESTIMATED BASED ON INITIAL T PROFILE
C ---* 
              DO 50 J=1,NLAK(I)-1
                  JMIX=J
                  TTOP=TLAK(I,J)
                  TBOT=TLAK(I,J+1)
                  IF (TTOP-TBOT .GT. 1.0) EXIT
50            CONTINUE
              HDPTH(I)=DELZLK*JMIX
              DTEMP(I)=TLAK(I,JMIX)-TLAK(I,JMIX+1) !see Spigel et al 1986
C ---* 
C ---* INITIAL MIXED LAYER TEMP (CELSIUS), DENSITY, EXPANSIVITY, 
C ---* AND REDUCED GRAVITY
C ---* 
             TCMIX(I)=( (TLAK(I,1)+TLAK(I,JMIX))/2.0) -TFREZ 
             CALL EQNST(EXPW(I),RHOMIX(I),TCMIX(I),0.0)
             GRED(I) = GRAV*ABS(RHOW-RHOMIX(I))/RHOW
             IF (GRED(I).LT.0.)  CALL XIT('CLASSL',-1)
100       CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C     * ICE COVER AND SNOW PARAMETERS AT BEGINNING OF TIMESTEP
C---------------------------------------
      DO 120 I=IL1,IL2
          LKICEH0(I)=LKICEH(I)
          ICELIM=LLAK(I)*1.0E-5        
          FICE(I)=MIN(ICELIM,LKICEH(I))/ICELIM   
          FTOT(I)=1.0
          IF(SNO(I).GT.0.0)    THEN
              ZSNOW(I)=SNO(I)/RHOSNO(I)
              IF(ZSNOW(I).GE.(SNOLIM-0.00001)) THEN
                  FLS(I)=1.0
              ELSE
                  FLS(I)=ZSNOW(I)/SNOLIM
                  ZSNOW(I)=SNOLIM
                  WSNOW(I)=WSNOW(I)/FLS(I)
              ENDIF
          ELSE
              ZSNOW(I)=0.0
              FLS(I)=0.0
          ENDIF
          IF(S(I).GT.0.0)                                THEN
              IF(LKICEH(I).GE.DELSKIN)          THEN
                  IF(FLS(I).GT.0.0)         THEN
                      SN(I)=S(I)/FLS(I)
                  ELSE
                      SN(I)=S(I)
                  ENDIF
                  SL(I)=0.0
              ELSE
                  SN(I)=0.0
                  SL(I)=S(I)
              ENDIF
          ELSE
              SN(I)=0.0
              SL(I)=0.0
          ENDIF
          IF(R(I).GT.0.0)                                THEN
              IF(FLS(I).GT.0.0)         THEN
                  RN(I)=R(I)
                  RL(I)=(1.0-FLS(I))*R(I)
              ELSE
                  RN(I)=0.0
                  RL(I)=R(I)
              ENDIF
          ELSE
              RN(I)=0.0
              RL(I)=0.0
          ENDIF
          IF(FLS(I).GT.0) THEN
              FSCALL(I)=FLS(I)
              TSTART(I)=TSNOW(I)
          ELSE
              FSCALL(I)=1.0
              TSTART(I)=TFREZ
          ENDIF
          PCPN(I) =FLS(I)*RHOW*RN(I)+FSCALL(I)*RHOSNI(I)*SN(I)
          HTCS(I)=0.0
          HMFN(I)=0.0
          QFN(I)=0.0
          ROFN(I)=0.0
          GZEROSL(I)=0.0
          QMELTS(I)=0.0
          HRPCPL(I)=(TR(I)+TFREZ-T0(I))*RL(I)*HCPW
          HSPCPL(I)=(TS(I)+TFREZ-T0(I))*SL(I)*SPHICE*RHOSNI(I)
          HCONV(I)=CLHMLT*SL(I)*RHOSNI(I)
          HTCL(I)=HRPCPL(I)+HSPCPL(I)-HCONV(I)
120   CONTINUE
C-----------------------------------------------------------------------
C     * WIND SPEED AND PRECIP
C
      DO 140 I=IL1,IL2
          VA(I)=MAX(VMIN,SQRT(UWIND(I)*UWIND(I)+VWIND(I)*VWIND(I)))
          FCOR(I)=2.0*7.29E-5*SIN(RADJ(I))
          PCP(I)=R(I)+S(I)
140   CONTINUE
C
C-----------------------------------------------------------------------
C     * SNOW PACK ENERGY AND WATER BALANCE
C
!      CALL SNOALBA(ALVSSN,ALIRSN,ALVSSC,ALIRSC,ALBSNO,            
!     1             TRSNOWC,ALSNO,TRSNOWG,FSDB,FSFB,RHOSNO,   
!     2             REFSN,BCSN,SNO,CSZ,ZSNOW,FLS,ASVDAT,ASIDAT,  
!     3             ALVSG,ALIRG,                                  
!     4             ILG,IGL,IL1,IL2,JL,IALS,NBS,ISNOALB)            
      CALL SNOALBA(ALVSSN,ALIRSN,ALVSSC,ALIRSC,ALBSNO,
     1             TRSNOWC,ZSNOW,FLS,ASVDAT,ASIDAT,
     2             ILG,IGL,IL1,IL2,JL,IALS)
                                                                        
      CALL TLSPREP(GCOEFFS,GCONSTS,CPHCHS,TCSNOW,HCPSNO,IWATER,
     1             ZRSLDM,ZRSLDH,ZRSLFM,ZRSLFH,ZDSLM,ZDSLH,
     2             ZOSCLM,ZOSCLH,ZOMLNS,ZOELNS,ZOM,ZOH,
     3             TVIRTA,TPOTA,CRIBS,DRAGS,CEVAP,IEVAP,ISAND,
     4             FLS,ZSNOW,TSNOW,RHOSNO,WSNOW,ZREFM,ZREFH,
     5             ZDIAGM,ZDIAGH,TA,QA,VA,IZREF,ILG,IL1,IL2,IGL,JL)
C
!      CALL TSOLVE(ISNOW,FLS,                                       
!     1            QSWNS,QLWOS,QTRANSL,QSENSS,QEVAPS,EVAPS,          
!     2            TZEROS,QZEROS,GSNOW,QMELTS,CDHS,CDMS,RIBS,CFLUXS, 
!     3            FTEMPS,FVAPS,ILMOS,UES,HS,                           
!     4            QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,                 
!     5            ALVSSN,ALIRSN,CRIBS,CPHCHS,CEVAP,TVIRTA,          
!     6            ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,       
!     7            GCONSTS,GCOEFFS,TSTART,PCP,TRSNOWG,FSSB,ALSNO,   
!     8            IWATER,IEVAP,ITERCT,ISAND,                      
!     9            ISLFD,ITG,ILG,IGL,IL1,IL2,JL,NBS,ISNOALB,        
!     A            TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,                
!     B            DCFLXM,CFLUXM,WZERO,TRTOP,A,B,                  
!     C            LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF)           
      CALL TSOLVE(ISNOW,FLS,
     1            QSWNS,QLWOS,QTRANSL,QSENSS,QEVAPS,EVAPS,
     2            TZEROS,QZEROS,GSNOW,QMELTS,CDHS,CDMS,RIBS,CFLUXS,
     3            FTEMPS,FVAPS,ILMOS,UES,HS,
     4            QSWINV,QSWINI,QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,
     5            ALVSSN,ALIRSN,CRIBS,CPHCHS,CEVAP,TVIRTA,
     6            ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,
     7            GCONSTS,GCOEFFS,TSTART,TRSNOWG,PCP,
     8            IWATER,IEVAP,ITERCT,ISAND,
     9            ISLFD,ITG,ILG,IGL,IL1,IL2,JL,
     A            TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,
     B            DCFLXM,CFLUXM,WZERO,TRTOP,A,B,
     C            LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF)
C
      CALL TLSPOST(GSNOW,TSNOW,WSNOW,RHOSNO,QMELTS,GZEROSL,
     1            TSNBOT,HTCS,HMFN,QFN,EVAPS,RPCN,TRPCN,SPCN,TSPCN,
     2            GCONSTS,GCOEFFS,T0,ZSNOW,TCSNOW,HCPSNO,QTRANSL,
     3            RN,TR,SN,TS,TZEROS,RHOSNI,
     4            FLS,DELSKIN,ILG,IL1,IL2,JL      )
C                                                                       
      CALL SNOVAP(RHOSNO,ZSNOW,HCPSNO,TSNOW,EVAPS,QFN,QFL,HTCS,
     1            WLOST,TRUNOF,RUNOFF,TOVRFL,OVRFLW,
     2            FLS,RPCN,SPCN,RHOSNI,WSNOW,ILG,IL1,IL2,JL)
C
      CALL TMELT (ZSNOW,TSNOW,QMELTS,RPCN,TRPCN,GZEROSL,RALB,
     1            HMFN,HTCS,HTC,FLS,HCPSNO,RHOSNO,WSNOW,
     2            ISAND,IGL,ILG,IL1,IL2,JL)
C
      CALL SNINFL(RPCN,TRPCN,ZSNOW,TSNOW,RHOSNO,HCPSNO,WSNOW,
     1            HTCS,HMFN,PCPL,ROFN,FLS,ILG,IL1,IL2,JL)   
C
      CALL SNOALBW(ALBSNO,RHOSNO,ZSNOW,HCPSNO,TSNOW,              
     1             FLS,SPCN,RMELT,WSNOW,RHOMAX,ISAND,                 
     2             ILG,IGL,IL1,IL2,JL)                             
C                                                                                  
      CALL SNOADD(ALBSNO,TSNOW,RHOSNO,ZSNOW,HCPSNO,HTCS,
     1            FSCALL,SPCN,TSPCN,RHOSNI,WSNOW,ILG,IL1,IL2,JL)
C
      DO I=IL1,IL2
          IF(ZSNOW(I).GT.0.0)                           THEN
              TSNOW(I)=TSNOW(I)+TFREZ
              SNO(I)=ZSNOW(I)*RHOSNO(I)*FSCALL(I)
              WSNOW(I)=WSNOW(I)*FLS(I)
          ELSE
              TSNOW(I)=0.0
              RHOSNO(I)=0.0
              SNO(I)=0.0
              WSNOW(I)=0.0
          ENDIF
C-----------------------------------------------------------
C Melt last bit of snow if below threshold or no ice exists
C
          IF(SNO(I).LT.1.0E-3 .AND. SNO(I).GT.0.0 .OR. LKICEH(I).LE.0.0)
     1    THEN
              ROFN(I)=ROFN(I)+(SNO(I)+WSNOW(I))/DELT
              PCPL(I)=PCPL(I)+(SNO(I)+WSNOW(I))/DELT
              HTCS(I)=HTCS(I)-TSNOW(I)*(SPHICE*SNO(I)+SPHW*WSNOW(I))/
     1                DELT
              TSNOW(I)=0.0
              RHOSNO(I)=0.0
              SNO(I)=0.0
              WSNOW(I)=0.0
          ENDIF
C---------------------------------------------------------------
C Heat capacity calculation for use if necessary by: (1) heat 
C added from snowmelt/rain runoff; and (2) snow ice production
C Any heat added goes into layer 1, not the skin layer.
          ICEBOT=RHOIW*LKICEH0(I)   !bottom of floating ice
          ZTOP=DELSKIN             !top of first layer
          ZBOT=DELSKIN + DELZLK    !bottom of first layer
          IF (ICEBOT .GE. ZBOT) THEN 
            HCAP=HCPICE
          ELSE IF (ICEBOT .LE. ZTOP) THEN
            HCAP=HCPW
          ELSE 
            Z=ICEBOT-ZTOP
            HCAP=(Z*HCPICE + (DELZLK-Z)*HCPW)/DELZLK
          ENDIF
C-----------------------------------------------------------
C Add heat of snow melt/rain reaching bottom of snowcover to ice
C Heat added to first layer below skin
C ROFN cools then runs off - ie does not add to LKICEH
C
C-----------------------------------------------------------
C RUNOFF ICE PRODUCTION/HEATING TURNED OFF: JUNE 2016, M.MACKAY
C
          IF (ROFN(I).GT.0.0 .AND. LKICEH0(I).GT.0.0) THEN
             ROFICE=0.0             !runoff ice production turned off
Cmdm         ROFICE=DELT*ROFN(I)/RHOICE    !ice production this timestep
Cmdm         ROFICEH(I)=ROFICEH(I)+ROFICE  !cumulative diagnostic
C Cool ROFN to TFREZ, then add heat to ice layer below skin
             HWARM=HCPW*(TRPCN(I)-0.0)*DELT*ROFN(I)/RHOW
             HFREZ=RHOICE*CLHMLT*ROFICE
             TLAK(I,1)=TLAK(I,1) + (HFREZ+HWARM)/(HCAP*DELZLK)
Cmdm         LKICEH(I)=LKICEH(I)+ROFICE
          ENDIF
C======================================================================
C SNOW ICE
c---------
C Produce snow-ice if weight of snow cover sufficient
C Pore volume in snow fills with water then freezes
C
          BBLFAC=1.0                         !non-bubble fraction in snow ice
          ICECAP =LKICEH0(I)*(RHOW-RHOICE)   !snow holding capacity of ice
          IF (SNO(I).GT.ICECAP .AND. LKICEH0(I).GT.0.0) THEN

             ICE0=LKICEH0(I)            !initial ice thickness
             SNO0=SNO(I)                     !initial snow mass
             ZSNOW0=SNO(I)/RHOSNO(I)         !initial mean snow depth 
             PORE=(RHOICE-RHOSNO(I))/RHOICE  !pore volume in snow
             ALPHA=(RHOW*PORE*BBLFAC + RHOICE*(1.0-PORE))/RHOICE
             ETA=(RHOW-RHOICE)/RHOSNO(I)
C Final snow cover mass equals new ice holding capacity
             SNO(I)=RHOSNO(I)*ETA*(ICE0+ALPHA*ZSNOW0)/
     1               (1.0+ALPHA*ETA)
             LKICEH(I)=SNO(I)/(RHOSNO(I)*ETA)
C Mass of liquid water that freezes in slush layer
             MASSL=RHOW*PORE*BBLFAC*(SNO0-SNO(I))/RHOSNO(I)
C Cumulative snow-ice diagnostic
             SNICEH(I)=SNICEH(I)+LKICEH(I)-ICE0
C Add heat of fusion to snow pack
C First warm snow to TFREZ, then freeze lake water
C Lake water is assumed to be at TFREZ already (ie comes from layer that
C contains ICEBOT, which is at TFREZ)
            HWARM=HCPSNO(I)*(TFREZ-TSNOW(I))*(SNO0-SNO(I))/
     1                    RHOSNO(I)
            HFREZ=CLHMLT*MASSL
            HFLX=HFREZ-HWARM
C Latent heat goes goes into snowpack unless snow too thin
           IF (SNO(I) .GE. 5.0E-3) THEN
C Partition latent heat between ice and snow based on heat capacities
Cmdm test   HCPRAT=HCPSNO(I)/HCPICE
Cmdm test   HFLXS=HFLX*(HCPRAT/(1.0+HCPRAT))
Cmdm test   HFLXS=HFLX/2.0
Cmdm test   HFLX=0.0   !mdm test
            HFLXS=HFLX           !all latent heat goes into snow cover

            HFLXI=HFLX-HFLXS
            TLAK(I,1)=TLAK(I,1) + HFLXI/(HCAP*DELZLK) !mdm test
            TSNOW(I)=TSNOW(I)+HFLXS/(HCPSNO(I)*SNO(I)/RHOSNO(I)) 
            IF(TSNOW(I).GT.TFREZ) THEN
              QMLTS=(TSNOW(I)-TFREZ)*HCPSNO(I)*SNO(I)/RHOSNO(I) !J/m2
              IF (QMLTS .GT. SNO(I)*CLHMLT) THEN
C All snow melts. Excess heat put in lake
                print*, "all snow melts during flooding"
                HFLX=QMLTS-SNO(I)*CLHMLT
                TLAK(I,1)=TLAK(I,1) + HFLX/(HCAP*DELZLK)
                SNO(I)=0.0
                TSNOW(I)=0.0
              ELSE
C Some snow melts
                SNO(I)=SNO(I)-QMLTS/CLHMLT
                TSNOW(I)=TFREZ
              ENDIF
            ENDIF                                                  
           ELSE
            print*, "no snow: all heat into lake"
            TLAK(I,1)=TLAK(I,1) + (HFREZ-HWARM)/(HCAP*DELZLK)
           ENDIF
          ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C     * LAKE DRAG COEFFICIENTS 
C
      CALL DRCOEFL (CDML,CDHL,DRAGL,VA,T0,TA,QA,PRES,ZREFM,ZREFH,FICE,
     1              NSTEP,ILG,IL1,IL2)
C-----------------------------------------------------------------------
C     * TOTAL SOLAR RADIATION, AND WATER FRICTION VELOCITY
C
      DO 150 I=IL1,IL2
          QSWIN(I) = QSWINV(I) + QSWINI(I)
          IF (LKICEH(I) .LE. 0) THEN
              USTAR(I)=VA(I)*SQRT(CDML(I)*RHOAIR(I)/RHOW)
          ELSE
              USTAR(I)=0.0            !no wind stress through ice
          ENDIF
150   CONTINUE
C
C------------------------
C REMOVE STATIC INSTABILITY IF PRESENT BY MIXING WITH ADJACENT LAYER
C  - include skin layer if no ice is present
C  -  Sept 2, 2011 M MacKay
      CALL FREECONV(LKICEH,T0,TLAK,RHOIW,NLAK,NLAKMAX,
     >                  ILG,IL1,IL2,IYEAR,IHOUR,IDAY,IMIN )
C-----------------------------------------------------------------------
C --- * COMPUTE WATER EXTINCTION COEFFICIENTS
C
      CALL LKTRANS (CQ1A,CQ1B,CQ2A,CQ2B,CQ3A,CQ3B,BLAK,IL1,IL2,ILG,
     1              CQ1BI,CQ2BI,CQ3BI)
C
C-----------------------------------------------------------------------
C --- * COMPUTE SURFACE ENERGY BALANCE FOR CURRENT TIMESTEP AND STEP
C --- * FORWARD SKIN TEMP T0.  COMPUTE ICE COVER IN SKIN LAYER.
      CALL TSOLVL(TLAK,T0,LKICEH,Q0,F0,QSURFL, 
     1            FSGL,FLGL,HFSL,HEVL,QFL,ALVSL,ALIRL,
     2            QSWIN,QLWIN,CSZ,TA,QA,VA,PRES,RHOAIR,CDHL,
     3            GZEROSL,QTRANSL,HTCL,FLS,FICE,
     4            CQ1A,CQ1B,CQ2A,CQ2B,CQ3A,CQ3B,
     5            CQ1BI,CQ2BI,CQ3BI,G0,
     6            NLAKMAX,ILG,IL1,IL2   )
C
C-----------------------------------------------------------------------
C --- * COMPUTE SOLAR FLUX (QFLX) FOR CURRENT TIMESTEP
C        include attenuation through ice (including leads) if present
C
      DO 200 I=IL1,IL2
          ICEBOT=RHOIW*LKICEH(I)
          ICETOP=LKICEH(I)-ICEBOT
          DO 180 J=1,NLAK(I)
              Z=DELSKIN + DELZLK*J 
              IF ( LKICEH(I) .LE. 0.0) THEN        !NO ICE 
                  ATTEN1=CQ1B(I)*Z
                  ATTEN2=CQ2B(I)*Z
                  ATTEN3=CQ3B(I)*Z
              ELSE 
                 IF (ICEBOT .GT. Z) THEN        !Z inside ice
                     ATTEN1=FICE(I)*(CQ1BI(I)*(Z+ICETOP)) +
     >                      (1.-FICE(I))*CQ1B(I)*Z
                     ATTEN2=FICE(I)*(CQ2BI(I)*(Z+ICETOP)) +
     >                      (1.-FICE(I))*CQ2B(I)*Z
                     ATTEN3=FICE(I)*(CQ3BI(I)*(Z+ICETOP)) +
     >                      (1.-FICE(I))*CQ3B(I)*Z
                 ELSE
                  ATTEN1=FICE(I)*(CQ1BI(I)*LKICEH(I)+CQ1B(I)*(Z-ICEBOT))
     >                 + (1.-FICE(I))*CQ1B(I)*Z
                  ATTEN2=FICE(I)*(CQ2BI(I)*LKICEH(I)+CQ2B(I)*(Z-ICEBOT))
     >                 + (1.-FICE(I))*CQ2B(I)*Z
                  ATTEN3=FICE(I)*(CQ3BI(I)*LKICEH(I)+CQ3B(I)*(Z-ICEBOT))
     >                 + (1.-FICE(I))*CQ3B(I)*Z
                 ENDIF
              ENDIF
              QFLX(I,J)=FSGL(I)*(CQ1A(I)*EXP(-ATTEN1) +
     >                        CQ2A(I)*EXP(-ATTEN2) +
     >                        CQ3A(I)*EXP(-ATTEN3) )
180       CONTINUE
200   CONTINUE
C
C-----------------------------------------------------------------------
C --- * COMPUTE CONDUCTIVE HEAT FLUX (FFLX) FOR CURRENT TIMESTEP
C --- * THERMAL CONDUCTIVITY IS WEIGHTED AVERAGE OF ICE AND WATER
C --- * IF ICE IS PRESENT IN LAYER
C
      DO 300 I=IL1,IL2
        ICEBOT=RHOIW*LKICEH(I)
        ICETOP=LKICEH(I)-ICEBOT
        DO 250, J=1,NLAK(I)-1
          ZTOP=DELSKIN + DELZLK*(J -1)
          ZBOT=DELSKIN + DELZLK*J
          IF (ICEBOT .GE. ZBOT) THEN 
            FFLX(I,J)=(-TCICE/DELZLK)*(TLAK(I,J+1)-TLAK(I,J))
          ELSE IF (ICEBOT .LT. ZBOT .AND. ICEBOT .GT. ZTOP) THEN
            TC=((ICEBOT-ZTOP)*TCICE + (ZBOT-ICEBOT)*TCW)/DELZLK
            FFLX(I,J)=(-TC/DELZLK)*(TLAK(I,J+1)-TLAK(I,J))
          ELSE
            FFLX(I,J)=(-TCW/DELZLK)*(TLAK(I,J+1)-TLAK(I,J))
          ENDIF
250     CONTINUE
        NLEV=NLAK(I)
C ---* COMPUTE THERMAL FLUX AT LAKE BOTTOM INTO SEDIMENT
C ---* ASSUMES LAKE DOES NOT FREEZE TO BOTTOM
C ---* (ADIABATIC BC RECOVERED WHEN TCSED=0)
        TSEDT=( (TCSED*TSED(I)/DSED)+(TCW*TLAK(I,NLEV)/DELZLK) )/
     >        ( (TCSED/DSED)+(TCW/DELZLK) )
        FFLX(I,NLEV)= (-2.0*TCW/DELZLK)*(TSEDT-TLAK(I,NLEV))
CCC     FFLX(I,NLEV)=0.0	!adiabatic lower B.C.
300   CONTINUE
C
C-----------------------------------------------------------------------
C ---* COMPUTE TEMPERATURE PROFILE (TLAK) BEFORE TURBULENT MIXING 
C    * FOR NEXT TIMESTEP.  
C    * HEAT CAPACITY OF LAYER IS WEIGHTED AVERAGE OF WATER AND ICE IF
C    * NECESSARY
C
      DO 400 I=IL1,IL2
        ICEBOT=RHOIW*LKICEH(I)
        ICETOP=LKICEH(I)-ICEBOT
        ICEBOT0=RHOIW*LKICEH0(I)
        ICETOP0=LKICEH0(I)-ICEBOT
C
C --COLUMN  --- TEMP BEFORE MELT/FREEZE
        DO 310, J=1,NLAK(I)
          TLAK0(J)=TLAK(I,J)
          ZTOP=DELSKIN + DELZLK*(J -1)
          ZBOT=DELSKIN + DELZLK*J
          IF (ICEBOT .GE. ZBOT) THEN 
            HCAP=HCPICE
          ELSE IF (ICEBOT .LE. ZTOP) THEN
            HCAP=HCPW
          ELSE 
            Z=ICEBOT-ZTOP
            HCAP=(Z*HCPICE + (DELZLK-Z)*HCPW)/DELZLK
          ENDIF
          IF (J .EQ. 1) THEN
            EFLUX=F0(I)-FFLX(I,1)+Q0(I)-QFLX(I,1)
          ELSE
            EFLUX=FFLX(I,J-1)-FFLX(I,J)+QFLX(I,J-1)-QFLX(I,J)
          ENDIF
          TLAK(I,J)=TLAK(I,J) + (DELT/(DELZLK*HCAP))*EFLUX
310     CONTINUE
C --UPDATE SEDIMENT TEMPERATURE
        NLEV=NLAK(I)
        FFLXSED=(-2.0*TCSED/DSED)*(TSEDB-TSED(I))
        TSED(I)=TSED(I) + (DELT/(DSED*HCPSED))*
     >          (FFLX(I,NLEV)-FFLXSED+QSED*QFLX(I,NLEV))
C  
C   ICE GROWTH OR DECAY
C 
        DO 320, J=1,NLAK(I)
          ZTOP=DELSKIN + DELZLK*(J -1)
          ZBOT=DELSKIN + DELZLK*J
          IF (J .EQ. 1) THEN
            EFLUX=F0(I)-FFLX(I,1)+Q0(I)-QFLX(I,1)
          ELSE
            EFLUX=FFLX(I,J-1)-FFLX(I,J)+QFLX(I,J-1)-QFLX(I,J)
          ENDIF
C
C --- FREEZING --------------------
C
          IF (EFLUX .LT. 0.0 .AND. ICEBOT0 .LT. ZBOT .AND.
     >                             ICEBOT0 .GE. ZTOP) THEN
C ----Net energy flux used to lower T to TFREZ
            IF (TLAK0(J) .GT. TFREZ) THEN
              ECOOL=(ZBOT-ICEBOT0)*HCPW*(TLAK0(J)-TFREZ)/DELT
            ELSE
              ECOOL=0.0
            ENDIF
C ----Remaining energy flux (if any) used to freeze ice
            EAVAIL=EFLUX+ECOOL
            IF (EAVAIL .LT. 0.0) THEN
              NEWICE=-(DELT/(RHOICE*CLHMLT))*EAVAIL
              LKICEH(I)=LKICEH(I)+NEWICE
              ICEBOT=RHOIW*LKICEH(I)
              TLAK(I,J)=TFREZ
C-----LIMIT ICE GROWTH TO THE CURRENT LAYER
              IF (ICEBOT .GT. ZBOT) THEN
                EHEAT=(RHOICE*CLHMLT*(ICEBOT-ZBOT))/DELT
                TLAK(I,J)=TLAK(I,J) - (EHEAT*DELT)/(DELZLK*HCPICE)
                LKICEH(I)=ZBOT/RHOIW
              ENDIF
            ENDIF
          ENDIF
C
C --- MELTING --------------------
C
          IF (EFLUX .GT. 0.0 .AND. ICEBOT0 .GT. ZTOP) THEN
C ----Net energy flux used to raise T to TFREZ
            ZICE=MIN(DELZLK, ICEBOT0-ZTOP)
            EHEAT=ZICE*HCPICE*(TFREZ-TLAK0(J))/DELT
C ----Remaining energy flux (if any) used to melt ice
            EAVAIL=EFLUX-EHEAT
            IF (EAVAIL .GT. 0.0) THEN
              NEWICE=-(DELT/(RHOICE*CLHMLT))*EAVAIL
              LKICEH(I)=LKICEH(I)+NEWICE
              ICEBOT=RHOIW*LKICEH(I)
              TLAK(I,J)=TFREZ
C-----LIMIT ICE MELT TO THE CURRENT LAYER
              IF (ICEBOT .LT. ZTOP) THEN
                EHEAT=RHOICE*CLHMLT*(ZTOP-ICEBOT)/DELT
                TLAK(I,J)=TFREZ + (EHEAT*DELT)/(DELZLK*HCPW)
                LKICEH(I)=ZTOP/RHOIW
              ENDIF
            ENDIF
          ENDIF
320     CONTINUE
400   CONTINUE

C-----------------------------------------------------------------------
C ---* COMPUTE MIXED LAYER DEPTH (HDPTH), TKE, SHEAR FLOW (DELU)
C    * FOR NEXT TIMESTEP
C
      CALL MIXLYR(DTEMP,Q0,NLAK,USTAR,IL1,IL2,ILG,NLAKMAX,
     1              HDPTH,TKE,DELU,FQU,BFLX,DISS,EXPW,FSGL,
     2              FSHEAR,FENTRA,HLAK,LLAK,GRED,TRAN,
     3              CQ1A,CQ1B,CQ2A,CQ2B,CQ3A,CQ3B,RHOMIX,
     4              FLGL,HFSL,HEVL,LKICEH)
C
C-----------------------------------------------------------------------
C ---* MIX TEMP OVER MIXED LAYER (now mass weighted)
C
      DO 600 I=IL1,IL2
       ICEBOT=RHOIW*LKICEH(I)
C
       Z2=DELZLK+DELSKIN
       ZBOT=DELSKIN+DELZLK*NLAK(I)
       IF (HDPTH(I) .LT. Z2) THEN	!fully stratified: no mixing
         JMIX=1
         ZMIX=DELZLK+DELSKIN
       ELSE IF (HDPTH(I) .GE. ZBOT) THEN	!fully mixed
         JMIX=NLAK(I)
         ZMIX=ZBOT
       ELSE
         DO 510, J=2,NLAK(I)
           Z=DELSKIN + DELZLK*J
           IF (HDPTH(I) .LT. Z) EXIT
510      CONTINUE
         JMIX=J-1
         ZMIX=Z-DELZLK
       ENDIF
C ---------------------------------------------------------------------------
C -- MIXING UNDER ICE ALLOWED (ie temp mixed between bottom of mixed layer
C                              and bottom of ice cover)
C ---------------------------------------------------------------------------
        IF (LKICEH(I) .LE. 0.0) THEN
          TC1=T0(I)-TFREZ
          CALL EQNST(XXX,RHO1,TC1,0.05)
          TBAR=DELSKIN*RHO1*T0(I)
          MASS=DELSKIN*RHO1
        ELSE
          TBAR=0.0
          MASS=0.0
        ENDIF
        DO 520, J=1,JMIX
          ZTOP=DELSKIN + (J-1)*DELZLK
          ZBOT=ZTOP+DELZLK
          IF (ICEBOT .LE. ZTOP) THEN
           TC1=TLAK(I,J)-TFREZ
           CALL EQNST(XXX,RHO1,TC1,ZBOT)
           TBAR=TBAR + RHO1*TLAK(I,J)*DELZLK
           MASS=MASS + RHO1*DELZLK
          ENDIF
520     CONTINUE
        IF ((JMIX .GE. 2) .AND. ((ZMIX-ICEBOT).GE.DELZLK) ) THEN
          TMIX=TBAR/MASS
        ELSE
          TMIX=TLAK(I,1)
        ENDIF  
C-----------------------------------------------------------------------
C MIX TEMPERATURES: include skin layer if no ice 
C-----------------------------------------------------------------------
C--Mix skin with first layer temperature
Cmdm    IF ( (LKICEH(I).LE.0.0) .AND. (JMIX.LT.2) ) THEN
        IF ( (LKICEH(I).LE.0.0) .AND. (JMIX.EQ.1) ) THEN
          TC1=T0(I)-TFREZ
          TC2=TLAK(I,1)-TFREZ
          CALL EQNST(XXX,RHO1,TC1,0.05)
          CALL EQNST(XXX,RHO2,TC2,0.5)
          T0(I) = (RHO1*DELSKIN*T0(I) + RHO2*DELZLK*TLAK(I,1))/
     1             (RHO1*DELSKIN + RHO2*DELZLK)
          TLAK(I,1)=T0(I)
        ELSE IF ( (LKICEH(I).LE.0.0) .AND. (JMIX.GE.2) ) THEN
          T0(I)=TMIX
          DO 525, J=1,JMIX
             TLAK(I,J)=TMIX
525       CONTINUE
C
C--- MIXING UNDER ICE
        ELSE IF ((JMIX .GE. 2) .AND. ((ZMIX-ICEBOT).GE.DELZLK) ) THEN
          print*, "MIXING UNDER ICE"
          DO 530, J=1,JMIX
            ZTOP=DELSKIN + (J-1)*DELZLK
            IF (ICEBOT .LE. ZTOP) THEN
             TLAK(I,J)=TMIX
            ENDIF
530       CONTINUE
        ENDIF
C======================================================================
C ---* COMPUTE TEMPERATURE, DENSITY, EXPANSIVITY OF MIXED LAYER WATER
C      EQN OF STATE FROM FARMER AND CARMACK, 1982
C
       TCMIX(I)=TMIX-TFREZ
       JMIXP1=MIN(NLAK(I),JMIX+1)
       CALL EQNST(EXPW(I),RHOMIX(I),TCMIX(I),HDPTH(I))
       CALL EQNST(XXX,RHO1,TLAK(I,JMIXP1)-TFREZ,HDPTH(I))
       GRED(I) = GRAV*ABS(RHO1-RHOMIX(I))/RHOW
Cmdm   GRED(I) = GRAV*ABS(RHO1-RHOMIX(I))/RHO1
       IF (GRED(I).LT.0.) 		CALL XIT('CLASSL',-2)
C
C-----------------------------------------------------------------------
C ---* MIX TEMPERATURE IN THERMOCLINE LAYER 
C          DISABLED FOR NOW: JAN 2013 (M.MACKAY)
C
C--Compute thermocline layer thickness (DELTHRM)
C--Limit allowed values 
C
      IF (LKICEH(I) .LE. 0.0) THEN 
       IF (GRED(I) .GT. 0.0) THEN
        DELTHRM=0.3*DELU(I)*DELU(I)/GRED(I)
       ELSE
        DELTHRM=0.0
       ENDIF
       IF (DELTHRM.GT.DELMAX) DELTHRM=DELMAX 
       IF (DELTHRM.LT.DELMIN) DELTHRM=DELMIN 

       HTOP=HDPTH(I)-(DELTHRM/2.)
       HBOT=HDPTH(I)+(DELTHRM/2.)
       DO 540, J=1,NLAK(I)
         Z=DELZLK*J
         IF (HTOP .LT. Z) EXIT
540    CONTINUE
       JTOP=J-1
       IF (JTOP.LT.1) JTOP=1
       ZTOP=DELZLK*JTOP
       TTOP=TLAK(I,JTOP)

       DO 550, J=1,NLAK(I)
         Z=DELZLK*J
         IF (HBOT .LT. Z) EXIT
550    CONTINUE
       JBOT=J-1
       IF (JBOT.LT.1) JBOT=1
       ZBOT=DELZLK*JBOT
       TBOT=TLAK(I,JBOT)
       IF (JBOT.LT.JTOP) 		CALL XIT('CLASSL',-3)

C-DISABLED
C-       IF (JBOT.GT.JTOP) THEN
C-        DO 560, J=JTOP,JBOT
C-          Z=DELZLK*J
C-          TLAK(I,J)=TTOP+((Z-ZTOP)*(TBOT-TTOP)/(ZBOT-ZTOP))
C-560     CONTINUE
C-       ENDIF

C--Compute temperature jump beneath mixed layer
C--
       IF (JMIX .LT. NLAK(I)) THEN
          DTEMP(I)=TMIX-TLAK(I,JBOT) 	!see Spigel et al 1986
       ELSE 
          DTEMP(I)=0.
       ENDIF
      ELSE 
        DTEMP(I)=0.
        DELTHRM=0.
      ENDIF
600   CONTINUE
C------------------------
C--Compute Wedderburn number diagnostic
C--
      DO 605 I=IL1,IL2
       IF  (LKICEH(I) .LE. 0.0 .AND. USTAR(I) .GT. 0.0 ) THEN
         WEDB=GRED(I)*HDPTH(I)*HDPTH(I)/(LLAK(I)*USTAR(I)*USTAR(I))
       ELSE
         WEDB=-999	!because USTAR=0
       ENDIF
C--Compute heat of melting/freezing for energy balance diagnostic
C--
       HMFL(I) = (LKICEH(I)-LKICEH0(I))*RHOICE*CLHMLT/DELT
C
       OSW(I) = QSWIN(I) - FLS(I)*(QSWNS(I)-QTRANSL(I)) - FSGL(I)
C-----------------------------------------------------------------------
C ---* WRITE OUTPUT FOR THIS TIMESTEP
C
!        WRITE(71,6010) IYEAR,IDAY,IHOUR,IMIN,G0(I),F0(I),FSGL(I),
!     1   Q0(I),FLGL(I),HFSL(I),HEVL(I),T0(I),LKICEH(I),SNO(I),
C    2   RHOSNI(I)*S(I),RHOW*R(I),OSW(I),SNO(I)/RHOSNO(I),SNICEH(I)
C    2   RHOSNI(I)*S(I),RHOW*R(I),OSW(I),ZSNOW(I),ROFICEH(I)
!     2   RHOSNI(I)*S(I),RHOW*R(I),OSW(I),ZSNOW(I),SNICEH(I)
!        WRITE(72,6020) IYEAR,IDAY,IHOUR,IMIN,
!     1                 (TLAK(I,J)-TFREZ,J=1,NLAK(I))
!        WRITE(73,6030) IYEAR,IDAY,IHOUR,IMIN,FQU(I),BFLX(I),DISS(I),
!     1                 TKE(I),DELU(I),HDPTH(I),JMIX,ZMIX,TMIX,DTEMP(I),
!     2                 FSHEAR(I),FENTRA(I)
Cmdm  WRITE(74,6040) IYEAR,IDAY,IHOUR,IMIN,USTAR(I),GRED(I),WEDB,
!      WRITE(74,6040) IYEAR,IDAY,IHOUR,IMIN,USTAR(I),EXPW(I),WEDB,
!     1                 HDPTH(I),DELTHRM,DELU(I),
!     2                 ZREFM(I),VA(I),CDML(I)
!      WRITE(75,6050) IYEAR,IDAY,IHOUR,IMIN,TSNOW(I)-TFREZ,WSNOW(I),
!     1               RHOSNO(I), ZSNOW(I),FLS(I)
!      WRITE(76,6060) IYEAR,IDAY,IHOUR,IMIN,QSWNS(I),QTRANSL(I),
!     1               QLWIN(I)-QLWOS(I),QSENSS(I),QEVAPS(I),
!     2               QMELTS(I),GZEROSL(I),CFLUXS(I)
!        NLEV=NLAK(I)
!      WRITE(77,6070) IYEAR,IDAY,IHOUR,IMIN,QSWIN(I),FSGL(I),QFLX(I,2),
!     1               FFLX(I,NLEV),TSED(I)-TFREZ,TLAK(I,NLEV)-TFREZ
605   CONTINUE

6010  FORMAT(I4,1X,3(I3,1X),8F8.2,2F7.3,5E10.3)
6020  FORMAT(I4,1X,3(I3,1X),100F7.2)
6030  FORMAT(I4,1X,3(I3,1X),4E10.2,F5.2,F6.2,I3,3F7.2,2E10.2)
6040  FORMAT(I4,1X,3(I3,1X),3E10.3,3F7.2,F8.3,F8.3,E10.3)
6050  FORMAT(I4,1X,3(I3,1X),5E10.3)
6060  FORMAT(I4,1X,3(I3,1X),8E10.3)
6070  FORMAT(I4,1X,3(I3,1X),6F7.1) 
C                                                                       
C     * SCREEN LEVEL DIAGNOSTICS.                                                    
C                                                                       
      DO I=IL1,IL2
          CDM(I)=FLS(I)*CDMS(I)+(1.0-FLS(I))*CDML(I)
          CDH(I)=FLS(I)*CDHS(I)+(1.0-FLS(I))*CDHL(I)
          DRAG(I)=FLS(I)*DRAGS(I)+(1.0-FLS(I))*DRAGL(I)
          ZOM(I)=ZREFM(I)/EXP(VKC/SQRT(DRAG(I)))
          ZOH(I)=ZOM(I)/3.0
          TSURF(I)=FLS(I)*TZEROS(I)+(1.0-FLS(I))*T0(I)
          QSURF(I)=FLS(I)*QZEROS(I)+(1.0-FLS(I))*QSURFL(I)
          ALVS(I)=FLS(I)*ALVSSN(I)+(1.0-FLS(I))*ALVSL(I)
          ALIR(I)=FLS(I)*ALIRSN(I)+(1.0-FLS(I))*ALIRL(I)
      ENDDO
C
      IF(ISLFD.EQ.0)                                         THEN   
          DO I=IL1,IL2                                            
              FACTM=ZDIAGM(I)+ZOM(I)  
              FACTH=ZDIAGH(I)+ZOM(I)   
              RATIOM=SQRT(CDM(I))*LOG(FACTM/ZOM(I))/VKC              
              RATIOM=MIN(RATIOM,1.)                                   
              RATIOH=SQRT(CDM(I))*LOG(FACTH/ZOH(I))/VKC              
              RATIOH=MIN(RATIOH,1.)                                   
              IF(TSURF(I).GT.TA(I))                THEN
                  RATIOH=RATIOH*CDH(I)/CDM(I)                         
                  RATIOH=MIN(RATIOH,(FACTH/ZREFH(I))**(1./3.))         
              ENDIF                                                   
              ST(I)=TSURF(I)-(MIN(RATIOH,1.))*(TSURF(I)-TA(I))       
              SQ(I)=QSURF(I)-(MIN(RATIOH,1.))*(QSURF(I)-QA(I))       
              SU(I)=RATIOM*UWIND(I)                                  
              SV(I)=RATIOM*VWIND(I)                                  
          ENDDO
C                                                                       
!          CALL SCREENRH(SH,ST,SQ,PRES,FTOT,ILG,IL1,IL2)
          CALL SCREENRH(SH,ST,SQ,PRES,FLS,ILG,IL1,IL2)
C                                                                       
      ELSEIF(ISLFD.EQ.1)                                        THEN
!          CALL SLDIAG(SU,SV,ST,SQ,
!     1                CDM,CDH,UWIND,VWIND,TA,QA,
!     2                TSURF,QSURF,ZOM,ZOH,FTOT,ZREFM,
!     3                ZDIAGM,ZDIAGH,ILG,IL1,IL2,JL)
C                                                                       
!          CALL SCREENRH(SH,ST,SQ,PRES,FTOT,ILG,IL1,IL2)
          CALL SLDIAG(SU,SV,ST,SQ,
     1                CDMS,CDHS,UWIND,VWIND,TPOTA,QA,
     2                TZEROS,QZEROS,ZOM,ZOH,FLS,ZREFM,
     3                ZDIAGM,ZDIAGH,ILG,IL1,IL2,JL)
C                                                                       
      ELSEIF(ISLFD.EQ.2)                                        THEN
!          CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TSURF,QSURF,
!     1                  ZOM,ZOH,ILMO,ZREFM,HBL,UE,FTEMP,FVAP,
!     2                  ZDIAGM,ZDIAGH,RADJ,FTOT,IL1,IL2,JL)
          CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TZEROS,QZEROS,
     1                  ZOM,ZOH,ILMOS,ZRSLFM,HS,UES,FTEMPS,FVAPS,
     2                  ZDIAGM,ZDIAGH,RADJ,FLS,IL1,IL2,JL)
      ENDIF                                                         
C                                                                       
      DO I=IL1,IL2                                              
          FSGS(I) =FLS(I)*(QSWNS(I)-QTRANSL(I))           
          FLGS(I) =FLS(I)*(QLWIN(I)-QLWOS(I))            
          HFSS(I) =FLS(I)*QSENSS(I)                     
          HEVS(I) =FLS(I)*QEVAPS(I)                     
          HTCS(I) =HTCS(I)-(FLS(I)*GZEROSL(I))
          HTCL(I) =HTCL(I)+(FLS(I)*GZEROSL(I))
          QLWAVG(I)=FLS(I)*QLWOS(I)+(1.0-FLS(I))*QLWIN(I)-FLGL(I)
          QSENS(I)=HFSL(I)+HFSS(I)
          TFLUX(I)=-QSENS(I)/(RHOAIR(I)*SPHAIR)
          QEVAP(I)=HEVL(I)+HEVS(I)
          EVAP(I)=QFL(I)+QFN(I)
          QFLUX(I)=-EVAP(I)/RHOAIR(I)
          EVPPOT(I)=EVAP(I)
          EVAPB(I)=1.0
          GT(I)=(QLWAVG(I)/SBC)**0.25
      ENDDO
C                                                                       
      RETURN   
      END        
