C
      SUBROUTINE CLASSN(IDAY,     KGROWS,     KGROWE,     DELT,         
     1     ILG,   IC,     ICC,    IG,         IL1,        IL2,          
     2     FCAN,  FGT,    ROOTDPTH, DELZ,     ZBOT,       CALSOIL,      
     3     AILCG,  TAIR,   TCAN,   TBAR,       THLIQ,      THPOR,       
     4     ETR,   RUNOFF, DRAIN,  BTDPTH,     XMINF,      BI,           
C -- PHYSICAL DEFINITION OF VEGETATION & ITS ENVIRONMENT (ABOVE)
     5     CLEAF,     CSTEM,      CROOT,      CLITR,      CSOM,         
     6     NPPLF,     NPPSM,      NPPRT,      RSPLTR,     RSPSOM,       
     7     LTRLF,     LTRSM,      LTRRT,      CLSLF,      CLSSM,        
     8     CLSRT,     CLSLTR,     CLSSOM,     CLTRSOM,                   
C -- C STOCKS & CHANGES (RATES) (ABOVE)
     9     RNLEAF,    RNSTEM,     RNROOT,     RNLITR,     RNSOM,        
     A     SNH4,      SNO3,       NRUB,       NRUB0,                    
C -- N RATIOS & CONTENTS IN C STOCKS OR SOIL BULK
     B     DNBFIX,    DNDEP,      DNFERI,     DNFERO,     DNPLTR,       
     C     DNPLOSS,   DNSLOSS,    DNLTRSOM,   DNMIN,      DNNIT,         
     D     DNPUP,     DNDNIT,     DNDRAIN,    DNVOL,                  
     E     DNSOURCE,  DNLOSS,      
C -- N BUDGTS
     F     N2OTOT,N2TOT)
C 
C       Called from CTEM
C       
C    -------------------------------------------------------------------------
C
C    7/15/2006 - NITROGEN COMPONENTS, REBUILT EXACTLY FOR CANADIAN TERRESTRIAL 
C                ECOSYSTEM MODEL (CTEM) VERSION 1.0 AS A PORTION OF CGC3M PROJECT
C                BY FENGMING YUAN, McMASTER UNIVERSITY 
C
C         * UNITS: ALL C STOCKS - KgC/m2;
C                  ALL N CONTNET - gN/m2 (GROUND-BASED), EXCEPT FOR NRUB0 (LAI-BASED);
C                  ALL C CHANGES (FLUXES) - gC/m2/sec
C                  ALL N CHANGES (FLUXES) - gN/m2/sec
C    -------------------------------------------------------------------------
      implicit none
C
C         * INPUTS VARIABLES/PARAMETERS
C
C -- PHYSICAL DEFINITION OF VEGETATION & ITS ENVIRONMENT
C
C         IDAY        - MODEL RUNNING DAY [DOY]
C         KGROWS      - STARTING DAY OF PLANT GROWTH PERIOD [DOY]
C         KGROWE      - ENDING DAY OF PLANT GROWTH PERIOD [DOY]
C         DELT        - MODEL RUNNING TIMESTEP FOR THIS MODULE [SEC]
C         ILG         - NO. OF GRID CELLS IN LATITUDE CIRCLE
C         IC          - NO. OF PFTs FOR USE IN CLASS (4)
C         ICC         - NO. OF PFTs FOR USE IN CTEM (9)
C         IG          - NO. OF SOIL LAYERS (3)
C         IL1, IL2    - IL1=1, IL2 = ILG
C         FCAN        - FRACIONAL COVERAGE OF CTEM''S 9 PFTS OVER THE GIVEN SUB-AREA
C         FGT         - FRACTION OF BARE SOIL
C         ROOTDPTH    - MAX. ROOT DEPTH OF EACH PFT [M]
C         DELZ        - DEPTHS OF 3 SOIL LAYERS [M]
C         ZBOT        - BOTTOM DEPTHS OF 3 SOIL LAYERS [M]
C         CALSOIL     - .TRUE. IF CALCIOUS SOIL TYPE (PH>7.0)
C         AILCG        - LAI OF TOTAL LEAF, 9PFTs  
C         AILCB        - LAI OF BROWN LEAF, 9PFTs  
C         TAIR        - AIR TEMPERATURE [K]
C         TCAN        - CANOPY TEMPERATURE [K]
C         TBAR        - SOIL TEMPERATURE [K]
C         THLIQ       - SOIL MOISTURE [M3/M3]
C         THPOR       - MAX. SOIL POROSITY [M3/M3]
C         ETR         - EVAPTRANSPORATION OF EACH PFTS IN CTEM AND BARE SOIL [MM/SEC]
C         RUNOFF      - GROUND SURFACE RUNOFF [MM/SEC]
C         DRAIN       - BOTTOM/LATERAL DRAINAGE [MM/SEC]
C             
C -- C STOCKS & CHANGES (RATES)
C
C         CLEAF       - TOTAL LEAF BIOMASS C [kgC/m2] 
C         CSTEM       - TOTAL STEM BIOMASS C [kgC/m2] 
C         CROOT       - TOTAL ROOT BIOMASS C [kgC/m2] 
C         CLITR       - TOTAL LITTER C [kgC/m2] 
C         CSOM        - TOTAL SOIL ORGANIC MATTER C [kgC/m2] 
C
C         NPPLF       - LEAF ALLOCATION OF NPP [gC/m2/sec]
C         NPPSM       - STEM ALLOCATION OF NPP [gC/m2/sec]
C         NPPRT       - ROOT ALLOCATION OF NPP [gC/m2/sec]
C         RSPLTR      - LITTER RESPIRATION RATE [gC/m2/sec]
C         RSPSOM      - SOM RESPIRATION RATE [gC/m2/sec]
C         LTRLF       - LEAF LITTER-FALLING RATE [gC/m2/sec]
C         LTRSM       - STEM LITTER-FALLING (TURNOVER) RATE [gC/m2/sec]
C         LTRRT       - ROOT LITTER-FALLING (TURNOVER) RATE [gC/m2/sec]
C         CLSLF       - LEAF DISTURBANCE LOSS (NON-LITTERING) RATE [gC/m2/sec]
C         CLSSM       - STEM DISTURBANCE LOSS (NON-LITTERING) RATE [gC/m2/sec]
C         CLSRT       - ROOT DISTURBANCE LOSS (NON-LITTERING) RATE [gC/m2/sec]
C         CLSLTR      - LITTER DISTURBANCE LOSS RATE [gC/m2/sec]
C         CLSSOM      - SOM DISTURBANCE LOSS RATE [gC/m2/sec]
C
C         * INPUTS/OUTPUTS VARIABLES/PARAMETERS
C
C -- N RATIOS & CONTENTS IN C STOCKS OR SOIL BULK
C
C         RNLEAF       - TOTAL LEAF BIOMASS N/C RATIO [-] 
C         RNSTEM       - TOTAL STEM BIOMASS N/C RATIO [-] 
C         RNROOT       - TOTAL ROOT BIOMASS N/C RATIO [-] 
C         RNLITR       - TOTAL LITTER N/C RATIO [-] 
C         RNSOM        - TOTAL SOIL ORGANIC MATTER N/C RATIO [-] 
C
C         SNH4         - EXTRACTABLE SOIL INORGANIC NH4 CONTENT [gN/m2]
C         SNO3         - SOIL INORGANIC NO3 CONTENT [gN/m2]
C         NRUB         - LEAF RUBISCO-RELATED N CONTENT [gC/m2 COVERED SURFACE AREA]
C         NRUB0        - LEAF RUBISCO-RELATED N CONTENT AT TOP CANOPY [gC/m2 LEAF AREA]
C 
C         * OUTPUTS VARIABLES/PARAMETERS
C
C -- N BUDGTS
C         DNBFIX       - N BIO-FIXATION RATE [gN/m2/sec]
C         DNDEP        - N ATMOSPHERIC DEPOSITION RATE [gN/m2/sec]
C         DNFERI       - N INORGANIC N FERTILIZATION RATE [gN/m2/sec]
C         DNFERO       - N ORGANIC N FERTILIZATION RATE [gN/m2/sec]
C
C         DNPLTR       - PLANT N LOSS RATE TO LITTER BY LITTERING [gN/m2/sec]
C         DNPLOSS      - PLANT N LOSS RATE BY NON-LITTERING (DISTURBANCE) [gN/m2/sec]
C         DNSLOSS      - LITTER/SOM N LOSS RATE BY DISTURBANCE [gN/m2/sec]
C
C         DNPUP        - PLANT N UPTAKE RATE [gN/m2/sec]
C         DNLTRSOM     - N TRANSFORMATION RATE FROM LITTER TO SOM [gN/m2/sec]-->decomposition
C         DNMIN        - SOIL N MINERALIZATION RATE [gN/m2/sec]
C         DNNIT        - SOIL NH4 NITRIFICATION RATE [gN/m2/sec]
C         DNDNIT       - SOIL NO3 DENITRIFICATION RATE [gN/m2/sec]
C         DNVOL        - SOIL NH4 VOLTALIZATION RATE [gN/m2/sec]
C         DNSOURCE     - TOTAL SOIL-PLANT N SOURCE RATE [gN/m2/sec]
C         DNLOSS       - TOTAL SOIL-PLANT N LOSS RATE [gN/m2/sec]
C
C -- N20 BY HSUO 2009-FEB-05
C         FLXdNO3      - (g N ha-1 d-1)  the maximum total N gas flux for a given soil
C                                        NO3 level (assuming high respiration rates)
C         FLXdCO2      - (g N ha-1 d-1)  the maximum total N gas flux for a given soil
C                                        respiration level (assuming high NO3 rates)
C         FLXdWFPS     - (fraction) the effect of WFPS on the denitrification rate
C
C
C         * SOIL-PLANT N CYCLYING PARAMETERS
C
C -- root- and soil-dependent uptake process
c         et0      !characteristic ET for N uptake [mm/s], ~ 17 mm/day
c         jmax     !High affinity maximum rate of ion uptake (nondimensional)
c         kl       !low affinity root ion uptake
c         km       !Mihalis Menten factor for 50% of maximum ion uptake rate
c         kmin0    !upper limit to kmin for adequate roots and ET
c         rtmass0  !reference root mass - now a constant
c         solNH4   !soluability of ammonium ion
c         bi       !clapp-hornburger coeficient
c         xminf    !factor to reduce soil ion uptake FOR LIGHT-LIMITED CANOPY
C -- parameterized plant/soil N/C ratios
c         npc1     !min. N to C ratio in plant labile reservoir
c         npc2     !max. N to C ratio in plant labile reservoir
c         rnlf0    !ideal structual N to C ratio for leaves in canopy
c         rnsm0    !ideal N to C ratio in living stem tissue
c         rnrt0    !ideal N to C ratio in living fine roots
c         conreal  !reallocation of nitrogen coefficient
C---- leaf Rubisco-N distribution along canopy depth
c         LAI0     !canopy top LAI with same Rubisco-N content
c         krubn    !canopy Rubisco-nitrogen decay coefficient
c         kn       !canopy nitrogen decay coefficient
C---- N loss rates from soil
c         kni0     !max. NH4 nitrafiction rate (1/s)
c         kdn0     !max. NO3 denitrafiction rate (1/s)
c         kv0      !max. NH4 volitization rate (1/s)
C---- N sources
c         nbfix0   !N source by biofixation at ref. conditions (gN/m2/s)
c         ndep0    !N source by deposition (gN/m2/yr)
c         nfer0    !N source by inorganic fertilization (gN/m2/yr)
c         nfero0   !N source by organic fertilization (gN/m2/yr)
C
      INTEGER IDAY,        DELT,     ILG,       IC,                      
     1        ICC,        IG,     IL1,    IL2
C     
      INTEGER KGROWS(ILG, ICC),     KGROWE(ILG,ICC)
C
      LOGICAL CALSOIL (ILG)
C
      REAL DELZ(ILG,IG),  ZBOT(ILG,IG)

      REAL FCAN(ILG, ICC),    FGT(ILG),           ROOTDPTH(ILG, ICC),               
     1     AILCG(ILG, ICC)  
C
      REAL TAIR(ILG),         TCAN(ILG)   
      REAL TBAR(ILG, IG),    THLIQ(ILG, IG),    THPOR(ILG, IG)      
      REAL ETR(ILG, ICC+1),   RUNOFF(ILG, ICC+1), DRAIN(ILG, ICC+1)
	REAL BTDPTH(ILG)
      REAL XMINF(ILG,ICC),    BI(ILG,IG)
C
      REAL CLEAF(ILG, ICC),   CSTEM(ILG, ICC),    CROOT(ILG, ICC)   
      REAL CLITR(ILG, ICC+1), CSOM(ILG, ICC+1)                 
C
      REAL NPPLF(ILG, ICC),   NPPSM(ILG, ICC),    NPPRT(ILG, ICC)
      REAL RSPLTR(ILG, ICC+1), RSPSOM(ILG, ICC+1)
      REAL LTRLF(ILG, ICC),   LTRSM(ILG, ICC),    LTRRT(ILG, ICC), 
     1     CLSLF(ILG, ICC),   CLSSM(ILG, ICC),    CLSRT(ILG, ICC)
      REAL CLSLTR(ILG,ICC+1), CLSSOM(ILG, ICC+1)
      REAL CLTRSOM(ILG,ICC+1)
C
C    * INPUTS/OUTPUTS
C
      REAL RNLEAF(ILG, ICC),  RNSTEM(ILG, ICC),   RNROOT(ILG, ICC)
      REAL RNLITR(ILG, ICC+1),RNSOM(ILG, ICC+1) 
      REAL SNH4(ILG, ICC+1),  SNO3(ILG, ICC+1)
      REAL NRUB(ILG, ICC),    NRUB0(ILG, ICC)
C
C     * OUTPUTS
C
      REAL DNBFIX(ILG,ICC),   DNPLTR(ILG, ICC),   DNPLOSS(ILG, ICC)
      REAL DNSLOSS(ILG,ICC+1),DNDEP(ILG, ICC+1),  DNFERI(ILG, ICC+1), 
     1     DNFERO(ILG, ICC+1),DNLTRSOM(ILG,ICC+1),DNMIN(ILG, ICC+1),  
     2     DNNIT(ILG, ICC+1), DNPUP(ILG,ICC+1),   DNDNIT(ILG, ICC+1),  
     3     DNRUNOF(ILG,ICC+1),DNDRAIN(ILG,ICC+1), DNVOL(ILG, ICC+1),
     4     DNSOURCE(ILG,ICC+1),DNLOSS(ILG, ICC+1)
C 
C     * SOIL-PLANT N CYCLING RELEVANT PARAMETERS 
C
      REAL ET0, JMAX, KL, KM, KMIN0, RTMASS0, SOLNH4
      REAL RNPL1, RNPL2, RNLF0, RNSM0, RNRT0
      REAL CONREAL
C
      REAL LAI0, KRUBN, KN
C
      REAL KNI0, KDN0, KV0
C
      REAL NBFIX0, NDEP0, NFER0, NFERO0
C
C    * LOCAL VARIABLES
C
      INTEGER I, J, KK
C
C     * C STOCK COMPONENTS NOT EXPLICITLY INPUT
      REAL CLFG
C     * C STOCKS IN UNIT OF gC/m2
      REAL CLEAFG,    CSTEMG,     CROOTG,    CLITRG,     CSOMG
C     * C STOCKS AT PREVIOUS TIMESTEP
      REAL CLEAFP,    CSTEMP,     CROOTP,    CLITRP,     CSOMP 
C     * N CONTENT IN UNIT OF gN/m2 (FIRST CALCULATED AND THEN UPDATED)
      REAL NLEAF,     NSTEMT,     NSTEM,     NROOTT,     NROOT,
     1     NLITR,     NSOM
C
      REAL TFREZ,     TLEF,       TSRZ,       FTSOIL       
      REAL WATRZ,     WSTORAGE
      REAL FCANSOIL(ILG,ICC+1)
C
      REAL RTDP,      FRSTMLIV,   FRROTLIV,   RTMASSX
      REAL RNDEAD,    RNSTMLIV,   RNROTLIV
C
      REAL LAMR,      LAMR3,      LAMR4,      LAMET,      LAMD,       
     1     LAMT,      KMIN3,      KMIN4,      BSW
C
      REAL NLGROW,    NSGROW,     NRGROW,     NLDPT,      NSDPT,
     1     NRDPT,     NDEMAND
      REAL NREALLO,   NTOTAL
	REAL FNRUB,     TX,         TN,         TOPT,       FSEAS
	REAL VMXR
C
      REAL NIMMOB
	REAL FNIT,      KNIT,       KDRAIN,     FDNIT,      KDNIT
	REAL GROWDAYS,  FNH4UP(ILG,ICC+1)
	REAL DNPUPALL,  DNPLTRALL,  DNBFIXALL
C 
      REAL N2OTOT(ILG,ICC+1),
     1     N2TOT(ILG,ICC+1)
C 
C 
C
      PARAMETER(KK=12) 
      PARAMETER(TFREZ = 273.16)
C
C     * RATIO OF N/C IN DEAD TISSUE OF TREES
      DATA RNDEAD /0.0001/
C     * SOIL-PLANT N CYCLING PARAMETERS
      COMMON /NITROGEN/ ET0,  JMAX,   KL,     KM,     KMIN0,  RTMASS0,      
     1                SOLNH4, RNLF0,  RNSM0,  RNRT0,  CONREAL,             
     2                LAI0,   KRUBN,  KN,     KNI0,   KDN0,   KV0,        
     3                NBFIX0, NDEP0,  NFER0,  NFERO0

C        SOCRES(I)

       REAL SOILPH(ILG,IG)
       REAL NN2OMAX, NKMAX, SCO2mx
       REAL SOILT !soil temperature
       REAL BulkD ! Bulk Density = (1-porosity)*2.65 (g/cm3,Linn&Doran,1984)
       REAL NSNH4(ILG,ICC+1), NSNO3(ILG,ICC)! unit convert (gN/m2-->ug N/g soil)
       REAL WFPS !Volumetric soil water content*100/porosity THLIQ (ILG-grid,IG-soil layers)
       REAL NWFPS 
       REAL NPH 
       REAL NSOILT
       REAL NNH4(ILG,ICC+1)
       REAL NN2O(ILG,ICC+1)
       
       REAL DCO2
       REAL FLXdNO3 
       REAL FLXdCO2 
       REAL FLXdWFPS 
       REAL FrWFPS 
       REAL FrNO3 
       REAL FrCO2  
       REAL RN2N2O 
       REAL DN2ON2(ILG,ICC+1) 
       REAL DN2O(ILG,ICC+1)  
       REAL DN2(ILG,ICC+1)  
	REAL PI
      DATA PI /3.1415926535898/
	REAL test1, test2, test3, test4
	
	 REAL Na, Nb, Nc, Nd, Da, Db, Dc, Dd
	 !*************parameters from (Parton,1992)*****************
       Na= 0.55   !0.55   SANDY !0.60   !MEDIUM  !this better read from the INI
       Nb= 1.70   !1.70   SANDY !1.27   !MEDIUM
       Nc= -0.007 !-0.007 SANDY !0.0012 !MEDIUM
       Nd= 3.22   !3.22   SANDY !2.84   !MEDIUM
       
       Da= 4.82   !4.82  SANDY !4.82  !MEDIUM !60.0 FINE !this better read from the INI
       Db= 14.0   !14.0  SANDY !14.0  !MEDIUM !18.0 FINE
       Dc= 16.0   !16.0  SANDY !16.0  !MEDIUM !22.0 FINE
       Dd= 1.39   !1.39  SANDY !1.39  !MEDIUM !1.06 FINE
	 !*************parameters from (Parton,1992)*****************
      
C        PI = 3.1415926535898

       SOILPH(1,1) = 4.1 !get from Matthias'(2006), better be predefined in the INI file
       SCO2mx= 80 !(kg C ha-1 d-1)
       NN2OMAX=30.0       !The maximum nitrification N2O (30.0 gN ha-1 d-1)with excess soil NH4
       NKMAX= 20    !N turnover coefficient, proportional to the soil N turnover rate (which is a function of texture, N fertility, managment), number get from (table1.Parton,1996)

!==================N2O by HSuo, 2009-Feb-05========================================
C 
C  
C
C      *  INITIALIZATION
C
      DO 100 I= IL1, IL2
c
          DO 110 J=1, ICC
              FCANSOIL(I,J) = FCAN(I,J)
              DNBFIX(I, J)  = 0.0
              DNPLTR(I, J)  = 0.0
              DNPLOSS(I, J) = 0.0
              DNPUP(I, J)   = 0.0
110       CONTINUE

          FCANSOIL(I,ICC+1) = FGT(I)

100   CONTINUE
C 
C 
      DO 120 J=1, ICC+1
          DO 130 I= IL1, IL2
C
              DNSLOSS(I,J)   = 0.0
              DNDEP(I, J)    = 0.0
              DNFERI(I, J)   = 0.0
              DNFERO(I, J)   = 0.0
              DNLTRSOM(I, J) = 0.0
              DNMIN(I, J)    = 0.0
              DNNIT(I, J)    = 0.0 
              DNRUNOF(I, J)  = 0.0 
              DNDRAIN(I, J)  = 0.0 
              DNDNIT(I, J)   = 0.0
              DNVOL(I, J)    = 0.0 
              DNSOURCE(I, J) = 0.0
              DNLOSS(I, J)   = 0.0
              N2OTOT(I,J)    = 0.0
              N2TOT(I,J)     = 0.0
              FNH4UP(I,J)    = 0.0
130       CONTINUE
120   CONTINUE
C
      DO 140 J = 1, ICC
          DO 150 I = IL1, IL2
            IF (FCAN(I,J) .GT. 0.0) THEN
C
C      * MOISTURE/TEMPERATURE VARIABLES
C
              TLEF = TCAN(I)  
        IF (ROOTDPTH(I,J).LE.DELZ(I,1)) THEN
                  WATRZ = THLIQ(I,1)/THPOR(I,1)
                  TSRZ = TBAR(I,1)
              ELSEIF (ROOTDPTH(I,J).LE.(DELZ(I,1)+DELZ(I,2))) THEN
                  RTDP = ROOTDPTH(I,J) - DELZ(I,1)
                  WATRZ = (THLIQ(I,1)*DELZ(I,1)+THLIQ(I,2)*RTDP)
     1                     /(THPOR(I,1)*DELZ(I,1)+THPOR(I,2)*RTDP)
                  TSRZ = (TBAR(I,1)*DELZ(I,1)+TBAR(I,2)*RTDP)          
     1                     /ROOTDPTH(I,J)
              ELSE
                  RTDP = ROOTDPTH(I,J) - DELZ(I,1) - DELZ(I,2)
                  WATRZ = (THLIQ(I,1)*DELZ(I,1)+THLIQ(I,2)*DELZ(I,2)   
     1                     +THLIQ(I,3)*RTDP)/(THPOR(I,1)*DELZ(I,1)     
     2                     +THPOR(I,2)*DELZ(I,2)+THPOR(I,3)*RTDP)
                  TSRZ = (TBAR(I,1)*DELZ(I,1)+TBAR(I,2)*DELZ(I,2)
     1                     +TBAR(I,3)*RTDP)/ROOTDPTH(I,J)
              ENDIF
C
C     * C STOCK UNIT: kgC/m2 --> gC/m2, FOR CALCULATION CONVINIENCE
C
              CLEAFG  = CLEAF(I,J) *1000.0
              CSTEMG  = CSTEM(I,J) *1000.0
              CROOTG  = CROOT(I,J) *1000.0
C
C     * C STOCKS AT PREVIOUS TIME STEP, NEEDED FOR CALCULATING PREVIOUS N CONTENTS  
C
              CLEAFP  = CLEAFG + (LTRLF(I,J)+CLSLF(I,J))*DELT 
     1                         - NPPLF(I,J)*DELT
              CSTEMP  = CSTEMG + (LTRSM(I,J)+CLSSM(I,J))*DELT 
     1                         - NPPSM(I,J)*DELT
              CROOTP  = CROOTG + (LTRRT(I,J)+CLSRT(I,J))*DELT 
     1                         - NPPRT(I,J)*DELT
C
C     * FRACTION OF LIVING STEM AND ROOT FOR TREES
C
              IF (J .LE. 5) THEN
                  FRSTMLIV = EXP(-0.2835*CSTEMG/1000.)
                  FRSTMLIV = MAX(0.01,MIN(1.0, FRSTMLIV))
                  FRROTLIV = EXP(-0.2835*CROOTG/1000.*3.0)
                  FRROTLIV = MAX(0.01,MIN(1.0, FRROTLIV))
              ELSE
                  FRSTMLIV = 1.0
                  FRROTLIV = 1.0
              ENDIF
C
C     * N CONTENT AT PREVIOUS TIME STEP  
C
              NLEAF   = RNLEAF(I,J) * CLEAFP
              NSTEMT  = RNSTEM(I,J) * CSTEMP                ! AVERAGED FOR WHOLE STEMS
              NROOTT  = RNROOT(I,J) * CROOTP                ! AVERAGED FOR WHOLE ROOTS
C
              NSTEM  = NSTEMT - RNDEAD * CSTEMP * (1.-FRSTMLIV)       ! AVERAGED FOR LIVING PORTION
              NROOT  = NROOTT - RNDEAD * CROOTP * (1.-FRROTLIV)       ! AVERAGED FOR LIVING PORTION     
                RNSTMLIV = NSTEM/(CSTEMP*FRSTMLIV+0.000001)   
                RNROTLIV = NROOT/(CROOTP*FRROTLIV+0.000001)   
C
C --------------- PLANT NITROGEN UPTAKES ---------------------------------------------------
C
C     * ROOT N UPTAKE RATE (ROOT ION AFFINITY, ET and SOIL N CONTENT DEPENDENT)
C
              RTMASSX = FRROTLIV*CROOTG/RTMASS0                                !dimensionless root C
              LAMR    = RTMASSX*XMINF(I,J)
              FTSOIL  = exp(0.0693*(TSRZ-298.16))
              LAMR3   = LAMR *(KL*SNO3(I,J)+JMAX*FTSOIL)/(KM+SNO3(I,J))        !NO3 ion uptake rate determined by root interface properties (affinity)
              LAMR4   = LAMR *(KL*SOLNH4*SNH4(I,J)+JMAX*FTSOIL)
     1                  /(KM+SOLNH4*SNH4(I,J))                                 !NH4 ion uptake rate determined by root interface properties (affinity)
C     
              LAMET   = 0.2* max(ETR(I,J), 0.01*ET0)/ET0/WATRZ
              LAMD    = 0.4*RTMASSX*WATRZ
              LAMT    = LAMET + LAMD
C 
              KMIN3   = MIN(0.99/DELT,KMIN0 * min(LAMR3,LAMT))                 ! CAN'T BE OVER 1.0/DELT, OTHERWISE WOULD EXHAUST THE N 
              KMIN4   = MIN(0.99/DELT,KMIN0 * min(LAMR4,SOLNH4*LAMT))
c       BW FNH4UP Fix  8/2015
      IF (KMIN4 .GT. 0.0 .AND. KMIN3 .GT. 0.0) THEN  !!!HSuo, NOV,2012
              FNH4UP(I,J)    = KMIN4*SNH4(I,J)/(KMIN4*SNH4(I,J)
     1                                   +KMIN3*SNO3(I,J))
      ENDIF
              DNPUP(I,J) = KMIN4*SNH4(I,J)+ KMIN3*SNO3(I,J)                    ! ET and soil N -dependent root-uptake rate (gN/m2/s)
C
C      * GROWTH DEMAND FOR NITROGEN NUTRIENT
C 
              NLGROW  = max(0.0, NPPLF(I,J)*RNLF0*1.25)                      ! leaf growth N requirement: 25% of structural N for Rubisco
              NSGROW  = max(0.0, NPPSM(I,J)*RNSM0)                             ! living stem tissue growth N requirement
              NRGROW  = max(0.0, NPPRT(I,J)*RNRT0)                             ! living root growth N requirement
C
C      * PLANT TISSUE N DEFICIT COMPENSATION
C
              NLDPT   = max(0.0, (CLEAFG/DELT-NPPLF(I,J))
     1                           *(RNLF0*1.25-RNLEAF(I,J)))                    ! plant leaf N deficit
              NSDPT   = max(0.0, (FRSTMLIV*CSTEMG/DELT-NPPSM(I,J))
     1                           *(RNSM0-RNSTMLIV))                            ! plant living-stem (sapwood) N deficit
              NRDPT   = max(0.0, (FRROTLIV*CROOTG/DELT-NPPRT(I,J))
     1                           *(RNRT0-RNROTLIV))                            ! plant fine root N deficit
C
              NDEMAND = max(1.e-20,                                            ! ndemad cannot be zero to avoid error in code
     1                  NLGROW+NSGROW+NRGROW+NLDPT+NSDPT+NRDPT)                ! maximum N demand: gN/m2/s
C
C     * N REALLOCATION TO C POOL DUE TO: 1) LEAF/FINE ROOT LITTERING; 2) LIVING-STEM -> NON-LIVING TISSUE, if any
              NREALLO = CONREAL*RNLEAF(I,J)*LTRLF(I,J)                         ! gN/m2/s: N reallocation due to leaf death
C
C     * N UPTAKES AFFACTED BY N REALLOCATION AND DEMAND
C
              DNPUP(I,J) = min(DNPUP(I,J),MAX(NDEMAND-NREALLO,0.0))          ! limited by plant growth demends and pool capacity for N
C
C -------------------------- PLANT N LOSSES ----------------------------------------------
C
C     * LITTERING TO LITTER/SOIL
C
              DNPLTR(I,J) = (1.-CONREAL)*RNLEAF(I,J)*LTRLF(I,J)               ! leaf-falling
     3                    + RNROOT(I,J)*LTRRT(I,J)                            ! root turnover
     5                    + RNSTEM(I,J)*LTRSM(I,J)                            ! stem turnover
C
C     * NON-LITTERING LOSSES
C
              DNPLOSS(I,J) = RNLEAF(I,J)*CLSLF(I,J)
     1                     + RNSTEM(I,J)*CLSSM(I,J)
     2                     + RNROOT(I,J)*CLSRT(I,J)
C
C ---------- PLANT N STATUS UPDATE AND CONTROLS ON PHOTOSYNTHESIS -----------------------------
C
              NTOTAL  = NLEAF + NSTEMT + NROOTT                               ! N at previous step (gN/m2)
     1                + DNPUP(I,J)*DELT                                       ! N uptake (gN/m2)
     2                - (DNPLTR(I,J)+DNPLOSS(I,J))*DELT                       ! plant death/turnover (gN/m2)
C
              NLEAF   = NLEAF -RNLEAF(I,J)*(LTRLF(I,J)+CLSLF(I,J))*DELT 
     1                        + (NLGROW+NLDPT)/NDEMAND                  
     2                          *DNPUP(I,J)*DELT
C
              NSTEMT  = NSTEMT -RNSTEM(I,J)*(LTRSM(I,J)+CLSSM(I,J))*DELT 
     1                       + (NSGROW+NSDPT)/NDEMAND                                     
     2                          *DNPUP(I,J)*DELT 
C
              NROOTT  = NROOTT -RNROOT(I,J)*(LTRRT(I,J)+CLSRT(I,J))*DELT
     1                       + (NRGROW+NRDPT)/NDEMAND                   
     2                          *DNPUP(I,J)*DELT 
C               
C    * RUBISCO-RELATED N CONTENT (gN/m2 COVERED LAND SURFACE) IN LEAF
C
              FNRUB   = (0.31+3.43*EXP(-1.81*AILCG(I,J)))                    
     1                 /(2.32+6.30*EXP(-1.11*AILCG(I,J)))
              NRUB(I,J) = MAX(FNRUB*NLEAF,0.0)                               !update total leaf Rubisco-N content: gN/m2, no less than 0.0gN/m2
C
C    * RUBISCO-N IN gN/m2 LEAF AREA IN TOP LEAF CANOPY
C
              IF (AILCG(I,J) .GE. LAI0) THEN  
                  NRUB0(I,J) = MAX(NRUB(I,J),0.1)
     1                              *((1.0-EXP(-KRUBN*LAI0))/LAI0)      
     2                               /(1.0-EXP(-KRUBN*AILCG(I,J))) 
              ELSE
                IF (AILCG(I,J).NE.0.0) THEN
                  NRUB0(I,J) = MAX(NRUB(I,J), 0.1)/AILCG(I,J)   !LAI<LAI0 (1.0), nrub0 is the averaged over whole canopy
                ELSE
                  NRUB0(I,J) = 0.0
                ENDIF
              ENDIF
C
C     * UPDATE PLANT N/C
C   
              IF (CLEAFG.GT.0.0) RNLEAF(I,J)  = NLEAF/CLEAFG
              IF (CSTEMG.GT.0.0) RNSTEM(I,J)  = NSTEMT/CSTEMG
              IF (CROOTG.GT.0.0) RNROOT(I,J)  = NROOTT/CROOTG
C
C     * BIOLOGICAL N FIXATION
C
              DNBFIX(I,J) = NBFIX0*FTSOIL*EXP(-0.2*SNO3(I,J))
C
            ENDIF
150       CONTINUE
140   CONTINUE
C
C ---------------------- SOIL N TRANSFORMATIONS AND LOSSES (& POOL UPDATE) ------------------------
C
      DO 160 J=1, ICC+1
          DO 170 I= IL1, IL2
            IF (FCANSOIL(I,J) .GT. 0.0) THEN
C      
C      * MOISTURE/TEMPERATURE VARIABLES FOR CONTROLING SOIL N PROCESSES
C
              IF (BTDPTH(I).LE.DELZ(I,1)) THEN
                  WSTORAGE = THLIQ(I,1)*BTDPTH(I)*1000.0              ! unit: mm
                  WATRZ = THLIQ(I,1)/THPOR(I,1)
                  TSRZ = TBAR(I,1)
                  BSW  = BI(I,1)
              ELSEIF (BTDPTH(I).LE.(DELZ(I,1)+DELZ(I,2))) THEN
                  RTDP = BTDPTH(I) - DELZ(I,1)
                  WSTORAGE = (THLIQ(I,1)*DELZ(I,1)                       
     1                         +THLIQ(I,2)*RTDP)*1000.0                       ! unit: mm
                  WATRZ = (THLIQ(I,1)*DELZ(I,1)+THLIQ(I,2)*RTDP)
     1                     /(THPOR(I,1)*DELZ(I,1)+THPOR(I,2)*RTDP)
                  TSRZ  = (TBAR(I,1)*DELZ(I,1)+TBAR(I,2)*RTDP)           
     1                     /BTDPTH(I)
                  BSW   = (BI(I,1)*DELZ(I,1)+BI(I,2)*RTDP)               
     1                     /BTDPTH(I)
              ELSE
                  RTDP = BTDPTH(I) - DELZ(I,1) - DELZ(I,2)
                  WSTORAGE = (THLIQ(I,1)*DELZ(I,1)                       
     1                    +THLIQ(I,2)*DELZ(I,2)+THLIQ(I,3)*RTDP)*1000.0      ! unit: mm
                  WATRZ = (THLIQ(I,1)*DELZ(I,1)+THLIQ(I,2)*DELZ(I,2)     
     1                  +THLIQ(I,3)*RTDP)/(THPOR(I,1)*DELZ(I,1)          
     2                   +THPOR(I,2)*DELZ(I,2)+THPOR(I,3)*RTDP)
                  TSRZ = (TBAR(I,1)*DELZ(I,1)+TBAR(I,2)*DELZ(I,2)        
     1                   +TBAR(I,3)*RTDP)/BTDPTH(I)
                  BSW = (BI(I,1)*DELZ(I,1)+BI(I,2)*DELZ(I,2)             
     1                   +BI(I,3)*RTDP)/BTDPTH(I)
              ENDIF
              FTSOIL  = exp(0.0693*(TSRZ-298.16))
C
              IF (J.LE.ICC) THEN
                  DNPUPALL  = DNPUP(I,J)
                  DNPLTRALL = DNPLTR(I,J)
                  DNBFIXALL = DNBFIX(I,J)
C
              ELSE
                  DNPUPALL  = 0.0
                  DNPLTRALL = 0.0
                  DNBFIXALL = 0.0
C
              ENDIF
C
C     * C STOCK UNIT: kgC/m2 --> gC/m2, FOR CALCULATION CONVINIENCE
C
              CLITRG  = CLITR(I,J) *1000.0
              CSOMG   = CSOM(I,J) *1000.0
C
C     * RESTORE N CONTENT AT PREVIOUS TIME STEP BY RESTORING C STOCK FIRST  
C
                        IF (J.LE.ICC) THEN
              CLITRP  = CLITRG + (RSPLTR(I,J)+CLTRSOM(I,J)+CLSLTR(I,J)
     1                         - LTRLF(I,J)-LTRSM(I,J)-LTRRT(I,J))*DELT
                        ELSE
              CLITRP  = CLITRG + (RSPLTR(I,J)+CLTRSOM(I,J)+CLSLTR(I,J))
     1                         *DELT
                        ENDIF
C
              CSOMP   = CSOMG + (RSPSOM(I,J)-CLTRSOM(I,J))*DELT
C
              NLITR   = RNLITR(I,J) * CLITRP
              NSOM    = RNSOM(I,J) * CSOMP
C
C     * N TRANSFORMATION FROM LITTER TO SOM (& LITTER POOL UPDATE)
C
              DNLTRSOM(I,J) = (RSPLTR(I,J)+ CLTRSOM(I,J))*RNLITR(I,J) 
C
              NLITR  = NLITR +(DNPLTRALL- DNLTRSOM(I,J))*DELT
C   
C     * SOM N IMMOBILIZATION AND MINERALIZATION (& SOM N POOL UPDATE)
C
              IF (NSOM.GT.0.)  NIMMOB = EXP(1.0-0.03*CSOM(I,J)/NSOM)        ! mineralization rate adjusting factor due to immobilization/mineralization turnover 
              IF (CSOMP.GT.0.) DNMIN(I,J)=NIMMOB*RSPSOM(I,J)*NSOM/CSOMP
C
              NSOM       = NSOM - DNMIN(I,J)*DELT                          ! Soil OM N mineralization
     1                    +DNLTRSOM(I,J)*DELT                              ! litter N transformation
     2                    +RSPSOM(I,J)*RNSOM(I,J)*DELT                     ! N release from SOM respiration
              NSOM       = MAX(NSOM, 0.0)
C
C     * SOIL NH4 NITRIFICATION
C
              FNIT   = (1.0-WATRZ)*WATRZ                                    ! SOIL MOISTURE DEPENDENCE of nitrification
              KNIT   = KNI0*FNIT*FTSOIL/(0.25+1.0/SNH4(I,J))

C     * SOIL NO3 LEACHING (DUE TO DRAINAGE)
              KDRAIN = MAX(DRAIN(I,J), 0.0 )/WSTORAGE                       ! DRAINAGE (mm/s), WSTORAGE (mm)
              DNDRAIN(I,J) = MIN(0.99/DELT,KDRAIN)*SNO3(I,J)                ! N leaching (gN/m2/s)
C
C     * SOIL NO3 DENITRIFICATION
C
              FDNIT  = WATRZ**BSW                                           ! water dependence of denitrification
              KDNIT  = KDN0*FTSOIL*FDNIT                                    ! denitrification: 1/s
              DNDNIT(I,J) = MIN(0.99/DELT,KDNIT)*SNO3(I,J)                  ! loss from denitrification: gN/m2/s

!==================N2O by HSuo, 2009-Feb-05========================================
       !N2O was calculated by Nitrification (NN2O) and Denitrification (DN2ON2=DN2O+DN2) parts
       !only consider soil layer 1-10 cm
         
       ! ----------------initialization-------------------
       SOILT= TBAR(I,2)-TFREZ       !in oC
       BulkD = (1-THPOR(I,1))*2.65        ! Bulk Density = (1-porosity)*2.65 (g/cm3,Linn&Doran,1984)
       NSNH4(I,J) = SNH4(I,J)/(BulkD*DELZ(I,1))
       NSNO3(I,J) = SNO3(I,J)/(BulkD*DELZ(I,1))     ! unit convert (gN/m2-->ug N/g soil); BD(g/cm3);DELZ(m)
       WFPS= THLIQ(I,1)/THPOR(I,1)  !fraction; Volumetric soil water content/porosity THLIQ (ILG-grid,IG-soil layers)
              
       !----------------------N2O by Nitrification------------------------
       NWFPS =  (( (WFPS-Nb)/(Na-Nb) ) ** (Nd* (Nb-Na) / (Na-Nc) ))        
     1         * (( (WFPS-Nc)/(Na-Nc) )**Nd)
       NPH = 0.56 + ATAN(PI * 0.45 * (-5 +SOILPH(I,1)))/PI
       NSOILT= -0.06 + 0.13 * EXP(0.07*SOILT)
       NNH4(I,J) = 1- EXP(-0.0105 * NSNH4(I,J))
       NN2O(I,J)=NWFPS*NPH*NSOILT*(NKMAX+NN2OMAX*NNH4(I,J))
              
       !------------------N2&N2O from Denitrification--------------------------------
       FLXdNO3 = 11000 + 40000 * ATAN(PI*0.002* ( NSNO3(I,J)-180 ) )/PI
       DCO2 =SCO2mx * NWFPS * NSOILT
       !DCO2 = SOCRES*10.368 !can also use CTEM's SOCRES (uMOL CO2/M2.S-->kg C ha-1 d-1)
       FLXdCO2 = 24000/(1+200/EXP(0.35*DCO2))-100
       FLXdWFPS = Da/(Db**(Dc/(Db**(Dd*NWFPS))))
       DN2ON2(I,J) = MIN(FLXdNO3,FLXdCO2,FLXdWFPS)
              
       !------- ratio of N2:N2O for denitrification-------------------------
       FrWFPS = 1.4/13**(17/13**(2.2*NWFPS))
       FrNO3 = 1-(0.5+(1*ATAN(PI*0.01*(NSNO3(I,1)-190)))/PI)*25
       FrCO2 = 13+(30.78*ATAN(PI*0.07*(DCO2-13)))/PI
       RN2N2O = MIN(FrNO3,FrCO2,FrWFPS)
       !-------N2O from Denitrification------------
       DN2O(I,J) = DN2ON2(I,J)/(1.0+RN2N2O)
       DN2(I,J) = DN2ON2(I,J)/(1.0+1.0/RN2N2O)
       !-----------total N2O from Nitrification & Denitrification
       N2OTOT(I,J) = DN2O(I,J)+NN2O(I,J)          !N2O flux, gN/(ha.day)
       N2OTOT(I,J) = N2OTOT(I,J)/(10000*24*60*60) !unit convert: gN/(ha.day)-->[gN/m2/sec]
       N2TOT(I,J)  = DN2(I,J)/(10000*24*60*60)    ![gN/m2/sec]
!==================N2O by HSuo, 2009-Feb-05========================================

C     * SOIL NH3 VOLTILATION 
              IF (CALSOIL(I)) THEN
                  DNVOL(I,J) = MIN(0.99/DELT,KV0)*SNH4(I,J)                 ! gN/m2/s
              ENDIF
C
C     * SOIL/LITTER N DISTURBANCE LOSSES
C
              DNSLOSS(I,J) = RNLITR(I,J)*CLSLTR(I,J)
     1                     + RNSOM(I,J)*CLSSOM(I,J)
C
C -------------- SOIL-PLANT SYSTEM N SOURCES ------------------------------------------------------
C
              IF (J .LE. 9) THEN
C     * INORGANIC N FERTILIZATION (Assuming applied within one day, although not actually)
                  IF (IDAY .EQ. KGROWS(I,J)) THEN
                      DNFERI(I,J) = 0.5*NFER0/86400.                             ! 50% annual fertilizer N (gN/m2/yr) is applied into field 
                                                                           ! in the starting day of growth period
                  ELSEIF (IDAY .EQ. INT(KGROWS(I,J)+30.)) THEN                                                                           
                      DNFERI(I,J) = 0.3*NFER0/86400.                             ! 30% annual fertilizer N (gN/m2/yr) is applied into field
                                                                           ! 30 DAYS AFTER STARING GROWTH
                  ELSEIF (IDAY .EQ. INT(KGROWS(I,J)+60.)) THEN 
                      DNFERI(I,J) = 0.2*NFER0/86400.                             ! 20% annual fertilizer N (gN/m2/yr) is applied into field
                                                                           ! 60 DAYS AFTER STARING GROWTH
                  ENDIF                                         
C
C     * ORGANIC N FERTILIZATION (Assuming applied within one day after harvesting, although not actually)
                  IF (IDAY .EQ. KGROWE(I,J)+10.0) THEN
                      DNFERO(I,J) = NFERO0/86400.                                ! 100% ANNUAL fertilizer N (gN/m2/yr) is applied into field 
                  ENDIF                                         
C
              ENDIF
C
C     * NITROGEN DEPOSITION (Assuming at a daily constant rate)
C
              DNDEP(I,J) = NDEP0/365./86400.
C               
C ------------- UPDATE SOIL-PLANT N OR N/C RATIOS IN POOLS & OUTPUTS -------------------------------
C
C     * SOIL INORGANIC NITROGEN CONTENTS
C
!bw      IF (KMIN4 .GT. 0.0 .AND. KMIN3 .GT. 0.0) THEN  !!!HSuo, NOV,2012
!  bw            FNH4UP    = KMIN4*SNH4(I,J)/(KMIN4*SNH4(I,J)
!     1                                   +KMIN3*SNO3(I,J))
!      ENDIF
              SNH4(I,J) = MAX(0.0, SNH4(I,J)                            
     1                        + DNBFIXALL * DELT                        
     2                        + (DNDEP(I,J)+DNFERI(I,J))*0.5*DELT         !half of fertilization and deposition
     3                        + DNMIN(I,J)*DELT                           !N mineralization
     4                        - DNPUPALL*FNH4UP(I,J)*DELT                      !NH4 uptake 
     5                        - DNNIT(I,J)*DELT                           !nitrification
     6                        - DNVOL(I,J)*DELT)                          !NH3 voltilation
C

              SNO3(I,J) = MAX(0.0, SNO3(I,J)                            
     1                        + (DNDEP(I,J)+DNFERI(I,J))*0.5*DELT         ! half of fertilization and deposition
     2                        + DNNIT(I,J)*DELT                           ! nitrification
     3                        - DNPUPALL*(1.0-FNH4UP(I,j))*DELT                ! NO3 uptake 
     4                        - DNDNIT(I,J)*DELT                          ! Denitrification
     5                        - DNDRAIN(I,J)*DELT)                        ! Leaching
C
C     * UPDATE N/C RATIOS IN LITTER/SOM
C
              IF (CLITRG.GT.0.0) RNLITR(I,J)  = NLITR/CLITRG
              IF (CSOMG.GT.0.0)  RNSOM(I,J)   = NSOM/CSOMG
C
C     * OUTPUT SOIL-PLANT TOTAL SOURCES AND LOSSES
C
              DNSOURCE(I,J)= DNBFIXALL+DNDEP(I,J)+DNFERI(I,J)             ! total N source (gN/m2/s)
C             HSuo-->MAY ADD MORE: PLANT UPTAKE FROM AIR,et.al

C             DNLOSS(I,J)  = DNRUNOF(I,J)+DNDRAIN(I,J)+ DNDNIT(I,J)+DNVOL(I,J) !total N losses (gN/m2/s)
              DNLOSS(I,J)  = DNDRAIN(I,J)+DNDNIT(I,J)+DNVOL(I,J)+     !HSuo changed-->DNDRAIN is the NO3 leaching due to drainage
     1                       N2TOT(I,J)+N2OTOT(I,J)+DNSLOSS(I,J)
              IF (J.LT.ICC+1) DNLOSS(I,J) = DNLOSS(I,J) + DNPLOSS(I,J)
C             HSuo-->MAY ADD MORE: fire,harvest,runoff,et.al
C 
            ENDIF
170       CONTINUE
160   CONTINUE
      RETURN
      END SUBROUTINE CLASSN
