!>\file
C!Purpose: Initialize subarea variables for surface water budget 
C!calculations, and perform preliminary calculations for diagnostic 
C!variables.
C!

      SUBROUTINE WPREP(THLQCO, THLQGO, THLQCS, THLQGS, THICCO, THICGO,
     1                 THICCS, THICGS, HCPCO,  HCPGO,  HCPCS,  HCPGS,
     2                 GRKSC,  GRKSG,  GRKSCS, GRKSGS,
     3                 SPCC,   SPCG,   SPCCS,  SPCGS,  TSPCC,  TSPCG,
     4                 TSPCCS, TSPCGS, RPCC,   RPCG,   RPCCS,  RPCGS,
     5                 TRPCC,  TRPCG,  TRPCCS, TRPCGS, EVPIC,  EVPIG,
     6                 EVPICS, EVPIGS, ZPONDC, ZPONDG, ZPNDCS, ZPNDGS,
     7                 XSNOWC, XSNOWG, XSNOCS, XSNOGS, ZSNOWC, ZSNOWG,
     8                 ZSNOCS, ZSNOGS, ALBSC,  ALBSG,  ALBSCS, ALBSGS, 
     9                 RHOSC,  RHOSG,  HCPSC,  HCPSG,  HCPSCS, HCPSGS, 
     A                 RUNFC,  RUNFG,  RUNFCS, RUNFGS,
     B                 TRUNFC, TRUNFG, TRNFCS, TRNFGS, TBASC,  TBASG,  
     C                 TBASCS, TBASGS, GFLXC,  GFLXG,  GFLXCS, GFLXGS,
     D                 SUBLC,  SUBLCS, WLOSTC, WLOSTG, WLSTCS, WLSTGS,
     E                 RAC,    RACS,   SNC,    SNCS,   TSNOWC, TSNOWG,
     F                 OVRFLW, SUBFLW, BASFLW, TOVRFL, TSUBFL, TBASFL, 
     G                 PCFC,   PCLC,   PCPN,   PCPG,   QFCF,   QFCL,
     H                 QFN,    QFG,    QFC,    HMFG,   
     I                 ROVG,   ROFC,   ROFN,   TRUNOF,
     J                 THLIQX, THICEX, THLDUM, THIDUM,
     J                 DT,     RDUMMY, ZERO,   IZERO,  DELZZ,
     K                 FC,     FG,     FCS,    FGS,    
     L                 THLIQC, THLIQG, THICEC, THICEG, HCPC,   HCPG,
     M                 TBARC,  TBARG,  TBARCS, TBARGS, TBASE,  TSURX,
     N                 FSVF,   FSVFS,  RAICAN, SNOCAN, RAICNS, SNOCNS, 
     O                 EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS, 
     P                 RPCP,   TRPCP,  SPCP,   TSPCP,  RHOSNI, ZPOND,  
     Q                 ZSNOW,  ALBSNO, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     R                 THPOR,  HCPS,   GRKSAT, ISAND,  DELZW,  DELZ,
     S                 ILG,    IL1,    IL2,    JL,     IG,     IGP1,
     T                 NLANDCS,NLANDGS,NLANDC, NLANDG, RADD,   SADD )
C
C     * AUG 25/11 - D.VERSEGHY. REFINE CALCULATION OF TEMPERATURE OF
C     *                         LUMPED PRECIPITATION.
C     * NOV 24/09 - D.VERSEGHY. RESTORE EVAPOTRANSPIRATION WHEN
C     *                         PRECIPITATION IS OCCURRING.
C     * MAR 27/08 - D.VERSEGHY. MOVE MODIFICATION OF GRKSAT IN PRESENCE
C     *                         OF ICE TO GRINFL AND GRDRAN.
C     * FEB 19/07 - D.VERSEGHY. MODIFICATIONS TO REFLECT SHIFT OF CANOPY
C     *                         WATER DEPOSITION CALCULATIONS TO TSOLVC,
C     *                         AND SUPPRESSION OF ALL EVAPOTRANSPIRATION 
C     *                         WHEN PRECIPITATION IS OCCURRING.
C     * MAR 23/06 - D.VERSEGHY. MODIFY CALCULATIONS OF SNOW THERMAL
C     *                         PROPERTIES TO ACCOUNT FOR WATER CONTENT.
C     * MAR 21/06 - P.BARTLETT. INITIALIZE ADDITIONAL VARIABLES TO ZERO.
C     * DEC 07/05 - D.VERSEGHY. ADD INITIALIZATION OF TBASE SUBAREAS.
C     * OCT 05/05 - D.VERSEGHY. MODIFY DELZZ CALCULATION FOR IG>3.
C     * APR 15/05 - D.VERSEGHY. SUBLIMATION OF INTERCEPTED SNOW TAKES
C     *                         PLACE BEFORE EVAPORATION OF INTERCEPTED
C     *                         RAIN.
C     * MAR 30/05 - D.VERSEGHY/R.SOULIS. ADD RUNOFF TEMPERATURE 
C     *                         INITIALIZATIONS AND MODIFICATION OF 
C     *                         GRKSAT FOR TEMPERATURE AND PRESENCE 
C     *                         OF ICE.
C     * SEP 13/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 21/04 - Y.DELAGE, R.HARVEY, D.VERSEGHY. NEW LOWER LIMIT 
C     *                         ON RADD AND SADD.
C     * SEP 26/02 - D.VERSEGHY. MODIFICATIONS ASSOCIATED WITH BUGFIX
C     *                         IN SUBCAN.
C     * AUG 06/02 - D.VERSEGHY. SHORTENED CLASS3 COMMON BLOCK.
C     * JUN 18/02 - D.VERSEGHY. MOVE PARTITIONING OF PRECIPITATION
C     *                         BETWEEN RAINFALL AND SNOWFALL INTO
C     *                         "CLASSI"; TIDY UP SUBROUTINE CALL;
C     *                         CHANGE RHOSNI FROM CONSTANT TO
C     *                         VARIABLE.
C     * OCT 04/01 - M.LAZARE.   NEW DIAGNOSTIC FIELD "ROVG".
C     * NOV 09/00 - D.VERSEGHY. MOVE DIAGNOSTIC CALCULATIONS FROM 
C     *                         SUBCAN INTO THIS ROUTINE.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     * JAN 02/95 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS; INTRODUCE CALCULATION OF
C     *                         OVERLAND FLOW.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         RATIONALIZE USE OF "WLOST":
C     *                         COMPLETION OF WATER BUDGET DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 16/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         INITIALIZE TWO NEW DIAGNOSTIC FIELDS.
C     * AUG 20/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REVISED CALCULATION OF CANOPY 
C     *                         SUBLIMATION RATE.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. NEW DIAGNOSTIC FIELDS. 
C     * APR 15/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. PREPARATION AND INITIALIZATION FOR
C     *                         LAND SURFACE WATER BUDGET CALCULATIONS.

      IMPLICIT NONE
C                                                     
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IG,IGP1,I,J,NLANDCS,NLANDGS,NLANDC,NLANDG
C
C     * OUTPUT ARRAYS.
C
      !
      !(Suffix CS = vegetation over snow cover; GS = bare snow cover; C
      !or CO = vegetation over ground; G or GO = bare ground.)
      !
      REAL THLQCO(ILG,IG) !< Subarea volumetric liquid water content of soil layers \f$[m^3 m^{-3}]\f$
      REAL THLQGO(ILG,IG) !< Subarea volumetric liquid water content of soil layers \f$[m^3 m^{-3}]\f$
      REAL THLQCS(ILG,IG) !< Subarea volumetric liquid water content of soil layers \f$[m^3 m^{-3}]\f$
      REAL THLQGS(ILG,IG) !< Subarea volumetric liquid water content of soil layers \f$[m^3 m^{-3}]\f$
C               
      REAL THICCO(ILG,IG) !<Subarea volumetric frozen water content of soil layers 
                          !<\f$[m^3 m^{-3}] (\theta_i)\f$
      REAL THICGO(ILG,IG) !<Subarea volumetric frozen water content of soil layers 
                          !<\f$[m^3 m^{-3}] (\theta_i)\f$
      REAL THICCS(ILG,IG) !<Subarea volumetric frozen water content of soil layers 
                          !<\f$[m^3 m^{-3}] (\theta_i)\f$
      REAL THICGS(ILG,IG) !<Subarea volumetric frozen water content of soil layers 
                          !<\f$[m^3 m^{-3}] (\theta_i)\f$
C              
      REAL HCPCO (ILG,IG) !<Subarea heat capacity of soil layers \f$[J m^{-3} K^{-1}]\f$
      REAL HCPGO (ILG,IG) !<Subarea heat capacity of soil layers \f$[J m^{-3} K^{-1}]\f$
      REAL HCPCS (ILG,IG) !<Subarea heat capacity of soil layers \f$[J m^{-3} K^{-1}]\f$
      REAL HCPGS (ILG,IG) !<Subarea heat capacity of soil layers \f$[J m^{-3} K^{-1}]\f$
C 
      REAL GRKSC (ILG,IG) !<Subarea saturated hydraulic conductivity \f$[m s^{-1}] (K_{sat})\f$
      REAL GRKSG (ILG,IG) !<Subarea saturated hydraulic conductivity \f$[m s^{-1}] (K_{sat})\f$
      REAL GRKSCS(ILG,IG) !<Subarea saturated hydraulic conductivity \f$[m s^{-1}] (K_{sat})\f$
      REAL GRKSGS(ILG,IG) !<Subarea saturated hydraulic conductivity \f$[m s^{-1}] (K_{sat})\f$
C
      REAL GFLXC (ILG,IG) !<Subarea heat flux between soil layers \f$[W m^{-2}]\f$
      REAL GFLXG (ILG,IG) !<Subarea heat flux between soil layers \f$[W m^{-2}]\f$
      REAL GFLXCS(ILG,IG) !<Subarea heat flux between soil layers \f$[W m^{-2}]\f$
      REAL GFLXGS(ILG,IG) !<Subarea heat flux between soil layers \f$[W m^{-2}]\f$
C
      REAL THLDUM(ILG,IG) !<Internal CLASSW dummy variable for soil frozen water \f$[m^3 m^{-3}]\f$
      REAL THIDUM(ILG,IG) !<Internal CLASSW dummy variable for soil frozen water \f$[m^3 m^{-3}]\f$
      REAL QFC   (ILG,IG) !<Water removed from soil layers by transpiration \f$[kg m^{-2} s^{-1}]\f$
      REAL HMFG  (ILG,IG) !<Energy associated with phase change of water in soil layers \f$[W m^{-2}]\f$
C
      REAL THLIQX(ILG,IGP1) !<Internal CLASSW work array for soil frozen water \f$[m^3 m^{-3}]\f$
      REAL THICEX(ILG,IGP1) !<Internal CLASSW work array for soil frozen water \f$[m^3 m^{-3}]\f$
C
      REAL SPCC  (ILG) !<Subarea snowfall rate \f$[m s^{-1}]\f$
      REAL SPCG  (ILG) !<Subarea snowfall rate \f$[m s^{-1}]\f$
      REAL SPCCS (ILG) !<Subarea snowfall rate \f$[m s^{-1}]\f$
      REAL SPCGS (ILG) !<Subarea snowfall rate \f$[m s^{-1}]\f$
C
      REAL TSPCC (ILG) !<Subarea snowfall temperature [K/C]
      REAL TSPCG (ILG) !<Subarea snowfall temperature [K/C]
      REAL TSPCCS(ILG) !<Subarea snowfall temperature [K/C]
      REAL TSPCGS(ILG) !<Subarea snowfall temperature [K/C]
C
      REAL RPCC  (ILG) !<Subarea rainfall rate \f$[m s^{-1}]\f$
      REAL RPCG  (ILG) !<Subarea rainfall rate \f$[m s^{-1}]\f$
      REAL RPCCS (ILG) !<Subarea rainfall rate \f$[m s^{-1}]\f$
      REAL RPCGS (ILG) !<Subarea rainfall rate \f$[m s^{-1}]\f$
C
      REAL TRPCC (ILG) !<Subarea rainfall temperature [K/C]
      REAL TRPCG (ILG) !<Subarea rainfall temperature [K/C]
      REAL TRPCCS(ILG) !<Subarea rainfall temperature [K/C]
      REAL TRPCGS(ILG) !<Subarea rainfall temperature [K/C]
C
      REAL EVPIC (ILG) !<Subarea evapotranspiration rate going into CLASSW \f$[m s^{-1}]\f$
      REAL EVPIG (ILG) !<Subarea evapotranspiration rate going into CLASSW \f$[m s^{-1}]\f$
      REAL EVPICS(ILG) !<Subarea evapotranspiration rate going into CLASSW \f$[m s^{-1}]\f$
      REAL EVPIGS(ILG) !<Subarea evapotranspiration rate going into CLASSW \f$[m s^{-1}]\f$
C
      REAL ZPONDC(ILG) !<Subarea depth of surface ponded water [m]
      REAL ZPONDG(ILG) !<Subarea depth of surface ponded water [m]
      REAL ZPNDCS(ILG) !<Subarea depth of surface ponded water [m]
      REAL ZPNDGS(ILG) !<Subarea depth of surface ponded water [m]
C
      REAL XSNOWC(ILG) !<Subarea fractional snow coverage [ ]
      REAL XSNOWG(ILG) !<Subarea fractional snow coverage [ ]
      REAL XSNOCS(ILG) !<Subarea fractional snow coverage [ ]
      REAL XSNOGS(ILG) !<Subarea fractional snow coverage [ ]
C
      REAL ZSNOWC(ILG) !<Subarea depth of snow pack
      REAL ZSNOWG(ILG) !<Subarea depth of snow pack
      REAL ZSNOCS(ILG) !<Subarea depth of snow pack
      REAL ZSNOGS(ILG) !<Subarea depth of snow pack
C
      REAL ALBSC (ILG) !<Subarea snow albedo [ ]
      REAL ALBSG (ILG) !<Subarea snow albedo [ ]
      REAL ALBSCS(ILG) !<Subarea snow albedo [ ]
      REAL ALBSGS(ILG) !<Subarea snow albedo [ ]
C
      REAL RHOSC (ILG) !<Subarea snow density \f$[kg m^{-3}]\f$
      REAL RHOSG (ILG) !<Subarea snow density \f$[kg m^{-3}]\f$
C
      REAL HCPSC (ILG) !<Subarea heat capacity of snow \f$[J m^{-3} K^{-1}] (C_s)\f$
      REAL HCPSG (ILG) !<Subarea heat capacity of snow \f$[J m^{-3} K^{-1}] (C_s)\f$
      REAL HCPSCS(ILG) !<Subarea heat capacity of snow \f$[J m^{-3} K^{-1}] (C_s)\f$
      REAL HCPSGS(ILG) !<Subarea heat capacity of snow \f$[J m^{-3} K^{-1}] (C_s)\f$
C
      REAL RUNFC (ILG) !<Subarea total runoff [m]
      REAL RUNFG (ILG) !<Subarea total runoff [m]
      REAL RUNFCS(ILG) !<Subarea total runoff [m]
      REAL RUNFGS(ILG) !<Subarea total runoff [m]
C
      REAL TRUNFC(ILG) !<Subarea total runoff temperature [K]
      REAL TRUNFG(ILG) !<Subarea total runoff temperature [K]
      REAL TRNFCS(ILG) !<Subarea total runoff temperature [K]
      REAL TRNFGS(ILG) !<Subarea total runoff temperature [K]
C
      REAL TBASC (ILG) !<Subarea temperature of bedrock in third soil layer [C]
      REAL TBASG (ILG) !<Subarea temperature of bedrock in third soil layer [C]
      REAL TBASCS(ILG) !<Subarea temperature of bedrock in third soil layer [C]
      REAL TBASGS(ILG) !<Subarea temperature of bedrock in third soil layer [C]
C
C
      REAL SUBLC (ILG) !<Subarea sublimation rate from vegetation \f$[m s^{-1}]\f$
      REAL SUBLCS(ILG) !<Subarea sublimation rate from vegetation \f$[m s^{-1}]\f$

      REAL WLOSTC(ILG) !<Subarea residual water not met by surface stores \f$[kg m^{-2}]\f$
      REAL WLOSTG(ILG) !<Subarea residual water not met by surface stores \f$[kg m^{-2}]\f$
      REAL WLSTCS(ILG) !<Subarea residual water not met by surface stores \f$[kg m^{-2}]\f$
      REAL WLSTGS(ILG) !<Subarea residual water not met by surface stores \f$[kg m^{-2}]\f$
C   
      REAL RAC   (ILG) !<Subarea liquid water on canopy going into CLASSW \f$[kg m^{-2}]\f$
      REAL RACS  (ILG) !<Subarea liquid water on canopy going into CLASSW \f$[kg m^{-2}]\f$
      REAL SNC   (ILG) !<Subarea frozen water on canopy going into CLASSW \f$[kg m^{-2}]\f$
      REAL SNCS  (ILG) !<Subarea frozen water on canopy going into CLASSW \f$[kg m^{-2}]\f$
      REAL TSNOWC(ILG) !<Subarea snowpack temperature [K]
      REAL TSNOWG(ILG) !<Subarea snowpack temperature [K]

      REAL OVRFLW(ILG)  !<Overland flow from top of soil column [m]   
      REAL SUBFLW(ILG)  !<Interflow from sides of soil column [m]
      REAL BASFLW(ILG)  !<Base flow from bottom of soil column [m] 
      REAL TOVRFL(ILG)  !<Temperature of overland flow from top of soil column [K]
      REAL TSUBFL(ILG)  !<Temperature of interflow from sides of soil column [K]
      REAL TBASFL(ILG)  !<Temperature of base flow from bottom of soil column [K]
      REAL PCFC  (ILG)  !<Frozen precipitation intercepted by vegetation \f$[kg m^{-2} s^{-1}]\f$
      REAL PCLC  (ILG)  !<Liquid precipitation intercepted by vegetation \f$[kg m^{-2} s^{-1}]\f$
      REAL PCPN  (ILG)  !<Precipitation incident on snow pack \f$[kg m^{-2} s^{-1}]\f$
      REAL PCPG  (ILG)  !<Precipitation incident on ground \f$[kg m^{-2} s^{-1}]\f$
      REAL QFCF  (ILG)  !<Sublimation from frozen water on vegetation \f$[kg m^{-2} s^{-1}]\f$
      REAL QFCL  (ILG)  !<Evaporation from liquid water on vegetation \f$[kg m^{-2} s^{-1}]\f$
      REAL QFN   (ILG)  !<Sublimation from snow pack \f$[kg m^{-2} s^{-1}]\f$ 
      REAL QFG   (ILG)  !<Evaporation from ground \f$[kg m^{-2} s^{-1}]\f$
      REAL ROVG  (ILG)  !<Liquid/frozen water runoff from vegetation to ground surface \f$[kg m^{-2} s^{-1}]\f$
      REAL ROFC  (ILG)  !<Liquid/frozen water runoff from vegetation \f$[kg m^{-2} s^{-1}]\f$
      REAL ROFN  (ILG)  !<Liquid water runoff from snow pack \f$[kg m^{-2} s^{-1}]\f$
      REAL TRUNOF(ILG)  !<Temperature of total runoff [K] 
      REAL DT    (ILG)  !<Time stepping variable used in GRINFL/GRDRAN [s]
      REAL RDUMMY(ILG)  !<Dummy variable 
      REAL ZERO  (ILG)  !<Zero vector used in several subroutines [ ]
C 
      INTEGER IZERO (ILG) !<Zero integer flag used in GRINFL
C
C     * INPUT ARRAYS.
C
      !(Suffix CS = vegetation over snow cover; GS = bare snow cover; C 
      !or CO = vegetation over ground; G or GO = bare ground.)
      !
      REAL FC    (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL FG    (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL FCS   (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL FGS   (ILG) !<Subarea fractional coverage of modelled area [ ]
C
      REAL FSVF  (ILG)  !<Sky view factor of ground under vegetation canopy [ ]
      REAL FSVFS (ILG)  !<Sky view factor of snow under vegetation canopy [ ]
      REAL RAICAN(ILG)  !<Intercepted liquid water stored on canopy over ground \f$[kg m^{-2}]\f$
      REAL SNOCAN(ILG)  !<Intercepted frozen water stored on canopy over ground \f$[kg m^{-2}]\f$
      REAL RAICNS(ILG)  !<Intercepted liquid water stored on canopy over snow \f$[kg m^{-2}]\f$
      REAL SNOCNS(ILG)  !<Intercepted frozen water stored on canopy over snow \f$[kg m^{-2}]\f$
      REAL EVAPC (ILG)  !<Evaporation from vegetation over ground \f$[m s^{-1}]\f$ 
      REAL EVAPCG(ILG)  !<Evaporation from ground under vegetation \f$[m s^{-1}]\f$
      REAL EVAPG (ILG)  !<Evaporation from bare ground \f$[m s^{-1}]\f$ 
      REAL EVAPCS(ILG)  !<Evaporation from vegetation over snow \f$[m s^{-1}]\f$ 
      REAL EVPCSG(ILG)  !<Evaporation from snow under vegetation \f$[m s^{-1}]\f$
      REAL EVAPGS(ILG)  !<Evaporation from snow on bare ground \f$[m s^{-1}]\f$
      REAL RPCP  (ILG)  !<Rainfall rate over modelled area \f$[m s^{-1}]\f$   
      REAL TRPCP (ILG)  !,Rainfall temperature over modelled area [C] 
      REAL SPCP  (ILG)  !<Snowfall rate over modelled area \f$[m s^{-1}]\f$ 
      REAL TSPCP (ILG)  !<Snowfall temperature over modelled area [C]
      REAL RHOSNI(ILG)  !<Density of fresh snow \f$[kg m^{-3}]\f$ 
      REAL ZPOND (ILG)  !<Depth of ponded water on surface [m] 
      REAL ZSNOW (ILG)  !<Depth of snow pack \f$[m] (z_s)\f$ 
      REAL ALBSNO(ILG)  !<Albedo of snow [ ]
      REAL WSNOCS(ILG)  !<Liquid water content of snow pack under vegetation \f$[kg m^{-2}] (w_s)\f$ 
      REAL WSNOGS(ILG)  !<Liquid water content of snow pack in bare areas \f$[kg m^{-2}] (w_s)\f$
      REAL RHOSCS(ILG)  !<Density of snow under vegetation \f$[kg m^{-3}] (\rho_s)\f$
      REAL RHOSGS(ILG)  !<Density of snow in bare areas \f$[kg m^{-3}] (\rho_s)\f$
      REAL TBASE (ILG)  !<Temperature of bedrock in third soil layer [K] 
      REAL TSURX(ILG,4) !<Ground surface temperature over subarea [K]
C
      REAL THLIQC(ILG,IG) !<Liquid water content of soil layers under vegetation \f$[m^3 m^{-3}]\f$
      REAL THLIQG(ILG,IG) !<Liquid water content of soil layers in bare areas \f$[m^3 m^{-3}]\f$
      REAL THICEC(ILG,IG) !<Frozen water content of soil layers under vegetation \f$[m^3 m^{-3}]\f$
      REAL THICEG(ILG,IG) !<Frozen water content of soil layers in bare areas \f$[m^3 m^{-3}]\f$
      REAL TBARC (ILG,IG) !<Subarea temperatures of soil layers \f$[C] (T_g)\f$
      REAL TBARG (ILG,IG) !<Subarea temperatures of soil layers \f$[C] (T_g)\f$
      REAL TBARCS(ILG,IG) !<Subarea temperatures of soil layers \f$[C] (T_g)\f$
      REAL TBARGS(ILG,IG) !<Subarea temperatures of soil layers \f$[C] (T_g)\f$
      REAL HCPC  (ILG,IG) !<Heat capacity of soil layers under vegetation \f$[J m^{-3} K^{-1}]\f$
      REAL HCPG  (ILG,IG) !<
C
C     * SOIL INFORMATION ARRAYS.
C
      REAL THPOR (ILG,IG) !<Pore volume in soil layer \f$[m^3 m^{-3}] (\theta_p)\f$
      REAL HCPS  (ILG,IG) !<Heat capacity of soil material \f$[J m^{-3} K^{-1}]\f$
      REAL GRKSAT(ILG,IG) !<Saturated hydraulic conductivity of soil layers \f$[m s^{-1}]\f$
      REAL DELZZ (ILG,IG) !<Soil layer depth variable used in TWCALC/TMCALC [m]
      REAL DELZW (ILG,IG) !<Permeable thickness of soil layer [m]
      REAL DELZ  (IG)     !<Overall thickness of soil layer [m]
C
      INTEGER ISAND (ILG,IG) !<Sand content flag
C
C     * INTERNAL WORK ARRAYS.
C
      REAL RADD  (ILG),   SADD  (ILG)  
C                                                                                  
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
      REAL TCW      !<Thermal conductivity of water \f$(0.57) [W m^{-1} K^{-1}]\f$
      REAL TCICE    !<Thermal conductivity of ice \f$(2.24) [W m^{-1} K^{-1}]\f$
      REAL TCSAND   !<Thermal conductivity of sand particles \f$(2.5) [W m^{-1} K^{-1}]\f$
      REAL TCCLAY   !<Thermal conductivity of fine mineral particles 
                    !<\f$(2.5) [W m^{-1} K^{-1}]\f$
      REAL TCOM     !<Thermal conductivity of organic matter \f$(0.25) [W m^{-1} K^{-1}]\f$
      REAL TCDRYS   !<Thermal conductivity of dry mineral soil \f$(0.275) [W m^{-1} K^{-1}]\f$
      REAL RHOSOL   !<Density of soil mineral matter \f$(2.65 * 10^3) [kg m^{-3}]\f$
      REAL RHOOM    !<Density of soil organic matter \f$(1.30 * 10^3) [kg m^{-3}]\f$
      REAL HCPW     !<Volumetric heat capacity of water \f$(4.187 * 10^6) 
                    !< [J m^{-3} K^{-1}]\f$
      REAL HCPICE   !<Volumetric heat capacity of ice \f$(1.9257 * 10^6) 
                    !< $[J m^{-3} K^{-1}]\f$
      REAL HCPSOL   !<Volumetric heat capacity of mineral matter 
                    !<\f$(2.25 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPOM    !<Volumetric heat capacity of organic matter 
                    !<\f$(2.50 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSND   !<Volumetric heat capacity of sand particles 
                    !<\f$(2.13 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPCLY   !<Volumetric heat capacity of fine mineral particles 
                    !<\f$(2.38 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL SPHW     !<Specific heat of water \f$(4.186 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHICE   !<Specific heat of ice \f$(2.10 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHVEG   !<Specific heat of vegetation matter \f$(2.70 * 10^3) 
                    !< [J kg^{-1} K^{-1}]\f$
      REAL SPHAIR   !<Specific heat of air \f$[J kg^{-1} K^{-1}]\f$
      REAL RHOW     !<Density of water \f$(1.0 * 10^3) [kg m^{-3}]\f$
      REAL RHOICE   !<Density of ice \f$(0.917 * 10^3) [kg m^{-3}]\f$
      REAL TCGLAC   !<Thermal conductivity of ice sheets \f$(2.24) [W m^{-1} K^{-1}]\f$
      REAL CLHMLT   !<Latent heat of freezing of water \f$(0.334 * 10^6) [J kg^{-1}]\f$
      REAL CLHVAP   !<Latent heat of vaporization of water \f$(2.501 * 10^6) [J kg^{-1}]\f$
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
      !>
      !!In the first three loops, various subarea arrays and internal 
      !!CLASSW variables are initialized.
      !! 
C     * INITIALIZE 2-D ARRAYS.
C
      DO 50 J=1,IG
      DO 50 I=IL1,IL2                                                               
          THLQCO(I,J)=0.0                                                           
          THLQGO(I,J)=0.0                                                           
          THLQCS(I,J)=0.0                                                           
          THLQGS(I,J)=0.0
          THICCO(I,J)=0.0                                                           
          THICGO(I,J)=0.0                                                           
          THICCS(I,J)=0.0                                                           
          THICGS(I,J)=0.0
          HCPCO (I,J)=0.0                                                            
          HCPGO (I,J)=0.0                                                            
          HCPCS (I,J)=0.0                                                            
          HCPGS (I,J)=0.0                                                            
          GRKSC (I,J)=0.0                                                            
          GRKSG (I,J)=0.0                                                            
          GRKSCS(I,J)=0.0                                                            
          GRKSGS(I,J)=0.0                                                            
          GFLXC (I,J)=0.0
          GFLXG (I,J)=0.0
          GFLXCS(I,J)=0.0
          GFLXGS(I,J)=0.0
          QFC   (I,J)=0.0
          HMFG  (I,J)=0.0
          THLDUM(I,J)=0.0
          THIDUM(I,J)=0.0
          IF(J.EQ.3.AND.IG.EQ.3) THEN
              DELZZ (I,J)=DELZW(I,J)
          ELSE
              DELZZ (I,J)=DELZ(J)
          ENDIF
   50 CONTINUE
C 
      DO 75 J=1,IGP1
      DO 75 I=IL1,IL2
          THLIQX(I,J)=0.0
          THICEX(I,J)=0.0
   75 CONTINUE
C
C     * INITIALIZE OTHER DIAGNOSTIC AND WORK ARRAYS.
C
      DO 100 I=IL1,IL2
          EVPICS(I)=EVAPCS(I)+EVPCSG(I)
          EVPIGS(I)=EVAPGS(I)
          EVPIC (I)=EVAPC (I)+EVAPCG(I)
          EVPIG (I)=EVAPG (I)
          TSNOWC(I)=0.0
          TSNOWG(I)=0.0
          WLOSTC(I)=0.0                                                                  
          WLOSTG(I)=0.0                                                                  
          WLSTCS(I)=0.0                                                                  
          WLSTGS(I)=0.0                                                                  
          RAC   (I)=RAICAN(I)
          RACS  (I)=RAICNS(I)                                                                 
          SNC   (I)=SNOCAN(I)                                                                  
          SNCS  (I)=SNOCNS(I)
          PCFC  (I)=0.0
          PCLC  (I)=0.0
          PCPN  (I)=0.0
          PCPG  (I)=0.0
          QFN   (I)=0.0
          QFG   (I)=0.0
          ROVG  (I)=0.0
          ROFC  (I)=0.0
          ROFN  (I)=0.0
          OVRFLW(I)=0.0
          SUBFLW(I)=0.0
          BASFLW(I)=0.0
          TOVRFL(I)=0.0
          TSUBFL(I)=0.0
          TBASFL(I)=0.0
          ZPONDC(I)=0.0
          ZPONDG(I)=0.0
          ZPNDCS(I)=0.0
          ZPNDGS(I)=0.0
          XSNOWC(I)=0.0
          XSNOWG(I)=0.0
          XSNOCS(I)=0.0
          XSNOGS(I)=0.0
          ZSNOWC(I)=0.0
          ZSNOWG(I)=0.0
          ZSNOCS(I)=0.0
          ZSNOGS(I)=0.0
          ALBSC (I)=0.0
          ALBSG (I)=0.0
          ALBSCS(I)=0.0
          ALBSGS(I)=0.0
          RHOSC (I)=0.0
          RHOSG (I)=0.0
          HCPSC (I)=0.0
          HCPSG (I)=0.0
          HCPSCS(I)=0.0
          HCPSGS(I)=0.0
          RUNFC (I)=0.0
          RUNFG (I)=0.0
          RUNFCS(I)=0.0
          RUNFGS(I)=0.0
          TRUNFC(I)=0.0
          TRUNFG(I)=0.0
          TRNFCS(I)=0.0
          TRNFGS(I)=0.0
          TRUNOF(I)=0.0
          TBASC (I)=TBASE(I)-TFREZ
          TBASG (I)=TBASE(I)-TFREZ
          TBASCS(I)=TBASE(I)-TFREZ
          TBASGS(I)=TBASE(I)-TFREZ
          DT    (I)=DELT
          RDUMMY(I)=0.
          ZERO  (I)=0.
          IZERO (I)=0 
C                                                                 
C     * PRECIPITATION DIAGNOSTICS.
C
          !>
          !!At the end of the 100 loop, a preliminary calculation of the 
          !!precipitation diagnostics over the four subareas is carried 
          !!out, as follows:
          !!
          !!The rainfall incident on the vegetation, PCLC, is summed over 
          !!the vegetated subareas FC and FCS, minus the fraction that 
          !!falls through the gaps in the canopy (denoted by the sky view 
          !!factors FSVF and FSVFS respectively). The rainfall incident 
          !!on the snowpack PCPN is the sum of the rainfall on the snow-
          !!covered bare area FGS, and that on the snow under the gaps in 
          !!the canopy in subarea FCS. The rainfall incident on bare 
          !!ground, PCPG, is the sum of the rainfall on the snow-free 
          !!bare area FG, and that on the ground under the gaps in the 
          !!canopy in subarea FC. The snowfall incident on the 
          !!vegetation, PCFC, as in the case of rainfall, is summed over 
          !!the vegetation subareas FC and FCS, minus the fraction that 
          !!falls through the gaps in the canopy. The remaining amount is 
          !!assigned to snowfall incident on the snow pack, PCPN.
          !!
          IF(RPCP(I).GT.0.)                                      THEN 
              PCLC(I)=(FCS(I)*(1.0-FSVFS(I))+FC(I)*(1.0-FSVF(I)))*
     1                RPCP(I)*RHOW
              PCPN(I)=(FCS(I)*FSVFS(I)+FGS(I))*RPCP(I)*RHOW
              PCPG(I)=(FC(I)*FSVF(I)+FG(I))*RPCP(I)*RHOW
          ENDIF
C
          IF(SPCP(I).GT.0.)                                      THEN 
              PCFC(I)=(FCS(I)*(1.0-FSVFS(I))+FC(I)*(1.0-FSVF(I)))*
     1                SPCP(I)*RHOSNI(I)
              PCPN(I)=PCPN(I)+(FCS(I)*FSVFS(I)+FGS(I)+
     1                FC(I)*FSVF(I)+FG(I))*SPCP(I)*RHOSNI(I)
          ENDIF
  100 CONTINUE
      !>
      !!In loops 200 to 550, each of the four subareas is addressed in 
      !!turn. Additional variables are initialized, and an empirical 
      !!correction is applied to the saturated hydraulic conductivity in 
      !!each of the soil layers over the subarea, to account for the 
      !!viscosity of water at the layer temperature (Dingman, 2002):
      !!
      !!\f$K_{sat}' = (1.7915 * 10^{-3}) K_{sat} / [(2.0319 * 10^{-4})+(1.5883 * 10^{-3}) exp(-T_g^{0.9}/22.0)]\f$
      !!
C
C     * RAINFALL/SNOWFALL RATES AND OTHER INITIALIZATION PROCEDURES
C     * OVER GRID CELL SUBAREAS. DOWNWARD WATER FLUXES ARE LUMPED
C     * TOGETHER WITH PRECIPITATION, AND UPWARD AND DOWNWARD WATER
C     * FLUXES CANCEL OUT. CORRECTION MADE TO SOIL SATURATED HYDRAULIC
C     * CONDUCTIVITY FOR WATER VISCOSITY EFFECTS. 
C
C     * CALCULATIONS FOR CANOPY OVER SNOW.
C
      IF(NLANDCS.GT.0)                                              THEN
C
          DO 200 J=1,IG
          DO 200 I=IL1,IL2
              IF(FCS(I).GT.0.)                                THEN 
                  THLQCS(I,J)=THLIQC(I,J)                                               
                  THICCS(I,J)=THICEC(I,J)                                               
                  HCPCS (I,J)=HCPC  (I,J)
                  IF(THPOR(I,J).GT.0.0001)               THEN
                      GRKSCS(I,J)=GRKSAT(I,J)*(1.7915E-03/
     1                    (2.0319E-04+1.5883E-03*EXP(-((MAX(0.0,
     2                    MIN(100.,TBARCS(I,J)))**0.9)/22.))))
                  ELSE
                      GRKSCS(I,J)=GRKSAT(I,J)
                  ENDIF
              ENDIF                                                  
  200     CONTINUE
C
          !>
          !!Next, a preliminary calculation of the water vapour flux 
          !!diagnostics for each subarea is carried out. Over the 
          !!vegetated subareas, first the canopy evaporative flux is 
          !!assigned to sublimation if there is snow present on the 
          !!canopy (since it is assumed that any water present exists 
          !!within or underneath the snow). Then, if the sublimation rate 
          !!is upward, it is assigned to the diagnostic variable QFCF; if 
          !!it is downward, the portion of the flux corresponding to the 
          !!canopy-covered area is assigned to QFCF and that 
          !!corresponding to the gap area to QFN. Similarly, if the 
          !!evaporation rate is upward it is assigned to the diagnostic 
          !!variable QFCL; if it is downward, the flux corresponding to 
          !!the canopy-covered area is assigned to QFCL and that 
          !!corresponding to the gap area to QFN over subarea FCS and to 
          !!QFG over subarea FC. Over the non-vegetated subareas, the 
          !!evaporation rates are assigned to QFN for subarea GS, and to 
          !!QFG for subarea G.
          !!
          !!For the purposes of the subsequent water balance calculations 
          !!done in the other CLASSW subroutines, the subarea snowfall is 
          !!lumped together with any simultaneously occurring 
          !!sublimation, and the subarea rainfall with any simultaneously 
          !!occurring evaporation. Depending on whether the sum of the 
          !!snowfall and the sublimation, and the sum of the rainfall and 
          !!the evaporation, are positive (downward) or negative 
          !!(upward), corrections are applied to the appropriate 
          !!diagnostic variables, and the snowfall/rainfall are set to 
          !!the net flux if downward and the sublimation/evaporation are 
          !!set to the net flux if upward. The smaller of the two fluxes 
          !!in the sums are set to zero.
          !!
          !!Finally, ponded water and snow pack physical characteristics 
          !!are set, including the snow heat capacity, which is 
          !!calculated from the heat capacities of ice and water \f$C_i\f$ 
          !!and \f$C_w\f$, the snow, ice and water densities \f$\rho_s\f$ \f$\rho_i\f$, and 
          !!\f$\rho_w\f$, and the water content and depth of the snow pack \f$w_s\f$ 
          !!and \f$z_s\f$, as follows:
          !!
          !!\f$C_s = C_i [\rho_s /\rho_i] + C_w w_s /[\rho_w z_s]\f$
          !!
          DO 250 I=IL1,IL2
              IF(FCS(I).GT.0.)                           THEN  
                  IF(SNOCNS(I).GT.0.)      THEN                                                  
                      SUBLCS(I)=EVAPCS(I)
                      EVAPCS(I)=0.0
                  ELSE                                                                    
                      SUBLCS(I)=0.0                                                          
                  ENDIF
                  IF(SUBLCS(I).GT.0.0) THEN
                      QFCF(I)=QFCF(I)+FCS(I)*SUBLCS(I)*RHOW
                  ELSE
                      QFCF(I)=QFCF(I)+FCS(I)*(1.0-FSVFS(I))*SUBLCS(I)*
     1                        RHOW
                      QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*SUBLCS(I)*RHOW
                  ENDIF
                  IF(EVAPCS(I).GT.0.0) THEN
                      QFCL(I)=QFCL(I)+FCS(I)*EVAPCS(I)*RHOW
                  ELSE
                      QFCL(I)=QFCL(I)+FCS(I)*(1.0-FSVFS(I))*EVAPCS(I)*
     1                        RHOW
                      QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*EVAPCS(I)*RHOW
                  ENDIF
C
                  IF(SPCP(I).GT.0. .OR. SUBLCS(I).LT.0.) THEN                                      
                      SADD(I)=SPCP(I)-SUBLCS(I)*RHOW/RHOSNI(I)
                      IF(ABS(SADD(I)).LT.1.0E-12) SADD(I)=0.0
                      IF(SADD(I).GT.0.0) THEN                                                
                          IF(SUBLCS(I).GT.0.) THEN
                              QFCF(I)=QFCF(I)-FCS(I)*FSVFS(I)*
     1                                SUBLCS(I)*RHOW
                              QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*
     1                                SUBLCS(I)*RHOW
                          ENDIF
                          SPCCS (I)=SADD(I)                                                        
                          IF(SPCP(I).GT.0.0) THEN
                              TSPCCS(I)=TSPCP(I)+TFREZ                                                   
                          ELSE
                              TSPCCS(I)=MIN(TSURX(I,1),TFREZ)
                          ENDIF
                          SUBLCS(I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FCS(I)*FSVFS(I)*SPCP(I)*
     1                        RHOSNI(I)
                          PCFC(I)=PCFC(I)+FCS(I)*FSVFS(I)*SPCP(I)*
     1                        RHOSNI(I)
                          SUBLCS(I)=-SADD(I)*RHOSNI(I)/RHOW                                        
                          SPCCS (I)=0.0                                                         
                          TSPCCS(I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      SPCCS(I)=0.0                                                             
                      TSPCCS(I)=0.0                                                            
                  ENDIF
C
                  IF(RPCP(I).GT.0. .OR. EVAPCS(I).LT.0.) THEN                                      
                      RADD(I)=RPCP(I)-EVAPCS(I)                                                       
                      IF(ABS(RADD(I)).LT.1.0E-12) RADD(I)=0.0
                      IF(RADD(I).GT.0.)   THEN                                                
                          IF(EVAPCS(I).GT.0.) THEN
                              QFCL(I)=QFCL(I)-FCS(I)*FSVFS(I)*
     1                                EVAPCS(I)*RHOW
                              QFN(I)=QFN(I)+FCS(I)*FSVFS(I)*
     1                                EVAPCS(I)*RHOW
                          ENDIF
                          RPCCS (I)=RADD(I)                                                        
                          IF(RPCP(I).GT.0.0) THEN
                              TRPCCS(I)=TRPCP(I)+TFREZ                                                   
                          ELSE
                              TRPCCS(I)=MAX(TSURX(I,1),TFREZ)
                          ENDIF
                          EVAPCS(I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FCS(I)*FSVFS(I)*RPCP(I)*RHOW
                          PCLC(I)=PCLC(I)+FCS(I)*FSVFS(I)*RPCP(I)*RHOW
                          EVAPCS(I)=-RADD(I)                                                    
                          RPCCS (I)=0.0                                                         
                          TRPCCS(I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RPCCS(I)=0.0                                                             
                      TRPCCS(I)=0.0                                                            
                  ENDIF                                                                   
                  ZPNDCS(I)=ZPOND (I)                                                            
                  ZSNOCS(I)=ZSNOW (I)                                                            
                  ALBSCS(I)=ALBSNO(I)                                                           
                  HCPSCS(I)=HCPICE*RHOSCS(I)/RHOICE+HCPW*WSNOCS(I)/
     1                      (RHOW*ZSNOCS(I))
                  QFN   (I)=QFN(I)+FCS(I)*EVPCSG(I)*RHOW
              ENDIF
  250     CONTINUE
      ENDIF
C
C     * CALCULATIONS FOR SNOW-COVERED GROUND.
C
      IF(NLANDGS.GT.0)                                              THEN
C
          DO 300 J=1,IG
          DO 300 I=IL1,IL2
              IF(FGS(I).GT.0.)                                 THEN 
                  THLQGS(I,J)=THLIQG(I,J)                                               
                  THICGS(I,J)=THICEG(I,J)                                               
                  HCPGS (I,J)=HCPG  (I,J)
                  IF(THPOR(I,J).GT.0.0001)               THEN
                      GRKSGS(I,J)=GRKSAT(I,J)*(1.7915E-03/
     1                    (2.0319E-04+1.5883E-03*EXP(-((MAX(0.0,
     2                    MIN(100.,TBARGS(I,J)))**0.9)/22.))))
                  ELSE
                      GRKSGS(I,J)=GRKSAT(I,J)
                  ENDIF
              ENDIF                                                  
  300     CONTINUE
C
          DO 350 I=IL1,IL2
              IF(FGS(I).GT.0.)                              THEN 
                  QFN(I)=QFN(I)+FGS(I)*EVAPGS(I)*RHOW
                  IF(SPCP(I).GT.0. .OR. EVAPGS(I).LT.0.) THEN                                      
                      SADD(I)=SPCP(I)-EVAPGS(I)*RHOW/RHOSNI(I)
                      IF(ABS(SADD(I)).LT.1.0E-12) SADD(I)=0.0
                      IF(SADD(I).GT.0.0) THEN                                                
                          SPCGS (I)=SADD(I)                                                        
                          IF(SPCP(I).GT.0.0) THEN
                              TSPCGS(I)=TSPCP(I)
                          ELSE
                              TSPCGS(I)=MIN((TSURX(I,2)-TFREZ),0.0)
                          ENDIF
                          EVAPGS(I)=0.0                                                      
                      ELSE                                                                
                          EVAPGS(I)=-SADD(I)*RHOSNI(I)/RHOW                                        
                          SPCGS (I)=0.0                                                         
                          TSPCGS(I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      SPCGS (I)=0.0                                                             
                      TSPCGS(I)=0.0                                                            
                  ENDIF
C
                  IF(RPCP(I).GT.0.)                         THEN                                      
                      RADD(I)=RPCP(I)-EVAPGS(I)                                                       
                      IF(ABS(RADD(I)).LT.1.0E-12) RADD(I)=0.0
                      IF(RADD(I).GT.0.)   THEN                                                
                          RPCGS (I)=RADD(I)                                                        
                          TRPCGS(I)=TRPCP(I)
                          EVAPGS(I)=0.0                                                      
                      ELSE                                                                
                          EVAPGS(I)=-RADD(I)                                                    
                          RPCGS (I)=0.0                                                         
                          TRPCGS(I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RPCGS (I)=0.0                                                             
                      TRPCGS(I)=0.0                                                            
                  ENDIF                                                                   
                  ZPNDGS(I)=ZPOND (I)                                                            
                  ZSNOGS(I)=ZSNOW (I)                                                            
                  ALBSGS(I)=ALBSNO(I)                                                           
                  HCPSGS(I)=HCPICE*RHOSGS(I)/RHOICE+HCPW*WSNOGS(I)/
     1                      (RHOW*ZSNOGS(I))
              ENDIF
  350     CONTINUE
      ENDIF
C
C     * CALCULATIONS FOR CANOPY OVER BARE GROUND.
C
      IF(NLANDC.GT.0)                                               THEN
C
          DO 400 J=1,IG
          DO 400 I=IL1,IL2
              IF(FC(I).GT.0.)                                 THEN  
                  THLQCO(I,J)=THLIQC(I,J)                                               
                  THICCO(I,J)=THICEC(I,J)                                               
                  HCPCO (I,J)=HCPC  (I,J)
                  IF(THPOR(I,J).GT.0.0001)               THEN
                      GRKSC (I,J)=GRKSAT(I,J)*(1.7915E-03/
     1                    (2.0319E-04+1.5883E-03*EXP(-((MAX(0.0,
     2                    MIN(100.,TBARC(I,J)))**0.9)/22.))))
                  ELSE
                      GRKSC (I,J)=GRKSAT(I,J)
                  ENDIF
              ENDIF                                                  
  400     CONTINUE
C
          DO 450 I=IL1,IL2
              IF(FC(I).GT.0.)                            THEN 
                  IF(SNOCAN(I).GT.0.)      THEN                                                  
                      SUBLC(I)=EVAPC(I)
                      EVAPC(I)=0.0
                  ELSE                                                                    
                      SUBLC(I)=0.0                                                          
                  ENDIF
                  IF(SUBLC(I).GT.0.0) THEN
                      QFCF(I)=QFCF(I)+FC(I)*SUBLC(I)*RHOW
                  ELSE
                      QFCF(I)=QFCF(I)+FC(I)*(1.0-FSVF(I))*SUBLC(I)*
     1                        RHOW
                      QFN(I)=QFN(I)+FC(I)*FSVF(I)*SUBLC(I)*RHOW
                  ENDIF
                  IF(EVAPC(I).GT.0.0) THEN
                      QFCL(I)=QFCL(I)+FC(I)*EVAPC(I)*RHOW
                  ELSE
                      QFCL(I)=QFCL(I)+FC(I)*(1.0-FSVF(I))*EVAPC(I)*
     1                        RHOW
                      QFG(I)=QFG(I)+FC(I)*FSVF(I)*EVAPC(I)*RHOW
                  ENDIF
C
                  IF(SPCP(I).GT.0. .OR. SUBLC(I).LT.0.)  THEN                                      
                      SADD(I)=SPCP(I)-SUBLC(I)*RHOW/RHOSNI(I)
                      IF(ABS(SADD(I)).LT.1.0E-12) SADD(I)=0.0
                      IF(SADD(I).GT.0.0) THEN                                                
                          IF(SUBLC(I).GT.0.) THEN
                              QFCF(I)=QFCF(I)-FC(I)*FSVF(I)*SUBLC(I)*
     1                                RHOW
                              QFN(I)=QFN(I)+FC(I)*FSVF(I)*SUBLC(I)*
     1                                RHOW
                          ENDIF
                          SPCC  (I)=SADD(I)                                                        
                          IF(SPCP(I).GT.0.0) THEN
                             TSPCC (I)=TSPCP(I)+TFREZ                                                   
                          ELSE
                             TSPCC(I)=MIN(TSURX(I,3),TFREZ)
                          ENDIF
                          SUBLC (I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FC(I)*FSVF(I)*SPCP(I)*
     1                        RHOSNI(I)
                          PCFC(I)=PCFC(I)+FC(I)*FSVF(I)*SPCP(I)*
     1                        RHOSNI(I)
                          SUBLC (I)=-SADD(I)*RHOSNI(I)/RHOW                                        
                          SPCC  (I)=0.0                                                         
                          TSPCC (I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      SPCC  (I)=0.0                                                             
                      TSPCC (I)=0.0                                                            
                  ENDIF
C
                  IF(RPCP(I).GT.0. .OR. EVAPC(I).LT.0.)  THEN                                      
                      RADD(I)=RPCP(I)-EVAPC(I)                                                       
                      IF(ABS(RADD(I)).LT.1.0E-12) RADD(I)=0.0
                      IF(RADD(I).GT.0.)   THEN                                                
                          IF(EVAPC(I).GT.0.) THEN
                              QFCL(I)=QFCL(I)-FC(I)*FSVF(I)*EVAPC(I)*
     1                                RHOW
                              QFG(I)=QFG(I)+FC(I)*FSVF(I)*EVAPC(I)*
     1                                RHOW
                          ENDIF
                          RPCC  (I)=RADD(I)                                                        
                          IF(RPCP(I).GT.0.0) THEN
                              TRPCC (I)=TRPCP(I)+TFREZ  
                          ELSE
                              TRPCC (I)=MAX(TSURX(I,3),TFREZ)
                          ENDIF
                          EVAPC (I)=0.0                                                      
                      ELSE   
                          PCPG(I)=PCPG(I)-FC(I)*FSVF(I)*RPCP(I)*RHOW
                          PCLC(I)=PCLC(I)+FC(I)*FSVF(I)*RPCP(I)*RHOW
                          EVAPC (I)=-RADD(I)                                                    
                          RPCC  (I)=0.0                                                         
                          TRPCC (I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RPCC  (I)=0.0                                                             
                      TRPCC (I)=0.0                                                            
                  ENDIF         
                  ZPONDC(I)=ZPOND (I)                                                            
                  ZSNOWC(I)=0.
                  RHOSC (I)=0.
                  HCPSC (I)=0.
                  QFG   (I)=QFG(I)+FC(I)*EVAPCG(I)*RHOW
              ENDIF
  450     CONTINUE
      ENDIF
C
C     * CALCULATIONS FOR BARE GROUND.
C
      IF(NLANDG.GT.0)                                               THEN
C
          DO 500 J=1,IG
          DO 500 I=IL1,IL2
              IF(FG(I).GT.0.)                                 THEN 
                  THLQGO(I,J)=THLIQG(I,J)                                               
                  THICGO(I,J)=THICEG(I,J)                                               
                  HCPGO (I,J)=HCPG  (I,J)
                  IF(THPOR(I,J).GT.0.0001)               THEN
                      GRKSG (I,J)=GRKSAT(I,J)*(1.7915E-03/
     1                    (2.0319E-04+1.5883E-03*EXP(-((MAX(0.0,
     2                    MIN(100.,TBARG(I,J)))**0.9)/22.))))
                  ELSE
                      GRKSG (I,J)=GRKSAT(I,J)
                  ENDIF
              ENDIF                                                  
  500     CONTINUE
C
          DO 550 I=IL1,IL2
              IF(FG(I).GT.0.)                              THEN 
                  QFG(I)=QFG(I)+FG(I)*EVAPG(I)*RHOW
                  IF(SPCP(I).GT.0.)                 THEN                                      
                      SADD(I)=SPCP(I)-EVAPG(I)*RHOW/RHOSNI(I)
                      IF(ABS(SADD(I)).LT.1.0E-12) SADD(I)=0.0
                      IF(SADD(I).GT.0.0) THEN                                                
                          QFN(I)=QFN(I)+FG(I)*EVAPG(I)*RHOW
                          QFG(I)=QFG(I)-FG(I)*EVAPG(I)*RHOW
                          SPCG  (I)=SADD(I)                                                        
                          TSPCG (I)=TSPCP(I)
                          EVAPG (I)=0.0                                                      
                      ELSE                                                                
                          PCPN(I)=PCPN(I)-FG(I)*SPCP(I)*RHOSNI(I)
                          PCPG(I)=PCPG(I)+FG(I)*SPCP(I)*RHOSNI(I)
                          EVAPG (I)=-SADD(I)*RHOSNI(I)/RHOW                                        
                          SPCG  (I)=0.0                                                         
                          TSPCG (I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      SPCG  (I)=0.0                                                             
                      TSPCG (I)=0.0                                                            
                  ENDIF
C
                  IF(RPCP(I).GT.0. .OR. EVAPG(I).LT.0.)   THEN                                      
                      RADD(I)=RPCP(I)-EVAPG(I)                                                       
                      IF(ABS(RADD(I)).LT.1.0E-12) RADD(I)=0.0
                      IF(RADD(I).GT.0.)   THEN                                                
                          RPCG  (I)=RADD(I)                                                        
                          IF(RPCP(I).GT.0.0) THEN
                              TRPCG (I)=TRPCP(I)
                          ELSE
                              TRPCG (I)=MAX((TSURX(I,4)-TFREZ),0.0)
                          ENDIF
                          EVAPG (I)=0.0                                                      
                      ELSE                                                                
                          EVAPG (I)=-RADD(I)                                                    
                          RPCG  (I)=0.0                                                         
                          TRPCG (I)=0.0                                                        
                      ENDIF                                                               
                  ELSE                                                                    
                      RPCG (I)=0.0                                                             
                      TRPCG(I)=0.0                                                            
                  ENDIF     
                  ZPONDG(I)=ZPOND (I)                                                            
                  ZSNOWG(I)=0.
                  RHOSG (I)=0.
                  HCPSG (I)=0.
              ENDIF
  550     CONTINUE
      ENDIF
C
      RETURN                                                                      
      END
