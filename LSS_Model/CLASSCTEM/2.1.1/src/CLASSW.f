!>\file
!!Purpose: Call subroutines to perform surface water budget calculations
!!
      SUBROUTINE CLASSW(THLIQ,  THICE,  TBAR,   TCAN,   RCAN,   SNCAN,
     1                  RUNOFF, TRUNOF, SNO,    TSNOW,  RHOSNO, ALBSNO, 
     2                  WSNOW,  ZPOND,  TPOND,  GROWTH, FRZC,   TBASE,
     3                  GFLUX,
     4                  PCFC,   PCLC,   PCPN,   PCPG,   QFCF,   QFCL,
     5                  QFN,    QFG,    QFC,    HMFC,   HMFG,   HMFN,
     6                  HTCC,   HTCS,   HTC,    ROFC,   ROFN,   ROVG, 
     7                  WTRS,   WTRG,   OVRFLW, SUBFLW, BASFLW, 
     8                  TOVRFL, TSUBFL, TBASFL, EVAP,   QFLUX,  RHOAIR, 
     9                  TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG, 
     A                  THICEC, THICEG, HCPC,   HCPG,   RPCP,   TRPCP,  
     B                  SPCP,   TSPCP,  PCPR,   TA,     RHOSNI, GGEO,
     C                  FC,     FG,     FCS,    FGS,    TPONDC, TPONDG,
     D                  TPNDCS, TPNDGS, EVAPC,  EVAPCG, EVAPG,  EVAPCS,
     E                  EVPCSG, EVAPGS, QFREZC, QFREZG, QMELTC, QMELTG,
     F                  RAICAN, SNOCAN, RAICNS, SNOCNS, FSVF,   FSVFS,  
     G                  CWLCAP, CWFCAP, CWLCPS, CWFCPS, TCANO,  
     H                  TCANS,  CHCAP,  CHCAPS, CMASSC, CMASCS, ZSNOW,  
     I                  GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,
     J                  G12CS,  G12GS,  G23C,   G23G,   G23CS,  G23GS,
     K                  TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     L                  ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, TSFSAV,
     M                  TCTOPC, TCBOTC, TCTOPG, TCBOTG, FROOT,  FROOTS,
     N                  THPOR,  THLRET, THLMIN, BI,     PSISAT, GRKSAT,
     O                  THLRAT, THFC,   XDRAIN, HCPS, DELZ,
     P                  DELZW,  ZBOTW,  XSLOPE, GRKFAC, WFSURF, WFCINT,
     Q                  ISAND,  IGDR,
     R                  IWF,    ILG,    IL1,    IL2,    N,
     S                  JL,     IC,     IG,     IGP1,   IGP2,
     T                  NLANDCS,NLANDGS,NLANDC, NLANDG, NLANDI, 
     U                  MANNING_N, DD,  NCOUNT, T0_ACC,
     V                  SI, TSI, INFILTYPE, SNOWMELTD, SNOWMELTD_LAST,
     W                  MELTRUNOFF, SNOWINFIL,
     X                  CUMSNOWINFILCS, CUMSNOWINFILGS,
     Y                  SOIL_POR_MAX, SOIL_DEPTH, S0, T_ICE_LENS,
     Z                  NA, NTYPE, ILMOS, JLMOS,
     1                  BTC, BCAP, DCOEFF, BFCAP, BFCOEFF, BFMIN, BQMAX,
     2                  CMIN,  CMAX,    B,      K1,     K2,
     3                  ZPNDPRECS, ZPONDPREC, ZPONDPREG, ZPNDPREGS,
     4                  UM1CS,     UM1C,      UM1G,      UM1GS,
     5                  QM1CS,     QM1C,      QM1G,      QM1GS,
     6                  QM2CS,     QM2C,      QM2G,      QM2GS,  UMQ,
     7                  FSTRCS,    FSTRC,     FSTRG,     FSTRGS,
     8                  ZSNOCS,    ZSNOGS,    ZSNOWC,    ZSNOWG,
     9                  HCPSCS,    HCPSGS,    HCPSC,     HCPSG,
     A                  TSNOWC,    TSNOWG,    RHOSC,     RHOSG,
     B                  XSNOWC,    XSNOWG,    XSNOCS,    XSNOGS)

C     * AUG 04/15 - M.LAZARE.   SPLIT FROOT INTO TWO ARRAYS, FOR CANOPY
C     *                         AREAS WITH AND WITHOUT SNOW.
C     * OCT 03/14 - D.VERSEGHY. CHANGE LIMITING VALUE OF SNOW PACK
C     *                         FROM 100 KG/M2 TO 10 M.
C     * AUG 19/13 - M.LAZARE.   ADD CALCULATION OF "QFLUX" (NOW PASSED  
C     *                         IN ALONG WITH "RHOAIR") PREVIOUSLY DONE 
C     *                         IN CLASST.                              
C     * JUN 21/13 - M.LAZARE.   SET ZSNOW=0. IF THERE IS NO             
C     *                         SNOW IN ANY OF THE 4 SUBCLASSES,        
C     *                         SIMILAR TO WHAT IS DONE FOR THE         
C     *                         OTHER SNOW-RELATED FIELDS. 
C     * OCT 18/11 - M.LAZARE.   PASS IN IGDR THROUGH CALLS TO
C     *                         GRDRAN/GRINFL (ORIGINATES NOW
C     *                         IN CLASSB - ONE CONSISTENT
C     *                         CALCULATION).                                                                          
C     * APR 04/11 - D.VERSEGHY. ADD DELZ TO GRINFL CALL.
C     * DEC 07/09 - D.VERSEGHY. ADD RADD AND SADD TO WPREP CALL.
C     * JAN 06/09 - D.VERSEGHY. INCREASE LIMITING SNOW AMOUNT.
C     * FEB 25/08 - D.VERSEGHY. MODIFICATIONS REFLECTING CHANGES
C     *                         ELSEWHERE IN CODE.
C     * MAR 23/06 - D.VERSEGHY. CHANGES TO ADD MODELLING OF WSNOW;
C     *                         PASS IN GEOTHERMAL HEAT FLUX.
C     * MAR 21/06 - P.BARTLETT. PASS ADDITIONAL VARIABLES TO WPREP.
C     * DEC 07/05 - D.VERSEGHY. REVISIONS TO CALCULATION OF TBASE.
C     * OCT 05/05 - D.VERSEGHY. MODIFICATIONS TO ALLOW OPTION OF SUB-
C     *                         DIVIDING THIRD SOIL LAYER.
C     * MAR 23/05 - D.VERSEGHY. ADD VARIABLES TO SUBROUTINE CALLS.
C     * MAR 14/05 - D.VERSEGHY. RENAME SCAN TO SNCAN (RESERVED NAME
C     *                         IN F90).
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 08/04 - D.VERSEGHY. NEW LOWER LIMITS FOR RCAN, SCAN, ZPOND
C     *                         AND SNOW.
C     * DEC 09/02 - D.VERSEGHY. SWITCH CALLING ORDER OF TFREEZ AND
C     *                         SNOVAP FOR CONSISTENCY WITH DIAGNOSTICS.
C     * SEP 26.02 - D.VERSEGHY. CHANGED CALL TO SUBCAN.
C     * AUG 01/02 - D.VERSEGHY. ADD CALL TO WATROF, NEW SUBROUTINE
C     *                         CONTAINING WATERLOO OVERLAND FLOW
C     *                         AND INTERFLOW CALCULATIONS.
C     *                         SHORTENED CLASS3 COMMON BLOCK.
C     * JUL 03/02 - D.VERSEGHY. STREAMLINE SUBROUTINE CALLS; MOVE 
C     *                         CALCULATION OF BACKGROUND SOIL 
C     *                         PROPERTIES INTO "CLASSB"; CHANGE
C     *                         RHOSNI FROM CONSTANT TO VARIABLE.
C     * OCT 04/01 - M.LAZARE.   REMOVE SEVERAL OLD DIAGNOSTIC FIELDS
C     *                         AND ADD NEW FIELD "ROVG".
C     * MAY 14/01 - M.LAZARE.   ADD CALLS TO SUBROUTINE "SNOVAP" FOR
C     *                         FC AND FG SUBAREAS OF GRID CELL.
C     * OCT 20/00 - D.VERSEGHY. ADD WORK ARRAY "RHOMAX" FOR SNOALBW.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CHANGES RELATED TO VARIABLE SOIL DEPTH
C     *                         (MOISTURE HOLDING CAPACITY) AND DEPTH-
C     *                         VARYING SOIL PROPERTIES.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS; INTRODUCE CALCULATION OF
C     *                         OVERLAND FLOW.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4. 
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 24/95 - D.VERSEGHY. UPDATE ARRAY "EVAP" TO TAKE INTO 
C     *                         ACCOUNT "WLOST"; RATIONALIZE 
C     *                         CALCULATION OF THE LATTER.
C     *                         COMPLETION OF WATER BUDGET DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL 
C     *                         ORGANIC MATTER CONTENT.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         CHANGES TO SUBROUTINE CALLS ASSOCIATED
C     *                         WITH REVISIONS TO DIAGNOSTICS.
C     *                         ALLOW SPECIFICATION OF LIMITING POND
C     *                         DEPTH "PNDLIM" (PARALLEL CHANGES MADE
C     *                         SIMULTANEOUSLY IN TMCALC).
C     * DEC 16/94 - D.VERSEGHY. TWO NEW DIAGNOSTIC FIELDS.
C     * NOV 18/93 - D.VERSEGHY. LOCAL VERSION WITH INTERNAL WORK ARRAYS
C     *                         HARD-CODED FOR USE ON PCS.
C     * NOV 01/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REVISIONS ASSOCIATED WITH NEW VERSION
C     *                         OF TMCALC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. NUMEROUS NEW DIAGNOSTIC FIELDS.
C     * MAY 06/93 - D.VERSEGHY/M.LAZARE. CORRECT BUG IN CALL TO TMCALC
C     *                                  FOR CANOPY-SNOW CASE, WHERE
C     *                                  SHOULD BE PASSING "HCPCS"
C     *                                  INSTEAD OF "HCPGS". 
C     * MAY 15/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C                               CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. LAND SURFACE WATER BUDGET CALCULATIONS.
C                                                                                 
      IMPLICIT NONE

C     * INTEGER CONSTANTS.
C
      INTEGER IWF   !<Flag governing lateral soil water flow calculations
      INTEGER ILG,IL1,IL2,JL,IC,IG,IGP1,IGP2,I,J
      INTEGER NLANDCS   !<Number of modelled areas that contain subareas of canopy over snow
      INTEGER NLANDGS   !<Number of modelled areas that contain subareas of snow
      INTEGER NLANDC    !<Number of modelled areas that contain subareas of canopy over bare ground
      INTEGER NLANDG    !<Number of modelled areas that contain subareas of bare ground
      INTEGER NLANDI    !<Number of modelled areas that are ice sheets [ ]
      INTEGER IPTBAD,JPTBAD,KPTBAD,LPTBAD,N
C
C     * MAIN OUTPUT FIELDS.
C                                                                                  
      REAL THLIQ (ILG,IG)   !<Volumetric liquid water content of soil layers \f$[m^3 m^{-3}]\f$
      REAL THICE (ILG,IG)   !<Volumetric frozen water content of soil layers \f$[m^3 m^{-3}]\f$
      REAL TBAR  (ILG,IG)   !<Temperature of soil layers [K]
      REAL GFLUX (ILG,IG)   !<Heat flux at interfaces between soil layers \f$[W m^{-2}]\f$
C
      REAL TCAN  (ILG)  !<Vegetation canopy temperature [K]   
      REAL RCAN  (ILG)  !<Intercepted liquid water stored on canopy \f$[kg m^{-2}]\f$
      REAL SNCAN (ILG)  !<Intercepted frozen water stored on canopy \f$[kg m^{-2}]\f$
      REAL RUNOFF(ILG)  !<Total runoff from soil \f$[m or kg m^{-2} s^{-1}]\f$
      REAL SNO   (ILG)  !<Mass of snow pack \f$[kg m^{-2}]\f$  
      REAL TSNOW (ILG)  !<Snowpack temperature [K]  
      REAL RHOSNO(ILG)  !<Density of snow \f$[kg m^{-3}]\f$  
      REAL ALBSNO(ILG)  !<Snow albedo [ ]
      REAL ZPOND (ILG)  !<Depth of ponded water on surface [m]
      REAL TPOND (ILG)  !<Temperature of ponded water [K]
      REAL GROWTH(ILG)  !<Vegetation growth index [ ]  
      REAL TBASE (ILG)  !<Temperature of bedrock in third soil layer [K]
      REAL TRUNOF(ILG)  !<Temperature of total runoff [K]  
      REAL WSNOW (ILG)  !<Liquid water content of snow pack \f$[kg m^{-2}]\f$
C
C     * DIAGNOSTIC ARRAYS.
C
      REAL PCFC  (ILG)  !<Frozen precipitation intercepted by vegetation 
                        !<\f$[kg m^{-2} s^{-1}]\f$
      REAL PCLC  (ILG)  !<Liquid precipitation intercepted by vegetation 
                        !<\f$[kg m^{-2} s^{-1}]\f$
      REAL PCPN  (ILG)  !<Precipitation incident on snow pack 
                        !<\f$[kg m^{-2} s^{-1}]\f$
      REAL PCPG  (ILG)  !<Precipitation incident on ground \f$[kg m^{-2} s^{-1}]\f$  
      REAL QFCF  (ILG)  !<Sublimation from frozen water on vegetation 
                        !<\f$[kg m^{-2} s^{-1}]\f$
      REAL QFCL  (ILG)  !<Evaporation from liquid water on vegetation 
                        !<\f$[kg m^{-2} s^{-1}]\f$
      REAL QFN   (ILG)  !<Sublimation from snow pack \f$[kg m^{-2} s^{-1}]\f$  
      REAL QFG   (ILG)  !<Evaporation from ground \f$[kg m^{-2} s^{-1}]\f$  
      REAL HMFC  (ILG)  !<Diagnosed energy associated with phase change 
                        !<of water on vegetation \f$[W m^{-2}]\f$
      REAL HMFN  (ILG)  !<Diagnosed energy associated with phase change 
                        !<of water in snow pack \f$[W m^{-2}]\f$
      REAL HTCC  (ILG)  !<Diagnosed internal energy change of vegetation 
                        !<canopy due to conduction and/or change in mass \f$[W m^{-2}]\f$
      REAL HTCS  (ILG)  !<Diagnosed internal energy change of snow pack 
                        !<due to conduction and/or change in mass \f$[W m^{-2}]\f$
      REAL ROFC  (ILG)  !<Liquid/frozen water runoff from vegetation 
                        !<\f$[kg m^{-2} s^{-1}]\f$
      REAL ROFN  (ILG)  !<Liquid water runoff from snow pack \f$[kg m^{-2} s^{-1}]\f$  
      REAL ROVG  (ILG)  !<Liquid/frozen water runoff from vegetation to 
                        !<ground surface \f$[kg m^{-2} s^{-1}]\f$
      REAL WTRS  (ILG)  !<Diagnosed residual water transferred into or 
                        !<out of the snow pack \f$[kg m^{-2} s^{-1}]\f$
      REAL WTRG  (ILG)  !<Diagnosed residual water transferred into or 
                        !<out of the soil \f$[kg m^{-2} s^{-1}]\f$
      REAL OVRFLW(ILG)  !<Overland flow from top of soil column 
                        !\f$[m or kg m^{-2} s^{-1}]\f$  
      REAL SUBFLW(ILG)  !<Interflow from sides of soil column 
                        !<\f$[kg m^{-2} s^{-1}]\f$  
      REAL BASFLW(ILG)  !<Base flow from bottom of soil column 
                        !<\f$[m or kg m^{-2} s^{-1}]\f$
      REAL TOVRFL(ILG)  !<Temperature of overland flow from top of soil column [K]
      REAL TSUBFL(ILG)  !<Temperature of interflow from sides of soil column [K]
      REAL TBASFL(ILG)  !<Temperature of base flow from bottom of soil column [K]
      REAL EVAP  (ILG)  !<Diagnosed total surface water vapour flux over modelled area \f$[kg m^{-2} s^{-1}]\f$
      REAL QFLUX (ILG)  !<
      REAL RHOAIR (ILG) !<
C
      REAL QFC  (ILG,IG) !<Water removed from soil layers by transpiration \f$[kg m^{-2} s^{-1}]\f$
      REAL HMFG (ILG,IG) !<Diagnosed energy associated with phase change of water in soil layers \f$[W m^{-2}]\f$
      REAL HTC  (ILG,IG) !<Diagnosed internal energy change of soil layer due to conduction and/or change in mass \f$[W m^{-2}]\f$
C
C     * I/O FIELDS PASSED THROUGH CLASS.
C

      !(In composite definitions, suffix C or CO = vegetation over 
      !ground; G or GO = bare ground; CS = vegetation over snow cover; 
      !GS = bare snow cover.)
C
      REAL RPCP  (ILG)  !<Rainfall rate over modelled area \f$[m s^{-1}]\f$
      REAL TRPCP (ILG)  !<Rainfall temperature over modelled area [C] 
      REAL SPCP  (ILG)  !<Snowfall rate over modelled area \f$[m s^{-1}]\f$ 
      REAL TSPCP (ILG)  !<Snowfall temperature over modelled area [C]
      REAL PCPR  (ILG)  !<Surface precipitation rate \f$[kg m^{-2} s^{-1}]\f$ 
      REAL TA    (ILG)  !<Air temperature at reference height [K]
C
      REAL TBARC(ILG,IG) !<Subarea temperatures of soil layers [C]
      REAL TBARG(ILG,IG) !<Subarea temperatures of soil layers [C]
      REAL TBARCS(ILG,IG) !<Subarea temperatures of soil layers [C]
      REAL TBARGS(ILG,IG) !<Subarea temperatures of soil layers [C]
C                            
      REAL THLIQC(ILG,IG)   !<Liquid water content of soil layers under vegetation \f$[m^3 m^{-3}]\f$
      REAL THLIQG(ILG,IG)   !<Liquid water content of soil layers in bare areas \f$[m^3 m^{-3}]\f$
      REAL THICEC(ILG,IG)   !<Frozen water content of soil layers under vegetation \f$[m^3 m^{-3}]\f$
      REAL THICEG(ILG,IG)   !<Frozen water content of soil layers in bare areas \f$[m^3 m^{-3}]\f$                
      REAL HCPC  (ILG,IG)   !<Heat capacity of soil layers under vegetation \f$[J m^{-3} K^{-1}]\f$
      REAL HCPG  (ILG,IG)   !<Heat capacity of soil layers in bare areas \f$[J m^{-3} K^{-1}]\f$
      REAL TCTOPC(ILG,IG)   !<Thermal conductivity of soil at top of layer (vegetation over ground) \f$[W m^{-1} K^{-1}]\f$
      REAL TCBOTC(ILG,IG)   !<Thermal conductivity of soil at bottom of layer (vegetation over ground) \f$[W m^{-1} K^{-1}]\f$    
      REAL TCTOPG(ILG,IG)   !<Thermal conductivity of soil at top of layer (bare ground) \f$[W m^{-1} K^{-1}]\f$
      REAL TCBOTG(ILG,IG)   !<Thermal conductivity of soil at bottom of layer (bare ground) \f$[W m^{-1} K^{-1}]\f$
      REAL FROOT (ILG,IG)   !<Fraction of total transpiration contributed by soil layer [ ]
      REAL FROOTS (ILG,IG)  !<Fraction of total transpiration contributed by snow-covered soil layer [ ]
      REAL TSFSAV(ILG,4)    !<Ground surface temperature over subarea [K]
C
      REAL FC    (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL FG    (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL FCS   (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL FGS   (ILG) !<Subarea fractional coverage of modelled area [ ]
C
      REAL TPONDC(ILG) !<Subarea temperature of surface ponded water [C]
      REAL TPONDG(ILG) !<Subarea temperature of surface ponded water [C]
      REAL TPNDCS(ILG) !<Subarea temperature of surface ponded water [C]
      REAL TPNDGS(ILG) !<Subarea temperature of surface ponded water [C]
C          
      REAL EVAPC (ILG)  !<Evaporation from vegetation over ground \f$[m s^{-1}]\f$   
      REAL EVAPCG(ILG)  !<Evaporation from ground under vegetation \f$[m s^{-1}]\f$
      REAL EVAPG (ILG)  !<Evaporation from bare ground \f$[m s^{-1}]\f$ 
      REAL EVAPCS(ILG)  !<Evaporation from vegetation over snow \f$[m s^{-1}]\f$             
      REAL EVPCSG(ILG)  !<Evaporation from snow under vegetation \f$[m s^{-1}]\f$ 
      REAL EVAPGS(ILG)  !<Evaporation from snow on bare ground \f$[m s^{-1}]\f$ 
      REAL QFREZC(ILG)  !<Heat sink to be used for freezing water on ground under canopy \f$[W m^{-2}]\f$
      REAL QFREZG(ILG)  !<Heat sink to be used for freezing water on bare ground \f$[W m^{-2}]\f$
      REAL QMELTC(ILG)  !<Heat to be used for melting snow under canopy \f$[W m^{-2}]\f$
      REAL QMELTG(ILG)  !<Heat to be used for melting snow on bare ground \f$[W m^{-2}]\f$ 
      REAL RAICAN(ILG)  !<Intercepted liquid water stored on canopy over ground \f$[kg m^{-2}]\f$
      REAL SNOCAN(ILG)  !<Intercepted frozen water stored on canopy over ground \f$[kg m^{-2}]\f$ 
      REAL RAICNS(ILG)  !<Intercepted liquid water stored on canopy over snow \f$[kg m^{-2}]\f$
      REAL SNOCNS(ILG)  !<Intercepted frozen water stored on canopy over snow \f$[kg m^{-2}]\f$
      REAL FSVF  (ILG)  !<Sky view factor of ground under vegetation canopy [ ]
      REAL FSVFS (ILG)  !<Sky view factor of snow under vegetation canopy [ ]
      REAL CWLCAP(ILG)  !<Storage capacity of canopy over bare ground for liquid water \f$[kg m^{-2}]\f$
      REAL CWFCAP(ILG)  !<Storage capacity of canopy over bare ground for frozen water \f$[kg m^{-2}]\f$
      REAL CWLCPS(ILG)  !<Storage capacity of canopy over snow for liquid water \f$[kg m^{-2}]\f$
      REAL CWFCPS(ILG)  !<Storage capacity of canopy over snow for frozen water \f$[kg m^{-2}]\f$
      REAL TCANO (ILG)  !<Temperature of canopy over ground [K]   
      REAL TCANS (ILG)  !<Temperature of canopy over snow [K] 
      REAL CHCAP (ILG)  !<Heat capacity of canopy over bare ground \f$[J m^{-2} K^{-1}] \f$
      REAL CHCAPS(ILG)  !<Heat capacity of canopy over snow \f$[J m^{-2} K^{-1}] \f$ 
      REAL CMASSC(ILG)  !<Mass of canopy over bare ground \f$[kg m^{-2}]\f$   
      REAL CMASCS(ILG)  !<Mass of canopy over snow \f$[kg m^{-2}]\f$ 
      REAL ZSNOW (ILG)  !<Depth of snow pack [m] 
      REAL RHOSNI(ILG)  !<Density of fresh snow \f$[kg m^{-3}]\f$          
      REAL GZEROC(ILG)  !<Subarea heat flux at soil surface \f$[W m^{-2}]\f$
      REAL GZEROG(ILG)  !<Subarea heat flux at soil surface \f$[W m^{-2}]\f$
      REAL GZROCS(ILG)  !<Subarea heat flux at soil surface \f$[W m^{-2}]\f$
      REAL GZROGS(ILG)  !<Subarea heat flux at soil surface \f$[W m^{-2}]\f$
C            
      REAL G12C  (ILG)  !<Subarea heat flux between first and second soil layers \f$[W m^{-2}]\f$
      REAL G12G  (ILG)  !<Subarea heat flux between first and second soil layers \f$[W m^{-2}]\f$
      REAL G12CS (ILG)  !<Subarea heat flux between first and second soil layers \f$[W m^{-2}]\f$
      REAL G12GS (ILG)  !<Subarea heat flux between first and second soil layers \f$[W m^{-2}]\f$
C               
      REAL G23C  (ILG)  !<Subarea heat flux between second and third soil layers \f$[W m^{-2}]\f$
      REAL G23G  (ILG)  !<Subarea heat flux between second and third soil layers \f$[W m^{-2}]\f$
      REAL G23CS (ILG)  !<Subarea heat flux between second and third soil layers \f$[W m^{-2}]\f$
      REAL G23GS (ILG)  !<Subarea heat flux between second and third soil layers \f$[W m^{-2}]\f$
C
      REAL TSNOCS(ILG)  !<Temperature of snow pack under vegetation [K]   
      REAL TSNOGS(ILG)  !<Temperature of snow pack in bare areas [K] 
      REAL WSNOCS(ILG)  !<Liquid water content of snow pack under vegetation \f$[kg m^{-2}]\f$
      REAL WSNOGS(ILG)  !<Liquid water content of snow pack in bare areas \f$[kg m^{-2}]\f$
      REAL RHOSCS(ILG)  !<Density of snow under vegetation \f$[kg m^{-3}]\f$   
      REAL RHOSGS(ILG)  !<Density of snow in bare areas \f$[kg m^{-3}]\f$ 
      REAL ZPLIMC(ILG)  !<Subarea maximum ponding depth [m] 
      REAL ZPLIMG(ILG)  !<Subarea maximum ponding depth [m]
      REAL ZPLMCS(ILG)  !<Subarea maximum ponding depth [m]
      REAL ZPLMGS(ILG)  !<Subarea maximum ponding depth [m] 
      REAL GGEO  (ILG)  !<Geothermal heat flux at bottom of soil profile \f$[W m^{-2}]\f$
C
C     * SOIL PROPERTY ARRAYS.
C
      REAL THPOR (ILG,IG)   !<Pore volume in soil layer \f$[m^3 m^{-3}]\f$
      REAL THLRET(ILG,IG)   !<Liquid water retention capacity for organic soil [m3 m-3 ]
      REAL THLMIN(ILG,IG)   !<Residual soil liquid water content 
                            !<remaining after freezing or evaporation \f$[m^3 m^{-3}]\f$
      REAL BI    (ILG,IG)   !<Clapp and Hornberger empirical "b" parameter [ ]
      REAL GRKSAT(ILG,IG)   !<Saturated hydraulic conductivity of soil layer \f$[m s^{-1}]\f$
      REAL PSISAT(ILG,IG)   !<Soil moisture suction at saturation [m]
      REAL THLRAT(ILG,IG)   !<Fractional saturation of soil behind the wetting front [ ]
      REAL THFC  (ILG,IG)   !<Field capacity \f$[m^3 m^{-3}]\f$
      REAL HCPS  (ILG,IG)   !<Heat capacity of soil material \f$[J m^{-3} K^{-1}]\f$
      REAL DELZW (ILG,IG)   !<Overall thickness of soil layer [m]
      REAL DELZZ (ILG,IG)   !<Permeable thickness of soil layer [m]
      REAL ZBOTW (ILG,IG)   !<Depth to permeable bottom of soil layer [m]
      REAL XDRAIN(ILG)      !<Drainage index at bottom of soil profile [ ]   
      REAL XSLOPE(ILG)      !<Surface slope (used when running MESH code) [degrees]
      REAL GRKFAC(ILG)      !<WATROF parameter used when running MESH code [ ]
      REAL WFSURF(ILG)      !<WATROF parameter used when running MESH code [ ]
      REAL WFCINT(ILG)      !<WATROF parameter used when running MESH code [ ]
      REAL DELZ  (IG)       !<Overall thickness of soil layer [m]
C
      INTEGER ISAND(ILG,IG) !<Sand content flag
      INTEGER IGDR  (ILG)   !<Index of soil layer in which bedrock is encountered
C
C     * INTERNAL WORK ARRAYS USED THROUGHOUT CLASSW.
C
      REAL TBARWC(ILG,IG),TBARWG(ILG,IG),TBRWCS(ILG,IG),TBRWGS(ILG,IG),
     1     THLQCO(ILG,IG),THLQGO(ILG,IG),THLQCS(ILG,IG),THLQGS(ILG,IG),        
     2     THICCO(ILG,IG),THICGO(ILG,IG),THICCS(ILG,IG),THICGS(ILG,IG),        
     3     HCPCO (ILG,IG),HCPGO (ILG,IG),HCPCS (ILG,IG),HCPGS (ILG,IG),
     4     GRKSC (ILG,IG),GRKSG (ILG,IG),GRKSCS(ILG,IG),GRKSGS(ILG,IG),
     5     GFLXC (ILG,IG),GFLXG (ILG,IG),GFLXCS(ILG,IG),GFLXGS(ILG,IG)
C
      REAL SPCC  (ILG),   SPCG  (ILG),   SPCCS (ILG),   SPCGS (ILG),
     1     TSPCC (ILG),   TSPCG (ILG),   TSPCCS(ILG),   TSPCGS(ILG),
     2     RPCC  (ILG),   RPCG  (ILG),   RPCCS (ILG),   RPCGS (ILG),
     3     TRPCC (ILG),   TRPCG (ILG),   TRPCCS(ILG),   TRPCGS(ILG), 
     4     EVPIC (ILG),   EVPIG (ILG),   EVPICS(ILG),   EVPIGS(ILG),
     5     ZPONDC(ILG),   ZPONDG(ILG),   ZPNDCS(ILG),   ZPNDGS(ILG),
     6     XSNOWC(ILG),   XSNOWG(ILG),   XSNOCS(ILG),   XSNOGS(ILG),
     7     ZSNOWC(ILG),   ZSNOWG(ILG),   ZSNOCS(ILG),   ZSNOGS(ILG),
     8     ALBSC (ILG),   ALBSG (ILG),   ALBSCS(ILG),   ALBSGS(ILG),
     9     RHOSC (ILG),   RHOSG (ILG),   
     A     HCPSC (ILG),   HCPSG (ILG),   HCPSCS(ILG),   HCPSGS(ILG),
     B     RUNFC (ILG),   RUNFG (ILG),   RUNFCS(ILG),   RUNFGS(ILG),
     C     TRUNFC(ILG),   TRUNFG(ILG),   TRNFCS(ILG),   TRNFGS(ILG),
     D     TBASC (ILG),   TBASG (ILG),   TBASCS(ILG),   TBASGS(ILG)
C
      REAL SUBLC (ILG),   SUBLCS(ILG),   WLOSTC(ILG),   WLOSTG(ILG),
     1     WLSTCS(ILG),   WLSTGS(ILG),   RAC   (ILG),   RACS  (ILG),
     2     SNC   (ILG),   SNCS  (ILG),   TSNOWC(ILG),   TSNOWG(ILG), 
     3     DT    (ILG),   ZERO  (ILG),   RALB  (ILG),   ZFAV  (ILG),
     4     THLINV(ILG)
C
      INTEGER             LZFAV (ILG)
C
C     * INTERNAL WORK ARRAYS FOR WPREP AND CANADD.
C
      REAL RADD  (ILG),    SADD  (ILG)
C
C     * INTERNAL WORK FIELDS FOR GRINFL/GRDRAN (AND THEIR CALLED
C     * ROUTINES (I.E. WFILL,WFLOW,WEND) AND ICEBAL.
C
      REAL ZMAT  (ILG,IGP2,IGP1)
C
      REAL WMOVE (ILG,IGP2),   TMOVE (ILG,IGP2)
C
      REAL THLIQX(ILG,IGP1),   THICEX(ILG,IGP1),   TBARWX(ILG,IGP1),
     1     DELZX (ILG,IGP1),   ZBOTX (ILG,IGP1),   FDT   (ILG,IGP1),
     2     TFDT  (ILG,IGP1),   PSIF  (ILG,IGP1),   THLINF(ILG,IGP1),   
     3     GRKINF(ILG,IGP1),   FDUMMY(ILG,IGP1),   TDUMMY(ILG,IGP1),
     4     ZRMDR (ILG,IGP1)
C
      REAL THLMAX(ILG,IG),     THTEST(ILG,IG),     THLDUM(ILG,IG),
     1     THIDUM(ILG,IG),     TDUMW (ILG,IG)
C
      REAL TRMDR (ILG),    ZF    (ILG),    FMAX  (ILG),    TUSED (ILG),
     1     RDUMMY(ILG),    WEXCES(ILG),    FDTBND(ILG),    WADD  (ILG),
     2     TADD  (ILG),    WADJ  (ILG),    TIMPND(ILG),    DZF   (ILG),
     3     DTFLOW(ILG),    THLNLZ(ILG),    THLQLZ(ILG),    DZDISP(ILG),
     4     WDISP (ILG),    WABS  (ILG),    ZMOVE (ILG),    TBOT  (ILG)
C
      INTEGER              IGRN  (ILG),    IGRD  (ILG),    IZERO (ILG),    
     1                     IFILL (ILG),    LZF   (ILG),    NINF  (ILG),    
     2                     IFIND (ILG),    ITER  (ILG),    NEND  (ILG),    
     3                     ISIMP (ILG),    ICONT (ILG)
C
C     * INTERNAL WORK ARRAYS FOR CANVAP AND SNOALBW.
C
      REAL EVLOST(ILG),    RLOST (ILG),    RHOMAX(ILG)
C
      INTEGER              IROOT (ILG)
C
C     * INTERNAL WORK ARRAYS FOR WATROF.
C
      REAL THCRIT(ILG,IG), DODRN (ILG),     DOVER (ILG),
     1     DIDRN (ILG,IG), DIDRNMX(ILG,IG)
C
C     * INTERNAL WORK ARRAYS FOR CHKWAT.
C
      REAL BAL   (ILG)
C
C     * INTERNAL SCALARS.                                               
C                                                                       
      REAL SNOROF,WSNROF                                                
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
      REAL HCPW     !<Volumetric heat capacity of water \f$(4.187 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPICE   !<Volumetric heat capacity of ice \f$(1.9257 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSOL   !<Volumetric heat capacity of mineral matter 
                    !<\f$(2.25 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPOM    !<Volumetric heat capacity of organic matter 
                    !<\f$(2.50 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSND   !<Volumetric heat capacity of sand particles 
                    !<\f$(2.13 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPCLY   !<Volumetric heat capacity of fine mineral particles 
                    !<\f$(2.38 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL SPHW     !<Specific heat of water \f$(4.186 * 10^3) [J kg^{-1} K^{-1}] \f$
      REAL SPHICE   !<Specific heat of ice \f$(2.10 * 10^3) [J kg^{-1} K^{-1}] \f$
      REAL SPHVEG   !<Specific heat of vegetation matter \f$(2.70 * 10^3) [J kg^{-1} K^{-1}] \f$
      REAL SPHAIR   !<Specific heat of air \f$[J kg^{-1} K^{-1}] \f$
      REAL RHOW     !<Density of water \f$(1.0 * 10^3) [kg m^{-3}]\f$
      REAL RHOICE   !<Density of ice \f$(0.917 * 10^3) [kg m^{-3}]\f$
      REAL TCGLAC   !<Thermal conductivity of ice sheets \f$(2.24) [W m^{-1} K^{-1}]\f$
      REAL CLHMLT   !<Latent heat of freezing of water \f$(0.334 * 10^6) [J kg^{-1}] \f$
      REAL CLHVAP   !<Latent heat of vaporization of water \f$(2.501 * 10^6) [J kg^{-1}] \f$
C
      COMMON /CLASS1/ DELT,TFREZ                                               
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP

C==============The additional parameters and Variables for Interflow========================
C===================Added by Stefan Sauer, July 2017=================================



      REAL XDRAINH(ILG)   !The Fractional change in horizontal conductivity in a depth change of h0, Calculated as a Parameter 
      REAL MANNING_N(ILG)  !Manning's Roughness Coefficient, Calculated as an input Parameter
      REAL DD (ILG)        !Drainage Density, [M^2/M^3]
      REAL KSAT (ILG)      !Vertical Hydraulic Conductivity at Saturation at the Bottom of the Soil Layer, Used in WATROF
      REAL BULK_FC(ILG,IG) !Bulk Field Capacity, but THFCGAT is field Capacity in runclass36ctem.f (Not the Same)
      INTEGER :: NA,NTYPE  !Some Integer Parameter,
      REAL ILMOS(ILG)      !Some Unused variable for WATDRN3
      REAL JLMOS(ILG)      !Some unused variable for WATDRN3
      REAL BTC(NTYPE,IG)   !Some Unused Variable for WATDRN3
      REAL BCAP(NTYPE,IG)  !Some Unused Variable for WATDRN3
      REAL DCOEFF(NTYPE,IG)!Some Unused Variable for WATDRN3
      REAL BFCAP(NTYPE,IG) !Some Unused Variable for WATDRN3
      REAL BFCOEFF(NTYPE,IG)!Some Unused Variable for WATDRN3
      REAL BFMIN(NTYPE,IG) !Some Unused Variable for WATDRN3
      REAL BQMAX(NTYPE,IG) !Some Unused Variable for WATDRN3
      REAL FRZC(ILG)       !Some Unused Variable for SNINFLM
      INTEGER NCOUNT(ILG)  !Some Unused Variable for SNINFLM
      REAL T0_ACC          !Some Unused Variable for SNINFLM
      REAL SI(ILG)         !Some Unused Variable for SNINFLM
      REAL TSI(ILG)        !Some Unused Variable for SNINFLM
      INTEGER INFILTYPE(ILG)!Some Unused Variable for SNINFLM
      REAL SNOWMELTD(ILG)  !Some Unused Variable for SNINFLM
      REAL SNOWMELTD_LAST(ILG) !Some Unused Variable for SNINFLM
      REAL MELTRUNOFF(ILG) !Some Unused Variable for SNINFLM
      REAL SNOWINFIL(ILG)  !Some Unused Variable for SNINFLM
      REAL CUMSNOWINFILCS(ILG) !Some Unused Variable for SNINFLM
      REAL CUMSNOWINFILGS(ILG) !Some Unused Variable for SNINFLM
      REAL SOIL_POR_MAX    !Some Unused Variable for SNINFLM
      REAL SOIL_DEPTH      !Soil Depth Variable used only for SNINFLM
      REAL S0              !Soil Depth Variable used only for SNINFLM
      REAL T_ICE_LENS      !Some Unused Variable used only for SNINFLM
      REAL CMIN(ILG),CMAX(ILG), B(ILG), K1(ILG), K2(ILG) !PDMROF Variables (Not used in new CLASSW.f)
      REAL ZPNDPRECS(ILG), ZPONDPREC(ILG), ZPONDPREG(ILG),ZPNDPREGS(ILG) !Latflow Variables
      REAL UM1CS(ILG), UM1C(ILG), UM1G(ILG), UM1GS(ILG) !For PDMROF
      REAL QM1CS(ILG), QM1C(ILG), QM1G(ILG), QM1GS(ILG) !For PDMROF
      REAL QM2CS(ILG), QM2C(ILG), QM2G(ILG), QM2GS(ILG) !For PDMROF
      REAL UMQ(ILG)        !For PDMROF and LATFLOW
      REAL FSTRCS(ILG), FSTRC(ILG), FSTRGS(ILG), FSTRG(ILG) !Used for PDMROF

C
C-----------------------------------------------------------------------
      !>
      !!First, subroutine WPREP is called to initialize various arrays 
      !!and produce parameters for the four subareas of canopy over snow 
      !!(CS), snow on ground (GS), canopy over ground (C) and bare ground 
      !!(G). Then, for each of the four subareas, if the number of 
      !!modelled areas containing that subarea is greater than zero, a 
      !!series of subroutines is called. The subroutines associated with 
      !!each subarea are listed in the table below.
      !!
      !!\f[
      !!\begin{tabular} { | l | l | c | }
      !!\hline
      !! CANVAP  & Evaporation/sublimation of water from vegetation canopy        &   CS,C    \\ \hline
      !! CANADD  & Addition of rainfall/snowfall to canopy; throughfall and drip  &   CS,C    \\ \hline
      !! CWCALC  & Freezing/thawing of liquid/frozen water on canopy              &   CS,C    \\ \hline
      !! SUBCAN  & Precipitaiton and condensation under canopy                    &   CS,C    \\ \hline
      !! TWCALC  & Freezing/thawing of liquid/frozen water in soil                & CS,GS,C,G \\ \hline
      !! SNOVAP  & Sublimaiton from snow pack                                     & CS,GS,C,G \\ \hline
      !! TFREEZ  & Freezing of ponded water on soil                               & CS,GS,C,G \\ \hline
      !! TMELT   & Melting of snow pack                                           &   CS,GS   \\ \hline
      !! SNOADD  & Accumulation of snow on ground                                 & CS,GS,C,G \\ \hline
      !! SNINFL  & Infiltration of rain into snow pack                            &   CS,GS   \\ \hline
      !! ICEBAL  & Energy and water budget of ice sheets                          &   GS,G    \\ \hline
      !! GRINFL  & Infiltraiton of water into soil                                & CS,GS,C,G \\ \hline
      !! GRDRAN  & Soil water movement in response to gravity and suction forces  & CS,GS,C,G \\ \hline
      !! TMCALC  & Step ahead soil layer temperatures, check for freezing/thawing & CS,GS,C,G \\ \hline
      !! CHKWAT  & Check subarea moisture balances for closure                    & CS,GS,C,G \\ \hline
      !! SNOALBW & Temporal variation of snow albedo and density                  &   CS,GS   \\ \hline
      !!\end{tabular}
      !!\f]
C     * PREPARATION.

	  

      CALL WPREP(THLQCO, THLQGO, THLQCS, THLQGS, THICCO, THICGO,
     1           THICCS, THICGS, HCPCO,  HCPGO,  HCPCS,  HCPGS,
     2           GRKSC,  GRKSG,  GRKSCS, GRKSGS,
     3           SPCC,   SPCG,   SPCCS,  SPCGS,  TSPCC,  TSPCG,
     4           TSPCCS, TSPCGS, RPCC,   RPCG,   RPCCS,  RPCGS,
     5           TRPCC,  TRPCG,  TRPCCS, TRPCGS, EVPIC,  EVPIG,
     6           EVPICS, EVPIGS, ZPONDC, ZPONDG, ZPNDCS, ZPNDGS,
     7           XSNOWC, XSNOWG, XSNOCS, XSNOGS, ZSNOWC, ZSNOWG,
     8           ZSNOCS, ZSNOGS, ALBSC,  ALBSG,  ALBSCS, ALBSGS,
     9           RHOSC,  RHOSG,  HCPSC,  HCPSG,  HCPSCS, HCPSGS, 
     A           RUNFC,  RUNFG,  RUNFCS, RUNFGS,
     B           TRUNFC, TRUNFG, TRNFCS, TRNFGS, TBASC,  TBASG,  
     C           TBASCS, TBASGS, GFLXC,  GFLXG,  GFLXCS, GFLXGS,
     D           SUBLC,  SUBLCS, WLOSTC, WLOSTG, WLSTCS, WLSTGS,
     E           RAC,    RACS,   SNC,    SNCS,   TSNOWC, TSNOWG,
     F           OVRFLW, SUBFLW, BASFLW, TOVRFL, TSUBFL, TBASFL, 
     G           PCFC,   PCLC,   PCPN,   PCPG,   QFCF,   QFCL,
     H           QFN,    QFG,    QFC,    HMFG,   
     I           ROVG,   ROFC,   ROFN,   TRUNOF, 
     J           THLIQX, THICEX, THLDUM, THIDUM,
     K           DT,     RDUMMY, ZERO,   IZERO,  DELZZ,
     L           FC,     FG,     FCS,    FGS,    
     M           THLIQC, THLIQG, THICEC, THICEG, HCPC,   HCPG,
     N           TBARC,  TBARG,  TBARCS, TBARGS, TBASE,  TSFSAV,
     O           FSVF,   FSVFS,  RAICAN, SNOCAN, RAICNS, SNOCNS, 
     P           EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS, 
     Q           RPCP,   TRPCP,  SPCP,   TSPCP,  RHOSNI, ZPOND,  
     R           ZSNOW,  ALBSNO, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     S           THPOR,  HCPS,   GRKSAT, ISAND,  DELZW,  DELZ,
     T           ILG,    IL1,    IL2,    JL,     IG,     IGP1,
     U           NLANDCS,NLANDGS,NLANDC, NLANDG, RADD,   SADD  )
C
C

C
      IF(NLANDCS.GT.0)                                              THEN
          CALL CANVAP(EVAPCS,SUBLCS,RAICNS,SNOCNS,TCANS,THLQCS,
     1                TBARCS,ZSNOCS,WLSTCS,CHCAPS,QFCF,QFCL,QFN,QFC,
     2                HTCC,HTCS,HTC,FCS,CMASCS,TSNOCS,HCPSCS,RHOSCS,
     3                FROOTS,THPOR,THLMIN,DELZW,EVLOST,RLOST,IROOT, 
     4                IG,ILG,IL1,IL2,JL,N  )
          CALL CANADD(2,RPCCS,TRPCCS,SPCCS,TSPCCS,RAICNS,SNOCNS,
     1                TCANS,CHCAPS,HTCC,ROFC,ROVG,PCPN,PCPG,
     2                FCS,FSVFS,CWLCPS,CWFCPS,CMASCS,RHOSNI,
     3                TSFSAV(1,1),RADD,SADD,ILG,IL1,IL2,JL)             
          CALL CWCALC(TCANS,RAICNS,SNOCNS,RDUMMY,RDUMMY,CHCAPS,
     1                HMFC,HTCC,FCS,CMASCS,ILG,IL1,IL2,JL)
          CALL SUBCAN(2,RPCCS,TRPCCS,SPCCS,TSPCCS,RHOSNI,EVPCSG,
     1                QFN,QFG,PCPN,PCPG,FCS,ILG,IL1,IL2,JL)
          CALL TWCALC(TBARCS,THLQCS,THICCS,HCPCS,TBRWCS,HMFG,HTC,
     1                FCS,ZERO,THPOR,THLMIN,HCPS,DELZW,DELZZ,ISAND,
     2                IG,ILG,IL1,IL2,JL)
          CALL SNOVAP(RHOSCS,ZSNOCS,HCPSCS,TSNOCS,EVPCSG,QFN,QFG,
     1                HTCS,WLSTCS,TRNFCS,RUNFCS,TOVRFL,OVRFLW,
     2                FCS,RPCCS,SPCCS,RHOSNI,WSNOCS,ILG,IL1,IL2,JL)
          CALL TFREEZ(ZPNDCS,TPNDCS,ZSNOCS,TSNOCS,ALBSCS,
     1                RHOSCS,HCPSCS,GZROCS,HMFG,HTCS,HTC,
     2                WTRS,WTRG,FCS,ZERO,WSNOCS,TA,TBARCS,
     3                ISAND,IG,ILG,IL1,IL2,JL) 
          CALL TMELT(ZSNOCS,TSNOCS,QMELTC,RPCCS,TRPCCS,
     1               GZROCS,RALB,HMFN,HTCS,HTC,FCS,HCPSCS,
     2               RHOSCS,WSNOCS,ISAND,IG,ILG,IL1,IL2,JL)
          CALL SNOADD(ALBSCS,TSNOCS,RHOSCS,ZSNOCS,
     1                HCPSCS,HTCS,FCS,SPCCS,TSPCCS,RHOSNI,WSNOCS,
     2                ILG,IL1,IL2,JL)
          CALL SNINFL(RPCCS,TRPCCS,ZSNOCS,TSNOCS,RHOSCS,HCPSCS,
     1                WSNOCS,HTCS,HMFN,PCPG,ROFN,FCS,ILG,IL1,IL2,JL)
          CALL GRINFL(1,THLQCS,THICCS,TBRWCS,BASFLW,TBASFL,RUNFCS,
     1                TRNFCS,ZFAV,LZFAV,THLINV,QFG,WLSTCS,
     2                FCS,EVPCSG,RPCCS,TRPCCS,TPNDCS,ZPNDCS,
     3                DT,ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     4                DELZX,ZBOTX,FDT,TFDT,PSIF,THLINF,GRKINF,
     5                THLMAX,THTEST,ZRMDR,FDUMMY,TDUMMY,THLDUM,
     6                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     7                ZERO,WEXCES,FDTBND,WADD,TADD,WADJ,TIMPND,
     8                DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,WABS,
     9                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSCS,
     A                THLRAT,THFC,DELZW,ZBOTW,XDRAIN,DELZ,ISAND,
     B                IGRN,IGRD,IFILL,IZERO,LZF,NINF,IFIND,ITER,
     C                NEND,ISIMP,IGDR,
     D                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL GRDRAN(1,THLQCS,THICCS,TBRWCS,FDUMMY,TDUMMY,
     1                BASFLW,TBASFL,RUNFCS,TRNFCS,
     2                QFG,WLSTCS,FCS,EVPCSG,RPCCS,ZPNDCS,
     3                DT,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     4                BI,PSISAT,GRKSCS,THFC,DELZW,XDRAIN,ISAND,
     5                IZERO,IGRN,IGRD,IGDR,
     6                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
         CALL WATROF(THLQCS,THICCS,ZPNDCS,TPNDCS,OVRFLW,TOVRFL,
     1                SUBFLW,TSUBFL,RUNFCS,TRNFCS,FCS,ZPLMCS,
     2                XSLOPE,GRKFAC,MANNING_N,DD,WFCINT,TBRWCS,
     3                DELZW,THPOR,THLMIN,BI,DODRN,DOVER,DIDRN,
     4                ISAND,IWF,IG,ILG,IL1,IL2,BULK_FC,
     6                NA,NTYPE,ILMOS,JLMOS,
     7                BTC,BCAP,DCOEFF,BFCAP,BFCOEFF,BFMIN,BQMAX)
          CALL TMCALC(TBARCS,THLQCS,THICCS,HCPCS,TPNDCS,ZPNDCS,
     1                TSNOCS,ZSNOCS,ALBSCS,RHOSCS,HCPSCS,TBASCS,
     2                OVRFLW,TOVRFL,RUNFCS,TRNFCS,HMFG,HTC,HTCS,
     3                WTRS,WTRG,FCS,TBRWCS,GZROCS,G12CS,
     4                G23CS,GGEO,TA,WSNOCS,TCTOPC,TCBOTC,GFLXCS,
     5                ZPLMCS,THPOR,THLMIN,HCPS,DELZW,DELZZ,DELZ,
     6                ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
          CALL CHKWAT(1,PCPR,EVPICS,RUNFCS,WLSTCS,RAICNS,SNOCNS,
     1                RACS,SNCS,ZPNDCS,ZPOND,THLQCS,THICCS,
     2                THLIQC,THICEC,ZSNOCS,RHOSCS,XSNOCS,SNO,
     3                WSNOCS,WSNOW,FCS,FGS,FCS,BAL,THPOR,THLMIN,
     4                DELZW,ISAND,IG,ILG,IL1,IL2,JL,N   ) 
          CALL SNOALBW(ALBSCS,RHOSCS,ZSNOCS,HCPSCS,
     1                 TSNOCS,FCS,SPCCS,RALB,WSNOCS,RHOMAX,
     2                 ISAND,ILG,IG,IL1,IL2,JL)       
      ENDIF      

C
C     * CALCULATIONS FOR SNOW-COVERED GROUND.
C
      IF(NLANDGS.GT.0)                                              THEN
          CALL TWCALC(TBARGS,THLQGS,THICGS,HCPGS,TBRWGS,HMFG,HTC,
     1                FGS,ZERO,THPOR,THLMIN,HCPS,DELZW,DELZZ,ISAND,
     2                IG,ILG,IL1,IL2,JL)
          CALL SNOVAP(RHOSGS,ZSNOGS,HCPSGS,TSNOGS,EVAPGS,QFN,QFG,
     1                HTCS,WLSTGS,TRNFGS,RUNFGS,TOVRFL,OVRFLW,
     2                FGS,RPCGS,SPCGS,RHOSNI,WSNOGS,ILG,IL1,IL2,JL)  
          CALL TFREEZ(ZPNDGS,TPNDGS,ZSNOGS,TSNOGS,ALBSGS,
     1                RHOSGS,HCPSGS,GZROGS,HMFG,HTCS,HTC,
     2                WTRS,WTRG,FGS,ZERO,WSNOGS,TA,TBARGS,
     3                ISAND,IG,ILG,IL1,IL2,JL) 
          CALL TMELT(ZSNOGS,TSNOGS,QMELTG,RPCGS,TRPCGS,
     1               GZROGS,RALB,HMFN,HTCS,HTC,FGS,HCPSGS,
     2               RHOSGS,WSNOGS,ISAND,IG,ILG,IL1,IL2,JL)
          CALL SNOADD(ALBSGS,TSNOGS,RHOSGS,ZSNOGS,
     1                HCPSGS,HTCS,FGS,SPCGS,TSPCGS,RHOSNI,WSNOGS,
     2                ILG,IL1,IL2,JL)
          CALL SNINFL(RPCGS,TRPCGS,ZSNOGS,TSNOGS,RHOSGS,HCPSGS,
     1                WSNOGS,HTCS,HMFN,PCPG,ROFN,FGS,ILG,IL1,IL2,JL)
          IF(NLANDI.NE.0)                                       THEN
              CALL ICEBAL(TBARGS,TPNDGS,ZPNDGS,TSNOGS,RHOSGS,ZSNOGS,
     1                    HCPSGS,ALBSGS,HMFG,HTCS,HTC,WTRS,WTRG,GFLXGS,
     2                    RUNFGS,TRNFGS,OVRFLW,TOVRFL,ZPLMGS,GGEO,
     3                    FGS,EVAPGS,RPCGS,TRPCGS,GZROGS,G12GS,G23GS,
     4                    HCPGS,QMELTG,WSNOGS,ZMAT,TMOVE,WMOVE,ZRMDR,
     5                    TADD,ZMOVE,TBOT,DELZ,ISAND,ICONT,
     6                    IWF,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
          ENDIF
          CALL GRINFL(2,THLQGS,THICGS,TBRWGS,BASFLW,TBASFL,RUNFGS,
     1                TRNFGS,ZFAV,LZFAV,THLINV,QFG,WLSTGS,
     2                FGS,EVAPGS,RPCGS,TRPCGS,TPNDGS,ZPNDGS,
     3                DT,ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     4                DELZX,ZBOTX,FDT,TFDT,PSIF,THLINF,GRKINF,
     5                THLMAX,THTEST,ZRMDR,FDUMMY,TDUMMY,THLDUM,
     6                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     7                ZERO,WEXCES,FDTBND,WADD,TADD,WADJ,TIMPND,
     8                DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,WABS,
     9                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSGS,
     A                THLRAT,THFC,DELZW,ZBOTW,XDRAIN,DELZ,ISAND,
     B                IGRN,IGRD,IFILL,IZERO,LZF,NINF,IFIND,ITER,
     C                NEND,ISIMP,IGDR,
     D                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL GRDRAN(2,THLQGS,THICGS,TBRWGS,FDUMMY,TDUMMY,
     1                BASFLW,TBASFL,RUNFGS,TRNFGS,
     2                QFG,WLSTGS,FGS,EVAPGS,RPCGS,ZPNDGS,
     3                DT,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     4                BI,PSISAT,GRKSGS,THFC,DELZW,XDRAIN,ISAND,
     5                IZERO,IGRN,IGRD,IGDR,
     6                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)

          CALL WATROF(THLQGS,THICGS,ZPNDGS,TPNDGS,OVRFLW,TOVRFL,
     1                SUBFLW,TSUBFL,RUNFGS,TRNFGS,FGS,ZPLMGS,
     2                XSLOPE,GRKFAC,MANNING_N,DD,WFCINT,TBRWGS,
     3                DELZW,THPOR,THLMIN,BI,DODRN,DOVER,DIDRN,
     4                ISAND,IWF,IG,ILG,IL1,IL2,BULK_FC,
     6                NA,NTYPE,ILMOS,JLMOS,
     7                BTC,BCAP,DCOEFF,BFCAP,BFCOEFF,BFMIN,BQMAX)

          CALL TMCALC(TBARGS,THLQGS,THICGS,HCPGS,TPNDGS,ZPNDGS,
     1                TSNOGS,ZSNOGS,ALBSGS,RHOSGS,HCPSGS,TBASGS,
     2                OVRFLW,TOVRFL,RUNFGS,TRNFGS,HMFG,HTC,HTCS,
     3                WTRS,WTRG,FGS,TBRWGS,GZROGS,G12GS,
     4                G23GS,GGEO,TA,WSNOGS,TCTOPG,TCBOTG,GFLXGS,
     5                ZPLMGS,THPOR,THLMIN,HCPS,DELZW,DELZZ,DELZ,
     6                ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
          CALL CHKWAT(2,PCPR,EVPIGS,RUNFGS,WLSTGS,RAICNS,SNOCNS,
     1                RACS,SNCS,ZPNDGS,ZPOND,THLQGS,THICGS,
     2                THLIQG,THICEG,ZSNOGS,RHOSGS,XSNOGS,SNO,
     3                WSNOGS,WSNOW,FCS,FGS,FGS,BAL,THPOR,THLMIN,
     4                DELZW,ISAND,IG,ILG,IL1,IL2,JL,N   ) 
          CALL SNOALBW(ALBSGS,RHOSGS,ZSNOGS,HCPSGS,
     1                 TSNOGS,FGS,SPCGS,RALB,WSNOGS,RHOMAX,
     2                 ISAND,ILG,IG,IL1,IL2,JL)       
      ENDIF                                                               
C
C     * CALCULATIONS FOR CANOPY OVER BARE GROUND.


	
C
      IF(NLANDC.GT.0)                                               THEN

          CALL CANVAP(EVAPC,SUBLC,RAICAN,SNOCAN,TCANO,THLQCO,
     1                TBARC,ZSNOWC,WLOSTC,CHCAP,QFCF,QFCL,QFN,QFC,
     2                HTCC,HTCS,HTC,FC,CMASSC,TSNOWC,HCPSC,RHOSC,
     3                FROOT,THPOR,THLMIN,DELZW,EVLOST,RLOST,IROOT,
     4                IG,ILG,IL1,IL2,JL,N  )

          CALL CANADD(1,RPCC,TRPCC,SPCC,TSPCC,RAICAN,SNOCAN,
     1                TCANO,CHCAP,HTCC,ROFC,ROVG,PCPN,PCPG,
     2                FC,FSVF,CWLCAP,CWFCAP,CMASSC,RHOSNI,
     3                TSFSAV(1,3),RADD,SADD,ILG,IL1,IL2,JL)             
          CALL CWCALC(TCANO,RAICAN,SNOCAN,RDUMMY,RDUMMY,CHCAP,
     1                HMFC,HTCC,FC,CMASSC,ILG,IL1,IL2,JL)
          CALL SUBCAN(1,RPCC,TRPCC,SPCC,TSPCC,RHOSNI,EVAPCG,
     1                QFN,QFG,PCPN,PCPG,FC,ILG,IL1,IL2,JL)

          CALL TWCALC(TBARC,THLQCO,THICCO,HCPCO,TBARWC,HMFG,HTC,
     1                FC,EVAPCG,THPOR,THLMIN,HCPS,DELZW,DELZZ,
     2                ISAND,IG,ILG,IL1,IL2,JL)

          CALL SNOVAP(RHOSC,ZSNOWC,HCPSC,TSNOWC,EVAPCG,QFN,QFG,
     1                HTCS,WLOSTC,TRUNFC,RUNFC,TOVRFL,OVRFLW,
     2                FC,RPCC,SPCC,RHOSNI,ZERO,ILG,IL1,IL2,JL)
          CALL TFREEZ(ZPONDC,TPONDC,ZSNOWC,TSNOWC,ALBSC,
     1                RHOSC,HCPSC,GZEROC,HMFG,HTCS,HTC,
     2                WTRS,WTRG,FC,QFREZC,ZERO,TA,TBARC,
     3                ISAND,IG,ILG,IL1,IL2,JL) 
          CALL SNOADD(ALBSC,TSNOWC,RHOSC,ZSNOWC,
     1                HCPSC,HTCS,FC,SPCC,TSPCC,RHOSNI,ZERO,
     2                ILG,IL1,IL2,JL)


          CALL GRINFL(3,THLQCO,THICCO,TBARWC,BASFLW,TBASFL,RUNFC,
     1                TRUNFC,ZFAV,LZFAV,THLINV,QFG,WLOSTC,
     2                FC,EVAPCG,RPCC,TRPCC,TPONDC,ZPONDC,
     3                DT,ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     4                DELZX,ZBOTX,FDT,TFDT,PSIF,THLINF,GRKINF,
     5                THLMAX,THTEST,ZRMDR,FDUMMY,TDUMMY,THLDUM,
     6                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     7                ZERO,WEXCES,FDTBND,WADD,TADD,WADJ,TIMPND,
     8                DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,WABS,
     9                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSC,
     A                THLRAT,THFC,DELZW,ZBOTW,XDRAIN,DELZ,ISAND,
     B                IGRN,IGRD,IFILL,IZERO,LZF,NINF,IFIND,ITER,
     C                NEND,ISIMP,IGDR,
     D                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)

          CALL GRDRAN(3,THLQCO,THICCO,TBARWC,FDUMMY,TDUMMY,
     1                BASFLW,TBASFL,RUNFC,TRUNFC,
     2                QFG,WLOSTC,FC,EVAPCG,RPCC,ZPONDC,
     3                DT,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     4                BI,PSISAT,GRKSC,THFC,DELZW,XDRAIN,ISAND,
     5                IZERO,IGRN,IGRD,IGDR,
     6                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)

          CALL WATROF(THLQCO,THICCO,ZPONDC,TPONDC,OVRFLW,TOVRFL,
     1                SUBFLW,TSUBFL,RUNFC,TRUNFC,FC,ZPLIMC,
     2                XSLOPE,GRKFAC,MANNING_N,DD,WFCINT,TBARWC,
     3                DELZW,THPOR,THLMIN,BI,DODRN,DOVER,DIDRN,
     4                ISAND,IWF,IG,ILG,IL1,IL2,BULK_FC,
     6                NA,NTYPE,ILMOS,JLMOS,
     7                BTC,BCAP,DCOEFF,BFCAP,BFCOEFF,BFMIN,BQMAX)

          CALL TMCALC(TBARC,THLQCO,THICCO,HCPCO,TPONDC,ZPONDC,
     1                TSNOWC,ZSNOWC,ALBSC,RHOSC,HCPSC,TBASC,
     2                OVRFLW,TOVRFL,RUNFC,TRUNFC,HMFG,HTC,HTCS,
     3                WTRS,WTRG,FC,TBARWC,GZEROC,G12C,
     4                G23C,GGEO,TA,ZERO,TCTOPC,TCBOTC,GFLXC,
     5                ZPLIMC,THPOR,THLMIN,HCPS,DELZW,DELZZ,DELZ,
     6                ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
          CALL CHKWAT(3,PCPR,EVPIC,RUNFC,WLOSTC,RAICAN,SNOCAN,
     1                RAC,SNC,ZPONDC,ZPOND,THLQCO,THICCO,
     2                THLIQC,THICEC,ZSNOWC,RHOSC,XSNOWC,SNO,
     3                ZERO,ZERO,FCS,FGS,FC,BAL,THPOR,THLMIN,
     4                DELZW,ISAND,IG,ILG,IL1,IL2,JL,N    ) 
C
      ENDIF                                                               
C
C     * CALCULATIONS FOR BARE GROUND.
C
      IF(NLANDG.GT.0)                                               THEN
          CALL TWCALC(TBARG,THLQGO,THICGO,HCPGO,TBARWG,HMFG,HTC,
     1                FG,EVAPG,THPOR,THLMIN,HCPS,DELZW,DELZZ,
     2                ISAND,IG,ILG,IL1,IL2,JL)             
          CALL SNOVAP(RHOSG,ZSNOWG,HCPSG,TSNOWG,EVAPG,QFN,QFG,
     1                HTCS,WLOSTG,TRUNFG,RUNFG,TOVRFL,OVRFLW,
     2                FG,RPCG,SPCG,RHOSNI,ZERO,ILG,IL1,IL2,JL)
          CALL TFREEZ(ZPONDG,TPONDG,ZSNOWG,TSNOWG,ALBSG,
     1                RHOSG,HCPSG,GZEROG,HMFG,HTCS,HTC,
     2                WTRS,WTRG,FG,QFREZG,ZERO,TA,TBARG,
     3                ISAND,IG,ILG,IL1,IL2,JL) 
          CALL SNOADD(ALBSG,TSNOWG,RHOSG,ZSNOWG,
     1                HCPSG,HTCS,FG,SPCG,TSPCG,RHOSNI,ZERO,
     2                ILG,IL1,IL2,JL)
          IF(NLANDI.NE.0)                                       THEN
              CALL ICEBAL(TBARG,TPONDG,ZPONDG,TSNOWG,RHOSG,ZSNOWG,
     1                    HCPSG,ALBSG,HMFG,HTCS,HTC,WTRS,WTRG,GFLXG,
     2                    RUNFG,TRUNFG,OVRFLW,TOVRFL,ZPLIMG,GGEO,
     3                    FG,EVAPG,RPCG,TRPCG,GZEROG,G12G,G23G,
     4                    HCPGO,QFREZG,ZERO,ZMAT,TMOVE,WMOVE,ZRMDR,
     5                    TADD,ZMOVE,TBOT,DELZ,ISAND,ICONT,
     6                    IWF,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
          ENDIF
          CALL GRINFL(4,THLQGO,THICGO,TBARWG,BASFLW,TBASFL,RUNFG,
     1                TRUNFG,ZFAV,LZFAV,THLINV,QFG,WLOSTG,
     2                FG,EVAPG,RPCG,TRPCG,TPONDG,ZPONDG,
     3                DT,ZMAT,WMOVE,TMOVE,THLIQX,THICEX,TBARWX,
     4                DELZX,ZBOTX,FDT,TFDT,PSIF,THLINF,GRKINF,
     5                THLMAX,THTEST,ZRMDR,FDUMMY,TDUMMY,THLDUM,
     6                THIDUM,TDUMW,TRMDR,ZF,FMAX,TUSED,RDUMMY,
     7                ZERO,WEXCES,FDTBND,WADD,TADD,WADJ,TIMPND,
     8                DZF,DTFLOW,THLNLZ,THLQLZ,DZDISP,WDISP,WABS,
     9                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSG,
     A                THLRAT,THFC,DELZW,ZBOTW,XDRAIN,DELZ,ISAND,
     B                IGRN,IGRD,IFILL,IZERO,LZF,NINF,IFIND,ITER,
     C                NEND,ISIMP,IGDR,
     D                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL GRDRAN(4,THLQGO,THICGO,TBARWG,FDUMMY,TDUMMY,
     1                BASFLW,TBASFL,RUNFG,TRUNFG,
     2                QFG,WLOSTG,FG,EVAPG,RPCG,ZPONDG,
     3                DT,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     4                BI,PSISAT,GRKSG,THFC,DELZW,XDRAIN,ISAND,
     5                IZERO,IGRN,IGRD,IGDR,
     6                IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
          CALL TMCALC(TBARG,THLQGO,THICGO,HCPGO,TPONDG,ZPONDG,
     1                TSNOWG,ZSNOWG,ALBSG,RHOSG,HCPSG,TBASG,
     2                OVRFLW,TOVRFL,RUNFG,TRUNFG,HMFG,HTC,HTCS,
     3                WTRS,WTRG,FG,TBARWG,GZEROG,G12G,
     4                G23G,GGEO,TA,ZERO,TCTOPG,TCBOTG,GFLXG,
     5                ZPLIMG,THPOR,THLMIN,HCPS,DELZW,DELZZ,DELZ,
     6                ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
          CALL WATROF(THLQGO,THICGO,ZPONDG,TPONDG,OVRFLW,TOVRFL,
     1                SUBFLW,TSUBFL,RUNFG,TRUNFG,FG,ZPLIMG,
     2                XSLOPE,GRKFAC,MANNING_N,DD,WFCINT,TBARWG,
     3                DELZW,THPOR,THLMIN,BI,DODRN,DOVER,DIDRN,
     4                ISAND,IWF,IG,ILG,IL1,IL2,BULK_FC,
     6                NA,NTYPE,ILMOS,JLMOS,
     7                BTC,BCAP,DCOEFF,BFCAP,BFCOEFF,BFMIN,BQMAX)
          CALL CHKWAT(4,PCPR,EVPIG,RUNFG,WLOSTG,RAICAN,SNOCAN,
     1                RAC,SNC,ZPONDG,ZPOND,THLQGO,THICGO,
     2                THLIQG,THICEG,ZSNOWG,RHOSG,XSNOWG,SNO,
     3                ZERO,ZERO,FCS,FGS,FG,BAL,THPOR,THLMIN,
     4                DELZW,ISAND,IG,ILG,IL1,IL2,JL,N   ) 
C
      ENDIF
      !>
      !!After these calls have been done, average values of the main 
      !!prognostic variables over the modelled area are determined by 
      !!performing weighted averages over the four subareas, and checks 
      !!are carried out to identify and remove vanishingly small values. 
      !!First the bedrock temperature in the third soil layer, the total 
      !!runoff and the runoff temperature are calculated. Then the total 
      !!runoff and the overland flow, interflow and baseflow are 
      !!converted from units of m to \f$kg m^{-2} s^{-1}\f$. The total surface water 
      !!vapour flux over the modelled area is updated to account for the 
      !!residual amounts of evaporative demand over the four subareas 
      !!that could not be supplied by surface stores (WLSTCS, WLSTGS, 
      !!WLOSTC and WLOSTG, variables that are defined internally in this 
      !!subroutine).
      !!
      !!The temperature of the vegetation canopy TCAN and the amount of 
      !!intercepted liquid water RCAN are calculated as weighted averages 
      !!over the two canopy subareas. A flag is set to trigger a call to 
      !!abort if TCAN is less than -100 C or greater than 100 C. If RCAN 
      !!is vanishingly small, it is added to the overland flow and to the 
      !!total runoff, and their respective temperatures are recalculated. 
      !!The diagnostic arrays ROFC, ROVG, PCPG and HTCC are updated, and 
      !!RCAN is set to zero. The amount of intercepted snow SNCAN is 
      !!likewise calculated as a weighted average over the two canopy 
      !!subareas. If SNCAN is vanishingly small, it is added to the 
      !!overland flow and to the total runoff, and their respective 
      !!temperatures are recalculated. The diagnostic arrays ROFC, ROVG, 
      !!PCPG and HTCC are updated, and SNCAN is set to zero. If there is 
      !!no canopy present, TCAN is set to zero.
      !!
      !!At the end of the 600 loop, the depth of ponded water ZPOND and 
      !!its temperature TPOND over the modelled area are calculated as 
      !!weighted averages over the four subareas. If ZPOND is vanishingly 
      !!small, then as in the case of intercepted water, it is added to 
      !!the overland flow and to the total runoff, and their respective 
      !!temperatures are recalculated. The diagnostic array HTC is 
      !!updated, and ZPOND and TPOND are set to zero.
      !!
C
C     * AVERAGE RUNOFF AND PROGNOSTIC VARIABLES OVER FOUR GRID CELL
C     * SUBAREAS.
C
      JPTBAD=0
      KPTBAD=0
      LPTBAD=0


      DO 600 I=IL1,IL2 
          TBASE (I)=FCS(I)*(TBASCS(I)+TFREZ) + 
     1              FGS(I)*(TBASGS(I)+TFREZ) +
     2              FC (I)*(TBASC (I)+TFREZ) + 
     3              FG (I)*(TBASG (I)+TFREZ)
          RUNOFF(I)=FCS(I)*RUNFCS(I) + FGS(I)*RUNFGS(I) +
     1              FC (I)*RUNFC (I) + FG (I)*RUNFG (I) 
          IF(RUNOFF(I).GT.0.0) 
     1        TRUNOF(I)=(FCS(I)*RUNFCS(I)*TRNFCS(I) + 
     2                   FGS(I)*RUNFGS(I)*TRNFGS(I) +
     3                   FC (I)*RUNFC (I)*TRUNFC(I) + 
     4                   FG (I)*RUNFG (I)*TRUNFG(I))/RUNOFF(I) 
          RUNOFF(I)=RUNOFF(I)*RHOW/DELT                                       
          OVRFLW(I)=OVRFLW(I)*RHOW/DELT
          SUBFLW(I)=SUBFLW(I)*RHOW/DELT
          BASFLW(I)=BASFLW(I)*RHOW/DELT
          EVAP  (I)=EVAP(I)-(FCS(I)*WLSTCS(I)+FGS(I)*WLSTGS(I)+
     1              FC(I)*WLOSTC(I)+FG(I)*WLOSTG(I))/DELT
          QFLUX(I)=-EVAP(I)/RHOAIR(I)                                   
          IF((FC(I)+FCS(I)).GT.0.)                                  THEN
              TCAN(I)=(FCS(I)*TCANS(I)*CHCAPS(I)+FC(I)*TCANO(I)*              
     1                CHCAP(I))/(FCS(I)*CHCAPS(I)+FC(I)*CHCAP(I))                 
              RCAN(I)= FCS(I)*RAICNS(I) + FC (I)*RAICAN(I)                            
              IF(TCAN(I).LT.173.16 .OR. TCAN(I).GT.373.16) JPTBAD=I
              IF(RCAN(I).LT.0.0) RCAN(I)=0.0
              IF(RCAN(I).LT.1.0E-5 .AND. RCAN(I).GT.0.0) THEN
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TCAN(I)*RCAN(I)/
     1                DELT)/(OVRFLW(I)+RCAN(I)/DELT)
                  OVRFLW(I)=OVRFLW(I)+RCAN(I)/DELT
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TCAN(I)*RCAN(I)/
     1                DELT)/(RUNOFF(I)+RCAN(I)/DELT)
                  RUNOFF(I)=RUNOFF(I)+RCAN(I)/DELT
                  ROFC(I)=ROFC(I)+RCAN(I)/DELT
                  ROVG(I)=ROVG(I)+RCAN(I)/DELT
                  PCPG(I)=PCPG(I)+RCAN(I)/DELT
                  HTCC(I)=HTCC(I)-TCAN(I)*SPHW*RCAN(I)/DELT
                  RCAN(I)=0.0
              ENDIF
              SNCAN  (I)=FCS(I)*SNOCNS(I) + FC (I)*SNOCAN(I)                            
              IF(SNCAN(I).LT.0.0) SNCAN(I)=0.0
              IF(SNCAN(I).LT.1.0E-5 .AND. SNCAN(I).GT.0.0) THEN
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TCAN(I)*SNCAN(I)/
     1                DELT)/(OVRFLW(I)+SNCAN(I)/DELT)
                  OVRFLW(I)=OVRFLW(I)+SNCAN(I)/DELT
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TCAN(I)*SNCAN(I)/
     1                DELT)/(RUNOFF(I)+SNCAN(I)/DELT)
                  RUNOFF(I)=RUNOFF(I)+SNCAN(I)/DELT
                  ROFC(I)=ROFC(I)+SNCAN(I)/DELT
                  ROVG(I)=ROVG(I)+SNCAN(I)/DELT
                  PCPG(I)=PCPG(I)+SNCAN(I)/DELT
                  HTCC(I)=HTCC(I)-TCAN(I)*SPHICE*SNCAN(I)/DELT
                  SNCAN(I)=0.0
              ENDIF
          ELSE                                                                
              TCAN(I)=0.0
          ENDIF                                                               
          IF(ZPNDCS(I).GT.0. .OR. ZPNDGS(I).GT.0. .OR.
     1                ZPONDC(I).GT.0. .OR. ZPONDG(I).GT.0.)    THEN 
              ZPOND(I)=(FCS(I)*ZPNDCS(I)+FGS(I)*ZPNDGS(I)+
     1                  FC (I)*ZPONDC(I)+FG (I)*ZPONDG(I))
              TPOND(I)=(FCS(I)*(TPNDCS(I)+TFREZ)*ZPNDCS(I)+
     1                  FGS(I)*(TPNDGS(I)+TFREZ)*ZPNDGS(I)+
     2                  FC (I)*(TPONDC(I)+TFREZ)*ZPONDC(I)+
     3                  FG (I)*(TPONDG(I)+TFREZ)*ZPONDG(I))/
     4                  ZPOND(I)
              IF(ZPOND(I).LT.0.0) ZPOND(I)=0.0
              IF(ZPOND(I).LT.1.0E-8 .AND. ZPOND(I).GT.0.0) THEN
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TPOND(I)*RHOW*
     1                ZPOND(I)/DELT)/(OVRFLW(I)+RHOW*ZPOND(I)/DELT)
                  OVRFLW(I)=OVRFLW(I)+RHOW*ZPOND(I)/DELT
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TPOND(I)*RHOW*
     1                ZPOND(I)/DELT)/(RUNOFF(I)+RHOW*ZPOND(I)/DELT)
                  RUNOFF(I)=RUNOFF(I)+RHOW*ZPOND(I)/DELT
                  HTC(I,1)=HTC(I,1)-TPOND(I)*HCPW*ZPOND(I)/DELT
                  TPOND(I)=TFREZ
                  ZPOND(I)=0.0                            
              ENDIF
         ELSE
              ZPOND(I)=0.0
              TPOND(I)=TFREZ
         ENDIF
  600 CONTINUE
	
      !>
      !!In the 650 loop, values of the snow prognostic variables are 
      !!calculated as weighted averages over the four subareas. The 
      !!weightings for the subareas include the four internally-defined 
      !!CLASSW variables XSNOCS, XSNOGS, XSNOWC and XSNOWG, which are set 
      !!in subroutine CHKWAT to 1 if the subarea snow depth is greater 
      !!than zero, and to zero otherwise. If the snow depth over the CS 
      !!and GS subareas is greater than zero (meaning that there was a 
      !!pre-existing snow cover at the beginning of the time step), the 
      !!average snow albedo ALBSNO is preferentially set to the average 
      !!over these two subareas. Otherwise ALBSNO is set to the average 
      !!over the C and G subareas (where snow has just been added in the 
      !!current time step). The snow temperature TSNOW and density RHOSNO 
      !!are set to weighted averages over the four subareas, using the 
      !!internally-defined subarea volumetric heat capacities 
      !!HCPSCS/GS/C/G and RHOSCS/GS/C/G. Finally the snow depth ZSNOW is 
      !!calculated from the subarea depths; the liquid water content of 
      !!the snow pack WSNOW is obtained as a weighted average over the CS 
      !!and GS subareas (assuming that freshly fallen snow does not yet 
      !!contain liquid water); and the snow mass is determined from ZSNOW 
      !!and RHOSNO. As in the case of intercepted and ponded water, if 
      !!the snow mass is vanishingly small it and its liquid water 
      !!content are added to the overland flow and to the total runoff, 
      !!and their respective temperatures are recalculated. The 
      !!diagnostic arrays ROFN, PCPG and HTCS are updated, and TSNOW, 
      !!RHOSNO, SNO and WSNOW are set to zero. Flags are set to trigger 
      !!calls to abort if TSNOW is less than 0 K or greater than 0.001 C. 
      !!Finally, the three abort flags set thus far are checked, and 
      !!calls to abort are performed if they are greater than zero.
      !!
      DO 650 I=IL1,IL2     
          IF(ZSNOCS(I).GT.0. .OR. ZSNOGS(I).GT.0. .OR.
     1       ZSNOWC(I).GT.0. .OR. ZSNOWG(I).GT.0.)              THEN                                             
              IF(ZSNOCS(I).GT.0. .OR. ZSNOGS(I).GT.0.)    THEN                         
                  ALBSNO(I)=(FCS(I)*ALBSCS(I)*XSNOCS(I)+
     1                       FGS(I)*ALBSGS(I)*XSNOGS(I))/
     2                      (FCS(I)*XSNOCS(I)+FGS(I)*XSNOGS(I))                   
              ELSE                                                            
                  ALBSNO(I)=(FC (I)*ALBSC(I)*XSNOWC(I) +
     1                       FG (I)*ALBSG(I)*XSNOWG(I))/
     2                      (FC (I)*XSNOWC(I)+FG (I)*XSNOWG(I))                     
              ENDIF                                                           
              TSNOW(I)=(FCS(I)*(TSNOCS(I)+TFREZ)*HCPSCS(I)*
     1                  ZSNOCS(I)*XSNOCS(I) +                
     2                  FGS(I)*(TSNOGS(I)+TFREZ)*HCPSGS(I)*
     3                  ZSNOGS(I)*XSNOGS(I) +                      
     4                  FC (I)*(TSNOWC(I)+TFREZ)*HCPSC(I)*
     5                  ZSNOWC(I)*XSNOWC(I) +                          
     6                  FG (I)*(TSNOWG(I)+TFREZ)*HCPSG(I)*
     7                  ZSNOWG(I)*XSNOWG(I))/                         
     8                 (FCS(I)*HCPSCS(I)*ZSNOCS(I)*XSNOCS(I) +                               
     9                  FGS(I)*HCPSGS(I)*ZSNOGS(I)*XSNOGS(I) +                                
     A                  FC (I)*HCPSC(I)*ZSNOWC(I)*XSNOWC(I) +                                 
     B                  FG (I)*HCPSG(I)*ZSNOWG(I)*XSNOWG(I))
              RHOSNO(I)=(FCS(I)*RHOSCS(I)*ZSNOCS(I)*XSNOCS(I) +                         
     1                   FGS(I)*RHOSGS(I)*ZSNOGS(I)*XSNOGS(I) +                                
     2                   FC (I)*RHOSC(I)*ZSNOWC(I)*XSNOWC(I) +                                 
     3                   FG (I)*RHOSG(I)*ZSNOWG(I)*XSNOWG(I))/                                
     4                  (FCS(I)*ZSNOCS(I)*XSNOCS(I) +
     5                   FGS(I)*ZSNOGS(I)*XSNOGS(I) +                 
     6                   FC (I)*ZSNOWC(I)*XSNOWC(I) +
     7                   FG (I)*ZSNOWG(I)*XSNOWG(I))                    
              ZSNOW(I)=FCS(I)*ZSNOCS(I) + FGS(I)*ZSNOGS(I) +
     1                 FC (I)*ZSNOWC(I) + FG (I)*ZSNOWG(I)
              WSNOW(I)=FCS(I)*WSNOCS(I) + FGS(I)*WSNOGS(I) 
              SNO(I)=ZSNOW(I)*RHOSNO(I)                                       
              IF(SNO(I).LT.0.0) SNO(I)=0.0
C                                                                       
C           * LIMIT SNOW MASS TO A MAXIMUM OF 10 METRES TO AVOID        
C           * SNOW PILING UP AT EDGES OF GLACIERS AT HIGH ELEVATIONS.   
C           * THIS IS TARGETTED AS OVERLAND RUNOFF.                     
C                                                                       
              IF(ZSNOW(I).GT.10.0) THEN                                 
                  SNOROF=(ZSNOW(I)-10.0)*RHOSNO(I)                      
                  WSNROF=WSNOW(I)*SNOROF/SNO(I)                         
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TSNOW(I)*(SNOROF+      
     1                      WSNROF)/DELT)/(OVRFLW(I)+(SNOROF+WSNROF)/   
     2                      DELT)                                       
                  OVRFLW(I)=OVRFLW(I)+(SNOROF+WSNROF)/DELT              
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TSNOW(I)*(SNOROF+      
     1                      WSNROF)/DELT)/(RUNOFF(I)+(SNOROF+WSNROF)/   
     2                      DELT)                                       
                  RUNOFF(I)=RUNOFF(I)+(SNOROF+WSNROF)/DELT              
                  ROFN(I)=ROFN(I)+(SNOROF+WSNROF)/DELT                  
                  PCPG(I)=PCPG(I)+(SNOROF+WSNROF)/DELT                  
                  HTCS(I)=HTCS(I)-TSNOW(I)*(SPHICE*SNOROF+SPHW*         
     1                    WSNROF)/DELT                                  
                  SNO(I)=SNO(I)-SNOROF                                  
                  WSNOW(I)=WSNOW(I)-WSNROF                              
                  ZSNOW(I)=10.0                                         
              ENDIF                                                     
              IF(SNO(I).LT.1.0E-2 .AND. SNO(I).GT.0.0) THEN
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TSNOW(I)*(SNO(I)+
     1                WSNOW(I))/DELT)/(OVRFLW(I)+(SNO(I)+WSNOW(I))/
     2                DELT)
                  OVRFLW(I)=OVRFLW(I)+(SNO(I)+WSNOW(I))/DELT
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TSNOW(I)*(SNO(I)+
     1                WSNOW(I))/DELT)/(RUNOFF(I)+(SNO(I)+WSNOW(I))/
     2                DELT)
                  RUNOFF(I)=RUNOFF(I)+(SNO(I)+WSNOW(I))/DELT
                  ROFN(I)=ROFN(I)+(SNO(I)+WSNOW(I))/DELT
                  PCPG(I)=PCPG(I)+(SNO(I)+WSNOW(I))/DELT
                  HTCS(I)=HTCS(I)-TSNOW(I)*(SPHICE*SNO(I)+SPHW*
     1                WSNOW(I))/DELT
                  TSNOW(I)=0.0
                  RHOSNO(I)=0.0
                  SNO(I)=0.0                            
                  WSNOW(I)=0.0
              ENDIF
          ELSE                                                                
              TSNOW(I)=0.0                                                    
              RHOSNO(I)=0.0                                                   
              SNO(I)=0.0                                                      
              WSNOW(I)=0.0
              ZSNOW(I)=0.0                                              
          ENDIF
C
          IF(TSNOW(I).LT.0.0) KPTBAD=I
          IF((TSNOW(I)-TFREZ).GT.1.0E-3) LPTBAD=I
  650 CONTINUE
C
      IF(JPTBAD.NE.0)                                               THEN
          WRITE(6,6625) JPTBAD,JL,TCAN(JPTBAD)
 6625     FORMAT('0AT (I,J)= (',I3,',',I3,'), TCAN = ',F10.5)
          CALL XIT('CLASSW2',-2)
      ENDIF
C
      IF(KPTBAD.NE.0)                                               THEN
          WRITE(6,6626) KPTBAD,JL,TSNOW(KPTBAD)
 6626     FORMAT('0AT (I,J)= (',I3,',',I3,'), TSNOW = ',F10.5)
          CALL XIT('CLASSW2',-3)
      ENDIF
C
      IF(LPTBAD.NE.0)                                               THEN
          WRITE(6,6626) LPTBAD,JL,TSNOW(LPTBAD)
          CALL XIT('CLASSW2',-4)
      ENDIF
C
      !>
      !!In the 700 loop, the temperature of each soil layer is calculated 
      !!as a weighted average over the four subareas. In the case of the 
      !!third soil layer., if the standard three-layer configuration is 
      !!being modelled (with a very thick third soil layer of 3.75 m), 
      !!the subarea layer temperatures TBARCS/GS/C/G and the layer heat 
      !!capacities HCPCS/GS/C/G apply to the permeable depth DELZW of the 
      !!layer, and the bedrock temperature TBASE and the rock heat 
      !!capacity HCPSND to the remainder, DELZ-DELZW. The averaging is 
      !!carried out accordingly. In all other soil layers, the layer 
      !!temperature applies to the whole thickness, whose heat capacity 
      !!is a weighted average of HCPCS/GS/C/G over DELZW and HCPSND over 
      !!DELZ-DELZW. The volumetric liquid water content THLIQ, the 
      !!volumetric frozen water content THICE, and the heat flux at the 
      !!soil layer interfaces GFLUX are calculated as simple weighted 
      !!averages over the subareas. A flag is set to trigger a call to 
      !!abort if the soil layer temperature is less than -100 C or 
      !!greater than 100 C, and after the end of the loop, a call to 
      !!abort is performed if the flag is greater than zero.
      !!
      IPTBAD=0
      DO 700 J=1,IG
      DO 700 I=IL1,IL2
          IF(IG.EQ. 3 .AND. J.EQ.IG .AND. ISAND(I,1).GT.-4)    THEN
              TBAR(I,J)=((FCS(I)*(TBARCS(I,J)+TFREZ)*HCPCS(I,J) +
     1                   FGS(I)*(TBARGS(I,J)+TFREZ)*HCPGS(I,J) +
     2                   FC (I)*(TBARC (I,J)+TFREZ)*HCPCO(I,J) +             
     3                   FG (I)*(TBARG (I,J)+TFREZ)*HCPGO(I,J))*
     4                   DELZW(I,J)+TBASE(I)*HCPSND*
     5                   (DELZ(J)-DELZW(I,J)))/
     4                  ((FCS(I)*HCPCS(I,J) + FGS(I)*HCPGS(I,J) +
     5                   FC (I)*HCPCO(I,J) + FG (I)*HCPGO(I,J))*
     8                   DELZW(I,J)+HCPSND*(DELZ(J)-DELZW(I,J)))             
          ELSE
              TBAR(I,J)=(FCS(I)*(TBARCS(I,J)+TFREZ)*(DELZW(I,J)*
     1                   HCPCS(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND)+
     2                   FGS(I)*(TBARGS(I,J)+TFREZ)*(DELZW(I,J)*
     3                   HCPGS(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND)+
     4                   FC (I)*(TBARC (I,J)+TFREZ)*(DELZW(I,J)*
     5                   HCPCO(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND)+             
     6                   FG (I)*(TBARG (I,J)+TFREZ)*(DELZW(I,J)*
     7                   HCPGO(I,J)+(DELZ(J)-DELZW(I,J))*HCPSND))/
     8                  (FCS(I)*(DELZW(I,J)*HCPCS(I,J)+
     9                   (DELZ(J)-DELZW(I,J))*HCPSND) + 
     A                   FGS(I)*(DELZW(I,J)*HCPGS(I,J)+
     B                   (DELZ(J)-DELZW(I,J))*HCPSND) +
     C                   FC (I)*(DELZW(I,J)*HCPCO(I,J)+
     D                   (DELZ(J)-DELZW(I,J))*HCPSND) + 
     E                   FG (I)*(DELZW(I,J)*HCPGO(I,J)+
     F                   (DELZ(J)-DELZW(I,J))*HCPSND))              
          ENDIF




          THLIQ(I,J)=FCS(I)*THLQCS(I,J)+FGS(I)*THLQGS(I,J)+
     1               FC (I)*THLQCO(I,J)+FG (I)*THLQGO(I,J)                                   
          THICE(I,J)=FCS(I)*THICCS(I,J)+FGS(I)*THICGS(I,J)+
     1               FC (I)*THICCO(I,J)+FG (I)*THICGO(I,J)
          GFLUX(I,J)=FCS(I)*GFLXCS(I,J)+FGS(I)*GFLXGS(I,J)+
     1               FC (I)*GFLXC (I,J)+FG (I)*GFLXG (I,J)
C     ipy test
C          IF(THLIQ(I,J).GT.THFC(I,J))                               THEN
C              BASFLW(I)=BASFLW(I)+(THLIQ(I,J)-THFC(I,J))*DELZW(I,J)*
C     1            RHOW/DELT
C              RUNOFF(I)=RUNOFF(I)+(THLIQ(I,J)-THFC(I,J))*DELZW(I,J)*
C     1            RHOW/DELT
C              HTC(I,J)=HTC(I,J)-TBAR(I,J)*(THLIQ(I,J)-THFC(I,J))*
C     1            HCPW*DELZW(I,J)/DELT
C              THLIQ(I,J)=THFC(I,J)
C          ENDIF
          IF(TBAR(I,1).LT.173.16 .OR. TBAR(I,1).GT.373.16) IPTBAD=I
  700 CONTINUE                   
C
      IF(IPTBAD.NE.0)                                               THEN
          WRITE(6,6600) IPTBAD,JL,TBAR(IPTBAD,1)
 6600     FORMAT('0AT (I,J)= (',I3,',',I3,'), TBAR(1) = ',F10.5)
          CALL XIT('CLASSW2',-1)
      ENDIF
C
      !>Finally, subroutine CGROW is called to update the vegetation 
      !!growth index.
      !!
      CALL CGROW(GROWTH,TBAR,TA,FC,FCS,ILG,IG,IL1,IL2,JL)
C                                                                                  
      RETURN  
	WRITE(*,*) 'THLQCO in 1232:'
	WRITE(*,*) THLQCO

                                                                    
      END        
