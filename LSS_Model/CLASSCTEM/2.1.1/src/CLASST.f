!>\file
!>Purpose: Call subroutines to perform surface energy budget calculations.
!>

      SUBROUTINE CLASST (TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG,
     1   THICEC, THICEG, HCPC,   HCPG,   TCTOPC, TCBOTC, TCTOPG, TCBOTG,
     2   GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,   G12CS,  G12GS,  
     3   G23C,   G23G,   G23CS,  G23GS,  QFREZC, QFREZG, QMELTC, QMELTG, 
     4   EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS, TCANO,  TCANS,  
     5   RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP,  CHCAPS, TPONDC, TPONDG,  
     6   TPNDCS, TPNDGS, TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     7   ITERCT, CDH,    CDM,    QSENS,  TFLUX,  QEVAP,  EVAP,          
     8   EVPPOT, ACOND,  EVAPB,  GT,     QG, 
     9   ST,     SU,     SV,     SQ,     SRH,
     A   GTBS, SFCUBS, SFCVBS, USTARBS,                                 
     B   FSGV,   FSGS,   FSGG,   FLGV,   FLGS,   FLGG,   
     C   HFSC,   HFSS,   HFSG,   HEVC,   HEVS,   HEVG,   HMFC,   HMFN, 
     D   HTCC,   HTCS,   HTC,    QFCF,   QFCL,   DRAG,   WTABLE, ILMO,    
     E   UE,     HBL,    TAC,    QAC,    ZREFM,  ZREFH,  ZDIAGM, ZDIAGH, 
     F   VPD,    TADP,   RHOAIR, QSWINV, QSWINI, QLWIN,  UWIND,  VWIND,   
     G   TA,     QA,     PADRY,  FC,     FG,     FCS,    FGS,    RBCOEF,
     H   FSVF,   FSVFS,  PRESSG, VMOD,   ALVSCN, ALIRCN, ALVSG,  ALIRG,  
     I   ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, ALIRSC,
     J   TRVSCN, TRIRCN, TRVSCS, TRIRCS, RC,     RCS,    WTRG,   QLWAVG,
     K   FRAINC, FSNOWC, FRAICS, FSNOCS, CMASSC, CMASCS, DISP,   DISPS,  
     L   ZOMLNC, ZOELNC, ZOMLNG, ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS, 
     M   TBAR,   THLIQ,  THICE,  TPOND,  ZPOND,  TBASE,  TCAN,   TSNOW,  
     N   ZSNOW,  RHOSNO, WSNOW,  THPOR,  THLRET, THLMIN, THFC,   THLW,
     O   TRSNOWC, TRSNOWG, ALSNO, FSSB,  FROOT,  FROOTS,
     P   RADJ,   PCPR,   HCPS,   TCS,    TSFSAV, DELZ,   DELZW,  ZBOTW,  
     Q   FTEMP,  FVAP,   RIB,   
     R   ISAND,
     S   AILCG,          AILCGS,         FCANC,          FCANCS,
     T   CO2CONC,        CO2I1CG,        CO2I1CS,        CO2I2CG,
     U   CO2I2CS,        COSZS,          XDIFFUS,        SLAI,
     V   ICTEM,          ctem_on,       RMATCTEM,       FCANCMX,
     W   L2MAX,          NOL2PFTS,       CFLUXCG,        CFLUXCS,
     X   ANCSVEG,        ANCGVEG,        RMLCSVEG,       RMLCGVEG,
     Y   TCSNOW, GSNOW,                                                 
     Z   ITC,    ITCG,   ITG,    ILG,    IL1,IL2,JL,N,   IC,     
     +   IG,     IZREF,  ISLFD,  NLANDCS,NLANDGS,NLANDC, NLANDG, NLANDI,
     +   NBS, ISNOALB,LFSTATUS,DAYL, DAYL_MAX)
C
C     * AUG 30/16 - J.Melton    Replace ICTEMMOD with ctem_on (logical switch).
C     * AUG 04/15 - M.LAZARE.   SPLIT FROOT INTO TWO ARRAYS, FOR CANOPY
C     *                         AREAS WITH AND WITHOUT SNOW.
C     * JUL 22/15 - D.VERSEGHY. CHANGES TO TSOLVC AND TSOLVE CALLS.
C     * FEB 09/15 - D.VERSEGHY. New version for gcm18 and class 3.6:
C     *                         - Revised calls to revised TPREP for
C     *                           initialization of SRH and SLDIAG.
C     *                         - Input {THFC,THLW} (from CLASSB) replace
C     *                           work arrays {FIELDSM,WILTSM}.
C     *                         - Calculation of new bare-soil fields
C     *                           {GTBS,SFCUBS,SFCVBS,USTARBS}.
C     * SEP 09/14 - D.VERSEGHY/M.LAZARE. CORRECTIONS TO SCREEN LEVEL
C     *                         DIAGNOSTIC CALCULATIONS.
C     * AUG 19/13 - M.LAZARE.   REMOVE CALCULATION AND REFERENCES TO    
C     *                         "QFLUX" (NOW DONE IN CLASSW).           
C     * JUN 21/13 - M.LAZARE.   REVISED CALL TO TPREP TO SUPPORT ADDING 
C     *                         INITIALIZATION OF "GSNOW".              
C     * JUN 10/13 - M.LAZARE/   ADD SUPPORT FOR "ISNOALB" FORMULATION.  
C     *             M.NAMAZI.                                           
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
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER NLANDCS!<Number of modelled areas that contain subareas of canopy over 
                     !<bare ground / bare ground / canopy over snow /snow
      INTEGER NLANDGS!<Number of modelled areas that contain subareas of canopy over 
                     !<bare ground / bare ground / canopy over snow /snow
      INTEGER NLANDC !<Number of modelled areas that contain subareas of canopy over 
                     !<bare ground / bare ground / canopy over snow /snow
      INTEGER NLANDG !<Number of modelled areas that contain subareas of canopy over 
                     !<bare ground / bare ground / canopy over snow /snow
      INTEGER NLANDI !<Number of modelled areas that are ice sheets [ ]
      INTEGER ISNOW  !<
      INTEGER N      !<
      INTEGER NBS    !<
      INTEGER ISNOALB!<  
C
      INTEGER ITC   !<Flag to select iteration scheme for canopy temperature
      INTEGER ITCG  !<Flag to select iteration scheme for surface under canopy
      INTEGER ITG   !<Flag to select iteration scheme for ground or snow surface
      INTEGER ILG   !<
      INTEGER IL1   !<
      INTEGER IL2   !<
      INTEGER JL    !<
      INTEGER IC    !<
      INTEGER IG    !<
      INTEGER IZREF !<Flag governing treatment of surface roughness length
      INTEGER ISLFD !<Flag governing options for surface stability functions and diagnostic calculations
      INTEGER I,J
C
C     * OUTPUT FIELDS.
C                          
      REAL ACOND (ILG) !<Diagnosed product of drag coefficient and wind speed over modelled area \f$[m s^{-1} ]\f$
      REAL CDH   (ILG) !<Surface drag coefficient for heat \f$[ ] (C_{DH} )\f$
      REAL CDM   (ILG) !<Surface drag coefficient for momentum \f$[ ] (C_{DM} )\f$
      REAL CHCAP (ILG) !<Surface drag coefficient for momentum \f$[ ] (C_{DM} )\f$
      REAL CHCAPS(ILG) !<Heat capacity of canopy over bare ground \f$[J m^{-2} K^{-1} ]\f$
      REAL DRAG  (ILG) !<Surface drag coefficient under neutral stability [ ]

      REAL EVAP  (ILG) !<Diagnosed total surface water vapour flux over modelled area \f$[kg m^{-2} s^{-1} ]\f$
      REAL EVAPB (ILG) !<Evaporation efficiency at ground surface [ ]
      REAL EVAPC (ILG) !<Evaporation from vegetation over ground \f$[m s^{-1} ]\f$
      REAL EVAPCG(ILG) !<Evaporation from ground under vegetation \f$[m s^{-1} ]\f$
      REAL EVAPCS(ILG) !<Evaporation from vegetation over snow \f$[m s^{-1} ]\f$
      REAL EVAPG (ILG) !<Evaporation from bare ground \f$[m s^{-1} ]\f$
      REAL EVAPGS(ILG) !<Evaporation from snow on bare ground \f$[m s^{-1} ]\f$
      REAL EVPCSG(ILG) !<Evaporation from snow under vegetation \f$[m s^{-1} ]\f$
      REAL EVPPOT(ILG) !<Diagnosed potential evapotranspiration \f$[kg m^{-2} s^{-1} ] (E_p )\f$
      REAL FLGG  (ILG) !<Diagnosed net longwave radiation at soil surface \f$[W m^{-2} ]\f$
      REAL FLGS  (ILG) !<Diagnosed net longwave radiation at snow surface \f$[W m^{-2} ]\f$
      REAL FLGV  (ILG) !<Diagnosed net longwave radiation on vegetation canopy \f$[W m^{-2} ]\f$
      REAL FSGG  (ILG) !<Diagnosed net shortwave radiation at soil surface \f$[W m^{-2} ]\f$
      REAL FSGS  (ILG) !<Diagnosed net shortwave radiation at snow surface \f$[W m^{-2} ]\f$
      REAL FSGV  (ILG) !<Diagnosed net shortwave radiation on vegetation canopy \f$[W m^{-2} ]\f$
      REAL FTEMP (ILG) !<
      REAL FVAP  (ILG) !<

      REAL G12C  (ILG) !<Subarea heat flux between first and second soil layers \f$[W m^{-2} ]\f$
      REAL G12CS (ILG) !<Subarea heat flux between first and second soil layers \f$[W m^{-2} ]\f$
      REAL G12G  (ILG) !<Subarea heat flux between first and second soil layers \f$[W m^{-2} ]\f$
      REAL G12GS (ILG) !<Subarea heat flux between first and second soil layers \f$[W m^{-2} ]\f$
      REAL G23C  (ILG) !<Subarea heat flux between second and third soil layers \f$[W m^{-2} ]\f$
      REAL G23CS (ILG) !<Subarea heat flux between second and third soil layers \f$[W m^{-2} ]\f$
      REAL G23G  (ILG) !<Subarea heat flux between second and third soil layers \f$[W m^{-2} ]\f$
      REAL G23GS (ILG) !<Subarea heat flux between second and third soil layers \f$[W m^{-2} ]\f$
      REAL GT    (ILG) !<Diagnosed effective surface black-body temperature \f$[K] (T_{0,eff} )\f$
      REAL GTBS  (ILG) !<
      REAL GZEROC(ILG) !<Subarea heat flux at soil surface \f$[W m^{-2} ]\f$
      REAL GZEROG(ILG) !<Subarea heat flux at soil surface \f$[W m^{-2} ]\f$
      REAL GZROCS(ILG) !<Subarea heat flux at soil surface \f$[W m^{-2} ]\f$
      REAL GZROGS(ILG) !<Subarea heat flux at soil surface \f$[W m^{-2} ]\f$

      REAL HBL   (ILG)    !<Height of the atmospheric boundary layer [m]
      REAL HCPC  (ILG,IG) !<Heat capacity of soil layers under vegetation \f$[J m^{-3} K^{-1} ]\f$
      REAL HCPG  (ILG,IG) !<Heat capacity of soil layers in bare areas \f$[J m^{-3} K^{-1} ]\f$
      REAL HEVC  (ILG)    !<Diagnosed latent heat flux on vegetation canopy \f$[W m^{-2} ]\f$
      REAL HEVG  (ILG)    !<Diagnosed latent heat flux at soil surface \f$[W m^{-2} ]\f$
      REAL HEVS  (ILG)    !<Diagnosed latent heat flux at snow surface \f$[W m^{-2} ]\f$
      REAL HFSC  (ILG)    !<Diagnosed sensible heat flux on vegetation canopy \f$[W m^{-2} ]\f$
      REAL HFSG  (ILG)    !<Diagnosed sensible heat flux at soil surface \f$[W m^{-2} ]\f$
      REAL HFSS  (ILG)    !<Diagnosed sensible heat flux at snow surface \f$[W m^{-2} ]\f$
      REAL HMFC  (ILG)    !<Diagnosed energy associated with phase change of water on vegetation \f$[W m^{-2} ]\f$
      REAL HMFN  (ILG)    !<Diagnosed energy associated with phase change of water in snow pack \f$[W m^{-2} ]\f$
      REAL HTC   (ILG,IG) !<Diagnosed internal energy change of soil layer due to conduction 
                          !<and/or change in mass \f$[W m^{-2} ]\f$
      REAL HTCC  (ILG)    !<Diagnosed internal energy change of vegetation canopy due to conduction 
                          !<and/or change in mass \f$[W m^{-2} ]\f$
      REAL HTCS  (ILG)    !<Diagnosed internal energy change of snow pack due to conduction 
                          !<and/or change in mass \f$[W m^{-2} ]\f$

      REAL ILMO  (ILG)         !<Inverse of Monin-Obukhov roughness length \f$(m^{-1} ]\f$
      INTEGER ITERCT(ILG,6,50) !<Counter of number of iterations required to solve surface energy balance
                               !<for the elements of the four subareas

      REAL QAC   (ILG) !<Specific humidity of air within vegetation canopy space \f$[kg kg^{-1} ]\f$
      REAL QEVAP (ILG) !<Diagnosed total surface latent heat flux over modelled area \f$[W m^{-2} ]\f$
      REAL QFCF  (ILG) !<Sublimation from frozen water on vegetation \f$[kg m^{-2} s^{-1} ]\f$
      REAL QFCL  (ILG) !<Evaporation from liquid water on vegetation \f$[kg m^{-2} s^{-1} ]\f$

      REAL QFREZC(ILG) !<Heat sink to be used for freezing water on ground under canopy \f$[W m^{-2} ]\f$
      REAL QFREZG(ILG) !<Heat sink to be used for freezing water on bare ground \f$[W m^{-2} ]\f$
      REAL QG    (ILG) !<Diagnosed surface specific humidity \f$[kg kg^{-1} ]\f$
      REAL QLWAVG(ILG) !<Upwelling longwave radiation from land surface \f$[W m^{-2} ]\f$
      REAL QMELTC(ILG) !<Heat to be used for melting snow under canopy \f$[W m^{-2} ]\f$
      REAL QMELTG(ILG) !<Heat to be used for melting snow on bare ground \f$[W m^{-2} ]\f$
      REAL QSENS (ILG) !<Diagnosed total surface sensible heat flux over modelled area \f$[W m^{-2} ]\f$
      REAL RAICAN(ILG) !<Intercepted liquid water stored on canopy over ground \f$[kg m^{-2} ]\f$
      REAL RAICNS(ILG) !<Intercepted liquid water stored on canopy over snow \f$[kg m^{-2} ]\f$
      REAL RHOSCS(ILG) !<Density of snow under vegetation \f$[kg m^{-3} ]\f$
      REAL RHOSGS(ILG) !<Density of snow in bare areas \f$[kg m^{-3} ]\f$
      REAL RIB   (ILG) !<
      REAL SFCUBS(ILG) !<
      REAL SFCVBS(ILG) !<
      REAL SNOCAN(ILG) !<Intercepted frozen water stored on canopy over ground \f$[kg m^{-2} ]\f$
      REAL SNOCNS(ILG) !<Intercepted frozen water stored on canopy over snow \f$[kg m^{-2} ]\f$
      REAL SQ    (ILG) !<Diagnosed screen-level specific humidity \f$[kg kg^{-1} ]\f$
      REAL SRH   (ILG) !<Diagnosed screen-level relative humidity [%]
      REAL ST    (ILG) !<Diagnosed screen-level air temperature [K]
      REAL SU    (ILG) !<Diagnosed anemometer-level zonal wind \f$[m s^{-1} ]\f$
      REAL SV    (ILG) !<Diagnosed anemometer-level meridional wind \f$[m s^{-1} ]\f$

      REAL TAC   (ILG)    !<Temperature of air within vegetation canopy [K]
      REAL TBARC (ILG,IG) !<Subarea temperatures of soil layers [C]
      REAL TBARCS(ILG,IG) !<Subarea temperatures of soil layers [C]
      REAL TBARG (ILG,IG) !<Subarea temperatures of soil layers [C]
      REAL TBARGS(ILG,IG) !<Subarea temperatures of soil layers [C]
      REAL TCANO (ILG)    !<Temperature of canopy over ground [K]
      REAL TCANS (ILG)    !<Temperature of canopy over snow [K]
      REAL TCBOTC(ILG,IG) !<Thermal conductivity of soil at bottom of layer \f$[W m^{-1} K^{-1} ]\f$
      REAL TCBOTG(ILG,IG) !<Thermal conductivity of soil at bottom of layer \f$[W m^{-1} K^{-1} ]\f$
      REAL TCTOPC(ILG,IG) !<Thermal conductivity of soil at top of layer \f$[W m^{-1} K^{-1} ]\f$
      REAL TCTOPG(ILG,IG) !<Thermal conductivity of soil at top of layer \f$[W m^{-1} K^{-1} ]\f$
      REAL TFLUX (ILG)    !<Product of surface drag coefficient, wind speed and surface-air 
                          !<temperature difference \f$[K m s^{-1} ]\f$
      REAL THICEC(ILG,IG) !<Frozen water content of soil layers under vegetation \f$[m^3 m^{-3} ]\f$
      REAL THICEG(ILG,IG) !<Frozen water content of soil layers in bare areas \f$[m^3 m^{-3} ]\f$
      REAL THLIQC(ILG,IG) !<Liquid water content of soil layers under vegetation \f$[m^3 m^{-3} ]\f$
      REAL THLIQG(ILG,IG) !<Liquid water content of soil layers in bare areas \f$[m^3 m^{-3} ]\f$
      REAL TPONDC(ILG)    !<Subarea temperature of surface ponded water [C]
      REAL TPONDG(ILG)    !<Subarea temperature of surface ponded water [C]
      REAL TPNDCS(ILG)    !<Subarea temperature of surface ponded water [C]
      REAL TPNDGS(ILG)    !<Subarea temperature of surface ponded water [C]
      REAL TSFSAV(ILG,4)  !<Ground surface temperature over subarea [K]
      REAL TSNOCS(ILG)    !<Temperature of snow pack under vegetation [K]
      REAL TSNOGS(ILG)    !<Temperature of snow pack in bare areas [K]

      REAL UE    (ILG)  !<Friction velocity of air \f$[m s^{-1} ]\f$
      REAL USTARBS(ILG) !<
      REAL WSNOCS(ILG)  !<Liquid water content of snow pack under vegetation \f$[kg m^{-2} ]\f$
      REAL WSNOGS(ILG)  !<Liquid water content of snow pack in bare areas \f$[kg m^{-2} ]\f$
      REAL WTABLE(ILG)  !<Depth of water table in soil [m]
      REAL WTRG  (ILG)  !<Diagnosed residual water transferred into or out of the soil \f$[kg m^{-2} s^{-1} ]\f$
C
C
C     * INPUT FIELDS.
C
      REAL ZREFM (ILG) !<Reference height associated with forcing wind speed [m]
      REAL ZREFH (ILG) !<Reference height associated with forcing air temperature and humidity [m]
      REAL ZDIAGM(ILG) !<User-specified height associated with diagnosed anemometer-level wind speed [m]
      REAL ZDIAGH(ILG) !<User-specified height associated with diagnosed screen-level variables [m]
      REAL VPD   (ILG) !<Vapour pressure deficit [mb]
      REAL TADP  (ILG) !<Dew point temperature of air [K]
      REAL RHOAIR(ILG) !<Density of air \f$[kg m^{-3} ] (\rho_a )\f$
      REAL QSWINV(ILG) !<Visible radiation incident on horizontal surface \f$[W m^{-2} ]\f$
      REAL QSWINI(ILG) !<Near-infrared radiation incident on horizontal surface \f$[W m^{-2} ]\f$
      REAL QLWIN (ILG) !<Downwelling longwave radiation at bottom of atmosphere \f$[W m^{-2} ]\f$
      REAL UWIND (ILG) !<Zonal component of wind speed \f$[m s^{-1} ] (U_a )\f$
      REAL VWIND (ILG) !<Meridional component of wind speed \f$[m s^{-1} ] (V_a )\f$
      REAL TA    (ILG) !<Air temperature at reference height \f$[K] (T_a )\f$
      REAL QA    (ILG) !<Specific humidity at reference height \f$[kg kg^{-1} ]\f$
      REAL PADRY (ILG) !<Partial pressure of dry air \f$[Pa] (p_{dry} )\f$
      REAL FC    (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL FG    (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL FCS   (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL FGS   (ILG) !<Subarea fractional coverage of modelled area [ ]
      REAL RBCOEF(ILG) !<Parameter for calculation of leaf boundary resistance
      REAL FSVF  (ILG) !<Sky view factor for bare ground under canopy [ ]
      REAL FSVFS (ILG) !<Sky view factor for snow under canopy [ ]
      REAL PRESSG(ILG) !<Surface atmospheric pressure [Pa]
      REAL VMOD  (ILG) !<Wind speed at reference height \f$[m s^{-1} ]\f$
      REAL ALVSCN(ILG) !<Visible/near-IR albedo of vegetation over bare ground [ ]
      REAL ALIRCN(ILG) !<Visible/near-IR albedo of vegetation over bare ground [ ]
      REAL ALVSG (ILG) !<Visible/near-IR albedo of open bare ground [ ]
      REAL ALIRG (ILG) !<Visible/near-IR albedo of open bare ground [ ]
      REAL ALVSCS(ILG) !<Visible/near-IR albedo of vegetation over snow [ ]
      REAL ALIRCS(ILG) !<Visible/near-IR albedo of vegetation over snow [ ]
      REAL ALVSSN(ILG) !<Visible/near-IR albedo of open snow cover [ ]
      REAL ALIRSN(ILG) !<Visible/near-IR albedo of open snow cover [ ]
      REAL ALVSGC(ILG) !<Visible/near-IR albedo of bare ground under vegetation [ ]
      REAL ALIRGC(ILG) !<Visible/near-IR albedo of bare ground under vegetation [ ]
      REAL ALVSSC(ILG) !<Visible/near-IR albedo of snow under vegetation [ ]
      REAL ALIRSC(ILG) !<Visible/near-IR albedo of snow under vegetation [ ]
      REAL TRVSCN(ILG) !<Visible/near-IR transmissivity of vegetation over bare ground [ ]
      REAL TRIRCN(ILG) !<Visible/near-IR transmissivity of vegetation over bare ground [ ]
      REAL TRVSCS(ILG) !<Visible/near-IR transmissivity of vegetation over snow [ ]
      REAL TRIRCS(ILG) !<Visible/near-IR transmissivity of vegetation over snow [ ]
      REAL RC    (ILG) !<Stomatal resistance of vegetation over bare ground \f$[s m^{-1} ]\f$
      REAL RCS   (ILG) !<Stomatal resistance of vegetation over snow \f$[s m^{-1} ]\f$
      REAL FRAINC(ILG) !<Fractional coverage of canopy by liquid water over snow-free subarea [ ]
      REAL FSNOWC(ILG) !<Fractional coverage of canopy by frozen water over snow-free subarea [ ]
      REAL FRAICS(ILG) !<Fractional coverage of canopy by liquid water over snow-covered subarea [ ]
      REAL FSNOCS(ILG) !<Fractional coverage of canopy by frozen water over snow-covered subarea [ ]
      REAL CMASSC(ILG) !<Mass of canopy over bare ground \f$[kg m^{-2} ]\f$
      REAL CMASCS(ILG) !<Mass of canopy over snow \f$[kg m^{-2} ]\f$
      REAL DISP  (ILG) !<Displacement height of vegetation over bare ground [m] (d)
      REAL DISPS (ILG) !<Displacement height of vegetation over snow [m] (d)
      REAL ZOMLNC(ILG) !<Logarithm of roughness length for momentum of vegetation over bare ground [ ]
      REAL ZOELNC(ILG) !<Logarithm of roughness length for heat of vegetation over bare ground [ ]
      REAL ZOMLNG(ILG) !<Logarithm of roughness length for momentum of bare ground [ ]
      REAL ZOELNG(ILG) !<Logarithm of roughness length for heat of bare ground [ ]
      REAL ZOMLCS(ILG) !<Logarithm of roughness length for momentum of vegetation over snow [ ]
      REAL ZOELCS(ILG) !<Logarithm of roughness length for heat of vegetation over snow [ ]
      REAL ZOMLNS(ILG) !<Logarithm of roughness length for momentum of snow [ ]
      REAL ZOELNS(ILG) !<Logarithm of roughness length for heat of snow [ ]
      REAL TPOND (ILG) !<Temperature of ponded water [K]
      REAL ZPOND (ILG) !<Depth of ponded water on surface [m]
      REAL TBASE (ILG) !<Temperature of bedrock in third soil layer [K]
      REAL TCAN  (ILG) !<Vegetation canopy temperature [K]
      REAL TSNOW (ILG) !<Snowpack temperature [K]
      REAL ZSNOW (ILG) !<Depth of snow pack [m]
      REAL TRSNOWC(ILG)!<
      REAL RHOSNO(ILG) !<Density of snow \f$[kg m^{-3} ]\f$
      REAL WSNOW (ILG) !<Liquid water content of snow pack \f$[kg m^{-2} ]\f$
      REAL RADJ  (ILG) !<Latitude of grid cell (positive north of equator) [rad] \f$(\varphi)\f$
      REAL PCPR  (ILG) !<Surface precipitation rate \f$[kg m^{-2} s^{-1} ]\f$
C                                                                       
      REAL TRSNOWG(ILG,NBS), ALSNO(ILG,NBS), FSSB(ILG,NBS)              
C     
      REAL TBAR  (ILG,IG) !<Temperature of soil layers [K]
      REAL THLIQ (ILG,IG) !<Volumetric liquid water content of soil layers \f$[m^3 m^{-3} ]\f$
      REAL THICE (ILG,IG) !<Volumetric frozen water content of soil layers \f$[m^3 m^{-3} ]\f$
C
C     * SOIL PROPERTY ARRAYS.
C
      REAL THPOR (ILG,IG) !<Pore volume in soil layer \f$[m^3 m^{-3} ]\f$
      REAL THLRET(ILG,IG) !<Liquid water retention capacity for organic soil \f$[m^3 m^{-3} ]\f$
      REAL THLMIN(ILG,IG) !<Residual soil liquid water content remaining after freezing or evaporation \f$[m^3 m^{-3} ]\f$
      REAL THFC  (ILG,IG) !<Field capacity \f$[m^3 m^{-3} ]\f$
      REAL THLW  (ILG,IG)
      REAL HCPS  (ILG,IG) !<Heat capacity of soil material \f$[J m^{-3} K^{-1} ]\f$
      REAL TCS   (ILG,IG) !<Thermal conductivity of soil particles \f$[W m^{-1} K^{-1} ]\f$
      REAL DELZ  (IG)     !<Overall thickness of soil layer [m]
      REAL DELZW (ILG,IG) !<Permeable thickness of soil layer [m]
      REAL ZBOTW (ILG,IG) !<Depth to permeable bottom of soil layer [m]
      REAL FROOT (ILG,IG)
      REAL FROOTS(ILG,IG)
C
      INTEGER  ISAND (ILG,IG) !<Sand content flag
C
C     * CTEM-RELATED I/O FIELDS.
C

      REAL AILCG(ILG,ICTEM)   !< GREEN LAI FOR USE WITH PHOTOSYNTHESIS SUBTROUTINE FOR CANOPY OVER GROUND SUBAREA
      REAL AILCGS(ILG,ICTEM)  !< GREEN LAI FOR USE WITH PHOTOSYNTHESIS SUBTROUTINE FOR CANOPY OVER SNOW SUBAREA
      REAL FCANC(ILG,ICTEM)   !< FRACTIONAL COVERAGE OF 8 CARBON PFTs, CANOPY OVER GROUND
      REAL FCANCS(ILG,ICTEM)  !< FRACTIONAL COVERAGE OF 8 CARBON PFTs, CANOPY OVER SNOW
      REAL CO2CONC(ILG)       !< ATMOS. CO2 CONC. IN PPM
      REAL CO2I1CG(ILG,ICTEM) !< INTERCELLULAR CO2 CONC FOR 8 PFTs FOR CANOPY OVER GROUND SUBAREA (Pa) - FOR SINGLE/SUNLIT LEAF
      REAL CO2I1CS(ILG,ICTEM) !< SAME AS ABOVE BUT FOR SHADED LEAF
      REAL CO2I2CG(ILG,ICTEM) !< INTERCELLULAR CO2 CONC FOR 8 PFTs FOR CANOPY OVER SNOWSUBAREA (Pa) - FOR SINGLE/SUNLIT LEAF
      REAL CO2I2CS(ILG,ICTEM) !< SAME AS ABOVE BUT FOR SHADED LEAF
      REAL COSZS(ILG)         !< COSINE OF SUN'S ZENITH ANGLE
      REAL XDIFFUS(ILG)       !< FRACTION OF DIFFUSED RADIATION
      REAL SLAI(ILG,ICTEM)    !< STORAGE LAI. SEE PHTSYN SUBROUTINE FOR MORE DETAILS.
      REAL RMATCTEM(ILG,ICTEM,IG) !< FRACTION OF ROOTS IN EACH SOIL LAYER FOR EACH OF CTEM's 8 PFTs
      REAL FCANCMX(ILG,ICTEM) !< MAX. FRACTIONAL COVERAGE OF CTEM PFTs
      REAL ANCSVEG(ILG,ICTEM) !< NET PHOTOSYNTHETIC RATE FOR CTEM's 8 PFTs FOR CANOPY OVER SNOW SUBAREA
      REAL ANCGVEG(ILG,ICTEM) !< NET PHOTOSYNTHETIC RATE FOR CTEM's 8 PFTs FOR CANOPY OVER GROUND SUBAREA
      REAL RMLCSVEG(ILG,ICTEM)!< LEAF RESPIRATION RATE FOR CTEM's 8 PFTs FOR CANOPY OVER SNOW SUBAREA
      REAL RMLCGVEG(ILG,ICTEM)!< LEAF RESPIRATION RATE FOR CTEM's 8 PFTs FOR CANOPY OVER GROUND SUBAREA

      REAL CFLUXCG(ILG)
      REAL CFLUXCS(ILG)
C
      INTEGER ICTEM               !< 8 (CTEM's PLANT FUNCTIONAL TYPES)
      LOGICAL ctem_on             !< TRUE GIVES COUPLING TO CTEM
      INTEGER LFSTATUS(ILG,ICTEM) !< LEAF PHENOLOGICAL STATUS (SEE PHENOLOGY)
      REAL DAYL_MAX(ILG)          !< MAXIMUM DAYLENGTH FOR THAT LOCATION
      REAL DAYL(ILG)              !< DAYLENGTH FOR THAT LOCATION

      INTEGER L2MAX, NOL2PFTS(IC)
C
C     * INTERNAL WORK ARRAYS FOR THIS ROUTINE.
C
      REAL VA    (ILG),   ZRSLDM(ILG),   ZRSLDH(ILG),   ZRSLFM(ILG),   
     1     ZRSLFH(ILG),   ZDSLM (ILG),   ZDSLH (ILG),   TPOTA (ILG),   
     2     TVIRTA(ILG),   CRIB  (ILG),   CPHCHC(ILG),   CPHCHG(ILG),   
     3     HCPSCS(ILG),   HCPSGS(ILG),   TCSNOW(ILG),   CEVAP (ILG),   
     4     TBAR1P(ILG),   GSNOWC(ILG),   GSNOWG(ILG),   GSNOW(ILG) ,    
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
     I     STT   (ILG),   SQT   (ILG),   SUT   (ILG),   SVT   (ILG),    
     J     SHT   (ILG)                                                  
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
     5     TCANP (ILG),    TRTOP (ILG),   TRTOPG(ILG,NBS), QSTOR (ILG), 
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
      REAL THTOT,CA,CB,WACSAT,QACSAT,RATIOM,RATIOH,FACTM,FACTH          
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
!>First, two parameters are calculated for later use in the CLASST subroutines: the corrected wind speed \f$v_a\f$ ,
!!and the Coriolis parameter \f$f_{cor}\f$ (describing the effect of the earth’s rotation on the movement of air
!!according to the reference frame of the earth’s surface). The wind speed correction is applied because it
!!is assumed that air is never completely still, so \f$v_a\f$ is specified as the maximum of VMOD and a limiting
!!lower value of \f$0.1 m s^{-1}\f$ . The Coriolis parameter is calculated from the angular velocity \f$\Omega\f$ of the earth’s
!!rotation (7.29 x 10 -5 radians/s), and the latitude \f$\varphi\f$:
!!\f$f_{cor} = 2 \Omega sin \varphi\f$
!!
!!The packing and unpacking of binary files may cause small shifts in the values of variables at certain
!!points in the model run, so checks are performed on the depth of ponded water and on the soil liquid and
!!frozen moisture contents to ensure that unphysical values have not occurred. If the ponded water depth
!!is vanishingly small or less than zero, it is set to zero. If the soil liquid water content falls below the set
!!minimum value, it is set to the minimum value. If the soil frozen water content is less than zero, it is set
!!back to zero. If the sum of the liquid water content and frozen water content converted to an equivalent
!!liquid amount is greater than the pore volume, both are re-normalized by the pore volume. (This
!!treatment of frozen water is employed in recognition of the fact that water expands upon freezing and can
!!therefore occupy a greater volume than the nominal pore volume of the soil.) The small changes in
!!internal energy and water content of the soil layers resulting from these operations are accounted for by
!!updating the diagnostic variables HTC and WTRG.
!!
!!If CLASS is being run in coupled mode with CTEM, several CTEM variables are initialized to zero.
!!Subroutine TPREP is then called, to carry out the initialization of a number of variables and to do
!!preparatory calculations of various parameters associated with the energy and water budget calculations.
!!
!!The energy budget calculations that follow are performed over the four subareas, of canopy over snow
!!(CS), snow over ground (GS), canopy over bare ground (C) and bare ground (G). First, a counter
!!NLANDx is defined for each subarea, containing the number of modelled areas in which the subarea
!!occurs. The subarea calculations are only done if the relevant counter is greater than zero. A counter
!!NLANDI is also set to the number of modelled areas that are ice sheets (indicated by ISAND = -4).
!!(This is used later in subroutine CLASSW to toggle the call to subroutine ICEBAL.)
!!
!!For each subarea, the roughness lengths for momentum and heat, ZOM and ZOH, are obtained from
!!their logarithms, and are then used to determine various height intervals in the atmosphere for subsequent
!!calculations. These heights are derived from the input variables ZREFM and ZREFH, the reference
!!heights corresponding to the input values of wind speed and of air temperature and humidity respectively,
!!and ZDIAGM and ZDIAGH, the heights at which the diagnostic values of anemometer wind speed and
!!screen level temperature and humidity are to be determined. The form of the calculations depends on the
!!value of the flag IZREF. If IZREF = 1, the zero plane is taken to lie at the physical ground surface (as
!!with field measurements); if IZREF = 2, the zero plane is taken to lie at the local roughness height for
!!momentum(as with atmospheric models). The variables ZRSLDM and ZRSLDH are the height
!!differences used in subroutine DRCOEF to express the interval between the conceptual bottom of the
!!atmosphere and the reference heights for wind speed and for temperature and humidity respectively; the
!!variables ZRSLFM and ZRSLFH are the corresponding height differences used in subroutine
!!FLXSURFZ. If IZREF = 1, ZRSLDM and ZRSLDH are set to ZREFM and ZREFH minus the
!!displacement height DISP or DISPS, and ZRSLFM and ZRSLFH are set to ZREFM and ZREFH minus
!!the roughness length ZOM and DISP or DISPS. If IZREF = 2, ZRSLDM and ZRSLDH are set to
!!ZREFM and ZREFH plus the roughness height ZOM, and ZRSLFM and ZRSLFH are set to ZREFM
!!and ZREFH minus DISP or DISPS. (In the absence of a vegetation canopy, the displacement height is
!!zero.) The variables ZDSLM and ZDSLH are the heights above the bottom of the modelled atmosphere
!!at which the diagnostic values are to be calculated. If IZREF = 1, they are set to ZDIAGM and
!!ZDIAGH minus ZOM; if IZREF = 2 they are simply set to ZDIAGM and ZDIAGH. At the end of the
!!branch in the code, the ratios ZOSCLM and ZOSCLH, which are used in subroutine DRCOEF, are
!!calculated as ZOM/ZRSLDM and ZOH/ZRSLDH respectively.
!!
!!Several other local parameters are also calculated. The potential temperature is the temperature that air
!!would have if brought adiabatically (without addition or removal of heat) to a given height. The potential
!!temperature of the air at the reference height, \f$T_{a,pot}\f$ , is calculated relative to the height at which the
!!horizontal wind speed goes to zero, using the dry adiabatic lapse rate, \f$dT/dz = -g/c_p\f$ , where g is the
!!acceleration due to gravity and \f$c_p\f$ is the specific heat at constant pressure:. Thus,
!!\f$T_{a,pot} = T_a + z_{ref,h} g/c_p\f$
!!where \f$T_a\f$ is the temperature of the air at the reference height and \f$z_{ref,h}\f$ is the height interval, equivalent to
!!ZRSLFH defined above. If CLASS is being run coupled to an atmospheric model, i.e. if IZREF=2, the
!!air temperature at the reference height has already been adiabatically extrapolated before being passed to
!!CLASS. Otherwise, the correction is performed using the above equation.
!!
!!The virtual potential temperature of the air at the reference height, \f$T_{a,v}\f$ , is the potential temperature
!!adjusted for the reduction in air density due to the presence of water vapour. This is applied in order to
!!enable the use of the equation of state for dry air. \f$T_{a,v}\f$ can be approximated as:
!!\f$T_{a,v} = T_{a,pot} [1 + 0.61 q_a ]\f$
!!where \f$q_a\f$ is the specific humidity of the air at the reference height.
!!
!!The bulk Richardson number \f$Ri_B\f$ , used in the calculations of the atmospheric stability functions in
!!subroutine DRCOEF, is formulated as:
!!\f$Ri_B = [T_0 – T_{a,v} ] (-g z_{ref} )/(T_{a,v} v_a^2 )\f$
!!where \f$T_0\f$ is the surface temperature. For ease of calculation later on, the factor multiplying \f$[T_0 – T_{a,v} ]\f$ on
!!the right-hand side of the above equation is evaluated and assigned to a coefficient CRIB, using ZRSLDM
!!for \f$z_{ref}\f$ . The drag coefficient under neutral stability, \f$C_{DN}\f$ , is expressed using basic flux-gradient analysis as:
!!\f$C_{DN} = k^2 /[ln(z_{ref} ) – ln(z_0 )]^2\f$
!!where k is the von Karman constant and \f$z_0\f$ is the roughness length. ZRSLDM is used for \f$z_{ref}\f$ and the
!!logarithm of the local roughness length for \f$ln(z_0 )\f$, and the neutral drag coefficient DRAG over the
!!modelled area is obtained as a weighted average over the four subareas.
!!
!!For the two subareas with canopy cover, the wind speed of the air at the canopy top, \f$v_{a,c}\f$ , is obtained by
!!applying the classic logarithmic wind law for the wind speed v(z) at a height z:
!!\f$kv(z)/v_* = ln[(z – d)/z_0 ]\f$
!!where \f$v_*\f$ is the friction velocity and d is the displacement height. Thus, \f$v_{a,c}\f$ at the canopy height H can be
!!related to v a at the reference height \f$z_{ref}\f$ as:
!!\f$v_{a,c} = v_a [ln(H – d) – ln(z_0 )]/[ln(z_{ref} ) – ln(z_0 )]\f$
!!
!!The vegetation height is calculated as \f$10z_0\f$ . Local values of the temperature of the canopy air TAC and
!!the humidity of the canopy air QAC are assigned to variables TACCS/TACCO and QACCS/QACCO
!!respectively.
!!
!!At this point calls are made to a series of subroutines addressing the calculation of the energy balance
!!components of the subarea in question. The calls are summarized in the table below.
!!
!!\f[
!!\begin{tabular} { | l | l | c | c | c | c | }
!!\hline
!! & & CS & GS & C & G \\
!! \hline
!!CWCALC & Freezing/thawing of liquid/frozen water on canopy & YES & & YES & \\ \hline
!!TNPREP & Set coefficients for temperature calculations in soil & YES & YES & YES & YES \\ \hline
!!TSPREP & Set coefficients for temperature calculations of snow & YES & YES & & \\ \hline
!!TSOLVC & Calculate components of canopy energy balance & YES & & YES & \\ \hline
!!TSOLVE & Calculate components of ground or snow energy balance & & YES & & YES \\ \hline
!!TSPOST & Heat conduction in snow pack & YES & YES & & \\ \hline
!!TNPOST & Heat conduction in soil & YES & YES & YES & YES \\ \hline
!!
!!\end{tabular}
!!\f]
!!
!!After these calls, various diagnostic calculations are performed. First the screen-level temperature and
!!humidity, and the anemometer-level zonal and meridional wind speeds, are calculated. Three options are
!!provided for doing this, indicated by the flag ISLFD. If ISLFD = 0, the simple approach used in the
!!Canadian GCM is selected. The ratio of the square root of the surface drag coefficient for momentum,
!!\f$C_{DM}\f$ , to that of the neutral drag coefficient \f$C_{DN}\f$ , is calculated for the screen height \f$z_s\f$ (RATFC1) and the
!!anemometer height (RATFCA1), to give a measure of the degree of atmospheric instability. If the bulk
!!Richardson number RIB is positive (indicating stable conditions), RATFC1 is adopted for the screen-level
!!calculations; if RIB is negative (indicating unstable conditions), the ratio used is the minimum of the ratio
!!of the drag coefficient for heat \f$C_{DH}\f$ to \f$C_{DN}\f$ , and \f$(z_s / z_{ref} )^{1/3}\f$ , a measure of the depth of the convection.
!!These ratios are applied to the calculation of the screen and anemometer level variables. If the ratios are
!!large, indicating strong coupling with the atmosphere, the screen level variables tend to the values at the
!!reference height; if the ratio is small, they tend to the values at the surface. At the end of the loop, the
!!CCCma subroutine SCREENRH is called to evaluate the screen-level relative humidity.
!!
!!If ISLFD= 1 or 2, the more rigorous calculations in subroutines SLDIAG and DIASURFZ are followed.
!!The calculations done in SLDIAG are consistent with the modelling approach used in subroutine
!!DRCOEF to determine the atmospheric stability functions, so when ISLFD = 1, DRCOEF and
!!SLDIAG are called. The calculations done in DIASURFZ are consistent with the modelling approach
!!used in subroutine FLXSURFZ for the atmospheric stability functions, so when ISLFD = 2, FLXSURFZ
!!and DIASURFZ are called.
!!
!!A number of additional diagnostic variables are calculated as weighted averages over the four subareas.
!!For the most part, these calculations are straightforward; only the calculation of the porential
!!evapotranspiration \f$E_p\f$ (EVPPOT) involves some complexity. \f$E_p\f$ is defined as the evapotranspiration that
!!would occur under ambient atmospheric conditions if the soil were completely saturated and the
!!vegetation canopy were completely water-covered, i.e. if there were no surface resistances to evaporation:
!!\f$E_p = \rho_a C_{DH} v_a [q_{0,sat} – q_a ]\f$
!!where \f$\rho_a\f$ is the density of air and \f$q_{0,sat}\f$ is the saturated specific humidity at the surface. For the ground or
!!snow surface \f$q_{0,sat}\f$ was calculated in subroutine TSOLVE. For the canopy, the saturated specific humidity
!!at the canopy air temperature, \f$q_{ac,sat}\f$ , is used. This is obtained from the mixing ratio at saturation, \f$w_{ac,sat}\f$ :
!!\f$q_{ac,sat} = w_{ac,sat} /[1 + w_{ac,sat} ]\f$
!!
!!The mixing ratio is a function of the saturation vapour pressure \f$e_{ac,sat}\f$ at the canopy air temperature:
!!\f$w_{ac,sat} = 0.622 e_{ac,sat} /(p_{dry} )\f$
!!
!!A standard empirical equation for the saturation vapour pressure dependence on the temperature T is
!!used:
!!\f[e_{sat} = 611.0 exp[17.269(T – T_f )/(T – 35.86)]      T \geq T_f \f]
!!\f[e_{sat} = 611.0 exp[21.874(T – T_f )/(T – 7.66)]       T < T_f \f]
!!where \f$T_f\f$ is the freezing point.
!!
!!At the end of the code dealing with the four subareas, several more diagnostic variables are evaluated.
!!Again, these calculations are generally straightforward. The effective black-body surface temperature \f$T_{0,eff}\f$
!!is obtained by inverting the Stefan-Boltzmann equation:
!!\f$L\uparrow = \sigma T_{0,eff}^4\f$
!!where \f$L\uparrow\f$ is the outgoing longwave radiation and \f$\sigma\f$ is the Stefan-Boltzmann constant. The evaporation
!!efficiency parameter EVAPB is calculated as the ratio of the actual evapotranspiration to the potential
!!evapotranspiration.
!!
      DO 50 I=IL1,IL2                                                            
          VA(I)=MAX(VMIN,VMOD(I)) 
          FCOR(I)=2.0*7.29E-5*SIN(RADJ(I))
C
C     * CHECK DEPTH OF PONDED WATER FOR UNPHYSICAL VALUES.
C
          IF(ZPOND(I).LT.1.0E-8) ZPOND(I)=0.0
          QG(I)=0.0
   50 CONTINUE
C
C     * CHECK LIQUID AND FROZEN SOIL MOISTURE CONTENTS FOR SMALL
C     * ABERRATIONS CAUSED BY PACKING/UNPACKING.
C       
      DO 60 J=1,IG
      DO 60 I=IL1,IL2
          IF(ISAND(I,1).GT.-4)                                   THEN
              HTC(I,J)=HTC(I,J)-TBAR(I,J)*(HCPW*THLIQ(I,J)+
     1                 HCPICE*THICE(I,J))*DELZW(I,J)/DELT
              WTRG(I)=WTRG(I)-(RHOW*THLIQ(I,J)+RHOICE*THICE(I,J))*
     1                DELZW(I,J)/DELT
              IF(THLIQ(I,J).LT.THLMIN(I,J)) 
     1            THLIQ(I,J)=THLMIN(I,J)
              IF(THICE(I,J).LT.0.0) THICE(I,J)=0.0                        
              THTOT=THLIQ(I,J)+THICE(I,J)*RHOICE/RHOW                     
              IF(THTOT.GT.THPOR(I,J))           THEN                              
                  THLIQ(I,J)=MAX(THLIQ(I,J)*THPOR(I,J)/               
     1                       THTOT,THLMIN(I,J))                                       
                  THICE(I,J)=(THPOR(I,J)-THLIQ(I,J))*
     1                           RHOW/RHOICE
                  IF(THICE(I,J).LT.0.0) THICE(I,J)=0.0                        
              ENDIF
              HTC(I,J)=HTC(I,J)+TBAR(I,J)*(HCPW*THLIQ(I,J)+
     1                 HCPICE*THICE(I,J))*DELZW(I,J)/DELT
              WTRG(I)=WTRG(I)+(RHOW*THLIQ(I,J)+RHOICE*THICE(I,J))*
     1                DELZW(I,J)/DELT
          ENDIF
   60 CONTINUE                                                        
C
      IF (ctem_on) THEN
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
     7                 QMELTC, QMELTG, EVAP,   GSNOW,                   
     8                 TPONDC, TPONDG, TPNDCS, TPNDGS, QSENSC, QSENSG, 
     9                 QEVAPC, QEVAPG, TACCO,  QACCO,  TACCS,  QACCS,  
     A                 ILMOX,  UEX,    HBLX,
     B                 ILMO,   UE,     HBL,   
     C                 ST,     SU,     SV,     SQ,     SRH,             
     D                 CDH,    CDM,    QSENS,  QEVAP,  QLWAVG,          
     E                 FSGV,   FSGS,   FSGG,   FLGV,   FLGS,   FLGG,   
     F                 HFSC,   HFSS,   HFSG,   HEVC,   HEVS,   HEVG,   
     G                 HMFC,   HMFN,   QFCF,   QFCL,   EVPPOT, ACOND,  
     H                 DRAG,   THLIQ,  THICE,  TBAR,   ZPOND,  TPOND,  
     I                 THPOR,  THLMIN, THLRET, THFC,   HCPS,   TCS,    
     J                 TA,     RHOSNO, TSNOW,  ZSNOW,  WSNOW,  TCAN,
     K                 FC,     FCS,    DELZ,   DELZW,  ZBOTW,
     L                 ISAND,  ILG,    IL1,    IL2,    JL,     IG,  
     M                 FVEG,   TCSATU, TCSATF, FTEMP,  FTEMPX, FVAP,
     N                 FVAPX,  RIB,    RIBX  )   

        
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
          IF(FC (I).GT.0.)            NLANDC =NLANDC +1
          IF(FCS(I).GT.0.)            NLANDCS=NLANDCS+1
          IF(FG (I).GT.0.)            NLANDG =NLANDG +1
          IF(FGS(I).GT.0.)            NLANDGS=NLANDGS+1
          IF(ISAND(I,1).EQ.-4)        NLANDI =NLANDI +1
70    CONTINUE
C
C     * CALCULATIONS FOR CANOPY OVER SNOW.
C                                                                                           
      IF(NLANDCS.GT.0)                                              THEN                
          DO 100 I=IL1,IL2    
              IF(FCS(I).GT.0.)                                      THEN
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
                  DRAG(I)=DRAG(I)+FCS(I)*(VKC/(LOG(ZRSLDM(I))-
     1                    ZOMLCS(I)))**2
                  VAC(I)=VA(I)*(LOG(10.0*ZOM(I)-DISPS(I))-ZOMLCS(I))/
     1                   (LOG(ZRSLDM(I))-ZOMLCS(I))
                  TACCS(I)=TAC(I)
                  QACCS(I)=QAC(I)
              ENDIF
  100     CONTINUE
C                                     
          CALL CWCALC(TCANS,RAICNS,SNOCNS,FRAICS,FSNOCS,CHCAPS,
     1                HMFC,HTCC,FCS,CMASCS,ILG,IL1,IL2,JL)
          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,IWATER, 
     2                TBAR,TCTOPC,TCBOTC,
     +                FCS,ZPOND,TBAR1P,DELZ,TCSNOW,ZSNOW,
     3                ISAND,ILG,IL1,IL2,JL,IG                      )    
          CALL TSPREP(GCOEFFS,GCONSTS,CPHCHG,IWATER,
     1                FCS,ZSNOW,TSNOW,TCSNOW,
     2                ILG,IL1,IL2,JL      )
          ISNOW=1


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
     A                FCOR,GCONSTS,GCOEFFS,TSFSAV(1,1),TRSNOWC,FSNOCS,  
     B                FRAICS,CHCAPS,CMASCS,PCPR,FROOTS,THLMIN,DELZW,
     +                RHOSCS,ZSNOW,IWATER,IEVAP,ITERCT,    
     C                ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,N,  
     D                TSTEP,TVIRTC,TVIRTG,EVBETA,XEVAP,EVPWET,Q0SAT,
     E                RA,RB,RAGINV,RBINV,RBTINV,RBCINV,TVRTAC,TPOTG,
     F                RESID,TCANP,
     G                WZERO,XEVAPM,DCFLXM,WC,DRAGIN,CFLUXM,CFLX,IEVAPC,
     H                TRTOP,QSTOR,CFSENS,CFEVAP,QSGADD,AC,BC,
     I                LZZ0,LZZ0T,FM,FH,ITER,NITER,KF1,KF2,
     J                AILCGS,FCANCS,CO2CONC,RMATCTEM,
     K                THLIQC,THFC,THLW,ISAND,IG,COSZS,PRESSG,
     L                XDIFFUS,ICTEM,IC,CO2I1CS,CO2I2CS,
     M                ctem_on,SLAI,FCANCMX,L2MAX,
     N                NOL2PFTS,CFLUXCS,ANCSVEG,RMLCSVEG,LFSTATUS,
     O                DAYL, DAYL_MAX)

          CALL TSPOST(GSNOWC,TSNOCS,WSNOCS,RHOSCS,QMELTC,
     1                GZROCS,TSNBOT,HTCS,HMFN,
     2                GCONSTS,GCOEFFS,GCONST,GCOEFF,TBAR,
     3                TSURX,ZSNOW,TCSNOW,HCPSCS,QTRANS,
     4                FCS,DELZ,ILG,IL1,IL2,JL,IG            )
          CALL TNPOST(TBARCS,G12CS,G23CS,TPNDCS,GZROCS,ZERO,GCONST,
     1                GCOEFF,TBAR,TCTOPC,TCBOTC,HCPC,ZPOND,TSNBOT,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FCS,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG       )
C
C     * DIAGNOSTICS.
C
          IF(ISLFD.EQ.0)                                         THEN
            DO 150 I=IL1,IL2
              IF(FCS(I).GT.0.)                THEN
                FACTM=ZDSLM(I)+ZOM(I)                                   
                FACTH=ZDSLH(I)+ZOM(I)                                   
                RATIOM=SQRT(CDMX(I))*LOG(FACTM/ZOM(I))/VKC              
                RATIOM=MIN(RATIOM,1.)                                   
                RATIOH=SQRT(CDMX(I))*LOG(FACTH/ZOH(I))/VKC              
                RATIOH=MIN(RATIOH,1.)                                   
                IF(RIBX(I).LT.0.)  THEN                                 
                  RATIOH=RATIOH*CDHX(I)/CDMX(I)                         
                  RATIOH=MIN(RATIOH,(FACTH/ZRSLDH(I))**(1./3.))         
                  ENDIF                                                    
                STT(I)=TACCS(I)-(MIN(RATIOH,1.))*(TACCS(I)-TA(I))       
                SQT(I)=QACCS(I)-(MIN(RATIOH,1.))*(QACCS(I)-QA(I))       
                SUT(I)=RATIOM*UWIND(I)                                  
                SVT(I)=RATIOM*VWIND(I)                                  
              ENDIF
  150     CONTINUE
C
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FCS,ILG,IL1,IL2)
C                                                                       
            DO I=IL1,IL2                                                
              IF(FCS(I).GT.0.)                THEN                      
                ST (I)=ST (I)+FCS(I)*STT(I)                             
                SQ (I)=SQ (I)+FCS(I)*SQT(I)                             
                SU (I)=SU (I)+FCS(I)*SUT(I)                             
                SV (I)=SV (I)+FCS(I)*SVT(I)                             
                SRH(I)=SRH(I)+FCS(I)*SHT(I)
              ENDIF                                                     
            ENDDO                                                       
C                                                                       
          ELSEIF(ISLFD.EQ.1)                                        THEN
            CALL SLDIAG(SUT,SVT,STT,SQT,                                
     1                    CDMX,CDHX,UWIND,VWIND,TPOTA,QA,
     2                    TACCS,QACCS,ZOM,ZOH,FCS,ZRSLDM,
     3                    ZDSLM,ZDSLH,ILG,IL1,IL2,JL)
C                                                                       
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FCS,ILG,IL1,IL2)           
C                                                                       
            DO I=IL1,IL2                                                
              IF(FCS(I).GT.0.)                THEN                      
                ST (I)=ST (I)+FCS(I)*STT(I)                             
                SQ (I)=SQ (I)+FCS(I)*SQT(I)                             
                SU (I)=SU (I)+FCS(I)*SUT(I)                             
                SV (I)=SV (I)+FCS(I)*SVT(I)                             
                SRH(I)=SRH(I)+FCS(I)*SHT(I)                             
              ENDIF                                                     
            ENDDO                                                       
          ELSEIF(ISLFD.EQ.2)                                        THEN
              CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TACCS,QACCS,
     1                    ZOM,ZOH,ILMOX,ZRSLFM,HBLX,UEX,FTEMPX,FVAPX,
     2                    ZDSLM,ZDSLH,RADJ,FCS,IL1,IL2,JL)
          ENDIF
C
          DO 175 I=IL1,IL2
              IF(FCS(I).GT.0.)                                      THEN
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
                  EVPPOT(I)=EVPPOT(I)+FCS(I)*RHOAIR(I)*CFLUX(I)*
     1                     (QACSAT-QA(I))
                  ACOND(I)=ACOND(I)+FCS(I)*CFLUX(I)
                  ILMO(I) =ILMO(I)+FCS(I)*ILMOX(I)
                  UE(I)   =UE(I)+FCS(I)*UEX(I)
                  HBL(I)  =HBL(I)+FCS(I)*HBLX(I)
                  CDH (I) =CDH(I)+FCS(I)*CDHX(I)
                  CDM (I) =CDM(I)+FCS(I)*CDMX(I)
                  TSFSAV(I,1)=TSURX(I)
                  QG(I)=QG(I)+FCS(I)*QACCS(I)
                  QSENS(I)=QSENS(I)+FCS(I)*QSENSX(I)
                  QEVAP(I)=QEVAP(I)+FCS(I)*QEVAPX(I)
                  QLWAVG(I)=QLWAVG(I)+FCS(I)*QLWX(I)
                  FSGV(I) =FSGV(I)+FCS(I)*QSWNC(I)
                  FSGS(I) =FSGS(I)+FCS(I)*QSWNG(I)
                  FSGG(I) =FSGG(I)+FCS(I)*QTRANS(I)
                  FLGV(I) =FLGV(I)+FCS(I)*(QLWIN(I)+QLWOG(I)-2.0*
     1                     QLWOC(I))*(1.0-FSVFS(I))
                  FLGS(I) =FLGS(I)+FCS(I)*(QLWOC(I)*(1.0-FSVFS(I))+
     1                     QLWIN(I)*FSVFS(I)-QLWOG(I))
                  IF(ITC.EQ.1) THEN
                      HFSC(I) =HFSC(I)+FCS(I)*QSENSC(I)
                  ELSE
                      HFSC(I) =HFSC(I)+FCS(I)*(QSENSC(I)-QSENSG(I))
                  ENDIF
                  HFSS(I) =HFSS(I)+FCS(I)*QSENSG(I)
                  HEVC(I) =HEVC(I)+FCS(I)*QEVAPC(I)
                  HEVS(I) =HEVS(I)+FCS(I)*QEVAPG(I)
                  HMFC(I) =HMFC(I)+FCS(I)*QPHCHC(I)
                  HTCS(I) =HTCS(I)+FCS(I)*(-GZROCS(I)+
     1                     QTRANS(I))
                  HTC(I,1)=HTC(I,1)+FCS(I)*(GZROCS(I)-QTRANS(I)-
     1                     G12CS(I))
                  HTC(I,2)=HTC(I,2)+FCS(I)*(G12CS(I)-G23CS(I))
                  HTC(I,3)=HTC(I,3)+FCS(I)*G23CS(I)
                  FTEMP(I)= FTEMP(I) + FCS(I) * FTEMPX(I)
                  FVAP (I)= FVAP (I) + FCS(I) * FVAPX (I)
                  RIB  (I)= RIB  (I) + FCS(I) * RIBX  (I)
                  GSNOW(I) =GSNOW(I)+FCS(I)/(FCS(I)+FGS(I))*GSNOWC(I)   
              ENDIF
  175     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR SNOW-COVERED GROUND.
C
      IF(NLANDGS.GT.0)                                              THEN
          DO 200 I=IL1,IL2                                    
              IF(FGS(I).GT.0.)                                      THEN
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
                  DRAG(I)=DRAG(I)+FGS(I)*(VKC/(LOG(ZRSLDM(I))-
     1                    ZOMLNS(I)))**2
              ENDIF
  200     CONTINUE
C
          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,IWATER, 
     2                TBAR,TCTOPG,TCBOTG,
     +                FGS,ZPOND,TBAR1P,DELZ,TCSNOW,ZSNOW,
     3                ISAND,ILG,IL1,IL2,JL,IG                  )        
          CALL TSPREP(GCOEFFS,GCONSTS,CPHCHG,IWATER,
     1                FGS,ZSNOW,TSNOW,TCSNOW,
     2                ILG,IL1,IL2,JL      )
          ISNOW=1 
          CALL TSOLVE(ISNOW,FGS,
     1                QSWX,QLWX,QTRANS,QSENSX,QEVAPX,EVAPGS,
     2                TSURX,QSURX,GSNOWG,QMELTG,CDHX,CDMX,RIBX,CFLUX,
     3                FTEMPX,FVAPX,ILMOX,UEX,HBLX, 
     4                QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,                   
     5                ALVSSN,ALIRSN,CRIB,CPHCHG,CEVAP,TVIRTA,
     6                ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,
     7                GCONSTS,GCOEFFS,TSFSAV(1,2),PCPR,                 
     +                TRSNOWG,FSSB,ALSNO,                               
     +                THLIQG,THLMIN,DELZW,RHOSGS,ZSNOW,
     8                IWATER,IEVAP,ITERCT,ISAND,
     9                ISLFD,ITG,ILG,IG,IL1,IL2,JL,NBS,ISNOALB,          
     A                TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,
     B                DCFLXM,CFLUXM,WZERO,TRTOPG,AC,BC,                 
     C                LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF  )
          CALL TSPOST(GSNOWG,TSNOGS,WSNOGS,RHOSGS,QMELTG,
     1                GZROGS,TSNBOT,HTCS,HMFN,
     2                GCONSTS,GCOEFFS,GCONST,GCOEFF,TBAR,
     3                TSURX,ZSNOW,TCSNOW,HCPSGS,QTRANS,
     4                FGS,DELZ,ILG,IL1,IL2,JL,IG            )
          CALL TNPOST(TBARGS,G12GS,G23GS,TPNDGS,GZROGS,ZERO,GCONST,
     1                GCOEFF,TBAR,TCTOPG,TCBOTG,HCPG,ZPOND,TSNBOT,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FGS,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG       )
C
C     * DIAGNOSTICS.
C
          IF(ISLFD.EQ.0)                                         THEN
            DO 250 I=IL1,IL2
              IF(FGS(I).GT.0.)                THEN
                FACTM=ZDSLM(I)+ZOM(I)                                   
                FACTH=ZDSLH(I)+ZOM(I)                                   
                RATIOM=SQRT(CDMX(I))*LOG(FACTM/ZOM(I))/VKC              
                RATIOM=MIN(RATIOM,1.)                                   
                RATIOH=SQRT(CDMX(I))*LOG(FACTH/ZOH(I))/VKC              
                RATIOH=MIN(RATIOH,1.)                                   
                IF(RIBX(I).LT.0.)  THEN                                 
                  RATIOH=RATIOH*CDHX(I)/CDMX(I)                         
                  RATIOH=MIN(RATIOH,(FACTH/ZRSLDH(I))**(1./3.))         
                  ENDIF                                                      
                STT(I)=TSURX(I)-(MIN(RATIOH,1.))*(TSURX(I)-TA(I))       
                SQT(I)=QSURX(I)-(MIN(RATIOH,1.))*(QSURX(I)-QA(I))       
                SUT(I)=RATIOM*UWIND(I)                                  
                SVT(I)=RATIOM*VWIND(I)                                  
              ENDIF
  250     CONTINUE
C
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FGS,ILG,IL1,IL2)
C                                                                       
            DO I=IL1,IL2                                                
              IF(FGS(I).GT.0.)                THEN                      
                ST (I)=ST (I)+FGS(I)*STT(I)                             
                SQ (I)=SQ (I)+FGS(I)*SQT(I)                             
                SU (I)=SU (I)+FGS(I)*SUT(I)                             
                SV (I)=SV (I)+FGS(I)*SVT(I)                             
                SRH(I)=SRH(I)+FGS(I)*SHT(I)
              ENDIF                                                     
            ENDDO                                                       
C                                                                       
          ELSEIF(ISLFD.EQ.1)                                        THEN
            CALL SLDIAG(SUT,SVT,STT,SQT,                                
     1                    CDMX,CDHX,UWIND,VWIND,TPOTA,QA,
     2                    TSURX,QSURX,ZOM,ZOH,FGS,ZRSLDM,
     3                    ZDSLM,ZDSLH,ILG,IL1,IL2,JL)
C                                                                       
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FGS,ILG,IL1,IL2)           
C                                                                       
            DO I=IL1,IL2                                                
              IF(FGS(I).GT.0.)                THEN                      
                ST (I)=ST (I)+FGS(I)*STT(I)                             
                SQ (I)=SQ (I)+FGS(I)*SQT(I)                             
                SU (I)=SU (I)+FGS(I)*SUT(I)                             
                SV (I)=SV (I)+FGS(I)*SVT(I)                             
                SRH(I)=SRH(I)+FGS(I)*SHT(I)                             
              ENDIF                                                     
            ENDDO                                                       
          ELSEIF(ISLFD.EQ.2)                                        THEN  
              CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TSURX,QSURX,
     1                    ZOM,ZOH,ILMOX,ZRSLFM,HBLX,UEX,FTEMPX,FVAPX,
     2                    ZDSLM,ZDSLH,RADJ,FGS,IL1,IL2,JL)
          ENDIF
C
          DO 275 I=IL1,IL2
              IF(FGS(I).GT.0.)                                      THEN
                  EVPPOT(I)=EVPPOT(I)+FGS(I)*RHOAIR(I)*CFLUX(I)*
     1                     (Q0SAT(I)-QA(I))
                  ACOND(I)=ACOND(I)+FGS(I)*CFLUX(I)
                  ILMO(I) =ILMO(I)+FGS(I)*ILMOX(I)
                  UE(I)   =UE(I)+FGS(I)*UEX(I)
                  HBL(I)  =HBL(I)+FGS(I)*HBLX(I)
                  CDH (I) =CDH(I)+FGS(I)*CDHX(I)
                  CDM (I) =CDM(I)+FGS(I)*CDMX(I)
                  TSFSAV(I,2)=TSURX(I)
                  QG(I)=QG(I)+FGS(I)*QSURX(I)
                  QSENS(I)=QSENS(I)+FGS(I)*QSENSX(I)
                  QEVAP(I)=QEVAP(I)+FGS(I)*QEVAPX(I)
                  QLWAVG(I)=QLWAVG(I)+FGS(I)*QLWX(I)
                  FSGS(I) =FSGS(I)+FGS(I)*(QSWX(I)-QTRANS(I))
                  FSGG(I) =FSGG(I)+FGS(I)*QTRANS(I)
                  FLGS(I) =FLGS(I)+FGS(I)*(QLWIN(I)-QLWX(I))
                  HFSS(I) =HFSS(I)+FGS(I)*QSENSX(I)
                  HEVS(I) =HEVS(I)+FGS(I)*QEVAPX(I)
                  HTCS(I) =HTCS(I)+FGS(I)*(-GZROGS(I)+
     1                     QTRANS(I))
                  HTC(I,1)=HTC(I,1)+FGS(I)*(GZROGS(I)-QTRANS(I)-
     1                     G12GS(I))
                  HTC(I,2)=HTC(I,2)+FGS(I)*(G12GS(I)-G23GS(I))
                  HTC(I,3)=HTC(I,3)+FGS(I)*G23GS(I)
                  FTEMP(I)= FTEMP(I) + FGS(I) * FTEMPX(I)
                  FVAP (I)= FVAP (I) + FGS(I) * FVAPX (I)
                  RIB  (I)= RIB  (I) + FGS(I) * RIBX  (I)
                  GSNOW(I) =GSNOW(I)+FGS(I)/(FCS(I)+FGS(I))*GSNOWG(I)   
              ENDIF
  275     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR CANOPY OVER BARE GROUND.
C           
      IF(NLANDC.GT.0)                                               THEN
          DO 300 I=IL1,IL2                                    
              IF(FC(I).GT.0.)                                       THEN
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
                  DRAG(I)=DRAG(I)+FC(I)*(VKC/(LOG(ZRSLDM(I))-
     1                    ZOMLNC(I)))**2
                  VAC(I)=VA(I)*(LOG(10.0*ZOM(I)-DISP(I))-ZOMLNC(I))/
     1                (LOG(ZRSLDM(I))-ZOMLNC(I))
                  TACCO(I)=TAC(I)
                  QACCO(I)=QAC(I)
              ENDIF
  300     CONTINUE
C
          CALL CWCALC(TCANO,RAICAN,SNOCAN,FRAINC,FSNOWC,CHCAP,
     1                HMFC,HTCC,FC,CMASSC,ILG,IL1,IL2,JL)

          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,IWATER, 
     2                TBAR,TCTOPC,TCBOTC,
     +                FC,ZPOND,TBAR1P,DELZ,TCSNOW,ZSNOW,
     3                ISAND,ILG,IL1,IL2,JL,IG                      )    
          ISNOW=0

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
     A                FCOR,GCONST,GCOEFF,TSFSAV(1,3),TRSNOWC,FSNOWC,    
     B                FRAINC,CHCAP,CMASSC,PCPR,FROOT,THLMIN,DELZW,
     +                ZERO,ZERO,IWATER,IEVAP,ITERCT,     
     C                ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,N,  
     D                TSTEP,TVIRTC,TVIRTG,EVBETA,XEVAP,EVPWET,Q0SAT,
     E                RA,RB,RAGINV,RBINV,RBTINV,RBCINV,TVRTAC,TPOTG,
     F                RESID,TCANP,
     G                WZERO,XEVAPM,DCFLXM,WC,DRAGIN,CFLUXM,CFLX,IEVAPC,
     H                TRTOP,QSTOR,CFSENS,CFEVAP,QSGADD,AC,BC,
     I                LZZ0,LZZ0T,FM,FH,ITER,NITER,KF1,KF2,
     J                AILCG,FCANC,CO2CONC,RMATCTEM,
     K                THLIQC,THFC,THLW,ISAND,IG,COSZS,PRESSG,
     L                XDIFFUS,ICTEM,IC,CO2I1CG,CO2I2CG,
     M                ctem_on,SLAI,FCANCMX,L2MAX,
     N                NOL2PFTS,CFLUXCG,ANCGVEG,RMLCGVEG,LFSTATUS,
     O                DAYL, DAYL_MAX)

          CALL TNPOST(TBARC,G12C,G23C,TPONDC,GZEROC,QFREZC,GCONST,
     1                GCOEFF,TBAR,TCTOPC,TCBOTC,HCPC,ZPOND,TSURX,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FC,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG       )
C
C     * DIAGNOSTICS.
C
          IF(ISLFD.EQ.0)                                         THEN
            DO 350 I=IL1,IL2
              IF(FC(I).GT.0.)                 THEN
                FACTM=ZDSLM(I)+ZOM(I)                                   
                FACTH=ZDSLH(I)+ZOM(I)                                   
                RATIOM=SQRT(CDMX(I))*LOG(FACTM/ZOM(I))/VKC              
                RATIOM=MIN(RATIOM,1.)                                   
                RATIOH=SQRT(CDMX(I))*LOG(FACTH/ZOH(I))/VKC              
                RATIOH=MIN(RATIOH,1.)                                   
                IF(RIBX(I).LT.0.)  THEN                                 
                  RATIOH=RATIOH*CDHX(I)/CDMX(I)                         
                  RATIOH=MIN(RATIOH,(FACTH/ZRSLDH(I))**(1./3.))         
                  ENDIF                                                     
                STT(I)=TACCO(I)-(MIN(RATIOH,1.))*(TACCO(I)-TA(I))       
                SQT(I)=QACCO(I)-(MIN(RATIOH,1.))*(QACCO(I)-QA(I))       
                SUT(I)=RATIOM*UWIND(I)                                  
                SVT(I)=RATIOM*VWIND(I)                                  
              ENDIF
  350     CONTINUE
C
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FC,ILG,IL1,IL2)
C                                                                       
            DO I=IL1,IL2                                                
              IF(FC(I).GT.0.)                 THEN                      
                ST (I)=ST (I)+FC(I)*STT(I)                              
                SQ (I)=SQ (I)+FC(I)*SQT(I)                              
                SU (I)=SU (I)+FC(I)*SUT(I)                              
                SV (I)=SV (I)+FC(I)*SVT(I)                              
                SRH(I)=SRH(I)+FC(I)*SHT(I)
              ENDIF                                                     
            ENDDO                                                       
          ELSEIF(ISLFD.EQ.1)                                        THEN
            CALL SLDIAG(SUT,SVT,STT,SQT,                                
     1                    CDMX,CDHX,UWIND,VWIND,TPOTA,QA,
     2                    TACCO,QACCO,ZOM,ZOH,FC,ZRSLDM,
     3                    ZDSLM,ZDSLH,ILG,IL1,IL2,JL)
C                                                                       
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FC,ILG,IL1,IL2)            
C                                                                       
            DO I=IL1,IL2                                                
              IF(FC(I).GT.0.)                 THEN                      
                ST (I)=ST (I)+FC(I)*STT(I)                              
                SQ (I)=SQ (I)+FC(I)*SQT(I)                              
                SU (I)=SU (I)+FC(I)*SUT(I)                              
                SV (I)=SV (I)+FC(I)*SVT(I)                              
                SRH(I)=SRH(I)+FC(I)*SHT(I)                              
              ENDIF                                                     
            ENDDO                                                       
          ELSEIF(ISLFD.EQ.2)                                        THEN    
              CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TACCO,QACCO,
     1                    ZOM,ZOH,ILMOX,ZRSLFM,HBLX,UEX,FTEMPX,FVAPX,
     2                    ZDSLM,ZDSLH,RADJ,FC,IL1,IL2,JL)
          ENDIF
C
          DO 375 I=IL1,IL2
              IF(FC(I).GT.0.)                                       THEN
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
                  EVPPOT(I)=EVPPOT(I)+FC(I)*RHOAIR(I)*CFLUX(I)*
     1                     (QACSAT-QA(I))
                  ACOND(I)=ACOND(I)+FC(I)*CFLUX(I)
                  ILMO(I) =ILMO(I)+FC(I)*ILMOX(I)
                  UE(I)   =UE(I)+FC(I)*UEX(I)
                  HBL(I)  =HBL(I)+FC(I)*HBLX(I)
                  CDH (I) =CDH(I)+FC(I)*CDHX(I)
                  CDM (I) =CDM(I)+FC(I)*CDMX(I)
                  TSFSAV(I,3)=TSURX(I)
                  QG(I)=QG(I)+FC(I)*QACCO(I)
                  QSENS(I)=QSENS(I)+FC(I)*QSENSX(I)
                  QEVAP(I)=QEVAP(I)+FC(I)*QEVAPX(I)
                  QLWAVG(I)=QLWAVG(I)+FC(I)*QLWX(I)
                  FSGV(I) =FSGV(I)+FC(I)*QSWNC(I)
                  FSGG(I) =FSGG(I)+FC(I)*QSWNG(I)
                  FLGV(I) =FLGV(I)+FC(I)*(QLWIN(I)+QLWOG(I)-2.0*
     1                     QLWOC(I))*(1.0-FSVF(I))
                  FLGG(I) =FLGG(I)+FC(I)*(FSVF(I)*QLWIN(I)+
     1                     (1.0-FSVF(I))*QLWOC(I)-QLWOG(I))
                  IF(ITC.EQ.1) THEN
                      HFSC(I) =HFSC(I)+FC(I)*QSENSC(I)
                  ELSE
                      HFSC(I) =HFSC(I)+FC(I)*(QSENSC(I)-QSENSG(I))
                  ENDIF
                  HFSG(I) =HFSG(I)+FC(I)*QSENSG(I)
                  HEVC(I) =HEVC(I)+FC(I)*QEVAPC(I)
                  HEVG(I) =HEVG(I)+FC(I)*QEVAPG(I)
                  HMFC(I) =HMFC(I)+FC(I)*QPHCHC(I)
                  HTC(I,1)=HTC(I,1)+FC(I)*(-G12C(I))
                  HTC(I,2)=HTC(I,2)+FC(I)*(G12C(I)-G23C(I))
                  HTC(I,3)=HTC(I,3)+FC(I)*G23C(I)
                  FTEMP(I)= FTEMP(I) + FC(I) * FTEMPX(I)
                  FVAP (I)= FVAP (I) + FC(I) * FVAPX (I)
                  RIB  (I)= RIB  (I) + FC(I) * RIBX   (I)
              ENDIF
  375     CONTINUE
      ENDIF                                                               
C
C     * CALCULATIONS FOR BARE GROUND.
C                     
      IF(NLANDG.GT.0)                                               THEN
          DO 400 I=IL1,IL2                                    
              IF(FG(I).GT.0.)                                       THEN
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
                  DRAG(I)=DRAG(I)+FG(I)*(VKC/(LOG(ZRSLDM(I))-
     1                    ZOMLNG(I)))**2
              ENDIF
  400     CONTINUE
C
          CALL TNPREP(A1,A2,B1,B2,C2,GDENOM,GCOEFF,
     1                GCONST,CPHCHG,IWATER, 
     2                TBAR,TCTOPG,TCBOTG,
     +                FG,ZPOND,TBAR1P,DELZ,TCSNOW,ZSNOW,
     3                ISAND,ILG,IL1,IL2,JL,IG                      )    
          ISNOW=0
          CALL TSOLVE(ISNOW,FG,
     1                QSWX,QLWX,QTRANS,QSENSX,QEVAPX,EVAPG,
     2                TSURX,QSURX,GZEROG,QFREZG,CDHX,CDMX,RIBX,CFLUX,
     3                FTEMPX,FVAPX,ILMOX,UEX,HBLX, 
     4                QLWIN,TPOTA,QA,VA,PADRY,RHOAIR,                   
     5                ALVSG,ALIRG,CRIB,CPHCHG,CEVAP,TVIRTA,
     6                ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,FCOR,
     7                GCONST,GCOEFF,TSFSAV(1,4),PCPR,                   
     +                TRSNOWG,FSSB,ALSNO,                               
     +                THLIQG,THLMIN,DELZW,ZERO,ZERO,
     8                IWATER,IEVAP,ITERCT,ISAND,
     9                ISLFD,ITG,ILG,IG,IL1,IL2,JL, NBS,ISNOALB,         
     A                TSTEP,TVIRTS,EVBETA,Q0SAT,RESID,
     B                DCFLXM,CFLUXM,WZERO,TRTOPG,AC,BC,                 
     C                LZZ0,LZZ0T,FM,FH,ITER,NITER,JEVAP,KF )
          CALL TNPOST(TBARG,G12G,G23G,TPONDG,GZEROG,QFREZG,GCONST,
     1                GCOEFF,TBAR,TCTOPG,TCBOTG,HCPG,ZPOND,TSURX,
     2                TBASE,TBAR1P,A1,A2,B1,B2,C2,FG,IWATER,
     3                ISAND,DELZ,DELZW,ILG,IL1,IL2,JL,IG      )
C
C     * DIAGNOSTICS.
C
          IF(ISLFD.EQ.0)                                         THEN
            DO 450 I=IL1,IL2
              IF(FG(I).GT.0.)                 THEN
                FACTM=ZDSLM(I)+ZOM(I)                                   
                FACTH=ZDSLH(I)+ZOM(I)                                   
                RATIOM=SQRT(CDMX(I))*LOG(FACTM/ZOM(I))/VKC              
                RATIOM=MIN(RATIOM,1.)                                   
                RATIOH=SQRT(CDMX(I))*LOG(FACTH/ZOH(I))/VKC              
                RATIOH=MIN(RATIOH,1.)                                   
                IF(RIBX(I).LT.0.)  THEN                                 
                  RATIOH=RATIOH*CDHX(I)/CDMX(I)                         
                  RATIOH=MIN(RATIOH,(FACTH/ZRSLDH(I))**(1./3.))         
                  ENDIF                                                        
                STT(I)=TSURX(I)-(MIN(RATIOH,1.))*(TSURX(I)-TA(I))       
                SQT(I)=QSURX(I)-(MIN(RATIOH,1.))*(QSURX(I)-QA(I))       
                SUT(I)=RATIOM*UWIND(I)                                  
                SVT(I)=RATIOM*VWIND(I)                                  
              ENDIF
  450       CONTINUE
C
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FG,ILG,IL1,IL2)
C                                                                       
            DO I=IL1,IL2                                                
              IF(FG(I).GT.0.)                THEN                       
                ST (I)=ST (I)+FG(I)*STT(I)                              
                SQ (I)=SQ (I)+FG(I)*SQT(I)                              
                SU (I)=SU (I)+FG(I)*SUT(I)                              
                SV (I)=SV (I)+FG(I)*SVT(I)                              
                SRH(I)=SRH(I)+FG(I)*SHT(I)
                SFCUBS (I)=SUT(I)                                       
                SFCVBS (I)=SVT(I)                                       
                USTARBS(I)=VA(I)*SQRT(CDMX(I))                          
              ENDIF                                                     
            ENDDO                                                       
          ELSEIF(ISLFD.EQ.1)                                        THEN
            CALL SLDIAG(SUT,SVT,STT,SQT,                                
     1                    CDMX,CDHX,UWIND,VWIND,TPOTA,QA,
     2                    TSURX,QSURX,ZOM,ZOH,FG,ZRSLDM,
     3                    ZDSLM,ZDSLH,ILG,IL1,IL2,JL)
C                                                                       
            CALL SCREENRH(SHT,STT,SQT,PRESSG,FG,ILG,IL1,IL2)            
C                                                                       
            DO I=IL1,IL2                                                
              IF(FG(I).GT.0.)                THEN                       
                ST (I)=ST (I)+FG(I)*STT(I)                              
                SQ (I)=SQ (I)+FG(I)*SQT(I)                              
                SU (I)=SU (I)+FG(I)*SUT(I)                              
                SV (I)=SV (I)+FG(I)*SVT(I)                              
                SRH(I)=SRH(I)+FG(I)*SHT(I)                              
                SFCUBS (I)=SUT(I)                                       
                SFCVBS (I)=SVT(I)                                       
                USTARBS(I)=VA(I)*SQRT(CDMX(I))                          
              ENDIF                                                     
            ENDDO                                                       
          ELSEIF(ISLFD.EQ.2)                                        THEN      
              CALL DIASURFZ(SU,SV,ST,SQ,ILG,UWIND,VWIND,TSURX,QSURX,
     1                    ZOM,ZOH,ILMOX,ZRSLFM,HBLX,UEX,FTEMPX,FVAPX,
     2                    ZDSLM,ZDSLH,RADJ,FG,IL1,IL2,JL)
          ENDIF
C
          DO 475 I=IL1,IL2
              IF(FG(I).GT.0.)                                       THEN
                  EVPPOT(I)=EVPPOT(I)+FG(I)*RHOAIR(I)*CFLUX(I)*
     1                     (Q0SAT(I)-QA(I))
                  ACOND(I)=ACOND(I)+FG(I)*CFLUX(I)
                  ILMO(I) =ILMO(I)+FG(I)*ILMOX(I)
                  UE(I)   =UE(I)+FG(I)*UEX(I)
                  HBL(I)  =HBL(I)+FG(I)*HBLX(I)
                  CDH (I) =CDH(I)+FG(I)*CDHX(I)
                  CDM (I) =CDM(I)+FG(I)*CDMX(I)
                  TSFSAV(I,4)=TSURX(I)
                  GTBS(I)=TSURX(I)
                  QG(I)=QG(I)+FG(I)*QSURX(I)
                  QSENS(I)=QSENS(I)+FG(I)*QSENSX(I)
                  QEVAP(I)=QEVAP(I)+FG(I)*QEVAPX(I)
                  QLWAVG(I)=QLWAVG(I)+FG(I)*QLWX(I)
                  FSGG(I) =FSGG(I)+FG(I)*QSWX(I)
                  FLGG(I) =FLGG(I)+FG(I)*(QLWIN(I)-QLWX(I))
                  HFSG(I) =HFSG(I)+FG(I)*QSENSX(I)
                  HEVG(I) =HEVG(I)+FG(I)*QEVAPX(I)
                  HTC(I,1)=HTC(I,1)+FG(I)*(-G12G(I))
                  HTC(I,2)=HTC(I,2)+FG(I)*(G12G(I)-G23G(I))
                  HTC(I,3)=HTC(I,3)+FG(I)*G23G(I)
                  FTEMP(I)= FTEMP(I) + FG(I) * FTEMPX(I)
                  FVAP (I)= FVAP (I) + FG(I) * FVAPX (I)
                  RIB  (I)= RIB  (I) + FG(I) * RIBX  (I)
              ENDIF
  475     CONTINUE
      ENDIF                                                               
C
C     * ADDITIONAL DIAGNOSTIC VARIABLES. 
C
      DO 500 I=IL1,IL2
          GT(I)=(QLWAVG(I)/SBC)**0.25                                            
          TFLUX(I)=-QSENS(I)/(RHOAIR(I)*SPHAIR)  

                                
          EVAP(I)=EVAP(I)+RHOW*
     1           (FCS(I)*(EVAPCS(I)+EVPCSG(I)) + FGS(I)*EVAPGS(I) +              
     2            FC (I)*(EVAPC (I)+EVAPCG(I)) + FG (I)*EVAPG(I))                                          
          IF(EVPPOT(I).NE.0.0) THEN
              EVAPB(I)=EVAP(I)/EVPPOT(I)
          ELSE
              EVAPB(I)=0.0
          ENDIF
          IF((FCS(I)+FC(I)).GT.1.0E-5) THEN
              TAC(I)=(FCS(I)*TACCS(I)+FC(I)*TACCO(I))/(FCS(I)+FC(I))
              QAC(I)=(FCS(I)*QACCS(I)+FC(I)*QACCO(I))/(FCS(I)+FC(I))
          ELSE
              TAC(I)=TA(I)
              QAC(I)=QA(I)
          ENDIF
         
         

  500 CONTINUE

                                                              
      RETURN                                                                      
      END        
