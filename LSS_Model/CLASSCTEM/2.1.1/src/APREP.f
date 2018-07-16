!>\file
!!Purpose: Calculate various land surface parameters.
!!

!>
!!This subroutine is hard-coded to handle the standard four vegetation categories recognized by CLASS
!!(needleleaf trees, broadleaf trees, crops and grass), so a call to abort is performed if the number of
!!vegetation classes, IC, is not equal to 4. A set of diagnostic and accumulator arrays is then initialized 
!!to zero, and the liquid water suction in the soil is set to an arbitrarily high value.
!!
      SUBROUTINE APREP(FC,FG,FCS,FGS,PAICAN,PAICNS,FSVF,FSVFS, 
     1            FRAINC,FSNOWC,FRAICS,FSNOCS,RAICAN,RAICNS,SNOCAN,
     2            SNOCNS,DISP,DISPS,ZOMLNC,ZOMLCS,ZOELNC,ZOELCS,
     3            ZOMLNG,ZOMLNS,ZOELNG,ZOELNS,CHCAP,CHCAPS,CMASSC,
     4            CMASCS,CWLCAP,CWFCAP,CWLCPS,CWFCPS,RBCOEF,
     5            ZPLIMC,ZPLIMG,ZPLMCS,ZPLMGS,HTCC,HTCS,HTC,
     +            FROOT,FROOTS,
     6            WTRC,WTRS,WTRG,CMAI,PAI,PAIS,AIL,FCAN,FCANS,PSIGND,
     7            FCANMX,ZOLN,PAIMAX,PAIMIN,CWGTMX,ZRTMAX,
     8            PAIDAT,HGTDAT,THLIQ,THICE,TBAR,RCAN,SNCAN,
     9            TCAN,GROWTH,ZSNOW,TSNOW,FSNOW,RHOSNO,SNO,Z0ORO,
     A            ZBLEND,ZPLMG0,ZPLMS0,
     B            TA,RHOAIR,RADJ,DLON,RHOSNI,DELZ,DELZW,ZBOTW,
     C            THPOR,THLMIN,PSISAT,BI,PSIWLT,HCPS,ISAND,
     D            ILG,IL1,IL2,JL,IC,ICP1,IG,IDAY,IDISP,IZREF,IWF,
     E            IPAI,IHGT,RMAT,H,HS,CWCPAV,GROWA,GROWN,GROWB,
     F            RRESID,SRESID,FRTOT,FRTOTS,
     G            FCANCMX,ICTEM,ctem_on,RMATC,
     H            AILC,PAIC,AILCG,L2MAX,NOL2PFTS,
     I            AILCGS,FCANCS,FCANC,ZOLNC,CMASVEGC,SLAIC )
C
C     * AUG 30/16 - J.Melton    Replace ICTEMMOD with ctem_on (logical switch).
C     * JAN 05/15 - J.MELTON.   TREE PFTS NOW HAVE A MINIMUM PAI OF 1 (LIKE
C     *                         CROPS AND GRASSES) TO PREVENT WILD CANOPY TEMPERATURE
C     *                         VALUES WHEN THE CANOPY IS SMALL.
C     * AUG 04/15 - D.VERSEGHY. SPLIT FROOT INTO TWO ARRAYS, FOR CANOPY
C     *                         AREAS WITH AND WITHOUT SNOW.
C     * SEP 05/12 - J.MELTON.   CHANGED IDAY
C                               CONVERSION FROM FLOAT TO REAL, REINTEGRATED
C                               CTEM 
C     * NOV 15/11 - M.LAZARE.   CTEM ADDED. CALCULATIONS ARE DIFFERENT
C     *                         IN SEVERAL AREAS, UNDER CONTROL OF
C     *                         "ICTEMMOD" SWITCH (ICTEMMOD=0 REVERTS
C     *                         BACK TO APREP4 FORMULATION). THIS 
C     *                         INCLUDES NEW INPUT "PAIC".
C     * OCT 07/11 - V.FORTIN/D.VERSEGHY. MAKE THE LIMITING PONDING DEPTH 
C     *                         CALCULATION OVER ORGANIC SOILS THE SAME 
C     *                         AS OVER MINERAL SOILS (LOOP 175).
C     * DEC 23/09 - D.VERSEGHY. IN LIMITING PONDING DEPTH CALCULATIONS,
C     *                         IDENTIFY PEATLANDS WHERE ISAND(I,2)=-2
C     * JAN 06/09 - D.VERSEGHY. REINTRODUCE CHECKS ON FRACTIONAL AREAS.
C     * MAR 25/08 - D.VERSEGHY. DISTINGUISH BETWEEN LEAF AREA INDEX
C     *                         AND PLANT AREA INDEX.
C     * JAN 17/08 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS; REMOVE
C     *                         SUPERFLUOUS CHECKS ON FRACTIONAL AREAS.
C     * NOV 30/06 - E.CHAN/M.LAZARE/D.VERSEGHY. CHANGE RADJ TO REAL;
C     *                         ENSURE CONSISTENCY IN CALCULATION
C     *                         OF FRACTIONAL CANOPY AREAS.
C     * SEP 13/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3 IN 100,
C     *                         450 LOOPS.
C     * MAR 14/05 - D.VERSEGHY. RENAME SCAN TO SNCAN (RESERVED NAME
C     *                         IN F90); TREAT SOIL FROZEN WATER AS ICE
C     *                         VOLUME RATHER THAN AS EQUIVALENT WATER.
C     * MAR 03/05 - Y.DELAGE.   ADD CONTRIBUTION OF SUBGRID-SCALE
C     *                         OROGRAPHY TO ROUGHNESS LENGTH.
C     * JAN 12/05 - P.BARTLETT/D.VERSEGHY. DETERMINE SEPARATE CANOPY
C     *                         WATER INTERCEPTION CAPACITIES FOR
C     *                         RAIN AND SNOW, AND NEW FRACTIONAL
C     *                         CANOPY COVERAGE OF INTERCEPTED RAIN
C     *                         AND SNOW; DEFINE NEW PARAMETER RBCOEF
C     *                         FOR RBINV CALCULATION IN TSOLVC.
C     * NOV 03/04 - D.VERSEGHY. CHANGE RADJ AND DLON TO GATHERED 
C     *                         VARIABLES AND REMOVE ILAND ARRAY;
C     *                         ADD "IMPLICIT NONE" COMMAND.
C     * JUL 05/04 - Y.DELAGE/D.VERSEGHY. PROTECT SENSITIVE CALCULATIONS
C     *                         AGAINST ROUNDOFF ERRORS.
C     * JUL 02/03 - D.VERSEGHY. RATIONALIZE ASSIGNMENT OF RESIDUAL
C     *                         CANOPY MOISTURE TO SOIL LAYERS.
C     * DEC 05/02 - Y.DELAGE/D.VERSEGHY. ADD PARTS OF CANOPY AIR MASS TO 
C     *                         CANOPY MASS ONLY IF IDISP=0 OR IZREF=2.
C     *                         ALSO, REPLACE LOGARITHMIC AVERAGING OF
C     *                         ROUGHNESS HEIGHTS WITH BLENDING HEIGHT
C     *                         AVERAGING.
C     * JUL 31/02 - D.VERSEGHY. MOVE CALCULATION OF PSIGND AND FULL 
C     *                         CALCULATION OF FROOT INTO THIS ROUTINE
C     *                         FROM TPREP; REMOVE CALCULATION OF RCMIN.
C     *                         SHORTENED CLASS3 COMMON BLOCK.
C     * JUL 23/02 - D.VERSEGHY. MOVE ADDITION OF AIR TO CANOPY MASS
C     *                         INTO THIS ROUTINE; SHORTENED CLASS4
C     *                         COMMON BLOCK.
C     * MAR 18/02 - D.VERSEGHY. MOVE CALCULATION OF SOIL PROPERTIES INTO
C     *                         ROUTINE "CLASSB"; ALLOW FOR ASSIGNMENT 
C     *                         OF SPECIFIED TIME-VARYING VEGETATION
C     *                         HEIGHT AND LEAF AREA INDEX.
C     * SEP 19/00 - D.VERSEGHY. ADD CALCULATION OF VEGETATION-DEPENDENT
C     *                         COEFFICIENTS FOR DETERMINATION OF STOMATAL 
C     *                         RESISTANCE.
C     * APR 12/00 - D.VERSEGHY. RCMIN NOW VARIES WITH VEGETATION TYPE:
C     *                         PASS IN BACKGROUND ARRAY "RCMINX".
C     * DEC 16/99 - A.WU/D.VERSEGHY. ADD CALCULATION OF NEW LEAF DIMENSION 
C     *                              PARAMETER FOR REVISED CANOPY TURBULENT
C     *                              TRANSFER FORMULATION.
C     * NOV 16/98 - M.LAZARE.   "DLON" NOW PASSED IN AND USED DIRECTLY
C     *                         (INSTEAD OF INFERRING FROM "LONSL" AND 
C     *                         "ILSL" WHICH USED TO BE PASSED) TO CALCULATE
C     *                         GROWTH INDEX. THIS IS DONE TO MAKE THE PHYSICS
C     *                         PLUG COMPATIBLE FOR USE WITH THE RCM WHICH 
C     *                         DOES NOT HAVE EQUALLY-SPACED LONGITUDES.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * OCT 11/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         BUG FIX: TO AVOID ROUND-OFF ERRORS,
C     *                         SET CANOPY COVER EQUAL TO 1 IF THE
C     *                         CALCULATED SUM OF FC AND FCS IS
C     *                         VERY CLOSE TO 1.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.
C     *                         ALSO CORRECT BUG IN CALCULATION OF
C     *                         DEGLON, AND USE IDISP TO DETERMINE
C     *                         METHOD OF CALCULATING DISP AND DISPS.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 16/95 - D.VERSEGHY. THREE NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * NOV 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         RATIONALIZE CALCULATION OF RCMIN. 
C     * NOV 12/94 - D.VERSEGHY. FIX BUGS IN SENESCING LIMB OF CROP
C     *                         GROWTH INDEX AND IN CANOPY MASS
C     *                         CALCULATION.
C     * MAY 06/93 - M.LAZARE/D.VERSEGHY. CLASS - VERSION 2.1.
C     *                                  USE NEW "CANEXT" CANOPY 
C     *                                  EXTINCTION ARRAY TO DEFINE
C     *                                  SKY-VIEW FACTORS. ALSO, CORRECT
C     *                                  MINOR BUG WHERE HAD "IF(IN.LE.9)..."
C     *                                  INSTEAD OF "IF(IN.GT.9)...".  
C     * DEC 12/92 - M.LAZARE.   MODIFIED FOR MULTIPLE LATITUDES.
C     * OCT 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CALCULATION OF LAND SURFACE CANOPY 
C     *                         PARAMETERS.
C
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IC,ICP1,IG,IDAY,IDISP,IZREF,IWF,
     1        IPAI,IHGT,I,J,K,IN,NL
C                                                                                 
C     * OUTPUT ARRAYS USED ELSEWHERE IN CLASS.                                    
C                                                                                 
      REAL FC    (ILG) !<Subarea fractional coverage of modelled area (X) [ ]
      REAL FG    (ILG) !<Subarea fractional coverage of modelled area (X) [ ]
      REAL FCS   (ILG) !<Subarea fractional coverage of modelled area (X) [ ]
      REAL FGS   (ILG) !<Subarea fractional coverage of modelled area (X) [ ]
    
      REAL PAICAN(ILG) !<Plant area index of canopy over bare ground (\f$\Lambda_p\f$) [ ]
      REAL PAICNS(ILG) !<Plant area index of canopy over snow (\f$\Lambda_p\f$) [ ]

      REAL FSVF  (ILG) !<Sky view factor for bare ground under canopy (\f$\chi\f$) [ ]
      REAL FSVFS (ILG) !<Sky view factor for snow under canopy (\f$\chi\f$) [ ]
      REAL FRAINC(ILG) !<Fractional coverage of canopy by liquid water over snow-free subarea [ ]
      REAL FSNOWC(ILG) !<Fractional coverage of canopy by frozen water over snow-free subarea [ ]
      REAL FRAICS(ILG) !<Fractional coverage of canopy by liquid water over snow-covered subarea [ ]
      REAL FSNOCS(ILG) !<Fractional coverage of canopy by frozen water over snow-covered subarea [ ]

      REAL RAICAN(ILG) !<Intercepted liquid water stored on canopy over bare ground (\f$W_l\f$) [\f$kg m^{-2}\f$] 
      REAL RAICNS(ILG) !<Intercepted liquid water stored on canopy over snow (\f$W_l\f$) [\f$kg m^{-2}\f$]
      REAL SNOCAN(ILG) !<Intercepted frozen water stored on canopy over bare soil (\f$W_f\f$) [\f$kg m^{-2}\f$]
      REAL SNOCNS(ILG) !<Intercepted frozen water stored on canopy over snow (\f$W_f\f$) [\f$kg m^{-2}\f$]

      REAL DISP  (ILG) !<Displacement height of vegetation over bare ground (d) [m]
      REAL DISPS (ILG) !<Displacement height of vegetation over snow (d) [m]

      REAL ZOMLNC(ILG) !<Logarithm of roughness length for momentum of vegetation over bare ground [ ]
      REAL ZOMLCS(ILG) !<Logarithm of roughness length for momentum of vegetation over snow [ ]
      REAL ZOELNC(ILG) !<Logarithm of roughness length for heat of vegetation over bare ground [ ]
      REAL ZOELCS(ILG) !<Logarithm of roughness length for heat of vegetation over snow [ ]
      REAL ZOMLNG(ILG) !<Logarithm of roughness length for momentum of bare ground [ ]
      REAL ZOMLNS(ILG) !<Logarithm of roughness length for momentum of snow [ ]
      REAL ZOELNG(ILG) !<Logarithm of roughness length for heat of bare ground [ ]
      REAL ZOELNS(ILG) !<Logarithm of roughness length for heat of snow [ ]
      REAL RBCOEF(ILG) !<Parameter for calculation of leaf boundary resistance (\f$C_{rb}\f$)
      REAL CHCAP (ILG) !<Heat capacity of canopy over bare ground [\f$J m^{-2} K^{-1}\f$]
      REAL CHCAPS(ILG) !<Heat capacity of canopy over snow [\f$J m^{-2} K^{-1}\f$]
      REAL CMASSC(ILG) !<Mass of canopy over bare ground [\f$kg m^{-2}\f$]
      REAL CMASCS(ILG) !<Mass of canopy over snow [\f$kg m^{-2}\f$]
      REAL CWLCAP(ILG) !<Storage capacity of canopy over bare ground for liquid water (\f$W_{l,max}\f$) [\f$kg m^{-2}\f$]
      REAL CWFCAP(ILG) !<Storage capacity of canopy over bare ground for frozen water (\f$W_{f,max}\f$) [\f$kg m^{-2}\f$]
      REAL CWLCPS(ILG) !<Storage capacity of canopy over snow for liquid water (\f$W_{l,max}\f$) [\f$kg m^{-2}\f$]
      REAL CWFCPS(ILG) !<Storage capacity of canopy over snow for frozen water (\f$W_{f,max}\f$) [\f$kg m^{-2}\f$]

      REAL ZPLIMC(ILG) !<Maximum water ponding depth for ground under canopy [m]
      REAL ZPLIMG(ILG) !<Maximum water ponding depth for bare ground [m]
      REAL ZPLMCS(ILG) !<Maximum water ponding depth for ground under snow under canopy [m]
      REAL ZPLMGS(ILG) !<Maximum water ponding depth for ground under snow [m]

      REAL HTCC  (ILG) !<Diagnosed internal energy change of vegetation canopy
                       !!due to conduction and/or change in mass [\f$W m^{-2}\f$]
      REAL HTCS  (ILG) !<Diagnosed internal energy change of snow pack
                       !!due to conduction and/or change in mass [\f$W m^{-2}\f$]
      REAL WTRC  (ILG) !<Diagnosed residual water transferred off the vegetation canopy [\f$kg m^{-2} s^{-1}\f$]
      REAL WTRS  (ILG) !<Diagnosed residual water transferred into or out of the snow pack [\f$kg m^{-2} s^{-1}\f$]
      REAL WTRG  (ILG) !<Diagnosed residual water transferred into or out of the soil [\f$kg m^{-2} s^{-1}\f$]
      REAL CMAI  (ILG) !<Aggregated mass of vegetation canopy [\f$kg m^{-2}\f$]

      REAL FROOT (ILG,IG) !<Fraction of total transpiration contributed by soil layer [ ]
      REAL FROOTS(ILG,IG) !<
      REAL HTC   (ILG,IG) !<Diagnosed internal energy change of soil layer
                          !!due to conduction and/or change in mass [\f$W m^{-2}\f$]

C                                                                                 
C     * OUTPUT ARRAYS ONLY USED ELSEWHERE IN CLASSA.                              
C                                                                                 
      REAL PAI   (ILG,IC) !<Plant area index of vegetation category over bare ground [ ]
      REAL PAIS  (ILG,IC) !<Plant area index of vegetation category over snow [ ]
      REAL AIL   (ILG,IC) !<Leaf area index of vegetation category over bare ground [ ]
      REAL FCAN  (ILG,IC) !<Fractional coverage of vegetation category over bare ground (\f$X_i\f$) [ ]
      REAL FCANS (ILG,IC) !<Fractional coverage of vegetation category over snow (\f$X_i\f$) [ ]
      REAL PSIGND(ILG)    !<Minimum liquid moisture suction in soil layers [m]

C                                                                                 
C     * INPUT ARRAYS.                                      
C                                                                                 
      REAL FCANMX(ILG,ICP1) !<Maximum fractional coverage of modelled area by vegetation category [ ]
      REAL ZOLN  (ILG,ICP1) !<Natural logarithm of maximum roughness length of vegetation category [ ]
      REAL PAIMAX(ILG,IC)   !<Maximum plant area index of vegetation category [ ]
      REAL PAIMIN(ILG,IC)   !<Minimum plant area index of vegetation category [ ]
      REAL CWGTMX(ILG,IC)   !<Maximum canopy mass for vegetation category [\f$kg m^{-2}\f$]
      REAL ZRTMAX(ILG,IC)   !<Maximum rooting depth of vegetation category [m]
      REAL PAIDAT(ILG,IC)   !<Optional user-specified value of plant area indices of
                            !!vegetation categories to override CLASS-calculated values [ ]
      REAL HGTDAT(ILG,IC)   !<Optional user-specified values of height of
                            !!vegetation categories to override CLASS-calculated values [m]
      REAL THLIQ (ILG,IG)   !<Volumetric liquid water content of soil layers (\f$\theta\f$ l) [\f$m^3 m^{-3}\f$]
      REAL THICE (ILG,IG)   !<Frozen water content of soil layers under vegetation [\f$m^3 m^{-3}\f$]
      REAL TBAR  (ILG,IG)   !<Temperature of soil layers [K]

      REAL RCAN  (ILG) !<Intercepted liquid water stored on canopy (\f$W_l\f$) [\f$kg m^{-2}\f$]
      REAL SNCAN (ILG) !<Intercepted frozen water stored on canopy (\f$W_f\f$) [\f$kg m^{-2}\f$]
      REAL TCAN  (ILG) !<Vegetation canopy temperature [K]
      REAL GROWTH(ILG) !<Vegetation growth index [ ]
      REAL ZSNOW (ILG) !<Depth of snow pack (\f$z_s\f$) [m]
      REAL TSNOW (ILG) !<Snowpack temperature [K]
      REAL FSNOW (ILG) !<Diagnosed fractional snow coverage [ ]
      REAL RHOSNO(ILG) !<Density of snow (\f$ s\f$) [\f$kg m^{-3}\f$]
      REAL SNO   (ILG) !<Mass of snow pack (\f$W_s\f$) [\f$kg m^{-2}\f$]
      REAL TA    (ILG) !<Air temperature at reference height [K]
      REAL RHOAIR(ILG) !<Density of air [\f$kg m^{-3}\f$]
      REAL DLON  (ILG) !<Longitude of grid cell (east of Greenwich) [degrees]
      REAL Z0ORO (ILG) !<Orographic roughness length [m]
      REAL ZBLEND(ILG) !<Atmospheric blending height for surface roughness length averaging (\f$z_b\f$) [m]
      REAL RHOSNI(ILG) !<Density of fresh snow (\f$\rho\f$ s,f) [\f$kg m^{-3}\f$]
      REAL ZPLMG0(ILG) !<Maximum water ponding depth for snow-free subareas
                       !!(user-specified when running MESH code) [m]
      REAL ZPLMS0(ILG) !<Maximum water ponding depth for snow-covered subareas
                       !!(user-specified when running MESH code) [m]
      REAL RADJ  (ILG) !<Latitude of grid cell (positive north of equator) [rad]

C
C     * SOIL PROPERTY ARRAYS.                                     
C                                                                                 
      REAL DELZW (ILG,IG) !<Permeable thickness of soil layer [m]
      REAL ZBOTW (ILG,IG) !<Depth to permeable bottom of soil layer [m]
      REAL THPOR (ILG,IG) !<Pore volume in soil layer (\f$\theta\f$ p) [\f$m^3 m^{-3}\f$]
      REAL THLMIN(ILG,IG) !<Residual soil liquid water content remaining after freezing or evaporation [\f$m^3 m^{-3}\f$]
      REAL PSISAT(ILG,IG) !<Soil moisture suction at saturation (\f$\Psi\f$ sat) [m]
      REAL BI    (ILG,IG) !<Clapp and Hornberger empirical "b" parameter [ ]
      REAL PSIWLT(ILG,IG) !<Soil moisture suction at wilting point (\f$\Psi\f$ w) [m]
      REAL HCPS  (ILG,IG) !<Volumetric heat capacity of soil particles [\f$J m^{-3}\f$]

C                                                                                 
      INTEGER ISAND (ILG,IG) !<Sand content flag

C                                               
C     * OTHER DATA ARRAYS WITH NON-VARYING VALUES.
C                                                                                 

      REAL GROWYR(18,4,2) !<
      REAL DELZ  (IG)     !<Soil layer thickness [m]
      REAL ZORAT (4)      !<
      REAL CANEXT(4)      !<
      REAL XLEAF (4)      !<
C                                                                                 
C     * WORK ARRAYS NOT USED ELSEWHERE IN CLASSA.                          
C                                                                                 
      REAL RMAT (ILG,IC,IG),H     (ILG,IC),  HS    (ILG,IC),                      
     1     CWCPAV(ILG),     GROWA (ILG),     GROWN (ILG),     
     2     GROWB (ILG),     RRESID(ILG),     SRESID(ILG),
     3     FRTOT (ILG),     FRTOTS(ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL DAY,GROWG,FSUM,SNOI,ZSNADD,THSUM,THICEI,THLIQI,ZROOT,
     1     ZROOTG,FCOEFF,PSII,LZ0ORO,THR_LAI,PSIRAT
C
C     * CTEM-RELATED FIELDS.
C
      REAL  AILC (ILG,IC)
      REAL  PAIC   (ILG,IC)
      REAL  AILCG(ILG,ICTEM)   !<GREEN LAI FOR USE WITH PHTSYN SUBROUTINE
      REAL  AILCGS (ILG,ICTEM) !<GREEN LAI FOR CANOPY OVER SNOW SUB-AREA
      REAL  RMATC(ILG,IC,IG)
      REAL  FCANCMX(ILG,ICTEM)  
      REAL  FCANC(ILG,ICTEM)   !<FRACTION OF CANOPY OVER GROUND FOR CTEM's 9 PFTs
      REAL  FCANCS (ILG,ICTEM) !<FRACTION OF CANOPY OVER SNOW FOR CTEM's 9 PFTs
      REAL  ZOLNC(ILG,IC)
      REAL  CMASVEGC(ILG,IC)
      REAL  SLAIC(ILG,IC)
C
C     * NOL2PFTS - NUMBER OF LEVEL 2 CTEM PFTs
C     * SEE BIO2STR SUBROUTINE FOR EXPLANATION OF OTHER CTEM VARIABLES

C     * INTERNAL WORK FIELD.
C
      REAL  SFCANCMX(ILG,IC)
C
      LOGICAL ctem_on

      INTEGER ICTEM, M, N, K1, K2, L2MAX, NOL2PFTS(IC)
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,
     1     HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,
     2     SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP,PI,ZOLNG,ZOLNS,ZOLNI,
     3     ZORATG     
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /CLASS6/ PI,GROWYR,ZOLNG,ZOLNS,ZOLNI,ZORAT,ZORATG     
      COMMON /CLASS7/ CANEXT,XLEAF
C-----------------------------------------------------------------------          
      IF(IC.NE.4)                               CALL XIT('APREP',-2)
C
C     * INITIALIZE DIAGNOSTIC AND OTHER ARRAYS.
C
      DO 100 I=IL1,IL2
          HTCC(I) =0.0
          HTCS(I) =0.0
          DO 50 J=1,IG
              HTC(I,J)=0.0
   50     CONTINUE
          WTRC(I) =0.0
          WTRS(I) =0.0
          WTRG(I) =0.0
          FRTOT(I)=0.0
          FRTOTS(I)=0.0
          DISP  (I)=0.                                                            
          ZOMLNC(I)=0.                                                            
          ZOELNC(I)=1.                                                            
          DISPS (I)=0.                                                            
          ZOMLCS(I)=0.                                                            
          ZOELCS(I)=1.                                                            
          ZOMLNG(I)=0.                                                            
          ZOELNG(I)=0.                                                            
          ZOMLNS(I)=0.                                                            
          ZOELNS(I)=0.                                                            
          CMASSC(I)=0.                                                            
          CMASCS(I)=0.                                                            
          PSIGND(I)=1.0E+5
  100 CONTINUE
C 
C     * DETERMINE GROWTH INDEX FOR CROPS (VEGETATION TYPE 3).
C     * MUST USE UN-GATHERED LONGITUDES TO COMPUTE ACTUAL LONGITUDE/
C     * LATITUDE VALUES.  
C                                                                                 
      DAY=REAL(IDAY)                                                             
C
C     * FOR CTEM, CROP GROWTH IS BUILT IN, SO GROWA=1.
C
      IF (.not. ctem_on) THEN
        !>
        !!In the 120 loop, the growth index for crops, GROWA, is calculated (if CLASS is not being run coupled to
        !!CTEM). This is done by referring to the three-dimensional array GROWYR, which contains values
        !!corresponding to the four Julian days of the year on which crops are planted, on which they reach
        !!maturity, on which harvesting begins, and on which the harvest is complete, for each ten-degree latitude
        !!half-circle in each hemisphere. These are generic, average dates, approximated using information gleaned
        !!from annual UN FAO (Food and Agriculture Organization) reports. (In the tropics, it is assumed that
        !!areas classified as agricultural are constantly under cultivation, so all four values are set to zero.)
        !!
        !!First, the latitude of the modelled area is converted from a value in radians to a value from 1 to 18, IN,
        !!corresponding to the index of the latitude circle (1 for latitudes \f$80-90^o S\f$, 18 for latitudes \f$80-90^o N\f$). Then
        !!the hemisphere index, NL, is set to 1 for the Eastern Hemisphere, and 2 for the Western Hemisphere. If
        !!the planting date for the modelled area is zero (indicating a location in the tropics), GROWA is set to 1.
        !!Otherwise, GROWA is set to 1 if the day of the year lies between the maturity date and the start of the
        !!54harvest, and to zero if the day of the year lies between the end of the harvest and the planting date. For
        !!dates in between, the value of GROWA is interpolated between 0 and 1. Checks are performed at the
        !!end to ensure that GROWA is not less than 0 or greater than 1. If the calculated value of GROWA is
        !!vanishingly small, it is set to zero.
        !!
        DO 120 I=IL1,IL2
          IN = INT( (RADJ(I)+PI/2.0)*18.0/PI ) + 1
          IF(DLON(I).GT.190. .AND. DLON(I).LT.330.)            THEN           
              NL=2                                                            
          ELSE                                                                
              NL=1                                                            
          ENDIF                                                               
          IF(GROWYR(IN,1,NL).LT.0.1)                           THEN           
              GROWA(I)=1.0                                                    
          ELSE                                                                
              IF(IN.GT.9)                                 THEN
                IF(DAY.GE.GROWYR(IN,2,NL).AND.DAY.LT.GROWYR(IN,3,NL))           
     1              GROWA(I)=1.0                                                
                IF(DAY.GE.GROWYR(IN,4,NL).OR.DAY.LT.GROWYR(IN,1,NL))            
     1              GROWA(I)=0.0                                
              ELSE
                IF(DAY.GE.GROWYR(IN,2,NL).OR.DAY.LT.GROWYR(IN,3,NL))           
     1              GROWA(I)=1.0                                                
                IF(DAY.GE.GROWYR(IN,4,NL).AND.DAY.LT.GROWYR(IN,1,NL))            
     1              GROWA(I)=0.0                                
              ENDIF                
              IF(DAY.GE.GROWYR(IN,1,NL).AND.DAY.LT.GROWYR(IN,2,NL))           
     1            GROWA(I)=(DAY-GROWYR(IN,1,NL))/(GROWYR(IN,2,NL)-            
     2                     GROWYR(IN,1,NL))                                   
              IF(DAY.GE.GROWYR(IN,3,NL).AND.DAY.LT.GROWYR(IN,4,NL))           
     1            GROWA(I)=(GROWYR(IN,4,NL)-DAY)/(GROWYR(IN,4,NL)-            
     2                     GROWYR(IN,3,NL))                                   
              GROWA(I)=MAX(0.0,MIN(GROWA(I),1.0))
              IF(GROWA(I).LT.1.0E-5) GROWA(I)=0.0
          ENDIF                                                               
  120   CONTINUE                                                                
      ELSE
        DO I=IL1,IL2
          GROWA(I)=1.
        ENDDO
      ENDIF
C                                                                                 
C     * DETERMINE GROWTH INDICES FOR NEEDLELEAF TREES, BROADLEAF
C     * TREES AND GRASS (VEGETATION TYPES 1, 2 AND 4); CALCULATE
C     * VEGETATION HEIGHT, CORRECTED FOR GROWTH STAGE FOR CROPS
C     * AND FOR SNOW COVER FOR CROPS AND GRASS; CALCULATE CURRENT
C     * LEAF AREA INDEX FOR FOUR VEGETATION TYPES.
C

      !>
      !!In the 150 loop the other three growth indices are evaluated, as well as the vegetation heights and plant
      !!area indices for the four vegetation categories over snow-covered and snow-free ground. The
      !!background growth index for trees, GROWTH, is evaluated separately in subroutine CGROW. It varies
      !!from a value of 0 for dormant or leafless periods to 1 for fully-leafed periods, with a sixty-day transition
      !!between the two. When senescence begins, it is set instantaneously to -1 and thereafter increases over a
      !!sixty-day period back to 0. (The onset of spring budburst and fall senescence are triggered by near-zero
      !!values of the air temperature and the first soil layer temperature.) For needleleaf trees, the growth index
      !!GROWN is simply set to the absolute value of GROWTH. For broadleaf trees, the transition period is
      !!assumed to last thirty days instead of sixty, and so the growth index GROWB is set to the absolute value
      !!of double the value of GROWTH, with upper and lower limits of 1 and 0. Finally, the growth index of
      !!grasses is set to 1 all year round.
      !!
      DO 150 I=IL1,IL2    
                                                    
          GROWN(I)=ABS(GROWTH(I))                                                 
          IF(GROWTH(I).GT.0.0)                      THEN                          
              GROWB(I)=MIN(1.0,GROWTH(I)*2.0)                                   
          ELSE                                                                    
              GROWB(I)=MAX(0.0,(ABS(GROWTH(I))*2.0-1.0))                        
          ENDIF                                                                   
          GROWG=1.0                                                               
C                
C    ----------------- CTEM MODIFICATIONS -----------------------------\
C    IF USING CTEM's STRUCTURAL ATTRIBUTES OVERWRITE ZOLN

          IF (ctem_on) THEN
            ZOLN(I,1)=ZOLNC(I,1)
            ZOLN(I,2)=ZOLNC(I,2)
            ZOLN(I,3)=ZOLNC(I,3)
            ZOLN(I,4)=ZOLNC(I,4)
          ENDIF
C    ----------------- CTEM MODIFICATIONS -----------------------------/
C
      !>
      !!A branch in the code occurs next, depending on the value of the flag IHGT. If IHGT=0, the values of
      !!vegetation height calculated by CLASS are to be used. For trees and grass, the vegetation height under
      !!snow-free conditions is assumed to be constant year-round, and is calculated as 10 times the exponential
      !!of ZOLN, the logarithm of the maximum vegetation roughness length. For crops, this maximum height
      !!is multiplied by GROWA. If IHGT=1, vegetation heights specified by the user are utilized instead. This
      !!height H for each of the four vegetation categories is used to calculate the height HS over snow-covered
      !!areas. For needleleaf and broadleaf trees, HS is set to H. For crops and grass, HS is calculated by
      !!subtracting the snow depth ZSNOW from H, to account for the burying of short vegetation by snow.
      !!

          IF(IHGT.EQ.0) THEN
              H(I,1)=10.0*EXP(ZOLN(I,1))                                              
              H(I,2)=10.0*EXP(ZOLN(I,2))                                              
              H(I,3)=10.0*EXP(ZOLN(I,3))*GROWA(I)                                     
              H(I,4)=10.0*EXP(ZOLN(I,4))                                              
          ELSE
              H(I,1)=HGTDAT(I,1)
              H(I,2)=HGTDAT(I,2)
              H(I,3)=HGTDAT(I,3)
              H(I,4)=HGTDAT(I,4)
          ENDIF
          HS(I,1)=H(I,1)                                                          
          HS(I,2)=H(I,2)                                                          
          HS(I,3)=MAX(H(I,3)-ZSNOW(I),1.0E-3)                                       
          HS(I,4)=MAX(H(I,4)-ZSNOW(I),1.0E-3)                                       

      !>
      !!If CLASS is being run uncoupled to CTEM, a second branch now occurs, depending on the value of the
      !!flag IPAI. If IPAI=0, the values of plant area index calculated by CLASS are to be used. For all four
      !!vegetation categories, the plant area index over snow-free ground, PAI, is determined by interpolating
      !!between the annual maximum and minimum plant area indices using the growth index. If IPAI=1, plant
      !!area index values specified by the user are utilized instead. For trees, the plant area index over snow-
      !!covered ground, PAIS, is set to PAI. For crops and grass, if H>0, PAIS is set to PAI scaled by the ratio
      !!of HS/H; otherwise, it is set to zero. Lastly, the leaf area indices for the four vegetation categories over
      !!snow-free ground, AIL, are determined from the PAI values. For needleleaf trees, AIL is estimated as
      !!0.90 PAI; for broadleaf trees it is estimated as the excess PAI over the annual minimum value. For crops
      !!and grass AIL is assumed to be equal to PAI. (If CLASS is being run coupled to CTEM, the CTEM-
      !!generated values of PAI and AIL are used instead.)
      !!    
          IF(IPAI.EQ.0) THEN
C    ----------------- CTEM MODIFICATIONS -----------------------------\
C             USE CTEM GENERATED PAI OR CLASS' OWN SPECIFIED PAI
              IF (ctem_on) THEN
                PAI(I,1)=PAIC(I,1)
                PAI(I,2)=PAIC(I,2)
                PAI(I,3)=PAIC(I,3)
                PAI(I,4)=PAIC(I,4)
              ELSE
                PAI(I,1)=PAIMIN(I,1)+GROWN(I)*(PAIMAX(I,1)-PAIMIN(I,1))                 
                PAI(I,2)=PAIMIN(I,2)+GROWB(I)*(PAIMAX(I,2)-PAIMIN(I,2))                 
                PAI(I,3)=PAIMIN(I,3)+GROWA(I)*(PAIMAX(I,3)-PAIMIN(I,3))                 
                PAI(I,4)=PAIMIN(I,4)+GROWG   *(PAIMAX(I,4)-PAIMIN(I,4)) 
              ENDIF
C    ----------------- CTEM MODIFICATIONS -----------------------------/
C
          ELSE
              PAI(I,1)=PAIDAT(I,1)
              PAI(I,2)=PAIDAT(I,2)
              PAI(I,3)=PAIDAT(I,3)
              PAI(I,4)=PAIDAT(I,4)
          ENDIF
          PAIS(I,1)=PAI(I,1)                                                      
          PAIS(I,2)=PAI(I,2)                                                      
          IF(H(I,3).GT.0.0) THEN                                                  
              PAIS(I,3)=PAI(I,3)*HS(I,3)/H(I,3)                                   
          ELSE                                                                    
              PAIS(I,3)=0.0                                                       
          ENDIF                                                                   
          IF(H(I,4).GT.0.0) THEN                                                  
              PAIS(I,4)=PAI(I,4)*HS(I,4)/H(I,4)                                   
          ELSE                                                                    
              PAIS(I,4)=0.0                                                       
          ENDIF                                                                   
C
C    ----------------- CTEM MODIFICATIONS -----------------------------\
C
          IF (ctem_on) THEN
             AIL(I,1)=MAX(AILC(I,1), SLAIC(I,1))
             AIL(I,2)=MAX(AILC(I,2), SLAIC(I,2))
             AIL(I,3)=MAX(AILC(I,3), SLAIC(I,3))
             AIL(I,4)=MAX(AILC(I,4), SLAIC(I,4))
          ELSE
C    ----------------- CTEM MODIFICATIONS -----------------------------/
C
            AIL(I,1)=PAI(I,1)*0.90
            AIL(I,2)=MAX((PAI(I,2)-PAIMIN(I,2)),0.0)
            AIL(I,3)=PAI(I,3)
            AIL(I,4)=PAI(I,4)
          ENDIF
    
C    ----------------- CTEM MODIFICATIONS -----------------------------\
C
C         ESTIMATE GREEN LAI FOR CANOPY OVER SNOW FRACTION FOR CTEM's
C         9 PFTs, JUST LIKE CLASS DOES.
C
          IF (ctem_on) THEN
            AILCGS(I,1)=AILCG(I,1)    !<NDL EVG
            AILCGS(I,2)=AILCG(I,2)    !<NDL DCD
            AILCGS(I,3)=AILCG(I,3)    !<BDL EVG
            AILCGS(I,4)=AILCG(I,4)    !<BDL DCD CLD
            AILCGS(I,5)=AILCG(I,5)    !<BDL DCD DRY
            IF(H(I,3).GT.0.0) THEN
              AILCGS(I,6)=AILCG(I,6)*HS(I,3)/H(I,3)  !<C3 CROP
              AILCGS(I,7)=AILCG(I,7)*HS(I,3)/H(I,3)  !<C4 CROP
            ELSE
              AILCGS(I,6)=0.0
              AILCGS(I,7)=0.0
            ENDIF
            IF(H(I,4).GT.0.0) THEN
              AILCGS(I,8)=AILCG(I,8)*HS(I,4)/H(I,4)  !<C3 GRASS
              AILCGS(I,9)=AILCG(I,9)*HS(I,4)/H(I,4)  !<C4 GRASS
            ELSE
              AILCGS(I,8)=0.0
              AILCGS(I,9)=0.0
            ENDIF
          ENDIF
C    ----------------- CTEM MODIFICATIONS -----------------------------/
C
  150 CONTINUE         
C
C     * ADJUST FRACTIONAL COVERAGE OF GRID CELL FOR CROPS AND
C     * GRASS IF LAI FALLS BELOW A SET THRESHOLD VALUE DUE TO 
C     * GROWTH STAGE OR SNOW COVER; RESET LAI TO THE THRESHOLD
C     * VALUE; CALCULATE RESULTANT GRID CELL COVERAGE BY CANOPY, 
C     * BARE GROUND, CANOPY OVER SNOW AND SNOW OVER BARE GROUND.
C     * 
C     * ALSO CALCULATE SURFACE DETENTION CAPACITY FOR FOUR
C     * GRID CELL SUBAREAS BASED ON VALUES SUPPLIED BY 
C     * U. OF WATERLOO:
C     *        IMPERMEABLE SURFACES: 0.001 M.
C     *        BARE SOIL:            0.002 M.
C     *        LOW VEGETATION:       0.003 M.
C     *        FOREST:               0.01  M.
C                                                                                 
      THR_LAI=1.0
      !>
      !!In the 175 loop, the fractional coverage of the modelled area by each of the four vegetation categories is
      !!calculated, for snow-free (FCAN) and snow-covered ground (FCANS). For needleleaf and broadleaf
      !!trees, FCAN is set to the maximum coverage FCANMX of the vegetation category, scaled by the snow-
      !!free fraction of the modelled area, 1-FSNOW. For crops and grass, this calculation is modified for cases
      !!where the plant area index has been calculated as falling below a threshold value owing to growth stage or
      !!burying by snow. (If CLASS is being run coupled to CTEM, this threshold value is set to 0.05; otherwise
      !!it is set to 1.) In such cases the vegetation coverage is assumed to become discontinuous, and so an
      !!additional multiplication by PAI is performed to produce a reduced value of FCAN, and PAI is reset to
      !!the threshold value. An identical procedure is followed to determine the FCANS values.
      !!
      !!The areal fractions of each of the four CLASS subareas, vegetation over bare soil (FC), bare soil (FG),
      !!vegetation over snow (FCS) and snow (FGS) are then calculated, using the FCAN and FCANS values and
      !!FSNOW. Checks are carried out, and adjustments performed if necessary, to ensure that none of the
      !!four subareas is vanishingly small. The values of FSNOW and of the four FCANs and FCANSs are
      !!recalculated accordingly. Finally, checks are carried out to ensure that each of the four subareas is greater
      !!than zero, and that their sum is unity. If this is not the case, a call to abort is performed.
      !!In the last part of the 175 loop, the limiting ponding depth for surface water is determined for each of the
      !!four subareas. If the flag IWF is zero, indicating that lateral flow of water within the soil is to be
      !!neglected, these values are assigned as follows. If the index ISAND of the first soil layer is -3 or -4,
      !!indicating a rock surface or an ice sheet, the bare soil ponding limit, ZPLIMG, is set to 1 mm; otherwise
      !!ZPLIMG is set to 2 mm. If the fractional area of snow on bare soil is greater than zero, the subarea
      !!ponding limit ZPLMGS is set to the weighted average of ZPLIMG over the areas where snow has not
      !!buried vegetation and where it has buried crops, and to 3 mm over areas where it has buried grass;
      !!otherwise to zero. If the fractional area of canopy over bare soil is greater than zero, the subarea ponding
      !!depth ZPLIMC is set to the weighted average of 1 cm under trees and 3 mm under crops and grass;
      !!otherwise to zero. If the fractional area of canopy over snow is greater than zero, the subarea ponding
      !!depth ZPLMCS is also currently set to the weighted average of 1 cm under trees and 3 mm under crops
      !!and grass; otherwise to zero. Finally, if the flag IWF is greater than zero, indicating that lateral flow of soil
      !!water is being modelled, externally derived user-specified values of the ponding limit for the four subareas
      !!are assigned.
      !!

      DO 175 I=IL1,IL2                                                            
          FCAN(I,1)=FCANMX(I,1)*(1.0-FSNOW(I))                                    
          FCAN(I,2)=FCANMX(I,2)*(1.0-FSNOW(I))                                    
          IF(FCAN(I,1).LT.1.0E-5) FCAN(I,1)=0.0
          IF(FCAN(I,2).LT.1.0E-5) FCAN(I,2)=0.0

          ! PAI has a minimum value of 1.0 for all PFTs. This is to prevent
          ! wild canopy temperature values that could occur when the canopy
          ! size is small.
          do j = 1,4
           IF(PAI(I,j).LT.THR_LAI) THEN
             FCAN(I,j)=FCANMX(I,j)*(1.0-FSNOW(I))*PAI(I,j)
             PAI (I,j)=THR_LAI
           ELSE
             FCAN(I,j)=FCANMX(I,j)*(1.0-FSNOW(I))
           ENDIF
          end do
          IF(FCAN(I,3).LT.1.0E-5) FCAN(I,3)=0.0
          IF(FCAN(I,4).LT.1.0E-5) FCAN(I,4)=0.0
C                                                                                 
          FCANS(I,1)=FCANMX(I,1)*FSNOW(I)                                         
          FCANS(I,2)=FCANMX(I,2)*FSNOW(I)                                         
          IF(FCANS(I,1).LT.1.0E-5) FCANS(I,1)=0.0
          IF(FCANS(I,2).LT.1.0E-5) FCANS(I,2)=0.0
          do j = 1,4
            IF(PAIS(I,j).LT.THR_LAI) THEN
              FCANS(I,j)=FCANMX(I,j)*FSNOW(I)*PAIS(I,j)
              PAIS (I,j)=THR_LAI
            ELSE
              FCANS(I,j)=FCANMX(I,j)*FSNOW(I)
            ENDIF
          end do
          IF(FCANS(I,3).LT.1.0E-5) FCANS(I,3)=0.0
          IF(FCANS(I,4).LT.1.0E-5) FCANS(I,4)=0.0
C                                                                                 
          FC (I)=FCAN(I,1)+FCAN(I,2)+FCAN(I,3)+FCAN(I,4)                
          FG (I)=1.0-FSNOW(I)-FC(I)                                     
          FCS(I)=FCANS(I,1)+FCANS(I,2)+FCANS(I,3)+FCANS(I,4)            
          FGS(I)=FSNOW(I)-FCS(I)                                        
          IF(ABS(1.0-FCS(I)-FC(I)).LT.8.0E-5) THEN
              IF(FCS(I).LT.1.0E-5) THEN
                FSNOW (I)=0.0 
              ELSE IF (FC(I).LT.1.0E-5) THEN
                FSNOW(I)= 1.0  
              ENDIF
              IF(FCS(I).GT.0.) THEN
                FCANS(I,1)=FCANS(I,1)*FSNOW(I)/FCS(I)
                FCANS(I,2)=FCANS(I,2)*FSNOW(I)/FCS(I)
                FCANS(I,3)=FCANS(I,3)*FSNOW(I)/FCS(I)
                FCANS(I,4)=FCANS(I,4)*FSNOW(I)/FCS(I)
              ENDIF
              IF(FC(I).GT.0.) THEN
                FCAN(I,1)=FCAN(I,1)*(1.0-FSNOW(I))/FC(I)
                FCAN(I,2)=FCAN(I,2)*(1.0-FSNOW(I))/FC(I)
                FCAN(I,3)=FCAN(I,3)*(1.0-FSNOW(I))/FC(I)
                FCAN(I,4)=FCAN(I,4)*(1.0-FSNOW(I))/FC(I)
              ENDIF
              FCS(I)=MIN(FSNOW(I),1.0)
              FC(I)=1.0-FCS(I)
              FGS(I)=0.0
              FG(I)=0.0
          ENDIF
          FC (I)=MAX(FC (I),0.0)
          FG (I)=MAX(FG (I),0.0)
          FCS(I)=MAX(FCS(I),0.0)
          FGS(I)=MAX(FGS(I),0.0)
          FSUM=(FCS(I)+FGS(I)+FC(I)+FG(I))
          FC (I)=FC (I)/FSUM
          FG (I)=FG (I)/FSUM
          FCS(I)=FCS(I)/FSUM
          FGS(I)=FGS(I)/FSUM
          IF(ABS(1.0-FCS(I)-FGS(I)-FC(I)-FG(I)).GT.1.0E-5) 
     1                                   CALL XIT('APREP',-1)
C
          IF(IWF.EQ.0) THEN
              IF(ISAND(I,1).EQ.-4) THEN
                  ZPLIMG(I)=0.001
              ELSEIF(ISAND(I,1).EQ.-3) THEN
                  ZPLIMG(I)=0.001
              ELSE
                  ZPLIMG(I)=0.002
              ENDIF
              IF(FGS(I).GT.0.0) THEN
                  ZPLMGS(I)=(ZPLIMG(I)*FSNOW(I)*(1.0-FCANMX(I,1)-
     1                      FCANMX(I,2)-FCANMX(I,3)-FCANMX(I,4))+
     2                      ZPLIMG(I)*(FSNOW(I)*FCANMX(I,3)-
     3                      FCANS(I,3))+0.003*(FSNOW(I)*FCANMX(I,4)-
     4                      FCANS(I,4)))/FGS(I)
              ELSE
                  ZPLMGS(I)=0.0
              ENDIF
              IF(FC(I).GT.0.0) THEN
                  ZPLIMC(I)=(0.01*(FCAN(I,1)+FCAN(I,2))+0.003*
     1                      (FCAN(I,3)+FCAN(I,4)))/FC(I)
              ELSE
                  ZPLIMC(I)=0.0
              ENDIF
              IF(FCS(I).GT.0.0) THEN
                  ZPLMCS(I)=(0.01*(FCANS(I,1)+FCANS(I,2))+0.003*
     1                      (FCANS(I,3)+FCANS(I,4)))/FCS(I)
              ELSE
                  ZPLMCS(I)=0.0
              ENDIF
          ELSE
              ZPLMCS(I)=ZPLMS0(I)
              ZPLMGS(I)=ZPLMS0(I)
              ZPLIMC(I)=ZPLMG0(I)
              ZPLIMG(I)=ZPLMG0(I)
          ENDIF
  175 CONTINUE                                                                    
C                                                                                 
C     * PARTITION INTERCEPTED LIQUID AND FROZEN MOISTURE BETWEEN
C     * CANOPY OVERLYING BARE GROUND AND CANOPY OVERLYING SNOW,
C     * USING DIFFERENT EFFECTIVE LEAF AREAS FOR EACH.  ADD
C     * RESIDUAL TO SOIL MOISTURE OR SNOW (IF PRESENT); CALCULATE
C     * RELATIVE FRACTIONS OF LIQUID AND FROZEN INTERCEPTED 
C     * MOISTURE ON CANOPY.
C                    

      !>
      !!In loop 200, calculations are done related to the interception of water on vegetation. First, the plant area
      !!indices of the composite vegetation canopy over the snow-free and snow-covered subareas are calculated
      !!as weighted averages of the plant area indices of the four vegetation categories over each subarea. The
      !!liquid water interception capacity \f$W_{l,max}\f$ on each of the two subareas is calculated as
      !!\f$W_{l,max} = 0.20 \Lambda_p\f$
      !!where \f$\Lambda_p\f$ is the plant area index of the composite canopy. This simple relation has been found to work
      !!well for a wide range of canopy types and precipitation events (see Verseghy et al, 1993). If either the
      !!average amount of liquid water on the canopy, RCAN, or the total cancpy coverage, FC+FCS, is less than
      !!a small threshold value, the value of RCAN is stored in a residual water array RRESID, and RCAN is set
      !!to zero. Next the intercepted liquid water is partitioned between FC and FCS. First RCAN is re-
      !!evaluated as an average value over the canopy-covered area only, rather than over the whole modelled
      !!area. Then the intercepted liquid water amounts on vegetation over snow-free (RAICAN) and snow-
      !!covered areas (RAICNS) are calculated by making use of the relations
      !!\f$W_{L,0} / \Lambda_{p,0} = W_{L,s} / \Lambda_{p,s}\f$ and
      !!\f$W_l (X_0 + X_s) = W_{l,0} X_0 + W_{l,s} X_s\f$
      !!where \f$W_l\f$ is the liquid water on the canopy, \f$X\f$ is the fractional area, and the subscripts \f$0\f$ and \f$s\f$ refer to
      !!snow-free and snow-covered areas respectively.
      !!
      !!For snow interception on the canopy, a modified calculation of the plant area indices \f$\Lambda_{p,0}\f$ and \f$\Lambda_{p,s}\f$ is
      !!performed, assigning a weight of 0.7 to the plant area index of needleleaf trees, to account for the effect
      !!of needle clumping. The interception capacity for snow, \f$W_{f,max}\f$, is calculated following Bartlett et al.
      !!(2006), using a relation developed by Schmidt and Gluns (1991):
      !!\f$W_{f,max} = 6.0 \Lambda_p [0.27 + 46.0 \rho_{s,f} ]\f$
      !!where \f$\rho_{s,f} is the density of fresh snow. As was done for the intercepted liquid water, if either the average
      !!amount of snow on the canopy, SNCAN, or the total cancpy coverage is less than a small threshold value,
      !!the value of SNCAN is stored in a residual water array SRESID, and SNCAN is set to zero. Next the
      !!intercepted snow is partitioned between FC and FCS. First SNCAN is recalculated as an average over the
      !!canopy-covered area only, rather than over the whole modelled area. Then the intercepted snow amounts
      !!on vegetation over snow-free (SNOCAN) and snow-covered areas (SNOCNS) are calculated in the same
      !!way as for RAICAN and RAICNS.
      !!
      !!The fractional canopy coverages of snow and liquid water are calculated as the ratio of the intercepted
      !!snow or liquid water to their respective interception capacities. Separate values are determined for the
      !!snow-covered (FSNOCS, FRAICS) and snow-free (FSNOWC, FRAINC) subareas. If intercepted snow
      !!and liquid water are both present on the canopy, part of the liquid water cover is assumed to underlie the
      !!snow cover, so the fractional liquid water coverage is decreased by the fractional snow coverage, to yield
      !!the fractional coverage of liquid water that is exposed to the air.
      !!
      !!Next, tests are performed to ascertain whether the liquid water and snow on the canopy exceed their
      !!respective interception capacities. If so, the excess is assigned to RRESID for liquid water and SRESID
      !!for snow, and the intercepted liquid water or snow is reset to the respective interception capacity. The
      !!sum of RRESID and SRESID is added to WTRC, the diagnosed residual water transferred off the
      !!canopy, and the diagnosed change in internal energy of the canopy, HTCC, is updated. If the fractional
      !!coverage of snow on the modelled area is greater than zero, SRESID is added to the snow pack; the snow
      !!depth, mass, and temperature are recalculated, the diagnosed change in internal energy of the snow pack
      !!HTCS is updated, and SRESID is added to WTRS, the diagnosed residual water transferred to or from
      !!the snow pack. The remaining amounts of RRESID and SRESID are added to the soil. For each layer in
      !!turn whose permeable depth is greater than zero, if the sum of RRESID, SRESID and the ambient soil
      !!liquid and frozen moisture contents is less than the pore volume THPOR, RRESID is added to the liquid
      !!water content and SRESID is added to the frozen moisture content. The layer temperature is
      !!recalculated, the diagnosed change in internal energy HTC is updated, and RRESID and SRESID are
      !!added to WTRG, the diagnosed residual water transferred into or out of the soil, and are then set to zero.
      !!

      DO 200 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                     THEN                
              PAICAN(I)=(FCAN(I,1)*PAI(I,1)+FCAN(I,2)*PAI(I,2)+                   
     1                   FCAN(I,3)*PAI(I,3)+FCAN(I,4)*PAI(I,4))/FC(I)             
          ELSE                                                                    
              PAICAN(I)=0.0                                                       
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                    THEN                
              PAICNS(I)=(FCANS(I,1)*PAIS(I,1)+FCANS(I,2)*PAIS(I,2)+               
     1                   FCANS(I,3)*PAIS(I,3)+FCANS(I,4)*PAIS(I,4))/              
     2                   FCS(I)                                                   
          ELSE                                                                    
              PAICNS(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
          CWLCAP(I)=0.20*PAICAN(I)                                                
          CWLCPS(I)=0.20*PAICNS(I)                                                
C
          RRESID(I)=0.0
          IF(RCAN(I).LT.1.0E-5 .OR. (FC(I)+FCS(I)).LT.1.0E-5) THEN
              RRESID(I)=RRESID(I)+RCAN(I)
              RCAN(I)=0.0
          ENDIF
C
          IF(RCAN(I).GT.0. .AND. (FC(I)+FCS(I)).GT.0.)        THEN                
              RCAN(I)=RCAN(I)/(FC(I)+FCS(I))                                      
              IF(PAICAN(I).GT.0.0)                 THEN                           
                  RAICAN(I)=RCAN(I)*(FC(I)+FCS(I))/(FC(I)+FCS(I)*                 
     1                      PAICNS(I)/PAICAN(I))                                  
              ELSE                                                                
                  RAICAN(I)=0.0                                                   
              ENDIF                                                               
              IF(PAICNS(I).GT.0.0)                 THEN                           
                  RAICNS(I)=RCAN(I)*(FC(I)+FCS(I))/(FCS(I)+FC(I)*                 
     1                      PAICAN(I)/PAICNS(I))                                  
              ELSE                                                               
                  RAICNS(I)=0.0                                                   
              ENDIF                                                               
          ELSE                                                                    
              RAICAN(I)=0.0                                                       
              RAICNS(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
          IF(FC(I).GT.0.)                                     THEN                
              PAICAN(I)=(0.7*FCAN(I,1)*PAI(I,1)+FCAN(I,2)*PAI(I,2)+                   
     1                   FCAN(I,3)*PAI(I,3)+FCAN(I,4)*PAI(I,4))/FC(I)             
          ELSE                                                                    
              PAICAN(I)=0.0                                                       
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                    THEN                
              PAICNS(I)=(0.7*FCANS(I,1)*PAIS(I,1)+FCANS(I,2)*PAIS(I,2)+               
     1                   FCANS(I,3)*PAIS(I,3)+FCANS(I,4)*PAIS(I,4))/              
     2                   FCS(I)                                                   
          ELSE                                                                    
              PAICNS(I)=0.0                                                       
          ENDIF                                                                   
C
          CWFCAP(I)=6.0*PAICAN(I)*(0.27+46.0/RHOSNI(I))
          CWFCPS(I)=6.0*PAICNS(I)*(0.27+46.0/RHOSNI(I))
C
          SRESID(I)=0.0
          IF(SNCAN(I).LT.1.0E-5 .OR. (FC(I)+FCS(I)).LT.1.0E-5) THEN
              SRESID(I)=SRESID(I)+SNCAN(I)
              SNCAN(I)=0.0
          ENDIF
C
          IF(SNCAN(I).GT.0. .AND. (FC(I)+FCS(I)).GT.0.)        THEN                
              SNCAN(I)=SNCAN(I)/(FC(I)+FCS(I))                                      
              IF(PAICAN(I).GT.0.0)                 THEN                           
                  SNOCAN(I)=SNCAN(I)*(FC(I)+FCS(I))/(FC(I)+FCS(I)*                 
     1                      PAICNS(I)/PAICAN(I))                                  
              ELSE                                                                
                  SNOCAN(I)=0.0                                                   
              ENDIF                                                               
              IF(PAICNS(I).GT.0.0)                 THEN                           
                  SNOCNS(I)=SNCAN(I)*(FC(I)+FCS(I))/(FCS(I)+FC(I)*                 
     1                      PAICAN(I)/PAICNS(I))                                  
              ELSE                                                                
                  SNOCNS(I)=0.0                                                   
              ENDIF                                                               
          ELSE                                                                    
              SNOCAN(I)=0.0                                                       
              SNOCNS(I)=0.0                                                       
          ENDIF                                                                   
C                                                                                 
          IF(CWFCAP(I).GT.0.0)                                  THEN
              FSNOWC(I)=MIN(SNOCAN(I)/CWFCAP(I),1.0)
          ELSE
              FSNOWC(I)=0.0
          ENDIF
          IF(CWFCPS(I).GT.0.0)                                  THEN
              FSNOCS(I)=MIN(SNOCNS(I)/CWFCPS(I),1.0)
          ELSE
              FSNOCS(I)=0.0
          ENDIF
C
          IF(CWLCAP(I).GT.0.0)                                  THEN
              FRAINC(I)=MIN(RAICAN(I)/CWLCAP(I),1.0)
          ELSE
              FRAINC(I)=0.0
          ENDIF
          IF(CWLCPS(I).GT.0.0)                                  THEN
              FRAICS(I)=MIN(RAICNS(I)/CWLCPS(I),1.0)
          ELSE                                                                    
              FRAICS(I)=0.0                                                       
          ENDIF                                                                   
          FRAINC(I)=MAX(0.0,MIN(FRAINC(I)-FSNOWC(I),1.0))
          FRAICS(I)=MAX(0.0,MIN(FRAICS(I)-FSNOCS(I),1.0))
C                                                                                 
          IF(RAICAN(I).GT.CWLCAP(I))                            THEN
              RRESID(I)=RRESID(I)+FC(I)*(RAICAN(I)-CWLCAP(I))
              RAICAN(I)=CWLCAP(I)
          ENDIF
          IF(SNOCAN(I).GT.CWFCAP(I))                            THEN
              SRESID(I)=SRESID(I)+FC(I)*(SNOCAN(I)-CWFCAP(I))
              SNOCAN(I)=CWFCAP(I)
          ENDIF
C
          IF(RAICNS(I).GT.CWLCPS(I))                            THEN
              RRESID(I)=RRESID(I)+FCS(I)*(RAICNS(I)-CWLCPS(I))
              RAICNS(I)=CWLCPS(I)
          ENDIF
          IF(SNOCNS(I).GT.CWFCPS(I))                            THEN
              SRESID(I)=SRESID(I)+FCS(I)*(SNOCNS(I)-CWFCPS(I))
              SNOCNS(I)=CWFCPS(I)
          ENDIF
C
          WTRC (I)=WTRC(I)-(RRESID(I)+SRESID(I))/DELT
          HTCC (I)=HTCC(I)-TCAN(I)*(SPHW*RRESID(I)+SPHICE*SRESID(I))/
     1             DELT
          IF(FSNOW(I).GT.0.0)                      THEN                           
              SNOI=SNO(I)
              ZSNADD=SRESID(I)/(RHOSNO(I)*FSNOW(I))                               
              ZSNOW(I)=ZSNOW(I)+ZSNADD
              SNO(I)=ZSNOW(I)*FSNOW(I)*RHOSNO(I)                                  
              TSNOW(I)=(TCAN(I)*SPHICE*SRESID(I)+TSNOW(I)*HCPICE*
     1                 SNOI/RHOICE)/(HCPICE*SNO(I)/RHOICE)
              HTCS (I)=HTCS(I)+TCAN(I)*SPHICE*SRESID(I)/DELT
              WTRS (I)=WTRS(I)+SRESID(I)/DELT
              SRESID(I)=0.0
          ENDIF                                                                   
C
          DO 190 J=1,IG
              IF(DELZW(I,J).GT.0.0 .AND. (RRESID(I).GT.0.0
     1                  .OR. SRESID(I).GT.0.0))                THEN
                  THSUM=THLIQ(I,J)+THICE(I,J)+
     1                (RRESID(I)+SRESID(I))/(RHOW*DELZW(I,J))
                  IF(THSUM.LT.THPOR(I,J)) THEN
                      THICEI=THICE(I,J) 
                      THLIQI=THLIQ(I,J)
                      THICE(I,J)=THICE(I,J)+SRESID(I)/
     1                    (RHOICE*DELZW(I,J))                        
                      THLIQ(I,J)=THLIQ(I,J)+RRESID(I)/
     1                    (RHOW*DELZW(I,J))                             
                      TBAR(I,J)=(TBAR(I,J)*((DELZ(J)-DELZW(I,J))*
     1                    HCPSND+DELZW(I,J)*(THLIQI*HCPW+THICEI*
     2                    HCPICE+(1.0-THPOR(I,J))*HCPS(I,J)))+TCAN(I)*
     3                    (RRESID(I)*HCPW/RHOW+SRESID(I)*HCPICE/RHOICE))
     4                    /((DELZ(J)-DELZW(I,J))*HCPSND+DELZW(I,J)*
     5                    (HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+HCPS(I,J)*
     6                    (1.0-THPOR(I,J))))
                      HTC(I,J)=HTC(I,J)+TCAN(I)*(RRESID(I)*HCPW/RHOW+
     1                    SRESID(I)*HCPICE/RHOICE)/DELT
                      WTRG (I)=WTRG(I)+(RRESID(I)+SRESID(I))/DELT
                      RRESID(I)=0.0
                      SRESID(I)=0.0
                  ENDIF
              ENDIF
  190     CONTINUE
C
  200 CONTINUE                                                                    
C                                                                                 
C     * CALCULATION OF ROUGHNESS LENGTHS FOR HEAT AND MOMENTUM AND
C     * ZERO-PLANE DISPLACEMENT FOR CANOPY OVERLYING BARE SOIL AND
C     * CANOPY OVERLYING SNOW.
C  

!>
!!In loops 250 and 275, calculations of the displacement height and the logarithms of the roughness lengths
!!for heat and momentum are performed for the vegetated subareas. The displacement height \f$d_i\f$ and the
!!roughness length \f$z_{0,i}\f$ for the separate vegetation categories are obtained as simple ratios of the canopy
!!height H:
!!\f$d_i = 0.70 H\f$
!!\f$z_{0,i} = 0.10 H\f$
!!
!!The averaged displacement height d over the vegetated subareas is only calculated if the flag IDISP has
!!been set to 1. If IDISP = 0, this indicates that the atmospheric model is using a terrain-following
!!coordinate system, and thus the displacement height is treated as part of the "terrain". If DISP = 1, d is
!!calculated as a logarithmic average over the vegetation categories:
!!\f$X ln(d) = \Sigma [X_i ln(d_i)]\f$
!!where X is the fractional coverage of the subarea. The averaged roughness length for momentum \f$z_{0m}\f$
!!over the subarea is determined based on the assumption that averaging should be performed on the basis
!!of the drag coefficient formulation. Thus, following Delage et al. (1999), and after Mason (1988):
!!\f$X/ln^2 (z_b /z_{0m}) = \Sigma [X_i /ln^2 (z_b /z_{0i})]\f$
!!
!!The averaged roughness length for heat \f$z_{0e}\f$ over the subarea is calculated as a geometric mean over the
!!vegetation categories:
!!\f$z_{0e} z_{0mX} = \Pi ( z_{0i}^{2Xi} )\f$
!!
                                                                               
      DO 250 J=1,IC                                                               
      DO 250 I=IL1,IL2                                                            
          IF(FC(I).GT.0. .AND. H(I,J).GT.0.)                     THEN             
              IF(IDISP.EQ.1)   DISP(I)=DISP(I)+FCAN (I,J)*
     1                                 LOG(0.7*H(I,J))                     
              ZOMLNC(I)=ZOMLNC(I)+FCAN (I,J)/
     1                  ((LOG(ZBLEND(I)/(0.1*H(I,J))))**2)
              ZOELNC(I)=ZOELNC(I)*
     1                  (0.01*H(I,J)*H(I,J)/ZORAT(IC))**FCAN(I,J)
          ENDIF                                                                   
          IF(FCS(I).GT.0. .AND. HS(I,J).GT.0.)                   THEN             
              IF(IDISP.EQ.1)   DISPS(I)=DISPS (I)+FCANS(I,J)*
     1                         LOG(0.7*HS(I,J))                    
              ZOMLCS(I)=ZOMLCS(I)+FCANS(I,J)/
     1                  ((LOG(ZBLEND(I)/(0.1*HS(I,J))))**2)
              ZOELCS(I)=ZOELCS(I)*
     1                  (0.01*HS(I,J)*HS(I,J)/ZORAT(IC))**FCANS(I,J)
          ENDIF                                                                   
  250 CONTINUE                                                                    
C                                                                                 
      DO 275 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                        THEN             
              IF(IDISP.EQ.1)   DISP(I)=EXP(DISP(I)/FC(I))                                        
              ZOMLNC(I)=ZBLEND(I)/EXP(SQRT(1.0/(ZOMLNC(I)/FC(I)))) 
              ZOELNC(I)=LOG(ZOELNC(I)**(1.0/FC(I))/ZOMLNC(I))
              ZOMLNC(I)=LOG(ZOMLNC(I))
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                       THEN             
              IF(IDISP.EQ.1)   DISPS(I)=EXP(DISPS(I)/FCS(I))                                      
              ZOMLCS(I)=ZBLEND(I)/EXP(SQRT(1.0/(ZOMLCS(I)/FCS(I)))) 
              ZOELCS(I)=LOG(ZOELCS(I)**(1.0/FCS(I))/ZOMLCS(I))
              ZOMLCS(I)=LOG(ZOMLCS(I))
          ENDIF                                                                   
  275 CONTINUE                                                                    
C                                                                                 
C     * ADJUST ROUGHNESS LENGTHS OF BARE SOIL AND SNOW-COVERED BARE
C     * SOIL FOR URBAN ROUGHNESS IF PRESENT.
C        
!>
!!In loop 300, calculations of the logarithms of the roughness lengths for heat and momentum are
!!performed for the bare ground and snow-covered subareas. Background values of \f$ln(z_{om})\f$ for soil, snow
!!cover, ice sheets and urban areas are passed into the subroutine through common blocks. In CLASS,
!!urban areas are treated very simply, as areas of bare soil with a high roughness length. The subarea values
!!of \f$ln(z_{om})\f$ for bare soil and snow are therefore adjusted for the fractional coverage of urban area. Values
!!for the ratio between the roughness lengths for momentum and heat for bare soil and snow are also
!!passed in via common blocks. These are used to derive subarea values of \f$ln(z_{oe})\f$ from \f$ln(z_{om})\f$.
!!
                                                                         
      DO 300 I=IL1,IL2                                                            
          IF(FG(I).GT.0.)                                        THEN             
              IF(ISAND(I,1).NE.-4)                   THEN                         
                  ZOMLNG(I)=((FG(I)-FCANMX(I,5)*(1.0-FSNOW(I)))*ZOLNG+            
     1                      FCANMX(I,5)*(1.0-FSNOW(I))*ZOLN(I,5))/FG(I)           
              ELSE                                                                
                  ZOMLNG(I)=ZOLNI                                                 
              ENDIF                                                               
              ZOELNG(I)=ZOMLNG(I)-LOG(ZORATG)                                    
          ENDIF                                                                   
          IF(FGS(I).GT.0.)                                       THEN             
              ZOMLNS(I)=((FGS(I)-FCANMX(I,5)*FSNOW(I))*ZOLNS+                     
     1                  FCANMX(I,5)*FSNOW(I)*ZOLN(I,5))/FGS(I)                    
              ZOELNS(I)=ZOMLNS(I)-LOG(ZORATG)                                    
          ENDIF                                                                   
  300 CONTINUE                                                                    
C                                                                                 
C     * ADD CONTRIBUTION OF OROGRAPHY TO MOMENTUM ROUGHNESS LENGTH
C

!>
!!In loop 325, an adjustment is applied to \f$ln(z_{om})\f$ if the effect of terrain roughness needs to be taken into
!!account. If the surface orographic roughness length is not vanishingly small, its logarithm, LZ0ORO, is
!!calculated. If it is greater than the calculated logarithm of the roughness length for momentum of any of
!!the subareas, these are reset to LZ0ORO.
!!

      DO 325 I=IL1,IL2
          IF(Z0ORO(I).GT.1.0E-4) THEN
              LZ0ORO=LOG(Z0ORO(I))
          ELSE
              LZ0ORO=-10.0
          ENDIF
          ZOMLNC(I)=MAX(ZOMLNC(I),LZ0ORO)
          ZOMLCS(I)=MAX(ZOMLCS(I),LZ0ORO)
          ZOMLNG(I)=MAX(ZOMLNG(I),LZ0ORO)
          ZOMLNS(I)=MAX(ZOMLNS(I),LZ0ORO)
  325  CONTINUE
C     
C     * CALCULATE HEAT CAPACITY FOR CANOPY OVERLYING BARE SOIL AND
C     * CANOPY OVERLYING SNOW.
C     * ALSO CALCULATE INSTANTANEOUS GRID-CELL AVERAGED CANOPY MASS.
C          

!>
!!In loop 350, the canopy mass is calculated as a weighted average over the vegetation categories, for
!!canopy over bare soil (CMASSC) and over snow (CMASCS). (For crops over bare soil, the mass is
!!adjusted according to the growth index; for crops and grass over snow, the mass is additionally adjusted
!!to take into account burying by snow.) If IDISP = 0, indicating that the vegetation displacement height is
!!part of the "terrain", the mass of air within the displacement height is normalized by the vegetation heat
!!capacity and added to the canopy mass. If IZREF = 2, indicating that the bottom of the atmosphere is
!!taken to lie at the local roughness length rather than at the ground surface, the mass of air within the
!!roughness length is likewise normalized by the vegetation heat capacity and added to the canopy mass.
!!The canopy heat capacities over bare soil (CHCAP) and over snow (CHCAPS) are evaluated from the
!!respective values of canopy mass and of intercepted liquid water and snow. The aggregated canopy mass
!!CMAI is recalculated, and is used to determine the change in internal energy of the canopy, HTCC, owing
!!to growth or disappearance of the vegetation.
!!
                                                                       
      DO 350 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                       THEN                     
C     ---------------- CTEM MODIFICATIONS -----------------------------\

              IF (ctem_on) THEN

                CMASSC(I)=(FCAN(I,1)*CMASVEGC(I,1)+
     1                     FCAN(I,2)*CMASVEGC(I,2)+
     2                     FCAN(I,3)*CMASVEGC(I,3)+
     3                     FCAN(I,4)*CMASVEGC(I,4))/FC (I)
              ELSE
C    ----------------- CTEM MODIFICATIONS -----------------------------/
              CMASSC(I)=(FCAN(I,1)*CWGTMX(I,1)+FCAN (I,2)*CWGTMX(I,2)+                   
     1                   FCAN(I,3)*CWGTMX(I,3)*GROWA(I)+
     2                   FCAN(I,4)*CWGTMX(I,4))/FC (I)           
              ENDIF     !CTEM MODIFICATION
C
              IF(IDISP.EQ.0) THEN
                  CMASSC(I)=CMASSC(I)+RHOAIR(I)*(SPHAIR/SPHVEG)*0.7*
     1                     (FCAN(I,1)*H(I,1)+FCAN(I,2)*H(I,2)+
     2                      FCAN(I,3)*H(I,3)+FCAN(I,4)*H(I,4))/FC(I)
              ENDIF
              IF(IZREF.EQ.2) THEN
                  CMASSC(I)=CMASSC(I)+RHOAIR(I)*(SPHAIR/SPHVEG)*0.1*
     1                     (FCAN(I,1)*H(I,1)+FCAN(I,2)*H(I,2)+
     2                      FCAN(I,3)*H(I,3)+FCAN(I,4)*H(I,4))/FC(I)
              ENDIF
          ENDIF                                                                          
          IF(FCS(I).GT.0.)                                      THEN                     
C    ----------------- CTEM MODIFICATIONS -----------------------------\
              IF (ctem_on) THEN
                CMASCS(I)=FCANS(I,1)*CMASVEGC(I,1)+
     1                     FCANS(I,2)*CMASVEGC(I,2)+
     2                     FCANS(I,3)*CMASVEGC(I,3)
     3                    *HS(I,3)/MAX(H(I,3),HS(I,3))+
     4                     FCANS(I,4)*CMASVEGC(I,4)
     5                    *HS(I,4)/MAX(H(I,4),HS(I,4))/FCS(I)
              ELSE
C    ----------------- CTEM MODIFICATIONS -----------------------------/
              CMASCS(I)=(FCANS(I,1)*CWGTMX(I,1)+FCANS(I,2)*CWGTMX(I,2)+                  
     1                   FCANS(I,3)*CWGTMX(I,3)*GROWA(I)
     2                  *HS(I,3)/MAX(H(I,3),HS(I,3))+                            
     3                   FCANS(I,4)*CWGTMX(I,4)                         
     4                  *HS(I,4)/MAX(H(I,4),HS(I,4)))/FCS(I)                     
              ENDIF   ! CTEM MODIFICATION
C
              IF(IDISP.EQ.0) THEN
                  CMASCS(I)=CMASCS(I)+RHOAIR(I)*(SPHAIR/SPHVEG)*0.7*
     1                      (FCANS(I,1)*HS(I,1)+FCANS(I,2)*HS(I,2)+
     2                       FCANS(I,3)*HS(I,3)+FCANS(I,4)*HS(I,4))/
     3                       FCS(I)
              ENDIF
              IF(IZREF.EQ.2) THEN
                  CMASCS(I)=CMASCS(I)+RHOAIR(I)*(SPHAIR/SPHVEG)*0.1*
     1                      (FCANS(I,1)*HS(I,1)+FCANS(I,2)*HS(I,2)+
     2                       FCANS(I,3)*HS(I,3)+FCANS(I,4)*HS(I,4))/
     3                       FCS(I)
              ENDIF
          ENDIF             
                                                      
          CHCAP (I)=SPHVEG*CMASSC(I)+SPHW*RAICAN(I)+SPHICE*SNOCAN(I)              
          CHCAPS(I)=SPHVEG*CMASCS(I)+SPHW*RAICNS(I)+SPHICE*SNOCNS(I)              
          HTCC  (I)=HTCC(I)-SPHVEG*CMAI(I)*TCAN(I)/DELT
C     ---------------- CTEM MODIFICATIONS -----------------------------\

C         THIS, BELOW, WAS MAKING IT SO THAT OUR READ-IN TCAN WAS BEING
C         OVERWRITTEN BY TA FOR THE FIRST TIME STEP. JM JAN 2013
          IF (ctem_on) THEN

            CMAI  (I)=FC(I)*CMASSC(I)+FCS(I)*CMASCS(I)
            IF(CMAI(I).LT.1.0E-5 .AND. (CMASSC(I).GT.0.0 .OR.
     1              CMASCS(I).GT.0.0)) TCAN(I)=TA(I)
          ELSE
          IF(CMAI(I).LT.1.0E-5 .AND. (CMASSC(I).GT.0.0 .OR.
     1              CMASCS(I).GT.0.0)) TCAN(I)=TA(I)
          CMAI  (I)=FC(I)*CMASSC(I)+FCS(I)*CMASCS(I)
          ENDIF 
C    ----------------- CTEM MODIFICATIONS -----------------------------/
          HTCC  (I)=HTCC(I)+SPHVEG*CMAI(I)*TCAN(I)/DELT
          RBCOEF(I)=0.0
  350 CONTINUE                                                                    
C                                                                                 
C     * CALCULATE VEGETATION ROOTING DEPTH AND FRACTION OF ROOTS 
C     * IN EACH SOIL LAYER (SAME FOR SNOW/BARE SOIL CASES).
C     * ALSO CALCULATE LEAF BOUNDARY RESISTANCE PARAMETER RBCOEF.
C                

!>
!!In the 450 and 500 loops, the fraction of plant roots in each soil layer is calculated. If CLASS is being run
!!coupled to CTEM, the CTEM-derived values are assigned. Otherwise, for each vegetation category the
!!rooting depth ZROOT is set to the background maximum value, except in the case of crops, for which it
!!is set to the maximum scaled by GROWA. If the soil permeable depth is less than ZROOT, ZROOT is
!!set to this depth instead. Values are then assigned in the matrix RMAT, which stores the fraction of roots
!!in each vegetation category for each soil layer. According to Feddes et al. (1974), the fractional root
!!volume R(z) below a depth z is well represented for many varieties of plants by the following exponential
!!function:
!!\f$R(z) = a_1 exp(-3.0z) + a_2.\f$
!!
!!Making use of the boundary conditions \f$R(0) = 1\f$ and \f$R(z_r ) = 0\f$, where \f$z_r\f$ is the rooting depth ZROOT, it
!!can be seen that the fraction of roots within a soil depth interval \f$\Delta z\f$ can be obtained as the difference
!!between R(z) evaluated at the top \f$(z_T)\f$ and bottom \f$(z_B)\f$ of the interval:
!!\f$R(\Delta z) = [exp(-3.0z_T) - exp(-3.0z_B)]/ [1 - exp(-3.0z_r)]\f$
!!
!!The total fraction of roots in each soil layer, FROOT, can then be determined as a weighted average over
!!the four vegetation categories.
!!
!!In loop 450, a leaf boundary resistance parameter \f$C_{rb}\f$ , incorporating the plant area indices of the four
!!vegetation subareas, is also calculated for later use in subroutine TSOLVC:
!!\f$C_{rb} = C_l \Lambda_{p,i}^{0.5} /0.75 \bullet [1 - exp(-0.75 \Lambda_{p,i}^{0.5} )]\f$
!!where \f$C_l\f$ is a parameter that varies with the vegetation category. The aggregated value of \f$C_{rb}\f$ is obtained
!!as a weighted average over the four vegetation categories over bare ground and snow cover.
!!                                                                 
      DO 450 J=1,IC                                                               
      DO 450 I=IL1,IL2                                                            
        IF (ctem_on) THEN
          RMAT(I,J,1)=RMATC(I,J,1)
          RMAT(I,J,2)=RMATC(I,J,2)
          RMAT(I,J,3)=RMATC(I,J,3)
        ELSE
          ZROOT=ZRTMAX(I,J)
          IF(J.EQ.3) ZROOT=ZRTMAX(I,J)*GROWA(I)                                   
          ZROOTG=0.0
          DO 375 K=1,IG
              ZROOTG=ZROOTG+DELZW(I,K)
375       CONTINUE
          ZROOT=MIN(ZROOT,ZROOTG)
          DO 400 K=1,IG
              IF(ZROOT.LE.(ZBOTW(I,K)-DELZW(I,K)+0.0001))          THEN
                  RMAT(I,J,K)=0.0
              ELSEIF(ZROOT.LE.ZBOTW(I,K))                          THEN             
                  RMAT(I,J,K)=(EXP(-3.0*(ZBOTW(I,K)-DELZW(I,K)))-
     1                EXP(-3.0*ZROOT))/(1.0-EXP(-3.0*ZROOT))
              ELSE                                                                    
                  RMAT(I,J,K)=(EXP(-3.0*(ZBOTW(I,K)-DELZW(I,K)))-
     1                EXP(-3.0*ZBOTW(I,K)))/(1.0-EXP(-3.0*ZROOT))
              ENDIF
400       CONTINUE
        ENDIF
C
        IF((FC(I)+FCS(I)).GT.0.)                               THEN             
            RBCOEF(I)=RBCOEF(I)+
     1                (FCAN(I,J)*XLEAF(J)*(SQRT(PAI(I,J))/0.75)*
     2                (1.0-EXP(-0.75*SQRT(PAI(I,J))))+
     3                FCANS(I,J)*XLEAF(J)*(SQRT(PAIS(I,J))/0.75)*
     4                (1.0-EXP(-0.75*SQRT(PAIS(I,J)))))/
     5                (FC(I)+FCS(I))    
        ENDIF               
  450 CONTINUE                                                                    
C                                                                                 
      DO 500 J=1,IG                                                               
      DO 500 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                               THEN             
              FROOT(I,J)=(FCAN(I,1)*RMAT(I,1,J) +                    
     1                    FCAN(I,2)*RMAT(I,2,J) +                    
     2                    FCAN(I,3)*RMAT(I,3,J) +                    
     3                    FCAN(I,4)*RMAT(I,4,J))/FC(I)                    
          ELSE                                                                    
              FROOT(I,J)=0.0                                                      
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                              THEN             
              FROOTS(I,J)=(FCANS(I,1)*RMAT(I,1,J) +                    
     1                     FCANS(I,2)*RMAT(I,2,J) +                    
     2                     FCANS(I,3)*RMAT(I,3,J) +                    
     3                     FCANS(I,4)*RMAT(I,4,J))/FCS(I)    
          ELSE                                                                    
              FROOTS(I,J)=0.0                                                      
          ENDIF                                                                   
  500 CONTINUE                                                                    
C                                                                                 
C     * CALCULATE SKY-VIEW FACTORS FOR BARE GROUND AND SNOW 
C     * UNDERLYING CANOPY.                                                         
C            

!>
!!In loop 600, the sky view factor \f$\chi\f$ of the ground underlying the canopy is calculated for the vegetated
!!subareas. The standard approach is to determine \f$\chi\f$ as an exponential function of the plant area index \f$\Lambda_p\f$ :
!!\f$\chi = exp[-c \Lambda_p ]\f$
!!where c is a constant depending on the vegetation category. The subarea values of \f$\chi\f$ are obtained as
!!weighted averages over the four vegetation categories.
!!
                                                                     
      DO 600 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                        THEN             
              FSVF (I)=(FCAN (I,1)*EXP(CANEXT(1)*PAI (I,1)) +                          
     1                  FCAN (I,2)*EXP(CANEXT(2)*PAI (I,2)) +                          
     2                  FCAN (I,3)*EXP(CANEXT(3)*PAI (I,3)) +                          
     3                  FCAN (I,4)*EXP(CANEXT(4)*PAI (I,4)))/FC (I)                    
          ELSE                                                                    
              FSVF (I)=0.                                                         
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                       THEN             
              FSVFS(I)=(FCANS(I,1)*EXP(CANEXT(1)*PAIS(I,1)) +                          
     1                  FCANS(I,2)*EXP(CANEXT(2)*PAIS(I,2)) +                          
     2                  FCANS(I,3)*EXP(CANEXT(3)*PAIS(I,3)) +                          
     3                  FCANS(I,4)*EXP(CANEXT(4)*PAIS(I,4)))/FCS(I)                    
          ELSE                                                                    
              FSVFS(I)=0.                                                         
          ENDIF                                                                   
  600 CONTINUE                                       
C                                                                                  
C     * CALCULATE BULK SOIL MOISTURE SUCTION FOR STOMATAL RESISTANCE.
C     * CALCULATE FRACTIONAL TRANSPIRATION EXTRACTED FROM SOIL LAYERS.
C

!>
!!In the 650 loop, the fraction of the total transpiration of water by plants that is extracted from each soil
!!layer is determined. This is done by weighting the values of FROOT calculated above by the relative soil
!!moisture suction in each layer: \f$( \Psi_w - \Psi_i )/( \Psi_w - \Psi_{sat} )\f$
!!where \f$\Psi_i\f$ , the soil moisture suction in the layer, is obtained as
!!\f$\Psi_i = \Psi_{sat} ( \theta_{l,i} / \theta_p )^{-b}\f$
!!In these equations \f$\Psi_w\f$ is the soil moisture suction at the wilting point, \f$\Psi_{sat}\f$ is the suction at 
!!saturation, \f$\theta_{l,i}\f$ is the volumetric liquid water content of the soil layer, \f$\theta_p\f$ is the pore 
!!volume, and b is an empirical parameter developed by Clapp and Hornberger (1978). The layer values of FROOT are then 
!!re-normalized so that their sum adds up to unity. In this loop, the representative soil moisture suction PSIGND is also
!!calculated for later use in the vegetation stomatal resistance formulation, as the minimum value of \f$\Psi_i\f$ and
!!\f$\Psi_w\f$ over all the soil layers.
!!

      DO 650 J=1,IG                                                               
      DO 650 I=IL1,IL2                                                            
          IF(FCS(I).GT.0.0 .OR. FC(I).GT.0.0)                      THEN          
              IF(THLIQ(I,J).GT.(THLMIN(I,J)+0.01))          THEN
                  PSII=PSISAT(I,J)*(THLIQ(I,J)/THPOR(I,J))**(-BI(I,J))
                  PSII=MIN(PSII,PSIWLT(I,J))
                  IF(FROOT(I,J).GT.0.0) PSIGND(I)=MIN(PSIGND(I),PSII)
                  PSIRAT=(PSIWLT(I,J)-PSII)/(PSIWLT(I,J)-PSISAT(I,J))          
                  FROOT(I,J)=FROOT(I,J)*PSIRAT
                  FROOTS(I,J)=FROOTS(I,J)*PSIRAT
                  FRTOT(I)=FRTOT(I)+FROOT(I,J)                                    
                  FRTOTS(I)=FRTOTS(I)+FROOTS(I,J)                                    
              ELSE
                  FROOT(I,J)=0.0
                  FROOTS(I,J)=0.0
              ENDIF                                                               
          ENDIF                                                                   
  650 CONTINUE                                                                    
C                                                                                 
      DO 700 J=1,IG                                                               
      DO 700 I=IL1,IL2                                                            
          IF(FRTOT(I).GT.0.)                                       THEN           
              FROOT(I,J)=FROOT(I,J)/FRTOT(I)                                      
          ENDIF                                                                   
          IF(FRTOTS(I).GT.0.)                                      THEN           
              FROOTS(I,J)=FROOTS(I,J)/FRTOTS(I)                                      
          ENDIF                                                                   
  700 CONTINUE                                                                    
C 
C     * CALCULATE EFFECTIVE LEAF AREA INDICES FOR TRANSPIRATION.
C

!>
!!Finally, in loop 800 the aggregated canopy plant area indices PAICAN and PAICNS are set back to their
!!original values, from the modified values used for the snow interception calculations above; and if CLASS
!!is being run coupled with CTEM, a set of CTEM-related calculations is performed.
!!

      DO 800 I=IL1,IL2                                                            
          IF(FC(I).GT.0.)                                     THEN                
              PAICAN(I)=(FCAN(I,1)*PAI(I,1)+FCAN(I,2)*PAI(I,2)+                   
     1                   FCAN(I,3)*PAI(I,3)+FCAN(I,4)*PAI(I,4))/FC(I)             
          ELSE                                                                    
              PAICAN(I)=0.0                                                       
          ENDIF                                                                   
          IF(FCS(I).GT.0.)                                    THEN                
              PAICNS(I)=(FCANS(I,1)*PAIS(I,1)+FCANS(I,2)*PAIS(I,2)+               
     1                   FCANS(I,3)*PAIS(I,3)+FCANS(I,4)*PAIS(I,4))/              
     2                   FCS(I)                                                   
          ELSE                                                                    
              PAICNS(I)=0.0                                                       
          ENDIF                                                                   
  800 CONTINUE
C
      IF (ctem_on) THEN
C
C       * ESTIMATE FCANC AND FCANCS FOR USE BY PHTSYN SUBROUTINE BASED ON
C       * FCAN AND FCANS FOR CTEM PFTS.
C
        DO 810 J = 1, IC
        DO 810 I = IL1, IL2
          SFCANCMX(I,J)=0.0  !< SUM OF FCANCMXS
  810   CONTINUE
C
        K1=0
        DO 830 J = 1, IC
          IF(J.EQ.1) THEN
            K1 = K1 + 1
          ELSE
            K1 = K1 + NOL2PFTS(J-1)
          ENDIF
          K2 = K1 + NOL2PFTS(J) - 1
          DO 820 M = K1, K2
          DO 820 I = IL1, IL2
              SFCANCMX(I,J)=SFCANCMX(I,J)+FCANCMX(I,M)
  820     CONTINUE
  830   CONTINUE
C
        K1=0
        DO 860 J = 1, IC
          IF(J.EQ.1) THEN
            K1 = K1 + 1
          ELSE
            K1 = K1 + NOL2PFTS(J-1)
          ENDIF
          K2 = K1 + NOL2PFTS(J) - 1
          DO 850 M = K1, K2
          DO 850 I = IL1, IL2
             IF(SFCANCMX(I,J).GT.1.E-20) THEN
               FCANC(I,M)  = FCAN(I,J) * (FCANCMX(I,M)/SFCANCMX(I,J))
               FCANCS(I,M) = FCANS(I,J)* (FCANCMX(I,M)/SFCANCMX(I,J))
             ELSE
               FCANC(I,M)  = 0.0
               FCANCS(I,M) = 0.0
             ENDIF
  850     CONTINUE
  860   CONTINUE
      ENDIF
C                                                                                 
      RETURN
      END
