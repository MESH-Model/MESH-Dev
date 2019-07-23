!>\file
!!Purpose: Solution of surface energy balance for vegetated subareas.
!!
      SUBROUTINE TSOLVC(ISNOW,FI,
     1                 QSWNET,QSWNC,QSWNG,QLWOUT,QLWOC,QLWOG,QTRANS,
     2                 QSENS,QSENSC,QSENSG,QEVAP,QEVAPC,QEVAPG,EVAPC,
     3                 EVAPG,EVAP,TCAN,QCAN,TZERO,QZERO,GZERO,QMELTC,
     4                 QMELTG,RAICAN,SNOCAN,CDH,CDM,RIB,TAC,QAC,
     5                 CFLUX,FTEMP,FVAP,ILMO,UE,H,QFCF,QFCL,HTCC,
     6                 QSWINV,QSWINI,QLWIN,TPOTA,TA,QA,VA,VAC,PADRY,
     7                 RHOAIR,ALVISC,ALNIRC,ALVISG,ALNIRG,TRVISC,TRNIRC,
     7                 FSVF,CRIB,CPHCHC,CPHCHG,CEVAP,TADP,TVIRTA,RC,
     8                 RBCOEF,ZOSCLH,ZOSCLM,ZRSLFH,ZRSLFM,ZOH,ZOM,
     A                 FCOR,GCONST,GCOEFF,TGND,TRSNOW,FSNOWC,FRAINC,
     B                 CHCAP,CMASS,PCPR,FROOT,THLMIN,DELZW,RHOSNO,ZSNOW,
     +                 IWATER,IEVAP,ITERCT,  
     C                 ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,N,
     D                 TSTEP,TVIRTC,TVIRTG,EVBETA,XEVAP,EVPWET,Q0SAT,
     E                 RA,RB,RAGINV,RBINV,RBTINV,RBCINV,TVRTAC,
     F                 TPOTG,RESID,
     G                 TCANO,WZERO,XEVAPM,DCFLXM,WC,DRAGIN,CFLUXM,CFLX,
     H                 IEVAPC,TRTOP,QSTOR,CFSENS,CFEVAP,QSGADD,A,B,
     I                 LZZ0,LZZ0T,FM,FH,ITER,NITER,KF1,KF2,
     J                 AILCG,FCANC,CO2CONC,RMATCTEM,
     K                 THLIQ,THFC,THLW,ISAND,IG,COSZS,PRESSG,
     L                 XDIFFUS,ICTEM,IC,CO2I1,CO2I2,
     M                 ctem_on,SLAI,FCANCMX,L2MAX,
     N                 NOL2PFTS,CFLUXV,ANVEG,RMLVEG, LFSTATUS,
     O                 DAYL,DAYL_MAX,
     P                 CTEMN, XNUP_VEG, NRUB0, VCMAX0)
C
C     * AUG 30/16 - J.Melton    Replace ICTEMMOD with ctem_on (logical switch).
C     * JUL 22/15 - D.VERSEGHY. LIMIT CALCULATED EVAPORATION RATES
C     *                         ACCORDING TO WATER AVAILABILITY.
C     * FEB 27/15 - J. MELTON - WILTSM AND FIELDSM ARE RENAMED THLW AND THFC, RESPECTIVELY.
C     * JUN 27/14 - D.VERSEGHY. CHANGE ITERATION LIMIT BACK TO 50 FOR
C     *                         BISECTION SCHEME; BUGFIX IN CALCULATION
C     *                         OF EVPWET.
C     * OCT 30/12 - V. ARORA  - CFLUXV WAS BEING INITIALIZED TO ZERO INAPPROPRIATELY
C     *                         FOR MOSAIC RUNS. NOT A PROBLEM WITH COMPOSITE
C     *                         RUNS. CREATED A TEMPORARY STORAGE VAR TO ALLOW
C     *                         AN APPROPRIATE VALUE FOR THE INITIALIZATION
C     * SEP 05/12 - J.MELTON. - MADE AN IMPLICIT INT TO REAL CONVERSION
C     *                         EXPLICIT. ALSO PROBLEM WITH TAC, SEE NOTE
C     *                         IN THE CODE BEFORE CALL TO PHTSYN.
C     * NOV 11/11 - M.LAZARE. - INCORPORATES CTEM. THIS INVOLVES
C     *                         SEVERAL CHANGES AND NEW OUTPUT ROUTINES.
C     *                         QSWNVC IS PROMOTED TO A WORK ARRAY
C     *                         SINCE PASSED AS INPUT TO THE NEW CALLED
C     *                         PHOTOSYNTHESIS ROUTINE "PHTSYN3". THE
C     *                         CTEM CANOPY RESISTANCE COMING OUT OF
C     *                         THIS ROUTINE, "RCPHTSYN" IS STORED INTO
C     *                         THE USUAL "RC" ARRAY AS LONG AS THE
C     *                         BONE-DRY SOIL FLAG IS NOT SET (RC=1.E20).
C     *                         WE ALSO HAVE TO PASS "TA" THROUGH FROM
C     *                         CLASST. FINALLY, "ISAND", "FIELDSM" AND
C     *                         "WILTSM" ARE PASSED THROUGH TO PHTSYN3
C     *                         FOR CTEM.
C     * OCT 14/11 - D.VERSEGHY. FOR POST-ITERATION CLEANUP WITH N-R SCHEME,
C     *                         REMOVE CONDITION INVOLVING LAST ITERATION
C     *                         TEMPERATURE.
C     * DEC 07/09 - D.VERSEGHY. RESTORE EVAPOTRANSPIRATION WHEN
C     *                         PRECIPITATION IS OCCURRING; ADD EVAPC
C     *                         TO EVAP WHEN DEPOSITION OF WATER ON
C     *                         CANOPY IS OCCURRING.
C     * MAR 13/09 - D.VERSEGHY. REPLACE COMMON BLOCK SURFCON WITH CLASSD2;
C     *                         REVISED CALL TO FLXSURFZ.
C     * JAN 20/09 - D.VERSEGHY. CORRECT CALCULATION OF TPOTG.
C     * JAN 06/09 - E.CHAN/D.VERSEGHY. SET UPPER LIMIT FOR TSTEP IN
C     *                         N-R ITERATION SCHEME.
C     * FEB 26/08 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS; REMOVE
C     *                         "ILW" SWITCH; SUPPRESS WATER VAPOUR FLUX
C     *                         IF PRECIPITATION IS OCCURRING.
C     * FEB 19/07 - D.VERSEGHY. UPDATE CANOPY WATER STORES IN THIS
C     *                         ROUTINE INSTEAD OF CANADD FOR CASES
C     *                         OF WATER DEPOSITION.
C     * MAY 17/06 - D.VERSEGHY. ADD IL1 AND IL2 TO CALL TO FLXSURFZ;
C     *                         REMOVE JL FROM CALL TO DRCOEF.
C     * APR 15/05 - D.VERSEGHY. SUBLIMATION OF INTERCEPTED SNOW TAKES
C     *                         PLACE BEFORE EVAPORATION OF INTERCEPTED
C     *                         RAIN.
C     * APR 14/05 - Y.DELAGE.   REFINEMENTS TO N-R ITERATION SCHEME.
C     * FEB 23/05 - D.VERSEGHY. INCORPORATE A SWITCH TO USE EITHER THE
C     *                         BISECTION ITERATION SCHEME WITH CANOPY
C     *                         AIR PARAMETRIZATION, OR THE NEWTON-
C     *                         RAPHSON ITERATION SCHEME WITH MODIFIED
C     *                         ZOH.
C     * JAN 31/05 - Y.DELAGE.   USE THE CANOPY AIR RESISTANCE TO CALCULATE A
C     *                         ROUGHNESS LENGTH FOR TEMPERATURE AND HUMIDITY.
C     *                         REPLACE SECANT METHOD BY NEWTON-RAPHSON SCHEME
C     *                         FOR BOTH ITERATION LOOPS.
C     *                         LIMIT NUMBER OF ITERATIONS (ITERMX) TO 5 AND
C     *                         APPLY CORRECTIONS IF RESIDUE REMAINS.
C     * JAN 12/05 - P.BARTLETT/D.VERSEGHY. MODIFICATION TO CALCULATION
C     *                         OF RBINV; ALLOW SUBLIMATION OF FROZEN
C     *                         WATER ONLY ONTO SNOW-COVERED PORTION
C     *                         OF CANOPY.
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 06/04 - Y.DELAGE/D.VERSEGHY. PROTECT SENSITIVE CALCULATIONS
C     *                         FROM ROUNDOFF ERRORS.
C     * NOV 07/02 - Y.DELAGE/D.VERSEGHY. NEW CALL TO FLXSURFZ; VIRTUAL
C     *                         AND POTENTIAL TEMPERATURE CORRECTIONS.
C     * NOV 01/02 - P.BARTLETT. MODIFICATIONS TO CALCULATIONS OF QAC
C     *                         AND RB.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * MAR 28/02 - D.VERSEGHY. STREAMLINED SUBROUTINE CALL.
C     * MAR 10/02 - M.LAZARE.   VECTORIZE LOOP 650 BY SPLITTING INTO TWO.
C     * JAN 18/02 - P.BARTLETT/D.VERSEGHY. NEW "BETA" FORMULATION FOR
C     *                         BARE SOIL EVAPORATION BASED ON LEE AND
C     *                         PIELKE.
C     * APR 11/01 - M.LAZARE.   SHORTENED "CLASS2" COMMON BLOCK.
C     * OCT 06/00 - D.VERSEGHY. CONDITIONAL "IF" IN ITERATION SEQUENCE
C     *                         TO AVOID DIVIDE BY ZERO.
C     * DEC 16/99 - A.WU/D.VERSEGHY. REVISED CANOPY TURBULENT FLUX
C     *                              FORMULATION: ADD PARAMETRIZATION
C     *                              OF CANOPY AIR TEMPERATURE.
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
C     *                         NOW DETERMINED IN ROUTINE "DRCOEF".
C     * OCT 04/94 - D.VERSEGHY. CHANGE "CALL ABORT" TO "CALL XIT" TO
C     *                         ENABLE RUNNING ON PCS.
C     * JAN 24/94 - M.LAZARE.   UNFORMATTED I/O COMMENTED OUT IN LOOPS
C     *                         200 AND 600.
C     * JUL 29/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         ADD TRANSMISSION THROUGH SNOWPACK TO
C     *                         "QSWNET" FOR DIAGNOSTIC PURPOSES.
C     * OCT 15/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. ITERATIVE TEMPERATURE CALCULATIONS
C     *                         FOR VEGETATION CANOPY AND UNDERLYING
C     *                         SURFACE.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      !input variable
      INTEGER ISNOW !<Flag indicating presence or absence of snow
      INTEGER ISLFD,ITC,ITCG,ILG,IL1,IL2,JL,I,J,N,KK    
C
      INTEGER NUMIT,IBAD,NIT,ITERMX
C
C     * OUTPUT ARRAYS.
C
      REAL QSWNET(ILG) !<Total net shortwave radiation of canopy and underlying surface \f$[W m^{-2} ]\f$
      REAL QSWNC (ILG) !<Net shortwave radiation on vegetation canopy \f$[W m^{-2} ] (K_{*c} )\f$
      REAL QSWNG (ILG) !<Net shortwave radiation at underlying surface \f$[W m^{-2} ] (K_{*g} )\f$
      REAL QLWOUT(ILG) !<Upwelling longwave radiation from canopy and underlying surface \f$[W m^{-2} ]\f$
      REAL QLWOC (ILG) !<Upwelling longwave radiation from vegetation canopy \f$[W m^{-2} ] (L \uparrow_c , L \downarrow_c )\f$
      REAL QLWOG (ILG) !<Upwelling longwave radiation from underlying surface \f$[W m^{-2} ] (L \uparrow_g )\f$
      REAL QTRANS(ILG) !<Shortwave radiation transmitted into surface \f$[W m^{-2} ] \f$
      REAL QSENS (ILG) !<Sensible heat flux from canopy and underlying surface \f$[W m^{-2} ] (Q_H )\f$
      REAL QSENSC(ILG) !<Sensible heat flux from vegetation canopy \f$[W m^{-2} ] (Q_{H,c} )\f$
      REAL QSENSG(ILG) !<Sensible heat flux from underlying surface \f$[W m^{-2} ] (Q_{H,g} )\f$
      REAL QEVAP (ILG) !<Latent heat flux from canopy and underlying surface \f$[W m^{-2} ] (Q_E )\f$
      REAL QEVAPC(ILG) !<Latent heat flux from vegetation canopy \f$[W m^{-2} ] (Q_{E,c} )\f$
      REAL QEVAPG(ILG) !<Latent heat flux from underlying surface \f$[W m^{-2} ] (Q_{E,g} )\f$
      REAL EVAPC (ILG) !<Evaporation rate from vegetation \f$[kg m^{-2} s^{-1} ] (E_c )\f$
      REAL EVAPG (ILG) !<Evaporation rate from underlying surface \f$[kg m^{-2} s^{-1} ] (E(0))\f$
      REAL TCAN  (ILG) !<Vegetation canopy temperature \f$[K] (T_c )\f$
      REAL QCAN  (ILG) !<Saturated specific humidity at canopy temperature \f$[kg kg^{-1} ] (q_c )\f$
      REAL TZERO (ILG) !<Temperature at surface \f$[K] (T(0))\f$
      REAL QZERO (ILG) !<Specific humidity at surface \f$[kg kg^{-1} ] (q(0))\f$
      REAL GZERO (ILG) !<Heat flux into surface \f$[W m^{-2} ] (G(0))\f$
      REAL QMELTC(ILG) !<Heat available for melting snow or freezing water on the vegetation \f$[W m^{-2} ]\f$ 
      REAL QMELTG(ILG) !<Heat available for melting snow or freezing water on the underlying surface \f$[W m^{-2} ]\f$
      REAL RAICAN(ILG) !<Intercepted liquid water stored on vegetation canopy \f$[kg m^{-2} ]\f$
      REAL SNOCAN(ILG) !<Intercepted frozen water stored on vegetation canopy \f$[kg m^{-2} ]\f$
      REAL CDH   (ILG) !<Surface drag coefficient for heat [ ]
      REAL CDM   (ILG) !<Surface drag coefficient for momentum [ ]
      REAL RIB   (ILG) !<Bulk Richardson number at surface [ ]
      REAL TAC   (ILG) !<Temperature of air within vegetation canopy \f$[K] (T_{ac} )\f$
      REAL QAC   (ILG) !<Specific humidity of air within vegetation canopy space \f$[kg kg^{-1} ] (q_{ac} )\f$
      REAL CFLUX (ILG) !<Product of surface drag coefficient and wind speed \f$[m s^{-1} ]\f$
      REAL FTEMP (ILG) !<Product of surface-air temperature gradient, 
                       !<drag coefficient and wind speed \f$[K m s^{-1} ]\f$
      REAL FVAP  (ILG) !<Product of surface-air humidity gradient, drag coefficient 
                       !<and wind speed \f$[kg kg^{-1} m s^{-1} ]\f$
      REAL ILMO  (ILG) !<Inverse of Monin-Obukhov roughness length \f$(m^{-1} ]\f$
      REAL UE    (ILG) !<Friction velocity of air \f$[m s^{-1} ]\f$
      REAL H     (ILG) !<Height of the atmospheric boundary layer [m]
      REAL QFCF  (ILG) !<Sublimation from frozen water on vegetation \f$[kg m^{-2} s^{-1} ]\f$
      REAL QFCL  (ILG) !<Evaporation from liquid water on vegetation \f$[kg m^{-2} s^{-1} ]\f$
      REAL HTCC  (ILG) !<Internal energy change of canopy due to changes in temperature and/or mass \f$[W m^{-2} ]\f$ 
      REAL EVAP  (ILG) !<Diagnosed total surface water vapour flux over modelled area \f$[kg m^{-2} s^{-1} ]\f$
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG) !<Fractional coverage of subarea in question on modelled area [ ]
      REAL QSWINV(ILG) !<Visible radiation incident on horizontal surface \f$[W m^{-2} ] (K \downarrow )\f$
      REAL QSWINI(ILG) !<Near-infrared radiation incident on horizontal surface \f$[W m^{-2} ] (K \downarrow )\f$
      REAL QLWIN (ILG) !<Downwelling longwave radiation at bottom of atmosphere \f$[W m^{-2} ] (L \downarrow )\f$
      REAL TPOTA (ILG) !<Potential temperature of air at reference height \f$[K] (T_{a,pot} )\f$
      REAL TA    (ILG) !<Air temperature at reference height [K]
      REAL QA    (ILG) !<Specific humidity at reference height \f$[kg kg^{-1} ] (q_a )\f$
      REAL VA    (ILG) !<Wind speed at reference height \f$[m s^{-1} ]\f$
      REAL VAC   (ILG) !<Wind speed within vegetation canopy space \f$[m s^{-1} ] (v_{ac} )\f$
      REAL PADRY (ILG) !<Partial pressure of dry air \f$[Pa] (p_{dry} )\f$
      REAL RHOAIR(ILG) !<Density of air \f$[kg m^{-3} ] ( \rho_a )\f$
      REAL ALVISC(ILG) !<Visible albedo of vegetation canopy \f$[ ] ( \alpha_c )\f$
      REAL ALNIRC(ILG) !<Near-IR albedo of vegetation canopy \f$[ ] ( \alpha_c )\f$
      REAL ALVISG(ILG) !<Visible albedo of underlying surface \f$[ ] ( \alpha_g )\f$
      REAL ALNIRG(ILG) !<Near-IR albedo of underlying surface \f$[ ] ( \alpha_g )\f$
      REAL TRVISC(ILG) !<Visible transmissivity of vegetation canopy \f$[ ] ( \tau_c )\f$
      REAL TRNIRC(ILG) !<Near-IR transmissivity of vegetation canopy \f$[ ] ( \tau_c )\f$
      REAL FSVF  (ILG) !<Sky view factor of ground underlying canopy \f$[ ] ( \chi )\f$
      REAL CRIB  (ILG) !<Richardson number coefficient \f$[K^{-1} ]\f$
      REAL CPHCHC(ILG) !<Latent heat of vaporization on vegetation canopy \f$[J kg^{-1} ] (L_v )\f$
      REAL CPHCHG(ILG) !<Latent heat of vaporization on underlying ground \f$[J kg^{-1} ] (L_v )\f$
      REAL CEVAP (ILG) !<Soil evaporation efficiency coefficient [ ]
      REAL TADP  (ILG) !<Dew point of air at reference height [K]
      REAL TVIRTA(ILG) !<Virtual potential temperature of air at reference height \f$[K] (T_{a,v} )\f$
      REAL RC    (ILG) !<Stomatal resistance of vegetation \f$[s m^{-1}] (r_c )\f$
      REAL RBCOEF(ILG) !<Parameter for calculation of leaf boundary resistance
      REAL ZOSCLH(ILG) !<Ratio of roughness length for heat to reference height
                       !<for temperature and humidity [ ]
      REAL ZOSCLM(ILG) !<Ratio of roughness length for momentum to reference height for wind speed [ ]
      REAL ZRSLFH(ILG) !<Difference between reference height for temperature and humidity and 
                       !<height at which extrapolated wind speed goes to zero [m]
      REAL ZRSLFM(ILG) !<Difference between reference height for wind speed and height at which 
                       !<extrapolated wind speed goes to zero [m]
      REAL ZOH   (ILG) !<Surface roughness length for heat [m]
      REAL ZOM   (ILG) !<Surface roughness length for momentum \f$[m] (z_{0,m} )\f$
      REAL FCOR  (ILG) !<Coriolis parameter \f$[s_{-1} ]\f$
      REAL GCONST(ILG) !<Intercept used in equation relating surface heat flux
                       !<to surface temperature \f$[W m^{-2} ]\f$
      REAL GCOEFF(ILG) !<Multiplier used in equation relating surface heat flux
                       !<to surface temperature \f$[W m^{-2} K^{-1} ]\f$
      REAL TGND  (ILG) !<Starting point for surface temperature iteration [K]
      REAL TRSNOW(ILG) !<Short-wave transmissivity of snow pack [ ]
      REAL FSNOWC(ILG) !<Fractional coverage of canopy by frozen water \f$[ ] (F_s )\f$
      REAL FRAINC(ILG) !<Fractional coverage of canopy by liquid water \f$[ ] (F_l )\f$
      REAL CHCAP (ILG) !<Heat capacity of vegetation canopy \f$[J m^{-2} K^{-1} ] (C_c )\f$

      REAL CMASS (ILG) !<Mass of vegetation canopy \f$[kg m^{-2} ]\f$
      REAL PCPR  (ILG) !<Surface precipitation rate \f$[kg m^{-2} s^{-1} ]\f$
      REAL RHOSNO(ILG) !<
      REAL ZSNOW (ILG) !<
C
      REAL FROOT (ILG,IG) !<
      REAL THLMIN(ILG,IG) !<
      REAL DELZW(ILG,IG) !<
C
      INTEGER IWATER(ILG) !<Flag indicating condition of surface (dry, water-covered or snow-covered)
      INTEGER IEVAP (ILG) !<Flag indicating whether surface evaporation is occurring or not output variable
      INTEGER ITERCT(ILG,6,50) !<Counter of number of iterations required to solve energy balance for four subareas
C
C     * ARRAYS FOR CTEM.
C
C     * AILCG    - GREEN LAI FOR CARBON PURPOSES
C     * FCANC    - FRACTIONAL COVERAGE OF 8 CARBON PFTs
C     * CO2CONC  - ATMOS. CO2 CONC. IN PPM
C     * RMATCTEM - FRACTION OF ROOTS IN EACH SOIL LAYER FOR EACH OF THE 8 PFTs
C                  FOR CARBON RELATED PURPOSES.
C     * RCPHTSYN - STOMATAL RESISTANCE ESTIMATED BY THE PHTSYN SUBROUTINE, S/M
C     * COSZS    - COS OF SUN'S ZENITH ANGLE
C     * XDIFFUS  - FRACTION OF DIFFUSED RADIATION
C     * CO2I1    - INTERCELLULAR CO2 CONC.(PA) FOR THE SINGLE/SUNLIT LEAF
C     * CO2I2    - INTERCELLULAR CO2 CONC.(PA) FOR THE SHADED LEAF
C     * CTEM1    - LOGICAL BOOLEAN FOR USING CTEM's STOMATAL RESISTANCE
C                  OR NOT
C     * CTEM2    - LOGICAL BOOLEAN FOR USING CTEM's STRUCTURAL ATTRIBUTES
C                  OR NOT
C     * SLAI     - STORAGE LAI. SEE PHTSYN SUBROUTINE FOR MORE DETAILS.
C     * FCANCMX  - MAX. FRACTIONAL COVERAGE OF CTEM PFTs
C     * L2MAX    - MAX. NUMBER OF LEVEL 2 CTEM PFTs
C     * NOL2PFTS - NUMBER OF LEVEL 2 CTEM PFTs
C     * ANVEG    - NET PHTOSYNTHETIC RATE, u-MOL/M^2/S, FOR CTEM's 8 PFTs
C     * RMLVEG   - LEAF MAINTENANCE RESP. RATE, u-MOL/M^2/S, FOR CTEM's 8 PFTs
C
      REAL AILCG(ILG,ICTEM),     FCANC(ILG,ICTEM),        CO2CONC(ILG),
     1     CO2I1(ILG,ICTEM),     CO2I2(ILG,ICTEM),          COSZS(ILG),
     3          PRESSG(ILG),         XDIFFUS(ILG),     SLAI(ILG,ICTEM),
     4                     RMATCTEM(ILG,ICTEM,IG),  FCANCMX(ILG,ICTEM),
     5     ANVEG(ILG,ICTEM),    RMLVEG(ILG,ICTEM),       THLIQ(ILG,IG),
     6         THFC(ILG,IG),       THLW(ILG,IG),      CFLUXV(ILG),
     7       CFLUXV_IN(ILG)

      REAL DAYL_MAX(ILG)      ! MAXIMUM DAYLENGTH FOR THAT LOCATION
      REAL DAYL(ILG)          ! DAYLENGTH FOR THAT LOCATION


      INTEGER ISAND(ILG,IG),    LFSTATUS(ILG,ICTEM)
C
      LOGICAL ctem_on

      INTEGER ICTEM, L2MAX, NOL2PFTS(IC), IC, IG
C
C     * LOCAL WORK ARRAYS FOR CTEM.
C
      REAL RCPHTSYN(ILG), QSWNVC(ILG)
C
C     * GENERAL INTERNAL WORK ARRAYS.
C
      REAL TSTEP (ILG),    TVIRTC(ILG),    TVIRTG(ILG),
     1     EVBETA(ILG),    XEVAP (ILG),    EVPWET(ILG),    Q0SAT (ILG),
     2     RA    (ILG),    RB    (ILG),    RAGINV(ILG),    RBINV (ILG),
     3     RBTINV(ILG),    RBCINV(ILG),    TVRTAC(ILG),
     4     TPOTG (ILG),    RESID (ILG),    TCANO (ILG),
     5     TRTOP (ILG),    QSTOR (ILG),    A     (ILG),    B     (ILG),
     6     LZZ0  (ILG),    LZZ0T (ILG),
     7     FM    (ILG),    FH    (ILG),    WZERO (ILG),    XEVAPM(ILG),
     8     DCFLXM(ILG),    WC    (ILG),    DRAGIN(ILG),    CFLUXM(ILG),
     9     CFSENS(ILG),    CFEVAP(ILG),    QSGADD(ILG),    CFLX  (ILG),
     A     EVPMAX(ILG),    WTRTOT(ILG)
C                                                                       
      REAL WAVAIL(ILG,IG), WROOT (ILG,IG)
C
      INTEGER              ITER  (ILG),    NITER (ILG),    IEVAPC(ILG),
     1                     KF1   (ILG),    KF2   (ILG)


C     Nitrogen variables  BW 7/2015
      LOGICAL CTEMN  
      REAL XNUP_VEG(ILG,ICTEM), NRUB0(ILG,ICTEM), VCMAX0(ILG,ICTEM)
C
C     * TEMPORARY VARIABLES.
C
      REAL QSWNVG,QSWNIG,QSWNIC,HFREZ,HCONV,
     1     RCONV,HCOOL,HMELT,SCONV,HWARM,WCAN,DQ0DT,
     2     DRDT0,QEVAPT,BOWEN,DCFLUX,DXEVAP,TCANT,QEVAPCT,
     3     TZEROT,YEVAP,RAGCO,EZERO,WTRANSP,WTEST
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,HCPW,HCPICE,
     1     HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,
     2     RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP,DELTA,CGRAV,CKARM,CPD,
     3     AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /PHYCON/ DELTA,CGRAV,CKARM,CPD
      COMMON /CLASSD2/ AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
C


C-----------------------------------------------------------------------
C     * INITIALIZATION AND PRE-ITERATION SEQUENCE.
C===================== CTEM =====================================\
C
      DO I = 1,ILG
        QSWNVC(I)=0.0
      ENDDO
C===================== CTEM =====================================/
C
      IF(ITCG.LT.2) THEN
          ITERMX=50
      ELSE
          ITERMX=5
      ENDIF
C      IF(ISNOW.EQ.0) THEN
C          EZERO=0.0
C      ELSE
C          EZERO=2.0
C      ENDIF
      EZERO=0.0
      RAGCO=1.9E-3
C
!>
!!For the subcanopy surface temperature iteration, two alternative schemes are offered: the bisection
!!method (selected if the flag ITCG = 1) and the Newton-Raphson method (selected if ITCG = 2). In the
!!first case, the maximum number of iterations ITERMX is set to 12, and in the second case it is set to 5.
!!An optional windless transfer coefficient EZERO is made available, which can be used, following the
!!recommendations of Brown et al. (2006), to prevent the sensible heat flux over snow packs from
!!becoming vanishingly small under highly stable conditions. If the snow cover flag ISNOW is zero
!!(indicating bare ground), EZERO is set to zero; if ISNOW=1, EZERO is set to \f$2.0 W m^{-2} K^{-1}\f$ . The
!!surface transfer coefficient under conditions of free convection, RAGCO, is set to \f$1.9 x 10^{-3}\f$ (see the
!!calculation of RAGINV below).
!!
!!In the 50 loop, some preliminary calculations are done. The shortwave transmissivity at the surface,
!!TRTOP, is set to zero in the absence of a snow pack, and to the transmissivity of snow, TRSNOW,
!!otherwise. The net shortwave radiation at the surface, QSWNG, is calculated as the sum of the net visible
!!and net near-infrared shortwave radiation. Each of these is obtained as:
!!\f$K_{*g} = K \downarrow \tau_c [1 - \alpha_g ]\f$
!!where \f$K_{*g} is the net radiation at the surface, \f$K \downarrow\f$ is the incoming shortwave radiation above the canopy, \f$\tau_c\f$
!!is the canopy transmissivity and \f$\alpha_g\f$ is the surface albedo. This average value is corrected for the amount
!!of radiation transmitted into the surface, QTRANS, obtained using TRTOP. The net shortwave radiation
!!for the vegetation canopy, QSWNC, is calculated as the sum of the net visible and net near-infrared
!!shortwave radiation. Each of these is determined as:
!!\f$K_{*c} = K \downarrow [1 - \alpha_c ] - K_{*g}\f$
!!where \f$K_{*c} is the net radiation on the canopy and \f$\alpha_c\f$ is the canopy albedo. If the canopy temperature is
!!essentially 0 K, indicating that a canopy was not present in the previous time step but has now appeared,
!!the canopy temperature is initialized to the potential temperature of the air, TPOTA. The outgoing
!!longwave radiation emitted upward \f$(L \uparrow_c )\f$ or downward \f$(L \downarrow_c )\f$ from the canopy is calculated using the
!!standard Stefan-Boltzmann equation:
!!\f$L \uparrow_c = L \downarrow_c = \sigma T_c^4\f$
!!where \f$\sigma\f$ is the Stefan-Boltzmann constant and \f$T_c\f$ is the canopy temperature.
!!
!!Virtual temperature is defined as temperature adjusted for the reduction in air density due to the presence
!!of water vapour. This is applied in order to enable the use of the equation of state for dry air. The virtual
!!temperature can be approximated by multiplying the actual temperature by a factor [1 + 0.61 q], where q
!!is the specific humidity of the air in question. Thus, the virtual temperature of air at the vegetation
!!canopy temperature, \f$T_{c,v}\f$ , is obtained as:
!!\f$T_{c,v} = T_c [1 + 0.61 q_c ]\f$
!!where \f$q_c\f$ is the saturated specific humidity at the canopy temperature. This is determined from the
!!saturation mixing ratio at the canopy temperature, \f$w_c\f$ :
!!\f$q_c = w_c /[1 + w_c ]\f$
!!The saturation mixing ratio is a function of the saturation vapour pressure \f$e_c\f$ at the canopy temperature:
!!\f$w_c = 0.622 e_c /(p_{dry} )\f$
!!where \f$p_{dry}\f$ is the partial pressure of dry air. A standard empirical equation for the saturation vapour
!!pressure dependence on the temperature T is used:
!!
!!\f$e_{sat} = 611.0 exp[17.269(T - T_f )/(T - 35.86)]    T \geq T_f\f$
!!\f$e_{sat} = 611.0 exp[21.874(T - T_f )/(T - 7.66)]     T < T_f\f$
!!
!!where \f$T_f\f$ is the freezing point. The virtual temperature of the air in the canopy space, \f$T_{ac,v}\f$ , is likewise
!!calculated from the canopy air temperature \f$T_{ac}\f$ and the specific humidity in the canopy air space, \f$q_{ac}\f$ , as
!!
!!\f$T_{ac,v} = T_{ac} [1 + 0.61 q_{ac} ]\f$
!!
!!If the Newton-Raphson method is being used for the canopy temperature iteration (pre-selected by
!!setting the flag ITC to 2), the temperature of the air in the canopy space is approximated as the canopy
!!temperature, and the specific humidity in the canopy air space is approximated as the specific humidity of
!!the air above the canopy.
!!
!!If there is intercepted snow on the vegetation, the latent heat associated with water flux from the canopy,
!!CPHCHC, is set to the latent heat of sublimation (the sum of the heat of vaporization and the heat of
!!melting). Otherwise the latent heat is set to that of vaporization alone. The leaf boundary layer resistance
!!RB, and its inverse RBINV, are calculated using the wind speed in the canopy air space, VAC, and a
!!coefficient RBCOEF evaluated in subroutine APREP. This coefficient is formulated after Bartlett (2004),
!!who developed an expression for the inverse of the leaf boundary resistance, \f$1/r_b\f$ , drawing on the analysis
!!of Bonan (1996) and McNaughton and van den Hurk (1995), of the form:
!!
!!\f$1/r_b = v_{ac}^{1/2} \sigma f_i \gamma_i \Lambda_i^{1/2} /0.75 [1 - exp(-0.75 \Lambda_i^{1/2})]\f$
!!
!!where \f$v_{ac}\f$ is the wind speed in the canopy air space, \f$f_i\f$ is the fractional coverage of each vegetation type i
!!over the subarea in question, \f$\Lambda_i\f$ is its leaf area index, and \f$\gamma_i\f$ is a vegetation-dependent parameter which
!!incorporates the effects of leaf dimension and sheltering. The initial value of the surface temperature
!!TZERO is set to TGND, which contains the value of TZERO from the previous time step, and the
!!initial temperature of the canopy, TCANO, is set to the current canopy temperature. The first step in the
!!iteration sequence, TSTEP, is set to 1.0 K. The flag ITER is set to 1 for each element of the set of
!!modelled areas, indicating that its surface temperature has not yet been found. The iteration counter
!!NITER is initialized to 1 for each element. Initial values are assigned to other variables.
!!
!!(At this point in the code, if CLASS is being run in coupled mode with CTEM, the CTEM subroutine
!!PHTSYN3 is called. These lines are commented out for uncoupled runs.)
!!


      DO 50 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              IF(ISNOW.EQ.0)                      THEN
                  TRTOP(I)=0.
              ELSE
                  TRTOP(I)=TRSNOW(I)
              ENDIF
              QSWNVG=QSWINV(I)*TRVISC(I)*(1.0-ALVISG(I))
              QSWNIG=QSWINI(I)*TRNIRC(I)*(1.0-ALNIRG(I))
              QSWNG(I)=QSWNVG+QSWNIG
              QTRANS(I)=QSWNG(I)*TRTOP(I)
              QSWNG(I)=QSWNG(I)-QTRANS(I)
              QSWNVC(I)=QSWINV(I)*(1.0-ALVISC(I))-QSWNVG
              QSWNIC=QSWINI(I)*(1.0-ALNIRC(I))-QSWNIG
              QSWNC(I)=QSWNVC(I)+QSWNIC
              IF(ABS(TCAN(I)).LT.1.0E-3)        TCAN(I)=TPOTA(I)
              QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
C
              IF(TCAN(I).GE.TFREZ)                         THEN
                  A(I)=17.269
                  B(I)=35.86
              ELSE
                  A(I)=21.874
                  B(I)=7.66
              ENDIF
              WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1             (TCAN(I)-B(I)))/PADRY(I)
              QCAN(I)=WCAN/(1.0+WCAN)
              TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))

              IF(ITC.EQ.2) THEN
                  TAC(I)=TCAN(I)
                  QAC(I)=QA(I)
              ENDIF

              TVRTAC(I)=TAC(I)*(1.0+0.61*QAC(I))
C
              IF(SNOCAN(I).GT.0.)             THEN
                  CPHCHC(I)=CLHVAP+CLHMLT
              ELSE
                  CPHCHC(I)=CLHVAP
              ENDIF
              RBINV(I)=RBCOEF(I)*SQRT(VAC(I))
              IF (RBINV(I) .LE. 0.) CALL XIT('TSOLVC',0)
              ! If RBINV is <= 0, it is possible your INI file is missing
              ! the PAIMIN and PAIMAX values.
              RB(I)=1.0/RBINV(I)
              TZERO(I)=TGND(I)
              TCANO(I)=TCAN(I)
              TSTEP(I)=1.0
              ITER(I)=1
              NITER(I)=1
              QMELTC(I)=0.0
              QMELTG(I)=0.0
              IF(ISNOW.EQ.1)                               THEN
                  KF1(I)=1
                  KF2(I)=2
                  EVPMAX(I)=RHOSNO(I)*ZSNOW(I)/DELT 
              ELSE
                  KF1(I)=4
                  KF2(I)=5
                  EVPMAX(I)=RHOW*(THLIQ(I,1)-THLMIN(I,1))*DELZW(I,1)/
     1                      DELT
                  EVPMAX(I)=MAX(EVPMAX(I),0.)
              ENDIF
          ENDIF
   50 CONTINUE
C
C     * CALL PHOTOSYNTHESIS SUBROUTINE HERE TO GET A NEW ESTIMATE OF
C     * RC BASED ON PHOTOSYNTHESIS.
C
      IF(ctem_on) THEN

C
C       STORE CFLUXV NUMBERS IN A TEMPORARY ARRAY
        DO I = IL1, IL2
          CFLUXV_IN(I)=CFLUXV(I)
        ENDDO
C
        CALL PHTSYN3(  AILCG, FCANC,     TCAN, CO2CONC,  PRESSG,    FI,
     1                CFLUXV,    QA,   QSWNVC,      IC,   THLIQ, ISAND,
     2                    TA,        RMATCTEM,   COSZS, XDIFFUS,   ILG,
     3                   IL1,   IL2,       IG,   ICTEM,   ISNOW,  SLAI,
     4               THFC,  THLW,  FCANCMX,   L2MAX,NOL2PFTS,
     5              RCPHTSYN, CO2I1,    CO2I2,   ANVEG,  RMLVEG,
     6              LFSTATUS,DAYL, DAYL_MAX,  !FLAG TEST LFSTATUS is new and brought in to test. JM Dec 4.
     7              CTEMN, NRUB0, XNUP_VEG,  VCMAX0)  !Nitrogen Components
C
C       * KEEP CLASS RC FOR BONEDRY POINTS (DIANA'S FLAG OF 1.E20) SUCH
C       * THAT WE GET (BALT-BEG) CONSERVATION.
C
        DO 70 I =IL1,IL2                                                
            RC(I)=MIN(RCPHTSYN(I),4999.999)
   70   CONTINUE                                                        

      ENDIF
C
C     * ITERATION FOR SURFACE TEMPERATURE OF GROUND UNDER CANOPY.
C     * LOOP IS REPEATED UNTIL SOLUTIONS HAVE BEEN FOUND FOR ALL POINTS
C     * ON THE CURRENT LATITUDE CIRCLE(S).
C
!>
!!The 100 continuation line marks the beginning of the surface temperature iteration sequence. First the
!!flag NUMIT (indicating whether there are still locations at the end of the current iteration step for which
!!the surface temperature has not yet been found) is set to zero. Loop 125 is then performed over the
!!vector of modelled areas. If ITER=1, the saturated specific humidity at the ground surface temperature,
!!\f$q(0)_{sat}\f$ , is calculated using the same method as that outlined above for the saturated specific humidity at
!!the canopy temperature. If there is a snow cover or ponded water present on the surface (IWATER > 0),
!!the surface evaporation efficiency EVBETA is set to 1 and the surface specific humidity q(0) is set to
!!\f$q(0)_{sat}\f$ . Otherwise EVBETA is set to CEVAP, the value obtained in subroutine TPREP on the basis of
!!ambient conditions, and q(0) is calculated from \f$q(0)_{sat}\f$ by making use of the definition of the surface
!!evaporation efficiency \f$\beta\f$:
!!
!!\f$\beta = [q(0) - q_{ac} ]/[q(0)_{sat} - q_{ac} ]\f$
!!
!!which is inverted to obtain an expression for q(0). If \f$q(0) > q_{ac}\f$ and the evaporation flag IEVAP has been
!!set to zero, EVBETA is reset to zero and q(0) is reset to \f$q_{ac}\f$.
!!
!!Next, the potential temperature of air at the ground surface temperature, \f$T(0)_{pot}\f$ , is calculated relative to
!!the sum of the displacement height d and the roughness length for momentum \f$z_{0,m}\f$ , the height at which
!!the canopy temperature is assumed to apply. (This is also the height corresponding to the potential
!!temperature of the air at the reference height above the canopy). \f$T(0)_{pot}\f$ is found using the dry adiabatic
!!lapse rate, \f$dT/dz = -g/c_p\f$ , where g is the acceleration due to gravity and \f$c_p\f$ is the specific heat at constant
!!pressure. Since the displacement height d is assumed to lie at 0.7 of the canopy height, and the roughness
!!length for momentum is assumed to lie at 0.1 of the canopy height, their sum can be obtained as \f$8.0 z_{0,m}\f$ .
!!Thus,
!!\f$T(0)_{pot} = T(0) - 8.0 z_{0,m} g/c_p\f$
!!
!!The virtual potential temperature at the surface is obtained using the same expression as above:
!!\f$T(0)_v = T(0)_{pot} [1 + 0.61 q(0)]\f$
!!
!!Since wind speeds under the canopy are expected to be relatively small, it is assumed that under stable or
!!neutral conditions, turbulent fluxes are negligible. If the virtual potential temperature of the surface is
!!more than 1 K greater than that of the canopy air, free convection conditions are assumed. Townsend's
!!(1964) equation for the surface-air transfer coefficient, or the inverse of the surface resistance \f$r_{a,,g}\f$ , is used
!!in a form derived from the analysis of Deardorff (1972):
!!\f$1/r_{a,,g} = 1.9 x 10^{-3} [T(0)_v - T_{ac,v} ]^{1/3}\f$
!!
!!The first derivative of the transfer coefficient with respect to the surface temperature is calculated for use
!!with the Newton-Raphson iteration scheme:
!!\f$d(1/r_{a,,g} )/dT = 1.9 x 10^{-3} [T(0)_v - T_{ac,v} ]^{-2/3} /3\f$
!!
!!If the virtual potential temperature of the surface is between 0.001 and 1 K greater than that of the
!!canopy air, a simple diffusion relation is assumed:
!!\f$1/r_{a,,g} = 1.9 x 10^{-3} [T(0)_v - T_{ac,v} ]\f$
!!Thus,
!!\f$d(1/r_{a,,g} )/dT = 1.9 x 10^{-3}\f$
!!
!!The remaining terms of the surface energy balance are now evaluated. The energy balance equation is
!!expressed as:
!!\f$K_{*g} + L_{*g} - Q_{H,g} - Q_{E,g} - G(0) = 0\f$
!!where \f$K_{*g}\f$ is the net surface shortwave radiation, \f$L_{*g}\f$ is the net longwave radiation, \f$Q_{H,g}\f$ is the sensible heat
!!flux, \f$Q_{E,g}\f$ is the latent heat flux, and G(0) is the conduction into the surface. \f$K_{*g}\f$ was evaluated earlier in
!!loop 50. \f$L_{*g}\f$ is calculated as the difference between the downwelling radiation \f$L \downarrow_g\f$ at the surface and the
!!upwelling radiation \f$L \uparrow_g\f$ . The downwelling radiation incident on the surface is determined from the
!!downwelling sky radiation above the canopy \f$L \downarrow\f$ and the downwelling radiation from the canopy itself,
!!\f$L \downarrow_c\f$ , weighted according to the sky view factor \f$\chi\f$:
!!\f$L \downarrow_g = \chi L \downarrow + [1 - \chi] L \downarrow_c\f$
!!The upwelling radiation is obtained using the Stefan-Boltzmann equation:
!!\f$L \uparrow_g = \sigma T(0)^4\f$
!!
!!The sensible heat flux is given by
!!\f$Q_{H,g} = \rho_a c_p [T(0)_{pot} - T_{a,c} ]/r_{a,,g}\f$
!!where \f$\rho_a\f$ is the density of the air and \f$c_p\f$ is its specific heat. (The windless transfer coefficient defined at
!!the beginning of the subroutine is currently not used under vegetation canopies.) The evaporation rate at
!!the surface, E(0), is calculated as
!!\f$E(0) = \rho_a [q(0) - q_{a,c} ]/r_{a,,g}\f$
!!
!!\f$Q_E\f$ is obtained by multiplying E(0) by the latent heat of vaporization at the surface. The heat flux into the
!!surface G(0) is determined as a linear function of T(0) (see documentation for subroutines TNPREP and
!!TSPREP). It can be seen that each of the terms of the surface energy balance is a function of a single
!!unknown, T(0) or TZERO. The residual RESID of the energy balance is now evaluated on the basis of
!!the current estimation for TZERO. If the absolute value of RESID is less than \f$5.0 W m^{-2}\f$ , or if the
!!absolute value of the iteration step TSTEP most recently used is less than 0.01 K, the surface temperature
!!is deemed to have been found and ITER is set to 0. If the iteration counter NITER is equal to the
!!maximum number and ITER is still 1, ITER is set to -1.
!!
!!In the following section, the iteration sequence is moved ahead a step. If ITCG = 1, the calculations for
!!the bisection method of solution in loop 150 are performed, over each of the modelled areas for which
!!ITER = 1. If NITER = 1 (indicating that this is the first step in the iteration), then if RESID > 0
!!(indicating that the current value for TZERO had undershot the correct value), TZERO is incremented
!!by 1 K; otherwise it is decremented by 1 K. If this is not the first step in the iteration, then if RESID >0
!!and TSTEP < 0 (indicating that TZERO has undershot the correct value and the last temperature
!!increment had been a negative one) or if RESID < 0 and TSTEP > 0 (indicating that TZERO has
!!overshot the correct value and the last temperature increment had been a positive one), TSTEP is divided
!!in half and its sign changed. TSTEP is then added to TZERO. The iteration counter NITER and the
!!flag NUMIT are each incremented by one. Finally, if NUMIT > 0, the iteration cycle is repeated from
!!line 100 on.
!!
!!If ITCG = 2, the calculations for the Newton-Raphson method of iteration in loop 175 are performed,
!!over each of the modelled areas for which ITER = 1. In this approach, the value \f$x_{n+1}\f$ used at each
!!iteration step is obtained from the value \f$x_n\f$ at the previous step as follows:
!!\f$x_{n+1} = x_n - f(x_n )/f'(x_n )\f$
!!
!!Identifying x n with TZERO and \f$f(x_n )\f$ with the surface energy balance equation, it can be seen that the
!!second term on the right-hand side corresponds to TSTEP; the numerator is equal to RESID and the
!!denominator to the first derivative of the energy balance equation evaluated at TZERO, which in turn is
!!equal to the sum of the derivatives of the individual terms:
!!f[
!!d(L \uparrow_g )/dT = -4 \sigma T(0)^3 f]f[
!!d(Q_{H,g} )/dT = \rho_a c_p {1/r_{a,,g} + [T(0)_{pot} - T_{ac} ] d(1/r_{a,,g} )/dT} f]f[
!!d(Q_{E,g} )/dT = L_v \rho_a {1/r_{a,,g} dq(0)/dT+ [q(0) - q_{ac} ] d(1/r_{a,,g} )/dT}
!!f]
!!and dG(0)/dT is equal to the coefficient multiplying TZERO in the equation for G(0). (\f$L_v\f$ is the latent
!!heat of vaporization at the surface.) At the end of the calculations the iteration counter NITER and the
!!flag NUMIT are each incremented by one, and upon exiting the loop, if NUMIT > 0, the iteration cycle is
!!repeated from line 100 on.
!!
  100 CONTINUE
C


      NUMIT=0
      DO 125 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              IF(TZERO(I).GE.TFREZ)                           THEN
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
                  QZERO(I)=EVBETA(I)*Q0SAT(I)+(1.0-EVBETA(I))*QAC(I)
                  IF(QZERO(I).GT.QAC(I) .AND. IEVAP(I).EQ.0) THEN
                      EVBETA(I)=0.0
                      QZERO(I)=QAC(I)
                  ENDIF
              ENDIF
C
              TPOTG(I)=TZERO(I)-8.0*ZOM(I)*GRAV/CPD
              TVIRTG(I)=TPOTG(I)*(1.0+0.61*QZERO(I))

              IF(TVIRTG(I).GT.TVRTAC(I)+1.)                   THEN
                  RAGINV(I)=RAGCO*(TVIRTG(I)-TVRTAC(I))**0.333333
                  DRAGIN(I)=0.333*RAGCO*(TVIRTG(I)-TVRTAC(I))**(-.667)
              ELSEIF(TVIRTG(I).GT.(TVRTAC(I)+0.001))          THEN
                  RAGINV(I)=RAGCO*(TVIRTG(I)-TVRTAC(I))
                  DRAGIN(I)=RAGCO
              ELSE
                  RAGINV(I)=0.0
                  DRAGIN(I)=0.0
              ENDIF
C
              QLWOG(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
              QSENSG(I)=RHOAIR(I)*SPHAIR*RAGINV(I)*
     1            (TPOTG(I)-TAC(I))	       



              EVAPG (I)=RHOAIR(I)*(QZERO(I)-QAC(I))*RAGINV(I)

              IF(EVAPG(I).GT.EVPMAX(I)) EVAPG(I)=EVPMAX(I)
              QEVAPG(I)=CPHCHG(I)*EVAPG(I)
              GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
              RESID(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1            QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)-GZERO(I)
              IF(ABS(RESID(I)).LT.5.0)                     ITER(I)=0
              IF(ABS(TSTEP(I)).LT.1.0E-2)                  ITER(I)=0
              IF(NITER(I).EQ.ITERMX .AND. ITER(I).EQ.1)    ITER(I)=-1
          ENDIF
125   CONTINUE
C
      IF(ITCG.LT.2) THEN
C
C     * OPTION #1: BISECTION ITERATION METHOD.
C
      DO 150 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              IF(NITER(I).EQ.1) THEN
                  IF(RESID(I).GT.0.0) THEN
                      TZERO(I)=TZERO(I)+TSTEP(I)
                  ELSE
                      TZERO(I)=TZERO(I)-TSTEP(I)
                  ENDIF
              ELSE
                  IF((RESID(I).GT.0. .AND. TSTEP(I).LT.0.) .OR.
     1                (RESID(I).LT.0. .AND. TSTEP(I).GT.0.))   THEN
                      TSTEP(I)=-TSTEP(I)/2.0
                  ENDIF
                  TZERO(I)=TZERO(I)+TSTEP(I)
              ENDIF
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  150 CONTINUE
C
      ELSE
C
C     * OPTION #2: NEWTON-RAPHSON ITERATION METHOD.
C
      DO 175 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              DQ0DT=-WZERO(I)*A(I)*(B(I)-TFREZ)/((TZERO(I)-B(I))*
     1               (1.0+WZERO(I)))**2*EVBETA(I)
              DRDT0=-4.0*SBC*TZERO(I)**3
     1               -GCOEFF(I)-RHOAIR(I)*SPHAIR*
     2              (RAGINV(I)+(TPOTG(I)-TAC(I))*DRAGIN(I))-
     3               CPHCHG(I)*RHOAIR(I)*(DQ0DT*RAGINV(I)
     4              +(QZERO(I)-QAC(I))*DRAGIN(I))
              TSTEP(I)=-RESID(I)/DRDT0
              IF(ABS(TSTEP(I)).GT.20.0) TSTEP(I)=SIGN(10.0,TSTEP(I))
              TZERO(I)=TZERO(I)+TSTEP(I)
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  175 CONTINUE
C
      ENDIF
C
      IF(NUMIT.GT.0)                                    GO TO 100
C
C     * IF CONVERGENCE HAS NOT BEEN REACHED FOR ITERATION METHOD #2,
C     * CALCULATE TEMPERATURE AND FLUXES ASSUMING NEUTRAL STABILITY
C     * AND USING BOWEN RATIO APPROACH.
C
      IF(ITCG.EQ.2)                                                 THEN
C
!>
!!After the iteration has been completed, if the Newton-Raphson method has been used, a check is carried
!!out in loop 200 to ascertain whether convergence has not been reached (i.e. whether ITER = -1) for any
!!location. In such cases it is assumed that conditions of near-neutral stability at the surface are the cause of
!!the difficulty in finding a solution. A trial value of TZERO is calculated using the virtual potential
!!temperature of the canopy. If the absolute value of RESID is \f$> 15 W m^{-2}\f$ , TZERO is set to this trial
!!value. The values of q(0) and the components of the surface energy balance are recalculated as above,
!!except that \f$Q_{H,g}\f$ and \f$Q_{E,g}\f$ are assumed as a first approximation to be zero. RESID is determined on this
!!basis, and is then partitioned between \f$Q_{H,g}\f$ and \f$Q_{E,g}\f$ on the basis of the Bowen ratio B, the ratio of \f$Q_{H,g}\f$
!!over \f$Q_{E,g}\f$ . Setting the residual R equal to \f$Q_{H,g} + Q_{E,g}\f$ , and substituting for \f$Q_{H,g}\f$ using \f$B = Q_{H,g} /Q_{E,g}\f$ ,
!!results in:
!!\f$Q_{E,g} = R/(1 + B)\f$
!!\f$Q_{H,g}\f$ is then obtained as \f$R - Q_{E,g}\f$ , the residual is reset to zero, and E(0) is recalculated from \f$Q_{E,g}\f$ .
!!
!!At this point a check is performed for unphysical values of the surface temperature, i.e. for values greater
!!than 100 C or less than -100 C. If such values are encountered, an error message is printed and a call to
!!abort is carried out.
!!
      DO 200 I=IL1,IL2
          IF(ITER(I).EQ.-1)                                  THEN
             TZEROT=TVIRTC(I)/(1.0+0.61*QZERO(I))
             IF(ABS(RESID(I)).GT.15.) THEN
                TZERO(I)=TZEROT
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
                QZERO(I)=EVBETA(I)*Q0SAT(I)+(1.0-EVBETA(I))*QAC(I)
                QLWOG(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
                GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
                RESID(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1              QLWOC(I)-QLWOG(I)-GZERO(I)
                QEVAPT=CPHCHG(I)*(QZERO(I)-QAC(I))
                BOWEN=SPHAIR*(TZERO(I)-TAC(I))/
     1             SIGN(MAX(ABS(QEVAPT),1.E-6),QEVAPT)
                QEVAPG(I)=RESID(I)/SIGN(MAX(ABS(1.+BOWEN),0.1),1.+BOWEN)
                QSENSG(I)=RESID(I)-QEVAPG(I)
                RESID(I)=0.
                EVAPG(I)=QEVAPG(I)/CPHCHG(I)

             ENDIF
          ENDIF
  200 CONTINUE
C
      ENDIF
C
      IBAD=0
C
      DO 225 I=IL1,IL2
C          IF(FI(I).GT.0. .AND. ITER(I).EQ.-1)                     THEN
C              WRITE(6,6250) I,JL,NITER(I),RESID(I),TZERO(I),RIB(I)
C6250          FORMAT('0SUBCAN ITERATION LIMIT',3X,3I3,3(F8.2,E12.4))
C          ENDIF
          IF(FI(I).GT.0.)                                           THEN
              IF(TZERO(I).LT.173.16 .OR. TZERO(I).GT.373.16)    THEN
                  IBAD=I
              ENDIF
          ENDIF
 225  CONTINUE
C
      IF(IBAD.NE.0)                                                 THEN
          WRITE(6,6370) IBAD,N,TZERO(IBAD),NITER(IBAD),ISNOW
 6370     FORMAT('0BAD GROUND ITERATION TEMPERATURE',3X,2I8,F16.2,2I4)
          WRITE(6,6380) QSWNG(IBAD),FSVF(IBAD),QLWIN(IBAD),QLWOC(IBAD),
     1        QLWOG(IBAD),QSENSG(IBAD),QEVAPG(IBAD),GZERO(IBAD)
          WRITE(6,6380) TCAN(IBAD)
          CALL XIT('TSOLVC',-1)
      ENDIF
C
C     * POST-ITERATION CLEAN-UP.
C
!>
!!Finally, clean-up calculations are performed in loop 250. A check is carried out to ensure that TZERO is
!!not less than 0 C if ponded water is present on the surface (IWATER = 1) or greater than 0 C if snow is
!!present on the surface (IWATER = 2). If either is the case, TZERO is reset to the freezing point, and
!!q(0), \f$T(0)_{pot}\f$ and \f$T(0)_v\f$ are re-evaluated. The components of the surface energy balance are recalculated
!!using the above equations. The residual of the energy balance equation is assigned to the energy
!!associated with phase change of water at the surface, QMELTG, and RESID is set to zero.
!!
!!In the last part of the loop, some final adjustments are made to a few other variables. If the evaporation
!!flux is vanishingly small, it is added to RESID and reset to zero. If both RESID and \f$Q_{E,g}\f$ are not small,
!!and if the precipitation rate is vanishingly small, RESID is added to Q E,g ; otherwise RESID is added to
!!\f$Q_{H,g}\f$ . Lastly, the iteration counter ITERCT is updated for the level corresponding to the subarea type and
!!the value of NITER.
!!
      DO 250 I=IL1,IL2
          IF(FI(I).GT.0.)                                        THEN
              IF((IWATER(I).EQ.1 .AND. TZERO(I).LT.TFREZ) .OR.
     1              (IWATER(I).EQ.2 .AND. TZERO(I).GT.TFREZ))  THEN
                  TZERO(I)=TFREZ
                  WZERO(I)=0.622*611.0/PADRY(I)
                  QZERO(I)=WZERO(I)/(1.0+WZERO(I))
                  TPOTG(I)=TZERO(I)-8.0*ZOM(I)*GRAV/CPD
                  TVIRTG(I)=TPOTG(I)*(1.0+0.61*QZERO(I))
C
                  QLWOG(I)=SBC*TZERO(I)*TZERO(I)*TZERO(I)*TZERO(I)
                  GZERO(I)=GCOEFF(I)*TZERO(I)+GCONST(I)
                  IF(TVIRTG(I).GT.(TVRTAC(I)+0.001))         THEN
                      RAGINV(I)=RAGCO*(TVIRTG(I)-TVRTAC(I))**0.333333
                      QSENSG(I)=RHOAIR(I)*SPHAIR*RAGINV(I)*
     1                          (TPOTG(I)-TAC(I))
                      EVAPG (I)=RHOAIR(I)*(QZERO(I)-QAC(I))*RAGINV(I)

                  ELSE
                      RAGINV(I)=0.0
                      QSENSG(I)=0.0
                      EVAPG (I)=0.0
                  ENDIF
                  IF(EVAPG(I).GT.EVPMAX(I)) EVAPG(I)=EVPMAX(I)
                  QEVAPG(I)=CPHCHG(I)*EVAPG(I)
                  QMELTG(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                 QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)-GZERO(I)
                  RESID(I)=0.0
              ENDIF
C
              IF(ABS(EVAPG(I)).LT.1.0E-8) THEN
                  RESID(I)=RESID(I)+QEVAPG(I)
                  EVAPG(I)=0.0
                  QEVAPG(I)=0.0
              ENDIF
C              IF(RESID(I).GT.15. .AND. QEVAPG(I).GT.10. .AND. PCPR(I)   
C     1                   .LT.1.0E-8)                 THEN               
C                  QEVAPG(I)=QEVAPG(I)+RESID(I)                          
C              ELSE                                                      
                  QSENSG(I)=QSENSG(I)+RESID(I)
C              ENDIF                                                     
              ITERCT(I,KF2(I),NITER(I))=ITERCT(I,KF2(I),NITER(I))+1
          ENDIF
  250 CONTINUE
C
C     * PRE-ITERATION SEQUENCE FOR VEGETATION CANOPY.
C
!>
!!In the 300 loop, preliminary calculations are done in preparation for the canopy temperature iteration.
!!The sensible heat flux from the surface is treated differently if ITC = 1 (bisection method of solution) and
!!ITC = 2 (Newton-Raphson method of solution). In the first instance the surface sensible heat flux is
!!applied to heating the air in the canopy space; in the second, the canopy and the air space within it are
!!treated as one aggregated mass, and the sensible heat flux from below is assumed to be added to its energy
!!balance. Thus, if ITC = 1, the sensible heat flux that is added to the canopy, QSGADD, is set to 0. If
!!ITC = 2, it is set to \f$Q_{H,g}\f$ ; and \f$T_{ac}\f$ is set to \f$T_c\f$ , \f$q_{ac}\f$ to \f$q_c\f$ , and \f$T_{ac,v}\f$ to \f$T_{c,v}\f$ .
!!The flag ITER is set to 1 for each
!!element of the set of modelled areas, indicating that its surface temperature has not yet been found. The
!!iteration counter NITER is initialized to 1 for each element. The first step in the iteration sequence,
!!TSTEP, is set to 1.0 K. Initial values are assigned to other variables. After exiting the loop, the
!!maximum number of iterations ITERMX is set to 12 if ITC = 1, and to 5 if ITC = 2.
!!
      DO 300 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              QSGADD(I)=0.0
              IF(ITC.EQ.2) THEN
                  QSGADD(I)=QSENSG(I)
                  TAC(I)=TCAN(I)
                  QAC(I)=QCAN(I)
                  TVRTAC(I)=TVIRTC(I)
              ENDIF
              ITER(I)=1
              NITER(I)=1
              TSTEP(I)=1.0
              CFLUXM(I)=0.0
              DCFLXM(I)=0.0
              WTRTOT(I)=0.0
          ENDIF
  300 CONTINUE
C
      DO 350 J=1,IG
      DO 350 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN 
              WAVAIL(I,J)=RHOW*(THLIQ(I,J)-THLMIN(I,J))*DELZW(I,J)
              IF(J.EQ.1 .AND. EVAPG(I).GT.0.0) 
     1            WAVAIL(I,J)=WAVAIL(I,J)-EVAPG(I)*DELT
              WAVAIL(I,J)=MAX(WAVAIL(I,J),0.)
              WROOT(I,J)=0.0
          ENDIF
  350 CONTINUE
C
      IF(ITC.LT.2) THEN
          ITERMX=50
      ELSE
          ITERMX=5
      ENDIF
C
C     * ITERATION FOR CANOPY TEMPERATURE.
C     * LOOP IS REPEATED UNTIL SOLUTIONS HAVE BEEN FOUND FOR ALL POINTS
C     * ON THE CURRENT LATITUDE CIRCLE(S).
C
!>
!!The 400 continuation line marks the beginning of the canopy temperature iteration sequence. First the
!!flags NIT (indicating whether there are still locations at the beginning of the current iteration step for
!!which the surface temperature has not yet been found) and NUMIT (indicating whether there are still
!!locations at the end of the current iteration step for which the surface temperature has not yet been
!!found) are set to zero. Loop 450 is then performed over the set of modelled areas. If ITER=1, NIT is
!!incremented by one; if ITC = 1, the vegetation virtual temperature \f$T_{c,v}\f$ is recalculated.
!!
!!If NIT > 0, a subroutine is called to evaluate the stability-corrected surface drag coefficients for heat and
!!momentum. The subroutine selection is made on the basis of the flag ISLFD. If ISLFD=1, indicating
!!that the calculations are to be consistent with CCCma conventions, subroutine DRCOEF is called; if
!!ISLFD=2, indicating that the calculations are to be consistent with RPN conventions, subroutine
!!FLXSURFZ is called.
!!
!!Next, canopy parameters and turbulent transfer coefficients are calculated prior to evaluating the terms of
!!the surface energy balance equation. If ITC = 1, the analysis of Garratt (1992) is followed. The sensible
!!and latent heat fluxes from the canopy air to the overlying atmosphere, \f$Q_H\f$ and \f$Q_E\f$ , are obtained as the
!!sums of the sensible and latent heat fluxes from the canopy to the canopy air, \f$Q_{H,c}\f$ and \f$Q_{E,c}\f$ , and from the
!!underlying surface to the canopy air, \f$Q_{H,g}\f$ and \f$Q_{E,g}\f$ :
!!
!!\f$Q_H = Q_{H,c} + Q_{H,g}\f$
!!
!!\f$Q_E = Q_{E,c} + Q_{E,g}\f$
!!
!!where
!!
!!\f$Q_H = \rho_a c_p [T_{a,c} - T_{a,pot,} ]/r_a\f$
!!
!!\f$Q_{H,c} = \rho_a c_p [T_c - T_{a,c,} ]/r_b\f$
!!
!!and
!!
!!\f$Q_E = L_v \rho_a [q_{a,c} - q_{a,} ]/r_a\f$
!!
!!\f$Q_{E,c} = L_v \rho_a [q_{,c} - q_{a,c,} ]/(r_b + r_c )\f$
!!
!!The equations for the sensible and latent heat fluxes from the surface were presented above. In these
!!expressions, \f$T_{a,pot} and \f$q_a\f$ are the potential temperature and specific humidity of the air overlying the
!!canopy, \f$r_a\f$ is the aerodynamic resistance to turbulent transfer between the canopy air and the overlying air,
!!and \f$r_c\f$ is the stomatal resistance to transpiration. (The term CFLUX that is generated by the subroutines
!!DRCOEF and FLXSURFZ is equivalent to \f$1/r_a\f$ .) Thus, \f$T_{a,c}\f$ and \f$q_{a,c}\f$ can be evaluated as
!!
!!\f$T_{a,c} = [T_{a,pot} /r_a + T_c /r_b + T(0)_{pot} /r_{a,,g} ]/[1/r_a + 1/r_b + 1/r_{a,,g} ]\f$
!!\f$q_{a,c} = [q_a /r_a + q_c /(r_b + r_c ) + q(0)/r_{a,,g} ]/[1/r_a + 1/(r_b + r_c ) + 1/r_{a,,g} ]\f$
!!
!!If the water vapour flux is towards the canopy leaves, or if the canopy is snow-covered or rain-covered, \f$r_c\f$
!!is zero. If the water vapour flux is away from the canopy leaves and the fractional snow or water
!!coverage on the canopy, \f$F_s\f$ or \f$F_l\f$ , is less than 1, the term \f$1/(r_b + r_c )\f$ adjusted for the presence of
!!intercepted snow or water, \f$X_E\f$ (XEVAP in the code) is calculated as a weighted average, as follows. If \f$F_s > 0\f$,
!!the canopy must be at a temperature of 0 C or less, and so it is deduced that no transpiration can be
!!occurring. Thus,
!!\f$X_E = (F_s + F_l )/r_b\f$ .
!!
!!Otherwise, \f$X_E\f$ is calculated on the assumption that the resistance is equal to \f$r_b\f$ over the water-covered
!!area and \f$r_b + r_c\f$ over the rest:
!!\f$X_E = F_l /r_b + [1 - F_l ]/[r_b + r_c ]\f$
!!
  400 CONTINUE
C
      NUMIT=0
      NIT=0
!>
!!In the 450 loop, XEVAP is first set as a trial value to RBINV (neglecting stomatal resistance), and QAC is
!!calculated using this value. If \f$q_{a,c} < q_c\f$ , indicating that the vapour flux is away from the canopy leaves,
!!XEVAP is recalculated as above and QAC is re-evaluated. Otherwise, if \f$F_s > 0\f$, XEVAP is scaled by \f$F_s\f$ ,
!!since it is assumed that snow on the canopy will be at a lower temperature than the canopy itself, and
!!deposition via sublimation will occur preferentially onto it. \f$T_{a,c}\f$ and \f$T_{ac,v}\f$ are calculated as above, and the
!!canopy-air turbulent transfer coefficients for sensible and latent heat flux, CFSENS and CFEVAP, are set
!!to RBINV and XEVAP respectively.
!!
!!If ITC = 2, the canopy parameters and turbulent transfer coefficients are calculated, as noted above, on
!!the basis of the assumption that the vegetation canopy and the air space within it can be treated as one
!!aggregated mass, with a single representative temperature. Thus, the resistances \f$r_a\f$ and \f$r_b\f$ are considered as
!!acting in series upon the sensible and latent heat fluxes between the canopy and the overlying air:
!!
!!\f$Q_{H,c} = \rho_a c_p [T_{,c} - T_{a,pot,} ]/(r_a + r_b )\f$
!!
!!\f$Q_{E,c} = L_v \rho_a [q_{,c} - q_{a,} ]/(r_a + r_b + r_c )\f$
!!
      DO 450 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                     THEN
              NIT=NIT+1
              IF(ITC.EQ.1) THEN
                  IF(TCAN(I).GE.TFREZ)                       THEN
                      A(I)=17.269
                      B(I)=35.86
                  ELSE
                      A(I)=21.874
                      B(I)=7.66
                  ENDIF
                  WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1                 (TCAN(I)-B(I)))/PADRY(I)
                  QCAN(I)=WCAN/(1.0+WCAN)
                  TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
              ENDIF
          ENDIF
  450 CONTINUE
C
      IF(NIT.GT.0)                                                  THEN
C
C     * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT)
C     * AND OTHER RELATED QUANTITIES BETWEEN CANOPY AIR SPACE AND
C     * ATMOSPHERE.
C
        IF(ISLFD.LT.2) THEN
            CALL DRCOEF(CDM,CDH,RIB,CFLUX,QAC,QA,ZOSCLM,ZOSCLH,
     1                  CRIB,TVRTAC,TVIRTA,VA,FI,ITER,
     2                  ILG,IL1,IL2)
        ELSE
            CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TAC,QAC,H,ZOM,ZOH,
     3                    LZZ0,LZZ0T,FM,FH,ILG,IL1,IL2,FI,ITER,JL )
        ENDIF
C
C     * CALCULATE CANOPY AIR TEMPERATURE AND SPECIFIC HUMIDITY OF
C     * CANOPY AIR (FIRST WITHOUT RC TO CHECK FOR CONDENSATION;
C     * IF NO CONDENSATION EXISTS, RECALCULATE).
C
        IF(ITC.EQ.1) THEN
C
        DO 475 I=IL1,IL2
            IF (FI(I).GT.0. .AND. ITER(I).EQ.1)                THEN
                XEVAP(I)=RBINV(I)
                QAC(I)=(QCAN(I)*XEVAP(I)+QZERO(I)*RAGINV(I)+
     1              QA(I)*CFLUX(I))/(XEVAP(I)+RAGINV(I)+CFLUX(I))
                IF(QAC(I).LT.QCAN(I))                     THEN
                   IF(FSNOWC(I).GT.0.0)               THEN
                       XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RB(I)
                   ELSE
                       XEVAP(I)=FRAINC(I)/RB(I)+(1.0-FRAINC(I))/
     1                          (RB(I)+RC(I))
                       QAC(I)=(QCAN(I)*XEVAP(I)+QZERO(I)*RAGINV(I)+
     1                     QA(I)*CFLUX(I))/(XEVAP(I)+RAGINV(I)+
     2                     CFLUX(I))
                   ENDIF
                ELSE
                    IF(FSNOWC(I).GT.1.0E-5) THEN
                        XEVAP(I)=FSNOWC(I)/RB(I)
                    ELSE
                        XEVAP(I)=1.0/RB(I)
                    ENDIF
                ENDIF
                TAC(I)=(TCAN(I)*RBINV(I)+TPOTG(I)*RAGINV(I)+
     1              TPOTA(I)*CFLUX(I))/(RBINV(I)+RAGINV(I)+CFLUX(I))
                TVRTAC(I)=TAC(I)*(1.0+0.61*QAC(I))
                CFSENS(I)=RBINV(I)
                CFEVAP(I)=XEVAP(I)
            ENDIF
475     CONTINUE
C
        ELSE
C
!>
!!In loop 500 the term \f$1/(r_a + r_b )\f$ is calculated from the variables CFLUX (the inverse of \f$r_a\f$ ) and RBINV
!!(the inverse of \f$r_b\f$ ), and assigned to the temporary variable CFLX. If the incoming visible shortwave
!!radiation QSWINV is greater than or equal to \f$25 W m^{-2}\f$ , the calculated value of CFLX is retained; if
!!QSWINV is zero, it is reset to CFLUX; and between these two limits it varies linearly between the two.
!!
!!Thus the effect of the calculated leaf boundary resistance is suppressed during conditions of zero or low
!!solar heating. (This is done to avoid unrealistically low calculated turbulent fluxes at night, which can lead
!!to anomalously low canopy temperatures.) The overall aerodynamic resistance \f$r_A = r_a + r_b\f$ is obtained as
!!1/CFLX and assigned to the variable RA. As with ITC = 1, if \f$q_a < q_c\f$ , XEVAP is recalculated as above
!!(except that \f$r_A\f$ is substituted for \f$r_b\f$ ). If \f$F_s > 0\f$, XEVAP is again scaled by \f$F_s\f$ .
!!
!!In the approach used here, the specific humidity of the aggregated canopy \f$q_{0,c}\f$ is not assumed to be equal
!!to the saturated specific humidity at the canopy temperature, but is rather determined using \f$X_E\f$ . If the
!!two methods of calculating \f$Q_{E,c}\f$ are assumed to be analogous:
!!
!!\f$Q_{E,c} = L_v \rho_a X_E [q_{,c} - q_{a,} ]\f$
!!
!!\f$Q_{E,c} = L_v \rho_a [q_{,0,c} - q_{a,} ]/r_A\f$
!!
!!then solving for \f$q_{0,c}\f$ leads to the expression
!!
!!\f$q_{0,c} = r_A X_E q_c + [1 - r_A X_E ]q_a\f$
!!
!!In the second part of the 500 loop the saturated specific humidity of the canopy is calculated as before
!!and adjusted using the above equation to obtain the specific humidity of the aggregated canopy. This is
!!then used to calculate \f$T_{c,v}\f$ . The canopy-air turbulent transfer coefficients for sensible and latent heat flux,
!!CFSENS and CFEVAP, are both set to CFLX, and for calculation purposes in the following loop \f$T_{a,c}\f$ is
!!set to the potential temperature of the air above the canopy, and \f$q_{a,c}\f$ to the specific humidity of the air
!!above the canopy.
!!
        DO 500 I=IL1,IL2
            IF (FI(I).GT.0. .AND. ITER(I).EQ.1)                THEN
                CFLX(I)=RBINV(I)*CFLUX(I)/(RBINV(I)+CFLUX(I))
                CFLX(I)=CFLUX(I)+(CFLX(I)-CFLUX(I))*
     1              MIN(1.0,QSWINV(I)*0.04)
                RA(I)=1.0/CFLX(I)
                IF(QA(I).LT.QCAN(I))                     THEN
                   IF(FSNOWC(I).GT.0.0)               THEN
                       XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RA(I)
                   ELSE
                       XEVAP(I)=FRAINC(I)/RA(I)+(1.0-FRAINC(I))/
     1                          (RA(I)+RC(I))
                   ENDIF
                ELSE
                    IF(FSNOWC(I).GT.1.0E-5) THEN
                        XEVAP(I)=FSNOWC(I)/RA(I)
                    ELSE
                        XEVAP(I)=1.0/RA(I)
                    ENDIF
                ENDIF
                IF(TCAN(I).GE.TFREZ)                         THEN
                    A(I)=17.269
                    B(I)=35.86
                ELSE
                    A(I)=21.874
                    B(I)=7.66
                ENDIF
                WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1               (TCAN(I)-B(I)))/PADRY(I)
                WC(I)=WCAN
                QCAN(I)=WCAN/(1.0+WCAN)
                QCAN(I)=RA(I)*XEVAP(I)*QCAN(I)+(1.0-RA(I)*XEVAP(I))*
     1              QA(I)
                TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                CFSENS(I)=CFLX(I)
                CFEVAP(I)=CFLX(I)
                TAC(I)=TPOTA(I)
                QAC(I)=QA(I)
            ENDIF
500     CONTINUE
C
        ENDIF
C
C     * CALCULATE THE TERMS IN THE ENERGY BALANCE AND SOLVE.
C
!>
!!In the 525 loop, the terms of the canopy energy balance are evaluated. The energy balance is expressed
!!as:
!!
!!\f$K_{*c} + L_{*c} - Q_{H,c} + Q_{H,g+} - Q_{E,c} - \Delta Q_{S,c} = 0\f$
!!
!!(The term \f$Q_{H,g+}\f$ corresponds to the variable QSGADD discussed above; the term \f$\Delta Q_{S,c}\f$ represents the
!!change of energy storage in the canopy.) The net shortwave radiation \f$K_{*c}\f$ was evaluated earlier in the 50
!!loop. The net longwave radiation is obtained as:
!!
!!\f$L_{*c} = (1 - \chi) [L \downarrow + L \uparrow_g - 2 L \downarrow_c ]\f$
!!
!!\f$Q_{H,c}\f$ is calculated as above. If there is intercepted liquid or frozen water on the canopy, or if the vapour
!!flux is downward, or if the stomatal resistance is less than a limiting high value of \f$5000 s m^{-1}\f$ , the canopy
!!vapour flux \f$E_c\f$ (that is, \f$Q_{E,c} /L_v\f$ ) is calculated as above and the flag IEVAPC is set to 1; otherwise \f$E_c\f$ and
!!IEVAPC are both set to zero and \f$q_c\f$ is set to \f$q_a\f$ . If the water vapour flux is towards the canopy and the
!!canopy temperature is greater than the air dew point temperature, the flux is set to zero. If there is
!!intercepted water on the canopy, a limiting evaporation flux EVPWET is calculated as the rate required to
!!sublimate all of the intercepted snow if \f$F_s > 0\f$, or all of the intercepted rain otherwise. If the canopy is
!!more than half covered by intercepted water and the calculated canopy vapour flux is greater than
!!EVPWET, it is reset to EVPWET and IEVAPC is set to zero. \f$Q_{E,c}\f$ is calculated from \f$E_c\f$ and \f$\Delta Q_{S,c}\f$ is
!!obtained as
!!
!!\f$\Delta Q_{S,c} = C_c [T_c - T_{c,o} ]/ \Delta t\f$
!!
!!where \f$C_c\f$ is the canopy heat capacity, \f$T_{c,o}\f$ is the canopy temperature from the previous time step and \f$\Delta t\f$ is
!!the length of the time step. The residual RESID of the energy balance is evaluated on the basis of the
!!current estimation for the canopy temperature TCAN. If the absolute value of RESID is less than \f$5.0 W
!!m^{-2}\f$ , or if the absolute value of the iteration step TSTEP most recently used is less than 0.01 K, the surface
!!temperature is deemed to have been found and ITER is set to 0. If the iteration counter NITER is equal
!!to the maximum number and ITER is still 1, ITER is set to -1.
!!
!!In the following section, the iteration sequence is moved ahead a step. If ITC = 1, the calculations for the
!!bisection method of solution in loop 550 are performed, over each of the modelled areas for which ITER
!!= 1. If NITER = 1 (indicating that this is the first step in the iteration), then if RESID > 0 (indicating
!!that the current value for TCAN had undershot the correct value), TCAN is incremented by 1 K;
!!otherwise it is decremented by 1 K. If this is not the first step in the iteration, then if RESID >0 and
!!TSTEP < 0 (indicating that TCAN has undershot the correct value and the last temperature increment
!!had been a negative one) or if RESID < 0 and TSTEP > 0 (indicating that TCAN has overshot the
!!correct value and the last temperature increment had been a positive one), TSTEP is divided in half and
!!its sign changed. TSTEP is then added to TCAN. If TCAN is vanishingly close to 0 C, it is reset to that
!!value. The iteration counter NITER and the flag NUMIT are each incremented by one. Finally, if
!!NUMIT > 0, the iteration cycle is repeated from line 400 on.
!!
!!If ITC = 2, the calculations for the Newton-Raphson method of iteration in loop 575 are performed, over
!!each of the modelled areas for which ITER = 1. As outlined above, in this approach the value \f$x_{n+1}\f$ used
!!at each iteration step is obtained from the value \f$x_n\f$ at the previous step as follows:
!!
!!\f$x_{n+1} = x_n - f(x_n )/f'(x_n )\f$
!!
!!Identifying \f$x_n\f$ with TCAN and \f$f(x_n )\f$ with the surface energy balance equation, it can be seen that the
!!second term on the right-hand side corresponds to TSTEP; the numerator is equal to RESID and the
!!denominator to the first derivative of the energy balance equation evaluated at TCAN, which in turn is
!!equal to the sum of the derivatives of the individual terms:
!!
!!\f$d(L_{*c} )/dT = -8 \sigma T_c^3 (1 - \chi)\f$
!!
!!\f$d(Q_{H,c} )/dT = \rho_a c_p {1/r_A + [T_c - T_{a,pot} ] d(1/r_A )/dT}\f$
!!
!!\f$d(Q_{E,c} )/dT = L_v \rho_a {X_E dq_c /dT + [q_c - q_a ] dX_E /dT}\f$
!!
!!\f$d \Delta Q_{S,c} /dT = C_c / \Delta t\f$
!!
!!The term \f$d(1/r_A )/dT\f$ is represented by the variable DCFLXM, which is approximated as the difference
!!between CFLX and its value for the previous iteration, CFLUXM, divided by TSTEP. The term \f$dX_E /dT\f$
!!is represented by the variable DXEVAP, which is approximated as the difference between XEVAP and
!!its value for the previous iteration, XEVAPM, divided by TSTEP. The calculated value of TSTEP
!!obtained from the above calculations is constrained to be between -10 and 5 K to dampen any spurious
!!oscillations, and is then added to TCAN. If the resulting value of TCAN is vanishingly close to 0 C, it is
!!reset to that value. At the end of the calculations the iteration counter NITER and the flag NUMIT are
!!each incremented by one. The values of \f$T_{a,c}\f$ , \f$q_{a,c}\f$ and \f$T_{ac,v}\f$ are reset to \f$T_c\f$ , \f$q_c\f$ and \f$T_{c,v}\f$ respectively. Upon
!!exiting the loop, if NUMIT > 0, the iteration cycle is repeated from line 400 on.
!!
        DO 525 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
              QSENSC(I)=RHOAIR(I)*SPHAIR*CFSENS(I)*(TCAN(I)-TAC(I))
              IF(FRAINC(I).GT.0. .OR. FSNOWC(I).GT.0. .OR.
     1           RC(I).LE.5000. .OR. QAC(I).GT.QCAN(I))       THEN

                  EVAPC(I)=RHOAIR(I)*CFEVAP(I)*(QCAN(I)-QAC(I))
                  IEVAPC(I)=1
              ELSE
                  EVAPC(I)=0.0
                  IEVAPC(I)=0
                  QCAN(I)=QA(I)
              ENDIF
              IF(EVAPC(I).LT.0. .AND. TCAN(I).GT.TADP(I)) EVAPC(I)=0.0
              IF(SNOCAN(I).GT.0.)                            THEN
                  EVPWET(I)=SNOCAN(I)/DELT
                  IF(EVAPC(I).GT.EVPWET(I)) EVAPC(I)=EVPWET(I)
              ELSE
                  EVPWET(I)=RAICAN(I)/DELT
                  IF(EVAPC(I).GT.EVPWET(I)) THEN  
                      WTRANSP=(EVAPC(I)-EVPWET(I))*DELT
                      EVPMAX(I)=EVPWET(I)
                      WTRTOT(I)=0.0
                      DO J=1,IG
                          WTEST=WTRANSP*FROOT(I,J)
                          WROOT(I,J)=MIN(WTEST,WAVAIL(I,J))
                          WTRTOT(I)=WTRTOT(I)+WROOT(I,J)
                          EVPMAX(I)=EVPMAX(I)+WROOT(I,J)/DELT
                      ENDDO
                      IF(EVAPC(I).GT.EVPMAX(I)) EVAPC(I)=EVPMAX(I)
                  ENDIF
              ENDIF
              QEVAPC(I)=CPHCHC(I)*EVAPC(I)
              QSTOR (I)=CHCAP(I)*(TCAN(I)-TCANO(I))/DELT
              RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-2.0*QLWOC(I))*
     1             (1.0-FSVF(I))+QSGADD(I)-QSENSC(I)-QEVAPC(I)-
     2             QSTOR(I)-QMELTC(I)
              IF(ABS(RESID(I)).LT.5.0)                       ITER(I)=0
              IF(ABS(TSTEP(I)).LT. 1.0E-2)                   ITER(I)=0
              IF(NITER(I).EQ.ITERMX .AND. ITER(I).EQ.1)      ITER(I)=-1
          ENDIF
  525   CONTINUE

      IF(ITC.LT.2) THEN
C
C     * OPTION #1: SECANT/BISECTION ITERATION METHOD.
C
        DO 550 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              IF(NITER(I).EQ.1) THEN
                  IF(RESID(I).GT.0.0) THEN
                      TCAN(I)=TCAN(I)+TSTEP(I)
                  ELSE
                      TCAN(I)=TCAN(I)-TSTEP(I)
                  ENDIF
              ELSE
                  IF((RESID(I).GT.0. .AND. TSTEP(I).LT.0.) .OR.
     1                (RESID(I).LT.0. .AND. TSTEP(I).GT.0.))    THEN
                      TSTEP(I)=-TSTEP(I)/2.0
                  ENDIF
                  TCAN(I)=TCAN(I)+TSTEP(I)
              ENDIF
              IF(ABS(TCAN(I)-TFREZ).LT.1.0E-6)             TCAN(I)=TFREZ
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
          ENDIF
  550   CONTINUE
C
      ELSE
C
C     * OPTION #2: NEWTON-RAPHSON ITERATION METHOD.
C
        DO 575 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              IF(NITER(I).GT.1)                              THEN
                  DCFLUX=(CFLX(I)-CFLUXM(I))/
     1               SIGN(MAX(.001,ABS(TSTEP(I))),TSTEP(I))
                  IF(ABS(TVIRTA(I)-TVIRTC(I)).LT.0.4)
     1                DCFLUX=MAX(DCFLUX,0.8*DCFLXM(I))
                  DXEVAP=(XEVAP(I)-XEVAPM(I))/
     1                SIGN(MAX(.001,ABS(TSTEP(I))),TSTEP(I))
              ELSE
                  DCFLUX=0.
                  DXEVAP=0.
              ENDIF
              XEVAPM(I)=XEVAP(I)
              CFLUXM(I)=CFLX(I)
              DCFLXM(I)=DCFLUX
              DRDT0=-4.0*SBC*TCAN(I)*TCAN(I)*TCAN(I)*(1.0-FSVF(I))*
     1              2.0-RHOAIR(I)*SPHAIR*(CFLX(I)+MAX(0.,
     2              TCAN(I)-TPOTA(I))*DCFLUX)+REAL(IEVAPC(I))*CPHCHC(I)*
     3              RHOAIR(I)*(XEVAP(I)*WC(I)*A(I)*(B(I)-TFREZ)/
     4              ((TCAN(I)-B(I))*(1.0+WC(I)))**2-(QCAN(I)-QA(I))*
     5              DXEVAP)-CHCAP(I)/DELT
              TSTEP(I)=-RESID(I)/DRDT0
              TSTEP(I)=MAX(-10.,MIN(5.,TSTEP(I)))
              TCAN(I)=TCAN(I)+TSTEP(I)
              IF(ABS(TCAN(I)-TFREZ).LT.1.0E-3)             TCAN(I)=TFREZ
              NITER(I)=NITER(I)+1
              NUMIT=NUMIT+1
              TAC(I)=TCAN(I)
              QAC(I)=QCAN(I)
              TVRTAC(I)=TVIRTC(I)
          ENDIF
  575   CONTINUE
C
      ENDIF
C
      ENDIF
      IF(NUMIT.GT.0)                                    GO TO 400
C
C     * IF CONVERGENCE HAS NOT BEEN REACHED FOR ITERATION METHOD #2,
C     * CALCULATE TEMPERATURE AND FLUXES ASSUMING NEUTRAL STABILITY.

      IF(ITC.EQ.2) THEN
C
        NUMIT=0
!>
!!After the iteration has been completed, if the Newton-Raphson method has been used, a check is carried
!!out in loop 600 to ascertain whether convergence has not been reached (i.e. whether ITER = -1) for any
!!location. In such cases it is assumed that conditions of near-neutral stability at the surface are the cause of
!!the difficulty in finding a solution. The flags NUMIT and IEVAPC are set to zero, and a trial value of
!!TCAN is calculated using the virtual potential temperature of the air and the canopy specific humidity. If
!!the absolute value of RESID is \f$> 100 W m^{-2}\f$ , TCAN is set to this trial value. The values of \f$q_{0,c} and the
!!components of the surface energy balance are recalculated as above, except that \f$Q_{H,c}\f$ and \f$Q_{E,c}\f$ are assumed
!!as a first approximation to be zero. RESID is determined on this basis, and is then assigned to \f$Q_{H,c}\f$ and
!!\f$Q_{E,c}\f$ . If RESID > 0, \f$Q_{E,c}\f$ is set to this value; otherwise it is equally divided between \f$Q_{H,c}\f$ and \f$Q_{E,c}\f$ . The
!!residual is then reset to zero, and E(0) and \f$T_{v,c}\f$ are recalculated. NUMIT is incremented by 1, and the flag
!!IEVAPC for the current location is set to 1.
!!
        DO 600 I=IL1,IL2
!          if (tvirta(i) .gt. 100.) then !flag! TEMP FIX! JM July 8 2013.
          IEVAPC(I)=0
          IF(ITER(I).EQ.-1)                   THEN
            TCANT=TVIRTA(I)/(1.0+0.61*QCAN(I))
            IF(ABS(RESID(I)).GT.100.)  THEN
               TCAN(I)=TCANT
               IF(TCAN(I).GE.TFREZ)                         THEN
                  A(I)=17.269
                  B(I)=35.86
               ELSE
                  A(I)=21.874
                  B(I)=7.66
               ENDIF
               WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1             (TCAN(I)-B(I)))/PADRY(I)
               QCAN(I)=WCAN/(1.0+WCAN)
               IF(FSNOWC(I).GT.0.0)               THEN
                   YEVAP=FRAINC(I)+FSNOWC(I)
               ELSE
                   YEVAP=FRAINC(I)+(1.0-FRAINC(I))*10./(10.+RC(I))
               ENDIF
               QCAN(I)=YEVAP*QCAN(I)+(1.0-YEVAP)*QA(I)
               QSTOR(I)=CHCAP(I)*(TCAN(I)-TCANO(I))/DELT
               QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
               RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-2.0*QLWOC(I))*
     1             (1.0-FSVF(I))+QSENSG(I)-QSTOR(I)
               IF(RESID(I).GT.0.) THEN
                   QEVAPC(I)=RESID(I)
               ELSE
                   QEVAPC(I)=RESID(I)*0.5
               ENDIF
               QSENSC(I)=RESID(I)-QEVAPC(I)
               RESID(I)=0.
               EVAPC(I)=QEVAPC(I)/CPHCHC(I)
               TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
               NUMIT=NUMIT+1
               IEVAPC(I)=1
            ENDIF
          ENDIF
!         end if
  600   CONTINUE
!>
!!After loop 600, calls to DRCOEF or FLXSURFZ are performed to re-evaluate the surface turbulent
!!transfer coefficients for any locations for which the fluxes were modified in the previous loop, i.e. for any
!!locations for which IEVAPC was set to 1. After this a check is performed for unphysical values of the
!!canopy temperature, i.e. for values greater than 100 C or less than -100 C. If such values are encountered,
!!an error message is printed and a call to abort is carried out.
!!
!!Next a check is carried out to determine whether freezing or melting of intercepted water has occurred
!!over the current time step. If this is the case, adjustments are required to the canopy temperature, the
!!intercepted liquid and frozen water amounts and fractional coverages, and to \f$C_c\f$ and \f$\Delta Q_{S,c}\f$ . In loop 650, if
!!there is liquid water stored on the canopy and the canopy temperature is less than 0 C, the first half of the
!!adjustment to \f$\Delta Q_{S,c}\f$ is performed, and the flags ITER and NIT are set to 1. The available energy sink
!!HFREZ is calculated from CHCAP and the difference between TCAN and 0 C, and compared with
!!HCONV, calculated as the energy sink required to freeze all of the liquid water on the canopy. If
!!HFREZ \f$\leq\f$ HCONV, the amount of water that can be frozen is calculated using the latent heat of melting.
!!The fractional coverages of frozen and liquid water FSNOWC and FRAINC and their masses SNOCAN
!!and RAICAN are adjusted accordingly, TCAN is set to 0 C, and the amount of energy involved is stored
!!in the diagnostic variable QMELTC. Otherwise all of the intercepted liquid water is converted to frozen
!!water, and the energy available for cooling the canopy is calculated as HCOOL = HFREZ - HCONV.
!!This available energy is applied to decreasing the temperature of the canopy, using the specific heat of the
!!canopy elements, and the amount of energy that was involved in the phase change is stored in the
!!diagnostic variable QMELTC. In both cases \f$q_c\f$ and \f$T_{c,v}\f$ are recalculated, and at the end of the loop \f$C_c\f$ and
!!\f$\Delta Q_{S,c}\f$ are re-evaluated.
!!
c
      IF(NUMIT.GT.0) THEN
         IF(ISLFD.LT.2) THEN
            CALL DRCOEF(CDM,CDH,RIB,CFLUX,QA,QA,ZOSCLM,ZOSCLH,
     1                  CRIB,TVIRTC,TVIRTA,VA,FI,IEVAPC,
     2                  ILG,IL1,IL2)
         ELSE
            CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TCAN,QCAN,H,ZOM,ZOH,
     3                    LZZ0,LZZ0T,FM,FH,ILG,IL1,IL2,FI,IEVAPC,JL )
         ENDIF
      ENDIF
C
      ENDIF
C
      IBAD=0
C
      DO 625 I=IL1,IL2
C         IF(FI(I).GT.0. .AND. ITER(I).EQ.-1)                      THEN
C             WRITE(6,6350) I,JL,NITER(I),RESID(I),TCAN(I),RIB(I)
C6350         FORMAT('0CANOPY ITERATION LIMIT',3X,3I3,3(F8.2,E12.4))
C         ENDIF
          IF(FI(I).GT.0. .AND. (TCAN(I).LT.173.16 .OR.
     1                           TCAN(I).GT.373.16))                THEN
              IBAD=I
          ENDIF
  625 CONTINUE
C
      IF(IBAD.NE.0)                                                 THEN
          WRITE(6,6375) IBAD,JL,TCAN(IBAD),NITER(IBAD),ISNOW
 6375     FORMAT('0BAD CANOPY ITERATION TEMPERATURE',3X,2I3,F16.2,2I4)
          WRITE(6,6380) QSWNC(IBAD),QLWIN(IBAD),QLWOG(IBAD),
     1                  QLWOC(IBAD),QSENSG(IBAD),QSENSC(IBAD),
     2                  QEVAPC(IBAD),QSTOR(IBAD),QMELTC(IBAD)
          WRITE(6,6380) TCAN(IBAD),TPOTA(IBAD),TZERO(IBAD)
 6380     FORMAT(2X,9F10.2)
         CALL XIT('TSOLVC',-2)
      ENDIF
C
C     * POST-ITERATION CLEAN-UP.
C
      NIT=0
      DO 650 I=IL1,IL2
          IF(FI(I).GT.0.) THEN
              IF(RAICAN(I).GT.0. .AND. TCAN(I).LT.TFREZ)      THEN
                  QSTOR(I)=-CHCAP(I)*TCANO(I)/DELT
                  ITER(I)=1
                  NIT=NIT+1
                  HFREZ=CHCAP(I)*(TFREZ-TCAN(I))
                  HCONV=RAICAN(I)*CLHMLT
                  IF(HFREZ.LE.HCONV)                       THEN
                     RCONV=HFREZ/CLHMLT
                     FSNOWC(I)=FSNOWC(I)+FRAINC(I)*RCONV/RAICAN(I)
                     FRAINC(I)=FRAINC(I)-FRAINC(I)*RCONV/RAICAN(I)
                     SNOCAN(I)=SNOCAN(I)+RCONV
                     RAICAN(I)=RAICAN(I)-RCONV
                     TCAN  (I)=TFREZ
                     QMELTC(I)=-CLHMLT*RCONV/DELT
                     WCAN=0.622*611.0/PADRY(I)
                     QCAN(I)=WCAN/(1.0+WCAN)
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                  ELSE
                     HCOOL=HFREZ-HCONV
                     SNOCAN(I)=SNOCAN(I)+RAICAN(I)
                     FSNOWC(I)=FSNOWC(I)+FRAINC(I)
                     FRAINC(I)=0.0
                     TCAN  (I)=-HCOOL/(SPHVEG*CMASS(I)+SPHICE*
     1                         SNOCAN(I))+TFREZ
                     QMELTC(I)=-CLHMLT*RAICAN(I)/DELT
                     RAICAN(I)=0.0
                     A(I)=21.874
                     B(I)=7.66
                     WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1                    (TCAN(I)-B(I)))/PADRY(I)
                     QCAN(I)=WCAN/(1.0+WCAN)
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                  ENDIF
                  CHCAP(I)=SPHVEG*CMASS(I)+SPHICE*SNOCAN(I)+
     1                     SPHW*RAICAN(I)
                  QSTOR(I)=QSTOR(I)+CHCAP(I)*TCAN(I)/DELT
              ELSE
                  ITER(I)=0
              ENDIF
              IF(ITC.EQ.2) THEN
                  TAC(I)=TCAN(I)
                  QAC(I)=QCAN(I)
                  TVRTAC(I)=TVIRTC(I)
              ENDIF
          ENDIF
  650 CONTINUE
C
!>
!!In loop 675, if there is frozen water stored on the canopy and the canopy temperature is greater than 0 C,
!!the first half of the adjustment to \f$\Delta Q_{S,c}\f$ is performed, and the flags ITER and NIT are set to 1. The
!!available energy for melting, HMELT, is calculated from CHCAP and the difference between TCAN and
!!0 C, and compared with HCONV, calculated as the energy required to melt all of the frozen water on the
!!canopy. If HMELT \f$\leq\f$ HCONV, the amount of frozen water that can be melted is calculated using the
!!latent heat of melting. The fractional coverages of frozen and liquid water FSNOWC and FRAINC and
!!their masses SNOCAN and RAICAN are adjusted accordingly, TCAN is set to 0 C, and the amount of
!!energy involved is stored in the diagnostic variable QMELTC. Otherwise, all of the intercepted frozen
!!water is converted to liquid water, and the energy available for warming the canopy is calculated as
!!HWARM = HMELT - HCONV. This available energy is applied to increasing the temperature of the
!!canopy, using the specific heats of the canopy elements, and the amount of energy that was involved in
!!the phase change is stored in the diagnostic variable QMELTC. In both cases \f$q_c\f$ and \f$T_{c,v}\f$ are recalculated,
!!and at the end of the loop \f$C_c\f$ and \f$\Delta Q_{S,c}\f$ are re-evaluated.
!!
      DO 675 I=IL1,IL2
          IF(FI(I).GT.0.) THEN
              IF(SNOCAN(I).GT.0. .AND. TCAN(I).GT.TFREZ)    THEN
                  QSTOR(I)=-CHCAP(I)*TCANO(I)/DELT
                  ITER(I)=1
                  NIT=NIT+1
                  HMELT=CHCAP(I)*(TCAN(I)-TFREZ)
                  HCONV=SNOCAN(I)*CLHMLT
                  IF(HMELT.LE.HCONV)                       THEN
                     SCONV=HMELT/CLHMLT
                     FRAINC(I)=FRAINC(I)+FSNOWC(I)*SCONV/SNOCAN(I)
                     FSNOWC(I)=FSNOWC(I)-FSNOWC(I)*SCONV/SNOCAN(I)
                     SNOCAN(I)=SNOCAN(I)-SCONV
                     RAICAN(I)=RAICAN(I)+SCONV
                     TCAN  (I)=TFREZ
                     QMELTC(I)=CLHMLT*SCONV/DELT
                     WCAN=0.622*611.0/PADRY(I)
                     QCAN(I)=WCAN/(1.0+WCAN)
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                  ELSE
                     HWARM=HMELT-HCONV
                     RAICAN(I)=RAICAN(I)+SNOCAN(I)
                     FRAINC(I)=FRAINC(I)+FSNOWC(I)
                     FSNOWC(I)=0.0
                     TCAN  (I)=HWARM/(SPHVEG*CMASS(I)+SPHW*
     1                         RAICAN(I))+TFREZ
                     QMELTC(I)=CLHMLT*SNOCAN(I)/DELT
                     SNOCAN(I)=0.0
                     A(I)=17.269
                     B(I)=35.86
                     WCAN=0.622*611.0*EXP(A(I)*(TCAN(I)-TFREZ)/
     1                    (TCAN(I)-B(I)))/PADRY(I)
                     QCAN(I)=WCAN/(1.0+WCAN)
                     TVIRTC(I)=TCAN(I)*(1.0+0.61*QCAN(I))
                  ENDIF
                  CHCAP(I)=SPHVEG*CMASS(I)+SPHW*RAICAN(I)+
     1                     SPHICE*SNOCAN(I)
                  QSTOR(I)=QSTOR(I)+CHCAP(I)*TCAN(I)/DELT
              ENDIF
              IF(ITC.EQ.2) THEN
                  TAC(I)=TCAN(I)
                  QAC(I)=QCAN(I)
                  TVRTAC(I)=TVIRTC(I)
              ENDIF
          ENDIF
  675 CONTINUE
C
      IF(NIT.GT.0)                                         THEN
C
C     * CALCULATE SURFACE DRAG COEFFICIENTS (STABILITY-DEPENDENT)
C     * AND OTHER RELATED QUANTITIES BETWEEN CANOPY AIR SPACE AND
C     * ATMOSPHERE.
C
        IF(ISLFD.LT.2) THEN
            CALL DRCOEF(CDM,CDH,RIB,CFLUX,QAC,QA,ZOSCLM,ZOSCLH,
     1                  CRIB,TVRTAC,TVIRTA,VA,FI,ITER,
     2                  ILG,IL1,IL2)
        ELSE
            CALL FLXSURFZ(CDM,CDH,CFLUX,RIB,FTEMP,FVAP,ILMO,
     1                    UE,FCOR,TPOTA,QA,ZRSLFM,ZRSLFH,VA,
     2                    TAC,QAC,H,ZOM,ZOH,
     3                    LZZ0,LZZ0T,FM,FH,ILG,IL1,IL2,FI,ITER,JL )
        ENDIF
      ENDIF
C
C     * REMAINING CALCULATIONS.
C
      IF(ITC.EQ.1) THEN
C
!>
!!For the locations over which melting or freezing of water on the canopy has occurred, the surface fluxes
!!must now be recalculated to reflect the modified canopy temperature and humidity. If NIT > 0, first
!!DRCOEF or FLXSURFZ is called to re-evaluate the surface turbulent transfer coefficients over all
!!locations where ITER has been set to 1. Loops 700 and 750 repeat the calculations done in loops 475
!!and 500, to obtain the surface transfer coefficients. Loop 800 repeats the calculations of the surface
!!fluxes done in loop 525.
!!
      DO 700 I=IL1,IL2
          IF (FI(I).GT.0. .AND. ITER(I).EQ.1)                THEN
              XEVAP(I)=RBINV(I)
              QAC(I)=(QCAN(I)*XEVAP(I)+QZERO(I)*RAGINV(I)+
     1            QA(I)*CFLUX(I))/(XEVAP(I)+RAGINV(I)+CFLUX(I))
              IF(QAC(I).LT.QCAN(I))                     THEN
                 IF(FSNOWC(I).GT.0.0)               THEN
                     XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RB(I)
                 ELSE
                     XEVAP(I)=FRAINC(I)/RB(I)+(1.0-FRAINC(I))/
     1                        (RB(I)+RC(I))
                     QAC(I)=(QCAN(I)*XEVAP(I)+QZERO(I)*RAGINV(I)+
     1                   QA(I)*CFLUX(I))/(XEVAP(I)+RAGINV(I)+
     2                   CFLUX(I))
                 ENDIF
              ELSE
                  IF(FSNOWC(I).GT.1.0E-5) THEN
                      XEVAP(I)=FSNOWC(I)/RB(I)
                  ELSE
                      XEVAP(I)=1.0/RB(I)
                  ENDIF
              ENDIF
              TAC(I)=(TCAN(I)*RBINV(I)+TPOTG(I)*RAGINV(I)+
     1            TPOTA(I)*CFLUX(I))/(RBINV(I)+RAGINV(I)+CFLUX(I))
              TVRTAC(I)=TAC(I)*(1.0+0.61*QAC(I))
              CFSENS(I)=RBINV(I)
              CFEVAP(I)=XEVAP(I)
          ENDIF
700   CONTINUE
C
      ELSE
C
      DO 750 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              CFLX(I)=RBINV(I)*CFLUX(I)/(RBINV(I)+CFLUX(I))
              CFLX(I)=CFLUX(I)+(CFLX(I)-CFLUX(I))*
     1            MIN(1.0,QSWINV(I)*0.04)
              RA(I)=1.0/CFLX(I)
              IF(QA(I).LT.QCAN(I))                     THEN
                 IF(FSNOWC(I).GT.0.0)               THEN
                     XEVAP(I)=(FRAINC(I)+FSNOWC(I))/RA(I)
                 ELSE
                     XEVAP(I)=FRAINC(I)/RA(I)+(1.0-FRAINC(I))/
     1                        (RA(I)+RC(I))
                 ENDIF
              ELSE
                  IF(FSNOWC(I).GT.1.0E-5) THEN
                      XEVAP(I)=FSNOWC(I)/RA(I)
                  ELSE
                      XEVAP(I)=1.0/RA(I)
                  ENDIF
              ENDIF
              QCAN(I)=RA(I)*XEVAP(I)*QCAN(I)+(1.0-RA(I)*
     1            XEVAP(I))*QA(I)
              CFSENS(I)=CFLX(I)
              CFEVAP(I)=CFLX(I)
              TAC(I)=TPOTA(I)
              QAC(I)=QA(I)
          ENDIF
750   CONTINUE
C
      ENDIF
C
      DO 800 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ITER(I).EQ.1)                       THEN
              IF(SNOCAN(I).GT.0.)             THEN
                  CPHCHC(I)=CLHVAP+CLHMLT
              ELSE
                  CPHCHC(I)=CLHVAP
              ENDIF
              QLWOC(I)=SBC*TCAN(I)*TCAN(I)*TCAN(I)*TCAN(I)
              QSENSC(I)=RHOAIR(I)*SPHAIR*CFSENS(I)*(TCAN(I)-TAC(I))
              IF(FRAINC(I).GT.0. .OR. FSNOWC(I).GT.0. .OR.
     1           RC(I).LE.5000. .OR. QAC(I).GT.QCAN(I))       THEN
                  EVAPC(I)=RHOAIR(I)*CFEVAP(I)*(QCAN(I)-QAC(I))
              ELSE
                  EVAPC(I)=0.0
              ENDIF
              IF(EVAPC(I).LT.0. .AND. TCAN(I).GE.TADP(I)) EVAPC(I)=0.0
              IF(SNOCAN(I).GT.0.)                            THEN
                  EVPWET(I)=SNOCAN(I)/DELT
                  IF(EVAPC(I).GT.EVPWET(I)) EVAPC(I)=EVPWET(I)
              ELSE
                  EVPWET(I)=RAICAN(I)/DELT
                  IF(EVAPC(I).GT.EVPWET(I)) THEN  
                      WTRANSP=(EVAPC(I)-EVPWET(I))*DELT
                      EVPMAX(I)=EVPWET(I)
                      WTRTOT(I)=0.0
                      DO J=1,IG
                          WTEST=WTRANSP*FROOT(I,J)
                          WROOT(I,J)=MIN(WTEST,WAVAIL(I,J))
                          WTRTOT(I)=WTRTOT(I)+WROOT(I,J)
                          EVPMAX(I)=EVPMAX(I)+WROOT(I,J)/DELT
                      ENDDO
                      IF(EVAPC(I).GT.EVPMAX(I)) EVAPC(I)=EVPMAX(I)
              ENDIF
              ENDIF                                                     
              QEVAPC(I)=CPHCHC(I)*EVAPC(I)
              RESID(I)=QSWNC(I)+(QLWIN(I)+QLWOG(I)-2.0*QLWOC(I))*
     1             (1.0-FSVF(I))+QSGADD(I)-QSENSC(I)-QEVAPC(I)-
     2             QSTOR(I)-QMELTC(I)
          ENDIF
  800 CONTINUE
C
      DO 850 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              IF(ABS(EVAPC(I)).LT.1.0E-8) THEN
                  RESID(I)=RESID(I)+QEVAPC(I)
                  EVAPC(I)=0.0
                  QEVAPC(I)=0.0
              ENDIF
              QSENSC(I)=QSENSC(I)+RESID(I)
              IF(ABS(TZERO(I)-TFREZ).LT.1.0E-3) THEN
                  QMELTG(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)-GZERO(I)
              ELSE
                  GZERO(I)=QSWNG(I)+FSVF(I)*QLWIN(I)+(1.0-FSVF(I))*
     1                QLWOC(I)-QLWOG(I)-QSENSG(I)-QEVAPG(I)
              ENDIF
              IF(EVAPC(I).LT.0.) THEN
                  IF(SNOCAN(I).GT.0.)             THEN
                      SNOCAN(I)=SNOCAN(I)-EVAPC(I)*DELT
                      QFCF(I)=QFCF(I)+FI(I)*EVAPC(I)
                      HTCC(I)=HTCC(I)-FI(I)*TCAN(I)*SPHICE*EVAPC(I)
                  ELSE
                      RAICAN(I)=RAICAN(I)-EVAPC(I)*DELT
                      QFCL(I)=QFCL(I)+FI(I)*EVAPC(I)
                      HTCC(I)=HTCC(I)-FI(I)*TCAN(I)*SPHW*EVAPC(I)
                  ENDIF
                  EVAP(I)=EVAP(I)+FI(I)*EVAPC(I)
                  EVAPC(I)=0.0
                  CHCAP(I)=SPHVEG*CMASS(I)+SPHICE*SNOCAN(I)+
     1                     SPHW*RAICAN(I)
              ENDIF
              QSWNET(I)=QSWNG(I)+QSWNC(I)+QTRANS(I)
              QLWOUT(I)=FSVF(I)*QLWOG(I)+(1.0-FSVF(I))*QLWOC(I)
              QSENS(I)=QSENSC(I)+QSENSG(I)-QSGADD(I)
              QEVAP(I)=QEVAPC(I)+QEVAPG(I)
              EVAPC(I)=EVAPC(I)/RHOW
              EVAPG(I)=EVAPG(I)/RHOW


              ITERCT(I,KF1(I),NITER(I))=ITERCT(I,KF1(I),NITER(I))+1
          ENDIF
  850 CONTINUE

      IF (ctem_on) THEN
C
C       * STORE AERODYNAMIC CONDUCTANCE FOR USE IN NEXT TIME STEP
C       * OVERWRITE OLDER NUMBERS ONLY WHEN FRACTION OF CANOPY
C       * OR FRACTION OF CANOPY OVER SNOW (AKA FI) IS > 0.
C
        DO 900 I = IL1, IL2
          IF(FI(I).GT.0.)                                          THEN
            CFLUXV(I) = CFLUX(I)
          ELSE
            CFLUXV(I) = CFLUXV_IN(I)
          ENDIF
  900   CONTINUE
      ENDIF
C
      DO 950 J=1,IG
      DO 950 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN 
              IF(WTRTOT(I).GT.0.0) FROOT(I,J)=WROOT(I,J)/WTRTOT(I)
          ENDIF                                                         
  950 CONTINUE
C
!>
!!At the end of the subroutine, some final diagnostic and clean-up calculations are performed. If the
!!evaporation rate from the canopy, EVAPC, is very small, it is added to the residual of the canopy energy
!!balance equation, RESID, and then reset to zero. The overall residual is added to the sensible heat flux
!!from the canopy. The energy balance of the surface underlying the canopy is then re-evaluated to take
!!into account the new value of the canopy longwave radiation. If the surface temperature is close to 0 C,
!!the residual of the equation is assigned to QMELTG, representing the energy associated with melting or
!!freezing of water at the surface; otherwise it is assigned to the ground heat flux. If the water vapour flux
!!is towards the canopy, the water sublimated or condensed is assigned to interception storage: to
!!SNOCAN if SNOCAN > 0 (for consistency with the definition of CPHCHC), and to RAICAN
!!otherwise. The diagnostic water vapour flux variables QFCF and QFCL, and the diagnostic change of
!!canopy heat storage HTCC, are updated accordingly, and the canopy heat capacity CHCAP is
!!recalculated; EVAPC is added to the diagnostic variable EVAP and then reset to zero. Finally, the net
!!shortwave radiation, outgoing longwave radiation, sensible and latent heat fluxes are calculated for the
!!whole canopy-ground surface ensemble; the evaporation rates are converted to \f$m s^{-1}\f$ ; and the iteration
!!counter ITERCT is updated for the level corresponding to the subarea type and the value of NITER.
!!
      RETURN
      END

