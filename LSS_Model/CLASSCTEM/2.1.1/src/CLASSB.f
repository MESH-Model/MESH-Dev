C>\file
!!Purpose: Assign thermal and hydraulic properties to soil layers based on sand/clay content, or soil type.
!!Also calculate permeable thickness of soil layers, and wet and dry surface albedo for mineral soils.
!!
      SUBROUTINE CLASSB(THPOR,THLRET,THLMIN,BI,PSISAT,GRKSAT,
     1                  THLRAT,HCPS,TCS,THFC,THLW,PSIWLT,
     2                  DELZW,ZBOTW,ALGWET,ALGDRY,
     +                  ALGWV,ALGWN,ALGDV,ALGDN,
     3                  SAND,CLAY,ORGM,SOCI,DELZ,ZBOT,SDEPTH,
     4                  ISAND,IGDR,NL,NM,IL1,IL2,IM,IG,IGRALB)

C
C     * JUN 24/15 - J. MELTON.  PASS IN IGRALB SO THAT WE CAN SKIP
C                               USING SOCI IF IGRALB IS 0.
C     * JAN 15/15 - D.VERSEGHY. CHANGE PSIWLT FOR MINERAL SOILS
C     *                         TO A CONSTANT VALUE OF 150 M.
C     *                         AND ADD NEW VARIABLE THLW.
C     * AUG 25/14 - M.LAZARE.   PASS IN NEW WET AND DRY SOIL
C     *                         BRIGHTNESS FIELDS FROM CLM.
C     * NOV 16/13 - M.LAZARE.   FINAL VERSION FOR GCM17:
C     *                         - REVERT BACK TO CLASS2.7
C     *                           SPECIFICATION FOR "ALGWET".
C     * NOV 11/11 - M.LAZARE.   - IMPLEMENT CTEM CHOICE OF
C     *                           ALGDRY DETERMINED BY ADDED
C     *                           PASSED SWITCH "ICTEMMOD".
C     * OCT 18/11 - M.LAZARE.   - REMOVE UNUSED "IORG".
C     *                         - CHANGE "THSAND", "THORG"
C     *                           AND "THFINE" FROM ARRAYS
C     *                           (INTERNAL ONLY) TO SCALAR.
C     *                         - IGDR NOW PASSED OUT TO BE
C     *                           USED IN GRINFL/GRDRAN/WEND.
C     *                         - PASS IN IL1 AND IL2 TO
C     *                           DEFINE LOOPS.
C     * OCT 08/11 - M.LAZARE.   ALGDRY CHANGED BACK TO FORMULA
C     *                         USED IN GCM15I (.0056->.0046).
C     * SEP 27/11 - D.VERSEGHY. CONSTRAIN DELZW TO BE >= 5 CMS
C     *                         TO AVOID PROBLEMATIC UNDERSHOOTS.
C     * AUG 25/11 - D.VERSEGHY. USE THFC FORMULATION FOR BOTTOM
C     *                         LAYER AT BOTTOM OF SOIL PERMEABLE
C     *                         DEPTH.
C     * DEC 23/09 - V.FORTIN.   REVISE CALCULATION OF THFC FOR
C     *                         BOTTOM LAYER IN MINERAL SOILS
C     *                         ACCORDING TO SOULIS ET AL. (2009).
C     * JAN 06/09 - D.VERSEGHY. REVERSE ORDER OF 200 AND 300 LOOPS.
C     * DEC 11/07 - D.VERSEGHY. CHANGE CALCULATION OF TCS FROM
C     *                         GEOMETRIC MEAN TO LINEAR MEAN.
C     * FEB 07/07 - D.VERSEGHY. SET THFC TO THLRET FOR ORGANIC SOILS;
C     *                         STREAMLINE SOME CALCULATIONS.
C     * SEP 15/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3 IN 300 LOOP.
C     * APR 06/05 - D.VERSEGHY. MOVE CALCULATION OF GRKTLD
C     *                         INTO GRINFL; REVISED CALCULATION
C     *                         OF ALGDRY (WITH M.LAZARE).
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * SEP 04/03 - D.VERSEGHY. PERMEABLE THICKNESS OF SOIL
C     *                         LAYERS CONSTRAINED TO >= 1 MM;
C     *                         PROTECT SENSITIVE CALCULATIONS
C     *                         AGAINST ROUNDOFF ERROR.
C     * JUN 28/02 - D.VERSEGHY. ASSIGN SOIL HYDROLOGICAL AND
C     *                         THERMAL PROPERTIES BASED ON
C     *                         SAND, CLAY AND ORGANIC MATTER
C     *                         CONTENT.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER NL,NM,IL1,IL2,IM,IG,I,J,M
C
C     * OUTPUT ARRAYS.
C
      REAL THPOR (NL,NM,IG) !<Pore volume \f$[m^3 m^{-3} ] ( \theta_p )\f$
      REAL THLRET(NL,NM,IG) !<Liquid water retention capacity for organic soil \f$[m^3 m^{-3} ] (\theta_{ret} )\f$
      REAL THLMIN(NL,NM,IG) !<Residual soil liquid water content remaining after freezing or evaporation \f$[m^3 m^{-3} ] (\theta_{min} )\f$
      REAL BI    (NL,NM,IG) !<Clapp and Hornberger empirical parameter [ ] (b)
      REAL PSISAT(NL,NM,IG) !<Soil moisture suction at saturation [m] \f$(\Psi_{sat} )\f$
      REAL GRKSAT(NL,NM,IG) !<Hydraulic conductivity of soil at saturation \f$[m s^{-1} ] (K_{sat} )\f$
      REAL THLRAT(NL,NM,IG) !<Fractional saturation of soil at half the saturated hydraulic conductivity [ ] \f$(f_{inf} )\f$
      REAL HCPS  (NL,NM,IG) !<Volumetric heat capacity of soil matter \f$[J m^{-3} K^{-1} ] (C_g )\f$
      REAL TCS   (NL,NM,IG) !<Thermal conductivity of soil \f$[W m^{-1} K^{-1} ] (\tau_g )\f$
      REAL THFC  (NL,NM,IG) !<Field capacity \f$[m^3 m^{-3} ] (\theta_{fc} )\f$
      REAL THLW  (NL,NM,IG) !<
      REAL PSIWLT(NL,NM,IG) !<Soil moisture suction at wilting point [m] \f$(\Psi_{wilt} )\f$
      REAL DELZW (NL,NM,IG) !<Thickness of permeable part of soil layer [m]
      REAL ZBOTW (NL,NM,IG) !<Depth of bottom of permeable part of soil layer [m]
      REAL ALGWET(NL,NM)    !<All-wave albedo of wet soil for modelled area [ ]
      REAL ALGDRY(NL,NM)    !<All-wave albedo of dry soil for modelled area [ ]
      REAL ALGWV (NL,NM)    !<
      REAL ALGWN (NL,NM)    !<
      REAL ALGDV (NL,NM)    !<
      REAL ALGDN (NL,NM)    !<
C
      INTEGER ISAND (NL,NM,IG) !<Sand content flag
      INTEGER IGDR  (NL,NM) !<Index of soil layer in which bedrock is encountered
C
C     * INPUT ARRAYS.
C
      REAL SAND  (NL,NM,IG) !<Percent sand content of soil layer [percent] \f$(X_{sand} )\f$
      REAL CLAY  (NL,NM,IG) !<Percent clay content of soil layer [percent] \f$(X_{clay} )\f$
      REAL ORGM  (NL,NM,IG) !<Percent organic matter content of soil layer [percent]
      REAL DELZ  (IG)       !<Thickness of soil layer [m]
      REAL ZBOT  (IG)       !<Depth of bottom of soil layer [m]
      REAL SDEPTH(NL,NM)    !<Permeable depth of soil column (depth to bedrock) [m] \f$(z_b )\f$
      REAL SOCI  (NL,NM)   !<
C
      INTEGER IGRALB !< IF IGRALB IS SET TO 0, THE WET AND DRY SOIL ALBEDOS ARE
                     !! CALCULATED ON THE BASIS OF SOIL TEXTURE.  IF IT IS SET TO 1,
                     !! THEY ARE ASSIGNED VALUES BASED ON THE NCAR CLM SOIL "COLOUR"  DATASET.
C
      REAL THPORG (3),      THRORG (3),      THMORG (3),
     1     BORG   (3),      PSISORG(3),      GRKSORG(3)
C
C     * TEMPORARY VARIABLES.
C
      REAL ALWV(20), ALWN(20), ALDV(20), ALDN(20)
C
      REAL VSAND,VORG,VFINE,VTOT,AEXP,ABC,THSAND,THFINE,THORG
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL TCW,TCICE,TCSAND,TCFINE,TCOM,TCDRYS,RHOSOL,RHOOM,
     1     HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPFIN,SPHW,SPHICE,SPHVEG,
     2     SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCFINE,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPFIN,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /CLASS5/ THPORG,THRORG,THMORG,BORG,PSISORG,GRKSORG
C
      DATA ALWV /0.25,0.23,0.21,0.20,0.19,0.18,0.17,0.16,0.15,0.14,0.13,
     1           0.12,0.11,0.10,0.09,0.08,0.07,0.06,0.05,0.04/
      DATA ALWN /0.50,0.46,0.42,0.40,0.38,0.36,0.34,0.32,0.30,0.28,0.26,
     1           0.24,0.22,0.20,0.18,0.16,0.14,0.12,0.10,0.08/
      DATA ALDV /0.36,0.34,0.32,0.31,0.30,0.29,0.28,0.27,0.26,0.25,0.24,
     1           0.23,0.22,0.20,0.18,0.16,0.14,0.12,0.10,0.08/
      DATA ALDN /0.61,0.57,0.53,0.51,0.49,0.48,0.45,0.43,0.41,0.39,0.37,
     1           0.35,0.33,0.31,0.29,0.27,0.25,0.23,0.21,0.16/
C---------------------------------------------------------------------
C

!>
!!In the first section of code, two integer flags are evaluated: IGDR, the index of the soil layer in which the
!!bottom of the soil permeable depth, \f$z_b\f$ , occurs; and ISAND, the value of the SAND variable for each soil
!!layer converted to an integer. ISAND is used throughout the CLASS code as a flag to determine which
!!branches of code to execute.
!!

      DO 50 M=1,IM
      DO 50 I=IL1,IL2
          IGDR(I,M)=1
50    CONTINUE
C
      DO 100 J=1,IG
      DO 100 M=1,IM
      DO 100 I=IL1,IL2
          ISAND (I,M,J)=NINT(SAND(I,M,J))
          IF(ISAND(I,M,J).GT.-3) IGDR(I,M)=J
100   CONTINUE
C
!>
!!In loop 200, calculations are done to determine the permeable thickness DELZW of each soil layer. If
!!the land cover is an ice sheet (indicated by an ISAND value of -4 in the top layer), the value of DELZW
!!in each layer is set to DELZ, the standard thickness of the corresponding thermal layer, and the soil flag is
!!set to -4. If the layer consists of rock, indicated by an ISAND value of -3, DELZW is set to 0. For soil
!!layers where \f$z_b\f$ occurs below the bottom of the layer, DELZW is set to DELZ; for layers where \f$z_b\f$ occurs
!!near the top of the layer, DELZW is set to 0 and ISAND is set to -3. For the soil layer containing \f$z_b\f$ ,
!!DELZW is set to the distance from the top of the layer to \f$z_b\f$ , and is further constrained to be \f$\geq\f$ 5 cm, to
!!avoid overshoots in the water movement calculations. For all layers, the distance to the bottom of its
!!respective permeable depth, ZBOTW, is set to the depth of the top of the layer plus DELZW. At the
!!end of the loop, if the soil is a mineral one, the wet and dry albedo values are calculated using simple
!!empirical functions derived from values given in Wilson and Henderson-Sellers (1985). (The dry albedo
!!calculation varies slightly depending on whether or not CTEM is being run.)
!!

      DO 200 M=1,IM
      DO 200 I=IL1,IL2
          DO 150 J=1,IG
              IF(ISAND(I,M,1).EQ.-4) THEN
                  DELZW(I,M,J)=DELZ(J)
                  ISAND(I,M,J)=-4
              ELSEIF(ISAND(I,M,J).EQ.-3) THEN
                  DELZW(I,M,J)=0.0
              ELSEIF(SDEPTH(I,M).GE.ZBOT(J)) THEN
                  DELZW(I,M,J)=DELZ(J)
              ELSEIF(SDEPTH(I,M).LT.(ZBOT(J)-DELZ(J)+0.025)) THEN
                  DELZW(I,M,J)=0.0
                  ISAND(I,M,J)=-3
              ELSE
                  DELZW(I,M,J)=MAX(0.05,(SDEPTH(I,M)-(ZBOT(J)-DELZ(J))))
              ENDIF
              ZBOTW(I,M,J)=MAX(0.0,ZBOT(J)-DELZ(J))+DELZW(I,M,J)
150       CONTINUE
          IF(SAND(I,M,1).GE.0.0) THEN
              ALGWET(I,M)=0.08+0.0022*SAND(I,M,1)
              ALGDRY(I,M)=MIN(0.14+0.0046*SAND(I,M,1),0.45)
              IF (IGRALB .NE. 0) THEN
                ALGWV(I,M)=ALWV(NINT(SOCI(I,M)))
                ALGWN(I,M)=ALWN(NINT(SOCI(I,M)))
                ALGDV(I,M)=ALDV(NINT(SOCI(I,M)))
                ALGDN(I,M)=ALDN(NINT(SOCI(I,M)))
              ELSE
                ALGWV(I,M)=0.0
                ALGWN(I,M)=0.0
                ALGDV(I,M)=0.0
                ALGDN(I,M)=0.0
              ENDIF
          ELSE
              ALGWET(I,M)=0.0
              ALGDRY(I,M)=0.0
              ALGWV(I,M)=0.0
              ALGWN(I,M)=0.0
              ALGDV(I,M)=0.0
              ALGDN(I,M)=0.0
          ENDIF
200   CONTINUE
C
!>
!!In loop 300, various thermal and hydraulic soil properties are assigned to each of the soil layers,
!!depending on soil type. Values of ISAND greater than zero indicate mineral soil. The pore volume \f$\theta_p\f$ ,
!!the saturated hydraulic conductivity \f$K_{sat}\f$ , and the soil moisture suction at saturation \f$\Psi\f$ sat are calculated from
!!the percentage sand content \f$X_{sand}\f$ , and the hydraulic parameter b is calculated from the percentage clay
!!content \f$X_{clay}\f$ , based on empirical relationships given in Cosby et al. (1984):
!!\f$\theta_p = (-0.126 X_{sand} +48.9)/100.0\f$
!!\f$b = 0.159 X_{clay} + 2.91\f$
!!\f$\Psi_{sat} = 0.01 exp(-0.0302 X_{sand} + 4.33)\f$
!!\f$K_{sat} = 7.0556 x 10 -6 exp(0.0352 X_{sand} - 2.035)\f$
!!
!!The fractional saturation of the soil at half the saturated hydraulic conductivity, \f$f_{inf}\f$ , is calculated by
!!inverting the Clapp and Hornberger (1978) expression relating hydraulic conductivity \f$K\f$ to liquid water
!!content of the soil \f$\theta_l\f$ :
!!\f$K = K_{sat} (\theta_l / \theta_p ) (2b + 3)\f$
!!
!!Thus,
!!\f$f_{inf} = 0.5 1/(2b+3)\f$
!!
!!The residual soil liquid water content remaining after evaporation or freezing, \f$\theta_{min}\f$ , and the liquid water
!!retention capacity, \f$\theta_{ret}\f$ , are both set for mineral soils to a textbook value of 0.04.
!!The volumetric sand, silt + clay and organic matter components of the soil matrix are derived by
!!converting the percent values to volume fractions. The overall volumetric heat capacity of the soil
!!material, \f$C_g\f$ , is then calculated as a weighted average:
!!\f$C_g = \Sigma (C_{sand} \theta_{sand} + C_{fine} \theta_{fine} + C_{org} \theta_{org} )/(1 - \theta_p )\f$
!!where the subscript "fine" refers to the silt and clay particles taken together. The thermal conductivity of
!!the soil material, \f$\tau_g\f$ , is likewise calculated as a weighted average over the thermal conductivities of the
!!components:
!!\f$\tau_g = \Sigma (\tau_{sand} \theta_{sand} + \tau_{fine} \theta_{fine} + \tau_{org} \theta_{org} )/(1 - \theta_p )\f$
!!
!!The field capacity \f$\theta_{fc}\f$ , that is, the liquid water content of the soil at which gravitational drainage effectively
!!ceases, is calculated by setting the expression for K above to a value of 0.1 mm \f$d^{-1}\f$ , and solving for the
!!liquid water content:
!!\f$\theta_{fc} = \theta_p (1.157 x 10^{-9} /K_{sat} )^{1/(2b + 3)}\f$
!!
!!The only exception is the field capacity of the lowest permeable layer in mineral soils (layer IGDR), which
!!is determined using an expression developed by Soulis et al. (2010), which takes into account the
!!permeable depth of the whole overlying soil:
!!\f$\theta_{fc} = \theta_p /(b-1) \bullet (\Psi_{sat} b/ z_b )^{1/b} \bullet [(3b+2)^{(b-1)/b} - (2b+2)^{(b-1)/b} ]\f$
!!
!!The soil moisture suction \f$\Psi_{wilt}\f$ at the wilting point (the liquid water content at which plant roots can no
!!longer draw water from the soil) is calculated from the saturated soil moisture suction using the Clapp and
!!Hornberger (1978) expression, with the liquid water content approximated as 0.5 \f$\theta_{fc}\f$ :
!!\f$\Psi_{wilt} = \Psi_{sat} (0.5 \theta_{fc} / \theta_p )^{-b}\f$
!!Organic soils are flagged with an ISAND value of -2. For these soils, the variables \f$\theta_p\f$ , b, \f$K_{sat}\f$ , \f$\Psi_{sat}\f$ , \f$\theta_{min}\f$ , and
!!\f$\theta_{ret}\f$ are assigned values based on the peat texture (fibric, hemic or sapric). These values are obtained from
!!the arrays in common block CLASS5 (see the CLASSBD documentation above). The current default is
!!to assume the first soil layer as fibric, the second as hemic, and any lower layers as sapric. The volumetric
!!heat capacity and thermal conductivity are set to textbook values for organic matter; the field capacity is
!!set equal to the retention capacity; \f$f_{inf}\f$ is obtained as above; and \f$\Psi_{wilt}\f$ is assume to apply at a liquid water
!!content of \f$\theta_{min}\f$ .
!!
!!In the cases of rock soils and ice sheets (respectively flagged with ISAND values of -3 and -4), all of the
!!above variables are set to zero except for the volumetric heat capacity and the thermal conductivity, which
!!are assigned values representative of rock or ice.
!!

      DO 300 J=1,IG
      DO 300 M=1,IM
      DO 300 I=IL1,IL2
          IF(ISAND(I,M,J).EQ.-4) THEN
              THPOR (I,M,J)=0.0
              THLRET(I,M,J)=0.0
              THLMIN(I,M,J)=0.0
              BI    (I,M,J)=0.0
              PSISAT(I,M,J)=0.0
              GRKSAT(I,M,J)=0.0
              THLRAT(I,M,J)=0.0
              HCPS(I,M,J)=HCPICE
              TCS(I,M,J)=TCICE
              THFC(I,M,J)=0.0
              THLW(I,M,J)=0.0
              PSIWLT(I,M,J)=0.0
          ELSEIF(ISAND(I,M,J).EQ.-3) THEN
              THPOR (I,M,J)=0.0
              THLRET(I,M,J)=0.0
              THLMIN(I,M,J)=0.0
              BI    (I,M,J)=0.0
              PSISAT(I,M,J)=0.0
              GRKSAT(I,M,J)=0.0
              THLRAT(I,M,J)=0.0
              HCPS(I,M,J)=HCPSND
              TCS(I,M,J)=TCSAND
              THFC(I,M,J)=0.0
              THLW(I,M,J)=0.0
              PSIWLT(I,M,J)=0.0
          ELSEIF(ISAND(I,M,J).EQ.-2) THEN
              THPOR (I,M,J)=THPORG(MIN(J,3))
              THLRET(I,M,J)=THRORG(MIN(J,3))
              THLMIN(I,M,J)=THMORG(MIN(J,3))
              BI    (I,M,J)=BORG(MIN(J,3))
              PSISAT(I,M,J)=PSISORG(MIN(J,3))
              GRKSAT(I,M,J)=GRKSORG(MIN(J,3))
              THLRAT(I,M,J)=0.5**(1.0/(2.0*BI(I,M,J)+3.0))
              HCPS(I,M,J)=HCPOM
              TCS(I,M,J)=TCOM
              THFC(I,M,J)=THLRET(I,M,J)
              THLW(I,M,J)=THLMIN(I,M,J)
              PSIWLT(I,M,J)=PSISAT(I,M,J)*(THLMIN(I,M,J)/
     1            THPOR(I,M,J))**(-BI(I,M,J))
          ELSEIF(SAND(I,M,J).GE.0.) THEN
              THPOR (I,M,J)=(-0.126*SAND(I,M,J)+48.9)/100.0
              THLRET(I,M,J)=0.04
              THLMIN(I,M,J)=0.04
              BI    (I,M,J)=0.159*CLAY(I,M,J)+2.91
              PSISAT(I,M,J)=0.01*EXP(-0.0302*SAND(I,M,J)+4.33)
              GRKSAT(I,M,J)=7.0556E-6*(EXP(0.0352*SAND(I,M,J)-2.035))
              THLRAT(I,M,J)=0.5**(1.0/(2.0*BI(I,M,J)+3.0))
              VSAND=SAND(I,M,J)/(RHOSOL*100.0)
              VORG=ORGM(I,M,J)/(RHOOM*100.0)
              VFINE=(100.0-SAND(I,M,J)-ORGM(I,M,J))/(RHOSOL*100.0)
              VTOT=VSAND+VFINE+VORG
              THSAND=(1.0-THPOR(I,M,J))*VSAND/VTOT
              THORG=(1.0-THPOR(I,M,J))*VORG/VTOT
              THFINE=1.0-THPOR(I,M,J)-THSAND-THORG
              HCPS(I,M,J)=(HCPSND*THSAND+HCPFIN*THFINE+
     1            HCPOM*THORG)/(1.0-THPOR(I,M,J))
              TCS(I,M,J)=(TCSAND*THSAND+TCOM*THORG+
     1            TCFINE*THFINE)/(1.0-THPOR(I,M,J))
              IF(J.NE.IGDR(I,M))                       THEN
                  THFC(I,M,J)=THPOR(I,M,J)*(1.157E-9/GRKSAT(I,M,J))**
     1                (1.0/(2.0*BI(I,M,J)+3.0))
              ELSE
                  AEXP=(BI(I,M,J)-1.0)/BI(I,M,J)
                  ABC=(3.0*BI(I,M,J)+2.0)**AEXP-
     1                (2.0*BI(I,M,J)+2.0)**AEXP
                  THFC(I,M,J)=(ABC*THPOR(I,M,J)/(BI(I,M,J)-1.0))*
     1                (PSISAT(I,M,J)*BI(I,M,J)/SDEPTH(I,M))**
     2                (1.0/BI(I,M,J))
              ENDIF
              PSIWLT(I,M,J)=150.0
              THLW(I,M,J)=THPOR(I,M,J)*(PSIWLT(I,M,J)/PSISAT(I,M,J))**
     1                    (-1.0/BI(I,M,J))

          ENDIF
300   CONTINUE
C
      RETURN
      END
