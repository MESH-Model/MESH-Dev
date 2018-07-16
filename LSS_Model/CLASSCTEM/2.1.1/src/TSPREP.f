!>\file
C!Calculate coefficients for solution of snow pack heat conduction.
C!
      SUBROUTINE TSPREP(GCOEFFS,GCONSTS,CPHCHG,IWATER,
     1                  FI,ZSNOW,TSNOW,TCSNOW,
     2                  ILG,IL1,IL2,JL      )
C
C     * AUG 16/06 - D.VERSEGHY. MAJOR REVISION TO IMPLEMENT THERMAL
C     *                         SEPARATION OF SNOW AND SOIL.
C     * MAY 24/06 - D.VERSEGHY. LIMIT DELZ3 TO <= 4.1 M.
C     * OCT 04/05 - D.VERSEGHY. USE THREE-LAYER TBAR,TCTOP,TCBOT.
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 06/02 - D.VERSEGHY. SHORTENED CLASS3 COMMON BLOCK.
C     * JUN 17/02 - D.VERSEGHY. USE NEW LUMPED SOIL AND PONDED WATER
C     *                         TEMPERATURE FOR FIRST LAYER; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         INCORPORATE EXPLICITLY CALCULATED
C     *                         THERMAL CONDUCTIVITIES AT TOPS AND 
C     *                         BOTTOMS OF SOIL LAYERS.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * NOV 28/94 - M. LAZARE.  CLASS - VERSION 2.3.
C     *                         TCSATW,TCSATI DECLARED REAL(16).
C     * APR 10/92 - M. LAZARE.  CLASS - VERSION 2.1.
C     *                         DIVIDE PREVIOUS SUBROUTINE "T4LAYR" 
C     *                         INTO "TSPREP" AND "TSPOST" AND
C     *                         VECTORIZE.
C     * APR 11/89 - D.VERSEGHY. CALCULATE COEFFICIENTS FOR GROUND HEAT
C     *                         FLUX, EXPRESSED AS A LINEAR FUNCTION OF
C     *                         SURFACE TEMPERATURE. COEFFICIENTS ARE
C     *                         CALCULATED FROM LAYER TEMPERATURES, 
C     *                         THICKNESSES AND THERMAL CONDUCTIVITIES,
C     *                         ASSUMING A QUADRATIC VARIATION OF
C     *                         TEMPERATURE WITH DEPTH WITHIN EACH
C     *                         SOIL/SNOW LAYER. SET THE SURFACE 
C     *                         LATENT HEAT OF VAPORIZATION OF WATER
C     *                         AND THE STARTING TEMPERATURE FOR THE
C     *                         ITERATION IN "TSOLVC"/"TSOLVE".
C
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I,J
C
C     * OUTPUT ARRAYS.
C
      REAL GCOEFFS(ILG)     !<Multiplier used in equation relating snow surface heat flux to snow surface
                            !<temperature \f$[W m^{-2} K^{-1}]\f$
      REAL GCONSTS(ILG)     !<Intercept used in equation relating snow surface heat flux to snow surface
                            !<temperature \f$[W m^{-2} ]\f$
      REAL CPHCHG(ILG)      !<Latent heat of sublimation \f$[J kg^{-1}]\f$

C
      INTEGER IWATER(ILG)   !<Flag indicating condition of surface (dry, water-covered or snow-covered)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on modelled area [ ]
      REAL ZSNOW (ILG)  !<Depth of snow pack \f$[m] (\Delta z_s)\f$
      REAL TSNOW (ILG)  !<Snowpack temperature \f$[K] (T_s(\Delta z_s))\f$
      REAL TCSNOW(ILG)  !<Thermal conductivity of snow \f$[W m^{-1} K^{-1}]\f$
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL TCW      !<Thermal conductivity of water \f$(0.57) [W m^{-1} K^{-1}]\f$
      REAL TCICE    !<Thermal conductivity of ice \f$(2.24) [W m^{-1} K^{-1}]\f$
      REAL TCSAND   !<Thermal conductivity of sand particles \f$(2.5) [W m^{-1} K^{-1}]\f$
      REAL TCCLAY   !<Thermal conductivity of fine mineral particles \f$(2.5) [W m^{-1} K^{-1}]\f$
      REAL TCOM     !<Thermal conductivity of organic matter \f$(0.25) [W m^{-1} K^{-1}]\f$
      REAL TCDRYS   !<Thermal conductivity of dry mineral soil \f$(0.25) [W m^{-1} K^{-1}]\f$
      REAL RHOSOL   !<Density of soil mineral matter \f$(2.65 * 10^3) [kg m^{-3}]\f$
      REAL RHOOM    !<Density of soil organic matter \f$(1.30 * 10^3) [kg m^{-3}]\f$
      REAL HCPW     !<Volumetric heat capacity of water \f$(4.187 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPICE   !<Volumetric heat capacity of ice \f$(1.9257 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSOL   !<Volumetric heat capacity of mineral matter \f$(2.25 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPOM    !<Volumetric heat capacity of organic matter \f$(2.50 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSND   !<Volumetric heat capacity of sand particles \f$(2.13 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPCLY   !<Volumetric heat capacity of fine mineral particles \f$(2.38 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL SPHW     !<Specific heat of water \f$(4.186 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHICE   !<Specific heat of ice \f$(2.10 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHVEG   !<Specific heat of vegetation matter \f$(2.70 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHAIR   !<Specific heat of air \f$[J kg^{-1} K^{-1}]\f$
      REAL RHOW     !<Density of water \f$(1.0 * 10^3) [kg m^{-3}]\f$
      REAL RHOICE   !<Density of ice \f$(0.917 * 10^3) [kg m^{-3}]\f$
      REAL TCGLAC   !<Thermal conductivity of ice sheets \f$(2.24) [W m^{-1} K^{-1}]\f$
      REAL CLHMLT   !<Latent heat of freezing of water \f$(0.334 * 10^6) [J kg^{-1}]\f$
      REAL CLHVAP   !<Latent heat of vaporization of water \f$(2.501 * 10^6) [J kg^{-1}]\f$
C
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C>
C!   In this subroutine, coefficients are derived for an equation 
C!   relating the heat flux at the snow surface to the snow surface 
C!   temperature. It is assumed that the variation of temperature T with 
C!   depth z in the snow pack can be modelled by using a quadratic 
C!   equation:
C!
C!   \f$T(z) = (1/2) a z^2 + b z + c\f$
C!
C!   By substituting 0 for z in the above equation and in the expressions 
C!   for its first and second derivatives, it can be shown that a = 
C!   T"(0), b = T'(0), and c = T(0). The term T"(0) can be evaluated from 
C!   the expression for the first derivative evaluated at the bottom of 
C!   the snow pack, \f$T(\Delta z_s)\f$:
C!
C!   \f$T"(0) = [T'(\Delta z_s) - T'(0)]/ \Delta z_s\f$
C!
C!   The temperature gradient T'(0) at the snow surface is related to the 
C!   surface heat flux G(0) by the snow
C!   thermal conductivity \f$\lambda_s\f$:
C!
C!   \f$G(0) = -\lambda_s T'(0)\f$
C!
C!   The average snow temperature, \f$T_s(\Delta z_s)\f$, can be obtained by integrating 
C!   the resulting equation for T(z) between 0 and \f$\Delta z_s\f$. Making use of 
C!   all of the above expressions, and assuming as a first approximation 
C!   that the heat flux at the bottom of the snow pack is zero, a linear 
C!   equation can be derived relating G(0) to T(0):
C!
C!   \f$G(0) = 3 \lambda_s / \Delta z_s [T(0) - T_s(\Delta z_s)]\f$
C!
C!   Just four calculations are performed in this subroutine. The slope 
C!   and intercept of the G(0) vs. T(0) relation, GCOEFF and GCONST, are 
C!   evaluated as \f$3 \lambda_s / \Delta z_s\f$ and \f$-3 \lambda_s T_s(\Delta z_s)/ \Delta z_s\f$ 
C!   respectively; the flag IWATER is set to 2, indicating a snow 
C!   surface; and the latent heat of vaporization at the surface, 
C!   CPHCHG, is set to the value for sublimation (by adding the latent 
C!   heat of melting to the latent heat of vaporization).
C!
C!     * CALCULATE COEFFICIENTS.
C!
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              GCOEFFS(I)=3.0*TCSNOW(I)/ZSNOW(I)
              GCONSTS(I)=-3.0*TCSNOW(I)*TSNOW(I)/ZSNOW(I)
              IWATER(I)=2                                                                    
              CPHCHG(I)=CLHVAP+CLHMLT
          ENDIF
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END
