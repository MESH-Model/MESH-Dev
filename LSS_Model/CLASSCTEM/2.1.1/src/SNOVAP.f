!>\file
!>Purpose: Sublimation calculations for the snow pack on the ground.
!>
      SUBROUTINE SNOVAP(RHOSNO,ZSNOW,HCPSNO,TSNOW,EVAP,QFN,QFG,HTCS,
     1                  WLOST,TRUNOF,RUNOFF,TOVRFL,OVRFLW,
     2                  FI,R,S,RHOSNI,WSNOW,ILG,IL1,IL2,JL)
C
C     * AUG 25/11 - D.VERSEGHY. CORRECT CALCULATION OF TRUNOF
C     *                         AND TOVRFL.
C     * FEB 22/07 - D.VERSEGHY. NEW ACCURACY LIMITS FOR R AND S.
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 26/02 - D.VERSEGHY. CHANGE RHOSNI FROM CONSTANT TO
C     *                         VARIABLE.
C     * APR 11/01 - M.LAZARE.   CHECK FOR EXISTENCE OF SNOW BEFORE
C     *                         PERFORMING CALCULATIONS.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 16/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         INCORPORATE DIAGNOSTIC ARRAY "WLOST". 
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         ADDITIONAL DIAGNOSTIC CALCULATION -
C     *                         UPDATE HTCS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. SUBLIMATION FROM SNOWPACK.
C                                          
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL RHOSNO(ILG) !<Density of snow pack \f$[kg m^{-3}] (\rho_s)\f$   
      REAL ZSNOW (ILG) !<Depth of snow pack \f$[m] (z_g)\f$ 
      REAL HCPSNO(ILG) !<Heat capacity of snow pack \f$[J m^{-3} K^{-1}] (C_s)\f$ 
      REAL TSNOW (ILG) !<Temperature of the snow pack \f$[C] (T_s)\f$
      REAL EVAP  (ILG) !<Sublimation rate from snow surface at start of subroutine \f$[m s^{-1}]\f$
      REAL QFN   (ILG) !<Sublimation from snow pack \f$[kg m^{-2} s^{-1}]\f$ 
      REAL QFG   (ILG) !<Evaporation from ground \f$[kg m^{-2} s^{-1}]\f$
      REAL HTCS  (ILG) !<Internal energy change of snow pack due to 
                       !<conduction and/or change in mass \f$[W m^{-2}] (I_s)\f$
      REAL WLOST (ILG) !<Residual amount of water that cannot be supplied by surface stores \f$[kg m^{-2}]\f$
      REAL TRUNOF(ILG) !<Temperature of total runoff [K] 
      REAL RUNOFF(ILG) !<Total runoff \f$[m s^{-1}]\f$ 
      REAL TOVRFL(ILG) !<Temperature of overland flow [K]
      REAL OVRFLW(ILG) !<Overland flow from top of soil column \f$[m s^{-1}]\f$
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG) !<Fractional coverage of subarea in question on modelled area \f$[ ] (X_i)\f$
      REAL R     (ILG) !<Rainfall rate incident on snow pack \f$[m s^{-1}]\f$ 
      REAL S     (ILG) !<Snowfall rate incident on snow pack \f$[kg m^{-2} s^{-1}]\f$
      REAL RHOSNI(ILG) !<Density of fresh snow \f$[kg m^{-3}]\f$
      REAL WSNOW (ILG) !<Liquid water content of snow pack \f$[kg m^{-2}] (w_s)\f$
C
C     * TEMPORARY VARIABLES.
C
      REAL ZADD,ZLOST,ZREM
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT   !<Time step [s]
      REAL TFREZ  !<Freezing point of water [K]
      REAL HCPW   !<Volumetric heat capacity of water \f$(4.187 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPICE !<Volumetric heat capacity of ice \f$(1.9257 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSOL !<Volumetric heat capacity of mineral matter \f$(2.25 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPOM  !<Volumetric heat capacity of organic matter \f$(2.50 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSND !<Volumetric heat capacity of sand particles \f$(2.13 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPCLY !<Volumetric heat capacity of fine mineral particles \f$(2.38 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL SPHW   !<Specific heat of water \f$(4.186 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHICE !<Specific heat of ice \f$(2.10 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHVEG !<Specific heat of vegetation matter \f$(2.70 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHAIR !<Specific heat of air \f$[J kg^{-1} K^{-1}]\f$
      REAL RHOW   !<Density of water \f$(1.0 * 10^3) [kg m^{-3}]\f$
      REAL RHOICE !<Density of ice \f$(0.917 * 10^3) [kg m^{-3}]\f$
      REAL TCGLAC !<Thermal conductivity of ice sheets \f$(2.24) [W m^{-1} K^{-1}]\f$
      REAL CLHMLT !<Latent heat of freezing of water \f$(0.334 * 10^6) [J kg^{-1}]\f$
      REAL CLHVAP !<Latent heat of vaporization of water \f$(2.501 * 10^6) [J kg^{-1}]\f$
C                                       
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
      !>
      !!These calculations are done if a snowpack is present and there is 
      !!no rainfall or snowfall occurring. The change of internal energy 
      !!\f$I_s\f$ of the snow pack as a result of the change in its mass is 
      !!calculated as the difference in \f$I_s\f$ between the beginning and end 
      !!of the subroutine:
      !!
      !!\f$\Delta I_s = X_i \Delta [ C_s z_s T_s ] / \Delta t\f$
      !!
      !!where \f$C_s\f$ represents the volumetric heat capacity of the snow 
      !!pack, \f$T_s\f$ its temperature, \f$\Delta t\f$ the length of the time step, 
      !!and \f$X_i\f$ the fractional coverage of the subarea under consideration 
      !!relative to the modelled area.
      !!
      !!If the sublimation rate EVAP over the snow pack is negative 
      !!(downward), the deposited depth of snow ZADD is calculated from 
      !!EVAP by converting it from a liquid water flux to a fresh snow 
      !!depth using RHOSNI, the fresh snow density. The snowpack density 
      !!is updated as a weighted average of the original snow density 
      !!RHOSNO and RHOSNI. The new snow depth is calculated as the sum of 
      !!the old snow depth ZSNOW and ZADD. The new volumetric heat 
      !!capacity of the snow pack is obtained from the heat capacities of 
      !!ice and water \f$C_i\f$ and \f$C_w\f$, the snow, ice and water densities 
      !!\f$\rho_s\f$ \f$\rho_i\f$, and \f$\rho_w\f$, and the water content and depth of the 
      !!snow pack \f$w_s\f$ and \f$z_s\f$, as:
      !!
      !!\f$C_s = C_i [\rho_s / \rho_i ] + C_w w_s /[\rho_w z_s]\f$
      !!
      !!If the sublimation rate is positive, the depth of the snow pack 
      !!ZLOST that is sublimated over the time step is calculated from 
      !!EVAP using RHOSNO. If ZLOST \f$\leq\f$ ZSNOW, the snow depth is reduced 
      !!and HCPSNO is recalculated. Otherwise the deficit amount ZREM is 
      !!calculated from ZLOST – ZSNOW and converted to a depth of water. 
      !!This amount is further converted to an evaporation rate by 
      !!applying a correction factor of \f$(L_m+ L_v)/L_v\f$, where \f$L_m\f$ is the 
      !!latent heat of melting and \f$L_v\f$ is the latent heat of vaporization 
      !!(to account for the fact that the energy is now being used to 
      !!evaporate water instead of sublimate snow). This necessarily 
      !!leads to a small discrepancy between the overall vapour flux for 
      !!the subarea that was originally calculated in CLASST, and the 
      !!actual change of water storage in the subarea, and therefore this 
      !!discrepancy is added to the housekeeping variable WLOST for use 
      !!in the water balance checks done later in CHKWAT. If there was 
      !!liquid water in the snow pack, WSNOW, it is assigned to overall 
      !!runoff RUNOFF, and to overland flow OVRFLW. The resulting 
      !!temperatures of the runoff and overland flow, TRUNOF and TOVRFL, 
      !!are recalculated as weighted averages using the original runoff 
      !!amounts and temperatures, and the original snow temperature TSNOW 
      !!for WSNOW. The snow depth, heat capacity, temperature and water 
      !!content are all set to zero. Finally, since ZREM now becomes soil 
      !!evaporation rather than snow sublimation, the diagnostic 
      !!variables QFN and QFG, representing the vapour flux from snow and 
      !!soil respectively, are adjusted to reflect this.
      !!
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. (S(I).LT.1.0E-11 .OR. R(I).LT.1.0E-11)
     1                .AND. ZSNOW(I).GT.0.)                       THEN
              HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
              IF(EVAP(I).LT.0.)                             THEN 
                  ZADD=-EVAP(I)*DELT*RHOW/RHOSNI(I)
                  RHOSNO(I)=(ZSNOW(I)*RHOSNO(I)+ZADD*RHOSNI(I))/
     1                      (ZSNOW(I)+ZADD)                          
                  ZSNOW (I)=ZSNOW(I)+ZADD                                                        
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  EVAP  (I)=0.0                                                                
              ELSE                                                                        
                  ZLOST=EVAP(I)*DELT*RHOW/RHOSNO(I)
                  IF(ZLOST.LE.ZSNOW(I))                     THEN 
                      ZSNOW(I)=ZSNOW(I)-ZLOST                                                   
                      EVAP (I)=0.0                                                            
                      HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                    (RHOW*ZSNOW(I))
                  ELSE                                                                    
                      ZREM=(ZLOST-ZSNOW(I))*RHOSNO(I)/RHOW
                      ZSNOW(I)=0.0                                                           
                      HCPSNO(I)=0.0
                      EVAP(I)=ZREM*(CLHMLT+CLHVAP)/(CLHVAP*DELT)
                      WLOST(I)=WLOST(I)-ZREM*RHOW*CLHMLT/CLHVAP
                      IF(RUNOFF(I).GT.0. .OR. WSNOW(I).GT.0.)
     1                 TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TSNOW(I)+TFREZ)*
     1                      WSNOW(I)/RHOW)/(RUNOFF(I)+WSNOW(I)/RHOW)
                      RUNOFF(I)=RUNOFF(I)+WSNOW(I)/RHOW
                      IF(OVRFLW(I).GT.0. .OR. WSNOW(I).GT.0.)
     1                 TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+(TSNOW(I)+TFREZ)*
     1                      FI(I)*WSNOW(I)/RHOW)/(OVRFLW(I)+FI(I)*
     2                      WSNOW(I)/RHOW)
                      OVRFLW(I)=OVRFLW(I)+FI(I)*WSNOW(I)/RHOW
                      TSNOW(I)=0.0 
                      WSNOW(I)=0.0
                      QFN(I)=QFN(I)-FI(I)*ZREM*RHOW/DELT
                      QFG(I)=QFG(I)+FI(I)*EVAP(I)*RHOW
                  ENDIF                                                                   
              ENDIF 
              HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
          ENDIF                                                                      
  100 CONTINUE
      RETURN                                                                      
      END        
