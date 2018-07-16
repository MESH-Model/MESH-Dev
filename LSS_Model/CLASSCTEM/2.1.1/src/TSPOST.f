!>\file
C!Purpose: Snow temperature calculations and cleanup after surface 
C!energy budget calculations.
C!

      SUBROUTINE TSPOST(GSNOW,TSNOW,WSNOW,RHOSNO,QMELTG,
     1                  GZERO,TSNBOT,HTCS,HMFN,
     2                  GCONSTS,GCOEFFS,GCONST,GCOEFF,TBAR,
     3                  TSURF,ZSNOW,TCSNOW,HCPSNO,QTRANS,
     4                  FI,DELZ,ILG,IL1,IL2,JL,IG            )
C
C     * AUG 16/06 - D.VERSEGHY. MAJOR REVISION TO IMPLEMENT THERMAL
C     *                         SEPARATION OF SNOW AND SOIL.
C     * MAR 23/06 - D.VERSEGHY. ADD CALCULATIONS TO ALLOW FOR WATER 
C     *                         FREEZING IN SNOWPACK.
C     * OCT 04/05 - D.VERSEGHY. MODIFY 300 LOOP FOR CASES WHERE IG>3.
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 17/02 - D.VERSEGHY. RESET PONDED WATER TEMPERATURE 
C     *                         USING CALCULATED GROUND HEAT FLUX;
C     *                         SHORTENED CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         INCORPORATE EXPLICITLY CALCULATED
C     *                         THERMAL CONDUCTIVITIES AT TOPS AND
C     *                         BOTTOMS OF SOIL LAYERS, AND
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         FIX BUG IN CALCULATION OF FLUXES
C     *                         BETWEEN SOIL LAYERS (PRESENT SINCE 
C     *                         RELEASE OF CLASS VERSION 2.5).
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF TBARPR(I,1).
C     * APR 10/92 - M.LAZARE.   CLASS - VERSION 2.2.
C     *                         DIVIDE PREVIOUS SUBROUTINE "T4LAYR" INTO
C     *                         "TSPREP" AND "TSPOST" AND VECTORIZE.
C     * APR 11/89 - D.VERSEGHY. CALCULATE HEAT FLUXES BETWEEN SNOW/SOIL
C     *                         LAYERS; CONSISTENCY CHECK ON CALCULATED 
C     *                         SURFACE LATENT HEAT OF MELTING/
C     *                         FREEZING; STEP AHEAD SNOW LAYER 
C     *                         TEMPERATURE AND ASSIGN EXCESS HEAT TO
C     *                         MELTING IF NECESSARY; DISAGGREGATE
C     *                         FIRST SOIL LAYER TEMPERATURE INTO
C     *                         PONDED WATER AND SOIL TEMPERATURES;
C     *                         ADD SHORTWAVE RADIATION TRANSMITTED
C     *                         THROUGH SNOWPACK TO HEAT FLUX AT TOP
C     *                         OF FIRST SOIL LAYER; CONVERT LAYER
C     *                         TEMPERATURES TO DEGREES C.
C
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,IG,I,J
C
C     * OUTPUT ARRAYS.
C
      REAL GZERO (ILG)  !<Heat conduction into soil surface \f$[W m^{-2}] (G(\Delta z_s))\f$   
      REAL TSNBOT(ILG)  !<Temperature at bottom of snow pack [K]
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL GSNOW (ILG)  !<Heat conduction into surface of snow pack \f$[W m^{-2}] (G(0))\f$    
      REAL TSNOW (ILG)  !<Snowpack temperature \f$[K/C] (T_s)\f$
      REAL WSNOW (ILG)  !<Liquid water content of snow pack \f$[kg m^{-2}] (w_s)\f$  
      REAL RHOSNO(ILG)  !<Density of snow \f$[kg m^{-3}] (\rho_s)\f$
      REAL QMELTG(ILG)  !<Available energy to be applied to melting of snow \f$[W m^{-2}]\f$
      REAL HTCS  (ILG)  !<Internal energy change of snow pack due to 
                        !<conduction and/or change in mass \f$[W m^{-2}] (I_s)\f$ 
      REAL HMFN  (ILG)  !<Energy associated with phase change of water in 
                        !<snow pack \f$[W m^{-2}]\f$
C
C     * INPUT ARRAYS.
C
      REAL TSURF (ILG)  !<Snow surface temperature [K]    
      REAL ZSNOW (ILG)  !<Depth of snow pack \f$[m] (\Delta z_s)\f$
      REAL TCSNOW(ILG)  !<Thermal conductivity of snow \f$[W m^{-1} K^{-1}]\f$  
      REAL HCPSNO(ILG)  !<Heat capacity of snow \f$[J m^{-3} K^1] (C_s)\f$
      REAL QTRANS(ILG)  !<Shortwave radiation transmitted through the 
                        !<snow pack \f$[W m^{-2}]\f$
      REAL GCONST(ILG)  !<Intercept used in equation relating snow 
                        !<surface heat flux to snow surface temperature \f$[W m^{-2}]\f$
      REAL GCOEFF(ILG)  !<Multiplier used in equation relating snow 
                        !<surface heat flux to snow surface temperature \f$[W m^{-2} K^{-1}]\f$
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on 
                        !<modelled area \f$[ ] (X_i)\f$
      REAL GCONSTS(ILG) !<Intercept used in equation relating snow 
                        !<surface heat flux to snow surface temperature \f$[W m^{-2}]\f$
      REAL GCOEFFS(ILG) !<Multiplier used in equation relating snow 
                        !<surface heat flux to snow surface temperature \f$[W m^{-2} K^{-1}]\f$
      REAL TBAR(ILG,IG) !<Temperatures of soil layers, averaged over 
                        !<modelled area [K]
      REAL DELZ  (IG)   !<Overall thickness of soil layer [m]
C
C     * TEMPORARY VARIABLES.
C
      REAL GSNOLD,HADD,HCONV,WFREZ
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
      REAL HCPW     !<Volumetric heat capacity of water \f$(4.187 * 10^6) 
                    !<[J m^{-3} K^{-1}]\f$
      REAL HCPICE   !<Volumetric heat capacity of ice \f$(1.9257 * 10^6) 
                    !<[J m^{-3} K^{-1}]\f$
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
                    !<[J kg^{-1} K^{-1}]\f$
      REAL SPHAIR   !<Specific heat of air \f$[J kg^{-1} K^{-1}]\f$
      REAL RHOW     !<Density of water \f$(1.0 * 10^3) \f$[kg m^{-3}]\f$
      REAL RHOICE   !<Density of ice \f$(0.917 * 10^3) \f$[kg m^{-3}]\f$
      REAL TCGLAC   !<Thermal conductivity of ice sheets \f$(2.24) [W m^{-1} K^{-1}]\f$
      REAL CLHMLT   !<Latent heat of freezing of water \f$(0.334 * 10^6) [J kg^{-1}]\f$
      REAL CLHVAP   !<Latent heat of vaporization of water \f$(2.501 * 10^6) [J kg^{-1}]\f$
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C
      !>
      !!In the 100 loop, the heat flux into the snow surface (without 
      !!adjustments that may have been applied relating to partitioning 
      !!of the residual of the surface energy balance among the surface 
      !!flux terms) is calculated from the snow surface temperature 
      !!TSURF, using the GCOEFFS and GCONSTS terms (see documentation of 
      !!subroutine TSPREP). The temperature at the bottom of the snow 
      !!pack, TSNBOT, is then calculated. Currently TSNBOT is determined 
      !!as a simple average of the temperatures of the snow and the first 
      !!soil layer, weighted according to their respective depths (and 
      !!constrained to be \f$\leq\f$ 0 C), but this is under review. The heat 
      !!flux into the soil surface is then evaluated from TSNBOT and the 
      !!GCOEFF and GCONST terms (see documentation of subroutine TNPREP). 
      !!If the energy to be applied to the melting of snow, QMELTG, is 
      !!negative (indicating an energy sink), QMELTG is added to the heat 
      !!flux into the ground, GZERO, and reset to zero. The temperature 
      !!of the snow pack is then stepped forward using the heat fluxes at 
      !!the top and bottom of the snow pack, G(0) and \f$G(\Delta z_s)\f$:
      !!
      !!\f$\Delta T_s = [G(0) - G(\Delta z_s)] \Delta t /(C_s \Delta z_s)\f$
      !!
      !!where \f$C_s\f$ is the snow heat capacity, \f$\Delta t\f$ the time step and 
      !!\f$\Delta z_s\f$ the snow depth. If the new snow temperature is greater than 
      !!zero, the excess amount of heat is calculated and added to QMELTG 
      !!and subtracted from GSNOW, and TSNOW is reset to 0 C. Finally, 
      !!the shortwave radiation transmitted through the snow pack, 
      !!QTRANS, is added to GZERO.
      !!
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              GSNOLD=GCOEFFS(I)*TSURF(I)+GCONSTS(I)
              TSNBOT(I)=(ZSNOW(I)*TSNOW(I)+DELZ(1)*TBAR(I,1))/
     1             (ZSNOW(I)+DELZ(1))
C              TSNBOT(I)=0.90*TSNOW(I)+0.10*TBAR(I,1)
C              TSNBOT(I)=TSURF(I)-GSNOLD*ZSNOW(I)/(2.0*TCSNOW(I))
              TSNBOT(I)=MIN(TSNBOT(I),TFREZ)
              GZERO(I)=GCOEFF(I)*TSNBOT(I)+GCONST(I)
              IF(QMELTG(I).LT.0.)                               THEN
                  GSNOW(I)=GSNOW(I)+QMELTG(I)                                                      
                  QMELTG(I)=0.                                                              
              ENDIF                                                                       
              TSNOW(I)=TSNOW(I)+(GSNOW(I)-GZERO(I))*DELT/
     1                          (HCPSNO(I)*ZSNOW(I))-TFREZ                         
              IF(TSNOW(I).GT.0.)                                THEN
                  QMELTG(I)=QMELTG(I)+TSNOW(I)*HCPSNO(I)*ZSNOW(I)/DELT
                  GSNOW(I)=GSNOW(I)-TSNOW(I)*HCPSNO(I)*ZSNOW(I)/DELT
                  TSNOW(I)=0.                                                               
              ENDIF                                                                       
              GZERO(I)=GZERO(I)+QTRANS(I)
          ENDIF
  100 CONTINUE
C 
      !>
      !!In the 200 loop, since liquid water is assumed only to exist in 
      !!the snow pack if it is at 0 C, a check is carried out to 
      !!determine whether the liquid water content WSNOW > 0 at the same 
      !!time as the snow temperature TSNOW < 0. If so, the change of 
      !!internal energy \f$I_s\f$ of the snow pack as a result of this phase 
      !!change is calculated as the difference in \f$I_s\f$ between the 
      !!beginning and end of the loop:
      !!
      !!\f$\Delta I_s = X_i \Delta [C_s T_s]/ \Delta t\f$
      !!
      !!where \f$X_i\f$ represents the fractional coverage of the subarea under 
      !!consideration relative to the modelled area. The total energy 
      !!sink HADD available to freeze liquid water in the snow pack is 
      !!calculated from TSNOW, and the amount of energy HCONV required to 
      !!freeze all the available water is calculated from WSNOW. If 
      !!HADD < HCONV, only part of WSNOW is frozen; this amount WFREZ is 
      !!calculated from HADD and subtracted from WSNOW, the snow 
      !!temperature is reset to 0 C, the frozen water is used to update 
      !!the snow density, and the snow heat capacity is recalculated:
      !!
      !!\f$C_s = C_i [\rho_s /\rho_i] + C_w w_s/[\rho_w \Delta z_s]\f$
      !!
      !!where \f$C_i\f$ and \f$C_w\f$ are the heat capacities of ice and water 
      !!respectively, \f$w_s\f$ is the snow water content and \f$\rho_s\f$, \f$\rho_i\f$ 
      !!and \f$\rho_w\f$ are the densities of snow, ice and water respectively. 
      !!If HADD > HCONV, the available energy sink is sufficient to 
      !!freeze all of WSNOW. HADD is recalculated as HADD – HCONV, WFREZ 
      !!is set to WSNOW and added to the snow density, WSNOW is set to 
      !!zero, the snow heat capacity is recalculated and HADD is used to 
      !!determine a new value of TSNOW. Finally, WFREZ is used to update 
      !!the diagnostic variables HMFN describing phase changes of water 
      !!in the snow pack, and the change in internal energy HTCS.
      !!
      DO 200 I=IL1,IL2
           IF(FI(I).GT.0. .AND. TSNOW(I).LT.0. .AND. WSNOW(I).GT.0.)
     1                                                              THEN
             HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
             HADD=-TSNOW(I)*HCPSNO(I)*ZSNOW(I)
             HCONV=CLHMLT*WSNOW(I)
             IF(HADD.LE.HCONV)                           THEN                                                  
                 WFREZ=HADD/CLHMLT
                 HADD=0.0
                 WSNOW(I)=MAX(0.0,WSNOW(I)-WFREZ)
                 TSNOW(I)=0.0
                 RHOSNO(I)=RHOSNO(I)+WFREZ/ZSNOW(I)
                 HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1               (RHOW*ZSNOW(I))
             ELSE                                                                        
                 HADD=HADD-HCONV                                                         
                 WFREZ=WSNOW(I)
                 WSNOW(I)=0.0
                 RHOSNO(I)=RHOSNO(I)+WFREZ/ZSNOW(I)
                 HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
                 TSNOW(I)=-HADD/(HCPSNO(I)*ZSNOW(I))
             ENDIF
             HMFN(I)=HMFN(I)-FI(I)*CLHMLT*WFREZ/DELT
             HTCS(I)=HTCS(I)-FI(I)*CLHMLT*WFREZ/DELT
             HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
          ENDIF
  200 CONTINUE
C
      RETURN                                                                      
      END
