!>\file
C!Purpose: Add snow incident on the ground surface to the snow pack.
C!
      SUBROUTINE SNOADD(ALBSNO,TSNOW,RHOSNO,ZSNOW,HCPSNO,HTCS,
     1                  FI,S,TS,RHOSNI,WSNOW,ILG,IL1,IL2,JL)
C
C     * NOV 17/11 - M.LAZARE.   CHANGE SNOW ALBEDO REFRESHMENT 
C     *                         THRESHOLD (SNOWFALL IN CURRENT
C     *                         TIMESTEP) FROM 0.005 TO 1.E-4 M. 
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * SEP 10/04 - R.HARVEY/D.VERSEGHY. INCREASE SNOW ALBEDO 
C     *                         REFRESHMENT THRESHOLD; ADD
C     *                         "IMPLICIT NONE" COMMAND.
C     * JUL 26/02 - D.VERSEGHY. CHANGE RHOSNI FROM CONSTANT TO
C     *                         VARIABLE.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. ACCUMULATION OF SNOW ON GROUND.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL ALBSNO(ILG)  !<Albedo of snow [ ]   
      REAL TSNOW (ILG)  !<Temperature of the snow pack \f$[C] (T_s)\f$ 
      REAL RHOSNO(ILG)  !<Density of snow pack \f$[kg m^{-3}] (\rho_s)\f$
      REAL ZSNOW (ILG)  !<Depth of snow pack \f$[m] (z_s)\f$
      REAL HCPSNO(ILG)  !<Heat capacity of snow pack \f$[J m^{-3} K^{-1}] (C_s)\f$ 
      REAL HTCS  (ILG)  !<Internal energy change of snow pack due to 
                        !<conduction and/or change in mass \f$[W m^{-2}] (I_s)\f$
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on modelled area \f$[ ] (X_i)\f$
      REAL S     (ILG)  !<Snowfall rate incident on snow pack \f$[m s^{-1}]\f$ 
      REAL TS    (ILG)  !<Temperature of snowfall [C] 
      REAL RHOSNI(ILG)  !<Density of fresh snow \f$[kg m^{-3}]\f$
      REAL WSNOW (ILG)  !<Liquid water content of snow pack \f$[kg m^{-2}] (w_s)\f$

C     * TEMPORARY VARIABLES.
C
      REAL SNOFAL,HCPSNP
C                                                                                 
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
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
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
      !>
      !!The change of internal energy HTCS of the snow pack as a result of 
      !!the snowfall added to it is calculated as the difference in Is 
      !!between the beginning and end of the subroutine:
      !!
      !!\f$\Delta I_s = X_i \Delta [C_s z_s T_s] / \Delta t\f$
      !!
      !!where \f$C_s\f$ represents the volumetric heat capacity of the snow 
      !!pack, \f$T_s\f$ its temperature, \f$\Delta\f$ the length of the time step, 
      !!and \f$X_i\f$ the fractional coverage of the subarea under consideration 
      !!relative to the modelled area.
      !!
      !!The amount of snow incident at the given time step, SNOFAL, is 
      !!calculated from S and the timestep length DELT. If 
      !!SNOFAL \f$\geq\f$ 0.1 mm, the snow albedo is set to the fresh snow value 
      !!of 0.84. Otherwise, if the snow is falling on bare ground, its 
      !!initial albedo is set to the old snow value of 0.50. The heat 
      !!capacity of the precipitating snow, HCPSNP, is calculated from 
      !!the fresh snow density RHOSNI and the heat capacity and density 
      !!of ice. The new temperature of the snow pack is calculated as a 
      !!weighted average of its old temperature, weighted by the snow 
      !!depth ZSNOW and heat capacity HCPSNO, and the snowfall 
      !!temperature, weighted by SNOFAL and HCPSNP. The new density of 
      !!snow is calculated as a weighted average of the original density 
      !!RHOSNO and RHOSNI, and the new snow depth is calculated as 
      !!ZSNOW + SNOFAL. Finally, the new heat capacity of the snow pack 
      !!is obtained from the heat capacities of ice and water \f$C_i\f$ and 
      !!\f$C_w\f$, the snow, ice and water densities \f$\rho_s\f$ \f$rho_i\f$, and \f$\rho_w\f$, 
      !!and the water content and depth of the snow pack \f$w_s\f$ and \f$z_s\f$, 
      !!as:
      !!
      !!\f$C_s = C_i [ \rho_s /\rho_i ] + C_w w_s /[\rho_w z_s]\f$
      !!
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. S(I).GT.0.)                         THEN
              HTCS  (I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                  ZSNOW(I)/DELT
              SNOFAL=S(I)*DELT                      
              IF(SNOFAL.GE.1.E-4)                               THEN 
                  ALBSNO(I)=0.84                                                             
              ELSE IF(.NOT.(ZSNOW(I).GT.0.))                THEN
                  ALBSNO(I)=0.50                                                         
              ENDIF                                                                   
              HCPSNP=HCPICE*RHOSNI(I)/RHOICE
              TSNOW (I)=((TSNOW(I)+TFREZ)*ZSNOW(I)*HCPSNO(I) +
     1                   (TS   (I)+TFREZ)*SNOFAL  *HCPSNP)/
     2                  (ZSNOW(I)*HCPSNO(I) + SNOFAL*HCPSNP) -
     3                   TFREZ
              RHOSNO(I)=(ZSNOW(I)*RHOSNO(I) + SNOFAL*RHOSNI(I))/
     1                  (ZSNOW(I)+SNOFAL)                          
              ZSNOW (I)=ZSNOW(I)+SNOFAL                                                          
              HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                  (RHOW*ZSNOW(I))
              HTCS  (I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                  ZSNOW(I)/DELT
          ENDIF                                                 
  100 CONTINUE
C                                                                                  
      RETURN                                                                      
      END    
