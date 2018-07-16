!>\file
!!Purpose: Perform temperature stepping and surface runoff 
!!calculations over ice sheets.
!!

      SUBROUTINE ICEBAL(TBAR,TPOND,ZPOND,TSNOW,RHOSNO,ZSNOW,HCPSNO,
     1                  ALBSNO,HMFG,HTCS,HTC,WTRS,WTRG,GFLUX,
     2                  RUNOFF,TRUNOF,OVRFLW,TOVRFL,ZPLIM,GGEO,
     3                  FI,EVAP,R,TR,GZERO,G12,G23,HCP,QMELT,WSNOW,
     4                  ZMAT,TMOVE,WMOVE,ZRMDR,TADD,ZMOVE,TBOT,DELZ,
     5                  ISAND,ICONT,IWF,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
C
C     * OCT 03/14 - D.VERSEGHY. CHANGE LIMITING VALUE OF SNOW ON ICE
C     *                         FROM 100 KG/M2 TO 10 M.
C     * DEC 27/07 - D.VERSEGHY. ADD GEOTHERMAL HEAT FLUX; ADD ICE MASS
C     *                         LOSS TO RUNOFF.
C     * NOV 01/06 - D.VERSEGHY. ALLOW PONDING OF WATER ON ICE SHEETS.
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * OCT 07/05 - D.VERSEGHY. MODIFY FOR CASES WHERE IG>3.
C     * MAR 30/05 - D.VERSEGHY. ADD RUNOFF TEMPERATURE CALCULATION;
C     *                         REMOVE UPDATE TO WTRG IN LOOP 300
C     *                         (BUGFIX).
C     * SEP 24/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 24/02 - D.VERSEGHY. UPDATE SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * DEC 12/01 - D.VERSEGHY. PASS IN SWITCH TO CALCULATE SURFACE FLOW
C     *                         ONLY IF WATFLOOD ROUTINES ARE NOT CALLED.
C     * NOV 16/98 - M.LAZARE.   "WTRG" UPDATED TO GAIN ICE MASS AS "WTRS"
C     *                         LOSES SNOW MASS IN SNOW->ICE CONVERSION
C     *                         (TWO PLACES).
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY 
C     *                         BETWEEN SOIL LAYERS IN MAIN CODE.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. STEP AHEAD "SOIL" LAYER TEMPERATURES
C     *                         OVER CONTINENTAL ICE SHEETS; ASSIGN
C     *                         PONDED WATER TO RUNOFF; ADJUST LAYER
C     *                         DEPTHS FOR ACCUMULATION/ABLATION.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IWF,IG,IGP1,IGP2,ILG,IL1,IL2,JL,I,J,K,N
C
C     * INPUT/OUTPUT FIELDS.
C                                                                                 
      REAL TBAR  (ILG,IG) !<Temperature of ice layer \f$[C] (T_{av}( \Delta z))\f$
      REAL HMFG  (ILG,IG) !<Energy associated with freezing or thawing of water in ice layer \f$[W m^{-2}]\f$
      REAL HTC   (ILG,IG) !<Internal energy change of ice layer due to conduction and/or change in mass \f$[W m^{-2}]\f$ (Ij)
      REAL GFLUX (ILG,IG) !<Heat flow between ice layers \f$[W m^{-2}] (G(\Delta z))\f$
C
      REAL TPOND (ILG)  !<Temperature of ponded water [C]
      REAL ZPOND (ILG)  !<Depth of ponded water [m]  
      REAL TSNOW (ILG)  !<Temperature of the snow pack [C]  
      REAL RHOSNO(ILG)  !<Density of snow pack \f$[kg m^{-3}]\f$
      REAL ZSNOW (ILG)  !<Depth of snow pack [m]  
      REAL HCPSNO(ILG)  !<Heat capacity of snow pack \f$[J m^{-3} K^{-1}]\f$ 
      REAL ALBSNO(ILG)  !<Albedo of snow [ ]  
      REAL HTCS  (ILG)  !<Internal energy change of ice layer due to conduction and/or change in mass \f$[W m^{-2}] (I_j) \f$
      REAL WTRS  (ILG)  !<Water transferred into or out of the snow pack \f$[kg m^{-2} s^{-1}]\f$ 
      REAL WTRG  (ILG)  !<Water transferred into or out of the ice \f$[kg m^{-2} s^{-1}]\f$  
      REAL RUNOFF(ILG)  !<Total runoff from ice column [m]  
      REAL TRUNOF(ILG)  !<Temperature of total runoff from ice column [K]
      REAL OVRFLW(ILG)  !<Overland flow from top of ice column [m]
      REAL TOVRFL(ILG)  !<Temperature of overland flow from top of ice column [K]
C
C     * INPUT FIELDS.
C
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on modelled area \f$[ ] (X)\f$    
      REAL EVAP  (ILG)  !<Evaporation rate from ice surface \f$[m s^{-1}]\f$
      REAL R     (ILG)  !<Rainfall rate at ice surface \f$[m s^{-1}]\f$  
      REAL TR    (ILG)  !<Temperature of rainfall [C]
      REAL GZERO (ILG)  !<Heat flow into ice surface \f$[W m^{-2}] (G(0))\f$ 
      REAL G12   (ILG)  !<Heat flow between first and second ice layers \f$[W m^{-2}] (G(\Delta z1))\f$
      REAL G23   (ILG)  !<Heat flow between second and third ice layers \f$[W m^{-2}] (G(\Delta z2))\f$
      REAL QMELT (ILG)  !<Energy available for melting of ice \f$[W m^{-2}]\f$
      REAL WSNOW (ILG)  !<Liquid water content of snow pack \f$[kg m^{-2}]\f$
      REAL ZPLIM (ILG)  !<Limiting depth of ponded water [m] 
      REAL GGEO  (ILG)  !<Geothermal heat flux at bottom of modelled ice profile \f$[W m^{-2}]\f$
C
      REAL HCP   (ILG,IG)   !<Heat capacity of ice layer \f$[J m^{-3} K^{-1}]\f$
C
      INTEGER ISAND (ILG,IG)!<Sand content flag
C
      REAL DELZ  (IG)       !<Overall thickness of ice layer \f$[m] (\Delta z)\f$
C
C     * WORK FIELDS.
C
      REAL ZMAT  (ILG,IGP2,IGP1)
      REAL TMOVE (ILG,IGP2)
      REAL WMOVE (ILG,IGP2)
      REAL ZRMDR (ILG,IGP1)
C
      REAL TADD  (ILG)
      REAL ZMOVE (ILG)
      REAL TBOT  (ILG) 
C
      INTEGER ICONT (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL TZERO,RADD,QADD,ZMELT,GP1,HCOOL,HWARM,HFREZ,ZFREZ,SNOCONV
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
      REAL HCPW     !<Volumetric heat capacity of water \f$(4.187 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPICE   !<Volumetric heat capacity of ice \f$(1.9257 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSOL   !<Volumetric heat capacity of mineral matter \f$(2.25*10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPOM    !<Volumetric heat capacity of organic matter \f$(2.50*10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSND   !<Volumetric heat capacity of sand particles \f$(2.13*10^6) [J m^{-3} K^{-1}]\f$
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
      COMMON /CLASS1/ DELT,TFREZ                                               
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
!>
!>In the 100 loop, any rainfall or snowmelt R reaching the ice surface is added to the ponded water on the surface. The ponded 
!>water temperature is calculated as the weighted average of the existing pond and the rainfall or snowmelt added, and the change 
!>in internal energy HTC of the first ice layer is updated using the temperature of the added water.
!>

C     * ADD RAINFALL OR SNOWMELT TO PONDED WATER AND ASSIGN EXCESS
C     * TO RUNOFF.  CHECK FOR POND FREEZING.
C
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                THEN
              IF(R(I).GT.0.)                                THEN 
                 RADD=R(I)*DELT                                                             
                 TPOND(I)=((TPOND(I)+TFREZ)*ZPOND(I)+(TR(I)+TFREZ)*
     1               RADD)/(ZPOND(I)+RADD)-TFREZ
                 ZPOND(I)=ZPOND(I)+RADD                                                        
                 HTC (I,1)=HTC(I,1)+FI(I)*(TR(I)+TFREZ)*HCPW*
     1                     RADD/DELT
              ENDIF
!>
!!If a full-scale hydrological modelling application is not being run, that is, if only vertical fluxes of energy
!!and moisture are being modelled, the flag IWF will have been pre-set to zero. In this case, overland flow
!!of water is treated using a simple approach: if the ponded depth of water on the soil surface ZPOND
!!exceeds a pre-determined limiting value ZPLIM, the excess is assigned to overland flow. The total runoff
!!from the ice sheet, RUNOFF, is incremented by the excess of the ponded water, and the overland flow
!!for the whole grid cell OVRFLW is incremented by the product of the excess ponded water and the
!!fractional area of the grid cell. The temperature of the overall runoff from the modelled area TRUNOF,
!!and the temperature of the overland flow for the grid cell TOVRFL, are calculated as weighted averages
!!over their previous values and the ponded water temperature TPOND. The internal energy change HTC
!!of the first soil layer is adjusted for the amount of water lost, and ZPOND is set to ZPLIM.
!!
              IF(IWF.EQ.0 .AND. (ZPOND(I)-ZPLIM(I)).GT.1.0E-8) THEN
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TPOND(I)+TFREZ)*
     1                   (ZPOND(I)-ZPLIM(I)))/(RUNOFF(I)+ZPOND(I)-
     2                   ZPLIM(I))
                  RUNOFF(I)=RUNOFF(I)+ZPOND(I)-ZPLIM(I)
                  TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+(TPOND(I)+TFREZ)*
     1                   FI(I)*(ZPOND(I)-ZPLIM(I)))/(OVRFLW(I)+
     2                   FI(I)*(ZPOND(I)-ZPLIM(I)))
                  OVRFLW(I)=OVRFLW(I)+FI(I)*(ZPOND(I)-ZPLIM(I)) 
                  HTC(I,1)=HTC(I,1)-FI(I)*(TPOND(I)+TFREZ)*HCPW*
     1                   (ZPOND(I)-ZPLIM(I))/DELT
                  ZPOND(I)=MIN(ZPOND(I),ZPLIM(I))
              ENDIF
!>
!!If the temperature of the remaining ponded water is greater than 0 C, the sink of energy required to cool
!!it to 0 C, HCOOL, is calculated and compared with the amount of energy required to warm the first ice
!!layer to 0 C, HWARM. If HWARM > HCOOL, the energy sink of the first layer is used to cool the
!!ponded water to 0 C, and the layer temperature is updated accordingly. Otherwise, the ponded water
!!temperature and the temperature of the first ice layer are both set to 0 C, and the excess energy source
!!given by HCOOL-HWARM is added to the heat available for melting ice, QMELT.
!!
              IF(TPOND(I).GT.0.001)                           THEN
                  HCOOL=TPOND(I)*HCPW*ZPOND(I)
                  HWARM=-TBAR(I,1)*HCPICE*DELZ(1)
                  IF(HWARM.GT.HCOOL)                     THEN
                      TBAR(I,1)=TBAR(I,1)+HCOOL/(HCPICE*DELZ(1))
                      TPOND(I)=0.0
                  ELSE
                      TBAR(I,1)=0.0
                      TPOND(I)=0.0
                      QMELT(I)=QMELT(I)+(HCOOL-HWARM)/DELT
                  ENDIF
              ENDIF
          ENDIF
  100 CONTINUE
!>
!!In loop 125, if the temperature of the first ice layer is less than -2 C after the above operations (i.e. if it is
!!not very close to 0 C), and if the ponded water depth is not vanishingly small, freezing of the ponded
!!water can take place. The energy sink required to freeze all of the ponded water, HFREZ, is calculated
!!and compared with HWARM, the amount of energy required to raise the temperature of the first ice layer
!!to 0 C. If HWARM > HFREZ, then HFREZ is converted into an equivalent temperature change using
!!the heat capacity of ice, and added to the temperature of the first ice layer. HFREZ is also used to update
!!HMFG, the diagnosed energy used for phase changes of water in the first ice layer. The internal energy
!!of the first soil layer, HTC, is adjusted to account for the loss of the ponded water, which is assumed to
!!be added to the snow pack. The ponded water is converted into a frozen depth ZFREZ, which is used to
!!update the internal energy of the snow pack, HTCS. If there is not a pre-existing snow pack, the snow
!!albedo is set to the limiting low value of 0.50. The temperature and density of the snow pack are
!!recalculated as weighted averages over the original values and the frozen amount that has been added.
!!ZFREZ is added to the snow depth ZSNOW. The snow heat capacity is recalculated using the new value
!!of the snow density. Finally, the diagnostic amounts of water transferred to the snow pack, WTRS, and
!!from the ice, WTRG, are updated using ZFREZ.
!!
      DO 125 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                THEN
              IF(TBAR(I,1).LT.-2.0 .AND. ZPOND(I).GT.1.0E-8) THEN
                  HFREZ=ZPOND(I)*RHOW*CLHMLT
                  HWARM=-TBAR(I,1)*HCPICE*DELZ(1)
                  IF(HWARM.GE.HFREZ) THEN
                      TBAR(I,1)=TBAR(I,1)+HFREZ/(HCPICE*DELZ(1))
                      HMFG(I,1)=HMFG(I,1)-FI(I)*HFREZ/DELT
                      HTC(I,1)=HTC(I,1)-FI(I)*HCPW*TFREZ*ZPOND(I)/DELT
                      ZFREZ=ZPOND(I)*RHOW/RHOICE                                                 
                      ZPOND(I)=0.0
                      TPOND(I)=0.0
                      HTCS(I)=HTCS(I)+FI(I)*HCPICE*TFREZ*ZFREZ/DELT
                      IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                      TSNOW(I)=((TSNOW(I)+TFREZ)*HCPSNO(I)*ZSNOW(I)+
     1                     TFREZ*HCPICE*ZFREZ)          
     2                     /(HCPSNO(I)*ZSNOW(I)+HCPICE*ZFREZ)-TFREZ
                      RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/
     1                    (ZSNOW(I)+ZFREZ)                        
                      ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                      HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                    (RHOW*ZSNOW(I))
                      WTRS(I)=WTRS(I)+FI(I)*ZFREZ*RHOICE/DELT
                      WTRG(I)=WTRG(I)-FI(I)*ZFREZ*RHOICE/DELT
                  ENDIF                                                                       
              ENDIF
          ENDIF
  125 CONTINUE
C
C     * STEP AHEAD ICE LAYER TEMPERATURES.
C
!>
!!In the 150 loop the heat fluxes between the ice layers are calculated for the optional multiple-layer
!!configuration, in which the standard third layer, normally with a thickness of 3.75 m, can be subdivided
!!into smaller layers and extended to a greater depth if desired. (The heat fluxes at the ice surface, and
!!between the first and second and the second and third layers, were already calculated in the CLASST
!!subroutines.) The remaining fluxes are calculated by using a simple linearization of the soil temperature
!!profile. The expression for the ground heat flux at a depth z, G(z), which depends on the thermal
!!conductivity \f$\lambda(z)\f$ and the temperature gradient, is written as:
!!
!!\f$G(z) = \lambda(z) dT(z)/dz\f$
!!
!!The linearized form is thus:
!!
!!\f$G_j = 2 \lambda_i (T_{j-1} – T_j) / (\Delta z_{j-1} + \Delta z_j)\f$
!!
!!where \f$G_j\f$ is the heat flux at the top of layer j, \f$T_j\f$ and \f$\Delta z_j\f$ refer to the temperature and thickness 
!!of the layer, and \f$\lambda_i\f$ is the thermal conductivity of ice.
!!
      IF(IG.GT.3) THEN
      DO 150 J=4,IG
          DO 150 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)            THEN
                  GFLUX(I,J)=2.0*TCGLAC*(TBAR(I,J-1)-TBAR(I,J))/
     1                       (DELZ(J-1)+DELZ(J))
              ENDIF
  150     CONTINUE
      ENDIF
C     
      !>
      !!In the 200 loop a value is assigned to the temperature at the bottom of the ice profile, TBOT, and the
      !!temperatures for the first three ice layers are stepped ahead. If the standard three-layer configuration is
      !!being used, TBOT is obtained by making use of the assumption (see documentation for subroutine
      !!TNPREP) that the variation of temperature T with depth z within each soil layer can be modelled using a
      !!quadratic equation:
      !!
      !!\f$T(z) = 1/2 a z^2 + b z +c\f$
      !!
      !!It can be shown that the temperature at the bottom of a given soil layer, \f$T(\Delta z)\f$, is related to the
      !!temperature at the top of the layer T(0) and the heat fluxes at the top and bottom of the layer, G(0) and
      !!\f$G(\Delta z)\f$, as follows:
      !!
      !!\f$T(\Delta z) = T(0) - (\Delta z/ 2 \lambda_i)[G(0) + G(\Delta z)]\f$
      !!
      !!Making use of the continuity requirement that the heat flux and temperature at the bottom of a given
      !!layer must be equal to the heat flux and temperature at the top of the layer beneath it, an expression for
      !!the temperature at the bottom of the third ice layer can be obtained as a function of the temperature at
      !!the surface and the fluxes between the ice layers:
      !!
      !!\f$T(\Delta z_3) = T(0) – {G(\Delta z_2) [\Delta z_3 + \Delta z_2] + G(\Delta z_1) [\Delta z_2 + \Delta z_1] + G(0) \Delta z_{21}} / 2 \lambda_i\f$
      !!
      !!The surface temperature T(0) is obtained by integrating the equation for T(z) to obtain an expression for
      !!the average layer temperature \f$T_{av}(\Delta z)\f$, and then inverting this to solve for T(0):
      !!
      !!\f$T(0) = T_{av}(\Delta z_1) + (\Delta z_1/ 3\lambda_i) [G(0) + 1/2 G(\Delta z_1)]\f$
      !!
      !!The third layer temperature is then updated using the geothermal flux GGEO. If the optional multiple-
      !!layer configuration is used, TBOT is simply set to the temperature of the lowest layer. In either the
      !!standard or the multiple-layer case, the first three layer temperatures are updated using the heat fluxes at
      !!the surface, between the first and second layers and between the second and third layers, which were
      !!determined in the CLASST subroutines. Finally, the latter fluxes are assigned to the appropriate levels in
      !!the diagnostic GFLUX vector.
      !!
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              IF(IG.EQ.3) THEN
                  TZERO=TBAR(I,1)+DELZ(1)*(GZERO(I)+0.5*G12(I))/
     1                (3.0*TCGLAC)                        
                  TBOT(I)=TZERO-(G23(I)*(DELZ(3)+DELZ(2))+G12(I)*
     1                    (DELZ(1)+DELZ(2))+GZERO(I)*DELZ(1))/
     2                    (2.0*TCGLAC)
                  TBAR(I,3)=TBAR(I,3)-GGEO(I)*DELT/
     1                    (HCP(I,3)*DELZ(3))                           
              ELSE
                  TBOT(I)=TBAR(I,IG)
              ENDIF
              TBAR(I,1)=TBAR(I,1)+(GZERO(I)-G12(I))*DELT/
     1                   (HCP(I,1)*DELZ(1))                       
              TBAR(I,2)=TBAR(I,2)+(G12(I)-G23(I))*DELT/
     1                   (HCP(I,2)*DELZ(2))                         
              TBAR(I,3)=TBAR(I,3)+G23(I)*DELT/
     1                   (HCP(I,3)*DELZ(3))                           
              GFLUX(I,1)=GZERO(I)
              GFLUX(I,2)=G12(I)
              GFLUX(I,3)=G23(I)
          ENDIF
  200 CONTINUE
C
      IF(IG.GT.3)                                                  THEN
      !>
      !!In the 250 loop, the GFLUX values determined in the 150 loop are used to update the temperatures in
      !!the third and lower ice layers for the multiple-layer configuration. The calculations are bracketed by a
      !!determination of the change of internal energy \f$I_j\f$ of the ice layers as a result of the heat fluxes, obtained as
      !!the difference in \f$I_j\f$ between the beginning and end of the calculations:
      !!
      !!\f$\Delta I_j = X_i \Delta[C_i \Delta z_j T_{av}(\Delta z_j)]/ \Delta t\f$
      !!
      !!where \f$C_i\f$ is the heat capacity of ice, \f$Delta t\f$ is the length of the time step, and \f$X_i\f$ is the 
      !!fractional coverage of the subarea under consideration relative to the modelled area.
      !!
      DO 250 J=3,IG                                                               
      DO 250 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                 THEN
              HTC (I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                  DELZ(J)/DELT
              IF(J.EQ.3)                                THEN
                  TBAR(I,J)=TBAR(I,J)-GFLUX(I,J+1)*DELT/
     1                      (HCP(I,J)*DELZ(J))
              ELSEIF(J.EQ.IG)                           THEN
                  TBAR(I,J)=TBAR(I,J)+(GFLUX(I,J)-GGEO(I))*DELT/
     1                      (HCP(I,J)*DELZ(J))
              ELSE
                  TBAR(I,J)=TBAR(I,J)+(GFLUX(I,J)-GFLUX(I,J+1))*DELT/
     1                      (HCP(I,J)*DELZ(J))
              ENDIF
              HTC (I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                  DELZ(J)/DELT
          ENDIF
  250 CONTINUE
      ENDIF
C
C     * IF LAYER TEMPERATURES OVERSHOOT ZERO, ADD EXCESS HEAT TO
C     * HEAT OF MELTING.
C
      !>
      !!In the 300 loop, checks are carried out to determine whether any of the ice layer temperatures has
      !!overshot 0 C as a result of the calculations in the previous loop. If so, the excess energy is assigned to a
      !!temporary variable QADD and is also added to the total heat available for melting of the ice, QMELT.
      !!QADD is subtracted from the internal energy of the layer in questions and is added to the internal energy
      !!of the first layer, since melting is assumed to proceed from the top downwards. The temperature of the
      !!layer is reset to 0 C. Finally, the first half of a calculation of the change of internal energy of each ice layer
      !!is performed, to be completed at the end of the subroutine.
      !!
      DO 300 J=1,IG
      DO 300 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              IF(TBAR(I,J).GT.0.)                            THEN  
                  QADD=TBAR(I,J)*HCPICE*DELZ(J)/DELT  
                  QMELT(I)=QMELT(I)+QADD
                  HTC(I,J)=HTC(I,J)-FI(I)*QADD
                  HTC(I,1)=HTC(I,1)+FI(I)*QADD
                  TBAR(I,J)=0.0                                                       
              ENDIF
              HTC(I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                 DELZ(J)/DELT
          ENDIF
  300 CONTINUE
      !>
      !!In the next three loops, the ice layers are adjusted downward to account for the removal of mass at the
      !!surface by melting or sublimation. First, the temperature TMOVE of the ice into which the layer is
      !!moving is set to the temperature of the layer below it. TMOVE for the bottom layer is set to TBOT. A
      !!depth of ice ZMELT is calculated as the amount of ice for which QMELT is sufficient to both raise its
      !!temperature to 0 C (if necessary) and melt it. The temperature of the overall runoff and the overland flow
      !!are updated as averages of the original temperature and the meltwater temperature (assumed to be at the
      !!freezing point), weighted according to their respective amounts, and the overall runoff and overland flow
      !!are incremented by ZMELT (with ZMELT converted to an equivalent water depth). The energy used for
      !!the melting of ice is calculated from ZMELT and added to HMFG for the first layer, and the amount of
      !!energy used to raise the layer temperature to 0 C is added to HTC for the first layer. The total depth of
      !!downward adjustment of the ice layers, ZMOVE, is obtained as the sum of ZMELT and the sublimation
      !!depth, calculated from EVAP. This amount is added to the diagnostic variable WTRG. Finally, the new
      !!temperature of each ice layer is calculated over the layer thickness DELZ as the average of the original
      !!temperature weighted by DELZ-ZMOVE and TMOVE weighted by ZMOVE.
      !!
C
C     * APPLY CALCULATED HEAT OF MELTING TO UPPER ICE LAYER; ADD MELTED
C     * WATER TO TOTAL RUNOFF; CALCULATE DEPTH OF ICE REMOVED BY MELTING
C     * AND SUBLIMATION; RECALCULATE ICE LAYER TEMPERATURES.
C     
      DO 325 J=1,IG-1
      DO 325 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              TMOVE(I,J)=TBAR(I,J+1)
          ENDIF
  325 CONTINUE                                                                
C
      DO 350 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN                         
              IF(QMELT(I).GT.0. .OR. EVAP(I).GT.0.)           THEN                                        
                  TMOVE(I,IG)=TBOT(I)                                                           
                  ZMELT=QMELT(I)*DELT/((0.0-TBAR(I,1))*HCPICE+
     1                  CLHMLT*RHOICE)                 
                  IF(ZMELT.GT.0.)                   THEN
                      TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+TFREZ*ZMELT*
     1                   RHOICE/RHOW)/(RUNOFF(I)+ZMELT*RHOICE/RHOW)
                      TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+TFREZ*FI(I)*ZMELT*
     1                   RHOICE/RHOW)/(OVRFLW(I)+FI(I)*ZMELT*RHOICE/
     1                   RHOW)
                  ENDIF
                  RUNOFF(I)=RUNOFF(I)+ZMELT*RHOICE/RHOW                                         
                  OVRFLW(I)=OVRFLW(I)+FI(I)*ZMELT*RHOICE/RHOW                                         
                  HMFG(I,1)=HMFG(I,1)+FI(I)*CLHMLT*RHOICE*ZMELT/DELT
                  HTC (I,1)=HTC(I,1)-FI(I)*(QMELT(I)-CLHMLT*RHOICE*
     1                     ZMELT/DELT)
                  ZMOVE (I)=ZMELT+EVAP(I)*DELT*RHOW/RHOICE
                  WTRG  (I)=WTRG(I)+FI(I)*ZMOVE(I)*RHOICE/DELT
              ENDIF
          ENDIF                                                                       
  350 CONTINUE                                       
C
      DO 400 J=1,IG
      DO 400 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       (QMELT(I).GT.0. .OR. EVAP(I).GT.0.))                   THEN 
              TBAR(I,J)=(TBAR(I,J)*(DELZ(J)-ZMOVE(I))+TMOVE(I,J)*
     1                   ZMOVE(I))/DELZ(J)
          ENDIF                                                                       
  400 CONTINUE  
      !>
      !!In the next loops, the ice layers are adjusted upward to account for addition of mass at the surface by
      !!conversion of snow to ice. Snow is converted to ice if the mass of the snow pack exceeds \f$100 kg m^{-2}\f$, or
      !!if the density exceeds \f$900 kg m^{-3}\f$ (approaching that of ice). In the first case the excess over
      !!and above \f$100 kg m^{-2}\f$ is converted; in the second, the whole snow pack is converted. 
      !!These calculations are performed
      !!in the 500 loop, bracketed by a calculation of the change in internal energy of the snow pack, HTCS. In
      !!both cases the first level of the ice level movement matrix WMOVE is set to the amount of snow that is
      !!converted, expressed as a depth of ice, and the first level of the temperature matrix TMOVE is set to the
      !!snow temperature. The amount of converted snow is added to the diagnostic variables WTRS and
      !!WTRG. The depth, density and heat capacity of the snow are recalculated. If the entire snow pack is
      !!being converted and the water content WSNOW was non-zero, WSNOW is subtracted from WTRS and
      !!added to WTRG, and is also added to the total runoff and the overland flow. The runoff and overland
      !!flow temperatures are updated accordingly. The snow temperature and water content are reset to zero.
      !!The amount of ice that is lost to the bottom of the profile is added to the total runoff, and the runoff
      !!temperature is updated accordingly.                                        
      !!
C
C     * IF SNOW PACK EXCEEDS 100 KG M-2 OR SNOW DENSITY EXCEEDS 
C     * 900 KG M-3, CONVERT EXCESS TO ICE AND MOVE THE LOCATIONS
C     * OF THE ICE LAYERS ACCORDINGLY.
C
      DO 500 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              ICONT(I)=0
              SNOCONV=0.
              HTCS(I)=HTCS(I)-FI(I)*(TSNOW(I)+TFREZ)*HCPSNO(I)*
     1                ZSNOW(I)/DELT
              IF((ZSNOW(I)).GT.10.)                         THEN        
                  SNOCONV=(ZSNOW(I)-10.0)*RHOSNO(I)                     
                  WMOVE(I,1)=SNOCONV/RHOICE                                
                  TMOVE(I,1)=TSNOW(I)                                                      
                  WTRS(I)=WTRS(I)-FI(I)*WMOVE(I,1)*RHOICE/DELT
                  WTRG(I)=WTRG(I)+FI(I)*WMOVE(I,1)*RHOICE/DELT
                  ZSNOW(I)=10.0                                         
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  ICONT(I)=1
              ELSE IF(RHOSNO(I).GE.900.)                      THEN
                  SNOCONV=ZSNOW(I)*RHOSNO(I)
                  WMOVE(I,1)=SNOCONV/RHOICE                                        
                  TMOVE(I,1)=TSNOW(I)                                                      
                  WTRS(I)=WTRS(I)-FI(I)*(SNOCONV+WSNOW(I))/DELT
                  WTRG(I)=WTRG(I)+FI(I)*(SNOCONV+WSNOW(I))/DELT
                  ZSNOW(I)=0.0                                                           
                  RHOSNO(I)=0.0                                                          
                  HCPSNO(I)=0.0                                                          
                  IF(WSNOW(I).GT.0.0)                    THEN
                      TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TSNOW(I)+TFREZ)*
     1                      WSNOW(I)/RHOW)/(RUNOFF(I)+WSNOW(I)/RHOW)
                      RUNOFF(I)=RUNOFF(I)+WSNOW(I)/RHOW
                      TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+(TSNOW(I)+TFREZ)*
     1                      FI(I)*WSNOW(I)/RHOW)/(OVRFLW(I)+FI(I)*
     2                      WSNOW(I)/RHOW)
                      OVRFLW(I)=OVRFLW(I)+FI(I)*WSNOW(I)/RHOW
                  ENDIF
                  TSNOW(I)=0.0
                  WSNOW(I)=0.0
                  ICONT(I)=1
              ENDIF                     
              HTCS(I)=HTCS(I)+FI(I)*(TSNOW(I)+TFREZ)*HCPSNO(I)*
     1                ZSNOW(I)/DELT
              IF(SNOCONV.GT.0.) TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+
     1                TBAR(I,IG)*SNOCONV/RHOW)/(RUNOFF(I)+SNOCONV/RHOW)
              RUNOFF(I)=RUNOFF(I)+SNOCONV/RHOW
          ENDIF
  500 CONTINUE
C        
      !>
      !!In the remaining parts of the code, the actual adjustment of ice layer positions is performed. First the
      !!levels of the available depth matrix ZRMDR are set to the ice layer thicknesses; the matrix ZMAT is
      !!initialized to zero; and each level J of WMOVE and TMOVE from 2 to the bottom of the ice profile is
      !!set to the value of DELZ and TBAR respectively of the J-1 ice level. The ZMAT matrix represents the
      !!depth of each ice layer J that is occupied by ice from level K in the layer movement matrix WMOVE after
      !!the layer adjustments are complete. In the 700 loop, starting at the top of the ice profile, an attempt is
      !!made to assign each layer of WMOVE in turn to the K,J level of ZMAT. If the calculated value of
      !!ZMAT is greater than the available depth ZRMDR of the layer, ZMAT is set to ZRMDR, WMOVE is
      !!decremented by ZRMDR, and ZRMDR is set to zero. Otherwise the calculated value of ZMAT is
      !!accepted, ZRMDR is decremented by ZMAT, and WMOVE is set to zero. 
      !!
      !!Finally, the 900 loop is performed over each ice layer J to determine the new layer 
      !!temperature. The temperature adjustment variable TADD is initialized to zero, and then incremented by the 
      !!temperature TMOVE of each level K weighted by the corresponding K,J level of ZMAT, and finally by 
      !!the original layer temperature weighted by the corresponding level of ZRMDR. The layer temperature is 
      !!reset to TADD normalized by DELZ, and the updating of HTC, begun at the end of the 300 loop, is completed.
      !!      
      DO 550 J=1,IG
      DO 550 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                   THEN
              ZRMDR(I,J)=DELZ(J)                                                    
          ENDIF                                                  
  550 CONTINUE                                                            
C               
      DO 600 J=1,IG
      DO 600 K=1,IG+1
      DO 600 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       ICONT(I).EQ.1)                                         THEN
              ZMAT(I,K,J)=0.0 
          ENDIF
  600 CONTINUE
C
      DO 650 J=2,IG+1
      DO 650 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       ICONT(I).EQ.1)                                         THEN
              WMOVE(I,J)=DELZ(J-1)                                                  
              TMOVE(I,J)=TBAR(I,J-1)                                                
          ENDIF
  650 CONTINUE
C
      DO 700 K=1,IG+1
      DO 700 J=1,IG
      DO 700 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1       ICONT(I).EQ.1)                                         THEN
              IF(ZRMDR(I,J).GT.0. .AND. WMOVE(I,K).GT.0.)      THEN                    
                  ZMAT(I,K,J)=WMOVE(I,K)                                          
                  IF(ZMAT(I,K,J).GE.ZRMDR(I,J))           THEN                              
                      ZMAT(I,K,J)=ZRMDR(I,J)                                      
                      WMOVE(I,K)=WMOVE(I,K)-ZRMDR(I,J)                              
                      ZRMDR(I,J)=0.0                                            
                  ELSE                                                        
                      ZRMDR(I,J)=ZRMDR(I,J)-ZMAT(I,K,J)                             
                      WMOVE(I,K)=0.0                                            
                  ENDIF                                                       
              ENDIF                                                           
          ENDIF
  700 CONTINUE
C
      DO 900 J=1,IG
          DO 750 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)               THEN
                  TADD(I)=0.
              ENDIF
  750     CONTINUE
C
          DO 800 K=1,IG+1
          DO 800 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4 .AND.
     1           ICONT(I).EQ.1)                                     THEN
                  TADD(I)=TADD(I)+TMOVE(I,K)*ZMAT(I,K,J)
              ENDIF                                    
  800     CONTINUE                                                            
C
          DO 850 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)               THEN
                  TADD(I)=TADD(I)+TBAR(I,J)*ZRMDR(I,J)                                        
                  TBAR(I,J)=TADD(I)/DELZ(J)
                  HTC(I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*HCPICE*
     1                     DELZ(J)/DELT
              ENDIF                                              
  850     CONTINUE
  900 CONTINUE                                                                
C                                                                                  
      RETURN                                                                      
      END        
