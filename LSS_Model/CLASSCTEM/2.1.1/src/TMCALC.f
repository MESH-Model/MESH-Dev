!>\file
!!Purpose: Calculate overland flow; step ahead pond and soil layer temperatures, and check for freezing of
!!the pond and freezing or thawing of liquid or frozen water in the soil layers. Adjust pond temperature,
!!soil layer temperatures and water stores accordingly.
!!

      SUBROUTINE TMCALC(TBAR,THLIQ,THICE,HCP,TPOND,ZPOND,TSNOW,ZSNOW,
     1                  ALBSNO,RHOSNO,HCPSNO,TBASE,OVRFLW,TOVRFL,
     2                  RUNOFF,TRUNOF,HMFG,HTC,HTCS,WTRS,WTRG,
     3                  FI,TBARW,GZERO,G12,G23,GGEO,TA,WSNOW,
     4                  TCTOP,TCBOT,GFLUX,ZPLIM,THPOR,THLMIN,HCPS,
     5                  DELZW,DELZZ,DELZ,ISAND,IWF,IG,ILG,IL1,IL2,JL,N)
C
C     * FEB 22/08 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS.
C     * NOV 20/06 - D.VERSEGHY. ADD GEOTHERMAL HEAT FLUX.
C     * APR 03/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW,
C     * DEC 07/05 - D.VERSEGHY. REVISED HEAT FLUX CALCULATION BETWEEN
C     *                         SOIL AND TBASE IN LAYER 3.
C     * OCT 06/05 - D.VERSEGHY. MODIFY FOR CASES WHERE IG>3.
C     * MAR 30/05 - D.VERSEGHY. ADD RUNOFF TEMPERATURE CALCULATION.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * MAY 16/03 - Y.DELAGE/D.VERSEGHY. BUGFIX IN FREEZING/
C     *                                  THAWING CALCULATIONS
C     *                                  (PRESENT SINCE V.2.7)
C     * JUN 17/02 - D.VERSEGHY. REMOVE INCORPORATION OF PONDED WATER
C     *                         INTO FIRST LAYER SOIL MOISTURE;
C     *                         UPDATE SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * DEC 12/01 - D.VERSEGHY. PASS IN SWITCH TO DO SURFACE FLOW
C     *                         CALCULATION ONLY IF WATFLOOD ROUTINES
C     *                         ARE NOT BEING RUN.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS; INTRODUCE CALCULATION
C     *                         OF OVERLAND FLOW.
C     * AUG 30/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         VARIABLE SURFACE DETENTION CAPACITY
C     *                         IMPLEMENTED.
C     * AUG 18/95 - D.VERSEGHY. REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS AND FRACTIONAL
C     *                         ORGANIC MATTER CONTENT.
C     * AUG 16/95 - D.VERSEGHY. TWO NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATIONS OF TBAR AND HTC;
C     *                         ALLOW SPECIFICATION OF LIMITING POND 
C     *                         DEPTH "PNDLIM" (PARALLEL CHANGES
C     *                         MADE SIMULTANEOUSLY IN CLASSW).
C     * NOV 01/93 - D.VERSEGHY. CLASS - VERSION 2.2.
C     *                         REVISED VERSION WITH IN-LINED CODE
C     *                         FROM TWCALC AND TFREEZ TO PERMIT 
C     *                         FREEZING AND THAWING OF SOIL LAYERS
C     *                         AT THE END OF EACH TIME STEP.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. STORE PONDED WATER INTO FIRST 
C     *                         SOIL LAYER LIQUID WATER; STEP
C     *                         AHEAD SOIL LAYER TEMPERATURES 
C     *                         USING CONDUCTION HEAT FLUX
C     *                         CALCULATED AT TOP AND BOTTOM
C     *                         OF EACH LAYER.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IWF,IG,ILG,IL1,IL2,JL,I,J,N
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL TBAR  (ILG,IG) !<Temperature of soil layer \f$[C] (T_g)\f$
      REAL THLIQ (ILG,IG) !<Volumetric liquid water content of soil layer \f$(\theta_l) [m^3 m^{-3}]\f$ 
      REAL THICE (ILG,IG) !<Volumetric frozen water content of soil layer \f$(\theta_i) [m^3 m^{-3}]\f$
      REAL HCP   (ILG,IG) !<Heat capacity of soil layer \f$[J m^{-3} K^{-1}] (C_g)\f$
      REAL HMFG  (ILG,IG) !<Energy associated with freezing or thawing of water in soil layer \f$[W m^{-2}]\f$
      REAL HTC   (ILG,IG) !<Internal energy change of soil layer due to conduction and/or 
                          !<change in mass \f$[W m^{-2}] (I_g)\f$
C
      REAL TPOND (ILG) !<Temperature of ponded water \f$[C] (T_p)\f$
      REAL ZPOND (ILG) !<Depth of ponded water \f$[m] (z_p)\f$
      REAL TSNOW (ILG) !<Temperature of the snow pack \f$[C] (T_s)\f$
      REAL ZSNOW (ILG) !<Depth of snow pack \f$[m] (z_g)\f$
      REAL ALBSNO(ILG) !<Albedo of snow [ ]
      REAL RHOSNO(ILG) !<Density of snow pack \f$(\rho_s) [kg m^{-3}]\f$
      REAL HCPSNO(ILG) !<Heat capacity of snow pack \f$[J m^{-3} K^{-1}] (C_s)\f$
      REAL TBASE (ILG) !<Temperature of bedrock in third soil layer, if only three layers are being modelled [K]
      REAL OVRFLW(ILG) !<Overland flow from top of soil column [m]
      REAL TOVRFL(ILG) !<Temperature of overland flow from top of soil column [K]
      REAL RUNOFF(ILG) !<Total runoff from soil column [m]
      REAL TRUNOF(ILG) !<Temperature of total runoff from soil column [K]
      REAL HTCS  (ILG) !<Internal energy change of snow pack due to conduction and/or 
                       !<change in mass \f$[W m^{-2}] (I_s)\f$
      REAL WTRS  (ILG) !<Water transferred into or out of the snow pack \f$[kg m^{-2} s^{-1}]\f$
      REAL WTRG  (ILG) !<Water transferred into or out of the soil \f$[kg m^{-2} s^{-1}]\f$
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)    !<Fractional coverage of subarea in question on modelled area \f$[ ] (X_i)\f$
      REAL TBARW (ILG,IG) !<Temperature of water in soil layer [C]
      REAL GZERO (ILG)    !<Heat flow into soil surface \f$[W m^{-2}]\f$
      REAL G12   (ILG)    !<Heat flow between first and second soil layers \f$[W m^{-2}]\f$
      REAL G23   (ILG)    !<Heat flow between second and third soil layers \f$[W m^{-2}]\f$
      REAL GGEO  (ILG)    !<Geothermal heat flux at bottom of soil profile \f$[W m^{-2}]\f$
      REAL TA    (ILG)    !<Air temperature [K]
      REAL WSNOW (ILG)    !<Liquid water content of snow pack \f$[kg m^{-2}] (w_s)\f$
      REAL TCTOP (ILG,IG) !<Thermal conductivity of soil at top of soil layer \f$[W m^{-1} K^{-1}]\f$
      REAL TCBOT (ILG,IG) !<Thermal conductivity of soil at bottom of soil layer \f$[W m^{-1} K^{-1}]\f$
      REAL ZPLIM (ILG)    !<Limiting depth of ponded water [m]
C
C     * SOIL INFORMATION ARRAYS.
C
      REAL THPOR (ILG,IG) !<Pore volume in soil layer \f$(\theta_p) [m^3 m^{-3}]\f$
      REAL THLMIN(ILG,IG) !<Residual soil liquid water content remaining after 
                          !<freezing or evaporation \f$(\theta_r) [m^3 m^{-3}]\f$
      REAL HCPS  (ILG,IG) !<Heat capacity of soil material \f$[J m^{-3} K^{-1}] (C_m)\f$
      REAL DELZW (ILG,IG) !<Permeable thickness of soil layer \f$(\Delta z_{g,w}) [m]\f$
      REAL DELZZ (ILG,IG) !<Soil layer thicknesses to bottom of permeable depth 
                          !<for standard three-layer configuration,
                          !<or to bottom of thermal depth for multiple layers \f$(\Delta z_{g,z}) [m]\f$
      REAL DELZ  (IG)     !<Overall thickness of soil layer [m]
C
      INTEGER ISAND (ILG,IG) !<Sand content flag
C
C     * TEMPORARY VARIABLES.
C
      REAL GFLUX (ILG,IG) !<Heat flow between soil layers \f$[W m^{-2}]\f$
C
      REAL GP1,ZFREZ,HADD,HCONV,TTEST,TLIM,HEXCES,THFREZ,THMELT,G3B
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,
     1     SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C
C     * CALCULATE SUBSURFACE AND OVERLAND RUNOFF TERMS; ADJUST
C     * SURFACE PONDING DEPTH.
C

!>
!!If a full-scale hydrological modelling application is not being run, that is, if only vertical fluxes of energy
!!and moisture are being modelled, the flag IWF is pre-set to zero. In this case, overland flow of water is
!!treated using a simple approach: if the ponded depth of water on the soil surface ZPOND exceeds a
!!limiting value ZPLIM (which varies by land surface type), the excess is assigned to overland flow. These
!!calculations are carried out in loop 100, for all modelled areas except ice sheets (which are handled in
!!subroutine ICEBAL). The overall runoff from the modelled area in question, RUNOFF, is incremented
!!by the excess ponded water, and the overland flow for the whole grid cell OVRFLW is incremented by
!!the product of the excess ponded water and the fractional area of the grid cell. The temperature of the
!!overall runoff from the modelled area TRUNOF, and the temperature of the overland flow for the grid
!!cell TOVRFL, are calculated as weighted averages over their previous values and the ponded water
!!temperature TPOND. Finally, ZPOND is set to ZPLIM.
!!

      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4 .AND. IWF.EQ.0 .AND.
     1                         (ZPOND(I)-ZPLIM(I)).GT.1.0E-8)     THEN
              TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TPOND(I)+TFREZ)*
     1                  (ZPOND(I)-ZPLIM(I)))/(RUNOFF(I)+
     2                  (ZPOND(I)-ZPLIM(I)))
              RUNOFF(I)=RUNOFF(I)+(ZPOND(I)-ZPLIM(I))
              TOVRFL(I)=(TOVRFL(I)*OVRFLW(I)+(TPOND(I)+TFREZ)*
     1                  FI(I)*(ZPOND(I)-ZPLIM(I)))/(OVRFLW(I)+
     2                  FI(I)*(ZPOND(I)-ZPLIM(I)))
              OVRFLW(I)=OVRFLW(I)+FI(I)*(ZPOND(I)-ZPLIM(I))
              ZPOND(I)=MIN(ZPOND(I),ZPLIM(I))
          ENDIF
  100 CONTINUE
C
C     * UPDATE SOIL TEMPERATURES AFTER GROUND WATER MOVEMENT.
C
!>
!!In loop 200, the calculation of the change in internal energy HTC within the soil layers due to movement
!!of soil water (addressed in the preceding subroutines GRINFL and GRDRAN) is completed. (The initial
!!step was performed at the end of subroutine TWCALC.) The volumetric heat capacity of the soil layers
!!HCP is recalculated as a weighted average over the volumetric contents of liquid water, frozen water and
!!soil particles. The temperature TBAR of each soil layer is recalculated as a weighted average of the liquid
!!water temperature TBARW resulting from soil water movement, and the previous soil layer temperature.
!!As noted in the documentation for subroutine TWCALC., TBARW and HCP apply only over the
!!permeable depth of the soil layer, DELZW, whereas TBAR applies over the whole depth, DELZZ, and is
!!associated with HCPSND, the volumetric heat capacity assigned to rock, in the interval DELZZ-
!!DELZW. (For the default three-layer version of CLASS, DELZZ=DELZW in the third soil layer; see
!!the documentation of loop 550 below.)
!!

      DO 200 J=1,IG
      DO 200 I=IL1,IL2



          IF(FI(I).GT.0. .AND. DELZW(I,J).GT.0. .AND. ISAND(I,1).GT.-4)
     1                                                            THEN


              HTC(I,J)=HTC(I,J)+FI(I)*((TBARW(I,J)+TFREZ)*
     1                 HCPW*THLIQ(I,J)+(TBAR(I,J)+TFREZ)*
     2                 HCPICE*THICE(I,J))*DELZW(I,J)/DELT

              HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                 HCPS(I,J)*(1.-THPOR(I,J))                                                   
              TBAR(I,J)=((TBARW(I,J)+TFREZ)*HCPW*THLIQ(I,J)*
     1                  DELZW(I,J)+(TBAR(I,J)+TFREZ)*((HCPICE*
     2                  THICE(I,J)+HCPS(I,J)*(1.-THPOR(I,J)))*
     3                  DELZW(I,J)+HCPSND*(DELZZ(I,J)-DELZW(I,J))))/
     4                  (HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     5                  DELZW(I,J)))-TFREZ


          ENDIF


  200 CONTINUE
C
C     * STEP AHEAD POND TEMPERATURE; CHECK FOR FREEZING, AND ADD
C     * FROZEN WATER TO SNOW PACK.
C
!>
!!In loop 300 the calculation of the change of internal energy within the first soil layer due to changes in the
!!surface ponded water is completed (for diagnostic purposes, the first level of HTC includes the internal
!!energy of both the ponded water and the first soil layer). (The initial step of this calculation was
!!performed at the end of subroutine TFREEZ.) The flow of heat between the bottom of the ponded
!!water and the top of the first soil layer, GP1, is calculated by assuming a linear variation with depth of the
!!heat flux between the top of the pond, GZERO, and between the first and second soil layers, G12. Since
!!the heat flux G(z) varies directly with the temperature gradient dT(z)/dz, it can be seen that this approach
!!is consistent with the assumption made in the CLASST subroutines that T(z) is a quadratic function of
!!depth (and therefore that its first derivative is a linear function of depth). The temperature of the ponded
!!water is stepped ahead using GZERO and GP1, and GZERO is reset to GP1 for use in the later soil
!!temperature calculations.
!!
      DO 300 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4 .AND. ZPOND(I).GT.0.0)
     1                                                             THEN
              HTC(I,1)=HTC(I,1)+FI(I)*HCPW*(TPOND(I)+TFREZ)*
     1            ZPOND(I)/DELT
              GP1=ZPOND(I)*(G12(I)-GZERO(I))/(ZPOND(I)+DELZZ(I,1))+
     1            GZERO(I)
              TPOND(I) =TPOND(I)+(GZERO(I)-GP1)*DELT/(HCPW*ZPOND(I))
              GZERO(I)=GP1
          ENDIF
  300 CONTINUE
C
!>
!!In the 400 loop, a check is performed to ascertain whether the ponded water temperature has fallen below
!!0 C as a result of the above temperature change. If so, calculations are carried out analogous to those
!!done in TFREEZ. A recalculation of the available energy in the snow pack, HTCS, is initiated. The
!!available energy sink HADD is calculated from TPOND and ZPOND, and TPOND is set to 0 C. The
!!amount of energy required to freeze the whole depth of ponded water is calculated as HCONV. An
!!update of the first level value of HTC, reflecting the freezing of ponded water, is initiated. If HADD <
!!HCONV, the available energy sink is sufficient to freeze only part of the ponded water. This depth
!!ZFREZ is calculated from HADD, and subtracted from ZPOND. ZFREZ is then converted from a
!!depth of water to a depth of ice, and is reassigned as part of the snow pack. The snow temperature,
!!density, heat capacity and depth are recalculated as weighted averages of the pre-existing snow properties
!!and those of the frozen water. If there was no snow on the ground to begin with, the snow albedo is
!!initialized to its minimum background value of 0.50.
!!
!!If HADD > HCONV, the available energy sink is sufficient to freeze the whole depth of ponded water
!!and then to decrease the frozen water temperature to below 0 C. ZFREZ is evaluated as ZPOND
!!converted to a depth of ice, and the remaining energy sink HADD-HCONV is used to calculated a test
!!temperature TTEST of the newly formed ice. In order to avoid unphysical overshoots of the frozen
!!water temperature, a limiting temperature TLIM is determined as the minimum of the snow temperature
!!and the first level soil temperature if there is pre-existing snow, and as the minimum of the air
!!temperature and the first level soil temperature if not. If TTEST < TLIM, the excess energy sink required
!!to decrease the frozen water temperature from TLIM to TTEST is calculated and added to GZERO, and
!!the new frozen water temperature is assigned as TLIM; otherwise it is assigned as TTEST. The energy
!!sink used to cool the frozen water from 0 C to TLIM or TTEST is decremented from the first level of
!!HTC, and the snow temperature is recalculated as the weighted average of the pre-existing snow
!!temperature and the frozen water temperature. If there was no pre-existing snow, the snow albedo is
!!initialized to the background value of 0.50. The density, heat capacity and depth of the snow are
!!recalculated as above. Finally, the recalculations of the internal energy diagnostic variables HTC and
!!HTCS are completed; and ZFREZ is used to update the diagnostic variable HMFG describing the energy
!!associated with phase changes of water in soil layers, and the diagnostic variables WTRS and WTRG
!!describing transfers of water into or out of the snow and soil respectively.
!!
      DO 400 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4 .AND. ZPOND(I).GT.0. 
     1                   .AND. TPOND(I).LT.0.)
     1                                                              THEN
             HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1               ZSNOW(I)/DELT
             ZFREZ=0.0
             HADD=-TPOND(I)*HCPW*ZPOND(I)                                              
             TPOND(I)=0.0                                                               
             HCONV=CLHMLT*RHOW*ZPOND(I)                                               
             HTC(I,1)=HTC(I,1)-FI(I)*HCPW*TFREZ*ZPOND(I)/DELT
             IF(HADD.LE.HCONV)             THEN                                                      
                ZFREZ=HADD/(CLHMLT*RHOW)                                                
                ZPOND(I)=ZPOND(I)-ZFREZ                                                       
                ZFREZ=ZFREZ*RHOW/RHOICE                                                 
                IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                TSNOW(I)=TSNOW(I)*HCPSNO(I)*ZSNOW(I)/(HCPSNO(I)*ZSNOW(I)
     1                  +HCPICE*ZFREZ)                    
                RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/(ZSNOW(I)
     1                   +ZFREZ)                        
                IF(ZSNOW(I).GT.0.0) THEN
                    HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                   (RHOW*ZSNOW(I))
                ELSE
                    HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
                ENDIF
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
             ELSE                                                                        
                HADD=HADD-HCONV                                                         
                ZFREZ=ZPOND(I)*RHOW/RHOICE                                                 
                TTEST=-HADD/(HCPICE*ZFREZ)                                              
                IF(ZSNOW(I).GT.0.0) THEN
                    TLIM=MIN(TSNOW(I),TBAR(I,1))
                ELSE
                    TLIM=MIN(TA(I)-TFREZ,TBAR(I,1))
                ENDIF
                IF(TTEST.LT.TLIM)       THEN                                    
                   HEXCES=HADD+TLIM*HCPICE*ZFREZ                         
                   GZERO(I)=GZERO(I)-HEXCES/DELT                                             
                   HTC(I,1)=HTC(I,1)+FI(I)*(HADD-HEXCES)/DELT
                   TSNOW(I)=(TSNOW(I)*HCPSNO(I)*ZSNOW(I)+
     1                      TLIM*HCPICE*ZFREZ)          
     2                      /(HCPSNO(I)*ZSNOW(I)+HCPICE*ZFREZ)                                    
                ELSE                                                                    
                   TSNOW(I)=(TSNOW(I)*HCPSNO(I)*ZSNOW(I)+TTEST*HCPICE*
     1                       ZFREZ)/(HCPSNO(I)*ZSNOW(I)+HCPICE*ZFREZ)                                    
                   HTC(I,1)=HTC(I,1)+FI(I)*HADD/DELT
                ENDIF                                                                   
                IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/(ZSNOW(I)+
     1                     ZFREZ)                        
                IF(ZSNOW(I).GT.0.0) THEN
                    HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                   (RHOW*ZSNOW(I))
                ELSE
                    HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
                ENDIF
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                ZPOND(I)=0.0                                                               
             ENDIF                                                                       
             HTC (I,1)=HTC (I,1)+FI(I)*HCPW*TFREZ*ZPOND(I)/DELT
             HMFG(I,1)=HMFG(I,1)-FI(I)*CLHMLT*RHOICE*ZFREZ/DELT
             WTRS(I)=WTRS(I)+FI(I)*ZFREZ*RHOICE/DELT
             WTRG(I)=WTRG(I)-FI(I)*ZFREZ*RHOICE/DELT
             HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
          ENDIF
  400 CONTINUE
C
C     * STEP AHEAD SOIL LAYER TEMPERATURES; CHECK FOR FREEZING OR
C     * THAWING.
C
      IF(IG.GT.3) THEN
!>
!!In the 500, 550 and 600 loops, the heat fluxes between soil layers and the updated temperatures for the
!!soil layers are calculated according to the discretization approach that has been selected for the model run.
!!It is assumed that the first two layer thicknesses are always set to 0.10 and 0.25 m respectively, but two
!!possible options exist for the treatment of deeper layers. In the operational, "three-layer" configuration, a
!!single third soil layer is modelled of thickness 3.75 m, with a user-specified permeable thickness DELZW.
!!TBAR of the third layer is taken to apply to this permeable thickness, whereas a separate bedrock
!!temperature, TBASE, is carried for the impermeable thickness, represented by DELZ-DELZW. This
!!strategy is adopted so that temperature variations caused by melting and freezing of water are confined to
!!the permeable thickness. In the "multi-layer" configuration, there can be any number of deeper layers, of
!!thicknesses specified by the user (with the proviso that the thicknesses not be small enough to lead to
!!numerical instability in the forward explicit time-stepping scheme). In this configuration it is deemed
!!unnecessary to carry a separate calculation of TBASE.
!!
!!In the CLASST subroutines, the heat fluxes at the top of the first soil layer, and between the first and
!!second and the second and third soil layers, were determined. In loop 500, the fluxes between the
!!remaining soil layers are calculated for the multi-layer configuration, and assigned to the heat flux vector
!!GFLUX. Since the temperature variation is increasingly damped with depth, a simple linearization of the
!!temperature profile is used. The expression for the ground heat flux at depth z, G(z), which depends on
!!the thermal conductivity \f$\lambda (z)\f$ and the temperature gradient, is given as:
!!\f$G(z) = \lambda (z) dT(z)/dz\f$
!!The linearized form is written as:
!!\f$G_j = (\lambda_{j-1} + \lambda_j ) (T_{j-1} - T_j ) / (\Delta z_{j-1} - \Delta z_j )\f$
!!where G j is the heat flux at the top of layer j, and \f$\lambda_j\f$ , \f$T_j\f$ and \f$\Delta z_j\f$ refer to the thermal conductivity,
!!temperature and heat capacity of the layer.
!!
      DO 500 J=4,IG
          DO 500 I=IL1,IL2
              IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4)            THEN
                  GFLUX(I,J)=(TCBOT(I,J-1)+TCTOP(I,J))*(TBAR(I,J-1)-
     1                       TBAR(I,J))/(DELZ(J-1)+DELZ(J))
              ENDIF
  500     CONTINUE
      ENDIF
C     
!>
!!In the 550 loop, the first and second soil layer temperatures are stepped ahead using GZERO, G12 and
!!G23. (Recall that the calculated heat capacity HCP applies to the permeable thickness DELZW, and the
!!heat capacity of rock HCPSND applies to the rest of the layer thickness, DELZZ-DELZW.) If the three-
!!layer configuration is being used, then a check is carried out to ascertain whether the permeable thickness
!!DELZZ of the third layer is greater than zero. If so, and if DELZZ is not effectively equal to DELZ, the
!!heat flow G3B at the interface between the permeable thickness and the underlying bedrock is calculated
!!using the linearized heat flow equation given above; TBAR is updated using the difference between G23
!!and G3B, and TBASE is updated using the difference between G3B and GGEO, the geothermal heat
!!flux at the bottom of the soil profile. If DELZZ is equal to DELZ, the whole layer is permeable and
!!TBAR is updated using the difference between G23 and GGEO. If DELZZ is zero, the whole layer is
!!impermeable, and TBASE is updated using the difference between G23 and GGEO. HTC for the third
!!layer is updated with the GGEO value. Finally, if the multi-layer configuration is being used, TBAR of
!!the third layer is updated using G23, the flux at the top of the layer. For diagnostic purposes, the first
!!three levels of the GFLUX vector are assigned as GZERO, G12 and G23 respectively.
!!
      DO 550 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4)               THEN
              TBAR(I,1)=TBAR(I,1)+(GZERO(I)-G12(I))*DELT/
     1                  (HCP(I,1)*DELZW(I,1)+HCPSND*(DELZZ(I,1)-
     2                  DELZW(I,1)))
              TBAR(I,2)=TBAR(I,2)+(G12  (I)-G23(I))*DELT/
     1                  (HCP(I,2)*DELZW(I,2)+HCPSND*(DELZZ(I,2)-
     2                  DELZW(I,2)))                         
              IF(IG.EQ.3)                                THEN
                  IF(DELZZ(I,3).GT.0.0)       THEN
                      IF(DELZZ(I,IG).LT.(DELZ(IG)-1.0E-5)) THEN
                          G3B=(TCBOT(I,3)+TCTOP(I,3))*(TBAR(I,3)-
     1                        TBASE(I))/DELZ(3)
                          TBAR(I,3)=TBAR(I,3)+(G23(I)-G3B)*DELT/
     1                        (HCP(I,3)*DELZZ(I,3))
                          TBASE(I)=TBASE(I)+(G3B-GGEO(I))*DELT/
     2                        (HCPSND*(DELZ(3)-DELZZ(I,3)))   
                      ELSE
                          TBAR(I,3)=TBAR(I,3)+(G23(I)-GGEO(I))*DELT/
     1                        (HCP(I,3)*DELZW(I,3))
                      ENDIF
                  ELSE
                      TBASE(I)=TBASE(I)+(G23(I)-GGEO(I))*DELT/
     1                    (HCPSND*DELZ(3))
                  ENDIF
                  HTC(I,3)=HTC(I,3)-FI(I)*GGEO(I)
              ELSE
                  TBAR(I,3)=TBAR(I,3)+G23(I)*DELT/
     1                      (HCP(I,3)*DELZW(I,3)+HCPSND*(DELZ(3)-
     2                      DELZW(I,3)))                         
              ENDIF
              GFLUX(I,1)=GZERO(I)
              GFLUX(I,2)=G12(I)
              GFLUX(I,3)=G23(I)
          ENDIF
  550 CONTINUE
C
!>
!!In the 600 loop, the remaining soil layer temperature calculations are done for layers 3 and deeper, for the
!!multi-layer configuration. At the beginning and end of the loop an updated calculation of HTC for the
!!layer in question is initiated and completed respectively. For the third layer, TBAR is updated using the
!!heat flux at the bottom of the layer; for the last layer, TBAR is updated using the difference between
!!GFLUX at the top of the layer and GGEO at the bottom. For the intermediate layers, TBAR is
!!calculated using the difference between the GFLUX values at the top and bottom of the layer.
!!
      DO 600 J=3,IG                                                               
      DO 600 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).GT.-4 .AND. IG.GT.3)   THEN
              HTC(I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
              IF(J.EQ.3)                                THEN
                  TBAR(I,J)=TBAR(I,J)-GFLUX(I,J+1)*DELT/
     1                      (HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                      DELZW(I,J)))                         
              ELSEIF(J.EQ.IG)                           THEN
                  TBAR(I,J)=TBAR(I,J)+(GFLUX(I,J)-GGEO(I))*DELT/
     1                      (HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                      DELZW(I,J)))                         
              ELSE
                  TBAR(I,J)=TBAR(I,J)+(GFLUX(I,J)-GFLUX(I,J+1))*DELT/
     1                  (HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                  DELZW(I,J)))                         
              ENDIF
              HTC(I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
          ENDIF
  600 CONTINUE
C     
!>
!!In the 700 loop, checks are carried out to determine whether, as a result of the forward stepping of the
!!temperature, TBAR has fallen below 0 C while liquid water still exists in the layer, or risen above 0 C
!!while frozen water still exists in the layer. If either occurs, adjustments to the water content are
!!performed analogous to those done in subroutine TWCALC. Again, at the beginning and end of the loop
!!an updated calculation of HTC for each layer in turn is initiated and completed respectively.
!!
!!If the soil layer temperature is less than 0 C and the volumetric liquid water content THLIQ of the layer is
!!greater than the residual water content THLMIN, the water content THFREZ that can be frozen by the
!!available energy sink is calculated from TBAR and the weighted average of HCP and HCPSND. If
!!THFREZ \f$\leq\f$ THLIQ - THLMIN, all of the available energy sink is used to freeze part of the liquid water
!!content in the permeable part of the soil layer. The amount of energy involved is subtracted from HTC
!!and added to HMFG. THFREZ is subtracted from THLIQ, converted to an ice volume and added to
!!THICE. HCP is recalculated, and the layer temperature is set to 0 C. Otherwise, all of the liquid water
!!content of the layer above THLMIN is converted to frozen water, and HMFG and HTC are recalculated
!!to reflect this. HCP is recomputed, and the remaining energy sink HADD is applied to decreasing the
!!temperature of the soil layer.
!!
!!If the soil layer temperature is greater than 0 C and the volumetric ice content THICE of the layer is
!!greater than zero, the ice content THMELT that can be melted by the available energy is calculated from
!!TBAR and the weighted average of HCP and HCPSND. If THMELT \f$\leq\f$ THICE, all of the available
!!energy is used to melt part of the frozen water content of the permeable part of the layer. The amount of
!!energy involved is subtracted from HTC and added to HMFG. THMELT is subtracted from THICE,
!!converted to a liquid water volume and added to THLIQ; HCP is recalculated and the layer temperature
!!is set to 0 C. Otherwise, all of the frozen water content of the layer is converted to liquid water, and
!!HMFG and HTC are recalculated to reflect this. HCP is recomputed, and the remaining energy HADD
!!is applied to increasing the temperature of the soil layer.
!!
      DO 700 J=1,IG                                                               
      DO 700 I=IL1,IL2
          IF(FI(I).GT.0. .AND. DELZW(I,J).GT.0. .AND. ISAND(I,1).GT.-4)
     1                                                            THEN
              HTC(I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
              IF(TBAR(I,J).LT.0. .AND. THLIQ(I,J).GT.THLMIN(I,J))
     1                                                           THEN                        
                  THFREZ=-(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                    DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOW*
     2                    DELZW(I,J))                              
                  IF(THFREZ.LE.(THLIQ(I,J)-THLMIN(I,J))) THEN                         
                      HMFG(I,J)=HMFG(I,J)-FI(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)-FI(I)*THFREZ*CLHMLT*
     1                          RHOW*DELZW(I,J)/DELT
                      THLIQ(I,J)=THLIQ(I,J)-THFREZ                                        
                      THICE(I,J)=THICE(I,J)+THFREZ*RHOW/RHOICE                            
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=0.0                                                   
                  ELSE                                                                
                      HMFG(I,J)=HMFG(I,J)-FI(I)*(THLIQ(I,J)-
     1                    THLMIN(I,J))*CLHMLT*RHOW*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)-FI(I)*(THLIQ(I,J)-
     1                    THLMIN(I,J))*CLHMLT*RHOW*DELZW(I,J)/DELT
                      HADD=(THFREZ-(THLIQ(I,J)-THLMIN(I,J)))*CLHMLT*
     1                     RHOW*DELZW(I,J)
                      THICE(I,J)=THICE(I,J)+(THLIQ(I,J)-
     1                           THLMIN(I,J))*RHOW/RHOICE          
                      THLIQ(I,J)=THLMIN(I,J)
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=-HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                  ENDIF                                                               
              ENDIF
C                                                                   
              IF(TBAR(I,J).GT.0. .AND. THICE(I,J).GT.0.)        THEN                           
                  THMELT=(HCP(I,J)*DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     1                    DELZW(I,J)))*TBAR(I,J)/(CLHMLT*RHOICE*
     2                    DELZW(I,J))                             
                  IF(THMELT.LE.THICE(I,J))                 THEN 
                      HMFG(I,J)=HMFG(I,J)+FI(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)+FI(I)*THMELT*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      THICE(I,J)=THICE(I,J)-THMELT                                        
                      THLIQ(I,J)=THLIQ(I,J)+THMELT*RHOICE/RHOW                            
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=0.0                                                   
                  ELSE                                                                
                      HMFG(I,J)=HMFG(I,J)+FI(I)*THICE(I,J)*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HTC(I,J)=HTC(I,J)+FI(I)*THICE(I,J)*CLHMLT*
     1                          RHOICE*DELZW(I,J)/DELT
                      HADD=(THMELT-THICE(I,J))*CLHMLT*RHOICE*
     1                          DELZW(I,J)
                      THLIQ(I,J)=THLIQ(I,J)+THICE(I,J)*RHOICE/RHOW                          
                      THICE(I,J)=0.0                                                    
                      HCP(I,J)=HCPW*THLIQ(I,J)+HCPICE*THICE(I,J)+
     1                           HCPS(I,J)*(1.-THPOR(I,J))
                      TBAR (I,J)=HADD/(HCP(I,J)*DELZW(I,J)+HCPSND*
     1                           (DELZZ(I,J)-DELZW(I,J)))
                  ENDIF                                                               
              ENDIF
              HTC(I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*(HCP(I,J)*
     1                    DELZW(I,J)+HCPSND*(DELZZ(I,J)-
     2                    DELZW(I,J)))/DELT
          ENDIF
          

  700 CONTINUE
C                                                                                  
      RETURN                                                                      
      END 
