!>\file
C!Purpose: Calculate canopy interception of rainfall and snowfall, 
C!and determine rainfall/snowfall rates at ground surface as a 
C!result of throughfall and unloading.
C!   
      SUBROUTINE CANADD(IWATER,R,TR,S,TS,RAICAN,SNOCAN,TCAN,CHCAP,
     1                  HTCC,ROFC,ROVG,PCPN,PCPG,FI,FSVF,
     2                  CWLCAP,CWFCAP,CMASS,RHOSNI,TSURX,RDRIP,SDRIP,
     3                  ILG,IL1,IL2, JL)
  
C     * NOV 22/06 - E.CHAN/D.VERSEGHY. UNCONDITIONALLY SET TR AND TS.
C     * JAN 05/05 - P.BARTLETT. CORRECT/REFINE SNOW INTERCEPTION
C     *                         CALCULATIONS.
C     * SEP 13/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 29/02 - D.VERSEGHY/S.FASSNACHT. NEW SNOW INTERCEPTION
C     *                                     ALGORITHM,
C     * JUL 24/02 - D.VERSEGHY. MOVE DIAGNOSTIC CALCULATIONS FROM
C     *                         CLASSW INTO THIS ROUTINE; CHANGE
C     *                         RHOSNI FROM CONSTANT TO VARIABLE.
C     * JUN 20/02 - D.VERSEGHY. ADDITIONAL DIAGNOSTIC CALCULATIONS.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CALCULATE CANOPY INTERCEPTION; ADD
C     *                         THROUGHFALL AND CANOPY DRIP TO
C     *                         PRECIPITATION REACHING GROUND.
C     *                         ADJUST CANOPY TEMPERATURE AND HEAT
C     *                         CAPACITY.
C
      IMPLICIT NONE
C                                                                
C     * INTEGER CONSTANTS.
C
      INTEGER IWATER,ILG,IL1,IL2,JL,I
C 
C     * INPUT/OUTPUT ARRAYS.
C
      REAL R     (ILG)  !<Rainfall rate over subarea in question \f$[m s^{-1}]\f$    
      REAL TR    (ILG)  !<Temperature of rainfall [C]  
      REAL S     (ILG)  !<Snowfall rate over subarea in question \f$[m s^{-1}]\f$
      REAL TS    (ILG)  !<Temperature of snowfall [C]
      REAL RAICAN(ILG)  !<Intercepted liquid water stored on the canopy 
                        !!\f$[kg m^{-2}] (W_{l,c})\f$
      REAL SNOCAN(ILG)  !<Intercepted frozen water stored on the canopy 
                        !!\f$[kg m^{-2}] (W_{f,c})\f$
      REAL TCAN  (ILG)  !<Temperature of vegetation canopy \f$[K] (T_c)\f$  
      REAL CHCAP (ILG)  !<Heat capacity of vegetation canopy \f$[J m^{-2} K^{-1}] (C_c)\f$
      REAL HTCC  (ILG)  !<Internal energy change of canopy due to changes 
                        !!in temperature and/or mass \f$[W m^{-2}] (I_c)\f$
      REAL ROFC  (ILG)  !<Liquid/frozen water runoff from vegetation \f$[kg m^{-2} s^{-1}]\f$  
      REAL ROVG  (ILG)  !<Liquid/frozen water runoff from vegetation to 
                        !!ground surface \f$[kg m^{-2} s^{-1}]\f$
      REAL PCPN  (ILG)  !<Precipitation incident on snow pack \f$[kg m^{-2} s^{-1}]\f$
      REAL PCPG  (ILG)  !<Precipitation incident on ground \f$[kg m^{-2} s^{-1}]\f$
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on 
                        !!modelled area \f$[ ] (X_i)\f$
      REAL FSVF  (ILG)  !<Sky view factor of surface under vegetation canopy [ ]
      REAL CWLCAP(ILG)  !<Interception storage capacity of vegetation for 
                        !!liquid water \f$[kg m^{-2}]\f$
      REAL CWFCAP(ILG)  !<Interception storage capacity of vegetation for 
                        !!frozen water \f$[kg m^{-2}] (W_{f,max})\f$
      REAL CMASS (ILG)  !<Mass of vegetation canopy \f$[kg m^{-2}]\f$
      REAL RHOSNI(ILG)  !<Density of fresh snow \f$[kg m^{-3}]\f$  
      REAL TSURX (ILG)  !<Ground or snow surface temperature of subarea [K]
C
C     * INTERNAL WORK ARRAYS.
C
      REAL RDRIP (ILG),    SDRIP (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL RTHRU,RINT,STHRU,SINT,TRCAN,TSCAN,RWXCES,SLOAD,SWXCES,
     1     SNUNLD,CHCAPI,TCANI
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
      REAL HCPW     !<Volumetric heat capacity of water \f$(4.187 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPICE   !<Volumetric heat capacity of ice \f$(1.9257 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSOL   !<Volumetric heat capacity of mineral matter 
                    !!\f$(2.25 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPOM    !<Volumetric heat capacity of organic matter 
                    !!\f$(2.50 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSND   !<Volumetric heat capacity of sand particles 
                    !!\f$(2.13 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPCLY   !<Volumetric heat capacity of fine mineral particles 
                    !!\f$(2.38 * 10^6) [J m^{-3} K^{-1}]\f$
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
      !>The calculations in this subroutine are performed if the rainfall 
      !>or snowfall rates over the modelled area are greater than zero, 
      !>or if the intercepted liquid water RAICAN or frozen water SNOCAN 
      !>is greater than zero (to allow for unloading). The throughfall of 
      !>rainfall or snowfall incident on the canopy, RTHRU or STHRU, is 
      !>calculated from FSVF, the canopy gap fraction or sky view factor. 
      !>The remaining rainfall or snowfall is assigned to interception, 
      !>as RINT and SINT. The resulting temperature of liquid water on 
      !>the canopy, TRCAN, is calculated as a weighted average of RAICAN 
      !>at the canopy temperature TCAN, and RINT at the rainfall 
      !>temperature TR. The resulting temperature of frozen water on the 
      !>canopy, TSCAN, is calculated as a weighted average of SNOCAN at 
      !>the canopy temperature TCAN, and SINT at the snowfall temperature 
      !>TS.
      !>
      DO 100 I=IL1,IL2
          RDRIP(I)=0.0
          SDRIP(I)=0.0
          IF(FI(I).GT.0. .AND. (R(I).GT.0. .OR. S(I).GT.0. .OR.
     1            RAICAN(I).GT.0. .OR. SNOCAN(I).GT.0.))           THEN
              RTHRU=R(I)*FSVF(I)                                                                
              RINT=(R(I)-RTHRU)*DELT*RHOW                                                    
              STHRU=S(I)*FSVF(I)                                                                
              SINT=(S(I)-STHRU)*DELT*RHOSNI(I)
              IF((RAICAN(I)+RINT).GT.0.)                 THEN
                  TRCAN=(RAICAN(I)*TCAN(I)+RINT*TR(I))/(RAICAN(I)+RINT)                               
              ELSE                                                                        
                  TRCAN=0.0                                                               
              ENDIF                                                                       
              IF((SNOCAN(I)+SINT).GT.0.)                 THEN 
                  TSCAN=(SNOCAN(I)*TCAN(I)+SINT*TS(I))/(SNOCAN(I)+SINT)                               
              ELSE                                                                        
                  TSCAN=0.0                                                               
              ENDIF                                                                       
C             
              !>
              !>Calculations are now done to ascertain whether the total 
              !>liquid water on the canopy exceeds the liquid water 
              !>interception capacity CWLCAP. If such is the case, this 
              !>excess is assigned to RDRIP, water dripping off the 
              !>canopy. The rainfall rate reaching the surface under the 
              !>canopy is calculated as RDRIP + RTHRU, and the 
              !>temperature of this water flux is calculated as a 
              !>weighted average of RDRIP at a temperature of TRCAN, and 
              !>RTHRU at the original rainfall temperature TR. The 
              !>remaining intercepted water becomes CWLCAP. Otherwise, 
              !>the rainfall rate reaching the surface under the canopy 
              !>is set to RTHRU, and the liquid water on the canopy 
              !>RAICAN is augmented by RINT.
              !>                           
              RWXCES=RINT+RAICAN(I)-CWLCAP(I)
              IF(RWXCES.GT.0.)                           THEN 
                  RDRIP(I)=RWXCES/(DELT*RHOW)  
                  IF((RDRIP(I)+RTHRU).GT.0.)       THEN                                           
                      TR(I)=(RDRIP(I)*TRCAN+RTHRU*TR(I))/
     1                      (RDRIP(I)+RTHRU)                             
                  ELSE                                                                    
                      TR(I)=0.0                                                              
                  ENDIF                                                                   
                  R(I)=RDRIP(I)+RTHRU                                                           
                  RAICAN(I)=CWLCAP(I)
              ELSE
                  R(I)=RTHRU                                                                 
                  RAICAN(I)=RAICAN(I)+RINT                                                      
              ENDIF
C
              !>
              !>Interception and unloading of snow on the canopy is calculated using a more complex method. The
              !>amount of snow intercepted during a snowfall event over a time step, \f$\Delta W_{f,i}\f$ , or SLOAD, is obtained from
              !>the initial intercepted snow amount \f$W_{f,c}\f$ and the interception capacity \f$W_{f,max}\f$ , following Hedstrom and
              !>Pomeroy (1998), as:
              !>
              !>\f$\Delta W_{f,i} = (W_{f,max} – W_{f,c} ) [1 – exp(-S_{int} /W_{f,max} )]\f$
              !>
              !>where \f$S_{int}\f$ is the amount of snow incident on the canopy 
              !>during the time step. The amount of snow not stored by 
              !>interception, SWXCES, is calculated as SINT – SLOAD. 
              !>Between and during precipitation events, snow is unloaded 
              !>from the canopy through wind gusts and snow 
              !>densification. These effects of these processes are 
              !>estimated using an empirical exponential relationship for 
              !>the snow unloading rate \f$W_{f,u}\f$ or SNUNLD, again following 
              !>Hedstrom and Pomeroy (1998):
              !>
              !>\f$W_{f,u} = {W_{f,c} + \Delta W_{f,i} } exp (-U \Delta t)\f$
              !>
              !>where U is a snow unloading coefficient, assigned a value 
              !>of \f$0.1 d^{-1}\f$ or \f$1.157 * 10^{-6} s{-1}\f$. The sum of SWXCES and 
              !>SNUNLD is assigned to SDRIP, the snow or frozen water 
              !>falling off the canopy. The snowfall rate reaching the 
              !>surface under the canopy is calculated as SDRIP + STHRU, 
              !>and the temperature of this water flux is calculated as 
              !>a weighted average of SDRIP at a temperature of TSCAN, 
              !>and STHRU at the original snowfall temperature TS. The 
              !>frozen water stored on the canopy is recalculated as 
              !>SNOCAN + SINT – SWXCES - SNUNLD. Otherwise, the snowfall 
              !>rate reaching the surface under the canopy is set to 
              !>STHRU, and SNOCAN is augmented by SINT.
              !>
              SLOAD=(CWFCAP(I)-SNOCAN(I))*(1.0-EXP(-SINT/CWFCAP(I)))
              SWXCES=SINT-SLOAD
              SNUNLD=(SLOAD+SNOCAN(I))*(1.0-EXP(-1.157E-6*DELT))
              IF(SWXCES.GT.0. .OR. SNUNLD.GT.0.)                 THEN 
                  SDRIP(I)=(MAX(SWXCES,0.0)+SNUNLD)/(DELT*RHOSNI(I))
                  IF((SDRIP(I)+STHRU).GT.0.)       THEN                                           
                      TS(I)=(SDRIP(I)*TSCAN+STHRU*TS(I))/
     1                      (SDRIP(I)+STHRU)                             
                  ELSE                                                                    
                      TS(I)=0.0                                                              
                  ENDIF                                                                   
                  S(I)=SDRIP(I)+STHRU                                                           
                  SNOCAN(I)=SNOCAN(I)+SINT-MAX(SWXCES,0.0)-SNUNLD
              ELSE                                                                        
                  S(I)=STHRU                                                                 
                  SNOCAN(I)=SNOCAN(I)+SINT                                                      
              ENDIF
C
              !>
              !>In the final section of the subroutine, the initial heat 
              !>capacity and temperature of the canopy are saved in 
              !>temporary variables. The new canopy heat capacity is 
              !>calculated as a weighted average over the specific heats 
              !>of the liquid and frozen water stored on the canopy and 
              !>the canopy mass. The canopy temperature is calculated as 
              !>a weighted average over the stored liquid and frozen 
              !>water at the updated temperatures TRCAN and TSCAN, and 
              !>the vegetation mass at the original temperature TCAN. 
              !>Then the change in internal energy \f$I_c\f$ of the vegetation 
              !>canopy as a result of the water movement above is 
              !>calculated as the difference in \f$I_c\f$ before and after 
              !>these processes:
              !>\f[
              !>\Delta I_c = X_i \Delta [C_c T_c ] / \Delta t
              !>\f]
              !>where \f$C_c\f$ represents the canopy heat capacity, \f$T_c\f$ the 
              !>canopy temperature, \f$\Delta t\f$ the length of the time step, and 
              !>\f$X_i\f$ the fractional coverage of the subarea under 
              !>consideration relative to the modelled area.
              !>
              !>Finally, the rainfall and snowfall temperatures are 
              !>converted to degrees C. (In the absence of precipitation, 
              !>both are set equal to the surface temperature of the 
              !>subarea to avoid floating point errors in later 
              !>subroutines.) For subareas with a snow cover 
              !>(IWATER = 2), the water running off the canopy and the 
              !>precipitation incident on the snow pack are updated using
              !>RDRIP and SDRIP. For subareas without snow cover 
              !>(IWATER = 1), the water running off the canopy is updated 
              !>using RDRIP and SDRIP, the precipitation incident on the 
              !>snow pack is augmented by SDRIP, and the precipitation 
              !>incident on bare ground is augmented by RDRIP.
              !>
              CHCAPI  =CHCAP(I)
              TCANI   =TCAN(I)
              CHCAP(I)=RAICAN(I)*SPHW+SNOCAN(I)*SPHICE+CMASS(I)*SPHVEG                                
              TCAN (I)=(RAICAN(I)*SPHW*TRCAN+SNOCAN(I)*SPHICE*TSCAN+
     1                 CMASS(I)*SPHVEG*TCAN(I))/CHCAP(I)
              HTCC (I)=HTCC(I)+FI(I)*(CHCAP(I)*TCAN(I)-CHCAPI*TCANI)/
     1                 DELT
              IF(R(I).GT.0.0)                      THEN
                  TR(I)=TR(I)-TFREZ                                                                 
              ELSE
                  TR(I)=MAX(TSURX(I)-TFREZ,0.0)
              ENDIF
              IF(S(I).GT.0.0)                      THEN
                  TS(I)=TS(I)-TFREZ                                                                 
              ELSE
                  TS(I)=MIN(TSURX(I)-TFREZ,0.0)
              ENDIF
              IF(IWATER.EQ.2) THEN
                  ROFC(I)=ROFC(I)+FI(I)*(RDRIP(I)*RHOW+SDRIP(I)*
     1                RHOSNI(I))
                  PCPN(I)=PCPN(I)+FI(I)*(RDRIP(I)*RHOW+SDRIP(I)*
     1                RHOSNI(I))
              ENDIF
              IF(IWATER.EQ.1) THEN
                  ROFC(I)=ROFC(I)+FI(I)*(RDRIP(I)*RHOW+SDRIP(I)*
     1                RHOSNI(I))
                  ROVG(I)=ROVG(I)+FI(I)*(RDRIP(I)*RHOW+SDRIP(I)*
     1                RHOSNI(I))
                  PCPN(I)=PCPN(I)+FI(I)*SDRIP(I)*RHOSNI(I)
                  PCPG(I)=PCPG(I)+FI(I)*RDRIP(I)*RHOW
              ENDIF
          ENDIF
  100 CONTINUE                                                                        
C
      RETURN                                                                      
      END
