!>\file      
C!Purpose: Update liquid and frozen water stores on canopy and in soil in 
C!response to calculated sublimation, evaporation and transpiration
C!rates.
C!         
      SUBROUTINE CANVAP(EVAP,SUBL,RAICAN,SNOCAN,TCAN,THLIQ,TBAR,ZSNOW,
     1                  WLOST,CHCAP,QFCF,QFCL,QFN,QFC,HTCC,HTCS,HTC,
     2                  FI,CMASS,TSNOW,HCPSNO,RHOSNO,FROOT,THPOR,
     3                  THLMIN,DELZW,EVLOST,RLOST,IROOT,
     4                  IG,ILG,IL1,IL2,JL,N   )
                                                            
C     * SEP 15/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3.
C     * SEP 13/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 20/02 - D.VERSEGHY. TIDY UP SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE SOIL 
C     *                         PERMEABLE DEPTH.
C     * DEC 30/96 - D.VERSEGHY. CLASS - VERSION 2.6.
C     *                         BUGFIXES IN CALCULATION OF QFN AND 
C     *                         QFC.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         RATIONALIZE CALCULATION OF WLOST;
C     *                         REFINE CALCULATION OF QFCL.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3. 
C     *                         ADDITIONAL DIAGNOSTIC CALCULATIONS -
C     *                         HTCC AND HTC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C                                        NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CALCULATE ACTUAL EVAPORATION, 
C     *                         SUBLIMATION AND TRANSPIRATION FROM
C     *                         VEGETATION CANOPY.
C
      IMPLICIT NONE
C                                                                
C     * INTEGER CONSTANTS.
C
      INTEGER IG,ILG,IL1,IL2,JL,I,J,N
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL THLIQ (ILG,IG)   !<Volumetric liquid water content of soil 
                            !layer \f$[m^3 m^{-3}] (\theta_l)\f$ 
      REAL TBAR  (ILG,IG)   !<Temperature of soil layer \f$[K] (T_g)\f$ 
      REAL QFC   (ILG,IG)   !<Transpired water removed from soil layer \f$[kg m^{-2} s^{-1}]\f$
      REAL HTC   (ILG,IG)   !<Internal energy change of soil layer due to 
                            !!conduction and/or change in mass \f$[W m^{-2}] (I_g)\f$
C
      REAL EVAP  (ILG)  !<Evapotranspiration rate from vegetation canopy \f$[m s^{-1}]\f$
      REAL SUBL  (ILG)  !<Calculated sublimation rate from vegetation canopy \f$[m s^{-1}]\f$  
      REAL RAICAN(ILG)  !<Intercepted liquid water stored on the canopy \f$[kg m^{-2}]\f$  
      REAL SNOCAN(ILG)  !<Intercepted frozen water stored on the canopy \f$[kg m^{-2}]\f$
      REAL TCAN  (ILG)  !<Temperature of vegetation canopy \f$[K] (T_c)\f$
      REAL ZSNOW (ILG)  !<Depth of snow pack \f$[m] (z_g)\f$
      REAL WLOST (ILG)  !<Residual amount of water that cannot be 
                        !!supplied by surface stores \f$[kg m^{-2}]\f$
      REAL CHCAP (ILG)  !<Heat capacity of vegetation canopy \f$[J m^{-2} K^{-1}] (C_c)\f$
      REAL QFCF  (ILG)  !<Sublimation from frozen water in canopy 
                        !!interception store \f$[kg m^{-2} s^{-1}]\f$
      REAL QFCL  (ILG)  !<Evaporation from liquid water in canopy 
                        !!interception store \f$[kg m^{-2} s^{-1} ]\f$
      REAL QFN   (ILG)  !<Sublimation from snow pack \f$[kg m^{-2} s^{-1}]\f$
      REAL HTCC  (ILG)  !<Internal energy change of canopy due to changes 
                        !!in temperature and/or mass \f$[W m^{-2}] (I_c)\f$
      REAL HTCS  (ILG)  !<Internal energy change of snow pack due to 
                        !!conduction and/or change in mass \f$[W m^{-2}] (I_s)\f$
  
C
C     * INPUT ARRAYS.
C
      REAL FROOT (ILG,IG)   !<Fractional contribution of soil layer to 
                            !!transpiration [ ] 
      REAL THPOR(ILG,IG)    !<Pore volume in soil layer \f$[m^3 m^{-3}]\f$
      REAL THLMIN(ILG,IG)   !<Residual soil liquid water content 
                            !!remaining after freezing or evaporation \f$[m^3 m^{-3}]\f$
      REAL DELZW (ILG,IG)   !<Permeable depth of soil layer \f$[m] (\Delta z_{g,w})\f$
C
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on 
                        !!modelled area \f$[ ] (X_i)\f$
      REAL CMASS (ILG)  !<Mass of vegetation canopy \f$[kg m^{-2}]\f$  
      REAL TSNOW (ILG)  !<Temperature of the snow pack [C]  
      REAL HCPSNO(ILG)  !<Heat capacity of snow pack \f$[J m^{-3} K^{-1}] (C_s)\f$ 
      REAL RHOSNO(ILG)  !<Density of snow pack \f$[kg m^{-3}]\f$
C
C     * WORK ARRAYS.
C
      REAL EVLOST(ILG),    RLOST (ILG)
C
      INTEGER              IROOT (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL SLOST,THTRAN,THLLIM
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
      !!The calculated fluxes of liquid and frozen water from the canopy 
      !!to the overlying air, obtained as outputs of WPREP, are applied 
      !!to the liquid and frozen intercepted water stores on the 
      !!vegetation canopy, and to the liquid and frozen moisture stores 
      !!at the surface and in the soil. Since there may not be sufficient 
      !!water in one or more of these stores to sustain the calculated 
      !!rate over the whole time step, a hierarchy of operations is 
      !!followed as described below. The change of internal energy HTC in 
      !!the canopy, snow and soil layers as a result of these processes 
      !!is calculated as the difference in HTC between the beginning and 
      !!end of the subroutine:
      !!\f[
      !!\Delta I_c = X_i \Delta (C_c T_c )/ \Delta t
      !!\f]\f[
      !!\Delta I_s = X_i \Delta (C_s T_s z_s )/ \Delta t
      !!\f]
      !!where the C terms represent volumetric heat capacities and the T 
      !!terms temperatures of the canopy and snow pack, \f$\Delta t\f$ is the 
      !!length of the time step, \f$z_s\f$ the snow depth, and \f$X_i\f$ the fractional 
      !!coverage of the subarea under consideration relative to the 
      !!modelled area. For the soil layers, since only the liquid water 
      !!content is affected by these calculations, the change in internal 
      !!energy of each layer is calculated from the change in liquid 
      !!water content \f$\theta_l\f$ as:
      !!\f[ \Delta I_g = X_i C_w \Delta z_{g,w} \Delta (T_g \theta_l )/ \Delta t \f]
      !!


C     * INITIALIZE ARRAYS.
C     * (THE WORK ARRAY "IROOT" INDICATES POINTS WHERE TRANSPIRATION
C     * CAN OCCUR.)
C
      DO 50 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              RLOST (I)=0.0
              EVLOST(I)=0.0 
              IROOT (I)=0
              HTCC  (I)=HTCC(I)-FI(I)*TCAN(I)*CHCAP(I)/DELT
              HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
          ENDIF
50    CONTINUE
C
      DO 100 J=1,IG
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              HTC (I,J)=HTC(I,J)-FI(I)*(TBAR(I,J)+TFREZ)*THLIQ(I,J)*
     1            HCPW*DELZW(I,J)/DELT
              IF(FROOT(I,J).GT.1.0E-5) IROOT(I)=1
          ENDIF
  100 CONTINUE
      !>
      !!Sublimation is addressed first. The predicted mass of sublimated 
      !!water SLOST is calculated and compared to the frozen water in the 
      !!canopy interception store, SNOCAN. If SLOST \f$\leq\f$ SNOCAN, all of the 
      !!sublimated water is subtracted from SNOCAN. If not, the excess 
      !!sublimation is calculated as SLOST-SNOCAN, QFCF is corrected for 
      !!the canopy sublimation difference, and SNOCAN is set to zero. 
      !!Next, the new value of SLOST is compared to the snowpack mass, 
      !!calculated as ZSNOW*RHOSNO. If SLOST \f$\leq\f$ ZSNOW*RHOSNO, all of the 
      !!remaining sublimated water is taken from the snow pack, and QFN 
      !!is modified to reflect this loss. Otherwise, the excess 
      !!sublimation is calculated as SLOST - ZSNOW*RHOSNO, QFN is 
      !!adjusted accordingly, and ZSNOW is set to zero. There now remain 
      !!no further frozen moisture stores from which sublimated water can 
      !!be taken (frozen water in the soil is assumed to be immobile), so 
      !!the remaining energy that had been assigned to sublimation is 
      !!assigned to canopy evaporation instead, and QFCL is duly 
      !!recalculated. This means, however, that a small imbalance will 
      !!arise in the water budget owing to the difference between the 
      !!latent heats of sublimation and evaporation. This imbalance is 
      !!assigned to the housekeeping variable WLOST.
C
C     * SUBLIMATION CASE.  IF SNOW ON CANOPY IS INSUFFICIENT TO SUPPLY
C     * DEMAND, RESIDUAL IS TAKEN FIRST FROM SNOW UNDERLYING CANOPY AND
C     * THEN FROM LIQUID WATER ON CANOPY.
C
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0. .AND. SUBL(I).GT.0.)                      THEN 
              SLOST=SUBL(I)*DELT*RHOW                                                    
              IF(SLOST.LE.SNOCAN(I))                          THEN  
                  SNOCAN(I)=SNOCAN(I)-SLOST                                                 
                  SUBL(I)=0.0                                                            
              ELSE                                                                    
                  SLOST=SLOST-SNOCAN(I)                                                  
                  QFCF(I)=QFCF(I)-FI(I)*SLOST/DELT
                  SNOCAN(I)=0.0                                                          
                  IF(SLOST.LE.ZSNOW(I)*RHOSNO(I))           THEN                                      
                      ZSNOW(I)=ZSNOW(I)-SLOST/RHOSNO(I)                                        
                      SUBL(I)=0.0                                                        
                      QFN(I)=QFN(I)+FI(I)*SLOST/DELT
                  ELSE                                                                
                      SLOST=SLOST-ZSNOW(I)*RHOSNO(I)                                        
                      QFN(I)=QFN(I)+FI(I)*ZSNOW(I)*RHOSNO(I)/DELT
                      ZSNOW(I)=0.0                                                       
                      WLOST(I)=WLOST(I)-SLOST*CLHMLT/CLHVAP                                     
                      EVAP(I)=EVAP(I)+SLOST*(CLHMLT+CLHVAP)/
     1                        (CLHVAP*DELT*RHOW)              
                      QFCL(I)=QFCL(I)+FI(I)*SLOST*(CLHMLT+CLHVAP)/
     1                        (CLHVAP*DELT)
                  ENDIF                                                               
              ENDIF                                                                   
          ENDIF
  200 CONTINUE
      !>
      !!Now canopy evaporation is addressed. It is assumed that all 
      !!intercepted liquid water evaporates before transpiration begins, 
      !!since there is a canopy stomatal resistance associated with 
      !!transpiration and there is none associated with evaporation. The 
      !!predicted mass of evaporated water RLOST is calculated and 
      !!compared to the liquid water in the canopy interception store, 
      !!RAICAN. If RLOST \f$\leq\f$ RAICAN, all of the evaporated water is 
      !!subtracted from RAICAN. If not, the excess evaporation is 
      !!calculated as RLOST - RAICAN, and QFCL is corrected for the 
      !!canopy evaporation difference. This excess evaporation is now 
      !!treated as transpiration. An initial check is done by referring 
      !!to the diagnostic flag IROOT, which was set to 1 at the beginning 
      !!of the subroutine if there was water available for transpiration 
      !!in any of the soil layers. If IROOT is zero, no transpiration can 
      !!occur and the excess evaporation is stored in the temporary 
      !!variable EVLOST.
C
C     * EVAPORATION.  IF WATER ON CANOPY IS INSUFFICIENT TO SUPPLY
C     * DEMAND, ASSIGN RESIDUAL TO TRANSPIRATION.
C
      DO 300 I=IL1,IL2
          IF(FI(I).GT.0. .AND. EVAP(I).GT.0.)                      THEN
              RLOST(I)=EVAP(I)*RHOW*DELT
              IF(RLOST(I).LE.RAICAN(I))                         THEN 
                  RAICAN(I)=RAICAN(I)-RLOST(I)
                  EVAP  (I)=0.
                  RLOST (I)=0.
              ELSE                                                                    
                  RLOST(I)=RLOST(I)-RAICAN(I)                                                  
                  QFCL(I)=QFCL(I)-FI(I)*RLOST(I)/DELT
                  IF(IROOT(I).EQ.0) EVLOST(I)=RLOST(I)
                  EVAP  (I)=0. 
                  RAICAN(I)=0.
              ENDIF
          ENDIF
  300 CONTINUE
      !!
      !!The next loop is performed if IROOT=1, i.e. if transpiration is 
      !!possible. For each soil layer, the volumetric water content that 
      !!is removed by transpiration, THTRAN, is calculated from RLOST 
      !!(converted to a volumetric content by dividing by the density of 
      !!water and the permeable thickness of the soil layer), and the 
      !!fractional contribution of the soil layer FROOT. If there is 
      !!enough liquid water in the soil layer to supply THTRAN, the 
      !!diagnostic transpiration flux QFC for the layer is updated using 
      !!THTRAN, and the liquid water content of the layer is updated as 
      !!THLIQ-THTRAN. If not, QFC is updated using the available water in 
      !!the soil layer, THLIQ is set to THLMIN, and the residual 
      !!untranspired water is added to EVLOST.
C
C     * TRANSPIRATION.
C
      DO 400 J=1,IG
      DO 400 I=IL1,IL2 
          IF(FI(I).GT.0. .AND. IROOT(I).GT.0)                     THEN
              IF(DELZW(I,J).GT.0.0) THEN
                  THTRAN=RLOST(I)*FROOT(I,J)/(RHOW*DELZW(I,J))                      
              ELSE
                  THTRAN=0.0
              ENDIF
              IF(THPOR(I,J).LT.THLMIN(I,J))           THEN
                  THLLIM=THPOR(I,J)
              ELSE
                  THLLIM=THLMIN(I,J)
              ENDIF
              IF(THTRAN.LE.(THLIQ(I,J)-THLLIM))                 THEN                        
                  QFC  (I,J)=QFC(I,J)+FI(I)*RLOST(I)*FROOT(I,J)/DELT
                  THLIQ(I,J)=THLIQ(I,J)-THTRAN                                
              ELSE                                                        
                  QFC  (I,J)=QFC(I,J)+FI(I)*(THLIQ(I,J)-THLLIM)*RHOW*
     1                       DELZW(I,J)/DELT
                  EVLOST (I)=EVLOST(I)+(THTRAN+THLLIM-THLIQ(I,J))*RHOW*            
     1                       DELZW(I,J)                                             
                  THLIQ(I,J)=THLLIM
              ENDIF                                                       
          ENDIF
  400 CONTINUE                                                        
      !!
      !!In the final cleanup, the canopy heat capacity is recalculated, 
      !!the contents of EVLOST are added to WLOST, and the remaining 
      !!internal energy calculations are completed.
C
C     * CLEANUP.
C
      DO 500 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              CHCAP(I)=RAICAN(I)*SPHW+SNOCAN(I)*SPHICE+CMASS(I)*SPHVEG
              WLOST(I)=WLOST(I)+EVLOST(I)  
              HTCC  (I)=HTCC(I)+FI(I)*TCAN(I)*CHCAP(I)/DELT
              HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
          ENDIF
  500 CONTINUE
C
      DO 550 J=1,IG
      DO 550 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              HTC (I,J)=HTC(I,J)+FI(I)*(TBAR(I,J)+TFREZ)*THLIQ(I,J)*
     1            HCPW*DELZW(I,J)/DELT
          ENDIF
  550 CONTINUE
C                                                                        
      RETURN                                                                      
      END                                                                                 
