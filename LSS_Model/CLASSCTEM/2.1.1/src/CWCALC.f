!>\file
C!Purpose: Check for freezing or thawing of liquid or frozen water 
C!on the vegetation canopy, and adjust canopy temperature and 
C!intercepted water stores accordingly.
C!

      SUBROUTINE CWCALC(TCAN,RAICAN,SNOCAN,FRAINC,FSNOWC,CHCAP,
     1                  HMFC,HTCC,FI,CMASS,ILG,IL1,IL2,JL) 
C      
C                                                           
C     * MAR 25/08 - D.VERSEGHY. UPDATE FRAINC AND FSNOWC.
C     * SEP 24/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 20/02 - D.VERSEGHY. COSMETIC REARRANGEMENT OF
C     *                         SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * MAR 17/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 13/91 - D.VERSEGHY. ADJUST CANOPY TEMPERATURE AND
C     *                         INTERCEPTED LIQUID/FROZEN 
C     *                         MOISTURE STORES FOR FREEZING/
C     *                         THAWING.
C
      IMPLICIT NONE
C                                       
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I
C
C     * I/O ARRAYS.
C
      REAL TCAN  (ILG)  !<Temperature of vegetation canopy \f$[K] (T_c)\f$
      REAL RAICAN(ILG)  !<Intercepted liquid water stored on the canopy \f$[kg m^{-2}]\f$ 
      REAL SNOCAN(ILG)  !<Intercepted frozen water stored on the canopy \f$[kg m^{-2}]\f$
      REAL FRAINC(ILG)  !<Fractional coverage of canopy by liquid water [ ]  
      REAL FSNOWC(ILG)  !<Fractional coverage of canopy by frozen water [ ]  
      REAL CHCAP (ILG)  !<Heat capacity of vegetation canopy \f$[J m^{-2} K^{-1}] (C_c)\f$  
      REAL HMFC  (ILG)  !<Energy associated with freezing or thawing of water in canopy interception stores \f$[W m^{-2}]\f$  
      REAL HTCC  (ILG)  !<Internal energy change of canopy due to changes in temperature and/or mass \f$[W m^{-2}] (I_c)\f$
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on modelled area \f$[ ] (X_i)\f$
      REAL CMASS (ILG)  !<Mass of vegetation canopy \f$[kg m^{-2}]\f$

C
C     * TEMPORARY VARIABLES.
C
      REAL HFREZ,HCONV,RCONV,HCOOL,HMELT,SCONV,HWARM
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
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
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C---------------------------------------------------------------------
!>
!!The change of internal energy \f$I_c\f$ of the vegetation canopy as a 
!!result of the phase change processes treated here is calculated as 
!!the difference in \f$I_c\f$ between the beginning and end of the subroutine:
!!
!!\f$\Delta I_c = X_i \Delta [C_c T_c ] / \Delta t\f$
!!
!!where \f$C_c\f$ represents the volumetric heat capacity of the canopy, \f$T_c\f$ 
!!its temperature, \f$\Delta t\f$ the length of the time step, and \f$X_i\f$ the 
!!fractional coverage of the subarea under consideration relative to 
!!the modelled area.
!!
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                        THEN
              HTCC  (I)=HTCC(I)-FI(I)*TCAN(I)*CHCAP(I)/DELT
              !>
              !!If there is liquid water stored on the canopy and the
              !!canopy temperature is less than 0 C, the available
              !!energy sink HFREZ is calculated from CHCAP and the
              !!difference between TCAN and 0 C, and
              !!compared with HCONV, calculated as the energy sink 
              !!required to freeze all of the liquid water on the
              !!canopy. If HFREZ \f$\leq\f$ HCONV, the amount of water that 
              !!can be frozen is calculated using the latent heat
              !!of melting. The fractional coverages of frozen and 
              !!liquid water FSNOWC and FRAINC and their masses
              !!SNOCAN and RAICAN are adjusted accordingly, TCAN is 
              !!set to 0 C, and the amount of energy
              !!involved is subtracted from the internal energy HTCC
              !!and added to HMFC. Otherwise all of the
              !!intercepted liquid water is converted to frozen 
              !!water, and the energy available for cooling the canopy is
              !!calculated as HCOOL = HFREZ – HCONV. This available 
              !!energy is applied to decreasing the
              !!temperature of the canopy, using the specific heat of
              !!the canopy elements, and the amount of energy that
              !!was involved in the phase change is subtracted from 
              !!HTCC and added to HMFC.
              !!                                                 
              IF(RAICAN(I).GT.0. .AND. TCAN(I).LT.TFREZ)      THEN                                    
                  HFREZ=CHCAP(I)*(TFREZ-TCAN(I))                                                
                  HCONV=RAICAN(I)*CLHMLT  
                  IF(HFREZ.LE.HCONV)                       THEN 
                     RCONV=HFREZ/CLHMLT                                                  
                     FSNOWC(I)=FSNOWC(I)+FRAINC(I)*RCONV/RAICAN(I)
                     FRAINC(I)=FRAINC(I)-FRAINC(I)*RCONV/RAICAN(I)
                     SNOCAN(I)=SNOCAN(I)+RCONV                                                 
                     RAICAN(I)=RAICAN(I)-RCONV                                                 
                     TCAN  (I)=TFREZ                                                          
                     HMFC  (I)=HMFC(I)-FI(I)*CLHMLT*RCONV/DELT
                     HTCC  (I)=HTCC(I)-FI(I)*CLHMLT*RCONV/DELT
                  ELSE                                                                    
                     HCOOL=HFREZ-HCONV                                                   
                     SNOCAN(I)=SNOCAN(I)+RAICAN(I)                                                
                     FSNOWC(I)=FSNOWC(I)+FRAINC(I)
                     FRAINC(I)=0.0
                     TCAN  (I)=-HCOOL/(SPHVEG*CMASS(I)+SPHICE*
     1                         SNOCAN(I))+TFREZ  
                     HMFC  (I)=HMFC(I)-FI(I)*CLHMLT*RAICAN(I)/DELT
                     HTCC  (I)=HTCC(I)-FI(I)*CLHMLT*RAICAN(I)/DELT
                     RAICAN(I)=0.0                                                          
                  ENDIF                                                                   
              ENDIF
              !>
              !!If there is frozen water stored on the canopy and the 
              !!canopy temperature is greater than 0 C, the available
              !!energy for melting, HMELT, is calculated from CHCAP and 
              !!the difference between TCAN and 0 C, and
              !!compared with HCONV, calculated as the energy required to
              !!melt all of the frozen water on the canopy.
              !!If HMELT \f$\leq\f$ HCONV, the amount of frozen water that can be 
              !!melted is calculated using the latent heat
              !!of melting. The fractional coverages of frozen and liquid
              !!water FSNOWC and FRAINC and their masses
              !!SNOCAN and RAICAN are adjusted accordingly, TCAN is set 
              !!to 0 C, and the amount of energy
              !!involved is subtracted from HTCC and added to HMFC. 
              !!Otherwise, all of the intercepted frozen water is
              !!converted to liquid water, and the energy available for 
              !!warming the canopy is calculated as HWARM =
              !!HMELT – HCONV. This available energy is applied to 
              !!increasing the temperature of the canopy, using
              !!the specific heats of the canopy elements, and the amount
              !!of energy that was involved in the phase
              !!change is subtracted from HTCC and added to HMFC.
              !!                                                        
              IF(SNOCAN(I).GT.0. .AND. TCAN(I).GT.TFREZ)        THEN 
                  HMELT=CHCAP(I)*(TCAN(I)-TFREZ)                                                
                  HCONV=SNOCAN(I)*CLHMLT                                                     
                  IF(HMELT.LE.HCONV)                       THEN 
                     SCONV=HMELT/CLHMLT                                                  
                     FRAINC(I)=FRAINC(I)+FSNOWC(I)*SCONV/SNOCAN(I)
                     FSNOWC(I)=FSNOWC(I)-FSNOWC(I)*SCONV/SNOCAN(I)
                     SNOCAN(I)=SNOCAN(I)-SCONV                                                 
                     RAICAN(I)=RAICAN(I)+SCONV                                                 
                     TCAN(I)=TFREZ                                                          
                     HMFC  (I)=HMFC(I)+FI(I)*CLHMLT*SCONV/DELT
                     HTCC  (I)=HTCC(I)+FI(I)*CLHMLT*SCONV/DELT
                  ELSE                                                                    
                     HWARM=HMELT-HCONV                                                   
                     RAICAN(I)=RAICAN(I)+SNOCAN(I)                                                
                     FRAINC(I)=FRAINC(I)+FSNOWC(I)
                     FSNOWC(I)=0.0
                     TCAN(I)=HWARM/(SPHVEG*CMASS(I)+SPHW*RAICAN(I))+
     1                       TFREZ                         
                     HMFC  (I)=HMFC(I)+FI(I)*CLHMLT*SNOCAN(I)/DELT
                     HTCC  (I)=HTCC(I)+FI(I)*CLHMLT*SNOCAN(I)/DELT
                     SNOCAN(I)=0.0                                                          
                  ENDIF                                                                   
              ENDIF 
              !>
              !!In the final cleanup, the canopy heat capacity is 
              !!recomputed and the remaining internal energy calculations
              !!are completed.
              !!
              CHCAP(I)=SPHVEG*CMASS(I)+SPHW*RAICAN(I)+SPHICE*SNOCAN(I)
              HTCC (I)=HTCC(I)+FI(I)*TCAN(I)*CHCAP(I)/DELT
          ENDIF                                
  100 CONTINUE
      RETURN                                                                      
      END 
