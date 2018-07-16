!>\file
C!Purpose: Address freezing of water ponded on ground surface.
C!
      SUBROUTINE TFREEZ(ZPOND,TPOND,ZSNOW,TSNOW,ALBSNO,RHOSNO,HCPSNO,
     1                  GZERO,HMFG,HTCS,HTC,WTRS,WTRG,FI,QFREZ,
     2                  WSNOW,TA,TBAR,ISAND,IG,ILG,IL1,IL2,JL)
C
C     * JAN 06/09 - D.VERSEGHY. SET QFREZ TO ZERO AFTER CALCULATION
C     *                         OF HADD.
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUN 20/02 - D.VERSEGHY. COSMETIC CHANGES TO SUBROUTINE CALL;
C     *                         SHORTENED CLASS4 COMMON BLOCK.
C     * MAY 24/02 - D.VERSEGHY. PASS IN ENTIRE SOIL TEMPERATURE
C     *                         ARRAY.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE 
C     *                         DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * AUG 16/95 - D.VERSEGHY. TWO NEW ARRAYS TO COMPLETE WATER
C     *                         BALANCE DIAGNOSTICS.
C     * DEC 22/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         REVISE CALCULATION OF HTC.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. FREEZING OF PONDED WATER.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL ZPOND (ILG)  !<Depth of ponded water \f$[m] (z_p)\f$  
      REAL TPOND (ILG)  !<Temperature of ponded water \f$[C] (T_p)\f$ 
      REAL ZSNOW (ILG)  !<Depth of snow pack \f$[m] (z_g)\f$
      REAL TSNOW (ILG)  !<Temperature of the snow pack \f$[C] (T_s)\f$
      REAL ALBSNO(ILG)  !<Albedo of snow [ ]
      REAL RHOSNO(ILG)  !<Density of snow pack \f$[kg m^{-3}] (\rho_s)\f$  
      REAL HCPSNO(ILG)  !<Heat capacity of snow pack \f$[J m^{-3} K^{-1}] (C_s)\f$
      REAL GZERO (ILG)  !<Heat flow into soil surface \f$[W m^{-2}]\f$
      REAL HTCS  (ILG)  !<Internal energy change of snow pack due to conduction and/or change in mass \f$[W m^{-2}]\f$ (Is)
      REAL WTRS  (ILG)  !<Water transferred into or out of the snow pack \f$[kg m^{-2} s^{-1}]\f$ 
      REAL WTRG  (ILG)  !<Water transferred into or out of the soil \f$[kg m^{-2} s^{-1}]\f$
C
      REAL HMFG  (ILG,IG) !<Energy associated with phase change of water in soil layers \f$[W m^{-2}]\f$
      REAL HTC   (ILG,IG) !<Internal energy change of soil layer due to conduction and/or change in mass \f$[W m^{-2}]\f$ (Ig)
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)    !<Fractional coverage of subarea in question on modelled area \f$[ ] (X_i)\f$    
      REAL QFREZ (ILG)    !<Energy sink for freezing of water at the ground surface \f$[W m^{-2}]\f$ 
      REAL WSNOW (ILG)    !<Liquid water content of snow pack \f$[kg m^{-2}] (w_s)\f$
      REAL TA    (ILG)    !<Air temperature [K]  
      REAL TBAR  (ILG,IG) !<Temperature of soil layer \f$[C] (T_g)\f$
C
      INTEGER ISAND (ILG,IG) !<Sand content flag
C
C     * TEMPORARY VARIABLES.
C
      REAL ZFREZ,HADD,HCOOL,HCONV,TTEST,TLIM,HEXCES
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
      REAL SPHW     !<Specific heat of water \f$(4.186 * 10^3) [J kg m^{-1} K^{-1}]\f$
      REAL SPHICE   !<Specific heat of ice \f$(2.10 * 10^3) [J kg m^{-1} K^{-1}]\f$
      REAL SPHVEG   !<Specific heat of vegetation matter \f$(2.70 * 10^3) [J kg m^{-1} K^{-1}]\f$
      REAL SPHAIR   !<Specific heat of air \f$[J kg m^{-1} K^{-1}]\f$
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
      !!Freezing of water ponded on the ground surface occurs if an 
      !!energy sink QFREZ is produced as a result
      !!of the solution of the surface energy balance, or if the pond 
      !!temperature at the beginning of the
      !!subroutine has been projected to be below 0 C. The change of 
      !!internal energy I in the snow and first soil
      !!layer (which for the purposes of diagnostic calculations includes 
      !!the ponded water) as a result of these
      !!processes is calculated as the difference in I between the 
      !!beginning and end of the subroutine:
      !!
      !!\f$\Delta I_s = X_i \Delta(C_s T_s z_s) / \Delta t\f$
      !!\f$\Delta I_g = X_i \Delta(C_w T_p z_p)/\Delta t\f$
      !!
      !!where the C terms represent volumetric heat capacities, the T 
      !!terms temperatures, and the z terms depths
      !!of the snow pack and the ponded water respectively, \f$\Delta t\f$ is the 
      !!length of the time step, and \f$X_i\f$ the
      !!fractional coverage of the subarea under consideration relative 
      !!to the modelled area.
      !!
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ZPOND(I).GT.0. .AND. (TPOND(I).LT.0. 
     1                     .OR. QFREZ(I).LT.0.))           THEN
             HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
             ZFREZ=0.0
             HADD=-QFREZ(I)*DELT                                                            
             QFREZ(I)=0.0
             IF(TPOND(I).LT.0.)            THEN                                                       
                 HADD=HADD-TPOND(I)*HCPW*ZPOND(I)
                 TPOND(I)=0.0              
             ENDIF        
             !>
             !!The energy sink HADD to be applied to the ponded water is 
             !!calculated from QFREZ and the pond
             !!temperature TPOND (if it is below 0 C). Two diagnostic 
             !!variables, HCOOL and HCONV, are calculated as the energy 
             !!sink required to cool the ponded water to 0 C, and that 
             !!required both to cool and to freeze
             !!the ponded water, respectively. If HADD \f$\leq\f$ HCOOL, the 
             !!available energy sink is only sufficient to
             !!decrease the temperature of the ponded water. This 
             !!decrease is applied, and the energy used is added to
             !!the internal energy HTC of the first soil layer.
             !!                                                               
             HCOOL=TPOND(I)*HCPW*ZPOND(I)                                                      
             HCONV=HCOOL+CLHMLT*RHOW*ZPOND(I)                                               
             HTC (I,1)=HTC (I,1)-FI(I)*HCPW*(TPOND(I)+TFREZ)*
     1                 ZPOND(I)/DELT
             IF(HADD.LE.HCOOL)             THEN                                                      
                TPOND(I)=TPOND(I)-HADD/(HCPW*ZPOND(I))                                           
                HTC(I,1)=HTC(I,1)+FI(I)*HADD/DELT
             !>
             !!If HADD > HCOOL but HADD \f$\leq\f$ HCONV, the available energy 
             !!sink is sufficient to decrease the
             !!ponded water temperature to 0 C and to freeze part of it. 
             !!The energy used in freezing is calculated as
             !!HADD – HCOOL, and is used to calculate the depth of frozen 
             !!water ZFREZ, which is then subtracted
             !!from the ponded water depth ZPOND. HCOOL is added to HTC, 
             !!and ZFREZ is converted from a
             !!depth of water to a depth of ice and added to the snow 
             !!pack. If there is not a pre-existing snow pack, the
             !!snow albedo is set to the background value for old snow, 
             !!0.50. The temperature, density and heat
             !!capacity of the snow pack are recalculated.
             !!
             ELSE IF(HADD.LE.HCONV)        THEN                                                  
                HADD=HADD-HCOOL                                                         
                ZFREZ=HADD/(CLHMLT*RHOW)                                                
                ZPOND(I)=ZPOND(I)-ZFREZ                                                       
                HTC(I,1)=HTC(I,1)+FI(I)*HCOOL/DELT
                ZFREZ=ZFREZ*RHOW/RHOICE                                                 
                IF(.NOT.(ZSNOW(I).GT.0.0)) ALBSNO(I)=0.50                                     
                TSNOW(I)=TSNOW(I)*HCPSNO(I)*ZSNOW(I)/(HCPSNO(I)*ZSNOW(I)
     1                   +HCPICE*ZFREZ)                    
                RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOICE*ZFREZ)/(ZSNOW(I)
     1                   +ZFREZ)                        
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                   (RHOW*ZSNOW(I))
                TPOND(I)=0.0   
             !>
             !!If HADD > HCONV, the available energy sink is sufficient 
             !!to cool and freeze the whole depth of
             !!ponded water, and also to decrease the temperature of the 
             !!ice thus formed. The ponded water depth is
             !!converted to a depth of ice ZFREZ, and HCOOL is added to 
             !!HTC. In order to avoid unphysical
             !!temperature decreases caused by the length of the time 
             !!step and the small ponding depth, a limit is set on
             !!the allowable decrease. The theoretical temperature of 
             !!the newly formed ice, TTEST, is calculated from
             !!HADD – HCONV. If there is a pre-existing snow pack, the 
             !!limiting temperature TLIM is set to the
             !!minimum of the snow pack temperature TSNOW, the first soil 
             !!layer temperature, and 0 C; otherwise it is
             !!set to the minimum of the air temperature, the first soil 
             !!layer temperature and 0 C. If TTEST < TLIM,
             !!the new ice is assigned TLIM as its temperature in the 
             !!recalculation of TSNOW, the excess heat sink
             !!HEXCES is calculated from HADD and TLIM and assigned to 
             !!the ground heat flux GZERO, and
             !!HADD – HEXCES is used to update HTC. Otherwise the new ice 
             !!is assigned TTEST as its temperature
             !!in the recalculation of TSNOW, and HADD is used to update 
             !!HTC. If there is not a pre-existing snow
             !!pack, the snow albedo is set to the background value. The 
             !!density, depth and heat capacity of the snow
             !!pack are recalculated, and the pond depth and temperature 
             !!are set to zero.
             !!                                                            
             ELSE                                                                        
                HADD=HADD-HCONV                                                         
                ZFREZ=ZPOND(I)*RHOW/RHOICE                                                 
                HTC(I,1)=HTC(I,1)+FI(I)*HCOOL/DELT
                TTEST=-HADD/(HCPICE*ZFREZ)                                              
                IF(ZSNOW(I).GT.0.0) THEN
                    TLIM=MIN(TSNOW(I),TBAR(I,1))
                ELSE
                    TLIM=MIN(TA(I)-TFREZ,TBAR(I,1))
                ENDIF
                TLIM=MIN(TLIM,0.0)
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
     1                    ZFREZ)                        
                ZSNOW(I)=ZSNOW(I)+ZFREZ                                                       
                HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                    (RHOW*ZSNOW(I))
                ZPOND(I)=0.0                                                               
                TPOND(I)=0.0                                                               
             ENDIF    
             !>
             !!At the end of the subroutine, the internal energy 
             !!calculations are completed, and ZFREZ is used to
             !!update the diagnostic variable HMFG describing the energy 
             !!associated with phase changes of water in soil
             !!layers, and also the diagnostic variables WTRS and WTRG 
             !!describing transfers of water into or out of the
             !!snow and soil respectively. Finally, the initial step in 
             !!the calculation of the internal energy change for the
             !!ponded water over the following subroutines GRINFL and 
             !!GRDRAN is performed (the calculation is
             !!completed in subroutine TMCALC).
             !!                                                                   
             HTC (I,1)=HTC (I,1)+FI(I)*HCPW*(TPOND(I)+TFREZ)*
     1                 ZPOND(I)/DELT
             HMFG(I,1)=HMFG(I,1)-FI(I)*CLHMLT*RHOICE*ZFREZ/DELT
             WTRS(I)=WTRS(I)+FI(I)*ZFREZ*RHOICE/DELT
             WTRG(I)=WTRG(I)-FI(I)*ZFREZ*RHOICE/DELT
             HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
          ENDIF
          IF(FI(I).GT.0. .AND.ISAND(I,1).GT.-4)                    THEN
             HTC (I,1)=HTC (I,1)-FI(I)*HCPW*(TPOND(I)+TFREZ)*
     1                 ZPOND(I)/DELT
          ENDIF
  100 CONTINUE
C                                                                           
      RETURN                                                                      
      END        
