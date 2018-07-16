!>\file
C!Purpose: Address infiltration of rain and meltwater into snow 
C!pack, and snow ripening.
C!
      SUBROUTINE SNINFL(R,TR,ZSNOW,TSNOW,RHOSNO,HCPSNO,WSNOW,
     1                  HTCS,HMFN,PCPG,ROFN,FI,ILG,IL1,IL2,JL)                      
C
C     * DEC 23/09 - D.VERSEGHY. RESET WSNOW TO ZERO WHEN SNOW
C     *                         PACK DISAPPEARS.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * DEC 16/94 - D.VERSEGHY. CLASS - VERSION 2.3.
C     *                         NEW DIAGNOSTIC FIELD "ROFN" ADDED.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. RAIN INFILTRATION INTO SNOWPACK.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL R     (ILG)  !<Rainfall rate incident on snow pack \f$[m s^{-1}]\f$
      REAL TR    (ILG)  !<Temperature of rainfall [C]  
      REAL ZSNOW (ILG)  !<Depth of snow pack \f$[m] (z_g)\f$
      REAL TSNOW (ILG)  !<Temperature of the snow pack \f$[C] (T_s)\f$
      REAL RHOSNO(ILG)  !<Density of snow pack \f$[kg m^{-3}] (\rho_s )\f$  
      REAL HCPSNO(ILG)  !<Heat capacity of snow pack \f$[J m^{-3} K^{-1}] (C_s)\f$
      REAL WSNOW (ILG)  !<Liquid water content of snow pack \f$[kg m^{-2}] (w_s)\f$ 
      REAL HTCS  (ILG)  !<Internal energy change of snow pack due to conduction and/or change in mass \f$[W m^{-2}] (I_s)\f$  
      REAL HMFN  (ILG)  !<Energy associated with freezing or thawing of water in the snow pack \f$[W m^{-2}]\f$
      REAL PCPG  (ILG)  !<Precipitation incident on ground \f$[kg m^{-2} s^{-1}]\f$  
      REAL ROFN  (ILG)  !<Runoff reaching the ground surface from the bottom of the snow pack \f$[kg m^{-2} s^{-1}]\f$

C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on modelled area \f$[ ] (X_i)\f$

C
C     * TEMPORARY VARIABLES.
C
      REAL RAIN,HRCOOL,HRFREZ,HSNWRM,HSNMLT,ZMELT,ZFREZ,WSNCAP,WAVAIL
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
      REAL HCPW     !<Volumetric heat capacity of water \f$(4.187 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPICE   !<Volumetric heat capacity of ice \f$(1.9257*10^6) [J m^{-3} K^{-1}]\f$
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
C
      WSNCAP=0.04
C      WSNCAP=0.0
C-----------------------------------------------------------------------
      !>
      !!The rainfall rate, i.e. the liquid water precipitation rate 
      !!incident on the snow pack from the atmosphere,
      !!from canopy drip and/or from melting of the top of the snow pack, 
      !!may cause warming and/or melting
      !!of the snow pack as a whole. The overall change of internal 
      !!energy of the snow pack as a result of the
      !!rainfall added to it, \f$I_s\f$ or HTCS, is calculated as the difference 
      !!in Is between the beginning and end of the
      !!subroutine:
      !!
      !!\f$\Delta I_s = X_i \Delta [C_s z_s T_s]/Delta t\f$
      !!
      !!where \f$C_s\f$ represents the volumetric heat capacity of the snow 
      !!pack, \f$T_s\f$ its temperature, \f$\Delta\f$ the length of the
      !!time step, and \f$X_i\f$ the fractional coverage of the subarea under 
      !!consideration relative to the modelled area.  
      !!
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. R(I).GT.0. .AND. ZSNOW(I).GT.0.)
     1                                                              THEN
              HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
              RAIN=R(I)*DELT
              !>
              !!Four diagnostic variables are evaluated at the outset. 
              !!HRCOOL, the energy sink required to cool the
              !!whole rainfall amount to 0 C, is calculated from the 
              !!rainfall rate and temperature, and the heat capacity of
              !!water. HRFREZ, the energy sink required to freeze all the 
              !!rainfall, is obtained from the latent heat of
              !!melting, the density of water and the rainfall rate. 
              !!HSNWRM, the energy required to warm the whole
              !!snow pack to 0 C, is calculated from the temperature, 
              !!heat capacity and depth of the snow pack.
              !!HSNMLT, the energy required to melt all the snow, is 
              !!obtained from the latent heat of melting and the
              !!heat capacity and depth of the snow pack.
              !!                                                                 
              HRCOOL=TR(I)*HCPW*RAIN                                                         
              HRFREZ=CLHMLT*RHOW*RAIN                                                     
              HSNWRM=(0.0-TSNOW(I))*HCPSNO(I)*ZSNOW(I)                                             
              HSNMLT=CLHMLT*RHOSNO(I)*ZSNOW(I)
              !>
              !!If HRCOOL \f$\geq\f$ (HSNWRM + HSNMLT), the energy contributed by 
              !!the temperature of the rainfall is
              !!sufficient to warm to 0 C and melt the whole snow pack. 
              !!HRCOOL is recalculated as the difference
              !!between HRCOOL and (HSNWRM + HSNMLT), and the snow depth 
              !!is converted to a water depth
              !!ZMELT. The energy used to melt the snow is added to the 
              !!diagnostic variables HMFN, representing the
              !!energy associated with water phase changes in the snow 
              !!pack, and HTCS. The new temperature of the
              !!rainfall is calculated by applying HRCOOL over the new 
              !!rainfall rate reaching the soil, which now
              !!includes the original rainfall rate, the melted snow pack 
              !!and the liquid water that was contained in the
              !!snow pack. The depth, temperature, density, heat capacity 
              !!and liquid water content of the snow pack are
              !!set to zero.
              !!                                                  
              IF(HRCOOL.GE.(HSNWRM+HSNMLT))                 THEN                                          
                  HRCOOL=HRCOOL-(HSNWRM+HSNMLT)                                           
                  ZMELT=ZSNOW(I)*RHOSNO(I)/RHOW                                                 
                  HMFN(I)=HMFN(I)+FI(I)*CLHMLT*ZMELT*RHOW/DELT
                  HTCS(I)=HTCS(I)+FI(I)*CLHMLT*ZMELT*RHOW/DELT
                  TR(I)=HRCOOL/(HCPW*(ZMELT+RAIN+WSNOW(I)/RHOW))                                           
                  R(I)=R(I)+(ZMELT+WSNOW(I)/RHOW)/DELT
                  ZSNOW(I)=0.0                                                               
                  TSNOW(I)=0.0                                                               
                  RHOSNO(I)=0.0                                                              
                  HCPSNO(I)=0.0                                                              
                  WSNOW(I)=0.0
              !>
              !!If HRCOOL \f$\geq\f$ HSNWRM but HRCOOL < (HSNWRM + HSNMLT), the 
              !!energy contributed by the
              !!temperature of the rainfall is sufficient to warm the 
              !!whole snowpack to 0 C but not to melt all of it.
              !!HSNMLT is therefore recalculated as HRCOOL - HSNWRM, and 
              !!used to determine a melted depth of
              !!the snowpack ZMELT, which is subtracted from the snow 
              !!depth ZSNOW. The energy used to melt this
              !!depth of snow is added to HMFN and HTCS. The total water 
              !!now available for retention in the
              !!snowpack, WAVAIL, is obtained as the sum of the mass of 
              !!melted water and the mass of water originally
              !!retained in the snow pack, WSNOW. This amount is compared 
              !!to the water retention capacity of the
              !!snow pack, calculated from the maximum retention 
              !!percentage by weight, WSNCAP (currently set to
              !!4%). If WAVAIL is greater than the water retention 
              !!capacity, WSNOW is set to the capacity value and
              !!the excess is reassigned to ZMELT. Otherwise WSNOW is set 
              !!to WAVAIL and ZMELT is set to zero.
              !!The temperature of the snow and the temperature TR of the 
              !!rainfall reaching the ground surface are each
              !!set to 0 C, HCPSNO is recalculated, and ZMELT is added to 
              !!the rainfall rate R.
              !!
              ELSE IF(HRCOOL.GE.HSNWRM .AND. HRCOOL.LT.(HSNWRM+HSNMLT))
     1                                                      THEN
                  HSNMLT=HRCOOL-HSNWRM                                                    
                  ZMELT=HSNMLT/(CLHMLT*RHOSNO(I))                                            
                  HMFN(I)=HMFN(I)+FI(I)*CLHMLT*ZMELT*RHOSNO(I)/DELT
                  HTCS(I)=HTCS(I)+FI(I)*CLHMLT*ZMELT*RHOSNO(I)/DELT
                  ZSNOW(I)=ZSNOW(I)-ZMELT                                                       
                  WAVAIL=ZMELT*RHOSNO(I)+WSNOW(I)
                  IF(WAVAIL.GT.(WSNCAP*ZSNOW(I)*RHOSNO(I))) THEN
                      WSNOW(I)=WSNCAP*ZSNOW(I)*RHOSNO(I)
                      ZMELT=(WAVAIL-WSNOW(I))/RHOW
                  ELSE
                      WSNOW(I)=WAVAIL
                      ZMELT=0.0
                  ENDIF
                  TSNOW(I)=0.0                                                               
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  TR(I)=0.0                                                                  
                  R(I)=R(I)+ZMELT/DELT   
              !>
              !!If HSNWRM \f$\geq\f$ (HRCOOL + HRFREZ), the energy sink of the 
              !!snow pack is sufficient to cool to 0 C
              !!and freeze all of the rainfall. HSNWRM is recalculated 
              !!as the difference between HSNWRM and
              !!(HRCOOL + HRFREZ). The energy used in the freezing, 
              !!HRFREZ, is added to HMFN and HTCS.
              !!The rainfall is applied to increasing the density of the 
              !!snow pack RHOSNO; if the new density is greater
              !!than the density of ice, RHOSNO is reset to the ice 
              !!density and ZSNOW is recalculated. HCPSNO is
              !!also recalculated, and the new snow temperature is 
              !!obtained from HSNWRM, HCPSNO and ZSNOW.
              !!R and TR are set to zero.
              !!                                                       
              ELSE IF(HSNWRM.GE.(HRCOOL+HRFREZ))            THEN                                      
                  HSNWRM=(HRCOOL+HRFREZ)-HSNWRM                                           
                  HMFN(I)=HMFN(I)-FI(I)*HRFREZ/DELT
                  HTCS(I)=HTCS(I)-FI(I)*HRFREZ/DELT
                  RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOW*RAIN)/ZSNOW(I)                                   
                  IF(RHOSNO(I).GT.RHOICE)      THEN                                               
                      ZSNOW(I)=RHOSNO(I)*ZSNOW(I)/RHOICE                                           
                      RHOSNO(I)=RHOICE                                                       
                  ENDIF                                                                   
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  TSNOW(I)=HSNWRM/(HCPSNO(I)*ZSNOW(I))                                             
                  TR(I)=0.0                                                                  
                  R(I)=0.0   
              !>
              !!If HSNWRM > HRCOOL and HSNWRM < (HRCOOL + HRFREZ), the 
              !!!energy sink of the snow pack
              !!is sufficient to cool the rainfall to 0 C, but not to 
              !!freeze all of it. HRFREZ is therefore recalculated as
              !!HSNWRM – HRCOOL, and used to determine a depth of rain to 
              !!be frozen, ZFREZ. The energy used
              !!in the freezing is added to HMFN and HTCS. The frozen 
              !!rainfall is applied to increasing the density of
              !!the snow pack as above; if the calculated density exceeds 
              !!that of ice, RHOSNO and ZSNOW are
              !!recalculated. The water available for retention in the 
              !!snow pack, WAVAIL, is obtained as the sum of the
              !!unfrozen rainfall and WSNOW, and compared to the water 
              !!retention capacity of the snow pack. If
              !!WAVAIL is greater than the water retention capacity, 
              !!WSNOW is set to the capacity value and WAVAIL
              !!is recalculated. Otherwise WSNOW is set to WAVAIL and 
              !!WAVAIL is set to zero. The heat capacity of
              !!the snow is recalculated, R is calculated from WAVAIL, 
              !!and TR and TSNOW are set to zero.
              !!                                                                
              ELSE IF(HSNWRM.GE.HRCOOL .AND. HSNWRM.LT.(HRCOOL+HRFREZ)) 
     1                                                      THEN
                  HRFREZ=HSNWRM-HRCOOL                                                    
                  ZFREZ=HRFREZ/(CLHMLT*RHOW)                                              
                  HMFN(I)=HMFN(I)-FI(I)*CLHMLT*ZFREZ*RHOW/DELT
                  HTCS(I)=HTCS(I)-FI(I)*CLHMLT*ZFREZ*RHOW/DELT
                  RHOSNO(I)=(RHOSNO(I)*ZSNOW(I)+RHOW*ZFREZ)/ZSNOW(I)                                  
                  IF(RHOSNO(I).GT.RHOICE)      THEN                                               
                      ZSNOW(I)=RHOSNO(I)*ZSNOW(I)/RHOICE                                           
                      RHOSNO(I)=RHOICE                                                       
                  ENDIF                                                                   
                  WAVAIL=(RAIN-ZFREZ)*RHOW+WSNOW(I)
                  IF(WAVAIL.GT.(WSNCAP*ZSNOW(I)*RHOSNO(I))) THEN
                      WSNOW(I)=WSNCAP*ZSNOW(I)*RHOSNO(I)
                      WAVAIL=WAVAIL-WSNOW(I)
                  ELSE
                      WSNOW(I)=WAVAIL
                      WAVAIL=0.0
                  ENDIF
                  HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                (RHOW*ZSNOW(I))
                  R(I)=WAVAIL/(RHOW*DELT)
                  TR(I)=0.0                                                                  
                  TSNOW(I)=0.0                                                               
              ENDIF 
              !>
              !!Finally, the calculation of the change in internal energy 
              !!is completed, and the rainfall rate leaving the
              !!bottom of the snow pack and reaching the soil is added to 
              !!the diagnostic variables PCPG and ROFN.
              !!                                                                      
              HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                ZSNOW(I)/DELT
              PCPG(I)=PCPG(I)+FI(I)*R(I)*RHOW
              ROFN(I)=ROFN(I)+FI(I)*R(I)*RHOW
          ENDIF
  100 CONTINUE
C                                                                          
      RETURN                                                                      
      END        
