!>\file
C!Purpose: Address melting of the snow pack.
C!
      SUBROUTINE TMELT(ZSNOW,TSNOW,QMELT,R,TR,GZERO,RALB,
     1                 HMFN,HTCS,HTC,FI,HCPSNO,RHOSNO,WSNOW,
     2                 ISAND,IG,ILG,IL1,IL2,JL)
C                                                                                 
C     * JAN 06/09 - D.VERSEGHY/M.LAZARE. SPLIT 100 LOOP INTO TWO.
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * SEP 24/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * JAN 02/95 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * JUL 30/93 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.2.
C     *                                  NEW DIAGNOSTIC FIELDS.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. MELTING OF SNOWPACK.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL HTC (ILG,IG) !<Internal energy change of soil layer due to conduction and/or change in mass \f$[W m^{-2}]\f$
      REAL ZSNOW (ILG)  !<Depth of snow pack [m]   
      REAL TSNOW (ILG)  !<Temperature of the snow pack [C] 
      REAL QMELT (ILG)  !<Energy available for melting of snow \f$[W m^{-2}]\f$ 
      REAL R     (ILG)  !<Rainfall rate \f$[m s^{-1}]\f$
      REAL TR    (ILG)  !<Temperature of rainfall [C] 
      REAL GZERO (ILG)  !<Heat flow into soil surface \f$[W m^{-2}]\f$ 
      REAL RALB  (ILG)  !<Rainfall rate saved for snow albedo calculations \f$[m s^{-1}]\f$ 
      REAL HMFN  (ILG)  !<Energy associated with freezing or thawing of water in the snow pack \f$[W m^{-2}]\f$
      REAL HTCS  (ILG)  !<Internal energy change of snow pack due to conduction and/or change in mass \f$[W m^{-2}]\f$
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on modelled area [ ]  
      REAL HCPSNO(ILG)  !<Heat capacity of snow pack \f$[J m^{-3} K^{-1}]\f$
      REAL RHOSNO(ILG)  !<Density of snow pack \f$[kg m^{-3}]\f$
      REAL WSNOW (ILG)  !<Liquid water content of snow pack \f$[kg m^{-2}]\f$
      INTEGER ISAND (ILG,IG) !<Sand content flag
C                                                                                 
C     * TEMPORARY VARIABLES.
C
      REAL HADD,HCONV,ZMELT,RMELT,RMELTS,TRMELT
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
C-----------------------------------------------------------------------
      !>
      !!Melting of the snow pack occurs if a source of available energy 
      !!QMELT is produced as a result of the solution of the surface 
      !!energy balance, or if the snow pack temperature is projected to 
      !!go above 0 C in the current time step (the available energy thus 
      !!produced is added to QMELT in subroutine TSPOST). The change in 
      !!internal energy in the snow pack is calculated at the beginning 
      !!and end of the subroutine, and stored in diagnostic variable HTCS 
      !!(see notes on subroutine SNOADD).
      !!
      !!The calculations in the 100 loop are performed if QFREZ and the 
      !!snow depth ZSNOW are both greater than zero. The available energy 
      !!HADD to be applied to the snow pack is calculated from QMELT. The 
      !!amount of energy required to raise the snow pack temperature to 
      !!0 C and melt it completely is calculated as HCONV. If HADD \f$\leq\f$ 
      !!HCONV, the depth of snow ZMELT that is warmed to 0 C and melted 
      !!is calculated from HADD. (It is assumed that melting of an upper 
      !!layer of snow can occur even if the lower part of the snow pack 
      !!is still below 0 C.) The amount of water generated by melting 
      !!the snow, RMELTS, is calculated from ZMELT, and the temperature 
      !!of the meltwater TRMELT is set to 0 C. ZMELT is subtracted from 
      !!ZSNOW, the heat capacity of the snow is recalculated, and HTCS is 
      !!corrected for the amount of heat used to warm the removed portion 
      !!of the snow pack.
      !!
      !!If HADD > HCONV, the amount of available energy is sufficient to 
      !!warm and melt the whole snow pack, with some energy left over. 
      !!The amount of water generated by melting the snow, RMELTS, is 
      !!calculated from ZSNOW, and the total amount of water reaching the 
      !!soil, RMELT, is obtained by adding the liquid water content of 
      !!the snow pack, WSNOW, to RMELTS. HADD is recalculated as HADD – 
      !!HCONV, and used to calculate TRMELT. The snow depth, heat 
      !!capacity, temperature and water content are set to zero, and HTCS 
      !!is corrected for the amount of heat that was used to warm the 
      !!snow pack to 0 C.
      !!
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              IF(QMELT(I).GT.0. .AND. ZSNOW(I).GT.0.)           THEN
                  HTCS(I)=HTCS(I)-FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                    ZSNOW(I)/DELT
                  HADD =QMELT(I)*DELT                                                             
                  HCONV=(0.0-TSNOW(I))*HCPSNO(I)*ZSNOW(I) + 
     1                          CLHMLT*RHOSNO(I)*ZSNOW(I)                          
                  IF(HADD.LE.HCONV)                     THEN
                      ZMELT=HADD/((0.0-TSNOW(I))*HCPSNO(I)+
     1                      CLHMLT*RHOSNO(I))                           
                      RMELTS=ZMELT*RHOSNO(I)/(RHOW*DELT)                                          
                      RMELT=RMELTS
                      TRMELT=0.0                                                              
                      ZSNOW(I)=ZSNOW(I)-ZMELT                                                       
                      HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1                    (RHOW*ZSNOW(I))
                      HTCS (I)=HTCS(I)-FI(I)*(QMELT(I)-CLHMLT*RMELT*
     1                         RHOW)
                  ELSE                                                                        
                      RMELTS=ZSNOW(I)*RHOSNO(I)/RHOW                                                 
                      RMELT=RMELTS+WSNOW(I)/RHOW
                      HADD=HADD-HCONV                                                         
                      TRMELT=HADD/(HCPW*RMELT)                                                
                      RMELT=RMELT/DELT                                                        
                      RMELTS=RMELTS/DELT
                      ZSNOW (I)=0.0                                                               
                      HCPSNO(I)=0.0
                      TSNOW (I)=0.0                                                               
                      WSNOW (I)=0.0
                      HTCS (I)=HTCS(I)-FI(I)*(QMELT(I)-CLHMLT*RMELTS*
     1                         RHOW-HADD/DELT)
                  ENDIF             
                  !>
                  !!After the IF block, the diagnostic variable HMFN 
                  !!describing melting or freezing of water in the snow 
                  !!pack is updated using RMELTS, the temperature of the 
                  !!rainfall rate reaching the soil is updated using 
                  !!TRMELT, and RMELT is added to the rainfall rate R. 
                  !!QMELT is set to zero, and a flag variable RALB, used 
                  !!later in subroutine SNOALBW, is set to the rainfall 
                  !!rate reaching the ground.
                  !!                                                          
                  HMFN (I)=HMFN(I)+FI(I)*CLHMLT*RMELTS*RHOW
                  TR   (I)=(R(I)*TR(I)+RMELT*TRMELT)/(R(I)+RMELT)
                  R    (I)=R(I)+RMELT
                  QMELT(I)=0.0
                  HTCS(I)=HTCS(I)+FI(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1                    ZSNOW(I)/DELT
              ENDIF
              RALB(I)=R(I)
          ENDIF
  100 CONTINUE                                                                   
C
      !>
      !!In the 200 loop, a check is performed to see whether QMELT is 
      !!still greater than zero and the modelled area is not an ice sheet 
      !!(ISAND > -4). In this case QMELT is added to the ground heat flux 
      !!GZERO, and the internal energy diagnostics HTCS and HTC for the 
      !!snow and soil respectively are corrected. The flag variable RALB 
      !!is evaluated as above.
      !!
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0.)                                          THEN
              IF(QMELT(I).GT.0. AND. ISAND(I,1).GT.-4)      THEN
                  GZERO(I)=GZERO(I)+QMELT(I)
                  HTCS (I)=HTCS(I)-FI(I)*QMELT(I)
                  HTC(I,1)=HTC(I,1)+FI(I)*QMELT(I)
              ENDIF
              RALB(I)=R(I)
          ENDIF
  200 CONTINUE                                                                   
C                                                                                  
      RETURN                                                                      
      END        
