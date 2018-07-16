!>\file
C!Purpose: Assess water flux elements at the ground surface under 
C!the vegetation canopy.
C!
      SUBROUTINE SUBCAN(IWATER,R,TR,S,TS,RHOSNI,EVAPG,QFN,QFG,
     1                  PCPN,PCPG,FI,ILG,IL1,IL2,JL)
C
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 21/04 - D.VERSEGHY. NEW LOWER LIMITS ON RADD AND SADD,
C     *                         CONSISTENT WITH WPREP.
C     * SEP 26/02 - D.VERSEGHY. BUGFIX IN CALCULATIONS OF QFN/QFG.
C     * JUL 24/02 - D.VERSEGHY. MODIFICAITONS TO ALLOW FOR 
C     *                         SIMULTANEOUS RAINFALL AND SNOWFALL;
C     *                         CHANGE RHOSNI FROM CONSTANT TO
C     *                         VARIABLE.
C     * JUN 20/02 - D.VERSEGHY. UPDATE SUBROUTINE CALL.
C     * NOV 09/00 - D.VERSEGHY. MOVE DIAGNOSTIC CALCULATIONS INTO
C     *                         WPREP.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         PASS IN NEW "CLASS4" COMMON BLOCK.
C     * AUG 24/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         INCORPORATE DIAGNOSTICS.
C     * APR 21/92 - D.VERSEGHY/M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. PERFORM "WPREP" CALCULATIONS UNDER 
C     *                         CANOPY: LUMP DOWNWARD WATER VAPOUR 
C     *                         FLUXES TOGETHER WITH PRECIPITATION 
C     *                         REACHING GROUND.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IWATER,ILG,IL1,IL2,JL,I
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL R     (ILG)  !<Rainfall rate incident on ground \f$[m s^{-1}]\f$
      REAL TR    (ILG)  !<Temperature of rainfall [C]  
      REAL S     (ILG)  !<Snowfall rate incident on ground \f$[m s^{-1}]\f$
      REAL TS    (ILG)  !<Temperature of snowfall [C]
      REAL RHOSNI(ILG)  !<Density of fresh snow \f$[kg m^{-3}]\f$  
      REAL EVAPG (ILG)  !<Evaporation rate from surface \f$[m s^{-1}]\f$  
      REAL QFN   (ILG)  !<Sublimation from snow pack \f$[kg m^{-2} s^{-1}]\f$  
      REAL QFG   (ILG)  !<Evaporation from ground \f$[kg m^{-2} s^{-1}]\f$
      REAL PCPN  (ILG)  !<Precipitation incident on ground \f$[kg m^{-2} s^{-1}]\f$  
      REAL PCPG  (ILG)  !<Precipitation incident on ground \f$[kg m^{-2} s^{-1}]\f$
C
C     * INPUT ARRAYS.
C
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on modelled area [ ]
C
C     * TEMPORARY VARIABLES.
C
      REAL SADD,RADD
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
      !!This subroutine starts with the precipitation rate under the 
      !!canopy (a result of throughfall and unloading)
      !!and calculates the resulting overall evaporation or deposition 
      !!rates.
      !!
      DO 100 I=IL1,IL2
          !>
          !!For IWATER = 2 (snow on the ground under the canopy), the 
          !!water vapour flux EVAPG at the ground
          !!surface is in the first instance assumed to be sublimation. 
          !!Thus the first step is to compare it to the
          !!snowfall rate. The sum of the snowfall rate and the 
          !!evaporation rate, SADD, is calculated as S – EVAPG,
          !!with EVAPG converted from a liquid water flux (the standard 
          !!output from TSOLVC) to a snow flux. If
          !!SADD is greater than zero (indicating a downward flux) the 
          !!snowfall rate is set to SADD and EVAPG is
          !!set to zero. Otherwise EVAPG is set to -SADD (converted back 
          !!to a liquid water flux), and S and TS are
          !!set to zero.
          !!
          IF(FI(I).GT.0. .AND. IWATER.EQ.2)                        THEN
              IF(S(I).GT.0. .OR. EVAPG(I).LT.0.)             THEN  
                  SADD=S(I)-EVAPG(I)*RHOW/RHOSNI(I)
                  IF(ABS(SADD).LT.1.0E-12) SADD=0.0
                  IF(SADD.GT.0.)                        THEN
                      S(I)=SADD                                                              
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-SADD*RHOSNI(I)/RHOW                                             
                      S(I)=0.0                                                               
                      TS(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  S(I)=0.0                                                                   
                  TS(I)=0.0                                                                  
              ENDIF
C
              !>
              !!After this section, any remaining evaporative flux is 
              !!compared to the rainfall rate. The sum RADD is
              !!calculated as R – EVAPG. If RADD is greater than zero, 
              !!the rainfall rate is set to RADD and EVAPG is
              !!set to zero. Otherwise EVAPG is set to –RADD, and R and 
              !!TR are set to zero.
              !!
              IF(R(I).GT.0. .OR. EVAPG(I).LT.0.)            THEN  
                  RADD=R(I)-EVAPG(I)                                                            
                  IF(ABS(RADD).LT.1.0E-12) RADD=0.0
                  IF(RADD.GT.0.)                     THEN 
                      R(I)=RADD                                                              
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-RADD                                                         
                      R(I)=0.0                                                               
                      TR(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  R(I)=0.0                                                                   
                  TR(I)=0.0   
              ENDIF                                                               
          ENDIF                                                                       
  100 CONTINUE
C 
      !>
      !!Analogous calculations are done for IWATER = 1 (bare ground under
      !!the canopy). In this case EVAPG
      !!is assumed in the first instance to be evaporation or 
      !!condensation. Thus the first step is to compare it to
      !!the rainfall rate, and the same steps are followed as in the 
      !!paragraph above. Afterwards, any remaining
      !!vapour flux is compared to the snowfall rate. If SADD is positive 
      !!(downward), EVAPG, which is now
      !!considered to be absorbed into the snowfall rate, is subtracted 
      !!from the ground vapour flux QFG and
      !!added to the snow vapour flux QFN. If SADD is negative (upward), 
      !!S, which has now been absorbed
      !!into the evaporative flux, is subtracted from the snow 
      !!precipitation flux PCPN and added to the ground
      !!precipitation flux PCPG.
      !!                                                                                 
      DO 200 I=IL1,IL2
          IF(FI(I).GT.0. .AND. IWATER.EQ.1)                        THEN
              IF(R(I).GT.0. .OR. EVAPG(I).LT.0.)            THEN  
                  RADD=R(I)-EVAPG(I)                                                            
                  IF(ABS(RADD).LT.1.0E-12) RADD=0.0
                  IF(RADD.GT.0.)                     THEN 
                      R(I)=RADD                                                              
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-RADD                                                         
                      R(I)=0.0                                                               
                      TR(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  R(I)=0.0                                                                   
                  TR(I)=0.0   
              ENDIF                                                               
C
              IF(S(I).GT.0. .OR. EVAPG(I).LT.0.)             THEN  
                  SADD=S(I)-EVAPG(I)*RHOW/RHOSNI(I)
                  IF(ABS(SADD).LT.1.0E-12) SADD=0.0
                  IF(SADD.GT.0.)                        THEN
                      S(I)=SADD                                                              
                      QFN(I)=QFN(I)+FI(I)*EVAPG(I)*RHOW
                      QFG(I)=QFG(I)-FI(I)*EVAPG(I)*RHOW
                      EVAPG(I)=0.0                                                           
                  ELSE                                                                    
                      EVAPG(I)=-SADD*RHOSNI(I)/RHOW                                             
                      PCPN(I)=PCPN(I)-FI(I)*S(I)*RHOSNI(I)
                      PCPG(I)=PCPG(I)+FI(I)*S(I)*RHOSNI(I)
                      S(I)=0.0                                                               
                      TS(I)=0.0                                                              
                  ENDIF                                                                   
              ELSE                                                                        
                  S(I)=0.0                                                                   
                  TS(I)=0.0                                                                  
              ENDIF
          ENDIF                                                                       
  200 CONTINUE
C                                                                                  
      RETURN                                                                      
      END       
