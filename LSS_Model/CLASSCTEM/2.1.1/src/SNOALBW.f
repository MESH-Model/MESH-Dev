!>\file
C!Purpose: Calculate decrease in snow albedo and increase in density 
C!due to aging.
C!
      SUBROUTINE SNOALBW(ALBSNO,RHOSNO,ZSNOW,HCPSNO,TSNOW,
     1                   FI,S,RMELT,WSNOW,RHOMAX,ISAND,
     2                   ILG,IG,IL1,IL2,JL)       
C
C     * APR 17/14 - D.VERSEGHY. MAKE SNOW ALBEDO REFRESHMENT VALUE
C     *                         CONSISTENT WITH SNOADD.
C     * MAR 07/07 - D.VERSEGHY. STREAMLINE SOME CALCULATIONS.
C     * MAR 24/06 - D.VERSEGHY. ALLOW FOR PRESENCE OF WATER IN SNOW.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * SEP 10/04 - R.HARVEY/D.VERSEGHY. INCREASE SNOW ALBEDO 
C     *                         REFRESHMENT AND WETTING THRESHOLDS.
C     * AUG 04/04 - Y.DELAGE/D.VERSEGHY. PROTECT SENSITIVE
C     *                         CALCULATIONS AGAIST ROUNDOFF 
C     *                         ERRORS.
C     * APR 21/04 - F.SEGLENIEKS/D.VERSEGHY. BUG FIX IN SNOW
C     *                         TEMPERATURE COMPARISONS.
C     * JUL 26/02 - D.VERSEGHY. SHORTENED CLASS4 COMMON BLOCK.
C     * OCT 20/00 - R.BROWN/D.VERSEGHY. MODIFIED SNOW DENSITY
C     *                                 CALCULATIONS, ACCOUNTING
C     *                                 FOR SETTLING IN WARM AND
C     *                                 COLD SNOW.
C     * JUN 05/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         SPECIFY LOCATION OF ICE SHEETS
C     *                         BY SOIL TEXTURE ARRAY RATHER
C     *                         THAN BY SOIL COLOUR INDEX.
C     * JAN 02/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         COMPLETION OF ENERGY BALANCE
C     *                         DIAGNOSTICS.
C     * MAR 13/92 - M.LAZARE.   CLASS - VERSION 2.1.
C     *                         CODE FOR MODEL VERSION GCM7 -
C     *                         DIVIDE PREVIOUS SUBROUTINE 
C     *                         "SNOALB" INTO "SNOALBA" AND
C     *                         "SNOALBW" AND VECTORIZE.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. CALCULATE DECREASE IN SNOW ALBEDO
C     *                         AND INCREASE IN DENSITY DUE TO
C     *                         AGING. (ASSIGN DIFFERENT LOWER
C     *                         SNOW ALBEDO LIMITS FOR DRY AND
C     *                         MELTING SNOW.)
C                                                                                 
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IG,IL1,IL2,JL,I,IPTBAD
C                                                                                 
C     * OUTPUT ARRAYS.                                                            
C                                                                                 
      REAL ALBSNO(ILG)  !<Albedo of snow \f$[ ] (\alpha_s)\f$  
      REAL RHOSNO(ILG)  !<Density of snow pack \f$[kg m^{-3}] (\rho_s )\f$
      REAL ZSNOW (ILG)  !<Depth of snow pack \f$[m] (z_s)\f$
      REAL HCPSNO(ILG)  !<Heat capacity of snow pack \f$[J m^{-3} K^{-1}]\f$
C                                                                                 
C     * INPUT ARRAYS.                                                             
C                                                                                 
      REAL TSNOW (ILG)  !<Temperature of the snow pack \f$[C]\f$ 
      REAL FI    (ILG)  !<Fractional coverage of subarea in question on modelled area [ ]
      REAL S     (ILG)  !<Snowfall rate \f$[m s^{-1}] \f$
      REAL RMELT (ILG)  !<Melt rate at top of snow pack \f$[m s^{-1}]\f$
      REAL WSNOW (ILG)  !<Liquid water content of snow pack \f$[kg m^{-2}]\f$
C 
      INTEGER ISAND (ILG,IG) !<Sand content flag
C
C     * WORK ARRAY.                                                             
C                                                                                 
      REAL RHOMAX(ILG)  !<Maximum density of snow pack \f$[kg m^{-3}] (\rho_{s,max})\f$
C
C     * TEMPORARY VARIABLES.
C
      REAL TIMFAC,RHOOLD
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
C----------------------------------------------------------------------
      !>
      !!The albedo and density of snow are modelled using empirical 
      !!exponential decay functions. In the absence
      !!of any fresh snowfall the snow albedo \f$\alpha_s\f$ is assumed to decrease 
      !!exponentially with time from a fresh
      !!snow value of 0.84 to a background old snow value \f$\alpha_{s,old}\f$ using an 
      !!expression based on data given in
      !!Aguado (1985), Robinson and Kukla (1984) and Dirmhirn and Eaton 
      !!(1975):
      !!
      !!\f$\alpha_s (t+1) = [\alpha_s (t) - \alpha_{s,old}] exp [-0.01 \Delta t / 3600] + \alpha_{s,old}\f$
      !!
      !!where \f$\Delta t\f$ is the length of the time step. If the melt rate RMELT 
      !!at the top of the snow pack is non-
      !!negligible or if the temperature of the snow is close to 0 C, 
      !!\f$\alpha_{s,old}\f$ is assigned a value of 0.50; otherwise \f$\alpha_{s,old}\f$
      !!is assigned a value of 0.70.
      !!
      !!The maximum snow density \f$\rho_{s,max}\f$ is estimated as a function of 
      !!snow depth \f$z_s\f$, after Tabler et al. (1990):
      !!
      !!\f$\rho_{s,max} = A_s - [204.70/ z_s] [1.0 - exp(-z_s /0.673)]\f$
      !!
      !!The empirical constant \f$A_s\f$ is assigned a value of 450.0 for cold 
      !!snow packs, and 700.0 for snow packs near
      !!the melting point, following Brown et al. (2006).
      !!
      !!The density of snow \f$\rho_s\f$ increases exponentially with time from its 
      !!fresh snow value to the background old
      !!snow density calculated above, according to an expression 
      !!analogous to that for albedo, derived from the
      !!field measurements of Longley (1960) and Gold (1958):
      !!
      !!\f$\rho_s (t+1) = [\rho_s (t) - \rho_{s,max} ] exp [-0.01 \Delta t/3600] + \rho{s,max}\f$
      !!
      !!The snow depth and heat capacity are adjusted (see notes on 
      !!subroutine SNOVAP), and a check is
      !!performed with a call to abort if for unphysical albedo values 
      !!are encountered.
      !!               
      IPTBAD=0                                                                    
      DO 100 I=IL1,IL2  
          IF(ZSNOW(I).GT.0. .AND. 
     1            FI  (I).GT.0. .AND. S(I)*DELT.LT.1.0E-4)       THEN   
              IF(ALBSNO(I).GT.0.5001 .AND. (RMELT(I).GT.1.0E-7 .OR.
     1                TSNOW(I).GE.-0.01)) THEN
                  ALBSNO(I)=(ALBSNO(I)-0.50)*EXP(-0.01*DELT/3600.0)+
     1                0.50                    
              ELSE IF(ALBSNO(I).GT.0.7001 .AND. RMELT(I).LE.1.0E-7)
     1                                                            THEN
                  ALBSNO(I)=(ALBSNO(I)-0.70)*EXP(-0.01*DELT/3600.0)+
     1                0.70
              ENDIF                                                           
          ENDIF
C                                                       
          IF(FI(I).GT.0. .AND. ZSNOW(I).GT.0.0001)                THEN
              IF(TSNOW(I).LT.-0.01)                   THEN
                  RHOMAX(I)=450.0-(204.7/ZSNOW(I))*
     1                (1.0-EXP(-ZSNOW(I)/0.673))
              ELSE
                  RHOMAX(I)=700.0-(204.7/ZSNOW(I))*
     1                (1.0-EXP(-ZSNOW(I)/0.673))
              ENDIF
          ENDIF
C
          IF(FI(I).GT.0. .AND. ZSNOW(I).GT.0.0001 .AND. 
     1       RHOSNO(I).LT.(RHOMAX(I)-0.01))                       THEN
              RHOOLD=RHOSNO(I)                                                       
              RHOSNO(I)=(RHOSNO(I)-RHOMAX(I))*EXP(-0.01*DELT/3600.0)+
     1            RHOMAX(I)
              ZSNOW(I)=ZSNOW(I)*RHOOLD/RHOSNO(I) 
              HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1            (RHOW*ZSNOW(I))
          ENDIF
          IF((ALBSNO(I).LT.0.49 .OR. ALBSNO(I).GT.1.0) .AND. 
     1       ZSNOW (I).GT.0. .AND. FI(I).GT.0.)               IPTBAD=I
  100 CONTINUE                                                                    
C
      IF(IPTBAD.NE.0) THEN                                                        
         WRITE(6,6100) IPTBAD,JL,ALBSNO(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALBSNO = ',F10.5)            
         CALL XIT('SNOALBW',-1)                                                               
      ENDIF                                                                       
C                                                                                 
      RETURN                                                                      
      END        
