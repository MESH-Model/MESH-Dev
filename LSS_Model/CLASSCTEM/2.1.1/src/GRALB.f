!>\file
!!Purpose: Calculate visible and near-IR ground albedos.
!!
      SUBROUTINE GRALB(ALVSG,ALIRG,ALVSGC,ALIRGC,
     1                 ALGWV,ALGWN,ALGDV,ALGDN,ALGWET,ALGDRY, 
     +                 THLIQ,FSNOW,ALVSU,ALIRU,FCMXU,                   
     2                 AGVDAT,AGIDAT,FG,ISAND,  
     3                 ILG,IG,IL1,IL2,JL,IALG,IGRALB)        
C
C     * JAN 16/15 - D.VERSEGHY. CORRECT ACCOUNTING FOR URBAN ALBEDO.
C     * AUG 25/14 - M.LAZARE.   PASS IN NEW WET AND DRY SOIL BRIGHTNESS
C     *                         FIELDS FROM CLM.
C     * NOV 16/13 - M.LAZARE.   FINAL VERSION FOR GCM17:                
C     *                         - REMOVE UNNECESSARY LOWER BOUND        
C     *                           OF 1.E-5 ON "FURB".                   
C     * APR 13/06 - D.VERSEGHY. SEPARATE ALBEDOS FOR OPEN AND
C     *                         CANOPY-COVERED GROUND.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * SEP 04/03 - D.VERSEGHY. RATIONALIZE CALCULATION OF URBAN
C     *                         ALBEDO.
C     * MAR 18/02 - D.VERSEGHY. UPDATES TO ALLOW ASSIGNMENT OF USER-
C     *                         SPECIFIED VALUES TO GROUND ALBEDO.
C     *                         PASS IN ICE AND ORGANIC ALBEDOS
C     *                         VIA NEW COMMON BLOCK "CLASS8".
C     * FEB 07/02 - D.VERSEGHY. REVISIONS TO BARE SOIL ALBEDO
C     *                         CALCULATIONS; REMOVAL OF SOIL
C     *                         MOISTURE EXTRAPOLATION TO SURFACE.
C     * JUN 05/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         CALCULATE SOIL ALBEDO FROM PERCENT
C     *                         SAND CONTENT RATHER THAN FROM COLOUR 
C     *                         INDEX.
C     * SEP 27/96 - D.VERSEGHY. CLASS - VERSION 2.5.
C     *                         FIX BUG TO CALCULATE GROUND ALBEDO
C     *                         UNDER CANOPIES AS WELL AS OVER BARE
C     *                         SOIL.
C     * NOV 29/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         "CALL ABORT" CHANGED TO "CALL XIT",
C     *                         TO ENABLE RUNNING RUN ON PC'S.
C     * FEB 12/93 - D.VERSEGHY/M.LAZARE. INCREASE DRY SOIL ALBEDO TO 
C     *                                  0.45 FROM 0.35. 
C     * MAR 13/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CLASS - VERSION 2.0.
C     *                         CODE FOR MODEL VERSION GCM7U (WITH
C     *                         CANOPY). 
C     * APR 11/89 - D.VERSEGHY. CALCULATE VISIBLE AND NEAR-IR SOIL
C     *                         ALBEDOS BASED ON TEXTURE AND SURFACE
C     *                         WETNESS. (SET TO ICE ALBEDOS OVER
C     *                         CONTINENTAL ICE SHEETS.)
C
      IMPLICIT NONE
C                
C     * INTEGER CONSTANTS.
C
      INTEGER  ILG,IG,IL1,IL2,JL,IALG,IPTBAD,I,IGRALB 
C
C     * OUTPUT ARRAYS.
C
      REAL ALVSG (ILG)  !<Visible albedo of bare ground \f$[ ] (\alpha_{g,VIS})\f$   
      REAL ALIRG (ILG)  !<Near-IR albedo of bare ground \f$[ ] (\alpha_{g,NIR})\f$
      REAL ALVSGC (ILG) !<Visible albedo of ground under vegetation canopy [ ]
      REAL ALIRGC (ILG) !<Near-IR albedo of ground under vegetation canopy [ ]
C
C     * INPUT ARRAYS.
C
      REAL ALGWET(ILG)    !<All-wave albedo of wet soil for modelled area \f$[ ] (\alpha_{g,wet})\f$
      REAL ALGDRY(ILG)    !<All-wave albedo of dry soil for modelled area \f$[ ] (\alpha_{g,dry})\f$
      REAL ALGWV (ILG)    !
      REAL ALGWN (ILG)    ! 
      REAL ALGDV (ILG)    !
      REAL ALGDN (ILG)    !
      REAL THLIQ (ILG,IG) !<Volumetric liquid water content of soil layers \f$[m^3 m^{-3}]\f$
      REAL ALVSU (ILG)    !<Visible albedo of urban part of modelled area \f$[ ] (alpha_{u,VIS})\f$
      REAL ALIRU (ILG)    !<Near-IR albedo of urban part of modelled area \f$[ ] (alpha_{u,NIR})\f$
      REAL FCMXU (ILG)    !<Fractional coverage of urban part of modelled area \f$[ ] (X_u)\f$
      REAL AGVDAT(ILG)    !<Assigned value of visible albedo of ground – optional [ ]
      REAL AGIDAT(ILG)    !<Assigned value of near-IR albedo of ground – optional [ ]
      REAL FG    (ILG)    !
      REAL FSNOW (ILG)    !
C
      INTEGER ISAND (ILG,IG)!<Soil type flag based on sand content, assigned in subroutine CLASSB
C
C     * TEMPORARY VARIABLES.
C
      REAL FURB,ALBSOL
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL ALVSI    !<Visible albedo of ice (0.95)
      REAL ALIRI    !<Near-infrared albedo of ice (0.73)
      REAL ALVSO    !<Visible albedo of organic matter (0.05)
      REAL ALIRO    !<Near-infrared albedo of organic matter (0.30)
      REAL ALBRCK   !<Albedo of rock (0.27) 
C
      COMMON /CLASS8/ ALVSI,ALIRI,ALVSO,ALIRO,ALBRCK
C---------------------------------------------------------------------
      !>
      !!If the ISAND flag for the surface soil layer is greater than zero 
      !!(indicating mineral soil), the visible and near-IR open ground 
      !!albedos, \f$\alpha_{g,VIS}\f$ and \f$\alpha_{g,NIR}\f$, are calculated on the basis of the 
      !!wet and dry ground albedos \f$\alpha_{g,wet}\f$ and \f$\alpha_{g,dry}\f$ which were assigned 
      !!for the modelled area in CLASSB. Idso et al. (1975) found a 
      !!correlation between the soil liquid moisture content in the top 
      !!10 cm of soil (represented in CLASS by that of the first soil 
      !!layer, \f$\theta_{1,1}\f$) and the total surface albedo \f$\alpha_{g,T}\f$: for liquid 
      !!water contents less than 0.22 \f$m^3 m^{-3}\f$, \f$\alpha_{g,T}\f$ took on the value of 
      !!\f$\alpha_{g,dry}\f$; for liquid water contents greater than 0.26 \f$m^3 m^{-3}\f$, 
      !!\f$\alpha_{g,T}\f$ took on the value of \f$\alpha_{g,wet}\f$. For values of \f$\theta_{1,1}\f$ between 
      !!these two limits, a linear relationship is assumed:
      !!
      !!\f$[\alpha_{g,T} - \alpha_{g,dry} ] / [\theta_{l,1} - 0.22] = [\alpha_{g,wet} - \alpha_{g,dry} ]/[0.26 - 0.22]\f$
      !!
      !!Thus, in GRALB \f$\alpha_{g,T}\f$ is calculated as follows:
      !!
      !!\f$\alpha_{g,T} = \alpha_{g,dry}\f$            \f$\theta_{l,1} \leq 0.22 \f$ \n
      !!\f$\alpha_{g,T} = \theta_{l,1} [\alpha_{g,wet} - \alpha_{g,dry} ]/0.04 - 5.50 [\alpha_{g,wet} - \alpha_{g,dry} ] + \alpha_{g,dry} \f$
      !!\f$0.22 < \theta_{l,1} < 0.26 \f$ \n
      !!\f$\alpha_{g,T} = \alpha{g,wet} \f$             \f$  0.26 \leq \theta_{l,1}\f$
      !!
      !!The total albedo is partitioned into values for the visible and 
      !!near-IR albedo by making use of the observation that in the case 
      !!of mineral soils, the near-IR albedo is typically twice that of 
      !!the visible albedo (e.g. Dickinson, 1983). Since the partitioning 
      !!of incoming shortwave radiation into visible and near-IR can be 
      !!approximated as 50:50 on average, \f$\alpha_{g,VIS}\f$ and \f$\alpha_{g,NIR}\f$ can be calculated as
      !!
      !!\f$\alpha_{g,VIS} = 2.0 \alpha_{g,T} /3.0\f$ \n
      !!\f$\alpha_{g,NIR} = 2.0 \alpha_{g,VIS}\f$
      !!
      !!Finally, a correction is applied to \f$\alpha_{g,VIS}\f$ and \f$\alpha_{g,NIR}\f$ in order to 
      !!account for the possible presence of urban areas over the 
      !!modelled area. Visible and near-IR albedos are assigned for local 
      !!urban areas, \f$\alpha_{g,VIS}\f$ and \f$\alpha_{g,NIR}\f$, as part of the background data 
      !!(see the section on “Data Requirements”). A weighted average is 
      !!calculated from the fractional urban area \f$X_u\f$ as:
      !!
      !!\f$\alpha_{g,VIS} = X_u \alpha_{u,VIS} + [1.0-X_u ] \alpha_{g,VIS}\f$\n
      !!\f$\alpha_{g,NIR} = X_u \alpha_{u,NIR} + [1.0-X_u ] \alpha_{g,NIR}\f$
      !!
      !!If the soil on the modelled area is not mineral, i.e. if the 
      !!ISAND flag is less than zero, \f$\alpha_{g,VIS}\f$ and \f$\alpha_{g,NIR}\f$ are determined as 
      !!follows:
      !!
      !!If ISAND = -2, indicating organic soil, \f$\alpha_{g,VIS}\f$ and \f$\alpha_{g,NIR}\f$ are 
      !!assigned values of 0.05 and 0.30 respectively from the lookup 
      !!tables in the block data subroutine CLASSBD, corresponding to 
      !!average measured values reported in Comer et al. (2000).
      !!
      !!If ISAND = -3, indicating rock at the surface, \f$\alpha_{g,T}\f$ is given a 
      !!value of 0.27 from CLASSBD, based on typical literature values 
      !!(e.g. Sellers, 1974), and this is partitioned into values of 
      !!\f$\alpha_{g,VIS}\f$ and \f$\alpha_{g,NIR}\f$ as above.
      !!
      !!If ISAND = -4, indicating continental ice sheet or glacier, \f$\alpha_{g,VIS}\f$ 
      !!and \f$\alpha_{g,NIR}\f$ are assigned values of 0.95 and 073 from CLASSBD, 
      !!reflecting values reported for Antarctica (e.g. Sellers, 1974).
      !!
      !!The above calculations are all performed if the flag IALG is set 
      !!to zero. If IALG is set to one, indicating that assigned ground 
      !!albedos are to be used instead of calculated values, \f$\alpha_{g,VIS}\f$ and 
      !!\f$\alpha_{g,NIR}\f$ are set to the assigned values AGVDAT and AGIDAT 
      !!respectively.
      !!
      !!Lastly, the ground values of visible and near-IR albedo under the 
      !!vegetation canopy are currently set equal to the open values 
      !!(this approach is currently under review).
      !!
      IPTBAD=0
      DO 100 I=IL1,IL2
         IF(IALG.EQ.0)                                          THEN
            IF(ISAND(I,1).GE.0)                          THEN
	
                FURB=FCMXU(I)*(1.0-FSNOW(I))                        
                IF(IGRALB.EQ.0) THEN
                IF(THLIQ(I,1).GE.0.26) THEN  
                   ALBSOL=ALGWET(I)            
                ELSEIF(THLIQ(I,1).LE.0.22) THEN 
                   ALBSOL=ALGDRY(I)              
                ELSE                         
                   ALBSOL=THLIQ(I,1)*(ALGWET(I)-ALGDRY(I))/0.04+
     1                    ALGDRY(I)-5.50*(ALGWET(I)-ALGDRY(I)) 
                ENDIF     
                    
                ALVSG(I)=2.0*ALBSOL/3.0    
                ALIRG(I)=2.0*ALVSG(I)     
                ELSE
                   IF(THLIQ(I,1).GE.0.26) THEN                      
                      ALVSG(I)=ALGWV(I)                            
                      ALIRG(I)=ALGWN(I)                           
                   ELSEIF(THLIQ(I,1).LE.0.22) THEN                   
                      ALVSG(I)=ALGDV(I)                              
                      ALIRG(I)=ALGDN(I)                              
                   ELSE 
	                                                
                      ALVSG(I)=THLIQ(I,1)*(ALGWV(I)-ALGDV(I))/0.04+  
     1                       ALGDV(I)-5.50*(ALGWV(I)-ALGDV(I))       
                      ALIRG(I)=THLIQ(I,1)*(ALGWN(I)-ALGDN(I))/0.04+  
     1                       ALGDN(I)-5.50*(ALGWN(I)-ALGDN(I))       
                   ENDIF                                             
                ENDIF
C                                                                       
                IF(FG(I).GT.0.001)                          THEN

                    ALVSG(I)=((FG(I)-FURB)*ALVSG(I)+FURB*ALVSU(I))/FG(I)
                    ALIRG(I)=((FG(I)-FURB)*ALIRG(I)+FURB*ALIRU(I))/FG(I)
                ENDIF

                IF(ALVSG(I).GT.1.0.OR.ALVSG(I).LT.0.0) IPTBAD=I
                IF(ALIRG(I).GT.1.0.OR.ALIRG(I).LT.0.0) IPTBAD=I
            ELSE IF(ISAND(I,1).EQ.-4)                    THEN
                ALVSG(I)=ALVSI
                ALIRG(I)=ALIRI
            ELSE IF(ISAND(I,1).EQ.-3)                    THEN
                IF(IGRALB.EQ.0) THEN
                ALVSG(I)=2.0*ALBRCK/3.0                                                        
                ALIRG(I)=2.0*ALVSG(I)                                                             
                ELSE
                    ALVSG(I)=ALGDV(I)
                    ALIRG(I)=ALGDN(I)
                ENDIF
            ELSE IF(ISAND(I,1).EQ.-2)                    THEN
                ALVSG(I)=ALVSO
                ALIRG(I)=ALIRO
            ENDIF
         ELSEIF(IALG.EQ.1)                                     THEN
            ALVSG(I)=AGVDAT(I)
            ALIRG(I)=AGIDAT(I)
         ENDIF     
         ALVSGC(I)=ALVSG(I)
         ALIRGC(I)=ALIRG(I)
  100 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6100) IPTBAD,JL,ALVSG(IPTBAD),ALIRG(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALVSG,ALIRG = ',2F10.5)
         CALL XIT('GRALB',-1)
      ENDIF

      RETURN                                                                      
      END
