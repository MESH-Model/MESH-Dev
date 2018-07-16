!>\file
C!Purpose: Recalculate liquid water content of soil layers after 
C!infiltration, and evaluate baseflow.
C!
      SUBROUTINE WEND(THLIQX,THICEX,TBARWX,ZPOND,TPOND,
     1                BASFLW,TBASFL,RUNOFF,TRUNOF,FI,
     2                WMOVE,TMOVE,LZF,NINF,TRMDR,THLINF,DELZX,
     3                ZMAT,ZRMDR,FDTBND,WADD,TADD,FDT,TFDT,
     4                THLMAX,THTEST,THLDUM,THIDUM,TDUMW,
     5                TUSED,RDUMMY,ZERO,WEXCES,XDRAIN,
     6                THPOR,THLRET,THLMIN,BI,PSISAT,GRKSAT,
     7                THFC,DELZW,ISAND,IGRN,IGRD,IGDR,IZERO,
     8                IVEG,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
C
C     * OCT 18/11 - M.LAZARE.   PASS IN "IGDR" AS AN INPUT FIELD 
C     *                         (ORIGINATING IN CLASSB) RATHER
C     *                         THAN REPEATING THE CALCULATION HERE
C     *                         AS AN INTERNAL WORK FIELD.
C     * DEC 15/10 - D.VERSEGHY. ALLOW FOR BASEFLOW WHEN BEDROCK
C     *                         LIES WITHIN SOIL PROFILE.
C     * JAN 06/09 - D.VERSEGHY. ADD ZPOND AND TPOND TO SUBROUTINE
C     *                         CALL; ASSIGN RESIDUAL OF WMOVE TO
C     *                         PONDED WATER; REVISE LOOP 550;
C     *                         DELETE CALCULATION OF FDTBND.
C     * MAY 17/06 - D.VERSEGHY. PROTECT AGAINST DIVISIONS BY ZERO.
C     * OCT 21/05 - D.VERSEGHY. FIX MINOR BUGS IN CLEANUP AND
C     *                         RUNOFF TEMPERATURE CALCULATION.
C     * MAR 23/05 - D.VERSEGHY. ADD VARIABLES TO GRDRAN CALL;
C     *                         ADD CALCULATION OF RUNOFF
C     *                         TEMPERATURE.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * APR 24/03 - D.VERSEGHY. ADD CHECK FOR OVERFLOW IN SOIL
C     *                         LAYER CONTAINING WETTING FRONT.
C     * OCT 15/02 - D.VERSEGHY. BUGFIX IN CALCULATION OF FDTBND
C     *                         (PRESENT ONLY IN PROTOTYPE
C     *                         VERSIONS OF CLASS VERSION 3.0).
C     * JUN 21/02 - D.VERSEGHY. UPDATE SUBROUTINE CALL; SHORTENED
C     *                         CLASS4 COMMON BLOCK.
C     * DEC 12/01 - D.VERSEGHY. ADD SEPARATE CALCULATION OF BASEFLOW
C     *                         AT BOTTOM OF SOIL COLUMN.
C     * OCT 20/97 - D.VERSEGHY. APPLY ACCURACY LIMIT ON FLOWS IN AND
C     *                         OUT OF LAYER CONTAINING WETTING FRONT,
C     *                         IN ORDER TO ENSURE MOISTURE CONSERVATION.
C     * JUN 20/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         MODIFICATIONS TO ALLOW FOR VARIABLE
C     *                         SOIL PERMEABLE DEPTH.
C     * AUG 18/95 - D.VERSEGHY. CLASS - VERSION 2.4.
C     *                         REVISIONS TO ALLOW FOR INHOMOGENEITY
C     *                         BETWEEN SOIL LAYERS.
C     * APR 24/92 - D.VERSEGHY,M.LAZARE. CLASS - VERSION 2.1.
C     *                                  REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. RECALCULATE LIQUID MOISTURE CONTENT
C     *                         OF SOIL LAYERS AFTER INFILTRATION
C     *                         AND EVALUATE FLOW ("RUNOFF") FROM
C     *                         BOTTOM OF SOIL COLUMN.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IVEG,IG,IGP1,IGP2,ILG,IL1,IL2,JL,I,J,K,N
C
C     * OUTPUT FIELDS.
C
      REAL THLIQX(ILG,IGP1) !<Volumetric liquid water content of soil 
                            !<layer \f$[m^3 m^{-3}]\f$
      REAL THICEX(ILG,IGP1) !<Volumetric frozen water content of soil 
                            !<layer \f$[m^3 m^{-3}]\f$
      REAL TBARWX(ILG,IGP1) !<Temperature of water in soil layer [C]
      REAL ZPOND (ILG)      !<Depth of ponded water [m]
      REAL TPOND (ILG)      !<Temperature of ponded water [C]
      REAL BASFLW(ILG)      !<Base flow from bottom of soil column \f$[kg m^{-2}]\f$
      REAL TBASFL(ILG)      !<Temperature of base flow from bottom of soil column [K]
      REAL RUNOFF(ILG)      !<Total runoff from soil column [m]
      REAL TRUNOF(ILG)      !<Temperature of total runoff from soil column [K]
C
C     * INPUT FIELDS.
C
      REAL WMOVE (ILG,IGP2) !<Water movement matrix \f$[m^3 m^{-2}]\f$
      REAL TMOVE (ILG,IGP2) !<Temperature matrix associated with ground water movement [C]
      REAL THLINF(ILG,IGP1) !<Volumetric liquid water content behind the 
                            !<wetting front \f$[m^3 m^{-3}]\f$
      REAL FI    (ILG)      !<Fractional coverage of subarea in question on modelled area [ ]
      REAL TRMDR (ILG)      !<Time remaining in current time step [s]
      REAL DELZX (ILG,IGP1) !<Permeable depth of soil layer [m]
C
      INTEGER                LZF   (ILG),      NINF  (ILG), 
     1                       IGRN  (ILG)    
C
C     * INTERNAL WORK ARRAYS.
C
      REAL ZMAT  (ILG,IGP2,IGP1),  ZRMDR (ILG,IGP1)
C
      REAL FDTBND(ILG),    WADD  (ILG),    TADD  (ILG) 
C
C     * INTERNAL ARRAYS USED IN CALLING GRDRAN.
C
      REAL FDT   (ILG,IGP1), TFDT  (ILG,IGP1)
C
      REAL THLMAX(ILG,IG), THTEST(ILG,IG), THLDUM(ILG,IG),
     1     THIDUM(ILG,IG), TDUMW (ILG,IG)          
C
      REAL TUSED (ILG),    RDUMMY(ILG),    ZERO  (ILG),
     1     WEXCES(ILG)
C
      INTEGER              IGRD  (ILG),    IZERO (ILG),
     1                     IGDR  (ILG) 
C
C     * TEMPORARY VARIABLES.
C
      REAL WREM,TREM,THDRAN,THINFL,WDRA,TDRA
C
C     * SOIL INFORMATION ARRAYS.
C
      REAL THPOR (ILG,IG), THLRET(ILG,IG), THLMIN(ILG,IG), 
     1     BI    (ILG,IG), PSISAT(ILG,IG), GRKSAT(ILG,IG), 
     2     THFC  (ILG,IG), DELZW (ILG,IG), XDRAIN(ILG)
C  
      INTEGER              ISAND (ILG,IG)
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
      REAL HCPW     !<Volumetric heat capacity of water \f$(4.187 * 10^6) 
                    !<[J m^{-3} K^{-1}]\f$
      REAL HCPICE   !<Volumetric heat capacity of ice \f$(1.9257 * 10^6) 
                    !<[J m^{-3} K^{-1}]\f$
      REAL HCPSOL   !<Volumetric heat capacity of mineral matter 
                    !<\f$(2.25 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPOM    !<Volumetric heat capacity of organic matter 
                    !<\f$(2.50 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPSND   !<Volumetric heat capacity of sand particles 
                    !<\f$(2.13 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL HCPCLY   !<Volumetric heat capacity of fine mineral particles 
                    !<\f$(2.38 * 10^6) [J m^{-3} K^{-1}]\f$
      REAL SPHW     !<Specific heat of water \f$(4.186 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHICE   !<Specific heat of ice \f$(2.10 * 10^3) [J kg^{-1} K^{-1}]\f$
      REAL SPHVEG   !<Specific heat of vegetation matter \f$(2.70 * 10^3) 
                    !<[J kg^{-1} K^{-1}]\f$
      REAL SPHAIR   !<Specific heat of air \f$[J kg^{-1} K^{-1}]\f$
      REAL RHOW     !<Density of water \f$(1.0 * 10^3) [kg m^{-3}]\f$
      REAL RHOICE   !<Density of ice \f$(0.917 * 10^3) [kg m^{-3}]\f$
      REAL TCGLAC   !<Thermal conductivity of ice sheets \f$(2.24) 
                    !<[W m^{-1} K^{-1}]\f$
      REAL CLHMLT   !<Latent heat of freezing of water \f$(0.334 * 10^6) [J kg^{-1}]\f$
      REAL CLHVAP   !<Latent heat of vaporization of water \f$(2.501 * 10^6) [J kg^{-1}]\f$
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C
      !>
      !!At levels in the soil profile lower than the bottom of the 
      !!wetting front, redistribution of soil liquid water proceeds in 
      !!response to normal gravity and suction forces. Subroutine GRDRAN 
      !!is called to calculate these redistributions. The time period 
      !!TUSED that is passed to GRDRAN is set in the 100 loop to the time 
      !!period over which infiltration was occurring during the current 
      !!time step, except if the wetting front has passed the bottom of 
      !!the lowest soil layer, in which case TUSED is set to zero (since 
      !!the flows calculated by GRDRAN are not required). GRDRAN is 
      !!called using dummy variables THLDUM, THIDUM and TDUMW, which are 
      !!set in loop 125 to the liquid water content, the frozen water 
      !!content and the water temperature of the soil layers 
      !!respectively.
      !!
C
C     * DETERMINE AMOUNT OF TIME OUT OF CURRENT MODEL STEP DURING WHICH 
C     * INFILTRATION WAS OCCURRING.
C     * SET WORK ARRAY "TUSED" TO ZERO FOR POINTS WHERE WETTING FRONT
C     * IS BELOW BOTTOM OF LOWEST SOIL LAYER TO SUPPRESS CALCULATIONS
C     * DONE IN "GRDRAN".
C
      DO 100 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. LZF(I).LE.IG)                       THEN
              TUSED(I)=DELT-TRMDR(I)
          ELSE
              TUSED(I)=0.
          ENDIF
  100 CONTINUE
C
C     * INITIALIZATION.
C
      DO 125 J=1,IG
      DO 125 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              THLDUM(I,J)=THLIQX(I,J)                                                 
              THIDUM(I,J)=THICEX(I,J)                                                 
              TDUMW (I,J)=TBARWX(I,J)                  
          ENDIF
  125 CONTINUE 
C
C     * CALL "GRDRAN" WITH COPIES OF CURRENT LIQUID AND FROZEN SOIL
C     * MOISTURE CONTENTS AND LAYER TEMPERATURES TO DETERMINE MOISTURE
C     * FLOW BETWEEN LAYERS BELOW THE WETTING FRONT.
C
      CALL GRDRAN(IVEG,THLDUM,THIDUM,TDUMW,FDT,TFDT,RDUMMY,RDUMMY,
     1            RDUMMY,RDUMMY,RDUMMY,RDUMMY,FI,ZERO,ZERO,ZERO,
     2            TUSED,WEXCES,THLMAX,THTEST,THPOR,THLRET,THLMIN,
     3            BI,PSISAT,GRKSAT,THFC,DELZW,XDRAIN,ISAND,LZF,
     4            IZERO,IGRD,IGDR,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
      !>
      !!After GRDRAN has been called, the maximum value of the water 
      !!movement index NINF is set to the number of soil layers plus 1. 
      !!The values in the matrix ZRMDR, representing for each soil layer 
      !!the depth that has not been affected by infiltration, are 
      !!initialized to the soil permeable layer thicknesses DELZX. The 
      !!water flows FDT coming out of GRDRAN are set to zero at the soil 
      !!layer interfaces above the wetting front. For the layer 
      !!containing the wetting front, if the flow at the bottom of the 
      !!layer is upward, it is set to zero (to avoid possible overflows 
      !!in liquid water content).
      !!
C
C     * INITIALIZATION OF ARRAYS IN PREPARATION FOR RE-ALLOCATION OF
C     * MOISTURE STORES WITHIN SOIL LAYERS; SUPPRESS WATER FLOWS 
C     * CALCULATED IN GRDRAN ABOVE WETTING FRONT; CONSISTENCY CHECK
C     * FOR WATER FLOWS INTO LAYER CONTAINING WETTING FRONT.
C
      DO 150 I=IL1,IL2
          IF(IGRN(I).GT.0)                                       THEN
               NINF(I)=MIN(NINF(I),IGP1)                                                        
          ENDIF                                                                       
  150 CONTINUE
C
      DO 200 J=IGP1,1,-1                                                           
      DO 200 I=IL1,IL2
          IF(IGRN(I).GT.0)                                          THEN
              ZRMDR(I,J)=DELZX(I,J)
              IF(J.LE.LZF(I))                                 THEN
                  FDT(I,J)=0.0
              ENDIF
              IF(J.LT.IGP1)                                   THEN
                  IF(J.EQ.LZF(I).AND.FDT(I,J+1).LT.0.)  THEN
                      FDT(I,J+1)=0.0
                  ENDIF
              ENDIF
          ENDIF
  200 CONTINUE
C
      !>
      !!The values in the three-dimensional matrix ZMAT are initialized 
      !!to zero. This matrix contains the depth of each soil layer J 
      !!(including the dummy soil layer below the lowest layer) that is 
      !!filled by water from level K in the water movement matrix WMOVE. 
      !!In WMOVE, the first level contains the amount of water that has 
      !!infiltrated at the surface during the time period in question, 
      !!and each successive level K contains the amount of water in soil 
      !!layer K-1 that has been displaced during the infiltration, down 
      !!to the soil layer containing the wetting front. Thus, the number 
      !!of levels in WMOVE that are used in the current infiltration 
      !!calculations, NINF, is equal to LZF+1, where LZF is the index of 
      !!the soil layer containing the wetting front; or to IGP1, the 
      !!number of soil layers IG plus 1, if LZF is greater than IG, i.e. 
      !!if the wetting front has penetrated below the bottom of the 
      !!lowest soil layer into the underlying dummy layer. In the 400 
      !!loop, starting at the top of the soil profile, an attempt is made 
      !!to assign each layer of WMOVE, converted into a depth by using 
      !!the volumetric water content THLINF behind the wetting front for 
      !!the soil layer, in turn to the K,J level of ZMAT. If the 
      !!calculated value of ZMAT is greater than the available depth 
      !!ZRMDR of the layer, ZMAT is set to ZRMDR, WMOVE is decremented by 
      !!ZRMDR converted back to a water amount, and ZRMDR is set to zero. 
      !!Otherwise the calculated value of ZMAT is accepted, ZRMDR is 
      !!decremented by ZMAT, and WMOVE is set to zero. At the end of 
      !!these calculations, any remaining residual amounts in the WMOVE 
      !!matrix are assigned to ponded water, and the ponded water 
      !!temperature is updated accordingly.
      !!
      DO 300 J=1,IGP1                                                             
      DO 300 K=1,IGP1
      DO 300 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. K.LE.NINF(I))                       THEN
              ZMAT(I,K,J)=0.0                                                       
          ENDIF
  300 CONTINUE                        
C
C     * ASSIGN VALUES IN MATRIX "ZMAT": DETERMINE DEPTH OUT OF EACH
C     * SOIL LAYER J WHICH IS FILLED BY WATER FROM RESERVOIR K
C     * IN "WMOVE"; FIND THE DEPTH "ZRMDR" LEFT OVER WITHIN EACH
C     * SOIL LAYER.
C
      DO 400 K=1,IGP1
      DO 400 J=1,IGP1
      DO 400 I=IL1,IL2
          IF(IGRN(I).GT.0 .AND. K.LE.NINF(I))                       THEN
              IF(ZRMDR(I,J).GT.1.0E-5 .AND. WMOVE(I,K).GT.0.) THEN                        
                  ZMAT(I,K,J)=WMOVE(I,K)/THLINF(I,J)                                    
                  IF(ZMAT(I,K,J).GT.ZRMDR(I,J)) THEN                                  
                      ZMAT(I,K,J)=ZRMDR(I,J)                                          
                      WMOVE(I,K)=WMOVE(I,K)-ZRMDR(I,J)*THLINF(I,J)                        
                      ZRMDR(I,J)=0.0                                                
                  ELSE                                                            
                      ZRMDR(I,J)=ZRMDR(I,J)-ZMAT(I,K,J)                                 
                      WMOVE(I,K)=0.0                                                
                  ENDIF                                                           
              ENDIF                                                               
          ENDIF
  400 CONTINUE
C
      DO 450 J=1,IGP1
      DO 450 I=IL1,IL2
          IF(IGRN(I).GT.0. AND. WMOVE(I,J).GT.0.0)                  THEN
              TPOND(I)=(TPOND(I)*ZPOND(I)+TMOVE(I,J)*WMOVE(I,J))/
     1            (ZPOND(I)+WMOVE(I,J))
              ZPOND(I)=ZPOND(I)+WMOVE(I,J)
          ENDIF
 450  CONTINUE
      !>
      !!As a result of the above processes, the liquid water content and 
      !!temperature of each soil layer (excluding the bottom, dummy 
      !!layer) will be a combined result of infiltration processes (WADD, 
      !!TADD), redistribution processes (WDRA, TDRA), and water in the 
      !!layer that has remained unaffected (WREM, TREM). For each layer, 
      !!WADD is calculated by summing over the respective ZMAT values 
      !!corresponding to that layer multiplied by THLINF, and TADD by 
      !!summing over the ZMAT and THLINF values multiplied by the 
      !!respective TMOVE values. WREM is obtained as the product of the 
      !!original water content THLIQX multiplied by ZRMDR, and TREM as 
      !!the product of the water temperature TBARWX, THLIQX and ZRMDR. 
      !!For the soil layer containing the wetting front, a check is 
      !!carried out to determine whether the liquid moisture content 
      !!resulting from the infiltration and drainage processes, THINFL, 
      !!is less than the residual liquid moisture content THLMIN. If so, 
      !!the flow FDT at the bottom of the layer is recalculated as the 
      !!value required to keep THLIQX at THLMIN. WDRA is obtained from 
      !!the difference between the water fluxes FDT at the top and bottom 
      !!of the layer, supplied by GRDRAN, and TDRA is obtained from the 
      !!water fluxes FDT and their corresponding temperatures TFDT. 
      !!Finally, THLIQX is calculated as the sum of WADD, WREM and WDRA 
      !!normalized by DELZX, and TBARWX as the sum of TADD, TREM and TDRA 
      !!normalized by the product of THLIQX and DELZX.
      !!
C
C     * ADD WATER CONTENT AND TEMPERATURE CHANGES DUE TO INFILTRATION
C     * (WADD, TADD) AND DRAINAGE (WDRA, TDRA) TO WATER REMAINING IN
C     * EACH SOIL LAYER AFTER THESE PROCESSES (WREM, TREM).
C
      DO 600 J=IG,1,-1
          DO 500 I=IL1,IL2
              IF(IGRN(I).GT.0)                                      THEN
                  WADD(I)=0.
                  TADD(I)=0.
              ENDIF
  500     CONTINUE
C  
          DO 525 K=1,IGP1
          DO 525 I=IL1,IL2
              IF(IGRN(I).GT.0 .AND. K.LE.NINF(I))                   THEN
                  WADD(I)=WADD(I)+THLINF(I,J)*ZMAT(I,K,J)                                       
                  TADD(I)=TADD(I)+TMOVE(I,K)*THLINF(I,J)*ZMAT(I,K,J)
              ENDIF
  525     CONTINUE
C
          DO 550 I=IL1,IL2
              IF(IGRN(I).GT.0 .AND. DELZW(I,J).GT.1.0E-4)           THEN
                 IF(ZRMDR(I,J).GT.1.0E-5)                 THEN 
                    WREM=THLIQX(I,J)*ZRMDR(I,J)                                             
                    TREM=TBARWX(I,J)*THLIQX(I,J)*ZRMDR(I,J)                                   
                 ELSE                                                                    
                    WREM=0.0                                                            
                    TREM=0.0                                                            
                 ENDIF                                                                   
                 IF(J.EQ.LZF(I))                      THEN    
                    THINFL=(WADD(I)+WREM+FDT(I,J)-FDT(I,J+1))/DELZW(I,J)                                 
                    IF(THINFL.LT.THLMIN(I,J))   THEN                                           
                       FDT(I,J+1)=WADD(I)+WREM+FDT(I,J)-THLMIN(I,J)*
     1                            DELZW(I,J)
                    ENDIF                                                               
                 ENDIF                                                                   
                 WDRA=FDT(I,J)-FDT(I,J+1)                                                    
                 TDRA=FDT(I,J)*TFDT(I,J)-FDT(I,J+1)*TFDT(I,J+1)
              


                 THLIQX(I,J)=(WADD(I)+WREM+WDRA)/DELZW(I,J)
                 THLIQX(I,J)=MAX(THLIQX(I,J),THLMIN(I,J))
                 TBARWX(I,J)=(TADD(I)+TREM+TDRA)/(THLIQX(I,J)*
     1                        DELZW(I,J))
              ENDIF                          
  550     CONTINUE
  600 CONTINUE

      !>
      !!Lastly, the base flow BASFLW at the bottom of the soil profile 
      !!and its temperature TBASFL are calculated. If the wetting front 
      !!is located in the dummy soil layer below the soil profile, BASFLW 
      !!is obtained by summing over the ZMAT values for the IGP1 level, 
      !!multiplied by the dummy layer THLINF value and the fractional 
      !!coverage FI of the modelled subarea. TBASFL is similarly obtained 
      !!as the weighted average of the original TBASFL and the values of 
      !!TMOVE corresponding to the ZMAT values. The overall subarea 
      !!runoff RUNOFF and its temperature TRUNOF are calculated in the 
      !!same manner without the FI weightings. Otherwise, the baseflow 
      !!and total runoff are obtained from the value of FDT at the bottom 
      !!of the IGDR layer, and their temperatures from the values of TFDT 
      !!and FDT.
      !!
C
C     * CALCULATE FLOW OUT OF BOTTOM OF SOIL COLUMN DUE TO INFILTRATION
C     * AND GRAVITY DRAINAGE AND ADD TO TOTAL RUNOFF AND BASEFLOW.
C


      DO 700 K=1,IGP1
      DO 700 I=IL1,IL2


          IF(IGRN(I).GT.0)                                        THEN
              IF(LZF(I).EQ.IGP1 .AND. K.LE.NINF(I) .AND. 
     1                THLINF(I,IGP1)*ZMAT(I,K,IGP1).GT.0.0)   THEN 
                  TBASFL(I)=(TBASFL(I)*BASFLW(I)+FI(I)*(TMOVE(I,K)+
     1                TFREZ)*THLINF(I,IGP1)*ZMAT(I,K,IGP1))/(BASFLW(I)+
     2                FI(I)*THLINF(I,IGP1)*ZMAT(I,K,IGP1))                
                  BASFLW(I)=BASFLW(I)+FI(I)*THLINF(I,IGP1)*
     1                      ZMAT(I,K,IGP1)
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TMOVE(I,K)+TFREZ)*
     1                THLINF(I,IGP1)*ZMAT(I,K,IGP1))/(RUNOFF(I)+
     2                THLINF(I,IGP1)*ZMAT(I,K,IGP1))
                  RUNOFF(I)=RUNOFF(I)+THLINF(I,IGP1)*ZMAT(I,K,IGP1)
              ELSE IF(K.EQ.(IGDR(I)+1) .AND. FDT(I,K).GT.1.0E-8)  THEN
                  TBASFL(I)=(TBASFL(I)*BASFLW(I)+FI(I)*(TFDT(I,K)+
     1                TFREZ)*FDT(I,K))/(BASFLW(I)+FI(I)*FDT(I,K))
                  BASFLW(I)=BASFLW(I)+FI(I)*FDT(I,K)
                  TRUNOF(I)=(TRUNOF(I)*RUNOFF(I)+(TFDT(I,K)+TFREZ)*
     1                FDT(I,K))/(RUNOFF(I)+FDT(I,K))
                  RUNOFF(I)=RUNOFF(I)+FDT(I,K)
              ENDIF                              
          ENDIF
  700 CONTINUE                                                                
C                                                                                  
      RETURN                                                                      
      END        
