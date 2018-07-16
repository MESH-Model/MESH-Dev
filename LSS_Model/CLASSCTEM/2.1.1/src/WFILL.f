!>\file
C!Purpose: Evaluate infiltration of water into soil under 
C!unsaturated conditions.
C!
      SUBROUTINE WFILL(WMOVE,TMOVE,LZF,NINF,ZF,TRMDR,R,TR,
     1                 PSIF,GRKINF,THLINF,THLIQX,TBARWX,
     2                 DELZX,ZBOTX,DZF,TIMPND,WADJ,WADD,
     3                 IFILL,IFIND,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N )
C
C     * JAN 06/09 - D.VERSEGHY. CORRECT LZF AND ZF ASSIGNMENTS IN LOOP 
C     *                         100; ADDITIONAL DZF CHECK IN LOOP 400.
C     * MAR 22/06 - D.VERSEGHY. MOVE IFILL TEST OUTSIDE ALL IF BLOCKS.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 29/04 - D.VERSEGHY. PROTECT SENSITIVE CALCULATIONS 
C     *                         AGAINST ROUNDOFF ERRORS.
C     * JUN 21/02 - D.VERSEGHY. UPDATE SUBROUTINE CALL.
C     * MAY 17/99 - D.VERSEGHY. PUT LIMIT ON CONDITION BASED ON "GRKINF"
C     *                         SO THAT "LZF" IS ALWAYS INITIALIZED.
C     * NOV 30/94 - M.LAZARE.   BRACKET TERMS IN "WADJ" CALCULATION IN
C     *                         LOOP 200 TO AVOID OPTIMIZATION LEADING
C     *                         TO RE-ORDERING OF CALCULATION AND
C     *                         RARE VERY LARGE ITERATION LIMITS.
C     * AUG 16/93 - D.VERSEGHY/M.LAZARE. ADD MISSING OUTER LOOP ON "J"
C     *                                  IN 200 LOOP.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE 
C     *                                  FOR MODEL VERSION GCM7.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. UNSATURATED FLOW OF WATER INTO SOIL.            
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,IGP1,IGP2,ILG,IL1,IL2,JL,I,J,N
C
C     * OUTPUT FIELDS.
C                      
      REAL    WMOVE (ILG,IGP2) !<Water movement matrix \f$[m^3 m^{-2}]\f$   
      REAL    TMOVE (ILG,IGP2) !<Temperature matrix associated with ground water movement [C}
      INTEGER LZF   (ILG)      !<Index of soil layer in which wetting front is located
      INTEGER NINF  (ILG)      !<Number of levels involved in water movement
      REAL    ZF    (ILG)      !<Depth of the wetting front [m]           
      REAL    TRMDR (ILG)      !<Remainder of time step after unsaturated infiltration ceases [s]
C
C     * INPUT FIELDS.
C
      REAL    R     (ILG)      !<Rainfall rate at ground surface \f$[m s^{-1}]\f$           
      REAL    TR    (ILG)      !<Temperature of rainfall [C]    
      REAL    PSIF  (ILG,IGP1) !<Soil water suction across the wetting front [m]  
      REAL    GRKINF(ILG,IGP1) !<Hydraulic conductivity of soil behind the wetting front \f$[m s^{-1}]\f$  
      REAL    THLINF(ILG,IGP1) !<Volumetric liquid water content behind the wetting front \f$[m^3 m^{-3}]\f$  
      REAL    THLIQX(ILG,IGP1) !<Volumetric liquid water content of soil layer \f$[m^3 m^{-3}]\f$  
      REAL    TBARWX(ILG,IGP1) !<Temperature of water in soil layer [C]   
      REAL    DELZX (ILG,IGP1) !<Permeable depth of soil layer \f$[m] (\Delta z_{g,w})\f$   
      REAL    ZBOTX (ILG,IGP1) !<Depth of bottom of soil layer [m]       
      INTEGER IFILL (ILG)      !<Flag indicating whether unsaturated infiltration is occurring
C
C
C     * INTERNAL WORK FIELDS.
C
      REAL DZF   (ILG),        TIMPND (ILG),
     1     WADJ  (ILG),        WADD  (ILG)      
C
      INTEGER                  IFIND (ILG) 
C
C     * TEMPORARY VARIABLE.
C
      REAL THLADD
C-----------------------------------------------------------------------
      !>
      !!The infiltration rate \f$F_{inf}\f$ under conditions of a constant water 
      !!supply can be expressed, e.g. in Mein and Larson (1973), as
      !!
      !!\f$F_{inf} = \f$K_{inf}\f$ [(\Psi_f + z_f)/ z_f ]\f$
      !!
      !!where \f$K_{inf}\f$ is the hydraulic conductivity of the soil behind the 
      !!wetting front, \f$\Psi_f\f$ is the soil moisture suction across the 
      !!wetting front, and \f$z_f\f$ is the depth of the wetting front. It can 
      !!be seen that \f$F_{inf}\f$ decreases with increasing \f$z_f\f$ to an asymptotic 
      !!value of \f$K_{inf}\f$. Thus, if the rainfall rate r is less than 
      !!\f$K_{inf}\f$, the actual infiltration rate is limited by r, i.e. 
      !!\f$F_{inf} = r\f$. Otherwise, \f$F_{inf}\f$ will be equal to R until the right-hand 
      !!side of the above equation becomes less than r, after which point 
      !!the above equation applies and ponding of excess water begins on 
      !!the surface. The depth of the wetting front at this time \f$t_p\f$ can 
      !!be calculated by setting \f$F_{inf}\f$ equal to r in the above equation 
      !!and solving for \f$z_f\f$. This results in:
      !!
      !!\f$z_f = \Psi_f /[r/K_{inf} – 1]\f$
      !!
      !!The amount of water added to the soil up to the time of ponding 
      !!is \f$t_p r\f$, or \f$z_f (\theta_{inf} – \theta_l)\f$, where \f$\theta_l\f$ and \f$\theta_{inf}\f$ are 
      !!respectively the liquid water content of the soil before and 
      !!after the wetting front has passed. Setting these two equal and 
      !!solving for \f$t_p\f$ results in
      !!
      !!\f$t_p = z_f (\theta_{inf} – \theta_l)/r\f$
      !!
C     * INITIALIZATION.
C
      DO 50 I=IL1,IL2
          IFIND(I)=0 
          WADJ(I)=0.
   50 CONTINUE
C                                                                    
C     * TEST SUCCESSIVE SOIL LAYERS TO FIND DEPTH OF WETTING FRONT 
C     * AT THE TIME PONDING BEGINS, I.E. AT THE TIME THE DECREASING
C     * INFILTRATION RATE EQUALS THE RAINFALL RATE.
C
      !>
      !!In the 100 loop, a check is done for each successive soil layer 
      !!to compare the infiltration rate in the layer with the rainfall 
      !!rate. If \f$K_{inf} < R\f$, a test calculation is performed to determine 
      !!where the depth of the wetting front would theoretically occur at 
      !!the ponding time \f$t_p\f$. If the calculated value of \f$z_f\f$ is less than 
      !!the depth of the top of the soil layer, \f$z_f\f$ is set to the depth of 
      !!the top of the layer; if \f$z_f\f$ falls within the soil layer, that 
      !!value of \f$z_f\f$ is accepted. In both cases, the index LZF is set to 
      !!the index of the layer, and the flag IFIND, indicating that \f$z_f\f$, 
      !!has been successfully located, is set to 1. If the infiltration 
      !!rate in the soil layer is greater than the rainfall rate, \f$z_f\f$ is 
      !!provisionally set to the bottom of the current layer, and LZF to 
      !!the index of the next layer. IFIND remains zero. If the 
      !!infiltration rate in the layer is vanishingly small, \f$z_f\f$ is set to 
      !!the depth of the top of the current layer, LZF to the index of 
      !!the overlying layer, and IFIND to 1.
      !!
      DO 100 J=1,IGP1
      DO 100 I=IL1,IL2
          IF(IFILL(I).GT.0 .AND. IFIND(I).EQ.0)                     THEN
             IF(GRKINF(I,J).GT.1.0E-12 .AND.
     1                            GRKINF(I,J).LT.(R(I)-1.0E-8))  THEN
                  ZF(I)=PSIF(I,J)/(R(I)/GRKINF(I,J)-1.0)                                  
                  IF(ZF(I).LT.(ZBOTX(I,J)-DELZX(I,J))) THEN                          
                      ZF(I)=MAX(ZBOTX(I,J)-DELZX(I,J),0.0)
                      LZF(I)=J                                                   
                      IFIND(I)=1                                                 
                  ELSE IF(ZF(I).LT.ZBOTX(I,J))         THEN                                 
                      LZF(I)=J                                                   
                      IFIND(I)=1                                                 
                  ENDIF                                                       
              ELSE IF (GRKINF(I,J).GT.1.0E-12)                   THEN
                  ZF(I)=ZBOTX(I,J)                                                 
                  LZF(I)=MIN(J+1,IGP1)                                          
              ELSE IF (GRKINF(I,J).LE.1.0E-12)                   THEN
                  IF(J.EQ.1) THEN
                      ZF(I)=0.0
                      LZF(I)=1
                  ELSE 
                      ZF(I)=ZBOTX(I,J-1)
                      LZF(I)=J-1
                  ENDIF
                  IFIND(I)=1                                                         
              ENDIF                                                               
          ENDIF                                                                   
  100 CONTINUE
      !>
      !!If LZF is greater than 1, some adjustment to the equation for \f$t_p\f$ 
      !!above is required to account for the fact that the values of 
      !!\f$\theta_{inf}\f$ and \f$\theta_l\f$ in the layer containing the wetting front may 
      !!differ from those in the overlying layers. The equation for \f$t_p\f$ 
      !!above can be rewritten as
      !!
      !!\f$t_p = [z_f [\theta_{inf}(z_f) – \theta_l(z_f)] + w_{adj}]/r\f$
      !!
      !!where \f$w_{adj}\f$ is calculated as
      !!
      !!\f$LZF-1\f$
      !!
      !!\f$w_{adj} = \Sigma[(\theta_{inf. i} – \theta_{l,i} ) – (\theta_{inf} (z_f) – \theta_l (z_f))] \Delta z_{g,w}\f$
      !!
      !!\f$i=1\f$
      !!
      !!The adjusting volume WADJ is calculated in loop 200, and the time 
      !!to ponding TIMPND in loop 250. If TIMPND is greater than the 
      !!amount of time remaining in the current time step TRMDR, then 
      !!unsaturated infiltration is deemed to be occurring over the 
      !!entire time step. In this case, the amount of water infiltrating 
      !!over the time step is assigned to the first level of the water 
      !!movement matrix WMOVE and to the accounting variable WADD, and 
      !!the temperature of the infiltrating water is assigned to the 
      !!first level of the matrix TMOVE. 
      !!
C
C     * FIND THE VOLUME OF WATER NEEDED TO CORRECT FOR THE DIFFERENCE 
C     * (IF ANY) BETWEEN THE LIQUID MOISTURE CONTENTS OF THE LAYERS 
C     * OVERLYING THE WETTING FRONT AND THAT OF THE LAYER CONTAINING 
C     * THE WETTING FRONT.
C
      DO 200 J=1,IGP1
      DO 200 I=IL1,IL2
          IF(IFILL(I).GT.0 .AND. LZF(I).GT.1 .AND. J.LT.LZF(I))     THEN
              WADJ(I)=WADJ(I)+DELZX(I,J)*( (THLINF(I,J)-THLIQX(I,J)) -
     1                (THLINF(I,LZF(I))-THLIQX(I,LZF(I))) )
          ENDIF                                                    
  200 CONTINUE                                                                
C
C     * CALCULATE THE TIME TO PONDING, GIVEN THE DEPTH REACHED BY THE 
C     * WETTING FRONT AT THAT TIME.

      DO 250 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN
              TIMPND(I)=(ZF(I)*(THLINF(I,LZF(I))-THLIQX(I,LZF(I)))+
     1                 WADJ(I))/R(I)                                 
              TIMPND(I)=MAX(TIMPND(I),0.0)
              IF(ZF(I).GT.10.0) TIMPND(I)=1.0E+8
C
C     * IN THE CASE WHERE THE TIME TO PONDING EXCEEDS OR EQUALS THE
C     * TIME REMAINING IN THE CURRENT MODEL STEP, RECALCULATE THE 
C     * ACTUAL DEPTH ATTAINED BY THE WETTING FRONT OVER THE CURRENT
C     * MODEL STEP; ASSIGN VALUES IN THE WATER MOVEMENT MATRIX.
C      
              IF(TIMPND(I).GE.TRMDR(I))                 THEN 
                  TMOVE(I,1)=TR(I)                                                             
                  WMOVE(I,1)=R(I)*TRMDR(I)                                                        
                  WADD(I)=WMOVE(I,1)
              ENDIF
          ENDIF    
  250 CONTINUE
C
      !>
      !!In loop 300 WADD is partitioned over the soil profile by 
      !!comparing in turn the liquid water content of each soil layer 
      !!with the calculated liquid water content behind the wetting front 
      !!THLINF, and decrementing WADD layer by layer until a layer is 
      !!reached in which the remainder of WADD is insufficient to raise 
      !!the liquid water content to THLINF. If this condition is reached, 
      !!LZF is set to the index of the soil layer; the depth of the 
      !!wetting front DZF within the layer, obtained as 
      !!WADD/(THLINF-THLIQX), is added to the depth of the bottom of the 
      !!overlying layer to obtain ZF. 
      !!
      DO 300 J=1,IGP1
      DO 300 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN
              IF(TIMPND(I).GE.TRMDR(I) .AND. WADD(I).GT.0.)  THEN
                  THLADD=MAX(THLINF(I,J)-THLIQX(I,J),0.0)                           
                  IF(THLADD.GT.0.)                      THEN                                          
                      DZF(I)=WADD(I)/THLADD                                             
                  ELSE                                                            
                      DZF(I)=1.0E+8
                  ENDIF                                                           
                  IF(DZF(I).GT.(DELZX(I,J)+1.0E-5))     THEN                                        
                      WADD(I)=WADD(I)-THLADD*DELZX(I,J)                                   
                  ELSE                                                            
                      DZF(I)=MIN(DZF(I),DELZX(I,J))
                      LZF(I)=J                                                       
                      IF(J.EQ.1)                 THEN                                             
                          ZF(I)=DZF(I)                                                  
                      ELSE                                                        
                          ZF(I)=ZBOTX(I,J-1)+DZF(I)                                       
                      ENDIF                                                       
                      WADD(I)=0.                                                    
                  ENDIF                                                           
              ENDIF
          ENDIF                                                               
  300 CONTINUE
C     
      !>
      !!In loop 400, the water content in each soil layer J existing 
      !!above ZF is assigned to the J+1 level of the water movement 
      !!matrix WMOVE, and the respective water temperatures are assigned 
      !!to TMOVE.
      !!                                                           
      DO 400 J=1,IGP1
      DO 400 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN
              IF(TIMPND(I).GE.TRMDR(I) .AND. J.LE.LZF(I))      THEN
                  TMOVE(I,J+1)=TBARWX(I,J)                                                
                  IF(J.EQ.LZF(I) .AND. DZF(I).LT.DELZX(I,J)) THEN                                                   
                      WMOVE(I,J+1)=THLIQX(I,J)*DZF(I)                                        
                  ELSE                                                                
                      WMOVE(I,J+1)=THLIQX(I,J)*DELZX(I,J)                                   
                  ENDIF
              ENDIF                                 
          ENDIF                              
  400 CONTINUE 
      !>
      !!If TIMPND < TRMDR, the amount of water infiltrating between the 
      !!start of the time step and TIMPND is again assigned to the first 
      !!level of the water movement matrix WMOVE, and the temperature of 
      !!the infiltrating water is assigned to the first level of the 
      !!matrix TMOVE. The depth DZF of the wetting front within the layer 
      !!containing it is calculated by subtracting the depth of the 
      !!bottom of the overlying layer from ZF.
      !!
C
C     * IN THE CASE WHERE THE TIME TO PONDING IS LESS THAN THE TIME
C     * REMAINING IN THE CURRENT MODEL STEP, ACCEPT THE DEPTH OF THE
C     * WETTING FRONT FROM LOOP 100; ASSIGN VALUES IN THE WATER
C     * MOVEMENT MATRIX.
C
      DO 450 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN
              IF(TIMPND(I).LT.TRMDR(I))                   THEN
              TMOVE(I,1)=TR(I)                                                             
                  WMOVE(I,1)=R(I)*TIMPND(I)                                                        
                  IF(LZF(I).EQ.1)                  THEN 
                      DZF(I)=ZF(I)                                                              
                  ELSE                                                                    
                      DZF(I)=ZF(I)-ZBOTX(I,LZF(I)-1)                                                 
                  ENDIF
              ENDIF
          ENDIF
  450 CONTINUE
C
      !>
      !!In loop 500, the water content in each soil layer J existing 
      !!above ZF is assigned to the J+1 level of the water movement 
      !!matrix WMOVE, and the respective water temperatures are assigned 
      !!to TMOVE. 
      !!
      DO 500 J=1,IGP1
      DO 500 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN
              IF(TIMPND(I).LT.TRMDR(I) .AND. J.LE.LZF(I)) THEN
                  TMOVE(I,J+1)=TBARWX(I,J)                                                
                  IF(J.EQ.LZF(I))                      THEN                                                   
                      WMOVE(I,J+1)=THLIQX(I,J)*DZF(I)                                        
                  ELSE                                                                
                      WMOVE(I,J+1)=THLIQX(I,J)*DELZX(I,J)                                   
                  ENDIF                                 
              ENDIF
          ENDIF                              
  500 CONTINUE                                                                
      !>
      !!Finally, the time remaining in the current time step after the 
      !!period of unsaturated infiltration is recalculated, and the 
      !!counter NINF is set to LZF+1.
      !!
C
C     * CALCULATE TIME REMAINING IN CURRENT MODEL STEP AFTER
C     * UNSATURATED FLOW.
C
      DO 600 I=IL1,IL2
          IF(IFILL(I).GT.0)                                         THEN
              IF(TIMPND(I).GE.TRMDR(I))              THEN    
                  TRMDR(I)=0.
              ELSE
                  TRMDR(I)=TRMDR(I)-TIMPND(I)
              ENDIF
              NINF(I)=LZF(I)+1
          ENDIF               
  600 CONTINUE                                                 
C                                                                                  
      RETURN                                                                      
      END        
