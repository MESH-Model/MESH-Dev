!>\file
C!Purpose: Evaluate growth index used in calculating vegetation 
C!parameters for forests.
C!
      SUBROUTINE CGROW(GROWTH,TBAR,TA,FC,FCS,ILG,IG,IL1,IL2,JL)


C     * MAR 09/07 - D.VERSEGHY. CHANGE SENESCENCE THRESHOLD FROM
C     *                         0.10 TO 0.90.
C     * SEP 23/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * DEC 03/03 - V.ARORA. CHANGE THRESHOLD OF LEAF-OUT FROM 0 C TO
C     *                      2 C.
C     * SEP 25/97 - M.LAZARE. CLASS - VERSION 2.7. 
C     *                       INSERT "IF" CONDITION TO PERFORM THESE 
C     *                       CALCULATIONS ONLY IF CANOPY IS PRESENT.
C     * APR 24/92 - D.VERSEGHY/M.LAZARE. REVISED AND VECTORIZED CODE
C     *                                  FOR MODEL VERSION GCM7.
C     * APR 11/89 - D.VERSEGHY. INCREMENT/DECREMENT GROWTH INDEX FOR
C     *                         VEGETATION TYPES 1 AND 2 (NEEDLELEAF
C     *                         AND BROADLEAF TREES).
C
      IMPLICIT NONE
C                                      
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IG,IL1,IL2,JL,I
C
C     * OUTPUT ARRAY.
C
      REAL GROWTH(ILG)  !<Tree growth index [ ]
C
C     * INPUT ARRAYS.
C                                           
      REAL TBAR  (ILG,IG)   !<Temperature of soil layers [K]
C
      REAL TA(ILG)  !<Air temperature [K]  
      REAL FC(ILG)  !<Fractional coverage of vegetation without snow on 
                    !!modelled area [ ]   
      REAL FCS(ILG) !<Fractional coverage of vegetation with underlying 
                    !!snow pack on modelled area [ ]
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT     !<Time step [s]
      REAL TFREZ    !<Freezing point of water [K]
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
C-----------------------------------------------------------------------
      !>
      !!The growth index that is calculated here varies from a value of 1 
      !!for periods when the trees are mature and fully leaved, to 0 for 
      !!dormant and leafless periods, with a linear transition between 
      !!the two. The transition periods are assumed to last for sixty 
      !!days; therefore during these periods the growth index is 
      !!incremented by \f$ \Delta t /5.184x10^6 \f$ where \f$\Delta t\f$ is the time step in 
      !!seconds. 
      !!
      !!The transition period from dormant to fully leafed is triggered 
      !!when both the air temperature and the temperature of the first 
      !!soil layer are above 2 C. If one of these conditions is not met 
      !!afterwards, the growth index is reset back to 0. Increments are 
      !!added continuously thereafter until the index reaches 1.
      !! 
      !!The transition from fully leafed to dormant is triggered when 
      !!either the air temperature or the temperature of the first soil 
      !!layer falls below 2 C. When this first happens at the end of the 
      !!fully-leafed period, the growth index is set instantaneously to 
      !!-1 and increments are continuously added from that point until 
      !!the index reaches 0. 
      !!
      !!The absolute value of this growth index is utilized for 
      !!performing calculations of various forest vegetation parameters 
      !!in subroutine APREP; thus its shape as used there is that of a 
      !!symmetrical trapezoidal function.
      !!
      DO 100 I=IL1,IL2
          IF((FC(I)+FCS(I)).GT.0.0)                                 THEN
              IF(GROWTH(I).LT.0.0)                                THEN
                  GROWTH(I)=MIN(0.0,(GROWTH(I)+DELT/5.184E6)) 
              ELSE                   
                  IF(TA(I).GT.(TFREZ+2.0).AND.TBAR(I,1).GT.(TFREZ+2.0))  
     1                                                          THEN 
                      GROWTH(I)=MIN(1.0,(GROWTH(I)+DELT/5.184E6))
                  ELSE 
                      IF(GROWTH(I).GT.0.90)     THEN     
                          GROWTH(I)=-GROWTH(I)  
                      ELSE              
                          GROWTH(I)=0.0  
                      ENDIF             
                  ENDIF
              ENDIF
          ENDIF
  100 CONTINUE
C                     
      RETURN                                                                      
      END        
