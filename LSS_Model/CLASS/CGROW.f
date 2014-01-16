      SUBROUTINE CGROW(GROWTH,TBAR,TA,FC,FCS,ILG,IG,IL1,IL2,JL,q)

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
      use MODELS, only : Nmod
      IMPLICIT NONE
C                                      
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IG,IL1,IL2,JL,I,q
C
C     * OUTPUT ARRAY.
C
      REAL GROWTH(ILG,Nmod)
C
C     * INPUT ARRAYS.
C                                           
      REAL TBAR  (ILG,IG,Nmod)
C
      REAL TA(ILG),  FC(ILG,Nmod),   FCS(ILG,Nmod)
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
C-----------------------------------------------------------------------
      DO 100 I=IL1,IL2
          IF((FC(I,q)+FCS(I,q)).GT.0.0)                            THEN
              IF(GROWTH(I,q).LT.0.0)                                THEN
                  GROWTH(I,q)=MIN(0.0,(GROWTH(I,q)+DELT/5.184E6)) 
              ELSE                   
                  IF(TA(I).GT.(TFREZ+2.0).AND.TBAR(I,1,q).GT.(TFREZ+2.))  
     1                                                          THEN 
                      GROWTH(I,q)=MIN(1.0,(GROWTH(I,q)+DELT/5.184E6))
                  ELSE 
                      IF(GROWTH(I,q).GT.0.90)     THEN     
                          GROWTH(I,q)=-GROWTH(I,q)  
                      ELSE              
                          GROWTH(I,q)=0.0  
                      ENDIF             
                  ENDIF
              ENDIF
          ENDIF
  100 CONTINUE
C                     
      RETURN                                                                      
      END        
