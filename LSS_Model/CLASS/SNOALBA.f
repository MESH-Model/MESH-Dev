      SUBROUTINE SNOALBA(ALVSSN,ALIRSN,ALVSSC,ALIRSC,ALBSNO,
     1                   TRSNOW,ZSNOW,FSNOW,ASVDAT,ASIDAT,
     2                   ILG,IG,IL1,IL2,JL,IALS,COSZS,
     3                   RHOSNI,SPCP,TSNOW,Sage)
C
C     * FEB 05/07 - D.VERSEGHY. STREAMLINE CALCULATIONS OF
C     *                         ALVSSN AND ALIRSN.
C     * APR 13/06 - D.VERSEGHY. SEPARATE ALBEDOS FOR OPEN AND 
C     *                         CANOPY-COVERED SNOW.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * MAR 18/02 - D.VERSEGHY. UPDATES TO ALLOW ASSIGNMENT OF
C     *                         USER-SPECIFIED VALUES TO SNOW
C     *                         ALBEDO.
C     * JUN 05/97 - D.VERSEGHY. CLASS - VERSION 2.7.
C     *                         SPECIFY LOCATION OF ICE SHEETS
C     *                         BY SOIL TEXTURE ARRAY RATHER
C     *                         THAN BY SOIL COLOUR INDEX.
C     * NOV 29/94 - M.LAZARE.   CLASS - VERSION 2.3.
C     *                         CALL ABORT CHANGED TO CALL XIT TO 
C     *                         ENABLE RUNNING ON PC'S.
C     * MAR 13/92 - M.LAZARE.   CODE FOR MODEL VERSION GCM7 -
C     *                         DIVIDE PREVIOUS SUBROUTINE 
C     *                         "SNOALB" INTO "SNOALBA" AND
C     *                         "SNOALBW" AND VECTORIZE.
C     * AUG 12/91 - D.VERSEGHY. CODE FOR MODEL VERSION GCM7U -
C     *                         CLASS VERSION 2.0 (WITH CANOPY).
C     * APR 11/89 - D.VERSEGHY. DISAGGREGATE SNOW ALBEDO INTO
C     *                         VISIBLE AND NEAR-IR PORTIONS;
C     *                         CALCULATE TRANSMISSIVITY TO
C     *                         SHORTWAVE RADIATION.
C
      IMPLICIT NONE
C    
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IG,IL1,IL2,JL,IALS,IPTBAD,I
C
C     * OUTPUT ARRAYS.
C
      REAL   ALVSSN(ILG),  ALIRSN(ILG),  ALVSSC(ILG),  ALIRSC(ILG),
     1       TRSNOW(ILG), Sage(ILG)
C
C     * INPUT ARRAYS.
C
      REAL   ALBSNO(ILG),  ZSNOW (ILG),  FSNOW (ILG),
     1       ASVDAT(ILG),  ASIDAT(ILG),  COSZS (ILG),
     2       RHOSNI(ILG),  SPCP  (ILG),  TSNOW (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL r1,fage,fcosz,xdiffus
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,
     1     SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C                                                                                 
      COMMON /CLASS1/ DELT,TFREZ
C
C------------------------------------------------------------------
      IPTBAD=0
      DO 100 I=IL1,IL2                                           
         IF(FSNOW(I).GT.0.0 .AND. IALS.EQ.0)              THEN
          r1=min(exp(5000*(1/TFREZ - 1/TSNOW(I))), 1.)
          Sage(I)=max((Sage(I) + 1e-6*(r1 + r1**10 + 0.3)*DELT)*
     1               (1 - 0.1*RHOSNI(I)*SPCP(I)*DELT), 0.)
          if (ZSNOW(I) == 0) Sage(I)=0.
          fage = 1 - 1 / (1 + Sage(I))
          fcosz = max((1-2*COSZS(I))/(1+2*COSZS(I)), 0.)
          if(SPCP(I)>0.) then
            xdiffus=1.0
          else
            xdiffus=max(0.0,min(1.0-0.9*COSZS(I),1.0))
          endif
          ALVSSN(I)= xdiffus*(1-0.2*fage)*0.95 + 
     1             (1-xdiffus)*((1-0.2*fage)*0.95 +
     2             0.4*fcosz*(1-(1-0.2*fage)*0.95))
          ALIRSN(I)= xdiffus*(1-0.5*fage)*0.65 + 
     1             (1-xdiffus)*((1-0.5*fage)*0.65 +
     2             0.4*fcosz*(1-(1-0.5*fage)*0.65))
          ELSE IF(FSNOW(I).GT.0.0 .AND. IALS.EQ.1)         THEN  
             ALVSSN(I)=ASVDAT(I)
             ALIRSN(I)=ASIDAT(I)
         ENDIF                                                                   
         ALVSSC(I)=ALVSSN(I)
         ALIRSC(I)=ALIRSN(I)
         TRSNOW(I)=EXP(-25.0*ZSNOW(I))                                                 
  100 CONTINUE
C
      IF(IPTBAD.NE.0) THEN
         WRITE(6,6100) IPTBAD,JL,ALVSSN(IPTBAD),ALIRSN(IPTBAD)
 6100    FORMAT('0AT (I,J)= (',I3,',',I3,'), ALVSSN,ALIRSN = ',2F10.5)
         CALL XIT('SNOALBA',-1)
      ENDIF
C
      RETURN
      END
