      SUBROUTINE CLASSI(VPD,TADP,PADRY,RHOAIR,RHOSNI,
     1                  RPCP,TRPCP,SPCP,TSPCP,
     2                  TA,QA,PCPR,RRATE,SRATE,PRESSG,
     3                  IPCP,NL,IL1,IL2)
C
C     * JUL 31/25 - F.YASSIN.   ADDED IPCP=5 BASED ON HARDER & POMEROY
C     *                         (2013) CONVERTED FROM CHRM.
C     * NOV 17/11 - M.LAZARE.   REMOVE CALCULATION OF PCPR
C     *                         FOR IPCP=4 (REDUNDANT SINCE
C     *                         PCPR MUST BE PASSED IN FOR
C     *                         IF CONDITION ON LINE 100).
C     * NOV 22/06 - P.BARTLETT. CALCULATE PCPR IF IPCP=4.
C     * JUN 06/06 - V.FORTIN.   ADD OPTION FOR PASSING IN
C     *                         RAINFALL AND SNOWFALL RATES
C     *                         CALCULATED BY ATMOSPHERIC MODEL.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND;
C     *                         MOVE CLOUD FRACTION CALCULATION
C     *                         BACK INTO DRIVER.
C     * SEP 04/03 - D.VERSEGHY. NEW LOWER LIMIT ON PRECIPITATION
C     *                         RATE.
C     * AUG 09/02 - D.VERSEGHY. MOVE CALCULATION OF SOME
C     *                         ATMOSPHERIC VARIABLES HERE
C     *                         PRIOR TO GATHERING.
C     * JUL 26/02 - R.BROWN/S.FASSNACHT/D.VERSEGHY. PROVIDE 
C     *                         ALTERNATE METHODS OF ESTIMATING 
C     *                         RAINFALL/SNOWFALL PARTITIONING.
C     * JUN 27/02 - D.VERSEGHY. ESTIMATE FRACTIONAL CLOUD COVER
C     *                         AND RAINFALL/SNOWFALL RATES
C     *                         IF NECESSARY.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IPCP,NL,IL1,IL2,I
C
C     * OUTPUT ARRAYS.
C
      REAL VPD   (NL),  TADP  (NL),  PADRY (NL),  RHOAIR(NL),
     1     RHOSNI(NL),  RPCP  (NL),  TRPCP (NL),  
     2     SPCP  (NL),  TSPCP (NL)  
C
C     * INPUT ARRAYS.
C
      REAL TA    (NL),  QA    (NL),  PRESSG(NL), 
     1     PCPR  (NL),  RRATE (NL),  SRATE (NL)
C
C     * WORK ARRAYS.
C
      REAL PHASE (NL)
C
C     * TEMPORARY VARIABLES.
C
      REAL EA,CA,CB,EASAT,CONST,DFU,LMDA,LFU,PTA,CRIT,CRIT1,
     1     TI1,TI2,TT1,TT2,AA,BB,CC,GRUICBULB
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,HCPW,HCPICE,
     1     HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,
     2     RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
 
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C----------------------------------------------------------------
C
C     * CALCULATION OF ATMOSPHERIC INPUT VARIABLES.
C
      DO 100 I=IL1,IL2
          EA=QA(I)*PRESSG(I)/(0.622+0.378*QA(I))                              
          IF(TA(I).GE.TFREZ) THEN                                             
              CA=17.269                                                       
              CB=35.86                                                        
          ELSE                                                                
              CA=21.874                                                       
              CB=7.66                                                         
          ENDIF                                                               
          EASAT=611.0*EXP(CA*(TA(I)-TFREZ)/(TA(I)-CB))                        
          VPD(I)=MAX(0.0,(EASAT-EA)/100.0)                                     
          PADRY(I)=PRESSG(I)-EA                                                  
          RHOAIR(I)=PADRY(I)/(RGAS*TA(I))+EA/(RGASV*TA(I))                          
          CONST=LOG(EA/611.0)                                          
          TADP(I)=(CB*CONST-CA*TFREZ)/(CONST-CA)
C
C     * DENSITY OF FRESH SNOW.
C
          IF(TA(I).LE.TFREZ) THEN
              RHOSNI(I)=67.92+51.25*EXP((TA(I)-TFREZ)/2.59)
          ELSE
              RHOSNI(I)=MIN((119.17+20.0*(TA(I)-TFREZ)),200.0)
          ENDIF
C
C     * PRECIPITATION PARTITIONING BETWEEN RAIN AND SNOW.
C
          RPCP (I)=0.0  
          TRPCP(I)=0.0 
          SPCP (I)=0.0
          TSPCP(I)=0.0   
          IF(PCPR(I).GT.1.0E-8)                              THEN 
              IF(IPCP.EQ.1)                           THEN
                  IF(TA(I).GT.TFREZ) THEN
                      RPCP (I)=PCPR(I)/RHOW                   
                      TRPCP(I)=MAX((TA(I)-TFREZ),0.0)                   
                  ELSE
                      SPCP (I)=PCPR(I)/RHOSNI(I)
                      TSPCP(I)=MIN((TA(I)-TFREZ),0.0)               
                  ENDIF
              ELSEIF(IPCP.EQ.2)                       THEN
                  IF(TA(I).LE.TFREZ) THEN
                      PHASE(I)=1.0
                  ELSEIF(TA(I).GE.(TFREZ+2.0)) THEN
                      PHASE(I)=0.0 
                  ELSE
                      PHASE(I)=1.0-0.5*(TA(I)-TFREZ)
                  ENDIF
                  RPCP(I)=(1.0-PHASE(I))*PCPR(I)/RHOW
                  IF(RPCP(I).GT.0.0) TRPCP(I)=MAX((TA(I)-TFREZ),0.0) 
                  SPCP(I)=PHASE(I)*PCPR(I)/RHOSNI(I)
                  IF(SPCP(I).GT.0.0) TSPCP(I)=MIN((TA(I)-TFREZ),0.0)
              ELSEIF(IPCP.EQ.3)                       THEN
                  IF(TA(I).LE.TFREZ) THEN
                      PHASE(I)=1.0
                  ELSEIF(TA(I).GE.(TFREZ+6.0)) THEN
                      PHASE(I)=0.0
                  ELSE
                      PHASE(I)=(0.0202*(TA(I)-TFREZ)**6-0.3660*
     1                    (TA(I)-TFREZ)**5+2.0399*(TA(I)-TFREZ)**4-
     2                    1.5089*(TA(I)-TFREZ)**3-15.038*
     3                    (TA(I)-TFREZ)**2+4.6664*(TA(I)-TFREZ)+100.0)/
     4                    100.0
                      PHASE(I)=MAX(0.0,MIN(1.0,PHASE(I)))
                  ENDIF
                  RPCP(I)=(1.0-PHASE(I))*PCPR(I)/RHOW
                  IF(RPCP(I).GT.0.0) TRPCP(I)=MAX((TA(I)-TFREZ),0.0) 
                  SPCP(I)=PHASE(I)*PCPR(I)/RHOSNI(I)
                  IF(SPCP(I).GT.0.0) TSPCP(I)=MIN((TA(I)-TFREZ),0.0)
              ELSEIF(IPCP.EQ.4)                       THEN
                  RPCP(I)=RRATE(I)/RHOW
                  IF(RPCP(I).GT.0.0) TRPCP(I)=MAX((TA(I)-TFREZ),0.0) 
                  SPCP(I)=SRATE(I)/RHOSNI(I)
                  IF(SPCP(I).GT.0.0) TSPCP(I)=MIN((TA(I)-TFREZ),0.0)
              ELSEIF(IPCP.EQ.5)                       THEN
                  DFU=2.06*(10.0e-05)*(TA(I)/273.15)**1.75
                  LMDA=0.000063*TA(I)+0.00673
                  IF(TA(I)<273.16) THEN
                      LFU=1000.0*(2834.1-0.29*(TA(I)-273.16)-0.004*
     1                    (TA(I)-273.16)**2)
                  ELSE
                      LFU=1000.0*(2501.0-(2.361*(TA(I)-273.16)))
                  ENDIF
                  PTA=0.01801528*(EA/EASAT)*611.0*
     1             EXP(17.3*(TA(I)-273.16)/(237.3+(TA(I)-273.16)))/
     2             (8.31441*TA(I))
                  TI1=250.0
                  CRIT=9999.0
                  DO WHILE (CRIT > 0.0001) !Iteration solution optimised by using the Newton-Raphston method.
                      TT1=TI1+0.001*TI1
                      TT2=TI1-0.001*TI1
                      AA=-TI1+TA(I)+
     1                    (LFU*DFU/LMDA)*(PTA-0.01801528*611.0*
     2                    EXP(17.3*(TI1-273.16)/(237.3+(TI1-273.16)))/
     3                    (8.31441*TI1))
                      BB=-TT1+TA(I)+
     1                    (LFU*DFU/LMDA)*(PTA-0.01801528*611.0*
     2                    EXP(17.3*(TT1-273.16)/(237.3+(TT1-273.16)))/
     3                    (8.31441*TT1))
                      CC=-TT2+TA(I)+
     1                    (LFU*DFU/LMDA)*(PTA-0.01801528*611.0*
     2                    EXP(17.3*(TT2-273.16)/(237.3+(TT2-273.16)))/
     3                    (8.31441*TT2))
                      TI2=TI1-real(AA/((BB-CC)/(0.002*TI1)))
                      CRIT1=TI1-TI2
                      IF(CRIT1<0.0) THEN
                          CRIT=-CRIT1
                      ELSE
                          CRIT=CRIT1
                      ENDIF
                      TI1=TI2
                  END DO
                  GRUICBULB=TI1-273.16
                  IF(GRUICBULB<-10.0) THEN !Eoverflow if ratio calculated with icebulb < -39C.
                      PHASE(I)=1.0
                  ELSE
                      PHASE(I)=1.0-(1.0/(1.0 + 2.50286*
     1                    (0.125006**GRUICBULB)))
                      PHASE(I)=MAX(0.0,MIN(1.0,PHASE(I)))
                  ENDIF
                  RPCP(I)=(1.0-PHASE(I))*PCPR(I)/RHOW
                  IF(RPCP(I).GT.0.0) TRPCP(I)=MAX((TA(I)-TFREZ),0.0)
                  SPCP(I)=PHASE(I)*PCPR(I)/RHOSNI(I)
                  IF(SPCP(I).GT.0.0) TSPCP(I)=MIN((TA(I)-TFREZ),0.0)
              ENDIF
          ENDIF
100   CONTINUE
C
      RETURN
      END
