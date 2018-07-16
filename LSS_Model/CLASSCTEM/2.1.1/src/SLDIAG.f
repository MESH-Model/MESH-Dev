!>\file
!!CALCULATES NEAR SURFACE OUTPUT VARIABLES
!!
      SUBROUTINE SLDIAG(SUT,SVT,STT,SQT,CDM,CDH,UA,VA,TA,QA,T0,Q0,
     1                  Z0M,Z0E,F,ZA,ZU,ZT,ILG,IL1,IL2,JL)

C     * JUN 23/14 - M.LAZARE.   New version for gcm18+:                 
C     *                         - Bugfix to calculation of              
C     *                           screen temperature and                
C     *                           screen specific humidity.             
C     *                         - Accumulation removed (now             
C     *                           done in classt/oiflux11) so           
C     *                           that a screen relative humidity       
C     *                           can be calculated. Therefore,        
C     *                           "instantaneous" fields are            
C     *                           calculated and passed out             
C     *                           instead. 
C     * SEP 05/12 - J.MELTON.   Made some numbers explicitly reals                              
C     * OCT 17/11 - D.VERSEGHY. ADD CODE TO CIRCUMVENT SPECIAL
C     *                         CASE WHERE TA~T0 OR QA~QO, THUS
C     *                         AVOIDING A DIVIDE BY ZERO.
C     * NOV 04/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * JUL 19/96 - Y. DELAGE.  
C     ------------------------------------

      IMPLICIT NONE
c
C     * INTEGER CONSTANTS
C
      INTEGER ILG   !<NUMBER OF POINTS TO BE TREATED
      INTEGER IL1,IL2,JL,I
c
C     * INPUT FIELDS
C
      REAL SUT(ILG)  !<U COMPONENT OF THE WIND AT ZU  
      REAL SVT(ILG)  !<V COMPONENT OF THE WIND AT ZU
      REAL STT(ILG)  !<TEMPERATURE AT ZT
      REAL SQT(ILG)  !<SPECIFIC HUMIDITY AT ZT
C
C     * OUTPUT FIELDS
C
      REAL CDM(ILG) !<DRAG COEFFICIENT
      REAL CDH(ILG) !<TRASFER COEFFICIENT FOR HEAT AND MOISTURE
      REAL UA(ILG)  !<U COMPONENT OF THE WIND AT ZA
      REAL VA(ILG)  !<V COMPONENT OF THE WIND AT ZA
      REAL TA(ILG)  !<POTENTIAL TEMPERATURE AT ZA
      REAL QA(ILG)  !<SPECIFIC HUMIDITY AT REFERENCE HEIGHT
      REAL Z0M(ILG) !<ROUGHNESS LENGTH FOR MOMENTUM
      REAL Z0E(ILG) !<ROUGHNESS LENGTH FOR HEAT AND MOISTURE
      REAL F(ILG)   !<FRACTION OF GRID POINT AFFECTED BY PROCESS
      REAL T0(ILG)  !<TEMPERATURE AT BOTTOM OF SURFACE LAYER
      REAL Q0(ILG)  !<SPECIFIC HUMIDITY AT BOTTOM OF SURFACE LAYER
      REAL ZA(ILG)  !<TOP OF SURFACE LAYER
      REAL ZU(ILG)  !<HEIGHT OF OUTPUT WIND
      REAL ZT(ILG)  !<HEIGHT OF OUTPUT TEMPERATURE AND HUMIDITY
c
C     TEMPORARY VARIABLES
C 
      REAL PR,WSPD,CM,US,TS,QS,L,UVA,RATIO,UVU,TTA,CE
c
C     * COMMON BLOCK PARAMETERS
C
      REAL RGAS     !<Gas constant [J kg-1 K-1]
      REAL RGASV    !<Gas constant for water vapour [J kg-1 K-1]
      REAL GRAV     !<Acceleration due to gravity [m s-1]
      REAL SBC      !<Stefan-Boltzmann constant [W m-2 K-4]
      REAL VKC      !<Von Karman constant (0.40)
      REAL CT       !<Drag coefficient for water (1.15*10^-3)
      REAL VMIN     !<Minimum wind speed (0.1) [m s-1]
c
      REAL PSM,PSE,Y,PIM,PIE,X
c
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      
c     * STABILITY FUNCTIONS FOR THE STABLE CASE

      PSM(X)= -X -.667*(X-5.0/.35)*EXP(-.35*X)
      PSE(X)= -(1.0+.667*X)**1.5 -.667*(X-5.0/.35)*EXP(-.35*X)

c     * STABILITY FUNCTIONS FOR THE UNSTABLE CASE

      Y(X)=(1.0-16.0*X)**.25
      PIM(X)= LOG((1.0+X)**2*(1.0+X**2)) -2.0*ATAN(X)
      PIE(X)= 2.0*LOG(1.0+X**2)

      PR=1.0
      DO 100 I=IL1,IL2
      IF(F(I).GT.0.)                                                THEN

C     * CALCULATION OF SURFACE FLUXES AND MONIN-OBUKHOV LENGTH

        WSPD=MAX(VMIN,SQRT(UA(I)**2+VA(I)**2))
        CM=SQRT(CDM(I))
        US=CM*WSPD

        IF(ABS(TA(I)-T0(I)).LT.0.01) THEN
            TS=-0.01*CDH(I)/CM
        ELSE
            TS=CDH(I)*(TA(I)-T0(I))/CM
        ENDIF
        IF(ABS(QA(I)-Q0(I)).LT.1.0E-7) THEN
            QS=-1.0E-7*CDH(I)/CM
        ELSE
            QS=CDH(I)*(QA(I)-Q0(I))/CM
        ENDIF

        L=TA(I)*US**2/(VKC*GRAV*(TS*(1.0+.61*QA(I))+.61*TA(I)*QS))
      
C     * CALCULATE CORRECTION FACTORS TO TAKE INTO ACCOUNT THE APPROXIMATIONS
C     * IN DRCOEF

        IF(L.GT.0.)                                                 THEN

C     * STABLE CASE
          
         UVA=US/VKC*(LOG(ZA(I)/Z0M(I))-PSM(ZA(I)/L)+PSM(Z0M(I)/L))
         RATIO=WSPD/UVA
         UVU=US/VKC*(LOG((ZU(I)+Z0M(I))/Z0M(I))-PSM((ZU(I)+Z0M(I))/L)
     1       +PSM(Z0M(I)/L))*RATIO
         TTA=T0(I)+TS/VKC*PR*(LOG(ZA(I)/Z0E(I))-PSE(ZA(I)/L)+           
     1          PSE(Z0E(I)/L))                                          
         RATIO=(TA(I)-T0(I))/SIGN(MAX(ABS(TTA-T0(I)),1.E-4),TTA-T0(I))
         CE=(LOG((ZT(I)+Z0M(I))/Z0E(I))-PSE((ZT(I)+Z0M(I))/L)
     1      +PSE(Z0E(I)/L))*RATIO*PR/VKC

        ELSE

C     * UNSTABLE CASE

         UVA=US/VKC*(LOG(ZA(I)/Z0M(I))-PIM(Y(ZA(I)/L))+PIM(Y(Z0M(I)/L)))
         RATIO=WSPD/UVA
         UVU=US/VKC*(LOG((ZU(I)+Z0M(I))/Z0M(I))-PIM(Y((ZU(I)+Z0M(I))/L))
     1        +PIM(Y(Z0M(I)/L)))*RATIO

         TTA=T0(I)+TS/VKC*PR*(LOG(ZA(I)/Z0E(I))-PIE(Y(ZA(I)/L))+
     1          PIE(Y(Z0E(I)/L)))
         RATIO=(TA(I)-T0(I))/SIGN(MAX(ABS(TTA-T0(I)),1.E-4),TTA-T0(I))
         CE=(LOG((ZT(I)+Z0M(I))/Z0E(I))-PIE(Y((ZT(I)+Z0M(I))/L))
     1      +PIE(Y(Z0E(I)/L)))*RATIO*PR/VKC

        ENDIF
C                                                                       
        SUT(I)=UVU*UA(I)/WSPD                                           
        SVT(I)=UVU*VA(I)/WSPD                                           
        STT(I)=T0(I)+TS*CE                                              
        SQT(I)=Q0(I)+QS*CE                                              
      ENDIF
  100 CONTINUE

      RETURN
      END




