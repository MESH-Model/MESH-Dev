      SUBROUTINE TLSPREP(GCOEFFS,GCONSTS,CPHCHS,TCSNOW,HCPSNO,IWATER,
     1                  ZRSLDM,ZRSLDH,ZRSLFM,ZRSLFH,ZDSLM,ZDSLH,
     2                  ZOSCLM,ZOSCLH,ZOMLNS,ZOELNS,ZOM,ZOH,
     3                  TVIRTA,TPOTA,CRIB,DRAGS,CEVAP,IEVAP,ISAND,
     4                  FLS,ZSNOW,TSNOW,RHOSNO,WSNOW,ZREFM,ZREFH,
     5                  ZDIAGM,ZDIAGH,TA,QA,VA,IZREF,ILG,IL1,IL2,IG,JL)
C
C     * MAY 13/15 - D.VERSEGHY. PREPARATION FOR LAKE SNOW TEMPERATURE
C     *                         CALCULATIONS (BASED ON CLASS 
C     *                         SUBROUTINES CLASST, TPREP AND TSPREP).
C
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER IZREF,ILG,IL1,IL2,IG,JL,I,J
C
C     * OUTPUT ARRAYS.
C
      REAL GCOEFFS(ILG),   GCONSTS(ILG),   CPHCHS (ILG),   TCSNOW (ILG),
     1     ZRSLDM (ILG),   ZRSLDH (ILG),   ZRSLFM (ILG),   ZRSLFH (ILG),
     2     ZDSLM  (ILG),   ZDSLH  (ILG),   ZOSCLM (ILG),   ZOSCLH (ILG),
     3     ZOMLNS (ILG),   ZOELNS (ILG),   ZOM    (ILG),   ZOH    (ILG),
     4     TVIRTA (ILG),   TPOTA  (ILG),   CRIB   (ILG),   DRAGS  (ILG),
     5     CEVAP  (ILG),   HCPSNO (ILG)
C
      INTEGER              IWATER(ILG),    IEVAP  (ILG)     
      INTEGER              ISAND (ILG,IG)
C
C     * INPUT ARRAYS.
C
      REAL FLS   (ILG),    ZSNOW (ILG),    TSNOW (ILG),    RHOSNO(ILG),
     1     WSNOW (ILG),    ZREFM (ILG),    ZREFH (ILG),    ZDIAGM(ILG),
     2     ZDIAGH(ILG),    TA    (ILG),    QA    (ILG),    VA    (ILG)
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,
     1     HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,
     2     SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP,
     3     DELTA,CGRAV,CKARM,CPD 
C
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /PHYCON/ DELTA,CGRAV,CKARM,CPD
C-----------------------------------------------------------------------
C     * CALCULATIONS FOR SNOW-COVERED GROUND.                           
C                                                                       
      DO 100 I=IL1,IL2                                              
          IF(FLS(I).GT.0.)                                      THEN
              ZOM(I)=0.001
              ZOMLNS(I)=LOG(ZOM(I))
              ZOH(I)=0.0003
              ZOELNS(I)=LOG(ZOH(I))
              IF(IZREF.EQ.1) THEN                                   
                  ZRSLDM(I)=ZREFM(I)                                
                  ZRSLDH(I)=ZREFH(I)                                
                  ZRSLFM(I)=ZREFM(I)-ZOM(I)                         
                  ZRSLFH(I)=ZREFH(I)-ZOM(I)                         
                  ZDSLM(I)=ZDIAGM(I)-ZOM(I)                         
                  ZDSLH(I)=ZDIAGH(I)-ZOM(I)                         
                  TPOTA(I)=TA(I)+ZRSLFH(I)*CGRAV/CPD                 
              ELSE                                                  
                  ZRSLDM(I)=ZREFM(I)+ZOM(I)                         
                  ZRSLDH(I)=ZREFH(I)+ZOM(I)                         
                  ZRSLFM(I)=ZREFM(I)                                
                  ZRSLFH(I)=ZREFH(I)                                
                  ZDSLM(I)=ZDIAGM(I)                                
                  ZDSLH(I)=ZDIAGH(I)                                
                  TPOTA(I)=TA(I)                                    
              ENDIF                                                 
              ZOSCLM(I)=ZOM(I)/ZRSLDM(I)                            
              ZOSCLH(I)=ZOH(I)/ZRSLDH(I)                            
              TVIRTA(I)=TPOTA(I)*(1.0+0.61*QA(I))                   
              CRIB(I)=-CGRAV*ZRSLDM(I)/(TVIRTA(I)*VA(I)**2)          
              DRAGS(I)=(CKARM/(LOG(ZRSLDM(I))-ZOMLNS(I)))**2  
          ENDIF                                                     
  100     CONTINUE                                                      
C                                                                       
C     * THERMAL PROPERTIES OF SNOW.                                     
C                                                                       
      DO 200 I=IL1,IL2                                                  
          IF(ZSNOW(I).GT.0.)                                        THEN
              HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/          
     1            (RHOW*ZSNOW(I))                                       
              IF(RHOSNO(I).LT.156.0) THEN                               
                  TCSNOW(I)=0.234E-3*RHOSNO(I)+0.023                    
              ELSE                                                      
                  TCSNOW(I)=3.233E-6*RHOSNO(I)*RHOSNO(I)-1.01E-3*       
     1                RHOSNO(I)+0.138                                   
              ENDIF                                                     
Cmdm          TCSNOW(I)=2.576E-6*RHOSNO(I)*RHOSNO(I)+0.074       !test: Mellor
Cmdm          TCSNOW(I)=0.021+4.2E-4*RHOSNO(I)+
Cmdm 1            2.2E-9*RHOSNO(I)*RHOSNO(I)*RHOSNO(I)      !test: Rogers et al 1995
Cmdm          TCSNOW(I)=2.22362*(RHOSNO(I)/1000.0)**1.885   !test: Yen 1981
Cmdm          TCSNOW(I)=0.0688*EXP(0.0088*
Cmcm 1                      (TSNOW(I)-273.16)+4.6682*RHOSNO(I)/1000.0) !Pitman&Zuckerman 1967
          ENDIF
  200 CONTINUE
C                                                                       
C     * CALCULATE COEFFICIENTS.
C
      DO 300 I=IL1,IL2
          IF(FLS(I).GT.0.)                                          THEN
              GCOEFFS(I)=3.0*TCSNOW(I)/ZSNOW(I)
              GCONSTS(I)=-3.0*TCSNOW(I)*TSNOW(I)/ZSNOW(I)
              CPHCHS(I)=CLHVAP+CLHMLT
              IWATER(I)=2             
          ELSE
              IWATER(I)=1
          ENDIF
          CEVAP(I)=1.0
          IEVAP(I)=1
          DO 250 J=1,IG
              ISAND(I,J)=-4
  250     CONTINUE
  300 CONTINUE
C
      RETURN                                                                      
      END
