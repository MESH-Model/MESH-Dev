      SUBROUTINE TLSPOST(GSNOW,TSNOW,WSNOW,RHOSNO,QMELTS,GZERO,TSNBOT,
     1                  HTCS,HMFN,QFN,EVAPS,RPCN,TRPCN,SPCN,TSPCN,
     2                  GCONSTS,GCOEFFS,T0,ZSNOW,TCSNOW,HCPSNO,QTRANS,
     3                  RPCP,TRPCP,SPCP,TSPCP,TZEROS,RHOSNI,
     4                  FLS,DELSKIN,ILG,IL1,IL2,JL            )
C
C     * SEP 01/15 - D.VERSEGHY. LAKE SNOW TEMPERATURE AND HEAT FLUX
C     *                         CALCULATIONS (BASED ON CLASS SUBROUTINE
C     *                         TSPOST); NET SURFACE WATER FLUX TERMS
C     *                         (BASED ON CLASS SUBROUTINE WPREP).
C
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I,J
C
C     * OUTPUT ARRAYS.
C
      REAL GZERO (ILG),    TSNBOT(ILG),
     1     RPCN  (ILG),    TRPCN (ILG),    SPCN  (ILG),    TSPCN (ILG)
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL GSNOW (ILG),    TSNOW (ILG),    WSNOW (ILG),    RHOSNO(ILG),
     1     QMELTS(ILG),    HTCS  (ILG),    HMFN  (ILG),    QFN   (ILG),
     2     EVAPS (ILG)
C
C     * INPUT ARRAYS.
C
      REAL T0    (ILG),    ZSNOW (ILG),    TCSNOW (ILG),   HCPSNO(ILG), 
     1     QTRANS(ILG),    GCONSTS(ILG),   GCOEFFS(ILG),   
     2     RPCP  (ILG),    TRPCP  (ILG),   SPCP   (ILG),   TSPCP (ILG),
     3     TZEROS(ILG),    RHOSNI (ILG),   FLS    (ILG)
C
      REAL DELSKIN
C
C     * TEMPORARY VARIABLES.
C
      REAL RADD  (ILG),    SADD  (ILG)
C
      REAL HADD,HCONV,WFREZ
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1     RHOSOL,RHOOM,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     2     SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ                                       
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C
      DO 100 I=IL1,IL2
          IF(FLS(I).GT.0.)                                          THEN
              TSNBOT(I)=(ZSNOW(I)*TSNOW(I)+DELSKIN*T0(I))/
     1             (ZSNOW(I)+DELSKIN)
              GZERO(I)=-2.0*TCSNOW(I)*(TSNBOT(I)-TSNOW(I))/ZSNOW(I)
              IF(QMELTS(I).LT.0.)                               THEN
                  GSNOW(I)=GSNOW(I)+QMELTS(I)                           
                  QMELTS(I)=0.                                          
              ENDIF                                                     
              TSNOW(I)=TSNOW(I)+(GSNOW(I)-GZERO(I))*DELT/
     1                          (HCPSNO(I)*ZSNOW(I))-TFREZ             
              IF(TSNOW(I).GT.0.)                                THEN
                  QMELTS(I)=QMELTS(I)+TSNOW(I)*HCPSNO(I)*ZSNOW(I)/DELT
                  GSNOW(I)=GSNOW(I)-TSNOW(I)*HCPSNO(I)*ZSNOW(I)/DELT
                  TSNOW(I)=0.                                         
              ENDIF                                                  
          ENDIF
  100 CONTINUE
C 
      DO 200 I=IL1,IL2
           IF(FLS(I).GT.0. .AND. TSNOW(I).LT.0. .AND. WSNOW(I).GT.0.)
     1                                                              THEN
             HTCS(I)=HTCS(I)-FLS(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
             HADD=-TSNOW(I)*HCPSNO(I)*ZSNOW(I)
             HCONV=CLHMLT*WSNOW(I)
             IF(HADD.LE.HCONV)                           THEN      
                 WFREZ=HADD/CLHMLT
                 HADD=0.0
                 WSNOW(I)=MAX(0.0,WSNOW(I)-WFREZ)
                 TSNOW(I)=0.0
                 RHOSNO(I)=RHOSNO(I)+WFREZ/ZSNOW(I)
                 HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1               (RHOW*ZSNOW(I))
             ELSE                 
                 HADD=HADD-HCONV 
                 WFREZ=WSNOW(I)
                 WSNOW(I)=0.0
                 RHOSNO(I)=RHOSNO(I)+WFREZ/ZSNOW(I)
                 HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
                 TSNOW(I)=-HADD/(HCPSNO(I)*ZSNOW(I))
             ENDIF
             HMFN(I)=HMFN(I)-FLS(I)*CLHMLT*WFREZ/DELT
             HTCS(I)=HTCS(I)-FLS(I)*CLHMLT*WFREZ/DELT
             HTCS(I)=HTCS(I)+FLS(I)*HCPSNO(I)*(TSNOW(I)+TFREZ)*ZSNOW(I)/
     1               DELT
          ENDIF
  200 CONTINUE
C
      DO 300 I=IL1,IL2
          QFN(I)=FLS(I)*EVAPS(I)*RHOW
          IF(SPCP(I).GT.0. .OR. EVAPS(I).LT.0.) THEN           
              SADD(I)=SPCP(I)-EVAPS(I)*RHOW/RHOSNI(I)
              IF(ABS(SADD(I)).LT.1.0E-12) SADD(I)=0.0
              IF(SADD(I).GT.0.0) THEN                          
                  SPCN (I)=SADD(I)                            
                  IF(SPCP(I).GT.0.0) THEN
                      TSPCN(I)=TSPCP(I)
                  ELSE
                      TSPCN(I)=MIN((TZEROS(I)-TFREZ),0.0)
                  ENDIF
                  EVAPS(I)=0.0                               
              ELSE                                            
                  EVAPS(I)=-SADD(I)*RHOSNI(I)/RHOW           
                  SPCN (I)=0.0                               
                  TSPCN(I)=0.0                               
              ENDIF                                           
          ELSE                                                
              SPCN (I)=0.0                                   
              TSPCN(I)=0.0                                   
          ENDIF

          IF(RPCP(I).GT.0.)                         THEN     
              RADD(I)=RPCP(I)-EVAPS(I)                      
              IF(ABS(RADD(I)).LT.1.0E-12) RADD(I)=0.0
              IF(RADD(I).GT.0.)   THEN                      
                  RPCN (I)=RADD(I)                         
                  TRPCN(I)=TRPCP(I)
                  EVAPS(I)=0.0                            
              ELSE                                         
                  EVAPS(I)=-RADD(I)                       
                  RPCN (I)=0.0                            
                  TRPCN(I)=0.0                           
              ENDIF                                       
          ELSE                                            
              RPCN (I)=0.0                               
              TRPCN(I)=0.0                               
          ENDIF                                             
  300 CONTINUE
C
      RETURN                    
      END
