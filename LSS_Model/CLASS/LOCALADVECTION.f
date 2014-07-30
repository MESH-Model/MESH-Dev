      SUBROUTINE LOCALADVECTION(TSNOW,WSNOW,RHOSNO,QMELTG,
     1                  HTCS,HMFN,ZSNOW,TCSNOW,HCPSNO,PS,FS,HB,
     2                  HFSS,HEVS,FI,ILG,IL1,IL2,JL,q    )
C
C     * JAN 18/14 - M.MACDONALD.LOCAL ADVECTION OF SENSIBLE HEAT & WATER VAPOUR 
C     *                         FROM BARE GROUND TO SNOW PATCHES BASED ON
C     *                         CHERKAUER & LETTENMAIER (2003) PARAMETERISATION
C     *                         OF MARSH AND POMEROY (1996) AND MARSH ET AL (1997).
C     *                         CHANGES TO TSNOW AND QMELT BASED ON TSPOST.
C
      use MODELS, only : Nmod, lam
      IMPLICIT NONE
C                                                                                 
C     * INTEGER CONSTANTS.
C
      INTEGER ILG,IL1,IL2,JL,I,J,q
C
C     * INPUT/OUTPUT ARRAYS.
C
      REAL TSNOW (ILG),    WSNOW (ILG),    RHOSNO(ILG),
     1     QMELTG(ILG),    HTCS  (ILG),    HMFN  (ILG),
     2     HFSS  (ILG),    HEVS  (ILG)
C
C     * INPUT ARRAYS.
C
      REAL ZSNOW (ILG,Nmod),  TCSNOW(ILG), HCPSNO(ILG),   FI(ILG,Nmod)
      DOUBLE PRECISION     PS    (ILG),       FS    (ILG), HB    (ILG)
C
C     * TEMPORARY VARIABLES.
C
      DOUBLE PRECISION HADD,HCONV,WFREZ,HA
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1     SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C
      COMMON /CLASS1/ DELT,TFREZ                                                  
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C
      DO 100 I=IL1,IL2
          IF(FI(I,q).GT.0. .AND. HB(I).GT.0.)                       THEN
              !GZERO(I): positive into snowpackK
              HA=FS(I)*HB(I)*(1-PS(I))/PS(I)
              HFSS(I)=HFSS(I)-FI(I,q)*HA !check +-
              TSNOW(I)=TSNOW(I)+HA*DELT/(HCPSNO(I)*ZSNOW(I,q)) !check +-
              !TSNOW(I)=TSNOW(I)-(HA/2)*DELT/(HCPSNO(I)*ZSNOW(I,q))
              !HEVS(I)=HEVS(I)-FI(I,q)*(HA/2) !check +-
              IF(TSNOW(I).GT.0.)                                THEN
                  QMELTG(I)=QMELTG(I)+TSNOW(I)*HCPSNO(I)*ZSNOW(I,q)/DELT
                  TSNOW(I)=0.                                                               
              ENDIF
          ENDIF
  100 CONTINUE
C 
      DO 200 I=IL1,IL2
           IF(FI(I,q).GT.0. .AND. TSNOW(I).LT.0. .AND. WSNOW(I).GT.0.
     1         .AND. HB(I).GT.0.)                                   THEN
             HTCS(I)=HTCS(I)-FI(I,q)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1               ZSNOW(I,q)/DELT
             HADD=-TSNOW(I)*HCPSNO(I)*ZSNOW(I,q)
             HCONV=CLHMLT*WSNOW(I)
             IF(HADD.LE.HCONV)                           THEN                                                  
                 WFREZ=HADD/CLHMLT
                 HADD=0.0
                 WSNOW(I)=MAX(0.0,WSNOW(I)-WFREZ)
                 TSNOW(I)=0.0
                 RHOSNO(I)=RHOSNO(I)+WFREZ/ZSNOW(I,q)
                 HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE+HCPW*WSNOW(I)/
     1               (RHOW*ZSNOW(I,q))
             ELSE                                                                        
                 HADD=HADD-HCONV                                                         
                 WFREZ=WSNOW(I)
                 WSNOW(I)=0.0
                 RHOSNO(I)=RHOSNO(I)+WFREZ/ZSNOW(I,q)
                 HCPSNO(I)=HCPICE*RHOSNO(I)/RHOICE
                 TSNOW(I)=-HADD/(HCPSNO(I)*ZSNOW(I,q))
             ENDIF
             HMFN(I)=HMFN(I)-FI(I,q)*CLHMLT*WFREZ/DELT
             HTCS(I)=HTCS(I)-FI(I,q)*CLHMLT*WFREZ/DELT
             HTCS(I)=HTCS(I)+FI(I,q)*HCPSNO(I)*(TSNOW(I)+TFREZ)*
     1               ZSNOW(I,q)/DELT
          ENDIF
  200 CONTINUE
C
      RETURN                                                                      
      END
