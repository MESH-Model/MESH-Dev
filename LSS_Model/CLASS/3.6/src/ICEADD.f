      SUBROUTINE ICEADD(TBAR,TPOND,ZPOND,TSNOW,RHOSNO,ZSNOW,HCPSNO,
     1                  ALBSNO,HMFG,HTCS,HTC,WTRS,WTRG,GFLUX,
     2                  RUNOFF,TRUNOF,OVRFLW,TOVRFL,ZPLIM,GGEO,
     3                  FI,EVAP,R,TR,GZERO,G12,G23,HCP,QMELT,WSNOW,
     4                  ZMAT,TMOVE,WMOVE,ZRMDR,TADD,ZMOVE,TBOT,DELZ,
     5                  ISAND,ICONT,IWF,IG,IGP1,IGP2,ILG,IL1,IL2,JL,N)
C
C     * JAN 31/19 - D.PRINCZ.   SPLIT ICEBAL SO IT CAN BE USED WITH
C     *                         OTHER OVERLAND RUNOFF ROUTINES.
C     *                         MOVED MOVING R TO ZPOND FROM ICEBAL
C     *                         TO FACILITATE RESTRUCTURING.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER IG,IGP1,IGP2,ILG,IL1,IL2,JL,I,J,K,N
      INTEGER IWF(ILG)
C
C     * INPUT/OUTPUT FIELDS.
C
      REAL TBAR  (ILG,IG), HMFG  (ILG,IG), HTC   (ILG,IG),
     1     GFLUX (ILG,IG)
C
      REAL TPOND (ILG),    ZPOND (ILG),    TSNOW (ILG),    RHOSNO(ILG),    
     1     ZSNOW (ILG),    HCPSNO(ILG),    ALBSNO(ILG),    HTCS  (ILG),    
     2     WTRS  (ILG),    WTRG  (ILG),    RUNOFF(ILG),    TRUNOF(ILG),
     3     OVRFLW(ILG),    TOVRFL(ILG)
C
C     * INPUT FIELDS.
C
      REAL FI    (ILG),    EVAP  (ILG),    R     (ILG),    TR    (ILG), 
     1     GZERO (ILG),    G12   (ILG),    G23   (ILG),    QMELT (ILG),
     2     WSNOW (ILG),    ZPLIM (ILG),    GGEO  (ILG)
C
      REAL HCP   (ILG,IG)
C
      INTEGER              ISAND (ILG,IG)
C
      REAL DELZ  (IG)
C
C     * WORK FIELDS.
C
      REAL ZMAT  (ILG,IGP2,IGP1),          TMOVE (ILG,IGP2),
     1     WMOVE (ILG,IGP2),               ZRMDR (ILG,IGP1)
C
      REAL TADD  (ILG),    ZMOVE (ILG),    TBOT  (ILG) 
C
      INTEGER              ICONT (ILG)
C
C     * TEMPORARY VARIABLES.
C
      REAL RADD
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,
     1     SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
C                                    
      COMMON /CLASS1/ DELT,TFREZ                                               
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
C-----------------------------------------------------------------------
C
C     * ADD RAINFALL OR SNOWMELT TO PONDED WATER FOR ISAND -4.
C
      DO 100 I=IL1,IL2
          IF(FI(I).GT.0. .AND. ISAND(I,1).EQ.-4)                THEN
              IF(R(I).GT.0.)                                THEN 
                 RADD=R(I)*DELT                                                             
                 TPOND(I)=((TPOND(I)+TFREZ)*ZPOND(I)+(TR(I)+TFREZ)*
     1               RADD)/(ZPOND(I)+RADD)-TFREZ
                 ZPOND(I)=ZPOND(I)+RADD                                                        
                 HTC (I,1)=HTC(I,1)+FI(I)*(TR(I)+TFREZ)*HCPW*
     1                     RADD/DELT
              ENDIF                                                                       
          ENDIF
  100 CONTINUE                                                                
C                                                                                  
      RETURN                                                                      
      END        
