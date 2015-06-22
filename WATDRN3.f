C
CDGP  2011-05-05: FOR WARDRN3 (RIC)
C
C     *****************************************************************
C     SUB WATDRN3
C     *****************************************************************
C     IMPLEMENTS RIC'S WATDRN METHOD.  VARIABLES ARE COLLECTED BY 
C     WATDRNB.
C     *****************************************************************
C     JUN 03/11 - DGP      ADDED
C     JUL 10/11 - DGP      ADDED USE FLAGS FOR WD3FLOW.
C
      SUBROUTINE WATDRN3 (ASAT0,ASAT1,GRKSAT,GRKEFF,DELT,
     1  SUBFLW,BASFLW,
     2  IGND,NA,NTYPE,ILG,IL1,IL2,ILMOS,JLMOS,
     3  BTC,BCAP,DCOEFF,BFCAP,BFCOEFF,BFMIN,BQMAX)
C
      USE FLAGS
C
      IMPLICIT NONE
C
      INTEGER :: IGND,NA,NTYPE,ILG,IL1,IL2,K,M
C
      INTEGER, DIMENSION(ILG) :: ILMOS,JLMOS
C
      REAL, DIMENSION(NTYPE,IGND) :: BTC,BCAP,DCOEFF,BFCAP,BFCOEFF,
     1  BFMIN,BQMAX
      REAL, DIMENSION(ILG) :: ASAT0,ASAT1,GRKSAT,GRKEFF,
     1  SUBFLW,BASFLW
      REAL, DIMENSION(IGND) :: QHAT,BFHAT
      REAL :: DELT,QFLOW,BFLOW,TOL
C
C     *****************************************************************
      TOL=1.E-6
C
C     *****************************************************************
C     CALCULATE SUBFLOW AND BASEFLOW.  WE HAVE TO TRANSLATE FROM GAT-
C     TO ROW-INDEXING.
C
      DO K=IL1,IL2
        QFLOW=0.
        BFLOW=0.
        DO M=1,IGND
*FOR SUBFLOW
        IF (ASAT0(K) .LE. (BCAP(JLMOS(K),M)+TOL)) THEN
            QHAT(M)=0.
        ELSE IF (ASAT0(K) .GE. (BTC(JLMOS(K),M)-TOL)) THEN
            QHAT(M)=1.
        ELSE
            QHAT(M)=
     1  (ASAT0(K)-BCAP(JLMOS(K),M))
     2    /(BTC(JLMOS(K),M)-BCAP(JLMOS(K),M))
     3  **DCOEFF(JLMOS(K),M)
        END IF
            QHAT(M)=BQMAX(JLMOS(K),M)*QHAT(M)
*FOR BASEFLOW
        IF (ASAT0(K) .LE. (BFCAP(JLMOS(K),M)+TOL)) THEN
            BFHAT(M)=0.
        ELSE IF (ASAT0(K) .GE. (1.-TOL)) THEN
            BFHAT(M)=1.
        ELSE
            BFHAT(M)=
     1  (ASAT0(K)-BFCAP(JLMOS(K),M))
     2    /(1.-BFCAP(JLMOS(K),M))
     3  **BFCOEFF(JLMOS(K),M)
        END IF
            BFHAT(M)=BFMIN(JLMOS(K),M)*BFHAT(M)
        ENDDO
        DO M=1,IGND
            QFLOW = QFLOW + QHAT(M)
            BFLOW = BFLOW + BFHAT(M)
        ENDDO
        ASAT1=MAX(0.0,ASAT0-QFLOW*DELT)
*CORRECTION/CONFIGURATION FOR NOT USED BASFLW
*WD3FLOW=0 THEN SUBFLW=SUBFLW,BASFLW=BASFLW
        SUBFLW(K)=QFLOW
        BASFLW(K)=BFLOW
*WD3FLOW=1 THEN SUBFLW=SUBFLW+BASFLW,BASFLW=0
*WD3FLOW=2 THEN SUBFLW=SUBFLW,BASFLW=0
        IF (WD3FLOW .EQ. 1) THEN
            SUBFLW(K)=SUBFLW(K)+BASFLW(K)
        END IF
        IF (WD3FLOW .EQ. 1 .OR. WD3FLOW .EQ. 2) THEN
            BASFLW(K)=0.
        END IF
      ENDDO
C
C     *****************************************************************
C     RETURN
C
      RETURN
C
      END SUBROUTINE WATDRN3
