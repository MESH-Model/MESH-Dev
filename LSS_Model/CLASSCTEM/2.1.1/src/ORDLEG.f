!>\file
!!THIS ROUTINE IS A SUBSET OF BELOUSOVS ALGORITHM 
!!USED TO CALCULATE ORDINARY LEGENDRE POLYNOMIALS.
!!
!!SX = LEGENDRE POLYNOMIAL EVALUATED AT COA
!!
!!COA = COSINE OF COLATITUDE
!!
!!IR = WAVE NUMBER 
!! 
      SUBROUTINE ORDLEG(SX,COA,IR)
C 
C     * JUL 14/92 - E. CHAN (ADD REAL*8 DECLARATIONS)
C
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z) 
C
      PI = 3.1415926535898
      SQR2=SQRT(2.) 
      IRPP = IR + 1 
      IRPPM = IRPP - 1
      DELTA =   ACOS(COA) 
      SIA =  SIN(DELTA) 
C 
      THETA=DELTA 
      C1=SQR2 
C 
      DO 20 N=1,IRPPM 
CRL L      FN=FLOAT(N) 
      FN=REAL(N) 
      FN2=2.*FN 
      FN2SQ=FN2*FN2 
      C1=C1* SQRT(1.0-1.0/FN2SQ)
   20 CONTINUE
C 
      N=IRPPM 
      ANG=FN*THETA
      S1=0.0
      C4=1.0
      A=-1.0
      B=0.0 
      N1=N+1
C 
      DO 27 KK=1,N1,2 
      K=KK-1
      IF (K.EQ.N) C4=0.5*C4 
      S1=S1+C4* COS(ANG)
      A=A+2.0 
      B=B+1.0 
CRL L      FK=FLOAT(K)
      FK=REAL(K)
      ANG=THETA*(FN-FK-2.0) 
      C4=(A*(FN-B+1.0)/(B*(FN2-A)))*C4
   27 CONTINUE
C 
      SX=S1*C1
C 
      RETURN
      END 
