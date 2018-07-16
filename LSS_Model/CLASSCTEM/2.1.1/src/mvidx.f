!>\file

C     * MAY 12/2004 - L.SOLHEIM
C     *
C>     * GIVEN A MONOTONIC VECTOR V OF LENGTH N AND A VALUE X,
C!     * RETURN THE INDEX MVIDX SUCH THAT X IS BETWEEN 
C!     * V(MVIDX) AND V(MVIDX+1).
C!     *
C!     * V MUST BE MONOTONIC, EITHER INCREASING OF DECREASING.
C!     * THERE IS NO CHECK ON WHETHER OR NOT THIS VECTOR IS 
C!     * MONOTONIC.
C!     *
C!     * THIS FUNCTION RETURNS 1 OR N-1 IF X IS OUT OF RANGE.
C!     *
C!     * INPUT:
C!     *
C!     *   REAL    V(N) ...MONITONIC VECTOR (INCREASING OR DECREASING)
C!     *   INTEGER N    ...SIZE OF V
C!     *   REAL    X    ...SINGLE REAL VALUE
C!     *
C!     * OUTPUT:
C!     *
C!     *   V(MVIDX) .LE/.GE. X .LE./.GE. V(MVIDX+1)
C!
      INTEGER FUNCTION MVIDX(V,N,X)
C-----------------------------------------------------------------------
      IMPLICIT REAL (A-H,O-Z),
     +INTEGER (I-N)

      REAL X,V(N)
      INTEGER N
      INTEGER JL,JM,JU
C-----------------------------------------------------------------------

      IF(X.EQ.V(1)) THEN
        MVIDX=1
        RETURN
      ENDIF
      IF(X.EQ.V(N)) THEN
        MVIDX=N-1
        RETURN
      ENDIF
      JL=1
      JU=N
10    IF(JU-JL.GT.1) THEN
        JM=(JU+JL)/2
        IF((V(N).GT.V(1)).EQV.(X.GT.V(JM)))THEN
          JL=JM
        ELSE
          JU=JM
        ENDIF
        GOTO 10
      ENDIF
      MVIDX=JL
      RETURN
      END
