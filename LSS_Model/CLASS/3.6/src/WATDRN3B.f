C
CDGP  2011-05-05: FOR WARDRN3 (RIC) (THIS SUB FORMERLY WATDRAIN_B)
CDGP  2011-04-21: FOR WATDRAIN (2.1)
C
C     *****************************************************************
C     SUB WATDRN3B
C     *****************************************************************
C     INITIALIZES WATDRN (RIC) VARIABLES WITH AN EXTERNAL CALL TO
C     MAPLE CODE
C     *****************************************************************
C     APR 26/11 - DGP      "qshort_text_v3.txt"
C     MAY 05/11 - DGP      "q15_bflow.3.txt"
C     MAY 26/11 - DGP      "q15.2.txt"
C     JUN 03/11 - DGP      "q15.4.txt", ADDED BQMAX, REPLACED BFTC 
C       WITH BTC TO FOLLOW RIC'S CODE
C     JUL 11/11 - DGP      DEFINITION CHANGED TO PASS RAW VALUES
C                          TO PREP FOR CODE CONVERSION
C                          (DDEN CONVERSION REMOVED).
C     AUG 29/11 - DGP      ADDED 'STATUS' ON SOIL_OUT FILE (UNIT=402).
C
      SUBROUTINE WATDRN3B (PSISAT,THPOR,GRKSAT,BI,XSLOPE,DD,
     1  NA,NTYPE,IGND,
     2  BTC,BCAP,DCOEFF,BFCAP,BFCOEFF,BFMIN,BQMAX,
     3  SAND,CLAY)
C
      USE FLAGS
C
      IMPLICIT NONE
C
      INTEGER :: IOS,IGND,NTYPE,NA,J,M
C
      REAL, DIMENSION(NTYPE,IGND) :: BTC,BCAP,DCOEFF,BFCAP,BFCOEFF,
     1  BFMIN,BQMAX
      REAL, DIMENSION(NA,NTYPE,IGND) :: PSISAT,THPOR,GRKSAT,BI,
     1  SAND,CLAY
      REAL, DIMENSION(NA,NTYPE) :: XSLOPE,DD
C
C     *****************************************************************
C     INITIALIZE AND EXIT IF WD3 /= 1
C
      DO 50 M=1,NTYPE
      DO 50 J=1,IGND
        BCAP(M,J)=0.0
        BTC(M,J)=0.0
        DCOEFF(M,J)=0.0
        BFCAP(M,J)=0.0
        BFMIN(M,J)=0.0
        BFCOEFF(M,J)=0.0
        BQMAX(M,J)=0.0
50    ENDDO
      IF (WD3 .NE. 1) RETURN
C
C     *****************************************************************
C     IF "soil_out.txt" EXISTS AND WD3NEWFILE=0, GOTO 416
C
      OPEN(UNIT=402,FILE="soil_out.txt",IOSTAT=IOS,STATUS='OLD')
      IF (WD3NEWFILE .EQ. 0 .AND. IOS .EQ. 0) THEN
        GOTO 416
      END IF
C
C     *****************************************************************
C     WRITE "soil_param.txt" FILE
C     *****************************************************************
C     VARIABLES: PSI0      =PSISAT (CLASS)
C                PHI       =THPOR
C                K         =GRKSAT
C                B         =BI
C                XXSLOPE   =XSLOPE
C                XXLENGTH  =1/(2*DD)
C     *****************************************************************
C
      OPEN(UNIT=401,FILE="soil_param.txt")
C
C     *****************************************************************
C     DO FOR EACH ELEMENT AND LAYER
C
      DO 100 M=1,NTYPE
      DO 100 J=1,IGND
        WRITE(UNIT=401,FMT="(2I7,8E15.6)") M,J,
*     1      XSLOPE(1,M),1/(2*DD(1,M)),PSISAT(1,M,J),THPOR(1,M,J),
     1      XSLOPE(1,M),DD(1,M),PSISAT(1,M,J),THPOR(1,M,J),
     2      GRKSAT(1,M,J),BI(1,M,J),
     3      SAND(1,M,J),CLAY(1,M,J)
100   ENDDO
      CLOSE(UNIT=401)
C
C     *****************************************************************
C     CALL "cqm.bat" (MAPLE) EXECUTABLE
C
CDGP  ORIGINAL CALL
*      CALL SYSTEM("cqm.bat",STATUS=IOS)
C     ADDED TEMP FIX FOR INTEL COMPILER
      IOS = 1
      IF (IOS .NE. 0) THEN
        WRITE(6,*)
        WRITE(6,*) "MESH could not initialize WD3."
        WRITE(6,*) "MESH has deactivated WD3."
        WD3=0
        WRITE(6,*)
        WRITE(6,*) "WATDRN is running."
        WRITE(6,*)
      END IF
C
C     *****************************************************************
C     READ "soilout.txt" FILE
C
416   DO
*DBUG FOR AN ERROR IN MAPLE FMT;
*        READ(UNIT=402,FMT="(I7,I8)",ADVANCE='NO',IOSTAT=IOS) M,J
        READ(UNIT=402,FMT="(2I7)",ADVANCE='NO',IOSTAT=IOS) M,J
        IF (IOS .NE. 0) EXIT
*FOR q15.4 AND LATER
        READ(UNIT=402,FMT="(7E15.6)") BCAP(M,J),BTC(M,J),DCOEFF(M,J),
     1      BFCAP(M,J),BFMIN(M,J),BFCOEFF(M,J),BQMAX(M,J)
*DBUG FOR AN ERROR IN MAPLE FMT (E16 VS E15);
*FOR q15.3 AND EARLIER;
*        READ(UNIT=402,FMT="(6E16.6)") BCAP(M,J),DCOEFF(M,J),
*     1      BFCAP(M,J),BTC(M,J),BFCOEFF(M,J),BFMIN(M,J)
      ENDDO
      CLOSE(UNIT=402)
C
C     *****************************************************************
C     RETURN
C
      RETURN
C
      END SUBROUTINE WATDRN3B
