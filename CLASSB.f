      SUBROUTINE CLASSB(THPOR,THLRET,THLMIN,BI,PSISAT,GRKSAT,
     1                  THLRAT,HCPS,TCS,THFC,PSIWLT,
     2                  DELZW,ZBOTW,ALGWET,ALGDRY,
     3                  SAND,CLAY,ORGM,DELZ,ZBOT,SDEPTH,
     4                  ISAND,IORG,NL,NM,IL,IM,IG)
C
C     * SEP 15/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3 IN 300 LOOP.
C     * APR 06/05 - D.VERSEGHY. MOVE CALCULATION OF GRKTLD
C     *                         INTO GRINFL; REVISED CALCULATION
C     *                         OF ALGDRY (WITH M.LAZARE).
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * SEP 04/03 - D.VERSEGHY. PERMEABLE THICKNESS OF SOIL
C     *                         LAYERS CONSTRAINED TO >= 1 MM;
C     *                         PROTECT SENSITIVE CALCULATIONS
C     *                         AGAINST ROUNDOFF ERROR.
C     * JUN 28/02 - D.VERSEGHY. ASSIGN SOIL HYDROLOGICAL AND
C     *                         THERMAL PROPERTIES BASED ON
C     *                         SAND, CLAY AND ORGANIC MATTER
C     *                         CONTENT.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER NL,NM,IL,IM,IG,I,J,M
C
C     * OUTPUT ARRAYS.
C
      REAL THPOR (NL,NM,IG),  THLRET(NL,NM,IG),  THLMIN(NL,NM,IG),
     1     BI    (NL,NM,IG),  PSISAT(NL,NM,IG),  GRKSAT(NL,NM,IG),  
     2     THLRAT(NL,NM,IG),  HCPS  (NL,NM,IG),  
     3     TCS   (NL,NM,IG),  THFC  (NL,NM,IG),  PSIWLT(NL,NM,IG),
     4     DELZW (NL,NM,IG),  ZBOTW (NL,NM,IG),
     4     ALGWET(NL,NM),     ALGDRY(NL,NM)
C
      INTEGER               ISAND (NL,NM,IG),  IORG  (NL,NM,IG)
C
C     * INPUT ARRAYS.
C
      REAL SAND  (NL,NM,IG),  CLAY  (NL,NM,IG),  ORGM  (NL,NM,IG),
     1     DELZ  (3),         ZBOT  (3),         SDEPTH(NL,NM) 
C
      REAL THPORG (3),      THRORG (3),      THMORG (3),
     1     BORG   (3),      PSISORG(3),      GRKSORG(3)
C
C     * WORK ARRAYS.
C
      REAL THSAND(NL,NM,IG),  THCLAY(NL,NM,IG),  THORG (NL,NM,IG)
C
C     * TEMPORARY VARIABLES.
C
      REAL VSAND,VORG,VCLAY,VTOT
C     * FOR NEW CALCULATION OF WILTING POINT
      REAL PSI_LIMIT
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,RHOSOL,RHOOM,
     1     HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,SPHICE,SPHVEG,
     2     SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP 
C
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /CLASS5/ THPORG,THRORG,THMORG,BORG,PSISORG,GRKSORG
C---------------------------------------------------------------------
C

c Ric's changes July 5, 2007 
c Define value of psi_limit as 13 athmospheres
              PSI_LIMIT=130.0

      DO 100 J=1,IG
      DO 100 M=1,IM
      DO 100 I=1,IL
          ISAND (I,M,J)=NINT(SAND(I,M,J))                                               
          IORG  (I,M,J)=NINT(ORGM(I,M,J))                                               
          IF(ISAND(I,M,J).EQ.-4) THEN
              THPOR (I,M,J)=0.0
              THLRET(I,M,J)=0.0
              THLMIN(I,M,J)=0.0
              BI    (I,M,J)=0.0
              PSISAT(I,M,J)=0.0
              GRKSAT(I,M,J)=0.0
              THLRAT(I,M,J)=0.0
              HCPS(I,M,J)=HCPICE
              TCS(I,M,J)=TCICE
          ELSEIF(ISAND(I,M,J).EQ.-3) THEN
              THPOR (I,M,J)=0.0
              THLRET(I,M,J)=0.0
              THLMIN(I,M,J)=0.0
              BI    (I,M,J)=0.0
              PSISAT(I,M,J)=0.0
              GRKSAT(I,M,J)=0.0
              THLRAT(I,M,J)=0.0
              HCPS(I,M,J)=HCPSND
              TCS(I,M,J)=TCSAND
          ELSEIF(ISAND(I,M,J).EQ.-2) THEN
              THPOR (I,M,J)=THPORG(IORG(I,M,J))
              THLRET(I,M,J)=THRORG(IORG(I,M,J))
              THLMIN(I,M,J)=THMORG(IORG(I,M,J))
              BI    (I,M,J)=BORG(IORG(I,M,J))
              PSISAT(I,M,J)=PSISORG(IORG(I,M,J))
              GRKSAT(I,M,J)=GRKSORG(IORG(I,M,J))
              THLRAT(I,M,J)=0.5**(1.0/(2.0*BI(I,M,J)+3.0))
              HCPS(I,M,J)=HCPOM
              TCS(I,M,J)=TCOM
          ELSEIF(SAND(I,M,J).GT.0) THEN
              THPOR (I,M,J)=(-0.126*SAND(I,M,J)+48.9)/100.0
c   original mesh 1.0.0c version
c              THLRET(I,M,J)=0.04
              THLMIN(I,M,J)=0.04
              BI    (I,M,J)=0.159*CLAY(I,M,J)+2.91
              PSISAT(I,M,J)=(10.0**(-0.0131*SAND(I,M,J)+1.88))/100.0
c Ric's changes July 5, 2007 
c Now define THLRET as wilting point
c 			THLRET(I,M,J)=THPOR(I,M,J)*(PSISAT(I,M,J)/PSI_LIMIT)
c     +		    **(1.0/BI(I,M,J))
c Frank's change September 4, 2007
c Make sure that THLRET has a minimum value of THLMIN
 			THLRET(I,M,J)=MAX(THPOR(I,M,J)*(PSISAT(I,M,J)/PSI_LIMIT)
     +		    **(1.0/BI(I,M,J)),THLMIN(I,M,J))
             GRKSAT(I,M,J)=(10.0**(0.0153*SAND(I,M,J)-0.884))*
     1            7.0556E-6
              THLRAT(I,M,J)=0.5**(1.0/(2.0*BI(I,M,J)+3.0))
              VSAND=SAND(I,M,J)/(RHOSOL*100.0)
              VORG=ORGM(I,M,J)/(RHOOM*100.0)
c   original mesh 1.0.0c version
c			VCLAY=(100.0-SAND(I,M,J)-ORGM(I,M,J))/(RHOSOL*100.0)
c Ric's changes July 5, 2007 
c Use value of clay percentage that is read in from class.ini file
              VCLAY=CLAY(I,M,J)/(RHOSOL*100.0)
              VTOT=VSAND+VCLAY+VORG
              THSAND(I,M,J)=(1.0-THPOR(I,M,J))*VSAND/VTOT
              THORG(I,M,J)=(1.0-THPOR(I,M,J))*VORG/VTOT
              THCLAY(I,M,J)=1.0-THPOR(I,M,J)-THSAND(I,M,J)-THORG(I,M,J)
              HCPS(I,M,J)=(HCPSND*THSAND(I,M,J)+HCPCLY*THCLAY(I,M,J)+
     1            HCPOM*THORG(I,M,J))/(1.0-THPOR(I,M,J))
              TCS(I,M,J)=((TCSAND**THSAND(I,M,J))*(TCOM**THORG(I,M,J))*
     1            (TCCLAY**THCLAY(I,M,J)))**(1.0/(1.0-THPOR(I,M,J)))
          ENDIF
          IF(THPOR(I,M,J).GT.0.0) THEN
              THFC(I,M,J)=EXP(LOG(1.157E-9/GRKSAT(I,M,J))/
     1            (2.0*BI(I,M,J)+3.0)+LOG(THPOR(I,M,J)))
c   original mesh 1.0.0c version
c              PSIWLT(I,M,J)=PSISAT(I,M,J)*(MIN(0.5*THFC(I,M,J),
c     1            THLMIN(I,M,J))/THPOR(I,M,J))**(-BI(I,M,J))
c Ric's changes July 5, 2007 
              PSIWLT(I,M,J)=MIN(PSISAT(I,M,J)*(MIN(0.5*THFC(I,M,J),
     1            THLMIN(I,M,J))/THPOR(I,M,J))**(-BI(I,M,J)),PSI_LIMIT)
          ELSE
              THFC(I,M,J)=0.0
              PSIWLT(I,M,J)=0.0
          ENDIF
100   CONTINUE
C
      DO 300 M=1,IM
      DO 300 I=1,IL
          DO 250 J=1,IG
              IF(ISAND(I,M,1).EQ.-4) THEN
                  DELZW(I,M,J)=DELZ(J)
              ELSEIF(SDEPTH(I,M).GE.ZBOT(J)) THEN
                  DELZW(I,M,J)=DELZ(J)
              ELSEIF(SDEPTH(I,M).LT.(ZBOT(J)-DELZ(J)+0.001)) THEN
                  DELZW(I,M,J)=0.0
                  ISAND(I,M,J)=-3
              ELSE
                  DELZW(I,M,J)=SDEPTH(I,M)-(ZBOT(J)-DELZ(J))
              ENDIF
              ZBOTW(I,M,J)=MAX(0.0,ZBOT(J)-DELZ(J))+DELZW(I,M,J)
250       CONTINUE
          IF(SAND(I,M,1).GE.0.0) THEN
              ALGWET(I,M)=0.08+0.0006*SAND(I,M,1)
              ALGDRY(I,M)=MIN(0.14+0.0056*SAND(I,M,1),0.45)
          ELSE
              ALGWET(I,M)=0.0
              ALGDRY(I,M)=0.0
          ENDIF
300   CONTINUE
C
      RETURN
      END
