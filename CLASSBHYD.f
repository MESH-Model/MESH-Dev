      SUBROUTINE CLASSBHYD(THPOR,THLRET,THLMIN,BI,PSISAT,GRKSAT,
     1                  THLRAT,HCPS,TCS,THFC,PSIWLT,
     2                  DELZW,ZBOTW,ALGWET,ALGDRY,
     3                  SAND,CLAY,ORGM,DELZ,ZBOT,SDEPTH,
     4                  ISAND,IORG,NL,NM,IL,IM,IG,wc_thpor,wc_thlret,
     5            wc_thlmin,wc_bi,wc_psisat,wc_grksat,wc_hcps,wc_tcs,
     6            wc_algwet,wc_algdry)
C
C     * DEC 11/07 - D.VERSEGHY. CHANGE CALCULATION OF TCS FROM
C     *                         GEOMETRIC MEAN TO LINEAR MEAN.
C     * FEB 07/07 - D.VERSEGHY. SET THFC TO THLRET FOR ORGANIC SOILS;
C     *                         STREAMLINE SOME CALCULATIONS.
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
     1     DELZ  (IG),        ZBOT  (IG),        SDEPTH(NL,NM) 
C
      REAL THPORG (3),      THRORG (3),      THMORG (3),
     1     BORG   (3),      PSISORG(3),      GRKSORG(3)
C
C     * INTERNAL WORK ARRAYS FOR CLASSBHYD.
C     * These are used if soil parameters are read in directly from new_soil.ini

      REAL wc_thpor (NL,NM,IG),wc_thlret(NL,NM,IG),
     1     wc_thlmin(NL,NM,IG),wc_bi    (NL,NM,IG),
     2     wc_psisat(NL,NM,IG),wc_grksat(NL,NM,IG),
     3     wc_hcps  (NL,NM,IG),wc_tcs   (NL,NM,IG),
     4     wc_algwet(NL,NM)   ,wc_algdry(NL,NM)
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
      PRINT*, "WARNING, ROUTINE CLASSBHYD.f IS NO LONGER USED"            !RIC SOULIS ADDED THIS
      PAUSE                                                               !RIC SOULIS ADDED THIS
      RETURN                                                              !RIC SOULIS ADDED THIS
      
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
              THFC(I,M,J)=0.0
              PSIWLT(I,M,J)=0.0
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
              THFC(I,M,J)=0.0
              PSIWLT(I,M,J)=0.0
          ELSEIF(ISAND(I,M,J).EQ.-2) THEN
              THPOR (I,M,J)=wc_thpor (i,m,j)
              THLRET(I,M,J)=wc_thlret(i,m,j)
              THLMIN(I,M,J)=wc_thlmin(i,m,j)
              BI    (I,M,J)=wc_bi    (i,m,j)
              PSISAT(I,M,J)=wc_psisat(i,m,j)
              GRKSAT(I,M,J)=wc_grksat(i,m,j)
              THLRAT(I,M,J)=0.5**(1.0/(2.0*BI(I,M,J)+3.0))
              HCPS(I,M,J)=HCPOM
              TCS(I,M,J)=TCOM
              THFC(I,M,J)=THLRET(I,M,J)
              PSIWLT(I,M,J)=PSISAT(I,M,J)*(THLMIN(I,M,J)/
     1            THPOR(I,M,J))**(-BI(I,M,J))
          ELSEIF(SAND(I,M,J).GE.0) THEN                                 !Ric Soulis changed gt to ge
              THPOR (I,M,J)=wc_thpor (i,m,j)
c              THLRET(I,M,J)=wc_thlret(i,m,j)                             !RIC SOULIS REMOVED
              THLMIN(I,M,J)=wc_thlmin(i,m,j)
              BI    (I,M,J)=wc_bi    (i,m,j)
              PSISAT(I,M,J)=wc_psisat(i,m,j)
              GRKSAT(I,M,J)=wc_grksat(i,m,j)
              THLRAT(I,M,J)=0.5**(1.0/(2.0*BI(I,M,J)+3.0))
              HCPS(I,M,J)=wc_hcps(i,m,j)
              TCS(I,M,J)=wc_tcs(i,m,j)
      THFC(I,M,J)=(THPOR(I,M,J)/(BI(I,M,J)-1))*                          !Ric Soulis added this
     1            (PSISAT(I,M,J)*BI(I,M,J)/DELZW(I,M,J))**(1/BI(I,M,J))* !Ric Soulis added this
     2            ((3*BI(I,M,J)+2)**((BI(I,M,J)-1)/BI(I,M,J))-           !Ric Soulis added this
     3            (2*BI(I,M,J)+2)**((BI(I,M,J)-1)/BI(I,M,J)))            !Ric Soulis added this
              PSIWLT(I,M,J)=PSISAT(I,M,J)*(MAX(0.5*THFC(I,M,J),
     1            THLMIN(I,M,J))/THPOR(I,M,J))**(-BI(I,M,J))
      THLRET(I,M,J)=THFC(I,M,J)                                           !RIC SOULIS ADDED THIS
          ENDIF
100   CONTINUE                                                          
C                                                                        
      DO 300 M=1,IM                                                     
      DO 300 I=1,IL                                                     
          DO 250 J=1,IG                                                 
              IF(ISAND(I,M,1).EQ.-4) THEN                               
                  DELZW(I,M,J)=DELZ(J)                                  
                  !CHANGED THIS TO BE CONSISTENT WITH CLASSB.f            !RIC SOULIS ADDED THIS
                  ISAND(I,M,J)=-4                                         !RIC SOULIS ADDED THIS
              ELSEIF(SDEPTH(I,M).GE.ZBOT(J)) THEN                       
                  DELZW(I,M,J)=DELZ(J)                                  
              ELSEIF(SDEPTH(I,M).LT.(ZBOT(J)-DELZ(J)+0.01)) THEN        
                 DELZW(I,M,J)=0.0                                       
                  ISAND(I,M,J)=-3                                       
              ELSE                                                      
                  DELZW(I,M,J)=SDEPTH(I,M)-(ZBOT(J)-DELZ(J))            
              ENDIF                                                     
              ZBOTW(I,M,J)=MAX(0.0,ZBOT(J)-DELZ(J))+DELZW(I,M,J)        
250       CONTINUE                                                      
          IF(SAND(I,M,1).GE.0.0) THEN                                   
              ALGWET(I,M)=wc_algwet(i,m)                                
              ALGDRY(I,M)=wc_algdry(i,m)                                
          ELSE                                                          
              ALGWET(I,M)=0.0                                           
              ALGDRY(I,M)=0.0                                           
          ENDIF                                                         
300   CONTINUE
C
      RETURN
      END
