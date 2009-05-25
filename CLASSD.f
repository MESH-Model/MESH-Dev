      SUBROUTINE CLASSD
C
C     * SEP 09/04 - D.VERSEGHY. HARMONIZE NAMING OF GCM COMMON
C     *                         BLOCK VARIABLES WITH REST OF CODE.
C     * JUL 18/03 - D.VERSEGHY. SPLIT OFF DATA STATEMENTS INTO
C     *                         NEW "CLASSBD" ROUTINE TO COMPLY
C     *                         WITH FORTRAN STANDARD.
C     * AUG 06/02 - D.VERSEGHY. DEFINE PHYSICAL CONSTANTS PASSED
C     *                         THROUGH CLASS COMMON BLOCKS.  
C     *                         (BASED ON ROUTINE "HYDCON".)
C
C     * THE FOLLOWING COMMON BLOCK PARAMETERS ARE DEFINED WITHIN
C     * THE ATMOSPHERIC MODEL.
C
      COMMON /PARAMS/ X1,    X2,    X3,    X4,   G,GAS,   X5,
     1                X6,    CPRES, GASV,  X7
      COMMON /PARAM1/ CPI,   X8,    CELZRO,X9,    X10,    X11
      COMMON /PARAM3/ X12,   X13,   X14,   X15,   SIGMA,  X16
      COMMON  /TIMES/ DELTIM,K1,    K2,    K3,    K4,     K5,
     1                K6,    K7,    K8,    K9,    K10,    K11

C     * ADDITIONAL COMMON BLOCKS ARE DEFINED SPECIFICALLY FOR USE 
C     * IN CLASS.

      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /CLASS5/ THPORG(3),THRORG(3),THMORG(3),BORG(3),
     1                PSISORG(3),GRKSORG(3)
      COMMON /CLASS6/ PI,GROWYR(18,4,2),ZOLNG,ZOLNS,ZOLNI,ZORAT(4),
     1                ZORATG
      COMMON /CLASS7/ CANEXT(4),XLEAF(4)
      COMMON /CLASS8/ ALVSI,ALIRI,ALVSO,ALIRO,ALBRCK
      COMMON /PHYCON/ DELTA,CGRAV,CKARM,CPD
      COMMON /SURFCON/ AS,ASX,CI,BS,BETA,FACTN,HMIN
C
C==================================================================
C     * RE-DEFINE CONSTANTS FOR STORAGE IN CLASS COMMON BLOCKS.
C
      DELT=DELTIM
      TFREZ=CELZRO
      RGAS=GAS
      RGASV=GASV
      GRAV=G
      CGRAV=G
      SBC=SIGMA
      CKARM=VKC
      SPHAIR=CPRES
      CPD=CPRES
      PI=CPI
C==================================================================
      RETURN
      END