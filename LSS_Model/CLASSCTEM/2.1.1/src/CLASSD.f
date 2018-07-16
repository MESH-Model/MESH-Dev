C>\file
C>
C!Purpose: Assign values to parameters in CLASS common blocks. CLASS 
C!incorporates several kinds of parameters in its common blocks. 
C!Some are defined specifically for use in the CLASS code; some are 
C!also shared with the atmospheric model (if running in coupled 
C!mode).
C!
      !!
      !!In subroutine CLASSD, for consistency throughout the code, some 
      !!parameters in the CLASS common blocks are set equal to their 
      !!corresponding values in the common blocks PARAMS, PARAM1, PARAM3 
      !!and TIMES from the CGCM. Also, some parameters in the common 
      !!block PHYCON which is used in the RPN subroutines DIASURFZ and 
      !!FLXSURFZ are set equal to their values defined in the CGCM or in 
      !!the block data routine CLASSBD. The parameters in question are as 
      !!follows:
      !!
      !!\f[
      !!\begin{tabular} { | l | l | l | c | }
      !!\hline
      !! CLASS/RPN name & GCM name & Definition                    & Units              \\ \hline
      !! DELT           & DELTIM   & Time step                     & s                  \\ \hline
      !! TFREZ          & CELZRO   & Freezing point of water       & K                  \\ \hline
      !! RGAS           & GAS      & Gas constant                  & $J kg^{-1} K^{-1}$ \\ \hline
      !! RGASV          & GASV     & Gas constant for water vapour & $J kg^{-1} K^{-1}$ \\ \hline
      !! GRAV           & G        & Acceleration due to gravity   & $m s^{-1}        $ \\ \hline
      !! CGRAV          & G        & Acceleration due to gravity   & $m s^{-1}        $ \\ \hline
      !! SBC            & SIGMA    & Stefan-Boltzmann constant     & $W m^{-2} K^{-4} $ \\ \hline
      !! CKARM          & VKC      & Von Karman constant           & -                  \\ \hline
      !! SPHAIR         & CPRES    & Specific heat of air          & $J kg^{-1} K^{-1}$ \\ \hline
      !! CPD            & CPRES    & Specific heat of air          & $J kg^{-1} K^{-1}$ \\ \hline
      !! PI             & CPI      & Pi                            & -                  \\ \hline
      !!\end{tabular}
      !!\f]
      SUBROUTINE CLASSD

C
C     * MAR 13/09 - D.VERSEGHY. REPLACE SURFCON COMMON BLOCK WITH
C     *                         CLASSD2.
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
      COMMON /EPS/    A,B,EPS1,EPS2
      COMMON /HTCP/   T1S,T2S,AI,BI,AW,BW,SLP

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
      COMMON /CLASSD2/ AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX
      COMMON /ESTWI/  RW1,RW2,RW3,RI1,RI2,RI3
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
      AI=2.88053E+6/1004.5
      BI=0.167E+3/1004.5
      AW=3.15213E+6/1004.5
      BW=2.38E+3/1004.5
      SLP=1.0/(T1S-T2S)
C==================================================================
      RETURN
      END
