!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer,
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer
!version 3 or (at your option) any later version that should be found at:
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software;
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec),
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
      PROGRAM RUNLAKE
C
C=======================================================================
C     * THE CANADIAN SMALL LAKE MODEL STANDALONE DRIVER
C     *
C     * AUTHOR: MURRAY MACKAY 		JAN 2014
C     *
C=======================================================================
      IMPLICIT NONE
C
      INTEGER,PARAMETER :: NLAT=1,NMOS=3,ILG=NLAT*NMOS
      INTEGER,PARAMETER :: NLAKMAX=200                              
      INTEGER,PARAMETER :: NBS=4,IGL=1,IRSTRT=0
      INTEGER MIDROT (NLAT,NMOS)
      INTEGER N,NLTEST,NMTEST,I,J,K,L,M,JL1,JL2,
     1        NCOUNT,NDAY,IHOUR,IMIN,IDAY,IYEAR,NML,NMW,
     2        IDISP,IZREF,ISLFD,IPCP,ITG,IALS,ISNOALB
C
C-- GATHER-SCATTER INDEX ARRAYS
      INTEGER,DIMENSION(ILG) :: ILMOS,JLMOS,IWMOS,JWMOS
C
C-- LAKE TILE PARAMETERS                                            
      INTEGER NLAKGAT(ILG), NLAKROT(NLAT,NMOS)
      REAL,   DIMENSION(NLAT,NMOS) :: HLAKROT, LLAKROT, BLAKROT,    
     1           HFSLROT, HEVLROT, FSGLROT, FLGLROT, HMFLROT,
     2           ASVDROT, ASIDROT,REFROT,BCSNROT
      REAL,   DIMENSION(ILG) :: HLAKGAT, LLAKGAT, BLAKGAT              
      REAL,   DIMENSION(ILG) :: ASVLGAT, ASILGAT,BCSNLGAT,REFLGAT,
     1           ZDMLGAT,ZDHLGAT
C-- LAKE PROGNOSTIC VARIABLES                                       
      REAL,   DIMENSION(NLAT,NMOS,NLAKMAX) :: TLAKROT               
      REAL,   DIMENSION(ILG,NLAKMAX) :: TLAKGAT                     
      REAL,   DIMENSION(ILG) :: EXPW,DTEMP,HDPTH,DELU,GRED,         
     1                    TKELAK,T0LAK,LKICEH,RHOMIX,TSED,SNICEH,ROFICEH
C-- LAKE DIAGNOSTIC VARIABLES                                       
      REAL,   DIMENSION(ILG) :: HFSLGAT, HEVLGAT, FSGLGAT, FLGLGAT, 
     1                       HMFLGAT,HTCLGAT,FICEGAT,FLSGAT,G0SLGAT,
     2                       FSGSLGAT,FLGSLGAT,HFSSLGAT,HEVSLGAT,
     3                       HMFNLGAT,HTCSLGAT
      REAL,   DIMENSION(ILG) :: PCPLGAT,PCPNLGAT,QFLGAT,QFNLGAT,
     1                       ROFNLGAT,SFTLGAT,SFULGAT,SFVLGAT,SFQLGAT,
     2                       SFHLGAT,QLWOLGAT,ALVLGAT,ALILGAT,EFLGAT,
     3                       GTLGAT,QGLGAT,DRLGAT,PETLGAT,QSENLGAT,
     4                       TFXLGAT,QEVPLGAT,QFSLGAT,QFXLGAT,SNOLGAT,
     5                       RHOSLGAT,TSNOLGAT,ALBSLGAT,WSNOLGAT
      REAL,DIMENSION(NLAT,NMOS) :: PCPLROT,PCPNLROT,QFLROT,QFNLROT,
     1                       ROFNLROT,FSGSLROT,FLGSLROT,HFSSLROT,
     2                       HEVSLROT,HMFNLROT,HTCSLROT,HTCLROT,
     3                       SFTLROT,SFULROT,SFVLROT,SFQLROT,SFHLROT,
     4                       QLWOLROT,ALVLROT,ALILROT,EFLROT,GTLROT,
     5                       QGLROT,DRLROT,PETLROT,QSENLROT,TFXLROT,
     6                       QEVPLROT,QFSLROT,QFXLROT,SNOLROT,RHOSLROT,
     7                       TSNOLROT,ALBSLROT,WSNOLROT
CC    INTEGER, PARAMETER :: DP=SELECTED_REAL_KIND(15,307)
      INTEGER, PARAMETER :: SP=kind(1.0)
      INTEGER, PARAMETER :: DP=SELECTED_REAL_KIND(2*precision(1.0_sp))
      REAL(dp),   DIMENSION(ILG) :: CTLSTP 
C
      REAL,DIMENSION(ILG,NBS) :: 
     1        FSDBLGAT,   FSFBLGAT,   FSSBLGAT
      REAL,DIMENSION(NLAT,NBS) ::
     1        FSDBROL,   FSFBROL,   FSSBROL
C
C-- ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES
C
      REAL,DIMENSION(NLAT,NMOS) :: FAREROT
      REAL,DIMENSION(NLAT) ::
     1      ZRFMROW,   ZRFHROW,   ZDMROW ,   ZDHROW ,  
     2      ZBLDROW,   FSVHROW,   FSIHROW,   RADJROW,
     3      CSZROW ,   FDLROW ,   ULROW  ,   VLROW  ,   
     4      TAROW  ,   QAROW  ,   PRESROW,   PREROW ,  
     5      PADRROW,   VPDROW ,   TADPROW,   RHOAROW,  
     6      UVROW  ,   GCROW  ,   RPCPROW,   TRPCROW,
     7      SPCPROW,   TSPCROW,   RHSIROW,   RPREROW,
     8      SPREROW   
C
      REAL,DIMENSION(ILG) ::
     1      ZRFMGAT,   ZRFHGAT,   ZDMGAT ,   ZDHGAT ,  
     2      ZBLDGAT,   FSVHGAT,   FSIHGAT,   RADJGAT,
     3      CSZGAT ,   FDLGAT ,   ULGAT  ,   VLGAT  ,   
     4      TAGAT  ,   QAGAT  ,   PRESGAT,   PREGAT ,  
     5      PADRGAT,   VPDGAT ,   TADPGAT,   RHOAGAT,
     6      RPCPLGAT,  TRPCLGAT,  SPCPLGAT,  TSPCLGAT,
     7      RHSILGAT,  RADJLGAT,  PADRLGAT
C
C-- LAND SURFACE DIAGNOSTIC VARIABLES
C
      REAL,DIMENSION(NLAT,NMOS) :: CDHROT ,   CDMROT 
      REAL,DIMENSION(ILG) ::       CDHGAT ,   CDMGAT 
C
C-- ARRAYS USED FOR OUTPUT AND DISPLAY PURPOSES.
C
      CHARACTER     TITLE1*4,     TITLE2*4,     TITLE3*4,
     1              TITLE4*4,     TITLE5*4,     TITLE6*4
      CHARACTER     NAME1*4,      NAME2*4,      NAME3*4,
     1              NAME4*4,      NAME5*4,      NAME6*4
      CHARACTER     PLACE1*4,     PLACE2*4,     PLACE3*4,
     1              PLACE4*4,     PLACE5*4,     PLACE6*4
C 
C-- LOCAL CONSTANTS AND VARIABLES.
C
      REAL DEGLAT,DEGLON,FSDOWN,DAY,DECL,HOUR,COSZ,
     1     EA,CA,CB,EASAT,CONST,HCAP,ZTOP,ZBOT,Z,QSUML,
     2     RHOIW,ICETOP,ICEBOT
C
C-- COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ, CLHVAP,PI,
     1     RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,TCW,TCICE,TCSAND,TCCLAY,
     2     TCOM,TCDRYS,RHOSOL,RHOOM,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,
     3     HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,
     4     CKARM,CGRAV,CPD,DELTA,AS,ASX,ANGMAX,BS,BETA,CI,FACTN,HMIN
C
C    * LAKE TILE COMMON BLOCK PARAMETERS                            
C    *                                                              
      REAL TKECN,TKECF,TKECE,TKECS,HDPTHMIN, DUMAX, 
     1  TKEMIN,DELMAX,DELMIN,EMSW,DELZLK,DELSKIN,DHMAX,TKECL
C=======================================================================
C     * PHYSICAL CONSTANTS.
C     * THE FOLLOWING COMMON BLOCKS ARE DEFINED SPECIFICALLY FOR USE 
C     * IN CLASS, AND ARE RETAINED HERE TO MAINTAIN COMPATIBILITY
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /PHYCON/ DELTA,CGRAV,CKARM,CPD
C
C    * LAKE TILE COMMON BLOCK                                       
C    *                                                              
      COMMON /LAKECON/ TKECN,TKECF,TKECE,TKECS,HDPTHMIN,            
     2             TKEMIN,DELMAX,DELMIN,EMSW,DELZLK,DELSKIN,DHMAX,  
     3             TKECL,DUMAX                                      
C
C     * ADDITIONAL VALUES FOR RPN AND GCM COMMON BLOCKS.
C
      COMMON /CLASSD2/ AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX              
C
      DATA      DELTA,      AS,         ASX,        ANGMAX
     1       /  0.608,      12.0,       4.7,        0.85   /

      DATA      CI,         BS,         BETA,       FACTN,      HMIN 
     1       /  40.0,       1.0,        1.0,        1.2,        40.   /
C
C    * PARAMETERS ORIGINALLY FROM CLASS COMMON BLOCK DATA           
C    *                                                              
      DATA      VKC,        CT,         VMIN,     EMSW
     1       /  0.40,       1.15E-3,    0.1,      0.97     /
      DATA      TCW,        TCICE,      TCSAND,    TCCLAY,  TCOM
     1       /  0.57,       2.24,       2.5,       2.5,     0.25  /
      DATA      TCDRYS,     RHOSOL,     RHOOM
     1       /  0.275,      2.65E3,     1.30E3  /    
      DATA      HCPW,       HCPICE,     HCPSOL,    HCPOM
     1       /  4.187E6,    1.9257E6,   2.25E6,    2.50E6 /
      DATA      HCPSND,     HCPCLY,     SPHW,      SPHICE,  SPHVEG
     1       /  2.13E6,     2.38E6,     4.186E3,   2.10E3,  2.70E3 /
      DATA      RHOW,       RHOICE,     TCGLAC,    CLHMLT,  CLHVAP
     1       /  1.0E3,      0.917E3,    2.24,      0.334E6, 2.501E6/
C
C--Lake TKE process efficiencies                                    
      DATA  TKECN,      TKECF,      TKECE,      TKECS,      TKECL   
     1/     1.33,       0.25,       1.15,       0.20,      0.2350/ 
CToo?1/     1.33,       0.25,       1.15,       0.20,      0.2250/
CELA 1/     1.33,       0.25,       1.15,       0.20,      0.2350/
C
C--Lake process parameter limits, and grid spacing                  
      DATA  HDPTHMIN, TKEMIN,  DELMAX,  DELMIN,  DELZLK,  DELSKIN   
     1/     0.5,      1.0E-12, 5.0,     0.5,     0.5,     0.050 /   
      DATA  DHMAX,    DUMAX                                         
     1/     2.0,      0.1   /                                       
C
C     * ASSIGN VALUES NORMALLY SPECIFIED WITHIN THE GCM.
C
      DATA      PI   /  3.1415926535898    /
      DATA      GRAV,       RGAS,       SPHAIR,    RGASV
     1/         9.80616,    287.04,     1.00464E3,  461.50   /
      DATA      TFREZ,      SBC
     1/         273.16,     5.66796E-8  /
      DATA      DELT  
     1/         600.0   /       ! 10 minutes (eg. Raksjon, ELA)
C    1/         300.0   /       ! 5 minutes (eg. Toolik)
C    1/         1800.0  /       ! half hrly
      CGRAV=GRAV
      CKARM=VKC
      CPD=SPHAIR
C
C=======================================================================
C     * CLASS SWITCHES select different runtime options
C
C     * IF IDISP=0, VEGETATION DISPLACEMENT HEIGHTS ARE IGNORED,
C     * BECAUSE THE ATMOSPHERIC MODEL CONSIDERS THESE TO BE PART
C     * OF THE "TERRAIN".
C     * IF IDISP=1, VEGETATION DISPLACEMENT HEIGHTS ARE CALCULATED.
C
C     * IF IZREF=1, THE BOTTOM OF THE ATMOSPHERIC MODEL IS TAKEN
C     * TO LIE AT THE GROUND SURFACE.
C     * IF IZREF=2, THE BOTTOM OF THE ATMOSPHERIC MODEL IS TAKEN
C     * TO LIE AT THE LOCAL ROUGHNESS HEIGHT.
C
C     * IF ISLFD=0, DRCOEF IS CALLED FOR SURFACE STABILITY CORRECTIONS
C     * AND THE ORIGINAL GCM SET OF SCREEN-LEVEL DIAGNOSTIC CALCULATIONS 
C     * IS DONE.
C     * IF ISLFD=1, DRCOEF IS CALLED FOR SURFACE STABILITY CORRECTIONS
C     * AND SLDIAG IS CALLED FOR SCREEN-LEVEL DIAGNOSTIC CALCULATIONS. 
C     * IF ISLFD=2, FLXSURFZ IS CALLED FOR SURFACE STABILITY CORRECTIONS
C     * AND DIASURF IS CALLED FOR SCREEN-LEVEL DIAGNOSTIC CALCULATIONS. 
C
C     * IF IPCP=1, THE RAINFALL-SNOWFALL CUTOFF IS TAKEN TO LIE AT 0 C.
C     * IF IPCP=2, A LINEAR PARTITIONING OF PRECIPITATION BETWEEEN 
C     * RAINFALL AND SNOWFALL IS DONE BETWEEN 0 C AND 2 C.
C     * IF IPCP=3, RAINFALL AND SNOWFALL ARE PARTITIONED ACCORDING TO
C     * A POLYNOMIAL CURVE BETWEEN 0 C AND 6 C.
C     * IF IPCP=4, THE RAINFALL, SNOWFALL AND TOTAL PRECIPITATION RATES
C     * ARE READ IN DIRECTLY.
C
C     * ITC, ITCG AND ITG ARE SWITCHES TO CHOOSE THE ITERATION SCHEME TO
C     * BE USED IN CALCULATING THE CANOPY OR GROUND SURFACE TEMPERATURE
C     * RESPECTIVELY.  IF THE SWITCH IS SET TO 1, A BISECTION METHOD IS
C     * USED; IF TO 2, THE NEWTON-RAPHSON METHOD IS USED.
C     
C     * IF IPAI, IHGT, IALC, IALS AND IALG ARE ZERO, THE VALUES OF 
C     * PLANT AREA INDEX, VEGETATION HEIGHT, CANOPY ALBEDO, SNOW ALBEDO
C     * AND SOIL ALBEDO RESPECTIVELY CALCULATED BY CLASS ARE USED.
C     * IF ANY OF THESE SWITCHES IS SET TO 1, THE VALUE OF THE
C     * CORRESPONDING PARAMETER CALCULATED BY CLASS IS OVERRIDDEN BY
C     * A USER-SUPPLIED INPUT VALUE.
C
      IDISP=1             !1 for stanalone with field obs.  Not used for lake model
      IZREF=1             !1 for standalone with field obs.  Used in TLSPREP 
      ISLFD=0
      IPCP=1
      ITG=1               !std
      IALS=0
      ISNOALB=0
C=======================================================================
C     * OPEN FILES FOR READING AND WRITING.

      OPEN(UNIT=50,FILE='LAKE.ini',STATUS='OLD')
      OPEN(UNIT=51,FILE='LAKE.met',STATUS='OLD')
      OPEN(UNIT=71,FILE='LAKE.of1')                                 
      OPEN(UNIT=72,FILE='LAKE.of2')                                 
      OPEN(UNIT=73,FILE='LAKE.of3')                                 
      OPEN(UNIT=74,FILE='LAKE.of4')                                 
      OPEN(UNIT=75,FILE='LAKE.of5')                                 
      OPEN(UNIT=76,FILE='LAKE.of6')                                 
      OPEN(UNIT=77,FILE='LAKE.of7')                                 
      OPEN(UNIT=79,FILE='LAKE.of9')                                 

C     * READ AND PROCESS INITIALIZATION AND BACKGROUND INFORMATION.
C     * FIRST, MODEL RUN SPECIFICATIONS.

      READ (50,5010) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      READ (50,5010) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      READ (50,5010) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
C                                                                   
C   * LAKE DIAGNOSTIC OUTPUT FILES                                  
C                                                                   
      WRITE(71,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6      
      WRITE(71,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6            
      WRITE(72,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6      
      WRITE(72,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6            
      WRITE(73,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6      
      WRITE(73,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6            
      WRITE(74,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6      
      WRITE(74,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6            
      WRITE(75,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6      
      WRITE(75,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6            
      WRITE(76,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6      
      WRITE(76,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6            
      WRITE(77,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6      
      WRITE(77,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6            
      WRITE(79,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6      
      WRITE(79,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6            
C
      WRITE(71,7011)                                                
7011  FORMAT('YEAR DAY  HR MIN     E0     F0      Q*      Q0',      
     1       '      L*     Hs      He     T0      Lake Ice')        
      WRITE(72,7012)                                                
7012  FORMAT('YEAR DAY  HR MIN     TEMP ')                          
      WRITE(73,7013)                                                
7013  FORMAT('YEAR DAY  HR MIN     FQU      BFLX      DISS',        
     1 '    TKE DELU HDPTH JMIX ZMIX  TMIX    DTEMP',               
     2 '    FSHEAR FENTRA')                                         
      WRITE(74,7014)                                                
7014  FORMAT('YEAR DAY  HR MIN     U*     GRED      WEDB      ',    
     1       '  HDPTH DELTHRM DELU')                                
      WRITE(75,7015)                                                
7015  FORMAT('YEAR DAY  HR MIN     TSNOW   WSNOW    RHOSNO',
     1       '   ZSNOW')
      WRITE(76,7016)                                                
7016  FORMAT('YEAR DAY  HR MIN     QSWNS   QTRANSL  QLNET ',
     1       '   QSENSS  QEVAPS   QMELT   GZEROSL')
      WRITE(77,7017)                                                
7017  FORMAT('YEAR DAY  HR MIN     QSWIN   FSGL    QFLX(1m)')
      WRITE(79,7019)                                                
7019  FORMAT('YEAR DAY  HR MIN     EnergyBalance')
C                                                                   
C=======================================================================
C   * READ GEOPHYSICAL DATA
C
      READ(50,5020) DEGLAT,DEGLON,ZRFMROW(1),ZRFHROW(1),ZBLDROW(1),
     1              GCROW(1),NLTEST,NMTEST

      DO 50 I=1,NLTEST
      DO 50 M=1,NMTEST
          READ(50,5010) TITLE1                                      
          READ(50,5045) MIDROT(I,M),FAREROT(I,M)                    
          IF (MIDROT(I,M) .GT. 0.0 ) THEN                           
            CALL XIT('RUNLAKE',-1)
      ELSE
C-- READ IN WATER TILE INFORMATION                              
           READ(50,5095) HLAKROT(I,M),LLAKROT(I,M),BLAKROT(I,M)
           NLAKROT(I,M)=NINT(HLAKROT(I,M)/DELZLK)
           READ(50,5096) (TLAKROT(I,M,J),J=1,NLAKROT(I,M))          
      ENDIF
50    CONTINUE

      DO 100 I=1,NLTEST
      DO 100 M=1,NMTEST
          IF (NLAKROT(I,M) .GE. 1) THEN                             
          DO 73 J=1,NLAKROT(I,M)                                    
            IF (TLAKROT(I,M,J).LE.200.0) THEN                       
              TLAKROT(I,M,J)=TLAKROT(I,M,J)+TFREZ                   
            ENDIF                                                   
73        CONTINUE                                                  
          ENDIF                                                     
C-- INITIALIZE LAKE SNOW CONDITIONS
          SNOLROT(I,M)=0.0
          ALBSLROT(I,M)=0.0
          RHOSLROT(I,M)=0.0
          TSNOLROT(I,M)=0.0
          WSNOLROT(I,M)=0.0
100   CONTINUE
C
C=======================================================================
5010  FORMAT(2X,6A4)
5020  FORMAT(5F10.2,F7.1,3I5)
5045  FORMAT(I8,F8.3)                                               
5095  FORMAT(3F10.1)                                      
5096  FORMAT(200F6.2)                                               
5300  FORMAT(1X,I2,I3,I5,I6,2F9.2,E14.4,F9.2,E12.3,F8.2,F12.2,3F9.2,
     1       F9.4)
6001  FORMAT('CLASS TEST RUN:     ',6A4)
6002  FORMAT('RESEARCHER:         ',6A4)
6003  FORMAT('INSTITUTION:        ',6A4)
C
C------------------------------------------------------------------
C THE CLASS GATHER-SCATTER MACHINERY IS RETAINED HERE FOR
C COMPATIBILITY
C------------------------------------------------------------------
      NML=0
      NMW=0
      DO 110 I=1,NLTEST
              DO 120 J=1,NMTEST
                  IF(FAREROT(I,J).GT.0.0)      THEN
                      IF(MIDROT(I,J).GT.0)    THEN
                          NML=NML+1
                          ILMOS(NML)=I
                          JLMOS(NML)=J
                      ELSE
                          NMW=NMW+1
                          IWMOS(NMW)=I
                          JWMOS(NMW)=J
                      ENDIF
                  ENDIF
  120         CONTINUE
  110 CONTINUE
      print*, "NML=",NML, NMW
C=======================================================================
C     * LAUNCH RUN.

      N=0
      NCOUNT=1
      NDAY=86400/NINT(DELT)

200   CONTINUE
C
C========================================================================
C     * READ IN METEOROLOGICAL FORCING DATA FOR CURRENT TIME STEP;
C     * CALCULATE SOLAR ZENITH ANGLE AND COMPONENTS OF INCOMING SHORT-
C     * WAVE RADIATION FLUX; ESTIMATE FLUX PARTITIONS IF NECESSARY.
C
      N=N+1
      DO 250 I=1,NLTEST
          READ(51,5300,END=999) IHOUR,IMIN,IDAY,IYEAR,FSDOWN,FDLROW(I),
     1         PREROW(I),TAROW(I),QAROW(I),UVROW(I),PRESROW(I)
Cmdm 
Cmdm      PREROW(I)=0.0                   !test
Cmdm      PREROW(I)=0.3*PREROW(I)         !test: scale precip
Cmdm      PREROW(I)=PREROW(I)/600.0       !correction for Eagle lake data
Cmdm      FDLROW(I)=0.95*FDLROW(I)    !test
Cmdm      FDLROW(I)=1.05*FDLROW(I)    !test
C
          FSVHROW(I)=0.5*FSDOWN
          FSIHROW(I)=0.5*FSDOWN
          TAROW(I)=TAROW(I)+TFREZ
          ULROW(I)=UVROW(I)
          VLROW(I)=0.0
          UVROW(I)=MAX(VMIN,UVROW(I))
          ZDMROW(I)=10.0
          ZDHROW(I)=2.0
          FSSBROL(I,1)=FSVHROW(I)
          FSSBROL(I,2)=FSIHROW(I)
          RPREROW(I)=0.0           !only needed for IPC=4
          SPREROW(I)=0.0           !only needed for IPC=4
250   CONTINUE
C
      DAY=REAL(IDAY)+(REAL(IHOUR)+REAL(IMIN)/60.)/24.
      DECL=SIN(2.*PI*(284.+DAY)/365.)*23.45*PI/180.
      HOUR=(REAL(IHOUR)+REAL(IMIN)/60.)*PI/12.-PI
      DO 300 I=1,NLTEST
          RADJROW(I)=DEGLAT*PI/180.
          COSZ=SIN(RADJROW(I))*SIN(DECL)+COS(RADJROW(I))*
     >                     COS(DECL)*COS(HOUR)
          CSZROW(I)=SIGN(MAX(ABS(COSZ),1.0E-3),COSZ)
300   CONTINUE
C================================================================
C
C     * CALCULATION OF ATMOSPHERIC INPUT VARIABLES.
C
      CALL CLASSI(VPDROW,TADPROW,PADRROW,RHOAROW,RHSIROW,
     1            RPCPROW,TRPCROW,SPCPROW,TSPCROW,TAROW,QAROW,
     2            PREROW,RPREROW,SPREROW,PRESROW,
     3            IPCP,NLAT,1,NLTEST)
C
C======================================================================
C GATHER CODE FROM CLASSG
C-- GATHER LAKE RELEVANT VARIABLES INTO WATER-TILE GAT ARRAYS                                       
C                                                                   
      DO 700 K=1,NMW                                                
          HLAKGAT(K+NML)=HLAKROT(IWMOS(K),JWMOS(K))                 
          LLAKGAT(K+NML)=LLAKROT(IWMOS(K),JWMOS(K))                 
          BLAKGAT(K+NML)=BLAKROT(IWMOS(K),JWMOS(K))                 
          NLAKGAT(K+NML)=NLAKROT(IWMOS(K),JWMOS(K))                 
          DO 705 L=1,NLAKGAT(K+NML)                                 
            TLAKGAT(K+NML,L)=TLAKROT(IWMOS(K),JWMOS(K),L)           
705       CONTINUE                                                  
          ASVLGAT(K+NML)=ASVDROT(IWMOS(K),JWMOS(K))
          ASILGAT(K+NML)=ASIDROT(IWMOS(K),JWMOS(K))
          BCSNLGAT(K+NML)=BCSNROT(IWMOS(K),JWMOS(K))
          REFLGAT(K+NML)=REFROT(IWMOS(K),JWMOS(K))
          SNOLGAT(K+NML)=SNOLROT(IWMOS(K),JWMOS(K))
          RHOSLGAT(K+NML)=RHOSLROT(IWMOS(K),JWMOS(K))
          TSNOLGAT(K+NML)=TSNOLROT(IWMOS(K),JWMOS(K))
          ALBSLGAT(K+NML)=ALBSLROT(IWMOS(K),JWMOS(K))
          WSNOLGAT(K+NML)=WSNOLROT(IWMOS(K),JWMOS(K))
700   CONTINUE                                                      
C
C-- ATMOSPHERIC FORCING VARIABLES NEEDED FOR LAKE TILES         
C   GATHERED ON TOP OF LAND TILES                               
C
      DO 800 K=1,NMW                                                
          FSVHGAT(K+NML)=FSVHROW(IWMOS(K))                          
          FSIHGAT(K+NML)=FSIHROW(IWMOS(K))                          
          CSZGAT (K+NML)=CSZROW (IWMOS(K))                          
          FDLGAT (K+NML)=FDLROW (IWMOS(K))                          
          ULGAT  (K+NML)=ULROW  (IWMOS(K))                          
          VLGAT  (K+NML)=VLROW  (IWMOS(K))                           
          TAGAT  (K+NML)=TAROW  (IWMOS(K))                          
          QAGAT  (K+NML)=QAROW  (IWMOS(K))                          
          PRESGAT(K+NML)=PRESROW(IWMOS(K))                          
          RHOAGAT(K+NML)=RHOAROW(IWMOS(K))                          
          ZRFMGAT(K+NML)=ZRFMROW(IWMOS(K))                          
          ZRFHGAT(K+NML)=ZRFHROW(IWMOS(K))                          
          DO L=1,NBS
            FSDBLGAT(K+NML,L)=FSDBROL(IWMOS(K),L) 
            FSFBLGAT(K+NML,L)=FSFBROL(IWMOS(K),L) 
            FSSBLGAT(K+NML,L)=FSSBROL(IWMOS(K),L) 
          ENDDO
          ZDMLGAT(K+NML)=ZDMROW(IWMOS(K))
          ZDHLGAT(K+NML)=ZDHROW(IWMOS(K))
          RPCPLGAT(K+NML)=RPCPROW(IWMOS(K))
          TRPCLGAT(K+NML)=TRPCROW(IWMOS(K))
          SPCPLGAT(K+NML)=SPCPROW(IWMOS(K))
          TSPCLGAT(K+NML)=TSPCROW(IWMOS(K))
          RHSILGAT(K+NML)=RHSIROW(IWMOS(K))
          RADJLGAT(K+NML)=RADJROW(IWMOS(K))
          PADRLGAT(K+NML)=PADRROW(IWMOS(K))
800   CONTINUE                                                      
C========================================================================
C CHECK ENERGY BALANCE - start of timestep
C ICEBOT doesn't include weight of snow
C
      RHOIW=RHOICE/RHOW
      JL1=1+NML                                                     
      JL2=NMW+NML                                                  
      DO 320 I=JL1,JL2
        ICEBOT=RHOIW*LKICEH(I)
        ICETOP=LKICEH(I)-ICEBOT
        IF (ICEBOT .GE. DELSKIN) THEN 
          HCAP=HCPICE
        ELSE IF (LKICEH(I) .LE. 0.0) THEN
          HCAP=HCPW
        ELSE 
          HCAP=(LKICEH(I)*HCPICE + (DELSKIN-ICEBOT)*HCPW)/DELSKIN
        ENDIF
        IF (N .EQ. 1) THEN 
          CTLSTP(I)= -HCAP*TLAKGAT(I,1)*DELSKIN
        ELSE
          CTLSTP(I)= -HCAP*T0LAK(I)*DELSKIN
        ENDIF
C
        DO 330, J=1,NLAKGAT(I)
          ZTOP=DELSKIN + DELZLK*(J -1)
          ZBOT=DELSKIN + DELZLK*J
          IF (ICEBOT .GE. ZBOT) THEN 
            HCAP=HCPICE
          ELSE IF (ICEBOT .LE. ZTOP) THEN
            HCAP=HCPW
          ELSE 
            Z=ICEBOT-ZTOP
            HCAP=(Z*HCPICE + (DELZLK-Z)*HCPW)/DELZLK
          ENDIF
          CTLSTP(I)=CTLSTP(I) - HCAP*TLAKGAT(I,J)*DELZLK
330     CONTINUE
320   CONTINUE
C
C========================================================================
C          * LAKE MODEL
C                                                                   
      CALL CLASSL (HLAKGAT, LLAKGAT, BLAKGAT, NLAKGAT, TLAKGAT,   
     1 T0LAK, HDPTH, LKICEH, SNICEH, ROFICEH,
     2 SNOLGAT,RHOSLGAT,TSNOLGAT,ALBSLGAT,WSNOLGAT,
     3 CDHGAT,CDMGAT,QSENLGAT,TFXLGAT,QEVPLGAT,QFSLGAT,QFXLGAT,
     4 PETLGAT, EFLGAT, GTLGAT, QGLGAT, DRLGAT,
     5 SFTLGAT,SFULGAT,SFVLGAT,SFQLGAT,SFHLGAT,QLWOLGAT,ALVLGAT,ALILGAT,
     6 FSGLGAT, FLGLGAT, HFSLGAT, HEVLGAT, HMFLGAT, HTCLGAT,
     7 FSGSLGAT, FLGSLGAT, HFSSLGAT, HEVSLGAT, HMFNLGAT, HTCSLGAT,
     8 PCPLGAT,PCPNLGAT,QFLGAT,QFNLGAT,ROFNLGAT,FICEGAT,FLSGAT,G0SLGAT,
     9 EXPW, DTEMP, TKELAK, DELU, GRED, RHOMIX,   
     A FSVHGAT, FSIHGAT, FDLGAT, ULGAT, VLGAT, TAGAT, QAGAT,
     B RHOAGAT, PADRLGAT, PRESGAT, CSZGAT, ZRFMGAT, ZRFHGAT, 
     C ZDMLGAT,ZDHLGAT,RPCPLGAT,TRPCLGAT,SPCPLGAT,TSPCLGAT,RHSILGAT,
     >   RADJLGAT,
     D ASVLGAT,ASILGAT,FSDBLGAT, FSFBLGAT, FSSBLGAT, REFLGAT, BCSNLGAT,
     E ILG, JL1, JL2, NLAT, NLAKMAX, ISLFD, IZREF, ITG,
     F IALS, NBS, ISNOALB, IGL, IRSTRT,
     G N, IYEAR, IDAY, IHOUR, IMIN, TSED  )
C
C========================================================================
C CHECK ENERGY BALANCE - end of timestep
C
      DO 325 I=JL1,JL2
        ICEBOT=RHOIW*LKICEH(I)
        ICETOP=LKICEH(I)-ICEBOT
        IF (ICEBOT .GE. DELSKIN) THEN 
          HCAP=HCPICE
        ELSE IF (LKICEH(I) .LE. 0.0) THEN
          HCAP=HCPW
        ELSE 
          HCAP=(LKICEH(I)*HCPICE + (DELSKIN-ICEBOT)*HCPW)/DELSKIN
        ENDIF
        CTLSTP(I)= CTLSTP(I) + HCAP*T0LAK(I)*DELSKIN
        DO 335, J=1,NLAKGAT(I)
          ZTOP=DELSKIN + DELZLK*(J -1)
          ZBOT=DELSKIN + DELZLK*J
          IF (ICEBOT .GE. ZBOT) THEN 
            HCAP=HCPICE
          ELSE IF (ICEBOT .LE. ZTOP) THEN
            HCAP=HCPW
          ELSE 
            Z=ICEBOT-ZTOP
            HCAP=(Z*HCPICE + (DELZLK-Z)*HCPW)/DELZLK
          ENDIF
          CTLSTP(I)=CTLSTP(I) + HCAP*TLAKGAT(I,J)*DELZLK
335     CONTINUE
        CTLSTP(I)=CTLSTP(I)/DELT
        QSUML=FSGLGAT(I)+FLGLGAT(I)-HFSLGAT(I)-HEVLGAT(I)-HMFLGAT(I)


        WRITE(79,6019) IYEAR,IDAY,IHOUR,IMIN,QSUML-CTLSTP(I),QSUML
6019    FORMAT(I4,1X,3(I3,1X),2F8.2)
CCC     IF(ABS(CTLSTP(I)-QSUML).GE. 10.0) THEN
CCC           WRITE(6,6440) N,DAY,CTLSTP(I),QSUML,QSUML-CTLSTP(I)
CCC6440          FORMAT(2X,'LAKE ENERGY BALANCE  ',I8,F8.3,2F20.8,F8.2)
Cmdm          WRITE(6,6450) FSGLGAT(I),FLGLGAT(I),HFSLGAT(I),
Cmdm 1             HEVLGAT(I),HMFLGAT(I),LKICEH(I),T0LAK(I)
Cmdm6450          FORMAT(2X,7F15.6)
Ctest         STOP
CCC     ENDIF
325   CONTINUE
C
C========================================================================
C SCATTER CODE FROM CLASSS 
C-- LAKE DIAGNOSTIC VARIABLES SPLIT OUT                     
      DO 701 K=1,NMW                                                
          HLAKROT(IWMOS(K),JWMOS(K))=HLAKGAT(K+NML)                 
          LLAKROT(IWMOS(K),JWMOS(K))=LLAKGAT(K+NML)                 
          BLAKROT(IWMOS(K),JWMOS(K))=BLAKGAT(K+NML)                 
          NLAKROT(IWMOS(K),JWMOS(K))=NLAKGAT(K+NML)                 
          DO 706 L=1,NLAKGAT(K+NML)                                 
            TLAKROT(IWMOS(K),JWMOS(K),L)=TLAKGAT(K+NML,L)           
706       CONTINUE                                                  
          ASVDROT(IWMOS(K),JWMOS(K))=ASVLGAT(K+NML)
          ASIDROT(IWMOS(K),JWMOS(K))=ASILGAT(K+NML)
          SNOLROT(IWMOS(K),JWMOS(K))=SNOLGAT(K+NML)
          RHOSLROT(IWMOS(K),JWMOS(K))=RHOSLGAT(K+NML)
          TSNOLROT(IWMOS(K),JWMOS(K))=TSNOLGAT(K+NML)
          ALBSLROT(IWMOS(K),JWMOS(K))=ALBSLGAT(K+NML)
          WSNOLROT(IWMOS(K),JWMOS(K))=WSNOLGAT(K+NML)
701   CONTINUE                                                      
C
C-- FLUX DIAGNOSTIC VARIABLES SPLIT OUT                     
      DO 385 K=1,NMW                                                
          CDHROT (IWMOS(K),JWMOS(K))=CDHGAT (K+NML)                 
          CDMROT (IWMOS(K),JWMOS(K))=CDMGAT (K+NML)                 
          HFSLROT (IWMOS(K),JWMOS(K))=HFSLGAT (K+NML)               
          HEVLROT (IWMOS(K),JWMOS(K))=HEVLGAT(K+NML)                
          FSGLROT (IWMOS(K),JWMOS(K))=FSGLGAT(K+NML)                
          FLGLROT (IWMOS(K),JWMOS(K))=FLGLGAT(K+NML)                
          HMFLROT (IWMOS(K),JWMOS(K))=HMFLGAT(K+NML)                
          PCPLROT (IWMOS(K),JWMOS(K))=PCPLGAT(K+NML)                
          PCPNLROT (IWMOS(K),JWMOS(K))=PCPNLGAT(K+NML)                
          QFLROT (IWMOS(K),JWMOS(K))=QFLGAT(K+NML)                
          QFNLROT (IWMOS(K),JWMOS(K))=QFNLGAT(K+NML)                
          ROFNLROT (IWMOS(K),JWMOS(K))=ROFNLGAT(K+NML)                
          HFSSLROT (IWMOS(K),JWMOS(K))=HFSSLGAT (K+NML)               
          HEVSLROT (IWMOS(K),JWMOS(K))=HEVSLGAT(K+NML)                
          FSGSLROT (IWMOS(K),JWMOS(K))=FSGSLGAT(K+NML)                
          FLGSLROT (IWMOS(K),JWMOS(K))=FLGSLGAT(K+NML)                
          HMFNLROT (IWMOS(K),JWMOS(K))=HMFNLGAT(K+NML)                
          HTCSLROT (IWMOS(K),JWMOS(K))=HTCSLGAT(K+NML)                
          HTCLROT (IWMOS(K),JWMOS(K))=HTCLGAT(K+NML)                
          SFTLROT (IWMOS(K),JWMOS(K))=SFTLGAT(K+NML)                
          SFULROT (IWMOS(K),JWMOS(K))=SFULGAT(K+NML)                
          SFVLROT (IWMOS(K),JWMOS(K))=SFVLGAT(K+NML)                
          SFQLROT (IWMOS(K),JWMOS(K))=SFQLGAT(K+NML)                
          SFHLROT (IWMOS(K),JWMOS(K))=SFHLGAT(K+NML)                
          QLWOLROT (IWMOS(K),JWMOS(K))=QLWOLGAT(K+NML)                
          ALVLROT (IWMOS(K),JWMOS(K))=ALVLGAT(K+NML)                
          ALILROT (IWMOS(K),JWMOS(K))=ALILGAT(K+NML)                
          PETLROT (IWMOS(K),JWMOS(K))=PETLGAT(K+NML)                
          EFLROT (IWMOS(K),JWMOS(K))=EFLGAT(K+NML)                
          GTLROT (IWMOS(K),JWMOS(K))=GTLGAT(K+NML)                
          QGLROT (IWMOS(K),JWMOS(K))=QGLGAT(K+NML)                
          DRLROT (IWMOS(K),JWMOS(K))=DRLGAT(K+NML)                
          QSENLROT (IWMOS(K),JWMOS(K))=QSENLGAT(K+NML)                
          TFXLROT (IWMOS(K),JWMOS(K))=TFXLGAT(K+NML)                
          QEVPLROT (IWMOS(K),JWMOS(K))=QEVPLGAT(K+NML)                
          QFSLROT (IWMOS(K),JWMOS(K))=QFSLGAT(K+NML)                
          QFXLROT (IWMOS(K),JWMOS(K))=QFXLGAT(K+NML)                
385   CONTINUE                                                      
C
C-- DAY COUNTER
      NCOUNT=NCOUNT+1
      IF(NCOUNT.GT.NDAY) THEN
          NCOUNT=1
      ENDIF
C=======================================================================
C
      GO TO 200
999   CONTINUE
C
      STOP
      END
