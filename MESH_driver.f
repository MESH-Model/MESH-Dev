      PROGRAM RUNCLASS

C       MESH DRIVER
C
C
C     * AUG 28/07 - F.SEGLENIEKS. CHANGED FILENAMES AND REARRANGED THE CODE
C     * MAY 21/07 - B.DAVISON.    INITIAL VERSION BASED ON WORK OF E.D. SOULIS
C       AND F. SEGLENIEKS AT THE UNIVERSITY OF WATERLOO

C=======================================================================
C     * DIMENSION STATEMENTS.

C     * FIRST SET OF DEFINITIONS:
C     * BACKGROUND VARIABLES, AND PROGNOSTIC AND DIAGNOSTIC
C     * VARIABLES NORMALLY PROVIDED BY AND/OR USED BY THE GCM.
C     * THE SUFFIX "ROW" REFERS TO VARIABLES EXISTING ON THE
C     * MOSAIC GRID ON THE CURRENT LATITUDE CIRCLE.  THE SUFFIX
C     * "GAT" REFERS TO THE SAME VARIABLES AFTER THEY HAVE UNDERGONE
C     * A "GATHER" OPERATION IN WHICH THE TWO MOSAIC DIMENSIONS
C     * ARE COLLAPSED INTO ONE.  THE SUFFIX "GRD" REFERS BOTH TO
C     * GRID-CONSTANT INPUT VARIABLES. AND TO GRID-AVERAGED
C     * DIAGNOSTIC VARIABLES.
C
C     * THE FIRST DIMENSION ELEMENT OF THE "ROW" VARIABLES
C     * REFERS TO THE NUMBER OF GRID CELLS ON THE CURRENT
C     * LATITUDE CIRCLE.  IN THIS STAND-ALONE VERSION, THIS
C     * NUMBER IS ARBITRARILY SET TO THREE, TO ALLOW UP TO THREE
C     * SIMULTANEOUS TESTS TO BE RUN.  THE SECOND DIMENSION
C     * ELEMENT OF THE "ROW" VARIABLES REFERS TO THE MAXIMUM
C     * NUMBER OF TILES IN THE MOSAIC.  IN THIS STAND-ALONE
C     * VERSION, THIS NUMBER IS SET TO EIGHT.  THE FIRST
C     * DIMENSION ELEMENT IN THE "GAT" VARIABLES IS GIVEN BY
C     * THE PRODUCT OF THE FIRST TWO DIMENSION ELEMENTS IN THE
C     * "ROW" VARIABLES.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
!     FUTUREDO: Anything using NLAT needs to be allocatable.
!     For now, set it to 1500 for SSRB
      INTEGER,PARAMETER :: NLAT=210,NMOS=15,ILG=NLAT*NMOS
      INTEGER,PARAMETER :: ICAN=4,IGND=3,ICP1=ICAN+1
      INTEGER,PARAMETER :: M_X=100,M_Y=100,M_S=40,M_R=3,M_C=5
C
C     WATERSHED RELATED VARIABLES
      INTEGER LATDEGMIN,LATMINMIN,LATDEGMAX,LATMINMAX,LONDEGMIN,
     +     LONMINMIN,LONDEGMAX,LONMINMAX
      INTEGER WF_NA,WF_NAA,WF_NTYPE,WF_IYMIN,WF_IYMAX,WF_JXMIN,
     +WF_JXMAX,WF_IMAX,WF_JMAX,WF_NRIV
      REAL WF_GRDN,WF_GRDE

      INTEGER WF_YY(NLAT),WF_XX(NLAT),WF_IBN(NLAT),
     +     WF_IROUGH(NLAT),WF_ICHNL(NLAT),WF_NEXT(NLAT),WF_ELEV(NLAT),
     +     WF_IREACH(NLAT)
      REAL*8 WF_DA(NLAT),WF_BNKFLL(NLAT),
     +     WF_CHANNELSLOPE(NLAT),
     +     WF_FRAC(NLAT),LATLENGTH,LONGLENGTH
      REAL*8 WF_ACLASS(NLAT,NMOS), WF_AL
	REAL WF_LAND_MAX, WF_LAND_SUM
      INTEGER WF_LAND_COUNT

C     IOSTAT VARIABLE
      INTEGER IOS

C FOR OUTPUT
      INTEGER N_OUT(5), II_OUT(5), WF_NUM_POINTS
      INTEGER I_OUT
      CHARACTER DIR_OUT(5)*10 , BNAM*12
      CHARACTER*10 GENDIR_OUT
      REAL TOTAL_ROFACC,TOTAL_ROFOACC,TOTAL_ROFSACC,TOTAL_ROFBACC
      REAL TOTAL_EVAPACC,TOTAL_PREACC, INIT_STORE, FINAL_STORE
	REAL TOTAL_AREA

C     OTHER RANDOM VARIABLES
      INTEGER IY,JX
	INTEGER CONFLAGS, OPTFLAGS, INDEPPAR, DEPPAR
      INTEGER BASINSHORTWAVEFLAG, BASINLONGWAVEFLAG, BASINRAINFLAG,
     1        BASINTEMPERATUREFLAG, BASINWINDFLAG, BASINPRESFLAG,
     2        BASINHUMIDITYFLAG
      INTEGER HOURLYFLAG

C     INPUT MET DATA VARIABLES
C     THESE HAVE TO BE REAL*4 IN ORDER TO READ IN THE MET DATA CORRECTLY
      REAL*4 R4SHRTGRID2D(M_Y,M_X)
      REAL*4 R4LONGGRID2D(M_Y,M_X)
      REAL*4 R4RAINGRID2D(M_Y,M_X)
      REAL*4 R4TEMPGRID2D(M_Y,M_X)
      REAL*4 R4WINDGRID2D(M_Y,M_X)
      REAL*4 R4PRESGRID2D(M_Y,M_X)
      REAL*4 R4HUMDGRID2D(M_Y,M_X)


c     sca variables
!     This is the fraction of each square that's in the basin
      REAL BASIN_FRACTION(NLAT)

c 84 is the value for TVC, for another basin, figure out NLTEST and use that instead
C 39 is the value for WC
      REAL basin_SCA
      REAL basin_SWE
C     STREAMFLOW VARIABLES
      INTEGER WF_NO, WF_NL, WF_MHRD, WF_KT
      INTEGER WF_IY(M_S),WF_JX(M_S), WF_S(M_S)
      REAL WF_QHYD(M_S),WF_QHYD_AVG(M_S),WF_QSYN(M_S)
      CHARACTER WF_GAGE(M_S)*8

C     RESERVOIR VARIABLES
      INTEGER WF_NORESV, WF_NREL, WF_KTR, WF_NORESV_CTRL
	INTEGER WF_IRES(M_R), WF_JRES(M_R), WF_RES(M_R), WF_R(M_R)
	REAL WF_B1(M_R),WF_B2(M_R),WF_QREL(M_R), WF_RESSTORE(M_R)
	CHARACTER WF_RESNAME(M_R)*8

C FOR BASEFLOW INITIALIZATION
      INTEGER JAN

C     FOR ROUTING
      INTEGER WF_ROUTETIMESTEP, WF_TIMECOUNT, DRIVERTIMESTEP
	REAL WF_R1(M_C),WF_R2(M_C),WF_NHYD(NLAT),WF_QBASE(NLAT)
	REAL WF_QI2(NLAT),WF_QO1(NLAT),WF_QO2(NLAT), WF_QR(NLAT)
	REAL WF_STORE1(NLAT), WF_STORE2(NLAT), WF_QI1(NLAT)

      INTEGER IDISP,IZREF,ISLFD,IPCP,IWF,ILAI,IHGT,IALC,
     1        IALS,IALG,N,ILW,ITG,ITC,ITCG,NLTEST,NMTEST,NCOUNT,NSUM,
     2        IHOUR,IMIN,IDAY,IYEAR,NML,NMW,NWAT,NICE,
     3        NLANDCS,NLANDGS,NLANDC,NLANDG,NLANDI,I,J,K,L,M
C
C Saul=======
c IYEAR_START: Initial year for data at the bin file
c IDAY_START : Initial day for data at the bin file
c IHOUR_START: Initial hour for data at the bin file
c IMIN_START : Initial minute for data at the bin file
C ISTEP_START: Time step; 1:hour, 2:half of hour
C toskip   : The number of variables in the file per timestep
C HOURLY_START_*: Start day/year for recording hourly averaged data
C HOURLY_STOP_*: Stop day/year for recording hourly averaged data
C DAILY_START_*: Start day/year for recording daily averaged data
C DAILY_STOP_*: Stop day/year for recording daily averaged data
      INTEGER IHOUR_START,IMIN_START,IDAY_START,IYEAR_START,ISTEP_START
      INTEGER IHOUR_END,IMIN_END,IDAY_END,IYEAR_END
      INTEGER nyy,ndy,nmy,nhy,nrs,toskip
      INTEGER HOURLY_START_DAY, HOURLY_STOP_DAY, DAILY_START_DAY,
     1           DAILY_STOP_DAY
      INTEGER HOURLY_START_YEAR, HOURLY_STOP_YEAR, DAILY_START_YEAR,
     1           DAILY_STOP_YEAR
C===========
      INTEGER K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11
C
C     * LAND SURFACE PROGNOSTIC VARIABLES.
C
      REAL,DIMENSION(NLAT,NMOS,IGND) ::
     1        TBARROW,   THLQROW,   THICROW
C
      REAL,DIMENSION(NLAT,NMOS) ::
     1        TPNDROW,   ZPNDROW,   TBASROW,
     2        ALBSROW,   TSNOROW,   RHOSROW,
     3        SNOROW ,   TCANROW,   RCANROW,
     4        SCANROW,   GROROW ,   CMAIROW,
     5        TACROW ,   QACROW ,   WSNOROW
C
      REAL    TSFSROW(NLAT,NMOS,4)
C
      REAL,DIMENSION(ILG,IGND) ::
     1        TBARGAT, THLQGAT, THICGAT
C
      REAL,DIMENSION(ILG) ::
     1        TPNDGAT,   ZPNDGAT,   TBASGAT,
     2        ALBSGAT,   TSNOGAT,   RHOSGAT,
     3        SNOGAT ,   TCANGAT,   RCANGAT,
     4        SCANGAT,   GROGAT ,   CMAIGAT,
     5        TACGAT ,   QACGAT ,   WSNOGAT
C
      REAL    TSFSGAT(ILG,4)
C
C     * GATHER-SCATTER INDEX ARRAYS.
C
      INTEGER  ILMOS (ILG),  JLMOS  (ILG),  IWMOS  (ILG),  JWMOS (ILG),
     1         IWAT  (NLAT), IICE   (NLAT)
C
C     * CANOPY AND SOIL INFORMATION ARRAYS.
C     * (THE LENGTH OF THESE ARRAYS IS DETERMINED BY THE NUMBER
C     * OF SOIL LAYERS (3) AND THE NUMBER OF BROAD VEGETATION
C     * CATEGORIES (4, OR 5 INCLUDING URBAN AREAS).)
C
      REAL,DIMENSION(NLAT,NMOS,ICP1) ::
     1              FCANROW,  LNZ0ROW,
     2              ALVCROW,  ALICROW
C
      REAL,DIMENSION(NLAT,NMOS,ICAN) ::
     1              LAMXROW,  LAMNROW,
     2              CMASROW,  ROOTROW,
     3              RSMNROW,  QA50ROW,
     4              VPDAROW,  VPDBROW,
     5              PSGAROW,  PSGBROW,
     6              AILDROW,  HGTDROW,
     7              ACVDROW,  ACIDROW
C
      REAL,DIMENSION(ILG,ICP1) ::
     1              FCANGAT,  LNZ0GAT,
     2              ALVCGAT,  ALICGAT
C
      REAL,DIMENSION(ILG,ICAN) ::
     1              LAMXGAT,  LAMNGAT,
     2              CMASGAT,  ROOTGAT,
     3              RSMNGAT,  QA50GAT,
     4              VPDAGAT,  VPDBGAT,
     5              PSGAGAT,  PSGBGAT,
     6              AILDGAT,  HGTDGAT,
     7              ACVDGAT,  ACIDGAT
C
      REAL,DIMENSION(NLAT,NMOS,IGND) ::
     1        THPROW ,  THRROW ,  THMROW ,
     2        BIROW  ,  PSISROW,  GRKSROW,
     3        THRAROW,  HCPSROW,
     4        TCSROW ,  THFCROW,  PSIWROW,
     5        DLZWROW,  ZBTWROW
C
      REAL,DIMENSION(NLAT,NMOS) ::
     1        DRNROW ,   XSLPROW,   GRKFROW,
     2        WFSFROW,   WFCIROW,   ALGWROW,
     3        ALGDROW,   ASVDROW,   ASIDROW,
     4        AGVDROW,   AGIDROW,   ZSNLROW,
     5        ZPLGROW,   ZPLSROW
C
      REAL,DIMENSION(ILG,IGND) ::
     1        THPGAT ,  THRGAT ,  THMGAT ,
     2        BIGAT  ,  PSISGAT,  GRKSGAT,
     3        THRAGAT,  HCPSGAT,
     4        TCSGAT ,  THFCGAT,  PSIWGAT,
     5        DLZWGAT,  ZBTWGAT,  GFLXGAT
C
      REAL,DIMENSION(ILG) ::
     1        DRNGAT ,   XSLPGAT,   GRKFGAT,
     2        WFSFGAT,   WFCIGAT,   ALGWGAT,
     3        ALGDGAT,   ASVDGAT,   ASIDGAT,
     4        AGVDGAT,   AGIDGAT,   ZSNLGAT,
     5        ZPLGGAT,   ZPLSGAT
C
      REAL    SANDROW(NLAT,NMOS,IGND), CLAYROW(NLAT,NMOS,IGND),
     1        ORGMROW(NLAT,NMOS,IGND),
     2        SDEPROW(NLAT,NMOS),      FAREROW(NLAT,NMOS)
C
      INTEGER MIDROW (NLAT,NMOS),     ISNDROW(NLAT,NMOS,IGND),
     1        ISNDGAT( ILG,IGND),     IORG   (NLAT,NMOS,IGND)

      REAL  THPORG (  3), THRORG (  3), THMORG (  3), BORG   (  3),
     1      PSISORG(  3), GRKSORG(  3)
C
      REAL  CANEXT(ICAN), XLEAF (ICAN), ZORAT (ICAN),
     1      DELZ  (IGND), ZBOT  (IGND),
     2      GROWYR (  18,4,2)

C * WATROF DECLARATIONS
      REAL  DDROW(NLAT,NMOS),MANNROW(NLAT,NMOS),DDGAT(ILG),MANNGAT(ILG)
C
C     * ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES.
C
      REAL,DIMENSION(NLAT) ::
     1      ZRFMGRD,   ZRFHGRD,   ZDMGRD ,   ZDHGRD ,
     2      ZBLDGRD,   FSVHGRD,   FSIHGRD,   RADJGRD,
     3      CSZGRD ,   FDLGRD ,   ULGRD  ,   VLGRD  ,
     4      TAGRD  ,   QAGRD  ,   PRESGRD,   PREGRD ,
     5      PADRGRD,   VPDGRD ,   TADPGRD,   RHOAGRD,
     6      RPCPGRD,   TRPCGRD,   SPCPGRD,   TSPCGRD,
     7      RHSIGRD,   FCLOGRD,   DLONGRD,   UVGRD  ,
     8      XDIFFUS,   GCGRD  ,   Z0ORGRD,   GGEOGRD,
     9      RPREGRD,   SPREGRD
C
      REAL,DIMENSION(ILG) ::
     1      ZRFMGAT,   ZRFHGAT,   ZDMGAT ,   ZDHGAT ,
     2      ZBLDGAT,   FSVHGAT,   FSIHGAT,   RADJGAT,
     3      CSZGAT ,   FDLGAT ,   ULGAT  ,   VLGAT  ,
     4      TAGAT  ,   QAGAT  ,   PRESGAT,   PREGAT ,
     5      PADRGAT,   VPDGAT ,   TADPGAT,   RHOAGAT,
     6      RPCPGAT,   TRPCGAT,   SPCPGAT,   TSPCGAT,
     7      RHSIGAT,   FCLOGAT,   DLONGAT,   Z0ORGAT,
     8      GGEOGAT
C
C     * LAND SURFACE DIAGNOSTIC VARIABLES.
C
      REAL,DIMENSION(NLAT,NMOS) ::
     1      CDHROW ,   CDMROW ,   HFSROW ,   TFXROW ,
     2      QEVPROW,   QFSROW ,   QFXROW ,   PETROW ,
     3      GAROW  ,   EFROW  ,   GTROW  ,   QGROW  ,
     4      TSFROW ,   ALVSROW,   ALIRROW,   FSNOROW,
     5      SFCTROW,   SFCUROW,   SFCVROW,   SFCQROW,
     6      FSGVROW,   FSGSROW,   FSGGROW,   FLGVROW,
     7      FLGSROW,   FLGGROW,   HFSCROW,   HFSSROW,
     8      HFSGROW,   HEVCROW,   HEVSROW,   HEVGROW,
     9      HMFCROW,   HMFNROW,   HTCCROW,   HTCSROW,
     A      PCFCROW,   PCLCROW,   PCPNROW,   PCPGROW,
     B      QFGROW ,   QFNROW ,   QFCLROW,   QFCFROW,
     C      ROFROW ,   ROFOROW,   ROFSROW,   ROFBROW,
     D      TROFROW,   TROOROW,   TROSROW,   TROBROW,
     E      ROFCROW,   ROFNROW,   ROVGROW,   WTRCROW,
     F      WTRSROW,   WTRGROW,   DRROW  ,   WTABROW,
     G      ILMOROW,   UEROW  ,   HBLROW
C
      REAL,DIMENSION(ILG) ::
     1      CDHGAT ,   CDMGAT ,   HFSGAT ,   TFXGAT ,
     2      QEVPGAT,   QFSGAT ,   QFXGAT ,   PETGAT ,
     3      GAGAT  ,   EFGAT  ,   GTGAT  ,   QGGAT  ,
     4      TSFGAT ,   ALVSGAT,   ALIRGAT,   FSNOGAT,
     5      SFCTGAT,   SFCUGAT,   SFCVGAT,   SFCQGAT,
     6      FSGVGAT,   FSGSGAT,   FSGGGAT,   FLGVGAT,
     7      FLGSGAT,   FLGGGAT,   HFSCGAT,   HFSSGAT,
     8      HFSGGAT,   HEVCGAT,   HEVSGAT,   HEVGGAT,
     9      HMFCGAT,   HMFNGAT,   HTCCGAT,   HTCSGAT,
     A      PCFCGAT,   PCLCGAT,   PCPNGAT,   PCPGGAT,
     B      QFGGAT ,   QFNGAT ,   QFCLGAT,   QFCFGAT,
     C      ROFGAT ,   ROFOGAT,   ROFSGAT,   ROFBGAT,
     D      TROFGAT,   TROOGAT,   TROSGAT,   TROBGAT,
     E      ROFCGAT,   ROFNGAT,   ROVGGAT,   WTRCGAT,
     F      WTRSGAT,   WTRGGAT,   DRGAT  ,   WTABGAT,
     G      ILMOGAT,   UEGAT  ,   HBLGAT
C
      REAL,DIMENSION(NLAT) ::
     1      CDHGRD ,   CDMGRD ,   HFSGRD ,   TFXGRD ,
     2      QEVPGRD,   QFSGRD ,   QFXGRD ,   PETGRD ,
     3      GAGRD  ,   EFGRD  ,   GTGRD  ,   QGGRD  ,
     4      TSFGRD ,   ALVSGRD,   ALIRGRD,   FSNOGRD,
     5      SFCTGRD,   SFCUGRD,   SFCVGRD,   SFCQGRD,
     6      FSGVGRD,   FSGSGRD,   FSGGGRD,   FLGVGRD,
     7      FLGSGRD,   FLGGGRD,   HFSCGRD,   HFSSGRD,
     8      HFSGGRD,   HEVCGRD,   HEVSGRD,   HEVGGRD,
     9      HMFCGRD,   HMFNGRD,   HTCCGRD,   HTCSGRD,
     A      PCFCGRD,   PCLCGRD,   PCPNGRD,   PCPGGRD,
     B      QFGGRD ,   QFNGRD ,   QFCLGRD,   QFCFGRD,
     C      ROFGRD ,   ROFOGRD,   ROFSGRD,   ROFBGRD,
     D      ROFCGRD,   ROFNGRD,   ROVGGRD,   WTRCGRD,
     E      WTRSGRD,   WTRGGRD,   DRGRD  ,   WTABGRD,
     F      ILMOGRD,   UEGRD  ,   HBLGRD ,   SNOGRD
C
      REAL    HMFGROW(NLAT,NMOS,IGND),   HTCROW (NLAT,NMOS,IGND),
     1        QFCROW (NLAT,NMOS,IGND),
     2        HMFGGAT(ILG,IGND),         HTCGAT (ILG,IGND),
     3        QFCGAT (ILG,IGND),
     4        HMFGGRD(NLAT,IGND),        HTCGRD (NLAT,IGND),
     5        QFCGRD (NLAT,IGND)
C
      INTEGER     ITCTROW(NLAT,NMOS,6,50),  ITCTGAT(ILG,6,50)

C     * ARRAYS USED FOR OUTPUT AND DISPLAY PURPOSES.
C     * (THE SUFFIX "ACC" REFERS TO ACCUMULATOR ARRAYS USED IN
C     * CALCULATING TIME AVERAGES.)

      CHARACTER     TITLE1*4,     TITLE2*4,     TITLE3*4,
     1              TITLE4*4,     TITLE5*4,     TITLE6*4
      CHARACTER     NAME1*4,      NAME2*4,      NAME3*4,
     1              NAME4*4,      NAME5*4,      NAME6*4
      CHARACTER     PLACE1*4,     PLACE2*4,     PLACE3*4,
     1              PLACE4*4,     PLACE5*4,     PLACE6*4

      REAL,DIMENSION(NLAT) ::
     1              PREACC ,   GTACC  ,   QEVPACC,
     2              HFSACC ,   ROFACC ,   SNOACC ,
     3              ALVSACC,   ALIRACC,   FSINACC,
     4              FLINACC,   TAACC  ,   UVACC  ,
     5              PRESACC,   QAACC  ,
     6              EVAPACC,   FLUTACC,   ROFOACC , ROFSACC, ROFBACC,
     7              HMFNACC,   WTBLACC,   WSNOACC,
     8              RHOSACC,   TSNOACC,   TCANACC,
     9              RCANACC,   SCANACC,   GROACC ,
     A              CANARE ,   SNOARE

      REAL          TBARACC(NLAT,IGND), THLQACC(NLAT,IGND),
     1              THICACC(NLAT,IGND), THALACC(NLAT,IGND),
     2              TBAR3(ILG,3)
C
C     * ARRAYS DEFINED TO PASS INFORMATION BETWEEN THE THREE MAJOR
C     * SUBSECTIONS OF CLASS ("CLASSA", "CLASST" AND "CLASSW").

      REAL,DIMENSION(ILG,IGND) ::
     1        TBARC  ,     TBARG  ,     TBARCS ,
     2        TBARGS ,     THLIQC ,     THLIQG ,
     3        THICEC ,     THICEG ,     FROOT  ,
     4        HCPC   ,     HCPG   ,
     5        TCTOP  ,     TCBOT
C
      REAL  FC     (ILG), FG     (ILG), FCS    (ILG), FGS    (ILG),
     1      RBCOEF (ILG), ZSNOW  (ILG),
     2      AILCAN (ILG), AILCNS (ILG), FSVF   (ILG), FSVFS  (ILG),
     3      ALVSCN (ILG), ALIRCN (ILG), ALVSG  (ILG), ALIRG  (ILG),
     4      ALVSCS (ILG), ALIRCS (ILG), ALVSSN (ILG), ALIRSN (ILG),
     5      ALVSGC (ILG), ALIRGC (ILG), ALVSSC (ILG), ALIRSC (ILG),
     6      TRVSCN (ILG), TRIRCN (ILG), TRVSCS (ILG), TRIRCS (ILG),
     7      RC     (ILG), RCS    (ILG), FRAINC (ILG), FSNOWC (ILG),
     8      CMASSC (ILG), CMASCS (ILG), DISP   (ILG), DISPS  (ILG),
     9      ZOMLNC (ILG), ZOELNC (ILG), ZOMLNG (ILG), ZOELNG (ILG),
     A      ZOMLCS (ILG), ZOELCS (ILG), ZOMLNS (ILG), ZOELNS (ILG),
     B      TRSNOW (ILG), CHCAP  (ILG), CHCAPS (ILG),
     C      GZEROC (ILG), GZEROG (ILG), GZROCS (ILG), GZROGS (ILG),
     D      G12C   (ILG), G12G   (ILG), G12CS  (ILG), G12GS  (ILG),
     E      G23C   (ILG), G23G   (ILG), G23CS  (ILG), G23GS  (ILG),
     F      QFREZC (ILG), QFREZG (ILG), QMELTC (ILG), QMELTG (ILG),
     G      EVAPC  (ILG), EVAPCG (ILG), EVAPG  (ILG), EVAPCS (ILG),
     H      EVPCSG (ILG), EVAPGS (ILG), TCANO  (ILG), TCANS  (ILG),
     I      RAICAN (ILG), SNOCAN (ILG), RAICNS (ILG), SNOCNS (ILG),
     J      CWLCAP (ILG), CWFCAP (ILG), CWLCPS (ILG), CWFCPS (ILG),
     K      TSNOCS (ILG), TSNOGS (ILG), RHOSCS (ILG), RHOSGS (ILG),
     L      WSNOCS (ILG), WSNOGS (ILG),
     M      TPONDC (ILG), TPONDG (ILG), TPNDCS (ILG), TPNDGS (ILG),
     N      ZPLMCS (ILG), ZPLMGS (ILG), ZPLIMC (ILG), ZPLIMG (ILG)
C
      REAL  ZTHRC (ILG,3,2),    ZTHRG (ILG,3,2),
     1      ZTHRCS(ILG,3,2),    ZTHRGS(ILG,3,2)
C
C     * DIAGNOSTIC ARRAYS USED FOR CHECKING ENERGY AND WATER
C     * BALANCES.
C
      REAL CTVSTP(ILG),   CTSSTP(ILG),   CT1STP(ILG),   CT2STP(ILG),
     1     CT3STP(ILG),   WTVSTP(ILG),   WTSSTP(ILG),   WTGSTP(ILG)

C BRUCE
C     * INTERNAL WORK ARRAYS FOR CLASSBHYD and Hydrology (iwf.ini and soil.ini).
C     * These are used if soil parameters are read in directly from soil.ini

      INTEGER SOIL_IOS

      REAL wc_thpor (NLAT,NMOS,IGND),wc_thlret(NLAT,NMOS,IGND),
     1     wc_thlmin(NLAT,NMOS,IGND),wc_bi    (NLAT,NMOS,IGND),
     2     wc_psisat(NLAT,NMOS,IGND),wc_grksat(NLAT,NMOS,IGND),
     3     wc_hcps  (NLAT,NMOS,IGND),wc_tcs   (NLAT,NMOS,IGND),
     4     wc_algwet(NLAT,NMOS)     ,wc_algdry(NLAT,NMOS)

C==========

C
C
C     * START ENSIM == FOR ENSIM == FOR ENSIM == FOR ENSIM ==
C
      INTEGER,      ALLOCATABLE::wfo_pick(:)
      CHARACTER*50, ALLOCATABLE::wfo_attributes(:)
      CHARACTER*10 wf_landclassname(10)
      INTEGER*4 wfo_yy,wfo_mm,wfo_dd,wfo_hh,wfo_mi,wfo_ss,
     *                   wfo_ms,nj,ireport, ensim_month, ensim_day
      INTEGER*4 WFO_SEQ, ENSIM_IOS

C
C End of ENSIM Changes == ENSIM == ENSIM == ENSIM == ENSIM == ENSIM ==
C
c  VARIABLES FOR INITIAL SOIL MOISTURE AND SOIL TEMPERATURE
c  Saul M. feb 26 2008
      REAL*4,       ALLOCATABLE::valuem(:,:,:)
      REAL*4,       ALLOCATABLE::valuet(:,:,:)
c
      INTEGER*4 s_ios, ix
c

C     * CONSTANTS AND TEMPORARY VARIABLES.
C
      REAL DEGLAT,DEGLON,FSDOWN1,FSDOWN2,FSDOWN3,DAY,
     1     DECL,HOUR,COSZ,
     2     ALTOT,FSSTAR,FLSTAR,QH,QE,BEG,SNOMLT,ZSN,TCN,TSN,TPN,GTOUT
      INTEGER JLAT

      REAL FSDOWN(NLAT)

! *************************************************************
! For reading in options information from MESH_run_options.ini
! *************************************************************
      CHARACTER(20) :: IRONAME
      INTEGER       :: IROVAL

! *******************************************************************
! For reading in the last information in mesh_paramters_hyrology.ini
! *******************************************************************
      CHARACTER(30) :: NMTESTFORMAT

c================
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL X1,X2,X3,X4,G,GAS,X5,X6,CPRES,GASV,X7,CPI,X8,CELZRO,X9,
     1     X10,X11,X12,X13,X14,X15,SIGMA,X16,DELTIM,DELT,TFREZ,
     2     RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,TCW,TCICE,TCSAND,TCCLAY,
     3     TCOM,TCDRYS,RHOSOL,RHOOM,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,
     4     HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,
     5     CLHVAP,PI,ZOLNG,ZOLNS,ZOLNI,ZORATG,ALVSI,ALIRI,ALVSO,ALIRO,
     6     ALBRCK,DELTA,CGRAV,CKARM,CPD,AS,ASX,CI,BS,BETA,FACTN,HMIN
C
c ******************************************************
c Set physical constants and common blocks
c ******************************************************

      COMMON /PARAMS/ X1,    X2,    X3,    X4,   G,GAS,   X5,
     1                X6,    CPRES, GASV,  X7
      COMMON /PARAM1/ CPI,   X8,    CELZRO,X9,    X10,    X11
      COMMON /PARAM3/ X12,   X13,   X14,   X15,   SIGMA,  X16
      COMMON  /TIMES/ DELTIM,K1,    K2,    K3,    K4,     K5,
     1                K6,    K7,    K8,    K9,    K10,    K11
C
C     * THE FOLLOWING COMMON BLOCKS ARE DEFINED SPECIFICALLY FOR USE
C     * IN CLASS, VIA BLOCK DATA AND THE SUBROUTINE "CLASSD".
C
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS2/ RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN
      COMMON /CLASS3/ TCW,TCICE,TCSAND,TCCLAY,TCOM,TCDRYS,
     1                RHOSOL,RHOOM
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
      COMMON /CLASS5/ THPORG,THRORG,THMORG,BORG,PSISORG,GRKSORG
      COMMON /CLASS6/ PI,GROWYR,ZOLNG,ZOLNS,ZOLNI,ZORAT,ZORATG
      COMMON /CLASS7/ CANEXT,XLEAF
      COMMON /CLASS8/ ALVSI,ALIRI,ALVSO,ALIRO,ALBRCK
      COMMON /PHYCON/ DELTA,CGRAV,CKARM,CPD
      COMMON /SURFCON/ AS,ASX,CI,BS,BETA,FACTN,HMIN

	INTEGER LZFFLG,EXTFLG,IWFICE,ERRFLG
      REAL VICEFLG,PSI_LIMIT,HICEFLG

      COMMON /WATFLGS/ VICEFLG,PSI_LIMIT,HICEFLG,LZFFLG,EXTFLG,IWFICE,
     +ERRFLG,IMIN,IHOUR,IDAY,IYEAR

      DATA VICEFLG/3.0/,PSI_LIMIT/1.0/,HICEFLG/1.0/,LZFFLG/0/,EXTFLG/0/,
     +     IWFICE/3/,ERRFLG/1/



c ******************************************************
c Intialize CLASS variables
c ******************************************************

C
      CALL CLASSD

C
C     * THE FOLLOWING SWITCHES SELECT DIFFERENT RUN OPTIONS.
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
C
C     * ITC, ITCG AND ITG ARE SWITCHES TO CHOOSE THE ITERATION SCHEME TO
C     * BE USED IN CALCULATING THE CANOPY OR GROUND SURFACE TEMPERATURE
C     * RESPECTIVELY.  IF THE SWITCH IS SET TO 1, A COMBINATION OF
C     * SECANT AND BISECTION METHODS IS USED; IF TO 2, THE NEWTON-
C     * RAPHSON METHOD IS USED.
C
C     * IF IWF=0, ONLY OVERLAND FLOW AND BASEFLOW ARE MODELLED, AND
C     * THE GROUND SURFACE SLOPE IS NOT MODELLED.
C     * IF IWF=n (0<n<4), THE MODIFIED CALCULATIONS OF OVERLAND FLOW
C     * AND INTERFLOW ARE PERFORMED; INTERFLOW IS DRAWN FROM THE TOP
C     * n SOIL LAYERS.
C
C     * IF ILAI, IHGT, IALC, IALS AND IALG ARE ZERO, THE VALUES OF
C     * LEAF ARE INDEX, VEGETATION HEIGHT, CANOPY ALBEDO, SNOW ALBEDO
C     * AND SOIL ALBEDO RESPECTIVELY CALCULATED BY CLASS ARE USED.
C     * IF ANY OF THESE SWITCHES IS SET TO 1, THE VALUE OF THE
C     * CORRESPONDING PARAMETER CALCULATED BY CLASS IS OVERRIDDEN BY
C     * A USER-SUPPLIED INPUT VALUE.
C
      IDISP=1
      IZREF=1
      ISLFD=2
      IPCP=1
      ITC=2
      ITCG=2
      ITG=2
      IWF=3
      ILAI=0
      IHGT=0
      IALC=0
      IALS=0
      IALG=0
      N=0

      !set HOURLYFLAG to 1 for hourly forcing data
      !set HOURLYFLAG to 0 for 30 minute forcing data
      ! 30 minute forcing data should be the default
      HOURLYFLAG = 0

! The above parameter values are defaults, to change to a different
! value, use the MESH_input_run_options.ini file


! Set the default value of BASIN_FRACTION so that it will be initialised
! correctly later
      BASIN_FRACTION(1) = -1


c *********************************************************************
c Open and read in values from MESH_input_run_options.ini file
c *********************************************************************

      OPEN(UNIT=53,FILE='MESH_input_run_options.ini',STATUS='OLD')

      DO I=1,3
        READ(53,*)
      ENDDO

      READ(53,"(I5)") CONFLAGS

      ! Set flag values based on given input
      IF(CONFLAGS.GT.0) THEN
        DO I=1,CONFLAGS
          READ(53,"(A20, I4)") IRONAME, IROVAL
          IF (IRONAME == "IDISP") THEN
            IDISP = IROVAL
          ELSE IF (IRONAME == "IZREF") THEN
            IZREF = IROVAL
          ELSE IF (IRONAME == "ISLFD") THEN
            ISLFD = IROVAL
          ELSE IF (IRONAME == "IPCP") THEN
            IPCP = IROVAL
          ELSE IF (IRONAME == "ITC") THEN
            ITC = IROVAL
          ELSE IF (IRONAME == "ITCG") THEN
            ITCG = IROVAL
          ELSE IF (IRONAME == "ITG") THEN
            ITG = IROVAL
          ELSE IF (IRONAME == "IWF") THEN
            IWF = IROVAL
          ELSE IF (IRONAME == "ILAI") THEN
            ILAI = IROVAL
          ELSE IF (IRONAME == "IHGT") THEN
            IHGT = IROVAL
          ELSE IF (IRONAME == "IALC") THEN
            IALC = IROVAL
          ELSE IF (IRONAME == "IALS") THEN
            IALS = IROVAL
          ELSE IF (IRONAME == "IALG") THEN
            IALG = IROVAL
          ELSE IF (IRONAME == "N") THEN
            N = IROVAL
          ELSE IF (IRONAME == "HOURLYFLAG") THEN
            HOURLYFLAG = IROVAL
          ELSE
            ! Error when reading the input file
            WRITE (*, *)
     *        "The input file is corrupted in the flags information."
          END IF
        ENDDO
      ENDIF

      DO I=1,2
        READ(53,*)
      ENDDO

      READ (53,"(I5)") WF_NUM_POINTS
c make sure there are no more than 5 outpoints
      IF(WF_NUM_POINTS.GT.5) THEN
	  PRINT *, 'You can only have up to 5 output points'
	  PRINT *, 'Please adjust the MESH_input_run_options.ini file'
	  STOP
	ENDIF
      READ(53,*)

      READ(53,"(5I10)") (N_OUT(I),I=1,WF_NUM_POINTS)
      READ(53,"(5I10)") (II_OUT(I),I=1,WF_NUM_POINTS)
      READ(53,"(5A10)") (DIR_OUT(I),I=1,WF_NUM_POINTS)

      !This is the directory to output the mesh_output* files and the basin_swe/sca files
      READ(53,*)
      READ(53,*)
      READ(53,'(A10)') GENDIR_OUT

!PARAMESH
      !This section is used to start part way through the bin file
      READ(53,*) !P
      READ(53,*) !P
      READ(53,'(I4, I4, I4, I4)') IYEAR_START, IDAY_START, !P
     +  IHOUR_START, IMIN_START !P
      READ(53,'(I4, I4, I4, I4)') IYEAR_END, IDAY_END, !P
     +  IHOUR_END, IMIN_END !P

      CLOSE(UNIT=53)

c *********************************************************************
c Open and read in values from MESH_input_soil_levels.txt file
c *********************************************************************

      OPEN(UNIT=52,FILE='MESH_input_soil_levels.txt',STATUS='OLD')

      DO I=1,IGND
        READ(52,'(2X,F8.2)') DELZ(I)
      ENDDO

      ZBOT(1) = DELZ(1)
      DO I=2,IGND
         ZBOT(I) = ZBOT(I-1) + DELZ(I)
      ENDDO

	CLOSE(UNIT=52)

c *********************************************************************
c Open and read in values from MESH_input_drainage_database.txt file
c *********************************************************************

      OPEN(UNIT=20,FILE='MESH_input_drainage_database.txt',STATUS='OLD')

      READ(20,'(I5,50X,I5)') WF_NA,WF_NAA
      READ(20,'(F10.0,5X,2I5)') WF_AL,WF_NRIV,WF_NTYPE
      WF_GRDN=0.0
      WF_GRDE=0.0


c     IOSTAT capture prevents error when not all values present
      READ(20,'(12I5,2F5.0)',IOSTAT=IOS) WF_IYMIN,WF_IYMAX,
     +  WF_JXMIN,WF_JXMAX,LATDEGMIN,LATMINMIN,LATDEGMAX,LATMINMAX,
     +  LONDEGMIN,LONMINMIN,LONDEGMAX,LONMINMAX,WF_GRDN,WF_GRDE

c     Condition for Lat/Long by Frank S Sept/1999
      IF( WF_GRDN.GT.0.0 ) THEN
        WF_IYMIN=LATDEGMIN*60+LATMINMIN
        WF_IYMAX=LATDEGMAX*60+LATMINMAX
        WF_JXMIN=LONDEGMIN*60+LONMINMIN
        WF_JXMAX=LONDEGMAX*60+LONMINMAX

      ELSE
c       Define wf_grdn & wf_grde for UTM
        WF_GRDN=WF_AL/1000.
        WF_GRDE=WF_AL/1000.
      ENDIF

      READ(20,'(2I5)') WF_IMAX,WF_JMAX

c check if we are going to get an "array bounds out of range" error
      IF(WF_IMAX.GT.M_Y) THEN
        PRINT *, 'size of grid arrays in MESH: ',M_Y
        PRINT *, 'number up/down (north/south) ',
     +           'grids from MESH_drainage_database.txt'
	  PRINT *, ' file: ',WF_IMAX
        PRINT *, 'Please adjust these values.'
        STOP
	ENDIF

      IF(WF_JMAX.GT.M_X) THEN
        PRINT *, 'size of grid arrays in MESH: ',M_X
        PRINT *, 'no. of east/west (left/right) grids from ',
     +           'MESH_drainage_database.txt'
	  PRINT *, ' file: ',WF_JMAX
        PRINT *, 'Please adjust these values.'
        STOP
	ENDIF


      DO I = 1, WF_IMAX
        READ(20,*)
      ENDDO

      DO I = 1,WF_NA
        READ(20,'(5X,2I5,3F10.5,I7,5I5,F5.2,5X,15F5.2)')WF_YY(I),
     +    WF_XX(I),WF_DA(I),WF_BNKFLL(I),WF_CHANNELSLOPE(I),
     +    WF_ELEV(I),WF_IBN(I),WF_IROUGH(I),WF_ICHNL(I),
     +    WF_NEXT(I),WF_IREACH(I),WF_FRAC(I),
     +    (WF_ACLASS(I,J),J=1,WF_NTYPE)
C check to make sure land cover areas sum to 100%
        WF_LAND_COUNT=1
        WF_LAND_MAX=0.0
        WF_LAND_SUM=0.0
        DO J=1,WF_NTYPE
          WF_LAND_SUM=WF_LAND_SUM+WF_ACLASS(I,J)
          IF(WF_ACLASS(I,J).GT.WF_LAND_MAX) THEN
            WF_LAND_COUNT=J
    	      WF_LAND_MAX=WF_ACLASS(I,J)
          ENDIF
        ENDDO
	  IF(WF_LAND_SUM.NE.1.0)THEN
	    WF_ACLASS(I,WF_LAND_COUNT)=
     +    WF_ACLASS(I,WF_LAND_COUNT)-(WF_LAND_SUM-1.0)
        ENDIF
      ENDDO

      CLOSE(UNIT=20)

c *********************************************************************
c Open basin_shortwave.csv
c *********************************************************************
      OPEN(unit=90,file='basin_shortwave.csv',
     +  STATUS='OLD',IOSTAT=IOS)

      IF(IOS.NE.0)THEN
        !no basin shortwave file exists, use 'default' c05 behaviour
        BASINSHORTWAVEFLAG = 0
        PRINT *, 'basin_shortwave.csv not found'
      ELSE
        !basin shortwave files does exist, use Pablo's code related
        !to shortwave
        BASINSHORTWAVEFLAG = 1
        PRINT *, 'basin_shortwave.csv found'
      ENDIF

c *********************************************************************
c Open basin_longwave.csv
c *********************************************************************
      OPEN(unit=91,file='basin_longwave.csv',
     +  STATUS='OLD',IOSTAT=IOS)

      IF(IOS.NE.0)THEN
        !no basin longwave file exists, use 'default' c05 behaviour
        BASINLONGWAVEFLAG = 0
        PRINT *, 'basin_longwave.csv not found'
      ELSE
        !basin longwave files does exist, use Pablo's code related
        !to longwave
        BASINLONGWAVEFLAG = 1
        PRINT *, 'basin_longwave.csv found'
      ENDIF

c *********************************************************************
c Open basin_rain.csv
c *********************************************************************
      OPEN(unit=92,file='basin_rain.csv',
     +  STATUS='OLD',IOSTAT=IOS)

      IF(IOS.NE.0)THEN
        !no basin rain file exists, use 'default' c05 behaviour
        BASINRAINFLAG = 0
        PRINT *, 'basin_rain.csv not found'
      ELSE
        !basin rain files does exist, use Pablo's code related
        !to rain
        BASINRAINFLAG = 1
        PRINT *, 'basin_rain.csv found'
      ENDIF

c *********************************************************************
c Open basin_temperature.csv
c *********************************************************************
      OPEN(unit=93,file='basin_temperature.csv',
     +  STATUS='OLD',IOSTAT=IOS)

      IF(IOS.NE.0)THEN
        !no basin temperature file exists, use 'default' c05 behaviour
        BASINTEMPERATUREFLAG = 0
        PRINT *, 'basin_temperature.csv not found'
      ELSE
        !basin temperature files does exist, use Pablo's code related
        !to temperature
        BASINTEMPERATUREFLAG = 1
        PRINT *, 'basin_temperature.csv found'
      ENDIF

c *********************************************************************
c Open basin_wind.csv
c *********************************************************************
      OPEN(unit=94,file='basin_wind.csv',
     +  STATUS='OLD',IOSTAT=IOS)

      IF(IOS.NE.0)THEN
        !no basin wind file exists, use 'default' c05 behaviour
        BASINWINDFLAG = 0
        PRINT *, 'basin_wind.csv not found'
      ELSE
        !basin wind files does exist, use Pablo's code related
        !to wind
        BASINWINDFLAG = 1
        PRINT *, 'basin_wind.csv found'
      ENDIF

c *********************************************************************
c Open basin_pres.csv
c *********************************************************************
      OPEN(unit=95,file='basin_pres.csv',
     +  STATUS='OLD',IOSTAT=IOS)

      IF(IOS.NE.0)THEN
        !no basin pres file exists, use 'default' c05 behaviour
        BASINPRESFLAG = 0
        PRINT *, 'basin_pres.csv not found'
      ELSE
        !basin pres files does exist, use Pablo's code related
        !to pres
        BASINPRESFLAG = 1
        PRINT *, 'basin_pres.csv found'
      ENDIF

c *********************************************************************
c Open basin_humidity.csv
c *********************************************************************
      OPEN(unit=96,file='basin_humidity.csv',
     +  STATUS='OLD',IOSTAT=IOS)

      IF(IOS.NE.0)THEN
        !no basin humidity file exists, use 'default' c05 behaviour
        BASINHUMIDITYFLAG = 0
        PRINT *, 'basin_humidity.csv not found'
      ELSE
        !basin humidity files does exist, use Pablo's code related
        !to humidity
        BASINHUMIDITYFLAG = 1
        PRINT *, 'basin_humidity.csv found'
      ENDIF


c *********************************************************************
c Open additional output files
c *********************************************************************
      OPEN(unit=85,file=".\" // GENDIR_OUT(1:INDEX(GENDIR_OUT," ")-1) //
     +                  '\basin_SCA_alldays.csv')
      OPEN(unit=86,file=".\" // GENDIR_OUT(1:INDEX(GENDIR_OUT," ")-1) //
     +                  '\basin_SWE_alldays.csv')

c *********************************************************************
c Open and read in values from MESH_parameters_CLASS.ini file
c *********************************************************************

      OPEN(UNIT=50,FILE='MESH_parameters_CLASS.ini',
     1             STATUS='OLD')

      READ (50,5010) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      READ (50,5010) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      READ (50,5010) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      READ(50,5020) DEGLAT,DEGLON,ZRFMGRD(1),ZRFHGRD(1),ZBLDGRD(1),
     1              GCGRD(1),ILW,NLTEST,NMTEST

c set NLTEST to wf_na no matter what value was read in from the CLASS.INI file
      NLTEST=WF_NA

      JLAT=NINT(DEGLAT)

      I=1
      DO M=1,NMTEST
        READ(50,5040) (FCANROW(1,M,J),J=1,ICAN+1),(LAMXROW(1,M,J),
     1                  J=1,ICAN)
        READ(50,5040) (LNZ0ROW(1,M,J),J=1,ICAN+1),(LAMNROW(1,M,J),
     1                  J=1,ICAN)
        READ(50,5040) (ALVCROW(1,M,J),J=1,ICAN+1),(CMASROW(1,M,J),
     1                  J=1,ICAN)
        READ(50,5040) (ALICROW(1,M,J),J=1,ICAN+1),(ROOTROW(1,M,J),
     1                  J=1,ICAN)
        READ(50,5030) (RSMNROW(1,M,J),J=1,ICAN),
     1                  (QA50ROW(1,M,J),J=1,ICAN)
        READ(50,5030) (VPDAROW(1,M,J),J=1,ICAN),
     1                  (VPDBROW(1,M,J),J=1,ICAN)
        READ(50,5030) (PSGAROW(1,M,J),J=1,ICAN),
     1                  (PSGBROW(1,M,J),J=1,ICAN)
        READ(50,5041) DRNROW(1,M),SDEPROW(1,M),FAREROW(1,M),
     1                  DDROW(1,M)
        READ(50,5090) XSLPROW(1,M),GRKFROW(1,M),MANNROW(1,M),
     1                  WFCIROW(1,M),MIDROW(1,M)
        READ(50,5080) (SANDROW(1,M,J),J=1,IGND) !soil layers
        READ(50,5080) (CLAYROW(1,M,J),J=1,IGND)
        READ(50,5080) (ORGMROW(1,M,J),J=1,IGND)
        READ(50,5050) (TBARROW(1,M,J),J=1,IGND),TCANROW(1,M),
     1                  TSNOROW(1,M),TPNDROW(1,M)
        READ(50,5060) (THLQROW(1,M,J),J=1,IGND),(THICROW(1,M,J),
     1                  J=1,IGND),ZPNDROW(1,M)
        READ(50,5070) RCANROW(1,M),SCANROW(1,M),SNOROW(1,M),
     1                  ALBSROW(1,M),RHOSROW(1,M),GROROW(1,M)
      ENDDO

!PARAMESH
      READ(50,*) HOURLY_START_DAY, HOURLY_STOP_DAY, DAILY_START_DAY,
     1           DAILY_STOP_DAY !P, IDAY_START, IDAY_END
      READ(50,*) HOURLY_START_YEAR, HOURLY_STOP_YEAR, DAILY_START_YEAR,
     1           DAILY_STOP_YEAR !P, IYEAR_START, IYEAR_END
C READ IN HOUR, MINUTE, DAY AND YEAR FROM CLASS.INI FILE AS IT IS
C NOT PRESENT IN THE MET FILES
      READ(50,5200) IHOUR,IMIN,IDAY,IYEAR

C CLOSE UNIT 50 AS WE DON'T NEED ANYTHING ELSE FROM THE CLASS.INI FILE
      CLOSE(UNIT=50)

c CLASS requires that each GRU for each grid square has its own parameter value,
c for MESH the value read in from the parameter file is assumed to be valid for
c all grid squares in the study area - Frank Seglenieks Aug 2007

c bjd - This would be a good spot for setting pre-distributed values

      DO I=2,NLTEST
        DO M=1,NMTEST
          DO J=1, ICP1
            FCANROW(I,M,J)=   FCANROW(1,M,J)
            LNZ0ROW(I,M,J)=   LNZ0ROW(1,M,J)
            ALVCROW(I,M,J)=   ALVCROW(1,M,J)
            ALICROW(I,M,J)=   ALICROW(1,M,J)
          ENDDO

          DO J=1, ICAN
            LAMXROW(I,M,J)=   LAMXROW(1,M,J)
            LAMNROW(I,M,J)=   LAMNROW(1,M,J)
            CMASROW(I,M,J)=   CMASROW(1,M,J)
            ROOTROW(I,M,J)=   ROOTROW(1,M,J)
            RSMNROW(I,M,J)=   RSMNROW(1,M,J)
            QA50ROW(I,M,J)=   QA50ROW(1,M,J)
            VPDAROW(I,M,J)=   VPDAROW(1,M,J)
            VPDBROW(I,M,J)=   VPDBROW(1,M,J)
            PSGAROW(I,M,J)=   PSGAROW(1,M,J)
            PSGBROW(I,M,J)=   PSGBROW(1,M,J)
          ENDDO

          DO J=1,IGND
            SANDROW(I,M,J)=   SANDROW(1,M,J)
            CLAYROW(I,M,J)=   CLAYROW(1,M,J)
            ORGMROW(I,M,J)=   ORGMROW(1,M,J)
          ENDDO

          DO J=1,IGND
            TBARROW(I,M,J)=   TBARROW(1,M,J)
            THLQROW(I,M,J)=   THLQROW(1,M,J)
            THICROW(I,M,J)=   THICROW(1,M,J)
          ENDDO

          TCANROW(I,M)=     TCANROW(1,M)
          TSNOROW(I,M)=     TSNOROW(1,M)
          DRNROW(I,M)=      DRNROW(1,M)
          SDEPROW(I,M)=     SDEPROW(1,M)
          FAREROW(I,M)=     FAREROW(1,M)
          MANNROW(I,M)=     MANNROW(1,M)
          XSLPROW(I,M)=     XSLPROW(1,M)
          GRKFROW(I,M)=     GRKFROW(1,M)
          DDROW(I,M)=       DDROW(1,M)
          WFSFROW(I,M)=     WFSFROW(1,M)
          WFCIROW(I,M)=     WFCIROW(1,M)
          MIDROW(I,M)=      MIDROW(1,M)
          TPNDROW(I,M)=     TPNDROW(1,M)
          ZPNDROW(I,M)=     ZPNDROW(1,M)
          RCANROW(I,M)=     RCANROW(1,M)
          SCANROW(I,M)=     SCANROW(1,M)
          SNOROW(I,M)=      SNOROW(1,M)
          ALBSROW(I,M)=     ALBSROW(1,M)
          RHOSROW(I,M)=     RHOSROW(1,M)
          GROROW(I,M)=      GROROW(1,M)
        ENDDO  !DO M=1,NMTEST
      ENDDO  !DO I=2,NLTEST


c *********************************************************************
c Open and read in values from soil.ini file
c
c Bruce Davison, August 13, 2004
c Changes to the soil parameters so that they're read-in directly.
c
c Read in the soil parameters that used to be calculated from %sand, %clay
c *********************************************************************

      OPEN(UNIT=23,file='soil.ini',status='old',iostat=SOIL_IOS)
C     * CHECK TO SEE IF THERE IS A new_soil.ini FILE

      IF(soil_ios.ne.0)THEN
        PRINT*,'The soil.ini file was NOT found'
        PRINT*,'%sand, %clay, %organic from class.ini will be used'
        PRINT*,'(i.e. CLASSB.f will be used)'
        PRINT*,'-----------------------------------'
      ELSE
        PRINT*,'The soil.ini file WAS found'
        PRINT*,'CLASSBHYD.f will be used'
        PRINT*,'-----------------------------------'
        READ (23,*)
        READ (23,"(10F8.5)") (wc_thpor (1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_thpor (1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_thpor (1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_thlret(1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_thlret(1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_thlret(1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_thlmin(1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_thlmin(1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_thlmin(1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_bi    (1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_bi    (1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_bi    (1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_psisat(1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_psisat(1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_psisat(1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10E12.5)") (wc_grksat(1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10E12.5)") (wc_grksat(1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10E12.5)") (wc_grksat(1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10E12.5)") (wc_hcps  (1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10E12.5)") (wc_hcps  (1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10E12.5)") (wc_hcps  (1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_tcs   (1,m,1),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_tcs   (1,m,2),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_tcs   (1,m,3),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_algwet(1,m),m=1,NMTEST)
        READ (23,*)
        READ (23,"(10F8.5)") (wc_algdry(1,m),m=1,NMTEST)
      ENDIF

      CLOSE(unit=23)



c     ==============================
c *********************************************************************
c Open and read INITIAL SOIL MOSITURE AND SOIL TEMPERATURE values
c when data is available
C files: S_moisture.txt : soil moisture in layer 1, 2 and 3
C files: T_temperature.txt : soil temperature in layer 1, 2 and 3
c *********************************************************************
C SOIL MOSITURE
      ALLOCATE (valuem(wf_imax,wf_jmax,ignd))
      OPEN(UNIT=59,FILE='s_moisture.txt',STATUS='old',IOSTAT=s_ios)
      IF(s_ios.eq.0)THEN
        DO J=1,IGND
          READ(59,*)
          DO iy=1,wf_imax
             READ(59,*)(valuem(iy,ix,j),ix=1,wf_jmax)
          ENDDO
        ENDDO
        DO I=1,NLTEST     !number of cells
          IY = WF_YY(I)
          JX = WF_XX(I)
          DO M=1,NMTEST   !number of classes
            DO J=1,IGND   !soil layers
              THLQROW(I,M,J)= valuem(IY,JX,J)
            ENDDO
          ENDDO
        ENDDO
      ELSE
         PRINT*,'...S_MOSITURE.TXT file do not exist...'
         PRINT*,'...Running without gridded initial soil moisture...'
      ENDIF
         CLOSE(59)
C
C SOIL TEMPERATURE
      ALLOCATE (valuet(wf_imax,wf_jmax,ignd))
      OPEN(UNIT=59,FILE='s_temperature.txt',STATUS='old',IOSTAT=s_ios)
      IF(s_ios.eq.0)THEN
        DO J=1,IGND
          READ(59,*)
          DO iy=1,wf_imax
             READ(59,*)(valuet(iy,ix,j),ix=1,wf_jmax)
          ENDDO
        ENDDO
        DO I=1,NLTEST     !number of cells
          IY = WF_YY(I)
          JX = WF_XX(I)
          DO M=1,NMTEST   !number of classes
            DO J=1,IGND   !soil layers
              TBARROW(I,M,J)=   valuet(IY,JX,J)
            ENDDO
          ENDDO
        ENDDO
      ELSE
         PRINT*,'...S_TEMPERATURE.TXT file do not exist...'
         PRINT*,'...Running without gridded initial soil temperature...'
      ENDIF
         CLOSE(59)
C     ==================================
C
c *********************************************************************
c Open and read in values from MESH_parameters_hydrology.ini file
c *********************************************************************

      OPEN(UNIT=23,FILE='MESH_parameters_hydrology.ini',
     +  STATUS='OLD',IOSTAT=IOS)

      IF(IOS.NE.0)THEN
        PRINT*,'The MESH_parameters_hydrology.ini file was NOT found'
        PRINT*,'Please create one and restart the program'
        PRINT*
        STOP
      ELSE

        DO I=1,3
          READ(23,*)
        ENDDO

        READ(23,"(I5)") OPTFLAGS

        IF(OPTFLAGS.GT.0) THEN
          DO I=1,OPTFLAGS
            READ(23,*)
          ENDDO
        ENDIF

        DO I=1,2
          READ(23,*)
        ENDDO

        READ (23,"(5F6.3)") (WF_R2(i),I=1,5)

        DO I=1,2
          READ(23,*)
        ENDDO

        READ(23,"(I8)") INDEPPAR
        IF(INDEPPAR.GT.0) THEN
          DO I=1,INDEPPAR
            READ(23,"(F8.3)")
          ENDDO
        ENDIF

        DO I=1,2
          READ(23,*)
        ENDDO

        READ(23,"(I8)") I
	    IF(I.NE.WF_NTYPE) THEN
          PRINT *, 'Number of GRUs in hydrology file: ',I
          PRINT *, 'Number of GRUs in drainage database: ',WF_NTYPE
          PRINT *, 'Please adjust these values.'
          STOP
	    ENDIF

        READ(23,"(I8)") DEPPAR
        READ(23,*)
        IF(DEPPAR.GT.0) THEN
          READ(23,*) (ZSNLROW(1,M),M=1,NMTEST)
          READ(23,*) (ZPLSROW(1,M),M=1,NMTEST)
          READ(23,*) (ZPLGROW(1,M),M=1,NMTEST)
        ENDIF

        DO I=2,NLTEST
          DO M=1,NMTEST
            ZSNLROW(I,M)=ZSNLROW(1,M)
            ZPLSROW(I,M)=ZPLSROW(1,M)
            ZPLGROW(I,M)=ZPLGROW(1,M)
          ENDDO
        ENDDO

        CLOSE(UNIT=23)

      ENDIF !IF(IOS.NE.0)THEN


c *********************************************************************
c Open and read in values from MESH_input_reservoir.txt file
c *********************************************************************

      OPEN(UNIT=21,FILE='MESH_input_reservoir.txt',STATUS='OLD')
	READ(21,'(3I5)') WF_NORESV,WF_NREL,WF_KTR
      WF_NORESV_CTRL=0

      IF( WF_NORESV.GT.0 ) THEN
        DO I=1,WF_NORESV
          READ(21,'(2I5,2G10.3,25X,A12,I2)') WF_IRES(I),WF_JRES(I),
     +      WF_B1(I),WF_B2(I),WF_RESNAME(I), WF_RES(I)
          WF_IRES(I)=INT((REAL(WF_IRES(I))-REAL(WF_IYMIN))/WF_GRDN+1.0)
          WF_JRES(I)=INT((REAL(WF_JRES(I))-REAL(WF_JXMIN))/WF_GRDE+1.0)
c check if point is in watershed and in river reaches
          WF_R(I)=0
          DO J=1,NLTEST
            IF( WF_IRES(I).EQ.WF_YY(J).AND.WF_JRES(I).EQ.WF_XX(J))THEN
              WF_R(I)=J
            ENDIF
          ENDDO
          IF(WF_R(I).EQ.0) THEN
            PRINT *, 'Reservoir Station: ',I,' is not in the basin'
	      PRINT *, 'Up/Down Coordinate: ', wf_ires(I)
	      PRINT *, 'Left/Right Coordinate: ', wf_jres(I)
            STOP
          ENDIF
          IF(WF_IREACH(WF_R(I)).NE.I) THEN
            PRINT *, 'Reservoir Station: ',I,
     +        ' is not in the correct reach'
	      PRINT *, 'Up/Down Coordinate: ', wf_ires(I)
	      PRINT *, 'Left/Right Coordinate: ', wf_jres(I)
	      PRINT *, 'ireach value at station: ', wf_iy(I)
            STOP
	    ENDIF
          IF( WF_B1(I).eq.0.0 ) THEN
            WF_NORESV_CTRL=WF_NORESV_CTRL+1
          ENDIF
        ENDDO
      ENDIF
c leave file open and read in the reservoir files when needed



c *********************************************************************
c Open and read in values from MESH_input_streamflow.txt file
c *********************************************************************

      OPEN(UNIT=22,FILE='MESH_input_streamflow.txt',STATUS='OLD')
      READ(22,*)
      READ(22,'(4I5)') WF_NO,WF_NL,WF_MHRD,WF_KT
      DO I=1,WF_NO
        READ(22,'(2I5,1X,A12)')WF_IY(I),WF_JX(I),WF_GAGE(I)
        WF_IY(I)=INT((REAL(WF_IY(I))-REAL(WF_IYMIN))/WF_GRDN+1.0)
        WF_JX(I)=INT((REAL(WF_JX(I))-REAL(WF_JXMIN))/WF_GRDE+1.0)

      ENDDO

      DO I=1,WF_NO
        WF_S(I)=0
        DO J=1,NLTEST
          IF( WF_IY(I).EQ.WF_YY(J).AND.WF_JX(I).EQ.WF_XX(J) ) THEN
            WF_S(I)=J
          ENDIF
        ENDDO
        IF(WF_S(I).EQ.0) THEN
          PRINT *, 'STREAMFLOW GAUGE: ',I,' IS NOT IN THE BASIN'
	    PRINT *, 'UP/DOWN COORDINATE: ', WF_IY(I)
	    PRINT *, 'LEFT/RIGHT COORDINATE: ', WF_JX(I)
          STOP
	  ENDIF
	
	  !note: this assumes streamflow data will be daily,
	  !if WF_KT is something other than 24 there will be a
	  !problem
	  !TODO: find out if WF_KT can/will be a value other than 24
	  nrs =( (IYEAR_START - IYEAR)*365 + (IDAY_START-IDAY) )
	  PRINT*, NRS
	  IF (NRS > 0) THEN
	    DO J=1, NRS
	      READ(22,*,IOSTAT=IOS)
	      IF (IOS < 0) THEN
	        PRINT *, 'ERROR: end of file reached when reading ',
     +          ' MESH_input_streamflow.txt, The start date in ',
     +          ' MESH_input_run_options.ini may be out of range'
              STOP
	      ENDIF
	    ENDDO
	  ENDIF

cric     initialise smoothed variables
        wf_qsyn(I)=0.0
	  wf_qhyd_avg(I)=0.0
      ENDDO

c leave unit open and read new streamflow each hour

c *********************************************************************
c Check to make sure input values are consistent
c *********************************************************************

c compare land classes in class.ini and drainage database files
      IF(WF_NTYPE.NE.NMTEST.AND.WF_NTYPE.GT.0) THEN
        PRINT *, 'land classes from MESH_parameters_CLASS.ini: ',NMTEST
        PRINT *, 'land classes from MESH_drainage_database.txt:',
     +            WF_NTYPE
        PRINT *, 'Please adjust these values.'
	  STOP
	ENDIF

c check if we are going to get an "array bounds out of range" error
      IF(WF_NA.GT.NLAT) THEN
        PRINT *, 'NLAT value in declaration statement : ',NLAT
        PRINT *, 'No. of grids from MESH_drainage_database.txt: ',WF_NA
        PRINT *, 'Please adjust these values.'
	  STOP
	ENDIF

      IF(WF_NTYPE.GT.NMOS) THEN
        PRINT *, 'NMOS value in declaration statement : ',NMOS
        PRINT *, 'No. of grids from MESH_drainage_database.txt: ',
     +            WF_NTYPE
        PRINT *, 'Please adjust these values.'
	  STOP
	ENDIF

c check that run points are in the basin and that there are no repeats

      DO I=1, WF_NUM_POINTS
        IF(N_OUT(I).GT.NLTEST) THEN
         PRINT *, 'No. of grids from MESH_drainage_database.txt:',NLTEST
         PRINT *, 'out point ',i,' is: ',N_OUT(I)
         PRINT *, 'please adjust MESH_run_options.ini file'
	   STOP
	  ENDIF

        IF(I.LT.WF_NUM_POINTS) THEN
	    DO J=I+1,WF_NUM_POINTS
            IF(N_OUT(I).EQ.N_OUT(J)) THEN
	        PRINT *, 'grid number ', n_out(i)
	        PRINT *, 'is repeated in MESH_run_options.ini file'
              PRINT *, 'please adjust MESH_run_options.ini file'
	        STOP
	      ENDIF
          ENDDO
	  ENDIF

	ENDDO



c *********************************************************************
c Set some more intial values and clear accumulators
c *********************************************************************

c assign values of lat/long to each square
! there are a bunch of magic numbers here, this should get cleaned up
! or documented
      DO I=1, NLTEST
        LATLENGTH=WF_AL/1000./(111.136-0.5623*COS(2*(DEGLAT*PI/180.0))+
     +  0.0011*COS(4*(DEGLAT*PI/180.0)))
        LONGLENGTH=WF_AL/1000./(111.4172*COS((DEGLAT*PI/180.0))-
     +  0.094*COS(3*(DEGLAT*PI/180.0))+0.0002*COS(5*(DEGLAT*PI/180.0)))
        RADJGRD(I)=( (DEGLAT-(REAL(WF_IMAX)/2.0)*LATLENGTH)
     +  +(WF_YY(I)-0.5)*LATLENGTH)*PI/180.
        DLONGRD(I)=(DEGLON-(REAL(WF_JMAX)/2.0)*LONGLENGTH)
     +  +(WF_XX(I)-0.5)*LONGLENGTH
        ZRFMGRD(I)=ZRFMGRD(1)
        ZRFHGRD(I)=ZRFHGRD(1)
        ZBLDGRD(I)=ZBLDGRD(1)
        GCGRD(i)=GCGRD(1)
        Z0ORGRD(I)=0.0
        GGEOGRD(I)=0.0
        ZDMGRD(I)=10.0
        ZDHGRD(I)=2.0
      ENDDO

c adjust wf_naa to the be number of outlet squares
      WF_NAA=NLTEST-WF_NAA

c set initial values of ncount and nsum
! NCOUNT = which half-hour period the current time is:
! The first period (0:00-0:30) is #1, the last period (23:30-0:00) is #48
      NCOUNT=IHOUR*2+IMIN/30+1
      NSUM=1

c Set value of FAREROW

      TOTAL_AREA=0.0
      DO I=1,NLTEST
        DO M=1,NMTEST
          FAREROW(I,M)= WF_ACLASS(I,M) * WF_FRAC(I)
          TOTAL_AREA=TOTAL_AREA+FAREROW(I,M)
          !FUTUREDO: Bruce, WF_FRAC is calculated by EnSim
          ! using Dan Princz's instructions for EnSim
          ! WF_FRAC can be greater than 1.00
          ! So, we cannot use FAREROW in place of BASIN_FRACTION
        ENDDO
      ENDDO

c routing parameters
      WF_ROUTETIMESTEP=900
      WF_TIMECOUNT=0
      DRIVERTIMESTEP=DELT    ! Be sure it's REAL*8
	JAN=1  !jan=1 first time through, jan=2 after that

      DO I=1,NLTEST
        DO M=1,NMTEST
          DO J=1,IGND
            TBARROW(I,M,J)=TBARROW(I,M,J)+TFREZ
          END DO
          TSNOROW(I,M)=TSNOROW(I,M)+TFREZ
          TCANROW(I,M)=TCANROW(I,M)+TFREZ
          TPNDROW(I,M)=TPNDROW(I,M)+TFREZ
          TBASROW(I,M)=TBARROW(I,M,3)
          CMAIROW(I,M)=0.
          WSNOROW(I,M)=0.
          TSFSROW(I,M,1)=TFREZ
          TSFSROW(I,M,2)=TFREZ
          TSFSROW(I,M,3)=TBARROW(I,M,1)
          TSFSROW(I,M,4)=TBARROW(I,M,1)
          TACROW (I,M)=TCANROW(I,M)
          QACROW (I,M)=0.5E-2
          IF(IGND.GT.3)THEN ! should stay this way to work with class
            DO J=4,IGND
              THLQROW(I,M,J)=THLQROW(I,M,3)
              THICROW(I,M,J)=THICROW(I,M,3)
              TBARROW(I,M,J)=TBARROW(I,M,3)
              IF(SDEPROW(I,M).LT.(ZBOT(J-1)+0.001) .AND.
     1          SANDROW(I,M,3).GT.-2.5)THEN
                SANDROW(I,M,J)=-3.0
                CLAYROW(I,M,J)=-3.0
                ORGMROW(I,M,J)=-3.0
              ELSE
                SANDROW(I,M,J)=SANDROW(I,M,3)
                CLAYROW(I,M,J)=CLAYROW(I,M,3)
                ORGMROW(I,M,J)=ORGMROW(I,M,3)
              ENDIF
            ENDDO
          ENDIF
          DO K=1,6
            DO L=1,50
              ITCTROW(I,M,K,L)=0
            ENDDO
          ENDDO

        ENDDO !DO M=1,NMTEST
      ENDDO !DO I=1,NLTEST

c clear accumulating variables
      TOTAL_ROFACC=0.0
      TOTAL_ROFOACC=0.0
      TOTAL_ROFSACC=0.0
      TOTAL_ROFBACC=0.0
      TOTAL_EVAPACC=0.0
      TOTAL_PREACC=0.0

      DO I=1,NLTEST
        PREACC(I)=0.
        GTACC(I)=0.
        QEVPACC(I)=0.
        EVAPACC(I)=0.
        HFSACC(I)=0.
        HMFNACC(I)=0.
        ROFACC(I)=0.
        ROFOACC(I)=0.
        ROFSACC(I)=0.
        ROFBACC(I)=0.
        WTBLACC(I)=0.
        ALVSACC(I)=0.
        ALIRACC(I)=0.
        RHOSACC(I)=0.
        SNOACC(I)=0.
        WSNOACC(I)=0.
        CANARE(I)=0.
        SNOARE(I)=0.
        TSNOACC(I)=0.
        TCANACC(I)=0.
        RCANACC(I)=0.
        SCANACC(I)=0.
        GROACC(I)=0.
        FSINACC(I)=0.
        FLINACC(I)=0.
        FLUTACC(I)=0.
        TAACC(I)=0.
        UVACC(I)=0.
        PRESACC(I)=0.
        QAACC(I)=0.
        DO J=1,IGND
          TBARACC(I,J)=0.
          THLQACC(I,J)=0.
          THICACC(I,J)=0.
          THALACC(I,J)=0.
        ENDDO
      ENDDO

c ******************************************************
c echo print information to MESH_output_echo_print.txt
c ******************************************************

      OPEN(UNIT=58,FILE=".\" // GENDIR_OUT(1:INDEX(GENDIR_OUT," ")-1) //
     +                  '\MESH_output_echo_print.txt')

      WRITE(58,"('MESH_input_run_options.ini')")
      WRITE(58,*)
      WRITE(58,"('Configuration flags:')")
      IF(CONFLAGS.GT.0) THEN
        DO I=1,CONFLAGS
          WRITE (58,*)
        ENDDO
      ENDIF
	WRITE(58,"('WF_NUM_POINTS: ',I5)") WF_NUM_POINTS
      WRITE(58,"('Out directory:',5A10)") (DIR_OUT(I),I=1,WF_NUM_POINTS)
      WRITE(58,"('Grid number:  ',5I10)") (N_OUT(I),I=1,WF_NUM_POINTS)
      WRITE(58,"('Land class:   ',5I10)") (II_OUT(I),I=1,WF_NUM_POINTS)
      WRITE (58,*)

      WRITE(58,"('MESH_parameters_hydrology.ini')")
      WRITE(58,*)
      WRITE(58,"('Option flags:')")
      IF(OPTFLAGS.GT.0) THEN
        DO I=1,OPTFLAGS
          WRITE (58,*)
        ENDDO
      ENDIF
      WRITE(58,"('River roughnesses:')")
      WRITE(58,"(5F6.3)") (WF_R2(I),I=1,5) ! MAGIC NUMBER
      WRITE(58,"('Land class independent hydrologic parameters:')")
      IF(INDEPPAR.GT.0) THEN
        DO I=1,INDEPPAR
          WRITE (58,*)
        ENDDO
      ENDIF
      WRITE(58,"('Land class dependent hydrologic parameters:')")
      IF(DEPPAR.GT.0) THEN
      WRITE(NMTESTFORMAT, "(A10,I3,'F10.2)')") "('ZSNLROW'", NMTEST
      WRITE(58,NMTESTFORMAT) (ZSNLROW(1,M),M=1,NMTEST)
      WRITE(NMTESTFORMAT, "(A10,I3,'F10.2)')") "('ZPLSROW'", NMTEST
      WRITE(58,NMTESTFORMAT) (ZPLSROW(1,M),M=1,NMTEST)
      WRITE(NMTESTFORMAT, "(A10,I3,'F10.2)')") "('ZPLGROW'", NMTEST
      WRITE(58,NMTESTFORMAT) (ZPLGROW(1,M),M=1,NMTEST)
      ENDIF

      WRITE (58,*)


      WRITE(58,"('MESH_parameters_CLASS.ini')")
      WRITE(58,*)
      WRITE(58,5010) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(58,5010) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(58,5010) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(58,5020) DEGLAT,DEGLON,ZRFMGRD(1),ZRFHGRD(1),ZBLDGRD(1),
     1              GCGRD(1),ILW,NLTEST,NMTEST
      I=1
      DO M=1,NMTEST
      WRITE(58,5040) (FCANROW(I,M,J),J=1,ICAN+1),(LAMXROW(I,M,J),
     +	J=1,ICAN)
      WRITE(58,5040) (LNZ0ROW(I,M,J),J=1,ICAN+1),(LAMNROW(I,M,J),
     1                  J=1,ICAN)
      WRITE(58,5040) (ALVCROW(I,M,J),J=1,ICAN+1),(CMASROW(I,M,J),
     1                  J=1,ICAN)
      WRITE(58,5040) (ALICROW(I,M,J),J=1,ICAN+1),(ROOTROW(I,M,J),
     1                  J=1,ICAN)
      WRITE(58,5030) (RSMNROW(I,M,J),J=1,ICAN),
     1                  (QA50ROW(I,M,J),J=1,ICAN)
      WRITE(58,5030) (VPDAROW(I,M,J),J=1,ICAN),
     1                  (VPDBROW(I,M,J),J=1,ICAN)
      WRITE(58,5030) (PSGAROW(I,M,J),J=1,ICAN),
     1                  (PSGBROW(I,M,J),J=1,ICAN)
      WRITE(58,5042) DRNROW(I,M),SDEPROW(I,M),FAREROW(I,M),
     1                  DDROW(I,M)
      WRITE(58,5090) XSLPROW(I,M),GRKFROW(I,M),MANNROW(I,M),
     1                  WFCIROW(I,M),MIDROW(I,M)
      WRITE(58,5080) (SANDROW(I,M,J),J=1,IGND)
      WRITE(58,5080) (CLAYROW(I,M,J),J=1,IGND)
      WRITE(58,5080) (ORGMROW(I,M,J),J=1,IGND)
      WRITE(58,5050) (TBARROW(I,M,J),J=1,IGND),TCANROW(I,M),
     1                  TSNOROW(I,M),TPNDROW(I,M)
      WRITE(58,5060) (THLQROW(I,M,J),J=1,IGND),(THICROW(I,M,J),
     1                  J=1,IGND),ZPNDROW(I,M)
      WRITE(58,5070) RCANROW(I,M),SCANROW(I,M),SNOROW(I,M),
     1                  ALBSROW(I,M),RHOSROW(I,M),GROROW(I,M)
      WRITE(58,*)
      ENDDO


c *********************************************************************
c Open the MESH_input_forcing.bin file
c *********************************************************************

      OPEN(UNIT=51,FILE='MESH_input_forcing.bin',STATUS='OLD',
     +           FORM='UNFORMATTED')

      !IYEAR is set in the MESH_parameters_CLASS.ini file
      !IYEAR_START is set in the MESH_input_run_options.ini file
!P      !IYEAR_START is set in the MESH_parameters_class.ini file

      !the following code is used to skip entries at the start
      !of the bin file

!PARAMESH

      nyy = IYEAR_START - IYEAR
      ndy = IDAY_START  - IDAY
      nmy = IMIN_START  - IMIN !P
      nhy = IHOUR_START - IHOUR !P
!P      nmy = IMIN
!P      nhy = 24 - IHOUR
      ! set ISTEP_START based on HOURLYFLAG
      !  (could be optimised as ISTEP_START = 2 - HOURLYFLAG)
      IF (HOURLYFLAG == 1) THEN
        ISTEP_START = 1
      ELSE
        ISTEP_START = 2
      ENDIF
      nrs =(nyy*365+ndy)*24*istep_START + nhy*istep_START + nmy/30 !P
!P      nrs =(nyy*365+ndy)*24*istep_START + nhy*istep_START - nmy/30
      IF (IYEAR_START == 0 .AND. IDAY_START == 0 .AND. IMIN_START == 0
     +    .AND. IHOUR_START == 0) THEN !P
!P      IF ((IYEAR_START == 0 .AND. IDAY_START == 0) .OR.
!P     1    (IYEAR_START == IYEAR .AND. IDAY_START == IDAY)) THEN
        !Special case, all 0s. Automatically start at the beginning.
!P        Another special case, starting date is the start date
        IYEAR_START = IYEAR
        IDAY_START = IDAY
        IHOUR_START = IHOUR !P
        IMIN_START = IMIN !P
        nrs = 0
      ELSEIF (nrs.lt.0) THEN
        PRINT*,'Desired start date is before the start of the ',
     +    'data in MESH_input_forcing.bin'
        PRINT *, 'Please adjust the start date in ',
     +    'MESH_input_run_options.ini'
        STOP
      ENDIF

      PRINT *, 'Skipping',NRS,'Registers in bin file'
      IYEAR = IYEAR_START
      IDAY = IDAY_START
!      IHOUR = IHOUR_START
!      IMIN = IMIN_START

      !calculate number of data values that will be in the file
      toskip = BASINSHORTWAVEFLAG + BASINLONGWAVEFLAG + BASINRAINFLAG +
     +         BASINTEMPERATUREFLAG + BASINWINDFLAG + BASINPRESFLAG +
     +         BASINHUMIDITYFLAG
      toskip = 7 - toskip
      DO i=1,nrs
        DO J=1,toskip
          READ(51,END=999)
        ENDDO
      ENDDO



c *********************************************************************
c Open and print header information to the output files
c *********************************************************************
      OPEN(UNIT=70,FILE=".\" // GENDIR_OUT(1:INDEX(GENDIR_OUT," ")-1) //
     +                  '\MESH_output_streamflow.csv')

c Set up the CLASSOF* files to print out into the correct directory
      DO I=1, wf_num_points
        BNAM=DIR_OUT(i)
        OPEN(UNIT=150+i*10+1,FILE=".\"//BNAM(1:INDEX(BNAM," ")-1)//
     +   "\CLASSOF1.csv")
        OPEN(UNIT=150+i*10+2,FILE=".\"//BNAM(1:INDEX(BNAM," ")-1)//
     +   "\CLASSOF2.csv")
        OPEN(UNIT=150+i*10+3,FILE=".\"//BNAM(1:INDEX(BNAM," ")-1)//
     +   "\CLASSOF3.csv")
        OPEN(UNIT=150+i*10+4,FILE=".\"//BNAM(1:INDEX(BNAM," ")-1)//
     +   "\CLASSOF4.csv")
        OPEN(UNIT=150+i*10+5,FILE=".\"//BNAM(1:INDEX(BNAM," ")-1)//
     +   "\CLASSOF5.csv")
        OPEN(UNIT=150+i*10+6,FILE=".\"//BNAM(1:INDEX(BNAM," ")-1)//
     +   "\CLASSOF6.csv")
        OPEN(UNIT=150+i*10+7,FILE=".\"//BNAM(1:INDEX(BNAM," ")-1)//
     +   "\CLASSOF7.csv")
        OPEN(UNIT=150+i*10+8,FILE=".\"//BNAM(1:INDEX(BNAM," ")-1)//
     +   "\CLASSOF8.csv")
        OPEN(UNIT=150+i*10+9,FILE=".\"//BNAM(1:INDEX(BNAM," ")-1)//
     +   "\CLASSOF9.csv")

        WRITE(150+i*10+1,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(150+i*10+1,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(150+i*10+1,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(150+i*10+1,6011)
 6011   FORMAT('IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,BEG,GTOUT,
     1   SNOACC(I),RHOSACC(I),WSNOACC(I),ALTOT,ROFACC(I),ROFOACC(I),
     2   ROFSACC(I),ROFBACC(I)')
        WRITE(150+i*10+2,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(150+i*10+2,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(150+i*10+2,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(150+i*10+2,6012)
 6012   FORMAT('IDAY,IYEAR,TBARACC(I 1)-TFREZ,THLQACC(I 1),THICACC(I 1),
     1   TBARACC(I 2)-TFREZ,THLQACC(I 2),THICACC(I 2),
     2   TBARACC(I 3)-TFREZ,THLQACC(I 3),THICACC(I 3),TCN,RCANACC(I),
     3   SCANACC(I),TSN,ZSN')
        WRITE(150+i*10+3,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(150+i*10+3,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(150+i*10+3,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(150+i*10+3,6013)
 6013   FORMAT('IDAY,IYEAR,FSINACC(I),FLINACC(I),TAACC(I)-TFREZ,
     1   UVACC(I),PRESACC(I),QAACC(I),PREACC(I),EVAPACC(I)')
        WRITE(150+i*10+4,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(150+i*10+4,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(150+i*10+4,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(150+i*10+4,6014)
 6014   FORMAT('IHOUR,IMIN,IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,BEG,
     1   GTOUT,SNOROW(I M),RHOSROW(I M),WSNOROW(I M),ALTOT,ROFROW(I M),
     2   TPN,ZPNDROW(I M)')
        WRITE(150+i*10+5,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(150+i*10+5,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(150+i*10+5,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(150+i*10+5,6015)
 6015   FORMAT('IHOUR,IMIN,IDAY,IYEAR,TBARROW(I M 1)-TFREZ,
     1   THLQROW(I M 1),THICROW(I M 1),TBARROW(I M 2)-TFREZ,
     2   THLQROW(I M 2),THICROW(I M 2),TBARROW(I M 3)-TFREZ,
     3   THLQROW(I M 3),THICROW(I M 3),TCN,RCANROW(I M),SCANROW(I M),
     4   TSN,ZSN')
        WRITE(150+i*10+6,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(150+i*10+6,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(150+i*10+6,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(150+i*10+6,6016)
 6016   FORMAT('IHOUR,IMIN,IDAY,FSDOWN(I),FDLGRD(I),PREGRD(I),
     1   TAGRD(I)-TFREZ,UVGRD(I),PRESGRD(I),QAGRD(I)')
        WRITE(150+i*10+7,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(150+i*10+7,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(150+i*10+7,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(150+i*10+7,6017)
 6017   FORMAT('TROFROW(I M),TROOROW(I M),TROSROW(I M),TROBROW(I M),
     1   ROFROW(I M),ROFOROW(I M),ROFSROW(I M),ROFBROW(I M),
     2   FCS(I),FGS(I),FC(I),FG(I)')
        WRITE(150+i*10+8,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(150+i*10+8,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(150+i*10+8,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(150+i*10+8,6018)
 6018   FORMAT('FSGVROW(I M),FSGSROW(I M),FSGGROW(I M),FLGVROW(I M),
     1   FLGSROW(I M),FLGGROW(I M),HFSCROW(I M),HFSSROW(I M),
     2   HFSGROW(I M),HEVCROW(I M),HEVSROW(I M),HEVGROW(I M),
     3   HMFCROW(I M),HMFNROW(I M),HMFGROW(I M 1),HMFGROW(I M 2),
     4   HMFGROW(I M 3),HTCCROW(I M),HTCSROW(I M),HTCROW(I M 1),
     5   HTCROW(I M 2),HTCROW(I M 3)')
        WRITE(150+i*10+9,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(150+i*10+9,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(150+i*10+9,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(150+I*10+9,6019)
 6019   FORMAT('PCFCROW(I M),PCLCROW(I M),PCPNROW(I M),PCPGROW(I M),
     1   QFCFROW(I M),QFCLROW(I M),QFNROW(I M),QFGROW(I M),
     2   QFCROW(I M 1),QFCROW(I M 2),QFCROW(I M 3),ROFCROW(I M),
     3   ROFNROW(I M),ROFOROW(I M),ROFROW(I M),WTRCROW(I M),
     4   WTRSROW(I M),WTRGROW(I M)')
      ENDDO


c *********************************************************************
c Open and read in values from wfo_spec.txt file
c *********************************************************************

C     * ENSIM: Open wfo_spec.txt to read in which parameters to output
        OPEN(unit=56,file='wfo_spec.txt',status='old',iostat=ensim_ios)

        IF(ensim_ios.eq.0)THEN
          READ(56,'(I5)',iostat=ios)nj
          READ(56,'(I5)',iostat=ios)ireport

C         Allocate the necessary arrays
          ALLOCATE (wfo_pick(nj))
          ALLOCATE (wfo_attributes(nj))

          IF(ios.ne.0)THEN
            PRINT*,' Problem reading the first two lines of the'
            PRINT*,' wfo_spec.txt file'
            PRINT*
            STOP
          ENDIF
          PRINT*,' WFO_SPEC.TXT file found,'
          PRINT*,' The following EnSim output will be written:'

          DO j=1,nj
            READ(56,'(i1,5x,a50)',iostat=ios)wfo_pick(j),
     +        wfo_attributes(j)
            IF(ios.ne.0)THEN
              PRINT*,'iostat code =',ios
              PRINT*,' Read to line ',j,' in wfo_spec.txt'
              PRINT*,' then a problem was found'
              STOP
            ENDIF
            IF(wfo_pick(j).eq.1)THEN
              PRINT*,wfo_attributes(j)
            ENDIF
          ENDDO
          CLOSE(unit=56,status='keep')
        ELSE
          PRINT*,'WFO_SPEC.TXT file not found, no EnSim output'//
     +           ' will be written'
        ENDIF
        PRINT *


!>>>>>>>>>>>>>>>>>> AB:  ENSIM HEADERS
C
C
C         WRITE THE HEADER FOR ENSIM FILES:

      IF(ensim_ios.eq.0)THEN
       CALL write_both_headers('UTM     ',wf_jmax,wf_imax,3000.0,
     *         477000.0,6699000.0,NMTEST,jan,wfo_pick,wf_landclassname,
     *         NMOS)
      ENDIF
C
C For the ENSIM timestamp
      wfo_seq=0

C End of ENSIM Changes




c *********************************************************************
c Output information to screen
c *********************************************************************

      PRINT *, 'NUMBER OF GRID SQUARES: ',NLTEST
	PRINT *, 'NUMBER OF LAND CLASSES (WITH IMPERVIOUS): ', NMTEST
	PRINT *, 'NUMBER OF RIVER CLASSES: ', WF_NRIV
	PRINT *, 'MINIMUM NUMBER FOR ILG: ',NLTEST*NMTEST
      PRINT *, 'NUMBER OF GRID SQUARES IN X DIRECTION: ', WF_IMAX
      PRINT *, 'NUMBER OF GRID SQUARES IN Y DIRECTION: ', WF_JMAX
      PRINT *, 'LENGTH OF SIDE OF GRID SQUARE IN M: ', WF_AL
	PRINT *, 'NUMBER OF DRAINAGE OUTLETS: ', WF_NAA
	PRINT *, 'NUMBER OF STREAMFLOW GUAGES: ', WF_NO
      DO I=1,WF_NO
        PRINT *,'STREAMFLOW STATION: ',I,'I: ',WF_IY(I),'J: ',WF_JX(I)
      ENDDO
	PRINT *, 'NUMBER OF RESERVOIR STATIONS: ', WF_NORESV
      IF( WF_NORESV.GT.0 ) THEN
        DO I=1,WF_NORESV
       PRINT *,'RESERVOIR STATION: ',I,'I: ',WF_IRES(I),'J: ',WF_JRES(I)
        ENDDO
      ENDIF

      PRINT *
      PRINT *, 'Found these output locations:'
      PRINT *, 'Output Directory, grid number, land class number'
      DO I=1, WF_NUM_POINTS
        PRINT *, DIR_OUT(I),N_OUT(I),II_OUT(I)
      ENDDO
      PRINT *


	PRINT *
	PRINT *, 'DONE INTITIALIZATION'
      PRINT *
	PRINT *, 'STARTING MESH'

5010  FORMAT(2X,6A4)
5020  FORMAT(5F10.2,F7.1,3I5)
5030  FORMAT(4F8.3,8X,4F8.3)
5040  FORMAT(9F8.3)
5041  FORMAT(3F8.3,F8.2)
5042  FORMAT(3F8.3,F8.4)
5050  FORMAT(6F10.2)
5060  FORMAT(7F10.3)
5070  FORMAT(2F10.4,F10.2,F10.3,F10.4,F10.3)
5080  FORMAT(3F10.1)
5090  FORMAT(4E8.1,I8)
5200  FORMAT(4I10)
5300  FORMAT(1X,I2,I3,I5,I6,2F9.2,E14.4,F9.2,E12.3,F8.2,F12.2,3F9.2,
     1       F9.4)
6001  FORMAT('CLASS TEST RUN:     ',6A4)
6002  FORMAT('RESEARCHER:         ',6A4)
6003  FORMAT('INSTITUTION:        ',6A4)



c *********************************************************************
c Call CLASSB to set more CLASS variables
c *********************************************************************
!bjd - July 25, 2005: For inputting field measured soil properties.

      IF(soil_ios.ne.0) THEN

      CALL CLASSB(THPROW,THRROW,THMROW,BIROW,PSISROW,GRKSROW,
     1            THRAROW,HCPSROW,TCSROW,THFCROW,PSIWROW,
     2            DLZWROW,ZBTWROW,ALGWROW,ALGDROW,
     3            SANDROW,CLAYROW,ORGMROW,DELZ,ZBOT,
     4            SDEPROW,ISNDROW,
     5            IORG,NLAT,NMOS,NLTEST,NMTEST,IGND)

      ELSE

      CALL CLASSBHYD(THPROW,THRROW,THMROW,BIROW,PSISROW,GRKSROW,
     1            THRAROW,HCPSROW,TCSROW,THFCROW,PSIWROW,
     2            DLZWROW,ZBTWROW,ALGWROW,ALGDROW,
     3            SANDROW,CLAYROW,ORGMROW,DELZ,ZBOT,
     4            SDEPROW,ISNDROW,
     5            IORG,NLAT,NMOS,NLTEST,NMTEST,IGND,wc_thpor,wc_thlret,
     6            wc_thlmin,wc_bi,wc_psisat,wc_grksat,wc_hcps,wc_tcs,
     7            wc_algwet,wc_algdry)

      ENDIF

c *********************************************************************
c Start of main loop that is run each half hour
c *********************************************************************



200   CONTINUE

C
c *********************************************************************
c Read in Meteorological forcing data
c *********************************************************************

C     * READ IN METEOROLOGICAL FORCING DATA FOR CURRENT TIME STEP;
C     * CALCULATE SOLAR ZENITH ANGLE AND COMPONENTS OF INCOMING SHORT-
C     * WAVE RADIATION FLUX; ESTIMATE FLUX PARTITIONS IF NECESSARY.
C
      N=N+1

      IF( (HOURLYFLAG == 1 .AND. IMIN.EQ.0) !hourly forcing data
     +  .OR.
     +     HOURLYFLAG == 0) THEN !half-hourly forcing data

        IF(BASINSHORTWAVEFLAG.EQ.0)THEN !use c05 code
          READ(51,END=999) ((R4SHRTGRID2D(I,J),J=1,WF_JMAX),I=1,WF_IMAX)
        ENDIF
        IF(BASINLONGWAVEFLAG.EQ.0)THEN !use c05 code
          READ(51,END=999) ((R4LONGGRID2D(I,J),J=1,WF_JMAX),I=1,WF_IMAX)
        ENDIF
        IF(BASINRAINFLAG.EQ.0)THEN !use c05 code
          READ(51,END=999) ((R4RAINGRID2D(I,J),J=1,WF_JMAX),I=1,WF_IMAX)
        ENDIF
        IF(BASINTEMPERATUREFLAG.EQ.0)THEN !use c05 code
          READ(51,END=999) ((R4TEMPGRID2D(I,J),J=1,WF_JMAX),I=1,WF_IMAX)
        ENDIF
        IF(BASINWINDFLAG.EQ.0)THEN !use c05 code
          READ(51,END=999) ((R4WINDGRID2D(I,J),J=1,WF_JMAX),I=1,WF_IMAX)
        ENDIF
        IF(BASINPRESFLAG.EQ.0)THEN !use c05 code
          READ(51,END=999) ((R4PRESGRID2D(I,J),J=1,WF_JMAX),I=1,WF_IMAX)
        ENDIF
        IF(BASINHUMIDITYFLAG.EQ.0)THEN !use c05 code
          READ(51,END=999) ((R4HUMDGRID2D(I,J),J=1,WF_JMAX),I=1,WF_IMAX)
        ENDIF
        DO I=1,NLTEST
          IY = WF_YY(I)
          JX = WF_XX(I)
          IF(BASINSHORTWAVEFLAG.EQ.0)THEN !use c05 code
            FSDOWN(I)=R4SHRTGRID2D(IY,JX)
            FSVHGRD(I)=0.5*R4SHRTGRID2D(IY,JX)
            FSIHGRD(I)=FSVHGRD(I)
          ENDIF
          IF (BASINLONGWAVEFLAG == 0) THEN !use c05 code
            FDLGRD(I)=R4LONGGRID2D(IY,JX)
          ENDIF
          IF (BASINRAINFLAG == 0) THEN !use c05 code
            PREGRD(I)=R4RAINGRID2D(IY,JX) !c05
          ENDIF
!c         to convert precipitation in mm/s - S.M. Feb 25, 2008
!         PREGRD(I)=R4RAINGRID2D(IY,JX)*ISTEP_START/3600
          IF (BASINTEMPERATUREFLAG == 0) THEN !use c05 code
            TAGRD(I)=R4TEMPGRID2D(IY,JX)
          ENDIF
          IF (BASINWINDFLAG == 0) THEN !use c05 code
            ULGRD(I)=R4WINDGRID2D(IY,JX)
          ENDIF
          VLGRD(I)=0.0
          UVGRD(I)=MAX(VMIN,ULGRD(I))
          IF (BASINPRESFLAG == 0) THEN !use c05 code
            PRESGRD(I)=R4PRESGRID2D(IY,JX)
          ENDIF
          IF (BASINHUMIDITYFLAG == 0) THEN !use c05 code
            QAGRD(I)=R4HUMDGRID2D(IY,JX)
          ENDIF
        END DO
      ENDIF

c *********************************************************************
c Read in current reservoir release value
c *********************************************************************


c only read in current value if we are on the correct time step
c however put in an exception if this is the first time through (ie. jan=1),
c otherwise depending on the hour of the first time step
c there might not be any data in wf_qrel, wf_qhyd
c make sure we have a controlled reservoir (if not the mod(IHOUR,wf_ktr)
c may give an error Frank S Jun 2007
      IF(WF_NORESV_CTRL.GT.0) THEN
        IF(MOD(IHOUR,WF_KTR).EQ.0.AND.IMIN.EQ.0) THEN
c         READ in current reservoir value
          READ(21,'(100F10.3)',IOSTAT=IOS)(WF_QREL(I),
     +		I=1,WF_NORESV_CTRL)
          IF(IOS.NE.0) THEN
            PRINT *, 'ran out of reservoir data before met data'
            STOP
	    ENDIF
        ELSE
          IF (JAN.EQ.1.AND.WF_NORESV_CTRL.GT.0) THEN
            READ(21,'(100F10.3)',IOSTAT=IOS)(WF_QREL(I),
     +	      I=1,WF_NORESV_CTRL)
            REWIND 21
            READ(21,*)
            DO I=1,WF_NORESV
              READ(21,*)
   	      ENDDO
          ENDIF
        ENDIF
      ENDIF

c *********************************************************************
c Read in current streamflow value
c *********************************************************************

c only read in current value if we are on the correct time step
c also read in the first value if this is the first time through
      IF(MOD(IHOUR,WF_KT).EQ.0.AND.IMIN.EQ.0) THEN
c       read in current streamflow value
        READ(22,'(100F10.3)',IOSTAT=IOS) (WF_QHYD(I),I=1,WF_NO)
        IF(IOS.NE.0) THEN
          PRINT *, 'ran out of streamflow data before met data'
	    STOP
		ENDIF
      ELSE
	    IF(JAN.EQ.1) THEN
          READ(22,'(100F10.3)',IOSTAT=IOS) (WF_QHYD(I),I=1,WF_NO)
          REWIND 22
          READ(22,*)
          READ(22,*)
	    DO I=1,WF_NO
            READ(22,*)
          ENDDO
	    ENDIF
      ENDIF

c *********************************************************************
c Set some more CLASS parameters
c *********************************************************************

      !This estimates the fractional cloud cover (FCLOGRD) by the basis
      ! of the solar zenith angle and the occurrence of precipitation.
      ! Assumed to be 1 (100%) when precipitation occurs and somewhere
      ! in the range of [0.1,1] based on the location of the sun in the
      ! sky when precipitation is not occuring. (0.1 when the sun is at
      ! the zenith, 1 when the sun is at the horizon).
      DAY=REAL(IDAY)+(REAL(IHOUR)+REAL(IMIN)/60.)/24.
      DECL=SIN(2.*PI*(284.+DAY)/365.)*23.45*PI/180.
      HOUR=(REAL(IHOUR)+REAL(IMIN)/60.)*PI/12.-PI
      COSZ=SIN(RADJGRD(1))*SIN(DECL)+COS(RADJGRD(1))*COS(DECL)*COS(HOUR)

      DO I=1,NLTEST
        CSZGRD(I)=SIGN(MAX(ABS(COSZ),1.0E-3),COSZ)
        IF(PREGRD(I).GT.0.) THEN
          XDIFFUS(I)=1.0
        ELSE
          XDIFFUS(I)=MAX(0.0,MIN(1.0-0.9*COSZ,1.0))
        ENDIF
        FCLOGRD(I)=XDIFFUS(I)
      ENDDO

c *********************************************************************
c Start of calls to CLASS subroutines
c *********************************************************************

C
      CALL CLASSI(VPDGRD,TADPGRD,PADRGRD,RHOAGRD,RHSIGRD,
     1            RPCPGRD,TRPCGRD,SPCPGRD,TSPCGRD,TAGRD,QAGRD,
     2            PREGRD,RPREGRD,SPREGRD,PRESGRD,
     3            IPCP,NLAT,1,NLTEST)
C
      CALL GATPREP(ILMOS,JLMOS,IWMOS,JWMOS,IWAT,IICE,
     1             NML,NMW,NWAT,NICE,GCGRD,FAREROW,MIDROW,
     2             NLAT,NMOS,ILG,1,NLTEST,NMTEST)

C Calculate initial storage (after reading in resume.txt file if applicable)

      IF(JAN.EQ.1) THEN
        INIT_STORE=0.0
        DO I=1,NLTEST
        DO M=1,NMTEST
          IF(WF_FRAC(I).GT.0.0)THEN
            INIT_STORE=INIT_STORE+(RCANROW(I,M)+SCANROW(I,M)+SNOROW(I,M)
     1      +(THLQROW(I,M,1)*RHOW+THICROW(I,M,1)*RHOICE)*
     2            DLZWROW(I,M,1)+ZPNDROW(I,M)*RHOW
     3      +(THLQROW(I,M,2)*RHOW+THICROW(I,M,2)*RHOICE)*DLZWROW(I,M,2)
     4      +(THLQROW(I,M,3)*RHOW+THICROW(I,M,3)*RHOICE)*DLZWROW(I,M,3))
     5      *FAREROW(I,M)
	    ENDIF
        ENDDO
        ENDDO
      ENDIF

C
      CALL CLASSG (TBARGAT,THLQGAT,THICGAT,TPNDGAT,ZPNDGAT,
     1             TBASGAT,ALBSGAT,TSNOGAT,RHOSGAT,SNOGAT,
     2             TCANGAT,RCANGAT,SCANGAT,GROGAT, CMAIGAT,
     3             FCANGAT,LNZ0GAT,ALVCGAT,ALICGAT,LAMXGAT,
     4             LAMNGAT,CMASGAT,ROOTGAT,RSMNGAT,QA50GAT,
     5             VPDAGAT,VPDBGAT,PSGAGAT,PSGBGAT,AILDGAT,
     6             HGTDGAT,ACVDGAT,ACIDGAT,TSFSGAT,WSNOGAT,
     7             THPGAT, THRGAT, THMGAT, BIGAT,  PSISGAT,
     8             GRKSGAT,THRAGAT,HCPSGAT,TCSGAT,
     9             THFCGAT,PSIWGAT,DLZWGAT,ZBTWGAT,
     A             ZSNLGAT,ZPLGGAT,ZPLSGAT,TACGAT, QACGAT,
     B             DRNGAT, XSLPGAT,GRKFGAT,WFSFGAT,WFCIGAT,
     C             ALGWGAT,ALGDGAT,ASVDGAT,ASIDGAT,AGVDGAT,
     D             AGIDGAT,ISNDGAT,RADJGAT,ZBLDGAT,Z0ORGAT,
     E             ZRFMGAT,ZRFHGAT,ZDMGAT, ZDHGAT, FSVHGAT,
     F             FSIHGAT,CSZGAT, FDLGAT, ULGAT,  VLGAT,
     G             TAGAT,  QAGAT,  PRESGAT,PREGAT, PADRGAT,
     H             VPDGAT, TADPGAT,RHOAGAT,RPCPGAT,TRPCGAT,
     I             SPCPGAT,TSPCGAT,RHSIGAT,FCLOGAT,DLONGAT,
     J             GGEOGAT,
     K             CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT,
     L             QFSGAT, QFXGAT, PETGAT, GAGAT,  EFGAT,
     M             GTGAT,  QGGAT,  TSFGAT, ALVSGAT,ALIRGAT,
     N             SFCTGAT,SFCUGAT,SFCVGAT,SFCQGAT,FSNOGAT,
     O             FSGVGAT,FSGSGAT,FSGGGAT,FLGVGAT,FLGSGAT,
     P             FLGGGAT,HFSCGAT,HFSSGAT,HFSGGAT,HEVCGAT,
     Q             HEVSGAT,HEVGGAT,HMFCGAT,HMFNGAT,HTCCGAT,
     R             HTCSGAT,PCFCGAT,PCLCGAT,PCPNGAT,PCPGGAT,
     S             QFGGAT, QFNGAT, QFCLGAT,QFCFGAT,ROFGAT,
     T             ROFOGAT,ROFSGAT,ROFBGAT,TROFGAT,TROOGAT,
     U             TROSGAT,TROBGAT,ROFCGAT,ROFNGAT,ROVGGAT,
     V             WTRCGAT,WTRSGAT,WTRGGAT,DRGAT,
     W             HMFGGAT,HTCGAT, QFCGAT, ITCTGAT,
     X             ILMOS,JLMOS,IWMOS,JWMOS,
     Y             NML,NLAT,NMOS,ILG,IGND,ICAN,ICAN+1,
     Z             TBARROW,THLQROW,THICROW,TPNDROW,ZPNDROW,
     +             TBASROW,ALBSROW,TSNOROW,RHOSROW,SNOROW,
     +             TCANROW,RCANROW,SCANROW,GROROW, CMAIROW,
     +             FCANROW,LNZ0ROW,ALVCROW,ALICROW,LAMXROW,
     +             LAMNROW,CMASROW,ROOTROW,RSMNROW,QA50ROW,
     +             VPDAROW,VPDBROW,PSGAROW,PSGBROW,AILDROW,
     +             HGTDROW,ACVDROW,ACIDROW,TSFSROW,WSNOROW,
     +             THPROW, THRROW, THMROW, BIROW,  PSISROW,
     +             GRKSROW,THRAROW,HCPSROW,TCSROW,
     +             THFCROW,PSIWROW,DLZWROW,ZBTWROW,
     +             ZSNLROW,ZPLGROW,ZPLSROW,TACROW, QACROW,
     +             DRNROW, XSLPROW,GRKFROW,WFSFROW,WFCIROW,
     +             ALGWROW,ALGDROW,ASVDROW,ASIDROW,AGVDROW,
     +             AGIDROW,ISNDROW,RADJGRD,ZBLDGRD,Z0ORGRD,
     +             ZRFMGRD,ZRFHGRD,ZDMGRD, ZDHGRD, FSVHGRD,
     +             FSIHGRD,CSZGRD, FDLGRD, ULGRD,  VLGRD,
     +             TAGRD,  QAGRD,  PRESGRD,PREGRD, PADRGRD,
     +             VPDGRD, TADPGRD,RHOAGRD,RPCPGRD,TRPCGRD,
     +             SPCPGRD,TSPCGRD,RHSIGRD,FCLOGRD,DLONGRD,
     +             GGEOGRD,MANNROW,MANNGAT,DDROW,DDGAT  )

      IF(BASINSHORTWAVEFLAG.EQ.1)THEN !use Pablo's code
        READ(90,'(3f11.5)'),FSDOWN1,FSDOWN2,FSDOWN3
        DO I=1,NML
C READ FROM FILE THE INCOMING SHORT WAVE (NF=fsdown1,SF=fsdown2,FLAT=fsdown3)
C AND APPLY THOSE VALUES TO THE 13 TILES (GRUs)
c 1F+NF,2F+SF,3F+EF,4F+flat,5S+NF,6S+SF,7S+EF,8S+flat
c 9A+NF,10A+SF,11A+EF,12A+flat,13WATER+flat

! THIS IS A TERRIBLE HACK TO PABLO'S TERRIBLE HACK BECAUSE I DON'T UNDERSTAND
! HOW TO RESOLVE THIS CONFLICT INVOLVING FSDOWN
          IF(jlmos(I).eq.1.or.jlmos(I).eq.5.or.jlmos(I).eq.9) THEN
            IF(ihour.eq.7) THEN
              CONTINUE
            ENDIF
            !fsdown=fsdown1
            !FSVHGAT(I)=0.5*FSDOWN
            !FSIHGAT(I)=0.5*FSDOWN
            FSVHGAT(I)=0.5*FSDOWN1
            FSIHGAT(I)=0.5*FSDOWN1
          ELSEIF(jlmos(I).eq.2.or.jlmos(I).eq.6
     +           .or.jlmos(I).eq.10) THEN
            !fsdown=fsdown2
            !FSVHGAT(I)=0.5*FSDOWN
            !FSIHGAT(I)=0.5*FSDOWN
            FSVHGAT(I)=0.5*FSDOWN2
            FSIHGAT(I)=0.5*FSDOWN2
          ELSE
            !fsdown=fsdown3
            !FSVHGAT(I)=0.5*FSDOWN
            !FSIHGAT(I)=0.5*FSDOWN
            FSVHGAT(I)=0.5*FSDOWN3
            FSIHGAT(I)=0.5*FSDOWN3
          ENDIF
        END DO
      ENDIF

! FUTUREDO: Ensure assumptions on reading the longwave/rain/etc. data
! is correct.

! *********************************************************************
! basin_shortwave.csv -- is this a better version than above?
!                        assuming we change the input files to this
! *********************************************************************
C      IF (BASINSHORTWAVEFLAG == 1) THEN
C        DO I = 1, NLTEST
C          READ (90, *) FSVHGAT(I)
C          FSIHGAT(I) = FSVHGAT(I)
C        END DO
C      ENDIF

! *********************************************************************
! basin_longwave.csv
! *********************************************************************
      IF (BASINLONGWAVEFLAG == 1) THEN
        DO I = 1, NLTEST
          READ (91, *) FDLGRD(I)
        END DO
      ENDIF

! *********************************************************************
! basin_rain.csv
! *********************************************************************
      IF (BASINRAINFLAG == 1) THEN
        DO I = 1, NLTEST
          READ (92, *) PREGRD(I)
        END DO
      ENDIF

! *********************************************************************
! basin_temperature.csv
! *********************************************************************
      IF (BASINTEMPERATUREFLAG == 1) THEN
        DO I = 1, NLTEST
          READ (93, *) TAGRD(I)
        END DO
      ENDIF

! *********************************************************************
! basin_wind.csv
! *********************************************************************
      IF (BASINWINDFLAG == 1) THEN
        DO I = 1, NLTEST
          READ (94, *) ULGRD(I)
        END DO
      ENDIF

! *********************************************************************
! basin_pres.csv
! *********************************************************************
      IF (BASINPRESFLAG == 1) THEN
        DO I = 1, NLTEST
          READ (95, *) PRESGRD(I)
        END DO
      ENDIF

! *********************************************************************
! basin_humidity.csv
! *********************************************************************
      IF (BASINHUMIDITYFLAG == 1) THEN
        DO I = 1, NLTEST
          READ (96, *) QAGRD(I)
        END DO
      ENDIF


C
C========================================================================
C
      CALL CLASSZ (0,      CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP,
     1             WTVSTP, WTSSTP, WTGSTP,
     2             FSGVGAT,FLGVGAT,HFSCGAT,HEVCGAT,HMFCGAT,HTCCGAT,
     3             FSGSGAT,FLGSGAT,HFSSGAT,HEVSGAT,HMFNGAT,HTCSGAT,
     4             FSGGGAT,FLGGGAT,HFSGGAT,HEVGGAT,HMFGGAT,HTCGAT,
     5             PCFCGAT,PCLCGAT,QFCFGAT,QFCLGAT,ROFCGAT,WTRCGAT,
     6             PCPNGAT,QFNGAT, ROFNGAT,WTRSGAT,PCPGGAT,QFGGAT,
     7             QFCGAT, ROFGAT, WTRGGAT,CMAIGAT,RCANGAT,SCANGAT,
     8             TCANGAT,SNOGAT, WSNOGAT,TSNOGAT,THLQGAT,THICGAT,
     9             HCPSGAT,THPGAT, DLZWGAT,TBARGAT,ZPNDGAT,TPNDGAT,
     A             DELZ,   FCS,    FGS,    FC,     FG,
     B             1,      NML,    ILG,    IGND,   N    )
C
C========================================================================
C
C     * ALBEDO AND TRANSMISSIVITY CALCULATIONS; GENERAL VEGETATION
C     * CHARACTERISTICS.
C
      CALL CLASSA    (FC,     FG,     FCS,    FGS,    ALVSCN, ALIRCN,
     1                ALVSG,  ALIRG,  ALVSCS, ALIRCS, ALVSSN, ALIRSN,
     2                ALVSGC, ALIRGC, ALVSSC, ALIRSC,
     3                TRVSCN, TRIRCN, TRVSCS, TRIRCS, AILCAN, AILCNS,
     4                FSVF,   FSVFS,  RAICAN, RAICNS, SNOCAN, SNOCNS,
     5                FRAINC, FSNOWC, DISP,   DISPS,  ZOMLNC, ZOMLCS,
     6                ZOELNC, ZOELCS, ZOMLNG, ZOMLNS, ZOELNG, ZOELNS,
     7                CHCAP,  CHCAPS, CMASSC, CMASCS, CWLCAP, CWFCAP,
     8                CWLCPS, CWFCPS, RC,     RCS,    RBCOEF, FROOT,
     9                ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, TRSNOW, ZSNOW,
     A                WSNOGAT,ALVSGAT,ALIRGAT,HTCCGAT,HTCSGAT,HTCGAT,
     B                WTRCGAT,WTRSGAT,WTRGGAT,CMAIGAT,FSNOGAT,
     C                FCANGAT,LNZ0GAT,ALVCGAT,ALICGAT,LAMXGAT,LAMNGAT,
     D                CMASGAT,ROOTGAT,RSMNGAT,QA50GAT,VPDAGAT,VPDBGAT,
     E                PSGAGAT,PSGBGAT,AILDGAT,HGTDGAT,ACVDGAT,ACIDGAT,
     F                ASVDGAT,ASIDGAT,AGVDGAT,AGIDGAT,ALGWGAT,ALGDGAT,
     G                THLQGAT,THICGAT,TBARGAT,RCANGAT,SCANGAT,TCANGAT,
     H                GROGAT, SNOGAT, TSNOGAT,RHOSGAT,ALBSGAT,ZBLDGAT,
     I                Z0ORGAT,ZSNLGAT,ZPLGGAT,ZPLSGAT,
     J                FCLOGAT,TAGAT,  VPDGAT, RHOAGAT,CSZGAT,
     K                FSVHGAT,RADJGAT,DLONGAT,RHSIGAT,DELZ,   DLZWGAT,
     L                ZBTWGAT,THPGAT, THMGAT, PSISGAT,BIGAT,  PSIWGAT,
     M                HCPSGAT,ISNDGAT,IDAY,   ILG,    1,      NML,
     N                JLAT,   ICAN,   ICAN+1, IGND,   IDISP,  IZREF,
     O                IWF,    ILAI,   IHGT,   IALC,   IALS,   IALG  )
C
C-----------------------------------------------------------------------
C          * SURFACE TEMPERATURE AND FLUX CALCULATIONS.
C
      CALL  CLASST     (TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG,
     1  THICEC, THICEG, HCPC,   HCPG,   TCTOP,  TCBOT,  GZEROC, GZEROG,
     2  GZROCS, GZROGS, G12C,   G12G,   G12CS,  G12GS,  G23C,   G23G,
     3  G23CS,  G23GS,  QFREZC, QFREZG, QMELTC, QMELTG, EVAPC,  EVAPCG,
     4  EVAPG,  EVAPCS, EVPCSG, EVAPGS, TCANO,  TCANS,
     5  RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP,  CHCAPS, TPONDC, TPONDG,
     6  TPNDCS, TPNDGS, TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     7  ZTHRC,  ZTHRG,  ZTHRCS, ZTHRGS,
     8  ITCTGAT,CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT,QFSGAT, QFXGAT,
     9  PETGAT, GAGAT,  EFGAT,  GTGAT,  QGGAT,  TSFGAT, SFCTGAT,SFCUGAT,
     A  SFCVGAT,SFCQGAT,FSGVGAT,FSGSGAT,FSGGGAT,FLGVGAT,FLGSGAT,FLGGGAT,
     B  HFSCGAT,HFSSGAT,HFSGGAT,HEVCGAT,HEVSGAT,HEVGGAT,HMFCGAT,HMFNGAT,
     C  HTCCGAT,HTCSGAT,HTCGAT, DRGAT,  WTABGAT,ILMOGAT,UEGAT,  HBLGAT,
     D  TACGAT, QACGAT, ZRFMGAT,ZRFHGAT,ZDMGAT, ZDHGAT, TBAR3,
     E  VPDGAT, TADPGAT,RHOAGAT,FSVHGAT,FSIHGAT,FDLGAT, ULGAT,  VLGAT,
     F  TAGAT,  QAGAT,  PADRGAT,FC,     FG,     FCS,    FGS,    RBCOEF,
     G  AILCAN, AILCNS, FSVF,   FSVFS,  ALVSCN, ALIRCN, ALVSG,  ALIRG,
     H  ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, ALIRSC,
     I  TRVSCN, TRIRCN, TRVSCS, TRIRCS,
     J  RC,     RCS,    FRAINC, FSNOWC, CMASSC, CMASCS, DISP,   DISPS,
     K  ZOMLNC, ZOELNC, ZOMLNG, ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS,
     L  TBARGAT,THLQGAT,THICGAT,TPNDGAT,ZPNDGAT,TBASGAT,TCANGAT,TSNOGAT,
     M  ZSNOW,  TRSNOW, RHOSGAT,WSNOGAT,THPGAT, THRGAT, THMGAT, THFCGAT,
     N  RADJGAT,HCPSGAT,TCSGAT, TSFSGAT,DELZ,   DLZWGAT,ZBTWGAT,ISNDGAT,
     O  ILW,    ITC,    ITCG,   ITG,    ILG,    1,NML,  JLAT,   ICAN,
     P  IGND,   IZREF,  ISLFD,  NLANDCS,NLANDGS,NLANDC, NLANDG, NLANDI)
C
C-----------------------------------------------------------------------
C          * WATER BUDGET CALCULATIONS.
C
          CALL CLASSW  (THLQGAT,THICGAT,TBARGAT,TCANGAT,RCANGAT,SCANGAT,
     1                  ROFGAT, TROFGAT,SNOGAT, TSNOGAT,RHOSGAT,ALBSGAT,
     2                  WSNOGAT,ZPNDGAT,TPNDGAT,GROGAT, TBASGAT,GFLXGAT,
     3                  PCFCGAT,PCLCGAT,PCPNGAT,PCPGGAT,QFCFGAT,QFCLGAT,
     4                  QFNGAT, QFGGAT, QFCGAT, HMFCGAT,HMFGGAT,HMFNGAT,
     5                  HTCCGAT,HTCSGAT,HTCGAT, ROFCGAT,ROFNGAT,ROVGGAT,
     6                  WTRSGAT,WTRGGAT,ROFOGAT,ROFSGAT,ROFBGAT,
     7                  TROOGAT,TROSGAT,TROBGAT,QFSGAT,
     8                  TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG,
     9                  THICEC, THICEG, HCPC,   HCPG,   RPCPGAT,TRPCGAT,
     A                  SPCPGAT,TSPCGAT,PREGAT, TAGAT,  RHSIGAT,GGEOGAT,
     B                  FC,     FG,     FCS,    FGS,    TPONDC, TPONDG,
     C                  TPNDCS, TPNDGS, EVAPC,  EVAPCG, EVAPG,  EVAPCS,
     D                  EVPCSG, EVAPGS, QFREZC, QFREZG, QMELTC, QMELTG,
     E                  RAICAN, SNOCAN, RAICNS, SNOCNS, FROOT,  FSVF,
     F                  FSVFS,  CWLCAP, CWFCAP, CWLCPS, CWFCPS, TCANO,
     G                  TCANS,  CHCAP,  CHCAPS, CMASSC, CMASCS, ZSNOW,
     H                  GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,
     I                  G12CS,  G12GS,  G23C,   G23G,   G23CS,  G23GS,
     J                  TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     K                  ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, TSFSGAT,
     J                  TCTOP,  TCBOT,  ZTHRC,  ZTHRG,  ZTHRCS, ZTHRGS,
     M                  THPGAT, THRGAT, THMGAT, BIGAT,  PSISGAT,GRKSGAT,
     N                  THRAGAT,THFCGAT,DRNGAT, HCPSGAT,DELZ,
     O                  DLZWGAT,ZBTWGAT,XSLPGAT,GRKFGAT,WFSFGAT,WFCIGAT,
     P                  ISNDGAT,IWF,    ILG,    1,      NML,
     Q                  JLAT,   ICAN,   IGND,   IGND+1, IGND+2,
     R                  NLANDCS,NLANDGS,NLANDC, NLANDG,NLANDI,MANNGAT,
     S                  DDGAT  )
C
C========================================================================
C
      CALL CLASSZ (1,      CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP,
     1             WTVSTP, WTSSTP, WTGSTP,
     2             FSGVGAT,FLGVGAT,HFSCGAT,HEVCGAT,HMFCGAT,HTCCGAT,
     3             FSGSGAT,FLGSGAT,HFSSGAT,HEVSGAT,HMFNGAT,HTCSGAT,
     4             FSGGGAT,FLGGGAT,HFSGGAT,HEVGGAT,HMFGGAT,HTCGAT,
     5             PCFCGAT,PCLCGAT,QFCFGAT,QFCLGAT,ROFCGAT,WTRCGAT,
     6             PCPNGAT,QFNGAT, ROFNGAT,WTRSGAT,PCPGGAT,QFGGAT,
     7             QFCGAT, ROFGAT, WTRGGAT,CMAIGAT,RCANGAT,SCANGAT,
     8             TCANGAT,SNOGAT, WSNOGAT,TSNOGAT,THLQGAT,THICGAT,
     9             HCPSGAT,THPGAT, DLZWGAT,TBARGAT,ZPNDGAT,TPNDGAT,
     A             DELZ,   FCS,    FGS,    FC,     FG,
     B             1,      NML,    ILG,    IGND,   N    )
C
C=======================================================================

      CALL CLASSS (TBARROW,THLQROW,THICROW,TPNDROW,ZPNDROW,
     1             TBASROW,ALBSROW,TSNOROW,RHOSROW,SNOROW,
     2             TCANROW,RCANROW,SCANROW,GROROW,TSFSROW,
     3             CDHROW, CDMROW, HFSROW, TFXROW, QEVPROW,
     4             QFSROW, QFXROW, PETROW, GAROW,  EFROW,
     5             GTROW,  QGROW,  TSFROW, ALVSROW,ALIRROW,
     6             CMAIROW,SFCTROW,SFCUROW,SFCVROW,SFCQROW,
     7             FSGVROW,FSGSROW,FSGGROW,FLGVROW,FLGSROW,
     8             FLGGROW,HFSCROW,HFSSROW,HFSGROW,HEVCROW,
     9             HEVSROW,HEVGROW,HMFCROW,HMFNROW,HTCCROW,
     A             HTCSROW,PCFCROW,PCLCROW,PCPNROW,PCPGROW,
     B             QFGROW, QFNROW, QFCLROW,QFCFROW,ROFROW,
     C             ROFOROW,ROFSROW,ROFBROW,TROFROW,TROOROW,
     D             TROSROW,TROBROW,ROFCROW,ROFNROW,ROVGROW,
     E             WTRCROW,WTRSROW,WTRGROW,DRROW,  WTABROW,
     F             ILMOROW,UEROW,  HBLROW, TACROW, QACROW,
     G             HMFGROW,HTCROW, QFCROW, WSNOROW,FSNOROW,
     H             ITCTROW,ILMOS,JLMOS,IWMOS,JWMOS,
     I             NML,NLAT,NMOS,ILG,IGND,ICAN,ICAN+1,
     J             TBARGAT,THLQGAT,THICGAT,TPNDGAT,ZPNDGAT,
     K             TBASGAT,ALBSGAT,TSNOGAT,RHOSGAT,SNOGAT,
     L             TCANGAT,RCANGAT,SCANGAT,GROGAT,TSFSGAT,
     M             CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT,
     N             QFSGAT, QFXGAT, PETGAT, GAGAT,  EFGAT,
     O             GTGAT,  QGGAT,  TSFGAT, ALVSGAT,ALIRGAT,
     P             CMAIGAT,SFCTGAT,SFCUGAT,SFCVGAT,SFCQGAT,
     Q             FSGVGAT,FSGSGAT,FSGGGAT,FLGVGAT,FLGSGAT,
     R             FLGGGAT,HFSCGAT,HFSSGAT,HFSGGAT,HEVCGAT,
     S             HEVSGAT,HEVGGAT,HMFCGAT,HMFNGAT,HTCCGAT,
     T             HTCSGAT,PCFCGAT,PCLCGAT,PCPNGAT,PCPGGAT,
     U             QFGGAT, QFNGAT, QFCLGAT,QFCFGAT,ROFGAT,
     V             ROFOGAT,ROFSGAT,ROFBGAT,TROFGAT,TROOGAT,
     W             TROSGAT,TROBGAT,ROFCGAT,ROFNGAT,ROVGGAT,
     X             WTRCGAT,WTRSGAT,WTRGGAT,DRGAT,  WTABGAT,
     Y             ILMOGAT,UEGAT,  HBLGAT, TACGAT, QACGAT,
     Z             HMFGGAT,HTCGAT, QFCGAT, WSNOGAT,FSNOGAT,
     +             ITCTGAT,MANNROW,MANNGAT,DDROW,DDGAT)

c *********************************************************************
c Calculate values for output files and print them out
c *********************************************************************

C
C=======================================================================
C     * WRITE FIELDS FROM CURRENT TIME STEP TO OUTPUT FILES.

6100  FORMAT((I4,','),(I5,','),9(F8.2,','),2(F8.3,','),
     +         (F12.4,','),4(F12.4,','))
6200  FORMAT((I4,','),(I5,','),5((F8.2,','),2(F6.3,',')),
     +         (F8.2,','),2(F7.4,','),2(F8.2,','),(E12.5,','))
6300  FORMAT((I4,','),(I5,','),3(F9.2,','),(F8.2,','),
     +         (F10.2,','),(E12.3,','),2(F12.3,','),(F8.3,','))


6400  FORMAT((I2,','),(I3,','),(I5,','),(I6,','),9(F8.2,','),2(F7.3,',')
     +,(E11.3,','),(F8.2,','),(F12.4,','))
6500  FORMAT((I2,','),(I3,','),(I5,','),(I6,','),3(F7.2,',',2(F6.3,','))
     +,(F8.2,','),2(F8.4,','),(F8.2,','),(F8.3,','))
6600  FORMAT((I2,','),(I3,','),(I5,','),2(F10.2,','),(F12.6,',')
     +,(F10.2,','),(F8.2,','),(F10.2,','),(F15.9,','))

6700  FORMAT(12(E11.4,','))
6800  FORMAT(22(F10.4,','))
6900  FORMAT(21(E12.4,','))


      DO I=1,NLTEST
        DO M=1,NMTEST
          IF(FSDOWN(I).GT.0.0) THEN
            ALTOT=(ALVSROW(I,M)+ALIRROW(I,M))/2.0
          ELSE
            ALTOT=0.0
          ENDIF

          FSSTAR=FSDOWN(I)*(1.0-ALTOT)
          FLSTAR=FDLGRD(I)-SBC*GTROW(I,M)**4
          QH=HFSROW(I,M)
          QE=QEVPROW(I,M)
          BEG=FSSTAR+FLSTAR-QH-QE
          SNOMLT=HMFNROW(I,M)

          IF(RHOSROW(I,M).GT.0.0) THEN
            ZSN=SNOROW(I,M)/RHOSROW(I,M)
          ELSE
            ZSN=0.0
          ENDIF

          IF(TCANROW(I,M).GT.0.01) THEN
            TCN=TCANROW(I,M)-TFREZ
          ELSE
            TCN=0.0
          ENDIF

          IF(TSNOROW(I,M).GT.0.01) THEN
            TSN=TSNOROW(I,M)-TFREZ
          ELSE
            TSN=0.0
          ENDIF

          IF(TPNDROW(I,M).GT.0.01) THEN
            TPN=TPNDROW(I,M)-TFREZ
          ELSE
            TPN=0.0
          ENDIF

          IF(ILW.EQ.1) THEN
            GTOUT=GTROW(I,M)-TFREZ
          ELSE
            GTOUT=0.0
          ENDIF

      I_OUT=0
      DO K=1, WF_NUM_POINTS
        IF(I.EQ.N_OUT(K).AND.M.EQ.II_OUT(k)) THEN
        ! figure out nlmos and mlmos of grid square I
          DO N=1, NML
            IF(ILMOS(N).EQ.N_OUT(K).AND.JLMOS(N).EQ.II_OUT(K)) THEN
              I_OUT=N
            ENDIF
	    ENDDO

          IF(I_OUT.EQ.0) THEN
            PRINT *,'In the input file there the following'
            PRINT *, 'grid square', i, ' has no area in land class', m
            PRINT *,'Please adjust the MESH_input_run_options file,
     *        as a guide here'
            PRINT *,'are the land class fractions for that square:'
            DO N=1, NMTEST
              PRINT *, 'land class ',n,' has an area of:',wf_aclass(i,n)
            ENDDO
	      STOP
          ENDIF


          WRITE(150+k*10+4,6400) IHOUR,IMIN,IDAY,IYEAR,FSSTAR,FLSTAR,QH,
     1                   QE,SNOMLT,BEG,GTOUT,SNOROW(I,M),RHOSROW(I,M),
     2                   WSNOROW(I,M),ALTOT,ROFROW(I,M),
     3                   TPN,ZPNDROW(I,M)
          WRITE(150+k*10+5,6500) IHOUR,IMIN,IDAY,IYEAR,
     1                   (TBARROW(I,M,J)-TFREZ,THLQROW(I,M,J),
     2                   THICROW(I,M,J),J=1,IGND),TCN,RCANROW(I,M),
     3                   SCANROW(I,M),TSN,ZSN
          WRITE(150+k*10+6,6600) IHOUR,IMIN,IDAY,FSDOWN(I),FDLGRD(I),
     1                   PREGRD(I),TAGRD(I)-TFREZ,UVGRD(I),PRESGRD(I),
     2                   QAGRD(I)
          WRITE(150+k*10+7,6700) TROFROW(I,M),TROOROW(I,M),TROSROW(I,M),
     1                   TROBROW(I,M),ROFROW(I,M),ROFOROW(I,M),
     2                   ROFSROW(I,M),ROFBROW(I,M),
     3                   FCS(I),FGS(I),FC(I),FG(I)
          WRITE(150+k*10+8,6800) FSGVROW(I,M),FSGSROW(I,M),FSGGROW(I,M),
     1                   FLGVROW(I,M),FLGSROW(I,M),FLGGROW(I,M),
     2                   HFSCROW(I,M),HFSSROW(I,M),HFSGROW(I,M),
     3                   HEVCROW(I,M),HEVSROW(I,M),HEVGROW(I,M),
     4                   HMFCROW(I,M),HMFNROW(I,M),
     5                   (HMFGROW(I,M,J),J=1,IGND),
     6                   HTCCROW(I,M),HTCSROW(I,M),
     7                   (HTCROW(I,M,J),J=1,IGND)
          WRITE(150+k*10+9,6900) PCFCROW(I,M),PCLCROW(I,M),PCPNROW(I,M),
     1                   PCPGROW(I,M),QFCFROW(I,M),QFCLROW(I,M),
     2                   QFNROW(I,M),QFGROW(I,M),(QFCROW(I,M,J),
     3                   J=1,IGND),ROFCROW(I,M),ROFNROW(I,M),
     4                   ROFOROW(I,M),ROFROW(I,M),WTRCROW(I,M),
     5                   WTRSROW(I,M),WTRGROW(I,M)

        ENDIF !IF(I.EQ.N_OUT(K).AND.M.EQ.II_OUT(k)) THEN
      ENDDO !DO K=1, WF_NUM_POINTS
      ENDDO !DO M=1,NMTEST
      ENDDO !DO I=1,NLTEST
C Write ENSIM output
c-----------------------------------------------------c
c
      IF(ensim_ios.EQ.0)THEN
        !calculate month/day
        if (iday < 32) then
          ensim_month = 1
          ensim_day = iday
        elseif (iday < 60) then
          ensim_month = 2
          ensim_day = iday - 31
        elseif (iday < 91) then
          ensim_month = 3
          ensim_day = iday - 59
        elseif (iday < 121) then
          ensim_month = 4
          ensim_day = iday - 90
        elseif (iday < 152) then
          ensim_month = 5
          ensim_day = iday - 120
        elseif (iday < 182) then
          ensim_month = 6
          ensim_day = iday - 151
        elseif (iday < 213) then
          ensim_month = 7
          ensim_day = iday - 181
        elseif (iday < 244) then
          ensim_month = 8
          ensim_day = iday - 212
        elseif (iday < 274) then
          ensim_month = 9
          ensim_day = iday - 243
        elseif (iday < 305) then
          ensim_month = 10
          ensim_day = iday - 273
        elseif (iday < 335) then
          ensim_month = 11
          ensim_day = iday - 304
        else
          ensim_month = 12
          ensim_day = iday - 334
        endif
          
        CALL ENSIM(NML, NLTEST, NMTEST, NCOUNT, IMIN, ireport,
     1    wfo_seq, IYEAR, ensim_MONTH, ensim_DAY, IHOUR, IMIN, 0,
     2    0, wf_xx, wf_yy, wf_imax, wf_jmax, wfo_pick, M_Y, M_X,
     3    NLAT, NMOS, PREGRD, DELT, TAGRD, TFREZ, FSDOWN, FDLGRD, UVGRD,
     4    PRESGRD, QAGRD, ROFBROW, ROFSROW, ROFOROW, ROFROW, FAREROW,
     5    QFSROW, QEVPROW, HFSROW, SBC, GTROW, ALVSROW,
     6    ALIRROW, FC, FG, FCS, FGS, GZEROC, GZEROG,
      !TODO: check that fsnorow is correct (bjd - Jan 15/08)
     7    GZROCS, GZROGS, SNOROW, ZSNOW, FSNOROW, TSNOROW,
     8    TBARROW, THLQROW, THICROW, RHOW, DELZ,ILMOS,JLMOS, ILG)

      ENDIF
C
C=======================================================================
C     * CALCULATE GRID CELL AVERAGE DIAGNOSTIC FIELDS.
C
      DO I=1,NLTEST
          CDHGRD(I)=0.
          CDMGRD(I)=0.
          HFSGRD(I)=0.
          TFXGRD(I)=0.
          QEVPGRD(I)=0.
          QFSGRD(I)=0.
          QFXGRD(I)=0.
          PETGRD(I)=0.
          GAGRD(I)=0.
          EFGRD(I)=0.
          GTGRD(I)=0.
          QGGRD(I)=0.
          TSFGRD(I)=0.
          ALVSGRD(I)=0.
          ALIRGRD(I)=0.
          SFCTGRD(I)=0.
          SFCUGRD(I)=0.
          SFCVGRD(I)=0.
          SFCQGRD(I)=0.
          FSNOGRD(I)=0.
          FSGVGRD(I)=0.
          FSGSGRD(I)=0.
          FSGGGRD(I)=0.
          SNOGRD(I)=0.
          FLGVGRD(I)=0.
          FLGSGRD(I)=0.
          FLGGGRD(I)=0.
          HFSCGRD(I)=0.
          HFSSGRD(I)=0.
          HFSGGRD(I)=0.
          HEVCGRD(I)=0.
          HEVSGRD(I)=0.
          HEVGGRD(I)=0.
          HMFCGRD(I)=0.
          HMFNGRD(I)=0.
          HTCCGRD(I)=0.
          HTCSGRD(I)=0.
          PCFCGRD(I)=0.
          PCLCGRD(I)=0.
          PCPNGRD(I)=0.
          PCPGGRD(I)=0.
          QFGGRD(I)=0.
          QFNGRD(I)=0.
          QFCLGRD(I)=0.
          QFCFGRD(I)=0.
          ROFGRD(I)=0.
          ROFOGRD(I)=0.
          ROFSGRD(I)=0.
          ROFBGRD(I)=0.
          ROFCGRD(I)=0.
          ROFNGRD(I)=0.
          ROVGGRD(I)=0.
          WTRCGRD(I)=0.
          WTRSGRD(I)=0.
          WTRGGRD(I)=0.
          DRGRD(I)=0.
          WTABGRD(I)=0.
          ILMOGRD(I)=0.
          UEGRD(I)=0.
          HBLGRD(I)=0.
          DO J=1,IGND
            HMFGGRD(I,J)=0.
            HTCGRD(I,J)=0.
            QFCGRD(I,J)=0.
          ENDDO
      ENDDO !DO I=1,NLTEST
C
      DO I=1,NLTEST
        DO M=1,NMTEST
          CDHGRD(I)=CDHGRD(I)+CDHROW(I,M)*FAREROW(I,M)
          CDMGRD(I)=CDMGRD(I)+CDMROW(I,M)*FAREROW(I,M)
          HFSGRD(I)=HFSGRD(I)+HFSROW(I,M)*FAREROW(I,M)
          TFXGRD(I)=TFXGRD(I)+TFXROW(I,M)*FAREROW(I,M)
          QEVPGRD(I)=QEVPGRD(I)+QEVPROW(I,M)*FAREROW(I,M)
          QFSGRD(I)=QFSGRD(I)+QFSROW(I,M)*FAREROW(I,M)
          QFXGRD(I)=QFXGRD(I)+QFXROW(I,M)*FAREROW(I,M)
          PETGRD(I)=PETGRD(I)+PETROW(I,M)*FAREROW(I,M)
          GAGRD(I)=GAGRD(I)+GAROW(I,M)*FAREROW(I,M)
          EFGRD(I)=EFGRD(I)+EFROW(I,M)*FAREROW(I,M)
          GTGRD(I)=GTGRD(I)+GTROW(I,M)*FAREROW(I,M)
          QGGRD(I)=QGGRD(I)+QGROW(I,M)*FAREROW(I,M)
          TSFGRD(I)=TSFGRD(I)+TSFROW(I,M)*FAREROW(I,M)
          ALVSGRD(I)=ALVSGRD(I)+ALVSROW(I,M)*FAREROW(I,M)
          ALIRGRD(I)=ALIRGRD(I)+ALIRROW(I,M)*FAREROW(I,M)
          SFCTGRD(I)=SFCTGRD(I)+SFCTROW(I,M)*FAREROW(I,M)
          SFCUGRD(I)=SFCUGRD(I)+SFCUROW(I,M)*FAREROW(I,M)
          SFCVGRD(I)=SFCVGRD(I)+SFCVROW(I,M)*FAREROW(I,M)
          SFCQGRD(I)=SFCQGRD(I)+SFCQROW(I,M)*FAREROW(I,M)
          FSNOGRD(I)=FSNOGRD(I)+FSNOROW(I,M)*FAREROW(I,M)
          FSGVGRD(I)=FSGVGRD(I)+FSGVROW(I,M)*FAREROW(I,M)
          FSGSGRD(I)=FSGSGRD(I)+FSGSROW(I,M)*FAREROW(I,M)
          FSGGGRD(I)=FSGGGRD(I)+FSGGROW(I,M)*FAREROW(I,M)
          SNOGRD(I)=SNOGRD(I)+SNOROW(I,M)*FAREROW(I,M)
          FLGVGRD(I)=FLGVGRD(I)+FLGVROW(I,M)*FAREROW(I,M)
          FLGSGRD(I)=FLGSGRD(I)+FLGSROW(I,M)*FAREROW(I,M)
          FLGGGRD(I)=FLGGGRD(I)+FLGGROW(I,M)*FAREROW(I,M)
          HFSCGRD(I)=HFSCGRD(I)+HFSCROW(I,M)*FAREROW(I,M)
          HFSSGRD(I)=HFSSGRD(I)+HFSSROW(I,M)*FAREROW(I,M)
          HFSGGRD(I)=HFSGGRD(I)+HFSGROW(I,M)*FAREROW(I,M)
          HEVCGRD(I)=HEVCGRD(I)+HEVCROW(I,M)*FAREROW(I,M)
          HEVSGRD(I)=HEVSGRD(I)+HEVSROW(I,M)*FAREROW(I,M)
          HEVGGRD(I)=HEVGGRD(I)+HEVGROW(I,M)*FAREROW(I,M)
          HMFCGRD(I)=HMFCGRD(I)+HMFCROW(I,M)*FAREROW(I,M)
          HMFNGRD(I)=HMFNGRD(I)+HMFNROW(I,M)*FAREROW(I,M)
          HTCCGRD(I)=HTCCGRD(I)+HTCCROW(I,M)*FAREROW(I,M)
          HTCSGRD(I)=HTCSGRD(I)+HTCSROW(I,M)*FAREROW(I,M)
          PCFCGRD(I)=PCFCGRD(I)+PCFCROW(I,M)*FAREROW(I,M)
          PCLCGRD(I)=PCLCGRD(I)+PCLCROW(I,M)*FAREROW(I,M)
          PCPNGRD(I)=PCPNGRD(I)+PCPNROW(I,M)*FAREROW(I,M)
          PCPGGRD(I)=PCPGGRD(I)+PCPGROW(I,M)*FAREROW(I,M)
          QFGGRD(I)=QFGGRD(I)+QFGROW(I,M)*FAREROW(I,M)
          QFNGRD(I)=QFNGRD(I)+QFNROW(I,M)*FAREROW(I,M)
          QFCLGRD(I)=QFCLGRD(I)+QFCLROW(I,M)*FAREROW(I,M)
          QFCFGRD(I)=QFCFGRD(I)+QFCFROW(I,M)*FAREROW(I,M)
          ROFGRD(I)=ROFGRD(I)+ROFROW(I,M)*FAREROW(I,M)
          ROFOGRD(I)=ROFOGRD(I)+ROFOROW(I,M)*FAREROW(I,M)
          ROFSGRD(I)=ROFSGRD(I)+ROFSROW(I,M)*FAREROW(I,M)
          ROFBGRD(I)=ROFBGRD(I)+ROFBROW(I,M)*FAREROW(I,M)
          ROFCGRD(I)=ROFCGRD(I)+ROFCROW(I,M)*FAREROW(I,M)
          ROFNGRD(I)=ROFNGRD(I)+ROFNROW(I,M)*FAREROW(I,M)
          ROVGGRD(I)=ROVGGRD(I)+ROVGROW(I,M)*FAREROW(I,M)
          WTRCGRD(I)=WTRCGRD(I)+WTRCROW(I,M)*FAREROW(I,M)
          WTRSGRD(I)=WTRSGRD(I)+WTRSROW(I,M)*FAREROW(I,M)
          WTRGGRD(I)=WTRGGRD(I)+WTRGROW(I,M)*FAREROW(I,M)
          DRGRD(I)=DRGRD(I)+DRROW(I,M)*FAREROW(I,M)
          WTABGRD(I)=WTABGRD(I)+WTABROW(I,M)*FAREROW(I,M)
          ILMOGRD(I)=ILMOGRD(I)+ILMOROW(I,M)*FAREROW(I,M)
          UEGRD(I)=UEGRD(I)+UEROW(I,M)*FAREROW(I,M)
          HBLGRD(I)=HBLGRD(I)+HBLROW(I,M)*FAREROW(I,M)
          DO J=1,IGND
              HMFGGRD(I,J)=HMFGGRD(I,J)+HMFGROW(I,M,J)*FAREROW(I,M)
              HTCGRD(I,J)=HTCGRD(I,J)+HTCROW(I,M,J)*FAREROW(I,M)
              QFCGRD(I,J)=QFCGRD(I,J)+QFCROW(I,M,J)*FAREROW(I,M)
          ENDDO
        ENDDO !DO M=1,NMTEST
      ENDDO !DO I=1,NLTEST

C calculate and write the basin avg SCA similar to watclass3.0f5
C Same code than in wf_ensim.f subrutine of watclass3.0f8
C Especially for version MESH_Prototype 3.3.1.7b (not to be incorporated in future versions)
C calculate and write the basin avg SWE using the similar fudge factor!!!

      IF (BASIN_FRACTION(1) == -1) THEN
        DO I = 1, NLTEST ! nltest = number of grid squares
c         BASIN_FRACTION is the basin snow cover
c         (portions of the grids outside the basin are not included)
c         for a given day - IDAY in the if statement
          BASIN_FRACTION(I) = WF_FRAC(I)
        ENDDO
      ENDIF

      IF((IHOUR.EQ.12).AND.(IMIN.EQ.0))  THEN

          basin_SCA = 0.0
          basin_SWE = 0.0

          do I=1,NLTEST
             if(BASIN_FRACTION(I).NE.0.0) then
                basin_SCA = basin_SCA + FSNOGRD(I)/BASIN_FRACTION(I)
                basin_SWE = basin_SWE + SNOGRD(I)/BASIN_FRACTION(I)
             endif
          enddo

         basin_SCA = basin_SCA/NLTEST
         basin_SWE = basin_SWE/NLTEST

      ENDIF


      IF ((IHOUR.EQ.12).AND.(IMIN.EQ.0))  THEN
        WRITE(85,'(I5, ",", F10.3)') IDAY, basin_SCA
        WRITE(86,'(I5, ",", F10.3)') IDAY, basin_SWE
      ENDIF

C
C=======================================================================
C     * ACCUMULATE OUTPUT DATA FOR DIURNALLY AVERAGED FIELDS.

      DO I=1,NLTEST
        DO M=1,NMTEST
          PREACC(I)=PREACC(I)+PREGRD(I)*FAREROW(I,M)*DELT
          GTACC(I)=GTACC(I)+GTROW(I,M)*FAREROW(I,M)
          QEVPACC(I)=QEVPACC(I)+QEVPROW(I,M)*FAREROW(I,M)
          EVAPACC(I)=EVAPACC(I)+QFSROW(I,M)*FAREROW(I,M)*DELT
          HFSACC(I)=HFSACC(I)+HFSROW(I,M)*FAREROW(I,M)
          HMFNACC(I)=HMFNACC(I)+HMFNROW(I,M)*FAREROW(I,M)
          ROFACC(I)=ROFACC(I)+ROFROW(I,M)*FAREROW(I,M)*DELT
          ROFOACC(I)=ROFOACC(I)+ROFOROW(I,M)*FAREROW(I,M)*DELT
          ROFSACC(I)=ROFSACC(I)+ROFSROW(I,M)*FAREROW(I,M)*DELT
          ROFBACC(I)=ROFBACC(I)+ROFBROW(I,M)*FAREROW(I,M)*DELT
          WTBLACC(I)=WTBLACC(I)+WTABROW(I,M)*FAREROW(I,M)
          DO J=1,IGND
            TBARACC(I,J)=TBARACC(I,J)+TBARROW(I,M,J)*FAREROW(I,M)
            THLQACC(I,J)=THLQACC(I,J)+THLQROW(I,M,J)*FAREROW(I,M)
            THICACC(I,J)=THICACC(I,J)+THICROW(I,M,J)*FAREROW(I,M)
            THALACC(I,J)=THALACC(I,J)+(THLQROW(I,M,J)+THICROW(I,M,J))
     1                  *FAREROW(I,M)
          ENDDO
          ALVSACC(I)=ALVSACC(I)+ALVSROW(I,M)*FAREROW(I,M)*FSVHGRD(I)
          ALIRACC(I)=ALIRACC(I)+ALIRROW(I,M)*FAREROW(I,M)*FSIHGRD(I)
          IF(SNOROW(I,M).GT.0.0) THEN
            RHOSACC(I)=RHOSACC(I)+RHOSROW(I,M)*FAREROW(I,M)
            TSNOACC(I)=TSNOACC(I)+TSNOROW(I,M)*FAREROW(I,M)
            WSNOACC(I)=WSNOACC(I)+WSNOROW(I,M)*FAREROW(I,M)
            SNOARE(I)=SNOARE(I)+FAREROW(I,M)
          ENDIF
          IF(TCANROW(I,M).GT.0.5) THEN
            TCANACC(I)=TCANACC(I)+TCANROW(I,M)*FAREROW(I,M)
            CANARE(I)=CANARE(I)+FAREROW(I,M)
          ENDIF
          SNOACC(I)=SNOACC(I)+SNOROW(I,M)*FAREROW(I,M)
          RCANACC(I)=RCANACC(I)+RCANROW(I,M)*FAREROW(I,M)
          SCANACC(I)=SCANACC(I)+SCANROW(I,M)*FAREROW(I,M)
          GROACC(I)=GROACC(I)+GROROW(I,M)*FAREROW(I,M)
          FSINACC(I)=FSINACC(I)+FSDOWN(I)*FAREROW(I,M)
          FLINACC(I)=FLINACC(I)+FDLGRD(I)*FAREROW(I,M)
          FLUTACC(I)=FLUTACC(I)+SBC*GTROW(I,M)**4*FAREROW(I,M)
          TAACC(I)=TAACC(I)+TAGRD(I)*FAREROW(I,M)
          UVACC(I)=UVACC(I)+UVGRD(I)*FAREROW(I,M)
          PRESACC(I)=PRESACC(I)+PRESGRD(I)*FAREROW(I,M)
          QAACC(I)=QAACC(I)+QAGRD(I)*FAREROW(I,M)
        ENDDO !DO M=1,NMTEST
      ENDDO !DO I=1,NLTEST
C
C     * CALCULATE AND PRINT DAILY AVERAGES.
C
      IF(NCOUNT.EQ.48) THEN !48 is the last half-hour period of the day
                            ! when they're numbered 1-48

      DO 800 I=1,NLTEST
          PREACC(I)=PREACC(I)
          GTACC(I)=GTACC(I)/REAL(NSUM)
          QEVPACC(I)=QEVPACC(I)/REAL(NSUM)
          EVAPACC(I)=EVAPACC(I)
          HFSACC(I)=HFSACC(I)/REAL(NSUM)
          HMFNACC(I)=HMFNACC(I)/REAL(NSUM)
          ROFACC(I)=ROFACC(I)
          ROFOACC(I)=ROFOACC(I)
          ROFSACC(I)=ROFSACC(I)
          ROFBACC(I)=ROFBACC(I)
          WTBLACC(I)=WTBLACC(I)/REAL(NSUM)

          DO J=1,IGND
            TBARACC(I,J)=TBARACC(I,J)/REAL(NSUM)
            THLQACC(I,J)=THLQACC(I,J)/REAL(NSUM)
            THICACC(I,J)=THICACC(I,J)/REAL(NSUM)
            THALACC(I,J)=THALACC(I,J)/REAL(NSUM)
          ENDDO

          IF(FSINACC(I).GT.0.0) THEN
            ALVSACC(I)=ALVSACC(I)/(FSINACC(I)*0.5)
            ALIRACC(I)=ALIRACC(I)/(FSINACC(I)*0.5)
          ELSE
            ALVSACC(I)=0.0
            ALIRACC(I)=0.0
          ENDIF

          IF(SNOARE(I).GT.0.0) THEN
            RHOSACC(I)=RHOSACC(I)/SNOARE(I)
            TSNOACC(I)=TSNOACC(I)/SNOARE(I)
            WSNOACC(I)=WSNOACC(I)/SNOARE(I)
          ENDIF

          IF(CANARE(I).GT.0.0) THEN
            TCANACC(I)=TCANACC(I)/CANARE(I)
          ENDIF

          SNOACC(I)=SNOACC(I)/REAL(NSUM)
          RCANACC(I)=RCANACC(I)/REAL(NSUM)
          SCANACC(I)=SCANACC(I)/REAL(NSUM)
          GROACC(I)=GROACC(I)/REAL(NSUM)
          FSINACC(I)=FSINACC(I)/REAL(NSUM)
          FLINACC(I)=FLINACC(I)/REAL(NSUM)
          FLUTACC(I)=FLUTACC(I)/REAL(NSUM)
          TAACC(I)=TAACC(I)/REAL(NSUM)
          UVACC(I)=UVACC(I)/REAL(NSUM)
          PRESACC(I)=PRESACC(I)/REAL(NSUM)
          QAACC(I)=QAACC(I)/REAL(NSUM)

          !ALTOT is the average of the
          ! visible spectrum and infrared spectrum
          ALTOT=(ALVSACC(I)+ALIRACC(I))/2.0
          FSSTAR=FSINACC(I)*(1.-ALTOT)
          FLSTAR=FLINACC(I)-FLUTACC(I)
          QH=HFSACC(I)
          QE=QEVPACC(I)
          BEG=FSSTAR+FLSTAR-QH-QE
          SNOMLT=HMFNACC(I)
          IF(RHOSACC(I).GT.0.0) THEN
            ZSN=SNOACC(I)/RHOSACC(I)
          ELSE
            ZSN=0.0
          ENDIF

          IF(TCANACC(I).GT.0.01) THEN !MAGIC NUMBER
            TCN=TCANACC(I)-TFREZ
          ELSE
            TCN=0.0
          ENDIF

          IF(TSNOACC(I).GT.0.01) THEN ! MAGIC NUMBER
            TSN=TSNOACC(I)-TFREZ
          ELSE
            TSN=0.0
          ENDIF

          IF(ILW.EQ.1) THEN
            GTOUT=GTACC(I)-TFREZ
          ELSE
            GTOUT=0.0
          ENDIF

c see if we are at one of the output points
      DO K=1, WF_NUM_POINTS
        IF(I.EQ.N_OUT(K)) THEN
c figure out nlmos and mlmos of grid square I
          DO N=1, NML
            IF(ILMOS(N).EQ.N_OUT(K)) THEN
              I_OUT=N
            ENDIF
	    ENDDO

          WRITE(150+k*10+1,6100) IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,
     1                       BEG,GTOUT,SNOACC(I),RHOSACC(I),
     2                       WSNOACC(I),ALTOT,ROFACC(I),ROFOACC(I),
     3                       ROFSACC(I),ROFBACC(I)
          WRITE(150+k*10+2,6200) IDAY,IYEAR,(TBARACC(I,J)-TFREZ,
     1                       THLQACC(I,J),THICACC(I,J),J=1,IGND),
     2                       TCN,RCANACC(I),SCANACC(I),TSN,ZSN
          WRITE(150+k*10+3,6300) IDAY,IYEAR,FSINACC(I),FLINACC(I),
     1                       TAACC(I)-TFREZ,UVACC(I),PRESACC(I),
     2                       QAACC(I),PREACC(I),EVAPACC(I)

        ENDIF  !IF(I.EQ.N_OUT(K)) THEN
      ENDDO  !DO K=1, WF_NUM_POINTS

c update components for final tally
      TOTAL_ROFACC=TOTAL_ROFACC+ROFACC(I)
      TOTAL_ROFOACC=TOTAL_ROFOACC+ROFOACC(I)
      TOTAL_ROFSACC=TOTAL_ROFSACC+ROFSACC(I)
      TOTAL_ROFBACC=TOTAL_ROFBACC+ROFBACC(I)
      TOTAL_EVAPACC=TOTAL_EVAPACC+EVAPACC(I)
      TOTAL_PREACC=TOTAL_PREACC+PREACC(I)

C
C     * RESET ACCUMULATOR ARRAYS.
C
          PREACC(I)=0.
          GTACC(I)=0.
          QEVPACC(I)=0.
          HFSACC(I)=0.
          HMFNACC(I)=0.
          ROFACC(I)=0.
          SNOACC(I)=0.
          CANARE(I)=0.
          SNOARE(I)=0.
          ROFOACC(I)=0.
          ROFSACC(I)=0.
          ROFBACC(I)=0.
          WTBLACC(I)=0.
          DO J=1,IGND
              TBARACC(I,J)=0.
              THLQACC(I,J)=0.
              THICACC(I,J)=0.
              THALACC(I,J)=0.
          ENDDO
          ALVSACC(I)=0.
          ALIRACC(I)=0.
          RHOSACC(I)=0.
          TSNOACC(I)=0.
          WSNOACC(I)=0.
          TCANACC(I)=0.
          RCANACC(I)=0.
          SCANACC(I)=0.
          GROACC(I)=0.
          FSINACC(I)=0.
          FLINACC(I)=0.
          TAACC(I)=0.
          UVACC(I)=0.
          PRESACC(I)=0.
          QAACC(I)=0.
          EVAPACC(I)=0.
          FLUTACC(I)=0.
800   CONTINUE

      ENDIF  ! IF(NCOUNT.EQ.48) THEN

      NCOUNT=NCOUNT+1
      NSUM=NSUM+1
      IF(NCOUNT.GT.48) THEN !48 is the last half-hour period of the day
                            ! when they're numbered 1-48
        NCOUNT=1
        NSUM=1
      ENDIF

c *********************************************************************
c Call routing routine
c *********************************************************************


      CALL WF_ROUTE(WF_ROUTETIMESTEP,WF_R1,WF_R2,
     A     WF_NA,WF_NAA,WF_NTYPE,WF_IMAX,WF_JMAX,WF_IYMIN,
     B     WF_IYMAX,WF_JXMIN,WF_JXMAX,WF_YY,WF_XX,WF_IBN,WF_IROUGH,
     C     WF_ICHNL,WF_NEXT,WF_IREACH,WF_AL,WF_GRDN,WF_GRDE,
     D     WF_DA,WF_BNKFLL,WF_CHANNELSLOPE,WF_ELEV,WF_FRAC,
     E     WF_NO,WF_NL,WF_MHRD,WF_KT,WF_IY,WF_JX,
     F     WF_QHYD,WF_RES,WF_RESSTORE,WF_NORESV_CTRL,WF_R,
     G     WF_NORESV,WF_NREL,WF_KTR,WF_IRES,WF_JRES,WF_RESNAME,
     H     WF_B1,WF_B2,WF_QREL, WF_QR,
     I     WF_TIMECOUNT,WF_NHYD,WF_QBASE,WF_QI1,WF_QI2,WF_QO1,WF_QO2,
     J     WF_STORE1,WF_STORE2,
     K     DRIVERTIMESTEP,ROFGRD, NLAT, M_C,M_R,M_S, NLTEST,
     L     WF_S, JAN,IDAY,IHOUR,IMIN)

      DO I=1,WF_NO
        WF_QSYN(I)=WF_QO2(WF_S(I))
        WF_QHYD_AVG(I)=WF_QHYD(I)
      ENDDO

c *********************************************************************
c Write measured and simulated streamflow to file
c *********************************************************************


      IF(NCOUNT.EQ.48) THEN !48 is the last half-hour period of the day
                            ! when they're numbered 1-48
c write out the spl.csv file
        WRITE(70,'(I5,",",F10.3,100(",",F10.3))') IDAY,(WF_QHYD_AVG(I),
     +    WF_QSYN(I),I=1,WF_NO)
c print out spl.csv file to screen
        WRITE(6,'(2I5, F10.3,100(F10.3))') IYEAR,IDAY,(WF_QHYD_AVG(I),
     +    WF_QSYN(I),I=1,WF_NO)
      ENDIF

c *********************************************************************
c Update time counters and return to beginning of main loop
c *********************************************************************

      IMIN = IMIN + 30 ! increment the current time by 30 minutes
      IF (IMIN.EQ.60) THEN
        IMIN = 0
        IHOUR = IHOUR + 1
        IF (IHOUR.EQ.24) THEN
          IHOUR = 0
          IDAY = IDAY + 1
          IF (IDAY >= 366) THEN
            IF (MOD(IYEAR,400) == 0) THEN !LEAP YEAR
              IF (IDAY == 367) THEN
                IDAY = 1
                IYEAR = IYEAR + 1
              ENDIF
            ELSE IF (MOD(IYEAR,100) == 0) THEN !NOT A LEAP YEAR
              IDAY = 1
              IYEAR = IYEAR + 1
            ELSE IF (MOD(IYEAR,4) == 0) THEN !LEAP YEAR
              IF (IDAY == 367) THEN
                IDAY = 1
                IYEAR = IYEAR + 1
              ENDIF
            ELSE !NOT A LEAP YEAR
              IDAY = 1
              IYEAR = IYEAR + 1
            ENDIF
          ENDIF
        ENDIF
      ENDIF

      !check if we should terminate the run yet
      IF (IYEAR >= IYEAR_END .AND. IYEAR_END > 0) THEN
        IF(IYEAR > IYEAR_END) THEN
          GOTO 999
        ELSEIF (IYEAR == IYEAR_END .AND. IDAY >= IDAY_END) THEN
          IF (IDAY > IDAY_END) THEN
            GOTO 999
          ELSEIF (IDAY == IDAY_END .AND. IHOUR >= IHOUR_END) THEN
            IF (IHOUR > IHOUR_END) THEN
              GOTO 999
            ELSEIF (IHOUR == IHOUR_END .AND. IMIN >= IMIN_END) THEN
              GOTO 999
            ENDIF
          ENDIF
        ENDIF
      ENDIF

C=======================================================================
C
      GO TO 200

c *********************************************************************
c Run is now over, print final results to the screen and close files
c *********************************************************************

999   CONTINUE
      PRINT *, 'Reached the end of the forcing data, ',
     +  'MESH_input_forcing.bin'
C Calculate final storage
      FINAL_STORE=0.0
      DO I=1,NLTEST
      DO M=1,NMTEST
		IF(WF_FRAC(I).NE.0.0)THEN
          FINAL_STORE=FINAL_STORE+(RCANROW(I,M)+SCANROW(I,M)+SNOROW(I,M)
     1    +(THLQROW(I,M,1)*RHOW+THICROW(I,M,1)*RHOICE)*
     2          DLZWROW(I,M,1)+ZPNDROW(I,M)*RHOW
     3    +(THLQROW(I,M,2)*RHOW+THICROW(I,M,2)*RHOICE)*DLZWROW(I,M,2)
     4    +(THLQROW(I,M,3)*RHOW+THICROW(I,M,3)*RHOICE)*DLZWROW(I,M,3))
     5    *FAREROW(I,M)
		ENDIF
      ENDDO
      ENDDO

c write out final totals to screen
         WRITE(6,*)
         WRITE(6,'(A,F11.3)') '  Total Precipitation         (mm) = ',
     +        TOTAL_PREACC/TOTAL_AREA
         WRITE(6,'(A,F11.3)') '  Total Evaporation           (mm) = ',
     +        TOTAL_EVAPACC/TOTAL_AREA
         WRITE(6,'(A,F11.3)') '  Total Runoff                (mm) = ',
     +        TOTAL_ROFACC/TOTAL_AREA
         WRITE(6,'(A,3F11.3)') '  Storage(Change/Init/Final)  (mm) = ',
     +        (FINAL_STORE-INIT_STORE)/TOTAL_AREA,
     +        INIT_STORE/TOTAL_AREA,
     +        FINAL_STORE/TOTAL_AREA
         WRITE(6,*)
         WRITE(6,'(A,F11.3)') '  Total Overland flow         (mm) = ',
     +        TOTAL_ROFOACC/TOTAL_AREA
         WRITE(6,'(A,F11.3)') '  Total Interflow             (mm) = ',
     +        TOTAL_ROFSACC/TOTAL_AREA
         WRITE(6,'(A,F11.3)') '  Total Baseflow              (mm) = ',
     +        TOTAL_ROFBACC/TOTAL_AREA
         WRITE(6,*)
         WRITE(6,'(A32)') 'Program has terminated normally.'
         WRITE(6,*)
c write out final totals to file
         WRITE(58,*)
         WRITE(58,'(A,F11.3)') '  Total Precipitation         (mm) = ',
     +        TOTAL_PREACC/TOTAL_AREA
         WRITE(58,'(A,F11.3)') '  Total Evaporation           (mm) = ',
     +        TOTAL_EVAPACC/TOTAL_AREA
         WRITE(58,'(A,F11.3)') '  Total Runoff                (mm) = ',
     +        TOTAL_ROFACC/TOTAL_AREA
         WRITE(58,'(A,3F11.3)')'  Storage(Change/Init/Final)  (mm) = ',
     +        (FINAL_STORE-INIT_STORE)/TOTAL_AREA,
     +        INIT_STORE/TOTAL_AREA,
     +        FINAL_STORE/TOTAL_AREA
         WRITE(58,'(A,F11.3)') '  Total Overland flow         (mm) = ',
     +        TOTAL_ROFOACC/TOTAL_AREA
         WRITE(58,'(A,F11.3)') '  Total Interflow             (mm) = ',
     +        TOTAL_ROFSACC/TOTAL_AREA
         WRITE(58,'(A,F11.3)') '  Total Baseflow              (mm) = ',
     +        TOTAL_ROFBACC/TOTAL_AREA
         WRITE(58,*)
         WRITE(58,*)
         WRITE(58,'(A32)') 'Program has terminated normally.'
         WRITE(58,*)

      CLOSE(UNIT=21)
      CLOSE(UNIT=22)
      CLOSE(UNIT=51)
      CLOSE(UNIT=58)
      CLOSE(UNIT=70)
      close(unit=85) !only for watclass3.0f5
      close(unit=86)
      close(unit=90)

      DO I=1, wf_num_points
        CLOSE(UNIT=150+i*10+1)
        CLOSE(UNIT=150+i*10+2)
        CLOSE(UNIT=150+i*10+3)
        CLOSE(UNIT=150+i*10+4)
        CLOSE(UNIT=150+i*10+5)
        CLOSE(UNIT=150+i*10+6)
        CLOSE(UNIT=150+i*10+7)
        CLOSE(UNIT=150+i*10+8)
        CLOSE(UNIT=150+i*10+9)
      ENDDO

      STOP
      END