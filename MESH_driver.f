      PROGRAM RUNCLASS

C     * SHELL PROGRAM TO RUN "CLASS" ("CANADIAN LAND SURFACE SCHEME") 
C     * VERSION 3.3 IN STAND-ALONE MODE USING SPECIFIED BOUNDARY 
C     * CONDITIONS AND ATMOSPHERIC FORCING.

C     * Stand-alone driver for MESH - Bruce Davison May 21, 2007
C     * Based on the work of Soulis et al. at the University of Waterloo

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
C
      INTEGER,PARAMETER :: NLAT=193,NMOS=8,ILG=NLAT*NMOS
ctvc      INTEGER,PARAMETER :: NLAT=84,NMOS=8,ILG=NLAT*NMOS
cwgc      INTEGER,PARAMETER :: NLAT=193,NMOS=8,ILG=NLAT*NMOS
      INTEGER,PARAMETER :: ICAN=4,IGND=3,ICP1=ICAN+1
      INTEGER,PARAMETER :: M_X=20,M_Y=18,M_S=40,M_R=3,M_C=5
ctvc      INTEGER,PARAMETER :: M_X=16,M_Y=12,M_S=40,M_R=3,M_C=5
cwgc      INTEGER,PARAMETER :: M_X=20,M_Y=18,M_S=40,M_R=3,M_C=5
C
c     watershed related variables
      integer latdegmin,latminmin,latdegmax,latminmax,londegmin,
     +     lonminmin,londegmax,lonminmax
      integer wf_na,wf_naa,wf_ntype,wf_iymin,wf_iymax,wf_jxmin,
     +wf_jxmax,wf_grdn,wf_grde,wf_imax,wf_jmax,wf_nriv

      integer wf_yy(NLAT),wf_xx(NLAT),wf_ibn(NLAT),
     +     wf_irough(NLAT),wf_ichnl(NLAT),wf_next(NLAT),wf_elev(NLAT),
     +     wf_ireach(NLAT)
      real*8 wf_da(NLAT),wf_bnkfll(NLAT),
     +     wf_channelSlope(NLAT),
     +     wf_frac(NLAT),latlength,longlength
      real*8 wf_aclass(NLAT,NMOS), wf_al
	real wf_land_max, wf_land_sum
      integer wf_land_count

c     iostat variable
      integer ios
      
c for output
      integer n_out(5), ii_out(5), wf_num_points
      integer i_out
	character wf_pt_dir(5)*10 , BNAM*12

c     other random variables
      integer ii,JOUT1,JOUT2,JAV1,JAV2,KOUT1,KOUT2,KAV1,KAV2,iy,jx,
     +        smooth
      
c    replacing 'n' with 'nn' so as not to confuse watclass 'n' with CLASS 'N'
      integer nn

c     input met data variables
c     These have to be REAL*4 in order to read in the met data correctly
      REAL*4 r4ShrtGrid2D(M_Y,M_X)
      REAL*4 r4LongGrid2D(M_Y,M_X)
      REAL*4 r4RainGrid2D(M_Y,M_X)
      REAL*4 r4TempGrid2D(M_Y,M_X)
      REAL*4 r4WindGrid2D(M_Y,M_X)
      REAL*4 r4PresGrid2D(M_Y,M_X)
      REAL*4 r4HumdGrid2D(M_Y,M_X)
     
c     streamflow variables
      integer wf_no, wf_nl, wf_mhrd, wf_kt
      integer wf_iy(M_S),wf_jx(M_S), wf_s(M_S)
      real wf_qhyd(M_S),wf_qhyd_avg(M_S),wf_qsyn(M_S)
      character wf_gage(M_S)*8

c     reservoir variables
      integer wf_noresv, wf_nrel, wf_ktr, wf_noresv_ctrl
	integer wf_ires(M_R), wf_jres(M_R), wf_res(M_R), wf_r(M_R)
	real wf_b1(M_R),wf_b2(M_R),wf_qrel(M_R), wf_resstore(M_R)
	character wf_resname(M_R)*8

c for baseflow initialization
      integer jan

c     for routing
      integer wf_RouteTimeStep, wf_TimeCount, DriverTimeStep
	real wf_r1(M_C),wf_r2(M_C),wf_nhyd(NLAT),wf_qbase(NLAT),wf_qi1(NLAT)
	real wf_qi2(NLAT),wf_qo1(NLAT),wf_qo2(NLAT), wf_qr(NLAT)
	real wf_store1(NLAT), wf_store2(NLAT)
c      character ihv*1

      INTEGER IDISP,IZREF,ISLFD,IPCP,IWF,ILAI,IHGT,IALC,
     1        IALS,IALG,N,ILW,ITG,ITC,ITCG,NLTEST,NMTEST,NCOUNT,NSUM,
     2        IHOUR,IMIN,IDAY,IYEAR,NML,NMW,NWAT,NICE,
     3        NLANDCS,NLANDGS,NLANDC,NLANDG,NLANDI,I,J,K,L,M
C
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
     F      ILMOGRD,   UEGRD  ,   HBLGRD 
C
      REAL    HMFGROW(NLAT,NMOS,IGND),   HTCROW (NLAT,NMOS,IGND),
     1        QFCROW (NLAT,NMOS,IGND),
     2        HMFGGAT(ILG,IGND),         HTCGAT (ILG,IGND), 
     3        QFCGAT (ILG,IGND),
     4        HMFGGRD(NLAT,IGND),        HTCGRD (NLAT,IGND),
     5        QFCGRD (NLAT,IGND)
C
      INTEGER     ITCTROW(NLAT,NMOS,6,50),  ITCTGAT(ILG,6,50)
      INTEGER     ISUM(6)
 
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
C
C     * CONSTANTS AND TEMPORARY VARIABLES.
C
      REAL DEGLAT,DEGLON,JLAT,FSDOWN,ZLEV,DAY,DECL,HOUR,COSZ,
     1     QSUMV,QSUMS,QSUM1,QSUM2,QSUM3,WSUMV,WSUMS,WSUMG,ALTOT,
     2     FSSTAR,FLSTAR,QH,QE,BEG,SNOMLT,ZSN,TCN,TSN,TPN,GTOUT
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
C=======================================================================
C     * PHYSICAL CONSTANTS.
C     * PARAMETERS IN THE FOLLOWING COMMON BLOCKS ARE NORMALLY DEFINED
C     * WITHIN THE GCM.

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

	INTEGER lzfflg,extflg,iwfice,errflg,iwfoflw
      REAL viceflg,psi_limit,hiceflg

      COMMON /WATFLGS/ viceflg,psi_limit,hiceflg,lzfflg,extflg,iwfice,
     +errflg,IMIN,IHOUR,IDAY,IYEAR

      DATA viceflg/3.0/,psi_limit/1.0/,hiceflg/1.0/,lzfflg/0/,extflg/0/,
     +     iwfice/3/,errflg/1/

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
C     * IF IWF=n (0<n<4), THE WATFLOOD CALCULATIONS OF OVERLAND FLOW 
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

C=======================================================================
C     * OPEN FILES FOR READING AND WRITING.

      OPEN(UNIT=50,FILE='MESH_parameters_CLASS.ini',
     1             STATUS='OLD')
      OPEN(UNIT=51,FILE='MESH_input_forcing.bin',STATUS='OLD',
     +           FORM='UNFORMATTED')
      OPEN(UNIT=52,FILE='MESH_input_soil_levels.txt',STATUS='OLD')
      OPEN(UNIT=70,FILE='MESH_output_streamflow.csv')

C     * READ AND PROCESS INITIALIZATION AND BACKGROUND INFORMATION.
C
!      DO 25 J=1,IGND
!          READ(52,5002) DELZ(J),ZBOT(J)
! 25   CONTINUE
!
!5002  FORMAT(2X,2F8.2)

       DO 26 J=1,IGND
           READ(52,5003) DELZ(J)
  26   CONTINUE
    
5003   FORMAT(2X,F8.2)

       ZBOT(1)=DELZ(1)
       ZBOT(2)=DELZ(1)+DELZ(2)
       ZBOT(3)=DELZ(1)+DELZ(2)+DELZ(3)
      
      READ (50,5010) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      READ (50,5010) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      READ (50,5010) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      READ(50,5020) DEGLAT,DEGLON,ZRFMGRD(1),ZRFHGRD(1),ZBLDGRD(1),
     1              GCGRD(1),ILW,NLTEST,NMTEST
      JLAT=NINT(DEGLAT)

C READ IN TOP OF SHED FILE TO GET WATERSHED PARAMETERS
      open(unit=20,file='MESH_drainage_database.txt',status='old')

      read(20,'(i5,50X,i5)') wf_na,wf_naa
      read(20,'(f10.0,5X,2i5)') wf_al,wf_nriv,wf_ntype

c set NLTEST to wf_na no matter what value was read in from the CLASS.INI file
      NLTEST=wf_na

c check if number of land classes in CLASS.INI file matches up with number in WATFLOOD.SHD file
      if(wf_ntype.ne.NMTEST.and.wf_ntype.gt.0) then
      print *, 'number of land classes from CLASS.INI file: ',NMTEST
      print *, 'no. of land classes from WATFLOOD.SHD file: ',wf_ntype
      print *, 'Hands up if we see a problem here.'
	stop
	endif

c if there is a negative number in the WATFLOOD.shd file make sure there are enough classes
c in the CLASS.INI file to cover it
      if(abs(wf_ntype).gt.NMTEST.and.wf_ntype.lt.0) then
      print *, 'number of land classes from CLASS.INI file: ',NMTEST
      print *, 'no. of land classes from WATFLOOD.SHD file:',wf_ntype
      print *, 'Please adjust one of these file.'
	stop
	endif

c check if we are going to get an "array bounds out of range" error
      if(wf_na.gt.NLAT) then
      print *, 'size of grid arrays in WATCLASS: ',NLAT
      print *, 'number of grids from WATFLOOD.SHD file: ',wf_na
      print *, 'Hands up if we see a problem here.'
      stop
	endif

      if(wf_ntype.gt.NMOS) then
      print *, 'size of grid arrays in WATCLASS: ',NMOS
      print *, 'no. of land classes from WATFLOOD.SHD file: ',wf_ntype
      print *, 'Hands up if we see a problem here.'
      stop
	endif

c now that we know they are equal, we will not use wf_na and wf_ntype anymore
c however we will now adjust wf_naa to the be number of outlet squares
      wf_naa=NLTEST-wf_naa

c============
c Lines moved from here ....Saul
c============

c May 20, 2007 - Pablo Dornes and Bruce Davison
c We found a problem with the calculation of deglat and deglon. We need to ask
c Ric about how he calculates this because it really messes up the results.
c As with the comment below, these changes are from watclass3f8.

c for now set all latitudes, longitudes, and references the same for all grids
c      do i=1, nltest
c      RADJGRD(i)=DEGLAT*PI/180.
c      DLONGRD(i)=DEGLON
c      ZRFMGRD(i)=ZRFMGRD(1)
c	ZRFHGRD(i)=ZRFHGRD(1)
c	ZBLDGRD(i)=ZBLDGRD(1)
c      GCGRD(i)=GCGRD(1)

c      Z0ORGRD(I)=0.0
c      GGEOGRD(I)=0.0


c      enddo


	      I=1
      DO 50 M=1,NMTEST
c read in MESH_parameters_CLASS.INI file
          READ(50,5040) (FCANROW(I,M,J),J=1,ICAN+1),(LAMXROW(I,M,J),
     1                  J=1,ICAN)
          READ(50,5040) (LNZ0ROW(I,M,J),J=1,ICAN+1),(LAMNROW(I,M,J),
     1                  J=1,ICAN)
          READ(50,5040) (ALVCROW(I,M,J),J=1,ICAN+1),(CMASROW(I,M,J),
     1                  J=1,ICAN)
          READ(50,5040) (ALICROW(I,M,J),J=1,ICAN+1),(ROOTROW(I,M,J),
     1                  J=1,ICAN)
          READ(50,5030) (RSMNROW(I,M,J),J=1,ICAN),
     1                  (QA50ROW(I,M,J),J=1,ICAN)
          READ(50,5030) (VPDAROW(I,M,J),J=1,ICAN),
     1                  (VPDBROW(I,M,J),J=1,ICAN)
          READ(50,5030) (PSGAROW(I,M,J),J=1,ICAN),
     1                  (PSGBROW(I,M,J),J=1,ICAN)
          READ(50,5041) DRNROW(I,M),SDEPROW(I,M),FAREROW(I,M),
     1                  DDROW(I,M)
          READ(50,5090) XSLPROW(I,M),GRKFROW(I,M),MANNROW(I,M),
     1                  WFCIROW(I,M),MIDROW(I,M)
          READ(50,5080) (SANDROW(I,M,J),J=1,3)
          READ(50,5080) (CLAYROW(I,M,J),J=1,3)
          READ(50,5080) (ORGMROW(I,M,J),J=1,3)
          READ(50,5050) (TBARROW(I,M,J),J=1,3),TCANROW(I,M),
     1                  TSNOROW(I,M),TPNDROW(I,M)
          READ(50,5060) (THLQROW(I,M,J),J=1,3),(THICROW(I,M,J),
     1                  J=1,3),ZPNDROW(I,M)
          READ(50,5070) RCANROW(I,M),SCANROW(I,M),SNOROW(I,M),
     1                  ALBSROW(I,M),RHOSROW(I,M),GROROW(I,M)
50    CONTINUE

C     * CHECK TO SEE IF THERE IS AN MESH_parameters_hydrology.ini FILE

      open(unit=23,file='MESH_parameters_hydrology.ini',
     +status='old',iostat=ios)

      if(ios.ne.0)then

        print*,'The MESH_parameters_hydrology.ini file was NOT found'
        print*,'Please create one and restart the program'
        print*
        stop
      else

      read (23,*)
      read (23,*)
      read (23,"(5F6.3)") (wf_r2(i),i=1,5)
      read (23,*)
      I=1
      DO 55 M=1,NMTEST
c read D100 and ponding depths for each GRU.
      read (23,"(3F7.4)") ZSNLROW(I,M),ZPLSROW(I,M),ZPLGROW(I,M)
55    CONTINUE
      read (23,*)
      read (23,"(I5)") wf_num_points
c make sure there are no more than 5 outpoints
      if(wf_num_points.gt.5) then
	print *, 'You can only have up to 5 output points'
	print *, 'Please adjust the watclass.ini file'
	stop
	endif
      read (23,*)
      read (23,*)
      read (23,*)
      read (23,*)

      do i=1, wf_num_points
      read (23,10) wf_pt_dir(i), n_out(i), ii_out(i)
      enddo

 10   format(A10,2I7)
 
      print *, 'Found these output locations:'
      print *, 'Output Directory, grid number, land class number'
      do i=1, wf_num_points
      print *, wf_pt_dir(i),n_out(i),ii_out(i)         
      enddo
      print *

      close(23)

c this might seem silly, but it is probably better to now put all the variables
c that were read in to the same value as the first value
c if the value of wf_ntype is less than zero then take that the value from the CLASS.INI
c file for that land class as the only land class
c bjd - This would be a good spot for setting pre-distributed values
 
      DO I=2,NLTEST
      DO M=1,NMTEST
         do j=1, ican+1
      FCANROW(I,M,J)=   FCANROW(1,M,J)
      LNZ0ROW(I,M,J)=   LNZ0ROW(1,M,J)
      ALVCROW(I,M,J)=   ALVCROW(1,M,J)
      ALICROW(I,M,J)=   ALICROW(1,M,J)
	enddo       

	do j=1, ican
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
      enddo       

      do j=1,3    
	SANDROW(I,M,J)=   SANDROW(1,M,J)
      CLAYROW(I,M,J)=   CLAYROW(1,M,J)
      ORGMROW(I,M,J)=   ORGMROW(1,M,J)
      enddo       

	do j=1,ignd 
      TBARROW(I,M,J)=   TBARROW(1,M,J)
      THLQROW(I,M,J)=   THLQROW(1,M,J)
	THICROW(I,M,J)=   THICROW(1,M,J)
      enddo           

c  set apprpriate values for all D100's and ponding limits in the basin	
      ZSNLROW(I,M)=     ZSNLROW(1,M)
      ZPLSROW(I,M)=     ZPLSROW(1,M)
      ZPLGROW(I,M)=     ZPLGROW(1,M)

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
      enddo
	enddo

c check that run points are in the basin and that there are no repeats

      do i=1, wf_num_points
        if(n_out(i).gt.NLTEST) then
         print *, 'number of grids from WATFLOOD.SHD file: ',NLTEST
         print *, 'out point ',i,' from watclass.ini file is: ',n_out(i)
         print *, 'please adjust watclass.ini file'
	   stop
	  endif

      if(i.lt.wf_num_points) then
	  do j=i+1,wf_num_points
          if(n_out(i).eq.n_out(j)) then
	      print *, 'grid number ', n_out(i)
	      print *, 'is repeated in watclass.ini file'
            print *, 'please adjust watclass.ini file'
	      stop
	    endif
        enddo
	endif
      
	enddo

      endif !    if(ios.ne.0)then


c Set up the OF* files to print out into the correct directory
c these lines cause a problem in the red hat compiler, so remove for now
      if(.TRUE.) then ! bjd - june 29, 2004
      do i=1, wf_num_points
      BNAM=wf_pt_dir(i)
      OPEN(UNIT=150+i*10+1,FILE='./'//BNAM(1:INDEX(BNAM,' ')-1)//
     +'/CLASSOF1.of')
      OPEN(UNIT=150+i*10+2,FILE='./'//BNAM(1:INDEX(BNAM,' ')-1)//
     +'/CLASSOF2.of')
      OPEN(UNIT=150+i*10+3,FILE='./'//BNAM(1:INDEX(BNAM,' ')-1)//
     +'/CLASSOF3.of')
      OPEN(UNIT=150+i*10+4,FILE='./'//BNAM(1:INDEX(BNAM,' ')-1)//
     +'/CLASSOF4.of')
      OPEN(UNIT=150+i*10+5,FILE='./'//BNAM(1:INDEX(BNAM,' ')-1)//
     +'/CLASSOF5.of')
      OPEN(UNIT=150+i*10+6,FILE='./'//BNAM(1:INDEX(BNAM,' ')-1)//
     +'/CLASSOF6.of')
      OPEN(UNIT=150+i*10+7,FILE='./'//BNAM(1:INDEX(BNAM,' ')-1)//
     +'/CLASSOF7.of')
      OPEN(UNIT=150+i*10+8,FILE='./'//BNAM(1:INDEX(BNAM,' ')-1)//
     +'/CLASSOF8.of')
      OPEN(UNIT=150+i*10+9,FILE='./'//BNAM(1:INDEX(BNAM,' ')-1)//
     +'/CLASSOF9.of')

      WRITE(150+i*10+1,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(150+i*10+1,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(150+i*10+1,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(150+i*10+1,6011)
6011  FORMAT('IDAY IYEAR FSSTAR FLSTAR QH QE SNOMLT BEG GTOUT 
     1 SNOACC(I) RHOSACC(I) WSNOACC(I) ALTOT ROFACC(I) ROFOACC(I) 
     3 ROFSACC(I) ROFBACC(I)')
      WRITE(150+i*10+2,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(150+i*10+2,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(150+i*10+2,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(150+i*10+2,6012)
6012  FORMAT('IDAY,IYEAR,TBARACC(I 1)-TFREZ,THLQACC(I 1),THICACC(I 1),
     1 TBARACC(I 2)-TFREZ,THLQACC(I 2),THICACC(I 2),TBARACC(I 3)-TFREZ,
     2 THLQACC(I 3),THICACC(I 3),TCN,RCANACC(I),SCANACC(I),TSN,ZSN')
      WRITE(150+i*10+3,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(150+i*10+3,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(150+i*10+3,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(150+i*10+3,6013)
6013  FORMAT('IDAY,IYEAR,FSINACC(I),FLINACC(I),TAACC(I)-TFREZ,UVACC(I),
     1 PRESACC(I),QAACC(I),PREACC(I),EVAPACC(I)')
      WRITE(150+i*10+4,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(150+i*10+4,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(150+i*10+4,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(150+i*10+4,6014)
6014  FORMAT('IHOUR,IMIN,IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,BEG,
     1 GTOUT,SNOROW(I M),RHOSROW(I M),WSNOROW(I M),ALTOT,ROFROW(I M),
     2 TPN,ZPNDROW(I M)')                  
      WRITE(150+i*10+5,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(150+i*10+5,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(150+i*10+5,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(150+i*10+5,6015)
6015  FORMAT('IHOUR,IMIN,IDAY,IYEAR,TBARROW(I M 1)-TFREZ,THLQROW(I M 1),
     1 THICROW(I M 1),TBARROW(I M 2)-TFREZ,THLQROW(I M 2),
     2 THICROW(I M 2),TBARROW(I M 3)-TFREZ,THLQROW(I M 3),
     3 THICROW(I M 3),TCN,RCANROW(I M),SCANROW(I M),TSN,ZSN')
      WRITE(150+i*10+6,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(150+i*10+6,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(150+i*10+6,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(150+i*10+6,6016)
6016  FORMAT('IHOUR,IMIN,IDAY,FSDOWN,FDLGRD(I),PREGRD(I),TAGRD(I)-TFREZ,
     1 UVGRD(I),PRESGRD(I),QAGRD(I)')
      WRITE(150+i*10+7,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(150+i*10+7,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(150+i*10+7,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(150+i*10+7,6017)
6017  FORMAT('TROFROW(I M),TROOROW(I M),TROSROW(I M),TROBROW(I M),
     1 ROFROW(I M),ROFOROW(I M),ROFSROW(I M),ROFBROW(I M),
     2 FCS(I),FGS(I),FC(I),FG(I)')
      WRITE(150+i*10+8,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(150+i*10+8,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(150+i*10+8,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(150+i*10+8,6018)
6018  FORMAT('FSGVROW(I M),FSGSROW(I M),FSGGROW(I M),FLGVROW(I M),
     1 FLGSROW(I M),FLGGROW(I M),HFSCROW(I M),HFSSROW(I M),HFSGROW(I M),
     2 HEVCROW(I M),HEVSROW(I M),HEVGROW(I M),HMFCROW(I M),HMFNROW(I M),
     3 HMFGROW(I M 1),HMFGROW(I M 2),HMFGROW(I M 3),HTCCROW(I M),
     4 HTCSROW(I M),HTCROW(I M 1),HTCROW(I M 2),HTCROW(I M 3)')
      WRITE(150+i*10+9,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
      WRITE(150+i*10+9,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
      WRITE(150+i*10+9,6003) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
      WRITE(150+I*10+9,6019)
6019  FORMAT('PCFCROW(I M),PCLCROW(I M),PCPNROW(I M),PCPGROW(I M),
     1 QFCFROW(I M),QFCLROW(I M),QFNROW(I M),QFGROW(I M),QFCROW(I M 1),
     2 QFCROW(I M 2),QFCROW(I M 3),ROFCROW(I M),ROFNROW(I M),
     3 ROFOROW(I M),ROFROW(I M),WTRCROW(I M),WTRSROW(I M),WTRGROW(I M)')
      enddo
      endif

C READ IN REST OF WATFLOOD.SHD FILE
      wf_grdn=0.0
      wf_grde=0.0
c     IOSTAT capture prevents error when not all values present
      read(20,'(12i5,2f5.0)',IOSTAT=ios) wf_iymin,wf_iymax,
     +     wf_jxmin,wf_jxmax,latdegmin,latminmin,latdegmax,latminmax,
     +     londegmin,lonminmin,londegmax,lonminmax,wf_grdn,wf_grde

c     Condition for Lat/Long by Frank S Sept/1999
      if( wf_grdn.gt.0.0 ) then
         wf_iymin=latdegmin*60+latminmin
         wf_iymax=latdegmax*60+latminmax
         wf_jxmin=londegmin*60+lonminmin
         wf_jxmax=londegmax*60+lonminmax
      else
c        Define wf_grdn & wf_grde for UTM
         wf_grdn=wf_al/1000.
         wf_grde=wf_al/1000.
      endif

      read(20,'(2i5)') wf_imax,wf_jmax

c check if we are going to get an "array bounds out of range" error
      if(wf_imax.gt.M_Y) then
      print *, 'size of grid arrays in WATCLASS: ',M_Y
      print *, 'number up/down grids from WATFLOOD.SHD file: ',wf_imax
      print *, 'Hands up if we see a problem here.'
      stop
	endif

      if(wf_jmax.gt.M_X) then
      print *, 'size of grid arrays in WATCLASS: ',M_X
      print *, 'no. of east/west frids from WATFLOOD.SHD file: ',wf_jmax
      print *, 'Hands up if we see a problem here.'
      stop
	endif

      do i = wf_imax,1,-1
         read(20,*)
      enddo

      do nn = 1,NLTEST
         read(20,'(5x,2i5,3f10.5,I7,5i5,F5.2,5x,15f5.2)')wf_yy(nn),
     +        wf_xx(nn),wf_da(nn),wf_bnkfll(nn),wf_channelSlope(nn),
     +        wf_elev(nn),wf_ibn(nn),wf_irough(nn),wf_ichnl(nn),
     +        wf_next(nn),wf_ireach(nn),wf_frac(nn),
     +        (wf_aclass(nn,ii),ii=1,NMTEST)

C quick check to make sure land cover areas sum to 100% 
         wf_land_count=1
         wf_land_max=0.0         
         wf_land_sum=0.0
         do ii=1,NMTEST
           wf_land_sum=wf_land_sum+wf_aclass(nn,ii)
           if(wf_aclass(nn,ii).gt.wf_land_max) then
	       wf_land_count=ii
    	       wf_land_max=wf_aclass(nn,ii)
	     endif
	   enddo
	   if(wf_land_sum.ne.1.0) wf_aclass(nn,wf_land_count)=
     +   wf_aclass(nn,wf_land_count)-(wf_land_sum-1.0)
      enddo
      close(unit=20)
c=================
c=================
c now assign values of lat/long to each square (bjd - not sure how this works... took it directly from watflow.f)
      do i=1, nltest

      latlength=wf_al/1000./(111.136-0.5623*cos(2*(DEGLAT*PI/180.0))+
     +0.0011*cos(4*(DEGLAT*PI/180.0)))
	longlength=wf_al/1000./(111.4172*cos((DEGLAT*PI/180.0))-
     +0.094*cos(3*(DEGLAT*PI/180.0))+0.0002*cos(5*(DEGLAT*PI/180.0)))

      RADJGRD(i)=( (deglat-(real(wf_imax)/2.0)*latlength)
     ++(wf_yy(i)-0.5)*latlength)*PI/180.
      DLONGRD(i)=(deglon-(real(wf_jmax)/2.0)*longlength) 
     ++(wf_xx(i)-0.5)*longlength
      ZRFMGRD(i)=ZRFMGRD(1)
	ZRFHGRD(i)=ZRFHGRD(1)
	ZBLDGRD(i)=ZBLDGRD(1)
      GCGRD(i)=GCGRD(1)
      Z0ORGRD(I)=0.0
      GGEOGRD(I)=0.0
      ZDMGRD(I)=10.0
      ZDHGRD(I)=2.0
      enddo
c===============
c ******************************************************
c echo print class.ini file to watflow_input.txt START
c ******************************************************

      OPEN(UNIT=58,FILE='MESH_input_echo_print.txt')

      WRITE(58,5040) RADJGRD(1),DLONGRD(1),ZRFMGRD(1),
     +ZRFHGRD(1),ZBLDGRD(1),GCGRD(1)
      WRITE(58,*)

      WRITE(58,"(A11)") 'MESH_parameters_CLASS.INI :'
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
      WRITE(58,5080) (SANDROW(I,M,J),J=1,3)
      WRITE(58,5080) (CLAYROW(I,M,J),J=1,3)
      WRITE(58,5080) (ORGMROW(I,M,J),J=1,3)
      WRITE(58,5050) (TBARROW(I,M,J),J=1,IGND),TCANROW(I,M),
     1                  TSNOROW(I,M),TPNDROW(I,M)
      WRITE(58,5060) (THLQROW(I,M,J),J=1,IGND),(THICROW(I,M,J),
     1                  J=1,IGND),ZPNDROW(I,M)
      WRITE(58,5070) RCANROW(I,M),SCANROW(I,M),SNOROW(I,M),
     1                  ALBSROW(I,M),RHOSROW(I,M),GROROW(I,M)
      WRITE(58,*)
      ENDDO


C PRINT TO watflow_input.txt
      WRITE(58,"(A14)") 'MESH_parameters_hydrology.ini :'
	write (58,"(6F6.3,' r2 values & factor')")(wf_r2(i),i=1,5)
	write (58,"(I5,' wf_num_points')") wf_num_points
      do i=1, wf_num_points
	write (58,10) wf_pt_dir(i), n_out(i), ii_out(i)
      enddo
      write (58,*)

      close(58)

c ******************************************************
c echo print class.ini file to watflow_input.txt END
c ******************************************************

c==============
c===============
c now that we have the *.shd file we have to override the FAREROW value because it is
c just easier to use the values from the *.shd file (Ted Whidden's solution, Frank and Ric
c have many other ways of handling farerow for different situations.)

      DO I=1,NLTEST
      DO M=1,NMTEST
         FAREROW(I,M)= wf_aclass(i,m) * wf_frac(i)     
      ENDDO
      ENDDO

c read in reservoir file
       open(unit=21,file='MESH_input_reservoir.txt',status='old')
		   read(21,'(3i5)') wf_noresv,wf_nrel,wf_ktr
      wf_noresv_ctrl=0             ! Number of controlled reservoirs

      if( wf_noresv.gt.0 ) then
         do i=1,wf_noresv
         read(21,'(2i5,2g10.3,25x,a12,i2)') wf_ires(i),wf_jres(i),
     +        wf_b1(i),wf_b2(i),wf_resname(i), wf_res(i)
        wf_ires(i)=int((real(wf_ires(i))-real(wf_iymin))/wf_grdn+1.0)
        wf_jres(i)=int((real(wf_jres(i))-real(wf_jxmin))/wf_grde+1.0)

c check if point is in watershed and in river reaches
         wf_r(i)=0
           do j=1,NLTEST
             if( wf_ires(i).eq.wf_yy(j).and.wf_jres(i).eq.wf_xx(j))then
             wf_r(i)=j
             endif
           enddo
         if(wf_r(i).eq.0) then
         print *, 'Reservoir Station: ',i,' is not in the basin'
	   print *, 'Up/Down Coordinate: ', wf_ires(i)
	   print *, 'Left/Right Coordinate: ', wf_jres(i)
         stop
	   endif
         if(wf_ireach(wf_r(i)).ne.i) then
         print *, 'Reservoir Station: ',i,' is not in the correct reach'
	   print *, 'Up/Down Coordinate: ', wf_ires(i)
	   print *, 'Left/Right Coordinate: ', wf_jres(i)
	   print *, 'ireach value at station: ', wf_iy(i)
         stop
	   endif
            if( wf_b1(i).eq.0.0 ) then
               wf_noresv_ctrl=wf_noresv_ctrl+1
            endif
         end do
       endif
c leave file open and read in the reservoir files when needed
     


C READ IN STREAMFLOW FILES

      open(unit=22,file='MESH_input_streamflow.txt',status='old')
      read(22,*) 
      read(22,'(4i5)') wf_no,wf_nl,wf_mhrd,wf_kt
	do i=1,wf_no
         read(22,'(2i5,1x,a12)')wf_iy(i),wf_jx(i),wf_gage(i)
         wf_iy(i)=int((real(wf_iy(i))-real(wf_iymin))/wf_grdn+1.0)
         wf_jx(i)=int((real(wf_jx(i))-real(wf_jxmin))/wf_grde+1.0)
      end do

         do l=1,wf_no
         wf_s(l)=0
           do i=1,NLTEST
             if( wf_iy(l).eq.wf_yy(i).and.wf_jx(l).eq.wf_xx(i) ) then
             wf_s(l)=i
             endif
           enddo
         if(wf_s(l).eq.0) then
         print *, 'Streamflow gauge: ',l,' is not in the basin'
	   print *, 'Up/Down Coordinate: ', wf_iy(l)
	   print *, 'Left/Right Coordinate: ', wf_jx(l)
         stop
	   endif

cric     initialise smoothed variables
         wf_qsyn(l)=0.0
	   wf_qhyd_avg(l)=0.0

         enddo

c leave unit open and read new streamflow each hour

c routing parameters
      wf_RouteTimeStep=900
      wf_TimeCount=0           
      DriverTimeStep=DELT    ! Be sure it's REAL*8
	jan=1  !jan=1 first time through, jan=2 after that

C PRINT OUT OF HEADER INFORMATION

      PRINT *, 'NUMBER OF GRID SQUARES: ',NLTEST
	PRINT *, 'NUMBER OF LAND CLASSES (WITH IMPERVIOUS): ', NMTEST
	PRINT *, 'NUMBER OF RIVER CLASSES: ', wf_nriv
	PRINT *, 'MINIMUM NUMBER FOR ILG: ',NLTEST*NMTEST
      PRINT *, 'NUMBER OF GRID SQUARES IN X DIRECTION: ', WF_IMAX
      PRINT *, 'NUMBER OF GRID SQUARES IN Y DIRECTION: ', WF_JMAX
      PRINT *, 'LENGTH OF SIDE OF GRID SQUARE IN M: ', wf_al
	PRINT *, 'NUMBER OF DRAINAGE OUTLETS: ', wf_naa
	PRINT *, 'NUMBER OF STREAMFLOW GUAGES: ', wf_no
         do i=1,wf_no
      PRINT *,'STREAMFLOW STATION: ',I,'I: ',wf_iy(i),'J: ',wf_jx(i)
         ENDDO
	PRINT *, 'NUMBER OF RESERVOIR STATIONS: ', wf_noresv
      if( wf_noresv.gt.0 ) then
         do i=1,wf_noresv
      PRINT *,'RESERVOIR STATION: ',I,'I: ',wf_ires(i),'J: ',wf_jres(i)
         ENDDO
      endif


	PRINT *
	PRINT *, 'DONE INTITIALIZATION'
      PRINT *
	PRINT *, 'STARTING MESH'

C END OF HEADER INFO PRINT OUT

      DO 75 I=1,NLTEST
      DO 75 M=1,NMTEST
          TBARROW(I,M,1)=TBARROW(I,M,1)+TFREZ
          TBARROW(I,M,2)=TBARROW(I,M,2)+TFREZ
          TBARROW(I,M,3)=TBARROW(I,M,3)+TFREZ
          TSNOROW(I,M)=TSNOROW(I,M)+TFREZ
          TCANROW(I,M)=TCANROW(I,M)+TFREZ
          TPNDROW(I,M)=TPNDROW(I,M)+TFREZ
          TBASROW(I,M)=TBARROW(I,M,3)
          CMAIROW(I,M)=0.
          WSNOROW(I,M)=0.
c          ZSNLROW(I,M)=0.10
          TSFSROW(I,M,1)=TFREZ
          TSFSROW(I,M,2)=TFREZ
          TSFSROW(I,M,3)=TBARROW(I,M,1)
          TSFSROW(I,M,4)=TBARROW(I,M,1)
          TACROW (I,M)=TCANROW(I,M)
          QACROW (I,M)=0.5E-2
          IF(IGND.GT.3)                                 THEN
              DO 65 J=4,IGND
                  THLQROW(I,M,J)=THLQROW(I,M,3)
                  THICROW(I,M,J)=THICROW(I,M,3)
                  TBARROW(I,M,J)=TBARROW(I,M,3)
                  IF(SDEPROW(I,M).LT.(ZBOT(J-1)+0.001) .AND.
     1                  SANDROW(I,M,3).GT.-2.5)     THEN
                      SANDROW(I,M,J)=-3.0
                      CLAYROW(I,M,J)=-3.0
                      ORGMROW(I,M,J)=-3.0
                  ELSE
                      SANDROW(I,M,J)=SANDROW(I,M,3)
                      CLAYROW(I,M,J)=CLAYROW(I,M,3)
                      ORGMROW(I,M,J)=ORGMROW(I,M,3)
                  ENDIF
65            CONTINUE
          ENDIF
          DO 70 K=1,6
          DO 70 L=1,50
              ITCTROW(I,M,K,L)=0
70        CONTINUE
75    CONTINUE
      
      DO 150 I=1,NLTEST
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
          DO 100 J=1,IGND
              TBARACC(I,J)=0.
              THLQACC(I,J)=0.
              THICACC(I,J)=0.
              THALACC(I,J)=0.
100       CONTINUE
150   CONTINUE

      CALL CLASSB(THPROW,THRROW,THMROW,BIROW,PSISROW,GRKSROW,
     1            THRAROW,HCPSROW,TCSROW,THFCROW,PSIWROW,
     2            DLZWROW,ZBTWROW,ALGWROW,ALGDROW,
     3            SANDROW,CLAYROW,ORGMROW,DELZ,ZBOT,
     4            SDEPROW,ISNDROW,
     5            IORG,NLAT,NMOS,NLTEST,NMTEST,IGND)

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
C5300  FORMAT(1X,2I2,I5,I6,2F9.2,E14.4,F9.2,E12.3,F8.2,F12.2,3F9.2,
5300  FORMAT(1X,I2,I3,I5,I6,2F9.2,E14.4,F9.2,E12.3,F8.2,F12.2,3F9.2,
     1       F9.4)
6001  FORMAT('CLASS TEST RUN:     ',6A4)
6002  FORMAT('RESEARCHER:         ',6A4)
6003  FORMAT('INSTITUTION:        ',6A4)

C=======================================================================
C     * LAUNCH RUN.

      READ(50,5200) JOUT1,JOUT2,JAV1,JAV2 
      READ(50,5200) KOUT1,KOUT2,KAV1,KAV2 
C READ IN HOUR, MINUTE, DAY AND YEAR FROM CLASS.INI FILE AS IT IS
C NOT PRESENT IN THE MET FILES
      READ(50,5200) IHOUR,IMIN,IDAY,IYEAR
      NCOUNT=IHOUR*2+IMIN/30+1
      NSUM=1

C CLOSE UNIT 50 AS WE DON'T NEED ANYTHING ELSE FROM THE CLASS.INI FILE
      CLOSE(50)

200   CONTINUE
C
C========================================================================
C     * READ IN METEOROLOGICAL FORCING DATA FOR CURRENT TIME STEP;
C     * CALCULATE SOLAR ZENITH ANGLE AND COMPONENTS OF INCOMING SHORT-
C     * WAVE RADIATION FLUX; ESTIMATE FLUX PARTITIONS IF NECESSARY.
C
      N=N+1
C      DO 250 I=1,NLTEST
C          READ(51,5300,END=999) IHOUR,IMIN,IDAY,IYEAR,FSDOWN,FDLGRD(I),
CC     1         PREGRD(I),TAGRD(I),QAGRD(I),UVGRD(I),PRESGRD(I),ZLEV
C     1         PREGRD(I),TAGRD(I),QAGRD(I),UVGRD(I),PRESGRD(I)
C          FSVHGRD(I)=0.5*FSDOWN
C          FSIHGRD(I)=0.5*FSDOWN
C          TAGRD(I)=TAGRD(I)+TFREZ
C          ULGRD(I)=UVGRD(I)
C          VLGRD(I)=0.0
C          UVGRD(I)=MAX(VMIN,UVGRD(I))
CC          ZRFMGRD(I)=ZLEV
CC          ZRFHGRD(I)=ZLEV
CC          ZBLDGRD(I)=ZLEV
C250   CONTINUE

C SECTION TO READ IN MET DATA FROM THE BINARY FILE

c   Use this for hourly forcing data
         IF( MOD(IHOUR,1).EQ.0 .AND. IMIN.EQ.0 ) THEN
c   Use this for 30 minute forcing data
c         IF( MOD(IHOUR,1).EQ.0 ) THEN
         READ(51,END=999) ((r4ShrtGrid2D(i,j),j=1,wf_jmax),i=1,wf_imax)
         READ(51,END=999) ((r4LongGrid2D(i,j),j=1,wf_jmax),i=1,wf_imax)
         READ(51,END=999) ((r4RainGrid2D(i,j),j=1,wf_jmax),i=1,wf_imax)
         READ(51,END=999) ((r4TempGrid2D(i,j),j=1,wf_jmax),i=1,wf_imax)
         READ(51,END=999) ((r4WindGrid2D(i,j),j=1,wf_jmax),i=1,wf_imax)
         READ(51,END=999) ((r4PresGrid2D(i,j),j=1,wf_jmax),i=1,wf_imax)
         READ(51,END=999) ((r4HumdGrid2D(i,j),j=1,wf_jmax),i=1,wf_imax)
           DO I=1,NLTEST
              iy = wf_yy(i)
              jx = wf_xx(i)
	        FSDOWN=r4ShrtGrid2D(iy,jx)
              FSVHGRD(I)=0.5*r4ShrtGrid2D(iy,jx)
              FSIHGRD(I)=FSVHGRD(I)
              FDLGRD(I)=r4LongGrid2D(iy,jx)
              PREGRD(I)=r4RainGrid2D(iy,jx)  
c don't know about adding tfrez or not             
              TAGRD(I)=r4TempGrid2D(iy,jx)
              ULGRD(I)=r4WindGrid2D(iy,jx)
              VLGRD(I)=0.0 
              UVGRD(I)=MAX(VMIN,ULGRD(I))
              PRESGRD(I)=r4PresGrid2D(iy,jx)
              QAGRD(I)=r4HumdGrid2D(iy,jx)
           END DO
         ENDIF

c only read in current value if we are on the correct time step
c however put in an exception if this is the first time through (ie. jan=1),
c otherwise depending on the hour of the first time step there might not be any data in wf_qrel, wf_qhyd
c make sure we have a controlled reservoir (if not the mod(IHOUR,wf_ktr) may give an error Frank S Jun 2007
      if(wf_noresv_ctrl.gt.0) then
        if(mod(IHOUR,wf_ktr).eq.0.and.imin.eq.0) then
c         read in current reservoir value
          read(21,'(100f10.3)',IOSTAT=ios)(wf_qrel(i),
     +		i=1,wf_noresv_ctrl)
          if(ios.ne.0) then
            print *, 'ran out of reservoir data before met data'
            stop
	    endif
        else
          if (jan.eq.1.and.wf_noresv_ctrl.gt.0) then 
            read(21,'(100f10.3)',IOSTAT=ios)(wf_qrel(i),
     +	  i=1,wf_noresv_ctrl)
            rewind 21
            read(21,*)
            do i=1,wf_noresv
              read(21,*)
   	      enddo
          endif 
        endif
      endif

c only read in current value if we are on the correct time step
c also read in the first value if this is the first time through
      if(mod(IHOUR,wf_kt).eq.0.and.imin.eq.0) then
c       read in current streamflow value
            read(22,'(100F10.3)',IOSTAT=ios) (wf_qhyd(i),i=1,wf_no)
              if(ios.ne.0) then
                print *, 'ran out of streamflow data before met data'
	          stop
		        endif
      else
	  if(jan.eq.1) then
          read(22,'(100F10.3)',IOSTAT=ios) (wf_qhyd(i),i=1,wf_no)
          rewind 22
          read(22,*) 
          read(22,*) 
	    do i=1,wf_no
            read(22,*)
          end do
	  endif
      endif

C END OF WATCLASS CODE
C
      DAY=REAL(IDAY)+(REAL(IHOUR)+REAL(IMIN)/60.)/24.
      DECL=SIN(2.*PI*(284.+DAY)/365.)*23.45*PI/180.
      HOUR=(REAL(IHOUR)+REAL(IMIN)/60.)*PI/12.-PI
      COSZ=SIN(RADJGRD(1))*SIN(DECL)+COS(RADJGRD(1))*COS(DECL)*COS(HOUR)

      DO 300 I=1,NLTEST
          CSZGRD(I)=SIGN(MAX(ABS(COSZ),1.0E-3),COSZ)
          IF(PREGRD(I).GT.0.) THEN
              XDIFFUS(I)=1.0
          ELSE
              XDIFFUS(I)=MAX(0.0,MIN(1.0-0.9*COSZ,1.0))
          ENDIF
          FCLOGRD(I)=XDIFFUS(I)
300   CONTINUE
C
      CALL CLASSI(VPDGRD,TADPGRD,PADRGRD,RHOAGRD,RHSIGRD,
     1            RPCPGRD,TRPCGRD,SPCPGRD,TSPCGRD,TAGRD,QAGRD,
     2            PREGRD,RPREGRD,SPREGRD,PRESGRD,
     3            IPCP,NLAT,1,NLTEST)
C
      CALL GATPREP(ILMOS,JLMOS,IWMOS,JWMOS,IWAT,IICE,
     1             NML,NMW,NWAT,NICE,GCGRD,FAREROW,MIDROW,
     2             NLAT,NMOS,ILG,1,NLTEST,NMTEST)
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
C
C=======================================================================
C     * WRITE FIELDS FROM CURRENT TIME STEP TO OUTPUT FILES.

6100  FORMAT(1X,I4,I5,9F8.2,2F8.3,4F12.4)
6200  FORMAT(1X,I4,I5,5(F8.2,2F6.3))
6300  FORMAT(1X,I4,I5,4(F8.2,2F6.3),3F7.2,9F8.3)
6201  FORMAT(1X,I4,I5,3(F8.2,2F6.3),F8.2,2F8.4,F8.2,F8.3)
6301  FORMAT(1X,I4,I5,3F9.2,F8.2,F10.2,E12.3,2F12.3,F8.3)
6400  FORMAT(1X,I2,I3,I5,I6,9F8.2,2F7.3,E11.3,F8.2,F12.4)
6500  FORMAT(1X,I2,I3,I5,I6,3(F7.2,2F6.3),F8.2,2F8.4,F8.2,F8.3)
6600  FORMAT(1X,I2,I3,I5,I6,4(F7.2,2F6.3),9F7.2)
6601  FORMAT(1X,I2,I3,I5,2F10.2,E12.3,F10.2,F8.2,F10.2,E12.3)
6700  FORMAT(2X,12E11.4)
6800  FORMAT(2X,22(F10.4,2X))
6900  FORMAT(2X,21(E12.4,2X))


      DO 450 I=1,NLTEST
      DO 425 M=1,NMTEST
          IF(FSDOWN.GT.0.0) THEN
              ALTOT=(ALVSROW(I,M)+ALIRROW(I,M))/2.0
          ELSE
              ALTOT=0.0
          ENDIF
          FSSTAR=FSDOWN*(1.0-ALTOT)
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

      i_out=0
      do k=1, wf_num_points
      if(I.eq.n_out(k).and.M.eq.ii_out(k)) then
c figure out nlmos and mlmos of grid square I
      do n=1, nml
      if(ilmos(n).eq.n_out(k).and.jlmos(n).eq.ii_out(k)) then
      i_out=n
      endif
	enddo

      if(i_out.eq.0) then
         print *,'In the WATCLASS.INI file there the following'
         print *, 'grid square', i, ' has no area in land class', m
         print *,'Please adjust WATCLASS.INI file, as a guide here'
         print *,'are the land class fractions for that square:'
         do n=1, NMTEST
           print *, 'land class ',n,' has an area of:',wf_aclass(i,n)
         enddo
	   stop
      endif

        if(.TRUE.) then ! bjd - june 29, 2004
          WRITE(150+k*10+4,6400) IHOUR,IMIN,IDAY,IYEAR,FSSTAR,FLSTAR,QH,
     1                   QE,SNOMLT,BEG,GTOUT,SNOROW(I,M),RHOSROW(I,M),
     2                   WSNOROW(I,M),ALTOT,ROFROW(I,M),
     3                   TPN,ZPNDROW(I,M)
          WRITE(150+k*10+5,6500) IHOUR,IMIN,IDAY,IYEAR,
     1                   (TBARROW(I,M,J)-TFREZ,THLQROW(I,M,J),
     2                   THICROW(I,M,J),J=1,3),TCN,RCANROW(I,M),
     3                   SCANROW(I,M),TSN,ZSN
          WRITE(150+k*10+6,6601) IHOUR,IMIN,IDAY,FSDOWN,FDLGRD(I),
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
     5                   (HMFGROW(I,M,J),J=1,3),
     6                   HTCCROW(I,M),HTCSROW(I,M),
     7                   (HTCROW(I,M,J),J=1,3)
          WRITE(150+k*10+9,6900) PCFCROW(I,M),PCLCROW(I,M),PCPNROW(I,M),
     1                   PCPGROW(I,M),QFCFROW(I,M),QFCLROW(I,M),
     2                   QFNROW(I,M),QFGROW(I,M),(QFCROW(I,M,J),J=1,3),
     3                   ROFCROW(I,M),ROFNROW(I,M),ROFOROW(I,M),
     4                   ROFROW(I,M),WTRCROW(I,M),WTRSROW(I,M),
     5                   WTRGROW(I,M)
        endif ! (.TRUE.)
      endif ! end of if(I.eq.n_out(j).and.M.eq.ii_out(j) then
      enddo ! end of do j=1, wf_num_points
425   CONTINUE
450   CONTINUE
C
C=======================================================================
C     * CALCULATE GRID CELL AVERAGE DIAGNOSTIC FIELDS.
C
      DO 525 I=1,NLTEST
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
          DO 500 J=1,IGND
              HMFGGRD(I,J)=0.
              HTCGRD(I,J)=0.
              QFCGRD(I,J)=0.
500       CONTINUE
525   CONTINUE
C
      DO 600 I=1,NLTEST
      DO 575 M=1,NMTEST
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
          DO 550 J=1,IGND
              HMFGGRD(I,J)=HMFGGRD(I,J)+HMFGROW(I,M,J)*FAREROW(I,M)        
              HTCGRD(I,J)=HTCGRD(I,J)+HTCROW(I,M,J)*FAREROW(I,M)        
              QFCGRD(I,J)=QFCGRD(I,J)+QFCROW(I,M,J)*FAREROW(I,M)        
550       CONTINUE
575   CONTINUE
600   CONTINUE
C
C=======================================================================
C     * ACCUMULATE OUTPUT DATA FOR DIURNALLY AVERAGED FIELDS.

      DO 675 I=1,NLTEST
      DO 650 M=1,NMTEST
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
          DO 625 J=1,IGND
              TBARACC(I,J)=TBARACC(I,J)+TBARROW(I,M,J)*FAREROW(I,M)
              THLQACC(I,J)=THLQACC(I,J)+THLQROW(I,M,J)*FAREROW(I,M)
              THICACC(I,J)=THICACC(I,J)+THICROW(I,M,J)*FAREROW(I,M)
              THALACC(I,J)=THALACC(I,J)+(THLQROW(I,M,J)+THICROW(I,M,J))
     1                    *FAREROW(I,M)
625       CONTINUE
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
          FSINACC(I)=FSINACC(I)+FSDOWN*FAREROW(I,M)
          FLINACC(I)=FLINACC(I)+FDLGRD(I)*FAREROW(I,M)
          FLUTACC(I)=FLUTACC(I)+SBC*GTROW(I,M)**4*FAREROW(I,M)
          TAACC(I)=TAACC(I)+TAGRD(I)*FAREROW(I,M)
          UVACC(I)=UVACC(I)+UVGRD(I)*FAREROW(I,M)
          PRESACC(I)=PRESACC(I)+PRESGRD(I)*FAREROW(I,M)
          QAACC(I)=QAACC(I)+QAGRD(I)*FAREROW(I,M)
650   CONTINUE
675   CONTINUE
C
C     * CALCULATE AND PRINT DAILY AVERAGES.
C 
      IF(NCOUNT.EQ.48) THEN

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
          DO 725 J=1,IGND
              TBARACC(I,J)=TBARACC(I,J)/REAL(NSUM)
              THLQACC(I,J)=THLQACC(I,J)/REAL(NSUM)
              THICACC(I,J)=THICACC(I,J)/REAL(NSUM)
              THALACC(I,J)=THALACC(I,J)/REAL(NSUM)
725       CONTINUE
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
              IF(TCANACC(I).GT.0.01) THEN
                  TCN=TCANACC(I)-TFREZ
              ELSE
                  TCN=0.0
              ENDIF
              IF(TSNOACC(I).GT.0.01) THEN
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
      do k=1, wf_num_points
      if(I.eq.n_out(k)) then
c figure out nlmos and mlmos of grid square I
      do n=1, nml
      if(ilmos(n).eq.n_out(k)) then
      i_out=n
      endif
	enddo
      if(.TRUE.) then ! bjd - june 29, 2004
         WRITE(150+k*10+1,6100) IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,
     1                       BEG,GTOUT,SNOACC(I),RHOSACC(I),
     2                       WSNOACC(I),ALTOT,ROFACC(I),ROFOACC(I),
     3                       ROFSACC(I),ROFBACC(I)
         WRITE(150+k*10+2,6201) IDAY,IYEAR,(TBARACC(I,J)-TFREZ,
     1                       THLQACC(I,J),THICACC(I,J),J=1,3),
     2                       TCN,RCANACC(I),SCANACC(I),TSN,ZSN
         WRITE(150+k*10+3,6301) IDAY,IYEAR,FSINACC(I),FLINACC(I),
     1                       TAACC(I)-TFREZ,UVACC(I),PRESACC(I),
     2                       QAACC(I),PREACC(I),EVAPACC(I)
      endif
      endif  ! end of   if(I.eq.n_out(k)) then
	enddo  ! end of   do j=1, wf_num_points
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
          DO 750 J=1,IGND
              TBARACC(I,J)=0.
              THLQACC(I,J)=0.
              THICACC(I,J)=0.
              THALACC(I,J)=0.
750       CONTINUE
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

      ENDIF  !      IF(NCOUNT.EQ.48) THEN

      NCOUNT=NCOUNT+1
      NSUM=NSUM+1
      IF(NCOUNT.GT.48) THEN
          NCOUNT=1
          NSUM=1
      ENDIF

      call wf_route(wf_RouteTimeStep,wf_r1,wf_r2,
     b     wf_na,wf_naa,wf_ntype,wf_imax,wf_jmax,wf_iymin,
     b     wf_iymax,wf_jxmin,wf_jxmax,wf_yy,wf_xx,wf_ibn,wf_irough,
     b     wf_ichnl,wf_next,wf_ireach,wf_al,wf_grdn,wf_grde,
     b     wf_da,wf_bnkfll,wf_channelSlope,wf_elev,wf_frac,
     f     wf_no,wf_nl,wf_mhrd,wf_kt,wf_iy,wf_jx,
     f     wf_qhyd,wf_res,wf_resstore,wf_noresv_ctrl,wf_r,
     r     wf_noresv,wf_nrel,wf_ktr,wf_ires,wf_jres,wf_resname,
     r     wf_b1,wf_b2,wf_qrel, wf_qr,
     s     wf_TimeCount,wf_nhyd,wf_qbase,wf_qi1,wf_qi2,wf_qo1,wf_qo2,
     s     wf_store1,wf_store2,
     +     DriverTimeStep,ROFGRD, NLAT, M_C,M_R,M_S, NLTEST,
     +     wf_s, jan,IDAY,IHOUR,IMIN)

           do l=1,wf_no
             wf_qsyn(l)=wf_qo2(wf_s(l))
             wf_qhyd_avg(l)=wf_qhyd(l)
           enddo

      IF(NCOUNT.EQ.48) THEN
c write out the spl.csv file
         write(70,850) IDAY,(wf_qhyd_avg(l),wf_qsyn(l),l=1,wf_no)
c print out spl.csv file to screen
         write(6,851) iyear,IDAY,(wf_qhyd_avg(l),wf_qsyn(l),l=1,wf_no)
      ENDIF

 850  format(I5, ',', F10.3,100(',',F10.3))
 851  format(2I5, F10.3,100(F10.3))

C UPDATE THE HOUR AND DAY COUNTER AS IT DOESN'T READ THEM IN FROM THE MET
C FILE LIKE IN CLASS
      IDAY=IDAY+(IHOUR+(IMIN+30)/60)/24
      IHOUR=IHOUR+(IMIN+30)/60
      IF(IHOUR.EQ.24) IHOUR=0
      IMIN=IMIN+30
      IF(IMIN.EQ.60) IMIN=0

      IF(MOD(REAL(IYEAR),4.0).EQ.0.0) THEN
      IF(IDAY.GT.366) THEN
          IDAY=1
          IYEAR=IYEAR+1
      ENDIF
      ELSE
      IF(IDAY.GT.365) THEN
          IDAY=1
          IYEAR=IYEAR+1
      ENDIF
      ENDIF

C=======================================================================
C
      GO TO 200
999   CONTINUE
C
      DO 825 J=1,6
          ISUM(J)=0
825   CONTINUE
      DO 900 I=1,50
c          WRITE(67,850) I,(ITCTGAT(1,J,I),J=1,6)
          DO 810 J=1,6
              ISUM(J)=ISUM(J)+ITCTGAT(1,J,I)
810       CONTINUE
900   CONTINUE
c850   FORMAT(2X,7I10)
c      WRITE(67,875) (ISUM(J),J=1,6)
875   FORMAT(2X,10X,6I10)

      close(unit=21)
      close(unit=22)
	close(unit=51)
      close(unit=70)

      STOP
      END
