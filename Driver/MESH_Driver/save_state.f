      SUBROUTINE SAVE_STATE(
     +   HOURLYFLAG, IMIN, IMIN2,
     +   BASINSHORTWAVEFLAG, BASINLONGWAVEFLAG, 
     +   BASINRAINFLAG, BASINTEMPERATUREFLAG, 
     +   BASINWINDFLAG, BASINPRESFLAG, BASINHUMIDITYFLAG, 
     +   FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, 
     +   I, J, XCOUNT, YCOUNT, jan, 
     +   VPDGRD, TADPGRD, PADRGRD, RHOAGRD, RHSIGRD, 
     +   RPCPGRD, TRPCGRD, SPCPGRD, TSPCGRD, TAGRD, 
     +   QAGRD, PREGRD, RPREGRD, SPREGRD, PRESGRD, 
!MAM - FOR FORCING DATA INTERPOLATION PURPOSE
     +   FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE,
     +   TAGATPRE, ULGATPRE, PRESGATPRE, QAGATPRE, 
     +   IPCP, NA, NLTEST, ILMOS, JLMOS, IWMOS, JWMOS, 
     +   IWAT, IICE, NML, NMW, NWAT, NICE, 
     +   GCGRD, FAREROW, MIDROW, NTYPE, ILG, NMTEST, 
     +   TBARGAT, THLQGAT, THICGAT, TPNDGAT, ZPNDGAT, 
     +   TBASGAT, ALBSGAT, TSNOGAT, RHOSGAT, SNOGAT, 
     +   TCANGAT, RCANGAT, SCANGAT, GROGAT, CMAIGAT, 
     +   FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT, LAMXGAT, 
     +   LAMNGAT, CMASGAT, ROOTGAT, RSMNGAT, QA50GAT, 
     +   VPDAGAT, VPDBGAT, PSGAGAT, PSGBGAT, AILDGAT, 
     +   HGTDGAT, ACVDGAT, ACIDGAT, TSFSGAT, WSNOGAT, 
     +   THPGAT, THRGAT, THMGAT, BIGAT, PSISGAT, 
     +   GRKSGAT, THRAGAT, HCPSGAT, TCSGAT, THFCGAT, 
     +   PSIWGAT, DLZWGAT, ZBTWGAT, ZSNLGAT, ZPLGGAT, 
     +   ZPLSGAT, TACGAT, QACGAT, DRNGAT, XSLPGAT, 
     +   XDGAT, WFSFGAT, KSGAT, ALGWGAT, ALGDGAT, 
     +   ASVDGAT, ASIDGAT, AGVDGAT, AGIDGAT, ISNDGAT, 
     +   RADJGAT, ZBLDGAT, Z0ORGAT, ZRFMGAT, ZRFHGAT, 
     +   ZDMGAT, ZDHGAT, FSVHGAT, FSIHGAT, CSZGAT, 
     +   FDLGAT, ULGAT, VLGAT, TAGAT, QAGAT, PRESGAT, 
     +   PREGAT, PADRGAT, VPDGAT, TADPGAT, RHOAGAT, 
     +   RPCPGAT, TRPCGAT, SPCPGAT, TSPCGAT, RHSIGAT, 
     +   FCLOGAT, DLONGAT, GGEOGAT, CDHGAT, CDMGAT, 
     +   HFSGAT, TFXGAT, QEVPGAT, QFSGAT, QFXGAT, 
     +   PETGAT, GAGAT, EFGAT, GTGAT, QGGAT, TSFGAT, 
     +   ALVSGAT, ALIRGAT, SFCTGAT, SFCUGAT, SFCVGAT, 
     +   SFCQGAT, FSNOGAT, FSGVGAT, FSGSGAT, FSGGGAT, 
     +   FLGVGAT, FLGSGAT, FLGGGAT, HFSCGAT, HFSSGAT, 
     +   HFSGGAT, HEVCGAT, HEVSGAT, HEVGGAT, HMFCGAT, 
     +   HMFNGAT, HTCCGAT, HTCSGAT, PCFCGAT, PCLCGAT, 
     +   PCPNGAT, PCPGGAT, QFGGAT, QFNGAT, QFCLGAT, 
     +   QFCFGAT, ROFGAT, ROFOGAT, ROFSGAT, ROFBGAT, 
     +   TROFGAT, TROOGAT, TROSGAT, TROBGAT, ROFCGAT, 
     +   ROFNGAT, ROVGGAT, WTRCGAT, WTRSGAT, WTRGGAT, 
     +   DRGAT, HMFGGAT, HTCGAT, QFCGAT, ITCTGAT, 
     +   IGND, ICAN, ICP1, 
     +   TBARROW, THLQROW, THICROW, TPNDROW, ZPNDROW, 
     +   TBASROW, ALBSROW, TSNOROW, RHOSROW, SNOROW, 
     +   TCANROW, RCANROW, SCANROW, GROROW, CMAIROW, 
     +   FCANROW, LNZ0ROW, ALVCROW, ALICROW, LAMXROW, 
     +   LAMNROW, CMASROW, ROOTROW, RSMNROW, QA50ROW, 
     +   VPDAROW, VPDBROW, PSGAROW, PSGBROW, AILDROW, 
     +   HGTDROW, ACVDROW, ACIDROW, TSFSROW, WSNOROW, 
     +   THPROW, THRROW, THMROW, BIROW, PSISROW, 
     +   GRKSROW, THRAROW, HCPSROW, TCSROW, THFCROW, 
     +   PSIWROW, DLZWROW, ZBTWROW, ZSNLROW, ZPLGROW, 
     +   ZPLSROW, TACROW, QACROW, DRNROW, XSLPROW, 
     +   XDROW, WFSFROW, KSROW, ALGWROW, ALGDROW, 
     +   ASVDROW, ASIDROW, AGVDROW, AGIDROW, 
     +   ISNDROW, RADJGRD, ZBLDGRD, Z0ORGRD, 
     +   ZRFMGRD, ZRFHGRD, ZDMGRD, ZDHGRD, CSZGRD,
     +   ULGRD, VLGRD, FCLOGRD, DLONGRD, GGEOGRD, 
     +   MANNROW, MANNGAT, DDROW, DDGAT, 
     +   CTVSTP, CTSSTP, CT1STP, CT2STP, CT3STP, 
     +   WTVSTP, WTSSTP, WTGSTP, 
     +   DELZ, FCS, FGS, FC, FG, N, 
     +   ALVSCN, ALIRCN, ALVSG, ALIRG, ALVSCS, 
     +   ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, 
     +   ALVSSC, ALIRSC, TRVSCN, TRIRCN, TRVSCS, 
     +   TRIRCS, FSVF, FSVFS, 
     +   RAICAN, RAICNS, SNOCAN, SNOCNS,
     +   FRAINC, FSNOWC, FRAICS, FSNOCS, 
     +   DISP, DISPS, ZOMLNC, ZOMLCS, ZOELNC, ZOELCS, 
     +   ZOMLNG, ZOMLNS, ZOELNG, ZOELNS, 
     +   CHCAP, CHCAPS, CMASSC, CMASCS, CWLCAP, 
     +   CWFCAP, CWLCPS, CWFCPS, RC, RCS, RBCOEF, 
     +   FROOT, ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, 
     +   TRSNOW, ZSNOW, IDAY, JLAT, IDISP, 
     +   IZREF, IWF, ILAI, IHGT, IALC, IALS, IALG, 
     +   TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG,
     +   THICEC, THICEG, HCPC, HCPG, TCTOPC, TCBOTC, 
     +   TCTOPG, TCBOTG,
     +   GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G, 
     +   G12CS, G12GS, G23C, G23G, G23CS, G23GS, 
     +   QFREZC, QFREZG, QMELTC, QMELTG, 
     +   EVAPC, EVAPCG,EVAPG, EVAPCS, EVPCSG, EVAPGS, 
     +   TCANO, TCANS, TPONDC, TPONDG, TPNDCS, TPNDGS, 
     +   TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS, 
     +   WTABGAT, 
     +   ILMOGAT, UEGAT, HBLGAT,
     +   ILW, ITC, ITCG, ITG, ISLFD, 
     +   NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI, 
     +   GFLXGAT, CDHROW, CDMROW, HFSROW, TFXROW, 
     +   QEVPROW, QFSROW, QFXROW, PETROW, GAROW, 
     +   EFROW, GTROW, QGROW, TSFROW, ALVSROW, 
     +   ALIRROW, SFCTROW, SFCUROW, SFCVROW, SFCQROW, 
     +   FSGVROW, FSGSROW, FSGGROW, FLGVROW, FLGSROW, 
     +   FLGGROW, HFSCROW, HFSSROW, HFSGROW, HEVCROW, 
     +   HEVSROW, HEVGROW, HMFCROW, HMFNROW, HTCCROW, 
     +   HTCSROW, PCFCROW, PCLCROW, PCPNROW, PCPGROW, 
     +   QFGROW, QFNROW, QFCLROW, QFCFROW, ROFROW, 
     +   ROFOROW, ROFSROW, ROFBROW, TROFROW, TROOROW, 
     +   TROSROW, TROBROW, ROFCROW, ROFNROW, ROVGROW, 
     +   WTRCROW, WTRSROW, WTRGROW, DRROW, WTABROW, 
     +   ILMOROW, UEROW, HBLROW, HMFGROW, HTCROW,
     +   QFCROW, FSNOROW, ITCTROW, NCOUNT, ireport, 
     +   wfo_seq, IYEAR, ensim_MONTH, ensim_DAY, 
     +   IHOUR, XXX, YYY, NLAT,
     +   NMOS, DELT, TFREZ, UVGRD, SBC, RHOW, CURREC,
     +   M_C, M_S, M_R,
     A     WF_ROUTETIMESTEP,WF_R1,WF_R2,NAA,IYMIN,
     B     WF_IYMAX,JXMIN,WF_JXMAX,WF_IBN,WF_IROUGH,
     C     WF_ICHNL,WF_NEXT,WF_IREACH,AL,GRDN,GRDE,
     D     WF_DA,WF_BNKFLL,WF_CHANNELSLOPE,WF_ELEV,FRAC,
     E     WF_NO,WF_NL,WF_MHRD,WF_KT,WF_IY,WF_JX,
     F     WF_QHYD,WF_RES,WF_RESSTORE,WF_NORESV_CTRL,WF_R,
     G     WF_NORESV,WF_NREL,WF_KTR,WF_IRES,WF_JRES,WF_RESNAME,
     H     WF_B1,WF_B2,WF_QREL, WF_QR,
     I     WF_TIMECOUNT,WF_NHYD,WF_QBASE,WF_QI1,WF_QI2,WF_QO1,WF_QO2,
     J     WF_STORE1,WF_STORE2,
     K     DRIVERTIMESTEP,ROFGRD,
     L     WF_S, 
     1  TOTAL_ROFACC, TOTAL_ROFOACC, TOTAL_ROFSACC,
     2  TOTAL_ROFBACC, TOTAL_EVAPACC, TOTAL_PREACC, INIT_STORE,
     3  FINAL_STORE, TOTAL_AREA)
!> This subroutine is used to write a resume file.
!> The resume file contains all of the variables used
!> in the loop for 
!> 
!> 
!> 
!> 
!>    
     
     
      IMPLICIT NONE
      
      INTEGER :: HOURLYFLAG, IMIN, IMIN2,
     +  BASINSHORTWAVEFLAG, BASINLONGWAVEFLAG, 
     +  BASINRAINFLAG, BASINTEMPERATUREFLAG, 
     +  BASINWINDFLAG, BASINPRESFLAG, BASINHUMIDITYFLAG,
     +  I, J,XCOUNT, YCOUNT, jan, IPCP, NA,
     +  NLTEST, NML, NMW, NWAT, NICE,
     +  NTYPE, ILG, NMTEST, IGND, ICAN, ICP1,N,
     +  IDAY, JLAT, IDISP, IZREF, IWF,ILAI, IHGT, IALC, 
     +  IALS, IALG, ILW, ITC, ITCG, ITG, ISLFD, 
     +  NLANDCS, NLANDGS, NLANDC, NLANDG, NLANDI,
     +  NCOUNT, ireport, IYEAR, IHOUR,
     +  NLAT, NMOS
     
      integer*4 wfo_seq, ensim_MONTH, ensim_DAY, CURREC
      
      REAL DELT, TFREZ, SBC, RHOW
      
      integer*4,DIMENSION(NA):: XXX, YYY
      
      INTEGER IWAT(NA), IICE(NA)
!array of size (NA, NTYPE, IGND)
      REAL, DIMENSION(NA, NTYPE, IGND)::
     +  TBARROW, THLQROW, THICROW, THPROW,
     +  THRROW, THMROW, BIROW,
     +  PSISROW, GRKSROW, THRAROW,
     +  HCPSROW, TCSROW, THFCROW,
     +  PSIWROW, DLZWROW, ZBTWROW,
     +  DLONGRD, HMFGROW, HTCROW, QFCROW
      
      INTEGER, DIMENSION(NA, NTYPE, IGND)::
     +  ISNDROW
      
!arrays of size(NA, NTYPE, ICP1)
      REAL, DIMENSION(NA, NTYPE, ICP1)::
     +  FCANROW, LNZ0ROW, ALVCROW, ALICROW
      
      
!arrays of size (NA, NTYPE, ICAN)
      REAL, DIMENSION(NA, NTYPE, ICAN)::
     +   LAMXROW, LAMNROW, CMASROW, ROOTROW,
     +   RSMNROW, QA50ROW, VPDAROW, VPDBROW, 
     +   PSGAROW, PSGBROW, AILDROW, HGTDROW, 
     +   ACVDROW, ACIDROW
      
      
      

!arrays of size (NA, NTYPE)
      REAL, DIMENSION(NA, NTYPE)::
     +  FAREROW, TPNDROW, ZPNDROW,
     +  TBASROW, ALBSROW, TSNOROW,
     +  RHOSROW, SNOROW, TCANROW,
     +  RCANROW, SCANROW, GROROW,
     +  CMAIROW, WSNOROW, ZSNLROW,
     +  ZPLGROW, ZPLSROW, TACROW,
     +  QACROW, DRNROW, XSLPROW,
     +  XDROW, WFSFROW, KSROW,
     +  ALGWROW, ALGDROW, ASVDROW,
     +  ASIDROW, AGVDROW, AGIDROW,
     +  MANNROW, DDROW, CDHROW, CDMROW,
     +   HFSROW, TFXROW, QEVPROW, QFSROW, 
     + QFXROW, PETROW, GAROW, 
     + EFROW, GTROW, QGROW, TSFROW, ALVSROW, 
     + ALIRROW, SFCTROW, SFCUROW, 
     + SFCVROW, SFCQROW, FSGVROW, 
     + FSGSROW, FSGGROW, FLGVROW, 
     + FLGSROW, DRROW, 
     + FLGGROW, HFSCROW, HFSSROW, 
     + HFSGROW,HEVCROW, HEVSROW, 
     + HEVGROW, 
     + HMFCROW, HMFNROW, HTCCROW, 
     + HTCSROW, PCFCROW, PCLCROW, 
     + PCPNROW, TROSROW,TROBROW,
     + PCPGROW, QFGROW, QFNROW, 
     + QFCLROW, QFCFROW, ROFROW, 
     + ROFOROW, TROFROW, TROOROW,
     + ROFSROW, ROFBROW, ROFCROW, 
     + ROFNROW, ROVGROW, WTRCROW, 
     + WTRSROW, WTRGROW, WTABROW, 
     + ILMOROW, UEROW, HBLROW, FSNOROW
      
      INTEGER, DIMENSION(NA, NTYPE) ::
     + MIDROW 
      
!arrays of size (ILG,IGND)
      REAL, DIMENSION(ILG,IGND)::
     +  TBARGAT, THLQGAT, THICGAT, THPGAT, THRGAT,
     +  THMGAT, BIGAT, PSISGAT, GRKSGAT, THRAGAT, 
     +  HCPSGAT, TCSGAT, THFCGAT, PSIWGAT, DLZWGAT, 
     +  ZBTWGAT, HMFGGAT, HTCGAT, QFCGAT, FROOT,
     +  TBARC, TBARG, TBARCS, TBARGS, THLIQC, THLIQG,
     +  THICEC, THICEG, HCPC, HCPG, TCTOPC, TCBOTC,
     +  TCTOPG, TCBOTG, GFLXGAT
      
      INTEGER, DIMENSION(ILG,IGND)::
     +  ISNDGAT
      
! arrays of size (ILG,ICP1)
      REAL, DIMENSION(ILG,ICP1)::
     +  FCANGAT, LNZ0GAT, ALVCGAT, ALICGAT
      
!arrays of size (ILG, ICAN)
      REAL, DIMENSION(ILG, ICAN)::
     +  LAMXGAT, LAMNGAT, CMASGAT, ROOTGAT,
     +  RSMNGAT, QA50GAT, VPDAGAT, VPDBGAT, 
     +  PSGAGAT, PSGBGAT, AILDGAT, HGTDGAT,
     +  ACVDGAT, ACIDGAT
      
!arrays of size (ILG, 4)
      REAL, DIMENSION(ILG, 4):: TSFSGAT
      
!arrays of size(ILG, 6, 50)
      INTEGER, DIMENSION(ILG, 6, 50)::
     +  ITCTGAT
      
      INTEGER, DIMENSION(NA, NTYPE, 6, 50)::
     +  ITCTROW
      
!arrays of size NA, NTYPE, 4)
      REAL, DIMENSION(NA, NTYPE, 4):: TSFSROW

!CRAIGS STUFF
! arrays of size ILG
      INTEGER, DIMENSION(ILG) :: ILMOS, JLMOS, IWMOS, JWMOS

      REAL, DIMENSION(ILG) :: TPNDGAT, ZPNDGAT, TBASGAT,
     +      ALBSGAT, TSNOGAT, RHOSGAT,
     +      SNOGAT, TCANGAT, RCANGAT,
     +      SCANGAT, GROGAT, CMAIGAT,
     +      WSNOGAT
      
      REAL, DIMENSION(ILG) :: ZSNLGAT, ZPLGGAT,
     +      ZPLSGAT

      REAL, DIMENSION(ILG) :: TACGAT, QACGAT, DRNGAT,
     +  XSLPGAT, XDGAT, WFSFGAT, KSGAT, ALGWGAT,
     +  ALGDGAT, ASVDGAT, ASIDGAT,
     +  AGVDGAT, AGIDGAT
      
      REAL, DIMENSION(ILG) :: RADJGAT,
     +  ZBLDGAT, Z0ORGAT, ZRFMGAT,
     +  ZRFHGAT, ZDMGAT, ZDHGAT,
     +  FSVHGAT, FSIHGAT, CSZGAT,
     +  FDLGAT, ULGAT, VLGAT,
     +  TAGAT, QAGAT, PRESGAT,
     +  PREGAT, PADRGAT, VPDGAT,
     +  TADPGAT, RHOAGAT, RPCPGAT,
     +  TRPCGAT, SPCPGAT, TSPCGAT,
     +  RHSIGAT, FCLOGAT, DLONGAT,
     +  GGEOGAT
      
      REAL, DIMENSION(ILG):: CDHGAT, CDMGAT,
     +  HFSGAT, TFXGAT, QEVPGAT,
     +  QFSGAT, QFXGAT, PETGAT,
     +  GAGAT, EFGAT, GTGAT,
     +  QGGAT, TSFGAT, ALVSGAT,
     +  ALIRGAT, SFCTGAT, SFCUGAT,
     +  SFCVGAT, SFCQGAT, 
     +  FSGVGAT, FSGSGAT, FSGGGAT,
     +  FLGVGAT, FLGSGAT, FLGGGAT,
     +  HFSCGAT, HFSSGAT, HFSGGAT,
     +  HEVCGAT, HEVSGAT, HEVGGAT,
     +  HMFCGAT, HMFNGAT, HTCCGAT,
     +  HTCSGAT, PCFCGAT, PCLCGAT,
     +  PCPNGAT, PCPGGAT, QFGGAT,
     +  QFNGAT, QFCLGAT, QFCFGAT,
     +  ROFGAT, ROFOGAT, ROFSGAT,
     +  ROFBGAT, TROFGAT, TROOGAT,
     +  TROSGAT, TROBGAT, ROFCGAT,
     +  ROFNGAT, ROVGGAT, WTRCGAT,
     +  WTRSGAT, WTRGGAT, DRGAT
      
      REAL, DIMENSION(ILG) :: MANNGAT, DDGAT

      REAL, DIMENSION(ILG) :: CTVSTP, CTSSTP, CT1STP, CT2STP,
     +  CT3STP, WTVSTP, WTSSTP,
     +  WTGSTP
      
      REAL, DIMENSION(ILG) :: FCS, FGS, 
     +  FC, FG, ALVSCN, ALIRCN,
     +  ALVSG, ALIRG, ALVSCS,
     +  ALIRCS, ALVSSN, ALIRSN,
     +  ALVSGC, ALIRGC, ALVSSC,
     +  ALIRSC, TRVSCN, TRIRCN,
     +  TRVSCS, TRIRCS, FSVF, FSVFS,
     +  RAICAN, RAICNS, SNOCAN, SNOCNS

      REAL, DIMENSION(ILG) :: 
     +  FRAINC, FSNOWC, FRAICS, FSNOCS,
     +  DISP, DISPS, ZOMLNC, ZOMLCS, ZOELNC, ZOELCS,
     +  ZOMLNG, ZOMLNS, ZOELNG, ZOELNS, 
     +  CHCAP, CHCAPS, CMASSC, CMASCS, CWLCAP, 
     +  CWFCAP, CWLCPS, CWFCPS
      
      REAL, DIMENSION(ILG) ::  RC, RCS,RBCOEF, ZPLIMC,
     +  ZPLIMG, ZPLMCS, ZPLMGS,
     +  TRSNOW,ZSNOW,
     +  GZEROC, GZEROG, GZROCS, GZROGS, G12C, G12G,
     +  G12CS, G12GS, G23C, G23G, G23CS, G23GS, 
     +  QFREZC, QFREZG, QMELTC, QMELTG, 
     +  EVAPC, EVAPCG,EVAPG, EVAPCS, EVPCSG, EVAPGS, 
     +  TCANO, TCANS,
     +  TPONDC, TPONDG, TPNDCS, TPNDGS,
     +  TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS
      
      REAL, DIMENSION(ILG) ::WTABGAT, 
     +   ILMOGAT, UEGAT, HBLGAT,FSNOGAT
     

      REAL, DIMENSION(IGND) :: DELZ

      REAL, DIMENSION(NA) :: FSVHGRD,
     +  FSIHGRD, FDLGRD, VPDGRD, TADPGRD,
     +  PADRGRD, RHOAGRD, RHSIGRD, RPCPGRD,
     +  TRPCGRD, SPCPGRD, TSPCGRD, TAGRD,
     +  QAGRD, PREGRD, RPREGRD, SPREGRD,
     +  PRESGRD
!MAM - FOR FORCING DATA INTERPOLATION PURPOSE
      REAL, DIMENSION(ILG) :: 
     +   FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE,
     +   TAGATPRE, ULGATPRE, PRESGATPRE, QAGATPRE 

      REAL, DIMENSION(NA) :: GCGRD, UVGRD,
     +  RADJGRD, ZBLDGRD, Z0ORGRD,
     +  ZRFMGRD, ZRFHGRD, ZDMGRD,
     +  ZDHGRD, CSZGRD, ULGRD,
     +  VLGRD, FCLOGRD, GGEOGRD,
     +  FSDOWN
      

      INTEGER WF_ROUTETIMESTEP, WF_TIMECOUNT, DRIVERTIMESTEP, M_C, NAA,
     +  IYMIN,WF_IYMAX,JXMIN,WF_JXMAX, M_R, M_S,
     +  WF_IBN(NLAT),WF_IROUGH(NLAT),
     +  WF_ICHNL(NLAT),WF_NEXT(NLAT),WF_IREACH(NLAT),
     +  WF_NO, WF_NL, WF_MHRD, WF_KT, WF_IY(M_S),
     +  WF_JX(M_S), WF_S(M_S),WF_ELEV(NLAT)
           
      REAL WF_R1(M_C), WF_R2(M_C)
      REAL*8 :: AL
      REAL*4 :: GRDN, GRDE
      REAL*8, DIMENSION(NLAT) :: WF_DA,WF_BNKFLL,
     +  WF_CHANNELSLOPE
      REAL*4 :: FRAC(NLAT)
      REAL WF_QHYD(M_S)
      INTEGER WF_RES(M_R), WF_R(M_R)
      REAL ::  WF_RESSTORE(M_R)
      INTEGER :: WF_NORESV, WF_NREL, WF_KTR, WF_NORESV_CTRL
      INTEGER, DIMENSION(M_R) :: WF_IRES, WF_JRES
      CHARACTER, DIMENSION(M_R) :: WF_RESNAME*8
      REAL, DIMENSION(M_R) :: WF_B1, WF_B2, WF_QREL
      REAL, DIMENSION(NLAT) :: WF_QR, WF_NHYD, WF_QBASE, WF_QI1, WF_QI2,
     1  WF_QO1, WF_QO2, WF_STORE1, WF_STORE2, ROFGRD
      
      
      REAL :: TOTAL_ROFACC, TOTAL_ROFOACC, TOTAL_ROFSACC,
     1  TOTAL_ROFBACC, TOTAL_EVAPACC, TOTAL_PREACC, INIT_STORE,
     2  FINAL_STORE, TOTAL_AREA

      integer resumeIostat
!open output resume file
      OPEN (10, FILE="./class_resume.txt", STATUS="REPLACE", 
     +      IOSTAT=resumeIostat)
! write the non-array variables
      WRITE(10,*) HOURLYFLAG
      WRITE(10,*) BASINSHORTWAVEFLAG
      WRITE(10,*) BASINLONGWAVEFLAG
      WRITE(10,*) BASINRAINFLAG
      WRITE(10,*) BASINTEMPERATUREFLAG
      WRITE(10,*) BASINWINDFLAG
      WRITE(10,*) BASINPRESFLAG
      WRITE(10,*) BASINHUMIDITYFLAG
      WRITE(10,*) FSDOWN
      WRITE(10,*) FSVHGRD
      WRITE(10,*) FSIHGRD
      WRITE(10,*) FDLGRD
      WRITE(10,*) I
      WRITE(10,*) J
      WRITE(10,*) XCOUNT
      WRITE(10,*) YCOUNT
      WRITE(10,*) jan
      WRITE(10,*) VPDGRD
      WRITE(10,*) TADPGRD
      WRITE(10,*) PADRGRD
      WRITE(10,*) RHOAGRD
      WRITE(10,*) RHSIGRD
      WRITE(10,*) RPCPGRD
      WRITE(10,*) TRPCGRD
      WRITE(10,*) SPCPGRD
      WRITE(10,*) TSPCGRD
      WRITE(10,*) TAGRD
      WRITE(10,*) QAGRD
      WRITE(10,*) PREGRD
      WRITE(10,*) RPREGRD
      WRITE(10,*) SPREGRD
      WRITE(10,*) PRESGRD
!MAM - FOR FORCING DATA INTERPOLATION PURPOSE
      WRITE(10,*) FSVHGATPRE
      WRITE(10,*) FSIHGATPRE
      WRITE(10,*) FDLGATPRE
      WRITE(10,*) PREGATPRE
      WRITE(10,*) TAGATPRE
      WRITE(10,*) ULGATPRE
      WRITE(10,*) PRESGATPRE
      WRITE(10,*) QAGATPRE 
      WRITE(10,*) IPCP
      WRITE(10,*) NA
      WRITE(10,*) NLTEST
      WRITE(10,*) ILMOS
      WRITE(10,*) JLMOS
      WRITE(10,*) IWMOS
      WRITE(10,*) JWMOS
      WRITE(10,*) IWAT
      WRITE(10,*) IICE
      WRITE(10,*) NML
      WRITE(10,*) NMW
      WRITE(10,*) NWAT
      WRITE(10,*) NICE
      WRITE(10,*) GCGRD
      WRITE(10,*) FAREROW
      WRITE(10,*) MIDROW
      WRITE(10,*) NTYPE
      WRITE(10,*) ILG
      WRITE(10,*) NMTEST
      WRITE(10,*) TBARGAT
      WRITE(10,*) THLQGAT
      WRITE(10,*) THICGAT
      WRITE(10,*) TPNDGAT
      WRITE(10,*) ZPNDGAT
      WRITE(10,*) TBASGAT
      WRITE(10,*) ALBSGAT
      WRITE(10,*) TSNOGAT
      WRITE(10,*) RHOSGAT
      WRITE(10,*) SNOGAT
      WRITE(10,*) TCANGAT
      WRITE(10,*) RCANGAT
      WRITE(10,*) SCANGAT
      WRITE(10,*) GROGAT
      WRITE(10,*) CMAIGAT
      WRITE(10,*) FCANGAT
      WRITE(10,*) LNZ0GAT
      WRITE(10,*) ALVCGAT
      WRITE(10,*) ALICGAT
      WRITE(10,*) LAMXGAT
      WRITE(10,*) LAMNGAT
      WRITE(10,*) CMASGAT
      WRITE(10,*) ROOTGAT
      WRITE(10,*) RSMNGAT
      WRITE(10,*) QA50GAT
      WRITE(10,*) VPDAGAT
      WRITE(10,*) VPDBGAT
      WRITE(10,*) PSGAGAT
      WRITE(10,*) PSGBGAT
      WRITE(10,*) AILDGAT
      WRITE(10,*) HGTDGAT
      WRITE(10,*) ACVDGAT
      WRITE(10,*) ACIDGAT
      WRITE(10,*) TSFSGAT
      WRITE(10,*) WSNOGAT
      WRITE(10,*) THPGAT
      WRITE(10,*) THRGAT
      WRITE(10,*) THMGAT
      WRITE(10,*) BIGAT
      WRITE(10,*) PSISGAT
      WRITE(10,*) GRKSGAT
      WRITE(10,*) THRAGAT
      WRITE(10,*) HCPSGAT
      WRITE(10,*) TCSGAT
      WRITE(10,*) THFCGAT
      WRITE(10,*) PSIWGAT
      WRITE(10,*) DLZWGAT
      WRITE(10,*) ZBTWGAT
      WRITE(10,*) ZSNLGAT
      WRITE(10,*) ZPLGGAT
      WRITE(10,*) ZPLSGAT
      WRITE(10,*) TACGAT
      WRITE(10,*) QACGAT
      WRITE(10,*) DRNGAT
      WRITE(10,*) XSLPGAT
      WRITE(10,*) XDGAT
      WRITE(10,*) WFSFGAT
      WRITE(10,*) KSGAT
      WRITE(10,*) ALGWGAT
      WRITE(10,*) ALGDGAT
      WRITE(10,*) ASVDGAT
      WRITE(10,*) ASIDGAT
      WRITE(10,*) AGVDGAT
      WRITE(10,*) AGIDGAT
      WRITE(10,*) ISNDGAT
      WRITE(10,*) RADJGAT
      WRITE(10,*) ZBLDGAT
      WRITE(10,*) Z0ORGAT
      WRITE(10,*) ZRFMGAT
      WRITE(10,*) ZRFHGAT
      WRITE(10,*) ZDMGAT
      WRITE(10,*) ZDHGAT
      WRITE(10,*) FSVHGAT
      WRITE(10,*) FSIHGAT
      WRITE(10,*) CSZGAT
      WRITE(10,*) FDLGAT
      WRITE(10,*) ULGAT
      WRITE(10,*) VLGAT
      WRITE(10,*) TAGAT
      WRITE(10,*) QAGAT
      WRITE(10,*) PRESGAT
      WRITE(10,*) PREGAT
      WRITE(10,*) PADRGAT
      WRITE(10,*) VPDGAT
      WRITE(10,*) TADPGAT
      WRITE(10,*) RHOAGAT
      WRITE(10,*) RPCPGAT
      WRITE(10,*) TRPCGAT
      WRITE(10,*) SPCPGAT
      WRITE(10,*) TSPCGAT
      WRITE(10,*) RHSIGAT
      WRITE(10,*) FCLOGAT
      WRITE(10,*) DLONGAT
      WRITE(10,*) GGEOGAT
      WRITE(10,*) CDHGAT
      WRITE(10,*) CDMGAT
      WRITE(10,*) HFSGAT
      WRITE(10,*) TFXGAT
      WRITE(10,*) QEVPGAT
      WRITE(10,*) QFSGAT
      WRITE(10,*) QFXGAT
      WRITE(10,*) PETGAT
      WRITE(10,*) GAGAT
      WRITE(10,*) EFGAT
      WRITE(10,*) GTGAT
      WRITE(10,*) QGGAT
      WRITE(10,*) TSFGAT
      WRITE(10,*) ALVSGAT
      WRITE(10,*) ALIRGAT
      WRITE(10,*) SFCTGAT
      WRITE(10,*) SFCUGAT
      WRITE(10,*) SFCVGAT
      WRITE(10,*) SFCQGAT
      WRITE(10,*) FSNOGAT
      WRITE(10,*) FSGVGAT
      WRITE(10,*) FSGSGAT
      WRITE(10,*) FSGGGAT
      WRITE(10,*) FLGVGAT
      WRITE(10,*) FLGSGAT
      WRITE(10,*) FLGGGAT
      WRITE(10,*) HFSCGAT
      WRITE(10,*) HFSSGAT
      WRITE(10,*) HFSGGAT
      WRITE(10,*) HEVCGAT
      WRITE(10,*) HEVSGAT
      WRITE(10,*) HEVGGAT
      WRITE(10,*) HMFCGAT
      WRITE(10,*) HMFNGAT
      WRITE(10,*) HTCCGAT
      WRITE(10,*) HTCSGAT
      WRITE(10,*) PCFCGAT
      WRITE(10,*) PCLCGAT
      WRITE(10,*) PCPNGAT
      WRITE(10,*) PCPGGAT
      WRITE(10,*) QFGGAT
      WRITE(10,*) QFNGAT
      WRITE(10,*) QFCLGAT
      WRITE(10,*) QFCFGAT
      WRITE(10,*) ROFGAT
      WRITE(10,*) ROFOGAT
      WRITE(10,*) ROFSGAT
      WRITE(10,*) ROFBGAT
      WRITE(10,*) TROFGAT
      WRITE(10,*) TROOGAT
      WRITE(10,*) TROSGAT
      WRITE(10,*) TROBGAT
      WRITE(10,*) ROFCGAT
      WRITE(10,*) ROFNGAT
      WRITE(10,*) ROVGGAT
      WRITE(10,*) WTRCGAT
      WRITE(10,*) WTRSGAT
      WRITE(10,*) WTRGGAT
      WRITE(10,*) DRGAT
      WRITE(10,*) HMFGGAT
      WRITE(10,*) HTCGAT
      WRITE(10,*) QFCGAT
      WRITE(10,*) ITCTGAT
      WRITE(10,*) TBARROW
      WRITE(10,*) THLQROW
      WRITE(10,*) THICROW
      WRITE(10,*) TPNDROW
      WRITE(10,*) ZPNDROW
      WRITE(10,*) TBASROW
      WRITE(10,*) ALBSROW
      WRITE(10,*) TSNOROW
      WRITE(10,*) RHOSROW
      WRITE(10,*) SNOROW
      WRITE(10,*) TCANROW
      WRITE(10,*) RCANROW
      WRITE(10,*) SCANROW
      WRITE(10,*) GROROW
      WRITE(10,*) CMAIROW
      WRITE(10,*) FCANROW
      WRITE(10,*) LNZ0ROW
      WRITE(10,*) ALVCROW
      WRITE(10,*) ALICROW
      WRITE(10,*) LAMXROW
      WRITE(10,*) LAMNROW
      WRITE(10,*) CMASROW
      WRITE(10,*) ROOTROW
      WRITE(10,*) RSMNROW
      WRITE(10,*) QA50ROW
      WRITE(10,*) VPDAROW
      WRITE(10,*) VPDBROW
      WRITE(10,*) PSGAROW
      WRITE(10,*) PSGBROW
      WRITE(10,*) AILDROW
      WRITE(10,*) HGTDROW
      WRITE(10,*) ACVDROW
      WRITE(10,*) ACIDROW
      WRITE(10,*) TSFSROW
      WRITE(10,*) WSNOROW
      WRITE(10,*) THPROW
      WRITE(10,*) THRROW
      WRITE(10,*) THMROW
      WRITE(10,*) BIROW
      WRITE(10,*) PSISROW
      WRITE(10,*) GRKSROW
      WRITE(10,*) THRAROW
      WRITE(10,*) HCPSROW
      WRITE(10,*) TCSROW
      WRITE(10,*) THFCROW
      WRITE(10,*) PSIWROW
      WRITE(10,*) DLZWROW
      WRITE(10,*) ZBTWROW
      WRITE(10,*) ZSNLROW
      WRITE(10,*) ZPLGROW
      WRITE(10,*) ZPLSROW
      WRITE(10,*) TACROW
      WRITE(10,*) QACROW
      WRITE(10,*) DRNROW
      WRITE(10,*) XSLPROW
      WRITE(10,*) XDROW
      WRITE(10,*) WFSFROW
      WRITE(10,*) KSROW
      WRITE(10,*) ALGWROW
      WRITE(10,*) ALGDROW
      WRITE(10,*) ASVDROW
      WRITE(10,*) ASIDROW
      WRITE(10,*) AGVDROW
      WRITE(10,*) AGIDROW
      WRITE(10,*) ISNDROW
      WRITE(10,*) RADJGRD
      WRITE(10,*) ZBLDGRD
      WRITE(10,*) Z0ORGRD
      WRITE(10,*) ZRFMGRD
      WRITE(10,*) ZRFHGRD
      WRITE(10,*) ZDMGRD
      WRITE(10,*) ZDHGRD
      WRITE(10,*) CSZGRD
      WRITE(10,*) ULGRD
      WRITE(10,*) VLGRD
      WRITE(10,*) FCLOGRD
      WRITE(10,*) DLONGRD
      WRITE(10,*) GGEOGRD
      WRITE(10,*) MANNROW
      WRITE(10,*) MANNGAT
      WRITE(10,*) DDROW
      WRITE(10,*) DDGAT
      WRITE(10,*) CTVSTP
      WRITE(10,*) CTSSTP
      WRITE(10,*) CT1STP
      WRITE(10,*) CT2STP
      WRITE(10,*) CT3STP
      WRITE(10,*) WTVSTP
      WRITE(10,*) WTSSTP
      WRITE(10,*) WTGSTP
      WRITE(10,*) DELZ
      WRITE(10,*) FCS
      WRITE(10,*) FGS
      WRITE(10,*) FC
      WRITE(10,*) FG
      WRITE(10,*) N
      WRITE(10,*) ALVSCN
      WRITE(10,*) ALIRCN
      WRITE(10,*) ALVSG
      WRITE(10,*) ALIRG
      WRITE(10,*) ALVSCS
      WRITE(10,*) ALIRCS
      WRITE(10,*) ALVSSN
      WRITE(10,*) ALIRSN
      WRITE(10,*) ALVSGC
      WRITE(10,*) ALIRGC
      WRITE(10,*) ALVSSC
      WRITE(10,*) ALIRSC
      WRITE(10,*) TRVSCN
      WRITE(10,*) TRIRCN
      WRITE(10,*) TRVSCS
      WRITE(10,*) TRIRCS
      WRITE(10,*) FSVF
      WRITE(10,*) FSVFS
      WRITE(10,*) RAICAN
      WRITE(10,*) RAICNS
      WRITE(10,*) SNOCAN
      WRITE(10,*) SNOCNS
      WRITE(10,*) FRAINC
      WRITE(10,*) FSNOWC
      WRITE(10,*) FRAICS
      WRITE(10,*) FSNOCS
      WRITE(10,*) DISP
      WRITE(10,*) DISPS
      WRITE(10,*) ZOMLNC
      WRITE(10,*) ZOMLCS
      WRITE(10,*) ZOELNC
      WRITE(10,*) ZOELCS
      WRITE(10,*) ZOMLNG
      WRITE(10,*) ZOMLNS
      WRITE(10,*) ZOELNG
      WRITE(10,*) ZOELNS
      WRITE(10,*) CHCAP
      WRITE(10,*) CHCAPS
      WRITE(10,*) CMASSC
      WRITE(10,*) CMASCS
      WRITE(10,*) CWLCAP
      WRITE(10,*) CWFCAP
      WRITE(10,*) CWLCPS
      WRITE(10,*) CWFCPS
      WRITE(10,*) RC
      WRITE(10,*) RCS
      WRITE(10,*) RBCOEF
      WRITE(10,*) FROOT
      WRITE(10,*) ZPLIMC
      WRITE(10,*) ZPLIMG
      WRITE(10,*) ZPLMCS
      WRITE(10,*) ZPLMGS
      WRITE(10,*) TRSNOW
      WRITE(10,*) ZSNOW
      WRITE(10,*) IDAY
      WRITE(10,*) JLAT
      WRITE(10,*) IDISP
      WRITE(10,*) IZREF
      WRITE(10,*) IWF
      WRITE(10,*) ILAI
      WRITE(10,*) IHGT
      WRITE(10,*) IALC
      WRITE(10,*) IALS
      WRITE(10,*) IALG
      WRITE(10,*) TBARC
      WRITE(10,*) TBARG
      WRITE(10,*) TBARCS
      WRITE(10,*) TBARGS
      WRITE(10,*) THLIQC
      WRITE(10,*) THLIQG
      WRITE(10,*) THICEC
      WRITE(10,*) THICEG
      WRITE(10,*) HCPC
      WRITE(10,*) HCPG
      WRITE(10,*) TCTOPC
      WRITE(10,*) TCBOTC
      WRITE(10,*) TCTOPG
      WRITE(10,*) TCBOTG
      WRITE(10,*) GZEROC
      WRITE(10,*) GZEROG
      WRITE(10,*) GZROCS
      WRITE(10,*) GZROGS
      WRITE(10,*) G12C
      WRITE(10,*) G12G
      WRITE(10,*) G12CS
      WRITE(10,*) G12GS
      WRITE(10,*) G23C
      WRITE(10,*) G23G
      WRITE(10,*) G23CS
      WRITE(10,*) G23GS
      WRITE(10,*) QFREZC
      WRITE(10,*) QFREZG
      WRITE(10,*) QMELTC
      WRITE(10,*) QMELTG
      WRITE(10,*) EVAPC
      WRITE(10,*) EVAPCG
      WRITE(10,*) EVAPG
      WRITE(10,*) EVAPCS
      WRITE(10,*) EVPCSG
      WRITE(10,*) EVAPGS
      WRITE(10,*) TCANO
      WRITE(10,*) TCANS
      WRITE(10,*) TPONDC
      WRITE(10,*) TPONDG
      WRITE(10,*) TPNDCS
      WRITE(10,*) TPNDGS
      WRITE(10,*) TSNOCS
      WRITE(10,*) TSNOGS
      WRITE(10,*) WSNOCS
      WRITE(10,*) WSNOGS
      WRITE(10,*) RHOSCS
      WRITE(10,*) RHOSGS
      WRITE(10,*) WTABGAT
      WRITE(10,*) ILMOGAT
      WRITE(10,*) UEGAT
      WRITE(10,*) HBLGAT
      WRITE(10,*) ILW
      WRITE(10,*) ITC
      WRITE(10,*) ITCG
      WRITE(10,*) ITG
      WRITE(10,*) ISLFD
      WRITE(10,*) NLANDCS
      WRITE(10,*) NLANDGS
      WRITE(10,*) NLANDC
      WRITE(10,*) NLANDG
      WRITE(10,*) NLANDI
      WRITE(10,*) GFLXGAT
      WRITE(10,*) CDHROW
      WRITE(10,*) CDMROW
      WRITE(10,*) HFSROW
      WRITE(10,*) TFXROW
      WRITE(10,*) QEVPROW
      WRITE(10,*) QFSROW
      WRITE(10,*) QFXROW
      WRITE(10,*) PETROW
      WRITE(10,*) GAROW
      WRITE(10,*) EFROW
      WRITE(10,*) GTROW
      WRITE(10,*) QGROW
      WRITE(10,*) TSFROW
      WRITE(10,*) ALVSROW
      WRITE(10,*) ALIRROW
      WRITE(10,*) SFCTROW
      WRITE(10,*) SFCUROW
      WRITE(10,*) SFCVROW
      WRITE(10,*) SFCQROW
      WRITE(10,*) FSGVROW
      WRITE(10,*) FSGSROW
      WRITE(10,*) FSGGROW
      WRITE(10,*) FLGVROW
      WRITE(10,*) FLGSROW
      WRITE(10,*) FLGGROW
      WRITE(10,*) HFSCROW
      WRITE(10,*) HFSSROW
      WRITE(10,*) HFSGROW
      WRITE(10,*) HEVCROW
      WRITE(10,*) HEVSROW
      WRITE(10,*) HEVGROW
      WRITE(10,*) HMFCROW
      WRITE(10,*) HMFNROW
      WRITE(10,*) HTCCROW
      WRITE(10,*) HTCSROW
      WRITE(10,*) PCFCROW
      WRITE(10,*) PCLCROW
      WRITE(10,*) PCPNROW
      WRITE(10,*) PCPGROW
      WRITE(10,*) QFGROW
      WRITE(10,*) QFNROW
      WRITE(10,*) QFCLROW
      WRITE(10,*) QFCFROW
      WRITE(10,*) ROFROW
      WRITE(10,*) ROFOROW
      WRITE(10,*) ROFSROW
      WRITE(10,*) ROFBROW
      WRITE(10,*) TROFROW
      WRITE(10,*) TROOROW
      WRITE(10,*) TROSROW
      WRITE(10,*) TROBROW
      WRITE(10,*) ROFCROW
      WRITE(10,*) ROFNROW
      WRITE(10,*) ROVGROW
      WRITE(10,*) WTRCROW
      WRITE(10,*) WTRSROW
      WRITE(10,*) WTRGROW
      WRITE(10,*) DRROW
      WRITE(10,*) WTABROW
      WRITE(10,*) ILMOROW
      WRITE(10,*) UEROW
      WRITE(10,*) HBLROW
      WRITE(10,*) HMFGROW
      WRITE(10,*) HTCROW
      WRITE(10,*) QFCROW
      WRITE(10,*) FSNOROW
      WRITE(10,*) ITCTROW
      WRITE(10,*) NCOUNT
      WRITE(10,*) ireport
      WRITE(10,*) wfo_seq
      WRITE(10,*) IYEAR
      WRITE(10,*) ensim_MONTH
      WRITE(10,*) ensim_DAY
      WRITE(10,*) IHOUR
      WRITE(10,*) XXX
      WRITE(10,*) YYY
      WRITE(10,*) NLAT
      WRITE(10,*) NMOS
      WRITE(10,*) DELT
      WRITE(10,*) TFREZ
      WRITE(10,*) UVGRD
      WRITE(10,*) SBC
      WRITE(10,*) RHOW
      WRITE(10,*) CURREC
      WRITE(10,*) WF_ROUTETIMESTEP
      WRITE(10,*) WF_R1
      WRITE(10,*) WF_R2
      WRITE(10,*) NAA
      WRITE(10,*) IYMIN
      WRITE(10,*) WF_IYMAX
      WRITE(10,*) JXMIN
      WRITE(10,*) WF_JXMAX
      WRITE(10,*) WF_IBN
      WRITE(10,*) WF_IROUGH
      WRITE(10,*) WF_ICHNL
      WRITE(10,*) WF_NEXT
      WRITE(10,*) WF_IREACH
      WRITE(10,*) AL
      WRITE(10,*) GRDN
      WRITE(10,*) GRDE
      WRITE(10,*) WF_DA
      WRITE(10,*) WF_BNKFLL
      WRITE(10,*) WF_CHANNELSLOPE
      WRITE(10,*) WF_ELEV
      WRITE(10,*) FRAC
      WRITE(10,*) WF_NO
      WRITE(10,*) WF_NL
      WRITE(10,*) WF_MHRD
      WRITE(10,*) WF_KT
      WRITE(10,*) WF_IY
      WRITE(10,*) WF_JX
      WRITE(10,*) WF_QHYD
      WRITE(10,*) WF_RES
      WRITE(10,*) WF_RESSTORE
      WRITE(10,*) WF_NORESV_CTRL
      WRITE(10,*) WF_R
      WRITE(10,*) WF_NORESV
      WRITE(10,*) WF_NREL
      WRITE(10,*) WF_KTR
      WRITE(10,*) WF_IRES
      WRITE(10,*) WF_JRES
      WRITE(10,*) WF_B1
      WRITE(10,*) WF_B2
      WRITE(10,*) WF_QREL
      WRITE(10,*) WF_QR
      WRITE(10,*) WF_TIMECOUNT
      WRITE(10,*) WF_NHYD
      WRITE(10,*) WF_QBASE
      WRITE(10,*) WF_QI1
      WRITE(10,*) WF_QI2
      WRITE(10,*) WF_QO1
      WRITE(10,*) WF_QO2
      WRITE(10,*) WF_STORE1
      WRITE(10,*) WF_STORE2
      WRITE(10,*) DRIVERTIMESTEP
      WRITE(10,*) ROFGRD
      WRITE(10,*) WF_S
      WRITE(10,*) JAN
      WRITE(10,*) IDAY
      WRITE(10,*) IHOUR
      WRITE(10,*) IMIN
      WRITE(10,*) IMIN2
      WRITE(10,*) TOTAL_ROFACC
      WRITE(10,*) TOTAL_ROFOACC
      WRITE(10,*) TOTAL_ROFSACC
      WRITE(10,*) TOTAL_ROFBACC
      WRITE(10,*) TOTAL_EVAPACC
      WRITE(10,*) TOTAL_PREACC
      WRITE(10,*) INIT_STORE
      WRITE(10,*) FINAL_STORE
      WRITE(10,*) TOTAL_AREA
      
      CLOSE (10)
      RETURN
      END
