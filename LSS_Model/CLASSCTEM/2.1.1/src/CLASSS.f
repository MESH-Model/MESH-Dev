!>\file
!>
!!Purpose: Scatter variables from long, gathered vectors back onto original two-dimensional arrays (latitude
!!circle x mosaic tiles).
!!
      SUBROUTINE CLASSS (TBARROT,THLQROT,THICROT,TSFSROT,TPNDROT,       
     1                   ZPNDROT,TBASROT,ALBSROT,TSNOROT,RHOSROT,       
     2                   SNOROT, GTROT,  TCANROT,RCANROT,SCANROT,       
     3                  GROROT, CMAIROT,TACROT, QACROT, WSNOROT,
     4                   REFROT, BCSNROT,EMISROT,SALBROT,CSALROT,       
     5                   ILMOS,JLMOS,                                   
     6                   NML,NL,NT,NM,ILG,IG,IC,ICP1,NBS,               
     7                   TBARGAT,THLQGAT,THICGAT,TSFSGAT,TPNDGAT,       
     8                   ZPNDGAT,TBASGAT,ALBSGAT,TSNOGAT,RHOSGAT,       
     9                   SNOGAT, GTGAT,  TCANGAT,RCANGAT,SCANGAT,       
     A                   GROGAT, CMAIGAT,TACGAT, QACGAT, WSNOGAT,       
     B                   REFGAT, BCSNGAT,EMISGAT,SALBGAT,CSALGAT)       
C                                                                       
C     * Jun 20, 2014 - M.Lazare. New version for gcm18, called          
C     *                          by new "sfcproc2":                     
C     *                          - Adds SALBGAT/SALBROT and             
C     *                            CSALGAT,CSALROT (need to pass        
C     *                            NBS as well).                        
C     *                          - Adds EMISGAT/EMISROT.                
C     *                          - Adds GTGAT/GTROT.                    
C     *                          - Adds NT (NTLD in sfcproc2            
C     *                            call) to dimension land-only         
C     *                            ROT fields, consistent with          
C     *                            new comrow12.                        
C     *                          - Unused IWMOS,JWMOS removed.          
C     * Jun 12, 2013 - M.Lazare. Previous version for gcm17,            
C     *                          called by "sfcproc".                   
C     *                          CLASS scatter routine called by        
C     *                          "sfcproc" in new version gcm17.        
C     * NOTE: This contains the following changes compared to the       
C     *       working temporary version used in conjunction with        
C     *       updates to gcm16 (ie not official):                       
C     *         1) {REF,BCSN} added for Maryam's new code.              
C     *         2) GFLX removed.                                        
C
C     * OCT 25/11 - M.LAZARE.   REMOVE OPERATIONS ON INTERNAL
C     *                         ROT ARRAYS (NOW DONE DIRECTLY
C     *                         GAT->ROW IN SFCPROC).
C     * OCT 07/11 - M.LAZARE.   REMOVE TSF.
C     * OCT 05/11 - M.LAZARE.   ADD SFCH.
C     * OCT 04/11 - M.LAZARE.   REMOVE ITCT.
C     * MAR 23/06 - D.VERSEGHY. ADD WSNO,FSNO.
C     * MAR 18/05 - D.VERSEGHY. ADDITIONAL VARIABLES.
C     * FEB 18/05 - D.VERSEGHY. ADD "TSFS" VARIABLES.
C     * AUG 05/04 - D.VERSEGHY. ADD NEW DIAGNOSTIC VARIABLES
C     *                         ILMO, UE AND HBL.
C     * AUG 15/02 - D.VERSEGHY. SCATTER OPERATION ON CLASS 
C     *                         VARIABLES.
C
      IMPLICIT NONE
C
C     * INTEGER CONSTANTS.
C
      INTEGER NML,NL,NT,NM,ILG,IG,IC,ICP1,NBS,K,L,M                     
C
C     * LAND SURFACE PROGNOSTIC VARIABLES.
C


      REAL    SALBROT(NL,NM,NBS) !<
                                 !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    CSALROT(NL,NM,NBS) !<                  
                                 !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    TBARROT(NL,NT,IG) !<Temperature of soil layers [K]
                                !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    THLQROT(NL,NT,IG) !<Volumetric liquid water content of soil layers \f$[m^3 m^{-3} ]\f$
                                !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    THICROT(NL,NT,IG) !<Volumetric frozen water content of soil layers \f$[m^3 m^{-3} ]\f$
                                !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    TSFSROT(NL,NT,4) !<Ground surface temperature over subarea [K]                            
                               !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    TPNDROT(NL,NT) !<Temperature of ponded water [K]
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    ZPNDROT(NL,NT) !<Depth of ponded water on surface [m]
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    TBASROT(NL,NT) !<Temperature of bedrock in third soil layer [K]
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    ALBSROT(NL,NM) !<Snow albedo [ ]
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    TSNOROT(NL,NM) !<Snowpack temperature [K]
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    RHOSROT(NL,NM) !<Density of snow \f$[kg m^{-3} ]\f$
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    SNOROT (NL,NM) !<Mass of snow pack \f$[kg m^{-2} ]\f$
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    GTROT  (NL,NM) !<
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    TCANROT(NL,NT) !<Vegetation canopy temperature [K]
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    RCANROT(NL,NT) !<Intercepted liquid water stored on canopy \f$[kg m^{-2} ]\f$
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    SCANROT(NL,NT) !<Intercepted frozen water stored on canopy \f$[kg m^{-2} ]\f$
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    GROROT (NL,NT) !<Vegetation growth index [ ]   
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    TACROT (NL,NT) !<Temperature of air within vegetation canopy [K]
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    QACROT (NL,NT) !<Specific humidity of air within vegetation canopy space \f$[kg kg^{-1} ]\f$
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    WSNOROT(NL,NT) !<Liquid water content of snow pack \f$[kg m^{-2} ]\f$
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    CMAIROT(NL,NT) !<Aggregated mass of vegetation canopy \f$[kg m^{-2} ]\f$
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    REFROT (NL,NM) !<
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    BCSNROT(NL,NM) !<    
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
      REAL    EMISROT(NL,NM) !<                                            
                             !!Suffix ROT refers to variables on original two-dimensional arrays.
C
      REAL    SALBGAT(ILG,NBS) !<
                               !!Suffix GAT refers to variables on gathered long vectors.
      REAL    CSALGAT(ILG,NBS) !<                       
                               !!Suffix GAT refers to variables on gathered long vectors.
      REAL    TBARGAT(ILG,IG) !<Temperature of soil layers [K]
                              !!Suffix GAT refers to variables on gathered long vectors.
      REAL    THLQGAT(ILG,IG) !<Volumetric liquid water content of soil layers \f$[m^3 m^{-3} ]\f$
                              !!Suffix GAT refers to variables on gathered long vectors.
      REAL    THICGAT(ILG,IG) !<Volumetric frozen water content of soil layers \f$[m^3 m^{-3} ]\f$
                              !!Suffix GAT refers to variables on gathered long vectors.
      REAL    TSFSGAT(ILG,4) !<Ground surface temperature over subarea [K]
                             !!Suffix GAT refers to variables on gathered long vectors.
      REAL    TPNDGAT(ILG) !<Temperature of ponded water [K]
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    ZPNDGAT(ILG) !<Depth of ponded water on surface [m]
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    TBASGAT(ILG) !<Temperature of bedrock in third soil layer [K]
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    ALBSGAT(ILG) !<Snow albedo [ ]
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    TSNOGAT(ILG) !<Snowpack temperature [K]
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    RHOSGAT(ILG) !<Density of snow \f$[kg m^{-3} ]\f$
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    SNOGAT (ILG) !<Mass of snow pack \f$[kg m^{-2} ]\f$
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    GTGAT  (ILG) !<
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    TCANGAT(ILG) !<Vegetation canopy temperature [K]
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    RCANGAT(ILG) !<Intercepted liquid water stored on canopy \f$[kg m^{-2} ]\f$
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    SCANGAT(ILG) !<Intercepted frozen water stored on canopy \f$[kg m^{-2} ]\f$
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    GROGAT (ILG) !<Vegetation growth index [ ]
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    TACGAT (ILG) !<Temperature of air within vegetation canopy [K]
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    QACGAT (ILG) !<Specific humidity of air within vegetation canopy space \f$[kg kg^{-1} ]\f$
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    WSNOGAT(ILG) !<Liquid water content of snow pack \f$[kg m^{-2} ]\f$
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    CMAIGAT(ILG) !<Aggregated mass of vegetation canopy \f$[kg m^{-2} ]\f$
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    REFGAT (ILG) !<
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    BCSNGAT(ILG) !<
                           !!Suffix GAT refers to variables on gathered long vectors.
      REAL    EMISGAT(ILG) !<
                           !!Suffix GAT refers to variables on gathered long vectors.
                                              
C
C     * GATHER-SCATTER INDEX ARRAYS.
C
      INTEGER  ILMOS (ILG),  JLMOS  (ILG)                               
C----------------------------------------------------------------------
      DO 100 K=1,NML
          TPNDROT(ILMOS(K),JLMOS(K))=TPNDGAT(K)  
          ZPNDROT(ILMOS(K),JLMOS(K))=ZPNDGAT(K)  
          TBASROT(ILMOS(K),JLMOS(K))=TBASGAT(K)  
          ALBSROT(ILMOS(K),JLMOS(K))=ALBSGAT(K)  
          TSNOROT(ILMOS(K),JLMOS(K))=TSNOGAT(K)  
          RHOSROT(ILMOS(K),JLMOS(K))=RHOSGAT(K)  
          SNOROT (ILMOS(K),JLMOS(K))=SNOGAT (K)  
          GTROT  (ILMOS(K),JLMOS(K))=GTGAT  (K)                         
          WSNOROT(ILMOS(K),JLMOS(K))=WSNOGAT(K)  
          TCANROT(ILMOS(K),JLMOS(K))=TCANGAT(K)  
          RCANROT(ILMOS(K),JLMOS(K))=RCANGAT(K)  
          SCANROT(ILMOS(K),JLMOS(K))=SCANGAT(K)  
          GROROT (ILMOS(K),JLMOS(K))=GROGAT (K)  
          TACROT (ILMOS(K),JLMOS(K))=TACGAT (K)  
          QACROT (ILMOS(K),JLMOS(K))=QACGAT (K)  
          CMAIROT(ILMOS(K),JLMOS(K))=CMAIGAT(K)  
          REFROT (ILMOS(K),JLMOS(K))=REFGAT (K)                         
          BCSNROT(ILMOS(K),JLMOS(K))=BCSNGAT(K)                         
          EMISROT(ILMOS(K),JLMOS(K))=EMISGAT(K)      

!>
!!The prognostic variables are scattered from the long, gathered arrays (collapsing the latitude and mosaic
!!dimensions into one) back onto the original arrays using the pointer vectors generated in GATPREP.
!!

  100 CONTINUE
C
      DO 200 L=1,NBS                                                    
      DO 200 K=1,NML
          SALBROT(ILMOS(K),JLMOS(K),L)=SALBGAT(K,L)                     
          CSALROT(ILMOS(K),JLMOS(K),L)=CSALGAT(K,L)                     
  200 CONTINUE                                                          
C                                                                       
      DO 300 L=1,IG                                                     
      DO 300 K=1,NML                                                    
          TBARROT(ILMOS(K),JLMOS(K),L)=TBARGAT(K,L)
          THLQROT(ILMOS(K),JLMOS(K),L)=THLQGAT(K,L)
          THICROT(ILMOS(K),JLMOS(K),L)=THICGAT(K,L)
  300 CONTINUE                                                          
C
      DO 400 L=1,4                                                      
      DO 400 K=1,NML                                                    
          TSFSROT(ILMOS(K),JLMOS(K),L)=TSFSGAT(K,L)
  400 CONTINUE                                                          
C
      RETURN
      END
