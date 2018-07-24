!>\file
C>
C!Purpose: Gather variables from two-dimensional arrays (latitude 
C!circle x mosaic tiles) onto long vectors for optimum processing 
C!efficiency on vector supercomputers.
C!

      SUBROUTINE CLASSG(TBARGAT,THLQGAT,THICGAT,TPNDGAT,ZPNDGAT,
     1                  TBASGAT,ALBSGAT,TSNOGAT,RHOSGAT,SNOGAT, 
     2                  TCANGAT,RCANGAT,SCANGAT,GROGAT, CMAIGAT,
     3                  FCANGAT,LNZ0GAT,ALVCGAT,ALICGAT,PAMXGAT,
     4                  PAMNGAT,CMASGAT,ROOTGAT,RSMNGAT,QA50GAT,
     5                  VPDAGAT,VPDBGAT,PSGAGAT,PSGBGAT,PAIDGAT,
     6                  HGTDGAT,ACVDGAT,ACIDGAT,TSFSGAT,WSNOGAT,
     7                  THPGAT, THRGAT, THMGAT, BIGAT,  PSISGAT,
     8                  GRKSGAT,THRAGAT,HCPSGAT,TCSGAT, IGDRGAT,
     9                   THFCGAT,THLWGAT,PSIWGAT,DLZWGAT,ZBTWGAT,
     A                   VMODGAT,ZSNLGAT,ZPLGGAT,ZPLSGAT,TACGAT, 
     B                   QACGAT,DRNGAT,DDGAT,XSLPGAT,GRKFGAT,WFSFGAT,
     C                   WFCIGAT,ALGWVGAT,ALGWNGAT,ALGDVGAT,ALGDNGAT,
     +                   ALGWGAT,ALGDGAT,ASVDGAT,ASIDGAT,AGVDGAT,
     D                  AGIDGAT,ISNDGAT,RADJGAT,ZBLDGAT,Z0ORGAT,
     E                  ZRFMGAT,ZRFHGAT,ZDMGAT, ZDHGAT, FSVHGAT,
     F                   FSIHGAT,FSDBGAT,FSFBGAT,FSSBGAT,CSZGAT,        
     +                   FSGGAT, FLGGAT, FDLGAT, ULGAT,  VLGAT,         
     G                  TAGAT,  QAGAT,  PRESGAT,PREGAT, PADRGAT,
     H                  VPDGAT, TADPGAT,RHOAGAT,RPCPGAT,TRPCGAT,
     I                  SPCPGAT,TSPCGAT,RHSIGAT,FCLOGAT,DLONGAT,
     J                   GGEOGAT,GUSTGAT,REFGAT, BCSNGAT,DEPBGAT,
     K                   ILMOS,JLMOS,
     L                   NML,NL,NT,NM,ILG,IG,IC,ICP1,NBS,               
     M                  TBARROT,THLQROT,THICROT,TPNDROT,ZPNDROT,
     N                  TBASROT,ALBSROT,TSNOROT,RHOSROT,SNOROT, 
     O                  TCANROT,RCANROT,SCANROT,GROROT, CMAIROT,
     P                  FCANROT,LNZ0ROT,ALVCROT,ALICROT,PAMXROT,
     Q                  PAMNROT,CMASROT,ROOTROT,RSMNROT,QA50ROT,
     R                  VPDAROT,VPDBROT,PSGAROT,PSGBROT,PAIDROT,
     S                  HGTDROT,ACVDROT,ACIDROT,TSFSROT,WSNOROT,
     T                  THPROT, THRROT, THMROT, BIROT,  PSISROT,
     U                  GRKSROT,THRAROT,HCPSROT,TCSROT, IGDRROT,
     V                   THFCROT,THLWROT,PSIWROT,DLZWROT,ZBTWROT,
     W                   VMODL, ZSNLROT,ZPLGROT,ZPLSROT,TACROT, 
     X                   QACROT,DRNROT,DDROT,XSLPROT,GRKFROT,WFSFROT,
     Y                   WFCIROT,ALGWVROT,ALGWNROT,ALGDVROT,ALGDNROT,
     =                   ALGWROT,ALGDROT,ASVDROT,ASIDROT,AGVDROT,    
     Z                  AGIDROT,ISNDROT,RADJ   ,ZBLDROW,Z0ORROW,
     +                  ZRFMROW,ZRFHROW,ZDMROW, ZDHROW, FSVHROW,
     +                   FSIHROW,FSDBROL,FSFBROL,FSSBROL,CSZROW,        
     +                   FSGROL, FLGROL, FDLROL, ULROW,  VLROW,         
     +                  TAROW,  QAROW,  PRESROW,PREROW, PADRROW,
     +                  VPDROW, TADPROW,RHOAROW,RPCPROW,TRPCROW,
     +                  SPCPROW,TSPCROW,RHSIROW,FCLOROW,DLONROW,
     +                  GGEOROW,GUSTROL,REFROT, BCSNROT,DEPBROW )

C     * Dec 30, 2014 - D.Verseghy. Re-introduce ALGW,ALGD.
C     * Aug 19, 2014 - M.Lazare. New version called by "sfcproc2":      
C     *                          - {ALGWV,ALGWN,ALGDV,ALGDN} replace    
C     *                            {ALGW,ALGD}.                         
C     *                          - FSG,FLG,GUST added.                  
C     *                          - FDLROW changed to FDLROL (cosmetic). 
C     *                          - Adds GTGAT/GTROT.                    
C     *                          - Adds NT (NTLD in sfcproc2            
C     *                            call) to dimension land-only         
C     *                            ROT fields, consistent with          
C     *                            new comrow12.                        
C     *                          - Unused IWMOS,JWMOS removed.          
C     * Jun 13, 2013 - M.Lazare. CLASS gather routine called by         
C     *                          "sfcproc" in new version gcm17.        
C     * NOTE: This contains the following changes compared to the       
C     *       working temporary version used in conjunction with        
C     *       updates to gcm16 (ie not official):                       
C     *         1) {DEPB,REF,BCSN} added for Maryam's new code.         
C     *         2) {FSDB,FSFB,FSSB} added for Jason's new code.   
C     * OCT 18/11 - M.LAZARE.  ADD IGDR.
C     * OCT 07/11 - M.LAZARE.  ADD VMODL->VMODGAT.
C     * OCT 05/11 - M.LAZARE.  PUT BACK IN PRESGROW->PRESGAT
C     *                        REQUIRED FOR ADDED SURFACE RH 
C     *                        CALCULATION.
C     * OCT 03/11 - M.LAZARE.  REMOVE ALL INITIALIZATION TO
C     *                        ZERO OF GAT ARRAYS (NOW DONE
C     *                        IN CLASS DRIVER).
C     * SEP 16/11 - M.LAZARE.  - ROW->ROT AND GRD->ROW.
C     *                        - REMOVE INITIALIZATION OF
C     *                          {ALVS,ALIR} TO ZERO.
C     *                        - REMOVE PRESGROW->PRESGAT 
C     *                          (OCEAN-ONLY NOW).
C     *                        - RADJROW (64-BIT) NOW RADJ
C     *                          (32-BIT).
C     * MAR 23/06 - D.VERSEGHY. ADD WSNO,FSNO,GGEO.
C     * MAR 18/05 - D.VERSEGHY. ADDITIONAL VARIABLES.
C     * FEB 18/05 - D.VERSEGHY. ADD "TSFS" VARIABLES.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 15/02 - D.VERSEGHY. GATHER OPERATION ON CLASS 
C     *                         VARIABLES.
C 
      IMPLICIT NONE
C
C     (Suffix GAT refers to variables on gathered long vectors; suffix 
C     ROT refers to variables on original two-dimensional arrays.)
C
C     * INTEGER CONSTANTS.
C
      INTEGER  NML,NL,NM,NT,ILG,IG,IC,ICP1,K,L,M,NBS                    
C
C     * LAND SURFACE PROGNOSTIC VARIABLES.
C
      REAL TBARROT(NL,NT,IG)    !<Temperature of soil layers [K]
      REAL THLQROT(NL,NT,IG)    !<Volumetric liquid water content of soil 
                                !!layers \f$[m^3 m^{-3}]\f$
      REAL THICROT(NL,NT,IG)    !<Frozen water content of soil layers 
                                !!under vegetation \f$[m^3 m^{-3}]\f$
      REAL TPNDROT(NL,NT)   !<Temperature of ponded water [K]    
      REAL ZPNDROT(NL,NT)   !<Depth of ponded water on surface [m]
      REAL TBASROT(NL,NT)   !<Temperature of bedrock in third soil layer [K]
      REAL ALBSROT(NL,NM)   !<Snow albedo [ ]
      REAL TSNOROT(NL,NM)   !<Snowpack temperature [K]
      REAL RHOSROT(NL,NM)   !<Density of snow \f$[kg m^{-3}]\f$
      REAL SNOROT (NL,NM)   !<Mass of snow pack \f$[kg m^{-2}]\f$
      REAL TCANROT(NL,NT)   !<Vegetation canopy temperature [K] 
      REAL RCANROT(NL,NT)   !<Intercepted liquid water stored on canopy \f$[kg m^{-2}]\f$
      REAL SCANROT(NL,NT)   !<Intercepted frozen water stored on canopy \f$[kg m^{-2}]\f$
      REAL GROROT (NL,NT)   !<Vegetation growth index [ ] 
      REAL CMAIROT(NL,NT)   !<Aggregated mass of vegetation canopy \f$[kg m^{-2}]\f$
      REAL TSFSROT(NL,NT,4) !<Ground surface temperature over subarea [K] 
      REAL TACROT (NL,NT)   !<Temperature of air within vegetation canopy [K]
      REAL QACROT (NL,NT)   !<Specific humidity of air within vegetation 
                            !!canopy \f$[kg kg^{-1}]\f$
      REAL WSNOROT(NL,NT)   !<Liquid water content of snow pack \f$[kg m^{-2}]\f$
      REAL REFROT(NL,NM)    !
      REAL BCSNROT(NL,NM)   !
C
      REAL    TBARGAT(ILG,IG),   THLQGAT(ILG,IG),   THICGAT(ILG,IG), 
     1        TPNDGAT(ILG),      ZPNDGAT(ILG),      TBASGAT(ILG),   
     2        ALBSGAT(ILG),      TSNOGAT(ILG),      RHOSGAT(ILG),   
     3        SNOGAT (ILG),      TCANGAT(ILG),      RCANGAT(ILG),   
     4        SCANGAT(ILG),      GROGAT (ILG),      CMAIGAT(ILG),
     5        TSFSGAT(ILG,4),    TACGAT (ILG),      QACGAT (ILG),
     6        WSNOGAT(ILG),      REFGAT (ILG),      BCSNGAT(ILG)
C
C     * GATHER-SCATTER INDEX ARRAYS.
C
      INTEGER  ILMOS (ILG)  !<Index of latitude grid cell corresponding 
                            !!to current element of gathered vector of 
                            !!land surface variables [ ]
      INTEGER  JLMOS (ILG)  !<Index of mosaic tile corresponding to 
                            !!current element of gathered vector of land 
                            !!surface variables [ ]

C
C     * CANOPY AND SOIL INFORMATION ARRAYS.
C     * (THE LENGTH OF THESE ARRAYS IS DETERMINED BY THE NUMBER
C     * OF SOIL LAYERS (3) AND THE NUMBER OF BROAD VEGETATION
C     * CATEGORIES (4, OR 5 INCLUDING URBAN AREAS).)
C
      REAL FCANROT(NL,NT,ICP1)  !<Maximum fractional coverage of modelled 
                                !!area by vegetation category [ ]
      REAL LNZ0ROT(NL,NT,ICP1)  !<Natural logarithm of maximum roughness 
                                !!length of vegetation category [ ]
      REAL ALVCROT(NL,NT,ICP1)  !<Background average visible albedo of 
                                !!vegetation category [ ]
      REAL ALICROT(NL,NT,ICP1)  !<Background average near-infrared albedo 
                                !!of vegetation category [ ]
      REAL PAMXROT(NL,NT,IC)    !<Maximum plant area index of vegetation 
                                !!category [ ]
      REAL PAMNROT(NL,NT,IC)    !<Minimum plant area index of vegetation 
                                !!category [ ]
      REAL CMASROT(NL,NT,IC)    !<Maximum canopy mass for vegetation 
                                !!category \f$[kg m^{-2}]\f$
      REAL ROOTROT(NL,NT,IC)    !<Maximum rooting depth of vegetation 
                                !!category [m]
      REAL RSMNROT(NL,NT,IC)    !<Minimum stomatal resistance of 
                                !!vegetation category \f$[s m^{-1}]\f$
      REAL QA50ROT(NL,NT,IC)    !<Reference value of incoming shortwave 
                                !!radiation for vegetation category (used 
                                !!in stomatal resistance calculation) \f$[W m^{-2}]\f$
      REAL VPDAROT(NL,NT,IC)    !<Vapour pressure deficit coefficient for 
                                !!vegetation category (used in stomatal 
                                !!resistance calculation) [ ]
      REAL VPDBROT(NL,NT,IC)    !<Vapour pressure deficit coefficient for 
                                !!vegetation category (used in stomatal 
                                !!resistance calculation) [ ]
      REAL PSGAROT(NL,NT,IC)    !<Soil moisture suction coefficient for 
                                !!vegetation category (used in stomatal 
                                !!resistance calculation) [ ]
      REAL PSGBROT(NL,NT,IC)    !<Soil moisture suction coefficient for 
                                !!vegetation category (used in stomatal 
                                !!resistance calculation) [ ]
      REAL PAIDROT(NL,NT,IC)    !<Optional user-specified value of plant 
                                !!area indices of vegetation categories 
                                !!to override CLASS-calculated values [ ]
      REAL HGTDROT(NL,NT,IC)    !<Optional user-specified values of 
                                !!height of vegetation categories to 
                                !!override CLASS-calculated values [m]
      REAL ACVDROT(NL,NT,IC)    !<Optional user-specified value of canopy 
                                !!visible albedo to override CLASS-
                                !!calculated value [ ]
      REAL ACIDROT(NL,NT,IC)    !<Optional user-specified value of canopy 
                                !!near-infrared albedo to override CLASS-
                                !!calculated value [ ]
C
      REAL          FCANGAT(ILG,ICP1),   LNZ0GAT(ILG,ICP1),
     1              ALVCGAT(ILG,ICP1),   ALICGAT(ILG,ICP1),
     2              PAMXGAT(ILG,IC),     PAMNGAT(ILG,IC),
     3              CMASGAT(ILG,IC),     ROOTGAT(ILG,IC),
     4              RSMNGAT(ILG,IC),     QA50GAT(ILG,IC),
     5              VPDAGAT(ILG,IC),     VPDBGAT(ILG,IC),
     6              PSGAGAT(ILG,IC),     PSGBGAT(ILG,IC),
     7              PAIDGAT(ILG,IC),     HGTDGAT(ILG,IC),
     8              ACVDGAT(ILG,IC),     ACIDGAT(ILG,IC)
C
      REAL THPROT (NL,NT,IG)    !<Pore volume in soil layer \f$[m^3 m^{-3}]\f$
      REAL THRROT (NL,NT,IG)    !<Liquid water retention capacity for 
                                !!organic soil \f$[m^3 m^{-3}]\f$
      REAL THMROT (NL,NT,IG)    !<Residual soil liquid water content 
                                !!remaining after freezing or evaporation \f$[m^3 m^{-3}]\f$
      REAL BIROT  (NL,NT,IG)    !<Clapp and Hornberger empirical "b"
                                !!parameter [ ]
      REAL PSISROT(NL,NT,IG)    !<Soil moisture suction at saturation [m]
      REAL GRKSROT(NL,NT,IG)    !<Saturated hydraulic conductivity of 
                                !!soil layers \f$[m s^{-1}]\f$
      REAL THRAROT(NL,NT,IG)    !<Fractional saturation of soil behind 
                                !!the wetting front [ ]
      REAL HCPSROT(NL,NT,IG)    !<Volumetric heat capacity of soil 
                                !!particles \f$[J m^{-3}]\f$
      REAL TCSROT (NL,NT,IG)    !<Thermal conductivity of soil particles 
                                !!\f$[W m^{-1} K^{-1}]\f$
      REAL THFCROT(NL,NT,IG)    !<Field capacity \f$[m^3 m^{-3}]\f$
      REAL THLWROT(NL,NT,IG)    !<Wilting point \f$[m^3 m^{-3}]\f$
      REAL PSIWROT(NL,NT,IG)    !<Soil moisture suction at wilting point 
                                !![m]
      REAL DLZWROT(NL,NT,IG)    !<Permeable thickness of soil layer [m]
      REAL ZBTWROT(NL,NT,IG)    !<Depth to permeable bottom of soil layer [m]
      REAL DRNROT (NL,NT)       !<Drainage index at bottom of soil profile [ ]
      REAL DDROT  (NL,NT)       !<Drainage Density [km^{-1}]
      REAL XSLPROT(NL,NT)       !<Surface slope (used when running MESH code) [degrees]
      REAL GRKFROT(NL,NT)       !<WATROF parameter used when running MESH code [ ]
      REAL WFSFROT(NL,NT)       !<WATROF parameter used when running MESH code [ ]
      REAL WFCIROT(NL,NT)       !<WATROF parameter used when running MESH code [ ]
      REAL ALGWVROT(NL,NT)      !
      REAL ALGWNROT(NL,NT)      !
      REAL ALGDVROT(NL,NT)      !
      REAL ALGDNROT(NL,NT)      !
      REAL ALGWROT(NL,NT)       !<Reference albedo for saturated soil [ ]
      REAL ALGDROT(NL,NT)       !<Reference albedo for dry soil [ ]
      REAL ASVDROT(NL,NT)       !<Optional user-specified value of snow 
                                !!visible albedo to override CLASS-
                                !!calculated value [ ]
      REAL ASIDROT(NL,NT)       !<Optional user-specified value of snow 
                                !!near-infrared albedo to override CLASS-
                                !!calculated value [ ]
      REAL AGVDROT(NL,NT)       !<Optional user-specified value of ground 
                                !!visible albedo to override CLASS-
                                !!calculated value [ ]
      REAL AGIDROT(NL,NT)       !<Optional user-specified value of ground 
                                !!near-infrared albedo to override CLASS-
                                !!calculated value [ ]
      REAL ZSNLROT(NL,NT)       !<Limiting snow depth below which 
                                !!coverage is < 100% [m]
      REAL ZPLGROT(NL,NT)       !<Maximum water ponding depth for snow-
                                !!free subareas (user-specified when 
                                !!running MESH code) [m]
      REAL ZPLSROT(NL,NT)       !<Maximum water ponding depth for snow-
                                !!covered subareas (user-specified when 
                                !!running MESH code) [m]
C

      REAL    THPGAT (ILG,IG),   THRGAT (ILG,IG),   THMGAT (ILG,IG),
     1        BIGAT  (ILG,IG),   PSISGAT(ILG,IG),   GRKSGAT(ILG,IG),   
     2        THRAGAT(ILG,IG),   HCPSGAT(ILG,IG),   DDGAT(ILG),
     3        TCSGAT (ILG,IG),   THFCGAT(ILG,IG),   THLWGAT(ILG,IG),   
     4        PSIWGAT(ILG,IG),   DLZWGAT(ILG,IG),   ZBTWGAT(ILG,IG), 
     5        DRNGAT (ILG),      XSLPGAT(ILG),      GRKFGAT(ILG),
     6        WFSFGAT(ILG),      WFCIGAT(ILG),      ALGWVGAT(ILG),      
     7        ALGWNGAT(ILG),     ALGDVGAT(ILG),     ALGDNGAT(ILG),      
     +        ALGWGAT(ILG),      ALGDGAT(ILG),      
     8        ASVDGAT(ILG),      ASIDGAT(ILG),                          
     9        AGVDGAT(ILG),      AGIDGAT(ILG),      ZSNLGAT(ILG),       
     A        ZPLGGAT(ILG),      ZPLSGAT(ILG)                           
C
      INTEGER ISNDROT(NL,NT,IG), ISNDGAT(ILG,IG)!<Sand content flag
      INTEGER IGDRROT(NL,NT),    IGDRGAT(ILG)   !<Index of soil layer in 
                                                !!which bedrock is 
                                                !!encountered
C
C     * ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES.
C
      REAL ZRFMROW( NL) !<Reference height associated with forcing wind 
                        !!speed [m]
      REAL ZRFHROW( NL) !<Reference height associated with forcing air 
                        !!temperature and humidity [m]
      REAL ZDMROW ( NL) !<User-specified height associated with diagnosed 
                        !!anemometer-level wind speed [m]
      REAL ZDHROW ( NL) !<User-specified height associated with diagnosed 
                        !!screen-level variables [m]
      REAL FSVHROW( NL) !<Visible radiation incident on horizontal 
                        !!surface \f$[W m^{-2}]\f$
      REAL FSIHROW( NL) !<Near-infrared radiation incident on horizontal 
                        !!surface \f$[W m^{-2}]\f$
      REAL CSZROW ( NL) !<Cosine of solar zenith angle [ ]
      REAL FSGROL ( NL) !
      REAL FLGROL ( NL) !
      REAL FDLROL ( NL) !<Downwelling longwave radiation at bottom of 
                        !!atmosphere \f$[W m^{-2}]\f$

      REAL ULROW  ( NL) !<Zonal component of wind speed \f$[m s^{-1}]\f$
      REAL VLROW  ( NL) !<Meridional component of wind speed \f$[m s^{-1}]\f$
      REAL TAROW  ( NL) !<Air temperature at reference height [K]
      REAL QAROW  ( NL) !<Specific humidity at reference height \f$[kg kg^{-1}]\f$
      REAL PRESROW( NL) !<Surface air pressure [Pa]
      REAL PREROW ( NL) !<Surface precipitation rate \f$[kg m^{-2} s^{-1}]\f$
      REAL PADRROW( NL) !<Partial pressure of dry air [Pa]
      REAL VPDROW ( NL) !<Vapour pressure deficit [mb]
      REAL TADPROW( NL) !<Dew point temperature of air [K]
      REAL RHOAROW( NL) !<Density of air \f$[kg m^{-3}]\f$
      REAL ZBLDROW( NL) !<Atmospheric blending height for surface 
                        !!roughness length averaging [m]
      REAL Z0ORROW( NL) !<Orographic roughness length [m]
      REAL RPCPROW( NL) !<Rainfall rate over modelled area \f$[m s^{-1}]\f$
      REAL TRPCROW( NL) !<Rainfall temperature [K]
      REAL SPCPROW( NL) !<Snowfall rate over modelled area \f$[m s^{-1}]\f$
      REAL TSPCROW( NL) !<Snowfall temperature [K]
      REAL RHSIROW( NL) !<Density of fresh snow \f$[kg m^{-3}]\f$
      REAL FCLOROW( NL) !<Fractional cloud cover [ ]
      REAL DLONROW( NL) !<Longitude of grid cell (east of Greenwich) [degrees]
      REAL GGEOROW( NL) !<Geothermal heat flux at bottom of soil profile 
                        !!\f$[W m^{-2}]\f$
      REAL GUSTROL (NL) !
      REAL RADJ   ( NL) !<Latitude of grid cell (positive north of equator) [rad]
      REAL VMODL  ( NL) !<Wind speed at reference height \f$[m s^{-1}]\f$
      REAL DEPBROW (NL) !

      REAL, DIMENSION(NL,NBS) ::  FSDBROL
      REAL, DIMENSION(NL,NBS) ::  FSFBROL
      REAL, DIMENSION(NL,NBS) ::  FSSBROL

C
      REAL  ZRFMGAT(ILG), ZRFHGAT(ILG), ZDMGAT (ILG), ZDHGAT (ILG),
     1      FSVHGAT(ILG), FSIHGAT(ILG), CSZGAT (ILG),                   
     2      FSGGAT (ILG), FLGGAT (ILG), FDLGAT (ILG),                   
     3      ULGAT  (ILG), VLGAT  (ILG), TAGAT  (ILG), QAGAT  (ILG),     
     4      PRESGAT(ILG), PREGAT (ILG), PADRGAT(ILG), VPDGAT (ILG),     
     5      TADPGAT(ILG), RHOAGAT(ILG), ZBLDGAT(ILG), Z0ORGAT(ILG),     
     6      RPCPGAT(ILG), TRPCGAT(ILG), SPCPGAT(ILG), TSPCGAT(ILG),     
     7      RHSIGAT(ILG), FCLOGAT(ILG), DLONGAT(ILG), GGEOGAT(ILG),     
     8      GUSTGAT(ILG), RADJGAT(ILG), VMODGAT(ILG), DEPBGAT(ILG) 
     
      REAL, DIMENSION(ILG,NBS) :: FSDBGAT, FSFBGAT, FSSBGAT             
C----------------------------------------------------------------------
      !
      !The prognostic, background and input variables are gathered into 
      !long arrays (collapsing the latitude and mosaic dimensions into 
      !one, but retaining the soil level and canopy category dimensions) 
      !using the pointer vectors generated in GATPREP.
      !

	
      DO 100 K=1,NML
          TPNDGAT(K)=TPNDROT(ILMOS(K),JLMOS(K))  
          ZPNDGAT(K)=ZPNDROT(ILMOS(K),JLMOS(K))  
          TBASGAT(K)=TBASROT(ILMOS(K),JLMOS(K))  
          ALBSGAT(K)=ALBSROT(ILMOS(K),JLMOS(K))  
          TSNOGAT(K)=TSNOROT(ILMOS(K),JLMOS(K))  
          RHOSGAT(K)=RHOSROT(ILMOS(K),JLMOS(K))  
          SNOGAT (K)=SNOROT (ILMOS(K),JLMOS(K))  
          REFGAT (K)=REFROT (ILMOS(K),JLMOS(K))                         
          BCSNGAT(K)=BCSNROT(ILMOS(K),JLMOS(K))                         
          WSNOGAT(K)=WSNOROT(ILMOS(K),JLMOS(K))  
          TCANGAT(K)=TCANROT(ILMOS(K),JLMOS(K))  
          RCANGAT(K)=RCANROT(ILMOS(K),JLMOS(K))  
          SCANGAT(K)=SCANROT(ILMOS(K),JLMOS(K))  
          GROGAT (K)=GROROT (ILMOS(K),JLMOS(K))  
          CMAIGAT(K)=CMAIROT(ILMOS(K),JLMOS(K))  
          DRNGAT (K)=DRNROT (ILMOS(K),JLMOS(K))
          DDGAT(K)=DDROT(ILMOS(K),JLMOS(K))
          XSLPGAT(K)=XSLPROT(ILMOS(K),JLMOS(K))  
          GRKFGAT(K)=GRKFROT(ILMOS(K),JLMOS(K))  
          WFSFGAT(K)=WFSFROT(ILMOS(K),JLMOS(K))  
          WFCIGAT(K)=WFCIROT(ILMOS(K),JLMOS(K))  
          ALGWVGAT(K)=ALGWVROT(ILMOS(K),JLMOS(K))                       
          ALGWNGAT(K)=ALGWNROT(ILMOS(K),JLMOS(K))                       
          ALGDVGAT(K)=ALGDVROT(ILMOS(K),JLMOS(K))                       
          ALGDNGAT(K)=ALGDNROT(ILMOS(K),JLMOS(K))                       
          ALGWGAT(K)=ALGWROT(ILMOS(K),JLMOS(K))  
          ALGDGAT(K)=ALGDROT(ILMOS(K),JLMOS(K))  
c         ASVDGAT(K)=ASVDROT(ILMOS(K),JLMOS(K))  
c         ASIDGAT(K)=ASIDROT(ILMOS(K),JLMOS(K))  
c         AGVDGAT(K)=AGVDROT(ILMOS(K),JLMOS(K))  
c         AGIDGAT(K)=AGIDROT(ILMOS(K),JLMOS(K))  
          ZSNLGAT(K)=ZSNLROT(ILMOS(K),JLMOS(K))  
!===============User Specified Interflow Parameters, July/2017==========
          ZPLGGAT(K)=ZPLGROT(ILMOS(K),JLMOS(K))  
          ZPLSGAT(K)=ZPLSROT(ILMOS(K),JLMOS(K))
!DAN    Restored user-specified values
!          ZPLGGAT(K) = 0.05
!          ZPLSGAT(K) = 0.05
!========End of User specification================================          
          TACGAT (K)=TACROT (ILMOS(K),JLMOS(K))  
          QACGAT (K)=QACROT (ILMOS(K),JLMOS(K))  
          IGDRGAT(K)=IGDRROT(ILMOS(K),JLMOS(K))
          ZBLDGAT(K)=ZBLDROW(ILMOS(K))
          Z0ORGAT(K)=Z0ORROW(ILMOS(K))
          ZRFMGAT(K)=ZRFMROW(ILMOS(K))
          ZRFHGAT(K)=ZRFHROW(ILMOS(K))
          ZDMGAT (K)=ZDMROW(ILMOS(K))
          ZDHGAT (K)=ZDHROW(ILMOS(K))
          FSVHGAT(K)=FSVHROW(ILMOS(K))
          FSIHGAT(K)=FSIHROW(ILMOS(K))
          CSZGAT (K)=CSZROW (ILMOS(K))
          FSGGAT (K)=FSGROL (ILMOS(K))                                  
          FLGGAT (K)=FLGROL (ILMOS(K))                                  
          FDLGAT (K)=FDLROL (ILMOS(K))                                  
          ULGAT  (K)=ULROW  (ILMOS(K))
          VLGAT  (K)=VLROW  (ILMOS(K))
          TAGAT  (K)=TAROW  (ILMOS(K))
          QAGAT  (K)=QAROW  (ILMOS(K))
          PRESGAT(K)=PRESROW(ILMOS(K))
          PREGAT (K)=PREROW (ILMOS(K))
          PADRGAT(K)=PADRROW(ILMOS(K))
          VPDGAT (K)=VPDROW (ILMOS(K))
          TADPGAT(K)=TADPROW(ILMOS(K))
          RHOAGAT(K)=RHOAROW(ILMOS(K))
          RPCPGAT(K)=RPCPROW(ILMOS(K))
          TRPCGAT(K)=TRPCROW(ILMOS(K))
          SPCPGAT(K)=SPCPROW(ILMOS(K))
          TSPCGAT(K)=TSPCROW(ILMOS(K))
          RHSIGAT(K)=RHSIROW(ILMOS(K))
          FCLOGAT(K)=FCLOROW(ILMOS(K))
          DLONGAT(K)=DLONROW(ILMOS(K))
          GGEOGAT(K)=GGEOROW(ILMOS(K))
          GUSTGAT(K)=GUSTROL(ILMOS(K))                                  
          RADJGAT(K)=RADJ   (ILMOS(K))
          VMODGAT(K)=VMODL  (ILMOS(K))
          DEPBGAT(K)=DEPBROW(ILMOS(K))                                  
  100 CONTINUE
C

      DO 250 L=1,IG
      DO 200 K=1,NML
          TBARGAT(K,L)=TBARROT(ILMOS(K),JLMOS(K),L)
          THLQGAT(K,L)=THLQROT(ILMOS(K),JLMOS(K),L)
          THICGAT(K,L)=THICROT(ILMOS(K),JLMOS(K),L)
          THPGAT (K,L)=THPROT (ILMOS(K),JLMOS(K),L)
          THRGAT (K,L)=THRROT (ILMOS(K),JLMOS(K),L)
          THMGAT (K,L)=THMROT (ILMOS(K),JLMOS(K),L)
          BIGAT  (K,L)=BIROT  (ILMOS(K),JLMOS(K),L)
          PSISGAT(K,L)=PSISROT(ILMOS(K),JLMOS(K),L)
          GRKSGAT(K,L)=GRKSROT(ILMOS(K),JLMOS(K),L)
C===================GRKSGAT should be KSAT, which is being recorded==========================
C====================from the .INI file as WFCINT, see if it makes a differance=====
C============================Stefan Sauer, July 2017=================

C============================================================
          THRAGAT(K,L)=THRAROT(ILMOS(K),JLMOS(K),L)
          HCPSGAT(K,L)=HCPSROT(ILMOS(K),JLMOS(K),L)
          TCSGAT (K,L)=TCSROT (ILMOS(K),JLMOS(K),L)
          THFCGAT(K,L)=THFCROT(ILMOS(K),JLMOS(K),L)
          THLWGAT(K,L)=THLWROT(ILMOS(K),JLMOS(K),L)          
          PSIWGAT(K,L)=PSIWROT(ILMOS(K),JLMOS(K),L)
          DLZWGAT(K,L)=DLZWROT(ILMOS(K),JLMOS(K),L)
          ZBTWGAT(K,L)=ZBTWROT(ILMOS(K),JLMOS(K),L)
          ISNDGAT(K,L)=ISNDROT(ILMOS(K),JLMOS(K),L)
  200 CONTINUE
  250 CONTINUE
C
      DO 300 L=1,ICP1
      DO 300 K=1,NML
          FCANGAT(K,L)=FCANROT(ILMOS(K),JLMOS(K),L)
          LNZ0GAT(K,L)=LNZ0ROT(ILMOS(K),JLMOS(K),L)
          ALVCGAT(K,L)=ALVCROT(ILMOS(K),JLMOS(K),L)
          ALICGAT(K,L)=ALICROT(ILMOS(K),JLMOS(K),L)
  300 CONTINUE
C
      DO 400 L=1,IC
      DO 400 K=1,NML
          PAMXGAT(K,L)=PAMXROT(ILMOS(K),JLMOS(K),L)
          PAMNGAT(K,L)=PAMNROT(ILMOS(K),JLMOS(K),L)
          CMASGAT(K,L)=CMASROT(ILMOS(K),JLMOS(K),L)
          ROOTGAT(K,L)=ROOTROT(ILMOS(K),JLMOS(K),L)
          RSMNGAT(K,L)=RSMNROT(ILMOS(K),JLMOS(K),L)
          QA50GAT(K,L)=QA50ROT(ILMOS(K),JLMOS(K),L)
          VPDAGAT(K,L)=VPDAROT(ILMOS(K),JLMOS(K),L)
          VPDBGAT(K,L)=VPDBROT(ILMOS(K),JLMOS(K),L)
          PSGAGAT(K,L)=PSGAROT(ILMOS(K),JLMOS(K),L)
          PSGBGAT(K,L)=PSGBROT(ILMOS(K),JLMOS(K),L)
c         PAIDGAT(K,L)=PAIDROT(ILMOS(K),JLMOS(K),L)
c         HGTDGAT(K,L)=HGTDROT(ILMOS(K),JLMOS(K),L)
c         ACVDGAT(K,L)=ACVDROT(ILMOS(K),JLMOS(K),L)
c         ACIDGAT(K,L)=ACIDROT(ILMOS(K),JLMOS(K),L)
          TSFSGAT(K,L)=TSFSROT(ILMOS(K),JLMOS(K),L)
400   CONTINUE

C                                                                       
      DO L = 1, NBS                                                     
         DO K = 1, NML                                                  
            FSDBGAT(K,L) = FSDBROL(ILMOS(K),L)                          
            FSFBGAT(K,L) = FSFBROL(ILMOS(K),L)                          
            FSSBGAT(K,L) = FSSBROL(ILMOS(K),L)                          
         END DO ! K                                                     
      END DO ! L                                                        
      RETURN
      END
