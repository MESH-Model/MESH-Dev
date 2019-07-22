!>\file
!! Principle driver program to run CLASS in stand-alone mode using specified boundary
!! conditions and atmospheric forcing, coupled to CTEM.
!!
!! # Overview
!!
!! This driver program initializes the run, reads in CLASS input files, manages the run
!! and the coupling between CLASS and CTEM, writes the CLASS sub-monthly outputs, and
!! closes the run.

      MODULE RUNCLASS36CTEM
C
C     REVISION HISTORY:

C     * JUL 2019 : Code added Nitrogen components from Huang et al.(2011) and Ben Windeler
C       Stefan Sauer
C
C     * JUL 2018 : Code here editied heavily by Daniel Princz and Stefan Sauer for Module use in MESH
C
C
C     * Dec 1 2016 : Code can now handle leap years in the input MET
C       Jean-Sebastien Landry
C
C     * Aug 31 2016 : Added proper calculation of ALTOT as provided by Diana.
C       Joe Melton
C
C     * Mar 9  2016 : For consistency I have changed all inputs (except MET) to be adopted into the row/gat
C     * Joe Melton    framework. This means that a per gridcell value is then also assigned per tile. This
C                     just makes it easier to deal with in the model code since it fits into the loops like the vars.
C
C     * Feb 10 2016 : Trimmed CTEM secondary vars from driver. They are not used, so can find in io_driver
C     * Joe Melton
C
C     * JAN 6 2015
C     * Joe Melton : Added the soil methane sink subroutine
C
C     * JUL 2 2015
C     * JOE MELTON : Took many calculations out of this driver and into subroutines. Introduced
C                    modular structure and made CTEM vars into pointers. Harmonized CLASS v. 3.6.1
C                    code with CTEM code and into this driver to produce CLASS v. 3.6.2.
C
C     * JAN 14 2014
C     * JOE MELTON : Harmonized the field capacity and wilting point calculations between CLASS and CTEM.
C                    took the code out of runclassctem and it is now fully done within CLASSB. Harmonized names too.
C
C     * JUN 2014
C     * RUDRA SHRESTHA : ADD IN WETLAND CODE
C
C     * JUL 2013
C     * JOE MELTON : REMOVED CTEM1 AND CTEM2 OPTIONS, REPLACED WITH CTEM_ON. INTRODUCE
C                                  MODULES. RESTRUCTURE OUTPUTS AND CTEM VARIABLE DECLARATIONS
C                                  OUTPUTS ARE NOW THE SAME FOR BOTH MOSAIC AND COMPOSITE MODES
C
C     * DEC 2012
C     * JOE MELTON : REMOVED GOTO STATEMENTS, CLEANED UP AND FIXED INCONSISTENCIES
C                                  IN HOW INPUT DATA READ IN. ALSO MADE LUC WORK FOR
C                                  BOTH COMPOSITE AND MOSAIC APPROACHES.
C
C     * OCT 2012
C     * YIRAN PENG AND JOE MELTON: BRING IN COMPETITION TO 3.6 AND MAKE IT
C                                  SO THE MODEL CAN START FROM BARE GROUND
C                                  OR FROM THE INI AND CTM FILES INPUTS
C
C     * SEP 2012
C     * JOE MELTON: COUPLED CLASS3.6 AND CTEM
C
C     * NOV 2011
C     * YIRAN PENG AND VIVEK ARORA: COUPLED CLASS3.5 AND CTEM
C
C     * SEPT 8, 2009
C     * RONG LI AND VIVEK ARORA: COUPLED CLASS3.4 AND CTEM
C
C=======================================================================
!>
!!------------------------------------------------------------------
!! ## Dimension statements.

!!     ### first set of definitions:
!!     background variables, and prognostic and diagnostic
!!     variables normally provided by and/or used by the gcm.
!!      the suffix "rot" refers to variables existing on the
!!      mosaic grid on the current latitude circle.  the suffix
!!      "gat" refers to the same variables after they have undergone
!!      a "gather" operation in which the two mosaic dimensions
!!      are collapsed into one.  the suffix "row" refers both to
!!      grid-constant input variables. and to grid-averaged
!!      diagnostic variables.
!!
!!      the first dimension element of the "rot" variables
!!      refers to the number of grid cells on the current
!!      latitude circle.  in this stand-alone version, this
!!      number is arbitrarily set to three, to allow up to three
!!      simultaneous tests to be run.  the second dimension
!!      element of the "rot" variables refers to the maximum
!!      number of tiles in the mosaic.  in this stand-alone
!!      version, this number is set to eight.  the first
!!      dimension element in the "gat" variables is given by
!!      the product of the first two dimension elements in the
!!      "rot" variables.
!!
!!     The majority of CTEM parameters are stored in ctem_params.f90.
!!     Also the CTEM variables are stored in modules that we point to
!!     in this driver. We access the variables and parameters
!!     through use statements for modules:




!Some constants here were cut out because they already exist within MESH (NTYPE,IGND)

      use ctem_params,        only : initpftpars,nmon,ILG, nlat, nmos,
     1                               ican,icp1, icc, iccp1, IGND,
     2                               monthend, mmday,modelpft, l2max,
     3                               deltat, abszero, monthdays,seed,
     4                               crop, NBS, lat, edgelat,earthrad,
     5                               lon

      use landuse_change,     only : initialize_luc, readin_luc

      use ctem_statevars,     only : vrot,vgat,c_switch,initrowvars,
     1                               class_out,resetclassmon,
     2                               resetclassyr,
     3                               resetmonthend,resetyearend,
     4                               resetclassaccum,ctem_grd,
     5                               ctem_tile,resetgridavg,
     6                               finddaylength,
     7                               veg_rot_allocate,
     8                               veg_gat_allocate,
     9                               class_moyr_output_allocate,
     A                               ctem_gridavg_allocate,
     B                               ctem_tile_level_allocate,
     C                               ctem_monthly_allocate,
     D                               ctem_gridavg_monthly_allocate,
     E                               ctem_tileavg_monthly_allocate,
     F                               ctem_annual_allocate,
     G                               ctem_gridavg_annual_allocate,
     H                               ctem_tileavg_annual_allocate

      use io_driver,          only : read_from_ctm, create_outfiles,
     1                               write_ctm_rs, class_monthly_aw,
     2                               ctem_annual_aw,ctem_monthly_aw,
     3                               close_outfiles,ctem_daily_aw,
     4                               class_annual_aw


      implicit none


!This is MESH-SPECIFIC and is used to get the output from a single grid cell
 !> GRID OUTPUT POINTS
        !* N_OUT: GRID SQUARE TO OUTPUT
        !* II_OUT: GRU TO OUTPUT
        !* WF_NUM_POINTS: NUMBER OF GRID OUTPUTS
        !Set dimension to 10 as this is the maximum size
        integer :: N_OUT(10) 
        integer :: II_OUT(10) 
        integer :: K_OUT(10) 
        integer :: WF_NUM_POINTS
        integer :: CONFLAGS

        !* DIR_OUT: OUTPUT DIRECTORY (10 CHARACTER STRING)
        character*15 :: DIR_OUT(10)



C
C     * INTEGER CONSTANTS.
C
      INTEGER IDISP  !<Flag governing treatment of vegetation displacement height
      INTEGER IZREF  !<Flag governing treatment of surface roughness length
      INTEGER ISLFD  !<Flag governing options for surface stability functions and diagnostic calculations
      INTEGER IPCP   !<Flag selecting algorithm for dividing precipitation between rainfall and snowfall
      integer, dimension(:), allocatable :: IWF !<Flag governing lateral soil water flow calculations
      INTEGER IPAI   !<Flag to enable use of user-specified plant area index
      INTEGER IHGT   !<Flag to enable use of user-specified vegetation height
      INTEGER IALC   !<Flag to enable use of user-specified canopy albedo
      INTEGER IALS   !<Flag to enable use of user-specified snow albedo
      INTEGER IALG   !<Flag to enable use of user-specified ground albedo
      INTEGER N      !<
      INTEGER ITG    !<Flag to select iteration scheme for ground or snow surface
      INTEGER ITC    !<Flag to select iteration scheme for canopy temperature
      INTEGER ITCG   !<Flag to select iteration scheme for surface under canopy
      INTEGER isnoalb!<
      INTEGER igralb !<


      INTEGER NLTEST  !<Number of grid cells being modelled for this run
      INTEGER NMTEST  !<Number of mosaic tiles per grid cell being modelled for this run
      INTEGER NCOUNT  !<Counter for daily averaging
      INTEGER NDAY    !<
      INTEGER IMONTH  !<
      INTEGER NDMONTH !<
      INTEGER NT      !<
      INTEGER IHOUR   !<Hour of day
      INTEGER IMIN    !<Minutes elapsed in current hour
      INTEGER IDAY    !<Julian day of the year
      INTEGER IYEAR   !<Year of run
      INTEGER NML     !<Counter representing number of mosaic tiles on modelled domain that are land
      INTEGER NMW     !<Counter representing number of mosaic tiles on modelled domain that are lakes
      INTEGER JLAT    !<Integer index corresponding to latitude of grid cell
      INTEGER NLANDCS !<Number of modelled areas that contain subareas of canopy over snow
      INTEGER NLANDGS !<Number of modelled areas that contain subareas of snow over bare ground
      INTEGER NLANDC  !<Number of modelled areas that contain subareas of canopy over bare ground
      INTEGER NLANDG  !<Number of modelled areas that contain subareas of bare ground
      INTEGER NLANDI  !<Number of modelled areas that are ice sheets
      INTEGER I,J,K,L,M
      INTEGER NTLD    !<

      INTEGER JK      !<Nitrogen ctem PFT select
C
      INTEGER K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11
      INTEGER ITA        !<
      INTEGER ITCAN      !<
      INTEGER ITD        !<
      INTEGER ITAC       !<
      INTEGER ITS        !<
      INTEGER ITSCR      !<
      INTEGER ITD2       !<
      INTEGER ITD3       !<
      INTEGER ITD4       !<
      INTEGER ISTEPS     !<
      INTEGER NFS        !<
      INTEGER NDRY       !<
      INTEGER NAL        !<
      INTEGER NFT        !<
      REAL TAHIST(200)   !<
      REAL TCHIST(200)   !<
      REAL TACHIST(200)  !<
      REAL TDHIST(200)   !<
      REAL TSHIST(200)   !<
      REAL TSCRHIST(200) !<
      REAL TD2HIST(200)  !<
      REAL TD3HIST(200)  !<
      REAL TD4HIST(200)  !<
      real, dimension(:), allocatable :: PAICAN   !<

      INTEGER*4 TODAY(3), NOW(3)

!DAN    changed these to allocatable (with no explicit dimensions)
!DAN    allocated after indices are transferred from MESH in init()
      real, dimension(:), allocatable :: ALBSGAT        !<Snow albedo [ ]
      real, dimension(:,:), allocatable :: ALBSROT      !<
      real, dimension(:), allocatable :: CMAIGAT        !<Aggregated mass of vegetation canopy \f$[kg m^{-2} ]\f$
      real, dimension(:,:), allocatable :: CMAIROT      !<
      real, dimension(:), allocatable :: GROGAT         !<Vegetation growth index [ ]
      real, dimension(:,:), allocatable :: GROROT       !<
      real, dimension(:), allocatable :: QACGAT         !<Specific humidity of air within vegetation canopy space \f$[kg kg^{-1} ]\f$
      real, dimension(:,:), allocatable :: QACROT       !<
      real, dimension(:), allocatable :: RCANGAT        !<Intercepted liquid water stored on canopy \f$[kg m^{-2} ]\f$
      real, dimension(:,:), allocatable :: RCANROT      !<
      real, dimension(:), allocatable :: RHOSGAT        !<Density of snow \f$[kg m^{-3} ]\f$
      real, dimension(:,:), allocatable :: RHOSROT      !<
      real, dimension(:), allocatable :: SCANGAT        !<Intercepted frozen water stored on canopy \f$[kg m^{-2} ]\f$
      real, dimension(:,:), allocatable :: SCANROT      !<
      real, dimension(:), allocatable :: SNOGAT         !<Mass of snow pack [kg m^{-2} ]\f$
      real, dimension(:,:), allocatable :: SNOROT       !<
      real, dimension(:), allocatable :: TACGAT         !<Temperature of air within vegetation canopy [K]
      real, dimension(:,:), allocatable :: TACROT       !<
      real, dimension(:,:), allocatable :: TBARGAT      !<Temperature of soil layers [K]
      real, dimension(:,:,:), allocatable :: TBARROT    !<
      real, dimension(:), allocatable :: TBASGAT        !<Temperature of bedrock in third soil layer [K]
      real, dimension(:,:), allocatable :: TBASROT      !<
      real, dimension(:), allocatable :: TCANGAT        !<Vegetation canopy temperature [K]
      real, dimension(:,:), allocatable :: TCANROT      !<
      real, dimension(:,:), allocatable :: THICGAT      !<Volumetric frozen water content of soil layers \f$[m^3 m^{-3} ]\f$
      real, dimension(:,:,:), allocatable :: THICROT    !<
      real, dimension(:,:), allocatable :: THLQGAT      !<Volumetric liquid water content of soil layers \f$[m^3 m^{-3} ]\f$
      real, dimension(:,:,:), allocatable :: THLQROT    !<
      real, dimension(:), allocatable :: TPNDGAT        !<Temperature of ponded water [K]
      real, dimension(:,:), allocatable :: TPNDROT      !<
      real, dimension(:,:), allocatable :: TSFSGAT      !<Ground surface temperature over subarea [K]
      real, dimension(:,:,:), allocatable :: TSFSROT    !<
      real, dimension(:), allocatable :: TSNOGAT        !<Snowpack temperature [K]
      real, dimension(:,:), allocatable :: TSNOROT      !<
      real, dimension(:), allocatable :: WSNOGAT        !<Liquid water content of snow pack \f$[kg m^{-2} ]\f$
      real, dimension(:,:), allocatable :: WSNOROT      !<
      real, dimension(:), allocatable :: ZPNDGAT        !<Depth of ponded water on surface [m]
      real, dimension(:,:), allocatable :: ZPNDROT      !<



C
C     * LAND SURFACE PROGNOSTIC VARIABLES.
C
C
      real, dimension(:,:), allocatable :: REFROT   !<
      real, dimension(:,:), allocatable :: BCSNROT  !<
      real, dimension(:), allocatable :: REFGAT     !<
      real, dimension(:), allocatable :: BCSNGAT    !<
C
C     * GATHER-SCATTER INDEX ARRAYS.
C
      integer, dimension(:), allocatable :: ILMOS  !<Index of grid cell corresponding to current element of gathered vector of land surface variables [ ]
      integer, dimension(:), allocatable :: JLMOS  !<Index of mosaic tile corresponding to current element of gathered vector of land surface variables [ ]
      integer, dimension(:), allocatable :: IWMOS  !<Index of grid cell corresponding to current element of gathered vector of inland water body variables [ ]
      integer, dimension(:), allocatable :: JWMOS  !<Index of mosaic tile corresponding to current element of gathered vector of inland water body variables [ ]
C
C     * CANOPY AND SOIL INFORMATION ARRAYS.
C     * (THE LAST DIMENSION OF MOST OF THESE ARRAYS IS GIVEN BY
C     * THE NUMBER OF SOIL LAYERS (IGND), THE NUMBER OF BROAD
C     * VEGETATION CATEGORIES (ICAN), OR ICAN+1.
C


      !Additional variables for IWF and WATROF
      real, dimension(:), allocatable :: MANNING_N  !<Manning's Roughness Coefficient, Calculated as an input Parameter
      real, dimension(:), allocatable :: DD         !<Drainage Density, [M^2/M^3]
      real, dimension(:,:), allocatable :: BULK_FC  !<

      real, dimension(:,:), allocatable :: ACIDGAT      !<Optional user-specified value of canopy near-infrared albedo to override CLASS-calculated value [ ]
      real, dimension(:,:,:), allocatable :: ACIDROT    !<
      real, dimension(:,:), allocatable :: ACVDGAT      !<Optional user-specified value of canopy visible albedo to override CLASS-calculated value [ ]
      real, dimension(:,:,:), allocatable :: ACVDROT    !<
      real, dimension(:), allocatable :: AGIDGAT        !<Optional user-specified value of ground near-infrared albedo to override CLASS-calculated value [ ]
      real, dimension(:,:), allocatable :: AGIDROT      !<
      real, dimension(:), allocatable :: AGVDGAT        !<Optional user-specified value of ground visible albedo to override CLASS-calculated value [ ]
      real, dimension(:,:), allocatable :: AGVDROT      !<
      real, dimension(:), allocatable :: ALGDGAT        !<Reference albedo for dry soil [ ]
      real, dimension(:,:), allocatable :: ALGDROT      !<
      real, dimension(:), allocatable :: ALGWGAT        !<Reference albedo for saturated soil [ ]
      real, dimension(:,:), allocatable :: ALGWROT      !<
      real, dimension(:,:), allocatable :: ALICGAT      !<Background average near-infrared albedo of vegetation category [ ]
      real, dimension(:,:,:), allocatable :: ALICROT    !<
      real, dimension(:,:), allocatable :: ALVCGAT      !<Background average visible albedo of vegetation category [ ]
      real, dimension(:,:,:), allocatable :: ALVCROT    !<
      real, dimension(:), allocatable :: ASIDGAT        !<Optional user-specified value of snow near-infrared albedo to override CLASS-calculated value [ ]
      real, dimension(:,:), allocatable :: ASIDROT      !<
      real, dimension(:), allocatable :: ASVDGAT        !<Optional user-specified value of snow visible albedo to override CLASS-calculated value [ ]
      real, dimension(:,:), allocatable :: ASVDROT      !<
      real, dimension(:,:), allocatable :: BIGAT        !<Clapp and Hornberger empirical “b” parameter [ ]
      real, dimension(:,:,:), allocatable :: BIROT      !<
      real, dimension(:,:,:), allocatable :: CLAYROT    !<Percentage clay content of soil
      real, dimension(:,:), allocatable :: CMASGAT      !<Maximum canopy mass for vegetation category \f$[kg m^{-2} ]\f$
      real, dimension(:,:,:), allocatable :: CMASROT    !<
      real, dimension(:), allocatable :: DDGAT          !<Drainage Density [km^{-1} ]
      real, dimension(:,:), allocatable :: DDROT        !<
      real, dimension(:,:), allocatable :: DLZWGAT      !<Permeable thickness of soil layer [m]
      real, dimension(:,:,:), allocatable :: DLZWROT    !<
      real, dimension(:), allocatable :: DRNGAT         !<Drainage index at bottom of soil profile [ ]
      real, dimension(:,:), allocatable :: DRNROT       !<
      real, dimension(:,:), allocatable :: FAREROT      !<Fractional coverage of mosaic tile on modelled area
      real, dimension(:,:), allocatable :: FCANGAT      !<Maximum fractional coverage of modelled area by vegetation category [ ]
      real, dimension(:,:,:), allocatable :: FCANROT    !<
      real, dimension(:), allocatable :: GRKFGAT        !<The Fractional change in horizontal conductivity in a depth change of h0, Calculated as a Parameter, Used in WATROF [ ]
      real, dimension(:,:), allocatable :: GRKFROT      !<
      real, dimension(:,:), allocatable :: GRKSGAT      !<Saturated hydraulic conductivity of soil layers \f$[m s^{-1} ]\f$ Note that this should be registered as WFCINT
      real, dimension(:,:,:), allocatable :: GRKSROT    !<
      real, dimension(:,:), allocatable :: HCPSGAT      !<Volumetric heat capacity of soil particles \f$[J m^{-3} ]\f$
      real, dimension(:,:,:), allocatable :: HCPSROT    !<
      real, dimension(:,:), allocatable :: HGTDGAT      !<Optional user-specified values of height of vegetation categories to override CLASS-calculated values [m]
      real, dimension(:,:,:), allocatable :: HGTDROT    !<
      integer, dimension(:), allocatable :: IGDRGAT     !<Index of soil layer in which bedrock is encountered
      integer, dimension(:,:), allocatable :: IGDRROT   !<
      integer, dimension(:,:), allocatable :: ISNDGAT   !<Integer identifier associated with sand content
      integer, dimension(:,:,:), allocatable :: ISNDROT !<
      real, dimension(:,:), allocatable :: LNZ0GAT      !<Natural logarithm of maximum roughness length of vegetation category [ ]
      real, dimension(:,:,:), allocatable :: LNZ0ROT    !<
      integer, dimension(:,:), allocatable :: MIDROT    !<Mosaic tile type identifier (1 for land surface, 0 for inland lake)
      real, dimension(:,:,:), allocatable :: ORGMROT    !<Percentage organic matter content of soil
      real, dimension(:,:), allocatable :: PAIDGAT      !<Optional user-specified value of plant area indices of vegetation categories to override CLASS-calculated values [ ]
      real, dimension(:,:,:), allocatable :: PAIDROT    !<
      real, dimension(:,:), allocatable :: PAMNGAT      !<Minimum plant area index of vegetation category [ ]
      real, dimension(:,:,:), allocatable :: PAMNROT    !<
      real, dimension(:,:), allocatable :: PAMXGAT      !<Minimum plant area index of vegetation category [ ]
      real, dimension(:,:,:), allocatable :: PAMXROT    !<
      real, dimension(:,:), allocatable :: PSGAGAT      !<Soil moisture suction coefficient for vegetation category (used in stomatal resistance calculation) [ ]
      real, dimension(:,:,:), allocatable :: PSGAROT    !<
      real, dimension(:,:), allocatable :: PSGBGAT      !<Soil moisture suction coefficient for vegetation category (used in stomatal resistance calculation) [ ]
      real, dimension(:,:,:), allocatable :: PSGBROT    !<
      real, dimension(:,:), allocatable :: PSISGAT      !<Soil moisture suction at saturation [m]
      real, dimension(:,:,:), allocatable :: PSISROT    !<
      real, dimension(:,:), allocatable :: PSIWGAT      !<Soil moisture suction at wilting point [m]
      real, dimension(:,:,:), allocatable :: PSIWROT    !<
      real, dimension(:,:), allocatable :: QA50GAT      !<Reference value of incoming shortwave radiation for vegetation category (used in stomatal resistance calculation) \f$[W m^{-2} ]\f$
      real, dimension(:,:,:), allocatable :: QA50ROT    !<
      real, dimension(:,:), allocatable :: ROOTGAT      !<Maximum rooting depth of vegetation category [m]
      real, dimension(:,:,:), allocatable :: ROOTROT    !<
      real, dimension(:,:), allocatable :: RSMNGAT      !<Minimum stomatal resistance of vegetation category \f$[s m^{-1} ]\f$
      real, dimension(:,:,:), allocatable :: RSMNROT    !<
      real, dimension(:,:,:), allocatable :: SANDROT    !<Percentage sand content of soil
      real, dimension(:,:), allocatable :: SDEPROT      !<Depth to bedrock in the soil profile
      real, dimension(:,:), allocatable :: TCSGAT       !<Thermal conductivity of soil particles \f$[W m^{-1} K^{-1} ]\f$
      real, dimension(:,:,:), allocatable :: TCSROT     !<
      real, dimension(:,:), allocatable :: THFCGAT      !<Field capacity \f$[m^3 m^{-3} ]\f$
      real, dimension(:,:,:), allocatable :: THFCROT    !<
      real, dimension(:,:), allocatable :: THMGAT       !<Residual soil liquid water content remaining after freezing or evaporation \f$[m^3 m^{-3} ]\f$
      real, dimension(:,:,:), allocatable :: THMROT     !<
      real, dimension(:,:), allocatable :: THPGAT       !<Pore volume in soil layer \f$[m^3 m^{-3} ]\f$
      real, dimension(:,:,:), allocatable :: THPROT     !<
      real, dimension(:,:), allocatable :: THRGAT       !<Liquid water retention capacity for organic soil \f$[m^3 m^{-3} ]\f$
      real, dimension(:,:,:), allocatable :: THRROT     !<
      real, dimension(:,:), allocatable :: THRAGAT      !<Fractional saturation of soil behind the wetting front [ ]
      real, dimension(:,:,:), allocatable :: THRAROT    !<
      real, dimension(:,:), allocatable :: VPDAGAT      !<Vapour pressure deficit coefficient for vegetation category (used in stomatal resistance calculation) [ ]
      real, dimension(:,:,:), allocatable :: VPDAROT    !<
      real, dimension(:,:), allocatable :: VPDBGAT      !<Vapour pressure deficit coefficient for vegetation category (used in stomatal resistance calculation) [ ]
      real, dimension(:,:,:), allocatable :: VPDBROT    !<
      real, dimension(:), allocatable :: WFCIGAT        !<Vertical Hydraulic Conductivity at Saturation at the Bottom of the Soil Layer, Used in WATROF [ ]
      real, dimension(:,:), allocatable :: WFCIROT      !<
      real, dimension(:), allocatable :: WFSFGAT        !<Manning's n for overland flow, Used in WATROF [ ]
      real, dimension(:,:), allocatable :: WFSFROT      !<
      real, dimension(:), allocatable :: XSLPGAT        !<Surface slope (used when running MESH code) [degrees]
      real, dimension(:,:), allocatable :: XSLPROT      !<
      real, dimension(:,:), allocatable :: ZBTWGAT      !<Depth to permeable bottom of soil layer [m]
      real, dimension(:,:,:), allocatable :: ZBTWROT    !<
      real, dimension(:), allocatable :: ZPLGGAT        !<Maximum water ponding depth for snow-free subareas (user-specified when running MESH code) [m]
      real, dimension(:,:), allocatable :: ZPLGROT      !<
      real, dimension(:), allocatable :: ZPLSGAT        !<Maximum water ponding depth for snow-covered subareas (user-specified when running MESH code) [m]
      real, dimension(:,:), allocatable :: ZPLSROT      !<
      real, dimension(:), allocatable :: ZSNLGAT        !<Limiting snow depth below which coverage is < 100% [m]
      real, dimension(:,:), allocatable :: ZSNLROT      !<



      real, dimension(:,:,:), allocatable :: THLWROT!<
      real, dimension(:,:), allocatable :: ZSNOROT  !<
      real, dimension(:,:), allocatable :: ALGWVROT !<
      real, dimension(:,:), allocatable :: ALGWNROT !<
      real, dimension(:,:), allocatable :: ALGDVROT !<
      real, dimension(:,:), allocatable :: ALGDNROT !<
      real, dimension(:,:), allocatable :: EMISROT  !<
      real, dimension(:,:,:), allocatable :: SALBROT!<
      real, dimension(:,:,:), allocatable :: CSALROT!<
      real, dimension(:,:), allocatable :: FSDBROL  !<
      real, dimension(:,:), allocatable :: FSFBROL  !<
      real, dimension(:,:), allocatable :: FSSBROL  !<

      real, dimension(:,:), allocatable :: THLWGAT  !<
      real, dimension(:), allocatable :: ALGWVGAT   !<
      real, dimension(:), allocatable :: ALGWNGAT   !<
      real, dimension(:), allocatable :: ALGDVGAT   !<
      real, dimension(:), allocatable :: ALGDNGAT   !<
      real, dimension(:), allocatable :: EMISGAT    !<
      real, dimension(:,:), allocatable :: SOCIROT  !<
C
      real, dimension(:,:), allocatable :: FSDBGAT  !<
      real, dimension(:,:), allocatable :: FSFBGAT  !<
      real, dimension(:,:), allocatable :: FSSBGAT  !<
      real, dimension(:,:), allocatable :: SALBGAT  !<
      real, dimension(:,:), allocatable :: CSALGAT  !<
C
C     * ARRAYS ASSOCIATED WITH COMMON BLOCKS.
C
!DAN    these kept as-was because correspond to look-up table
      REAL THPORG (  3) !<
      REAL THRORG (  3) !<
      REAL THMORG (  3) !<
      REAL BORG   (  3) !<
      REAL PSISORG(  3) !<
      REAL GRKSORG(  3) !<
C
      REAL CANEXT(ICAN) !<
      REAL XLEAF(ICAN)  !<
      REAL ZORAT(ICAN)  !<
      real, dimension(:), allocatable :: DELZ   !<
      real, dimension(:), allocatable :: ZBOT   !<
!DAN    'GROWYR' kept as-was because corresponds to look-up table
      REAL GROWYR (  18,4,2) !<
C
C     * ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES.

      real, dimension(:), allocatable :: CSZGAT  !<Cosine of solar zenith angle [ ]
      real, dimension(:), allocatable :: CSZROW  !<
      real, dimension(:), allocatable :: DLONGAT !<Longitude of grid cell (east of Greenwich) [degrees]
      real, dimension(:), allocatable :: DLONROW !<
      real, dimension(:), allocatable :: FCLOGAT !<Fractional cloud cover [ ]
      real, dimension(:), allocatable :: FCLOROW !<
      real, dimension(:), allocatable :: FDLGAT  !<Downwelling longwave radiation at bottom of atmosphere (i.e. incident on modelled land surface elements \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: FDLROW  !<
      real, dimension(:), allocatable :: FSIHGAT !<Near-infrared radiation incident on horizontal surface \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: FSIHROW !<
      real, dimension(:), allocatable :: FSVHGAT !<Visible radiation incident on horizontal surface \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: FSVHROW !<
      real, dimension(:), allocatable :: GCROW   !<Type identifier for grid cell (1 = sea ice, 0 = ocean, -1 = land)
      real, dimension(:), allocatable :: GGEOGAT !<Geothermal heat flux at bottom of soil profile \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: GGEOROW !<
      real, dimension(:), allocatable :: PADRGAT !<Partial pressure of dry air [Pa]
      real, dimension(:), allocatable :: PADRROW !<
      real, dimension(:), allocatable :: PREGAT  !<Surface precipitation rate \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:), allocatable :: PREROW  !<
      real, dimension(:), allocatable :: PRESGAT !<Surface air pressure [Pa]
      real, dimension(:), allocatable :: PRESROW !<
      real, dimension(:), allocatable :: QAGAT   !<Specific humidity at reference height \f$[kg kg^{-1} ]\f$
      real, dimension(:), allocatable :: QAROW   !<
      real, dimension(:), allocatable :: RADJGAT !<Latitude of grid cell (positive north of equator) [rad]
      real, dimension(:), allocatable :: RADJROW !<
      real, dimension(:), allocatable :: RHOAGAT !<Density of air \f$[kg m^{-3} ]\f$
      real, dimension(:), allocatable :: RHOAROW !<
      real, dimension(:), allocatable :: RHSIGAT !<Density of fresh snow \f$[kg m^{-3} ]\f$
      real, dimension(:), allocatable :: RHSIROW !<
      real, dimension(:), allocatable :: RPCPGAT !<Rainfall rate over modelled area \f$[m s^{-1} ]\f$
      real, dimension(:), allocatable :: RPCPROW !<
      real, dimension(:), allocatable :: RPREROW !<Rainfall rate over modelled area \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:), allocatable :: SPCPGAT !<Snowfall rate over modelled area \f$[m s^{-1} ]\f$
      real, dimension(:), allocatable :: SPCPROW !<
      real, dimension(:), allocatable :: SPREROW !<Snowfall rate over modelled area \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:), allocatable :: TAGAT   !<Air temperature at reference height [K]
      real, dimension(:), allocatable :: TAROW   !<
      real, dimension(:), allocatable :: TADPGAT !<Dew point temperature of air [K]
      real, dimension(:), allocatable :: TADPROW !<
      real, dimension(:), allocatable :: TRPCGAT !<Rainfall temperature [K]
      real, dimension(:), allocatable :: TRPCROW !<
      real, dimension(:), allocatable :: TSPCGAT !<Snowfall temperature [K]
      real, dimension(:), allocatable :: TSPCROW !<
      real, dimension(:), allocatable :: ULGAT   !<Zonal component of wind velocity \f$[m s^{-1} ]\f$
      real, dimension(:), allocatable :: ULROW   !<
      real, dimension(:), allocatable :: VLGAT   !<Meridional component of wind velocity \f$[m s^{-1} ]\f$
      real, dimension(:), allocatable :: VLROW   !<
      real, dimension(:), allocatable :: VMODGAT !<Wind speed at reference height \f$[m s^{-1} ]\f$
      real, dimension(:), allocatable :: VMODROW !<
      real, dimension(:), allocatable :: VPDGAT  !<Vapour pressure deficit [mb]
      real, dimension(:), allocatable :: VPDROW  !<
      real, dimension(:), allocatable :: Z0ORGAT !<Orographic roughness length [m]
      real, dimension(:), allocatable :: ZBLDGAT !<Atmospheric blending height for surface roughness length averaging [m]
      real, dimension(:), allocatable :: ZBLDROW !<
      real, dimension(:), allocatable :: ZDHGAT  !<User-specified height associated with diagnosed screen-level variables [m]
      real, dimension(:), allocatable :: ZDHROW  !<
      real, dimension(:), allocatable :: ZDMGAT  !<User-specified height associated with diagnosed anemometer-level wind speed [m]
      real, dimension(:), allocatable :: ZDMROW  !<
      real, dimension(:), allocatable :: ZRFHGAT !<Reference height associated with forcing air temperature and humidity [m]
      real, dimension(:), allocatable :: ZRFHROW !<
      real, dimension(:), allocatable :: ZRFMGAT !<Reference height associated with forcing wind speed [m]
      real, dimension(:), allocatable :: ZRFMROW !<
C
      real, dimension(:), allocatable :: UVROW   !<
      real, dimension(:), allocatable :: XDIFFUS !<
      real, dimension(:), allocatable :: Z0ORROW !<
C
      real, dimension(:), allocatable :: DLATROW !<
      real, dimension(:), allocatable :: FSSROW  !< Shortwave radiation \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: PRENROW !<
      real, dimension(:), allocatable :: CLDTROW !<
      real, dimension(:), allocatable :: FSGROL  !<
      real, dimension(:), allocatable :: FLGROL  !<
      real, dimension(:), allocatable :: GUSTROL !<
      real, dimension(:), allocatable :: DEPBROW !<
C
      real, dimension(:), allocatable :: FSGGAT  !<
      real, dimension(:), allocatable :: FLGGAT  !<
      real, dimension(:), allocatable :: GUSTGAT !<
      real, dimension(:), allocatable :: DEPBGAT !<
      real, dimension(:), allocatable :: GTBS    !<
      real, dimension(:), allocatable :: SFCUBS  !<
      real, dimension(:), allocatable :: SFCVBS  !<
      real, dimension(:), allocatable :: USTARBS !<
      real, dimension(:), allocatable :: TCSNOW  !<
      real, dimension(:), allocatable :: GSNOW   !<
C
C     * LAND SURFACE DIAGNOSTIC VARIABLES.
C
      real, dimension(:), allocatable :: ALIRGAT    !<Diagnosed total near-infrared albedo of land surface [ ]
      real, dimension(:,:), allocatable :: ALIRROT  !<
      real, dimension(:), allocatable :: ALIRROW    !<
      real, dimension(:), allocatable :: ALVSGAT    !<Diagnosed total visible albedo of land surface [ ]
      real, dimension(:,:), allocatable :: ALVSROT  !<
      real, dimension(:), allocatable :: ALVSROW    !<
      real, dimension(:), allocatable :: CDHGAT     !<Surface drag coefficient for heat [ ]
      real, dimension(:,:), allocatable :: CDHROT   !<
      real, dimension(:), allocatable :: CDHROW     !<
      real, dimension(:), allocatable :: CDMGAT     !<Surface drag coefficient for momentum [ ]
      real, dimension(:,:), allocatable :: CDMROT   !<
      real, dimension(:), allocatable :: CDMROW     !<
      real, dimension(:), allocatable :: DRGAT      !<Surface drag coefficient under neutral stability [ ]
      real, dimension(:,:), allocatable :: DRROT    !<
      real, dimension(:), allocatable :: DRROW      !<
      real, dimension(:), allocatable :: EFGAT      !<Evaporation efficiency at ground surface [ ]
      real, dimension(:,:), allocatable :: EFROT    !<
      real, dimension(:), allocatable :: EFROW      !<
      real, dimension(:), allocatable :: FLGGGAT    !<Diagnosed net longwave radiation at soil surface \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: FLGGROT  !<
      real, dimension(:), allocatable :: FLGGROW    !<
      real, dimension(:), allocatable :: FLGSGAT    !<Diagnosed net longwave radiation at snow surface \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: FLGSROT  !<
      real, dimension(:), allocatable :: FLGSROW    !<
      real, dimension(:), allocatable :: FLGVGAT    !<Diagnosed net longwave radiation on vegetation canopy \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: FLGVROT  !<
      real, dimension(:), allocatable :: FLGVROW    !<
      real, dimension(:), allocatable :: FSGGGAT    !<Diagnosed net shortwave radiation at soil surface \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: FSGGROT  !<
      real, dimension(:), allocatable :: FSGGROW    !<
      real, dimension(:), allocatable :: FSGSGAT    !<Diagnosed net shortwave radiation at snow surface \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: FSGSROT  !<
      real, dimension(:), allocatable :: FSGSROW    !<
      real, dimension(:), allocatable :: FSGVGAT    !<Diagnosed net shortwave radiation on vegetation canopy \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: FSGVROT  !<
      real, dimension(:), allocatable :: FSGVROW    !<
      real, dimension(:), allocatable :: FSNOGAT    !<Diagnosed fractional snow coverage [ ]
      real, dimension(:,:), allocatable :: FSNOROT  !<
      real, dimension(:), allocatable :: FSNOROW    !<
      real, dimension(:), allocatable :: GAGAT      !<Diagnosed product of drag coefficient and wind speed over modelled area \f$[m s^{-1} ]\f$
      real, dimension(:,:), allocatable :: GAROT    !<
      real, dimension(:), allocatable :: GAROW      !<
      real, dimension(:,:), allocatable :: GFLXGAT  !<Heat conduction between soil layers \f$[W m^{-2} ]\f$
      real, dimension(:,:,:), allocatable :: GFLXROT!<
      real, dimension(:,:), allocatable :: GFLXROW  !<
      real, dimension(:), allocatable :: GTGAT      !<Diagnosed effective surface black-body temperature [K]
      real, dimension(:,:), allocatable :: GTROT    !<
      real, dimension(:), allocatable :: GTROW      !<
      real, dimension(:), allocatable :: HBLGAT     !<Height of the atmospheric boundary layer [m]
      real, dimension(:,:), allocatable :: HBLROT   !<
      real, dimension(:), allocatable :: HBLROW     !<
      real, dimension(:), allocatable :: HEVCGAT    !<Diagnosed latent heat flux on vegetation canopy \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HEVCROT  !<
      real, dimension(:), allocatable :: HEVCROW    !<
      real, dimension(:), allocatable :: HEVGGAT    !<Diagnosed latent heat flux at soil surface \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HEVGROT  !<
      real, dimension(:), allocatable :: HEVGROW    !<
      real, dimension(:), allocatable :: HEVSGAT    !<Diagnosed latent heat flux at snow surface \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HEVSROT  !<
      real, dimension(:), allocatable :: HEVSROW    !<
      real, dimension(:), allocatable :: HFSGAT     !<Diagnosed total surface sensible heat flux over modelled area \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HFSROT   !<
      real, dimension(:), allocatable :: HFSROW     !<
      real, dimension(:), allocatable :: HFSCGAT    !<Diagnosed sensible heat flux on vegetation canopy \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HFSCROT  !<
      real, dimension(:), allocatable :: HFSCROW    !<
      real, dimension(:), allocatable :: HFSGGAT    !<Diagnosed sensible heat flux at soil surface \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HFSGROT  !<
      real, dimension(:), allocatable :: HFSGROW    !<
      real, dimension(:), allocatable :: HFSSGAT    !<Diagnosed sensible heat flux at snow surface \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HFSSROT  !<
      real, dimension(:), allocatable :: HFSSROW    !<
      real, dimension(:), allocatable :: HMFCGAT    !<Diagnosed energy associated with phase change of water on vegetation \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HMFCROT  !<
      real, dimension(:), allocatable :: HMFCROW    !<
      real, dimension(:,:), allocatable :: HMFGGAT  !<Diagnosed energy associated with phase change of water in soil layers \f$[W m^{-2} ]\f$
      real, dimension(:,:,:), allocatable :: HMFGROT!<
      real, dimension(:,:), allocatable :: HMFGROW  !<
      real, dimension(:), allocatable :: HMFNGAT    !<Diagnosed energy associated with phase change of water in snow pack \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HMFNROT  !<
      real, dimension(:), allocatable :: HMFNROW    !<
      real, dimension(:,:), allocatable :: HTCGAT   !<Diagnosed internal energy change of soil layer due to conduction and/or change in mass \f$[W m^{-2} ]\f$
      real, dimension(:,:,:), allocatable :: HTCROT !<
      real, dimension(:,:), allocatable :: HTCROW   !<
      real, dimension(:), allocatable :: HTCCGAT    !<Diagnosed internal energy change of vegetation canopy due to conduction and/or change in mass \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HTCCROT  !<
      real, dimension(:), allocatable :: HTCCROW    !<
      real, dimension(:), allocatable :: HTCSGAT    !<Diagnosed internal energy change of snow pack due to conduction and/or change in mass \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: HTCSROT  !<
      real, dimension(:), allocatable :: HTCSROW    !<
      real, dimension(:), allocatable :: ILMOGAT    !<Inverse of Monin-Obukhov roughness length \f$(m^{-1} ]\f$
      real, dimension(:,:), allocatable :: ILMOROT  !<
      real, dimension(:), allocatable :: ILMOROW    !<
      integer, dimension(:), allocatable :: ISUM    !<Total number of iterations required to solve surface energy balance for the elements of the four subareas for the current run
      integer, dimension(:,:,:), allocatable :: ITCTGAT     !<Counter of number of iterations required to solve surface energy balance for the elements of the four subareas
      integer, dimension(:,:,:,:), allocatable :: ITCTROT   !<
      real, dimension(:), allocatable :: PCFCGAT    !<Diagnosed frozen precipitation intercepted by vegetation \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: PCFCROT  !<
      real, dimension(:), allocatable :: PCFCROW    !<
      real, dimension(:), allocatable :: PCLCGAT    !<Diagnosed liquid precipitation intercepted by vegetation \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: PCLCROT  !<
      real, dimension(:), allocatable :: PCLCROW    !<
      real, dimension(:), allocatable :: PCPGGAT    !<Diagnosed precipitation incident on ground \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: PCPGROT  !<
      real, dimension(:), allocatable :: PCPGROW    !<
      real, dimension(:), allocatable :: PCPNGAT    !<Diagnosed precipitation incident on snow pack \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: PCPNROT  !<
      real, dimension(:), allocatable :: PCPNROW    !<
      real, dimension(:), allocatable :: PETGAT     !<Diagnosed potential evapotranspiration \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: PETROT   !<
      real, dimension(:), allocatable :: PETROW     !<
      real, dimension(:), allocatable :: QEVPGAT    !<Diagnosed total surface latent heat flux over modelled area \f$[W m^{-2} ]\f$
      real, dimension(:,:), allocatable :: QEVPROT  !<
      real, dimension(:), allocatable :: QEVPROW    !<
      real, dimension(:,:), allocatable :: QFCGAT   !<Diagnosed vapour flux from transpiration over modelled area \f$[W m^{-2} ]\f$
      real, dimension(:,:,:), allocatable :: QFCROT !<
      real, dimension(:,:), allocatable :: QFCROW   !<
      real, dimension(:), allocatable :: QFCFGAT    !<Diagnosed vapour flux from frozen water on vegetation \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: QFCFROT  !<
      real, dimension(:), allocatable :: QFCFROW    !<
      real, dimension(:), allocatable :: QFCLGAT    !<Diagnosed vapour flux from liquid water on vegetation \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: QFCLROT  !<
      real, dimension(:), allocatable :: QFCLROW    !<
      real, dimension(:), allocatable :: QFGGAT     !<Diagnosed water vapour flux from ground \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: QFGROT   !<
      real, dimension(:), allocatable :: QFGROW     !<
      real, dimension(:), allocatable :: QFNGAT     !<Diagnosed water vapour flux from snow pack \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: QFNROT   !<
      real, dimension(:), allocatable :: QFNROW     !<
      real, dimension(:), allocatable :: QFSGAT     !<Diagnosed total surface water vapour flux over modelled area \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: QFSROT   !<
      real, dimension(:), allocatable :: QFSROW     !<
      real, dimension(:), allocatable :: QFXGAT     !<Product of surface drag coefficient, wind speed and surface-air specific humidity difference \f$[m s^{-1} ]\f$
      real, dimension(:,:), allocatable :: QFXROT   !<
      real, dimension(:), allocatable :: QFXROW     !<
      real, dimension(:), allocatable :: QGGAT      !<Diagnosed surface specific humidity \f$[kg kg^{-1} ]\f$
      real, dimension(:,:), allocatable :: QGROT    !<
      real, dimension(:), allocatable :: QGROW      !<
      real, dimension(:), allocatable :: ROFGAT     !<Total runoff from soil \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: ROFROT   !<
      real, dimension(:), allocatable :: ROFROW     !<
      real, dimension(:), allocatable :: ROFBGAT    !<Base flow from bottom of soil column \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: ROFBROT  !<
      real, dimension(:), allocatable :: ROFBROW    !<
      real, dimension(:), allocatable :: ROFCGAT    !<Liquid/frozen water runoff from vegetation \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: ROFCROT  !<
      real, dimension(:), allocatable :: ROFCROW    !<
      real, dimension(:), allocatable :: ROFNGAT    !<Liquid water runoff from snow pack \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: ROFNROT  !<
      real, dimension(:), allocatable :: ROFNROW    !<
      real, dimension(:), allocatable :: ROFOGAT    !<Overland flow from top of soil column \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: ROFOROT  !<
      real, dimension(:), allocatable :: ROFOROW    !<
      real, dimension(:), allocatable :: ROFSGAT    !<Interflow from sides of soil column \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: ROFSROT  !<
      real, dimension(:), allocatable :: ROFSROW    !<
      real, dimension(:), allocatable :: ROVGGAT    !<Diagnosed liquid/frozen water runoff from vegetation to ground surface \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: ROVGROT  !<
      real, dimension(:), allocatable :: ROVGROW    !<
      real, dimension(:), allocatable :: SFCQGAT    !<Diagnosed screen-level specific humidity \f$[kg kg^{-1} ]\f$
      real, dimension(:,:), allocatable :: SFCQROT  !<
      real, dimension(:), allocatable :: SFCQROW    !<
      real, dimension(:), allocatable :: SFCTGAT    !<Diagnosed screen-level air temperature [K]
      real, dimension(:,:), allocatable :: SFCTROT  !<
      real, dimension(:), allocatable :: SFCTROW    !<
      real, dimension(:), allocatable :: SFCUGAT    !<Diagnosed anemometer-level zonal wind \f$[m s^{-1} ]\f$
      real, dimension(:,:), allocatable :: SFCUROT  !<
      real, dimension(:), allocatable :: SFCUROW    !<
      real, dimension(:), allocatable :: SFCVGAT    !<Diagnosed anemometer-level meridional wind \f$[m s^{-1} ]\f$
      real, dimension(:,:), allocatable :: SFCVROT  !<
      real, dimension(:), allocatable :: SFCVROW    !<
      real, dimension(:), allocatable :: TFXGAT     !<Product of surface drag coefficient, wind speed and surface-air temperature difference \f$[K m s^{-1} ]\f$
      real, dimension(:,:), allocatable :: TFXROT   !<
      real, dimension(:), allocatable :: TFXROW     !<
      real, dimension(:), allocatable :: TROBGAT    !<Temperature of base flow from bottom of soil column [K]
      real, dimension(:,:), allocatable :: TROBROT  !<
      real, dimension(:), allocatable :: TROFGAT    !<Temperature of total runoff [K]
      real, dimension(:,:), allocatable :: TROFROT  !<
      real, dimension(:), allocatable :: TROOGAT    !<Temperature of overland flow from top of soil column [K]
      real, dimension(:,:), allocatable :: TROOROT  !<
      real, dimension(:), allocatable :: TROSGAT    !<Temperature of interflow from sides of soil column [K]
      real, dimension(:,:), allocatable :: TROSROT  !<
      real, dimension(:), allocatable :: UEGAT      !<Friction velocity of air \f$[m s^{-1} ]\f$
      real, dimension(:,:), allocatable :: UEROT    !<
      real, dimension(:), allocatable :: UEROW      !<
      real, dimension(:), allocatable :: WTABGAT    !<Depth of water table in soil [m]
      real, dimension(:,:), allocatable :: WTABROT  !<
      real, dimension(:), allocatable :: WTABROW    !<
      real, dimension(:), allocatable :: WTRCGAT    !<Diagnosed residual water transferred off the vegetation canopy \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: WTRCROT  !<
      real, dimension(:), allocatable :: WTRCROW    !<
      real, dimension(:), allocatable :: WTRGGAT    !<Diagnosed residual water transferred into or out of the soil \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: WTRGROT  !<
      real, dimension(:), allocatable :: WTRGROW    !<
      real, dimension(:), allocatable :: WTRSGAT    !<Diagnosed residual water transferred into or out of the snow pack \f$[kg m^{-2} s^{-1} ]\f$
      real, dimension(:,:), allocatable :: WTRSROT  !<
      real, dimension(:), allocatable :: WTRSROW    !<
C
      real, dimension(:), allocatable :: QLWOGAT    !<
      real, dimension(:), allocatable :: SFRHGAT    !<
      real, dimension(:,:), allocatable :: SFRHROT  !<
      real, dimension(:), allocatable :: SFRHROW    !<
C
      real, dimension(:), allocatable :: FTEMP      !<
      real, dimension(:), allocatable :: FVAP       !<
      real, dimension(:), allocatable :: RIB        !<
C
C     * ARRAYS USED FOR OUTPUT AND DISPLAY PURPOSES.
C     * (THE SUFFIX "ACC" REFERS TO ACCUMULATOR ARRAYS USED IN
C     * CALCULATING TIME AVERAGES.)
C
      CHARACTER     TITLE1*4,     TITLE2*4,     TITLE3*4,
     1              TITLE4*4,     TITLE5*4,     TITLE6*4
      CHARACTER     NAME1*4,      NAME2*4,      NAME3*4,
     1              NAME4*4,      NAME5*4,      NAME6*4
      CHARACTER     PLACE1*4,     PLACE2*4,     PLACE3*4,
     1              PLACE4*4,     PLACE5*4,     PLACE6*4

!File name for the BasinAveraged File
!And the CTEM grided Output as well

      CHARACTER    BasinFile*80

C
      real, dimension(:), allocatable :: ALIRACC    !<Diagnosed total near-infrared albedo of land surface [ ]
      real, dimension(:), allocatable :: ALVSACC    !<Diagnosed total visible albedo of land surface [ ]
      real, dimension(:), allocatable :: EVAPACC    !<Diagnosed total surface water vapour flux over modelled area \f$[kg m^{-2} ]\f$
      real, dimension(:), allocatable :: FLINACC    !<Downwelling longwave radiation above surface \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: FLUTACC    !<Upwelling longwave radiation from surface \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: FSINACC    !<Downwelling shortwave radiation above surface \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: GROACC     !<Vegetation growth index [ ]
      real, dimension(:), allocatable :: GTACC      !<Diagnosed effective surface black-body temperature [K]
      real, dimension(:), allocatable :: HFSACC     !<Diagnosed total surface sensible heat flux over modelled area \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: HMFNACC    !<Diagnosed energy associated with phase change of water in snow pack \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: OVRACC     !<Overland flow from top of soil column \f$[kg m^{-2} ]\f$
      real, dimension(:), allocatable :: PREACC     !<Surface precipitation rate \f$[kg m^{-2} ]\f$
      real, dimension(:), allocatable :: PRESACC    !<Surface air pressure [Pa]
      real, dimension(:), allocatable :: QAACC      !<Specific humidity at reference height \f$[kg kg^{-1} ]\f$
      real, dimension(:), allocatable :: QEVPACC    !<Diagnosed total surface latent heat flux over modelled area \f$[W m^{-2} ]\f$
      real, dimension(:), allocatable :: RCANACC    !<Intercepted liquid water stored on canopy \f$[kg m^{-2} ]\f$
      real, dimension(:), allocatable :: RHOSACC    !<Density of snow \f$[kg m^{-3} ]\f$
      real, dimension(:), allocatable :: ROFACC     !<Total runoff from soil \f$[kg m^{-2} ]\f$
      real, dimension(:), allocatable :: SCANACC    !<Intercepted frozen water stored on canopy \f$[kg m^{-2} ]\f$
      real, dimension(:), allocatable :: SNOACC     !<Mass of snow pack \f$[kg m^{-2} ]\f$
      real, dimension(:), allocatable :: TAACC      !<Air temperature at reference height [K]
      real, dimension(:,:), allocatable :: TBARACC  !<Temperature of soil layers [K]
      real, dimension(:,:), allocatable :: THALACC  !<Total volumetric water content of soil layers \f$[m^3 m^{-3} ]\f$
      real, dimension(:,:), allocatable :: THICACC  !<Volumetric frozen water content of soil layers \f$[m^3 m^{-3} ]\f$
      real, dimension(:,:), allocatable :: THLQACC  !<Volumetric liquid water content of soil layers \f$[m^3 m^{-3} ]\f$
      real, dimension(:), allocatable :: TCANACC    !<Vegetation canopy temperature [K]
      real, dimension(:), allocatable :: TSNOACC    !<Snowpack temperature [K]
      real, dimension(:), allocatable :: UVACC      !<Wind speed \f$[m s^{-1} ]\f$
      real, dimension(:), allocatable :: WSNOACC    !<Liquid water content of snow pack \f$[kg m^{-2} ]\f$
      real, dimension(:), allocatable :: WTBLACC    !<Depth of water table in soil [m]
      real, dimension(:), allocatable :: ALTOTACC   !<Broadband albedo [-]
C
      real, dimension(:), allocatable :: CANARE  !<
      real, dimension(:), allocatable :: SNOARE  !<
C
C     * ARRAYS DEFINED TO PASS INFORMATION BETWEEN THE THREE MAJOR
C     * SUBSECTIONS OF CLASS ("CLASSA", "CLASST" AND "CLASSW").
C
      real, dimension(:,:), allocatable :: TBARC  !<
      real, dimension(:,:), allocatable :: TBARG  !<
      real, dimension(:,:), allocatable :: TBARCS !<
      real, dimension(:,:), allocatable :: TBARGS !<
      real, dimension(:,:), allocatable :: THLIQC !<
      real, dimension(:,:), allocatable :: THLIQG !<
      real, dimension(:,:), allocatable :: THICEC !<
      real, dimension(:,:), allocatable :: THICEG !<
      real, dimension(:,:), allocatable :: FROOT  !<
      real, dimension(:,:), allocatable :: HCPC   !<
      real, dimension(:,:), allocatable :: HCPG   !<
      real, dimension(:,:), allocatable :: FROOTS !<
      real, dimension(:,:), allocatable :: TCTOPC !<
      real, dimension(:,:), allocatable :: TCBOTC !<
      real, dimension(:,:), allocatable :: TCTOPG !<
      real, dimension(:,:), allocatable :: TCBOTG !<
C
      real, dimension(:), allocatable :: FC       !<
      real, dimension(:), allocatable :: FG       !<
      real, dimension(:), allocatable :: FCS      !<
      real, dimension(:), allocatable :: FGS      !<
      real, dimension(:), allocatable :: RBCOEF   !<
      real, dimension(:), allocatable :: ZSNOW    !<
      real, dimension(:), allocatable :: FSVF     !<
      real, dimension(:), allocatable :: FSVFS    !<
      real, dimension(:), allocatable :: ALVSCN   !<
      real, dimension(:), allocatable :: ALIRCN   !<
      real, dimension(:), allocatable :: ALVSG    !<
      real, dimension(:), allocatable :: ALIRG    !<
      real, dimension(:), allocatable :: ALVSCS   !<
      real, dimension(:), allocatable :: ALIRCS   !<
      real, dimension(:), allocatable :: ALVSSN   !<
      real, dimension(:), allocatable :: ALIRSN   !<
      real, dimension(:), allocatable :: ALVSGC   !<
      real, dimension(:), allocatable :: ALIRGC   !<
      real, dimension(:), allocatable :: ALVSSC   !<
      real, dimension(:), allocatable :: ALIRSC   !<
      real, dimension(:), allocatable :: TRVSCN   !<
      real, dimension(:), allocatable :: TRIRCN   !<
      real, dimension(:), allocatable :: TRVSCS   !<
      real, dimension(:), allocatable :: TRIRCS   !<
      real, dimension(:), allocatable :: RC       !<
      real, dimension(:), allocatable :: RCS      !<
      real, dimension(:), allocatable :: FRAINC   !<
      real, dimension(:), allocatable :: FSNOWC   !<
      real, dimension(:), allocatable :: FRAICS   !<
      real, dimension(:), allocatable :: FSNOCS   !<
      real, dimension(:), allocatable :: CMASSC   !<
      real, dimension(:), allocatable :: CMASCS   !<
      real, dimension(:), allocatable :: DISP     !<
      real, dimension(:), allocatable :: DISPS    !<
      real, dimension(:), allocatable :: ZOMLNC   !<
      real, dimension(:), allocatable :: ZOELNC   !<
      real, dimension(:), allocatable :: ZOMLNG   !<
      real, dimension(:), allocatable :: ZOELNG   !<
      real, dimension(:), allocatable :: ZOMLCS   !<
      real, dimension(:), allocatable :: ZOELCS   !<
      real, dimension(:), allocatable :: ZOMLNS   !<
      real, dimension(:), allocatable :: ZOELNS   !<
      real, dimension(:), allocatable :: TRSNOWC  !<
      real, dimension(:), allocatable :: CHCAP    !<
      real, dimension(:), allocatable :: CHCAPS   !<
      real, dimension(:), allocatable :: GZEROC   !<
      real, dimension(:), allocatable :: GZEROG   !<
      real, dimension(:), allocatable :: GZROCS   !<
      real, dimension(:), allocatable :: GZROGS   !<
      real, dimension(:), allocatable :: G12C     !<
      real, dimension(:), allocatable :: G12G     !<
      real, dimension(:), allocatable :: G12CS    !<
      real, dimension(:), allocatable :: G12GS    !<
      real, dimension(:), allocatable :: G23C     !<
      real, dimension(:), allocatable :: G23G     !<
      real, dimension(:), allocatable :: G23CS    !<
      real, dimension(:), allocatable :: G23GS    !<
      real, dimension(:), allocatable :: QFREZC   !<
      real, dimension(:), allocatable :: QFREZG   !<
      real, dimension(:), allocatable :: QMELTC   !<
      real, dimension(:), allocatable :: QMELTG   !<
      real, dimension(:), allocatable :: EVAPC    !<
      real, dimension(:), allocatable :: EVAPCG   !<
      real, dimension(:), allocatable :: EVAPG    !<
      real, dimension(:), allocatable :: EVAPCS   !<
      real, dimension(:), allocatable :: EVPCSG   !<
      real, dimension(:), allocatable :: EVAPGS   !<
      real, dimension(:), allocatable :: TCANO    !<
      real, dimension(:), allocatable :: TCANS    !<
      real, dimension(:), allocatable :: RAICAN   !<
      real, dimension(:), allocatable :: SNOCAN   !<
      real, dimension(:), allocatable :: RAICNS   !<
      real, dimension(:), allocatable :: SNOCNS   !<
      real, dimension(:), allocatable :: CWLCAP   !<
      real, dimension(:), allocatable :: CWFCAP   !<
      real, dimension(:), allocatable :: CWLCPS   !<
      real, dimension(:), allocatable :: CWFCPS   !<
      real, dimension(:), allocatable :: TSNOCS   !<
      real, dimension(:), allocatable :: TSNOGS   !<
      real, dimension(:), allocatable :: RHOSCS   !<
      real, dimension(:), allocatable :: RHOSGS   !<
      real, dimension(:), allocatable :: WSNOCS   !<
      real, dimension(:), allocatable :: WSNOGS   !<
      real, dimension(:), allocatable :: TPONDC   !<
      real, dimension(:), allocatable :: TPONDG   !<
      real, dimension(:), allocatable :: TPNDCS   !<
      real, dimension(:), allocatable :: TPNDGS   !<
      real, dimension(:), allocatable :: ZPLMCS   !<
      real, dimension(:), allocatable :: ZPLMGS   !<
      real, dimension(:), allocatable :: ZPLIMC   !<
      real, dimension(:), allocatable :: ZPLIMG   !<
C
      real, dimension(:,:), allocatable :: ALTG    !<
      real, dimension(:,:), allocatable :: ALSNO   !<
      real, dimension(:,:), allocatable :: TRSNOWG !<
C
C     * DIAGNOSTIC ARRAYS USED FOR CHECKING ENERGY AND WATER
C     * BALANCES.
C
      real, dimension(:), allocatable :: CTVSTP !<
      real, dimension(:), allocatable :: CTSSTP !<
      real, dimension(:,:), allocatable :: CTNSTP !<
      real, dimension(:), allocatable :: WTVSTP !<
      real, dimension(:), allocatable :: WTSSTP !<
      real, dimension(:), allocatable :: WTGSTP !<
C
C     * CONSTANTS AND TEMPORARY VARIABLES.
C
      REAL DEGLON,DAY,DECL,HOUR,COSZ,CUMSNO,EVAPSUM,
     1     QSUMV,QSUMS,QSUM1,QSUM2,QSUM3,WSUMV,WSUMS,WSUMG,ALTOT,
     2     FSSTAR,FLSTAR,QH,QE,BEG,SNOMLT,ZSN,TCN,TSN,TPN,GTOUT,TAC,
     3     TSURF,ALAVG,ALMAX,ACTLYR,FTAVG,FTMAX,FTABLE
C
C     * COMMON BLOCK PARAMETERS.
C
      REAL X1,X2,X3,X4,G,GAS,X5,X6,CPRES,GASV,X7,CPI,X8,CELZRO,X9,
     1     X10,X11,X12,X13,X14,X15,SIGMA,X16,DELTIM,DELT,TFREZ,
     2     RGAS,RGASV,GRAV,SBC,VKC,CT,VMIN,TCW,TCICE,TCSAND,TCCLAY,
     3     TCOM,TCDRYS,RHOSOL,RHOOM,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,
     4     HCPCLY,SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,
     5     CLHVAP,PI,ZOLNG,ZOLNS,ZOLNI,ZORATG,ALVSI,ALIRI,ALVSO,ALIRO,
     6     ALBRCK,DELTA,CGRAV,CKARM,CPD,AS,ASX,CI,BS,BETA,FACTN,HMIN,
     7     ANGMAX,A,B



C
c================= CTEM array declaration ===============================\
c
c     Local variables for coupling CLASS and CTEM
c

      character*80   titlec1
      character*80   argbuff
      character*160  command

       integer   lopcount,  isumc,     k1c,       k2c,
     2           jhhstd,    jhhendd,   jdstd,   jdendd,
     3           jhhsty,     jhhendy,   jdsty,   jdendy,
     4           month1,     month2,      xday,  ctemloop,
     5           nummetcylyrs, ncyear,  co2yr,   spinfast,
     6           nol2pfts(4),  popyr, metcylyrst, metcycendyr,
     7           climiyear,   popcycleyr,    cypopyr, lucyr,
     8           cylucyr, endyr,bigpftc(1), obswetyr,
     9           cywetldyr, trans_startyr, jmosty, obslghtyr,
     +           lath, testyr, lghtdy
        integer, dimension(:), allocatable :: curlatno
        integer, dimension(:), allocatable :: altotcount_ctm

      real      co2concin,    setco2conc, sumfare,
     1           temp_var, barefrac,
     2           ch4concin, setch4conc
        real, dimension(:,:), allocatable :: todfrac
        real, dimension(:,:), allocatable :: barf

        real, dimension(:), allocatable ::
     1          currlat,             wl,  grclarea,
     2             radl,          wossl,        sl,
     3               cl,             ml

        real fsstar_gat, flstar_gat
        real, dimension(:), allocatable ::
     1      fsinacc_gat, flutacc_gat, flinacc_gat,
     2      alswacc_gat, allwacc_gat, pregacc_gat,
     3      altotacc_gat,
     4      netrad_gat,  preacc_gat

!     For these below, the corresponding ROWs are defined by CLASS

       real, dimension(:), allocatable ::
     1      sdepgat,
     2      xdiffusgat, ! the corresponding ROW is CLASS's XDIFFUS
     3      faregat ! the ROT is FAREROT
       real, dimension(:,:), allocatable ::
     1      orgmgat,
     2      sandgat, claygat


!     Nitrogen Process Parameters

       real et0, jmax, kl, km,
     1      kmin0, rtmass0, r, solnh4,
     2      rnlf0, rnsm0, rnrt0,
     3      conreal,
     4      lai0, krubn, kn,
     5      kni0, kdn0, kv0,
     6      nbfix0, ndep0, nfer0, nfero0

      COMMON /NITROGEN/ et0, jmax, kl,
     1      km, kmin0, rtmass0,
     2      r, solnh4,
     3      rnlf0, rnsm0, rnrt0,
     4      conreal,
     5      lai0, krubn, kn,
     6      kni0, kdn0, kv0


      ! Model switches:
      logical, pointer :: ctem_on
      logical, pointer :: parallelrun
      logical, pointer :: cyclemet
      logical, pointer :: dofire
      logical, pointer :: run_model
      logical, pointer :: met_rewound
      logical, pointer :: reach_eof
      logical, pointer :: compete
      logical, pointer :: start_bare
      logical, pointer :: rsfile
      logical, pointer :: lnduseon
      logical, pointer :: co2on
      logical, pointer :: ch4on
      logical, pointer :: popdon
      logical, pointer :: inibioclim
      logical, pointer :: start_from_rs
      logical, pointer :: leap
      logical, pointer :: dowetlands
      logical, pointer :: obswetf
      logical, pointer :: transient_run
      logical, pointer :: ctemn

      ! ROW vars:
      logical, pointer, dimension(:,:,:) :: pftexistrow
      integer, pointer, dimension(:,:,:) :: colddaysrow
      integer, pointer, dimension(:,:) :: icountrow
      integer, pointer, dimension(:,:,:) :: lfstatusrow
      integer, pointer, dimension(:,:,:) :: pandaysrow
      integer, pointer, dimension(:,:) :: stdalnrow
      real, pointer, dimension(:,:) :: tcanrs
      real, pointer, dimension(:,:) :: tsnors
      real, pointer, dimension(:,:) :: tpndrs
      real, pointer, dimension(:,:,:) :: csum
      real, pointer, dimension(:,:,:) :: tbaraccrow_t
      real, pointer, dimension(:,:) :: tcanoaccrow_t
      real, pointer, dimension(:,:) :: uvaccrow_t
      real, pointer, dimension(:,:) :: vvaccrow_t

      real, pointer, dimension(:,:,:) :: ailcminrow         
      real, pointer, dimension(:,:,:) :: ailcmaxrow         
      real, pointer, dimension(:,:,:) :: dvdfcanrow         
      real, pointer, dimension(:,:,:) :: gleafmasrow        
      real, pointer, dimension(:,:,:) :: bleafmasrow        
      real, pointer, dimension(:,:,:) :: stemmassrow        
      real, pointer, dimension(:,:,:) :: rootmassrow        
      real, pointer, dimension(:,:,:) :: pstemmassrow       
      real, pointer, dimension(:,:,:) :: pgleafmassrow      
      real, pointer, dimension(:,:,:) :: fcancmxrow
      real, pointer, dimension(:,:) :: gavglairow
      real, pointer, dimension(:,:,:) :: zolncrow
      real, pointer, dimension(:,:,:) :: ailcrow
      real, pointer, dimension(:,:,:) :: ailcgrow
      real, pointer, dimension(:,:,:) :: ailcgsrow
      real, pointer, dimension(:,:,:) :: fcancsrow
      real, pointer, dimension(:,:,:) :: fcancrow
      real, pointer, dimension(:,:) :: co2concrow
      real, pointer, dimension(:,:) :: ch4concrow
      real, pointer, dimension(:,:,:) :: co2i1cgrow
      real, pointer, dimension(:,:,:) :: co2i1csrow
      real, pointer, dimension(:,:,:) :: co2i2cgrow
      real, pointer, dimension(:,:,:) :: co2i2csrow
      real, pointer, dimension(:,:,:) :: ancsvegrow
      real, pointer, dimension(:,:,:) :: ancgvegrow
      real, pointer, dimension(:,:,:) :: rmlcsvegrow
      real, pointer, dimension(:,:,:) :: rmlcgvegrow
      real, pointer, dimension(:,:,:) :: slairow
      real, pointer, dimension(:,:,:) :: ailcbrow
      real, pointer, dimension(:,:) :: canresrow
      real, pointer, dimension(:,:,:) :: flhrlossrow

      real, pointer, dimension(:,:,:) :: grwtheffrow
      real, pointer, dimension(:,:,:) :: lystmmasrow
      real, pointer, dimension(:,:,:) :: lyrotmasrow
      real, pointer, dimension(:,:,:) :: tymaxlairow
      real, pointer, dimension(:,:) :: vgbiomasrow
      real, pointer, dimension(:,:) :: gavgltmsrow
      real, pointer, dimension(:,:) :: gavgscmsrow
      real, pointer, dimension(:,:,:) :: stmhrlosrow
      real, pointer, dimension(:,:,:,:) :: rmatcrow
      real, pointer, dimension(:,:,:,:) :: rmatctemrow
      real, pointer, dimension(:,:,:) :: litrmassrow
      real, pointer, dimension(:,:,:) :: soilcmasrow
      real, pointer, dimension(:,:,:) :: vgbiomas_vegrow

      real, pointer, dimension(:,:,:) :: emit_co2row
      real, pointer, dimension(:,:,:) :: emit_corow
      real, pointer, dimension(:,:,:) :: emit_ch4row
      real, pointer, dimension(:,:,:) :: emit_nmhcrow
      real, pointer, dimension(:,:,:) :: emit_h2row
      real, pointer, dimension(:,:,:) :: emit_noxrow
      real, pointer, dimension(:,:,:) :: emit_n2orow
      real, pointer, dimension(:,:,:) :: emit_pm25row
      real, pointer, dimension(:,:,:) :: emit_tpmrow
      real, pointer, dimension(:,:,:) :: emit_tcrow
      real, pointer, dimension(:,:,:) :: emit_ocrow
      real, pointer, dimension(:,:,:) :: emit_bcrow
      real, pointer, dimension(:,:) :: burnfracrow
      real, pointer, dimension(:,:,:) :: burnvegfrow
      real, pointer, dimension(:,:,:) :: smfuncvegrow
      real, pointer, dimension(:,:) :: popdinrow
      real, pointer, dimension(:,:,:) :: btermrow
      real, pointer, dimension(:,:) :: ltermrow
      real, pointer, dimension(:,:,:) :: mtermrow

      real, pointer, dimension(:,:) :: extnprobrow
      real, pointer, dimension(:,:) :: prbfrhucrow
      real, pointer, dimension(:,:,:) :: mlightngrow
      real, pointer, dimension(:) :: dayl_maxrow
      real, pointer, dimension(:) :: daylrow


      real, pointer, dimension(:,:,:) :: bmasvegrow
      real, pointer, dimension(:,:,:) :: cmasvegcrow
      real, pointer, dimension(:,:,:) :: veghghtrow
      real, pointer, dimension(:,:,:) :: rootdpthrow
      real, pointer, dimension(:,:) :: rmlrow
      real, pointer, dimension(:,:) :: rmsrow
      real, pointer, dimension(:,:,:) :: tltrleafrow
      real, pointer, dimension(:,:,:) :: tltrstemrow
      real, pointer, dimension(:,:,:) :: tltrrootrow
      real, pointer, dimension(:,:,:) :: leaflitrrow
      real, pointer, dimension(:,:,:) :: roottemprow
      real, pointer, dimension(:,:,:) :: afrleafrow
      real, pointer, dimension(:,:,:) :: afrstemrow
      real, pointer, dimension(:,:,:) :: afrrootrow
      real, pointer, dimension(:,:,:) :: wtstatusrow
      real, pointer, dimension(:,:,:) :: ltstatusrow
      real, pointer, dimension(:,:) :: rmrrow

      real, pointer, dimension(:,:,:) :: slopefracrow
      real, pointer, dimension(:,:) :: ch4wet1row
      real, pointer, dimension(:,:) :: ch4wet2row
      real, pointer, dimension(:,:) :: wetfdynrow
      real, pointer, dimension(:,:) :: ch4dyn1row
      real, pointer, dimension(:,:) :: ch4dyn2row
      real, pointer, dimension(:,:,:) :: wetfrac_monrow
      real, pointer, dimension(:,:) :: ch4soillsrow

      real, pointer, dimension(:,:) :: lucemcomrow
      real, pointer, dimension(:,:) :: lucltrinrow
      real, pointer, dimension(:,:) :: lucsocinrow

      real, pointer, dimension(:,:) :: npprow
      real, pointer, dimension(:,:) :: neprow
      real, pointer, dimension(:,:) :: nbprow
      real, pointer, dimension(:,:) :: gpprow
      real, pointer, dimension(:,:) :: hetroresrow
      real, pointer, dimension(:,:) :: autoresrow
      real, pointer, dimension(:,:) :: soilcresprow
      real, pointer, dimension(:,:) :: rmrow
      real, pointer, dimension(:,:) :: rgrow
      real, pointer, dimension(:,:) :: litresrow
      real, pointer, dimension(:,:) :: socresrow
      real, pointer, dimension(:,:) :: dstcemlsrow
      real, pointer, dimension(:,:) :: litrfallrow
      real, pointer, dimension(:,:) :: humiftrsrow

      real, pointer, dimension(:,:,:) :: gppvegrow
      real, pointer, dimension(:,:,:) :: nepvegrow
      real, pointer, dimension(:,:,:) :: nbpvegrow
      real, pointer, dimension(:,:,:) :: nppvegrow
      real, pointer, dimension(:,:,:) :: hetroresvegrow
      real, pointer, dimension(:,:,:) :: autoresvegrow
      real, pointer, dimension(:,:,:) :: litresvegrow
      real, pointer, dimension(:,:,:) :: soilcresvegrow
      real, pointer, dimension(:,:,:) :: rmlvegaccrow
      real, pointer, dimension(:,:,:) :: rmsvegrow
      real, pointer, dimension(:,:,:) :: rmrvegrow
      real, pointer, dimension(:,:,:) :: rgvegrow
      real, pointer, dimension(:,:,:) :: litrfallvegrow
      real, pointer, dimension(:,:,:) :: humiftrsvegrow

      real, pointer, dimension(:,:,:) :: rothrlosrow
      real, pointer, dimension(:,:,:) :: pfcancmxrow
      real, pointer, dimension(:,:,:) :: nfcancmxrow
      real, pointer, dimension(:,:,:) :: alvsctmrow
      real, pointer, dimension(:,:,:) :: paicrow
      real, pointer, dimension(:,:,:) :: slaicrow
      real, pointer, dimension(:,:,:) :: alirctmrow
      real, pointer, dimension(:,:) :: cfluxcgrow
      real, pointer, dimension(:,:) :: cfluxcsrow
      real, pointer, dimension(:,:) :: dstcemls3row
      real, pointer, dimension(:,:,:) :: anvegrow
      real, pointer, dimension(:,:,:) :: rmlvegrow

      real, pointer, dimension(:,:) :: twarmmrow
      real, pointer, dimension(:,:) :: tcoldmrow
      real, pointer, dimension(:,:) :: gdd5row
      real, pointer, dimension(:,:) :: aridityrow
      real, pointer, dimension(:,:) :: srplsmonrow
      real, pointer, dimension(:,:) :: defctmonrow
      real, pointer, dimension(:,:) :: anndefctrow
      real, pointer, dimension(:,:) :: annsrplsrow
      real, pointer, dimension(:,:) :: annpcprow
      real, pointer, dimension(:,:) :: dry_season_lengthrow

      logical, pointer, dimension(:) :: calsoil
      integer, pointer, dimension(:) :: ndaytime

      real, pointer, dimension(:,:,:) :: rnleafrow
      real, pointer, dimension(:,:,:) :: rnstemrow
      real, pointer, dimension(:,:,:) :: rnrootrow
      real, pointer, dimension(:,:,:) :: rnlitrrow
      real, pointer, dimension(:,:,:) :: rnsomrow
      real, pointer, dimension(:,:,:) :: snh4row
      real, pointer, dimension(:,:,:) :: sno3row
      real, pointer, dimension(:,:,:) :: nrubrow
      real, pointer, dimension(:,:,:) :: nrub0row

      real, pointer, dimension(:,:,:) :: vcmx0row
      real, pointer, dimension(:,:) :: btdpthrow

      real, pointer, dimension(:,:) :: dndeprow
      real, pointer, dimension(:,:) :: dnferrow
      real, pointer, dimension(:,:) :: dnpltrrow
      real, pointer, dimension(:,:) :: dndisrow 
      real, pointer, dimension(:,:) :: dnlsomrow  
      real, pointer, dimension(:,:) :: dnminrow  
      real, pointer, dimension(:,:) :: dnnitrow
      real, pointer, dimension(:,:) :: dnpuprow 
      real, pointer, dimension(:,:) :: dndnitrow
      real, pointer, dimension(:,:) :: dnlearow 
      real, pointer, dimension(:,:) :: dnvolrow   
      real, pointer, dimension(:,:) :: dnsorrow     
      real, pointer, dimension(:,:) :: dnlosrow   
      real, pointer, dimension(:,:) :: n2ototrow  
      real, pointer, dimension(:,:) :: n2totrow  
      real, pointer, dimension(:,:) :: dnbfixrow  
      real, pointer, dimension(:,:) :: dnplossrow  
      real, pointer, dimension(:,:) :: dnslossrow 

      real, pointer, dimension(:,:) :: etprow  
      real, pointer, dimension(:,:) :: xminf  
      real, pointer, dimension(:,:) :: xminfbar


      ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
      ! GAT version:

      logical, pointer, dimension(:,:) :: pftexistgat
      integer, pointer, dimension(:,:) :: colddaysgat
      integer, pointer, dimension(:) :: icountgat
      integer, pointer, dimension(:,:) :: lfstatusgat
      integer, pointer, dimension(:,:) :: pandaysgat
      integer, pointer, dimension(:) :: stdalngat
      real, pointer, dimension(:) :: lightng

      real, pointer, dimension(:,:) :: ailcmingat      
      real, pointer, dimension(:,:) :: ailcmaxgat         
      real, pointer, dimension(:,:) :: dvdfcangat         
      real, pointer, dimension(:,:) :: gleafmasgat        
      real, pointer, dimension(:,:) :: bleafmasgat        
      real, pointer, dimension(:,:) :: stemmassgat        
      real, pointer, dimension(:,:) :: rootmassgat        
      real, pointer, dimension(:,:) :: pstemmassgat       
      real, pointer, dimension(:,:) :: pgleafmassgat      
      real, pointer, dimension(:,:) :: fcancmxgat
      real, pointer, dimension(:) :: gavglaigat
      real, pointer, dimension(:,:) :: zolncgat
      real, pointer, dimension(:,:) :: ailcgat
      real, pointer, dimension(:,:) :: ailcggat
      real, pointer, dimension(:,:) :: ailcgsgat
      real, pointer, dimension(:,:) :: fcancsgat
      real, pointer, dimension(:,:) :: fcancgat
      real, pointer, dimension(:) :: co2concgat
      real, pointer, dimension(:) :: ch4concgat
      real, pointer, dimension(:,:) :: co2i1cggat
      real, pointer, dimension(:,:) :: co2i1csgat
      real, pointer, dimension(:,:) :: co2i2cggat
      real, pointer, dimension(:,:) :: co2i2csgat
      real, pointer, dimension(:,:) :: ancsveggat
      real, pointer, dimension(:,:) :: ancgveggat
      real, pointer, dimension(:,:) :: rmlcsveggat
      real, pointer, dimension(:,:) :: rmlcgveggat
      real, pointer, dimension(:,:) :: slaigat
      real, pointer, dimension(:,:) :: ailcbgat
      real, pointer, dimension(:) :: canresgat
      real, pointer, dimension(:,:) :: flhrlossgat

      real, pointer, dimension(:,:) :: grwtheffgat
      real, pointer, dimension(:,:) :: lystmmasgat
      real, pointer, dimension(:,:) :: lyrotmasgat
      real, pointer, dimension(:,:) :: tymaxlaigat
      real, pointer, dimension(:) :: vgbiomasgat
      real, pointer, dimension(:) :: gavgltmsgat
      real, pointer, dimension(:) :: gavgscmsgat
      real, pointer, dimension(:,:) :: stmhrlosgat
      real, pointer, dimension(:,:,:) :: rmatcgat
      real, pointer, dimension(:,:,:) :: rmatctemgat
      real, pointer, dimension(:,:) :: litrmassgat
      real, pointer, dimension(:,:) :: soilcmasgat
      real, pointer, dimension(:,:) :: vgbiomas_veggat

      real, pointer, dimension(:,:) :: emit_co2gat
      real, pointer, dimension(:,:) :: emit_cogat
      real, pointer, dimension(:,:) :: emit_ch4gat
      real, pointer, dimension(:,:) :: emit_nmhcgat
      real, pointer, dimension(:,:) :: emit_h2gat
      real, pointer, dimension(:,:) :: emit_noxgat
      real, pointer, dimension(:,:) :: emit_n2ogat
      real, pointer, dimension(:,:) :: emit_pm25gat
      real, pointer, dimension(:,:) :: emit_tpmgat
      real, pointer, dimension(:,:) :: emit_tcgat
      real, pointer, dimension(:,:) :: emit_ocgat
      real, pointer, dimension(:,:) :: emit_bcgat
      real, pointer, dimension(:) :: burnfracgat
      real, pointer, dimension(:,:) :: burnvegfgat
      real, pointer, dimension(:,:) :: smfuncveggat
      real, pointer, dimension(:) :: popdingat
      real, pointer, dimension(:,:) :: btermgat
      real, pointer, dimension(:) :: ltermgat
      real, pointer, dimension(:,:) :: mtermgat

      real, pointer, dimension(:) :: extnprobgat
      real, pointer, dimension(:) :: prbfrhucgat
      real, pointer, dimension(:,:) :: mlightnggat
      real, pointer, dimension(:) :: dayl_maxgat
      real, pointer, dimension(:) :: daylgat

      real, pointer, dimension(:,:) :: bmasveggat
      real, pointer, dimension(:,:) :: cmasvegcgat
      real, pointer, dimension(:,:) :: veghghtgat
      real, pointer, dimension(:,:) :: rootdpthgat
      real, pointer, dimension(:) :: rmlgat
      real, pointer, dimension(:) :: rmsgat
      real, pointer, dimension(:,:) :: tltrleafgat
      real, pointer, dimension(:,:) :: tltrstemgat
      real, pointer, dimension(:,:) :: tltrrootgat
      real, pointer, dimension(:,:) :: leaflitrgat
      real, pointer, dimension(:,:) :: roottempgat
      real, pointer, dimension(:,:) :: afrleafgat
      real, pointer, dimension(:,:) :: afrstemgat
      real, pointer, dimension(:,:) :: afrrootgat
      real, pointer, dimension(:,:) :: wtstatusgat
      real, pointer, dimension(:,:) :: ltstatusgat
      real, pointer, dimension(:) :: rmrgat

      real, pointer, dimension(:,:) :: slopefracgat
      real, pointer, dimension(:) :: wetfrac_presgat
      real, pointer, dimension(:,:) :: wetfrac_mongat
      real, pointer, dimension(:) :: ch4wet1gat
      real, pointer, dimension(:) :: ch4wet2gat
      real, pointer, dimension(:) :: wetfdyngat
      real, pointer, dimension(:) :: ch4dyn1gat
      real, pointer, dimension(:) :: ch4dyn2gat
      real, pointer, dimension(:) :: ch4soillsgat

      real, pointer, dimension(:) :: lucemcomgat
      real, pointer, dimension(:) :: lucltringat
      real, pointer, dimension(:) :: lucsocingat

      real, pointer, dimension(:) :: nppgat
      real, pointer, dimension(:) :: nepgat
      real, pointer, dimension(:) :: nbpgat
      real, pointer, dimension(:) :: gppgat
      real, pointer, dimension(:) :: hetroresgat
      real, pointer, dimension(:) :: autoresgat
      real, pointer, dimension(:) :: soilcrespgat
      real, pointer, dimension(:) :: rmgat
      real, pointer, dimension(:) :: rggat
      real, pointer, dimension(:) :: litresgat
      real, pointer, dimension(:) :: socresgat
      real, pointer, dimension(:) :: dstcemlsgat
      real, pointer, dimension(:) :: litrfallgat
      real, pointer, dimension(:) :: humiftrsgat

      real, pointer, dimension(:,:) :: gppveggat
      real, pointer, dimension(:,:) :: nepveggat
      real, pointer, dimension(:,:) :: nbpveggat
      real, pointer, dimension(:,:) :: nppveggat
      real, pointer, dimension(:,:) :: hetroresveggat
      real, pointer, dimension(:,:) :: autoresveggat
      real, pointer, dimension(:,:) :: litresveggat
      real, pointer, dimension(:,:) :: soilcresveggat
      real, pointer, dimension(:,:) :: rmlvegaccgat
      real, pointer, dimension(:,:) :: rmsveggat
      real, pointer, dimension(:,:) :: rmrveggat
      real, pointer, dimension(:,:) :: rgveggat
      real, pointer, dimension(:,:) :: litrfallveggat
      real, pointer, dimension(:,:) :: humiftrsveggat

      real, pointer, dimension(:,:) :: rothrlosgat
      real, pointer, dimension(:,:) :: pfcancmxgat
      real, pointer, dimension(:,:) :: nfcancmxgat
      real, pointer, dimension(:,:) :: alvsctmgat
      real, pointer, dimension(:,:) :: paicgat
      real, pointer, dimension(:,:) :: slaicgat
      real, pointer, dimension(:,:) :: alirctmgat
      real, pointer, dimension(:) :: cfluxcggat
      real, pointer, dimension(:) :: cfluxcsgat
      real, pointer, dimension(:) :: dstcemls3gat
      real, pointer, dimension(:,:) :: anveggat
      real, pointer, dimension(:,:) :: rmlveggat

      real, pointer, dimension(:) :: twarmmgat
      real, pointer, dimension(:) :: tcoldmgat
      real, pointer, dimension(:) :: gdd5gat
      real, pointer, dimension(:) :: ariditygat
      real, pointer, dimension(:) :: srplsmongat
      real, pointer, dimension(:) :: defctmongat
      real, pointer, dimension(:) :: anndefctgat
      real, pointer, dimension(:) :: annsrplsgat
      real, pointer, dimension(:) :: annpcpgat
      real, pointer, dimension(:) :: dry_season_lengthgat

      real, pointer, dimension(:) :: tcurm
      real, pointer, dimension(:) :: srpcuryr
      real, pointer, dimension(:) :: dftcuryr
      real, pointer, dimension(:,:) :: tmonth
      real, pointer, dimension(:) :: anpcpcur
      real, pointer, dimension(:) :: anpecur
      real, pointer, dimension(:) :: gdd5cur
      real, pointer, dimension(:) :: surmncur
      real, pointer, dimension(:) :: defmncur
      real, pointer, dimension(:) :: srplscur
      real, pointer, dimension(:) :: defctcur

      real, pointer, dimension(:,:) :: geremortgat
      real, pointer, dimension(:,:) :: intrmortgat
      real, pointer, dimension(:,:) :: lambdagat
      real, pointer, dimension(:,:) :: ccgat
      real, pointer, dimension(:,:) :: mmgat
      integer, pointer, dimension(:) :: altotcntr_d

      real, pointer, dimension(:,:) :: rnleafgat
      real, pointer, dimension(:,:) :: rnstemgat
      real, pointer, dimension(:,:) :: rnrootgat
      real, pointer, dimension(:,:) :: rnlitrgat
      real, pointer, dimension(:,:) :: rnsomgat
      real, pointer, dimension(:,:) :: snh4gat
      real, pointer, dimension(:,:) :: sno3gat
      real, pointer, dimension(:,:) :: nrubgat
      real, pointer, dimension(:,:) :: nrub0gat

      real, pointer, dimension(:,:) :: vcmx0gat
      real, pointer, dimension(:) :: btdpthgat

      real, pointer, dimension(:) :: dndepgat
      real, pointer, dimension(:) :: dnfergat
      real, pointer, dimension(:) :: dnpltrgat
      real, pointer, dimension(:) :: dndisgat 
      real, pointer, dimension(:) :: dnlsomgat  
      real, pointer, dimension(:) :: dnmingat  
      real, pointer, dimension(:) :: dnnitgat
      real, pointer, dimension(:) :: dnpupgat 
      real, pointer, dimension(:) :: dndnitgat
      real, pointer, dimension(:) :: dnleagat 
      real, pointer, dimension(:) :: dnvolgat   
      real, pointer, dimension(:) :: dnsorgat     
      real, pointer, dimension(:) :: dnlosgat   
      real, pointer, dimension(:) :: n2ototgat  
      real, pointer, dimension(:) :: n2totgat  
      real, pointer, dimension(:) :: dnbfixgat  
      real, pointer, dimension(:) :: dnplossgat  
      real, pointer, dimension(:) :: dnslossgat 

      real, pointer, dimension(:) :: etpgat
      real, pointer, dimension(:) :: etpacc

      real, pointer, dimension(:) :: ovraccgat_t
      real, pointer, dimension(:) :: rofaccgat_t 


      ! Mosaic level:

      real, pointer, dimension(:,:) :: PREACC_t
      real, pointer, dimension(:,:) :: GTACC_t
      real, pointer, dimension(:,:) :: QEVPACC_t
      real, pointer, dimension(:,:) :: HFSACC_t
      real, pointer, dimension(:,:) :: HMFNACC_t
      real, pointer, dimension(:,:) :: ROFACC_t
      real, pointer, dimension(:,:) :: SNOACC_t
      real, pointer, dimension(:,:) :: OVRACC_t
      real, pointer, dimension(:,:) :: WTBLACC_t
      real, pointer, dimension(:,:,:) :: TBARACC_t
      real, pointer, dimension(:,:,:) :: THLQACC_t
      real, pointer, dimension(:,:,:) :: THICACC_t
      real, pointer, dimension(:,:,:) :: THALACC_t
      real, pointer, dimension(:,:) :: ALVSACC_t
      real, pointer, dimension(:,:) :: ALIRACC_t
      real, pointer, dimension(:,:) :: RHOSACC_t
      real, pointer, dimension(:,:) :: TSNOACC_t
      real, pointer, dimension(:,:) :: WSNOACC_t
      real, pointer, dimension(:,:) :: SNOARE_t
      real, pointer, dimension(:,:) :: TCANACC_t
      real, pointer, dimension(:,:) :: RCANACC_t
      real, pointer, dimension(:,:) :: SCANACC_t
      real, pointer, dimension(:,:) :: GROACC_t
      real, pointer, dimension(:,:) :: FSINACC_t
      real, pointer, dimension(:,:) :: FLINACC_t
      real, pointer, dimension(:,:) :: TAACC_t
      real, pointer, dimension(:,:) :: UVACC_t
      real, pointer, dimension(:,:) :: PRESACC_t
      real, pointer, dimension(:,:) :: QAACC_t
      real, pointer, dimension(:,:) :: ALTOTACC_t
      real, pointer, dimension(:,:) :: EVAPACC_t
      real, pointer, dimension(:,:) :: FLUTACC_t


!      Outputs

       real, pointer, dimension(:,:) :: tcanoaccrow_out
       real, pointer, dimension(:) :: tcanoaccgat_out
       real, pointer, dimension(:,:) :: qevpacc_m_save

!     -----------------------
!      Tile-level variables (denoted by an ending of "_t")

      real, pointer, dimension(:) :: fsnowacc_t
      real, pointer, dimension(:) :: tcansacc_t
      real, pointer, dimension(:) :: tcanoaccgat_t
      real, pointer, dimension(:) :: taaccgat_t
      real, pointer, dimension(:) :: uvaccgat_t
      real, pointer, dimension(:) :: vvaccgat_t
      real, pointer, dimension(:,:) :: tbaraccgat_t
      real, pointer, dimension(:,:) :: tbarcacc_t
      real, pointer, dimension(:,:) :: tbarcsacc_t
      real, pointer, dimension(:,:) :: tbargacc_t
      real, pointer, dimension(:,:) :: tbargsacc_t
      real, pointer, dimension(:,:) :: thliqcacc_t
      real, pointer, dimension(:,:) :: thliqgacc_t
      real, pointer, dimension(:,:) :: thliqacc_t
      real, pointer, dimension(:,:) :: thicecacc_t
      real, pointer, dimension(:,:) :: thicegacc_t
      real, pointer, dimension(:,:) :: ancsvgac_t
      real, pointer, dimension(:,:) :: ancgvgac_t
      real, pointer, dimension(:,:) :: rmlcsvga_t
      real, pointer, dimension(:,:) :: rmlcgvga_t

!     -----------------------
!     Grid-averaged variables (denoted with an ending of "_g")

      real, pointer, dimension(:) ::  fsstar_g
      real, pointer, dimension(:) ::  flstar_g
      real, pointer, dimension(:) ::  qh_g
      real, pointer, dimension(:) ::  qe_g
      real, pointer, dimension(:) ::  snomlt_g
      real, pointer, dimension(:) ::  beg_g
      real, pointer, dimension(:) ::  gtout_g
      real, pointer, dimension(:) ::  tpn_g
      real, pointer, dimension(:) ::  altot_g
      real, pointer, dimension(:) ::  tcn_g
      real, pointer, dimension(:) ::  tsn_g
      real, pointer, dimension(:) ::  zsn_g

      real, pointer, dimension(:) :: WSNOROT_g
      real, pointer, dimension(:) :: ROFSROT_g
      real, pointer, dimension(:) :: SNOROT_g
      real, pointer, dimension(:) :: RHOSROT_g
      real, pointer, dimension(:) :: ROFROT_g
      real, pointer, dimension(:) :: ZPNDROT_g
      real, pointer, dimension(:) :: RCANROT_g
      real, pointer, dimension(:) :: SCANROT_g
      real, pointer, dimension(:) :: TROFROT_g
      real, pointer, dimension(:) :: TROOROT_g
      real, pointer, dimension(:) :: TROBROT_g
      real, pointer, dimension(:) :: ROFOROT_g
      real, pointer, dimension(:) :: ROFBROT_g
      real, pointer, dimension(:) :: TROSROT_g
      real, pointer, dimension(:) :: FSGVROT_g
      real, pointer, dimension(:) :: FSGSROT_g
      real, pointer, dimension(:) :: FLGVROT_g
      real, pointer, dimension(:) :: FLGSROT_g
      real, pointer, dimension(:) :: HFSCROT_g
      real, pointer, dimension(:) :: HFSSROT_g
      real, pointer, dimension(:) :: HEVCROT_g
      real, pointer, dimension(:) :: HEVSROT_g
      real, pointer, dimension(:) :: HMFCROT_g
      real, pointer, dimension(:) :: HMFNROT_g
      real, pointer, dimension(:) :: HTCSROT_g
      real, pointer, dimension(:) :: HTCCROT_g
      real, pointer, dimension(:) :: FSGGROT_g
      real, pointer, dimension(:) :: FLGGROT_g
      real, pointer, dimension(:) :: HFSGROT_g
      real, pointer, dimension(:) :: HEVGROT_g
      real, pointer, dimension(:) :: CDHROT_g
      real, pointer, dimension(:) :: CDMROT_g
      real, pointer, dimension(:) :: SFCUROT_g
      real, pointer, dimension(:) :: SFCVROT_g
      real, pointer, dimension(:) :: fc_g
      real, pointer, dimension(:) :: fg_g
      real, pointer, dimension(:) :: fcs_g
      real, pointer, dimension(:) :: fgs_g
      real, pointer, dimension(:) :: PCFCROT_g
      real, pointer, dimension(:) :: PCLCROT_g
      real, pointer, dimension(:) :: PCPGROT_g
      real, pointer, dimension(:) :: QFCFROT_g
      real, pointer, dimension(:) :: QFGROT_g
      real, pointer, dimension(:,:) :: QFCROT_g
      real, pointer, dimension(:) :: ROFCROT_g
      real, pointer, dimension(:) :: ROFNROT_g
      real, pointer, dimension(:) :: WTRSROT_g
      real, pointer, dimension(:) :: WTRGROT_g
      real, pointer, dimension(:) :: PCPNROT_g
      real, pointer, dimension(:) :: QFCLROT_g
      real, pointer, dimension(:) :: QFNROT_g
      real, pointer, dimension(:) :: WTRCROT_g
      real, pointer, dimension(:,:) :: rmlvegrow_g
      real, pointer, dimension(:,:) :: anvegrow_g
      real, pointer, dimension(:,:) :: HMFGROT_g
      real, pointer, dimension(:,:) :: HTCROT_g
      real, pointer, dimension(:,:) :: TBARROT_g
      real, pointer, dimension(:,:) :: THLQROT_g
      real, pointer, dimension(:,:) :: THICROT_g
      real, pointer, dimension(:,:) :: GFLXROT_g

! Model Switches (rarely changed ones only! The rest are in joboptions file):

      logical, parameter :: obslght = .false.  ! if true the observed lightning will be used. False means you will use the
                                             ! lightning climatology from the CTM file. This was brought in for FireMIP runs.

   ! If you intend to have LUC BETWEEN tiles then set this to true:
      logical, parameter ::  onetile_perPFT = .False. ! NOTE: This is usually not the behaviour desired unless you are
                                                   ! running with one PFT on each tile and want them to compete for space
                                                   ! across tiles. In general keep this as False. JM Feb 2016.

!     leap year flag (if the switch 'leap' is true, this will be used, otherwise it remains false)
      logical :: leapnow = .false.
c
c============= CTEM array declaration done =============================/
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
      COMMON /CLASSD2/ AS,ASX,CI,BS,BETA,FACTN,HMIN,ANGMAX


!Once the variable declarations end, we will then define the subroutines
!This way all of the subroutines within CONTAINS will have access to the variables declared within the module
       CONTAINS

       SUBROUTINE RUNCLASS36CTEM_init(shd, fls, cm)		

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use FLAGS


        !Declaration of the MESH variables used to pass information


        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !Here we need to use the NLAT and NMOS values (from ctem_params)
        !and transform them into the usable parameters of NLTEST and NMTEST
        !Note that NLAT and NMOS is CTEM, NLTEST and NMTEST is CLASS and 
!DAN    Set NLTEST to NAA which is the ACTIVE number of grids -- in grid-based setups,
!DAN    this excludes the outlet, such that NAA might be NA-1 if only one outlet exists.
!DAN    Doing this solves problems in CTEM where values in cell NA are sometimes zero.
        NLTEST = shd%NAA
        NMTEST = shd%lc%NTYPE
        NML = shd%lc%NML




!==================================================
!Temparary fix for CLASSZ bug, must remove later
        IF(NML.gt.1000) THEN
        NML=1142
        ENDIF
!=================================================


        NMW = shd%wc%NML


        !ctem_params
        nlat = shd%NA
        nmos = shd%lc%NTYPE
        ilg = nlat*nmos
        ignd = shd%lc%IGND


        !Allocate vectors.
        allocate(IWF(ILG))
        allocate(PAICAN(ILG))
        allocate(ALBSGAT(ILG))
        allocate(ALBSROT(NLAT,NMOS))
        allocate(CMAIGAT(ILG))
        allocate(CMAIROT(NLAT,NMOS))
        allocate(GROGAT(ILG))
        allocate(GROROT(NLAT,NMOS))
        allocate(QACGAT(ILG))
        allocate(QACROT(NLAT,NMOS))
        allocate(RCANGAT(ILG))
        allocate(RCANROT(NLAT,NMOS))
        allocate(RHOSGAT(ILG))
        allocate(RHOSROT(NLAT,NMOS))
        allocate(SCANGAT(ILG))
        allocate(SCANROT(NLAT,NMOS))
        allocate(SNOGAT(ILG))
        allocate(SNOROT(NLAT,NMOS))
        allocate(TACGAT(ILG))
        allocate(TACROT(NLAT,NMOS))
        allocate(TBARGAT(ILG,IGND))
        allocate(TBARROT(NLAT,NMOS,IGND))
        allocate(TBASGAT(ILG))
        allocate(TBASROT(NLAT,NMOS))
        allocate(TCANGAT(ILG))
        allocate(TCANROT(NLAT,NMOS))
        allocate(THICGAT(ILG,IGND))
        allocate(THICROT(NLAT,NMOS,IGND))
        allocate(THLQGAT(ILG,IGND))
        allocate(THLQROT(NLAT,NMOS,IGND))
        allocate(TPNDGAT(ILG))
        allocate(TPNDROT(NLAT,NMOS))
        allocate(TSFSGAT(ILG,4))
        allocate(TSFSROT(NLAT,NMOS,4))
        allocate(TSNOGAT(ILG))
        allocate(TSNOROT(NLAT,NMOS))
        allocate(WSNOGAT(ILG))
        allocate(WSNOROT(NLAT,NMOS))
        allocate(ZPNDGAT(ILG))
        allocate(ZPNDROT(NLAT,NMOS))

        allocate(REFROT(NLAT,NMOS))
        allocate(BCSNROT(NLAT,NMOS))
        allocate(REFGAT(ILG))
        allocate(BCSNGAT(ILG))

        allocate(ILMOS(ILG))
        allocate(JLMOS(ILG))
        allocate(IWMOS(ILG))
        allocate(JWMOS(ILG))

        allocate(ACIDGAT(ILG,ICAN))
        allocate(ACIDROT(NLAT,NMOS,ICAN))
        allocate(ACVDGAT(ILG,ICAN))
        allocate(ACVDROT(NLAT,NMOS,ICAN))
        allocate(AGIDGAT(ILG))
        allocate(AGIDROT(NLAT,NMOS))
        allocate(AGVDGAT(ILG))
        allocate(AGVDROT(NLAT,NMOS))
        allocate(ALGDGAT(ILG))
        allocate(ALGDROT(NLAT,NMOS))
        allocate(ALGWGAT(ILG))
        allocate(ALGWROT(NLAT,NMOS))
        allocate(ALICGAT(ILG,ICP1))
        allocate(ALICROT(NLAT,NMOS,ICP1))
        allocate(ALVCGAT(ILG,ICP1))
        allocate(ALVCROT(NLAT,NMOS,ICP1))
        allocate(ASIDGAT(ILG))
        allocate(ASIDROT(NLAT,NMOS))
        allocate(ASVDGAT(ILG))
        allocate(ASVDROT(NLAT,NMOS))
        allocate(BIGAT(ILG,IGND))
        allocate(BIROT(NLAT,NMOS,IGND))
        allocate(CLAYROT(NLAT,NMOS,IGND))
        allocate(CMASGAT(ILG,ICAN))
        allocate(CMASROT(NLAT,NMOS,ICAN))
        allocate(DDGAT(ILG))
        allocate(DDROT(NLAT,NMOS))
        allocate(DLZWGAT(ILG,IGND))
        allocate(DLZWROT(NLAT,NMOS,IGND))
        allocate(DRNGAT(ILG))
        allocate(DRNROT(NLAT,NMOS))
        allocate(FAREROT(NLAT,NMOS))
        allocate(FCANGAT(ILG,ICP1))
        allocate(FCANROT(NLAT,NMOS,ICP1))
        allocate(GRKFGAT(ILG))
        allocate(GRKFROT(NLAT,NMOS))
        allocate(GRKSGAT(ILG,IGND))
        allocate(GRKSROT(NLAT,NMOS,IGND))
        allocate(HCPSGAT(ILG,IGND))
        allocate(HCPSROT(NLAT,NMOS,IGND))
        allocate(HGTDGAT(ILG,ICAN))
        allocate(HGTDROT(NLAT,NMOS,ICAN))
        allocate(IGDRGAT(ILG))
        allocate(IGDRROT(NLAT,NMOS))
        allocate(ISNDGAT(ILG,IGND))
        allocate(ISNDROT(NLAT,NMOS,IGND))
        allocate(LNZ0GAT(ILG,ICP1))
        allocate(LNZ0ROT(NLAT,NMOS,ICP1))
        allocate(MIDROT(NLAT,NMOS))
        allocate(ORGMROT(NLAT,NMOS,IGND))
        allocate(PAIDGAT(ILG,ICAN))
        allocate(PAIDROT(NLAT,NMOS,ICAN))
        allocate(PAMNGAT(ILG,ICAN))
        allocate(PAMNROT(NLAT,NMOS,ICAN))
        allocate(PAMXGAT(ILG,ICAN))
        allocate(PAMXROT(NLAT,NMOS,ICAN))
        allocate(PSGAGAT(ILG,ICAN))
        allocate(PSGAROT(NLAT,NMOS,ICAN))
        allocate(PSGBGAT(ILG,ICAN))
        allocate(PSGBROT(NLAT,NMOS,ICAN))
        allocate(PSISGAT(ILG,IGND))
        allocate(PSISROT(NLAT,NMOS,IGND))
        allocate(PSIWGAT(ILG,IGND))
        allocate(PSIWROT(NLAT,NMOS,IGND))
        allocate(QA50GAT(ILG,ICAN))
        allocate(QA50ROT(NLAT,NMOS,ICAN))
        allocate(ROOTGAT(ILG,ICAN))
        allocate(ROOTROT(NLAT,NMOS,ICAN))
        allocate(RSMNGAT(ILG,ICAN))
        allocate(RSMNROT(NLAT,NMOS,ICAN))
        allocate(SANDROT(NLAT,NMOS,IGND))
        allocate(SDEPROT(NLAT,NMOS))
        allocate(TCSGAT(ILG,IGND))
        allocate(TCSROT(NLAT,NMOS,IGND))
        allocate(THFCGAT(ILG,IGND))
        allocate(THFCROT(NLAT,NMOS,IGND))
        allocate(THMGAT(ILG,IGND))
        allocate(THMROT(NLAT,NMOS,IGND))
        allocate(THPGAT(ILG,IGND))
        allocate(THPROT(NLAT,NMOS,IGND))
        allocate(THRGAT(ILG,IGND))
        allocate(THRROT(NLAT,NMOS,IGND))
        allocate(THRAGAT(ILG,IGND))
        allocate(THRAROT(NLAT,NMOS,IGND))
        allocate(VPDAGAT(ILG,ICAN))
        allocate(VPDAROT(NLAT,NMOS,ICAN))
        allocate(VPDBGAT(ILG,ICAN))
        allocate(VPDBROT(NLAT,NMOS,ICAN))
        allocate(WFCIGAT(ILG))
        allocate(WFCIROT(NLAT,NMOS))
        allocate(WFSFGAT(ILG))
        allocate(WFSFROT(NLAT,NMOS))
        allocate(XSLPGAT(ILG))
        allocate(XSLPROT(NLAT,NMOS))
        allocate(ZBTWGAT(ILG,IGND))
        allocate(ZBTWROT(NLAT,NMOS,IGND))
        allocate(ZPLGGAT(ILG))
        allocate(ZPLGROT(NLAT,NMOS))
        allocate(ZPLSGAT(ILG))
        allocate(ZPLSROT(NLAT,NMOS))
        allocate(ZSNLGAT(ILG))
        allocate(ZSNLROT(NLAT,NMOS))
C
        allocate(THLWROT(NLAT,NMOS,IGND))
        allocate(ZSNOROT(NLAT,NMOS))
        allocate(ALGWVROT(NLAT,NMOS))
        allocate(ALGWNROT(NLAT,NMOS))
        allocate(ALGDVROT(NLAT,NMOS))
        allocate(ALGDNROT(NLAT,NMOS))
        allocate(EMISROT(NLAT,NMOS))
        allocate(SALBROT(NLAT,NMOS,NBS))
        allocate(CSALROT(NLAT,NMOS,NBS))
        allocate(FSDBROL(NLAT,NBS))
        allocate(FSFBROL(NLAT,NBS))
        allocate(FSSBROL(NLAT,NBS))
C
        allocate(THLWGAT(ILG,IGND))
        allocate(ALGWVGAT(ILG))
        allocate(ALGWNGAT(ILG))
        allocate(ALGDVGAT(ILG))
        allocate(ALGDNGAT(ILG))
        allocate(EMISGAT(ILG))
        allocate(SOCIROT(NLAT,NMOS))
C
        allocate(FSDBGAT(ILG,NBS))
        allocate(FSFBGAT(ILG,NBS))
        allocate(FSSBGAT(ILG,NBS))
        allocate(SALBGAT(ILG,NBS))
        allocate(CSALGAT(ILG,NBS))
C
!-        THPORG(3)
!-        THRORG(3)
!-        THMORG(3)
!-        BORG(3)
!-        PSISORG(3)
!-        GRKSORG(3)
C
!-        CANEXT(ICAN)
!-        XLEAF(ICAN)
!-        ZORAT(ICAN)
        allocate(DELZ(IGND))
        allocate(ZBOT(IGND))
!-        GROWYR(18,4,2)
C
!A lot of variable dimensions of ROW were changed to NMOS instead of NLAT for MESH (since NLAT is always 1)
!DAN  Reverted this because NLAT is not always 1
        allocate(CSZGAT(ILG))
        allocate(CSZROW(NLAT))
        allocate(DLONGAT(ILG))
        allocate(DLONROW(NLAT))
        allocate(FCLOGAT(ILG))
        allocate(FCLOROW(NLAT))
        allocate(FDLGAT(ILG))
        allocate(FDLROW(NLAT))
        allocate(FSIHGAT(ILG))
        allocate(FSIHROW(NLAT))
        allocate(FSVHGAT(ILG))
        allocate(FSVHROW(NLAT))
        allocate(GCROW(NLAT))
        allocate(GGEOGAT(ILG))
        allocate(GGEOROW(NLAT))
        allocate(PADRGAT(ILG))
        allocate(PADRROW(NLAT))
        allocate(PREGAT(ILG))
        allocate(PREROW(NLAT))
        allocate(PRESGAT(ILG))
        allocate(PRESROW(NLAT))
        allocate(QAGAT(ILG))
        allocate(QAROW(NLAT))
        allocate(RADJGAT(ILG))
        allocate(RADJROW(NLAT))
        allocate(RHOAGAT(ILG))
        allocate(RHOAROW(NLAT))
        allocate(RHSIGAT(ILG))
        allocate(RHSIROW(NLAT))
        allocate(RPCPGAT(ILG))
        allocate(RPCPROW(NLAT))
        allocate(RPREROW(NLAT))
        allocate(SPCPGAT(ILG))
        allocate(SPCPROW(NLAT))
        allocate(SPREROW(NLAT))
        allocate(TAGAT(ILG))
        allocate(TAROW(NLAT))
        allocate(TADPGAT(ILG))
        allocate(TADPROW(NLAT))
        allocate(TRPCGAT(ILG))
        allocate(TRPCROW(NLAT))
        allocate(TSPCGAT(ILG))
        allocate(TSPCROW(NLAT))
        allocate(ULGAT(ILG))
        allocate(ULROW(NLAT))
        allocate(VLGAT(ILG))
        allocate(VLROW(NLAT))
        allocate(VMODGAT(ILG))
        allocate(VMODROW(NLAT))
        allocate(VPDGAT(ILG))
        allocate(VPDROW(NLAT))
        allocate(Z0ORGAT(ILG))
        allocate(ZBLDGAT(ILG))
        allocate(ZBLDROW(NLAT))
        allocate(ZDHGAT(ILG))
        allocate(ZDHROW(NLAT))
        allocate(ZDMGAT(ILG))
        allocate(ZDMROW(NLAT))
        allocate(ZRFHGAT(ILG))
        allocate(ZRFHROW(NLAT))
        allocate(ZRFMGAT(ILG))
        allocate(ZRFMROW(NLAT))
C
        allocate(UVROW(NLAT))
        allocate(XDIFFUS(NLAT))
        allocate(Z0ORROW(NLAT))
C
        allocate(DLATROW(NLAT))
        allocate(FSSROW(NLAT))
        allocate(PRENROW(NLAT))
        allocate(CLDTROW(NLAT))
        allocate(FSGROL(NLAT))
        allocate(FLGROL(NLAT))
        allocate(GUSTROL(NLAT))
        allocate(DEPBROW(NLAT))
C
        allocate(FSGGAT(ILG))
        allocate(FLGGAT(ILG))
        allocate(GUSTGAT(ILG))
        allocate(DEPBGAT(ILG))
        allocate(GTBS(ILG))
        allocate(SFCUBS(ILG))
        allocate(SFCVBS(ILG))
        allocate(USTARBS(ILG))
        allocate(TCSNOW(ILG))
        allocate(GSNOW(ILG))
C
        allocate(ALIRGAT(ILG))
        allocate(ALIRROT(NLAT,NMOS))
        allocate(ALIRROW(NLAT))
        allocate(ALVSGAT(ILG))
        allocate(ALVSROT(NLAT,NMOS))
        allocate(ALVSROW(NLAT))
        allocate(CDHGAT(ILG))
        allocate(CDHROT(NLAT,NMOS))
        allocate(CDHROW(NLAT))
        allocate(CDMGAT(ILG))
        allocate(CDMROT(NLAT,NMOS))
        allocate(CDMROW(NLAT))
        allocate(DRGAT(ILG))
        allocate(DRROT(NLAT,NMOS))
        allocate(DRROW(NLAT))
        allocate(EFGAT(ILG))
        allocate(EFROT(NLAT,NMOS))
        allocate(EFROW(NLAT))
        allocate(FLGGGAT(ILG))
        allocate(FLGGROT(NLAT,NMOS))
        allocate(FLGGROW(NLAT))
        allocate(FLGSGAT(ILG))
        allocate(FLGSROT(NLAT,NMOS))
        allocate(FLGSROW(NLAT))
        allocate(FLGVGAT(ILG))
        allocate(FLGVROT(NLAT,NMOS))
        allocate(FLGVROW(NLAT))
        allocate(FSGGGAT(ILG))
        allocate(FSGGROT(NLAT,NMOS))
        allocate(FSGGROW(NLAT))
        allocate(FSGSGAT(ILG))
        allocate(FSGSROT(NLAT,NMOS))
        allocate(FSGSROW(NLAT))
        allocate(FSGVGAT(ILG))
        allocate(FSGVROT(NLAT,NMOS))
        allocate(FSGVROW(NLAT))
        allocate(FSNOGAT(ILG))
        allocate(FSNOROT(NLAT,NMOS))
        allocate(FSNOROW(NLAT))
        allocate(GAGAT(ILG))
        allocate(GAROT(NLAT,NMOS))
        allocate(GAROW(NLAT))
        allocate(GFLXGAT(ILG,IGND))
        allocate(GFLXROT(NLAT,NMOS,IGND))
        allocate(GFLXROW(NLAT,IGND))
        allocate(GTGAT(ILG))
        allocate(GTROT(NLAT,NMOS))
        allocate(GTROW(NLAT))
        allocate(HBLGAT(ILG))
        allocate(HBLROT(NLAT,NMOS))
        allocate(HBLROW(NLAT))
        allocate(HEVCGAT(ILG))
        allocate(HEVCROT(NLAT,NMOS))
        allocate(HEVCROW(NLAT))
        allocate(HEVGGAT(ILG))
        allocate(HEVGROT(NLAT,NMOS))
        allocate(HEVGROW(NLAT))
        allocate(HEVSGAT(ILG))
        allocate(HEVSROT(NLAT,NMOS))
        allocate(HEVSROW(NLAT))
        allocate(HFSGAT(ILG))
        allocate(HFSROT(NLAT,NMOS))
        allocate(HFSROW(NLAT))
        allocate(HFSCGAT(ILG))
        allocate(HFSCROT(NLAT,NMOS))
        allocate(HFSCROW(NLAT))
        allocate(HFSGGAT(ILG))
        allocate(HFSGROT(NLAT,NMOS))
        allocate(HFSGROW(NLAT))
        allocate(HFSSGAT(ILG))
        allocate(HFSSROT(NLAT,NMOS))
        allocate(HFSSROW(NLAT))
        allocate(HMFCGAT(ILG))
        allocate(HMFCROT(NLAT,NMOS))
        allocate(HMFCROW(NLAT))
        allocate(HMFGGAT(ILG,IGND))
        allocate(HMFGROT(NLAT,NMOS,IGND))
        allocate(HMFGROW(NLAT,IGND))
        allocate(HMFNGAT(ILG))
        allocate(HMFNROT(NLAT,NMOS))
        allocate(HMFNROW(NLAT))
        allocate(HTCGAT(ILG,IGND))
        allocate(HTCROT(NLAT,NMOS,IGND))
        allocate(HTCROW(NLAT,IGND))
        allocate(HTCCGAT(ILG))
        allocate(HTCCROT(NLAT,NMOS))
        allocate(HTCCROW(NLAT))
        allocate(HTCSGAT(ILG))
        allocate(HTCSROT(NLAT,NMOS))
        allocate(HTCSROW(NLAT))
        allocate(ILMOGAT(ILG))
        allocate(ILMOROT(NLAT,NMOS))
        allocate(ILMOROW(NLAT))
        allocate(ISUM(6))
        allocate(ITCTGAT(ILG,6,50))
        allocate(ITCTROT(NLAT,NMOS,6,50))
        allocate(PCFCGAT(ILG))
        allocate(PCFCROT(NLAT,NMOS))
        allocate(PCFCROW(NLAT))
        allocate(PCLCGAT(ILG))
        allocate(PCLCROT(NLAT,NMOS))
        allocate(PCLCROW(NLAT))
        allocate(PCPGGAT(ILG))
        allocate(PCPGROT(NLAT,NMOS))
        allocate(PCPGROW(NLAT))
        allocate(PCPNGAT(ILG))
        allocate(PCPNROT(NLAT,NMOS))
        allocate(PCPNROW(NLAT))
        allocate(PETGAT(ILG))
        allocate(PETROT(NLAT,NMOS))
        allocate(PETROW(NLAT))
        allocate(QEVPGAT(ILG))
        allocate(QEVPROT(NLAT,NMOS))
        allocate(QEVPROW(NLAT))
        allocate(QFCGAT(ILG,IGND))
        allocate(QFCROT(NLAT,NMOS,IGND))
        allocate(QFCROW(NLAT,IGND))
        allocate(QFCFGAT(ILG))
        allocate(QFCFROT(NLAT,NMOS))
        allocate(QFCFROW(NLAT))
        allocate(QFCLGAT(ILG))
        allocate(QFCLROT(NLAT,NMOS))
        allocate(QFCLROW(NLAT))
        allocate(QFGGAT(ILG))
        allocate(QFGROT(NLAT,NMOS))
        allocate(QFGROW(NLAT))
        allocate(QFNGAT(ILG))
        allocate(QFNROT(NLAT,NMOS))
        allocate(QFNROW(NLAT))
        allocate(QFSGAT(ILG))
        allocate(QFSROT(NLAT,NMOS))
        allocate(QFSROW(NLAT))
        allocate(QFXGAT(ILG))
        allocate(QFXROT(NLAT,NMOS))
        allocate(QFXROW(NLAT))
        allocate(QGGAT(ILG))
        allocate(QGROT(NLAT,NMOS))
        allocate(QGROW(NLAT))
        allocate(ROFGAT(ILG))
        allocate(ROFROT(NLAT,NMOS))
        allocate(ROFROW(NLAT))
        allocate(ROFBGAT(ILG))
        allocate(ROFBROT(NLAT,NMOS))
        allocate(ROFBROW(NLAT))
        allocate(ROFCGAT(ILG))
        allocate(ROFCROT(NLAT,NMOS))
        allocate(ROFCROW(NLAT))
        allocate(ROFNGAT(ILG))
        allocate(ROFNROT(NLAT,NMOS))
        allocate(ROFNROW(NLAT))
        allocate(ROFOGAT(ILG))
        allocate(ROFOROT(NLAT,NMOS))
        allocate(ROFOROW(NLAT))
        allocate(ROFSGAT(ILG))
        allocate(ROFSROT(NLAT,NMOS))
        allocate(ROFSROW(NLAT))
        allocate(ROVGGAT(ILG))
        allocate(ROVGROT(NLAT,NMOS))
        allocate(ROVGROW(NLAT))
        allocate(SFCQGAT(ILG))
        allocate(SFCQROT(NLAT,NMOS))
        allocate(SFCQROW(NLAT))
        allocate(SFCTGAT(ILG))
        allocate(SFCTROT(NLAT,NMOS))
        allocate(SFCTROW(NLAT))
        allocate(SFCUGAT(ILG))
        allocate(SFCUROT(NLAT,NMOS))
        allocate(SFCUROW(NLAT))
        allocate(SFCVGAT(ILG))
        allocate(SFCVROT(NLAT,NMOS))
        allocate(SFCVROW(NLAT))
        allocate(TFXGAT(ILG))
        allocate(TFXROT(NLAT,NMOS))
        allocate(TFXROW(NLAT))
        allocate(TROBGAT(ILG))
        allocate(TROBROT(NLAT,NMOS))
        allocate(TROFGAT(ILG))
        allocate(TROFROT(NLAT,NMOS))
        allocate(TROOGAT(ILG))
        allocate(TROOROT(NLAT,NMOS))
        allocate(TROSGAT(ILG))
        allocate(TROSROT(NLAT,NMOS))
        allocate(UEGAT(ILG))
        allocate(UEROT(NLAT,NMOS))
        allocate(UEROW(NLAT))
        allocate(WTABGAT(ILG))
        allocate(WTABROT(NLAT,NMOS))
        allocate(WTABROW(NLAT))
        allocate(WTRCGAT(ILG))
        allocate(WTRCROT(NLAT,NMOS))
        allocate(WTRCROW(NLAT))
        allocate(WTRGGAT(ILG))
        allocate(WTRGROT(NLAT,NMOS))
        allocate(WTRGROW(NLAT))
        allocate(WTRSGAT(ILG))
        allocate(WTRSROT(NLAT,NMOS))
        allocate(WTRSROW(NLAT))
C
        allocate(QLWOGAT(ILG))
        allocate(SFRHGAT(ILG))
        allocate(SFRHROT(NLAT,NMOS))
        allocate(SFRHROW(NLAT))
C
        allocate(FTEMP(ILG))
        allocate(FVAP(ILG))
        allocate(RIB(ILG))
C
        allocate(ALIRACC(NLAT))
        allocate(ALVSACC(NLAT))
        allocate(EVAPACC(NLAT))
        allocate(FLINACC(NLAT))
        allocate(FLUTACC(NLAT))
        allocate(FSINACC(NLAT))
        allocate(GROACC(NLAT))
        allocate(GTACC(NLAT))
        allocate(HFSACC(NLAT))
        allocate(HMFNACC(NLAT))
        allocate(OVRACC(NLAT))
        allocate(PREACC(NLAT))
        allocate(PRESACC(NLAT))
        allocate(QAACC(NLAT))
        allocate(QEVPACC(NLAT))
        allocate(RCANACC(NLAT))
        allocate(RHOSACC(NLAT))
        allocate(ROFACC(NLAT))
        allocate(SCANACC(NLAT))
        allocate(SNOACC(NLAT))
        allocate(TAACC(NLAT))
        allocate(TBARACC(NLAT,IGND))
        allocate(THALACC(NLAT,IGND))
        allocate(THICACC(NLAT,IGND))
        allocate(THLQACC(NLAT,IGND))
        allocate(TCANACC(NLAT))
        allocate(TSNOACC(NLAT))
        allocate(UVACC(NLAT))
        allocate(WSNOACC(NLAT))
        allocate(WTBLACC(NLAT))
        allocate(ALTOTACC(NLAT))
C
        allocate(CANARE(NLAT))
        allocate(SNOARE(NLAT))
C
C     * ARRAYS DEFINED TO PASS INFORMATION BETWEEN THE THREE MAJOR
C     * SUBSECTIONS OF CLASS ("CLASSA", "CLASST" AND "CLASSW").
C
        allocate(TBARC(ILG,IGND))
        allocate(TBARG(ILG,IGND))
        allocate(TBARCS(ILG,IGND))
        allocate(TBARGS(ILG,IGND))
        allocate(THLIQC(ILG,IGND))
        allocate(THLIQG(ILG,IGND))
        allocate(THICEC(ILG,IGND))
        allocate(THICEG(ILG,IGND))
        allocate(FROOT(ILG,IGND))
        allocate(HCPC(ILG,IGND))
        allocate(HCPG(ILG,IGND))
        allocate(FROOTS(ILG,IGND))
        allocate(TCTOPC(ILG,IGND))
        allocate(TCBOTC(ILG,IGND))
        allocate(TCTOPG(ILG,IGND))
        allocate(TCBOTG(ILG,IGND))
C
        allocate(FC(ILG))
        allocate(FG(ILG))
        allocate(FCS(ILG))
        allocate(FGS(ILG))
        allocate(RBCOEF(ILG))
        allocate(ZSNOW(ILG))
        allocate(FSVF(ILG))
        allocate(FSVFS(ILG))
        allocate(ALVSCN(ILG))
        allocate(ALIRCN(ILG))
        allocate(ALVSG(ILG))
        allocate(ALIRG(ILG))
        allocate(ALVSCS(ILG))
        allocate(ALIRCS(ILG))
        allocate(ALVSSN(ILG))
        allocate(ALIRSN(ILG))
        allocate(ALVSGC(ILG))
        allocate(ALIRGC(ILG))
        allocate(ALVSSC(ILG))
        allocate(ALIRSC(ILG))
        allocate(TRVSCN(ILG))
        allocate(TRIRCN(ILG))
        allocate(TRVSCS(ILG))
        allocate(TRIRCS(ILG))
        allocate(RC(ILG))
        allocate(RCS(ILG))
        allocate(FRAINC(ILG))
        allocate(FSNOWC(ILG))
        allocate(FRAICS(ILG))
        allocate(FSNOCS(ILG))
        allocate(CMASSC(ILG))
        allocate(CMASCS(ILG))
        allocate(DISP(ILG))
        allocate(DISPS(ILG))
        allocate(ZOMLNC(ILG))
        allocate(ZOELNC(ILG))
        allocate(ZOMLNG(ILG))
        allocate(ZOELNG(ILG))
        allocate(ZOMLCS(ILG))
        allocate(ZOELCS(ILG))
        allocate(ZOMLNS(ILG))
        allocate(ZOELNS(ILG))
        allocate(TRSNOWC(ILG))
        allocate(CHCAP(ILG))
        allocate(CHCAPS(ILG))
        allocate(GZEROC(ILG))
        allocate(GZEROG(ILG))
        allocate(GZROCS(ILG))
        allocate(GZROGS(ILG))
        allocate(G12C(ILG))
        allocate(G12G(ILG))
        allocate(G12CS(ILG))
        allocate(G12GS(ILG))
        allocate(G23C(ILG))
        allocate(G23G(ILG))
        allocate(G23CS(ILG))
        allocate(G23GS(ILG))
        allocate(QFREZC(ILG))
        allocate(QFREZG(ILG))
        allocate(QMELTC(ILG))
        allocate(QMELTG(ILG))
        allocate(EVAPC(ILG))
        allocate(EVAPCG(ILG))
        allocate(EVAPG(ILG))
        allocate(EVAPCS(ILG))
        allocate(EVPCSG(ILG))
        allocate(EVAPGS(ILG))
        allocate(TCANO(ILG))
        allocate(TCANS(ILG))
        allocate(RAICAN(ILG))
        allocate(SNOCAN(ILG))
        allocate(RAICNS(ILG))
        allocate(SNOCNS(ILG))
        allocate(CWLCAP(ILG))
        allocate(CWFCAP(ILG))
        allocate(CWLCPS(ILG))
        allocate(CWFCPS(ILG))
        allocate(TSNOCS(ILG))
        allocate(TSNOGS(ILG))
        allocate(RHOSCS(ILG))
        allocate(RHOSGS(ILG))
        allocate(WSNOCS(ILG))
        allocate(WSNOGS(ILG))
        allocate(TPONDC(ILG))
        allocate(TPONDG(ILG))
        allocate(TPNDCS(ILG))
        allocate(TPNDGS(ILG))
        allocate(ZPLMCS(ILG))
        allocate(ZPLMGS(ILG))
        allocate(ZPLIMC(ILG))
        allocate(ZPLIMG(ILG))
C
        allocate(ALTG(ILG,NBS))
        allocate(ALSNO(ILG,NBS))
        allocate(TRSNOWG(ILG,NBS))
C
        allocate(CTVSTP(ILG))
        allocate(CTSSTP(ILG))
        allocate(CTNSTP(ILG,IGND))
        allocate(WTVSTP(ILG))
        allocate(WTSSTP(ILG))
        allocate(WTGSTP(ILG))
C
        allocate(MANNING_N(ILG))
        allocate(DD(ILG))
        allocate(BULK_FC(ILG,IGND))
C
c================= CTEM array declaration ===============================\
c
        allocate(curlatno(ilg), altotcount_ctm(nlat))
        allocate(todfrac(ilg,icc), barf(nlat,nmos))
        allocate(currlat(ilg), wl(lat), grclarea(ilg),
     1           radl(lat), wossl(lat), sl(lat),
     2           cl(lat), ml(ilg))

        allocate(fsinacc_gat(ilg), flutacc_gat(ilg), flinacc_gat(ilg),
     1           alswacc_gat(ilg), allwacc_gat(ilg), pregacc_gat(ilg),
     2           altotacc_gat(ilg),
     3           netrad_gat(ilg), preacc_gat(ilg))

        allocate(sdepgat(ilg), orgmgat(ilg,ignd),
     1           sandgat(ilg,ignd), claygat(ilg,ignd),
     2           xdiffusgat(ilg), faregat(ilg))

        call veg_rot_allocate()
        call veg_gat_allocate()
        call class_moyr_output_allocate()
        call ctem_gridavg_allocate()
        call ctem_tile_level_allocate()
        call ctem_monthly_allocate()
        call ctem_gridavg_monthly_allocate()
        call ctem_tileavg_monthly_allocate()
        call ctem_annual_allocate()
        call ctem_gridavg_annual_allocate()
        call ctem_tileavg_annual_allocate()
C
C===================== CTEM ==============================================\

    ! Point pointers
      ctem_on           => c_switch%ctem_on
      parallelrun       => c_switch%parallelrun
      cyclemet          => c_switch%cyclemet
      dofire            => c_switch%dofire
      run_model         => c_switch%run_model
      met_rewound       => c_switch%met_rewound
      reach_eof         => c_switch%reach_eof
      compete           => c_switch%compete
      start_bare        => c_switch%start_bare
      rsfile            => c_switch%rsfile
      lnduseon          => c_switch%lnduseon
      co2on             => c_switch%co2on
      ch4on             => c_switch%ch4on
      popdon            => c_switch%popdon
      inibioclim        => c_switch%inibioclim
      start_from_rs     => c_switch%start_from_rs
      leap              => c_switch%leap         
      dowetlands        => c_switch%dowetlands
      obswetf           => c_switch%obswetf
      transient_run     => c_switch%transient_run
      ctemn             => c_switch%ctemn

      tcanrs            => vrot%tcanrs
      tsnors            => vrot%tsnors
      tpndrs            => vrot%tpndrs
      csum              => vrot%csum
      tbaraccrow_t      => vrot%tbaraccrow_m
      tcanoaccrow_t     => vrot%tcanoaccrow_m
      uvaccrow_t        => vrot%uvaccrow_m
      vvaccrow_t        => vrot%vvaccrow_m

      ! ROW:
      ailcminrow        => vrot%ailcmin
      ailcmaxrow        => vrot%ailcmax
      dvdfcanrow        => vrot%dvdfcan
      gleafmasrow       => vrot%gleafmas
      bleafmasrow       => vrot%bleafmas
      stemmassrow       => vrot%stemmass
      rootmassrow       => vrot%rootmass
      pstemmassrow      => vrot%pstemmass
      pgleafmassrow     => vrot%pgleafmass
      fcancmxrow        => vrot%fcancmx
      gavglairow        => vrot%gavglai
      zolncrow          => vrot%zolnc
      ailcrow           => vrot%ailc
      ailcgrow          => vrot%ailcg
      ailcgsrow         => vrot%ailcgs
      fcancsrow         => vrot%fcancs
      fcancrow          => vrot%fcanc
      co2concrow        => vrot%co2conc
      ch4concrow        => vrot%ch4conc
      co2i1cgrow        => vrot%co2i1cg
      co2i1csrow        => vrot%co2i1cs
      co2i2cgrow        => vrot%co2i2cg
      co2i2csrow        => vrot%co2i2cs
      ancsvegrow        => vrot%ancsveg
      ancgvegrow        => vrot%ancgveg
      rmlcsvegrow       => vrot%rmlcsveg
      rmlcgvegrow       => vrot%rmlcgveg
      slairow           => vrot%slai
      ailcbrow          => vrot%ailcb
      canresrow         => vrot%canres
      flhrlossrow       => vrot%flhrloss

      tcanoaccrow_out   => vrot%tcanoaccrow_out
      qevpacc_m_save    => vrot%qevpacc_m_save

      grwtheffrow       => vrot%grwtheff
      lystmmasrow       => vrot%lystmmas
      lyrotmasrow       => vrot%lyrotmas
      tymaxlairow       => vrot%tymaxlai
      vgbiomasrow       => vrot%vgbiomas
      gavgltmsrow       => vrot%gavgltms
      gavgscmsrow       => vrot%gavgscms
      stmhrlosrow       => vrot%stmhrlos
      rmatcrow          => vrot%rmatc
      rmatctemrow       => vrot%rmatctem
      litrmassrow       => vrot%litrmass
      soilcmasrow       => vrot%soilcmas
      vgbiomas_vegrow   => vrot%vgbiomas_veg

      emit_co2row       => vrot%emit_co2
      emit_corow        => vrot%emit_co
      emit_ch4row       => vrot%emit_ch4
      emit_nmhcrow      => vrot%emit_nmhc
      emit_h2row        => vrot%emit_h2
      emit_noxrow       => vrot%emit_nox
      emit_n2orow       => vrot%emit_n2o
      emit_pm25row      => vrot%emit_pm25
      emit_tpmrow       => vrot%emit_tpm
      emit_tcrow        => vrot%emit_tc
      emit_ocrow        => vrot%emit_oc
      emit_bcrow        => vrot%emit_bc
      burnfracrow       => vrot%burnfrac
      burnvegfrow       => vrot%burnvegf
      smfuncvegrow      => vrot%smfuncveg
      popdinrow         => vrot%popdin
      btermrow          => vrot%bterm
      ltermrow          => vrot%lterm
      mtermrow          => vrot%mterm

      extnprobrow       => vrot%extnprob
      prbfrhucrow       => vrot%prbfrhuc
      mlightngrow       => vrot%mlightng
      daylrow           => vrot%dayl
      dayl_maxrow       => vrot%dayl_max

      bmasvegrow        => vrot%bmasveg
      cmasvegcrow       => vrot%cmasvegc
      veghghtrow        => vrot%veghght
      rootdpthrow       => vrot%rootdpth
      rmlrow            => vrot%rml
      rmsrow            => vrot%rms
      tltrleafrow       => vrot%tltrleaf
      tltrstemrow       => vrot%tltrstem
      tltrrootrow       => vrot%tltrroot
      leaflitrrow       => vrot%leaflitr
      roottemprow       => vrot%roottemp
      afrleafrow        => vrot%afrleaf
      afrstemrow        => vrot%afrstem
      afrrootrow        => vrot%afrroot
      wtstatusrow       => vrot%wtstatus
      ltstatusrow       => vrot%ltstatus
      rmrrow            => vrot%rmr

      slopefracrow      => vrot%slopefrac
      ch4wet1row        => vrot%ch4wet1
      ch4wet2row        => vrot%ch4wet2
      wetfdynrow        => vrot%wetfdyn
      ch4dyn1row        => vrot%ch4dyn1
      ch4dyn2row        => vrot%ch4dyn2
      wetfrac_monrow    => vrot%wetfrac_mon
      ch4soillsrow      => vrot%ch4_soills

      lucemcomrow       => vrot%lucemcom
      lucltrinrow       => vrot%lucltrin
      lucsocinrow       => vrot%lucsocin

      npprow            => vrot%npp
      neprow            => vrot%nep
      nbprow            => vrot%nbp
      gpprow            => vrot%gpp
      hetroresrow       => vrot%hetrores
      autoresrow        => vrot%autores
      soilcresprow      => vrot%soilcresp
      rmrow             => vrot%rm
      rgrow             => vrot%rg
      litresrow         => vrot%litres
      socresrow         => vrot%socres
      dstcemlsrow       => vrot%dstcemls
      litrfallrow       => vrot%litrfall
      humiftrsrow       => vrot%humiftrs

      gppvegrow         => vrot%gppveg
      nepvegrow         => vrot%nepveg
      nbpvegrow         => vrot%nbpveg
      nppvegrow         => vrot%nppveg
      hetroresvegrow    => vrot%hetroresveg
      autoresvegrow     => vrot%autoresveg
      litresvegrow      => vrot%litresveg
      soilcresvegrow    => vrot%soilcresveg
      rmlvegaccrow      => vrot%rmlvegacc
      rmsvegrow         => vrot%rmsveg
      rmrvegrow         => vrot%rmrveg
      rgvegrow          => vrot%rgveg
      litrfallvegrow    => vrot%litrfallveg
      humiftrsvegrow    => vrot%humiftrsveg

      rothrlosrow       => vrot%rothrlos
      pfcancmxrow       => vrot%pfcancmx
      nfcancmxrow       => vrot%nfcancmx
      alvsctmrow        => vrot%alvsctm
      paicrow           => vrot%paic
      slaicrow          => vrot%slaic
      alirctmrow        => vrot%alirctm
      cfluxcgrow        => vrot%cfluxcg
      cfluxcsrow        => vrot%cfluxcs
      dstcemls3row      => vrot%dstcemls3
      anvegrow          => vrot%anveg
      rmlvegrow         => vrot%rmlveg

      pftexistrow       => vrot%pftexist
      colddaysrow       => vrot%colddays
      icountrow         => vrot%icount
      lfstatusrow       => vrot%lfstatus
      pandaysrow        => vrot%pandays
      stdalnrow         => vrot%stdaln

      twarmmrow            => vrot%twarmm
      tcoldmrow            => vrot%tcoldm
      gdd5row              => vrot%gdd5
      aridityrow           => vrot%aridity
      srplsmonrow          => vrot%srplsmon
      defctmonrow          => vrot%defctmon
      anndefctrow          => vrot%anndefct
      annsrplsrow          => vrot%annsrpls
      annpcprow            => vrot%annpcp
      dry_season_lengthrow => vrot%dry_season_length

      altotcntr_d       => vrot%altotcntr_d

      calsoil           => vrot%calsoil
      ndaytime          => vrot%ndaytime
    
      rnleafrow         => vrot%rnleaf
      rnstemrow         => vrot%rnstem
      rnrootrow         => vrot%rnroot
      rnlitrrow         => vrot%rnlitr
      rnsomrow          => vrot%rnsom
      snh4row           => vrot%snh4
      sno3row           => vrot%sno3
      nrubrow           => vrot%nrub
      nrub0row          => vrot%nrub0

      vcmx0row          => vrot%vcmx0
      btdpthrow         => vrot%btdpth

      dndeprow          => vrot%dndep
      dnferrow          => vrot%dnfer
      dnpltrrow         => vrot%dnpltr
      dndisrow          => vrot%dndis
      dnlsomrow         => vrot%dnlsom
      dnminrow          => vrot%dnmin
      dnnitrow          => vrot%dnnit
      dnpuprow          => vrot%dnpup
      dndnitrow         => vrot%dndnit
      dnlearow          => vrot%dnlea
      dnvolrow          => vrot%dnvol
      dnsorrow          => vrot%dnsor
      dnlosrow          => vrot%dnlos
      n2ototrow         => vrot%n2otot
      n2totrow          => vrot%n2tot
      dnbfixrow         => vrot%dnbfix
      dnplossrow        => vrot%dnploss
      dnslossrow        => vrot%dnsloss

      etprow            => vrot%etp
      xminf             => vrot%xminf
      xminfbar          => vrot%xminfbar


      ! >>>>>>>>>>>>>>>>>>>>>>>>>>
      ! GAT:

      lightng           => vgat%lightng
      tcanoaccgat_out   => vgat%tcanoaccgat_out

      ailcmingat        => vgat%ailcmin
      ailcmaxgat        => vgat%ailcmax
      dvdfcangat        => vgat%dvdfcan
      gleafmasgat       => vgat%gleafmas
      bleafmasgat       => vgat%bleafmas
      stemmassgat       => vgat%stemmass
      rootmassgat       => vgat%rootmass
      pstemmassgat      => vgat%pstemmass
      pgleafmassgat     => vgat%pgleafmass
      fcancmxgat        => vgat%fcancmx
      gavglaigat        => vgat%gavglai
      zolncgat          => vgat%zolnc
      ailcgat           => vgat%ailc
      ailcggat          => vgat%ailcg
      ailcgsgat         => vgat%ailcgs
      fcancsgat         => vgat%fcancs
      fcancgat          => vgat%fcanc
      co2concgat        => vgat%co2conc
      ch4concgat        => vgat%ch4conc
      co2i1cggat        => vgat%co2i1cg
      co2i1csgat        => vgat%co2i1cs
      co2i2cggat        => vgat%co2i2cg
      co2i2csgat        => vgat%co2i2cs
      ancsveggat        => vgat%ancsveg
      ancgveggat        => vgat%ancgveg
      rmlcsveggat       => vgat%rmlcsveg
      rmlcgveggat       => vgat%rmlcgveg
      slaigat           => vgat%slai
      ailcbgat          => vgat%ailcb
      canresgat         => vgat%canres
      flhrlossgat       => vgat%flhrloss

      grwtheffgat       => vgat%grwtheff
      lystmmasgat       => vgat%lystmmas
      lyrotmasgat       => vgat%lyrotmas
      tymaxlaigat       => vgat%tymaxlai
      vgbiomasgat       => vgat%vgbiomas
      gavgltmsgat       => vgat%gavgltms
      gavgscmsgat       => vgat%gavgscms
      stmhrlosgat       => vgat%stmhrlos
      rmatcgat          => vgat%rmatc
      rmatctemgat       => vgat%rmatctem
      litrmassgat       => vgat%litrmass
      soilcmasgat       => vgat%soilcmas
      vgbiomas_veggat   => vgat%vgbiomas_veg
      litrfallveggat    => vgat%litrfallveg
      humiftrsveggat    => vgat%humiftrsveg

      emit_co2gat       => vgat%emit_co2
      emit_cogat        => vgat%emit_co
      emit_ch4gat       => vgat%emit_ch4
      emit_nmhcgat      => vgat%emit_nmhc
      emit_h2gat        => vgat%emit_h2
      emit_noxgat       => vgat%emit_nox
      emit_n2ogat       => vgat%emit_n2o
      emit_pm25gat      => vgat%emit_pm25
      emit_tpmgat       => vgat%emit_tpm
      emit_tcgat        => vgat%emit_tc
      emit_ocgat        => vgat%emit_oc
      emit_bcgat        => vgat%emit_bc
      burnfracgat       => vgat%burnfrac
      burnvegfgat       => vgat%burnvegf
      popdingat         => vgat%popdin
      smfuncveggat      => vgat%smfuncveg
      btermgat          => vgat%bterm
      ltermgat          => vgat%lterm
      mtermgat          => vgat%mterm

      extnprobgat       => vgat%extnprob
      prbfrhucgat       => vgat%prbfrhuc
      mlightnggat       => vgat%mlightng
      daylgat           => vgat%dayl
      dayl_maxgat       => vgat%dayl_max

      bmasveggat        => vgat%bmasveg
      cmasvegcgat       => vgat%cmasvegc
      veghghtgat        => vgat%veghght
      rootdpthgat       => vgat%rootdpth
      rmlgat            => vgat%rml
      rmsgat            => vgat%rms
      tltrleafgat       => vgat%tltrleaf
      tltrstemgat       => vgat%tltrstem
      tltrrootgat       => vgat%tltrroot
      leaflitrgat       => vgat%leaflitr
      roottempgat       => vgat%roottemp
      afrleafgat        => vgat%afrleaf
      afrstemgat        => vgat%afrstem
      afrrootgat        => vgat%afrroot
      wtstatusgat       => vgat%wtstatus
      ltstatusgat       => vgat%ltstatus
      rmrgat            => vgat%rmr

      slopefracgat      => vgat%slopefrac
      wetfrac_presgat   => vgat%wetfrac_pres
      wetfrac_mongat    => vgat%wetfrac_mon
      ch4wet1gat        => vgat%ch4wet1
      ch4wet2gat        => vgat%ch4wet2
      wetfdyngat        => vgat%wetfdyn
      ch4dyn1gat        => vgat%ch4dyn1
      ch4dyn2gat        => vgat%ch4dyn2
      ch4soillsgat      => vgat%ch4_soills

      lucemcomgat       => vgat%lucemcom
      lucltringat       => vgat%lucltrin
      lucsocingat       => vgat%lucsocin

      nppgat            => vgat%npp
      nepgat            => vgat%nep
      nbpgat            => vgat%nbp
      gppgat            => vgat%gpp
      hetroresgat       => vgat%hetrores
      autoresgat        => vgat%autores
      soilcrespgat      => vgat%soilcresp
      rmgat             => vgat%rm
      rggat             => vgat%rg
      litresgat         => vgat%litres
      socresgat         => vgat%socres
      dstcemlsgat       => vgat%dstcemls
      litrfallgat       => vgat%litrfall
      humiftrsgat       => vgat%humiftrs

      gppveggat         => vgat%gppveg
      nepveggat         => vgat%nepveg
      nbpveggat         => vgat%nbpveg
      nppveggat         => vgat%nppveg
      hetroresveggat    => vgat%hetroresveg
      autoresveggat     => vgat%autoresveg
      litresveggat      => vgat%litresveg
      soilcresveggat    => vgat%soilcresveg
      rmlvegaccgat      => vgat%rmlvegacc
      rmsveggat         => vgat%rmsveg
      rmrveggat         => vgat%rmrveg
      rgveggat          => vgat%rgveg

      rothrlosgat       => vgat%rothrlos
      pfcancmxgat       => vgat%pfcancmx
      nfcancmxgat       => vgat%nfcancmx
      alvsctmgat        => vgat%alvsctm
      paicgat           => vgat%paic
      slaicgat          => vgat%slaic
      alirctmgat        => vgat%alirctm
      cfluxcggat        => vgat%cfluxcg
      cfluxcsgat        => vgat%cfluxcs
      dstcemls3gat      => vgat%dstcemls3
      anveggat          => vgat%anveg
      rmlveggat         => vgat%rmlveg

      twarmmgat            => vgat%twarmm
      tcoldmgat            => vgat%tcoldm
      gdd5gat              => vgat%gdd5
      ariditygat           => vgat%aridity
      srplsmongat          => vgat%srplsmon
      defctmongat          => vgat%defctmon
      anndefctgat          => vgat%anndefct
      annsrplsgat          => vgat%annsrpls
      annpcpgat            => vgat%annpcp
      dry_season_lengthgat => vgat%dry_season_length

      tcurm             => vgat%tcurm
      srpcuryr          => vgat%srpcuryr
      dftcuryr          => vgat%dftcuryr
      tmonth            => vgat%tmonth
      anpcpcur          => vgat%anpcpcur
      anpecur           => vgat%anpecur
      gdd5cur           => vgat%gdd5cur
      surmncur          => vgat%surmncur
      defmncur          => vgat%defmncur
      srplscur          => vgat%srplscur
      defctcur          => vgat%defctcur

      geremortgat       => vgat%geremort
      intrmortgat       => vgat%intrmort
      lambdagat         => vgat%lambda
      ccgat             => vgat%cc
      mmgat             => vgat%mm

      pftexistgat       => vgat%pftexist
      colddaysgat       => vgat%colddays
      icountgat         => vgat%icount
      lfstatusgat       => vgat%lfstatus
      pandaysgat        => vgat%pandays
      stdalngat         => vgat%stdaln

      rnleafgat         => vgat%rnleaf
      rnstemgat         => vgat%rnstem
      rnrootgat         => vgat%rnroot
      rnlitrgat         => vgat%rnlitr
      rnsomgat          => vgat%rnsom
      snh4gat           => vgat%snh4
      sno3gat           => vgat%sno3
      nrubgat           => vgat%nrub
      nrub0gat          => vgat%nrub0

      vcmx0gat          => vgat%vcmx0
      btdpthgat         => vgat%btdpth

      dndepgat          => vgat%dndep
      dnfergat          => vgat%dnfer
      dnpltrgat         => vgat%dnpltr
      dndisgat          => vgat%dndis
      dnlsomgat         => vgat%dnlsom
      dnmingat          => vgat%dnmin
      dnnitgat          => vgat%dnnit
      dnpupgat          => vgat%dnpup
      dndnitgat         => vgat%dndnit
      dnleagat          => vgat%dnlea
      dnvolgat          => vgat%dnvol
      dnsorgat          => vgat%dnsor
      dnlosgat          => vgat%dnlos
      n2ototgat         => vgat%n2otot
      n2totgat          => vgat%n2tot
      dnbfixgat         => vgat%dnbfix
      dnplossgat        => vgat%dnploss
      dnslossgat        => vgat%dnsloss

      etpgat            => vgat%etp
      etpacc            => vgat%etpacc

      rofaccgat_t       => vgat%rofacc_m
      ovraccgat_t       => vgat%ovracc_m


  
      ! Mosaic-level (CLASS vars):

      PREACC_t          => vrot%PREACC_M
      GTACC_t           => vrot%GTACC_M
      QEVPACC_t         => vrot%QEVPACC_M
      HFSACC_t          => vrot%HFSACC_M
      HMFNACC_t         => vrot%HMFNACC_M
      ROFACC_t          => vrot%ROFACC_M
      SNOACC_t          => vrot%SNOACC_M
      OVRACC_t          => vrot%OVRACC_M
      WTBLACC_t         => vrot%WTBLACC_M
      TBARACC_t         => vrot%TBARACC_M
      THLQACC_t         => vrot%THLQACC_M
      THICACC_t         => vrot%THICACC_M
      THALACC_t         => vrot%THALACC_M
      ALVSACC_t         => vrot%ALVSACC_M
      ALIRACC_t         => vrot%ALIRACC_M
      RHOSACC_t         => vrot%RHOSACC_M
      TSNOACC_t         => vrot%TSNOACC_M
      WSNOACC_t         => vrot%WSNOACC_M
      SNOARE_t          => vrot%SNOARE_M
      TCANACC_t         => vrot%TCANACC_M
      RCANACC_t         => vrot%RCANACC_M
      SCANACC_t         => vrot%SCANACC_M
      GROACC_t          => vrot%GROACC_M
      FSINACC_t         => vrot%FSINACC_M
      FLINACC_t         => vrot%FLINACC_M
      TAACC_t           => vrot%TAACC_M
      UVACC_t           => vrot%UVACC_M
      PRESACC_t         => vrot%PRESACC_M
      QAACC_t           => vrot%QAACC_M
      ALTOTACC_t        => vrot%ALTOTACC_M
      EVAPACC_t         => vrot%EVAPACC_M
      FLUTACC_t         => vrot%FLUTACC_M

      ! grid-averaged (CLASS vars)

      WSNOROT_g         => ctem_grd%WSNOROT_g
      ROFSROT_g         => ctem_grd%ROFSROT_g
      SNOROT_g          => ctem_grd%SNOROT_g
      RHOSROT_g         => ctem_grd%RHOSROT_g
      ROFROT_g          => ctem_grd%ROFROT_g
      ZPNDROT_g         => ctem_grd%ZPNDROT_g
      RCANROT_g         => ctem_grd%RCANROT_g
      SCANROT_g         => ctem_grd%SCANROT_g
      TROFROT_g         => ctem_grd%TROFROT_g
      TROOROT_g         => ctem_grd%TROOROT_g
      TROBROT_g         => ctem_grd%TROBROT_g
      ROFOROT_g         => ctem_grd%ROFOROT_g
      ROFBROT_g         => ctem_grd%ROFBROT_g
      TROSROT_g         => ctem_grd%TROSROT_g
      FSGVROT_g         => ctem_grd%FSGVROT_g
      FSGSROT_g         => ctem_grd%FSGSROT_g
      FLGVROT_g         => ctem_grd%FLGVROT_g
      FLGSROT_g         => ctem_grd%FLGSROT_g
      HFSCROT_g         => ctem_grd%HFSCROT_g
      HFSSROT_g         => ctem_grd%HFSSROT_g
      HEVCROT_g         => ctem_grd%HEVCROT_g
      HEVSROT_g         => ctem_grd%HEVSROT_g
      HMFCROT_g         => ctem_grd%HMFCROT_g
      HMFNROT_g         => ctem_grd%HMFNROT_g
      HTCSROT_g         => ctem_grd%HTCSROT_g
      HTCCROT_g         => ctem_grd%HTCCROT_g
      FSGGROT_g         => ctem_grd%FSGGROT_g
      FLGGROT_g         => ctem_grd%FLGGROT_g
      HFSGROT_g         => ctem_grd%HFSGROT_g
      HEVGROT_g         => ctem_grd%HEVGROT_g
      CDHROT_g          => ctem_grd%CDHROT_g
      CDMROT_g          => ctem_grd%CDMROT_g
      SFCUROT_g         => ctem_grd%SFCUROT_g
      SFCVROT_g         => ctem_grd%SFCVROT_g
      fc_g              => ctem_grd%fc_g
      fg_g              => ctem_grd%fg_g
      fcs_g             => ctem_grd%fcs_g
      fgs_g             => ctem_grd%fgs_g
      PCFCROT_g         => ctem_grd%PCFCROT_g
      PCLCROT_g         => ctem_grd%PCLCROT_g
      PCPGROT_g         => ctem_grd%PCPGROT_g
      QFCFROT_g         => ctem_grd%QFCFROT_g
      QFGROT_g          => ctem_grd%QFGROT_g
      QFCROT_g          => ctem_grd%QFCROT_g
      ROFCROT_g         => ctem_grd%ROFCROT_g
      ROFNROT_g         => ctem_grd%ROFNROT_g
      WTRSROT_g         => ctem_grd%WTRSROT_g
      WTRGROT_g         => ctem_grd%WTRGROT_g
      PCPNROT_g         => ctem_grd%PCPNROT_g
      QFCLROT_g         => ctem_grd%QFCLROT_g
      QFNROT_g          => ctem_grd%QFNROT_g
      WTRCROT_g         => ctem_grd%WTRCROT_g
      rmlvegrow_g       => ctem_grd%rmlvegrow_g
      anvegrow_g        => ctem_grd%anvegrow_g
      HMFGROT_g         => ctem_grd%HMFGROT_g
      HTCROT_g          => ctem_grd%HTCROT_g
      TBARROT_g         => ctem_grd%TBARROT_g
      THLQROT_g         => ctem_grd%THLQROT_g
      THICROT_g         => ctem_grd%THICROT_g
      GFLXROT_g         => ctem_grd%GFLXROT_g

       fsstar_g         => ctem_grd%fsstar_g
       flstar_g         => ctem_grd%flstar_g
       qh_g             => ctem_grd%qh_g
       qe_g             => ctem_grd%qe_g
       snomlt_g         => ctem_grd%snomlt_g
       beg_g            => ctem_grd%beg_g
       gtout_g          => ctem_grd%gtout_g
       tpn_g            => ctem_grd%tpn_g
       altot_g          => ctem_grd%altot_g
       tcn_g            => ctem_grd%tcn_g
       tsn_g            => ctem_grd%tsn_g
       zsn_g            => ctem_grd%zsn_g

      ! mosaic level variables (CLASS):

      fsnowacc_t        => ctem_tile%fsnowacc_t
      tcansacc_t        => ctem_tile%tcansacc_t
      tcanoaccgat_t     => ctem_tile%tcanoaccgat_t
      taaccgat_t        => ctem_tile%taaccgat_t
      uvaccgat_t        => ctem_tile%uvaccgat_t
      vvaccgat_t        => ctem_tile%vvaccgat_t
      tbaraccgat_t      => ctem_tile%tbaraccgat_t
      tbarcacc_t        => ctem_tile%tbarcacc_t
      tbarcsacc_t       => ctem_tile%tbarcsacc_t
      tbargacc_t        => ctem_tile%tbargacc_t
      tbargsacc_t       => ctem_tile%tbargsacc_t
      thliqcacc_t       => ctem_tile%thliqcacc_t
      thliqgacc_t       => ctem_tile%thliqgacc_t
      thliqacc_t        => ctem_tile%thliqacc_t
      thicecacc_t       => ctem_tile%thicecacc_t
      thicegacc_t       => ctem_tile%thicegacc_t
      ancsvgac_t        => ctem_tile%ancsvgac_t
      ancgvgac_t        => ctem_tile%ancgvgac_t
      rmlcsvga_t        => ctem_tile%rmlcsvga_t
      rmlcgvga_t        => ctem_tile%rmlcgvga_t
 


C===================== CTEM ==============================================/

      
      CALL CLASSD

!DAN    Pull from MESH.
      ZDMROW = pm%sfp%zrfm(1)
      ZDHROW = pm%sfp%zrfh(1)
      NTLD=NMOS
      CUMSNO = 0.0

!Create the basin averaged file for the basin averaged CTEM outputs
      BasinFile=trim(fls%GENDIR_OUT)//'/CTEM_Basin_Output.csv'


c     all model switches are read in from a namelist file

      call read_from_job_options(argbuff,transient_run,
     1             trans_startyr,ctemloop,ctem_on,ncyear,lnduseon,
     2             spinfast,cyclemet,nummetcylyrs,metcylyrst,co2on,
     3             setco2conc,ch4on,setch4conc,popdon,popcycleyr,
     4             parallelrun,dofire,dowetlands,obswetf,compete,
     5             inibioclim,start_bare,rsfile,start_from_rs,ctemn,
     6             leap,jmosty,idisp,izref,islfd,ipcp,itc,itcg,itg,
     7             ipai,ihgt,ialc,ials,ialg,isnoalb,igralb,jhhstd,
     8             jhhendd,jdstd,jdendd,jhhsty,jhhendy,jdsty,jdendy)

	!Here we will be reading the CLASS Logical parameters from MESH
	!Note that read_from_job_options still has these as arguments but the read in the subroutine above is commented out



       !To do: ask Dan how to access specific variables from MESH_input_run_options.ini
       !Can't figure it out from here unfortunately (fls% are able to be used from here)
       !For now we will directly read it from here then try and get it from op% later
         
         OPEN(unit=2015,file='MESH_input_run_options.ini')
         
         read(2015,*)
         read(2015,*) 
         read(2015,*)

!Read the number of control flags
         read(2015,*) CONFLAGS


      DO I=1,CONFLAGS
         read(2015,*)
      ENDDO

      read(2015,*)
      read(2015,*)
      read (2015,*) WF_NUM_POINTS !The number of grids in question
      read(2015,*)
      read(2015,*) (N_OUT(i),i = 1,WF_NUM_POINTS)
      read(2015,*) (II_OUT(i),i = 1,WF_NUM_POINTS)
      read(2015,'(5a10)') (DIR_OUT(i),i = 1,WF_NUM_POINTS)



c     Initialize the CTEM parameters
      call initpftpars(compete)
c
      lopcount = 1   ! initialize loop count to 1.
c
c     checking the time spent for running model
c
c      call idate(today)
c      call itime(now)
c      write(*,1000)   today(2), today(1), 2000+today(3), now
c 1000 format( 'start date: ', i2.2, '/', i2.2, '/', i4.4,
c     &      '; start time: ', i2.2, ':', i2.2, ':', i2.2 )
c
C     INITIALIZATION FOR COUPLING CLASS AND CTEM
C
       call initrowvars()
       call resetclassaccum(nltest,nmtest)

       IMONTH = 0

       do 11 i=1,nlat
        do 11 m=1,nmos
         barf(i,m)                = 1.0
         TCANOACCROW_t(I,M)       = 0.0
         UVACCROW_t(I,M)          = 0.0
         VVACCROW_t(I,M)          = 0.0
         TCANOACCROW_OUT(I,M)     = 0.0
11     continue

c     do some initializations for the reading in of data from files. these
c     initializations primarily affect how the model does a spinup or transient
c     simulation and which years of the input data are being read.

      if (.not. cyclemet .and. transient_run) then !transient simulation, set to dummy values
        metcylyrst=trans_startyr ! this will make it skip to the trans_startyr
        metcycendyr=9999
      else
c       find the final year of the cycling met
c       metcylyrst is defined in the joboptions file
        metcycendyr = metcylyrst + nummetcylyrs - 1
      endif

c     if cycling met (and not doing a transient run), find the popd and luc year to cycle with.
c     it is assumed that you always want to cycle the popd and luc
c     on the same year to be consistent. so if you are cycling the
c     met data, you can set a popd year (first case), or if cycling
c     the met data you can let the popcycleyr default to the met cycling
c     year by setting popcycleyr to -9999 (second case). if not cycling
c     the met data or you are doing a transient run that cycles the MET
c     at the start, cypopyr and cylucyr will default to a dummy value
c     (last case). (See example at bottom of read_from_job_options.f90
c     if confused)
c
      if (cyclemet .and. popcycleyr .ne. -9999 .and.
     &                                .not. transient_run) then
        cypopyr = popcycleyr
        cylucyr = popcycleyr
      else if (cyclemet .and. .not. transient_run) then
        cypopyr = metcylyrst
        cylucyr = metcylyrst
      else  ! give dummy value
        cypopyr = popcycleyr !-9999
        cylucyr = popcycleyr !-9999
      end if

c     CTEM initialization done
c
c     open files for reading and writing. these are for coupled model (class_ctem)
c     we added both grid and mosaic output files
c
c     * input files

c         If we wish to restart from the .CTM_RS and .INI_RS files, then
c         we move the original RS files into place and start from them.
          if (start_from_rs) then
             command='mv '//argbuff(1:strlen(argbuff))//'.INI_RS '
     &                    //argbuff(1:strlen(argbuff))//'.INI'
             call system(command)
             command='mv '//argbuff(1:strlen(argbuff))//'.CTM_RS '
     &                    //argbuff(1:strlen(argbuff))//'.CTM'
             call system(command)
          end if




!The name of the file here was changed for MESH purposes


c     luc file is opened in initialize_luc subroutine

      if (popdon) then
        open(unit=13,file=argbuff(1:strlen(argbuff))//'.POPD',
     &       status='old')
        read(13,*)  !Skip 3 lines of header
        read(13,*)
        read(13,*)
      endif
      if (co2on .or. ch4on) then
        open(unit=14,file=argbuff(1:strlen(argbuff))//'.CO2',
     &         status='old')
      endif

c
      if (obswetf) then
        open(unit=16,file=argbuff(1:strlen(argbuff))//'.WET',
     &         status='old')
      endif 

      if (obslght) then ! this was brought in for FireMIP
        open(unit=17,file=argbuff(1:strlen(argbuff))//'.LGHT',
     &         status='old')
      endif
c
c     * CLASS daily and half-hourly output files (monthly and annual are done in io_driver)
c
     

        DO k=1,WF_NUM_POINTS !Loop over the number of grids in question

!Initialize Argbuff again
       ARGBUFF = DIR_OUT(k)
       ARGBUFF=argbuff(1:strlen(argbuff))//'/CLASSCTEM_Out'
         
      if (.not. parallelrun) then ! stand alone mode, includes half-hourly and daily output
       OPEN(UNIT=61*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF1_G')  ! GRID-LEVEL DAILY OUTPUT FROM CLASS
       OPEN(UNIT=62*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF2_G')
       OPEN(UNIT=63*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF3_G')

       OPEN(UNIT=611*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF1_M') ! MOSAIC DAILY OUTPUT FROM CLASS
       OPEN(UNIT=621*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF2_M')
       OPEN(UNIT=631*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF3_M')

       OPEN(UNIT=64*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF4_M')  ! MOSAIC HALF-HOURLY OUTPUT FROM CLASS
       OPEN(UNIT=65*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF5_M')
       OPEN(UNIT=66*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF6_M')
       OPEN(UNIT=67*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF7_M')
       OPEN(UNIT=68*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF8_M')
       OPEN(UNIT=69*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF9_M')

       OPEN(UNIT=641*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF4_G') ! GRID-LEVEL HALF-HOURLY OUTPUT FROM CLASS
       OPEN(UNIT=651*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF5_G')
       OPEN(UNIT=661*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF6_G')
       OPEN(UNIT=671*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF7_G')
       OPEN(UNIT=681*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF8_G')
       OPEN(UNIT=691*k,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.OF9_G')
       end if

C     * READ AND PROCESS INITIALIZATION AND BACKGROUND INFORMATION.
C     * FIRST, MODEL RUN SPECIFICATIONS.

!
!      the ctem output file suffix naming convention is as follows:
!                       ".CT##{time}"
!      where the ## is a numerical identifier, {time} is any of H, D, M,
!      or Y for half hourly, daily, monthly, or yearly, respectively.
!
       ! Set up the CTEM half-hourly, daily, monthly and yearly files (if any needed), also
       ! setup the CLASS monthly and annual output files:

       call create_outfiles(argbuff,title1, title2, title3, title4,
     1                     title5,title6,name1, name2, name3, name4,
     2                     name5, name6, place1,place2, place3,
     3                     place4, place5, place6,BasinFile,
     4                     WF_NUM_POINTS,
     5                     N_OUT,II_OUT,DIR_OUT)

      IF(CTEM_ON) THEN

        if(obswetf) then
         read(16,*) TITLEC1
        end if
       ENDIF
C
      IF (.NOT. PARALLELRUN) THEN ! STAND ALONE MODE, INCLUDES HALF-HOURLY AND DAILY OUTPUT
C
       WRITE(61*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(61*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(61*k,6011)
6011  FORMAT(2X,'DAY  YEAR  K*  L*  QH  QE  SM  QG  ',
     1          'TR  SWE  DS  WS  AL  ROF  CUMS')

       WRITE(62*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(62*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6

       IF(IGND.GT.3) THEN
          WRITE(62*k,6012)
6012      FORMAT(2X,'DAY  YEAR  TG1  THL1  THI1  TG2  THL2  THI2  ',
     1              'TG3  THL3  THI3  TG4  THL4  THI4  TG5  THL5  ',
     2              'THI5')

       ELSE
          WRITE(62*k,6212)
6212      FORMAT(2X,'DAY  YEAR  TG1  THL1  THI1  TG2  THL2  THI2  ',
     1              'TG3  THL3  THI3  TCN  RCAN  SCAN  TSN  ZSN')

       ENDIF

       WRITE(63*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(63*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6

       IF(IGND.GT.3) THEN
          WRITE(63*k,6013)
6013      FORMAT(2X,'DAY  YEAR  TG6  THL6  THI6  TG7  THL7  THI7  ',
     1              'TG8  THL8  THI8  TG9  THL9  THI9  TG10'  ,
     2              'THL10  THI10')

       ELSE
          WRITE(63*k,6313)
6313      FORMAT(2X,'DAY YEAR KIN LIN TA UV PRES QA PCP EVAP')

       ENDIF
C
       WRITE(64*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(64*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(64*k,6014)
6014  FORMAT(2X,'HOUR  MIN  DAY  YEAR  K*  L*  QH  QE  SM  QG  ',
     1          'TR  SWE  DS  WS  AL  ROF  TPN  ZPN  CDH  CDM  ',
     2          'SFCU  SFCV  UV')

       WRITE(65*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(65*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6

       IF(IGND.GT.3) THEN
          WRITE(65*k,6015)
6015      FORMAT(2X,'HOUR  MIN  DAY  YEAR  TG1  THL1  THI1  TG2  ',
     1          'THL2  THI2  TG3  THL3  THI3  TG4  THL4  THI4  ',
     2          'TG5  THL5  THI5')

       ELSE
          WRITE(65*k,6515)
6515      FORMAT(2X,'HOUR  MIN  DAY  YEAR  TG1  THL1  THI1  TG2  ',
     1           'THL2  THI2  TG3  THL3  THI3  TCN  RCAN  SCAN  ',
     2           'TSN  ZSN  TCN-TA  TCANO  TAC  ACTLYR  FTABLE')

       ENDIF

       WRITE(66*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(66*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6

       IF(IGND.GT.3) THEN
          WRITE(66*k,6016)
6016      FORMAT(2X,'HOUR  MIN  DAY  YEAR  TG6  THL6  THI6  TG7  ',
     1          'THL7  THI7  TG8  THL8  THI8  TG9  THL9  THI9  ',
     2          'TG10  THL10  THI10  G0  G1  G2  G3  G4  G5  G6  ',
     3          'G7  G8  G9')

       ELSE
          WRITE(66*k,6616)
          WRITE(66*k,6615)
6616  FORMAT(2X,'HOUR  MIN  DAY  SWIN  LWIN  PCP  TA  VA  PA  QA')
6615  FORMAT(2X,'IF IGND <= 3, THIS FILE IS EMPTY')
       ENDIF

       WRITE(67*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(67*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(67*k,6017)
!     6017  FORMAT(2X,'WCAN SCAN CWLCAP CWFCAP FC FG FCS FGS CDH ', !runclass formatted.
!     1          'TCANO TCANS ALBS')
6017  FORMAT(2X,'HOUR  MIN  DAY  YEAR  ',
     1  'TROF     TROO     TROS     TROB      ROF     ROFO   ',
     2  '  ROFS        ROFB         FCS        FGS        FC       FG')

       WRITE(68*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(68*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(68*k,6018)
6018  FORMAT(2X,'HOUR  MIN  DAY  YEAR  ',
     1          'FSGV FSGS FSGG FLGV FLGS FLGG HFSC HFSS HFSG ',
     2          'HEVC HEVS HEVG HMFC HMFS HMFG1 HMFG2 HMFG3 ',
     3          'HTCC HTCS HTC1 HTC2 HTC3')

       WRITE(69*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(69*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(69*k,6019)
6019  FORMAT(2X,'HOUR  MIN  DAY  YEAR  ',
     1   'PCFC PCLC PCPN PCPG QFCF QFCL QFN QFG QFC1 ',
     2          'QFC2 QFC3 ROFC ROFN ROFO ROF WTRC WTRS WTRG')
!       runclass also has: EVDF ','CTV CTS CT1 CT2 CT3')
C
       WRITE(611*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(611*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(611*k,6011)
       WRITE(621*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(621*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C
       IF(IGND.GT.3) THEN
           WRITE(621*k,6012)
       ELSE
           WRITE(621*k,6212)
       ENDIF
C
       WRITE(631*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(631*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C
       IF(IGND.GT.3) THEN
          WRITE(631*k,6013)
       ELSE
          WRITE(631*k,6313)
       ENDIF
C
       WRITE(641*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(641*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(641*k,6008)
       WRITE(651*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(651*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C
       IF(IGND.GT.3) THEN
          WRITE(651*k,6015)
       ELSE
          WRITE(651*k,6515)
       ENDIF
C
       WRITE(661*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(661*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
C
       IF(IGND.GT.3) THEN
          WRITE(661*k,6016)
       ELSE
          WRITE(661*k,6616)
       ENDIF
C
       WRITE(671*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(671*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(671*k,6017)
       WRITE(681*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(681*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(681*k,6018)
       WRITE(691*k,6001) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
       WRITE(691*k,6002) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
       WRITE(691*k,6019)
C
6008  FORMAT(2X,'HOUR  MIN  DAY  YEAR  K*  L*  QH  QE  SM  QG  ',
     1          'TR  SWE  DS  WS  AL  ROF  TPN  ZPN  CDH  CDM  ',
     2          'SFCU  SFCV  UV')

       ENDIF !IF NOT PARALLELRUN
      ENDDO !Looping around each grid folder

C     CTEM FILE TITLES DONE
C======================= CTEM ========================================== /

!We would read the .INI file here but no need since we are using and reading from the MESH .INI file
!Since we have the "CONTAINS" statement at the beginning of this module, these variables should transfer over to RUNCLASS36CTEM_within_tile subroutine in this module
!Note that at this stage, we would need to put the variables into a CLASSG ROT format, which will be turned into GAT later for use

        DEGLON = shd%xlng(1)
        JLAT = NINT(shd%ylat(1)) !JLAT is only used for debugging purposes?

!DAN    Must transfer these here in order to convert from grid and GRU to tile indexing.
        ILMOS = shd%lc%ILMOS
        JLMOS = shd%lc%JLMOS
        IWMOS = shd%wc%ILMOS
        JWMOS = shd%wc%JLMOS

!DAN    'shd' variables use grid indexing.
!DAN    With a gridded domain, each cell has a unique DLAT/DLON.
      DO I=1,NLAT
        DLATROW(I) = shd%ylat(I)
        RADJROW(I) = DLATROW(I)*3.14159/180.0
        DLONROW(I) = shd%xlng(I)
      ENDDO

!DAN    'pm' and 'stas' variables use tile indexing; must convert these to grid using ILMOS and GRU using JLMOS.
      DO K=1,NML

        !Get grid and tile index
        I = ILMOS(K)
        M = JLMOS(K)

        !> Distribute variables. 

!Note that the MESH variables to the right have one dimension fewer than the ROT variables
!We are only transforming them into a 3D format to be compatible with RUNCLASS36CTEM
!So the MESH variables have dimension of (M,J), so simply ignore I
!DAN    MESH variables have dimension of (K,J), where K is the tile index;
!DAN    the ILMOS/JLMOS lookup tables connect K to (I,M).
!DAN    In short: (I,M) on the CTEM side, K on the MESH side.

!Remember that ROT is delcared dimension (NLAT,NMOS,ICP1) =(1,#of tiles, 3/4)
!Whereas GAT is delcared dimension (NMOS,ICP1) =(#of tiles, 3/4)
!DAN    GAT are declared dimension (NML,ICP1), NML is # of active tiles -- at most it's ILG = NLAT*NMOS

       !These variables are from the upper part of the CLASS files and are only used once for every tile

        ZRFMROW(I) = pm%sfp%zrfm(1)
        ZRFHROW(I) = pm%sfp%zrfh(1)
        ZBLDROW(I) = pm%sfp%zbld(1)
        GCROW(I) = pm%tp%gc(1)
        Z0ORROW(I) = 0.0
        GGEOROW(I) = 0.0

        DO J=1,(ICAN+1)
          FCANROT(I,M,J) = pm%cp%fcan(K,J) 
          LNZ0ROT(I,M,J) = pm%cp%lnz0(K,J)
          ALVCROT(I,M,J) = pm%cp%alvc(K,J)
          ALICROT(I,M,J) = pm%cp%alic(K,J)
        ENDDO

        DO J=1,ICAN
          PAMXROT(I,M,J) = pm%cp%lamx(K,J)
          PAMNROT(I,M,J) = pm%cp%lamn(K,J)
          CMASROT(I,M,J) = pm%cp%cmas(K,J)
          ROOTROT(I,M,J) = pm%cp%root(K,J)
          RSMNROT(I,M,J) = pm%cp%rsmn(K,J)
          QA50ROT(I,M,J) = pm%cp%qa50(K,J)
          VPDAROT(I,M,J) = pm%cp%vpda(K,J)
          VPDBROT(I,M,J) = pm%cp%vpdb(K,J)
          PSGAROT(I,M,J) = pm%cp%psga(K,J)
          PSGBROT(I,M,J) = pm%cp%psgb(K,J)
        ENDDO

     
        DRNROT(I,M) = pm%hp%drn(K)
        SDEPROT(I,M) = pm%slp%sdep(K)         !This is only called in ctem so check
        FAREROT(I,M) = shd%lc%ACLASS(I,M)     !This value does not take in the value in .ini but rather directly from the shd file  
                                              !Note that this value is only used for CTEM, all grided MESH/CLASS calculations are done externaly              
        ZSNLROT(I,M) = pm%snp%zsnl(K)            
        ZPLGROT(I,M) = pm%sfp%zplg(K)
        ZPLSROT(I,M) = pm%snp%zpls(K)
        DDROT(I,M) = pm%hp%dd(K)
        XSLPROT(I,M) = pm%tp%xslp(K)
        GRKFROT(I,M) = pm%hp%grkf(K)
        WFSFROT(I,M) = pm%hp%mann(K)
        WFCIROT(I,M) = pm%hp%ks(K)
        MIDROT(I,M) = pm%tp%mid(K)


!DAN    Changed to use IGND so it'll match the number of active soil layers
        DO J=1,IGND
          SANDROT(I,M,J) = pm%slp%sand(K,J)
          CLAYROT(I,M,J) = pm%slp%clay(K,J)
          ORGMROT(I,M,J) = pm%slp%orgm(K,J)
          TBARROT(I,M,J) = stas%sl%tbar(K,J) - TFREZ
        ENDDO

        TCANROT(I,M) = stas%cnpy%tcan(K) - TFREZ
        TSNOROT(I,M) = stas%sno%tsno(K) - TFREZ
        TPNDROT(I,M) = stas%sfc%tpnd(K) - TFREZ

!DAN    Changed to use IGND so it'll match the number of active soil layers
        DO J=1,IGND
          THLQROT(I,M,J) = stas%sl%thlq(K,J)
          THICROT(I,M,J) = stas%sl%thic(K,J)
        ENDDO


        ZPNDROT(I,M) = stas%sfc%zpnd(K)
        RCANROT(I,M) = stas%cnpy%rcan(K)
        SCANROT(I,M) = stas%cnpy%sncan(K) !Note that MESH uses SNCAN and not CLASS-CTEM's SCAN
        SNOROT(I,M) = stas%sno%sno(K)
        ALBSROT(I,M) = stas%sno%albs(K)
        RHOSROT(I,M) = stas%sno%rhos(K)
        GROROT(I,M) = stas%cnpy%gro(K)

!Now we provide the depth of the soil layers and the depth of the permeable layer ()
        DO J=1,IGND !(IGND being the number of soil layers)
          DELZ(J) = shd%lc%sl%DELZ(J)
          ZBOT(J) = shd%lc%sl%ZBOT(J)
!DAN    Per the note above about soil parameters, these should only be transferred if they've already
!DAN    been calculated by MESH, or else they'll be calculated by the call to 'CLASSB'.
!?          DLZWROT(I,M,J) = stas%sl%delzw(K,J)
!?          ZBTWROT(I,M,J) = stas%sl%zbotw(K,J)
        ENDDO
      ENDDO !End of the K Loop

!DAN    This variable is special, it seems to have no ROT counterpart, so transfer directly.
!DAN    The CTEM variable is declared to ILG, where the MESH might be declared to NML -- NML can be less than ILG;
!DAN    in this case, transfer using explicitly NML notation.
        IWF(1:NML) = pm%tp%iwf(1:NML)
        MANNING_N(1:NML) = pm%hp%mann(1:NML)

!Now we must provide the MESH model with the constant variables


C====================== CTEM =========================================== \
C
c     read from ctem initialization file (.CTM)

!This file will read in from the initial .CTM file the same method as before
!But the .CTM file must be in the same directory as the .INI files and the Makefile (/.../r1221/)
!So it's just a small adjustement to io_driver.f90 itself

      if (ctem_on) then
      call read_from_ctm(nltest,nmtest,FCANROT,FAREROT,
     1                   RSMNROT,QA50ROT,VPDAROT,VPDBROT,PSGAROT,
     2                   PSGBROT,DRNROT,SDEPROT,DDROT,XSLPROT,GRKFROT,
     3                   WFSFROT,WFCIROT,MIDROT,SANDROT, CLAYROT,
     4                   ORGMROT,TBARROT,THLQROT,THICROT,TCANROT,
     5                   TSNOROT,TPNDROT,ZPNDROT,RCANROT,SCANROT,
     6                   SNOROT,ALBSROT,RHOSROT,GROROT,argbuff,
     7                   onetile_perPFT)
      end if


!Read in the Nitrogen parameters
! Similar format to the .CTM file and will read in several GRUs of Nitrogen parameters
!This subroutine also initializes the JK integer


      if (ctemn) then
        call nitrogen_read(nltest,nmtest,icc,iccp1, rnleafrow, 
     1               rnstemrow, rnrootrow, rnlitrrow,rnsomrow,   
     2               snh4row, sno3row, et0, jmax, kl, km, kmin0,  
     3               rtmass0, solnh4, rnlf0, rnsm0, rnrt0, conreal,  
     4               LAI0, krubn, kn, kni0, kdn0, kv0, nbfix0,ndep0,  
     5               nfer0,nfero0,JK,fcancmxrow,etpacc,xminfbar)
      end if


c
C===================== CTEM =============================================== /

!     Complete some initial set up work:


!DAN    There's some overlap here with what's done in MESH.
!DAN    The states are overwritten at the beginning of the 'within_tile' routine.
      DO 100 I=1,NLTEST
      DO 100 M=1,NMTEST

!          TBARROT(I,M,1)=TBARROT(I,M,1)+TFREZ
!          TBARROT(I,M,2)=TBARROT(I,M,2)+TFREZ
!          TBARROT(I,M,3)=TBARROT(I,M,3)+TFREZ
!          TSNOROT(I,M)=TSNOROT(I,M)+TFREZ
!          TCANROT(I,M)=TCANROT(I,M)+TFREZ

!          TPNDROT(I,M)=TPNDROT(I,M)+TFREZ
!          TBASROT(I,M)=TBARROT(I,M,IGND) !Changed to IGND
!          CMAIROT(I,M)=0.
!          WSNOROT(I,M)=0.
!DAN    This is a parameter, transferred above.
!-          ZSNLROT(I,M)=0.10
!          TSFSROT(I,M,1)=TFREZ
!          TSFSROT(I,M,2)=TFREZ

!          TSFSROT(I,M,3)=TBARROT(I,M,1)
!          TSFSROT(I,M,4)=TBARROT(I,M,1)
!          TACROT (I,M)=TCANROT(I,M)
!          QACROT (I,M)=0.5E-2

!          IF(IGND.GT.3)                                 THEN
!              DO 65 J=4,IGND
!                  TBARROT(I,M,J)=TBARROT(I,M,3)
!                  IF(SDEPROT(I,M).LT.(ZBOT(J-1)+0.001) .AND.
!     1                  SANDROT(I,M,3).GT.-2.5)     THEN
!                      SANDROT(I,M,J)=-3.0
!                      CLAYROT(I,M,J)=-3.0
!                      ORGMROT(I,M,J)=-3.0
!                      THLQROT(I,M,J)=0.0
!                      THICROT(I,M,J)=0.0
!                  ELSE
!                      SANDROT(I,M,J)=SANDROT(I,M,3)
!                      CLAYROT(I,M,J)=CLAYROT(I,M,3)
!                      ORGMROT(I,M,J)=ORGMROT(I,M,3)
!                      THLQROT(I,M,J)=THLQROT(I,M,3)
!                      THICROT(I,M,J)=THICROT(I,M,3)
!                  ENDIF
!65            CONTINUE
!          ENDIF

          DO 75 K=1,6
          DO 75 L=1,50
              ITCTROT(I,M,K,L)=0
75        CONTINUE
100   CONTINUE

      DO 150 I=1,NLTEST
          PREACC(I)=0.
          GTACC(I)=0.
          QEVPACC(I)=0.
          EVAPACC(I)=0.
          HFSACC(I)=0.
          HMFNACC(I)=0.
          ROFACC(I)=0.
          ALTOTACC(I)=0.
          OVRACC(I)=0.
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
          altotcount_ctm(i)=0
          DO 125 J=1,IGND
              TBARACC(I,J)=0.
              THLQACC(I,J)=0.
              THICACC(I,J)=0.
              THALACC(I,J)=0.
125       CONTINUE
150   CONTINUE

          alswacc_gat(:)=0.
          allwacc_gat(:)=0.
          fsinacc_gat(:)=0.
          flinacc_gat(:)=0.
          flutacc_gat(:)=0.
          pregacc_gat(:)=0.
          altotacc_gat(:)=0.


c     initialize accumulated array for monthly & yearly output for class

      call resetclassmon(nltest)
      call resetclassyr(nltest)

      DO 175 I=1,200
          TAHIST(I)=0.0
          TCHIST(I)=0.0
          TACHIST(I)=0.0
          TDHIST(I)=0.0
          TD2HIST(I)=0.0
          TD3HIST(I)=0.0
          TD4HIST(I)=0.0
          TSHIST(I)=0.0
          TSCRHIST(I)=0.0
175   CONTINUE
      ALAVG=0.0
      ALMAX=0.0
      ACTLYR=0.0
      FTAVG=0.0
      FTMAX=0.0
      FTABLE=0.0

      CALL CLASSB(THPROT,THRROT,THMROT,BIROT,PSISROT,GRKSROT,
     1            THRAROT,HCPSROT,TCSROT,THFCROT,THLWROT,PSIWROT,
     2            DLZWROT,ZBTWROT,ALGWROT,ALGDROT,
     +            ALGWVROT,ALGWNROT,ALGDVROT,ALGDNROT,
     3            SANDROT,CLAYROT,ORGMROT,SOCIROT,DELZ,ZBOT,
     4            SDEPROT,ISNDROT,IGDRROT,
     5            NLAT,NMOS,1,NLTEST,NMTEST,IGND,IGRALB)

!DAN    Pulled from MESH-CLASS WPREP.
!DAN    Using ILMOS/JLMOS and ROT variables because GAT hasn't been populated yet.
c       Calculate BULK_FC, make sure we don't divide by 0 (for slope = 0 or bedrock i.e. %sand = -2)
        DO J=1,IGND
        DO K=1,NML
        I=ILMOS(K)
        M=JLMOS(K)
        IF(XSLPROT(I,M).gt.0.0 .AND. THPROT(I,M,J).gt.0.0)THEN
            BULK_FC(K,J)=
     +    (THPROT(I,M,J)/(BIROT(I,M,J)-1.))*
     +    ((2.*DDROT(I,M)*PSISROT(I,M,J)*BIROT(I,M,J)/XSLPROT(I,M))**
     +    (1./BIROT(I,M,J)))*
     +    ((3.*BIROT(I,M,J)+2.)**((BIROT(I,M,J)-1.)/BIROT(I,M,J))-
     +    (2.*BIROT(I,M,J)+2.)**((BIROT(I,M,J)-1.)/BIROT(I,M,J)))
        ELSE
c       make sure that BULK_FC is high to shut off interflow
            BULK_FC(K,J)=2.0
        ENDIF
        ENDDO
        ENDDO

5010  FORMAT(2X,6A4)
5020  FORMAT(5F10.2,F7.1,3I5)
5030  FORMAT(4F8.3,8X,4F8.3)
5040  FORMAT(9F8.3)
5050  FORMAT(6F10.2)
5060  FORMAT(7F10.3)
5070  FORMAT(2F10.4,F10.2,F10.3,F10.4,F10.3)
5080  FORMAT(3F10.1)
5090  FORMAT(4E8.1,I8)
5200  FORMAT(4I10)
5300  FORMAT(1X,I2,I3,I5,I6,2F9.2,E14.4,F9.2,E12.3,F8.2,F12.2,3F9.2,
     1       F9.4)
5301  FORMAT(I5,F10.4)
6001  FORMAT('#CLASS TEST RUN:     ',6A4)
6002  FORMAT('#RESEARCHER:         ',6A4)
6003  FORMAT('#INSTITUTION:        ',6A4)


C
C===================== CTEM =============================================== \
C    CTEM pointer delcarations
      ctem_on           => c_switch%ctem_on
      parallelrun       => c_switch%parallelrun
      cyclemet          => c_switch%cyclemet
      dofire            => c_switch%dofire
      run_model         => c_switch%run_model
      met_rewound       => c_switch%met_rewound
      reach_eof         => c_switch%reach_eof
      compete           => c_switch%compete
      start_bare        => c_switch%start_bare
      rsfile            => c_switch%rsfile
      lnduseon          => c_switch%lnduseon
      co2on             => c_switch%co2on
      ch4on             => c_switch%ch4on
      popdon            => c_switch%popdon
      inibioclim        => c_switch%inibioclim
      start_from_rs     => c_switch%start_from_rs
      ctemn             => c_switch%ctemn
      leap              => c_switch%leap         
      dowetlands        => c_switch%dowetlands
      obswetf           => c_switch%obswetf
      transient_run     => c_switch%transient_run

      tcanrs            => vrot%tcanrs
      tsnors            => vrot%tsnors
      tpndrs            => vrot%tpndrs
      csum              => vrot%csum
      tbaraccrow_t      => vrot%tbaraccrow_m
      tcanoaccrow_t     => vrot%tcanoaccrow_m
      uvaccrow_t        => vrot%uvaccrow_m
      vvaccrow_t        => vrot%vvaccrow_m

      ailcminrow        => vrot%ailcmin
      ailcmaxrow        => vrot%ailcmax
      dvdfcanrow        => vrot%dvdfcan
      gleafmasrow       => vrot%gleafmas
      bleafmasrow       => vrot%bleafmas
      stemmassrow       => vrot%stemmass
      rootmassrow       => vrot%rootmass
      pstemmassrow      => vrot%pstemmass
      pgleafmassrow     => vrot%pgleafmass
      fcancmxrow        => vrot%fcancmx
      gavglairow        => vrot%gavglai
      zolncrow          => vrot%zolnc
      ailcrow           => vrot%ailc
      ailcgrow          => vrot%ailcg
      ailcgsrow         => vrot%ailcgs
      fcancsrow         => vrot%fcancs
      fcancrow          => vrot%fcanc
      co2concrow        => vrot%co2conc
      ch4concrow        => vrot%ch4conc
      co2i1cgrow        => vrot%co2i1cg
      co2i1csrow        => vrot%co2i1cs
      co2i2cgrow        => vrot%co2i2cg
      co2i2csrow        => vrot%co2i2cs
      ancsvegrow        => vrot%ancsveg
      ancgvegrow        => vrot%ancgveg
      rmlcsvegrow       => vrot%rmlcsveg
      rmlcgvegrow       => vrot%rmlcgveg
      slairow           => vrot%slai
      ailcbrow          => vrot%ailcb
      canresrow         => vrot%canres
      flhrlossrow       => vrot%flhrloss

      tcanoaccrow_out   => vrot%tcanoaccrow_out
      qevpacc_m_save    => vrot%qevpacc_m_save

      grwtheffrow       => vrot%grwtheff
      lystmmasrow       => vrot%lystmmas
      lyrotmasrow       => vrot%lyrotmas
      tymaxlairow       => vrot%tymaxlai
      vgbiomasrow       => vrot%vgbiomas
      gavgltmsrow       => vrot%gavgltms
      gavgscmsrow       => vrot%gavgscms
      stmhrlosrow       => vrot%stmhrlos
      rmatcrow          => vrot%rmatc
      rmatctemrow       => vrot%rmatctem
      litrmassrow       => vrot%litrmass
      soilcmasrow       => vrot%soilcmas
      vgbiomas_vegrow   => vrot%vgbiomas_veg

      emit_co2row       => vrot%emit_co2
      emit_corow        => vrot%emit_co
      emit_ch4row       => vrot%emit_ch4
      emit_nmhcrow      => vrot%emit_nmhc
      emit_h2row        => vrot%emit_h2
      emit_noxrow       => vrot%emit_nox
      emit_n2orow       => vrot%emit_n2o
      emit_pm25row      => vrot%emit_pm25
      emit_tpmrow       => vrot%emit_tpm
      emit_tcrow        => vrot%emit_tc
      emit_ocrow        => vrot%emit_oc
      emit_bcrow        => vrot%emit_bc
      burnfracrow       => vrot%burnfrac
      burnvegfrow       => vrot%burnvegf
      smfuncvegrow      => vrot%smfuncveg
      popdinrow         => vrot%popdin
      btermrow          => vrot%bterm
      ltermrow          => vrot%lterm
      mtermrow          => vrot%mterm

      extnprobrow       => vrot%extnprob
      prbfrhucrow       => vrot%prbfrhuc
      mlightngrow       => vrot%mlightng
      daylrow           => vrot%dayl
      dayl_maxrow       => vrot%dayl_max

      bmasvegrow        => vrot%bmasveg
      cmasvegcrow       => vrot%cmasvegc
      veghghtrow        => vrot%veghght
      rootdpthrow       => vrot%rootdpth
      rmlrow            => vrot%rml
      rmsrow            => vrot%rms
      tltrleafrow       => vrot%tltrleaf
      tltrstemrow       => vrot%tltrstem
      tltrrootrow       => vrot%tltrroot
      leaflitrrow       => vrot%leaflitr
      roottemprow       => vrot%roottemp
      afrleafrow        => vrot%afrleaf
      afrstemrow        => vrot%afrstem
      afrrootrow        => vrot%afrroot
      wtstatusrow       => vrot%wtstatus
      ltstatusrow       => vrot%ltstatus
      rmrrow            => vrot%rmr

      slopefracrow      => vrot%slopefrac
      ch4wet1row        => vrot%ch4wet1
      ch4wet2row        => vrot%ch4wet2
      wetfdynrow        => vrot%wetfdyn
      ch4dyn1row        => vrot%ch4dyn1
      ch4dyn2row        => vrot%ch4dyn2
      wetfrac_monrow    => vrot%wetfrac_mon
      ch4soillsrow      => vrot%ch4_soills

      lucemcomrow       => vrot%lucemcom
      lucltrinrow       => vrot%lucltrin
      lucsocinrow       => vrot%lucsocin

      npprow            => vrot%npp
      neprow            => vrot%nep
      nbprow            => vrot%nbp
      gpprow            => vrot%gpp
      hetroresrow       => vrot%hetrores
      autoresrow        => vrot%autores
      soilcresprow      => vrot%soilcresp
      rmrow             => vrot%rm
      rgrow             => vrot%rg
      litresrow         => vrot%litres
      socresrow         => vrot%socres
      dstcemlsrow       => vrot%dstcemls
      litrfallrow       => vrot%litrfall
      humiftrsrow       => vrot%humiftrs

      gppvegrow         => vrot%gppveg
      nepvegrow         => vrot%nepveg
      nbpvegrow         => vrot%nbpveg
      nppvegrow         => vrot%nppveg
      hetroresvegrow    => vrot%hetroresveg
      autoresvegrow     => vrot%autoresveg
      litresvegrow      => vrot%litresveg
      soilcresvegrow    => vrot%soilcresveg
      rmlvegaccrow      => vrot%rmlvegacc
      rmsvegrow         => vrot%rmsveg
      rmrvegrow         => vrot%rmrveg
      rgvegrow          => vrot%rgveg
      litrfallvegrow    => vrot%litrfallveg
      humiftrsvegrow    => vrot%humiftrsveg

      rothrlosrow       => vrot%rothrlos
      pfcancmxrow       => vrot%pfcancmx
      nfcancmxrow       => vrot%nfcancmx
      alvsctmrow        => vrot%alvsctm
      paicrow           => vrot%paic
      slaicrow          => vrot%slaic
      alirctmrow        => vrot%alirctm
      cfluxcgrow        => vrot%cfluxcg
      cfluxcsrow        => vrot%cfluxcs
      dstcemls3row      => vrot%dstcemls3
      anvegrow          => vrot%anveg
      rmlvegrow         => vrot%rmlveg

      pftexistrow       => vrot%pftexist
      colddaysrow       => vrot%colddays
      icountrow         => vrot%icount
      lfstatusrow       => vrot%lfstatus
      pandaysrow        => vrot%pandays
      stdalnrow         => vrot%stdaln

      twarmmrow            => vrot%twarmm
      tcoldmrow            => vrot%tcoldm
      gdd5row              => vrot%gdd5
      aridityrow           => vrot%aridity
      srplsmonrow          => vrot%srplsmon
      defctmonrow          => vrot%defctmon
      anndefctrow          => vrot%anndefct
      annsrplsrow          => vrot%annsrpls
      annpcprow            => vrot%annpcp
      dry_season_lengthrow => vrot%dry_season_length

      altotcntr_d       => vrot%altotcntr_d

      calsoil           => vrot%calsoil
      ndaytime          => vrot%ndaytime
    
      rnleafrow         => vrot%rnleaf
      rnstemrow         => vrot%rnstem
      rnrootrow         => vrot%rnroot
      rnlitrrow         => vrot%rnlitr
      rnsomrow          => vrot%rnsom
      snh4row           => vrot%snh4
      sno3row           => vrot%sno3
      nrubrow           => vrot%nrub
      nrub0row          => vrot%nrub0

      vcmx0row          => vrot%vcmx0
      btdpthrow         => vrot%btdpth

      dndeprow          => vrot%dndep
      dnferrow          => vrot%dnfer
      dnpltrrow         => vrot%dnpltr
      dndisrow          => vrot%dndis
      dnlsomrow         => vrot%dnlsom
      dnminrow          => vrot%dnmin
      dnnitrow          => vrot%dnnit
      dnpuprow          => vrot%dnpup
      dndnitrow         => vrot%dndnit
      dnlearow          => vrot%dnlea
      dnvolrow          => vrot%dnvol
      dnsorrow          => vrot%dnsor
      dnlosrow          => vrot%dnlos
      n2ototrow         => vrot%n2otot
      n2totrow          => vrot%n2tot
      dnbfixrow         => vrot%dnbfix
      dnplossrow        => vrot%dnploss
      dnslossrow        => vrot%dnsloss

      etprow            => vrot%etp
      xminf             => vrot%xminf
      xminfbar          => vrot%xminfbar



      ! >>>>>>>>>>>>>>>>>>>>>>>>>>
      ! GAT:

      lightng           => vgat%lightng
      tcanoaccgat_out   => vgat%tcanoaccgat_out

      ailcmingat        => vgat%ailcmin
      ailcmaxgat        => vgat%ailcmax
      dvdfcangat        => vgat%dvdfcan
      gleafmasgat       => vgat%gleafmas
      bleafmasgat       => vgat%bleafmas
      stemmassgat       => vgat%stemmass
      rootmassgat       => vgat%rootmass
      pstemmassgat      => vgat%pstemmass
      pgleafmassgat     => vgat%pgleafmass
      fcancmxgat        => vgat%fcancmx
      gavglaigat        => vgat%gavglai
      zolncgat          => vgat%zolnc
      ailcgat           => vgat%ailc
      ailcggat          => vgat%ailcg
      ailcgsgat         => vgat%ailcgs
      fcancsgat         => vgat%fcancs
      fcancgat          => vgat%fcanc
      co2concgat        => vgat%co2conc
      ch4concgat        => vgat%ch4conc
      co2i1cggat        => vgat%co2i1cg
      co2i1csgat        => vgat%co2i1cs
      co2i2cggat        => vgat%co2i2cg
      co2i2csgat        => vgat%co2i2cs
      ancsveggat        => vgat%ancsveg
      ancgveggat        => vgat%ancgveg
      rmlcsveggat       => vgat%rmlcsveg
      rmlcgveggat       => vgat%rmlcgveg
      slaigat           => vgat%slai
      ailcbgat          => vgat%ailcb
      canresgat         => vgat%canres
      flhrlossgat       => vgat%flhrloss

      grwtheffgat       => vgat%grwtheff
      lystmmasgat       => vgat%lystmmas
      lyrotmasgat       => vgat%lyrotmas
      tymaxlaigat       => vgat%tymaxlai
      vgbiomasgat       => vgat%vgbiomas
      gavgltmsgat       => vgat%gavgltms
      gavgscmsgat       => vgat%gavgscms
      stmhrlosgat       => vgat%stmhrlos
      rmatcgat          => vgat%rmatc
      rmatctemgat       => vgat%rmatctem
      litrmassgat       => vgat%litrmass
      soilcmasgat       => vgat%soilcmas
      vgbiomas_veggat   => vgat%vgbiomas_veg
      litrfallveggat    => vgat%litrfallveg
      humiftrsveggat    => vgat%humiftrsveg

      emit_co2gat       => vgat%emit_co2
      emit_cogat        => vgat%emit_co
      emit_ch4gat       => vgat%emit_ch4
      emit_nmhcgat      => vgat%emit_nmhc
      emit_h2gat        => vgat%emit_h2
      emit_noxgat       => vgat%emit_nox
      emit_n2ogat       => vgat%emit_n2o
      emit_pm25gat      => vgat%emit_pm25
      emit_tpmgat       => vgat%emit_tpm
      emit_tcgat        => vgat%emit_tc
      emit_ocgat        => vgat%emit_oc
      emit_bcgat        => vgat%emit_bc
      burnfracgat       => vgat%burnfrac
      burnvegfgat       => vgat%burnvegf
      popdingat         => vgat%popdin
      smfuncveggat      => vgat%smfuncveg
      btermgat          => vgat%bterm
      ltermgat          => vgat%lterm
      mtermgat          => vgat%mterm

      extnprobgat       => vgat%extnprob
      prbfrhucgat       => vgat%prbfrhuc
      mlightnggat       => vgat%mlightng
      daylgat           => vgat%dayl
      dayl_maxgat       => vgat%dayl_max

      bmasveggat        => vgat%bmasveg
      cmasvegcgat       => vgat%cmasvegc
      veghghtgat        => vgat%veghght
      rootdpthgat       => vgat%rootdpth
      rmlgat            => vgat%rml
      rmsgat            => vgat%rms
      tltrleafgat       => vgat%tltrleaf
      tltrstemgat       => vgat%tltrstem
      tltrrootgat       => vgat%tltrroot
      leaflitrgat       => vgat%leaflitr
      roottempgat       => vgat%roottemp
      afrleafgat        => vgat%afrleaf
      afrstemgat        => vgat%afrstem
      afrrootgat        => vgat%afrroot
      wtstatusgat       => vgat%wtstatus
      ltstatusgat       => vgat%ltstatus
      rmrgat            => vgat%rmr

      slopefracgat      => vgat%slopefrac
      wetfrac_presgat   => vgat%wetfrac_pres
      wetfrac_mongat    => vgat%wetfrac_mon
      ch4wet1gat        => vgat%ch4wet1
      ch4wet2gat        => vgat%ch4wet2
      wetfdyngat        => vgat%wetfdyn
      ch4dyn1gat        => vgat%ch4dyn1
      ch4dyn2gat        => vgat%ch4dyn2
      ch4soillsgat      => vgat%ch4_soills

      lucemcomgat       => vgat%lucemcom
      lucltringat       => vgat%lucltrin
      lucsocingat       => vgat%lucsocin

      nppgat            => vgat%npp
      nepgat            => vgat%nep
      nbpgat            => vgat%nbp
      gppgat            => vgat%gpp
      hetroresgat       => vgat%hetrores
      autoresgat        => vgat%autores
      soilcrespgat      => vgat%soilcresp
      rmgat             => vgat%rm
      rggat             => vgat%rg
      litresgat         => vgat%litres
      socresgat         => vgat%socres
      dstcemlsgat       => vgat%dstcemls
      litrfallgat       => vgat%litrfall
      humiftrsgat       => vgat%humiftrs

      gppveggat         => vgat%gppveg
      nepveggat         => vgat%nepveg
      nbpveggat         => vgat%nbpveg
      nppveggat         => vgat%nppveg
      hetroresveggat    => vgat%hetroresveg
      autoresveggat     => vgat%autoresveg
      litresveggat      => vgat%litresveg
      soilcresveggat    => vgat%soilcresveg
      rmlvegaccgat      => vgat%rmlvegacc
      rmsveggat         => vgat%rmsveg
      rmrveggat         => vgat%rmrveg
      rgveggat          => vgat%rgveg

      rothrlosgat       => vgat%rothrlos
      pfcancmxgat       => vgat%pfcancmx
      nfcancmxgat       => vgat%nfcancmx
      alvsctmgat        => vgat%alvsctm
      paicgat           => vgat%paic
      slaicgat          => vgat%slaic
      alirctmgat        => vgat%alirctm
      cfluxcggat        => vgat%cfluxcg
      cfluxcsgat        => vgat%cfluxcs
      dstcemls3gat      => vgat%dstcemls3
      anveggat          => vgat%anveg
      rmlveggat         => vgat%rmlveg

      twarmmgat            => vgat%twarmm
      tcoldmgat            => vgat%tcoldm
      gdd5gat              => vgat%gdd5
      ariditygat           => vgat%aridity
      srplsmongat          => vgat%srplsmon
      defctmongat          => vgat%defctmon
      anndefctgat          => vgat%anndefct
      annsrplsgat          => vgat%annsrpls
      annpcpgat            => vgat%annpcp
      dry_season_lengthgat => vgat%dry_season_length

      tcurm             => vgat%tcurm
      srpcuryr          => vgat%srpcuryr
      dftcuryr          => vgat%dftcuryr
      tmonth            => vgat%tmonth
      anpcpcur          => vgat%anpcpcur
      anpecur           => vgat%anpecur
      gdd5cur           => vgat%gdd5cur
      surmncur          => vgat%surmncur
      defmncur          => vgat%defmncur
      srplscur          => vgat%srplscur
      defctcur          => vgat%defctcur

      geremortgat       => vgat%geremort
      intrmortgat       => vgat%intrmort
      lambdagat         => vgat%lambda
      ccgat             => vgat%cc
      mmgat             => vgat%mm

      pftexistgat       => vgat%pftexist
      colddaysgat       => vgat%colddays
      icountgat         => vgat%icount
      lfstatusgat       => vgat%lfstatus
      pandaysgat        => vgat%pandays
      stdalngat         => vgat%stdaln

      rnleafgat         => vgat%rnleaf
      rnstemgat         => vgat%rnstem
      rnrootgat         => vgat%rnroot
      rnlitrgat         => vgat%rnlitr
      rnsomgat          => vgat%rnsom
      snh4gat           => vgat%snh4
      sno3gat           => vgat%sno3
      nrubgat           => vgat%nrub
      nrub0gat          => vgat%nrub0

      vcmx0gat          => vgat%vcmx0
      btdpthgat         => vgat%btdpth

      dndepgat          => vgat%dndep
      dnfergat          => vgat%dnfer
      dnpltrgat         => vgat%dnpltr
      dndisgat          => vgat%dndis
      dnlsomgat         => vgat%dnlsom
      dnmingat          => vgat%dnmin
      dnnitgat          => vgat%dnnit
      dnpupgat          => vgat%dnpup
      dndnitgat         => vgat%dndnit
      dnleagat          => vgat%dnlea
      dnvolgat          => vgat%dnvol
      dnsorgat          => vgat%dnsor
      dnlosgat          => vgat%dnlos
      n2ototgat         => vgat%n2otot
      n2totgat          => vgat%n2tot
      dnbfixgat         => vgat%dnbfix
      dnplossgat        => vgat%dnploss
      dnslossgat        => vgat%dnsloss

      etpgat            => vgat%etp
      etpacc            => vgat%etpacc

      rofaccgat_t       => vgat%rofacc_m
      ovraccgat_t       => vgat%ovracc_m

      ! Mosaic-level (CLASS vars):

      PREACC_t          => vrot%PREACC_M
      GTACC_t           => vrot%GTACC_M
      QEVPACC_t         => vrot%QEVPACC_M
      HFSACC_t          => vrot%HFSACC_M
      HMFNACC_t         => vrot%HMFNACC_M
      ROFACC_t          => vrot%ROFACC_M
      SNOACC_t          => vrot%SNOACC_M
      OVRACC_t          => vrot%OVRACC_M
      WTBLACC_t         => vrot%WTBLACC_M
      TBARACC_t         => vrot%TBARACC_M
      THLQACC_t         => vrot%THLQACC_M
      THICACC_t         => vrot%THICACC_M
      THALACC_t         => vrot%THALACC_M
      ALVSACC_t         => vrot%ALVSACC_M
      ALIRACC_t         => vrot%ALIRACC_M
      RHOSACC_t         => vrot%RHOSACC_M
      TSNOACC_t         => vrot%TSNOACC_M
      WSNOACC_t         => vrot%WSNOACC_M
      SNOARE_t          => vrot%SNOARE_M
      TCANACC_t         => vrot%TCANACC_M
      RCANACC_t         => vrot%RCANACC_M
      SCANACC_t         => vrot%SCANACC_M
      GROACC_t          => vrot%GROACC_M
      FSINACC_t         => vrot%FSINACC_M
      FLINACC_t         => vrot%FLINACC_M
      TAACC_t           => vrot%TAACC_M
      UVACC_t           => vrot%UVACC_M
      PRESACC_t         => vrot%PRESACC_M
      QAACC_t           => vrot%QAACC_M
      ALTOTACC_t        => vrot%ALTOTACC_M
      EVAPACC_t         => vrot%EVAPACC_M
      FLUTACC_t         => vrot%FLUTACC_M

      ! grid-averaged (CLASS vars)

      WSNOROT_g         => ctem_grd%WSNOROT_g
      ROFSROT_g         => ctem_grd%ROFSROT_g
      SNOROT_g          => ctem_grd%SNOROT_g
      RHOSROT_g         => ctem_grd%RHOSROT_g
      ROFROT_g          => ctem_grd%ROFROT_g
      ZPNDROT_g         => ctem_grd%ZPNDROT_g
      RCANROT_g         => ctem_grd%RCANROT_g
      SCANROT_g         => ctem_grd%SCANROT_g
      TROFROT_g         => ctem_grd%TROFROT_g
      TROOROT_g         => ctem_grd%TROOROT_g
      TROBROT_g         => ctem_grd%TROBROT_g
      ROFOROT_g         => ctem_grd%ROFOROT_g
      ROFBROT_g         => ctem_grd%ROFBROT_g
      TROSROT_g         => ctem_grd%TROSROT_g
      FSGVROT_g         => ctem_grd%FSGVROT_g
      FSGSROT_g         => ctem_grd%FSGSROT_g
      FLGVROT_g         => ctem_grd%FLGVROT_g
      FLGSROT_g         => ctem_grd%FLGSROT_g
      HFSCROT_g         => ctem_grd%HFSCROT_g
      HFSSROT_g         => ctem_grd%HFSSROT_g
      HEVCROT_g         => ctem_grd%HEVCROT_g
      HEVSROT_g         => ctem_grd%HEVSROT_g
      HMFCROT_g         => ctem_grd%HMFCROT_g
      HMFNROT_g         => ctem_grd%HMFNROT_g
      HTCSROT_g         => ctem_grd%HTCSROT_g
      HTCCROT_g         => ctem_grd%HTCCROT_g
      FSGGROT_g         => ctem_grd%FSGGROT_g
      FLGGROT_g         => ctem_grd%FLGGROT_g
      HFSGROT_g         => ctem_grd%HFSGROT_g
      HEVGROT_g         => ctem_grd%HEVGROT_g
      CDHROT_g          => ctem_grd%CDHROT_g
      CDMROT_g          => ctem_grd%CDMROT_g
      SFCUROT_g         => ctem_grd%SFCUROT_g
      SFCVROT_g         => ctem_grd%SFCVROT_g
      fc_g              => ctem_grd%fc_g
      fg_g              => ctem_grd%fg_g
      fcs_g             => ctem_grd%fcs_g
      fgs_g             => ctem_grd%fgs_g
      PCFCROT_g         => ctem_grd%PCFCROT_g
      PCLCROT_g         => ctem_grd%PCLCROT_g
      PCPGROT_g         => ctem_grd%PCPGROT_g
      QFCFROT_g         => ctem_grd%QFCFROT_g
      QFGROT_g          => ctem_grd%QFGROT_g
      QFCROT_g          => ctem_grd%QFCROT_g
      ROFCROT_g         => ctem_grd%ROFCROT_g
      ROFNROT_g         => ctem_grd%ROFNROT_g
      WTRSROT_g         => ctem_grd%WTRSROT_g
      WTRGROT_g         => ctem_grd%WTRGROT_g
      PCPNROT_g         => ctem_grd%PCPNROT_g
      QFCLROT_g         => ctem_grd%QFCLROT_g
      QFNROT_g          => ctem_grd%QFNROT_g
      WTRCROT_g         => ctem_grd%WTRCROT_g
      rmlvegrow_g       => ctem_grd%rmlvegrow_g
      anvegrow_g        => ctem_grd%anvegrow_g
      HMFGROT_g         => ctem_grd%HMFGROT_g
      HTCROT_g          => ctem_grd%HTCROT_g
      TBARROT_g         => ctem_grd%TBARROT_g
      THLQROT_g         => ctem_grd%THLQROT_g
      THICROT_g         => ctem_grd%THICROT_g
      GFLXROT_g         => ctem_grd%GFLXROT_g

       fsstar_g         => ctem_grd%fsstar_g
       flstar_g         => ctem_grd%flstar_g
       qh_g             => ctem_grd%qh_g
       qe_g             => ctem_grd%qe_g
       snomlt_g         => ctem_grd%snomlt_g
       beg_g            => ctem_grd%beg_g
       gtout_g          => ctem_grd%gtout_g
       tpn_g            => ctem_grd%tpn_g
       altot_g          => ctem_grd%altot_g
       tcn_g            => ctem_grd%tcn_g
       tsn_g            => ctem_grd%tsn_g
       zsn_g            => ctem_grd%zsn_g

      ! mosaic level variables (CLASS):

      fsnowacc_t        => ctem_tile%fsnowacc_t
      tcansacc_t        => ctem_tile%tcansacc_t
      tcanoaccgat_t     => ctem_tile%tcanoaccgat_t
      taaccgat_t        => ctem_tile%taaccgat_t
      uvaccgat_t        => ctem_tile%uvaccgat_t
      vvaccgat_t        => ctem_tile%vvaccgat_t
      tbaraccgat_t      => ctem_tile%tbaraccgat_t
      tbarcacc_t        => ctem_tile%tbarcacc_t
      tbarcsacc_t       => ctem_tile%tbarcsacc_t
      tbargacc_t        => ctem_tile%tbargacc_t
      tbargsacc_t       => ctem_tile%tbargsacc_t
      thliqcacc_t       => ctem_tile%thliqcacc_t
      thliqgacc_t       => ctem_tile%thliqgacc_t
      thliqacc_t        => ctem_tile%thliqacc_t
      thicecacc_t       => ctem_tile%thicecacc_t
      thicegacc_t       => ctem_tile%thicegacc_t
      ancsvgac_t        => ctem_tile%ancsvgac_t
      ancgvgac_t        => ctem_tile%ancgvgac_t
      rmlcsvga_t        => ctem_tile%rmlcsvga_t
      rmlcgvga_t        => ctem_tile%rmlcgvga_t

C
c     ctem initializations.
c

      if (ctem_on) then

c
c     calculate number of level 2 pfts using modelpft
c
      do 101 j = 1, ican
        isumc = 0
        k1c = (j-1)*l2max + 1
        k2c = k1c + (l2max - 1)
        do n = k1c, k2c
          if(modelpft(n).eq.1) isumc = isumc + 1
        enddo
        nol2pfts(j)=isumc  ! number of level 2 pfts
101   continue


      do 110 i=1,nltest
       do 110 m=1,nmtest
        do 111 j = 1, icc
          co2i1csrow(i,m,j)=0.0     !intercellular co2 concentrations
          co2i1cgrow(i,m,j)=0.0
          co2i2csrow(i,m,j)=0.0
          co2i2cgrow(i,m,j)=0.0
          slairow(i,m,j)=0.0        !if bio2str is not called we need to initialize this to zero
111     continue
110   continue
c
      do 123 i =1, ilg
         fsnowacc_t(i)=0.0         !daily accu. fraction of snow
         tcansacc_t(i)=0.0         !daily accu. canopy temp. over snow
         taaccgat_t(i)=0.0
c       Nitrogen ROF and OVR for ctem and CLASSN
         ovraccgat_t(i)=0.0
         rofaccgat_t(i)=0.0 

c
         do 128 j = 1, icc
           ancsvgac_t(i,j)=0.0    !daily accu. net photosyn. for canopy over snow subarea
           ancgvgac_t(i,j)=0.0    !daily accu. net photosyn. for canopy over ground subarea
           rmlcsvga_t(i,j)=0.0    !daily accu. leaf respiration for canopy over snow subarea
           rmlcgvga_t(i,j)=0.0    !daily accu. leaf respiration for canopy over ground subarea
           todfrac(i,j)=0.0
128      continue


         do 112 j = 1,ignd       !soil temperature and moisture over different subareas
            tbarcacc_t (i,j)=0.0
            tbarcsacc_t(i,j)=0.0
            tbargacc_t (i,j)=0.0
            tbargsacc_t(i,j)=0.0
            !thliqcacc_t(i,j)=0.0              !Comment this out for now as this is causing a Segmentation Fault, don't need it right now anyway
            !thliqgacc_t(i,j)=0.0
            !thliqacc_t(i,j)=0.0
            !thicecacc_t(i,j)=0.0
            !thicegacc_t(i,j)=0.0
112      continue
123    continue


c
c     find fcancmx with class' fcanmxs and dvdfcans read from ctem's
c     initialization file. this is to divide needle leaf and broad leaf
c     into dcd and evg, and crops and grasses into c3 and c4.
c
      do 113 j = 1, ican
        do 114 i=1,nltest
        do 114 m=1,nmtest
c
          k1c = (j-1)*l2max + 1
          k2c = k1c + (l2max - 1)
c
          do n = k1c, k2c
            if(modelpft(n).eq.1)then
              icountrow(i,m) = icountrow(i,m) + 1
              csum(i,m,j) = csum(i,m,j) +
     &         dvdfcanrow(i,m,icountrow(i,m))

!              Added in seed here to prevent competition from getting
!              pfts with no seed fraction.  JM Feb 20 2014.
              if (compete .and. .not. onetile_perPFT) then
               fcancmxrow(i,m,icountrow(i,m))=max(seed,FCANROT(i,m,j)*
     &         dvdfcanrow(i,m,icountrow(i,m)))
               barf(i,m) = barf(i,m) - fcancmxrow(i,m,icountrow(i,m))
              else
               fcancmxrow(i,m,icountrow(i,m))=FCANROT(i,m,j)*
     &         dvdfcanrow(i,m,icountrow(i,m))
              end if
            endif
          enddo
c
!           if( abs(csum(i,m,j)-1.0).gt.abszero ) then
!            write(6,1130)i,m,j
! 1130       format('dvdfcans for (',i1,',',i1,',',i1,') must add to 1.0')
!             call xit('runclass36ctem', -3)
!           endif
c
114     continue
113   continue


!     Now make sure that you aren�t over 1.0 for a tile (i.e. with a negative
!     bare ground fraction due to the seed fractions being added in.) JM Mar 9 2016
      do i=1,nltest
       do m = 1,nmtest
        if (barf(i,m) .lt. 0.) then
         bigpftc=maxloc(fcancmxrow(i,m,:))
         ! reduce the most predominant PFT by barf and 1.0e-5,
         ! which ensures that our barefraction is non-zero to avoid
         ! problems later.
         fcancmxrow(i,m,bigpftc(1))=fcancmxrow
     &                (i,m,bigpftc(1))+barf(i,m) - 1.0e-5
        end if
       end do
      end do
c
c     ----------

c     preparation with the input datasets prior to launching run:

       if(obswetf) then
        obswetyr=-99999
         do while (obswetyr .lt. metcylyrst)
            do i=1,nltest
              ! Read the values into the first tile
              read(16,*) obswetyr,(wetfrac_monrow(i,1,j),j=1,12)
              if (nmtest > 1) then
                do m = 2,nmtest !spread grid values over all tiles for easier use in model
                  wetfrac_monrow(i,m,:) = wetfrac_monrow(i,1,:)
                end do
              end if
            end do
         end do
         backspace(16)
       else !not needed, just set to 0 and move on.
         wetfrac_monrow(:,:,:) = 0.0
       end if

       if(obslght) then
        obslghtyr=-99999
        do while (obslghtyr .lt. metcylyrst)
            do i=1,nltest
              read(17,*) obslghtyr,(mlightngrow(i,1,j),j=1,12) ! read into the first tile
              if (nmtest > 1) then
                do m = 2,nmtest !spread grid values over all tiles for easier use in model
                  mlightngrow(i,m,:) = mlightngrow(i,1,:)
                end do
              end if
            end do
         end do
         backspace(17)
       end if

c      find the popd data to cycle over, popd is only cycled over when the met is cycled.

       if (cyclemet .and. popdon) then
        popyr=-99999  ! initialization, forces entry to loop below
        do while (popyr .lt. cypopyr)
         do i = 1, nltest
          read(13,5301) popyr,popdinrow(i,1) !place it in the first tile
          if (nmtest > 1) then
            do m = 2, nmtest
              popdinrow(i,m) = popdinrow(i,1) !spread this value over all tiles
            end do
          end if
         enddo
        enddo
       endif
c
      end if ! ctem_on

      iyear=-99999  ! initialization, forces entry to loop below


      if (ctem_on) then

c       if land use change switch is on then read the fractional coverages
c       of ctem's 9 pfts for the first year.
c
!        if (lnduseon .and. transient_run) then
!
!         reach_eof=.false.  !flag for when read to end of luc input file
!
!         call initialize_luc(iyear,argbuff,nmtest,nltest,
!     1                     nol2pfts,cyclemet,
!     2                     cylucyr,lucyr,FCANROT,FAREROT,nfcancmxrow,
!     3                     pfcancmxrow,fcancmxrow,reach_eof,start_bare,
!     4                     compete,onetile_perPFT)
!
!         if (reach_eof) goto 999

!       endif ! if (lnduseon)
c
c     with fcancmx calculated above and initialized values of all ctem pools,
c     find mosaic tile (grid) average vegetation biomass, litter mass, and soil c mass.
c     also initialize additional variables which are used by ctem.


      do 115 i = 1,nltest
        do 115 m = 1,nmtest
          vgbiomasrow(i,m)=0.0
          gavglairow(i,m)=0.0
          gavgltmsrow(i,m)=0.0
          gavgscmsrow(i,m)=0.0
          lucemcomrow(i,m)=0.0      !land use change combustion emission losses !Comment this out as it's causing a Segmentation Fault
          lucltrinrow(i,m)=0.0      !land use change inputs to litter pool
          lucsocinrow(i,m)=0.0      !land use change inputs to soil c pool
          colddaysrow(i,m,1)=0      !cold days counter for ndl dcd
          colddaysrow(i,m,2)=0      !cold days counter for crops

          do 116 j = 1, icc
            vgbiomasrow(i,m)=vgbiomasrow(i,m)+fcancmxrow(i,m,j)*
     &        (gleafmasrow(i,m,j)+stemmassrow(i,m,j)+
     &         rootmassrow(i,m,j)+bleafmasrow(i,m,j))
            gavgltmsrow(i,m)=gavgltmsrow(i,m)+fcancmxrow(i,m,j)*
     &                       litrmassrow(i,m,j)
            gavgscmsrow(i,m)=gavgscmsrow(i,m)+fcancmxrow(i,m,j)*
     &         soilcmasrow(i,m,j)


            grwtheffrow(i,m,j)=100.0 !set growth efficiency to some large number  !Using the Pointer for now
c                                      !so that no growth related mortality occurs in
c                                      !first year

            flhrlossrow(i,m,j)=0.0     !fall/harvest loss
            stmhrlosrow(i,m,j)=0.0     !stem harvest loss for crops  
            rothrlosrow(i,m,j)=0.0     !root death for crops  !Using the Pointer for now
            lystmmasrow(i,m,j)=stemmassrow(i,m,j)
            lyrotmasrow(i,m,j)=rootmassrow(i,m,j)
            tymaxlairow(i,m,j)=0.0


116      continue



c
c *     initialize accumulated array for monthly and yearly output for ctem
c
         call resetmonthend(nltest,nmtest)
         call resetyearend(nltest,nmtest)
c
115   continue
	

c
      do 117 i = 1,nltest
        do 117 m = 1,nmtest
	  
         gavgltmsrow(i,m)=gavgltmsrow(i,m)+ (1.0-FCANROT(i,m,1)-
     &       FCANROT(i,m,2)-
     &    FCANROT(i,m,3)-FCANROT(i,m,4))*litrmassrow(i,m,icc+1)
         gavgscmsrow(i,m)=gavgscmsrow(i,m)+ (1.0-FCANROT(i,m,1)-
     &   FCANROT(i,m,2)-
     &    FCANROT(i,m,3)-FCANROT(i,m,4))*soilcmasrow(i,m,icc+1)
c
117   continue
c
!DAN    this has already been done by MESH
!-      CALL GATPREP(ILMOS,JLMOS,IWMOS,JWMOS,
!-     1             NML,NMW,GCROW,FAREROT,MIDROT,
!-     2             NLAT,NMOS,ILG,1,NLTEST,NMTEST)

      call ctemg1(gleafmasgat,bleafmasgat,stemmassgat,rootmassgat,
     1      fcancmxgat,zbtwgat,dlzwgat,sdepgat,ailcggat,ailcbgat,
     2      ailcgat,zolncgat,rmatcgat,rmatctemgat,slaigat,
     3      bmasveggat,cmasvegcgat,veghghtgat,
     4      rootdpthgat,alvsctmgat,alirctmgat,
     5      paicgat,    slaicgat, faregat,
     6      ilmos,jlmos,iwmos,jwmos,
     7      nml,
     8      gleafmasrow,bleafmasrow,stemmassrow,rootmassrow,
     9      fcancmxrow,ZBTWROT,DLZWROT,SDEPROT,ailcgrow,ailcbrow,
     a      ailcrow,zolncrow,rmatcrow,rmatctemrow,slairow,
     b      bmasvegrow,cmasvegcrow,veghghtrow,
     c      rootdpthrow,alvsctmrow,alirctmrow,
     d      paicrow,    slaicrow, FAREROT)




      call bio2str( gleafmasgat,bleafmasgat,stemmassgat,rootmassgat,
     1                           1,      nml,    fcancmxgat, zbtwgat,
     2                        dlzwgat, nol2pfts,   sdepgat,
     4                       ailcggat, ailcbgat,  ailcgat, zolncgat,
     5                       rmatcgat, rmatctemgat,slaigat,bmasveggat,
     6                 cmasvegcgat,veghghtgat, rootdpthgat,alvsctmgat,
     7                     alirctmgat, paicgat,  slaicgat )


!     Nitrogen initialization
      IF (CTEMN) THEN
          DO I=1,NLTEST
              DO  M =1,NMTEST
              BTDPTHROW(I,M)= MAX(ROOTROT(I,M,1),ROOTROT(I,M,2),
     &                         ROOTROT(I,M,3))  !BOTTOM DRAINAGE DEPTH

!      INITIAL VCMAX0 (umol/m2/s)
                DO J=1,ICC
                  VCMX0ROW(I,M,J) = 60.0E-6          !DEFAULT VALUE
                ENDDO !J
              ENDDO !M
!Comment out these yearly values for now
              !YRNDEP(I)  = 0.0     ! YEARLY N DEPOSITION (gN/m2)
              !YRNFER(I)  = 0.0     ! YEARLY N FERTILIZATION (gN/m2)
              !YRNPLTR(I) = 0.0     ! YEARLY N LITTER-FALLING (gN/m2)
              !YRNDIS(I)  = 0.0     ! YEARLY N DISTURBANCE LOSS (gN/m2)
              !YRNLSOM(I) = 0.0     ! YEARLY N HUMIFICATION (gN/m2)
              !YRNMIN(I)  = 0.0     ! YEARLY N MINERALIZATION (gN/m2)
              !YRNNIT(I)  = 0.0     ! YEARLY N NITRIFICATION (gN/m2)
              !YRNPUP(I)  = 0.0     ! YEARLY N PLANT UPTAKE (gN/m2)
              !YRNDNIT(I) = 0.0     ! YEARLY N DENITRIFICATION (gN/m2)
              !YRNLEA(I)  = 0.0     ! YEARLY NO3 LEACHING (gN/m2)
              !YRNVOL(I)  = 0.0     ! YEARLY NH4 VOLATILIZATION(gN/m2)
              !YRNSRCE(I) = 0.0     ! YEARLY N SOURCES (gN/m2)
              !YRNLOSS(I) = 0.0     ! YEARLY N LOSSES (gN/m2)
              !YRNN2O(I)  = 0.0   ! YEARLY N2O LOSSES (gN/m2) HSuo
              !YRNN2(I)   = 0.0   ! YEARLY N2 LOSSES (gN/m2) HSuo
         ENDDO !I

         DO I=1,ILG
              CALSOIL(I)  = .TRUE.                   ! DEFINE SOIL TYPE AS CALCIOUS
            ! Currently constant, if changed should go through gather/scatter
         ENDDO

      ENDIF !CTEMN


      call ctems1(gleafmasrow,bleafmasrow,stemmassrow,rootmassrow,
     1      fcancmxrow,ZBTWROT,DLZWROT,SDEPROT,ailcgrow,ailcbrow,
     2      ailcrow,zolncrow,rmatcrow,rmatctemrow,slairow,
     3      bmasvegrow,cmasvegcrow,veghghtrow,
     4      rootdpthrow,alvsctmrow,alirctmrow,
     5      paicrow,    slaicrow,
     6      ilmos,jlmos,iwmos,jwmos,
     7      nml,
     8      gleafmasgat,bleafmasgat,stemmassgat,rootmassgat,
     9      fcancmxgat,zbtwgat,dlzwgat,sdepgat,ailcggat,ailcbgat,
     a      ailcgat,zolncgat,rmatcgat,rmatctemgat,slaigat,
     b      bmasveggat,cmasvegcgat,veghghtgat,
     c      rootdpthgat,alvsctmgat,alirctmgat,
     d      paicgat,    slaicgat)

      ! LUC and disturbance need to know the area of the gridcell. Find it here and pass into CTEM

      do i = 1, nml
        currlat(i)=radjrow(1)*180.0/pi !following rest of code, radjrow is always given index of 1 offline.
        curlatno(i)=0
      end do

      ! Find current latitude number
      do k = 1, lat
        do i = 1, nml
          if(currlat(i).ge.edgelat(k).and.
     1      currlat(i).lt.edgelat(k+1))then
            curlatno(i)=k
          endif
        end do
      end do

      do 190 j = 1, nml
        if(curlatno(j).eq.0)then
            write(6,2000)j
2000        format('cannot find current latitude no. for i = ',i3)
            call xit ('driver',-5)
        endif

        do i = 1,nml
            lath = curlatno(i)/2
            call gaussg(lath,sl,wl,cl,radl,wossl)
            call trigl(lath,sl,wl,cl,radl,wossl)
        enddo


        do i = 1, nml
            ml(i) = 1.0/real(lon) ! wl contains zonal weights, lets find meridional weights
            grclarea(i) = 4.0*pi*(earthrad**2)*wl(1)*ml(1)
     1                     *faregat(i)/2.0  ! km^2, faregat is areal fraction of each mosaic
                                            ! dividing by 2.0 because wl(1 to lat) add to 2.0 not 1.0
        end do
190    continue

c
!       ! FLAG test JM Dec 18 2015
!     Find the maximum daylength at this location for day 172 = June 21st - summer solstice.
      do i = 1, nltest
       if (radjrow(1) > 0.) then
        call finddaylength(172.0, radjrow(1),dayl_maxrow(i)) !following rest of code, radjrow is always given index of 1 offline.
       else ! S. Hemi so do N.Hemi winter solstice Dec 21
        call finddaylength(355.0, radjrow(1),dayl_maxrow(i)) !following rest of code, radjrow is always given index of 1 offline.
       end if
      end do
      ! end FLAG test JM Dec 18 2015

      endif   ! if (ctem_on)

c     ctem initial preparation done

C===================== CTEM ============================================ /
C==========================================================================================================

C     **** LAUNCH RUN. ****

      N=0
      NCOUNT=1
      NDAY=86400/NINT(DELT) !Based on number of simulations in a single year

     !Nitrogen time initialization
      do I=1,NLTEST
          NDAYTIME(I) = 0
      enddo

	WRITE(*,*) ' '
	WRITE(*,*) 'Successful: RUNCLASS36CTEM_init() is done'
	WRITE(*,*) ' '  


!Now we close the initialization subroutine
      END SUBROUTINE            !RUNCLASS36CTEM_init


      SUBROUTINE RUNCLASS36CTEM_within_tile(shd, fls, cm)


         use mpi_module
         use model_files_variables
         use sa_mesh_shared_variables
         use model_dates
         use climate_forcing
         use model_output_variabletypes
         use FLAGS

        !We don't need to add the module init or config because we already have the init subroutine/module in this module.


         type(ShedGridParams) :: shd
         type(fl_ids) :: fls
         type(clim_info) :: cm


	  !> Grab climate data for this MESH Loop
	  !The variables names were changed so that they are compatible with runclass36ctem below
	  !Note that the varibales here are long, because il1 and il2 represent the number of active grids

!DAN    MESH runs in terms of tiles, CTEM doesn't seem to implement il1:il2 (from CLASS),
!DAN    which means it should run 1:NML.

       !Since runclass36ctem still uses CLASSG, we will be putting using the ROW variables for the MET data
       !Note that the current year, day, hour and minute will also be defined here,

         iyear = ic%now%year    !Current Year
         iday = ic%now%jday     !Current Day of the Year 
         ihour = ic%now%hour    !Current Minute of the hour
         imin = ic%now%mins     !Current hour of Day	


       !Now use the updated variables from the .INI file.
       !Note that these variables have dimension ILG, which in our case is the same as NMOS/NMTEST
!DAN    ILG is the total number of active tiles (which increment by the number of active NMOS inside each grid NLAT).
!DAN    Must convert from tile indexing using ILMOS=NLAT and JLMOS=NMOS.



      DO k = 1, NML

         !Get the grid and GRU
         I = ILMOS(k)
         M = JLMOS(k)

         !Assign the values
         FSSROW(I) = cm%dat(ck%FB)%GAT(k)   !Incoming Shortwave Radiation
         FDLROW(I) = cm%dat(ck%FI)%GAT(k)   !Incoming Longwave
         PREROW(I) = cm%dat(ck%RT)%GAT(k)   !Precipitation Rate
         TAROW(I) = cm%dat(ck%TT)%GAT(k) - TFREZ   !Add the -273.16 because runclass36ctem assumes that it is in [C], while MESH has this data in [K] (Air Temperature)
         UVROW(I) = cm%dat(ck%UV)%GAT(k)    !Wind Speed
         PRESROW(I) = cm%dat(ck%P0)%GAT(k)  !Pressure
         QAROW(I) = cm%dat(ck%HU)%GAT(k)    !Specific Humidity
         QACROT(I,M) = stas%cnpy%qac(k)
         RCANROT(I,M) = stas%cnpy%rcan(k)
         SCANROT(I,M) = stas%cnpy%sncan(k)
         TACROT(I,M) = stas%cnpy%tac(k)
         TCANROT(I,M) = stas%cnpy%tcan(k)
         CMAIROT(I,M) = stas%cnpy%cmai(k)
         GROROT(I,M) = stas%cnpy%gro(k)
         SNOROT(I,M) = stas%sno%sno(k)
         ALBSROT(I,M) = stas%sno%albs(k)
         RHOSROT(I,M) = stas%sno%rhos(k)
         TSNOROT(I,M) = stas%sno%tsno(k)
         WSNOROT(I,M) = stas%sno%wsno(k)
         TPNDROT(I,M) = stas%sfc%tpnd(k)
         ZPNDROT(I,M) = stas%sfc%zpnd(k)
         TSFSROT(I,M,:) = stas%sfc%tsfs(k, :)
         TBASROT(I,M) = stas%sl%tbas(k)
         THICROT(I,M,:) = stas%sl%thic(k, :)
         THLQROT(I,M,:) = stas%sl%thlq(k, :)
         TBARROT(I,M,:) = stas%sl%tbar(k, :)
      ENDDO



!The formats from the previous subroutine do not transfer over so we'll just quickly define it again,
!It's not like declaring variables so we don't need to worry about repeating formats
5010  FORMAT(2X,6A4)
5020  FORMAT(5F10.2,F7.1,3I5)
5030  FORMAT(4F8.3,8X,4F8.3)
5040  FORMAT(9F8.3)
5050  FORMAT(6F10.2)
5060  FORMAT(7F10.3)
5070  FORMAT(2F10.4,F10.2,F10.3,F10.4,F10.3)
5080  FORMAT(3F10.1)
5090  FORMAT(4E8.1,I8)
5200  FORMAT(4I10)
5300  FORMAT(1X,I2,I3,I5,I6,2F9.2,E14.4,F9.2,E12.3,F8.2,F12.2,3F9.2,
     1       F9.4)
5301  FORMAT(I5,F10.4)
6001  FORMAT('#CLASS TEST RUN:     ',6A4)
6002  FORMAT('#RESEARCHER:         ',6A4)
6003  FORMAT('#INSTITUTION:        ',6A4)

!Since this is a different SUBROUTINE, we need to redefine the variables used in the within tile subroutine


!The following CTEM initialization is used 
C===================== CTEM ============================================ \

      run_model=.true.
      met_rewound=.false.

	!This was the space for met_rewound, however it was hard-coded to always be false so don't bother here


C===================== CTEM ============================================ /

C
      N=N+1 !Extend the iteration

      DO 250 I=1,NMTEST
C         THIS READS IN ONE 30 MIN SLICE OF MET DATA, WHEN IT REACHES
C         THE END OF FILE IT WILL GO TO 999.
!          READ(12,5300,END=999) IHOUR,IMIN,IDAY,IYEAR,FSSROW(I),
!     1        FDLROW(I),PREROW(I),TAROW(I),QAROW(I),UVROW(I),PRESROW(I)

        if (leap.and.(ihour.eq.0).and.(imin.eq.0).and.(iday.eq.1)) then

        ! We do not check the MET files to make sure the incoming MET is in fact
        ! 366 days if leapnow. You must verify this in your own input files. Later
        ! in the code it will fail and print an error message to screen warning you
        ! that your file is not correct.
          if (leapnow) then ! adjust the calendars and set the error check.
            monthdays = (/ 31,29,31,30,31,30,31,31,30,31,30,31 /)
            monthend = (/ 0,31,60,91,121,152,182,213,244,274,305,
     &                    335,366 /)
        !======Added thing to fix leap Years Bug====================
        !===================Stefan Sauer August/2017===========
            jhhendd = 366 !day of the year to stop writing the half-hourly output
            jdendd  = 366 !day of the year to stop writing the daily output

            mmday = (/ 16,46,76,107,137,168,198,229,260,290,321,351 /)
          else ! common years
            monthdays = (/ 31,28,31,30,31,30,31,31,30,31,30,31 /)
            monthend = (/ 0,31,59,90,120,151,181,212,243,273,304,334,
     &                  365 /)
            mmday = (/ 16,46,75,106,136,167,197,228,259,289,320,350 /)

          end if
        end if
250   CONTINUE


C===================== CTEM ============================================ \
c         Assign the met climate year to climiyear
          climiyear = iyear

!         If in a transient_run that has to cycle over MET then change
!         the iyear here:
          if (transient_run .and. cyclemet) then
            iyear = iyear - (metcylyrst - trans_startyr)
          end if
c
          if(lopcount .gt. 1) then
            if (cyclemet) then
              iyear=iyear + nummetcylyrs*(lopcount-1)
            else
              iyear=iyear + ncyear*(lopcount-1)
            end if
          endif   ! lopcount .gt. 1

!         write(*,*)'year=',iyear,'day=',iday,' hour=',ihour,' min=',imin
C===================== CTEM ============================================ /


!DAN    This doesn't seem right; closed NMOS loop above, opened NLAT loop here.
!DAN    What happens in the multiple grid case with only 1 GRU is that it applies
!DAN    transformations to only grid 1; results in TSOLVE errors where TA is still in
!DAN    degrees C in all other grids (because the transformations haven't been applied).
      DO I=1,NLAT
          FSVHROW(I)=0.5*FSSROW(I)
          FSIHROW(I)=0.5*FSSROW(I)
          TAROW(I)=TAROW(I)+TFREZ
          ULROW(I)=UVROW(I)
          VLROW(I)=0.0
          VMODROW(I)=UVROW(I)

          !In the new four-band albedo calculation for snow, the incoming
          ! radiation for snow or bare soil is now passed into TSOLVE via this new array:
          FSSBROL(I,1)=FSVHROW(I)
          FSSBROL(I,2)=FSIHROW(I)

      ENDDO

C
      DAY=REAL(IDAY)+(REAL(IHOUR)+REAL(IMIN)/60.)/24.
      if (leapnow) then 
        DECL=SIN(2.*PI*(284.+DAY)/366.)*23.45*PI/180.
      else
        DECL=SIN(2.*PI*(284.+DAY)/365.)*23.45*PI/180.
      endif 

      HOUR=(REAL(IHOUR)+REAL(IMIN)/60.)*PI/12.-PI
      COSZ=SIN(RADJROW(1))*SIN(DECL)+COS(RADJROW(1))*COS(DECL)*COS(HOUR)

      DO 300 I=1,NLTEST
          CSZROW(I)=SIGN(MAX(ABS(COSZ),1.0E-3),COSZ)
          IF(PREROW(I).GT.0.) THEN
              XDIFFUS(I)=1.0
          ELSE
              XDIFFUS(I)=MAX(0.0,MIN(1.0-0.9*COSZ,1.0))
          ENDIF
          FCLOROW(I)=XDIFFUS(I)
300   CONTINUE
C
		

C===================== CTEM ============================================ \
C
      ! If needed, read in the accessory input files (popd, wetlands, lightining...)
      if (iday.eq.1.and.ihour.eq.0.and.imin.eq.0) then

            if (ctem_on) then
             do i=1,nltest
              if (obswetf) then
              ! FLAG note that this will be read in, regardless of the iyear, if the
              ! obswetf flag is true. This means you have to be restarting from a run
              ! that ends the year prior to the first year in this file.
              ! Read into the first tile position
                 read(16,*) obswetyr,
     1                               (wetfrac_monrow(i,1,j),j=1,12)
                if (nmtest > 1) then
                  do m = 2,nmtest !spread grid values over all tiles for easier use in model
                    wetfrac_monrow(i,m,:) = wetfrac_monrow(i,1,:)
                  end do
                end if
              else
                wetfrac_monrow(:,:,:) = 0.0
              endif !obswetf

              if(obslght) then
              ! FLAG note that this will be read in, regardless of the iyear, if the
              ! obswetf flag is true. This means you have to be restarting from a run
              ! that ends the year prior to the first year in this file.
                read(17,*,end=312) obslghtyr,(mlightngrow(i,1,j),j=1,12) ! read into the first tile
                if (nmtest > 1) then
                  do m = 2,nmtest !spread grid values over all tiles for easier use in model
                    mlightngrow(i,m,:) = mlightngrow(i,1,:)
                  end do
                end if
312             continue !if end of file, just keep using the last year of lighting data.
              end if !obslight
             end do
            endif ! ctem_on

c         If popdon=true, calculate fire extinguishing probability and
c         probability of fire due to human causes from population density
c         input data. In disturb.f90 this will overwrite extnprobrow
c         and prbfrhucgrd that are read in from the .ctm file. Set
c         cypopyr = -9999 when we don't want to cycle over the popd data
c         so this allows us to grab a new value each year.

          if(popdon .and. transient_run) then
            do while (popyr .lt. iyear)
             do i=1,nltest
              read(13,5301) popyr,popdinrow(i,1) !place it in the first tile
              if (nmtest > 1) then
                do m = 2, nmtest
                  popdinrow(i,m) = popdinrow(i,1) !spread this value over all tiles
                end do
              end if
             enddo
            enddo
          endif

c         If co2on is true, read co2concin from input datafile and
c         overwrite co2concrow, otherwise set to constant value.
!         Same applies to CH4.

          if(co2on .or. ch4on) then
           if (transient_run) then
                testyr = iyear
            do while (co2yr .lt. testyr)
             do i=1,nltest
              read(14,*) co2yr,co2concin,ch4concin
              do m=1,nmtest
                if (co2on) co2concrow(i,m)=co2concin
                if (ch4on) ch4concrow(i,m)=ch4concin
              enddo
             enddo
            enddo !co2yr < testyr
           else ! still spinning but you apparently want the CO2 to move forward with time
                ! we assume the year you want to start from here is trans_startyr
                testyr = trans_startyr
                ! Now make sure we end up starting from the testyr
                if (co2yr .lt. testyr) then
                 do while (co2yr .lt. testyr)
                  do i=1,nltest
                    read(14,*) co2yr,co2concin,ch4concin
                   do m=1,nmtest
                    if (co2on) co2concrow(i,m)=co2concin
                    if (ch4on) ch4concrow(i,m)=ch4concin
                   enddo
                  enddo
                 enddo !co2yr < testyr
                else ! years beyond the first, just go up in years without paying attention to iyear (since it is cycling)
                   do i=1,nltest
                    read(14,*) co2yr,co2concin,ch4concin
                   do m=1,nmtest
                    if (co2on) co2concrow(i,m)=co2concin
                    if (ch4on) ch4concrow(i,m)=ch4concin
                   enddo
                  enddo
                end if
           end if !transient_run or not
          end if !co2 or ch4on

          if (.not. co2on .or. .not. ch4on) then !constant co2 or ch4
            do i=1,nltest
             do m=1,nmtest
              if (.not. co2on) co2concrow(i,m)=setco2conc
              if (.not. co2on) ch4concrow(i,m)=setch4conc
             enddo
            enddo
          endif

c         If lnduseon is true, read in the luc data now

          if (ctem_on .and. lnduseon .and. transient_run) then

            !call readin_luc(iyear,nmtest,nltest,lucyr,
!     &                   nfcancmxrow,pfcancmxrow,reach_eof,compete,
!     &                   onetile_perPFT)
!            if (reach_eof) goto 999
!
!          else ! lnduseon = false or met is cycling in a spin up run

c         Land use is not on or the met data is being cycled, so the
c         pfcancmx value is also the nfcancmx value.

           nfcancmxrow=pfcancmxrow

          endif ! lnduseon/cyclemet

      endif   ! at the first day of each year i.e.
c             ! if (iday.eq.1.and.ihour.eq.0.and.imin.eq.0)

      !       ! FLAG test JM Dec 18 2015
      if (ihour.eq.0.and.imin.eq.0) then ! first time step of the day
      ! Find the daylength of this day
        do i = 1, nltest
          call finddaylength(real(iday), radjrow(1), daylrow(i)) !following rest of code, radjrow is always given index of 1 offline.
        end do
      end if
      ! end FLAG test JM Dec 18 2015


C===================== CTEM ============================================ /



      CALL CLASSI(VPDROW,TADPROW,PADRROW,RHOAROW,RHSIROW,
     1            RPCPROW,TRPCROW,SPCPROW,TSPCROW,TAROW,QAROW,
     2            PREROW,RPREROW,SPREROW,PRESROW,
     3            IPCP,NLAT,1,NLTEST)

C
      CUMSNO=CUMSNO+SPCPROW(1)*RHSIROW(1)*DELT


      CALL CLASSG (TBARGAT,THLQGAT,THICGAT,TPNDGAT,ZPNDGAT,
     1             TBASGAT,ALBSGAT,TSNOGAT,RHOSGAT,SNOGAT,
     2             TCANGAT,RCANGAT,SCANGAT,GROGAT, CMAIGAT,
     3             FCANGAT,LNZ0GAT,ALVCGAT,ALICGAT,PAMXGAT,
     4             PAMNGAT,CMASGAT,ROOTGAT,RSMNGAT,QA50GAT,
     5             VPDAGAT,VPDBGAT,PSGAGAT,PSGBGAT,PAIDGAT,
     6             HGTDGAT,ACVDGAT,ACIDGAT,TSFSGAT,WSNOGAT,
     7             THPGAT, THRGAT, THMGAT, BIGAT,  PSISGAT,
     8             GRKSGAT,THRAGAT,HCPSGAT,TCSGAT, IGDRGAT,
     9             THFCGAT,THLWGAT,PSIWGAT,DLZWGAT,ZBTWGAT,
     A             VMODGAT,ZSNLGAT,ZPLGGAT,ZPLSGAT,TACGAT,
     B             QACGAT,DRNGAT,DDGAT,XSLPGAT,GRKFGAT,WFSFGAT,
     C             WFCIGAT,ALGWVGAT,ALGWNGAT,ALGDVGAT,ALGDNGAT,
     +             ALGWGAT,ALGDGAT,ASVDGAT,ASIDGAT,AGVDGAT,
     D             AGIDGAT,ISNDGAT,RADJGAT,ZBLDGAT,Z0ORGAT,
     E             ZRFMGAT,ZRFHGAT,ZDMGAT, ZDHGAT, FSVHGAT,
     F             FSIHGAT,FSDBGAT,FSFBGAT,FSSBGAT,CSZGAT,
     +             FSGGAT, FLGGAT, FDLGAT, ULGAT,  VLGAT,
     G             TAGAT,  QAGAT,  PRESGAT,PREGAT, PADRGAT,
     H             VPDGAT, TADPGAT,RHOAGAT,RPCPGAT,TRPCGAT,
     I             SPCPGAT,TSPCGAT,RHSIGAT,FCLOGAT,DLONGAT,
     J             GGEOGAT,GUSTGAT,REFGAT, BCSNGAT,DEPBGAT,
     K             ILMOS,JLMOS,
     L             NML,NLAT,NTLD,NMOS,ILG,IGND,ICAN,ICAN+1,NBS,
     M             TBARROT,THLQROT,THICROT,TPNDROT,ZPNDROT,
     N             TBASROT,ALBSROT,TSNOROT,RHOSROT,SNOROT,
     O             TCANROT,RCANROT,SCANROT,GROROT, CMAIROT,
     P             FCANROT,LNZ0ROT,ALVCROT,ALICROT,PAMXROT,
     Q             PAMNROT,CMASROT,ROOTROT,RSMNROT,QA50ROT,
     R             VPDAROT,VPDBROT,PSGAROT,PSGBROT,PAIDROT,
     S             HGTDROT,ACVDROT,ACIDROT,TSFSROT,WSNOROT,
     T             THPROT, THRROT, THMROT, BIROT,  PSISROT,
     U             GRKSROT,THRAROT,HCPSROT,TCSROT, IGDRROT,
     V             THFCROT,THLWROT,PSIWROT,DLZWROT,ZBTWROT,
     W             VMODROW,ZSNLROT,ZPLGROT,ZPLSROT,TACROT,
     X             QACROT,DRNROT,DDROT,XSLPROT,GRKFROT,WFSFROT,
     Y             WFCIROT,ALGWVROT,ALGWNROT,ALGDVROT,ALGDNROT,
     +             ALGWROT,ALGDROT,ASVDROT,ASIDROT,AGVDROT,
     Z             AGIDROT,ISNDROT,RADJROW,ZBLDROW,Z0ORROW,
     +             ZRFMROW,ZRFHROW,ZDMROW, ZDHROW, FSVHROW,
     +             FSIHROW,FSDBROL,FSFBROL,FSSBROL,CSZROW,
     +             FSGROL, FLGROL, FDLROW, ULROW,  VLROW,
     +             TAROW,  QAROW,  PRESROW,PREROW, PADRROW,
     +             VPDROW, TADPROW,RHOAROW,RPCPROW,TRPCROW,
     +             SPCPROW,TSPCROW,RHSIROW,FCLOROW,DLONROW,
     +             GGEOROW,GUSTROL,REFROT, BCSNROT,DEPBROW )


C
C    * INITIALIZATION OF DIAGNOSTIC VARIABLES SPLIT OUT OF CLASSG
C    * FOR CONSISTENCY WITH GCM APPLICATIONS.
C
      DO 330 K=1,ILG
          CDHGAT (K)=0.0
          CDMGAT (K)=0.0
          HFSGAT (K)=0.0
          TFXGAT (K)=0.0
          QEVPGAT(K)=0.0
          QFSGAT (K)=0.0
          QFXGAT (K)=0.0
          PETGAT (K)=0.0
          GAGAT  (K)=0.0
          EFGAT  (K)=0.0
          GTGAT  (K)=0.0
          QGGAT  (K)=0.0
          ALVSGAT(K)=0.0
          ALIRGAT(K)=0.0
          SFCTGAT(K)=0.0
          SFCUGAT(K)=0.0
          SFCVGAT(K)=0.0
          SFCQGAT(K)=0.0
          FSNOGAT(K)=0.0
          FSGVGAT(K)=0.0
          FSGSGAT(K)=0.0
          FSGGGAT(K)=0.0
          FLGVGAT(K)=0.0
          FLGSGAT(K)=0.0
          FLGGGAT(K)=0.0
          HFSCGAT(K)=0.0
          HFSSGAT(K)=0.0
          HFSGGAT(K)=0.0
          HEVCGAT(K)=0.0
          HEVSGAT(K)=0.0
          HEVGGAT(K)=0.0
          HMFCGAT(K)=0.0
          HMFNGAT(K)=0.0
          HTCCGAT(K)=0.0
          HTCSGAT(K)=0.0
          PCFCGAT(K)=0.0
          PCLCGAT(K)=0.0
          PCPNGAT(K)=0.0
          PCPGGAT(K)=0.0
          QFGGAT (K)=0.0
          QFNGAT (K)=0.0
          QFCFGAT(K)=0.0
          QFCLGAT(K)=0.0
          ROFGAT (K)=0.0
          ROFOGAT(K)=0.0
          ROFSGAT(K)=0.0
          ROFBGAT(K)=0.0
          TROFGAT(K)=0.0
          TROOGAT(K)=0.0
          TROSGAT(K)=0.0
          TROBGAT(K)=0.0
          ROFCGAT(K)=0.0
          ROFNGAT(K)=0.0
          ROVGGAT(K)=0.0
          WTRCGAT(K)=0.0
          WTRSGAT(K)=0.0
          WTRGGAT(K)=0.0
          DRGAT  (K)=0.0
330   CONTINUE
C
      DO 334 L=1,IGND
      DO 332 K=1,ILG
          HMFGGAT(K,L)=0.0
          HTCGAT (K,L)=0.0
          QFCGAT (K,L)=0.0
          GFLXGAT(K,L)=0.0
332   CONTINUE
334   CONTINUE
C
      DO 340 M=1,50
          DO 338 L=1,6
              DO 336 K=1,NML
                  ITCTGAT(K,L,M)=0
336           CONTINUE
338       CONTINUE
340   CONTINUE

C

      CALL CLASSZ (0,      CTVSTP, CTSSTP,
     1             CTNSTP, WTVSTP, WTSSTP, WTGSTP,
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
C===================== CTEM ============================================ \
C
      call ctemg2(fcancmxgat,rmatcgat,zolncgat,paicgat,
     1      ailcgat,     ailcggat,    cmasvegcgat,  slaicgat,
     2      ailcgsgat,   fcancsgat,   fcancgat,     rmatctemgat,
     3      co2concgat,  co2i1cggat,  co2i1csgat,   co2i2cggat,
     4      co2i2csgat,  xdiffusgat,  slaigat,      cfluxcggat,
     5      cfluxcsgat,  ancsveggat,  ancgveggat,   rmlcsveggat,
     6      rmlcgveggat, canresgat,   sdepgat,      ch4concgat,
     7      sandgat,     claygat,     orgmgat,
     8      anveggat,    rmlveggat,   tcanoaccgat_t,tbaraccgat_t,
     9      uvaccgat_t,  vvaccgat_t,  mlightnggat,  prbfrhucgat,
     a      extnprobgat, stdalngat,   pfcancmxgat,  nfcancmxgat,
     b      stemmassgat, rootmassgat, litrmassgat,  gleafmasgat,
     c      bleafmasgat, soilcmasgat, ailcbgat,     flhrlossgat,
     d      pandaysgat,  lfstatusgat, grwtheffgat,  lystmmasgat,
     e      lyrotmasgat, tymaxlaigat, vgbiomasgat,  gavgltmsgat,
     f      stmhrlosgat, bmasveggat,  colddaysgat,  rothrlosgat,
     g      alvsctmgat,  alirctmgat,  gavglaigat,   nppgat,
     h      nepgat,      hetroresgat, autoresgat,   soilcrespgat,
     i      rmgat,       rggat,       nbpgat,       litresgat,
     j      socresgat,   gppgat,      dstcemlsgat,  litrfallgat,
     k      humiftrsgat, veghghtgat,  rootdpthgat,  rmlgat,
     l      rmsgat,      rmrgat,      tltrleafgat,  tltrstemgat,
     m      tltrrootgat, leaflitrgat, roottempgat,  afrleafgat,
     n      afrstemgat,  afrrootgat,  wtstatusgat,  ltstatusgat,
     o      burnfracgat, smfuncveggat, lucemcomgat,  lucltringat,
     p      lucsocingat, nppveggat,   dstcemls3gat, popdingat,
     q      faregat,     gavgscmsgat, rmlvegaccgat, pftexistgat,
     &      rmsveggat,   rmrveggat,   rgveggat,    vgbiomas_veggat,
     &      gppveggat,   nepveggat,   ailcmingat,   ailcmaxgat,
     &      emit_co2gat,  emit_cogat, emit_ch4gat,  emit_nmhcgat,
     &      emit_h2gat,   emit_noxgat,emit_n2ogat,  emit_pm25gat,
     &      emit_tpmgat,  emit_tcgat, emit_ocgat,   emit_bcgat,
     &      btermgat,     ltermgat,   mtermgat, daylgat,dayl_maxgat,
     &      nbpveggat,    hetroresveggat, autoresveggat,litresveggat,
     &      soilcresveggat, burnvegfgat, pstemmassgat, pgleafmassgat,
     &      ch4wet1gat, ch4wet2gat,  slopefracgat, wetfrac_mongat,
     &      wetfdyngat, ch4dyn1gat,  ch4dyn2gat, ch4soillsgat,
     &      twarmmgat,    tcoldmgat,     gdd5gat,
     1      ariditygat, srplsmongat,  defctmongat, anndefctgat,
     2      annsrplsgat,   annpcpgat,  dry_season_lengthgat,
     r      ilmos,       jlmos,       iwmos,        jwmos,
     s      nml,      fcancmxrow,  rmatcrow,    zolncrow,  paicrow,
     v      ailcrow,     ailcgrow,    cmasvegcrow,  slaicrow,
     w      ailcgsrow,   fcancsrow,   fcancrow,     rmatctemrow,
     x      co2concrow,  co2i1cgrow,  co2i1csrow,   co2i2cgrow,
     y      co2i2csrow,  xdiffus,     slairow,      cfluxcgrow,
     z      cfluxcsrow,  ancsvegrow,  ancgvegrow,   rmlcsvegrow,
     1      rmlcgvegrow, canresrow,   SDEPROT,      ch4concrow,
     2      SANDROT,     CLAYROT,     ORGMROT,
     3      anvegrow,    rmlvegrow,   tcanoaccrow_t,tbaraccrow_t,
     4      uvaccrow_t,  vvaccrow_t,  mlightngrow,  prbfrhucrow,
     5      extnprobrow, stdalnrow,   pfcancmxrow,  nfcancmxrow,
     6      stemmassrow, rootmassrow, litrmassrow,  gleafmasrow,
     7      bleafmasrow, soilcmasrow, ailcbrow,     flhrlossrow,
     8      pandaysrow,  lfstatusrow, grwtheffrow,  lystmmasrow,
     9      lyrotmasrow, tymaxlairow, vgbiomasrow,  gavgltmsrow,
     a      stmhrlosrow, bmasvegrow,  colddaysrow,  rothrlosrow,
     b      alvsctmrow,  alirctmrow,  gavglairow,   npprow,
     c      neprow,      hetroresrow, autoresrow,   soilcresprow,
     d      rmrow,       rgrow,       nbprow,       litresrow,
     e      socresrow,   gpprow,      dstcemlsrow,  litrfallrow,
     f      humiftrsrow, veghghtrow,  rootdpthrow,  rmlrow,
     g      rmsrow,      rmrrow,      tltrleafrow,  tltrstemrow,
     h      tltrrootrow, leaflitrrow, roottemprow,  afrleafrow,
     i      afrstemrow,  afrrootrow,  wtstatusrow,  ltstatusrow,
     j      burnfracrow, smfuncvegrow, lucemcomrow,  lucltrinrow,
     k      lucsocinrow, nppvegrow,   dstcemls3row, popdinrow,
     l      FAREROT,     gavgscmsrow, rmlvegaccrow, pftexistrow,
     &      rmsvegrow,   rmrvegrow,   rgvegrow,    vgbiomas_vegrow,
     &      gppvegrow,   nepvegrow,   ailcminrow,   ailcmaxrow,
     &      emit_co2row,  emit_corow, emit_ch4row,  emit_nmhcrow,
     &      emit_h2row,   emit_noxrow,emit_n2orow,  emit_pm25row,
     &      emit_tpmrow,  emit_tcrow, emit_ocrow,   emit_bcrow,
     &      btermrow,     ltermrow,   mtermrow, daylrow, dayl_maxrow,
     &      nbpvegrow,    hetroresvegrow, autoresvegrow,litresvegrow,
     &      soilcresvegrow, burnvegfrow, pstemmassrow, pgleafmassrow,
     &      ch4wet1row, ch4wet2row,  slopefracrow, wetfrac_monrow,
     &      wetfdynrow, ch4dyn1row, ch4dyn2row, ch4soillsrow,
     &      twarmmrow,    tcoldmrow,     gdd5row,
     1      aridityrow, srplsmonrow,  defctmonrow, anndefctrow,
     2      annsrplsrow,   annpcprow,  dry_season_lengthrow)
c
C===================== CTEM ============================================ /
C
C-----------------------------------------------------------------------
C     * ALBEDO AND TRANSMISSIVITY CALCULATIONS; GENERAL VEGETATION
C     * CHARACTERISTICS.

C     * ADAPTED TO COUPLING OF CLASS3.6 AND CTEM by including: zolnc,
!     * cmasvegc, alvsctm, alirctm in the arguments.




C
      CALL CLASSA    (FC,     FG,     FCS,    FGS,    ALVSCN, ALIRCN,
     1                ALVSG,  ALIRG,  ALVSCS, ALIRCS, ALVSSN, ALIRSN,
     2                ALVSGC, ALIRGC, ALVSSC, ALIRSC, TRVSCN, TRIRCN,
     3                TRVSCS, TRIRCS, FSVF,   FSVFS,
     4                RAICAN, RAICNS, SNOCAN, SNOCNS, FRAINC, FSNOWC,
     5                FRAICS, FSNOCS, DISP,   DISPS,  ZOMLNC, ZOMLCS,
     6                ZOELNC, ZOELCS, ZOMLNG, ZOMLNS, ZOELNG, ZOELNS,
     7                CHCAP,  CHCAPS, CMASSC, CMASCS, CWLCAP, CWFCAP,
     8                CWLCPS, CWFCPS, RC,     RCS,    RBCOEF, FROOT,
     9                FROOTS, ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, ZSNOW,
     A                WSNOGAT,ALVSGAT,ALIRGAT,HTCCGAT,HTCSGAT,HTCGAT,
     +                ALTG,   ALSNO,  TRSNOWC,TRSNOWG,
     B                WTRCGAT,WTRSGAT,WTRGGAT,CMAIGAT,FSNOGAT,
     C                FCANGAT,LNZ0GAT,ALVCGAT,ALICGAT,PAMXGAT,PAMNGAT,
     D                CMASGAT,ROOTGAT,RSMNGAT,QA50GAT,VPDAGAT,VPDBGAT,
     E                PSGAGAT,PSGBGAT,PAIDGAT,HGTDGAT,ACVDGAT,ACIDGAT,
     F                ASVDGAT,ASIDGAT,AGVDGAT,AGIDGAT,ALGWGAT,ALGDGAT,
     +                ALGWVGAT,ALGWNGAT,ALGDVGAT,ALGDNGAT,
     G                THLQGAT,THICGAT,TBARGAT,RCANGAT,SCANGAT,TCANGAT,
     H                GROGAT, SNOGAT, TSNOGAT,RHOSGAT,ALBSGAT,ZBLDGAT,
     I                Z0ORGAT,ZSNLGAT,ZPLGGAT,ZPLSGAT,
     J                FCLOGAT,TAGAT,  VPDGAT, RHOAGAT,CSZGAT,
     +                FSDBGAT,FSFBGAT,REFGAT, BCSNGAT,
     K                FSVHGAT,RADJGAT,DLONGAT,RHSIGAT,DELZ,   DLZWGAT,
     L                ZBTWGAT,THPGAT, THMGAT, PSISGAT,BIGAT,  PSIWGAT,
     M                HCPSGAT,ISNDGAT,
     P                FCANCMXGAT,ICC,ctem_on,RMATCGAT,ZOLNCGAT,
     Q                CMASVEGCGAT,AILCGAT,PAICGAT,L2MAX, NOL2PFTS,
     R                SLAICGAT,AILCGGAT,AILCGSGAT,FCANCGAT,FCANCSGAT,
     R                IDAY,   ILG,  1,      NML,  NBS,
     N                JLAT,N, ICAN,   ICAN+1, IGND,   IDISP,  IZREF,
     O                IWF,    IPAI,   IHGT,   IALC,   IALS,   IALG,
     P                ISNOALB,IGRALB, alvsctmgat,alirctmgat )



C
C-----------------------------------------------------------------------
C          * SURFACE TEMPERATURE AND FLUX CALCULATIONS.

C          * ADAPTED TO COUPLING OF CLASS3.6 AND CTEM
!          * by including in the arguments: lfstatus



      CALL CLASST     (TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG,
     1  THICEC, THICEG, HCPC,   HCPG,   TCTOPC, TCBOTC, TCTOPG, TCBOTG,
     2  GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,   G12CS,  G12GS,
     3  G23C,   G23G,   G23CS,  G23GS,  QFREZC, QFREZG, QMELTC, QMELTG,
     4  EVAPC,  EVAPCG, EVAPG,  EVAPCS, EVPCSG, EVAPGS, TCANO,  TCANS,
     5  RAICAN, SNOCAN, RAICNS, SNOCNS, CHCAP,  CHCAPS, TPONDC, TPONDG,
     6  TPNDCS, TPNDGS, TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     7  ITCTGAT,CDHGAT, CDMGAT, HFSGAT, TFXGAT, QEVPGAT,QFSGAT,
     8  PETGAT, GAGAT,  EFGAT,  GTGAT,  QGGAT,
     +  SFCTGAT,SFCUGAT,SFCVGAT,SFCQGAT,SFRHGAT,
     +  GTBS,   SFCUBS, SFCVBS, USTARBS,
     9  FSGVGAT,FSGSGAT,FSGGGAT,FLGVGAT,FLGSGAT,FLGGGAT,
     A  HFSCGAT,HFSSGAT,HFSGGAT,HEVCGAT,HEVSGAT,HEVGGAT,HMFCGAT,HMFNGAT,
     B  HTCCGAT,HTCSGAT,HTCGAT, QFCFGAT,QFCLGAT,DRGAT,  WTABGAT,ILMOGAT,
     C  UEGAT,  HBLGAT, TACGAT, QACGAT, ZRFMGAT,ZRFHGAT,ZDMGAT, ZDHGAT,
     D  VPDGAT, TADPGAT,RHOAGAT,FSVHGAT,FSIHGAT,FDLGAT, ULGAT,  VLGAT,
     E  TAGAT,  QAGAT,  PADRGAT,FC,     FG,     FCS,    FGS,    RBCOEF,
     F  FSVF,   FSVFS,  PRESGAT,VMODGAT,ALVSCN, ALIRCN, ALVSG,  ALIRG,
     G  ALVSCS, ALIRCS, ALVSSN, ALIRSN, ALVSGC, ALIRGC, ALVSSC, ALIRSC,
     H  TRVSCN, TRIRCN, TRVSCS, TRIRCS, RC,     RCS,    WTRGGAT,QLWOGAT,
     I  FRAINC, FSNOWC, FRAICS, FSNOCS, CMASSC, CMASCS, DISP,   DISPS,
     J  ZOMLNC, ZOELNC, ZOMLNG, ZOELNG, ZOMLCS, ZOELCS, ZOMLNS, ZOELNS,
     K  TBARGAT,THLQGAT,THICGAT,TPNDGAT,ZPNDGAT,TBASGAT,TCANGAT,TSNOGAT,
     L  ZSNOW,  RHOSGAT,WSNOGAT,THPGAT, THRGAT, THMGAT, THFCGAT,THLWGAT,
     +  TRSNOWC,TRSNOWG,ALSNO,  FSSBGAT, FROOT, FROOTS,
     M  RADJGAT,PREGAT, HCPSGAT,TCSGAT, TSFSGAT,DELZ,   DLZWGAT,ZBTWGAT,
     N  FTEMP,  FVAP,   RIB,    ISNDGAT,
     O  AILCGGAT,  AILCGSGAT, FCANCGAT,FCANCSGAT,CO2CONCGAT,CO2I1CGGAT,
     P  CO2I1CSGAT,CO2I2CGGAT,CO2I2CSGAT,CSZGAT,XDIFFUSGAT,SLAIGAT,ICC,
     Q  ctem_on,RMATCTEMGAT,FCANCMXGAT,L2MAX,  NOL2PFTS,CFLUXCGGAT,
     R  CFLUXCSGAT,ANCSVEGGAT,ANCGVEGGAT,RMLCSVEGGAT,RMLCGVEGGAT,
     S  TCSNOW,GSNOW,ITC,ITCG,ITG,    ILG,    1,NML,  JLAT,N, ICAN,
     T  IGND,   IZREF,  ISLFD,  NLANDCS,NLANDGS,NLANDC, NLANDG, NLANDI,
     U  NBS,    ISNOALB,lfstatusgat,daylgat, dayl_maxgat)



          CALL CLASSW  (THLQGAT,THICGAT,TBARGAT,TCANGAT,RCANGAT,SCANGAT,
     1                  ROFGAT, TROFGAT,SNOGAT, TSNOGAT,RHOSGAT,ALBSGAT,
     2                  WSNOGAT,ZPNDGAT,TPNDGAT,GROGAT, TBASGAT,
     3                  GFLXGAT,
     4                  PCFCGAT,PCLCGAT,PCPNGAT,PCPGGAT,QFCFGAT,QFCLGAT,
     5                  QFNGAT, QFGGAT, QFCGAT, HMFCGAT,HMFGGAT,HMFNGAT,
     6                  HTCCGAT,HTCSGAT,HTCGAT, ROFCGAT,ROFNGAT,ROVGGAT,
     7                  WTRSGAT,WTRGGAT,ROFOGAT,ROFSGAT,ROFBGAT,
     8                  TROOGAT,TROSGAT,TROBGAT,QFSGAT, QFXGAT, RHOAGAT,
     9                  TBARC,  TBARG,  TBARCS, TBARGS, THLIQC, THLIQG,
     A                  THICEC, THICEG, HCPC,   HCPG,   RPCPGAT,TRPCGAT,
     B                  SPCPGAT,TSPCGAT,PREGAT, TAGAT,  RHSIGAT,GGEOGAT,
     C                  FC,     FG,     FCS,    FGS,    TPONDC, TPONDG,
     D                  TPNDCS, TPNDGS, EVAPC,  EVAPCG, EVAPG,  EVAPCS,
     E                  EVPCSG, EVAPGS, QFREZC, QFREZG, QMELTC, QMELTG,
     F                  RAICAN, SNOCAN, RAICNS, SNOCNS, FSVF,    FSVFS,
     G                  CWLCAP, CWFCAP, CWLCPS, CWFCPS, TCANO,
     H                  TCANS,  CHCAP,  CHCAPS, CMASSC, CMASCS, ZSNOW,
     I                  GZEROC, GZEROG, GZROCS, GZROGS, G12C,   G12G,
     J                  G12CS,  G12GS,  G23C,   G23G,   G23CS,  G23GS,
     K                  TSNOCS, TSNOGS, WSNOCS, WSNOGS, RHOSCS, RHOSGS,
     L                  ZPLIMC, ZPLIMG, ZPLMCS, ZPLMGS, TSFSGAT,
     M                  TCTOPC, TCBOTC, TCTOPG, TCBOTG, FROOT,   FROOTS,
     N                  THPGAT, THRGAT, THMGAT, BIGAT,  PSISGAT,GRKSGAT,
     O                  THRAGAT,THFCGAT,DRNGAT, HCPSGAT,DELZ,
     P                  DLZWGAT,ZBTWGAT,XSLPGAT,GRKFGAT,WFSFGAT,WFCIGAT,
     Q                  ISNDGAT,IGDRGAT,
     R                  IWF,    ILG,    1,      NML,    N,
     S                  JLAT,   ICAN,   IGND,   IGND+1, IGND+2,
     T                  NLANDCS,NLANDGS,NLANDC, NLANDG, NLANDI,
     U                  MANNING_N, DDGAT, BULK_FC)

	                          
C=======================================================================

      CALL CLASSZ (1,      CTVSTP, CTSSTP,
     1             CTNSTP, WTVSTP, WTSSTP, WTGSTP,
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

C===================== CTEM ============================================ \
c
c     accumulate variables not already accumulated but which are required by
c     ctem.
c


      if (ctem_on) then
        do 700 i = 1, nml
c
          alswacc_gat(i)=alswacc_gat(i)+alvsgat(i)*fsvhgat(i)
          allwacc_gat(i)=allwacc_gat(i)+alirgat(i)*fsihgat(i)
          fsinacc_gat(i)=fsinacc_gat(i)+FSSROW(1) ! FLAG! Do this offline only (since all tiles
                                                  ! are the same in a gridcell and we run
                                                  ! only one gridcell at a time. JM Feb 4 2016.
          flinacc_gat(i)=flinacc_gat(i)+fdlgat(i)
          flutacc_gat(i)=flutacc_gat(i)+sbc*gtgat(i)**4
          pregacc_gat(i)=pregacc_gat(i)+pregat(i)*delt
c
          fsnowacc_t(i)=fsnowacc_t(i)+fsnogat(i)
          tcanoaccgat_t(i)=tcanoaccgat_t(i)+tcano(i)
          tcansacc_t(i)=tcansacc_t(i)+tcans(i)
          taaccgat_t(i)=taaccgat_t(i)+tagat(i)
          vvaccgat_t(i)=vvaccgat_t(i)+ vlgat(i)
          uvaccgat_t(i)=uvaccgat_t(i)+ulgat(i)
          if (FSSROW(I) .gt. 0.) then
            altotacc_gat(i) = altotacc_gat(i) + (FSSROW(I)-
     1                (FSGVGAT(I)+FSGSGAT(I)+FSGGGAT(I)))
     2                /FSSROW(I)
             altotcount_ctm = altotcount_ctm + 1
          end if
c
          do 710 j=1,ignd
             tbaraccgat_t(i,j)=tbaraccgat_t(i,j)+tbargat(i,j)
             tbarcacc_t(i,j)=tbarcacc_t(i,j)+tbarc(i,j)
             tbarcsacc_t(i,j)=tbarcsacc_t(i,j)+tbarcs(i,j)
             tbargacc_t(i,j)=tbargacc_t(i,j)+tbarg(i,j)
             tbargsacc_t(i,j)=tbargsacc_t(i,j)+tbargs(i,j)
             thliqcacc_t(i,j)=thliqcacc_t(i,j)+thliqc(i,j)
             thliqgacc_t(i,j)=thliqgacc_t(i,j)+thliqg(i,j)
             thliqacc_t(i,j) = thliqacc_t(i,j) + THLQGAT(i,j)
             thicecacc_t(i,j)=thicecacc_t(i,j)+thicec(i,j)
             thicegacc_t(i,j)=thicegacc_t(i,j)+thiceg(i,j)
710       continue
c
          do 713 j = 1, icc
            ancsvgac_t(i,j)=ancsvgac_t(i,j)+ancsveggat(i,j)
            ancgvgac_t(i,j)=ancgvgac_t(i,j)+ancgveggat(i,j)
            rmlcsvga_t(i,j)=rmlcsvga_t(i,j)+rmlcsveggat(i,j)
            rmlcgvga_t(i,j)=rmlcgvga_t(i,j)+rmlcgveggat(i,j)
713       continue
c
700     continue
      endif !if (ctem_on)

	
c
      if(ncount.eq.nday) then
c
c         daily averages of accumulated variables for ctem
c
        if (ctem_on) then

          do 855 i=1,nml
c
c           net radiation and precipitation estimates for ctem's bioclim
c
            if(fsinacc_gat(i).gt.0.0) then
              alswacc_gat(i)=alswacc_gat(i)/(fsinacc_gat(i)*0.5)
              allwacc_gat(i)=allwacc_gat(i)/(fsinacc_gat(i)*0.5)
            else
              alswacc_gat(i)=0.0
              allwacc_gat(i)=0.0
            endif
c
            uvaccgat_t(i)=uvaccgat_t(i)/real(nday)
            vvaccgat_t(i)=vvaccgat_t(i)/real(nday)
            fsinacc_gat(i)=fsinacc_gat(i)/real(nday)
            flinacc_gat(i)=flinacc_gat(i)/real(nday)
            flutacc_gat(i)=flutacc_gat(i)/real(nday)
c
            if (altotcount_ctm(i) > 0) then
                altotacc_gat(i)=altotacc_gat(i)/real(altotcount_ctm(i))
            else
                altotacc_gat(i)=0.
            end if
            fsstar_gat=fsinacc_gat(i)*(1.-altotacc_gat(i))
            flstar_gat=flinacc_gat(i)-flutacc_gat(i)
            netrad_gat(i)=fsstar_gat+flstar_gat
            preacc_gat(i)=pregacc_gat(i)
c
            fsnowacc_t(i)=fsnowacc_t(i)/real(nday)
            tcanoaccgat_t(i)=tcanoaccgat_t(i)/real(nday)
            tcansacc_t(i)=tcansacc_t(i)/real(nday)
            taaccgat_t(i)=taaccgat_t(i)/real(nday)
c
            do 831 j=1,ignd
              tbaraccgat_t(i,j)=tbaraccgat_t(i,j)/real(nday)
              tbarcacc_t(i,j) = tbaraccgat_t(i,j)
              tbarcsacc_t(i,j) = tbaraccgat_t(i,j)
              tbargacc_t(i,j) = tbaraccgat_t(i,j)
              tbargsacc_t(i,j) = tbaraccgat_t(i,j)
c
              thliqcacc_t(i,j)=thliqcacc_t(i,j)/real(nday)
              thliqgacc_t(i,j)=thliqgacc_t(i,j)/real(nday)
              thliqacc_t(i,j)=thliqacc_t(i,j)/real(nday)
              thicecacc_t(i,j)=thicecacc_t(i,j)/real(nday)
              thicegacc_t(i,j)=thicegacc_t(i,j)/real(nday)
831         continue
c
            do 832 j = 1, icc
              ancsvgac_t(i,j)=ancsvgac_t(i,j)/real(nday)
              ancgvgac_t(i,j)=ancgvgac_t(i,j)/real(nday)
              rmlcsvga_t(i,j)=rmlcsvga_t(i,j)/real(nday)
              rmlcgvga_t(i,j)=rmlcgvga_t(i,j)/real(nday)
832         continue
c
c           pass on mean monthly lightning for the current month to ctem
c           lightng(i)=mlightng(i,month)
c
c           in a very simple way try to interpolate monthly lightning to
c           daily lightning
              if(iday.ge.mmday(1)-1.and.iday.lt.mmday(2))then ! >=15,<46.- mid jan - mid feb
                month1=1
                month2=2
                xday=iday-mmday(1)-1
             else if(iday.ge.mmday(2).and.iday.lt.mmday(3))then ! >=46,<75(76) mid feb - mid mar
                month1=2
                month2=3
                xday=iday-mmday(2)
              else if(iday.ge.mmday(3).and.iday.lt.mmday(4))then ! >=75(76),<106(107) mid mar - mid apr
                month1=3
                month2=4
                xday=iday-mmday(3)
              else if(iday.ge.mmday(4).and.iday.lt.mmday(5))then ! >=106(107),<136(137) mid apr - mid may
                month1=4
                month2=5
                xday=iday-mmday(4)
              else if(iday.ge.mmday(5).and.iday.lt.mmday(6))then ! >=136(137),<167(168) mid may - mid june
                month1=5
                month2=6
                xday=iday-mmday(5)
              else if(iday.ge.mmday(6).and.iday.lt.mmday(7))then ! >=167(168),<197(198) mid june - mid july
                month1=6
                month2=7
                xday=iday-mmday(6)
              else if(iday.ge.mmday(7).and.iday.lt.mmday(8))then ! >=197(198), <228(229) mid july - mid aug
                month1=7
                month2=8
                xday=iday-mmday(7)
              else if(iday.ge.mmday(8).and.iday.lt.mmday(9))then ! >=228(229), < 259(260) mid aug - mid sep
                month1=8
                month2=9
                xday=iday-mmday(8)
              else if(iday.ge.mmday(9).and.iday.lt.mmday(10))then ! >= 259(260), < 289(290) mid sep - mid oct
                month1=9
                month2=10
                xday=iday-mmday(9)
              else if(iday.ge.mmday(10).and.iday.lt.mmday(11))then ! >= 289(290), < 320(321) mid oct - mid nov
                month1=10
                month2=11
                xday=iday-mmday(10)
              else if(iday.ge.mmday(11).and.iday.lt.mmday(12))then ! >=320(321), < 350(351) mid nov - mid dec
                month1=11
                month2=12
                xday=iday-mmday(11)
              else if(iday.ge.mmday(12).or.iday.lt.mmday(1)-1)then ! >= 350(351) < 15 mid dec - mid jan
                month1=12
                month2=1
                xday=iday-mmday(12)
                if(xday.lt.0)xday=iday+15
              endif
c
            lightng(i)=mlightnggat(i,month1)+(real(xday)/30.0)*
     &                 (mlightnggat(i,month2)-mlightnggat(i,month1))
c
            if (obswetf) then
                wetfrac_presgat(i)=wetfrac_mongat(i,month1)+
     &                 (real(xday)/30.0)*
     &                 (wetfrac_mongat(i,month2)-
     &                  wetfrac_mongat(i,month1))
            endif !obswetf

c
855   continue
c
c     Call Canadian Terrestrial Ecosystem Model which operates at a
c     daily time step, and uses daily accumulated values of variables
c     simulated by CLASS.
c


        call ctem ( fcancmxgat, fsnowacc_t,    sandgat,    claygat,
     2                      1,        nml,        iday,    radjgat,
     4          tcanoaccgat_t,  tcansacc_t, tbarcacc_t,tbarcsacc_t,
     5             tbargacc_t, tbargsacc_t, taaccgat_t,    dlzwgat,
     6             ancsvgac_t,  ancgvgac_t, rmlcsvga_t, rmlcgvga_t,
     7                zbtwgat, thliqcacc_t,thliqgacc_t,     deltat,
     8             uvaccgat_t,  vvaccgat_t,    lightng,prbfrhucgat,
     9            extnprobgat,   stdalngat,tbaraccgat_t,  popdon,
     a               nol2pfts, pfcancmxgat, nfcancmxgat,  lnduseon,
     b            thicecacc_t,thicegacc_t,sdepgat,spinfast, todfrac,
     &                compete,  netrad_gat,  preacc_gat,  grclarea,
     &              popdingat,  dofire, dowetlands,obswetf, isndgat,
     &          faregat,onetile_perPFT,wetfrac_presgat,slopefracgat,
     &             currlat,         THPGAT,       BIGAT,    PSISGAT,
     &             ch4concgat,      GRAV, RHOW, RHOICE, leapnow,
c    -------------- inputs used by ctem are above this line ---------
     c            stemmassgat, rootmassgat, litrmassgat, gleafmasgat,
     d            bleafmasgat, soilcmasgat,    ailcggat,    ailcgat,
     e               zolncgat,  rmatctemgat,   rmatcgat,  ailcbgat,
     f            flhrlossgat,  pandaysgat, lfstatusgat, grwtheffgat,
     g            lystmmasgat, lyrotmasgat, tymaxlaigat, vgbiomasgat,
     h            gavgltmsgat, gavgscmsgat, stmhrlosgat,     slaigat,
     i             bmasveggat, cmasvegcgat,  colddaysgat, rothrlosgat,
     j                fcangat,  alvsctmgat,   alirctmgat,  gavglaigat,
     &                  tcurm,    srpcuryr,     dftcuryr,  inibioclim,
     &                 tmonth,    anpcpcur,      anpecur,     gdd5cur,
     &               surmncur,    defmncur,     srplscur,    defctcur,
     &            geremortgat, intrmortgat,    lambdagat,
     &            pftexistgat,   twarmmgat,    tcoldmgat,     gdd5gat,
     1             ariditygat, srplsmongat,  defctmongat, anndefctgat,
     2            annsrplsgat,   annpcpgat,  dry_season_lengthgat,
     &              burnvegfgat, pstemmassgat, pgleafmassgat,
c    -------------- inputs updated by ctem are above this line ------
     k                 nppgat,      nepgat, hetroresgat, autoresgat,
     l            soilcrespgat,       rmgat,       rggat,      nbpgat,
     m              litresgat,    socresgat,     gppgat, dstcemlsgat,
     n            litrfallgat,  humiftrsgat, veghghtgat, rootdpthgat,
     1            litrfallveggat,  humiftrsveggat,
     o                 rmlgat,      rmsgat,     rmrgat,  tltrleafgat,
     p            tltrstemgat, tltrrootgat, leaflitrgat, roottempgat,
     q             afrleafgat,  afrstemgat,  afrrootgat, wtstatusgat,
     r            ltstatusgat, burnfracgat, smfuncveggat, lucemcomgat,
     s            lucltringat, lucsocingat,   nppveggat,
     t            dstcemls3gat,    paicgat,    slaicgat,
     u            emit_co2gat,  emit_cogat,  emit_ch4gat, emit_nmhcgat,
     v             emit_h2gat, emit_noxgat,  emit_n2ogat, emit_pm25gat,
     w            emit_tpmgat,  emit_tcgat,   emit_ocgat,   emit_bcgat,
     &               btermgat,    ltermgat,     mtermgat,
     &            ccgat,             mmgat,
     &          rmlvegaccgat,    rmsveggat,  rmrveggat,  rgveggat,
     &       vgbiomas_veggat, gppveggat,  nepveggat, nbpveggat,
     &        hetroresveggat, autoresveggat, litresveggat,
     &           soilcresveggat, nml, ilmos, jlmos, ch4wet1gat,
     &          ch4wet2gat, wetfdyngat, ch4dyn1gat, ch4dyn2gat,
     &          ch4soillsgat)
c    ---------------- outputs are listed above this line ------------

c


!     reset mosaic accumulator arrays. These are scattered in ctems2 so we need
!     to reset here, prior to ctems2.
        do i = 1, nml
          vvaccgat_t(i)=0.0  !
          uvaccgat_t(i)=0.0  !
          do j=1,ignd
             tbaraccgat_t(i,j)=0.0 !
          end do
        end do

       endif  ! if(ctem_on)

      endif  ! if(ncount.eq.nday)

C===================== CTEM ============================================ /
C

	

      CALL CLASSS (TBARROT,THLQROT,THICROT,TSFSROT,TPNDROT,
     1             ZPNDROT,TBASROT,ALBSROT,TSNOROT,RHOSROT,
     2             SNOROT, GTROT, TCANROT,RCANROT,SCANROT,
     3             GROROT, CMAIROT,TACROT, QACROT, WSNOROT,
     +             REFROT, BCSNROT,EMISROT,SALBROT,CSALROT,
     4             ILMOS,JLMOS,NML,NLAT,NTLD,NMOS,
     5             ILG,IGND,ICAN,ICAN+1,NBS,
     6             TBARGAT,THLQGAT,THICGAT,TSFSGAT,TPNDGAT,
     7             ZPNDGAT,TBASGAT,ALBSGAT,TSNOGAT,RHOSGAT,
     8             SNOGAT, GTGAT, TCANGAT,RCANGAT,SCANGAT,
     9             GROGAT, CMAIGAT,TACGAT, QACGAT, WSNOGAT,
     +             REFGAT, BCSNGAT,EMISGAT,SALBGAT,CSALGAT)



C
C    * SCATTER OPERATION ON DIAGNOSTIC VARIABLES SPLIT OUT OF
C    * CLASSS FOR CONSISTENCY WITH GCM APPLICATIONS.
C
      DO 380 K=1,NML
          CDHROT (ILMOS(K),JLMOS(K))=CDHGAT (K)
          CDMROT (ILMOS(K),JLMOS(K))=CDMGAT (K)
          HFSROT (ILMOS(K),JLMOS(K))=HFSGAT (K)
          TFXROT (ILMOS(K),JLMOS(K))=TFXGAT (K)
          QEVPROT(ILMOS(K),JLMOS(K))=QEVPGAT(K)
          QFSROT (ILMOS(K),JLMOS(K))=QFSGAT (K)
          QFXROT (ILMOS(K),JLMOS(K))=QFXGAT (K)
          PETROT (ILMOS(K),JLMOS(K))=PETGAT (K)
          GAROT  (ILMOS(K),JLMOS(K))=GAGAT  (K)
          EFROT  (ILMOS(K),JLMOS(K))=EFGAT  (K)
          QGROT  (ILMOS(K),JLMOS(K))=QGGAT  (K)
          ALVSROT(ILMOS(K),JLMOS(K))=ALVSGAT(K)
          ALIRROT(ILMOS(K),JLMOS(K))=ALIRGAT(K)
          SFCTROT(ILMOS(K),JLMOS(K))=SFCTGAT(K)
          SFCUROT(ILMOS(K),JLMOS(K))=SFCUGAT(K)
          SFCVROT(ILMOS(K),JLMOS(K))=SFCVGAT(K)
          SFCQROT(ILMOS(K),JLMOS(K))=SFCQGAT(K)
          FSNOROT(ILMOS(K),JLMOS(K))=FSNOGAT(K)
          FSGVROT(ILMOS(K),JLMOS(K))=FSGVGAT(K)
          FSGSROT(ILMOS(K),JLMOS(K))=FSGSGAT(K)
          FSGGROT(ILMOS(K),JLMOS(K))=FSGGGAT(K)
          FLGVROT(ILMOS(K),JLMOS(K))=FLGVGAT(K)
          FLGSROT(ILMOS(K),JLMOS(K))=FLGSGAT(K)
          FLGGROT(ILMOS(K),JLMOS(K))=FLGGGAT(K)
          HFSCROT(ILMOS(K),JLMOS(K))=HFSCGAT(K)
          HFSSROT(ILMOS(K),JLMOS(K))=HFSSGAT(K)
          HFSGROT(ILMOS(K),JLMOS(K))=HFSGGAT(K)
          HEVCROT(ILMOS(K),JLMOS(K))=HEVCGAT(K)
          HEVSROT(ILMOS(K),JLMOS(K))=HEVSGAT(K)
          HEVGROT(ILMOS(K),JLMOS(K))=HEVGGAT(K)
          HMFCROT(ILMOS(K),JLMOS(K))=HMFCGAT(K)
          HMFNROT(ILMOS(K),JLMOS(K))=HMFNGAT(K)
          HTCCROT(ILMOS(K),JLMOS(K))=HTCCGAT(K)
          HTCSROT(ILMOS(K),JLMOS(K))=HTCSGAT(K)
          PCFCROT(ILMOS(K),JLMOS(K))=PCFCGAT(K)
          PCLCROT(ILMOS(K),JLMOS(K))=PCLCGAT(K)
          PCPNROT(ILMOS(K),JLMOS(K))=PCPNGAT(K)
          PCPGROT(ILMOS(K),JLMOS(K))=PCPGGAT(K)
          QFGROT (ILMOS(K),JLMOS(K))=QFGGAT (K)
          QFNROT (ILMOS(K),JLMOS(K))=QFNGAT (K)
          QFCLROT(ILMOS(K),JLMOS(K))=QFCLGAT(K)
          QFCFROT(ILMOS(K),JLMOS(K))=QFCFGAT(K)
          ROFROT (ILMOS(K),JLMOS(K))=ROFGAT (K)
          ROFOROT(ILMOS(K),JLMOS(K))=ROFOGAT(K)
          ROFSROT(ILMOS(K),JLMOS(K))=ROFSGAT(K)
          ROFBROT(ILMOS(K),JLMOS(K))=ROFBGAT(K)
          TROFROT(ILMOS(K),JLMOS(K))=TROFGAT(K)
          TROOROT(ILMOS(K),JLMOS(K))=TROOGAT(K)
          TROSROT(ILMOS(K),JLMOS(K))=TROSGAT(K)
          TROBROT(ILMOS(K),JLMOS(K))=TROBGAT(K)
          ROFCROT(ILMOS(K),JLMOS(K))=ROFCGAT(K)
          ROFNROT(ILMOS(K),JLMOS(K))=ROFNGAT(K)
          ROVGROT(ILMOS(K),JLMOS(K))=ROVGGAT(K)
          WTRCROT(ILMOS(K),JLMOS(K))=WTRCGAT(K)
          WTRSROT(ILMOS(K),JLMOS(K))=WTRSGAT(K)
          WTRGROT(ILMOS(K),JLMOS(K))=WTRGGAT(K)
          DRROT  (ILMOS(K),JLMOS(K))=DRGAT  (K)
          WTABROT(ILMOS(K),JLMOS(K))=WTABGAT(K)
          ILMOROT(ILMOS(K),JLMOS(K))=ILMOGAT(K)
          UEROT  (ILMOS(K),JLMOS(K))=UEGAT(K)
          HBLROT (ILMOS(K),JLMOS(K))=HBLGAT(K)
380   CONTINUE
C
      DO 390 L=1,IGND
      DO 390 K=1,NML
          HMFGROT(ILMOS(K),JLMOS(K),L)=HMFGGAT(K,L)
          HTCROT (ILMOS(K),JLMOS(K),L)=HTCGAT (K,L)
          QFCROT (ILMOS(K),JLMOS(K),L)=QFCGAT (K,L)
          GFLXROT(ILMOS(K),JLMOS(K),L)=GFLXGAT(K,L)
390   CONTINUE
C
      DO 430 M=1,50
          DO 420 L=1,6
              DO 410 K=1,NML
                  ITCTROT(ILMOS(K),JLMOS(K),L,M)=ITCTGAT(K,L,M)
410           CONTINUE
420       CONTINUE
430   CONTINUE
C

C
C===================== CTEM ============================================ \
C
      call ctems2(fcancmxrow,rmatcrow,zolncrow,paicrow,
     1      ailcrow,     ailcgrow,    cmasvegcrow,  slaicrow,
     2      ailcgsrow,   fcancsrow,   fcancrow,     rmatctemrow,
     3      co2concrow,  co2i1cgrow,  co2i1csrow,   co2i2cgrow,
     4      co2i2csrow,  xdiffus,     slairow,      cfluxcgrow,
     5      cfluxcsrow,  ancsvegrow,  ancgvegrow,   rmlcsvegrow,
     6      rmlcgvegrow, canresrow,   SDEPROT,      ch4concrow,
     7      SANDROT,     CLAYROT,     ORGMROT,
     8      anvegrow,    rmlvegrow,   tcanoaccrow_t,tbaraccrow_t,
     9      uvaccrow_t,  vvaccrow_t,  prbfrhucrow,
     a      extnprobrow, pfcancmxrow,  nfcancmxrow,
     b      stemmassrow, rootmassrow, litrmassrow,  gleafmasrow,
     c      bleafmasrow, soilcmasrow, ailcbrow,     flhrlossrow,
     d      pandaysrow,  lfstatusrow, grwtheffrow,  lystmmasrow,
     e      lyrotmasrow, tymaxlairow, vgbiomasrow,  gavgltmsrow,
     f      stmhrlosrow, bmasvegrow,  colddaysrow,  rothrlosrow,
     g      alvsctmrow,  alirctmrow,  gavglairow,   npprow,
     h      neprow,      hetroresrow, autoresrow,   soilcresprow,
     i      rmrow,       rgrow,       nbprow,       litresrow,
     j      socresrow,   gpprow,      dstcemlsrow,  litrfallrow,
     k      humiftrsrow, veghghtrow,  rootdpthrow,  rmlrow,
     1      litresvegrow, humiftrsvegrow,
     l      rmsrow,      rmrrow,      tltrleafrow,  tltrstemrow,
     m      tltrrootrow, leaflitrrow, roottemprow,  afrleafrow,
     n      afrstemrow,  afrrootrow,  wtstatusrow,  ltstatusrow,
     o      burnfracrow, smfuncvegrow, lucemcomrow,  lucltrinrow,
     p      lucsocinrow, nppvegrow,   dstcemls3row,
     q      FAREROT,     gavgscmsrow, tcanoaccrow_out,
     &      rmlvegaccrow, rmsvegrow,  rmrvegrow,    rgvegrow,
     &      vgbiomas_vegrow,gppvegrow,nepvegrow,ailcminrow,ailcmaxrow,
     &      FCANROT,      pftexistrow,
     &      emit_co2row,  emit_corow, emit_ch4row,  emit_nmhcrow,
     &      emit_h2row,   emit_noxrow,emit_n2orow,  emit_pm25row,
     &      emit_tpmrow,  emit_tcrow, emit_ocrow,   emit_bcrow,
     &      btermrow,     ltermrow,   mtermrow,
     &      nbpvegrow,   hetroresvegrow, autoresvegrow,litresvegrow,
     &      soilcresvegrow, burnvegfrow, pstemmassrow, pgleafmassrow,
     &      ch4wet1row, ch4wet2row,
     &      wetfdynrow, ch4dyn1row, ch4dyn2row, ch4soillsrow,
     &      twarmmrow,    tcoldmrow,     gdd5row,
     1      aridityrow, srplsmonrow,  defctmonrow, anndefctrow,
     2      annsrplsrow,   annpcprow,  dry_season_lengthrow,
c    ----
     r      ilmos,       jlmos,       iwmos,        jwmos,
     s      nml,     fcancmxgat,  rmatcgat,    zolncgat,     paicgat,
     v      ailcgat,     ailcggat,    cmasvegcgat,  slaicgat,
     w      ailcgsgat,   fcancsgat,   fcancgat,     rmatctemgat,
     x      co2concgat,  co2i1cggat,  co2i1csgat,   co2i2cggat,
     y      co2i2csgat,  xdiffusgat,  slaigat,      cfluxcggat,
     z      cfluxcsgat,  ancsveggat,  ancgveggat,   rmlcsveggat,
     1      rmlcgveggat, canresgat,   sdepgat,      ch4concgat,
     2      sandgat,     claygat,     orgmgat,
     3      anveggat,    rmlveggat,   tcanoaccgat_t,tbaraccgat_t,
     4      uvaccgat_t,  vvaccgat_t,  prbfrhucgat,
     5      extnprobgat, pfcancmxgat,  nfcancmxgat,
     6      stemmassgat, rootmassgat, litrmassgat,  gleafmasgat,
     7      bleafmasgat, soilcmasgat, ailcbgat,     flhrlossgat,
     8      pandaysgat,  lfstatusgat, grwtheffgat,  lystmmasgat,
     9      lyrotmasgat, tymaxlaigat, vgbiomasgat,  gavgltmsgat,
     a      stmhrlosgat, bmasveggat,  colddaysgat,  rothrlosgat,
     b      alvsctmgat,  alirctmgat,  gavglaigat,   nppgat,
     c      nepgat,      hetroresgat, autoresgat,   soilcrespgat,
     d      rmgat,       rggat,       nbpgat,       litresgat,
     e      socresgat,   gppgat,      dstcemlsgat,  litrfallgat,
     f      humiftrsgat, veghghtgat,  rootdpthgat,  rmlgat,
     1      litresveggat, humiftrsveggat,
     g      rmsgat,      rmrgat,      tltrleafgat,  tltrstemgat,
     h      tltrrootgat, leaflitrgat, roottempgat,  afrleafgat,
     i      afrstemgat,  afrrootgat,  wtstatusgat,  ltstatusgat,
     j      burnfracgat, smfuncveggat, lucemcomgat,  lucltringat,
     k      lucsocingat, nppveggat,   dstcemls3gat,
     l      faregat,     gavgscmsgat, tcanoaccgat_out,
     &      rmlvegaccgat, rmsveggat,  rmrveggat,    rgveggat,
     &      vgbiomas_veggat,gppveggat,nepveggat,ailcmingat,ailcmaxgat,
     &      fcangat,      pftexistgat,
     &      emit_co2gat,  emit_cogat, emit_ch4gat,  emit_nmhcgat,
     &      emit_h2gat,   emit_noxgat,emit_n2ogat,  emit_pm25gat,
     &      emit_tpmgat,  emit_tcgat, emit_ocgat,   emit_bcgat,
     &      btermgat,     ltermgat,   mtermgat,
     &      nbpveggat, hetroresveggat, autoresveggat,litresveggat,
     &      soilcresveggat, burnvegfgat, pstemmassgat, pgleafmassgat,
     &      ch4wet1gat, ch4wet2gat,
     &      wetfdyngat, ch4dyn1gat, ch4dyn2gat,ch4soillsgat,
     &      twarmmgat,    tcoldmgat,     gdd5gat,
     1      ariditygat, srplsmongat,  defctmongat, anndefctgat,
     2      annsrplsgat,   annpcpgat,  dry_season_lengthgat)



      if(ncount.eq.nday) then

c     reset mosaic accumulator arrays.

      if (ctem_on) then
        do 705 i = 1, nml

          fsinacc_gat(i)=0.
          flinacc_gat(i)=0.
          flutacc_gat(i)=0.
          alswacc_gat(i)=0.
          allwacc_gat(i)=0.
          pregacc_gat(i)=0.
          fsnowacc_t(i)=0.0
          tcanoaccgat_out(i)=tcanoaccgat_t(i)
          tcanoaccgat_t(i)=0.0
          tcansacc_t(i)=0.0
          taaccgat_t(i)=0.0
          altotacc_gat(i) = 0.0
          altotcount_ctm(i)=0

          do 715 j=1,ignd
             tbarcacc_t(i,j)=0.0
             tbarcsacc_t(i,j)=0.0
             tbargacc_t(i,j)=0.0
             tbargsacc_t(i,j)=0.0
             thliqcacc_t(i,j)=0.0
             thliqgacc_t(i,j)=0.0
             thliqacc_t(i,j)=0.0
             thicecacc_t(i,j)=0.0
             thicegacc_t(i,j)=0.0
715       continue

          do 716 j = 1, icc
            ancsvgac_t(i,j)=0.0
            ancgvgac_t(i,j)=0.0
            rmlcsvga_t(i,j)=0.0
            rmlcgvga_t(i,j)=0.0
716       continue

705     continue
      endif  ! if(ctem_on)
      end if



C===================== CTEM ============================================ /
C

C=======================================================================
C     * WRITE FIELDS FROM CURRENT TIME STEP TO OUTPUT FILES.

6100  FORMAT(1X,I4,I5,9F8.2,2F8.3,F12.4,F8.2,2(A6,I2))
6200  FORMAT(1X,I4,I5,3(F8.2,2F6.3),F8.2,2F8.4,F8.2,F8.3,2(A6,I2))
6201  FORMAT(1X,I4,I5,5(F7.2,2F6.3),2(A6,I2))
6300  FORMAT(1X,I4,I5,3F9.2,F8.2,F10.2,E12.3,2F12.3,A6,I2)
6400  FORMAT(1X,I2,I3,I5,I6,9F8.2,2F7.3,E11.3,F8.2,F12.4,5F9.5,2(A6,I2))
6500  FORMAT(1X,I2,I3,I5,I6,3(F7.2,2F6.3),F8.2,2F8.4,F8.2,4F8.3,
     &       2F7.3,2(A6,I2))
6600  FORMAT(1X,I2,I3,I5,2F10.2,E12.3,F10.2,F8.2,F10.2,E12.3,2(A6,I2))
6501  FORMAT(1X,I2,I3,I5,I6,5(F7.2,2F6.3),2(A6,I2))
6601  FORMAT(1X,I2,I3,I5,I6,7(F7.2,2F6.3),10F9.4,2(A6,I2))
6700  FORMAT(1X,I2,I3,I5,I6,2X,12E11.4,2(A6,I2))
6800  FORMAT(1X,I2,I3,I5,I6,2X,22(F10.4,2X),2(A6,I2))
6900  FORMAT(1X,I2,I3,I5,I6,2X,18(E12.4,2X),2(A6,I2))
C
C===================== CTEM ============================================ \
c
c  fc,fg,fcs and fgs are one_dimensional in class subroutines
c  the transformations here to grid_cell mean fc_g,fg_g,fcs_g and fgs_g
c  are only applicable when nltest=1 (e.g., one grid cell)
c
      do i=1,nltest
        fc_g(i)=0.0
        fg_g(i)=0.0
        fcs_g(i)=0.0
        fgs_g(i)=0.0
        do m=1,nmtest
          fc_g(i)=fc_g(i)+fc(m)
          fg_g(i)=fg_g(i)+fg(m)
          fcs_g(i)=fcs_g(i)+fcs(m)
          fgs_g(i)=fgs_g(i)+fgs(m)
        enddo
      enddo
c
C===================== CTEM =====================================/
C

      ACTLYR=0.0
      FTABLE=0.0
      DO 440 J=1,IGND
          IF(ABS(TBARGAT(1,J)-TFREZ).LT.0.0001) THEN
              IF(ISNDGAT(1,J).GT.-3) THEN
                  ACTLYR=ACTLYR+(THLQGAT(1,J)/(THLQGAT(1,J)+
     1                THICGAT(1,J)))*DLZWGAT(1,J)
              ELSEIF(ISNDGAT(1,J).EQ.-3) THEN
                  ACTLYR=ACTLYR+DELZ(J)
              ENDIF
          ELSEIF(TBARGAT(1,J).GT.TFREZ) THEN
              ACTLYR=ACTLYR+DELZ(J)
          ENDIF
          IF(ABS(TBARGAT(1,J)-TFREZ).LT.0.0001) THEN
              IF(ISNDGAT(1,J).GT.-3) THEN
                  FTABLE=FTABLE+(THICGAT(1,J)/(THLQGAT(1,J)+
     1                THICGAT(1,J)-THMGAT(1,J)))*DLZWGAT(1,J)
              ELSE
                  FTABLE=FTABLE+DELZ(J)
              ENDIF
          ELSEIF(TBARGAT(1,J).LT.TFREZ) THEN
              FTABLE=FTABLE+DELZ(J)
          ENDIF
440   CONTINUE
C
      IF ((LEAPNOW .AND. IDAY.GE.183 .AND. IDAY.LE.244) .OR. 
     &    (.not. LEAPNOW .AND. IDAY.GE.182 .AND. IDAY.LE.243)) THEN
          ALAVG=ALAVG+ACTLYR
          NAL=NAL+1
          IF(ACTLYR.GT.ALMAX) ALMAX=ACTLYR
      ENDIF
C
      IF ((LEAPNOW .AND. IDAY.GE.1 .AND. IDAY.LE.60) .OR. 
     &    (.not. LEAPNOW .AND. IDAY.GE.1 .AND. IDAY.LE.59)) THEN
          FTAVG=FTAVG+FTABLE
          NFT=NFT+1
          IF(FTABLE.GT.FTMAX) FTMAX=FTABLE
      ENDIF

      if (.not. parallelrun) then ! stand alone mode, include half-hourly
c                                 ! output for CLASS & CTEM
C


!Because this is MESH-CTEM and getting all grids would be too consuming, we only write for the grid we want
      DO 450 I=1,NLTEST  !1 to the number of grid output files


c       initialization of various grid-averaged variables
        call resetgridavg(nltest)

       DO 425 M=1,NMTEST

          IF(FSSROW(I).GT.0.0) THEN
              ALTOT=(FSSROW(I)-(FSGVROT(I,M)+FSGSROT(I,M)
     1              +FSGGROT(I,M)))/FSSROW(I)
          ELSE
              ALTOT=0.0
          ENDIF
          FSSTAR=FSSROW(I)*(1.0-ALTOT)
          FLSTAR=FDLROW(I)-SBC*GTROT(I,M)**4
          QH=HFSROT(I,M)
          QE=QEVPROT(I,M)
C          BEG=FSSTAR+FLSTAR-QH-QE !(commented out in runclass.fieldsite)
          BEG=GFLXGAT(1,1)  !FLAG!
C          USTARBS=UVROW(1)*SQRT(CDMROT(I,M)) !FLAG (commented out in runclass.fieldsite)
          SNOMLT=HMFNROT(I,M)
          IF(RHOSROT(I,M).GT.0.0) THEN
              ZSN=SNOROT(I,M)/RHOSROT(I,M)
          ELSE
              ZSN=0.0
          ENDIF
          IF(TCANROT(I,M).GT.0.01) THEN
              TCN=TCANROT(I,M)-TFREZ
          ELSE
              TCN=0.0
          ENDIF
          TSURF=FCS(I)*TSFSGAT(I,1)+FGS(I)*TSFSGAT(I,2)+
     1           FC(I)*TSFSGAT(I,3)+FG(I)*TSFSGAT(I,4)
C          IF(FSSROW(I).GT.0.0 .AND. (FCS(I)+FC(I)).GT.0.0) THEN
C          IF(FSSROW(I).GT.0.0) THEN
              NFS=NFS+1
              ITA=NINT(TAROW(I)-TFREZ)
              ITCAN=NINT(TCN)
              ITAC=NINT(TACGAT(I)-TFREZ)
              ITSCR=NINT(SFCTGAT(I)-TFREZ)
              ITS=NINT(TSURF-TFREZ)
C              ITD=ITS-ITA
              ITD=ITCAN-ITA
              ITD2=ITCAN-ITSCR
              ITD3=ITCAN-ITAC
              ITD4=ITAC-ITA
C              IF(ITA.GT.0.0) THEN
                  TAHIST(ITA+100)=TAHIST(ITA+100)+1.0
                  TCHIST(ITCAN+100)=TCHIST(ITCAN+100)+1.0
                  TSHIST(ITS+100)=TSHIST(ITS+100)+1.0
                  TACHIST(ITAC+100)=TACHIST(ITAC+100)+1.0
                  TDHIST(ITD+100)=TDHIST(ITD+100)+1.0
                  TD2HIST(ITD2+100)=TD2HIST(ITD2+100)+1.0
                  TD3HIST(ITD3+100)=TD3HIST(ITD3+100)+1.0
                  TD4HIST(ITD4+100)=TD4HIST(ITD4+100)+1.0
                  TSCRHIST(ITSCR+100)=TSCRHIST(ITSCR+100)+1.0
C              ENDIF
C          ENDIF     
          IF(FC(I).GT.0.1 .AND. RC(I).GT.1.0E5) NDRY=NDRY+1

C
          IF(TSNOROT(I,M).GT.0.01) THEN
              TSN=TSNOROT(I,M)-TFREZ
          ELSE
              TSN=0.0
          ENDIF
          IF(TPNDROT(I,M).GT.0.01) THEN
              TPN=TPNDROT(I,M)-TFREZ
          ELSE
              TPN=0.0
          ENDIF
          GTOUT=GTROT(I,M)-TFREZ
          EVAPSUM=QFCFROT(I,M)+QFCLROT(I,M)+QFNROT(I,M)+QFGROT(I,M)+
     1                   QFCROT(I,M,1)+QFCROT(I,M,2)+QFCROT(I,M,3)



C
!===================== CTEM =====================================\
!         start writing output
!         For MESH-CTEM Write Output within a loop as we may have multiple grids in question

  
          DO K=1,WF_NUM_POINTS !Create a DO loop for each folder     
           if(I .eq. N_OUT(K) .and. M .eq. II_out(K) ) THEN
            !IF there is no combination, then the entire writing process will be skipped
c
          if ((iyear .ge. jhhsty) .and. (iyear .le. jhhendy)) then
           if ((iday .ge. jhhstd) .and. (iday .le. jhhendd)) then


C===================== CTEM =====================================/
          WRITE(64*k,6400) IHOUR,IMIN,IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,
     1                   SNOMLT,BEG,GTOUT,SNOROT(I,M),RHOSROT(I,M),
     2                   WSNOROT(I,M),ALTOT,ROFROT(I,M),
     3                   TPN,ZPNDROT(I,M),CDHROT(I,M),CDMROT(I,M),
     4                   SFCUROT(I,M),SFCVROT(I,M),UVROW(I),' TILE ',m


          IF(IGND.GT.3) THEN
C===================== CTEM =====================================\


              write(65*k,6500) ihour,imin,iday,iyear,(TBARROT(i,m,j)-
     1                   tfrez,THLQROT(i,m,j),THICROT(i,m,j),j=1,3),
     2                  tcn,RCANROT(i,m),SCANROT(i,m),tsn,zsn,
     3                   TCN-(TAROW(I)-TFREZ),TCANO(I)-TFREZ,
     4                   TACGAT(I)-TFREZ,ACTLYR,FTABLE,' TILE ',m
              write(66*k,6601) ihour,imin,iday,iyear,(TBARROT(i,m,j)-
     1                   tfrez,THLQROT(i,m,j),THICROT(i,m,j),j=4,10),
     2                   (GFLXROT(i,m,j),j=1,10),
     3                   ' TILE ',m
         else

              write(65*k,6500) ihour,imin,iday,iyear,(TBARROT(i,m,j)-
     1                   tfrez,THLQROT(i,m,j),THICROT(i,m,j),j=1,3),
     2                  tcn,RCANROT(i,m),SCANROT(i,m),tsn,zsn,
     3                   TCN-(TAROW(I)-TFREZ),TCANO(I)-TFREZ,
     4                   TACGAT(I)-TFREZ,ACTLYR,FTABLE,' TILE ',m

C===================== CTEM =====================================/
          ENDIF
C
          WRITE(67*k,6700) IHOUR,IMIN,IDAY,IYEAR,
     1                   TROFROT(I,M),TROOROT(I,M),TROSROT(I,M),
     2                   TROBROT(I,M),ROFROT(I,M),ROFOROT(I,M),
     3                   ROFSROT(I,M),ROFBROT(I,M),
     4                   FCS(M),FGS(M),FC(M),FG(M),' TILE ',M
          WRITE(68*k,6800) IHOUR,IMIN,IDAY,IYEAR,
     1                   FSGVROT(I,M),FSGSROT(I,M),FSGGROT(I,M),
     2                   FLGVROT(I,M),FLGSROT(I,M),FLGGROT(I,M),
     3                   HFSCROT(I,M),HFSSROT(I,M),HFSGROT(I,M),
     4                   HEVCROT(I,M),HEVSROT(I,M),HEVGROT(I,M),
     5                   HMFCROT(I,M),HMFNROT(I,M),
     6                   (HMFGROT(I,M,J),J=1,3),
     7                   HTCCROT(I,M),HTCSROT(I,M),
     8                   (HTCROT(I,M,J),J=1,3),' TILE ',M
          WRITE(69*k,6900) IHOUR,IMIN,IDAY,IYEAR,
     1                   PCFCROT(I,M),PCLCROT(I,M),PCPNROT(I,M),
     2                   PCPGROT(I,M),QFCFROT(I,M),QFCLROT(I,M),
     3                   QFNROT(I,M),QFGROT(I,M),(QFCROT(I,M,J),J=1,3),
     4                   ROFCROT(I,M),ROFNROT(I,M),ROFOROT(I,M),
     5                   ROFROT(I,M),WTRCROT(I,M),WTRSROT(I,M),
     6                   WTRGROT(I,M),' TILE ',M
C===================== CTEM =====================================\
	  

C
         endif
        endif ! half hourly output loop.
        endif ! Checking if this is a writable output
        ENDDO ! End of K loop for each output folder
c
c         Write half-hourly CTEM results to file *.CT01H
c
c         Net photosynthetic rates and leaf maintenance respiration for
c         each pft. however, if ctem_on then physyn subroutine
c         is using storage lai while actual lai is zero. if actual lai is
c         zero then we make anveg and rmlveg zero as well because these
c         are imaginary just like storage lai. note that anveg and rmlveg
c         are not passed to ctem. rather ancsveg, ancgveg, rmlcsveg, and
c         rmlcgveg are passed.
c


          if (ctem_on) then

            do 760 j = 1,icc
             if(ailcgrow(i,m,j).le.0.0) then
                anvegrow(i,m,j)=0.0
                rmlvegrow(i,m,j)=0.0
              else
                anvegrow(i,m,j)=ancsvegrow(i,m,j)*FSNOROT(i,m) +
     &                          ancgvegrow(i,m,j)*(1. - FSNOROT(i,m))
                rmlvegrow(i,m,j)=rmlcsvegrow(i,m,j)*FSNOROT(i,m) +
     &                         rmlcgvegrow(i,m,j)*(1. - FSNOROT(i,m))
              endif
760         continue
c


          if ((iyear .ge. jhhsty) .and. (iyear .le. jhhendy)) then
           if ((iday .ge. jhhstd) .and. (iday .le. jhhendd)) then

              write(71,7200)ihour,imin,iday,iyear,(anvegrow(i,m,j),
     1                    j=1,icc),(rmlvegrow(i,m,j),j=1,icc),
     2                    ' TILE ',m
            endif
           end if

           do j = 1,icc
              anvegrow_g(i,j)=anvegrow_g(i,j)+anvegrow(i,m,j)
     1                                        *FAREROT(i,m)
              rmlvegrow_g(i,j)=rmlvegrow_g(i,j)+rmlvegrow(i,m,j)
     1                                         *FAREROT(i,m)
            enddo

          endif   ! ctem_on

7200      format(1x,i2,1x,i2,i5,i5,9f11.3,9f11.3,2(a6,i2))

 
          fsstar_g(i)    =fsstar_g(i) + fsstar*FAREROT(i,m)
          flstar_g(i)    =flstar_g(i) + flstar*FAREROT(i,m)
          qh_g(i)        =qh_g(i)     + qh*FAREROT(i,m)
          qe_g(i)        =qe_g(i)     + qe*FAREROT(i,m)
          snomlt_g(i)    =snomlt_g(i) + snomlt*FAREROT(i,m)
          beg_g(i)       =beg_g(i)    + beg*FAREROT(i,m)
          gtout_g(i)     =gtout_g(i)  + gtout*FAREROT(i,m)
          tcn_g(i)       =tcn_g(i)    + tcn*FAREROT(i,m)
          tsn_g(i)       =tsn_g(i)    + tsn*FAREROT(i,m)
          zsn_g(i)       =zsn_g(i)    + zsn*FAREROT(i,m)
          altot_g(i)     =altot_g(i)  + altot*FAREROT(i,m)
          tpn_g(i)       =tpn_g(i)    + tpn*FAREROT(i,m)



          do j=1,ignd
            TBARROT_g(i,j)=TBARROT_g(i,j) + TBARROT(i,m,j)*FAREROT(i,m)
            THLQROT_g(i,j)=THLQROT_g(i,j) + THLQROT(i,m,j)*FAREROT(i,m)
            THICROT_g(i,j)=THICROT_g(i,j) + THICROT(i,m,j)*FAREROT(i,m)
            GFLXROT_g(i,j)=GFLXROT_g(i,j) + GFLXROT(i,m,j)*FAREROT(i,m)
            HMFGROT_g(i,j)=HMFGROT_g(i,j) + HMFGROT(i,m,j)*FAREROT(i,m)
            HTCROT_g(i,j)=HTCROT_g(i,j) + HTCROT(i,m,j)*FAREROT(i,m)
            QFCROT_g(i,j)=QFCROT_g(i,j) + QFCROT(i,m,j)*FAREROT(i,m)
          enddo
c


          ZPNDROT_g(i)=ZPNDROT_g(i) + ZPNDROT(i,m)*FAREROT(i,m)
          RHOSROT_g(i)=RHOSROT_g(i) + RHOSROT(i,m)*FAREROT(i,m)
          WSNOROT_g(i)=WSNOROT_g(i) + WSNOROT(i,m)*FAREROT(i,m)
          RCANROT_g(i)=RCANROT_g(i) + RCANROT(i,m)*FAREROT(i,m)
          SCANROT_g(i)=SCANROT_g(i) + SCANROT(i,m)*FAREROT(i,m)
          TROFROT_g(i)=TROFROT_g(i) + TROFROT(i,m)*FAREROT(i,m)
          TROOROT_g(i)=TROOROT_g(i) + TROOROT(i,m)*FAREROT(i,m)
          TROSROT_g(i)=TROSROT_g(i) + TROSROT(i,m)*FAREROT(i,m)
          TROBROT_g(i)=TROBROT_g(i) + TROBROT(i,m)*FAREROT(i,m)
          ROFOROT_g(i)=ROFOROT_g(i) + ROFOROT(i,m)*FAREROT(i,m)
          ROFSROT_g(i)=ROFSROT_g(i) + ROFSROT(i,m)*FAREROT(i,m)
          ROFBROT_g(i)=ROFBROT_g(i) + ROFBROT(i,m)*FAREROT(i,m)
          FSGVROT_g(i)=FSGVROT_g(i) + FSGVROT(i,m)*FAREROT(i,m)
          FSGSROT_g(i)=FSGSROT_g(i) + FSGSROT(i,m)*FAREROT(i,m)
          FSGGROT_g(i)=FSGGROT_g(i) + FSGGROT(i,m)*FAREROT(i,m)
          FLGVROT_g(i)=FLGVROT_g(i) + FLGVROT(i,m)*FAREROT(i,m)
          FLGSROT_g(i)=FLGSROT_g(i) + FLGSROT(i,m)*FAREROT(i,m)
          FLGGROT_g(i)=FLGGROT_g(i) + FLGGROT(i,m)*FAREROT(i,m)
          HFSCROT_g(i)=HFSCROT_g(i) + HFSCROT(i,m)*FAREROT(i,m)
          HFSSROT_g(i)=HFSSROT_g(i) + HFSSROT(i,m)*FAREROT(i,m)
          HFSGROT_g(i)=HFSGROT_g(i) + HFSGROT(i,m)*FAREROT(i,m)
          HEVCROT_g(i)=HEVCROT_g(i) + HEVCROT(i,m)*FAREROT(i,m)
          HEVSROT_g(i)=HEVSROT_g(i) + HEVSROT(i,m)*FAREROT(i,m)
          HEVGROT_g(i)=HEVGROT_g(i) + HEVGROT(i,m)*FAREROT(i,m)
          HMFCROT_g(i)=HMFCROT_g(i) + HMFCROT(i,m)*FAREROT(i,m)
          HMFNROT_g(i)=HMFNROT_g(i) + HMFNROT(i,m)*FAREROT(i,m)
          HTCCROT_g(i)=HTCCROT_g(i) + HTCCROT(i,m)*FAREROT(i,m)
          HTCSROT_g(i)=HTCSROT_g(i) + HTCSROT(i,m)*FAREROT(i,m)
          PCFCROT_g(i)=PCFCROT_g(i) + PCFCROT(i,m)*FAREROT(i,m)
          PCLCROT_g(i)=PCLCROT_g(i) + PCLCROT(i,m)*FAREROT(i,m)
          PCPNROT_g(i)=PCPNROT_g(i) + PCPNROT(i,m)*FAREROT(i,m)
          PCPGROT_g(i)=PCPGROT_g(i) + PCPGROT(i,m)*FAREROT(i,m)
          QFCFROT_g(i)=QFCFROT_g(i) + QFCFROT(i,m)*FAREROT(i,m)
          QFCLROT_g(i)=QFCLROT_g(i) + QFCLROT(i,m)*FAREROT(i,m)
          ROFCROT_g(i)=ROFCROT_g(i) + ROFCROT(i,m)*FAREROT(i,m)
          ROFNROT_g(i)=ROFNROT_g(i) + ROFNROT(i,m)*FAREROT(i,m)
          WTRCROT_g(i)=WTRCROT_g(i) + WTRCROT(i,m)*FAREROT(i,m)
          WTRSROT_g(i)=WTRSROT_g(i) + WTRSROT(i,m)*FAREROT(i,m)
          WTRGROT_g(i)=WTRGROT_g(i) + WTRGROT(i,m)*FAREROT(i,m)
          QFNROT_g(i) =QFNROT_g(i) + QFNROT(i,m)*FAREROT(i,m)
          QFGROT_g(i) =QFGROT_g(i) + QFGROT(i,m)*FAREROT(i,m)
          ROFROT_g(i) =ROFROT_g(i) + ROFROT(i,m)*FAREROT(i,m)
          SNOROT_g(i) =SNOROT_g(i) + SNOROT(i,m)*FAREROT(i,m)
          CDHROT_g(i) =CDHROT_g(i) + CDHROT(i,m)*FAREROT(i,m)
          CDMROT_g(i) =CDMROT_g(i) + CDMROT(i,m)*FAREROT(i,m)
          SFCUROT_g(i) =SFCUROT_g(i) + SFCUROT(i,m)*FAREROT(i,m)
          SFCVROT_g(i) =SFCVROT_g(i) + SFCVROT(i,m)*FAREROT(i,m)


425    CONTINUE
C===================== CTEM =====================================\
C      WRITE CTEM OUTPUT FILES
       

!         start writing output
!         For MESH-CTEM Write Output within a loop as we may have multiple grids in question

  
          DO K=1,WF_NUM_POINTS !Create a DO loop for each folder     
           if(I .eq. N_OUT(K)) THEN
            !IF there is no combination, then the entire writing process will be skipped


C
      if ((iyear .ge. jhhsty) .and. (iyear .le. jhhendy)) then
       if ((iday .ge. jhhstd) .and. (iday .le. jhhendd)) then

C=======================Verification of iDay for Leap Years===============

           WRITE(711*k,7200)IHOUR,IMIN,IDAY,IYEAR,(ANVEGROW_G(I,J),
     1                 J=1,ICC),(RMLVEGROW_G(I,J),J=1,ICC)


!       WRITE(641*k,6400) IHOUR,IMIN,IDAY,IYEAR,FSSTAR_G(i),FLSTAR_G(i),
!     1                  QH_G(i),QE_G(i),SNOMLT_G(i),BEG_G(i),GTOUT_G(i),
!     2                  SNOROT_G(I),RHOSROT_G(I),WSNOROT_G(I),
!     3                  ALTOT_G(i),ROFROT_G(I),TPN_G(i),ZPNDROT_G(I),
!     4                  CDHROT_G(I),CDMROT_G(I),SFCUROT_G(I),
!     5                  SFCVROT_G(I),UVROW(I)
!         WRITE(651*k,6500) IHOUR,IMIN,IDAY,IYEAR,(TBARROT_G(I,J)-
!     1                   TFREZ,THLQROT_G(I,J),THICROT_G(I,J),J=1,3),
!     2                   TCN_G(i),RCANROT_G(I),SCANROT_G(I),TSN_G(i),
!     3                   ZSN_G(i),TCN_G(i)-(TAROW(I)-TFREZ),
!     4                   TCANO(I)-TFREZ,TACGAT(I)-TFREZ,ACTLYR,FTABLE
C
!         IF(IGND.GT.3) THEN
!          WRITE(661*k,6601) IHOUR,IMIN,IDAY,IYEAR,(TBARROT_G(I,J)-
!     1                   TFREZ,THLQROT_G(I,J),THICROT_G(I,J),J=4,10),
!     2                   (GFLXROT_G(I,J),J=1,10)
!         ELSE
!          WRITE(661*k,6600) IHOUR,IMIN,IDAY,FSSROW(I),FDLROW(I),PREROW(I),
!     1                   TAROW(I)-TFREZ,UVROW(I),PRESROW(I),QAROW(I)
!         ENDIF
C
!         WRITE(671*k,6700) IHOUR,IMIN,IDAY,IYEAR,
!     &                   TROFROT_G(I),TROOROT_G(I),TROSROT_G(I),
!     1                   TROBROT_G(I),ROFROT_G(I),ROFOROT_G(I),
!     2                   ROFSROT_G(I),ROFBROT_G(I),
!     3                   FCS_G(I),FGS_G(I),FC_G(I),FG_G(I)
!         WRITE(681*k,6800) IHOUR,IMIN,IDAY,IYEAR,
!     &                   FSGVROT_G(I),FSGSROT_G(I),FSGGROT_G(I),
!     1                   FLGVROT_G(I),FLGSROT_G(I),FLGGROT_G(I),
!     2                   HFSCROT_G(I),HFSSROT_G(I),HFSGROT_G(I),
!     3                   HEVCROT_G(I),HEVSROT_G(I),HEVGROT_G(I),
!     4                   HMFCROT_G(I),HMFNROT_G(I),
!     5                   (HMFGROT_G(I,J),J=1,3),
!     6                   HTCCROT_G(I),HTCSROT_G(I),
!     7                   (HTCROT_G(I,J),J=1,3)
!         WRITE(691*k,6900) IHOUR,IMIN,IDAY,IYEAR,
!     &                   PCFCROT_G(I),PCLCROT_G(I),PCPNROT_G(I),
!     1                   PCPGROT_G(I),QFCFROT_G(I),QFCLROT_G(I),
!     2                   QFNROT_G(I),QFGROT_G(I),(QFCROT_G(I,J),J=1,3),
!     3                   ROFCROT_G(I),ROFNROT_G(I),ROFOROT_G(I),
!     4                   ROFROT_G(I),WTRCROT_G(I),WTRSROT_G(I),
!     5                   WTRGROT_G(I)
C
        endif
       ENDIF ! if write half-hourly
       ENDIF !If there is a combination
       ENDDO !WF_NUM_POINTS
C===================== CTEM =====================================/


450   CONTINUE


      endif ! not parallelrun


      if(.not.parallelrun) then ! stand alone mode, includes
c                               ! diagnostic fields
C===================== CTEM =====================================/
C
      DO 525 I=1,NLTEST
          CDHROW(I)=0.
          CDMROW(I)=0.
          HFSROW(I)=0.
          TFXROW(I)=0.
          QEVPROW(I)=0.
          QFSROW(I)=0.
          QFXROW(I)=0.
          PETROW(I)=0.
          GAROW(I)=0.
          EFROW(I)=0.
          GTROW(I)=0.
          QGROW(I)=0.
          ALVSROW(I)=0.
          ALIRROW(I)=0.
          SFCTROW(I)=0.
          SFCUROW(I)=0.
          SFCVROW(I)=0.
          SFCQROW(I)=0.
          SFRHROW(I)=0.
          FSNOROW(I)=0.
          FSGVROW(I)=0.
          FSGSROW(I)=0.
          FSGGROW(I)=0.
          FLGVROW(I)=0.
          FLGSROW(I)=0.
          FLGGROW(I)=0.
          HFSCROW(I)=0.
          HFSSROW(I)=0.
          HFSGROW(I)=0.
          HEVCROW(I)=0.
          HEVSROW(I)=0.
          HEVGROW(I)=0.
          HMFCROW(I)=0.
          HMFNROW(I)=0.
          HTCCROW(I)=0.
          HTCSROW(I)=0.
          PCFCROW(I)=0.
          PCLCROW(I)=0.
          PCPNROW(I)=0.
          PCPGROW(I)=0.
          QFGROW(I)=0.
          QFNROW(I)=0.
          QFCLROW(I)=0.
          QFCFROW(I)=0.
          ROFROW(I)=0.
          ROFOROW(I)=0.
          ROFSROW(I)=0.
          ROFBROW(I)=0.
          ROFCROW(I)=0.
          ROFNROW(I)=0.
          ROVGROW(I)=0.
          WTRCROW(I)=0.
          WTRSROW(I)=0.
          WTRGROW(I)=0.
          DRROW(I)=0.
          WTABROW(I)=0.
          ILMOROW(I)=0.
          UEROW(I)=0.
          HBLROW(I)=0.
          DO 500 J=1,IGND
              HMFGROW(I,J)=0.
              HTCROW(I,J)=0.
              QFCROW(I,J)=0.
              GFLXROW(I,J)=0.
500       CONTINUE
525   CONTINUE
C
      DO 600 I=1,NLTEST
      DO 575 M=1,NMTEST
          CDHROW(I)=CDHROW(I)+CDHROT(I,M)*FAREROT(I,M)
          CDMROW(I)=CDMROW(I)+CDMROT(I,M)*FAREROT(I,M)
          HFSROW(I)=HFSROW(I)+HFSROT(I,M)*FAREROT(I,M)
          TFXROW(I)=TFXROW(I)+TFXROT(I,M)*FAREROT(I,M)
          QEVPROW(I)=QEVPROW(I)+QEVPROT(I,M)*FAREROT(I,M)
          QFSROW(I)=QFSROW(I)+QFSROT(I,M)*FAREROT(I,M)
          QFXROW(I)=QFXROW(I)+QFXROT(I,M)*FAREROT(I,M)
          PETROW(I)=PETROW(I)+PETROT(I,M)*FAREROT(I,M)
          GAROW(I)=GAROW(I)+GAROT(I,M)*FAREROT(I,M)
          EFROW(I)=EFROW(I)+EFROT(I,M)*FAREROT(I,M)
          GTROW(I)=GTROW(I)+GTROT(I,M)*FAREROT(I,M)
          QGROW(I)=QGROW(I)+QGROT(I,M)*FAREROT(I,M)
          ALVSROW(I)=ALVSROW(I)+ALVSROT(I,M)*FAREROT(I,M)
          ALIRROW(I)=ALIRROW(I)+ALIRROT(I,M)*FAREROT(I,M)
          SFCTROW(I)=SFCTROW(I)+SFCTROT(I,M)*FAREROT(I,M)
          SFCUROW(I)=SFCUROW(I)+SFCUROT(I,M)*FAREROT(I,M)
          SFCVROW(I)=SFCVROW(I)+SFCVROT(I,M)*FAREROT(I,M)
          SFCQROW(I)=SFCQROW(I)+SFCQROT(I,M)*FAREROT(I,M)
          SFRHROW(I)=SFRHROW(I)+SFRHROT(I,M)*FAREROT(I,M)
          FSNOROW(I)=FSNOROW(I)+FSNOROT(I,M)*FAREROT(I,M)
          FSGVROW(I)=FSGVROW(I)+FSGVROT(I,M)*FAREROT(I,M)
          FSGSROW(I)=FSGSROW(I)+FSGSROT(I,M)*FAREROT(I,M)
          FSGGROW(I)=FSGGROW(I)+FSGGROT(I,M)*FAREROT(I,M)
          FLGVROW(I)=FLGVROW(I)+FLGVROT(I,M)*FAREROT(I,M)
          FLGSROW(I)=FLGSROW(I)+FLGSROT(I,M)*FAREROT(I,M)
          FLGGROW(I)=FLGGROW(I)+FLGGROT(I,M)*FAREROT(I,M)
          HFSCROW(I)=HFSCROW(I)+HFSCROT(I,M)*FAREROT(I,M)
          HFSSROW(I)=HFSSROW(I)+HFSSROT(I,M)*FAREROT(I,M)
          HFSGROW(I)=HFSGROW(I)+HFSGROT(I,M)*FAREROT(I,M)
          HEVCROW(I)=HEVCROW(I)+HEVCROT(I,M)*FAREROT(I,M)
          HEVSROW(I)=HEVSROW(I)+HEVSROT(I,M)*FAREROT(I,M)
          HEVGROW(I)=HEVGROW(I)+HEVGROT(I,M)*FAREROT(I,M)
          HMFCROW(I)=HMFCROW(I)+HMFCROT(I,M)*FAREROT(I,M)
          HMFNROW(I)=HMFNROW(I)+HMFNROT(I,M)*FAREROT(I,M)
          HTCCROW(I)=HTCCROW(I)+HTCCROT(I,M)*FAREROT(I,M)
          HTCSROW(I)=HTCSROW(I)+HTCSROT(I,M)*FAREROT(I,M)
          PCFCROW(I)=PCFCROW(I)+PCFCROT(I,M)*FAREROT(I,M)
          PCLCROW(I)=PCLCROW(I)+PCLCROT(I,M)*FAREROT(I,M)
          PCPNROW(I)=PCPNROW(I)+PCPNROT(I,M)*FAREROT(I,M)
          PCPGROW(I)=PCPGROW(I)+PCPGROT(I,M)*FAREROT(I,M)
          QFGROW(I)=QFGROW(I)+QFGROT(I,M)*FAREROT(I,M)
          QFNROW(I)=QFNROW(I)+QFNROT(I,M)*FAREROT(I,M)
          QFCLROW(I)=QFCLROW(I)+QFCLROT(I,M)*FAREROT(I,M)
          QFCFROW(I)=QFCFROW(I)+QFCFROT(I,M)*FAREROT(I,M)
          ROFROW(I)=ROFROW(I)+ROFROT(I,M)*FAREROT(I,M)
          ROFOROW(I)=ROFOROW(I)+ROFOROT(I,M)*FAREROT(I,M)
          ROFSROW(I)=ROFSROW(I)+ROFSROT(I,M)*FAREROT(I,M)
          ROFBROW(I)=ROFBROW(I)+ROFBROT(I,M)*FAREROT(I,M)
          ROFCROW(I)=ROFCROW(I)+ROFCROT(I,M)*FAREROT(I,M)
          ROFNROW(I)=ROFNROW(I)+ROFNROT(I,M)*FAREROT(I,M)
          ROVGROW(I)=ROVGROW(I)+ROVGROT(I,M)*FAREROT(I,M)
          WTRCROW(I)=WTRCROW(I)+WTRCROT(I,M)*FAREROT(I,M)
          WTRSROW(I)=WTRSROW(I)+WTRSROT(I,M)*FAREROT(I,M)
          WTRGROW(I)=WTRGROW(I)+WTRGROT(I,M)*FAREROT(I,M)
          DRROW(I)=DRROW(I)+DRROT(I,M)*FAREROT(I,M)
          WTABROW(I)=WTABROW(I)+WTABROT(I,M)*FAREROT(I,M)
          ILMOROW(I)=ILMOROW(I)+ILMOROT(I,M)*FAREROT(I,M)
          UEROW(I)=UEROW(I)+UEROT(I,M)*FAREROT(I,M)
          HBLROW(I)=HBLROW(I)+HBLROT(I,M)*FAREROT(I,M)
          DO 550 J=1,IGND
              HMFGROW(I,J)=HMFGROW(I,J)+HMFGROT(I,M,J)*FAREROT(I,M)
              HTCROW(I,J)=HTCROW(I,J)+HTCROT(I,M,J)*FAREROT(I,M)
              QFCROW(I,J)=QFCROW(I,J)+QFCROT(I,M,J)*FAREROT(I,M)
              GFLXROW(I,J)=GFLXROW(I,J)+GFLXROT(I,M,J)*FAREROT(I,M)
550       CONTINUE
575   CONTINUE
600   CONTINUE
C

C===================== CTEM =====================================\

      endif ! not parallelrun, for diagnostic fields
c
      if(.not.parallelrun) then ! stand alone mode, includes daily output for class
C===================== CTEM =====================================/
C
C     * ACCUMULATE OUTPUT DATA FOR DIURNALLY AVERAGED FIELDS. BOTH GRID
C       MEAN AND MOSAIC MEAN
C
      DO 675 I=1,NLTEST

          IF (FSSROW(I) .gt. 0.) then
            ALTOTACC(I)=ALTOTACC(I) + (FSSROW(I)-(FSGVROW(I)
     1                   +FSGSROW(I)+FSGGROW(I)))/FSSROW(I)
            altotcntr_d(i)=altotcntr_d(i) + 1
          END IF

      DO 650 M=1,NMTEST
          PREACC(I)=PREACC(I)+PREROW(I)*FAREROT(I,M)*DELT
          GTACC(I)=GTACC(I)+GTROT(I,M)*FAREROT(I,M)
          QEVPACC(I)=QEVPACC(I)+QEVPROT(I,M)*FAREROT(I,M)
          EVAPACC(I)=EVAPACC(I)+QFSROT(I,M)*FAREROT(I,M)*DELT
          HFSACC(I)=HFSACC(I)+HFSROT(I,M)*FAREROT(I,M)
          HMFNACC(I)=HMFNACC(I)+HMFNROT(I,M)*FAREROT(I,M)
          ROFACC(I)=ROFACC(I)+ROFROT(I,M)*FAREROT(I,M)*DELT
          OVRACC(I)=OVRACC(I)+ROFOROT(I,M)*FAREROT(I,M)*DELT
          WTBLACC(I)=WTBLACC(I)+WTABROT(I,M)*FAREROT(I,M)
          DO 625 J=1,IGND
              TBARACC(I,J)=TBARACC(I,J)+TBARROT(I,M,J)*FAREROT(I,M)
              THLQACC(I,J)=THLQACC(I,J)+THLQROT(I,M,J)*FAREROT(I,M)
              THICACC(I,J)=THICACC(I,J)+THICROT(I,M,J)*FAREROT(I,M)
              THALACC(I,J)=THALACC(I,J)+(THLQROT(I,M,J)+THICROT(I,M,J))
     1                    *FAREROT(I,M)
625       CONTINUE
          ALVSACC(I)=ALVSACC(I)+ALVSROT(I,M)*FAREROT(I,M)*FSVHROW(I)
          ALIRACC(I)=ALIRACC(I)+ALIRROT(I,M)*FAREROT(I,M)*FSIHROW(I)
          IF(SNOROT(I,M).GT.0.0) THEN
              RHOSACC(I)=RHOSACC(I)+RHOSROT(I,M)*FAREROT(I,M)
              TSNOACC(I)=TSNOACC(I)+TSNOROT(I,M)*FAREROT(I,M)
              WSNOACC(I)=WSNOACC(I)+WSNOROT(I,M)*FAREROT(I,M)
              SNOARE(I)=SNOARE(I)+FAREROT(I,M)
          ENDIF
          IF(TCANROT(I,M).GT.0.5) THEN
              TCANACC(I)=TCANACC(I)+TCANROT(I,M)*FAREROT(I,M)
              CANARE(I)=CANARE(I)+FAREROT(I,M)
          ENDIF
          SNOACC(I)=SNOACC(I)+SNOROT(I,M)*FAREROT(I,M)
          RCANACC(I)=RCANACC(I)+RCANROT(I,M)*FAREROT(I,M)
          SCANACC(I)=SCANACC(I)+SCANROT(I,M)*FAREROT(I,M)
          GROACC(I)=GROACC(I)+GROROT(I,M)*FAREROT(I,M)
          FSINACC(I)=FSINACC(I)+FSSROW(I)*FAREROT(I,M)
          FLINACC(I)=FLINACC(I)+FDLROW(I)*FAREROT(I,M)
          FLUTACC(I)=FLUTACC(I)+SBC*GTROT(I,M)**4*FAREROT(I,M)
          TAACC(I)=TAACC(I)+TAROW(I)*FAREROT(I,M)
          UVACC(I)=UVACC(I)+UVROW(I)*FAREROT(I,M)
          PRESACC(I)=PRESACC(I)+PRESROW(I)*FAREROT(I,M)
          QAACC(I)=QAACC(I)+QAROW(I)*FAREROT(I,M)
650   CONTINUE
675   CONTINUE
C
C     * CALCULATE AND PRINT DAILY AVERAGES.
C
      IF(NCOUNT.EQ.NDAY) THEN

      DO 800 I=1,NLTEST
          PREACC(I)=PREACC(I)
          GTACC(I)=GTACC(I)/REAL(NDAY)
          QEVPACC(I)=QEVPACC(I)/REAL(NDAY)
          EVAPACC(I)=EVAPACC(I)
          HFSACC(I)=HFSACC(I)/REAL(NDAY)
          HMFNACC(I)=HMFNACC(I)/REAL(NDAY)
          ROFACC(I)=ROFACC(I)
          OVRACC(I)=OVRACC(I)
          WTBLACC(I)=WTBLACC(I)/REAL(NDAY)
          DO 725 J=1,IGND
              TBARACC(I,J)=TBARACC(I,J)/REAL(NDAY)
              THLQACC(I,J)=THLQACC(I,J)/REAL(NDAY)
              THICACC(I,J)=THICACC(I,J)/REAL(NDAY)
              THALACC(I,J)=THALACC(I,J)/REAL(NDAY)
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
          SNOACC(I)=SNOACC(I)/REAL(NDAY)
          RCANACC(I)=RCANACC(I)/REAL(NDAY)
          SCANACC(I)=SCANACC(I)/REAL(NDAY)
          GROACC(I)=GROACC(I)/REAL(NDAY)
          FSINACC(I)=FSINACC(I)/REAL(NDAY)
          FLINACC(I)=FLINACC(I)/REAL(NDAY)
          FLUTACC(I)=FLUTACC(I)/REAL(NDAY)
          TAACC(I)=TAACC(I)/REAL(NDAY)
          UVACC(I)=UVACC(I)/REAL(NDAY)
          PRESACC(I)=PRESACC(I)/REAL(NDAY)
          QAACC(I)=QAACC(I)/REAL(NDAY)
          if (altotcntr_d(i) > 0) then
            ALTOTACC(I)=ALTOTACC(I)/REAL(altotcntr_d(i))
          else
            ALTOTACC(I)=0.
          end if
              FSSTAR=FSINACC(I)*(1.-ALTOTACC(I))
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
              GTOUT=GTACC(I)-TFREZ
C
             DO K=1,WF_NUM_POINTS !Create a DO loop for each folder     
             if(I .eq. N_OUT(K) .and. M .eq. II_out(K) ) THEN
            !IF there's is no combination, then the entire writing process will be skipped


             if ((iyear .ge. jdsty) .and. (iyear .le. jdendy)) then
              if ((iday .ge. jdstd) .and. (iday .le. jdendd)) then

              WRITE(61*k,6100) IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,
     1                       BEG,GTOUT,SNOACC(I),RHOSACC(I),
     2                       WSNOACC(I),ALTOTACC(I),ROFACC(I),CUMSNO
              IF(IGND.GT.3) THEN
                  WRITE(62*k,6201) IDAY,IYEAR,(TBARACC(I,J)-TFREZ,
     1                       THLQACC(I,J),THICACC(I,J),J=1,5)
                  WRITE(63*k,6201) IDAY,IYEAR,(TBARACC(I,J)-TFREZ,
     1                       THLQACC(I,J),THICACC(I,J),J=6,10)
              ELSE
                  WRITE(62*k,6200) IDAY,IYEAR,(TBARACC(I,J)-TFREZ,
     1                       THLQACC(I,J),THICACC(I,J),J=1,3),
     2                       TCN,RCANACC(I),SCANACC(I),TSN,ZSN
                  WRITE(63*k,6300) IDAY,IYEAR,FSINACC(I),FLINACC(I),
     1                       TAACC(I)-TFREZ,UVACC(I),PRESACC(I),
     2                       QAACC(I),PREACC(I),EVAPACC(I)
              ENDIF !IGND
             endif
            ENDIF
            ENDIF !The if of N_OUT
            ENDDO !End the DO of WF_Points
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
          OVRACC(I)=0.
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
          ALTOTACC(I) = 0.
          EVAPACC(I)=0.
          FLUTACC(I)=0.
800   CONTINUE

      ENDIF ! IF(NCOUNT.EQ.NDAY)

C===================== CTEM =====================================\
C
C     CALCULATE AND PRINT MOSAIC DAILY AVERAGES.
C
!       start -> FLAG JM
      DO 676 I=1,NLTEST
      DO 658 M=1,NMTEST
          PREACC_t(I,M)=PREACC_t(I,M)+PREROW(I)*DELT
          GTACC_t(I,M)=GTACC_t(I,M)+GTROT(I,M)
          QEVPACC_t(I,M)=QEVPACC_t(I,M)+QEVPROT(I,M)
          EVAPACC_t(I,M)=EVAPACC_t(I,M)+QFSROT(I,M)*DELT
          HFSACC_t(I,M)=HFSACC_t(I,M)+HFSROT(I,M)
          HMFNACC_t(I,M)=HMFNACC_t(I,M)+HMFNROT(I,M)
          ROFACC_t(I,M)=ROFACC_t(I,M)+ROFROT(I,M)*DELT
          OVRACC_t(I,M)=OVRACC_t(I,M)+ROFOROT(I,M)*DELT
          WTBLACC_t(I,M)=WTBLACC_t(I,M)+WTABROT(I,M)

          DO 626 J=1,IGND
              TBARACC_t(I,M,J)=TBARACC_t(I,M,J)+TBARROT(I,M,J)
              THLQACC_t(I,M,J)=THLQACC_t(I,M,J)+THLQROT(I,M,J)
              THICACC_t(I,M,J)=THICACC_t(I,M,J)+THICROT(I,M,J)
              THALACC_t(I,M,J)=THALACC_t(I,M,J)+(THLQROT(I,M,J)+
     1           THICROT(I,M,J))
626       CONTINUE

          ALVSACC_t(I,M)=ALVSACC_t(I,M)+ALVSROT(I,M)*FSVHROW(I)
          ALIRACC_t(I,M)=ALIRACC_t(I,M)+ALIRROT(I,M)*FSIHROW(I)
          IF(SNOROT(I,M).GT.0.0) THEN
              RHOSACC_t(I,M)=RHOSACC_t(I,M)+RHOSROT(I,M)
              TSNOACC_t(I,M)=TSNOACC_t(I,M)+TSNOROT(I,M)
              WSNOACC_t(I,M)=WSNOACC_t(I,M)+WSNOROT(I,M)
              SNOARE_t(I,M) = SNOARE_t(I,M) + 1.0 !FLAG test.
          ENDIF
          IF(TCANROT(I,M).GT.0.5) THEN
              TCANACC_t(I,M)=TCANACC_t(I,M)+TCANROT(I,M)
C              CANARE(I)=CANARE(I)+FAREROT(I,M)
          ENDIF
          SNOACC_t(I,M)=SNOACC_t(I,M)+SNOROT(I,M)
          RCANACC_t(I,M)=RCANACC_t(I,M)+RCANROT(I,M)
          SCANACC_t(I,M)=SCANACC_t(I,M)+SCANROT(I,M)
          GROACC_t(I,M)=GROACC_t(I,M)+GROROT(I,M)
          IF (FSSROW(I) .gt. 0.) THEN ! we will reuse the altotcntr_d counter values so don't need to do again.
            ALTOTACC_t(I,M)=ALTOTACC_t(I,M) + (FSSROW(I)-
     1                    (FSGVROT(I,M)+FSGSROT(I,M)+
     2                     FSGGROT(I,M)))/FSSROW(I)
          END IF
          FSINACC_t(I,M)=FSINACC_t(I,M)+FSSROW(I)
          FLINACC_t(I,M)=FLINACC_t(I,M)+FDLROW(I)
          FLUTACC_t(I,M)=FLUTACC_t(I,M)+SBC*GTROT(I,M)**4
          TAACC_t(I,M)=TAACC_t(I,M)+TAROW(I)
          UVACC_t(I,M)=UVACC_t(I,M)+UVROW(I)
          PRESACC_t(I,M)=PRESACC_t(I,M)+PRESROW(I)
          QAACC_t(I,M)=QAACC_t(I,M)+QAROW(I)
658   CONTINUE
676   CONTINUE
C
C     CALCULATE AND PRINT DAILY AVERAGES.


C
      IF(NCOUNT.EQ.NDAY) THEN

      DO 808 I=1,NLTEST
        DO 809 M=1,NMTEST
          PREACC_t(I,M)=PREACC_t(I,M)     !became [kg m-2 day-1] instead of [kg m-2 s-1]
          GTACC_t(I,M)=GTACC_t(I,M)/REAL(NDAY)
          QEVPACC_t(I,M)=QEVPACC_t(I,M)/REAL(NDAY)
          EVAPACC_t(I,M)=EVAPACC_t(I,M)   !became [kg m-2 day-1] instead of [kg m-2 s-1]
          HFSACC_t(I,M)=HFSACC_t(I,M)/REAL(NDAY)
          HMFNACC_t(I,M)=HMFNACC_t(I,M)/REAL(NDAY)
          ROFACC_t(I,M)=ROFACC_t(I,M)   !became [kg m-2 day-1] instead of [kg m-2 s-1
          OVRACC_t(I,M)=OVRACC_t(I,M)   !became [kg m-2 day-1] instead of [kg m-2 s-1]
          WTBLACC_t(I,M)=WTBLACC_t(I,M)/REAL(NDAY)
          DO 726 J=1,IGND
            TBARACC_t(I,M,J)=TBARACC_t(I,M,J)/REAL(NDAY)
            THLQACC_t(I,M,J)=THLQACC_t(I,M,J)/REAL(NDAY)
            THICACC_t(I,M,J)=THICACC_t(I,M,J)/REAL(NDAY)
            THALACC_t(I,M,J)=THALACC_t(I,M,J)/REAL(NDAY)
726       CONTINUE
C
          IF(FSINACC_t(I,M).GT.0.0) THEN
            ALVSACC_t(I,M)=ALVSACC_t(I,M)/(FSINACC_t(I,M)*0.5)
            ALIRACC_t(I,M)=ALIRACC_t(I,M)/(FSINACC_t(I,M)*0.5)
          ELSE
            ALVSACC_t(I,M)=0.0
            ALIRACC_t(I,M)=0.0
          ENDIF
C
          SNOACC_t(I,M)=SNOACC_t(I,M)/REAL(NDAY)
          if (SNOARE_t(I,M) .GT. 0.) THEN
             RHOSACC_t(I,M)=RHOSACC_t(I,M)/SNOARE_t(I,M)
             TSNOACC_t(I,M)=TSNOACC_t(I,M)/SNOARE_t(I,M)
             WSNOACC_t(I,M)=WSNOACC_t(I,M)/SNOARE_t(I,M)
          END IF
          TCANACC_t(I,M)=TCANACC_t(I,M)/REAL(NDAY)
          RCANACC_t(I,M)=RCANACC_t(I,M)/REAL(NDAY)
          SCANACC_t(I,M)=SCANACC_t(I,M)/REAL(NDAY)
          GROACC_t(I,M)=GROACC_t(I,M)/REAL(NDAY)
          FSINACC_t(I,M)=FSINACC_t(I,M)/REAL(NDAY)
          FLINACC_t(I,M)=FLINACC_t(I,M)/REAL(NDAY)
          FLUTACC_t(I,M)=FLUTACC_t(I,M)/REAL(NDAY)
          TAACC_t(I,M)=TAACC_t(I,M)/REAL(NDAY)
          UVACC_t(I,M)=UVACC_t(I,M)/REAL(NDAY)
          PRESACC_t(I,M)=PRESACC_t(I,M)/REAL(NDAY)
          QAACC_t(I,M)=QAACC_t(I,M)/REAL(NDAY)
          if (altotcntr_d(i) > 0) then
            ALTOTACC_t(I,M)=ALTOTACC_t(I,M)/REAL(altotcntr_d(i))
          else
            ALTOTACC_t(I,M)=0.
          end if
          FSSTAR=FSINACC_t(I,M)*(1.-ALTOTACC_t(I,M))
          FLSTAR=FLINACC_t(I,M)-FLUTACC_t(I,M)
          QH=HFSACC_t(I,M)
          QE=QEVPACC_t(I,M)
          QEVPACC_M_SAVE(I,M)=QEVPACC_t(I,M)   !FLAG! What is the point of this? JM Apr 1 2015
          BEG=FSSTAR+FLSTAR-QH-QE
          SNOMLT=HMFNACC_t(I,M)
C
          IF(RHOSACC_t(I,M).GT.0.0) THEN
              ZSN=SNOACC_t(I,M)/RHOSACC_t(I,M)
          ELSE
              ZSN=0.0
          ENDIF
C
          IF(TCANACC_t(I,M).GT.0.01) THEN
              TCN=TCANACC_t(I,M)-TFREZ
          ELSE
              TCN=0.0
          ENDIF
C
          IF(TSNOACC_t(I,M).GT.0.01) THEN
              TSN=TSNOACC_t(I,M)-TFREZ
          ELSE
              TSN=0.0
          ENDIF
C
          GTOUT=GTACC_t(I,M)-TFREZ
C


          DO K=1,WF_NUM_POINTS !Create a DO loop for each folder     
           if(I .eq. N_OUT(K) .and. M .eq. II_out(K) ) THEN
            !IF there is no combination, then the entire writing process will be skipped

          if ((iyear .ge. jdsty) .and. (iyear .le. jdendy)) then
           if ((iday .ge. jdstd) .and. (iday .le. jdendd)) then
C
C         WRITE TO OUTPUT FILES

          WRITE(611*k,6100) IDAY,IYEAR,FSSTAR,FLSTAR,QH,QE,SNOMLT,
     1                    BEG,GTOUT,SNOACC_t(I,M),RHOSACC_t(I,M),
     2                    WSNOACC_t(I,M),ALTOTACC_t(I,M),ROFACC_t(I,M),
     3                    CUMSNO,' TILE ',M
            IF(IGND.GT.3) THEN
               WRITE(621*k,6201) IDAY,IYEAR,(TBARACC_t(I,M,J)-TFREZ,
     1                  THLQACC_t(I,M,J),THICACC_t(I,M,J),J=1,5),
     2                  ' TILE ',M
               WRITE(631*k,6201) IDAY,IYEAR,(TBARACC_t(I,M,J)-TFREZ,
     1                  THLQACC_t(I,M,J),THICACC_t(I,M,J),J=6,10),
     2                  ' TILE ',M
            ELSE
               WRITE(621*k,6200) IDAY,IYEAR,(TBARACC_t(I,M,J)-TFREZ,
     1                  THLQACC_t(I,M,J),THICACC_t(I,M,J),J=1,3),
     2                  TCN,RCANACC_t(I,M),SCANACC_t(I,M),TSN,ZSN,
     3                  ' TILE ',M


               WRITE(631*k,6300) IDAY,IYEAR,FSINACC_t(I,M),
     1                    FLINACC_t(I,M), TAACC_t(I,M)-TFREZ,
     2                    UVACC_t(I,M),PRESACC_t(I,M),
     3                    QAACC_t(I,M), PREACC_t(I,M),
     4                    EVAPACC_t(I,M),
     5                   ' TILE ',M
            ENDIF


C
           endif
          ENDIF ! IF write daily
          ENDIF ! If we have the desired GRU/Grid
          ENDDO !END the K loop

C
C          INITIALIZATION FOR MOSAIC TILE AND GRID VARIABLES
C
            call resetclassaccum(nltest,nmtest)
C
809   CONTINUE
808   CONTINUE

      ENDIF ! IF(NCOUNT.EQ.NDAY)
C
      ENDIF !  IF(.NOT.PARALLELRUN)

C=======================================================================

!     Only bother with monthly calculations if we desire those outputs to be written out.
      if (iyear .ge. jmosty) then

        call class_monthly_aw(IDAY,IYEAR,NCOUNT,NDAY,SBC,DELT,
     1                       nltest,nmtest,ALVSROT,FAREROT,FSVHROW,
     2                       ALIRROT,FSIHROW,GTROT,FSSROW,FDLROW,
     3                       HFSROT,ROFROT,PREROW,QFSROT,QEVPROT,
     4                       SNOROT,TAROW,WSNOROT,TBARROT,THLQROT,
     5                       THICROT,TFREZ,QFCROT,QFGROT,QFNROT,
     6                       QFCLROT,QFCFROT,FSGVROT,FSGSROT,
     7                       FSGGROT)

       DO NT=1,NMON
        IF(IDAY.EQ.monthend(NT+1).AND.NCOUNT.EQ.NDAY)THEN
         IMONTH=NT
        ENDIF
       ENDDO

      end if !skip the monthly calculations/writing unless iyear>=jmosty

      call class_annual_aw(IDAY,IYEAR,NCOUNT,NDAY,SBC,DELT,
     1                       nltest,nmtest,ALVSROT,FAREROT,FSVHROW,
     2                       ALIRROT,FSIHROW,GTROT,FSSROW,FDLROW,
     3                       HFSROT,ROFROT,PREROW,QFSROT,QEVPROT,
     4                       TAROW,QFCROT,FSGVROT,FSGSROT,FSGGROT,
     5                       leapnow)

c     CTEM output and write out

      if(.not.parallelrun) then ! stand alone mode, includes daily and yearly mosaic-mean output for ctem

c     calculate daily outputs from ctem

       if (ctem_on) then
         if(ncount.eq.nday) then
          call ctem_daily_aw(nltest,nmtest,iday,FAREROT,
     1                      iyear,jdstd,jdsty,jdendd,jdendy,grclarea,
     2                      onetile_perPFT,WF_NUM_POINTS,N_OUT,II_OUT)
         endif ! if(ncount.eq.nday)
       endif ! if(ctem_on)

       endif ! if(not.parallelrun)

c=======================================================================


c     Calculate monthly & yearly output for ctem


c     First initialize some output variables
c     initialization is done just before use.

      if (ctem_on) then
       if(ncount.eq.nday) then

!     Only bother with monthly calculations if we desire those outputs to be written out.
      if (iyear .ge. jmosty) then

        call ctem_monthly_aw(nltest,nmtest,iday,FAREROT,iyear,nday,
     1                        onetile_perPFT)

        end if !to write out the monthly outputs or not

c       Accumulate and possibly write out yearly outputs
            call ctem_annual_aw(nltest,nmtest,iday,FAREROT,iyear,
     1                           onetile_perPFT,leapnow)

      endif ! if(ncount.eq.nday)
      endif ! if(ctem_on)

C     OPEN AND WRITE TO THE RESTART FILES


       IF ((leapnow .and.IDAY.EQ.366.AND.NCOUNT.EQ.NDAY) .OR. 
     &  (.not. leapnow .and.IDAY.EQ.365.AND.NCOUNT.EQ.NDAY)) THEN


        IF (RSFILE) THEN
C       WRITE .INI_RS FOR CLASS RESTART DATA

        OPEN(UNIT=100,FILE=ARGBUFF(1:STRLEN(ARGBUFF))//'.INI_RS')

        WRITE(100,5010) TITLE1,TITLE2,TITLE3,TITLE4,TITLE5,TITLE6
        WRITE(100,5010) NAME1,NAME2,NAME3,NAME4,NAME5,NAME6
        WRITE(100,5010) PLACE1,PLACE2,PLACE3,PLACE4,PLACE5,PLACE6
        WRITE(100,5020)DLATROW(1),DEGLON,ZRFMROW(1),ZRFHROW(1),
     1                 ZBLDROW(1),GCROW(1),NLTEST,NMTEST
        DO I=1,NLTEST
          DO M=1,NMTEST

C         IF START_BARE (SO EITHER COMPETE OR LNDUSEON), THEN WE NEED TO CREATE
C         THE FCANROT FOR THE RS FILE.
          IF (START_BARE .AND. onetile_perPFT) THEN
           IF (M .LE. 2) THEN                     !NDL
            FCANROT(I,M,1)=1.0
           ELSEIF (M .GE. 3 .AND. M .LE. 5) THEN  !BDL
            FCANROT(I,M,2)=1.0
           ELSEIF (M .EQ. 6 .OR. M .EQ. 7) THEN  !CROP
            FCANROT(I,M,3)=1.0
           ELSEIF (M .EQ. 8 .OR. M .EQ. 9) THEN  !GRASSES
            FCANROT(I,M,4)=1.0
           ELSE                                  !BARE
            FCANROT(I,M,5)=1.0
           ENDIF
          ENDIF !START_BARE/onetile_perPFT

            WRITE(100,5040) (FCANROT(I,M,J),J=1,ICAN+1),(PAMXROT(I,M,J),
     1                      J=1,ICAN)
            WRITE(100,5040) (LNZ0ROT(I,M,J),J=1,ICAN+1),(PAMNROT(I,M,J),
     1                      J=1,ICAN)
            WRITE(100,5040) (ALVCROT(I,M,J),J=1,ICAN+1),(CMASROT(I,M,J),
     1                      J=1,ICAN)
            WRITE(100,5040) (ALICROT(I,M,J),J=1,ICAN+1),(ROOTROT(I,M,J),
     1                      J=1,ICAN)
            WRITE(100,5030) (RSMNROT(I,M,J),J=1,ICAN),
     1                      (QA50ROT(I,M,J),J=1,ICAN)
            WRITE(100,5030) (VPDAROT(I,M,J),J=1,ICAN),
     1                      (VPDBROT(I,M,J),J=1,ICAN)
            WRITE(100,5030) (PSGAROT(I,M,J),J=1,ICAN),
     1                      (PSGBROT(I,M,J),J=1,ICAN)
            WRITE(100,5040) DRNROT(I,M),SDEPROT(I,M),FAREROT(I,M)
            WRITE(100,5090) XSLPROT(I,M),GRKFROT(I,M),WFSFROT(I,M),
     1                      WFCIROT(I,M),MIDROT(I,M)
            WRITE(100,5080) (SANDROT(I,M,J),J=1,3)
            WRITE(100,5080) (CLAYROT(I,M,J),J=1,3)
            WRITE(100,5080) (ORGMROT(I,M,J),J=1,3)
C           Temperatures are in degree C
            IF (TCANROT(I,M).NE.0.0) TCANRS(I,M)=TCANROT(I,M)-273.16
            IF (TSNOROT(I,M).NE.0.0) TSNORS(I,M)=TSNOROT(I,M)-273.16
            IF (TPNDROT(I,M).NE.0.0) TPNDRS(I,M)=TPNDROT(I,M)-273.16
            WRITE(100,5050) (TBARROT(I,M,J)-273.16,J=1,3),TCANRS(I,M),
     2                      TSNORS(I,M),TPNDRS(I,M)
            WRITE(100,5060) (THLQROT(I,M,J),J=1,3),(THICROT(I,M,J),
     1                      J=1,3),ZPNDROT(I,M)
            WRITE(100,5070) RCANROT(I,M),SCANROT(I,M),SNOROT(I,M),
     1                      ALBSROT(I,M),RHOSROT(I,M),GROROT(I,M)
C           WRITE(100,5070) 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
          ENDDO
        ENDDO

        DO J=1,IGND
          WRITE(100,5002) DELZ(J),ZBOT(J)
        ENDDO
5002  FORMAT(2F8.3)
C
        WRITE(100,5200) JHHSTD,JHHENDD,JDSTD,JDENDD
        WRITE(100,5200) JHHSTY,JHHENDY,JDSTY,JDENDY
        CLOSE(100)
C
c       write .CTM_RS for ctem restart data
c
         if (ctem_on) then
            call write_ctm_rs(nltest,nmtest,FCANROT,argbuff)
        endif ! ctem_on
c
       endif ! if iday=365/366
      endif ! if generate restart files
c
c      check if the model is done running.

       if ((leapnow.and.iday.eq.366.and.ncount.eq.nday) .or.
     &   (.not. leapnow .and.iday.eq.365.and.ncount.eq.nday)) then

          if (cyclemet .and. climiyear .ge. metcycendyr) then

            lopcount = lopcount+1

             if(lopcount.le.ctemloop .and. .not. transient_run)then

              rewind(12)   ! rewind met file

               if(obswetf) then
                rewind(16) !rewind obswetf file
                read(16,*) ! read in the header
               endif
              if (obslght) then
                 obslghtyr=-9999
                 rewind(17)
              endif

              met_rewound = .true.
              iyear=-9999
              obswetyr=-9999

               if(popdon) then
                 rewind(13) !rewind popd file
                 read(13,*) ! skip header (3 lines)
                 read(13,*) ! skip header (3 lines)
                 read(13,*) ! skip header (3 lines)
               endif
               if((co2on .or. ch4on) .and. trans_startyr < 0) then
                 rewind(14) !rewind co2 file
               endif

             else if (lopcount.le.ctemloop .and. transient_run)then
             ! rewind only the MET file (since we are looping over the MET  while
             ! the other inputs continue on.
               rewind(12)   ! rewind met file

               if (obslght) then ! FLAG
                 obslghtyr=-999
                 rewind(17)
                 do while (obslghtyr .lt. metcylyrst)
                   do i=1,nltest
                    read(17,*) obslghtyr,(mlightngrow(i,1,j),j=1,12) ! read into the first tile
                    if (nmtest > 1) then
                      do m = 2,nmtest !spread grid values over all tiles for easier use in model
                        mlightngrow(i,m,:) = mlightngrow(i,1,:)
                      end do
                    end if
                   end do
                 end do
                 backspace(17)
               endif
             else

              if (transient_run .and. cyclemet) then
              ! Now switch from cycling over the MET to running through the file
              rewind(12)   ! rewind met file
              if (obslght) then !FLAG
                 obslghtyr=-999
                 rewind(17)
                 do while (obslghtyr .lt. metcylyrst)
                   do i=1,nltest
                    read(17,*) obslghtyr,(mlightngrow(i,1,j),j=1,12) ! read into the first tile
                    if (nmtest > 1) then
                      do m = 2,nmtest !spread grid values over all tiles for easier use in model
                        mlightngrow(i,m,:) = mlightngrow(i,1,:)
                      end do
                    end if
                   end do
                 end do
               backspace(17)
              endif
              cyclemet = .false.
              lopcount = 1
              endyr = metcylyrst + ncyear - 1  !set the new end year. We assume you are starting from the start of your MET file!
              
              else
               run_model = .false.
              endif

             endif

          else if (iyear .eq. endyr .and. .not. cyclemet) then

             run_model = .false.

          endif !if cyclemet and iyear > metcycendyr
       endif !last day of year check

C===================== CTEM =====================================/
C
        NCOUNT=NCOUNT+1
        IF(NCOUNT.GT.NDAY) THEN
            NCOUNT=1
        ENDIF

!DAN    Must again convert from NLAT/NMOS to tile indexing to transfer values back to MESH.
      DO I=IL1,IL2

        stas%cnpy%rcan(I) = RCANGAT(I)
        stas%cnpy%sncan(I) = SCANGAT(I) !Remember : SNCAN is in MESH, while CTEM uses SCAN
        stas%cnpy%cmai(I) = CMAIGAT(I)
        stas%cnpy%tac(I) = TACGAT(I)
        stas%cnpy%tcan(I) = TCANGAT(I)
        stas%cnpy%qac(I) = QACGAT(I)
        stas%cnpy%gro(I) = GROGAT(I)
        stas%cnpy%pevp(I) = PETGAT(I)
        stas%sno%sno(I) = SNOGAT(I)
        stas%sno%albs(I) = ALBSGAT(I)
        stas%sno%fsno(I) = FSNOGAT(I)
        stas%sno%rhos(I) = RHOSGAT(I)
        stas%sno%tsno(I) = TSNOGAT(I)
        stas%sno%wsno(I) = WSNOGAT(I)
        stas%sfc%alvs(I) = ALVSGAT(I)
        stas%sfc%alir(I) = ALIRGAT(I)
        stas%sfc%gte(I) = GTGAT(I)  !Note that MESH uses GTE, while classctem uses GT
        stas%sfc%tpnd(I) = TPNDGAT(I)
        stas%sfc%zpnd(I) = ZPNDGAT(I)
        stas%sfc%evap(I) = QFSGAT(I) 
        stas%sfc%rofo(I) = ROFOGAT(I)
        stas%sfc%qevp(I) = QEVPGAT(I)
        stas%sfc%hfs(I) = HFSGAT(I)
        stas%sfc%gzero(I) = GFLXGAT(I, 1)
        stas%sfc%tsfs(I, :) = TSFSGAT(I, :)
        stas%sl%tbas(I) = TBASGAT(I)
        stas%sl%rofs(I) = ROFSGAT(I)
        stas%sl%thic(I, :) = THICGAT(I, :)
        stas%sl%thlq(I, :) = THLQGAT(I, :)
        stas%sl%tbar(I, :) = TBARGAT(I, :)
        stas%sl%gflx(I, :) = GFLXGAT(I, :)
        stas%lzs%rofb(I) = ROFBGAT(I)
      ENDDO


          END SUBROUTINE !Since this is the end of the subroutine, this part will essentially be the end of one run_within_tile simulation
      

!      ENDDO !MAIN MODEL LOOP Don't need the main loop 

      SUBROUTINE RUNCLASS36CTEM_finalize()


        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use FLAGS


C     MODEL RUN HAS COMPLETED SO NOW CLOSE OUTPUT FILES AND EXIT
C==================================================================
c
c      checking the time spent for running model
c
c      call idate(today)
c      call itime(now)
c      write(*,1001) today(2), today(1), 2000+today(3), now
c 1001 format( 'end date: ', i2.2, '/', i2.2, '/', i4.4,
c     &      '; end time: ', i2.2, ':', i2.2, ':', i2.2 )
c
      IF (.NOT. PARALLELRUN) THEN
C       FIRST ANY CLASS OUTPUT FILES
        CLOSE(61)
        CLOSE(62)
        CLOSE(63)
        CLOSE(64)
        CLOSE(65)
        CLOSE(66)
        CLOSE(67)
        CLOSE(68)
        CLOSE(69)
        CLOSE(611)
        CLOSE(621)
        CLOSE(631)
        CLOSE(641)
        CLOSE(651)
        CLOSE(661)
        CLOSE(671)
        CLOSE(681)
        CLOSE(691)
        end if ! moved this up from below so it calls the close subroutine. JRM.

c       then ctem ones

        call close_outfiles()
c
c     close the input files too
      close(12)
      close(13)
      close(14)
      if (obswetf) then
        close(16)  !*.WET
      end if
      if (obslght) then
         close(17)
      end if
      call exit
C
c         the 999 label below is hit when an input file reaches its end.
999       continue

            lopcount = lopcount+1

             if(lopcount.le.ctemloop)then

              rewind(12)   ! rewind met file

                if(obswetf) then
                  rewind(16) !rewind obswetf file
                  read(16,*) ! read in the header
                endif

              met_rewound = .true.
              iyear=-9999
              obswetyr=-9999   !Rudra

               if(popdon) then
                 rewind(13) !rewind popd file
                 read(13,*) ! skip header (3 lines)
                 read(13,*) ! skip header
                 read(13,*) ! skip header
               endif
               if((co2on .or. ch4on) .and. trans_startyr < 0) then
                 rewind(14) !rewind co2 file
               endif
              if (obslght) then
                 rewind(17)
              endif

             else

              run_model = .false.

             endif

c     return to the time stepping loop
      if (run_model) then
!         goto 200
      else

c     close the output files
C
c     FIRST ANY CLASS OUTPUT FILES
      IF (.NOT. PARALLELRUN) THEN

        CLOSE(61)
        CLOSE(62)
        CLOSE(63)
        CLOSE(64)
        CLOSE(65)
        CLOSE(66)
        CLOSE(67)
        CLOSE(68)
        CLOSE(69)
        CLOSE(611)
        CLOSE(621)
        CLOSE(631)
        CLOSE(641)
        CLOSE(651)
        CLOSE(661)
        CLOSE(671)
        CLOSE(681)
        CLOSE(691)
      end if

c     Then the CTEM ones
      call close_outfiles()

C     CLOSE THE INPUT FILES TOO
      CLOSE(12)
      CLOSE(13)
      CLOSE(14)

         CALL EXIT
      END IF

1001  continue

       write(*,*)'Error while reading WETF file'
       run_model=.false.



C ============================= CTEM =========================/

	!Copy over the state variables so that they can be used for MESH later



       END SUBROUTINE           !This is the end of the entire RUNCLASS36CTEM_finalize subroutine, or the third subroutine in this file


      INTEGER FUNCTION STRLEN(ST)
      INTEGER       I
      CHARACTER     ST*(*)
      I = LEN(ST)
      DO WHILE (ST(I:I) .EQ. ' ')
        I = I - 1
      ENDDO
      STRLEN = I
      RETURN
      END FUNCTION


      END MODULE
