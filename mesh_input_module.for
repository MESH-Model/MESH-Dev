      module MESH_INPUT_MODULE

      INTEGER NYEARS
      REAL, DIMENSION(:), ALLOCATABLE :: t0_ACC
      TYPE OutputPoints
!* N_OUT: GRID SQUARE TO OUTPUT
!* II_OUT: GRU TO OUTPUT
        INTEGER, DIMENSION(:), ALLOCATABLE :: N_OUT, II_OUT
!* DIR_OUT: OUTPUT DIRECTORY (10 CHARACTER STRING)
        CHARACTER*10, DIMENSION(:), ALLOCATABLE :: DIR_OUT
      END TYPE

!> variables used in drainage database or new_shd.r2c
      !> TYPE ShedInformation 

!* WF_ELEV: GRID SQUARE ELEVATION
!* WF_ICHNL: RIVER CLASS
!* WF_NEXT: RECEIVING GRID SQUARE
!* WF_CHANNELSLOPE: INTERNAL CHANNEL SLOPE
        INTEGER, DIMENSION(:), ALLOCATABLE :: WF_IBN,
     +    WF_IROUGH, WF_ICHNL, WF_NEXT, WF_ELEV, WF_IREACH
        REAL*8, DIMENSION(:), ALLOCATABLE :: WF_DA, WF_BNKFLL,
     +    WF_CHANNELSLOPE
!* BASIN_FRACTION: is the fraction of each square that's in the basin
        REAL, DIMENSION(:), ALLOCATABLE :: BASIN_FRACTION
      !END TYPE

!> variables for reading soil_levels
      TYPE SoilLevels
!* DELZ: SOIL LAYER DEPTH (SOIL_LEVELS.TXT)
!* ZBOT: CUMULATIVE SOIL DEPTH (SOIL_LEVELS.TXT)
        REAL, DIMENSION(:), ALLOCATABLE :: DELZ, ZBOT
      END TYPE

!!variables for reading parameters_class.ini
      TYPE ClassParameters
       !> ATMOSPHERIC AND GRID-CONSTANT INPUT VARIABLES:
        REAL, DIMENSION(:), ALLOCATABLE :: ZRFMGRD,
     +    ZRFHGRD, ZBLDGRD, GCGRD
!> CANOPY AND SOIL INFORMATION (CLASS):
        REAL, DIMENSION(:, :, :), ALLOCATABLE ::
     +    FCANROW, LNZ0ROW, ALVCROW, ALICROW, PAMXROW, PAMNROW,
     +    CMASROW, ROOTROW, RSMNROW, QA50ROW, VPDAROW, VPDBROW, PSGAROW,
     +    PSGBROW
!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
!* dden (DDEN): DRAINAGE DENSITY (CLASS.INI)
!* MANN (WFSF): MANNING'S n (CLASS.INI)
        REAL, DIMENSION(:, :), ALLOCATABLE :: DRNROW,  XSLPROW,
     +    XDROW, SDEPROW, FAREROW, DDROW,   MANNROW, TCANROW,
     +    TSNOROW, TPNDROW, ZPNDROW, rncanROW, sncanROW, SNOROW,
     +    ALBSROW, RHOSROW, GROROW,  KSROW
!* MIDROW: DEFINITION IN CLASS DOCUMENTATION (CLASS.INI)
        INTEGER, DIMENSION(:, :), ALLOCATABLE :: MIDROW
!> LAND SURFACE PROGNOSTIC VARIABLES (CLASS.INI):
!* TBAR: INITIAL SOIL LAYER TEMPERATURE
!* THLQ: INITIAL SOIL LAYER LIQUID WATER CONTENT
!* THIC: INITIAL SOIL LAYER ICE WATER CONTENT
!* SAND: PERCENT-CONTENT OF SAND IN SOIL LAYER (CLASS.INI)
!* CLAY: PERCENT-CONTENT OF CLAY IN SOIL LAYER (CLASS.INI)
!* ORGM: PERCENT-CONTENT OF ORGANIC MATTER IN SOIL LAYER (CLASS.INI)
        REAL, DIMENSION(:, :, :), ALLOCATABLE ::
     +    SANDROW, CLAYROW, ORGMROW,
     +    TBARROW, THLQROW, THICROW
      END TYPE

!> variables for read_soil_ini
      TYPE SoilValues
        REAL, DIMENSION(:,:), ALLOCATABLE :: wc_algwet, wc_algdry
        REAL, DIMENSION(:,:,:), ALLOCATABLE :: wc_thpor, wc_thlret,
     +    wc_thlmin, wc_bi, wc_psisat, wc_grksat, wc_hcps, wc_tcs
      END TYPE

!> variables for read_parameters_hydrology
      TYPE HydrologyParameters
        REAL, DIMENSION(:,:), ALLOCATABLE :: ZSNLROW, ZPLSROW, ZPLGROW, 
     +                                       FRZCROW,
     +                                       CMAXROW, CMINROW, BROW,
     +                                       K1ROW,   K2ROW,
     +                      fetchROW, HtROW, N_SROW, A_SROW, DistribROW
      END TYPE

      end module MESH_INPUT_MODULE
