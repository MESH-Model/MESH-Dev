      module MESH_INPUT_MODULE

      INTEGER NYEARS
      REAL, DIMENSION(:), ALLOCATABLE :: t0_ACC
      TYPE OutputPoints
!* N_OUT: GRID SQUARE TO OUTPUT
!* II_OUT: GRU TO OUTPUT
        INTEGER, DIMENSION(:), ALLOCATABLE :: N_OUT, II_OUT, K_OUT
!* DIR_OUT: OUTPUT DIRECTORY (10 CHARACTER STRING)
        CHARACTER*10, DIMENSION(:), ALLOCATABLE :: DIR_OUT
      END TYPE

!> variables used in drainage database or new_shd.r2c
      !> TYPE ShedInformation 

!* WF_ELEV: GRID SQUARE ELEVATION
!* WF_ICHNL: RIVER CLASS
!* WF_NEXT: RECEIVING GRID SQUARE
!* WF_CHANNELSLOPE: INTERNAL CHANNEL SLOPE
!        INTEGER, DIMENSION(:), ALLOCATABLE :: WF_IBN,
!     +    WF_IROUGH, WF_ICHNL, WF_NEXT, WF_ELEV, WF_IREACH
!        REAL*8, DIMENSION(:), ALLOCATABLE :: WF_DA, WF_BNKFLL,
!     +    WF_CHANNELSLOPE
!* BASIN_FRACTION: is the fraction of each square that's in the basin
!        REAL, DIMENSION(:), ALLOCATABLE :: BASIN_FRACTION
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
!* DD (DDEN): DRAINAGE DENSITY (CLASS.INI)
!* MANN (WFSF): MANNING'S n (CLASS.INI)
        REAL, DIMENSION(:, :), ALLOCATABLE :: DRNROW,  XSLPROW,
     +    XDROW, SDEPROW, FAREROW, DDROW,   MANNROW, TCANROW,
     +    TSNOROW, TPNDROW, ZPNDROW, RCANROW, SCANROW, SNOROW,
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

      contains

!>    ******************************************************************
!>    Calculate and return array indices accounting for where grid
!>    indices lie in the NML sequence.
!>    ******************************************************************
      subroutine GetIndices(inp, izero, ipid,
     +                      NML, ILMOS,
     +                      il1, il2, ilen)

      integer, intent(in) :: inp, izero, ipid
      integer, intent(in) :: NML
      integer, intent(in), dimension(:) :: ILMOS

      integer, intent(out) :: il1, il2, ilen

!>    Calculate an initial lower index.
      il1 = max(
     +        min(ceiling(NML/real(inp-izero))*(ipid-izero)+1, NML), 0)

!>    On succeeding nodes, bump the index to begin at the next grid in
!>    the sequence if otherwise the GRUs and/or tiles of the grid would
!>    be broken across nodes.
      if (ipid > (0+izero)) then
        do while (ILMOS(il1-1) == ILMOS(il1))
          il1 = il1+1
        end do
      end if

!>    Calculate an initial upper index.
      il2 = max(
     +        min(
     +          ceiling(NML/real(inp-izero))*((ipid-izero)+1),NML),il1)

!>    Bump the index to include the entire grid so that the GRUs and/or
!>    tiles of the grid are not broken across nodes.
      if (ipid < (inp-1)) then
        do while (ILMOS(il2) == ILMOS(il2+1) .and. il2 < NML)
          il2 = il2+1
        end do
      end if

!>    Calculate the total number of active elements in the sequence.
      ilen = il2-il1+1

      end subroutine

      end module MESH_INPUT_MODULE
