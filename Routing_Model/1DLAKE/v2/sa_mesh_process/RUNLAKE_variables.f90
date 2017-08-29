module RUNLAKE_variables

    use RUNLAKE_constants
    use RUNCLASS36_variables

    implicit none

    integer :: NMW

    !> Lake tile parameters.
    !*  NTYPEL: Number of lake tiles. [--].
    !*  DELT_L: Time-step for the lake model (usually DELT_L < model time-step). [s].
    integer NTYPEL
    real DELT_L
    character(len = 10) LAKEOUTDIR
    type tile
        integer NLAK
        character(len = 8) lakeTile
        real HLAK, LLAK, BLAK
        real, dimension(:), allocatable :: TLAK, FARE
    end type

    type LAKE_tile_paramGat
        real, dimension(:), allocatable :: HLAKGAT, LLAKGAT, BLAKGAT
        integer, dimension(:), allocatable :: NLAKGAT
        real, dimension(:, :), allocatable :: TLAKGAT
    end type
    real, dimension(:), allocatable :: &
        ASVLGAT, ASILGAT, BCSNLGAT, REFLGAT, ZDMLGAT, ZDHLGAT

    !> Lake prognostic variables.
    type LAKE_pronostic_variables
        real, dimension(:), allocatable :: &
            EXPW, DTEMP, HDPTH, DELU, GRED, TKELAK, T0LAK, LKICEH, &
            RHOMIX, TSED, SNICEH, ROFICEH
    end type

    !> Lake diagnostic variables.
    integer, parameter :: SP = kind(1.0)
    integer, parameter :: DP = selected_real_kind(2*precision(1.0_sp))
    type LAKE_diagnostic_variables
        real(kind = DP), dimension(:), allocatable :: CTLSTP
        real, dimension(:), allocatable :: &
            HFSL, HEVL, FSGL, FLGL, HMFL, HTCL, FICE, &
            FLS, G0SL, FSGSL, FLGSL, HFSSL, HEVSL, HMFNL, HTCSL, &
            PCPL, PCPNL, QFL, QFNL, ROFNL, SFTL, SFUL, SFVL, &
            SFQL, SFHL, QLWOL, ALVL, ALIL, EFL, GTL, QGL, DRL, &
            PETL, QSENL, TFXL, QEVPL, QFSL, QFXL, SNOL, RHOSL, &
            TSNOL, ALBSL, WSNOL
        real, dimension(:, :), allocatable :: FSDBL, FSFBL, FSSBL
    end type

    type(tile), dimension(:), allocatable, save :: lakeTileParam
    type(LAKE_tile_paramGat), save :: ltp
    type(CLASS_forcing_input), save :: cfiL
    type(CLASS_atmospheric_variables), save :: catvL
    type(CLASS_diagnostic_variables), save :: cdvL
    type(LAKE_pronostic_variables), save :: lpv, lpvi, lpvg
    type(LAKE_diagnostic_variables), save :: ldv, ldvi, ldvg

end module
