module RUNLAKE_variables

    use RUNLAKE_constants

    implicit none

    !*  NLAKMAX: Max number of lake levels. [--].
    !*  DTS: Time-step for the lake model (usually less than the model time-step). [s].
    type RUNLAKE_option_flags
        integer :: ISLFD = 2, IZREF = 1, ITG = 2, IALS = 0, NBS = 4, ISNOALB = 0, IPCP = 1, NLYRMAX = 200
        real :: DELZLK = 0.5, DTS = 1800.0
    end type

    !*  HLAK: Mean depth of lake. [m].
    !*  BLAK: Mean light extinction coefficient. [m-1].
    !*  LLAK: Lake horizontal length scale. [m].
    !*  NLAK: Number of vertical levels. [--].
    !*  TLAK: Temperature profile of lake tile. [K].
    type RUNLAKE_parameters
        real, dimension(:), allocatable :: HLAK, LLAK, BLAK
        integer, dimension(:), allocatable :: NLAK
        real, dimension(:, :), allocatable :: TLAK, FRAC
    end type

    !*  QSWINV: Visible shortwave radiation. [W m-2].
    !*  QSWINI: Near infrared shortwave radiation. [W m-2].
    !*  QLWIN: Downwelling longwave radiation. [W m-2].
    !*  PRES: Surface atmospheric pressure. [Pa].
    !*  ZREFH: Reference height for air temperature and humidity. [m].
    !*  QA: Specific humidity at reference height. [kg kg-1].
    !*  TA: Air temperature at reference height. [K].
    !*  ZREFM: Reference height for wind. [m].
    !*  UWIND: Zonal wind speed. [m s-1].
    !*  VWIND: Meridional wind speed. [m s-1].
    !*  RHOAIR: Density of air. [kg m-3].
    !*  TADP: Dew point temperature of air. [K].
    !*  PADRY: Partial pressure of dry air. [Pa].
    !*  VPD: Vapour pressure deficit of air. [mb].
    !*  RHOSNI: Density of fresh snow. [kg m-3].
    !*  RPRE: User-provided rainfall rate over modelled area. [kg m-2 s-1].
    !*  SPRE: User-provided snowfall rate over modelled area. [kg m-2 s-1].
    !*  PCPR: Precipitation rate over modelled area. [kg m-2 s-1].
    !*  RPCP: Calculated rainfall rate over modelled area. [m s-1].
    !*  TRPCP: Rainfall temperature over modelled area. [degrees C].
    !*  SPCP: Calculated snowfall rate over modelled area. [m s-1].
    !*  TSPCP: Snowfall temperature over modelled area. [degrees C].
    !*  CSZ: Cosine of solar zenith angle. [--].
    !*  RADJ: Latitude of grid cell (positive north of equator). [rad].
    type RUNLAKE_driving_variables
        real, dimension(:), allocatable :: &
            QSWINV, QSWINI, QLWIN, PRES, ZREFH, QA, TA, ZREFM, UWIND, VWIND, &
            RHOAIR, TADP, PADRY, VPD, RHOSNI, &
            RPRE, SPRE, PCPR, RPCP, TRPCP, SPCP, TSPCP, &
            CSZ, RADJ
    end type

!    integer :: NMW

    !> Lake tile parameters.
    character(len = 10) LAKEOUTDIR

    !*  CDH: Surface drag coefficient for heat. [ ].
    !*  CDM: Surface drag coefficient for momentum. [ ].
    !*  FLGL: Net LW radiation at lake surface. [W m-2].
    !*  FSGL: Net SW radiation at lake surface. [W m-2].
    !*  HEVL: Surface latent heat flux. [W m-2].
    !*  HFSL: Surface sensible heat flux. [W m-2].
    !*  HMFL: Energy associated with lake ice freeze/melt. [W m-2].
    real, dimension(:), allocatable :: &
        ZDH, ZDM, CDH, CDM, ASVL, ASIL, BCSNL, REFL

    !> Lake prognostic variables.
    !*  EXPW: Volume expansivity of water. [K-1].
    !*  DTEMP: Temperature jump across bottom of mixed layer. [K].
    !*  HDPTH: Depth of mixed layer. [m].
    !*  DELU: Current jump across bottom of mixed layer. [m s-1].
    !*  GRED: Reduced gravity across bottom of mixed layer. [m s-2].
    !*  TKE: Mean mixed layer TKE per unit mass. [m2 s-2].
    !*  T0: Lake surface temperature. [K].
    !*  LKICEH: Mean ice thickness. [m].
    !*  RHOMIX: Mean density of mixed layer. [kg m-3].
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

    type(RUNLAKE_driving_variables), save :: lfv
    type(LAKE_pronostic_variables), save :: lpv, lpvi, lpvg
    type(LAKE_diagnostic_variables), save :: ldv, ldvi, ldvg

    !*  NTILE: Total number of lake tiles. [--].
    type RUNLAKE_container
        type(RUNLAKE_option_flags) :: op
        type(RUNLAKE_parameters) :: pm, pm_nlak
        integer :: NTILE = 0
        logical :: PROCESS_ACTIVE = .false.
    end type

    type(RUNLAKE_container), save :: lm

    contains

    subroutine RUNLAKE_parameters_allocate(pm, n, ierr)

        implicit none

        type(RUNLAKE_parameters) pm
        integer, intent(in) :: n
        integer, intent(out) :: ierr

        allocate(pm%HLAK(n), pm%LLAK(n), pm%BLAK(n), pm%NLAK(n))
        pm%HLAK = 0.0; pm%LLAK = 0.0; pm%BLAK = 0.0; pm%NLAK = 0

    end subroutine

end module
