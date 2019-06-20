!>
!> Description:
!>  Contains variable types for common parameters of the model, including
!>  parameters for the river channel routing and land surface schemes.
!>
module input_parameters

    implicit none

    !* INPUTPARAMSFORM: Determines how parameters are read from file.
    !>  Options:
    !>      - ini:  From CLASS.ini and Hydrology.ini (default).
    !>      - csv:  From CSV by GRU.
    !>      - r2c:  From r2c by grid.
    character(len = 80), save :: INPUTPARAMSFORM = ''
    integer, save :: INPUTPARAMSFORMFLAG = 1

    !> Type: Tile parameters.
    !>  Physiographic parameters of the file.
    !>
    !> Variables:
    !*  gc: Ground cover type. [--].
    !*  fare: Active fraction of the grid cell. [--].
    !*  xslp: Estimated average slope of the GRU. [--].
    !*  mid: Mosaic type of the tile. [--].
    type tile_parameters
        real(kind = 4), dimension(:), allocatable :: gc, fare, xslp
        integer(kind = 4), dimension(:), allocatable :: mid, iwf
    end type

    !> Type: Canopy parameters.
    !>  Parameters of the vegetation canopy.
    !>
    !> Variables:
    !*  fcan: Annual maximum fraction of the grid-cell occupied by vegetation category or land cover. [--].
    !*  z0or: Orographic roughness length. [m].
    !*  lnz0: Natural logarithm of the roughness length of the vegetation category or land cover. [--].
    !*  alvc: Average visible albedo of the vegetation category when fully-leafed or of the land cover. [--].
    !*  alic: Average near-infrared albedo of the vegetation category when fully-leafed or of the land cover. [--].
    !*  lamx: Annual maximum leaf-area index of the vegetation category. [--].
    !*  lamn: Annual minimum leaf-area index of the vegetation category. [--].
    !*  cmas: Annual maximum canopy mass of the vegetation category. [kg m-2].
    !*  root: Annual maximum rooting depth of the vegetation category. [m].
    !*  rsmn: Minimum stomatal resistance of the vegetation category. [s m-1].
    !*  qa50: Reference value of shortwave radiation used in the calculation of the stomatal resistance of the vegetation category. [W m-2].
    !*  vpda: Vapor pressure deficit coefficient 'A' used in the calculation of the stomatal resistance of the vegetation category. [--].
    !*  vpdb: Vapor pressure deficit coefficient 'B' used in the calculation of the stomatal resistance of the vegetation category. [--].
    !*  psga: Soil moisture suction coefficient 'A' used in the calculation of the stomatal resistance of the vegetation category. [--].
    !*  psgb: Soil moisture suction coefficient 'B' used in the calculation of the stomatal resistance of the vegetation category. [--].
    type canopy_parameters
        real(kind = 4), dimension(:, :), allocatable :: fcan, z0or, lnz0, alvc, alic
        real(kind = 4), dimension(:, :), allocatable :: lamx, lamn, cmas, root, rsmn, qa50, vpda, vpdb, psga, psgb
    end type

    !> Type: Surface parameters.
    !>  Parameters for the interface between the soil column and canopy or atmosphere.
    !>
    !> Variables:
    !*  zbld: Height into the atmosphere for aggregating surface roughness (usually in the order of 50-100 m). [m].
    !*  zrfh: Reference height (measurement height) for temperature and humidity. [m].
    !*  zrfm: Reference height (measurement height) for wind speed. [m].
    !*  zplg: Maximum depth of liquid water allowed to be stored on the ground surface for snow-free areas. [m].
    type surface_parameters
        real(kind = 4), dimension(:), allocatable :: zbld, zrfh, zrfm, zplg
    end type

    !> Type: Snow parameters.
    !>  Parameters of the snow pack at the surface.
    !>
    !> Variables:
    !*  zsnl: Minimum depth to consider 100% cover of snow on the ground surface. [m].
    !*  zpls: Maximum depth of liquid water allowed to be stored on the ground surface for snow-covered areas. [m].
    type snow_parameters
        real(kind = 4), dimension(:), allocatable :: zsnl, zpls
    end type

    !> Type: Soil parameters.
    !>  Parameters of soil layers in the column.
    !>
    !> Variables:
    !*  sand: Percent content of sand in the mineral soil. [%].
    !*  clay: Percent content of clay in the mineral soil. [%].
    !*  orgm: Percent content of organic matter in the mineral soil. [%].
    !*  sdep: Permeable depth of the soil column. [m].
    !*  delz: Layer thickness. [m].
    !*  zbot: Depth to bottom of the column. [m].
    !*  alwet: Reference all-wave albedo of wet soil for modelled area. [--].
    !*  aldry: Reference all-wave albedo of dry soil for modelled area. [--].
    !*  thpor: Pore volume. [m3 m-3].
    !*  thlret: Liquid water retention capacity for organic soil. [m3 m-3].
    !*  thlmin: Residual soil liquid water content remaining after freezing or evaporation. [m3 m-3].
    !*  thlrat: Fractional saturation of soil at half the saturated hydraulic conductivity. [--].
    !*  bi: Clapp and Hornberger empirical parameter. [--].
    !*  psisat: Soil moisture suction at saturation. [m].
    !*  psiwlt: Soil moisture suction at wilting point. [m].
    !*  grksat: Hydraulic conductivity of soil at saturation. [m s-1].
    !*  thfc: Field capacity. [m3 m-3].
    !*  hcps: Volumetric heat capacity of soil matter. [J m-3 K-1].
    !*  tcs: Thermal conductivity of soil. [W m-1 K-1].
    type soil_parameters
        real(kind = 4), dimension(:), allocatable :: sdep, alwet, aldry
        real(kind = 4), dimension(:), allocatable :: delz, zbot
        real(kind = 4), dimension(:, :), allocatable :: sand, clay, orgm, &
            thpor, thlret, thlmin, thlrat, bi, psisat, psiwlt, grksat, thfc, hcps, tcs
    end type

    !> Type: Hydraulic parameters.
    !>  Parameters for hydraulic processes.
    !>
    !> Variables:
    !*  drn: Drainage index, set to 1.0 to allow the soil physics to model drainage or to a value between 0.0 and 1.0 to impede drainage. [--].
    !*  dd: Estimated drainage density of the GRU. [km km-2].
    !*  grkf: Fraction of the saturated surface soil conductivity moving in the horizontal direction. [--].
    !*  mann: Manning's n. [s m-1/3].
    !*  ks: Saturated surface soil conductivity. [m s-1].
    type hydraulic_parameters
        real(kind = 4), dimension(:), allocatable :: drn, dd, grkf, mann, ks
    end type

    !> Type: parameters
    !>
    !> Description:
    !>  Collection of input parameters.
    !> Variables:
    !*  unit: Scale of the parameterization (e.g., 'tile', 'grid', 'gru').
    !*  inid: Should be .true. if arrays have been initialized.
    type parameters
        type(tile_parameters) :: tp
        type(canopy_parameters) :: cp
        type(surface_parameters) :: sfp
        type(snow_parameters) :: snp
        type(soil_parameters) :: slp
        type(hydraulic_parameters) :: hp
        logical :: inid = .false.
        character(len = 4) :: unit = ''
    end type

    !> SA_MESH shared input parameters.
    !*  pm: Input parameters at the tile (NML, GAT) level.
    !*  pm_grid: Input parameters at the grid (NA, NAA, GRD) level.
    !*  pm_gru: Input parameters at the GRU (NTYPE, ROW) level.
    type(parameters), save :: pm, pm_grid, pm_gru

    contains

    !> Description: Subroutine to initialize (allocate and set to zero)
    !>  elements of an instance of the parameters type.
    subroutine pm_init(pm, pm_unit, n, nsl, ncan, ncan1, ierr)

        !> Input variables.
        type(parameters) :: pm
        character(len = *), intent(in) :: pm_unit
        integer, intent(in) :: n, nsl, ncan, ncan1

        !> Output variables.
        integer, intent(out), optional :: ierr

        !> Initialize the return status.
        ierr = 0

        !> Return if the instance is already initialized.
        if (pm%inid) return

        !> Allocate elements in the parameter type.
        allocate(pm%tp%gc(n), pm%tp%fare(n), pm%tp%xslp(n), pm%tp%mid(n), pm%tp%iwf(n), &
                 pm%cp%fcan(n, ncan1), pm%cp%z0or(n, ncan1), pm%cp%lnz0(n, ncan1), pm%cp%alvc(n, ncan1), pm%cp%alic(n, ncan1), &
                 pm%cp%lamx(n, ncan), pm%cp%lamn(n, ncan), pm%cp%cmas(n, ncan), pm%cp%root(n, ncan), pm%cp%rsmn(n, ncan), &
                 pm%cp%qa50(n, ncan), pm%cp%vpda(n, ncan), pm%cp%vpdb(n, ncan), pm%cp%psga(n, ncan), pm%cp%psgb(n, ncan), &
                 pm%sfp%zbld(n), pm%sfp%zrfh(n), pm%sfp%zrfm(n), pm%sfp%zplg(n), pm%snp%zsnl(n), pm%snp%zpls(n), &
                 pm%slp%sdep(n), pm%slp%alwet(n), pm%slp%aldry(n), &
                 pm%slp%delz(nsl), pm%slp%zbot(nsl), &
                 pm%slp%sand(n, nsl), pm%slp%clay(n, nsl), pm%slp%orgm(n, nsl), &
                 pm%slp%thpor(n, nsl), pm%slp%thlret(n, nsl), pm%slp%thlmin(n, nsl), pm%slp%thlrat(n, nsl), &
                 pm%slp%bi(n, nsl), pm%slp%psisat(n, nsl), pm%slp%psiwlt(n, nsl), pm%slp%grksat(n, nsl), &
                 pm%slp%thfc(n, nsl), pm%slp%hcps(n, nsl), pm%slp%tcs(n, nsl), &
                 pm%hp%drn(n), pm%hp%dd(n), pm%hp%grkf(n), pm%hp%mann(n), pm%hp%ks(n), stat = ierr)
        pm%inid = (ierr == 0)
        pm%unit = pm_unit

        !> Initialize elements in the parameter type.
        if (pm%inid) then
            pm%tp%gc = 0.0; pm%tp%fare = 0.0; pm%tp%xslp = 0.0; pm%tp%mid = 0; pm%tp%iwf = -1
            pm%cp%fcan = 0.0; pm%cp%z0or = 0.0; pm%cp%lnz0 = 0.0; pm%cp%alvc = 0.0; pm%cp%alic = 0.0
            pm%cp%lamx = 0.0; pm%cp%lamn = 0.0; pm%cp%cmas = 0.0; pm%cp%root = 0.0; pm%cp%rsmn = 0.0
            pm%cp%qa50 = 0.0; pm%cp%vpda = 0.0; pm%cp%vpdb = 0.0; pm%cp%psga = 0.0; pm%cp%psgb = 0.0
            pm%sfp%zbld = 0.0; pm%sfp%zrfh = 0.0; pm%sfp%zrfm = 0.0; pm%sfp%zplg = 0.0; pm%snp%zsnl = 0.0; pm%snp%zpls = 0.0
            pm%slp%sdep = 0.0; pm%slp%alwet = 0.0; pm%slp%aldry = 0.0
            pm%slp%delz = 0.0; pm%slp%zbot = 0.0
            pm%slp%sand = 0.0; pm%slp%clay = 0.0; pm%slp%orgm = 0.0
            pm%slp%thpor = 0.0; pm%slp%thlret = 0.0; pm%slp%thlmin = 0.0; pm%slp%thlrat = 0.0
            pm%slp%bi = 0.0; pm%slp%psisat = 0.0; pm%slp%psiwlt = 0.0; pm%slp%grksat = 0.0
            pm%slp%thfc = 0.0; pm%slp%hcps = 0.0; pm%slp%tcs = 0.0
            pm%hp%drn = 0.0; pm%hp%dd = 0.0; pm%hp%grkf = 0.0; pm%hp%mann = 0.0; pm%hp%ks = 0.0
        end if

    end subroutine

end module
