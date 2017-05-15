!>
!> Description:
!>  Contains variable types for common parameters of the model, including
!>  parameters for the river channel routing and land surface schemes.
!>
!> Instances of these types are accesible by the
!> 'sa_mesh_shared_parameters' module.
!>
module input_parameters

    !> Type: Tile parameters.
    !>  Physiographic parameters of the file.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  gc: Ground cover type. [--].
    !*  fare: Active fraction of the grid cell. [--].
    !*  xslp: Estimated average slope of the GRU. [--].
    !*  mid: Mosaic type of the tile. [--].
    type tile_parameters
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: gc, fare, xslp
        integer(kind = 4), dimension(:), allocatable :: mid
    end type

    !> Type: Canopy parameters.
    !>  Parameters of the vegetation canopy.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
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
        integer(kind = 4) :: n
        real(kind = 4), dimension(:, :), allocatable :: fcan, z0or, lnz0, alvc, alic
        real(kind = 4), dimension(:, :), allocatable :: lamx, lamn, cmas, root, rsmn, qa50, vpda, vpdb, psga, psgb
    end type

    !> Type: Surface parameters.
    !>  Parameters for the interface between the soil column and canopy or atmosphere.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  zbld: Height into the atmosphere for aggregating surface roughness (usually in the order of 50-100 m). [m].
    !*  zrfh: Reference height (measurement height) for temperature and humidity. [m].
    !*  zrfm: Reference height (measurement height) for wind speed. [m].
    !*  zplg: Maximum depth of liquid water allowed to be stored on the ground surface for snow-free areas. [m].
    type surface_parameters
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: zbld, zrfh, zrfm, zplg
    end type

    !> Type: Snow parameters.
    !>  Parameters of the snow pack at the surface.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  zsnl: Minimum depth to consider 100% cover of snow on the ground surface. [m].
    !*  zpls: Maximum depth of liquid water allowed to be stored on the ground surface for snow-covered areas. [m].
    type snow_parameters
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: zsnl, zpls
    end type

    !> Type: Soil parameters.
    !>  Parameters of soil layers in the column.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
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
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  drn: Drainage index, set to 1.0 to allow the soil physics to model drainage or to a value between 0.0 and 1.0 to impede drainage. [--].
    !*  dd: Estimated drainage density of the GRU. [km km-2].
    !*  grkf: Fraction of the saturated surface soil conductivity moving in the horizontal direction. [--].
    !*  mann: Manning's n. [s m-1/3].
    !*  ks: Saturated surface soil conductivity. [m s-1].
    type hydraulic_parameters
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: drn, dd, grkf, mann, ks
    end type

end module
