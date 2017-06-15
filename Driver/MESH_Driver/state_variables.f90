!>
!> Description:
!>  Contains variable types for states of variables in the model, such
!>  as components of the water and energy balances, streamflow channels,
!>  and reservoirs.
!>
module state_variables

    implicit none

    !* SAVE/RESUMESTATES: Saves or resume states from file.
    !>  Options:
    !>      - none: Save and resume no states to and from file (default).
    !>      - txt:  In text format.
    !>      - seq:  Sequential binary format.
    !>      - csv:  From CSV by GRU (RESUMESTATES only).
    !>      - r2c:  From r2c by grid (RESUMESTATES only).
    character(len = 80), save :: RESUMESTATES = 'RESUMESTATES none', SAVESTATES = 'SAVESTATES none'

    !> Type: flow_state
    !>  State of fluxes for flow.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  qi: Flow in to the element. [m3 s-1].
    !*  qo: Flow from the element. [m3 s-1].
    !*  s: Channel storage held in the element. [m3].
    type flow_state
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: qi, qo, s
    end type

    !> Type: river_flow (extends: flow_state)
    !>  State of fluxes for a river channel.
    !>
    !> Variables:
    type, extends(flow_state) :: river_flow

    end type

    !> Type: lake_flow (extends: flow_state)
    !>  State of fluxes for a water body.
    !>
    !> Variables:
    !*  ab: Volume abstracted at the end of the time-step. [m3].
    type, extends(flow_state) :: lake_flow
        real(kind = 4), dimension(:), allocatable :: ab
    end type

    !> Type: canopy
    !>  States of canopy.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  qac: Specific humidity of air within vegetation canopy space. [kg kg-1].
    !*  rcan: Intercepted liquid water stored on canopy. [kg m-2].
    !*  sncan: Intercepted frozen water stored on canopy. [kg m-2].
    !*  tac: Temperature of air within vegetation canopy. [K].
    !*  tcan: Vegetation canopy temperature. [K].
    !*  cmai: Aggregated mass of vegetation canopy. [kg m-2].
    !*  gro: Vegetation growth index.
    !*  pevp: Diagnosed potential evapotranspiration. [kg m-2 s-1].
    !*  evpb: Evaporation efficiency (EVP to PEVP) of the canopy. [--].
    !*  arrd: Arridity index (PRE to PEVP). [--].
    type canopy
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: qac, rcan, sncan, tac, tcan, cmai, gro, pevp, evpb, arrd
    end type

    !> Type: snow_balance
    !>  State of snow at the surface.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  sno: Mass of snow pack. [kg m-2].
    !*  albs: Snow albedo.
    !*  fsno: Diagnosed fractional snow coverage. [ ].
    !*  rhos: Density of snow. [kg m-3].
    !*  tsno: Snowpack temperature. [K].
    !*  wsno: Liquid water content of snow pack. [kg m-2].
    type snow_balance
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: sno, albs, fsno, rhos, tsno, wsno
    end type

    !> Type: surface_interface
    !>  States at the interface between the atmosphere and soil profile.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  tsfs: Ground surface temperature over subarea. [K].
    !*  tpnd: Temperature of ponded water. [K].
    !*  zpnd: Depth of ponded water on surface. [m].
    !*  pndw: Ponded water storage on the surface. [kg m-2].
    !*  evap: Evapotranspiration. [kg m-2].
    !*  rofo: Overland component of total runoff. [kg m-2].
    type surface_interface
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: tpnd, zpnd, pndw, evap, qevp, hfs, rofo
        real(kind = 4), dimension(:, :), allocatable :: tsfs
    end type

    !> Type: soil_layer
    !>  States of the soil profile.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  thic: Volumetric frozen water content of soil layers. [m3 m-3].
    !*  fzws: Frozen water storage. [kg m-2].
    !*  thlq: Volumetric liquid water content of soil layers. [m3 m-3].
    !*  lqws: Liquid water storage. [kg m-2].
    !*  tbar: Temperature of soil layers. [K].
    !*  tbas: Temperature of bedrock in third soil layer. [K].
    !*  delzw: Thickness of permeable part of soil layer. [m].
    !*  zbotw: Depth of bottom of permeable part of soil layer. [m].
    !*  rofs: Interflow component of total runoff. [kg m-2].
    !*  gflx: Heat conduction between soil layers. [W m-2].
    !*  ggeo: Geothermal heat flux. [W m-2].
    type soil_layer
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: tbas, ggeo, rofs
        real(kind = 4), dimension(:, :), allocatable :: thic, fzws, thlq, lqws, tbar, delzw, zbotw, gflx
    end type

    !> Type: deep_zone
    !>  States of deep zone storage.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  zlw: Depth of water. [m].
    !*  rofb: Baseflow component of total runoff. [kg m-2].
    type deep_zone
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: zlw, rofb
    end type

    !> Type: storage_state
    !>  Storage state at the beginning and end of the time-step.
    !>
    !> Indices:
    !*  n: Number of elements dimensioned.
    !>
    !> Variables:
    !*  s1: Storage at the beginning of the time-step.
    !*  s2: Storage at the end of the time-step.
    type storage_state
        integer(kind = 4) :: n
        real(kind = 4), dimension(:), allocatable :: s, ds
    end type

    !> Type: water_storage (extends: storage_state)
    !>  States of water held by different parts of the vertical profile.
    !>
    !> Inherited:
    !*  s1: Storage at the beginning of the time-step. [kg m-2].
    !*  s2: Storage at the end of the time-step. [kg m-2].
    !>
    !> Variables:
    !*  cnpy: Water held in the canopy. [kg m-2].
    !*  sfc: Water held at the surface. [kg m-2].
    !*  sl: Water held in soil. [kg m-2].
    !*  lz: Water held in an aquifer or lower zone storage. [kg m-2].
    !*  dz: Water held in deep zone storage. [kg m-2].
    !*  lost: Water lost. [kg m-2].
    type, extends(storage_state) :: water_storage
        real(kind = 4), dimension(:), allocatable :: cnpy, sfc, sl, lz, dz, lost
    end type

    !> Type: states
    !>
    !> Description:
    !>  Contains variable types for states of variables in the model,
    !>  such as components of the water and energy balances, streamflow
    !>  channels, and reservoirs.
    type states
        type(river_flow) :: chnl
        type(lake_flow) :: lk, rsvr
        type(canopy) :: cnpy
        type(snow_balance) :: sno
        type(surface_interface) :: sfc
        type(soil_layer) :: sl
        type(deep_zone) :: lzs, dzs
        logical :: inid = .false.
        character(len = 4) :: unit = ''
    end type

    !> State of SA_MESH variables in the current time-step.
    !*  stas: State of variables at the tile (NML, GAT) level.
    !*  stas_grid: State of variables at the grid (NA, NAA, GRD) level.
    !*  stas_gru: State of variables at the GRU (NTYPE, ROW) level.
    type(states), save :: stas, stas_grid, stas_gru

    contains

    !> Description: Subroutine to initialize (allocate and set to zero)
    !>  elements of an instance of the states type.
    subroutine stas_init(stas, stas_unit, n, nsl, ierr)

        !> Input variables.
        type(states) :: stas
        character(len = *), intent(in) :: stas_unit
        integer, intent(in) :: n, nsl

        !> Output variables.
        integer, intent(out), optional :: ierr

        !> Return if the instance is already initialized.
        if (stas%inid) return

        !> Allocate elements in the type.
        allocate( &

            !> Canopy.
            stas%cnpy%qac(n), stas%cnpy%rcan(n), stas%cnpy%sncan(n), stas%cnpy%tac(n), stas%cnpy%tcan(n), &
            stas%cnpy%cmai(n), stas%cnpy%gro(n), stas%cnpy%pevp(n), stas%cnpy%evpb(n), stas%cnpy%arrd(n), &

            !> Snow.
            stas%sno%sno(n), stas%sno%albs(n), stas%sno%fsno(n), stas%sno%rhos(n), stas%sno%tsno(n), stas%sno%wsno(n), &

            !> Surface or at near surface.
            stas%sfc%tpnd(n), stas%sfc%zpnd(n), stas%sfc%pndw(n), stas%sfc%evap(n), stas%sfc%qevp(n), &
            stas%sfc%hfs(n), stas%sfc%rofo(n), stas%sfc%tsfs(n, 4), &

            !> Soil layers.
            stas%sl%thic(n, nsl), stas%sl%fzws(n, nsl), stas%sl%thlq(n, nsl), stas%sl%lqws(n, nsl), &
            stas%sl%tbar(n, nsl), stas%sl%tbas(n), stas%sl%delzw(n, nsl), stas%sl%zbotw(n, nsl), stas%sl%rofs(n), &
            stas%sl%gflx(n, nsl), stas%sl%ggeo(n), &

            !> Lower zone storage.
            stas%lzs%zlw(n), stas%lzs%rofb(n), &

            !> Deep zone storage.
            stas%dzs%zlw(n), stas%dzs%rofb(n), &

            !> Stream channel.
            stas%chnl%qi(n), stas%chnl%qo(n), stas%chnl%s(n), &

            stat = ierr)

        !> Mark that the variable has been initialized.
        stas%inid = (ierr == 0)

        !> Save the scope of the state type.
        stas%unit = stas_unit

        !> Initialize elements in the type.
        if (stas%inid) then

            !> Canopy.
            stas%cnpy%qac = 0.0; stas%cnpy%rcan = 0.0; stas%cnpy%sncan = 0.0; stas%cnpy%tac = 0.0; stas%cnpy%tcan = 0.0
            stas%cnpy%cmai = 0.0; stas%cnpy%gro = 0.0; stas%cnpy%pevp = 0.0; stas%cnpy%evpb = 0.0; stas%cnpy%arrd = 0.0

            !> Snow.
            stas%sno%sno = 0.0; stas%sno%albs = 0.0; stas%sno%fsno = 0.0; stas%sno%rhos = 0.0; stas%sno%tsno = 0.0
            stas%sno%wsno = 0.0

            !> Surface or at near surface.
            stas%sfc%tpnd = 0.0; stas%sfc%zpnd = 0.0; stas%sfc%pndw = 0.0; stas%sfc%evap = 0.0; stas%sfc%qevp = 0.0
            stas%sfc%hfs = 0.0; stas%sfc%rofo = 0.0; stas%sfc%tsfs = 0.0

            !> Soil layers.
            stas%sl%thic = 0.0; stas%sl%fzws = 0.0; stas%sl%thlq = 0.0; stas%sl%lqws = 0.0
            stas%sl%tbar = 0.0; stas%sl%tbas = 0.0; stas%sl%delzw = 0.0; stas%sl%zbotw = 0.0; stas%sl%rofs = 0.0
            stas%sl%gflx = 0.0; stas%sl%ggeo = 0.0

            !> Lower zone storage.
            stas%lzs%zlw = 0.0; stas%lzs%rofb = 0.0

            !> Deep zone storage.
            stas%dzs%zlw = 0.0; stas%dzs%rofb = 0.0

            !> Stream channel.
            stas%chnl%qi = 0.0; stas%chnl%qo = 0.0; stas%chnl%s = 0.0

        end if

    end subroutine

end module
