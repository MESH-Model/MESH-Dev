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
    character(len = 80), save :: RESUMESTATES = 'RESUMESTATES none'
    character(len = 80), save :: SAVESTATES = 'SAVESTATES none'

    !> Type: channel
    !>  States of channels and flow.
    !>
    !> Variables:
    !*  rff: Contributing runoff. [kg m-2].
    !*  rchg: Contributing recharge. [kg m-2].
    !*  qi: Flow in to the element. [m3 s-1].
    !*  qo: Flow from the element. [m3 s-1].
    !*  stg: Channel storage held in the element. [m3].
    !*  zlvl: Stage level from the element. [m].
    !*  div: Volume diverted to the channel. [m3].
    !*  ab: Volume abstracted from the channel. [m3].
    type channel
        real(kind = 4), dimension(:), allocatable :: &
            rff, rchg, &
            qi, qo, stg, zlvl, &
            div, ab
    end type

    !> Type: canopy
    !>  States of canopy.
    !>
    !> Variables:
    !*  rcan: Intercepted liquid water stored on canopy. [kg m-2].
    !*  sncan: Intercepted frozen water stored on canopy. [kg m-2].
    !*  cmas: Aggregated mass of vegetation canopy. [kg m-2].
    !*  tac: Temperature of air within vegetation canopy. [K].
    !*  tcan: Vegetation canopy temperature. [K].
    !*  qac: Specific humidity of air within vegetation canopy space. [kg kg-1].
    !*  gro: Vegetation growth index.
    type canopy
        real(kind = 4), dimension(:), allocatable :: &
            rcan, sncan, &
            cmas, tac, tcan, qac, gro
    end type

    !> Type: snow_balance
    !>  State of snow at the surface.
    !>
    !> Variables:
    !*  zsno: Snow depth. [m].
    !*  sno: Mass of snow pack. [kg m-2].
    !*  fsno: Diagnosed fractional snow coverage. [ ].
    !*  albs: Snow albedo.
    !*  rhos: Density of snow. [kg m-3].
    !*  wsno: Liquid water content of snow pack. [kg m-2].
    !*  tsno: Snowpack temperature. [K].
    type snow_balance
        real(kind = 4), dimension(:), allocatable :: &
            zsno, &
            sno, fsno, albs, rhos, wsno, &
            tsno
    end type

    !> Type: surface_interface
    !>  States at the interface between the atmosphere and soil profile.
    !>
    !> Variables:
    !*  albt: Total albedo of the surface (visible and near-infrared). [--].
    !*  alvs: Visible albedo of the surface. [--].
    !*  alir: Near-infrared albedo of the surface. [--].
    !*  gte: Effective black-body temperature at the surface. [K].
    !*  zpnd: Depth of ponded water on surface. [m].
    !*  pndw: Ponded water storage on the surface. [kg m-2].
    !*  tpnd: Temperature of ponded water. [K].
    !*  pevp: Diagnosed potential evapotranspiration. [kg m-2 s-1].
    !*  evap: Evapotranspiration. [kg m-2].
    !*  evpb: Evaporation efficiency (EVP to PEVP) of the canopy. [--].
    !*  arrd: Arridity index (PRE to PEVP). [--].
    !*  rofo: Overland component of total runoff. [kg m-2 s-1].
    !*  qevp: Latent heat flux at the surface. [W m-2].
    !*  hfs: Sensible heat flux at the surface. [W m-2].
    !*  gzero: Heat flux into the soil at the surface. [W m-2].
    !*  tsfs: Ground surface temperature over subarea. [K].
    type surface_interface
        real(kind = 4), dimension(:), allocatable :: &
            albt, alvs, alir, gte, &
            zpnd, pndw, tpnd, &
            pevp, evap, evpb, arrd, &
            rofo, &
            qevp, hfs, gzero
        real(kind = 4), dimension(:, :), allocatable :: tsfs
    end type

    !> Type: soil_layer
    !>  States of the soil profile.
    !>
    !> Variables:
    !*  ggeo: Geothermal heat flux. [W m-2].
    !*  rofs: Interflow component of total runoff. [kg m-2 s-1].
    !*  tbas: Temperature of bedrock in third soil layer. [K].
    !*  delzw: Thickness of permeable part of soil layer. [m].
    !*  zbotw: Depth of bottom of permeable part of soil layer. [m].
    !*  thic: Volumetric frozen water content of soil layers. [m3 m-3].
    !*  fzws: Frozen water storage. [kg m-2].
    !*  thlq: Volumetric liquid water content of soil layers. [m3 m-3].
    !*  lqws: Liquid water storage. [kg m-2].
    !*  tbar: Temperature of soil layers. [K].
    !*  gflx: Heat conduction between soil layers. [W m-2].
    type soil_layer
        real(kind = 4), dimension(:), allocatable :: ggeo, rofs, tbas
        real(kind = 4), dimension(:, :), allocatable :: &
            delzw, zbotw, &
            thic, fzws, thlq, lqws, &
            tbar, gflx
    end type

    !> Type: deep_zone
    !>  States of deep zone storage.
    !>
    !> Variables:
    !*  ws: Liquid water storage. [kg m-2].
    !*  rofb: Baseflow component of total runoff. [kg m-2 s-1].
    type deep_zone
        real(kind = 4), dimension(:), allocatable :: ws, rofb
    end type

    !> Type: storage_state
    !>  Storage state at the beginning and end of the time-step.
    !>
    !> Variables:
    !*  s1: Storage at the beginning of the time-step.
    !*  s2: Storage at the end of the time-step.
    type storage_state
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

    !> Type: tile_states
    !>
    !> Description:
    !>  Contains variable types for states of variables in the model,
    !>  such as components of the water and energy balances.
    type tile_states
        type(canopy) :: cnpy
        type(snow_balance) :: sno
        type(surface_interface) :: sfc
        type(soil_layer) :: sl
        type(deep_zone) :: lzs, dzs
        logical :: inid = .false.
    end type

    !> Type: grid_states
    !>
    !> Description:
    !>  Contains variable types for states of variables in the model,
    !>  such as components of the water and energy balances, and
    !>  grid-based channels.
    type grid_states
        type(canopy) :: cnpy
        type(snow_balance) :: sno
        type(surface_interface) :: sfc
        type(soil_layer) :: sl
        type(deep_zone) :: lzs, dzs
        type(channel) :: chnl
        logical :: inid = .false.
    end type

    !> Type: structure_states
    !>
    !> Description:
    !>  Contains variable types for states of variables in the model,
    !>  at such structures as gauge locations, lakes, and reservoirs.
    type structure_states
        type(channel) :: stmg, lk, rsvr
        logical :: inid = .false.
    end type

    !> State of SA_MESH variables in the current time-step.
    !*  stas: State of variables at the tile (NML, GAT) level.
    !*  stas_gru: State of variables at the GRU (NTYPE, ROW) level.
    !*  stas_grid: State of grid-based variables at the grid (NA, NAA, GRD) level.
    !*  stas_fms: States of structures.
    type(tile_states), save :: stas, stas_gru
    type(grid_states), save :: stas_grid
    type(structure_states), save :: stas_fms

    contains

    !> Description: Subroutine to initialize (allocate and set to zero)
    !>  elements of an instance of the tile_states type.
    subroutine stas_tile_init(stas, n, nsl, ierr)

        !> Input variables.
        type(tile_states) :: stas
        integer, intent(in) :: n, nsl

        !> Output variables.
        integer, intent(out), optional :: ierr

        !> Return if the instance is already initialized.
        if (stas%inid) return

        !> Allocate elements in the type.
        allocate( &

            !> Canopy.
            stas%cnpy%rcan(n), stas%cnpy%sncan(n), &
            stas%cnpy%cmas(n), stas%cnpy%tac(n), stas%cnpy%tcan(n), stas%cnpy%qac(n), stas%cnpy%gro(n), &

            !> Snow.
            stas%sno%zsno(n), &
            stas%sno%sno(n), stas%sno%albs(n), stas%sno%fsno(n), stas%sno%rhos(n), stas%sno%wsno(n), &
            stas%sno%tsno(n), &

            !> Surface or at near surface.
            stas%sfc%albt(n), stas%sfc%alvs(n), stas%sfc%alir(n), stas%sfc%gte(n), &
            stas%sfc%zpnd(n), stas%sfc%pndw(n), stas%sfc%tpnd(n), &
            stas%sfc%pevp(n), stas%sfc%evap(n), stas%sfc%evpb(n), stas%sfc%arrd(n), &
            stas%sfc%rofo(n), &
            stas%sfc%qevp(n), stas%sfc%hfs(n), stas%sfc%gzero(n), &
            stas%sfc%tsfs(n, 4), &

            !> Soil layers.
            stas%sl%ggeo(n), stas%sl%rofs(n), stas%sl%tbas(n), &
            stas%sl%delzw(n, nsl), stas%sl%zbotw(n, nsl), &
            stas%sl%thic(n, nsl), stas%sl%fzws(n, nsl), stas%sl%thlq(n, nsl), stas%sl%lqws(n, nsl), &
            stas%sl%tbar(n, nsl), stas%sl%gflx(n, nsl), &

            !> Lower zone storage.
            stas%lzs%ws(n), stas%lzs%rofb(n), &

            !> Deep zone storage.
            stas%dzs%ws(n), stas%dzs%rofb(n), &
            stat = ierr)

        !> Mark that the variable has been initialized.
        stas%inid = (ierr == 0)

        !> Initialize elements in the type.
        if (stas%inid) then

            !> Canopy.
            stas%cnpy%rcan = 0.0; stas%cnpy%sncan = 0.0
            stas%cnpy%cmas = 0.0; stas%cnpy%tac = 0.0; stas%cnpy%tcan = 0.0; stas%cnpy%qac = 0.0; stas%cnpy%gro = 0.0

            !> Snow.
            stas%sno%zsno = 0.0
            stas%sno%sno = 0.0; stas%sno%albs = 0.0; stas%sno%fsno = 0.0; stas%sno%rhos = 0.0; stas%sno%wsno = 0.0
            stas%sno%tsno = 0.0

            !> Surface or at near surface.
            stas%sfc%albt = 0.0; stas%sfc%alvs = 0.0; stas%sfc%alir = 0.0; stas%sfc%gte = 0.0
            stas%sfc%zpnd = 0.0; stas%sfc%pndw = 0.0; stas%sfc%tpnd = 0.0
            stas%sfc%pevp = 0.0; stas%sfc%evap = 0.0; stas%sfc%evpb = 0.0; stas%sfc%arrd = 0.0
            stas%sfc%rofo = 0.0
            stas%sfc%qevp = 0.0; stas%sfc%hfs = 0.0; stas%sfc%gzero = 0.0
            stas%sfc%tsfs = 0.0

            !> Soil layers.
            stas%sl%ggeo = 0.0; stas%sl%rofs = 0.0; stas%sl%tbas = 0.0
            stas%sl%delzw = 0.0; stas%sl%zbotw = 0.0
            stas%sl%thic = 0.0; stas%sl%fzws = 0.0; stas%sl%thlq = 0.0; stas%sl%lqws = 0.0
            stas%sl%tbar = 0.0; stas%sl%gflx = 0.0

            !> Lower zone storage.
            stas%lzs%ws = 0.0; stas%lzs%rofb = 0.0

            !> Deep zone storage.
            stas%dzs%ws = 0.0; stas%dzs%rofb = 0.0

        end if

    end subroutine

    !> Description: Subroutine to initialize (allocate and set to zero)
    !>  elements of an instance of the grid_states type.
    subroutine stas_grid_init(stas, n, nsl, ierr)

        !> Input variables.
        type(grid_states) :: stas
        integer, intent(in) :: n, nsl

        !> Output variables.
        integer, intent(out), optional :: ierr

        !> Return if the instance is already initialized.
        if (stas%inid) return

        !> Allocate elements in the type.
        allocate( &

            !> Canopy.
            stas%cnpy%rcan(n), stas%cnpy%sncan(n), &
            stas%cnpy%cmas(n), stas%cnpy%tac(n), stas%cnpy%tcan(n), stas%cnpy%qac(n), stas%cnpy%gro(n), &

            !> Snow.
            stas%sno%zsno(n), &
            stas%sno%sno(n), stas%sno%albs(n), stas%sno%fsno(n), stas%sno%rhos(n), stas%sno%wsno(n), &
            stas%sno%tsno(n), &

            !> Surface or at near surface.
            stas%sfc%albt(n), stas%sfc%alvs(n), stas%sfc%alir(n), stas%sfc%gte(n), &
            stas%sfc%zpnd(n), stas%sfc%pndw(n), stas%sfc%tpnd(n), &
            stas%sfc%pevp(n), stas%sfc%evap(n), stas%sfc%evpb(n), stas%sfc%arrd(n), &
            stas%sfc%rofo(n), &
            stas%sfc%qevp(n), stas%sfc%hfs(n), stas%sfc%gzero(n), &
            stas%sfc%tsfs(n, 4), &

            !> Soil layers.
            stas%sl%ggeo(n), stas%sl%rofs(n), stas%sl%tbas(n), &
            stas%sl%delzw(n, nsl), stas%sl%zbotw(n, nsl), &
            stas%sl%thic(n, nsl), stas%sl%fzws(n, nsl), stas%sl%thlq(n, nsl), stas%sl%lqws(n, nsl), &
            stas%sl%tbar(n, nsl), stas%sl%gflx(n, nsl), &

            !> Lower zone storage.
            stas%lzs%ws(n), stas%lzs%rofb(n), &

            !> Deep zone storage.
            stas%dzs%ws(n), stas%dzs%rofb(n), &

            !> Stream channel.
            stas%chnl%rff(n), stas%chnl%rchg(n), &
            stas%chnl%qi(n), stas%chnl%qo(n), stas%chnl%stg(n), stas%chnl%zlvl(n), &
            stas%chnl%div(n), stas%chnl%ab(n), &
            stat = ierr)

        !> Mark that the variable has been initialized.
        stas%inid = (ierr == 0)

        !> Initialize elements in the type.
        if (stas%inid) then

            !> Canopy.
            stas%cnpy%rcan = 0.0; stas%cnpy%sncan = 0.0
            stas%cnpy%cmas = 0.0; stas%cnpy%tac = 0.0; stas%cnpy%tcan = 0.0; stas%cnpy%qac = 0.0; stas%cnpy%gro = 0.0

            !> Snow.
            stas%sno%zsno = 0.0
            stas%sno%sno = 0.0; stas%sno%albs = 0.0; stas%sno%fsno = 0.0; stas%sno%rhos = 0.0; stas%sno%wsno = 0.0
            stas%sno%tsno = 0.0

            !> Surface or at near surface.
            stas%sfc%albt = 0.0; stas%sfc%alvs = 0.0; stas%sfc%alir = 0.0; stas%sfc%gte = 0.0
            stas%sfc%zpnd = 0.0; stas%sfc%pndw = 0.0; stas%sfc%tpnd = 0.0
            stas%sfc%pevp = 0.0; stas%sfc%evap = 0.0; stas%sfc%evpb = 0.0; stas%sfc%arrd = 0.0
            stas%sfc%rofo = 0.0
            stas%sfc%qevp = 0.0; stas%sfc%hfs = 0.0; stas%sfc%gzero = 0.0
            stas%sfc%tsfs = 0.0

            !> Soil layers.
            stas%sl%ggeo = 0.0; stas%sl%rofs = 0.0; stas%sl%tbas = 0.0
            stas%sl%delzw = 0.0; stas%sl%zbotw = 0.0
            stas%sl%thic = 0.0; stas%sl%fzws = 0.0; stas%sl%thlq = 0.0; stas%sl%lqws = 0.0
            stas%sl%tbar = 0.0; stas%sl%gflx = 0.0

            !> Lower zone storage.
            stas%lzs%ws = 0.0; stas%lzs%rofb = 0.0

            !> Deep zone storage.
            stas%dzs%ws = 0.0; stas%dzs%rofb = 0.0

            !> Stream channel.
            stas%chnl%rff = 0.0; stas%chnl%rchg = 0.0
            stas%chnl%qi = 0.0; stas%chnl%qo = 0.0; stas%chnl%stg = 0.0; stas%chnl%zlvl = 0.0
            stas%chnl%div = 0.0; stas%chnl%ab = 0.0

        end if

    end subroutine

    !> Description: Subroutine to initialize (allocate and set to zero)
    !>  elements of an instance of the structure_states type.
    subroutine stas_fms_init(stas, nstmg, nlk, nrsvr, ierr)

        !> Input variables.
        type(structure_states) :: stas
        integer, intent(in) :: nstmg, nlk, nrsvr

        !> Output variables.
        integer, intent(out), optional :: ierr

        !> Return if the instance is already initialized.
        if (stas%inid) return

        !> Allocate elements in the type.
        ierr = 0

        !> Streamflow gauge locations.
        if (ierr == 0 .and. nstmg > 0) then
            allocate( &
                stas%stmg%rff(nstmg), stas%stmg%rchg(nstmg), &
                stas%stmg%qi(nstmg), stas%stmg%qo(nstmg), stas%stmg%stg(nstmg), stas%stmg%zlvl(nstmg), &
                stas%stmg%div(nstmg), stas%stmg%ab(nstmg), &
                stat = ierr)
            if (ierr == 0) then
                stas%stmg%rff = 0.0; stas%stmg%rchg = 0.0
                stas%stmg%qi = 0.0; stas%stmg%qo = 0.0; stas%stmg%stg = 0.0; stas%stmg%zlvl = 0.0
                stas%stmg%div = 0.0; stas%stmg%ab = 0.0
            end if
        end if

        !> Lakes.
        if (ierr == 0 .and. nlk > 0) then
            allocate( &
                stas%lk%rff(nlk), stas%lk%rchg(nlk), &
                stas%lk%qi(nlk), stas%lk%qo(nlk), stas%lk%stg(nlk), stas%lk%zlvl(nlk), &
                stas%lk%div(nlk), stas%lk%ab(nlk), &
                stat = ierr)
            if (ierr == 0) then
                stas%lk%rff = 0.0; stas%lk%rchg = 0.0
                stas%lk%qi = 0.0; stas%lk%qo = 0.0; stas%lk%stg = 0.0; stas%lk%zlvl = 0.0
                stas%lk%div = 0.0; stas%lk%ab = 0.0
            end if
        end if

        !> Reservoirs.
        if (ierr == 0 .and. nrsvr > 0) then
            allocate( &
                stas%rsvr%rff(nrsvr), stas%rsvr%rchg(nrsvr), &
                stas%rsvr%qi(nrsvr), stas%rsvr%qo(nrsvr), stas%rsvr%stg(nrsvr), stas%rsvr%zlvl(nrsvr), &
                stas%rsvr%div(nrsvr), stas%rsvr%ab(nrsvr), &
                stat = ierr)
            if (ierr == 0) then
                stas%rsvr%rff = 0.0; stas%rsvr%rchg = 0.0
                stas%rsvr%qi = 0.0; stas%rsvr%qo = 0.0; stas%rsvr%stg = 0.0; stas%rsvr%zlvl = 0.0
                stas%rsvr%div = 0.0; stas%rsvr%ab = 0.0
            end if
        end if

        !> Mark that the variable has been initialized.
        stas%inid = (ierr == 0)

    end subroutine

end module
