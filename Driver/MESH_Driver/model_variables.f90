!> Description:
!>  Contains variable types for model variables, such
!>  as components of the water and energy balances, streamflow channels,
!>  and reservoirs.
module model_variables

    implicit none

    !* SAVE/RESUMESTATES: Flag to save or resume variables from file.
    !>  Options:
    !>      - none: Save and resume no states to and from file (default).
    !>      - txt:  In text format.
    !>      - seq:  Sequential binary format.
    !>      - csv:  From CSV by GRU (RESUMESTATES only).
    !>      - r2c:  From r2c by grid (RESUMESTATES only).
    character(len = 80), save :: RESUMESTATES = 'RESUMESTATES none'
    character(len = 80), save :: SAVESTATES = 'SAVESTATES none'

    !> Description:
    !>  Container for variables.
    type model_variables_fields

        !* fsin: Incoming shortwave radiation at the surface. [W m-2].
        !* fsdr: Direct component of incoming shortwave radiation at the surface. [W m-2].
        !* fsdff: Diffuse component of incoming shortwave radiation at the surface. [W m-2].
        !* flin: Incoming longwave radiation at the surface. [W m-2].
        !* ta: Air temperature (at user-specified reference height). [K].
        !* qa: Specific humidity (at user-specificed reference height). [kg kg-1].
        !* pres: Air pressure at the surface. [Pa].
        !* uv: Wind speed (at user-specified reference height). [m s-1].
        !* wdir: Wind direction (at user-specified referenced height). [--].
        !* uu: U-component of wind speed (at user-specified reference height). [m s-1].
        !* vv: V-component of wind speed (at user-specified reference height). [m s-1].
        !* pre: Total incoming precipitation rate. [kg m-2 s-1].
        !* prern: Total incoming liquid precipitation rate. [kg m-2 s-1].
        !* presn: Total incoming solid precipitation rate. [kg m-2 s-1].
        real, dimension(:), pointer :: fsin
        real, dimension(:), pointer :: fsdr
        real, dimension(:), pointer :: fsdff
        real, dimension(:), pointer :: flin
        real, dimension(:), pointer :: ta
        real, dimension(:), pointer :: qa
        real, dimension(:), pointer :: pres
        real, dimension(:), pointer :: uv
        real, dimension(:), pointer :: wdir
        real, dimension(:), pointer :: uu
        real, dimension(:), pointer :: vv
        real, dimension(:), pointer :: pre
        real, dimension(:), pointer :: prern
        real, dimension(:), pointer :: presn

        !* rff: Contributing runoff. [kg m-2].
        !* rchg: Contributing recharge. [kg m-2].
        !* qi: Flow in to the element. [m3 s-1].
        !* qo: Flow from the element. [m3 s-1].
        !* stgch: Channel storage held in the element. [m3].
        !* zlvl: Stage level from the element. [m].
        !* div: Volume diverted to the channel. [m3].
        !* ab: Volume abstracted from the channel. [m3].
        real, dimension(:), pointer :: rff
        real, dimension(:), pointer :: rchg
        real, dimension(:), pointer :: qi
        real, dimension(:), pointer :: qo
        real, dimension(:), pointer :: stgch
        real, dimension(:), pointer :: zlvl
        real, dimension(:), pointer :: div
        real, dimension(:), pointer :: ab

        !* rcan: Intercepted liquid water stored on canopy. [kg m-2].
        !* sncan: Intercepted frozen water stored on canopy. [kg m-2].
        !* cmas: Aggregated mass of vegetation canopy. [kg m-2].
        !* tac: Temperature of air within vegetation canopy. [K].
        !* tcan: Vegetation canopy temperature. [K].
        !* qac: Specific humidity of air within vegetation canopy space. [kg kg-1].
        !* gro: Vegetation growth index.
        real, dimension(:), pointer :: rcan
        real, dimension(:), pointer :: sncan
        real, dimension(:), pointer :: cmas
        real, dimension(:), pointer :: tac
        real, dimension(:), pointer :: tcan
        real, dimension(:), pointer :: qac
        real, dimension(:), pointer :: gro

        !* zsno: Snow depth. [m].
        !* sno: Mass of snow pack. [kg m-2].
        !* fsno: Diagnosed fractional snow coverage. [ ].
        !* albs: Snow albedo.
        !* rhos: Density of snow. [kg m-3].
        !* wsno: Liquid water content of snow pack. [kg m-2].
        !* tsno: Snowpack temperature. [K].
        real, dimension(:), pointer :: zsno
        real, dimension(:), pointer :: sno
        real, dimension(:), pointer :: fsno
        real, dimension(:), pointer :: albs
        real, dimension(:), pointer :: rhos
        real, dimension(:), pointer :: wsno
        real, dimension(:), pointer :: tsno

        !* albt: Total albedo of the surface (visible and near-infrared). [--].
        !* alvs: Visible albedo of the surface. [--].
        !* alir: Near-infrared albedo of the surface. [--].
        !* gte: Effective black-body temperature at the surface. [K].
        !* zpnd: Depth of ponded water on surface. [m].
        !* pndw: Ponded water storage on the surface. [kg m-2].
        !* tpnd: Temperature of ponded water. [K].
        !* fstr: Contributing fraction of ponded water (PDMROF). [--].
        !* pevp: Diagnosed potential evapotranspiration. [kg m-2 s-1].
        !* evap: Evapotranspiration. [kg m-2].
        !* evpb: Evaporation efficiency (EVP to PEVP) of the canopy. [--].
        !* arrd: Arridity index (PRE to PEVP). [--].
        !* rofo: Overland component of total runoff. [kg m-2 s-1].
        !* qevp: Latent heat flux at the surface. [W m-2].
        !* hfs: Sensible heat flux at the surface. [W m-2].
        !* gzero: Heat flux into the soil at the surface. [W m-2].
        !* tsfs: Ground surface temperature over subarea. [K].
        real, dimension(:), pointer :: albt
        real, dimension(:), pointer :: alvs
        real, dimension(:), pointer :: alir
        real, dimension(:), pointer :: gte
        real, dimension(:), pointer :: zpnd
        real, dimension(:), pointer :: pndw
        real, dimension(:), pointer :: tpnd
        real, dimension(:), pointer :: fstr
        real, dimension(:), pointer :: pevp
        real, dimension(:), pointer :: evap
        real, dimension(:), pointer :: evpb
        real, dimension(:), pointer :: arrd
        real, dimension(:), pointer :: rofo
        real, dimension(:), pointer :: qevp
        real, dimension(:), pointer :: hfs
        real, dimension(:), pointer :: gzero
        real, dimension(:, :), pointer :: tsfs

        !* ggeo: Geothermal heat flux. [W m-2].
        !* rofs: Interflow component of total runoff. [kg m-2 s-1].
        !* tbas: Temperature of bedrock in third soil layer. [K].
        !* delzw: Thickness of permeable part of soil layer. [m].
        !* zbotw: Depth of bottom of permeable part of soil layer. [m].
        !* thic: Volumetric frozen water content of soil layers. [m3 m-3].
        !* fzws: Frozen water storage. [kg m-2].
        !* thlq: Volumetric liquid water content of soil layers. [m3 m-3].
        !* lqws: Liquid water storage. [kg m-2].
        !* tbar: Temperature of soil layers. [K].
        !* gflx: Heat conduction between soil layers. [W m-2].
        real, dimension(:), pointer :: ggeo
        real, dimension(:), pointer :: rofs
        real, dimension(:), pointer :: tbas
        real, dimension(:, :), pointer :: delzw
        real, dimension(:, :), pointer :: zbotw
        real, dimension(:, :), pointer :: thic
        real, dimension(:, :), pointer :: fzws
        real, dimension(:, :), pointer :: thlq
        real, dimension(:, :), pointer :: lqws
        real, dimension(:, :), pointer :: tbar
        real, dimension(:, :), pointer :: gflx

        !* lzs: Liquid water storage in the lower zone. [kg m-2].
        !* dzs: Liquid water storage in the deep zone. [kg m-2].
        !* rofb: Baseflow component of total runoff. [kg m-2 s-1].
        real, dimension(:), pointer :: lzs
        real, dimension(:), pointer :: dzs
        real, dimension(:), pointer :: rofb

        !* stgw: Total liquid water storage in the land surface. [kg m-2].
        !* stge: Total energy stored in the system. [W m-2].
        real, dimension(:), pointer :: stgw
        real, dimension(:), pointer :: stge
    end type

    !> Description:
    !>  Container for a group of variables.
    !>
    !> Variables:
    !*  tile: Elemental computational unit 1:NML.
    !*  gru: By GRU 1:NTYPE.
    !*  grid: Grid combined from contributing GRUs (by ACLASS) 1:NA.
    type model_variables_groups
        type(model_variables_fields) tile, gru, grid, basin
    end type

    !> State of SA_MESH variables in the current time-step.
    !*  vs: Group of variables (e.g., tile, grid).
    type(model_variables_groups), save :: vs

    contains

    !> Description:
    !>  Subroutine to reset/zero a group of variables.
    !>
    !> Variables:
    !*  group: Group of variables.
    !*  ierr: Return status
    subroutine model_variables_group_reset(group, ierr)

        !> Input/output variables.
        type(model_variables_fields) group

        !> Output variables.
        integer, intent(out) :: ierr

        !> Initialize the return status.
        ierr = 0

        !> Initialize variables.
        group%fsin = 0.0
        group%fsdr = 0.0
        group%fsdff = 0.0
        group%flin = 0.0
        group%ta = 0.0
        group%qa = 0.0
        group%pres = 0.0
        group%uv = 0.0
        group%wdir = 0.0
        group%uu = 0.0
        group%vv = 0.0
        group%pre = 0.0
        group%prern = 0.0
        group%presn = 0.0
        group%rff = 0.0
        group%rchg = 0.0
        group%qi = 0.0
        group%qo = 0.0
        group%stgch = 0.0
        group%zlvl = 0.0
        group%div = 0.0
        group%ab = 0.0
        group%rcan = 0.0
        group%sncan = 0.0
        group%cmas = 0.0
        group%tac = 0.0
        group%tcan = 0.0
        group%qac = 0.0
        group%gro = 0.0
        group%zsno = 0.0
        group%sno = 0.0
        group%fsno = 0.0
        group%albs = 0.0
        group%rhos = 0.0
        group%wsno = 0.0
        group%tsno = 0.0
        group%albt = 0.0
        group%alvs = 0.0
        group%alir = 0.0
        group%gte = 0.0
        group%zpnd = 0.0
        group%pndw = 0.0
        group%tpnd = 0.0
        group%fstr = 0.0
        group%pevp = 0.0
        group%evap = 0.0
        group%evpb = 0.0
        group%arrd = 0.0
        group%rofo = 0.0
        group%qevp = 0.0
        group%hfs = 0.0
        group%gzero = 0.0
        group%tsfs = 0.0
        group%ggeo = 0.0
        group%rofs = 0.0
        group%tbas = 0.0
        group%delzw = 0.0
        group%zbotw = 0.0
        group%thic = 0.0
        group%fzws = 0.0
        group%thlq = 0.0
        group%lqws = 0.0
        group%tbar = 0.0
        group%gflx = 0.0
        group%lzs = 0.0
        group%dzs = 0.0
        group%rofb = 0.0
        group%stgw = 0.0
        group%stge = 0.0

    end subroutine

    !> Description:
    !>  Subroutine to reset/zero all variables.
    !>
    !> Variables:
    !*  shd: Basin information.
    !*  ierr: Return status
    subroutine model_variables_reset(shd, ierr)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer z

        !> Initialize the return status.
        ierr = 0
        z = 0

        !> Reset groups.
        if (ro%RUNTILE) then
            call model_variables_group_reset(vs%tile, z); if (z /= 0) ierr = z
            call model_variables_group_reset(vs%gru, z); if (z /= 0) ierr = z
        end if
        if (ro%RUNGRID) then
            call model_variables_group_reset(vs%grid, z); if (z /= 0) ierr = z
            call model_variables_group_reset(vs%basin, z); if (z /= 0) ierr = z
        end if

    end subroutine

    !> Description:
    !>  Subroutine to allocate a group of variables.
    !>
    !> Variables:
    !*  group: Group of variables.
    !*  n: Index of elements (e.g., tiles, grids).
    !*  nsl: Number of layers.
    !*  ierr: Return status
    subroutine model_variables_group_allocate(group, n, nsl, ierr)

        !> Input variables.
        integer, intent(in) :: n, nsl

        !> Input/output variables.
        type(model_variables_fields) group

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer z

        !> Initialize the return status.
        ierr = 0
        z = 0

        !> Allocate variables.
        allocate(group%fsin(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fsdr(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fsdff(n), stat = z); if (z /= 0) ierr = z
        allocate(group%flin(n), stat = z); if (z /= 0) ierr = z
        allocate(group%ta(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qa(n), stat = z); if (z /= 0) ierr = z
        allocate(group%pres(n), stat = z); if (z /= 0) ierr = z
        allocate(group%uv(n), stat = z); if (z /= 0) ierr = z
        allocate(group%wdir(n), stat = z); if (z /= 0) ierr = z
        allocate(group%uu(n), stat = z); if (z /= 0) ierr = z
        allocate(group%vv(n), stat = z); if (z /= 0) ierr = z
        allocate(group%pre(n), stat = z); if (z /= 0) ierr = z
        allocate(group%prern(n), stat = z); if (z /= 0) ierr = z
        allocate(group%presn(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rff(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rchg(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qi(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qo(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stgch(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zlvl(n), stat = z); if (z /= 0) ierr = z
        allocate(group%div(n), stat = z); if (z /= 0) ierr = z
        allocate(group%ab(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rcan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%sncan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%cmas(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tac(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tcan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qac(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gro(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%sno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%albs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rhos(n), stat = z); if (z /= 0) ierr = z
        allocate(group%wsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%albt(n), stat = z); if (z /= 0) ierr = z
        allocate(group%alvs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%alir(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gte(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zpnd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%pndw(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tpnd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fstr(n), stat = z); if (z /= 0) ierr = z
        allocate(group%pevp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%evap(n), stat = z); if (z /= 0) ierr = z
        allocate(group%evpb(n), stat = z); if (z /= 0) ierr = z
        allocate(group%arrd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rofo(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qevp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%hfs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gzero(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tsfs(n, 4), stat = z); if (z /= 0) ierr = z
        allocate(group%ggeo(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rofs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tbas(n), stat = z); if (z /= 0) ierr = z
        allocate(group%delzw(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%zbotw(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thic(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%fzws(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thlq(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%lqws(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%tbar(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%gflx(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%lzs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%dzs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rofb(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stgw(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stge(n), stat = z); if (z /= 0) ierr = z

    end subroutine

    !> Description:
    !>  Subroutine to allocate variables.
    !>
    !> Variables:
    !*  shd: Basin information.
    !*  ierr: Return status
    subroutine model_variables_init(shd, ierr)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer z

        !> Initialize the return status.
        ierr = 0
        z = 0

        !> Allocate and initialize groups.
        if (ro%RUNTILE) then
            call model_variables_group_allocate(vs%tile, shd%lc%NML, shd%lc%IGND, z); if (z /= 0) ierr = z
            call model_variables_group_reset(vs%tile, z); if (z /= 0) ierr = z
            call model_variables_group_allocate(vs%gru, shd%lc%NTYPE, shd%lc%IGND, z); if (z /= 0) ierr = z
            call model_variables_group_reset(vs%gru, z); if (z /= 0) ierr = z
        end if
        if (ro%RUNGRID) then
            call model_variables_group_allocate(vs%grid, shd%NA, shd%lc%IGND, z); if (z /= 0) ierr = z
            call model_variables_group_reset(vs%grid, z); if (z /= 0) ierr = z
            call model_variables_group_allocate(vs%basin, shd%NA, shd%lc%IGND, z); if (z /= 0) ierr = z
            call model_variables_group_reset(vs%basin, z); if (z /= 0) ierr = z
        end if

    end subroutine

end module
