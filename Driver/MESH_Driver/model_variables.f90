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
        real, dimension(:), pointer :: fsin => null()
        real, dimension(:), pointer :: fsdr => null()
        real, dimension(:), pointer :: fsdff => null()
        real, dimension(:), pointer :: flin => null()
        real, dimension(:), pointer :: ta => null()
        real, dimension(:), pointer :: qa => null()
        real, dimension(:), pointer :: pres => null()
        real, dimension(:), pointer :: uv => null()
        real, dimension(:), pointer :: wdir => null()
        real, dimension(:), pointer :: uu => null()
        real, dimension(:), pointer :: vv => null()
        real, dimension(:), pointer :: pre => null()
        real, dimension(:), pointer :: prern => null()
        real, dimension(:), pointer :: presn => null()

        !* rff: Contributing runoff. [kg m-2].
        !* rchg: Contributing recharge. [kg m-2].
        !* qi: Flow in to the element. [m3 s-1].
        !* qo: Flow from the element. [m3 s-1].
        !* stgch: Channel storage held in the element. [m3].
        !* zlvl: Stage level from the element. [m].
        !* div: Volume diverted to the channel. [m3].
        !* ab: Volume abstracted from the channel. [m3].
        real, dimension(:), pointer :: rff => null()
        real, dimension(:), pointer :: rchg => null()
        real, dimension(:), pointer :: qi => null()
        real, dimension(:), pointer :: qo => null()
        real, dimension(:), pointer :: stgch => null()
        real, dimension(:), pointer :: zlvl => null()
        real, dimension(:), pointer :: div => null()
        real, dimension(:), pointer :: ab => null()

        !* rcan: Intercepted liquid water stored on canopy. [kg m-2].
        !* sncan: Intercepted frozen water stored on canopy. [kg m-2].
        !* cmas: Aggregated mass of vegetation canopy. [kg m-2].
        !* tac: Temperature of air within vegetation canopy. [K].
        !* tcan: Vegetation canopy temperature. [K].
        !* qac: Specific humidity of air within vegetation canopy space. [kg kg-1].
        !* gro: Vegetation growth index.
        real, dimension(:), pointer :: rcan => null()
        real, dimension(:), pointer :: sncan => null()
        real, dimension(:), pointer :: cmas => null()
        real, dimension(:), pointer :: tac => null()
        real, dimension(:), pointer :: tcan => null()
        real, dimension(:), pointer :: qac => null()
        real, dimension(:), pointer :: gro => null()

        !* zsno: Snow depth. [m].
        !* sno: Mass of snow pack. [kg m-2].
        !* fsno: Diagnosed fractional snow coverage. [ ].
        !* albs: Snow albedo.
        !* rhos: Density of snow. [kg m-3].
        !* wsno: Liquid water content of snow pack. [kg m-2].
        !* tsno: Snowpack temperature. [K].
        real, dimension(:), pointer :: zsno => null()
        real, dimension(:), pointer :: sno => null()
        real, dimension(:), pointer :: fsno => null()
        real, dimension(:), pointer :: albs => null()
        real, dimension(:), pointer :: rhos => null()
        real, dimension(:), pointer :: wsno => null()
        real, dimension(:), pointer :: tsno => null()

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
        real, dimension(:), pointer :: albt => null()
        real, dimension(:), pointer :: alvs => null()
        real, dimension(:), pointer :: alir => null()
        real, dimension(:), pointer :: gte => null()
        real, dimension(:), pointer :: zpnd => null()
        real, dimension(:), pointer :: pndw => null()
        real, dimension(:), pointer :: tpnd => null()
        real, dimension(:), pointer :: fstr => null()
        real, dimension(:), pointer :: pevp => null()
        real, dimension(:), pointer :: evap => null()
        real, dimension(:), pointer :: evpb => null()
        real, dimension(:), pointer :: arrd => null()
        real, dimension(:), pointer :: rofo => null()
        real, dimension(:), pointer :: qevp => null()
        real, dimension(:), pointer :: hfs => null()
        real, dimension(:), pointer :: gzero => null()
        real, dimension(:, :), pointer :: tsfs => null()

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
        real, dimension(:), pointer :: ggeo => null()
        real, dimension(:), pointer :: rofs => null()
        real, dimension(:), pointer :: tbas => null()
        real, dimension(:, :), pointer :: delzw => null()
        real, dimension(:, :), pointer :: zbotw => null()
        real, dimension(:, :), pointer :: thic => null()
        real, dimension(:, :), pointer :: fzws => null()
        real, dimension(:, :), pointer :: thlq => null()
        real, dimension(:, :), pointer :: lqws => null()
        real, dimension(:, :), pointer :: tbar => null()
        real, dimension(:, :), pointer :: gflx => null()

        !* lzs: Liquid water storage in the lower zone. [kg m-2].
        !* dzs: Liquid water storage in the deep zone. [kg m-2].
        !* rofb: Baseflow component of total runoff. [kg m-2 s-1].
        real, dimension(:), pointer :: lzs => null()
        real, dimension(:), pointer :: dzs => null()
        real, dimension(:), pointer :: rofb => null()

        !* stgw: Total liquid water storage in the land surface. [kg m-2].
        !* stge: Total energy stored in the system. [W m-2].
        real, dimension(:), pointer :: stgw => null()
        real, dimension(:), pointer :: stge => null()
    end type

    !> Description:
    !>  Container for a group of variables.
    !>
    !> Variables:
    !*  tile: Elemental computational unit 1:NML.
    !*  gru: By GRU 1:NTYPE.
    !*  grid: Grid combined from contributing GRUs (by ACLASS) 1:NA.
    type model_variables_groups
        type(model_variables_fields), pointer :: tile, gru, grid, basin
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
        if (associated(group%fsin)) group%fsin = 0.0
        if (associated(group%fsdr)) group%fsdr = 0.0
        if (associated(group%fsdff)) group%fsdff = 0.0
        if (associated(group%flin)) group%flin = 0.0
        if (associated(group%ta)) group%ta = 0.0
        if (associated(group%qa)) group%qa = 0.0
        if (associated(group%pres)) group%pres = 0.0
        if (associated(group%uv)) group%uv = 0.0
        if (associated(group%wdir)) group%wdir = 0.0
        if (associated(group%uu)) group%uu = 0.0
        if (associated(group%vv)) group%vv = 0.0
        if (associated(group%pre)) group%pre = 0.0
        if (associated(group%prern)) group%prern = 0.0
        if (associated(group%presn)) group%presn = 0.0
        if (associated(group%rff)) group%rff = 0.0
        if (associated(group%rchg)) group%rchg = 0.0
        if (associated(group%qi)) group%qi = 0.0
        if (associated(group%qo)) group%qo = 0.0
        if (associated(group%stgch)) group%stgch = 0.0
        if (associated(group%zlvl)) group%zlvl = 0.0
        if (associated(group%div)) group%div = 0.0
        if (associated(group%ab)) group%ab = 0.0
        if (associated(group%rcan)) group%rcan = 0.0
        if (associated(group%sncan)) group%sncan = 0.0
        if (associated(group%cmas)) group%cmas = 0.0
        if (associated(group%tac)) group%tac = 0.0
        if (associated(group%tcan)) group%tcan = 0.0
        if (associated(group%qac)) group%qac = 0.0
        if (associated(group%gro)) group%gro = 0.0
        if (associated(group%zsno)) group%zsno = 0.0
        if (associated(group%sno)) group%sno = 0.0
        if (associated(group%fsno)) group%fsno = 0.0
        if (associated(group%albs)) group%albs = 0.0
        if (associated(group%rhos)) group%rhos = 0.0
        if (associated(group%wsno)) group%wsno = 0.0
        if (associated(group%tsno)) group%tsno = 0.0
        if (associated(group%albt)) group%albt = 0.0
        if (associated(group%alvs)) group%alvs = 0.0
        if (associated(group%alir)) group%alir = 0.0
        if (associated(group%gte)) group%gte = 0.0
        if (associated(group%zpnd)) group%zpnd = 0.0
        if (associated(group%pndw)) group%pndw = 0.0
        if (associated(group%tpnd)) group%tpnd = 0.0
        if (associated(group%fstr)) group%fstr = 0.0
        if (associated(group%pevp)) group%pevp = 0.0
        if (associated(group%evap)) group%evap = 0.0
        if (associated(group%evpb)) group%evpb = 0.0
        if (associated(group%arrd)) group%arrd = 0.0
        if (associated(group%rofo)) group%rofo = 0.0
        if (associated(group%qevp)) group%qevp = 0.0
        if (associated(group%hfs)) group%hfs = 0.0
        if (associated(group%gzero)) group%gzero = 0.0
        if (associated(group%tsfs)) group%tsfs = 0.0
        if (associated(group%ggeo)) group%ggeo = 0.0
        if (associated(group%rofs)) group%rofs = 0.0
        if (associated(group%tbas)) group%tbas = 0.0
        if (associated(group%delzw)) group%delzw = 0.0
        if (associated(group%zbotw)) group%zbotw = 0.0
        if (associated(group%thic)) group%thic = 0.0
        if (associated(group%fzws)) group%fzws = 0.0
        if (associated(group%thlq)) group%thlq = 0.0
        if (associated(group%lqws)) group%lqws = 0.0
        if (associated(group%tbar)) group%tbar = 0.0
        if (associated(group%gflx)) group%gflx = 0.0
        if (associated(group%lzs)) group%lzs = 0.0
        if (associated(group%dzs)) group%dzs = 0.0
        if (associated(group%rofb)) group%rofb = 0.0
        if (associated(group%stgw)) group%stgw = 0.0
        if (associated(group%stge)) group%stge = 0.0

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
        type(model_variables_fields), pointer :: group

        !> Output variables.
        integer, intent(out) :: ierr

        !> Local variables.
        integer z

        !> Initialize the return status.
        ierr = 0
        z = 0

        !> Allocate group.
        allocate(group)

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
