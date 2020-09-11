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

        !* fsin: Incoming shortwave radiation at the surface. [W m**-2].
        !* fsdr: Direct component of incoming shortwave radiation at the surface. [W m**-2].
        !* fsdff: Diffuse component of incoming shortwave radiation at the surface. [W m**-2].
        !* flin: Incoming longwave radiation at the surface. [W m**-2].
        !* ta: Air temperature (at user-specified reference height). [K].
        !* qa: Specific humidity (at user-specificed reference height). [kg kg**-1].
        !* pres: Air pressure at the surface. [Pa].
        !* uv: Wind speed (at user-specified reference height). [m s**-1].
        !* wdir: Wind direction (at user-specified referenced height). [degrees].
        !* uu: U-component of wind speed (at user-specified reference height). [m s**-1].
        !* vv: V-component of wind speed (at user-specified reference height). [m s**-1].
        !* pre: Total incoming precipitation rate. [kg m**-2 s**-1].
        !* prern: Total incoming liquid precipitation rate. [kg m**-2 s**-1].
        !* presno: Total incoming solid precipitation rate. [kg m**-2 s**-1].
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
        real, dimension(:), pointer :: presno => null()

        !* lqwscan: Liquid water interception in the canopy. [kg m**-2].
        !* fzwscan: Frozen water interception in the canopy. [kg m**-2].
        !* cmas: Organic mass of the canopy. [kg m**-2].
        !* tacan: Air temperature in the canopy. [K].
        !* qacan: Specific humidity of air in the canopy. [kg kg**-1].
        !* tcan: Vegetation canopy temperature. [K].
        !* gro: Vegetation growth index. [--].
        real, dimension(:), pointer :: lqwscan => null()
        real, dimension(:), pointer :: fzwscan => null()
        real, dimension(:), pointer :: cmas => null()
        real, dimension(:), pointer :: tacan => null()
        real, dimension(:), pointer :: qacan => null()
        real, dimension(:), pointer :: tcan => null()
        real, dimension(:), pointer :: gro => null()

        !* sno: Snow mass. [kg m**-2].
        !* rhosno: Snow density. [kg m**-3].
        !* zsno: Snow depth. [m].
        !* fsno: Fraction of fully snow covered area. [fraction].
        !* albsno: Snow albedo. [fraction].
        !* lqwssno: Liquid water content of the snow. [kg m**-2].
        !* tsno: Snowpack temperature. [K].
        !* drainsno: Drainage from the bottom of the snowpack (runoff rate). [kg m**-2 s**-1].
        real, dimension(:), pointer :: sno => null()
        real, dimension(:), pointer :: rhosno => null()
        real, dimension(:), pointer :: zsno => null()
        real, dimension(:), pointer :: fsno => null()
        real, dimension(:), pointer :: albsno => null()
        real, dimension(:), pointer :: lqwssno => null()
        real, dimension(:), pointer :: tsno => null()
        real, dimension(:), pointer :: drainsno => null()

        !* albt: Total albedo of the surface (visible and near-infrared). [fraction].
        !* alvs: Visible component of the total albedo of the surface. [fraction].
        !* alir: Near-infrared components of the total albedo of the surface. [fraction].
        !* gte: Effective black-body temperature at the surface. [K].
        !* zpnd: Depth of ponded water. [m].
        !* lqwspnd: Liquid water storage of ponded water. [kg m**-2].
        !* tpnd: Temperature of ponded water. [K].
        !* fstr: Contributing fraction of ponded water (PDMROF). [fraction].
        !* potevp: Potential evaporation rate. [kg m**-2 s**-1].
        !* et: Evapotranspiration rate. [kg m**-2 s**-1].
        !* evpb: Evaporation efficiency (ET to POTEVP) of the canopy. [--].
        !* arrd: Arridity index (PRE to POTEVP). [--].
        !* ovrflw: Overland runoff rate. [kg m**-2 s**-1].
        !* qevp: Latent heat flux at the surface. [W m**-2].
        !* qsens: Sensible heat flux at the surface. [W m**-2].
        !* gzero: Heat flux into the ground. [W m**-2].
        !* tsfs: Ground surface temperature over subarea. [K].
        real, dimension(:), pointer :: albt => null()
        real, dimension(:), pointer :: alvs => null()
        real, dimension(:), pointer :: alir => null()
        real, dimension(:), pointer :: gte => null()
        real, dimension(:), pointer :: zpnd => null()
        real, dimension(:), pointer :: lqwspnd => null()
        real, dimension(:), pointer :: tpnd => null()
        real, dimension(:), pointer :: fstr => null()
        real, dimension(:), pointer :: potevp => null()
        real, dimension(:), pointer :: et => null()
        real, dimension(:), pointer :: evpb => null()
        real, dimension(:), pointer :: arrd => null()
        real, dimension(:), pointer :: ovrflw => null()
        real, dimension(:), pointer :: qevp => null()
        real, dimension(:), pointer :: qsens => null()
        real, dimension(:), pointer :: gzero => null()
        real, dimension(:, :), pointer :: tsfs => null()

        !* ggeo: Geothermal heat flux. [W m**-2].
        !* tbas: Temperature of bedrock in third soil layer. [K].
        !* thlqsol: Volumetric liquid water content of the soil. [m3 m**-3].
        !* thicsol: Volumetric frozen water content of the soil. [m3 m**-3].
        !* lqwssol: Liquid water storage in the soil. [kg m**-2].
        !* fzwssol: Frozen water storage in the soil. [kg m**-2].
        !* tsol: Temperature of the soil. [K].
        !* gflx: Heat conduction between soil layers. [W m**-2].
        !* latflw: Interflow runoff rate. [kg m**-2 s**-1].
        !* dzwat: Permeable thickness of the soil layer. [m].
        !* zbotwat: Permeable depth of the soil layer. [m].
        !* drainsol: Drainage from the bottom of the permeable soil column (runoff rate). [kg m**-2 s**-1].
        real, dimension(:), pointer :: ggeo => null()
        real, dimension(:), pointer :: tbas => null()
        real, dimension(:, :), pointer :: thlqsol => null()
        real, dimension(:, :), pointer :: thicsol => null()
        real, dimension(:, :), pointer :: lqwssol => null()
        real, dimension(:, :), pointer :: fzwssol => null()
        real, dimension(:, :), pointer :: tsol => null()
        real, dimension(:, :), pointer :: gflx => null()
        real, dimension(:, :), pointer :: latflw => null()
        real, dimension(:, :), pointer :: dzwat => null()
        real, dimension(:, :), pointer :: zbotwat => null()
        real, dimension(:), pointer :: drainsol => null()

        !* rchg: Drainage into groundwater/lower zone storage. [mm].
        !* stggw: Groundwater/lower zone storage. [mm].
        !* dzs: Deep aquifer water storage. [mm].
        real, dimension(:), pointer :: rchg => null()
        real, dimension(:), pointer :: stggw => null()
        real, dimension(:), pointer :: dzs => null()

        !* stge: Total energy stored in the system. [W m**-2].
        !* stgw: Total liquid water storage in the land surface. [kg m**-2].
        real, dimension(:), pointer :: stge => null()
        real, dimension(:), pointer :: stgw => null()

        !* rff: Total runoff (from all surface, subsurface, and groundwater components). [mm].
        !* qi: Flow rate entering the channel. [m**3 s**-1].
        !* qo: Flow rate leaving the channel (discharge). [m**3 s**-1].
        !* stgch: Channel storage. [m**3].
        !* zlvl: Stage level. [m].
        !* div: Storage diverted to a routing element. [m**3].
        !* abstr: Storage abstracted from a routing element. [m**3].
        real, dimension(:), pointer :: rff => null()
        real, dimension(:), pointer :: qi => null()
        real, dimension(:), pointer :: qo => null()
        real, dimension(:), pointer :: stgch => null()
        real, dimension(:), pointer :: zlvl => null()
        real, dimension(:), pointer :: div => null()
        real, dimension(:), pointer :: abstr => null()
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
        if (associated(group%presno)) group%presno = 0.0
        if (associated(group%lqwscan)) group%lqwscan = 0.0
        if (associated(group%fzwscan)) group%fzwscan = 0.0
        if (associated(group%cmas)) group%cmas = 0.0
        if (associated(group%tacan)) group%tacan = 0.0
        if (associated(group%qacan)) group%qacan = 0.0
        if (associated(group%tcan)) group%tcan = 0.0
        if (associated(group%gro)) group%gro = 0.0
        if (associated(group%zsno)) group%zsno = 0.0
        if (associated(group%rhosno)) group%rhosno = 0.0
        if (associated(group%sno)) group%sno = 0.0
        if (associated(group%fsno)) group%fsno = 0.0
        if (associated(group%albsno)) group%albsno = 0.0
        if (associated(group%lqwssno)) group%lqwssno = 0.0
        if (associated(group%tsno)) group%tsno = 0.0
        if (associated(group%drainsno)) group%drainsno = 0.0
        if (associated(group%albt)) group%albt = 0.0
        if (associated(group%alvs)) group%alvs = 0.0
        if (associated(group%alir)) group%alir = 0.0
        if (associated(group%gte)) group%gte = 0.0
        if (associated(group%zpnd)) group%zpnd = 0.0
        if (associated(group%lqwspnd)) group%lqwspnd = 0.0
        if (associated(group%tpnd)) group%tpnd = 0.0
        if (associated(group%fstr)) group%fstr = 0.0
        if (associated(group%potevp)) group%potevp = 0.0
        if (associated(group%et)) group%et = 0.0
        if (associated(group%evpb)) group%evpb = 0.0
        if (associated(group%arrd)) group%arrd = 0.0
        if (associated(group%ovrflw)) group%ovrflw = 0.0
        if (associated(group%qevp)) group%qevp = 0.0
        if (associated(group%qsens)) group%qsens = 0.0
        if (associated(group%gzero)) group%gzero = 0.0
        if (associated(group%tsfs)) group%tsfs = 0.0
        if (associated(group%ggeo)) group%ggeo = 0.0
        if (associated(group%tbas)) group%tbas = 0.0
        if (associated(group%thlqsol)) group%thlqsol = 0.0
        if (associated(group%thicsol)) group%thicsol = 0.0
        if (associated(group%lqwssol)) group%lqwssol = 0.0
        if (associated(group%fzwssol)) group%fzwssol = 0.0
        if (associated(group%tsol)) group%tsol = 0.0
        if (associated(group%gflx)) group%gflx = 0.0
        if (associated(group%latflw)) group%latflw = 0.0
        if (associated(group%dzwat)) group%dzwat = 0.0
        if (associated(group%zbotwat)) group%zbotwat = 0.0
        if (associated(group%drainsol)) group%drainsol = 0.0
        if (associated(group%rchg)) group%rchg = 0.0
        if (associated(group%stggw)) group%stggw = 0.0
        if (associated(group%dzs)) group%dzs = 0.0
        if (associated(group%stge)) group%stge = 0.0
        if (associated(group%stgw)) group%stgw = 0.0
        if (associated(group%rff)) group%rff = 0.0
        if (associated(group%qi)) group%qi = 0.0
        if (associated(group%qo)) group%qo = 0.0
        if (associated(group%stgch)) group%stgch = 0.0
        if (associated(group%zlvl)) group%zlvl = 0.0
        if (associated(group%div)) group%div = 0.0
        if (associated(group%abstr)) group%abstr = 0.0

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
        allocate(group%presno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%lqwscan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fzwscan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%cmas(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tacan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qacan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tcan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gro(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rhosno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%sno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%albsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%lqwssno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%drainsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%albt(n), stat = z); if (z /= 0) ierr = z
        allocate(group%alvs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%alir(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gte(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zpnd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%lqwspnd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tpnd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fstr(n), stat = z); if (z /= 0) ierr = z
        allocate(group%potevp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%et(n), stat = z); if (z /= 0) ierr = z
        allocate(group%evpb(n), stat = z); if (z /= 0) ierr = z
        allocate(group%arrd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%ovrflw(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qevp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qsens(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gzero(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tsfs(n, 4), stat = z); if (z /= 0) ierr = z
        allocate(group%ggeo(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tbas(n), stat = z); if (z /= 0) ierr = z
        allocate(group%thlqsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thicsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%lqwssol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%fzwssol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%tsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%gflx(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%latflw(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%dzwat(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%zbotwat(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%drainsol(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rchg(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stggw(n), stat = z); if (z /= 0) ierr = z
        allocate(group%dzs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stge(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stgw(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rff(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qi(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qo(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stgch(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zlvl(n), stat = z); if (z /= 0) ierr = z
        allocate(group%div(n), stat = z); if (z /= 0) ierr = z
        allocate(group%abstr(n), stat = z); if (z /= 0) ierr = z

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
