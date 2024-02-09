!> Description:
!>  Contains variable types for model variables, such
!>  as components of the water and energy balances, streamflow channels,
!>  and reservoirs.
module model_variables

    !> 'mesh_io_constants' for character lengths.
    use mesh_io_constants, only: SHORT_FIELD_LENGTH, LONG_FIELD_LENGTH

    implicit none

    !> Description:
    !>  Container for variables.
    type model_variables_fields

        !> Meteorology/climatology variables.
        !* fsin: Incoming shortwave radiation at the surface. [W m**-2].
        !* fsvs: Visible component of incoming shortwave radiation at the surface. [W m**-2].
        !* fsir: Infrared component of incoming shortwave radiation at the surface. [W m**-2].
        !* fsdr: Direct component of incoming shortwave radiation at the surface. [W m**-2].
        !* fsdff: Diffuse component of incoming shortwave radiation at the surface. [W m**-2].
        !* flin: Incoming longwave radiation at the surface. [W m**-2].
        !* ta: Air temperature (at user-specified reference height). [K].
        !* qa: Specific humidity (at user-specificed reference height). [kg kg**-1].
        !* pres: Air pressure at the surface. [Pa].
        !* uu: U-component of wind speed (at user-specified reference height). [m s**-1].
        !* vv: V-component of wind speed (at user-specified reference height). [m s**-1].
        !* uv: Wind speed (at user-specified reference height). [m s**-1].
        !* wdir: Wind direction (at user-specified referenced height). [degrees].
        !* prern: Total incoming liquid precipitation rate. [kg m**-2 s**-1].
        !* presno: Total incoming solid precipitation rate. [kg m**-2 s**-1].
        !* pre: Total incoming precipitation rate. [kg m**-2 s**-1].
        real, dimension(:), allocatable :: fsin
        real, dimension(:), allocatable :: fsvs
        real, dimension(:), allocatable :: fsir
        real, dimension(:), allocatable :: fsdr
        real, dimension(:), allocatable :: fsdff
        real, dimension(:), allocatable :: flin
        real, dimension(:), allocatable :: ta
        real, dimension(:), allocatable :: qa
        real, dimension(:), allocatable :: pres
        real, dimension(:), allocatable :: uu
        real, dimension(:), allocatable :: vv
        real, dimension(:), allocatable :: uv
        real, dimension(:), allocatable :: wdir
        real, dimension(:), allocatable :: prern
        real, dimension(:), allocatable :: presno
        real, dimension(:), allocatable :: pre

        !> Canopy variables.
        !* lqwscan: Liquid water interception in the canopy. [kg m**-2].
        !* fzwscan: Frozen water interception in the canopy. [kg m**-2].
        !* cmas: Organic mass of the canopy. [kg m**-2].
        !* tacan: Air temperature in the canopy. [K].
        !* qacan: Specific humidity of air in the canopy. [kg kg**-1].
        !* uvcan: Wind speed of air in the canopy. [m s**-1].
        !* tcan: Vegetation canopy temperature. [K].
        !* gro: Vegetation growth index. [--].
        real, dimension(:), allocatable :: lqwscan
        real, dimension(:), allocatable :: fzwscan
        real, dimension(:), allocatable :: cmas
        real, dimension(:), allocatable :: tacan
        real, dimension(:), allocatable :: qacan
        real, dimension(:), allocatable :: uvcan
        real, dimension(:), allocatable :: tcan
        real, dimension(:), allocatable :: gro

        !> Snow variables.
        !* fsno: Fraction of fully snow covered area. [fraction].
        !* sno: Snow mass. [kg m**-2].
        !* rhosno: Snow density. [kg m**-3].
        !* zsno: Snow depth. [m].
        !* lqwssno: Liquid water content of the snow. [kg m**-2].
        !* tsno: Snowpack temperature. [K].
        !* albsno: Snow albedo. [fraction].
        !* drainsno: Drainage from the bottom of the snowpack (runoff rate). [kg m**-2 s**-1].
        real, dimension(:), allocatable :: fsno
        real, dimension(:), allocatable :: sno
        real, dimension(:), allocatable :: rhosno
!-        real, dimension(:), allocatable :: zsno
        real, dimension(:), allocatable :: lqwssno
        real, dimension(:), allocatable :: tsno
        real, dimension(:), allocatable :: albsno
        real, dimension(:), allocatable :: drainsno

        !> Surface variables.
        !* albt: Total albedo of the surface (visible and near-infrared). [fraction].
        !* alvs: Visible component of the total albedo of the surface. [fraction].
        !* alir: Near-infrared components of the total albedo of the surface. [fraction].
        !* gte: Effective black-body temperature at the surface. [K].
        !* zpnd: Depth of ponded water. [m].
        !* lqwspnd: Liquid water storage of ponded water. [kg m**-2].
        !* tpnd: Temperature of ponded water. [K].
        !* pndcaf: Contributing fraction of ponded water (PDMROF). [fraction].
        !* potevp: Potential evaporation rate. [kg m**-2 s**-1].
        !* et: Evapotranspiration rate. [kg m**-2 s**-1].
        !* evpb: Evaporation efficiency (ET to POTEVP) of the canopy. [--].
        !* arrd: Arridity index (PRE to POTEVP). [--].
        !* ovrflw: Overland runoff rate. [kg m**-2 s**-1].
        !* qevp: Latent heat flux at the surface. [W m**-2].
        !* qsens: Sensible heat flux at the surface. [W m**-2].
        !* gzero: Heat flux into the ground. [W m**-2].
        !* tsfs: Ground surface temperature over subarea. [K].
        !* tsurf: Surface temperature. [K].
        real, dimension(:), allocatable :: albt
        real, dimension(:), allocatable :: alvs
        real, dimension(:), allocatable :: alir
        real, dimension(:), allocatable :: gte
        real, dimension(:), allocatable :: zpnd
!-        real, dimension(:), allocatable :: lqwspnd
        real, dimension(:), allocatable :: tpnd
        real, dimension(:), allocatable :: pndcaf
        real, dimension(:), allocatable :: potevp
        real, dimension(:), allocatable :: et
!-        real, dimension(:), allocatable :: evpb
!-        real, dimension(:), allocatable :: arrd
        real, dimension(:), allocatable :: ovrflw
        real, dimension(:), allocatable :: qevp
        real, dimension(:), allocatable :: qsens
        real, dimension(:), allocatable :: gzero
        real, dimension(:, :), allocatable :: tsfs
        real, dimension(:), allocatable :: tsurf

        !> Ice/glacier variables.
        !* lqwsice: Liquid water storage converted to ice in growing glaciers. [kg m**-2].
        !* tice: Temperature of liquid water converted to ice in growing glaciers. [K].
        real, dimension(:), allocatable :: lqwsice
        real, dimension(:), allocatable :: tice

        !> Subsurface/soil variables.
        !* dzsol: Thickness of the soil layer. [m].
        !* dzsolhyd: Permeable thickness of the soil layer. [m].
        !* thlqsol: Volumetric liquid water content of the soil. [m3 m**-3].
        !* thicsol: Volumetric frozen water content of the soil. [m3 m**-3].
        !* lqwssol: Liquid water storage in the soil. [kg m**-2].
        !* fzwssol: Frozen water storage in the soil. [kg m**-2].
        !* tsol: Temperature of the soil. [K].
        !* gflx: Heat conduction between soil layers. [W m**-2].
        !* hcps: Heat Capcity of soil material (soil base not including mositure). [J m-3 K-1].
        !* hcpc, hcpg: Heat Capacity of soil under canopy and under bare ground (considering moisture content (water & ice). [J m-3 K-1].
        !* tctopc, tcbotc: Thermal conductivity of soil under canopy at the top & bottom of the layer (considering moisture content). [W m-1 K-1].
        !* tctopg, tcbotg: Thermal conductivity of soil under bare ground at the top & bottom of the layer (considering moisture content). [W m-1 K-1].
        !* latflw: Interflow runoff rate. [kg m**-2 s**-1].
        !* zsol: Depth to the bottom of the soil column. [m].
        !* zsolhyd: Permeable depth of the soil layer. [m].
        !* zsolsat: Depth to the first saturated layer in the soil column (presumed water table). [m].
        !* ggeo: Geothermal heat flux. [W m**-2].
        !* tbas: Temperature of bedrock in third soil layer. [K].
        !* drainsol: Drainage from the bottom of the permeable soil column (runoff rate). [kg m**-2 s**-1].
        real, dimension(:, :), allocatable :: dzsol
        real, dimension(:, :), allocatable :: dzsolhyd
        real, dimension(:, :), allocatable :: thlqsol
        real, dimension(:, :), allocatable :: thicsol
        real, dimension(:, :), allocatable :: lqwssol
        real, dimension(:, :), allocatable :: fzwssol
        real, dimension(:, :), allocatable :: tsol
        real, dimension(:, :), allocatable :: gflx
        real, dimension(:, :), allocatable :: hcps
        real, dimension(:, :), allocatable :: hcpc
        real, dimension(:, :), allocatable :: hcpg
        real, dimension(:, :), allocatable :: tctopc
        real, dimension(:, :), allocatable :: tctopg
        real, dimension(:, :), allocatable :: tcbotc
        real, dimension(:, :), allocatable :: tcbotg
        real, dimension(:, :), allocatable :: latflw
        real, dimension(:), allocatable :: zsol
        real, dimension(:), allocatable :: zsolhyd
        real, dimension(:), allocatable :: zsolsat
        real, dimension(:), allocatable :: ggeo
        real, dimension(:), allocatable :: tbas
        real, dimension(:), allocatable :: drainsol

        !> Groundwater/lower zone storage variables.
        !* rchg: Drainage into groundwater/lower zone storage. [mm].
        !* stggw: Groundwater/lower zone storage. [mm].
        !* lkg: Leakage from groundwater/lower zone storage. [mm].
        !* dzs: Deep aquifer water storage. [mm].
        real, dimension(:), allocatable :: rchg
        real, dimension(:), allocatable :: stggw
        real, dimension(:), allocatable :: lkg
!-        real, dimension(:), allocatable :: dzs

        !> Diagnostic variables.
        !* stge: Total energy stored in the system. [W m**-2].
        !* stgw: Total liquid water storage in the land surface. [kg m**-2].
!-        real, dimension(:), allocatable :: stge
!-        real, dimension(:), allocatable :: stgw

        !> Routing variables.
        !* rff: Total runoff (from all surface, subsurface, and groundwater components). [mm].
        !* qi: Flow rate entering the channel. [m**3 s**-1].
        !* qo: Flow rate leaving the channel (discharge). [m**3 s**-1].
        !* stgch: Channel storage. [m**3].
        !* zlvl: Stage level. [m].
        !* div: Storage diverted to a routing element. [m**3].
        !* abstr: Storage abstracted from a routing element. [m**3].
        real, dimension(:), allocatable :: rff
        real, dimension(:), allocatable :: qi
        real, dimension(:), allocatable :: qo
        real, dimension(:), allocatable :: stgch
        real, dimension(:), allocatable :: zlvl
        real, dimension(:), allocatable :: div
        real, dimension(:), allocatable :: abstr

        !> Basin attributes (general).
        integer, dimension(:), allocatable :: next_id
        real, dimension(:), allocatable :: surface_area
        real, dimension(:), allocatable :: area_weight
        real, dimension(:), allocatable :: topo_elev
        real, dimension(:), allocatable :: topo_slope

        !> Routing attributes.
        real, dimension(:), allocatable :: chnl_slope
        real, dimension(:), allocatable :: chnl_length
        integer, dimension(:), allocatable :: ichnl
        integer, dimension(:), allocatable :: ireach
        real, dimension(:), allocatable :: drainage_area
        real, dimension(:), allocatable :: bankfull

        !> Reference fields.
        real, dimension(:), allocatable :: lon
        real, dimension(:), allocatable :: lat

        !> Maps.
        integer, dimension(:, :), allocatable :: from_grid_xy
        integer, dimension(:), allocatable :: from_grid_x
        integer, dimension(:), allocatable :: from_grid_y
        integer, dimension(:), allocatable :: from_gru
        integer, dimension(:), allocatable :: from_riverclass
        integer, dimension(:), allocatable :: from_cell

        !> Indices.
        character(len = SHORT_FIELD_LENGTH) :: dim_name = ''
        integer :: dim_length = 0
    end type

    !> Description:
    !>  Container for a group of variables.
    !>
    !> Variables:
    !*  tile: Elemental computational unit 1:NML.
    !*  gru: By GRU 1:NTYPE.
    !*  grid: Grid combined from contributing GRUs (by ACLASS) 1:NA.
    type model_variables_groups

        !> Variable groups.
        type(model_variables_fields), pointer :: tile, gru, grid, basin

        !> Dimension sizes/counts.
        integer :: grid_x = 0
        integer :: grid_y = 0
        integer :: gru_count = 0
        integer :: riverclass_count = 0
        integer :: cell_count = 0
        integer :: active_cell_count = 0
        integer :: landtile_count = 0
        integer :: sol_count = 0
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

        !> 'mesh_io_constants' for 'NODATA' values.
        use mesh_io_constants, only: NO_DATA_REAL

        !> Input/output variables.
        type(model_variables_fields) group

        !> Output variables.
        integer, intent(out) :: ierr

        !> Initialize the return status.
        ierr = 0

        !> Meteorology/climatology variables.
        if (allocated(group%fsin)) group%fsin = 0.0
        if (allocated(group%fsvs)) group%fsvs = 0.0
        if (allocated(group%fsir)) group%fsir = 0.0
        if (allocated(group%fsdr)) group%fsdr = 0.0
        if (allocated(group%fsdff)) group%fsdff = 0.0
        if (allocated(group%flin)) group%flin = 0.0
        if (allocated(group%ta)) group%ta = 0.0
        if (allocated(group%qa)) group%qa = 0.0
        if (allocated(group%pres)) group%pres = 0.0
        if (allocated(group%uu)) group%uu = 0.0
        if (allocated(group%vv)) group%vv = 0.0
        if (allocated(group%uv)) group%uv = 0.0
        if (allocated(group%wdir)) group%wdir = 0.0
        if (allocated(group%prern)) group%prern = 0.0
        if (allocated(group%presno)) group%presno = 0.0
        if (allocated(group%pre)) group%pre = 0.0

        !> Canopy variables.
        if (allocated(group%lqwscan)) group%lqwscan = 0.0
        if (allocated(group%fzwscan)) group%fzwscan = 0.0
        if (allocated(group%cmas)) group%cmas = 0.0
        if (allocated(group%tacan)) group%tacan = 0.0
        if (allocated(group%qacan)) group%qacan = 0.0
        if (allocated(group%uvcan)) group%uvcan = 0.0
        if (allocated(group%tcan)) group%tcan = 0.0
        if (allocated(group%gro)) group%gro = 0.0

        !> Snow variables.
        if (allocated(group%fsno)) group%fsno = 0.0
        if (allocated(group%sno)) group%sno = 0.0
        if (allocated(group%rhosno)) group%rhosno = 0.0
!-        if (allocated(group%zsno)) group%zsno = 0.0
        if (allocated(group%lqwssno)) group%lqwssno = 0.0
        if (allocated(group%tsno)) group%tsno = 0.0
        if (allocated(group%albsno)) group%albsno = 0.0
        if (allocated(group%drainsno)) group%drainsno = 0.0

        !> Surface variables.
        if (allocated(group%albt)) group%albt = 0.0
        if (allocated(group%alvs)) group%alvs = 0.0
        if (allocated(group%alir)) group%alir = 0.0
        if (allocated(group%gte)) group%gte = 0.0
        if (allocated(group%zpnd)) group%zpnd = 0.0
!-        if (allocated(group%lqwspnd)) group%lqwspnd = 0.0
        if (allocated(group%tpnd)) group%tpnd = 0.0
        if (allocated(group%pndcaf)) group%pndcaf = 0.0
        if (allocated(group%potevp)) group%potevp = 0.0
        if (allocated(group%et)) group%et = 0.0
!-        if (allocated(group%evpb)) group%evpb = 0.0
!-        if (allocated(group%arrd)) group%arrd = 0.0
        if (allocated(group%ovrflw)) group%ovrflw = 0.0
        if (allocated(group%qevp)) group%qevp = 0.0
        if (allocated(group%qsens)) group%qsens = 0.0
        if (allocated(group%gzero)) group%gzero = 0.0
        if (allocated(group%tsfs)) group%tsfs = 0.0
        if (allocated(group%tsurf)) group%tsurf = 0.0

        !> Ice/glacier variables.
        if (allocated(group%lqwsice)) group%lqwsice = 0.0
        if (allocated(group%tice)) group%tice = 0.0

        !> Subsurface/soil variables.
        if (allocated(group%dzsol)) group%dzsol = 0.0
        if (allocated(group%dzsolhyd)) group%dzsolhyd = 0.0
        if (allocated(group%thlqsol)) group%thlqsol = 0.0
        if (allocated(group%thicsol)) group%thicsol = 0.0
        if (allocated(group%lqwssol)) group%lqwssol = 0.0
        if (allocated(group%fzwssol)) group%fzwssol = 0.0
        if (allocated(group%tsol)) group%tsol = 0.0
        if (allocated(group%gflx)) group%gflx = 0.0
        if (allocated(group%hcps)) group%hcps = 0.0
        if (allocated(group%hcpc)) group%hcpc = 0.0
        if (allocated(group%hcpg)) group%hcpg = 0.0
        if (allocated(group%tctopc)) group%tctopc = 0.0
        if (allocated(group%tcbotc)) group%tcbotc = 0.0
        if (allocated(group%tctopg)) group%tctopg = 0.0
        if (allocated(group%tcbotg)) group%tcbotg = 0.0
        if (allocated(group%latflw)) group%latflw = 0.0
        if (allocated(group%zsol)) group%zsol = 0.0
        if (allocated(group%zsolhyd)) group%zsolhyd = 0.0
        if (allocated(group%zsolsat)) group%zsolsat = 0.0
        if (allocated(group%ggeo)) group%ggeo = 0.0
        if (allocated(group%tbas)) group%tbas = 0.0
        if (allocated(group%drainsol)) group%drainsol = 0.0

        !> Groundwater/lower zone storage variables.
        if (allocated(group%rchg)) group%rchg = NO_DATA_REAL
        if (allocated(group%stggw)) group%stggw = NO_DATA_REAL
        if (allocated(group%lkg)) group%lkg = NO_DATA_REAL
!-        if (allocated(group%dzs)) group%dzs = 0.0

        !> Diagnostic variables.
!-        if (allocated(group%stge)) group%stge = 0.0
!-        if (allocated(group%stgw)) group%stgw = 0.0

        !> Routing variables.
        if (allocated(group%rff)) group%rff = NO_DATA_REAL
        if (allocated(group%qi)) group%qi = 0.0
        if (allocated(group%qo)) group%qo = 0.0
        if (allocated(group%stgch)) group%stgch = 0.0
        if (allocated(group%zlvl)) group%zlvl = 0.0
        if (allocated(group%div)) group%div = 0.0
        if (allocated(group%abstr)) group%abstr = 0.0

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
        if (.not. associated(group)) allocate(group)

        !> Meteorology/climatology variables.
!-        allocate(group%fsin(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%fsvs(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%fsir(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%fsdr(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%fsdff(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%flin(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%ta(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%qa(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%pres(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%uu(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%vv(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%uv(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%wdir(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%prern(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%presno(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%pre(n), stat = z); if (z /= 0) ierr = z

        !> Canopy variables.
        allocate(group%lqwscan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%fzwscan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%cmas(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tacan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qacan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%uvcan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tcan(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gro(n), stat = z); if (z /= 0) ierr = z

        !> Snow variables.
        allocate(group%fsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%sno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%rhosno(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%zsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%lqwssno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%albsno(n), stat = z); if (z /= 0) ierr = z
        allocate(group%drainsno(n), stat = z); if (z /= 0) ierr = z

        !> Surface variables.
        allocate(group%albt(n), stat = z); if (z /= 0) ierr = z
        allocate(group%alvs(n), stat = z); if (z /= 0) ierr = z
        allocate(group%alir(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gte(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zpnd(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%lqwspnd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tpnd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%pndcaf(n), stat = z); if (z /= 0) ierr = z
        allocate(group%potevp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%et(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%evpb(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%arrd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%ovrflw(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qevp(n), stat = z); if (z /= 0) ierr = z
        allocate(group%qsens(n), stat = z); if (z /= 0) ierr = z
        allocate(group%gzero(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tsfs(n, 4), stat = z); if (z /= 0) ierr = z
        allocate(group%tsurf(n), stat = z); if (z /= 0) ierr = z

        !> Ice/glacier variables.
        allocate(group%lqwsice(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tice(n), stat = z); if (z /= 0) ierr = z

        !> Subsurface/soil variables.
        allocate(group%dzsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%dzsolhyd(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thlqsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%thicsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%lqwssol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%fzwssol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%tsol(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%gflx(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%hcps(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%hcpc(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%hcpg(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%tctopc(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%tctopg(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%tcbotc(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%tcbotg(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%latflw(n, nsl), stat = z); if (z /= 0) ierr = z
        allocate(group%zsol(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zsolhyd(n), stat = z); if (z /= 0) ierr = z
        allocate(group%zsolsat(n), stat = z); if (z /= 0) ierr = z
        allocate(group%ggeo(n), stat = z); if (z /= 0) ierr = z
        allocate(group%tbas(n), stat = z); if (z /= 0) ierr = z
        allocate(group%drainsol(n), stat = z); if (z /= 0) ierr = z

        !> Groundwater/lower zone storage variables.
        allocate(group%rchg(n), stat = z); if (z /= 0) ierr = z
        allocate(group%stggw(n), stat = z); if (z /= 0) ierr = z
        allocate(group%lkg(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%dzs(n), stat = z); if (z /= 0) ierr = z

        !> Diagnostic variables.
!-        allocate(group%stge(n), stat = z); if (z /= 0) ierr = z
!-        allocate(group%stgw(n), stat = z); if (z /= 0) ierr = z

        !> Routing variables.
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
            if (associated(vs%tile)) then
                vs%tile%dim_length = shd%lc%NML
                vs%tile%dim_name = 'tile'
            end if
            call model_variables_group_reset(vs%tile, z); if (z /= 0) ierr = z
            call model_variables_group_allocate(vs%gru, shd%lc%NTYPE, shd%lc%IGND, z); if (z /= 0) ierr = z
            if (associated(vs%gru)) then
                vs%gru%dim_length = shd%lc%NTYPE
                vs%gru%dim_name = 'gru'
            end if
            call model_variables_group_reset(vs%gru, z); if (z /= 0) ierr = z
        end if
        if (ro%RUNGRID) then
            call model_variables_group_allocate(vs%grid, shd%NA, shd%lc%IGND, z); if (z /= 0) ierr = z
            if (associated(vs%grid)) then
                vs%grid%dim_length = shd%NA
                vs%grid%dim_name = 'grid'
            end if
            call model_variables_group_reset(vs%grid, z); if (z /= 0) ierr = z
            call model_variables_group_allocate(vs%basin, shd%NA, shd%lc%IGND, z); if (z /= 0) ierr = z
            if (associated(vs%basin)) then
                vs%basin%dim_length = shd%NA
                vs%basin%dim_name = 'basin'
            end if
            call model_variables_group_reset(vs%basin, z); if (z /= 0) ierr = z
        end if

    end subroutine

end module
