module output_variables

    implicit none

    !> Description: Interface for 'output_variables_allocate'.
    !>  Allocates variables for storing output based on components
    !>  that are active in the model.
    interface output_variables_allocate
        module procedure output_variables_allocate_real_1d
        module procedure output_variables_allocate_real_2d
        module procedure output_variables_allocate_int_1d
    end interface

    !> Description: Interface for 'output_variables_update_field'.
    !>  Updates aggregated fields from per time-step output variables
    !>  based on components that are active in the model.
    interface output_variables_update_field
        module procedure output_variables_update_field_real_1d
        module procedure output_variables_update_field_real_2d
        module procedure output_variables_update_field_int_1d
    end interface

    !> Keys for identifying output time-series.
    !*  PTS: Per model time-step.
    !*  TOT: Total (e.g., accumulated).
    !*  DLY: Daily.
    !*  HLY: Hourly.
    !*  MLY: Monthly.
    !*  YLY: Yearly.
    integer, parameter :: TKPTS = 5
    integer, parameter :: TKTOT = 1
    integer, parameter :: TKDLY = 2
    integer, parameter :: TKHLY = 4
    integer, parameter :: TKMLY = 3
    integer, parameter :: TKYLY = 6

    !> Description:
    !>  Group containing output variables.
    type output_variables_group

        !> Meteorological forcing.
        real, dimension(:), allocatable :: pre
        real, dimension(:), allocatable :: fsin
        real, dimension(:), allocatable :: flin
        real, dimension(:), allocatable :: ta
        real, dimension(:), allocatable :: qa
        real, dimension(:), allocatable :: pres
        real, dimension(:), allocatable :: uv

        !> Water balance.
        real, dimension(:), allocatable :: evap
        real, dimension(:), allocatable :: pevp
        real, dimension(:), allocatable :: evpb
        real, dimension(:), allocatable :: arrd
        real, dimension(:), allocatable :: rof
        real, dimension(:), allocatable :: rofo
        real, dimension(:), allocatable :: rofs
        real, dimension(:), allocatable :: rofb
        real, dimension(:), allocatable :: rcan
        real, dimension(:), allocatable :: sncan
        real, dimension(:), allocatable :: gro
        real, dimension(:), allocatable :: sno
        real, dimension(:), allocatable :: fsno
        real, dimension(:), allocatable :: wsno
        real, dimension(:), allocatable :: zpnd
        real, dimension(:), allocatable :: pndw
        real, dimension(:), allocatable :: lzs
        real, dimension(:), allocatable :: dzs
        real, dimension(:, :), allocatable :: thlq
        real, dimension(:, :), allocatable :: lqws
        real, dimension(:, :), allocatable :: thic
        real, dimension(:, :), allocatable :: fzws
        real, dimension(:, :), allocatable :: alws
        real, dimension(:), allocatable :: stgw

        !> Energy balance.
        real, dimension(:), allocatable :: cmas
        real, dimension(:), allocatable :: tcan
        real, dimension(:), allocatable :: tsno
        real, dimension(:), allocatable :: tpnd
        real, dimension(:), allocatable :: alvs
        real, dimension(:), allocatable :: alir
        real, dimension(:), allocatable :: albt
        real, dimension(:), allocatable :: fsout
        real, dimension(:), allocatable :: flout
        real, dimension(:), allocatable :: gte
        real, dimension(:), allocatable :: qh
        real, dimension(:), allocatable :: qe
        real, dimension(:), allocatable :: gzero
        real, dimension(:, :), allocatable :: gflx
        real, dimension(:, :), allocatable :: tbar
        real, dimension(:, :), allocatable :: tmax
        real, dimension(:, :), allocatable :: tmin
        real, dimension(:), allocatable :: ald
        real, dimension(:, :), allocatable :: zod
        real, dimension(:), allocatable :: stge

        !> Channels and routing.
        real, dimension(:), allocatable :: rff
        real, dimension(:), allocatable :: rchg
        real, dimension(:), allocatable :: qi
        real, dimension(:), allocatable :: stgch
        real, dimension(:), allocatable :: qo
        real, dimension(:), allocatable :: zlvl

        !*  freq: To identify the time-stepping.
        integer freq
    end type

    !> Description:
    !>  Group container for output variable series.
    !>
    !> Members:
    !*  ts: Per time-step.
    !*  tot: Total (e.g., accumulated).
    !*  dly: Daily.
    !*  hly: Hourly.
    !*  mly: Monthly.
    !*  yly: Yearly.
    type output_variables_group_container
        type(output_variables_group) ts, tot, dly, hly, mly, yly
    end type

    !> Description:
    !>  Container for output objects including 'tile' and 'grid'
    !>  groups and NO_DATA values.
    !>
    !> Members:
    !*  grid: Instance of group for grid-based output.
    !*  tile: Instance of group for tile-based output.
    !*  NO_DATA: No data value (type: real).
    !*  NO_DATA_INT: No data value (type: integer).
    type output_variables_container
        type(output_variables_group_container) grid, tile
        integer :: NO_DATA_INT = -1
        real :: NO_DATA = -1.0
    end type

    !*  out: Instance of output variables.
    type(output_variables_container), save :: out

    contains

    !> Description:
    !>  Allocate single dimension vector of type real to 'n'.
    !>  Initializes 'field' to zero unless already allocated.
    !>
    !> Input:
    !*  n: dimension to allocate 'field'.
    !>
    !> Input/output:
    !*  field: Variable to allocate; returns if allocated.
    !>
    subroutine output_variables_allocate_real_1d(field, n)
        integer n
        real, allocatable :: field(:)
        if (allocated(field)) return
        allocate(field(n))
        field = 0.0
    end subroutine

    !> Description:
    !>  Allocate two dimension array of type real to 'n' and 'j'.
    !>  Initializes 'field' to zero unless already allocated.
    !>
    !> Input:
    !*  n: dimension to allocate 'field' in the first dimension.
    !*  j: dimension to allocate 'field' in the second dimension.
    !>
    !> Input/output:
    !*  field: Variable to allocate; returns if allocated.
    !>
    subroutine output_variables_allocate_real_2d(field, n, j)
        integer n, j
        real, allocatable :: field(:, :)
        if (allocated(field)) return
        allocate(field(n, j))
        field = 0.0
    end subroutine

    !> Description:
    !>  Allocate single dimension vector of type integer to 'n'.
    !>  Initializes 'field' to zero unless already allocated.
    !>
    !> Input:
    !*  n: dimension to allocate 'field'.
    !>
    !> Input/output:
    !*  field: Variable to allocate; returns if allocated.
    !>
    subroutine output_variables_allocate_int_1d(field, n)
        integer n
        integer, allocatable :: field(:)
        if (allocated(field)) return
        allocate(field(n))
        field = 0
    end subroutine

    !> Description:
    !>  Allocate and initialize the per time-step output variables of
    !>  the provided group. Assign frequency keys to the time-series.
    subroutine output_variables_init_group(shd, group, n)

        !> 'shd_variables' required for indices from 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams) :: shd
        integer n

        !> Input/output variables.
        type(output_variables_group_container) group

        !> Local variables.
        integer s

        !> Indices for allocation.
        s = shd%lc%IGND

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            allocate(group%ts%pre(n))
            allocate(group%ts%fsin(n))
            allocate(group%ts%flin(n))
            allocate(group%ts%ta(n))
            allocate(group%ts%qa(n))
            allocate(group%ts%pres(n))
            allocate(group%ts%uv(n))
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            allocate(group%ts%evap(n))
            allocate(group%ts%pevp(n))
            allocate(group%ts%evpb(n))
            allocate(group%ts%arrd(n))
            allocate(group%ts%rof(n))
            allocate(group%ts%rofo(n))
            allocate(group%ts%rofs(n))
            allocate(group%ts%rofb(n))
            allocate(group%ts%rcan(n))
            allocate(group%ts%sncan(n))
            allocate(group%ts%gro(n))
            allocate(group%ts%sno(n))
            allocate(group%ts%fsno(n))
            allocate(group%ts%wsno(n))
            allocate(group%ts%zpnd(n))
            allocate(group%ts%pndw(n))
            allocate(group%ts%lzs(n))
            allocate(group%ts%dzs(n))
            allocate(group%ts%thlq(n, s))
            allocate(group%ts%lqws(n, s))
            allocate(group%ts%thic(n, s))
            allocate(group%ts%fzws(n, s))
            allocate(group%ts%alws(n, s))
            allocate(group%ts%stgw(n))
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            allocate(group%ts%cmas(n))
            allocate(group%ts%tcan(n))
            allocate(group%ts%tsno(n))
            allocate(group%ts%tpnd(n))
            allocate(group%ts%alvs(n))
            allocate(group%ts%alir(n))
            allocate(group%ts%albt(n))
            allocate(group%ts%fsout(n))
            allocate(group%ts%flout(n))
            allocate(group%ts%gte(n))
            allocate(group%ts%qh(n))
            allocate(group%ts%qe(n))
            allocate(group%ts%gzero(n))
            allocate(group%ts%gflx(n, s))
            allocate(group%ts%tbar(n, s))
            allocate(group%ts%tmax(n, s))
            allocate(group%ts%tmin(n, s))
            allocate(group%ts%ald(n))
            allocate(group%ts%zod(n, 1))
            allocate(group%ts%stge(n))
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            allocate(group%ts%rff(n))
            allocate(group%ts%rchg(n))
            allocate(group%ts%qi(n))
            allocate(group%ts%stgch(n))
            allocate(group%ts%qo(n))
            allocate(group%ts%zlvl(n))
        end if

        !> Assign frequency keys.
        group%dly%freq = TKDLY
        group%hly%freq = TKHLY
        group%ts%freq = TKPTS
        group%mly%freq = TKMLY
        group%yly%freq = TKYLY
        group%tot%freq = TKTOT

    end subroutine

    !> Description:
    !>  Allocate and initialize the per time-step output variables of
    !>  all groups. Assign frequency keys to the time-series.
    subroutine output_variables_init(shd)

        !> 'shd_variables' required for indices from 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams) :: shd

        !> Tile-based.
        if (ro%RUNTILE) call output_variables_init_group(shd, out%tile, shd%lc%NML)

        !> Grid-based.
        if (ro%RUNGRID) call output_variables_init_group(shd, out%grid, shd%NA)

    end subroutine

    !> Description:
    !>  Update 'field' from 'val' using the provided function 'fn'.
    !>  Reset 'field' if the first time-step of the period ('its' == 1).
    !>  Calculate an average provided the 'avg' function ('fn' == 'avg')
    !>  if the last time-step of period ('dnts' > 0).
    !>  Assign the NO_DATA value at indices where 'val' contains the
    !>  NO_DATA value.
    subroutine output_variables_update_field_real_1d(field, val, its, dnts, fn)
        integer its, dnts
        real, dimension(:) :: field, val
        character(len = *) fn
        if (its == 1) field = 0.0
        select case (fn)
            case ('sum')
                field = field + val
            case ('avg')
                field = field + val
                if (dnts > 0) field = field/dnts
            case ('max')
                field = max(field, val)
            case ('min')
                field = min(field, val)
            case default
                field = val
        end select
        where (val == out%NO_DATA) field = out%NO_DATA
    end subroutine

    !> Description:
    !>  Update 'field' from 'val' using the provided function 'fn'.
    !>  Reset 'field' if the first time-step of the period ('its' == 1).
    !>  Calculate an average provided the 'avg' function ('fn' == 'avg')
    !>  if the last time-step of period ('dnts' > 0).
    !>  Assign the NO_DATA value at indices where 'val' contains the
    !>  NO_DATA value.
    subroutine output_variables_update_field_real_2d(field, val, its, dnts, fn)
        integer its, dnts
        real, dimension(:, :) :: field, val
        character(len = *) fn
        if (its == 1) field = 0.0
        select case (fn)
            case ('sum')
                field = field + val
            case ('avg')
                field = field + val
                if (dnts > 0) field = field/dnts
            case ('max')
                field = max(field, val)
            case ('min')
                field = min(field, val)
            case default
                field = val
        end select
        where (val == out%NO_DATA) field = out%NO_DATA
    end subroutine

    !> Description:
    !>  Update 'field' from 'val' using the provided function 'fn'.
    !>  Reset 'field' if the first time-step of the period ('its' == 1).
    !>  Calculate an average provided the 'avg' function ('fn' == 'avg')
    !>  if the last time-step of period ('dnts' > 0).
    !>  Assign the NO_DATA value at indices where 'val' contains the
    !>  NO_DATA value.
    subroutine output_variables_update_field_int_1d(field, val, its, fn)
        integer its
        integer, dimension(:) :: field, val
        character(len = *) fn
        if (its == 1) field = 0
        select case (fn)
            case ('sum')
                field = field + val
            case ('max')
                field = max(field, val)
            case ('min')
                field = min(field, val)
            case default
                field = val
        end select
        where (val == out%NO_DATA_INT) field = out%NO_DATA_INT
    end subroutine

    !> Description:
    !>  Update the output variable from the per time-step values.
    !>  Only fields allocated are updated.
    subroutine output_variables_update_series(series, group)

        !> 'model_dates' required for 'ic' variable (counter and time-stepping).
        use model_dates

        !> Input/output variables.
        type(output_variables_group) series
        type(output_variables_group_container) group

        !> Local variables.
        integer its, dnts

        !> Determine the current time-step in the period.
        !> Assign values to pass to the update routine.
        dnts = 0
        its = 0
        select case (series%freq)

            !> Daily.
            case (TKDLY)
                if (mod(ic%ts_daily, 3600/ic%dts*24) == 0) dnts = ic%ts_daily
                its = ic%ts_daily

            !> Hourly.
            case (TKHLY)
                if (mod(ic%ts_hourly, 3600/ic%dts) == 0) dnts = ic%ts_hourly
                its = ic%ts_hourly

            !> Monthly.
            case (TKMLY)

            !> Yearly.
            case (TKYLY)

            !> Total (e.g., accumulated).
            case (TKTOT)
                dnts = ic%ts_count
        end select

        !> Channels and routing.
        if (allocated(series%rff)) call output_variables_update_field(series%rff, group%ts%rff, its, dnts, 'sum')
        if (allocated(series%rchg)) call output_variables_update_field(series%rchg, group%ts%rchg, its, dnts, 'sum')
        if (allocated(series%qi)) call output_variables_update_field(series%qi, group%ts%qi, its, dnts, 'avg')
        if (allocated(series%stgch)) call output_variables_update_field(series%stgch, group%ts%stgch, its, dnts, 'avg')
        if (allocated(series%qo)) call output_variables_update_field(series%qo, group%ts%qo, its, dnts, 'avg')
!        if (allocated(series%zlvl)) call output_variables_update_field(series%zlvl, group%ts%zlvl, its, dnts, 'avg')

    end subroutine

    !> Description:
    !>  Update per time-step variables if not already updated by other
    !>  routines (e.g., if all values still equal the NO_DATA value) of
    !>  all groups. Update output variables of higher time frequencies
    !>  (from the per time-step series).
    subroutine output_variables_update(shd, cm)

        !> 'shd_variables' required for indices from 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'climate_forcing' required for the 'cm' variable.
        !> 'state_variables' required for 'stas' variable.
        use shd_variables
        use control_variables
        use climate_forcing
        use state_variables

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Tile-based.
        if (ro%RUNTILE) then
            call output_variables_update_series(out%tile%dly, out%tile)
            call output_variables_update_series(out%tile%hly, out%tile)
            call output_variables_update_series(out%tile%mly, out%tile)
            call output_variables_update_series(out%tile%yly, out%tile)
            call output_variables_update_series(out%tile%tot, out%tile)
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            if (all(out%grid%ts%qi == out%NO_DATA)) out%grid%ts%qi = stas_grid%chnl%qi
            if (all(out%grid%ts%stgch == out%NO_DATA)) out%grid%ts%stgch = stas_grid%chnl%stg
            if (all(out%grid%ts%qo == out%NO_DATA)) out%grid%ts%qo = stas_grid%chnl%qo
!            if (all(out%grid%ts%zlvl == out%NO_DATA)) out%grid%ts%zlvl = stas_grid%chnl%zlvl
        end if

        !> Grid-based.
        if (ro%RUNGRID) then
            call output_variables_update_series(out%grid%dly, out%grid)
            call output_variables_update_series(out%grid%hly, out%grid)
            call output_variables_update_series(out%grid%mly, out%grid)
            call output_variables_update_series(out%grid%yly, out%grid)
            call output_variables_update_series(out%grid%tot, out%grid)
        end if

    end subroutine

    !> Description:
    !>  Reset output variables of the provided group. Set variables to
    !>  'val' (e.g., NO_DATA value).
    subroutine output_variables_reset_group(group, val)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input variables.
        real val

        !> Input/output variables.
        type(output_variables_group_container) group

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            group%ts%pre = val
            group%ts%fsin = val
            group%ts%flin = val
            group%ts%ta = val
            group%ts%qa = val
            group%ts%pres = val
            group%ts%uv = val
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            group%ts%evap = val
            group%ts%pevp = val
            group%ts%evpb = val
            group%ts%arrd = val
            group%ts%rof = val
            group%ts%rofo = val
            group%ts%rofs = val
            group%ts%rofb = val
            group%ts%rcan = val
            group%ts%sncan = val
            group%ts%gro = val
            group%ts%sno = val
            group%ts%fsno = val
            group%ts%wsno = val
            group%ts%zpnd = val
            group%ts%pndw = val
            group%ts%lzs = val
            group%ts%dzs = val
            group%ts%thlq = val
            group%ts%lqws = val
            group%ts%thic = val
            group%ts%fzws = val
            group%ts%alws = val
            group%ts%stgw = val
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            group%ts%cmas = val
            group%ts%tcan = val
            group%ts%tsno = val
            group%ts%tpnd = val
            group%ts%alvs = val
            group%ts%alir = val
            group%ts%albt = val
            group%ts%fsout = val
            group%ts%flout = val
            group%ts%gte = val
            group%ts%qh = val
            group%ts%qe = val
            group%ts%gzero = val
            group%ts%gflx = val
            group%ts%tbar = val
            group%ts%tmax = val
            group%ts%tmin = val
            group%ts%ald = val
            group%ts%zod = val
            group%ts%stge = val
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            group%ts%rff = val
            group%ts%rchg = val
            group%ts%qi = val
            group%ts%stgch = val
            group%ts%qo = val
            group%ts%zlvl = val
        end if

    end subroutine

    !> Description:
    !>  Reset output variables of all groups. Set variables to the
    !>  NO_DATA value.
    subroutine output_variables_reset()

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Tile-based.
        if (ro%RUNTILE) call output_variables_reset_group(out%tile, out%NO_DATA)

        !> Grid-based.
        if (ro%RUNGRID) call output_variables_reset_group(out%grid, out%NO_DATA)

    end subroutine

end module
