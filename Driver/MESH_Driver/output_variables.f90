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

    !> Keys for identifying output variable group.
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
        real, dimension(:, :), allocatable :: frws
        real, dimension(:, :), allocatable :: alws
        real, dimension(:), allocatable :: stgw

        !> Energy balance.
        real, dimension(:), allocatable :: cmas
        real, dimension(:), allocatable :: tcan
        real, dimension(:), allocatable :: tsno
        real, dimension(:), allocatable :: tpnd
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
    !>  Container for output variable groups and NO_DATA variables.
    !>
    !> Members:
    !*  NO_DATA: No data value (type: real).
    !*  NO_DATA_INT: No data value (type: integer).
    !*  ts: Per time-step.
    !*  tot: Total (e.g., accumulated).
    !*  dly: Daily.
    !*  hly: Hourly.
    !*  mly: Monthly.
    !*  yly: Yearly.
    type output_variables_container
        type(output_variables_group) ts, dly, hly, mly, yly, tot
        integer :: NO_DATA_INT = -1
        real :: NO_DATA = -1.0
    end type

    !*  out: Instance of container for output variable groups.
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
    !>  Allocate and initialize per time-step output variables. Assigns
    !>  frequency keys to the respective output variable groups.
    subroutine output_variables_init(shd)

        !> 'shd_variables' required for indices from 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams) :: shd

        !> Local variables.
        integer na, nml, s

        !> Indices for allocation.
        na = shd%NA
        nml = shd%lc%NML
        s = shd%lc%IGND

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            allocate(out%ts%pre(nml))
            allocate(out%ts%fsin(nml))
            allocate(out%ts%flin(nml))
            allocate(out%ts%ta(nml))
            allocate(out%ts%qa(nml))
            allocate(out%ts%pres(nml))
            allocate(out%ts%uv(nml))
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            allocate(out%ts%evap(nml))
            allocate(out%ts%pevp(nml))
            allocate(out%ts%evpb(nml))
            allocate(out%ts%arrd(nml))
            allocate(out%ts%rof(nml))
            allocate(out%ts%rofo(nml))
            allocate(out%ts%rofs(nml))
            allocate(out%ts%rofb(nml))
            allocate(out%ts%rcan(nml))
            allocate(out%ts%sncan(nml))
            allocate(out%ts%gro(nml))
            allocate(out%ts%sno(nml))
            allocate(out%ts%fsno(nml))
            allocate(out%ts%wsno(nml))
            allocate(out%ts%zpnd(nml))
            allocate(out%ts%pndw(nml))
            allocate(out%ts%lzs(nml))
            allocate(out%ts%dzs(nml))
            allocate(out%ts%thlq(nml, s))
            allocate(out%ts%lqws(nml, s))
            allocate(out%ts%thic(nml, s))
            allocate(out%ts%frws(nml, s))
            allocate(out%ts%alws(nml, s))
            allocate(out%ts%stgw(nml))
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            allocate(out%ts%cmas(nml))
            allocate(out%ts%tcan(nml))
            allocate(out%ts%tsno(nml))
            allocate(out%ts%tpnd(nml))
            allocate(out%ts%albt(nml))
            allocate(out%ts%fsout(nml))
            allocate(out%ts%flout(nml))
            allocate(out%ts%gte(nml))
            allocate(out%ts%qh(nml))
            allocate(out%ts%qe(nml))
            allocate(out%ts%gzero(nml))
            allocate(out%ts%gflx(nml, s))
            allocate(out%ts%tbar(nml, s))
            allocate(out%ts%tmax(nml, s))
            allocate(out%ts%tmin(nml, s))
            allocate(out%ts%ald(nml))
            allocate(out%ts%zod(nml, 1))
            allocate(out%ts%stge(nml))
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            allocate(out%ts%rff(na))
            allocate(out%ts%rchg(na))
            allocate(out%ts%qi(na))
            allocate(out%ts%stgch(na))
            allocate(out%ts%qo(na))
            allocate(out%ts%zlvl(na))
        end if

        !> Assign frequency keys.
        out%dly%freq = TKDLY
        out%hly%freq = TKHLY
        out%ts%freq = TKPTS
        out%mly%freq = TKMLY
        out%yly%freq = TKYLY
        out%tot%freq = TKTOT

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
        where (val == out%NO_DATA) field = out%NO_DATA_INT
    end subroutine

    !> Description:
    !>  Update the output variable from the per time-step values.
    !>  Only fields allocated are updated.
    subroutine output_variables_update_series(vars)

        !> 'model_dates' required for 'ic' variable (counter and time-stepping).
        use model_dates

        !> Input/output variable.
        type(output_variables_group) vars

        !> Local variables.
        integer its, dnts

        !> Determine the current time-step in the period.
        !> Assign values to pass to the update routine.
        dnts = 0
        its = 0
        select case (vars%freq)

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
        if (allocated(vars%rff)) call output_variables_update_field(vars%rff, out%ts%rff, its, dnts, 'sum')
        if (allocated(vars%rchg)) call output_variables_update_field(vars%rchg, out%ts%rchg, its, dnts, 'sum')
        if (allocated(vars%qi)) call output_variables_update_field(vars%qi, out%ts%qi, its, dnts, 'avg')
        if (allocated(vars%stgch)) call output_variables_update_field(vars%stgch, out%ts%stgch, its, dnts, 'avg')
        if (allocated(vars%qo)) call output_variables_update_field(vars%qo, out%ts%qo, its, dnts, 'avg')
!        if (allocated(vars%zlvl)) call output_variables_update_field(vars%zlvl, out%ts%zlvl, its, dnts, 'avg')

    end subroutine

    !> Description:
    !>  Update per time-step variables if not already updated by other
    !>  routines (e.g., if all values still equal the NO_DATA value).
    !>  Then update output variables of higher time frequencies from the
    !>  per time-step values.
    subroutine output_variables_update()

        !> 'control_variables' required to check for active modelling components.
        !> 'state_variables' required for 'stas' variable.
        use control_variables
        use state_variables

        !> Channels and routing.
        if (ro%RUNCHNL) then
            if (all(out%ts%qi == out%NO_DATA)) out%ts%qi = stas_grid%chnl%qi
            if (all(out%ts%stgch == out%NO_DATA)) out%ts%stgch = stas_grid%chnl%stg
            if (all(out%ts%qo == out%NO_DATA)) out%ts%qo = stas_grid%chnl%qo
!            if (all(out%ts%zlvl == out%NO_DATA)) out%ts%zlvl = stas_grid%chnl%zlvl
        end if

        !> Update other time frequencies from the per time-step values.
        call output_variables_update_series(out%dly)
        call output_variables_update_series(out%hly)
        call output_variables_update_series(out%mly)
        call output_variables_update_series(out%yly)
        call output_variables_update_series(out%tot)

    end subroutine

    !> Description:
    !>  Reset output variables. Variables are set to the NO_DATA value.
    subroutine output_variables_reset()

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            out%ts%pre = out%NO_DATA
            out%ts%fsin = out%NO_DATA
            out%ts%flin = out%NO_DATA
            out%ts%ta = out%NO_DATA
            out%ts%qa = out%NO_DATA
            out%ts%pres = out%NO_DATA
            out%ts%uv = out%NO_DATA
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            out%ts%evap = out%NO_DATA
            out%ts%pevp = out%NO_DATA
            out%ts%evpb = out%NO_DATA
            out%ts%arrd = out%NO_DATA
            out%ts%rof = out%NO_DATA
            out%ts%rofo = out%NO_DATA
            out%ts%rofs = out%NO_DATA
            out%ts%rofb = out%NO_DATA
            out%ts%rcan = out%NO_DATA
            out%ts%sncan = out%NO_DATA
            out%ts%gro = out%NO_DATA
            out%ts%sno = out%NO_DATA
            out%ts%fsno = out%NO_DATA
            out%ts%wsno = out%NO_DATA
            out%ts%zpnd = out%NO_DATA
            out%ts%pndw = out%NO_DATA
            out%ts%lzs = out%NO_DATA
            out%ts%dzs = out%NO_DATA
            out%ts%thlq = out%NO_DATA
            out%ts%lqws = out%NO_DATA
            out%ts%thic = out%NO_DATA
            out%ts%frws = out%NO_DATA
            out%ts%alws = out%NO_DATA
            out%ts%stgw = out%NO_DATA
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            out%ts%cmas = out%NO_DATA
            out%ts%tcan = out%NO_DATA
            out%ts%tsno = out%NO_DATA
            out%ts%tpnd = out%NO_DATA
            out%ts%albt = out%NO_DATA
            out%ts%fsout = out%NO_DATA
            out%ts%flout = out%NO_DATA
            out%ts%gte = out%NO_DATA
            out%ts%qh = out%NO_DATA
            out%ts%qe = out%NO_DATA
            out%ts%gzero = out%NO_DATA
            out%ts%gflx = out%NO_DATA
            out%ts%tbar = out%NO_DATA
            out%ts%tmax = out%NO_DATA
            out%ts%tmin = out%NO_DATA
            out%ts%ald = out%NO_DATA
            out%ts%zod = out%NO_DATA
            out%ts%stge = out%NO_DATA
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            out%ts%rff = out%NO_DATA
            out%ts%rchg = out%NO_DATA
            out%ts%qi = out%NO_DATA
            out%ts%stgch = out%NO_DATA
            out%ts%qo = out%NO_DATA
            out%ts%zlvl = out%NO_DATA
        end if

    end subroutine

end module
