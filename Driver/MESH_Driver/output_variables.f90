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
    subroutine output_variables_init(shd, cm)

        !> 'shd_variables' required for indices from 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'climate_forcing' required for the 'cm' variable.
        use shd_variables
        use control_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Tile-based.
        if (ro%RUNTILE) then
            call output_variables_init_group(shd, out%tile, shd%lc%NML)
            call output_variables_reset_group(out%tile)
            call output_variables_update_tile(shd, cm)
        end if

        !> Grid-based.
        if (ro%RUNGRID) then
            call output_variables_init_group(shd, out%grid, shd%NA)
            call output_variables_reset_group(out%grid)
            call output_variables_update_grid(shd, cm)
        end if

    end subroutine

    !> Description:
    !>  Update per time-step output variables for tile variables.
    subroutine output_variables_update_tile(shd, cm)

        !> 'shd_variables' required for indices from 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'state_variables' required for the 'stas' variable.
        !> 'climate_forcing' required for the 'cm' variable.
        use shd_variables
        use control_variables
        use state_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        real nv
        real :: RHOW = 1000.0, RHOICE = 917.0, TFREZ = 273.16

        !> NO_DATA value
        nv = out%NO_DATA

        !> Meteorological forcing.
        !> Climate variables are not allocated by group so must check 'allocated' status.
        if (ro%RUNCLIM) then
            if (allocated(cm%dat(ck%RT)%GAT)) then
                where (out%tile%ts%pre == nv) out%tile%ts%pre = cm%dat(ck%RT)%GAT
            end if
            if (allocated(cm%dat(ck%FB)%GAT)) then
                where (out%tile%ts%fsin == nv) out%tile%ts%fsin = cm%dat(ck%FB)%GAT
            end if
            if (allocated(cm%dat(ck%FI)%GAT)) then
                where (out%tile%ts%flin == nv) out%tile%ts%flin = cm%dat(ck%FI)%GAT
            end if
            if (allocated(cm%dat(ck%TT)%GAT)) then
                where (out%tile%ts%ta == nv) out%tile%ts%ta = cm%dat(ck%TT)%GAT
            end if
            if (allocated(cm%dat(ck%HU)%GAT)) then
                where (out%tile%ts%qa == nv) out%tile%ts%qa = cm%dat(ck%HU)%GAT
            end if
            if (allocated(cm%dat(ck%P0)%GAT)) then
                where (out%tile%ts%pres == nv) out%tile%ts%pres = cm%dat(ck%P0)%GAT
            end if
            if (allocated(cm%dat(ck%UV)%GAT)) then
                where (out%tile%ts%uv == nv) out%tile%ts%uv = cm%dat(ck%UV)%GAT
            end if
        end if

        !> Water balance.
        !> 'stas' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNBALWB) then
            where (out%tile%ts%evap == nv) out%tile%ts%evap = stas%sfc%evap
            where (out%tile%ts%pevp == nv) out%tile%ts%pevp = stas%sfc%pevp
            where (out%tile%ts%evpb == nv) out%tile%ts%evpb = stas%sfc%evpb
            where (out%tile%ts%arrd == nv) out%tile%ts%arrd = stas%sfc%arrd
            where (out%tile%ts%rof == nv)
                out%tile%ts%rof = (stas%sfc%rofo + stas%sl%rofs + stas%lzs%rofb + stas%dzs%rofb)
            end where
            where (out%tile%ts%rofo == nv) out%tile%ts%rofo = stas%sfc%rofo
            where (out%tile%ts%rofs == nv) out%tile%ts%rofs = stas%sl%rofs
            where (out%tile%ts%rofb == nv) out%tile%ts%rofb = (stas%lzs%rofb + stas%dzs%rofb)
            where (out%tile%ts%rcan == nv) out%tile%ts%rcan = stas%cnpy%rcan
            where (out%tile%ts%sncan == nv) out%tile%ts%sncan = stas%cnpy%sncan
            where (out%tile%ts%gro == nv) out%tile%ts%gro = stas%cnpy%gro
            where (out%tile%ts%sno == nv) out%tile%ts%sno = stas%sno%sno
            where (out%tile%ts%fsno == nv) out%tile%ts%fsno = stas%sno%fsno
            where (out%tile%ts%wsno == nv) out%tile%ts%wsno = stas%sno%wsno
            where (out%tile%ts%zpnd == nv) out%tile%ts%zpnd = stas%sfc%zpnd
            where (out%tile%ts%pndw == nv) out%tile%ts%pndw = stas%sfc%zpnd*RHOW
            where (out%tile%ts%lzs == nv) out%tile%ts%lzs = stas%lzs%ws
            where (out%tile%ts%dzs == nv) out%tile%ts%dzs = stas%dzs%ws
            where (out%tile%ts%thlq == nv) out%tile%ts%thlq = stas%sl%thlq
            where (out%tile%ts%lqws == nv) out%tile%ts%lqws = stas%sl%thlq*stas%sl%delzw*RHOW
            where (out%tile%ts%thic == nv) out%tile%ts%thic = stas%sl%thic
            where (out%tile%ts%fzws == nv) out%tile%ts%fzws = stas%sl%thic*stas%sl%delzw*RHOICE
            where (out%tile%ts%alws == nv) out%tile%ts%alws = (stas%sl%thlq*RHOW + stas%sl%thic*RHOICE)*stas%sl%delzw
!            where (out%tile%ts%stgw == nv) out%tile%ts%stgw =
        end if

        !> Energy balance.
        !> 'stas' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNBALEB) then
            where (out%tile%ts%cmas == nv) out%tile%ts%cmas = stas%cnpy%cmas
            where (out%tile%ts%tcan == nv) out%tile%ts%tcan = stas%cnpy%tcan
            where (out%tile%ts%tsno == nv) out%tile%ts%tsno = stas%sno%tsno
            where (out%tile%ts%tpnd == nv) out%tile%ts%tpnd = stas%sfc%tpnd
            where (out%tile%ts%albt == nv) out%tile%ts%albt = stas%sfc%albt
            where (out%tile%ts%alvs == nv) out%tile%ts%alvs = stas%sfc%alvs
            where (out%tile%ts%alir == nv) out%tile%ts%alir = stas%sfc%alir
            if (allocated(cm%dat(ck%FB)%GAT)) then
                where (out%tile%ts%fsout == nv) out%tile%ts%fsout = cm%dat(ck%FB)%GAT*(1.0 - stas%sfc%albt)
            end if
            where (out%tile%ts%gte == nv) out%tile%ts%gte = stas%sfc%gte
            where (out%tile%ts%flout == nv) out%tile%ts%flout = 5.66796E-8*stas%sfc%gte**4
            where (out%tile%ts%qh == nv) out%tile%ts%qh = stas%sfc%hfs
            where (out%tile%ts%qe == nv) out%tile%ts%qe = stas%sfc%qevp
            where (out%tile%ts%gzero == nv) out%tile%ts%gzero = stas%sfc%gzero
            where (out%tile%ts%gflx == nv) out%tile%ts%gflx = stas%sl%gflx
            where (out%tile%ts%tbar == nv) out%tile%ts%tbar = stas%sl%tbar
            where (out%tile%ts%tmax == nv) out%tile%ts%tmax = max(out%tile%ts%tmax, stas%sl%tbar)
            where (out%tile%ts%tmin == nv) out%tile%ts%tmin = min(out%tile%ts%tmin, stas%sl%tbar)
!            where (out%tile%ts%ald == nv) out%tile%ts%ald =
!            where (out%tile%ts%zod == nv) out%tile%ts%zod =
!            where (out%tile%ts%stge == nv) out%tile%ts%stge =
        end if

    end subroutine

    !> Description:
    !>  Update per time-step output variables for grid variables.
    !>  Aggregate from tile to grid where necessary.
    !>  Temporary arrays are used because resetting the output variable
    !>  breaks the check against the NO_DATA value (for the case when
    !>  the variable has been updated by other routines).
    subroutine output_variables_update_grid(shd, cm)

        !> 'shd_variables' required for indices from 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'state_variables' required for the 'stas' variable.
        !> 'climate_forcing' required for the 'cm' variable.
        use shd_variables
        use control_variables
        use state_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer n, s, k, i
        real nv, fgru
        real :: RHOW = 1000.0, RHOICE = 917.0, TFREZ = 273.16
        real, dimension(:), allocatable :: &
            pre, fsin, flin, ta, qa, pres, uv, &
            evap, pevp, evpb, arrd, rof, rofo, rofs, rofb, &
            rcan, sncan, gro, sno, fsno, wsno, zpnd, lzs, dzs, &
            cmas, tcan, tsno, tpnd, &
            albt, alvs, alir, gte, qh, qe, gzero, &
            fcnpy, fsncov, fzpnd, &
            rff, rchg
        real, dimension(:, :), allocatable :: &
            thlq, lqws, thic, fzws, &
            gflx, tbar

        !> NO_DATA value.
        nv = out%NO_DATA

        !> Allocate and intialize temporary accumulators.
        n = shd%NA
        s = shd%lc%IGND

        !> Meteorological forcing.
        !> Climate variables are not allocated by group so must check 'allocated' status.
        if (ro%RUNCLIM) then
            allocate(pre(n), fsin(n), flin(n), ta(n), qa(n), pres(n), uv(n))
            pre = 0.0; fsin = 0.0; flin = 0.0; ta = 0.0; qa = 0.0; pres = 0.0; uv = 0.0
        end if

        !> Counters for partial averaging.
        if (ro%RUNBALWB .or. ro%RUNBALEB) then
            allocate(fcnpy(n), fsncov(n), fzpnd(n))
            fcnpy = 0.0; fsncov = 0.0; fzpnd = 0.0
        end if

        !> Water balance.
        !> 'stas' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNBALWB) then
            allocate( &
                evap(n), pevp(n), evpb(n), arrd(n), rof(n), rofo(n), rofs(n), rofb(n), &
                rcan(n), sncan(n), gro(n), sno(n), fsno(n), wsno(n), zpnd(n), lzs(n), dzs(n), &
                thlq(n, s), lqws(n, s), thic(n, s), fzws(n, s))
            evap = 0.0; pevp = 0.0; evpb = 0.0; arrd = 0.0; rof = 0.0; rofo = 0.0; rofs = 0.0; rofb = 0.0
            rcan = 0.0; sncan = 0.0; gro = 0.0; sno = 0.0; fsno = 0.0; wsno = 0.0; zpnd = 0.0; lzs = 0.0; dzs = 0.0
            thlq = 0.0; lqws = 0.0; thic = 0.0; fzws = 0.0
        end if

        !> Energy balance.
        !> 'stas' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNBALEB) then
            allocate( &
                cmas(n), tcan(n), tsno(n), tpnd(n), &
                albt(n), alvs(n), alir(n), gte(n), qh(n), qe(n), gzero(n), &
                gflx(n, s), tbar(n, s))
            cmas = 0.0; tcan = 0.0; tsno = 0.0; tpnd = 0.0
            albt = 0.0; alvs = 0.0; alir = 0.0; gte = 0.0; qh = 0.0; qe = 0.0; gzero = 0.0
            gflx = 0.0; tbar = 0.0
        end if

        !> Channels and routing.
        !> 'stas_grid' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNCHNL) then
            allocate(rff(n), rchg(n))
            rff = 0.0; rchg = 0.0
        end if

        !> Update grid variables from tiles.
        do k = 1, shd%lc%NML

            !> Indices.
            i = shd%lc%ILMOS(k)
            fgru = shd%lc%ACLASS(i, shd%lc%JLMOS(k))

            !> Meteorological forcing.
            !> Climate variables are not allocated by group so must check 'allocated' status.
            if (ro%RUNCLIM) then
                if (allocated(cm%dat(ck%RT)%GAT)) pre(i) = pre(i) + cm%dat(ck%RT)%GAT(k)*fgru
                if (allocated(cm%dat(ck%FB)%GAT)) fsin(i) = fsin(i) + cm%dat(ck%FB)%GAT(k)*fgru
                if (allocated(cm%dat(ck%FI)%GAT)) flin(i) = flin(i) + cm%dat(ck%FI)%GAT(k)*fgru
                if (allocated(cm%dat(ck%TT)%GAT)) ta(i) = ta(i) + cm%dat(ck%TT)%GAT(k)*fgru
                if (allocated(cm%dat(ck%HU)%GAT)) qa(i) = qa(i) + cm%dat(ck%HU)%GAT(k)*fgru
                if (allocated(cm%dat(ck%P0)%GAT)) pres(i) = pres(i) + cm%dat(ck%P0)%GAT(k)*fgru
                if (allocated(cm%dat(ck%UV)%GAT)) uv(i) = uv(i) + cm%dat(ck%UV)%GAT(k)*fgru
            end if

            !> Counters for partial averaging.
            if (ro%RUNBALWB .or. ro%RUNBALEB) then
                fcnpy(i) = fcnpy(i) + fgru
                fsncov(i) = fsncov(i) + fgru
                fzpnd(i) = fzpnd(i) + fgru
            end if

            !> Water balance.
            !> 'stas' variables are allocated by group so 'allocated' status is assumed.
            if (ro%RUNBALWB) then
                evap(i) = evap(i) + stas%sfc%evap(k)*fgru
                pevp(i) = pevp(i) + stas%sfc%pevp(k)*fgru
                evpb(i) = evpb(i) + stas%sfc%evpb(k)*fgru
                arrd(i) = arrd(i) + stas%sfc%arrd(k)*fgru
                rof(i) = rof(i) + (stas%sfc%rofo(k) + stas%sl%rofs(k) + stas%lzs%rofb(k) + stas%dzs%rofb(k))*fgru
                rofo(i) = rofo(i) + stas%sfc%rofo(k)*fgru
                rofs(i) = rofs(i) + stas%sl%rofs(k)*fgru
                rofb(i) = rofb(i) + (stas%lzs%rofb(k) + stas%dzs%rofb(k))*fgru
                rcan(i) = rcan(i) + stas%cnpy%rcan(k)*fgru
                sncan(i) = sncan(i) + stas%cnpy%sncan(k)*fgru
                gro(i) = gro(i) + stas%cnpy%gro(k)*fgru
                sno(i) = sno(i) + stas%sno%sno(k)*fgru
                fsno(i) = fsno(i) + stas%sno%fsno(k)*fgru
                wsno(i) = wsno(i) + stas%sno%wsno(k)*fgru
                zpnd(i) = zpnd(i) + stas%sfc%zpnd(k)*fgru
                lzs(i) = lzs(i) + stas%lzs%ws(k)*fgru
                dzs(i) = dzs(i) + stas%dzs%ws(k)*fgru
                thlq(i, :) = thlq(i, :) + stas%sl%thlq(k, :)*fgru
                lqws(i, :) = lqws(i, :) + stas%sl%thlq(k, :)*stas%sl%delzw(k, :)*RHOW
                thic(i, :) = thic(i, :) + stas%sl%thic(k, :)*fgru
                fzws(i, :) = fzws(i, :) + stas%sl%thic(k, :)*stas%sl%delzw(k, :)*RHOICE
            end if

            !> Energy balance.
            !> 'stas' variables are allocated by group so 'allocated' status is assumed.
            if (ro%RUNBALEB) then
                cmas(i) = cmas(i) + stas%cnpy%cmas(k)*fgru
                tcan(i) = tcan(i) + stas%cnpy%tcan(k)*fgru
                tsno(i) = tsno(i) + stas%sno%tsno(k)*fgru
                tpnd(i) = tpnd(i) + stas%sfc%tpnd(k)*fgru
                albt(i) = albt(i) + stas%sfc%albt(k)*fgru
                alvs(i) = alvs(i) + stas%sfc%alvs(k)*fgru
                alir(i) = alir(i) + stas%sfc%alir(k)*fgru
                gte(i) = gte(i) + stas%sfc%gte(k)*fgru
                qh(i) = qh(i) + stas%sfc%hfs(k)*fgru
                qe(i) = qe(i) + stas%sfc%qevp(k)*fgru
                gzero(i) = gzero(i) + stas%sfc%gzero(k)*fgru
                gflx(i, :) = gflx(i, :) + stas%sl%gflx(k, :)*fgru
                tbar(i, :) = tbar(i, :) + stas%sl%tbar(k, :)*fgru
            end if

            !> Channels and routing.
            !> 'stas_grid' variables are allocated by group so 'allocated' status is assumed.
            if (ro%RUNCHNL) then
                rff(i) = rff(i) + (stas%sfc%rofo(k) + stas%sl%rofs(k))*fgru
                rchg(i) = rchg(i) + (stas%lzs%rofb(k) + stas%dzs%rofb(k))*fgru
            end if
        end do

        !> Assign back to output variables.

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            where (out%grid%ts%pre == nv) out%grid%ts%pre = pre
            where (out%grid%ts%fsin == nv) out%grid%ts%fsin = fsin
            where (out%grid%ts%flin == nv) out%grid%ts%flin = flin
            where (out%grid%ts%ta == nv) out%grid%ts%ta = ta
            where (out%grid%ts%qa == nv) out%grid%ts%qa = qa
            where (out%grid%ts%pres == nv) out%grid%ts%pres = pres
            where (out%grid%ts%uv == nv) out%grid%ts%uv = uv
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            where (out%grid%ts%evap == nv) out%grid%ts%evap = evap
            where (out%grid%ts%pevp == nv) out%grid%ts%pevp = pevp
            where (out%grid%ts%evpb == nv) out%grid%ts%evpb = evpb
            where (out%grid%ts%arrd == nv) out%grid%ts%arrd = arrd
            where (out%grid%ts%rof == nv) out%grid%ts%rof = rof
            where (out%grid%ts%rofo == nv) out%grid%ts%rofo = rofo
            where (out%grid%ts%rofs == nv) out%grid%ts%rofs = rofs
            where (out%grid%ts%rofb == nv) out%grid%ts%rofb = rofb
            where (out%grid%ts%rcan == nv) out%grid%ts%rcan = rcan
            where (out%grid%ts%sncan == nv) out%grid%ts%sncan = sncan
            where (out%grid%ts%gro == nv .and. fcnpy > 0.0) out%grid%ts%gro = gro/fcnpy
            where (out%grid%ts%sno == nv) out%grid%ts%sno = sno
            where (out%grid%ts%fsno == nv) out%grid%ts%fsno = fsno
            where (out%grid%ts%wsno == nv) out%grid%ts%wsno = wsno
            where (out%grid%ts%zpnd == nv) out%grid%ts%zpnd = zpnd
            where (out%grid%ts%pndw == nv) out%grid%ts%pndw = zpnd*RHOW
            where (out%grid%ts%lzs == nv) out%grid%ts%lzs = lzs
            where (out%grid%ts%dzs == nv) out%grid%ts%dzs = dzs
            where (out%grid%ts%thlq == nv) out%grid%ts%thlq = thlq
            where (out%grid%ts%lqws == nv) out%grid%ts%lqws = lqws
            where (out%grid%ts%thic == nv) out%grid%ts%thic = thic
            where (out%grid%ts%fzws == nv) out%grid%ts%fzws = fzws
            where (out%grid%ts%alws == nv) out%grid%ts%alws = lqws + fzws
!            where (out%grid%ts%stgw == nv) out%grid%ts%stgw =
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            where (out%grid%ts%cmas == nv .and. fcnpy > 0.0) out%grid%ts%cmas = cmas/fcnpy
            where (out%grid%ts%tcan == nv .and. fcnpy > 0.0) out%grid%ts%tcan = tcan/fcnpy
            where (out%grid%ts%tsno == nv .and. fsncov > 0.0) out%grid%ts%tsno = tsno/fsncov
            where (out%grid%ts%tpnd == nv .and. fzpnd > 0.0) out%grid%ts%tpnd = tpnd/fzpnd
            where (out%grid%ts%albt == nv) out%grid%ts%albt = albt
            where (out%grid%ts%alvs == nv) out%grid%ts%alvs = alvs
            where (out%grid%ts%alir == nv) out%grid%ts%alir = alir
            if (allocated(fsin)) then
                where (out%grid%ts%fsout == nv) out%grid%ts%fsout = fsin*(1.0 - albt)
            end if
            where (out%grid%ts%gte == nv) out%grid%ts%gte = gte
            where (out%grid%ts%flout == nv) out%grid%ts%flout = 5.66796E-8*gte**4
            where (out%grid%ts%qh == nv) out%grid%ts%qh = qh
            where (out%grid%ts%qe == nv) out%grid%ts%qe = qe
            where (out%grid%ts%gzero == nv) out%grid%ts%gzero = gzero
            where (out%grid%ts%gflx == nv) out%grid%ts%gflx = gflx
            where (out%grid%ts%tbar == nv) out%grid%ts%tbar = tbar
!            where (out%grid%ts%tmax == nv) out%grid%ts%tmax =
!            where (out%grid%ts%tmin == nv) out%grid%ts%tmin =
!            where (out%grid%ts%ald == nv) out%grid%ts%ald =
!            where (out%grid%ts%zod == nv) out%grid%ts%zod =
!            where (out%grid%ts%stge == nv) out%grid%ts%stge =
        end if

        !> Channels and routing.
        !> 'stas_grid' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNCHNL) then
            where (out%grid%ts%rff == nv) out%grid%ts%rff = rff
            where (out%grid%ts%rchg == nv) out%grid%ts%rchg = rchg
            where (out%grid%ts%qi == nv) out%grid%ts%qi = stas_grid%chnl%qi
            where (out%grid%ts%stgch == nv) out%grid%ts%stgch = stas_grid%chnl%stg
            where (out%grid%ts%qo == nv) out%grid%ts%qo = stas_grid%chnl%qo
!            where (out%grid%ts%zlvl == nv) out%grid%ts%zlvl = stas_grid%chnl%zlvl
        end if

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
        use shd_variables
        use control_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Tile-based.
        if (ro%RUNTILE) then
            call output_variables_update_tile(shd, cm)
            call output_variables_update_series(out%tile%dly, out%tile)
            call output_variables_update_series(out%tile%hly, out%tile)
            call output_variables_update_series(out%tile%mly, out%tile)
            call output_variables_update_series(out%tile%yly, out%tile)
            call output_variables_update_series(out%tile%tot, out%tile)
        end if

        !> Grid-based.
        if (ro%RUNGRID) then
            call output_variables_update_grid(shd, cm)
            call output_variables_update_series(out%grid%dly, out%grid)
            call output_variables_update_series(out%grid%hly, out%grid)
            call output_variables_update_series(out%grid%mly, out%grid)
            call output_variables_update_series(out%grid%yly, out%grid)
            call output_variables_update_series(out%grid%tot, out%grid)
        end if

    end subroutine

    !> Description:
    !>  Reset output variables of the provided group. Set variables to
    !>  the NO_DATA value.
    subroutine output_variables_reset_group(group)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input/output variables.
        type(output_variables_group_container) group

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            group%ts%pre = out%NO_DATA
            group%ts%fsin = out%NO_DATA
            group%ts%flin = out%NO_DATA
            group%ts%ta = out%NO_DATA
            group%ts%qa = out%NO_DATA
            group%ts%pres = out%NO_DATA
            group%ts%uv = out%NO_DATA
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            group%ts%evap = out%NO_DATA
            group%ts%pevp = out%NO_DATA
            group%ts%evpb = out%NO_DATA
            group%ts%arrd = out%NO_DATA
            group%ts%rof = out%NO_DATA
            group%ts%rofo = out%NO_DATA
            group%ts%rofs = out%NO_DATA
            group%ts%rofb = out%NO_DATA
            group%ts%rcan = out%NO_DATA
            group%ts%sncan = out%NO_DATA
            group%ts%gro = out%NO_DATA
            group%ts%sno = out%NO_DATA
            group%ts%fsno = out%NO_DATA
            group%ts%wsno = out%NO_DATA
            group%ts%zpnd = out%NO_DATA
            group%ts%pndw = out%NO_DATA
            group%ts%lzs = out%NO_DATA
            group%ts%dzs = out%NO_DATA
            group%ts%thlq = out%NO_DATA
            group%ts%lqws = out%NO_DATA
            group%ts%thic = out%NO_DATA
            group%ts%fzws = out%NO_DATA
            group%ts%alws = out%NO_DATA
            group%ts%stgw = out%NO_DATA
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            group%ts%cmas = out%NO_DATA
            group%ts%tcan = out%NO_DATA
            group%ts%tsno = out%NO_DATA
            group%ts%tpnd = out%NO_DATA
            group%ts%alvs = out%NO_DATA
            group%ts%alir = out%NO_DATA
            group%ts%albt = out%NO_DATA
            group%ts%fsout = out%NO_DATA
            group%ts%flout = out%NO_DATA
            group%ts%gte = out%NO_DATA
            group%ts%qh = out%NO_DATA
            group%ts%qe = out%NO_DATA
            group%ts%gzero = out%NO_DATA
            group%ts%gflx = out%NO_DATA
            group%ts%tbar = out%NO_DATA
            group%ts%tmax = out%NO_DATA
            group%ts%tmin = out%NO_DATA
            group%ts%ald = out%NO_DATA
            group%ts%zod = out%NO_DATA
            group%ts%stge = out%NO_DATA
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            group%ts%rff = out%NO_DATA
            group%ts%rchg = out%NO_DATA
            group%ts%qi = out%NO_DATA
            group%ts%stgch = out%NO_DATA
            group%ts%qo = out%NO_DATA
            group%ts%zlvl = out%NO_DATA
        end if

    end subroutine

    !> Description:
    !>  Reset output variables of all groups. Set variables to the
    !>  NO_DATA value.
    subroutine output_variables_reset()

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Tile-based.
        if (ro%RUNTILE) call output_variables_reset_group(out%tile)

        !> Grid-based.
        if (ro%RUNGRID) call output_variables_reset_group(out%grid)

    end subroutine

end module
