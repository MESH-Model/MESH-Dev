module output_variables

    use variablename_constants

    implicit none

    !> Description:
    !>  Container for output variables.
    type output_fields
        real, dimension(:), allocatable :: &
            pre, fsin, fsvh, fsih, fsdr, fsdf, flin, ta, qa, pres, uu, vv, uv, wdir, &
            prec, evap, pevp, evpb, arrd, gro, rof, rofo, rofs, rofb, &
            rcan, sncan, sno, fsno, wsno, zpnd, pndw, lzs, dzs, stgw, &
            cmas, tcan, tsno, tpnd, &
            alvs, alir, albt, fsout, flout, gte, qh, qe, gzero, stge, &
            rff, rchg, qi, stgch, qo, zlvl
        real, dimension(:, :), allocatable :: &
            thlq, lqws, thic, fzws, alws, &
            gflx, tbar
    end type

    !> Description:
    !>  Container for a series of output variables (e.g., at various time intervals).
    !>
    !> Variables:
    !*  tile: Elemental computational unit 1:NML.
    !*  grid: Grid combined from contributing GRUs (by ACLASS) 1:NA.
    !*  basin: Same as grid but accumulated according to drainage direction 1:NA.
    type output_series
        type(output_fields) tile, grid, basin
    end type

    !> Description:
    !>  Container for output variables and NO_DATA values.
    !>
    !> Variables:
    !*  tot, y, m, d, h, ts: Output at variable time intervals.
    !*  NO_DATA: No data value (type: real).
    !*  NO_DATA_INT: No data value (type: integer).
    type output_variables_container
        type(output_series) tot, y, m, d, h, ts
        integer :: NO_DATA_INT = -1
        real :: NO_DATA = -1.0
    end type

    !*  out: Instance of output variables.
    type(output_variables_container), save :: out

    !> Description:
    !>  Interface for 'output_variables_allocate'.
    interface output_variables_allocate
        module procedure output_variables_allocate_1d
        module procedure output_variables_allocate_2d
    end interface

    contains

    !> Description:
    !>  Allocate and initialize data variable. 'field' is allocated to
    !>  dimension 'n' and set to the NO_DATA value.
    subroutine output_variables_allocate_1d(field, n)

        !> Input/output variables.
        integer, intent(in) :: n
        real, dimension(:), allocatable :: field

        !> Allocate and initialize variable
        if (.not. allocated(field)) then
            allocate(field(n))
            field = out%NO_DATA
        end if

    end subroutine

    !> Description:
    !>  Allocate and initialize data variable. 'field' is allocated to
    !>  dimension 'n' and 'j', and set to the NO_DATA value.
    subroutine output_variables_allocate_2d(field, n, j)

        !> Input/output variables.
        integer, intent(in) :: n, j
        real, dimension(:, :), allocatable :: field

        !> Allocate and initialize variable
        if (.not. allocated(field)) then
            allocate(field(n, j))
            field = out%NO_DATA
        end if

    end subroutine

    !> Description:
    !>  Allocate the output variable to 'n' and optionally 'nsl'.
    !>  The value is set to the NO_DATA value.
    subroutine output_variables_allocate_val(fields, vname, n, nsl)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input variables.
        type(output_fields), intent(in) :: fields
        character(len = *), intent(in) :: vname
        integer, intent(in) :: n
        integer, intent(in), optional :: nsl

        !> Copy the variable.
        select case (vname)

            !> Meteorological forcing.
            case (VN_PRE)
                if (ro%RUNCLIM) call output_variables_allocate(fields%pre, n)
            case (VN_FSIN)
                if (ro%RUNCLIM) call output_variables_allocate(fields%fsin, n)
            case (VN_FLIN)
                if (ro%RUNCLIM) call output_variables_allocate(fields%flin, n)
            case (VN_TA)
                if (ro%RUNCLIM) call output_variables_allocate(fields%ta, n)
            case (VN_QA)
                if (ro%RUNCLIM) call output_variables_allocate(fields%qa, n)
            case (VN_PRES)
                if (ro%RUNCLIM) call output_variables_allocate(fields%pres, n)
            case (VN_UV)
                if (ro%RUNCLIM) call output_variables_allocate(fields%uv, n)

            !> Water balance.
            case (VN_PREC)
                if (ro%RUNBALWB) call output_variables_allocate(fields%prec, n)
            case (VN_EVAP)
                if (ro%RUNBALWB) call output_variables_allocate(fields%evap, n)
            case (VN_PEVP)
                if (ro%RUNBALWB) call output_variables_allocate(fields%pevp, n)
            case (VN_EVPB)
                if (ro%RUNBALWB) call output_variables_allocate(fields%evpb, n)
            case (VN_ARRD)
                if (ro%RUNBALWB) call output_variables_allocate(fields%arrd, n)
            case (VN_GRO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%gro, n)
            case (VN_ROF)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rof, n)
            case (VN_ROFO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rofo, n)
            case (VN_ROFS)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rofs, n)
            case (VN_ROFB)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rofb, n)
            case (VN_RCAN)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rcan, n)
            case (VN_SNCAN)
                if (ro%RUNBALWB) call output_variables_allocate(fields%sncan, n)
            case (VN_SNO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%sno, n)
            case (VN_FSNO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%fsno, n)
            case (VN_WSNO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%wsno, n)
            case (VN_ZPND)
                if (ro%RUNBALWB) call output_variables_allocate(fields%zpnd, n)
            case (VN_PNDW)
                if (ro%RUNBALWB) call output_variables_allocate(fields%pndw, n)
            case (VN_LZS)
                if (ro%RUNBALWB) call output_variables_allocate(fields%lzs, n)
            case (VN_DZS)
                if (ro%RUNBALWB) call output_variables_allocate(fields%dzs, n)
            case (VN_STGW)
                if (ro%RUNBALWB) call output_variables_allocate(fields%stgw, n)
            case (VN_THLQ)
                if (ro%RUNBALWB .and. present(nsl)) call output_variables_allocate(fields%thlq, n, nsl)
            case (VN_LQWS)
                if (ro%RUNBALWB .and. present(nsl)) call output_variables_allocate(fields%lqws, n, nsl)
            case (VN_THIC)
                if (ro%RUNBALWB .and. present(nsl)) call output_variables_allocate(fields%thic, n, nsl)
            case (VN_FZWS)
                if (ro%RUNBALWB .and. present(nsl)) call output_variables_allocate(fields%fzws, n, nsl)
            case (VN_ALWS)
                if (ro%RUNBALWB .and. present(nsl)) call output_variables_allocate(fields%alws, n, nsl)

            !> Energy balance.
            case (VN_CMAS)
                if (ro%RUNBALEB) call output_variables_allocate(fields%cmas, n)
            case (VN_TCAN)
                if (ro%RUNBALEB) call output_variables_allocate(fields%tcan, n)
            case (VN_TSNO)
                if (ro%RUNBALEB) call output_variables_allocate(fields%tsno, n)
            case (VN_TPND)
                if (ro%RUNBALEB) call output_variables_allocate(fields%tpnd, n)
            case (VN_ALBT)
                if (ro%RUNBALEB) call output_variables_allocate(fields%albt, n)
            case (VN_ALVS)
                if (ro%RUNBALEB) call output_variables_allocate(fields%alvs, n)
            case (VN_ALIR)
                if (ro%RUNBALEB) call output_variables_allocate(fields%alir, n)
            case (VN_FSOUT)
                if (ro%RUNBALEB) call output_variables_allocate(fields%fsout, n)
            case (VN_GTE)
                if (ro%RUNBALEB) call output_variables_allocate(fields%gte, n)
            case (VN_FLOUT)
                if (ro%RUNBALEB) call output_variables_allocate(fields%flout, n)
            case (VN_QH)
                if (ro%RUNBALEB) call output_variables_allocate(fields%qh, n)
            case (VN_QE)
                if (ro%RUNBALEB) call output_variables_allocate(fields%qe, n)
            case (VN_GZERO)
                if (ro%RUNBALEB) call output_variables_allocate(fields%gzero, n)
            case (VN_STGE)
                if (ro%RUNBALEB) call output_variables_allocate(fields%stge, n)
            case (VN_GFLX)
                if (ro%RUNBALEB .and. present(nsl)) call output_variables_allocate(fields%gflx, n, nsl)
            case (VN_TBAR)
                if (ro%RUNBALEB .and. present(nsl)) call output_variables_allocate(fields%tbar, n, nsl)

            !> Channels and routing.
            case (VN_RFF)
                if (ro%RUNCHNL) call output_variables_allocate(fields%rff, n)
            case (VN_RCHG)
                if (ro%RUNCHNL) call output_variables_allocate(fields%rchg, n)
            case (VN_QI)
                if (ro%RUNCHNL) call output_variables_allocate(fields%qi, n)
            case (VN_STGCH)
                if (ro%RUNCHNL) call output_variables_allocate(fields%stgch, n)
            case (VN_QO)
                if (ro%RUNCHNL) call output_variables_allocate(fields%qo, n)
            case (VN_ZLVL)
                if (ro%RUNCHNL) call output_variables_allocate(fields%zlvl, n)
        end select

    end subroutine

    !> Description:
    !>  Allocate output variables.
    subroutine output_variables_init_fields(shd, fields, n)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: n

        !> Input/output variables.
        type(output_fields) fields

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            allocate(fields%pre(n))
            allocate(fields%fsin(n))
            allocate(fields%flin(n))
            allocate(fields%ta(n))
            allocate(fields%qa(n))
            allocate(fields%pres(n))
            allocate(fields%uv(n))
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            allocate(fields%prec(n))
            allocate(fields%evap(n))
            allocate(fields%pevp(n))
            allocate(fields%evpb(n))
            allocate(fields%arrd(n))
            allocate(fields%gro(n))
            allocate(fields%rof(n))
            allocate(fields%rofo(n))
            allocate(fields%rofs(n))
            allocate(fields%rofb(n))
            allocate(fields%rcan(n))
            allocate(fields%sncan(n))
            allocate(fields%sno(n))
            allocate(fields%fsno(n))
            allocate(fields%wsno(n))
            allocate(fields%zpnd(n))
            allocate(fields%pndw(n))
            allocate(fields%lzs(n))
            allocate(fields%dzs(n))
            allocate(fields%stgw(n))
            allocate(fields%thlq(n, shd%lc%IGND))
            allocate(fields%lqws(n, shd%lc%IGND))
            allocate(fields%thic(n, shd%lc%IGND))
            allocate(fields%fzws(n, shd%lc%IGND))
            allocate(fields%alws(n, shd%lc%IGND))
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            allocate(fields%cmas(n))
            allocate(fields%tcan(n))
            allocate(fields%tsno(n))
            allocate(fields%tpnd(n))
            allocate(fields%alvs(n))
            allocate(fields%alir(n))
            allocate(fields%albt(n))
            allocate(fields%fsout(n))
            allocate(fields%flout(n))
            allocate(fields%gte(n))
            allocate(fields%qh(n))
            allocate(fields%qe(n))
            allocate(fields%gzero(n))
            allocate(fields%stge(n))
            allocate(fields%gflx(n, shd%lc%IGND))
            allocate(fields%tbar(n, shd%lc%IGND))
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            allocate(fields%rff(n))
            allocate(fields%rchg(n))
            allocate(fields%qi(n))
            allocate(fields%stgch(n))
            allocate(fields%qo(n))
            allocate(fields%zlvl(n))
        end if

    end subroutine

    !> Description:
    !>  Allocate and initialize output variables.
    subroutine output_variables_init(shd, cm)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'climate_forcing' required for 'cm'.
        use shd_variables
        use control_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Tile-based.
        if (ro%RUNTILE) then
            call output_variables_init_fields(shd, out%ts%tile, shd%lc%NML)
            call output_variables_reset_fields(shd, out%ts%tile)
        end if

        !> Grid-based.
        if (ro%RUNGRID) then
            call output_variables_init_fields(shd, out%ts%grid, shd%NA)
            call output_variables_reset_fields(shd, out%ts%grid)
            call output_variables_init_fields(shd, out%ts%basin, shd%NA)
            call output_variables_reset_fields(shd, out%ts%basin)
        end if

        !> Update.
        call output_variables_update_ts(shd, cm)

    end subroutine

    !> Description:
    !>  Update output variables from current model states and variables.
    subroutine output_variables_update_ts(shd, cm)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'state_variables' required for 'stas'.
        !> 'climate_forcing' required for 'cm'.
        use shd_variables
        use control_variables
        use state_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Tile-based.
        if (ro%RUNTILE) then

            !> Meteorological forcing.
            !> Climate variables are not allocated by group so must check 'allocated' status.
            if (ro%RUNCLIM) then
                if (allocated(cm%dat(ck%RT)%GAT)) then
                    if (all(out%ts%tile%pre == out%NO_DATA)) out%ts%tile%pre = cm%dat(ck%RT)%GAT
                end if
                if (allocated(cm%dat(ck%FB)%GAT)) then
                    if (all(out%ts%tile%fsin == out%NO_DATA)) out%ts%tile%fsin = cm%dat(ck%FB)%GAT
                end if
                if (allocated(cm%dat(ck%FI)%GAT)) then
                    if (all(out%ts%tile%flin == out%NO_DATA)) out%ts%tile%flin = cm%dat(ck%FI)%GAT
                end if
                if (allocated(cm%dat(ck%TT)%GAT)) then
                    if (all(out%ts%tile%ta == out%NO_DATA)) out%ts%tile%ta = cm%dat(ck%TT)%GAT
                end if
                if (allocated(cm%dat(ck%HU)%GAT)) then
                    if (all(out%ts%tile%qa == out%NO_DATA)) out%ts%tile%qa = cm%dat(ck%HU)%GAT
                end if
                if (allocated(cm%dat(ck%P0)%GAT)) then
                    if (all(out%ts%tile%pres == out%NO_DATA)) out%ts%tile%pres = cm%dat(ck%P0)%GAT
                end if
                if (allocated(cm%dat(ck%UV)%GAT)) then
                    if (all(out%ts%tile%uv == out%NO_DATA)) out%ts%tile%uv = cm%dat(ck%UV)%GAT
                end if
            end if

            !> Water balance.
            !> 'stas' variables are allocated by group so 'allocated' status is assumed.
            if (ro%RUNBALWB) then
                if (allocated(cm%dat(ck%RT)%GAT)) then
                    if (all(out%ts%tile%prec == out%NO_DATA)) out%ts%tile%prec = cm%dat(ck%RT)%GAT
                end if
                if (all(out%ts%tile%evap == out%NO_DATA)) out%ts%tile%evap = stas%sfc%evap
                if (all(out%ts%tile%pevp == out%NO_DATA)) out%ts%tile%pevp = stas%sfc%pevp
                if (all(out%ts%tile%evpb == out%NO_DATA)) out%ts%tile%evpb = stas%sfc%evpb
                if (all(out%ts%tile%arrd == out%NO_DATA)) out%ts%tile%arrd = stas%sfc%arrd
                if (all(out%ts%tile%gro == out%NO_DATA)) out%ts%tile%gro = stas%cnpy%gro
                if (all(out%ts%tile%rof == out%NO_DATA)) then
                    out%ts%tile%rof = stas%sfc%rofo + stas%sl%rofs + stas%lzs%rofb + stas%dzs%rofb
                end if
                if (all(out%ts%tile%rofo == out%NO_DATA)) out%ts%tile%rofo = stas%sfc%rofo
                if (all(out%ts%tile%rofs == out%NO_DATA)) out%ts%tile%rofs = stas%sl%rofs
                if (all(out%ts%tile%rofb == out%NO_DATA)) out%ts%tile%rofb = stas%lzs%rofb + stas%dzs%rofb
                if (all(out%ts%tile%rcan == out%NO_DATA)) out%ts%tile%rcan = stas%cnpy%rcan
                if (all(out%ts%tile%sncan == out%NO_DATA)) out%ts%tile%sncan = stas%cnpy%sncan
                if (all(out%ts%tile%sno == out%NO_DATA)) out%ts%tile%sno = stas%sno%sno
                if (all(out%ts%tile%fsno == out%NO_DATA)) out%ts%tile%fsno = stas%sno%fsno
                if (all(out%ts%tile%wsno == out%NO_DATA)) out%ts%tile%wsno = stas%sno%wsno
                if (all(out%ts%tile%zpnd == out%NO_DATA)) out%ts%tile%zpnd = stas%sfc%zpnd
                if (all(out%ts%tile%pndw == out%NO_DATA)) out%ts%tile%pndw = stas%sfc%pndw
                if (all(out%ts%tile%lzs == out%NO_DATA)) out%ts%tile%lzs = stas%lzs%ws
                if (all(out%ts%tile%dzs == out%NO_DATA)) out%ts%tile%dzs = stas%dzs%ws
!                if (all(out%ts%tile%stgw == out%NO_DATA)) out%ts%tile%stgw =
                if (all(out%ts%tile%thlq == out%NO_DATA)) out%ts%tile%thlq = stas%sl%thlq
                if (all(out%ts%tile%lqws == out%NO_DATA)) out%ts%tile%lqws = stas%sl%lqws
                if (all(out%ts%tile%thic == out%NO_DATA)) out%ts%tile%thic = stas%sl%thic
                if (all(out%ts%tile%fzws == out%NO_DATA)) out%ts%tile%fzws = stas%sl%fzws
                if (all(out%ts%tile%alws == out%NO_DATA)) out%ts%tile%alws = stas%sl%lqws + stas%sl%fzws
            end if

            !> Energy balance.
            !> 'stas' variables are allocated by group so 'allocated' status is assumed.
            if (ro%RUNBALEB) then
                if (all(out%ts%tile%cmas == out%NO_DATA)) out%ts%tile%cmas = stas%cnpy%cmas
                if (all(out%ts%tile%tcan == out%NO_DATA)) out%ts%tile%tcan = stas%cnpy%tcan
                if (all(out%ts%tile%tsno == out%NO_DATA)) out%ts%tile%tsno = stas%sno%tsno
                if (all(out%ts%tile%tpnd == out%NO_DATA)) out%ts%tile%tpnd = stas%sfc%tpnd
                if (all(out%ts%tile%albt == out%NO_DATA)) out%ts%tile%albt = stas%sfc%albt
                if (all(out%ts%tile%alvs == out%NO_DATA)) out%ts%tile%alvs = stas%sfc%alvs
                if (all(out%ts%tile%alir == out%NO_DATA)) out%ts%tile%alir = stas%sfc%alir
                if (allocated(cm%dat(ck%FB)%GAT)) then
                    if (all(out%ts%tile%fsout == out%NO_DATA)) out%ts%tile%fsout = cm%dat(ck%FB)%GAT*(1.0 - stas%sfc%albt)
                end if
                if (all(out%ts%tile%gte == out%NO_DATA)) out%ts%tile%gte = stas%sfc%gte
                if (all(out%ts%tile%flout == out%NO_DATA)) out%ts%tile%flout = 5.66796E-8*stas%sfc%gte**4
                if (all(out%ts%tile%qh == out%NO_DATA)) out%ts%tile%qh = stas%sfc%hfs
                if (all(out%ts%tile%qe == out%NO_DATA)) out%ts%tile%qe = stas%sfc%qevp
                if (all(out%ts%tile%gzero == out%NO_DATA)) out%ts%tile%gzero = stas%sfc%gzero
!                if (all(out%ts%tile%stge == out%NO_DATA)) out%ts%tile%stge =
                if (all(out%ts%tile%gflx == out%NO_DATA)) out%ts%tile%gflx = stas%sl%gflx
                if (all(out%ts%tile%tbar == out%NO_DATA)) out%ts%tile%tbar = stas%sl%tbar
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then

            !> Meteorological forcing.
            !> Climate variables are not allocated by group so must check 'allocated' status.
            if (ro%RUNCLIM) then
                if (allocated(cm%dat(ck%RT)%GRD)) then
                    if (all(out%ts%grid%pre == out%NO_DATA)) out%ts%grid%pre = cm%dat(ck%RT)%GRD
                end if
                if (allocated(cm%dat(ck%FB)%GRD)) then
                    if (all(out%ts%grid%fsin == out%NO_DATA)) out%ts%grid%fsin = cm%dat(ck%FB)%GRD
                end if
                if (allocated(cm%dat(ck%FI)%GRD)) then
                    if (all(out%ts%grid%flin == out%NO_DATA)) out%ts%grid%flin = cm%dat(ck%FI)%GRD
                end if
                if (allocated(cm%dat(ck%TT)%GRD)) then
                    if (all(out%ts%grid%ta == out%NO_DATA)) out%ts%grid%ta = cm%dat(ck%TT)%GRD
                end if
                if (allocated(cm%dat(ck%HU)%GRD)) then
                    if (all(out%ts%grid%qa == out%NO_DATA)) out%ts%grid%qa = cm%dat(ck%HU)%GRD
                end if
                if (allocated(cm%dat(ck%P0)%GRD)) then
                    if (all(out%ts%grid%pres == out%NO_DATA)) out%ts%grid%pres = cm%dat(ck%P0)%GRD
                end if
                if (allocated(cm%dat(ck%UV)%GRD)) then
                    if (all(out%ts%grid%uv == out%NO_DATA)) out%ts%grid%uv = cm%dat(ck%UV)%GRD
                end if
            end if

            !> Water balance.
            !> 'stas_grid' variables are allocated by group so 'allocated' status is assumed.
            if (ro%RUNBALWB) then
                if (allocated(cm%dat(ck%RT)%GRD)) then
                    if (all(out%ts%grid%prec == out%NO_DATA)) out%ts%grid%prec = cm%dat(ck%RT)%GRD
                end if
                if (all(out%ts%grid%evap == out%NO_DATA)) out%ts%grid%evap = stas_grid%sfc%evap
                if (all(out%ts%grid%pevp == out%NO_DATA)) out%ts%grid%pevp = stas_grid%sfc%pevp
                if (all(out%ts%grid%evpb == out%NO_DATA)) out%ts%grid%evpb = stas_grid%sfc%evpb
                if (all(out%ts%grid%arrd == out%NO_DATA)) out%ts%grid%arrd = stas_grid%sfc%arrd
                if (all(out%ts%grid%gro == out%NO_DATA)) out%ts%grid%gro = stas_grid%cnpy%gro
                if (all(out%ts%grid%rof == out%NO_DATA)) then
                    out%ts%grid%rof = stas_grid%sfc%rofo + stas_grid%sl%rofs + stas_grid%lzs%rofb + stas_grid%dzs%rofb
                end if
                if (all(out%ts%grid%rofo == out%NO_DATA)) out%ts%grid%rofo = stas_grid%sfc%rofo
                if (all(out%ts%grid%rofs == out%NO_DATA)) out%ts%grid%rofs = stas_grid%sl%rofs
                if (all(out%ts%grid%rofb == out%NO_DATA)) out%ts%grid%rofb = stas_grid%lzs%rofb + stas_grid%dzs%rofb
                if (all(out%ts%grid%rcan == out%NO_DATA)) out%ts%grid%rcan = stas_grid%cnpy%rcan
                if (all(out%ts%grid%sncan == out%NO_DATA)) out%ts%grid%sncan = stas_grid%cnpy%sncan
                if (all(out%ts%grid%sno == out%NO_DATA)) out%ts%grid%sno = stas_grid%sno%sno
                if (all(out%ts%grid%fsno == out%NO_DATA)) out%ts%grid%fsno = stas_grid%sno%fsno
                if (all(out%ts%grid%wsno == out%NO_DATA)) out%ts%grid%wsno = stas_grid%sno%wsno
                if (all(out%ts%grid%zpnd == out%NO_DATA)) out%ts%grid%zpnd = stas_grid%sfc%zpnd
                if (all(out%ts%grid%pndw == out%NO_DATA)) out%ts%grid%pndw = stas_grid%sfc%pndw
                if (all(out%ts%grid%lzs == out%NO_DATA)) out%ts%grid%lzs = stas_grid%lzs%ws
                if (all(out%ts%grid%dzs == out%NO_DATA)) out%ts%grid%dzs = stas_grid%dzs%ws
!                if (all(out%ts%grid%stgw == out%NO_DATA)) out%ts%grid%stgw =
                if (all(out%ts%grid%thlq == out%NO_DATA)) out%ts%grid%thlq = stas_grid%sl%thlq
                if (all(out%ts%grid%lqws == out%NO_DATA)) out%ts%grid%lqws = stas_grid%sl%lqws
                if (all(out%ts%grid%thic == out%NO_DATA)) out%ts%grid%thic = stas_grid%sl%thic
                if (all(out%ts%grid%fzws == out%NO_DATA)) out%ts%grid%fzws = stas_grid%sl%fzws
                if (all(out%ts%grid%alws == out%NO_DATA)) out%ts%grid%alws = stas_grid%sl%lqws + stas_grid%sl%fzws
            end if

            !> Energy balance.
            !> 'stas_grid' variables are allocated by group so 'allocated' status is assumed.
            if (ro%RUNBALEB) then
                if (all(out%ts%grid%cmas == out%NO_DATA)) out%ts%grid%cmas = stas_grid%cnpy%cmas
                if (all(out%ts%grid%tcan == out%NO_DATA)) out%ts%grid%tcan = stas_grid%cnpy%tcan
                if (all(out%ts%grid%tsno == out%NO_DATA)) out%ts%grid%tsno = stas_grid%sno%tsno
                if (all(out%ts%grid%tpnd == out%NO_DATA)) out%ts%grid%tpnd = stas_grid%sfc%tpnd
                if (all(out%ts%grid%albt == out%NO_DATA)) out%ts%grid%albt = stas_grid%sfc%albt
                if (all(out%ts%grid%alvs == out%NO_DATA)) out%ts%grid%alvs = stas_grid%sfc%alvs
                if (all(out%ts%grid%alir == out%NO_DATA)) out%ts%grid%alir = stas_grid%sfc%alir
                if (allocated(cm%dat(ck%FB)%GRD)) then
                    if (all(out%ts%grid%fsout == out%NO_DATA)) out%ts%grid%fsout = cm%dat(ck%FB)%GRD*(1.0 - stas_grid%sfc%albt)
                end if
                if (all(out%ts%grid%gte == out%NO_DATA)) out%ts%grid%gte = stas_grid%sfc%gte
                if (all(out%ts%grid%flout == out%NO_DATA)) out%ts%grid%flout = 5.66796E-8*stas_grid%sfc%gte**4
                if (all(out%ts%grid%qh == out%NO_DATA)) out%ts%grid%qh = stas_grid%sfc%hfs
                if (all(out%ts%grid%qe == out%NO_DATA)) out%ts%grid%qe = stas_grid%sfc%qevp
                if (all(out%ts%grid%gzero == out%NO_DATA)) out%ts%grid%gzero = stas_grid%sfc%gzero
!                if (all(out%ts%grid%stge == out%NO_DATA)) out%ts%grid%stge =
                if (all(out%ts%grid%gflx == out%NO_DATA)) out%ts%grid%gflx = stas_grid%sl%gflx
                if (all(out%ts%grid%tbar == out%NO_DATA)) out%ts%grid%tbar = stas_grid%sl%tbar
            end if

            !> Channels and routing.
            !> 'stas_grid' variables are allocated by group so 'allocated' status is assumed.
            if (ro%RUNCHNL) then
                if (all(out%ts%grid%rff == out%NO_DATA)) out%ts%grid%rff = stas_grid%chnl%rff
                if (all(out%ts%grid%rchg == out%NO_DATA)) out%ts%grid%rchg = stas_grid%chnl%rchg
                if (all(out%ts%grid%qi == out%NO_DATA)) out%ts%grid%qi = stas_grid%chnl%qi
                if (all(out%ts%grid%stgch == out%NO_DATA)) out%ts%grid%stgch = stas_grid%chnl%stg
                if (all(out%ts%grid%qo == out%NO_DATA)) out%ts%grid%qo = stas_grid%chnl%qo
!                if (all(out%ts%grid%zlvl == out%NO_DATA)) out%ts%grid%zlvl = stas_grid%chnl%zlvl
            end if
        end if

    end subroutine

    !> Description:
    !>  Update the 'dat' vector using the 'val' vector.
    !>  Reset 'dat' if the time-step of the current interval "its" is 1.
    !>  Calculate an average if the function "fn" is 'avg' and the
    !>  time-steps to divide the number "dnts" by is greater than zero.
    !>  Override calculations where 'val' is equal to the NO_DATA value
    !>  with the NO_DATA value in 'dat'.
    subroutine output_variables_update_values(dat, val, its, dnts, fn)

        !> Input variables.
        integer, intent(in) :: its, dnts
        real, dimension(:), intent(in) :: val
        character(len = *), intent(in) :: fn

        !> Input/output variables.
        real, dimension(:) :: dat

        !> Reset the variable if this is the first time-step in the series.
        if (its == 1) dat = 0.0

        !> Apply the 'fn' function.
        !> The default case is to set 'dat' to 'val'.
        select case (fn)
            case ('sum')
                dat = dat + val
            case ('avg')
                dat = dat + val
                if (dnts > 0) dat = dat/dnts
            case ('max')
                dat = max(dat, val)
            case ('min')
                dat = min(dat, val)
            case default
                dat = val
        end select

        !> Assign the 'NO_DATA' value where 'NO_DATA' existed in 'val'.
        where (val == out%NO_DATA) dat = out%NO_DATA

    end subroutine

    !> Description:
    !>  Update output variables of larger time intervals from the 'ts'
    !>  values.
    subroutine output_variables_update_series(shd, series, its, dnts)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        integer, intent(in) :: its, dnts
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Local variables.
        integer j

        !> Tile-based.
        if (ro%RUNTILE) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (allocated(series%tile%pre)) then
                    call output_variables_update_values(series%tile%pre, out%ts%tile%pre, its, dnts, 'avg')
                end if
                if (allocated(series%tile%fsin)) then
                    call output_variables_update_values(series%tile%fsin, out%ts%tile%fsin, its, dnts, 'avg')
                end if
                if (allocated(series%tile%flin)) then
                    call output_variables_update_values(series%tile%flin, out%ts%tile%flin, its, dnts, 'avg')
                end if
                if (allocated(series%tile%ta)) then
                    call output_variables_update_values(series%tile%ta, out%ts%tile%ta, its, dnts, 'avg')
                end if
                if (allocated(series%tile%qa)) then
                    call output_variables_update_values(series%tile%qa, out%ts%tile%qa, its, dnts, 'avg')
                end if
                if (allocated(series%tile%pres)) then
                    call output_variables_update_values(series%tile%pres, out%ts%tile%pres, its, dnts, 'avg')
                end if
                if (allocated(series%tile%uv)) then
                    call output_variables_update_values(series%tile%uv, out%ts%tile%uv, its, dnts, 'avg')
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (allocated(series%tile%prec)) then
                    call output_variables_update_values(series%tile%prec, out%ts%tile%prec, its, dnts, 'sum')
                end if
                if (allocated(series%tile%evap)) then
                    call output_variables_update_values(series%tile%evap, out%ts%tile%evap, its, dnts, 'sum')
                end if
                if (allocated(series%tile%pevp)) then
                    call output_variables_update_values(series%tile%pevp, out%ts%tile%pevp, its, dnts, 'sum')
                end if
                if (allocated(series%tile%evpb)) then
                    call output_variables_update_values(series%tile%evpb, out%ts%tile%evpb, its, dnts, 'avg')
                end if
                if (allocated(series%tile%arrd)) then
                    call output_variables_update_values(series%tile%arrd, out%ts%tile%arrd, its, dnts, 'avg')
                end if
                if (allocated(series%tile%gro)) then
                    call output_variables_update_values(series%tile%gro, out%ts%tile%gro, its, dnts, 'avg')
                end if
                if (allocated(series%tile%rof)) then
                    call output_variables_update_values(series%tile%rof, out%ts%tile%rof, its, dnts, 'sum')
                end if
                if (allocated(series%tile%rofo)) then
                    call output_variables_update_values(series%tile%rofo, out%ts%tile%rofo, its, dnts, 'sum')
                end if
                if (allocated(series%tile%rofs)) then
                    call output_variables_update_values(series%tile%rofs, out%ts%tile%rofs, its, dnts, 'sum')
                end if
                if (allocated(series%tile%rofb)) then
                    call output_variables_update_values(series%tile%rofb, out%ts%tile%rofb, its, dnts, 'sum')
                end if
                if (allocated(series%tile%rcan)) then
                    call output_variables_update_values(series%tile%rcan, out%ts%tile%rcan, its, dnts, 'sum')
                end if
                if (allocated(series%tile%sncan)) then
                    call output_variables_update_values(series%tile%sncan, out%ts%tile%sncan, its, dnts, 'sum')
                end if
                if (allocated(series%tile%sno)) then
                    call output_variables_update_values(series%tile%sno, out%ts%tile%sno, its, dnts, 'sum')
                end if
                if (allocated(series%tile%fsno)) then
                    call output_variables_update_values(series%tile%fsno, out%ts%tile%fsno, its, dnts, 'sum')
                end if
                if (allocated(series%tile%wsno)) then
                    call output_variables_update_values(series%tile%wsno, out%ts%tile%wsno, its, dnts, 'sum')
                end if
                if (allocated(series%tile%zpnd)) then
                    call output_variables_update_values(series%tile%zpnd, out%ts%tile%zpnd, its, dnts, 'avg')
                end if
                if (allocated(series%tile%pndw)) then
                    call output_variables_update_values(series%tile%pndw, out%ts%tile%pndw, its, dnts, 'sum')
                end if
                if (allocated(series%tile%lzs)) then
                    call output_variables_update_values(series%tile%lzs, out%ts%tile%lzs, its, dnts, 'sum')
                end if
                if (allocated(series%tile%dzs)) then
                    call output_variables_update_values(series%tile%dzs, out%ts%tile%dzs, its, dnts, 'sum')
                end if
                if (allocated(series%tile%stgw)) then
                    call output_variables_update_values(series%tile%stgw, out%ts%tile%stgw, its, dnts, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (allocated(series%tile%thlq)) then
                        call output_variables_update_values(series%tile%thlq(:, j), out%ts%tile%thlq(:, j), its, dnts, 'avg')
                    end if
                    if (allocated(series%tile%lqws)) then
                        call output_variables_update_values(series%tile%lqws(:, j), out%ts%tile%lqws(:, j), its, dnts, 'sum')
                    end if
                    if (allocated(series%tile%thic)) then
                        call output_variables_update_values(series%tile%thic(:, j), out%ts%tile%thic(:, j), its, dnts, 'avg')
                    end if
                    if (allocated(series%tile%fzws)) then
                        call output_variables_update_values(series%tile%fzws(:, j), out%ts%tile%fzws(:, j), its, dnts, 'sum')
                    end if
                    if (allocated(series%tile%alws)) then
                        call output_variables_update_values(series%tile%alws(:, j), out%ts%tile%alws(:, j), its, dnts, 'sum')
                    end if
                end do
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (allocated(series%tile%cmas)) then
                    call output_variables_update_values(series%tile%cmas, out%ts%tile%cmas, its, dnts, 'sum')
                end if
                if (allocated(series%tile%tcan)) then
                    call output_variables_update_values(series%tile%tcan, out%ts%tile%tcan, its, dnts, 'avg')
                end if
                if (allocated(series%tile%tsno)) then
                    call output_variables_update_values(series%tile%tsno, out%ts%tile%tsno, its, dnts, 'avg')
                end if
                if (allocated(series%tile%tpnd)) then
                    call output_variables_update_values(series%tile%tpnd, out%ts%tile%tpnd, its, dnts, 'avg')
                end if
                if (allocated(series%tile%albt)) then
                    call output_variables_update_values(series%tile%albt, out%ts%tile%albt, its, dnts, 'avg')
                end if
                if (allocated(series%tile%alvs)) then
                    call output_variables_update_values(series%tile%alvs, out%ts%tile%alvs, its, dnts, 'avg')
                end if
                if (allocated(series%tile%alir)) then
                    call output_variables_update_values(series%tile%alir, out%ts%tile%alir, its, dnts, 'avg')
                end if
                if (allocated(series%tile%fsout)) then
                    call output_variables_update_values(series%tile%fsout, out%ts%tile%fsout, its, dnts, 'avg')
                end if
                if (allocated(series%tile%gte)) then
                    call output_variables_update_values(series%tile%gte, out%ts%tile%gte, its, dnts, 'avg')
                end if
                if (allocated(series%tile%flout)) then
                    call output_variables_update_values(series%tile%flout, out%ts%tile%flout, its, dnts, 'avg')
                end if
                if (allocated(series%tile%qh)) then
                    call output_variables_update_values(series%tile%qh, out%ts%tile%qh, its, dnts, 'avg')
                end if
                if (allocated(series%tile%qe)) then
                    call output_variables_update_values(series%tile%qe, out%ts%tile%qe, its, dnts, 'avg')
                end if
                if (allocated(series%tile%gzero)) then
                    call output_variables_update_values(series%tile%gzero, out%ts%tile%gzero, its, dnts, 'avg')
                end if
                if (allocated(series%tile%stge)) then
                    call output_variables_update_values(series%tile%stge, out%ts%tile%stge, its, dnts, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (allocated(series%tile%gflx)) then
                        call output_variables_update_values(series%tile%gflx(:, j), out%ts%tile%gflx(:, j), its, dnts, 'avg')
                    end if
                    if (allocated(series%tile%tbar)) then
                        call output_variables_update_values(series%tile%tbar(:, j), out%ts%tile%tbar(:, j), its, dnts, 'avg')
                    end if
                end do
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (allocated(series%grid%pre)) then
                    call output_variables_update_values(series%grid%pre, out%ts%grid%pre, its, dnts, 'avg')
                end if
                if (allocated(series%grid%fsin)) then
                    call output_variables_update_values(series%grid%fsin, out%ts%grid%fsin, its, dnts, 'avg')
                end if
                if (allocated(series%grid%flin)) then
                    call output_variables_update_values(series%grid%flin, out%ts%grid%flin, its, dnts, 'avg')
                end if
                if (allocated(series%grid%ta)) then
                    call output_variables_update_values(series%grid%ta, out%ts%grid%ta, its, dnts, 'avg')
                end if
                if (allocated(series%grid%qa)) then
                    call output_variables_update_values(series%grid%qa, out%ts%grid%qa, its, dnts, 'avg')
                end if
                if (allocated(series%grid%pres)) then
                    call output_variables_update_values(series%grid%pres, out%ts%grid%pres, its, dnts, 'avg')
                end if
                if (allocated(series%grid%uv)) then
                    call output_variables_update_values(series%grid%uv, out%ts%grid%uv, its, dnts, 'avg')
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (allocated(series%grid%prec)) then
                    call output_variables_update_values(series%grid%prec, out%ts%grid%prec, its, dnts, 'sum')
                end if
                if (allocated(series%grid%evap)) then
                    call output_variables_update_values(series%grid%evap, out%ts%grid%evap, its, dnts, 'sum')
                end if
                if (allocated(series%grid%pevp)) then
                    call output_variables_update_values(series%grid%pevp, out%ts%grid%pevp, its, dnts, 'sum')
                end if
                if (allocated(series%grid%evpb)) then
                    call output_variables_update_values(series%grid%evpb, out%ts%grid%evpb, its, dnts, 'avg')
                end if
                if (allocated(series%grid%arrd)) then
                    call output_variables_update_values(series%grid%arrd, out%ts%grid%arrd, its, dnts, 'avg')
                end if
                if (allocated(series%grid%gro)) then
                    call output_variables_update_values(series%grid%gro, out%ts%grid%gro, its, dnts, 'avg')
                end if
                if (allocated(series%grid%rof)) then
                    call output_variables_update_values(series%grid%rof, out%ts%grid%rof, its, dnts, 'sum')
                end if
                if (allocated(series%grid%rofo)) then
                    call output_variables_update_values(series%grid%rofo, out%ts%grid%rofo, its, dnts, 'sum')
                end if
                if (allocated(series%grid%rofs)) then
                    call output_variables_update_values(series%grid%rofs, out%ts%grid%rofs, its, dnts, 'sum')
                end if
                if (allocated(series%grid%rofb)) then
                    call output_variables_update_values(series%grid%rofb, out%ts%grid%rofb, its, dnts, 'sum')
                end if
                if (allocated(series%grid%rcan)) then
                    call output_variables_update_values(series%grid%rcan, out%ts%grid%rcan, its, dnts, 'sum')
                end if
                if (allocated(series%grid%sncan)) then
                    call output_variables_update_values(series%grid%sncan, out%ts%grid%sncan, its, dnts, 'sum')
                end if
                if (allocated(series%grid%sno)) then
                    call output_variables_update_values(series%grid%sno, out%ts%grid%sno, its, dnts, 'sum')
                end if
                if (allocated(series%grid%fsno)) then
                    call output_variables_update_values(series%grid%fsno, out%ts%grid%fsno, its, dnts, 'sum')
                end if
                if (allocated(series%grid%wsno)) then
                    call output_variables_update_values(series%grid%wsno, out%ts%grid%wsno, its, dnts, 'sum')
                end if
                if (allocated(series%grid%zpnd)) then
                    call output_variables_update_values(series%grid%zpnd, out%ts%grid%zpnd, its, dnts, 'avg')
                end if
                if (allocated(series%grid%pndw)) then
                    call output_variables_update_values(series%grid%pndw, out%ts%grid%pndw, its, dnts, 'sum')
                end if
                if (allocated(series%grid%lzs)) then
                    call output_variables_update_values(series%grid%lzs, out%ts%grid%lzs, its, dnts, 'sum')
                end if
                if (allocated(series%grid%dzs)) then
                    call output_variables_update_values(series%grid%dzs, out%ts%grid%dzs, its, dnts, 'sum')
                end if
                if (allocated(series%grid%stgw)) then
                    call output_variables_update_values(series%grid%stgw, out%ts%grid%stgw, its, dnts, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (allocated(series%grid%thlq)) then
                        call output_variables_update_values(series%grid%thlq(:, j), out%ts%grid%thlq(:, j), its, dnts, 'avg')
                    end if
                    if (allocated(series%grid%lqws)) then
                        call output_variables_update_values(series%grid%lqws(:, j), out%ts%grid%lqws(:, j), its, dnts, 'sum')
                    end if
                    if (allocated(series%grid%thic)) then
                        call output_variables_update_values(series%grid%thic(:, j), out%ts%grid%thic(:, j), its, dnts, 'avg')
                    end if
                    if (allocated(series%grid%fzws)) then
                        call output_variables_update_values(series%grid%fzws(:, j), out%ts%grid%fzws(:, j), its, dnts, 'sum')
                    end if
                    if (allocated(series%grid%alws)) then
                        call output_variables_update_values(series%grid%alws(:, j), out%ts%grid%alws(:, j), its, dnts, 'sum')
                    end if
                end do
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (allocated(series%grid%cmas)) then
                    call output_variables_update_values(series%grid%cmas, out%ts%grid%cmas, its, dnts, 'sum')
                end if
                if (allocated(series%grid%tcan)) then
                    call output_variables_update_values(series%grid%tcan, out%ts%grid%tcan, its, dnts, 'avg')
                end if
                if (allocated(series%grid%tsno)) then
                    call output_variables_update_values(series%grid%tsno, out%ts%grid%tsno, its, dnts, 'avg')
                end if
                if (allocated(series%grid%tpnd)) then
                    call output_variables_update_values(series%grid%tpnd, out%ts%grid%tpnd, its, dnts, 'avg')
                end if
                if (allocated(series%grid%albt)) then
                    call output_variables_update_values(series%grid%albt, out%ts%grid%albt, its, dnts, 'avg')
                end if
                if (allocated(series%grid%alvs)) then
                    call output_variables_update_values(series%grid%alvs, out%ts%grid%alvs, its, dnts, 'avg')
                end if
                if (allocated(series%grid%alir)) then
                    call output_variables_update_values(series%grid%alir, out%ts%grid%alir, its, dnts, 'avg')
                end if
                if (allocated(series%grid%fsout)) then
                    call output_variables_update_values(series%grid%fsout, out%ts%grid%fsout, its, dnts, 'avg')
                end if
                if (allocated(series%grid%gte)) then
                    call output_variables_update_values(series%grid%gte, out%ts%grid%gte, its, dnts, 'avg')
                end if
                if (allocated(series%grid%flout)) then
                    call output_variables_update_values(series%grid%flout, out%ts%grid%flout, its, dnts, 'avg')
                end if
                if (allocated(series%grid%qh)) then
                    call output_variables_update_values(series%grid%qh, out%ts%grid%qh, its, dnts, 'avg')
                end if
                if (allocated(series%grid%qe)) then
                    call output_variables_update_values(series%grid%qe, out%ts%grid%qe, its, dnts, 'avg')
                end if
                if (allocated(series%grid%gzero)) then
                    call output_variables_update_values(series%grid%gzero, out%ts%grid%gzero, its, dnts, 'avg')
                end if
                if (allocated(series%grid%stge)) then
                    call output_variables_update_values(series%grid%stge, out%ts%grid%stge, its, dnts, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (allocated(series%grid%gflx)) then
                        call output_variables_update_values(series%grid%gflx(:, j), out%ts%grid%gflx(:, j), its, dnts, 'avg')
                    end if
                    if (allocated(series%grid%tbar)) then
                        call output_variables_update_values(series%grid%tbar(:, j), out%ts%grid%tbar(:, j), its, dnts, 'avg')
                    end if
                end do
            end if

            !> Channels and routing.
            if (ro%RUNCHNL) then
                if (allocated(series%grid%rff)) then
                    call output_variables_update_values(series%grid%rff, out%ts%grid%rff, its, dnts, 'sum')
                end if
                if (allocated(series%grid%rchg)) then
                    call output_variables_update_values(series%grid%rchg, out%ts%grid%rchg, its, dnts, 'sum')
                end if
                if (allocated(series%grid%qi)) then
                    call output_variables_update_values(series%grid%qi, out%ts%grid%qi, its, dnts, 'avg')
                end if
                if (allocated(series%grid%stgch)) then
                    call output_variables_update_values(series%grid%stgch, out%ts%grid%stgch, its, dnts, 'avg')
                end if
                if (allocated(series%grid%qo)) then
                    call output_variables_update_values(series%grid%qo, out%ts%grid%qo, its, dnts, 'avg')
                end if
                if (allocated(series%grid%zlvl)) then
                    call output_variables_update_values(series%grid%zlvl, out%ts%grid%zlvl, its, dnts, 'avg')
                end if
            end if
        end if

    end subroutine

    !> Description:
    !>  Update output variables from current model states and variables.
    subroutine output_variables_update(shd, cm)

        !> 'shd_variables' required for 'shd'.
        !> 'climate_forcing' required for 'cm'.
        !> 'model_dates' required for 'ic' (counter and time-stepping).
        use shd_variables
        use climate_forcing
        use model_dates

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Local variables.
        integer dnts

        !> Update.
        call output_variables_update_ts(shd, cm)

        !> Totals (e.g., accumulated).
        dnts = 0
        call output_variables_update_series(shd, out%tot, ic%ts_count, dnts)

        !> Yearly.
        if (ic%now%year /= ic%next%year) then
            dnts = ic%ts_yearly
        else
            dnts = 0
        end if
        call output_variables_update_series(shd, out%y, ic%ts_yearly, dnts)

        !> Monthly.
        if (ic%now%month /= ic%next%month) then
            dnts = ic%ts_monthly
        else
            dnts = 0
        end if
        call output_variables_update_series(shd, out%m, ic%ts_monthly, dnts)

        !> Daily.
        if (ic%now%day /= ic%next%day) then
            dnts = ic%ts_daily
        else
            dnts = 0
        end if
        call output_variables_update_series(shd, out%d, ic%ts_daily, dnts)

        !> Hourly.
        if (ic%now%hour /= ic%next%hour) then
            dnts = ic%ts_hourly
        else
            dnts = 0
        end if
        call output_variables_update_series(shd, out%h, ic%ts_hourly, dnts)

    end subroutine

    !> Description:
    !>  Allocate output variables.
    subroutine output_variables_reset_fields(shd, fields)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_fields) fields

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            fields%pre = out%NO_DATA
            fields%fsin = out%NO_DATA
            fields%flin = out%NO_DATA
            fields%ta = out%NO_DATA
            fields%qa = out%NO_DATA
            fields%pres = out%NO_DATA
            fields%uv = out%NO_DATA
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            fields%prec = out%NO_DATA
            fields%evap = out%NO_DATA
            fields%pevp = out%NO_DATA
            fields%evpb = out%NO_DATA
            fields%arrd = out%NO_DATA
            fields%gro = out%NO_DATA
            fields%rof = out%NO_DATA
            fields%rofo = out%NO_DATA
            fields%rofs = out%NO_DATA
            fields%rofb = out%NO_DATA
            fields%rcan = out%NO_DATA
            fields%sncan = out%NO_DATA
            fields%sno = out%NO_DATA
            fields%fsno = out%NO_DATA
            fields%wsno = out%NO_DATA
            fields%zpnd = out%NO_DATA
            fields%pndw = out%NO_DATA
            fields%lzs = out%NO_DATA
            fields%dzs = out%NO_DATA
            fields%stgw = out%NO_DATA
            fields%thlq = out%NO_DATA
            fields%lqws = out%NO_DATA
            fields%thic = out%NO_DATA
            fields%fzws = out%NO_DATA
            fields%alws = out%NO_DATA
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            fields%cmas = out%NO_DATA
            fields%tcan = out%NO_DATA
            fields%tsno = out%NO_DATA
            fields%tpnd = out%NO_DATA
            fields%albt = out%NO_DATA
            fields%alvs = out%NO_DATA
            fields%alir = out%NO_DATA
            fields%fsout = out%NO_DATA
            fields%gte = out%NO_DATA
            fields%flout = out%NO_DATA
            fields%qh = out%NO_DATA
            fields%qe = out%NO_DATA
            fields%gzero = out%NO_DATA
            fields%stge = out%NO_DATA
            fields%gflx = out%NO_DATA
            fields%tbar = out%NO_DATA
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            fields%rff = out%NO_DATA
            fields%rchg = out%NO_DATA
            fields%qi = out%NO_DATA
            fields%stgch = out%NO_DATA
            fields%qo = out%NO_DATA
            fields%zlvl = out%NO_DATA
        end if

    end subroutine

    !> Description:
    !>  Reset output variables to the default NO_DATA value.
    subroutine output_variables_reset(shd, cm)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'climate_forcing' required for 'cm'.
        use shd_variables
        use control_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Tile-based.
        if (ro%RUNTILE) then
            call output_variables_reset_fields(shd, out%ts%tile)
        end if

        !> Grid-based.
        if (ro%RUNGRID) then
            call output_variables_reset_fields(shd, out%ts%grid)
            call output_variables_reset_fields(shd, out%ts%basin)
        end if

    end subroutine

    !> Description:
    !>  Copy the output variable to the 'val' vector.
    subroutine output_variables_val(shd, fields, vname, val, ignd)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(output_fields), intent(in) :: fields
        character(len = *), intent(in) :: vname
        integer, intent(in), optional :: ignd

        !> Output variables.
        real, dimension(:), intent(out) :: val

        !> NO_DATA value.
        val = out%NO_DATA

        !> Copy the variable.
        select case (vname)

            !> Meteorological forcing.
            case (VN_PRE)
                if (ro%RUNCLIM) val = fields%pre
            case (VN_FSIN)
                if (ro%RUNCLIM) val = fields%fsin
            case (VN_FLIN)
                if (ro%RUNCLIM) val = fields%flin
            case (VN_TA)
                if (ro%RUNCLIM) val = fields%ta
            case (VN_QA)
                if (ro%RUNCLIM) val = fields%qa
            case (VN_PRES)
                if (ro%RUNCLIM) val = fields%pres
            case (VN_UV)
                if (ro%RUNCLIM) val = fields%uv

            !> Water balance.
            case (VN_PREC)
                if (ro%RUNBALWB) val = fields%prec
            case (VN_EVAP)
                if (ro%RUNBALWB) val = fields%evap
            case (VN_PEVP)
                if (ro%RUNBALWB) val = fields%pevp
            case (VN_EVPB)
                if (ro%RUNBALWB) val = fields%evpb
            case (VN_ARRD)
                if (ro%RUNBALWB) val = fields%arrd
            case (VN_GRO)
                if (ro%RUNBALWB) val = fields%gro
            case (VN_ROF)
                if (ro%RUNBALWB) val = fields%rof
            case (VN_ROFO)
                if (ro%RUNBALWB) val = fields%rofo
            case (VN_ROFS)
                if (ro%RUNBALWB) val = fields%rofs
            case (VN_ROFB)
                if (ro%RUNBALWB) val = fields%rofb
            case (VN_RCAN)
                if (ro%RUNBALWB) val = fields%rcan
            case (VN_SNCAN)
                if (ro%RUNBALWB) val = fields%sncan
            case (VN_SNO)
                if (ro%RUNBALWB) val = fields%sno
            case (VN_FSNO)
                if (ro%RUNBALWB) val = fields%fsno
            case (VN_WSNO)
                if (ro%RUNBALWB) val = fields%wsno
            case (VN_ZPND)
                if (ro%RUNBALWB) val = fields%zpnd
            case (VN_PNDW)
                if (ro%RUNBALWB) val = fields%pndw
            case (VN_LZS)
                if (ro%RUNBALWB) val = fields%lzs
            case (VN_DZS)
                if (ro%RUNBALWB) val = fields%dzs
            case (VN_STGW)
                if (ro%RUNBALWB) val = fields%stgw
            case (VN_THLQ)
                if (ro%RUNBALWB .and. present(ignd)) then
                    if (ignd > 0) val = fields%thlq(:, ignd)
                end if
            case (VN_LQWS)
                if (ro%RUNBALWB .and. present(ignd)) then
                    if (ignd > 0) val = fields%lqws(:, ignd)
                end if
            case (VN_THIC)
                if (ro%RUNBALWB .and. present(ignd)) then
                    if (ignd > 0) val = fields%thic(:, ignd)
                end if
            case (VN_FZWS)
                if (ro%RUNBALWB .and. present(ignd)) then
                    if (ignd > 0) val = fields%fzws(:, ignd)
                end if
            case (VN_ALWS)
                if (ro%RUNBALWB .and. present(ignd)) then
                    if (ignd > 0) val = fields%alws(:, ignd)
                end if

            !> Energy balance.
            case (VN_CMAS)
                if (ro%RUNBALEB) val = fields%cmas
            case (VN_TCAN)
                if (ro%RUNBALEB) val = fields%tcan
            case (VN_TSNO)
                if (ro%RUNBALEB) val = fields%tsno
            case (VN_TPND)
                if (ro%RUNBALEB) val = fields%tpnd
            case (VN_ALBT)
                if (ro%RUNBALEB) val = fields%albt
            case (VN_ALVS)
                if (ro%RUNBALEB) val = fields%alvs
            case (VN_ALIR)
                if (ro%RUNBALEB) val = fields%alir
            case (VN_FSOUT)
                if (ro%RUNBALEB) val = fields%fsout
            case (VN_GTE)
                if (ro%RUNBALEB) val = fields%gte
            case (VN_FLOUT)
                if (ro%RUNBALEB) val = fields%flout
            case (VN_QH)
                if (ro%RUNBALEB) val = fields%qh
            case (VN_QE)
                if (ro%RUNBALEB) val = fields%qe
            case (VN_GZERO)
                if (ro%RUNBALEB) val = fields%gzero
            case (VN_STGE)
                if (ro%RUNBALEB) val = fields%stge
            case (VN_GFLX)
                if (ro%RUNBALEB .and. present(ignd)) then
                    if (ignd > 0) val = fields%gflx(:, ignd)
                end if
            case (VN_TBAR)
                if (ro%RUNBALEB .and. present(ignd)) then
                    if (ignd > 0) val = fields%tbar(:, ignd)
                end if

            !> Channels and routing.
            case (VN_RFF)
                if (ro%RUNCHNL) val = fields%rff
            case (VN_RCHG)
                if (ro%RUNCHNL) val = fields%rchg
            case (VN_QI)
                if (ro%RUNCHNL) val = fields%qi
            case (VN_STGCH)
                if (ro%RUNCHNL) val = fields%stgch
            case (VN_QO)
                if (ro%RUNCHNL) val = fields%qo
            case (VN_ZLVL)
                if (ro%RUNCHNL) val = fields%zlvl
        end select

    end subroutine

end module
