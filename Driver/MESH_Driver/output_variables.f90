module output_variables

    use variablename_constants

    implicit none

    !> Description:
    !>  Container for output variables.
    type output_fields
        real, dimension(:), pointer :: pre => null()
        real, dimension(:), pointer :: fsin => null()
        real, dimension(:), pointer :: fsvh => null()
        real, dimension(:), pointer :: fsih => null()
        real, dimension(:), pointer :: fsdr => null()
        real, dimension(:), pointer :: fsdf => null()
        real, dimension(:), pointer :: flin => null()
        real, dimension(:), pointer :: ta => null()
        real, dimension(:), pointer :: qa => null()
        real, dimension(:), pointer :: pres => null()
        real, dimension(:), pointer :: uu => null()
        real, dimension(:), pointer :: vv => null()
        real, dimension(:), pointer :: uv => null()
        real, dimension(:), pointer :: wdir => null()
        real, dimension(:), pointer :: prec => null()
        real, dimension(:), pointer :: evap => null()
        real, dimension(:), pointer :: pevp => null()
        real, dimension(:), pointer :: evpb => null()
        real, dimension(:), pointer :: arrd => null()
        real, dimension(:), pointer :: gro => null()
        real, dimension(:), pointer :: rof => null()
        real, dimension(:), pointer :: rofo => null()
        real, dimension(:), pointer :: rofs => null()
        real, dimension(:), pointer :: rofb => null()
        real, dimension(:), pointer :: rcan => null()
        real, dimension(:), pointer :: sncan => null()
        real, dimension(:), pointer :: sno => null()
        real, dimension(:), pointer :: fsno => null()
        real, dimension(:), pointer :: wsno => null()
        real, dimension(:), pointer :: zpnd => null()
        real, dimension(:), pointer :: pndw => null()
        real, dimension(:), pointer :: lzs => null()
        real, dimension(:), pointer :: dzs => null()
        real, dimension(:, :), pointer :: thlq => null()
        real, dimension(:, :), pointer :: lqws => null()
        real, dimension(:, :), pointer :: thic => null()
        real, dimension(:, :), pointer :: fzws => null()
        real, dimension(:, :), pointer :: alws => null()
        real, dimension(:), pointer :: stgw => null()
        real, dimension(:), pointer :: cmas => null()
        real, dimension(:), pointer :: tcan => null()
        real, dimension(:), pointer :: tsno => null()
        real, dimension(:), pointer :: tpnd => null()
        real, dimension(:), pointer :: alvs => null()
        real, dimension(:), pointer :: alir => null()
        real, dimension(:), pointer :: albt => null()
        real, dimension(:), pointer :: fsout => null()
        real, dimension(:), pointer :: flout => null()
        real, dimension(:), pointer :: gte => null()
        real, dimension(:), pointer :: qh => null()
        real, dimension(:), pointer :: qe => null()
        real, dimension(:), pointer :: gzero => null()
        real, dimension(:, :), pointer :: gflx => null()
        real, dimension(:, :), pointer :: tbar => null()
        real, dimension(:), pointer :: stge => null()
        real, dimension(:), pointer :: rff => null()
        real, dimension(:), pointer :: rchg => null()
        real, dimension(:), pointer :: qi => null()
        real, dimension(:), pointer :: stgch => null()
        real, dimension(:), pointer :: qo => null()
        real, dimension(:), pointer :: zlvl => null()
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
        module procedure output_variables_allocate_1d_pntr
        module procedure output_variables_allocate_2d_pntr
    end interface

    contains

    !> Description:
    !>  Allocate and initialize data variable. 'field' is allocated to
    !>  dimension 'n' and set to the 'NO_DATA' value. Associate 'pntr'.
    subroutine output_variables_allocate_1d_pntr(field, n, pntr)

        !> Input/output variables.
        integer, intent(in) :: n
        real, dimension(:), pointer :: field
        real, dimension(:), optional, pointer :: pntr

        !> Allocate and initialize variable
        if (.not. associated(field)) then
            allocate(field(n))
            field = out%NO_DATA
        end if

        !> Associate pointer.
        if (present(pntr)) pntr => field

    end subroutine

    !> Description:
    !>  Allocate and initialize data variable. 'field' is allocated to
    !>  dimension 'n' and 'j', and set to the 'NO_DATA' value.
    !>  Associate 'pntr' provided 'ig'.
    subroutine output_variables_allocate_2d_pntr(field, n, j, pntr, ig)

        !> Input variables.
        integer, intent(in) :: n, j
        integer, intent(in), optional :: ig

        !> Input/output variables.
        real, dimension(:, :), pointer :: field
        real, dimension(:), optional, pointer :: pntr

        !> Allocate and initialize variable
        if (.not. associated(field)) then
            allocate(field(n, j))
            field = out%NO_DATA
        end if

        !> Associate pointer.
        if (present(pntr) .and. present(ig)) pntr => field(:, ig)

    end subroutine

    !> Description:
    !>  Allocate the output variable to 'n' and optionally 'nsl'.
    !>  The value is set to the 'NO_DATA' value.
    subroutine output_variables_allocate_field_pntr(pntr, fields, vname, n, nsl, ig)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input variables.
        type(output_fields), intent(in) :: fields
        character(len = *), intent(in) :: vname
        integer, intent(in) :: n
        integer, intent(in), optional :: nsl, ig

        !> Input/output variables.
        real, dimension(:), pointer :: pntr

        !> Copy the variable.
        select case (vname)

            !> Meteorological forcing.
            case (VN_PRE)
                if (ro%RUNCLIM) call output_variables_allocate(fields%pre, n, pntr)
            case (VN_FSIN)
                if (ro%RUNCLIM) call output_variables_allocate(fields%fsin, n, pntr)
            case (VN_FLIN)
                if (ro%RUNCLIM) call output_variables_allocate(fields%flin, n, pntr)
            case (VN_TA)
                if (ro%RUNCLIM) call output_variables_allocate(fields%ta, n, pntr)
            case (VN_QA)
                if (ro%RUNCLIM) call output_variables_allocate(fields%qa, n, pntr)
            case (VN_PRES)
                if (ro%RUNCLIM) call output_variables_allocate(fields%pres, n, pntr)
            case (VN_UV)
                if (ro%RUNCLIM) call output_variables_allocate(fields%uv, n, pntr)

            !> Water balance.
            case (VN_PREC)
                if (ro%RUNBALWB) call output_variables_allocate(fields%prec, n, pntr)
            case (VN_EVAP)
                if (ro%RUNBALWB) call output_variables_allocate(fields%evap, n, pntr)
            case (VN_PEVP)
                if (ro%RUNBALWB) call output_variables_allocate(fields%pevp, n, pntr)
            case (VN_EVPB)
                if (ro%RUNBALWB) call output_variables_allocate(fields%evpb, n, pntr)
            case (VN_ARRD)
                if (ro%RUNBALWB) call output_variables_allocate(fields%arrd, n, pntr)
            case (VN_GRO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%gro, n, pntr)
            case (VN_ROF)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rof, n, pntr)
            case (VN_ROFO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rofo, n, pntr)
            case (VN_ROFS)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rofs, n, pntr)
            case (VN_ROFB)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rofb, n, pntr)
            case (VN_RCAN)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rcan, n, pntr)
            case (VN_SNCAN)
                if (ro%RUNBALWB) call output_variables_allocate(fields%sncan, n, pntr)
            case (VN_SNO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%sno, n, pntr)
            case (VN_FSNO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%fsno, n, pntr)
            case (VN_WSNO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%wsno, n, pntr)
            case (VN_ZPND)
                if (ro%RUNBALWB) call output_variables_allocate(fields%zpnd, n, pntr)
            case (VN_PNDW)
                if (ro%RUNBALWB) call output_variables_allocate(fields%pndw, n, pntr)
            case (VN_LZS)
                if (ro%RUNBALWB) call output_variables_allocate(fields%lzs, n, pntr)
            case (VN_DZS)
                if (ro%RUNBALWB) call output_variables_allocate(fields%dzs, n, pntr)
            case (VN_STGW)
                if (ro%RUNBALWB) call output_variables_allocate(fields%stgw, n, pntr)
            case (VN_THLQ)
                if (ro%RUNBALWB .and. present(nsl) .and. present(ig)) call output_variables_allocate(fields%thlq, n, nsl, pntr, ig)
            case (VN_LQWS)
                if (ro%RUNBALWB .and. present(nsl) .and. present(ig)) call output_variables_allocate(fields%lqws, n, nsl, pntr, ig)
            case (VN_THIC)
                if (ro%RUNBALWB .and. present(nsl) .and. present(ig)) call output_variables_allocate(fields%thic, n, nsl, pntr, ig)
            case (VN_FZWS)
                if (ro%RUNBALWB .and. present(nsl) .and. present(ig)) call output_variables_allocate(fields%fzws, n, nsl, pntr, ig)
            case (VN_ALWS)
                if (ro%RUNBALWB .and. present(nsl) .and. present(ig)) call output_variables_allocate(fields%alws, n, nsl, pntr, ig)

            !> Energy balance.
            case (VN_CMAS)
                if (ro%RUNBALEB) call output_variables_allocate(fields%cmas, n, pntr)
            case (VN_TCAN)
                if (ro%RUNBALEB) call output_variables_allocate(fields%tcan, n, pntr)
            case (VN_TSNO)
                if (ro%RUNBALEB) call output_variables_allocate(fields%tsno, n, pntr)
            case (VN_TPND)
                if (ro%RUNBALEB) call output_variables_allocate(fields%tpnd, n, pntr)
            case (VN_ALBT)
                if (ro%RUNBALEB) call output_variables_allocate(fields%albt, n, pntr)
            case (VN_ALVS)
                if (ro%RUNBALEB) call output_variables_allocate(fields%alvs, n, pntr)
            case (VN_ALIR)
                if (ro%RUNBALEB) call output_variables_allocate(fields%alir, n, pntr)
            case (VN_FSOUT)
                if (ro%RUNBALEB) call output_variables_allocate(fields%fsout, n, pntr)
            case (VN_GTE)
                if (ro%RUNBALEB) call output_variables_allocate(fields%gte, n, pntr)
            case (VN_FLOUT)
                if (ro%RUNBALEB) call output_variables_allocate(fields%flout, n, pntr)
            case (VN_QH)
                if (ro%RUNBALEB) call output_variables_allocate(fields%qh, n, pntr)
            case (VN_QE)
                if (ro%RUNBALEB) call output_variables_allocate(fields%qe, n, pntr)
            case (VN_GZERO)
                if (ro%RUNBALEB) call output_variables_allocate(fields%gzero, n, pntr)
            case (VN_STGE)
                if (ro%RUNBALEB) call output_variables_allocate(fields%stge, n, pntr)
            case (VN_GFLX)
                if (ro%RUNBALEB .and. present(nsl) .and. present(ig)) call output_variables_allocate(fields%gflx, n, nsl, pntr, ig)
            case (VN_TBAR)
                if (ro%RUNBALEB .and. present(nsl) .and. present(ig)) call output_variables_allocate(fields%tbar, n, nsl, pntr, ig)

            !> Channels and routing.
            case (VN_RFF)
                if (ro%RUNCHNL) call output_variables_allocate(fields%rff, n, pntr)
            case (VN_RCHG)
                if (ro%RUNCHNL) call output_variables_allocate(fields%rchg, n, pntr)
            case (VN_QI)
                if (ro%RUNCHNL) call output_variables_allocate(fields%qi, n, pntr)
            case (VN_STGCH)
                if (ro%RUNCHNL) call output_variables_allocate(fields%stgch, n, pntr)
            case (VN_QO)
                if (ro%RUNCHNL) call output_variables_allocate(fields%qo, n, pntr)
            case (VN_ZLVL)
                if (ro%RUNCHNL) call output_variables_allocate(fields%zlvl, n, pntr)
        end select

    end subroutine

    !> Description:
    !>  Allocate output variables if the model states and variables,
    !>  used in the update routine, are allocated.
    subroutine output_variables_init_fields(shd, cm, series)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'climate_forcing' required for 'cm'.
        !> 'state_variables' required for 'stas'.
        use shd_variables
        use control_variables
        use climate_forcing
        use state_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Input/output variables.
        type(output_series) series

        !> Local variables.
        integer nsl, n
        nsl = shd%lc%IGND

        !> Tile-based.
        if (ro%RUNTILE) then
            n = shd%lc%NML

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (allocated(cm%dat(ck%RT)%GAT)) allocate(series%tile%pre(n))
                if (allocated(cm%dat(ck%FB)%GAT)) allocate(series%tile%fsin(n))
                if (allocated(cm%dat(ck%FI)%GAT)) allocate(series%tile%flin(n))
                if (allocated(cm%dat(ck%TT)%GAT)) allocate(series%tile%ta(n))
                if (allocated(cm%dat(ck%HU)%GAT)) allocate(series%tile%qa(n))
                if (allocated(cm%dat(ck%P0)%GAT)) allocate(series%tile%pres(n))
                if (allocated(cm%dat(ck%UV)%GAT)) allocate(series%tile%uv(n))
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (allocated(cm%dat(ck%RT)%GAT)) allocate(series%tile%prec(n))
                if (allocated(stas%sfc%evap)) allocate(series%tile%evap(n))
                if (allocated(stas%sfc%pevp)) allocate(series%tile%pevp(n))
                if (allocated(stas%sfc%evpb)) allocate(series%tile%evpb(n))
                if (allocated(stas%sfc%arrd)) allocate(series%tile%arrd(n))
                if (allocated(stas%cnpy%gro)) allocate(series%tile%gro(n))
                if (allocated(stas%sfc%rofo) .or. allocated(stas%sl%rofs) .or. allocated(stas%lzs%rofb) .or. &
                    allocated(stas%dzs%rofb)) allocate(series%tile%rof(n))
                if (allocated(stas%sfc%rofo)) allocate(series%tile%rofo(n))
                if (allocated(stas%sl%rofs)) allocate(series%tile%rofs(n))
                if (allocated(stas%lzs%rofb) .and. allocated(stas%dzs%rofb)) allocate(series%tile%rofb(n))
                if (allocated(stas%cnpy%rcan)) allocate(series%tile%rcan(n))
                if (allocated(stas%cnpy%sncan)) allocate(series%tile%sncan(n))
                if (allocated(stas%sno%sno)) allocate(series%tile%sno(n))
                if (allocated(stas%sno%fsno)) allocate(series%tile%fsno(n))
                if (allocated(stas%sno%wsno)) allocate(series%tile%wsno(n))
                if (allocated(stas%sfc%zpnd)) allocate(series%tile%zpnd(n))
                if (allocated(stas%sfc%pndw)) allocate(series%tile%pndw(n))
                if (allocated(stas%lzs%ws)) allocate(series%tile%lzs(n))
                if (allocated(stas%dzs%ws)) allocate(series%tile%dzs(n))
                if (allocated(stas%sl%thlq)) allocate(series%tile%thlq(n, nsl))
                if (allocated(stas%sl%lqws)) allocate(series%tile%lqws(n, nsl))
                if (allocated(stas%sl%thic)) allocate(series%tile%thic(n, nsl))
                if (allocated(stas%sl%fzws)) allocate(series%tile%fzws(n, nsl))
                if (allocated(stas%sl%lqws) .or. allocated(stas%sl%fzws)) allocate(series%tile%alws(n, shd%lc%IGND))
                allocate(series%tile%stgw(n))
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (allocated(stas%cnpy%cmas)) allocate(series%tile%cmas(n))
                if (allocated(stas%cnpy%tcan)) allocate(series%tile%tcan(n))
                if (allocated(stas%sno%tsno)) allocate(series%tile%tsno(n))
                if (allocated(stas%sfc%tpnd)) allocate(series%tile%tpnd(n))
                if (allocated(stas%sfc%alvs)) allocate(series%tile%alvs(n))
                if (allocated(stas%sfc%alir)) allocate(series%tile%alir(n))
                if (allocated(stas%sfc%albt)) allocate(series%tile%albt(n))
                if (allocated(stas%sfc%gte)) allocate(series%tile%gte(n))
                if (allocated(cm%dat(ck%FB)%GAT) .and. allocated(stas%sfc%albt)) allocate(series%tile%fsout(n))
                if (allocated(stas%sfc%gte)) allocate(series%tile%flout(n))
                if (allocated(stas%sfc%hfs)) allocate(series%tile%qh(n))
                if (allocated(stas%sfc%qevp)) allocate(series%tile%qe(n))
                if (allocated(stas%sfc%gzero)) allocate(series%tile%gzero(n))
                if (allocated(stas%sl%gflx)) allocate(series%tile%gflx(n, nsl))
                if (allocated(stas%sl%tbar)) allocate(series%tile%tbar(n, nsl))
                allocate(series%tile%stge(n))
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then
            n = shd%NA

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (allocated(cm%dat(ck%RT)%GRD)) allocate(series%grid%pre(n))
                if (allocated(cm%dat(ck%FB)%GRD)) allocate(series%grid%fsin(n))
                if (allocated(cm%dat(ck%FI)%GRD)) allocate(series%grid%flin(n))
                if (allocated(cm%dat(ck%TT)%GRD)) allocate(series%grid%ta(n))
                if (allocated(cm%dat(ck%HU)%GRD)) allocate(series%grid%qa(n))
                if (allocated(cm%dat(ck%P0)%GRD)) allocate(series%grid%pres(n))
                if (allocated(cm%dat(ck%UV)%GRD)) allocate(series%grid%uv(n))
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (allocated(cm%dat(ck%RT)%GRD)) allocate(series%grid%prec(n))
                if (allocated(stas_grid%sfc%evap)) allocate(series%grid%evap(n))
                if (allocated(stas_grid%sfc%pevp)) allocate(series%grid%pevp(n))
                if (allocated(stas_grid%sfc%evpb)) allocate(series%grid%evpb(n))
                if (allocated(stas_grid%sfc%arrd)) allocate(series%grid%arrd(n))
                if (allocated(stas_grid%cnpy%gro)) allocate(series%grid%gro(n))
                if (allocated(stas_grid%sfc%rofo) .or. allocated(stas_grid%sl%rofs) .or. allocated(stas_grid%lzs%rofb) .or. &
                    allocated(stas_grid%dzs%rofb)) allocate(series%grid%rof(n))
                if (allocated(stas_grid%sfc%rofo)) allocate(series%grid%rofo(n))
                if (allocated(stas_grid%sl%rofs)) allocate(series%grid%rofs(n))
                if (allocated(stas_grid%lzs%rofb) .and. allocated(stas_grid%dzs%rofb)) allocate(series%grid%rofb(n))
                if (allocated(stas_grid%cnpy%rcan)) allocate(series%grid%rcan(n))
                if (allocated(stas_grid%cnpy%sncan)) allocate(series%grid%sncan(n))
                if (allocated(stas_grid%sno%sno)) allocate(series%grid%sno(n))
                if (allocated(stas_grid%sno%fsno)) allocate(series%grid%fsno(n))
                if (allocated(stas_grid%sno%wsno)) allocate(series%grid%wsno(n))
                if (allocated(stas_grid%sfc%zpnd)) allocate(series%grid%zpnd(n))
                if (allocated(stas_grid%sfc%pndw)) allocate(series%grid%pndw(n))
                if (allocated(stas_grid%lzs%ws)) allocate(series%grid%lzs(n))
                if (allocated(stas_grid%dzs%ws)) allocate(series%grid%dzs(n))
                if (allocated(stas_grid%sl%thlq)) allocate(series%grid%thlq(n, nsl))
                if (allocated(stas_grid%sl%lqws)) allocate(series%grid%lqws(n, nsl))
                if (allocated(stas_grid%sl%thic)) allocate(series%grid%thic(n, nsl))
                if (allocated(stas_grid%sl%fzws)) allocate(series%grid%fzws(n, nsl))
                if (allocated(stas_grid%sl%lqws) .or. allocated(stas_grid%sl%fzws)) allocate(series%grid%alws(n, shd%lc%IGND))
                allocate(series%grid%stgw(n))
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (allocated(stas_grid%cnpy%cmas)) allocate(series%grid%cmas(n))
                if (allocated(stas_grid%cnpy%tcan)) allocate(series%grid%tcan(n))
                if (allocated(stas_grid%sno%tsno)) allocate(series%grid%tsno(n))
                if (allocated(stas_grid%sfc%tpnd)) allocate(series%grid%tpnd(n))
                if (allocated(stas_grid%sfc%alvs)) allocate(series%grid%alvs(n))
                if (allocated(stas_grid%sfc%alir)) allocate(series%grid%alir(n))
                if (allocated(stas_grid%sfc%albt)) allocate(series%grid%albt(n))
                if (allocated(stas_grid%sfc%gte)) allocate(series%grid%gte(n))
                if (allocated(cm%dat(ck%FB)%GRD) .and. allocated(stas_grid%sfc%albt)) allocate(series%grid%fsout(n))
                if (allocated(stas_grid%sfc%gte)) allocate(series%grid%flout(n))
                if (allocated(stas_grid%sfc%hfs)) allocate(series%grid%qh(n))
                if (allocated(stas_grid%sfc%qevp)) allocate(series%grid%qe(n))
                if (allocated(stas_grid%sfc%gzero)) allocate(series%grid%gzero(n))
                if (allocated(stas_grid%sl%gflx)) allocate(series%grid%gflx(n, nsl))
                if (allocated(stas_grid%sl%tbar)) allocate(series%grid%tbar(n, nsl))
                allocate(series%grid%stge(n))
            end if

            !> Channels and routing.
            if (ro%RUNCHNL) then
                if (allocated(stas_grid%chnl%rff)) allocate(series%grid%rff(n))
                if (allocated(stas_grid%chnl%rchg)) allocate(series%grid%rchg(n))
                if (allocated(stas_grid%chnl%qi)) allocate(series%grid%qi(n))
                if (allocated(stas_grid%chnl%stg)) allocate(series%grid%stgch(n))
                if (allocated(stas_grid%chnl%qo)) allocate(series%grid%qo(n))
                if (allocated(stas_grid%chnl%zlvl)) allocate(series%grid%zlvl(n))
            end if
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

        !> Allocate and initialize model time-step variables.
        call output_variables_init_fields(shd, cm, out%ts)
        call output_variables_reset_fields(shd, out%ts)

    end subroutine

    !> Description:
    !>  Update output variables from current model states and variables.
    !>  Variables are updated if all elements of the group are equal to
    !>  the 'NO_DATA' value; if not the case, updates are assumed to
    !>  have occured in the model (e.g., by process modules), and those
    !>  values are preserved.
    !>  The model time-step output variables are allocated according
    !>  to the allocation status of the model states and variables;
    !>  checks are made against the model time-step output variable
    !>  to see if it has been associated.
    subroutine output_variables_update_ts(shd, cm)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'climate_forcing' required for 'cm'.
        !> 'state_variables' required for 'stas'.
        use shd_variables
        use control_variables
        use climate_forcing
        use state_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Local variables.
        logical lcheck

        !> Tile-based.
        if (ro%RUNTILE) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (associated(out%ts%tile%pre)) then
                    if (all(out%ts%tile%pre == out%NO_DATA)) out%ts%tile%pre = cm%dat(ck%RT)%GAT
                end if
                if (associated(out%ts%tile%fsin)) then
                    if (all(out%ts%tile%fsin == out%NO_DATA)) out%ts%tile%fsin = cm%dat(ck%FB)%GAT
                end if
                if (associated(out%ts%tile%flin)) then
                    if (all(out%ts%tile%flin == out%NO_DATA)) out%ts%tile%flin = cm%dat(ck%FI)%GAT
                end if
                if (associated(out%ts%tile%ta)) then
                    if (all(out%ts%tile%ta == out%NO_DATA)) out%ts%tile%ta = cm%dat(ck%TT)%GAT
                end if
                if (associated(out%ts%tile%qa)) then
                    if (all(out%ts%tile%qa == out%NO_DATA)) out%ts%tile%qa = cm%dat(ck%HU)%GAT
                end if
                if (associated(out%ts%tile%pres)) then
                    if (all(out%ts%tile%pres == out%NO_DATA)) out%ts%tile%pres = cm%dat(ck%P0)%GAT
                end if
                if (associated(out%ts%tile%uv)) then
                    if (all(out%ts%tile%uv == out%NO_DATA)) out%ts%tile%uv = cm%dat(ck%UV)%GAT
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(out%ts%tile%prec)) then
                    if (all(out%ts%tile%prec == out%NO_DATA)) out%ts%tile%prec = cm%dat(ck%RT)%GAT
                end if
                if (associated(out%ts%tile%evap)) then
                    if (all(out%ts%tile%evap == out%NO_DATA)) out%ts%tile%evap = stas%sfc%evap
                end if
                if (associated(out%ts%tile%pevp)) then
                    if (all(out%ts%tile%pevp == out%NO_DATA)) out%ts%tile%pevp = stas%sfc%pevp
                end if
                if (associated(out%ts%tile%evpb)) then
                    if (all(out%ts%tile%evpb == out%NO_DATA)) out%ts%tile%evpb = stas%sfc%evpb
                end if
                if (associated(out%ts%tile%arrd)) then
                    if (all(out%ts%tile%arrd == out%NO_DATA)) out%ts%tile%arrd = stas%sfc%arrd
                end if
                if (associated(out%ts%tile%gro)) then
                    if (all(out%ts%tile%gro == out%NO_DATA)) out%ts%tile%gro = stas%cnpy%gro
                end if
                if (associated(out%ts%tile%rof)) then
                    if (all(out%ts%tile%rof == out%NO_DATA)) then
                        out%ts%tile%rof = 0.0
                        lcheck = .true.
                    else
                        lcheck = .false.
                    end if
                    if (associated(out%ts%tile%rofo)) then
                        if (all(out%ts%tile%rofo == out%NO_DATA)) out%ts%tile%rofo = stas%sfc%rofo
                        if (lcheck) out%ts%tile%rof = out%ts%tile%rof + stas%sfc%rofo
                    end if
                    if (associated(out%ts%tile%rofs)) then
                        if (all(out%ts%tile%rofs == out%NO_DATA)) out%ts%tile%rofs = stas%sl%rofs
                        if (lcheck) out%ts%tile%rof = out%ts%tile%rof + stas%sl%rofs
                    end if
                    if (associated(out%ts%tile%rofb)) then
                        if (all(out%ts%tile%rofb == out%NO_DATA)) out%ts%tile%rofb = stas%lzs%rofb + stas%dzs%rofb
                        if (lcheck) out%ts%tile%rof = out%ts%tile%rof + stas%lzs%rofb + stas%dzs%rofb
                    end if
                end if
                if (associated(out%ts%tile%rcan)) then
                    if (all(out%ts%tile%rcan == out%NO_DATA)) out%ts%tile%rcan = stas%cnpy%rcan
                end if
                if (associated(out%ts%tile%sncan)) then
                    if (all(out%ts%tile%sncan == out%NO_DATA)) out%ts%tile%sncan = stas%cnpy%sncan
                end if
                if (associated(out%ts%tile%sno)) then
                    if (all(out%ts%tile%sno == out%NO_DATA)) out%ts%tile%sno = stas%sno%sno
                end if
                if (associated(out%ts%tile%fsno)) then
                    if (all(out%ts%tile%fsno == out%NO_DATA)) out%ts%tile%fsno = stas%sno%fsno
                end if
                if (associated(out%ts%tile%wsno)) then
                    if (all(out%ts%tile%wsno == out%NO_DATA)) out%ts%tile%wsno = stas%sno%wsno
                end if
                if (associated(out%ts%tile%zpnd)) then
                    if (all(out%ts%tile%zpnd == out%NO_DATA)) out%ts%tile%zpnd = stas%sfc%zpnd
                end if
                if (associated(out%ts%tile%pndw)) then
                    if (all(out%ts%tile%pndw == out%NO_DATA)) out%ts%tile%pndw = stas%sfc%pndw
                end if
                if (associated(out%ts%tile%lzs)) then
                    if (all(out%ts%tile%lzs == out%NO_DATA)) out%ts%tile%lzs = stas%lzs%ws
                end if
                if (associated(out%ts%tile%dzs)) then
                    if (all(out%ts%tile%dzs == out%NO_DATA)) out%ts%tile%dzs = stas%dzs%ws
                end if
                if (associated(out%ts%tile%thlq)) then
                    if (all(out%ts%tile%thlq == out%NO_DATA)) out%ts%tile%thlq = stas%sl%thlq
                end if
                if (associated(out%ts%tile%thic)) then
                    if (all(out%ts%tile%thic == out%NO_DATA)) out%ts%tile%thic = stas%sl%thic
                end if
                if (associated(out%ts%tile%alws)) then
                    if (all(out%ts%tile%alws == out%NO_DATA)) then
                        out%ts%tile%alws = 0.0
                        lcheck = .true.
                    else
                        lcheck = .false.
                    end if
                    if (associated(out%ts%tile%lqws)) then
                        if (all(out%ts%tile%lqws == out%NO_DATA)) out%ts%tile%lqws = stas%sl%lqws
                        if (lcheck) out%ts%tile%alws = out%ts%tile%alws + stas%sl%lqws
                    end if
                    if (associated(out%ts%tile%fzws)) then
                        if (all(out%ts%tile%fzws == out%NO_DATA)) out%ts%tile%fzws = stas%sl%fzws
                        if (lcheck) out%ts%tile%alws = out%ts%tile%alws + stas%sl%fzws
                    end if
                end if
!                if (all(out%ts%tile%stgw == out%NO_DATA)) out%ts%tile%stgw =
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(out%ts%tile%cmas)) then
                    if (all(out%ts%tile%cmas == out%NO_DATA)) out%ts%tile%cmas = stas%cnpy%cmas
                end if
                if (associated(out%ts%tile%tcan)) then
                    if (all(out%ts%tile%tcan == out%NO_DATA)) out%ts%tile%tcan = stas%cnpy%tcan
                end if
                if (associated(out%ts%tile%tsno)) then
                    if (all(out%ts%tile%tsno == out%NO_DATA)) out%ts%tile%tsno = stas%sno%tsno
                end if
                if (associated(out%ts%tile%tpnd)) then
                    if (all(out%ts%tile%tpnd == out%NO_DATA)) out%ts%tile%tpnd = stas%sfc%tpnd
                end if
                if (associated(out%ts%tile%alvs)) then
                    if (all(out%ts%tile%alvs == out%NO_DATA)) out%ts%tile%alvs = stas%sfc%alvs
                end if
                if (associated(out%ts%tile%alir)) then
                    if (all(out%ts%tile%alir == out%NO_DATA)) out%ts%tile%alir = stas%sfc%alir
                end if
                if (associated(out%ts%tile%albt)) then
                    if (all(out%ts%tile%albt == out%NO_DATA)) out%ts%tile%albt = stas%sfc%albt
                end if
                if (associated(out%ts%tile%gte)) then
                    if (all(out%ts%tile%gte == out%NO_DATA)) out%ts%tile%gte = stas%sfc%gte
                end if
                if (associated(out%ts%tile%fsout)) then
                    if (all(out%ts%tile%fsout == out%NO_DATA)) out%ts%tile%fsout = cm%dat(ck%FB)%GAT*(1.0 - stas%sfc%albt)
                end if
                if (associated(out%ts%tile%flout)) then
                    if (all(out%ts%tile%flout == out%NO_DATA)) out%ts%tile%flout = 5.66796E-8*stas%sfc%gte**4
                end if
                if (associated(out%ts%tile%qh)) then
                    if (all(out%ts%tile%qh == out%NO_DATA)) out%ts%tile%qh = stas%sfc%hfs
                end if
                if (associated(out%ts%tile%qe)) then
                    if (all(out%ts%tile%qe == out%NO_DATA)) out%ts%tile%qe = stas%sfc%qevp
                end if
                if (associated(out%ts%tile%gzero)) then
                    if (all(out%ts%tile%gzero == out%NO_DATA)) out%ts%tile%gzero = stas%sfc%gzero
                end if
                if (associated(out%ts%tile%gflx)) then
                    if (all(out%ts%tile%gflx == out%NO_DATA)) out%ts%tile%gflx = stas%sl%gflx
                end if
                if (associated(out%ts%tile%tbar)) then
                    if (all(out%ts%tile%tbar == out%NO_DATA)) out%ts%tile%tbar = stas%sl%tbar
                end if
!                if (all(out%ts%tile%stge == out%NO_DATA)) out%ts%tile%stge =
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (associated(out%ts%grid%pre)) then
                    if (all(out%ts%grid%pre == out%NO_DATA)) out%ts%grid%pre = cm%dat(ck%RT)%GRD
                end if
                if (associated(out%ts%grid%fsin)) then
                    if (all(out%ts%grid%fsin == out%NO_DATA)) out%ts%grid%fsin = cm%dat(ck%FB)%GRD
                end if
                if (associated(out%ts%grid%flin)) then
                    if (all(out%ts%grid%flin == out%NO_DATA)) out%ts%grid%flin = cm%dat(ck%FI)%GRD
                end if
                if (associated(out%ts%grid%ta)) then
                    if (all(out%ts%grid%ta == out%NO_DATA)) out%ts%grid%ta = cm%dat(ck%TT)%GRD
                end if
                if (associated(out%ts%grid%qa)) then
                    if (all(out%ts%grid%qa == out%NO_DATA)) out%ts%grid%qa = cm%dat(ck%HU)%GRD
                end if
                if (associated(out%ts%grid%pres)) then
                    if (all(out%ts%grid%pres == out%NO_DATA)) out%ts%grid%pres = cm%dat(ck%P0)%GRD
                end if
                if (associated(out%ts%grid%uv)) then
                    if (all(out%ts%grid%uv == out%NO_DATA)) out%ts%grid%uv = cm%dat(ck%UV)%GRD
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(out%ts%grid%prec)) then
                    if (all(out%ts%grid%prec == out%NO_DATA)) out%ts%grid%prec = cm%dat(ck%RT)%GRD
                end if
                if (associated(out%ts%grid%evap)) then
                    if (all(out%ts%grid%evap == out%NO_DATA)) out%ts%grid%evap = stas_grid%sfc%evap
                end if
                if (associated(out%ts%grid%pevp)) then
                    if (all(out%ts%grid%pevp == out%NO_DATA)) out%ts%grid%pevp = stas_grid%sfc%pevp
                end if
                if (associated(out%ts%grid%evpb)) then
                    if (all(out%ts%grid%evpb == out%NO_DATA)) out%ts%grid%evpb = stas_grid%sfc%evpb
                end if
                if (associated(out%ts%grid%arrd)) then
                    if (all(out%ts%grid%arrd == out%NO_DATA)) out%ts%grid%arrd = stas_grid%sfc%arrd
                end if
                if (associated(out%ts%grid%gro)) then
                    if (all(out%ts%grid%gro == out%NO_DATA)) out%ts%grid%gro = stas_grid%cnpy%gro
                end if
                if (associated(out%ts%grid%rof)) then
                    if (all(out%ts%grid%rof == out%NO_DATA)) then
                        out%ts%grid%rof = 0.0
                        lcheck = .true.
                    else
                        lcheck = .false.
                    end if
                    if (associated(out%ts%grid%rofo)) then
                        if (all(out%ts%grid%rofo == out%NO_DATA)) out%ts%grid%rofo = stas_grid%sfc%rofo
                        if (lcheck) out%ts%grid%rof = out%ts%grid%rof + stas_grid%sfc%rofo
                    end if
                    if (associated(out%ts%grid%rofs)) then
                        if (all(out%ts%grid%rofs == out%NO_DATA)) out%ts%grid%rofs = stas_grid%sl%rofs
                        if (lcheck) out%ts%grid%rof = out%ts%grid%rof + stas_grid%sl%rofs
                    end if
                    if (associated(out%ts%grid%rofb)) then
                        if (all(out%ts%grid%rofb == out%NO_DATA)) out%ts%grid%rofb = stas_grid%lzs%rofb + stas_grid%dzs%rofb
                        if (lcheck) out%ts%grid%rof = out%ts%grid%rof + stas_grid%lzs%rofb + stas_grid%dzs%rofb
                    end if
                end if
                if (associated(out%ts%grid%rcan)) then
                    if (all(out%ts%grid%rcan == out%NO_DATA)) out%ts%grid%rcan = stas_grid%cnpy%rcan
                end if
                if (associated(out%ts%grid%sncan)) then
                    if (all(out%ts%grid%sncan == out%NO_DATA)) out%ts%grid%sncan = stas_grid%cnpy%sncan
                end if
                if (associated(out%ts%grid%sno)) then
                    if (all(out%ts%grid%sno == out%NO_DATA)) out%ts%grid%sno = stas_grid%sno%sno
                end if
                if (associated(out%ts%grid%fsno)) then
                    if (all(out%ts%grid%fsno == out%NO_DATA)) out%ts%grid%fsno = stas_grid%sno%fsno
                end if
                if (associated(out%ts%grid%wsno)) then
                    if (all(out%ts%grid%wsno == out%NO_DATA)) out%ts%grid%wsno = stas_grid%sno%wsno
                end if
                if (associated(out%ts%grid%zpnd)) then
                    if (all(out%ts%grid%zpnd == out%NO_DATA)) out%ts%grid%zpnd = stas_grid%sfc%zpnd
                end if
                if (associated(out%ts%grid%pndw)) then
                    if (all(out%ts%grid%pndw == out%NO_DATA)) out%ts%grid%pndw = stas_grid%sfc%pndw
                end if
                if (associated(out%ts%grid%lzs)) then
                    if (all(out%ts%grid%lzs == out%NO_DATA)) out%ts%grid%lzs = stas_grid%lzs%ws
                end if
                if (associated(out%ts%grid%dzs)) then
                    if (all(out%ts%grid%dzs == out%NO_DATA)) out%ts%grid%dzs = stas_grid%dzs%ws
                end if
                if (associated(out%ts%grid%thlq)) then
                    if (all(out%ts%grid%thlq == out%NO_DATA)) out%ts%grid%thlq = stas_grid%sl%thlq
                end if
                if (associated(out%ts%grid%thic)) then
                    if (all(out%ts%grid%thic == out%NO_DATA)) out%ts%grid%thic = stas_grid%sl%thic
                end if
                if (associated(out%ts%grid%alws)) then
                    if (all(out%ts%grid%alws == out%NO_DATA)) then
                        out%ts%grid%alws = 0.0
                        lcheck = .true.
                    else
                        lcheck = .false.
                    end if
                    if (associated(out%ts%grid%lqws)) then
                        if (all(out%ts%grid%lqws == out%NO_DATA)) out%ts%grid%lqws = stas_grid%sl%lqws
                        if (lcheck) out%ts%grid%alws = out%ts%grid%alws + stas_grid%sl%lqws
                    end if
                    if (associated(out%ts%grid%fzws)) then
                        if (all(out%ts%grid%fzws == out%NO_DATA)) out%ts%grid%fzws = stas_grid%sl%fzws
                        if (lcheck) out%ts%grid%alws = out%ts%grid%alws + stas_grid%sl%fzws
                    end if
                end if
!                if (all(out%ts%grid%stgw == out%NO_DATA)) out%ts%grid%stgw =
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(out%ts%grid%cmas)) then
                    if (all(out%ts%grid%cmas == out%NO_DATA)) out%ts%grid%cmas = stas_grid%cnpy%cmas
                end if
                if (associated(out%ts%grid%tcan)) then
                    if (all(out%ts%grid%tcan == out%NO_DATA)) out%ts%grid%tcan = stas_grid%cnpy%tcan
                end if
                if (associated(out%ts%grid%tsno)) then
                    if (all(out%ts%grid%tsno == out%NO_DATA)) out%ts%grid%tsno = stas_grid%sno%tsno
                end if
                if (associated(out%ts%grid%tpnd)) then
                    if (all(out%ts%grid%tpnd == out%NO_DATA)) out%ts%grid%tpnd = stas_grid%sfc%tpnd
                end if
                if (associated(out%ts%grid%alvs)) then
                    if (all(out%ts%grid%alvs == out%NO_DATA)) out%ts%grid%alvs = stas_grid%sfc%alvs
                end if
                if (associated(out%ts%grid%alir)) then
                    if (all(out%ts%grid%alir == out%NO_DATA)) out%ts%grid%alir = stas_grid%sfc%alir
                end if
                if (associated(out%ts%grid%albt)) then
                    if (all(out%ts%grid%albt == out%NO_DATA)) out%ts%grid%albt = stas_grid%sfc%albt
                end if
                if (associated(out%ts%grid%gte)) then
                    if (all(out%ts%grid%gte == out%NO_DATA)) out%ts%grid%gte = stas_grid%sfc%gte
                end if
                if (associated(out%ts%grid%fsout)) then
                    if (all(out%ts%grid%fsout == out%NO_DATA)) out%ts%grid%fsout = cm%dat(ck%FB)%GRD*(1.0 - stas_grid%sfc%albt)
                end if
                if (associated(out%ts%grid%flout)) then
                    if (all(out%ts%grid%flout == out%NO_DATA)) out%ts%grid%flout = 5.66796E-8*stas_grid%sfc%gte**4
                end if
                if (associated(out%ts%grid%qh)) then
                    if (all(out%ts%grid%qh == out%NO_DATA)) out%ts%grid%qh = stas_grid%sfc%hfs
                end if
                if (associated(out%ts%grid%qe)) then
                    if (all(out%ts%grid%qe == out%NO_DATA)) out%ts%grid%qe = stas_grid%sfc%qevp
                end if
                if (associated(out%ts%grid%gzero)) then
                    if (all(out%ts%grid%gzero == out%NO_DATA)) out%ts%grid%gzero = stas_grid%sfc%gzero
                end if
                if (associated(out%ts%grid%gflx)) then
                    if (all(out%ts%grid%gflx == out%NO_DATA)) out%ts%grid%gflx = stas_grid%sl%gflx
                end if
                if (associated(out%ts%grid%tbar)) then
                    if (all(out%ts%grid%tbar == out%NO_DATA)) out%ts%grid%tbar = stas_grid%sl%tbar
                end if
!                if (all(out%ts%grid%stge == out%NO_DATA)) out%ts%grid%stge =
            end if

            !> Channels and routing.
            if (ro%RUNCHNL) then
                if (associated(out%ts%grid%rff)) then
                    if (all(out%ts%grid%rff == out%NO_DATA)) out%ts%grid%rff = stas_grid%chnl%rff
                end if
                if (associated(out%ts%grid%rchg)) then
                    if (all(out%ts%grid%rchg == out%NO_DATA)) out%ts%grid%rchg = stas_grid%chnl%rchg
                end if
                if (associated(out%ts%grid%qi)) then
                    if (all(out%ts%grid%qi == out%NO_DATA)) out%ts%grid%qi = stas_grid%chnl%qi
                end if
                if (associated(out%ts%grid%stgch)) then
                    if (all(out%ts%grid%stgch == out%NO_DATA)) out%ts%grid%stgch = stas_grid%chnl%stg
                end if
                if (associated(out%ts%grid%qo)) then
                    if (all(out%ts%grid%qo == out%NO_DATA)) out%ts%grid%qo = stas_grid%chnl%qo
                end if
!                if (associated(out%ts%grid%zlvl)) then
!                    if (all(out%ts%grid%zlvl == out%NO_DATA)) out%ts%grid%zlvl = stas_grid%chnl%zlvl
!                end if
            end if
        end if

    end subroutine

    !> Description:
    !>  Update the 'dat' vector using the 'val' vector.
    !>  Reset 'dat' if the time-step of the current interval "its" is 1.
    !>  Calculate an average if the function "fn" is 'avg' and the
    !>  time-steps to divide the number "dnts" by is greater than zero.
    !>  Override calculations where 'val' is equal to the 'NO_DATA'
    !>  value with the 'NO_DATA' value in 'dat'.
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
                if (associated(series%tile%pre)) then
                    call output_variables_update_values(series%tile%pre, out%ts%tile%pre, its, dnts, 'avg')
                end if
                if (associated(series%tile%fsin)) then
                    call output_variables_update_values(series%tile%fsin, out%ts%tile%fsin, its, dnts, 'avg')
                end if
                if (associated(series%tile%flin)) then
                    call output_variables_update_values(series%tile%flin, out%ts%tile%flin, its, dnts, 'avg')
                end if
                if (associated(series%tile%ta)) then
                    call output_variables_update_values(series%tile%ta, out%ts%tile%ta, its, dnts, 'avg')
                end if
                if (associated(series%tile%qa)) then
                    call output_variables_update_values(series%tile%qa, out%ts%tile%qa, its, dnts, 'avg')
                end if
                if (associated(series%tile%pres)) then
                    call output_variables_update_values(series%tile%pres, out%ts%tile%pres, its, dnts, 'avg')
                end if
                if (associated(series%tile%uv)) then
                    call output_variables_update_values(series%tile%uv, out%ts%tile%uv, its, dnts, 'avg')
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(series%tile%prec)) then
                    call output_variables_update_values(series%tile%prec, out%ts%tile%prec, its, dnts, 'sum')
                end if
                if (associated(series%tile%evap)) then
                    call output_variables_update_values(series%tile%evap, out%ts%tile%evap, its, dnts, 'sum')
                end if
                if (associated(series%tile%pevp)) then
                    call output_variables_update_values(series%tile%pevp, out%ts%tile%pevp, its, dnts, 'sum')
                end if
                if (associated(series%tile%evpb)) then
                    call output_variables_update_values(series%tile%evpb, out%ts%tile%evpb, its, dnts, 'avg')
                end if
                if (associated(series%tile%arrd)) then
                    call output_variables_update_values(series%tile%arrd, out%ts%tile%arrd, its, dnts, 'avg')
                end if
                if (associated(series%tile%gro)) then
                    call output_variables_update_values(series%tile%gro, out%ts%tile%gro, its, dnts, 'avg')
                end if
                if (associated(series%tile%rof)) then
                    call output_variables_update_values(series%tile%rof, out%ts%tile%rof, its, dnts, 'sum')
                end if
                if (associated(series%tile%rofo)) then
                    call output_variables_update_values(series%tile%rofo, out%ts%tile%rofo, its, dnts, 'sum')
                end if
                if (associated(series%tile%rofs)) then
                    call output_variables_update_values(series%tile%rofs, out%ts%tile%rofs, its, dnts, 'sum')
                end if
                if (associated(series%tile%rofb)) then
                    call output_variables_update_values(series%tile%rofb, out%ts%tile%rofb, its, dnts, 'sum')
                end if
                if (associated(series%tile%rcan)) then
                    call output_variables_update_values(series%tile%rcan, out%ts%tile%rcan, its, dnts, 'sum')
                end if
                if (associated(series%tile%sncan)) then
                    call output_variables_update_values(series%tile%sncan, out%ts%tile%sncan, its, dnts, 'sum')
                end if
                if (associated(series%tile%sno)) then
                    call output_variables_update_values(series%tile%sno, out%ts%tile%sno, its, dnts, 'sum')
                end if
                if (associated(series%tile%fsno)) then
                    call output_variables_update_values(series%tile%fsno, out%ts%tile%fsno, its, dnts, 'sum')
                end if
                if (associated(series%tile%wsno)) then
                    call output_variables_update_values(series%tile%wsno, out%ts%tile%wsno, its, dnts, 'sum')
                end if
                if (associated(series%tile%zpnd)) then
                    call output_variables_update_values(series%tile%zpnd, out%ts%tile%zpnd, its, dnts, 'avg')
                end if
                if (associated(series%tile%pndw)) then
                    call output_variables_update_values(series%tile%pndw, out%ts%tile%pndw, its, dnts, 'sum')
                end if
                if (associated(series%tile%lzs)) then
                    call output_variables_update_values(series%tile%lzs, out%ts%tile%lzs, its, dnts, 'sum')
                end if
                if (associated(series%tile%dzs)) then
                    call output_variables_update_values(series%tile%dzs, out%ts%tile%dzs, its, dnts, 'sum')
                end if
                if (associated(series%tile%stgw)) then
                    call output_variables_update_values(series%tile%stgw, out%ts%tile%stgw, its, dnts, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%tile%thlq)) then
                        call output_variables_update_values(series%tile%thlq(:, j), out%ts%tile%thlq(:, j), its, dnts, 'avg')
                    end if
                    if (associated(series%tile%lqws)) then
                        call output_variables_update_values(series%tile%lqws(:, j), out%ts%tile%lqws(:, j), its, dnts, 'sum')
                    end if
                    if (associated(series%tile%thic)) then
                        call output_variables_update_values(series%tile%thic(:, j), out%ts%tile%thic(:, j), its, dnts, 'avg')
                    end if
                    if (associated(series%tile%fzws)) then
                        call output_variables_update_values(series%tile%fzws(:, j), out%ts%tile%fzws(:, j), its, dnts, 'sum')
                    end if
                    if (associated(series%tile%alws)) then
                        call output_variables_update_values(series%tile%alws(:, j), out%ts%tile%alws(:, j), its, dnts, 'sum')
                    end if
                end do
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(series%tile%cmas)) then
                    call output_variables_update_values(series%tile%cmas, out%ts%tile%cmas, its, dnts, 'sum')
                end if
                if (associated(series%tile%tcan)) then
                    call output_variables_update_values(series%tile%tcan, out%ts%tile%tcan, its, dnts, 'avg')
                end if
                if (associated(series%tile%tsno)) then
                    call output_variables_update_values(series%tile%tsno, out%ts%tile%tsno, its, dnts, 'avg')
                end if
                if (associated(series%tile%tpnd)) then
                    call output_variables_update_values(series%tile%tpnd, out%ts%tile%tpnd, its, dnts, 'avg')
                end if
                if (associated(series%tile%albt)) then
                    call output_variables_update_values(series%tile%albt, out%ts%tile%albt, its, dnts, 'avg')
                end if
                if (associated(series%tile%alvs)) then
                    call output_variables_update_values(series%tile%alvs, out%ts%tile%alvs, its, dnts, 'avg')
                end if
                if (associated(series%tile%alir)) then
                    call output_variables_update_values(series%tile%alir, out%ts%tile%alir, its, dnts, 'avg')
                end if
                if (associated(series%tile%fsout)) then
                    call output_variables_update_values(series%tile%fsout, out%ts%tile%fsout, its, dnts, 'avg')
                end if
                if (associated(series%tile%gte)) then
                    call output_variables_update_values(series%tile%gte, out%ts%tile%gte, its, dnts, 'avg')
                end if
                if (associated(series%tile%flout)) then
                    call output_variables_update_values(series%tile%flout, out%ts%tile%flout, its, dnts, 'avg')
                end if
                if (associated(series%tile%qh)) then
                    call output_variables_update_values(series%tile%qh, out%ts%tile%qh, its, dnts, 'avg')
                end if
                if (associated(series%tile%qe)) then
                    call output_variables_update_values(series%tile%qe, out%ts%tile%qe, its, dnts, 'avg')
                end if
                if (associated(series%tile%gzero)) then
                    call output_variables_update_values(series%tile%gzero, out%ts%tile%gzero, its, dnts, 'avg')
                end if
                if (associated(series%tile%stge)) then
                    call output_variables_update_values(series%tile%stge, out%ts%tile%stge, its, dnts, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%tile%gflx)) then
                        call output_variables_update_values(series%tile%gflx(:, j), out%ts%tile%gflx(:, j), its, dnts, 'avg')
                    end if
                    if (associated(series%tile%tbar)) then
                        call output_variables_update_values(series%tile%tbar(:, j), out%ts%tile%tbar(:, j), its, dnts, 'avg')
                    end if
                end do
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (associated(series%grid%pre)) then
                    call output_variables_update_values(series%grid%pre, out%ts%grid%pre, its, dnts, 'avg')
                end if
                if (associated(series%grid%fsin)) then
                    call output_variables_update_values(series%grid%fsin, out%ts%grid%fsin, its, dnts, 'avg')
                end if
                if (associated(series%grid%flin)) then
                    call output_variables_update_values(series%grid%flin, out%ts%grid%flin, its, dnts, 'avg')
                end if
                if (associated(series%grid%ta)) then
                    call output_variables_update_values(series%grid%ta, out%ts%grid%ta, its, dnts, 'avg')
                end if
                if (associated(series%grid%qa)) then
                    call output_variables_update_values(series%grid%qa, out%ts%grid%qa, its, dnts, 'avg')
                end if
                if (associated(series%grid%pres)) then
                    call output_variables_update_values(series%grid%pres, out%ts%grid%pres, its, dnts, 'avg')
                end if
                if (associated(series%grid%uv)) then
                    call output_variables_update_values(series%grid%uv, out%ts%grid%uv, its, dnts, 'avg')
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(series%grid%prec)) then
                    call output_variables_update_values(series%grid%prec, out%ts%grid%prec, its, dnts, 'sum')
                end if
                if (associated(series%grid%evap)) then
                    call output_variables_update_values(series%grid%evap, out%ts%grid%evap, its, dnts, 'sum')
                end if
                if (associated(series%grid%pevp)) then
                    call output_variables_update_values(series%grid%pevp, out%ts%grid%pevp, its, dnts, 'sum')
                end if
                if (associated(series%grid%evpb)) then
                    call output_variables_update_values(series%grid%evpb, out%ts%grid%evpb, its, dnts, 'avg')
                end if
                if (associated(series%grid%arrd)) then
                    call output_variables_update_values(series%grid%arrd, out%ts%grid%arrd, its, dnts, 'avg')
                end if
                if (associated(series%grid%gro)) then
                    call output_variables_update_values(series%grid%gro, out%ts%grid%gro, its, dnts, 'avg')
                end if
                if (associated(series%grid%rof)) then
                    call output_variables_update_values(series%grid%rof, out%ts%grid%rof, its, dnts, 'sum')
                end if
                if (associated(series%grid%rofo)) then
                    call output_variables_update_values(series%grid%rofo, out%ts%grid%rofo, its, dnts, 'sum')
                end if
                if (associated(series%grid%rofs)) then
                    call output_variables_update_values(series%grid%rofs, out%ts%grid%rofs, its, dnts, 'sum')
                end if
                if (associated(series%grid%rofb)) then
                    call output_variables_update_values(series%grid%rofb, out%ts%grid%rofb, its, dnts, 'sum')
                end if
                if (associated(series%grid%rcan)) then
                    call output_variables_update_values(series%grid%rcan, out%ts%grid%rcan, its, dnts, 'sum')
                end if
                if (associated(series%grid%sncan)) then
                    call output_variables_update_values(series%grid%sncan, out%ts%grid%sncan, its, dnts, 'sum')
                end if
                if (associated(series%grid%sno)) then
                    call output_variables_update_values(series%grid%sno, out%ts%grid%sno, its, dnts, 'sum')
                end if
                if (associated(series%grid%fsno)) then
                    call output_variables_update_values(series%grid%fsno, out%ts%grid%fsno, its, dnts, 'sum')
                end if
                if (associated(series%grid%wsno)) then
                    call output_variables_update_values(series%grid%wsno, out%ts%grid%wsno, its, dnts, 'sum')
                end if
                if (associated(series%grid%zpnd)) then
                    call output_variables_update_values(series%grid%zpnd, out%ts%grid%zpnd, its, dnts, 'avg')
                end if
                if (associated(series%grid%pndw)) then
                    call output_variables_update_values(series%grid%pndw, out%ts%grid%pndw, its, dnts, 'sum')
                end if
                if (associated(series%grid%lzs)) then
                    call output_variables_update_values(series%grid%lzs, out%ts%grid%lzs, its, dnts, 'sum')
                end if
                if (associated(series%grid%dzs)) then
                    call output_variables_update_values(series%grid%dzs, out%ts%grid%dzs, its, dnts, 'sum')
                end if
                if (associated(series%grid%stgw)) then
                    call output_variables_update_values(series%grid%stgw, out%ts%grid%stgw, its, dnts, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%grid%thlq)) then
                        call output_variables_update_values(series%grid%thlq(:, j), out%ts%grid%thlq(:, j), its, dnts, 'avg')
                    end if
                    if (associated(series%grid%lqws)) then
                        call output_variables_update_values(series%grid%lqws(:, j), out%ts%grid%lqws(:, j), its, dnts, 'sum')
                    end if
                    if (associated(series%grid%thic)) then
                        call output_variables_update_values(series%grid%thic(:, j), out%ts%grid%thic(:, j), its, dnts, 'avg')
                    end if
                    if (associated(series%grid%fzws)) then
                        call output_variables_update_values(series%grid%fzws(:, j), out%ts%grid%fzws(:, j), its, dnts, 'sum')
                    end if
                    if (associated(series%grid%alws)) then
                        call output_variables_update_values(series%grid%alws(:, j), out%ts%grid%alws(:, j), its, dnts, 'sum')
                    end if
                end do
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(series%grid%cmas)) then
                    call output_variables_update_values(series%grid%cmas, out%ts%grid%cmas, its, dnts, 'sum')
                end if
                if (associated(series%grid%tcan)) then
                    call output_variables_update_values(series%grid%tcan, out%ts%grid%tcan, its, dnts, 'avg')
                end if
                if (associated(series%grid%tsno)) then
                    call output_variables_update_values(series%grid%tsno, out%ts%grid%tsno, its, dnts, 'avg')
                end if
                if (associated(series%grid%tpnd)) then
                    call output_variables_update_values(series%grid%tpnd, out%ts%grid%tpnd, its, dnts, 'avg')
                end if
                if (associated(series%grid%albt)) then
                    call output_variables_update_values(series%grid%albt, out%ts%grid%albt, its, dnts, 'avg')
                end if
                if (associated(series%grid%alvs)) then
                    call output_variables_update_values(series%grid%alvs, out%ts%grid%alvs, its, dnts, 'avg')
                end if
                if (associated(series%grid%alir)) then
                    call output_variables_update_values(series%grid%alir, out%ts%grid%alir, its, dnts, 'avg')
                end if
                if (associated(series%grid%fsout)) then
                    call output_variables_update_values(series%grid%fsout, out%ts%grid%fsout, its, dnts, 'avg')
                end if
                if (associated(series%grid%gte)) then
                    call output_variables_update_values(series%grid%gte, out%ts%grid%gte, its, dnts, 'avg')
                end if
                if (associated(series%grid%flout)) then
                    call output_variables_update_values(series%grid%flout, out%ts%grid%flout, its, dnts, 'avg')
                end if
                if (associated(series%grid%qh)) then
                    call output_variables_update_values(series%grid%qh, out%ts%grid%qh, its, dnts, 'avg')
                end if
                if (associated(series%grid%qe)) then
                    call output_variables_update_values(series%grid%qe, out%ts%grid%qe, its, dnts, 'avg')
                end if
                if (associated(series%grid%gzero)) then
                    call output_variables_update_values(series%grid%gzero, out%ts%grid%gzero, its, dnts, 'avg')
                end if
                if (associated(series%grid%stge)) then
                    call output_variables_update_values(series%grid%stge, out%ts%grid%stge, its, dnts, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%grid%gflx)) then
                        call output_variables_update_values(series%grid%gflx(:, j), out%ts%grid%gflx(:, j), its, dnts, 'avg')
                    end if
                    if (associated(series%grid%tbar)) then
                        call output_variables_update_values(series%grid%tbar(:, j), out%ts%grid%tbar(:, j), its, dnts, 'avg')
                    end if
                end do
            end if

            !> Channels and routing.
            if (ro%RUNCHNL) then
                if (associated(series%grid%rff)) then
                    call output_variables_update_values(series%grid%rff, out%ts%grid%rff, its, dnts, 'sum')
                end if
                if (associated(series%grid%rchg)) then
                    call output_variables_update_values(series%grid%rchg, out%ts%grid%rchg, its, dnts, 'sum')
                end if
                if (associated(series%grid%qi)) then
                    call output_variables_update_values(series%grid%qi, out%ts%grid%qi, its, dnts, 'avg')
                end if
                if (associated(series%grid%stgch)) then
                    call output_variables_update_values(series%grid%stgch, out%ts%grid%stgch, its, dnts, 'avg')
                end if
                if (associated(series%grid%qo)) then
                    call output_variables_update_values(series%grid%qo, out%ts%grid%qo, its, dnts, 'avg')
                end if
                if (associated(series%grid%zlvl)) then
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
    !>  Set output variables to the 'NO_DATA' value.
    subroutine output_variables_reset_fields(shd, series)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Tile-based.
        if (ro%RUNTILE) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                series%tile%pre = out%NO_DATA
                series%tile%fsin = out%NO_DATA
                series%tile%flin = out%NO_DATA
                series%tile%ta = out%NO_DATA
                series%tile%qa = out%NO_DATA
                series%tile%pres = out%NO_DATA
                series%tile%uv = out%NO_DATA
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                series%tile%prec = out%NO_DATA
                series%tile%evap = out%NO_DATA
                series%tile%pevp = out%NO_DATA
                series%tile%evpb = out%NO_DATA
                series%tile%arrd = out%NO_DATA
                series%tile%gro = out%NO_DATA
                series%tile%rof = out%NO_DATA
                series%tile%rofo = out%NO_DATA
                series%tile%rofs = out%NO_DATA
                series%tile%rofb = out%NO_DATA
                series%tile%rcan = out%NO_DATA
                series%tile%sncan = out%NO_DATA
                series%tile%sno = out%NO_DATA
                series%tile%fsno = out%NO_DATA
                series%tile%wsno = out%NO_DATA
                series%tile%zpnd = out%NO_DATA
                series%tile%pndw = out%NO_DATA
                series%tile%lzs = out%NO_DATA
                series%tile%dzs = out%NO_DATA
                series%tile%stgw = out%NO_DATA
                series%tile%thlq = out%NO_DATA
                series%tile%lqws = out%NO_DATA
                series%tile%thic = out%NO_DATA
                series%tile%fzws = out%NO_DATA
                series%tile%alws = out%NO_DATA
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                series%tile%cmas = out%NO_DATA
                series%tile%tcan = out%NO_DATA
                series%tile%tsno = out%NO_DATA
                series%tile%tpnd = out%NO_DATA
                series%tile%albt = out%NO_DATA
                series%tile%alvs = out%NO_DATA
                series%tile%alir = out%NO_DATA
                series%tile%fsout = out%NO_DATA
                series%tile%gte = out%NO_DATA
                series%tile%flout = out%NO_DATA
                series%tile%qh = out%NO_DATA
                series%tile%qe = out%NO_DATA
                series%tile%gzero = out%NO_DATA
                series%tile%stge = out%NO_DATA
                series%tile%gflx = out%NO_DATA
                series%tile%tbar = out%NO_DATA
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                series%grid%pre = out%NO_DATA
                series%grid%fsin = out%NO_DATA
                series%grid%flin = out%NO_DATA
                series%grid%ta = out%NO_DATA
                series%grid%qa = out%NO_DATA
                series%grid%pres = out%NO_DATA
                series%grid%uv = out%NO_DATA
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                series%grid%prec = out%NO_DATA
                series%grid%evap = out%NO_DATA
                series%grid%pevp = out%NO_DATA
                series%grid%evpb = out%NO_DATA
                series%grid%arrd = out%NO_DATA
                series%grid%gro = out%NO_DATA
                series%grid%rof = out%NO_DATA
                series%grid%rofo = out%NO_DATA
                series%grid%rofs = out%NO_DATA
                series%grid%rofb = out%NO_DATA
                series%grid%rcan = out%NO_DATA
                series%grid%sncan = out%NO_DATA
                series%grid%sno = out%NO_DATA
                series%grid%fsno = out%NO_DATA
                series%grid%wsno = out%NO_DATA
                series%grid%zpnd = out%NO_DATA
                series%grid%pndw = out%NO_DATA
                series%grid%lzs = out%NO_DATA
                series%grid%dzs = out%NO_DATA
                series%grid%stgw = out%NO_DATA
                series%grid%thlq = out%NO_DATA
                series%grid%lqws = out%NO_DATA
                series%grid%thic = out%NO_DATA
                series%grid%fzws = out%NO_DATA
                series%grid%alws = out%NO_DATA
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                series%grid%cmas = out%NO_DATA
                series%grid%tcan = out%NO_DATA
                series%grid%tsno = out%NO_DATA
                series%grid%tpnd = out%NO_DATA
                series%grid%albt = out%NO_DATA
                series%grid%alvs = out%NO_DATA
                series%grid%alir = out%NO_DATA
                series%grid%fsout = out%NO_DATA
                series%grid%gte = out%NO_DATA
                series%grid%flout = out%NO_DATA
                series%grid%qh = out%NO_DATA
                series%grid%qe = out%NO_DATA
                series%grid%gzero = out%NO_DATA
                series%grid%stge = out%NO_DATA
                series%grid%gflx = out%NO_DATA
                series%grid%tbar = out%NO_DATA
            end if

            !> Channels and routing.
            if (ro%RUNCHNL) then
                series%grid%rff = out%NO_DATA
                series%grid%rchg = out%NO_DATA
                series%grid%qi = out%NO_DATA
                series%grid%stgch = out%NO_DATA
                series%grid%qo = out%NO_DATA
                series%grid%zlvl = out%NO_DATA
            end if
        end if

    end subroutine

    !> Description:
    !>  Reset output variables to the 'NO_DATA' value.
    subroutine output_variables_reset(shd, cm)

        !> 'shd_variables' required for 'shd'.
        !> 'climate_forcing' required for 'cm'.
        use shd_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Set variables to the 'NO_DATA' value.
        call output_variables_reset_fields(shd, out%ts)

    end subroutine

end module
