module output_variables

    use variable_names
    use model_variables

    implicit none

    !> Description:
    !>  Container for output variables.
    type output_fields
        real, dimension(:), pointer :: fsin => null()
        real, dimension(:), pointer :: ifsin => null()
        real, dimension(:), pointer :: fsvh => null()
        real, dimension(:), pointer :: fsih => null()
        real, dimension(:), pointer :: fsdr => null()
        real, dimension(:), pointer :: fsdf => null()
        real, dimension(:), pointer :: flin => null()
        real, dimension(:), pointer :: ta => null()
        real, dimension(:), pointer :: qa => null()
        real, dimension(:), pointer :: pres => null()
        real, dimension(:), pointer :: uv => null()
        real, dimension(:), pointer :: wdir => null()
        real, dimension(:), pointer :: uu => null()
        real, dimension(:), pointer :: vv => null()
        real, dimension(:), pointer :: pre => null()
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
        real, dimension(:), pointer :: zsno => null()
        real, dimension(:), pointer :: rhosno => null()
        real, dimension(:), pointer :: sno => null()
        real, dimension(:), pointer :: fsno => null()
        real, dimension(:), pointer :: isno => null()
        real, dimension(:), pointer :: wsno => null()
        real, dimension(:), pointer :: zpnd => null()
        real, dimension(:), pointer :: ipnd => null()
        real, dimension(:), pointer :: pndw => null()
        real, dimension(:), pointer :: lzs => null()
        real, dimension(:), pointer :: dzs => null()
        real, dimension(:, :), pointer :: thlq => null()
        real, dimension(:, :), pointer :: lqws => null()
        real, dimension(:, :), pointer :: thic => null()
        real, dimension(:, :), pointer :: fzws => null()
        real, dimension(:, :), pointer :: alws => null()
        real, dimension(:), pointer :: stg0w => null()
        real, dimension(:), pointer :: stgw => null()
        real, dimension(:), pointer :: dstgw => null()
        real, dimension(:), pointer :: tcan => null()
        real, dimension(:), pointer :: ican => null()
        real, dimension(:), pointer :: cmas => null()
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
        real, dimension(:), pointer :: stg0e => null()
        real, dimension(:), pointer :: stge => null()
        real, dimension(:), pointer :: dstge => null()
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
    !>  Type for process modules to integrate with output fields.
    type output_fields_surrogate
        real, dimension(:), pointer :: &
            y_tile => null(), m_tile => null(), d_tile => null(), h_tile => null(), &
            y_grid => null(), m_grid => null(), d_grid => null(), h_grid => null()
    end type

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
        if (.not. associated(field)) allocate(field(n))

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
        if (.not. associated(field)) allocate(field(n, j))

        !> Associate pointer.
        if (present(pntr) .and. present(ig)) pntr => field(:, ig)

    end subroutine

    !> Description:
    !>  Allocate the output variable to 'n' and optionally 'nsl'.
    !>  The value is set to the 'NO_DATA' value.
    subroutine output_variables_allocate_pntr(pntr, fields, vname, n, nsl, ig)

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
            case (VN_FSIN, VN_FSVH, VN_FSIH)
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
            case (VN_PRE)
                if (ro%RUNCLIM) call output_variables_allocate(fields%pre, n, pntr)

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
            case (VN_ZSNO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%zsno, n, pntr)
            case (VN_RHOSNO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%rhosno, n, pntr)
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
            case (VN_STGW)
                if (ro%RUNBALWB) then
                    call output_variables_allocate(fields%stgw, n, pntr)
                    call output_variables_allocate(fields%stg0w, n)
                    call output_variables_allocate(fields%dstgw, n)
                end if
            case (VN_DSTGW)
                if (ro%RUNBALWB) then
                    call output_variables_allocate(fields%stgw, n)
                    call output_variables_allocate(fields%stg0w, n)
                    call output_variables_allocate(fields%dstgw, n, pntr)
                end if

            !> Energy balance.
            case (VN_TCAN)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%tcan, n, pntr)
                    call output_variables_allocate(fields%ican, n)
                end if
            case (VN_CMAS)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%cmas, n, pntr)
                    call output_variables_allocate(fields%ican, n)
                end if
            case (VN_TSNO)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%tsno, n, pntr)
                    call output_variables_allocate(fields%isno, n)
                end if
            case (VN_TPND)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%tpnd, n, pntr)
                    call output_variables_allocate(fields%ipnd, n)
                end if
            case (VN_ALBT)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%albt, n, pntr)
                    call output_variables_allocate(fields%ifsin, n)
                end if
            case (VN_ALVS)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%alvs, n, pntr)
                    call output_variables_allocate(fields%ifsin, n)
                end if
            case (VN_ALIR)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%alir, n, pntr)
                    call output_variables_allocate(fields%ifsin, n)
                end if
            case (VN_FSOUT)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%fsout, n, pntr)
                    call output_variables_allocate(fields%ifsin, n)
                end if
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
            case (VN_GFLX)
                if (ro%RUNBALEB .and. present(nsl) .and. present(ig)) call output_variables_allocate(fields%gflx, n, nsl, pntr, ig)
            case (VN_TBAR)
                if (ro%RUNBALEB .and. present(nsl) .and. present(ig)) call output_variables_allocate(fields%tbar, n, nsl, pntr, ig)
            case (VN_STGE)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%stge, n, pntr)
                    call output_variables_allocate(fields%stg0e, n)
                    call output_variables_allocate(fields%dstge, n)
                end if
            case (VN_DSTGE)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%stge, n)
                    call output_variables_allocate(fields%stg0e, n)
                    call output_variables_allocate(fields%dstge, n, pntr)
                end if

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
    !>  Allocate output variables.
    subroutine output_variables_group_init(group, n, nsl)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input variables.
        integer, intent(in) :: n, nsl

        !> Input/output variables.
        type(output_fields) group

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            call output_variables_allocate(group%fsin, n)
            call output_variables_allocate(group%flin, n)
            call output_variables_allocate(group%ta, n)
            call output_variables_allocate(group%qa, n)
            call output_variables_allocate(group%pres, n)
            call output_variables_allocate(group%uv, n)
            call output_variables_allocate(group%pre, n)
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            call output_variables_allocate(group%prec, n)
            call output_variables_allocate(group%evap, n)
            call output_variables_allocate(group%pevp, n)
            call output_variables_allocate(group%evpb, n)
            call output_variables_allocate(group%arrd, n)
            call output_variables_allocate(group%gro, n)
            call output_variables_allocate(group%rof, n)
            call output_variables_allocate(group%rofo, n)
            call output_variables_allocate(group%rofs, n)
            call output_variables_allocate(group%rofb, n)
            call output_variables_allocate(group%rcan, n)
            call output_variables_allocate(group%sncan, n)
            call output_variables_allocate(group%zsno, n)
            call output_variables_allocate(group%rhosno, n)
            call output_variables_allocate(group%sno, n)
            call output_variables_allocate(group%fsno, n)
            call output_variables_allocate(group%wsno, n)
            call output_variables_allocate(group%zpnd, n)
            call output_variables_allocate(group%pndw, n)
            call output_variables_allocate(group%lzs, n)
            call output_variables_allocate(group%dzs, n)
            call output_variables_allocate(group%thlq, n, nsl)
            call output_variables_allocate(group%lqws, n, nsl)
            call output_variables_allocate(group%thic, n, nsl)
            call output_variables_allocate(group%fzws, n, nsl)
            call output_variables_allocate(group%alws, n, nsl)
            call output_variables_allocate(group%stgw, n)
            call output_variables_allocate(group%stg0w, n)
            call output_variables_allocate(group%dstgw, n)
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            call output_variables_allocate(group%ican, n)
            call output_variables_allocate(group%tcan, n)
            call output_variables_allocate(group%cmas, n)
            call output_variables_allocate(group%isno, n)
            call output_variables_allocate(group%tsno, n)
            call output_variables_allocate(group%ipnd, n)
            call output_variables_allocate(group%tpnd, n)
            call output_variables_allocate(group%ifsin, n)
            call output_variables_allocate(group%alvs, n)
            call output_variables_allocate(group%alir, n)
            call output_variables_allocate(group%albt, n)
            call output_variables_allocate(group%fsout, n)
            call output_variables_allocate(group%gte, n)
            call output_variables_allocate(group%flout, n)
            call output_variables_allocate(group%qh, n)
            call output_variables_allocate(group%qe, n)
            call output_variables_allocate(group%gzero, n)
            call output_variables_allocate(group%gflx, n, nsl)
            call output_variables_allocate(group%tbar, n, nsl)
            call output_variables_allocate(group%stge, n)
            call output_variables_allocate(group%stg0e, n)
            call output_variables_allocate(group%dstge, n)
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            call output_variables_allocate(group%rff, n)
            call output_variables_allocate(group%rchg, n)
            call output_variables_allocate(group%qi, n)
            call output_variables_allocate(group%stgch, n)
            call output_variables_allocate(group%qo, n)
            call output_variables_allocate(group%zlvl, n)
        end if

    end subroutine

    !> Description:
    !>  Set output variables to the 'NO_DATA' value.
    subroutine output_variables_group_reset(group)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input/output variables.
        type(output_fields) group

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            group%pre = out%NO_DATA
            group%fsin = out%NO_DATA
            group%flin = out%NO_DATA
            group%ta = out%NO_DATA
            group%qa = out%NO_DATA
            group%pres = out%NO_DATA
            group%uv = out%NO_DATA
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            group%prec = out%NO_DATA
            group%evap = out%NO_DATA
            group%pevp = out%NO_DATA
            group%evpb = out%NO_DATA
            group%arrd = out%NO_DATA
            group%gro = out%NO_DATA
            group%rof = out%NO_DATA
            group%rofo = out%NO_DATA
            group%rofs = out%NO_DATA
            group%rofb = out%NO_DATA
            group%rcan = out%NO_DATA
            group%sncan = out%NO_DATA
            group%zsno = out%NO_DATA
            group%rhosno = out%NO_DATA
            group%sno = out%NO_DATA
            group%fsno = out%NO_DATA
            group%wsno = out%NO_DATA
            group%zpnd = out%NO_DATA
            group%pndw = out%NO_DATA
            group%lzs = out%NO_DATA
            group%dzs = out%NO_DATA
            group%thlq = out%NO_DATA
            group%lqws = out%NO_DATA
            group%thic = out%NO_DATA
            group%fzws = out%NO_DATA
            group%alws = out%NO_DATA
            group%stg0w = group%stgw
            group%stgw = 0.0
            group%dstgw = 0.0
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            group%ican = 0.0
            group%tcan = out%NO_DATA
            group%cmas = out%NO_DATA
            group%isno = 0.0
            group%tsno = out%NO_DATA
            group%ipnd = 0.0
            group%tpnd = out%NO_DATA
            group%ifsin = 0.0
            group%albt = out%NO_DATA
            group%alvs = out%NO_DATA
            group%alir = out%NO_DATA
            group%fsout = out%NO_DATA
            group%gte = out%NO_DATA
            group%flout = out%NO_DATA
            group%qh = out%NO_DATA
            group%qe = out%NO_DATA
            group%gzero = out%NO_DATA
            group%gflx = out%NO_DATA
            group%tbar = out%NO_DATA
            group%stg0e = group%stge
            group%stge = 0.0
            group%dstge = 0.0
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            group%rff = out%NO_DATA
            group%rchg = out%NO_DATA
            group%qi = out%NO_DATA
            group%stgch = out%NO_DATA
            group%qo = out%NO_DATA
            group%zlvl = out%NO_DATA
        end if

    end subroutine

    !> Description:
    !>  Reset output variables to the 'NO_DATA' value.
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

        !> Set variables to the 'NO_DATA' value.
        if (ro%RUNTILE) then
            call output_variables_group_reset(out%ts%tile)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_reset(out%ts%grid)
            call output_variables_group_reset(out%ts%basin)
        end if

    end subroutine

    !> Description:
    !>  Allocate and initialize output variables.
    subroutine output_variables_series_init(shd, cm, series)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'climate_forcing' required for 'cm'.
        use shd_variables
        use control_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Input/output variables.
        type(output_series) series

        !> Allocate and initialize the variables.
        if (ro%RUNTILE) then
            call output_variables_group_init(series%tile, shd%lc%NML, shd%lc%IGND)
            call output_variables_group_reset(series%tile)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_init(series%grid, shd%NA, shd%lc%IGND)
            call output_variables_group_reset(series%grid)
            call output_variables_group_init(series%basin, shd%NA, shd%lc%IGND)
            call output_variables_group_reset(series%basin)
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
        call output_variables_series_init(shd, cm, out%ts)

    end subroutine

    !> Description:
    !>  Update output variables from current model states and variables.
    !>  Variables are updated if all elements of the group are equal to
    !>  the 'NO_DATA' value; if not the case, updates are assumed to
    !>  have occured in the model (e.g., by process modules), and those
    !>  values are preserved.
    subroutine output_variables_group_update_ts(shd, cm, group, group_vs)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'climate_forcing' required for 'cm'.
        use shd_variables
        use control_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm
        type(model_variables_fields), intent(in) :: group_vs

        !> Input/output variables.
        type(output_fields) group

        !> Local variables.
        logical lcheck

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            if (all(group%fsin == out%NO_DATA)) group%fsin = group_vs%fsin
            if (all(group%flin == out%NO_DATA)) group%flin = group_vs%flin
            if (all(group%ta == out%NO_DATA)) group%ta = group_vs%ta
            if (all(group%qa == out%NO_DATA)) group%qa = group_vs%qa
            if (all(group%pres == out%NO_DATA)) group%pres = group_vs%pres
            if (all(group%uv == out%NO_DATA)) group%uv = group_vs%uv
            if (all(group%pre == out%NO_DATA)) group%pre = group_vs%pre
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            if (all(group%prec == out%NO_DATA)) group%prec = group_vs%pre*ic%dts
            if (all(group%evap == out%NO_DATA)) group%evap = group_vs%evap
            if (all(group%pevp == out%NO_DATA)) group%pevp = group_vs%pevp
            if (all(group%evpb == out%NO_DATA)) group%evpb = group_vs%evpb
            if (all(group%arrd == out%NO_DATA)) group%arrd = group_vs%arrd
            if (all(group%gro == out%NO_DATA)) group%gro = group_vs%gro
            if (all(group%rof == out%NO_DATA)) then
                group%rof = 0.0
                lcheck = .true.
            else
                lcheck = .false.
            end if
            if (all(group%rofo == out%NO_DATA)) group%rofo = group_vs%rofo
            if (lcheck) then
                where (group%rofo /= out%NO_DATA) group%rof = group%rof + group%rofo
            end if
            if (all(group%rofs == out%NO_DATA)) group%rofs = group_vs%rofs
            if (lcheck) then
                where (group%rofs /= out%NO_DATA) group%rof = group%rof + group%rofs
            end if
            if (all(group%rofb == out%NO_DATA)) group%rofb = group_vs%rofb
            if (lcheck) then
                where (group%rofb /= out%NO_DATA) group%rof = group%rof + group%rofb
            end if
            if (all(group%rcan == out%NO_DATA)) group%rcan = group_vs%rcan
            where (group%rcan /= out%NO_DATA) group%stgw = group%stgw + group%rcan
            if (all(group%sncan == out%NO_DATA)) group%sncan = group_vs%sncan
            where (group%sncan /= out%NO_DATA) group%stgw = group%stgw + group%sncan
            if (all(group%zsno == out%NO_DATA)) group%zsno = group_vs%zsno
            if (all(group%rhosno == out%NO_DATA)) group%rhosno = group_vs%rhos
            if (all(group%sno == out%NO_DATA)) group%sno = group_vs%sno
            if (all(group%wsno == out%NO_DATA)) group%wsno = group_vs%wsno
            where (group%stgw /= out%NO_DATA) group%stgw = group%stgw + group%wsno
            if (all(group%fsno == out%NO_DATA)) group%fsno = group_vs%fsno
            if (all(group%zpnd == out%NO_DATA)) group%zpnd = group_vs%zpnd
            if (all(group%pndw == out%NO_DATA)) group%pndw = group_vs%pndw
            where (group%ipnd > 0.0) group%stgw = group%stgw + group%pndw
            if (all(group%lzs == out%NO_DATA)) group%lzs = group_vs%lzs
            where (group%lzs /= out%NO_DATA) group%stgw = group%stgw + group%lzs
            if (all(group%dzs == out%NO_DATA)) group%dzs = group_vs%dzs
            where (group%dzs /= out%NO_DATA) group%stgw = group%stgw + group%dzs
            if (all(group%thlq == out%NO_DATA)) group%thlq = group_vs%thlq
            if (all(group%thic == out%NO_DATA)) group%thic = group_vs%thic
            if (all(group%alws == out%NO_DATA)) then
                group%alws = 0.0
                lcheck = .true.
            else
                lcheck = .false.
            end if
            if (all(group%lqws == out%NO_DATA)) group%lqws = group_vs%lqws
            where (sum(group%lqws, 2) /= out%NO_DATA) group%stgw = group%stgw + sum(group%lqws, 2)
            if (lcheck) then
                where (group%lqws /= out%NO_DATA) group%alws = group%alws + group%lqws
            end if
            if (all(group%fzws == out%NO_DATA)) group%fzws = group_vs%fzws
            where (sum(group%fzws, 2) /= out%NO_DATA) group%stgw = group%stgw + sum(group%fzws, 2)
            if (lcheck) then
                where (group%fzws /= out%NO_DATA) group%alws = group%alws + group%fzws
            end if
            if (all(group%stgw == 0.0)) then
                group%stg0w = out%NO_DATA
                group%stgw = out%NO_DATA
                group%dstgw = out%NO_DATA
            else
                group%dstgw = group%stgw - group%stg0w
            end if
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            if (all(group%tcan == out%NO_DATA)) group%tcan = group_vs%tcan
            where (group%tcan > 0.0)
                group%ican = 1.0
            elsewhere
                group%ican = 0.0
            end where
            if (all(group%cmas == out%NO_DATA)) group%cmas = group_vs%cmas
            where (group%ican == 0.0) group%cmas = 0.0
            if (all(group%tsno == out%NO_DATA)) group%tsno = group_vs%tsno
            where (group%tsno > 0.0)
                group%isno = 1.0
            elsewhere
                group%isno = 0.0
            end where
            if (all(group%tpnd == out%NO_DATA)) group%tpnd = group_vs%tpnd
            where (group%tpnd > 0.0)
                group%ipnd = 1.0
            elsewhere
                group%ipnd = 0.0
            end where
            where (group%fsin > 0.0)
                group%ifsin = 1.0
            elsewhere
                group%ifsin = 0.0
            end where
            if (all(group%alvs == out%NO_DATA)) group%alvs = group_vs%alvs
            if (all(group%alir == out%NO_DATA)) group%alir = group_vs%alir
            if (all(group%albt == out%NO_DATA)) group%albt = group_vs%albt
            if (all(group%fsout == out%NO_DATA)) group%fsout = group_vs%fsin*(1.0 - group_vs%albt)
            if (all(group%gte == out%NO_DATA)) group%gte = group_vs%gte
            if (all(group%flout == out%NO_DATA)) group%flout = 5.66796E-8*group_vs%gte**4
            if (all(group%qh == out%NO_DATA)) group%qh = group_vs%hfs
            if (all(group%qe == out%NO_DATA)) group%qe = group_vs%qevp
            if (all(group%gzero == out%NO_DATA)) group%gzero = group_vs%gzero
            if (all(group%gflx == out%NO_DATA)) group%gflx = group_vs%gflx
            if (all(group%tbar == out%NO_DATA)) group%tbar = group_vs%tbar
            if (all(group%stge == 0.0)) then
                group%stg0e = out%NO_DATA
                group%stge = out%NO_DATA
                group%dstge = out%NO_DATA
            else
                group%dstge = group%stge - group%stg0e
            end if
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            if (all(group%rff == out%NO_DATA)) group%rff = group_vs%rff
            if (all(group%rchg == out%NO_DATA)) group%rchg = group_vs%rchg
            if (all(group%qi == out%NO_DATA)) group%qi = group_vs%qi
            if (all(group%stgch == out%NO_DATA)) group%stgch = group_vs%stgch
            if (all(group%qo == out%NO_DATA)) group%qo = group_vs%qo
            if (all(group%zlvl == out%NO_DATA)) group%zlvl = group_vs%zlvl
        end if

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
        use shd_variables
        use control_variables
        use climate_forcing

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Update the variables.
        if (ro%RUNTILE) then
            call output_variables_group_update_ts(shd, cm, out%ts%tile, vs%tile)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_update_ts(shd, cm, out%ts%grid, vs%grid)
            call output_variables_group_update_ts(shd, cm, out%ts%basin, vs%basin)
        end if

    end subroutine

    !> Description:
    !>  Update the 'dat' vector using the 'val' vector.
    !>  Reset 'dat' if the time-step of the current interval "its" is 1.
    !>  Calculate an average if the function "fn" is 'avg' using the
    !>  number of time-steps elapsed "its".
    !>  Override calculations where 'val' was originally equal to the
    !>  'NO_DATA' value.
    subroutine output_variables_field_update(dat, val, its, fn)

        !> Input variables.
        integer, intent(in) :: its
        real, dimension(:), intent(in) :: val
        character(len = *), intent(in) :: fn

        !> Input/output variables.
        real, dimension(:) :: dat

        !> Reset the variable if this is the first time-step in the series.
        if (its == 1) then
            where (val /= out%NO_DATA) dat = 0.0
        end if

        !> Apply the 'fn' function.
        !> The default case is to set 'dat' to 'val'.
        select case (fn)
            case ('sum')
                where (dat /= out%NO_DATA .and. val /= out%NO_DATA) dat = dat + val
            case ('avg')
                where (dat /= out%NO_DATA .and. val /= out%NO_DATA) dat = (dat*(its - 1) + val)/its
            case ('max')
                where (dat /= out%NO_DATA .and. val /= out%NO_DATA) dat = max(dat, val)
            case ('min')
                where (dat /= out%NO_DATA .and. val /= out%NO_DATA) dat = min(dat, val)
            case default
                dat = val
        end select

    end subroutine

    !> Description:
    !>  Calculate an average of 'dat' from 'val' for fields that do not
    !>  necessarily update at every time-step. 'ival' communicates if
    !>  the value should be updated. 'idat' is the counter of the number
    !>  of times the variable has been updated for the moving average.
    subroutine output_variables_field_icount_average(dat, val, idat, ival)

        !> Input variables.
        real, dimension(:), intent(in) :: val, ival

        !> Input/output variables.
        real, dimension(:) :: dat, idat

        !> Apply the function.
        where (val /= out%NO_DATA)
            where (idat == 0.0) dat = 0.0
            where (ival /= 0.0) dat = (dat*(idat - 1.0) + val)/idat
        end where

    end subroutine

    !> Description:
    !>  Apply a transform to 'dat' using 'cfactorm' and 'cfactora'.
    !>  Override calculations where 'val' was originally equal to the
    !>  'NO_DATA' value.
    subroutine output_variables_field_transform(dat, cfactorm, cfactora)

        !> Input variables.
        real, dimension(:), intent(in), optional :: cfactorm, cfactora

        !> Input/output variables.
        real, dimension(:) :: dat

        !> Apply transforms to the variable.
        if (present(cfactorm)) then
            where (dat /= out%NO_DATA) dat = dat*cfactorm
        end if
        if (present(cfactora)) then
            where (dat /= out%NO_DATA) dat = dat + cfactora
        end if

    end subroutine

    !> Description:
    !>  Update output variables of larger time intervals from the 'ts'
    !>  values.
    subroutine output_variables_group_update(shd, group, group_ts, its)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        integer, intent(in) :: its
        type(ShedGridParams), intent(in) :: shd
        type(output_fields), intent(in) :: group_ts

        !> Input/output variables.
        type(output_fields) group

        !> Local variables.
        integer j

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            if (associated(group%fsin)) then
                call output_variables_field_update(group%fsin, group_ts%fsin, its, 'avg')
            end if
            if (associated(group%flin)) then
                call output_variables_field_update(group%flin, group_ts%flin, its, 'avg')
            end if
            if (associated(group%ta)) then
                call output_variables_field_update(group%ta, group_ts%ta, its, 'avg')
            end if
            if (associated(group%qa)) then
                call output_variables_field_update(group%qa, group_ts%qa, its, 'avg')
            end if
            if (associated(group%pres)) then
                call output_variables_field_update(group%pres, group_ts%pres, its, 'avg')
            end if
            if (associated(group%uv)) then
                call output_variables_field_update(group%uv, group_ts%uv, its, 'avg')
            end if
            if (associated(group%pre)) then
                call output_variables_field_update(group%pre, group_ts%pre, its, 'avg')
            end if
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            if (associated(group%prec)) then
                call output_variables_field_update(group%prec, group_ts%prec, its, 'sum')
            end if
            if (associated(group%evap)) then
                call output_variables_field_update(group%evap, group_ts%evap, its, 'sum')
            end if
            if (associated(group%pevp)) then
                call output_variables_field_update(group%pevp, group_ts%pevp, its, 'sum')
            end if
            if (associated(group%evpb)) then
                call output_variables_field_update(group%evpb, group_ts%evpb, its, 'avg')
            end if
            if (associated(group%arrd)) then
                call output_variables_field_update(group%arrd, group_ts%arrd, its, 'avg')
            end if
            if (associated(group%gro)) then
                call output_variables_field_update(group%gro, group_ts%gro, its, 'avg')
            end if
            if (associated(group%rof)) then
                call output_variables_field_update(group%rof, group_ts%rof, its, 'sum')
            end if
            if (associated(group%rofo)) then
                call output_variables_field_update(group%rofo, group_ts%rofo, its, 'sum')
            end if
            if (associated(group%rofs)) then
                call output_variables_field_update(group%rofs, group_ts%rofs, its, 'sum')
            end if
            if (associated(group%rofb)) then
                call output_variables_field_update(group%rofb, group_ts%rofb, its, 'sum')
            end if
            if (associated(group%rcan)) then
                call output_variables_field_update(group%rcan, group_ts%rcan, its, 'avg')
            end if
            if (associated(group%sncan)) then
                call output_variables_field_update(group%sncan, group_ts%sncan, its, 'avg')
            end if
            if (associated(group%zsno)) then
                call output_variables_field_update(group%zsno, group_ts%zsno, its, 'avg')
            end if
            if (associated(group%rhosno)) then
                call output_variables_field_update(group%rhosno, group_ts%rhosno, its, 'avg')
            end if
            if (associated(group%sno)) then
                call output_variables_field_update(group%sno, group_ts%sno, its, 'avg')
            end if
            if (associated(group%fsno)) then
                call output_variables_field_update(group%fsno, group_ts%fsno, its, 'avg')
            end if
            if (associated(group%wsno)) then
                call output_variables_field_update(group%wsno, group_ts%wsno, its, 'avg')
            end if
            if (associated(group%zpnd)) then
                call output_variables_field_update(group%zpnd, group_ts%zpnd, its, 'avg')
            end if
            if (associated(group%pndw)) then
                call output_variables_field_update(group%pndw, group_ts%pndw, its, 'avg')
            end if
            if (associated(group%lzs)) then
                call output_variables_field_update(group%lzs, group_ts%lzs, its, 'avg')
            end if
            if (associated(group%dzs)) then
                call output_variables_field_update(group%dzs, group_ts%dzs, its, 'avg')
            end if
            do j = 1, shd%lc%IGND
                if (associated(group%thlq)) then
                    call output_variables_field_update(group%thlq(:, j), group_ts%thlq(:, j), its, 'avg')
                end if
                if (associated(group%lqws)) then
                    call output_variables_field_update(group%lqws(:, j), group_ts%lqws(:, j), its, 'avg')
                end if
                if (associated(group%thic)) then
                    call output_variables_field_update(group%thic(:, j), group_ts%thic(:, j), its, 'avg')
                end if
                if (associated(group%fzws)) then
                    call output_variables_field_update(group%fzws(:, j), group_ts%fzws(:, j), its, 'avg')
                end if
                if (associated(group%alws)) then
                    call output_variables_field_update(group%alws(:, j), group_ts%alws(:, j), its, 'avg')
                end if
            end do
            if (associated(group%stgw)) then
                if (associated(group%stg0w) .and. its == 1) then
                    call output_variables_field_update(group%stg0w, group%stgw, its, 'val')
                end if
                call output_variables_field_update(group%stgw, group_ts%stgw, its, 'avg')
                if (associated(group%dstgw) .and. associated(group%stg0w)) then
                    where (group%stgw /= out%NO_DATA) group%dstgw = group%stgw - group%stg0w
                end if
            end if
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            if (associated(group%ican)) then
                call output_variables_field_update(group%ican, group_ts%ican, its, 'sum')
            end if
            if (associated(group%tcan)) then
                call output_variables_field_icount_average(group%tcan, group_ts%tcan, group%ican, group_ts%ican)
            end if
            if (associated(group%cmas)) then
                call output_variables_field_icount_average(group%cmas, group_ts%cmas, group%ican, group_ts%ican)
            end if
            if (associated(group%isno)) then
                call output_variables_field_update(group%isno, group_ts%isno, its, 'sum')
            end if
            if (associated(group%tsno)) then
                call output_variables_field_icount_average(group%tsno, group_ts%tsno, group%isno, group_ts%isno)
            end if
            if (associated(group%ipnd)) then
                call output_variables_field_update(group%ipnd, group_ts%ipnd, its, 'sum')
            end if
            if (associated(group%tpnd)) then
                call output_variables_field_icount_average(group%tpnd, group_ts%tpnd, group%ipnd, group_ts%ipnd)
            end if
            if (associated(group%ifsin)) then
                call output_variables_field_update(group%ifsin, group_ts%ifsin, its, 'sum')
            end if
            if (associated(group%albt)) then
                call output_variables_field_icount_average(group%albt, group_ts%albt, group%ifsin, group_ts%ifsin)
            end if
            if (associated(group%alvs)) then
                call output_variables_field_icount_average(group%alvs, group_ts%alvs, group%ifsin, group_ts%ifsin)
            end if
            if (associated(group%alir)) then
                call output_variables_field_icount_average(group%alir, group_ts%alir, group%ifsin, group_ts%ifsin)
            end if
            if (associated(group%fsout)) then
                call output_variables_field_update(group%fsout, group_ts%fsout, its, 'avg')
            end if
            if (associated(group%gte)) then
                call output_variables_field_update(group%gte, group_ts%gte, its, 'avg')
            end if
            if (associated(group%flout)) then
                call output_variables_field_update(group%flout, group_ts%flout, its, 'avg')
            end if
            if (associated(group%qh)) then
                call output_variables_field_update(group%qh, group_ts%qh, its, 'avg')
            end if
            if (associated(group%qe)) then
                call output_variables_field_update(group%qe, group_ts%qe, its, 'avg')
            end if
            if (associated(group%gzero)) then
                call output_variables_field_update(group%gzero, group_ts%gzero, its, 'avg')
            end if
            do j = 1, shd%lc%IGND
                if (associated(group%gflx)) then
                    call output_variables_field_update(group%gflx(:, j), group_ts%gflx(:, j), its, 'avg')
                end if
                if (associated(group%tbar)) then
                    call output_variables_field_update(group%tbar(:, j), group_ts%tbar(:, j), its, 'avg')
                end if
            end do
            if (associated(group%stge)) then
                if (associated(group%stg0e) .and. its == 1) then
                    call output_variables_field_update(group%stg0e, group%stge, its, 'val')
                end if
                call output_variables_field_update(group%stge, group_ts%stge, its, 'avg')
                if (associated(group%dstge) .and. associated(group%stg0e)) then
                    where (group%stge /= out%NO_DATA) group%dstge = group%stge - group%stg0e
                end if
            end if
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            if (associated(group%rff)) then
                call output_variables_field_update(group%rff, group_ts%rff, its, 'sum')
            end if
            if (associated(group%rchg)) then
                call output_variables_field_update(group%rchg, group_ts%rchg, its, 'sum')
            end if
            if (associated(group%qi)) then
                call output_variables_field_update(group%qi, group_ts%qi, its, 'avg')
            end if
            if (associated(group%stgch)) then
                call output_variables_field_update(group%stgch, group_ts%stgch, its, 'avg')
            end if
            if (associated(group%qo)) then
                call output_variables_field_update(group%qo, group_ts%qo, its, 'avg')
            end if
            if (associated(group%zlvl)) then
                call output_variables_field_update(group%zlvl, group_ts%zlvl, its, 'avg')
            end if
        end if

    end subroutine

    !> Description:
    !>  Update output variables of larger time intervals from the 'ts'
    !>  values.
    subroutine output_variables_series_update(shd, series, its)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        integer, intent(in) :: its
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Update groups.
        if (ro%RUNTILE) then
            call output_variables_group_update(shd, series%tile, out%ts%tile, its)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_update(shd, series%grid, out%ts%grid, its)
            call output_variables_group_update(shd, series%basin, out%ts%basin, its)
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

        !> Update 'ts' values.
        call output_variables_update_ts(shd, cm)

        !> Totals (e.g., accumulated).
        call output_variables_series_update(shd, out%tot, ic%ts_count)

        !> Yearly.
        call output_variables_series_update(shd, out%y, ic%ts_yearly)

        !> Monthly.
        call output_variables_series_update(shd, out%m, ic%ts_monthly)

        !> Daily.
        call output_variables_series_update(shd, out%d, ic%ts_daily)

        !> Hourly.
        call output_variables_series_update(shd, out%h, ic%ts_hourly)

    end subroutine

end module
