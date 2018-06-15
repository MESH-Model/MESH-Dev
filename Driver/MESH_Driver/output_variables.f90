module output_variables

    use variable_names

    implicit none

    !> Description:
    !>  Container for output variables.
    type output_fields
        real, dimension(:), pointer :: pre => null()
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
        real, dimension(:), pointer :: cmas => null()
        real, dimension(:), pointer :: tcan => null()
        real, dimension(:), pointer :: ican => null()
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
            case (VN_FSIN, VN_FSVH, VN_FSIH)
                if (ro%RUNCLIM) then
                    call output_variables_allocate(fields%fsin, n, pntr)
                    call output_variables_allocate(fields%ifsin, n)
                end if
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
            case (VN_SNO)
                if (ro%RUNBALWB) then
                    call output_variables_allocate(fields%sno, n, pntr)
                    call output_variables_allocate(fields%isno, n)
                end if
            case (VN_FSNO)
                if (ro%RUNBALWB) call output_variables_allocate(fields%fsno, n, pntr)
            case (VN_WSNO)
                if (ro%RUNBALWB) then
                    call output_variables_allocate(fields%wsno, n, pntr)
                    call output_variables_allocate(fields%isno, n)
                end if
            case (VN_ZPND)
                if (ro%RUNBALWB) then
                    call output_variables_allocate(fields%zpnd, n, pntr)
                    call output_variables_allocate(fields%ipnd, n)
                end if
            case (VN_PNDW)
                if (ro%RUNBALWB) then
                    call output_variables_allocate(fields%pndw, n, pntr)
                    call output_variables_allocate(fields%ipnd, n)
                end if
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
            case (VN_CMAS)
                if (ro%RUNBALEB) call output_variables_allocate(fields%cmas, n, pntr)
            case (VN_TCAN)
                if (ro%RUNBALEB) then
                    call output_variables_allocate(fields%tcan, n, pntr)
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
    !>  Allocate output variables if the model states and variables,
    !>  used in the update routine, are allocated.
    subroutine output_variables_init_fields(shd, cm, series)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'climate_forcing' required for 'cm'.
        !> 'model_variables' required for 'vs'.
        use shd_variables
        use control_variables
        use climate_forcing
        use model_variables

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
                if (associated(vs%tile%fsin)) then
                    call output_variables_allocate(series%tile%fsin, n)
                    call output_variables_allocate(series%tile%ifsin, n)
                end if
                if (associated(vs%tile%flin)) call output_variables_allocate(series%tile%flin, n)
                if (associated(vs%tile%ta)) call output_variables_allocate(series%tile%ta, n)
                if (associated(vs%tile%qa)) call output_variables_allocate(series%tile%qa, n)
                if (associated(vs%tile%pres)) call output_variables_allocate(series%tile%pres, n)
                if (associated(vs%tile%uv)) call output_variables_allocate(series%tile%uv, n)
                if (associated(vs%tile%pre)) call output_variables_allocate(series%tile%pre, n)
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(vs%tile%pre)) call output_variables_allocate(series%tile%prec, n)
                if (associated(vs%tile%evap)) call output_variables_allocate(series%tile%evap, n)
                if (associated(vs%tile%pevp)) call output_variables_allocate(series%tile%pevp, n)
                if (associated(vs%tile%evpb)) call output_variables_allocate(series%tile%evpb, n)
                if (associated(vs%tile%arrd)) call output_variables_allocate(series%tile%arrd, n)
                if (associated(vs%tile%gro)) call output_variables_allocate(series%tile%gro, n)
                if (associated(vs%tile%rofo) .or. associated(vs%tile%rofs) .or. associated(vs%tile%rofb)) then
                    call output_variables_allocate(series%tile%rof, n)
                end if
                if (associated(vs%tile%rofo)) call output_variables_allocate(series%tile%rofo, n)
                if (associated(vs%tile%rofs)) call output_variables_allocate(series%tile%rofs, n)
                if (associated(vs%tile%rofb)) call output_variables_allocate(series%tile%rofb, n)
                if (associated(vs%tile%rcan)) call output_variables_allocate(series%tile%rcan, n)
                if (associated(vs%tile%sncan)) call output_variables_allocate(series%tile%sncan, n)
                if (associated(vs%tile%sno)) then
                    call output_variables_allocate(series%tile%sno, n)
                    call output_variables_allocate(series%tile%isno, n)
                end if
                if (associated(vs%tile%fsno)) call output_variables_allocate(series%tile%fsno, n)
                if (associated(vs%tile%wsno)) then
                    call output_variables_allocate(series%tile%wsno, n)
                    call output_variables_allocate(series%tile%isno, n)
                end if
                if (associated(vs%tile%zpnd)) then
                    call output_variables_allocate(series%tile%zpnd, n)
                    call output_variables_allocate(series%tile%ipnd, n)
                end if
                if (associated(vs%tile%pndw)) then
                    call output_variables_allocate(series%tile%pndw, n)
                    call output_variables_allocate(series%tile%ipnd, n)
                end if
                if (associated(vs%tile%lzs)) call output_variables_allocate(series%tile%lzs, n)
                if (associated(vs%tile%dzs)) call output_variables_allocate(series%tile%dzs, n)
                if (associated(vs%tile%thlq)) call output_variables_allocate(series%tile%thlq, n, nsl)
                if (associated(vs%tile%lqws)) call output_variables_allocate(series%tile%lqws, n, nsl)
                if (associated(vs%tile%thic)) call output_variables_allocate(series%tile%thic, n, nsl)
                if (associated(vs%tile%fzws)) call output_variables_allocate(series%tile%fzws, n, nsl)
                if (associated(vs%tile%lqws) .or. associated(vs%tile%fzws)) then
                    call output_variables_allocate(series%tile%alws, n, nsl)
                end if
                call output_variables_allocate(series%tile%stgw, n)
                call output_variables_allocate(series%tile%stg0w, n)
                call output_variables_allocate(series%tile%dstgw, n)
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(vs%tile%cmas)) call output_variables_allocate(series%tile%cmas, n)
                if (associated(vs%tile%tcan)) then
                    call output_variables_allocate(series%tile%tcan, n)
                    call output_variables_allocate(series%tile%ican, n)
                end if
                if (associated(vs%tile%tsno)) then
                    call output_variables_allocate(series%tile%tsno, n)
                    call output_variables_allocate(series%tile%isno, n)
                end if
                if (associated(vs%tile%tpnd)) then
                    call output_variables_allocate(series%tile%tpnd, n)
                    call output_variables_allocate(series%tile%ipnd, n)
                end if
                if (associated(vs%tile%alvs)) then
                    call output_variables_allocate(series%tile%alvs, n)
                end if
                if (associated(vs%tile%alir)) then
                    call output_variables_allocate(series%tile%alir, n)
                    call output_variables_allocate(series%tile%ifsin, n)
                end if
                if (associated(vs%tile%albt)) then
                    call output_variables_allocate(series%tile%albt, n)
                    call output_variables_allocate(series%tile%ifsin, n)
                end if
                if (associated(vs%tile%gte)) call output_variables_allocate(series%tile%gte, n)
                if (associated(vs%tile%fsin) .and. associated(vs%tile%albt)) then
                    call output_variables_allocate(series%tile%fsout, n)
                    call output_variables_allocate(series%tile%ifsin, n)
                end if
                if (associated(vs%tile%gte)) call output_variables_allocate(series%tile%flout, n)
                if (associated(vs%tile%hfs)) call output_variables_allocate(series%tile%qh, n)
                if (associated(vs%tile%qevp)) call output_variables_allocate(series%tile%qe, n)
                if (associated(vs%tile%gzero)) call output_variables_allocate(series%tile%gzero, n)
                if (associated(vs%tile%gflx)) call output_variables_allocate(series%tile%gflx, n, nsl)
                if (associated(vs%tile%tbar)) call output_variables_allocate(series%tile%tbar, n, nsl)
                call output_variables_allocate(series%tile%stge, n)
                call output_variables_allocate(series%tile%stg0e, n)
                call output_variables_allocate(series%tile%dstge, n)
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then
            n = shd%NA

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (associated(vs%grid%fsin)) then
                    call output_variables_allocate(series%grid%fsin, n)
                    call output_variables_allocate(series%grid%ifsin, n)
                end if
                if (associated(vs%grid%flin)) call output_variables_allocate(series%grid%flin, n)
                if (associated(vs%grid%ta)) call output_variables_allocate(series%grid%ta, n)
                if (associated(vs%grid%qa)) call output_variables_allocate(series%grid%qa, n)
                if (associated(vs%grid%pres)) call output_variables_allocate(series%grid%pres, n)
                if (associated(vs%grid%uv)) call output_variables_allocate(series%grid%uv, n)
                if (associated(vs%grid%pre)) call output_variables_allocate(series%grid%pre, n)
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(vs%grid%pre)) call output_variables_allocate(series%grid%prec, n)
                if (associated(vs%grid%evap)) call output_variables_allocate(series%grid%evap, n)
                if (associated(vs%grid%pevp)) call output_variables_allocate(series%grid%pevp, n)
                if (associated(vs%grid%evpb)) call output_variables_allocate(series%grid%evpb, n)
                if (associated(vs%grid%arrd)) call output_variables_allocate(series%grid%arrd, n)
                if (associated(vs%grid%gro)) call output_variables_allocate(series%grid%gro, n)
                if (associated(vs%grid%rofo) .or. associated(vs%grid%rofs) .or. associated(vs%grid%rofb)) then
                    call output_variables_allocate(series%grid%rof, n)
                end if
                if (associated(vs%grid%rofo)) call output_variables_allocate(series%grid%rofo, n)
                if (associated(vs%grid%rofs)) call output_variables_allocate(series%grid%rofs, n)
                if (associated(vs%grid%rofb)) call output_variables_allocate(series%grid%rofb, n)
                if (associated(vs%grid%rcan)) call output_variables_allocate(series%grid%rcan, n)
                if (associated(vs%grid%sncan)) call output_variables_allocate(series%grid%sncan, n)
                if (associated(vs%grid%sno)) then
                    call output_variables_allocate(series%grid%sno, n)
                    call output_variables_allocate(series%grid%isno, n)
                end if
                if (associated(vs%grid%fsno)) call output_variables_allocate(series%grid%fsno, n)
                if (associated(vs%grid%wsno)) then
                    call output_variables_allocate(series%grid%wsno, n)
                    call output_variables_allocate(series%grid%isno, n)
                end if
                if (associated(vs%grid%zpnd)) then
                    call output_variables_allocate(series%grid%zpnd, n)
                    call output_variables_allocate(series%grid%ipnd, n)
                end if
                if (associated(vs%grid%pndw)) then
                    call output_variables_allocate(series%grid%pndw, n)
                    call output_variables_allocate(series%grid%ipnd, n)
                end if
                if (associated(vs%grid%lzs)) call output_variables_allocate(series%grid%lzs, n)
                if (associated(vs%grid%dzs)) call output_variables_allocate(series%grid%dzs, n)
                if (associated(vs%grid%thlq)) call output_variables_allocate(series%grid%thlq, n, nsl)
                if (associated(vs%grid%lqws)) call output_variables_allocate(series%grid%lqws, n, nsl)
                if (associated(vs%grid%thic)) call output_variables_allocate(series%grid%thic, n, nsl)
                if (associated(vs%grid%fzws)) call output_variables_allocate(series%grid%fzws, n, nsl)
                if (associated(vs%grid%lqws) .or. associated(vs%grid%fzws)) then
                    call output_variables_allocate(series%grid%alws, n, nsl)
                end if
                call output_variables_allocate(series%grid%stgw, n)
                call output_variables_allocate(series%grid%stg0w, n)
                call output_variables_allocate(series%grid%dstgw, n)
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(vs%grid%cmas)) call output_variables_allocate(series%grid%cmas, n)
                if (associated(vs%grid%tcan)) then
                    call output_variables_allocate(series%grid%tcan, n)
                    call output_variables_allocate(series%grid%ican, n)
                end if
                if (associated(vs%grid%tsno)) then
                    call output_variables_allocate(series%grid%tsno, n)
                    call output_variables_allocate(series%grid%isno, n)
                end if
                if (associated(vs%grid%tpnd)) then
                    call output_variables_allocate(series%grid%tpnd, n)
                    call output_variables_allocate(series%grid%ipnd, n)
                end if
                if (associated(vs%grid%alvs)) then
                    call output_variables_allocate(series%grid%alvs, n)
                end if
                if (associated(vs%grid%alir)) then
                    call output_variables_allocate(series%grid%alir, n)
                    call output_variables_allocate(series%grid%ifsin, n)
                end if
                if (associated(vs%grid%albt)) then
                    call output_variables_allocate(series%grid%albt, n)
                    call output_variables_allocate(series%grid%ifsin, n)
                end if
                if (associated(vs%grid%gte)) call output_variables_allocate(series%grid%gte, n)
                if (associated(vs%grid%fsin) .and. associated(vs%grid%albt)) then
                    call output_variables_allocate(series%grid%fsout, n)
                    call output_variables_allocate(series%grid%ifsin, n)
                end if
                if (associated(vs%grid%gte)) call output_variables_allocate(series%grid%flout, n)
                if (associated(vs%grid%hfs)) call output_variables_allocate(series%grid%qh, n)
                if (associated(vs%grid%qevp)) call output_variables_allocate(series%grid%qe, n)
                if (associated(vs%grid%gzero)) call output_variables_allocate(series%grid%gzero, n)
                if (associated(vs%grid%gflx)) call output_variables_allocate(series%grid%gflx, n, nsl)
                if (associated(vs%grid%tbar)) call output_variables_allocate(series%grid%tbar, n, nsl)
                call output_variables_allocate(series%grid%stge, n)
                call output_variables_allocate(series%grid%stg0e, n)
                call output_variables_allocate(series%grid%dstge, n)
            end if

            !> Channels and routing.
            if (ro%RUNCHNL) then
                if (associated(vs%grid%rff)) call output_variables_allocate(series%grid%rff, n)
                if (associated(vs%grid%rchg)) call output_variables_allocate(series%grid%rchg, n)
                if (associated(vs%grid%qi)) call output_variables_allocate(series%grid%qi, n)
                if (associated(vs%grid%stgch)) call output_variables_allocate(series%grid%stgch, n)
                if (associated(vs%grid%qo)) call output_variables_allocate(series%grid%qo, n)
                if (associated(vs%grid%zlvl)) call output_variables_allocate(series%grid%zlvl, n)
            end if

            !> DA averaged.
            if (ro%RUNCLIM) then
                if (associated(vs%grid%fsin)) then
                    call output_variables_allocate(series%basin%fsin, n)
                    call output_variables_allocate(series%basin%ifsin, n)
                end if
                if (associated(vs%grid%flin)) call output_variables_allocate(series%basin%flin, n)
                if (associated(vs%grid%ta)) call output_variables_allocate(series%basin%ta, n)
                if (associated(vs%grid%qa)) call output_variables_allocate(series%basin%qa, n)
                if (associated(vs%grid%pres)) call output_variables_allocate(series%basin%pres, n)
                if (associated(vs%grid%uv)) call output_variables_allocate(series%basin%uv, n)
                if (associated(vs%grid%pre)) call output_variables_allocate(series%basin%pre, n)
            end if
            if (ro%RUNBALWB) then
                if (associated(vs%grid%pre)) call output_variables_allocate(series%basin%prec, n)
                if (associated(vs%grid%evap)) call output_variables_allocate(series%basin%evap, n)
                if (associated(vs%grid%pevp)) call output_variables_allocate(series%basin%pevp, n)
                if (associated(vs%grid%evpb)) call output_variables_allocate(series%basin%evpb, n)
                if (associated(vs%grid%arrd)) call output_variables_allocate(series%basin%arrd, n)
                if (associated(vs%grid%gro)) call output_variables_allocate(series%basin%gro, n)
                if (associated(vs%grid%rofo) .or. associated(vs%grid%rofs) .or. associated(vs%grid%rofb)) then
                    call output_variables_allocate(series%basin%rof, n)
                end if
                if (associated(vs%grid%rofo)) call output_variables_allocate(series%basin%rofo, n)
                if (associated(vs%grid%rofs)) call output_variables_allocate(series%basin%rofs, n)
                if (associated(vs%grid%rofb)) call output_variables_allocate(series%basin%rofb, n)
                if (associated(vs%grid%rcan)) call output_variables_allocate(series%basin%rcan, n)
                if (associated(vs%grid%sncan)) call output_variables_allocate(series%basin%sncan, n)
                if (associated(vs%grid%sno)) then
                    call output_variables_allocate(series%basin%sno, n)
                    call output_variables_allocate(series%basin%isno, n)
                end if
                if (associated(vs%grid%fsno)) call output_variables_allocate(series%basin%fsno, n)
                if (associated(vs%grid%wsno)) then
                    call output_variables_allocate(series%basin%wsno, n)
                    call output_variables_allocate(series%basin%isno, n)
                end if
                if (associated(vs%grid%zpnd)) then
                    call output_variables_allocate(series%basin%zpnd, n)
                    call output_variables_allocate(series%basin%ipnd, n)
                end if
                if (associated(vs%grid%pndw)) then
                    call output_variables_allocate(series%basin%pndw, n)
                    call output_variables_allocate(series%basin%ipnd, n)
                end if
                if (associated(vs%grid%lzs)) call output_variables_allocate(series%basin%lzs, n)
                if (associated(vs%grid%dzs)) call output_variables_allocate(series%basin%dzs, n)
                if (associated(vs%grid%thlq)) call output_variables_allocate(series%basin%thlq, n, nsl)
                if (associated(vs%grid%lqws)) call output_variables_allocate(series%basin%lqws, n, nsl)
                if (associated(vs%grid%thic)) call output_variables_allocate(series%basin%thic, n, nsl)
                if (associated(vs%grid%fzws)) call output_variables_allocate(series%basin%fzws, n, nsl)
                if (associated(vs%grid%lqws) .or. associated(vs%grid%fzws)) then
                    call output_variables_allocate(series%basin%alws, n, nsl)
                end if
                call output_variables_allocate(series%basin%stgw, n)
                call output_variables_allocate(series%basin%stg0w, n)
                call output_variables_allocate(series%basin%dstgw, n)
            end if
            if (ro%RUNBALEB) then
                if (associated(vs%grid%cmas)) call output_variables_allocate(series%basin%cmas, n)
                if (associated(vs%grid%tcan)) then
                    call output_variables_allocate(series%basin%tcan, n)
                    call output_variables_allocate(series%basin%ican, n)
                end if
                if (associated(vs%grid%tsno)) then
                    call output_variables_allocate(series%basin%tsno, n)
                    call output_variables_allocate(series%basin%isno, n)
                end if
                if (associated(vs%grid%tpnd)) then
                    call output_variables_allocate(series%basin%tpnd, n)
                    call output_variables_allocate(series%basin%ipnd, n)
                end if
                if (associated(vs%grid%alvs)) then
                    call output_variables_allocate(series%basin%alvs, n)
                end if
                if (associated(vs%grid%alir)) then
                    call output_variables_allocate(series%basin%alir, n)
                    call output_variables_allocate(series%basin%ifsin, n)
                end if
                if (associated(vs%grid%albt)) then
                    call output_variables_allocate(series%basin%albt, n)
                    call output_variables_allocate(series%basin%ifsin, n)
                end if
                if (associated(vs%grid%gte)) call output_variables_allocate(series%basin%gte, n)
                if (associated(vs%grid%fsin) .and. associated(vs%grid%albt)) then
                    call output_variables_allocate(series%basin%fsout, n)
                    call output_variables_allocate(series%basin%ifsin, n)
                end if
                if (associated(vs%grid%gte)) call output_variables_allocate(series%basin%flout, n)
                if (associated(vs%grid%hfs)) call output_variables_allocate(series%basin%qh, n)
                if (associated(vs%grid%qevp)) call output_variables_allocate(series%basin%qe, n)
                if (associated(vs%grid%gzero)) call output_variables_allocate(series%basin%gzero, n)
                if (associated(vs%grid%gflx)) call output_variables_allocate(series%basin%gflx, n, nsl)
                if (associated(vs%grid%tbar)) call output_variables_allocate(series%basin%tbar, n, nsl)
                call output_variables_allocate(series%basin%stge, n)
                call output_variables_allocate(series%basin%stg0e, n)
                call output_variables_allocate(series%basin%dstge, n)
            end if
            if (ro%RUNCHNL) then
                if (associated(vs%grid%rff)) call output_variables_allocate(series%basin%rff, n)
                if (associated(vs%grid%rchg)) call output_variables_allocate(series%basin%rchg, n)
                if (associated(vs%grid%qi)) call output_variables_allocate(series%basin%qi, n)
                if (associated(vs%grid%stgch)) call output_variables_allocate(series%basin%stgch, n)
                if (associated(vs%grid%qo)) call output_variables_allocate(series%basin%qo, n)
                if (associated(vs%grid%zlvl)) call output_variables_allocate(series%basin%zlvl, n)
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
        !> 'model_variables' required for 'vs'.
        use shd_variables
        use control_variables
        use climate_forcing
        use model_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(clim_info), intent(in) :: cm

        !> Local variables.
        integer ii, i, j
        real, dimension(size(shd%DA)) :: DA
        logical lcheck

        !> Tile-based.
        if (ro%RUNTILE) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (associated(out%ts%tile%fsin)) then
                    if (all(out%ts%tile%fsin == out%NO_DATA)) out%ts%tile%fsin = vs%tile%fsin
                end if
                if (associated(out%ts%tile%flin)) then
                    if (all(out%ts%tile%flin == out%NO_DATA)) out%ts%tile%flin = vs%tile%flin
                end if
                if (associated(out%ts%tile%ta)) then
                    if (all(out%ts%tile%ta == out%NO_DATA)) out%ts%tile%ta = vs%tile%ta
                end if
                if (associated(out%ts%tile%qa)) then
                    if (all(out%ts%tile%qa == out%NO_DATA)) out%ts%tile%qa = vs%tile%qa
                end if
                if (associated(out%ts%tile%pres)) then
                    if (all(out%ts%tile%pres == out%NO_DATA)) out%ts%tile%pres = vs%tile%pres
                end if
                if (associated(out%ts%tile%uv)) then
                    if (all(out%ts%tile%uv == out%NO_DATA)) out%ts%tile%uv = vs%tile%uv
                end if
                if (associated(out%ts%tile%pre)) then
                    if (all(out%ts%tile%pre == out%NO_DATA)) out%ts%tile%pre = vs%tile%pre
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(out%ts%tile%prec)) then
                    if (all(out%ts%tile%prec == out%NO_DATA)) out%ts%tile%prec = vs%tile%pre
                end if
                if (associated(out%ts%tile%evap)) then
                    if (all(out%ts%tile%evap == out%NO_DATA)) out%ts%tile%evap = vs%tile%evap
                end if
                if (associated(out%ts%tile%pevp)) then
                    if (all(out%ts%tile%pevp == out%NO_DATA)) out%ts%tile%pevp = vs%tile%pevp
                end if
                if (associated(out%ts%tile%evpb)) then
                    if (all(out%ts%tile%evpb == out%NO_DATA)) out%ts%tile%evpb = vs%tile%evpb
                end if
                if (associated(out%ts%tile%arrd)) then
                    if (all(out%ts%tile%arrd == out%NO_DATA)) out%ts%tile%arrd = vs%tile%arrd
                end if
                if (associated(out%ts%tile%gro)) then
                    if (all(out%ts%tile%gro == out%NO_DATA)) out%ts%tile%gro = vs%tile%gro
                end if
                if (associated(out%ts%tile%rof)) then
                    if (all(out%ts%tile%rof == out%NO_DATA)) then
                        out%ts%tile%rof = 0.0
                        lcheck = .true.
                    else
                        lcheck = .false.
                    end if
                    if (associated(out%ts%tile%rofo)) then
                        if (all(out%ts%tile%rofo == out%NO_DATA)) out%ts%tile%rofo = vs%tile%rofo
                        if (lcheck) out%ts%tile%rof = out%ts%tile%rof + vs%tile%rofo
                    end if
                    if (associated(out%ts%tile%rofs)) then
                        if (all(out%ts%tile%rofs == out%NO_DATA)) out%ts%tile%rofs = vs%tile%rofs
                        if (lcheck) out%ts%tile%rof = out%ts%tile%rof + vs%tile%rofs
                    end if
                    if (associated(out%ts%tile%rofb)) then
                        if (all(out%ts%tile%rofb == out%NO_DATA)) out%ts%tile%rofb = vs%tile%rofb
                        if (lcheck) out%ts%tile%rof = out%ts%tile%rof + vs%tile%rofb
                    end if
                end if
                if (associated(out%ts%tile%rcan)) then
                    if (all(out%ts%tile%rcan == out%NO_DATA)) then
                        out%ts%tile%rcan = vs%tile%rcan
                        out%ts%tile%stgw = out%ts%tile%stgw + vs%tile%rcan
                    end if
                end if
                if (associated(out%ts%tile%sncan)) then
                    if (all(out%ts%tile%sncan == out%NO_DATA)) then
                        out%ts%tile%sncan = vs%tile%sncan
                        out%ts%tile%stgw = out%ts%tile%stgw + vs%tile%sncan
                    end if
                end if
                if (associated(out%ts%tile%sno)) then
                    if (all(out%ts%tile%sno == out%NO_DATA)) then
                        out%ts%tile%sno = vs%tile%sno
                        out%ts%tile%stgw = out%ts%tile%stgw + vs%tile%sno
                    end if
                end if
                if (associated(out%ts%tile%fsno)) then
                    if (all(out%ts%tile%fsno == out%NO_DATA)) out%ts%tile%fsno = vs%tile%fsno
                end if
                if (associated(out%ts%tile%wsno)) then
                    if (all(out%ts%tile%wsno == out%NO_DATA)) then
                        out%ts%tile%wsno = vs%tile%wsno
                        out%ts%tile%stgw = out%ts%tile%stgw + vs%tile%wsno
                    end if
                end if
                if (associated(out%ts%tile%zpnd)) then
                    if (all(out%ts%tile%zpnd == out%NO_DATA)) out%ts%tile%zpnd = vs%tile%zpnd
                end if
                if (associated(out%ts%tile%pndw)) then
                    if (all(out%ts%tile%pndw == out%NO_DATA)) then
                        out%ts%tile%pndw = vs%tile%pndw
                        out%ts%tile%stgw = out%ts%tile%stgw + vs%tile%pndw
                    end if
                end if
                if (associated(out%ts%tile%lzs)) then
                    if (all(out%ts%tile%lzs == out%NO_DATA)) then
                        out%ts%tile%lzs = vs%tile%lzs
                        out%ts%tile%stgw = out%ts%tile%stgw + vs%tile%lzs
                    end if
                end if
                if (associated(out%ts%tile%dzs)) then
                    if (all(out%ts%tile%dzs == out%NO_DATA)) then
                        out%ts%tile%dzs = vs%tile%dzs
                        out%ts%tile%stgw = out%ts%tile%stgw + vs%tile%dzs
                    end if
                end if
                if (associated(out%ts%tile%thlq)) then
                    if (all(out%ts%tile%thlq == out%NO_DATA)) out%ts%tile%thlq = vs%tile%thlq
                end if
                if (associated(out%ts%tile%thic)) then
                    if (all(out%ts%tile%thic == out%NO_DATA)) out%ts%tile%thic = vs%tile%thic
                end if
                if (associated(out%ts%tile%alws)) then
                    if (all(out%ts%tile%alws == out%NO_DATA)) then
                        out%ts%tile%alws = 0.0
                        lcheck = .true.
                    else
                        lcheck = .false.
                    end if
                    if (associated(out%ts%tile%lqws)) then
                        if (all(out%ts%tile%lqws == out%NO_DATA)) out%ts%tile%lqws = vs%tile%lqws
                        out%ts%tile%stgw = out%ts%tile%stgw + sum(vs%tile%lqws, 2)
                        if (lcheck) out%ts%tile%alws = out%ts%tile%alws + vs%tile%lqws
                    end if
                    if (associated(out%ts%tile%fzws)) then
                        if (all(out%ts%tile%fzws == out%NO_DATA)) out%ts%tile%fzws = vs%tile%fzws
                        out%ts%tile%stgw = out%ts%tile%stgw + sum(vs%tile%fzws, 2)
                        if (lcheck) out%ts%tile%alws = out%ts%tile%alws + vs%tile%fzws
                    end if
                end if
                if (all(out%ts%tile%stgw == 0.0)) then
                    out%ts%tile%stg0w = out%NO_DATA
                    out%ts%tile%stgw = out%NO_DATA
                    out%ts%tile%dstgw = out%NO_DATA
                else
                    out%ts%tile%dstgw = out%ts%tile%stgw - out%ts%tile%stg0w
                end if
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(out%ts%tile%cmas)) then
                    if (all(out%ts%tile%cmas == out%NO_DATA)) out%ts%tile%cmas = vs%tile%cmas
                end if
                if (associated(out%ts%tile%tcan)) then
                    if (all(out%ts%tile%tcan == out%NO_DATA)) out%ts%tile%tcan = vs%tile%tcan
                end if
                if (associated(out%ts%tile%tsno)) then
                    if (all(out%ts%tile%tsno == out%NO_DATA)) out%ts%tile%tsno = vs%tile%tsno
                end if
                if (associated(out%ts%tile%tpnd)) then
                    if (all(out%ts%tile%tpnd == out%NO_DATA)) out%ts%tile%tpnd = vs%tile%tpnd
                end if
                if (associated(out%ts%tile%alvs)) then
                    if (all(out%ts%tile%alvs == out%NO_DATA)) out%ts%tile%alvs = vs%tile%alvs
                end if
                if (associated(out%ts%tile%alir)) then
                    if (all(out%ts%tile%alir == out%NO_DATA)) out%ts%tile%alir = vs%tile%alir
                end if
                if (associated(out%ts%tile%albt)) then
                    if (all(out%ts%tile%albt == out%NO_DATA)) out%ts%tile%albt = vs%tile%albt
                end if
                if (associated(out%ts%tile%gte)) then
                    if (all(out%ts%tile%gte == out%NO_DATA)) out%ts%tile%gte = vs%tile%gte
                end if
                if (associated(out%ts%tile%fsout)) then
                    if (all(out%ts%tile%fsout == out%NO_DATA)) out%ts%tile%fsout = vs%tile%fsin*(1.0 - vs%tile%albt)
                end if
                if (associated(out%ts%tile%flout)) then
                    if (all(out%ts%tile%flout == out%NO_DATA)) out%ts%tile%flout = 5.66796E-8*vs%tile%gte**4
                end if
                if (associated(out%ts%tile%qh)) then
                    if (all(out%ts%tile%qh == out%NO_DATA)) out%ts%tile%qh = vs%tile%hfs
                end if
                if (associated(out%ts%tile%qe)) then
                    if (all(out%ts%tile%qe == out%NO_DATA)) out%ts%tile%qe = vs%tile%qevp
                end if
                if (associated(out%ts%tile%gzero)) then
                    if (all(out%ts%tile%gzero == out%NO_DATA)) out%ts%tile%gzero = vs%tile%gzero
                end if
                if (associated(out%ts%tile%gflx)) then
                    if (all(out%ts%tile%gflx == out%NO_DATA)) out%ts%tile%gflx = vs%tile%gflx
                end if
                if (associated(out%ts%tile%tbar)) then
                    if (all(out%ts%tile%tbar == out%NO_DATA)) out%ts%tile%tbar = vs%tile%tbar
                end if
                if (all(out%ts%tile%stge == 0.0)) then
                    out%ts%tile%stg0e = out%NO_DATA
                    out%ts%tile%stge = out%NO_DATA
                    out%ts%tile%dstge = out%NO_DATA
                else
                    out%ts%tile%dstge = out%ts%tile%stge - out%ts%tile%stg0e
                end if
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (associated(out%ts%grid%fsin)) then
                    if (all(out%ts%grid%fsin == out%NO_DATA)) out%ts%grid%fsin = vs%grid%fsin
                end if
                if (associated(out%ts%grid%flin)) then
                    if (all(out%ts%grid%flin == out%NO_DATA)) out%ts%grid%flin = vs%grid%flin
                end if
                if (associated(out%ts%grid%ta)) then
                    if (all(out%ts%grid%ta == out%NO_DATA)) out%ts%grid%ta = vs%grid%ta
                end if
                if (associated(out%ts%grid%qa)) then
                    if (all(out%ts%grid%qa == out%NO_DATA)) out%ts%grid%qa = vs%grid%qa
                end if
                if (associated(out%ts%grid%pres)) then
                    if (all(out%ts%grid%pres == out%NO_DATA)) out%ts%grid%pres = vs%grid%pres
                end if
                if (associated(out%ts%grid%uv)) then
                    if (all(out%ts%grid%uv == out%NO_DATA)) out%ts%grid%uv = vs%grid%uv
                end if
                if (associated(out%ts%grid%pre)) then
                    if (all(out%ts%grid%pre == out%NO_DATA)) out%ts%grid%pre = vs%grid%pre
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(out%ts%grid%prec)) then
                    if (all(out%ts%grid%prec == out%NO_DATA)) out%ts%grid%prec = vs%grid%pre
                end if
                if (associated(out%ts%grid%evap)) then
                    if (all(out%ts%grid%evap == out%NO_DATA)) out%ts%grid%evap = vs%grid%evap
                end if
                if (associated(out%ts%grid%pevp)) then
                    if (all(out%ts%grid%pevp == out%NO_DATA)) out%ts%grid%pevp = vs%grid%pevp
                end if
                if (associated(out%ts%grid%evpb)) then
                    if (all(out%ts%grid%evpb == out%NO_DATA)) out%ts%grid%evpb = vs%grid%evpb
                end if
                if (associated(out%ts%grid%arrd)) then
                    if (all(out%ts%grid%arrd == out%NO_DATA)) out%ts%grid%arrd = vs%grid%arrd
                end if
                if (associated(out%ts%grid%gro)) then
                    if (all(out%ts%grid%gro == out%NO_DATA)) out%ts%grid%gro = vs%grid%gro
                end if
                if (associated(out%ts%grid%rof)) then
                    if (all(out%ts%grid%rof == out%NO_DATA)) then
                        out%ts%grid%rof = 0.0
                        lcheck = .true.
                    else
                        lcheck = .false.
                    end if
                    if (associated(out%ts%grid%rofo)) then
                        if (all(out%ts%grid%rofo == out%NO_DATA)) out%ts%grid%rofo = vs%grid%rofo
                        if (lcheck) out%ts%grid%rof = out%ts%grid%rof + vs%grid%rofo
                    end if
                    if (associated(out%ts%grid%rofs)) then
                        if (all(out%ts%grid%rofs == out%NO_DATA)) out%ts%grid%rofs = vs%grid%rofs
                        if (lcheck) out%ts%grid%rof = out%ts%grid%rof + vs%grid%rofs
                    end if
                    if (associated(out%ts%grid%rofb)) then
                        if (all(out%ts%grid%rofb == out%NO_DATA)) out%ts%grid%rofb = vs%grid%rofb
                        if (lcheck) out%ts%grid%rof = out%ts%grid%rof + vs%grid%rofb
                    end if
                end if
                if (associated(out%ts%grid%rcan)) then
                    if (all(out%ts%grid%rcan == out%NO_DATA)) then
                        out%ts%grid%rcan = vs%grid%rcan
                        out%ts%grid%stgw = out%ts%grid%stgw + vs%grid%rcan
                    end if
                end if
                if (associated(out%ts%grid%sncan)) then
                    if (all(out%ts%grid%sncan == out%NO_DATA)) then
                        out%ts%grid%sncan = vs%grid%sncan
                        out%ts%grid%stgw = out%ts%grid%stgw + vs%grid%sncan
                    end if
                end if
                if (associated(out%ts%grid%sno)) then
                    if (all(out%ts%grid%sno == out%NO_DATA)) then
                        out%ts%grid%sno = vs%grid%sno
                        out%ts%grid%stgw = out%ts%grid%stgw + vs%grid%sno
                    end if
                end if
                if (associated(out%ts%grid%fsno)) then
                    if (all(out%ts%grid%fsno == out%NO_DATA)) out%ts%grid%fsno = vs%grid%fsno
                end if
                if (associated(out%ts%grid%wsno)) then
                    if (all(out%ts%grid%wsno == out%NO_DATA)) then
                        out%ts%grid%wsno = vs%grid%wsno
                        out%ts%grid%stgw = out%ts%grid%stgw + vs%grid%wsno
                    end if
                end if
                if (associated(out%ts%grid%zpnd)) then
                    if (all(out%ts%grid%zpnd == out%NO_DATA)) then
                        out%ts%grid%zpnd = vs%grid%zpnd
                    end if
                end if
                if (associated(out%ts%grid%pndw)) then
                    if (all(out%ts%grid%pndw == out%NO_DATA)) then
                        out%ts%grid%pndw = vs%grid%pndw
                        out%ts%grid%stgw = out%ts%grid%stgw + vs%grid%pndw
                    end if
                end if
                if (associated(out%ts%grid%lzs)) then
                    if (all(out%ts%grid%lzs == out%NO_DATA)) then
                        out%ts%grid%lzs = vs%grid%lzs
                        out%ts%grid%stgw = out%ts%grid%stgw + vs%grid%lzs
                    end if
                end if
                if (associated(out%ts%grid%dzs)) then
                    if (all(out%ts%grid%dzs == out%NO_DATA)) then
                        out%ts%grid%dzs = vs%grid%dzs
                        out%ts%grid%stgw = out%ts%grid%stgw + vs%grid%dzs
                    end if
                end if
                if (associated(out%ts%grid%thlq)) then
                    if (all(out%ts%grid%thlq == out%NO_DATA)) out%ts%grid%thlq = vs%grid%thlq
                end if
                if (associated(out%ts%grid%thic)) then
                    if (all(out%ts%grid%thic == out%NO_DATA)) out%ts%grid%thic = vs%grid%thic
                end if
                if (associated(out%ts%grid%alws)) then
                    if (all(out%ts%grid%alws == out%NO_DATA)) then
                        out%ts%grid%alws = 0.0
                        lcheck = .true.
                    else
                        lcheck = .false.
                    end if
                    if (associated(out%ts%grid%lqws)) then
                        if (all(out%ts%grid%lqws == out%NO_DATA)) out%ts%grid%lqws = vs%grid%lqws
                        out%ts%grid%stgw = out%ts%grid%stgw + sum(vs%grid%lqws, 2)
                        if (lcheck) out%ts%grid%alws = out%ts%grid%alws + vs%grid%lqws
                    end if
                    if (associated(out%ts%grid%fzws)) then
                        if (all(out%ts%grid%fzws == out%NO_DATA)) out%ts%grid%fzws = vs%grid%fzws
                        out%ts%grid%stgw = out%ts%grid%stgw + sum(vs%grid%fzws, 2)
                        if (lcheck) out%ts%grid%alws = out%ts%grid%alws + vs%grid%fzws
                    end if
                end if
                if (all(out%ts%grid%stgw == 0.0)) then
                    out%ts%grid%stg0w = out%NO_DATA
                    out%ts%grid%stgw = out%NO_DATA
                    out%ts%grid%dstgw = out%NO_DATA
                else
                    out%ts%grid%dstgw = out%ts%grid%stgw - out%ts%grid%stg0w
                end if
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(out%ts%grid%cmas)) then
                    if (all(out%ts%grid%cmas == out%NO_DATA)) out%ts%grid%cmas = vs%grid%cmas
                end if
                if (associated(out%ts%grid%tcan)) then
                    if (all(out%ts%grid%tcan == out%NO_DATA)) out%ts%grid%tcan = vs%grid%tcan
                end if
                if (associated(out%ts%grid%tsno)) then
                    if (all(out%ts%grid%tsno == out%NO_DATA)) out%ts%grid%tsno = vs%grid%tsno
                end if
                if (associated(out%ts%grid%tpnd)) then
                    if (all(out%ts%grid%tpnd == out%NO_DATA)) out%ts%grid%tpnd = vs%grid%tpnd
                end if
                if (associated(out%ts%grid%alvs)) then
                    if (all(out%ts%grid%alvs == out%NO_DATA)) out%ts%grid%alvs = vs%grid%alvs
                end if
                if (associated(out%ts%grid%alir)) then
                    if (all(out%ts%grid%alir == out%NO_DATA)) out%ts%grid%alir = vs%grid%alir
                end if
                if (associated(out%ts%grid%albt)) then
                    if (all(out%ts%grid%albt == out%NO_DATA)) out%ts%grid%albt = vs%grid%albt
                end if
                if (associated(out%ts%grid%gte)) then
                    if (all(out%ts%grid%gte == out%NO_DATA)) out%ts%grid%gte = vs%grid%gte
                end if
                if (associated(out%ts%grid%fsout)) then
                    if (all(out%ts%grid%fsout == out%NO_DATA)) out%ts%grid%fsout = vs%grid%fsin*(1.0 - vs%grid%albt)
                end if
                if (associated(out%ts%grid%flout)) then
                    if (all(out%ts%grid%flout == out%NO_DATA)) out%ts%grid%flout = 5.66796E-8*vs%grid%gte**4
                end if
                if (associated(out%ts%grid%qh)) then
                    if (all(out%ts%grid%qh == out%NO_DATA)) out%ts%grid%qh = vs%grid%hfs
                end if
                if (associated(out%ts%grid%qe)) then
                    if (all(out%ts%grid%qe == out%NO_DATA)) out%ts%grid%qe = vs%grid%qevp
                end if
                if (associated(out%ts%grid%gzero)) then
                    if (all(out%ts%grid%gzero == out%NO_DATA)) out%ts%grid%gzero = vs%grid%gzero
                end if
                if (associated(out%ts%grid%gflx)) then
                    if (all(out%ts%grid%gflx == out%NO_DATA)) out%ts%grid%gflx = vs%grid%gflx
                end if
                if (associated(out%ts%grid%tbar)) then
                    if (all(out%ts%grid%tbar == out%NO_DATA)) out%ts%grid%tbar = vs%grid%tbar
                end if
                if (all(out%ts%grid%stge == 0.0)) then
                    out%ts%grid%stg0e = out%NO_DATA
                    out%ts%grid%stge = out%NO_DATA
                    out%ts%grid%dstge = out%NO_DATA
                else
                    out%ts%grid%dstge = out%ts%grid%stge - out%ts%grid%stg0e
                end if
            end if

            !> Channels and routing.
            if (ro%RUNCHNL) then
                if (associated(out%ts%grid%rff)) then
                    if (all(out%ts%grid%rff == out%NO_DATA)) out%ts%grid%rff = vs%grid%rff
                end if
                if (associated(out%ts%grid%rchg)) then
                    if (all(out%ts%grid%rchg == out%NO_DATA)) out%ts%grid%rchg = vs%grid%rchg
                end if
                if (associated(out%ts%grid%qi)) then
                    if (all(out%ts%grid%qi == out%NO_DATA)) out%ts%grid%qi = vs%grid%qi
                end if
                if (associated(out%ts%grid%stgch)) then
                    if (all(out%ts%grid%stgch == out%NO_DATA)) out%ts%grid%stgch = vs%grid%stgch
                end if
                if (associated(out%ts%grid%qo)) then
                    if (all(out%ts%grid%qo == out%NO_DATA)) out%ts%grid%qo = vs%grid%qo
                end if
                if (associated(out%ts%grid%zlvl)) then
                    if (all(out%ts%grid%zlvl == out%NO_DATA)) out%ts%grid%zlvl = vs%grid%zlvl
                end if
            end if

            !> Initialize variables for DA averaged.
            if (ro%RUNCLIM) then
                where (shd%DA > 0.0)
                    out%ts%basin%fsin = out%ts%grid%fsin*shd%FRAC
                    out%ts%basin%flin = out%ts%grid%flin*shd%FRAC
                    out%ts%basin%ta = out%ts%grid%ta*shd%FRAC
                    out%ts%basin%qa = out%ts%grid%qa*shd%FRAC
                    out%ts%basin%pres = out%ts%grid%pres*shd%FRAC
                    out%ts%basin%uv = out%ts%grid%uv*shd%FRAC
                    out%ts%basin%pre = out%ts%grid%pre*shd%FRAC
                end where
            end if
            if (ro%RUNBALWB) then
                where (shd%DA > 0.0)
                    out%ts%basin%prec = out%ts%grid%prec*shd%FRAC
                    out%ts%basin%evap = out%ts%grid%evap*shd%FRAC
                    out%ts%basin%pevp = out%ts%grid%pevp*shd%FRAC
                    out%ts%basin%evpb = out%ts%grid%evpb*shd%FRAC
                    out%ts%basin%arrd = out%ts%grid%arrd*shd%FRAC
                    out%ts%basin%gro = out%ts%grid%gro*shd%FRAC
                    out%ts%basin%rof = out%ts%grid%rof*shd%FRAC
                    out%ts%basin%rofo = out%ts%grid%rofo*shd%FRAC
                    out%ts%basin%rofs = out%ts%grid%rofs*shd%FRAC
                    out%ts%basin%rofb = out%ts%grid%rofb*shd%FRAC
                    out%ts%basin%rcan = out%ts%grid%rcan*shd%FRAC
                    out%ts%basin%sncan = out%ts%grid%sncan*shd%FRAC
                    out%ts%basin%sno = out%ts%grid%sno*shd%FRAC
                    out%ts%basin%fsno = out%ts%grid%fsno*shd%FRAC
                    out%ts%basin%wsno = out%ts%grid%wsno*shd%FRAC
                    out%ts%basin%zpnd = out%ts%grid%zpnd*shd%FRAC
                    out%ts%basin%pndw = out%ts%grid%pndw*shd%FRAC
                    out%ts%basin%lzs = out%ts%grid%lzs*shd%FRAC
                    out%ts%basin%dzs = out%ts%grid%dzs*shd%FRAC
                end where
                do j = 1, shd%lc%IGND
                    where (shd%DA > 0.0)
                        out%ts%basin%thlq(:, j) = out%ts%grid%thlq(:, j)*shd%FRAC
                        out%ts%basin%lqws(:, j) = out%ts%grid%lqws(:, j)*shd%FRAC
                        out%ts%basin%thic(:, j) = out%ts%grid%thic(:, j)*shd%FRAC
                        out%ts%basin%fzws(:, j) = out%ts%grid%fzws(:, j)*shd%FRAC
                        out%ts%basin%alws(:, j) = out%ts%grid%alws(:, j)*shd%FRAC
                    end where
                end do
                where (shd%DA > 0.0)
                    out%ts%basin%stg0w = out%ts%grid%stg0w*shd%FRAC
                    out%ts%basin%stgw = out%ts%grid%stgw*shd%FRAC
                    out%ts%basin%dstgw = out%ts%grid%dstgw*shd%FRAC
                end where
            end if
            if (ro%RUNBALEB) then
                where (shd%DA > 0.0)
                    out%ts%basin%cmas = out%ts%grid%cmas*shd%FRAC
                    out%ts%basin%tcan = out%ts%grid%tcan*shd%FRAC
                    out%ts%basin%tsno = out%ts%grid%tsno*shd%FRAC
                    out%ts%basin%tpnd = out%ts%grid%tpnd*shd%FRAC
                    out%ts%basin%alvs = out%ts%grid%alvs*shd%FRAC
                    out%ts%basin%alir = out%ts%grid%alir*shd%FRAC
                    out%ts%basin%albt = out%ts%grid%albt*shd%FRAC
                    out%ts%basin%gte = out%ts%grid%gte*shd%FRAC
                    out%ts%basin%fsout = out%ts%grid%fsout*shd%FRAC
                    out%ts%basin%flout = out%ts%grid%flout*shd%FRAC
                    out%ts%basin%qh = out%ts%grid%qh*shd%FRAC
                    out%ts%basin%qe = out%ts%grid%qe*shd%FRAC
                    out%ts%basin%gzero = out%ts%grid%gzero*shd%FRAC
                end where
                do j = 1, shd%lc%IGND
                    where (shd%DA > 0.0)
                        out%ts%basin%gflx(:, j) = out%ts%grid%gflx(:, j)*shd%FRAC
                        out%ts%basin%tbar(:, j) = out%ts%grid%tbar(:, j)*shd%FRAC
                    end where
                end do
                where (shd%DA > 0.0)
                    out%ts%basin%stg0e = out%ts%grid%stg0e*shd%FRAC
                    out%ts%basin%stge = out%ts%grid%stge*shd%FRAC
                    out%ts%basin%dstge = out%ts%grid%dstge*shd%FRAC
                end where
            end if
            if (ro%RUNCHNL) then
                where (shd%DA > 0.0)
                    out%ts%basin%rff = out%ts%grid%rff*shd%FRAC
                    out%ts%basin%rchg = out%ts%grid%rchg*shd%FRAC
                    out%ts%basin%qi = out%ts%grid%qi
                    out%ts%basin%stgch = out%ts%grid%stgch*shd%FRAC
                    out%ts%basin%qo = out%ts%grid%qo
                    out%ts%basin%zlvl = out%ts%grid%zlvl
                end where
            end if

            !> Counters.
            where (out%ts%basin%fsin > 0.0)
                out%ts%basin%ifsin = 1
            elsewhere
                out%ts%basin%ifsin = 0
            end where
            where (out%ts%basin%sno > 0.0)
                out%ts%basin%isno = 1
            elsewhere
                out%ts%basin%isno = 0
            end where
            where (out%ts%basin%zpnd > 0.0)
                out%ts%basin%ipnd = 1
            elsewhere
                out%ts%basin%ipnd = 0
            end where
            where (out%ts%basin%tcan > 0.0)
                out%ts%basin%ican = 1
            elsewhere
                out%ts%basin%ican = 0
            end where

            !> DA averaged.
            do i = 1, shd%NAA
                ii = shd%NEXT(i)
                if (ii > 0) then

                    !> Counter for time-averaging is either on (1) if any cell has contributed or off (0).
                    out%ts%basin%ifsin(ii) = max(out%ts%basin%ifsin(ii), out%ts%basin%ifsin(i))
                    out%ts%basin%ican(ii) = max(out%ts%basin%ican(ii), out%ts%basin%ican(i))
                    out%ts%basin%isno(ii) = max(out%ts%basin%isno(ii), out%ts%basin%isno(i))
                    out%ts%basin%ipnd(ii) = max(out%ts%basin%ipnd(ii), out%ts%basin%ipnd(i))

                    !> Update variables.
                    if (ro%RUNCLIM) then
                        if (out%ts%basin%fsin(i) /= out%NO_DATA) then
                            out%ts%basin%fsin(ii) = out%ts%basin%fsin(ii) + out%ts%basin%fsin(i)
                        end if
                        if (out%ts%basin%flin(i) /= out%NO_DATA) then
                            out%ts%basin%flin(ii) = out%ts%basin%flin(ii) + out%ts%basin%flin(i)
                        end if
                        if (out%ts%basin%ta(i) /= out%NO_DATA) then
                            out%ts%basin%ta(ii) = out%ts%basin%ta(ii) + out%ts%basin%ta(i)
                        end if
                        if (out%ts%basin%qa(i) /= out%NO_DATA) then
                            out%ts%basin%qa(ii) = out%ts%basin%qa(ii) + out%ts%basin%qa(i)
                        end if
                        if (out%ts%basin%pres(i) /= out%NO_DATA) then
                            out%ts%basin%pres(ii) = out%ts%basin%pres(ii) + out%ts%basin%pres(i)
                        end if
                        if (out%ts%basin%uv(i) /= out%NO_DATA) then
                            out%ts%basin%uv(ii) = out%ts%basin%uv(ii) + out%ts%basin%uv(i)
                        end if
                        if (out%ts%basin%pre(i) /= out%NO_DATA) then
                            out%ts%basin%pre(ii) = out%ts%basin%pre(ii) + out%ts%basin%pre(i)
                        end if
                    end if
                    if (ro%RUNBALWB) then
                        if (out%ts%basin%prec(i) /= out%NO_DATA) then
                            out%ts%basin%prec(ii) = out%ts%basin%prec(ii) + out%ts%basin%prec(i)
                        end if
                        if (out%ts%basin%evap(i) /= out%NO_DATA) then
                            out%ts%basin%evap(ii) = out%ts%basin%evap(ii) + out%ts%basin%evap(i)
                        end if
                        if (out%ts%basin%pevp(i) /= out%NO_DATA) then
                            out%ts%basin%pevp(ii) = out%ts%basin%pevp(ii) + out%ts%basin%pevp(i)
                        end if
                        if (out%ts%basin%evpb(i) /= out%NO_DATA) then
                            out%ts%basin%evpb(ii) = out%ts%basin%evpb(ii) + out%ts%basin%evpb(i)
                        end if
                        if (out%ts%basin%arrd(i) /= out%NO_DATA) then
                            out%ts%basin%arrd(ii) = out%ts%basin%arrd(ii) + out%ts%basin%arrd(i)
                        end if
                        if (out%ts%basin%gro(i) /= out%NO_DATA) then
                            out%ts%basin%gro(ii) = out%ts%basin%gro(ii) + out%ts%basin%gro(i)
                        end if
                        if (out%ts%basin%rof(i) /= out%NO_DATA) then
                            out%ts%basin%rof(ii) = out%ts%basin%rof(ii) + out%ts%basin%rof(i)
                        end if
                        if (out%ts%basin%rofo(i) /= out%NO_DATA) then
                            out%ts%basin%rofo(ii) = out%ts%basin%rofo(ii) + out%ts%basin%rofo(i)
                        end if
                        if (out%ts%basin%rofs(i) /= out%NO_DATA) then
                            out%ts%basin%rofs(ii) = out%ts%basin%rofs(ii) + out%ts%basin%rofs(i)
                        end if
                        if (out%ts%basin%rofb(i) /= out%NO_DATA) then
                            out%ts%basin%rofb(ii) = out%ts%basin%rofb(ii) + out%ts%basin%rofb(i)
                        end if
                        if (out%ts%basin%rcan(i) /= out%NO_DATA) then
                            out%ts%basin%rcan(ii) = out%ts%basin%rcan(ii) + out%ts%basin%rcan(i)
                        end if
                        if (out%ts%basin%sncan(i) /= out%NO_DATA) then
                            out%ts%basin%sncan(ii) = out%ts%basin%sncan(ii) + out%ts%basin%sncan(i)
                        end if
                        if (out%ts%basin%sno(i) /= out%NO_DATA) then
                            out%ts%basin%sno(ii) = out%ts%basin%sno(ii) + out%ts%basin%sno(i)
                        end if
                        if (out%ts%basin%fsno(i) /= out%NO_DATA) then
                            out%ts%basin%fsno(ii) = out%ts%basin%fsno(ii) + out%ts%basin%fsno(i)
                        end if
                        if (out%ts%basin%wsno(i) /= out%NO_DATA) then
                            out%ts%basin%wsno(ii) = out%ts%basin%wsno(ii) + out%ts%basin%wsno(i)
                        end if
                        if (out%ts%basin%zpnd(i) /= out%NO_DATA) then
                            out%ts%basin%zpnd(ii) = out%ts%basin%zpnd(ii) + out%ts%basin%zpnd(i)
                        end if
                        if (out%ts%basin%pndw(i) /= out%NO_DATA) then
                            out%ts%basin%pndw(ii) = out%ts%basin%pndw(ii) + out%ts%basin%pndw(i)
                        end if
                        if (out%ts%basin%lzs(i) /= out%NO_DATA) then
                            out%ts%basin%lzs(ii) = out%ts%basin%lzs(ii) + out%ts%basin%lzs(i)
                        end if
                        if (out%ts%basin%dzs(i) /= out%NO_DATA) then
                            out%ts%basin%dzs(ii) = out%ts%basin%dzs(ii) + out%ts%basin%dzs(i)
                        end if
                        where (out%ts%basin%thlq(i, :) /= out%NO_DATA)
                            out%ts%basin%thlq(ii, :) = out%ts%basin%thlq(ii, :) + out%ts%basin%thlq(i, :)
                        end where
                        where (out%ts%basin%lqws(i, :) /= out%NO_DATA)
                            out%ts%basin%lqws(ii, :) = out%ts%basin%lqws(ii, :) + out%ts%basin%lqws(i, :)
                        end where
                        where (out%ts%basin%thic(i, :) /= out%NO_DATA)
                            out%ts%basin%thic(ii, :) = out%ts%basin%thic(ii, :) + out%ts%basin%thic(i, :)
                        end where
                        where (out%ts%basin%fzws(i, :) /= out%NO_DATA)
                            out%ts%basin%fzws(ii, :) = out%ts%basin%fzws(ii, :) + out%ts%basin%fzws(i, :)
                        end where
                        where (out%ts%basin%alws(i, :) /= out%NO_DATA)
                            out%ts%basin%alws(ii, :) = out%ts%basin%alws(ii, :) + out%ts%basin%alws(i, :)
                        end where
                        if (out%ts%basin%stg0w(i) /= out%NO_DATA) then
                            out%ts%basin%stg0w(ii) = out%ts%basin%stg0w(ii) + out%ts%basin%stg0w(i)
                        end if
                        if (out%ts%basin%stgw(i) /= out%NO_DATA) then
                            out%ts%basin%stgw(ii) = out%ts%basin%stgw(ii) + out%ts%basin%stgw(i)
                        end if
                        if (out%ts%basin%dstgw(i) /= out%NO_DATA) then
                            out%ts%basin%dstgw(ii) = out%ts%basin%dstgw(ii) + out%ts%basin%dstgw(i)
                        end if
                    end if
                    if (ro%RUNBALEB) then
                        if (out%ts%basin%cmas(i) /= out%NO_DATA) then
                            out%ts%basin%cmas(ii) = out%ts%basin%cmas(ii) + out%ts%basin%cmas(i)
                        end if
                        if (out%ts%basin%tcan(i) /= out%NO_DATA) then
                            out%ts%basin%tcan(ii) = out%ts%basin%tcan(ii) + out%ts%basin%tcan(i)
                        end if
                        if (out%ts%basin%tsno(i) /= out%NO_DATA) then
                            out%ts%basin%tsno(ii) = out%ts%basin%tsno(ii) + out%ts%basin%tsno(i)
                        end if
                        if (out%ts%basin%tpnd(i) /= out%NO_DATA) then
                            out%ts%basin%tpnd(ii) = out%ts%basin%tpnd(ii) + out%ts%basin%tpnd(i)
                        end if
                        if (out%ts%basin%alvs(i) /= out%NO_DATA) then
                            out%ts%basin%alvs(ii) = out%ts%basin%alvs(ii) + out%ts%basin%alvs(i)
                        end if
                        if (out%ts%basin%alir(i) /= out%NO_DATA) then
                            out%ts%basin%alir(ii) = out%ts%basin%alir(ii) + out%ts%basin%alir(i)
                        end if
                        if (out%ts%basin%albt(i) /= out%NO_DATA) then
                            out%ts%basin%albt(ii) = out%ts%basin%albt(ii) + out%ts%basin%albt(i)
                        end if
                        if (out%ts%basin%gte(i) /= out%NO_DATA) then
                            out%ts%basin%gte(ii) = out%ts%basin%gte(ii) + out%ts%basin%gte(i)
                        end if
                        if (out%ts%basin%fsout(i) /= out%NO_DATA) then
                            out%ts%basin%fsout(ii) = out%ts%basin%fsout(ii) + out%ts%basin%fsout(i)
                        end if
                        if (out%ts%basin%flout(i) /= out%NO_DATA) then
                            out%ts%basin%flout(ii) = out%ts%basin%flout(ii) + out%ts%basin%flout(i)
                        end if
                        if (out%ts%basin%qh(i) /= out%NO_DATA) then
                            out%ts%basin%qh(ii) = out%ts%basin%qh(ii) + out%ts%basin%qh(i)
                        end if
                        if (out%ts%basin%qe(i) /= out%NO_DATA) then
                            out%ts%basin%qe(ii) = out%ts%basin%qe(ii) + out%ts%basin%qe(i)
                        end if
                        if (out%ts%basin%gzero(i) /= out%NO_DATA) then
                            out%ts%basin%gzero(ii) = out%ts%basin%gzero(ii) + out%ts%basin%gzero(i)
                        end if
                        where (out%ts%basin%gflx(i, :) /= out%NO_DATA)
                            out%ts%basin%gflx(ii, :) = out%ts%basin%gflx(ii, :) + out%ts%basin%gflx(i, :)
                        end where
                        where (out%ts%basin%tbar(i, :) /= out%NO_DATA)
                            out%ts%basin%tbar(ii, :) = out%ts%basin%tbar(ii, :) + out%ts%basin%tbar(i, :)
                        end where
                        if (out%ts%basin%stg0e(i) /= out%NO_DATA) then
                            out%ts%basin%stg0e(ii) = out%ts%basin%stg0e(ii) + out%ts%basin%stg0e(i)
                        end if
                        if (out%ts%basin%stge(i) /= out%NO_DATA) then
                            out%ts%basin%stge(ii) = out%ts%basin%stge(ii) + out%ts%basin%stge(i)
                        end if
                        if (out%ts%basin%dstge(i) /= out%NO_DATA) then
                            out%ts%basin%dstge(ii) = out%ts%basin%dstge(ii) + out%ts%basin%dstge(i)
                        end if
                    end if
                    if (ro%RUNCHNL) then
                        if (out%ts%basin%rff(i) /= out%NO_DATA) then
                            out%ts%basin%rff(ii) = out%ts%basin%rff(ii) + out%ts%basin%rff(i)
                        end if
                        if (out%ts%basin%rchg(i) /= out%NO_DATA) then
                            out%ts%basin%rchg(ii) = out%ts%basin%rchg(ii) + out%ts%basin%rff(i)
                        end if
                        if (out%ts%basin%stgch(i) /= out%NO_DATA) then
                            out%ts%basin%stgch(ii) = out%ts%basin%stgch(ii) + out%ts%basin%rff(i)
                        end if
                    end if
                end if
            end do

            !> Normalize.
            DA = shd%DA/((shd%AL/1000.0)**2)
            if (ro%RUNCLIM) then
                where (DA > 0.0)
                    where (out%ts%basin%fsin /= out%NO_DATA) out%ts%basin%fsin = out%ts%basin%fsin/DA
                    where (out%ts%basin%flin /= out%NO_DATA) out%ts%basin%flin = out%ts%basin%flin/DA
                    where (out%ts%basin%ta /= out%NO_DATA) out%ts%basin%ta = out%ts%basin%ta/DA
                    where (out%ts%basin%qa /= out%NO_DATA) out%ts%basin%qa = out%ts%basin%qa/DA
                    where (out%ts%basin%pres /= out%NO_DATA) out%ts%basin%pres = out%ts%basin%pres/DA
                    where (out%ts%basin%uv /= out%NO_DATA) out%ts%basin%uv = out%ts%basin%uv/DA
                    where (out%ts%basin%pre /= out%NO_DATA) out%ts%basin%pre = out%ts%basin%pre/DA
                end where
            end if
            if (ro%RUNBALWB) then
                where (DA > 0.0)
                    where (out%ts%basin%prec /= out%NO_DATA) out%ts%basin%prec = out%ts%basin%prec/DA
                    where (out%ts%basin%evap /= out%NO_DATA) out%ts%basin%evap = out%ts%basin%evap/DA
                    where (out%ts%basin%pevp /= out%NO_DATA) out%ts%basin%pevp = out%ts%basin%pevp/DA
                    where (out%ts%basin%evpb /= out%NO_DATA) out%ts%basin%evpb = out%ts%basin%evpb/DA
                    where (out%ts%basin%arrd /= out%NO_DATA) out%ts%basin%arrd = out%ts%basin%arrd/DA
                    where (out%ts%basin%gro /= out%NO_DATA) out%ts%basin%gro = out%ts%basin%gro/DA
                    where (out%ts%basin%rof /= out%NO_DATA) out%ts%basin%rof = out%ts%basin%rof/DA
                    where (out%ts%basin%rofo /= out%NO_DATA) out%ts%basin%rofo = out%ts%basin%rofo/DA
                    where (out%ts%basin%rofs /= out%NO_DATA) out%ts%basin%rofs = out%ts%basin%rofs/DA
                    where (out%ts%basin%rofb /= out%NO_DATA) out%ts%basin%rofb = out%ts%basin%rofb/DA
                    where (out%ts%basin%rcan /= out%NO_DATA) out%ts%basin%rcan = out%ts%basin%rcan/DA
                    where (out%ts%basin%sncan /= out%NO_DATA) out%ts%basin%sncan = out%ts%basin%sncan/DA
                    where (out%ts%basin%sno /= out%NO_DATA) out%ts%basin%sno = out%ts%basin%sno/DA
                    where (out%ts%basin%fsno /= out%NO_DATA) out%ts%basin%fsno = out%ts%basin%fsno/DA
                    where (out%ts%basin%wsno /= out%NO_DATA) out%ts%basin%wsno = out%ts%basin%wsno/DA
                    where (out%ts%basin%zpnd /= out%NO_DATA) out%ts%basin%zpnd = out%ts%basin%zpnd/DA
                    where (out%ts%basin%pndw /= out%NO_DATA) out%ts%basin%pndw = out%ts%basin%pndw/DA
                    where (out%ts%basin%lzs /= out%NO_DATA) out%ts%basin%lzs = out%ts%basin%lzs/DA
                    where (out%ts%basin%dzs /= out%NO_DATA) out%ts%basin%dzs = out%ts%basin%dzs/DA
                end where
                do j = 1, shd%lc%IGND
                    where (DA > 0.0)
                        where (out%ts%basin%thlq(:, j) /= out%NO_DATA)
                            out%ts%basin%thlq(:, j) = out%ts%basin%thlq(:, j)/DA
                        end where
                        where (out%ts%basin%lqws(:, j) /= out%NO_DATA)
                            out%ts%basin%lqws(:, j) = out%ts%basin%lqws(:, j)/DA
                        end where
                        where (out%ts%basin%thic(:, j) /= out%NO_DATA)
                            out%ts%basin%thic(:, j) = out%ts%basin%thic(:, j)/DA
                        end where
                        where (out%ts%basin%fzws(:, j) /= out%NO_DATA)
                            out%ts%basin%fzws(:, j) = out%ts%basin%fzws(:, j)/DA
                        end where
                        where (out%ts%basin%alws(:, j) /= out%NO_DATA)
                            out%ts%basin%alws(:, j) = out%ts%basin%alws(:, j)/DA
                        end where
                    end where
                end do
                where (DA > 0.0)
                    where (out%ts%basin%stg0w /= out%NO_DATA) out%ts%basin%stg0w = out%ts%basin%stg0w/DA
                    where (out%ts%basin%stgw /= out%NO_DATA) out%ts%basin%stgw = out%ts%basin%stgw/DA
                    where (out%ts%basin%dstgw /= out%NO_DATA) out%ts%basin%dstgw = out%ts%basin%dstgw/DA
                end where
            end if
            if (ro%RUNBALEB) then
                where (DA > 0.0)
                    where (out%ts%basin%cmas /= out%NO_DATA) out%ts%basin%cmas = out%ts%basin%cmas/DA
                    where (out%ts%basin%tcan /= out%NO_DATA) out%ts%basin%tcan = out%ts%basin%tcan/DA
                    where (out%ts%basin%tsno /= out%NO_DATA) out%ts%basin%tsno = out%ts%basin%tsno/DA
                    where (out%ts%basin%tpnd /= out%NO_DATA) out%ts%basin%tpnd = out%ts%basin%tpnd/DA
                    where (out%ts%basin%alvs /= out%NO_DATA) out%ts%basin%alvs = out%ts%basin%alvs/DA
                    where (out%ts%basin%alir /= out%NO_DATA) out%ts%basin%alir = out%ts%basin%alir/DA
                    where (out%ts%basin%albt /= out%NO_DATA) out%ts%basin%albt = out%ts%basin%albt/DA
                    where (out%ts%basin%gte /= out%NO_DATA) out%ts%basin%gte = out%ts%basin%gte/DA
                    where (out%ts%basin%fsout /= out%NO_DATA) out%ts%basin%fsout = out%ts%basin%fsout/DA
                    where (out%ts%basin%flout /= out%NO_DATA) out%ts%basin%flout = out%ts%basin%flout/DA
                    where (out%ts%basin%qh /= out%NO_DATA) out%ts%basin%qh = out%ts%basin%qh/DA
                    where (out%ts%basin%qe /= out%NO_DATA) out%ts%basin%qe = out%ts%basin%qe/DA
                    where (out%ts%basin%gzero /= out%NO_DATA) out%ts%basin%gzero = out%ts%basin%gzero/DA
                end where
                do j = 1, shd%lc%IGND
                    where (DA > 0.0)
                        where (out%ts%basin%gflx(:, j) /= out%NO_DATA)
                            out%ts%basin%gflx(:, j) = out%ts%basin%gflx(:, j)/DA
                        end where
                        where (out%ts%basin%tbar(:, j) /= out%NO_DATA)
                            out%ts%basin%tbar(:, j) = out%ts%basin%tbar(:, j)/DA
                        end where
                    end where
                end do
                where (DA > 0.0)
                    where (out%ts%basin%stg0e /= out%NO_DATA) out%ts%basin%stg0e = out%ts%basin%stg0e/DA
                    where (out%ts%basin%stge /= out%NO_DATA) out%ts%basin%stge = out%ts%basin%stge/DA
                    where (out%ts%basin%dstge /= out%NO_DATA) out%ts%basin%dstge = out%ts%basin%dstge/DA
                end where
            end if
            if (ro%RUNCHNL) then
                where (DA > 0.0)
                    where (out%ts%basin%rff /= out%NO_DATA) out%ts%basin%rff = out%ts%basin%rff/DA
                    where (out%ts%basin%rchg /= out%NO_DATA) out%ts%basin%rchg = out%ts%basin%rchg/DA
                    where (out%ts%basin%stgch /= out%NO_DATA) out%ts%basin%stgch = out%ts%basin%stgch/DA
                end where
            end if
        end if

    end subroutine

    !> Description:
    !>  Update the 'dat' vector using the 'val' vector.
    !>  Reset 'dat' if the time-step of the current interval "its" is 1.
    !>  Calculate an average if the function "fn" is 'avg' using the
    !>  number of time-steps elapsed "its".
    !>  Override calculations where 'val' was originally equal to the
    !>  'NO_DATA' value.
    subroutine output_variables_update_values(dat, val, its, fn, cfactorm, cfactora)

        !> Input variables.
        integer, intent(in) :: its
        real, dimension(:), intent(in) :: val
        character(len = *), intent(in) :: fn
        real, intent(in), optional :: cfactorm, cfactora

        !> Input/output variables.
        real, dimension(:) :: dat

        !> Local variables.
        real, dimension(size(val)) :: v

        !> Reset the variable if this is the first time-step in the series.
        if (its == 1) dat = 0.0

        !> Apply transforms to local variable.
        v = val
        if (present(cfactorm)) v = v*cfactorm
        if (present(cfactora)) v = v + cfactora

        !> Apply the 'fn' function.
        !> The default case is to set 'dat' to 'v'.
        select case (fn)
            case ('sum')
                dat = dat + v
            case ('avg')
                dat = (dat*(its - 1) + v)/its
            case ('max')
                dat = max(dat, v)
            case ('min')
                dat = min(dat, v)
            case default
                dat = v
        end select

        !> Assign the 'NO_DATA' value where 'NO_DATA' existed in 'val'.
        where (val == out%NO_DATA) dat = out%NO_DATA

    end subroutine

    !> Description:
    !>  Update output variables of larger time intervals from the 'ts'
    !>  values.
    subroutine output_variables_update_series(shd, series, its)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        integer, intent(in) :: its
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Local variables.
        integer j

        !> Tile-based.
        if (ro%RUNTILE) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (associated(series%tile%fsin)) then
                    call output_variables_update_values(series%tile%fsin, out%ts%tile%fsin, its, 'avg')
                end if
                if (associated(series%tile%flin)) then
                    call output_variables_update_values(series%tile%flin, out%ts%tile%flin, its, 'avg')
                end if
                if (associated(series%tile%ta)) then
                    call output_variables_update_values(series%tile%ta, out%ts%tile%ta, its, 'avg')
                end if
                if (associated(series%tile%qa)) then
                    call output_variables_update_values(series%tile%qa, out%ts%tile%qa, its, 'avg')
                end if
                if (associated(series%tile%pres)) then
                    call output_variables_update_values(series%tile%pres, out%ts%tile%pres, its, 'avg')
                end if
                if (associated(series%tile%uv)) then
                    call output_variables_update_values(series%tile%uv, out%ts%tile%uv, its, 'avg')
                end if
                if (associated(series%tile%pre)) then
                    call output_variables_update_values(series%tile%pre, out%ts%tile%pre, its, 'avg')
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(series%tile%prec)) then
                    call output_variables_update_values(series%tile%prec, out%ts%tile%prec, its, 'sum')
                end if
                if (associated(series%tile%evap)) then
                    call output_variables_update_values(series%tile%evap, out%ts%tile%evap, its, 'sum')
                end if
                if (associated(series%tile%pevp)) then
                    call output_variables_update_values(series%tile%pevp, out%ts%tile%pevp, its, 'sum')
                end if
                if (associated(series%tile%evpb)) then
                    call output_variables_update_values(series%tile%evpb, out%ts%tile%evpb, its, 'avg')
                end if
                if (associated(series%tile%arrd)) then
                    call output_variables_update_values(series%tile%arrd, out%ts%tile%arrd, its, 'avg')
                end if
                if (associated(series%tile%gro)) then
                    call output_variables_update_values(series%tile%gro, out%ts%tile%gro, its, 'avg')
                end if
                if (associated(series%tile%rof)) then
                    call output_variables_update_values(series%tile%rof, out%ts%tile%rof, its, 'sum')
                end if
                if (associated(series%tile%rofo)) then
                    call output_variables_update_values(series%tile%rofo, out%ts%tile%rofo, its, 'sum')
                end if
                if (associated(series%tile%rofs)) then
                    call output_variables_update_values(series%tile%rofs, out%ts%tile%rofs, its, 'sum')
                end if
                if (associated(series%tile%rofb)) then
                    call output_variables_update_values(series%tile%rofb, out%ts%tile%rofb, its, 'sum')
                end if
                if (associated(series%tile%rcan)) then
                    call output_variables_update_values(series%tile%rcan, out%ts%tile%rcan, its, 'avg')
                end if
                if (associated(series%tile%sncan)) then
                    call output_variables_update_values(series%tile%sncan, out%ts%tile%sncan, its, 'avg')
                end if
                if (associated(series%tile%sno)) then
                    call output_variables_update_values(series%tile%sno, out%ts%tile%sno, its, 'avg')
                end if
                if (associated(series%tile%fsno)) then
                    call output_variables_update_values(series%tile%fsno, out%ts%tile%fsno, its, 'avg')
                end if
                if (associated(series%tile%wsno)) then
                    call output_variables_update_values(series%tile%wsno, out%ts%tile%wsno, its, 'avg')
                end if
                if (associated(series%tile%zpnd)) then
                    call output_variables_update_values(series%tile%zpnd, out%ts%tile%zpnd, its, 'avg')
                end if
                if (associated(series%tile%pndw)) then
                    call output_variables_update_values(series%tile%pndw, out%ts%tile%pndw, its, 'avg')
                end if
                if (associated(series%tile%lzs)) then
                    call output_variables_update_values(series%tile%lzs, out%ts%tile%lzs, its, 'avg')
                end if
                if (associated(series%tile%dzs)) then
                    call output_variables_update_values(series%tile%dzs, out%ts%tile%dzs, its, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%tile%thlq)) then
                        call output_variables_update_values(series%tile%thlq(:, j), out%ts%tile%thlq(:, j), its, 'avg')
                    end if
                    if (associated(series%tile%lqws)) then
                        call output_variables_update_values(series%tile%lqws(:, j), out%ts%tile%lqws(:, j), its, 'avg')
                    end if
                    if (associated(series%tile%thic)) then
                        call output_variables_update_values(series%tile%thic(:, j), out%ts%tile%thic(:, j), its, 'avg')
                    end if
                    if (associated(series%tile%fzws)) then
                        call output_variables_update_values(series%tile%fzws(:, j), out%ts%tile%fzws(:, j), its, 'avg')
                    end if
                    if (associated(series%tile%alws)) then
                        call output_variables_update_values(series%tile%alws(:, j), out%ts%tile%alws(:, j), its, 'avg')
                    end if
                end do
                if (associated(series%tile%stgw)) then
                    if (associated(series%tile%stg0w) .and. its == 1) then
                        call output_variables_update_values(series%tile%stg0w, series%tile%stgw, its, 'val')
                    end if
                    call output_variables_update_values(series%tile%stgw, out%ts%tile%stgw, its, 'avg')
                    if (associated(series%tile%dstgw) .and. associated(series%tile%stg0w)) then
                        where (series%tile%stgw /= out%NO_DATA) series%tile%dstgw = series%tile%stgw - series%tile%stg0w
                    end if
                end if
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(series%tile%cmas)) then
                    call output_variables_update_values(series%tile%cmas, out%ts%tile%cmas, its, 'sum')
                end if
                if (associated(series%tile%tcan)) then
                    call output_variables_update_values(series%tile%tcan, out%ts%tile%tcan, its, 'avg')
                end if
                if (associated(series%tile%tsno)) then
                    call output_variables_update_values(series%tile%tsno, out%ts%tile%tsno, its, 'avg')
                end if
                if (associated(series%tile%tpnd)) then
                    call output_variables_update_values(series%tile%tpnd, out%ts%tile%tpnd, its, 'avg')
                end if
                if (associated(series%tile%albt)) then
                    call output_variables_update_values(series%tile%albt, out%ts%tile%albt, its, 'avg')
                end if
                if (associated(series%tile%alvs)) then
                    call output_variables_update_values(series%tile%alvs, out%ts%tile%alvs, its, 'avg')
                end if
                if (associated(series%tile%alir)) then
                    call output_variables_update_values(series%tile%alir, out%ts%tile%alir, its, 'avg')
                end if
                if (associated(series%tile%fsout)) then
                    call output_variables_update_values(series%tile%fsout, out%ts%tile%fsout, its, 'avg')
                end if
                if (associated(series%tile%gte)) then
                    call output_variables_update_values(series%tile%gte, out%ts%tile%gte, its, 'avg')
                end if
                if (associated(series%tile%flout)) then
                    call output_variables_update_values(series%tile%flout, out%ts%tile%flout, its, 'avg')
                end if
                if (associated(series%tile%qh)) then
                    call output_variables_update_values(series%tile%qh, out%ts%tile%qh, its, 'avg')
                end if
                if (associated(series%tile%qe)) then
                    call output_variables_update_values(series%tile%qe, out%ts%tile%qe, its, 'avg')
                end if
                if (associated(series%tile%gzero)) then
                    call output_variables_update_values(series%tile%gzero, out%ts%tile%gzero, its, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%tile%gflx)) then
                        call output_variables_update_values(series%tile%gflx(:, j), out%ts%tile%gflx(:, j), its, 'avg')
                    end if
                    if (associated(series%tile%tbar)) then
                        call output_variables_update_values(series%tile%tbar(:, j), out%ts%tile%tbar(:, j), its, 'avg')
                    end if
                end do
                if (associated(series%tile%stge)) then
                    if (associated(series%tile%stg0e) .and. its == 1) then
                        call output_variables_update_values(series%tile%stg0e, series%tile%stge, its, 'val')
                    end if
                    call output_variables_update_values(series%tile%stge, out%ts%tile%stge, its, 'avg')
                    if (associated(series%tile%dstge) .and. associated(series%tile%stg0e)) then
                        where (series%tile%stge /= out%NO_DATA) series%tile%dstge = series%tile%stge - series%tile%stg0e
                    end if
                end if
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                if (associated(series%grid%fsin)) then
                    call output_variables_update_values(series%grid%fsin, out%ts%grid%fsin, its, 'avg')
                end if
                if (associated(series%grid%flin)) then
                    call output_variables_update_values(series%grid%flin, out%ts%grid%flin, its, 'avg')
                end if
                if (associated(series%grid%ta)) then
                    call output_variables_update_values(series%grid%ta, out%ts%grid%ta, its, 'avg')
                end if
                if (associated(series%grid%qa)) then
                    call output_variables_update_values(series%grid%qa, out%ts%grid%qa, its, 'avg')
                end if
                if (associated(series%grid%pres)) then
                    call output_variables_update_values(series%grid%pres, out%ts%grid%pres, its, 'avg')
                end if
                if (associated(series%grid%uv)) then
                    call output_variables_update_values(series%grid%uv, out%ts%grid%uv, its, 'avg')
                end if
                if (associated(series%grid%pre)) then
                    call output_variables_update_values(series%grid%pre, out%ts%grid%pre, its, 'avg')
                end if
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                if (associated(series%grid%prec)) then
                    call output_variables_update_values(series%grid%prec, out%ts%grid%prec, its, 'sum')
                end if
                if (associated(series%grid%evap)) then
                    call output_variables_update_values(series%grid%evap, out%ts%grid%evap, its, 'sum')
                end if
                if (associated(series%grid%pevp)) then
                    call output_variables_update_values(series%grid%pevp, out%ts%grid%pevp, its, 'sum')
                end if
                if (associated(series%grid%evpb)) then
                    call output_variables_update_values(series%grid%evpb, out%ts%grid%evpb, its, 'avg')
                end if
                if (associated(series%grid%arrd)) then
                    call output_variables_update_values(series%grid%arrd, out%ts%grid%arrd, its, 'avg')
                end if
                if (associated(series%grid%gro)) then
                    call output_variables_update_values(series%grid%gro, out%ts%grid%gro, its, 'avg')
                end if
                if (associated(series%grid%rof)) then
                    call output_variables_update_values(series%grid%rof, out%ts%grid%rof, its, 'sum')
                end if
                if (associated(series%grid%rofo)) then
                    call output_variables_update_values(series%grid%rofo, out%ts%grid%rofo, its, 'sum')
                end if
                if (associated(series%grid%rofs)) then
                    call output_variables_update_values(series%grid%rofs, out%ts%grid%rofs, its, 'sum')
                end if
                if (associated(series%grid%rofb)) then
                    call output_variables_update_values(series%grid%rofb, out%ts%grid%rofb, its, 'sum')
                end if
                if (associated(series%grid%rcan)) then
                    call output_variables_update_values(series%grid%rcan, out%ts%grid%rcan, its, 'avg')
                end if
                if (associated(series%grid%sncan)) then
                    call output_variables_update_values(series%grid%sncan, out%ts%grid%sncan, its, 'avg')
                end if
                if (associated(series%grid%sno)) then
                    call output_variables_update_values(series%grid%sno, out%ts%grid%sno, its, 'avg')
                end if
                if (associated(series%grid%fsno)) then
                    call output_variables_update_values(series%grid%fsno, out%ts%grid%fsno, its, 'avg')
                end if
                if (associated(series%grid%wsno)) then
                    call output_variables_update_values(series%grid%wsno, out%ts%grid%wsno, its, 'avg')
                end if
                if (associated(series%grid%zpnd)) then
                    call output_variables_update_values(series%grid%zpnd, out%ts%grid%zpnd, its, 'avg')
                end if
                if (associated(series%grid%pndw)) then
                    call output_variables_update_values(series%grid%pndw, out%ts%grid%pndw, its, 'avg')
                end if
                if (associated(series%grid%lzs)) then
                    call output_variables_update_values(series%grid%lzs, out%ts%grid%lzs, its, 'avg')
                end if
                if (associated(series%grid%dzs)) then
                    call output_variables_update_values(series%grid%dzs, out%ts%grid%dzs, its, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%grid%thlq)) then
                        call output_variables_update_values(series%grid%thlq(:, j), out%ts%grid%thlq(:, j), its, 'avg')
                    end if
                    if (associated(series%grid%lqws)) then
                        call output_variables_update_values(series%grid%lqws(:, j), out%ts%grid%lqws(:, j), its, 'avg')
                    end if
                    if (associated(series%grid%thic)) then
                        call output_variables_update_values(series%grid%thic(:, j), out%ts%grid%thic(:, j), its, 'avg')
                    end if
                    if (associated(series%grid%fzws)) then
                        call output_variables_update_values(series%grid%fzws(:, j), out%ts%grid%fzws(:, j), its, 'avg')
                    end if
                    if (associated(series%grid%alws)) then
                        call output_variables_update_values(series%grid%alws(:, j), out%ts%grid%alws(:, j), its, 'avg')
                    end if
                end do
                if (associated(series%grid%stgw)) then
                    if (associated(series%grid%stg0w) .and. its == 1) then
                        call output_variables_update_values(series%grid%stg0w, series%grid%stgw, its, 'val')
                    end if
                    call output_variables_update_values(series%grid%stgw, out%ts%grid%stgw, its, 'avg')
                    if (associated(series%grid%dstgw) .and. associated(series%grid%stg0w)) then
                        where (series%grid%stgw /= out%NO_DATA) series%grid%dstgw = series%grid%stgw - series%grid%stg0w
                    end if
                end if
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                if (associated(series%grid%cmas)) then
                    call output_variables_update_values(series%grid%cmas, out%ts%grid%cmas, its, 'sum')
                end if
                if (associated(series%grid%tcan)) then
                    call output_variables_update_values(series%grid%tcan, out%ts%grid%tcan, its, 'avg')
                end if
                if (associated(series%grid%tsno)) then
                    call output_variables_update_values(series%grid%tsno, out%ts%grid%tsno, its, 'avg')
                end if
                if (associated(series%grid%tpnd)) then
                    call output_variables_update_values(series%grid%tpnd, out%ts%grid%tpnd, its, 'avg')
                end if
                if (associated(series%grid%albt)) then
                    call output_variables_update_values(series%grid%albt, out%ts%grid%albt, its, 'avg')
                end if
                if (associated(series%grid%alvs)) then
                    call output_variables_update_values(series%grid%alvs, out%ts%grid%alvs, its, 'avg')
                end if
                if (associated(series%grid%alir)) then
                    call output_variables_update_values(series%grid%alir, out%ts%grid%alir, its, 'avg')
                end if
                if (associated(series%grid%fsout)) then
                    call output_variables_update_values(series%grid%fsout, out%ts%grid%fsout, its, 'avg')
                end if
                if (associated(series%grid%gte)) then
                    call output_variables_update_values(series%grid%gte, out%ts%grid%gte, its, 'avg')
                end if
                if (associated(series%grid%flout)) then
                    call output_variables_update_values(series%grid%flout, out%ts%grid%flout, its, 'avg')
                end if
                if (associated(series%grid%qh)) then
                    call output_variables_update_values(series%grid%qh, out%ts%grid%qh, its, 'avg')
                end if
                if (associated(series%grid%qe)) then
                    call output_variables_update_values(series%grid%qe, out%ts%grid%qe, its, 'avg')
                end if
                if (associated(series%grid%gzero)) then
                    call output_variables_update_values(series%grid%gzero, out%ts%grid%gzero, its, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%grid%gflx)) then
                        call output_variables_update_values(series%grid%gflx(:, j), out%ts%grid%gflx(:, j), its, 'avg')
                    end if
                    if (associated(series%grid%tbar)) then
                        call output_variables_update_values(series%grid%tbar(:, j), out%ts%grid%tbar(:, j), its, 'avg')
                    end if
                end do
                if (associated(series%grid%stge)) then
                    if (associated(series%grid%stg0e) .and. its == 1) then
                        call output_variables_update_values(series%grid%stg0e, series%grid%stge, its, 'val')
                    end if
                    call output_variables_update_values(series%grid%stge, out%ts%grid%stge, its, 'avg')
                    if (associated(series%grid%dstge) .and. associated(series%grid%stg0e)) then
                        where (series%grid%stge /= out%NO_DATA) series%grid%dstge = series%grid%stge - series%grid%stg0e
                    end if
                end if
            end if

            !> Channels and routing.
            if (ro%RUNCHNL) then
                if (associated(series%grid%rff)) then
                    call output_variables_update_values(series%grid%rff, out%ts%grid%rff, its, 'sum')
                end if
                if (associated(series%grid%rchg)) then
                    call output_variables_update_values(series%grid%rchg, out%ts%grid%rchg, its, 'sum')
                end if
                if (associated(series%grid%qi)) then
                    call output_variables_update_values(series%grid%qi, out%ts%grid%qi, its, 'avg')
                end if
                if (associated(series%grid%stgch)) then
                    call output_variables_update_values(series%grid%stgch, out%ts%grid%stgch, its, 'avg')
                end if
                if (associated(series%grid%qo)) then
                    call output_variables_update_values(series%grid%qo, out%ts%grid%qo, its, 'avg')
                end if
                if (associated(series%grid%zlvl)) then
                    call output_variables_update_values(series%grid%zlvl, out%ts%grid%zlvl, its, 'avg')
                end if
            end if

            !> DA averaged.
            if (ro%RUNCLIM) then
                if (associated(series%basin%fsin)) then
                    call output_variables_update_values(series%basin%fsin, out%ts%basin%fsin, its, 'avg')
                end if
                if (associated(series%basin%flin)) then
                    call output_variables_update_values(series%basin%flin, out%ts%basin%flin, its, 'avg')
                end if
                if (associated(series%basin%ta)) then
                    call output_variables_update_values(series%basin%ta, out%ts%basin%ta, its, 'avg')
                end if
                if (associated(series%basin%qa)) then
                    call output_variables_update_values(series%basin%qa, out%ts%basin%qa, its, 'avg')
                end if
                if (associated(series%basin%pres)) then
                    call output_variables_update_values(series%basin%pres, out%ts%basin%pres, its, 'avg')
                end if
                if (associated(series%basin%uv)) then
                    call output_variables_update_values(series%basin%uv, out%ts%basin%uv, its, 'avg')
                end if
                if (associated(series%basin%pre)) then
                    call output_variables_update_values(series%basin%pre, out%ts%basin%pre, its, 'avg')
                end if
            end if
            if (ro%RUNBALWB) then
                if (associated(series%basin%prec)) then
                    call output_variables_update_values(series%basin%prec, out%ts%basin%prec, its, 'sum')
                end if
                if (associated(series%basin%evap)) then
                    call output_variables_update_values(series%basin%evap, out%ts%basin%evap, its, 'sum')
                end if
                if (associated(series%basin%pevp)) then
                    call output_variables_update_values(series%basin%pevp, out%ts%basin%pevp, its, 'sum')
                end if
                if (associated(series%basin%evpb)) then
                    call output_variables_update_values(series%basin%evpb, out%ts%basin%evpb, its, 'avg')
                end if
                if (associated(series%basin%arrd)) then
                    call output_variables_update_values(series%basin%arrd, out%ts%basin%arrd, its, 'avg')
                end if
                if (associated(series%basin%gro)) then
                    call output_variables_update_values(series%basin%gro, out%ts%basin%gro, its, 'avg')
                end if
                if (associated(series%basin%rof)) then
                    call output_variables_update_values(series%basin%rof, out%ts%basin%rof, its, 'sum')
                end if
                if (associated(series%basin%rofo)) then
                    call output_variables_update_values(series%basin%rofo, out%ts%basin%rofo, its, 'sum')
                end if
                if (associated(series%basin%rofs)) then
                    call output_variables_update_values(series%basin%rofs, out%ts%basin%rofs, its, 'sum')
                end if
                if (associated(series%basin%rofb)) then
                    call output_variables_update_values(series%basin%rofb, out%ts%basin%rofb, its, 'sum')
                end if
                if (associated(series%basin%rcan)) then
                    call output_variables_update_values(series%basin%rcan, out%ts%basin%rcan, its, 'avg')
                end if
                if (associated(series%basin%sncan)) then
                    call output_variables_update_values(series%basin%sncan, out%ts%basin%sncan, its, 'avg')
                end if
                if (associated(series%basin%sno)) then
                    call output_variables_update_values(series%basin%sno, out%ts%basin%sno, its, 'avg')
                end if
                if (associated(series%basin%fsno)) then
                    call output_variables_update_values(series%basin%fsno, out%ts%basin%fsno, its, 'avg')
                end if
                if (associated(series%basin%wsno)) then
                    call output_variables_update_values(series%basin%wsno, out%ts%basin%wsno, its, 'avg')
                end if
                if (associated(series%basin%zpnd)) then
                    call output_variables_update_values(series%basin%zpnd, out%ts%basin%zpnd, its, 'avg')
                end if
                if (associated(series%basin%pndw)) then
                    call output_variables_update_values(series%basin%pndw, out%ts%basin%pndw, its, 'avg')
                end if
                if (associated(series%basin%lzs)) then
                    call output_variables_update_values(series%basin%lzs, out%ts%basin%lzs, its, 'avg')
                end if
                if (associated(series%basin%dzs)) then
                    call output_variables_update_values(series%basin%dzs, out%ts%basin%dzs, its, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%basin%thlq)) then
                        call output_variables_update_values(series%basin%thlq(:, j), out%ts%basin%thlq(:, j), its, 'avg')
                    end if
                    if (associated(series%basin%lqws)) then
                        call output_variables_update_values(series%basin%lqws(:, j), out%ts%basin%lqws(:, j), its, 'avg')
                    end if
                    if (associated(series%basin%thic)) then
                        call output_variables_update_values(series%basin%thic(:, j), out%ts%basin%thic(:, j), its, 'avg')
                    end if
                    if (associated(series%basin%fzws)) then
                        call output_variables_update_values(series%basin%fzws(:, j), out%ts%basin%fzws(:, j), its, 'avg')
                    end if
                    if (associated(series%basin%alws)) then
                        call output_variables_update_values(series%basin%alws(:, j), out%ts%basin%alws(:, j), its, 'avg')
                    end if
                end do
                if (associated(series%basin%stgw)) then
                    if (associated(series%basin%stg0w) .and. its == 1) then
                        call output_variables_update_values(series%basin%stg0w, series%basin%stgw, its, 'val')
                    end if
                    call output_variables_update_values(series%basin%stgw, out%ts%basin%stgw, its, 'avg')
                    if (associated(series%basin%dstgw) .and. associated(series%basin%stg0w)) then
                        where (series%basin%stgw /= out%NO_DATA) series%basin%dstgw = series%basin%stgw - series%basin%stg0w
                    end if
                end if
            end if
            if (ro%RUNBALEB) then
                if (associated(series%basin%cmas)) then
                    call output_variables_update_values(series%basin%cmas, out%ts%basin%cmas, its, 'sum')
                end if
                if (associated(series%basin%tcan)) then
                    call output_variables_update_values(series%basin%tcan, out%ts%basin%tcan, its, 'avg')
                end if
                if (associated(series%basin%tsno)) then
                    call output_variables_update_values(series%basin%tsno, out%ts%basin%tsno, its, 'avg')
                end if
                if (associated(series%basin%tpnd)) then
                    call output_variables_update_values(series%basin%tpnd, out%ts%basin%tpnd, its, 'avg')
                end if
                if (associated(series%basin%albt)) then
                    call output_variables_update_values(series%basin%albt, out%ts%basin%albt, its, 'avg')
                end if
                if (associated(series%basin%alvs)) then
                    call output_variables_update_values(series%basin%alvs, out%ts%basin%alvs, its, 'avg')
                end if
                if (associated(series%basin%alir)) then
                    call output_variables_update_values(series%basin%alir, out%ts%basin%alir, its, 'avg')
                end if
                if (associated(series%basin%fsout)) then
                    call output_variables_update_values(series%basin%fsout, out%ts%basin%fsout, its, 'avg')
                end if
                if (associated(series%basin%gte)) then
                    call output_variables_update_values(series%basin%gte, out%ts%basin%gte, its, 'avg')
                end if
                if (associated(series%basin%flout)) then
                    call output_variables_update_values(series%basin%flout, out%ts%basin%flout, its, 'avg')
                end if
                if (associated(series%basin%qh)) then
                    call output_variables_update_values(series%basin%qh, out%ts%basin%qh, its, 'avg')
                end if
                if (associated(series%basin%qe)) then
                    call output_variables_update_values(series%basin%qe, out%ts%basin%qe, its, 'avg')
                end if
                if (associated(series%basin%gzero)) then
                    call output_variables_update_values(series%basin%gzero, out%ts%basin%gzero, its, 'avg')
                end if
                do j = 1, shd%lc%IGND
                    if (associated(series%basin%gflx)) then
                        call output_variables_update_values(series%basin%gflx(:, j), out%ts%basin%gflx(:, j), its, 'avg')
                    end if
                    if (associated(series%basin%tbar)) then
                        call output_variables_update_values(series%basin%tbar(:, j), out%ts%basin%tbar(:, j), its, 'avg')
                    end if
                end do
                if (associated(series%basin%stge)) then
                    if (associated(series%basin%stg0e) .and. its == 1) then
                        call output_variables_update_values(series%basin%stg0e, series%basin%stge, its, 'val')
                    end if
                    call output_variables_update_values(series%basin%stge, out%ts%basin%stge, its, 'avg')
                    if (associated(series%basin%dstge) .and. associated(series%basin%stg0e)) then
                        where (series%basin%stge /= out%NO_DATA) series%basin%dstge = series%basin%stge - series%basin%stg0e
                    end if
                end if
            end if
            if (ro%RUNCHNL) then
                if (associated(series%basin%rff)) then
                    call output_variables_update_values(series%basin%rff, out%ts%basin%rff, its, 'sum')
                end if
                if (associated(series%basin%rchg)) then
                    call output_variables_update_values(series%basin%rchg, out%ts%basin%rchg, its, 'sum')
                end if
                if (associated(series%basin%qi)) then
                    call output_variables_update_values(series%basin%qi, out%ts%basin%qi, its, 'avg')
                end if
                if (associated(series%basin%stgch)) then
                    call output_variables_update_values(series%basin%stgch, out%ts%basin%stgch, its, 'avg')
                end if
                if (associated(series%basin%qo)) then
                    call output_variables_update_values(series%basin%qo, out%ts%basin%qo, its, 'avg')
                end if
                if (associated(series%basin%zlvl)) then
                    call output_variables_update_values(series%basin%zlvl, out%ts%basin%zlvl, its, 'avg')
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

        !> Update 'ts' values.
        call output_variables_update_ts(shd, cm)

        !> Totals (e.g., accumulated).
        call output_variables_update_series(shd, out%tot, ic%ts_count)

        !> Yearly.
        call output_variables_update_series(shd, out%y, ic%ts_yearly)

        !> Monthly.
        call output_variables_update_series(shd, out%m, ic%ts_monthly)

        !> Daily.
        call output_variables_update_series(shd, out%d, ic%ts_daily)

        !> Hourly.
        call output_variables_update_series(shd, out%h, ic%ts_hourly)

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
                series%tile%fsin = out%NO_DATA
                series%tile%ifsin = 0.0
                series%tile%flin = out%NO_DATA
                series%tile%ta = out%NO_DATA
                series%tile%qa = out%NO_DATA
                series%tile%pres = out%NO_DATA
                series%tile%uv = out%NO_DATA
                series%tile%pre = out%NO_DATA
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
                series%tile%isno = 0.0
                series%tile%fsno = out%NO_DATA
                series%tile%wsno = out%NO_DATA
                series%tile%zpnd = out%NO_DATA
                series%tile%ipnd = 0.0
                series%tile%pndw = out%NO_DATA
                series%tile%lzs = out%NO_DATA
                series%tile%dzs = out%NO_DATA
                series%tile%thlq = out%NO_DATA
                series%tile%lqws = out%NO_DATA
                series%tile%thic = out%NO_DATA
                series%tile%fzws = out%NO_DATA
                series%tile%alws = out%NO_DATA
                series%tile%stg0w = series%tile%stgw
                series%tile%stgw = 0.0
                series%tile%dstgw = 0.0
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                series%tile%cmas = out%NO_DATA
                series%tile%tcan = out%NO_DATA
                series%tile%ican = 0.0
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
                series%tile%gflx = out%NO_DATA
                series%tile%tbar = out%NO_DATA
                series%tile%stg0e = series%tile%stge
                series%tile%stge = 0.0
                series%tile%dstge = 0.0
            end if
        end if

        !> Grid-based.
        if (ro%RUNGRID) then

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                series%grid%pre = out%NO_DATA
                series%grid%fsin = out%NO_DATA
                series%grid%ifsin = 0.0
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
                series%grid%isno = 0.0
                series%grid%fsno = out%NO_DATA
                series%grid%wsno = out%NO_DATA
                series%grid%zpnd = out%NO_DATA
                series%grid%ipnd = 0.0
                series%grid%pndw = out%NO_DATA
                series%grid%lzs = out%NO_DATA
                series%grid%dzs = out%NO_DATA
                series%grid%thlq = out%NO_DATA
                series%grid%lqws = out%NO_DATA
                series%grid%thic = out%NO_DATA
                series%grid%fzws = out%NO_DATA
                series%grid%alws = out%NO_DATA
                series%grid%stg0w = series%grid%stgw
                series%grid%stgw = 0.0
                series%grid%dstgw = 0.0
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                series%grid%cmas = out%NO_DATA
                series%grid%tcan = out%NO_DATA
                series%grid%ican = 0.0
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
                series%grid%gflx = out%NO_DATA
                series%grid%tbar = out%NO_DATA
                series%grid%stg0e = series%grid%stge
                series%grid%stge = 0.0
                series%grid%dstge = 0.0
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

            !> Meteorological forcing.
            if (ro%RUNCLIM) then
                series%basin%pre = out%NO_DATA
                series%basin%fsin = out%NO_DATA
                series%basin%ifsin = 0.0
                series%basin%flin = out%NO_DATA
                series%basin%ta = out%NO_DATA
                series%basin%qa = out%NO_DATA
                series%basin%pres = out%NO_DATA
                series%basin%uv = out%NO_DATA
            end if

            !> Water balance.
            if (ro%RUNBALWB) then
                series%basin%prec = out%NO_DATA
                series%basin%evap = out%NO_DATA
                series%basin%pevp = out%NO_DATA
                series%basin%evpb = out%NO_DATA
                series%basin%arrd = out%NO_DATA
                series%basin%gro = out%NO_DATA
                series%basin%rof = out%NO_DATA
                series%basin%rofo = out%NO_DATA
                series%basin%rofs = out%NO_DATA
                series%basin%rofb = out%NO_DATA
                series%basin%rcan = out%NO_DATA
                series%basin%sncan = out%NO_DATA
                series%basin%sno = out%NO_DATA
                series%basin%isno = 0.0
                series%basin%fsno = out%NO_DATA
                series%basin%wsno = out%NO_DATA
                series%basin%zpnd = out%NO_DATA
                series%basin%ipnd = 0.0
                series%basin%pndw = out%NO_DATA
                series%basin%lzs = out%NO_DATA
                series%basin%dzs = out%NO_DATA
                series%basin%thlq = out%NO_DATA
                series%basin%lqws = out%NO_DATA
                series%basin%thic = out%NO_DATA
                series%basin%fzws = out%NO_DATA
                series%basin%alws = out%NO_DATA
                series%basin%stg0w = series%basin%stgw
                series%basin%stgw = 0.0
                series%basin%dstgw = 0.0
            end if

            !> Energy balance.
            if (ro%RUNBALEB) then
                series%basin%cmas = out%NO_DATA
                series%basin%tcan = out%NO_DATA
                series%basin%ican = 0.0
                series%basin%tsno = out%NO_DATA
                series%basin%tpnd = out%NO_DATA
                series%basin%albt = out%NO_DATA
                series%basin%alvs = out%NO_DATA
                series%basin%alir = out%NO_DATA
                series%basin%fsout = out%NO_DATA
                series%basin%gte = out%NO_DATA
                series%basin%flout = out%NO_DATA
                series%basin%qh = out%NO_DATA
                series%basin%qe = out%NO_DATA
                series%basin%gzero = out%NO_DATA
                series%basin%gflx = out%NO_DATA
                series%basin%tbar = out%NO_DATA
                series%basin%stg0e = series%basin%stge
                series%basin%stge = 0.0
                series%basin%dstge = 0.0
            end if

            !> Channels and routing.
            if (ro%RUNCHNL) then
                series%basin%rff = out%NO_DATA
                series%basin%rchg = out%NO_DATA
                series%basin%qi = out%NO_DATA
                series%basin%stgch = out%NO_DATA
                series%basin%qo = out%NO_DATA
                series%basin%zlvl = out%NO_DATA
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
