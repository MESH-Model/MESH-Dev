module output_variables

    use variable_names
    use model_variables

    implicit none

    !> Description:
    !>  Container for output variables.
    type output_fields

        !> Meteorology/climatology variables.
        real, dimension(:), pointer :: ifsin => null()
        real, dimension(:), pointer :: fsin => null()
        real, dimension(:), pointer :: fsdr => null()
        real, dimension(:), pointer :: fsdff => null()
        real, dimension(:), pointer :: fsout => null()
        real, dimension(:), pointer :: flin => null()
        real, dimension(:), pointer :: flout => null()
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
        real, dimension(:), pointer :: prec => null()
        real, dimension(:), pointer :: precrn => null()
        real, dimension(:), pointer :: precsno => null()

        !> Canopy variables.
        real, dimension(:), pointer :: ican => null()
        real, dimension(:), pointer :: rcan => null()
        real, dimension(:), pointer :: sncan => null()
        real, dimension(:), pointer :: cmas => null()
        real, dimension(:), pointer :: tcan => null()
        real, dimension(:), pointer :: gro => null()

        !> Snow variables.
        real, dimension(:), pointer :: isno => null()
        real, dimension(:), pointer :: sno => null()
        real, dimension(:), pointer :: rhosno => null()
        real, dimension(:), pointer :: zsno => null()
        real, dimension(:), pointer :: fsno => null()
        real, dimension(:), pointer :: wsno => null()
        real, dimension(:), pointer :: tsno => null()
        real, dimension(:), pointer :: rofsno => null()

        !> Surface variables.
        real, dimension(:), pointer :: albt => null()
        real, dimension(:), pointer :: alvs => null()
        real, dimension(:), pointer :: alir => null()
        real, dimension(:), pointer :: gte => null()
        real, dimension(:), pointer :: ipnd => null()
        real, dimension(:), pointer :: zpnd => null()
        real, dimension(:), pointer :: pndw => null()
        real, dimension(:), pointer :: tpnd => null()
        real, dimension(:), pointer :: pevp => null()
        real, dimension(:), pointer :: evap => null()
        real, dimension(:), pointer :: evpb => null()
        real, dimension(:), pointer :: arrd => null()
        real, dimension(:), pointer :: rofo => null()
        real, dimension(:), pointer :: qe => null()
        real, dimension(:), pointer :: qh => null()
        real, dimension(:), pointer :: gzero => null()

        !> Subsurface/soil variables.
        real, dimension(:, :), pointer :: thlq => null()
        real, dimension(:, :), pointer :: thic => null()
        real, dimension(:, :), pointer :: lqws => null()
        real, dimension(:, :), pointer :: fzws => null()
        real, dimension(:, :), pointer :: alws => null()
        real, dimension(:, :), pointer :: tbar => null()
        real, dimension(:, :), pointer :: gflx => null()
        real, dimension(:, :), pointer :: rofs => null()
        real, dimension(:), pointer :: rofb => null()

        !> Groundwater/lower zone storage variables.
        real, dimension(:), pointer :: rchg => null()
        real, dimension(:), pointer :: lzs => null()
        real, dimension(:), pointer :: dzs => null()

        !> Diagnostic variables.
        real, dimension(:), pointer :: stg0e => null()
        real, dimension(:), pointer :: stge => null()
        real, dimension(:), pointer :: dstge => null()
        real, dimension(:), pointer :: stg0w => null()
        real, dimension(:), pointer :: stgw => null()
        real, dimension(:), pointer :: dstgw => null()

        !> Routing variables.
        real, dimension(:), pointer :: rff => null()
        real, dimension(:), pointer :: rof => null()
        real, dimension(:), pointer :: qi => null()
        real, dimension(:), pointer :: qo => null()
        real, dimension(:), pointer :: stgch => null()
        real, dimension(:), pointer :: zlvl => null()

        !> Indices.
        integer :: n1 = 0
        integer :: n2 = 0

        !> Association to model variable group.
        type(model_variables_fields), pointer :: vs => null()

        !> Association to 'ts' base group.
        type(output_fields), pointer :: ts => null()
    end type

    !> Description:
    !>  Container for a series of output variables (e.g., at various time intervals).
    !>
    !> Variables:
    !*  tile: Elemental computational unit 1:NML.
    !*  grid: Grid combined from contributing GRUs (by ACLASS) 1:NA.
    !*  basin: Same as grid but accumulated according to drainage direction 1:NA.
    type output_series
        type(output_fields), pointer :: tile, grid, basin
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
            y_tile => null(), m_tile => null(), d_tile => null(), h_tile => null(), ts_tile => null(), &
            y_grid => null(), m_grid => null(), d_grid => null(), h_grid => null(), ts_grid => null()
    end type

    !> Description:
    !>  Interface for 'output_variables_allocate'.
    interface output_variables_allocate
        module procedure output_variables_allocate_1d_pntr
        module procedure output_variables_allocate_2d_pntr
    end interface

    interface output_variables_activate
        module procedure output_variables_activate_pntr
        module procedure output_variables_activate_vnames
    end interface

    contains

    !> Description:
    !>  Allocate and initialize data variable. 'field' is allocated to
    !>  dimension 'n1'.
    subroutine output_variables_allocate_1d_pntr(field, n1, pntr)

        !> Input/output variables.
        integer, intent(in) :: n1
        real, dimension(:), pointer :: field
        real, dimension(:), optional, pointer :: pntr

        !> Allocate and initialize variable
        if (.not. associated(field)) then
            allocate(field(n1))
            field = out%NO_DATA
        end if

        !> Associate the pointer.
        if (present(pntr)) pntr => field

    end subroutine

    !> Description:
    !>  Allocate and initialize data variable. 'field' is allocated to
    !>  dimension 'n1' and 'n2'.
    subroutine output_variables_allocate_2d_pntr(field, n1, n2, pntr, ig)

        !> Input variables.
        integer, intent(in) :: n1, n2
        integer, intent(in), optional :: ig

        !> Input/output variables.
        real, dimension(:, :), pointer :: field
        real, dimension(:), optional, pointer :: pntr

        !> Allocate and initialize variable
        if (.not. associated(field)) then
            allocate(field(n1, n2))
            field = out%NO_DATA
        end if

        !> Associate the pointer.
        if (present(pntr) .and. present(ig)) pntr => field(:, ig)

    end subroutine

    !> Description:
    !>  Allocate the output variable and associate it to 'pntr'.
    recursive subroutine output_variables_activate_pntr(fields, vname, pntr, ig)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input variables.
        type(output_fields), intent(in) :: fields
        character(len = *), intent(in) :: vname
        integer, intent(in), optional :: ig

        !> Input/output variables.
        real, dimension(:), optional, pointer :: pntr

        !> Local variables.
        logical t
        integer n1, n2, z

        !> Indices.
        n1 = fields%n1
        n2 = fields%n2

        !> Copy the variable.
        z = 0
        select case (vname)

            !> Meteorological forcing.
            case (VN_FSIN, VN_FSVH, VN_FSIH)
                if (associated(fields%vs%fsin)) then
                    call output_variables_allocate(fields%fsin, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsin, n1)
                    call output_variables_allocate(fields%ifsin, n1)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%ifsin, n1)
                else
                    z = 1
                end if
            case (VN_FSDIR)
                if (associated(fields%vs%fsdr)) then
                    call output_variables_allocate(fields%fsdr, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsdr, n1)
                else
                    z = 1
                end if
            case (VN_FSDFF)
                if (associated(fields%vs%fsdff)) then
                    call output_variables_allocate(fields%fsdff, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsdff, n1)
                else
                    z = 1
                end if
            case (VN_FSOUT)
                if (associated(fields%vs%fsin) .and. associated(fields%vs%albt)) then
                    call output_variables_allocate(fields%fsout, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsout, n1)
                    call output_variables_allocate(fields%ifsin, n1)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%ifsin, n1)
                else
                    z = 1
                end if
            case (VN_FLIN)
                if (associated(fields%vs%flin)) then
                    call output_variables_allocate(fields%flin, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%flin, n1)
                else
                    z = 1
                end if
            case (VN_FLOUT)
                if (associated(fields%vs%gte)) then
                    call output_variables_allocate(fields%flout, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%flout, n1)
                else
                    z = 1
                end if
            case (VN_TA)
                if (associated(fields%vs%ta)) then
                    call output_variables_allocate(fields%ta, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%ta, n1)
                else
                    z = 1
                end if
            case (VN_QA)
                if (associated(fields%vs%qa)) then
                    call output_variables_allocate(fields%qa, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%qa, n1)
                else
                    z = 1
                end if
            case (VN_PRES)
                if (associated(fields%vs%pres)) then
                    call output_variables_allocate(fields%pres, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%pres, n1)
                else
                    z = 1
                end if
            case (VN_UV)
                if (associated(fields%vs%uv)) then
                    call output_variables_allocate(fields%uv, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%uv, n1)
                else
                    z = 1
                end if
            case (VN_WDIR)
                if (associated(fields%vs%wdir)) then
                    call output_variables_allocate(fields%wdir, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%wdir, n1)
                else
                    z = 1
                end if
            case (VN_UU)
                if (associated(fields%vs%uu)) then
                    call output_variables_allocate(fields%uu, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%uu, n1)
                else
                    z = 1
                end if
            case (VN_VV)
                if (associated(fields%vs%vv)) then
                    call output_variables_allocate(fields%vv, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%vv, n1)
                else
                    z = 1
                end if
            case (VN_PRE)
                if (associated(fields%vs%pre)) then
                    call output_variables_allocate(fields%pre, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%pre, n1)
                else
                    z = 1
                end if
            case (VN_PRERN)
                if (associated(fields%vs%prern)) then
                    call output_variables_allocate(fields%prern, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%prern, n1)
                else
                    z = 1
                end if
            case (VN_PRESNO)
                if (associated(fields%vs%presno)) then
                    call output_variables_allocate(fields%presno, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%presno, n1)
                else
                    z = 1
                end if
            case (VN_PREC)
                if (associated(fields%vs%pre)) then
                    call output_variables_allocate(fields%prec, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%prec, n1)
                else
                    z = 1
                end if
            case (VN_PRECRN)
                if (associated(fields%vs%prern)) then
                    call output_variables_allocate(fields%precrn, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%precrn, n1)
                else
                    z = 1
                end if
            case (VN_PRECSNO)
                if (associated(fields%vs%presno)) then
                    call output_variables_allocate(fields%precsno, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%precsno, n1)
                else
                    z = 1
                end if
            case (VN_RCAN)
                if (associated(fields%vs%rcan)) then
                    call output_variables_allocate(fields%rcan, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%rcan, n1)
                else
                    z = 1
                end if
            case (VN_SNCAN)
                if (associated(fields%vs%sncan)) then
                    call output_variables_allocate(fields%sncan, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%sncan, n1)
                else
                    z = 1
                end if
            case (VN_CMAS)
                call output_variables_activate_pntr(fields, VN_TCAN)
                if (associated(fields%tcan) .and. associated(fields%vs%cmas)) then
                    call output_variables_allocate(fields%cmas, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%cmas, n1)
                else
                    z = 1
                end if
            case (VN_TCAN)
                if (associated(fields%vs%tcan)) then
                    call output_variables_allocate(fields%tcan, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%tcan, n1)
                    call output_variables_allocate(fields%ican, n1)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%ican, n1)
                else
                    z = 1
                end if
            case (VN_GRO)
                if (associated(fields%vs%gro)) then
                    call output_variables_allocate(fields%gro, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%gro, n1)
                else
                    z = 1
                end if
            case (VN_SNO)
                if (associated(fields%vs%sno)) then
                    call output_variables_allocate(fields%sno, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%sno, n1)
                else
                    z = 1
                end if
            case (VN_RHOSNO)
                if (associated(fields%vs%rhos)) then
                    call output_variables_allocate(fields%rhosno, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%rhosno, n1)
                else
                    z = 1
                end if
            case (VN_ZSNO)
                if (associated(fields%vs%zsno)) then
                    call output_variables_allocate(fields%zsno, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%zsno, n1)
                else
                    z = 1
                end if
            case (VN_FSNO)
                if (associated(fields%vs%fsno)) then
                    call output_variables_allocate(fields%fsno, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%fsno, n1)
                else
                    z = 1
                end if
            case (VN_WSNO)
                if (associated(fields%vs%wsno)) then
                    call output_variables_allocate(fields%wsno, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%wsno, n1)
                else
                    z = 1
                end if
            case (VN_TSNO)
                if (associated(fields%vs%tsno)) then
                    call output_variables_allocate(fields%tsno, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%tsno, n1)
                    call output_variables_allocate(fields%isno, n1)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%isno, n1)
                else
                    z = 1
                end if
            case (VN_ROFSNO)
                if (associated(fields%vs%rofsno)) then
                    call output_variables_allocate(fields%rofsno, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%rofsno, n1)
                else
                    z = 1
                end if
            case (VN_ALBT)
                call output_variables_activate_pntr(fields, VN_FSIN)
                if (associated(fields%fsin) .and. associated(fields%vs%albt)) then
                    call output_variables_allocate(fields%albt, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%albt, n1)
                else
                    z = 1
                end if
            case (VN_ALVS)
                call output_variables_activate_pntr(fields, VN_FSIN)
                if (associated(fields%fsin) .and. associated(fields%vs%alvs)) then
                    call output_variables_allocate(fields%alvs, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%alvs, n1)
                else
                    z = 1
                end if
            case (VN_ALIR)
                call output_variables_activate_pntr(fields, VN_FSIN)
                if (associated(fields%fsin) .and. associated(fields%vs%alir)) then
                    call output_variables_allocate(fields%alir, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%alir, n1)
                else
                    z = 1
                end if
            case (VN_GTE)
                if (associated(fields%vs%gte)) then
                    call output_variables_allocate(fields%gte, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%gte, n1)
                else
                    z = 1
                end if
            case (VN_ZPND)
                if (associated(fields%vs%zpnd)) then
                    call output_variables_allocate(fields%zpnd, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%zpnd, n1)
                else
                    z = 1
                end if
            case (VN_PNDW)
                if (associated(fields%vs%pndw)) then
                    call output_variables_allocate(fields%pndw, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%pndw, n1)
                else
                    z = 1
                end if
            case (VN_TPND)
                if (associated(fields%vs%tpnd)) then
                    call output_variables_allocate(fields%tpnd, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%tpnd, n1)
                    call output_variables_allocate(fields%ipnd, n1)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%ipnd, n1)
                else
                    z = 1
                end if
            case (VN_PEVP)
                if (associated(fields%vs%pevp)) then
                    call output_variables_allocate(fields%pevp, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%pevp, n1)
                else
                    z = 1
                end if
            case (VN_EVAP)
                if (associated(fields%vs%evap)) then
                    call output_variables_allocate(fields%evap, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%evap, n1)
                else
                    z = 1
                end if
            case (VN_EVPB)
                if (associated(fields%vs%evpb)) then
                    call output_variables_allocate(fields%evpb, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%evpb, n1)
                else
                    z = 1
                end if
            case (VN_ARRD)
                if (associated(fields%vs%arrd)) then
                    call output_variables_allocate(fields%arrd, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%arrd, n1)
                else
                    z = 1
                end if
            case (VN_ROFO)
                if (associated(fields%vs%rofo)) then
                    call output_variables_allocate(fields%rofo, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%rofo, n1)
                else
                    z = 1
                end if
            case (VN_QE)
                if (associated(fields%vs%qevp)) then
                    call output_variables_allocate(fields%qe, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%qe, n1)
                else
                    z = 1
                end if
            case (VN_QH)
                if (associated(fields%vs%hfs)) then
                    call output_variables_allocate(fields%qh, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%qh, n1)
                else
                    z = 1
                end if
            case (VN_GZERO)
                if (associated(fields%vs%gzero)) then
                    call output_variables_allocate(fields%gzero, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%gzero, n1)
                else
                    z = 1
                end if
            case (VN_THLQ)
                if (associated(fields%vs%thlq)) then
                    call output_variables_allocate(fields%thlq, n1, n2, pntr, ig)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%thlq, n1, n2)
                else
                    z = 1
                end if
            case (VN_THIC)
                if (associated(fields%vs%thic)) then
                    call output_variables_allocate(fields%thic, n1, n2, pntr, ig)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%thic, n1, n2)
                else
                    z = 1
                end if
            case (VN_LQWS)
                if (associated(fields%vs%lqws)) then
                    call output_variables_activate_pntr(fields, VN_THLQ)
                    if (associated(fields%thlq)) then
                        call output_variables_allocate(fields%lqws, n1, n2, pntr, ig)
                        if (associated(fields%ts)) call output_variables_allocate(fields%ts%lqws, n1, n2)
                    end if
                else
                    z = 1
                end if
            case (VN_FZWS)
                if (associated(fields%vs%fzws)) then
                    call output_variables_activate_pntr(fields, VN_THIC)
                    if (associated(fields%thic)) then
                        call output_variables_allocate(fields%fzws, n1, n2, pntr, ig)
                        if (associated(fields%ts)) call output_variables_allocate(fields%ts%fzws, n1, n2)
                    end if
                else
                    z = 1
                end if
            case (VN_ALWS)
                call output_variables_activate_pntr(fields, VN_LQWS)
                call output_variables_activate_pntr(fields, VN_FZWS)
                if (associated(fields%lqws) .or. associated(fields%fzws)) then
                    call output_variables_allocate(fields%alws, n1, n2, pntr, ig)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%alws, n1, n2)
                else
                    z = 1
                end if
            case (VN_TBAR)
                if (associated(fields%vs%tbar)) then
                    call output_variables_allocate(fields%tbar, n1, n2, pntr, ig)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%tbar, n1, n2)
                else
                    z = 1
                end if
            case (VN_GFLX)
                if (associated(fields%vs%gflx)) then
                    call output_variables_allocate(fields%gflx, n1, n2, pntr, ig)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%gflx, n1, n2)
                else
                    z = 1
                end if
            case (VN_ROFS)
                if (associated(fields%vs%rofs)) then
                    call output_variables_allocate(fields%rofs, n1, n2, pntr, ig)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%rofs, n1, n2)
                else
                    z = 1
                end if
            case (VN_ROFB)
                if (associated(fields%vs%rofb)) then
                    call output_variables_allocate(fields%rofb, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%rofb, n1)
                else
                    z = 1
                end if
            case (VN_RCHG)
                if (associated(fields%vs%rchg)) then
                    call output_variables_allocate(fields%rchg, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%rchg, n1)
                else
                    z = 1
                end if
            case (VN_LZS)
                if (associated(fields%vs%lzs)) then
                    call output_variables_allocate(fields%lzs, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%lzs, n1)
                else
                    z = 1
                end if
            case (VN_DZS)
                if (associated(fields%vs%dzs)) then
                    call output_variables_allocate(fields%dzs, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%dzs, n1)
                else
                    z = 1
                end if
            case (VN_STGE)
                call output_variables_allocate(fields%stge, n1, pntr)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%stge, n1)
                call output_variables_allocate(fields%stg0e, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%stg0e, n1)
                call output_variables_allocate(fields%dstge, n1)
                if (associated(fields%ts)) call output_variables_allocate(fields%ts%dstge, n1)
            case (VN_DSTGE)
                call output_variables_activate_pntr(fields, VN_STGE)
                if (associated(fields%stge) .and. associated(fields%stg0e) .and. associated(fields%dstge)) then
                    call output_variables_allocate(fields%dstge, n1, pntr)
                end if
            case (VN_STGW)
                t = .true.
                call output_variables_activate_pntr(fields, VN_RCAN); t = (t .and. associated(fields%rcan))
                call output_variables_activate_pntr(fields, VN_SNCAN); t = (t .and. associated(fields%sncan))
                call output_variables_activate_pntr(fields, VN_SNO); t = (t .and. associated(fields%sno))
                call output_variables_activate_pntr(fields, VN_WSNO); t = (t .and. associated(fields%wsno))
                call output_variables_activate_pntr(fields, VN_PNDW); t = (t .and. associated(fields%pndw))
                call output_variables_activate_pntr(fields, VN_LQWS); t = (t .and. associated(fields%lqws))
                call output_variables_activate_pntr(fields, VN_FZWS); t = (t .and. associated(fields%fzws))
                call output_variables_activate_pntr(fields, VN_LZS); t = (t .and. associated(fields%lzs))
                call output_variables_activate_pntr(fields, VN_DZS); t = (t .and. associated(fields%dzs))
                if (t) then
                    call output_variables_allocate(fields%stgw, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%stgw, n1)
                    call output_variables_allocate(fields%stg0w, n1)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%stg0w, n1)
                    call output_variables_allocate(fields%dstgw, n1)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%dstgw, n1)
                end if
            case (VN_DSTGW)
                call output_variables_activate_pntr(fields, VN_STGW)
                if (associated(fields%stgw) .and. associated(fields%stg0w) .and. associated(fields%dstgw)) then
                    call output_variables_allocate(fields%dstgw, n1, pntr)
                end if
            case (VN_RFF)
                if (associated(fields%vs%rff)) then
                    call output_variables_allocate(fields%rff, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%rff, n1)
                else
                    z = 1
                end if
            case (VN_ROF)
                call output_variables_activate_pntr(fields, VN_ROFO)
                call output_variables_activate_pntr(fields, VN_ROFS)
                call output_variables_activate_pntr(fields, VN_ROFB)
                if (associated(fields%rofo) .and. associated(fields%rofs) .and. associated(fields%rofb)) then
                    call output_variables_allocate(fields%rof, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%rof, n1)
                else
                    z = 1
                end if
            case (VN_QI)
                if (associated(fields%vs%qi)) then
                    call output_variables_allocate(fields%qi, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%qi, n1)
                else
                    z = 1
                end if
            case (VN_QO)
                if (associated(fields%vs%qo)) then
                    call output_variables_allocate(fields%qo, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%qo, n1)
                else
                    z = 1
                end if
            case (VN_STGCH)
                if (associated(fields%vs%stgch)) then
                    call output_variables_allocate(fields%stgch, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%stgch, n1)
                else
                    z = 1
                end if
            case (VN_ZLVL)
                if (associated(fields%vs%zlvl)) then
                    call output_variables_allocate(fields%zlvl, n1, pntr)
                    if (associated(fields%ts)) call output_variables_allocate(fields%ts%zlvl, n1)
                else
                    z = 1
                end if
        end select

        !> Assign 'NO_DATA' value if the pointer has not been assigned because a necessary variable is not active.
        if (present(pntr) .and. z == 1) then
            if (.not. associated(pntr)) allocate(pntr(n1))
            pntr = out%NO_DATA
        end if

    end subroutine

    !> Description:
    !>  Allocate the specified output variables.
    subroutine output_variables_activate_vnames(fields, vnames)

        !> Input variables.
        type(output_fields), intent(in) :: fields
        character(len = *), dimension(:), intent(in) :: vnames

        !> Local variables.
        integer i

        !> Allocate.
        do i = 1, size(vnames)
            call output_variables_activate_pntr(fields, vnames(i))
        end do

    end subroutine

    !> Description:
    !>  Initialize and associate the attributes of the output
    !>  variables group.
    subroutine output_variables_group_init(group_vs, group, n1, n2)

        !> Input variables.
        type(model_variables_fields), intent(in), pointer :: group_vs
        integer, intent(in) :: n1, n2

        !> Input/output variables.
        type(output_fields), pointer :: group

        !> Allocate.
        if (.not. associated(group)) allocate(group)

        !> Assign indices.
        group%n1 = n1
        group%n2 = n2

        !> Associate to model variable group.
        group%vs => group_vs

    end subroutine

    !> Description:
    !>  Associate the output variable 'ts' group.
    subroutine output_variables_group_associate_ts(series)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input/output variables.
        type(output_series) series

        !> Associate to the 'ts' group.
        if (ro%RUNTILE) then
            series%tile%ts => out%ts%tile
        end if
        if (ro%RUNGRID) then
            series%grid%ts => out%ts%grid
            series%basin%ts => out%ts%basin
        end if

    end subroutine

    !> Description:
    !>  Set output variables to the 'NO_DATA' value.
    subroutine output_variables_group_reset(group)

        !> 'control_variables' required to check for active modelling components.
        use control_variables

        !> Input/output variables.
        type(output_fields) group

        !> Initialize variables.
        if (associated(group%ifsin)) group%ifsin = 0.0
        if (associated(group%fsin)) group%fsin = out%NO_DATA
        if (associated(group%fsdr)) group%fsdr = out%NO_DATA
        if (associated(group%fsdff)) group%fsdff = out%NO_DATA
        if (associated(group%fsout)) group%fsout = out%NO_DATA
        if (associated(group%flin)) group%flin = out%NO_DATA
        if (associated(group%flout)) group%flout = out%NO_DATA
        if (associated(group%ta)) group%ta = out%NO_DATA
        if (associated(group%qa)) group%qa = out%NO_DATA
        if (associated(group%pres)) group%pres = out%NO_DATA
        if (associated(group%uv)) group%uv = out%NO_DATA
        if (associated(group%wdir)) group%wdir = out%NO_DATA
        if (associated(group%uu)) group%uu = out%NO_DATA
        if (associated(group%vv)) group%vv = out%NO_DATA
        if (associated(group%pre)) group%pre = out%NO_DATA
        if (associated(group%prern)) group%prern = out%NO_DATA
        if (associated(group%presno)) group%presno = out%NO_DATA
        if (associated(group%prec)) group%prec = out%NO_DATA
        if (associated(group%precrn)) group%precrn = out%NO_DATA
        if (associated(group%precsno)) group%precsno = out%NO_DATA
        if (associated(group%ican)) group%ican = 0.0
        if (associated(group%rcan)) group%rcan = out%NO_DATA
        if (associated(group%sncan)) group%sncan = out%NO_DATA
        if (associated(group%cmas)) group%cmas = out%NO_DATA
        if (associated(group%tcan)) group%tcan = out%NO_DATA
        if (associated(group%gro)) group%gro = out%NO_DATA
        if (associated(group%isno)) group%isno = 0.0
        if (associated(group%sno)) group%sno = out%NO_DATA
        if (associated(group%rhosno)) group%rhosno = out%NO_DATA
        if (associated(group%zsno)) group%zsno = out%NO_DATA
        if (associated(group%fsno)) group%fsno = out%NO_DATA
        if (associated(group%wsno)) group%wsno = out%NO_DATA
        if (associated(group%tsno)) group%tsno = out%NO_DATA
        if (associated(group%rofsno)) group%rofsno = out%NO_DATA
        if (associated(group%albt)) group%albt = out%NO_DATA
        if (associated(group%alvs)) group%alvs = out%NO_DATA
        if (associated(group%alir)) group%alir = out%NO_DATA
        if (associated(group%gte)) group%gte = out%NO_DATA
        if (associated(group%ipnd)) group%ipnd = 0.0
        if (associated(group%zpnd)) group%zpnd = out%NO_DATA
        if (associated(group%pndw)) group%pndw = out%NO_DATA
        if (associated(group%tpnd)) group%tpnd = out%NO_DATA
        if (associated(group%pevp)) group%pevp = out%NO_DATA
        if (associated(group%evap)) group%evap = out%NO_DATA
        if (associated(group%evpb)) group%evpb = out%NO_DATA
        if (associated(group%arrd)) group%arrd = out%NO_DATA
        if (associated(group%rofo)) group%rofo = out%NO_DATA
        if (associated(group%qe)) group%qe = out%NO_DATA
        if (associated(group%qh)) group%qh = out%NO_DATA
        if (associated(group%gzero)) group%gzero = out%NO_DATA
        if (associated(group%thlq)) group%thlq = out%NO_DATA
        if (associated(group%thic)) group%thic = out%NO_DATA
        if (associated(group%lqws)) group%lqws = out%NO_DATA
        if (associated(group%fzws)) group%fzws = out%NO_DATA
        if (associated(group%alws)) group%alws = out%NO_DATA
        if (associated(group%tbar)) group%tbar = out%NO_DATA
        if (associated(group%gflx)) group%gflx = out%NO_DATA
        if (associated(group%rofs)) group%rofs = out%NO_DATA
        if (associated(group%rofb)) group%rofb = out%NO_DATA
        if (associated(group%rchg)) group%rchg = out%NO_DATA
        if (associated(group%lzs)) group%lzs = out%NO_DATA
        if (associated(group%dzs)) group%dzs = out%NO_DATA
        if (associated(group%stge)) then
            group%stg0e = group%stge
            group%stge = 0.0
            group%dstge = 0.0
        end if
        if (associated(group%stgw)) then
            group%stg0w = group%stgw
            group%stgw = 0.0
            group%dstgw = 0.0
        end if
        if (associated(group%rff)) group%rff = out%NO_DATA
        if (associated(group%rof)) group%rof = out%NO_DATA
        if (associated(group%qi)) group%qi = out%NO_DATA
        if (associated(group%qo)) group%qo = out%NO_DATA
        if (associated(group%stgch)) group%stgch = out%NO_DATA
        if (associated(group%zlvl)) group%zlvl = out%NO_DATA

    end subroutine

    !> Description:
    !>  Reset output variables to the 'NO_DATA' value.
    subroutine output_variables_reset(shd)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

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
    subroutine output_variables_series_init(shd, series)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Input/output variables.
        type(output_series) series

        !> Allocate and initialize the variables.
        if (ro%RUNTILE) then
            call output_variables_group_init(vs%tile, series%tile, shd%lc%NML, shd%lc%IGND)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_init(vs%grid, series%grid, shd%NA, shd%lc%IGND)
            call output_variables_group_init(vs%basin, series%basin, shd%NA, shd%lc%IGND)
        end if

    end subroutine

    !> Description:
    !>  Allocate and initialize output variables.
    subroutine output_variables_init(shd)

        !> 'shd_variables' required for 'shd'.
        use shd_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Initialize 'ts' values.
        call output_variables_series_init(shd, out%ts)

        !> Totals (e.g., accumulated).
        call output_variables_series_init(shd, out%tot)
        call output_variables_group_associate_ts(out%tot)

        !> Yearly.
        call output_variables_series_init(shd, out%y)
        call output_variables_group_associate_ts(out%y)

        !> Monthly.
        call output_variables_series_init(shd, out%m)
        call output_variables_group_associate_ts(out%m)

        !> Daily.
        call output_variables_series_init(shd, out%d)
        call output_variables_group_associate_ts(out%d)

        !> Hourly.
        call output_variables_series_init(shd, out%h)
        call output_variables_group_associate_ts(out%h)

    end subroutine

    !> Description:
    !>  Update output variables from current model states and variables.
    !>  Variables are updated if all elements of the group are equal to
    !>  the 'NO_DATA' value; if not the case, updates are assumed to
    !>  have occured in the model (e.g., by process modules), and those
    !>  values are preserved.
    subroutine output_variables_group_update_ts(shd, group, group_vs)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        !> 'model_dates' required for 'ic' (counter and time-stepping).
        use shd_variables
        use control_variables
        use model_dates

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(model_variables_fields), intent(in) :: group_vs

        !> Input/output variables.
        type(output_fields) group

        !> Local variables.
        integer j

        !> Update variables.
        if (associated(group%ifsin)) then
            where (group_vs%fsin > 0.0)
                group%ifsin = 1.0
            elsewhere
                group%ifsin = 0.0
            end where
        end if
        if (associated(group%fsin)) then
            if (all(group%fsin == out%NO_DATA)) group%fsin = group_vs%fsin
        end if
        if (associated(group%fsdr)) then
            if (all(group%fsdr == out%NO_DATA)) group%fsdr = group_vs%fsdr
        end if
        if (associated(group%fsdff)) then
            if (all(group%fsdff == out%NO_DATA)) group%fsdff = group_vs%fsdff
        end if
        if (associated(group%fsout)) then
            if (all(group%fsout == out%NO_DATA)) group%fsout = group_vs%fsin*(1.0 - group_vs%albt)
        end if
        if (associated(group%flin)) then
            if (all(group%flin == out%NO_DATA)) group%flin = group_vs%flin
        end if
        if (associated(group%flout)) then
            if (all(group%flout == out%NO_DATA)) group%flout = 5.66796E-8*group_vs%gte**4
        end if
        if (associated(group%ta)) then
            if (all(group%ta == out%NO_DATA)) group%ta = group_vs%ta
        end if
        if (associated(group%qa)) then
            if (all(group%qa == out%NO_DATA)) group%qa = group_vs%qa
        end if
        if (associated(group%pres)) then
            if (all(group%pres == out%NO_DATA)) group%pres = group_vs%pres
        end if
        if (associated(group%uv)) then
            if (all(group%uv == out%NO_DATA)) group%uv = group_vs%uv
        end if
        if (associated(group%wdir)) then
            if (all(group%wdir == out%NO_DATA)) group%wdir = group_vs%wdir
        end if
        if (associated(group%uu)) then
            if (all(group%uu == out%NO_DATA)) group%uu = group_vs%uu
        end if
        if (associated(group%vv)) then
            if (all(group%vv == out%NO_DATA)) group%vv = group_vs%vv
        end if
        if (associated(group%pre)) then
            if (all(group%pre == out%NO_DATA)) group%pre = group_vs%pre
        end if
        if (associated(group%prern)) then
            if (all(group%prern == out%NO_DATA)) group%prern = group_vs%prern
        end if
        if (associated(group%presno)) then
            if (all(group%presno == out%NO_DATA)) group%presno = group_vs%presno
        end if
        if (associated(group%prec)) then
            if (all(group%prec == out%NO_DATA)) group%prec = group_vs%pre*ic%dts
        end if
        if (associated(group%precrn)) then
            if (all(group%precrn == out%NO_DATA)) group%precrn = group_vs%prern*ic%dts
        end if
        if (associated(group%precsno)) then
            if (all(group%precsno == out%NO_DATA)) group%precsno = group_vs%presno*ic%dts
        end if
        if (associated(group%ican)) then
            where (group_vs%tcan > 0.0)
                group%ican = 1.0
            elsewhere
                group%ican = 0.0
            end where
        end if
        if (associated(group%rcan)) then
            if (all(group%rcan == out%NO_DATA)) group%rcan = group_vs%rcan
        end if
        if (associated(group%sncan)) then
            if (all(group%sncan == out%NO_DATA)) group%sncan = group_vs%sncan
        end if
        if (associated(group%cmas)) then
            if (all(group%cmas == out%NO_DATA)) then
                where (group%ican == 1.0)
                    group%cmas = group_vs%cmas
                elsewhere
                    group%cmas = 0.0
                end where
            end if
        end if
        if (associated(group%tcan)) then
            if (all(group%tcan == out%NO_DATA)) then
                where (group%ican == 1.0)
                    group%tcan = group_vs%tcan
                elsewhere
                    group%tcan = 0.0
                endwhere
            end if
        end if
        if (associated(group%gro)) then
            if (all(group%gro == out%NO_DATA)) group%gro = group_vs%gro
        end if
        if (associated(group%isno)) then
            where (group_vs%tsno > 0.0)
                group%isno = 1.0
            elsewhere
                group%isno = 0.0
            end where
        end if
        if (associated(group%sno)) then
            if (all(group%sno == out%NO_DATA)) group%sno = group_vs%sno
        end if
        if (associated(group%rhosno)) then
            if (all(group%rhosno == out%NO_DATA)) group%rhosno = group_vs%rhos
        end if
        if (associated(group%zsno)) then
            if (all(group%zsno == out%NO_DATA)) group%zsno = group_vs%zsno
        end if
        if (associated(group%fsno)) then
            if (all(group%fsno == out%NO_DATA)) group%fsno = group_vs%fsno
        end if
        if (associated(group%wsno)) then
            if (all(group%wsno == out%NO_DATA)) group%wsno = group_vs%wsno
        end if
        if (associated(group%tsno)) then
            if (all(group%tsno == out%NO_DATA)) then
                where (group%isno == 1.0)
                    group%tsno = group_vs%tsno
                elsewhere
                    group%tsno = 0.0
                end where
            end if
        end if
        if (associated(group%rofsno)) then
            if (all(group%rofsno == out%NO_DATA)) group%rofsno = group_vs%rofsno
        end if
        if (associated(group%albt)) then
            if (all(group%albt == out%NO_DATA)) group%albt = group_vs%albt
        end if
        if (associated(group%alvs)) then
            if (all(group%alvs == out%NO_DATA)) group%alvs = group_vs%alvs
        end if
        if (associated(group%alir)) then
            if (all(group%alir == out%NO_DATA)) group%alir = group_vs%alir
        end if
        if (associated(group%gte)) then
            if (all(group%gte == out%NO_DATA)) group%gte = group_vs%gte
        end if
        if (associated(group%ipnd)) then
            where (group_vs%tpnd > 0.0)
                group%ipnd = 1.0
            elsewhere
                group%ipnd = 0.0
            end where
        end if
        if (associated(group%zpnd)) then
            if (all(group%zpnd == out%NO_DATA)) group%zpnd = group_vs%zpnd
        end if
        if (associated(group%pndw)) then
            if (all(group%pndw == out%NO_DATA)) group%pndw = group_vs%pndw
        end if
        if (associated(group%tpnd)) then
            if (all(group%tpnd == out%NO_DATA)) then
                where (group%ipnd == 1.0)
                    group%tpnd = group_vs%tpnd
                elsewhere
                    group%tpnd = 0.0
                end where
            end if
        end if
        if (associated(group%pevp)) then
            if (all(group%pevp == out%NO_DATA)) group%pevp = group_vs%pevp
        end if
        if (associated(group%evap)) then
            if (all(group%evap == out%NO_DATA)) group%evap = group_vs%evap
        end if
        if (associated(group%evpb)) then
            if (all(group%evpb == out%NO_DATA)) group%evpb = group_vs%evpb
        end if
        if (associated(group%arrd)) then
            if (all(group%arrd == out%NO_DATA)) group%arrd = group_vs%arrd
        end if
        if (associated(group%rofo)) then
            if (all(group%rofo == out%NO_DATA)) group%rofo = group_vs%rofo
        end if
        if (associated(group%qe)) then
            if (all(group%qe == out%NO_DATA)) group%qe = group_vs%qevp
        end if
        if (associated(group%qh)) then
            if (all(group%qh == out%NO_DATA)) group%qh = group_vs%hfs
        end if
        if (associated(group%gzero)) then
            if (all(group%gzero == out%NO_DATA)) group%gzero = group_vs%gzero
        end if
        if (associated(group%thlq)) then
            if (all(group%thlq == out%NO_DATA)) group%thlq = group_vs%thlq
        end if
        if (associated(group%thic)) then
            if (all(group%thic == out%NO_DATA)) group%thic = group_vs%thic
        end if
        if (associated(group%lqws)) then
            if (all(group%lqws == out%NO_DATA)) group%lqws = group_vs%lqws
        end if
        if (associated(group%fzws)) then
            if (all(group%fzws == out%NO_DATA)) group%fzws = group_vs%fzws
        end if
        if (associated(group%alws)) then
            if (all(group%alws == out%NO_DATA)) then
                group%alws = 0.0
                where (group%lqws /= out%NO_DATA) group%alws = group%alws + group%lqws
                where (group%fzws /= out%NO_DATA) group%alws = group%alws + group%fzws
            end if
        end if
        if (associated(group%tbar)) then
            if (all(group%tbar == out%NO_DATA)) group%tbar = group_vs%tbar
        end if
        if (associated(group%gflx)) then
            if (all(group%gflx == out%NO_DATA)) group%gflx = group_vs%gflx
        end if
        if (associated(group%rofs)) then
            if (all(group%rofs == out%NO_DATA)) group%rofs = group_vs%rofs
        end if
        if (associated(group%rofb)) then
            if (all(group%rofb == out%NO_DATA)) group%rofb = group_vs%rofb
        end if
        if (associated(group%rchg)) then
            if (all(group%rchg == out%NO_DATA)) group%rchg = group_vs%rchg
        end if
        if (associated(group%lzs)) then
            if (all(group%lzs == out%NO_DATA)) group%lzs = group_vs%lzs
        end if
        if (associated(group%dzs)) then
            if (all(group%dzs == out%NO_DATA)) group%dzs = group_vs%dzs
        end if
        if (associated(group%stge)) then
            if (all(group%stge == 0.0)) then
                group%stg0e = out%NO_DATA
                group%stge = out%NO_DATA
                group%dstge = out%NO_DATA
            else
                group%dstge = group%stge - group%stg0e
            end if
        end if
        if (associated(group%stgw)) then
            if (all(group%stgw == 0.0)) then
                where (group%rcan /= out%NO_DATA) group%stgw = group%stgw + group%rcan
                where (group%sncan /= out%NO_DATA) group%stgw = group%stgw + group%sncan
                where (group%sno /= out%NO_DATA) group%stgw = group%stgw + group%sno
                where (group%wsno /= out%NO_DATA) group%stgw = group%stgw + group%wsno
                where (group%pndw /= out%NO_DATA) group%stgw = group%stgw + group%pndw
                do j = 1, shd%lc%IGND
                    where (group%lqws(:, j) /= out%NO_DATA) group%stgw = group%stgw + group%lqws(:, j)
                    where (group%fzws(:, j) /= out%NO_DATA) group%stgw = group%stgw + group%fzws(:, j)
                end do
                where (group%lzs /= out%NO_DATA) group%stgw = group%stgw + group%lzs
                where (group%dzs /= out%NO_DATA) group%stgw = group%stgw + group%dzs
            end if
            if (all(group%stgw == 0.0)) then
                group%stg0w = out%NO_DATA
                group%stgw = out%NO_DATA
                group%dstgw = out%NO_DATA
            else
                group%dstgw = group%stgw - group%stg0w
            end if
        end if
        if (associated(group%rff)) then
            if (all(group%rff == out%NO_DATA)) group%rff = group_vs%rff
        end if
        if (associated(group%rof)) then
            if (all(group%rof == out%NO_DATA)) then
                group%rof = 0.0
                where (group%rofo /= out%NO_DATA) group%rof = group%rof + group%rofo
                do j = 1, shd%lc%IGND
                    where (group%rofs(:, j) /= out%NO_DATA) group%rof = group%rof + group%rofs(:, j)
                end do
                where (group%rofb /= out%NO_DATA) group%rof = group%rof + group%rofb
            end if
        end if
        if (associated(group%qi)) then
            if (all(group%qi == out%NO_DATA)) group%qi = group_vs%qi
        end if
        if (associated(group%qo)) then
            if (all(group%qo == out%NO_DATA)) group%qo = group_vs%qo
        end if
        if (associated(group%stgch)) then
            if (all(group%stgch == out%NO_DATA)) group%stgch = group_vs%stgch
        end if
        if (associated(group%zlvl)) then
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
    subroutine output_variables_update_ts(shd)

        !> 'shd_variables' required for 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Update the variables.
        if (ro%RUNTILE) then
            call output_variables_group_update_ts(shd, out%ts%tile, vs%tile)
        end if
        if (ro%RUNGRID) then
            call output_variables_group_update_ts(shd, out%ts%grid, vs%grid)
            call output_variables_group_update_ts(shd, out%ts%basin, vs%basin)
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

        !> Update variables.
        if (associated(group%ifsin)) then
            call output_variables_field_update(group%ifsin, group_ts%ifsin, its, 'sum')
        end if
        if (associated(group%fsin)) then
            call output_variables_field_update(group%fsin, group_ts%fsin, its, 'avg')
        end if
        if (associated(group%fsdr)) then
            call output_variables_field_update(group%fsdr, group_ts%fsdr, its, 'avg')
        end if
        if (associated(group%fsdff)) then
            call output_variables_field_update(group%fsdff, group_ts%fsdff, its, 'avg')
        end if
        if (associated(group%fsout)) then
            call output_variables_field_update(group%fsout, group_ts%fsout, its, 'avg')
        end if
        if (associated(group%flin)) then
            call output_variables_field_update(group%flin, group_ts%flin, its, 'avg')
        end if
        if (associated(group%flout)) then
            call output_variables_field_update(group%flout, group_ts%flout, its, 'avg')
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
        if (associated(group%wdir)) then
            call output_variables_field_update(group%wdir, group_ts%wdir, its, 'avg')
        end if
        if (associated(group%uu)) then
            call output_variables_field_update(group%uu, group_ts%uu, its, 'avg')
        end if
        if (associated(group%vv)) then
            call output_variables_field_update(group%vv, group_ts%vv, its, 'avg')
        end if
        if (associated(group%pre)) then
            call output_variables_field_update(group%pre, group_ts%pre, its, 'avg')
        end if
        if (associated(group%prern)) then
            call output_variables_field_update(group%prern, group_ts%prern, its, 'avg')
        end if
        if (associated(group%presno)) then
            call output_variables_field_update(group%presno, group_ts%presno, its, 'avg')
        end if
        if (associated(group%prec)) then
            call output_variables_field_update(group%prec, group_ts%prec, its, 'sum')
        end if
        if (associated(group%precrn)) then
            call output_variables_field_update(group%precrn, group_ts%precrn, its, 'sum')
        end if
        if (associated(group%precsno)) then
            call output_variables_field_update(group%precsno, group_ts%precsno, its, 'sum')
        end if
        if (associated(group%ican)) then
            call output_variables_field_update(group%ican, group_ts%ican, its, 'sum')
        end if
        if (associated(group%rcan)) then
            call output_variables_field_update(group%rcan, group_ts%rcan, its, 'avg')
        end if
        if (associated(group%sncan)) then
            call output_variables_field_update(group%sncan, group_ts%sncan, its, 'avg')
        end if
        if (associated(group%cmas)) then
            call output_variables_field_icount_average(group%cmas, group_ts%cmas, group%ican, group_ts%ican)
        end if
        if (associated(group%tcan)) then
            call output_variables_field_icount_average(group%tcan, group_ts%tcan, group%ican, group_ts%ican)
        end if
        if (associated(group%gro)) then
            call output_variables_field_update(group%gro, group_ts%gro, its, 'avg')
        end if
        if (associated(group%isno)) then
            call output_variables_field_update(group%isno, group_ts%isno, its, 'sum')
        end if
        if (associated(group%sno)) then
            call output_variables_field_update(group%sno, group_ts%sno, its, 'avg')
        end if
        if (associated(group%rhosno)) then
            call output_variables_field_update(group%rhosno, group_ts%rhosno, its, 'avg')
        end if
        if (associated(group%zsno)) then
            call output_variables_field_update(group%zsno, group_ts%zsno, its, 'avg')
        end if
        if (associated(group%fsno)) then
            call output_variables_field_update(group%fsno, group_ts%fsno, its, 'avg')
        end if
        if (associated(group%wsno)) then
            call output_variables_field_update(group%wsno, group_ts%wsno, its, 'avg')
        end if
        if (associated(group%tsno)) then
            call output_variables_field_icount_average(group%tsno, group_ts%tsno, group%isno, group_ts%isno)
        end if
        if (associated(group%rofsno)) then
            call output_variables_field_update(group%rofsno, group_ts%rofsno, its, 'sum')
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
        if (associated(group%gte)) then
            call output_variables_field_update(group%gte, group_ts%gte, its, 'avg')
        end if
        if (associated(group%ipnd)) then
            call output_variables_field_update(group%ipnd, group_ts%ipnd, its, 'sum')
        end if
        if (associated(group%zpnd)) then
            call output_variables_field_update(group%zpnd, group_ts%zpnd, its, 'avg')
        end if
        if (associated(group%pndw)) then
            call output_variables_field_update(group%pndw, group_ts%pndw, its, 'avg')
        end if
        if (associated(group%tpnd)) then
            call output_variables_field_icount_average(group%tpnd, group_ts%tpnd, group%ipnd, group_ts%ipnd)
        end if
        if (associated(group%pevp)) then
            call output_variables_field_update(group%pevp, group_ts%pevp, its, 'sum')
        end if
        if (associated(group%evap)) then
            call output_variables_field_update(group%evap, group_ts%evap, its, 'sum')
        end if
        if (associated(group%evpb)) then
            call output_variables_field_update(group%evpb, group_ts%evpb, its, 'avg')
        end if
        if (associated(group%arrd)) then
            call output_variables_field_update(group%arrd, group_ts%arrd, its, 'avg')
        end if
        if (associated(group%rofo)) then
            call output_variables_field_update(group%rofo, group_ts%rofo, its, 'sum')
        end if
        if (associated(group%qe)) then
            call output_variables_field_update(group%qe, group_ts%qe, its, 'avg')
        end if
        if (associated(group%qh)) then
            call output_variables_field_update(group%qh, group_ts%qh, its, 'avg')
        end if
        if (associated(group%gzero)) then
            call output_variables_field_update(group%gzero, group_ts%gzero, its, 'avg')
        end if
        do j = 1, shd%lc%IGND
            if (associated(group%thlq)) then
                call output_variables_field_update(group%thlq(:, j), group_ts%thlq(:, j), its, 'avg')
            end if
            if (associated(group%thic)) then
                call output_variables_field_update(group%thic(:, j), group_ts%thic(:, j), its, 'avg')
            end if
            if (associated(group%lqws)) then
                call output_variables_field_update(group%lqws(:, j), group_ts%lqws(:, j), its, 'avg')
            end if
            if (associated(group%fzws)) then
                call output_variables_field_update(group%fzws(:, j), group_ts%fzws(:, j), its, 'avg')
            end if
            if (associated(group%alws)) then
                call output_variables_field_update(group%alws(:, j), group_ts%alws(:, j), its, 'avg')
            end if
            if (associated(group%tbar)) then
                call output_variables_field_update(group%tbar(:, j), group_ts%tbar(:, j), its, 'avg')
            end if
            if (associated(group%gflx)) then
                call output_variables_field_update(group%gflx(:, j), group_ts%gflx(:, j), its, 'avg')
            end if
            if (associated(group%rofs)) then
                call output_variables_field_update(group%rofs(:, j), group_ts%rofs(:, j), its, 'sum')
            end if
        end do
        if (associated(group%rofb)) then
            call output_variables_field_update(group%rofb, group_ts%rofb, its, 'sum')
        end if
        if (associated(group%rchg)) then
            call output_variables_field_update(group%rchg, group_ts%rchg, its, 'sum')
        end if
        if (associated(group%lzs)) then
            call output_variables_field_update(group%lzs, group_ts%lzs, its, 'avg')
        end if
        if (associated(group%dzs)) then
            call output_variables_field_update(group%dzs, group_ts%dzs, its, 'avg')
        end if
        if (associated(group%stge)) then
            if (its == 1) then
                call output_variables_field_update(group%stg0e, group%stge, its, 'val')
            end if
            call output_variables_field_update(group%stge, group_ts%stge, its, 'avg')
            where (group%stge /= out%NO_DATA) group%dstge = group%stge - group%stg0e
        end if
        if (associated(group%stgw)) then
            if (its == 1) then
                call output_variables_field_update(group%stg0w, group%stgw, its, 'val')
            end if
            call output_variables_field_update(group%stgw, group_ts%stgw, its, 'avg')
            where (group%stgw /= out%NO_DATA) group%dstgw = group%stgw - group%stg0w
        end if
        if (associated(group%rff)) then
            call output_variables_field_update(group%rff, group_ts%rff, its, 'sum')
        end if
        if (associated(group%rof)) then
            call output_variables_field_update(group%rof, group_ts%rof, its, 'sum')
        end if
        if (associated(group%qi)) then
            call output_variables_field_update(group%qi, group_ts%qi, its, 'avg')
        end if
        if (associated(group%qo)) then
            call output_variables_field_update(group%qo, group_ts%qo, its, 'avg')
        end if
        if (associated(group%stgch)) then
            call output_variables_field_update(group%stgch, group_ts%stgch, its, 'avg')
        end if
        if (associated(group%zlvl)) then
            call output_variables_field_update(group%zlvl, group_ts%zlvl, its, 'avg')
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
    subroutine output_variables_update(shd)

        !> 'shd_variables' required for 'shd'.
        !> 'model_dates' required for 'ic' (counter and time-stepping).
        use shd_variables
        use model_dates

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Update 'ts' values.
        call output_variables_update_ts(shd)

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
