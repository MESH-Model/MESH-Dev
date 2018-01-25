module output_variables

    implicit none

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
    !>  Data type for storing data.
    !>
    !> Variables:
    !*  y, m, d, h, ts: Arrays of the variable at various time intervals.
    type output_variables_group
        real, dimension(:), allocatable :: tot, y, m, d, h, ts
    end type

    !> Description:
    !>  Group container for output variable series.
    type output_variables_group_container
        type(output_variables_group) &
            pre, fsin, fsvh, fsih, fsdr, fsdf, flin, ta, qa, pres, uu, vv, uv, wdir, &
            gro, evap, pevp, evpb, arrd, rof, rofo, rofs, rofb, &
            rcan, sncan, sno, fsno, wsno, zpnd, pndw, lzs, dzs, stgw, &
            cmas, tcan, tsno, tpnd, &
            alvs, alir, albt, fsout, flout, gte, qh, qe, gzero, stge, &
            ald, zod, &
            rff, rchg, qi, stgch, qo, zlvl
        type(output_variables_group), dimension(:), allocatable :: &
            thlq, lqws, thic, fzws, alws, &
            gflx, tbar
    end type

    !> Description:
    !>  Container for output objects including 'tile' and 'grid'
    !>  groups and NO_DATA values.
    !>
    !> Variables:
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
    !>  Allocate and initialize group.
    subroutine output_variables_allocate(field, n)

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
    !>  Allocate and initialize the per time-step output variables of
    !>  the provided group. Assign frequency keys to the time-series.
    subroutine output_variables_init_group(shd, group, n)

        !> 'shd_variables' required for indices from 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: n

        !> Input/output variables.
        type(output_variables_group_container) group

        !> Local variables.
        integer j, s

        !> Indices for allocation.
        s = shd%lc%IGND

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            allocate(group%pre%ts(n))
            allocate(group%fsin%ts(n))
            allocate(group%flin%ts(n))
            allocate(group%ta%ts(n))
            allocate(group%qa%ts(n))
            allocate(group%pres%ts(n))
            allocate(group%uv%ts(n))
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            allocate(group%gro%ts(n))
            allocate(group%evap%ts(n))
            allocate(group%pevp%ts(n))
            allocate(group%evpb%ts(n))
            allocate(group%arrd%ts(n))
            allocate(group%rof%ts(n))
            allocate(group%rofo%ts(n))
            allocate(group%rofs%ts(n))
            allocate(group%rofb%ts(n))
            allocate(group%rcan%ts(n))
            allocate(group%sncan%ts(n))
            allocate(group%sno%ts(n))
            allocate(group%fsno%ts(n))
            allocate(group%wsno%ts(n))
            allocate(group%zpnd%ts(n))
            allocate(group%pndw%ts(n))
            allocate(group%lzs%ts(n))
            allocate(group%dzs%ts(n))
            allocate(group%stgw%ts(n))
            allocate(group%thlq(s))
            allocate(group%lqws(s))
            allocate(group%thic(s))
            allocate(group%fzws(s))
            allocate(group%alws(s))
            do j = 1, s
                allocate(group%thlq(j)%ts(n))
                allocate(group%lqws(j)%ts(n))
                allocate(group%thic(j)%ts(n))
                allocate(group%fzws(j)%ts(n))
                allocate(group%alws(j)%ts(n))
            end do
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            allocate(group%cmas%ts(n))
            allocate(group%tcan%ts(n))
            allocate(group%tsno%ts(n))
            allocate(group%tpnd%ts(n))
            allocate(group%alvs%ts(n))
            allocate(group%alir%ts(n))
            allocate(group%albt%ts(n))
            allocate(group%fsout%ts(n))
            allocate(group%flout%ts(n))
            allocate(group%gte%ts(n))
            allocate(group%qh%ts(n))
            allocate(group%qe%ts(n))
            allocate(group%gzero%ts(n))
            allocate(group%stge%ts(n))
            allocate(group%gflx(s))
            allocate(group%tbar(s))
            do j = 1, s
                allocate(group%gflx(j)%ts(n))
                allocate(group%tbar(j)%ts(n))
            end do
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            allocate(group%rff%ts(n))
            allocate(group%rchg%ts(n))
            allocate(group%qi%ts(n))
            allocate(group%stgch%ts(n))
            allocate(group%qo%ts(n))
            allocate(group%zlvl%ts(n))
        end if

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
            call output_variables_reset_group(shd, out%tile)
            call output_variables_update_tile(shd, cm)
        end if

        !> Grid-based.
        if (ro%RUNGRID) then
            call output_variables_init_group(shd, out%grid, shd%NA)
            call output_variables_reset_group(shd, out%grid)
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
        integer j

        !> Meteorological forcing.
        !> Climate variables are not allocated by group so must check 'allocated' status.
        if (ro%RUNCLIM) then
            if (allocated(cm%dat(ck%RT)%GAT)) then
                if (all(out%tile%pre%ts == out%NO_DATA)) out%tile%pre%ts = cm%dat(ck%RT)%GAT
            end if
            if (allocated(cm%dat(ck%FB)%GAT)) then
                if (all(out%tile%fsin%ts == out%NO_DATA)) out%tile%fsin%ts = cm%dat(ck%FB)%GAT
            end if
            if (allocated(cm%dat(ck%FI)%GAT)) then
                if (all(out%tile%flin%ts == out%NO_DATA)) out%tile%flin%ts = cm%dat(ck%FI)%GAT
            end if
            if (allocated(cm%dat(ck%TT)%GAT)) then
                if (all(out%tile%ta%ts == out%NO_DATA)) out%tile%ta%ts = cm%dat(ck%TT)%GAT
            end if
            if (allocated(cm%dat(ck%HU)%GAT)) then
                if (all(out%tile%qa%ts == out%NO_DATA)) out%tile%qa%ts = cm%dat(ck%HU)%GAT
            end if
            if (allocated(cm%dat(ck%P0)%GAT)) then
                if (all(out%tile%pres%ts == out%NO_DATA)) out%tile%pres%ts = cm%dat(ck%P0)%GAT
            end if
            if (allocated(cm%dat(ck%UV)%GAT)) then
                if (all(out%tile%uv%ts == out%NO_DATA)) out%tile%uv%ts = cm%dat(ck%UV)%GAT
            end if
        end if

        !> Water balance.
        !> 'stas' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNBALWB) then
            if (all(out%tile%gro%ts == out%NO_DATA)) out%tile%gro%ts = stas%cnpy%gro
            if (all(out%tile%evap%ts == out%NO_DATA)) out%tile%evap%ts = stas%sfc%evap
            if (all(out%tile%pevp%ts == out%NO_DATA)) out%tile%pevp%ts = stas%sfc%pevp
            if (all(out%tile%evpb%ts == out%NO_DATA)) out%tile%evpb%ts = stas%sfc%evpb
            if (all(out%tile%arrd%ts == out%NO_DATA)) out%tile%arrd%ts = stas%sfc%arrd
            if (all(out%tile%rof%ts == out%NO_DATA)) then
                out%tile%rof%ts = stas%sfc%rofo + stas%sl%rofs + stas%lzs%rofb + stas%dzs%rofb
            end if
            if (all(out%tile%rofo%ts == out%NO_DATA)) out%tile%rofo%ts = stas%sfc%rofo
            if (all(out%tile%rofs%ts == out%NO_DATA)) out%tile%rofs%ts = stas%sl%rofs
            if (all(out%tile%rofb%ts == out%NO_DATA)) out%tile%rofb%ts = stas%lzs%rofb + stas%dzs%rofb
            if (all(out%tile%rcan%ts == out%NO_DATA)) out%tile%rcan%ts = stas%cnpy%rcan
            if (all(out%tile%sncan%ts == out%NO_DATA)) out%tile%sncan%ts = stas%cnpy%sncan
            if (all(out%tile%sno%ts == out%NO_DATA)) out%tile%sno%ts = stas%sno%sno
            if (all(out%tile%fsno%ts == out%NO_DATA)) out%tile%fsno%ts = stas%sno%fsno
            if (all(out%tile%wsno%ts == out%NO_DATA)) out%tile%wsno%ts = stas%sno%wsno
            if (all(out%tile%zpnd%ts == out%NO_DATA)) out%tile%zpnd%ts = stas%sfc%zpnd
            if (all(out%tile%pndw%ts == out%NO_DATA)) out%tile%pndw%ts = stas%sfc%pndw
            if (all(out%tile%lzs%ts == out%NO_DATA)) out%tile%lzs%ts = stas%lzs%ws
            if (all(out%tile%dzs%ts == out%NO_DATA)) out%tile%dzs%ts = stas%dzs%ws
!            if (all(out%tile%stgw%ts == out%NO_DATA)) out%tile%stgw%ts =
            do j = 1, shd%lc%IGND
                if (all(out%tile%thlq(j)%ts == out%NO_DATA)) out%tile%thlq(j)%ts = stas%sl%thlq(:, j)
                if (all(out%tile%lqws(j)%ts == out%NO_DATA)) out%tile%lqws(j)%ts = stas%sl%lqws(:, j)
                if (all(out%tile%thic(j)%ts == out%NO_DATA)) out%tile%thic(j)%ts = stas%sl%thic(:, j)
                if (all(out%tile%fzws(j)%ts == out%NO_DATA)) out%tile%fzws(j)%ts = stas%sl%fzws(:, j)
                if (all(out%tile%alws(j)%ts == out%NO_DATA)) out%tile%alws(j)%ts = stas%sl%lqws(:, j) + stas%sl%fzws(:, j)
            end do
        end if

        !> Energy balance.
        !> 'stas' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNBALEB) then
            if (all(out%tile%cmas%ts == out%NO_DATA)) out%tile%cmas%ts = stas%cnpy%cmas
            if (all(out%tile%tcan%ts == out%NO_DATA)) out%tile%tcan%ts = stas%cnpy%tcan
            if (all(out%tile%tsno%ts == out%NO_DATA)) out%tile%tsno%ts = stas%sno%tsno
            if (all(out%tile%tpnd%ts == out%NO_DATA)) out%tile%tpnd%ts = stas%sfc%tpnd
            if (all(out%tile%albt%ts == out%NO_DATA)) out%tile%albt%ts = stas%sfc%albt
            if (all(out%tile%alvs%ts == out%NO_DATA)) out%tile%alvs%ts = stas%sfc%alvs
            if (all(out%tile%alir%ts == out%NO_DATA)) out%tile%alir%ts = stas%sfc%alir
            if (allocated(cm%dat(ck%FB)%GAT)) then
                if (all(out%tile%fsout%ts == out%NO_DATA)) out%tile%fsout%ts = cm%dat(ck%FB)%GAT*(1.0 - stas%sfc%albt)
            end if
            if (all(out%tile%gte%ts == out%NO_DATA)) out%tile%gte%ts = stas%sfc%gte
            if (all(out%tile%flout%ts == out%NO_DATA)) out%tile%flout%ts = 5.66796E-8*stas%sfc%gte**4
            if (all(out%tile%qh%ts == out%NO_DATA)) out%tile%qh%ts = stas%sfc%hfs
            if (all(out%tile%qe%ts == out%NO_DATA)) out%tile%qe%ts = stas%sfc%qevp
            if (all(out%tile%gzero%ts == out%NO_DATA)) out%tile%gzero%ts = stas%sfc%gzero
!            if (all(out%tile%stge%ts == out%NO_DATA)) out%tile%stge%ts =
            do j = 1, shd%lc%IGND
                if (all(out%tile%gflx(j)%ts == out%NO_DATA)) out%tile%gflx(j)%ts = stas%sl%gflx(:, j)
                if (all(out%tile%tbar(j)%ts == out%NO_DATA)) out%tile%tbar(j)%ts = stas%sl%tbar(:, j)
            end do
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
        integer j

        !> Meteorological forcing.
        !> Climate variables are not allocated by group so must check 'allocated' status.
        if (ro%RUNCLIM) then
            if (allocated(cm%dat(ck%RT)%GRD)) then
                if (all(out%grid%pre%ts == out%NO_DATA)) out%grid%pre%ts = cm%dat(ck%RT)%GRD
            end if
            if (allocated(cm%dat(ck%FB)%GRD)) then
                if (all(out%grid%fsin%ts == out%NO_DATA)) out%grid%fsin%ts = cm%dat(ck%FB)%GRD
            end if
            if (allocated(cm%dat(ck%FI)%GRD)) then
                if (all(out%grid%flin%ts == out%NO_DATA)) out%grid%flin%ts = cm%dat(ck%FI)%GRD
            end if
            if (allocated(cm%dat(ck%TT)%GRD)) then
                if (all(out%grid%ta%ts == out%NO_DATA)) out%grid%ta%ts = cm%dat(ck%TT)%GRD
            end if
            if (allocated(cm%dat(ck%HU)%GRD)) then
                if (all(out%grid%qa%ts == out%NO_DATA)) out%grid%qa%ts = cm%dat(ck%HU)%GRD
            end if
            if (allocated(cm%dat(ck%P0)%GRD)) then
                if (all(out%grid%pres%ts == out%NO_DATA)) out%grid%pres%ts = cm%dat(ck%P0)%GRD
            end if
            if (allocated(cm%dat(ck%UV)%GRD)) then
                if (all(out%grid%uv%ts == out%NO_DATA)) out%grid%uv%ts = cm%dat(ck%UV)%GRD
            end if
        end if

        !> Water balance.
        !> 'stas_grid' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNBALWB) then
            if (all(out%grid%gro%ts == out%NO_DATA)) out%grid%gro%ts = stas_grid%cnpy%gro
            if (all(out%grid%evap%ts == out%NO_DATA)) out%grid%evap%ts = stas_grid%sfc%evap
            if (all(out%grid%pevp%ts == out%NO_DATA)) out%grid%pevp%ts = stas_grid%sfc%pevp
            if (all(out%grid%evpb%ts == out%NO_DATA)) out%grid%evpb%ts = stas_grid%sfc%evpb
            if (all(out%grid%arrd%ts == out%NO_DATA)) out%grid%arrd%ts = stas_grid%sfc%arrd
            if (all(out%grid%rof%ts == out%NO_DATA)) then
                out%grid%rof%ts = stas_grid%sfc%rofo + stas_grid%sl%rofs + stas_grid%lzs%rofb + stas_grid%dzs%rofb
            end if
            if (all(out%grid%rofo%ts == out%NO_DATA)) out%grid%rofo%ts = stas_grid%sfc%rofo
            if (all(out%grid%rofs%ts == out%NO_DATA)) out%grid%rofs%ts = stas_grid%sl%rofs
            if (all(out%grid%rofb%ts == out%NO_DATA)) out%grid%rofb%ts = stas_grid%lzs%rofb + stas_grid%dzs%rofb
            if (all(out%grid%rcan%ts == out%NO_DATA)) out%grid%rcan%ts = stas_grid%cnpy%rcan
            if (all(out%grid%sncan%ts == out%NO_DATA)) out%grid%sncan%ts = stas_grid%cnpy%sncan
            if (all(out%grid%sno%ts == out%NO_DATA)) out%grid%sno%ts = stas_grid%sno%sno
            if (all(out%grid%fsno%ts == out%NO_DATA)) out%grid%fsno%ts = stas_grid%sno%fsno
            if (all(out%grid%wsno%ts == out%NO_DATA)) out%grid%wsno%ts = stas_grid%sno%wsno
            if (all(out%grid%zpnd%ts == out%NO_DATA)) out%grid%zpnd%ts = stas_grid%sfc%zpnd
            if (all(out%grid%pndw%ts == out%NO_DATA)) out%grid%pndw%ts = stas_grid%sfc%pndw
            if (all(out%grid%lzs%ts == out%NO_DATA)) out%grid%lzs%ts = stas_grid%lzs%ws
            if (all(out%grid%dzs%ts == out%NO_DATA)) out%grid%dzs%ts = stas_grid%dzs%ws
!            if (all(out%grid%stgw%ts == out%NO_DATA)) out%grid%stgw%ts =
            do j = 1, shd%lc%IGND
                if (all(out%grid%thlq(j)%ts == out%NO_DATA)) out%grid%thlq(j)%ts = stas_grid%sl%thlq(:, j)
                if (all(out%grid%lqws(j)%ts == out%NO_DATA)) out%grid%lqws(j)%ts = stas_grid%sl%lqws(:, j)
                if (all(out%grid%thic(j)%ts == out%NO_DATA)) out%grid%thic(j)%ts = stas_grid%sl%thic(:, j)
                if (all(out%grid%fzws(j)%ts == out%NO_DATA)) out%grid%fzws(j)%ts = stas_grid%sl%fzws(:, j)
                if (all(out%grid%alws(j)%ts == out%NO_DATA)) out%grid%alws(j)%ts = stas_grid%sl%lqws(:, j) + stas_grid%sl%fzws(:, j)
            end do
        end if

        !> Energy balance.
        !> 'stas_grid' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNBALEB) then
            if (all(out%grid%cmas%ts == out%NO_DATA)) out%grid%cmas%ts = stas_grid%cnpy%cmas
            if (all(out%grid%tcan%ts == out%NO_DATA)) out%grid%tcan%ts = stas_grid%cnpy%tcan
            if (all(out%grid%tsno%ts == out%NO_DATA)) out%grid%tsno%ts = stas_grid%sno%tsno
            if (all(out%grid%tpnd%ts == out%NO_DATA)) out%grid%tpnd%ts = stas_grid%sfc%tpnd
            if (all(out%grid%albt%ts == out%NO_DATA)) out%grid%albt%ts = stas_grid%sfc%albt
            if (all(out%grid%alvs%ts == out%NO_DATA)) out%grid%alvs%ts = stas_grid%sfc%alvs
            if (all(out%grid%alir%ts == out%NO_DATA)) out%grid%alir%ts = stas_grid%sfc%alir
            if (allocated(cm%dat(ck%FB)%GRD)) then
                if (all(out%grid%fsout%ts == out%NO_DATA)) out%grid%fsout%ts = cm%dat(ck%FB)%GRD*(1.0 - stas_grid%sfc%albt)
            end if
            if (all(out%grid%gte%ts == out%NO_DATA)) out%grid%gte%ts = stas_grid%sfc%gte
            if (all(out%grid%flout%ts == out%NO_DATA)) out%grid%flout%ts = 5.66796E-8*stas_grid%sfc%gte**4
            if (all(out%grid%qh%ts == out%NO_DATA)) out%grid%qh%ts = stas_grid%sfc%hfs
            if (all(out%grid%qe%ts == out%NO_DATA)) out%grid%qe%ts = stas_grid%sfc%qevp
            if (all(out%grid%gzero%ts == out%NO_DATA)) out%grid%gzero%ts = stas_grid%sfc%gzero
!            if (all(out%grid%stge%ts == out%NO_DATA)) out%grid%stge%ts =
            do j = 1, shd%lc%IGND
                if (all(out%grid%gflx(j)%ts == out%NO_DATA)) out%grid%gflx(j)%ts = stas_grid%sl%gflx(:, j)
                if (all(out%grid%tbar(j)%ts == out%NO_DATA)) out%grid%tbar(j)%ts = stas_grid%sl%tbar(:, j)
            end do
        end if

        !> Channels and routing.
        !> 'stas_grid' variables are allocated by group so 'allocated' status is assumed.
        if (ro%RUNCHNL) then
            if (all(out%grid%rff%ts == out%NO_DATA)) out%grid%rff%ts = stas_grid%chnl%rff
            if (all(out%grid%rchg%ts == out%NO_DATA)) out%grid%rchg%ts = stas_grid%chnl%rchg
            if (all(out%grid%qi%ts == out%NO_DATA)) out%grid%qi%ts = stas_grid%chnl%qi
            if (all(out%grid%stgch%ts == out%NO_DATA)) out%grid%stgch%ts = stas_grid%chnl%stg
            if (all(out%grid%qo%ts == out%NO_DATA)) out%grid%qo%ts = stas_grid%chnl%qo
!            if (all(out%grid%zlvl%ts == out%NO_DATA)) out%grid%zlvl%ts = stas_grid%chnl%zlvl
        end if

    end subroutine

    !> Description:
    !>  Update 'field' from 'val' using the provided function 'fn'.
    !>  Reset 'field' if the first time-step of the period ('its' == 1).
    !>  Calculate an average provided the 'avg' function ('fn' == 'avg')
    !>  if the last time-step of period ('dnts' > 0).
    !>  Assign the NO_DATA value at indices where 'val' contains the
    !>  NO_DATA value.
    subroutine output_variables_update_field(field, val, its, dnts, fn)
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
    !>  Update the output variable for other time intervals.
    !>  Only fields allocated are updated.
    subroutine output_variables_update_group(group, fn)

        !> 'model_dates' required for 'ic' variable (counter and time-stepping).
        use model_dates

        !> Input/output variables.
        type(output_variables_group) group
        character(len = *) fn

        !> Local variables.
        integer dnts

        !> Totals (e.g., accumulated).
        if (allocated(group%tot)) then
            dnts = 0
            call output_variables_update_field(group%tot, group%ts, ic%ts_count, dnts, fn)
        end if

        !> Yearly.
        if (allocated(group%y)) then
            if (ic%now%year /= ic%next%year) then
                dnts = ic%ts_yearly
            else
                dnts = 0
            end if
            call output_variables_update_field(group%y, group%ts, ic%ts_yearly, dnts, fn)
        end if

        !> Monthly.
        if (allocated(group%m)) then
            if (ic%now%month /= ic%next%month) then
                dnts = ic%ts_monthly
            else
                dnts = 0
            end if
            call output_variables_update_field(group%m, group%ts, ic%ts_monthly, dnts, fn)
        end if

        !> Daily.
        if (allocated(group%d)) then
            if (ic%now%day /= ic%next%day) then
                dnts = ic%ts_daily
            else
                dnts = 0
            end if
            call output_variables_update_field(group%d, group%ts, ic%ts_daily, dnts, fn)
        end if

        !> Hourly.
        if (allocated(group%h)) then
            if (ic%now%hour /= ic%next%hour) then
                dnts = ic%ts_hourly
            else
                dnts = 0
            end if
            call output_variables_update_field(group%h, group%ts, ic%ts_hourly, dnts, fn)
        end if

    end subroutine

    !> Description:
    !>  Update the output variables.
    subroutine output_variables_update_series(shd, series)

        !> 'shd_variables' required for indices from 'shd'.
        use shd_variables

        !> Input variables.
        type(ShedGridParams) :: shd

        !> Input/output variables.
        type(output_variables_group_container) series

        !> Local variables.
        integer j

        !> Meteorological forcing.
        call output_variables_update_group(series%pre, 'sum')
        call output_variables_update_group(series%fsin, 'avg')
        call output_variables_update_group(series%flin, 'avg')
        call output_variables_update_group(series%ta, 'avg')
        call output_variables_update_group(series%qa, 'avg')
        call output_variables_update_group(series%pres, 'avg')
        call output_variables_update_group(series%uv, 'avg')

        !> Water balance.
        call output_variables_update_group(series%gro, 'avg')
        call output_variables_update_group(series%evap, 'sum')
        call output_variables_update_group(series%pevp, 'sum')
!        call output_variables_update_group(series%evpb, 'avg')
!        call output_variables_update_group(series%arrd, 'avg')
        call output_variables_update_group(series%rof, 'sum')
        call output_variables_update_group(series%rofo, 'sum')
        call output_variables_update_group(series%rofs, 'sum')
        call output_variables_update_group(series%rofb, 'sum')
        call output_variables_update_group(series%rcan, 'sum')
        call output_variables_update_group(series%sncan, 'sum')
        call output_variables_update_group(series%sno, 'sum')
        call output_variables_update_group(series%fsno, 'sum')
        call output_variables_update_group(series%wsno, 'sum')
        call output_variables_update_group(series%zpnd, 'avg')
        call output_variables_update_group(series%pndw, 'sum')
        call output_variables_update_group(series%lzs, 'sum')
        call output_variables_update_group(series%dzs, 'sum')
        call output_variables_update_group(series%stgw, 'val')
        do j = 1, shd%lc%IGND
            call output_variables_update_group(series%thlq(j), 'avg')
            call output_variables_update_group(series%lqws(j), 'sum')
            call output_variables_update_group(series%thic(j), 'avg')
            call output_variables_update_group(series%fzws(j), 'sum')
            call output_variables_update_group(series%alws(j), 'sum')
        end do

        !> Energy balance.
        call output_variables_update_group(series%cmas, 'sum')
        call output_variables_update_group(series%tcan, 'avg')
        call output_variables_update_group(series%tsno, 'avg')
        call output_variables_update_group(series%tpnd, 'avg')
        call output_variables_update_group(series%albt, 'avg')
        call output_variables_update_group(series%alvs, 'avg')
        call output_variables_update_group(series%alir, 'avg')
        call output_variables_update_group(series%fsout, 'avg')
        call output_variables_update_group(series%gte, 'avg')
        call output_variables_update_group(series%flout, 'avg')
        call output_variables_update_group(series%qh, 'avg')
        call output_variables_update_group(series%qe, 'avg')
        call output_variables_update_group(series%gzero, 'avg')
        call output_variables_update_group(series%stge, 'val')
        do j = 1, shd%lc%IGND
            call output_variables_update_group(series%gflx(j), 'avg')
            call output_variables_update_group(series%tbar(j), 'avg')
        end do

        !> Channels and routing.
        call output_variables_update_group(series%rff, 'sum')
        call output_variables_update_group(series%rchg, 'sum')
        call output_variables_update_group(series%qi, 'avg')
        call output_variables_update_group(series%stgch, 'avg')
        call output_variables_update_group(series%qo, 'avg')
!        call output_variables_update_group(series%zlvl, 'avg')

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
            call output_variables_update_series(shd, out%tile)
        end if

        !> Grid-based.
        if (ro%RUNGRID) then
            call output_variables_update_grid(shd, cm)
            call output_variables_update_series(shd, out%grid)
        end if

    end subroutine

    !> Description:
    !>  Reset output variables of the provided group. Set variables to
    !>  the NO_DATA value.
    subroutine output_variables_reset_group(shd, group)

        !> 'shd_variables' required for indices from 'shd'.
        !> 'control_variables' required to check for active modelling components.
        use shd_variables
        use control_variables

        !> Input/output variables.
        type(ShedGridParams), intent(in) :: shd
        type(output_variables_group_container) group

        !> Local variables.
        integer j

        !> Meteorological forcing.
        if (ro%RUNCLIM) then
            group%pre%ts = out%NO_DATA
            group%fsin%ts = out%NO_DATA
            group%flin%ts = out%NO_DATA
            group%ta%ts = out%NO_DATA
            group%qa%ts = out%NO_DATA
            group%pres%ts = out%NO_DATA
            group%uv%ts = out%NO_DATA
        end if

        !> Water balance.
        if (ro%RUNBALWB) then
            group%gro%ts = out%NO_DATA
            group%evap%ts = out%NO_DATA
            group%pevp%ts = out%NO_DATA
            group%evpb%ts = out%NO_DATA
            group%arrd%ts = out%NO_DATA
            group%rof%ts = out%NO_DATA
            group%rofo%ts = out%NO_DATA
            group%rofs%ts = out%NO_DATA
            group%rofb%ts = out%NO_DATA
            group%rcan%ts = out%NO_DATA
            group%sncan%ts = out%NO_DATA
            group%sno%ts = out%NO_DATA
            group%fsno%ts = out%NO_DATA
            group%wsno%ts = out%NO_DATA
            group%zpnd%ts = out%NO_DATA
            group%pndw%ts = out%NO_DATA
            group%lzs%ts = out%NO_DATA
            group%dzs%ts = out%NO_DATA
            group%stgw%ts = out%NO_DATA
            do j = 1, shd%lc%IGND
                group%thlq(j)%ts = out%NO_DATA
                group%lqws(j)%ts = out%NO_DATA
                group%thic(j)%ts = out%NO_DATA
                group%fzws(j)%ts = out%NO_DATA
                group%alws(j)%ts = out%NO_DATA
            end do
        end if

        !> Energy balance.
        if (ro%RUNBALEB) then
            group%cmas%ts = out%NO_DATA
            group%tcan%ts = out%NO_DATA
            group%tsno%ts = out%NO_DATA
            group%tpnd%ts = out%NO_DATA
            group%alvs%ts = out%NO_DATA
            group%alir%ts = out%NO_DATA
            group%albt%ts = out%NO_DATA
            group%fsout%ts = out%NO_DATA
            group%flout%ts = out%NO_DATA
            group%gte%ts = out%NO_DATA
            group%qh%ts = out%NO_DATA
            group%qe%ts = out%NO_DATA
            group%gzero%ts = out%NO_DATA
            group%stge%ts = out%NO_DATA
            do j = 1, shd%lc%IGND
                group%gflx(j)%ts = out%NO_DATA
                group%tbar(j)%ts = out%NO_DATA
            end do
        end if

        !> Channels and routing.
        if (ro%RUNCHNL) then
            group%rff%ts = out%NO_DATA
            group%rchg%ts = out%NO_DATA
            group%qi%ts = out%NO_DATA
            group%stgch%ts = out%NO_DATA
            group%qo%ts = out%NO_DATA
            group%zlvl%ts = out%NO_DATA
        end if

    end subroutine

    !> Description:
    !>  Reset output variables of all groups. Set variables to the
    !>  NO_DATA value.
    subroutine output_variables_reset(shd, cm)

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
        if (ro%RUNTILE) call output_variables_reset_group(shd, out%tile)

        !> Grid-based.
        if (ro%RUNGRID) call output_variables_reset_group(shd, out%grid)

    end subroutine

end module
