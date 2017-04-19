module rte_module

    implicit none

    !> Input variables for dynamic time-stepping.
    !*  dtminusr: Maximum time step [s].
    !*  mindtmin: Minimum time step [s].
    !*  maxindex: Maximum number of interations in reducing the time step [--].
    !*  dtminfrac: Time-step reducing factor [--].
    real(kind = 4) :: dtminusr = 1800.0, mindtmin = 10.0, maxindex = 50, dtminfrac = 0.75

    !> Convergence threshold.
    !*  convthreshusr: convergence level for channel routing.
    real(kind = 4) :: convthreshusr = 0.01

    !> Input parameters.
    type rte_params
        real, dimension(:), allocatable :: flz, pwr, r1n, r2n, mndr, aa2, aa3, aa4, widep
    end type

    !> Instances of the input parameters.
    !*  rtepm: Grid-based (GRD, NA/NAA).
    !*  rtepm_iak: River class-based (IAK).
    type(rte_params), save :: rtepm, rtepm_iak

    !> Configuration flags.
    type rte_flags
        logical :: PROCESS_ACTIVE = .false.
    end type

    !> Instance of control flags.
    type(rte_flags), save :: rteflg

    !> Option flags.
    type rte_options

    end type

    !> Instance of option flags.
    type(rte_options), save :: rteops

    contains

    !>
    !> _init() adapted from read_shed_fst_mod.f90.
    !>

    subroutine run_rte_init(shd, stfl, rrls)

        !> area_watflood: Shared variables used throughout rte code.
        use area_watflood

        !> mpi_module: Required for 'ipid'.
        use mpi_module

        !> sa_mesh_shared_variables: Variables, parameters, and types from SA_MESH.
        use sa_mesh_shared_variables
        use FLAGS

        !> model_output_variabletypes: Streamflow and reservoir output variables for SA_MESH.
        use model_output_variabletypes

        !> Basin properties from SA_MESH.
        type(ShedGridParams) :: shd

        !> Streamflow and reservoir output variables for SA_MESH.
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer n, l
        real ry, rx

        !> Return if not the head node or if the process is not active.
        if (ipid /= 0 .or. .not. rteflg%PROCESS_ACTIVE) return

        !> Transfer grid properties.
        ycount = shd%yCount
        imax = shd%yCount !> ycount and imax are used interchangeably in the code
        ydelta = shd%yDelta
        yorigin = shd%yOrigin
        xcount = shd%xCount
        jmax = shd%xCount !> jmax and xcount are used interghangeably in the code
        xdelta = shd%xDelta
        xorigin = shd%xOrigin
        al = shd%AL
        astep = al/1000.0
        istep = int(astep)
        step2 = astep*astep
        na = shd%NA
        naa = shd%NAA
        ntype = shd%lc%NTYPE + 1
        no = fms%stmg%n

        !> Allocate and transfer grid variables.
        allocate(xxx(na), yyy(na), s(ycount, xcount), &
                 next(na), da(na), bnkfll(na), slope(na), &
!                 elev(na)
                 rl(na), ibn(na), &
!                 sl1(na), sl2(na)
                 ichnl(na), ireach(na), &
                 grid_area(na), frac(na), aclass(na, ntype), &
                 glacier_flag(na))
        xxx = shd%xxx
        yyy = shd%yyy
        next = shd%NEXT
        da = shd%DA
        bnkfll = shd%BNKFLL
        slope = sqrt(shd%SLOPE_CHNL)
!+        elev = shd%ELEV
        rl = shd%CHNL_LEN
        ibn = shd%IAK
!+        sl1 = shd%SLOPE_INT
        ichnl = shd%ICHNL
        ireach = shd%IREACH
        grid_area = shd%AREA
        frac = shd%FRAC
        aclass = shd%lc%ACLASS
        glacier_flag = 'n'

!todo: useful checks higher in the code.
        if (minval(slope) < 0) then
            print *, 'Invalid channel slope variable (negative slopes)'
            stop
        end if
        if (minval(rl) < 0) then
            print *, 'Invalid channel length variable'
            stop
        end if
!+        if (minval(sl1) < 0) then
!+            print *, 'Invalid minimum interior slope'
!+            stop
!+        end if

        !> Allocate and assign parameter values.
        allocate(flz(na), flz2(na), pwr(na), r1n(na), r2n(na), rlake(na), &
                 mndr(na), aa2(na), aa3(na), aa4(na), theta(na), widep(na), kcond(na))
        flz = rtepm%flz; pwr = rtepm%pwr; r1n = rtepm%r1n; r2n = rtepm%r2n
        mndr = rtepm%mndr; aa2 = rtepm%aa2; aa3 = rtepm%aa3; aa4 = rtepm%aa4; widep = rtepm%widep
        flz2 = 0.0; rlake = 0.0; theta = 0.0; kcond = 0.0

        !> Adjust the calculated channel length by the degree of meandering.
        rl = rl*mndr

        !> Allocate many of the arrays used by the routing code. This block
        !> is deliberately skipping arrays that reference land use types,
        !> since wetland routing is currently not enabled,
        !> and most of the land use peculiarities are in WATFLOOD
        !> rather than this version of RPN-Watroute.
        allocate(qi1(na), qi2(na), qo1(na), qo2(na), qo2sim(na), &
                 qr(na), d2(na), qda(na), cap(na), over(na), &
                 qmax(na), res(na), sump(na), store1(na), &
                 store2(na), att(na), qbase(na), nreach(maxval(ireach)), &
                 totd1(na), totuzs(na), totsnw(na), qstream(na), &
                 totchnl(na), totgrid(na), netflow(na), storinit(na), &
                 lzs(na), sumrechrg(na), sumrff(na), rechrg(na), &
                 qlz(na), qdrng(na), qdrngfs(na), qstrm(na), sumq1(na), &
                 sumqint(na), sumq1fs(na), sumqintfs(na), strloss(na), &
                 qdrng2(na), qdrngfs2(na), wetwid(na), chawid(na), &
                 chadep(na), wstore1(na), wstore2(na), wcap(na), &
                 flowxa(na), chaxa(na), satxa(na), wetxa(na), &
                 hcha1(na), hcha2(na), hwet1(na), hwet2(na), qin(na), &
                 qswevp(na), qswrain(na), qiwet1(na), qiwet2(na), &
                 qowet1(na), qowet2(na), wetarea(na), chaarea(na), &
                 bin_precip(na), wsat(na), wetfrac(na), qo2rem(na))
        qi1 = 0.0; qi2 = 0.0; qo1 = 0.0; qo2 = 0.0; qo2sim = 0.0
        qr = 0.0; d2 = 0.0; qda = 0.0; cap = 0.0; over = 0.0
        qmax = 0.0; res = 0; sump = 0.0; store1 = 0.0
        store2 = 0.0; att = 0.0; qbase = 0.0; nreach = 0
        totd1 = 0.0; totuzs = 0.0; totsnw = 0.0; qstream = 0.0
        totchnl = 0.0; totgrid = 0.0; netflow = 0.0; storinit = 0.0
        lzs = 0.0; sumrechrg = 0.0; sumrff = 0.0; rechrg = 0.0
        qlz = 0.0; qdrng = 0.0; qdrngfs = 0.0; qstrm = 0.0; sumq1 = 0.0
        sumqint = 0.0; sumq1fs = 0.0; sumqintfs = 0.0; strloss = 0.0
        qdrng2 = 0.0; qdrngfs2 = 0.0; wetwid = 0.0; chawid = 0.0
        chadep = 0.0; wstore1 = 0.0; wstore2 = 0.0; wcap = 0.0
        flowxa = 0.0; chaxa = 0.0; satxa = 0.0; wetxa = 0.0
        hcha1 = 0.0; hcha2 = 0.0; hwet1 = 0.0; hwet2 = 0.0; qin = 0.0
        qswevp = 0.0; qswrain = 0.0; qiwet1 = 0.0; qiwet2 = 0.0
        qowet1 = 0.0; qowet2 = 0.0; wetarea = 0.0; chaarea = 0.0
        bin_precip = 0.0; wsat = 0.0; wetfrac = 0.0; qo2rem = 0.0

        !> Allocate and assign other variables.
        allocate(nhyd(ycount, xcount), iflowgrid(no), nopt(no))
        iflowgrid = fms%stmg%rnk
        nopt = -1

        !> rev. 9.1.60  Jul.  27/04  - NK: reversed definitions for sl1 & sl2 Int. Slope
        do n = 1, naa

!+            sl2(n) = sqrt(sl1(n))
            if (a4 == 0) a4 = 1.0

            !> CAP IS THE VOLUME OF WATER IN A REACH FOR THE MEAN ANNUAL FLOW.
!            widep = a11

            !> Compute the channel cross-sectional area based on a rather
            !> complicated fitting formula.  aa2/3/4 are tunable parameters.
            if (aa4(n) > 0.0) then
                chaxa(n) = (aa2(n) + aa3(n)*da(n)**aa4(n))
            else

                !> da(n) should never be less than zero, but it can
                !> happen if the rank listing is improperly configured.
                if (da(n) <= 0) then
                    chaxa(n) = 0

                    !> If xxx/yyy are both 0 for this cell, then we have a missing index. In theory,
                    !> this cell shouldn't affect the rest of the computation, so all we really want is for
                    !> the remaidner of this procedure to not die with a floating point exception.
                    if (xxx(n) == 0 .and. yyy(n) == 0) then
                        widep(n) = 1
                        chadep(n) = 1
                    end if

                !> rev. 9.2.12  Sep.  15/05  - NK: added EXCEL eqn to flowinit
                !> EXCEL compatible equation. aa4 must be -ve in the par file
                else
                    chaxa(n) = 10.0**(aa2(n)*alog10(da(n)) + aa3(n))
                end if

                !> had to put a lower bound on channel area to avoid NaN in resume file
                !> NK  Oct. 5/05
                chaxa(n) = amax1(1.0, chaxa(n))

            end if

            !> Channel capacity is the cross-sectional area times channel length.
            cap(n) = chaxa(n)*rl(n)

            !> Since a channel has a deep part plus a shallow, sloping bank,
            !> compute an effective channel depth.
            chadep(n) = sqrt(chaxa(n)/widep(n))
            chawid(n) = chaxa(n)/chadep(n)
            flz2(n) = 1.0 - (1.0 - flz(n))

            !> Fix suggested by Frank Seglenieks, based on changes made
            !> to WATFLOOD ca. 2007: if we keep track of biome types
            !> and a grid cell has more water fraction than channel
            !> area, then the channel area calculation must have been
            !> incorrect -- replace the computed channel area with
            !> water_fracion * grid size, and then from that recompute
            !> capacity.

            !> Define channel area.
            chaarea(n) = chawid(n)*rl(n)

            !> Check to see if that's sensible based on land-use.

            !> aclass(:,ntype) is the fraction of the grid cell that is water;
            !> this is only enforced at read-in of the shed/par files, and
            !> needs to be properly maintained.
            if (ntype >= 1 .and. aclass(n, ntype) > chaarea(n)/grid_area(n)) then

                !> Replace the areas with ones based on the land-use data.
                chaarea(n) = grid_area(n)*aclass(n, ntype)
                chawid(n) = chaarea(n)/rl(n) ! New width using the same effective depth
                cap(n) = chaarea(n)*chadep(n)

                !> Leave chaxa untouched for now, this may be a mistake.
! csubich -- experimental: update chaxa appropriately also
                chaxa(n) = cap(n)/rl(n) ! Capacity divided by length

            end if

        end do !n = 1, naa

        !> What class is the water class?
        ii_water = ntype

        !> Allocate output variable for the driver.
!todo: move this
        stfl%ns = no
        allocate(stfl%qhyd(no), stfl%qsyn(no))
        stfl%qhyd = 0.0
        stfl%qsyn = 0.0

!todo: move this
        open(70, file = './BASINAVG1/MESH_output_streamflow.rte.csv', status = 'unknown', action = 'write')
        write(70, 1010, advance = 'no') 'YEAR', 'DAY'
        do l = 1, fms%stmg%n
            write(70, 1010, advance = 'no') 'QOMEAS', 'QOSIM'
        end do
        write(70, *)

        strfw_option = 'streamflow_comparison'
        allocate(qhyd(1,999999)); qhyd = 0.0
        open(54, file = './BASINAVG1/spl_rpn.rte.csv', action = 'write')

1010    format(9999(g15.7e2, ','))

    end subroutine

    !>
    !> _between_grid() adapted from rte_sub.f.
    !>

    subroutine run_rte_between_grid(shd, wb, stfl, rrls)

        !> area_watflood: Shared variables used throughout rte code.
        use area_watflood

        !> mpi_module: Required for 'ipid'.
        use mpi_module

        !> sa_mesh_shared_variables: Variables, parameters, and types from SA_MESH.
        use sa_mesh_shared_variables

        !> MODEL_OUTPUT: water_balance type for 'wb'.
        use MODEL_OUTPUT

        !> model_output_variabletypes: Streamflow and reservoir output variables for SA_MESH.
        use model_output_variabletypes

        !> Basin properties from SA_MESH.
        type(ShedGridParams) :: shd

        !> Water balance from SA_MESH (includes runoff).
        type(water_balance) :: wb

        !> Streamflow and reservoir output variables for SA_MESH.
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables for dynamic time-stepping.
        real(kind = 4) qi2_strt(naa), qo2_strt(naa), store2_strt(naa), route_dt, hr_div, sec_div, dtmin
        real tqi1, tqo1, tax, tqo2, tstore2, tstore1
        integer indexi, no_dtold

        !> Local variables for the call to baseflow.
        real(kind = 4) sdlz, dlz

        !> Local diagnostic variables.
!        integer year_last, month_last, day_last, hour_last
        integer :: exitstatus = 0

        !> Local variables.
        integer n, l

        !> Local variables not used.
        character(len = 14) :: date = ''

        !> Return if not the head node or if the process is not active.
        if (ipid /= 0 .or. .not. rteflg%PROCESS_ACTIVE) return

!todo: move this
        if (mod(ic%now%hour, 24) == 0 .and. ic%now%mins == 0) then
            read(22, *) (stfl%qhyd(l), l = 1, no)
        end if

        !> Accumulate runoff to the routing time-step.
        if (ic%ts_hourly == 1) then
            qr(1:naa) = 0.0
        end if
        qr(1:naa) = qr(1:naa) + (wb%rofo(1:naa) + wb%rofs(1:naa))

        !> Recharge accumulates perpetually.
        lzs(1:naa) = lzs(1:naa) + wb%rofb(1:naa)/shd%FRAC(1:NAA)

        !> Return if no the last time-step of the hour.
        if ((mod(ic%ts_hourly, 3600/ic%dts) /= 0)) then
            return
        end if

        !> Increment counters.
        fhr = fhr + 1

        !> Date
        year1 = ic%now%year
        month_now = ic%now%month
        day_now = ic%now%day
        hour_now = ic%now%hour + 1

        !> Convert surface runoff from [kg/m^2] to [cms].
        qr(1:naa) = qr(1:naa)*1000.0*step2/3600.0

        !> Route the recharge thru the lower zone.
        do n = 1, naa
            call baseflow(n, dlz, sdlz, (1000.0*step2/3600.0))
        end do

        !> baseflow gives us qlz in flow units (cms), not in mm
        !> so there is no need to convert before adding to qr.
        qr(1:naa) = qr(1:naa) + qlz(1:naa)

        !> Remember the input values from the start of the time step.
        qi2_strt(1:naa) = qi2(1:naa)
        qo2_strt(1:naa) = qo2(1:naa)
        store2_strt(1:naa) = store2(1:naa)

        !> If flow insertion, use simulated instead of flow inserted value at gauge location.
        if (trim(strfw_option) == 'streamflow_insertion') then
            do l = 1, no
                n = fms%stmg%rnk(l)
                qo2_strt(n) = qo2sim(n)
            end do
        end if

        !> Calculate a dynamic dtmin by doing a quick and dirty routing
        !> This helps to ensure that the routing solutions inside the route function
        !> are able to converge to a solution
        !> We are assuming that the channels are not overflowing, so if it turns out that
        !> there is overbank flow/storage, then route may have to iterate a bit longer to
        !> find the solution even if dtmin is relatively small
!DD  Override the value calculated above by timer.f (fixed for all hours)
        dtmin = dtminusr  ! specified by the user and read in by rte.f
!        dtminfrac = 0.75
!        maxindex = 50   ! Permit more loops in this version of the routing loop since qo2 has no weighting applied

        do n = 1, naa

            tqo1 = qo2(n)

            !> If flow insertion, rely on simulated instead of possibly
            !> inserted flow value at gauge location to estimate required time-step.
            if (trim(strfw_option) == 'streamflow_insertion') then
                do l = 1, no
                    if (iflowgrid(l) == n) then
                        tqo1 = qo2sim(n)
                    end if
                end do
            end if
!            tqo1 = 0.0

            tqi1 = qi2(n)
15          indexi = 0
            if (dtmin <= mindtmin) exit
            no_dt = max(int(3599.0/dtmin) + 1, 1)
            route_dt = 3600.0/float(no_dt)
            sec_div = route_dt/2.0
            tax = store1(n)/rl(n)
!            tqo2 = 0.0
            tqo2 = max(tax, 0.0)**1.67*slope(n)/chawid(n)**0.667/r2n(n)

            !> Use qi2 = 0.0 below to really constrain dtmin by keeping store2 low
            !> We don't want to set qi2 to zero though because it is used in route
            !> so we just use a hard-coded 0.0 in this equation
!16          tstore2 = store1(n)
16          tstore2 = store1(n) + (tqi1 + 0.0 - tqo1 - tqo2)*sec_div

            !> Now check to see if this qo2 is large enough that it will cause problems
            !> in the next time step when it is put into qo1.
            !> This has been known to happen when there is a sudden reduction in qi2 compared to qi1
!            tstore1 = tstore2
            tstore1 = tstore2 + (-tqo2)*sec_div

            !> If qo2 is so large that it's emptying the grid cell's storage in one time
            !> step, then we need to reduce the size of the time step to prevent that from
            !> happening. This is analogous to meeting the CFL condition in advection solvers.
            !> However, if store1 was very small, then small or even slightly negative store2
            !> might be a legitimate solution (i.e. the cell has actually dried up). So we'll let that go.
            !> Note that we also need to reduce the time step if we anticipate that qo1 will be too large
            !> in our next forecast/analysis time.
            if (tstore2 < 0.0 .or. tstore1 < 0.0) then

                !> Keep making qo2 smaller untill store2 becomes positive.
                tqo2 = tqo2/2.0
                indexi = indexi + 1

                !> Reduce the time step by a factor of dtminfrac (default = 0.75, set above).
                if (indexi > maxindex) then
                    dtmin = dtmin*dtminfrac
                    go to 15
                end if

                !> Redo the store2 calculation within the same iteration.
                go to 16
            end if

        end do !n = 1, naa

        !> Re-specifying dtmin as dtminusr cancels the effect of the code immediately above.
        !> However, if the iteration loop in route is unstable, dtmin still decreases.
        dtmin = dtminusr

        !> Let the time step be as small as mindtmin.
17      dtmin = max(mindtmin, dtmin)
        no_dt = max(int(3599.0/dtmin) + 1, 1)
        dtmin = 3600.0/real(no_dt)
        route_dt = 3600.0/float(no_dt)
        sec_div = route_dt/2.0
        hr_div = sec_div/3600.0
        exitstatus = 0

        !> Override the value declared above (fixed for all hours).
        a6 = dtmin

        !> The value of dtmin has been used to determine how many
        !> times route is called. Route will determine a new dtmin
        !> for the next time step.

!EG_MOD prepare arrays for storing average flows
        if (.not. allocated(avr_qo)) allocate(avr_qo(na))
        avr_qo = 0.0

        !> ROUTE ROUTES WATER THROUGH EACH SQUARE USING STORAGE ROUTING.
        !> rev. 9.3.11  Feb.  17/07  - NK: force hourly time steps for runoff
        !> rev. 9.3.12  Feb.  20/07  - NK: changed dtmin & call to route
        do n = 1, no_dt

            call route(sec_div, hr_div, dtmin, mindtmin, convthreshusr, (ic%count_hour + 1), n, real(ic%ts_count - 1), &
                       date, exitstatus)

            if (exitstatus /= 0) then
                if (dtmin <= mindtmin) then
                    write(*, '(a15, a43, f6.1)') 'route yields a ', 'negative store2 value at dtmin = mindtmin: ', mindtmin
                    write(*, '(a25, a50)') 'It''s likely that qo1 is ', 'so large that store2 is negative even with qo2=0.0'
                    write(*, '(a29, a50, a50)') &
                        'If this run was started from ', 'shed2flowinit utility, then try lowering the QI, ', &
                        'QO, and STORE ratios until this error is resolved'
                    write(*, *) 'Else rerun with a smaller value of dtmin'
                    stop
                else

                    !> Reduce the time step by a factor of dtminfrac (default = 0.75, set above).
                    dtmin = dtmin*dtminfrac
                    no_dtold = no_dt
                    no_dt = max(int(3599.0/dtmin) + 1, 1)

                    !> Reduce dtmin sufficiently to decrease no_dt.
                    do while (no_dt <= no_dtold)
                        dtmin = dtmin*dtminfrac
                        no_dtold = no_dt
                        no_dt = max(int(3599.0/dtmin) + 1, 1)
                    end do
                    dtmin = 3600.0/real(no_dt)

                    !> Restore the input values from the start of the time step.
                    qi2(1:naa) = qi2_strt(1:naa)
                    qo2(1:naa) = qo2_strt(1:naa)
                    store2(1:naa) = store2_strt(1:naa)
                    go to 17

                end if
            end if

        end do !n = 1, no_dt

        !> Remember the last processed date/time in case need to output flow ICs after the time loop.
!        year_last = year1
!        month_last = month_now
!        day_last = day_now
!        hour_last = hour_now

        !> Return average streamflow value to SA_MESH.
        do l = 1, no
            stfl%qsyn(l) = qo2(fms%stmg%rnk(l))
        end do

!todo: move this
        if ((mod(ic%ts_daily, 3600/ic%dts*24) == 0)) then
            write(70, 1010, advance = 'no') ic%now%year, ic%now%jday
            do l = 1, fms%stmg%n
                write(70, 1010, advance = 'no') stfl%qhyd(l), stfl%qsyn(l)
            end do
            write(70, *)
        end if

1010    format(9999(g15.7e2, ','))

    end subroutine

end module
