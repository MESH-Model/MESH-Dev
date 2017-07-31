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
        real, dimension(:), allocatable :: r1n, r2n, mndr, aa2, aa3, aa4, widep
    end type
    real, dimension(:), allocatable :: avr_qo2

    !> Instances of the input parameters.
    !*  rtepm: Grid-based (GRD, NA/NAA).
    !*  rtepm_iak: River class-based (IAK).
    type(rte_params), save :: rtepm, rtepm_iak

    !> Configuration flags.
    type rte_flags
        logical :: PROCESS_ACTIVE = .false.
        integer :: cap_shd = 0
    end type

    !> Instance of control flags.
    type(rte_flags), save :: rteflg

    !> Option flags.
    type rte_options

    end type

    !> Instance of option flags.
    type(rte_options), save :: rteops

!temp: Override for diversions.
    real, dimension(:), allocatable, save :: qdiv2
!    character*12, dimension(4), save :: &
!        tdivname = ['06EC002', 'irrigat', '05LL019', '05QB005']
!    real*4, dimension(4), save :: &
!        txstrdiv = [-999.0000, -107.4900, -98.4045, -999.0000], &
!        tystrdiv = [-999.0000, 52.0640, 50.0134, -999.0000], &
!        txenddiv = [-98.9810, -999.9900, -98.3956, -91.3572], &
!        tyenddiv = [56.6906, -999.9900, 50.2219, 50.9251]
!    integer*4, dimension(4), save :: &
!        tval1div = [1.0, 0.5, 1.0, 1.0], &
!        tval2div = [3, 2, 1, 3], &
!        tval3div = [0, 11, 0, 0], &
!        tval4div = [0, 11, 0, 0]

!temp: Override for irrigation
    integer, dimension(:), allocatable :: totirrigpts

    contains

    !>
    !> _init() adapted from read_shed_fst_mod.f90.
    !>

    subroutine run_rte_init(fls, shd, stfl, rrls)

        !> area_watflood: Shared variables used throughout rte code.
        use area_watflood

        !> mpi_module: Required for 'ipid'.
        use mpi_module

        !> sa_mesh_shared_variables: Variables, parameters, and types from SA_MESH.
        use model_files_variables
        use sa_mesh_shared_variables
        use FLAGS

        !> model_output_variabletypes: Streamflow and reservoir output variables for SA_MESH.
        use model_output_variabletypes

        type(fl_ids) :: fls

        !> Basin properties from SA_MESH.
        type(ShedGridParams) :: shd

        !> Streamflow and reservoir output variables for SA_MESH.
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer n, l
        real ry, rx

!temp: for overrides
        real, dimension(:, :), allocatable :: datarr
        integer j, i
        character(len = 40) in_line
        character(len = 4) ffmti
        integer, dimension(4) :: jstrdiv, istrdiv, jenddiv, ienddiv
        integer, dimension(:), allocatable :: iminirr, imaxirr, jminirr, jmaxirr
        integer irindex, irpt

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
        Nreaches = maxval(shd%IREACH)
        grid_area = shd%AREA
        frac = shd%FRAC
        aclass = shd%lc%ACLASS
        glacier_flag = 'n'

        !> Allocate and assign parameter values.
        allocate(r1n(na), r2n(na), rlake(na), &
                 mndr(na), aa2(na), aa3(na), aa4(na), theta(na), widep(na), kcond(na))
        r1n = rtepm%r1n; r2n = rtepm%r2n
        mndr = rtepm%mndr; aa2 = rtepm%aa2; aa3 = rtepm%aa3; aa4 = rtepm%aa4; widep = rtepm%widep
        rlake = 0.0; theta = 0.0; kcond = 0.0

!temp: Override
!        aa2 = 0.1; aa3 = 0.9; aa4 = 0.67
!        widep = 20.0
!        naa = 5980
!        allocate(datarr(shd%xCount, shd%yCount))
!        datarr = 0.0
!        open(600, file = 'r1n_running_avg.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            r1n(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do
!        datarr = 0.0
!        open(600, file = 'r2n_running_avg.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            r2n(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do
!        deallocate(datarr)

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
                 sumrechrg(na), sumrff(na), rechrg(na), &
                 qdrng(na), qdrngfs(na), qstrm(na), sumq1(na), &
                 sumqint(na), sumq1fs(na), sumqintfs(na), strloss(na), &
                 qdrng2(na), qdrngfs2(na), wetwid(na), chawid(na), &
                 chadep(na), wstore1(na), wstore2(na), wcap(na), &
                 flowxa(na), chaxa(na), satxa(na), wetxa(na), &
                 hcha1(na), hcha2(na), hwet1(na), hwet2(na), qin(na), &
                 qswevp(na), qswrain(na), qiwet1(na), qiwet2(na), &
                 qowet1(na), qowet2(na), wetarea(na), chaarea(na), &
                 bin_precip(na), wsat(na), wetfrac(na), qo2rem(na), &
!tied to irr
                 qo2remirr(na))
        qi1 = 0.0; qi2 = 0.0; qo1 = 0.0; qo2 = 0.0; qo2sim = 0.0
        qr = 0.0; d2 = 0.0; qda = 0.0; cap = 0.0; over = 0.0
        qmax = 0.0; res = 0; sump = 0.0; store1 = 0.0
        store2 = 0.0; att = 0.0; qbase = 0.0; nreach = 0
        totd1 = 0.0; totuzs = 0.0; totsnw = 0.0; qstream = 0.0
        totchnl = 0.0; totgrid = 0.0; netflow = 0.0; storinit = 0.0
        sumrechrg = 0.0; sumrff = 0.0; rechrg = 0.0
        qdrng = 0.0; qdrngfs = 0.0; qstrm = 0.0; sumq1 = 0.0
        sumqint = 0.0; sumq1fs = 0.0; sumqintfs = 0.0; strloss = 0.0
        qdrng2 = 0.0; qdrngfs2 = 0.0; wetwid = 0.0; chawid = 0.0
        chadep = 0.0; wstore1 = 0.0; wstore2 = 0.0; wcap = 0.0
        flowxa = 0.0; chaxa = 0.0; satxa = 0.0; wetxa = 0.0
        hcha1 = 0.0; hcha2 = 0.0; hwet1 = 0.0; hwet2 = 0.0; qin = 0.0
        qswevp = 0.0; qswrain = 0.0; qiwet1 = 0.0; qiwet2 = 0.0
        qowet1 = 0.0; qowet2 = 0.0; wetarea = 0.0; chaarea = 0.0
        bin_precip = 0.0; wsat = 0.0; wetfrac = 0.0; qo2rem = 0.0
        qo2remirr = 0.0

        !> Allocate and assign other variables.
        allocate(store2_strt(naa), nhyd(ycount, xcount), iflowgrid(no), nopt(no))
        store2_strt = 0.0
        iflowgrid = fms%stmg%meta%rnk
        nopt = -1

        !> rev. 9.1.60  Jul.  27/04  - NK: reversed definitions for sl1 & sl2 Int. Slope
        do n = 1, naa

!+            sl2(n) = sqrt(sl1(n))
            if (a4 == 0) a4 = 1.0

            !> CAP IS THE VOLUME OF WATER IN A REACH FOR THE MEAN ANNUAL FLOW.
!            widep = a11

            !> Set 'chaxa' to the bankfull area, or else
            !> Compute the channel cross-sectional area based on a rather
            !> complicated fitting formula.  aa2/3/4 are tunable parameters.
            if (rteflg%cap_shd == 1) then
                chaxa(n) = bnkfll(n)
            else if (aa4(n) > 0.0) then
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

!temp: override for initial states
!        allocate(datarr(shd%xCount, shd%yCount))

!        datarr = 0.0
!        open(600, file = 'QI1_012.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            qi1(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do
!        qi2 = qi1
!        datarr = 0.0
!        open(600, file = 'QO1_012.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            qo1(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do
!        qo2 = qo1
!        datarr = 0.0
!        open(600, file = 'STOR_012.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            store1(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do
!        store2 = store1

!        datarr = 0.0
!        open(600, file = 'OVER_012.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            over(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do
!        datarr = 0.0
!        open(600, file = 'LZS_012.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            lzs(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do

!        datarr = 0.0
!        open(600, file = 'QO1_012_10.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            qo2sim(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do
!        datarr = 0.0
!        open(600, file = 'QO1_012_20.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            qo2rem(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do
!        datarr = 0.0
!        open(600, file = 'QO1_012_30.grid')
!        do i = 1, shd%yCount
!            read(600, *) (datarr(j, i), j = 1, shd%xCount)
!        end do
!        close(600)
!        do n = 1, naa
!            qo2remirr(n) = datarr(shd%xxx(n), shd%yyy(n))
!        end do

!        deallocate(datarr)

!        resumflg = 'y'

!temp: override to read from file
!        open(602, file = 'RFF.grid')
!        open(603, file = 'RCH.grid')

        ! Reservoirs.
        noresv = fms%rsvr%n
        Nreaches = noresv
        if (fms%rsvr%n > 0) then
            ktr = fms%rsvr%qorls%dts
            allocate( &
                b1(noresv), b2(noresv), b3(noresv), &
                b4(noresv), b5(noresv), ires(noresv), jres(noresv), &
                b6(noresv), b7(noresv), &
!                yres(noresv), xres(noresv), poliflg(noresv), &
                resname(noresv), &
                qrel(noresv, 999999), &
                qdwpr(noresv, 999999), lake_elv(noresv, 999999), &
                lake_inflow(noresv, 999999), &
    !tied to val2divyes == 1
                resindex(noresv), &
    !tied to fhr
                lake_stor(noresv, 999999), lake_outflow(noresv, 999999), &
                del_stor(noresv, 999999))
!                qstream_sum(noresv, 999999), strloss_sum(noresv, 999999))
            qrel(:, 1) = fms%rsvr%qorls%val
            qdwpr = 0.0; lake_elv = 0.0
            lake_inflow = 0.0
!tied to fhr
            lake_stor = 0.0; lake_outflow = 0.0
            del_stor = 0.0
!            qstream_sum = 0.0; strloss_sum = 0.0
            resname = fms%rsvr%meta%name
            b1 = fms%rsvr%rls%b1
            b2 = fms%rsvr%rls%b2
            b3 = fms%rsvr%rls%b3
            b4 = fms%rsvr%rls%b4
            b5 = fms%rsvr%rls%b5
            b6 = fms%rsvr%rls%area
            b7 = fms%rsvr%rls%lvlz0
!            where (b3 == 0.0)
!                poliflg = 'n'
!            elsewhere
!                poliflg = 'y'
!            end where
            jres = fms%rsvr%meta%jx
            ires = fms%rsvr%meta%iy
            resindex = fms%rsvr%meta%rnk

            !> Initial reservoir storage.
            allocate(reach_last(noresv))
            do l = 1, noresv
                reach_last(l) = lake_elv(l, 1)
                n = resindex(l)
                if (b6(l) > 0.0) store1(n) = max(0.0, reach_last(l) - b7(l))*b6(l)
                store2(n) = store1(n)
            end do

        end if

!temp: Override for diversions.
!        nodiv = 4
!        allocate( &
!            val1div(nodiv), val2div(nodiv), val3div(nodiv), val4div(nodiv), &
!            divstrindex(nodiv), divendindex(nodiv), divname(nodiv), qdiv(nodiv, 999999), qdiv2(nodiv))
!        val1div = 0.0; val2div = 0; val3div = 0; val4div = 0
!        istrdiv = 0; jstrdiv = 0
!        ienddiv = 0; jenddiv = 0
!        divstrindex = 0; divendindex = 0; divname = ''; qdiv = 0.0
!        divname = tdivname
!        val1div = tval1div
!        val2div = tval2div
!        if (any(val2div > 0)) val2divyes = 1
!        val3div = tval3div
!        val4div = tval4div
!        jstrdiv = int((txstrdiv - xorigin)/xdelta) + 1
!        istrdiv = int((tystrdiv - yorigin)/ydelta) + 1
!        jenddiv = int((txenddiv - xorigin)/xdelta) + 1
!        ienddiv = int((tyenddiv - yorigin)/ydelta) + 1
!        divstrindex = -1 ! Points outside the watershed: reset the index value to -1.
!        divendindex = -1
!        do l = 1, nodiv
!            do n = 1, naa
!                if (xxx(n) == jstrdiv(l) .and. yyy(n) == istrdiv(l)) then
!                    divstrindex(l) = n
!                end if
!                if (xxx(n) == jenddiv(l) .and. yyy(n) == ienddiv(l)) then
!                    divendindex(l) = n
!                end if
!            end do
!            if ((val2div(l) == 1 .and. divstrindex(l) == -1) .or. (val2div(l) == 1 .and. divendindex(l) == -1)) then
!                print *, 'Error for station ', l, ': incompatibility between value2 and location of stations.'
!                print *, 'Check lat-lon of this station.'
!                print *, 'DIVR ', l, ' type ', val2div(l), ' START ', divstrindex(l), ' END', divendindex(l)
!                stop
!            end if
!            if ((val2div(l) == 2 .and. divstrindex(l) == -1) .or. (val2div(l) == 2 .and. divendindex(l) /= -1)) then
!                print *, 'Error for station ', l, ': incompatibility between value2 and location of stations.'
!                print *, 'Check lat-lon of this station.'
!                print *, 'DIVR ', l, ' type ', val2div(l), ' START ', divstrindex(l), ' END', divendindex(l)
!                stop
!            end if
!            if ((val2div(l) == 3 .and. divendindex(l) == -1) .or. (val2div(l) == 3 .and. divstrindex(l) /= -1)) then
!                print *, 'Error for station ', l, ': incompatibility between value2 and location of stations.'
!                print *, 'Check lat-lon of this station.'
!                print *, 'DIVR ', l, ' type ', val2div(l), ' START ', divstrindex(l), ' END', divendindex(l)
!                stop
!            end if
!        end do

!temp: Override for irrigation.
!        nodivirrig = 0                          ! The number of irrigation regions
!        maxirrigpts = 0                         ! The largest number of points in an individual irrigation region
!        do l = 1, nodiv                         ! Determine how many regions are to be irrigated
!            if (divname(l)(1:5) == 'irrig' .or. divname(l)(1:5) == 'Irrig') then
!                if (val2div(l) /= 2) then
!                    print *, 'Irrigation station must be type 2 diversion'
!                    print *, 'DIVR ', l, ' type ', val2div(l)
!                    stop
!                end if
!                nodivirrig = nodivirrig + 1         ! The maximum number of points to be irrigated
!                maxirrigpts = max(maxirrigpts, (val3div(l) + 1)*(val4div(l) + 1)) ! Add an extra point in each direction: needed if value3 or value4 is an even number
!            end if
!        end do
!        if (nodivirrig > 0) then
!            !   irrigindx: the indices (1-naa) of points to be irrigated
!            !   qdivirrig: the volume (m3/s) of water to be removed from the irrigated point in the simulation timestep
!            allocate( &
!                totirrigpts(nodivirrig), &
!                iminirr(nodivirrig), imaxirr(nodivirrig), &
!                jminirr(nodivirrig), jmaxirr(nodivirrig), &
!                irrigindx(nodivirrig, maxirrigpts), &
!                qdivirrig(nodivirrig, 999999))
!            irrigindx = -1
!            qdivirrig = 0
!
!            ! Determine the grid points in which water is to be lost due to irrigation
!            irindex = 0
!            do l = 1, nodiv
!                if (divname(l)(1:5) == 'irrig' .or. divname(l)(1:5) == 'Irrig') then
!                    irindex = irindex + 1
!                    iminirr(irindex) = max(istrdiv(l) - val4div(l)/2, 0)  ! Define the rectangle from the provided values
!                    imaxirr(irindex) = min(istrdiv(l) + val4div(l)/2, ycount)
!                    jminirr(irindex) = max(jstrdiv(l) - val3div(l)/2, 0)
!                    jmaxirr(irindex) = min(jstrdiv(l) + val3div(l)/2, xcount)
!                    irpt = 0
!                    do i = iminirr(irindex), imaxirr(irindex) ! Determine the number of points in this rectangle that are also in the watershed
!                        do j = jminirr(irindex), jmaxirr(irindex)
!                            do n = 1, naa
!                                if(xxx(n) == j .and. yyy(n) == i) then
!                                    irpt = irpt + 1
!                                    irrigindx(irindex, irpt) = n
!                                    exit
!                                end if
!                            end do
!                        end do
!                    end do
!                    totirrigpts(irindex) = irpt
!                end if
!            end do
!        end if
!        open(67, file = '20121201_div.tb0')
!        in_line = ''
!        do while (in_line /= ':EndHeader')
!            read(67, '(a)') in_line
!        end do
!        if (nodiv > 0) then
!            do i = 1, (30*24 + 13)
!                read(67, *) (qdiv(l, 1), l = 1, nodiv)
!            end do
!            backspace(67)
!            do l = 1, nodiv ! Loop through the diversion names searching for an irrigation area
!                if (qdiv(l, 1) < 0.0 .and. val2div(l) == 3) then
!                    print *, 'Diversion is type 3 (diversion starts outside of watershed).'
!                    print *, 'The diversion value cannot be negative for type 3.'
!                    print *, 'DIVR ', l, ' type ', val2div(l), ' qdiv ', qdiv(l, 1)
!                    stop
!                end if
!            end do
!        end if

        !> Allocate output variable for the driver.
!todo: move this
        stfl%ns = no
        allocate(stfl%qhyd(no), stfl%qsyn(no))
        stfl%qhyd = 0.0
        stfl%qsyn = 0.0

!        strfw_option = 'streamflow_comparison'
!todo: fix this.
        allocate(qhyd(no, 999999)); qhyd = 0.0
!        open(54, file = './spl_rpn.rte.csv', action = 'write')
!        write(54, '(a)') 'Observed and simulated streamflows (m^3/s)'
!        write(54, '(a)') 'Station,,,,05GG001 ,,05HG001 ,,05KD003 ,,05KJ001 ,,05KL001 ,,05MH005 ,,05MJ001 ,,05OC001 ,,' // &
!                         '05OC012 ,,05OJ005 ,,05PF063 ,,05PF068 ,,05PF069 ,,05UE005 ,,05UF006 ,,05UF007 ,,05LM001 ,,05PE020 ,,' // &
!                         '05LH005 ,,05UB008 ,,05AJ001 ,,05AA024 ,,05AB021 ,,05AD007 ,,05AE027 ,,05AG006 ,,05BA001 ,,05BB001 ,,' // &
!                         '05BE004 ,,05BH004 ,,05BH008 ,,05BL024 ,,05BL024 ,,05BM002 ,,05BM004 ,,05BM004 ,,05BN012 ,,05CA009 ,,' // &
!                         '05CA009 ,,05CC002 ,,05CC007 ,,05CE001 ,,05CK004 ,,05DA009 ,,05DB006 ,,05DC001 ,,05DC010 ,,05DC011 ,,' // &
!                         '05DD005 ,,05DD007 ,,05DF001 ,,05EA001 ,,05EE007 ,,05EE009 ,,05EF001 ,,05FA001 ,,05FC001 ,,05FE004 ,,' // &
!                         '05HD036 ,,05HD039 ,,05KE010 ,'
!        write(54, '(a)') 'Longitude,,,,-105.631,,-106.504,,-103.157,,-101.068,, -99.231,, -98.749,, -97.265,, -97.074,,' // &
!                         ' -97.045,, -96.724,, -95.430,, -95.856,, -96.037,, -96.576,, -94.493,, -94.229,, -98.537,, -94.603,,' // &
!                         ' -99.379,, -97.976,,-111.056,,-113.860,,-113.578,,-112.876,,-113.302,,-111.755,,-116.112,,-115.546,,' // &
!                         '-114.992,,-114.001,,-114.001,,-113.714,,-113.714,,-113.582,,-112.452,,-112.452,,-111.468,,-115.010,,' // &
!                         '-115.010,,-113.716,,-114.139,,-112.602,,-110.205,,-116.530,,-114.702,,-114.844,,-116.329,,-115.990,,' // &
!                         '-115.407,,-116.674,,-113.584,,-113.300,,-110.481,,-111.888,,-109.500,,-113.575,,-112.310,,-109.931,,' // &
!                         '-108.367,,-107.524,,-104.575,'
!        write(54, '(a)') 'Latitude,,,,  53.384,,  52.321,,  53.883,,  54.018,,  53.268,,  49.879,,  50.049,,  49.189,,' // &
!                         '  49.734,,  50.326,,  50.405,,  50.578,,  50.748,,  56.146,,  56.561,,  56.578,,  51.824,,  50.198,,' // &
!                         '  52.001,,  54.351,,  50.204,,  49.668,,  50.201,,  49.843,,  48.949,,  50.032,,  51.654,,  51.289,,' // &
!                         '  51.288,,  51.290,,  51.290,,  50.930,,  50.930,,  50.926,,  50.932,,  50.932,,  50.211,,  51.812,,' // &
!                         '  51.812,,  52.376,,  52.374,,  51.650,,  51.106,,  52.013,,  52.375,,  52.556,,  52.194,,  52.369,,' // &
!                         '  53.088,,  52.915,,  53.632,,  53.990,,  53.632,,  53.454,,  53.812,,  52.912,,  52.917,,  53.092,,' // &
!                         '  49.846,,  50.574,,  53.991,'
!        write(54, '(a)') 'Xcoord_grid,      86,,      80,,     104,,     119,,     132,,     135,,     146,,     147,,' // &
!                         '     147,,     150,,     159,,     156,,     155,,     151,,     166,,     167,,     137,,     165,,' // &
!                         '     131,,     141,,      48,,      28,,      30,,      35,,      32,,      43,,      12,,      16,,' // &
!                         '      20,,      27,,      27,,      29,,      29,,      30,,      38,,      38,,      45,,      20,,' // &
!                         '      20,,      29,,      26,,      37,,      54,,       9,,      22,,      21,,      10,,      13,,' // &
!                         '      17,,       8,,      30,,      32,,      52,,      42,,      59,,      30,,      39,,      56,,' // &
!                         '      67,,      73,,      94,'
!        write(54, '(a)') 'Ycoord_grid,,,,      47,,      41,,      49,,      50,,      46,,      27,,      28,,      23,,' // &
!                         '      26,,      30,,      30,,      31,,      32,,      62,,      64,,      64,,      38,,      29,,' // &
!                         '      39,,      52,,      29,,      26,,      29,,      27,,      22,,      28,,      37,,      35,,' // &
!                         '      35,,      35,,      35,,      33,,      33,,      33,,      33,,      33,,      29,,      38,,' // &
!                         '      38,,      41,,      41,,      37,,      34,,      39,,      41,,      42,,      40,,      41,,' // &
!                         '      45,,      44,,      48,,      50,,      48,,      47,,      49,,      44,,      44,,      45,,' // &
!                         '      27,,      31,,      50,'
!        write(54, '(a)') 'YEAR,MONTH,DAY,HOUR,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,' // &
!                         'SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,' // &
!                         'OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,' // &
!                         'SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,' // &
!                         'OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,OBS,SIM,' // &
!                         'OBS,SIM,OBS,SIM'

        !> Open output files for streamflow.
!todo: move this.
        open(70, file = './' // trim(fls%GENDIR_OUT) // '/' // 'MESH_output_streamflow.csv', status = 'unknown', action = 'write')
        write(70, 1010, advance = 'no') 'YEAR', 'DAY'
        do l = 1, fms%stmg%n
            write(ffmti, '(i3)') l
            write(70, 1010, advance = 'no') 'QOMEAS' // trim(adjustl(ffmti)), 'QOSIM' // trim(adjustl(ffmti))
        end do
        write(70, *)

1010    format(9999(g15.7e2, ','))

    end subroutine

    !>
    !> _between_grid() adapted from rte_sub.f.
    !>

    subroutine run_rte_between_grid(fls, shd, wb, stfl, rrls)

        !> area_watflood: Shared variables used throughout rte code.
        use area_watflood

        !> mpi_module: Required for 'ipid'.
        use mpi_module

        !> sa_mesh_shared_variables: Variables, parameters, and types from SA_MESH.
        use model_files_variables
        use sa_mesh_shared_variables

        !> MODEL_OUTPUT: water_balance type for 'wb'.
        use MODEL_OUTPUT

        !> model_output_variabletypes: Streamflow and reservoir output variables for SA_MESH.
        use model_output_variabletypes

        type(fl_ids) :: fls

        !> Basin properties from SA_MESH.
        type(ShedGridParams) :: shd

        !> Water balance from SA_MESH (includes runoff).
        type(water_balance) :: wb

        !> Streamflow and reservoir output variables for SA_MESH.
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables for dynamic time-stepping.
        real(kind = 4) qi2_strt(naa), qo2_strt(naa), route_dt, hr_div, sec_div, dtmin
        real tqi1, tqo1, tax, tqo2, tstore2, tstore1
        integer indexi, no_dtold

        !> Local diagnostic variables.
!        integer year_last, month_last, day_last, hour_last
        integer :: exitstatus = 0

        !> Local variables.
        integer n, l

!temp: override to read from file
        real, dimension(:, :, :), allocatable :: datarr
        integer j, i, irindex

        !> Local variables not used.
        character(len = 14) :: date = ''

        !> Return if not the head node or if the process is not active.
        if (ipid /= 0 .or. .not. rteflg%PROCESS_ACTIVE) return

!todo: move this
!-        if (mod(ic%now%hour, fms%stmg%qomeas%dts) == 0 .and. ic%now%mins == 0) then
!-            read(22, *) (fms%stmg%qomeas%val(l), l = 1, fms%stmg%n)
!-            stfl%qhyd = fms%stmg%qomeas%val
!-        end if

        !> Accumulate runoff to the routing time-step.
        if (ic%ts_hourly == 1) then
            qr(1:naa) = 0.0
        end if
        qr(1:naa) = qr(1:naa) + (wb%rofo(1:naa) + wb%rofs(1:naa) + wb%rofb(1:naa))

        !> Return if no the last time-step of the hour.
        if (mod(ic%ts_hourly, 3600/ic%dts) /= 0) then
            return
        end if

!temp: override to read from file
!        allocate(datarr(shd%xCount, shd%yCount, 2))
!        read(602, *)
!        read(603, *)
!        do i = 1, shd%yCount
!            read(602, *) (datarr(j, i, 1), j = 1, shd%xCount)
!            read(603, *) (datarr(j, i, 2), j = 1, shd%xCount)
!        end do
!        do n = 1, naa

            !> Pass all values of surface runoff (RFF) within a reach and positive values elsewhere to qr.
            !> Remember negative values of RFF outside of reaches and add to lzs.
!            if (ireach(n) <= 0 .and. datarr(shd%xxx(n), shd%yyy(n), 1) < 0.0) then
!                qr(n) = 0.0
!                lzs(n) = lzs(n) + datarr(shd%xxx(n), shd%yyy(n), 2) + datarr(shd%xxx(n), shd%yyy(n), 1)
!            else
!                qr(n) = datarr(shd%xxx(n), shd%yyy(n), 1)
!                lzs(n) = lzs(n) + datarr(shd%xxx(n), shd%yyy(n), 2)
!            end if
!        end do

        !> Increment counters.
        fhr = fhr + 1

        !> Diversion data.
        if (nodiv > 0) then
            read(67, *) (qdiv2(l), l = 1, nodiv)
            do l = 1, nodiv
                if (qdiv2(l) < 0.0 .and. val2div(l) == 3) then
                    print *, 'Diversion is type 3 (diversion starts outside of watershed).'
                    print *, 'The diversion value cannot be negative for type 3.'
                    print *, 'DIVR ', l, ' type ', val2div(l), ' qdiv ', qdiv2(l)
                    stop
                else if (qdiv2(l) < 0.0) then
                    qdiv2(l) = 0.0
                end if
                irindex = 0
                if (divname(l)(1:5) == 'irrig' .or. divname(l)(1:5) == 'Irrig') then
                    irindex = irindex + 1
                    qdivirrig(irindex, 1) = qdiv(l, 1)/totirrigpts(irindex)
                end if
            end do
            qdiv(:, fhr) = qdiv2
        end if

        !> Date
        year1 = ic%now%year
        month_now = ic%now%month
        day_now = ic%now%day
        hour_now = ic%now%hour + 1

        !> Convert surface runoff from [kg/m^2] to [cms].
        qr(1:naa) = qr(1:naa)*1000.0*step2/3600.0

        !> Remember the input values from the start of the time step.
        qi2_strt(1:naa) = qi2(1:naa)
        qo2_strt(1:naa) = qo2(1:naa)
        store2_strt(1:naa) = store2(1:naa)
        if (fms%stmg%n > 0) qhyd(:, fhr) = fms%stmg%qomeas%val
        if (fms%rsvr%n > 0) qrel(:, fhr) = fms%rsvr%qorls%val

        !> If flow insertion, use simulated instead of flow inserted value at gauge location.
        if (trim(strfw_option) == 'streamflow_insertion') then
            do l = 1, no
                n = fms%stmg%meta%rnk(l)
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
        if (.not. allocated(avr_qo2)) allocate(avr_qo2(na))
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

        !> Accumulate.
        avr_qo2 = avr_qo2 + avr_qo

        !> Return the daily average value of streamflow value to SA_MESH in the last time-step of the day.
!todo: preserve per time-step value.
        if (mod(ic%ts_daily, 3600/ic%dts*24) == 0) then
            avr_qo2 = avr_qo2/24.0
            do l = 1, fms%stmg%n
                stfl%qsyn(l) = avr_qo2(fms%stmg%meta%rnk(l))
            end do
            avr_qo2 = 0.0

            !> Preserve last reach state.
            do l = 1, noresv
               reach_last(l) = lake_elv(l, fhr)
            end do

            !> Write daily output for streamflow.
!todo: move this
            write(70, 1010, advance = 'no') ic%now%year, ic%now%jday
            write(70, 1010, advance = 'no') (stfl%qhyd(l), stfl%qsyn(l), l = 1, fms%stmg%n)
            write(70, *)

        end if

1010    format(9999(g15.7e2, ','))

    end subroutine

end module
