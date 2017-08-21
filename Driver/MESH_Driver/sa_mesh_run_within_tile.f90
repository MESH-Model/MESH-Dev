module sa_mesh_run_within_tile

    implicit none

!>>>irrigation
    !*  MINFSTG: Fraction of storage to leave in the channel, not accessible for irrigation. [--].
    !*  MINTHFC: Fraction of field capacity used to determine irrigation demand. [--].
    real, save :: MINFSTG = 0.05
    real, save :: MINTHFC = 0.5
    !*  IRDMND: Calculated irrigation demand. [kg m-2 s-1].
    !*  IRAVAI: Water available for irrigation. [kg m-2 s-1].
    !*  OLDPRE: Diagnostic variable of precipitation before adding water for irrigation. [kg m-2 s-1].
    !*  NEWPRE: Diagnostic variable of precipitation after adding water for irrigation. [kg m-2 s-1].
    real, dimension(:), allocatable, save :: IRDMND, IRAVAI, OLDPRE, NEWPRE
!<<<irrigation

    contains

    subroutine run_within_tile_init(shd, fls, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for calls to processes.
        use RUNCLASS36_config
        use RUNSVS113_config
        use baseflow_module
        use cropland_irrigation_init

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

!>>>irrigation
        allocate(IRDMND(shd%lc%NML), IRAVAI(shd%lc%NML), OLDPRE(shd%lc%NML), NEWPRE(shd%lc%NML))
!<<<irrigation

        !> Call processes.
        call RUNCLASS36_init(shd, fls, cm)
        call RUNSVS113_init(shd, fls, cm)
        call bflm_init(fls, shd, cm)
        call runci_init(shd, fls)

    end subroutine

    function run_within_tile(shd, fls, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for 'il1:il2' indices.
        use mpi_module

        !> Required for calls to processes.
        use RUNCLASS36_module
        use RUNSVS113_module
        use baseflow_module
        use cropland_irrigation_within_tile

        character(100) run_within_tile

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

!>>>irrigation
        integer n, l, k, j
        real ir, lqsum, check
        real, dimension(:), allocatable :: SUMIRDMND, SUMIRAVAI, SUMOLDPRE, SUMNEWPRE
        integer iun
        character(len = 3) ffmti
        character(len = 200) fn
!<<<irrigation

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        stas%cnpy%pevp(il1:il2) = 0.0
        stas%sfc%evap(il1:il2) = 0.0
        stas%cnpy%evpb(il1:il2) = 0.0
        stas%sfc%qevp(il1:il2) = 0.0
        stas%sfc%hfs(il1:il2) = 0.0
        stas%sfc%rofo(il1:il2) = 0.0
        stas%sl%rofs(il1:il2) = 0.0
        stas%lzs%rofb(il1:il2) = 0.0
        stas%dzs%rofb(il1:il2) = 0.0

        run_within_tile = ''

!>>>irrigation (using soil moisture)
        if (ipid /= 0 .or. izero == 0) then
            IRAVAI = 0.0
            OLDPRE = 0.0
            NEWPRE = 0.0
            do k = il1, il2 !GRU -> loop for timestep
                IRDMND(k) = 0.0   !initialization for each time step
                if (pm%tp%mid(k) == 9 .and. sum(stas%sl%thic(k, :)) == 0.0) then ! check irrDist GRU and no ice
!                    do j = 1, shd%lc%IGND ! loop for each Soil layers
                    do j = 1, 3 ! loop for each Soil layers
                        check = MINTHFC*pm%slp%thfc(k, j) ! calculate 50% of field capacity
!                        lqsum =  stas%sl%thlq(k, j) + stas%sl%thic(k, j) ! sum liquid and ice water content in soil
                        lqsum =  stas%sl%thlq(k, j)
                        if (lqsum < check)then ! check if sum of soil moisture is less than 50% of FC
                            ir = (pm%slp%thfc(k, j) - lqsum)*stas%sl%delzw(k, j) ! calculate irrigation water for each permeable soil depth
!print "('THFC, LQSUM, DELZW, IR, SoilLayer', 4f8.4, i3)", csfv%THFC(k, j), lqsum, csfv%DELZW(k, j), ir, j
                        else
                            ir = 0.0
                        end if
                        IRDMND(k) = IRDMND(k) + ir ! sum of complete soil depth
                    end do !soil layer
!                    IRDMND(k) = (IRDMND(k)*(1000.0/ic%dts)) - cm%dat(ck%RT)%GAT(k)
                    IRDMND(k) = IRDMND(k)*(1000.0/ic%dts) ! convert into mm/sec
                    IRAVAI(k) = max(IRDMND(k) - cm%dat(ck%RT)%GAT(k), 0.0) ! subtract current precipitation to calculate actual requirement if there is rain
                    if (pm%tp%iabsp(k) > 0) then
                        n = fms%absp%meta%rnk(pm%tp%iabsp(k)) !RANK of channel to pull abstraction
                    else
                        n = shd%lc%ILMOS(k) ! pull from own cell if no abstraction point defined
                    end if
                    IRAVAI(k) = min(stas_grid%chnl%s(n)*(1.0 - MINFSTG)/shd%AREA(n)*1000.0/ic%dts, IRAVAI(k)) ! adjust to the maximum water available from channel storage (m3 to mm)
                    OLDPRE(k) = cm%dat(ck%RT)%GAT(k)
                    cm%dat(ck%RT)%GAT(k) = cm%dat(ck%RT)%GAT(k) + IRAVAI(k) ! add irrigation water into precipitation
                    NEWPRE(k) = cm%dat(ck%RT)%GAT(k)
                    stas_grid%chnl%s(n) = stas_grid%chnl%s(n) - &
                        (IRAVAI(k)*ic%dts/1000.0)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*shd%AREA(n)
                end if ! check Crop GRU tile
            end do ! GRU tile
        end if
!<<<irrigation

        !> Call processes.
        call RUNCLASS36_within_tile(shd, fls, cm)
        call RUNSVS113(shd, fls, cm)
        call bflm_within_tile(fls, shd, cm)
        call runci_within_tile(shd, fls, cm)

        !> MPI exchange.
        call run_within_tile_mpi(shd, cm)

!>>>irrigation
        if (ipid == 0) then
            if (.not. allocated(SUMIRDMND)) allocate(SUMIRDMND(fms%absp%n))
            if (.not. allocated(SUMIRAVAI)) allocate(SUMIRAVAI(fms%absp%n))
            if (.not. allocated(SUMOLDPRE)) allocate(SUMOLDPRE(fms%absp%n))
            if (.not. allocated(SUMNEWPRE)) allocate(SUMNEWPRE(fms%absp%n))
            if (ic%ts_count == 1) then ! first time (can't put in within_tile_init because structures are read after in between_grid_init).
                open(unit = 1981, file = "irrigation.csv") ! open file for output
                write(1981, 1010) 'YEAR', 'DAY', 'HOUR', 'MINS', 'IRDMND', 'IRAVAI', 'IRTOT', 'OLDPRE', 'NEWPRE'
                do l = 1, fms%absp%n
                    iun = 1981 + l
                    write(ffmti, '(i3)') l
                    fn = 'irrigation_' // trim(adjustl(ffmti)) // '.csv'
                    open(unit = iun, file = fn)
                    write(iun, 1010) 'YEAR', 'DAY', 'HOUR', 'MINS', 'IRDMND', 'IRAVAI', 'IRTOT', 'OLDPRE', 'NEWPRE'
                end do
            end if
            write(1981, 1010) &
                ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins, &
                sum(IRDMND), sum(IRAVAI), sum(IRAVAI), sum(OLDPRE), sum(NEWPRE)
            SUMIRDMND = 0.0; SUMIRAVAI = 0.0; SUMOLDPRE = 0.0; SUMNEWPRE = 0.0
            do k = 1, shd%lc%NML
                if (pm%tp%iabsp(k) > 0) then
                    SUMIRDMND(pm%tp%iabsp(k)) = SUMIRDMND(pm%tp%iabsp(k)) + IRDMND(k)
                    SUMIRAVAI(pm%tp%iabsp(k)) = SUMIRAVAI(pm%tp%iabsp(k)) + IRAVAI(k)
                    SUMOLDPRE(pm%tp%iabsp(k)) = SUMOLDPRE(pm%tp%iabsp(k)) + OLDPRE(k)
                    SUMNEWPRE(pm%tp%iabsp(k)) = SUMNEWPRE(pm%tp%iabsp(k)) + NEWPRE(k)
                end if
            end do
            do l = 1, fms%absp%n
                iun = 1981 + l
                write(iun, 1010) &
                    ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins, &
                    SUMIRDMND(l), SUMIRAVAI(l), SUMIRAVAI(l), SUMOLDPRE(l), SUMNEWPRE(l)
            end do
        end if
1010    format(9999(g15.7e2, ','))
!<<<irrigation

        where (stas%cnpy%pevp(il1:il2) /= 0.0)
            stas%cnpy%evpb(il1:il2) = stas%sfc%evap(il1:il2)/stas%cnpy%pevp(il1:il2)
            stas%cnpy%arrd(il1:il2) = cm%dat(ck%RT)%GAT(il1:il2)/stas%cnpy%pevp(il1:il2)
        end where

        return

    end function

    subroutine run_within_tile_mpi(shd, cm)

        !> For: MPI variables, barrier flag, il1:il2 parse utility
        use mpi_module

        !> For: Model states, 'ic', 'cm'.
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        !> For: SAVERESUMEFLAG, RESUMEFLAG.
        use FLAGS

        !> For BASEFLOWFLAG.
        use baseflow_module

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer ipid_recv, itag, ierrcode, istop, i, j, u, invars, ilen, iiln, ii1, ii2, ierr
        logical lstat
        integer, dimension(:), allocatable :: irqst
        integer, dimension(:, :), allocatable :: imstat

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Gather variables from parallel nodes.

        !> Calculate the total number of active elements in the sequence.
        ilen = (il2 - il1) + 1

        !> Send/receive process.
        itag = ic%ts_count*1000
        invars = 16 + 4*shd%lc%IGND

        !> Update the variable count per the active control flags.
        if (SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) invars = invars + 10 + 4

        !> BASEFLOWFLAG.
        if (bflm%BASEFLOWFLAG == 1) then
            invars = invars + 1
        end if

!>>>irrigation
        invars = invars + 5
!<<<irrigation

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))
            irqst = mpi_request_null

            i = 1
            call mpi_isend(stas%sfc%evap(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%cnpy%pevp(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%sfc%rofo(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%sl%rofs(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%lzs%rofb(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%lzs%lqws(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%dzs%rofb(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%dzs%lqws(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%cnpy%sncan(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%cnpy%rcan(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%sfc%zpnd(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%sno%sno(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%sno%fsno(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%sno%wsno(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%sfc%hfs(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(stas%sfc%qevp(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            do j = 1, shd%lc%IGND
                call mpi_isend(stas%sl%thlq(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%sl%thic(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%sl%gflx(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%sl%tbar(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            end do

            !> Send optional variables per the active control flags.
            if (SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) then
                call mpi_isend(stas%sno%albs(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%cnpy%cmai(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%cnpy%gro(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%cnpy%qac(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%sno%rhos(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%cnpy%tac(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%sl%tbas(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%cnpy%tcan(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%sfc%tpnd(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_isend(stas%sno%tsno(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                do j = 1, 4
                    call mpi_isend(stas%sfc%tsfs(il1:il2, j), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr)
                    i = i + 1
                end do
            end if !(SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) then

            !> BASEFLOWFLAG.
            if (bflm%BASEFLOWFLAG == 1) then
                call mpi_isend(Qb(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            end if

!>>>irrigation
            call mpi_isend(IRDMND(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(IRAVAI(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(OLDPRE(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(cm%dat(ck%RT)%GAT(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
            call mpi_isend(NEWPRE(il1:il2), ilen, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
!<<<irrigation

            lstat = .false.
            do while (.not. lstat)
                call mpi_testall(invars, irqst, lstat, imstat, ierr)
            end do

        else if (inp > 1) then

            !> Receive data from worker nodes.
            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))

            !> Receive and assign variables.
            do u = 1, (inp - 1)

!                print *, 'initiating irecv for:', u, ' with ', itag

                irqst = mpi_request_null
                imstat = 0

                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iiln)

                !> Calculate the total number of active elements in the sequence.
                ilen = (il2 - il1) + 1

                i = 1
                call mpi_irecv(stas%sfc%evap(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%cnpy%pevp(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%sfc%rofo(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%sl%rofs(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%lzs%rofb(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%lzs%lqws(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%dzs%rofb(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%dzs%lqws(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%cnpy%sncan(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%cnpy%rcan(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%sfc%zpnd(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%sno%sno(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%sno%fsno(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%sno%wsno(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%sfc%hfs(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(stas%sfc%qevp(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                do j = 1, shd%lc%IGND
                    call mpi_irecv(stas%sl%thlq(ii1:ii2, j), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr)
                    i = i + 1
                    call mpi_irecv(stas%sl%thic(ii1:ii2, j), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr)
                    i = i + 1
                    call mpi_irecv(stas%sl%gflx(ii1:ii2, j), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr)
                    i = i + 1
                    call mpi_irecv(stas%sl%tbar(ii1:ii2, j), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr)
                    i = i + 1
                end do

                !> Send optional variables per the active control flags.
                if (SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) then
                    call mpi_irecv(stas%sno%albs(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(stas%cnpy%cmai(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(stas%cnpy%gro(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(stas%cnpy%qac(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(stas%sno%rhos(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(stas%cnpy%tac(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(stas%sl%tbas(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(stas%cnpy%tcan(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(stas%sfc%tpnd(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    call mpi_irecv(stas%sno%tsno(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                    do j = 1, 4
                        call mpi_irecv(stas%sfc%tsfs(ii1:ii2, j), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr)
                        i = i + 1
                    end do
                end if !(SAVERESUMEFLAG >= 3 .and. SAVERESUMEFLAG <= 5) then

                !> BASEFLOWFLAG.
                if (bflm%BASEFLOWFLAG == 1) then
                    call mpi_irecv(Qb(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                end if

!>>>irrigation
                call mpi_irecv(IRDMND(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(IRAVAI(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(OLDPRE(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(cm%dat(ck%RT)%GAT(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
                call mpi_irecv(NEWPRE(ii1:ii2), iiln, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
!<<<irrigation

                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(invars, irqst, lstat, imstat, ierr)
                end do

            end do !u = 1, (inp - 1)

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, ierr)

    end subroutine

    subroutine run_within_tile_finalize(fls, shd, cm)

        use model_files_variabletypes
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for calls to processes.
        use RUNCLASS36_config
        use baseflow_module

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Call processes.
        call RUNCLASS36_finalize(fls, shd, cm)
        call bflm_finalize(fls, shd, cm)

    end subroutine

end module
