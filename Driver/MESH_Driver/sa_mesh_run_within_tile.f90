module sa_mesh_run_within_tile

    implicit none

    contains

    subroutine run_within_tile_init(shd, fls, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for calls to processes.
        use RUNCLASS36_config
        use RUNSVS113_config
        use baseflow_module
        use cropland_irrigation_init
        use RUNLAKE_config
        use RUNLAKE_module
        use RUNLAKE_variables

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        integer m, i

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Call processes.
        call RUNCLASS36_init(shd, fls, cm)
        call RUNSVS113_init(shd, fls, cm)
        call bflm_init(fls, shd, cm)
        call runci_init(shd, fls)

        call RUNLAKE_init

        call gatherLakeTileParam(shd%NA)
        shd%wc%ILG = NMW
        allocate(shd%wc%ILMOS(NMW), shd%wc%JLMOS(NMW))

        !> Count the number of tiles that are land 'lc' or water 'wc' and
        !> store the respective ID's of the grid and GRU in the 'ILMOS' and
        !> 'JLMOS' variables.
        NMW = 0
        do i = 1, shd%NA
          do m = 1, NTYPEL
            if (lakeTileParam(m)%FARE(i) > 0.0) then
                            NMW = NMW + 1
                            shd%wc%ILMOS(NMW) = i
                            shd%wc%JLMOS(NMW) = m
            end if
          end do
        end do

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
        use RUNLAKE_module ! included by LAM, 12-JUL-2017

        character(100) run_within_tile

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

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

        !> Call processes.
        call RUNCLASS36_within_tile(shd, fls, cm)
        call RUNSVS113(shd, fls, cm)
        call bflm_within_tile(fls, shd, cm)
        call runci_within_tile(shd, fls, cm)
        call RUNLAKE_within_tile(shd, cm)

        !> MPI exchange.
        call run_within_tile_mpi(shd)

        where (stas%cnpy%pevp(il1:il2) /= 0.0)
            stas%cnpy%evpb(il1:il2) = stas%sfc%evap(il1:il2)/stas%cnpy%pevp(il1:il2)
            stas%cnpy%arrd(il1:il2) = cm%dat(ck%RT)%GAT(il1:il2)/stas%cnpy%pevp(il1:il2)
        end where

        return

    end function

    subroutine run_within_tile_mpi(shd)

        !> For: MPI variables, barrier flag, il1:il2 parse utility
        use mpi_module

        !> For: Model states, 'ic'.
        use sa_mesh_shared_variables
        use model_dates

        !> For: SAVERESUMEFLAG, RESUMEFLAG.
        use FLAGS

        !> For BASEFLOWFLAG.
        use baseflow_module

        !> Input variables.
        type(ShedGridParams) :: shd

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
