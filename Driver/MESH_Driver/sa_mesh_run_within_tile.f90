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

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Call processes.
        call RUNCLASS36_init(shd, fls, cm)
        call RUNSVS113_init(shd, fls, cm)
        call bflm_init(fls, shd, cm)
        call runci_init(shd, fls)

        !> Update variables.
        call run_within_tile_stas_update(shd, cm)

    end subroutine

    function run_within_tile(shd, fls, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for calls to processes.
        use RUNCLASS36_module
        use RUNSVS113_module
        use baseflow_module
        use cropland_irrigation_within_tile

        character(100) run_within_tile

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        run_within_tile = ''

        !> MPI exchange.
        call run_within_tile_mpi_irecv(shd, cm)

        !> Call processes.
        call RUNCLASS36_within_tile(shd, fls, cm)
        call RUNSVS113(shd, fls, cm)
        call bflm_within_tile(fls, shd, cm)
        call runci_within_tile(shd, fls, cm)

        !> MPI exchange.
        call run_within_tile_mpi_isend(shd, cm)

        !> Update variables.
        call run_within_tile_stas_update(shd, cm)

    end function

    subroutine run_within_tile_mpi_isend(shd, cm)

        !> For: MPI variables, barrier flag, il1:il2 parse utility.
        use mpi_module

        !> Process modules (required for variables).
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use baseflow_module, only: bflm, Qb

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer nvars, t, i, j, u, s, ii1, ii2, iin, z
        logical lstat
        integer, allocatable :: irqst(:), imstat(:, :)
        real, dimension(:), allocatable :: cnpy, sno, sfc, sl, lzs, dzs

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Count the number of active variables included in the exchange.
        nvars = 6
        if (bflm%BASEFLOWFLAG == 1) nvars = nvars + 1
        if (nvars == 0) return

        !> Exchange variables.
        if (allocated(irqst)) deallocate(irqst)
        if (allocated(imstat)) deallocate(imstat)
        allocate(irqst(nvars), imstat(mpi_status_size, nvars))
        t = ic%ts_count*1000

        !> Other variables
        s = shd%lc%IGND

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            !> Assign the indices.
            ii1 = il1; ii2 = il2; iin = iln

            !> Reset the exchange variables.
            i = 1
            irqst = mpi_request_null

            !> Canopy.
            allocate(cnpy(8*iin))
            cnpy((1 + iin*0):(iin*1)) = stas%cnpy%rcan(ii1:ii2)
            cnpy((1 + iin*1):(iin*2)) = stas%cnpy%sncan(ii1:ii2)
            cnpy((1 + iin*2):(iin*3)) = stas%cnpy%cmai(ii1:ii2)
            cnpy((1 + iin*3):(iin*4)) = stas%cnpy%tac(ii1:ii2)
            cnpy((1 + iin*4):(iin*5)) = stas%cnpy%tcan(ii1:ii2)
            cnpy((1 + iin*5):(iin*6)) = stas%cnpy%qac(ii1:ii2)
            cnpy((1 + iin*6):(iin*7)) = stas%cnpy%gro(ii1:ii2)
            cnpy((1 + iin*7):(iin*8)) = stas%cnpy%pevp(ii1:ii2)
            call mpi_isend(cnpy, size(cnpy), mpi_real, 0, t + i, mpi_comm_world, irqst(i), z)
            i = i + 1

            !> Snow.
            allocate(sno(6*iin))
            sno((1 + iin*0):(iin*1)) = stas%sno%sno(ii1:ii2)
            sno((1 + iin*1):(iin*2)) = stas%sno%albs(ii1:ii2)
            sno((1 + iin*2):(iin*3)) = stas%sno%fsno(ii1:ii2)
            sno((1 + iin*3):(iin*4)) = stas%sno%rhos(ii1:ii2)
            sno((1 + iin*4):(iin*5)) = stas%sno%tsno(ii1:ii2)
            sno((1 + iin*5):(iin*6)) = stas%sno%wsno(ii1:ii2)
            call mpi_isend(sno, size(sno), mpi_real, 0, t + i, mpi_comm_world, irqst(i), z)
            i = i + 1

            !> Surface or at near surface.
            allocate(sfc((11 + 4)*iin))
            sfc((1 + iin*0):(iin*1)) = stas%sfc%albt(ii1:ii2)
            sfc((1 + iin*1):(iin*2)) = stas%sfc%alvs(ii1:ii2)
            sfc((1 + iin*2):(iin*3)) = stas%sfc%alir(ii1:ii2)
            sfc((1 + iin*3):(iin*4)) = stas%sfc%gte(ii1:ii2)
            sfc((1 + iin*4):(iin*5)) = stas%sfc%zpnd(ii1:ii2)
            sfc((1 + iin*5):(iin*6)) = stas%sfc%tpnd(ii1:ii2)
            sfc((1 + iin*6):(iin*7)) = stas%sfc%evap(ii1:ii2)
            sfc((1 + iin*7):(iin*8)) = stas%sfc%rofo(ii1:ii2)
            sfc((1 + iin*8):(iin*9)) = stas%sfc%qevp(ii1:ii2)
            sfc((1 + iin*9):(iin*10)) = stas%sfc%hfs(ii1:ii2)
            sfc((1 + iin*10):(iin*11)) = stas%sfc%gzero(ii1:ii2)
            do j = 0, 3
                sfc((1 + iin*(11 + j)):(iin*(12 + j))) = stas%sfc%tsfs(ii1:ii2, j + 1)
            end do
            call mpi_isend(sfc, size(sfc), mpi_real, 0, t + i, mpi_comm_world, irqst(i), z)
            i = i + 1

            !> Soil layers.
            allocate(sl((2 + 4*s)*iin))
            sl((1 + iin*0):(iin*1)) = stas%sl%tbas(ii1:ii2)
            sl((1 + iin*1):(iin*2)) = stas%sl%rofs(ii1:ii2)
            do j = 0, s - 1
                sl((1 + iin*(2 + j*4)):(iin*(3 + j*4))) = stas%sl%thic(ii1:ii2, j + 1)
                sl((1 + iin*(3 + j*4)):(iin*(4 + j*4))) = stas%sl%thlq(ii1:ii2, j + 1)
                sl((1 + iin*(4 + j*4)):(iin*(5 + j*4))) = stas%sl%tbar(ii1:ii2, j + 1)
                sl((1 + iin*(5 + j*4)):(iin*(6 + j*4))) = stas%sl%gflx(ii1:ii2, j + 1)
            end do
            call mpi_isend(sl, size(sl), mpi_real, 0, t + i, mpi_comm_world, irqst(i), z)
            i = i + 1

            !> Lower zone storage.
            allocate(lzs(2*iin))
            lzs((1 + iin*0):(iin*1)) = stas%lzs%lqws(ii1:ii2)
            lzs((1 + iin*1):(iin*2)) = stas%lzs%rofb(ii1:ii2)
            call mpi_isend(lzs, size(lzs), mpi_real, 0, t + i, mpi_comm_world, irqst(i), z)
            i = i + 1

            !> Deep zone storage.
            allocate(dzs(2*iin))
            dzs((1 + iin*0):(iin*1)) = stas%dzs%lqws(ii1:ii2)
            dzs((1 + iin*1):(iin*2)) = stas%dzs%rofb(ii1:ii2)
            call mpi_isend(dzs, size(dzs), mpi_real, 0, t + i, mpi_comm_world, irqst(i), z)
            i = i + 1

            !> BASEFLOWFLAG.
            if (bflm%BASEFLOWFLAG == 1) then
                call mpi_isend(Qb(ii1:ii2), iin, mpi_real, 0, t + i, mpi_comm_world, irqst(i), z)
                i = i + 1
            end if

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call mpi_testall(nvars, irqst, lstat, imstat, z)
            end do

            !> Deallocate temporary arrays.
            deallocate(cnpy, sno, sfc, sl, lzs, dzs)

        else if (inp > 1) then

            !> Receive data from worker nodes.
            do u = 1, (inp - 1)

                !> Get and assign the indices.
                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iin)

                !> Allocate temporary arrays.
                allocate(cnpy(8*iin))
                allocate(sno(6*iin))
                allocate(sfc((11 + 4)*iin))
                allocate(sl((2 + 4*s)*iin))
                allocate(lzs(2*iin))
                allocate(dzs(2*iin))

                !> Reset the exchange variables.
                i = 1
                irqst = mpi_request_null
                imstat = 0

                !> Receive variables.
                call mpi_irecv(cnpy, size(cnpy), mpi_real, u, t + i, mpi_comm_world, irqst(i), z); i = i + 1
                call mpi_irecv(sno, size(sno), mpi_real, u, t + i, mpi_comm_world, irqst(i), z); i = i + 1
                call mpi_irecv(sfc, size(sfc), mpi_real, u, t + i, mpi_comm_world, irqst(i), z); i = i + 1
                call mpi_irecv(sl, size(sl), mpi_real, u, t + i, mpi_comm_world, irqst(i), z); i = i + 1
                call mpi_irecv(lzs, size(lzs), mpi_real, u, t + i, mpi_comm_world, irqst(i), z); i = i + 1
                call mpi_irecv(dzs, size(dzs), mpi_real, u, t + i, mpi_comm_world, irqst(i), z); i = i + 1

                !> BASEFLOWFLAG.
                if (bflm%BASEFLOWFLAG == 1) then
                    call mpi_irecv(Qb(ii1:ii2), iin, mpi_real, u, t + i, mpi_comm_world, irqst(i), z); i = i + 1
                end if

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(nvars, irqst, lstat, imstat, z)
                end do

                !> Assign variables.

                !> Canopy.
                stas%cnpy%rcan(ii1:ii2) = cnpy((1 + iin*0):(iin*1))
                stas%cnpy%sncan(ii1:ii2) = cnpy((1 + iin*1):(iin*2))
                stas%cnpy%cmai(ii1:ii2) = cnpy((1 + iin*2):(iin*3))
                stas%cnpy%tac(ii1:ii2) = cnpy((1 + iin*3):(iin*4))
                stas%cnpy%tcan(ii1:ii2) = cnpy((1 + iin*4):(iin*5))
                stas%cnpy%qac(ii1:ii2) = cnpy((1 + iin*5):(iin*6))
                stas%cnpy%gro(ii1:ii2) = cnpy((1 + iin*6):(iin*7))
                stas%cnpy%pevp(ii1:ii2) = cnpy((1 + iin*7):(iin*8))

                !> Snow.
                stas%sno%sno(ii1:ii2) = sno((1 + iin*0):(iin*1))
                stas%sno%albs(ii1:ii2) = sno((1 + iin*1):(iin*2))
                stas%sno%fsno(ii1:ii2) = sno((1 + iin*2):(iin*3))
                stas%sno%rhos(ii1:ii2) = sno((1 + iin*3):(iin*4))
                stas%sno%tsno(ii1:ii2) = sno((1 + iin*4):(iin*5))
                stas%sno%wsno(ii1:ii2) = sno((1 + iin*5):(iin*6))

                !> Surface or at near surface.
                stas%sfc%albt(ii1:ii2) = sfc((1 + iin*0):(iin*1))
                stas%sfc%alvs(ii1:ii2) = sfc((1 + iin*1):(iin*2))
                stas%sfc%alir(ii1:ii2) = sfc((1 + iin*2):(iin*3))
                stas%sfc%gte(ii1:ii2) = sfc((1 + iin*3):(iin*4))
                stas%sfc%zpnd(ii1:ii2) = sfc((1 + iin*4):(iin*5))
                stas%sfc%tpnd(ii1:ii2) = sfc((1 + iin*5):(iin*6))
                stas%sfc%evap(ii1:ii2) = sfc((1 + iin*6):(iin*7))
                stas%sfc%rofo(ii1:ii2) = sfc((1 + iin*7):(iin*8))
                stas%sfc%qevp(ii1:ii2) = sfc((1 + iin*8):(iin*9))
                stas%sfc%hfs(ii1:ii2) = sfc((1 + iin*9):(iin*10))
                stas%sfc%gzero(ii1:ii2) = sfc((1 + iin*10):(iin*11))
                do j = 0, 3
                    stas%sfc%tsfs(ii1:ii2, j + 1) = sfc((1 + iin*(11 + j)):(iin*(12 + j)))
                end do

                !> Soil layers.
                stas%sl%tbas(ii1:ii2) = sl((1 + iin*0):(iin*1))
                stas%sl%rofs(ii1:ii2) = sl((1 + iin*1):(iin*2))
                do j = 0, s - 1
                    stas%sl%thic(ii1:ii2, j + 1) = sl((1 + iin*(2 + j*4)):(iin*(3 + j*4)))
                    stas%sl%thlq(ii1:ii2, j + 1) = sl((1 + iin*(3 + j*4)):(iin*(4 + j*4)))
                    stas%sl%tbar(ii1:ii2, j + 1) = sl((1 + iin*(4 + j*4)):(iin*(5 + j*4)))
                    stas%sl%gflx(ii1:ii2, j + 1) = sl((1 + iin*(5 + j*4)):(iin*(6 + j*4)))
                end do

                !> Lower zone storage.
                stas%lzs%lqws(ii1:ii2) = lzs((1 + iin*0):(iin*1))
                stas%lzs%rofb(ii1:ii2) = lzs((1 + iin*1):(iin*2))

                !> Deep zone storage.
                stas%dzs%lqws(ii1:ii2) = dzs((1 + iin*0):(iin*1))
                stas%dzs%rofb(ii1:ii2) = dzs((1 + iin*1):(iin*2))

                !> Deallocate temporary arrays.
                deallocate(cnpy, sno, sfc, sl, lzs, dzs)

            end do !u = 1, (inp - 1)

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, z)

    end subroutine

    subroutine run_within_tile_mpi_irecv(shd, cm)

        !> For: MPI variables, barrier flag, il1:il2 parse utility.
        use mpi_module

        !> Process modules (required for variables).
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer nvars, t, i, j, u, ii1, ii2, iin, z
        logical lstat
        integer, allocatable :: irqst(:), imstat(:, :)

        !> Return if grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Count the number of active variables included in the exchange.
        nvars = 0
        if (nvars == 0) return

        !> Exchange variables.
        if (allocated(irqst)) deallocate(irqst)
        if (allocated(imstat)) deallocate(imstat)
        allocate(irqst(nvars), imstat(mpi_status_size, nvars))
        t = ic%ts_count*1000 + 400

        !> Assign the indices.
        ii1 = 1
        ii2 = shd%lc%NML
        iin = shd%lc%NML

        if (inp > 1 .and. ipid == 0) then

            !> Send data to worker nodes.
            do u = 1, (inp - 1)

                !> Reset the exchange variables.
                i = 1
                irqst = mpi_request_null
                imstat = 0

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(nvars, irqst, lstat, imstat, z)
                end do

            end do !u = 1, (inp - 1)

        else if (inp > 1) then

            !> Receive data from head-node.
            !> Reset the exchange variables.
            i = 1
            irqst = mpi_request_null

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call mpi_testall(nvars, irqst, lstat, imstat, z)
            end do

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, z)

    end subroutine

    subroutine run_within_tile_stas_update(shd, cm)

        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for 'il1:il2' indices.
        use mpi_module

!+todo: There's a dependency on CLASSBD.f.
        use RUNCLASS36_constants, only: RHOW, RHOICE

        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Update variables.
        stas%sl%fzws(il1:il2, :) = stas%sl%thic(il1:il2, :)*stas%sl%delzw(il1:il2, :)*RHOICE
        stas%sl%lqws(il1:il2, :) = stas%sl%thlq(il1:il2, :)*stas%sl%delzw(il1:il2, :)*RHOW
        where (stas%sfc%evap(il1:il2) > 0.0 .and. stas%cnpy%pevp(il1:il2) /= 0.0)
            stas%cnpy%evpb(il1:il2) = stas%sfc%evap(il1:il2)/stas%cnpy%pevp(il1:il2)
        elsewhere
            stas%cnpy%evpb(il1:il2) = 0.0
        end where
        if (allocated(cm%dat(ck%RT)%GAT)) then
            where (stas%cnpy%pevp(il1:il2) /= 0.0)
                stas%cnpy%arrd(il1:il2) = cm%dat(ck%RT)%GAT(il1:il2)/stas%cnpy%pevp(il1:il2)
            elsewhere
                stas%cnpy%arrd(il1:il2) = 0.0
            end where
        end if
        where (stas%sfc%alvs(il1:il2) > 0.0 .and. stas%sfc%alir(il1:il2) > 0.0)
            stas%sfc%albt(il1:il2) = (stas%sfc%alvs(il1:il2) + stas%sfc%alir(il1:il2))/2.0
        elsewhere
            stas%sfc%albt(il1:il2) = 0.0
        end where
        stas%sfc%pndw(il1:il2) = stas%sfc%zpnd(il1:il2)*RHOW
        where (stas%sno%sno(il1:il2) == 0.0)
            stas%sno%wsno(il1:il2) = 0.0
            stas%sno%tsno(il1:il2) = 0.0
        end where
        where (stas%sfc%zpnd(il1:il2) == 0.0) stas%sfc%tpnd(il1:il2) = 0.0

    end subroutine

    subroutine run_within_tile_finalize(fls, shd, cm)

        use model_files_variabletypes
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for calls to processes.
        use RUNCLASS36_config
        use RUNSVS113_config
        use baseflow_module

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Call processes.
        call RUNCLASS36_finalize(fls, shd, cm)
        call RUNSVS113_finalize(shd, fls, cm)
        call bflm_finalize(fls, shd, cm)

    end subroutine

end module
