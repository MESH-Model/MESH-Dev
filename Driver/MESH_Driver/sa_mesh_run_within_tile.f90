module sa_mesh_run_within_tile

    !> 'model_files_variables' required for 'fls' object and file keys.
    !> 'sa_mesh_common' required for common SA_MESH variables and routines.
    !> 'climate_forcing' required for 'cm' variable.
    !> 'mpi_module' required for MPI variables, tile/grid parsing utility, barrier flag.
    use model_files_variables
    use sa_mesh_common
    use climate_forcing
    use mpi_module

    implicit none

    contains

    subroutine run_within_tile_init(fls, shd, cm)

        !> Process modules.
        use RUNCLASS36_config
        use runsvs_mesh
        use baseflow_module
        use cropland_irrigation_init

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Call processes.
        call RUNCLASS36_init(shd, fls, cm)
        call runsvs_mesh_init(shd, fls, cm)
        call bflm_init(fls, shd, cm)
        call runci_init(shd, fls)

        !> Update variables.
        call run_within_tile_stas_update(fls, shd, cm)

    end subroutine

    subroutine run_within_tile(fls, shd, cm)

        !> Process modules.
        use RUNCLASS36_module
        use runsvs_mesh
        use baseflow_module
        use cropland_irrigation_within_tile

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> MPI exchange.
        call run_within_tile_mpi_irecv(fls, shd, cm)

        !> Reset variables non-prognostic variables.
        call run_within_tile_stas_reset(fls, shd, cm)

        !> Call processes.
        call RUNCLASS36_within_tile(shd, fls, cm)
        call runsvs_mesh_within_tile(shd, fls, cm)
        call bflm_within_tile(fls, shd, cm)
        call runci_within_tile(shd, fls, cm)

        !> MPI exchange.
        call run_within_tile_mpi_isend(fls, shd, cm)

        !> Update variables.
        call run_within_tile_stas_update(fls, shd, cm)

    end subroutine

    subroutine run_within_tile_mpi_isend(fls, shd, cm)

        !> Process modules.
        use baseflow_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer nvars, t, i, j, u, s, ii1, ii2, iin, z
        logical lstat
        integer, allocatable :: irqst(:), imstat(:, :)
        real, dimension(:), allocatable :: cnpy, sno, sfc, sl, lz, dz

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Count the number of active variables included in the exchange.
        nvars = 6
        if (bflm%BASEFLOWFLAG == 1) nvars = nvars + 1
        if (nvars == 0) return

        !> Exchange variables.
        if (allocated(irqst)) deallocate(irqst)
        if (allocated(imstat)) deallocate(imstat)
        allocate(irqst(nvars), imstat(MPI_STATUS_SIZE, nvars))
        t = ic%ts_count*1000

        !> Other variables
        s = shd%lc%IGND

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            !> Assign the indices.
            ii1 = il1; ii2 = il2; iin = iln

            !> Reset the exchange variables.
            i = 1
            irqst = MPI_REQUEST_NULL

            !> Canopy.
            allocate(cnpy(7*iin))
            cnpy((1 + iin*0):(iin*1)) = vs%tile%rcan(ii1:ii2)
            cnpy((1 + iin*1):(iin*2)) = vs%tile%sncan(ii1:ii2)
            cnpy((1 + iin*2):(iin*3)) = vs%tile%cmas(ii1:ii2)
            cnpy((1 + iin*3):(iin*4)) = vs%tile%tac(ii1:ii2)
            cnpy((1 + iin*4):(iin*5)) = vs%tile%tcan(ii1:ii2)
            cnpy((1 + iin*5):(iin*6)) = vs%tile%qac(ii1:ii2)
            cnpy((1 + iin*6):(iin*7)) = vs%tile%gro(ii1:ii2)
            call MPI_Isend(cnpy, size(cnpy), MPI_REAL, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
            i = i + 1

            !> Snow.
            allocate(sno(6*iin))
            sno((1 + iin*0):(iin*1)) = vs%tile%sno(ii1:ii2)
            sno((1 + iin*1):(iin*2)) = vs%tile%albs(ii1:ii2)
            sno((1 + iin*2):(iin*3)) = vs%tile%fsno(ii1:ii2)
            sno((1 + iin*3):(iin*4)) = vs%tile%rhos(ii1:ii2)
            sno((1 + iin*4):(iin*5)) = vs%tile%wsno(ii1:ii2)
            sno((1 + iin*5):(iin*6)) = vs%tile%tsno(ii1:ii2)
            call MPI_Isend(sno, size(sno), MPI_REAL, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
            i = i + 1

            !> Surface or at near surface.
            allocate(sfc((13 + 4)*iin))
            sfc((1 + iin*0):(iin*1)) = vs%tile%albt(ii1:ii2)
            sfc((1 + iin*1):(iin*2)) = vs%tile%alvs(ii1:ii2)
            sfc((1 + iin*2):(iin*3)) = vs%tile%alir(ii1:ii2)
            sfc((1 + iin*3):(iin*4)) = vs%tile%gte(ii1:ii2)
            sfc((1 + iin*4):(iin*5)) = vs%tile%zpnd(ii1:ii2)
            sfc((1 + iin*5):(iin*6)) = vs%tile%tpnd(ii1:ii2)
            sfc((1 + iin*6):(iin*7)) = vs%tile%fstr(ii1:ii2)
            sfc((1 + iin*7):(iin*8)) = vs%tile%pevp(ii1:ii2)
            sfc((1 + iin*8):(iin*9)) = vs%tile%evap(ii1:ii2)
            sfc((1 + iin*9):(iin*10)) = vs%tile%rofo(ii1:ii2)
            sfc((1 + iin*10):(iin*11)) = vs%tile%qevp(ii1:ii2)
            sfc((1 + iin*11):(iin*12)) = vs%tile%hfs(ii1:ii2)
            sfc((1 + iin*12):(iin*13)) = vs%tile%gzero(ii1:ii2)
            do j = 0, 3
                sfc((1 + iin*(13 + j)):(iin*(14 + j))) = vs%tile%tsfs(ii1:ii2, j + 1)
            end do
            call MPI_Isend(sfc, size(sfc), MPI_REAL, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
            i = i + 1

            !> Soil layers.
            allocate(sl((2 + 4*s)*iin))
            sl((1 + iin*0):(iin*1)) = vs%tile%tbas(ii1:ii2)
            sl((1 + iin*1):(iin*2)) = vs%tile%rofs(ii1:ii2)
            do j = 0, s - 1
                sl((1 + iin*(2 + j*4)):(iin*(3 + j*4))) = vs%tile%thic(ii1:ii2, j + 1)
                sl((1 + iin*(3 + j*4)):(iin*(4 + j*4))) = vs%tile%thlq(ii1:ii2, j + 1)
                sl((1 + iin*(4 + j*4)):(iin*(5 + j*4))) = vs%tile%tbar(ii1:ii2, j + 1)
                sl((1 + iin*(5 + j*4)):(iin*(6 + j*4))) = vs%tile%gflx(ii1:ii2, j + 1)
            end do
            call MPI_Isend(sl, size(sl), MPI_REAL, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
            i = i + 1

            !> Lower zone storage.
            allocate(lz(iin))
            lz((1 + iin*0):(iin*1)) = vs%tile%lzs(ii1:ii2)
            call MPI_Isend(lz, size(lz), MPI_REAL, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
            i = i + 1

            !> Deep zone storage.
            allocate(dz(2*iin))
            dz((1 + iin*0):(iin*1)) = vs%tile%dzs(ii1:ii2)
            dz((1 + iin*1):(iin*2)) = vs%tile%rofb(ii1:ii2)
            call MPI_Isend(dz, size(dz), MPI_REAL, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
            i = i + 1

            !> BASEFLOWFLAG.
            if (bflm%BASEFLOWFLAG == 1) then
                call MPI_Isend(Qb(ii1:ii2), iin, MPI_REAL, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
                i = i + 1
            end if

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call MPI_Testall(nvars, irqst, lstat, imstat, z)
            end do

            !> Deallocate temporary arrays.
            deallocate(cnpy, sno, sfc, sl, lz, dz)

        else if (inp > 1) then

            !> Receive data from worker nodes.
            do u = 1, (inp - 1)

                !> Get and assign the indices.
                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iin)

                !> Allocate temporary arrays.
                allocate(cnpy(7*iin))
                allocate(sno(6*iin))
                allocate(sfc((13 + 4)*iin))
                allocate(sl((2 + 4*s)*iin))
                allocate(lz(iin))
                allocate(dz(2*iin))

                !> Reset the exchange variables.
                i = 1
                irqst = MPI_REQUEST_NULL
                imstat = 0

                !> Receive variables.
                call MPI_Irecv(cnpy, size(cnpy), MPI_REAL, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
                call MPI_Irecv(sno, size(sno), MPI_REAL, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
                call MPI_Irecv(sfc, size(sfc), MPI_REAL, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
                call MPI_Irecv(sl, size(sl), MPI_REAL, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
                call MPI_Irecv(lz, size(lz), MPI_REAL, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
                call MPI_Irecv(dz, size(dz), MPI_REAL, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1

                !> BASEFLOWFLAG.
                if (bflm%BASEFLOWFLAG == 1) then
                    call MPI_Irecv(Qb(ii1:ii2), iin, MPI_REAL, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
                end if

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
                    call MPI_Testall(nvars, irqst, lstat, imstat, z)
                end do

                !> Assign variables.

                !> Canopy.
                vs%tile%rcan(ii1:ii2) = cnpy((1 + iin*0):(iin*1))
                vs%tile%sncan(ii1:ii2) = cnpy((1 + iin*1):(iin*2))
                vs%tile%cmas(ii1:ii2) = cnpy((1 + iin*2):(iin*3))
                vs%tile%tac(ii1:ii2) = cnpy((1 + iin*3):(iin*4))
                vs%tile%tcan(ii1:ii2) = cnpy((1 + iin*4):(iin*5))
                vs%tile%qac(ii1:ii2) = cnpy((1 + iin*5):(iin*6))
                vs%tile%gro(ii1:ii2) = cnpy((1 + iin*6):(iin*7))

                !> Snow.
                vs%tile%sno(ii1:ii2) = sno((1 + iin*0):(iin*1))
                vs%tile%albs(ii1:ii2) = sno((1 + iin*1):(iin*2))
                vs%tile%fsno(ii1:ii2) = sno((1 + iin*2):(iin*3))
                vs%tile%rhos(ii1:ii2) = sno((1 + iin*3):(iin*4))
                vs%tile%wsno(ii1:ii2) = sno((1 + iin*4):(iin*5))
                vs%tile%tsno(ii1:ii2) = sno((1 + iin*5):(iin*6))

                !> Surface or at near surface.
                vs%tile%albt(ii1:ii2) = sfc((1 + iin*0):(iin*1))
                vs%tile%alvs(ii1:ii2) = sfc((1 + iin*1):(iin*2))
                vs%tile%alir(ii1:ii2) = sfc((1 + iin*2):(iin*3))
                vs%tile%gte(ii1:ii2) = sfc((1 + iin*3):(iin*4))
                vs%tile%zpnd(ii1:ii2) = sfc((1 + iin*4):(iin*5))
                vs%tile%tpnd(ii1:ii2) = sfc((1 + iin*5):(iin*6))
                vs%tile%fstr(ii1:ii2) = sfc((1 + iin*6):(iin*7))
                vs%tile%pevp(ii1:ii2) = sfc((1 + iin*7):(iin*8))
                vs%tile%evap(ii1:ii2) = sfc((1 + iin*8):(iin*9))
                vs%tile%rofo(ii1:ii2) = sfc((1 + iin*9):(iin*10))
                vs%tile%qevp(ii1:ii2) = sfc((1 + iin*10):(iin*11))
                vs%tile%hfs(ii1:ii2) = sfc((1 + iin*11):(iin*12))
                vs%tile%gzero(ii1:ii2) = sfc((1 + iin*12):(iin*13))
                do j = 0, 3
                    vs%tile%tsfs(ii1:ii2, j + 1) = sfc((1 + iin*(13 + j)):(iin*(14 + j)))
                end do

                !> Soil layers.
                vs%tile%tbas(ii1:ii2) = sl((1 + iin*0):(iin*1))
                vs%tile%rofs(ii1:ii2) = sl((1 + iin*1):(iin*2))
                do j = 0, s - 1
                    vs%tile%thic(ii1:ii2, j + 1) = sl((1 + iin*(2 + j*4)):(iin*(3 + j*4)))
                    vs%tile%thlq(ii1:ii2, j + 1) = sl((1 + iin*(3 + j*4)):(iin*(4 + j*4)))
                    vs%tile%tbar(ii1:ii2, j + 1) = sl((1 + iin*(4 + j*4)):(iin*(5 + j*4)))
                    vs%tile%gflx(ii1:ii2, j + 1) = sl((1 + iin*(5 + j*4)):(iin*(6 + j*4)))
                end do

                !> Lower zone storage.
                vs%tile%lzs(ii1:ii2) = lz((1 + iin*0):(iin*1))

                !> Deep zone storage.
                vs%tile%dzs(ii1:ii2) = dz((1 + iin*0):(iin*1))
                vs%tile%rofb(ii1:ii2) = dz((1 + iin*1):(iin*2))

                !> Deallocate temporary arrays.
                deallocate(cnpy, sno, sfc, sl, lz, dz)

            end do !u = 1, (inp - 1)

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, z)

    end subroutine

    subroutine run_within_tile_mpi_irecv(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer nvars, t, i, j, u, ii1, ii2, iin, z
        logical lstat
        integer, allocatable :: irqst(:), imstat(:, :)

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Count the number of active variables included in the exchange.
        nvars = 0
        if (nvars == 0) return

        !> Exchange variables.
        if (allocated(irqst)) deallocate(irqst)
        if (allocated(imstat)) deallocate(imstat)
        allocate(irqst(nvars), imstat(MPI_STATUS_SIZE, nvars))
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
                irqst = MPI_REQUEST_NULL
                imstat = 0

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
                    call MPI_Testall(nvars, irqst, lstat, imstat, z)
                end do

            end do !u = 1, (inp - 1)

        else if (inp > 1) then

            !> Receive data from head-node.
            !> Reset the exchange variables.
            i = 1
            irqst = MPI_REQUEST_NULL

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call MPI_Testall(nvars, irqst, lstat, imstat, z)
            end do

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, z)

    end subroutine

    subroutine run_within_tile_stas_reset(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Reset variables non-prognostic variables.
        vs%tile%zsno(il1:il2) = 0.0
        vs%tile%fsno(il1:il2) = 0.0
        vs%tile%albt(il1:il2) = 0.0
        vs%tile%alvs(il1:il2) = 0.0
        vs%tile%alir(il1:il2) = 0.0
        vs%tile%gte(il1:il2) = 0.0
        vs%tile%pevp(il1:il2) = 0.0
        vs%tile%evap(il1:il2) = 0.0
        vs%tile%rofo(il1:il2) = 0.0
        vs%tile%qevp(il1:il2) = 0.0
        vs%tile%fstr(il1:il2) = 0.0
        vs%tile%hfs(il1:il2) = 0.0
        vs%tile%gzero(il1:il2) = 0.0
        vs%tile%rofs(il1:il2) = 0.0
        vs%tile%gflx(il1:il2, :) = 0.0
        vs%tile%rofb(il1:il2) = 0.0

    end subroutine

    subroutine run_within_tile_stas_update(fls, shd, cm)

!+todo: There's a dependency on CLASSBD.f.
        use RUNCLASS36_constants, only: RHOW, RHOICE

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Update variables.
        where (vs%tile%sno(il1:il2) == 0.0)
            vs%tile%wsno(il1:il2) = 0.0
            vs%tile%tsno(il1:il2) = 0.0
        end where
        if (all(vs%tile%zsno(il1:il2) == 0.0)) then
            where (vs%tile%rhos(il1:il2) > 0.0)
                vs%tile%zsno(il1:il2) = vs%tile%sno(il1:il2)/vs%tile%rhos(il1:il2)
            end where
        end if
        if (all(vs%tile%albt(il1:il2) == 0.0)) then
            where (vs%tile%alvs(il1:il2) > 0.0 .and. vs%tile%alir(il1:il2) > 0.0)
                vs%tile%albt(il1:il2) = (vs%tile%alvs(il1:il2) + vs%tile%alir(il1:il2))/2.0
            elsewhere
                vs%tile%albt(il1:il2) = 0.0
            end where
        end if
        vs%tile%pndw(il1:il2) = vs%tile%zpnd(il1:il2)*RHOW
        where (vs%tile%zpnd(il1:il2) == 0.0) vs%tile%tpnd(il1:il2) = 0.0
        where (vs%tile%evap(il1:il2) > 0.0 .and. vs%tile%pevp(il1:il2) /= 0.0)
            vs%tile%evpb(il1:il2) = vs%tile%evap(il1:il2)/vs%tile%pevp(il1:il2)
        elsewhere
            vs%tile%evpb(il1:il2) = 0.0
        end where
        where (vs%tile%pevp(il1:il2) /= 0.0)
            vs%tile%arrd(il1:il2) = vs%tile%pre(il1:il2)/vs%tile%pevp(il1:il2)
        elsewhere
            vs%tile%arrd(il1:il2) = 0.0
        end where
        vs%tile%fzws(il1:il2, :) = vs%tile%thic(il1:il2, :)*vs%tile%delzw(il1:il2, :)*RHOICE
        vs%tile%lqws(il1:il2, :) = vs%tile%thlq(il1:il2, :)*vs%tile%delzw(il1:il2, :)*RHOW

    end subroutine

    subroutine run_within_tile_finalize(fls, shd, cm)

        !> Process modules.
        use RUNCLASS36_config
        use baseflow_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Call processes.
        call RUNCLASS36_finalize(fls, shd, cm)
        call bflm_finalize(fls, shd, cm)

    end subroutine

end module
