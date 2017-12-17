module sa_mesh_run_within_grid

    implicit none

    contains

    subroutine run_within_grid_init(shd, fls, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Update variables.
        call run_within_grid_stas_update(shd, cm)

    end subroutine

    subroutine run_within_grid(shd, fls, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for calls to processes.
        use baseflow_module

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Update variables.
        call run_within_grid_stas_update(shd, cm)

        !> Call processes.
        call bflm_within_grid(fls, shd, cm)

    end subroutine

    subroutine run_within_grid_mpi_isend(shd, cm)

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
        t = ic%ts_count*1000 + 200

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            !> Assign the indices.
            ii1 = shd%lc%ILMOS(il1)
            ii2 = shd%lc%ILMOS(il2)
            iin = (ii2 - ii1) + 1

            !> Reset the exchange variables.
            i = 1
            irqst = mpi_request_null

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call mpi_testall(nvars, irqst, lstat, imstat, z)
            end do

        else if (inp > 1) then

            !> Receive data from worker nodes.
            do u = 1, (inp - 1)

                !> Get and assign the indices.
                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iin)
                ii1 = shd%lc%ILMOS(ii1)
                ii2 = shd%lc%ILMOS(ii2)
                iin = (ii2 - ii1) + 1

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

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, z)

    end subroutine

    subroutine run_within_grid_mpi_irecv(shd, cm)

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
        ii2 = shd%NA
        iin = (ii2 - ii1) + 1

        if (inp > 1 .and. ipid == 0) then

            !> Send data to worker nodes.
            do u = 1, (inp - 1)

                !> Reset exchange variables.
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
            !> Reset exchange variables.
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

    subroutine run_within_grid_stas_update(shd, cm)

        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for 'il1:il2' and 'i1:i2' indexing.
        use mpi_module

!+todo: There's a dependency on CLASSBD.f.
        use RUNCLASS36_constants, only: RHOW, RHOICE

        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        integer k, ki, kj
        real fcan(i1:i2), fsno(i1:i2), fpnd(i1:i2), frac

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Initialize variables.
        stas_grid%sfc%evap(i1:i2) = 0.0
        stas_grid%sfc%rofo(i1:i2) = 0.0
        stas_grid%sl%rofs(i1:i2) = 0.0
        stas_grid%lzs%rofb(i1:i2) = 0.0
        stas_grid%lzs%lqws(i1:i2) = 0.0
        stas_grid%dzs%rofb(i1:i2) = 0.0
        stas_grid%dzs%lqws(i1:i2) = 0.0
        stas_grid%cnpy%pevp(i1:i2) = 0.0
        stas_grid%cnpy%evpb(i1:i2) = 0.0
        stas_grid%cnpy%arrd(i1:i2) = 0.0
        stas_grid%cnpy%cmai(i1:i2) = 0.0
        stas_grid%cnpy%tcan(i1:i2) = 0.0
        stas_grid%sfc%alvs(i1:i2) = 0.0
        stas_grid%sfc%alir(i1:i2) = 0.0
        stas_grid%sfc%gte(i1:i2) = 0.0
        stas_grid%sfc%qevp(i1:i2) = 0.0
        stas_grid%sfc%hfs(i1:i2) = 0.0
        stas_grid%sfc%gzero(i1:i2) = 0.0
        stas_grid%sl%tbar(i1:i2, :) = 0.0
        stas_grid%sl%thic(i1:i2, :) = 0.0
        stas_grid%sl%fzws(i1:i2, :) = 0.0
        stas_grid%sl%thlq(i1:i2, :) = 0.0
        stas_grid%sl%lqws(i1:i2, :) = 0.0
        stas_grid%sl%gflx(i1:i2, :) = 0.0
        stas_grid%cnpy%rcan(i1:i2) = 0.0
        stas_grid%cnpy%sncan(i1:i2) = 0.0
        stas_grid%sno%sno(i1:i2) = 0.0
        stas_grid%sno%tsno(i1:i2)  = 0.0
        stas_grid%sno%wsno(i1:i2) = 0.0
        stas_grid%sfc%zpnd(i1:i2) = 0.0
        stas_grid%sfc%pndw(i1:i2) = 0.0
        stas_grid%sfc%tpnd(i1:i2) = 0.0

        !> Update variables.
        fcan(i1:i2) = 0.0
        fsno(i1:i2) = 0.0
        fpnd(i1:i2) = 0.0
        do k = il1, il2
            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)
            frac = shd%lc%ACLASS(ki, kj)
            stas_grid%sfc%evap(ki) = stas_grid%sfc%evap(ki) + stas%sfc%evap(k)*frac
            stas_grid%sfc%rofo(ki) = stas_grid%sfc%rofo(ki) + stas%sfc%rofo(k)*frac
            stas_grid%sl%rofs(ki) = stas_grid%sl%rofs(ki) + stas%sl%rofs(k)*frac
            stas_grid%lzs%rofb(ki) = stas_grid%lzs%rofb(ki) + stas%lzs%rofb(k)*frac
            stas_grid%lzs%lqws(ki) = stas_grid%lzs%lqws(ki) + stas%lzs%lqws(k)*frac
            stas_grid%dzs%rofb(ki) = stas_grid%dzs%rofb(ki) + stas%dzs%rofb(k)*frac
            stas_grid%dzs%lqws(ki) = stas_grid%dzs%lqws(ki) + stas%dzs%lqws(k)*frac
            stas_grid%cnpy%pevp(ki) = stas_grid%cnpy%pevp(ki) + stas%cnpy%pevp(k)*frac
            stas_grid%cnpy%evpb(ki) = stas_grid%cnpy%evpb(ki) + stas%cnpy%evpb(k)*frac
            stas_grid%cnpy%arrd(ki) = stas_grid%cnpy%arrd(ki) + stas%cnpy%arrd(k)*frac
            stas_grid%cnpy%cmai(ki) = stas_grid%cnpy%cmai(ki) + stas%cnpy%cmai(k)*frac
            if (stas%cnpy%tcan(k) > 0.0) then
                stas_grid%cnpy%tcan(ki) = stas_grid%cnpy%tcan(ki) + stas%cnpy%tcan(k)*frac
                fcan(ki) = fcan(ki) + frac
            end if
            stas_grid%sfc%alvs(ki) = stas_grid%sfc%alvs(ki) + stas%sfc%alvs(k)*frac
            stas_grid%sfc%alir(ki) = stas_grid%sfc%alir(ki) + stas%sfc%alir(k)*frac
            stas_grid%sfc%gte(ki) = stas_grid%sfc%gte(ki) + stas%sfc%gte(k)*frac
            stas_grid%sfc%qevp(ki) = stas_grid%sfc%qevp(ki) + stas%sfc%qevp(k)*frac
            stas_grid%sfc%hfs(ki) = stas_grid%sfc%hfs(ki) + stas%sfc%hfs(k)*frac
            stas_grid%sfc%gzero(ki) = stas_grid%sfc%gzero(ki) + stas%sfc%gzero(k)*frac
            stas_grid%sl%tbar(ki, :) = stas_grid%sl%tbar(ki, :) + stas%sl%tbar(k, :)*frac
            stas_grid%sl%thic(ki, :) = stas_grid%sl%thic(ki, :) + stas%sl%thic(k, :)*frac
            stas_grid%sl%fzws(ki, :) = stas_grid%sl%fzws(ki, :) + stas%sl%thic(k, :)*stas%sl%delzw(k, :)*frac*RHOICE
            stas_grid%sl%thlq(ki, :) = stas_grid%sl%thlq(ki, :) + stas%sl%thlq(k, :)*frac
            stas_grid%sl%lqws(ki, :) = stas_grid%sl%lqws(ki, :) + stas%sl%thlq(k, :)*stas%sl%delzw(k, :)*frac*RHOW
            stas_grid%sl%gflx(ki, :) = stas_grid%sl%gflx(ki, :) + stas%sl%gflx(k, :)*frac
            stas_grid%cnpy%rcan(ki) = stas_grid%cnpy%rcan(ki) + stas%cnpy%rcan(k)*frac
            stas_grid%cnpy%sncan(ki) = stas_grid%cnpy%sncan(ki) + stas%cnpy%sncan(k)*frac
            stas_grid%sno%sno(ki) = stas_grid%sno%sno(ki) + stas%sno%sno(k)*frac
            if (stas%sno%sno(k) > 0.0) then
                stas_grid%sno%wsno(ki) = stas_grid%sno%wsno(ki) + stas%sno%wsno(k)*frac
                stas_grid%sno%tsno(ki) = stas_grid%sno%tsno(ki) + stas%sno%tsno(k)*frac
                fsno(ki) = fsno(ki) + frac
            end if
            stas_grid%sfc%zpnd(ki) = stas_grid%sfc%zpnd(ki) + stas%sfc%zpnd(k)*frac
            if (stas%sfc%zpnd(k) > 0.0) then
                stas_grid%sfc%tpnd(ki) = stas_grid%sfc%tpnd(ki) + stas%sfc%tpnd(k)*frac
                fpnd(ki) = fpnd(ki) + frac
            end if
        end do
        where (stas_grid%sfc%evap(i1:i2) > 0.0 .and. stas_grid%cnpy%pevp(i1:i2) /= 0.0)
            stas_grid%cnpy%evpb(i1:i2) = stas_grid%sfc%evap(i1:i2)/stas_grid%cnpy%pevp(i1:i2)
        end where
        if (allocated(cm%dat(ck%RT)%GRD)) then
            where (stas_grid%cnpy%pevp(i1:i2) /= 0.0)
                stas_grid%cnpy%arrd(i1:i2) = cm%dat(ck%RT)%GRD(i1:i2)/stas_grid%cnpy%pevp(i1:i2)
            end where
        end if
        where (stas_grid%sfc%alvs(i1:i2) > 0.0 .and. stas_grid%sfc%alir(i1:i2) > 0.0)
            stas_grid%sfc%albt(i1:i2) = (stas_grid%sfc%alvs(i1:i2) + stas_grid%sfc%alir(i1:i2))/2.0
        end where
        stas_grid%sfc%pndw(i1:i2) = stas_grid%sfc%zpnd(i1:i2)*RHOW
        where (fcan(i1:i2) > 0.0) stas_grid%cnpy%tcan(i1:i2) = stas_grid%cnpy%tcan(i1:i2)/fcan(i1:i2)
        where (fsno(i1:i2) > 0.0)
            stas_grid%sno%tsno(i1:i2) = stas_grid%sno%tsno(i1:i2)/fsno(i1:i2)
        end where
        where (fpnd(i1:i2) > 0.0) stas_grid%sfc%tpnd(i1:i2) = stas_grid%sfc%tpnd(i1:i2)/fpnd(i1:i2)

    end subroutine

    subroutine run_within_grid_finalize(fls, shd, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

    end subroutine

end module
