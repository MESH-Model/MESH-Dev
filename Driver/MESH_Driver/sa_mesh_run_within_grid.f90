module sa_mesh_run_within_grid

    implicit none

    contains

    subroutine run_within_grid_init(shd, fls, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for 'il1:il2' and 'i1:i2' indexing.
        use mpi_module

!+todo: There's a dependency on CLASSBD.f.
        use RUNCLASS36_constants, only: RHOW, RHOICE

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        integer k, ki, kj
        real frac

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Initialize grid-based accumulators.
        stas_grid%sl%tbar(i1:i2, :) = 0.0
        stas_grid%sl%thic(i1:i2, :) = 0.0
        stas_grid%sl%fzws(i1:i2, :) = 0.0
        stas_grid%sl%thlq(i1:i2, :) = 0.0
        stas_grid%sl%lqws(i1:i2, :) = 0.0
        stas_grid%cnpy%rcan(i1:i2) = 0.0
        stas_grid%cnpy%sncan(i1:i2) = 0.0
        stas_grid%sno%sno(i1:i2) = 0.0
        stas_grid%sno%wsno(i1:i2) = 0.0
        stas_grid%sfc%zpnd(i1:i2) = 0.0
        stas_grid%sfc%pndw(i1:i2) = 0.0
        stas_grid%lzs%lqws(i1:i2) = 0.0
        stas_grid%dzs%lqws(i1:i2) = 0.0

        !> Aggregate grid-based accumulators.
        do k = il1, il2
            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)
            frac = shd%lc%ACLASS(ki, kj)
            stas_grid%sl%tbar(ki, :) = stas_grid%sl%tbar(ki, :) + stas%sl%tbar(k, :)*frac
            stas_grid%sl%thic(ki, :) = stas_grid%sl%thic(ki, :) + stas%sl%thic(k, :)*frac
            stas_grid%sl%fzws(ki, :) = stas_grid%sl%fzws(ki, :) + stas%sl%thic(k, :)*frac*stas%sl%delzw(k, :)*RHOICE
            stas_grid%sl%thlq(ki, :) = stas_grid%sl%thlq(ki, :) + stas%sl%thlq(k, :)*frac
            stas_grid%sl%lqws(ki, :) = stas_grid%sl%lqws(ki, :) + stas%sl%thlq(k, :)*frac*stas%sl%delzw(k, :)*RHOW
            stas_grid%cnpy%rcan(ki) = stas_grid%cnpy%rcan(ki) + stas%cnpy%rcan(k)*frac
            stas_grid%cnpy%sncan(ki) = stas_grid%cnpy%sncan(ki) + stas%cnpy%sncan(k)*frac
            stas_grid%sno%sno(ki) = stas_grid%sno%sno(ki) + stas%sno%sno(k)*frac
            if (stas%sno%sno(k) > 0.0) then
                stas_grid%sno%wsno(ki) = stas_grid%sno%wsno(ki) + stas%sno%wsno(k)*frac
            end if
            stas_grid%sfc%zpnd(ki) = stas_grid%sfc%zpnd(ki) + stas%sfc%zpnd(k)*frac
            stas_grid%lzs%lqws(ki) = stas_grid%lzs%lqws(ki) + stas%lzs%lqws(k)*frac
            stas_grid%dzs%lqws(ki) = stas_grid%dzs%lqws(ki) + stas%dzs%lqws(k)*frac
        end do
        stas_grid%sfc%pndw(i1:i2) = stas_grid%sfc%zpnd(i1:i2)*RHOW

    end subroutine

    subroutine run_within_grid(shd, fls, cm)

        use model_files_variables
        use sa_mesh_shared_variables
        use climate_forcing

        !> Required for 'il1:il2' and 'i1:i2' indexing.
        use mpi_module

!+todo: There's a dependency on CLASSBD.f.
        use RUNCLASS36_constants, only: RHOW, RHOICE

        !> Required for calls to processes.
        use baseflow_module

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        integer k, ki, kj
        real frac

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Initialize grid-based accumulators.
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
        stas_grid%sfc%qevp(i1:i2) = 0.0
        stas_grid%sfc%hfs(i1:i2)  = 0.0
        stas_grid%sl%tbar(i1:i2, :) = 0.0
        stas_grid%sl%thic(i1:i2, :) = 0.0
        stas_grid%sl%fzws(i1:i2, :) = 0.0
        stas_grid%sl%thlq(i1:i2, :) = 0.0
        stas_grid%sl%lqws(i1:i2, :) = 0.0
        stas_grid%sl%gflx(i1:i2, :) = 0.0
        stas_grid%cnpy%rcan(i1:i2) = 0.0
        stas_grid%cnpy%sncan(i1:i2) = 0.0
        stas_grid%sno%sno(i1:i2) = 0.0
        stas_grid%sno%wsno(i1:i2) = 0.0
        stas_grid%sfc%zpnd(i1:i2) = 0.0
        stas_grid%sfc%pndw(i1:i2) = 0.0

        !> Aggregate grid-based accumulators.
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
            stas_grid%sfc%qevp(ki) = stas_grid%sfc%qevp(ki) + stas%sfc%qevp(k)*frac
            stas_grid%sfc%hfs(ki)  = stas_grid%sfc%hfs(ki) + stas%sfc%hfs(k)*frac
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
            end if
            stas_grid%sfc%zpnd(ki) = stas_grid%sfc%zpnd(ki) + stas%sfc%zpnd(k)*frac
        end do
        stas_grid%sfc%pndw(i1:i2) = stas_grid%sfc%zpnd(i1:i2)*RHOW

        !> Call processes.
        call bflm_within_grid(fls, shd, cm)

    end subroutine

    subroutine run_within_grid_mpi_isend(shd, cm)

        !> For: MPI variables, barrier flag, il1:il2 parse utility
        use mpi_module

        !> Process modules (required for variables).
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer ipid_recv, nvars, itag, ierrcode, istop, i, j, u, iin, ii1, ii2, ierr
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
        itag = ic%ts_count*1000 + 200

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            !> Grab indices and reset exchange variables.
            ii1 = shd%lc%ILMOS(il1)
            ii2 = shd%lc%ILMOS(il2)
            iin = (ii2 - ii1) + 1

            i = 1
            irqst = mpi_request_null

!            call mpi_isend(stas_grid%chnl%s(ii1:ii2), iin, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call mpi_testall(nvars, irqst, lstat, imstat, ierr)
            end do

        else if (inp > 1) then

            !> Receive data from worker nodes.
            do u = 1, (inp - 1)

                !> Grab indices and reset exchange variables.
                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iin)
                ii1 = shd%lc%ILMOS(ii1)
                ii2 = shd%lc%ILMOS(ii2)
                iin = (ii2 - ii1) + 1

                i = 1
                irqst = mpi_request_null
                imstat = 0

!                call mpi_irecv(stas_grid%chnl%s(ii1:ii2), iin, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(nvars, irqst, lstat, imstat, ierr)
                end do

            end do !u = 1, (inp - 1)

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, ierr)

    end subroutine

    subroutine run_within_grid_mpi_irecv(shd, cm)

        !> For: MPI variables, barrier flag, il1:il2 parse utility
        use mpi_module

        !> Process modules (required for variables).
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer ipid_recv, nvars, itag, ierrcode, istop, i, j, u, iin, ii1, ii2, ierr
        logical lstat
        integer, allocatable :: irqst(:), imstat(:, :)

        !> Return if grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Count the number of active variables included in the exchange.
!        nvars = 0
!        if (fms%absp%n > 0) nvars = nvars + 1
        nvars = 1
        if (nvars == 0) return

        !> Exchange variables.
        if (allocated(irqst)) deallocate(irqst)
        if (allocated(imstat)) deallocate(imstat)
        allocate(irqst(nvars), imstat(mpi_status_size, nvars))
        itag = ic%ts_count*1000 + 400

        if (inp > 1 .and. ipid == 0) then

            !> Send data to worker nodes.
            do u = 1, (inp - 1)

                !> Grab indices and reset exchange variables.
                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iin)
                ii1 = 1
                ii2 = shd%NA
                iin = (ii2 - ii1) + 1

                i = 1
                irqst = mpi_request_null
                imstat = 0

!                if (fms%absp%n > 0) then
                    call mpi_isend(stas_grid%chnl%s(ii1:ii2), iin, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
!                end if

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(nvars, irqst, lstat, imstat, ierr)
                end do

            end do !u = 1, (inp - 1)

        else if (inp > 1) then

            !> Receive data from head-node.
            !> Grab indices and reset exchange variables.
            ii1 = 1
            ii2 = shd%NA
            iin = (ii2 - ii1) + 1

            i = 1
            irqst = mpi_request_null

!            if (fms%absp%n > 0) then
                call mpi_irecv(stas_grid%chnl%s(ii1:ii2), iin, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
!            end if

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call mpi_testall(nvars, irqst, lstat, imstat, ierr)
            end do

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, ierr)

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
