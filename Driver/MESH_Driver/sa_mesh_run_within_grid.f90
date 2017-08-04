module sa_mesh_run_within_grid

    implicit none

    contains

    subroutine run_within_grid_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

!+todo: There's a dependency on CLASSBD.f (block data, though described below as module)
        use RUNCLASS36_constants, only: RHOW, RHOICE

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        integer k, ki, kj, i1, i2
        real FRAC

        i1 = shd%lc%ILMOS(il1)
        i2 = shd%lc%ILMOS(il2)

!>>>irrigation
        if (inp > 1 .and. ipid > 0) allocate(stas%chnl%s(i1:i2))
!<<<irrigation

        !> Initialize grid-based states.
        do k = il1, il2

            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)

            FRAC = shd%lc%ACLASS(ki, kj)*shd%FRAC(ki)

            if (FRAC > 0.0) then
                sp%TBAR(ki, :) = sp%TBAR(ki, :) + stas%sl%tbar(k, :)*shd%lc%ACLASS(ki, kj)
                sp%THIC(ki, :) = sp%THIC(ki, :) + stas%sl%thic(k, :)*FRAC
                wb%FRWS(ki, :) = wb%FRWS(ki, :) + stas%sl%thic(k, :)*stas%sl%delzw(k, :)*FRAC*RHOICE
                sp%THLQ(ki, :) = sp%THLQ(ki, :) + stas%sl%thlq(k, :)*FRAC
                wb%LQWS(ki, :) = wb%LQWS(ki, :) + stas%sl%thlq(k, :)*stas%sl%delzw(k, :)*FRAC*RHOW
                wb%RCAN(ki) = wb%RCAN(ki) + stas%cnpy%rcan(k)*FRAC
                wb%SNCAN(ki) = wb%SNCAN(ki) + stas%cnpy%sncan(k)*FRAC
                wb%SNO(ki) = wb%SNO(ki) + stas%sno%sno(k)*FRAC
                if (stas%sno%sno(k) > 0.0) then
                    wb%WSNO(ki) = wb%WSNO(ki) + stas%sno%wsno(k)*FRAC
                end if
                wb%PNDW(ki) = wb%PNDW(ki) + stas%sfc%zpnd(k)*FRAC*RHOW
            end if

        end do

        wb%STG(i1:i2) = &
            wb%RCAN(i1:i2) + wb%SNCAN(i1:i2) + wb%SNO(i1:i2) + wb%WSNO(i1:i2) + wb%PNDW(i1:i2) + &
            sum(wb%LQWS(i1:i2, :), 2) + sum(wb%FRWS(i1:i2, :), 2)

    end subroutine

    subroutine run_within_grid(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_parameters
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

!+todo: There's a dependency on CLASSBD.f (block data, though described below as module)
        use RUNCLASS36_constants, only: RHOW, RHOICE

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        integer k, ki, kj, i1, i2
        real FRAC

        i1 = shd%lc%ILMOS(il1)
        i2 = shd%lc%ILMOS(il2)

        !> Update grid based states.
        do k = il1, il2

            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)

            FRAC = shd%lc%ACLASS(ki, kj)*shd%FRAC(ki)

            if (FRAC > 0.0) then
                wb%PRE(ki) = wb%PRE(ki) + cm%dat(ck%RT)%GAT(k)*FRAC*ic%dts
                wb%EVAP(ki) = wb%EVAP(ki) + stas%sfc%evap(k)*FRAC*ic%dts
                wb%ROF(ki) = wb%ROF(ki) + (stas%sfc%rofo(k) + stas%sl%rofs(k) + stas%lzs%rofb(k) + stas%dzs%rofb(k))*FRAC*ic%dts
                wb%ROFO(ki) = wb%ROFO(ki) + stas%sfc%rofo(k)*FRAC*ic%dts
                wb%ROFS(ki) = wb%ROFS(ki) + stas%sl%rofs(k)*FRAC*ic%dts
                wb%ROFB(ki) = wb%ROFB(ki) + (stas%lzs%rofb(k) + stas%dzs%rofb(k))*FRAC*ic%dts
                wb%pevp(ki) = wb%pevp(ki) + stas%cnpy%pevp(k)*FRAC*ic%dts
                wb%evpb(ki) = wb%evpb(ki) + stas%cnpy%evpb(k)*FRAC
                wb%arrd(ki) = wb%arrd(ki) + stas%cnpy%arrd(k)*FRAC
                eb%QEVP(ki) = eb%QEVP(ki) + stas%sfc%qevp(k)*FRAC
                eb%HFS(ki)  = eb%HFS(ki) + stas%sfc%hfs(k)*FRAC
                sp%TBAR(ki, :) = sp%TBAR(ki, :) + stas%sl%tbar(k, :)*shd%lc%ACLASS(ki, kj)
                sp%THIC(ki, :) = sp%THIC(ki, :) + stas%sl%thic(k, :)*FRAC
                wb%FRWS(ki, :) = wb%FRWS(ki, :) + stas%sl%thic(k, :)*stas%sl%delzw(k, :)*FRAC*RHOICE
                sp%THLQ(ki, :) = sp%THLQ(ki, :) + stas%sl%thlq(k, :)*FRAC
                wb%LQWS(ki, :) = wb%LQWS(ki, :) + stas%sl%thlq(k, :)*stas%sl%delzw(k, :)*FRAC*RHOW
                eb%GFLX(ki, :) = eb%GFLX(ki, :) + stas%sl%gflx(k, :)*FRAC
                wb%RCAN(ki) = wb%RCAN(ki) + stas%cnpy%rcan(k)*FRAC
                wb%SNCAN(ki) = wb%SNCAN(ki) + stas%cnpy%sncan(k)*FRAC
                wb%SNO(ki) = wb%SNO(ki) + stas%sno%sno(k)*FRAC
                if (stas%sno%sno(k) > 0.0) then
                    wb%WSNO(ki) = wb%WSNO(ki) + stas%sno%wsno(k)*FRAC
                end if
                wb%PNDW(ki) = wb%PNDW(ki) + stas%sfc%zpnd(k)*FRAC*RHOW
            end if

        end do

        wb%DSTG(i1:i2) = &
            wb%RCAN(i1:i2) + wb%SNCAN(i1:i2) + wb%SNO(i1:i2) + wb%WSNO(i1:i2) + wb%PNDW(i1:i2) + &
            sum(wb%LQWS(i1:i2, :), 2) + sum(wb%FRWS(i1:i2, :), 2) - &
            wb%STG(i1:i2)
        wb%STG(i1:i2) = wb%DSTG(i1:i2) + wb%STG(i1:i2)

    end subroutine

    subroutine run_within_grid_mpi_isend(shd, cm)

        !> For: MPI variables, barrier flag, il1:il2 parse utility
        use mpi_flags
        use mpi_shared_variables
        use mpi_module
        use mpi_utilities

        !> For: Model states, 'ic', 'cm'.
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer ipid_recv, itag, ierrcode, istop, i, j, u, invars, iin, ii1, ii2, ierr
        logical lstat
        integer, dimension(:), allocatable :: irqst
        integer, dimension(:, :), allocatable :: imstat

        !> Gather variables from parallel nodes.

        !> Send/receive process.
        itag = ic%ts_count*1000
        invars = 0

!>>>irrigation
        invars = invars + 1
!<<<irrigation

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))
            irqst = mpi_request_null

            ii1 = shd%lc%ILMOS(il1)
            ii2 = shd%lc%ILMOS(il2)
            iin = (ii2 - ii1) + 1

            i = 1
!>>>irrigation
            call mpi_isend(stas%chnl%s(ii1:ii2), iin, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
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

                irqst = mpi_request_null
                imstat = 0

                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iin)
                ii1 = shd%lc%ILMOS(ii1)
                ii2 = shd%lc%ILMOS(ii2)
                iin = (ii2 - ii1) + 1

                i = 1
!>>>irrigation
                call mpi_irecv(stas%chnl%s(ii1:ii2), iin, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
!<<<irrigation

                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(invars, irqst, lstat, imstat, ierr)
                end do

            end do !u = 1, (inp - 1)

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, ierr)

    end subroutine

    subroutine run_within_grid_mpi_irecv(shd, cm)

        !> For: MPI variables, barrier flag, il1:il2 parse utility
        use mpi_flags
        use mpi_shared_variables
        use mpi_module
        use mpi_utilities

        !> For: Model states, 'ic', 'cm'.
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer ipid_recv, itag, ierrcode, istop, i, j, u, invars, iin, ii1, ii2, ierr
        logical lstat
        integer, dimension(:), allocatable :: irqst
        integer, dimension(:, :), allocatable :: imstat

        !> Gather variables from parallel nodes.

        !> Send/receive process.
        itag = ic%ts_count*1000
        invars = 0

!>>>irrigation
        invars = invars + 1
!<<<irrigation

        if (inp > 1 .and. ipid == 0) then

            !> Send data to worker nodes.
            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))

            !> Receive and assign variables.
            do u = 1, (inp - 1)

                irqst = mpi_request_null
                imstat = 0

                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iin)
                ii1 = shd%lc%ILMOS(ii1)
                ii2 = shd%lc%ILMOS(ii2)
                iin = (ii2 - ii1) + 1

!                print *, ipid, ' sending ' , ii1, ' to ', ii2, ', to ', u, ' on ', itag

                i = 1
!>>>irrigation
                call mpi_isend(stas%chnl%s(ii1:ii2), iin, mpi_real, u, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
!<<<irrigation

                lstat = .false.
                do while (.not. lstat)
                    call mpi_testall(invars, irqst, lstat, imstat, ierr)
                end do

!                print *, ipid, ' done sending to ', u

            end do !u = 1, (inp - 1)

        else if (inp > 1) then

            !> Receive data from head-node.
            if (allocated(irqst)) deallocate(irqst)
            if (allocated(imstat)) deallocate(imstat)
            allocate(irqst(invars), imstat(mpi_status_size, invars))
            irqst = mpi_request_null

            ii1 = shd%lc%ILMOS(il1)
            ii2 = shd%lc%ILMOS(il2)
            iin = (ii2 - ii1) + 1

!            print *, ipid, ' receiving ' , ii1, ' to ', ii2, ', from ', 0, ' on ', itag

            i = 1
!>>>irrigation
            call mpi_irecv(stas%chnl%s(ii1:ii2), iin, mpi_real, 0, itag + i, mpi_comm_world, irqst(i), ierr); i = i + 1
!<<<irrigation

            lstat = .false.
            do while (.not. lstat)
                call mpi_testall(invars, irqst, lstat, imstat, ierr)
            end do

!            print *, ipid, ' done receiving from ', 0

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, ierr)

    end subroutine

    subroutine run_within_grid_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        use model_files_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

    end subroutine

end module
