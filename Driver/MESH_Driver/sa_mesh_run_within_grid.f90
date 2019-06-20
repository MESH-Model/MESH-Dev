module sa_mesh_run_within_grid

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

    subroutine run_within_grid_init(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Update variables.
        call run_within_grid_stas_update(fls, shd, cm)

    end subroutine

    subroutine run_within_grid(fls, shd, cm)

        !> Process modules.
        use baseflow_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Update variables.
        call run_within_grid_stas_update(fls, shd, cm)

        !> Call processes.
        call bflm_within_grid(fls, shd, cm)

    end subroutine

    subroutine run_within_grid_mpi_isend(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer nvars, t, i, j, u, ii1, ii2, iin, z
        logical lstat
        integer, allocatable :: irqst(:), imstat(:, :)

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Count the number of active variables included in the exchange.
        nvars = 0
        if (nvars == 0) return

        !> Exchange variables.
        if (allocated(irqst)) deallocate(irqst)
        if (allocated(imstat)) deallocate(imstat)
        allocate(irqst(nvars), imstat(MPI_STATUS_SIZE, nvars))
        t = ic%ts_count*1000 + 200

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            !> Assign the indices.
            ii1 = shd%lc%ILMOS(il1)
            ii2 = shd%lc%ILMOS(il2)
            iin = (ii2 - ii1) + 1

            !> Reset the exchange variables.
            i = 1
            irqst = MPI_REQUEST_NULL

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call MPI_Testall(nvars, irqst, lstat, imstat, z)
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
                irqst = MPI_REQUEST_NULL
                imstat = 0

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
                    call MPI_Testall(nvars, irqst, lstat, imstat, z)
                end do

            end do !u = 1, (inp - 1)

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, z)

    end subroutine

    subroutine run_within_grid_mpi_irecv(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer nvars, t, i, j, u, ii1, ii2, iin, z
        logical lstat
        integer, allocatable :: irqst(:), imstat(:, :)
        real, dimension(:), allocatable :: chnl

        !> Return if tile and grid processes are not active.
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
        ii2 = shd%NA
        iin = (ii2 - ii1) + 1

        !> Allocate temporary arrays.
        allocate(chnl(iin)) !3*iin if diversion/abstraction

        if (inp > 1 .and. ipid == 0) then

            !> Send data to worker nodes.
            do u = 1, (inp - 1)

                !> Reset exchange variables.
                i = 1
                irqst = MPI_REQUEST_NULL
                imstat = 0

                !> Channel routing.
!                chnl((1 + iin*0):(iin*1)) = vs%grid%stg(ii1:ii2)
!                chnl((1 + iin*1):(iin*2)) = vs%grid%div(ii1:ii2)
!                chnl((1 + iin*2):(iin*3)) = vs%grid%ab(ii1:ii2)
!                call MPI_Isend(chnl, size(chnl), MPI_REAL, u, t + i, MPI_COMM_WORLD, irqst(i), z)
!                i = i + 1

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
                    call MPI_Testall(nvars, irqst, lstat, imstat, z)
                end do

            end do !u = 1, (inp - 1)

        else if (inp > 1) then

            !> Receive data from head-node.
            !> Reset exchange variables.
            i = 1
            irqst = MPI_REQUEST_NULL

            !> Receive variables.
!            call MPI_Irecv(chnl, size(chnl), MPI_REAL, 0, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call MPI_Testall(nvars, irqst, lstat, imstat, z)
            end do

            !> Assign variables.

            !> Channel routing.
!            vs%grid%stg(ii1:ii2) = chnl((1 + iin*0):(iin*1))
!            vs%grid%div(ii1:ii2) = chnl((1 + iin*1):(iin*2))
!            vs%grid%ab(ii1:ii2) = chnl((1 + iin*2):(iin*3))

        end if !(inp > 1 .and. ipid /= 0) then

        !> Deallocate temporary arrays.
        deallocate(chnl)

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) call MPI_Barrier(MPI_COMM_WORLD, z)

    end subroutine

    subroutine run_within_grid_stas_update(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer k, ki, kj
        real fcan(i1:i2), fsno(i1:i2), fpnd(i1:i2), frac

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Initialize variables.
        vs%grid%rcan(i1:i2) = 0.0
        vs%grid%sncan(i1:i2) = 0.0
        vs%grid%cmas(i1:i2) = 0.0
        vs%grid%tcan(i1:i2) = 0.0
        vs%grid%gro(i1:i2) = 0.0
        vs%grid%zsno(i1:i2) = 0.0
        vs%grid%rhos(i1:i2) = 0.0
        vs%grid%sno(i1:i2) = 0.0
        vs%grid%wsno(i1:i2) = 0.0
        vs%grid%tsno(i1:i2) = 0.0
        vs%grid%albt(i1:i2) = 0.0
        vs%grid%alvs(i1:i2) = 0.0
        vs%grid%alir(i1:i2) = 0.0
        vs%grid%gte(i1:i2) = 0.0
        vs%grid%zpnd(i1:i2) = 0.0
        vs%grid%pndw(i1:i2) = 0.0
        vs%grid%tpnd(i1:i2) = 0.0
        vs%grid%fstr(i1:i2) = 0.0
        vs%grid%pevp(i1:i2) = 0.0
        vs%grid%evap(i1:i2) = 0.0
        vs%grid%evpb(i1:i2) = 0.0
        vs%grid%arrd(i1:i2) = 0.0
        vs%grid%rofo(i1:i2) = 0.0
        vs%grid%qevp(i1:i2) = 0.0
        vs%grid%hfs(i1:i2) = 0.0
        vs%grid%gzero(i1:i2) = 0.0
        vs%grid%rofs(i1:i2) = 0.0
        vs%grid%thic(i1:i2, :) = 0.0
        vs%grid%fzws(i1:i2, :) = 0.0
        vs%grid%thlq(i1:i2, :) = 0.0
        vs%grid%lqws(i1:i2, :) = 0.0
        vs%grid%tbar(i1:i2, :) = 0.0
        vs%grid%gflx(i1:i2, :) = 0.0
        vs%grid%lzs(i1:i2) = 0.0
        vs%grid%dzs(i1:i2) = 0.0
        vs%grid%rofb(i1:i2) = 0.0

        !> Update variables.
        fcan(i1:i2) = 0.0
        fsno(i1:i2) = 0.0
        fpnd(i1:i2) = 0.0
        do k = il1, il2
            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)
            frac = shd%lc%ACLASS(ki, kj)
            vs%grid%rcan(ki) = vs%grid%rcan(ki) + vs%tile%rcan(k)*frac
            vs%grid%sncan(ki) = vs%grid%sncan(ki) + vs%tile%sncan(k)*frac
            if (vs%tile%tcan(k) > 0.0) then
                vs%grid%cmas(ki) = vs%grid%cmas(ki) + vs%tile%cmas(k)*frac
                vs%grid%tcan(ki) = vs%grid%tcan(ki) + vs%tile%tcan(k)*frac
                vs%grid%gro(ki) = vs%grid%gro(ki) + vs%tile%gro(k)*frac
                fcan(ki) = fcan(ki) + frac
            end if
            vs%grid%sno(ki) = vs%grid%sno(ki) + vs%tile%sno(k)*frac
            if (vs%tile%sno(k) > 0.0) then
                vs%grid%wsno(ki) = vs%grid%wsno(ki) + vs%tile%wsno(k)*frac
                vs%grid%tsno(ki) = vs%grid%tsno(ki) + vs%tile%tsno(k)*frac
                vs%grid%rhos(ki) = vs%grid%rhos(ki) + vs%tile%rhos(k)*frac
                fsno(ki) = fsno(ki) + frac
            end if
            vs%grid%albt(ki) = vs%grid%albt(ki) + vs%tile%albt(k)*frac
            vs%grid%alvs(ki) = vs%grid%alvs(ki) + vs%tile%alvs(k)*frac
            vs%grid%alir(ki) = vs%grid%alir(ki) + vs%tile%alir(k)*frac
            vs%grid%gte(ki) = vs%grid%gte(ki) + vs%tile%gte(k)*frac
            vs%grid%zpnd(ki) = vs%grid%zpnd(ki) + vs%tile%zpnd(k)*frac
            if (vs%tile%zpnd(k) > 0.0) then
                vs%grid%pndw(ki) = vs%grid%pndw(ki) + vs%tile%pndw(k)*frac
                vs%grid%tpnd(ki) = vs%grid%tpnd(ki) + vs%tile%tpnd(k)*frac
                vs%grid%fstr(ki) = vs%grid%fstr(ki) + vs%tile%fstr(k)*frac
                fpnd(ki) = fpnd(ki) + frac
            end if
            vs%grid%pevp(ki) = vs%grid%pevp(ki) + vs%tile%pevp(k)*frac
            vs%grid%evap(ki) = vs%grid%evap(ki) + vs%tile%evap(k)*frac
            vs%grid%evpb(ki) = vs%grid%evpb(ki) + vs%tile%evpb(k)*frac
            vs%grid%arrd(ki) = vs%grid%arrd(ki) + vs%tile%arrd(k)*frac
            vs%grid%rofo(ki) = vs%grid%rofo(ki) + vs%tile%rofo(k)*frac
            vs%grid%qevp(ki) = vs%grid%qevp(ki) + vs%tile%qevp(k)*frac
            vs%grid%hfs(ki) = vs%grid%hfs(ki) + vs%tile%hfs(k)*frac
            vs%grid%gzero(ki) = vs%grid%gzero(ki) + vs%tile%gzero(k)*frac
            vs%grid%rofs(ki) = vs%grid%rofs(ki) + vs%tile%rofs(k)*frac
            vs%grid%thic(ki, :) = vs%grid%thic(ki, :) + vs%tile%thic(k, :)*frac
            vs%grid%fzws(ki, :) = vs%grid%fzws(ki, :) + vs%tile%fzws(k, :)*frac
            vs%grid%thlq(ki, :) = vs%grid%thlq(ki, :) + vs%tile%thlq(k, :)*frac
            vs%grid%lqws(ki, :) = vs%grid%lqws(ki, :) + vs%tile%lqws(k, :)*frac
            vs%grid%tbar(ki, :) = vs%grid%tbar(ki, :) + vs%tile%tbar(k, :)*frac
            vs%grid%gflx(ki, :) = vs%grid%gflx(ki, :) + vs%tile%gflx(k, :)*frac
            vs%grid%lzs(ki) = vs%grid%lzs(ki) + vs%tile%lzs(k)*frac
            vs%grid%dzs(ki) = vs%grid%dzs(ki) + vs%tile%dzs(k)*frac
            vs%grid%rofb(ki) = vs%grid%rofb(ki) + vs%tile%rofb(k)*frac
        end do
        where (fcan(i1:i2) > 0.0)
            vs%grid%cmas(i1:i2) = vs%grid%cmas(i1:i2)/fcan(i1:i2)
            vs%grid%tcan(i1:i2) = vs%grid%tcan(i1:i2)/fcan(i1:i2)
            vs%grid%gro(i1:i2) = vs%grid%gro(i1:i2)/fcan(i1:i2)
        end where
        where (fsno(i1:i2) > 0.0)
            vs%grid%tsno(i1:i2) = vs%grid%tsno(i1:i2)/fsno(i1:i2)
            vs%grid%rhos(i1:i2) = vs%grid%rhos(i1:i2)/fsno(i1:i2)
        end where
        where (vs%grid%rhos(i1:i2) > 0.0)
            vs%grid%zsno(i1:i2) = vs%grid%sno(i1:i2)/vs%grid%rhos(i1:i2)
        end where
        where (fpnd(i1:i2) > 0.0) vs%grid%tpnd(i1:i2) = vs%grid%tpnd(i1:i2)/fpnd(i1:i2)

    end subroutine

    subroutine run_within_grid_finalize(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

    end subroutine

end module
