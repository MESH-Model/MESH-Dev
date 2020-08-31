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

        if (inp > 1 .and. ipid /= 0) then

            !> Send data back to head-node.
            !> Assign the indices.
            ii1 = shd%lc%ILMOS(il1)
            ii2 = shd%lc%ILMOS(il2)
            iin = (ii2 - ii1) + 1

            !> Reset the exchange variables.
            irqst = MPI_REQUEST_NULL
            t = itag
            i = 1

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
                irqst = MPI_REQUEST_NULL
                imstat = 0
                t = itag
                i = 1

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
                    call MPI_Testall(nvars, irqst, lstat, imstat, z)
                end do

            end do !u = 1, (inp - 1)

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) then
            call MPI_Barrier(MPI_COMM_WORLD, z)
            itag = 0
        else
            itag = t + i
        end if

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
                irqst = MPI_REQUEST_NULL
                imstat = 0
                t = itag
                i = 1

                !> Channel routing.
!                chnl((1 + iin*0):(iin*1)) = vs%grid%stgch(ii1:ii2)
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
            irqst = MPI_REQUEST_NULL
            t = itag
            i = 1

            !> Receive variables.
!            call MPI_Irecv(chnl, size(chnl), MPI_REAL, 0, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call MPI_Testall(nvars, irqst, lstat, imstat, z)
            end do

            !> Assign variables.

            !> Channel routing.
!            vs%grid%stgch(ii1:ii2) = chnl((1 + iin*0):(iin*1))
!            vs%grid%div(ii1:ii2) = chnl((1 + iin*1):(iin*2))
!            vs%grid%ab(ii1:ii2) = chnl((1 + iin*2):(iin*3))

        end if !(inp > 1 .and. ipid /= 0) then

        !> Deallocate temporary arrays.
        deallocate(chnl)

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) then
            call MPI_Barrier(MPI_COMM_WORLD, z)
            itag = 0
        else
            itag = t + i
        end if

    end subroutine

    subroutine run_within_grid_stas_update(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer k, ki, kj
        real tpndfrac(i1:i2), tsnofrac(i1:i2), tcanfrac(i1:i2), frac

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Initialize variables.
!        if (associated(vs%grid%fsin)) vs%grid%fsin(i1:i2) = 0.0
!        if (associated(vs%grid%fsdr)) vs%grid%fsdr(i1:i2) = 0.0
!        if (associated(vs%grid%fsdff)) vs%grid%fsdff(i1:i2) = 0.0
!        if (associated(vs%grid%flin)) vs%grid%flin(i1:i2) = 0.0
!        if (associated(vs%grid%ta)) vs%grid%ta(i1:i2) = 0.0
!        if (associated(vs%grid%qa)) vs%grid%qa(i1:i2) = 0.0
!        if (associated(vs%grid%pres)) vs%grid%pres(i1:i2) = 0.0
!        if (associated(vs%grid%uv)) vs%grid%uv(i1:i2) = 0.0
!        if (associated(vs%grid%wdir)) vs%grid%wdir(i1:i2) = 0.0
!        if (associated(vs%grid%uu)) vs%grid%uu(i1:i2) = 0.0
!        if (associated(vs%grid%vv)) vs%grid%vv(i1:i2) = 0.0
!        if (associated(vs%grid%pre)) vs%grid%pre(i1:i2) = 0.0
        if (associated(vs%grid%prern)) vs%grid%prern(i1:i2) = 0.0
        if (associated(vs%grid%presno)) vs%grid%presno(i1:i2) = 0.0
        if (associated(vs%grid%rcan)) vs%grid%rcan(i1:i2) = 0.0
        if (associated(vs%grid%sncan)) vs%grid%sncan(i1:i2) = 0.0
        if (associated(vs%grid%cmas)) vs%grid%cmas(i1:i2) = 0.0
        if (associated(vs%grid%tac)) vs%grid%tac(i1:i2) = 0.0
        if (associated(vs%grid%qac)) vs%grid%qac(i1:i2) = 0.0
        if (associated(vs%grid%tcan)) vs%grid%tcan(i1:i2) = 0.0
        if (associated(vs%grid%gro)) vs%grid%gro(i1:i2) = 0.0
        if (associated(vs%grid%sno)) vs%grid%sno(i1:i2) = 0.0
        if (associated(vs%grid%rhos)) vs%grid%rhos(i1:i2) = 0.0
        if (associated(vs%grid%zsno)) vs%grid%zsno(i1:i2) = 0.0
        if (associated(vs%grid%fsno)) vs%grid%fsno(i1:i2) = 0.0
        if (associated(vs%grid%albs)) vs%grid%albs(i1:i2) = 0.0
        if (associated(vs%grid%wsno)) vs%grid%wsno(i1:i2) = 0.0
        if (associated(vs%grid%tsno)) vs%grid%tsno(i1:i2) = 0.0
        if (associated(vs%grid%rofsno)) vs%grid%rofsno(i1:i2) = 0.0
        if (associated(vs%grid%albt)) vs%grid%albt(i1:i2) = 0.0
        if (associated(vs%grid%alvs)) vs%grid%alvs(i1:i2) = 0.0
        if (associated(vs%grid%alir)) vs%grid%alir(i1:i2) = 0.0
        if (associated(vs%grid%gte)) vs%grid%gte(i1:i2) = 0.0
        if (associated(vs%grid%zpnd)) vs%grid%zpnd(i1:i2) = 0.0
        if (associated(vs%grid%pndw)) vs%grid%pndw(i1:i2) = 0.0
        if (associated(vs%grid%tpnd)) vs%grid%tpnd(i1:i2) = 0.0
        if (associated(vs%grid%fstr)) vs%grid%fstr(i1:i2) = 0.0
        if (associated(vs%grid%pevp)) vs%grid%pevp(i1:i2) = 0.0
        if (associated(vs%grid%evap)) vs%grid%evap(i1:i2) = 0.0
        if (associated(vs%grid%evpb)) vs%grid%evpb(i1:i2) = 0.0
        if (associated(vs%grid%arrd)) vs%grid%arrd(i1:i2) = 0.0
        if (associated(vs%grid%rofo)) vs%grid%rofo(i1:i2) = 0.0
        if (associated(vs%grid%qevp)) vs%grid%qevp(i1:i2) = 0.0
        if (associated(vs%grid%hfs)) vs%grid%hfs(i1:i2) = 0.0
        if (associated(vs%grid%gzero)) vs%grid%gzero(i1:i2) = 0.0
        if (associated(vs%grid%tsfs)) vs%grid%tsfs(i1:i2, :) = 0.0
        if (associated(vs%grid%ggeo)) vs%grid%ggeo(i1:i2) = 0.0
        if (associated(vs%grid%tbas)) vs%grid%tbas(i1:i2) = 0.0
        if (associated(vs%grid%thlq)) vs%grid%thlq(i1:i2, :) = 0.0
        if (associated(vs%grid%thic)) vs%grid%thic(i1:i2, :) = 0.0
        if (associated(vs%grid%lqws)) vs%grid%lqws(i1:i2, :) = 0.0
        if (associated(vs%grid%fzws)) vs%grid%fzws(i1:i2, :) = 0.0
        if (associated(vs%grid%tbar)) vs%grid%tbar(i1:i2, :) = 0.0
        if (associated(vs%grid%gflx)) vs%grid%gflx(i1:i2, :) = 0.0
        if (associated(vs%grid%rofs)) vs%grid%rofs(i1:i2, :) = 0.0
        if (associated(vs%grid%delzw)) vs%grid%delzw(i1:i2, :) = 0.0
        if (associated(vs%grid%zbotw)) vs%grid%zbotw(i1:i2, :) = 0.0
        if (associated(vs%grid%rofb)) vs%grid%rofb(i1:i2) = 0.0
        if (associated(vs%grid%rchg)) vs%grid%rchg(i1:i2) = 0.0
        if (associated(vs%grid%lzs)) vs%grid%lzs(i1:i2) = 0.0
        if (associated(vs%grid%dzs)) vs%grid%dzs(i1:i2) = 0.0
        if (associated(vs%grid%stge)) vs%grid%stge(i1:i2) = 0.0
        if (associated(vs%grid%stgw)) vs%grid%stgw(i1:i2) = 0.0

        !> Update variables.
        tcanfrac(i1:i2) = 0.0
        tsnofrac(i1:i2) = 0.0
        tpndfrac(i1:i2) = 0.0
        do k = il1, il2
            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)
            frac = shd%lc%ACLASS(ki, kj)
!            if (associated(vs%grid%fsin) .and. associated(vs%tile%fsin)) then
!                vs%grid%fsin(ki) = vs%grid%fsin(ki) + vs%tile%fsin(k)*frac
!            end if
!            if (associated(vs%grid%fsdr) .and. associated(vs%tile%fsdr)) then
!                vs%grid%fsdr(ki) = vs%grid%fsdr(ki) + vs%tile%fsdr(k)*frac
!            end if
!            if (associated(vs%grid%fsdff) .and. associated(vs%tile%fsdff)) then
!                vs%grid%fsdff(ki) = vs%grid%fsdff(ki) + vs%tile%fsdff(k)*frac
!            end if
!            if (associated(vs%grid%flin) .and. associated(vs%tile%flin)) then
!                vs%grid%flin(ki) = vs%grid%flin(ki) + vs%tile%flin(k)*frac
!            end if
!            if (associated(vs%grid%ta) .and. associated(vs%tile%ta)) then
!                vs%grid%ta(ki) = vs%grid%ta(ki) + vs%tile%ta(k)*frac
!            end if
!            if (associated(vs%grid%qa) .and. associated(vs%tile%qa)) then
!                vs%grid%qa(ki) = vs%grid%qa(ki) + vs%tile%qa(k)*frac
!            end if
!            if (associated(vs%grid%pres) .and. associated(vs%tile%pres)) then
!                vs%grid%pres(ki) = vs%grid%pres(ki) + vs%tile%pres(k)*frac
!            end if
!            if (associated(vs%grid%uv) .and. associated(vs%tile%uv)) then
!                vs%grid%uv(ki) = vs%grid%uv(ki) + vs%tile%uv(k)*frac
!            end if
!            if (associated(vs%grid%wdir) .and. associated(vs%tile%wdir)) then
!                vs%grid%wdir(ki) = vs%grid%wdir(ki) + vs%tile%wdir(k)*frac
!            end if
!            if (associated(vs%grid%uu) .and. associated(vs%tile%uu)) then
!                vs%grid%uu(ki) = vs%grid%uu(ki) + vs%tile%uu(k)*frac
!            end if
!            if (associated(vs%grid%vv) .and. associated(vs%tile%vv)) then
!                vs%grid%vv(ki) = vs%grid%vv(ki) + vs%tile%vv(k)*frac
!            end if
!            if (associated(vs%grid%pre) .and. associated(vs%tile%pre)) then
!                vs%grid%pre(ki) = vs%grid%pre(ki) + vs%tile%pre(k)*frac
!            end if
            if (associated(vs%grid%prern) .and. associated(vs%tile%prern)) then
                vs%grid%prern(ki) = vs%grid%prern(ki) + vs%tile%prern(k)*frac
            end if
            if (associated(vs%grid%presno) .and. associated(vs%tile%presno)) then
                vs%grid%presno(ki) = vs%grid%presno(ki) + vs%tile%presno(k)*frac
            end if
            if (associated(vs%grid%rcan) .and. associated(vs%tile%rcan)) then
                vs%grid%rcan(ki) = vs%grid%rcan(ki) + vs%tile%rcan(k)*frac
            end if
            if (associated(vs%grid%sncan) .and. associated(vs%tile%sncan)) then
                vs%grid%sncan(ki) = vs%grid%sncan(ki) + vs%tile%sncan(k)*frac
            end if
            if (associated(vs%grid%cmas) .and. associated(vs%tile%cmas)) then
                vs%grid%cmas(ki) = vs%grid%cmas(ki) + vs%tile%cmas(k)*frac
            end if
            if (associated(vs%grid%tac) .and. associated(vs%tile%tac)) then
                vs%grid%tac(ki) = vs%grid%tac(ki) + vs%tile%tac(k)*frac
            end if
            if (associated(vs%grid%qac) .and. associated(vs%tile%qac)) then
                vs%grid%qac(ki) = vs%grid%qac(ki) + vs%tile%qac(k)*frac
            end if
            if (associated(vs%tile%tcan)) then
                if (vs%tile%tcan(k) > 0.0) then
                    if (associated(vs%grid%tcan) .and. associated(vs%tile%tcan)) then
                        vs%grid%tcan(ki) = vs%grid%tcan(ki) + vs%tile%tcan(k)*frac
                    end if
                    tcanfrac(ki) = tcanfrac(ki) + frac
                end if
            end if
            if (associated(vs%grid%gro) .and. associated(vs%tile%gro)) then
                vs%grid%gro(ki) = vs%grid%gro(ki) + vs%tile%gro(k)*frac
            end if
            if (associated(vs%grid%sno) .and. associated(vs%tile%sno)) then
                vs%grid%sno(ki) = vs%grid%sno(ki) + vs%tile%sno(k)*frac
            end if
            if (associated(vs%grid%rhos) .and. associated(vs%tile%rhos)) then
                vs%grid%rhos(ki) = vs%grid%rhos(ki) + vs%tile%rhos(k)*frac
            end if
            if (associated(vs%grid%zsno) .and. associated(vs%tile%zsno)) then
                vs%grid%zsno(ki) = vs%grid%zsno(ki) + vs%tile%zsno(k)*frac
            end if
            if (associated(vs%grid%fsno) .and. associated(vs%tile%fsno)) then
                vs%grid%fsno(ki) = vs%grid%fsno(ki) + vs%tile%fsno(k)*frac
            end if
            if (associated(vs%grid%albs) .and. associated(vs%tile%albs)) then
                vs%grid%albs(ki) = vs%grid%albs(ki) + vs%tile%albs(k)*frac
            end if
            if (associated(vs%grid%wsno) .and. associated(vs%tile%wsno)) then
                vs%grid%wsno(ki) = vs%grid%wsno(ki) + vs%tile%wsno(k)*frac
            end if
            if (associated(vs%tile%tsno)) then
                if (vs%tile%tsno(k) > 0.0) then
                    if (associated(vs%grid%tsno) .and. associated(vs%tile%tsno)) then
                        vs%grid%tsno(ki) = vs%grid%tsno(ki) + vs%tile%tsno(k)*frac
                    end if
                    tsnofrac(ki) = tsnofrac(ki) + frac
                end if
            end if
            if (associated(vs%grid%rofsno) .and. associated(vs%tile%rofsno)) then
                vs%grid%rofsno(ki) = vs%grid%rofsno(ki) + vs%tile%rofsno(k)*frac
            end if
            if (associated(vs%grid%albt) .and. associated(vs%tile%albt)) then
                vs%grid%albt(ki) = vs%grid%albt(ki) + vs%tile%albt(k)*frac
            end if
            if (associated(vs%grid%alvs) .and. associated(vs%tile%alvs)) then
                vs%grid%alvs(ki) = vs%grid%alvs(ki) + vs%tile%alvs(k)*frac
            end if
            if (associated(vs%grid%alir) .and. associated(vs%tile%alir)) then
                vs%grid%alir(ki) = vs%grid%alir(ki) + vs%tile%alir(k)*frac
            end if
            if (associated(vs%grid%gte) .and. associated(vs%tile%gte)) then
                vs%grid%gte(ki) = vs%grid%gte(ki) + vs%tile%gte(k)*frac
            end if
            if (associated(vs%grid%zpnd) .and. associated(vs%tile%zpnd)) then
                vs%grid%zpnd(ki) = vs%grid%zpnd(ki) + vs%tile%zpnd(k)*frac
            end if
            if (associated(vs%grid%pndw) .and. associated(vs%tile%pndw)) then
                vs%grid%pndw(ki) = vs%grid%pndw(ki) + vs%tile%pndw(k)*frac
            end if
            if (associated(vs%tile%tpnd)) then
                if (vs%tile%tpnd(k) > 0.0) then
                    if (associated(vs%grid%tpnd) .and. associated(vs%tile%tpnd)) then
                        vs%grid%tpnd(ki) = vs%grid%tpnd(ki) + vs%tile%tpnd(k)*frac
                    end if
                    tpndfrac(ki) = tpndfrac(ki) + frac
                end if
            end if
            if (associated(vs%grid%fstr) .and. associated(vs%tile%fstr)) then
                vs%grid%fstr(ki) = vs%grid%fstr(ki) + vs%tile%fstr(k)*frac
            end if
            if (associated(vs%grid%pevp) .and. associated(vs%tile%pevp)) then
                vs%grid%pevp(ki) = vs%grid%pevp(ki) + vs%tile%pevp(k)*frac
            end if
            if (associated(vs%grid%evap) .and. associated(vs%tile%evap)) then
                vs%grid%evap(ki) = vs%grid%evap(ki) + vs%tile%evap(k)*frac
            end if
            if (associated(vs%grid%evpb) .and. associated(vs%tile%evpb)) then
                vs%grid%evpb(ki) = vs%grid%evpb(ki) + vs%tile%evpb(k)*frac
            end if
            if (associated(vs%grid%arrd) .and. associated(vs%tile%arrd)) then
                vs%grid%arrd(ki) = vs%grid%arrd(ki) + vs%tile%arrd(k)*frac
            end if
            if (associated(vs%grid%rofo) .and. associated(vs%tile%rofo)) then
                vs%grid%rofo(ki) = vs%grid%rofo(ki) + vs%tile%rofo(k)*frac
            end if
            if (associated(vs%grid%qevp) .and. associated(vs%tile%qevp)) then
                vs%grid%qevp(ki) = vs%grid%qevp(ki) + vs%tile%qevp(k)*frac
            end if
            if (associated(vs%grid%hfs) .and. associated(vs%tile%hfs)) then
                vs%grid%hfs(ki) = vs%grid%hfs(ki) + vs%tile%hfs(k)*frac
            end if
            if (associated(vs%grid%gzero) .and. associated(vs%tile%gzero)) then
                vs%grid%gzero(ki) = vs%grid%gzero(ki) + vs%tile%gzero(k)*frac
            end if
            if (associated(vs%grid%tsfs) .and. associated(vs%tile%tsfs)) then
                vs%grid%tsfs(ki, :) = vs%grid%tsfs(ki, :) + vs%tile%tsfs(k, :)*frac
            end if
            if (associated(vs%grid%ggeo) .and. associated(vs%tile%ggeo)) then
                vs%grid%ggeo(ki) = vs%grid%ggeo(ki) + vs%tile%ggeo(k)*frac
            end if
            if (associated(vs%grid%tbas) .and. associated(vs%tile%tbas)) then
                vs%grid%tbas(ki) = vs%grid%tbas(ki) + vs%tile%tbas(k)*frac
            end if
            if (associated(vs%grid%thlq) .and. associated(vs%tile%thlq)) then
                vs%grid%thlq(ki, :) = vs%grid%thlq(ki, :) + vs%tile%thlq(k, :)*frac
            end if
            if (associated(vs%grid%thic) .and. associated(vs%tile%thic)) then
                vs%grid%thic(ki, :) = vs%grid%thic(ki, :) + vs%tile%thic(k, :)*frac
            end if
            if (associated(vs%grid%lqws) .and. associated(vs%tile%lqws)) then
                vs%grid%lqws(ki, :) = vs%grid%lqws(ki, :) + vs%tile%lqws(k, :)*frac
            end if
            if (associated(vs%grid%fzws) .and. associated(vs%tile%fzws)) then
                vs%grid%fzws(ki, :) = vs%grid%fzws(ki, :) + vs%tile%fzws(k, :)*frac
            end if
            if (associated(vs%grid%tbar) .and. associated(vs%tile%tbar)) then
                vs%grid%tbar(ki, :) = vs%grid%tbar(ki, :) + vs%tile%tbar(k, :)*frac
            end if
            if (associated(vs%grid%gflx) .and. associated(vs%tile%gflx)) then
                vs%grid%gflx(ki, :) = vs%grid%gflx(ki, :) + vs%tile%gflx(k, :)*frac
            end if
            if (associated(vs%grid%rofs) .and. associated(vs%tile%rofs)) then
                vs%grid%rofs(ki, :) = vs%grid%rofs(ki, :) + vs%tile%rofs(k, :)*frac
            end if
            if (associated(vs%grid%delzw) .and. associated(vs%tile%delzw)) then
                vs%grid%delzw(ki, :) = vs%grid%delzw(ki, :) + vs%tile%delzw(k, :)*frac
            end if
            if (associated(vs%grid%zbotw) .and. associated(vs%tile%zbotw)) then
                vs%grid%zbotw(ki, :) = vs%grid%zbotw(ki, :) + vs%tile%zbotw(k, :)*frac
            end if
            if (associated(vs%grid%rofb) .and. associated(vs%tile%rofb)) then
                vs%grid%rofb(ki) = vs%grid%rofb(ki) + vs%tile%rofb(k)*frac
            end if
            if (associated(vs%grid%rchg) .and. associated(vs%tile%rchg)) then
                vs%grid%rchg(ki) = vs%grid%rchg(ki) + vs%tile%rchg(k)*frac
            end if
            if (associated(vs%grid%lzs) .and. associated(vs%tile%lzs)) then
                vs%grid%lzs(ki) = vs%grid%lzs(ki) + vs%tile%lzs(k)*frac
            end if
            if (associated(vs%grid%dzs) .and. associated(vs%tile%dzs)) then
                vs%grid%dzs(ki) = vs%grid%dzs(ki) + vs%tile%dzs(k)*frac
            end if
        end do

        !> Fractional averages.
        if (associated(vs%grid%cmas)) then
            where (tcanfrac(i1:i2) > 0.0) vs%grid%cmas(i1:i2) = vs%grid%cmas(i1:i2)/tcanfrac(i1:i2)
        end if
        where (tcanfrac(i1:i2) > 0.0) vs%grid%tcan(i1:i2) = vs%grid%tcan(i1:i2)/tcanfrac(i1:i2)
        if (associated(vs%grid%gro)) then
            where (tcanfrac(i1:i2) > 0.0) vs%grid%gro(i1:i2) = vs%grid%gro(i1:i2)/tcanfrac(i1:i2)
        end if
        where (tsnofrac(i1:i2) > 0.0) vs%grid%tsno(i1:i2) = vs%grid%tsno(i1:i2)/tsnofrac(i1:i2)
        if (associated(vs%grid%rhos)) then
            where (tsnofrac(i1:i2) > 0.0) vs%grid%rhos(i1:i2) = vs%grid%rhos(i1:i2)/tsnofrac(i1:i2)
        end if
        where (tpndfrac(i1:i2) > 0.0) vs%grid%tpnd(i1:i2) = vs%grid%tpnd(i1:i2)/tpndfrac(i1:i2)

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
