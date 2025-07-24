module sa_mesh_run_within_grid

    !> 'model_files_variables' required for 'fls' object and file keys.
    !> 'sa_mesh_common' required for common SA_MESH variables and routines.
    !> 'model_dates' required for 'ic' counter.
    !> 'mpi_module' required for MPI variables, tile/grid parsing utility, barrier flag.
    use model_files_variables
    use sa_mesh_common
    use model_dates
    use mpi_module

    implicit none

    contains

    subroutine run_within_grid_init(fls, shd)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Update variables.
        call run_within_grid_stas_update(fls, shd)

    end subroutine

    subroutine run_within_grid(fls, shd)

        !> 'mesh_io_constants' for 'NODATA' values.
        use mesh_io_constants, only: NO_DATA_REAL

        !> Process modules.
        use baseflow_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
        integer j

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Pre-process update of variables.
        call run_within_grid_stas_update(fls, shd)

        !> Call processes.
        call bflm_within_grid(fls, shd)

        !> Post-process update of variables.
!        if (allocated(vs%grid%rchg) .and. allocated(vs%grid%drainsol)) then
!            if (all(vs%grid%rchg(i1:i2) == NO_DATA_REAL)) then
!                where (vs%grid%drainsol(i1:i2) /= NO_DATA_REAL) vs%grid%rchg(i1:i2) = vs%grid%drainsol(i1:i2)*ic%dts
!            end if
!        end if
        if (allocated(vs%grid%rff)) then
            if (all(vs%grid%rff(i1:i2) == NO_DATA_REAL)) then
                vs%grid%rff = 0.0
                if (allocated(vs%grid%ovrflw)) then
                    where (vs%grid%ovrflw(i1:i2) /= NO_DATA_REAL)
                        vs%grid%rff(i1:i2) = vs%grid%rff(i1:i2) + vs%grid%ovrflw(i1:i2)*ic%dts
                    end where
                end if
                if (allocated(vs%grid%latflw)) then
                    do j = 1, size(vs%grid%latflw, 2)
                        where (vs%grid%latflw(i1:i2, j) /= NO_DATA_REAL)
                            vs%grid%rff(i1:i2) = vs%grid%rff(i1:i2) + vs%grid%latflw(i1:i2, j)*ic%dts
                        end where
                    end do
                end if
                if (allocated(vs%grid%lkg)) then
                    where (vs%grid%lkg(i1:i2) /= NO_DATA_REAL)
                        vs%grid%rff(i1:i2) = vs%grid%rff(i1:i2) + vs%grid%lkg(i1:i2)
                    end where
                end if
            end if
        end if

    end subroutine

    subroutine run_within_grid_mpi_isend(fls, shd)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

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

    subroutine run_within_grid_mpi_irecv(fls, shd)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
        integer nvars, t, c, i, j, u, ii1, ii2, iin, z
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

        !> Check active kind of 'real'.
        if (kind(2.0) == 8) then
            c = MPI_REAL8
        else
            c = MPI_REAL
        end if

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
!                chnl((1 + iin*2):(iin*3)) = vs%grid%abstr(ii1:ii2)
!                call MPI_Isend(chnl, size(chnl), c, u, t + i, MPI_COMM_WORLD, irqst(i), z)
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
!            call MPI_Irecv(chnl, size(chnl), c, 0, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
                call MPI_Testall(nvars, irqst, lstat, imstat, z)
            end do

            !> Assign variables.

            !> Channel routing.
!            vs%grid%stgch(ii1:ii2) = chnl((1 + iin*0):(iin*1))
!            vs%grid%div(ii1:ii2) = chnl((1 + iin*1):(iin*2))
!            vs%grid%abstr(ii1:ii2) = chnl((1 + iin*2):(iin*3))

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

    subroutine run_within_grid_stas_update(fls, shd)

        !> 'mesh_io_constants' for 'NODATA' values.
        use mesh_io_constants, only: NO_DATA_REAL

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
        integer outlets, k, ki, kj
        real tpndfrac(i1:i2), tsnofrac(i1:i2), tcanfrac(i1:i2), ticefrac(i1:i2), frac

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

        !> Indices for outlets.
        outlets = min(shd%NA + 1, shd%NAA)

        !> Meteorology/climatology variables.
!        if (allocated(vs%grid%fsin)) vs%grid%fsin(i1:i2) = 0.0
!        if (allocated(vs%grid%fsvs)) vs%grid%fsvs(i1:i2) = 0.0
!        if (allocated(vs%grid%fsir)) vs%grid%fsir(i1:i2) = 0.0
!        if (allocated(vs%grid%fsdr)) vs%grid%fsdr(i1:i2) = 0.0
!        if (allocated(vs%grid%fsdff)) vs%grid%fsdff(i1:i2) = 0.0
!        if (allocated(vs%grid%flin)) vs%grid%flin(i1:i2) = 0.0
!        if (allocated(vs%grid%ta)) vs%grid%ta(i1:i2) = 0.0
!        if (allocated(vs%grid%qa)) vs%grid%qa(i1:i2) = 0.0
!        if (allocated(vs%grid%pres)) vs%grid%pres(i1:i2) = 0.0
!        if (allocated(vs%grid%uu)) vs%grid%uu(i1:i2) = 0.0
!        if (allocated(vs%grid%vv)) vs%grid%vv(i1:i2) = 0.0
!        if (allocated(vs%grid%uv)) vs%grid%uv(i1:i2) = 0.0
!        if (allocated(vs%grid%wdir)) vs%grid%wdir(i1:i2) = 0.0
        if (allocated(vs%grid%prern)) vs%grid%prern(i1:i2) = 0.0
        if (allocated(vs%grid%presno)) vs%grid%presno(i1:i2) = 0.0
!        if (allocated(vs%grid%pre)) vs%grid%pre(i1:i2) = 0.0

        !> Canopy variables.
        if (allocated(vs%grid%lqwscan)) vs%grid%lqwscan(i1:i2) = 0.0
        if (allocated(vs%grid%fzwscan)) vs%grid%fzwscan(i1:i2) = 0.0
        if (allocated(vs%grid%cmas)) vs%grid%cmas(i1:i2) = 0.0
        if (allocated(vs%grid%tacan)) vs%grid%tacan(i1:i2) = 0.0
        if (allocated(vs%grid%qacan)) vs%grid%qacan(i1:i2) = 0.0
        if (allocated(vs%grid%uvcan)) vs%grid%uvcan(i1:i2) = 0.0
        if (allocated(vs%grid%tcan)) vs%grid%tcan(i1:i2) = 0.0
        if (allocated(vs%grid%gro)) vs%grid%gro(i1:i2) = 0.0

        !> Snow variables.
        if (allocated(vs%grid%fsno)) vs%grid%fsno(i1:i2) = 0.0
        if (allocated(vs%grid%sno)) vs%grid%sno(i1:i2) = 0.0
        if (allocated(vs%grid%rhosno)) vs%grid%rhosno(i1:i2) = 0.0
!-        if (allocated(vs%grid%zsno)) vs%grid%zsno(i1:i2) = 0.0
        if (allocated(vs%grid%lqwssno)) vs%grid%lqwssno(i1:i2) = 0.0
        if (allocated(vs%grid%tsno)) vs%grid%tsno(i1:i2) = 0.0
        if (allocated(vs%grid%albsno)) vs%grid%albsno(i1:i2) = 0.0
        if (allocated(vs%grid%drainsno)) vs%grid%drainsno(i1:i2) = 0.0

        !> Surface variables.
        if (allocated(vs%grid%albt)) vs%grid%albt(i1:i2) = 0.0
        if (allocated(vs%grid%alvs)) vs%grid%alvs(i1:i2) = 0.0
        if (allocated(vs%grid%alir)) vs%grid%alir(i1:i2) = 0.0
        if (allocated(vs%grid%gte)) vs%grid%gte(i1:i2) = 0.0
        if (allocated(vs%grid%zpnd)) vs%grid%zpnd(i1:i2) = 0.0
!-        if (allocated(vs%grid%lqwspnd)) vs%grid%lqwspnd(i1:i2) = 0.0
        if (allocated(vs%grid%tpnd)) vs%grid%tpnd(i1:i2) = 0.0
        if (allocated(vs%grid%pndcaf)) vs%grid%pndcaf(i1:i2) = 0.0
        if (allocated(vs%grid%potevp)) vs%grid%potevp(i1:i2) = 0.0
        if (allocated(vs%grid%et)) vs%grid%et(i1:i2) = 0.0
!-        if (allocated(vs%grid%evpb)) vs%grid%evpb(i1:i2) = 0.0
!-        if (allocated(vs%grid%arrd)) vs%grid%arrd(i1:i2) = 0.0
        if (allocated(vs%grid%ovrflw)) vs%grid%ovrflw(i1:i2) = 0.0
        if (allocated(vs%grid%qevp)) vs%grid%qevp(i1:i2) = 0.0
        if (allocated(vs%grid%qsens)) vs%grid%qsens(i1:i2) = 0.0
        if (allocated(vs%grid%gzero)) vs%grid%gzero(i1:i2) = 0.0
        if (allocated(vs%grid%tsfs)) vs%grid%tsfs(i1:i2, :) = 0.0
        if (allocated(vs%grid%tsurf)) vs%grid%tsurf(i1:i2) = 0.0

        !> Ice/glacier variables.
        if (allocated(vs%grid%lqwsice)) vs%grid%lqwsice(i1:i2) = 0.0
        if (allocated(vs%grid%tice)) vs%grid%tice(i1:i2) = 0.0

        !> Subsurface/soil variables.
        if (allocated(vs%grid%dzsol)) vs%grid%dzsol(i1:i2, :) = 0.0
        if (allocated(vs%grid%dzsolhyd)) vs%grid%dzsolhyd(i1:i2, :) = 0.0
        if (allocated(vs%grid%thlqsol)) vs%grid%thlqsol(i1:i2, :) = 0.0
        if (allocated(vs%grid%thicsol)) vs%grid%thicsol(i1:i2, :) = 0.0
        if (allocated(vs%grid%lqwssol)) vs%grid%lqwssol(i1:i2, :) = 0.0
        if (allocated(vs%grid%fzwssol)) vs%grid%fzwssol(i1:i2, :) = 0.0
        if (allocated(vs%grid%tsol)) vs%grid%tsol(i1:i2, :) = 0.0
        if (allocated(vs%grid%gflx)) vs%grid%gflx(i1:i2, :) = 0.0
        if (allocated(vs%grid%hcps)) vs%grid%hcps(i1:i2, :) = 0.0
        if (allocated(vs%grid%hcpc)) vs%grid%hcpc(i1:i2, :) = 0.0
        if (allocated(vs%grid%hcpg)) vs%grid%hcpg(i1:i2, :) = 0.0
        if (allocated(vs%grid%tctopc)) vs%grid%tctopc(i1:i2, :) = 0.0
        if (allocated(vs%grid%tctopg)) vs%grid%tctopg(i1:i2, :) = 0.0
        if (allocated(vs%grid%tcbotc)) vs%grid%tcbotc(i1:i2, :) = 0.0
        if (allocated(vs%grid%tcbotg)) vs%grid%tcbotg(i1:i2, :) = 0.0
        if (allocated(vs%grid%latflw)) vs%grid%latflw(i1:i2, :) = 0.0
        if (allocated(vs%grid%zsol)) vs%grid%zsol(i1:i2) = 0.0
        if (allocated(vs%grid%zsolhyd)) vs%grid%zsolhyd(i1:i2) = 0.0
        if (allocated(vs%grid%zsolsat)) vs%grid%zsolsat(i1:i2) = 0.0
        if (allocated(vs%grid%ggeo)) vs%grid%ggeo(i1:i2) = 0.0
        if (allocated(vs%grid%tbas)) vs%grid%tbas(i1:i2) = 0.0
        if (allocated(vs%grid%drainsol)) vs%grid%drainsol(i1:i2) = 0.0

        !> Groundwater/lower zone storage variables.
        if (allocated(vs%grid%rchg)) then
            vs%grid%rchg = NO_DATA_REAL
            if (allocated(vs%tile%rchg)) then
                if (any(vs%tile%rchg(il1:il2) /= NO_DATA_REAL)) then
                    vs%grid%rchg(i1:i2) = 0.0
                    if (ipid == 0) vs%grid%rchg(outlets:) = 0.0
                end if
            end if
        end if
!        if (allocated(vs%grid%stggw)) vs%grid%stggw = 0.0
        if (allocated(vs%grid%lkg)) then
            vs%grid%lkg = NO_DATA_REAL
            if (allocated(vs%tile%lkg)) then
                if (any(vs%tile%lkg(il1:il2) /= NO_DATA_REAL)) then
                    vs%grid%lkg(i1:i2) = 0.0
                    if (ipid == 0) vs%grid%lkg(outlets:) = 0.0
                end if
            end if
        end if
!-        if (allocated(vs%grid%dzs)) vs%grid%dzs(i1:i2) = 0.0

        !> Diagnostic variables.
!-        if (allocated(vs%grid%stge)) vs%grid%stge(i1:i2) = 0.0
!-        if (allocated(vs%grid%stgw)) vs%grid%stgw(i1:i2) = 0.0

        !> Routing variables.
        if (allocated(vs%grid%rff)) vs%grid%rff = NO_DATA_REAL
!        if (allocated(vs%grid%rff)) then
!            vs%grid%rff = NO_DATA_REAL
!            if (allocated(vs%tile%rff)) then
!                if (any(vs%tile%rff(il1:il2) /= NO_DATA_REAL)) then
!                    vs%grid%rff(i1:i2) = 0.0
!                    if (ipid == 0) vs%grid%rff(outlets:) = 0.0
!                end if
!            end if
!        end if
!        if (allocated(vs%grid%qi)) vs%grid%qi(i1:i2) = 0.0
!        if (allocated(vs%grid%qo)) vs%grid%qo(i1:i2) = 0.0
!        if (allocated(vs%grid%stgch)) vs%grid%stgch(i1:i2) = 0.0
!        if (allocated(vs%grid%zlvl)) vs%grid%zlvl(i1:i2) = 0.0
!        if (allocated(vs%grid%div)) vs%grid%div(i1:i2) = 0.0
!        if (allocated(vs%grid%abstr)) vs%grid%abstr(i1:i2) = 0.0

        !> Update variables.
        tcanfrac(i1:i2) = 0.0
        tsnofrac(i1:i2) = 0.0
        tpndfrac(i1:i2) = 0.0
        ticefrac(i1:i2) = 0.0
        do k = il1, il2

            !> Indices and contributing fraction.
            ki = shd%lc%ILMOS(k)
            kj = shd%lc%JLMOS(k)
            frac = shd%lc%ACLASS(ki, kj)

            !> Meteorology/climatology variables.
!            if (allocated(vs%grid%fsin) .and. allocated(vs%tile%fsin)) then
!                if (vs%tile%fsin(k) /= huge(vs%tile%fsin)) vs%grid%fsin(ki) = vs%grid%fsin(ki) + vs%tile%fsin(k)*frac
!            end if
!            if (allocated(vs%grid%fsvs) .and. allocated(vs%tile%fsvs)) then
!                if (vs%tile%fsvs(k) /= huge(vs%tile%fsvs)) vs%grid%fsvs(ki) = vs%grid%fsvs(ki) + vs%tile%fsvs(k)*frac
!            end if
!            if (allocated(vs%grid%fsir) .and. allocated(vs%tile%fsir)) then
!                if (vs%tile%fsir(k) /= huge(vs%tile%fsir)) vs%grid%fsir(ki) = vs%grid%fsir(ki) + vs%tile%fsir(k)*frac
!            end if
!            if (allocated(vs%grid%fsdr) .and. allocated(vs%tile%fsdr)) then
!                if (vs%tile%fsdr(k) /= huge(vs%tile%fsdr)) vs%grid%fsdr(ki) = vs%grid%fsdr(ki) + vs%tile%fsdr(k)*frac
!            end if
!            if (allocated(vs%grid%fsdff) .and. allocated(vs%tile%fsdff)) then
!                if (vs%tile%fsdff(k) /= huge(vs%tile%fsdff)) vs%grid%fsdff(ki) = vs%grid%fsdff(ki) + vs%tile%fsdff(k)*frac
!            end if
!            if (allocated(vs%grid%flin) .and. allocated(vs%tile%flin)) then
!                if (vs%tile%flin(k) /= huge(vs%tile%flin)) vs%grid%flin(ki) = vs%grid%flin(ki) + vs%tile%flin(k)*frac
!            end if
!            if (allocated(vs%grid%ta) .and. allocated(vs%tile%ta)) then
!                if (vs%tile%ta(k) /= huge(vs%tile%ta)) vs%grid%ta(ki) = vs%grid%ta(ki) + vs%tile%ta(k)*frac
!            end if
!            if (allocated(vs%grid%qa) .and. allocated(vs%tile%qa)) then
!                if (vs%tile%qa(k) /= huge(vs%tile%qa)) vs%grid%qa(ki) = vs%grid%qa(ki) + vs%tile%qa(k)*frac
!            end if
!            if (allocated(vs%grid%pres) .and. allocated(vs%tile%pres)) then
!                if (vs%tile%pres(k) /= huge(vs%tile%pres)) vs%grid%pres(ki) = vs%grid%pres(ki) + vs%tile%pres(k)*frac
!            end if
!            if (allocated(vs%grid%uu) .and. allocated(vs%tile%uu)) then
!                if (vs%tile%uu(k) /= huge(vs%tile%uu)) vs%grid%uu(ki) = vs%grid%uu(ki) + vs%tile%uu(k)*frac
!            end if
!            if (allocated(vs%grid%vv) .and. allocated(vs%tile%vv)) then
!                if (vs%tile%vv(k) /= huge(vs%tile%vv)) vs%grid%vv(ki) = vs%grid%vv(ki) + vs%tile%vv(k)*frac
!            end if
!            if (allocated(vs%grid%uv) .and. allocated(vs%tile%uv)) then
!                if (vs%tile%uv(k) /= huge(vs%tile%uv)) vs%grid%uv(ki) = vs%grid%uv(ki) + vs%tile%uv(k)*frac
!            end if
!            if (allocated(vs%grid%wdir) .and. allocated(vs%tile%wdir)) then
!                if (vs%tile%wdir(k) /= huge(vs%tile%wdir)) vs%grid%wdir(ki) = vs%grid%wdir(ki) + vs%tile%wdir(k)*frac
!            end if
            if (allocated(vs%grid%prern) .and. allocated(vs%tile%prern)) then
                if (vs%tile%prern(k) /= huge(vs%tile%prern)) vs%grid%prern(ki) = vs%grid%prern(ki) + vs%tile%prern(k)*frac
            end if
            if (allocated(vs%grid%presno) .and. allocated(vs%tile%presno)) then
                if (vs%tile%presno(k) /= huge(vs%tile%presno)) vs%grid%presno(ki) = vs%grid%presno(ki) + vs%tile%presno(k)*frac
            end if
!            if (allocated(vs%grid%pre) .and. allocated(vs%tile%pre)) then
!                if (vs%tile%pre(k) /= huge(vs%tile%pre)) vs%grid%pre(ki) = vs%grid%pre(ki) + vs%tile%pre(k)*frac
!            end if

            !> Canopy variables.
            if (allocated(vs%grid%lqwscan) .and. allocated(vs%tile%lqwscan)) then
                if (vs%tile%lqwscan(k) /= huge(vs%tile%lqwscan)) then
                    vs%grid%lqwscan(ki) = vs%grid%lqwscan(ki) + vs%tile%lqwscan(k)*frac
                end if
            end if
            if (allocated(vs%grid%fzwscan) .and. allocated(vs%tile%fzwscan)) then
                if (vs%tile%fzwscan(k) /= huge(vs%tile%fzwscan)) then
                    vs%grid%fzwscan(ki) = vs%grid%fzwscan(ki) + vs%tile%fzwscan(k)*frac
                end if
            end if
            if (allocated(vs%grid%cmas) .and. allocated(vs%tile%cmas)) then
                if (vs%tile%cmas(k) /= huge(vs%tile%cmas)) vs%grid%cmas(ki) = vs%grid%cmas(ki) + vs%tile%cmas(k)*frac
            end if
            if (allocated(vs%grid%tacan) .and. allocated(vs%tile%tacan)) then
                if (vs%tile%tacan(k) /= huge(vs%tile%tacan)) vs%grid%tacan(ki) = vs%grid%tacan(ki) + vs%tile%tacan(k)*frac
            end if
            if (allocated(vs%grid%qacan) .and. allocated(vs%tile%qacan)) then
                if (vs%tile%qacan(k) /= huge(vs%tile%qacan)) vs%grid%qacan(ki) = vs%grid%qacan(ki) + vs%tile%qacan(k)*frac
            end if
            if (allocated(vs%grid%uvcan) .and. allocated(vs%tile%uvcan)) then
                if (vs%tile%uvcan(k) /= huge(vs%tile%uvcan)) vs%grid%uvcan(ki) = vs%grid%uvcan(ki) + vs%tile%uvcan(k)*frac
            end if
            if (allocated(vs%tile%tcan)) then
                if (vs%tile%tcan(k) > 0.0) then
                    if (allocated(vs%grid%tcan) .and. allocated(vs%tile%tcan)) then
                        if (vs%tile%tcan(k) /= huge(vs%tile%tcan)) vs%grid%tcan(ki) = vs%grid%tcan(ki) + vs%tile%tcan(k)*frac
                    end if
                    tcanfrac(ki) = tcanfrac(ki) + frac
                end if
            end if
            if (allocated(vs%grid%gro) .and. allocated(vs%tile%gro)) then
                if (vs%tile%gro(k) /= huge(vs%tile%gro)) vs%grid%gro(ki) = vs%grid%gro(ki) + vs%tile%gro(k)*frac
            end if

            !> Snow variables.
            if (allocated(vs%grid%fsno) .and. allocated(vs%tile%fsno)) then
                if (vs%tile%fsno(k) /= huge(vs%tile%fsno)) vs%grid%fsno(ki) = vs%grid%fsno(ki) + vs%tile%fsno(k)*frac
            end if
            if (allocated(vs%grid%sno) .and. allocated(vs%tile%sno)) then
                if (vs%tile%sno(k) /= huge(vs%tile%sno)) vs%grid%sno(ki) = vs%grid%sno(ki) + vs%tile%sno(k)*frac
            end if
            if (allocated(vs%grid%rhosno) .and. allocated(vs%tile%rhosno)) then
                if (vs%tile%rhosno(k) /= huge(vs%tile%rhosno)) vs%grid%rhosno(ki) = vs%grid%rhosno(ki) + vs%tile%rhosno(k)*frac
            end if
!-            if (allocated(vs%grid%zsno) .and. allocated(vs%tile%zsno)) then
!-                if (vs%tile%zsno(k) /= huge(vs%tile%zsno)) vs%grid%zsno(ki) = vs%grid%zsno(ki) + vs%tile%zsno(k)*frac
!-            end if
            if (allocated(vs%grid%lqwssno) .and. allocated(vs%tile%lqwssno)) then
                if (vs%tile%lqwssno(k) /= huge(vs%tile%lqwssno)) vs%grid%lqwssno(ki) = vs%grid%lqwssno(ki) + vs%tile%lqwssno(k)*frac
            end if
            if (allocated(vs%tile%tsno)) then
                if (vs%tile%tsno(k) > 0.0) then
                    if (allocated(vs%grid%tsno) .and. allocated(vs%tile%tsno)) then
                        if (vs%tile%tsno(k) /= huge(vs%tile%tsno)) vs%grid%tsno(ki) = vs%grid%tsno(ki) + vs%tile%tsno(k)*frac
                    end if
                    tsnofrac(ki) = tsnofrac(ki) + frac
                end if
            end if
            if (allocated(vs%grid%albsno) .and. allocated(vs%tile%albsno)) then
                if (vs%tile%albsno(k) /= huge(vs%tile%albsno)) vs%grid%albsno(ki) = vs%grid%albsno(ki) + vs%tile%albsno(k)*frac
            end if
            if (allocated(vs%grid%drainsno) .and. allocated(vs%tile%drainsno)) then
                if (vs%tile%drainsno(k) /= huge(vs%tile%drainsno)) then
                    vs%grid%drainsno(ki) = vs%grid%drainsno(ki) + vs%tile%drainsno(k)*frac
                end if
            end if

            !> Surface variables.
            if (allocated(vs%grid%albt) .and. allocated(vs%tile%albt)) then
                if (vs%tile%albt(k) /= huge(vs%tile%albt)) vs%grid%albt(ki) = vs%grid%albt(ki) + vs%tile%albt(k)*frac
            end if
            if (allocated(vs%grid%alvs) .and. allocated(vs%tile%alvs)) then
                if (vs%tile%alvs(k) /= huge(vs%tile%alvs)) vs%grid%alvs(ki) = vs%grid%alvs(ki) + vs%tile%alvs(k)*frac
            end if
            if (allocated(vs%grid%alir) .and. allocated(vs%tile%alir)) then
                if (vs%tile%alir(k) /= huge(vs%tile%alir)) vs%grid%alir(ki) = vs%grid%alir(ki) + vs%tile%alir(k)*frac
            end if
            if (allocated(vs%grid%gte) .and. allocated(vs%tile%gte)) then
                if (vs%tile%gte(k) /= huge(vs%tile%gte)) vs%grid%gte(ki) = vs%grid%gte(ki) + vs%tile%gte(k)*frac
            end if
            if (allocated(vs%grid%zpnd) .and. allocated(vs%tile%zpnd)) then
                if (vs%tile%zpnd(k) /= huge(vs%tile%zpnd)) vs%grid%zpnd(ki) = vs%grid%zpnd(ki) + vs%tile%zpnd(k)*frac
            end if
!-            if (allocated(vs%grid%lqwspnd) .and. allocated(vs%tile%lqwspnd)) then
!-                if (vs%tile%lqwspnd(k) /= huge(vs%tile%lqwspnd)) then
!-                    vs%grid%lqwspnd(ki) = vs%grid%lqwspnd(ki) + vs%tile%lqwspnd(k)*frac
!-                end if
!-            end if
            if (allocated(vs%tile%tpnd)) then
                if (vs%tile%tpnd(k) > 0.0) then
                    if (allocated(vs%grid%tpnd) .and. allocated(vs%tile%tpnd)) then
                        if (vs%tile%tpnd(k) /= huge(vs%tile%tpnd)) vs%grid%tpnd(ki) = vs%grid%tpnd(ki) + vs%tile%tpnd(k)*frac
                    end if
                    tpndfrac(ki) = tpndfrac(ki) + frac
                end if
            end if
            if (allocated(vs%grid%pndcaf) .and. allocated(vs%tile%pndcaf)) then
                if (vs%tile%pndcaf(k) /= huge(vs%tile%pndcaf)) vs%grid%pndcaf(ki) = vs%grid%pndcaf(ki) + vs%tile%pndcaf(k)*frac
            end if
            if (allocated(vs%grid%potevp) .and. allocated(vs%tile%potevp)) then
                if (vs%tile%potevp(k) /= huge(vs%tile%potevp)) vs%grid%potevp(ki) = vs%grid%potevp(ki) + vs%tile%potevp(k)*frac
            end if
            if (allocated(vs%grid%et) .and. allocated(vs%tile%et)) then
                if (vs%tile%et(k) /= huge(vs%tile%et)) vs%grid%et(ki) = vs%grid%et(ki) + vs%tile%et(k)*frac
            end if
!-            if (allocated(vs%grid%evpb) .and. allocated(vs%tile%evpb)) then
!-                if (vs%tile%evpb(k) /= huge(vs%tile%evpb)) vs%grid%evpb(ki) = vs%grid%evpb(ki) + vs%tile%evpb(k)*frac
!-            end if
!-            if (allocated(vs%grid%arrd) .and. allocated(vs%tile%arrd)) then
!-                if (vs%tile%arrd(k) /= huge(vs%tile%arrd)) vs%grid%arrd(ki) = vs%grid%arrd(ki) + vs%tile%arrd(k)*frac
!-            end if
            if (allocated(vs%grid%ovrflw) .and. allocated(vs%tile%ovrflw)) then
                if (vs%tile%ovrflw(k) /= huge(vs%tile%ovrflw)) vs%grid%ovrflw(ki) = vs%grid%ovrflw(ki) + vs%tile%ovrflw(k)*frac
            end if
            if (allocated(vs%grid%qevp) .and. allocated(vs%tile%qevp)) then
                if (vs%tile%qevp(k) /= huge(vs%tile%qevp)) vs%grid%qevp(ki) = vs%grid%qevp(ki) + vs%tile%qevp(k)*frac
            end if
            if (allocated(vs%grid%qsens) .and. allocated(vs%tile%qsens)) then
                if (vs%tile%qsens(k) /= huge(vs%tile%qsens)) vs%grid%qsens(ki) = vs%grid%qsens(ki) + vs%tile%qsens(k)*frac
            end if
            if (allocated(vs%grid%gzero) .and. allocated(vs%tile%gzero)) then
                if (vs%tile%gzero(k) /= huge(vs%tile%gzero)) vs%grid%gzero(ki) = vs%grid%gzero(ki) + vs%tile%gzero(k)*frac
            end if
            if (allocated(vs%grid%tsfs) .and. allocated(vs%tile%tsfs)) then
                if (all(vs%tile%tsfs(k, :) /= huge(vs%tile%tsfs))) then
                    vs%grid%tsfs(ki, :) = vs%grid%tsfs(ki, :) + vs%tile%tsfs(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%tsurf) .and. allocated(vs%tile%tsurf)) then
                if (vs%tile%tsurf(k) /= huge(vs%tile%tsurf)) vs%grid%tsurf(ki) = vs%grid%tsurf(ki) + vs%tile%tsurf(k)*frac
            end if

            !> Ice/glacier variables.
            if (allocated(vs%grid%lqwsice) .and. allocated(vs%tile%lqwsice)) then
                if (vs%tile%lqwsice(k) /= huge(vs%tile%lqwsice)) vs%grid%lqwsice(ki) = vs%grid%lqwsice(ki) + vs%tile%lqwsice(k)*frac
            end if
            if (allocated(vs%tile%tice)) then
                if (vs%tile%tice(k) > 0.0) then
                    if (allocated(vs%grid%tice) .and. allocated(vs%tile%tice)) then
                        if (vs%tile%tice(k) /= huge(vs%tile%tice)) vs%grid%tice(ki) = vs%grid%tice(ki) + vs%tile%tice(k)*frac
                    end if
                    ticefrac(ki) = ticefrac(ki) + frac
                end if
            end if

            !> Subsurface/soil variables.
            if (allocated(vs%grid%dzsol) .and. allocated(vs%tile%dzsol)) then
                if (all(vs%tile%dzsol(k, :) /= huge(vs%tile%dzsol))) then
                    vs%grid%dzsol(ki, :) = vs%grid%dzsol(ki, :) + vs%tile%dzsol(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%dzsolhyd) .and. allocated(vs%tile%dzsolhyd)) then
                if (all(vs%tile%dzsolhyd(k, :) /= huge(vs%tile%dzsolhyd))) then
                    vs%grid%dzsolhyd(ki, :) = vs%grid%dzsolhyd(ki, :) + vs%tile%dzsolhyd(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%thlqsol) .and. allocated(vs%tile%thlqsol)) then
                if (all(vs%tile%thlqsol(k, :) /= huge(vs%tile%thlqsol))) then
                    vs%grid%thlqsol(ki, :) = vs%grid%thlqsol(ki, :) + vs%tile%thlqsol(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%thicsol) .and. allocated(vs%tile%thicsol)) then
                if (all(vs%tile%thicsol(k, :) /= huge(vs%tile%thicsol))) then
                    vs%grid%thicsol(ki, :) = vs%grid%thicsol(ki, :) + vs%tile%thicsol(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%lqwssol) .and. allocated(vs%tile%lqwssol)) then
                if (all(vs%tile%lqwssol(k, :) /= huge(vs%tile%lqwssol))) then
                    vs%grid%lqwssol(ki, :) = vs%grid%lqwssol(ki, :) + vs%tile%lqwssol(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%fzwssol) .and. allocated(vs%tile%fzwssol)) then
                if (all(vs%tile%fzwssol(k, :) /= huge(vs%tile%fzwssol))) then
                    vs%grid%fzwssol(ki, :) = vs%grid%fzwssol(ki, :) + vs%tile%fzwssol(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%tsol) .and. allocated(vs%tile%tsol)) then
                if (all(vs%tile%tsol(k, :) /= huge(vs%tile%tsol))) then
                    vs%grid%tsol(ki, :) = vs%grid%tsol(ki, :) + vs%tile%tsol(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%gflx) .and. allocated(vs%tile%gflx)) then
                if (all(vs%tile%gflx(k, :) /= huge(vs%tile%gflx))) then
                    vs%grid%gflx(ki, :) = vs%grid%gflx(ki, :) + vs%tile%gflx(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%hcps) .and. allocated(vs%tile%hcps)) then
                if (all(vs%tile%hcps(k, :) /= huge(vs%tile%hcps))) then
                    vs%grid%hcps(ki, :) = vs%grid%hcps(ki, :) + vs%tile%hcps(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%hcpc) .and. allocated(vs%tile%hcpc)) then
                if (all(vs%tile%hcpc(k, :) /= huge(vs%tile%hcpc))) then
                    vs%grid%hcpc(ki, :) = vs%grid%hcpc(ki, :) + vs%tile%hcpc(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%hcpg) .and. allocated(vs%tile%hcpg)) then
                if (all(vs%tile%hcpg(k, :) /= huge(vs%tile%hcpg))) then
                    vs%grid%hcpg(ki, :) = vs%grid%hcpg(ki, :) + vs%tile%hcpg(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%tctopc) .and. allocated(vs%tile%tctopc)) then
                if (all(vs%tile%tctopc(k, :) /= huge(vs%tile%tctopc))) then
                    vs%grid%tctopc(ki, :) = vs%grid%tctopc(ki, :) + vs%tile%tctopc(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%tctopg) .and. allocated(vs%tile%tctopg)) then
                if (all(vs%tile%tctopg(k, :) /= huge(vs%tile%tctopg))) then
                    vs%grid%tctopg(ki, :) = vs%grid%tctopg(ki, :) + vs%tile%tctopg(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%tcbotc) .and. allocated(vs%tile%tcbotc)) then
                if (all(vs%tile%tcbotc(k, :) /= huge(vs%tile%tcbotc))) then
                    vs%grid%tcbotc(ki, :) = vs%grid%tcbotc(ki, :) + vs%tile%tcbotc(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%tcbotg) .and. allocated(vs%tile%tcbotg)) then
                if (all(vs%tile%tcbotg(k, :) /= huge(vs%tile%tcbotg))) then
                    vs%grid%tcbotg(ki, :) = vs%grid%tcbotg(ki, :) + vs%tile%tcbotg(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%latflw) .and. allocated(vs%tile%latflw)) then
                if (all(vs%tile%latflw(k, :) /= huge(vs%tile%latflw))) then
                    vs%grid%latflw(ki, :) = vs%grid%latflw(ki, :) + vs%tile%latflw(k, :)*frac
                end if
            end if
            if (allocated(vs%grid%zsol) .and. allocated(vs%tile%zsol)) then
                if (vs%tile%zsol(k) /= huge(vs%tile%zsol)) vs%grid%zsol(ki) = vs%grid%zsol(ki) + vs%tile%zsol(k)*frac
            end if
            if (allocated(vs%grid%zsolhyd) .and. allocated(vs%tile%zsolhyd)) then
                if (vs%tile%zsolhyd(k) /= huge(vs%tile%zsolhyd)) then
                    vs%grid%zsolhyd(ki) = vs%grid%zsolhyd(ki) + vs%tile%zsolhyd(k)*frac
                end if
            end if
            if (allocated(vs%grid%zsolsat) .and. allocated(vs%tile%zsolsat)) then
                if (vs%tile%zsolsat(k) /= huge(vs%tile%zsolsat)) then
                    vs%grid%zsolsat(ki) = vs%grid%zsolsat(ki) + vs%tile%zsolsat(k)*frac
                end if
            end if
            if (allocated(vs%grid%ggeo) .and. allocated(vs%tile%ggeo)) then
                if (vs%tile%ggeo(k) /= huge(vs%tile%ggeo)) vs%grid%ggeo(ki) = vs%grid%ggeo(ki) + vs%tile%ggeo(k)*frac
            end if
            if (allocated(vs%grid%tbas) .and. allocated(vs%tile%tbas)) then
                if (vs%tile%tbas(k) /= huge(vs%tile%tbas)) vs%grid%tbas(ki) = vs%grid%tbas(ki) + vs%tile%tbas(k)*frac
            end if
            if (allocated(vs%grid%drainsol) .and. allocated(vs%tile%drainsol)) then
                if (vs%tile%drainsol(k) /= huge(vs%tile%drainsol)) then
                    vs%grid%drainsol(ki) = vs%grid%drainsol(ki) + vs%tile%drainsol(k)*frac
                end if
            end if

            !> Groundwater/lower zone storage variables.
            if (allocated(vs%grid%rchg) .and. allocated(vs%tile%rchg)) then
                if (vs%tile%rchg(k) /= NO_DATA_REAL) vs%grid%rchg(ki) = vs%grid%rchg(ki) + vs%tile%rchg(k)*frac
            end if
            if (allocated(vs%grid%stggw) .and. allocated(vs%tile%stggw)) then
                if (vs%tile%stggw(k) /= NO_DATA_REAL) vs%grid%stggw(ki) = vs%grid%stggw(ki) + vs%tile%stggw(k)*frac
            end if
            if (allocated(vs%grid%lkg) .and. allocated(vs%tile%lkg)) then
                if (vs%tile%lkg(k) /= NO_DATA_REAL) vs%grid%lkg(ki) = vs%grid%lkg(ki) + vs%tile%lkg(k)*frac
            end if
!-            if (allocated(vs%grid%dzs) .and. allocated(vs%tile%dzs)) then
!-                if (vs%tile%dzs(k) /= huge(vs%tile%dzs)) vs%grid%dzs(ki) = vs%grid%dzs(ki) + vs%tile%dzs(k)*frac
!-            end if

            !> Routing variables.
!            if (allocated(vs%grid%rff) .and. allocated(vs%tile%rff)) then
!                if (vs%tile%rff(k) /= NO_DATA_REAL) vs%grid%rff(ki) = vs%grid%rff(ki) + vs%tile%rff(k)*frac
!            end if
!            if (allocated(vs%grid%qi) .and. allocated(vs%tile%qi)) then
!                if (vs%tile%qi(k) /= huge(vs%tile%qi)) vs%grid%qi(ki) = vs%grid%qi(ki) + vs%tile%qi(k)*frac
!            end if
!            if (allocated(vs%grid%qo) .and. allocated(vs%tile%qo)) then
!                if (vs%tile%qo(k) /= huge(vs%tile%qo)) vs%grid%qo(ki) = vs%grid%qo(ki) + vs%tile%qo(k)*frac
!            end if
!            if (allocated(vs%grid%stgch) .and. allocated(vs%tile%stgch)) then
!                if (vs%tile%stgch(k) /= huge(vs%tile%stgch)) vs%grid%stgch(ki) = vs%grid%stgch(ki) + vs%tile%stgch(k)*frac
!            end if
!            if (allocated(vs%grid%zlvl) .and. allocated(vs%tile%zlvl)) then
!                if (vs%tile%zlvl(k) /= huge(vs%tile%zlvl)) vs%grid%zlvl(ki) = vs%grid%zlvl(ki) + vs%tile%zlvl(k)*frac
!            end if
!            if (allocated(vs%grid%div) .and. allocated(vs%tile%div)) then
!                if (vs%tile%div(k) /= huge(vs%tile%div)) vs%grid%div(ki) = vs%grid%div(ki) + vs%tile%div(k)*frac
!            end if
!            if (allocated(vs%grid%abstr) .and. allocated(vs%tile%abstr)) then
!                if (vs%tile%abstr(k) /= huge(vs%tile%abstr)) vs%grid%abstr(ki) = vs%grid%abstr(ki) + vs%tile%abstr(k)*frac
!            end if
        end do

        !> Fractional averages.
        if (allocated(vs%grid%cmas)) then
            if (all(vs%grid%cmas(i1:i2) /= huge(vs%grid%cmas))) then
                where (tcanfrac(i1:i2) > 0.0) vs%grid%cmas(i1:i2) = vs%grid%cmas(i1:i2)/tcanfrac(i1:i2)
            end if
        end if
        if (allocated(vs%grid%tcan)) then
            if (all(vs%grid%tcan(i1:i2) /= huge(vs%grid%tcan))) then
                where (tcanfrac(i1:i2) > 0.0) vs%grid%tcan(i1:i2) = vs%grid%tcan(i1:i2)/tcanfrac(i1:i2)
            end if
        end if
        if (allocated(vs%grid%gro)) then
            if (all(vs%grid%gro(i1:i2) /= huge(vs%grid%gro))) then
                where (tcanfrac(i1:i2) > 0.0) vs%grid%gro(i1:i2) = vs%grid%gro(i1:i2)/tcanfrac(i1:i2)
            end if
        end if
        if (allocated(vs%grid%tsno)) then
            if (all(vs%grid%tsno(i1:i2) /= huge(vs%grid%tsno))) then
                where (tsnofrac(i1:i2) > 0.0) vs%grid%tsno(i1:i2) = vs%grid%tsno(i1:i2)/tsnofrac(i1:i2)
            end if
        end if
        if (allocated(vs%grid%rhosno)) then
            if (all(vs%grid%rhosno(i1:i2) /= huge(vs%grid%rhosno))) then
                where (tsnofrac(i1:i2) > 0.0) vs%grid%rhosno(i1:i2) = vs%grid%rhosno(i1:i2)/tsnofrac(i1:i2)
            end if
        end if
        if (allocated(vs%grid%tpnd)) then
            if (all(vs%grid%tpnd(i1:i2) /= huge(vs%grid%tpnd))) then
                where (tpndfrac(i1:i2) > 0.0) vs%grid%tpnd(i1:i2) = vs%grid%tpnd(i1:i2)/tpndfrac(i1:i2)
            end if
        end if
        if (allocated(vs%grid%tice)) then
            if (all(vs%grid%tice(i1:i2) /= huge(vs%grid%tice))) then
                where (ticefrac(i1:i2) > 0.0) vs%grid%tice(i1:i2) = vs%grid%tice(i1:i2)/ticefrac(i1:i2)
            end if
        end if

    end subroutine

    subroutine run_within_grid_finalize(fls, shd)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Return if tile and grid processes are not active.
        if (.not. ro%RUNTILE) return

    end subroutine

end module
