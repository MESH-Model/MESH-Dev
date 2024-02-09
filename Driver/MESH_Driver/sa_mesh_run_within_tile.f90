module sa_mesh_run_within_tile

    !> 'model_files_variables' required for 'fls' object and file keys.
    !> 'sa_mesh_common' required for common SA_MESH variables and routines.
    !> 'model_dates' required for 'ic' counter.
    !> 'mpi_module' required for MPI variables, tile/grid parsing utility, barrier flag.
    !> 'variable_types' for model variable types.
    use model_files_variables
    use sa_mesh_common
    use model_dates
    use mpi_module
    use variable_types

    implicit none

    !> Description:
    !>  Variable lists for MPI exchanges.
    class(model_variable), dimension(:), allocatable, private :: model_variables_to_head, model_variables_to_node

    contains

    subroutine run_within_tile_init(fls, shd)

        !> Process modules.
        use RUNCLASS36_config
        use runsvs_mesh
        use irrigation_module
        use baseflow_module
        use cropland_irrigation_init
        use mountain_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
        integer n, j

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Call processes.
        call irrigation_init(fls, shd)
        call mountain_init(fls, shd)
        call RUNCLASS36_init(shd, fls)
        call runsvs_mesh_init(shd, fls)
        call bflm_init(fls, shd)
        call runci_init(shd, fls)

        !> Update variables.
        call run_within_tile_stas_update(fls, shd)

        !> Output files.
        call irrigation_open_output(fls, shd)

        !> Build MPI lists.
        if (inp > 0) then

            !> Count the number of active variables.
            n = 0
            if (allocated(vs%tile%prern)) n = n + 1
            if (allocated(vs%tile%presno)) n = n + 1
            if (allocated(vs%tile%lqwscan)) n = n + 1
            if (allocated(vs%tile%fzwscan)) n = n + 1
            if (allocated(vs%tile%cmas)) n = n + 1
            if (allocated(vs%tile%tacan)) n = n + 1
            if (allocated(vs%tile%qacan)) n = n + 1
            if (allocated(vs%tile%uvcan)) n = n + 1
            if (allocated(vs%tile%tcan)) n = n + 1
            if (allocated(vs%tile%gro)) n = n + 1
            if (allocated(vs%tile%sno)) n = n + 1
            if (allocated(vs%tile%rhosno)) n = n + 1
            if (allocated(vs%tile%fsno)) n = n + 1
            if (allocated(vs%tile%albsno)) n = n + 1
            if (allocated(vs%tile%lqwssno)) n = n + 1
            if (allocated(vs%tile%tsno)) n = n + 1
            if (allocated(vs%tile%drainsno)) n = n + 1
            if (allocated(vs%tile%albt)) n = n + 1
            if (allocated(vs%tile%alvs)) n = n + 1
            if (allocated(vs%tile%alir)) n = n + 1
            if (allocated(vs%tile%gte)) n = n + 1
            if (allocated(vs%tile%zpnd)) n = n + 1
            if (allocated(vs%tile%tpnd)) n = n + 1
            if (allocated(vs%tile%pndcaf)) n = n + 1
            if (allocated(vs%tile%potevp)) n = n + 1
            if (allocated(vs%tile%et)) n = n + 1
            if (allocated(vs%tile%ovrflw)) n = n + 1
            if (allocated(vs%tile%qevp)) n = n + 1
            if (allocated(vs%tile%qsens)) n = n + 1
            if (allocated(vs%tile%gzero)) n = n + 1
            if (allocated(vs%tile%tsurf)) n = n + 1
            if (allocated(vs%tile%tsfs)) n = n + size(vs%tile%tsfs, 2)
            if (allocated(vs%tile%lqwsice)) n = n + 1
            if (allocated(vs%tile%tice)) n = n + 1
            if (allocated(vs%tile%zsolsat)) n = n + 1
            if (allocated(vs%tile%ggeo)) n = n + 1
            if (allocated(vs%tile%tbas)) n = n + 1
            if (allocated(vs%tile%drainsol)) n = n + 1
            if (allocated(vs%tile%thlqsol)) n = n + size(vs%tile%thlqsol, 2)
            if (allocated(vs%tile%thicsol)) n = n + size(vs%tile%thicsol, 2)
            if (allocated(vs%tile%tsol)) n = n + size(vs%tile%tsol, 2)
            if (allocated(vs%tile%gflx)) n = n + size(vs%tile%gflx, 2)
            if (allocated(vs%tile%hcps)) n = n + size(vs%tile%hcps, 2)
            if (allocated(vs%tile%hcpc)) n = n + size(vs%tile%hcpc, 2)
            if (allocated(vs%tile%hcpg)) n = n + size(vs%tile%hcpg, 2)
            if (allocated(vs%tile%tctopc)) n = n + size(vs%tile%tctopc, 2)
            if (allocated(vs%tile%tctopg)) n = n + size(vs%tile%tctopg, 2)
            if (allocated(vs%tile%tcbotc)) n = n + size(vs%tile%tcbotc, 2)
            if (allocated(vs%tile%tcbotg)) n = n + size(vs%tile%tcbotg, 2)
            if (allocated(vs%tile%latflw)) n = n + size(vs%tile%latflw, 2)
            if (allocated(vs%tile%rchg)) n = n + 1
            if (allocated(vs%tile%stggw)) n = n + 1
            if (allocated(vs%tile%lkg)) n = n + 1
            if (bflm%BASEFLOWFLAG == 1) then
                if (allocated(Qb)) n = n + 1
            end if
            if (n > 0) then
                allocate(model_variables_to_head(n))

                !> Assign to the list.
                n = 1
                if (allocated(vs%tile%prern)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%prern))
                    n = n + 1
                end if
                if (allocated(vs%tile%presno)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%presno))
                    n = n + 1
                end if
                if (allocated(vs%tile%lqwscan)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%lqwscan))
                    n = n + 1
                end if
                if (allocated(vs%tile%fzwscan)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%fzwscan))
                    n = n + 1
                end if
                if (allocated(vs%tile%cmas)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%cmas))
                    n = n + 1
                end if
                if (allocated(vs%tile%tacan)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tacan))
                    n = n + 1
                end if
                if (allocated(vs%tile%qacan)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%qacan))
                    n = n + 1
                end if
                if (allocated(vs%tile%uvcan)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%uvcan))
                    n = n + 1
                end if
                if (allocated(vs%tile%tcan)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tcan))
                    n = n + 1
                end if
                if (allocated(vs%tile%gro)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%gro))
                    n = n + 1
                end if
                if (allocated(vs%tile%sno)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%sno))
                    n = n + 1
                end if
                if (allocated(vs%tile%rhosno)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%rhosno))
                    n = n + 1
                end if
                if (allocated(vs%tile%fsno)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%fsno))
                    n = n + 1
                end if
                if (allocated(vs%tile%albsno)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%albsno))
                    n = n + 1
                end if
                if (allocated(vs%tile%lqwssno)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%lqwssno))
                    n = n + 1
                end if
                if (allocated(vs%tile%tsno)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tsno))
                    n = n + 1
                end if
                if (allocated(vs%tile%drainsno)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%drainsno))
                    n = n + 1
                end if
                if (allocated(vs%tile%albt)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%albt))
                    n = n + 1
                end if
                if (allocated(vs%tile%alvs)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%alvs))
                    n = n + 1
                end if
                if (allocated(vs%tile%alir)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%alir))
                    n = n + 1
                end if
                if (allocated(vs%tile%gte)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%gte))
                    n = n + 1
                end if
                if (allocated(vs%tile%zpnd)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%zpnd))
                    n = n + 1
                end if
                if (allocated(vs%tile%tpnd)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tpnd))
                    n = n + 1
                end if
                if (allocated(vs%tile%pndcaf)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%pndcaf))
                    n = n + 1
                end if
                if (allocated(vs%tile%potevp)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%potevp))
                    n = n + 1
                end if
                if (allocated(vs%tile%et)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%et))
                    n = n + 1
                end if
                if (allocated(vs%tile%ovrflw)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%ovrflw))
                    n = n + 1
                end if
                if (allocated(vs%tile%qevp)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%qevp))
                    n = n + 1
                end if
                if (allocated(vs%tile%qsens)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%qsens))
                    n = n + 1
                end if
                if (allocated(vs%tile%gzero)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%gzero))
                    n = n + 1
                end if
                if (allocated(vs%tile%tsurf)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tsurf))
                    n = n + 1
                end if
                if (allocated(vs%tile%tsfs)) then
                    do j = 1, size(vs%tile%tsfs, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tsfs(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%lqwsice)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%lqwsice))
                    n = n + 1
                end if
                if (allocated(vs%tile%tice)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tice))
                    n = n + 1
                end if
                if (allocated(vs%tile%zsolsat)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%zsolsat))
                    n = n + 1
                end if
                if (allocated(vs%tile%ggeo)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%ggeo))
                    n = n + 1
                end if
                if (allocated(vs%tile%tbas)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tbas))
                    n = n + 1
                end if
                if (allocated(vs%tile%drainsol)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%drainsol))
                    n = n + 1
                end if
                if (allocated(vs%tile%thlqsol)) then
                    do j = 1, size(vs%tile%thlqsol, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%thlqsol(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%thicsol)) then
                    do j = 1, size(vs%tile%thicsol, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%thicsol(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%tsol)) then
                    do j = 1, size(vs%tile%tsol, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tsol(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%gflx)) then
                    do j = 1, size(vs%tile%gflx, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%gflx(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%hcps)) then
                    do j = 1, size(vs%tile%hcps, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%hcps(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%hcpc)) then
                    do j = 1, size(vs%tile%hcpc, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%hcpc(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%hcpg)) then
                    do j = 1, size(vs%tile%hcpg, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%hcpg(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%tctopc)) then
                    do j = 1, size(vs%tile%tctopc, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tctopc(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%tctopg)) then
                    do j = 1, size(vs%tile%tctopg, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tctopg(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%tcbotc)) then
                    do j = 1, size(vs%tile%tcbotc, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tcbotc(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%tcbotg)) then
                    do j = 1, size(vs%tile%tcbotg, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%tcbotg(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%latflw)) then
                    do j = 1, size(vs%tile%latflw, 2)
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%latflw(:, j)))
                        n = n + 1
                    end do
                end if
                if (allocated(vs%tile%rchg)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%rchg))
                    n = n + 1
                end if
                if (allocated(vs%tile%stggw)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%stggw))
                    n = n + 1
                end if
                if (allocated(vs%tile%lkg)) then
                    allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = vs%tile%lkg))
                    n = n + 1
                end if
                if (bflm%BASEFLOWFLAG == 1) then
                    if (allocated(Qb)) then
                        allocate(model_variables_to_head(n)%field, source = model_variable_pointer_1d(dat = Qb))
                        n = n + 1
                    end if
                end if
            end if
            n = 0
            if (irrm%PROCESS_ACTIVE) then
                if (allocated(vs%tile%pre)) n = n + 1
            end if
            if (n > 0) then
                allocate(model_variables_to_node(n))

                !> Assign to the list.
                n = 1
                if (irrm%PROCESS_ACTIVE) then
                    if (allocated(vs%tile%pre)) then
                        allocate(model_variables_to_node(n)%field, source = model_variable_pointer_1d(dat = vs%tile%pre))
                        n = n + 1
                    end if
                end if
            end if
        end if

    end subroutine

    subroutine run_within_tile(fls, shd)

        !> Process modules.
        use RUNCLASS36_module
        use runsvs_mesh
        use irrigation_module
        use baseflow_module
        use cropland_irrigation_within_tile
        use mountain_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Reset variables non-prognostic variables.
        call run_within_tile_stas_reset(fls, shd)

        !> Call processes.
        call irrigation_within_tile(fls, shd)

        !> MPI exchange.
        call run_within_tile_mpi_irecv(fls, shd)

        !> Call processes.
        call mountain_within_tile(fls, shd)
        call RUNCLASS36_within_tile(shd, fls)
        call runsvs_mesh_within_tile(shd, fls)
        call bflm_within_tile(fls, shd)
        call runci_within_tile(shd, fls)

        !> MPI exchange.
        call run_within_tile_mpi_isend(fls, shd)

        !> Update variables.
        call run_within_tile_stas_update(fls, shd)

        !> Output files.
        call irrigation_write_output(fls, shd)

    end subroutine

    subroutine run_within_tile_mpi_isend(fls, shd)

        !> Process modules.
!-        use baseflow_module
!-        use irrigation_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
!-        integer nvars, t, j, s
        integer u, i, c, ii1, ii2, iin, z
        logical lstat
        integer, allocatable :: irqst(:), imstat(:, :)
!-        real, dimension(:), allocatable :: buffer, met, cnpy, sno, sfc, ice, sl, lz, dz
        real, dimension(:), allocatable :: mpi_buffer_real1d

        !> Return if tile processes are not active or if no variables are active in the exchange.
        if (.not. ro%RUNTILE .or. .not. allocated(model_variables_to_head)) return

        !> Count the number of active variables included in the exchange.
!-        nvars = 7
!-        if (bflm%BASEFLOWFLAG == 1) nvars = nvars + 1
!-        if (nvars == 0) return

        !> Exchange variables.
!-        if (allocated(irqst)) deallocate(irqst)
!-        if (allocated(imstat)) deallocate(imstat)
!-        allocate(irqst(nvars), imstat(MPI_STATUS_SIZE, nvars))
        allocate(irqst(1), imstat(MPI_STATUS_SIZE, 1))

        !> Check active kind of 'real'.
        if (kind(2.0) == 8) then
            c = MPI_REAL8
        else
            c = MPI_REAL
        end if

        !> Other variables
!-        s = shd%lc%IGND

        if (inp > 1 .and. ipid /= 0) then

            !> Assign the indices.
            ii1 = il1; ii2 = il2; iin = iln

            !> Reset the exchange variables.
            irqst = MPI_REQUEST_NULL
!-            t = itag
!-            i = 1

            !> Send data back to head-node.
            allocate(mpi_buffer_real1d(size(model_variables_to_head)*iin))
            do i = 1, size(model_variables_to_head)
                select type (this => model_variables_to_head(i)%field)
                    type is (model_variable_pointer_1d)
                        mpi_buffer_real1d((1 + iin*(i - 1)):(iin*i)) = this%dat(ii1:ii2)
                end select
            end do
            call MPI_Isend(mpi_buffer_real1d, size(mpi_buffer_real1d), c, 0, itag, MPI_COMM_WORLD, irqst(1), z)

            !> Meteorology/climatology variables.
!-            allocate(met(2*iin))
!-            met((1 + iin*0):(iin*1)) = vs%tile%prern(ii1:ii2)
!-            met((1 + iin*1):(iin*2)) = vs%tile%presno(ii1:ii2)
!-            call MPI_Isend(met, size(met), c, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
!-            i = i + 1

            !> Canopy variables.
!-            allocate(cnpy(7*iin))
!-            cnpy((1 + iin*0):(iin*1)) = vs%tile%lqwscan(ii1:ii2)
!-            cnpy((1 + iin*1):(iin*2)) = vs%tile%fzwscan(ii1:ii2)
!-            cnpy((1 + iin*2):(iin*3)) = vs%tile%cmas(ii1:ii2)
!-            cnpy((1 + iin*3):(iin*4)) = vs%tile%tacan(ii1:ii2)
!-            cnpy((1 + iin*4):(iin*5)) = vs%tile%qacan(ii1:ii2)
!-            cnpy((1 + iin*5):(iin*6)) = vs%tile%tcan(ii1:ii2)
!-            cnpy((1 + iin*6):(iin*7)) = vs%tile%gro(ii1:ii2)
!-            call MPI_Isend(cnpy, size(cnpy), c, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
!-            i = i + 1

            !> Snow variables.
!-            allocate(sno(7*iin))
!-            sno((1 + iin*0):(iin*1)) = vs%tile%sno(ii1:ii2)
!-            sno((1 + iin*1):(iin*2)) = vs%tile%rhosno(ii1:ii2)
!-            sno((1 + iin*2):(iin*3)) = vs%tile%zsno(ii1:ii2)
!-            sno((1 + iin*2):(iin*3)) = vs%tile%fsno(ii1:ii2)
!-            sno((1 + iin*3):(iin*4)) = vs%tile%albsno(ii1:ii2)
!-            sno((1 + iin*4):(iin*5)) = vs%tile%lqwssno(ii1:ii2)
!-            sno((1 + iin*5):(iin*6)) = vs%tile%tsno(ii1:ii2)
!-            sno((1 + iin*6):(iin*7)) = vs%tile%drainsno(ii1:ii2)
!-            call MPI_Isend(sno, size(sno), c, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
!-            i = i + 1

            !> Surface variables.
!-            allocate(sfc((14 + 1*4)*iin))
!-            sfc((1 + iin*0):(iin*1)) = vs%tile%albt(ii1:ii2)
!-            sfc((1 + iin*1):(iin*2)) = vs%tile%alvs(ii1:ii2)
!-            sfc((1 + iin*2):(iin*3)) = vs%tile%alir(ii1:ii2)
!-            sfc((1 + iin*3):(iin*4)) = vs%tile%gte(ii1:ii2)
!-            sfc((1 + iin*4):(iin*5)) = vs%tile%zpnd(ii1:ii2)
!-            sfc((1 + iin*5):(iin*6)) = vs%tile%tpnd(ii1:ii2)
!-            sfc((1 + iin*6):(iin*7)) = vs%tile%pndcaf(ii1:ii2)
!-            sfc((1 + iin*7):(iin*8)) = vs%tile%potevp(ii1:ii2)
!-            sfc((1 + iin*8):(iin*9)) = vs%tile%et(ii1:ii2)
!-            sfc((1 + iin*9):(iin*10)) = vs%tile%ovrflw(ii1:ii2)
!-            sfc((1 + iin*10):(iin*11)) = vs%tile%qevp(ii1:ii2)
!-            sfc((1 + iin*11):(iin*12)) = vs%tile%qsens(ii1:ii2)
!-            sfc((1 + iin*12):(iin*13)) = vs%tile%gzero(ii1:ii2)
!-            sfc((1 + iin*13):(iin*14)) = vs%tile%tsurf(ii1:ii2)
!-            do j = 0, 3
!-                sfc((1 + iin*(14 + j)):(iin*(15 + j))) = vs%tile%tsfs(ii1:ii2, j + 1)
!-            end do
!-            call MPI_Isend(sfc, size(sfc), c, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
!-            i = i + 1

            !> Ice/glacier variables.
!-            allocate(ice(2*iin))
!-            ice((1 + iin*0):(iin*1)) = vs%tile%lqwsice(ii1:ii2)
!-            ice((1 + iin*1):(iin*2)) = vs%tile%tice(ii1:ii2)
!-            call MPI_Isend(ice, size(ice), c, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
!-            i = i + 1

            !> Subsurface/soil variables.
!-            allocate(sl((4 + 5*s)*iin))
!-            sl((1 + iin*0):(iin*1)) = vs%tile%zsolsat(ii1:ii2)
!-            sl((1 + iin*1):(iin*2)) = vs%tile%ggeo(ii1:ii2)
!-            sl((1 + iin*2):(iin*3)) = vs%tile%tbas(ii1:ii2)
!-            sl((1 + iin*3):(iin*4)) = vs%tile%drainsol(ii1:ii2)
!-            do j = 0, s - 1
!-                sl((1 + iin*(4 + j*5)):(iin*(5 + j*5))) = vs%tile%thlqsol(ii1:ii2, j + 1)
!-                sl((1 + iin*(5 + j*5)):(iin*(6 + j*5))) = vs%tile%thicsol(ii1:ii2, j + 1)
!-                sl((1 + iin*(6 + j*5)):(iin*(7 + j*5))) = vs%tile%tsol(ii1:ii2, j + 1)
!-                sl((1 + iin*(7 + j*5)):(iin*(8 + j*5))) = vs%tile%gflx(ii1:ii2, j + 1)
!-                sl((1 + iin*(8 + j*5)):(iin*(9 + j*5))) = vs%tile%latflw(ii1:ii2, j + 1)
!-            end do
!-            call MPI_Isend(sl, size(sl), c, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
!-            i = i + 1

            !> Groundwater/lower zone storage variables.
!-            allocate(lz(3*iin))
!-            lz((1 + iin*0):(iin*1)) = vs%tile%rchg(ii1:ii2)
!-            lz((1 + iin*1):(iin*2)) = vs%tile%stggw(ii1:ii2)
!-            lz((1 + iin*2):(iin*3)) = vs%tile%lkg(ii1:ii2)
!-            lz((1 + iin*2):(iin*3)) = vs%tile%dzs(ii1:ii2)
!-            call MPI_Isend(lz, size(lz), c, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
!-            i = i + 1

            !> BASEFLOWFLAG.
!-            if (bflm%BASEFLOWFLAG == 1) then
!-                call MPI_Isend(Qb(ii1:ii2), iin, c, 0, t + i, MPI_COMM_WORLD, irqst(i), z)
!-                i = i + 1
!-            end if

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
!-                call MPI_Testall(nvars, irqst, lstat, imstat, z)
                call MPI_Testall(1, irqst, lstat, imstat, z)
            end do

            !> Deallocate temporary arrays.
!-            deallocate(met, cnpy, sno, sfc, ice, sl, lz)

        else if (inp > 1) then

            !> Receive data from worker nodes.
            do u = 1, (inp - 1)

                !> Get and assign the indices.
                call mpi_split_nml(inp, izero, u, shd%lc%NML, shd%lc%ILMOS, ii1, ii2, iin)

                !> Allocate temporary arrays.
                allocate(mpi_buffer_real1d(size(model_variables_to_head)*iin))
!-                allocate(met(2*iin))
!-                allocate(cnpy(7*iin))
!-                allocate(sno(7*iin))
!-                allocate(sfc((14 + 1*4)*iin))
!-                allocate(ice(2*iin))
!-                allocate(sl((4 + 5*s)*iin))
!-                allocate(lz(3*iin))

                !> Reset the exchange variables.
                irqst = MPI_REQUEST_NULL
                imstat = 0
!-                t = itag
!-                i = 1

                !> Receive variables.
!-                call MPI_Irecv(met, size(met), c, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
!-                call MPI_Irecv(cnpy, size(cnpy), c, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
!-                call MPI_Irecv(sno, size(sno), c, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
!-                call MPI_Irecv(sfc, size(sfc), c, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
!-                call MPI_Irecv(ice, size(ice), c, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
!-                call MPI_Irecv(sl, size(sl), c, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
!-                call MPI_Irecv(lz, size(lz), c, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
                call MPI_Irecv(mpi_buffer_real1d, size(mpi_buffer_real1d), c, u, itag, MPI_COMM_WORLD, irqst(1), z)

                !> BASEFLOWFLAG.
!-                if (bflm%BASEFLOWFLAG == 1) then
!-                    call MPI_Irecv(Qb(ii1:ii2), iin, c, u, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
!-                end if

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
!-                    call MPI_Testall(nvars, irqst, lstat, imstat, z)
                    call MPI_Testall(1, irqst, lstat, imstat, z)
                end do

                !> Meteorology/climatology variables.
!-                vs%tile%prern(ii1:ii2) = met((1 + iin*0):(iin*1))
!-                vs%tile%presno(ii1:ii2) = met((1 + iin*1):(iin*2))

                !> Canopy variables.
!-                vs%tile%lqwscan(ii1:ii2) = cnpy((1 + iin*0):(iin*1))
!-                vs%tile%fzwscan(ii1:ii2) = cnpy((1 + iin*1):(iin*2))
!-                vs%tile%cmas(ii1:ii2) = cnpy((1 + iin*2):(iin*3))
!-                vs%tile%tacan(ii1:ii2) = cnpy((1 + iin*3):(iin*4))
!-                vs%tile%qacan(ii1:ii2) = cnpy((1 + iin*4):(iin*5))
!-                vs%tile%tcan(ii1:ii2) = cnpy((1 + iin*5):(iin*6))
!-                vs%tile%gro(ii1:ii2) = cnpy((1 + iin*6):(iin*7))

                !> Snow variables.
!-                vs%tile%sno(ii1:ii2) = sno((1 + iin*0):(iin*1))
!-                vs%tile%rhosno(ii1:ii2) = sno((1 + iin*1):(iin*2))
!-                vs%tile%zsno(ii1:ii2) = sno((1 + iin*2):(iin*3))
!-                vs%tile%fsno(ii1:ii2) = sno((1 + iin*2):(iin*3))
!-                vs%tile%albsno(ii1:ii2) = sno((1 + iin*3):(iin*4))
!-                vs%tile%lqwssno(ii1:ii2) = sno((1 + iin*4):(iin*5))
!-                vs%tile%tsno(ii1:ii2) = sno((1 + iin*5):(iin*6))
!-                vs%tile%drainsno(ii1:ii2) = sno((1 + iin*6):(iin*7))

                !> Surface variables.
!-                vs%tile%albt(ii1:ii2) = sfc((1 + iin*0):(iin*1))
!-                vs%tile%alvs(ii1:ii2) = sfc((1 + iin*1):(iin*2))
!-                vs%tile%alir(ii1:ii2) = sfc((1 + iin*2):(iin*3))
!-                vs%tile%gte(ii1:ii2) = sfc((1 + iin*3):(iin*4))
!-                vs%tile%zpnd(ii1:ii2) = sfc((1 + iin*4):(iin*5))
!-                vs%tile%tpnd(ii1:ii2) = sfc((1 + iin*5):(iin*6))
!-                vs%tile%pndcaf(ii1:ii2) = sfc((1 + iin*6):(iin*7))
!-                vs%tile%potevp(ii1:ii2) = sfc((1 + iin*7):(iin*8))
!-                vs%tile%et(ii1:ii2) = sfc((1 + iin*8):(iin*9))
!-                vs%tile%ovrflw(ii1:ii2) = sfc((1 + iin*9):(iin*10))
!-                vs%tile%qevp(ii1:ii2) = sfc((1 + iin*10):(iin*11))
!-                vs%tile%qsens(ii1:ii2) = sfc((1 + iin*11):(iin*12))
!-                vs%tile%gzero(ii1:ii2) = sfc((1 + iin*12):(iin*13))
!-                vs%tile%tsurf(ii1:ii2) = sfc((1 + iin*13):(iin*14))
!-                do j = 0, 3
!-                    vs%tile%tsfs(ii1:ii2, j + 1) = sfc((1 + iin*(14 + j)):(iin*(15 + j)))
!-                end do

                !> Ice/glacier variables.
!-                vs%tile%lqwsice(ii1:ii2) = ice((1 + iin*0):(iin*1))
!-                vs%tile%tice(ii1:ii2) = ice((1 + iin*1):(iin*2))

                !> Subsurface/soil variables.
!-                vs%tile%zsolsat(ii1:ii2) = sl((1 + iin*0):(iin*1))
!-                vs%tile%ggeo(ii1:ii2) = sl((1 + iin*1):(iin*2))
!-                vs%tile%tbas(ii1:ii2) = sl((1 + iin*2):(iin*3))
!-                vs%tile%drainsol(ii1:ii2) = sl((1 + iin*3):(iin*4))
!-                do j = 0, s - 1
!-                    vs%tile%thlqsol(ii1:ii2, j + 1) = sl((1 + iin*(4 + j*5)):(iin*(5 + j*5)))
!-                    vs%tile%thicsol(ii1:ii2, j + 1) = sl((1 + iin*(5 + j*5)):(iin*(6 + j*5)))
!-                    vs%tile%tsol(ii1:ii2, j + 1) = sl((1 + iin*(6 + j*5)):(iin*(7 + j*5)))
!-                    vs%tile%gflx(ii1:ii2, j + 1) = sl((1 + iin*(7 + j*5)):(iin*(8 + j*5)))
!-                    vs%tile%latflw(ii1:ii2, j + 1) = sl((1 + iin*(8 + j*5)):(iin*(9 + j*5)))
!-                end do

                !> Groundwater/lower zone storage variables.
!-                vs%tile%rchg(ii1:ii2) = lz((1 + iin*0):(iin*1))
!-                vs%tile%stggw(ii1:ii2) = lz((1 + iin*1):(iin*2))
!-                vs%tile%lkg(ii1:ii2) = lz((1 + iin*2):(iin*3))
!-                vs%tile%dzs(ii1:ii2) = lz((1 + iin*2):(iin*3))

                !> Transfer variables.
                do i = 1, size(model_variables_to_head)
                    select type (this => model_variables_to_head(i)%field)
                        type is (model_variable_pointer_1d)
                            this%dat(ii1:ii2) = mpi_buffer_real1d((1 + iin*(i - 1)):(iin*i))
                    end select
                end do

                !> Deallocate temporary arrays.
!-                deallocate(met, cnpy, sno, sfc, ice, sl, lz)
                deallocate(mpi_buffer_real1d)

            end do !u = 1, (inp - 1)

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) then
            call MPI_Barrier(MPI_COMM_WORLD, z)
            itag = 0
        else
!-            itag = t + i
            itag = itag + 1
        end if

    end subroutine

    subroutine run_within_tile_mpi_irecv(fls, shd)

        !> Process modules.
!-        use irrigation_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
!-        integer nvars, t, j
        integer u, i, c, ii1, ii2, iin, z
        logical lstat
        integer, allocatable :: irqst(:), imstat(:, :)
        real, dimension(:), allocatable :: mpi_buffer_real1d

        !> Return if tile processes are not active or if no variables are active in the exchange.
        if (.not. ro%RUNTILE .or. .not. allocated(model_variables_to_node)) return

        !> Count the number of active variables included in the exchange.
!-        nvars = 0
!-        if (irrm%PROCESS_ACTIVE) nvars = nvars + 1
!-        if (nvars == 0) return

        !> Exchange variables.
!-        if (allocated(irqst)) deallocate(irqst)
!-        if (allocated(imstat)) deallocate(imstat)
!-        allocate(irqst(nvars), imstat(MPI_STATUS_SIZE, nvars))
        allocate(irqst(1), imstat(MPI_STATUS_SIZE, 1))

        !> Check active kind of 'real'.
        if (kind(2.0) == 8) then
            c = MPI_REAL8
        else
            c = MPI_REAL
        end if

        !> Assign the indices.
        ii1 = 1
        ii2 = shd%lc%NML
        iin = shd%lc%NML

        !> Allocate temporary array.
        allocate(mpi_buffer_real1d(size(model_variables_to_node)*iin))

        if (inp > 1 .and. ipid == 0) then

            !> Send data to worker nodes.
            do u = 1, (inp - 1)

                !> Reset the exchange variables.
                irqst = MPI_REQUEST_NULL
                imstat = 0
!-                t = itag
!-                i = 1

!-                if (irrm%PROCESS_ACTIVE) then
!-                    call mpi_isend(vs%tile%pre(ii1:ii2), iin, c, u, t + i, MPI_COMM_WORLD, irqst(i), z)
!-                    i = i + 1
!-                end if

                !> Send data to the worker nodes.
                do i = 1, size(model_variables_to_node)
                    select type (this => model_variables_to_node(i)%field)
                        type is (model_variable_pointer_1d)
                            mpi_buffer_real1d((1 + iin*(i - 1)):(iin*i)) = this%dat(ii1:ii2)
                    end select
                end do
                call MPI_Isend(mpi_buffer_real1d, size(mpi_buffer_real1d), c, u, itag, MPI_COMM_WORLD, irqst(1), z)

                !> Wait until the exchange completes.
                lstat = .false.
                do while (.not. lstat)
!-                    call MPI_Testall(nvars, irqst, lstat, imstat, z)
                    call MPI_Testall(1, irqst, lstat, imstat, z)
                end do

            end do !u = 1, (inp - 1)

        else if (inp > 1) then

            !> Reset the exchange variables.
            irqst = MPI_REQUEST_NULL
!-            t = itag
!-            i = 1

!-            if (irrm%PROCESS_ACTIVE) then
!-                call mpi_irecv(vs%tile%pre(ii1:ii2), iin, c, 0, t + i, MPI_COMM_WORLD, irqst(i), z); i = i + 1
!-            end if

            !> Receive data from head-node.
            call MPI_Irecv(mpi_buffer_real1d, size(mpi_buffer_real1d), c, 0, itag, MPI_COMM_WORLD, irqst(1), z)

            !> Wait until the exchange completes.
            lstat = .false.
            do while (.not. lstat)
!-                call MPI_Testall(nvars, irqst, lstat, imstat, z)
                call MPI_Testall(1, irqst, lstat, imstat, z)
            end do

            !> Transfer variables.
            do i = 1, size(model_variables_to_node)
                select type (this => model_variables_to_node(i)%field)
                    type is (model_variable_pointer_1d)
                        this%dat(ii1:ii2) = mpi_buffer_real1d((1 + iin*(i - 1)):(iin*i))
                end select
            end do

        end if !(inp > 1 .and. ipid /= 0) then

        if (inp > 1 .and. ic%ts_daily == MPIUSEBARRIER) then
            call MPI_Barrier(MPI_COMM_WORLD, z)
            itag = 0
        else
!-            itag = t + i
            itag = itag + 1
        end if

    end subroutine

    subroutine run_within_tile_stas_reset(fls, shd)

        !> 'mesh_io_constants' for 'NODATA' values.
        use mesh_io_constants, only: NO_DATA_REAL

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Meteorology/climatology variables.
!        if (allocated(vs%tile%fsin)) vs%tile%fsin(il1:il2) = 0.0
!        if (allocated(vs%tile%fsvs)) vs%tile%fsvs(il1:il2) = 0.0
!        if (allocated(vs%tile%fsir)) vs%tile%fsir(il1:il2) = 0.0
!        if (allocated(vs%tile%fsdr)) vs%tile%fsdr(il1:il2) = 0.0
!        if (allocated(vs%tile%fsdff)) vs%tile%fsdff(il1:il2) = 0.0
!        if (allocated(vs%tile%flin)) vs%tile%flin(il1:il2) = 0.0
!        if (allocated(vs%tile%ta)) vs%tile%ta(il1:il2) = 0.0
!        if (allocated(vs%tile%qa)) vs%tile%qa(il1:il2) = 0.0
!        if (allocated(vs%tile%pres)) vs%tile%pres(il1:il2) = 0.0
!        if (allocated(vs%tile%uu)) vs%tile%uu(il1:il2) = 0.0
!        if (allocated(vs%tile%vv)) vs%tile%vv(il1:il2) = 0.0
!        if (allocated(vs%tile%uv)) vs%tile%uv(il1:il2) = 0.0
!        if (allocated(vs%tile%wdir)) vs%tile%wdir(il1:il2) = 0.0
!        if (allocated(vs%tile%prern)) vs%tile%prern(il1:il2) = 0.0
!        if (allocated(vs%tile%presno)) vs%tile%presno(il1:il2) = 0.0
!        if (allocated(vs%tile%pre)) vs%tile%pre(il1:il2) = 0.0

        !> Canopy variables.
!        if (allocated(vs%tile%lqwscan)) vs%tile%lqwscan(il1:il2) = 0.0
!        if (allocated(vs%tile%fzwscan)) vs%tile%fzwscan(il1:il2) = 0.0
!        if (allocated(vs%tile%cmas)) vs%tile%cmas(il1:il2) = 0.0
!        if (allocated(vs%tile%tacan)) vs%tile%tacan(il1:il2) = 0.0
!        if (allocated(vs%tile%qacan)) vs%tile%qacan(il1:il2) = 0.0
!        if (allocated(vs%tile%uvcan)) vs%tile%uvcan(il1:il2) = 0.0
!        if (allocated(vs%tile%tcan)) vs%tile%tcan(il1:il2) = 0.0
!        if (allocated(vs%tile%gro)) vs%tile%gro(il1:il2) = 0.0

        !> Snow variables.
        if (allocated(vs%tile%fsno)) vs%tile%fsno(il1:il2) = 0.0
!        if (allocated(vs%tile%sno)) vs%tile%sno(il1:il2) = 0.0
!        if (allocated(vs%tile%rhosno)) vs%tile%rhosno(il1:il2) = 0.0
!-        if (allocated(vs%tile%zsno)) vs%tile%zsno(il1:il2) = 0.0
!        if (allocated(vs%tile%lqwssno)) vs%tile%lqwssno(il1:il2) = 0.0
!        if (allocated(vs%tile%tsno)) vs%tile%tsno(il1:il2) = 0.0
!        if (allocated(vs%tile%albsno)) vs%tile%albsno(il1:il2) = 0.0
        if (allocated(vs%tile%drainsno)) vs%tile%drainsno(il1:il2) = 0.0

        !> Surface variables.
        if (allocated(vs%tile%albt)) vs%tile%albt(il1:il2) = 0.0
        if (allocated(vs%tile%alvs)) vs%tile%alvs(il1:il2) = 0.0
        if (allocated(vs%tile%alir)) vs%tile%alir(il1:il2) = 0.0
        if (allocated(vs%tile%gte)) vs%tile%gte(il1:il2) = 0.0
!        if (allocated(vs%tile%zpnd)) vs%tile%zpnd(il1:il2) = 0.0
!-        if (allocated(vs%tile%lqwspnd)) vs%tile%lqwspnd(il1:il2) = 0.0
!        if (allocated(vs%tile%tpnd)) vs%tile%tpnd(il1:il2) = 0.0
        if (allocated(vs%tile%pndcaf)) vs%tile%pndcaf(il1:il2) = 0.0
        if (allocated(vs%tile%potevp)) vs%tile%potevp(il1:il2) = 0.0
        if (allocated(vs%tile%et)) vs%tile%et(il1:il2) = 0.0
!-        if (allocated(vs%tile%evpb)) vs%tile%evpb(il1:il2) = 0.0
!-        if (allocated(vs%tile%arrd)) vs%tile%arrd(il1:il2) = 0.0
        if (allocated(vs%tile%ovrflw)) vs%tile%ovrflw(il1:il2) = 0.0
        if (allocated(vs%tile%qevp)) vs%tile%qevp(il1:il2) = 0.0
        if (allocated(vs%tile%qsens)) vs%tile%qsens(il1:il2) = 0.0
        if (allocated(vs%tile%gzero)) vs%tile%gzero(il1:il2) = 0.0
!        if (allocated(vs%tile%tsfs)) vs%tile%tsfs(il1:il2, :) = 0.0
!        if (allocated(vs%tile%tsurf)) vs%tile%tsurf(il1:il2) = 0.0

        !> Ice/glacier variables.
        if (allocated(vs%tile%lqwsice)) vs%tile%lqwsice(il1:il2) = 0.0
        if (allocated(vs%tile%tice)) vs%tile%tice(il1:il2) = 0.0

        !> Subsurface/soil variables.
!        if (allocated(vs%tile%dzsol)) vs%tile%dzsol(il1:il2) = 0.0
!        if (allocated(vs%tile%dzsolhyd)) vs%tile%dzsolhyd(il1:il2) = 0.0
!        if (allocated(vs%tile%thlqsol)) vs%tile%thlqsol(il1:il2, :) = 0.0
!        if (allocated(vs%tile%thicsol)) vs%tile%thicsol(il1:il2, :) = 0.0
!        if (allocated(vs%tile%lqwssol)) vs%tile%lqwssol(il1:il2, :) = 0.0
!        if (allocated(vs%tile%fzwssol)) vs%tile%fzwssol(il1:il2, :) = 0.0
!        if (allocated(vs%tile%tsol)) vs%tile%tsol(il1:il2, :) = 0.0
        if (allocated(vs%tile%gflx)) vs%tile%gflx(il1:il2, :) = 0.0
        if (allocated(vs%tile%hcps)) vs%tile%hcps(il1:il2, :) = 0.0
        if (allocated(vs%tile%hcpc)) vs%tile%hcpc(il1:il2, :) = 0.0
        if (allocated(vs%tile%hcpg)) vs%tile%hcpg(il1:il2, :) = 0.0		
        if (allocated(vs%tile%tctopc)) vs%tile%tctopc(il1:il2, :) = 0.0
        if (allocated(vs%tile%tctopg)) vs%tile%tctopg(il1:il2, :) = 0.0
        if (allocated(vs%tile%tcbotc)) vs%tile%tcbotc(il1:il2, :) = 0.0
        if (allocated(vs%tile%tcbotg)) vs%tile%tcbotg(il1:il2, :) = 0.0
        if (allocated(vs%tile%latflw)) vs%tile%latflw(il1:il2, :) = 0.0
!        if (allocated(vs%tile%zsol)) vs%tile%zsol(il1:il2, :) = 0.0
!        if (allocated(vs%tile%zsolhyd)) vs%tile%zsolhyd(il1:il2, :) = 0.0
        if (allocated(vs%tile%zsolsat)) vs%tile%zsolsat(il1:il2) = 0.0
!        if (allocated(vs%tile%ggeo)) vs%tile%ggeo(il1:il2) = 0.0
!        if (allocated(vs%tile%tbas)) vs%tile%tbas(il1:il2) = 0.0
        if (allocated(vs%tile%drainsol)) vs%tile%drainsol(il1:il2) = 0.0

        !> Groundwater/lower zone storage variables.
        if (allocated(vs%tile%rchg)) vs%tile%rchg(il1:il2) = NO_DATA_REAL
        if (allocated(vs%tile%stggw)) vs%tile%stggw(il1:il2) = 0.0
        if (allocated(vs%tile%lkg)) vs%tile%lkg(il1:il2) = NO_DATA_REAL
!-       if (allocated(vs%tile%dzs)) vs%tile%dzs(il1:il2) = 0.0

        !> Diagnostic variables.
!-        if (allocated(vs%tile%stge)) vs%tile%stge(il1:il2) = 0.0
!-        if (allocated(vs%tile%stgw)) vs%tile%stgw(il1:il2) = 0.0

        !> Routing variables.
        if (allocated(vs%tile%rff)) vs%tile%rff(il1:il2) = NO_DATA_REAL
!        if (allocated(vs%tile%qi)) vs%tile%qi(il1:il2) = 0.0
!        if (allocated(vs%tile%qo)) vs%tile%qo(il1:il2) = 0.0
!        if (allocated(vs%tile%stgch)) vs%tile%stgch(il1:il2) = 0.0
!        if (allocated(vs%tile%zlvl)) vs%tile%zlvl(il1:il2) = 0.0
!        if (allocated(vs%tile%div)) vs%tile%div(il1:il2) = 0.0
!        if (allocated(vs%tile%abstr)) vs%tile%abstr(il1:il2) = 0.0

    end subroutine

    subroutine run_within_tile_stas_update(fls, shd)

!+todo: There's a dependency on CLASSBD.f.
        use RUNCLASS36_constants, only: RHOW, RHOICE

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
        integer j

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Update variables.
        if (allocated(vs%tile%sno)) then
            if (allocated(vs%tile%lqwssno)) then
                where (vs%tile%sno(il1:il2) == 0.0) vs%tile%lqwssno(il1:il2) = 0.0
            end if
            if (allocated(vs%tile%tsno)) then
                where (vs%tile%sno(il1:il2) == 0.0) vs%tile%tsno(il1:il2) = 0.0
            end if
!-            if (allocated(vs%tile%zsno) .and. allocated(vs%tile%rhosno)) then
!-                if (all(vs%tile%zsno(il1:il2) == 0.0)) then
!-                    where (vs%tile%rhosno(il1:il2) > 0.0) vs%tile%zsno(il1:il2) = vs%tile%sno(il1:il2)/vs%tile%rhosno(il1:il2)
!-                end if
!-            end if
        end if
        if (allocated(vs%tile%albt) .and. allocated(vs%tile%alvs) .and. allocated(vs%tile%alir)) then
            if (all(vs%tile%albt(il1:il2) == 0.0)) then
                where (vs%tile%alvs(il1:il2) > 0.0 .and. vs%tile%alir(il1:il2) > 0.0)
                    vs%tile%albt(il1:il2) = (vs%tile%alvs(il1:il2) + vs%tile%alir(il1:il2))/2.0
                elsewhere
                    vs%tile%albt(il1:il2) = 0.0
                end where
            end if
        end if
        if (allocated(vs%tile%zpnd)) then
!-            if (allocated(vs%tile%lqwspnd)) vs%tile%lqwspnd(il1:il2) = vs%tile%zpnd(il1:il2)*RHOW
            if (allocated(vs%tile%tpnd)) then
                where (vs%tile%zpnd(il1:il2) == 0.0) vs%tile%tpnd(il1:il2) = 0.0
            end if
        end if
!-        if (allocated(vs%tile%et)) then
!-            if (allocated(vs%tile%evpb) .and. allocated(vs%tile%potevp)) then
!-                where (vs%tile%et(il1:il2) > 0.0 .and. vs%tile%potevp(il1:il2) /= 0.0)
!-                    vs%tile%evpb(il1:il2) = vs%tile%et(il1:il2)/vs%tile%potevp(il1:il2)
!-                elsewhere
!-                    vs%tile%evpb(il1:il2) = 0.0
!-                end where
!-            end if
!-            if (allocated(vs%tile%arrd) .and. allocated(vs%tile%pre)) then
!-                where (vs%tile%potevp(il1:il2) /= 0.0)
!-                    vs%tile%arrd(il1:il2) = vs%tile%pre(il1:il2)/vs%tile%potevp(il1:il2)
!-                elsewhere
!-                    vs%tile%arrd(il1:il2) = 0.0
!-                end where
!-            end if
!-        end if
        if (allocated(vs%tile%dzsolhyd)) then
            if (allocated(vs%tile%lqwssol) .and. allocated(vs%tile%thlqsol)) then
                vs%tile%lqwssol(il1:il2, :) = vs%tile%thlqsol(il1:il2, :)*vs%tile%dzsolhyd(il1:il2, :)*RHOW
            end if
            if (allocated(vs%tile%fzwssol) .and. allocated(vs%tile%thicsol)) then
                vs%tile%fzwssol(il1:il2, :) = vs%tile%thicsol(il1:il2, :)*vs%tile%dzsolhyd(il1:il2, :)*RHOICE
            end if
        end if
        if (allocated(vs%tile%rchg) .and. allocated(vs%tile%drainsol)) then
            if (all(vs%tile%rchg(il1:il2) == NO_DATA_REAL)) then
                where (vs%tile%drainsol(il1:il2) /= NO_DATA_REAL) vs%tile%rchg(il1:il2) = vs%tile%drainsol(il1:il2)*ic%dts
            end if
        end if
        if (allocated(vs%tile%lkg) .and. allocated(vs%tile%rchg)) then
            if (all(vs%tile%lkg(il1:il2) == NO_DATA_REAL)) then
                where (vs%tile%rchg(il1:il2) /= NO_DATA_REAL) vs%tile%lkg(il1:il2) = vs%tile%rchg(il1:il2)
            end if
        end if
        if (allocated(vs%tile%rff)) then
            if (all(vs%tile%rff(il1:il2) == NO_DATA_REAL)) then
                vs%tile%rff = 0.0
                if (allocated(vs%tile%ovrflw)) then
                    where (vs%tile%ovrflw(il1:il2) /= NO_DATA_REAL)
                        vs%tile%rff(il1:il2) = vs%tile%rff(il1:il2) + vs%tile%ovrflw(il1:il2)*ic%dts
                    end where
                end if
                if (allocated(vs%tile%latflw)) then
                    do j = 1, size(vs%tile%latflw, 2)
                        where (vs%tile%latflw(il1:il2, j) /= NO_DATA_REAL)
                            vs%tile%rff(il1:il2) = vs%tile%rff(il1:il2) + vs%tile%latflw(il1:il2, j)*ic%dts
                        end where
                    end do
                end if
                if (allocated(vs%tile%lkg)) then
                    where (vs%tile%lkg(il1:il2) /= NO_DATA_REAL)
                        vs%tile%rff(il1:il2) = vs%tile%rff(il1:il2) + vs%tile%lkg(il1:il2)
                    end where
                end if
            end if
        end if

    end subroutine

    subroutine run_within_tile_finalize(fls, shd)

        !> Process modules.
        use RUNCLASS36_config
        use runsvs_mesh
        use baseflow_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Return if tile processes are not active.
        if (.not. ro%RUNTILE) return

        !> Call processes.
        call RUNCLASS36_finalize(fls, shd)
        call bflm_finalize(fls, shd)
        call runsvs_mesh_finalize(shd, fls)

    end subroutine

end module
