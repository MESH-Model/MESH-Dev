!>
!> Description:
!>  Subroutine to read initial states of variables from file. Variables
!>  shared by SA_MESH are accessible by 'sa_mesh_variables'.
!>  Other variables are accessible by their respecitve process
!>  module(s).
!>
subroutine read_initial_states(fls, shd, ierr)

    use strings
    use mpi_module
    use model_files_variables
    use sa_mesh_common
    use FLAGS

    use RUNCLASS36_constants
    use RUNCLASS36_variables
    use runsvs_mesh

    implicit none

    !> Input variables.
    type(fl_ids):: fls
    type(ShedGridParams) :: shd

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables for parsing INPUTPARAMSFORM.
    character(len = 20), dimension(100) :: out_args
    integer nargs
    character(1) :: delim = ' '

    !> Local variables.
    integer NA, NTYPE, NML, NSL, k, j, ignd, i, m

    !> Initialize the return status.
    ierr = 0

    !> Reset spacing for screen output.
    call reset_tab()

    !> Assign commonly used indices to local variables.
    NA = shd%NA
    NTYPE = shd%lc%NTYPE
    NML = shd%lc%NML
    NSL = shd%lc%IGND

    !>
    !> DISTRIBUTE.
    !>

    !> Distribute the values.
    do k = 1, shd%lc%NML

        !> Grab the indices of the grid cell and GRU.
        i = shd%lc%ILMOS(k)
        m = shd%lc%JLMOS(k)

        !> RUNCLASS36 and RUNSVS113.
        if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then
            vs%tile%tcan(k) = vs%gru%tcan(m) + TFREZ
            vs%tile%tsno(k) = vs%gru%tsno(m) + TFREZ
            vs%tile%rhos(k) = vs%gru%rhos(m)
            vs%tile%albs(k) = vs%gru%albs(m)
            vs%tile%tbar(k, :) = vs%gru%tbar(m, :) + TFREZ
            vs%tile%thlq(k, :) = vs%gru%thlq(m, :)
            vs%tile%thic(k, :) = vs%gru%thic(m, :)
        end if

        !> RUNCLASS36.
        if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
            vs%tile%tac(k) = vs%gru%tcan(m) + TFREZ
            vs%tile%qac(k) = 0.5e-2
            vs%tile%tpnd(k) = vs%gru%tpnd(m) + TFREZ
            vs%tile%zpnd(k) = vs%gru%zpnd(m)
            vs%tile%rcan(k) = vs%gru%rcan(m)
            vs%tile%sncan(k) = vs%gru%sncan(m)
            vs%tile%sno(k) = vs%gru%sno(m)
            vs%tile%gro(k) = vs%gru%gro(m)
            vs%tile%tsfs(k, 1) = TFREZ
            vs%tile%tsfs(k, 2) = TFREZ
            vs%tile%tsfs(k, 3) = vs%gru%tbar(m, 1) + TFREZ
            vs%tile%tsfs(k, 4) = vs%gru%tbar(m, 1) + TFREZ
            vs%tile%tbas(k) = vs%gru%tbar(m, NSL) + TFREZ
        end if

    end do !k = 1, shd%lc%NML

    !>
    !> RESUME FROM FILE.
    !>

    !> Parse the RESUMESTATES.
    call parse(RESUMESTATES, delim, out_args, nargs)

    !> Read the parameter values.
    select case (lowercase(out_args(1)))

        !> txt: In text format.
        !> seq: Sequential binary format.

        !> r2c: From r2c by grid.

        !> csv: From CSV by GRU.

    end select

    !> Distribute soil states to layers lower than the "last configured layer".
    if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. svs_mesh%PROCESS_ACTIVE) then

        !> Determine the "last configured layer" read from file (CLASS default: 3).
        if (NRSOILAYEREADFLAG > 3) then
            ignd = min(NRSOILAYEREADFLAG, NSL)
        else if (NRSOILAYEREADFLAG == 1) then
            ignd = 0
        else
            ignd = 3
        end if

        !> Assign states to layers lower than the "last configured layer" read from file.
        if (ignd > 0) then
            do j = (ignd + 1), shd%lc%IGND
                vs%tile%tbar(:, j) = vs%tile%tbar(:, ignd)
                vs%tile%thlq(:, j) = vs%tile%thlq(:, ignd)
                vs%tile%thic(:, j) = vs%tile%thic(:, ignd)
            end do
        end if
    end if

end subroutine
