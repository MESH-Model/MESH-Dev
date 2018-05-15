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
    use RUNSVS113_variables

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
    do k = il1, il2

        !> Grab the indices of the grid cell and GRU.
        i = shd%lc%ILMOS(k)
        m = shd%lc%JLMOS(k)

        !> RUNCLASS36 and RUNSVS113.
        if (RUNCLASS36_flgs%PROCESS_ACTIVE .or. RUNSVS113_flgs%PROCESS_ACTIVE) then
            stas%cnpy%tcan(k) = stas_gru%cnpy%tcan(m) + TFREZ
            stas%sno%tsno(k) = stas_gru%sno%tsno(m) + TFREZ
            stas%sno%rhos(k) = stas_gru%sno%rhos(m)
            stas%sno%albs(k) = stas_gru%sno%albs(m)
            stas%sl%tbar(k, :) = stas_gru%sl%tbar(m, :) + TFREZ
            stas%sl%thlq(k, :) = stas_gru%sl%thlq(m, :)
        end if

        !> RUNCLASS36.
        if (RUNCLASS36_flgs%PROCESS_ACTIVE) then
            stas%cnpy%tac(k) = stas_gru%cnpy%tcan(m) + TFREZ
            stas%cnpy%qac = 0.5e-2
            stas%sfc%tpnd(k) = stas_gru%sfc%tpnd(m) + TFREZ
            stas%sfc%zpnd(k) = stas_gru%sfc%zpnd(m)
            stas%cnpy%rcan(k) = stas_gru%cnpy%rcan(m)
            stas%cnpy%sncan(k) = stas_gru%cnpy%sncan(m)
            stas%sno%sno(k) = stas_gru%sno%sno(m)
            stas%cnpy%gro(k) = stas_gru%cnpy%gro(m)
            stas%sfc%tsfs(k, 1) = TFREZ
            stas%sfc%tsfs(k, 2) = TFREZ
            stas%sfc%tsfs(k, 3) = stas_gru%sl%tbar(m, 1) + TFREZ
            stas%sfc%tsfs(k, 4) = stas_gru%sl%tbar(m, 1) + TFREZ
            stas%sl%tbas(k) = stas_gru%sl%tbar(m, NSL) + TFREZ
            stas%sl%thic(k, :) = stas_gru%sl%thic(m, :)
        end if

    end do !k = il1, il2

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
    if (RUNCLASS36_flgs%PROCESS_ACTIVE) then

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
                stas%sl%tbar(:, j) = stas%sl%tbar(:, ignd)
                stas%sl%thlq(:, j) = stas%sl%thlq(:, ignd)
                stas%sl%thic(:, j) = stas%sl%thic(:, ignd)
            end do
        end if
    end if

end subroutine
