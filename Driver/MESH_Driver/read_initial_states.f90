!>
!> Description:
!>  Subroutine to read initial states of variables from file. Variables
!>  shared by SA_MESH are accessible by sa_mesh_shared_variables module.
!>  Other variables are accessible by their respecitve process
!>  module(s).
!>
subroutine read_initial_states(fls, shd, ierr)

    use strings
    use mpi_module
    use model_files_variables
    use sa_mesh_shared_variables
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
    integer NA, NTYPE, NML, NSL, k, i, m

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

    !> Saul M. feb 26 2008:
    !> Open and read initial soil moisture and temperature values
    !> when data is available from:
    !>  - s_moisture.txt: soil moisture in layer 1, 2 and 3
    !>  - t_temperature.txt: soil temperature in layer 1, 2 and 3
!todo: Replace this with reading from r2c
    call READ_S_MOISTURE_TXT( &
        shd%yCount, shd%xCount, NA, NTYPE, NML, NSL, shd%yyy, shd%xxx, shd%lc%ILMOS, shd%lc%JLMOS, &
        stas%sl%thlq, il1, il2)
    call READ_S_TEMPERATURE_TXT(&
        shd%yCount, shd%xCount, NA, NTYPE, NML, NSL, shd%yyy, shd%xxx, shd%lc%ILMOS, shd%lc%JLMOS, &
        stas%sl%tbar, il1, il2)

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

end subroutine
