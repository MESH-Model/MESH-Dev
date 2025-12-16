!>
!> Description:
!>  Subroutine to read initial states of variables from file. Variables
!>  shared by SA_MESH are accessible by 'sa_mesh_variables'.
!>  Other variables are accessible by their respecitve process
!>  module(s).
!>
subroutine read_initial_states_nc(fls, shd, fname, ierr)

    !> Common modules.
    use model_files_variables
    use sa_mesh_common
    use nc_io

    !> Process modules.
    use RUNCLASS36_variables
    use runsvs_mesh
    use WF_ROUTE_config
    use rte_module

    implicit none

    !> Input variables.
    type(fl_ids), intent(in) :: fls
    type(ShedGridParams), intent(in) :: shd
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    character(len = DEFAULT_LINE_LENGTH) units, line
    real, dimension(:), allocatable :: dat_xy(:, :), dat_xym(:, :, :), dat_xylm(:, :, :, :), dat_xycm(:, :, :, :)
    real, dimension(:), allocatable :: dat_x(:), dat_xm(:, :), dat_xlm(:, :, :), dat_xcm(:, :, :)
    real fill_r
    integer iun, m, x, y, v, k, j, i, z

    !> Initialize the return status.
    ierr = 0
#ifdef NETCDF

    !> Reset spacing for screen output.
    call reset_tab()

    !> Open input file.
    z = 0
    line = trim(fname) !'MESH_initial_values.nc'
    call nc4_open_input(line, iun = iun, ierr = ierr)
    if (ierr /= 0) return

    !> CLASS variables.
    if (svs_mesh%PROCESS_ACTIVE .or. RUNCLASS36_flgs%PROCESS_ACTIVE) then
        if (SHDFILEFMT == 5) then
            allocate( &
                dat_xm(shd%xCount, shd%lc%NTYPE), dat_xlm(shd%xCount, shd%lc%IGND, shd%lc%NTYPE), &
                dat_xcm(shd%xCount, 4, shd%lc%NTYPE))
            if (z == 0) call nc4_get_variable(iun, 'tile_albs', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%albsno(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_cmas', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%cmai(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_gro', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%gro(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_qac', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%qacan(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_rcan', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%lqwscan(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_rhos', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%rhosno(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_sncan', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%fzwscan(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_sno', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%sno(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tac', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tacan(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tbas', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tbas(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tcan', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tcan(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tbar', 'subbasin', 'level', 'gru', dat_xlm, fill_r, ierr = z)
            if (z == 0) then
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        if (dat_xlm(shd%xxx(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                            vs%tile%tsol(i, j) = dat_xlm(shd%xxx(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                        end if
                    end do
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_thic', 'subbasin', 'level', 'gru', dat_xlm, fill_r, ierr = z)
            if (z == 0) then
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        if (dat_xlm(shd%xxx(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                            vs%tile%thicsol(i, j) = dat_xlm(shd%xxx(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                        end if
                    end do
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_thlq', 'subbasin', 'level', 'gru', dat_xlm, fill_r, ierr = z)
            if (z == 0) then
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        if (dat_xlm(shd%xxx(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                            vs%tile%thlqsol(i, j) = dat_xlm(shd%xxx(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                        end if
                    end do
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tpnd', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tpnd(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tsfs', 'subbasin', 'subtile_types', 'gru', dat_xcm, fill_r, ierr = z)
            if (z == 0) then
                do j = 1, 4
                    do i = 1, shd%lc%NML
                        if (dat_xcm(shd%xxx(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                            vs%tile%tsfs(i, j) = dat_xcm(shd%xxx(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                        end if
                    end do
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tsno', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tsno(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_wsno', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%lqwssno(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_zpnd', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%zpnd(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_lzs', 'subbasin', 'gru', dat_xm, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%stggw(i) = dat_xm(shd%xxx(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
        else
            allocate( &
                dat_xym(shd%xCount, shd%yCount, shd%lc%NTYPE), dat_xylm(shd%xCount, shd%yCount, shd%lc%IGND, shd%lc%NTYPE), &
                dat_xycm(shd%xCount, shd%yCount, 4, shd%lc%NTYPE))
            if (z == 0) call nc4_get_variable(iun, 'tile_albs', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%albsno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_cmas', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%cmai(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_gro', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%gro(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_qac', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%qacan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_rcan', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%lqwscan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_rhos', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%rhosno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_sncan', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%fzwscan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_sno', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%sno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tac', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tacan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tbas', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tbas(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tcan', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tcan(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tbar', 'lon', 'lat', 'level', 'gru', dat_xylm, fill_r, ierr = z)
            if (z == 0) then
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        if (dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                            vs%tile%tsol(i, j) = dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                        end if
                    end do
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_thic', 'lon', 'lat', 'level', 'gru', dat_xylm, fill_r, ierr = z)
            if (z == 0) then
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        if (dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                            vs%tile%thicsol(i, j) = dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                        end if
                    end do
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_thlq', 'lon', 'lat', 'level', 'gru', dat_xylm, fill_r, ierr = z)
            if (z == 0) then
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        if (dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                            vs%tile%thlqsol(i, j) = dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                        end if
                    end do
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tpnd', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tpnd(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tsfs', 'lon', 'lat', 'subtile_types', 'gru', dat_xycm, fill_r, ierr = z)
            if (z == 0) then
                do j = 1, 4
                    do i = 1, shd%lc%NML
                        if (dat_xycm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) /= fill_r) then
                            vs%tile%tsfs(i, j) = dat_xycm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i))
                        end if
                    end do
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_tsno', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%tsno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_wsno', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%lqwssno(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_zpnd', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%zpnd(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'tile_lzs', 'lon', 'lat', 'gru', dat_xym, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%lc%NML
                    if (dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) /= fill_r) then
                        vs%tile%stggw(i) = dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i))
                    end if
                end do
            end if
        end if
    end if

    !> Routing variables.
    if (WF_RTE_flgs%PROCESS_ACTIVE .or. rteflg%PROCESS_ACTIVE) then
        if (SHDFILEFMT == 5) then
            allocate(dat_x(shd%xCount))
            if (z == 0) call nc4_get_variable(iun, 'grid_qi', 'subbasin', dat_x, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%NA
                    if (dat_x(shd%xxx(i)) /= fill_r) vs%grid%qi(i) = dat_x(shd%xxx(i))
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'grid_stgch', 'subbasin', dat_x, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%NA
                    if (dat_x(shd%xxx(i)) /= fill_r) vs%grid%stgch(i) = dat_x(shd%xxx(i))
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'grid_qo', 'subbasin', dat_x, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%NA
                    if (dat_x(shd%xxx(i)) /= fill_r) vs%grid%qo(i) = dat_x(shd%xxx(i))
                end do
            end if
        else
            allocate(dat_xy(shd%xCount, shd%yCount))
            if (z == 0) call nc4_get_variable(iun, 'grid_qi', 'lon', 'lat', dat_xy, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%NA
                    if (dat_xy(shd%xxx(i), shd%yyy(i)) /= fill_r) vs%grid%qi(i) = dat_xy(shd%xxx(i), shd%yyy(i))
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'grid_stgch', 'lon', 'lat', dat_xy, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%NA
                    if (dat_xy(shd%xxx(i), shd%yyy(i)) /= fill_r) vs%grid%stgch(i) = dat_xy(shd%xxx(i), shd%yyy(i))
                end do
            end if
            if (z == 0) call nc4_get_variable(iun, 'grid_qo', 'lon', 'lat', dat_xy, fill_r, ierr = z)
            if (z == 0) then
                do i = 1, shd%NA
                    if (dat_xy(shd%xxx(i), shd%yyy(i)) /= fill_r) vs%grid%qo(i) = dat_xy(shd%xxx(i), shd%yyy(i))
                end do
            end if
        end if
    end if

    !> Close file.
    if (z == 0) then
        call nc4_close_file(iun, line, ierr = ierr)
    end if
    if (z /= 0) then
        ierr = 1
    end if
#endif

end subroutine
