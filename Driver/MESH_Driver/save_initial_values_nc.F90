!>
!> Description:
!>  Subroutine to save initial states of variables to file. Variables
!>  shared by SA_MESH are accessible by 'sa_mesh_variables'.
!>  Other variables are accessible by their respecitve process
!>  module(s).
!>
subroutine save_initial_states_nc(fls, shd, fname, ierr)

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
    type(fl_ids):: fls
    type(ShedGridParams) :: shd
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    character(len = DEFAULT_LINE_LENGTH) line
    real, dimension(:), allocatable :: dat_xy(:, :), dat_xym(:, :, :), dat_xylm(:, :, :, :), dat_xycm(:, :, :, :)
    real, dimension(:), allocatable :: dat_xm(:, :), dat_xlm(:, :, :), dat_xcm(:, :, :)
    real, parameter :: NO_DATA = -999.999
    integer iun, m, l, c, x, y, v, j, i, z

    !> Initialize the return status.
    ierr = 0
#ifdef NETCDF

    !> Reset spacing for screen output.
    call reset_tab()

    !> Open output file.
    z = 0
    line = trim(fname) !'MESH_initial_values.nc'
    call nc4_open_output(line, iun = iun, ierr = ierr)
    if (ierr /= 0) return

    !> Add projection.
    if (z == 0) then
        call nc4_add_projection( &
            iun, shd%CoordSys%Proj, &
            shd%CoordSys%Ellips, shd%CoordSys%Zone, shd%CoordSys%earth_radius, shd%CoordSys%grid_north_pole_latitude, &
            shd%CoordSys%grid_north_pole_longitude, &
            ierr = z)
    end if

    !> Add dimensions.
    if (z == 0) then
        if (SHDFILEFMT == 5) then
            call nc4_define_dimension(iun, 'subbasin', dim_length = shd%xCount, did = x, ierr = z)
            call nc4_add_variable(iun, 'subbasin', x, shd%xxx, ierr = z)
            call nc4_add_variable(iun, 'lon', x, shd%CoordSys%lon, ierr = z)
            call nc4_add_variable(iun, 'lat', x, shd%CoordSys%lat, ierr = z)
        else
            call nc4_add_coordinates( &
                iun, shd%CoordSys%Proj, &
                shd%CoordSys%lat, shd%CoordSys%lon, shd%CoordSys%rlat, shd%CoordSys%rlon, &
                shd%CoordSys%xylat, shd%CoordSys%xylon, &
                shd%CoordSys%Ellips, shd%CoordSys%Zone, shd%CoordSys%earth_radius, shd%CoordSys%grid_north_pole_latitude, &
                shd%CoordSys%grid_north_pole_longitude, &
                dim1_id = y, dim2_id = x, &
                ierr = z)
        end if
    end if
    if (svs_mesh%PROCESS_ACTIVE .or. RUNCLASS36_flgs%PROCESS_ACTIVE) then
        if (z == 0) call nc4_define_dimension(iun, 'level', dim_length = shd%lc%IGND, did = l, ierr = z)
        if (z == 0) call nc4_define_dimension(iun, 'subtile_types', dim_length = 4, did = c, ierr = z)
        if (z == 0) call nc4_define_dimension(iun, 'gru', dim_length = shd%lc%NTYPE, did = m, ierr = z)
    end if

    !> CLASS variables.
    if (svs_mesh%PROCESS_ACTIVE .or. RUNCLASS36_flgs%PROCESS_ACTIVE) then
        if (SHDFILEFMT == 5) then
            allocate( &
                dat_xm(shd%xCount, shd%lc%NTYPE), dat_xlm(shd%xCount, shd%lc%IGND, shd%lc%NTYPE), &
                dat_xcm(shd%xCount, 4, shd%lc%NTYPE))
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%albsno(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_albs', x, m, dat_xm, long_name = 'Tile-based values for albs', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%cmas(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_cmas', x, m, dat_xm, long_name = 'Tile-based values for cmas', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%gro(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_gro', x, m, dat_xm, long_name = 'Tile-based values for gro', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%qacan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_qac', x, m, dat_xm, long_name = 'Tile-based values for qac', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%lqwscan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_rcan', x, m, dat_xm, long_name = 'Tile-based values for rcan', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%rhosno(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_rhos', x, m, dat_xm, long_name = 'Tile-based values for rhos', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%fzwscan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_sncan', x, m, dat_xm, long_name = 'Tile-based values for sncan', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%sno(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_sno', x, m, dat_xm, long_name = 'Tile-based values for sno', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%tacan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tac', x, m, dat_xm, long_name = 'Tile-based values for tac', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%tbas(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tbas', x, m, dat_xm, long_name = 'Tile-based values for tbas', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%tcan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tcan', x, m, dat_xm, long_name = 'Tile-based values for tcan', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xlm = NO_DATA
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        dat_xlm(shd%lc%ILMOS(i), j, shd%lc%JLMOS(i)) = vs%tile%tsol(i, j)
                    end do
                end do
                call nc4_add_variable( &
                    iun, 'tile_tbar', x, l, m, dat_xlm, long_name = 'Tile-based values for tbar', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xlm = NO_DATA
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        dat_xlm(shd%lc%ILMOS(i), j, shd%lc%JLMOS(i)) = vs%tile%thicsol(i, j)
                    end do
                end do
                call nc4_add_variable( &
                    iun, 'tile_thic', x, l, m, dat_xlm, long_name = 'Tile-based values for thic', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xlm = NO_DATA
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        dat_xlm(shd%lc%ILMOS(i), j, shd%lc%JLMOS(i)) = vs%tile%thlqsol(i, j)
                    end do
                end do
                call nc4_add_variable( &
                    iun, 'tile_thlq', x, l, m, dat_xlm, long_name = 'Tile-based values for thlq', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%tpnd(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tpnd', x, m, dat_xm, long_name = 'Tile-based values for tpnd', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xcm = NO_DATA
                do j = 1, 4
                    do i = 1, shd%lc%NML
                        dat_xcm(shd%lc%ILMOS(i), j, shd%lc%JLMOS(i)) = vs%tile%tsfs(i, j)
                    end do
                end do
                call nc4_add_variable( &
                    iun, 'tile_tsfs', x, c, m, dat_xcm, long_name = 'Tile-based values for tsfs', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%tsno(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tsno', x, m, dat_xm, long_name = 'Tile-based values for tsno', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%lqwssno(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_wsno', x, m, dat_xm, long_name = 'Tile-based values for wsno', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%zpnd(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_zpnd', x, m, dat_xm, long_name = 'Tile-based values for zpnd', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xm = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xm(shd%lc%ILMOS(i), shd%lc%JLMOS(i)) = vs%tile%stggw(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_lzs', x, m, dat_xm, long_name = 'Tile-based values for lzs', fill = NO_DATA, ierr = z)
            end if
        else
            allocate( &
                dat_xym(shd%xCount, shd%yCount, shd%lc%NTYPE), dat_xylm(shd%xCount, shd%yCount, shd%lc%IGND, shd%lc%NTYPE), &
                dat_xycm(shd%xCount, shd%yCount, 4, shd%lc%NTYPE))
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%albsno(i)
                end do
                call nc4_add_variable( &
                iun, 'tile_albs', x, y, m, dat_xym, long_name = 'Tile-based values for albs', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%cmas(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_cmas', x, y, m, dat_xym, long_name = 'Tile-based values for cmas', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%gro(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_gro', x, y, m, dat_xym, long_name = 'Tile-based values for gro', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%qacan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_qac', x, y, m, dat_xym, long_name = 'Tile-based values for qac', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%lqwscan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_rcan', x, y, m, dat_xym, long_name = 'Tile-based values for rcan', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%rhosno(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_rhos', x, y, m, dat_xym, long_name = 'Tile-based values for rhos', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%fzwscan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_sncan', x, y, m, dat_xym, long_name = 'Tile-based values for sncan', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%sno(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_sno', x, y, m, dat_xym, long_name = 'Tile-based values for sno', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tacan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tac', x, y, m, dat_xym, long_name = 'Tile-based values for tac', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tbas(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tbas', x, y, m, dat_xym, long_name = 'Tile-based values for tbas', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tcan(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tcan', x, y, m, dat_xym, long_name = 'Tile-based values for tcan', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xylm = NO_DATA
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) = vs%tile%tsol(i, j)
                    end do
                end do
                call nc4_add_variable( &
                    iun, 'tile_tbar', x, y, l, m, dat_xylm, long_name = 'Tile-based values for tbar', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xylm = NO_DATA
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) = vs%tile%thicsol(i, j)
                    end do
                end do
                call nc4_add_variable( &
                    iun, 'tile_thic', x, y, l, m, dat_xylm, long_name = 'Tile-based values for thic', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xylm = NO_DATA
                do j = 1, shd%lc%IGND
                    do i = 1, shd%lc%NML
                        dat_xylm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) = vs%tile%thlqsol(i, j)
                    end do
                end do
                call nc4_add_variable( &
                    iun, 'tile_thlq', x, y, l, m, dat_xylm, long_name = 'Tile-based values for thlq', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tpnd(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tpnd', x, y, m, dat_xym, long_name = 'Tile-based values for tpnd', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xycm = NO_DATA
                do j = 1, 4
                    do i = 1, shd%lc%NML
                        dat_xycm(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), j, shd%lc%JLMOS(i)) = vs%tile%tsfs(i, j)
                    end do
                end do
                call nc4_add_variable( &
                    iun, 'tile_tsfs', x, y, c, m, dat_xycm, long_name = 'Tile-based values for tsfs', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%tsno(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_tsno', x, y, m, dat_xym, long_name = 'Tile-based values for tsno', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%lqwssno(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_wsno', x, y, m, dat_xym, long_name = 'Tile-based values for wsno', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%zpnd(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_zpnd', x, y, m, dat_xym, long_name = 'Tile-based values for zpnd', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xym = NO_DATA
                do i = 1, shd%lc%NML
                    dat_xym(shd%xxx(shd%lc%ILMOS(i)), shd%yyy(shd%lc%ILMOS(i)), shd%lc%JLMOS(i)) = vs%tile%stggw(i)
                end do
                call nc4_add_variable( &
                    iun, 'tile_lzs', x, y, m, dat_xym, long_name = 'Tile-based values for lzs', fill = NO_DATA, ierr = z)
            end if
        end if
    end if

    !> Routing variables.
    if (WF_RTE_flgs%PROCESS_ACTIVE .or. rteflg%PROCESS_ACTIVE) then
        if (SHDFILEFMT == 5) then
            if (z == 0) then
                call nc4_add_variable( &
                    iun, 'grid_qi', x, vs%grid%qi, long_name = 'Grid-based values for qi', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                call nc4_add_variable( &
                    iun, 'grid_stgch', x, vs%grid%stgch, long_name = 'Grid-based values for stgch', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                call nc4_add_variable( &
                    iun, 'grid_qo', x, vs%grid%qo, long_name = 'Grid-based values for qo', fill = NO_DATA, ierr = z)
            end if
        else
            allocate(dat_xy(shd%xCount, shd%yCount))
            if (z == 0) then
                dat_xy = NO_DATA
                do i = 1, shd%NA
                    dat_xy(shd%xxx(i), shd%yyy(i)) = vs%grid%qi(i)
                end do
                call nc4_add_variable( &
                    iun, 'grid_qi', x, y, dat_xy, long_name = 'Grid-based values for qi', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xy = NO_DATA
                do i = 1, shd%NA
                    dat_xy(shd%xxx(i), shd%yyy(i)) = vs%grid%stgch(i)
                end do
                call nc4_add_variable( &
                    iun, 'grid_stgch', x, y, dat_xy, long_name = 'Grid-based values for stgch', fill = NO_DATA, ierr = z)
            end if
            if (z == 0) then
                dat_xy = NO_DATA
                do i = 1, shd%NA
                    dat_xy(shd%xxx(i), shd%yyy(i)) = vs%grid%qo(i)
                end do
                call nc4_add_variable( &
                    iun, 'grid_qo', x, y, dat_xy, long_name = 'Grid-based values for qo', fill = NO_DATA, ierr = z)
            end if
        end if
    end if

    !> Close file.
    if (z == 0) then
        call nc4_close_file(iun, line, ierr = z)
    end if
    if (z /= 0) then
        ierr = 1
    end if
#endif

end subroutine
