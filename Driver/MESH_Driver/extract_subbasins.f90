subroutine extract_subbasins(shd, ierr)

    use mpi_module
    use strings
    use sa_mesh_common
    use FLAGS

    implicit none

    !> Input variables.
    type(ShedGridParams) shd

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    integer NSUBBSN, z, n, m, k, l, j, i
    integer, dimension(:), allocatable :: SUBBASIN, STMGID, RANKFIRST, RANKLAST, STGRANKNEW
    type(ShedGridParams) SUBBASIN_shd
    real :: tol = 1.0E-6, tol_diff
    character(len = DEFAULT_LINE_LENGTH) line

    !> Initialize the return status.
    ierr = 0

    !> Print message to screen.
    call reset_tab()
    call print_remark('SUBBASINFLAG is active. Deactivating non-contributing areas by station location.')
    call increase_tab()
    if (fms%stmg%n == 0) then
        call print_remark('No station locations exist. No subbasins are defined.')
        return
    end if

    !> Allocate and initialize new 'shd' variable.
    allocate( &
        SUBBASIN_shd%xxx(shd%NA), SUBBASIN_shd%yyy(shd%NA), &
        SUBBASIN_shd%IROUGH(shd%NA), &
        SUBBASIN_shd%lc%ACLASS(shd%NA, shd%lc%NTYPE + 1), &
        SUBBASIN_shd%NEXT(shd%NA), &
        SUBBASIN_shd%IAK(shd%NA), SUBBASIN_shd%SLOPE_CHNL(shd%NA), SUBBASIN_shd%CHNL_LEN(shd%NA), &
        SUBBASIN_shd%ICHNL(shd%NA), SUBBASIN_shd%IREACH(shd%NA), &
        SUBBASIN_shd%DA(shd%NA), SUBBASIN_shd%AREA(shd%NA), SUBBASIN_shd%FRAC(shd%NA), &
        SUBBASIN_shd%BNKFLL(shd%NA), &
        SUBBASIN_shd%ELEV(shd%NA), SUBBASIN_shd%SLOPE_INT(shd%NA), &
        SUBBASIN_shd%DRDN(shd%NA), &
        SUBBASIN_shd%ylat(shd%NA), SUBBASIN_shd%xlng(shd%NA))
    SUBBASIN_shd%xxx = 0; SUBBASIN_shd%yyy = 0
    SUBBASIN_shd%IROUGH = 0
    SUBBASIN_shd%lc%ACLASS = 0.0
    SUBBASIN_shd%NEXT = 0
    SUBBASIN_shd%IAK = 0; SUBBASIN_shd%SLOPE_CHNL = 0.0; SUBBASIN_shd%CHNL_LEN = 0.0
    SUBBASIN_shd%ICHNL = 0; SUBBASIN_shd%IREACH = 0
    SUBBASIN_shd%DA = 0.0; SUBBASIN_shd%AREA = 0.0; SUBBASIN_shd%FRAC = 0.0
    SUBBASIN_shd%BNKFLL = 0.0
    SUBBASIN_shd%ELEV = 0.0; SUBBASIN_shd%SLOPE_INT = 0.0
    SUBBASIN_shd%DRDN = 0.0
    SUBBASIN_shd%ylat = 0.0; SUBBASIN_shd%xlng = 0.0

    !> Sanity check.
!todo: Copy this and move commented section to 'read_initial_inputs', alongside other generic sanity checks.
!todo    write(line, FMT_GEN) tol
!todo    call print_message('Re-accumulating DA by NEXT to a tolerance of ' // trim(adjustl(line)) // ' km2 (sanity check).')
    shd%AREA = shd%DA
    do n = 1, shd%NAA
        if (shd%NEXT(n) > 0 .and. shd%NEXT(n) /= n) then
            shd%AREA(shd%NEXT(n)) = shd%AREA(shd%NEXT(n)) - shd%DA(n)
        end if
    end do
    shd%AREA = shd%AREA*1000000.0
    shd%FRAC = shd%AREA/(shd%AL**2)
!todo    SUBBASIN_shd%DA = SUBBASIN_shd%AREA/1000000.0
!todo    do n = 1, shd%NAA
!todo        if (shd%NEXT(n) > 0 .and. shd%NEXT(n) /= n) then
!todo            SUBBASIN_shd%DA(shd%NEXT(n)) = SUBBASIN_shd%DA(shd%NEXT(n)) + SUBBASIN_shd%DA(n)
!todo        end if
!todo    end do
!todo    tol_diff = maxval(abs(shd%DA - SUBBASIN_shd%DA))/(shd%AL**2)
!todo    write(line, FMT_GEN) tol_diff
!todo    if (tol_diff > tol) then
!todo        call print_warning( &
!todo            'The comparison of re-accumulated DA to the existing DA exceeds the tolerance: ' // trim(adjustl(line)) // ' km2.')
!todo    else
!todo        call print_remark( &
!todo            'The re-accumulated DA compared to the existing DA is within tolerance: ' // trim(adjustl(line)) // ' km2.')
!todo    end if
!todo    if (DIAGNOSEMODE) then
!todo        write(line, FMT_GEN) 'GAUGE', 'DA (km2)', 'REACC_DA (km2)'
!todo        call print_message(line)
!todo        do l = 1, fms%stmg%n
!todo            write(line, FMT_GEN) fms%stmg%meta%name(l), shd%DA(fms%stmg%meta%rnk(l)), SUBBASIN_shd%DA(fms%stmg%meta%rnk(l))
!todo            call print_message(line)
!todo        end do
!todo    end if

    !> Mask station locations (using an inverted index to allow the re-indexing that follows).
    allocate(SUBBASIN(shd%NA))
    i = 0
    do l = 1, fms%stmg%n
        if (SUBBASIN(fms%stmg%meta%rnk(l)) /= 0) then
            call print_remark('The outlet at ' // trim(fms%stmg%meta%name(l)) // ' already exists as a subbasin.')
            i = i + 1
        else
            SUBBASIN(fms%stmg%meta%rnk(l)) = (l*-1)
        end if
    end do
    NSUBBSN = fms%stmg%n - i
    if (i > 0) then
        write(line, FMT_GEN) i
        call print_message(trim(adjustl(line)) // ' station locations share the same RANK and are repeated subbasins.')
    end if
    write(line, FMT_GEN) NSUBBSN
    call print_message('Masking the domain for ' // trim(adjustl(line)) // ' subbasins.')
    do n = maxval(fms%stmg%meta%rnk), 1, -1
        if (SUBBASIN(n) /= 0) then
            do m = 1, n
                if (shd%NEXT(m) == n .and. SUBBASIN(m) == 0) SUBBASIN(m) = SUBBASIN(n)
            end do
        end if
    end do
    SUBBASIN_shd%NA = count(SUBBASIN /= 0)
    SUBBASIN_shd%NAA = count(SUBBASIN /= 0)

    !> Assign subbasin ID, considering nested subbasins.
    call print_message('Reordering and checking for nested subbasins.')
    allocate(STMGID(NSUBBSN), RANKFIRST(NSUBBSN), RANKLAST(NSUBBSN), STGRANKNEW(fms%stmg%n))
    STMGID = 0; RANKFIRST = 0; RANKLAST = 0; STGRANKNEW = 0
    i = NSUBBSN
    l = 0
    do while (i > 0)

        !> Exit if no more subbasins exists to be index.
        if (.not. any(SUBBASIN < 0)) exit

        !> Find the highest ranking subbasin.
        if (l == 0) then
            do n = maxval(fms%stmg%meta%rnk), 1, -1
                if (SUBBASIN(n) < 0) then
                    l = SUBBASIN(n)*-1
                    exit
                end if
            end do
        end if

        !> Update the subbasin ID.
        RANKLAST(i) = fms%stmg%meta%rnk(l)
        STMGID(i) = l
        do n = 1, fms%stmg%meta%rnk(l)
            if (SUBBASIN(n) == (l*-1)) then
                SUBBASIN(n) = i
                if (RANKFIRST(i) == 0) RANKFIRST(i) = n
            end if
        end do

        !> Check to see if a subbasin exists directly upstream or reset 'l' to find the next highest ranking subbasin.
        l = 0
        do n = 1, RANKLAST(i)
            if (SUBBASIN(n) < 0 .and. shd%NEXT(n) > 0) then
                if (SUBBASIN(shd%NEXT(n)) == i) then
                    l = SUBBASIN(n)*-1
                    exit
                end if
            end if
        end do

        !> De-increment the subbasin ID.
        i = i - 1
    end do
    if (DIAGNOSEMODE) then
        write(line, FMT_GEN) 'SUBBASIN', 'CELLS', 'OUTLET'
        call print_message(line)
        do l = 1, NSUBBSN
            line = '(new basin)'
            if (shd%NEXT(RANKLAST(l)) > 0) then
                if (SUBBASIN(shd%NEXT(RANKLAST(l))) /= 0) then
                    line = fms%stmg%meta%name(STMGID(SUBBASIN(shd%NEXT(RANKLAST(l)))))
                end if
            end if
            write(line, FMT_GEN) fms%stmg%meta%name(STMGID(l)), count(SUBBASIN == l), trim(line)
!debug            write(line, FMT_GEN) l, count(SUBBASIN == l), SUBBASIN(shd%NEXT(RANKLAST(l)))
            call print_message(line)
        end do
    end if

        !> Set 'FRAC' to zero at inactive grids.
!-    where (SUBBASIN > 0) shd%FRAC = 0.0

    !> Reorder RANK/NEXT and transfer properties.
    call print_message('Recalculating RANK and NEXT.')
    i = SUBBASIN_shd%NAA
    do l = NSUBBSN, 1, -1

        !> Update streamflow gauge location (catching other stations that share the same location).
        if (count(fms%stmg%meta%rnk == RANKLAST(l)) > 0) then
            where (fms%stmg%meta%rnk == RANKLAST(l)) STGRANKNEW = i
        end if

        !> Update 'NEXT' at the outlet.
        n = RANKLAST(l)
        if (shd%NEXT(n) > 0) then
            if (SUBBASIN(shd%NEXT(n)) == 0) then

                !> Add a basin-level outlet if 'NEXT' is outside the revised domain.
                SUBBASIN_shd%NA = SUBBASIN_shd%NA + 1
                SUBBASIN_shd%xxx(SUBBASIN_shd%NA) = shd%xxx(shd%NEXT(n))
                SUBBASIN_shd%yyy(SUBBASIN_shd%NA) = shd%yyy(shd%NEXT(n))
                SUBBASIN_shd%NEXT(i) = SUBBASIN_shd%NA
            else

                !> Updated 'NEXT' of the outlet if the subbasin leads to another subbasin.
                j = SUBBASIN_shd%NAA
                do m = NSUBBSN, l, -1
                    do k = RANKLAST(m), RANKFIRST(m), -1
                        if (SUBBASIN(k) == m) then
                            j = j - 1
                            if (SUBBASIN(k) > l .and. k == shd%NEXT(n)) then
                                SUBBASIN_shd%NEXT(i) = j
                                exit
                            end if
                        end if
                    end do
                    if (SUBBASIN_shd%NEXT(i) /= 0) exit
                end do
            end if
        end if

        !> Cascade.
        do n = RANKLAST(l), RANKFIRST(l), -1
            if (SUBBASIN(n) == l) then

                !> Transfer properties.
                SUBBASIN_shd%xxx(i) = shd%xxx(n)
                SUBBASIN_shd%yyy(i) = shd%yyy(n)
                SUBBASIN_shd%IROUGH(i) = shd%IROUGH(n)
                SUBBASIN_shd%lc%ACLASS(i, :) = shd%lc%ACLASS(n, :)
                SUBBASIN_shd%IAK(i) = shd%IAK(n)
                SUBBASIN_shd%SLOPE_CHNL(i) = shd%SLOPE_CHNL(n)
                SUBBASIN_shd%CHNL_LEN(i) = shd%CHNL_LEN(n)
                SUBBASIN_shd%ICHNL(i) = shd%ICHNL(n)
                SUBBASIN_shd%IREACH(i) = shd%IREACH(n)
                SUBBASIN_shd%DA(i) = shd%DA(n)
                SUBBASIN_shd%AREA(i) = shd%AREA(n)
                SUBBASIN_shd%FRAC(i) = shd%FRAC(n)
                SUBBASIN_shd%BNKFLL(i) = shd%BNKFLL(n)
                SUBBASIN_shd%ELEV(i) = shd%ELEV(n)
                SUBBASIN_shd%SLOPE_INT(i) = shd%SLOPE_INT(n)
                SUBBASIN_shd%DRDN(i) = shd%DRDN(n)
                SUBBASIN_shd%ylat(i) = shd%ylat(n)
                SUBBASIN_shd%xlng(i) = shd%xlng(n)

                !> Update all 'NEXT' of cells that flow into 'n'.
                j = i
                do k = (n - 1), RANKFIRST(l), -1
                    if (SUBBASIN(k) == l) then
                        j = j - 1
                        if (shd%NEXT(k) == n) then
                            SUBBASIN_shd%NEXT(j) = i
                        end if
                    end if
                end do

                !> De-increment 'i'.
                i = i - 1
            end if
        end do
    end do

    !> Sanity check.
    write(line, FMT_GEN) tol
    call print_message('Re-accumulating DA by NEXT to a tolerance of ' // trim(adjustl(line)) // ' km2 (sanity check).')
    SUBBASIN_shd%DA = SUBBASIN_shd%AREA/1000000.0
    do n = 1, SUBBASIN_shd%NAA
        SUBBASIN_shd%DA(SUBBASIN_shd%NEXT(n)) = SUBBASIN_shd%DA(SUBBASIN_shd%NEXT(n)) + SUBBASIN_shd%DA(n)
    end do
    tol_diff = maxval(abs(shd%DA(fms%stmg%meta%rnk(:)) - SUBBASIN_shd%DA(STGRANKNEW(:))))/(shd%AL**2)
    write(line, FMT_GEN) tol_diff
    if (tol_diff > tol) then
        call print_warning( &
            'The comparison of re-accumulated DA to the existing DA exceeds the tolerance: ' // trim(adjustl(line)) // ' km2.')
    else
        call print_remark( &
            'The re-accumulated DA compared to the existing DA is within tolerance: ' // trim(adjustl(line)) // ' km2.')
    end if
    if (DIAGNOSEMODE) then
        write(line, FMT_GEN) 'GAUGE', 'DA (km2)', 'REACC_DA (km2)'
        call print_message(line)
        do l = 1, fms%stmg%n
            write(line, FMT_GEN) fms%stmg%meta%name(l), shd%DA(fms%stmg%meta%rnk(l)), SUBBASIN_shd%DA(STGRANKNEW(l))
            call print_message(line)
        end do
    end if

    !> Reallocate the actual 'shd' variable.
    deallocate( &
        shd%xxx, shd%yyy, &
        shd%IROUGH, &
        shd%lc%ACLASS, &
        shd%NEXT, &
        shd%IAK, shd%SLOPE_CHNL, shd%CHNL_LEN, &
        shd%ICHNL, shd%IREACH, &
        shd%DA, shd%AREA, shd%FRAC, &
        shd%BNKFLL, &
        shd%ELEV, shd%SLOPE_INT, &
        shd%DRDN, &
        shd%ylat, shd%xlng)
    shd%NA = SUBBASIN_shd%NA
    shd%NAA = SUBBASIN_shd%NAA
    allocate( &
        shd%xxx(shd%NA), shd%yyy(shd%NA), &
        shd%IROUGH(shd%NA), &
        shd%lc%ACLASS(shd%NA, shd%lc%NTYPE + 1), &
        shd%NEXT(shd%NA), &
        shd%IAK(shd%NA), shd%SLOPE_CHNL(shd%NA), shd%CHNL_LEN(shd%NA), &
        shd%ICHNL(shd%NA), shd%IREACH(shd%NA), &
        shd%DA(shd%NA), shd%AREA(shd%NA), shd%FRAC(shd%NA), &
        shd%BNKFLL(shd%NA), &
        shd%ELEV(shd%NA), shd%SLOPE_INT(shd%NA), &
        shd%DRDN(shd%NA), &
        shd%ylat(shd%NA), shd%xlng(shd%NA))
    shd%xxx = SUBBASIN_shd%xxx(1:shd%NA)
    shd%yyy = SUBBASIN_shd%yyy(1:shd%NA)
    shd%IROUGH = SUBBASIN_shd%IROUGH(1:shd%NA)
    shd%lc%ACLASS = SUBBASIN_shd%lc%ACLASS(1:shd%NA, :)
    shd%NEXT = SUBBASIN_shd%NEXT(1:shd%NA)
    shd%IAK = SUBBASIN_shd%IAK(1:shd%NA)
    shd%SLOPE_CHNL = SUBBASIN_shd%SLOPE_CHNL(1:shd%NA)
    shd%CHNL_LEN = SUBBASIN_shd%CHNL_LEN(1:shd%NA)
    shd%ICHNL = SUBBASIN_shd%ICHNL(1:shd%NA)
    shd%IREACH = SUBBASIN_shd%IREACH(1:shd%NA)
    shd%DA = SUBBASIN_shd%DA(1:shd%NA)
    shd%AREA = SUBBASIN_shd%AREA(1:shd%NA)
    shd%FRAC = SUBBASIN_shd%FRAC(1:shd%NA)
    shd%BNKFLL = SUBBASIN_shd%BNKFLL(1:shd%NA)
    shd%ELEV = SUBBASIN_shd%ELEV(1:shd%NA)
    shd%SLOPE_INT = SUBBASIN_shd%SLOPE_INT(1:shd%NA)
    shd%DRDN = SUBBASIN_shd%DRDN(1:shd%NA)
    shd%ylat = SUBBASIN_shd%ylat(1:shd%NA)
    shd%xlng = SUBBASIN_shd%xlng(1:shd%NA)
    fms%stmg%meta%rnk = STGRANKNEW
    deallocate(RANKFIRST, RANKLAST, STGRANKNEW)

    !> Extract 'RNKGRD'.
    shd%RNKGRD = 0
    do n = 1, shd%NA
        shd%RNKGRD(shd%yyy(n), shd%xxx(n)) = n
    end do

    !> Override for other structures.
!todo: Reservoirs actually need to be re-assigned if within the subbasin space.
    if (fms%rsvr%n /= count(shd%IREACH > 0) .and. count(shd%IREACH > 0) > 0) then
        call print_error('Reservoirs exist within active subbasins but these have not been properly re-assigned.')
        call print_message('Reservoirs within subbasins is not currently supported.')
        ierr = 1
    end if
    fms%rsvr%n = count(shd%IREACH > 0)

    !> Print diagnostic information to screen.
    write(line, FMT_GEN) 'SUBBASIN', 'GRIDS'
    call print_echo_txt(line)
    do l = 1, NSUBBSN
        write(line, FMT_GEN) fms%stmg%meta%name(STMGID(l)), count(SUBBASIN == l)
        call print_echo_txt(line)
    end do
    deallocate(SUBBASIN, STMGID)

    !> Print a summary of locations to file.
    write(line, FMT_GEN) fms%stmg%n
    call print_echo_txt('Number of streamflow gauges: ' // trim(adjustl(line)))
    write(line, FMT_GEN) 'GAUGE', 'IY', 'JX', 'RANK', 'DA (km2)'
    call print_echo_txt(line)
    do i = 1, fms%stmg%n
        write(line, FMT_GEN) &
            fms%stmg%meta%name(i), fms%stmg%meta%iy(i), fms%stmg%meta%jx(i), fms%stmg%meta%rnk(i), shd%DA(fms%stmg%meta%rnk(i))
        call print_echo_txt(line)
    end do

!todo: Organization; need to place this here for now because SUBBASINFLAG depends on reading structures,
!      which depend on reading parameter files, because need first date to skip; skipping and init()
!      need to be separated or else (as for now), must repeat some of this code.

    !> Re-count and calculate active tile and grid indices.

    !> Compute the maximum number of tile elements.
    shd%lc%ILG = shd%NA*shd%lc%NTYPE
    shd%wc%ILG = shd%NA*shd%lc%NTYPE

    !> Determine the number of active tiles.
    !> Store callback indices in the 'IxMOS' and 'JxMOS' variables.
!todo: Fix this for water tiles.
    deallocate(shd%lc%ILMOS, shd%lc%JLMOS, &
               shd%wc%ILMOS, shd%wc%JLMOS)
    allocate(shd%lc%ILMOS(shd%lc%ILG), shd%lc%JLMOS(shd%lc%ILG), &
             shd%wc%ILMOS(shd%wc%ILG), shd%wc%JLMOS(shd%wc%ILG))
    shd%lc%NML = 0
    shd%wc%NML = 0
    do i = 1, shd%NA

        !> Only count active GRUs (with > 0.0 contributing fraction).
        if (shd%FRAC(i) > 0.0) then
            do m = 1, shd%lc%NTYPE

                !> Land.
                if (shd%lc%ACLASS(i, m) > 0.0) then
                    shd%lc%NML = shd%lc%NML + 1
                    shd%lc%ILMOS(shd%lc%NML) = i
                    shd%lc%JLMOS(shd%lc%NML) = m

                !> Water.
!                        else
!                            shd%wc%NML = shd%wc%NML + 1
!                            shd%wc%ILMOS(shd%wc%NML) = i
!                            shd%wc%JLMOS(shd%wc%NML) = m
                end if
            end do
        end if
    end do

    !> Write information about tile configuration to file.
    if (DIAGNOSEMODE) then

        !> Land tiles.
        write(line, FMT_GEN) shd%lc%NML
        call print_echo_txt('Number of land tiles (NML): ' // trim(adjustl(line)))
        if (shd%lc%NML > 0) then
            write(line, FMT_GEN) 'Tile ID', 'Grid', 'GRU'
            call print_echo_txt(line)
            do k = 1, shd%lc%NML
                write(line, FMT_GEN) k, shd%lc%ILMOS(k), shd%lc%JLMOS(k)
                call print_echo_txt(line)
            end do
        end if

        !> Water tiles.
        write(line, FMT_GEN) shd%wc%NML
        call print_echo_txt('Number of water tiles (NMW): ' // trim(adjustl(line)))
        if (shd%wc%NML > 0) then
            write(line, FMT_GEN) 'Tile ID', 'Grid', 'GRU'
            call print_echo_txt(line)
            do k = 1, shd%wc%NML
                write(line, FMT_GEN) k, shd%wc%ILMOS(k), shd%wc%JLMOS(k)
                call print_echo_txt(line)
            end do
        end if
    end if

    !> Calculate active tiles in the current node.
    !> Update grid indices.
    call mpi_split_nml(inp, izero, ipid, shd%lc%NML, shd%lc%ILMOS, il1, il2, iln)
    i1 = shd%lc%ILMOS(il1)
    i2 = shd%lc%ILMOS(il2)

    !> Print summary.
    call reset_tab()
    call print_remark('The basin configuration has changed.')
    call increase_tab()
    write(line, FMT_GEN) shd%NA
    call print_message('Total number of grids: ' // trim(adjustl(line)))
    write(line, FMT_GEN) shd%NAA
    call print_message('Total number of grids inside the basin: ' // trim(adjustl(line)))
    write(line, FMT_GEN) shd%AL
    call print_message('Side length of grid: ' // trim(adjustl(line)) // ' m')
    write(line, FMT_GEN) shd%lc%NTYPE
    call print_message('Number of GRUs: ' // trim(adjustl(line)))
    write(line, FMT_GEN) shd%lc%NML
    call print_message('Number of land-based tiles: ' // trim(adjustl(line)))
    write(line, FMT_GEN) shd%NRVR
    call print_message('Number of river classes: ' // trim(adjustl(line)))

    !> Save to file.
    z = 0
    call save_shed_r2c(shd, 100, 'MESH_drainage_database_SUBBASINFLAG.r2c', z)
    if (z /= 0) then
        call print_error('The file could not be saved.')
        call program_abort()
    else
        call print_message('The file has been saved.')
        call program_end()
    end if

end subroutine
