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
    integer n, m, k, l, i
    integer, dimension(:), allocatable :: SUBBASIN, LOCSTART, LOCSTOP
    type(ShedGridParams) SUBBASIN_shd
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

    !> Set gauge locations to 1.
    allocate(SUBBASIN(shd%NA), LOCSTART(fms%stmg%n), LOCSTOP(fms%stmg%n))
    SUBBASIN = 0
    LOCSTART = fms%stmg%meta%rnk
    LOCSTOP = fms%stmg%meta%rnk
    i = 0
    do l = 1, fms%stmg%n
        if (SUBBASIN(fms%stmg%meta%rnk(l)) > 0) then
            call print_remark('The outlet at ' // trim(fms%stmg%meta%name(l)) // ' already exists as a subbasin.')
            i = i + 1
        end if
        SUBBASIN(fms%stmg%meta%rnk(l)) = l
    end do
    if (i > 0) then
        write(line, FMT_GEN) i
        call print_message(trim(adjustl(line)) // ' station locations share the same RANK and are repeated subbasins.')
    end if
    write(line, FMT_GEN) (fms%stmg%n - i)
    call print_message('Masking domains for ' // trim(adjustl(line)) // ' subbasins.')

    !> Mask grids upstream of gauge locations.
    i = 1
    do while (i > 0)
        i = 0
        do n = 1, shd%NAA
            if (SUBBASIN(shd%NEXT(n)) > 0 .and. SUBBASIN(n) == 0) then
                SUBBASIN(n) = SUBBASIN(shd%NEXT(n))
                i = 1
                LOCSTART(SUBBASIN(n)) = min(n, LOCSTART(SUBBASIN(n)))
            end if
        end do
    end do

!temp
!    allocate(grid(shd%yCount, shd%xCount))
!    grid = 0
!    do n = 1, shd%NA
!        grid(shd%yyy(n), shd%xxx(n)) = SUBBASIN(n)
!    end do
!    do y = 1, shd%yCount
!        write(10, *) (grid(y, x), x = 1, shd%xCount)
!    end do
!    deallocate(grid)

        !> Set 'FRAC' to zero at inactive grids.
!?    where (SUBBASIN > 0) shd%FRAC = 0.0

    !> Reorder RANK/NEXT and transfer properties.
    i = 1
    do l = 1, fms%stmg%n
        do n = LOCSTART(l), LOCSTOP(l)
            if (SUBBASIN(n) == l) then

                !> Update streamflow gauge location (catching other stations that share the same location).
                if (n == LOCSTOP(l)) then
                    where (fms%stmg%meta%rnk == LOCSTOP(l)) fms%stmg%meta%rnk = i
                end if

                !> Copy cell properties.
                SUBBASIN_shd%xxx(i) = shd%xxx(n)
                SUBBASIN_shd%yyy(i) = shd%yyy(n)
                SUBBASIN_shd%IROUGH(i) = shd%IROUGH(n)
                SUBBASIN_shd%lc%ACLASS(i, :) = shd%lc%ACLASS(n, :)
                SUBBASIN_shd%NEXT(i) = i + 1
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

                !> Increment 'i' (the new NA).
                i = i + 1

                !> Update the location of the basin outlet via this cell's 'NEXT'.
                if (shd%NEXT(n) > 0) then
                    SUBBASIN_shd%xxx(i) = shd%xxx(shd%NEXT(n))
                    SUBBASIN_shd%yyy(i) = shd%yyy(shd%NEXT(n))
                end if
            end if
        end do
    end do

    !> Update the new values for 'NA' and 'NAA'.
    SUBBASIN_shd%NA = i
    SUBBASIN_shd%NAA = i - 1

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
    do l = 1, fms%stmg%n
        write(line, FMT_GEN) l, count(SUBBASIN == l)
        call print_echo_txt(line)
    end do

    !> Print a summary of locations to file.
    write(line, FMT_GEN) fms%stmg%n
    call print_message('Number of streamflow gauges: ' // trim(adjustl(line)))
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

end subroutine
