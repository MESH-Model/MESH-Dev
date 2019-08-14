!> Description:
!>  Subroutine to save basin properties to a single-frame 'r2c' format
!>  file.
!>
!> Input variables:
!*  shd: Basin 'shed' object (properties).
!*  iun: Unit of the input file.
!*  fname: Full path to the output file.
!*
!> Output variables:
!*  ierr: Return status.
subroutine save_shed_r2c(shd, iun, fname, ierr)

    !> strings: For 'lowercase' and 'compact' functions.
    !> sa_mesh_common: For common MESH variables and routines.
    !> ensim_io: For routines to read 'r2c' format file.
    use strings
    use sa_mesh_common
!+?    use ensim_io

    implicit none

    !> Input variables.
    type(ShedGridParams), intent(in) :: shd
    integer, intent(in) :: iun
    character(len = *), intent(in) :: fname

    !> Output variables.
    integer, intent(out) :: ierr

    !> Local variables.
    character(len = DEFAULT_LINE_LENGTH) line
    character(len = 1000000) long_line
    character(len = 10) str10
    character(len = 8) str8
    integer m, n, j, i
    real, dimension(shd%yCount, shd%xCount) :: r2c_grid

    !> Initialize the return status.
    ierr = 0

    !> Open the file and read the header.
    call reset_tab()
    call print_message('SAVING: ' // trim(fname))
    call increase_tab()

    !> Open the file (write access).
    ierr = 0
    open(iun, file = fname, status = 'replace', form = 'formatted', action = 'write', iostat = ierr)
    if (ierr /= 0) return

    !> Write header.
    write(iun, '(a)') '########################################'
    write(iun, '(a)') ':FileType r2c ASCII EnSim 1.0'
    write(iun, '(a)') '#'
    write(iun, '(a)') '# DataType 2D Rect Cell'
    write(iun, '(a)') '#'
    write(iun, '(a)') ':Application MESH_DRIVER'
    write(iun, '(a)') ':Version 1.0'
    write(iun, '(a)') ':WrittenBy SUBBASINFLAG'
    call date_and_time(str8, str10)
    write(line, "(a4, '/', a2, '/', a2, 1x, a2, ':', a2)") str8(1:4), str8(5:6), str8(7:8), str10(1:2), str10(3:4)
    write(iun, '(a)') ':CreationDate ' // trim(adjustl(line))
    write(iun, '(a)') '#'
    write(iun, '(a)') '#---------------------------------------'
    write(iun, '(a)') '#'
    write(iun, '(a)') ':Name MESH_DRAIN_DB'
    write(iun, '(a)') '#'
    write(line, *) shd%AL
    write(iun, '(a)') ':NominalGridSize_AL ' // trim(adjustl(line))
    write(line, *) 1.0
    write(iun, '(a)') ':ContourInterval ' // trim(adjustl(line))
    write(line, *) 0.0
    write(iun, '(a)') ':ImperviousArea ' // trim(adjustl(line))
    write(line, *) (shd%lc%NTYPE + 1)
    write(iun, '(a)') ':ClassCount ' // trim(adjustl(line))
    write(line, *) shd%NRVR
    write(iun, '(a)') ':NumRiverClasses ' // trim(adjustl(line))
    write(line, *) 1.0
    write(iun, '(a)') ':ElevConversion ' // trim(adjustl(line))
    write(line, *) shd%NA
    write(iun, '(a)') ':TotalNumOfGrids ' // trim(adjustl(line))
    write(line, *) shd%NAA
    write(iun, '(a)') ':NumGridsInBasin ' // trim(adjustl(line))
    write(iun, '(a)') ':DebugGridNo ' // trim(adjustl(line))
    write(iun, '(a)') '#'
    write(iun, '(a)') ':Projection ' // trim(shd%CoordSys%Proj)
    write(iun, '(a)') ':Ellipsoid ' // trim(shd%CoordSys%Ellips)
    if (lowercase(shd%CoordSys%Proj) == 'utm') then
        write(iun, '(a)') ':Zone ' // trim(shd%CoordSys%Zone)
    end if
    write(iun, '(a)') '#'
    write(line, *) shd%xOrigin
    write(iun, '(a)') ':xOrigin ' // trim(adjustl(line))
    write(line, *) shd%yOrigin
    write(iun, '(a)') ':yOrigin ' // trim(adjustl(line))
    write(iun, '(a)') '#'
    write(iun, '(a)') '#'
    write(iun, '(a)') ':AttributeName 1 Rank'
    write(iun, '(a)') ':AttributeType 1 integer'
    write(iun, '(a)') ':AttributeName 2 Next'
    write(iun, '(a)') ':AttributeType 2 integer'
    write(iun, '(a)') ':AttributeName 3 DA'
    write(iun, '(a)') ':AttributeUnits 3 km**2'
    write(iun, '(a)') ':AttributeName 4 Bankfull'
    write(iun, '(a)') ':AttributeUnits 4 m**3'
    write(iun, '(a)') ':AttributeName 5 ChnlSlope'
    write(iun, '(a)') ':AttributeUnits 5 m m**-1'
    write(iun, '(a)') ':AttributeName 6 Elev'
    write(iun, '(a)') ':AttributeUnits 6 m'
    write(iun, '(a)') ':AttributeName 7 ChnlLength'
    write(iun, '(a)') ':AttributeUnits 7 m'
    write(iun, '(a)') ':AttributeName 8 IAK'
    write(iun, '(a)') ':AttributeType 8 integer'
    write(iun, '(a)') ':AttributeName 9 Chnl'
    write(iun, '(a)') ':AttributeType 9 integer'
    write(iun, '(a)') ':AttributeName 10 Reach'
    write(iun, '(a)') ':AttributeType 10 integer'
    write(iun, '(a)') ':AttributeName 11 GridArea'
    write(iun, '(a)') ':AttributeUnits 11 m**2'
    write(iun, '(a)') ':AttributeName 12 Latitude'
    write(iun, '(a)') ':AttributeUnits 12 degrees'
    write(iun, '(a)') ':AttributeName 13 Longitude'
    write(iun, '(a)') ':AttributeUnits 13 degrees'
    write(iun, '(a)') ':AttributeName 14 IntSlope'
    write(iun, '(a)') ':AttributeUnits 14 m m**-1'
    do m = 1, shd%lc%NTYPE
        write(line, *) m + 14
        write(iun, '(a)') ':AttributeName ' // trim(adjustl(line)) // ' GRU'
        write(iun, '(a)') ':AttributeUnits ' // trim(adjustl(line)) // ' fraction'
    end do
    write(line, *) m + 14
    write(iun, '(a)') ':AttributeName ' // trim(adjustl(line)) // ' impervious'
    write(iun, '(a)') ':AttributeUnits ' // trim(adjustl(line)) // ' fraction'
    write(iun, '(a)') '#'
    write(line, *) shd%xCount
    write(iun, '(a)') ':xCount ' // trim(adjustl(line))
    write(line, *) shd%yCount
    write(iun, '(a)') ':yCount ' // trim(adjustl(line))
    write(line, *) shd%xDelta
    write(iun, '(a)') ':xDelta ' // trim(adjustl(line))
    write(line, *) shd%yDelta
    write(iun, '(a)') ':yDelta ' // trim(adjustl(line))
    write(iun, '(a)') '#'
    write(iun, '(a)') '#'
    write(iun, '(a)') ':EndHeader'

    !> Transfer data to temporary variable and write data.
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = n
    end do
    do j = 1, shd%yCount
        write(long_line, *) (int(r2c_grid(j, i)), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%NEXT(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (int(r2c_grid(j, i)), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%DA(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%BNKFLL(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%SLOPE_CHNL(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%ELEV(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%CHNL_LEN(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%IAK(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (int(r2c_grid(j, i)), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%ICHNL(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (int(r2c_grid(j, i)), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%IREACH(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (int(r2c_grid(j, i)), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%AREA(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%ylat(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%xlng(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    r2c_grid = 0.0
    do n = 1, shd%NA
        r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%SLOPE_INT(n)
    end do
    do j = 1, shd%yCount
        write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
        call compact(long_line)
        write(iun, '(a)', iostat = ierr) trim(long_line)
        if (ierr /= 0) return
    end do
    do m = 1, shd%lc%NTYPE + 1
        r2c_grid = 0.0
        do n = 1, shd%NA
            r2c_grid(shd%yyy(n), shd%xxx(n)) = shd%lc%ACLASS(n, m)
        end do
        do j = 1, shd%yCount
            write(long_line, *) (r2c_grid(j, i), i = 1, shd%xCount)
            call compact(long_line)
            write(iun, '(a)', iostat = ierr) trim(long_line)
            if (ierr /= 0) return
        end do
    end do

    !> Close the file to free the unit.
    close(iun)

end subroutine
