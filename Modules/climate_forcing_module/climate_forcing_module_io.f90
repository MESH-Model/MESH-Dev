!> Description:
!>  Module to read climate forcing data from file.
module climate_forcing_io

    use climate_forcing_constants
    use climate_forcing_variabletypes
    use print_routines

    implicit none

    real, parameter, private :: NO_DATA = -999.999

    contains

    !> Description:
    !>  Open the climate forcing input file.
    !>
    !> Input/output variables:
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and its unit.
    !*  vid: Index of the climate forcing variable.
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error opening the file.
    logical function open_data(shd, cm, vid) result(ENDDATA)

        !> 'shd_variables': For 'shd' variable.
        use shd_variables

        !> 'strings': For 'lowercase' function.
        use strings, only: lowercase

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: vid

        !> Input/Output variables.
        type(clim_info) cm

        !> Local variables.
        integer ierr
        character(len = DEFAULT_LINE_LENGTH) line

        !> Initialize the return variable.
        ENDDATA = .false.

        !> Return if the variable is not marked active.
        if (.not. cm%dat(vid)%factive) return

        !> Open file depending on the format type of the climate data.
        select case (cm%dat(vid)%ffmt)

            !> ASCII R2C format.
            case (1)

                !> Open the file.
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.r2c'
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)

                !> Return on an error.
                if (ierr /= 0) goto 999

                !> Skip the header of the 'r2c' format file.
                line = ''
                do while (lowercase(line) /= ':endheader')
                    read(cm%dat(vid)%fiun, '(a10)', end = 998) line
                end do

                !> Set the block type.
                cm%dat(vid)%blocktype = cbk%GRD

            !> CSV format.
            case (2)
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.csv'
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRU

            !> Binary sequential format.
            case (3)
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.seq'
                open( &
                    cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', &
                    form = 'unformatted', access = 'sequential', iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRD

            !> ASCII format.
            case (4)
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.asc'
                open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRD

            !> CLASS 'MET' file.
            case (6)
                cm%dat(vid)%fname = 'basin_forcing'
                cm%dat(vid)%fpath = 'basin_forcing.met'
                cm%dat(vid)%blocktype = cbk%GRD
                if (vid == ck%MET) then
                    open(cm%dat(vid)%fiun, file = cm%dat(vid)%fpath, action = 'read', status = 'old', iostat = ierr)
                    if (ierr /= 0) goto 999
                end if

            !> Unknown file format.
            case default
                call print_error(trim(cm%dat(vid)%fname) // ' (' // trim(cm%dat(vid)%id_var) // '): Unsupported file format.')
                call program_abort()

        end select

        !> Allocate the block variable.
        if (allocated(cm%dat(vid)%blocks)) deallocate(cm%dat(vid)%blocks)
        select case (cm%dat(vid)%blocktype)
            case (1)

                !> Block type: GRD (Grid).
                allocate(cm%dat(vid)%blocks(shd%NA, cm%dat(vid)%nblocks), stat = ierr)
            case (2)

                !> Block type: GRU.
                allocate(cm%dat(vid)%blocks(shd%lc%NTYPE, cm%dat(vid)%nblocks), stat = ierr)
            case (3)

                !> Block type: GAT (Land element).
                allocate(cm%dat(vid)%blocks(shd%lc%NML, cm%dat(vid)%nblocks), stat = ierr)
        end select
        if (ierr /= 0) goto 997

        !> Flag that the file has been opened.
        cm%dat(vid)%fopen = .true.

        return

999     call print_error('Unable to open ' // trim(cm%dat(vid)%fpath) // ' or file not found.')
        call program_abort()

998     call print_error('Unable to read ' // trim(cm%dat(vid)%fpath) // ' or end of file.')
        call program_abort()

997     call print_error('Unable to allocate blocks for reading ' // trim(cm%dat(vid)%fpath) // ' data into memory.')
        call program_abort()

    end function

    !> Description:
    !>  Load data for the climate forcing variable from file.
    !>
    !> Input/output variables:
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and its unit.
    !*  vid: Index of the climate forcing variable.
    !*  iskip: Number of records to skip (default: 0).
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error reading from the file.
    logical function load_data(shd, cm, vid, iskip) result(ENDDATA)

        !> 'shd_variables': For 'shd' variable.
        use shd_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: vid, iskip

        !> Input/Output variables.
        type(clim_info) cm

        !> Local variables.
        integer t, n, j, i
        real GRD(shd%yCount, shd%xCount), MET(9)
        character(len = DEFAULT_LINE_LENGTH) line

        !> Initialize the return variable.
        ENDDATA = .false.

        !> Return if the file is not open or if it is not time to read new blocks.
        if (.not. cm%dat(vid)%fopen .or. cm%dat(vid)%iblock /= 1) return

        !> Set 't' to the number of blocks to read or skip.
        if (iskip > 0) then
            n = iskip
        else if (cm%dat(vid)%nblocks > 1) then
            n = cm%dat(vid)%nblocks
        else
            n = 1
        end if

        !> Reset the blocks to the NO_DATA value.
        cm%dat(vid)%blocks = NO_DATA

        !> Read data according to the format of the file.
        select case (cm%dat(vid)%ffmt)

            !> ASCII R2C format.
            case (1)
                do t = 1, n
                    read(cm%dat(vid)%fiun, *, end = 999) !':Frame'
                    read(cm%dat(vid)%fiun, *, end = 999) ((GRD(i, j), j = 1, shd%xCount), i = 1, shd%yCount)
                    read(cm%dat(vid)%fiun, *, end = 999) !':EndFrame'
                    if (iskip == 0) then
                        do i = 1, shd%NA
                            cm%dat(vid)%blocks(i, t) = GRD(shd%yyy(i), shd%xxx(i))
                        end do
                    end if
                end do

            !> CSV format.
            case (2)
                do t = 1, n
                    if (iskip == 0) then
                        read(cm%dat(vid)%fiun, *, end = 999) (cm%dat(vid)%blocks(j, t), j = 1, shd%lc%NTYPE)
                    else
                        read(cm%dat(vid)%fiun, *, end = 999)
                    end if
                end do

            !> Binary sequential format.
            case (3)
                do t = 1, n
                    if (iskip == 0) then
                        read(cm%dat(vid)%fiun, end = 999) !NTIME
                        read(cm%dat(vid)%fiun, end = 999) (cm%dat(vid)%blocks(i, t), i = 1, shd%NA)
                    else
                        read(cm%dat(vid)%fiun, end = 999)
                        read(cm%dat(vid)%fiun, end = 999)
                    end if
                end do

            !> ASCII format.
            case (4)
                do t = 1, n
                    if (iskip == 0) then
                        read(cm%dat(vid)%fiun, *, end = 999) (cm%dat(vid)%blocks(i, t), i = 1, shd%NA)
                    else
                        read(cm%dat(vid)%fiun, *, end = 999)
                    end if
                end do

            !> CLASS format MET file.
            case (6)
                do t = 1, n
                    if (iskip == 0) then

                        !> Read from the 'MET' file (to a generic array).
                        if (vid == ck%RR) then
                            read(cm%dat(ck%MET)%fiun, *, end = 999) i, i, i, i, MET(1:9)
                            cm%dat(ck%RR)%blocks(:, t) = MET(8)
                        else if (vid == ck%SR) then
                            read(cm%dat(ck%MET)%fiun, *, end = 999) i, i, i, i, MET(1:9)
                            cm%dat(ck%SR)%blocks(:, t) = MET(9)
                        else
                            read(cm%dat(ck%MET)%fiun, *, end = 999) i, i, i, i, MET(1:7)
                        end if

                        !> Backspace the record as other variables read independently.
                        backspace(cm%dat(ck%MET)%fiun)

                        !> Assign the appropriate field to the variable.
                        if (vid == ck%FB) then
                            cm%dat(ck%FB)%blocks(:, t) = MET(1)
                        else if (vid == ck%FI) then
                            cm%dat(ck%FI)%blocks(:, t) = MET(2)
                        else if (vid == ck%RT) then
                            cm%dat(ck%RT)%blocks(:, t) = MET(3)
                        else if (vid == ck%TT) then
                            cm%dat(ck%TT)%blocks(:, t) = MET(4) + 273.16
                        else if (vid == ck%HU) then
                            cm%dat(ck%HU)%blocks(:, t) = MET(5)
                        else if (vid == ck%UV) then
                            cm%dat(ck%UV)%blocks(:, t) = MET(6)
                        else if (vid == ck%P0) then
                            cm%dat(ck%P0)%blocks(:, t) = MET(7)
                        else if (vid == ck%MET) then

                            !> Put something in the 'MET' field to pass the ENDDATA check.
                            cm%dat(vid)%blocks(1, t) = cm%dat(ck%TT)%blocks(1, t)
                        end if
                    else
                        if (vid == ck%MET) then
                            read(cm%dat(vid)%fiun, *, end = 999)
                        end if
                    end if
                end do

            !> Unknown file format.
            case default
                call print_error(trim(cm%dat(vid)%fname) // ' (' // trim(cm%dat(vid)%id_var) // '): Unsupported file format.')
                call program_abort()

        end select

        return

999     continue

    end function

    !> Description:
    !>  Load data for the climate forcing variable from file.
    !>
    !> Input/output variables:
    !*  shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !*  cm: Climate forcing object. Contains the file name, format, and its unit.
    !*  vid: Index of the climate forcing variable.
    !*  iskip: Number of records to skip (default: 0).
    !>
    !> Returns:
    !*  ENDDATA: Returns .true. if there was an error updating the climate input forcing data.
    logical function update_data(shd, cm, vid, iskip) result(ENDDATA)

        !> 'shd_variables': For 'shd' variable.
        use shd_variables

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: vid, iskip

        !> Input/Output variables.
        type(clim_info) cm

        !> Initialize the return variable.
        ENDDATA = .false.

        !> Return if the file is not open.
        if (.not. cm%dat(vid)%fopen) return

        !> Read data (returns if the current block has already been read to memory).
        ENDDATA = load_data(shd, cm, vid, iskip)

        !> Return an error if the block contains the NO_DATA value.
        if (any(cm%dat(vid)%blocks(:, cm%dat(vid)%iblock) == NO_DATA) .and. iskip == 0) goto 999

        return

999     ENDDATA = .true.

    end function

end module
