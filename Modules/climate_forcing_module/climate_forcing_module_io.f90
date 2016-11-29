!>
!> Description: Module to read climate forcing data from file.
!>
module climate_forcing_io

    use climate_forcing_constants
    use climate_forcing_variabletypes

    implicit none

    contains

    !>
    !> Description: Open the climate forcing input file.
    !>
    !> Inputs:
    !>  - shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !>  - cm: Climate forcing object. Contains the file name, format, and its unit.
    !>  - vid: Index of the climate forcing variable.
    !>
    !> Outputs:
    !>  - ENDDATA: Returns .true. if there was an error opening the file.
    !>
    function open_data(shd, cm, vid) result(ENDDATA)

        use sa_mesh_shared_variables

        !> Input variables.
        type(ShedGridParams) :: shd
        integer vid

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer ierr
        character(10) end_of_r2c_header

        ENDDATA = .false.

        !> Return if the variable is not marked active.
        if (.not. cm%dat(vid)%factive) return

        !> Open file depending on the format type of the climate data.
        select case (cm%dat(vid)%ffmt)

            !> ASCII R2C format.
            case (1)

                !> Open the file.
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.r2c'
                open(cm%dat(vid)%fiun, &
                     file = trim(adjustl(cm%dat(vid)%fpath)), &
                     status = 'old', &
                     action = 'read', &
                     form = 'formatted', &
                     iostat = ierr)

                !> Return on an error.
                if (ierr /= 0) goto 999

                !> Skip the header of the 'r2c' format file.
                end_of_r2c_header = ''
                do while (end_of_r2c_header /= ':endHeader')
                    read(cm%dat(vid)%fiun, '(a10)', end = 998) end_of_r2c_header
                end do

                !> Set the block type.
                cm%dat(vid)%blocktype = cbk%GRD

            !> CSV format.
            case (2)
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.csv'
                open(cm%dat(vid)%fiun, &
                     file = trim(adjustl(cm%dat(vid)%fpath)), &
                     status = 'old', &
                     action = 'read', &
                     form = 'formatted', &
                     iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRU

            !> Binary sequential format.
            case (3)
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.seq'
                open(cm%dat(vid)%fiun, &
                     file = trim(adjustl(cm%dat(vid)%fpath)), &
                     status = 'old', &
                     action = 'read', &
                     form = 'unformatted', &
                     access = 'sequential', &
                     iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRD

            !> ASCII format.
            case (4)
                cm%dat(vid)%fpath = trim(adjustl(cm%dat(vid)%fname)) // '.asc'
                open(cm%dat(vid)%fiun, &
                     file = trim(adjustl(cm%dat(vid)%fpath)), &
                     status = 'old', &
                     action = 'read', &
                     form = 'formatted', &
                     iostat = ierr)
                if (ierr /= 0) goto 999
                cm%dat(vid)%blocktype = cbk%GRD

            !> Unknown file format.
            case default
                if (ro%VERBOSEMODE > 0) print 198, cm%dat(vid)%id_var, cm%dat(vid)%ffmt
                stop

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
        if (ro%VERBOSEMODE > 0) print 199, trim(adjustl(cm%dat(vid)%fpath))
        cm%dat(vid)%fopen = .true.

        return

699     format(//1x, (a), ' not found.', &
               /1x, 'Please adjust the MESH_input_run_options.ini file', &
               /1x, 'or put the file in the correct location.', /)

698     format(//1x, 'An error occurred reading the file ', (a), /)

697     format(//1x, 'An error occurred allocating variables for the climate variable ', (a), /)

199     format(1x, (a), ' found.')

198     format(//1x, 'The input forcing file format is not supported', &
               /2x, (a), i4/)

999     if (ro%VERBOSEMODE > 0) print 699, trim(adjustl(cm%dat(vid)%fpath))
        ENDDATA = .true.
        stop

998     if (ro%VERBOSEMODE > 0) print 698, trim(adjustl(cm%dat(vid)%fpath))
        ENDDATA = .true.
        stop

997     if (ro%VERBOSEMODE > 0) print 697, trim(adjustl(cm%dat(vid)%id_var))
        stop

    end function !open_data

    !>
    !> Description: Load data for the climate forcing variable from file.
    !>
    !> Inputs:
    !>  - shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !>  - cm: Climate forcing object. Contains the file name, format, and its unit.
    !>  - vid: Index of the climate forcing variable.
    !>  - skip_data: .true. to skip data; .false. to store data.
    !>
    !> Outputs:
    !>  - ENDDATA: Returns .true. if there was an error reading from the file.
    !>
    function load_data(shd, cm, vid, skip_data) result(ENDDATA)

        !> For: 'ShedGridParams' type, 'ro' run options for VERBOSEMODE.
        use sa_mesh_shared_variables

        !> Input variables.
        type(ShedGridParams) :: shd
        integer vid
        logical skip_data

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer ierr, t, j, i
        real inr2c(shd%yCount, shd%xCount)
        logical :: storedata = .true.

        !> Return if the file is not open or if it is not time to read new blocks.
        if (.not. cm%dat(vid)%fopen .or. cm%dat(vid)%iblock > 1) return

        !> Store data is 'skip_data' is not .true..
        storedata = .not. skip_data

        ENDDATA = .false.

        !> Reset the blocks.
        if (storedata) cm%dat(vid)%blocks = 0.0

        !> The outer loop is the number of time-steps read into memory at once.
        do t = 1, cm%dat(vid)%nblocks

            !> Read data according to the format of the file.
            select case (cm%dat(vid)%ffmt)

                !> ASCII R2C format.
                case (1)
                    read(cm%dat(vid)%fiun, *, end = 999) !':Frame'
                    read(cm%dat(vid)%fiun, *, end = 999) ((inr2c(i, j), j = 1, shd%xCount), i = 1, shd%yCount)
                    read(cm%dat(vid)%fiun, *, end = 999) !':EndFrame'
                    if (storedata) then
                        do i = 1, shd%NA
                            cm%dat(vid)%blocks(i, t) = inr2c(shd%yyy(i), shd%xxx(i))
                        end do
                    end if

                !> CSV format.
                case (2)
                    if (storedata) then
                        read(cm%dat(vid)%fiun, *, end = 999) (cm%dat(vid)%blocks(j, t), j = 1, shd%lc%NTYPE)
                    else
                        read(cm%dat(vid)%fiun, *, end = 999)
                    end if

                !> Binary sequential format.
                case (3)
                    if (storedata) then
                        read(cm%dat(vid)%fiun, end = 999) !NTIME
                        read(cm%dat(vid)%fiun, end = 999) (cm%dat(vid)%blocks(i, t), i = 1, shd%NA)
                    else
                        read(cm%dat(vid)%fiun, end = 999)
                        read(cm%dat(vid)%fiun, end = 999)
                    end if

                !> ASCII format.
                case (4)
                    read(cm%dat(vid)%fiun, *, end = 999) (cm%dat(vid)%blocks(i, t), i = 1, shd%NA)

                !> Unknown file format.
                case default
                    if (ro%VERBOSEMODE > 0) print 199, cm%dat(vid)%id_var, cm%dat(vid)%ffmt
                    stop

            end select
        end do

        return

199     format(//1x, 'The input forcing file format is not supported', &
               /2x, (a), i4, /)

999     ENDDATA = .true.

    end function !load_data

    !>
    !> Description: Load data for the climate forcing variable from file.
    !>
    !> Inputs:
    !>  - shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !>  - cm: Climate forcing object. Contains the file name, format, and its unit.
    !>  - vid: Index of the climate forcing variable.
    !>  - skip_data: .true. to skip data; .false. to store data.
    !>
    !> Outputs:
    !>  - ENDDATA: Returns .true. if there was an error updating the climate input forcing data.
    !>
    function update_data(shd, cm, vid, skip_data) result(ENDDATA)

        !> For: 'ShedGridParams' type.
        use sa_mesh_shared_variables

        !> Input variables.
        type(ShedGridParams) :: shd
        integer ii1, ii2, vid
        logical skip_data

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Ouput variables.
        logical ENDDATA

        !> Local variables.
        logical :: storedata = .true.

        !> Return if the file is not open or if it is not time to read new blocks.
        if (.not. cm%dat(vid)%fopen .or. cm%dat(vid)%iblock > 1) return

        !> Store data is 'skip_data' is not .true..
        storedata = .not. skip_data

        ENDDATA = .false.

        !> Read data (if needed).
        if (load_data(shd, cm, vid, .not. storedata)) goto 999

        !> Update the counter of the current time-step.
        if (cm%dat(vid)%nblocks > 1) then
            cm%dat(vid)%iblock = cm%dat(vid)%iblock + 1
            if (cm%dat(vid)%iblock > cm%dat(vid)%nblocks) then
                cm%dat(vid)%iblock = 1
            end if
        end if

        return

999     ENDDATA = .true.

    end function !update_data

end module
