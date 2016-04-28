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
    function open_data(shd, vid, cm) result(ENDDATA)

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
    function load_data(shd, skip_data, vid, cm) result(ENDDATA)

        !> Required for 'shd' variable.
        use sa_mesh_shared_variabletypes

        !> Required for 'ro' run options for VERBOSEMODE.
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

!-    subroutine READ_FORCING_DATA(shd, cm, iilen, ii1, ii2, &
!                                 FSDOWN, &
!-                                 FSVHGRD, FSIHGRD, &
!                                 FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
!-                                 FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, &
!-                                 ENDDATA)

    !> *****************************************************************
    !> Read in Meteorological forcing data
    !> *****************************************************************
    !>
    !> *****************************************************************
!-    !> THESE HAVE TO BE REAL*4 IN ORDER TO READ IN THE MET DATA
!-    !> CORRECTLY.
!-    !>
    !* R4SHRTGRID2D: VISIBLE SHORTWAVE RADIATION [W m-2]
    !* R4LONGGRID2D: DOWNWELLING LONGWAVE RADIATION [W m-2]
    !* R4RAINGRID2D: PRECIPITATION [kg m-2 s-1]
    !* R4TEMPGRID2D: AMBIENT AIR TEMPERATURE [K]
    !* R4WINDGRID2D: WIND SPEED AT REFERENCE HEIGHT [m s-1]
    !* R4PRESGRID2D: AIR PRESSURE AT SURFACE [Pa]
    !* R4HUMDGRID2D: SPECIFIC HUMIDITY AT REFERENCE HEIGHT [kg kg-1]
    !> *****************************************************************

!-        use sa_mesh_shared_variabletypes

        !> Input variables.
!-        type(ShedGridParams), intent(in) :: shd
!-        integer, intent(in) :: iilen, ii1, ii2

        !> Input/Output variables.
!-        type(clim_info) :: cm

        !> Output variables.
!-        real, dimension(shd%NA) :: FSVHGRD, FSIHGRD
!            FSDOWN, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD
!-        real, dimension(iilen) :: FSGAT, FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT
!-        logical ENDDATA

        !> Local variables.
!-        logical need_update

        !> For each forcing variable, a call is made to update_data
        !> to load new data from memory or file (if necessary).
        !> If new data are loaded then a call to SCATTER distributes
        !> these data from the GAT to GRD array.

        !> *************************************************************
        !> Read shortwave radiation data
        !> *************************************************************

!-        call update_data(shd, cm, cfk%FB, iilen, ii1, ii2, FSGAT, need_update, ENDDATA)
!-        if (need_update) then
!-            FSVHGAT = 0.5*FSGAT
!-            FSIHGAT = FSVHGAT
!-            call SCATTER(shd, FSGAT, iilen, ii1, ii2, cm%clin(cfk%FB)%GRD)
!-            FSVHGRD = 0.5*cm%clin(cfk%FB)%GRD
!-            FSIHGRD = FSVHGRD
!-        end if

        !> *************************************************************
        !> Read longwave radiation data
        !> *************************************************************

!-        call update_data(shd, cm, cfk%FI, iilen, ii1, ii2, FDLGAT, need_update, ENDDATA)
!-        if (need_update) then
!-            call SCATTER(shd, FDLGAT, iilen, ii1, ii2, cm%clin(cfk%FI)%GRD)
!-        end if

        !> *************************************************************
        !> Read precipitation data
        !> *************************************************************

!-        call update_data(shd, cm, cfk%PR, iilen, ii1, ii2, PREGAT, need_update, ENDDATA)
!-        if (need_update) then
!-            call SCATTER(shd, PREGAT, iilen, ii1, ii2, cm%clin(cfk%PR)%GRD)
!-        end if

                !> Read from two sources of rainfall input.
!todo: re-instate with alpha
!                case (6)
!                    call NeedUpdate_clim_data(shd, cfk%PR, cm, iilen, ii1, ii2, ENDDATA)
!                    call NeedUpdate_clim_data(shd, 8, cm, iilen, ii1, ii2, ENDDATA)
!                    PREGRD = cm%clin(8)%alpharain*cm%clin(cfk%PR)%climv(:, cm%clin(cfk%PR)%itime) + &
!                             (1.0 - cm%clin(8)%alpharain)*cm%clin(8)%climv(:, cm%clin(8)%itime)
!                    cm%clin(cfk%PR)%itime = cm%clin(cfk%PR)%itime + 1
!                    if (cm%clin(cfk%PR)%itime > size(cm%clin(cfk%PR)%climv, 3)) cm%clin(cfk%PR)%itime = 1
!                    cm%clin(8)%itime = cm%clin(8)%itime + 1
!                    if (cm%clin(8)%itime > size(cm%clin(8)%climv, 3)) cm%clin(8)%itime = 1
!                    call GATHER(shd, ii1, ii2, iilen, PREGRD, PREGAT)
!                    ICOUNT = ICOUNT + 1

        !> *************************************************************
        !> Read temperature data
        !> *************************************************************

!-        call update_data(shd, cm, cfk%TT, iilen, ii1, ii2, TAGAT, need_update, ENDDATA)
!-        if (need_update) then
!-            call SCATTER(shd, TAGAT, iilen, ii1, ii2, cm%clin(cfk%TT)%GRD)
!-        end if

        !> *************************************************************
        !> Read wind data
        !> *************************************************************

!-        call update_data(shd, cm, cfk%UV, iilen, ii1, ii2, ULGAT, need_update, ENDDATA)
!-        if (need_update) then
!-            call SCATTER(shd, ULGAT, iilen, ii1, ii2, cm%clin(cfk%UV)%GRD)
!-        end if

        !> *************************************************************
        !> Read pressure data
        !> *************************************************************

!-        call update_data(shd, cm, cfk%P0, iilen, ii1, ii2, PRESGAT, need_update, ENDDATA)
!-        if (need_update) then
!-            call SCATTER(shd, PRESGAT, iilen, ii1, ii2, cm%clin(cfk%P0)%GRD)
!-        end if

        !> *************************************************************
        !> Read humidity data
        !> *************************************************************

!-        call update_data(shd, cm, cfk%HU, iilen, ii1, ii2, QAGAT, need_update, ENDDATA)
!-        if (need_update) then
!-            call SCATTER(shd, QAGAT, iilen, ii1, ii2, cm%clin(cfk%HU)%GRD)
!-        end if

!644 format(/1x'The input forcing file format is not supported', &
!        /2x, A15, I4/)

!-        return

!-999     ENDDATA = .true.

!-    end subroutine !READ_FORCING_DATA

!-    subroutine SKIP_FORCING_DATA(shd, cm, iilen, ii1, ii2, ENDDATA)

!-        use sa_mesh_shared_variabletypes

        !> Input variables.
!-        type(ShedGridParams), intent(in) :: shd
!-        integer, intent(in) :: iilen, ii1, ii2

        !> Input/Output variables.
!-        type(clim_info) :: cm

        !> Output variables.
!-        logical ENDDATA

        !> Local variables.
!-        integer i

        !> Call skip data for each of the climate forcing variables.
        !> Variables have to be called individually in case they use
        !> different time-stepping.
!-        do i = 1, size(cm%clin)
!-            call skip_data(shd, cm, i, iilen, ii1, ii2, ENDDATA)
!-        end do

!-    end subroutine

    !> -----------------------------------------------------------------
    !> Description: Check if we need to load data again if that, we
    !> deallocate and allocate again and then we load data
    !> -----------------------------------------------------------------
!-    subroutine NeedUpdate_clim_data(shd, indx, cm, iilen, ii1, ii2, ENDDATA, skipdata)

!-        use sa_mesh_shared_variabletypes

        !> Inputs variables.
!-        type(ShedGridParams) :: shd
!-        integer indx, iilen, ii1, ii2
!-        logical, optional :: skipdata

        !> Input/Output variables.
!-        type(clim_info) :: cm

        !> Ouput variables.
!-        logical ENDDATA

        !> Local variables.
!-        logical :: storedata = .true.

        !> If skipdata is present and .true., the routine won't store
        !> the data.
!-        if (present(skipdata)) storedata = .not. skipdata

        !> Check if we need to update.
!-        if (cm%clin(indx)%itime == 1) then
!-            if (storedata) then
!-                if (allocated(cm%clin(indx)%climv)) deallocate(cm%clin(indx)%climv)
!                allocate(cm%clin(indx)%climv(iilen, cm%clin(indx)%nSeries, cm%clin(indx)%ntimes(cm%clin(indx)%readIndx)))
!todo: replace nseries with nfiles
!-                allocate(cm%clin(indx)%climv(iilen, 1, cm%clin(indx)%ntimes(cm%clin(indx)%readIndx)))
!-            end if
!-            call LoadData(shd, indx, cm, iilen, ii1, ii2, ENDDATA, .not. storedata)
!-        end if

!-    end subroutine !NeedUpdate_clim_data

!-    subroutine update_data(shd, cm, indx, iilen, ii1, ii2, gat, need_update, end_data)

!-        use strings
!-        use sa_mesh_shared_variabletypes

!-        type(ShedGridParams), intent(in) :: shd
!-        type(clim_info) :: cm
!-        integer, intent(in) :: indx, iilen, ii1, ii2
!-        real, dimension(:) :: gat
!-        logical, intent(out) :: need_update, end_data

!-        integer s, k, j, ios
!-        real a

        !> Read data on a new time-step.
!-        if (cm%clin(indx)%timestep_now == 0) then

            !> Read data (if needed).
!-            call NeedUpdate_clim_data(shd, indx, cm, iilen, ii1, ii2, end_data)

            !> Extract data from the climate variable.
!-            gat = cm%clin(indx)%climv(:, 1, cm%clin(indx)%itime)

            !> Apply conditions to the series of data is such conditions exist.
!-            if (cm%clin(indx)%nseries > 0) then
!-                do s = 1, cm%clin(indx)%nseries
!-                    select case (cm%clin(indx)%series(s)%attrtype)
!-                        case ('gru')
!-                            call value(cm%clin(indx)%series(s)%attr(1), j, ios)
!-                            call value(cm%clin(indx)%series(s)%attr(2), a, ios)
!-                            forall (k = ii1:ii2, shd%lc%JLMOS(k) == j) gat(k) = gat(k)*a
!-                    end select
!-                end do
!-            end if

            !> Update the counter of the current time-step.
!-            cm%clin(indx)%itime = cm%clin(indx)%itime + 1
!-            if (cm%clin(indx)%itime > size(cm%clin(indx)%climv, 3)) then
!-                cm%clin(indx)%itime = 1
!-            end if

            !> Set need_update to .true.
!-            need_update = .true.

!-        else

            !> If need_update is .false. then READ_FORCING_DATA does
            !> not re-distribute the data.
!-            need_update = .false.

!-        end if

!-    end subroutine

!-    subroutine skip_data(shd, cm, indx, iilen, ii1, ii2, end_data)

!-        use sa_mesh_shared_variabletypes

!-        type(ShedGridParams), intent(in) :: shd
!-        type(clim_info) :: cm
!-        integer, intent(in) :: indx, iilen, ii1, ii2
!-        logical, intent(out) :: end_data

        !> Read data on a new time-step.
!-        if (cm%clin(indx)%timestep_now == 0) then

            !> Read data to skip (if needed).
!-            call NeedUpdate_clim_data(shd, indx, cm, iilen, ii1, ii2, end_data)

            !> Update the counter of the current time-step.
!-            cm%clin(indx)%itime = cm%clin(indx)%itime + 1
!-            if (cm%clin(indx)%itime > size(cm%clin(indx)%climv, 3)) then
!-                cm%clin(indx)%itime = 1
!-            end if

!-        end if

!-    end subroutine

    !>
    !> Description: Load data for the climate forcing variable from file.
    !>
    !> Inputs:
    !>  - shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !>  - cm: Climate forcing object. Contains the file name, format, and its unit.
    !>  - vid: Index of the climate forcing variable.
    !>  - iilen:
    !>  - ii1
    !>  - ii2
    !>  - skip_data: .true. to skip data; .false. to store data.
    !>
    !> Outputs:
    !>  - ENDDATA: Returns .true. if there was an error reading from the file.
    !>
    function update_data(shd, ii1, ii2, skip_data, vid, cm) result(ENDDATA)

        !> Required for 'shd' variable.
        use sa_mesh_shared_variabletypes

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
        if (load_data(shd, .not. storedata, vid, cm)) goto 999

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
