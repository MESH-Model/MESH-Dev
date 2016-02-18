module climate_forcing_data

    use climate_forcing_variabletypes
    use climate_forcing_variables

    implicit none

    contains

    !> -----------------------------------------------------------------
    !> Description: Open Units and Load the first block of climate data
    !> -----------------------------------------------------------------
    subroutine OpenData(indx, cm)

        use sa_mesh_shared_variables

        !> Input variables.
        integer, intent(in) :: indx

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Local variables.
        integer ios
        character*80 end_of_r2c_header

        !> Open file depending on the format type of the climate data.
        select case (cm%clin(indx)%filefmt)

            !> ASCII R2C format.
            case (1)
                open(unit = cm%clin(indx)%unitR, &
                     file = trim(adjustl(cm%clin(indx)%name(1))) // '.r2c', &
                     action = 'read', &
                     status = 'old', &
                     form = 'formatted', &
                     iostat = ios)
                if (ios /= 0) then
                    if (ro%VERBOSEMODE > 0) print 670, trim(adjustl(cm%clin(indx)%name(1))) // '.r2c'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    if (ro%VERBOSEMODE > 0) print 676, trim(adjustl(cm%clin(indx)%name(1))) // '.r2c'
                    end_of_r2c_header = ''
                    do while (end_of_r2c_header /= ":endHeader")
                        read(cm%clin(indx)%unitR, '(A10)') end_of_r2c_header
                    end do
                    cm%clin(indx)%openFl = .true.
                end if

            !> CSV format.
            case (2)
                open(unit = cm%clin(indx)%unitR, &
                     file = trim(adjustl(cm%clin(indx)%name(1))) // '.csv', &
                     action = 'read', &
                     status = 'old', &
                     form = 'formatted', &
                     iostat = ios)
                if (ios /=0 ) then
                    if (ro%VERBOSEMODE > 0) print 670, trim(adjustl(cm%clin(indx)%name(1))) // '.csv'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    if (ro%VERBOSEMODE > 0) print 676, trim(adjustl(cm%clin(indx)%name(1))) // '.csv'
                    cm%clin(indx)%openFl = .true.
                end if

            !> Binary sequential format.
            case (3)
                open(UNIT = cm%clin(indx)%unitR, &
                     FILE = trim(adjustl(cm%clin(indx)%name(1))) // '.seq', &
                     action = 'read', &
                     status = 'old', &
                     form = 'unformatted', &
                     access = 'sequential', &
                     iostat = ios)
                if (ios /= 0) then
                    if (ro%VERBOSEMODE > 0) print 670, trim(adjustl(cm%clin(indx)%name(1))) // '.seq'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    if (ro%VERBOSEMODE > 0) print 676, trim(adjustl(cm%clin(indx)%name(1))) // '.seq'
                    cm%clin(indx)%openFl = .true.
                end if

            !> ASCII format.
            case (4)
                open(cm%clin(indx)%unitR, &
                     file = trim(adjustl(cm%clin(indx)%name(1))) // '.asc', &
                     action = 'read', &
                     status = 'old', &
                     form = 'formatted', &
                     iostat = ios)
                if (ios /= 0) then
                    if (ro%VERBOSEMODE > 0) print 670, trim(adjustl(cm%clin(indx)%name(1))) // '.asc'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    if (ro%VERBOSEMODE > 0) print 676, trim(adjustl(cm%clin(indx)%name(1))) // '.asc'
                    cm%clin(indx)%openFl = .true.
                end if

            case default
                if (ro%VERBOSEMODE > 0) print 644, cm%clin(indx)%id_var, cm%clin(indx)%filefmt
                stop
        end select

670 format(/ &
        /1x, (a), ' not found.', &
        /1x, 'Please adjust the MESH_input_run_options.ini file', &
        /1x, 'or put the file in the correct location.', /)

676 format(1x, (a), ' found.')

644 format(/ &
        /1x, 'The input forcing file format is not supported', &
        /2x, (a), i4/)

    end subroutine !OpenData

    !> -----------------------------------------------------------------
    !> Description: Load block of data
    !> -----------------------------------------------------------------
    subroutine LoadData(shd, indx, cm, iilen, ii1, ii2, ENDDATA, skipdata)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables

        !> Input variables.
        type(ShedGridParams) :: shd
        integer indx, iilen, ii1, ii2
        logical, optional :: skipdata

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer NTIME, IOS
        logical file_stat
        integer t, s, i, j, k, ii, jj
        real INR2C(shd%yCount, shd%xCount), INGRU(shd%lc%NTYPE), INGRD(shd%NA)
        logical :: storedata = .true.

        !> If skipdata is present and .true., the routine won't store
        !> the data.
        if (present(skipdata)) storedata = .not. skipdata

        select case (cm%clin(indx)%filefmt)

            !> ASCII R2C format.
            case (1)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR, *, end = 999) !:Frame line
                        do ii = 1, shd%yCount
                            if (storedata) then
                                read(cm%clin(indx)%unitR, *, end = 999) (INR2C(ii, jj), jj = 1, shd%xCount)
                            else
                                read(cm%clin(indx)%unitR, *, end = 999)
                            end if
                        end do
                        read(cm%clin(indx)%unitR, *, end = 999) !:EndFrame line
                        if (storedata) then
                            do k = ii1, ii2
                                cm%clin(indx)%climv(k, s, t) = INR2C(shd%yyy(shd%lc%ILMOS(k)), shd%xxx(shd%lc%ILMOS(k)))
                            end do
                        end if
                    end do
                end do

            !> CSV format.
            case (2)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        if (storedata) then
                            read(cm%clin(indx)%unitR, *, end = 999) (INGRU(j), j = 1, shd%lc%NTYPE)
                            do k = ii1, ii2
                                cm%clin(indx)%climv(k, s, t) = INGRU(shd%lc%JLMOS(k))
                            end do
                        else
                            read(cm%clin(indx)%unitR, *, end = 999)
                        end if
                    end do
                end do

            !> Binary sequential format.
            case (3)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        if (storedata) then
                            read(cm%clin(indx)%unitR, end = 999) NTIME
                            read(cm%clin(indx)%unitR, end = 999) (INGRD(i), i = 1, shd%NA)
                            do k = ii1, ii2
                                cm%clin(indx)%climv(k, s, t) = INGRD(shd%lc%ILMOS(k))
                            end do
                        else
                            read(cm%clin(indx)%unitR, end = 999)
                            read(cm%clin(indx)%unitR, end = 999)
                        end if
                    end do
                end do

            !> ASCII format.
            case (4)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR, *, end = 999) (INGRD(i), i = 1, shd%NA)
                        do k = ii1, ii2
                            cm%clin(indx)%climv(k, s, t) = INGRD(shd%lc%ILMOS(k))
                        end do
                    end do
                end do

            case default
                if (ro%VERBOSEMODE > 0) print 644, cm%clin(indx)%id_var, cm%clin(indx)%filefmt
                stop
        end select

        return

999 ENDDATA = .true.

644 format(/ &
        /1x, 'The input forcing file format is not supported', &
        /2x, (a), i4/)

    end subroutine !LoadData

    subroutine READ_FORCING_DATA(shd, cm, iilen, ii1, ii2, &
!                                 FSDOWN, &
                                 FSVHGRD, FSIHGRD, &
!                                 FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                 FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, &
                                 ENDDATA)

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

        use sa_mesh_shared_variabletypes

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iilen, ii1, ii2

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        real, dimension(shd%NA) :: FSVHGRD, FSIHGRD
!            FSDOWN, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD
        real, dimension(iilen) :: FSGAT, FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT
        logical ENDDATA

        !> Local variables.
        logical need_update

        !> For each forcing variable, a call is made to update_data
        !> to load new data from memory or file (if necessary).
        !> If new data are loaded then a call to SCATTER distributes
        !> these data from the GAT to GRD array.

        !> *************************************************************
        !> Read shortwave radiation data
        !> *************************************************************

        call update_data(shd, cm, cfk%FB, iilen, ii1, ii2, FSGAT, need_update, ENDDATA)
        if (need_update) then
            FSVHGAT = 0.5*FSGAT
            FSIHGAT = FSVHGAT
            call SCATTER(shd, FSGAT, iilen, ii1, ii2, cm%clin(cfk%FB)%GRD)
            FSVHGRD = 0.5*cm%clin(cfk%FB)%GRD
            FSIHGRD = FSVHGRD
        end if

        !> *************************************************************
        !> Read longwave radiation data
        !> *************************************************************

        call update_data(shd, cm, cfk%FI, iilen, ii1, ii2, FDLGAT, need_update, ENDDATA)
        if (need_update) then
            call SCATTER(shd, FDLGAT, iilen, ii1, ii2, cm%clin(cfk%FI)%GRD)
        end if

        !> *************************************************************
        !> Read precipitation data
        !> *************************************************************

        call update_data(shd, cm, cfk%PR, iilen, ii1, ii2, PREGAT, need_update, ENDDATA)
        if (need_update) then
            call SCATTER(shd, PREGAT, iilen, ii1, ii2, cm%clin(cfk%PR)%GRD)
        end if

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

        call update_data(shd, cm, cfk%TT, iilen, ii1, ii2, TAGAT, need_update, ENDDATA)
        if (need_update) then
            call SCATTER(shd, TAGAT, iilen, ii1, ii2, cm%clin(cfk%TT)%GRD)
        end if

        !> *************************************************************
        !> Read wind data
        !> *************************************************************

        call update_data(shd, cm, cfk%UV, iilen, ii1, ii2, ULGAT, need_update, ENDDATA)
        if (need_update) then
            call SCATTER(shd, ULGAT, iilen, ii1, ii2, cm%clin(cfk%UV)%GRD)
        end if

        !> *************************************************************
        !> Read pressure data
        !> *************************************************************

        call update_data(shd, cm, cfk%P0, iilen, ii1, ii2, PRESGAT, need_update, ENDDATA)
        if (need_update) then
            call SCATTER(shd, PRESGAT, iilen, ii1, ii2, cm%clin(cfk%P0)%GRD)
        end if

        !> *************************************************************
        !> Read humidity data
        !> *************************************************************

        call update_data(shd, cm, cfk%HU, iilen, ii1, ii2, QAGAT, need_update, ENDDATA)
        if (need_update) then
            call SCATTER(shd, QAGAT, iilen, ii1, ii2, cm%clin(cfk%HU)%GRD)
        end if

!644 format(/1x'The input forcing file format is not supported', &
!        /2x, A15, I4/)

        return

999     ENDDATA = .true.

    end subroutine !READ_FORCING_DATA

    subroutine SKIP_FORCING_DATA(shd, cm, iilen, ii1, ii2, ENDDATA)

        use sa_mesh_shared_variabletypes

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: iilen, ii1, ii2

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer i

        !> Call skip data for each of the climate forcing variables.
        !> Variables have to be called individually in case they use
        !> different time-stepping.
        do i = 1, size(cm%clin)
            call skip_data(shd, cm, i, iilen, ii1, ii2, ENDDATA)
        end do

    end subroutine

    !> -----------------------------------------------------------------
    !> Description: Check if we need to load data again if that, we
    !> deallocate and allocate again and then we load data
    !> -----------------------------------------------------------------
    subroutine NeedUpdate_clim_data(shd, indx, cm, iilen, ii1, ii2, ENDDATA, skipdata)

        use sa_mesh_shared_variabletypes

        !> Inputs variables.
        type(ShedGridParams) :: shd
        integer indx, iilen, ii1, ii2
        logical, optional :: skipdata

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Ouput variables.
        logical ENDDATA

        !> Local variables.
        logical :: storedata = .true.

        !> If skipdata is present and .true., the routine won't store
        !> the data.
        if (present(skipdata)) storedata = .not. skipdata

        !> Check if we need to update.
        if (cm%clin(indx)%itime == 1) then
            if (storedata) then
                if (allocated(cm%clin(indx)%climv)) deallocate(cm%clin(indx)%climv)
!                allocate(cm%clin(indx)%climv(iilen, cm%clin(indx)%nSeries, cm%clin(indx)%ntimes(cm%clin(indx)%readIndx)))
!todo: replace nseries with nfiles
                allocate(cm%clin(indx)%climv(iilen, 1, cm%clin(indx)%ntimes(cm%clin(indx)%readIndx)))
            end if
            call LoadData(shd, indx, cm, iilen, ii1, ii2, ENDDATA, .not. storedata)
        end if

    end subroutine !NeedUpdate_clim_data

    subroutine update_data(shd, cm, indx, iilen, ii1, ii2, gat, need_update, end_data)

        use strings
        use sa_mesh_shared_variabletypes

        type(ShedGridParams), intent(in) :: shd
        type(clim_info) :: cm
        integer, intent(in) :: indx, iilen, ii1, ii2
        real, dimension(:) :: gat
        logical, intent(out) :: need_update, end_data

        integer s, k, j, ios
        real a

        !> Read data on a new time-step.
        if (cm%clin(indx)%timestep_now == 0) then

            !> Read data (if needed).
            call NeedUpdate_clim_data(shd, indx, cm, iilen, ii1, ii2, end_data)

            !> Extract data from the climate variable.
            gat = cm%clin(indx)%climv(:, 1, cm%clin(indx)%itime)

            !> Apply conditions to the series of data is such conditions exist.
            if (cm%clin(indx)%nseries > 0) then
                do s = 1, cm%clin(indx)%nseries
                    select case (cm%clin(indx)%series(s)%attrtype)
                        case ('gru')
                            call value(cm%clin(indx)%series(s)%attr(1), j, ios)
                            call value(cm%clin(indx)%series(s)%attr(2), a, ios)
                            forall (k = ii1:ii2, shd%lc%JLMOS(k) == j) gat(k) = gat(k)*a
                    end select
                end do
            end if

            !> Update the counter of the current time-step.
            cm%clin(indx)%itime = cm%clin(indx)%itime + 1
            if (cm%clin(indx)%itime > size(cm%clin(indx)%climv, 3)) then
                cm%clin(indx)%itime = 1
            end if

            !> Set need_update to .true.
            need_update = .true.

        else

            !> If need_update is .false. then READ_FORCING_DATA does
            !> not re-distribute the data.
            need_update = .false.

        end if

    end subroutine

    subroutine skip_data(shd, cm, indx, iilen, ii1, ii2, end_data)

        use sa_mesh_shared_variabletypes

        type(ShedGridParams), intent(in) :: shd
        type(clim_info) :: cm
        integer, intent(in) :: indx, iilen, ii1, ii2
        logical, intent(out) :: end_data

        !> Read data on a new time-step.
        if (cm%clin(indx)%timestep_now == 0) then

            !> Read data to skip (if needed).
            call NeedUpdate_clim_data(shd, indx, cm, iilen, ii1, ii2, end_data)

            !> Update the counter of the current time-step.
            cm%clin(indx)%itime = cm%clin(indx)%itime + 1
            if (cm%clin(indx)%itime > size(cm%clin(indx)%climv, 3)) then
                cm%clin(indx)%itime = 1
            end if

        end if

    end subroutine

end module
