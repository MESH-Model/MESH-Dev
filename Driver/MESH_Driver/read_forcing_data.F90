module climate_forcing_data

!    use sa_mesh_shared_variabletypes
    use climate_forcing_variabletypes
    use climate_forcing_variables

    implicit none

    contains

    !> -----------------------------------------------------------------
    !> Description: Open Units and Load the first block of climate data
    !> -----------------------------------------------------------------
    subroutine OpenData(indx, cm)

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
                print *, cm%clin(indx)%unitR, trim(adjustl(cm%clin(indx)%name(1))) // '.r2c'
                open(unit = cm%clin(indx)%unitR, &
                     file = trim(adjustl(cm%clin(indx)%name(1))) // '.r2c', &
                     action = 'read', &
                     status = 'old', &
                     form = 'formatted', &
                     iostat = ios)
                if (ios /= 0) then
                    print 670, trim(adjustl(cm%clin(indx)%name(1))) // '.r2c'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    print 676, trim(adjustl(cm%clin(indx)%name(1))) // '.r2c'
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
                    print 670, trim(adjustl(cm%clin(indx)%name(1))) // '.csv'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    print 676, trim(adjustl(cm%clin(indx)%name(1))) // '.csv'
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
                    print 670, trim(adjustl(cm%clin(indx)%name(1))) // '.seq'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    print 676, trim(adjustl(cm%clin(indx)%name(1))) // '.seq'
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
                    print 670, trim(adjustl(cm%clin(indx)%name(1))) // '.asc'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    print 676, trim(adjustl(cm%clin(indx)%name(1))) // '.asc'
                    cm%clin(indx)%openFl = .true.
                end if

            case default
                print 644, cm%clin(indx)%id_var, cm%clin(indx)%filefmt
                stop
        end select

670 format(/ &
        /1x, a20, ' not found.', &
        /1x, 'Please adjust the MESH_input_run_options.ini file', &
        /1x, 'or put the file in the correct location.', /)

676 format(1x, a20, ' found.')

644 format(/ &
        /1x, 'The input forcing file format is not supported', &
        /2x, a15, i4/)

    end subroutine !OpenData

    !> -----------------------------------------------------------------
    !> Description: Load block of data
    !> -----------------------------------------------------------------
    subroutine LoadData(bi, indx, cm, ENDDATA)

        !> Input variables.
        type(basin_info) :: bi
        integer indx

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer NTIME
        integer t, s, i, j, k, ii, jj
        real INR2C(bi%YCOUNT, bi%XCOUNT), INGRU(bi%NTYPE), INGRD(bi%NA)

        select case (cm%clin(indx)%filefmt)

            !> ASCII R2C format.
            case (1)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR, *, end = 999) !:Frame line
                        do ii = 1, bi%YCOUNT
                            read(cm%clin(indx)%unitR, *, end = 999) (INR2C(ii, jj), jj = 1, bi%XCOUNT)
                        end do
                        read(cm%clin(indx)%unitR, *, end = 999) !:EndFrame line
                        do k = 1, bi%NML
                            cm%clin(indx)%climv(k, s, t) = INR2C(bi%YYY(bi%ILMOS(k)), bi%XXX(bi%ILMOS(k)))
                        end do
                    end do
                end do

            !> CSV format.
            case (2)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR, *, end = 999) (INGRU(j), j = 1, bi%NTYPE)
                        do k = 1, bi%NML
                            cm%clin(indx)%climv(k, s, t) = INGRU(bi%JLMOS(k))
                        end do
                    end do
                end do

            !> Binary sequential format.
            case (3)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR) NTIME
                        read(cm%clin(indx)%unitR, end = 999) (INGRD(i), i = 1, bi%NA)
                        do k = 1, bi%NML
                            cm%clin(indx)%climv(k, s, t) = INGRD(bi%ILMOS(k))
                        end do
                    end do
                end do

            !> ASCII format.
            case (4)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR, *, end = 999) (INGRD(i), i = 1, bi%NA)
                        do k = 1, bi%NML
                            cm%clin(indx)%climv(k, s, t) = INGRD(bi%ILMOS(k))
                        end do
                    end do
                end do

            case default
                print *, 'NOT IMPLEMENTED YET'
                stop
        end select

        return

999 ENDDATA = .true.

    end subroutine !LoadData

    subroutine READ_FORCING_DATA(bi, cm, &
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
    !> THESE HAVE TO BE REAL*4 IN ORDER TO READ IN THE MET DATA
    !> CORRECTLY.
    !>
    !* R4SHRTGRID2D: VISIBLE SHORTWAVE RADIATION [W m-2]
    !* R4LONGGRID2D: DOWNWELLING LONGWAVE RADIATION [W m-2]
    !* R4RAINGRID2D: PRECIPITATION [kg m-2 s-1]
    !* R4TEMPGRID2D: AMBIENT AIR TEMPERATURE [K]
    !* R4WINDGRID2D: WIND SPEED AT REFERENCE HEIGHT [m s-1]
    !* R4PRESGRID2D: AIR PRESSURE AT SURFACE [Pa]
    !* R4HUMDGRID2D: SPECIFIC HUMIDITY AT REFERENCE HEIGHT [kg kg-1]
    !> *****************************************************************

!    use strings
    use sa_mesh_shared_variabletypes
!    use FLAGS
!    use climate_forcing, only: clim_info, cfk, NeedUpdate_clim_data

        !> Input variables.
        type(basin_info), intent(in) :: bi

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        real*4, dimension(bi%NA) :: FSVHGRD, FSIHGRD
!            FSDOWN, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD
        real*4, dimension(bi%ILG) :: FSGAT, FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT
        logical ENDDATA

    !> Local variables.
!    real*4, dimension(bi%YCOUNT, bi%XCOUNT) :: INARRAY
!    real*4, dimension(bi%NTYPE) :: INVECTOR
!    real*4 JUNK
!    integer t, s, i, j, k, CURGRU, ICOUNT
!    integer s
        logical need_update

!    integer :: NTIME  ! time in read sequential

    !> Initialize counting number of r2c and csv files.
!    ICOUNT = 0

    !> *****************************************************************
    !> Read shortwave radiation data
    !> *****************************************************************
!    if (cm%clin(cfk%FB)%timestep_now == 0) then

        call update_data(bi, cm, cfk%FB, FSGAT, need_update, ENDDATA)

            !> Distribute the gridded variable.
!            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
        if (need_update) then
            FSVHGAT = 0.5*FSGAT
            FSIHGAT = FSVHGAT
            call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, cm%clin(cfk%FB)%climvGrd, FSGAT)
            FSVHGRD = 0.5*cm%clin(cfk%FB)%climvGrd
            FSIHGRD = FSVHGRD
        end if
!            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
!        else
!            select case (cm%clin(cfk%FB)%filefmt)

                !> Legacy binary format.
!                case (0)
!                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    do i = 1, bi%NA
!                        FSDOWN(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    FSVHGRD = 0.5*FSDOWN
!                    FSIHGRD = FSVHGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
!                    FSIHGAT = FSVHGAT

                !> ASCII R2C format.
!                case (1)
!                    read(cm%basefileunit + cfk%FB, *, end = 999) !:Frame line
!                    do i = 1, bi%YCOUNT
!                        read(cm%basefileunit + cfk%FB, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
!                    end do
!                    read(cm%basefileunit + cfk%FB, *, end = 999) !:EndFrame line
!                    do i = 1, bi%NA
!                        FSDOWN(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    FSVHGRD = 0.5*FSDOWN
!                    FSIHGRD = FSVHGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
!                    FSIHGAT = FSVHGAT
!                    ICOUNT = ICOUNT + 1

                !> CSV format.
!                case (2)
!                    read(cm%basefileunit + cfk%FB, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
!                    do i = 1, bi%NML
!                        CURGRU = bi%JLMOS(i)
!                        FSVHGAT(i) = 0.5*INVECTOR(CURGRU)
!                    end do
!                    FSIHGAT = FSVHGAT
!                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, FSVHGRD, FSVHGAT)
!                    FSDOWN = 2.0*FSVHGRD
!                    FSIHGRD = FSVHGRD
!                    ICOUNT = ICOUNT + 1

                !> Binary sequential format.
!                case (3)
!                    read(cm%basefileunit + cfk%FB, end = 999) NTIME
!                    read(cm%basefileunit + cfk%FB, end = 999) FSDOWN
!                    FSVHGRD = 0.5*FSDOWN
!                    FSIHGRD = FSVHGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
!                    FSIHGAT = FSVHGAT
!                    ICOUNT = ICOUNT + 1

                !> ASCII format.
!                case (4)
!                    read(cm%basefileunit + cfk%FB, *, end = 999) (FSDOWN(i), i = 1, bi%NA)
!                    FSVHGRD = 0.5*FSDOWN
!                    FSIHGRD = FSVHGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
!                    FSIHGAT = FSVHGAT
!                    ICOUNT = ICOUNT + 1

!                case default
!                    print 644, cm%clin(cfk%FB)%id_var, cm%clin(cfk%FB)%filefmt
!                    stop

!            end select !case (cm%clin(cfk%FB)%filefmt)
!    end if !(cm%clin(cfk%FB)%timestep_now == 0) then

    !> *****************************************************************
    !> Read longwave radiation data
    !> *****************************************************************
!    if (cm%clin(cfk%FI)%timestep_now == 0) then

        call update_data(bi, cm, cfk%FI, FDLGAT, need_update, ENDDATA)

        if (need_update) then
            call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, cm%clin(cfk%FI)%climvGrd, FDLGAT)
        end if
!            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
!        else
!            select case (cm%clin(cfk%FI)%filefmt)

!                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
!                    do k = 1, ICOUNT
!                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    end do
!                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    do i = 1, bi%NA
!                        FDLGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)

!                case (1)
!                    read(cm%basefileunit + cfk%FI, *, end = 999) !:Frame line
!                    do i = 1, bi%YCOUNT
!                        read(cm%basefileunit + cfk%FI, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
!                    end do
!                    read(cm%basefileunit + cfk%FI, *, end = 999) !:EndFrame line
!                    do i = 1, bi%NA
!                        FDLGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)
!                    ICOUNT = ICOUNT + 1

!                case (2)
!                    read(cm%basefileunit + cfk%FI, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
!                    do i = 1, bi%NML
!                        CURGRU = bi%JLMOS(i)
!                        FDLGAT(i) = INVECTOR(CURGRU)
!                    end do
!                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, FDLGRD, FDLGAT)
!                    ICOUNT = ICOUNT + 1

!                case (3)
!                    read(cm%basefileunit + cfk%FI, end = 999) NTIME
!                    read(cm%basefileunit + cfk%FI, end = 999) FDLGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)
!                    ICOUNT = ICOUNT + 1

!                case (4)
!                    read(cm%basefileunit + cfk%FI, *, end = 999) (FDLGRD(i), i = 1, bi%NA)
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)
!                    ICOUNT = ICOUNT + 1

!                case default
!                    print 644, cm%clin(cfk%FI)%id_var, cm%clin(cfk%FI)%filefmt
!                    stop

!            end select !case (cm%clin(cfk%FI)%filefmt)
!    end if !(cm%clin(cfk%FI)%timestep_now == 0) then

    !> *****************************************************************
    !> Read precipitation data
    !> *****************************************************************
!    if (cm%clin(cfk%PR)%timestep_now == 0) then

        call update_data(bi, cm, cfk%PR, PREGAT, need_update, ENDDATA)

!            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
        if (need_update) then
            call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, cm%clin(cfk%PR)%climvGrd, PREGAT)
        end if
!            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
!        else
!            select case (cm%clin(cfk%PR)%filefmt)

!                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
!                    do k = 1, ICOUNT
!                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    end do
!                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    do i = 1, bi%NA
!                        PREGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)

!                case (1)
!                    read(cm%basefileunit + cfk%PR, *, end = 999) !:Frame line
!                    do i = 1, bi%YCOUNT
!                        read (cm%basefileunit + cfk%PR, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
!                    end do
!                    read(cm%basefileunit + cfk%PR, *, end = 999) !:EndFrame line
!                    do i = 1, bi%NA
!                        PREGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
!                    ICOUNT = ICOUNT + 1

!                case (2)
!                    read(cm%basefileunit + cfk%PR, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
!                    do i = 1, bi%NML
!                        CURGRU = bi%JLMOS(i)
!                        PREGAT(i) = INVECTOR(CURGRU)
!                    end do
!                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, PREGRD, PREGAT)
!                    ICOUNT = ICOUNT + 1

!                case (3)
!                    read(cm%basefileunit + cfk%PR, end = 999) NTIME
!                    read(cm%basefileunit + cfk%PR, end = 999) PREGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
!                    ICOUNT = ICOUNT + 1

!                case (4)
!                    read(cm%basefileunit + cfk%PR, *, end = 999) (PREGRD(i), i = 1, bi%NA)
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
!                    ICOUNT = ICOUNT + 1

                !> Read from two sources of rainfall input.
!todo: re-instate with alpha
!                case (6)
!                    call NeedUpdate_clim_data(bi, cfk%PR, cm, ENDDATA)
!                    call NeedUpdate_clim_data(bi, 8, cm, ENDDATA)
!                    PREGRD = cm%clin(8)%alpharain*cm%clin(cfk%PR)%climv(:, cm%clin(cfk%PR)%itime) + &
!                             (1.0 - cm%clin(8)%alpharain)*cm%clin(8)%climv(:, cm%clin(8)%itime)
!                    cm%clin(cfk%PR)%itime = cm%clin(cfk%PR)%itime + 1
!                    if (cm%clin(cfk%PR)%itime > size(cm%clin(cfk%PR)%climv, 3)) cm%clin(cfk%PR)%itime = 1
!                    cm%clin(8)%itime = cm%clin(8)%itime + 1
!                    if (cm%clin(8)%itime > size(cm%clin(8)%climv, 3)) cm%clin(8)%itime = 1
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
!                    ICOUNT = ICOUNT + 1

!                case default
!                    print 644, cm%clin(cfk%PR)%id_var, cm%clin(cfk%PR)%filefmt
!                    stop

!            end select !case (cm%clin(cfk%PR)%filefmt)
!    end if !(cm%clin(cfk%PR)%timestep_now == 0) then

    !> *****************************************************************
    !> Read temperature data
    !> *****************************************************************
!    if (cm%clin(cfk%TT)%timestep_now == 0) then

        call update_data(bi, cm, cfk%TT, TAGAT, need_update, ENDDATA)

!            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)
        if (need_update) then
            call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, cm%clin(cfk%TT)%climvGrd, TAGAT)
        end if
!            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
!        else
!            select case (cm%clin(cfk%TT)%filefmt)

!                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
!                    do k = 1, ICOUNT
!                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    end do
!                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    do i = 1, bi%NA
!                        TAGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)

!                case (1)
!                    read(cm%basefileunit + cfk%TT, *, end = 999) !:Frame line
!                    do i = 1, bi%YCOUNT
!                        read(cm%basefileunit + cfk%TT, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
!                    end do
!                    read(cm%basefileunit + cfk%TT, *, end = 999) !:EndFrame line
!                    do i = 1, bi%NA
!                        TAGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)
!                    ICOUNT = ICOUNT + 1

!                case (2)
!                    read(cm%basefileunit + cfk%TT, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
!                    do i = 1, bi%NML
!                        CURGRU = bi%JLMOS(i)
!                        TAGAT(i) = INVECTOR(CURGRU)
!                    end do
!                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, TAGRD, TAGAT)
!                    ICOUNT = ICOUNT + 1

!                case (3)
!                    read(cm%basefileunit + cfk%TT, end = 999) NTIME
!                    read(cm%basefileunit + cfk%TT, end = 999) TAGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)
!                    ICOUNT = ICOUNT + 1

!                case (4)
!                    read(cm%basefileunit + cfk%TT, *, end = 999) (TAGRD(i), i = 1, bi%NA)
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)
!                    ICOUNT = ICOUNT + 1

!                case default
!                    print 644, cm%clin(cfk%TT)%id_var, cm%clin(cfk%TT)%filefmt
!                    stop

!            end select !case (cm%clin(cfk%TT)%filefmt)
!    end if !(cm%clin(cfk%TT)%timestep_now == 0) then

    !> *****************************************************************
    !> Read wind data
    !> *****************************************************************
!    if (cm%clin(cfk%UV)%timestep_now == 0) then

        call update_data(bi, cm, cfk%UV, ULGAT, need_update, ENDDATA)

!            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)
        if (need_update) then
            call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, cm%clin(cfk%UV)%climvGrd, ULGAT)
        end if
!            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
!        else
!            select case (cm%clin(cfk%UV)%filefmt)

!                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
!                    do k = 1, ICOUNT
!                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    end do
!                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    do i = 1, bi%NA
!                        ULGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
                    !VLGRD = 0.0
                    !VLGAT = 0.0
                    !UVGRD = max(VMIN, ULGRD)
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)

!                case (1)
!                    read(cm%basefileunit + cfk%UV, *, end = 999) !:Frame line
!                    do i = 1, bi%YCOUNT
!                        read(cm%basefileunit + cfk%UV, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
!                    end do
!                    read(cm%basefileunit + cfk%UV, *, end = 999) !:EndFrame line
!                    do i = 1, bi%NA
!                        ULGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
                    !VLGRD = 0.0
                    !VLGAT = 0.0
                    !UVGRD = max(VMIN, ULGRD)
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)
!                    ICOUNT = ICOUNT + 1

!                case (2)
!                    read(cm%basefileunit + cfk%UV, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
!                    do i=1, bi%NML
!                        CURGRU = bi%JLMOS(i)
!                        ULGAT(i) = INVECTOR(CURGRU)
!                    end do
                    !VLGRD = 0.0
                    !VLGAT = 0.0
!                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, ULGRD, ULGAT)
!                    ICOUNT = ICOUNT + 1

!                case (3)
!                    read(cm%basefileunit + cfk%UV, end = 999) NTIME
!                    read(cm%basefileunit + cfk%UV, end = 999) ULGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)
!                    ICOUNT = ICOUNT + 1

!                case (4)
!                    read(cm%basefileunit + cfk%UV, *, end = 999) (ULGRD(i), i = 1, bi%NA)
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)
!                    ICOUNT = ICOUNT + 1

!                case default
!                    print 644, cm%clin(cfk%UV)%id_var, cm%clin(cfk%UV)%filefmt
!                    stop

!            end select !case (cm%clin(cfk%UV)%filefmt)
!    end if !(cm%clin(cfk%UV)%timestep_now == 0) then

    !> *****************************************************************
    !> Read pressure data
    !> *****************************************************************
!    if (cm%clin(cfk%P0)%timestep_now == 0) then

        call update_data(bi, cm, cfk%P0, PRESGAT, need_update, ENDDATA)

!            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)
        if (need_update) then
            call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, cm%clin(cfk%P0)%climvGrd, PRESGAT)
        end if
!            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
!        else
!            select case (cm%clin(cfk%P0)%filefmt)

!                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
!                    do k = 1, ICOUNT
!                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    end do
!                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    do i = 1, bi%NA
!                        PRESGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)

!                case (1)
!                    read(cm%basefileunit + cfk%P0, *, end = 999) !:Frame line
!                    do i = 1, bi%YCOUNT
!                        read(cm%basefileunit + cfk%P0, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
!                    end do
!                    read(cm%basefileunit + cfk%P0, *, end = 999) !:EndFrame line
!                    do i = 1, bi%NA
!                        PRESGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)
!                    ICOUNT = ICOUNT + 1

!                case (2)
!                    read(cm%basefileunit + cfk%P0, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
!                    do i = 1, bi%NML
!                        CURGRU = bi%JLMOS(i)
!                        PRESGAT(i) = INVECTOR(CURGRU)
!                    end do
!                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, PRESGRD, PRESGAT)
!                    ICOUNT = ICOUNT + 1

!                case (3)
!                    read(cm%basefileunit + cfk%P0, end = 999) NTIME
!                    read(cm%basefileunit + cfk%P0, end = 999) PRESGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)
!                    ICOUNT = ICOUNT + 1

!                case (4)
!                    read(cm%basefileunit + cfk%P0, *, end = 999) (PRESGRD(i), i = 1, bi%NA)
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)
!                    ICOUNT = ICOUNT + 1

!                case default
!                    print 644, cm%clin(cfk%P0)%id_var, cm%clin(cfk%P0)%filefmt
!                    stop

!            end select !case (cm%clin(cfk%P0)%filefmt)
!    end if !(cm%clin(cfk%P0)%timestep_now == 0) then

    !> *****************************************************************
    !> Read humidity data
    !> *****************************************************************
!    if (cm%clin(cfk%HU)%timestep_now == 0) then

        call update_data(bi, cm, cfk%HU, QAGAT, need_update, ENDDATA)

!            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)
        if (need_update) then
            call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, cm%clin(cfk%HU)%climvGrd, QAGAT)
        end if
!            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
!        else
!            select case (cm%clin(cfk%HU)%filefmt)

!                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
!                    do k = 1, ICOUNT
!                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    end do
!                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
!                    do i = 1, bi%NA
!                        QAGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)

!                case (1)
!                    read(cm%basefileunit + cfk%HU, *, end = 999) !:Frame line
!                    do i = 1, bi%YCOUNT
!                        read(cm%basefileunit + cfk%HU, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
!                    end do
!                    read (cm%basefileunit + cfk%HU, *, end = 999) !:EndFrame line
!                    do i = 1, bi%NA
!                        QAGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
!                    end do
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)
!                    ICOUNT = ICOUNT + 1

!                case (2)
!                    read(cm%basefileunit + cfk%HU, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
!                    do i = 1, bi%NML
!                        CURGRU = bi%JLMOS(i)
!                        QAGAT(i) = INVECTOR(CURGRU)
!                    end do
!                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, QAGRD, QAGAT)
!                    ICOUNT = ICOUNT + 1

!                case (3)
!                    read(cm%basefileunit + cfk%HU, end = 999) NTIME
!                    read(cm%basefileunit + cfk%HU, end = 999) QAGRD
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)
!                    ICOUNT = ICOUNT + 1

!                case (4)
!                    read(cm%basefileunit + cfk%HU, *, end = 999) (QAGRD(i), i = 1, bi%NA)
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)
!                    ICOUNT = ICOUNT + 1

!                case default
!                    print 644, cm%clin(cfk%HU)%id_var, cm%clin(cfk%HU)%filefmt
!                    stop

!            end select !case (cm%clin(cfk%HU)%filefmt)
!    end if !(cm%clin(cfk%HU)%timestep_now == 0) then

!644 format(/1x'The input forcing file format is not supported', &
!        /2x, A15, I4/)

        return

999     ENDDATA = .true.

    end subroutine !READ_FORCING_DATA

    !> -----------------------------------------------------------------
    !> Description: Check if we need to load data again if that, we
    !> deallocate and allocate again and then we load data
    !> -----------------------------------------------------------------
    subroutine NeedUpdate_clim_data(bi, indx, cm, ENDDATA)

        !> Inputs variables.
        type(basin_info) :: bi
        integer indx

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Ouput variables.
        logical ENDDATA

        !> Check if we need to update.
        if (cm%clin(indx)%itime == 1) then
            if (allocated(cm%clin(indx)%climv)) deallocate(cm%clin(indx)%climv)
!            allocate(cm%clin(indx)%climv(bi%NML, cm%clin(indx)%nSeries, cm%clin(indx)%ntimes(cm%clin(indx)%readIndx)))
!todo: replace nseries with nfiles
            allocate(cm%clin(indx)%climv(bi%NML, 1, cm%clin(indx)%ntimes(cm%clin(indx)%readIndx)))
            call LoadData(bi, indx, cm, ENDDATA)
        end if

    end subroutine !NeedUpdate_clim_data

    subroutine update_data(bi, cm, indx, gat, need_update, end_data)

        use strings

        type(basin_info), intent(in) :: bi
        type(clim_info) :: cm
        integer, intent(in) :: indx
        real, dimension(:) :: gat
        logical, intent(out) :: need_update, end_data

        integer s, k, j, ios
        real a

        !> Read data on a new time-step.
        if (cm%clin(indx)%timestep_now == 0) then

            !> Read data (if needed).
            call NeedUpdate_clim_data(bi, indx, cm, end_data)

            !> Extract data from the climate variable.
            gat = cm%clin(indx)%climv(:, 1, cm%clin(indx)%itime)

            !> Apply conditions to the series of data is such conditions exist.
            if (cm%clin(indx)%nseries > 0) then
                do s = 1, cm%clin(indx)%nseries
                    select case (cm%clin(indx)%series(s)%attrtype)
                        case ('gru')
                            call value(cm%clin(indx)%series(s)%attr(1), j, ios)
                            call value(cm%clin(indx)%series(s)%attr(2), a, ios)
                            forall (k = 1:bi%NML, bi%JLMOS(k) == j) gat(k) = gat(k)*a
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

end module
