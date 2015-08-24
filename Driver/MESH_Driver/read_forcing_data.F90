subroutine READ_FORCING_DATA(bi, cm, &
                             FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
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

    use sa_mesh_shared_variabletypes
    use FLAGS
    use climate_forcing, only: clim_info, cfk, NeedUpdate_clim_data

    implicit none

    !> Input variables.
    type(basin_info), intent(in) :: bi

    !> Input/Output variables.
    type(clim_info) :: cm

    !> Output variables.
    real*4, dimension(bi%NA) :: FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD
    real*4, dimension(bi%ILG) :: FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT
    logical ENDDATA

    !> Local variables.
    real*4, dimension(bi%YCOUNT, bi%XCOUNT) :: INARRAY
    real*4, dimension(bi%NTYPE) :: INVECTOR
    real*4 JUNK
    integer t, s, i, j, k, CURGRU, ICOUNT

    integer :: NTIME  ! time in read sequential

    !> Initialize counting number of r2c and csv files.
    ICOUNT = 0

    !> *****************************************************************
    !> Read shortwave radiation data
    !> *****************************************************************
    if (cm%clin(cfk%FB)%timestep_now == 0) then

        !> Read into memory.
!        if (cm%clin(cfk%FB)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%FB, cm, ENDDATA)
            FSDOWN = 0.0
            do s = 1, size(cm%clin(cfk%FB)%climv, 2)
                FSDOWN = FSDOWN + cm%clin(cfk%FB)%climv(:, s, cm%clin(cfk%FB)%itime)*cm%clin(cfk%FB)%alpha(s)
            end do
            cm%clin(cfk%FB)%itime = cm%clin(cfk%FB)%itime + 1
            if (cm%clin(cfk%FB)%itime > size(cm%clin(cfk%FB)%climv, 3)) then
                cm%clin(cfk%FB)%itime = 1
            end if
            FSVHGRD = 0.5*FSDOWN
            FSIHGRD = FSVHGRD
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
            FSIHGAT = FSVHGAT
            ICOUNT = ICOUNT + 1

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
!        end if !(cm%clin(cfk%FB)%timeSize > 0) then
    end if !(cm%clin(cfk%FB)%timestep_now == 0) then

    !> *****************************************************************
    !> Read longwave radiation data
    !> *****************************************************************
    if (cm%clin(cfk%FI)%timestep_now == 0) then

        !> Read into memory.
!        if (cm%clin(cfk%FI)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%FI, cm, ENDDATA)
            FDLGRD = 0.0
            do s = 1, size(cm%clin(cfk%FI)%climv, 2)
                FDLGRD = FDLGRD + cm%clin(cfk%FI)%climv(:, s, cm%clin(cfk%FI)%itime)*cm%clin(cfk%FI)%alpha(s)
            end do
            cm%clin(cfk%FI)%itime = cm%clin(cfk%FI)%itime + 1
            if (cm%clin(cfk%FI)%itime > size(cm%clin(cfk%FI)%climv, 3)) then
                cm%clin(cfk%FI)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)
            ICOUNT = ICOUNT + 1

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
!        end if !(cm%clin(cfk%FI)%timeSize > 0) then
    end if !(cm%clin(cfk%FI)%timestep_now == 0) then

    !> *****************************************************************
    !> Read precipitation data
    !> *****************************************************************
    if (cm%clin(cfk%PR)%timestep_now == 0) then

        !> Read into memory.
!        if (cm%clin(cfk%PR)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%PR, cm, ENDDATA)
            PREGRD = 0.0
            do s = 1, size(cm%clin(cfk%PR)%climv, 2)
                PREGRD = PREGRD + cm%clin(cfk%PR)%climv(:, s, cm%clin(cfk%PR)%itime)*cm%clin(cfk%PR)%alpha(s)
            end do
            cm%clin(cfk%PR)%itime = cm%clin(cfk%PR)%itime + 1
            if (cm%clin(cfk%PR)%itime > size(cm%clin(cfk%PR)%climv, 3)) then
                cm%clin(cfk%PR)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
            ICOUNT = ICOUNT + 1

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
!                    if (cm%clin(cfk%PR)%itime > size(cm%clin(cfk%PR)%climv, 2)) cm%clin(cfk%PR)%itime = 1
!                    cm%clin(8)%itime = cm%clin(8)%itime + 1
!                    if (cm%clin(8)%itime > size(cm%clin(8)%climv, 2)) cm%clin(8)%itime = 1
!                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
!                    ICOUNT = ICOUNT + 1

!                case default
!                    print 644, cm%clin(cfk%PR)%id_var, cm%clin(cfk%PR)%filefmt
!                    stop

!            end select !case (cm%clin(cfk%PR)%filefmt)
!        end if !(cm%clin(cfk%PR)%timeSize > 0) then
    end if !(cm%clin(cfk%PR)%timestep_now == 0) then

    !> *****************************************************************
    !> Read temperature data
    !> *****************************************************************
    if (cm%clin(cfk%TT)%timestep_now == 0) then

        !> Read into memory.
!        if (cm%clin(cfk%TT)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%TT, cm, ENDDATA)
            TAGRD = 0.0
            do s = 1, size(cm%clin(cfk%TT)%climv, 2)
                TAGRD = TAGRD + cm%clin(cfk%TT)%climv(:, s, cm%clin(cfk%TT)%itime)*cm%clin(cfk%TT)%alpha(s)
            end do
            cm%clin(cfk%TT)%itime = cm%clin(cfk%TT)%itime + 1
            if (cm%clin(cfk%TT)%itime > size(cm%clin(cfk%TT)%climv, 3)) then
                cm%clin(cfk%TT)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)
            ICOUNT = ICOUNT + 1

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
!        end if !(cm%clin(cfk%TT)%timeSize > 0) then
    end if !(cm%clin(cfk%TT)%timestep_now == 0) then

    !> *****************************************************************
    !> Read wind data
    !> *****************************************************************
    if (cm%clin(cfk%UV)%timestep_now == 0) then

        !> Read into memory.
!        if (cm%clin(cfk%UV)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%UV, cm, ENDDATA)
            ULGRD = 0.0
            do s = 1, size(cm%clin(cfk%UV)%climv, 2)
                ULGRD = ULGRD + cm%clin(cfk%UV)%climv(:, s, cm%clin(cfk%UV)%itime)*cm%clin(cfk%UV)%alpha(s)
            end do
            cm%clin(cfk%UV)%itime = cm%clin(cfk%UV)%itime + 1
            if (cm%clin(cfk%UV)%itime > size(cm%clin(cfk%UV)%climv, 3)) then
                cm%clin(cfk%UV)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)
            ICOUNT = ICOUNT + 1

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
!        end if !(cm%clin(cfk%UV)%timeSize > 0) then
    end if !(cm%clin(cfk%UV)%timestep_now == 0) then

    !> *****************************************************************
    !> Read pressure data
    !> *****************************************************************
    if (cm%clin(cfk%P0)%timestep_now == 0) then

        !> Read into memory.
!        if (cm%clin(cfk%P0)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%P0, cm, ENDDATA)
            PRESGRD = 0.0
            do s = 1, size(cm%clin(cfk%P0)%climv, 2)
                PRESGRD = PRESGRD + cm%clin(cfk%P0)%climv(:, s, cm%clin(cfk%P0)%itime)*cm%clin(cfk%P0)%alpha(s)
            end do
            cm%clin(cfk%P0)%itime = cm%clin(cfk%P0)%itime + 1
            if (cm%clin(cfk%P0)%itime > size(cm%clin(cfk%P0)%climv, 3)) then
                cm%clin(cfk%P0)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)
            ICOUNT = ICOUNT + 1

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
!        end if !(cm%clin(cfk%P0)%timeSize > 0) then
    end if !(cm%clin(cfk%P0)%timestep_now == 0) then

    !> *****************************************************************
    !> Read humidity data
    !> *****************************************************************
    if (cm%clin(cfk%HU)%timestep_now == 0) then

        !> Read into memory.
!        if (cm%clin(cfk%HU)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%HU, cm, ENDDATA)
            QAGRD = 0.0
            do s = 1, size(cm%clin(cfk%HU)%climv, 2)
                QAGRD = QAGRD + cm%clin(cfk%HU)%climv(:, s, cm%clin(cfk%HU)%itime)*cm%clin(cfk%HU)%alpha(s)
            end do
            cm%clin(cfk%HU)%itime = cm%clin(cfk%HU)%itime + 1
            if (cm%clin(cfk%HU)%itime > size(cm%clin(cfk%HU)%climv, 3)) then
                cm%clin(cfk%HU)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)
            ICOUNT = ICOUNT + 1

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
!        end if !(cm%clin(cfk%HU)%timeSize > 0) then
    end if !(cm%clin(cfk%HU)%timestep_now == 0) then

!644 format(/1x'The input forcing file format is not supported', &
!        /2x, A15, I4/)

    return

999 ENDDATA = .true.

end subroutine !READ_FORCING_DATA
