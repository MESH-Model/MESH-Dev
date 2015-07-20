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
    integer i, j, k, CURGRU, ICOUNT

    integer :: NTIME  ! time in read sequential

    !> Initialize counting number of r2c and csv files.
    ICOUNT = 0

    !> *****************************************************************
    !> Read shortwave radiation data
    !> *****************************************************************
    if (cm%clin(cfk%FS)%timestep_now == 0) then

        !> Read into memory.
        if (cm%clin(cfk%FS)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%FS, cm, ENDDATA)
            FSDOWN = cm%clin(cfk%FS)%climv(:, cm%clin(cfk%FS)%itime)
            cm%clin(cfk%FS)%itime = cm%clin(cfk%FS)%itime + 1
            if (cm%clin(cfk%FS)%itime > size(cm%clin(cfk%FS)%climv, 2)) then
                cm%clin(cfk%FS)%itime = 1
            end if
            FSVHGRD = 0.5*FSDOWN
            FSIHGRD = FSVHGRD
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
            FSIHGAT = FSVHGAT
            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
        else
            select case (cm%clin(cfk%FS)%filefmt)

                !> Legacy binary format.
                case (0)
                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    do i = 1, bi%NA
                        FSDOWN(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    FSVHGRD = 0.5*FSDOWN
                    FSIHGRD = FSVHGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
                    FSIHGAT = FSVHGAT

                !> ASCII R2C format.
                case (1)
                    read(cm%basefileunit + cfk%FS, *, end = 999) !:Frame line
                    do i = 1, bi%YCOUNT
                        read(cm%basefileunit + cfk%FS, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
                    end do
                    read(cm%basefileunit + cfk%FS, *, end = 999) !:EndFrame line
                    do i = 1, bi%NA
                        FSDOWN(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    FSVHGRD = 0.5*FSDOWN
                    FSIHGRD = FSVHGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
                    FSIHGAT = FSVHGAT
                    ICOUNT = ICOUNT + 1

                !> CSV format.
                case (2)
                    read(cm%basefileunit + cfk%FS, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
                    do i = 1, bi%NML
                        CURGRU = bi%JLMOS(i)
                        FSVHGAT(i) = 0.5*INVECTOR(CURGRU)
                    end do
                    FSIHGAT = FSVHGAT
                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, FSVHGRD, FSVHGAT)
                    FSDOWN = 2.0*FSVHGRD
                    FSIHGRD = FSVHGRD
                    ICOUNT = ICOUNT + 1

                !> Binary sequential format.
                case (3)
                    read(cm%basefileunit + cfk%FS, end = 999) NTIME
                    read(cm%basefileunit + cfk%FS, end = 999) FSDOWN
                    FSVHGRD = 0.5*FSDOWN
                    FSIHGRD = FSVHGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
                    FSIHGAT = FSVHGAT
                    ICOUNT = ICOUNT + 1

                !> ASCII format.
                case (4)
                    read(cm%basefileunit + cfk%FS, *, end = 999) (FSDOWN(i), i = 1, bi%NA)
                    FSVHGRD = 0.5*FSDOWN
                    FSIHGRD = FSVHGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FSVHGRD, FSVHGAT)
                    FSIHGAT = FSVHGAT
                    ICOUNT = ICOUNT + 1

                case default
                    print 644, cm%clin(cfk%FS)%id_var, cm%clin(cfk%FS)%filefmt
                    stop

            end select !case (cm%clin(cfk%FS)%filefmt)
        end if !(cm%clin(cfk%FS)%timeSize > 0) then
    end if !(cm%clin(cfk%FS)%timestep_now == 0) then

    !> *****************************************************************
    !> Read longwave radiation data
    !> *****************************************************************
    if (cm%clin(cfk%FDL)%timestep_now == 0) then

        !> Read into memory.
        if (cm%clin(cfk%FDL)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%FDL, cm, ENDDATA)
            FDLGRD = cm%clin(cfk%FDL)%climv(:, cm%clin(cfk%FDL)%itime)
            cm%clin(cfk%FDL)%itime = cm%clin(cfk%FDL)%itime + 1
            if (cm%clin(cfk%FDL)%itime > size(cm%clin(cfk%FDL)%climv, 2)) then
                cm%clin(cfk%FDL)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)
            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
        else
            select case (cm%clin(cfk%FDL)%filefmt)

                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
                    do k = 1, ICOUNT
                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    end do
                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    do i = 1, bi%NA
                        FDLGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)

                case (1)
                    read(cm%basefileunit + cfk%FDL, *, end = 999) !:Frame line
                    do i = 1, bi%YCOUNT
                        read(cm%basefileunit + cfk%FDL, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
                    end do
                    read(cm%basefileunit + cfk%FDL, *, end = 999) !:EndFrame line
                    do i = 1, bi%NA
                        FDLGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)
                    ICOUNT = ICOUNT + 1

                case (2)
                    read(cm%basefileunit + cfk%FDL, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
                    do i = 1, bi%NML
                        CURGRU = bi%JLMOS(i)
                        FDLGAT(i) = INVECTOR(CURGRU)
                    end do
                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, FDLGRD, FDLGAT)
                    ICOUNT = ICOUNT + 1

                case (3)
                    read(cm%basefileunit + cfk%FDL, end = 999) NTIME
                    read(cm%basefileunit + cfk%FDL, end = 999) FDLGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)
                    ICOUNT = ICOUNT + 1

                case (4)
                    read(cm%basefileunit + cfk%FDL, *, end = 999) (FDLGRD(i), i = 1, bi%NA)
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, FDLGRD, FDLGAT)
                    ICOUNT = ICOUNT + 1

                case default
                    print 644, cm%clin(cfk%FDL)%id_var, cm%clin(cfk%FDL)%filefmt
                    stop

            end select !case (cm%clin(cfk%FDL)%filefmt)
        end if !(cm%clin(cfk%FDL)%timeSize > 0) then
    end if !(cm%clin(cfk%FDL)%timestep_now == 0) then

    !> *****************************************************************
    !> Read precipitation data
    !> *****************************************************************
    if (cm%clin(cfk%PRE)%timestep_now == 0) then

        !> Read into memory.
        if (cm%clin(cfk%PRE)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%PRE, cm, ENDDATA)
            PREGRD = cm%clin(cfk%PRE)%climv(:, cm%clin(cfk%PRE)%itime)
            cm%clin(cfk%PRE)%itime = cm%clin(cfk%PRE)%itime + 1
            if (cm%clin(cfk%PRE)%itime > size(cm%clin(cfk%PRE)%climv, 2)) then
                cm%clin(cfk%PRE)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
        else
            select case (cm%clin(cfk%PRE)%filefmt)

                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
                    do k = 1, ICOUNT
                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    end do
                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    do i = 1, bi%NA
                        PREGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)

                case (1)
                    read(cm%basefileunit + cfk%PRE, *, end = 999) !:Frame line
                    do i = 1, bi%YCOUNT
                        read (cm%basefileunit + cfk%PRE, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
                    end do
                    read(cm%basefileunit + cfk%PRE, *, end = 999) !:EndFrame line
                    do i = 1, bi%NA
                        PREGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
                    ICOUNT = ICOUNT + 1

                case (2)
                    read(cm%basefileunit + cfk%PRE, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
                    do i = 1, bi%NML
                        CURGRU = bi%JLMOS(i)
                        PREGAT(i) = INVECTOR(CURGRU)
                    end do
                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, PREGRD, PREGAT)
                    ICOUNT = ICOUNT + 1

                case (3)
                    read(cm%basefileunit + cfk%PRE, end = 999) NTIME
                    read(cm%basefileunit + cfk%PRE, end = 999) PREGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
                    ICOUNT = ICOUNT + 1

                case (4)
                    read(cm%basefileunit + cfk%PRE, *, end = 999) (PREGRD(i), i = 1, bi%NA)
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
                    ICOUNT = ICOUNT + 1

                !> Read from two sources of rainfall input.
                case (6)
                    call NeedUpdate_clim_data(bi, cfk%PRE, cm, ENDDATA)
                    call NeedUpdate_clim_data(bi, 8, cm, ENDDATA)
                    PREGRD = cm%clin(8)%alpharain*cm%clin(cfk%PRE)%climv(:, cm%clin(cfk%PRE)%itime) + &
                             (1.0 - cm%clin(8)%alpharain)*cm%clin(8)%climv(:, cm%clin(8)%itime)
                    cm%clin(cfk%PRE)%itime = cm%clin(cfk%PRE)%itime + 1
                    if (cm%clin(cfk%PRE)%itime > size(cm%clin(cfk%PRE)%climv, 2)) cm%clin(cfk%PRE)%itime = 1
                    cm%clin(8)%itime = cm%clin(8)%itime + 1
                    if (cm%clin(8)%itime > size(cm%clin(8)%climv, 2)) cm%clin(8)%itime = 1
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PREGRD, PREGAT)
                    ICOUNT = ICOUNT + 1

                case default
                    print 644, cm%clin(cfk%PRE)%id_var, cm%clin(cfk%PRE)%filefmt
                    stop

            end select !case (cm%clin(cfk%PRE)%filefmt)
        end if !(cm%clin(cfk%PRE)%timeSize > 0) then
    end if !(cm%clin(cfk%PRE)%timestep_now == 0) then

    !> *****************************************************************
    !> Read temperature data
    !> *****************************************************************
    if (cm%clin(cfk%TA)%timestep_now == 0) then

        !> Read into memory.
        if (cm%clin(cfk%TA)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%TA, cm, ENDDATA)
            TAGRD = cm%clin(cfk%TA)%climv(:, cm%clin(cfk%TA)%itime)
            cm%clin(cfk%TA)%itime = cm%clin(cfk%TA)%itime + 1
            if (cm%clin(cfk%TA)%itime > size(cm%clin(cfk%TA)%climv, 2)) then
                cm%clin(cfk%TA)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)
            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
        else
            select case (cm%clin(cfk%TA)%filefmt)

                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
                    do k = 1, ICOUNT
                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    end do
                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    do i = 1, bi%NA
                        TAGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)

                case (1)
                    read(cm%basefileunit + cfk%TA, *, end = 999) !:Frame line
                    do i = 1, bi%YCOUNT
                        read(cm%basefileunit + cfk%TA, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
                    end do
                    read(cm%basefileunit + cfk%TA, *, end = 999) !:EndFrame line
                    do i = 1, bi%NA
                        TAGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)
                    ICOUNT = ICOUNT + 1

                case (2)
                    read(cm%basefileunit + cfk%TA, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
                    do i = 1, bi%NML
                        CURGRU = bi%JLMOS(i)
                        TAGAT(i) = INVECTOR(CURGRU)
                    end do
                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, TAGRD, TAGAT)
                    ICOUNT = ICOUNT + 1

                case (3)
                    read(cm%basefileunit + cfk%TA, end = 999) NTIME
                    read(cm%basefileunit + cfk%TA, end = 999) TAGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)
                    ICOUNT = ICOUNT + 1

                case (4)
                    read(cm%basefileunit + cfk%TA, *, end = 999) (TAGRD(i), i = 1, bi%NA)
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, TAGRD, TAGAT)
                    ICOUNT = ICOUNT + 1

                case default
                    print 644, cm%clin(cfk%TA)%id_var, cm%clin(cfk%TA)%filefmt
                    stop

            end select !case (cm%clin(cfk%TA)%filefmt)
        end if !(cm%clin(cfk%TA)%timeSize > 0) then
    end if !(cm%clin(cfk%TA)%timestep_now == 0) then

    !> *****************************************************************
    !> Read wind data
    !> *****************************************************************
    if (cm%clin(cfk%UL)%timestep_now == 0) then

        !> Read into memory.
        if (cm%clin(cfk%UL)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%UL, cm, ENDDATA)
            ULGRD = cm%clin(cfk%UL)%climv(:, cm%clin(cfk%UL)%itime)
            cm%clin(cfk%UL)%itime = cm%clin(cfk%UL)%itime + 1
            if (cm%clin(cfk%UL)%itime > size(cm%clin(cfk%UL)%climv, 2)) then
                cm%clin(cfk%UL)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)
            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
        else
            select case (cm%clin(cfk%UL)%filefmt)

                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
                    do k = 1, ICOUNT
                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    end do
                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    do i = 1, bi%NA
                        ULGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    !VLGRD = 0.0
                    !VLGAT = 0.0
                    !UVGRD = max(VMIN, ULGRD)
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)

                case (1)
                    read(cm%basefileunit + cfk%UL, *, end = 999) !:Frame line
                    do i = 1, bi%YCOUNT
                        read(cm%basefileunit + cfk%UL, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
                    end do
                    read(cm%basefileunit + cfk%UL, *, end = 999) !:EndFrame line
                    do i = 1, bi%NA
                        ULGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    !VLGRD = 0.0
                    !VLGAT = 0.0
                    !UVGRD = max(VMIN, ULGRD)
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)
                    ICOUNT = ICOUNT + 1

                case (2)
                    read(cm%basefileunit + cfk%UL, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
                    do i=1, bi%NML
                        CURGRU = bi%JLMOS(i)
                        ULGAT(i) = INVECTOR(CURGRU)
                    end do
                    !VLGRD = 0.0
                    !VLGAT = 0.0
                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, ULGRD, ULGAT)
                    ICOUNT = ICOUNT + 1

                case (3)
                    read(cm%basefileunit + cfk%UL, end = 999) NTIME
                    read(cm%basefileunit + cfk%UL, end = 999) ULGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)
                    ICOUNT = ICOUNT + 1

                case (4)
                    read(cm%basefileunit + cfk%UL, *, end = 999) (ULGRD(i), i = 1, bi%NA)
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, ULGRD, ULGAT)
                    ICOUNT = ICOUNT + 1

                case default
                    print 644, cm%clin(cfk%UL)%id_var, cm%clin(cfk%UL)%filefmt
                    stop

            end select !case (cm%clin(cfk%UL)%filefmt)
        end if !(cm%clin(cfk%UL)%timeSize > 0) then
    end if !(cm%clin(cfk%UL)%timestep_now == 0) then

    !> *****************************************************************
    !> Read pressure data
    !> *****************************************************************
    if (cm%clin(cfk%PRES)%timestep_now == 0) then

        !> Read into memory.
        if (cm%clin(cfk%PRES)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%PRES, cm, ENDDATA)
            PRESGRD = cm%clin(cfk%PRES)%climv(:, cm%clin(cfk%PRES)%itime)
            cm%clin(cfk%PRES)%itime = cm%clin(cfk%PRES)%itime + 1
            if (cm%clin(cfk%PRES)%itime > size(cm%clin(cfk%PRES)%climv, 2)) then
                cm%clin(cfk%PRES)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)
            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
        else
            select case (cm%clin(cfk%PRES)%filefmt)

                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
                    do k = 1, ICOUNT
                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    end do
                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    do i = 1, bi%NA
                        PRESGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)

                case (1)
                    read(cm%basefileunit + cfk%PRES, *, end = 999) !:Frame line
                    do i = 1, bi%YCOUNT
                        read(cm%basefileunit + cfk%PRES, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
                    end do
                    read(cm%basefileunit + cfk%PRES, *, end = 999) !:EndFrame line
                    do i = 1, bi%NA
                        PRESGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)
                    ICOUNT = ICOUNT + 1

                case (2)
                    read(cm%basefileunit + cfk%PRES, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
                    do i = 1, bi%NML
                        CURGRU = bi%JLMOS(i)
                        PRESGAT(i) = INVECTOR(CURGRU)
                    end do
                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, PRESGRD, PRESGAT)
                    ICOUNT = ICOUNT + 1

                case (3)
                    read(cm%basefileunit + cfk%PRES, end = 999) NTIME
                    read(cm%basefileunit + cfk%PRES, end = 999) PRESGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)
                    ICOUNT = ICOUNT + 1

                case (4)
                    read(cm%basefileunit + cfk%PRES, *, end = 999) (PRESGRD(i), i = 1, bi%NA)
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, PRESGRD, PRESGAT)
                    ICOUNT = ICOUNT + 1

                case default
                    print 644, cm%clin(cfk%PRES)%id_var, cm%clin(cfk%PRES)%filefmt
                    stop

            end select !case (cm%clin(cfk%PRES)%filefmt)
        end if !(cm%clin(cfk%PRES)%timeSize > 0) then
    end if !(cm%clin(cfk%PRES)%timestep_now == 0) then

    !> *****************************************************************
    !> Read humidity data
    !> *****************************************************************
    if (cm%clin(cfk%QA)%timestep_now == 0) then

        !> Read into memory.
        if (cm%clin(cfk%QA)%timeSize > 0) then
            call NeedUpdate_clim_data(bi, cfk%QA, cm, ENDDATA)
            QAGRD = cm%clin(cfk%QA)%climv(:, cm%clin(cfk%QA)%itime)
            cm%clin(cfk%QA)%itime = cm%clin(cfk%QA)%itime + 1
            if (cm%clin(cfk%QA)%itime > size(cm%clin(cfk%QA)%climv, 2)) then
                cm%clin(cfk%QA)%itime = 1
            end if
            call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)
            ICOUNT = ICOUNT + 1

        !> Switch and read and a single record from file.
        else
            select case (cm%clin(cfk%QA)%filefmt)

                case (0)

                    !> Skip the forcing data that is read from r2c and csv files.
                    do k = 1, ICOUNT
                        read(51, end = 999) ((JUNK, j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    end do
                    read(51, end = 999) ((INARRAY(i, j), j = 1, bi%XCOUNT), i = 1, bi%YCOUNT)
                    do i = 1, bi%NA
                        QAGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)

                case (1)
                    read(cm%basefileunit + cfk%QA, *, end = 999) !:Frame line
                    do i = 1, bi%YCOUNT
                        read(cm%basefileunit + cfk%QA, *, end = 999) (INARRAY(i, j), j = 1, bi%XCOUNT)
                    end do
                    read (cm%basefileunit + cfk%QA, *, end = 999) !:EndFrame line
                    do i = 1, bi%NA
                        QAGRD(i) = INARRAY(bi%YYY(i), bi%XXX(i))
                    end do
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)
                    ICOUNT = ICOUNT + 1

                case (2)
                    read(cm%basefileunit + cfk%QA, *, end = 999) (INVECTOR(i), i = 1, bi%NTYPE)
                    do i = 1, bi%NML
                        CURGRU = bi%JLMOS(i)
                        QAGAT(i) = INVECTOR(CURGRU)
                    end do
                    call SCATTER(bi%NTYPE, bi%NA, bi%NML, bi%ILMOS, bi%JLMOS, bi%ACLASS, QAGRD, QAGAT)
                    ICOUNT = ICOUNT + 1

                case (3)
                    read(cm%basefileunit + cfk%QA, end = 999) NTIME
                    read(cm%basefileunit + cfk%QA, end = 999) QAGRD
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)
                    ICOUNT = ICOUNT + 1

                case (4)
                    read(cm%basefileunit + cfk%QA, *, end = 999) (QAGRD(i), i = 1, bi%NA)
                    call GATHER(bi%NA, bi%NML, bi%ILG, bi%ILMOS, QAGRD, QAGAT)
                    ICOUNT = ICOUNT + 1

                case default
                    print 644, cm%clin(cfk%QA)%id_var, cm%clin(cfk%QA)%filefmt
                    stop

            end select !case (cm%clin(cfk%QA)%filefmt)
        end if !(cm%clin(cfk%QA)%timeSize > 0) then
    end if !(cm%clin(cfk%QA)%timestep_now == 0) then

644 format(/1x'The input forcing file format is not supported', &
        /2x, A15, I4/)

    return

999 ENDDATA = .true.

end subroutine !READ_FORCING_DATA
