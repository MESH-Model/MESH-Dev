subroutine READ_FORCING_DATA(YCOUNT, XCOUNT, NTYPE, NA, NML, ILG, ILMOS, JLMOS, YYY, XXX, ENDDATA, ACLASS, &
                             FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                             FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, cm)

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

    use FLAGS
    use climate_forcing, only: clim_info, cfk, NeedUpdate_clim_data

    implicit none

    integer YCOUNT, XCOUNT, NTYPE, NA, NML, ILG
    logical ENDDATA

    real*4, dimension(YCOUNT, XCOUNT) :: INARRAY
    real*4, dimension(NTYPE) :: INVECTOR
    real*4, dimension(NA, NTYPE) :: ACLASS
    real*4, dimension(NA) :: FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD
    real*4, dimension(ILG) :: FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT
    real*4 JUNK
    integer*4, dimension(NA) :: YYY, XXX
    integer*4, dimension(ILG) :: ILMOS, JLMOS
    integer i, j, k, CURGRU, ICOUNT

    type(clim_info) :: cm

    integer :: NTIME  ! time in read sequential

    !> Initialize counting number of r2c and csv files.
    ICOUNT = 0

    !> *****************************************************************
    !> Read shortwave radiation data
    !> *****************************************************************
    if (cm%clin(cfk%FS)%timestep_now == 0) then

        !> Switch and read from the appropriate file format.
        select case (cm%clin(cfk%FS)%filefmt)

            !> Legacy binary format.
            case (0)
                read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
                do i = 1, NA
                    FSDOWN(i) = INARRAY(YYY(i), XXX(i))
                end do
                FSVHGRD = 0.5*FSDOWN
                FSIHGRD = FSVHGRD
                call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
                FSIHGAT = FSVHGAT

            !> ASCII R2C format.
            case (1)
                read(cm%basefileunit + cfk%FS, *, end = 999) !:Frame line
                do i = 1, YCOUNT
                    read(cm%basefileunit + cfk%FS, *, end = 999) (INARRAY(i, j), j = 1, XCOUNT)
                end do
                read(cm%basefileunit + cfk%FS, *, end = 999) !:EndFrame line
                do i = 1, NA
                    FSDOWN(i) = INARRAY(YYY(i), XXX(i))
                end do
                FSVHGRD = 0.5*FSDOWN
                FSIHGRD = FSVHGRD
                call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
                FSIHGAT = FSVHGAT
                ICOUNT = ICOUNT + 1

            !> CSV format.
            case (2)
                read(cm%basefileunit + cfk%FS, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
                do i = 1, NML
                    CURGRU = JLMOS(i)
                    FSVHGAT(i) = 0.5*INVECTOR(CURGRU)
                end do
                FSIHGAT = FSVHGAT
                call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, FSVHGRD, FSVHGAT)
                FSDOWN = 2.0*FSVHGRD
                FSIHGRD = FSVHGRD
                ICOUNT = ICOUNT + 1

            !> Binary sequential format.
            case (3)
                read(cm%basefileunit + cfk%FS, end = 999) NTIME
                read(cm%basefileunit + cfk%FS, end = 999) FSDOWN
                FSVHGRD = 0.5*FSDOWN
                FSIHGRD = FSVHGRD
                call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
                FSIHGAT = FSVHGAT
                ICOUNT = ICOUNT + 1

            !> ASCII format.
            case (4)
                read(cm%basefileunit + cfk%FS, *, end = 999) (FSDOWN(i), i = 1, NA)
                FSVHGRD = 0.5*FSDOWN
                FSIHGRD = FSVHGRD
                call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
                FSIHGAT = FSVHGAT
                ICOUNT = ICOUNT + 1

            !> In-memory option.
            case (5)
                call NeedUpdate_clim_data(cm, cfk%FS, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
                FSDOWN = cm%clin(cfk%FS)%climv(:, cm%clin(cfk%FS)%itime)
                cm%clin(cfk%FS)%itime = cm%clin(cfk%FS)%itime + 1
                if (cm%clin(cfk%FS)%itime > size(cm%clin(cfk%FS)%climv, 2)) cm%clin(cfk%FS)%itime = 1
                FSVHGRD = 0.5*FSDOWN
                FSIHGRD = FSVHGRD
                call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
                FSIHGAT = FSVHGAT
                ICOUNT = ICOUNT + 1

            case default
                print 644, cm%clin(cfk%FS)%id_var, cm%clin(cfk%FS)%filefmt
                stop

        end select !case (cm%clin(cfk%FS)%filefmt)
    end if !(cm%clin(cfk%FS)%timestep_now == 0) then

    !> *****************************************************************
    !> Read longwave radiation data
    !> *****************************************************************
    if (cm%clin(cfk%FDL)%timestep_now == 0) then

        !> Switch and read from the appropriate file format.
        select case (cm%clin(cfk%FDL)%filefmt)

            case (0)

                !> Skip the forcing data that is read from r2c and csv files.
                do k = 1, ICOUNT
                    read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
                end do
                read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
                do i = 1, NA
                    FDLGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)

            case (1)
                read(cm%basefileunit + cfk%FDL, *, end = 999) !:Frame line
                do i = 1, YCOUNT
                    read(cm%basefileunit + cfk%FDL, *, end = 999) (INARRAY(i, j), j = 1, XCOUNT)
                end do
                read(cm%basefileunit + cfk%FDL, *, end = 999) !:EndFrame line
                do i = 1, NA
                    FDLGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
                ICOUNT = ICOUNT + 1

            case (2)
                read(cm%basefileunit + cfk%FDL, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
                do i = 1, NML
                    CURGRU = JLMOS(i)
                    FDLGAT(i) = INVECTOR(CURGRU)
                end do
                call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, FDLGRD, FDLGAT)
                ICOUNT = ICOUNT + 1

            case (3)
                read(cm%basefileunit + cfk%FDL, end = 999) NTIME
                read(cm%basefileunit + cfk%FDL, end = 999) FDLGRD
                call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
                ICOUNT = ICOUNT + 1

            case (4)
                read(cm%basefileunit + cfk%FDL, *, end = 999) (FDLGRD(i), i = 1, NA)
                call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
                ICOUNT = ICOUNT + 1

            case (5)
                call NeedUpdate_clim_data(cm, cfk%FDL, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
                FDLGRD = cm%clin(cfk%FDL)%climv(:, cm%clin(cfk%FDL)%itime)
                cm%clin(cfk%FDL)%itime = cm%clin(cfk%FDL)%itime + 1
                if (cm%clin(cfk%FDL)%itime > size(cm%clin(cfk%FDL)%climv, 2)) cm%clin(cfk%FDL)%itime = 1
                call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
                ICOUNT = ICOUNT + 1

            case default
                print 644, cm%clin(cfk%FDL)%id_var, cm%clin(cfk%FDL)%filefmt
                stop

        end select !case (cm%clin(cfk%FDL)%filefmt)
    end if !(cm%clin(cfk%FDL)%timestep_now == 0) then

    !> *****************************************************************
    !> Read precipitation data
    !> *****************************************************************
    if (cm%clin(cfk%PRE)%timestep_now == 0) then

        !> Switch and read from the appropriate file format.
        select case (cm%clin(cfk%PRE)%filefmt)

            case (0)

                !> Skip the forcing data that is read from r2c and csv files.
                do k = 1, ICOUNT
                    read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
                end do
                read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
                do i = 1, NA
                    PREGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)

            case (1)
                read(cm%basefileunit + cfk%PRE, *, end = 999) !:Frame line
                do i = 1, YCOUNT
                    read (cm%basefileunit + cfk%PRE, *, end = 999) (INARRAY(i, j), j = 1, XCOUNT)
                end do
                read(cm%basefileunit + cfk%PRE, *, end = 999) !:EndFrame line
                do i = 1, NA
                    PREGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
                ICOUNT = ICOUNT + 1

            case (2)
                read(cm%basefileunit + cfk%PRE, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
                do i = 1,NML
                    CURGRU = JLMOS(i)
                    PREGAT(i) = INVECTOR(CURGRU)
                end do
                call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, PREGRD, PREGAT)
                ICOUNT = ICOUNT + 1

            case (3)
                read(cm%basefileunit + cfk%PRE, end = 999) NTIME
                read(cm%basefileunit + cfk%PRE, end = 999) PREGRD
                call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
                ICOUNT = ICOUNT + 1

            case (4)
                read(cm%basefileunit + cfk%PRE, *, end = 999) (PREGRD(i), i = 1, NA)
                call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
                ICOUNT = ICOUNT + 1

            case (5)
                call NeedUpdate_clim_data(cm, cfk%PRE, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
                PREGRD = cm%clin(cfk%PRE)%climv(:, cm%clin(cfk%PRE)%itime)
                cm%clin(cfk%PRE)%itime = cm%clin(cfk%PRE)%itime + 1
                if (cm%clin(cfk%PRE)%itime > size(cm%clin(cfk%PRE)%climv, 2)) cm%clin(cfk%PRE)%itime = 1
                call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
                ICOUNT = ICOUNT + 1

            !> Read from two sources of rainfall input.
            case (6)
                call NeedUpdate_clim_data(cm, cfk%PRE, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
                call NeedUpdate_clim_data(cm, 8, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
                PREGRD = cm%clin(8)%alpharain*cm%clin(cfk%PRE)%climv(:, cm%clin(cfk%PRE)%itime) + &
                         (1.0 - cm%clin(8)%alpharain)*cm%clin(8)%climv(:, cm%clin(8)%itime)
                cm%clin(cfk%PRE)%itime = cm%clin(cfk%PRE)%itime + 1
                if (cm%clin(cfk%PRE)%itime > size(cm%clin(cfk%PRE)%climv, 2)) cm%clin(cfk%PRE)%itime = 1
                cm%clin(8)%itime = cm%clin(8)%itime + 1
                if (cm%clin(8)%itime > size(cm%clin(8)%climv, 2)) cm%clin(8)%itime = 1
                call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
                ICOUNT = ICOUNT + 1

            case default
                print 644, cm%clin(cfk%PRE)%id_var, cm%clin(cfk%PRE)%filefmt
                stop

        end select !case (cm%clin(cfk%PRE)%filefmt)
    end if !(cm%clin(cfk%PRE)%timestep_now == 0) then

    !> *****************************************************************
    !> Read temperature data
    !> *****************************************************************
    if (cm%clin(cfk%TA)%timestep_now == 0) then

        !> Switch and read from the appropriate file format.
        select case (cm%clin(cfk%TA)%filefmt)

            case (0)

                !> Skip the forcing data that is read from r2c and csv files.
                do k = 1, ICOUNT
                    read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
                end do
                read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
                do i = 1, NA
                    TAGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)

            case (1)
                read(cm%basefileunit + cfk%TA, *, end = 999) !:Frame line
                do i = 1, YCOUNT
                    read(cm%basefileunit + cfk%TA, *, end = 999) (INARRAY(i, j), j = 1, XCOUNT)
                end do
                read(cm%basefileunit + cfk%TA, *, end = 999) !:EndFrame line
                do i = 1, NA
                    TAGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
                ICOUNT = ICOUNT + 1

            case (2)
                read(cm%basefileunit + cfk%TA, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
                do i = 1, NML
                    CURGRU = JLMOS(i)
                    TAGAT(i) = INVECTOR(CURGRU)
                end do
                call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, TAGRD, TAGAT)
                ICOUNT = ICOUNT + 1

            case (3)
                read(cm%basefileunit + cfk%TA, end = 999) NTIME
                read(cm%basefileunit + cfk%TA, end = 999) TAGRD
                call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
                ICOUNT = ICOUNT + 1

            case (4)
                read(cm%basefileunit + cfk%TA, *, end = 999) (TAGRD(i), i = 1, NA)
                call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
                ICOUNT = ICOUNT + 1

            case (5)
                call NeedUpdate_clim_data(cm, cfk%TA, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
                TAGRD = cm%clin(cfk%TA)%climv(:, cm%clin(cfk%TA)%itime)
                cm%clin(cfk%TA)%itime = cm%clin(cfk%TA)%itime + 1
                if (cm%clin(cfk%TA)%itime > size(cm%clin(cfk%TA)%climv, 2)) cm%clin(cfk%TA)%itime = 1
                call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
                ICOUNT = ICOUNT + 1

            case default
                print 644, cm%clin(cfk%TA)%id_var, cm%clin(cfk%TA)%filefmt
                stop

        end select !case (cm%clin(cfk%TA)%filefmt)
    end if !(cm%clin(cfk%TA)%timestep_now == 0) then

    !> *****************************************************************
    !> Read wind data
    !> *****************************************************************
    if (cm%clin(cfk%UL)%timestep_now == 0) then

        !> Switch and read from the appropriate file format.
        select case (cm%clin(cfk%UL)%filefmt)

            case (0)

                !> Skip the forcing data that is read from r2c and csv files.
                do k = 1, ICOUNT
                    read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
                end do
                read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
                do i = 1, NA
                    ULGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                !VLGRD = 0.0
                !VLGAT = 0.0
                !UVGRD = max(VMIN, ULGRD)
                call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)

            case (1)
                read(cm%basefileunit + cfk%UL, *, end = 999) !:Frame line
                do i = 1, YCOUNT
                    read(cm%basefileunit + cfk%UL, *, end = 999) (INARRAY(i, j), j = 1, XCOUNT)
                end do
                read(cm%basefileunit + cfk%UL, *, end = 999) !:EndFrame line
                do i = 1, NA
                    ULGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                !VLGRD = 0.0
                !VLGAT = 0.0
                !UVGRD = max(VMIN, ULGRD)
                call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
                ICOUNT = ICOUNT + 1

            case (2)
                read(cm%basefileunit + cfk%UL, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
                do i=1, NML
                    CURGRU = JLMOS(i)
                    ULGAT(i) = INVECTOR(CURGRU)
                end do
                !VLGRD = 0.0
                !VLGAT = 0.0
                call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, ULGRD, ULGAT)
                ICOUNT = ICOUNT + 1

            case (3)
                read(cm%basefileunit + cfk%UL, end = 999) NTIME
                read(cm%basefileunit + cfk%UL, end = 999) ULGRD
                call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
                ICOUNT = ICOUNT + 1

            case (4)
                read(cm%basefileunit + cfk%UL, *, end = 999) (ULGRD(i), i = 1, NA)
                call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
                ICOUNT = ICOUNT + 1

            case (5)
                call NeedUpdate_clim_data(cm, cfk%UL, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
                ULGRD = cm%clin(cfk%UL)%climv(:, cm%clin(cfk%UL)%itime)
                cm%clin(cfk%UL)%itime = cm%clin(cfk%UL)%itime + 1
                if (cm%clin(cfk%UL)%itime > size(cm%clin(cfk%UL)%climv, 2)) cm%clin(cfk%UL)%itime = 1
                call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
                ICOUNT = ICOUNT + 1

            case default
                print 644, cm%clin(cfk%UL)%id_var, cm%clin(cfk%UL)%filefmt
                stop

        end select !case (cm%clin(cfk%UL)%filefmt)
    end if !(cm%clin(cfk%UL)%timestep_now == 0) then

    !> *****************************************************************
    !> Read pressure data
    !> *****************************************************************
    if (cm%clin(cfk%PRES)%timestep_now == 0) then

        !> Switch and read from the appropriate file format.
        select case (cm%clin(cfk%PRES)%filefmt)

            case (0)

                !> Skip the forcing data that is read from r2c and csv files.
                do k = 1, ICOUNT
                    read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
                end do
                read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
                do i = 1, NA
                    PRESGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)

            case (1)
                read(cm%basefileunit + cfk%PRES, *, end = 999) !:Frame line
                do i = 1, YCOUNT
                    read(cm%basefileunit + cfk%PRES, *, end = 999) (INARRAY(i, j), j = 1, XCOUNT)
                end do
                read(cm%basefileunit + cfk%PRES, *, end = 999) !:EndFrame line
                do i = 1, NA
                    PRESGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
                ICOUNT = ICOUNT + 1

            case (2)
                read(cm%basefileunit + cfk%PRES, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
                do i = 1, NML
                    CURGRU = JLMOS(i)
                    PRESGAT(i) = INVECTOR(CURGRU)
                end do
                call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, PRESGRD, PRESGAT)
                ICOUNT = ICOUNT + 1

            case (3)
                read(cm%basefileunit + cfk%PRES, end = 999) NTIME
                read(cm%basefileunit + cfk%PRES, end = 999) PRESGRD
                call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
                ICOUNT = ICOUNT + 1

            case (4)
                read(cm%basefileunit + cfk%PRES, *, end = 999) (PRESGRD(i), i = 1, NA)
                call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
                ICOUNT = ICOUNT + 1

            case (5)
                call NeedUpdate_clim_data(cm, cfk%PRES, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
                PRESGRD = cm%clin(cfk%PRES)%climv(:, cm%clin(cfk%PRES)%itime)
                cm%clin(cfk%PRES)%itime = cm%clin(cfk%PRES)%itime + 1
                if (cm%clin(cfk%PRES)%itime > size(cm%clin(cfk%PRES)%climv, 2)) cm%clin(cfk%PRES)%itime = 1
                call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
                ICOUNT = ICOUNT + 1

            case default
                print 644, cm%clin(cfk%PRES)%id_var, cm%clin(cfk%PRES)%filefmt
                stop

        end select !case (cm%clin(cfk%PRES)%filefmt)
    end if !(cm%clin(cfk%PRES)%timestep_now == 0) then

    !> *****************************************************************
    !> Read humidity data
    !> *****************************************************************
    if (cm%clin(cfk%QA)%timestep_now == 0) then

        !> Switch and read from the appropriate file format.
        select case (cm%clin(cfk%QA)%filefmt)

            case (0)

                !> Skip the forcing data that is read from r2c and csv files.
                do k = 1, ICOUNT
                    read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
                end do
                read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
                do i = 1, NA
                    QAGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)

            case (1)
                read(cm%basefileunit + cfk%QA, *, end = 999) !:Frame line
                do i = 1, YCOUNT
                    read(cm%basefileunit + cfk%QA, *, end = 999) (INARRAY(i, j), j = 1, XCOUNT)
                end do
                read (cm%basefileunit + cfk%QA, *, end = 999) !:EndFrame line
                do i = 1, NA
                    QAGRD(i) = INARRAY(YYY(i), XXX(i))
                end do
                call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
                ICOUNT = ICOUNT + 1

            case (2)
                read(cm%basefileunit + cfk%QA, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
                do i = 1, NML
                    CURGRU = JLMOS(i)
                    QAGAT(i) = INVECTOR(CURGRU)
                end do
                call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, QAGRD, QAGAT)
                ICOUNT = ICOUNT + 1

            case (3)
                read(cm%basefileunit + cfk%QA, end = 999) NTIME
                read(cm%basefileunit + cfk%QA, end = 999) QAGRD
                call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
                ICOUNT = ICOUNT + 1

            case (4)
                read(cm%basefileunit + cfk%QA, *, end = 999) (QAGRD(i), i = 1, NA)
                call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
                ICOUNT = ICOUNT + 1

            case (5)
                call NeedUpdate_clim_data(cm, cfk%QA, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
                QAGRD = cm%clin(cfk%QA)%climv(:, cm%clin(cfk%QA)%itime)
                cm%clin(cfk%QA)%itime = cm%clin(cfk%QA)%itime + 1
                if (cm%clin(cfk%QA)%itime > size(cm%clin(cfk%QA)%climv, 2)) cm%clin(cfk%QA)%itime = 1
                call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
                ICOUNT = ICOUNT + 1

            case default
                print 644, cm%clin(cfk%QA)%id_var, cm%clin(cfk%QA)%filefmt
                stop

        end select !case (cm%clin(cfk%QA)%filefmt)
    end if !(cm%clin(cfk%QA)%timestep_now == 0) then

644 format(/1x'The input forcing file format is not supported', &
        /2x, A15, I4/)

    return

999 ENDDATA = .true.

end subroutine !READ_FORCING_DATA
