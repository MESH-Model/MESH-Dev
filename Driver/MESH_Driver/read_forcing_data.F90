subroutine READ_FORCING_DATA(YCOUNT, XCOUNT, NTYPE, NA, NML, ILG, ILMOS, JLMOS, YYY, XXX, ENDDATA, ACLASS, &
                             FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                             FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, itime, cm)

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

    integer :: itime
    integer :: NTIME  ! time in read sequential

    !> Initialize counting number of r2c and csv files.
    ICOUNT = 0

    !> *****************************************************************
    !> Read shortwave radiation data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_shortwave.bin
    !> *****************************************************************
    if (cm%clin(cfk%FS)%filefmt == 0) then
        read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            FSDOWN(i) = INARRAY(YYY(i), XXX(i))
        end do
        FSVHGRD = 0.5*FSDOWN
        FSIHGRD = FSVHGRD
        call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
        FSIHGAT = FSVHGAT

    !> *****************************************************************
    !> basin_shortwave.r2c
    !> *****************************************************************
    elseif (cm%clin(cfk%FS)%filefmt == 1) then
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

    !> *****************************************************************
    !> basin_shortwave.csv
    !> *****************************************************************
    elseif (cm%clin(cfk%FS)%filefmt == 2) then
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

    !> *****************************************************************
    !> basin_shortwave.seq
    !> *****************************************************************
    elseif (cm%clin(cfk%FS)%filefmt == 3) then
        read(cm%basefileunit + cfk%FS, end = 999) NTIME
        read(cm%basefileunit + cfk%FS, end = 999) FSDOWN
        FSVHGRD = 0.5*FSDOWN
        FSIHGRD = FSVHGRD
        call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
        FSIHGAT = FSVHGAT
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%FS)%filefmt == 4) then
        read(cm%basefileunit + cfk%FS, *, end = 999) (FSDOWN(i), i = 1, NA)
        FSVHGRD = 0.5*FSDOWN
        FSIHGRD = FSVHGRD
        call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
        FSIHGAT = FSVHGAT
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%FS)%filefmt == 5) then
        call NeedUpdate_clim_data(cm, cfk%FS, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        FSDOWN = cm%clin(cfk%FS)%climv(:, cm%clin(cfk%FS)%itime)
        FSVHGRD = 0.5*FSDOWN
        FSIHGRD = FSVHGRD
        call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
        FSIHGAT = FSVHGAT
        ICOUNT = ICOUNT + 1

    else
        print *, cm%clin(cfk%FS)%filefmt, ' SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read longwave radiation data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_longwave.bin
    !> *****************************************************************
    if (cm%clin(cfk%FDL)%filefmt == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            FDLGRD(i) = INARRAY(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)

    !> *****************************************************************
    !> basin_longwave.r2c
    !> *****************************************************************
    elseif (cm%clin(cfk%FDL)%filefmt == 1) then
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

    !> *****************************************************************
    !> basin_longwave.csv
    !> *****************************************************************
    elseif (cm%clin(cfk%FDL)%filefmt == 2) then
        read(cm%basefileunit + cfk%FDL, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
        do i = 1, NML
            CURGRU = JLMOS(i)
            FDLGAT(i) = INVECTOR(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, FDLGRD, FDLGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_longwave.seq
    !> *****************************************************************
    elseif (cm%clin(cfk%FDL)%filefmt == 3) then
        read(cm%basefileunit + cfk%FDL, end = 999) NTIME
        read(cm%basefileunit + cfk%FDL, end = 999) FDLGRD
        call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%FDL)%filefmt == 4) then
        read(cm%basefileunit + cfk%FDL, *, end = 999) (FDLGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%FDL)%filefmt == 5) then
        call NeedUpdate_clim_data(cm, cfk%FDL, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        FDLGRD = cm%clin(cfk%FDL)%climv(:, cm%clin(cfk%FDL)%itime)
        call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, cm%clin(cfk%FDL)%filefmt, ' SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read precipitation data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_rain.bin
    !> *****************************************************************
    if (cm%clin(cfk%PRE)%filefmt == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            PREGRD(i) = INARRAY(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)

    !> *****************************************************************
    !> basin_rain.r2c
    !> *****************************************************************
    elseif (cm%clin(cfk%PRE)%filefmt == 1) then
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

    !> *****************************************************************
    !> basin_rain.csv
    !> *****************************************************************
    elseif (cm%clin(cfk%PRE)%filefmt == 2) then
        read(cm%basefileunit + cfk%PRE, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
        do i = 1,NML
            CURGRU = JLMOS(i)
            PREGAT(i) = INVECTOR(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_rain.seq
    !> *****************************************************************
    elseif (cm%clin(cfk%PRE)%filefmt == 3) then
        read(cm%basefileunit + cfk%PRE, end = 999) NTIME
        read(cm%basefileunit + cfk%PRE, end = 999) PREGRD
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%PRE)%filefmt == 4) then
        read(cm%basefileunit + cfk%PRE, *, end = 999) (PREGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%PRE)%filefmt == 5) then
        call NeedUpdate_clim_data(cm, cfk%PRE, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        PREGRD = cm%clin(cfk%PRE)%climv(:, cm%clin(cfk%PRE)%itime)
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

      !> Read from two sources of rainfall input.
    elseif (cm%clin(cfk%PRE)%filefmt == 6) then
        call NeedUpdate_clim_data(cm, cfk%PRE, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        call NeedUpdate_clim_data(cm, 8, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        PREGRD = cm%clin(8)%alpharain*cm%clin(cfk%PRE)%climv(:, cm%clin(cfk%PRE)%itime) + &
                 (1.0 - cm%clin(8)%alpharain)*cm%clin(8)%climv(:, cm%clin(8)%itime)
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, cm%clin(cfk%PRE)%filefmt, ' SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read temperature data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_temperature.bin
    !> *****************************************************************
    if (cm%clin(cfk%TA)%filefmt == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            TAGRD(i) = INARRAY(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)

    !> *****************************************************************
    !> basin_temperature.r2c
    !> *****************************************************************
    elseif (cm%clin(cfk%TA)%filefmt == 1) then
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

    !> *****************************************************************
    !>  basin_temperature.csv
    !> *****************************************************************
    elseif (cm%clin(cfk%TA)%filefmt == 2) then
        read(cm%basefileunit + cfk%TA, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
        do i = 1, NML
            CURGRU = JLMOS(i)
            TAGAT(i) = INVECTOR(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, TAGRD, TAGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_temperature.seq
    !> *****************************************************************
    elseif (cm%clin(cfk%TA)%filefmt == 3) then
        read(cm%basefileunit + cfk%TA, end = 999) NTIME
        read(cm%basefileunit + cfk%TA, end = 999) TAGRD
        call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%TA)%filefmt == 4) then
        read(cm%basefileunit + cfk%TA, *, end = 999) (TAGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%TA)%filefmt == 5) then
        call NeedUpdate_clim_data(cm, cfk%TA, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        TAGRD = cm%clin(cfk%TA)%climv(:, cm%clin(cfk%TA)%itime)
        call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, cm%clin(cfk%TA)%filefmt, ' SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read wind data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_wind.bin
    !> *****************************************************************
    if (cm%clin(cfk%UL)%filefmt == 0) then !use the forcing bin
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

    !> *****************************************************************
    !> basin_wind.r2c
    !> *****************************************************************
    elseif (cm%clin(cfk%UL)%filefmt == 1) then
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

    !> *****************************************************************
    !> basin_wind.csv
    !> *****************************************************************
    elseif (cm%clin(cfk%UL)%filefmt == 2) then
        read(cm%basefileunit + cfk%UL, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
        do i=1, NML
            CURGRU = JLMOS(i)
            ULGAT(i) = INVECTOR(CURGRU)
        end do
        !VLGRD = 0.0
        !VLGAT = 0.0
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, ULGRD, ULGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_wind.seq
    !> *****************************************************************
    elseif (cm%clin(cfk%UL)%filefmt == 3) then
        read(cm%basefileunit + cfk%UL, end = 999) NTIME
        read(cm%basefileunit + cfk%UL, end = 999) ULGRD
        call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%UL)%filefmt == 4) then
        read(cm%basefileunit + cfk%UL, *, end = 999) (ULGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%UL)%filefmt == 5) then
        call NeedUpdate_clim_data(cm, cfk%UL, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        ULGRD = cm%clin(cfk%UL)%climv(:, cm%clin(cfk%UL)%itime)
        call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, cm%clin(cfk%UL)%filefmt, ' SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read pressure data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_pres.bin
    !> *****************************************************************
    if (cm%clin(cfk%PRES)%filefmt == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            PRESGRD(i) = INARRAY(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)

    !> *****************************************************************
    !> basin_pres.r2c
    !> *****************************************************************
    elseif (cm%clin(cfk%PRES)%filefmt == 1) then
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

    !> *****************************************************************
    !> basin_pres.csv
    !> *****************************************************************
    elseif (cm%clin(cfk%PRES)%filefmt == 2) then
        read(cm%basefileunit + cfk%PRES, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
        do i = 1, NML
            CURGRU = JLMOS(i)
            PRESGAT(i) = INVECTOR(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, PRESGRD, PRESGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_pres.seq
    !> *****************************************************************
    elseif (cm%clin(cfk%PRES)%filefmt == 3) then
        read(cm%basefileunit + cfk%PRES, end = 999) NTIME
        read(cm%basefileunit + cfk%PRES, end = 999) PRESGRD
        call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%PRES)%filefmt == 4) then
        read(cm%basefileunit + cfk%PRES, *, end = 999) (PRESGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%PRES)%filefmt == 5) then
        call NeedUpdate_clim_data(cm, cfk%PRES, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        PRESGRD = cm%clin(cfk%PRES)%climv(:, cm%clin(cfk%PRES)%itime)
        call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, cm%clin(cfk%PRES)%filefmt, ' SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read humidity data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_humidity.bin
    !> *****************************************************************
    if (cm%clin(cfk%QA)%filefmt == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((INARRAY(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            QAGRD(i) = INARRAY(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)

    !> *****************************************************************
    !> basin_humidity.r2c
    !> *****************************************************************
    elseif (cm%clin(cfk%QA)%filefmt == 1) then
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

    !> *****************************************************************
    !> basin_humidity.csv
    !> *****************************************************************
    elseif (cm%clin(cfk%QA)%filefmt == 2) then
        read(cm%basefileunit + cfk%QA, *, end = 999) (INVECTOR(i), i = 1, NTYPE)
        do i = 1, NML
            CURGRU = JLMOS(i)
            QAGAT(i) = INVECTOR(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, QAGRD, QAGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_humidity.seq
    !> *****************************************************************
    elseif (cm%clin(cfk%QA)%filefmt == 3) then
        read(cm%basefileunit + cfk%QA, end = 999) NTIME
        read(cm%basefileunit + cfk%QA, end = 999) QAGRD
        call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_humidity.asc
    !> *****************************************************************
    elseif (cm%clin(cfk%QA)%filefmt == 4) then
        read(cm%basefileunit + cfk%QA, *, end = 999) (QAGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
        ICOUNT = ICOUNT + 1

    elseif (cm%clin(cfk%QA)%filefmt == 5) then
        call NeedUpdate_clim_data(cm, cfk%QA, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        QAGRD = cm%clin(cfk%QA)%climv(:, cm%clin(cfk%QA)%itime)
        call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, cm%clin(cfk%QA)%filefmt, ' SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    return

999 ENDDATA = .true.

end subroutine !READ_FORCING_DATA
