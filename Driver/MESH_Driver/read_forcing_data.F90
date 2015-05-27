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
    use climate_forcing, only: clim_info, NeedUpdate_clim_data

    implicit none

    integer YCOUNT, XCOUNT, NTYPE, NA, NML, ILG
    logical ENDDATA

    real*4, dimension(YCOUNT, XCOUNT) :: R4SHRTGRID2D, R4LONGGRID2D, R4RAINGRID2D, R4TEMPGRID2D, &
                                         R4WINDGRID2D, R4PRESGRID2D, R4HUMDGRID2D
    real*4, dimension(NA, NTYPE) :: ACLASS
    real*4, dimension(NTYPE) :: R4SHRTGRU, R4LONGGRU, R4RAINGRU, R4TEMPGRU, R4WINDGRU, R4PRESGRU, R4HUMDGRU
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
    if (BASINSHORTWAVEFLAG == 0) then
        read(51, end = 999) ((R4SHRTGRID2D(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            FSDOWN(i) = R4SHRTGRID2D(YYY(i), XXX(i))
        end do
        FSVHGRD = 0.5*FSDOWN
        FSIHGRD = FSVHGRD
        call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
        FSIHGAT = FSVHGAT

    !> *****************************************************************
    !> basin_shortwave.r2c
    !> *****************************************************************
    elseif (BASINSHORTWAVEFLAG == 1) then
        read(90, end = 999) !:Frame line
        do i = 1, YCOUNT
            read(90, end = 999) (R4SHRTGRID2D(i, j), j = 1, XCOUNT)
        end do
        read(90, end = 999) !:EndFrame line
        do i = 1, NA
            FSDOWN(i) = R4SHRTGRID2D(YYY(i), XXX(i))
        end do
        FSVHGRD = 0.5*FSDOWN
        FSIHGRD = FSVHGRD
        call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
        FSIHGAT = FSVHGAT
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_shortwave.csv
    !> *****************************************************************
    elseif (BASINSHORTWAVEFLAG == 2) then
        read(90, end = 999) (R4SHRTGRU(i), i = 1, NTYPE)
        do i = 1, NML
            CURGRU = JLMOS(i)
            FSVHGAT(i) = 0.5*R4SHRTGRU(CURGRU)
        end do
        FSIHGAT = FSVHGAT
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, FSVHGRD, FSVHGAT)
        FSDOWN = 2.0*FSVHGRD
        FSIHGRD = FSVHGRD
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_shortwave.seq
    !> *****************************************************************
    elseif (BASINSHORTWAVEFLAG == 3) then
        read(90, end = 999) NTIME
        read(90, end = 999) FSDOWN
        FSVHGRD = 0.5*FSDOWN
        FSIHGRD = FSVHGRD
        call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
        FSIHGAT = FSVHGAT
        ICOUNT = ICOUNT + 1

    elseif (BASINSHORTWAVEFLAG == 4) then
        read(90, end = 999) (FSDOWN(i), i = 1, NA)
        FSVHGRD = 0.5*FSDOWN
        FSIHGRD = FSVHGRD
        call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
        FSIHGAT = FSVHGAT
        ICOUNT = ICOUNT + 1

    elseif (BASINSHORTWAVEFLAG == 5) then
        call NeedUpdate_clim_data(cm, 1, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        FSDOWN = cm%clin(1)%climv(:, cm%clin(1)%itime)
        FSVHGRD = 0.5*FSDOWN
        FSIHGRD = FSVHGRD
        call GATHER(NA, NML, ILG, ILMOS, FSVHGRD, FSVHGAT)
        FSIHGAT = FSVHGAT
        ICOUNT = ICOUNT + 1

    else
        print *, 'BASINSHORTWAVEFLAG SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read longwave radiation data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_longwave.bin
    !> *****************************************************************
    if (BASINLONGWAVEFLAG == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((R4LONGGRID2D(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            FDLGRD(i) = R4LONGGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)

    !> *****************************************************************
    !> basin_longwave.r2c
    !> *****************************************************************
    elseif (BASINLONGWAVEFLAG == 1) then
        read(91, end = 999) !:Frame line
        do i = 1, YCOUNT
            read(91, end = 999) (R4LONGGRID2D(i, j), j = 1, XCOUNT)
        end do
        read(91, end = 999) !:EndFrame line
        do i = 1, NA
            FDLGRD(i) = R4LONGGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_longwave.csv
    !> *****************************************************************
    elseif (BASINLONGWAVEFLAG == 2) then
        read(91, end = 999) (R4LONGGRU(i), i = 1, NTYPE)
        do i = 1, NML
            CURGRU = JLMOS(i)
            FDLGAT(i) = R4LONGGRU(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, FDLGRD, FDLGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_longwave.seq
    !> *****************************************************************
    elseif (BASINLONGWAVEFLAG == 3) then
        read(91, end = 999) NTIME
        read(91, end = 999) FDLGRD
        call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINLONGWAVEFLAG == 4) then
        read(91, end = 999) (FDLGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINLONGWAVEFLAG == 5) then
        call NeedUpdate_clim_data(cm, 2, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        FDLGRD = cm%clin(2)%climv(:, cm%clin(2)%itime)
        call GATHER(NA, NML, ILG, ILMOS, FDLGRD, FDLGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, 'BASINLONGWAVEFLAG SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read precipitation data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_rain.bin
    !> *****************************************************************
    if (BASINRAINFLAG == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((R4RAINGRID2D(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            PREGRD(i) = R4RAINGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)

    !> *****************************************************************
    !> basin_rain.r2c
    !> *****************************************************************
    elseif (BASINRAINFLAG == 1) then
        read(92, end = 999) !:Frame line
        do i = 1, YCOUNT
            read (92, end = 999) (R4RAINGRID2D(i, j), j = 1, XCOUNT)
        end do
        read(92, end = 999) !:EndFrame line
        do i = 1, NA
            PREGRD(i) = R4RAINGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_rain.csv
    !> *****************************************************************
    elseif (BASINRAINFLAG == 2) then
        read(92, end = 999) (R4RAINGRU(i), i = 1, NTYPE)
        do i = 1,NML
            CURGRU = JLMOS(i)
            PREGAT(i) = R4RAINGRU(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_rain.seq
    !> *****************************************************************
    elseif (BASINRAINFLAG == 3) then
        read(92, end = 999) NTIME
        read(92, end = 999) PREGRD
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINRAINFLAG == 4) then
        read(92, end = 999) (PREGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINRAINFLAG == 5) then
        call NeedUpdate_clim_data(cm, 3, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        PREGRD = cm%clin(3)%climv(:, cm%clin(3)%itime)
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

      !> Read from two sources of rainfall input.
    elseif (BASINRAINFLAG == 6) then
        call NeedUpdate_clim_data(cm, 3, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        call NeedUpdate_clim_data(cm, 8, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        PREGRD = cm%clin(8)%alpharain*cm%clin(3)%climv(:, cm%clin(3)%itime) + &
                 (1.0 - cm%clin(8)%alpharain)*cm%clin(8)%climv(:, cm%clin(8)%itime)
        call GATHER(NA, NML, ILG, ILMOS, PREGRD, PREGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, 'BASINRAINFLAG SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read temperature data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_temperature.bin
    !> *****************************************************************
    if (BASINTEMPERATUREFLAG == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((R4TEMPGRID2D(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            TAGRD(i) = R4TEMPGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)

    !> *****************************************************************
    !> basin_temperature.r2c
    !> *****************************************************************
    elseif (BASINTEMPERATUREFLAG == 1) then
        read(93, end = 999) !:Frame line
        do i = 1, YCOUNT
            read(93, end = 999) (R4TEMPGRID2D(i, j), j = 1, XCOUNT)
        end do
        read(93, end = 999) !:EndFrame line
        do i = 1, NA
            TAGRD(i) = R4TEMPGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !>  basin_temperature.csv
    !> *****************************************************************
    elseif (BASINTEMPERATUREFLAG == 2) then
        read(93, end = 999) (R4TEMPGRU(i), i = 1, NTYPE)
        do i = 1, NML
            CURGRU = JLMOS(i)
            TAGAT(i) = R4TEMPGRU(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, TAGRD, TAGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_temperature.seq
    !> *****************************************************************
    elseif (BASINTEMPERATUREFLAG == 3) then
        read(93, end = 999) NTIME
        read(93, end = 999) TAGRD
        call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINTEMPERATUREFLAG == 4) then
        read(93, end = 999) (TAGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINTEMPERATUREFLAG == 5) then
        call NeedUpdate_clim_data(cm, 4, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        TAGRD = cm%clin(4)%climv(:, cm%clin(4)%itime)
        call GATHER(NA, NML, ILG, ILMOS, TAGRD, TAGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, 'BASINTEMPERATUREFLAG SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read wind data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_wind.bin
    !> *****************************************************************
    if (BASINWINDFLAG == 0) then !use the forcing bin
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((R4WINDGRID2D(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            ULGRD(i) = R4WINDGRID2D(YYY(i), XXX(i))
        end do
        !VLGRD = 0.0
        !VLGAT = 0.0
        !UVGRD = max(VMIN, ULGRD)
        call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)

    !> *****************************************************************
    !> basin_wind.r2c
    !> *****************************************************************
    elseif (BASINWINDFLAG == 1) then
        read(94, end = 999) !:Frame line
        do i = 1, YCOUNT
            read(94, end = 999) (R4WINDGRID2D(i, j), j = 1, XCOUNT)
        end do
        read(94, end = 999) !:EndFrame line
        do i = 1, NA
            ULGRD(i) = R4WINDGRID2D(YYY(i), XXX(i))
        end do
        !VLGRD = 0.0
        !VLGAT = 0.0
        !UVGRD = max(VMIN, ULGRD)
        call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_wind.csv
    !> *****************************************************************
    elseif (BASINWINDFLAG == 2) then
        read(94, end = 999) (R4WINDGRU(i), i = 1, NTYPE)
        do i=1, NML
            CURGRU = JLMOS(i)
            ULGAT(i) = R4WINDGRU(CURGRU)
        end do
        !VLGRD = 0.0
        !VLGAT = 0.0
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, ULGRD, ULGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_wind.seq
    !> *****************************************************************
    elseif (BASINWINDFLAG == 3) then
        read(94, end = 999) NTIME
        read(94, end = 999) ULGRD
        call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINWINDFLAG == 4) then
        read(94, end = 999) (ULGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINWINDFLAG == 5) then
        call NeedUpdate_clim_data(cm, 5, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        ULGRD = cm%clin(5)%climv(:, cm%clin(5)%itime)
        call GATHER(NA, NML, ILG, ILMOS, ULGRD, ULGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, 'BASINWINDFLAG SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read pressure data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_pres.bin
    !> *****************************************************************
    if (BASINPRESFLAG == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((R4PRESGRID2D(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            PRESGRD(i) = R4PRESGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)

    !> *****************************************************************
    !> basin_pres.r2c
    !> *****************************************************************
    elseif (BASINPRESFLAG == 1) then
        read(95, end = 999) !:Frame line
        do i = 1, YCOUNT
            read(95, end = 999) (R4PRESGRID2D(i, j), j = 1, XCOUNT)
        end do
        read(95, end = 999) !:EndFrame line
        do i = 1, NA
            PRESGRD(i) = R4PRESGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_pres.csv
    !> *****************************************************************
    elseif (BASINPRESFLAG == 2) then
        read(95, end = 999) (R4PRESGRU(i), i = 1, NTYPE)
        do i = 1, NML
            CURGRU = JLMOS(i)
            PRESGAT(i) = R4PRESGRU(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, PRESGRD, PRESGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_pres.seq
    !> *****************************************************************
    elseif (BASINPRESFLAG == 3) then
        read(95, end = 999) NTIME
        read(95, end = 999) PRESGRD
        call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINPRESFLAG == 4) then
        read(95, end = 999) (PRESGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINPRESFLAG == 5) then
        call NeedUpdate_clim_data(cm, 6, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        PRESGRD = cm%clin(6)%climv(:, cm%clin(6)%itime)
        call GATHER(NA, NML, ILG, ILMOS, PRESGRD, PRESGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, 'BASINPRESSUREFLAG SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    !> *****************************************************************
    !> Read humidity data
    !> *****************************************************************

    !> *****************************************************************
    !> basin_humidity.bin
    !> *****************************************************************
    if (BASINHUMIDITYFLAG == 0) then
        !> Skip the forcing data that is read from r2c and csv files.
        do k = 1, ICOUNT
            read(51, end = 999) ((JUNK, j = 1, XCOUNT), i = 1, YCOUNT)
        end do
        read(51, end = 999) ((R4HUMDGRID2D(i, j), j = 1, XCOUNT), i = 1, YCOUNT)
        do i = 1, NA
            QAGRD(i) = R4HUMDGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)

    !> *****************************************************************
    !> basin_humidity.r2c
    !> *****************************************************************
    elseif (BASINHUMIDITYFLAG == 1) then
        read(96, end = 999) !:Frame line
        do i = 1, YCOUNT
            read(96, end = 999) (R4HUMDGRID2D(i, j), j = 1, XCOUNT)
        end do
        read (96, end = 999) !:EndFrame line
        do i = 1, NA
            QAGRD(i) = R4HUMDGRID2D(YYY(i), XXX(i))
        end do
        call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_humidity.csv
    !> *****************************************************************
    elseif (BASINHUMIDITYFLAG == 2) then
        read(96, end = 999) (R4HUMDGRU(i), i = 1, NTYPE)
        do i = 1, NML
            CURGRU = JLMOS(i)
            QAGAT(i) = R4HUMDGRU(CURGRU)
        end do
        call SCATTER(NTYPE, NA, NML, ILMOS, JLMOS, ACLASS, QAGRD, QAGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_humidity.seq
    !> *****************************************************************
    elseif (BASINHUMIDITYFLAG == 3) then
        read(96, end = 999) NTIME
        read(96, end = 999) QAGRD
        call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
        ICOUNT = ICOUNT + 1

    !> *****************************************************************
    !> basin_humidity.asc
    !> *****************************************************************
    elseif (BASINHUMIDITYFLAG == 4) then
        read(96, end = 999) (QAGRD(i), i = 1, NA)
        call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
        ICOUNT = ICOUNT + 1

    elseif (BASINHUMIDITYFLAG == 5) then
        call NeedUpdate_clim_data(cm, 7, itime, XCOUNT, YCOUNT, XXX, YYY, NA, ENDDATA)
        QAGRD = cm%clin(7)%climv(:, cm%clin(7)%itime)
        call GATHER(NA, NML, ILG, ILMOS, QAGRD, QAGAT)
        ICOUNT = ICOUNT + 1

    else
        print *, 'BASINHUMIDITYFLAG SHOULD BE EITHER 0, 1 0R 2'
        stop
    end if

    return

999 ENDDATA = .true.

end subroutine !READ_FORCING_DATA
