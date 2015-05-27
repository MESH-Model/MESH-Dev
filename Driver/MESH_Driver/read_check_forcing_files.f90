subroutine READ_CHECK_FORCING_FILES(NA, cm, ts)

    use FLAGS
    use climate_forcing
    use model_dates

    implicit none

    integer NA

    !> IOS is passed when the file is opened. The program will stop if IOS /= 0.
    integer IOS

    !> This variable is used to skip the header of r2c files.
    character(80) end_of_r2c_header

    type(clim_info) :: cm
    type(dates_model) :: ts

    !> Reset the number of forcing variables not in the forcing binary
    !> file.
    NUM_R2C = 0
    NUM_CSV = 0
    NUM_SEQ = 0

!todo change documentation to reflect that all 3 types of forcing files can be used

    !> *****************************************************************
    !> Open shortwave radiation data.
    !> *****************************************************************

    if (BASINSHORTWAVEFLAG == 1) then
        open(90, file = 'basin_shortwave.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_shortwave.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the r2c file in the correct location.'
            close(90)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_shortwave.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(90, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (BASINSHORTWAVEFLAG == 2) then
        open(90, file = 'basin_shortwave.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_shortwave.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the csv file in the correct location.'
            close(90)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_shortwave.csv found'
        end if

    elseif (BASINSHORTWAVEFLAG == 3) then
        open(90, file = 'basin_shortwave.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_shortwave.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(90)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_shortwave.seq found'
        end if

    elseif (BASINSHORTWAVEFLAG == 4) then
        open(90, file = 'basin_shortwave.asc', action = 'read', status = 'old', form = 'formatted', iostat = IOS)

    elseif (BASINSHORTWAVEFLAG == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call INIT_CLIM_INFO(cm, ts, 1, NA)
        call INIT_CLIM_DATA(cm, 'shortwave', 90)

    end if

    !> *****************************************************************
    !> Open longwave radiation data.
    !> *****************************************************************
    if (BASINLONGWAVEFLAG == 1) then
        open(91, file = 'basin_longwave.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_longwave.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2c file in the correct location.'
            close(91)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_longwave.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(91, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (BASINLONGWAVEFLAG == 2) then
        open(91, file = 'basin_longwave.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_longwave.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(91)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_longwave.csv found'
        end if

    elseif (BASINLONGWAVEFLAG == 3) then
        open(91, file = 'basin_longwave.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_longwave.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(91)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_longwave.seq found'
        end if

    elseif (BASINLONGWAVEFLAG == 4) then
        open(91, file = 'basin_longwave.asc', action = 'read', status = 'old', form = 'formatted', iostat = IOS)

    elseif (BASINLONGWAVEFLAG == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call INIT_CLIM_INFO(cm, ts, 2, NA)
        call INIT_CLIM_DATA(cm, 'longwave', 91)

    end if

    !> *****************************************************************
    !> Open precipitation data.
    !> *****************************************************************
    if (BASINRAINFLAG == 1) then
        open(92, file = 'basin_rain.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_rain.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the r2c file in the correct location.'
            close(92)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_rain.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(92, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (BASINRAINFLAG == 2) then
        open(92, file = 'basin_rain.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_rain.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the csv file in the correct location.'
            close(92)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_rain.csv found'
        end if

    elseif (BASINRAINFLAG == 3) then
        open(92, file = 'basin_rain.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_rain.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(92)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_rain.seq found'
        end if

    elseif (BASINRAINFLAG == 4) then
        open(92, file = 'basin_rain.asc', action = 'read', status = 'old', form = 'formatted', iostat = IOS)

    elseif (BASINRAINFLAG == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call INIT_CLIM_INFO(cm, ts, 3, NA)
        call INIT_CLIM_DATA(cm, 'rain', 92)

    elseif (BASINRAINFLAG == 6) then
        NUM_SEQ = NUM_SEQ + 1
        call INIT_CLIM_INFO(cm, ts, 3, NA)
        call INIT_CLIM_DATA(cm, 'rain', 921)
        call INIT_CLIM_INFO(cm, ts, 8, NA)
        call INIT_CLIM_DATA(cm, 'rain_2', 922)

    end if

    !> *****************************************************************
    !> Open temperature data.
    !> *****************************************************************
    if (BASINTEMPERATUREFLAG == 1) then
        open(93, file = 'basin_temperature.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_temperature.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2c file in the correct location.'
            close(93)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_temperature.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(93, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (BASINTEMPERATUREFLAG == 2) then
        open(93, file = 'basin_temperature.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_temperature.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(93)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_temperature.csv found'
        end if

    elseif (BASINTEMPERATUREFLAG == 3) then
        open(93, file = 'basin_temperature.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_temperature.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(93)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_temperature.seq found'
        end if

    elseif (BASINTEMPERATUREFLAG == 4) then
        open(93, file = 'basin_temperature.asc', action = 'read', status = 'old', form = 'formatted', iostat = IOS)

    elseif (BASINTEMPERATUREFLAG == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call INIT_CLIM_INFO(cm, ts, 4, NA)
        call INIT_CLIM_DATA(cm, 'temp', 93)

    end if

    !> *****************************************************************
    !> Open wind data.
    !> *****************************************************************
    if (BASINWINDFLAG == 1) then
        open(94, file = 'basin_wind.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_wind.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2c file in the correct location.'
            close(94)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_wind.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(94, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (BASINWINDFLAG == 2) then
        open(94, file = 'basin_wind.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_wind.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(94)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_wind.csv found'
        end if

      elseif (BASINWINDFLAG == 3) then
        open(94, file = 'basin_wind.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_wind.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(94)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_wind.seq found'
        end if

    elseif (BASINWINDFLAG == 4) then
        open(94, file = 'basin_wind.asc', action = 'read', status = 'old', form = 'formatted', iostat = IOS)

    elseif (BASINWINDFLAG == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call INIT_CLIM_INFO(cm, ts, 5, NA)
        call INIT_CLIM_DATA(cm, 'wind', 94)

    end if

    !> *****************************************************************
    !> Open pressure data.
    !> *****************************************************************
    if (BASINPRESFLAG == 1) then
        open(95, file = 'basin_pres.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_pres.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2cfile in the correct location.'
            close(95)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_pres.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(95, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (BASINPRESFLAG == 2) then
        open(95, file = 'basin_pres.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_pres.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(95)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_pres.csv found'
        end if

    elseif (BASINPRESFLAG == 3) then
        open(95, file = 'basin_pres.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_pres.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(95)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_pres.seq found'
        end if

    elseif (BASINPRESFLAG == 4) then
        open(95, file = 'basin_pres.asc', action = 'read', status = 'old', form = 'formatted', iostat = IOS)

    elseif (BASINPRESFLAG == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call INIT_CLIM_INFO(cm, ts, 6, NA)
        call INIT_CLIM_DATA(cm, 'pressure', 95)

    end if

    !> *****************************************************************
    !> Open humidity data.
    !> *****************************************************************
    if (BASINHUMIDITYFLAG == 1) then
        open(96, file = 'basin_humidity.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_humidity.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2c file in the correct location.'
            close(96)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_humidity.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(96, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (BASINHUMIDITYFLAG == 2) then
        open(96, file = 'basin_humidity.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_humidity.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(96)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_humidity.csv found'
        end if

    elseif (BASINHUMIDITYFLAG == 3) then
        open(96, file = 'basin_humidity.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_humidity.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(96)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_humidity.seq found'
        end if

    elseif (BASINHUMIDITYFLAG == 4) then
        open(96, file = 'basin_humidity.asc', action = 'read', status = 'old', form = 'formatted', iostat = IOS)

    elseif (BASINHUMIDITYFLAG == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call INIT_CLIM_INFO(cm, ts, 7, NA)
        call INIT_CLIM_DATA(cm, 'humidity', 96)

    end if

    return

end subroutine !READ_CHECK_FORCING_FILES
