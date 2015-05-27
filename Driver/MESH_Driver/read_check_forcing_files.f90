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

    if (cm%clin(cfk%FS)%filefmt == 1) then
        open(cm%basefileunit + cfk%FS, file = 'basin_shortwave.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_shortwave.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the r2c file in the correct location.'
            close(cm%basefileunit + cfk%FS)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_shortwave.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(cm%basefileunit + cfk%FS, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (cm%clin(cfk%FS)%filefmt == 2) then
        open(cm%basefileunit + cfk%FS, file = 'basin_shortwave.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_shortwave.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the csv file in the correct location.'
            close(cm%basefileunit + cfk%FS)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_shortwave.csv found'
        end if

    elseif (cm%clin(cfk%FS)%filefmt == 3) then
        open(cm%basefileunit + cfk%FS, file = 'basin_shortwave.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_shortwave.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(cm%basefileunit + cfk%FS)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_shortwave.seq found'
        end if

    elseif (cm%clin(cfk%FS)%filefmt == 4) then
        open(cm%basefileunit + cfk%FS, file = 'basin_shortwave.asc', action = 'read', status = 'old', form = 'formatted', &
             iostat = IOS)

    elseif (cm%clin(cfk%FS)%filefmt == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call Init_clim_info(cm, ts, cfk%FS, NA)
        call Init_clim_data(cm, 'shortwave', cm%basefileunit + cfk%FS)

    end if

    !> *****************************************************************
    !> Open longwave radiation data.
    !> *****************************************************************
    if (cm%clin(cfk%FDL)%filefmt == 1) then
        open(cm%basefileunit + cfk%FDL, file = 'basin_longwave.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_longwave.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2c file in the correct location.'
            close(cm%basefileunit + cfk%FDL)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_longwave.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(cm%basefileunit + cfk%FDL, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (cm%clin(cfk%FDL)%filefmt == 2) then
        open(cm%basefileunit + cfk%FDL, file = 'basin_longwave.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_longwave.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(cm%basefileunit + cfk%FDL)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_longwave.csv found'
        end if

    elseif (cm%clin(cfk%FDL)%filefmt == 3) then
        open(cm%basefileunit + cfk%FDL, file = 'basin_longwave.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_longwave.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(cm%basefileunit + cfk%FDL)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_longwave.seq found'
        end if

    elseif (cm%clin(cfk%FDL)%filefmt == 4) then
        open(cm%basefileunit + cfk%FDL, file = 'basin_longwave.asc', action = 'read', status = 'old', form = 'formatted', &
             iostat = IOS)

    elseif (cm%clin(cfk%FDL)%filefmt == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call Init_clim_info(cm, ts, cfk%FDL, NA)
        call Init_clim_data(cm, 'longwave', cm%basefileunit + cfk%FDL)

    end if

    !> *****************************************************************
    !> Open precipitation data.
    !> *****************************************************************
    if (cm%clin(cfk%PRE)%filefmt == 1) then
        open(cm%basefileunit + cfk%PRE, file = 'basin_rain.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_rain.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the r2c file in the correct location.'
            close(cm%basefileunit + cfk%PRE)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_rain.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(cm%basefileunit + cfk%PRE, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (cm%clin(cfk%PRE)%filefmt == 2) then
        open(cm%basefileunit + cfk%PRE, file = 'basin_rain.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_rain.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the csv file in the correct location.'
            close(cm%basefileunit + cfk%PRE)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_rain.csv found'
        end if

    elseif (cm%clin(cfk%PRE)%filefmt == 3) then
        open(cm%basefileunit + cfk%PRE, file = 'basin_rain.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_rain.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(cm%basefileunit + cfk%PRE)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_rain.seq found'
        end if

    elseif (cm%clin(cfk%PRE)%filefmt == 4) then
        open(cm%basefileunit + cfk%PRE, file = 'basin_rain.asc', action = 'read', status = 'old', form = 'formatted', &
             iostat = IOS)

    elseif (cm%clin(cfk%PRE)%filefmt == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call Init_clim_info(cm, ts, cfk%PRE, NA)
        call Init_clim_data(cm, 'rain', cm%basefileunit + cfk%PRE)

    elseif (cm%clin(cfk%PRE)%filefmt == 6) then
        NUM_SEQ = NUM_SEQ + 1
        call Init_clim_info(cm, ts, cfk%PRE, NA)
        call Init_clim_data(cm, 'rain', 921)
        call Init_clim_info(cm, ts, 8, NA)
        call Init_clim_data(cm, 'rain_2', 922)

    end if

    !> *****************************************************************
    !> Open temperature data.
    !> *****************************************************************
    if (cm%clin(cfk%TA)%filefmt == 1) then
        open(cm%basefileunit + cfk%TA, file = 'basin_temperature.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_temperature.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2c file in the correct location.'
            close(cm%basefileunit + cfk%TA)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_temperature.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(cm%basefileunit + cfk%TA, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (cm%clin(cfk%TA)%filefmt == 2) then
        open(cm%basefileunit + cfk%TA, file = 'basin_temperature.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_temperature.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(cm%basefileunit + cfk%TA)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_temperature.csv found'
        end if

    elseif (cm%clin(cfk%TA)%filefmt == 3) then
        open(cm%basefileunit + cfk%TA, file = 'basin_temperature.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_temperature.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(cm%basefileunit + cfk%TA)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_temperature.seq found'
        end if

    elseif (cm%clin(cfk%TA)%filefmt == 4) then
        open(cm%basefileunit + cfk%TA, file = 'basin_temperature.asc', action = 'read', status = 'old', form = 'formatted', &
             iostat = IOS)

    elseif (cm%clin(cfk%TA)%filefmt == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call Init_clim_info(cm, ts, cfk%TA, NA)
        call Init_clim_data(cm, 'temp', cm%basefileunit + cfk%TA)

    end if

    !> *****************************************************************
    !> Open wind data.
    !> *****************************************************************
    if (cm%clin(cfk%UL)%filefmt == 1) then
        open(cm%basefileunit + cfk%UL, file = 'basin_wind.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_wind.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2c file in the correct location.'
            close(cm%basefileunit + cfk%UL)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_wind.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(cm%basefileunit + cfk%UL, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (cm%clin(cfk%UL)%filefmt == 2) then
        open(cm%basefileunit + cfk%UL, file = 'basin_wind.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_wind.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(cm%basefileunit + cfk%UL)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_wind.csv found'
        end if

      elseif (cm%clin(cfk%UL)%filefmt == 3) then
        open(cm%basefileunit + cfk%UL, file = 'basin_wind.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_wind.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(cm%basefileunit + cfk%UL)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_wind.seq found'
        end if

    elseif (cm%clin(cfk%UL)%filefmt == 4) then
        open(cm%basefileunit + cfk%UL, file = 'basin_wind.asc', action = 'read', status = 'old', form = 'formatted', iostat = IOS)

    elseif (cm%clin(cfk%UL)%filefmt == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call Init_clim_info(cm, ts, cfk%UL, NA)
        call Init_clim_data(cm, 'wind', cm%basefileunit + cfk%UL)

    end if

    !> *****************************************************************
    !> Open pressure data.
    !> *****************************************************************
    if (cm%clin(cfk%PRES)%filefmt == 1) then
        open(cm%basefileunit + cfk%PRES, file = 'basin_pres.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_pres.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2cfile in the correct location.'
            close(cm%basefileunit + cfk%PRES)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_pres.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(cm%basefileunit + cfk%PRES, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (cm%clin(cfk%PRES)%filefmt == 2) then
        open(cm%basefileunit + cfk%PRES, file = 'basin_pres.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_pres.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(cm%basefileunit + cfk%PRES)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_pres.csv found'
        end if

    elseif (cm%clin(cfk%PRES)%filefmt == 3) then
        open(cm%basefileunit + cfk%PRES, file = 'basin_pres.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_pres.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(cm%basefileunit + cfk%PRES)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_pres.seq found'
        end if

    elseif (cm%clin(cfk%PRES)%filefmt == 4) then
        open(cm%basefileunit + cfk%PRES, file = 'basin_pres.asc', action = 'read', status = 'old', form = 'formatted', iostat = IOS)

    elseif (cm%clin(cfk%PRES)%filefmt == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call Init_clim_info(cm, ts, cfk%PRES, NA)
        call Init_clim_data(cm, 'pressure', cm%basefileunit + cfk%PRES)

    end if

    !> *****************************************************************
    !> Open humidity data.
    !> *****************************************************************
    if (cm%clin(cfk%QA)%filefmt == 1) then
        open(cm%basefileunit + cfk%QA, file = 'basin_humidity.r2c', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_humidity.r2c not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the r2c file in the correct location.'
            close(cm%basefileunit + cfk%QA)
            stop
        else
            NUM_R2C = NUM_R2C + 1
            print *, 'basin_humidity.r2c found'
            end_of_r2c_header = ''
            do while (end_of_r2c_header /= ":endHeader")
                read(cm%basefileunit + cfk%QA, '(A10)') end_of_r2c_header
            end do
        end if

    elseif (cm%clin(cfk%QA)%filefmt == 2) then
        open(cm%basefileunit + cfk%QA, file = 'basin_humidity.csv', action = 'read', status = 'old', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_humidity.csv not found'
            print *, 'please adjust the mesh_input_run_options.ini file,'
            print *, 'or put the csv file in the correct location.'
            close(cm%basefileunit + cfk%QA)
            stop
        else
            NUM_CSV = NUM_CSV + 1
            print *, 'basin_humidity.csv found'
        end if

    elseif (cm%clin(cfk%QA)%filefmt == 3) then
        open(cm%basefileunit + cfk%QA, file = 'basin_humidity.seq', action = 'read', status = 'old', form = 'unformatted', &
             access = 'sequential', iostat = IOS)
        if (IOS /= 0) then
            print *, 'basin_humidity.seq not found'
            print *, 'please adjust the mesh_input_run_options.ini file'
            print *, 'or put the seq file in the correct location.'
            close(cm%basefileunit + cfk%QA)
            stop
        else
            NUM_SEQ = NUM_SEQ + 1
            print *, 'basin_humidity.seq found'
        end if

    elseif (cm%clin(cfk%QA)%filefmt == 4) then
        open(cm%basefileunit + cfk%QA, file = 'basin_humidity.asc', action = 'read', status = 'old', form = 'formatted', &
             iostat = IOS)

    elseif (cm%clin(cfk%QA)%filefmt == 5) then
        NUM_SEQ = NUM_SEQ + 1
        call Init_clim_info(cm, ts, cfk%QA, NA)
        call Init_clim_data(cm, 'humidity', cm%basefileunit + cfk%QA)

    end if

    return

end subroutine !READ_CHECK_FORCING_FILES
