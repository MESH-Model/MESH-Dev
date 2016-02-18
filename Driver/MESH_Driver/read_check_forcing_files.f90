!> *********************************************************************
!> Subroutine to initialize the climate forcing input files.
!> *********************************************************************
subroutine READ_CHECK_FORCING_FILES(shd, ts, cm)

    use sa_mesh_shared_variabletypes
    use model_dates
    use climate_forcing_variabletypes
    use climate_forcing_variables
    use climate_forcing_config, only: open_config
    use climate_forcing

    implicit none
!todo change documentation to reflect that all 3 types of forcing files can be used

    !> *****************************************************************
    !> Input variables.
    !> *****************************************************************

    !* shd: Basin information (from shed file).
    type(ShedGridParams) :: shd

    !* ts: Time-step and time-series information of the simulation.
    type(dates_model) :: ts

    !> *****************************************************************
    !> Input/Output variables.
    !> *****************************************************************

    !* cm: Climate information and data variable.
    type(clim_info) :: cm

    !> *****************************************************************
    !> Local variables.
    !> *****************************************************************

    !* i: Index
    integer i

    !> Local variables.
    integer nts, rts, timeStepClimF

    !> Default configuration.
    do i = 1, size(cm%clin)

        !> Set the unit number and allocate the default number of source files.
        cm%clin(i)%unitR = cm%basefileunit + i
        allocate(cm%clin(i)%name(1))

        !> Allocate and initialize the alpha coefficient for the default series.
!        allocate(cm%clin(i)%alpha(cm%clin(i)%nSeries))
!        cm%clin(i)%alpha = 1.0 / cm%clin(i)%nSeries

        !> Allocate the gridded series.
        allocate(cm%clin(i)%GRD(shd%NA), cm%clin(i)%GAT(shd%lc%NML))

        !> Determine the number of time-steps in the run.
!todo: This doesn't work if run start and stop days are set to zeros;
!todo: The above should be reflected in module_dates where nr_days is determined;
        timeStepClimF = ts%nr_days*24*(60/TIME_STEP_MINS)/real(cm%clin(i)%hf)*TIME_STEP_MINS

        !> Determine the time-stepping if data is read into memory.
        if (timeStepClimF <= cm%clin(i)%timeSize) then
            allocate(cm%clin(i)%ntimes(1))
            cm%clin(i)%ntimes(1) = timeStepClimF
        else
            nts = timeStepClimF/cm%clin(i)%timeSize
            rts = timeStepClimF - cm%clin(i)%timeSize*nts
            if (rts == 0) then
                allocate(cm%clin(i)%ntimes(nts))
                cm%clin(i)%ntimes = cm%clin(i)%timeSize
            else
                allocate(cm%clin(i)%ntimes(nts + 1))
                cm%clin(i)%ntimes = cm%clin(i)%timeSize
                cm%clin(i)%ntimes(nts + 1) = rts
            end if
        end if

    end do

    !> Set the default file name.
    cm%clin(cfk%FB)%name(1) = 'basin_shortwave'
    cm%clin(cfk%FI)%name(1) = 'basin_longwave'
    cm%clin(cfk%PR)%name(1) = 'basin_rain'
    cm%clin(cfk%TT)%name(1) = 'basin_temperature'
    cm%clin(cfk%UV)%name(1) = 'basin_wind'
    cm%clin(cfk%P0)%name(1) = 'basin_pres'
    cm%clin(cfk%HU)%name(1) = 'basin_humidity'

    !> Read from file to override default configuration.
    call open_config(cm)

    !> Check if any of the files are in the legacy binary format.
    do i = 1, size(cm%clin)
        if (cm%clin(i)%filefmt == 0) then
            print 8900, adjustl(trim(cm%clin(i)%id_var))
            stop
        end if
    end do

    !> Special case two sources of precipitation with alpha constant.
!todo generalize this
!    if (indx == cfk%PR .and. cm%clin(cfk%PR)%filefmt == 6) then
!        call Init_clim_data(cfk%PR, 921, cm)
!        call Init_clim_data(8, 922, cm)
!        return
!    end if

    !> Call to open the forcing file.
!    call Init_clim_data(indx, cm%basefileunit + indx, cm)

8900 format( &
        /1x, 'Forcing data in the legacy binary format (*.bin) are no longer', &
        /1x, 'supported by the model.', &
        /3x, 'Forcing field: ', a, &
        /1x, 'Please convert these data to one of the supported formats.'/)

end subroutine !READ_CHECK_FORCING_FILES
