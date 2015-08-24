!> *********************************************************************
!> Subroutine to initialize the climate forcing input files.
!> *********************************************************************
subroutine READ_CHECK_FORCING_FILES(bi, ts, indx, cm)

    use sa_mesh_shared_variabletypes
    use climate_forcing
    use model_dates

    implicit none
!todo change documentation to reflect that all 3 types of forcing files can be used

    !> *****************************************************************
    !> Input variables.
    !> *****************************************************************

    !* bi: Basin information (from shed file).
    type(basin_info) :: bi

    !* ts: Time-step and time-series information of the simulation.
    type(dates_model) :: ts

    !* indx: Index of the field in the climate variable.
    integer indx

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

    !> Check if the format is the legacy binary format.
    do i = 1, size(cm%clin)
        if (cm%clin(i)%filefmt == 0) then
            print 8900, adjustl(trim(cm%clin(i)%id_var))
            stop
        end if
    end do

    !> Initialize the climate variable to read data into memory.
    if (cm%clin(indx)%timeSize > 0) then
        call Init_clim_info(bi, ts, indx, cm)
!        if (indx == cfk%PR .and. cm%clin(cfk%PR)%filefmt == 6) call Init_clim_info(bi, ts, 8, cm)
    end if

    !> Special case two sources of precipitation with alpha constant.
!todo generalize this
!    if (indx == cfk%PR .and. cm%clin(cfk%PR)%filefmt == 6) then
!        call Init_clim_data(cfk%PR, 921, cm)
!        call Init_clim_data(8, 922, cm)
!        return
!    end if

    !> Call to open the forcing file.
    call Init_clim_data(indx, cm%basefileunit + indx, cm)

8900 format( &
        /1x, 'Forcing data in the legacy binary format (*.bin) are no longer', &
        /1x, 'supported by the model.', &
        /3x, 'Forcing field: ', a, &
        /1x, 'Please convert these data to one of the supported formats.'/)

end subroutine !READ_CHECK_FORCING_FILES
