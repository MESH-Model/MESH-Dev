!> *********************************************************************
!> Subroutine to initialize the climate forcing input files.
!> *********************************************************************
subroutine READ_CHECK_FORCING_FILES(cm, indx, ts, bi)

    use sa_mesh_shared_variabletypes
    use climate_forcing
    use model_dates

    implicit none

    !> *****************************************************************
    !> Input/Output variables.
    !> *****************************************************************

    !* cm: Climate information and data variable.
    type(clim_info) :: cm

    !> *****************************************************************
    !> Input variables.
    !> *****************************************************************

    !* indx: Index of the climate variable.
    integer indx

    !* bi: Basin information (from shed file).
    type(basin_info) :: bi

    !* ts: Time-step and -series information.
    type(dates_model) :: ts

    !> *****************************************************************
    !> Internal variables.
    !> *****************************************************************

    !> Reset the number of forcing variables not in the forcing binary file.
    NUM_R2C = 0
    NUM_CSV = 0
    NUM_SEQ = 0

!todo change documentation to reflect that all 3 types of forcing files can be used

    !> Special case two sources of precipitation with alpha constant.
!todo generalize this
    if (indx == cfk%PRE .and. cm%clin(cfk%PRE)%filefmt == 6) then
        call Init_clim_info(cm, ts, cfk%PRE, bi)
        call Init_clim_data(cm, cfk%PRE, 921)
        call Init_clim_info(cm, ts, 8, bi)
        call Init_clim_data(cm, 8, 922)
        NUM_SEQ = NUM_SEQ + 1

    !> Initialize the climate variable for the case when data are to be read into memory.
    elseif (cm%clin(indx)%filefmt < 5) then
        cm%clin(indx)%flagRead = cm%clin(indx)%filefmt
    elseif (cm%clin(indx)%filefmt == 5) then
        call Init_clim_info(cm, ts, indx, bi)
    end if

    !> Set the filename and open the file.
    call Init_clim_data(cm, indx, cm%basefileunit + indx)

    !> Update the legacy open file counters.
    select case (cm%clin(indx)%filefmt)
        case (1, 4)
            NUM_R2C = NUM_R2C + 1
        case (2)
            NUM_CSV = NUM_CSV + 1
        case (3, 5)
            NUM_SEQ = NUM_SEQ + 1
    end select

end subroutine !READ_CHECK_FORCING_FILES
