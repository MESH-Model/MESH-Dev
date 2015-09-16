!>**********************************************************************
!>  Athor: Gonzalo Sapriza Azuri
!>  Description: Handled climate forcing data to be loaded in memory
!>**********************************************************************
module climate_forcing

    use climate_forcing_variabletypes
    use climate_forcing_variables

    implicit none

    !* YEAR_START_CLIM: Year at the start of the simulation.
    !* JDAY_START_CLIM: Julian day at the start of the simulation.
    !* HOUR_START_CLIM: Hour at the start of the simulation.
    !* MINS_START_CLIM: Minute (in 30-min. increment; either 0 or 30) at the start of the simulation.
!    integer YEAR_START_CLIM, JDAY_START_CLIM, HOUR_START_CLIM, MINS_START_CLIM

    real, dimension(:), allocatable :: &
        FSVHGRD, FSIHGRD, &
!        FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD,
        VLGRD, &
!        FSDOWN, &
        FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, VLGAT

    !> MAM - variables for forcing data interpolation:
    real, dimension(:), allocatable :: &
        FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE, &
        TAGATPRE, ULGATPRE, PRESGATPRE, QAGATPRE, &
        FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, &
        TAGATPST, ULGATPST, PRESGATPST, QAGATPST
    real TRATIO

    contains

    !> *****************************************************************
    !> Open the MESH_input_forcing.bin file
    !> *****************************************************************
    subroutine climate_module_init(bi, ts, cm, ENDDATA)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use FLAGS
        use climate_forcing_data, only: OpenData

        !> Input variables.
        type(basin_info) :: bi
        type(dates_model) :: ts

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        !* toskip: The number of variables in the file per timestep
!        integer nyy, ndy,
        integer JDAY_IND_MET, ISTEP_START, nmy, nhy, nrs, Jday_IND2, Jday_IND3, toskip
        integer i, j, m

        integer ilg

        !> Initialize the climate variable.
        call READ_CHECK_FORCING_FILES(bi, ts, cm)

        !> Open the forcing files.
        do i = 1, size(cm%clin)
            call OpenData(i, cm)
        end do

        !todo - leave these in for event based runs
        !> IYEAR is set in the MESH_parameters_CLASS.ini file
        !> YEAR_START is set in the MESH_input_run_options.ini file
!        nyy = YEAR_START - cm%start_date%year
!        ndy = JDAY_START - cm%start_date%jday
        nmy = MINS_START - cm%start_date%mins
        nhy = HOUR_START - cm%start_date%hour

        ! set ISTEP_START based on HOURLYFLAG
        !  (could be optimised as ISTEP_START = 2 - HOURLYFLAG)
        !HOURLYFLAG is 1 if the data is every hour, and 0 if the data is every half-hour
        !ISTEP_START is used to calculate nrs, and doubles the effect of the hours and
        ! minutes if the data is in half-hourly format
!        if (HOURLYFLAG == 1) then
!            ISTEP_START = 1
!        else
!            ISTEP_START = 2
!        end if
        !Note added by M. Mekonnen
        !ISTEP_START is used to count the number of records in one hour,
        !hence a 30 minute interval forcing data will have 2 records per hour (ISTEP_START = 2)
        !and a 1 hour interval forcing data will have 1 record per hour (ISTEP_START = 1). To
        !accomodate forcing data with time intervals greater than 1 hour,
        !it is better to count the number of records in a day:
!todo: This assumes all forcing data start at the same date (ok?) and share the same time-step (will cause error).
        ISTEP_START = 24*60/HOURLYFLAG
        if (mod(24*60, HOURLYFLAG) /= 0) then
            print 2334
            stop
        end if

2334 format(/, &
        /1x, 'Forcing data time interval needs to be in either', &
        /1x, 'of the following values:', &
        /1x, '30 or n*60 where n can be either 1, 2, 3, 4, 6, 8 or 12.', /)

        call Julian_Day_ID(YEAR_START, JDAY_START, Jday_IND2)
        call Julian_Day_ID(cm%start_date%year, cm%start_date%jday, Jday_IND3)
        if ((Jday_IND2 < Jday_IND3) .and. (YEAR_START /= 0)) then
            print 2442
            stop
        end if

2442 format(/, &
        /1x, 'ERROR: Simulation start date too early. The start date in the', &
        /3x, 'run options file may occur before the start date of the met.', &
        /3x, 'forcing input data in the CLASS parameter file.', /)

        !Notes added by M. Mekonnen - To keep nrs calculation as before
        !(and to be compatible with the above modification) we need to
        !divide ISTEP_START by 24.
        !nrs = JDAY_IND_MET*ISTEP_START*24 + nhy*ISTEP_START + nmy/30  !aLIU
        JDAY_IND_MET = Jday_IND2 - Jday_IND3
        nrs = JDAY_IND_MET*ISTEP_START + nhy*ISTEP_START/24 + nmy/30
        if (ro%VERBOSEMODE > 0) print *, 'NRS=', nrs
        ! FIX BUG IN JULIAN DAY CALCULATION FOR NRS ---ALIU FEB2009
        if (YEAR_START == 0 .and. JDAY_START == 0 .and. MINS_START == 0 .and. HOUR_START == 0) then
            nrs = 0
        elseif (nrs < 0) then
            print *, 'Desired start date is before the start of the ', &
                'data in MESH_input_forcing.bin'
            print *, 'Please adjust the start date in ', &
                'MESH_input_run_options.ini'
            stop
        end if

        !> the following code is used to skip entries at the start
        !> of the bin file
        if (ro%VERBOSEMODE > 0) print *, 'Skipping', nrs, 'Registers in bin file'

        do i = 1, nrs

            !> R2C-format (ASCII).
            if (cm%clin(cfk%FB)%filefmt == 1) then !Skip the r2c file's information
                read(90, *, end = 999)
                do m = 1, bi%YCOUNT
                    Read (90, *, end = 999)
                end do
                read (90, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%FI)%filefmt == 1) then
                read(91, *, end = 999) !:Frame line
                do m = 1, bi%YCOUNT
                    read(91, *, end = 999)
                end do
                read(91, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%PR)%filefmt == 1) then
                read(92, *, end = 999) !:Frame line
                do m = 1, bi%YCOUNT
                    read(92, *, end = 999)
                end do
                read(92, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%TT)%filefmt == 1) then
                read(93, *, END=999) !:Frame line
                do m = 1, bi%YCOUNT
                    read(93, *, end = 999)
                end do
                read(93, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%UV)%filefmt == 1) then
                read(94, *, end = 999) !:Frame line
                do m = 1, bi%YCOUNT
                    read(94, *, end = 999)
                end do
                read(94, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%P0)%filefmt == 1) then
                read(95, *, end = 999) !:Frame line
                do m = 1, bi%YCOUNT
                    read(95, *, end = 999)
                end do
                read(95, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%HU)%filefmt == 1) then
                read(96, *, end = 999) !:Frame line
                do m = 1, bi%YCOUNT
                    read(96, *, end = 999)
                end do
                read(96, *, end = 999)
            end if

            !> CSV-format.
            if (cm%clin(cfk%FB)%filefmt == 2) then !Skip the csv file's information
                read(90, * , end = 999)
            end if
            if (cm%clin(cfk%FI)%filefmt == 2) then
                read(91, *, end = 999)
            end if
            if (cm%clin(cfk%PR)%filefmt == 2) then
                read(92, *, end = 999)
            end if
            if (cm%clin(cfk%TT)%filefmt == 2) then
                read(93, *, end = 999)
            end if
            if (cm%clin(cfk%UV)%filefmt == 2) then
                read(94, *, end = 999)
            end if
            if (cm%clin(cfk%P0)%filefmt == 2) then
                read(95, *, end = 999)
            end if
            if (cm%clin(cfk%HU)%filefmt == 2) then
                read(96, *, end = 999)
            end if

        end do !i = 1, nrs

        !> Allocate and initialize GRD variables.
        allocate( &
            FSVHGRD(bi%NA), FSIHGRD(bi%NA), &
!            , FDLGRD(bi%NA), PREGRD(bi%NA), TAGRD(bi%NA), ULGRD(bi%NA), PRESGRD(bi%NA), &
!            QAGRD(bi%NA), &
            VLGRD(bi%NA))
!            , FSDOWN(bi%NA))
        FSVHGRD = 0.0
        FSIHGRD = 0.0
!        FDLGRD = 0.0
!        PREGRD = 0.0
!        TAGRD = 0.0
!        ULGRD = 0.0
!        PRESGRD = 0.0
!        QAGRD = 0.0
        VLGRD = 0.0
!        FSDOWN = 0.0

        !> Allocate and initialize GAT variables.
        ilg = bi%NA*bi%NTYPE
        allocate( &
            FSVHGAT(ilg), FSIHGAT(ilg), FDLGAT(ilg), PREGAT(ilg), TAGAT(ilg), ULGAT(ilg), PRESGAT(ilg), QAGAT(ilg), VLGAT(ilg))
        FSVHGAT = 0.0
        FSIHGAT = 0.0
        FDLGAT = 0.0
        PREGAT = 0.0
        TAGAT = 0.0
        ULGAT = 0.0
        PRESGAT = 0.0
        QAGAT = 0.0
        VLGAT = 0.0

        !> Allocate and initialize GAT variables for climate interpolation.
        allocate( &
            FSVHGATPRE(ilg), FSIHGATPRE(ilg), FDLGATPRE(ilg), PREGATPRE(ilg), &
            TAGATPRE(ilg), ULGATPRE(ilg), PRESGATPRE(ilg), QAGATPRE(ilg), &
            FSVHGATPST(ilg), FSIHGATPST(ilg), FDLGATPST(ilg), PREGATPST(ilg), &
            TAGATPST(ilg), ULGATPST(ilg), PRESGATPST(ilg), QAGATPST(ilg))
        FSVHGATPRE = 0.0
        FSIHGATPRE = 0.0
        FDLGATPRE = 0.0
        PREGATPRE = 0.0
        TAGATPRE = 0.0
        ULGATPRE = 0.0
        PRESGATPRE = 0.0
        QAGATPRE = 0.0
        FSVHGATPST = 0.0
        FSIHGATPST = 0.0
        FDLGATPST = 0.0
        PREGATPST = 0.0
        TAGATPST = 0.0
        ULGATPST = 0.0
        PRESGATPST = 0.0
        QAGATPST = 0.0

        return

999 ENDDATA = .true.

    end subroutine !climate_module_init

    !> *****************************************************************
    !> MAM - Read in initial meteorological forcing data
    !> *****************************************************************
    subroutine climate_module_loaddata(bi, firststep, cm, ENDDATA)

        use sa_mesh_shared_variabletypes
        use FLAGS
        use climate_forcing_data, only: READ_FORCING_DATA

        !> Input variables.
        type(basin_info) :: bi
        logical firststep

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer i

        if (firststep) then

            !> Set the current time step of the input forcing data.
            do i = 1, size(cm%clin)
                cm%clin(i)%timestep_now = TIME_STEP_NOW
            end do
            if (INTERPOLATIONFLAG == 0) then
                call READ_FORCING_DATA(bi, cm, &
!                                       FSDOWN, &
                                       FSVHGRD, FSIHGRD, &
!                                       FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                       FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, &
                                       ENDDATA)
            elseif (INTERPOLATIONFLAG == 1) then
                if (RESUMEFLAG /= 1) then
                    call READ_FORCING_DATA(bi, cm, &
!                                           FSDOWN, &
                                           FSVHGRD, FSIHGRD, &
!                                           FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                           FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE, TAGATPRE, ULGATPRE, &
                                           PRESGATPRE, QAGATPRE, &
                                           ENDDATA)
                    call READ_FORCING_DATA(bi, cm, &
!                                           FSDOWN, &
                                           FSVHGRD, FSIHGRD, &
!                                           FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                           FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
                                           PRESGATPST, QAGATPST, &
                                           ENDDATA)
                else
                    call READ_FORCING_DATA(bi, cm, &
!                                           FSDOWN, &
                                           FSVHGRD, FSIHGRD, &
!                                           FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                           FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
                                           PRESGATPST, QAGATPST, &
                                           ENDDATA)
                end if
            end if
            VLGRD = 0.0
            VLGAT = 0.0
        else

            !> Increment the current time step of the input forcing data.
            cm%clin(cfk%FB)%timestep_now = cm%clin(cfk%FB)%timestep_now + TIME_STEP_MINS
            if (cm%clin(cfk%FB)%timestep_now >= cm%clin(cfk%FB)%hf) then
                cm%clin(cfk%FB)%timestep_now = 0
                if (INTERPOLATIONFLAG == 1) then
                    FSVHGATPRE = FSVHGATPST
                    FSIHGATPRE = FSIHGATPST
                end if
            end if
            cm%clin(cfk%FI)%timestep_now = cm%clin(cfk%FI)%timestep_now + TIME_STEP_MINS
            if (cm%clin(cfk%FI)%timestep_now >= cm%clin(cfk%FI)%hf) then
                cm%clin(cfk%FI)%timestep_now = 0
                if (INTERPOLATIONFLAG == 1) FDLGATPRE = FDLGATPST
            end if
            cm%clin(cfk%PR)%timestep_now = cm%clin(cfk%PR)%timestep_now + TIME_STEP_MINS
            if (cm%clin(cfk%PR)%timestep_now >= cm%clin(cfk%PR)%hf) then
                cm%clin(cfk%PR)%timestep_now = 0
                if (INTERPOLATIONFLAG == 1) PREGATPRE = PREGATPST
            end if
            cm%clin(cfk%TT)%timestep_now = cm%clin(cfk%TT)%timestep_now + TIME_STEP_MINS
            if (cm%clin(cfk%TT)%timestep_now >= cm%clin(cfk%TT)%hf) then
                cm%clin(cfk%TT)%timestep_now = 0
                if (INTERPOLATIONFLAG == 1) TAGATPRE = TAGATPST
            end if
            cm%clin(cfk%UV)%timestep_now = cm%clin(cfk%UV)%timestep_now + TIME_STEP_MINS
            if (cm%clin(cfk%UV)%timestep_now >= cm%clin(cfk%UV)%hf) then
                cm%clin(cfk%UV)%timestep_now = 0
                if (INTERPOLATIONFLAG == 1) ULGATPRE = ULGATPST
            end if
            cm%clin(cfk%P0)%timestep_now = cm%clin(cfk%P0)%timestep_now + TIME_STEP_MINS
            if (cm%clin(cfk%P0)%timestep_now >= cm%clin(cfk%P0)%hf) then
                cm%clin(cfk%P0)%timestep_now = 0
                if (INTERPOLATIONFLAG == 1) PRESGATPRE = PRESGATPST
            end if
            cm%clin(cfk%HU)%timestep_now = cm%clin(cfk%HU)%timestep_now + TIME_STEP_MINS
            if (cm%clin(cfk%HU)%timestep_now >= cm%clin(cfk%HU)%hf) then
                cm%clin(cfk%HU)%timestep_now = 0
                if (INTERPOLATIONFLAG == 1) QAGATPRE = QAGATPST
            end if

            if (INTERPOLATIONFLAG == 1) then
                call READ_FORCING_DATA(bi, cm, &
!                                       FSDOWN, &
                                       FSVHGRD, FSIHGRD, &
!                                       FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                       FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
                                       PRESGATPST, QAGATPST, &
                                       ENDDATA)
            else
                call READ_FORCING_DATA(bi, cm, &
!                                       FSDOWN, &
                                       FSVHGRD, FSIHGRD, &
!                                       FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                       FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, &
                                       ENDDATA)
            end if

        end if !(firststep) then

    end subroutine !climate_module_loaddata

    subroutine climate_module_interpolatedata(bi, &
!todo: These variables can be stored elsewhere instead of passed.
        FAREGAT, &
        cm)

        use sa_mesh_shared_variabletypes

        !> Input variables.
        type(basin_info) :: bi
        real, dimension(:) :: FAREGAT

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Local variables.
        integer k

        !> Determine the time ratio (linear) and interpolate the data for the current time-step.
        TRATIO = min(1.0, real(cm%clin(cfk%FB)%timestep_now)/cm%clin(cfk%FB)%hf)
        FSVHGAT = FSVHGATPRE + TRATIO*(FSVHGATPST - FSVHGATPRE)
        FSIHGAT = FSIHGATPRE + TRATIO*(FSIHGATPST - FSIHGATPRE)
        TRATIO = min(1.0, real(cm%clin(cfk%FI)%timestep_now)/cm%clin(cfk%FI)%hf)
        FDLGAT = FDLGATPRE + TRATIO*(FDLGATPST - FDLGATPRE)
        TRATIO = min(1.0, real(cm%clin(cfk%PR)%timestep_now)/cm%clin(cfk%PR)%hf)
        PREGAT = PREGATPRE + TRATIO*(PREGATPST - PREGATPRE)
        TRATIO = min(1.0, real(cm%clin(cfk%TT)%timestep_now)/cm%clin(cfk%TT)%hf)
        TAGAT = TAGATPRE + TRATIO*(TAGATPST - TAGATPRE)
        TRATIO = min(1.0, real(cm%clin(cfk%UV)%timestep_now)/cm%clin(cfk%UV)%hf)
        ULGAT = ULGATPRE + TRATIO*(ULGATPST - ULGATPRE)
        TRATIO = min(1.0, real(cm%clin(cfk%P0)%timestep_now)/cm%clin(cfk%P0)%hf)
        PRESGAT = PRESGATPRE + TRATIO*(PRESGATPST - PRESGATPRE)
        TRATIO = min(1.0, real(cm%clin(cfk%HU)%timestep_now)/cm%clin(cfk%HU)%hf)
        QAGAT = QAGATPRE + TRATIO*(QAGATPST - QAGATPRE)

        !> Distribute the grid variables.
        FSVHGRD = 0.0
        FSIHGRD = 0.0
        cm%clin(cfk%FI)%climvGrd = 0.0
        cm%clin(cfk%UV)%climvGrd = 0.0
        cm%clin(cfk%TT)%climvGrd = 0.0
        cm%clin(cfk%HU)%climvGrd = 0.0
        cm%clin(cfk%P0)%climvGrd = 0.0
        cm%clin(cfk%PR)%climvGrd = 0.0
        do k = 1, bi%NML
            if (FAREGAT(k) > 0.0) then
                FSVHGRD(bi%ILMOS(k)) = FSVHGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*FSVHGAT(k)
                FSIHGRD(bi%ILMOS(k)) = FSIHGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*FSIHGAT(k)
                cm%clin(cfk%FI)%climvGrd(bi%ILMOS(k)) = cm%clin(cfk%FI)%climvGrd(bi%ILMOS(k)) + &
                    bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*FDLGAT(k)
                cm%clin(cfk%UV)%climvGrd(bi%ILMOS(k)) = cm%clin(cfk%UV)%climvGrd(bi%ILMOS(k)) + &
                    bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*ULGAT(k)
                cm%clin(cfk%TT)%climvGrd(bi%ILMOS(k)) = cm%clin(cfk%TT)%climvGrd(bi%ILMOS(k)) + &
                    bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*TAGAT(k)
                cm%clin(cfk%HU)%climvGrd(bi%ILMOS(k)) = cm%clin(cfk%HU)%climvGrd(bi%ILMOS(k)) + &
                    bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*QAGAT(k)
                cm%clin(cfk%P0)%climvGrd(bi%ILMOS(k)) = cm%clin(cfk%P0)%climvGrd(bi%ILMOS(k)) + &
                    bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*PRESGAT(k)
                cm%clin(cfk%PR)%climvGrd(bi%ILMOS(k)) = cm%clin(cfk%PR)%climvGrd(bi%ILMOS(k)) + &
                    bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*PREGAT(k)
            end if
        end do !k = 1, NML
        cm%clin(cfk%FB)%climvGrd = 2.0*FSVHGRD

    end subroutine !climate_module_interpolatedata

end module !climate_forcing
