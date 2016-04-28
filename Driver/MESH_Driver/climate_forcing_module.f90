!>**********************************************************************
!>  Athor: Gonzalo Sapriza Azuri
!>  Description: Handled climate forcing data to be loaded in memory
!>**********************************************************************
module climate_forcing

    use climate_forcing_constants
    use climate_forcing_variabletypes
    use climate_forcing_config
    use climate_forcing_io

    implicit none

    !* YEAR_START_CLIM: Year at the start of the simulation.
    !* JDAY_START_CLIM: Julian day at the start of the simulation.
    !* HOUR_START_CLIM: Hour at the start of the simulation.
    !* MINS_START_CLIM: Minute (in 30-min. increment; either 0 or 30) at the start of the simulation.
!-    integer YEAR_START_CLIM, JDAY_START_CLIM, HOUR_START_CLIM, MINS_START_CLIM

!-    real, dimension(:), allocatable, save :: &
!-        FSVHGRD, FSIHGRD, &
!-        FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD,
!-        VLGRD, &
!-        FSDOWN, &
!-        FSVHGAT, FSIHGAT, &
!-        FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, &
!-        VLGAT

    !> MAM - variables for forcing data interpolation:
!-    real, dimension(:), allocatable :: &
!-        FSVHGATPRE, FSIHGATPRE, &
!-        FDLGATPRE, PREGATPRE, &
!-        TAGATPRE, ULGATPRE, PRESGATPRE, QAGATPRE, &
!-        FSVHGATPST, FSIHGATPST
!-        FDLGATPST, PREGATPST, &
!-        TAGATPST, ULGATPST, PRESGATPST, QAGATPST
!-    real TRATIO

    contains

    !> *****************************************************************
    !> Open the MESH_input_forcing.bin file
    !> *****************************************************************
    subroutine climate_module_init(ts, shd, ii1, ii2, cm, ENDDATA)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use FLAGS

        !> Input variables.
        type(dates_model) :: ts
        type(ShedGridParams) :: shd
        integer ii1, ii2

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        !* toskip: The number of variables in the file per timestep
!        integer nyy, ndy,
        integer JDAY_IND_MET, ISTEP_START, nmy, nhy, nrs, Jday_IND2, Jday_IND3, toskip
!-        integer nts, rts, timeStepClimF
        integer vid, t, s, k, j, i

!-        integer ilg

        !> Allocate the climate forcing variable.
!?        cm%nclim = ck%nn
!?        if (allocated(cm%dat)) deallocate(cm%dat)
!?        allocate(cm%dat(cm%nclim))

        !> Set the default file name.
        cm%dat(ck%FB)%fname = 'basin_shortwave'
        cm%dat(ck%FI)%fname = 'basin_longwave'
        cm%dat(ck%RT)%fname = 'basin_rain'
        cm%dat(ck%TT)%fname = 'basin_temperature'
        cm%dat(ck%UV)%fname = 'basin_wind'
        cm%dat(ck%P0)%fname = 'basin_pres'
        cm%dat(ck%HU)%fname = 'basin_humidity'

        !> Read from file to override default configuration.
        call open_config(cm)

        !> Initialize the climate variable.
!-        call READ_CHECK_FORCING_FILES(shd, ts, cm)

    !> Call to open the forcing file.
!-    call Init_clim_data(vid, cm%basefileunit + vid, cm)

        do vid = 1, cm%nclim

            !> Check if the file is in the legacy binary format.
            if (cm%dat(vid)%ffmt == 0) then
                print 8900, adjustl(trim(cm%dat(vid)%id_var))
                stop
            end if

8900    format(/1x, 'Forcing data in the legacy binary format (*.bin) are no longer', &
               /1x, 'supported by the model.', &
               /3x, 'Forcing field: ', a, &
               /1x, 'Please convert these data to one of the supported formats.', /)

            !> Forcing data time step should not be less than 30 min - there is no
            !> any increase in accuracy as delt (CLASS model time step) is 30 min.
            if (cm%dat(vid)%hf < 30) then
                print 1028
                stop
            end if

1028    format(/1x, 'FORCING DATA TIME STEP IS LESS THAN 30 MIN', &
               /1x, 'AGGREGATE THE FORCING DATA TO 30 MIN INTERVAL AND TRY AGAIN', /)

            !> MAM - ALLOCATE AND INITIALIZE INTERPOLATION VARIABLES:
            !> For 30 minute forcing data there is no need for interpolation and
            !> hence no need to assign PRE and PST variables
            if (cm%dat(vid)%ipflg > 1 .or. (cm%dat(vid)%ipflg == 1 .and. cm%dat(vid)%hf == 30)) then
                print 9000
                cm%dat(vid)%ipflg = 0
            end if

9000    format(/1x, 'INTERPOLATIONFLAG IS NOT SPECIFIED CORRECTLY AND IS SET TO 0 BY THE MODEL.', &
               /1x, '0: NO INTERPOLATION OF FORCING DATA.', &
               /1x, '1: LINEARLY INTERPOLATES FORCING DATA FOR INTERMEDIATE TIME STEPS.', &
               /1x, 'NOTE: INTERPOLATIONFLAG SHOULD BE SET TO 0 FOR 30 MINUTE FORCING DATA.', /)

            !> Determine the number of time-steps in the run.
    !todo: This doesn't work if run start and stop days are set to zeros;
    !todo: The above should be reflected in module_dates where nr_days is determined;
!-            timeStepClimF = ts%nr_days*24*(60/TIME_STEP_MINS)/real(cm%dat(vid)%hf)*TIME_STEP_MINS

            !> Determine the time-stepping if data is read into memory.
!-            if (timeStepClimF <= cm%dat(vid)%nblocks) then
!-                allocate(cm%dat(vid)%ntimes(1))
!-                cm%dat(vid)%ntimes(1) = timeStepClimF
!-            else
!-                nts = timeStepClimF/cm%dat(vid)%nblocks
!-                rts = timeStepClimF - cm%dat(vid)%nblocks*nts
!-                if (rts == 0) then
!-                    allocate(cm%dat(vid)%ntimes(nts))
!-                    cm%dat(vid)%ntimes = cm%dat(vid)%nblocks
!-                else
!-                    allocate(cm%dat(vid)%ntimes(nts + 1))
!-                    cm%dat(vid)%ntimes = cm%dat(vid)%nblocks
!-                    cm%dat(vid)%ntimes(nts + 1) = rts
!-                end if
!-            end if

            !> Set the unit number and allocate the default number of source files.
            cm%dat(vid)%fiun = cm%basefileunit + vid
!-            allocate(cm%dat(vid)%name(1))

            !> Allocate the gridded series.
            allocate(cm%dat(vid)%GRD(shd%NA), cm%dat(vid)%GAT(shd%lc%NML), cm%dat(vid)%GRU(shd%lc%NTYPE))

            !> Open the forcing files.
            if (open_data(shd, vid, cm)) goto 999

!todo - leave these in for event based runs
        !> IYEAR is set in the MESH_parameters_CLASS.ini file
        !> YEAR_START is set in the MESH_input_run_options.ini file
!        nyy = YEAR_START - cm%dat(vid)%start_date%year
!        ndy = JDAY_START - cm%dat(vid)%start_date%jday
            nmy = MINS_START - cm%dat(vid)%start_date%mins
            nhy = HOUR_START - cm%dat(vid)%start_date%hour

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
            ISTEP_START = 24*60/cm%dat(vid)%hf
            if (mod(24*60, cm%dat(vid)%hf) /= 0) then
                print 2334
                stop
            end if

2334    format(//1x, 'Forcing data time interval needs to be in either', &
               /1x, 'of the following values:', &
               /1x, '30 or n*60 where n can be either 1, 2, 3, 4, 6, 8 or 12.', /)

            call Julian_Day_ID(YEAR_START, JDAY_START, Jday_IND2)
            call Julian_Day_ID(cm%dat(vid)%start_date%year, cm%dat(vid)%start_date%jday, Jday_IND3)
            if ((Jday_IND2 < Jday_IND3) .and. (YEAR_START /= 0)) then
                print 2442
                stop
            end if

2442    format(//1x, 'ERROR: Simulation start date too early. The start date in the', &
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

        !> Preserve the last record skipped with INTERPOLATIONFLAG 2.
!?            if (INTERPOLATIONFLAG == 2) nrs = nrs - 1

        !> Skip records of forcing data.
            do i = 1, nrs

                !> Call skip data for the climate forcing variable.
                if (update_data(shd, ii1, ii2, .true., vid, cm)) goto 999

            !> R2C-format (ASCII).
!-            if (cm%dat(ck%FB)%ffmt == 1) then !Skip the r2c file's information
!-                read(90, *, end = 999)
!-                do m = 1, shd%yCount
!-                    Read (90, *, end = 999)
!-                end do
!-                read (90, *, end = 999) !:EndFrame line
!-            end if
!-            if (cm%dat(ck%FI)%ffmt == 1) then
!-                read(91, *, end = 999) !:Frame line
!-                do m = 1, shd%yCount
!-                    read(91, *, end = 999)
!-                end do
!-                read(91, *, end = 999) !:EndFrame line
!-            end if
!-            if (cm%dat(ck%RT)%ffmt == 1) then
!-                read(92, *, end = 999) !:Frame line
!-                do m = 1, shd%yCount
!-                    read(92, *, end = 999)
!-                end do
!-                read(92, *, end = 999) !:EndFrame line
!-            end if
!-            if (cm%dat(ck%TT)%ffmt == 1) then
!-                read(93, *, END=999) !:Frame line
!-                do m = 1, shd%yCount
!-                    read(93, *, end = 999)
!-                end do
!-                read(93, *, end = 999) !:EndFrame line
!-            end if
!-            if (cm%dat(ck%UV)%ffmt == 1) then
!-                read(94, *, end = 999) !:Frame line
!-                do m = 1, shd%yCount
!-                    read(94, *, end = 999)
!-                end do
!-                read(94, *, end = 999) !:EndFrame line
!-            end if
!-            if (cm%dat(ck%P0)%ffmt == 1) then
!-                read(95, *, end = 999) !:Frame line
!-                do m = 1, shd%yCount
!-                    read(95, *, end = 999)
!-                end do
!-                read(95, *, end = 999) !:EndFrame line
!-            end if
!-            if (cm%dat(ck%HU)%ffmt == 1) then
!-                read(96, *, end = 999) !:Frame line
!-                do m = 1, shd%yCount
!-                    read(96, *, end = 999)
!-                end do
!-                read(96, *, end = 999)
!-            end if

            !> CSV-format.
!-            if (cm%dat(ck%FB)%ffmt == 2) then !Skip the csv file's information
!-                read(90, * , end = 999)
!-            end if
!-            if (cm%dat(ck%FI)%ffmt == 2) then
!-                read(91, *, end = 999)
!-            end if
!-            if (cm%dat(ck%RT)%ffmt == 2) then
!-                read(92, *, end = 999)
!-            end if
!-            if (cm%dat(ck%TT)%ffmt == 2) then
!-                read(93, *, end = 999)
!-            end if
!-            if (cm%dat(ck%UV)%ffmt == 2) then
!-                read(94, *, end = 999)
!-            end if
!-            if (cm%dat(ck%P0)%ffmt == 2) then
!-                read(95, *, end = 999)
!-            end if
!-            if (cm%dat(ck%HU)%ffmt == 2) then
!-                read(96, *, end = 999)
!-            end if

            end do !i = 1, nrs

            !> Activate fields for interpolation.
            if (cm%dat(vid)%ipflg == 1) then
                if (allocated(cm%dat(vid)%ipdat)) deallocate(cm%dat(vid)%ipdat)
                allocate(cm%dat(vid)%ipdat(size(cm%dat(vid)%blocks, 1), 2))
            end if

        !> Allocate and initialize the alpha coefficient for the default series.
!            allocate(cm%dat(vid)%alpha(cm%dat(vid)%nseries))
!            cm%dat(vid)%alpha = 1.0 / cm%dat(vid)%nseries

        !> Special case two sources of precipitation with alpha constant.
!todo generalize this
!?        if (vid == ck%RT .and. cm%dat(ck%RT)%ffmt == 6) then
!?            call Init_clim_data(ck%RT, 921, cm)
!?            call Init_clim_data(8, 922, cm)
!?            return
!?        end if

        end do !vid = 1, cm%nclim

        !> Allocate and initialize GRD variables.
!-        allocate( &
!-            FSVHGRD(shd%NA), FSIHGRD(shd%NA), &
!            , FDLGRD(shd%NA), PREGRD(shd%NA), TAGRD(shd%NA), ULGRD(shd%NA), PRESGRD(shd%NA), &
!            QAGRD(shd%NA), &
!-            VLGRD(shd%NA))
!            , FSDOWN(shd%NA))
!-        FSVHGRD = 0.0
!-        FSIHGRD = 0.0
!        FDLGRD = 0.0
!        PREGRD = 0.0
!        TAGRD = 0.0
!        ULGRD = 0.0
!        PRESGRD = 0.0
!        QAGRD = 0.0
!-        VLGRD = 0.0
!        FSDOWN = 0.0

        !> Allocate and initialize GAT variables.
!        ilg = shd%NA*shd%lc%NTYPE
!-        allocate( &
!-            FSVHGAT(shd%lc%NML), FSIHGAT(shd%lc%NML), &
!            FDLGAT(ilg), PREGAT(ilg), TAGAT(ilg), ULGAT(ilg), &
!            PRESGAT(ilg), QAGAT(ilg), &
!-            VLGAT(shd%lc%NML))
!-        FSVHGAT = 0.0
!-        FSIHGAT = 0.0
!        FDLGAT = 0.0
!        PREGAT = 0.0
!        TAGAT = 0.0
!        ULGAT = 0.0
!        PRESGAT = 0.0
!        QAGAT = 0.0
!-        VLGAT = 0.0

        !> Allocate and initialize GAT variables for climate interpolation.
!-        allocate( &
!-            FSVHGATPRE(shd%lc%NML), FSIHGATPRE(shd%lc%NML), &
!-            FDLGATPRE(ilg), PREGATPRE(ilg), &
!-            TAGATPRE(ilg), ULGATPRE(ilg), PRESGATPRE(ilg), QAGATPRE(ilg), &
!-            FSVHGATPST(shd%lc%NML), FSIHGATPST(shd%lc%NML))
!-            FDLGATPST(ilg), PREGATPST(ilg), &
!-            TAGATPST(ilg), ULGATPST(ilg), PRESGATPST(ilg), QAGATPST(ilg))
!-        FSVHGATPRE = 0.0
!-        FSIHGATPRE = 0.0
!-        FDLGATPRE = 0.0
!-        PREGATPRE = 0.0
!-        TAGATPRE = 0.0
!-        ULGATPRE = 0.0
!-        PRESGATPRE = 0.0
!-        QAGATPRE = 0.0
!-        FSVHGATPST = 0.0
!-        FSIHGATPST = 0.0
!-        FDLGATPST = 0.0
!-        PREGATPST = 0.0
!-        TAGATPST = 0.0
!-        ULGATPST = 0.0
!-        PRESGATPST = 0.0
!-        QAGATPST = 0.0

        return

999     ENDDATA = .true.

    end subroutine !climate_module_init

    !> *****************************************************************
    !> MAM - Read in initial meteorological forcing data
    !> *****************************************************************
!-    subroutine climate_module_loaddata(shd, firststep, cm, iilen, ii1, ii2, ENDDATA)

!-        use sa_mesh_shared_variabletypes
!-        use FLAGS
!-        use climate_forcing_data, only: READ_FORCING_DATA

        !> Input variables.
!-        type(ShedGridParams) :: shd
!-        logical firststep
!-        integer, intent(in) :: iilen, ii1, ii2

        !> Input/Output variables.
!-        type(clim_info) :: cm

        !> Output variables.
!-        logical ENDDATA

        !> Local variables.
!-        integer i

!-        if (firststep) then

            !> Set the current time step of the input forcing data.
!-            do i = 1, size(cm%clin)
!-                cm%clin(i)%timestep_now = TIME_STEP_NOW
!-            end do
!-            if (INTERPOLATIONFLAG == 0) then
!-                call READ_FORCING_DATA(shd, cm, iilen, ii1, ii2, &
!                                       FSDOWN, &
!-                                       FSVHGRD, FSIHGRD, &
!                                       FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
!-                                       FSVHGAT, FSIHGAT, cm%clin(cfk%FI)%GAT, cm%clin(cfk%PR)%GAT, cm%clin(cfk%TT)%GAT, &
!-                                       cm%clin(cfk%UV)%GAT, cm%clin(cfk%P0)%GAT, cm%clin(cfk%HU)%GAT, &
!-                                       ENDDATA)
!-            elseif (INTERPOLATIONFLAG == 1) then
!-                if (RESUMEFLAG /= 1) then
!-                    call READ_FORCING_DATA(shd, cm, iilen, ii1, ii2, &
!                                           FSDOWN, &
!-                                           FSVHGRD, FSIHGRD, &
!                                           FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
!-                                           FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE, TAGATPRE, ULGATPRE, &
!-                                           PRESGATPRE, QAGATPRE, &
!-                                           ENDDATA)
!-                    call READ_FORCING_DATA(shd, cm, iilen, ii1, ii2, &
!                                           FSDOWN, &
!-                                           FSVHGRD, FSIHGRD, &
!                                           FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
!-                                           FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
!-                                           PRESGATPST, QAGATPST, &
!-                                           ENDDATA)
!-                else
!-                    call READ_FORCING_DATA(shd, cm, iilen, ii1, ii2, &
!                                           FSDOWN, &
!-                                           FSVHGRD, FSIHGRD, &
!                                           FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
!-                                           FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
!-                                           PRESGATPST, QAGATPST, &
!-                                           ENDDATA)
!-                end if
!-            end if
!-            VLGRD = 0.0
!-            VLGAT = 0.0
!-        else

            !> Increment the current time step of the input forcing data.
!-            cm%clin(cfk%FB)%timestep_now = cm%clin(cfk%FB)%timestep_now + TIME_STEP_MINS
!-            if (cm%clin(cfk%FB)%timestep_now >= cm%clin(cfk%FB)%hf) then
!-                cm%clin(cfk%FB)%timestep_now = 0
!-                if (INTERPOLATIONFLAG == 1) then
!-                    FSVHGATPRE = FSVHGATPST
!-                    FSIHGATPRE = FSIHGATPST
!-                end if
!-            end if
!-            cm%clin(cfk%FI)%timestep_now = cm%clin(cfk%FI)%timestep_now + TIME_STEP_MINS
!-            if (cm%clin(cfk%FI)%timestep_now >= cm%clin(cfk%FI)%hf) then
!-                cm%clin(cfk%FI)%timestep_now = 0
!-                if (INTERPOLATIONFLAG == 1) FDLGATPRE = FDLGATPST
!-            end if
!-            cm%clin(cfk%PR)%timestep_now = cm%clin(cfk%PR)%timestep_now + TIME_STEP_MINS
!-            if (cm%clin(cfk%PR)%timestep_now >= cm%clin(cfk%PR)%hf) then
!-                cm%clin(cfk%PR)%timestep_now = 0
!-                if (INTERPOLATIONFLAG == 1) PREGATPRE = PREGATPST
!-            end if
!-            cm%clin(cfk%TT)%timestep_now = cm%clin(cfk%TT)%timestep_now + TIME_STEP_MINS
!-            if (cm%clin(cfk%TT)%timestep_now >= cm%clin(cfk%TT)%hf) then
!-                cm%clin(cfk%TT)%timestep_now = 0
!-                if (INTERPOLATIONFLAG == 1) TAGATPRE = TAGATPST
!-            end if
!-            cm%clin(cfk%UV)%timestep_now = cm%clin(cfk%UV)%timestep_now + TIME_STEP_MINS
!-            if (cm%clin(cfk%UV)%timestep_now >= cm%clin(cfk%UV)%hf) then
!-                cm%clin(cfk%UV)%timestep_now = 0
!-                if (INTERPOLATIONFLAG == 1) ULGATPRE = ULGATPST
!-            end if
!-            cm%clin(cfk%P0)%timestep_now = cm%clin(cfk%P0)%timestep_now + TIME_STEP_MINS
!-            if (cm%clin(cfk%P0)%timestep_now >= cm%clin(cfk%P0)%hf) then
!-                cm%clin(cfk%P0)%timestep_now = 0
!-                if (INTERPOLATIONFLAG == 1) PRESGATPRE = PRESGATPST
!-            end if
!-            cm%clin(cfk%HU)%timestep_now = cm%clin(cfk%HU)%timestep_now + TIME_STEP_MINS
!-            if (cm%clin(cfk%HU)%timestep_now >= cm%clin(cfk%HU)%hf) then
!-                cm%clin(cfk%HU)%timestep_now = 0
!-                if (INTERPOLATIONFLAG == 1) QAGATPRE = QAGATPST
!-            end if
!-
!-            if (INTERPOLATIONFLAG == 1) then
!-                call READ_FORCING_DATA(shd, cm, iilen, ii1, ii2, &
!                                       FSDOWN, &
!-                                       FSVHGRD, FSIHGRD, &
!                                       FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
!-                                       FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
!-                                       PRESGATPST, QAGATPST, &
!-                                       ENDDATA)
!-            else
!-                call READ_FORCING_DATA(shd, cm, iilen, ii1, ii2, &
!                                       FSDOWN, &
!-                                       FSVHGRD, FSIHGRD, &
!                                       FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
!-                                       FSVHGAT, FSIHGAT, cm%clin(cfk%FI)%GAT, cm%clin(cfk%PR)%GAT, cm%clin(cfk%TT)%GAT, &
!-                                       cm%clin(cfk%UV)%GAT, cm%clin(cfk%P0)%GAT, cm%clin(cfk%HU)%GAT, &
!-                                       ENDDATA)
!-            end if

!-        end if !(firststep) then

!-    end subroutine !climate_module_loaddata

!-    subroutine climate_module_interpolatedata(shd, &
!todo: These variables can be stored elsewhere instead of passed.
!-        cm, iilen, ii1, ii2)

!-        use sa_mesh_shared_variabletypes

        !> Input variables.
!-        type(ShedGridParams) :: shd
!-        integer, intent(in) :: iilen, ii1, ii2

        !> Input/Output variables.
!-        type(clim_info) :: cm

        !> Local variables.
!-        integer k

        !> Determine the time ratio (linear) and interpolate the data for the current time-step.
!-        TRATIO = min(1.0, real(cm%clin(cfk%FB)%timestep_now)/cm%clin(cfk%FB)%hf)
!-        FSVHGAT = FSVHGATPRE + TRATIO*(FSVHGATPST - FSVHGATPRE)
!-        FSIHGAT = FSIHGATPRE + TRATIO*(FSIHGATPST - FSIHGATPRE)
!-        TRATIO = min(1.0, real(cm%clin(cfk%FI)%timestep_now)/cm%clin(cfk%FI)%hf)
!-        cm%clin(cfk%FI)%GAT = FDLGATPRE + TRATIO*(FDLGATPST - FDLGATPRE)
!-        TRATIO = min(1.0, real(cm%clin(cfk%PR)%timestep_now)/cm%clin(cfk%PR)%hf)
!-        cm%clin(cfk%PR)%GAT = PREGATPRE + TRATIO*(PREGATPST - PREGATPRE)
!-        TRATIO = min(1.0, real(cm%clin(cfk%TT)%timestep_now)/cm%clin(cfk%TT)%hf)
!-        cm%clin(cfk%TT)%GAT = TAGATPRE + TRATIO*(TAGATPST - TAGATPRE)
!-        TRATIO = min(1.0, real(cm%clin(cfk%UV)%timestep_now)/cm%clin(cfk%UV)%hf)
!-        cm%clin(cfk%UV)%GAT = ULGATPRE + TRATIO*(ULGATPST - ULGATPRE)
!-        TRATIO = min(1.0, real(cm%clin(cfk%P0)%timestep_now)/cm%clin(cfk%P0)%hf)
!-        cm%clin(cfk%P0)%GAT = PRESGATPRE + TRATIO*(PRESGATPST - PRESGATPRE)
!-        TRATIO = min(1.0, real(cm%clin(cfk%HU)%timestep_now)/cm%clin(cfk%HU)%hf)
!-        cm%clin(cfk%HU)%GAT = QAGATPRE + TRATIO*(QAGATPST - QAGATPRE)

        !> Distribute the grid variables.
!-        call SCATTER(shd, iilen, ii1, ii2, FSVHGAT, FSVHGRD)
!-        call SCATTER(shd, iilen, ii1, ii2, FSIHGAT, FSIHGRD)
!-        call SCATTER(shd, iilen, ii1, ii2, cm%clin(cfk%FI)%GAT, cm%clin(cfk%FI)%GRD)
!-        call SCATTER(shd, iilen, ii1, ii2, cm%clin(cfk%UV)%GAT, cm%clin(cfk%UV)%GRD)
!-        call SCATTER(shd, iilen, ii1, ii2, cm%clin(cfk%TT)%GAT, cm%clin(cfk%TT)%GRD)
!-        call SCATTER(shd, iilen, ii1, ii2, cm%clin(cfk%HU)%GAT, cm%clin(cfk%HU)%GRD)
!-        call SCATTER(shd, iilen, ii1, ii2, cm%clin(cfk%P0)%GAT, cm%clin(cfk%P0)%GRD)
!-        call SCATTER(shd, iilen, ii1, ii2, cm%clin(cfk%PR)%GAT, cm%clin(cfk%PR)%GRD)
!-        cm%clin(cfk%FB)%GRD = 2.0*FSVHGRD

!-    end subroutine !climate_module_interpolatedata

    !>
    !> Description: Read in initial meteorological forcing data.
    !>
    function climate_module_update_data(shd, ic, cm, ii1, ii2) result(ENDDATA)

        !> Required for 'shd' variable.
        use sa_mesh_shared_variabletypes

        !> Required for 'ic' variable.
        use model_dates

!-        use FLAGS

        !> Required for 'value' function.
        use strings

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(iter_counter), intent(in) :: ic
        integer, intent(in) :: ii1, ii2
!-        logical firststep

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer ierr, vid, t, s, k, j, i
        real rt, alpha

        ENDDATA = .false.

        !> Loop through variables in the climate forcing object.
        do vid = 1, cm%nclim

            !> Update data if the climate variable is active.
            if (cm%dat(vid)%factive) then

                !> INTERPOLATIONFLAG 1 requires an additional frame be read in the first time-step.
                if (ic%ts_count == 1 .and. cm%dat(vid)%ipflg == 1) then
                    if (update_data(shd, ii1, ii2, .false., vid, cm)) goto 999
                    cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                end if

                !> Grab data from file.
                if (cm%dat(vid)%itimestep == 0) then

!-                VLGRD = 0.0
!-                VLGAT = 0.0

                    !> Update the input forcing data.
                    if (update_data(shd, ii1, ii2, .false., vid, cm)) goto 999

                    !> Apply conditions to the series of data is such conditions exist.
                    if (cm%dat(vid)%nseries > 0) then
                        do s = 1, cm%dat(vid)%nseries
                            select case (cm%dat(vid)%series(s)%attrtype)
                                case ('gru')
                                    call value(cm%dat(vid)%series(s)%attr(1), j, ierr)
                                    call value(cm%dat(vid)%series(s)%attr(2), alpha, ierr)
                                    forall (k = ii1:ii2, shd%lc%JLMOS(k) == j)
                                        cm%dat(vid)%GAT(k) = cm%dat(vid)%GAT(k)*alpha
                                    end forall
                            end select
                        end do
                    end if

                    !> Update interpolation fields.
                    if (cm%dat(vid)%ipflg == 1) then
                        cm%dat(vid)%ipdat(:, 1) = cm%dat(vid)%ipdat(:, 2)
                        cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                    end if

                end if

                !> Interpolate intermediate values.
                if (cm%dat(vid)%ipflg == 1) then
                    cm%dat(vid)%blocks(:, cm%dat(vid)%iblock) = cm%dat(vid)%ipdat(:, 1) + &
                        min(1.0, real(cm%dat(vid)%itimestep)/cm%dat(vid)%hf)*(cm%dat(vid)%ipdat(:, 2) - cm%dat(vid)%ipdat(:, 1))
                end if

                !> Extract data from the climate variable.
                select case (cm%dat(vid)%blocktype)

                    case (1)

                        !> Block type: GRD (Grid).
                        cm%dat(vid)%GRD = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                        do k = ii1, ii2
                            cm%dat(vid)%GAT(k) = cm%dat(vid)%GRD(shd%lc%ILMOS(k))
                        end do
                        do k = ii1, ii2
                            cm%dat(vid)%GRU(shd%lc%JLMOS(k)) = cm%dat(vid)%GAT(k)
                        end do

                    case (2)

                        !> Block type: GRU.
                        cm%dat(vid)%GRU = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                        cm%dat(vid)%GRD = 0.0
                        do k = ii1, ii2
                            j = shd%lc%JLMOS(k)
                            i = shd%lc%ILMOS(k)
                            cm%dat(vid)%GAT(k) = cm%dat(vid)%GRU(j)
                            cm%dat(vid)%GRD(i) = cm%dat(vid)%GRD(i) + shd%lc%ACLASS(i, j)*cm%dat(vid)%GRU(j)
                        end do

                    case (3)

                        !> Block type: GAT (Land element).
                        cm%dat(vid)%GAT = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                        cm%dat(vid)%GRD = 0.0
                        do k = ii1, ii2
                            j = shd%lc%JLMOS(k)
                            i = shd%lc%ILMOS(k)
                            cm%dat(vid)%GRD(i) = cm%dat(vid)%GRD(i) + shd%lc%ACLASS(i, j)*cm%dat(vid)%GAT(k)
                            cm%dat(vid)%GRU(j) = cm%dat(vid)%GAT(k)
                        end do

                    case default
                        stop 'blocktype error'

                end select

!-                if (vid == ck%FB) then
!-                    FSVHGAT = cm%dat(ck%FB)%GAT/2.0
!-                    FSVHGRD = cm%dat(ck%FB)%GRD/2.0
!-                    FSIHGAT = cm%dat(ck%FB)%GAT/2.0
!-                    FSIHGRD = cm%dat(ck%FB)%GRD/2.0
!-                end if

                !> Increment the time-step of the variable.
                cm%dat(vid)%itimestep = cm%dat(vid)%itimestep + (ic%dts/60)
                if (cm%dat(vid)%itimestep >= cm%dat(vid)%hf) then
                    cm%dat(vid)%itimestep = 0
                end if

            end if !cm%dat(vid)%factive) then

        end do !vid = 1, cm%nclim

        return

999     ENDDATA = .true.

    end function !climate_module_load_data

end module !climate_forcing
