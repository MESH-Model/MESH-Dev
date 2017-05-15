!>
!> Description: Module to manage input climate forcing data.
!>
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

    contains

    !>
    !> Description: Initialize the climate forcing object, allocate variables in the object to store data, and open input climate forcing files.
    !>
    !> Inputs:
    !>  - shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !>  - ii1: Starting index in the GAT vector.
    !>  - ii2: Stopping index in the GAT vector.
    !>  - cm: Climate forcing object. Contains the file name, format, and its unit.
    !>
    !> Outputs:
    !>  - ENDDATA: Returns .true. if there was an error occurred intializing the climate object or its variables.
    !>
    function climate_module_init(fls, shd, ii1, ii2, cm) result(ENDDATA)


        !> Required for 'fls', 'mfk' variables.
        use model_files_variables

        !> Required for 'shd', 'ro' variables.
        use sa_mesh_shared_variables

        !> Required for 'RESUMEFLAG'.
        use FLAGS

        !> Input variables.
        type(fl_ids) :: fls
!-        type(dates_model) :: ts
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
        integer vid, t, s, k, j, i, iun, ierr

        ENDDATA = .false.

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

        !> Initialize climate variables.
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

            !> Allocate the gridded series.
            allocate(cm%dat(vid)%GRD(shd%NA), cm%dat(vid)%GAT(shd%lc%NML), cm%dat(vid)%GRU(shd%lc%NTYPE))

            !> Open the forcing files.
            if (open_data(shd, cm, vid)) goto 999

!todo - leave these in for event based runs
            !> IYEAR is set in the MESH_parameters_CLASS.ini file
            !> YEAR_START is set in the MESH_input_run_options.ini file
!            nyy = ic%start%year - cm%dat(vid)%start_date%year
!            ndy = ic%start%jday - cm%dat(vid)%start_date%jday
            nmy = ic%start%mins - cm%dat(vid)%start_date%mins
            nhy = ic%start%hour - cm%dat(vid)%start_date%hour

            ! set ISTEP_START based on HOURLYFLAG
            !  (could be optimised as ISTEP_START = 2 - HOURLYFLAG)
            !HOURLYFLAG is 1 if the data is every hour, and 0 if the data is every half-hour
            !ISTEP_START is used to calculate nrs, and doubles the effect of the hours and
            ! minutes if the data is in half-hourly format
!            if (HOURLYFLAG == 1) then
!                ISTEP_START = 1
!            else
!                ISTEP_START = 2
!            end if
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

            call Julian_Day_ID(ic%start%year, ic%start%jday, Jday_IND2)
            call Julian_Day_ID(cm%dat(vid)%start_date%year, cm%dat(vid)%start_date%jday, Jday_IND3)
            if ((Jday_IND2 < Jday_IND3) .and. (ic%start%year /= 0)) then
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
            if (ic%start%year == 0 .and. ic%start%jday == 0 .and. ic%start%mins == 0 .and. ic%start%hour == 0) then
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
                if (update_data(shd, cm, vid, .true.)) goto 999

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

        !> Read the state of these variables.
        if (RESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat', status = 'old', action = 'read', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Stop if the state file does not contain the same number of climate variables.
            read(iun) ierr
            if (ierr /= cm%nclim) then
                print *, 'Incompatible ranking in climate state file: ' // trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat'
                print *, ' Number of clim. variables read: ', ierr
                print *, ' Number of clim. variabels expected: ', cm%nclim
                stop
            end if

            !> Loop through variables in the climate forcing object and populate their state from file.
            do vid = 1, cm%nclim

                !> Read the state of the climate variable (in case reading into memory).
                read(iun) cm%dat(vid)%blocks
                read(iun) cm%dat(vid)%iblock

                !> Read the last time-step read from file.
                read(iun) cm%dat(vid)%itimestep

                !> Read the interpolation state (if active).
                read(iun) cm%dat(vid)%ipflg
                if (cm%dat(vid)%ipflg == 1) then
                    read(iun) cm%dat(vid)%ipdat

                    !> INTERPOLATIONFLAG 1 requires an additional frame be read from the next time-step.
                    if (cm%dat(vid)%itimestep == 0) then
                        if (update_data(shd, cm, vid, .false.)) goto 999
                        cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                    end if
                end if

            end do !vid = 1, cm%nclim

            !> Close the file to free the unit.
            close(iun)

        end if !(RESUMEFLAG == 4) then

        return

999     ENDDATA = .true.

    end function !climate_module_init

    !>
    !> Description: Update climate data, either from memory or from input climate forcing files.
    !>
    !> Inputs:
    !>  - shd: Basin shed object. Contains information about the number of grids, GRUs, and land elements. Used to allocate objects.
    !>  - ic: Simulation counter object.
    !>  - ii1: Starting index in the GAT vector.
    !>  - ii2: Stopping index in the GAT vector.
    !>  - cm: Climate forcing object. Contains the file name, format, and its unit.
    !>
    !> Outputs:
    !>  - ENDDATA: Returns .true. if there was an error occurred intializing the climate object or its variables.
    !>
    function climate_module_update_data(fls, shd, ii1, ii2, cm) result(ENDDATA)

        !> Required for 'fls' variable.
        use model_files_variables

        !> Required for 'shd' variable.
        use sa_mesh_shared_variables

        !> Required for 'ic' variable.
        use model_dates

        !> Required for 'value' function.
        use strings

        !> Input variables.
        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: ii1, ii2

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
                    if (update_data(shd, cm, vid, .false.)) goto 999
                    cm%dat(vid)%ipdat(:, 2) = cm%dat(vid)%blocks(:, cm%dat(vid)%iblock)
                end if

                !> Grab data from file.
                if (cm%dat(vid)%itimestep == 0) then

                    !> Update the input forcing data.
                    if (update_data(shd, cm, vid, .false.)) goto 999

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

                !> Increment the time-step of the variable.
                cm%dat(vid)%itimestep = cm%dat(vid)%itimestep + (ic%dts/60)
                if (cm%dat(vid)%itimestep >= cm%dat(vid)%hf) then
                    cm%dat(vid)%itimestep = 0
                end if

            end if !cm%dat(vid)%factive) then

        end do !vid = 1, cm%nclim

        return

999     ENDDATA = .true.

    end function !climate_module_update_data

    subroutine climate_module_finalize(fls, shd, cm)

        !> Required for 'ipid' variable.
        use mpi_shared_variables

        !> Required for 'fls', 'mfk' variables.
        use model_files_variables

        !> Required for 'shd' variable.
        use sa_mesh_shared_variables

        !> Required for 'SAVERESUMEFLAG'.
        use FLAGS

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer vid, ierr, iun

        !> Return if not the head node.
        if (ipid /= 0) return

        !> Save the state of these variables.
        if (SAVERESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.clim_ipdat', status = 'replace', action = 'write', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Write the current number of climate variables.
            write(iun) cm%nclim

            !> Loop through variables in the climate forcing object and write the current state to file.
            do vid = 1, cm%nclim

                !> Save the state of the climate variable (in case reading into memory).
                write(iun) cm%dat(vid)%blocks
                write(iun) cm%dat(vid)%iblock

                !> Save the current time-step read from file.
                write(iun) cm%dat(vid)%itimestep

                !> Save the interpolation state (if active).
                write(iun) cm%dat(vid)%ipflg
                if (cm%dat(vid)%ipflg == 1) then
                    write(iun) cm%dat(vid)%ipdat
                end if

            end do !vid = 1, cm%nclim

            !> Close the file to free the unit.
            close(iun)

        end if !(SAVERESUMEFLAG == 4) then

    end subroutine

end module !climate_forcing
