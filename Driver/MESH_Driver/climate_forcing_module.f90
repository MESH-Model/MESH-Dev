!>**********************************************************************
!>  Athor: Gonzalo Sapriza Azuri
!>  Description: Handled climate forcing data to be loaded in memory
!>**********************************************************************
module climate_forcing

    use sa_mesh_shared_variabletypes
    use sa_mesh_shared_variables
    use model_dates

    implicit none

    type clim_info_read

!        type(counter_date_julian) :: start_date
        integer :: timeSize = 1  !minimum size of block read DEFINED IN THE INPUT FILE
        integer :: nSeries = 1
        integer, dimension(:), allocatable :: ntimes ! number of time in each block of readed data
        character(20) :: id_var !climate variable name
        integer :: filefmt = 0
        character fln*200
        integer unitR !Number unit
        logical openFl !true if file is open
        integer :: readIndx = 1 !index in the block of time that we are reading
        integer :: itime = 1 !time index
        character freq !time freq of data
        integer :: timestep_now = 0
        integer :: hf = 30 !hourly flag

        !* climv: Values for forcing data. (1: Grid; 2: Series; 3: Time-step).
        real, dimension(:, :, :), allocatable :: climv

        !* alpha: Uniform weight to assign when there are multiple series of data. (1: Series).
        real, dimension(:), allocatable :: alpha

    end type !clim_info_read

    type clim_info

        !* nclim: Number of climate variables.
!        integer :: nclim = 7
        integer :: basefileunit = 89
        type(clim_info_read) :: clin(7)
        type(counter_date_julian) :: start_date

    end type !clim_info

    !> *****************************************************************
    !> These are relative indices of the forcing files used throughout
    !> the I/O code for climate forcing.
    !> *****************************************************************
    type climate_forcing_file_keys

        !* FCLO: Fractional cloud cover [-]
        !integer :: FCLO = -9999

        !* FI: Downwelling longwave sky radiation [W m-2]
        integer :: FI = 2

        !* FB: Incoming solar radiation [W m-2]
        !>       CLASS ordinarily requires that the forcing incoming
        !>       shortwave radiation be partitioned into
        !>       the visible and near-infrared components. If these
        !>       are not available, however, they can each
        !>       be roughly estimated as approximately half of the
        !>       total incoming solar radiation.
        !* FSIH: Near infrared shortwave radiation incident on a
        !*       horizontal surface [W m-2]
        !* FSVH: Visible shortwave radiation incident on a horizontal
        !*       surface [W m-2]
        integer :: FB = 1
        !integer :: FSIH = -9999
        !integer :: FSVH = -9999

        !* PR: Surface precipitation rate [kg m-2 s-1]
        integer :: PR = 3

        !* P0: Surface air pressure [Pa]
        integer :: P0 = 6

        !* HU: Specific humidity at reference height [kg kg-1]
        integer :: HU = 7

        !* TT: Air temperature at reference height [K]
        integer :: TT = 4

        !* UU: Zonal component of wind velocity [m s-1]
        !* VV: Meridional component of wind velocity [m s-1]
        !>       CLASS does not actually require information on wind
        !>       direction. Thus, if only the scalar wind
        !>       speed is available, either ULGRD or VLGRD can be set
        !>       to it, and the other to zero.
        !* UV: Wind speed [m s-1]
        !integer :: UU = -9999
        !integer :: VV = -9999
        integer :: UV = 5

    end type

        !>******************************************************************************

    !> These variables are used to keep track of the number of forcing files
    !> that are in different formats
!    integer NUM_CSV, NUM_R2C, NUM_SEQ

    !* YEAR_START_CLIM: Year at the start of the simulation.
    !* JDAY_START_CLIM: Julian day at the start of the simulation.
    !* HOUR_START_CLIM: Hour at the start of the simulation.
    !* MINS_START_CLIM: Minute (in 30-min. increment; either 0 or 30) at the start of the simulation.
!    integer YEAR_START_CLIM, JDAY_START_CLIM, HOUR_START_CLIM, MINS_START_CLIM

    real, dimension(:), allocatable :: &
        FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, VLGRD, FSDOWN, &
        FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, VLGAT

    !> MAM - variables for forcing data interpolation:
    real, dimension(:), allocatable :: &
        FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE, &
        TAGATPRE, ULGATPRE, PRESGATPRE, QAGATPRE, &
        FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, &
        TAGATPST, ULGATPST, PRESGATPST, QAGATPST
    real TRATIO

    type(climate_forcing_file_keys) :: cfk

    contains

    !> -----------------------------------------------------------------
    !> Description: Initialization of clim_info
    !> -----------------------------------------------------------------
    subroutine Init_clim_info(bi, ts, indx, cm)

        !> Input variables.
        type(basin_info), intent(in) :: bi
        type(dates_model), intent(in) :: ts
        integer, intent(in) :: indx

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Local variables.
        integer nts, rts, timeStepClimF

        !> Allocate alpha for series.
        allocate(cm%clin(indx)%alpha(cm%clin(indx)%nSeries))
        cm%clin(indx)%alpha = 1.0 / cm%clin(indx)%nSeries

        timeStepClimF = ts%nr_days*24*(60/TIME_STEP_MINS)/real(cm%clin(indx)%hf)*TIME_STEP_MINS
        if (timeStepClimF <= cm%clin(indx)%timeSize) then
            allocate(cm%clin(indx)%ntimes(1))
            cm%clin(indx)%ntimes(1) = timeStepClimF
        else
            nts = timeStepClimF/cm%clin(indx)%timeSize
            rts = timeStepClimF - cm%clin(indx)%timeSize*nts
            if (rts == 0) then
                allocate(cm%clin(indx)%ntimes(nts))
                cm%clin(indx)%ntimes = cm%clin(indx)%timeSize

            else
                allocate(cm%clin(indx)%ntimes(nts + 1))
                cm%clin(indx)%ntimes = cm%clin(indx)%timeSize
                cm%clin(indx)%ntimes(nts + 1) = rts
            end if
        end if

    end subroutine Init_clim_info

    !> *****************************************************************
    !> Open the MESH_input_forcing.bin file
    !> *****************************************************************
    subroutine climate_module_init(bi, ts, cm, ENDDATA)

        use FLAGS

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

        !> Reset the number of forcing variables not in the forcing binary file.
!        NUM_R2C = 0
!        NUM_CSV = 0
!        NUM_SEQ = 0

        !todo - if we have time (or later), change the binary forcing files to
        !       one for each forcing variable
        !> Only open if there are not enough separate forcing files
!        if (cm%clin(cfk%FB)%filefmt == 0   .or. &
!            cm%clin(cfk%FI)%filefmt == 0  .or. &
!            cm%clin(cfk%PR)%filefmt == 0  .or. &
!            cm%clin(cfk%TT)%filefmt == 0   .or. &
!            cm%clin(cfk%UV)%filefmt == 0   .or. &
!            cm%clin(cfk%P0)%filefmt == 0 .or. &
!            cm%clin(cfk%HU)%filefmt == 0) then
!            open(51, file = 'MESH_input_forcing.bin', status = 'old', form = 'unformatted', action = 'read')
!        end if

        !> Open the rest of the forcing files.
        do i = 1, size(cm%clin)
            call READ_CHECK_FORCING_FILES(bi, ts, i, cm)

            !> Update the file format counters for the legacy binary format.
!            select case (cm%clin(i)%filefmt)
!                case (1, 4)
!                    NUM_R2C = NUM_R2C + 1
!                case (2)
!                    NUM_CSV = NUM_CSV + 1
!                case (3, 5)
!                    NUM_SEQ = NUM_SEQ + 1
!            end select
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

        !> skip the values in the forcing files
!        toskip = 7 - NUM_R2C - NUM_CSV - NUM_SEQ

        do i = 1, nrs

            !> Legacy BIN-format.
!            if (cm%clin(cfk%FB)%filefmt == 0   .or. &
!                cm%clin(cfk%FI)%filefmt == 0  .or. &
!                cm%clin(cfk%PR)%filefmt == 0  .or. &
!                cm%clin(cfk%TT)%filefmt == 0   .or. &
!                cm%clin(cfk%UV)%filefmt == 0   .or. &
!                cm%clin(cfk%P0)%filefmt == 0 .or. &
!                cm%clin(cfk%HU)%filefmt == 0) then
!                do j = 1, toskip
!                    read(51, end = 999) !Skip the bin's information
!                end do
!            end if

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
            FSVHGRD(bi%NA), FSIHGRD(bi%NA), FDLGRD(bi%NA), PREGRD(bi%NA), TAGRD(bi%NA), ULGRD(bi%NA), PRESGRD(bi%NA), &
            QAGRD(bi%NA), VLGRD(bi%NA), FSDOWN(bi%NA))
        FSVHGRD = 0.0
        FSIHGRD = 0.0
        FDLGRD = 0.0
        PREGRD = 0.0
        TAGRD = 0.0
        ULGRD = 0.0
        PRESGRD = 0.0
        QAGRD = 0.0
        VLGRD = 0.0
        FSDOWN = 0.0

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

        use FLAGS

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
            do i = 1, size(cm%clin, 1)
                cm%clin(i)%timestep_now = TIME_STEP_NOW
            end do
            if (INTERPOLATIONFLAG == 0) then
                call READ_FORCING_DATA(bi, cm, &
                                       FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                       FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, &
                                       ENDDATA)
            elseif (INTERPOLATIONFLAG == 1) then
                if (RESUMEFLAG /= 1) then
                    call READ_FORCING_DATA(bi, cm, &
                                           FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                           FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE, TAGATPRE, ULGATPRE, &
                                           PRESGATPRE, QAGATPRE, &
                                           ENDDATA)
                    call READ_FORCING_DATA(bi, cm, &
                                           FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                           FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
                                           PRESGATPST, QAGATPST, &
                                           ENDDATA)
                else
                    call READ_FORCING_DATA(bi, cm, &
                                           FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
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
                                       FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                       FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
                                       PRESGATPST, QAGATPST, &
                                       ENDDATA)
            else
                call READ_FORCING_DATA(bi, cm, &
                                       FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                       FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, &
                                       ENDDATA)
            end if

        end if !(firststep) then

    end subroutine !climate_module_loaddata

    !> -----------------------------------------------------------------
    !> Description: Initialization of climate data
    !>
    !> -----------------------------------------------------------------
    subroutine Init_clim_data(indx, flunit, cm)

        !> Input variables.
        integer, intent(in) :: indx, flunit

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Set the unit number.
        cm%clin(indx)%unitR = flunit

        !> Set the file name.
        if (indx == cfk%FB) then
            cm%clin(indx)%fln = 'basin_shortwave'
        elseif (indx == cfk%FI) then
            cm%clin(indx)%fln = 'basin_longwave'
        elseif (indx == cfk%PR) then
            cm%clin(indx)%fln = 'basin_rain'
!        elseif (indx == 8) then
!            cm%clin(indx)%fln = 'basin_rain_2'
        elseif (indx == cfk%TT) then
            cm%clin(indx)%fln = 'basin_temperature'
        elseif (indx == cfk%UV) then
            cm%clin(indx)%fln = 'basin_wind'
        elseif (indx == cfk%P0) then
            cm%clin(indx)%fln = 'basin_pres'
        elseif (indx == cfk%HU) then
            cm%clin(indx)%fln = 'basin_humidity'
        end if

        !> Call the subroutine to open the data.
        call OpenData(indx, cm)

    end subroutine !Init_clim_data

    !> -----------------------------------------------------------------
    !> Description: Open Units and Load the first block of climate data
    !>
    !> -----------------------------------------------------------------
    subroutine OpenData(indx, cm)

        !> Input variables.
        integer, intent(in) :: indx

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Local variables.
        integer ios
        character*80 end_of_r2c_header

        !> Open file depending on the format type of the climate data.
        select case (cm%clin(indx)%filefmt)

            !> ASCII R2C format.
            case (1)
                print *, cm%clin(indx)%unitR, trim(adjustl(cm%clin(indx)%fln)) // '.r2c'
                open(unit = cm%clin(indx)%unitR, &
                     file = trim(adjustl(cm%clin(indx)%fln)) // '.r2c', &
                     action = 'read', &
                     status = 'old', &
                     form = 'formatted', &
                     iostat = ios)
                if (ios /= 0) then
                    print 670, trim(adjustl(cm%clin(indx)%fln)) // '.r2c'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    print 676, trim(adjustl(cm%clin(indx)%fln)) // '.r2c'
                    end_of_r2c_header = ''
                    do while (end_of_r2c_header /= ":endHeader")
                        read(cm%clin(indx)%unitR, '(A10)') end_of_r2c_header
                    end do
                    cm%clin(indx)%openFl = .true.
                end if

            !> CSV format.
            case (2)
                open(unit = cm%clin(indx)%unitR, &
                     file = trim(adjustl(cm%clin(indx)%fln)) // '.csv', &
                     action = 'read', &
                     status = 'old', &
                     form = 'formatted', &
                     iostat = ios)
                if (ios /=0 ) then
                    print 670, trim(adjustl(cm%clin(indx)%fln)) // '.csv'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    print 676, trim(adjustl(cm%clin(indx)%fln)) // '.csv'
                    cm%clin(indx)%openFl = .true.
                end if

            !> Binary sequential format.
            case (3)
                open(UNIT = cm%clin(indx)%unitR, &
                     FILE = trim(adjustl(cm%clin(indx)%fln)) // '.seq', &
                     action = 'read', &
                     status = 'old', &
                     form = 'unformatted', &
                     access = 'sequential', &
                     iostat = ios)
                if (ios /= 0) then
                    print 670, trim(adjustl(cm%clin(indx)%fln)) // '.seq'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    print 676, trim(adjustl(cm%clin(indx)%fln)) // '.seq'
                    cm%clin(indx)%openFl = .true.
                end if

            !> ASCII format.
            case (4)
                open(cm%clin(indx)%unitR, &
                     file = trim(adjustl(cm%clin(indx)%fln)) // '.asc', &
                     action = 'read', &
                     status = 'old', &
                     form = 'formatted', &
                     iostat = ios)
                if (ios /= 0) then
                    print 670, trim(adjustl(cm%clin(indx)%fln)) // '.asc'
                    cm%clin(indx)%openFl = .false.
                    stop
                else
                    print 676, trim(adjustl(cm%clin(indx)%fln)) // '.asc'
                    cm%clin(indx)%openFl = .true.
                end if

            case default
                print 644, cm%clin(indx)%id_var, cm%clin(indx)%filefmt
                stop
        end select

670 format(/ &
        /1x, a20, ' not found.', &
        /1x, 'Please adjust the MESH_input_run_options.ini file', &
        /1x, 'or put the file in the correct location.', /)

676 format(1x, a20, ' found.')

644 format(/ &
        /1x, 'The input forcing file format is not supported', &
        /2x, a15, i4/)

    end subroutine !OpenData

    !> -----------------------------------------------------------------
    !> Description: Load block of data
    !> -----------------------------------------------------------------
    subroutine LoadData(bi, indx, cm, ENDDATA)

        !> Input variables.
        type(basin_info) :: bi
        integer indx

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Output variables.
        logical ENDDATA

        !> Local variables.
        integer NTIME
        integer t, s, i, j, ii, jj
        real INARRAY(bi%YCOUNT, bi%XCOUNT), INGRU(bi%NTYPE)

        select case (cm%clin(indx)%filefmt)

            !> ASCII R2C format.
            case (1)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR, *, end = 999) !:Frame line
                        do ii = 1, bi%YCOUNT
                            read(cm%clin(indx)%unitR, *, end = 999) (INARRAY(ii, jj), jj = 1, bi%XCOUNT)
                        end do
                        read(cm%clin(indx)%unitR, *, end = 999) !:EndFrame line
                        do i = 1, bi%NA
                            cm%clin(indx)%climv(i, s, t) = INARRAY(bi%YYY(i), bi%XXX(i))
                        end do
                    end do
                end do

            !> CSV format.
            case (2)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR, *, end = 999) (INGRU(i), i = 1, bi%NTYPE)
                        cm%clin(indx)%climv(:, s, t) = 0.0
                        do j = 1, bi%NTYPE
                            do i = 1, bi%NA
                                cm%clin(indx)%climv(i, s, t) = cm%clin(indx)%climv(i, s, t) + INGRU(j)*bi%ACLASS(i, j)
                            end do
                        end do
                    end do
                end do

            !> Binary sequential format.
            case (3)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR) NTIME
                        read(cm%clin(indx)%unitR, end = 999) cm%clin(indx)%climv(:, s, t)
                    end do
                end do

            !> ASCII format.
            case (4)
                do t = 1, size(cm%clin(indx)%climv, 3)
                    do s = 1, size(cm%clin(indx)%climv, 2)
                        read(cm%clin(indx)%unitR, *, end = 999) cm%clin(indx)%climv(:, s, t)
                    end do
                end do

            case default
                print *, 'NOT IMPLEMENTED YET'
                stop
        end select

        return

999 ENDDATA = .true.

    end subroutine !LoadData

    subroutine climate_module_interpolatedata(bi, &
!todo: These variables can be stored elsewhere instead of passed.
        FAREGAT, &
        cm)

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
        FDLGRD = 0.0
        ULGRD = 0.0
        TAGRD = 0.0
        QAGRD = 0.0
        PRESGRD = 0.0
        PREGRD = 0.0
        do k = 1, bi%NML
            if (FAREGAT(k) > 0.0) then
                FSVHGRD(bi%ILMOS(k)) = FSVHGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*FSVHGAT(k)
                FSIHGRD(bi%ILMOS(k)) = FSIHGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*FSIHGAT(k)
                FDLGRD(bi%ILMOS(k)) = FDLGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*FDLGAT(k)
                ULGRD(bi%ILMOS(k)) = ULGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*ULGAT(k)
                TAGRD(bi%ILMOS(k)) = TAGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*TAGAT(k)
                QAGRD(bi%ILMOS(k)) = QAGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*QAGAT(k)
                PRESGRD(bi%ILMOS(k)) = PRESGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*PRESGAT(k)
                PREGRD(bi%ILMOS(k)) = PREGRD(bi%ILMOS(k)) + bi%ACLASS(bi%ILMOS(k), bi%JLMOS(k))*PREGAT(k)
            end if
        end do !k = 1, NML
        FSDOWN = 2.0*FSVHGRD

    end subroutine !climate_module_interpolatedata

    !> -----------------------------------------------------------------
    !> Description: Check if we need to load data again if that, we
    !> deallocate and allocate again and then we load data
    !> -----------------------------------------------------------------
    subroutine NeedUpdate_clim_data(bi, indx, cm, ENDDATA)

        !> Inputs variables.
        type(basin_info) :: bi
        integer indx

        !> Input/Output variables.
        type(clim_info) :: cm

        !> Ouput variables.
        logical ENDDATA

        !> Check if we need to update.
        if (cm%clin(indx)%itime == 1) then
            if (allocated(cm%clin(indx)%climv)) deallocate(cm%clin(indx)%climv)
            allocate(cm%clin(indx)%climv(bi%NA, cm%clin(indx)%nSeries, cm%clin(indx)%ntimes(cm%clin(indx)%readIndx)))
            call LoadData(bi, indx, cm, ENDDATA)
        end if

    end subroutine !NeedUpdate_clim_data

end module !climate_forcing
