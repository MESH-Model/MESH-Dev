         module climate_forcing
         !>******************************************************************************
         !>  Athor: Gonzalo Sapriza Azuri
         !>  Description:
         !> Handled climate forcing data to be loaded in memory
         !>******************************************************************************

    use sa_mesh_shared_variabletypes
    use sa_mesh_shared_variables
    use model_dates

    implicit none

         !>******************************************************************************
!         TYPE clim_data
!
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: shortwave  !1
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: longwave   !2
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: rain       !3
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: temp       !4
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: wind       !5
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: pressure   !6
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: humidity   !7

!         END TYPE
         !>******************************************************************************
         TYPE clim_info_read

            integer :: timeSize  !minimum size of block read DEFINED IN THE INPUT FILE
            integer,dimension(:),allocatable :: ntimes ! number of time in each block of readed data
            character(15) :: id_var !climate variable name
            integer       :: flagID !Basic climage flag id
            integer       :: flagRead !type of format file
            integer :: filefmt = 0
            integer       :: unitR !Number unit
            logical       :: openFl !true if file is open
            integer       :: readIndx !index in the block of time that we are reading
            integer       :: itime    !time index
            character     :: freq !time freq of data
            integer       :: hf !hourly flag
            real,dimension(:,:) ,allocatable :: climv !Climate variable
            real :: alpharain !used only for rainfall

         END TYPE

         TYPE clim_info

            integer :: na !number of cell inside the basin
            integer :: nclim !number of climate variables
            integer :: basefileunit = 89
            type(clim_info_read) :: clin(8) !load extra rainfall
            !type(clim_info_read) :: clin(7)

         END TYPE

    !> *****************************************************************
    !> These are relative indices of the forcing files used throughout
    !> the I/O code for climate forcing.
    !> *****************************************************************
    type climate_forcing_file_keys

        !* FCLO: Fractional cloud cover [-]
        !integer :: FCLO = -9999

        !* FDL: Downwelling longwave sky radiation [W m-2]
        integer :: FDL = 2

        !* FS: Incoming solar radiation [W m-2]
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
        integer :: FS = 1
        !integer :: FSIH = -9999
        !integer :: FSVH = -9999

        !* PRE: Surface precipitation rate [kg m-2 s-1]
        integer :: PRE = 3

        !* PRES: Surface air pressure [Pa]
        integer :: PRES = 6

        !* QA: Specific humidity at reference height [kg kg-1]
        integer :: QA = 7

        !* TA: Air temperature at reference height [K]
        integer :: TA = 4

        !* UL: Zonal component of wind velocity [m s-1]
        !>       CLASS does not actually require information on wind
        !>       direction. Thus, if only the scalar wind
        !>       speed is available, either ULGRD or VLGRD can be set
        !>       to it, and the other to zero.
        !* VL: Meridional component of wind velocity [m s-1]
        integer :: UL = 5
        !integer :: VL = -9999

    end type

        !>******************************************************************************

    !> These variables are used to keep track of the number of forcing files
    !> that are in different formats
    integer NUM_CSV, NUM_R2C, NUM_SEQ

    !> For count times in reading climate forcing data
    integer ITIME

    !* YEAR_START_CLIM: Year at the start of the simulation.
    !* JDAY_START_CLIM: Julian day at the start of the simulation.
    !* HOUR_START_CLIM: Hour at the start of the simulation.
    !* MINS_START_CLIM: Minute (in 30-min. increment; either 0 or 30) at the start of the simulation.
    integer YEAR_START_CLIM, JDAY_START_CLIM, HOUR_START_CLIM, MINS_START_CLIM
    integer JDAY_IND_MET

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
        !>******************************************************************************
         subroutine Init_clim_info(cm,ts,indx,nna)
        !>------------------------------------------------------------------------------
        !>  Description: Initialization of clim_info
        !>
        !>------------------------------------------------------------------------------

         implicit none

         !Input
         integer,intent(in)              :: nna,indx
         type(dates_model),intent(in)    :: ts

         !Input Output
          type(clim_info),intent(inout) :: cm
         !Internals
         integer  :: i,nts,rts,timeStepClimF

         !cm%nclim = 7
         cm%nclim = 8
         cm%na = nna
         i = indx

         timeStepClimF = ts%nr_days*48/real(cm%clin(i)%hf)*30

         if (cm%clin(i)%flagId .ge. 5)then

            if(timeStepClimF.le.cm%clin(i)%timeSize)then

                allocate(cm%clin(i)%ntimes(1))
                cm%clin(i)%ntimes(1) = timeStepClimF

            else

                nts = timeStepClimF/cm%clin(i)%timeSize
                rts = timeStepClimF - cm%clin(i)%timeSize*nts

                if (rts.eq.0)then

                    allocate(cm%clin(i)%ntimes(nts))
                    cm%clin(i)%ntimes = cm%clin(i)%timeSize
                else
                    allocate(cm%clin(i)%ntimes(nts+1))
                    cm%clin(i)%ntimes = cm%clin(i)%timeSize
                    cm%clin(i)%ntimes(nts+1) = rts
                endif

            endif
            cm%clin(i)%readIndx = 1
         endif

!        write(400+indx,*)'flagId',cm%clin(i)%flagId
!        write(400+indx,*)'ntimes',cm%clin(i)%ntimes
!        write(400+indx,*)'timeSize',cm%clin(i)%timeSize
!        write(400+indx,*)'id_var',cm%clin(i)%id_var
!        write(400+indx,*)'flagRead',cm%clin(i)%flagRead
!        close(400+indx)
         end subroutine Init_clim_info

    !> *****************************************************************
    !> Open the MESH_input_forcing.bin file
    !> *****************************************************************
    subroutine climate_module_init(ts, bi, cm, ENDDATA, &
!todo: These variables can be stored elsewhere instead of passed.
        YCOUNT)

        use FLAGS

        !> Input variables.
        type(DATES_MODEL) :: ts
        type(basin_info) :: bi
        type(clim_info) :: cm
        logical ENDDATA
        integer YCOUNT

        !> Local variables.
        !* toskip   : The number of variables in the file per timestep
!        integer nyy, ndy,
        integer ISTEP_START, nmy, nhy, nrs, Jday_IND2, Jday_IND3, toskip
        integer i, j, m

        integer ilg

        !todo - if we have time (or later), change the binary forcing files to
        !       one for each forcing variable
        !> Only open if there are not enough separate forcing files
        if (NUM_R2C + NUM_CSV + NUM_SEQ < 7) then
            open(51, file = 'MESH_input_forcing.bin', status = 'old', form = 'unformatted', action = 'read')
        end if

        !todo - leave these in for event based runs
        !> IYEAR is set in the MESH_parameters_CLASS.ini file
        !> YEAR_START is set in the MESH_input_run_options.ini file
!        nyy = YEAR_START - YEAR_START_CLIM
!        ndy = JDAY_START - JDAY_START_CLIM
        nmy = MINS_START - MINS_START_CLIM
        nhy = HOUR_START - HOUR_START_CLIM

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
        ISTEP_START = 24*60/HOURLYFLAG
        if (mod(24*60, HOURLYFLAG) /= 0) then
            print 2334
            stop
        end if

2334 format( &
    //1x'Forcing data time interval needs to be in either', &
    /1x'of the following values:', &
    /1x'30 or n*60 where n can be either 1, 2, 3, 4, 6, 8 or 12.'/)

        call Julian_Day_ID(YEAR_START, JDAY_START, Jday_IND2)
        call Julian_Day_ID(YEAR_START_CLIM, JDAY_START_CLIM, Jday_IND3)
        if ((Jday_IND2 < Jday_IND3) .and. (YEAR_START /= 0)) then
            print 2442
            stop
        end if

2442 format( &
    //1x'ERROR: Simulation start date too early. The start date in the', &
    /3x'run options file may occur before the start date of the met.', &
    /3x'forcing input data in the CLASS parameter file.'/)

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
        toskip = 7 - NUM_R2C - NUM_CSV - NUM_SEQ

        do i = 1, nrs

            !> Legacy BIN-format.
            do j = 1, toskip
                read(51, end = 999) !Skip the bin's information
            end do

            !> R2C-format (ASCII).
            if (cm%clin(cfk%FS)%filefmt == 1) then !Skip the r2c file's information
                read(90, *, end = 999)
                do m = 1, YCOUNT
                    Read (90, *, end = 999)
                end do
                read (90, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%FDL)%filefmt == 1) then
                read(91, *, end = 999) !:Frame line
                do m = 1, YCOUNT
                    read(91, *, end = 999)
                end do
                read(91, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%PRE)%filefmt == 1) then
                read(92, *, end = 999) !:Frame line
                do m = 1, YCOUNT
                    read(92, *, end = 999)
                end do
                read(92, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%TA)%filefmt == 1) then
                read(93, *, END=999) !:Frame line
                do m = 1, YCOUNT
                    read(93, *, end = 999)
                end do
                read(93, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%UL)%filefmt == 1) then
                read(94, *, end = 999) !:Frame line
                do m = 1, YCOUNT
                    read(94, *, end = 999)
                end do
                read(94, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%PRES)%filefmt == 1) then
                read(95, *, end = 999) !:Frame line
                do m = 1, YCOUNT
                    read(95, *, end = 999)
                end do
                read(95, *, end = 999) !:EndFrame line
            end if
            if (cm%clin(cfk%QA)%filefmt == 1) then
                read(96, *, end = 999) !:Frame line
                do m = 1, YCOUNT
                    read(96, *, end = 999)
                end do
                read(96, *, end = 999)
            end if

            !> CSV-format.
            if (cm%clin(cfk%FS)%filefmt == 2) then !Skip the csv file's information
                read(90, * , end = 999)
            end if
            if (cm%clin(cfk%FDL)%filefmt == 2) then
                read(91, *, end = 999)
            end if
            if (cm%clin(cfk%PRE)%filefmt == 2) then
                read(92, *, end = 999)
            end if
            if (cm%clin(cfk%TA)%filefmt == 2) then
                read(93, *, end = 999)
            end if
            if (cm%clin(cfk%UL)%filefmt == 2) then
                read(94, *, end = 999)
            end if
            if (cm%clin(cfk%PRES)%filefmt == 2) then
                read(95, *, end = 999)
            end if
            if (cm%clin(cfk%QA)%filefmt == 2) then
                read(96, *, end = 999)
            end if

        end do !i = 1, nrs

        !> Allocate and initialize GRD variables.
        allocate( &
            FSVHGRD(bi%na), FSIHGRD(bi%na), FDLGRD(bi%na), PREGRD(bi%na), TAGRD(bi%na), ULGRD(bi%na), PRESGRD(bi%na), &
            QAGRD(bi%na), VLGRD(bi%na), FSDOWN(bi%na))
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
        ilg = bi%na*bi%nm
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
    subroutine climate_module_loaddata(bi, cm, firststep, ENDDATA, &
!todo: These variables can be stored elsewhere instead of passed.
        YCOUNT, XCOUNT, NML, ILMOS, JLMOS, YYY, XXX, ACLASS)

        use FLAGS

        !> Input variables.
        type(basin_info) :: bi
        type(CLIM_INFO) :: cm
        logical firststep, ENDDATA
        integer YCOUNT, XCOUNT, NML, ILMOS(:), JLMOS(:), &
            YYY(:), XXX(:)
        real ACLASS(:, :)

        !> Local variables.
        integer ilg

        ilg = bi%NA*bi%NM
        if (firststep) then
            if (INTERPOLATIONFLAG == 0) then
                call READ_FORCING_DATA(YCOUNT, XCOUNT, bi%NM, bi%NA, NML, ILG, ILMOS, JLMOS, YYY, XXX, ENDDATA, ACLASS, &
                                       FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                       FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, 1, cm)
                ITIME = 2
            elseif (INTERPOLATIONFLAG == 1) then
                if (RESUMEFLAG /= 1) then
                    call READ_FORCING_DATA(YCOUNT, XCOUNT, bi%NM, bi%NA, NML, ILG, ILMOS, JLMOS, YYY, XXX, ENDDATA, ACLASS, &
                                           FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                           FSVHGATPRE, FSIHGATPRE, FDLGATPRE, PREGATPRE, TAGATPRE, ULGATPRE, &
                                           PRESGATPRE, QAGATPRE, 1, cm)
                    call READ_FORCING_DATA(YCOUNT, XCOUNT, bi%NM, bi%NA, NML, ILG, ILMOS, JLMOS, YYY, XXX, ENDDATA, ACLASS, &
                                           FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                           FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
                                           PRESGATPST, QAGATPST, 2, cm)
                    ITIME = 3
                else
                    call READ_FORCING_DATA(YCOUNT, XCOUNT, bi%NM, bi%NA, NML, ILG, ILMOS, JLMOS, YYY, XXX, ENDDATA, ACLASS, &
                                           FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                           FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
                                           PRESGATPST, QAGATPST, 1, cm)
                    ITIME = 2
                end if
            end if
            VLGRD = 0.0
            VLGAT = 0.0
        else
            if (INTERPOLATIONFLAG == 1) then
                FSVHGATPRE = FSVHGATPST
                FSIHGATPRE = FSIHGATPST
                FDLGATPRE = FDLGATPST
                PREGATPRE = PREGATPST
                TAGATPRE = TAGATPST
                ULGATPRE = ULGATPST
                PRESGATPRE = PRESGATPST
                QAGATPRE = QAGATPST
                call READ_FORCING_DATA(YCOUNT, XCOUNT, bi%NM, bi%NA, NML, ILG, ILMOS, JLMOS, YYY, XXX, ENDDATA, ACLASS, &
                                       FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                       FSVHGATPST, FSIHGATPST, FDLGATPST, PREGATPST, TAGATPST, ULGATPST, &
                                       PRESGATPST, QAGATPST, ITIME, cm)
            else
                call READ_FORCING_DATA(YCOUNT, XCOUNT, bi%NM, bi%NA, NML, ILG, ILMOS, JLMOS, YYY, XXX, ENDDATA, ACLASS, &
                                       FSDOWN, FSVHGRD, FSIHGRD, FDLGRD, PREGRD, TAGRD, ULGRD, PRESGRD, QAGRD, &
                                       FSVHGAT, FSIHGAT, FDLGAT, PREGAT, TAGAT, ULGAT, PRESGAT, QAGAT, ITIME, cm)
            end if
            ITIME = ITIME + 1
        end if !(firststep) then

    end subroutine !climate_module_loaddata

        !>******************************************************************************
         subroutine Init_clim_data(cm,idvar,unitR)
        !>------------------------------------------------------------------------------
        !>  Description: Initialization of climate data
        !>
        !>------------------------------------------------------------------------------
         implicit none

         !Inputs
         integer,intent(in) :: unitR
         character(len=*),intent(in) :: idvar

         !Inputs Output
         type(clim_info),intent(inout) :: cm



         if (idvar .eq. 'shortwave')then

            allocate(cm%clin(1)%climv(cm%na,cm%clin(1)%ntimes(1)))
            cm%clin(1)%unitR = unitR
            call OpenData(cm,'basin_shortwave',1)

         endif

         if (idvar .eq. 'longwave')then

            allocate(cm%clin(2)%climv(cm%na,cm%clin(2)%ntimes(1)))
            cm%clin(2)%unitR = unitR
            call OpenData(cm,'basin_longwave',2)

         endif

         if (idvar .eq. 'rain')then

            allocate(cm%clin(3)%climv(cm%na,cm%clin(3)%ntimes(1)))
            cm%clin(3)%unitR = unitR
            call OpenData(cm,'basin_rain',3)

         endif
         
         if (idvar .eq. 'rain_2')then

            allocate(cm%clin(8)%climv(cm%na,cm%clin(8)%ntimes(1)))
            cm%clin(8)%unitR = unitR
            call OpenData(cm,'basin_rain_2',8)

         endif         

         if (idvar .eq. 'temp')then

            allocate(cm%clin(4)%climv(cm%na,cm%clin(4)%ntimes(1)))
            cm%clin(4)%unitR = unitR
            call OpenData(cm,'basin_temperature',4)

         endif

         if (idvar .eq. 'wind')then

            allocate(cm%clin(5)%climv(cm%na,cm%clin(5)%ntimes(1)))
            cm%clin(5)%unitR = unitR
            call OpenData(cm,'basin_wind',5)

         endif

         if (idvar .eq. 'pressure')then

            allocate(cm%clin(6)%climv(cm%na,cm%clin(6)%ntimes(1)))
            cm%clin(6)%unitR = unitR
            call OpenData(cm,'basin_pres',6)

         endif

         if (idvar .eq. 'humidity')then

            allocate(cm%clin(7)%climv(cm%na,cm%clin(7)%ntimes(1)))
            cm%clin(7)%unitR = unitR
            call OpenData(cm,'basin_humidity',7)

         endif


         end subroutine Init_clim_data

        !>******************************************************************************
         subroutine OpenData(cm,flname,indx)
        !>------------------------------------------------------------------------------
        !>  Description: Open Units and Load the first block of climate data
        !>
        !>------------------------------------------------------------------------------
         implicit none

         !Input
         integer,intent(in)  :: indx
         character(len=*),intent(in):: flname

         type(clim_info) :: cm
         integer   :: ios
         character*80 :: end_of_r2c_header

         !>Open file depending on the format type of the climate data
         !> r2c format
         if     (cm%clin(indx)%flagRead .eq. 1) then

            print*, cm%clin(indx)%unitR ,trim(adjustl(flname))//'.r2c'

            OPEN(unit   = cm%clin(indx)%unitR           , &
                 file   = trim(adjustl(flname))//'.r2c' , &
                 STATUS = 'OLD'                         , &
                 IOSTAT = IOS                           )

         !> IOS would be 0 if the file opened successfully.

            IF(IOS/=0)THEN
              !>no basin longwave file exists
              PRINT *, trim(adjustl(flname))//'.r2c'//'not found'
              PRINT *, 'please adjust the mesh_input_run_options.ini file,'
              PRINT *, 'or put the r2c file in the correct location.'
              CLOSE(cm%clin(indx)%unitR)
              cm%clin(indx)%openFl = .false.
              STOP
            ELSE
              PRINT *, trim(adjustl(flname))//'.r2c'//' found'
               end_of_r2c_header = ""
              DO WHILE (end_of_r2c_header /= ":endHeader")
                READ (cm%clin(indx)%unitR, '(A10)') end_of_r2c_header
              ENDDO

              cm%clin(indx)%openFl = .true.

            ENDIF


         elseif (cm%clin(indx)%flagRead .eq. 2) then

            OPEN(unit   = cm%clin(indx)%unitR            , &
                 file   = trim(adjustl(flname))//'.csv'  , &
                 STATUS = 'OLD'                          , &
                 IOSTAT = IOS                            )

            !> IOS would be 0 if the file opened successfully.
            IF(IOS/=0)THEN
              !> no basin longwave file exists
              PRINT *, trim(adjustl(flname))//'.csv'//'not found'
              PRINT *, 'please adjust the mesh_input_run_options.ini file,'
              PRINT *, 'or put the csv file in the correct location.'
              CLOSE(cm%clin(indx)%unitR)
              cm%clin(indx)%openFl = .false.
              STOP
             ELSE
                PRINT *, trim(adjustl(flname))//'.csv'//' found'
                cm%clin(indx)%openFl = .true.
             ENDIF

         elseif (cm%clin(indx)%flagRead .eq. 3) then

            OPEN(UNIT   = cm%clin(indx)%unitR           , &
                 FILE   = trim(adjustl(flname))//'.seq' , &
                 STATUS = 'OLD'                         , &
                 FORM   = 'unformatted'                 , &
                 ACTION = 'read'                        , &
                 ACCESS = 'sequential'                  , &
                 IOSTAT = IOS                           )

            IF(IOS/=0)THEN
              !> no basin shortwave file exists
              PRINT *, trim(adjustl(flname))//'.seq'//'not found'
              PRINT *, 'please adjust the mesh_input_run_options.ini file'
              PRINT *, 'or put the seq file in the correct location.'
              CLOSE(cm%clin(indx)%unitR)
              cm%clin(indx)%openFl = .false.
              STOP
            ELSE

              PRINT *, trim(adjustl(flname))//'.seq'//' found'
              cm%clin(indx)%openFl = .true.
            ENDIF


         elseif (cm%clin(indx)%flagRead .eq. 4) then

            OPEN(UNIT   =  cm%clin(indx)%unitR          , &
                 FILE   = trim(adjustl(flname))//'.asc' , &
                 STATUS = 'OLD'                         , &
                 FORM   = 'formatted'                   , &
                 ACTION = 'read'                        , &
                 IOSTAT =  IOS                          )

            IF(IOS/=0)THEN
              !> no basin shortwave file exists
              PRINT *, trim(adjustl(flname))//'.asc'//'not found'
              PRINT *, 'please adjust the mesh_input_run_options.ini file'
              PRINT *, 'or put the seq file in the correct location.'
              CLOSE(cm%clin(indx)%unitR)
              cm%clin(indx)%openFl = .false.
              STOP
            ELSE

              PRINT *, trim(adjustl(flname))//'.asc'//' found'
              cm%clin(indx)%openFl = .true.
            ENDIF

         endif

         end subroutine OpenData

        !>******************************************************************************
         subroutine LoadData(cm,indx,xcount,ycount,xxx,yyy,na,enddata)

        !>------------------------------------------------------------------------------
        !>  Description: Load block of data
        !>
        !>------------------------------------------------------------------------------
         implicit none
         type(clim_info) :: cm


         !Inputs variables
         integer :: XCOUNT,YCOUNT,NA
         integer :: XXX(NA),YYY(NA)
         integer :: indx

         !output
         logical :: enddata

         !Internals variables
         integer :: tm(2),ntime
         integer :: i,j,ii,jj
         real    :: R4GRID2D(YCOUNT,XCOUNT)

         i = indx

            if (cm%clin(i)%flagId .ge. 5)then

                tm  = shape(cm%clin(i)%climv)
                !> r2c file format
                if     (cm%clin(i)%flagRead .eq. 1) then
                    do j = 1, tm(2)
                        READ(cm%clin(i)%unitR, *, END=999) !:Frame line
                        do ii = 1,YCOUNT
                            READ(cm%clin(i)%unitR, *, END=999) (R4GRID2D(ii,jj),jj=1,XCOUNT)
                        enddo
                        READ(cm%clin(i)%unitR, *, END=999) !:EndFrame line
                        do ii=1,NA
                           cm%clin(i)%climv(ii,j)=R4GRID2D(YYY(ii),XXX(ii))
                        enddo
                     enddo

                elseif (cm%clin(i)%flagRead .eq. 2) then

                    print*, 'NOT IMPLEMENTED YET'
                    STOP

                elseif (cm%clin(i)%flagRead .eq. 3) then

                    do j = 1, tm(2)

                        READ(unit=cm%clin(i)%unitR) NTIME

                        READ(unit=cm%clin(i)%unitR,END=999) cm%clin(i)%climv(:,j)

                    enddo

                elseif (cm%clin(i)%flagRead .eq. 4) then

                    do j = 1, tm(2)

                        READ(cm%clin(i)%unitR,*,END=999) (cm%clin(i)%climv(ii,j),ii=1,tm(1))

                    enddo

                endif
            endif
         return
999 ENDDATA = .TRUE.
         end subroutine LoadData

    subroutine climate_module_interpolatedata(bi, cm, &
!todo: These variables can be stored elsewhere instead of passed.
        ACLASS, FAREGAT, NML, ILMOS, JLMOS)

        !> Input variables.
        type(basin_info) :: bi
        type(CLIM_INFO) :: cm
        real ACLASS(:, :), FAREGAT(:)
        integer NML, ILMOS(:), JLMOS(:)

        !> Local variables.
        integer k

!        TRATIO = min(1.0, real(TIME_STEP_NOW)/HOURLYFLAG)

        TRATIO = min(1.0, real(TIME_STEP_NOW)/cm%clin(1)%hf); FSVHGAT = FSVHGATPRE + TRATIO*(FSVHGATPST - FSVHGATPRE)
        TRATIO = min(1.0, real(TIME_STEP_NOW)/cm%clin(1)%hf); FSIHGAT = FSIHGATPRE + TRATIO*(FSIHGATPST - FSIHGATPRE)
        TRATIO = min(1.0, real(TIME_STEP_NOW)/cm%clin(2)%hf); FDLGAT = FDLGATPRE + TRATIO*(FDLGATPST - FDLGATPRE)
        TRATIO = min(1.0, real(TIME_STEP_NOW)/cm%clin(3)%hf); PREGAT = PREGATPRE + TRATIO*(PREGATPST - PREGATPRE)
        TRATIO = min(1.0, real(TIME_STEP_NOW)/cm%clin(4)%hf); TAGAT = TAGATPRE + TRATIO*(TAGATPST - TAGATPRE)
        TRATIO = min(1.0, real(TIME_STEP_NOW)/cm%clin(5)%hf); ULGAT = ULGATPRE + TRATIO*(ULGATPST - ULGATPRE)
        TRATIO = min(1.0, real(TIME_STEP_NOW)/cm%clin(6)%hf); PRESGAT = PRESGATPRE + TRATIO*(PRESGATPST - PRESGATPRE)
        TRATIO = min(1.0, real(TIME_STEP_NOW)/cm%clin(7)%hf); QAGAT = QAGATPRE + TRATIO*(QAGATPST - QAGATPRE)

        !> INTERPOLATE GRD VARIABLES
        FSVHGRD = 0.0
        FSIHGRD = 0.0
        FDLGRD = 0.0
        ULGRD = 0.0
        TAGRD = 0.0
        QAGRD = 0.0
        PRESGRD = 0.0
        PREGRD = 0.0

        do k = 1, NML
            if (FAREGAT(k) > 0.0) then
                FSVHGRD(ILMOS(k)) = FSVHGRD(ILMOS(k)) + ACLASS(ILMOS(k), JLMOS(k))*FSVHGAT(k)
                FSIHGRD(ILMOS(k)) = FSIHGRD(ILMOS(k)) + ACLASS(ILMOS(k), JLMOS(k))*FSIHGAT(k)
                FDLGRD(ILMOS(k)) = FDLGRD(ILMOS(k)) + ACLASS(ILMOS(k), JLMOS(k))*FDLGAT(k)
                ULGRD(ILMOS(k)) = ULGRD(ILMOS(k)) + ACLASS(ILMOS(k), JLMOS(k))*ULGAT(k)
                TAGRD(ILMOS(k)) = TAGRD(ILMOS(k)) + ACLASS(ILMOS(k), JLMOS(k))*TAGAT(k)
                QAGRD(ILMOS(k)) = QAGRD(ILMOS(k)) + ACLASS(ILMOS(k), JLMOS(k))*QAGAT(k)
                PRESGRD(ILMOS(k)) = PRESGRD(ILMOS(k)) + ACLASS(ILMOS(k), JLMOS(k))*PRESGAT(k)
                PREGRD(ILMOS(k)) = PREGRD(ILMOS(k)) + ACLASS(ILMOS(k), JLMOS(k))*PREGAT(k)
            end if
        end do !k = 1, NML
        FSDOWN = 2.0*FSVHGRD

    end subroutine !climate_module_interpolatedata

        !>******************************************************************************
         subroutine NeedUpdate_clim_data(cm,indx,timeC,xcount,ycount,xxx,yyy,na,enddata)

        !>------------------------------------------------------------------------------
        !>  Description: Check if we need to load data again if that, we deallocate
        !>  and allocate again and then we load data
        !>------------------------------------------------------------------------------
         implicit none
         type(clim_info) :: cm

         !Inputs variables
         integer :: XCOUNT,YCOUNT,NA
         integer :: XXX(NA),YYY(NA)
         integer :: timeC,indx

         !Ouput
         logical :: enddata

         integer :: i,tm(2),nstp,ss

         i = indx

         if (cm%clin(i)%flagId .ge. 5)then

            tm  = shape(cm%clin(i)%climv)

            if (timeC .eq. 1)then

                call LoadData(cm,i,xcount,ycount,xxx,yyy,na,enddata)

                nstp = 0

            !> Check if we need update
            elseif (cm%clin(i)%itime .eq. tm(2))then

                cm%clin(i)%readIndx = cm%clin(i)%readIndx + 1

                ss = size(cm%clin(i)%ntimes)

                if (cm%clin(i)%readIndx  .gt.  ss ) then

                    enddata = .true.

                    return

                else

                    deallocate(cm%clin(i)%climv)

                    allocate(cm%clin(i)%climv(cm%na,cm%clin(i)%ntimes(cm%clin(i)%readIndx)))

                    call LoadData(cm,i,xcount,ycount,xxx,yyy,na,enddata)

                    nstp = cm%clin(i)%ntimes(cm%clin(i)%readIndx-1)

                endif

            else

                nstp = cm%clin(i)%ntimes(cm%clin(i)%readIndx-1)

            endif

            cm%clin(i)%itime = timeC -  (cm%clin(i)%readIndx-1)*nstp

         endif



         end subroutine NeedUpdate_clim_data



         end module climate_forcing
