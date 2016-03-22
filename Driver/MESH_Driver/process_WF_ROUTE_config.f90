module process_WF_ROUTE_config

    use model_files_variabletypes, only: fl_ids

    implicit none

    !> *****************************************************************
    !> Object variables.
    !> *****************************************************************

    !>
    !> Description: Two types define objects used throughout the
    !>              module. WF_RTE_flags contains flags to enable and
    !>              disable the module and control its output.
    !>              WF_RTE_file_keys contains indices to objects
    !>              containing file information. WR_RTE_fls uses the
    !>              fls_id type from module_files_variabletypes to
    !>              store this information.
    !>

    type WF_RTE_flags

        !> Flag used to enable the module.
        logical :: PROCESS_ACTIVE = .true.

        integer :: STREAMFLOWFLAG = 1
        integer :: STREAMFLOWOUTFLAG = 2

    end type

    !> WF_RTE_flgs: Configuration flags for the module.
    type(WF_RTE_flags), save :: WF_RTE_flgs

    type WF_RTE_file_keys

        !> Input files.
        !* stfl_in: MESH_input_streamflow.txt
        !* resv_in: MESH_input_reservoir.txt
        integer :: stfl_in = 1
        integer :: resv_in = 2

        !> Output files.
        !* stfl_daily: MESH_output_streamflow.csv
        !* stfl_cumm: MESH_output_streamflow_cumulative.csv
        !* stfl_ts: MESH_output_streamflow_all.csv
        integer :: stfl_daily = 3
        integer :: stfl_cumm = 4
        integer :: stfl_ts = 5

    end type

    !> WF_RTE_fls: Stores information about files used by the module.
    type(fl_ids), save :: WF_RTE_fls

    type(WF_RTE_file_keys), save :: WF_RTE_flks

    !> *****************************************************************
    !> Local variables.
    !> *****************************************************************

    !>
    !> Description: Variables used by WF_ROUTE. These variables are used
    !> by WF_ROUTE and are only accessible to code that use this module.
    !>

    integer M_S, M_R
!-    integer, parameter :: M_C = 5
    !integer, parameter :: M_S = 290, M_R = 7, M_C = 5
    !M_S and M_R are now read in and used to allocate the appropriate arrays - Frank S Jul 2013
!todo it should be read in from the shd file
!todo M_S could be removed as it is now just a surrogate of WF_NO (KCK)

    !> STREAMFLOW VARIABLES
    !* WF_GAGE: GAUGE IDENTIFIER (8 CHARACTER STRING)
    !* WF_NO: NUMBER OF STREAMFLOW GAUGES
    !* WF_NL: NUMBER OF DATA POINTS
    !* WF_MHRD: NUMBER OF HOURS OF DATA PER MONTH
    !* WF_KT: HOURLY INCREMENT FOR STREAMFLOW INPUT (24 = DAILY)
    !* WF_IY: Y-DIRECTION GAUGE CO-ORDINATE (UTM OR LATLONG)
    !* WF_JX: X-DIRECTION GAUGE CO-ORDINATE (UTM OR LATLONG)
    !* WF_S: GAUGE'S PARENT GRID SQUARE
    !* WF_QHYD: STREAMFLOW VALUE (_AVG = DAILY AVERAGE)
    !* WF_QSYN: SIMULATED STREAFLOW VALUE (_AVG = DAILY AVERAGE)
    !* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
    !* WF_START_DAY OBSERVED STREAMFLOW START DAY
    !* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
    integer WF_NAA, WF_NO, WF_NL, WF_MHRD, WF_KT
    integer, dimension(:), allocatable :: WF_IY, WF_JX, WF_S
    real, dimension(:), allocatable :: WF_QHYD, WF_QHYD_AVG, WF_QHYD_CUM
    real, dimension(:), allocatable :: WF_QSYN, WF_QSYN_AVG, WF_QSYN_CUM
    character(8), dimension(:), allocatable :: WF_GAGE

    !> RESERVOIR VARIABLES
    integer, dimension(:), allocatable :: WF_IRES, WF_JRES, WF_RES, WF_R
    real, dimension(:), allocatable :: WF_B1, WF_B2, WF_QREL, WF_RESSTORE
    character(8), dimension(:), allocatable :: WF_RESNAME

    !> FOR BASEFLOW INITIALIZATION
    integer JAN

    !* WF_R1: MANNING'S N FOR RIVER CHANNEL
    !* WF_R2: OPTIMIZED RIVER ROUGHNESS FACTOR
    !* WF_QO2: SIMULATED STREAMFLOW VALUE
!-    real WF_R1(M_C), WF_R2(M_C)
    real, dimension(:), allocatable :: WF_NHYD, WF_QBASE, WF_QI2, &
        WF_QO1, WF_QO2, WF_QR, WF_STORE1, WF_STORE2, WF_QI1

    !> RESERVOIR MEASUREMENTS:
    !* WF_RESNAME: RESERVOIR IDENTIFIER (8 CHARACTER STRING)
    !* WF_NORESV: NUMBER OF RESERVOIRS
    !* WR_NREL: NUMBER OF DATA POINTS
    !* WF_KTR: HOURLY INCREMENT FOR RESERVOIR INPUR (24 = DAILY)
    !* WF_IRES: Y-DIRECTION GAUGE CO-ORDINATE
    !* WF_JRES: X-DIRECTION GAUGE CO-ORDINATE
    !* WF_R: RESERVOIR'S PARENT GRID SQUARE
    !* WF_QREL: RESERVOIR VALUE
    integer WF_NORESV, WF_NREL, WF_KTR, WF_NORESV_CTRL
    integer WF_ROUTETIMESTEP, WF_TIMECOUNT, DRIVERTIMESTEP

    contains

    !> *****************************************************************
    !> Subroutines.
    !> *****************************************************************

    !>
    !> Description: Allocate the object containing file information.
    !>
    subroutine configure_WF_ROUTE_fls()

        !> Allocate file object.
        allocate(WF_RTE_fls%fl(5))

    end subroutine

    !>
    !> Description: Check for the existence of input files, open them,
    !>              print diagnostic information, skip records, and open
    !>              the output files, in preparation for running the
    !>              WF_ROUTE process.
    !>
    subroutine run_WF_ROUTE_init(shd, fls, ic, stfl, rrls)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use model_output_variabletypes

        !> For: LOCATIONFLAG, STREAMFLOWOUTFLAG
        use FLAGS

        type(ShedGridParams), intent(in) :: shd
        type(fl_ids) :: fls
        type(iter_counter), intent(in) :: ic
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        !* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
        !* WF_START_DAY OBSERVED STREAMFLOW START DAY
        !* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
        integer NA
        integer WF_START_YEAR, WF_START_DAY, WF_START_HOUR
        integer JDAY_IND_STRM, JDAY_IND1, JDAY_IND2
        real I_G, J_G
        integer i, j, ierr, iun

        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

        NA = shd%NA
        WF_NAA = NA - shd%NAA

        allocate(WF_NHYD(NA), WF_QR(NA), &
                 WF_QBASE(NA), WF_QI2(NA), WF_QO1(NA), WF_QO2(NA), &
                 WF_STORE1(NA), WF_STORE2(NA), WF_QI1(NA))

        WF_NHYD = 0.0
        WF_QBASE = 0.0
        WF_QI2 = 0.0
        WF_QO1 = 0.0
        WF_QO2 = 0.0
        WF_QR = 0.0
        WF_STORE1 = 0.0
        WF_STORE2 = 0.0
        WF_QI1 = 0.0

        !> *************************************************************
        !>  Open and read in values from MESH_input_reservoir.txt file
        !> *************************************************************

        iun = WF_RTE_fls%fl(WF_RTE_flks%resv_in)%iun
        open(iun, file = WF_RTE_fls%fl(WF_RTE_flks%resv_in)%fn, status = 'old', action = 'read')
        read(iun, '(3i5)') WF_NORESV, WF_NREL, WF_KTR
        WF_NORESV_CTRL = 0

        ! allocate reservoir arrays
        M_R = WF_NORESV
        allocate(WF_IRES(M_R), WF_JRES(M_R), WF_RES(M_R), WF_R(M_R), WF_B1(M_R), WF_B2(M_R), &
                 WF_QREL(M_R), WF_RESSTORE(M_R), WF_RESNAME(M_R))

        if (WF_NORESV > 0) then
            do i = 1, WF_NORESV
                ! KCK Added to allow higher precision gauge sites
                if (LOCATIONFLAG == 1) then
                    read(iun, '(2f7.1, 2g10.3, 25x, a12, i2)') I_G, J_G, WF_B1(i), WF_B2(i), WF_RESNAME(i), WF_RES(i)
                    WF_IRES(i) = nint((I_G - shd%yOrigin*60.0)/shd%GRDN)
                    WF_JRES(i) = nint((J_G - shd%xOrigin*60.0)/shd%GRDE)
                else
                    read(iun, '(2i5, 2g10.3, 25x, a12, i2)') WF_IRES(i), WF_JRES(i), WF_B1(i), WF_B2(i), WF_RESNAME(i), WF_RES(i)
                    WF_IRES(i) = int((real(WF_IRES(i)) - real(shd%iyMin))/shd%GRDN + 1.0)
                    WF_JRES(i) = int((real(WF_JRES(i)) - real(shd%jxMin))/shd%GRDE + 1.0)
                end if
                !> check if point is in watershed and in river reaches
                WF_R(i) = 0
                do j = 1, NA
                    if (WF_IRES(i) == shd%yyy(j) .and. WF_JRES(i) == shd%xxx(j)) then
                        WF_R(i) = j
                    end if
                end do
                if (WF_R(i) == 0) then
                    print *, 'Reservoir Station: ', i, ' is not in the basin'
                    print *, 'Up/Down Coordinate: ', WF_IRES(i), shd%iyMin
                    print *, 'Left/Right Coordinate: ', WF_JRES(i), shd%jxMin
                    stop
                end if
                if (shd%IREACH(WF_R(i)) /= i) then
                    print *, 'Reservoir Station: ', i, ' is not in the correct reach'
                    print *, 'Up/Down Coordinate: ', WF_IRES(i)
                    print *, 'Left/Right Coordinate: ', WF_JRES(i)
!todo: This will crash. WF_IY is used before it's allocated.
                    print *, 'ireach value at station: ', WF_IY(i)
                    stop
                end if
                if (WF_B1(i) == 0.0) then
                    WF_NORESV_CTRL = WF_NORESV_CTRL + 1
                end if
            end do
        end if
        !> leave file open and read in the reservoir files when needed

        !> *********************************************************************
        !> Open and read in values from MESH_input_streamflow.txt file
        !> *********************************************************************

        iun = WF_RTE_fls%fl(WF_RTE_flks%stfl_in)%iun
        open(iun, file = WF_RTE_fls%fl(WF_RTE_flks%stfl_in)%fn, status = 'old', action = 'read')
        read(iun, *)
        read(iun, *) WF_NO, WF_NL, WF_MHRD, WF_KT, WF_START_YEAR, WF_START_DAY, WF_START_HOUR

! Allocate variable based on value from streamflow file
        M_S = WF_NO !todo M_S is same as WF_NO and could be removed.

        allocate(WF_IY(M_S), WF_JX(M_S), WF_S(M_S), WF_QHYD(M_S), WF_QHYD_AVG(M_S), WF_QHYD_CUM(M_S), &
                 WF_QSYN(M_S), WF_QSYN_AVG(M_S), WF_QSYN_CUM(M_S), WF_GAGE(M_S))

        do i = 1, WF_NO
            if (LOCATIONFLAG == 1) then
                read(iun, *) I_G, J_G, WF_GAGE(i)
                WF_IY(i) = nint((I_G - shd%yOrigin*60.0)/shd%GRDN)
                WF_JX(i) = nint((J_G - shd%xOrigin*60.0)/shd%GRDE)
            else
                read(iun, *) WF_IY(i), WF_JX(i), WF_GAGE(i)
                WF_IY(i) = int((real(WF_IY(i)) - real(shd%iyMin))/shd%GRDN + 1.0)
                WF_JX(i) = int((real(WF_JX(i)) - real(shd%jxMin))/shd%GRDE + 1.0)
            end if
        end do
        do i = 1, WF_NO
            WF_S(i) = 0
            do j = 1, NA
                if (WF_JX(i) == shd%xxx(j) .and. WF_IY(i) == shd%yyy(j)) then
                    WF_S(i) = j
                end if
            end do
            if (WF_S(i) == 0) then
                print *, 'STREAMFLOW GAUGE: ', i, ' IS NOT IN THE BASIN'
                print *, 'UP/DOWN', WF_IY(i), shd%iyMin, shd%yyy(j), shd%yCount
                print *, 'LEFT/RIGHT', WF_JX(i), shd%jxMin, shd%xxx(j), shd%xCount
                stop
            end if
        end do

        if (ro%VERBOSEMODE > 0) then
            print *, 'NUMBER OF STREAMFLOW GUAGES: ', WF_NO
            do i = 1, WF_NO
                print *, 'STREAMFLOW STATION: ', i, 'I: ', WF_IY(i), 'J: ', WF_JX(i)
            end do
            print *, 'NUMBER OF RESERVOIR STATIONS: ', WF_NORESV
            if (WF_NORESV > 0) then
                do i = 1, WF_NORESV
                    print *, 'RESERVOIR STATION: ', i, 'I: ', WF_IRES(i), 'J: ', WF_JRES(i)
                end do
            end if
        end if !(ro%VERBOSEMODE > 0) then

        !> ric     initialise smoothed variables
        WF_QSYN = 0.0
        WF_QSYN_AVG = 0.0
        WF_QHYD_AVG = 0.0
        WF_QSYN_CUM = 0.0
        WF_QHYD_CUM = 0.0

        !> Allocate the output variable for the streamflow hydrograph.
        stfl%ns = WF_NO
        allocate(stfl%qhyd(WF_NO), stfl%qsyn(WF_NO))
        stfl%qhyd = 0.0
        stfl%qsyn = 0.0

        !>MAM - The first stream flow record is used for flow initialization
        read(iun, *, iostat = ierr) (WF_QHYD(i), i = 1, WF_NO)
        backspace(iun)

        ! fixed streamflow start time bug. add in function to enable the
        ! correct start time. Feb2009 aliu.
        call Julian_Day_ID(WF_START_YEAR, WF_START_DAY, JDAY_IND1)
        call Julian_Day_ID(YEAR_START, JDAY_START, JDAY_IND2)
!        print *, WF_START_YEAR, WF_START_DAY, JDAY_IND1
        if (YEAR_START == 0) then
            JDAY_IND2 = JDAY_IND1
        end if
        if (JDAY_IND2 < JDAY_IND1) then
            print *, 'ERROR: Simulation start date too early, check ', &
                ' MESH_input_streamflow.txt, The start date in ', &
                ' MESH_input_run_options.ini may be out of range'
            stop
        end if
        JDAY_IND_STRM = (JDAY_IND2 - JDAY_IND1)*24/WF_KT

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !skip the unused streamflow records in streamflow.txt .
        do j = 1, JDAY_IND_STRM
            read(iun, *, iostat = ierr)
            if (ierr < 0) then
                print *, 'ERROR: end of file reached when reading ', &
                    ' MESH_input_streamflow.txt, The start date in ', &
                    ' MESH_input_run_options.ini may be out of range'
                stop
            end if
        end do
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        print *, 'Skipping', JDAY_IND_STRM, 'Registers in streamflow file'
        !> leave unit open and read new streamflow each hour

        WF_ROUTETIMESTEP = 900
        WF_TIMECOUNT = 0

        !* JAN: The first time throught he loop, jan = 1. Jan will equal 2 after that.
        JAN = 1

        !> Streamflow output files.
        if (STREAMFLOWOUTFLAG > 0) then

            !> Daily streamflow file.
            open(WF_RTE_fls%fl(WF_RTE_flks%stfl_daily)%iun, &
                 file = './' // trim(fls%GENDIR_OUT) // '/' // &
                        trim(adjustl(WF_RTE_fls%fl(WF_RTE_flks%stfl_daily)%fn)), &
                 iostat = ierr)

            !> Hourly and cumulative daily streamflow files.
            if (STREAMFLOWOUTFLAG >= 2) then
                open(WF_RTE_fls%fl(WF_RTE_flks%stfl_ts)%iun, &
                     file = './' // trim(fls%GENDIR_OUT) // '/' // &
                            adjustl(trim(WF_RTE_fls%fl(WF_RTE_flks%stfl_ts)%fn)))
                open(WF_RTE_fls%fl(WF_RTE_flks%stfl_cumm)%iun, &
                     file = './' // trim(fls%GENDIR_OUT) // '/' // &
                            adjustl(trim(WF_RTE_fls%fl(WF_RTE_flks%stfl_cumm)%fn)))
            end if

        end if !(STREAMFLOWOUTFLAG > 0) then

        if (RESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.wf_route', status = 'old', action = 'read', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Read inital values from the file.
            read(iun) JAN
            read(iun) wf_TimeCount
            read(iun) WF_QHYD
            read(iun) WF_QHYD_AVG
            read(iun) WF_QHYD_CUM
            read(iun) WF_QSYN
            read(iun) WF_QSYN_AVG
            read(iun) WF_QSYN_CUM
            read(iun) wf_qo2
            read(iun) wf_store2
            read(iun) wf_qi2

            !> Close the file to free the unit.
            close(iun)

        end if !(RESUMEFLAG == 4) then

    end subroutine

    subroutine run_WF_ROUTE_finalize(fls, shd, ic, cm, wb, eb, sv, stfl, rrls)

        use model_files_variabletypes
        use model_files_variables
        use sa_mesh_shared_variabletypes
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(iter_counter) :: ic
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer ierr, iun

        if (SAVERESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.wf_route', status = 'replace', action = 'write', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Write the current state of these variables to the file.
            write(iun) JAN
            write(iun) wf_TimeCount
            write(iun) WF_QHYD
            write(iun) WF_QHYD_AVG
            write(iun) WF_QHYD_CUM
            write(iun) WF_QSYN
            write(iun) WF_QSYN_AVG
            write(iun) WF_QSYN_CUM
            write(iun) wf_qo2
            write(iun) wf_store2
            write(iun) wf_qi2

            !> Close the file to free the unit.
            close(iun)

        end if !(SAVERESUMEFLAG == 4) then

    end subroutine

end module
