module WF_ROUTE_config

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

        !* If STREAMFLOWFLAG is 1, the user wants to output streamflow values for each
        !* timestep. If STREAMFLOWFLAG is 0, the user will get the default daily file.
        integer :: STREAMFLOWFLAG = 0

        !> BASIN CSV-FORMAT STREAMFLOW OUTPUT FLAG
        !>
        !>  Combine values to activate a subset of the files:
        !>     3 = 1 and 2
        !>     7 = 1, 2, and 4
        !>     5 = 1 and 4
        !>
        !> If enabled, saves the observed versus simulated streamflow output
        !> file. The flag can also enable the cumulative and every time-step
        !> streamflow files written by past model configurations.
        !>     0 = Create no output.
        !>     1 = Save the observed versus simulated streamflow output file.
        !>     2 = Save the observed versus simulated, as well as the
        !>         cumulative and every time-step streamflow files.
        !>     4 = Save the streamflow channel water balance to file.
        integer(kind=4) :: STREAMFLOWOUTFLAG = 3

        !> Flag to control the reservoir release function used in
        !> WF_ROUTE.f.
        !>  2 = 2-parameter power release function.
        !>  5 = 5-parameter polynomial release function.
        integer :: RESVRELSWFB = 2

        !> Channel length 'rl' flag for WF_ROUTE.f.
!todo: Verify units if read from shed file.
        !>  0 = 'rl' is calculated using WF_AL and WF_A1 (default).
        !>  1 = Values are taken from the 'chnllength' attribute from
        !>      the drainage database/r2c shed file.
        integer :: RLFLAG = 0

        !> Bankfull/capacity 'cap' flag for WF_ROUTE.f
!todo: Verify units if read from shed file.
        !>  0 = 'cap' is calculated using WF_DA, WF_A2, WF_A3, and WF_A4
        !>      (default).
        !>  1 = Values are taken from the 'bankfull' attribute from the
        !>      drainage database/r2c shed file.
        integer :: CAPFLAG = 0

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
        integer :: stfl_bal = 6
        integer :: stfl_ts = 5

    end type

    !> WF_RTE_fls: Stores information about files used by the module.
    type(fl_ids), save :: WF_RTE_fls

    type(WF_RTE_file_keys), save :: WF_RTE_flks

    type WF_RTE_parameters

        !> Channel roughness coefficients.
        !* r2: River channel roughness coefficient.
        !* r1: Overbank channel roughness coefficient.
        real(kind=4), dimension(:), allocatable :: r2, r1

        !> Fitting coefficients.
        !* aa1: Channel length coefficient.
        !* aa2: Bankfull area coefficient.
        !* aa3: Bankfull area coefficient.
        !* aa4: Bankfull area coefficient.
        real(kind=4), dimension(:), allocatable :: aa1, aa2, aa3, aa4

    end type

    type(WF_RTE_parameters), save :: wfp

    !> *****************************************************************
    !> Local variables.
    !> *****************************************************************

    !>
    !> Description: Variables used by WF_ROUTE. These variables are used
    !> by WF_ROUTE and are only accessible to code that use this module.
    !>

    !integer, parameter :: M_S = 290, M_R = 7, M_C = 5
!todo it should be read in from the shd file

    !> STREAMFLOW VARIABLES
    !* WF_NL: NUMBER OF DATA POINTS
    !* WF_MHRD: NUMBER OF HOURS OF DATA PER MONTH
    !* WF_KT: HOURLY INCREMENT FOR STREAMFLOW INPUT (24 = DAILY)
    !* WF_QHYD: STREAMFLOW VALUE (_AVG = DAILY AVERAGE)
    !* WF_QSYN: SIMULATED STREAFLOW VALUE (_AVG = DAILY AVERAGE)
    !* WF_A1: Channel fitting parameter for average channel length (default: 1.0).
    !* WF_A2: Channel fitting parameter for average bankfull capacity (default: 11.0).
    !* WF_A3: Channel fitting parameter for average bankfull capacity (default: 0.43).
    !* WF_A4: Channel fitting parameter for average bankfull capacity (default: 1.0).
    !* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
    !* WF_START_DAY OBSERVED STREAMFLOW START DAY
    !* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
    integer WF_NAA, WF_NL, WF_MHRD, WF_KT
!-    integer, dimension(:), allocatable :: WF_IY, WF_JX, WF_S
    real, dimension(:), allocatable :: WF_QHYD, WF_QHYD_AVG, WF_QHYD_CUM
    real, dimension(:), allocatable :: WF_QSYN, WF_QSYN_AVG, WF_QSYN_CUM
!-    character(8), dimension(:), allocatable :: WF_GAGE
!-    real, dimension(:), allocatable :: WF_A1, WF_A2, WF_A3, WF_A4

    !> RESERVOIR VARIABLES
!-    integer, dimension(:), allocatable :: WF_IRES, WF_JRES, WF_R
    integer, dimension(:), allocatable :: WF_RES
    real, dimension(:), allocatable :: WF_B1, WF_B2, WF_QREL, WF_RESSTORE
    real, dimension(:), allocatable :: WF_B3, WF_B4, WF_B5
!-    character(8), dimension(:), allocatable :: WF_RESNAME

    !> FOR BASEFLOW INITIALIZATION
    integer JAN

    !* WF_R1: MANNING'S N FOR RIVER CHANNEL
    !* WF_R2: OPTIMIZED RIVER ROUGHNESS FACTOR
    !* WF_QO2: SIMULATED STREAMFLOW VALUE
!-    real WF_R1(M_C), WF_R2(M_C)
    real, dimension(:), allocatable :: WF_NHYD, WF_QBASE, WF_QI2, &
        WF_QO1, WF_QR, WF_STORE1
!-    real, dimension(:), allocatable :: WF_QI1, WF_QO2, WF_STORE2

    !> RESERVOIR MEASUREMENTS:
    !* WR_NREL: NUMBER OF DATA POINTS
    !* WF_KTR: HOURLY INCREMENT FOR RESERVOIR INPUR (24 = DAILY)
    !* WF_QREL: RESERVOIR VALUE
    integer WF_NREL, WF_KTR, WF_NORESV_CTRL
    integer WF_ROUTETIMESTEP, WF_TIMECOUNT
!-    integer DRIVERTIMESTEP

    !* WF_NODATA_VALUE: No data value for when the streamflow record does not exist.
    real :: WF_NODATA_VALUE = -999.0

    real, dimension(:), allocatable :: WF_QO2_ACC_MM, WF_STORE2_ACC_MM

    contains

    !> *****************************************************************
    !> Subroutines.
    !> *****************************************************************

    !>
    !> Description: Allocate the object containing file information.
    !>
    subroutine WF_ROUTE_init_fls()

        !> Allocate file object.
        allocate(WF_RTE_fls%fl(6))

    end subroutine

    !>
    !> Description: Check for the existence of input files, open them,
    !>              print diagnostic information, skip records, and open
    !>              the output files, in preparation for running the
    !>              WF_ROUTE process.
    !>
    subroutine WF_ROUTE_init(shd, fls, stfl, rrls)

        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use model_output_variabletypes

        !> For: LOCATIONFLAG, STREAMFLOWOUTFLAG
        use FLAGS

        type(ShedGridParams), intent(in) :: shd
        type(fl_ids) :: fls
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        !* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
        !* WF_START_DAY OBSERVED STREAMFLOW START DAY
        !* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
        integer WF_START_YEAR, WF_START_DAY, WF_START_HOUR, JDAY_IND_STRM, JDAY_IND1, JDAY_IND2

        !> Temporary variables for frequently accessed terms.
        !* NA: Number of grid cells.
        !* NS: Number of streamflow gauges.
        !* NR: Number of reservoir outlets.
        integer NS, NR, NA

        !> Temporary variables.
        !* ry/iy: Y-component of the location (real/integer).
        !* rx/ix: X-component of the location (real/integer)
        !* i, j: Counters.
        !* ierr: Error return from external calls.
        !* iun: Unit number.
        real ry, rx
        integer iy, ix, l, i, j, ierr, iun

        character*10 fn

        !> Return if the process is inactive.
        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

        NA = shd%NA
        WF_NAA = NA - shd%NAA

        allocate(WF_NHYD(NA), WF_QR(NA), &
                 WF_QBASE(NA), WF_QI2(NA), WF_QO1(NA), &
                 WF_STORE1(NA), &
                 WF_QO2_ACC_MM(NA), WF_STORE2_ACC_MM(NA))

        WF_NHYD = 0.0
        WF_QBASE = 0.0
        WF_QI2 = 0.0
        WF_QO1 = 0.0
!-        WF_QO2 = 0.0
        WF_QR = 0.0
        WF_STORE1 = 0.0
!-        WF_STORE2 = 0.0
!-        WF_QI1 = 0.0
        WF_QO2_ACC_MM = 0.0
        WF_STORE2_ACC_MM = 0.0

        !> *************************************************************
        !>  Open and read in values from MESH_input_reservoir.txt file
        !> *************************************************************

        iun = WF_RTE_fls%fl(WF_RTE_flks%resv_in)%iun
        open(iun, file = WF_RTE_fls%fl(WF_RTE_flks%resv_in)%fn, status = 'old', action = 'read')
        read(iun, '(3i5)') fms%rsvr%n, WF_NREL, WF_KTR
        WF_NORESV_CTRL = 0
        NR = fms%rsvr%n

        if (NR > 0) then

            !> Allocate and initialize reservoir variables.
            allocate(WF_RES(NR), &
                     WF_B1(NR), WF_B2(NR), WF_B3(NR), WF_B4(NR), WF_B5(NR), &
                     WF_QREL(NR), WF_RESSTORE(NR))
            WF_QREL = 0.0
            WF_RESSTORE = 0.0
            WF_B1 = 0.0
            WF_B2 = 0.0
            WF_B3 = 0.0
            WF_B4 = 0.0
            WF_B5 = 0.0

            !> Allocate configuration variables for the driver.
            allocate(fms%rsvr%name(NR), &
                     fms%rsvr%y(NR), fms%rsvr%x(NR), &
                     fms%rsvr%iy(NR), fms%rsvr%jx(NR), fms%rsvr%rnk(NR), &
                     fms%rsvr%cfn(NR))

            !> Allocate state variables for the driver.
            allocate(stas%rsvr%qi(NR), stas%rsvr%qo(NR), stas%rsvr%s(NR), stas%rsvr%ab(NR))
            stas%rsvr%qi = 0.0
            stas%rsvr%qo = 0.0
            stas%rsvr%s = 0.0
            stas%rsvr%ab = 0.0

            !> Allocate output variable for the driver.
            rrls%nr = NR
            allocate(rrls%rls(NR), rrls%store(NR), rrls%abst(NR))
            rrls%rls = 0.0
            rrls%store = 0.0
            rrls%abst = 0.0

            do i = 1, NR
                ! KCK Added to allow higher precision gauge sites
                if (LOCATIONFLAG == 1) then
                    if (WF_RTE_flgs%RESVRELSWFB == 5) then
                        read(iun, '(2f7.1, 5g10.3, a7, i2)') ry, rx, &
                            WF_B1(i), WF_B2(i), WF_B3(i), WF_B4(i), WF_B5(i), &
                            fms%rsvr%name(i), WF_RES(i)
                    else
                        read(iun, '(2f7.1, 2g10.3, 25x, a12, i2)') ry, rx, WF_B1(i), WF_B2(i), fms%rsvr%name(i), WF_RES(i)
                    end if
                    fms%rsvr%y(i) = ry
                    fms%rsvr%iy(i) = nint((ry - shd%yOrigin*60.0)/shd%GRDN)
                    fms%rsvr%x(i) = rx
                    fms%rsvr%jx(i) = nint((rx - shd%xOrigin*60.0)/shd%GRDE)
                else
                    if (WF_RTE_flgs%RESVRELSWFB == 5) then
                        read(iun, '(2i5, 5g10.3, a7, i2)') iy, ix, &
                            WF_B1(i), WF_B2(i), WF_B3(i), WF_B4(i), WF_B5(i), &
                            fms%rsvr%name(i), WF_RES(i)
                    else
                        read(iun, '(2i5, 2g10.3, 25x, a12, i2)') &
                            iy, ix, WF_B1(i), WF_B2(i), fms%rsvr%name(i), WF_RES(i)
                    end if
                    fms%rsvr%y(i) = real(iy)
                    fms%rsvr%iy(i) = int((real(iy) - real(shd%iyMin))/shd%GRDN + 1.0)
                    fms%rsvr%x(i) = real(ix)
                    fms%rsvr%jx(i) = int((real(ix) - real(shd%jxMin))/shd%GRDE + 1.0)
                end if
                if (WF_B3(i) > 0.0) then
                    fms%rsvr%cfn(i) = 3
                else if (WF_B1(i) > 0.0) then
                    fms%rsvr%cfn(i) = 2
                else
                    fms%rsvr%cfn(i) = 1
                end if
                !> check if point is in watershed and in river reaches
                fms%rsvr%rnk(i) = 0
                do j = 1, NA
                    if (fms%rsvr%iy(i) == shd%yyy(j) .and. fms%rsvr%jx(i) == shd%xxx(j)) then
                        fms%rsvr%rnk(i) = j
                    end if
                end do
                if (fms%rsvr%rnk(i) == 0) then
                    print *, 'Reservoir Station: ', i, ' is not in the basin'
                    print *, 'Up/Down Coordinate: ', fms%rsvr%iy(i), shd%iyMin
                    print *, 'Left/Right Coordinate: ', fms%rsvr%jx(i), shd%jxMin
                    stop
                end if
                if (shd%IREACH(fms%rsvr%rnk(i)) /= i) then
                    print *, 'Reservoir Station: ', i, ' is not in the correct reach'
                    print *, 'Up/Down Coordinate: ', fms%rsvr%iy(i)
                    print *, 'Left/Right Coordinate: ', fms%rsvr%jx(i)
                    print *, 'IREACH value at station: ', shd%IREACH(fms%rsvr%rnk(i))
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
        read(iun, *) fms%stmg%n, WF_NL, WF_MHRD, WF_KT, WF_START_YEAR, WF_START_DAY, WF_START_HOUR
        NS = fms%stmg%n

        allocate(WF_QHYD(NS), WF_QHYD_AVG(NS), WF_QHYD_CUM(NS), &
                 WF_QSYN(NS), WF_QSYN_AVG(NS), WF_QSYN_CUM(NS))

        !> Allocate configuration variables for the driver.
            allocate(fms%stmg%name(NS), &
                     fms%stmg%y(NS), fms%stmg%x(NS), &
                     fms%stmg%iy(NS), fms%stmg%jx(NS), fms%stmg%rnk(NS))

        !> Allocate output variable for the driver.
        stfl%ns = NS
        allocate(stfl%qhyd(NS), stfl%qsyn(NS))
        stfl%qhyd = 0.0
        stfl%qsyn = 0.0

        do i = 1, NS
            if (LOCATIONFLAG == 1) then
                read(iun, *) ry, rx, fms%stmg%name(i)
                fms%stmg%y(i) = ry
                fms%stmg%iy(i) = nint((ry - shd%yOrigin*60.0)/shd%GRDN)
                fms%stmg%x(i) = rx
                fms%stmg%jx(i) = nint((rx - shd%xOrigin*60.0)/shd%GRDE)
            else
                read(iun, *) iy, ix, fms%stmg%name(i)
                fms%stmg%y(i) = real(iy)
                fms%stmg%iy(i) = int((real(iy) - real(shd%iyMin))/shd%GRDN + 1.0)
                fms%stmg%x(i) = real(ix)
                fms%stmg%jx(i) = int((real(ix) - real(shd%jxMin))/shd%GRDE + 1.0)
            end if
        end do
        do i = 1, NS
            fms%stmg%rnk(i) = 0
            do j = 1, NA
                if (fms%stmg%jx(i) == shd%xxx(j) .and. fms%stmg%iy(i) == shd%yyy(j)) then
                    fms%stmg%rnk(i) = j
                end if
            end do
            if (fms%stmg%rnk(i) == 0) then
                print *, 'STREAMFLOW GAUGE: ', i, ' IS NOT IN THE BASIN'
                print *, 'UP/DOWN', fms%stmg%iy(i), shd%iyMin, shd%yyy(j), shd%yCount
                print *, 'LEFT/RIGHT', fms%stmg%jx(i), shd%jxMin, shd%xxx(j), shd%xCount
                stop
            end if
        end do

        if (ro%VERBOSEMODE > 0) then
            print *, 'NUMBER OF STREAMFLOW GUAGES: ', NS
            do i = 1, NS
                print *, 'STREAMFLOW STATION: ', i, 'I: ', fms%stmg%iy(i), 'J: ', fms%stmg%jx(i)
            end do
            print *, 'NUMBER OF RESERVOIR STATIONS: ', NR
            if (NR > 0) then
                do i = 1, NR
                    print *, 'RESERVOIR STATION: ', i, 'I: ', fms%rsvr%iy(i), 'J: ', fms%rsvr%jx(i)
                end do
            end if
        end if !(ro%VERBOSEMODE > 0) then

        !> ric     initialise smoothed variables
        WF_QSYN = 0.0
        WF_QSYN_AVG = 0.0
        WF_QHYD_AVG = 0.0
        WF_QSYN_CUM = 0.0
        WF_QHYD_CUM = 0.0

        !>MAM - The first stream flow record is used for flow initialization
        read(iun, *, iostat = ierr) (WF_QHYD(i), i = 1, NS)
        backspace(iun)

        ! fixed streamflow start time bug. add in function to enable the
        ! correct start time. Feb2009 aliu.
        call Julian_Day_ID(WF_START_YEAR, WF_START_DAY, JDAY_IND1)
        call Julian_Day_ID(ic%start%year, ic%start%jday, JDAY_IND2)
!        print *, WF_START_YEAR, WF_START_DAY, JDAY_IND1
        if (ic%start%year == 0) then
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
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        print *, 'Skipping', JDAY_IND_STRM, 'Registers in streamflow file'
        do j = 1, JDAY_IND_STRM
            read(iun, *, iostat = ierr)
            if (ierr /= 0) then
                print *, 'WARNING: end of file reached when reading ', &
                    ' MESH_input_streamflow.txt, The start date in ', &
                    ' MESH_input_run_options.ini may be out of range'
!-                stop
                exit
            end if
        end do
        !> leave unit open and read new streamflow each hour

        WF_ROUTETIMESTEP = 900
        WF_TIMECOUNT = 0

        !* JAN: The first time throught he loop, jan = 1. Jan will equal 2 after that.
        JAN = 1

        !> Daily streamflow output file.
        if (btest(WF_RTE_flgs%STREAMFLOWOUTFLAG, 0)) then
            open(WF_RTE_fls%fl(WF_RTE_flks%stfl_daily)%iun, &
                 file = './' // trim(fls%GENDIR_OUT) // '/' // &
                        trim(adjustl(WF_RTE_fls%fl(WF_RTE_flks%stfl_daily)%fn)), &
                 iostat = ierr)
        end if

        !> Per time-step and cumulative daily streamflow files.
        if (btest(WF_RTE_flgs%STREAMFLOWOUTFLAG, 1)) then
            if (WF_RTE_flgs%STREAMFLOWFLAG == 1) then
                open(WF_RTE_fls%fl(WF_RTE_flks%stfl_ts)%iun, &
                     file = './' // trim(fls%GENDIR_OUT) // '/' // &
                            adjustl(trim(WF_RTE_fls%fl(WF_RTE_flks%stfl_ts)%fn)))
            end if
            open(WF_RTE_fls%fl(WF_RTE_flks%stfl_cumm)%iun, &
                 file = './' // trim(fls%GENDIR_OUT) // '/' // &
                        adjustl(trim(WF_RTE_fls%fl(WF_RTE_flks%stfl_cumm)%fn)))
        end if

        !> Streamflow channel water balance output file.
        if (btest(WF_RTE_flgs%STREAMFLOWOUTFLAG, 2)) then
            open(WF_RTE_fls%fl(WF_RTE_flks%stfl_bal)%iun, &
                 file = './' // trim(fls%GENDIR_OUT) // '/' // &
                        trim(adjustl(WF_RTE_fls%fl(WF_RTE_flks%stfl_bal)%fn)), &
                 iostat = ierr)
        end if

        !> Read the state of these variables.
        if (RESUMEFLAG == 4 .or. RESUMEFLAG == 5) then

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
            read(iun) stas%chnl%qo
            read(iun) stas%chnl%s
            read(iun) wf_qi2
            if (RESUMEFLAG == 4) then
                read(iun) WF_QO2_ACC_MM
                read(iun) WF_STORE2_ACC_MM
            end if

            !> Close the file to free the unit.
            close(iun)

        end if !(RESUMEFLAG == 4 .or. RESUMEFLAG == 5) then

        do l = 1, NR
            if(l.lt.10) then
                write (fn, '(I1)') l
            else
                write (fn, '(I2)') l
            endif
            open(UNIT=708+l, &
                 FILE='./' // trim(fls%GENDIR_OUT) // '/' // 'res_' // adjustl(trim(fn)) // '.csv', &
                 status='unknown', action = 'write')
            write(708+l,"(2(a6','),7(a12,','))") &
                'l', 'wf_r', &
                'wf_qi1', 'wf_store1', 'wf_qi2', 'wf_store2', 'wf_qo2'
        end do

    end subroutine

    subroutine WF_ROUTE_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        use mpi_shared_variables
        use model_files_variables
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer ierr, iun

        !> Return in the process is inactive.
        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

        !> Return if not the head node.
        if (ipid /= 0) return

        !> Save the state of these variables.
        if (SAVERESUMEFLAG == 4 .or. SAVERESUMEFLAG == 5) then

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
            write(iun) stas%chnl%qo
            write(iun) stas%chnl%s
            write(iun) wf_qi2
            if (SAVERESUMEFLAG == 4) then
                write(iun) WF_QO2_ACC_MM
                write(iun) WF_STORE2_ACC_MM
            end if

            !> Close the file to free the unit.
            close(iun)

        end if !(SAVERESUMEFLAG == 4 .or. SAVERESUMEFLAG == 5) then

    end subroutine

end module
