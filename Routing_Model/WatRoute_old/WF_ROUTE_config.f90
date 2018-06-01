module WF_ROUTE_config

    implicit none

    !> *****************************************************************
    !> Object variables.
    !> *****************************************************************

    !>
    !> Description: WF_RTE_flags contains flags to enable and
    !>              disable the module and control its output.
    !>

    type WF_RTE_flags

        !> Flag used to enable the module.
        logical :: PROCESS_ACTIVE = .true.

        !> Flag to control the reservoir release function used in
        !> WF_ROUTE.f.
        !>  2 = 2-parameter power release function.
        !>  5 = 5-parameter polynomial release function.
!?        integer :: RESVRELSWFB = 2

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

        integer :: RTE_TS = 1800

    end type

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

    !> WF_RTE_flgs: Configuration flags for the module.
    type(WF_RTE_flags), save :: WF_RTE_flgs

    !> *****************************************************************
    !> Local variables.
    !> *****************************************************************

    !>
    !> Description: Variables used by WF_ROUTE. These variables are used
    !> by WF_ROUTE and are only accessible to code that use this module.
    !>

    !> STREAMFLOW VARIABLES
    !* WF_NL: NUMBER OF DATA POINTS
    !* WF_MHRD: NUMBER OF HOURS OF DATA PER MONTH
    !* WF_KT: HOURLY INCREMENT FOR STREAMFLOW INPUT (24 = DAILY)
    !* WF_A1: Channel fitting parameter for average channel length (default: 1.0).
    !* WF_A2: Channel fitting parameter for average bankfull capacity (default: 11.0).
    !* WF_A3: Channel fitting parameter for average bankfull capacity (default: 0.43).
    !* WF_A4: Channel fitting parameter for average bankfull capacity (default: 1.0).
    integer WF_NAA, WF_NL, WF_MHRD, WF_KT

    !> RESERVOIR VARIABLES
    integer, dimension(:), allocatable :: WF_RES
    real, dimension(:), allocatable :: WF_B1, WF_B2, WF_RESSTORE
    real, dimension(:), allocatable :: WF_B3, WF_B4, WF_B5

    !> FOR BASEFLOW INITIALIZATION
    integer JAN

    !* WF_R1: MANNING'S N FOR RIVER CHANNEL
    !* WF_R2: OPTIMIZED RIVER ROUGHNESS FACTOR
    !* WF_QO2: SIMULATED STREAMFLOW VALUE
    real, dimension(:), allocatable :: WF_NHYD, WF_QBASE, WF_QI1, &
        WF_QO1, WF_QR, WF_STORE1

    !> RESERVOIR MEASUREMENTS:
    !* WR_NREL: NUMBER OF DATA POINTS
    !* WF_KTR: HOURLY INCREMENT FOR RESERVOIR INPUR (24 = DAILY)
    integer WF_NREL, WF_KTR, WF_NORESV_CTRL
    integer WF_ROUTETIMESTEP, WF_TIMECOUNT

    contains

    !> *****************************************************************
    !> Subroutines.
    !> *****************************************************************

    !>
    !> Description: Check for the existence of input files, open them,
    !>              print diagnostic information, skip records, and open
    !>              the output files, in preparation for running the
    !>              WF_ROUTE process.
    !>
    subroutine WF_ROUTE_init(fls, shd)

        use model_files_variables
        use sa_mesh_common
        use model_dates
        use FLAGS

        type(fl_ids) :: fls
        type(ShedGridParams), intent(in) :: shd

        !> Temporary variables for frequently accessed terms.
        !* NA: Number of grid cells.
        !* NS: Number of streamflow gauges.
        !* NR: Number of reservoir outlets.
        integer NS, NR, NA

        !> Local variables.
        !* iun: Unit number.
        !* ierr: Error return from external calls.
        integer iun, ierr, i

        !> Return if the process is inactive.
        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

        NA = shd%NA
        WF_NAA = NA - shd%NAA

        allocate(WF_NHYD(NA), WF_QR(NA), &
                 WF_QBASE(NA), WF_QI1(NA), WF_QO1(NA), &
                 WF_STORE1(NA))

        WF_NHYD = 0.0
        WF_QBASE = 0.0
        WF_QI1 = 0.0
        WF_QO1 = 0.0
        WF_QR = 0.0
        WF_STORE1 = 0.0

        !> *************************************************************
        !>  Open and read in values from MESH_input_reservoir.txt file
        !> *************************************************************

        WF_NORESV_CTRL = 0
        NR = fms%rsvr%n
        WF_KTR = fms%rsvr%rlsmeas%dts

        if (NR > 0) then

            !> Allocate and initialize reservoir variables.
            allocate(WF_RES(NR), &
                     WF_B1(NR), WF_B2(NR), WF_B3(NR), WF_B4(NR), WF_B5(NR), &
                     WF_RESSTORE(NR))
            WF_RESSTORE = 0.0
            WF_B1 = fms%rsvr%rls%b1
            WF_B2 = fms%rsvr%rls%b2
            WF_B3 = fms%rsvr%rls%b3
            WF_B4 = fms%rsvr%rls%b4
            WF_B5 = fms%rsvr%rls%b5

            do i = 1, NR
                if (WF_B3(i) > 0.0) then
                    fms%rsvr%rls%cfn(i) = 3
                else if (WF_B1(i) > 0.0) then
                    fms%rsvr%rls%cfn(i) = 2
                else
                    fms%rsvr%rls%cfn(i) = 1
                end if
                if (WF_B1(i) == 0.0) then
                    WF_NORESV_CTRL = WF_NORESV_CTRL + 1
                end if
            end do
        end if

        !> *********************************************************************
        !> Open and read in values from MESH_input_streamflow.txt file
        !> *********************************************************************

        NS = fms%stmg%n

        WF_KT = fms%stmg%qomeas%dts

        WF_ROUTETIMESTEP = 900
        WF_TIMECOUNT = 0

        !* JAN: The first time throught he loop, jan = 1. Jan will equal 2 after that.
        JAN = 1

        !> Read the state of these variables.
        if (RESUMEFLAG == 4 .or. RESUMEFLAG == 5) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.wf_route', status = 'old', action = 'read', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Read inital values from the file.
            if (RESUMEFLAG == 4) then
                read(iun) JAN
                read(iun) WF_TIMECOUNT
                read(iun)
                read(iun)
                read(iun)
                read(iun)
                read(iun)
                read(iun)
                read(iun) vs%grid%qo
                read(iun) vs%grid%stgch
                read(iun) vs%grid%qi
                read(iun)
                read(iun)
            else
                read(iun) JAN
                read(iun)
                read(iun)
                read(iun)
                read(iun)
                read(iun)
                read(iun)
                read(iun)
                read(iun) vs%grid%qo
                read(iun) vs%grid%stgch
                read(iun) vs%grid%qi
                read(iun)
                read(iun)
            end if

            !> Close the file to free the unit.
            close(iun)

        end if

    end subroutine

    subroutine WF_ROUTE_finalize(fls, shd)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use FLAGS

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd

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
            write(iun) WF_TIMECOUNT
            write(iun)
            write(iun)
            write(iun)
            write(iun)
            write(iun)
            write(iun)
            write(iun) vs%grid%qo
            write(iun) vs%grid%stgch
            write(iun) vs%grid%qi
            write(iun)
            write(iun)

            !> Close the file to free the unit.
            close(iun)

        end if

    end subroutine

end module
