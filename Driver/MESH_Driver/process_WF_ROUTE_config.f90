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

!    integer M_S, M_R
!    integer, parameter :: M_C = 5
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
!    integer WF_NO, WF_NL, WF_MHRD, WF_KT
!    integer, dimension(:), allocatable :: WF_IY, WF_JX, WF_S
!    real, dimension(:), allocatable :: WF_QHYD, WF_QHYD_AVG, WF_QHYD_CUM
!    real, dimension(:), allocatable :: WF_QSYN, WF_QSYN_AVG, WF_QSYN_CUM
!    character(8), dimension(:), allocatable :: WF_GAGE

    !> RESERVOIR VARIABLES
!    integer, dimension(:), allocatable :: WF_IRES, WF_JRES, WF_RES, WF_R
!    real, dimension(:), allocatable :: WF_B1, WF_B2, WF_QREL, WF_RESSTORE
!    character(8), dimension(:), allocatable :: WF_RESNAME

    !> FOR BASEFLOW INITIALIZATION
!    integer JAN

    !* WF_R1: MANNING'S N FOR RIVER CHANNEL
    !* WF_R2: OPTIMIZED RIVER ROUGHNESS FACTOR
    !* WF_QO2: SIMULATED STREAMFLOW VALUE
!    real WF_R1(M_C), WF_R2(M_C)
!    real, dimension(:), allocatable :: WF_NHYD, WF_QBASE, WF_QI2, &
!        WF_QO1, WF_QO2, WF_QR, WF_STORE1, WF_STORE2, WF_QI1

    !> RESERVOIR MEASUREMENTS:
    !* WF_RESNAME: RESERVOIR IDENTIFIER (8 CHARACTER STRING)
    !* WF_NORESV: NUMBER OF RESERVOIRS
    !* WR_NREL: NUMBER OF DATA POINTS
    !* WF_KTR: HOURLY INCREMENT FOR RESERVOIR INPUR (24 = DAILY)
    !* WF_IRES: Y-DIRECTION GAUGE CO-ORDINATE
    !* WF_JRES: X-DIRECTION GAUGE CO-ORDINATE
    !* WF_R: RESERVOIR'S PARENT GRID SQUARE
    !* WF_QREL: RESERVOIR VALUE
!    integer WF_NORESV, WF_NREL, WF_KTR, WF_NORESV_CTRL
!    integer WF_ROUTETIMESTEP, WF_TIMECOUNT, DRIVERTIMESTEP

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
    subroutine config_WF_ROUTE(shd, ic)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates

        type(ShedGridParams), intent(in) :: shd
        type(iter_counter), intent(in) :: ic

        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

    end subroutine

end module
