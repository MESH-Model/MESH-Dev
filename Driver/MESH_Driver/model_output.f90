module model_output

    !>******************************************************************************
    !>  Athor: Gonzalo Sapriza Azuri
    !>******************************************************************************

    use sa_mesh_shared_variables
    use model_dates
    use strings
    use model_files_variables
    use flags

    implicit none

    !>
    !> *****************************************************************************
    !> Although it may seen redundant, data types for groups are created
    !> to include the time-series dim, if applicable, because of the way
    !> arrays are stored in Fortran. Storing group(t)%vars(i) can have significant
    !> overhead when compared to group_vars(t, i).
    !> *****************************************************************************
    !>

    !>
    !> *****************************************************************************
    !> Meteorological output data
    !> *****************************************************************************
    !>

    !> Data type for storing meteorlogical data in time and space.
    !* (1: time, 2: space)
    type met_data_series

        real, dimension(:, :), allocatable :: &
            fsdown, fsvh, fsih, fdl, ul, &
            ta, qa, pres, pre

        contains

        !> Procedure to initialize the variables and allocate the arrays.
!        procedure :: init => init_met_data_series

    end type !met_data_series

    !> Data type for storing meteorological data in time or space.
    !* (1: time or space)
    type met_data

        real, dimension(:), allocatable :: &
            fsdown, fsvh, fsih, fdl, ul, &
            ta, qa, pres, pre

        contains

        !> Procedure to initialize the variables and allocate the arrays.
!        procedure :: init => init_met_data

    end type !met_data

    !>
    !> *****************************************************************************
    !> Water balance output data
    !> *****************************************************************************
    !* pre: Precipitation [kg m-2]
    !* evap: Evaporation (water lost by evapotranspiration and sublimation, both rain and snow components) [kg m-2]
    !* rof: Runoff (combination of overland-, subsurface-, and base-flows) [kg m-2]
    !* rofo: Overland flow component of runoff [kg m-2]
    !* rofs: Subsurface flow component of runoff [kg m-2]
    !* rofb: Baseflow component of runoff [kg m-2]
    !* rcan: Rainfall intercepted by the canopy [kg m-2]
    !* sncan: Snowfall intercepted by the canopy [kg m-2]
    !* pndw: Water ponded at the surface of the soil [kg m-2]
    !* sno: Snowpack at the surface of the soil [kg m-2]
    !* wsno: Water stored in the snowpack [kg m-2]
    !* stg: Water stored in the system [kg m-2]
    !* dstg: Difference of water stored in the system compared to the previous time-step of the element [kg m-2]
    !* grid_area: Fractional area of the grid-square [m2 m-2]
    !* lqws: Water stored in the soil matrix [kg m-2]
    !* frws: Frozen water (ice) stored in the soil matrix [kg m-2]
    !* basin_area: Total fractional area of the basin [m2 m-2]
    !> *****************************************************************************
    !>

    !> Data type to store components of the water balance in time and space.
    !* (1: time, 2: space) or (1: time, 2: space, 3: soil layer)
    type water_balance_series

        real, dimension(:, :), allocatable :: &
            pre, evap, pevp, evpb, arrd, rof, &
            rofo, rofs, rofb, &
            rcan, sncan, pndw, sno, wsno, &
            stg, dstg, &
            grid_area
        real, dimension(:, :, :), allocatable :: &
            lqws, frws
        real :: basin_area

        contains

!        procedure :: init => init_water_balance_series

    end type !water_balance_series

    !> Data type to store components of the water balance in time or space.
    !* (1: time or space) or (1: time or space, 2: soil layer)
    type water_balance

        real, dimension(:), allocatable :: &
            pre, evap, pevp, evpb, arrd, rof, &
            rofo, rofs, rofb, &
            rcan, sncan, pndw, sno, wsno, &
            stg, dstg, &
            grid_area
        real, dimension(:, :), allocatable :: &
            lqws, frws
        real :: basin_area

        contains

!        procedure :: init => init_water_balance
!        procedure :: deallocate => deallocate_water_balance

    end type !water_balance

    !> Data type to store components of the energy balance in time and space.
    type energy_balance_series

        real, dimension(:, :), allocatable :: &
            hfs, qevp

        real,dimension(:,:,:),allocatable :: &
            gflx

        contains

!        procedure :: init => init_energy_balance_series

    end type !energy_balance_series

    !> Data type to store components of the energy balance in time or space.
    type energy_balance

        real, dimension(:), allocatable :: &
            hfs, qevp

        real,dimension(:,:),allocatable :: &
            gflx

        contains

!        procedure :: init => init_energy_balance

    end type !energy_balance

    !> Data type to store soil parameters.
    !* tbar: Temperature of the soil layer (1: grid, 2: soil layer).
    !* thic: Fractional (frozen water) ice-content stored in the soil layer (1: grid, 2: soil layer).
    !* thlq: Fractional water-content stored in the soil layer (1: grid, 2: soil layer).
    type soil_statevars_series

        real, dimension(:, :,:), allocatable :: &
            tbar, thic, thlq

        contains

!        procedure :: init => init_soil_statevars_series

    end type

    type soil_statevars

        real, dimension(:, :), allocatable :: &
            tbar, thic, thlq

        contains

!        procedure :: init => init_soil_statevars

    end type

    type wr_output_series

        real, dimension(:, :), allocatable :: &
            rof, rchg

        contains

!        procedure :: init => init_wr_output_series

    end type !wr_output_series

    !> Data type to store the output format and data handling for an output variable.
    !* name: Name of the variable.
    !* nargs: Number of arguments in the argument array.
    !* args: Argument array containing flags for handling the output of the variable (1: Arguments)
    !* out_*: Output is written if .TRUE.; *: time interval (e.g., 'Y', 'M', etc.).
    !* out_fmt: Format of the output (e.g., 'R2C', 'SEQ', etc.).
    !* out_acc: Method of accumulation (e.g., if accumulated, averaged, etc., over the time period).
    type data_var_out

        character*20 :: name
        integer :: nargs
        character*20, dimension(:), allocatable :: args
        logical :: out_y, out_m, out_s, out_d, out_h
        character*20 :: out_fmt, out_seq, out_acc
        logical :: opt_printdate = .false.

        ! Grid cells where data is request
        integer, dimension(:), allocatable :: i_grds

        contains

!        procedure :: allocate_args => data_var_out_allocate_args

    end type

    !> Total water and energy balances.
    !* wb: Water balance (1: grid).
    !* eb: Energy balance (1: grid).
    type out_bal_intg

        !real, dimension(:)  ,allocatable :: TOTAL_PRE   , TOTAL_EVAP , TOTAL_ROF
        !real, dimension(:)  ,allocatable :: TOTAL_ZPND  , TOTAL_RCAN , TOTAL_SCAN
        !real, dimension(:)  ,allocatable :: TOTAL_SNO   , TOTAL_STORE, DSTG
        !real, dimension(:)  ,allocatable :: TOTAL_ROFO  , TOTAL_ROFS , TOTAL_ROFB
        !real, dimension(:,:),allocatable :: TOTAL_lqws  , TOTAL_frws
        type(water_balance) :: wb

        !real, dimension(:)  ,allocatable :: TOTAL_HFSACC, TOTAL_QEVPACC
        !real :: TOTAL_AREA
        type(energy_balance) :: eb

    end type

    !> Contains the internal output response for a specific rank ID
    !> This type is mainly used to look at the model response in the
    !> Permafrost, Daily time step.
    !* na_id: Rank ID.
    !* rofo: Overland flow component of runoff (1: grid).
    !* rofs: Interflow (sub-surface) flow component of runoff (1: grid).
    !* rofb: Baseflow component of runoff (1: grid).
    !* gflx: Heat conduction (1: grid, 2: soil layer).
    !* thlq: Fractional liquid water content (1: grid, 2: soil layer).
    !* thic: Fractional frozen water (ice) content (1: grid, 2: soil layer).
!    type out_inter_resp_ts
!
!        !Rank ID
!        integer,dimension(:),allocatable :: i_grds
!        !number of grids
!        integer :: n_grds
!        !Runoff
!        real, dimension(:,:)   , allocatable :: rofo, rofs, rofb
!
!        !State variable and flux in soil layers
!        real, dimension(:, :, :), allocatable :: gflx
!        real, dimension(:, :, :), allocatable :: thlq, thic, tbar
!
!        contains
!
!        procedure :: init => init_inter_resp_ts
!
!    end type

    !>******************************************************************************
    !> This type contains the fields outputs
    !* *_y: Yearly value
    !* *_m: Monthly value
    !* *_s: Seasonal value
    !* *_d: Daily value
    !* wb*: Water balance (1: time-based index)
    !* sp*: Soil parameter (1: time-based index)
    type out_flds

        ! Component of the water balance
        !real, dimension(:,:), allocatable :: prec_y, prec_m, prec_s !Precipitation
        !real, dimension(:,:), allocatable :: evap_y, evap_m, evap_s !Evaporation
        !real, dimension(:,:), allocatable :: roff_y, roff_m, roff_s !Runoff
        !real, dimension(:,:), allocatable :: dstg_y, dstg_m, dstg_s !Delta Storage

        ! State Variables soil
        !real, dimension(:,:,:), allocatable :: tbar_y, tbar_m, tbar_s !Temperature in the soil layers
        !real, dimension(:,:,:), allocatable :: lqws_y, lqws_m, lqws_s !Liquid content in the soil layer
        !real, dimension(:,:,:), allocatable :: frws_y, frws_m, frws_s !Ice content in the soil layer
        !real, dimension(:,:), allocatable :: rcan_y, rcan_m, rcan_s ! Rainfall intercepted by the canopy
        !real, dimension(:,:), allocatable :: scan_y, scan_m, scan_s ! Snowfall intercepted by the canopy
        !real, dimension(:,:), allocatable :: pndw_y, pndw_m, pndw_s ! Water ponded at the surface of the soil
        !real, dimension(:,:), allocatable :: sno_y, sno_m, sno_s ! Snow stored at the surface of the soil (snowpack)
        !real, dimension(:,:), allocatable :: wsno_y, wsno_m, wsno_s ! Water stored in the snowpack
        type(water_balance_series) :: wbt_y, wbt_m, wbt_s, wbt_d, wbt_h
        type(water_balance) :: wd_ts

        type(energy_balance_series) :: engt_y, engt_m, engt_s, engt_d, engt_h
        type(energy_balance) :: eng_ts


        type(soil_statevars_series) :: spt_y, spt_m, spt_s, spt_d, spt_h
        type(soil_statevars) :: sp_ts
        !type(soil_parameters_series) :: spt_y, spt_m, spt_s, spt_d, spt_h
        !type(soil_parameters) :: sp_ts

        !* mdt_h: Meteological data (hourly time-step).
        type(met_data_series) :: mdt_h
        type(met_data) :: md_ts

        type(wr_output_series) :: wroutt_h



        contains

!        procedure :: init => init_out_flds

    end type

    !>******************************************************************************
    !* flIn: File that contains the input information of the variables that we want to init and the frequency.
    !* pthOut: path out.
    !* ids_var_out: Array that contains the IDs of the files and frequency (e.g., 'PREPC', 'Y', 'M', 'S', 'CUM', 'SEQ').
    !* nr_out: Number of output variables.
    TYPE info_out

        character*450 :: flIn
        character*450 :: pthOut
!        character*20, dimension(:,:), allocatable :: ids_var_out
        integer :: nr_out = 0
        type(data_var_out), dimension(:), allocatable :: var_out

    END TYPE
    !>******************************************************************************

!-    !todo: Move this to somewhere more appropriate, perhaps as model_info.
!-    type(iter_counter), public :: public_ic

    !!!!!!!!!!!!--LAM Aug-2017
    ! to write outout files from SA_RTE
    type(fl_ids), save :: SA_RTE_fls_out
    !!!!!!!!!!!!--LAM Aug-2017

    contains

    subroutine init_met_data_series(mdt, shd, nts)

        !> Derived-type variable.
        type(met_data_series) :: mdt

        !> Input variables.
        !* nts: Number of time-steps in the series.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: nts

        !> Allocate the arrays.
        allocate( &
            mdt%fsdown(nts, shd%NA), mdt%fsvh(nts, shd%NA), mdt%fsih(nts, shd%NA), mdt%fdl(nts, shd%NA), mdt%ul(nts, shd%NA), &
            mdt%ta(nts, shd%NA), mdt%qa(nts, shd%NA), mdt%pres(nts, shd%NA), mdt%pre(nts, shd%NA))

        !> Explicitly set all variables to 0.0.
        mdt%fsdown = 0.0
        mdt%fsvh = 0.0
        mdt%fsih = 0.0
        mdt%fdl = 0.0
        mdt%ul = 0.0
        mdt%ta = 0.0
        mdt%qa = 0.0
        mdt%pres = 0.0
        mdt%pre = 0.0

    end subroutine !init_met_data_series

    subroutine init_met_data(md, shd)

        !> Derived-type variable.
        type(met_data) :: md

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Allocate the arrays.
        allocate( &
            md%fsdown(shd%NA), md%fsvh(shd%NA), md%fsih(shd%NA), md%fdl(shd%NA), md%ul(shd%NA), &
            md%ta(shd%NA), md%qa(shd%NA), md%pres(shd%NA), md%pre(shd%NA))

        !> Explicitly set all variables to 0.0.
        md%fsdown = 0.0
        md%fsvh = 0.0
        md%fsih = 0.0
        md%fdl = 0.0
        md%ul = 0.0
        md%ta = 0.0
        md%qa = 0.0
        md%pres = 0.0
        md%pre = 0.0

    end subroutine !init_met_data

    subroutine data_var_out_allocate_args(vo, args)

        !> Type variable.
        type(data_var_out) :: vo

        !> Input variables.
        character*20, dimension(:), intent(in) :: args

        !> Local variables.
        integer :: i, j, ngrds, ios
        character *1:: opts

        !> De-allocate the args if they have already been allocated.
        if (allocated(vo%args)) &
            deallocate(vo%args)

        !> Set nargs to the size of the array.
        vo%nargs = size(args)

        !> Allocate args and copy the input args to the array.
        allocate(vo%args(vo%nargs))
        vo%args = args

        !> Reset variables.
        vo%out_y   = .false.
        vo%out_m   = .false.
        vo%out_s   = .false.
        vo%out_d   = .false.
        vo%out_h   = .false.
        vo%out_fmt = "unknown"
        vo%out_seq = "gridorder"
        vo%out_acc = "unknown"

        !> Assign variables according to the args.
        do i = 1, vo%nargs
            opts = lowercase(trim(adjustl(vo%args(i))))
            !todo: A better means for comparison would be a string utility to convert all chars to lower-case, say, and then run the comparison.
            if (is_letter(opts))then
                select case (lowercase(trim(adjustl(vo%args(i)))))

                    !> Yearly output.
                    case ("y")
                        vo%out_y = .true.

                    !> Monthly output.
                    case ("m")
                        vo%out_m = .true.

                    !> Seasonal output.
                    case ("s")
                        vo%out_s = .true.

                    !> Daily output.
                    case ("d")
                        vo%out_d = .true.

                    !> Hourly:
                    case ("h")
                        vo%out_h = .true.

                    !> Output format.
                    case ("r2c", "seq", "binseq", "txt", "csv")
                        vo%out_fmt = vo%args(i)

                    !> Order of the selection being saved.
                    case ("gridorder", "shedorder")
                        vo%out_seq = lowercase(trim(adjustl(vo%args(i))))

                    !> Print date-stamp prior to the record.
                    case ("printdate")
                        vo%opt_printdate = .true.

                    !> Output format. Time series in grids
                    case("tsi")
                        vo%out_fmt = vo%args(i)
                        ngrds = vo%nargs - i
                        allocate(vo%i_grds(ngrds))
                        do j = 1, ngrds
                            call value(vo%args(i+j),vo%i_grds(j),ios)
                        enddo

                    !> Method of accumulation.
                    case ("cum", "avg", "max", "min")
                          vo%out_acc = vo%args(i)

                    case default
                        print *, trim(vo%args(i)) // " (Line ", i, ") is an unrecognized argument for output."

                end select !case (vo%args(i))

            end if
        end do !i = 1, vo%nargs

    end subroutine !data_var_out_allocate_args

!    subroutine info_out_allocate_var_out(ifo, nvo)
!
!        class(info_out) :: ifo
!        integer :: nvo
!
!        if (allocated(ifo%var_out)) deallocate(ifo%var_out)
!        allocate(ifo%var_out(nvo))
!
!    end subroutine !info_out_allocate_var_out

    subroutine init_out_flds(shd, ts, vr)

        !> Type variable.
        type(out_flds) :: vr

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts

        !> Local variables.
        integer :: i

        !> Allocate arrays using basin info.

        !> Yearly:
        call init_water_balance_series(vr%wbt_y, shd, ts%nyears)
        call init_soil_statevars_series(vr%spt_y, shd, ts%nyears)
        call init_energy_balance_series(vr%engt_y, shd, ts%nyears)

        !> Monthly:
        call init_water_balance_series(vr%wbt_m, shd, ts%nmonths)
        call init_soil_statevars_series(vr%spt_m, shd, ts%nmonths)
        call init_energy_balance_series(vr%engt_m, shd, ts%nmonths)

        !> Seasonally:
        call init_water_balance_series(vr%wbt_s, shd, ts%nseason)
        call init_soil_statevars_series(vr%spt_s, shd, ts%nseason)
        call init_energy_balance_series(vr%engt_s, shd, ts%nseason)

        !> Daily:
        call init_water_balance_series(vr%wbt_d, shd, ts%nr_days)
        call init_soil_statevars_series(vr%spt_d, shd, ts%nr_days)
        call init_energy_balance_series(vr%engt_d, shd, ts%nr_days)

        !> Hourly:
        call init_water_balance_series(vr%wbt_h, shd, max(1, 3600/ic%dts))
        call init_met_data_series(vr%mdt_h, shd, max(1, 3600/ic%dts))
        call init_energy_balance_series(vr%engt_h, shd, max(1, 3600/ic%dts))
        call init_wr_output_series(vr%wroutt_h, shd, max(1, 3600/ic%dts))

        !> Per time-step:
        call init_met_data(vr%md_ts, shd)

    end subroutine !init_out_flds

    subroutine init_water_balance_series(wbt, shd, nts)

        !> Type variable.
        type(water_balance_series) :: wbt

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: nts

        !> Allocate arrays using basin info.
        allocate( &
            wbt%pre(nts, shd%NA), wbt%evap(nts, shd%NA), wbt%pevp(nts, shd%NA), wbt%evpb(nts, shd%NA), wbt%arrd(nts, shd%NA), &
            wbt%rof(nts, shd%NA), wbt%rofo(nts, shd%NA), wbt%rofs(nts, shd%NA), wbt%rofb(nts, shd%NA), &
            wbt%rcan(nts, shd%NA), wbt%sncan(nts, shd%NA), &
            wbt%pndw(nts, shd%NA), wbt%sno(nts, shd%NA), wbt%wsno(nts, shd%NA), &
            wbt%stg(nts, shd%NA), wbt%dstg(nts, shd%NA), &
            wbt%grid_area(nts, shd%NA), &
            wbt%lqws(nts, shd%NA, shd%lc%IGND), wbt%frws(nts, shd%NA, shd%lc%IGND))

        !> Explicitly set all variables to 0.0.
        wbt%pre = 0.0
        wbt%evap = 0.0
        wbt%pevp = 0.0
        wbt%evpb = 0.0
        wbt%arrd = 0.0
        wbt%rof = 0.0
        wbt%rofo = 0.0
        wbt%rofs = 0.0
        wbt%rofb = 0.0
        wbt%rcan = 0.0
        wbt%sncan = 0.0
        wbt%pndw = 0.0
        wbt%sno = 0.0
        wbt%wsno = 0.0
        wbt%stg = 0.0
        wbt%dstg = 0.0
        wbt%grid_area = 0.0
        wbt%lqws = 0.0
        wbt%frws = 0.0
        wbt%basin_area = 0.0

    end subroutine !init_water_balance_series

    subroutine init_water_balance(wb, shd)

        !> Type variable.
        type(water_balance) :: wb

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Allocate arrays using basin info.
        allocate( &
            wb%pre(shd%NA), wb%evap(shd%NA), wb%pevp(shd%NA), wb%evpb(shd%NA), wb%arrd(shd%NA), &
            wb%rof(shd%NA), wb%rofo(shd%NA), wb%rofs(shd%NA), wb%rofb(shd%NA), &
            wb%rcan(shd%NA), wb%sncan(shd%NA), &
            wb%pndw(shd%NA), wb%sno(shd%NA), wb%wsno(shd%NA), &
            wb%stg(shd%NA), wb%dstg(shd%NA), &
            wb%grid_area(shd%NA), &
            wb%lqws(shd%NA, shd%lc%IGND), wb%frws(shd%NA, shd%lc%IGND))

        !> Explicitly set all variables to 0.0.
        wb%pre = 0.0
        wb%evap = 0.0
        wb%pevp = 0.0
        wb%evpb = 0.0
        wb%arrd = 0.0
        wb%rof = 0.0
        wb%rofo = 0.0
        wb%rofs = 0.0
        wb%rofb = 0.0
        wb%rcan = 0.0
        wb%sncan = 0.0
        wb%pndw = 0.0
        wb%sno = 0.0
        wb%wsno = 0.0
        wb%stg = 0.0
        wb%dstg = 0.0
        wb%grid_area = 0.0
        wb%lqws = 0.0
        wb%frws = 0.0
        wb%basin_area = 0.0

    end subroutine !init_water_balance

    subroutine init_energy_balance(eb, shd)

        !> Type variable.
        type(energy_balance) :: eb

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Allocate arrays using basin info.
        allocate( &
            eb%hfs(shd%NA), eb%qevp(shd%NA), eb%gflx(shd%NA, shd%lc%IGND))

        !> Explicitly set all variables to 0.0.
        eb%hfs  = 0.0
        eb%qevp = 0.0
        eb%gflx = 0.0


    end subroutine !init_energy_balance

    subroutine init_energy_balance_series(engt, shd,nts)

        !> Type variable.
        type(energy_balance_series) :: engt

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: nts

        !> Allocate arrays using basin info.
        allocate( &
            engt%hfs(nts, shd%NA)           , &
            engt%qevp(nts, shd%NA)          , &
            engt%gflx(nts, shd%NA, shd%lc%IGND) )

        !> Explicitly set all variables to 0.0.
        engt%hfs   = 0.0
        engt%qevp  = 0.0
        engt%gflx  = 0.0


    end subroutine !init_energy_balance_series

!    subroutine deallocate_water_balance(wb)

        !> Type variable.
!        class(water_balance) :: wb

        !> De-allocate arrays.
!        if (allocated(wb%pre)) deallocate(wb%pre)
!        if (allocated(wb%evap)) deallocate(wb%evap)
!        if (allocated(wb%rof)) deallocate(wb%rof)
!        if (allocated(wb%rofo)) deallocate(wb%rofo)
!        if (allocated(wb%rofs)) deallocate(wb%rofs)
!        if (allocated(wb%rofb)) deallocate(wb%rofb)
!        if (allocated(wb%rcan)) deallocate(wb%rcan)
!        if (allocated(wb%sncan)) deallocate(wb%sncan)
!        if (allocated(wb%pndw)) deallocate(wb%pndw)
!        if (allocated(wb%sno)) deallocate(wb%sno)
!        if (allocated(wb%wsno)) deallocate(wb%wsno)
!        if (allocated(wb%stg)) deallocate(wb%stg)
!        if (allocated(wb%dstg)) deallocate(wb%dstg)
!        if (allocated(wb%grid_area)) deallocate(wb%grid_area)
!        if (allocated(wb%lqws)) deallocate(wb%lqws)
!        if (allocated(wb%frws)) deallocate(wb%frws)

!    end subroutine !deallocate_water_balance

    subroutine init_soil_statevars(sv, shd)

        !> Type variable.
        type(soil_statevars) :: sv

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Allocate arrays using basin info.
        allocate( &
            sv%tbar(shd%NA, shd%lc%IGND) , &
            sv%thic(shd%NA, shd%lc%IGND) , &
            sv%thlq(shd%NA, shd%lc%IGND))

        !> Explicitly set all variables to 0.0.
        sv%tbar = 0.0
        sv%thic = 0.0
        sv%thlq = 0.0

    end subroutine !init_soil_statevars

    subroutine init_soil_statevars_series(sp, shd, nts)

        !> Type variable.
        type(soil_statevars_series) :: sp

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: nts

        !> Allocate arrays using basin info.
        allocate( &
            sp%tbar(nts, shd%NA, shd%lc%IGND), &
            sp%thic(nts, shd%NA, shd%lc%IGND), sp%thlq(nts, shd%NA, shd%lc%IGND))

        !> Explicitly set all variables to 0.0.
        sp%tbar = 0.0
        sp%thic = 0.0
        sp%thlq = 0.0

    end subroutine !init_soil_parameters

    subroutine init_wr_output_series(wroutt, shd, nts)

        !> Type variable.
        type(wr_output_series) :: wroutt

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: nts

        !> Allocate arrays using basin info.
        allocate( &
            wroutt%rof(nts, shd%NA), wroutt%rchg(nts, shd%NA))

        !> Explicitly set all variables to zero.
        wroutt%rof = 0.0
        wroutt%rchg = 0.0

    end subroutine !init_wr_output_series

    !>******************************************************************************
!    subroutine Init_Internal_resp(pmf_r, ts, ignd, naid)
!
!        !>----------------------------------------------------------------------
!        !>  Description: Init output of internal response
!        !>  Allocatation
!        !>----------------------------------------------------------------------
!
!        !Inputs
!        real, intent(in) :: naid
!        integer, intent(in) :: ignd
!        type(dates_model), intent(in) :: ts
!
!        !Output
!        type(OUT_INTER_RESP), intent(inout) :: pmf_r
!
!        !>--------------Main Subtrouine start-----------------------------------
!
!        allocate(pmf_r%rofo(ts%nr_days), &
!                 pmf_r%rofs(ts%nr_days), &
!                 pmf_r%rofb(ts%nr_days))
!
!        allocate(pmf_r%gflx(ts%nr_days, ignd), &
!                 pmf_r%thlq(ts%nr_days, ignd), &
!                 pmf_r%thic(ts%nr_days, ignd), &
!                 pmf_r%tbar(ts%nr_days, ignd))
!
!        pmf_r%na_id = naid
!
!    end subroutine Init_Internal_resp

    !>**********************************************************************
    subroutine Init_OutBal_Intg(bal, ts, ignd, area)

        !>------------------------------------------------------------------------------
        !>  Description: Init output water and energy balances
        !>
        !>------------------------------------------------------------------------------

        !Inputs
        real, intent(in) :: area
        integer, intent(in) :: ignd
        type(dates_model), intent(in) :: ts

        !Output
        type(out_bal_intg), intent(inout) :: bal

        !>--------------Main Subtrouine start-----------------------------------------------

        !> Allocate variables for basin totals.
        allocate(bal%wb%pre(ts%nr_days), bal%wb%evap(ts%nr_days), &
                 bal%wb%rof(ts%nr_days), &
                 bal%wb%rofo(ts%nr_days), bal%wb%rofs(ts%nr_days), bal%wb%rofb(ts%nr_days), &
                 bal%wb%rcan(ts%nr_days), bal%wb%sncan(ts%nr_days), &
                 bal%wb%pndw(ts%nr_days), bal%wb%sno(ts%nr_days), bal%wb%wsno(ts%nr_days), &
                 bal%wb%stg(ts%nr_days), bal%wb%dstg(ts%nr_days))

        allocate(bal%wb%lqws(ts%nr_days, ignd), bal%wb%frws(ts%nr_days, ignd))

        allocate(bal%eb%hfs(ts%nr_days), bal%eb%qevp(ts%nr_days))

        bal%wb%basin_area = area

    end subroutine Init_OutBal_Intg

    !>******************************************************************************
    subroutine Update_OutBal_Intg(bal, ts, ignd, &
                                  pre, evap, rof, &
                                  pndw, rcan, sncan, &
                                  sno, rofo, rofs, &
                                  rofb, stg, dstg, &
                                  frws, lqws, &
                                  hfs, qevp, &
                                  idate, isavg, nhours)

        !>------------------------------------------------------------------------------
        !>  Description: Update values and compute daily averages for water and
        !>  energy balance
        !>------------------------------------------------------------------------------

        !Input
        logical, intent(in) :: isavg
        integer, intent(in) :: idate, ignd
        integer, optional :: nhours

        type(dates_model), intent(in) :: ts

        real, intent(in) :: pre, evap, rof, pndw, rcan, sncan
        real, intent(in) :: sno, rofo, rofs, rofb, stg, dstg
        real, dimension(:), intent(in) :: frws, lqws
        real, intent(in) :: hfs, qevp

        !Output
        type(out_bal_intg), intent(inout) :: bal

        !Internal
        integer :: i

        !>--------------Main Subtrouine start-----------------------------------------------

        !> Rainfall
        bal%wb%pre(idate) = bal%wb%pre(idate) + pre
        if (isavg) &
            bal%wb%pre(idate) = bal%wb%pre(idate)/real(nhours)

        !> Evapotranspiration
        bal%wb%evap(idate) = bal%wb%evap(idate) + evap
        if (isavg) &
            bal%wb%evap(idate) = bal%wb%evap(idate)/real(nhours)

        !> Total runoff
        bal%wb%rof(idate) = bal%wb%rof(idate) + rof
        if (isavg) &
            bal%wb%rof(idate) = bal%wb%rof(idate)/real(nhours)

        !> Ponded water
        bal%wb%pndw(idate) = bal%wb%pndw(idate) + pndw
        if (isavg) &
            bal%wb%pndw(idate) = bal%wb%pndw(idate)/real(nhours)

        !> Rain intercepted in the canopy
        bal%wb%rcan(idate) = bal%wb%rcan(idate) + rcan
        if (isavg) &
            bal%wb%rcan(idate) = bal%wb%rcan(idate)/real(nhours)

        !> Snow intercepted in the canopy
        bal%wb%sncan(idate) = bal%wb%sncan(idate) + sncan
        if (isavg) &
            bal%wb%sncan(idate) = bal%wb%sncan(idate)/real(nhours)

        !> Snow on the surface
        bal%wb%sno(idate) = bal%wb%sno(idate) + sno
        if (isavg) &
            bal%wb%sno(idate) = bal%wb%sno(idate)/real(nhours)

        !> Overland component of total runoff
        bal%wb%rofo(idate) = bal%wb%rofo(idate) + rofo
        if (isavg) &
            bal%wb%rofo(idate) = bal%wb%rofo(idate)/real(nhours)

        !> Interflow/subsurface component of total runoff
        bal%wb%rofs(idate) = bal%wb%rofs(idate) + rofs
        if (isavg) &
            bal%wb%rofs(idate) = bal%wb%rofs(idate)/real(nhours)

        !> Baseflow component of total runoff
        bal%wb%rofb(idate) = bal%wb%rofb(idate) + rofb
        if (isavg) &
            bal%wb%rofb(idate) = bal%wb%rofb(idate)/real(nhours)

        !> Total Storage
        bal%wb%stg(idate) = bal%wb%stg(idate) + stg
        if (isavg) &
            bal%wb%stg(idate) = bal%wb%stg(idate)/real(nhours)

        !> Delta Storage
        bal%wb%dstg(idate) = bal%wb%dstg(idate) + dstg
        if (isavg) &
            bal%wb%dstg(idate) = bal%wb%dstg(idate)/real(nhours)

        !> Frozen and liquid water stored in the soil
        do i = 1, ignd
            bal%wb%frws(idate, i) = bal%wb%frws(idate, i) + frws(i)
            bal%wb%lqws(idate, i) = bal%wb%lqws(idate, i) + lqws(i)
            if (isavg) then
                bal%wb%frws(idate, i) = bal%wb%frws(idate, i)/real(nhours)
                bal%wb%lqws(idate, i) = bal%wb%lqws(idate, i)/real(nhours)
            end if
        end do

        !> Energy balance
        bal%eb%hfs(idate) = bal%eb%hfs(idate) + hfs
        bal%eb%qevp(idate) = bal%eb%qevp(idate) + qevp
        if (isavg) then
            bal%eb%hfs(idate) = bal%eb%hfs(idate)/real(nhours)
            bal%eb%qevp(idate) = bal%eb%qevp(idate)/real(nhours)
        end if

    end subroutine Update_OutBal_Intg

    !>******************************************************************************

    subroutine init_out(shd, ts, ifo, vr)

        !>------------------------------------------------------------------------------
        !>  Description: Read Output balance file
        !>
        !>------------------------------------------------------------------------------

        !Inputs
        type(ShedGridParams) :: shd

        !Inputs-Output
        type(dates_model) :: ts
        type(info_out) :: ifo
        type(out_flds) :: vr

        !Internals
        integer :: ios, i, j, k, istat, nargs

        character*50 :: vId
        !character*20 :: args(:)

        character*850 :: line

        character(len=20):: str
        character(len=1) :: delims
        integer, parameter :: StrMax=20, Nmax = 100
        character(len=StrMax), dimension(Nmax) :: argsLine

!-        call init_iter_counter(public_ic, YEAR_NOW, JDAY_NOW, HOUR_NOW, MINS_NOW)

        !>--------------Main Subtrouine start-----------------------------------------------

        open(unit   = 909                   , &
             file   = 'outputs_balance.txt' , &
             status = 'old'                 , &
             action = 'read'                , &
             iostat = ios                   )

        ifo%flIn = 'outputs_balance.txt'
        delims = ' '
        read(909, *) ifo%pthOut
        read(909, *) ifo%nr_out

!        allocate(ifo%ids_var_out(ifo%nr_out, 6))
!        call ifo%allocate_var_out(ifo%nr_out)
        allocate(ifo%var_out(ifo%nr_out), stat = istat)
        if (istat /= 0) &
            print *, "Error allocating output variable array from file."

        !> Initialize variable.
        call init_out_flds(shd, ts, vr)

        do i = 1, ifo%nr_out

            !> Read configuration information from file.
            !read(909, *) (ifo%ids_var_out(i, j), j = 1, 6)
            read(909,'(A)') line

            call parse(line,delims,argsLine,nargs)
            !ifo%varOut(i) = lowercase(argsLine(1))
            ifo%var_out(i)%name = argsLine(1)
            !read(909, *) ifo%var_out(i)%name, nargs
!            if (allocated(args)) deallocate(args)
!            allocate(args(nargs))
!            do j = 1, nargs
!                args(j) = argsLine(j)
!            enddo
            !read(909, *) ifo%var_out(i)%name, nargs, (args(j), j = 1, nargs)
            call data_var_out_allocate_args(ifo%var_out(i), argsLine(2:nargs))

            !temp: Copy to old array
!            ifo%ids_var_out(i, 1) = ifo%var_out(i)%name
!            do j = 2, nargs + 1
!                ifo%ids_var_out(i, j) = ifo%var_out(i)%args(j - 1)
!            end do

!            !> Yearly:
!            if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') &
!                allocate(vr%sp_y(ts%nyears), vr%wb_y(ts%nyears))

!            !> Monthly:
!            if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') &
!                allocate(vr%sp_m(ts%nmonths), vr%wb_m(ts%nmonths))

!            !> Seasonally:
!            if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') &
!                allocate(vr%sp_s(ts%nseason), vr%wb_s(ts%nseason))

            !> Extract variable ID.
!            vId = trim(adjustl(ifo%ids_var_out(i, 1)))
            vId = trim(adjustl(ifo%var_out(i)%name))
            select case (vId)

                case ('ZPND')
                    print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."
                    print *, "Use PNDW for ponded water at the surface [mm]."

                case ('ROFF')
                    print *, "Output of variable 'ROF' using keyword '" // trim(adjustl(vId)) // "' is not supported."
                    print *, "Use ROF for total runoff."

                case ('THIC', 'ICEContent_soil_layers')
                    print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."
                    print *, "Use LQWS for liquid water stored in the soil [mm]."

                case ('THLQ', 'LiquidContent_soil_layers')
                    print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."
                    print *, "Use FRWS for frozen water stored in the soil [mm]."

!                case default
!                    print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."

            end select
        end do ! i = 1, ifo%nr_out

        close(unit = 909)

    end subroutine Init_out

    subroutine updatefieldsout_temp(shd, ts, ifo, &
                                    md, wb, &
                                    vr)

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts
        type(info_out), intent(in) :: ifo
        type(met_data) :: md
        type(water_balance) :: wb

        !> Input-output variables.
        type(out_flds) :: vr

        !> Local variables.
        integer :: i, j
        character*5 :: freq
        logical writeout

        !> Determine if this is the last time-step of the hour.
        writeout = (mod(ic%ts_hourly, 3600/ic%dts) == 0)

        !> Update output fields.
        do i = 1, ifo%nr_out
            select case (trim(adjustl(ifo%var_out(i)%name)))

                case ("FSDOWN")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%mdt_h%fsdown(ic%ts_hourly, :) = md%fsdown
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%fsdown, freq, writeout, 882101, .true.)
                    end if

                case ("FSVH")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%mdt_h%fsvh(ic%ts_hourly, :) = md%fsvh
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%fsvh, freq, writeout, 882102, .true.)
                    end if

                case ("FSIH")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%mdt_h%fsih(ic%ts_hourly, :) = md%fsih
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%fsih, freq, writeout, 882103, .true.)
                    end if

                case ("FDL")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%mdt_h%fdl(ic%ts_hourly, :) = md%fdl
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%fdl, freq, writeout, 882104, .true.)
                    end if

                case ("UL")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%mdt_h%ul(ic%ts_hourly, :) = md%ul
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%ul, freq, writeout, 882105, .true.)
                    end if

                case ("TA")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%mdt_h%ta(ic%ts_hourly, :) = md%ta
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%ta, freq, writeout, 882106, .true.)
                    end if

                case ("QA")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%mdt_h%qa(ic%ts_hourly, :) = md%qa
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%qa, freq, writeout, 882107, .true.)
                    end if

                case ("PRES")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%mdt_h%pres(ic%ts_hourly, :) = md%pres
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%pres, freq, writeout, 882108, .true.)
                    end if

                case ("PRE")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%mdt_h%pre(ic%ts_hourly, :) = md%pre
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%pre, freq, writeout, 882109, .true.)
                    end if

                !*todo: Better way of storing variables in different formats (e.g., PRE [mm s-1] vs PREC [mm]).
                case ("PREC")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wbt_h%pre(ic%ts_hourly, :) = wb%pre
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%pre, freq, writeout, 882122, .true.)
                    end if

                case ("EVAP")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wbt_h%evap(ic%ts_hourly, :) = wb%evap
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%evap, freq, writeout, 882110, .true.)
                    end if

                case ("ROF")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wbt_h%rof(ic%ts_hourly, :) = wb%rof
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%rof, freq, writeout, 882111, .true.)
                    end if

                case ("LQWS")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        do j = 1, shd%lc%IGND
                            vr%wbt_h%lqws(ic%ts_hourly, :, j) = wb%lqws(:, j)
                            call check_write_var_out(shd, ifo, i, vr%wbt_h%lqws(:, :, j), freq, writeout, &
                                (882112 + (100000000*j)), .true., j)
                        end do
                    end if

                case ("FRWS")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        do j = 1, shd%lc%IGND
                            vr%wbt_h%frws(ic%ts_hourly, :, j) = wb%frws(:, j)
                            call check_write_var_out(shd, ifo, i, vr%wbt_h%frws(:, :, j), freq, writeout, &
                                (882113 + (100000000*j)), .true., j)
                        end do
                    end if

                case ("RCAN")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wbt_h%rcan(ic%ts_hourly, :) = wb%rcan
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%rcan, freq, writeout, 882114, .true.)
                    end if

                case ("SNCAN")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wbt_h%sncan(ic%ts_hourly, :) = wb%sncan
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%sncan, freq, writeout, 882115, .true.)
                    end if

                case ("PNDW")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wbt_h%pndw(ic%ts_hourly, :) = wb%pndw
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%pndw, freq, writeout, 882116, .true.)
                    end if

                case ("SNO")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wbt_h%sno(ic%ts_hourly, :) = wb%sno
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%sno, freq, writeout, 882117, .true.)
                    end if

                case ("WSNO")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wbt_h%wsno(ic%ts_hourly, :) = wb%wsno
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%wsno, freq, writeout, 882118, .true.)
                    end if

                case ("STG")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wbt_h%stg(ic%ts_hourly, :) = wb%stg
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%stg, freq, writeout, 882119, .true.)
                    end if

                case ("WR_RUNOFF")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wroutt_h%rof(ic%ts_hourly, :) = wb%rofo + wb%rofs
                        call check_write_var_out(shd, ifo, i, vr%wroutt_h%rof, freq, writeout, 882120, .true.)
                    end if

                case ("WR_RECHARGE")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        vr%wroutt_h%rchg(ic%ts_hourly, :) = wb%rofb
                        call check_write_var_out(shd, ifo, i, vr%wroutt_h%rchg, freq, writeout, 882121, .true.)
                    end if

            end select !case (trim(adjustl(ifo%var_out(i)%name)))
        end do !i = 1, ifo%nr_out

    end subroutine !updatefieldsout_temp

    subroutine check_write_var_out(shd, ifo, var_id, fld_in, freq, writeout, file_unit, keep_file_open, igndx)

        !> Input variables.
        type(info_out), intent(in) :: ifo
        integer, intent(in) :: var_id
        real, dimension(:, :) :: fld_in
        type(ShedGridParams), intent(in) :: shd
        character*5, intent(in) :: freq
        logical, intent(in) :: writeout
        integer, intent(in) :: file_unit
        logical :: keep_file_open
        integer, intent(in), optional :: igndx

        !> Local variables.
        integer, dimension(:, :), allocatable :: dates
        real, dimension(:, :), allocatable :: fld_out
        character*5 :: freq2
        character*1 :: st
        integer frame_no

        !> Write output if at end of time-step.
        if (writeout) then

            !> Write output.
            select case (trim(adjustl(freq)))

                case ("H")
                    allocate(fld_out(size(fld_in, 2), 1))
                    select case (trim(adjustl(ifo%var_out(var_id)%out_acc)))

                        case ("AVG")
                            fld_out(:, 1) = sum(fld_in, 1) / size(fld_in, 1)

                        case ("MAX")
                            fld_out(:, 1) = maxval(fld_in, 1)

                        case ("MIN")
                            fld_out(:, 1) = minval(fld_in, 1)

                        case default
                            fld_out(:, 1) = sum(fld_in, 1)

                    end select !case (trim(adjustl(ifo%var_out(i)%out_acc)))
                    frame_no = ic%count_hour + 1

            end select !case (trim(adjustl(freq)))

            !> Reset array.
            fld_in = 0.0

            !> fld will have been allocated if a supported frequency was selected.
            if (allocated(fld_out)) then

                !> Set dates to contain the current time-step.
                allocate(dates(1, 5))
                dates(1, 1) = ic%now%year
                dates(1, 2) = ic%now%month
                dates(1, 3) = ic%now%day
                dates(1, 4) = ic%now%jday
!todo: Flag to write 0-23 or 1-24.
                dates(1, 5) = (ic%now%hour + 1)

                !> Update freq to include soil layer (if applicable).
                if (present(igndx)) then
                    write(unit = st, fmt = "(I1)") igndx
                    freq2 = trim(freq) // "_" // st
                else
                    freq2 = freq
                end if

                !> Print the output.
                select case (trim(adjustl(ifo%var_out(var_id)%out_fmt)))

                    case ("r2c")
                        call WriteR2C(fld_out, var_id, ifo, shd, freq2, dates, file_unit, keep_file_open, frame_no)

                    case ("txt")
                        call WriteTxt(fld_out, var_id, ifo, shd, freq2, dates, file_unit, keep_file_open, frame_no)

                    case ("csv")
                        call WriteCSV(fld_out, var_id, ifo, shd, freq2, dates, file_unit, keep_file_open, frame_no)

                end select !case (trim(adjustl(ifo%var_out(var_id)%out_fmt)))

                !> De-allocate the temporary fld and dates variables.
                deallocate(fld_out, dates)

            end if !(allocated(fld)) then

        end if !(now_time /= old_time) then

    end subroutine !check_write_var_out

    subroutine UpdateFIELDSOUT(vr, ts, ifo, &
                               pre, evap, rof, dstg, &
                               tbar, lqws, frws, &
                               rcan, sncan, &
                               pndw, sno, wsno, &
                               gflx, hfs,qevp ,&
                               thlq, thic,&
                               ignd, &
                               iday, iyear)

        !>------------------------------------------------------------------------------
        !>  Description: Update values in each time step
        !>------------------------------------------------------------------------------

        !Inputs
        integer :: ignd
        integer :: iday, iyear
        type(dates_model) :: ts
        type(info_out) :: ifo

        real, dimension(:), intent(in) :: pre, evap, rof, dstg, &
                                          rcan, sncan, &
                                          pndw, sno, wsno ,&
                                          hfs,qevp
        real, dimension(:, :), intent(in) :: tbar, lqws, frws,&
                                             gflx,thlq,thic

        !Inputs-Output
        type(out_flds) :: vr

        !Internals
        integer :: i, j, iy, im, iss, id
        character*50 :: vId

        call GetIndicesDATES(iday, iyear, iy, im, iss, id, ts)

        do i = 1, ifo%nr_out

!            vId = trim(adjustl(ifo%ids_var_out(i, 1)))
            vId = trim(adjustl(ifo%var_out(i)%name))

            select case (vId)

                case ('PREC', 'Rainfall', 'Rain', 'Precipitation')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%pre(iy, :) = vr%wbt_y%pre(iy, :) + pre

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%pre(im, :) = vr%wbt_m%pre(im, :) + pre

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%pre(iss, :) = vr%wbt_s%pre(iss, :) + pre

                    if (ifo%var_out(i)%out_d) &
                        vr%wbt_d%pre(id, :) = vr%wbt_d%pre(id, :) + pre

                case ('EVAP', 'Evapotranspiration')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%evap(iy, :) = vr%wbt_y%evap(iy, :) + evap

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%evap(im, :) = vr%wbt_m%evap(im, :) + evap

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%evap(iss, :) = vr%wbt_s%evap(iss, :) + evap

                    if (ifo%var_out(i)%out_d) &
                        vr%wbt_d%evap(id, :) = vr%wbt_d%evap(id, :) + evap

                case ('Runoff', 'ROF')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%rof(iy, :) = vr%wbt_y%rof(iy, :)  + rof

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%rof(im, :) = vr%wbt_m%rof(im, :) + rof

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%rof(iss, :) = vr%wbt_s%rof(iss, :) + rof

                    if (ifo%var_out(i)%out_d) &
                        vr%wbt_d%rof(id, :) = vr%wbt_d%rof(id, :) + rof

                case ('DeltaStorage', 'DSTG')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%dstg(iy, :) = vr%wbt_y%dstg(iy, :) + dstg

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%dstg(im, :) =  vr%wbt_m%dstg(im, :) + dstg

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%dstg(iss, :) = vr%wbt_s%dstg(iss, :) + dstg

                    if (ifo%var_out(i)%out_d) &
                        vr%wbt_d%dstg(id, :) = vr%wbt_d%dstg(id, :) + dstg

                case ('TempSoil', 'Temperature_soil_layers', 'TBAR')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%spt_y%tbar(iy, :, :) = vr%spt_y%tbar(iy, :, :) + tbar

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%spt_m%tbar(im, :, :) = vr%spt_m%tbar(im, :, :) + tbar

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%spt_s%tbar(iss, :, :) = vr%spt_s%tbar(iss, :, :) + tbar

                    if (ifo%var_out(i)%out_d) &
                        vr%spt_d%tbar(id, :, :) = vr%spt_d%tbar(id, : , :) + tbar

                case ('LQWS')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%lqws(iy, :, :) = vr%wbt_y%lqws(iy, :, :) + lqws

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%lqws(im, :, :) = vr%wbt_m%lqws(im, :, :) + lqws

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%lqws(iss, :, :) = vr%wbt_s%lqws(iss, :, :) + lqws

                    if (ifo%var_out(i)%out_d) &
                        vr%wbt_d%lqws(id, :, :) = vr%wbt_d%lqws(id, :, :) + lqws

                case ('FRWS')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%frws(iy, :, :) = vr%wbt_y%frws(iy, :, :) + frws

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%frws(im, :, :) = vr%wbt_m%frws(im, :, :) + frws

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%frws(iss, :, :) = vr%wbt_s%frws(iss, :, :) + frws

                    if (ifo%var_out(i)%out_d) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_d%frws(id, :, :) = vr%wbt_d%frws(id, :, :) + frws

                case ('RCAN')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%rcan(iy, :) = vr%wbt_y%rcan(iy, :) + rcan

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%rcan(im, :) = vr%wbt_m%rcan(im, :) + rcan

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%rcan(iss, :) = vr%wbt_s%rcan(iss, :) + rcan

                case ('SCAN', 'SNCAN')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%sncan(iy, :) = vr%wbt_y%sncan(iy, :) + sncan

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%sncan(im, :) = vr%wbt_m%sncan(im, :) + sncan

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%sncan(iss, :) = vr%wbt_s%sncan(iss, :) + sncan

                case ('PNDW')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%pndw(iy, :) = vr%wbt_y%pndw(iy, :) + pndw

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%pndw(im, :) = vr%wbt_m%pndw(im, :) + pndw

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%pndw(iss, :) = vr%wbt_s%pndw(iss, :) + pndw

                case ('SNO')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%sno(iy, :) = vr%wbt_y%sno(iy, :) + sno

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%sno(im, :) = vr%wbt_m%sno(im, :) + sno

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%sno(iss, :) = vr%wbt_s%sno(iss, :) + sno

                case ('WSNO')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wbt_y%wsno(iy, :) = vr%wbt_y%wsno(iy, :) + wsno

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wbt_m%wsno(im, :) = vr%wbt_m%wsno(im, :) + wsno

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wbt_s%wsno(iss, :) = vr%wbt_s%wsno(iss, :) + wsno

                case ("STG")

                    if (ifo%var_out(i)%out_y) then
                        vr%wbt_y%stg(iy, :) = vr%wbt_y%stg(iy, :) + &
                            rcan + sncan + pndw + sno + wsno
                        do j = 1, ignd
                            vr%wbt_y%stg(iy, :) = vr%wbt_y%stg(iy, :) + lqws(:, j) + frws(:, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_m) then
                        vr%wbt_m%stg(im, :) = vr%wbt_m%stg(im, :) + &
                            rcan + sncan + pndw + sno + wsno
                        do j = 1, ignd
                            vr%wbt_m%stg(im, :) = vr%wbt_m%stg(im, :) + lqws(:, j) + frws(:, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_s) then
                        vr%wbt_s%stg(iss, :) = vr%wbt_s%stg(iss, :) + &
                            rcan + sncan + pndw + sno + wsno
                        do j = 1, ignd
                            vr%wbt_s%stg(iss, :) = vr%wbt_s%stg(iss, :) + lqws(:, j) + frws(:, j)
                        end do
                    end if

                case ('GFLX', 'HeatConduction')

                    if (ifo%var_out(i)%out_y) &
                        vr%engt_y%gflx(iy, :, :) = vr%engt_y%gflx(iy, :, :) + gflx

                    if (ifo%var_out(i)%out_m) &
                        vr%engt_m%gflx(im, :, :) = vr%engt_m%gflx(im, :, :) + gflx

                    if (ifo%var_out(i)%out_s) &
                        vr%engt_s%gflx(iss, :, :) = vr%engt_s%gflx(iss, :, :) + gflx

                    if (ifo%var_out(i)%out_d) &
                        vr%engt_d%gflx(id, :, :) = vr%engt_d%gflx(id, :, :) + gflx

               case ('HFS', 'SensibleHeat')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%engt_y%hfs(iy, :) = vr%engt_y%hfs(iy, :) + hfs

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%engt_m%hfs(im, :) = vr%engt_m%hfs(im, :) + hfs

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%engt_s%hfs(iss, :) = vr%engt_s%hfs(iss, :) + hfs

                    if (ifo%var_out(i)%out_d)&
                        vr%engt_d%hfs(id, :) = vr%engt_d%hfs(id, :) + hfs

               case ('QEVP', 'LatentHeat')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%engt_y%qevp(iy, :) = vr%engt_y%qevp(iy, :) + qevp

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%engt_m%qevp(im, :) = vr%engt_m%qevp(im, :) + qevp

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%engt_s%qevp(iss, :) = vr%engt_s%qevp(iss, :) + qevp

                    if (ifo%var_out(i)%out_d) &
                        vr%engt_d%qevp(id, :) = vr%engt_d%qevp(id, :) + qevp

                case ('THLQ')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%spt_y%thlq(iy, :, :) = vr%spt_y%thlq(iy, :, :) + thlq

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%spt_m%thlq(im, :, :) = vr%spt_m%thlq(im, :, :) + thlq

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%spt_s%thlq(iss, :, :) = vr%spt_s%thlq(iss, :, :) + thlq

                    if (ifo%var_out(i)%out_d) &
                        vr%spt_d%thlq(id, :, :) = vr%spt_d%thlq(id, :, :) + thlq

                case ('THIC')

                    if (ifo%var_out(i)%out_y) & !trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%spt_y%thic(iy, :, :) = vr%spt_y%thic(iy, :, :) + thic

                    if (ifo%var_out(i)%out_m) & !trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%spt_m%thic(im, :, :) = vr%spt_m%thic(im, :, :) + thic

                    if (ifo%var_out(i)%out_s) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%spt_s%thic(iss, :, :) = vr%spt_s%thic(iss, :, :) + thic

                    if (ifo%var_out(i)%out_d) & !trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%spt_d%thic(id, :, :) = vr%spt_d%thic(id, :, :) + thic

!                case default
!                    print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."

            end select
        end do ! i = 1, ifo%nr_out

    end subroutine UpdateFIELDSOUT

    subroutine Write_Outputs(shd, fls, ts, ifo, vr)

        !>------------------------------------------------------------------------------
        !>  Description: Loop over the variablaes to write
        !>  output balance's fields in selected format
        !>------------------------------------------------------------------------------

        !Inputs
        type(ShedGridParams), intent(in) :: shd
        type(fl_ids), intent(in) :: fls
        type(dates_model), intent(in) :: ts
        type(info_out), intent(in) :: ifo
        type(out_flds), intent(in) :: vr

        !Outputs
        !Files

        !Internals
        integer i, j
        character*50 vId
        character*5 freq
        logical writeout

        do i = 1, ifo%nr_out

!            vId = trim(adjustl(ifo%ids_var_out(i, 1)))
            vId = trim(adjustl(ifo%var_out(i)%name))

            !> Determine if this is the last time-step of the hour.
            writeout = .false.
            if (ifo%var_out(i)%out_h) writeout = (mod(ic%ts_hourly, 3600/ic%dts) == 0)

            select case (vId)

                case ("FSDOWN")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%fsdown, freq, writeout, 882101, .false.)
                    end if

                case ("FSVH")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%fsvh, freq, writeout, 882102, .false.)
                    end if

                case ("FSIH")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%fsih, freq, writeout, 882103, .false.)
                    end if

                case ("FDL")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%fdl, freq, writeout, 882104, .false.)
                    end if

                case ("UL")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%ul, freq, writeout, 882105, .false.)
                    end if

                case ("TA")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%ta, freq, writeout, 882106, .false.)
                    end if

                case ("QA")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%qa, freq, writeout, 882107, .false.)
                    end if

                case ("PRES")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%pres, freq, writeout, 882108, .false.)
                    end if

                case ("PRE")
                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%mdt_h%pre, freq, writeout, 882109, .false.)
                    end if

                case ('PREC', 'Rainfall', 'Rain', 'Precipitation')

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_d) &
                        call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%pre, freq, writeout, 882122, .false.)
                    end if

                case ('EVAP', 'Evapotranspiration')

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_d) &
                        call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%evap, freq, writeout, 882110, .false.)
                    end if

                case ('Runoff', 'ROF')

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_d) &
                        call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%rof, freq, writeout, 882111, .false.)
                    end if

                case ('DeltaStorage', 'DSTG')

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_d) &
                        call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)

                case ('TempSoil', 'Temperature_soil_layers', 'TBAR')

                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_m) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_s) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_d) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, "D", shd, ts%nr_days, fls, j)
                        end do
                    end if

                case ('THLQ')

                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_m) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_s) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_d) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, "D", shd, ts%nr_days, fls, j)
                        end do
                    end if

             case ('THIC')

                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_m) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_s) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_d) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, "D", shd, ts%nr_days, fls, j)
                        end do
                    end if

                case ('GFLX', 'HeatConduction')

                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_m) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_s) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_d) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, "D", shd, ts%nr_days, fls, j)
                        end do
                    end if

                case ('HFS','SensibleHeat')

                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_m) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_s) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_d) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, "D", shd, ts%nr_days, fls, j)
                        end do
                    end if

                case ('QEVP','LatentHeat')

                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_m) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_s) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_d) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, "D", shd, ts%nr_days, fls, j)
                        end do
                    end if

                case ('LQWS')

                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_m) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_s) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_d) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, "D", shd, ts%nr_days, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        do j = 1, shd%lc%IGND
                            call check_write_var_out(shd, ifo, i, vr%wbt_h%lqws(:, :, j), freq, writeout, &
                                (882112 + (100000000*j)), .false., j)
                        end do
                    end if

                case ('FRWS')

                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_m) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_s) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_d) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, "D", shd, ts%nr_days, fls, j)
                        end do
                    end if

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        do j = 1, shd%lc%IGND
                            call check_write_var_out(shd, ifo, i, vr%wbt_h%frws(:, :, j), freq, writeout, &
                                (882113 + (100000000*j)), .false., j)
                        end do
                    end if

                case ('RCAN')

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%rcan, freq, writeout, 882114, .false.)
                    end if

                case ('SCAN', 'SNCAN')

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%sncan, freq, writeout, 882115, .false.)
                    end if

                case ('PNDW')

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%pndw, freq, writeout, 882116, .false.)
                    end if

                case ('SNO')

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%sno, freq, writeout, 882117, .false.)
                    end if

                case ('WSNO')

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%wsno, freq, writeout, 882118, .false.)
                    end if

                case ("STG")

                    if (ifo%var_out(i)%out_y) &
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)

                    if (ifo%var_out(i)%out_m) &
                        call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)

                    if (ifo%var_out(i)%out_s) &
                        call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wbt_h%stg, freq, writeout, 882119, .false.)
                    end if

                case ("WR_RUNOFF")

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wroutt_h%rof, freq, writeout, 882120, .false.)
                    end if

                case ("WR_RECHARGE")

                    if (ifo%var_out(i)%out_h) then
                        freq = "H"
                        call check_write_var_out(shd, ifo, i, vr%wroutt_h%rchg, freq, writeout, 882121, .false.)
                    end if

!                case default
!                    print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."

                end select
            end do ! i = 1, ifo%nr_out

    end subroutine Write_Outputs

    !>******************************************************************************

    subroutine WriteFields_i(vr, ts, ifo, indx, freq, shd, nt, fls, igndx)

        !>------------------------------------------------------------------------------
        !>  Description: Loop over the variables to write
        !>  output balance's fields in selected format
        !>------------------------------------------------------------------------------

        !Inputs
        type(out_flds), intent(in) :: vr
        type(dates_model), intent(in) :: ts
        type(info_out), intent(in) :: ifo
        type(ShedGridParams), intent(in) :: shd
        type (fl_ids), intent(in) :: fls

        integer, intent(in) :: indx
        integer, intent(in) :: nt
        character*1, intent(in) :: freq

        integer, intent(in), optional :: igndx

        !Internals
        integer i, nr
        character*50 vId, tfunc
        integer, dimension(:), allocatable :: days
        character*5 freq2
        character*5 st
        real :: fld(shd%NA, nt)

        integer, dimension(:, :), allocatable :: dates

        vId = trim(adjustl(ifo%var_out(indx)%out_fmt))
        tfunc = trim(adjustl(ifo%var_out(indx)%out_acc))

        select case (trim(adjustl(ifo%var_out(indx)%name)))

            case ('PREC', 'Rainfall', 'Rain', 'Precipitation')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%pre(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%pre(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%pre(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'D') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_d%pre(i, :)
                    end do
                end if

            case ('EVAP', 'Evapotranspiration')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%evap(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%evap(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%evap(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'D') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_d%evap(i, :)
                    end do
                end if

            case ('Runoff', 'ROF')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%rof(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%rof(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%rof(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'D') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_d%rof(i, :)
                    end do
                end if

            case ('DeltaStorage', 'DSTG')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%dstg(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%dstg(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%dstg(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'D') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_d%dstg(i, :)
                    end do
                end if

            case ('TempSoil', 'Temperature_soil_layers', 'TBAR')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_y%tbar(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_m%tbar(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_s%tbar(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == "D") then
                    do i = 1, nt
                        fld(:, i) = vr%spt_d%tbar(i, :, igndx)
                    end do
                end if

            case ('THLQ')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_y%thlq(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_m%thlq(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_s%thlq(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == "D") then
                    do i = 1, nt
                        fld(:, i) = vr%spt_d%thlq(i, :, igndx)
                    end do
                end if

            case ('THIC')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_y%thic(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_m%thic(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_s%thic(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == "D") then
                    do i = 1, nt
                        fld(:, i) = vr%spt_d%thic(i, :, igndx)
                    end do
                end if

            case ('GFLX')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_y%gflx(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_m%gflx(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_s%gflx(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == "D") then
                    do i = 1, nt
                        fld(:, i) = vr%engt_d%gflx(i, :, igndx)
                    end do
                end if

            case ('HFS')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_y%hfs(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_m%hfs(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_s%hfs(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'D') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_d%hfs(i, :)
                    end do
                end if

            case ('QEVP')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_y%qevp(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_m%qevp(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_s%qevp(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'D') then
                    do i = 1, nt
                        fld(:, i) = vr%engt_d%qevp(i, :)
                    end do
                end if

            case ('LQWS')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%lqws(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%lqws(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%lqws(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == "D") then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_d%lqws(i, :, igndx)
                    end do
                end if

            case ('FRWS')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%frws(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%frws(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%frws(i, :, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == "D") then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_d%frws(i, :, igndx)
                    end do
                end if

            case ('RCAN')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%rcan(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%rcan(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%rcan(i, :)
                    end do
                end if

            case ('SCAN', 'SNCAN')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%sncan(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%sncan(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%sncan(i, :)
                    end do
                end if

            case ('PNDW')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%pndw(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%pndw(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%pndw(i, :)
                    end do
                end if

            case ('SNO')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%sno(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%sno(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%sno(i, :)
                    end do
                end if

            case ('WSNO')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%wsno(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%wsno(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%wsno(i, :)
                    end do
                end if

            case ("STG")

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_y%stg(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_m%stg(i, :)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wbt_s%stg(i, :)
                    end do
                end if

            case ('ZPND')
                print *, "Output of variable '" // trim(adjustl(ifo%var_out(indx)%name)) // &
                    "' is not Implemented yet."
                print *, "Use PNDW for ponded water at the surface [mm]."

            case ('ROFF')
                print *, "Output of variable 'ROF' using keyword '" // &
                    trim(adjustl(ifo%var_out(indx)%name)) // "' is not supported."
                print *, "Use ROF for total runoff."

!            case ('THIC', 'ICEContent_soil_layers')
!                print *, "Output of variable '" // trim(adjustl(ifo%var_out(indx)%name)) // &
!                    "' is not Implemented yet."
!                print *, "Use LQWS for liquid water stored in the soil [mm]."

!            case ('THLQ', 'LiquidContent_soil_layers')
!                print *, "Output of variable '" // trim(adjustl(ifo%var_out(indx)%name)) // &
!                    "' is not Implemented yet."
!                print *, "Use FRWS for frozen water stored in the soil [mm]."

            case default
                print *, "Output of variable '" // trim(adjustl(ifo%var_out(indx)%name)) // &
                    "' is not Implemented yet."

        end select !case (trim(adjustl(vars)))

        if (tfunc == 'AVG') then

            allocate(days(nt))

            select case (freq)

                case ('Y')
                    days = ts%daysINyears

                case ('M')
                    days = ts%daysINmonths

                case ('S')
                    days = ts%daysINseasons

                case default
                    days = 1

            end select !freq

            do i = 1, nt
                fld(:, i) = fld(:, i)/days(i)
            end do

            deallocate(days)

        end if

        select case (freq)

            case ('Y')
                allocate(dates(ts%nyears, 2))
                dates(:, 1) = ts%years
                dates(:, 2) = 1

            case ('M')
                allocate(dates(ts%nmonths, 2))
                dates = ts%mnthyears

            case ('S')
                allocate(dates(ts%nseason, 2))
                do i = 1, 12
                    dates(i, 1) = ts%years(1)
                    dates(i, 2) = i
                end do

            case ("D")
                allocate(dates(ts%nr_days, 3))
                do i = 1, ts%nr_days
                    dates(i, 1) = ts%dates(i, 1)
                    dates(i, 2) = ts%dates(i, 2)
                    dates(i, 3) = ts%dates(i, 3)
                end do

            case ("H")
!                allocate(dates(1, 5))
!                dates(1, 1) = ic%now%year
!                dates(1, 2) = ic%now%month
!                dates(1, 3) = ic%now%day
!                dates(1, 4) = ic%now%jday
!                dates(1, 5) = ic%now%hour

        end select !freq

        if (present(igndx)) then
            write(st, '(i5)') igndx
            freq2 = freq // '_' // trim(adjustl(st))
        else
            freq2 = freq
        end if
        vId = lowercase(vId)
        select case (vId)

            case('seq', 'binseq')
                call WriteSeq(fld, indx, ifo, freq2, dates)

            case('r2c')
                call WriteR2C(fld, indx, ifo, shd, freq2, dates)

            case('tsi')
                call WriteTsi(fld, indx, ifo, freq2, dates, fls)

            case ('txt')
                call WriteTxt(fld, indx, ifo, shd, freq2, dates)

            case ('csv')
                call WriteCSV(fld, indx, ifo, shd, freq2, dates)

            case default
                print *, "Output as file format '" // trim(adjustl(vId)) // "' is not implemented yet."

        end select

if (allocated(dates)) &
        deallocate(dates)

    end subroutine WriteFields_i

!>******************************************************************************
    subroutine WriteTsi(fld, indx, info, freq, dates, fls)

        !Inputs
        real fld(:, :)
        integer indx
        character*5 freq
        integer dates(:, :)
        type(info_out) :: info
        type(fl_ids) :: fls

        !Internal
        character*450 flOut
        integer ios, i, j
        integer nt, unitfl

!        if ((VARIABLEFILESFLAG == 1) .and. (fls%fl(mfk%out_response)%isInit)) then
        if (len_trim(adjustl(fls%fl(mfk%out_response)%fn)) > 0) then
            flOut = trim(adjustl(fls%pthOut)) // &
                    trim(adjustl(info%var_out(indx)%name)) // &
                    '_' // trim(adjustl(freq)) // '_' // &
                    trim(adjustl(fls%fl(mfk%out_response)%fn)) // '.ts'
!            unitfl = fls%fl(mfk%out_response)%iun
        else
            flOut = trim(adjustl(info%pthOut)) // &
                    trim(adjustl(info%var_out(indx)%name)) // &
                    '_' // trim(adjustl(freq)) // '.ts'
!            unitfl = 444
        end if

        unitfl = fls%fl(mfk%out_response)%iun
        open(unit = unitfl               , &
             file = trim(adjustl(flOut)) , &
             status = 'replace'          , &
             form = 'formatted'          , &
             action = 'write'            , &
             iostat = ios                )

        nt = size(dates(:, 1))

        do i = 1, nt
            write(unitfl, *) (fld(info%var_out(indx)%i_grds(j), i), j = 1, size(info%var_out(indx)%i_grds))
        end do

        close(unitfl)

    end subroutine WriteTsi

    !>******************************************************************************

    !>******************************************************************************

    subroutine WriteSeq(fld, indx, info, freq, dates)

        !>------------------------------------------------------------------------------
        !>  Description: Write bin sequential file
        !>
        !>------------------------------------------------------------------------------

        !Inputs
        real fld(:, :)
        integer indx
        character*5 freq
        integer dates(:, :)
        type(info_out) :: info

        !Internal
        character*450 flOut
        integer ios, i
        integer nt

        flOut = trim(adjustl(info%pthOut)) // &
                trim(adjustl(info%var_out(indx)%name)) // &
                '_' // trim(adjustl(freq)) // '.seq'

        open(unit   = 882                  , &
             file   = trim(adjustl(flOut)) , &
             status = 'replace'            , &
             form   = 'unformatted'        , &
             action = 'write'              , &
             access = 'sequential'         , &
             iostat = ios                  )

        nt = size(dates(:, 1))

        do i = 1, nt
            write(882) i
            write(882) fld(:, i)
        end do

        close(882)

    end subroutine WriteSeq

    !>******************************************************************************

    subroutine WriteR2C(fld, indx, info, shd, freq, dates, file_unit, keep_file_open, frame_no)

        !>------------------------------------------------------------------------------
        !>  Description: Write r2c file
        !>
        !>------------------------------------------------------------------------------

        !Inputs
        real fld(:, :)
        integer indx
        type(info_out) :: info
        type(ShedGridParams), intent(in) :: shd
        character(5) freq
        integer, allocatable :: dates(:, :)
        integer, optional :: file_unit
        logical, optional :: keep_file_open
        integer, optional :: frame_no

        !Internal
        character(450) flOut
        integer ios, i, un, nfr
        integer na1, nt, j, t, k
        real, dimension(:, :), allocatable :: data_aux
        character(10) ctime
        character(8) cday
        logical opened_status, close_file

        flOut = trim(adjustl(info%pthOut)) // trim(adjustl(info%var_out(indx)%name)) // '_' // trim(adjustl(freq)) // '.r2c'

        if (present(file_unit)) then
            un = file_unit
        else
            un = 882
        end if

        inquire(un, opened = opened_status)

        if (.not. opened_status) then

            open(un, file = trim(adjustl(flOut)), status = 'replace', form = 'formatted', action = 'write', iostat = ios)

            write(un, 3005) '########################################'
            write(un, 3005) ':FileType r2c  ASCII  EnSim 1.0         '
            write(un, 3005) '#                                       '
            write(un, 3005) '# DataType               2D Rect Cell   '
            write(un, 3005) '#                                       '
            write(un, 3005) ':Application               MeshOutput   '
            write(un, 3005) ':Version                 1.0.00         '
            write(un, 3005) ':WrittenBy          MESH_DRIVER         '

            call date_and_time(cday, ctime)

            write(un, 3010) ':CreationDate       ', cday(1:4), cday(5:6), cday(7:8), ctime(1:2), ctime(3:4)

            write(un, 3005) '#                                       '
            write(un, 3005) '#---------------------------------------'
            write(un, 3005) '#                                       '
            write(un, 3020) ':Name               ', info%var_out(indx)%name !info%ids_var_out(indx, 1)
            write(un, 3005) '#                                       '
            write(un, 3004) ':Projection         ', shd%CoordSys%Proj

            if (shd%CoordSys%Proj == 'LATLONG   ') &
                write(un, 3004) ':Ellipsoid          ', shd%CoordSys%Ellips
            if (shd%CoordSys%Proj == 'UTM       ') then
                write(un, 3004) ':Ellipsoid          ', shd%CoordSys%Ellips
                write(un, 3004) ':Zone               ', shd%CoordSys%Zone
            end if

            write(un, 3005) '#                                       '
            write(un, 3003) ':xOrigin            ', shd%xOrigin
            write(un, 3003) ':yOrigin            ', shd%yOrigin
            write(un, 3005) '#                                       '
            write(un, 3005) ':SourceFile            MESH_DRIVER      '
            write(un, 3005) '#                                       '

            write(un, 3020) ':AttributeName      ', info%var_out(indx)%name !info%ids_var_out(indx, 1)

            write(un, 3020) ':AttributeUnits     ', '' !info%ids_var_out(indx, 2)
            write(un, 3005) '#                                       '
            write(un, 3001) ':xCount             ', shd%xCount
            write(un, 3001) ':yCount             ', shd%yCount
            write(un, 3003) ':xDelta             ', shd%xDelta
            write(un, 3003) ':yDelta             ', shd%yDelta
            write(un, 3005) '#                                       '
            write(un, 3005) '#                                       '
            write(un, 3005) ':endHeader                              '

        end if !(.not. opened_status) then

        if (allocated(dates)) then

            nt = size(dates(:, 1))

            do t = 1, nt

                if (present(frame_no)) then
                    nfr = frame_no
                else
                    nfr = t
                end if

                if (size(dates, 2) == 5) then
                    write(un, 9000) ':Frame', nfr, nfr, dates(t, 1), dates(t, 2), dates(t, 3), dates(t, 5), 0
                elseif (size(dates, 2) == 3) then
                    write(un, 9000) ':Frame', nfr, nfr, dates(t, 1), dates(t, 2), dates(t, 3), 0, 0
                else
                    write(un, 9000) ':Frame', nfr, nfr, dates(t, 1), dates(t, 2), 1, 0, 0
                end if

                allocate(data_aux(shd%yCount, shd%xCount))
                data_aux = 0.0

                do k = 1, shd%NA
                    data_aux(shd%yyy(k), shd%xxx(k)) = fld(k, t)
                end do

                do j = 1, shd%yCount
                    write(un, '(999(e12.6,2x))') (data_aux(j, i), i = 1, shd%xCount)
                end do

                write(un, '(a)') ':EndFrame'

                deallocate(data_aux)

            end do

        end if !(allocated(dates)) then

        if (present(keep_file_open)) then
            close_file = .not. keep_file_open
        else
            close_file = .true.
        end if

        if (close_file) close(un)

        3000 format(a10, i5)
        3001 format(a20, i16)
        3002 format(2a20)
        3003 format(a20, f16.7)
        3004 format(a20, a10, 2x, a10)
        3005 format(a40)
        3006 format(a3, a10)
        3007 format(a14, i5, a6, i5)
        3010 format(a20, a4, '-', a2, '-', a2, 2x, a2, ':', a2)
        3012 format(a9)
        3020 format(a20, a40)
        9000 format(a6, 2i10, 3x, '"', i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ':00.000"')

    end subroutine WriteR2C

    !> Subroute: WriteTxt
    !> Write the output to file in text format.
    subroutine WriteTxt(fld, indx, info, shd, freq, dates, file_unit, keep_file_open, frame_no)

        !Inputs
        real fld(:, :)
        integer indx
        type(info_out) :: info
        type(ShedGridParams), intent(in) :: shd
        character(5) freq
        integer, allocatable :: dates(:, :)
        integer, optional :: file_unit
        logical, optional :: keep_file_open
        integer, optional :: frame_no

        !Internal
        character(450) flOut
        integer ios, i, un, nfr
        integer na1, nt, j, t, k
        real, dimension(:, :), allocatable :: data_aux
        character(10) ctime
        character(8) cday
        logical opened_status, close_file

        flOut = trim(adjustl(info%pthOut)) // trim(adjustl(info%var_out(indx)%name)) // '_' // trim(adjustl(freq)) // '.txt'

        if (present(file_unit)) then
            un = file_unit
        else
            un = 882
        end if

        inquire(un, opened = opened_status)
        if (.not. opened_status) then
            open(un, file = trim(adjustl(flOut)), status = 'replace', form = 'formatted', action = 'write')
        end if !(.not. opened_status) then

        if (allocated(dates)) then

            do t = 1, size(dates(:, 1))

                if (info%var_out(indx)%opt_printdate) then
                    if (size(dates, 2) == 5) then
                        write(un, 9000, advance = 'no') dates(t, 1), dates(t, 2), dates(t, 3), dates(t, 5), 0
                    elseif (size(dates, 2) == 3) then
                        write(un, 9000, advance = 'no') dates(t, 1), dates(t, 2), dates(t, 3), 0, 0
                    else
                        write(un, 9000, advance = 'no') dates(t, 1), dates(t, 2), 1, 0, 0
                    end if
                end if

                select case (info%var_out(indx)%out_seq)

                    case ('gridorder')
                        write(un, 9001) fld(:, t)

                    case ('shedorder')
                        allocate(data_aux(shd%yCount, shd%xCount))
                        data_aux = 0.0
                        do k = 1, shd%NA
                            data_aux(shd%yyy(k), shd%xxx(k)) = fld(k, t)
                        end do
                        do j = 1, (shd%yCount - 1)
                            write(un, 9001, advance = 'no') (data_aux(j, i), i = 1, shd%xCount)
                        end do
                        write(un, 9001) (data_aux(shd%yCount, i), i = 1, shd%xCount)
                        deallocate(data_aux)

                end select

            end do

        end if !(allocated(dates)) then

        if (present(keep_file_open)) then
            close_file = .not. keep_file_open
        else
            close_file = .true.
        end if

        if (close_file) close(un)

        9000 format('"', i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ':00.000"', 2x)
        9001 format(999(e12.6, 2x))

    end subroutine !WriteTxt

    !> Subroute: WriteCSV
    !> Write the output to file in CSV format.
    subroutine WriteCSV(fld, indx, info, shd, freq, dates, file_unit, keep_file_open, frame_no)

        !Inputs
        real fld(:, :)
        integer indx
        type(info_out) :: info
        type(ShedGridParams), intent(in) :: shd
        character(5) freq
        integer, allocatable :: dates(:, :)
        integer, optional :: file_unit
        logical, optional :: keep_file_open
        integer, optional :: frame_no

        !Internal
        character(450) flOut
        integer ios, i, un, nfr
        integer na1, nt, j, t, k
        real, dimension(:, :), allocatable :: data_aux
        character(10) ctime
        character(8) cday
        logical opened_status, close_file

        flOut = trim(adjustl(info%pthOut)) // trim(adjustl(info%var_out(indx)%name)) // '_' // trim(adjustl(freq)) // '.csv'

        if (present(file_unit)) then
            un = file_unit
        else
            un = 882
        end if

        inquire(un, opened = opened_status)
        if (.not. opened_status) then
            open(un, file = trim(adjustl(flOut)), status = 'replace', form = 'formatted', action = 'write')
        end if !(.not. opened_status) then

        if (allocated(dates)) then

            do t = 1, size(dates(:, 1))

                if (info%var_out(indx)%opt_printdate) then
                    if (size(dates, 2) == 5) then
                        write(un, 9000, advance = 'no') dates(t, 1), dates(t, 2), dates(t, 3), dates(t, 5), 0
                    elseif (size(dates, 2) == 3) then
                        write(un, 9000, advance = 'no') dates(t, 1), dates(t, 2), dates(t, 3), 0, 0
                    else
                        write(un, 9000, advance = 'no') dates(t, 1), dates(t, 2), 1, 0, 0
                    end if
                end if

                select case (info%var_out(indx)%out_seq)

                    case ('gridorder')
                        write(un, 9001) fld(:, t)

                    case ('shedorder')
                        allocate(data_aux(shd%yCount, shd%xCount))
                        data_aux = 0.0
                        do k = 1, shd%NA
                            data_aux(shd%yyy(k), shd%xxx(k)) = fld(k, t)
                        end do
                        do j = 1, (shd%yCount - 1)
                            write(un, 9001, advance = 'no') (data_aux(j, i), i = 1, shd%xCount)
                        end do
                        write(un, 9001) (data_aux(shd%yCount, i), i = 1, shd%xCount)
                        deallocate(data_aux)

                end select

            end do

        end if !(allocated(dates)) then

        if (present(keep_file_open)) then
            close_file = .not. keep_file_open
        else
            close_file = .true.
        end if

        if (close_file) close(un)

        9000 format('"', i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ':00.000"', ',')
        9001 format(999(e12.6, ','))

    end subroutine !WriteCSV

end module model_output
