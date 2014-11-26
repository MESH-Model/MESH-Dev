module model_output

    !>******************************************************************************
    !>  Athor: Gonzalo Sapriza Azuri
    !>******************************************************************************

    use model_dates

    implicit none

    !> Data type to store basin information
    !* na: Number of grid cells [-]
    !* nm: Number of GRUs [-]
    !* ignd: Number of soil layers per grid [-]
    type basin_info
        integer :: na, nm, ignd
    end type basin_info

    !> Data type to store components of the water balance
    !* pre(:): Precipitation [kg m-2] (1: grid)
    !* evap(:): Evaporation (water lost by evapotranspiration and sublimation, both rain and snow components) [kg m-2] (1: grid)
    !* rof(:): Runoff (combination of overland-, subsurface-, and base-flows) [kg m-2] (1: grid)
    !* rofo(:): Overland flow component of runoff [kg m-2] (1: grid)
    !* rofs(:): Subsurface flow component of runoff [kg m-2] (1: grid)
    !* rofb(:): Baseflow component of runoff [kg m-2] (1: grid)
    !* rcan(:): Rainfall intercepted by the canopy [kg m-2] (1: grid)
    !* sncan(:): Snowfall intercepted by the canopy [kg m-2] (1: grid)
    !* pndw(:): Water ponded at the surface of the soil [kg m-2] (1: grid)
    !* sno(:): Snowpack at the surface of the soil [kg m-2] (1: grid)
    !* wsno(:): Water stored in the snowpack [kg m-2] (1: grid)
    !* stg(:): Water stored in the system [kg m-2] (1: grid)
    !* dstg(:): Difference of water stored in the system compared to the previous time-step of the element [kg m-2] (1: grid)
    !* grid_area(:): Fractional area of the grid-square [m2 m-2] (1: grid)
    !* lqws(:, :): Water stored in the soil matrix [kg m-2] (1: grid, 2: soil layer)
    !* frws(:, :): Frozen water (ice) stored in the soil matrix [kg m-2] (1: grid, 2: soil layer)
    !* basin_area: Total fractional area of the basin [m2 m-2] (1: grid)
    type water_balance

        real, dimension(:), allocatable :: &
            pre, evap, rof, &
            rofo, rofs, rofb, &
            rcan, sncan, pndw, sno, wsno, &
            stg, dstg, &
            grid_area
        real, dimension(:, :), allocatable :: &
            lqws, frws
        real :: basin_area

        contains

        procedure :: init => init_water_balance

    end type water_balance

    !> Data type to store components of the energy balance
    !* hfs: (1: grid)
    !* qevp: (1: grid)
    type energy_balance

        real, dimension(:), allocatable :: &
            hfs, qevp

    end type

    !> Data type to store soil parameters.
    !* tbar: Temperature of the soil layer (1: grid, 2: soil layer).
    !* thic: Fractional (frozen water) ice-content stored in the soil layer (1: grid, 2: soil layer).
    !* thlq: Fractional water-content stored in the soil layer (1: grid, 2: soil layer).
    type soil_parameters

        real, dimension(:, :), allocatable :: &
            tbar, thic, thlq

        contains

        procedure :: init => init_soil_parameters

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
    type OUT_INTER_RESP

        !Rank ID
        integer :: na_id

        !Runoff
        real, dimension(:)  , allocatable :: rofo, rofs, rofb

        !State variable and flux in soil layers
        real, dimension(:,:), allocatable :: gflx
        real, dimension(:,:), allocatable :: thlq, thic, tbar

    end type

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
        type(water_balance), dimension(:), allocatable :: &
            wb_y, wb_m, wb_s, wb_d
        type(soil_parameters), dimension(:), allocatable :: &
            sp_y, sp_m, sp_s, sp_d

        contains

        procedure :: init => init_out_flds

    end type

    !>******************************************************************************
    !* flIn: File that contains the input information of the variables that we want to init and the frequency.
    !* pthOut: path out.
    !* ids_var_out: Array that contains the IDs of the files and frequency (e.g., 'PREPC', 'Y', 'M', 'S', 'CUM', 'SEQ').
    !* nr_out: Number of output variables.
    TYPE info_out

        character*450 :: flIn
        character*450 :: pthOut
        character*20, dimension(:,:), allocatable :: ids_var_out
        integer :: nr_out

    END TYPE
    !>******************************************************************************

    contains

    subroutine init_out_flds(vr, bi, ts)

        !> Type variable.
        class(out_flds) :: vr

        !> Input variables.
        type(basin_info), intent(in) :: bi
        type(dates_model), intent(in) :: ts

        !> Local variables.
        integer :: i

        !> Allocate arrays using basin info.
        allocate( &
            vr%wb_y(ts%nyears), vr%wb_m(ts%nmonths), vr%wb_s(ts%nseason), &
            vr%sp_y(ts%nyears), vr%sp_m(ts%nmonths), vr%sp_s(ts%nseason))

        !> Initialize sub-variables.
        do i = 1, ts%nyears
            call vr%wb_y(i)%init(bi)
            call vr%sp_y(i)%init(bi)
        end do
        do i = 1, ts%nmonths
            call vr%wb_m(i)%init(bi)
            call vr%sp_m(i)%init(bi)
        end do
        do i = 1, ts%nseason
            call vr%wb_s(i)%init(bi)
            call vr%sp_s(i)%init(bi)
        end do

    end subroutine !init_out_flds

    subroutine init_water_balance(wb, bi)

        !> Type variable.
        class(water_balance) :: wb

        !> Input variables.
        type(basin_info), intent(in) :: bi

        !> Allocate arrays using basin info.
        allocate( &
            wb%pre(bi%na), wb%evap(bi%na), wb%rof(bi%na), &
            wb%rofo(bi%na), wb%rofs(bi%na), wb%rofb(bi%na), &
            wb%rcan(bi%na), wb%sncan(bi%na), &
            wb%pndw(bi%na), wb%sno(bi%na), wb%wsno(bi%na), &
            wb%stg(bi%na), wb%dstg(bi%na), &
            wb%grid_area(bi%na), &
            wb%lqws(bi%na, bi%ignd), wb%frws(bi%na, bi%ignd))

        !> Explicitly set all variables to 0.0.
        wb%pre = 0.0
        wb%evap = 0.0
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

    subroutine init_soil_parameters(sp, bi)

        !> Type variable.
        class(soil_parameters) :: sp

        !> Input variables.
        type(basin_info), intent(in) :: bi

        !> Allocate arrays using basin info.
        allocate( &
            sp%tbar(bi%na, bi%ignd), &
            sp%thic(bi%na, bi%ignd), sp%thlq(bi%na, bi%ignd))

        !> Explicitly set all variables to 0.0.
        sp%tbar = 0.0
        sp%thic = 0.0
        sp%thlq = 0.0

    end subroutine !init_soil_parameters

    !>******************************************************************************
    subroutine Init_Internal_resp(pmf_r, ts, ignd, naid)

        !>----------------------------------------------------------------------
        !>  Description: Init output of internal response
        !>  Allocatation
        !>----------------------------------------------------------------------

        !Inputs
        real, intent(in) :: naid
        integer, intent(in) :: ignd
        type(dates_model), intent(in) :: ts

        !Output
        type(OUT_INTER_RESP), intent(inout) :: pmf_r

        !>--------------Main Subtrouine start-----------------------------------

        allocate(pmf_r%rofo(ts%nr_days), &
                 pmf_r%rofs(ts%nr_days), &
                 pmf_r%rofb(ts%nr_days))

        allocate(pmf_r%gflx(ts%nr_days, ignd), &
                 pmf_r%thlq(ts%nr_days, ignd), &
                 pmf_r%thic(ts%nr_days, ignd), &
                 pmf_r%tbar(ts%nr_days, ignd))

        pmf_r%na_id = naid

    end subroutine Init_Internal_resp

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
            bal%wb%frws(idate,i) = bal%wb%frws(idate,i) + frws(i)
            bal%wb%lqws(idate,i) = bal%wb%lqws(idate,i) + lqws(i)
            if (isavg) then
                bal%wb%frws(idate,i) = bal%wb%frws(idate,i)/real(nhours)
                bal%wb%lqws(idate,i) = bal%wb%lqws(idate,i)/real(nhours)
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

    subroutine init_out(vr, ts, ifo, bi)

        !>------------------------------------------------------------------------------
        !>  Description: Init Fields
        !>
        !>------------------------------------------------------------------------------

        !Inputs
        type(basin_info) :: bi

        !Inputs-Output
        type(out_flds) :: vr
        type(dates_model) :: ts
        type(info_out) :: ifo

        !Internals
        integer :: ios, i, j, k
        character*50 :: vId

        !>--------------Main Subtrouine start-----------------------------------------------

        open(unit = 909, &
             file = 'outputs_balance.txt', &
             status = 'old', &
             action = 'read', &
             iostat = ios)

        ifo%flIn = 'outputs_balance.txt'

        read(909, *) ifo%pthOut
        read(909, *) ifo%nr_out

        allocate(ifo%ids_var_out(ifo%nr_out, 6))

        !> Initialize variable.
        call vr%init(bi, ts)

        do i = 1, ifo%nr_out

            !> Read configuration information from file.
            read(909, *) (ifo%ids_var_out(i, j), j = 1, 6)

            !> Yearly:
            if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') &
                allocate(vr%sp_y(ts%nyears), vr%wb_y(ts%nyears))

            !> Monthly:
            if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') &
                allocate(vr%sp_m(ts%nmonths), vr%wb_m(ts%nmonths))

            !> Seasonally:
            if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') &
                allocate(vr%sp_s(ts%nseason), vr%wb_s(ts%nseason))

            !> Extract variable ID.
            vId = trim(adjustl(ifo%ids_var_out(i, 1)))
            select case (vId)

                case ('PREC', 'Rainfall', 'Rain', 'Precipitation')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%pre(bi%na))
                            vr%wb_y(k)%pre = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%pre(bi%na))
                            vr%wb_m(k)%pre = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%pre(bi%na))
                            vr%wb_s(k)%pre = 0.0
                        end do
                    end if

                case ('EVAP', 'Evapotranspiration')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%evap(bi%na))
                            vr%wb_y(k)%evap = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%evap(bi%na))
                            vr%wb_m(k)%evap = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%evap(bi%na))
                            vr%wb_s(k)%evap = 0.0
                        end do
                    end if

                case ('Runoff', 'ROF')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%rof(bi%na))
                            vr%wb_y(k)%rof = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%rof(bi%na))
                            vr%wb_m(k)%rof = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%rof(bi%na))
                            vr%wb_s(k)%rof = 0.0
                        end do
                    end if

                case ('DeltaStorage', 'DSTG')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%dstg(bi%na))
                            vr%wb_y(k)%dstg = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%dstg(bi%na))
                            vr%wb_m(k)%dstg = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%dstg(bi%na))
                            vr%wb_s(k)%dstg = 0.0
                        end do
                    end if

                case ('TempSoil', 'Temperature_soil_layers', 'TBAR')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%sp_y(k)%tbar(bi%na, bi%ignd))
                            vr%sp_y(k)%tbar = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%sp_m(k)%tbar(bi%na, bi%ignd))
                            vr%sp_m(k)%tbar = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%sp_s(k)%tbar(bi%na, bi%ignd))
                            vr%sp_s(k)%tbar = 0.0
                        end do
                    end if

                case ('LQWS')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%lqws(bi%na, bi%ignd))
                            vr%wb_y(k)%lqws = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%lqws(bi%na, bi%ignd))
                            vr%wb_m(k)%lqws = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%lqws(bi%na, bi%ignd))
                            vr%wb_s(k)%lqws = 0.0
                        end do
                    end if

                case ('FRWS')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%frws(bi%na, bi%ignd))
                            vr%wb_y(k)%frws = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%frws(bi%na, bi%ignd))
                            vr%wb_m(k)%frws = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%frws(bi%na, bi%ignd))
                            vr%wb_s(k)%frws = 0.0
                        end do
                    end if

                case ('RCAN')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%rcan(bi%na))
                            vr%wb_y(k)%rcan = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%rcan(bi%na))
                            vr%wb_m(k)%rcan = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%rcan(bi%na))
                            vr%wb_s(k)%rcan = 0.0
                        end do
                    end if

                case ('SCAN', 'SNCAN')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%sncan(bi%na))
                            vr%wb_y(k)%sncan = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%sncan(bi%na))
                            vr%wb_m(k)%sncan = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%sncan(bi%na))
                            vr%wb_s(k)%sncan = 0.0
                        end do
                    end if

                case ('PNDW')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%pndw(bi%na))
                            vr%wb_y(k)%pndw = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%pndw(bi%na))
                            vr%wb_m(k)%pndw = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%pndw(bi%na))
                            vr%wb_s(k)%pndw = 0.0
                        end do
                    end if

                case ('SNO')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%sno(bi%na))
                            vr%wb_y(k)%sno = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%sno(bi%na))
                            vr%wb_m(k)%sno = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%sno(bi%na))
                            vr%wb_s(k)%sno = 0.0
                        end do
                    end if

                case ('WSNO')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'zY') then
                        do k = 1, ts%nyears
                            allocate(vr%wb_y(k)%wsno(bi%na))
                            vr%wb_y(k)%wsno = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'zM') then
                        do k = 1, ts%nmonths
                            allocate(vr%wb_m(k)%wsno(bi%na))
                            vr%wb_m(k)%wsno = 0.0
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'zS') then
                        do k = 1, ts%nseason
                            allocate(vr%wb_s(k)%wsno(bi%na))
                            vr%wb_s(k)%wsno = 0.0
                        end do
                    end if

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

                case default
                    print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."

            end select
        end do ! i = 1, ifo%nr_out

        close(unit = 909)

    end subroutine Init_out

    subroutine UpdateFIELDSOUT(vr, ts, ifo, &
                               pre, evap, rof, dstg, &
                               tbar, lqws, frws, &
                               rcan, sncan, &
                               pndw, sno, wsno, &
                               na,  ignd, &
                               iday, iyear)

        !>------------------------------------------------------------------------------
        !>  Description: Update values in each time step
        !>------------------------------------------------------------------------------

        !Inputs
        integer :: na, ignd
        integer :: iday, iyear
        type(dates_model) :: ts
        type(info_out) :: ifo

        real, dimension(:), intent(in) :: pre, evap, rof, dstg, &
                                          rcan, sncan, &
                                          pndw, sno, wsno
        real, dimension(:, :), intent(in) :: tbar, lqws, frws

        !Inputs-Output
        type(out_flds) :: vr

        !Internals
        integer :: i, iy, im, iss
        character*50 :: vId

        call GetIndicesDATES(iday, iyear, iy, im, iss, ts)

        do i = 1, ifo%nr_out

            vId = trim(adjustl(ifo%ids_var_out(i, 1)))

            select case (vId)

                case ('PREC', 'Rainfall', 'Rain', 'Precipitation')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%pre = vr%wb_y(iy)%pre + pre

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%pre = vr%wb_m(im)%pre + pre

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%pre = vr%wb_s(iss)%pre  + pre

                case ('EVAP', 'Evapotranspiration')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%evap = vr%wb_y(iy)%evap + evap

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%evap = vr%wb_m(im)%evap + evap

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%evap = vr%wb_s(iss)%evap + evap

                case ('Runoff', 'ROF')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%rof = vr%wb_y(iy)%rof  + rof

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%rof = vr%wb_m(im)%rof + rof

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%rof = vr%wb_s(iss)%rof + rof

                case ('DeltaStorage', 'DSTG')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%dstg = vr%wb_y(iy)%dstg + dstg

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%dstg =  vr%wb_m(im)%dstg + dstg

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%dstg = vr%wb_s(iss)%dstg + dstg

                case ('TempSoil', 'Temperature_soil_layers', 'TBAR')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%sp_y(iy)%tbar = vr%sp_y(iy)%tbar + tbar

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%sp_m(im)%tbar = vr%sp_m(im)%tbar + tbar

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%sp_s(iss)%tbar = vr%sp_s(iss)%tbar + tbar

                case ('LQWS')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%lqws = vr%wb_y(iy)%lqws + lqws

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%lqws = vr%wb_m(im)%lqws + lqws

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%lqws = vr%wb_s(iss)%lqws + lqws

                case ('FRWS')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%frws = vr%wb_y(iy)%frws + frws

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%frws = vr%wb_m(im)%frws + frws

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%frws = vr%wb_s(iss)%frws + frws

                case ('RCAN')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%rcan = vr%wb_y(iy)%rcan + rcan

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%rcan = vr%wb_m(im)%rcan + rcan

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%rcan = vr%wb_s(iss)%rcan + rcan

                case ('SCAN', 'SNCAN')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%sncan = vr%wb_y(iy)%sncan + sncan

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%sncan = vr%wb_m(im)%sncan + sncan

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%sncan = vr%wb_s(iss)%sncan + sncan

                case ('PNDW')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%pndw = vr%wb_y(iy)%pndw + pndw

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%pndw = vr%wb_m(im)%pndw + pndw

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%pndw = vr%wb_s(iss)%pndw + pndw

                case ('SNO')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%sno = vr%wb_y(iy)%sno + sno

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%sno = vr%wb_m(im)%sno + sno

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%sno = vr%wb_s(iss)%sno + sno

                case ('WSNO')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        vr%wb_y(iy)%wsno = vr%wb_y(iy)%wsno + wsno

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        vr%wb_m(im)%wsno = vr%wb_m(im)%wsno + wsno

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        vr%wb_s(iss)%wsno = vr%wb_s(iss)%wsno + wsno

!                case default
!                    print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."

            end select
        end do ! i = 1, ifo%nr_out

    end subroutine UpdateFIELDSOUT

    subroutine Write_Outputs(vr, ts, ifo, bi)

        !>------------------------------------------------------------------------------
        !>  Description: Loop over the variablaes to write
        !>  output balance's fields in selected format
        !>------------------------------------------------------------------------------

        !Inputs
        type(out_flds) :: vr
        type(info_out) :: ifo
        type(dates_model) :: ts
        type(basin_info) :: bi

        !Outputs
        !Files

        !Internals
        integer :: i, j
        character*50 :: vId
        character*1 :: st

        do i = 1, ifo%nr_out

            vId = trim(adjustl(ifo%ids_var_out(i, 1)))

            select case (vId)

                case ('PREC', 'Rainfall', 'Rain', 'Precipitation')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears)

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths)

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason)

                case ('EVAP', 'Evapotranspiration')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears)

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths)

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason)

                case ('Runoff', 'ROF')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears)

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths)

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason)

                case ('DeltaStorage', 'DSTG')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears)

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths)

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason)

                case ('TempSoil', 'Temperature_soil_layers', 'TBAR')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') then
                        do j = 1, bi%ignd
                            write(unit = st, fmt = '(I1)') j
                            call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears, st, j)
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') then
                        do j = 1, bi%ignd
                            write(unit = st, fmt = '(I1)') j
                            call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths, st, j)
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') then
                        do j = 1, bi%ignd
                            write(unit = st, fmt = '(I1)') j
                            call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason, st, j)
                        end do
                    end if

                case ('LQWS')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') then
                        do j = 1, bi%ignd
                            write(unit = st, fmt = '(I1)') j
                            call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears, st, j)
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') then
                        do j = 1, bi%ignd
                            write(unit = st, fmt = '(I1)') j
                            call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths, st, j)
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') then
                        do j = 1, bi%ignd
                            write(unit = st, fmt = '(I1)') j
                            call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason, st, j)
                        end do
                    end if

                case ('FRWS')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') then
                        do j = 1, bi%ignd
                            write(unit = st, fmt = '(I1)') j
                            call WriteFields_i(vr, vId, ts, ifo, i, 'Y',bi%na, ts%nyears, st, j)
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') then
                        do j = 1, bi%ignd
                            write(unit = st, fmt = '(I1)') j
                            call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths, st, j)
                        end do
                    end if

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') then
                        do j = 1, bi%ignd
                            write(unit = st, fmt = '(I1)') j
                            call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason, st, j)
                        end do
                    end if

                case ('RCAN')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears)

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths)

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason)

                case ('SCAN', 'SNCAN')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears)

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths)

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason)

                case ('PNDW')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears)

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths)

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason)

                case ('SNO')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears)

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths)

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason)

                case ('WSNO')

                    if (trim(adjustl(ifo%ids_var_out(i, 2))) == 'Y') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'Y', bi%na, ts%nyears)

                    if (trim(adjustl(ifo%ids_var_out(i, 3))) == 'M') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'M', bi%na, ts%nmonths)

                    if (trim(adjustl(ifo%ids_var_out(i, 4))) == 'S') &
                        call WriteFields_i(vr, vId, ts, ifo, i, 'S', bi%na, ts%nseason)

!                case default
!                    print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."

                end select
            end do ! i = 1, ifo%nr_out

    end subroutine Write_Outputs

    !>******************************************************************************

    subroutine WriteFields_i(vr, vars, ts, ifo, indx, freq, na, nt, st, igndx)

        !>------------------------------------------------------------------------------
        !>  Description: Loop over the variables to write
        !>  output balance's fields in selected format
        !>------------------------------------------------------------------------------

        !Inputs
        type(out_flds), intent(in) :: vr
        character*20, intent(in) :: vars
        type(dates_model), intent(in) :: ts
        type(info_out), intent(in) :: ifo
        integer, intent(in) :: indx
        character*1, intent(in) :: freq
        integer, intent(in) :: na, nt
        character*1, intent(in), optional :: st
        integer, intent(in), optional :: igndx

        !Internals
        integer :: i, nr
        character*50 :: vId, tfunc
        integer, dimension(:), allocatable :: days
        character*3 :: freq2
        real :: fld(na, nt)

        integer, dimension(:, :), allocatable :: dates

        vId = trim(adjustl(ifo%ids_var_out(indx, 6)))
        tfunc = trim(adjustl(ifo%ids_var_out(indx, 5)))

        select case (trim(adjustl(vars)))

            case ('PREC', 'Rainfall', 'Rain', 'Precipitation')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%pre
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%pre
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%pre
                    end do
                end if

            case ('EVAP', 'Evapotranspiration')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%evap
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%evap
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%evap
                    end do
                end if

            case ('Runoff', 'ROF')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%rof
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%rof
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%rof
                    end do
                end if

            case ('DeltaStorage', 'DSTG')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%dstg
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%dstg
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%dstg
                    end do
                end if

            case ('TempSoil', 'Temperature_soil_layers', 'TBAR')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%sp_y(i)%tbar(:, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%sp_m(i)%tbar(:, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%sp_s(i)%tbar(:, igndx)
                    end do
                end if

            case ('LQWS')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%lqws(:, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%lqws(:, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%lqws(:, igndx)
                    end do
                end if

            case ('FRWS')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%frws(:, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%frws(:, igndx)
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%frws(:, igndx)
                    end do
                end if

            case ('RCAN')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%rcan
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%rcan
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%rcan
                    end do
                end if

            case ('SCAN', 'SNCAN')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%sncan
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%sncan
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%sncan
                    end do
                end if

            case ('PNDW')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%pndw
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%pndw
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%pndw
                    end do
                end if

            case ('SNO')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%sno
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%sno
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%sno
                    end do
                end if

            case ('WSNO')

                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_y(i)%wsno
                    end do
                end if

                if (trim(adjustl(freq)) == 'M') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_m(i)%wsno
                    end do
                end if

                if (trim(adjustl(freq)) == 'S') then
                    do i = 1, nt
                        fld(:, i) = vr%wb_s(i)%wsno
                    end do
                end if

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

            case default
                print *, "Output of variable '" // trim(adjustl(vId)) // "' is not Implemented yet."

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
                allocate(dates(ts%NMONTHS, 2))
                dates = ts%mnthyears

            case ('S')
                allocate(dates(ts%nseason, 2))
                do i = 1, 12
                    dates(i, 1) = ts%years(1)
                    dates(i, 2) = i
                end do

        end select !freq

        if (present(st)) then
            freq2 = freq // '_' // st
        else
            freq2 = freq
        end if

        select case (vId)

            case('seq', 'binseq')
                call WriteSeq(fld, indx, ifo, freq2, dates)

            case('r2c')
                call WriteR2C(fld, indx, ifo, freq2, dates)

            case default
                print *, "Output as file format '" // trim(adjustl(vId)) // "' is not implemented yet."

        end select

    end subroutine WriteFields_i

    !>******************************************************************************

    !>******************************************************************************

    subroutine WriteSeq(fld, indx, info, freq, dates)

        !>------------------------------------------------------------------------------
        !>  Description: Write bin sequential file
        !>
        !>------------------------------------------------------------------------------

        !Inputs
        real :: fld(:, :)
        integer :: indx
        character*3 :: freq
        integer :: dates(:, :)
        type(info_out) :: info

        !Internal
        character*450 :: flOut
        integer :: ios, i
        integer :: na, nt

        flOut = trim(adjustl(info%pthOut)) // &
                trim(adjustl(info%ids_var_out(indx, 1))) // &
                '_' // trim(adjustl(freq)) // '.seq'

        open(unit = 882, &
             file = trim(adjustl(flOut)), &
             status = 'replace', &
             form = 'unformatted', &
             action = 'write', &
             access = 'sequential', &
             iostat = ios)

        nt = size(dates(:, 1))

        do i = 1, nt
            write(882) i
            write(882) fld(:, i)
        end do

        close(882)

    end subroutine WriteSeq

    !>******************************************************************************

    subroutine WriteR2C(fld, indx, info, freq, dates)

        !>------------------------------------------------------------------------------
        !>  Description: Write r2c file
        !>
        !>------------------------------------------------------------------------------

        use area_watflood

        !Inputs
        real :: fld(:, :)
        integer :: indx
        type(info_out) :: info
        character*3 :: freq
        integer :: dates(:, :)

        !Internal
        character*450 :: flOut
        integer :: ios, i, un
        integer :: na1, nt, j, t, k
        real, dimension(:, :), allocatable :: data_aux
        character(10) :: ctime
        character(8) :: cday

        flOut = trim(adjustl(info%pthOut)) // &
                trim(adjustl(info%ids_var_out(indx, 1))) // &
                '_' // trim(adjustl(freq)) // '.r2c'

        un = 882

        open(unit = un, &
             file = trim(adjustl(flOut)), &
             status = 'replace', &
             form = 'formatted', &
             action = 'write', &
             iostat = ios)

        write(un, 3005) '########################################'
        write(un, 3005) ':FileType r2c  ASCII  EnSim 1.0         '
        write(un, 3005) '#                                       '
        write(un, 3005) '# DataType               2D Rect Cell   '
        write(un, 3005) '#                                       '
        write(un, 3005) ':Application               MeshOutput   '
        write(un, 3005) ':Version                 1.0.00         '
        write(un, 3020) ':WrittenBy          ', 'MESH_DRIVER                             '

        call date_and_time(cday, ctime)

        write(un, 3010) ':CreationDate       ', &
            cday(1:4), cday(5:6), cday(7:8), ctime(1:2), ctime(3:4)

        write(un, 3005) '#                                       '
        write(un, 3005) '#---------------------------------------'
        write(un, 3005) '#                                       '
        write(un, 3020) ':Name               ', info%ids_var_out(indx, 1)
        write(un, 3005) '#                                       '
        write(un, 3004) ':Projection         ', coordsys1

        if (coordsys1 == 'LATLONG   ') &
            write(un, 3004) ':Ellipsoid          ', datum1

        if (coordsys1 == 'UTM       ') then
            write(un, 3004) ':Ellipsoid          ', datum1
            write(un, 3004) ':Zone               ', zone1
        end if

        write(un, 3005) '#                                       '
        write(un, 3003) ':xOrigin            ', xorigin
        write(un, 3003) ':yOrigin            ', yorigin
        write(un, 3005) '#                                       '
        write(un, 3005) ':SourceFile            MESH_DRIVER      '
        write(un, 3005) '#                                       '

        write(un, *) ':AttributeName', info%ids_var_out(indx, 1)

        write(un, 3020) ':AttributeUnits     ', info%ids_var_out(indx, 2)
        write(un, 3005) '#                                       '
        write(un, 3001) ':xCount             ', xCount
        write(un, 3001) ':yCount             ', ycount
        write(un, 3003) ':xDelta             ', xdelta
        write(un, 3003) ':yDelta             ', yDelta
        write(un, 3005) '#                                       '
        write(un, 3005) '#                                       '
        write(un, 3005) ':endHeader                              '

        nt = size(dates(:, 1))

        do t = 1, nt

            write(un, 9000) ':Frame', t, t, dates(t, 1), dates(t, 2), 1, 0, 0

            allocate(data_aux(ycount, xcount))
            data_aux = 0.0

            do k = 1, na
                data_aux(yyy(k), xxx(k)) = fld(k, t)
            end do

            do j = 1, ycount
                write(un, '(*(e12.6,2x))') (data_aux(j, i), i = 1, xcount)
            end do

            write(un, '(a)') ':EndFrame'

            deallocate(data_aux)

        end do

        close(882)

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

end module model_output
