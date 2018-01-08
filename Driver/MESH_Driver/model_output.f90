module model_output

    !>******************************************************************************
    !>  Athor: Gonzalo Sapriza Azuri
    !>******************************************************************************

    use sa_mesh_shared_variables
    use model_dates
    use model_files_variables

    implicit none

    real, allocatable :: TBAR_dly(:, :, :), ALD_dly(:, :), TMAX_ann(:, :, :), TMIN_ann(:, :, :), ALD_ann(:, :), ZOD_ann(:, :, :)
    real, allocatable :: ZOD_TTOL(:)

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
    end type

    !> Data type for storing meteorological data in time or space.
    !* (1: time or space)
    type met_data
        real, dimension(:), allocatable :: &
            fsdown, fsvh, fsih, fdl, ul, &
            ta, qa, pres, pre
    end type

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
            stg, dstg, grid_area
        real, dimension(:, :, :), allocatable :: lqws, frws
        real :: basin_area
    end type

    !> Data type to store components of the water balance in time or space.
    !* (1: time or space) or (1: time or space, 2: soil layer)
    type water_balance
        real, dimension(:), allocatable :: &
            pre, evap, pevp, evpb, arrd, rof, &
            rofo, rofs, rofb, &
            rcan, sncan, pndw, sno, wsno, &
            stg, dstg, grid_area
        real, dimension(:, :), allocatable :: lqws, frws
        real :: basin_area
    end type

    !> Data type to store components of the energy balance in time and space.
    type energy_balance_series
        real, dimension(:, :), allocatable :: hfs, qevp
        real,dimension(:,:,:),allocatable :: gflx
    end type

    !> Data type to store components of the energy balance in time or space.
    type energy_balance
        real, dimension(:), allocatable :: hfs, qevp
        real,dimension(:,:),allocatable :: gflx
    end type

    !> Data type to store soil parameters.
    !* tbar: Temperature of the soil layer (1: grid, 2: soil layer).
    !* thic: Fractional (frozen water) ice-content stored in the soil layer (1: grid, 2: soil layer).
    !* thlq: Fractional water-content stored in the soil layer (1: grid, 2: soil layer).
    type soil_statevars_series
        real, dimension(:, :,:), allocatable :: tbar, thic, thlq
    end type

    type soil_statevars
        real, dimension(:, :), allocatable :: tbar, thic, thlq
    end type

    type wr_output_series
        real, dimension(:, :), allocatable :: rof, rchg
    end type

    !> Data type to store the output format and data handling for an output variable.
    !* name: Name of the variable.
    !* nargs: Number of arguments in the argument array.
    !* args: Argument array containing flags for handling the output of the variable (1: Arguments)
    !* out_*: Output is written if .TRUE.; *: time interval (e.g., 'Y', 'M', 'S', etc.).
    !* out_fmt: Format of the output (e.g., 'r2c', 'seq', etc.).
    !* out_acc: Method of accumulation (e.g., if accumulated, averaged, etc., over the time period).
    type data_var_out

        !> Meta information.
        character(len = 20) name
        integer iun
        logical out_y, out_m, out_s, out_d, out_h
        character(len = 20) out_fmt, out_seq, out_acc
        logical :: opt_printdate = .false.

        !> Indices for specific output.
        integer, dimension(:), allocatable :: i_grds, k_tiles, m_grus

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

        type(water_balance_series) :: wbt_y, wbt_m, wbt_s, wbt_d, wbt_h
        type(water_balance) :: wd_ts

        type(energy_balance_series) :: engt_y, engt_m, engt_s, engt_d, engt_h
        type(energy_balance) :: eng_ts

        type(soil_statevars_series) :: spt_y, spt_m, spt_s, spt_d, spt_h
        type(soil_statevars) :: sp_ts

        !* mdt_h: Meteological data (hourly time-step).
        type(met_data_series) :: mdt_h
        type(met_data) :: md_ts

        type(wr_output_series) :: wroutt_h

    end type

    !>******************************************************************************
    !* flIn: File that contains the input information of the variables that we want to init and the frequency.
    !* pthOut: path out.
    !* ids_var_out: Array that contains the IDs of the files and frequency (e.g., 'PREPC', 'Y', 'M', 'S', 'cum', 'seq').
    !* nr_out: Number of output variables.
    type info_out
        character(len = 450) flIn, pthOut
        integer :: nr_out = 0
        type(data_var_out), dimension(:), allocatable :: var_out
    end type

    contains

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
        md%fsdown = 0.0; md%fsvh = 0.0; md%fsih = 0.0; md%fdl = 0.0; md%ul = 0.0
        md%ta = 0.0; md%qa = 0.0; md%pres = 0.0; md%pre = 0.0

    end subroutine

    subroutine data_var_out_allocate_args(vo, args)

        use strings

        !> Type variable.
        type(data_var_out) :: vo

        !> Input variables.
        character(len = 20), dimension(:), intent(in) :: args
        integer nargs

        !> Local variables.
        integer n, j, i, ierr
        character(len = 20) opts

        !> Set nargs to the size of the array.
        nargs = size(args)

        !> Reset variables.
        vo%out_y   = .false.
        vo%out_m   = .false.
        vo%out_s   = .false.
        vo%out_d   = .false.
        vo%out_h   = .false.
        vo%out_fmt = 'unknown'
        vo%out_seq = 'gridorder'
        vo%out_acc = 'unknown'

        !> Assign variables according to the args.
        do i = 1, nargs
            if (is_letter(args(i)))then
                opts = lowercase(args(i))
                select case (opts)

                    !> Yearly output.
                    case ('y')
                        vo%out_y = .true.

                    !> Monthly output.
                    case ('m')
                        vo%out_m = .true.

                    !> Seasonal output.
                    case ('s')
                        vo%out_s = .true.

                    !> Daily output.
                    case ('d')
                        vo%out_d = .true.

                    !> Hourly:
                    case ('h')
                        vo%out_h = .true.

                    !> Output format.
                    case ('r2c', 'seq', 'binseq', 'txt', 'csv')
                        vo%out_fmt = opts

                    !> Order of the selection being saved.
                    case ('gridorder', 'shedorder')
                        vo%out_seq = opts

                    !> Print date-stamp prior to the record.
                    case ('printdate')
                        vo%opt_printdate = .true.

                    !> Output format. Time series in grids
                    case('tsi')
                        vo%out_fmt = opts
                        n = 0
                        do j = i + 1, nargs
                            if (is_letter(args(j))) exit
                            n = n + 1
                        end do
                        if (n > 0) then
                            allocate(vo%i_grds(n))
                            do j = 1, n
                                call value(args(i + j), vo%i_grds(j), ierr)
                            end do
                        end if

                    !> Output format. Time series in tiles
                    case('tsk')
                        vo%out_fmt = opts
                        n = 0
                        do j = i + 1, nargs
                            if (is_letter(args(j))) exit
                            n = n + 1
                        end do
                        if (n > 0) then
                            allocate(vo%k_tiles(n))
                            do j = 1, n
                                call value(args(i + j), vo%k_tiles(j), ierr)
                            end do
                        end if

                    !> Output format. Time series of GRUs
                    case('gru')
                        vo%out_fmt = 'r2c'
                        n = 0
                        do j = i + 1, nargs
                            if (is_letter(args(j))) exit
                            n = n + 1
                        end do
                        if (n > 0) then
                            allocate(vo%m_grus(n))
                            do j = 1, n
                                call value(args(i + j), vo%m_grus(j), ierr)
                            end do
                        end if

                    !> Method of accumulation.
                    case ('cum', 'avg', 'max', 'min')
                        vo%out_acc = opts

                    !> TTOL for ZOD
                    case ('ttol')
                        n = 0
                        do j = i + 1, nargs
                            if (is_letter(args(j))) exit
                            n = n + 1
                        end do
                        if (n > 0) then
                            if (allocated(ZOD_TTOL)) deallocate(ZOD_TTOL)
                            allocate(ZOD_TTOL(n))
                            do j = 1, n
                                call value(args(i + j), ZOD_TTOL(j), ierr)
                            end do
                        end if

                    case default
                        print 1010, trim(args(i)), trim(vo%name)

                end select
            end if
        end do

1010    format('REMARK: ', (a), ' (Variable ', (a), ') is an unrecognized argument for output.')

    end subroutine

    subroutine init_out_flds(shd, ts, ifo, vr)

        !> Type variable.
        type(info_out) :: ifo
        type(out_flds) :: vr

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        type(dates_model), intent(in) :: ts

        !> Local variables.
        integer nts_y, nts_m, nts_s, nts_d, nts_h
        integer nmax, jmax, j, i

        !> Time-steps.
        nts_y = ts%nyears
        nts_m = ts%nmonths
        nts_s = ts%nseason
        nts_d = ts%nr_days
        nts_h = max(1, 3600/ic%dts)

        !> Elements.
        nmax = shd%NA
        jmax = shd%lc%IGND

        !> Allocate and initialize variables.
        do i = 1, ifo%nr_out
            select case (ifo%var_out(i)%name)

                case ('FSDOWN')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%mdt_h%fsdown)) allocate(vr%mdt_h%fsdown(nts_h, nmax))
                        vr%mdt_h%fsdown = 0.0
                        ifo%var_out(i)%iun = 882101
                    end if

                case ('FSVH')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%mdt_h%fsvh)) allocate(vr%mdt_h%fsvh(nts_h, nmax))
                        vr%mdt_h%fsvh = 0.0
                        ifo%var_out(i)%iun = 882102
                    end if

                case ('FSIH')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%mdt_h%fsih)) allocate(vr%mdt_h%fsih(nts_h, nmax))
                        vr%mdt_h%fsih = 0.0
                        ifo%var_out(i)%iun = 882103
                    end if

                case ('FDL')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%mdt_h%fdl)) allocate(vr%mdt_h%fdl(nts_h, nmax))
                        vr%mdt_h%fdl = 0.0
                        ifo%var_out(i)%iun = 882104
                    end if

                case ('UL')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%mdt_h%ul)) allocate(vr%mdt_h%ul(nts_h, nmax))
                        vr%mdt_h%ul = 0.0
                        ifo%var_out(i)%iun = 882105
                    end if

                case ('TA')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%mdt_h%ta)) allocate(vr%mdt_h%ta(nts_h, nmax))
                        vr%mdt_h%ta = 0.0
                        ifo%var_out(i)%iun = 882106
                    end if

                case ('QA')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%mdt_h%qa)) allocate(vr%mdt_h%qa(nts_h, nmax))
                        vr%mdt_h%qa = 0.0
                        ifo%var_out(i)%iun = 882107
                    end if

                case ('PRES')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%mdt_h%pres)) allocate(vr%mdt_h%pres(nts_h, nmax))
                        vr%mdt_h%pres = 0.0
                        ifo%var_out(i)%iun = 882108
                    end if

                case ('PRE')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%mdt_h%pre)) allocate(vr%mdt_h%pre(nts_h, nmax))
                        vr%mdt_h%pre = 0.0
                        ifo%var_out(i)%iun = 882109
                    end if

                case ('WR_RUNOFF')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wroutt_h%rof)) allocate(vr%wroutt_h%rof(nts_h, nmax))
                        vr%wroutt_h%rof = 0.0
                        ifo%var_out(i)%iun = 882120
                    end if

                case ('WR_RECHARGE')
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wroutt_h%rchg)) allocate(vr%wroutt_h%rchg(nts_h, nmax))
                        vr%wroutt_h%rchg = 0.0
                        ifo%var_out(i)%iun = 882121
                    end if

                case ('PREC', 'Rainfall', 'Rain', 'Precipitation')
                    ifo%var_out(i)%name = 'PREC'
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%pre)) allocate(vr%wbt_y%pre(nts_y, nmax))
                        vr%wbt_y%pre = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%pre)) allocate(vr%wbt_m%pre(nts_m, nmax))
                        vr%wbt_m%pre = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%pre)) allocate(vr%wbt_s%pre(nts_s, nmax))
                        vr%wbt_s%pre = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%wbt_d%pre)) allocate(vr%wbt_d%pre(nts_d, nmax))
                        vr%wbt_d%pre = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%pre)) allocate(vr%wbt_h%pre(nts_h, nmax))
                        vr%wbt_h%pre = 0.0
                        ifo%var_out(i)%iun = 882122
                    end if

                case ('EVAP', 'Evapotranspiration')
                    ifo%var_out(i)%name = 'EVAP'
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%evap)) allocate(vr%wbt_y%evap(nts_y, nmax))
                        vr%wbt_y%evap = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%evap)) allocate(vr%wbt_m%evap(nts_m, nmax))
                        vr%wbt_m%evap = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%evap)) allocate(vr%wbt_s%evap(nts_s, nmax))
                        vr%wbt_s%evap = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%wbt_d%evap)) allocate(vr%wbt_d%evap(nts_d, nmax))
                        vr%wbt_d%evap = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%evap)) allocate(vr%wbt_h%evap(nts_h, nmax))
                        vr%wbt_h%evap = 0.0
                        ifo%var_out(i)%iun = 882110
                    end if

                case ('Runoff', 'ROF')
                    ifo%var_out(i)%name = 'ROF'
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%rof)) allocate(vr%wbt_y%rof(nts_y, nmax))
                        vr%wbt_y%rof = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%rof)) allocate(vr%wbt_m%rof(nts_m, nmax))
                        vr%wbt_m%rof = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%rof)) allocate(vr%wbt_s%rof(nts_s, nmax))
                        vr%wbt_s%rof = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%wbt_d%rof)) allocate(vr%wbt_d%rof(nts_d, nmax))
                        vr%wbt_d%rof = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%rof)) allocate(vr%wbt_h%rof(nts_h, nmax))
                        vr%wbt_h%rof = 0.0
                        ifo%var_out(i)%iun = 882111
                    end if

                case ('TempSoil', 'Temperature_soil_layers', 'TBAR')
                    ifo%var_out(i)%name = 'TBAR'
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%spt_y%tbar)) allocate(vr%spt_y%tbar(nts_y, nmax, jmax))
                        vr%spt_y%tbar = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%spt_m%tbar)) allocate(vr%spt_m%tbar(nts_m, nmax, jmax))
                        vr%spt_m%tbar = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%spt_s%tbar)) allocate(vr%spt_s%tbar(nts_s, nmax, jmax))
                        vr%spt_s%tbar = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%spt_d%tbar)) allocate(vr%spt_d%tbar(nts_d, nmax, jmax))
                        vr%spt_d%tbar = 0.0
                    end if

                case ('LQWS')
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%lqws)) allocate(vr%wbt_y%lqws(nts_y, nmax, jmax))
                        vr%wbt_y%lqws = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%lqws)) allocate(vr%wbt_m%lqws(nts_m, nmax, jmax))
                        vr%wbt_m%lqws = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%lqws)) allocate(vr%wbt_s%lqws(nts_s, nmax, jmax))
                        vr%wbt_s%lqws = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%wbt_d%lqws)) allocate(vr%wbt_d%lqws(nts_d, nmax, jmax))
                        vr%wbt_d%lqws = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%lqws)) allocate(vr%wbt_h%lqws(nts_h, nmax, jmax))
                        vr%wbt_h%lqws = 0.0
                        ifo%var_out(i)%iun = 882112
                    end if

                case ('FRWS')
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%frws)) allocate(vr%wbt_y%frws(nts_y, nmax, jmax))
                        vr%wbt_y%frws = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%frws)) allocate(vr%wbt_m%frws(nts_m, nmax, jmax))
                        vr%wbt_m%frws = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%frws)) allocate(vr%wbt_s%frws(nts_s, nmax, jmax))
                        vr%wbt_s%frws = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%wbt_d%frws)) allocate(vr%wbt_d%frws(nts_d, nmax, jmax))
                        vr%wbt_d%frws = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%frws)) allocate(vr%wbt_h%frws(nts_h, nmax, jmax))
                        vr%wbt_h%frws = 0.0
                        ifo%var_out(i)%iun = 882113
                    end if

                case ('RCAN')
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%rcan)) allocate(vr%wbt_y%rcan(nts_y, nmax))
                        vr%wbt_y%rcan = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%rcan)) allocate(vr%wbt_m%rcan(nts_m, nmax))
                        vr%wbt_m%rcan = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%rcan)) allocate(vr%wbt_s%rcan(nts_s, nmax))
                        vr%wbt_s%rcan = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%rcan)) allocate(vr%wbt_h%rcan(nts_h, nmax))
                        vr%wbt_h%rcan = 0.0
                        ifo%var_out(i)%iun = 882114
                    end if

                case ('SCAN', 'SNCAN')
                    ifo%var_out(i)%name = 'SNCAN'
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%sncan)) allocate(vr%wbt_y%sncan(nts_y, nmax))
                        vr%wbt_y%sncan = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%sncan)) allocate(vr%wbt_m%sncan(nts_m, nmax))
                        vr%wbt_m%sncan = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%sncan)) allocate(vr%wbt_s%sncan(nts_s, nmax))
                        vr%wbt_s%sncan = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%sncan)) allocate(vr%wbt_h%sncan(nts_h, nmax))
                        vr%wbt_h%sncan = 0.0
                        ifo%var_out(i)%iun = 882115
                    end if

                case ('PNDW')
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%pndw)) allocate(vr%wbt_y%pndw(nts_y, nmax))
                        vr%wbt_y%pndw = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%pndw)) allocate(vr%wbt_m%pndw(nts_m, nmax))
                        vr%wbt_m%pndw = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%pndw)) allocate(vr%wbt_s%pndw(nts_s, nmax))
                        vr%wbt_s%pndw = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%pndw)) allocate(vr%wbt_h%pndw(nts_h, nmax))
                        vr%wbt_h%pndw = 0.0
                        ifo%var_out(i)%iun = 882116
                    end if

                case ('SNO')
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%sno)) allocate(vr%wbt_y%sno(nts_y, nmax))
                        vr%wbt_y%sno = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%sno)) allocate(vr%wbt_m%sno(nts_m, nmax))
                        vr%wbt_m%sno = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%sno)) allocate(vr%wbt_s%sno(nts_s, nmax))
                        vr%wbt_s%sno = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%sno)) allocate(vr%wbt_h%sno(nts_h, nmax))
                        vr%wbt_h%sno = 0.0
                        ifo%var_out(i)%iun = 882117
                    end if

                case ('WSNO')
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%wsno)) allocate(vr%wbt_y%wsno(nts_y, nmax))
                        vr%wbt_y%wsno = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%wsno)) allocate(vr%wbt_m%wsno(nts_m, nmax))
                        vr%wbt_m%wsno = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%wsno)) allocate(vr%wbt_s%wsno(nts_s, nmax))
                        vr%wbt_s%wsno = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%wsno)) allocate(vr%wbt_h%wsno(nts_h, nmax))
                        vr%wbt_h%wsno = 0.0
                        ifo%var_out(i)%iun = 882118
                    end if

                case ('STG', 'DSTG')
                    ifo%var_out(i)%name = 'STG'
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%wbt_y%dstg)) allocate(vr%wbt_y%dstg(nts_y, nmax))
                        if (.not. allocated(vr%wbt_y%stg)) allocate(vr%wbt_y%stg(nts_y, nmax))
                        vr%wbt_y%dstg = 0.0; vr%wbt_y%stg = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%wbt_m%dstg)) allocate(vr%wbt_m%dstg(nts_m, nmax))
                        if (.not. allocated(vr%wbt_m%stg)) allocate(vr%wbt_m%stg(nts_m, nmax))
                        vr%wbt_m%dstg = 0.0; vr%wbt_m%stg = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%wbt_s%dstg)) allocate(vr%wbt_s%dstg(nts_s, nmax))
                        if (.not. allocated(vr%wbt_s%stg)) allocate(vr%wbt_s%stg(nts_s, nmax))
                        vr%wbt_s%dstg = 0.0; vr%wbt_s%stg = 0.0
                    end if
                    if (ifo%var_out(i)%out_h) then
                        if (.not. allocated(vr%wbt_h%dstg)) allocate(vr%wbt_h%dstg(nts_h, nmax))
                        if (.not. allocated(vr%wbt_h%stg)) allocate(vr%wbt_h%stg(nts_h, nmax))
                        vr%wbt_h%dstg = 0.0; vr%wbt_h%stg = 0.0
                        ifo%var_out(i)%iun = 882119
                    end if

                case ('GFLX', 'HeatConduction')
                    ifo%var_out(i)%name = 'GFLX'
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%engt_y%gflx)) allocate(vr%engt_y%gflx(nts_y, nmax, jmax))
                        vr%engt_y%gflx = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%engt_m%gflx)) allocate(vr%engt_m%gflx(nts_m, nmax, jmax))
                        vr%engt_m%gflx = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%engt_s%gflx)) allocate(vr%engt_s%gflx(nts_s, nmax, jmax))
                        vr%engt_s%gflx = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%engt_d%gflx)) allocate(vr%engt_d%gflx(nts_d, nmax, jmax))
                        vr%engt_d%gflx = 0.0
                    end if

               case ('HFS', 'SensibleHeat')
                    ifo%var_out(i)%name = 'HFS'
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%engt_y%hfs)) allocate(vr%engt_y%hfs(nts_y, nmax))
                        vr%engt_y%hfs = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%engt_m%hfs)) allocate(vr%engt_m%hfs(nts_m, nmax))
                        vr%engt_m%hfs = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%engt_s%hfs)) allocate(vr%engt_s%hfs(nts_s, nmax))
                        vr%engt_s%hfs = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%engt_d%hfs)) allocate(vr%engt_d%hfs(nts_d, nmax))
                        vr%engt_d%hfs = 0.0
                    end if

                case ('QEVP', 'LatentHeat')
                    ifo%var_out(i)%name = 'QEVP'
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%engt_y%qevp)) allocate(vr%engt_y%qevp(nts_y, nmax))
                        vr%engt_y%qevp = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%engt_m%qevp)) allocate(vr%engt_m%qevp(nts_m, nmax))
                        vr%engt_m%qevp = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%engt_s%qevp)) allocate(vr%engt_s%qevp(nts_s, nmax))
                        vr%engt_s%qevp = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%engt_d%qevp)) allocate(vr%engt_d%qevp(nts_d, nmax))
                        vr%engt_d%qevp = 0.0
                    end if

                case ('THLQ')
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%spt_y%thlq)) allocate(vr%spt_y%thlq(nts_y, nmax, jmax))
                        vr%spt_y%thlq = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%spt_m%thlq)) allocate(vr%spt_m%thlq(nts_m, nmax, jmax))
                        vr%spt_m%thlq = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%spt_s%thlq)) allocate(vr%spt_s%thlq(nts_s, nmax, jmax))
                        vr%spt_s%thlq = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%spt_d%thlq)) allocate(vr%spt_d%thlq(nts_d, nmax, jmax))
                        vr%spt_d%thlq = 0.0
                    end if

                case ('THIC')
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(vr%spt_y%thic)) allocate(vr%spt_y%thic(nts_y, nmax, jmax))
                        vr%spt_y%thic = 0.0
                    end if
                    if (ifo%var_out(i)%out_m) then
                        if (.not. allocated(vr%spt_m%thic)) allocate(vr%spt_m%thic(nts_m, nmax, jmax))
                        vr%spt_m%thic = 0.0
                    end if
                    if (ifo%var_out(i)%out_s) then
                        if (.not. allocated(vr%spt_s%thic)) allocate(vr%spt_s%thic(nts_s, nmax, jmax))
                        vr%spt_s%thic = 0.0
                    end if
                    if (ifo%var_out(i)%out_d) then
                        if (.not. allocated(vr%spt_d%thic)) allocate(vr%spt_d%thic(nts_d, nmax, jmax))
                        vr%spt_d%thic = 0.0
                    end if

                case ('ALD', 'ZOD', 'TMAX', 'TMIN')
                    if (ifo%var_out(i)%out_y) then
                        if (.not. allocated(TBAR_dly)) allocate(TBAR_dly(nts_d, nmax, jmax))
                        if (.not. allocated(ALD_dly)) allocate(ALD_dly(nts_d, nmax))
                        if (.not. allocated(TMAX_ann)) allocate(TMAX_ann(nts_y, nmax, jmax))
                        if (.not. allocated(TMIN_ann)) allocate(TMIN_ann(nts_y, nmax, jmax))
                        if (.not. allocated(ALD_ann)) allocate(ALD_ann(nts_y, nmax))
                        if (.not. allocated(ZOD_TTOL)) then
                            allocate(ZOD_TTOL(1)); ZOD_TTOL = 0.0
                        end if
                        if (.not. allocated(ZOD_ann)) allocate(ZOD_ann(nts_y, nmax, size(ZOD_TTOL)))
                        TBAR_dly = 0.0; ALD_dly = -1.0; TMAX_ann = 0.0; TMIN_ann = 1000.0; ALD_ann = -1.0; ZOD_ann = -1.0
                    end if
                    ifo%var_out(i)%out_acc = 'unknown'

            end select
        end do

    end subroutine

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
            wb%stg(shd%NA), wb%dstg(shd%NA), wb%grid_area(shd%NA), &
            wb%lqws(shd%NA, shd%lc%IGND), wb%frws(shd%NA, shd%lc%IGND))

        !> Explicitly set all variables to 0.0.
        wb%pre = 0.0; wb%evap = 0.0; wb%pevp = 0.0; wb%evpb = 0.0; wb%arrd = 0.0
        wb%rof = 0.0; wb%rofo = 0.0; wb%rofs = 0.0; wb%rofb = 0.0
        wb%rcan = 0.0; wb%sncan = 0.0
        wb%pndw = 0.0; wb%sno = 0.0; wb%wsno = 0.0
        wb%stg = 0.0; wb%dstg = 0.0; wb%grid_area = 0.0
        wb%lqws = 0.0; wb%frws = 0.0
        wb%basin_area = 0.0

    end subroutine

    subroutine init_energy_balance(eb, shd)

        !> Type variable.
        type(energy_balance) :: eb

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Allocate arrays using basin info.
        allocate(eb%hfs(shd%NA), eb%qevp(shd%NA), eb%gflx(shd%NA, shd%lc%IGND))

        !> Explicitly set all variables to 0.0.
        eb%hfs = 0.0; eb%qevp = 0.0; eb%gflx = 0.0

    end subroutine

    subroutine init_soil_statevars(sv, shd)

        !> Type variable.
        type(soil_statevars) :: sv

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd

        !> Allocate arrays using basin info.
        allocate(sv%tbar(shd%NA, shd%lc%IGND), sv%thic(shd%NA, shd%lc%IGND), sv%thlq(shd%NA, shd%lc%IGND))

        !> Explicitly set all variables to 0.0.
        sv%tbar = 0.0; sv%thic = 0.0; sv%thlq = 0.0

    end subroutine

    subroutine init_wr_output_series(wroutt, shd, nts)

        !> Type variable.
        type(wr_output_series) :: wroutt

        !> Input variables.
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: nts

        !> Allocate arrays using basin info.
        allocate(wroutt%rof(nts, shd%NA), wroutt%rchg(nts, shd%NA))

        !> Explicitly set all variables to zero.
        wroutt%rof = 0.0; wroutt%rchg = 0.0

    end subroutine

    subroutine init_out(shd, ts, ifo, vr)

        !>------------------------------------------------------------------------------
        !>  Description: Read Output balance file
        !>------------------------------------------------------------------------------

        use strings

        !Inputs
        type(ShedGridParams) :: shd

        !Inputs-Output
        type(dates_model) :: ts
        type(info_out) :: ifo
        type(out_flds) :: vr

        !Internals
        integer :: ios, i, j, k, istat, nargs

        character(len = 20) vId
        character(len = 850) line

        character(len = 20) str
        character delims
        integer, parameter :: StrMax = 20, Nmax = 100
        character(len = StrMax), dimension(Nmax) :: argsLine

        open(unit = 909, file = 'outputs_balance.txt', status = 'old', action = 'read', iostat = ios)

        ifo%flIn = 'outputs_balance.txt'
        delims = ' '
        read(909, *) ifo%pthOut
        read(909, *) ifo%nr_out

        allocate(ifo%var_out(ifo%nr_out), stat = istat)
        if (istat /= 0) print *, 'Error allocating output variable array from file.'

        do i = 1, ifo%nr_out

            !> Read configuration information from file.
            call readline(909, line, ios)
            if (index(line, '#') > 2) line = line(1:index(line, '#') - 1)
            if (index(line, '!') > 2) line = line(1:index(line, '!') - 1)
            call compact(line)

            call parse(line,delims,argsLine,nargs)
            ifo%var_out(i)%name = argsLine(1)

            call data_var_out_allocate_args(ifo%var_out(i), argsLine(2:nargs))

        end do

        close(unit = 909)

        !> Initialize variable.
        call init_out_flds(shd, ts, ifo, vr)

    end subroutine

    subroutine check_write_var_out(shd, ifo, fldId, fld_in, file_unit, keep_file_open, igndx)

        !> Input variables.
        type(info_out), intent(in) :: ifo
        character(len = *), intent(in) :: fldId
        real, dimension(:, :) :: fld_in
        type(ShedGridParams), intent(in) :: shd
        integer, intent(in) :: file_unit
        logical :: keep_file_open
        integer, intent(in), optional :: igndx

        !> Local variables.
        real, dimension(:, :), allocatable :: fld_out
        integer, dimension(:, :), allocatable :: dates
        character(len = 10) freq, st
        integer frame_no, i

        !> Loop through the output variables.
        do i = 1, ifo%nr_out

            !> Cycle fields.
            if (.not. ifo%var_out(i)%name == fldId) cycle

            !> Hourly.
            if (ifo%var_out(i)%out_h .and. (mod(ic%ts_hourly, 3600/ic%dts) == 0)) then

                !> Ready output.
                allocate(fld_out(size(fld_in, 2), 1))
                select case (ifo%var_out(i)%out_acc)

                    case ('avg')
                        fld_out(:, 1) = sum(fld_in, 1) / size(fld_in, 1)

                    case ('max')
                        fld_out(:, 1) = maxval(fld_in, 1)

                    case ('min')
                        fld_out(:, 1) = minval(fld_in, 1)

                    case default
                        fld_out(:, 1) = sum(fld_in, 1)

                end select
                frame_no = ic%count_hour + 1
                freq = 'H'

                !> Reset array.
                fld_in = 0.0

                !> Allocate and set dates variable to current time-step.
                allocate(dates(1, 5))
                dates(1, 1) = ic%now%year
                dates(1, 2) = ic%now%month
                dates(1, 3) = ic%now%day
                dates(1, 4) = ic%now%jday
!todo: Flag to write 0-23 or 1-24.
                dates(1, 5) = ic%now%hour + 1

            end if

            !> fld will have been allocated if a supported frequency was selected.
            if (allocated(fld_out)) then

                !> Update freq to include soil layer (if applicable).
                if (present(igndx)) then
                    write(st, '(i10)') igndx
                    freq = trim(adjustl(freq)) // '_' // trim(adjustl(st))
                else
                    freq = freq
                end if

                !> Print the output.
                select case (ifo%var_out(i)%out_fmt)

                    case ('r2c')
                        call WriteR2C(fld_out, i, ifo, shd, freq, dates, file_unit, keep_file_open, frame_no)

                    case ('txt')
                        call WriteTxt(fld_out, i, ifo, shd, freq, dates, file_unit, keep_file_open, frame_no)

                    case ('csv')
                        call WriteCSV(fld_out, i, ifo, shd, freq, dates, file_unit, keep_file_open, frame_no)

                end select

                !> De-allocate the temporary fld and dates variables.
                deallocate(fld_out, dates)

            end if

        end do

    end subroutine

    subroutine UpdateFIELDSOUT(fls, shd, ts, cm, ifo, vr)

        use model_files_variables
        use climate_forcing

        use permafrost_active_layer

        !>------------------------------------------------------------------------------
        !>  Description: Update values in each time step
        !>------------------------------------------------------------------------------

        !Inputs
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(info_out) :: ifo

        !Inputs-Output
        type(out_flds) :: vr

        !Internals
        integer iy, im, iss, id, j, i

        call GetIndicesDATES(ic%now%jday, ic%now%year, iy, im, iss, id, ts)

        !> FSDOWN.
        if (allocated(vr%mdt_h%fsdown)) then
            vr%mdt_h%fsdown(ic%ts_hourly, :) = out%grid%ts%fsin
            call check_write_var_out(shd, ifo, 'FSDOWN', vr%mdt_h%fsdown, 882101, .true.)
        end if

        !> FSVH.
        if (allocated(vr%mdt_h%fsvh)) then
            vr%mdt_h%fsvh(ic%ts_hourly, :) = out%grid%ts%fsin/2.0
            call check_write_var_out(shd, ifo, 'FSVH', vr%mdt_h%fsvh, 882102, .true.)
        end if

        !> FSIH.
        if (allocated(vr%mdt_h%fsih)) then
            vr%mdt_h%fsih(ic%ts_hourly, :) = out%grid%ts%fsin/2.0
            call check_write_var_out(shd, ifo, 'FSIH', vr%mdt_h%fsih, 882103, .true.)
        end if

        !> FDL.
        if (allocated(vr%mdt_h%fdl)) then
            vr%mdt_h%fdl(ic%ts_hourly, :) = out%grid%ts%flin
            call check_write_var_out(shd, ifo, 'FDL', vr%mdt_h%fdl, 882104, .true.)
        end if

        !> UL.
        if (allocated(vr%mdt_h%ul)) then
            vr%mdt_h%ul(ic%ts_hourly, :) = out%grid%ts%uv
            call check_write_var_out(shd, ifo, 'UL', vr%mdt_h%ul, 882105, .true.)
        end if

        !> TA.
        if (allocated(vr%mdt_h%ta)) then
            vr%mdt_h%ta(ic%ts_hourly, :) = out%grid%ts%ta
            call check_write_var_out(shd, ifo, 'TA', vr%mdt_h%ta, 882106, .true.)
        end if

        !> QA.
        if (allocated(vr%mdt_h%qa)) then
            vr%mdt_h%qa(ic%ts_hourly, :) = out%grid%ts%qa
            call check_write_var_out(shd, ifo, 'QA', vr%mdt_h%qa, 882107, .true.)
        end if

        !> PRES.
        if (allocated(vr%mdt_h%pres)) then
            vr%mdt_h%pres(ic%ts_hourly, :) = out%grid%ts%pres
            call check_write_var_out(shd, ifo, 'PRES', vr%mdt_h%pres, 882108, .true.)
        end if

        !> PRE.
        if (allocated(vr%mdt_h%pre)) then
            vr%mdt_h%pre(ic%ts_hourly, :) = out%grid%ts%pre
            call check_write_var_out(shd, ifo, 'PRE', vr%mdt_h%pre, 882109, .true.)
        end if

        !> PREC; Rainfall; Rain; Precipitation.
!todo: Better way of storing variables in different formats (e.g., PRE [mm s-1] vs PREC [mm]).
        if (allocated(vr%wbt_y%pre)) vr%wbt_y%pre(iy, :) = vr%wbt_y%pre(iy, :) + out%grid%ts%pre*shd%FRAC*ic%dts
        if (allocated(vr%wbt_m%pre)) vr%wbt_m%pre(im, :) = vr%wbt_m%pre(im, :) + out%grid%ts%pre*shd%FRAC*ic%dts
        if (allocated(vr%wbt_s%pre)) vr%wbt_s%pre(iss, :) = vr%wbt_s%pre(iss, :) + out%grid%ts%pre*shd%FRAC*ic%dts
        if (allocated(vr%wbt_d%pre)) vr%wbt_d%pre(id, :) = vr%wbt_d%pre(id, :) + out%grid%ts%pre*shd%FRAC*ic%dts
        if (allocated(vr%wbt_h%pre)) then
            vr%wbt_h%pre(ic%ts_hourly, :) = out%grid%ts%pre*shd%FRAC*ic%dts
            call check_write_var_out(shd, ifo, 'PREC', vr%wbt_h%pre, 882122, .true.)
        end if

        !> EVAP; Evapotranspiration.
        if (allocated(vr%wbt_y%evap)) vr%wbt_y%evap(iy, :) = vr%wbt_y%evap(iy, :) + out%grid%ts%evap*shd%FRAC*ic%dts
        if (allocated(vr%wbt_m%evap)) vr%wbt_m%evap(im, :) = vr%wbt_m%evap(im, :) + out%grid%ts%evap*shd%FRAC*ic%dts
        if (allocated(vr%wbt_s%evap)) vr%wbt_s%evap(iss, :) = vr%wbt_s%evap(iss, :) + out%grid%ts%evap*shd%FRAC*ic%dts
        if (allocated(vr%wbt_d%evap)) vr%wbt_d%evap(id, :) = vr%wbt_d%evap(id, :) + out%grid%ts%evap*shd%FRAC*ic%dts
        if (allocated(vr%wbt_h%evap)) then
            vr%wbt_h%evap(ic%ts_hourly, :) = out%grid%ts%evap*shd%FRAC*ic%dts
            call check_write_var_out(shd, ifo, 'EVAP', vr%wbt_h%evap, 882110, .true.)
        end if

        !> Runoff; ROF.
        if (allocated(vr%wbt_y%rof)) vr%wbt_y%rof(iy, :) = vr%wbt_y%rof(iy, :) + out%grid%ts%rof*shd%FRAC*ic%dts
        if (allocated(vr%wbt_m%rof)) vr%wbt_m%rof(im, :) = vr%wbt_m%rof(im, :) + out%grid%ts%rof*shd%FRAC*ic%dts
        if (allocated(vr%wbt_s%rof)) vr%wbt_s%rof(iss, :) = vr%wbt_s%rof(iss, :) + out%grid%ts%rof*shd%FRAC*ic%dts
        if (allocated(vr%wbt_d%rof)) vr%wbt_d%rof(id, :) = vr%wbt_d%rof(id, :) + out%grid%ts%rof*shd%FRAC*ic%dts
        if (allocated(vr%wbt_h%rof)) then
            vr%wbt_h%rof(ic%ts_hourly, :) = out%grid%ts%rof*shd%FRAC*ic%dts
            call check_write_var_out(shd, ifo, 'ROF', vr%wbt_h%rof, 882111, .true.)
        end if

        !> RCAN.
        if (allocated(vr%wbt_y%rcan)) vr%wbt_y%rcan(iy, :) = vr%wbt_y%rcan(iy, :) + out%grid%ts%rcan*shd%FRAC
        if (allocated(vr%wbt_m%rcan)) vr%wbt_m%rcan(im, :) = vr%wbt_m%rcan(im, :) + out%grid%ts%rcan*shd%FRAC
        if (allocated(vr%wbt_s%rcan)) vr%wbt_s%rcan(iss, :) = vr%wbt_s%rcan(iss, :) + out%grid%ts%rcan*shd%FRAC
        if (allocated(vr%wbt_h%rcan)) then
            vr%wbt_h%rcan(ic%ts_hourly, :) = out%grid%ts%rcan*shd%FRAC
            call check_write_var_out(shd, ifo, 'RCAN', vr%wbt_h%rcan, 882114, .true.)
        end if

        !> SCAN; SNCAN.
        if (allocated(vr%wbt_y%sncan)) vr%wbt_y%sncan(iy, :) = vr%wbt_y%sncan(iy, :) + out%grid%ts%sncan*shd%FRAC
        if (allocated(vr%wbt_m%sncan)) vr%wbt_m%sncan(im, :) = vr%wbt_m%sncan(im, :) + out%grid%ts%sncan*shd%FRAC
        if (allocated(vr%wbt_s%sncan)) vr%wbt_s%sncan(iss, :) = vr%wbt_s%sncan(iss, :) + out%grid%ts%sncan*shd%FRAC
        if (allocated(vr%wbt_h%sncan)) then
            vr%wbt_h%sncan(ic%ts_hourly, :) = out%grid%ts%sncan*shd%FRAC
            call check_write_var_out(shd, ifo, 'SNCAN', vr%wbt_h%sncan, 882115, .true.)
        end if

        !> PNDW.
        if (allocated(vr%wbt_y%pndw)) vr%wbt_y%pndw(iy, :) = vr%wbt_y%pndw(iy, :) + out%grid%ts%pndw*shd%FRAC
        if (allocated(vr%wbt_m%pndw)) vr%wbt_m%pndw(im, :) = vr%wbt_m%pndw(im, :) + out%grid%ts%pndw*shd%FRAC
        if (allocated(vr%wbt_s%pndw)) vr%wbt_s%pndw(iss, :) = vr%wbt_s%pndw(iss, :) + out%grid%ts%pndw*shd%FRAC
        if (allocated(vr%wbt_h%pndw)) then
            vr%wbt_h%pndw(ic%ts_hourly, :) = out%grid%ts%pndw*shd%FRAC
            call check_write_var_out(shd, ifo, 'PNDW', vr%wbt_h%pndw, 882116, .true.)
        end if

        !> SNO.
        if (allocated(vr%wbt_y%sno)) vr%wbt_y%sno(iy, :) = vr%wbt_y%sno(iy, :) + out%grid%ts%sno*shd%FRAC
        if (allocated(vr%wbt_m%sno)) vr%wbt_m%sno(im, :) = vr%wbt_m%sno(im, :) + out%grid%ts%sno*shd%FRAC
        if (allocated(vr%wbt_s%sno)) vr%wbt_s%sno(iss, :) = vr%wbt_s%sno(iss, :) + out%grid%ts%sno*shd%FRAC
        if (allocated(vr%wbt_h%sno)) then
            vr%wbt_h%sno(ic%ts_hourly, :) = out%grid%ts%sno*shd%FRAC
            call check_write_var_out(shd, ifo, 'SNO', vr%wbt_h%sno, 882117, .true.)
        end if

        !> WSNO.
        if (allocated(vr%wbt_y%wsno)) vr%wbt_y%wsno(iy, :) = vr%wbt_y%wsno(iy, :) + out%grid%ts%wsno*shd%FRAC
        if (allocated(vr%wbt_m%wsno)) vr%wbt_m%wsno(im, :) = vr%wbt_m%wsno(im, :) + out%grid%ts%wsno*shd%FRAC
        if (allocated(vr%wbt_s%wsno)) vr%wbt_s%wsno(iss, :) = vr%wbt_s%wsno(iss, :) + out%grid%ts%wsno*shd%FRAC
        if (allocated(vr%wbt_h%wsno)) then
            vr%wbt_h%wsno(ic%ts_hourly, :) = out%grid%ts%wsno*shd%FRAC
            call check_write_var_out(shd, ifo, 'WSNO', vr%wbt_h%wsno, 882118, .true.)
        end if

        !> HFS; SensibleHeat.
        if (allocated(vr%engt_y%hfs)) vr%engt_y%hfs(iy, :) = vr%engt_y%hfs(iy, :) + out%grid%ts%qh*shd%FRAC
        if (allocated(vr%engt_m%hfs)) vr%engt_m%hfs(im, :) = vr%engt_m%hfs(im, :) + out%grid%ts%qh*shd%FRAC
        if (allocated(vr%engt_s%hfs)) vr%engt_s%hfs(iss, :) = vr%engt_s%hfs(iss, :) + out%grid%ts%qh*shd%FRAC
        if (allocated(vr%engt_d%hfs)) vr%engt_d%hfs(id, :) = vr%engt_d%hfs(id, :) + out%grid%ts%qh*shd%FRAC

        !> QEVP; LatentHeat.
        if (allocated(vr%engt_y%qevp)) vr%engt_y%qevp(iy, :) = vr%engt_y%qevp(iy, :) + out%grid%ts%qe*shd%FRAC
        if (allocated(vr%engt_m%qevp)) vr%engt_m%qevp(im, :) = vr%engt_m%qevp(im, :) + out%grid%ts%qe*shd%FRAC
        if (allocated(vr%engt_s%qevp)) vr%engt_s%qevp(iss, :) = vr%engt_s%qevp(iss, :) + out%grid%ts%qe*shd%FRAC
        if (allocated(vr%engt_d%qevp)) vr%engt_d%qevp(id, :) = vr%engt_d%qevp(id, :) + out%grid%ts%qe*shd%FRAC

        !> Variables with soil layers.
        do j = 1, shd%lc%IGND

            !> LQWS.
            if (allocated(vr%wbt_y%lqws)) vr%wbt_y%lqws(iy, :, j) = vr%wbt_y%lqws(iy, :, j) + out%grid%ts%lqws(:, j)*shd%FRAC
            if (allocated(vr%wbt_m%lqws)) vr%wbt_m%lqws(im, :, j) = vr%wbt_m%lqws(im, :, j) + out%grid%ts%lqws(:, j)*shd%FRAC
            if (allocated(vr%wbt_s%lqws)) vr%wbt_s%lqws(iss, :, j) = vr%wbt_s%lqws(iss, :, j) + out%grid%ts%lqws(:, j)*shd%FRAC
            if (allocated(vr%wbt_d%lqws)) vr%wbt_d%lqws(id, :, j) = vr%wbt_d%lqws(id, :, j) + out%grid%ts%lqws(:, j)*shd%FRAC
            if (allocated(vr%wbt_h%lqws)) then
                vr%wbt_h%lqws(ic%ts_hourly, :, j) = out%grid%ts%lqws(:, j)*shd%FRAC
                call check_write_var_out(shd, ifo, 'LQWS', vr%wbt_h%lqws(:, :, j), (882112 + (100000000*j)), .true., j)
            end if

            !> FRWS.
            if (allocated(vr%wbt_y%frws)) vr%wbt_y%frws(iy, :, j) = vr%wbt_y%frws(iy, :, j) + out%grid%ts%fzws(:, j)*shd%FRAC
            if (allocated(vr%wbt_m%frws)) vr%wbt_m%frws(im, :, j) = vr%wbt_m%frws(im, :, j) + out%grid%ts%fzws(:, j)*shd%FRAC
            if (allocated(vr%wbt_s%frws)) vr%wbt_s%frws(iss, :, j) = vr%wbt_s%frws(iss, :, j) + out%grid%ts%fzws(:, j)*shd%FRAC
            if (allocated(vr%wbt_d%frws)) vr%wbt_d%frws(id, :, j) = vr%wbt_d%frws(id, :, j) + out%grid%ts%fzws(:, j)*shd%FRAC
            if (allocated(vr%wbt_h%frws)) then
                vr%wbt_h%frws(ic%ts_hourly, :, j) = out%grid%ts%fzws(:, j)*shd%FRAC
                call check_write_var_out(shd, ifo, 'FRWS', vr%wbt_h%frws(:, :, j), (882113 + (100000000*j)), .true., j)
            end if

            !> GFLX; HeatConduction.
            if (allocated(vr%engt_y%gflx)) vr%engt_y%gflx(iy, :, j) = vr%engt_y%gflx(iy, :, j) + out%grid%ts%gflx(:, j)*shd%FRAC
            if (allocated(vr%engt_m%gflx)) vr%engt_m%gflx(im, :, j) = vr%engt_m%gflx(im, :, j) + out%grid%ts%gflx(:, j)*shd%FRAC
            if (allocated(vr%engt_s%gflx)) vr%engt_s%gflx(iss, :, j) = vr%engt_s%gflx(iss, :, j) + out%grid%ts%gflx(:, j)*shd%FRAC
            if (allocated(vr%engt_d%gflx)) vr%engt_d%gflx(id, :, j) = vr%engt_d%gflx(id, :, j) + out%grid%ts%gflx(:, j)*shd%FRAC

            !> THLQ.
            if (allocated(vr%spt_y%thlq)) vr%spt_y%thlq(iy, :, j) = vr%spt_y%thlq(iy, :, j) + out%grid%ts%thlq(:, j)*shd%FRAC
            if (allocated(vr%spt_m%thlq)) vr%spt_m%thlq(im, :, j) = vr%spt_m%thlq(im, :, j) + out%grid%ts%thlq(:, j)*shd%FRAC
            if (allocated(vr%spt_s%thlq)) vr%spt_s%thlq(iss, :, j) = vr%spt_s%thlq(iss, :, j) + out%grid%ts%thlq(:, j)*shd%FRAC
            if (allocated(vr%spt_d%thlq)) vr%spt_d%thlq(id, :, j) = vr%spt_d%thlq(id, :, j) + out%grid%ts%thlq(:, j)*shd%FRAC

            !> THIC.
            if (allocated(vr%spt_y%thic)) vr%spt_y%thic(iy, :, j) = vr%spt_y%thic(iy, :, j) + out%grid%ts%thic(:, j)*shd%FRAC
            if (allocated(vr%spt_m%thic)) vr%spt_m%thic(im, :, j) = vr%spt_m%thic(im, :, j) + out%grid%ts%thic(:, j)*shd%FRAC
            if (allocated(vr%spt_s%thic)) vr%spt_s%thic(iss, :, j) = vr%spt_s%thic(iss, :, j) + out%grid%ts%thic(:, j)*shd%FRAC
            if (allocated(vr%spt_d%thic)) vr%spt_d%thic(id, :, j) = vr%spt_d%thic(id, :, j) + out%grid%ts%thic(:, j)*shd%FRAC

        end do

        !> TempSoil; Temperature_soil_layers; TBAR.
        if (allocated(vr%spt_y%tbar)) vr%spt_y%tbar(iy, :, :) = vr%spt_y%tbar(iy, :, :) + out%grid%ts%tbar
        if (allocated(vr%spt_m%tbar)) vr%spt_m%tbar(im, :, :) = vr%spt_m%tbar(im, :, :) + out%grid%ts%tbar
        if (allocated(vr%spt_s%tbar)) vr%spt_s%tbar(iss, :, :) = vr%spt_s%tbar(iss, :, :) + out%grid%ts%tbar
        if (allocated(vr%spt_d%tbar)) vr%spt_d%tbar(id, :, :) = vr%spt_d%tbar(id, : , :) + out%grid%ts%tbar

        !> DSTG; STG.
        if (allocated(vr%wbt_y%dstg) .and. allocated(vr%wbt_y%stg)) then
            vr%wbt_y%dstg(iy, :) = &
                (out%grid%ts%rcan + out%grid%ts%sncan + out%grid%ts%sno + out%grid%ts%wsno + out%grid%ts%pndw + &
                 sum(out%grid%ts%lqws, 2) + sum(out%grid%ts%fzws, 2) + &
                 out%grid%ts%lzs + out%grid%ts%dzs)*shd%FRAC - vr%wbt_y%stg(iy, :)
            vr%wbt_y%stg(iy, :) = vr%wbt_y%dstg(iy, :) + vr%wbt_y%stg(iy, :)
        end if
        if (allocated(vr%wbt_m%dstg) .and. allocated(vr%wbt_m%stg)) then
            vr%wbt_m%dstg(im, :) = &
                (out%grid%ts%rcan + out%grid%ts%sncan + out%grid%ts%sno + out%grid%ts%wsno + out%grid%ts%pndw + &
                 sum(out%grid%ts%lqws, 2) + sum(out%grid%ts%fzws, 2) + &
                 out%grid%ts%lzs + out%grid%ts%dzs)*shd%FRAC - vr%wbt_m%stg(im, :)
            vr%wbt_m%stg(im, :) = vr%wbt_m%dstg(im, :) + vr%wbt_m%stg(im, :)
        end if
        if (allocated(vr%wbt_s%dstg) .and. allocated(vr%wbt_s%stg)) then
            vr%wbt_s%dstg(iss, :) = &
                (out%grid%ts%rcan + out%grid%ts%sncan + out%grid%ts%sno + out%grid%ts%wsno + out%grid%ts%pndw + &
                 sum(out%grid%ts%lqws, 2) + sum(out%grid%ts%fzws, 2) + &
                 out%grid%ts%lzs + out%grid%ts%dzs)*shd%FRAC - vr%wbt_s%stg(iss, :)
            vr%wbt_s%stg(iss, :) = vr%wbt_s%dstg(iss, :) + vr%wbt_s%stg(iss, :)
        end if
        if (allocated(vr%wbt_d%dstg) .and. allocated(vr%wbt_d%stg)) then
            vr%wbt_d%dstg(id, :) = &
                (out%grid%ts%rcan + out%grid%ts%sncan + out%grid%ts%sno + out%grid%ts%wsno + out%grid%ts%pndw + &
                 sum(out%grid%ts%lqws, 2) + sum(out%grid%ts%fzws, 2) + &
                 out%grid%ts%lzs + out%grid%ts%dzs)*shd%FRAC - vr%wbt_d%stg(id, :)
            vr%wbt_d%stg(id, :) = vr%wbt_d%dstg(id, :) + vr%wbt_d%stg(id, :)
        end if
        if (allocated(vr%wbt_h%stg)) then
            vr%wbt_h%stg(ic%ts_hourly, :) = &
                (out%grid%ts%rcan + out%grid%ts%sncan + out%grid%ts%sno + out%grid%ts%wsno + out%grid%ts%pndw + &
                 sum(out%grid%ts%lqws, 2) + sum(out%grid%ts%fzws, 2) + &
                 out%grid%ts%lzs + out%grid%ts%dzs)*shd%FRAC
            call check_write_var_out(shd, ifo, 'STG', vr%wbt_h%stg, 882119, .true.)
        end if

        if (allocated(TBAR_dly)) then

            !> Accumulate TBAR at every time-step.
            TBAR_dly(id, : , :) = TBAR_dly(id, : , :) + out%grid%ts%tbar

            !> Last time-step of the day.
            if (ic%ts_daily == 24*3600/ic%dts) then
                TBAR_dly(id, : , :) = TBAR_dly(id, : , :)/ic%ts_daily !daily average temperature
                call active_layer_depth(TBAR_dly(id, :, :), shd%lc%sl%ZBOT, ALD_dly(id, :), shd%lc%IGND, shd%NA, 1, shd%NA)
                ALD_ann(iy, :) = max(ALD_ann(iy, :), ALD_dly(id, :))
                TMAX_ann(iy, :, :) = max(TMAX_ann(iy, :, :), TBAR_dly(id, :, :))
                TMIN_ann(iy, :, :) = min(TMIN_ann(iy, :, :), TBAR_dly(id, :, :))
            end if
        end if

        !> WR_RUNOFF.
        if (allocated(vr%wroutt_h%rof)) then
            vr%wroutt_h%rof(ic%ts_hourly, :) = out%grid%ts%rff*ic%dts
            call check_write_var_out(shd, ifo, 'WR_RUNOFF', vr%wroutt_h%rof, 882120, .true.)
        end if

        !> WR_RECHARGE.
        if (allocated(vr%wroutt_h%rchg)) then
            vr%wroutt_h%rchg(ic%ts_hourly, :) = out%grid%ts%rchg*ic%dts
            call check_write_var_out(shd, ifo, 'WR_RECHARGE', vr%wroutt_h%rchg, 882121, .true.)
        end if

    end subroutine

    subroutine Write_Outputs(shd, fls, ts, ifo, vr)

        !>------------------------------------------------------------------------------
        !>  Description: Loop over the variablaes to write
        !>  output balance's fields in selected format
        !>------------------------------------------------------------------------------

        use permafrost_active_layer

        !Inputs
        type(ShedGridParams), intent(in) :: shd
        type(fl_ids), intent(in) :: fls
        type(dates_model), intent(in) :: ts
        type(info_out), intent(in) :: ifo
        type(out_flds), intent(in) :: vr

        !Internals
        integer k, j, i
        character(len = 20) vId

        do i = 1, ifo%nr_out

            vId = trim(adjustl(ifo%var_out(i)%name))

            select case (vId)

                case ('FSDOWN')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%mdt_h%fsdown, 882101, .false.)

                case ('FSVH')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%mdt_h%fsvh, 882102, .false.)

                case ('FSIH')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%mdt_h%fsih, 882103, .false.)

                case ('FDL')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%mdt_h%fdl, 882104, .false.)

                case ('UL')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%mdt_h%ul, 882105, .false.)

                case ('TA')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%mdt_h%ta, 882106, .false.)

                case ('QA')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%mdt_h%qa, 882107, .false.)

                case ('PRES')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%mdt_h%pres, 882108, .false.)

                case ('PRE')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%mdt_h%pre, 882109, .false.)

                case ('PREC', 'Rainfall', 'Rain', 'Precipitation')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_d) call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wbt_h%pre, 882122, .false.)

                case ('EVAP', 'Evapotranspiration')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_d) call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wbt_h%evap, 882110, .false.)

                case ('Runoff', 'ROF')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_d) call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wbt_h%rof, 882111, .false.)

                case ('DeltaStorage', 'DSTG')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_d) call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)

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
                            call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls, j)
                        end do
                    end if

                case ('ALD')
                    if (ifo%var_out(i)%out_y) then
                        call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    end if
                case ('ZOD')
                    if (ifo%var_out(i)%out_y) then
                        do j = 1, size(ZOD_TTOL)
                            do k = 1, ts%nyears
                                call zero_oscillation_depth( &
                                    TMAX_ann(k, :, :), TMIN_ann(k, :, :), shd%lc%sl%ZBOT, ZOD_TTOL(j), &
                                    ZOD_ann(k, :, j), &
                                    shd%lc%IGND, shd%NA, 1, shd%NA)
                            end do
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if
                case ('TMAX')
                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
                        end do
                    end if
                case ('TMIN')
                    if (ifo%var_out(i)%out_y) then
                        do j = 1, shd%lc%IGND
                            call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls, j)
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
                            call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls, j)
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
                            call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls, j)
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
                            call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls, j)
                        end do
                    end if

                case ('HFS','SensibleHeat')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_d) call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)

                case ('QEVP','LatentHeat')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_d) call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls)

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
                            call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls, j)
                        end do
                    end if
                    if (ifo%var_out(i)%out_h) then
                        do j = 1, shd%lc%IGND
                            call check_write_var_out(shd, ifo, vId, vr%wbt_h%lqws(:, :, j), (882112 + (100000000*j)), .false., j)
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
                            call WriteFields_i(vr, ts, ifo, i, 'D', shd, ts%nr_days, fls, j)
                        end do
                    end if
                    if (ifo%var_out(i)%out_h) then
                        do j = 1, shd%lc%IGND
                            call check_write_var_out(shd, ifo, vId, vr%wbt_h%frws(:, :, j), (882113 + (100000000*j)), .false., j)
                        end do
                    end if

                case ('RCAN')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wbt_h%rcan, 882114, .false.)

                case ('SCAN', 'SNCAN')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wbt_h%sncan, 882115, .false.)

                case ('PNDW')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wbt_h%pndw, 882116, .false.)

                case ('SNO')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wbt_h%sno, 882117, .false.)

                case ('WSNO')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wbt_h%wsno, 882118, .false.)

                case ('STG')
                    if (ifo%var_out(i)%out_y) call WriteFields_i(vr, ts, ifo, i, 'Y', shd, ts%nyears, fls)
                    if (ifo%var_out(i)%out_m) call WriteFields_i(vr, ts, ifo, i, 'M', shd, ts%nmonths, fls)
                    if (ifo%var_out(i)%out_s) call WriteFields_i(vr, ts, ifo, i, 'S', shd, ts%nseason, fls)
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wbt_h%stg, 882119, .false.)

                case ('WR_RUNOFF')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wroutt_h%rof, 882120, .false.)

                case ('WR_RECHARGE')
                    if (ifo%var_out(i)%out_h) call check_write_var_out(shd, ifo, vId, vr%wroutt_h%rchg, 882121, .false.)

                end select
            end do

    end subroutine

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
        character(len = *), intent(in) :: freq

        integer, intent(in), optional :: igndx

        !Internals
        integer i, nr
        character(len = 20) vId, tfunc
        integer, dimension(:), allocatable :: days
        character(len = 10) freq2, st
        real :: fld(shd%NA, nt)

        integer, dimension(:, :), allocatable :: dates

        vId = trim(adjustl(ifo%var_out(indx)%out_fmt))
        tfunc = trim(adjustl(ifo%var_out(indx)%out_acc))

        select case (ifo%var_out(indx)%name)

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

                if (trim(adjustl(freq)) == 'D') then
                    do i = 1, nt
                        fld(:, i) = vr%spt_d%tbar(i, :, igndx)
                    end do
                end if

            case ('ALD')
                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = ALD_ann(i, :)
                    end do
                end if
            case ('ZOD')
                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = ZOD_ann(i, :, igndx)
                    end do
                end if
            case ('TMAX')
                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = TMAX_ann(i, :, igndx)
                    end do
                end if
            case ('TMIN')
                if (trim(adjustl(freq)) == 'Y') then
                    do i = 1, nt
                        fld(:, i) = TMIN_ann(i, :, igndx)
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

                if (trim(adjustl(freq)) == 'D') then
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

                if (trim(adjustl(freq)) == 'D') then
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

                if (trim(adjustl(freq)) == 'D') then
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

                if (trim(adjustl(freq)) == 'D') then
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

                if (trim(adjustl(freq)) == 'D') then
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

            case ('STG')

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

            case default
                print *, "Output of variable '" // trim(adjustl(ifo%var_out(indx)%name)) // "' is not implemented yet."

        end select

        if (tfunc == 'avg') then

            allocate(days(nt))

            select case (freq)

                case ('Y')
                    days = (86400/ic%dts)*ts%daysINyears

                case ('M')
                    days = (86400/ic%dts)*ts%daysINmonths

                case ('S')
                    days = (86400/ic%dts)*ts%daysINseasons

                case default
                    days = (86400/ic%dts)

            end select

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

            case ('D')
                allocate(dates(ts%nr_days, 3))
                do i = 1, ts%nr_days
                    dates(i, 1) = ts%dates(i, 1)
                    dates(i, 2) = ts%dates(i, 2)
                    dates(i, 3) = ts%dates(i, 3)
                end do

        end select

        if (present(igndx)) then
            write(st, '(i10)') igndx
            freq2 = freq // '_' // trim(adjustl(st))
        else
            freq2 = freq
        end if
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

        if (allocated(dates)) deallocate(dates)

    end subroutine

    subroutine WriteTsi(fld, indx, info, freq, dates, fls)

        !Inputs
        real fld(:, :)
        integer indx
        character(len = *), intent(in) :: freq
        integer dates(:, :)
        type(info_out) :: info
        type(fl_ids) :: fls

        !Internal
        character(len = 450) flOut
        integer ios, i, j
        integer nt, unitfl

        if (len_trim(adjustl(fls%fl(mfk%out_response)%fn)) > 0) then
            flOut = trim(adjustl(fls%pthOut)) // &
                    trim(adjustl(info%var_out(indx)%name)) // &
                    '_' // trim(adjustl(freq)) // '_' // &
                    trim(adjustl(fls%fl(mfk%out_response)%fn)) // '.ts'
        else
            flOut = trim(adjustl(info%pthOut)) // &
                    trim(adjustl(info%var_out(indx)%name)) // &
                    '_' // trim(adjustl(freq)) // '.ts'
        end if

        unitfl = fls%fl(mfk%out_response)%iun
        open(unit = unitfl, file = flOut, status = 'replace', &
             form = 'formatted', action = 'write', &
             iostat = ios)

        nt = size(dates(:, 1))

        do i = 1, nt
            write(unitfl, *) (fld(info%var_out(indx)%i_grds(j), i), j = 1, size(info%var_out(indx)%i_grds))
        end do

        close(unitfl)

    end subroutine

    subroutine WriteSeq(fld, indx, info, freq, dates)

        !>------------------------------------------------------------------------------
        !>  Description: Write bin sequential file
        !>------------------------------------------------------------------------------

        !Inputs
        real fld(:, :)
        integer indx
        character(len = *), intent(in) :: freq
        integer dates(:, :)
        type(info_out) :: info

        !Internal
        character(len = 450) flOut
        integer ios, i
        integer nt

        flOut = trim(adjustl(info%pthOut)) // &
                trim(adjustl(info%var_out(indx)%name)) // &
                '_' // trim(adjustl(freq)) // '.seq'

        open(unit = 882, file = flOut, status = 'replace', &
             form = 'unformatted', action = 'write', access = 'sequential', &
             iostat = ios)

        nt = size(dates(:, 1))

        do i = 1, nt
            write(882) i
            write(882) fld(:, i)
        end do

        close(882)

    end subroutine

    subroutine WriteR2C(fld, indx, info, shd, freq, dates, file_unit, keep_file_open, frame_no)

        !>------------------------------------------------------------------------------
        !>  Description: Write r2c file
        !>------------------------------------------------------------------------------

        !Inputs
        real fld(:, :)
        integer indx
        type(info_out) :: info
        type(ShedGridParams), intent(in) :: shd
        character(len = *), intent(in) :: freq
        integer, allocatable :: dates(:, :)
        integer, optional :: file_unit
        logical, optional :: keep_file_open
        integer, optional :: frame_no

        !Internal
        character(len = 450) flOut
        integer ios, i, un, nfr
        integer na1, nt, j, t, k
        real, dimension(:, :), allocatable :: data_aux
        character(len = 10) ctime
        character(len = 8) cday
        logical opened_status, close_file

        flOut = trim(adjustl(info%pthOut)) // trim(adjustl(info%var_out(indx)%name)) // '_' // trim(adjustl(freq)) // '.r2c'

        if (present(file_unit)) then
            un = file_unit
        else
            un = 882
        end if

        inquire(un, opened = opened_status)

        if (.not. opened_status) then

            open(un, file = trim(adjustl(flOut)), status = 'replace', &
                 form = 'formatted', action = 'write', &
                 iostat = ios)

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
            write(un, 3020) ':Name               ', info%var_out(indx)%name
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
            write(un, 3020) ':AttributeName      ', info%var_out(indx)%name
            write(un, 3020) ':AttributeUnits     ', ''
            write(un, 3005) '#                                       '
            write(un, 3001) ':xCount             ', shd%xCount
            write(un, 3001) ':yCount             ', shd%yCount
            write(un, 3003) ':xDelta             ', shd%xDelta
            write(un, 3003) ':yDelta             ', shd%yDelta
            write(un, 3005) '#                                       '
            write(un, 3005) '#                                       '
            write(un, 3005) ':endHeader                              '

        end if

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

        end if

        if (present(keep_file_open)) then
            close_file = .not. keep_file_open
        else
            close_file = .true.
        end if

        if (close_file) close(un)

3000    format(a10, i5)
3001    format(a20, i16)
3002    format(2a20)
3003    format(a20, f16.7)
3004    format(a20, a10, 2x, a10)
3005    format(a40)
3006    format(a3, a10)
3007    format(a14, i5, a6, i5)
3010    format(a20, a4, '-', a2, '-', a2, 2x, a2, ':', a2)
3012    format(a9)
3020    format(a20, a40)
9000    format(a6, 2i10, 3x, """", i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ":00.000""")

    end subroutine

    !> Subroute: WriteTxt
    !> Write the output to file in text format.
    subroutine WriteTxt(fld, indx, info, shd, freq, dates, file_unit, keep_file_open, frame_no)

        !Inputs
        real fld(:, :)
        integer indx
        type(info_out) :: info
        type(ShedGridParams), intent(in) :: shd
        character(len = *), intent(in) :: freq
        integer, allocatable :: dates(:, :)
        integer, optional :: file_unit
        logical, optional :: keep_file_open
        integer, optional :: frame_no

        !Internal
        character(len = 450) flOut
        integer ios, i, un, nfr
        integer na1, nt, j, t, k
        real, dimension(:, :), allocatable :: data_aux
        character(len = 10) ctime
        character(len = 8) cday
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

        end if

        if (present(keep_file_open)) then
            close_file = .not. keep_file_open
        else
            close_file = .true.
        end if

        if (close_file) close(un)

9000    format("""", i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ":00.000""", 2x)
9001    format(999(e12.6, 2x))

    end subroutine

    !> Subroute: WriteCSV
    !> Write the output to file in CSV format.
    subroutine WriteCSV(fld, indx, info, shd, freq, dates, file_unit, keep_file_open, frame_no)

        !Inputs
        real fld(:, :)
        integer indx
        type(info_out) :: info
        type(ShedGridParams), intent(in) :: shd
        character(len = *), intent(in) :: freq
        integer, allocatable :: dates(:, :)
        integer, optional :: file_unit
        logical, optional :: keep_file_open
        integer, optional :: frame_no

        !Internal
        character(len = 450) flOut
        integer ios, i, un, nfr
        integer na1, nt, j, t, k
        real, dimension(:, :), allocatable :: data_aux
        character(len = 10) ctime
        character(len = 8) cday
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
        end if

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

        end if

        if (present(keep_file_open)) then
            close_file = .not. keep_file_open
        else
            close_file = .true.
        end if

        if (close_file) close(un)

9000    format("""", i4, '/', i2.2, '/', i2.2, 1x, i2.2, ':', i2.2, ":00.000""", ',')
9001    format(999(e12.6, ','))

    end subroutine

end module
