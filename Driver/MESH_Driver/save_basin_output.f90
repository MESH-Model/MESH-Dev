module save_basin_output

    !> For: type(energy_balance).
    use sa_mesh_shared_variables
    use MODEL_OUTPUT, only: energy_balance

    implicit none

    !> String read from run_options.ini.
    character(len = 100), save :: BASINAVGWBFILEFLAG = ''

    private

    public run_save_basin_output_init, run_save_basin_output, run_save_basin_output_finalize
    public BASINAVGWBFILEFLAG

    !> Global types.

    !> For basin water balance.

    type BasinWaterBalance
        real, dimension(:), allocatable :: PRE, EVAP, ROF, ROFO, ROFS, ROFB, STG_INI, STG_FIN
    end type

    type, extends(BasinWaterBalance) :: BasinWaterStorage
        real, dimension(:), allocatable :: RCAN, SNCAN, SNO, WSNO, PNDW
        real, dimension(:, :), allocatable :: LQWS, FRWS
    end type

    !> For PEVP-EVAP and EVPB output.

    type BasinEvp
        real EVAP, PEVP, EVPB, ARRD
    end type

    !> Basin output.

    type BasinOutput
        type(BasinWaterStorage), dimension(:), allocatable :: wb
        type(BasinEvp), dimension(:), allocatable :: evpdts
    end type

    type BasinOutputConfigFlag
        integer :: t = 1
        integer, dimension(:), allocatable :: n, ns, nr
    end type

    type BasinOutputConfig
        type(BasinOutputConfigFlag) :: wb
    end type

    !> Local type instances.

    type(BasinOutputConfig), save :: bnoflg

    type(BasinOutput), save, private :: bno

    !> Indices for basin average output.
    !* IKEY_ACC: Accumulated over the run (per time-step).
    !* IKEY_MIN: Min. index of the basin averages (used in the allocation of the variables).
    !* IKEY_MAX: Max. number of indices (used in the allocation of the variables).
    !* IKEY_DLY: Daily average.
    !* IKEY_MLY: Monthly average.
    !* IKEY_HLY: Hourly average.
    !*(IKEY_SSL: Seasonal average.)
    integer, private :: IKEY_ACC = 1, IKEY_DLY = 2, IKEY_MLY = 3, IKEY_HLY = 4, IKEY_TSP = 5, NKEY = 5

    type(energy_balance) :: eb_out

    contains

    !> Global routines.

    subroutine run_save_basin_output_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variables
        use FLAGS
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer NA, NSL, ikey, n, ii, i, j, iun, ierr
        character(len = 3) nc

        !> Return if basin output has been disabled.
!-        if (BASINBALANCEOUTFLAG == 0) return

        !> Parse the water balance output file flag.
        if (len_trim(BASINAVGWBFILEFLAG) > 0) call parse_basin_output_flag(BASINAVGWBFILEFLAG, shd, bnoflg%wb)

        !> Grab values for common indices.
        NA = shd%NA
        NSL = shd%lc%IGND

        !> Allocate and zero variables for accumulations.
        allocate(bno%wb(NKEY))
        do ikey = 1, NKEY
            allocate(bno%wb(ikey)%PRE(NA), bno%wb(ikey)%EVAP(NA), bno%wb(ikey)%ROF(NA), &
                     bno%wb(ikey)%ROFO(NA), bno%wb(ikey)%ROFS(NA), bno%wb(ikey)%ROFB(NA), &
                     bno%wb(ikey)%RCAN(NA), bno%wb(ikey)%SNCAN(NA), &
                     bno%wb(ikey)%SNO(NA), bno%wb(ikey)%WSNO(NA), bno%wb(ikey)%PNDW(NA), &
                     bno%wb(ikey)%LQWS(NA, NSL), bno%wb(ikey)%FRWS(NA, NSL), &
                     bno%wb(ikey)%STG_INI(NA), bno%wb(ikey)%STG_FIN(NA))
            bno%wb(ikey)%PRE = 0.0
            bno%wb(ikey)%EVAP = 0.0
            bno%wb(ikey)%ROF = 0.0
            bno%wb(ikey)%ROFO = 0.0
            bno%wb(ikey)%ROFS = 0.0
            bno%wb(ikey)%ROFB = 0.0
            bno%wb(ikey)%RCAN = 0.0
            bno%wb(ikey)%SNCAN = 0.0
            bno%wb(ikey)%SNO = 0.0
            bno%wb(ikey)%WSNO = 0.0
            bno%wb(ikey)%PNDW = 0.0
            bno%wb(ikey)%LQWS = 0.0
            bno%wb(ikey)%FRWS = 0.0
            bno%wb(ikey)%STG_INI = 0.0
        end do

        !> Daily.
        if (btest(bnoflg%wb%t, 0)) then
            open(fls%fl(mfk%f900)%iun, &
                 file = './' // trim(fls%GENDIR_OUT) // '/' // trim(adjustl(fls%fl(mfk%f900)%fn)), &
                 iostat = ierr)
            call write_water_balance_header(shd, fls, fls%fl(mfk%f900)%iun, 86400)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((fls%fl(mfk%f900)%iun*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        call write_water_balance_header(shd, fls, (fls%fl(mfk%f900)%iun*1000 + n), 86400)
                    end if
                end do
            end if
        end if

        !> Monthly.
        if (btest(bnoflg%wb%t, 1)) then
            open(902, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Monthly.csv')
            call write_water_balance_header(shd, fls, 902, 86400)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((902*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Monthly_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        call write_water_balance_header(shd, fls, (902*1000 + n), 86400)
                    end if
                end do
            end if
        end if

        !> Hourly.
        if (btest(bnoflg%wb%t, 2)) then
            open(903, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Hourly.csv')
            call write_water_balance_header(shd, fls, 903, 3600)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((903*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Hourly_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        call write_water_balance_header(shd, fls, (903*1000 + n), 3600)
                    end if
                end do
            end if
        end if

        !> Per time-step.
        if (btest(bnoflg%wb%t, 3)) then
            open(904, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_ts.csv')
            call write_water_balance_header(shd, fls, 904, ic%dts)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((904*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_ts_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        call write_water_balance_header(shd, fls, (904*1000 + n), ic%dts)
                    end if
                end do
            end if
        end if

        !> Calculate initial storage and aggregate through neighbouring cells.
        do ikey = 1, NKEY
            bno%wb(ikey)%STG_INI = wb%RCAN + wb%SNCAN + wb%SNO + wb%WSNO + wb%PNDW + sum(wb%LQWS, 2) + sum(wb%FRWS, 2)
        end do
        do i = 1, shd%NAA
            ii = shd%NEXT(i)
            if (ii > 0) then
                do ikey = 1, NKEY
                    bno%wb(ikey)%STG_INI(ii) = bno%wb(ikey)%STG_INI(ii) + bno%wb(ikey)%STG_INI(i)
                end do
            end if
        end do

        !> Allocate and zero variables for accumulations.
        allocate(bno%evpdts(NKEY))
        bno%evpdts(:)%EVAP = 0.0
        bno%evpdts(:)%PEVP = 0.0
        bno%evpdts(:)%EVPB = 0.0
        bno%evpdts(:)%ARRD = 0.0

        !> Daily.
        if (btest(BASINAVGEVPFILEFLAG, 0)) then
            open(910, file = './' // trim(fls%GENDIR_OUT) // '/' // '/Basin_average_evap.csv')
            call update_evp_header(shd, fls, 910, 86400)
        end if

        !> Monthly.
        if (btest(BASINAVGEVPFILEFLAG, 1)) then
            open(911, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_evap_Monthly.csv')
            call update_evp_header(shd, fls, 911, 86400)
        end if

        !> Hourly.
        if (btest(BASINAVGEVPFILEFLAG, 2)) then
            open(912, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_evap_Hourly.csv')
            call update_evp_header(shd, fls, 912, 3600)
        end if

        !> Per time-step.
        if (btest(BASINAVGEVPFILEFLAG, 3)) then
            open(913, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_evap_ts.csv')
            call update_evp_header(shd, fls, 913, ic%dts)
        end if

        !> Allocate and zero variables for accumulations.
        allocate(eb_out%HFS(2:2), eb_out%QEVP(2:2), eb_out%GFLX(2:2, NSL))
        eb_out%QEVP = 0.0
        eb_out%HFS = 0.0

        !> Open CSV output files for the energy balance and write the header.
        if (BASINAVGEBFILEFLAG > 0) then
            open(901, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance.csv')
            write(901, 1010) 'YEAR', 'DAY', 'HFS', 'QEVP'
        end if

        !> Read initial variables values from file.
        if (RESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.basin_output', status = 'old', action = 'read', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Basin totals for the water balance (old accumulated).
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)
            read(iun)

            !> Basin totals for the water balance (for all time-step intervals).
            do ikey = 1, NKEY
                read(iun) bno%wb(ikey)%PRE(shd%NAA)
                read(iun) bno%wb(ikey)%EVAP(shd%NAA)
                read(iun) bno%wb(ikey)%ROF(shd%NAA)
                read(iun) bno%wb(ikey)%ROFO(shd%NAA)
                read(iun) bno%wb(ikey)%ROFS(shd%NAA)
                read(iun) bno%wb(ikey)%ROFB(shd%NAA)
                read(iun) bno%wb(ikey)%RCAN(shd%NAA)
                read(iun) bno%wb(ikey)%SNCAN(shd%NAA)
                read(iun) bno%wb(ikey)%SNO(shd%NAA)
                read(iun) bno%wb(ikey)%WSNO(shd%NAA)
                read(iun) bno%wb(ikey)%PNDW(shd%NAA)
                read(iun) bno%wb(ikey)%LQWS(shd%NAA, :)
                read(iun) bno%wb(ikey)%FRWS(shd%NAA, :)
                read(iun) bno%wb(ikey)%STG_INI(shd%NAA)
            end do

            !> Energy balance.
            read(iun) eb_out%QEVP
            read(iun) eb_out%HFS

            !> Close the file to free the unit.
            close(iun)

        end if !(RESUMEFLAG == 4) then

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_save_basin_output(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variables
        use FLAGS
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        !> Input variables.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer nmth, ndy, n
        real dnar

        !> Return if basin output has been disabled.
!-        if (BASINBALANCEOUTFLAG == 0) return

        !> Update the water balance.
        call update_water_balance(shd, wb)

        !> For PEVP-EVAP and EVPB output
        bno%evpdts(:)%EVAP = bno%evpdts(:)%EVAP + sum(wb%EVAP)/wb%basin_area
        bno%evpdts(:)%PEVP = bno%evpdts(:)%PEVP + sum(wb%PEVP)/wb%basin_area
        bno%evpdts(:)%EVPB = bno%evpdts(:)%EVPB + sum(wb%EVPB)/wb%basin_area
        bno%evpdts(:)%ARRD = bno%evpdts(:)%ARRD + sum(wb%ARRD)/wb%basin_area

        !> Hourly (wb): IKEY_HLY
        if (mod(ic%ts_hourly, 3600/ic%dts) == 0) then
!todo: change this to pass the index of the file object.
            if (btest(bnoflg%wb%t, 2)) then
                call save_water_balance(shd, 3600, IKEY_HLY)
                call write_water_balance(shd, fls, 903, 3600, shd%NAA, IKEY_HLY)
                if (allocated(bnoflg%wb%ns)) then
                    do n = 1, size(bnoflg%wb%ns)
                        if (bnoflg%wb%ns(n) > 0) then
                            call write_water_balance(shd, fls, (903*1000 + n), 3600, fms%stmg%rnk(bnoflg%wb%ns(n)), IKEY_DLY)
                        end if
                    end do
                end if
                call reset_water_balance(IKEY_HLY)
            end if
            if (btest(BASINAVGEVPFILEFLAG, 2)) call update_evp(shd, fls, 912, 3600, IKEY_HLY)
        end if

        !> Daily (wb, eb): IKEY_DLY
        if (mod(ic%ts_daily, 86400/ic%dts) == 0) then
            if (btest(bnoflg%wb%t, 0)) then
                call save_water_balance(shd, 86400, IKEY_DLY)
                call write_water_balance(shd, fls, fls%fl(mfk%f900)%iun, 86400, shd%NAA, IKEY_DLY)
                if (allocated(bnoflg%wb%ns)) then
                    do n = 1, size(bnoflg%wb%ns)
                        if (bnoflg%wb%ns(n) > 0) then
                            call write_water_balance(shd, fls, (fls%fl(mfk%f900)%iun*1000 + n), 86400, &
                                                     fms%stmg%rnk(bnoflg%wb%ns(n)), IKEY_DLY)
                        end if
                    end do
                end if
                call reset_water_balance(IKEY_DLY)
            end if
            if (btest(BASINAVGEVPFILEFLAG, 0)) call update_evp(shd, fls, 910, 86400, IKEY_DLY)

            !> Energy balance.
            dnar = wb%basin_area
            if (BASINAVGEBFILEFLAG > 0) then
                write(901, 1010) ic%now%year, ic%now%jday, eb_out%HFS(IKEY_DLY)/dnar, eb_out%QEVP(IKEY_DLY)/dnar
            end if
        end if

        !> Monthly (wb): IKEY_MLY
        if (mod(ic%ts_daily, 86400/ic%dts) == 0) then

            !> Determine the next day in the month.
            call Julian2MonthDay((ic%now%jday + 1), ic%now%year, nmth, ndy)

            !> Write-out if the next day will be a new month (current day is the last of the month).
            if (ndy == 1 .or. (ic%now%jday + 1) > leap_year(ic%now%year)) then
                call Julian2MonthDay(ic%now%jday, ic%now%year, nmth, ndy)
                if (btest(bnoflg%wb%t, 1)) then
                    call save_water_balance(shd, (86400*ndy), IKEY_MLY)
                    call write_water_balance(shd, fls, 902, (86400*ndy), shd%NAA, IKEY_MLY)
                    if (allocated(bnoflg%wb%ns)) then
                        do n = 1, size(bnoflg%wb%ns)
                            if (bnoflg%wb%ns(n) > 0) then
                                call write_water_balance(shd, fls, (902*1000 + n), (86400*ndy), &
                                                         fms%stmg%rnk(bnoflg%wb%ns(n)), IKEY_DLY)
                            end if
                        end do
                    end if
                    call reset_water_balance(IKEY_MLY)
                end if
                if (btest(BASINAVGEVPFILEFLAG, 1)) call update_evp(shd, fls, 911, (86400*ndy), IKEY_MLY)
            end if
        end if

        !> Time-step (wb): IKEY_TSP
        if (btest(bnoflg%wb%t, 3)) then
            call save_water_balance(shd, ic%dts, IKEY_TSP)
            call write_water_balance(shd, fls, 904, ic%dts, shd%NAA, IKEY_TSP)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        call write_water_balance(shd, fls, (904*1000 + n), ic%dts, fms%stmg%rnk(bnoflg%wb%ns(n)), IKEY_DLY)
                    end if
                end do
            end if
            call reset_water_balance(IKEY_TSP)
        end if
        if (btest(BASINAVGEVPFILEFLAG, 3)) call update_evp(shd, fls, 913, ic%dts, IKEY_TSP)

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_save_basin_output_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

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
        integer i, ierr, iun

        !> Return if not the head node.
        if (ipid /= 0) return

        !> Return if basin output has been disabled.
!-        if (BASINBALANCEOUTFLAG == 0) return

        !> Save the current state of the variables.
        if (SAVERESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.basin_output', status = 'replace', action = 'write', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Basin totals for the water balance.
            write(iun) bno%wb(IKEY_ACC)%PRE(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%EVAP(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%ROF(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%ROFO(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%ROFS(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%ROFB(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%LQWS(shd%NAA, :)
            write(iun) bno%wb(IKEY_ACC)%FRWS(shd%NAA, :)
            write(iun) bno%wb(IKEY_ACC)%RCAN(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%SNCAN(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%SNO(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%WSNO(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%PNDW(shd%NAA)
            write(iun) bno%wb(IKEY_ACC)%STG_INI(shd%NAA)

            !> Other accumulators for the water balance.
            do i = 1, NKEY
                write(iun) bno%wb(i)%PRE(shd%NAA)
                write(iun) bno%wb(i)%EVAP(shd%NAA)
                write(iun) bno%wb(i)%ROF(shd%NAA)
                write(iun) bno%wb(i)%ROFO(shd%NAA)
                write(iun) bno%wb(i)%ROFS(shd%NAA)
                write(iun) bno%wb(i)%ROFB(shd%NAA)
                write(iun) bno%wb(i)%RCAN(shd%NAA)
                write(iun) bno%wb(i)%SNCAN(shd%NAA)
                write(iun) bno%wb(i)%SNO(shd%NAA)
                write(iun) bno%wb(i)%WSNO(shd%NAA)
                write(iun) bno%wb(i)%PNDW(shd%NAA)
                write(iun) bno%wb(i)%LQWS(shd%NAA, :)
                write(iun) bno%wb(i)%FRWS(shd%NAA, :)
                write(iun) bno%wb(i)%STG_INI(shd%NAA)
            end do

            !> Energy balance.
            write(iun) eb_out%QEVP
            write(iun) eb_out%HFS

            !> Close the file to free the unit.
            close(iun)

        end if !(SAVERESUMEFLAG == 4) then

    end subroutine

    !> Local routines.

    !>
    !> Description: Subroutine to parse basin_output flag read from run_options.ini
    !>
    !> Input:
    !>  - in_line: basin_output flag read from run_options.ini
    !>
    !> Output:
    !>  - flg: Instance of type(BasinOutputConfigFlag) containing parsed information.
    !>
    subroutine parse_basin_output_flag(in_line, shd, flg)

        use strings

        implicit none

        !> Variables.
        character(len = *), intent(in) :: in_line
        type(ShedGridParams) :: shd
        type(BasinOutputConfigFlag) :: flg

        !> Local variables.
        character(len = 20), dimension(100) :: out_args
        integer nargs, n, j, i, ierr
        character(1) :: delim = ' '

        !> Parse the string.
        call parse(in_line, delim, out_args, nargs)

        !> Reset and construct the flag for output frequency.
        flg%t = 0
        do j = 2, nargs
            select case (lowercase(out_args(j)))
                case ('daily')
                    flg%t = flg%t + 1
                case ('monthly')
                    flg%t = flg%t + 2
                case ('hourly')
                    flg%t = flg%t + 4
                case ('ts')
                    flg%t = flg%t + 8
                case ('all')
                    flg%t = 1
                    flg%t = flg%t + 2
                    flg%t = flg%t + 4
                    flg%t = flg%t + 8
                    exit
                case ('default')
                    flg%t = 1
                    exit
                case ('none')
                    flg%t = 0
                    exit
            end select
        end do

        !> Determine output forms.
        do j = 2, nargs
            select case (lowercase(out_args(j)))
                case ('ns')
                    if (allocated(flg%ns)) deallocate(flg%ns)
                    n = 0
                    do i = j + 1, nargs
                        if (is_letter(out_args(i)(1:1))) exit
                        n = n + 1
                    end do
                    if (n == 0) then
                        n = fms%stmg%n
                        allocate(flg%ns(n))
                        do i = 1, n
                            flg%ns(i) = i
                        end do
                    else
                        allocate(flg%ns(n))
                        do i = j + 1, j + n
                            call value(out_args(i), flg%ns(i - j), ierr)
                            if (flg%ns(i - j) > fms%stmg%n) flg%ns(i - j) = 0
                        end do
                    end if
                case ('nr')
                    if (allocated(flg%nr)) deallocate(flg%nr)
                    n = 0
                    do i = j + 1, nargs
                        if (is_letter(out_args(i)(1:1))) exit
                        n = n + 1
                    end do
                    if (n == 0) then
                        n = fms%rsvr%n
                        allocate(flg%nr(n))
                        do i = 1, n
                            flg%nr(i) = i
                        end do
                    else
                        allocate(flg%nr(n))
                        do i = j + 1, j + n
                            call value(out_args(i), flg%nr(i - j), ierr)
                            if (flg%nr(i - j) > fms%rsvr%n) flg%nr(i - j) = 0
                        end do
                    end if
                case ('n')
                    if (allocated(flg%n)) deallocate(flg%n)
                    n = 0
                    do i = j + 1, nargs
                        if (is_letter(out_args(i)(1:1))) exit
                        n = n + 1
                    end do
                    if (n > 0) then
                        allocate(flg%n(n))
                        do i = j + 1, j + n
                            call value(out_args(i), flg%n(i - j), ierr)
                            if (flg%n(i - j) > shd%NAA) flg%n(i - j) = 0
                        end do
                    end if
            end select
        end do

    end subroutine

    subroutine update_water_balance(shd, wb)

        !> For 'shd' variable.
        use sa_mesh_shared_variables

        !> For 'wb' variable.
        use MODEL_OUTPUT

        !> Input variables.
        type(ShedGridParams) :: shd
        type(water_balance) :: wb

        !> Local variables.
        real, dimension(:), allocatable :: PRE, EVAP, ROF, ROFO, ROFS, ROFB, RCAN, SNCAN, SNO, WSNO, PNDW
        real, dimension(:, :), allocatable :: LQWS, FRWS
        integer NA, NSL, ikey, ii, i

        !> Allocate temporary variables.
        NA = shd%NA
        NSL = shd%lc%IGND
        allocate(PRE(NA), EVAP(NA), ROF(NA), ROFO(NA), ROFS(NA), ROFB(NA), &
                 RCAN(NA), SNCAN(NA), SNO(NA), WSNO(NA), PNDW(NA), &
                 LQWS(NA, NSL), FRWS(NA, NSL))

        !> Accumulate variables and aggregate through neighbouring cells.
        PRE = wb%PRE
        EVAP = wb%EVAP
        ROF = wb%ROF
        ROFO = wb%ROFO
        ROFS = wb%ROFS
        ROFB = wb%ROFB
        RCAN = wb%RCAN
        SNCAN = wb%SNCAN
        SNO = wb%SNO
        WSNO = wb%WSNO
        PNDW = wb%PNDW
        LQWS = wb%LQWS
        FRWS = wb%FRWS

        !> Aggregate through neighbouring cells.
        do i = 1, shd%NAA
            ii = shd%NEXT(i)
            if (ii > 0) then
                PRE(ii) = PRE(ii) + PRE(i)
                EVAP(ii) = EVAP(ii) + EVAP(i)
                ROF(ii) = ROF(ii) + ROF(i)
                ROFO(ii) = ROFO(ii) + ROFO(i)
                ROFS(ii) = ROFS(ii) + ROFS(i)
                ROFB(ii) = ROFB(ii) + ROFB(i)
                RCAN(ii) = RCAN(ii) + RCAN(i)
                SNCAN(ii) = SNCAN(ii) + SNCAN(i)
                SNO(ii) = SNO(ii) + SNO(i)
                WSNO(ii) = WSNO(ii) + WSNO(i)
                PNDW(ii) = PNDW(ii) + PNDW(i)
                LQWS(ii, :) = LQWS(ii, :) + LQWS(i, :)
                FRWS(ii, :) = FRWS(ii, :) + FRWS(i, :)
            end if
        end do

        !> Update run total.
        do ikey = 1, NKEY
            bno%wb(ikey)%PRE = bno%wb(ikey)%PRE + PRE
            bno%wb(ikey)%EVAP = bno%wb(ikey)%EVAP + EVAP
            bno%wb(ikey)%ROF = bno%wb(ikey)%ROF + ROF
            bno%wb(ikey)%ROFO = bno%wb(ikey)%ROFO + ROFO
            bno%wb(ikey)%ROFS = bno%wb(ikey)%ROFS + ROFS
            bno%wb(ikey)%ROFB = bno%wb(ikey)%ROFB + ROFB
            bno%wb(ikey)%RCAN = bno%wb(ikey)%RCAN + RCAN
            bno%wb(ikey)%SNCAN = bno%wb(ikey)%SNCAN + SNCAN
            bno%wb(ikey)%SNO = bno%wb(ikey)%SNO + SNO
            bno%wb(ikey)%WSNO = bno%wb(ikey)%WSNO + WSNO
            bno%wb(ikey)%PNDW = bno%wb(ikey)%PNDW + PNDW
            bno%wb(ikey)%LQWS = bno%wb(ikey)%LQWS + LQWS
            bno%wb(ikey)%FRWS = bno%wb(ikey)%FRWS + FRWS
        end do

    end subroutine

    subroutine save_water_balance(shd, dts, ikdts)

        use sa_mesh_shared_variables
        use model_dates

        !> Input variables.
        type(ShedGridParams) :: shd
        integer dts, ikdts

        !> Local variables.
        real dnts

        !> Denominator for time-step averaged variables.
        dnts = real(dts/ic%dts)

        !> Time-average storage components.
        bno%wb(ikdts)%RCAN = bno%wb(ikdts)%RCAN/dnts
        bno%wb(ikdts)%SNCAN = bno%wb(ikdts)%SNCAN/dnts
        bno%wb(ikdts)%SNO = bno%wb(ikdts)%SNO/dnts
        bno%wb(ikdts)%WSNO = bno%wb(ikdts)%WSNO/dnts
        bno%wb(ikdts)%PNDW = bno%wb(ikdts)%PNDW/dnts
        bno%wb(ikdts)%LQWS = bno%wb(ikdts)%LQWS/dnts
        bno%wb(ikdts)%FRWS = bno%wb(ikdts)%FRWS/dnts

        !> Calculate storage for the period.
        bno%wb(ikdts)%STG_FIN = sum(bno%wb(ikdts)%LQWS, 2) + sum(bno%wb(ikdts)%FRWS, 2) + &
                                bno%wb(ikdts)%RCAN + bno%wb(ikdts)%SNCAN + bno%wb(ikdts)%SNO + &
                                bno%wb(ikdts)%WSNO + bno%wb(ikdts)%PNDW

        !> Calculate storage for the run.
        bno%wb(IKEY_ACC)%STG_FIN = (sum(bno%wb(IKEY_ACC)%LQWS, 2) + sum(bno%wb(IKEY_ACC)%FRWS, 2) + &
                                    bno%wb(IKEY_ACC)%RCAN + bno%wb(IKEY_ACC)%SNCAN + &
                                    bno%wb(IKEY_ACC)%SNO + bno%wb(IKEY_ACC)%WSNO + bno%wb(IKEY_ACC)%PNDW) &
                                   /ic%ts_count

    end subroutine

    subroutine write_water_balance_header(shd, fls, fik, dts)

        use sa_mesh_shared_variables
        use model_files_variables

        !> Input variables.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts

        !> Local variables.
        integer j
        character(len = 3) ffmti

        !> Time-step information.
        write(fik, 1010, advance = 'no') 'YEAR', 'DAY'
        if (dts < 86400) write(fik, 1010, advance = 'no') 'HOUR'
        if (dts < 3600) write(fik, 1010, advance = 'no') 'MINS'

        !> Variables.
        write(fik, 1010, advance = 'no') &
            'PREACC', 'EVAPACC', 'ROFACC', 'ROFOACC', &
            'ROFSACC', 'ROFBACC', 'DSTGACC', &
            'PRE', 'EVAP', 'ROF', 'ROFO', 'ROFS', 'ROFB', 'SNCAN', 'RCAN', 'SNO', 'WSNO', 'PNDW'
        do j = 1, shd%lc%IGND
            write(ffmti, '(i3)') j
            write(fik, 1010, advance = 'no') &
                'LQWS' // trim(adjustl(ffmti)), 'FRWS' // trim(adjustl(ffmti)), 'ALWS' // trim(adjustl(ffmti))
        end do
        write(fik, 1010) 'LQWS', 'FRWS', 'ALWS', 'STG', 'DSTG'

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine write_water_balance(shd, fls, fik, dts, ina, ikdts)

        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates

        !> Input variables.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts, ina, ikdts

        !> Local variables.
        integer NSL, j
        real dnar

        !> Make sure the cell is inside the basin.
        ina = min(ina, shd%NAA)

        !> Contributing drainage area.
        dnar = shd%DA(ina)/((shd%AL/1000.0)**2)

        !> Write the time-stamp for the period.
        write(fik, 1010, advance = 'no') ic%now%year
        write(fik, 1010, advance = 'no') ic%now%jday
        if (dts < 86400) write(fik, 1010, advance = 'no') ic%now%hour
        if (dts < 3600) write(fik, 1010, advance = 'no') ic%now%mins

        !> Write the water balance to file.
        NSL = shd%lc%IGND
        write(fik, 1010) &
            bno%wb(IKEY_ACC)%PRE(ina)/dnar, bno%wb(IKEY_ACC)%EVAP(ina)/dnar, bno%wb(IKEY_ACC)%ROF(ina)/dnar, &
            bno%wb(IKEY_ACC)%ROFO(ina)/dnar, bno%wb(IKEY_ACC)%ROFS(ina)/dnar, bno%wb(IKEY_ACC)%ROFB(ina)/dnar, &
            (bno%wb(IKEY_ACC)%STG_FIN(ina) - bno%wb(IKEY_ACC)%STG_INI(ina))/dnar, &
            bno%wb(ikdts)%PRE(ina)/dnar, bno%wb(ikdts)%EVAP(ina)/dnar, bno%wb(ikdts)%ROF(ina)/dnar, &
            bno%wb(ikdts)%ROFO(ina)/dnar, bno%wb(ikdts)%ROFS(ina)/dnar, bno%wb(ikdts)%ROFB(ina)/dnar, &
            bno%wb(ikdts)%SNCAN(ina)/dnar, bno%wb(ikdts)%RCAN(ina)/dnar, &
            bno%wb(ikdts)%SNO(ina)/dnar, bno%wb(ikdts)%WSNO(ina)/dnar, &
            bno%wb(ikdts)%PNDW(ina)/dnar, &
            (bno%wb(ikdts)%LQWS(ina, j)/dnar, &
            bno%wb(ikdts)%FRWS(ina, j)/dnar, &
            (bno%wb(ikdts)%LQWS(ina, j) + bno%wb(ikdts)%FRWS(ina, j))/dnar, j = 1, NSL), &
            sum(bno%wb(ikdts)%LQWS(ina, :))/dnar, &
            sum(bno%wb(ikdts)%FRWS(ina, :))/dnar, &
            (sum(bno%wb(ikdts)%LQWS(ina, :)) + sum(bno%wb(ikdts)%FRWS(ina, :)))/dnar, &
            bno%wb(ikdts)%STG_FIN(ina)/dnar, &
            (bno%wb(ikdts)%STG_FIN(ina) - bno%wb(ikdts)%STG_INI(ina))/dnar

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine reset_water_balance(ikdts)

        !> Input variables.
        integer ikdts

        !> Update the final storage.
        bno%wb(ikdts)%STG_INI = bno%wb(ikdts)%STG_FIN

        !> Reset the accumulation for time-averaged output.
        bno%wb(ikdts)%PRE = 0.0
        bno%wb(ikdts)%EVAP = 0.0
        bno%wb(ikdts)%ROF = 0.0
        bno%wb(ikdts)%ROFO = 0.0
        bno%wb(ikdts)%ROFS = 0.0
        bno%wb(ikdts)%ROFB = 0.0
        bno%wb(ikdts)%RCAN = 0.0
        bno%wb(ikdts)%SNCAN = 0.0
        bno%wb(ikdts)%SNO = 0.0
        bno%wb(ikdts)%WSNO = 0.0
        bno%wb(ikdts)%PNDW = 0.0
        bno%wb(ikdts)%LQWS = 0.0
        bno%wb(ikdts)%FRWS = 0.0

    end subroutine

    subroutine update_evp(shd, fls, fik, dts, ikdts)

        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates

        !> Input variables.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts, ikdts

        !> Local variables.
        integer IGND, j
        real dnts

        !> Denominator for time-step averaged variables.
        dnts = real(dts/ic%dts)

        !> Average of the storage components.
        bno%evpdts(ikdts)%EVPB = bno%evpdts(ikdts)%EVPB/dnts

        !> Write the time-stamp for the period.
        write(fik, 1010, advance = 'no') ic%now%year
        write(fik, 1010, advance = 'no') ic%now%jday
        if (dts < 86400) write(fik, 1010, advance = 'no') ic%now%hour
        if (dts < 3600) write(fik, 1010, advance = 'no') ic%now%mins

        !> Write the water balance to file.
        IGND = shd%lc%IGND
        write(fik, 1010) bno%evpdts(ikdts)%EVAP, bno%evpdts(ikdts)%PEVP, bno%evpdts(ikdts)%EVPB, bno%evpdts(ikdts)%ARRD

        !> Reset the accumulation for time-averaged output.
        bno%evpdts(ikdts)%EVAP = 0.0
        bno%evpdts(ikdts)%PEVP = 0.0
        bno%evpdts(ikdts)%EVPB = 0.0
        bno%evpdts(ikdts)%ARRD = 0.0

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine update_evp_header(shd, fls, fik, dts)

        use sa_mesh_shared_variables
        use model_files_variables

        !> Input variables.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts

        !> Time-step information.
        write(fik, 1010, advance = 'no') 'YEAR', 'DAY'
        if (dts < 86400) write(fik, 1010, advance = 'no') 'HOUR'
        if (dts < 3600) write(fik, 1010, advance = 'no') 'MINS'

        !> Variables.
        write(fik, 1010) 'EVAP', 'PEVP', 'EVPB', 'ARRD'

1010    format(9999(g15.7e2, ','))

    end subroutine

end module
