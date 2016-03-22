module save_basin_output

    !> For: type(energy_balance).
    use MODEL_OUTPUT

    implicit none

    private update_water_balance

    !> Global types.

    type BasinWaterBalance
        real PRE, EVAP, ROF, ROFO, ROFS, ROFB, STG_INI, STG_FIN
    end type

    type, extends(BasinWaterBalance) :: BasinWaterStorage
        real RCAN, SNCAN, SNO, WSNO, PNDW
        real, dimension(:), allocatable :: LQWS, FRWS
    end type

    type BasinOutput
        type(BasinWaterStorage) :: wbtot
        type(BasinWaterStorage), dimension(:), allocatable :: wbdts
    end type

    !> Local type instances.

    type(BasinOutput), save, private :: bno

    !> Indices for basin average output.
    !* IKEY_ACC: Accumulated over the run (per time-step).
    !* IKEY_MIN: Min. index of the basin averages (used in the allocation of the variables).
    !* IKEY_MAX: Max. number of indices (used in the allocation of the variables).
    !* IKEY_DLY: Daily average.
    !* IKEY_MLY: Monthly average.
    !* IKEY_HLY: Hourly average.
    !*(IKEY_SSL: Seasonal average.)
    integer, private :: IKEY_ACC = 1, IKEY_DLY = 2, IKEY_MLY = 3, IKEY_HLY = 4, IKEY_TSP = 5

    type(energy_balance) :: eb_out

    contains

    !> Global routines.

    subroutine run_save_basin_output_init(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use FLAGS
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(iter_counter) :: ic
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables for formatting headers for the output files.
        character(20) IGND_CHAR
        character(500) WRT_900_1, WRT_900_2, WRT_900_3, WRT_900_4, WRT_900_f

        !> Local variables.
        integer IOUT, IGND, j, i, ierr, iun
        real dnar

        !> Return if basin output has been disabled.
        if (BASINBALANCEOUTFLAG == 0) return

        !> Denominator for basin area.
        dnar = wb%basin_area

        !> Allocate and zero variables for totals.
        IGND = shd%lc%IGND
        allocate(bno%wbtot%LQWS(IGND), bno%wbtot%FRWS(IGND))
        bno%wbtot%PRE = 0.0
        bno%wbtot%EVAP = 0.0
        bno%wbtot%ROF = 0.0
        bno%wbtot%ROFO = 0.0
        bno%wbtot%ROFS = 0.0
        bno%wbtot%ROFB = 0.0
        bno%wbtot%LQWS = 0.0
        bno%wbtot%FRWS = 0.0
        bno%wbtot%RCAN = 0.0
        bno%wbtot%SNCAN = 0.0
        bno%wbtot%SNO = 0.0
        bno%wbtot%WSNO = 0.0
        bno%wbtot%PNDW = 0.0

        !> Allocate and zero variables for time-averaged output.
        iout = max(IKEY_ACC, IKEY_DLY, IKEY_MLY, IKEY_HLY, IKEY_TSP)
        allocate(bno%wbdts(IOUT))
        bno%wbdts(:)%PRE = 0.0
        bno%wbdts(:)%EVAP = 0.0
        bno%wbdts(:)%ROF = 0.0
        bno%wbdts(:)%ROFO = 0.0
        bno%wbdts(:)%ROFS = 0.0
        bno%wbdts(:)%ROFB = 0.0
        bno%wbdts(:)%RCAN = 0.0
        bno%wbdts(:)%SNCAN = 0.0
        bno%wbdts(:)%SNO = 0.0
        bno%wbdts(:)%WSNO = 0.0
        bno%wbdts(:)%PNDW = 0.0
        do i = 1, IOUT
            allocate(bno%wbdts(i)%LQWS(IGND), bno%wbdts(i)%FRWS(IGND))
            bno%wbdts(i)%LQWS = 0.0
            bno%wbdts(i)%FRWS = 0.0
        end do
        allocate(eb_out%HFS(2:2), eb_out%QEVP(2:2), eb_out%GFLX(2:2, IGND))
        eb_out%QEVP = 0.0
        eb_out%HFS = 0.0

        !> Open CSV output files for the water balance.
        open(fls%fl(mfk%f900)%iun, &
             file = './' // trim(fls%GENDIR_OUT) // '/' // trim(adjustl(fls%fl(mfk%f900)%fn)), &
             iostat = ierr)
!todo: Create this only by flag.
        open(902, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Monthly.csv')
        open(903, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Hourly.csv')
        open(904, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_ts.csv')

        !> Create a header that accounts for the proper number of soil layers.
        wrt_900_1 = 'DAY,YEAR,PREACC' // ',EVAPACC,ROFACC,ROFOACC,' // &
            'ROFSACC,ROFBACC,PRE,EVAP,ROF,ROFO,ROFS,ROFB,SNCAN,RCAN,SNO,WSNO,PNDW,'
        wrt_900_2 = 'LQWS'
        wrt_900_3 = 'FRWS'
        wrt_900_4 = 'ALWS'
        do j = 1, IGND
            write(IGND_CHAR, '(i1)') j
            if (j < IGND) then
                wrt_900_2 = trim(adjustl(wrt_900_2)) // trim(adjustl(IGND_CHAR)) // ',LQWS'
                wrt_900_3 = trim(adjustl(wrt_900_3)) // trim(adjustl(IGND_CHAR)) // ',FRWS'
                wrt_900_4 = trim(adjustl(wrt_900_4)) // trim(adjustl(IGND_CHAR)) // ',ALWS'
            else
                wrt_900_2 = trim(adjustl(wrt_900_2)) // trim(adjustl(IGND_CHAR)) // ','
                wrt_900_3 = trim(adjustl(wrt_900_3)) // trim(adjustl(IGND_CHAR)) // ','
                wrt_900_4 = trim(adjustl(wrt_900_4)) // trim(adjustl(IGND_CHAR)) // ','
            end if
        end do !> j = 1, IGND
        wrt_900_f = trim(adjustl(wrt_900_1)) // &
                    trim(adjustl(wrt_900_2)) // &
                    trim(adjustl(wrt_900_3)) // &
                    trim(adjustl(wrt_900_4)) // 'LQWS,FRWS,ALWS,STG,DSTG,DSTGACC'

        !> Write the header.
        write(fls%fl(mfk%f900)%iun, '(a)') trim(adjustl(wrt_900_f))
        write(902, '(a)') trim(adjustl(wrt_900_f))
        write(903, '(a)') 'DAY,YEAR,HOUR,PREACC' // ',EVAPACC,ROFACC,ROFOACC,' // &
            'ROFSACC,ROFBACC,PRE,EVAP,ROF,ROFO,ROFS,ROFB,SCAN,RCAN,SNO,WSNO,ZPND,' // &
            trim(adjustl(wrt_900_2)) // &
            trim(adjustl(wrt_900_3)) // &
            trim(adjustl(wrt_900_4)) // &
            'THLQ,THLIC,THLQIC,STORAGE,DELTA_STORAGE,DSTOR_ACC'
        write(904, '(a)') 'DAY,YEAR,HOUR,MINS,PREACC' // ',EVAPACC,ROFACC,ROFOACC,' // &
            'ROFSACC,ROFBACC,PRE,EVAP,ROF,ROFO,ROFS,ROFB,SCAN,RCAN,SNO,WSNO,ZPND,' // &
            trim(adjustl(wrt_900_2)) // &
            trim(adjustl(wrt_900_3)) // &
            trim(adjustl(wrt_900_4)) // &
            'THLQ,THLIC,THLQIC,STORAGE,DELTA_STORAGE,DSTOR_ACC'

        !> Open CSV output files for the energy balance and write the header.
        open(901, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance.csv')
        write(901, '(a)') 'DAY,YEAR,HFS,QEVP'

        !> Calculate the initial storage component of the water balance and copy it to the time-averaged output variables.
        bno%wbtot%STG_INI = (sum(wb%LQWS) + sum(wb%FRWS) + &
                             sum(wb%RCAN) + sum(wb%SNCAN) + sum(wb%SNO) + sum(wb%WSNO) + sum(wb%PNDW)) &
                             /dnar
        bno%wbdts(:)%STG_INI = bno%wbtot%STG_INI

        !> Read initial variables values from file.
        if (RESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.basin_output', status = 'old', action = 'read', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Basin totals for the water balance.
            read(iun) bno%wbtot%PRE
            read(iun) bno%wbtot%EVAP
            read(iun) bno%wbtot%ROF
            read(iun) bno%wbtot%ROFO
            read(iun) bno%wbtot%ROFS
            read(iun) bno%wbtot%ROFB
            read(iun) bno%wbtot%LQWS
            read(iun) bno%wbtot%FRWS
            read(iun) bno%wbtot%RCAN
            read(iun) bno%wbtot%SNCAN
            read(iun) bno%wbtot%SNO
            read(iun) bno%wbtot%WSNO
            read(iun) bno%wbtot%PNDW
            read(iun) bno%wbtot%STG_INI

            !> Other accumulators for the water balance.
            iout = max(IKEY_ACC, IKEY_DLY, IKEY_MLY, IKEY_HLY, IKEY_TSP)
            do i = 1, iout
                read(iun) bno%wbdts(i)%PRE
                read(iun) bno%wbdts(i)%EVAP
                read(iun) bno%wbdts(i)%ROF
                read(iun) bno%wbdts(i)%ROFO
                read(iun) bno%wbdts(i)%ROFS
                read(iun) bno%wbdts(i)%ROFB
                read(iun) bno%wbdts(i)%RCAN
                read(iun) bno%wbdts(i)%SNCAN
                read(iun) bno%wbdts(i)%SNO
                read(iun) bno%wbdts(i)%WSNO
                read(iun) bno%wbdts(i)%PNDW
                read(iun) bno%wbdts(i)%LQWS
                read(iun) bno%wbdts(i)%FRWS
                read(iun) bno%wbdts(i)%STG_INI
            end do

            !> Energy balance.
            read(iun) eb_out%QEVP
            read(iun) eb_out%HFS

            !> Close the file to free the unit.
            close(iun)

        end if !(RESUMEFLAG == 4) then

    end subroutine

    subroutine run_save_basin_output(shd, fls, ts, ic, cm, wb, eb, sp, stfl, rrls)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use FLAGS
        use model_files_variabletypes
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        !> Input variables.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(iter_counter) :: ic
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer nmth, ndy, i
        real dnar

        !> Return if basin output has been disabled.
        if (BASINBALANCEOUTFLAG == 0) return

        !> Denominator for basin area.
        dnar = wb%basin_area

        !> Total accumulated water balance.
        bno%wbtot%PRE = bno%wbtot%PRE + sum(wb%PRE)/dnar
        bno%wbtot%EVAP = bno%wbtot%EVAP + sum(wb%EVAP)/dnar
        bno%wbtot%ROF = bno%wbtot%ROF + sum(wb%ROF)/dnar
        bno%wbtot%ROFO = bno%wbtot%ROFO + sum(wb%ROFO)/dnar
        bno%wbtot%ROFS = bno%wbtot%ROFS + sum(wb%ROFS)/dnar
        bno%wbtot%ROFB = bno%wbtot%ROFB + sum(wb%ROFB)/dnar
        bno%wbtot%LQWS(:) = bno%wbtot%LQWS(:) + sum(wb%LQWS, 1)/dnar
        bno%wbtot%FRWS(:) = bno%wbtot%FRWS(:) + sum(wb%FRWS, 1)/dnar
        bno%wbtot%RCAN = bno%wbtot%RCAN + sum(wb%RCAN)/dnar
        bno%wbtot%SNCAN = bno%wbtot%SNCAN + sum(wb%SNCAN)/dnar
        bno%wbtot%SNO = bno%wbtot%SNO + sum(wb%SNO)/dnar
        bno%wbtot%WSNO = bno%wbtot%WSNO + sum(wb%WSNO)/dnar
        bno%wbtot%PNDW = bno%wbtot%PNDW + sum(wb%PNDW)/dnar

        !> Accumulated average storage.
        bno%wbtot%STG_FIN = (sum(bno%wbtot%LQWS) + sum(bno%wbtot%FRWS) + &
                             bno%wbtot%RCAN + bno%wbtot%SNCAN + bno%wbtot%SNO + bno%wbtot%WSNO + bno%wbtot%PNDW) &
                             /ic%ts_count

        !> Accumulation of the water balance for time-averaged output.
        bno%wbdts(:)%PRE = bno%wbdts(:)%PRE + sum(wb%PRE)/dnar
        bno%wbdts(:)%EVAP = bno%wbdts(:)%EVAP + sum(wb%EVAP)/dnar
        bno%wbdts(:)%ROF = bno%wbdts(:)%ROF + sum(wb%ROF)/dnar
        bno%wbdts(:)%ROFO = bno%wbdts(:)%ROFO + sum(wb%ROFO)/dnar
        bno%wbdts(:)%ROFS = bno%wbdts(:)%ROFS + sum(wb%ROFS)/dnar
        bno%wbdts(:)%ROFB = bno%wbdts(:)%ROFB + sum(wb%ROFB)/dnar
        do i = 1, size(bno%wbdts)
            bno%wbdts(i)%LQWS = bno%wbdts(i)%LQWS + sum(wb%LQWS, 1)/dnar
            bno%wbdts(i)%FRWS = bno%wbdts(i)%FRWS + sum(wb%FRWS, 1)/dnar
        end do
        bno%wbdts(:)%RCAN = bno%wbdts(:)%RCAN + sum(wb%RCAN)/dnar
        bno%wbdts(:)%SNCAN = bno%wbdts(:)%SNCAN + sum(wb%SNCAN)/dnar
        bno%wbdts(:)%SNO = bno%wbdts(:)%SNO + sum(wb%SNO)/dnar
        bno%wbdts(:)%WSNO = bno%wbdts(:)%WSNO + sum(wb%WSNO)/dnar
        bno%wbdts(:)%PNDW = bno%wbdts(:)%PNDW + sum(wb%PNDW)/dnar

        !> Hourly (wb): IKEY_HLY
        if (mod(ic%ts_hourly, 3600/ic%dts) == 0) then
!todo: change this to pass the index of the file object.
            call update_water_balance(shd, fls, 903, ic, 3600, IKEY_HLY)
        end if

        !> Daily (wb, eb): IKEY_DLY
        if (mod(ic%ts_daily, 86400/ic%dts) == 0) then
            call update_water_balance(shd, fls, fls%fl(mfk%f900)%iun, ic, 86400, IKEY_DLY)

            !> Energy balance.
            write(901, "(i4,',', i5,',', 999(e12.5,','))") &
                  JDAY_NOW, YEAR_NOW, &
                  eb_out%HFS(IKEY_DLY)/dnar, &
                  eb_out%QEVP(IKEY_DLY)/dnar
        end if

        !> Monthly (wb): IKEY_MLY
        if (mod(ic%ts_daily, 86400/ic%dts) == 0) then

            !> Determine the next day in the month.
            call Julian2MonthDay((JDAY_NOW + 1), YEAR_NOW, nmth, ndy)

            !> Write-out if the next day will be a new month (current day is the last of the month).
            if (ndy == 1 .or. (JDAY_NOW + 1) > leap_year(YEAR_NOW)) then
                call Julian2MonthDay(JDAY_NOW, YEAR_NOW, nmth, ndy)
                call update_water_balance(shd, fls, 902, ic, (86400*ndy), IKEY_MLY)
            end if
        end if

        !> Time-step (wb): IKEY_TSP
        call update_water_balance(shd, fls, 904, ic, ic%dts, IKEY_TSP)

    end subroutine

    subroutine run_save_basin_output_finalize(fls, shd, ic, cm, wb, eb, sv, stfl, rrls)

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
        integer iout, i, ierr, iun

        !> Return if basin output has been disabled.
        if (BASINBALANCEOUTFLAG == 0) return

        !> Save the current state of the variables.
        if (SAVERESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.basin_output', status = 'replace', action = 'write', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Basin totals for the water balance.
            write(iun) bno%wbtot%PRE
            write(iun) bno%wbtot%EVAP
            write(iun) bno%wbtot%ROF
            write(iun) bno%wbtot%ROFO
            write(iun) bno%wbtot%ROFS
            write(iun) bno%wbtot%ROFB
            write(iun) bno%wbtot%LQWS
            write(iun) bno%wbtot%FRWS
            write(iun) bno%wbtot%RCAN
            write(iun) bno%wbtot%SNCAN
            write(iun) bno%wbtot%SNO
            write(iun) bno%wbtot%WSNO
            write(iun) bno%wbtot%PNDW
            write(iun) bno%wbtot%STG_INI

            !> Other accumulators for the water balance.
            iout = max(IKEY_ACC, IKEY_DLY, IKEY_MLY, IKEY_HLY, IKEY_TSP)
            do i = 1, iout
                write(iun) bno%wbdts(i)%PRE
                write(iun) bno%wbdts(i)%EVAP
                write(iun) bno%wbdts(i)%ROF
                write(iun) bno%wbdts(i)%ROFO
                write(iun) bno%wbdts(i)%ROFS
                write(iun) bno%wbdts(i)%ROFB
                write(iun) bno%wbdts(i)%RCAN
                write(iun) bno%wbdts(i)%SNCAN
                write(iun) bno%wbdts(i)%SNO
                write(iun) bno%wbdts(i)%WSNO
                write(iun) bno%wbdts(i)%PNDW
                write(iun) bno%wbdts(i)%LQWS
                write(iun) bno%wbdts(i)%FRWS
                write(iun) bno%wbdts(i)%STG_INI
            end do

            !> Energy balance.
            write(iun) eb_out%QEVP
            write(iun) eb_out%HFS

            !> Close the file to free the unit.
            close(iun)

        end if !(SAVERESUMEFLAG == 4) then

    end subroutine

    !> Local routines.

    subroutine update_water_balance(shd, fls, fik, ic, dts, ikdts)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_files_variabletypes
        use model_files_variables
        use model_dates

        !> Input variables.
        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        integer fik
        type(iter_counter) :: ic
        integer dts, ikdts

        !> Local variables.
        integer IGND, j
        real dnts

        !> Denominator for time-step averaged variables.
        dnts = real(dts/ic%dts)

        !> Average of the storage components.
        bno%wbdts(ikdts)%LQWS = bno%wbdts(ikdts)%LQWS/dnts
        bno%wbdts(ikdts)%FRWS = bno%wbdts(ikdts)%FRWS/dnts
        bno%wbdts(ikdts)%RCAN = bno%wbdts(ikdts)%RCAN/dnts
        bno%wbdts(ikdts)%SNCAN = bno%wbdts(ikdts)%SNCAN/dnts
        bno%wbdts(ikdts)%SNO = bno%wbdts(ikdts)%SNO/dnts
        bno%wbdts(ikdts)%WSNO = bno%wbdts(ikdts)%WSNO/dnts
        bno%wbdts(ikdts)%PNDW = bno%wbdts(ikdts)%PNDW/dnts

        !> Calculate storage for the period.
        bno%wbdts(ikdts)%STG_FIN = sum(bno%wbdts(ikdts)%LQWS) + sum(bno%wbdts(ikdts)%FRWS) + &
                                   bno%wbdts(ikdts)%RCAN + bno%wbdts(ikdts)%SNCAN + bno%wbdts(ikdts)%SNO + &
                                   bno%wbdts(ikdts)%WSNO + bno%wbdts(ikdts)%PNDW

        !> Write the time-stamp for the period.
!todo: change this to the unit attribute of the file object.
        write(fik, "(i4, ',')", advance = 'no') JDAY_NOW
        write(fik, "(i5, ',')", advance = 'no') YEAR_NOW
        if (dts < 86400) write(fik, "(i3, ',')", advance = 'no') HOUR_NOW
        if (dts < 3600) write(fik, "(i3, ',')", advance = 'no') MINS_NOW

        !> Write the water balance to file.
        IGND = shd%lc%IGND
        write(fik, "(999(e14.6, ','))") &
            bno%wbtot%PRE, bno%wbtot%EVAP, bno%wbtot%ROF, &
            bno%wbtot%ROFO, bno%wbtot%ROFS, bno%wbtot%ROFB, &
            bno%wbdts(ikdts)%PRE, bno%wbdts(ikdts)%EVAP, bno%wbdts(ikdts)%ROF, &
            bno%wbdts(ikdts)%ROFO, bno%wbdts(ikdts)%ROFS, bno%wbdts(ikdts)%ROFB, &
            bno%wbdts(ikdts)%SNCAN, bno%wbdts(ikdts)%RCAN, &
            bno%wbdts(ikdts)%SNO, bno%wbdts(ikdts)%WSNO, &
            bno%wbdts(ikdts)%PNDW, &
            (bno%wbdts(ikdts)%LQWS(j), j = 1, IGND), &
            (bno%wbdts(ikdts)%FRWS(j), j = 1, IGND), &
            ((bno%wbdts(ikdts)%LQWS(j) + bno%wbdts(ikdts)%FRWS(j)), j = 1, IGND), &
            sum(bno%wbdts(ikdts)%LQWS), &
            sum(bno%wbdts(ikdts)%FRWS), &
            (sum(bno%wbdts(ikdts)%LQWS) + sum(bno%wbdts(ikdts)%FRWS)), &
            bno%wbdts(ikdts)%STG_FIN, &
            (bno%wbdts(ikdts)%STG_FIN - bno%wbdts(ikdts)%STG_INI), &
            (bno%wbtot%STG_FIN - bno%wbtot%STG_INI)

        !> Update the final storage.
        bno%wbdts(ikdts)%STG_INI = bno%wbdts(ikdts)%STG_FIN

        !> Reset the accumulation for time-averaged output.
        bno%wbdts(ikdts)%PRE = 0.0
        bno%wbdts(ikdts)%EVAP = 0.0
        bno%wbdts(ikdts)%ROF = 0.0
        bno%wbdts(ikdts)%ROFO = 0.0
        bno%wbdts(ikdts)%ROFS = 0.0
        bno%wbdts(ikdts)%ROFB = 0.0
        bno%wbdts(ikdts)%LQWS = 0.0
        bno%wbdts(ikdts)%FRWS = 0.0
        bno%wbdts(ikdts)%RCAN = 0.0
        bno%wbdts(ikdts)%SNCAN = 0.0
        bno%wbdts(ikdts)%SNO = 0.0
        bno%wbdts(ikdts)%WSNO = 0.0
        bno%wbdts(ikdts)%PNDW = 0.0

    end subroutine

end module
