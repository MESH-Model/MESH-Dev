module save_basin_output

    use output_variables

    implicit none

    !> String read from run_options.ini.
    character(len = 100), save :: &
        BASINAVGWBFILEFLAG = '', BASINAVGEBFILEFLAG = '', STREAMFLOWOUTFLAG = '', REACHOUTFLAG = ''

    private

    public run_save_basin_output_init, run_save_basin_output, run_save_basin_output_finalize
    public BASINAVGWBFILEFLAG, BASINAVGEBFILEFLAG, STREAMFLOWOUTFLAG, REACHOUTFLAG

    !> Global types.

    !> For basin water balance.

    type BasinWaterBalance
        real, dimension(:), allocatable :: PRE, EVAP, ROF, ROFO, ROFS, ROFB, STG_INI, STG_FIN
    end type

    type, extends(BasinWaterBalance) :: BasinWaterStorage
        real, dimension(:), allocatable :: RCAN, SNCAN, SNO, WSNO, PNDW, LZS, DZS
        real, dimension(:, :), allocatable :: LQWS, FRWS
    end type

    !> For PEVP-EVAP and EVPB output.

    type BasinEvp
        real EVAP, PEVP, EVPB, ARRD
    end type

    !> For energy balance.
    !*  FSIN: Incoming shortwave radiation at the surface. [J m-2 during acc.; W m-2 output].
    !*  ALBT: Total albedo of the surface (visible and near-infrared). [--].
    !*  FSOUT: Outgoing shortwave radiation at the surface. [J m-2 during acc.; W m-2 output].
    !*  FLIN: Incoming longwave radiation at the surface. [J m-2 during acc.; W m-2 output].
    !*  GTE: Effective black-body temperature at the surface. [dC].
    !*  FLOUT: Outgoing longwave radiation at the surface. [J m-2 during acc.; W m-2 output].
    !*  QH: Sensible heat flux at the surface. [J m-2 during acc.; W m-2 output].
    !*  QE: Latent heat flux at the surface. [J m-2 during acc.; W m-2 output].
    !*  GZERO: Heat flux into the ground. [J m-2 during acc.; W m-2 output].
    !*  TA: Air temperature. [dC].
    !*  TCAN: Vegetation canopy temperature. [dC].
    !*  CMAS: Vegetation canopy mass. [kg m-2].
    !*  TSNOW: Snowpack temperature. [dC].
    !*  TPOND: Temperature of ponded water. [dC].
    !*  TBAR: Temperature of soil layers. [dC].
    !*  BAL0: Balance at the beginning of the time-step. [W m-2].
    !*  BAL1: Balance at the end of the time-step. [W m-2].
    type BasinEnergyBalance
        integer, dimension(:), allocatable :: &
            IFS, IPOND, ICAN, ISNOW
        real, dimension(:), allocatable :: &
            FSIN, ALBT, FSOUT, FLIN, GTE, FLOUT, QH, QE, GZERO, &
            TA, TCAN, CMAS, TSNOW, TPOND
        real, dimension(:, :), allocatable :: TBAR
        real, dimension(:), allocatable :: BAL0, BAL1
    end type

    !> Basin output.

    type BasinOutput
        type(BasinWaterStorage), dimension(:), allocatable :: wb
        type(BasinEvp), dimension(:), allocatable :: evpdts
        type(BasinEnergyBalance), dimension(:), allocatable :: eb
    end type

    type BasinOutputConfigFlag
        integer :: t = 1
        integer, dimension(:), allocatable :: n, ns, nr
    end type

    type BasinOutputConfig
        type(BasinOutputConfigFlag) wb, eb
    end type

    !> Local type instances.

    type(BasinOutputConfig), save :: bnoflg

    type(BasinOutput), save :: bno

    !> Indices for basin average output.
    !* IKEY_ACC: Accumulated over the run (per time-step).
    !* IKEY_MIN: Min. index of the basin averages (used in the allocation of the variables).
    !* IKEY_MAX: Max. number of indices (used in the allocation of the variables).
    !* IKEY_DLY: Daily average.
    !* IKEY_MLY: Monthly average.
    !* IKEY_HLY: Hourly average.
    !*(IKEY_SSL: Seasonal average.)
    integer :: IKEY_ACC = 1, IKEY_DLY = 2, IKEY_MLY = 3, IKEY_HLY = 4, IKEY_TSP = 5, NKEY = 5

    contains

    !> Global routines.

    subroutine run_save_basin_output_init(fls, shd, cm)

        use model_files_variables
        use sa_mesh_common
        use FLAGS
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer NA, NSL, ikey, n, ii, i, iun, ierr
        character(len = 3) nc

        !> Return if basin output has been disabled.
!-        if (BASINBALANCEOUTFLAG == 0) return

        !> Parse output file flags.
        if (len_trim(BASINAVGWBFILEFLAG) > 0) call parse_basin_output_flag(shd, BASINAVGWBFILEFLAG, bnoflg%wb)
        if (len_trim(BASINAVGEBFILEFLAG) > 0) call parse_basin_output_flag(shd, BASINAVGEBFILEFLAG, bnoflg%eb)

        !> Grab values for common indices.
        NA = shd%NA
        NSL = shd%lc%IGND

        !> Allocate and zero variables for accumulations.
!-        allocate(bno%wb(NKEY))
!-        allocate(bno%eb(NKEY))
!-        do ikey = 1, NKEY
!-            allocate(bno%wb(ikey)%PRE(NA), bno%wb(ikey)%EVAP(NA), bno%wb(ikey)%ROF(NA), &
!-                     bno%wb(ikey)%ROFO(NA), bno%wb(ikey)%ROFS(NA), bno%wb(ikey)%ROFB(NA), &
!-                     bno%wb(ikey)%RCAN(NA), bno%wb(ikey)%SNCAN(NA), &
!-                     bno%wb(ikey)%SNO(NA), bno%wb(ikey)%WSNO(NA), bno%wb(ikey)%PNDW(NA), &
!-                     bno%wb(ikey)%LQWS(NA, NSL), bno%wb(ikey)%FRWS(NA, NSL), &
!-                     bno%wb(ikey)%LZS(NA), bno%wb(ikey)%DZS(NA), &
!-                     bno%wb(ikey)%STG_INI(NA), bno%wb(ikey)%STG_FIN(NA))
!-            bno%wb(ikey)%PRE = 0.0
!-            bno%wb(ikey)%EVAP = 0.0
!-            bno%wb(ikey)%ROF = 0.0
!-            bno%wb(ikey)%ROFO = 0.0
!-            bno%wb(ikey)%ROFS = 0.0
!-            bno%wb(ikey)%ROFB = 0.0
!-            bno%wb(ikey)%RCAN = 0.0
!-            bno%wb(ikey)%SNCAN = 0.0
!-            bno%wb(ikey)%SNO = 0.0
!-            bno%wb(ikey)%WSNO = 0.0
!-            bno%wb(ikey)%PNDW = 0.0
!-            bno%wb(ikey)%LQWS = 0.0
!-            bno%wb(ikey)%FRWS = 0.0
!-            bno%wb(ikey)%LZS = 0.0
!-            bno%wb(ikey)%DZS = 0.0
!-            bno%wb(ikey)%STG_INI = 0.0
!-            allocate( &
!-                bno%eb(ikey)%IFS(NA), bno%eb(ikey)%ICAN(NA), bno%eb(ikey)%ISNOW(NA), bno%eb(ikey)%IPOND(NA), &
!-                bno%eb(ikey)%FSIN(NA), bno%eb(ikey)%ALBT(NA), bno%eb(ikey)%FSOUT(NA), &
!-                bno%eb(ikey)%FLIN(NA), bno%eb(ikey)%GTE(NA), bno%eb(ikey)%FLOUT(NA), &
!-                bno%eb(ikey)%QH(NA), bno%eb(ikey)%QE(NA), &
!-                bno%eb(ikey)%GZERO(NA), &
!-                bno%eb(ikey)%TA(NA), bno%eb(ikey)%TCAN(NA), bno%eb(ikey)%CMAS(NA), &
!-                bno%eb(ikey)%TSNOW(NA), bno%eb(ikey)%TPOND(NA), &
!-                bno%eb(ikey)%TBAR(NA, NSL), &
!-                bno%eb(ikey)%BAL0(NA), bno%eb(ikey)%BAL1(NA))
!-            bno%eb(ikey)%IFS = 0; bno%eb(ikey)%ICAN = 0; bno%eb(ikey)%ISNOW = 0; bno%eb(ikey)%IPOND = 0
!-            bno%eb(ikey)%FSIN = 0.0; bno%eb(ikey)%ALBT = 0.0; bno%eb(ikey)%FSOUT = 0.0
!-            bno%eb(ikey)%FLIN = 0.0; bno%eb(ikey)%GTE = 0.0; bno%eb(ikey)%FLOUT = 0.0
!-            bno%eb(ikey)%QH = 0.0; bno%eb(ikey)%QE = 0.0
!-            bno%eb(ikey)%GZERO = 0.0
!-            bno%eb(ikey)%TA = 0.0; bno%eb(ikey)%TCAN = 0.0; bno%eb(ikey)%CMAS = 0.0
!-            bno%eb(ikey)%TSNOW = 0.0; bno%eb(ikey)%TPOND = 0.0
!-            bno%eb(ikey)%TBAR = 0.0
!-            bno%eb(ikey)%BAL0 = 0.0; bno%eb(ikey)%BAL1 = 0.0
!-        end do

        !> Daily.
        if (btest(bnoflg%wb%t, 0)) then
            open(fls%fl(mfk%f900)%iun, &
                 file = './' // trim(fls%GENDIR_OUT) // '/' // trim(adjustl(fls%fl(mfk%f900)%fn)), &
                 iostat = ierr)
            call output_variables_series_init(shd, cm, out%d)
            call write_water_balance_header(fls, shd, fls%fl(mfk%f900)%iun, 86400)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((fls%fl(mfk%f900)%iun*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        call write_water_balance_header(fls, shd, (fls%fl(mfk%f900)%iun*1000 + n), 86400)
                    end if
                end do
            end if
        end if
        if (btest(bnoflg%eb%t, 0)) then
            open(901, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance.csv')
            call output_variables_series_init(shd, cm, out%d)
            call write_energy_balance_header(fls, shd, 901, 86400)
        end if

        !> Monthly.
        if (btest(bnoflg%wb%t, 1)) then
            open(902, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Monthly.csv')
            call output_variables_series_init(shd, cm, out%m)
            call write_water_balance_header(fls, shd, 902, 86400)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((902*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Monthly_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        call write_water_balance_header(fls, shd, (902*1000 + n), 86400)
                    end if
                end do
            end if
        end if
        if (btest(bnoflg%eb%t, 1)) then
            open(905, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance_Monthly.csv')
            call output_variables_series_init(shd, cm, out%m)
            call write_energy_balance_header(fls, shd, 905, 86400)
        end if

        !> Hourly.
        if (btest(bnoflg%wb%t, 2)) then
            open(903, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Hourly.csv')
            call output_variables_series_init(shd, cm, out%h)
            call write_water_balance_header(fls, shd, 903, 3600)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((903*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_Hourly_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        call write_water_balance_header(fls, shd, (903*1000 + n), 3600)
                    end if
                end do
            end if
        end if
        if (btest(bnoflg%eb%t, 2)) then
            open(906, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance_Hourly.csv')
            call output_variables_series_init(shd, cm, out%h)
            call write_energy_balance_header(fls, shd, 906, 3600)
        end if

        !> Per time-step.
        if (btest(bnoflg%wb%t, 3)) then
            open(904, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_ts.csv')
            call write_water_balance_header(fls, shd, 904, ic%dts)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        write(nc, '(i3)') bnoflg%wb%ns(n)
                        open((904*1000 + n), &
                             file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_water_balance_ts_Gauge' // &
                                    trim(adjustl(nc)) // '.csv', &
                             iostat = ierr)
                        call write_water_balance_header(fls, shd, (904*1000 + n), ic%dts)
                    end if
                end do
            end if
        end if
        if (btest(bnoflg%eb%t, 3)) then
            open(907, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_energy_balance_ts.csv')
            call write_energy_balance_header(fls, shd, 907, ic%dts)
        end if

        !> Calculate initial storage and aggregate through neighbouring cells.
!-        do ikey = 1, NKEY
!-            bno%wb(ikey)%STG_INI = &
!-                (out%ts%grid%rcan + out%ts%grid%sncan + out%ts%grid%sno + out%ts%grid%wsno + out%ts%grid%pndw + &
!-                 out%ts%grid%lzs + out%ts%grid%dzs + &
!-                 sum(out%ts%grid%lqws, 2) + sum(out%ts%grid%fzws, 2))*shd%FRAC
!-        end do
!-        do i = 1, shd%NAA
!-            ii = shd%NEXT(i)
!-            if (ii > 0) then
!-                do ikey = 1, NKEY
!-                    bno%wb(ikey)%STG_INI(ii) = bno%wb(ikey)%STG_INI(ii) + bno%wb(ikey)%STG_INI(i)
!-                end do
!-            end if
!-        end do

        !> Allocate and zero variables for accumulations.
        allocate(bno%evpdts(NKEY))
        bno%evpdts(:)%EVAP = 0.0
        bno%evpdts(:)%PEVP = 0.0
        bno%evpdts(:)%EVPB = 0.0
        bno%evpdts(:)%ARRD = 0.0

        !> Daily.
        if (btest(BASINAVGEVPFILEFLAG, 0)) then
            open(910, file = './' // trim(fls%GENDIR_OUT) // '/' // '/Basin_average_evap.csv')
            call update_evp_header(fls, shd, 910, 86400)
        end if

        !> Monthly.
        if (btest(BASINAVGEVPFILEFLAG, 1)) then
            open(911, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_evap_Monthly.csv')
            call update_evp_header(fls, shd, 911, 86400)
        end if

        !> Hourly.
        if (btest(BASINAVGEVPFILEFLAG, 2)) then
            open(912, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_evap_Hourly.csv')
            call update_evp_header(fls, shd, 912, 3600)
        end if

        !> Per time-step.
        if (btest(BASINAVGEVPFILEFLAG, 3)) then
            open(913, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_evap_ts.csv')
            call update_evp_header(fls, shd, 913, ic%dts)
        end if

        !> Read initial variables values from file.
        if (RESUMEFLAG == 4) then

            !> Open the resume file.
            iun = fls%fl(mfk%f883)%iun
            open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.basin_output', status = 'old', action = 'read', &
                 form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

            !> Basin totals for the water balance (old accumulated).
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)
!-            read(iun)

            !> Basin totals for the water balance (for all time-step intervals).
!-            do ikey = 1, NKEY
!-                read(iun) bno%wb(ikey)%PRE(shd%NAA)
!-                read(iun) bno%wb(ikey)%EVAP(shd%NAA)
!-                read(iun) bno%wb(ikey)%ROF(shd%NAA)
!-                read(iun) bno%wb(ikey)%ROFO(shd%NAA)
!-                read(iun) bno%wb(ikey)%ROFS(shd%NAA)
!-                read(iun) bno%wb(ikey)%ROFB(shd%NAA)
!-                read(iun) bno%wb(ikey)%RCAN(shd%NAA)
!-                read(iun) bno%wb(ikey)%SNCAN(shd%NAA)
!-                read(iun) bno%wb(ikey)%SNO(shd%NAA)
!-                read(iun) bno%wb(ikey)%WSNO(shd%NAA)
!-                read(iun) bno%wb(ikey)%PNDW(shd%NAA)
!-                read(iun) bno%wb(ikey)%LQWS(shd%NAA, :)
!-                read(iun) bno%wb(ikey)%FRWS(shd%NAA, :)
!-                read(iun) bno%wb(ikey)%STG_INI(shd%NAA)
!-            end do

            !> Energy balance.
!            read(iun) bno%eb%QEVP
!            read(iun) bno%eb%QH

            !> Close the file to free the unit.
            close(iun)

        end if !(RESUMEFLAG == 4) then

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_save_basin_output(fls, shd, cm)

        use model_files_variables
        use sa_mesh_common
        use FLAGS
        use model_dates
        use climate_forcing

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer nmth, ndy, n
        real dnar

        !> Return if basin output has been disabled.
!-        if (BASINBALANCEOUTFLAG == 0) return

        !> Update the water balance.
!-        call update_water_balance(shd, cm)

        !> For PEVP-EVAP and EVPB output
        bno%evpdts(:)%EVAP = bno%evpdts(:)%EVAP + sum(out%ts%grid%evap(1:shd%NA)*ic%dts*shd%FRAC)/sum(shd%FRAC)
        bno%evpdts(:)%PEVP = bno%evpdts(:)%PEVP + sum(out%ts%grid%pevp(1:shd%NA)*ic%dts*shd%FRAC)/sum(shd%FRAC)
        bno%evpdts(:)%EVPB = bno%evpdts(:)%EVPB + sum(out%ts%grid%evpb(1:shd%NA)*shd%FRAC)/sum(shd%FRAC)
        bno%evpdts(:)%ARRD = bno%evpdts(:)%ARRD + sum(out%ts%grid%arrd(1:shd%NA)*shd%FRAC)/sum(shd%FRAC)

        !> Update the energy balance.
        call update_energy_balance(shd, cm)

        !> Hourly: IKEY_HLY
        if (ic%now%hour /= ic%next%hour) then
!todo: change this to pass the index of the file object.
            if (btest(bnoflg%wb%t, 2)) then
!-                call save_water_balance(shd, 3600, IKEY_HLY)
                call write_water_balance(fls, shd, 903, 3600, shd%NAA, out%h)
                if (allocated(bnoflg%wb%ns)) then
                    do n = 1, size(bnoflg%wb%ns)
                        if (bnoflg%wb%ns(n) > 0) then
                            call write_water_balance(fls, shd, (903*1000 + n), 3600, fms%stmg%meta%rnk(bnoflg%wb%ns(n)), out%h)
                        end if
                    end do
                end if
!-                call reset_water_balance(IKEY_HLY)
            end if
            if (btest(BASINAVGEVPFILEFLAG, 2)) call update_evp(fls, shd, 912, 3600, IKEY_HLY)
            if (btest(bnoflg%eb%t, 2)) then
                call save_energy_balance(shd, 3600, IKEY_HLY)
                call write_energy_balance(fls, shd, 906, 3600, shd%NAA, out%h)
                call reset_energy_balance(IKEY_HLY)
            end if
        end if

        !> Daily: IKEY_DLY
        if (ic%now%day /= ic%next%day) then
            if (btest(bnoflg%wb%t, 0)) then
!-                call save_water_balance(shd, 86400, IKEY_DLY)
                call write_water_balance(fls, shd, fls%fl(mfk%f900)%iun, 86400, shd%NAA, out%d)
                if (allocated(bnoflg%wb%ns)) then
                    do n = 1, size(bnoflg%wb%ns)
                        if (bnoflg%wb%ns(n) > 0) then
                            call write_water_balance(fls, shd, (fls%fl(mfk%f900)%iun*1000 + n), 86400, &
                                                     fms%stmg%meta%rnk(bnoflg%wb%ns(n)), out%d)
                        end if
                    end do
                end if
!-                call reset_water_balance(IKEY_DLY)
            end if
            if (btest(BASINAVGEVPFILEFLAG, 0)) call update_evp(fls, shd, 910, 86400, IKEY_DLY)
            if (btest(bnoflg%eb%t, 0)) then
                call save_energy_balance(shd, 86400, IKEY_DLY)
                call write_energy_balance(fls, shd, 901, 86400, shd%NAA, out%d)
                call reset_energy_balance(IKEY_DLY)
            end if
        end if

        !> Monthly: IKEY_MLY
        if (ic%now%month /= ic%next%month) then

            !> Determine the next day in the month.
            call Julian2MonthDay((ic%now%jday + 1), ic%now%year, nmth, ndy)

            !> Write-out if the next day will be a new month (current day is the last of the month).
            if (ndy == 1 .or. (ic%now%jday + 1) > leap_year(ic%now%year)) then
                call Julian2MonthDay(ic%now%jday, ic%now%year, nmth, ndy)
                if (btest(bnoflg%wb%t, 1)) then
!-                    call save_water_balance(shd, (86400*ndy), IKEY_MLY)
                    call write_water_balance(fls, shd, 902, (86400*ndy), shd%NAA, out%m)
                    if (allocated(bnoflg%wb%ns)) then
                        do n = 1, size(bnoflg%wb%ns)
                            if (bnoflg%wb%ns(n) > 0) then
                                call write_water_balance(fls, shd, (902*1000 + n), (86400*ndy), &
                                                         fms%stmg%meta%rnk(bnoflg%wb%ns(n)), out%m)
                            end if
                        end do
                    end if
!-                    call reset_water_balance(IKEY_MLY)
                end if
                if (btest(BASINAVGEVPFILEFLAG, 1)) call update_evp(fls, shd, 911, (86400*ndy), IKEY_MLY)
                if (btest(bnoflg%eb%t, 1)) then
                    call save_energy_balance(shd, (86400*ndy), IKEY_MLY)
                    call write_energy_balance(fls, shd, 905, (86400*ndy), shd%NAA, out%m)
                    call reset_energy_balance(IKEY_MLY)
                end if
            end if
        end if

        !> Time-step: IKEY_TSP
        if (btest(bnoflg%wb%t, 3)) then
!-            call save_water_balance(shd, ic%dts, IKEY_TSP)
            call write_water_balance(fls, shd, 904, ic%dts, shd%NAA, out%ts)
            if (allocated(bnoflg%wb%ns)) then
                do n = 1, size(bnoflg%wb%ns)
                    if (bnoflg%wb%ns(n) > 0) then
                        call write_water_balance(fls, shd, (904*1000 + n), ic%dts, fms%stmg%meta%rnk(bnoflg%wb%ns(n)), out%ts)
                    end if
                end do
            end if
!-            call reset_water_balance(IKEY_TSP)
        end if
        if (btest(BASINAVGEVPFILEFLAG, 3)) call update_evp(fls, shd, 913, ic%dts, IKEY_TSP)
        if (btest(bnoflg%eb%t, 3)) then
            call save_energy_balance(shd, ic%dts, IKEY_TSP)
            call write_energy_balance(fls, shd, 907, ic%dts, shd%NAA, out%ts)
            call reset_energy_balance(IKEY_TSP)
        end if

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_save_basin_output_finalize(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_common
        use FLAGS
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

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
!-            write(iun) bno%wb(IKEY_ACC)%PRE(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%EVAP(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%ROF(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%ROFO(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%ROFS(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%ROFB(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%LQWS(shd%NAA, :)
!-            write(iun) bno%wb(IKEY_ACC)%FRWS(shd%NAA, :)
!-            write(iun) bno%wb(IKEY_ACC)%RCAN(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%SNCAN(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%SNO(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%WSNO(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%PNDW(shd%NAA)
!-            write(iun) bno%wb(IKEY_ACC)%STG_INI(shd%NAA)

            !> Other accumulators for the water balance.
!-            do i = 1, NKEY
!-                write(iun) bno%wb(i)%PRE(shd%NAA)
!-                write(iun) bno%wb(i)%EVAP(shd%NAA)
!-                write(iun) bno%wb(i)%ROF(shd%NAA)
!-                write(iun) bno%wb(i)%ROFO(shd%NAA)
!-                write(iun) bno%wb(i)%ROFS(shd%NAA)
!-                write(iun) bno%wb(i)%ROFB(shd%NAA)
!-                write(iun) bno%wb(i)%RCAN(shd%NAA)
!-                write(iun) bno%wb(i)%SNCAN(shd%NAA)
!-                write(iun) bno%wb(i)%SNO(shd%NAA)
!-                write(iun) bno%wb(i)%WSNO(shd%NAA)
!-                write(iun) bno%wb(i)%PNDW(shd%NAA)
!-                write(iun) bno%wb(i)%LQWS(shd%NAA, :)
!-                write(iun) bno%wb(i)%FRWS(shd%NAA, :)
!-                write(iun) bno%wb(i)%STG_INI(shd%NAA)
!-            end do

            !> Energy balance.
!-            write(iun) !bno%eb(2)%QEVP
!-            write(iun) !bno%eb(2)%QH

            !> Other accumulators for the water balance.
!-            do i = 1, NKEY
!-                write(iun) bno%wb(i)%LZS(shd%NAA)
!-                write(iun) bno%wb(i)%DZS(shd%NAA)
!-            end do

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
    subroutine parse_basin_output_flag(shd, in_line, flg)

        use sa_mesh_common
        use strings

        implicit none

        !> Variables.
        type(ShedGridParams) :: shd
        character(len = *), intent(in) :: in_line
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

    subroutine update_water_balance(shd, cm)

        !> For 'shd' variable type and output variables.
        use sa_mesh_common

        !> For 'cm' variable type.
        use climate_forcing

        !> For 'ic' variable.
        use model_dates

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
!-        real, dimension(:), allocatable :: PRE, EVAP, ROF, ROFO, ROFS, ROFB, RCAN, SNCAN, SNO, WSNO, PNDW, LZS, DZS
!-        real, dimension(:, :), allocatable :: LQWS, FRWS
!-        integer NA, NSL, ikey, j, ii, i

        !> Allocate temporary variables.
!-        NA = shd%NA
!-        NSL = shd%lc%IGND
!-        allocate(PRE(NA), EVAP(NA), ROF(NA), ROFO(NA), ROFS(NA), ROFB(NA), &
!-                 RCAN(NA), SNCAN(NA), SNO(NA), WSNO(NA), PNDW(NA), &
!-                 LQWS(NA, NSL), FRWS(NA, NSL), LZS(NA), DZS(NA))

        !> Accumulate variables and aggregate through neighbouring cells.
!-        PRE = out%ts%grid%prec*shd%FRAC
!-        EVAP = out%ts%grid%evap*ic%dts*shd%FRAC
!-        ROF = out%ts%grid%rof*ic%dts*shd%FRAC
!-        ROFO = out%ts%grid%rofo*ic%dts*shd%FRAC
!-        ROFS = out%ts%grid%rofs*ic%dts*shd%FRAC
!-        ROFB = out%ts%grid%rofb*ic%dts*shd%FRAC
!-        RCAN = out%ts%grid%rcan*shd%FRAC
!-        SNCAN = out%ts%grid%sncan*shd%FRAC
!-        SNO = out%ts%grid%sno*shd%FRAC
!-        WSNO = out%ts%grid%wsno*shd%FRAC
!-        PNDW = out%ts%grid%pndw*shd%FRAC
!-        do j = 1, shd%lc%IGND
!-            LQWS(:, j) = out%ts%grid%lqws(:, j)*shd%FRAC
!-            FRWS(:, j) = out%ts%grid%fzws(:, j)*shd%FRAC
!-        end do
!-        LZS = out%ts%grid%lzs*shd%FRAC
!-        DZS = out%ts%grid%dzs*shd%FRAC

        !> Aggregate through neighbouring cells.
!-        do i = 1, shd%NAA
!-            ii = shd%NEXT(i)
!-            if (ii > 0) then
!-                PRE(ii) = PRE(ii) + PRE(i)
!-                EVAP(ii) = EVAP(ii) + EVAP(i)
!-                ROF(ii) = ROF(ii) + ROF(i)
!-                ROFO(ii) = ROFO(ii) + ROFO(i)
!-                ROFS(ii) = ROFS(ii) + ROFS(i)
!-                ROFB(ii) = ROFB(ii) + ROFB(i)
!-                RCAN(ii) = RCAN(ii) + RCAN(i)
!-                SNCAN(ii) = SNCAN(ii) + SNCAN(i)
!-                SNO(ii) = SNO(ii) + SNO(i)
!-                WSNO(ii) = WSNO(ii) + WSNO(i)
!-                PNDW(ii) = PNDW(ii) + PNDW(i)
!-                LQWS(ii, :) = LQWS(ii, :) + LQWS(i, :)
!-                FRWS(ii, :) = FRWS(ii, :) + FRWS(i, :)
!-                LZS(ii) = LZS(ii) + LZS(i)
!-                DZS(ii) = DZS(ii) + DZS(i)
!-            end if
!-        end do

        !> Update run total.
!-        do ikey = 1, NKEY
!-            bno%wb(ikey)%PRE = bno%wb(ikey)%PRE + PRE
!-            bno%wb(ikey)%EVAP = bno%wb(ikey)%EVAP + EVAP
!-            bno%wb(ikey)%ROF = bno%wb(ikey)%ROF + ROF
!-            bno%wb(ikey)%ROFO = bno%wb(ikey)%ROFO + ROFO
!-            bno%wb(ikey)%ROFS = bno%wb(ikey)%ROFS + ROFS
!-            bno%wb(ikey)%ROFB = bno%wb(ikey)%ROFB + ROFB
!-            bno%wb(ikey)%RCAN = bno%wb(ikey)%RCAN + RCAN
!-            bno%wb(ikey)%SNCAN = bno%wb(ikey)%SNCAN + SNCAN
!-            bno%wb(ikey)%SNO = bno%wb(ikey)%SNO + SNO
!-            bno%wb(ikey)%WSNO = bno%wb(ikey)%WSNO + WSNO
!-            bno%wb(ikey)%PNDW = bno%wb(ikey)%PNDW + PNDW
!-            bno%wb(ikey)%LQWS = bno%wb(ikey)%LQWS + LQWS
!-            bno%wb(ikey)%FRWS = bno%wb(ikey)%FRWS + FRWS
!-            bno%wb(ikey)%LZS = bno%wb(ikey)%LZS + LZS
!-            bno%wb(ikey)%DZS = bno%wb(ikey)%DZS + DZS
!-        end do

    end subroutine

    subroutine save_water_balance(shd, dts, ikdts)

        use sa_mesh_common
        use model_dates

        !> Input variables.
        type(ShedGridParams) :: shd
        integer dts, ikdts

        !> Local variables.
!-        real dnts

        !> Denominator for time-step averaged variables.
!-        dnts = real(dts/ic%dts)

        !> Time-average storage components.
!-        bno%wb(ikdts)%RCAN = bno%wb(ikdts)%RCAN/dnts
!-        bno%wb(ikdts)%SNCAN = bno%wb(ikdts)%SNCAN/dnts
!-        bno%wb(ikdts)%SNO = bno%wb(ikdts)%SNO/dnts
!-        bno%wb(ikdts)%WSNO = bno%wb(ikdts)%WSNO/dnts
!-        bno%wb(ikdts)%PNDW = bno%wb(ikdts)%PNDW/dnts
!-        bno%wb(ikdts)%LQWS = bno%wb(ikdts)%LQWS/dnts
!-        bno%wb(ikdts)%FRWS = bno%wb(ikdts)%FRWS/dnts
!-        bno%wb(ikdts)%LZS = bno%wb(ikdts)%LZS/dnts
!-        bno%wb(ikdts)%DZS = bno%wb(ikdts)%DZS/dnts

        !> Calculate storage for the period.
!-        bno%wb(ikdts)%STG_FIN = sum(bno%wb(ikdts)%LQWS, 2) + sum(bno%wb(ikdts)%FRWS, 2) + &
!-                                bno%wb(ikdts)%RCAN + bno%wb(ikdts)%SNCAN + bno%wb(ikdts)%SNO + &
!-                                bno%wb(ikdts)%WSNO + bno%wb(ikdts)%PNDW + &
!-                                bno%wb(ikdts)%LZS + bno%wb(ikdts)%DZS

        !> Calculate storage for the run.
!-        bno%wb(IKEY_ACC)%STG_FIN = (sum(bno%wb(IKEY_ACC)%LQWS, 2) + sum(bno%wb(IKEY_ACC)%FRWS, 2) + &
!-                                    bno%wb(IKEY_ACC)%RCAN + bno%wb(IKEY_ACC)%SNCAN + &
!-                                    bno%wb(IKEY_ACC)%SNO + bno%wb(IKEY_ACC)%WSNO + bno%wb(IKEY_ACC)%PNDW +&
!-                                    bno%wb(IKEY_ACC)%LZS + bno%wb(IKEY_ACC)%DZS) &
!-                                   /ic%ts_count

    end subroutine

    subroutine write_water_balance_header(fls, shd, fik, dts)

        use model_files_variables
        use sa_mesh_common

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
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
        write(fik, 1010) 'LQWS', 'FRWS', 'ALWS', 'LZS', 'DZS', 'STG', 'DSTG'

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine write_water_balance(fls, shd, fik, dts, ina, series)

        use model_files_variables
        use sa_mesh_common
        use model_dates

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts, ina
        type(output_series) series

        !> Local variables.
        integer j
        real :: wsno = 0.0, pndw = 0.0

        !> Make sure the cell is inside the basin.
        ina = min(ina, shd%NAA)

        !> Check for 'NO_DATA' values and transform temperatures to degrees C.
        if (series%basin%wsno(ina) /= out%NO_DATA) wsno = series%basin%wsno(ina)
        if (series%basin%pndw(ina) /= out%NO_DATA) pndw = series%basin%pndw(ina)

        !> Write the time-stamp for the period.
        write(fik, 1010, advance = 'no') ic%now%year
        write(fik, 1010, advance = 'no') ic%now%jday
        if (dts < 86400) write(fik, 1010, advance = 'no') ic%now%hour
        if (dts < 3600) write(fik, 1010, advance = 'no') ic%now%mins

        !> Write the water balance to file.
        write(fik, 1010) &
            out%tot%basin%prec(ina), out%tot%basin%evap(ina)*ic%dts, out%tot%basin%rof(ina)*ic%dts, &
            out%tot%basin%rofo(ina)*ic%dts, out%tot%basin%rofs(ina)*ic%dts, out%tot%basin%rofb(ina)*ic%dts, &
            out%tot%basin%dstgw(ina), &
            series%basin%prec(ina), series%basin%evap(ina)*ic%dts, series%basin%rof(ina)*ic%dts, &
            series%basin%rofo(ina)*ic%dts, series%basin%rofs(ina)*ic%dts, series%basin%rofb(ina)*ic%dts, &
            series%basin%sncan(ina), series%basin%rcan(ina), &
            series%basin%sno(ina), wsno, &
            pndw, &
            (series%basin%lqws(ina, j), series%basin%fzws(ina, j), &
             series%basin%alws(ina, j), j = 1, shd%lc%IGND), &
            sum(series%basin%lqws(ina, :)), &
            sum(series%basin%fzws(ina, :)), &
            sum(series%basin%alws(ina, :)), &
            series%basin%lzs(ina), series%basin%dzs(ina), &
            series%basin%stgw(ina), &
            series%basin%dstgw(ina)

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine reset_water_balance(ikdts)

        !> Input variables.
        integer ikdts

        !> Update the final storage.
!-        bno%wb(ikdts)%STG_INI = bno%wb(ikdts)%STG_FIN

        !> Reset the accumulation for time-averaged output.
!-        bno%wb(ikdts)%PRE = 0.0
!-        bno%wb(ikdts)%EVAP = 0.0
!-        bno%wb(ikdts)%ROF = 0.0
!-        bno%wb(ikdts)%ROFO = 0.0
!-        bno%wb(ikdts)%ROFS = 0.0
!-        bno%wb(ikdts)%ROFB = 0.0
!-        bno%wb(ikdts)%RCAN = 0.0
!-        bno%wb(ikdts)%SNCAN = 0.0
!-        bno%wb(ikdts)%SNO = 0.0
!-        bno%wb(ikdts)%WSNO = 0.0
!-        bno%wb(ikdts)%PNDW = 0.0
!-        bno%wb(ikdts)%LQWS = 0.0
!-        bno%wb(ikdts)%FRWS = 0.0

    end subroutine

    subroutine update_evp(fls, shd, fik, dts, ikdts)

        use model_files_variables
        use sa_mesh_common
        use model_dates

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts, ikdts

        !> Local variables.
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
        write(fik, 1010) bno%evpdts(ikdts)%EVAP, bno%evpdts(ikdts)%PEVP, bno%evpdts(ikdts)%EVPB, bno%evpdts(ikdts)%ARRD

        !> Reset the accumulation for time-averaged output.
        bno%evpdts(ikdts)%EVAP = 0.0
        bno%evpdts(ikdts)%PEVP = 0.0
        bno%evpdts(ikdts)%EVPB = 0.0
        bno%evpdts(ikdts)%ARRD = 0.0

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine update_evp_header(fls, shd, fik, dts)

        use model_files_variables
        use sa_mesh_common

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
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

    subroutine update_energy_balance(shd, cm)

        !> For 'shd' variable type and output variables.
        use sa_mesh_common

        !> For 'cm' variable type.
        use climate_forcing

        !> For 'ic' variable.
        use model_dates

        !> Input variables.
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
!-        integer, dimension(:), allocatable :: &
!-            IFS, ICAN, ISNOW, IPOND
!-        real, dimension(:), allocatable :: &
!-            FSIN, ALBT, FSOUT, FLIN, GTE, FLOUT, QH, QE, GZERO, &
!-            TA, TCAN, CMAS, TSNOW, TPOND
!-        real, allocatable :: TBAR(:, :)
!-        integer ikey, n, s, ii, i, j
!-        real :: TFREZ = 273.16

        !> Prepare local variables for accumulation.
!-        n = shd%NA
!-        s = shd%lc%IGND
!-        allocate( &
!-            IFS(n), ICAN(n), ISNOW(n), IPOND(n), &
!-            FSIN(n), ALBT(n), FSOUT(n), FLIN(n), GTE(n), FLOUT(n), QH(n), QE(n), GZERO(n), &
!-            TA(n), TCAN(n), CMAS(n), TSNOW(n), TPOND(n), TBAR(n, s))
!-        IFS = 0; ICAN = 0; ISNOW = 0; IPOND = 0
!-        FSIN = 0.0; ALBT = 0.0; FSOUT = 0.0; FLIN = 0.0; GTE = 0.0; FLOUT = 0.0; QH = 0.0; QE = 0.0; GZERO = 0.0
!-        TA = 0.0; TCAN = 0.0; CMAS = 0.0; TSNOW = 0.0; TPOND = 0.0; TBAR = 0.0

        !> Time-averaged variables and averaging counters.
!-        where (out%ts%grid%fsin > 0.0)
!-            ALBT = out%ts%grid%albt*shd%FRAC
!-            IFS = 1
!-        end where
!-        where (out%ts%grid%gte > 0.0) GTE = (out%ts%grid%gte - TFREZ)*shd%FRAC
!-        where (out%ts%grid%ta > 0.0) TA = (out%ts%grid%ta - TFREZ)*shd%FRAC
!-        where (out%ts%grid%tcan > 0.0)
!-            CMAS = out%ts%grid%cmas*shd%FRAC
!-            TCAN = (out%ts%grid%tcan - TFREZ)*shd%FRAC
!-            ICAN = 1
!-        end where
!-        where (out%ts%grid%sno > 0.0)
!-            TSNOW = (out%ts%grid%tsno - TFREZ)*shd%FRAC
!-            ISNOW = 1
!-        end where
!-        where (out%ts%grid%zpnd > 0.0)
!-            TPOND = (out%ts%grid%tpnd - TFREZ)*shd%FRAC
!-            IPOND = 1
!-        end where
!-        do j = 1, shd%lc%IGND
!-            where (out%ts%grid%tbar(:, j) > 0.0) TBAR(:, j) = (out%ts%grid%tbar(:, j) - TFREZ)*shd%FRAC
!-        end do

        !> Accumulated fluxes.
        !> Converted from (W m-2 = J m-2 s-1) to J m-2 for accumulation.
!-        FSIN = out%ts%grid%fsin*ic%dts*shd%FRAC
!-        where (ALBT > 0.0)
!-            FSOUT = out%ts%grid%fsout*ic%dts*shd%FRAC
!-        end where
!-        FLIN = out%ts%grid%flin*ic%dts*shd%FRAC
!-        where (out%ts%grid%gte > 0.0) FLOUT = out%ts%grid%flout*ic%dts*shd%FRAC
!-        QH = out%ts%grid%qh*ic%dts*shd%FRAC
!-        QE = out%ts%grid%qe*ic%dts*shd%FRAC
!-        GZERO = out%ts%grid%gzero*ic%dts*shd%FRAC

        !> Propagate through basin cells (by flow direction).
        !> Variables are weighted by FRAC during accumulation.
!-        do i = 1, shd%NAA
!-            ii = shd%NEXT(i)
!-            if (ii > 0) then

                !> Time-averaged variables.
!-                ALBT(ii) = ALBT(ii) + ALBT(i)
!-                GTE(ii) = GTE(ii) + GTE(i)
!-                TA(ii) = TA(ii) + TA(i)
!-                TCAN(ii) = TCAN(ii) + TCAN(i)
!-                CMAS(ii) = CMAS(ii) + CMAS(i)
!-                TSNOW(ii) = TSNOW(ii) + TSNOW(i)
!-                TPOND(ii) = TPOND(ii) + TPOND(i)
!-                TBAR(ii, :) = TBAR(ii, :) + TBAR(i, :)

                !> Counter for time-averaging is either on (1) if any cell has contributed or off (0).
!-                IFS(ii) = max(IFS(ii), IFS(i))
!-                ICAN(ii) = max(ICAN(ii), ICAN(i))
!-                ISNOW(ii) = max(ISNOW(ii), ISNOW(i))
!-                IPOND(ii) = max(IPOND(ii), IPOND(i))

                !> Accumulated fluxes.
!-                FSIN(ii) = FSIN(ii) + FSIN(i)
!-                FSOUT(ii) = FSOUT(ii) + FSOUT(i)
!-                FLIN(ii) = FLIN(ii) + FLIN(i)
!-                FLOUT(ii) = FLOUT(ii) + FLOUT(i)
!-                QH(ii) = QH(ii) + QH(i)
!-                QE(ii) = QE(ii) + QE(i)
!-                GZERO(ii) = GZERO(ii) + GZERO(i)
!-            end if
!-        end do

        !> Update totals.
!-        do ikey = 1, NKEY

            !> Time-averaged variables and averaging counters.
!-            bno%eb(ikey)%ALBT = bno%eb(ikey)%ALBT + ALBT
!-            bno%eb(ikey)%IFS = bno%eb(ikey)%IFS + IFS
!-            bno%eb(ikey)%GTE = bno%eb(ikey)%GTE + GTE
!-            bno%eb(ikey)%TA = bno%eb(ikey)%TA + TA
!-            bno%eb(ikey)%TCAN = bno%eb(ikey)%TCAN + TCAN
!-            bno%eb(ikey)%ICAN = bno%eb(ikey)%ICAN + ICAN
!-            bno%eb(ikey)%CMAS = bno%eb(ikey)%CMAS + CMAS
!-            bno%eb(ikey)%TSNOW = bno%eb(ikey)%TSNOW + TSNOW
!-            bno%eb(ikey)%ISNOW = bno%eb(ikey)%ISNOW + ISNOW
!-            bno%eb(ikey)%TPOND = bno%eb(ikey)%TPOND + TPOND
!-            bno%eb(ikey)%IPOND = bno%eb(ikey)%IPOND + IPOND
!-            bno%eb(ikey)%TBAR = bno%eb(ikey)%TBAR + TBAR

            !> Accumulated fluxes.
!-            bno%eb(ikey)%FSIN = bno%eb(ikey)%FSIN + FSIN
!-            bno%eb(ikey)%FSOUT = bno%eb(ikey)%FSOUT + FSOUT
!-            bno%eb(ikey)%FLIN = bno%eb(ikey)%FLIN + FLIN
!-            bno%eb(ikey)%FLOUT = bno%eb(ikey)%FLOUT + FLOUT
!-            bno%eb(ikey)%QH = bno%eb(ikey)%QH + QH
!-            bno%eb(ikey)%QE = bno%eb(ikey)%QE + QE
!-            bno%eb(ikey)%GZERO = bno%eb(ikey)%GZERO + GZERO
!-        end do

    end subroutine

    subroutine save_energy_balance(shd, dts, ikdts)

        use sa_mesh_common
        use model_dates

        !> Input variables.
        type(ShedGridParams) :: shd
        integer dts, ikdts

        !> Local variables.
!-        real dnts

        !> Number of elapsed time-steps for time-averaged variables.
        !> (dts: total seconds elapsed, s-1)/(ic%dts: seconds in time-step, s-1) = time-steps elapsed.
!-        dnts = real(dts/ic%dts)

        !> Time-averaged variables.
!-        where (bno%eb(ikdts)%IFS > 0)
!-            bno%eb(ikdts)%ALBT = bno%eb(ikdts)%ALBT/bno%eb(ikdts)%IFS
!-        end where
!-        bno%eb(ikdts)%GTE = bno%eb(ikdts)%GTE/dnts
!-        bno%eb(ikdts)%TA = bno%eb(ikdts)%TA/dnts
!-        where (bno%eb(ikdts)%ICAN > 0)
!-            bno%eb(ikdts)%CMAS = bno%eb(ikdts)%CMAS/bno%eb(ikdts)%ICAN
!-            bno%eb(ikdts)%TCAN = bno%eb(ikdts)%TCAN/bno%eb(ikdts)%ICAN
!-        end where
!-        where (bno%eb(ikdts)%ISNOW > 0) bno%eb(ikdts)%TSNOW = bno%eb(ikdts)%TSNOW/bno%eb(ikdts)%ISNOW
!-        where (bno%eb(ikdts)%IPOND > 0) bno%eb(ikdts)%TPOND = bno%eb(ikdts)%TPOND/bno%eb(ikdts)%IPOND
!-        bno%eb(ikdts)%TBAR = bno%eb(ikdts)%TBAR/dnts

        !> Number of seconds elapsed for accumulated fluxes.
        !> dts: total seconds elapsed, s-1.
!-        dnts = real(dts)

        !> Conversion of energy fluxes back to rates.
        !> Output (W m-2) = (Accumulation, J m-2)/(seconds elapsed, s-1).
!-        bno%eb(ikdts)%FSIN = bno%eb(ikdts)%FSIN/dnts
!-        bno%eb(ikdts)%FSOUT = bno%eb(ikdts)%FSOUT/dnts
!-        bno%eb(ikdts)%FLIN = bno%eb(ikdts)%FLIN/dnts
!-        bno%eb(ikdts)%FLOUT = bno%eb(ikdts)%FLOUT/dnts
!-        bno%eb(ikdts)%QH = bno%eb(ikdts)%QH/dnts
!-        bno%eb(ikdts)%QE = bno%eb(ikdts)%QE/dnts
!-        bno%eb(ikdts)%GZERO = bno%eb(ikdts)%GZERO/dnts

    end subroutine

    subroutine write_energy_balance_header(fls, shd, fik, dts)

        use model_files_variables
        use sa_mesh_common

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
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

        !> Variable names.
        write(fik, 1010, advance = 'no') &
            'FSIN', 'FSOUT', 'ALBT', 'FLIN', 'FLOUT', 'GTE', 'QH', 'QE', 'GZERO', &
            'TA', 'TCAN', 'CMAS', 'TSNOW', 'TPOND'
        do j = 1, shd%lc%IGND
            write(ffmti, '(i3)') j
            write(fik, 1010, advance = 'no') 'TBAR' // trim(adjustl(ffmti))
        end do
        write(fik, 1010)

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine write_energy_balance(fls, shd, fik, dts, ina, series)

        use model_files_variables
        use sa_mesh_common
        use model_dates

        !> Input variables.
        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
!todo: change this to the unit attribute of the file object.
        integer fik
        integer dts, ina, ikdts
        type(output_series) series

        !> Local variables.
        integer j
!-        real frac
        real :: albt = 0.0, gte = 0.0, ta = 0.0, tcan = 0.0, cmas = 0.0, tsno = 0.0, tpnd = 0.0
        real tbar(shd%lc%IGND)
        real :: TFREZ = 273.16

        !> Make sure the cell is inside the basin.
        ina = min(ina, shd%NAA)

        !> Use 'frac' to normalize the accumulated weighting by contributing FRAC.
        !> Using DA to calculate 'frac' already accounts for grid FRAC accumulated by flow direction.
!-        frac = shd%DA(ina)/((shd%AL/1000.0)**2)

        !> Check for 'NO_DATA' values and transform temperatures to degrees C.
        albt = 0.0; gte = 0.0; ta = 0.0; tcan = 0.0; cmas = 0.0; tsno = 0.0; tpnd = 0.0
        if (series%basin%albt(ina) /= out%NO_DATA) albt = series%basin%albt(ina)
        if (series%basin%gte(ina) > (TFREZ - 100.0)) gte = series%basin%gte(ina) - TFREZ
        if (series%basin%ta(ina) > (TFREZ - 100.0)) ta = series%basin%ta(ina) - TFREZ
        if (series%basin%tcan(ina) > (TFREZ - 100.0)) tcan = series%basin%tcan(ina) - TFREZ
        if (series%basin%cmas(ina) /= out%NO_DATA) cmas = series%basin%cmas(ina)
        if (series%basin%tsno(ina) > (TFREZ - 100.0)) tsno = series%basin%tsno(ina) - TFREZ
        if (series%basin%tpnd(ina) > (TFREZ - 100.0)) tpnd = series%basin%tpnd(ina) - TFREZ
        tbar = 0.0
        where (series%basin%tbar(ina, :) > (TFREZ - 100.0)) tbar = series%basin%tbar(ina, :) - TFREZ

        !> Write the time-stamp for the period.
        write(fik, 1010, advance = 'no') ic%now%year
        write(fik, 1010, advance = 'no') ic%now%jday
        if (dts < 86400) write(fik, 1010, advance = 'no') ic%now%hour
        if (dts < 3600) write(fik, 1010, advance = 'no') ic%now%mins

        !> Calculate a balance for the period.
!-        bno%eb(ikdts)%BAL1 = ( &
!-            (bno%eb(ikdts)%FSIN(ina) - bno%eb(ikdts)%FSOUT(ina)) + &
!-            (bno%eb(ikdts)%FLIN(ina) - bno%eb(ikdts)%FLOUT(ina)) - &
!-            bno%eb(ikdts)%QH(ina) - bno%eb(ikdts)%QE(ina) - bno%eb(ikdts)%GZERO(ina))/frac

        !> Write the variables to file.
        write(fik, 1010) &
            series%basin%fsin(ina), series%basin%fsout(ina), &
            albt, &
            series%basin%flin(ina), series%basin%flout(ina), gte, &
            series%basin%qh(ina), series%basin%qe(ina), &
            series%basin%gzero(ina), &
            ta, tcan, cmas, &
            tsno, tpnd, &
            (tbar(j), j = 1, shd%lc%IGND)

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine reset_energy_balance(ikdts)

        !> Input variables.
        integer ikdts

        !> Preserve the previous balance value.
!-        bno%eb(ikdts)%BAL0 = bno%eb(ikdts)%BAL1

        !> Reset accumulated variables.
!-        bno%eb(ikdts)%IFS = 0; bno%eb(ikdts)%ICAN = 0; bno%eb(ikdts)%ISNOW = 0; bno%eb(ikdts)%IPOND = 0
!-        bno%eb(ikdts)%FSIN = 0.0; bno%eb(ikdts)%ALBT = 0.0; bno%eb(ikdts)%FSOUT = 0.0
!-        bno%eb(ikdts)%FLIN = 0.0; bno%eb(ikdts)%GTE = 0.0; bno%eb(ikdts)%FLOUT = 0.0
!-        bno%eb(ikdts)%QH = 0.0; bno%eb(ikdts)%QE = 0.0
!-        bno%eb(ikdts)%GZERO = 0.0
!-        bno%eb(ikdts)%TA = 0.0; bno%eb(ikdts)%TCAN = 0.0; bno%eb(ikdts)%CMAS = 0.0
!-        bno%eb(ikdts)%TSNOW = 0.0; bno%eb(ikdts)%TPOND = 0.0
!-        bno%eb(ikdts)%TBAR = 0.0

    end subroutine

end module
