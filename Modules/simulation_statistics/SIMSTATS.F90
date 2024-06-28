!>
!> Description:
!>  Module to calculate metrics based on observed and simulated streamflow.
!>
!> Pulled from MESH_driver.f90, originally as:
!> January 25, 2012 - M.A. Mekonnen
!> MAM - FOR AUTOCALIBRATION USING PRE-EMPTION - A MAXIMUM OF 1 YEAR (365 DAYS)
!> DAILY STREAM FLOW IS SUPPOSED TO BE USED FOR AUTOCALIBRATION PURPOSE.
!>
!> Updates:
!>  June 2024 - F. Yassin
!>      - Added `kge`, `rmse`, `mae` as output variables in `calc_stats` subroutine.
!>      - Changed the order of PBIAS calculation for meaningfull -ve and +ve bias and multiplied by 100 (as percent).
!>      - Moved calculations of `kge`, `rmse`, and `mae` from external modules or subroutines to `calc_stats`.
!>      - Updated `stats_init` subroutine to allocate new variables: `kge`, `rmse`, `mae`.
!>      - Expanded `stats_write` subroutine to accommodate added metrics.
!>      - Added observed and simulated time-to-peak (`TtP`): `tpds`, `tpdo`.
!>      - Added calculations for Center of Mass (TtCoM) for observed (`tcomo`) and simulated (`tcoms`) values.
!>      - Added calculations for Spring Pulse Onset Date (SPOD) for observed (`spodo`) and simulated (`spods`) values.
module SIMSTATS

    !> General modules.
    use SIMSTATS_config
    use flags
    use model_dates

    !> `PREEMPTIONFLAG`.
    use simstats_nse, only: nse_calc
    use simstats_sae, only: sae_calc
    use simstats_saesrt, only: saesrt_calc

    !> External modules for calculating specific stats.
!-    use calc_drms
!-    use calc_abserr

    implicit none

    private

    public mtsflg, stats_init, stats_update_stfl_daily, stats_write, fbest, ftest
    public stats_state_resume, stats_state_save
    public qobs, qsim, ncal, ns
    public get_simulation_dates, get_adjusted_start_date

!    integer, parameter :: dp=kind(0.d0)

!pulled from MESH_driver.f90
    !* NCAL:    ACTUAL NUMBER OF CALIBRATION DATA
    !* COUNTER: COUNTER FOR THE NUMBER OF PRE-EMPTION STARTS
    !* EXISTS:  LOGICAL TO CHECK IF "pre_emption_value.txt" FILE EXISTS
    !* SAE:     SUM OF ABSOLUTE VALUE OF ERRORS (DEVIATIONS BETWEEN OBSERVED
    !*          AND SIMULATED STREAM FLOWS)
    !* SAESRT:  SUM OF ABSOLUTE VALUE OF ERRORS (DEVIATIONS BETWEEN SORTED OBSERVED
    !*          AND SORTED SIMULATED STREAM FLOWS)
    !* NSE:     MEAN NASH-SUTCLIFFE EFFIECIENCY INDEX OF DAILY FLOWS
    !* FBEST:   SAE AT PREVIOUS TIME STEP TRIAL
    !* FTEST:   SAE AT CURRENT TIME STEP TRIAL
    !* QOBS:    OBSERVED DAILY STREAM FLOW
    !* QSIM:    SIMULATED DAILY STREAM FLOW

    integer ncal, ns
    real fbest, ftest
    real, dimension(:, :), allocatable :: qobs, qsim

    !> STATISTICS FOR MONTE CARLO SIMULATION
    real, dimension(:), allocatable :: bias, nsd, lnsd, nsw, tpds, tpdo, tpw, kge, rmse, mae, tcoms, tcomo, spods, spodo

!-    type(model_output_drms) :: st_drms
!-    type(model_output_abserr) :: st_abserr

    contains

    !> Subroutine to get the start and end dates.
    subroutine get_simulation_dates(start_year, start_jday, end_year, end_jday)

        !> Output variables
        integer, intent(out) :: start_year, start_jday
        integer, intent(out) :: end_year, end_jday

        !> Retrieve the start and end dates from `model_dates`.
        start_year = ic%start%year
        start_jday = ic%start%jday
        end_year = ic%stop%year
        end_jday = ic%stop%jday

    end subroutine

    !> Subroutine to get the adjusted start date after skipping `ilf` days.
    subroutine get_adjusted_start_date(ilf, adj_year, adj_jday)

        !> Input/output variables.
        integer, intent(in) :: ilf
        integer, intent(out) :: adj_year, adj_jday

        !> Local variables.
        integer year, jday, days_in_year

        !> Initialize with start date.
        year = ic%start%year
        jday = ic%start%jday + ilf

        !> Adjust for year transitions
        do while (jday > is_leap_year(year))
            jday = jday - is_leap_year(year)
            year = year + 1
        end do

        !> Set the adjusted date
        adj_year = year
        adj_jday = jday

    end subroutine

    !> Helper function to determine if a year is a leap year.
    integer function is_leap_year(y) result(ndays)

        !> Input/output variables.
        integer, intent(in) :: y
        logical is_leap

        !> Check if year is a leap year and return the number of days.
        is_leap = (mod(y, 4) == 0 .and. .not. mod(y, 100) == 0) .or. (mod(y, 400) == 0)
        if (is_leap) then
            ndays = 366
        else
            ndays = 365
        end if

    end function

    !>
    !> January 25, 2012 - M.A. Mekonnen
    !>
    !> The function computes model efficiency coefficients.
    !>
    !>  OBS     - Observed values
    !>  SIM     - Simulated values
    !>  N       - Number of days
    !>  NS      - Number of stations
    !>  NMIN    - Minimum number of days for model spin-up
    !>  SAE     - Absolute value error
    !>  MAE     - Mean absolute value error
    !>  RMSE    - Root mean squared error
    !>  BIAS    -
    !>  NSD     - Nash-Sutcliffe coefficient
    !>  LNSD    - Nash-Sutcliffe coefficient (with the natural logarithm of runoff)
    !>
    !> Updated June 2024 - F. Yassin (see notes above).
    !>
    subroutine calc_stats(obs, sim, n, bias, nsd, lnsd, nsw, tpds, tpdo, tpw, kge, rmse, mae, tcoms, tcomo, spods, spodo)

        !> Input variables.
        real, intent(in), dimension(:) :: obs, sim
        integer, intent(in) :: n

        !> Output variables.
        real, intent(out) :: bias, nsd, lnsd, nsw, tpds, tpdo, tpw, kge, rmse, mae, tcoms, tcomo, spods, spodo

        !> Local variables.
        !* ilf: Number of days left out in the calculation of metrics (METRICSSPINUP).
        integer ipo(1), ips(1), nad, naw, nw, ilf, j, i, k, ycount
        real lerrd(n), lerrdm(n), errd(n), errdm(n), errtp, ltol
        real obsdm, lobsdm, errd_sum, errdm_sum, lerrd_sum, lerrdm_sum
        real obswm, errw_sum, errwm_sum, sum_obs, sum_sim, mean_obs, mean_sim
        real sum_sq_diff_obs, sum_sq_diff_sim, sum_abs_diff, r, alpha, beta
        real stddev_obs, stddev_sim
        real, dimension(:), allocatable :: obsw, simw, errw, errwm
        real tcom_obs, tcom_sim, tcom_sum
        real mean_val, cum_sum, min_cum_sum, onset_day
        integer adj_year, adj_jday, days_in_year, start_day, end_day

        !> Intrinsic functions.
        intrinsic ceiling, maxloc, sum, count, min, max, log, abs, sqrt

        !> Tolerance for low-flow values.
        ltol = 0.0001

        !> Determine the number of weeks in the simulation.
        nw = ceiling(n/7.0)

        !> Zero the output variables.
        bias = 0.0; nsd = 0.0; lnsd = 0.0; nsw = 0.0; tpds = 0.0; tpdo = 0.0; tpw = 0.0; kge = 0.0; rmse = 0.0; mae = 0.0
        tcoms = 0.0; tcomo = 0.0; spods = 0.0; spodo = 0.0

        !> Apply the spin-up period (METRICSSPINUP).
        if (METRICSINCLUDESPINUP == 1) then
            ilf = 1
        else
            ilf = METRICSSPINUP
        end if

        !> Return if METRICSSPINUP exceeds the length of the run.
        if (METRICSSPINUP > ncal) return

        !> Get the adjusted start date.
        call get_adjusted_start_date(ilf, adj_year, adj_jday)

        !> Count the number of valid non-negative observations.
        nad = count(obs(ilf:n) >= 0.0)

        !> Return if the number of valid observations is zero.
        if (.not. nad > 0) return

        !> Calculate BIAS.
        bias = 100.0*sum(sim(ilf:n) - obs(ilf:n), mask = obs(ilf:n) > 0.0) / (sum(obs(ilf:n), mask = obs(ilf:n) >= 0.0))

        !> Calculate NSD.
        obsdm = sum(obs(ilf:n), mask = obs(ilf:n) >= 0.0)/nad
        errd_sum = sum((obs(ilf:n) - sim(ilf:n))**2, mask = obs(ilf:n) >= 0.0)
        errdm_sum = sum((obs(ilf:n) - obsdm)**2, mask = obs(ilf:n) >= 0.0)
        if (errdm_sum /= 0) then
            nsd = 1.0 - errd_sum/errdm_sum
        end if

        !> Calculate LNSD (NSE of natural log (ln)-daily flow).
        lobsdm = sum(log(obs(ilf:n) + ltol), mask = obs(ilf:n) >= 0.0)/nad
        lerrd_sum = sum((log(obs(ilf:n) + ltol) - log(sim(ilf:n) + ltol))**2, mask = obs(ilf:n) >= 0.0)
        lerrdm_sum = sum((log(obs(ilf:n) + ltol) - lobsdm)**2, mask = obs(ilf:n) >= 0.0)
        if (lerrdm_sum /= 0) then
            lnsd = 1.0 - lerrd_sum/lerrdm_sum
        end if

        !> Calculate RMSE.
        rmse = sqrt(sum((obs(ilf:n) - sim(ilf:n))**2, mask = obs(ilf:n) >= 0.0)/nad)

        !> Calculate MAE.
        mae = sum(abs(obs(ilf:n) - sim(ilf:n)), mask = obs(ilf:n) >= 0.0)/nad

        !> Calculate KGE (Kling-Gupta Efficiency).
        sum_obs = sum(obs(ilf:n), mask = obs(ilf:n) >= 0.0)
        sum_sim = sum(sim(ilf:n), mask = obs(ilf:n) >= 0.0)
        mean_obs = sum_obs/nad
        mean_sim = sum_sim/nad
        sum_sq_diff_obs = sum((obs(ilf:n) - mean_obs)**2, mask = obs(ilf:n) >= 0.0)
        sum_sq_diff_sim = sum((sim(ilf:n) - mean_sim)**2, mask = obs(ilf:n) >= 0.0)
        stddev_obs = sqrt(sum_sq_diff_obs/(nad - 1))
        stddev_sim = sqrt(sum_sq_diff_sim/(nad - 1))
        sum_abs_diff = sum((obs(ilf:n) - mean_obs)*(sim(ilf:n) - mean_sim), mask = obs(ilf:n) >= 0.0)
        r = sum_abs_diff/sqrt(sum_sq_diff_obs*sum_sq_diff_sim)
        alpha = stddev_sim/stddev_obs
        beta = mean_sim/mean_obs
        kge = 1.0 - sqrt((r - 1.0)**2 + (alpha - 1.0)**2 + (beta - 1.0)**2)

        !> Calculate time-to-peak for observed values.
        errtp = 0.0
        ycount = 0
        start_day = ilf
        do while (start_day <= n)
            days_in_year = is_leap_year(adj_year)
            end_day = min(start_day + days_in_year - adj_jday, n)
            if (sum(obs(start_day:end_day)) >= 0.0 .and. end_day - start_day + 1 > 200) then
                ipo = maxloc(obs(start_day:end_day)) !, mask = obs(start_day:end_day) >= 0.0)
                errtp = ipo(1)
                ycount = ycount + 1
            else
                errtp = 0.0
            end if
            tpdo = tpdo + abs(errtp)
            start_day = end_day + 1
            adj_year = adj_year + 1
            adj_jday = 1
        end do
        tpdo = tpdo/ycount

        !> Calculate time-to-peak for simulated values.
        errtp = 0.0
        ycount = 0
        start_day = ilf
        do while (start_day <= n)
            days_in_year = is_leap_year(adj_year)
            end_day = min(start_day + days_in_year - adj_jday, n)
            if (sum(sim(start_day:end_day)) >= 0.0 .and. end_day - start_day + 1 > 200) then
                ipo = maxloc(sim(start_day:end_day)) !, mask = sim(start_day:end_day) >= 0.0)
                errtp = ipo(1)
                ycount = ycount + 1
            else
                errtp = 0.0
            end if
            tpds = tpds + abs(errtp)
            start_day = end_day + 1
            adj_year = adj_year + 1
            adj_jday = 1
        end do
        tpds = tpds/ycount

        !> Calculate the Center of Mass (TtCoM) for observed values.
        tcom_sum = 0.0
        ycount = 0
        start_day = ilf
        do while (start_day <= n)
            days_in_year = is_leap_year(adj_year)
            end_day = min(start_day + days_in_year - adj_jday, n)
            if (sum(obs(start_day:end_day)) >= 0.0 .and. end_day - start_day + 1 > 200) then
                tcom_obs = 0.0
                do k = start_day, end_day
                    if (obs(k) >= 0.0) tcom_obs = tcom_obs + (k - start_day + 1)*obs(k)
                end do
                tcom_obs = tcom_obs/sum(obs(start_day:end_day), mask = sim(start_day:end_day) >= 0.0)
                tcom_sum = tcom_sum + tcom_obs
                ycount = ycount + 1
            end if
            start_day = end_day + 1
            adj_year = adj_year + 1
            adj_jday = 1
        end do
        tcomo = tcom_sum/real(ycount)

        !> Calculate the Center of Mass (TtCoM) for simulated values.
        tcom_sum = 0.0
        ycount = 0
        start_day = ilf
        do while (start_day <= n)
            days_in_year = is_leap_year(adj_year)
            end_day = min(start_day + days_in_year - adj_jday, n)
            if (sum(sim(start_day:end_day)) >= 0.0 .and. end_day - start_day + 1 > 200) then
                tcom_sim = 0.0
                do k = start_day, end_day
                    tcom_sim = tcom_sim + (k - start_day + 1)*sim(k)
                end do
                tcom_sim = tcom_sim/sum(sim(start_day:end_day))
                tcom_sum = tcom_sum + tcom_sim
                ycount = ycount + 1
            end if
            start_day = end_day + 1
            adj_year = adj_year + 1
            adj_jday = 1
        end do
        tcoms = tcom_sum/real(ycount)

        !> Calculate the Spring Pulse Onset Date (SPOD) for observed values.
        spodo = 0.0
        ycount = 0
        start_day = ilf
        do while (start_day <= n)
            days_in_year = is_leap_year(adj_year)
            end_day = min(start_day + days_in_year - adj_jday, n)
            cum_sum = 0.0
            min_cum_sum = 1.0E38
            onset_day = 0.0
            if (sum(obs(start_day:end_day)) >= 0.0 .and. end_day - start_day + 1 > 200) then
                mean_val = sum(obs(start_day:end_day), mask = sim(start_day:end_day) >= 0.0)/(end_day - start_day + 1)
                do k = start_day, end_day
                    if (obs(k) >= 0.0) cum_sum = cum_sum + (obs(k) - mean_val)
                    if (cum_sum < min_cum_sum) then
                        min_cum_sum = cum_sum
                        onset_day = mod(k - 1, 365) + 1
                    end if
                end do
                ycount = ycount + 1
            end if
            spodo = spodo + onset_day
            start_day = end_day + 1
            adj_year = adj_year + 1
            adj_jday = 1
        end do
        spodo = spodo/real(ycount)

        !> Calculate the Spring Pulse Onset Date (SPOD) for simulated values.
        spods = 0.0
        ycount = 0
        start_day = ilf
        do while (start_day <= n)
            days_in_year = is_leap_year(adj_year)
            end_day = min(start_day + days_in_year - adj_jday, n)
            cum_sum = 0.0
            min_cum_sum = 1.0E38
            onset_day = 0.0
            if (sum(sim(start_day:end_day)) >= 0.0 .and. end_day - start_day + 1 > 200) then
                mean_val = sum(sim(start_day:end_day))/(end_day - start_day + 1)
                do k = start_day, end_day
                    cum_sum = cum_sum + (sim(k) - mean_val)
                    if (cum_sum < min_cum_sum) then
                        min_cum_sum = cum_sum
                        onset_day = mod(k - 1, 365) + 1
                    end if
                end do
                ycount = ycount + 1
            end if
            spods = spods + onset_day
            start_day = end_day + 1
            adj_year = adj_year + 1
            adj_jday = 1
        end do
        spods = spods/real(ycount)

        !> Calculate the weekly values for daily observed and simulated values.
        allocate(obsw(nw), simw(nw))
        naw = 0
        do i = ilf, n, 7
            naw = naw + 1
            j = min(i + 6, n)
            obsw(naw) = sum(obs(i:j))
            simw(naw) = sum(sim(i:j))
        end do

        !> Return if the number of weekly observations is zero.
        if (.not. naw > 0) return

        !> Calculate NSW (NSE of weekly-flows).
        obswm = sum(obsw(1:naw), mask = obsw(1:naw) >= 0.0)/naw
        errw_sum = sum((obsw(1:naw) - simw(1:naw))**2)
        errwm_sum = sum((obsw(1:naw) - obswm)**2)
        if (errwm_sum /= 0) then
            nsw = 1.0 - errw_sum/errwm_sum
        end if

    end subroutine

    !>
    !> Description:
    !>
    subroutine stats_init(fls)

        use sa_mesh_common
        use model_files_variables

    !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        logical exists
        integer iun, ierr

        if (fms%stmg%n == 0) mtsflg%AUTOCALIBRATIONFLAG = 0

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0) return

        if (mtsflg%PREEMPTIONFLAG == 1) then
            call print_message('=================================================')
            call print_message('')
            call print_message('     SA_MESH IS RUNNING IN AUTOCALIBRATION MODE')
            call print_message('                USING PRE-EMPTION')
            call print_message('')
            call print_message('=================================================')
            call print_message('')
        end if

!todo: split into stats_state_resume().
        if (mtsflg%PREEMPTIONFLAG >= 1) then
            fbest = +1.0e+10
            inquire(file = trim(adjustl(mtsfl%fl(mtsk%PE)%fn)), exist = exists)
            if (exists) then
                iun = mtsfl%fl(mtsk%PE)%iun
                open(iun, file = trim(adjustl(mtsfl%fl(mtsk%PE)%fn)), status = 'old', action = 'read', iostat = ierr)
                if (ierr == 0) read(iun, *) fbest
                close(iun)
            end if
        end if

        ncal = 0
        ns = fms%stmg%n

        allocate(qobs(leap_year(ic%now%year), ns), qsim(leap_year(ic%now%year), ns))
        allocate( &
            bias(ns), nsd(ns), lnsd(ns), nsw(ns), tpds(ns), tpdo(ns), tpw(ns), kge(ns), rmse(ns), mae(ns), &
            tcoms(ns), tcomo(ns), spods(ns), spodo(ns))

        qobs = 0.0
        qsim = 0.0

!-        if (RESUMEFLAG == 4) then
!-            call stats_state_resume(fls)
!-        end if

    end subroutine

    !>
    !> Description:
    !>
    subroutine stats_update_stfl_daily(fls)

        use sa_mesh_common
        use model_files_variables

        !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        real, dimension(:, :), allocatable :: tmp

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0) return

        !> Increment number of simulated days run.
        ncal = ncal + 1

        !> Expand array.
        if (ncal > size(qobs, 1)) then

            !> Allocate temporary array.
            allocate(tmp(size(qobs, 1) + leap_year(ic%now%year), ns))

            !> Copy and expand 'qobs'.
            tmp = 0.0
            tmp(1:size(qobs, 1), :) = qobs
            deallocate(qobs)
            allocate(qobs(size(tmp, 1), ns))
            qobs = tmp

            !> Copy and expand 'qsim'.
            tmp = 0.0
            tmp(1:size(qsim, 1), :) = qsim
            deallocate(qsim)
            allocate(qsim(size(tmp, 1), ns))
            qsim = tmp

            !> Deallocate temporary array.
            deallocate(tmp)
        end if

        !> Copy measured and simulated values to local arrays.
        qobs(ncal, :) = fms%stmg%qomeas%val
        qsim(ncal, :) = out%d%grid%qo(fms%stmg%meta%rnk(:))

        !> `PREEMPTIONFLAG`.
        if (mtsflg%PREEMPTIONFLAG == 1) then
            if (OBJFNFLAG == 0) then
                ftest = sae_calc(qobs(1:ncal, :), qsim(1:ncal, :), ncal, size(qobs, 2), mtsflg%AUTOCALIBRATIONFLAG)
            elseif (OBJFNFLAG == 1) then
                ftest = saesrt_calc(qobs(1:ncal, :), qsim(1:ncal, :), ncal, size(qobs, 2), mtsflg%AUTOCALIBRATIONFLAG)
            elseif (OBJFNFLAG == 2) then
                call print_warning('SAEMSRT (OBJFNFLAG 2) does not support pre-emption.')
            elseif (OBJFNFLAG == 3) then
                call print_error( &
                    'NSE (OBJFNFLAG 3) does not support pre-emption. Disable pre-emption or use NegNSE (OBJFNFLAG 4) instead.')
                call program_abort()
            elseif (OBJFNFLAG == 4) then
                ftest = nse_calc(qobs(1:ncal, :), qsim(1:ncal, :), ncal, size(qobs, 2), mtsflg%AUTOCALIBRATIONFLAG)
                ftest = -1.0 * ftest
            end if
        end if

!        if (mtsflg%AUTOCALIBRATIONFLAG >= 1 .and. mtsflg%PREEMPTIONFLAG == 1) then
!            if (FTEST > FBEST) goto 199
!        end if

    end subroutine

    subroutine stats_state_save(fls)

        use sa_mesh_common
        use model_files_variables

        !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        integer j, iun, ierr

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0) return

        !> Open the file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.simstats', status = 'replace', action = 'write', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Write the values of the variables to file.
        write(iun) int(ncal, kind = 4)
        write(iun) int(ns, kind = 4)
        write(iun) real(fbest, kind = 4), real(ftest, kind = 4)
        do j = 1, ns
            write(iun) real(qobs(1:ncal, j), kind = 4)
            write(iun) real(qsim(1:ncal, j), kind = 4)
        end do

        !> Close the file to free the unit.
        close(iun)

    end subroutine

    subroutine stats_state_resume(fls)

        use sa_mesh_common
        use model_files_variables

        !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        integer(kind = 4) ncal_i4, ns_i4
        real(kind = 4) fbest_r4, ftest_r4
        real(kind = 4), dimension(:, :), allocatable :: qobs_r4, qsim_r4
        integer j, iun, ierr

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0) return

        !> Open the file.
        iun = fls%fl(mfk%f883)%iun
        open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.simstats', status = 'old', action = 'read', &
             form = 'unformatted', access = 'sequential', iostat = ierr)
!todo: condition for ierr.

        !> Read the values of the variables from file.
        read(iun) ncal_i4
        ncal = int(ncal_i4, kind(ncal))
        read(iun) ns_i4
        ns = int(ns_i4, kind(ns))
        read(iun) fbest_r4, ftest_r4
        fbest = real(fbest_r4, kind(fbest))
        ftest = real(ftest_r4, kind(ftest))
        if (allocated(qobs)) deallocate(qobs, qsim)
        allocate(qobs_r4(ncal, ns), qobs(ncal, ns), qsim_r4(ncal, ns), qsim(ncal, ns))
        do j = 1, ns
            read(iun) qobs_r4(1:ncal, j)
            read(iun) qsim_r4(1:ncal, j)
        end do
        qobs = real(qobs_r4, kind(qobs))
        qsim = real(qsim_r4, kind(qsim))
        deallocate(qobs_r4, qsim_r4)

        !> Close the file to free the unit.
        close(iun)

    end subroutine

    !>
    !> Description: Write the metrics of the simulation to file.
    !>
    subroutine stats_write(fls)

        use sa_mesh_common
        use model_files_variables

        !> External functions.
!-        real, external :: KGE

        !> Input variables.
        type(fl_ids) :: fls

        !> Local variables.
        logical exists
!-        real, dimension(:), allocatable :: fkge
        integer j, iun

!-        if (SAVERESUMEFLAG == 4) then
!-            call stats_state_save(fls)
!-            return
!-        end if

        !> Return if not the head node of if AUTOCALIBRATIONFLAG is not active, or the number of calibration points is zero.
        if (.not. ISHEADNODE .or. mtsflg%AUTOCALIBRATIONFLAG == 0 .or. ncal == 0) return

        !> Check if the array to keep file information for the metrics is allocated.
        if (.not. allocated(mtsfl%fl)) call init_metricsout_files()

        !> Write the output function for pre-emption.
        if (mtsflg%PREEMPTIONFLAG == 1 .and. mtsfl%fl(mtsk%fo)%init) then
            iun = mtsfl%fl(mtsk%fo)%iun
            open(iun, file = trim(adjustl(mtsfl%fl(mtsk%fo)%fn)))
            if (mtsflg%PREEMPTIONFLAG >= 1) ftest = ftest*ic%iter%jday/ncal
            write(iun, "(9999(g15.7e2, ' '))") ftest
            close(iun)
        end if

        !> Return if METRICSSPINUP exceeds the length of the run.
        if (METRICSSPINUP > ncal) return

        !> Calculate the metrics of the simulation.
!-        allocate(fkge(size(qobs, 2)))
!-        fkge = 0.0
        do j = 1, size(qobs, 2)
            call calc_stats( &
                qobs(1:ncal, j), qsim(1:ncal, j), ncal, &
                bias(j), nsd(j), lnsd(j), nsw(j), tpds(j), tpdo(j), tpw(j), kge(j), rmse(j), mae(j), &
                tcoms(j), tcomo(j), spods(j), spodo(j))
!-            fkge(j) = KGE(qobs(max(METRICSSPINUP,1):ncal, j), qsim(max(METRICSSPINUP,1):ncal, j), (ncal - max(METRICSSPINUP,1)) + 1)
        end do
!-        if (mtsfl%fl(mtsk%out)%init .or. mtsfl%fl(mtsk%RMSE)%init) st_drms = calc_drms_value(METRICSSPINUP, ncal, qobs, qsim)
!-        if (mtsfl%fl(mtsk%out)%init .or. mtsfl%fl(mtsk%RMSE)%init) st_abserr = calc_abserr_value(METRICSSPINUP, ncal, qobs, qsim)

        !> Results for Monte-Carlo style analysis are appended to results in an existing file.
        if (mtsfl%fl(mtsk%MC)%init) then
            inquire(file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%MC)%fn), exist = exists)
            iun = mtsfl%fl(mtsk%MC)%iun
            if (exists) then
                open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%MC)%fn), position = 'append', status = 'old')
            else
                open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%MC)%fn))
                write(iun, "(9999(g15.7e2, ' '))") "PBIAS ", "NSD ", "NSW ", "TPD "
            end if
            write(iun, "(9999(g15.7e2, ' '))") (bias(j), nsd(j), nsw(j), int(tpds(j)), j = 1, size(qobs, 2))
            close(iun)
        end if

        !> Write Nash-Sutcliffe coefficient of daily streamflow values.
!todo: there's probably a better way to store a set of multiple statistics in one file.
        if (mtsfl%fl(mtsk%NSE)%init) then
            iun = mtsfl%fl(mtsk%NSE)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%NSE)%fn))
            write(iun, "(9999(g15.7e2, ' '))") (nsd(j), j = 1, size(qobs, 2)), sum(nsd)/size(qobs, 2)
            close(iun)
        end if

        !> Write Nash-Sutcliffe coefficient of weekly streamflow values.
        if (mtsfl%fl(mtsk%NSW)%init) then
            iun = mtsfl%fl(mtsk%NSW)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%NSW)%fn))
            write(iun, "(9999(g15.7e2, ' '))") (nsw(j), j = 1, size(qobs, 2))
            close(iun)
        end if

        !> Write daily root mean squared error.
        if (mtsfl%fl(mtsk%RMSE)%init) then
            iun = mtsfl%fl(mtsk%RMSE)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%RMSE)%fn))
            write(iun, "(9999(g15.7e2, ' '))") rmse, sum(rmse)/size(rmse)
            close(iun)
        end if

        !> Write mean absolute error.
        if (mtsfl%fl(mtsk%ABSE)%init) then
            iun = mtsfl%fl(mtsk%ABSE)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%ABSE)%fn))
            write(iun, "(9999(g15.7e2, ' '))") mae, sum(mae)/size(mae)
            close(iun)
        end if

        !> Write the summary of the metrics to file.
        if (mtsfl%fl(mtsk%out)%init) then
            iun = mtsfl%fl(mtsk%out)%iun
            open(iun, file = trim(fls%GENDIR_OUT) // '/' // trim(mtsfl%fl(mtsk%out)%fn))
            write(iun, "(9999(g15.7e2, ' '))") &
                "Num", "Gauge", "MAE", "RMSE", "PBIAS", "AbsPBIAS", "NSD", "NegNSD", "lnNSD", "NeglnNSD", "KGE", "NegKGE", &
                "TPDS", "TPDO", "TCOMS", "TCOMO", "SPODS", "SPODO"
            do j = 1, size(qobs, 2)
                write(iun, "(9999(g15.7e2, ' '))") &
                    j, fms%stmg%meta%name(j), &
                    mae(j), rmse(j), bias(j), abs(bias(j)), nsd(j), (-1.0*nsd(j)), lnsd(j), (-1.0*lnsd(j)), kge(j), (-1.0*kge(j)), &
                    int(tpds(j)), int(tpdo(j)), int(tcoms(j)), int(tcomo(j)), int(spods(j)), int(spodo(j))
            end do
            close(iun)
        end if

    end subroutine

end module
