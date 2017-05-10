program storage_eval

    use EF_ParseUtilities, only: FIND_MONTH, FIND_DAY

    use legates_mae
    use legates_mse
    use krause_coeffd
    use krause_ceoffd_weighted
    use krause_nse

    implicit none

    integer, parameter :: nFiles = 2, jObs = 1, jSim = 2
    integer, dimension(nFiles) :: fUnit
    character(50), dimension(nFiles) :: fName

    integer :: month_now, month_old = 0, ijd, iyear, n = 0, nn = 0
    integer, parameter :: nrecords = 96
    integer, parameter :: wbal_stor_col = 33
    real, dimension(nrecords) :: obs = 0.0, sim = 0.0

    integer :: i, j, idummy, istat = 0
    real :: in_obs_monthly = 0.0, in_sim_monthly = 0.0
    real, dimension(wbal_stor_col - 1) :: rdummy

    real stat_mae, stat_mse, stat_coeffd, stat_coeffd_weighted, stat_nse

    fUnit(jObs) = 100
    fName(jObs) = 'GWL_storage.csv'
    fUnit(jSim) = 200
    fName(jSim) = 'Basin_average_water_balance.csv'
!    fName(jSim) = 'Basin_average_water_balance_Monthly.csv'

    ! Update file name from command argument.
    !*TODO

    print 1001

1001 format(/, 1x, 'Calculating monthly storage metrics.', /)

    ! Open the files.
    do i = 1, nFiles
        open(fUnit(i), file = fName(i), status = 'old', action = 'read')
    end do

    ! First line of the simulated record is header.
    read(fUnit(jSim), *)
!*
!* debug tally
open(98, file = 'daily_print_out.csv', status = 'unknown', action = 'write')
open(99, file = 'monthly_print_out.csv', status = 'unknown', action = 'write')
!*

    ! Read in the records.
    do while (istat == 0)

        ! Read in simulated record.
        read(fUnit(jsim), *, iostat = istat) ijd, iyear, (rdummy(j), j = 1, size(rdummy))
        if (is_iostat_end(istat)) then
            print 2001, 'simulated'
        else if (istat > 0) then
            print 2002, 'simulated', istat
            exit
        end if
        in_sim_monthly = in_sim_monthly + rdummy(wbal_stor_col - 2)
        nn = nn + 1

        ! Determine the month value.
        call FIND_MONTH(ijd, iyear, month_now)
        if (month_old == 0) then
            month_old = month_now
        end if
!*
!* debug tally
write(98, "(i6, ',', i4, *(',', f12.3))") month_now, nn, (in_sim_monthly/nn)
!*

        ! Update the tally values if the month has changed.
        if (month_old /= month_now) then

            ! Read in the observed record.
            read(fUnit(jObs), *, iostat = istat) idummy, in_obs_monthly
            if (is_iostat_end(istat)) then
                print 2001, 'observed'
            else if (istat > 0) then
                print 2002, 'observed', istat
                exit
            end if

            ! Update daily record counter.
            n = n + 1

            ! Save the values to the arrays.
            obs(n) = in_obs_monthly
            sim(n) = in_sim_monthly*1000/(nn*3600*24)
            in_sim_monthly = 0.0
!*
!* debug tally
write(99, "(i6, *(',', f12.3))") month_old, obs(n), sim(n)
!*
            nn = 0

            ! Updat the month.
            month_old = month_now

        end if !(month_old /= month_now) then

    end do !while (istat == 0)
!*
!* debug tally
close(98)
close(99)
!*

2001 format(1x, 'Reached end of ', (a), ' record.')
2002 format(/, 1x, 'Error reading ', (a), ' record with IOSTAT=', i4, /)

    ! Close the files.
    do i = 1, nFiles
        close(fUnit(i))
    end do

    ! Calculate the metrics.
    print 3001
    call calc_mae(obs, sim, stat_mae)
    print 3002, 'MAE', stat_mae
    call calc_mse(obs, sim, stat_mse)
    print 3002, 'MSE', stat_mse
    call calc_coeffd(obs, sim, stat_coeffd)
    print 3002, 'COEFFD', stat_coeffd
    call calc_coeffd_weighted(obs, sim, stat_coeffd_weighted)
    print 3002, 'COEFFD_WGTD', stat_coeffd_weighted
    call calc_nse(obs, sim, stat_nse)
    print 3002, 'NSE', stat_nse

3001 format(1x, 'Calculating statistics...', /)
3002 format(1x, a20, ':', f13.4)

    print 5001

5001 format(/, 1x, 'Normal end of program.')

end program !storage_eval
