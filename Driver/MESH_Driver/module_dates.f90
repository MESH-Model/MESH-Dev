!> *********************************************************************
!> Athor: Gonzalo Sapriza Azuri
!> Description: Handled time steps and dates in MESH
!> *********************************************************************
module model_dates

    implicit none

    type dates_model

        integer, dimension(:, :), allocatable :: dates      !year,month,day,julian day
        integer, dimension(:), allocatable :: years         !array of years in annual freq
        integer, dimension(:, :), allocatable :: mnthyears  !array of month,year in month freq
        integer, dimension(:), allocatable :: daysINyears   !nr days in years
        integer, dimension(:), allocatable :: daysINmonths  !nr days in months
        integer, dimension(:), allocatable :: daysINseasons !nr days in Seasons
!        integer start_date(4), end_date(4)                  !start_date,end_date
        character freq                                      !frequency
!        integer timestep                                    !model time-step
        integer nyears                                      !number oof years
        integer nseason                                     !12 months
        integer nmonths                                     !number of months
        integer nr_days                                     !number of days
        integer nr_timeStep                                 !total number of time steps
!        integer nr_timeStepClimF                            !total number of climate forcing time steps

    end type !dates_model

    type counter_date

        !> Date (from the beginning of the year).
        !*  year: Year.
        !*  jday: Day from the beginning of the year.
        !*  month: Month.
        !*  day: Day in month.
        !*  hour: Hour.
        !*  mins: Half-hour in the hour (0 or 30 minutes).
        integer :: year = 0, jday = 0, month = 0, day = 0, hour = 0, mins = 0

    end type !counter_date_julian

    type counter

        !> Time-stepping information.
        !*  dts: Time-step in seconds.
        !*  dtmins: Time-step in minutes.
        integer :: dts = 1800, dtmins = 30

        !> Current date.
        !*  start: Starting date.
        !*  now: Current date.
        !*  stop: Stopping date.
        type(counter_date) :: start, now, stop

        !> Time-step counts (no reset).
        !*  count_year: Number of simulation years run.
        !*  count_month: Number of simulation months run.
        !*  count_jday: Number of simulation days run.
        !*  count_hour: Number of simulation hours run.
        !*  count_mins: Number of simulation minutes run (half-hourly).
        !*  count_ts: Number of simulation time-steps run.
        integer :: count_year = 0, count_month = 0, count_jday = 0, count_hour = 0, count_mins = 0

        !> Time-step counts (reset).
        !*  ts_daily: Count of the current time-step. Resets daily.
        !*  ts_hourly: Count of the current time-step. Resets hourly.
        !*  ts_halfhourly: Count of the current time-step. Resets every hour or half-past the hour.
        integer :: ts_daily = 1, ts_hourly = 1, ts_halfhourly = 1, ts_count = 1

    end type

    !* TIME_STEP_NOW: Current time-step in minutes.
    !* TIME_STEP_MINS: Time-step of the model in minutes.
    !* TIME_STEP_DELT: Time-step of the model in seconds.
!-    integer YEAR_NOW, JDAY_NOW, HOUR_NOW, MINS_NOW
!-    integer TIME_STEP_NOW
!-    integer :: TIME_STEP_MINS = 30, TIME_STEP_DELT = 1800

    !* YEAR_START: Year at the start of the simulation.
    !* JDAY_START: Julian day at the start of the simulation.
    !* HOUR_START: Hour at the start of the simulation.
    !* MINS_START: Minute (in 30-min. increment; either 0 or 30) at the start of the simulation.
!-    integer YEAR_START, JDAY_START, HOUR_START, MINS_START
!-    integer YEAR_STOP, JDAY_STOP, HOUR_STOP, MINS_STOP

    type(counter), save :: ic

    contains

!-    subroutine counter_init()

        !> Update now counters.
!-        ic%now%year = 0
!-        ic%now%jday = 0
!-        ic%now%month = 0
!-        ic%now%day = 0
!-        ic%now%hour = 0
!-        ic%now%mins = 0

        !> Update time-step.
!-        ic%dts = 1800
!-        ic%dtmins = 30

        !> Initialize counters.
!-        ic%count_year = 0
!-        ic%count_month = 0
!-        ic%count_jday = 0
!-        ic%count_hour = 0
!-        ic%count_mins = 0
!-        ic%ts_daily = 1
!-        ic%ts_hourly = 1
!-        ic%ts_halfhourly = 1
!-        ic%ts_count = 1

!-    end subroutine

    subroutine counter_update()

        !> Local variables.
        integer old_year, old_jday, old_month, old_day, old_hour, old_mins

        !> Keep the old values for comparison.
        old_year = ic%now%year
        old_jday = ic%now%jday
        old_month = ic%now%month
        old_day = ic%now%day
        old_hour = ic%now%hour
        old_mins = ic%now%mins

        !> Increment the current time-step.
        ic%now%mins = ic%now%mins + ic%dtmins ! increment the current time by 30 minutes
        if (ic%now%mins == 60) then
            ic%now%mins = 0
            ic%now%hour = ic%now%hour + 1
            if (ic%now%hour == 24) then
                ic%now%hour = 0
                ic%now%jday = ic%now%jday + 1
                if (ic%now%jday >= 366) then
                    if (mod(ic%now%year, 400) == 0) then !LEAP YEAR
                        if (ic%now%jday == 367) then
                            ic%now%jday = 1
                            ic%now%year = ic%now%year + 1
                        end if
                    else if (mod(ic%now%year, 100) == 0) then !NOT A LEAP YEAR
                        ic%now%jday = 1
                        ic%now%year = ic%now%year + 1
                    else if (mod(ic%now%year, 4) == 0) then !LEAP YEAR
                        if (ic%now%jday == 367) then
                            ic%now%jday = 1
                            ic%now%year = ic%now%year + 1
                        end if
                    else !NOT A LEAP YEAR
                        ic%now%jday = 1
                        ic%now%year = ic%now%year + 1
                    end if
                end if
            end if
        end if

        !> Year:
        if (old_year /= ic%now%year) then
            ic%count_year = ic%count_year + 1
        end if

        !> Julian day:
        if (old_jday /= ic%now%jday) then
            ic%count_jday = ic%count_jday + 1
            ic%ts_daily = 0
        end if

        !> Determine the current month and day.
        call julian2monthday(ic%now%jday, ic%now%year, ic%now%month, ic%now%day)

        !> Month:
        if (old_month /= ic%now%month) then
            ic%count_month = ic%count_month + 1
        end if

        !> Hourly:
        if (old_hour /= ic%now%hour) then
            ic%count_hour = ic%count_hour + 1
            ic%ts_hourly = 0
        end if

        !> Minutes:
        if (old_mins /= ic%now%mins) then
            ic%count_mins = ic%count_mins + 1
            if (ic%now%mins == 0 .or. ic%now%mins == 30) then
                ic%ts_halfhourly = 0
            end if
        end if

        !debug: Print the now.
        !print *, "now: Y JD M D HR"
        !print *, ic%now_year, ic%now_jday, ic%now_month, ic%now_day, ic%now_hour

        !debug: Print count.
        !print *, "count: Y JD M D HR"
        !print *, ic%count_year, ic%count_jday, ic%count_month, ic%count_day, ic%count_hour

        !> Update time-step counters.
        ic%ts_daily = ic%ts_daily + 1
        ic%ts_hourly = ic%ts_hourly + 1
        ic%ts_halfhourly = ic%ts_halfhourly + 1
        ic%ts_count = ic%ts_count + 1

    end subroutine

    !> *****************************************************************
    !> Description: Handled dates in the model
    !> *****************************************************************
    subroutine get_dates(ts)

        !> Input
!        integer, intent(in) :: start_date(4), end_date(4) !Year,julianday,hour,min
!        integer, intent(in) :: stepclim                   !HOURLYFLAG

        !> Input/Output
        type(dates_model) :: ts

        !> Internal variables
        integer nr_years
        integer, allocatable :: days_inyear(:)
        integer :: jday, year, i, iyear, fyear
        integer nr_days!, hours

        ts%freq = "D"
        ts%nyears = ic%stop%year - ic%start%year + 1
        ts%nseason = 12

!        ts%start_date = start_date
!        ts%end_date = end_date

        iyear = ic%start%year
        fyear = ic%stop%year
        year = ic%start%year

        allocate(days_inyear(ts%nyears))
        allocate(ts%years(ts%nyears))
        allocate(ts%daysINyears(ts%nyears))
        allocate(ts%daysINseasons(12))

        nr_days = 0
        do i = 1, ts%nyears
            ts%years(i) = year
            days_inyear(i) = leap_year(year)
            year = year + 1
            if (i == 1) then
                nr_days = days_inyear(i) - ic%start%jday + 1
            else if (i == ts%nyears) then
                nr_days = nr_days + (days_inyear(i) - (days_inyear(i) - ic%stop%jday))
            else
                nr_days = nr_days + days_inyear(i)
            end if
        end do

        ts%nr_days = nr_days
        allocate(ts%dates(nr_days, 4))

        ts%nr_timeStep = ts%nr_days*48

!        hours= stepclim/30
!        ts%nr_timeStepClimF = ts%nr_days*48/hours

        year = ic%start%year
        jday = ic%start%jday
        do i = 1, nr_days
            ts%dates(i, 1) = year
            ts%dates(i, 4) = jday
            call Julian2MonthDay(jday, year, ts%dates(i, 2), ts%dates(i, 3))
            if ((leap_year(year) == 365) .and. (jday == 365)) then
                year = year + 1
                jday = 0
            else if ((leap_year(year) == 366) .and. (jday == 366)) then
                year = year + 1
                jday = 0
            end if
            jday = jday + 1
        end do

        call get_nr_months(ts)
        call get_nrDaysInUpFreq(ts)

    end subroutine !get_dates

    !> *****************************************************************
    !> Description: Get the number of days in months, seasons, years for
    !> the period of simulation
    !> *****************************************************************
    subroutine get_nrDaysInUpFreq(ts)

        !>Input
        type(dates_model) :: ts

        !> Internals
        integer i, i_year, i_month, i_season, j
        integer, dimension(:), allocatable :: days, days2

        allocate(days(ts%nr_days))

        !> Season
        days  = 0
        do i = 1, 12
            where(ts%dates(:, 2) == i) days = 1
            ts%daysINseasons(i) = sum(days)
            days = 0
        end do

        !> Year
        days = 0
        j = 1
        do i = ic%start%year, ic%stop%year
            where(ts%dates(:, 1) == i) days = 1
            ts%daysINyears(j) = sum(days)
            j = j + 1
            days = 0
        end do

        !> Months
        days = 0
        allocate(days2(ts%nr_days))
        days2 = 0
        do j = 1, ts%nmonths
            where(ts%dates(:, 1) == ts%mnthyears(j, 1)) days = 1
            where(ts%dates(:, 2) == ts%mnthyears(j, 2)) days2 = 1
            days = days*days2
            ts%daysINmonths(j) = sum(days)
            days = 0
            days2 = 0
        end do

        deallocate(days, days2)

    end subroutine !get_nrDaysInUpFreq

    !> *****************************************************************
    !> Description: Convert Julian day to month and day in gregorian
    !> calendar given the Julian day and the year
    !> *****************************************************************
    subroutine Julian2MonthDay(jday,year,month,day)

        !> Input
        integer, intent(in) :: jday, year

        !> Output
        integer, intent(out) :: month, day

        !> Internals
        integer, parameter, dimension(12) :: &
            daysnl = [31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365], &
            daysyl = [31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366]

        integer i, int_i, int_f

        do i = 2, 12

            if (leap_year(year) == 365) then
                int_i = daysnl(i - 1)
                int_f = daysnl(i)
            elseif (leap_year(year) == 366) then
                int_i = daysyl(i - 1)
                int_f = daysyl(i)
            end if

            if (jday <= 31) then
                month = 1
                day = jday
                exit
            else
                if ((jday > int_i) .and. (jday <= int_f)) then
                    month = i
                    day = jday - int_i
                    exit
                elseif (jday == int_i) then
                    month = i - 1
                    if (leap_year(year) == 365) then
                        day = jday - daysnl(i - 1)
                        exit
                    elseif (leap_year(year) == 366) then
                        day = jday - daysyl(i - 1)
                        exit
                    end if
                end if
            end if
        end do

    end subroutine !Julian2MonthDay

    !> *****************************************************************
    !> Description: Get the number of days in leap and normal years
    !> (365 or 366).
    !> *****************************************************************
    integer function leap_year(y) result(ndays)

        logical is_leap
        integer, intent(in) :: y

        is_leap = (mod(y, 4) == 0 .and. .not. mod(y, 100) == 0) .or. (mod(y, 400) == 0)

        if (is_leap) then
            ndays = 366
        else
            ndays = 365
        end if

    end function !leap_year

    !> *****************************************************************
    !> Description: Get the number of months
    !> *****************************************************************
    subroutine get_nr_months(ts)

        !> Input/Output
        type(dates_model) :: ts

        !> Internal
        integer i, mth, n_months, yr

        n_months = 1

        do i = 1, ts%nr_days - 1
            if (ts%dates(i, 2) /= ts%dates(i + 1, 2)) then
                n_months = n_months + 1
            end if
        end do

        ts%nmonths = n_months
        allocate(ts%mnthyears(n_months, 2))

        mth = ts%dates(1, 2)
        yr = ts%dates(1, 1)

        do i = 1, ts%nmonths
            ts%mnthyears(i, 1) = yr
            ts%mnthyears(i, 2) = mth
            mth = mth + 1
            if (mth == 13) then
                mth = 1
                yr = yr + 1
            end if
        end do

        allocate(ts%daysInmonths(ts%nmonths))

    end subroutine !get_nr_months

    !> *****************************************************************
    !> Description: Get the year, month, and season indices from Julian
    !> day and year
    !> *****************************************************************
    subroutine GetIndicesDATES(iday,iyear,iy,im,iss, id,ts)

        !> Inputs
        type(dates_model) :: ts
        integer, intent(in) :: iday, iyear

        !> Outputs
        integer, intent(out) :: iy, im, iss, id

        !> Internals
        integer day, i

        iy = 0
        id = iday
        if (iyear == ic%start%year) id = iday - ic%start%jday + 1
        do i = ic%start%year, ic%stop%year
            iy = iy + 1
            if (i == iyear) then
                exit
            end if
            id = id + ts%daysinyears(i - ic%start%year + 1)
        end do

        call Julian2MonthDay(iday, iyear, iss, day)

        do i = 1, ts%nmonths
            if (ts%mnthyears(i, 1) == iyear .and. ts%mnthyears(i, 2) == iss) then
                im = i
                exit
            end if
        end do

    end subroutine !GetIndicesDATES

end module !model_dates
