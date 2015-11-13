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

    type iter_counter

        integer &
            now_year, now_jday, now_month, now_day, &
            now_hour, now_mins, &
            dts, &
            count_year, count_jday, count_month, count_day, &
            count_hour, count_mins, &

            !* ts_daily: Count of the current time-step. Resets daily.
            !* ts_hourly: Count of the current time-step. Resets hourly.
            !* ts_halfhourly: Count of the current time-step. Resets every hour or half-past the hour.
            !* ts_count: Cummulative count of the time-step.
            ts_daily, ts_hourly, ts_halfhourly, ts_count

        contains

!        procedure :: init => init_iter_counter
!        procedure :: update_now => update_now_iter_counter

    end type

    !> *****************************************************************
    !> Type: counter_date_julian
    !> Description: Object to contain a Julian date, with hour and
    !> minute interval.
    !> *****************************************************************
    type counter_date_julian

        integer &

            !> Year.
            year, &

            !> Julian day.
            jday, &

            !> Hour.
            hour, &

            !> Minute interval.
            mins

    end type !counter_date

    !* TIME_STEP_NOW: Current time-step in minutes.
    !* TIME_STEP_MINS: Time-step of the model in minutes.
    !* TIME_STEP_DELT: Time-step of the model in seconds.
    integer YEAR_NOW, JDAY_NOW, HOUR_NOW, TIME_STEP_NOW, MINS_NOW
    integer :: TIME_STEP_MINS = 30, TIME_STEP_DELT = 1800

    !* YEAR_START: Year at the start of the simulation.
    !* JDAY_START: Julian day at the start of the simulation.
    !* HOUR_START: Hour at the start of the simulation.
    !* MINS_START: Minute (in 30-min. increment; either 0 or 30) at the start of the simulation.
    integer YEAR_START, JDAY_START, HOUR_START, MINS_START
    integer YEAR_STOP, JDAY_STOP, HOUR_STOP, MINS_STOP

    contains

    subroutine init_iter_counter(ic, start_year, start_day_julian, start_hour, start_mins, timestep)

        !> Derived-type variable.
        type(iter_counter) :: ic

        !> Input variables.
        !* timestep: Time-step of the model iteration [s].
        integer, intent(in) :: start_year, start_day_julian, start_hour, start_mins, timestep

        !> Update now counters.
        ic%now_year = start_year
        ic%now_jday = start_day_julian
        ic%now_hour = start_hour
        ic%now_mins = start_mins
        call julian2monthday(start_day_julian, start_year, ic%now_month, ic%now_day)

        !> Update time-step.
        TIME_STEP_DELT = timestep
        ic%dts = timestep

        !> Initialize counters.
        ic%count_year = 0
        ic%count_jday = 0
        ic%count_month = 0
        ic%count_day = 0
        ic%count_hour = 0
        ic%count_mins = 0
        ic%ts_daily = 1
        ic%ts_hourly = 1
        ic%ts_halfhourly = 1
        ic%ts_count = 1

    end subroutine

    subroutine update_now_iter_counter(ic, now_year, now_jday, now_hour, now_mins)

        !> Derived-type variable.
        type(iter_counter) :: ic

        !> Input variables.
        integer, intent(in) :: now_year, now_jday, now_hour, now_mins

        !> Local variables.
        integer :: month, day

        !> Year:
        if (now_year /= ic%now_year) then
            ic%count_year = ic%count_year + 1
            ic%now_year = now_year
        end if

        !> Julian day:
        if (now_jday /= ic%now_jday) then
            ic%count_jday = ic%count_jday + 1
            ic%now_jday = now_jday
        end if

        !> Determine the current month and day.
        call julian2monthday(now_jday, now_year, month, day)

        !> Month:
        if (month /= ic%now_month) then
            ic%count_month = ic%count_month + 1
            ic%now_month = month
        end if

        !> Day:
        if (day /= ic%now_day) then
            ic%count_day = ic%count_day + 1
            ic%now_day = day
            ic%ts_daily = 0
        end if

        !> Hourly:
        if (now_hour /= ic%now_hour) then
            ic%count_hour = ic%count_hour + 1
            ic%now_hour = now_hour
            ic%ts_hourly = 0
        end if

        !> Minutes:
        if (now_mins /= ic%now_mins) then
            ic%count_mins = ic%count_mins + 1
            ic%now_mins = now_mins
            if (ic%now_mins == 0 .or. ic%now_mins == 30) then
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
        ts%nyears = YEAR_STOP - YEAR_START + 1
        ts%nseason = 12

!        ts%start_date = start_date
!        ts%end_date = end_date

        iyear = YEAR_START
        fyear = YEAR_STOP
        year = YEAR_START

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
                nr_days = days_inyear(i) - JDAY_START + 1
            else if (i == ts%nyears) then
                nr_days = nr_days + (days_inyear(i) - (days_inyear(i) - JDAY_STOP))
            else
                nr_days = nr_days + days_inyear(i)
            end if
        end do

        ts%nr_days = nr_days
        allocate(ts%dates(nr_days, 4))

        ts%nr_timeStep = ts%nr_days*48

!        hours= stepclim/30
!        ts%nr_timeStepClimF = ts%nr_days*48/hours

        year = YEAR_START
        jday = JDAY_START
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
        do i = YEAR_START, YEAR_STOP
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
        if (iyear == YEAR_START) id = iday - JDAY_START + 1
        do i = YEAR_START, YEAR_STOP
            iy = iy + 1
            if (i == iyear) then
                exit
            end if
            id = id + ts%daysinyears(i - YEAR_START + 1)
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
