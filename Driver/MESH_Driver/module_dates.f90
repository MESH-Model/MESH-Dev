         module model_dates

        !>******************************************************************************
        !>  Athor: Gonzalo Sapriza Azuri
        !>  Description: handled time steps and dates in mesh
        !>******************************************************************************
            TYPE dates_model

                integer,dimension(:,:), allocatable :: dates         !year,month,day,julian day
                integer,dimension(:)  , allocatable :: years         !array of years in annual freq
                integer,dimension(:,:), allocatable :: mnthyears     !array of month,year in month freq
                integer,dimension(:)  , allocatable :: daysINyears   !nr days in years
                integer,dimension(:)  , allocatable :: daysINmonths  !nr days in months
                integer,dimension(:)  , allocatable :: daysINseasons !nr days in Seasons
                integer   :: start_date(4), end_date(4)              !start_date,end_date
                character :: freq                                    !frequency
                integer   :: nyears                                  !number oof years
                integer   :: nseason                                 !12 months
                integer   :: nmonths                                 !number of months
                integer   :: nr_days                                 !number of days
                integer   :: nr_timeStep                             !total number of time steps
                integer   :: nr_timeStepClimF                        !total number of climate forcing time steps

            END TYPE
            

            contains
        !>******************************************************************************
            subroutine get_dates(ts,start_date,end_date, stepclim)

        !>------------------------------------------------------------------------------
        !>  Description: Handled dates in the model
        !>
        !>------------------------------------------------------------------------------

            implicit none
            !Input
            integer,intent(in) :: start_date(4),end_date(4) !Year,julianday,hour,min
            integer,intent(in) :: stepclim                  !HOURLYFLAG
            !Input-Output
            type(dates_model),intent(inout) :: ts
            !Internal variables
            integer :: nr_years
            integer,allocatable :: days_inyear(:)
            integer :: jday,year,i,iyear,fyear
            integer :: nr_days, hours

            ts%freq = 'D'
            ts%nyears = end_date(1) - start_date(1) + 1
            ts%nseason = 12

            ts%start_date = start_date
            ts%end_date = end_date

            iyear = start_date(1)
            fyear = end_date(1)

            year = start_date(1)

            allocate(days_inyear(ts%nyears))
            allocate(ts%years(ts%nyears))
            allocate(ts%daysINyears(ts%nyears))
            allocate(ts%daysINseasons(12))

            do i = 1, ts%nyears

                ts%years(i) = year
                days_inyear(i) = leap_year(year)
                year = year + 1

            enddo


            nr_days = days_inyear(1) - start_date(2)   + &
                      sum(days_inyear(2:ts%nyears-1)) + &
                      end_date(2) + 1

            ts%nr_days = nr_days

            allocate(ts%dates(nr_days,4))

            ts%nr_timeStep = ts%nr_days*48

            hours= stepclim/30
            ts%nr_timeStepClimF = ts%nr_days*48/hours

            year = start_date(1)
            jday = start_date(2)

            do i = 1, nr_days

                ts%dates(i,1) = year
                ts%dates(i,4) = jday

                call Julian2MonthDay(jday, year,ts%dates(i,2), ts%dates(i,3))

               if ((leap_year(year) .eq. 365)  .and. &
                   (jday .eq. 365)) then

                   year = year + 1
                   jday = 0

               elseif ((leap_year(year) .eq. 366)  .and. &
                       (jday   .eq. 366)) then

                   year = year + 1
                   jday = 0

               endif

               jday = jday + 1

            enddo

            call get_nr_months(ts)
            call get_nrDaysInUpFreq(ts)

            end subroutine get_dates

        !>******************************************************************************
            subroutine get_nrDaysInUpFreq(ts)

        !>------------------------------------------------------------------------------
        !>  Description: Get the number of days in months, seasons, years for
        !>               the period of simulation
        !>------------------------------------------------------------------------------

            implicit none
            !Input
            type(dates_model) :: ts

            !Internals
            integer :: i,i_year,i_month, i_season,j
            integer, dimension(:), allocatable :: days,days2

         !>------------------------------------------------------------------------------

            allocate(days(ts%nr_days))
            days  = 0
            !Season
            do i = 1, 12

                where(ts%dates(:,2).eq.i) days = 1
                ts%daysINseasons(i) = sum(days)
                days = 0

            enddo

            days = 0

            !Year
            j = 1
            do i = ts%start_date(1), ts%end_date(1)

                where(ts%dates(:,1).eq.i) days = 1
                ts%daysINyears(j) = sum(days)
                j = j + 1
                days = 0

            enddo

            days = 0
            allocate(days2(ts%nr_days))
            days2 = 0

            !Months
            do j = 1, ts%nmonths

                where(ts%dates(:,1).eq.ts%mnthyears(j,1)) days  = 1
                where(ts%dates(:,2).eq.ts%mnthyears(j,2)) days2 = 1

                days = days*days2
                ts%daysINmonths(j) = sum(days)

                days  = 0
                days2 = 0
            enddo

            deallocate(days,days2)

            end subroutine get_nrDaysInUpFreq

        !>******************************************************************************
            subroutine Julian2MonthDay(jday,year,month,day)

        !>------------------------------------------------------------------------------
        !>  Description: Convert Julian day to month and day in gregorian calendar given
        !>               the julian day and the year
        !>------------------------------------------------------------------------------

            implicit none
            !Input
            integer,intent(in)  :: jday,year
            !Output
            integer,intent(out) :: month,day

            !Internals
            integer,parameter,dimension(12) ::                             &
                  daysnl = [31,59,90,120,151,181,212,243,273,304,334,365], &
                  daysyl = [31,60,91,121,152,182,213,244,274,305,335,366]

            integer  :: i,int_i,int_f

            do i = 2, 12

                if (leap_year(year).eq.365)then

                    int_i = daysnl(i-1)
                    int_f = daysnl(i)

                elseif (leap_year(year).eq.366)then

                    int_i = daysyl(i-1)
                    int_f = daysyl(i)

                endif

                if (jday.le.31)then

                    month = 1
                    day   = jday
                    exit

                else

                    if((jday.gt.int_i).and.(jday.le.int_f))then

                        month = i
                        day   = jday - int_i
                        exit

                    elseif (jday.eq.int_i) then

                        month = i -1

                        if (leap_year(year).eq.365)then

                            day   = jday - daysnl(i-1)
                            exit

                        elseif (leap_year(year).eq.366)then

                            day   = jday - daysyl(i-1)
                            exit

                        endif
                    endif
                endif
            enddo

            end subroutine Julian2MonthDay
        !>******************************************************************************
            integer function leap_year(y) result(ndays)
        !>------------------------------------------------------------------------------
        !>  Description: get the number of day in leap and normal years (3655 or 366)
        !>------------------------------------------------------------------------------
            implicit none
            logical :: is_leap
            integer,intent(in) :: y

            is_leap = (mod(y,4)==0 .and. .not. mod(y,100)==0) .or. (mod(y,400)==0)

            if (is_leap)then
                ndays = 366
            else
                ndays = 365
            endif

            end function leap_year
        !>******************************************************************************
            subroutine get_nr_months(ts)
        !>------------------------------------------------------------------------------
        !>  Description: get the number of months
        !>------------------------------------------------------------------------------
            implicit none

            !InputOutput
            type(dates_model) :: ts
            !Internal
            integer :: i,mth,n_months,yr

            n_months = 1

            do i = 1, ts%nr_days-1

                if (ts%dates(i,2).ne.ts%dates(i+1,2))then
                    n_months = n_months + 1
                endif

            enddo

            ts%nmonths = n_months
            allocate(ts%mnthyears(n_months,2))

            mth = ts%dates(1,2)
            yr  = ts%dates(1,1)

            do i = 1, ts%nmonths

                ts%mnthyears(i,1) = yr
                ts%mnthyears(i,2) = mth

                mth = mth + 1

                if (mth .eq. 13) then
                   mth = 1
                   yr = yr + 1
                endif

            enddo

            allocate(ts%daysInmonths(ts%nmonths))

            end subroutine get_nr_months


        !>******************************************************************************
        !>******************************************************************************
            subroutine GetIndicesDATES(iday,iyear,iy,im,iss,ts)
        !>------------------------------------------------------------------------------
        !>  Description: Get the year,month and season indices from julian day and year
        !>------------------------------------------------------------------------------
            implicit none
            !Inputs
            type(dates_model)    :: ts
            integer,intent(in)   :: iday,iyear
            !Outputs
            integer,intent(out)  :: iy,im,iss
            !Internals
            integer :: day,i

            iy = 0
            do i = ts%start_date(1), ts%end_date(1)
                iy = iy + 1
                if (i.eq.iyear)then
                    exit
                endif
            end do

            call Julian2MonthDay(iday,iyear,iss,day)


            do i = 1, ts%nmonths

                if((ts%mnthyears(i,1).eq.iyear) .and. &
                   (ts%mnthyears(i,2).eq.iss  ))   then
                    im = i
                    exit
                endif
            enddo

            end subroutine GetIndicesDATES
        !>******************************************************************************

         end module model_dates
