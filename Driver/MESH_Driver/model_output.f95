        module model_output

        !>******************************************************************************
        !>  Athor: Gonzalo Sapriza Azuri
        !>******************************************************************************
            !use EF_Module, only:ShedParam

            TYPE out_var

                ! Component of the water balance
                real, dimension(:,:), allocatable :: prec_y, prec_m, prec_s !Precipitation
                real, dimension(:,:), allocatable :: evap_y, evap_m, evap_s !Evaporation
                real, dimension(:,:), allocatable :: roff_y, roff_m, roff_s !Runoff
                real, dimension(:,:), allocatable :: dstg_y, dstg_m, dstg_s !Delta Storage

                ! State Variables soil
                real, dimension(:,:,:), allocatable :: tbar_y, tbar_m, tbar_s !Temperature in the soil layers
                real, dimension(:,:,:), allocatable :: thlq_y, thlq_m, thlq_s !Liquid content in the soil layer
                real, dimension(:,:,:), allocatable :: thic_y, thic_m, thic_s !Ice content in the soil layer

            END TYPE
        !>******************************************************************************
            TYPE dates_model

                integer,dimension(:,:), allocatable :: dates         !year,month,day,julian day
                integer,dimension(:)  , allocatable :: years         !array of years in annual freq
                integer,dimension(:,:), allocatable :: mnthyears     !array of month,year in month freq
                integer,dimension(:)  , allocatable :: daysINyears   !nr days in years
                integer,dimension(:)  , allocatable :: daysINmonths  !nr days in months
                integer,dimension(:)  , allocatable :: daysINseasons !nr days in Seasons
                integer   :: start_date(2), end_date(2)              !start_date,end_date
                character :: freq                                    !frequency
                integer   :: nyears                                  !number oof years
                integer   :: nseason                                 !12 months
                integer   :: nmonths                                 !number of months
                integer   :: nr_days                                 !number of days

            END TYPE
        !>******************************************************************************
            TYPE info_out

                character*450  :: flIn                                      !file that contain the input information of the variables that we want to init and the frequency
                character*450  :: pthOut                                    !path out
                character*20, dimension(:,:),allocatable :: ids_var_out     !array that contain the ids of the files and frequency ('PREPC','Y','M','S','CUM','SEQ')
                integer  :: nr_out                                          !number of output variables

            END TYPE
        !>******************************************************************************


            contains
        !>******************************************************************************
            subroutine Init_out(vr,ts,ifo,na,ignd)

            implicit none

            !Inputs
            integer :: na,ignd

            !Inputs-Output
            type(out_var)     :: vr
            type(dates_model) :: ts
            type(info_out)    :: ifo

            !Internals
            integer   :: IOS,i,j
            character*50 :: vId


        !>--------------Main Subtrouine start-----------------------------------------------

            OPEN(UNIT   = 901                   , &
                 FILE   = 'outputs_balance.txt' , &
                 STATUS = 'old'                 , &
                 ACTION = 'read'                , &
                 IOSTAT = IOS                     )

            ifo%flIn = 'outputs_balance.txt'

            read(901,*) ifo%pthOut
            read(901,*) ifo%nr_out

            allocate(ifo%ids_var_out(ifo%nr_out,6))

            do i = 1, ifo%nr_out

                read(901,*)(ifo%ids_var_out(i,j),j=1,6)

                vId = trim(adjustl(ifo%ids_var_out(i,1)))

                select case (vId)

                    case ('PREC','Rainfall','Rain','Precipitation')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            allocate(vr%prec_y(na,ts%nyears))
                            vr%prec_y = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            allocate(vr%prec_m(na,ts%nmonths))
                            vr%prec_m = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            allocate(vr%prec_s(na,ts%nseason))
                            vr%prec_s = 0.0e0

                        endif

                    case ('EVAP','Evapotranspiration')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            allocate(vr%evap_y(na,ts%nyears))
                            vr%evap_y = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            allocate(vr%evap_m(na,ts%nmonths))
                            vr%evap_m = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            allocate(vr%evap_s(na,ts%nseason))
                            vr%evap_s = 0.0e0

                        endif

                    case ('ROFF','Runoff')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            allocate(vr%roff_y(na,ts%nyears))
                            vr%roff_y = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            allocate(vr%roff_m(na,ts%nmonths))
                            vr%roff_m = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            allocate(vr%roff_s(na,ts%nseason))
                            vr%roff_s = 0.0e0

                        endif

                    case ('DeltaStorage','DSTG')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            allocate(vr%dstg_y(na,ts%nyears))
                            vr%dstg_y = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            allocate(vr%dstg_m(na,ts%nmonths))
                            vr%dstg_y = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            allocate(vr%dstg_s(na,ts%nseason))
                            vr%dstg_s = 0.0e0

                        endif

                    case ('TempSoil','Temperature_soil_layers','TBAR')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            allocate(vr%tbar_y(na,ignd,ts%nyears))
                            vr%tbar_y = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            allocate(vr%tbar_m(na,ignd,ts%nmonths))
                            vr%tbar_m = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            allocate(vr%tbar_s(na,ignd,ts%nseason))
                            vr%tbar_s = 0.0e0

                        endif

                    case ('THLQ','LiquidContent_soil_layers')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            allocate(vr%thlq_y(na,ignd,ts%nyears))
                            vr%thlq_y = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            allocate(vr%thlq_m(na,ignd,ts%nmonths))
                            vr%thlq_m = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            allocate(vr%thlq_s(na,ignd,ts%nseason))
                            vr%thlq_s = 0.0d0

                        endif

                    case ('THIC','ICEContent_soil_layers')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            allocate(vr%thic_y(na,ignd,ts%nyears))
                            vr%thic_y = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            allocate(vr%thic_m(na,ignd,ts%nmonths))
                            vr%thic_m = 0.0e0

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            allocate(vr%thic_s(na,ignd,ts%nseason))
                            vr%thic_s = 0.0e0

                        endif

                    case default

                        print*, vID, 'Not Implemented yet'

                end select
            enddo

            close(901)

            end subroutine Init_out
        !>******************************************************************************
            subroutine get_dates(ts,start_date,end_date)

        !>------------------------------------------------------------------------------
        !>  Description: Get date_time array (YYYY,MM,DD) based on the start and end date
        !>
        !>------------------------------------------------------------------------------

            implicit none
            !Input
            integer :: start_date(2),end_date(2) !Year,julianday,hour,min
            !Input-Output
            type(dates_model) :: ts
            !Internal variables
            integer :: nr_years
            integer,allocatable :: days_inyear(:)
            integer :: jday,year,i,iyear,fyear
            integer :: nr_days

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

            end subroutine
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
            subroutine UpdateFIELDSOUT(vr    ,  ts   , ifo           , &
                                       precp ,  evap , roff , dstg   , &
                                       tbar  ,  thlq , thic          , &
                                       na    ,  ignd                 , &
                                       IDAY  ,  IYEAR                )
        !>------------------------------------------------------------------------------
        !>  Description: Update values in each time step
        !>------------------------------------------------------------------------------

            implicit none

            !Inputs
            integer                 :: na,ignd
            integer                 :: iday,iyear
            type(dates_model)       :: ts
            type(info_out)          :: ifo

            real,dimension(na)      :: precp , evap , roff , dstg
            real,dimension(na,ignd) :: tbar  , thlq , thic

            !Inputs-Output
            type(out_var)      :: vr

            !Internals
            integer       :: i, iy, im, iss
            character*50  :: vId

            call GetIndicesDATES(IDAY,IYEAR,iy,im,iss,ts)

            do i = 1, ifo%nr_out

                vId = trim(adjustl(ifo%ids_var_out(i,1)))

                select case (vId)

                    case ('PREC','Rainfall','Rain','Precipitation')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            vr%prec_y(:,iy) = vr%prec_y(:,iy) + precp

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            vr%prec_m(:,im) = vr%prec_m(:,im) + precp

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            vr%prec_s(:,iss) = vr%prec_s(:,iss)  + precp

                        endif

                    case ('EVAP','Evapotranspiration')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            vr%evap_y(:,iy) = vr%evap_y(:,iy) + evap

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            vr%evap_m(:,im) = vr%evap_m(:,im) + evap

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            vr%evap_s(:,iss) = vr%evap_s(:,iss) + evap

                        endif

                    case ('ROFF','Runoff')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            vr%roff_y(:,iy) = vr%roff_y(:,iy)  + roff

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            vr%roff_m(:,im) = vr%roff_m(:,im) + roff

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            vr%roff_s(:,iss) = vr%roff_s(:,iss) + roff

                        endif

                    case ('DeltaStorage','DSTG')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            vr%dstg_y(:,iy) = vr%dstg_y(:,iy) + dstg

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            vr%dstg_m(:,im) =  vr%dstg_m(:,im) + dstg

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            vr%dstg_s(:,iss) = vr%dstg_s(:,iss) + dstg

                        endif

                    case ('TempSoil','Temperature_soil_layers','TBAR')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            vr%tbar_y(:,:,iy) = vr%tbar_y(:,:,iy) + tbar

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            vr%tbar_m(:,:,im) = vr%tbar_m(:,:,im) + tbar

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            vr%tbar_s(:,:,iss) = vr%tbar_s(:,:,iss) + tbar

                        endif

                    case ('THLQ','LiquidContent_soil_layers')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            vr%thlq_y(:,:,iy) = vr%thlq_y(:,:,iy) + thlq

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            vr%thlq_m(:,:,im) = vr%thlq_m(:,:,im) + thlq

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            vr%thlq_s(:,:,iss) = vr%thlq_s(:,:,iss) + thlq

                        endif

                    case ('THIC','ICEContent_soil_layers')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            vr%thic_y(:,:,iy) = vr%thic_y(:,:,iy) + thic

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            vr%thic_m(:,:,im) = vr%thic_m(:,:,im) + thic

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            vr%thic_s(:,:,iss) = vr%thic_s(:,:,iss) + thic

                        endif

                    case default

                        print*, vID, 'Not Implemented yet'

                end select
            enddo

            end subroutine UpdateFIELDSOUT
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
            subroutine Write_Outputs(vr,ts,ifo)
        !>------------------------------------------------------------------------------
        !>  Description: Loop over the variablaes to write
        !>  output balance's fields in selected format
        !>------------------------------------------------------------------------------
            implicit none
            !Inputs
            type(out_var)      :: vr
            type(info_out)     :: ifo
            type(dates_model)  :: ts

            !Outputs
            !Files

            !Internals
            integer       :: i,nai(3),na,j
            character*50  :: vId
            character*1   :: st

            nai = shape(vr%tbar_y)
            na = nai(1)

            do i = 1, ifo%nr_out

                vId = trim(adjustl(ifo%ids_var_out(i,1)))

                select case (vId)

                    case ('PREC','Rainfall','Rain','Precipitation')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            call WriteFields_i(vr%prec_y, ts, ifo, i, 'Y', NA, ts%nyears)

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            call WriteFields_i(vr%prec_m, ts, ifo, i,  'M', NA, ts%nmonths)

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            call WriteFields_i(vr%prec_s, ts, ifo, i,  'S', NA, ts%nseason)

                        endif

                    case ('EVAP','Evapotranspiration')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            call WriteFields_i(vr%evap_y, ts, ifo, i, 'Y', na, ts%nyears)

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            call WriteFields_i(vr%evap_m, ts, ifo, i,  'M',na, ts%nmonths)

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            call WriteFields_i(vr%evap_s, ts, ifo, i,  'S', na, ts%nseason)

                        endif

                    case ('ROFF','Runoff')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            call WriteFields_i(vr%roff_y, ts, ifo, i, 'Y', na, ts%nyears)

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            call WriteFields_i(vr%roff_m, ts, ifo, i,  'M', na, ts%nmonths)

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            call WriteFields_i(vr%roff_s, ts, ifo, i,  'S',na, ts%nseason)

                        endif

                    case ('DeltaStorage','DSTG')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then

                            call WriteFields_i(vr%dstg_y, ts, ifo, i, 'Y', na, ts%nyears)

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then

                            call WriteFields_i(vr%dstg_m, ts, ifo, i,  'M', na, ts%nmonths)

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            call WriteFields_i(vr%dstg_s, ts, ifo, i,  'S',na, ts%nseason)

                        endif

                    case ('TempSoil','Temperature_soil_layers','TBAR')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then
                            do j = 1, nai(2)
                                write(unit=st, fmt='(I1)') j
                                call WriteFields_i(vr%tbar_y(:,j,:), ts, ifo, i, 'Y',na, ts%nyears,st)
                            enddo
                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then
                            do j = 1, nai(2)
                                write(unit=st, fmt='(I1)') j
                                call WriteFields_i(vr%tbar_m(:,j,:), ts, ifo, i,  'M', na, ts%nmonths,st)
                            enddo
                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then
                            do j = 1, nai(2)
                                write(unit=st, fmt='(I1)') j
                                call WriteFields_i(vr%tbar_s(:,j,:), ts, ifo, i,  'S', na, ts%nseason, st)
                            enddo
                        endif

                    case ('THLQ','LiquidContent_soil_layers')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then
                            do j = 1, nai(2)
                                write(unit=st, fmt='(I1)') j
                                call WriteFields_i(vr%thlq_y(:,j,:), ts, ifo, i, 'Y',na, ts%nyears,st)
                            enddo

                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then
                            do j = 1, nai(2)
                                write(unit=st, fmt='(I1)') j
                                call WriteFields_i(vr%thlq_m(:,j,:), ts, ifo, i,  'M', na, ts%nmonths,st)
                            enddo
                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then
                            do j = 1, nai(2)
                                write(unit=st, fmt='(I1)') j
                                call WriteFields_i(vr%thlq_s(:,j,:), ts, ifo, i,  'S', na, ts%nseason,st)
                            enddo
                        endif

                    case ('THIC','ICEContent_soil_layers')

                        if (trim(adjustl(ifo%ids_var_out(i,2))).eq.'Y')then
                            do j = 1, nai(2)
                                write(unit=st, fmt='(I1)') j
                                call WriteFields_i(vr%thic_y(:,j,:), ts, ifo, i, 'Y',na, ts%nyears,st)
                            enddo
                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,3))).eq.'M')then
                            do j = 1, nai(2)
                                write(unit=st, fmt='(I1)') j
                                call WriteFields_i(vr%thic_m(:,j,:), ts, ifo, i,  'M' , NA, ts%nmonths,st)
                            enddo
                        endif

                        if (trim(adjustl(ifo%ids_var_out(i,4))).eq.'S')then

                            do j = 1, nai(2)
                                write(unit=st, fmt='(I1)') j
                                call WriteFields_i(vr%thic_s(:,j,:), ts, ifo, i,  'S' , NA, ts%nseason,st)
                            enddo
                        endif

                    case default

                        print*, vID, 'Not Implemented yet'

                end select
            enddo

            end subroutine Write_Outputs

         !>******************************************************************************
            subroutine WriteFields_i(fld, ts, ifo, indx, freq, na, nt, st)
        !>------------------------------------------------------------------------------
        !>  Description: Loop over the variablaes to write
        !>  output balance's fields in selected format
        !>------------------------------------------------------------------------------

            implicit none

            !Inputs
            type(info_out)     :: ifo
            type(dates_model)  :: ts
            integer            :: indx
            character*1        :: freq
            real               :: fld(na,nt)
            integer            :: na, nt
            character*1,intent(in),optional :: st

            !Internals
            integer       :: i,nr
            character*50  :: vId,tfunc
            integer,dimension(:), allocatable :: days
            character*3   :: freq2

            integer,dimension(:,:),allocatable :: dates

            vId = trim(adjustl(ifo%ids_var_out(indx,6)))
            tfunc = trim(adjustl(ifo%ids_var_out(indx,5)))

            if (tfunc .eq. 'AVG') then

                allocate(days(nt))

                if     (freq .eq. 'Y') then

                    days = ts%daysINyears

                elseif (freq .eq. 'M') then

                    days = ts%daysINmonths

                elseif (freq .eq. 'S') then

                    days = ts%daysINseasons

                endif

                do i = 1, nt

                    fld(:,i) = fld(:,i)/days(i)

                enddo

                deallocate(days)

            endif

            if     (freq .eq. 'Y') then

                allocate(dates(ts%nyears,2))
                dates(:,1) = ts%years
                dates(:,2) = 1

            elseif (freq .eq. 'M') then

                allocate(dates(ts%NMONTHS,2))
                dates = ts%mnthyears

            elseif (freq .eq. 'S') then

                allocate(dates(ts%nseason,2))
                do i =1,12
                    dates(i,1) = ts%years(1)
                    dates(i,2) = i
                enddo

            endif



            if (present(st))then
                freq2 = freq//'_'//st
            else
                freq2 = freq
            endif


            select case (vId)

                case('seq','binseq')

                    call WriteSeq(fld,indx,ifo,freq2,dates)

                case('r2c')

                    call WriteR2C(fld,indx,ifo,freq2,dates)

                case default

                    print*, 'Not implemented yet'
            end select


            end subroutine WriteFields_i
        !>******************************************************************************

        !>******************************************************************************
            subroutine WriteSeq(fld , indx,info, freq,dates)
        !>------------------------------------------------------------------------------
        !>  Description: Write bin sequential file
        !>
        !>------------------------------------------------------------------------------

            implicit none
            !Inputs

            real                :: fld(:,:)
            integer             :: indx
            character*3         :: freq
            integer             :: dates(:,:)
            type(info_out)      :: info

            !Internal
            character*450  :: flOut
            integer        :: ios,i
            integer             :: na, nt





            flOut = trim(adjustl(info%pthOut))//'\'//         &
                    trim(adjustl(info%ids_var_out(indx,1)))// &
                    '_'//trim(adjustl(freq))//'.seq'

            OPEN(UNIT   = 882                      , &
                 FILE    = trim(adjustl(flOut))    , &
                 STATUS  = 'replace'               , &
                 FORM    = 'unformatted'           , &
                 ACTION  = 'write'                 , &
                 ACCESS  = 'sequential'            , &
                 IOSTAT  = IOS                     )

            do i = 1, nt

                write(882) i
                write(882) fld(:,i)

            enddo

            close(882)

            end subroutine WriteSeq
        !>******************************************************************************
            subroutine WriteR2C(fld,indx,info, freq,dates)
        !>------------------------------------------------------------------------------
        !>  Description: Write r2c file
        !>
        !>------------------------------------------------------------------------------
            use area_watflood

            implicit none
            !Inputs
            real                :: fld(:,:)
            integer             :: indx
            type(info_out)      :: info
            character*3         :: freq
            integer             :: dates(:,:)


                        !Internal

            character*450                   :: flOut
            integer                         :: ios,i,un
            integer                         :: na1, nt,j,t,k
            real,dimension(:,:),allocatable :: data_aux
            character(10)                   :: ctime
            character(8)                    :: cday

            flOut = trim(adjustl(info%pthOut))//'\'// &
                    trim(adjustl(info%ids_var_out(indx,1)))// &
                    '_'//trim(adjustl(freq))//'.r2c'

            un  = 882

            OPEN(UNIT    = un                      , &
                 FILE    = trim(adjustl(flOut))    , &
                 STATUS  = 'replace'               , &
                 FORM    = 'formatted'             , &
                 ACTION  = 'write'                 , &
                 IOSTAT  = IOS                     )

            write(un,3005)'########################################'
            write(un,3005)':FileType r2c  ASCII  EnSim 1.0         '
            write(un,3005)'#                                       '
            write(un,3005)'# DataType               2D Rect Cell   '
            write(un,3005)'#                                       '
            write(un,3005)':Application               MeshOutput   '
            write(un,3005)':Version                 1.0.00         '
            write(un,3020)':WrittenBy          ','MESH_DRIVER                             '

            call date_and_time(cday,ctime)

            write(un,3010)':CreationDate       ', &
                            cday(1:4),cday(5:6),cday(7:8),ctime(1:2),ctime(3:4)

            write(un,3005)'#                                       '
            write(un,3005)'#---------------------------------------'
            write(un,3005)'#                                       '
            write(un,3020)':Name               ',info%ids_var_out(indx,1)
            write(un,3005)'#                                       '
            write(un,3004)':Projection         ',coordsys1

            if(coordsys1 .eq. 'LATLONG   ')then

                write(un,3004)':Ellipsoid          ',datum1

            endif

            if(coordsys1.eq.'UTM       ')then

                write(un,3004)':Ellipsoid          ',datum1
                write(un,3004)':Zone               ',zone1

            endif

            write(un,3005)'#                                       '
            write(un,3003)':xOrigin            ',xorigin
            write(un,3003)':yOrigin            ',yorigin
            write(un,3005)'#                                       '
            write(un,3005)':SourceFile            standalone MESH  '
            write(un,3005)'#                                       '

            write(un,*)':AttributeName',info%ids_var_out(indx,1)

            write(un,3020)':AttributeUnits     ',info%ids_var_out(indx,2)
            write(un,3005)'#                                       '
            write(un,3001)':xCount             ',xCount
            write(un,3001)':yCount             ',ycount
            write(un,3003)':xDelta             ',xdelta
            write(un,3003)':yDelta             ',yDelta
            write(un,3005)'#                                       '
            write(un,3005)'#                                       '
            write(un,3005)':endHeader                              '

            nt = size(dates(:,1))

            do t = 1, nt

                write(un,9000)':Frame',t,t,dates(t,1),dates(t,2),1,0,0

                allocate(data_aux(ycount,xcount))
                data_aux = 0.0e0

                do k = 1, na

                    data_aux(yyy(k),xxx(k)) = fld(k,t)

                enddo

                do j = 1, ycount

                    WRITE(un,'(999(E12.6,2x))') (data_aux(j,i),i=1,xcount)

                enddo

                write(un,'(A)')':EndFrame'

                deallocate(data_aux)

            enddo

            close(882)

3000 format(a10,i5)
3001 format(a20,i16)
3002 format(2a20)
3003 format(a20,f16.7)
3004 format(a20,a10,2x,a10)
3005 format(a40)
3006 format(a3,a10)
3007 format(a14,i5,a6,i5)
3010 format(a20,a4,'-',a2,'-',a2,2x,a2,':',a2)
3012 format(a9)
3020 format(a20,a40)
9000 FORMAT(A6,2I10,3X,'"',I4,'/',I2.2,'/',I2.2,1X,I2.2,':',I2.2,':00.000"')

            end subroutine WriteR2C



        end module model_output
