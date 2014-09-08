        module model_output

        !>******************************************************************************
        !>  Athor: Gonzalo Sapriza Azuri
        !>******************************************************************************

            use model_dates

            TYPE out_bal_intg
                ! WATER BALANCE
                real, dimension(:)  ,allocatable :: TOTAL_PRE   , TOTAL_EVAP , TOTAL_ROF
                real, dimension(:)  ,allocatable :: TOTAL_ZPND  , TOTAL_RCAN , TOTAL_SCAN
                real, dimension(:)  ,allocatable :: TOTAL_SNO   , TOTAL_STORE , DSTG
                real, dimension(:)  ,allocatable :: TOTAL_ROFO  , TOTAL_ROFS , TOTAL_ROFB
                real, dimension(:,:),allocatable :: TOTAL_THLQ  , TOTAL_THIC

                !Energy Balance
                real, dimension(:)  ,allocatable :: TOTAL_HFSACC   , TOTAL_QEVPACC
                real :: TOTAL_AREA

            END TYPE


            TYPE OUT_FLDS

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
            TYPE info_out

                character*450  :: flIn                                      !file that contain the input information of the variables that we want to init and the frequency
                character*450  :: pthOut                                    !path out
                character*20, dimension(:,:),allocatable :: ids_var_out     !array that contain the ids of the files and frequency ('PREPC','Y','M','S','CUM','SEQ')
                integer  :: nr_out                                          !number of output variables

            END TYPE
        !>******************************************************************************


            contains
        !>******************************************************************************
            subroutine Init_OutBal_Intg(bal,ts,ignd,area)
        !>------------------------------------------------------------------------------
        !>  Description: Init output water and energy balances
        !>
        !>------------------------------------------------------------------------------

            implicit none

            !Inputs
            real, intent(in)              :: area
            integer, intent(in)           :: ignd
            type(dates_model), intent(in) :: ts

            !Output
            type(out_bal_intg),intent(inout) :: bal

        !>--------------Main Subtrouine start-----------------------------------------------

            allocate(bal%TOTAL_PRE(ts%nr_days)   , bal%TOTAL_EVAP(ts%nr_days)  , &
                     bal%TOTAL_ROF(ts%nr_days)   , bal%TOTAL_ZPND(ts%nr_days)  , &
                     bal%TOTAL_RCAN(ts%nr_days)  , bal%TOTAL_SCAN(ts%nr_days)  , &
                     bal%TOTAL_SNO(ts%nr_days)   , bal%TOTAL_ROFO(ts%nr_days)  , &
                     bal%TOTAL_ROFS(ts%nr_days)  , bal%TOTAL_ROFB(ts%nr_days)  , &
                     bal%TOTAL_STORE(ts%nr_days) , bal%DSTG(ts%nr_days)        )

            allocate(bal%TOTAL_THIC(ts%nr_days,ignd)  , bal%TOTAL_THLQ(ts%nr_days,ignd))

            allocate(bal%TOTAL_HFSACC(ts%nr_days) , bal%TOTAL_QEVPACC(ts%nr_days))


            bal%TOTAL_AREA = area


            end subroutine Init_OutBal_Intg

        !>******************************************************************************
            subroutine Update_OutBal_Intg(bal     , ts    , ignd   , &
                                          pre     , evp   , rof    , &
                                          zpnd    , rcan  , scann  , &
                                          sno     , rofo  , rofs   , &
                                          rofb    , stg   , dstg   , &
                                          thic    , thlq  , hfsacc , &
                                          qevpacc ,                  &
                                          idate   , isavg , nhours   )
        !>------------------------------------------------------------------------------
        !>  Description: Update values and compute daily averages for water and
        !>  energy balance
        !>------------------------------------------------------------------------------

            implicit none
            !Input
            logical, intent(in) :: isavg
            integer, intent(in) :: idate,ignd
            integer,optional    :: nhours

            type(dates_model), intent(in) :: ts

            real, intent(in) ::  pre , evp , rof, zpnd , rcan, scann
            real, intent(in) ::  sno , rofo, rofs,rofb , stg , dstg
            real, intent(in) ::  thic(ignd),thlq(ignd)
            real, intent(in) ::  hfsacc, qevpacc

            !Output
            type(out_bal_intg),intent(inout) :: bal

            !Internal
            integer :: i

        !>--------------Main Subtrouine start-----------------------------------------------

            !Rainfall
            bal%TOTAL_PRE(idate)   = bal%TOTAL_PRE(idate) + pre
            if (isavg) then
                bal%TOTAL_PRE(idate)   = bal%TOTAL_PRE(idate)/real(nhours)
            endif

            !Evap
            bal%TOTAL_EVAP(idate)  = bal%TOTAL_EVAP(idate) + evp
            if (isavg) then
                bal%TOTAL_EVAP(idate)  = bal%TOTAL_EVAP(idate)/real(nhours)
            endif
            !Runoff
            bal%TOTAL_ROF(idate)   = bal%TOTAL_ROF(idate) + rof
            if (isavg) then
                bal%TOTAL_ROF(idate)  = bal%TOTAL_ROF(idate)/real(nhours)
            endif

            !Ponded water
            bal%TOTAL_ZPND(idate)  =  bal%TOTAL_ZPND(idate) + zpnd
            if (isavg) then
                bal%TOTAL_ZPND(idate)  = bal%TOTAL_ZPND(idate)/real(nhours)
            endif

            !Water intercepted in the canopy
            bal%TOTAL_RCAN(idate)  = bal%TOTAL_RCAN(idate) + rcan
            if (isavg) then
                bal%TOTAL_RCAN(idate)  = bal%TOTAL_RCAN(idate)/real(nhours)
            endif

            !Snow in the canopy
            bal%TOTAL_SCAN(idate)  =  bal%TOTAL_SCAN(idate) + scann
            if (isavg) then
                bal%TOTAL_SCAN(idate)  = bal%TOTAL_SCAN(idate)/real(nhours)
            endif

            !Snow
            bal%TOTAL_SNO(idate)   = bal%TOTAL_SNO(idate) + sno
            if (isavg) then
                bal%TOTAL_SNO(idate)  = bal%TOTAL_SNO(idate)/real(nhours)
            endif

            !overland runoff
            bal%TOTAL_ROFO(idate)  = bal%TOTAL_ROFO(idate) + rofo
            if (isavg) then
                bal%TOTAL_ROFO(idate)  = bal%TOTAL_ROFO(idate)/real(nhours)
            endif

            !subsurface runoff
            bal%TOTAL_ROFS(idate)  =  bal%TOTAL_ROFS(idate) + rofs
            if (isavg) then
                bal%TOTAL_ROFS(idate)  = bal%TOTAL_ROFS(idate)/real(nhours)
            endif

            !baseflow
            bal%TOTAL_ROFB(idate)  = bal%TOTAL_ROFB(idate) + rofb
            if (isavg) then
                bal%TOTAL_ROFB(idate)  = bal%TOTAL_ROFB(idate)/real(nhours)
            endif

            !Total Storage
            bal%TOTAL_STORE(idate) = bal%TOTAL_STORE(idate) + stg
            if (isavg) then
                bal%TOTAL_STORE(idate)  = bal%TOTAL_STORE(idate)/real(nhours)
            endif

            !Delta Storage
            bal%DSTG(idate)   = bal%DSTG(idate) + dstg
            if (isavg) then
                bal%DSTG(idate)  = bal%DSTG(idate)/real(nhours)
            endif

            do i = 1, ignd

                bal%TOTAL_THIC(idate,i) = bal%TOTAL_THIC(idate,i) + thic(i)
                bal%TOTAL_THLQ(idate,i) = bal%TOTAL_THLQ(idate,i) + thlq(i)

                if (isavg) then
                    bal%TOTAL_THIC(idate,i) = bal%TOTAL_THIC(idate,i)/real(nhours)
                    bal%TOTAL_THLQ(idate,i) = bal%TOTAL_THLQ(idate,i)/real(nhours)
                endif

            enddo

            bal%TOTAL_HFSACC(idate)  = bal%TOTAL_HFSACC(idate)   + hfsacc
            bal%TOTAL_QEVPACC(idate) = bal%TOTAL_QEVPACC(idate)  + qevpacc

            if (isavg) then
                bal%TOTAL_HFSACC(idate)  = bal%TOTAL_HFSACC(idate)/real(nhours)
                bal%TOTAL_QEVPACC(idate) = bal%TOTAL_QEVPACC(idate)/real(nhours)
            endif


            end subroutine Update_OutBal_Intg
        !>******************************************************************************
            subroutine Init_out(vr,ts,ifo,na,ignd)
        !>------------------------------------------------------------------------------
        !>  Description: Init Fields
        !>
        !>------------------------------------------------------------------------------

            implicit none

            !Inputs
            integer :: na,ignd

            !Inputs-Output
            type(OUT_FLDS)     :: vr
            type(dates_model) :: ts
            type(info_out)    :: ifo

            !Internals
            integer   :: IOS,i,j
            character*50 :: vId


        !>--------------Main Subtrouine start-----------------------------------------------

            OPEN(UNIT   = 909                   , &
                 FILE   = 'outputs_balance.txt' , &
                 STATUS = 'old'                 , &
                 ACTION = 'read'                , &
                 IOSTAT = IOS                     )

            ifo%flIn = 'outputs_balance.txt'

            read(909,*) ifo%pthOut
            read(909,*) ifo%nr_out

            allocate(ifo%ids_var_out(ifo%nr_out,6))

            do i = 1, ifo%nr_out

                read(909,*)(ifo%ids_var_out(i,j),j=1,6)

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

            close(909)

            end subroutine Init_out
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
            type(OUT_FLDS)      :: vr

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

            subroutine Write_Outputs(vr,ts,ifo)
        !>------------------------------------------------------------------------------
        !>  Description: Loop over the variablaes to write
        !>  output balance's fields in selected format
        !>------------------------------------------------------------------------------
            implicit none
            !Inputs
            type(OUT_FLDS)      :: vr
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





            flOut = trim(adjustl(info%pthOut))//       &
                    trim(adjustl(info%ids_var_out(indx,1)))// &
                    '_'//trim(adjustl(freq))//'.seq'

            OPEN(UNIT   = 882                      , &
                 FILE    = trim(adjustl(flOut))    , &
                 STATUS  = 'replace'               , &
                 FORM    = 'unformatted'           , &
                 ACTION  = 'write'                 , &
                 ACCESS  = 'sequential'            , &
                 IOSTAT  = IOS                     )

            nt = size(dates(:,1))

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

            flOut = trim(adjustl(info%pthOut))// &
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
