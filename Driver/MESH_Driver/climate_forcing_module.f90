         module climate_forcing
         !>******************************************************************************
         !>  Athor: Gonzalo Sapriza Azuri
         !>  Description:
         !> Handled climate forcing data to be loaded in memory
         !>******************************************************************************
         use model_dates
         !>******************************************************************************
!         TYPE clim_data
!
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: shortwave  !1
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: longwave   !2
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: rain       !3
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: temp       !4
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: wind       !5
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: pressure   !6
!            REAL,DIMENSION(:,:) ,ALLOCATABLE :: humidity   !7

!         END TYPE
         !>******************************************************************************
         TYPE clim_info_read

            integer :: timeSize  !minimum size of block read DEFINED IN THE INPUT FILE
            integer,dimension(:),allocatable :: ntimes ! number of time in each block of readed data
            character(15) :: id_var !climate variable name
            integer      :: flagID !Basic climage flag id
            integer      :: flagRead !type of format file
            integer      :: unitR !Number unit
            logical      :: openFl !true if file is open
            integer      :: readIndx !index in the block of time that we are reading
            integer      :: itime    !time index
            real,dimension(:,:) ,allocatable :: climv !Climate variable

         END TYPE

         TYPE clim_info

            integer :: na !number of cell inside the basin
            integer :: nclim !number of climate variables
            type(clim_info_read) :: clin(7)

         END TYPE
        !>******************************************************************************

         contains
        !>******************************************************************************
         subroutine Init_clim_info(cm,ts,indx,nna)
        !>------------------------------------------------------------------------------
        !>  Description: Initialization of clim_info
        !>
        !>------------------------------------------------------------------------------

         implicit none

         !Input
         integer,intent(in)              :: nna,indx
         type(dates_model),intent(in)    :: ts

         !Input Output
          type(clim_info),intent(inout) :: cm
         !Internals
         integer  :: i,nts,rts

         cm%nclim = 7
         cm%na = nna
         i = indx


         if (cm%clin(i)%flagId .eq. 5)then

            if(ts%nr_timeStepClimF.le.cm%clin(i)%timeSize)then

                allocate(cm%clin(i)%ntimes(1))
                cm%clin(i)%ntimes(1) = ts%nr_timeStepClimF

            else

                nts = ts%nr_timeStepClimF/cm%clin(i)%timeSize
                rts = ts%nr_timeStepClimF - cm%clin(i)%timeSize*nts

                if (rts.eq.0)then

                    allocate(cm%clin(i)%ntimes(nts))
                    cm%clin(i)%ntimes = cm%clin(i)%timeSize
                else
                    allocate(cm%clin(i)%ntimes(nts+1))
                    cm%clin(i)%ntimes = cm%clin(i)%timeSize
                    cm%clin(i)%ntimes(nts+1) = rts
                endif

            endif
            cm%clin(i)%readIndx = 1
         endif

!        write(400+indx,*)'flagId',cm%clin(i)%flagId
!        write(400+indx,*)'ntimes',cm%clin(i)%ntimes
!        write(400+indx,*)'timeSize',cm%clin(i)%timeSize
!        write(400+indx,*)'id_var',cm%clin(i)%id_var
!        write(400+indx,*)'flagRead',cm%clin(i)%flagRead
!        close(400+indx)
         end subroutine Init_clim_info

        !>******************************************************************************
         subroutine Init_clim_data(cm,idvar,unitR)
        !>------------------------------------------------------------------------------
        !>  Description: Initialization of climate data
        !>
        !>------------------------------------------------------------------------------
         implicit none

         !Inputs
         integer,intent(in) :: unitR
         character(len=*),intent(in) :: idvar

         !Inputs Output
         type(clim_info),intent(inout) :: cm



         if (idvar .eq. 'shortwave')then

            allocate(cm%clin(1)%climv(cm%na,cm%clin(1)%ntimes(1)))
            cm%clin(1)%unitR = unitR
            call OpenData(cm,'basin_shortwave',1)

         endif

         if (idvar .eq. 'longwave')then

            allocate(cm%clin(2)%climv(cm%na,cm%clin(2)%ntimes(1)))
            cm%clin(2)%unitR = unitR
            call OpenData(cm,'basin_longwave',2)

         endif

         if (idvar .eq. 'rain')then

            allocate(cm%clin(3)%climv(cm%na,cm%clin(3)%ntimes(1)))
            cm%clin(3)%unitR = unitR
            call OpenData(cm,'basin_rain',3)

         endif

         if (idvar .eq. 'temp')then

            allocate(cm%clin(4)%climv(cm%na,cm%clin(4)%ntimes(1)))
            cm%clin(4)%unitR = unitR
            call OpenData(cm,'basin_temperature',4)

         endif

         if (idvar .eq. 'wind')then

            allocate(cm%clin(5)%climv(cm%na,cm%clin(5)%ntimes(1)))
            cm%clin(5)%unitR = unitR
            call OpenData(cm,'basin_wind',5)

         endif

         if (idvar .eq. 'pressure')then

            allocate(cm%clin(6)%climv(cm%na,cm%clin(6)%ntimes(1)))
            cm%clin(6)%unitR = unitR
            call OpenData(cm,'basin_pres',6)

         endif

         if (idvar .eq. 'humidity')then

            allocate(cm%clin(7)%climv(cm%na,cm%clin(7)%ntimes(1)))
            cm%clin(7)%unitR = unitR
            call OpenData(cm,'basin_humidity',7)

         endif


         end subroutine Init_clim_data

        !>******************************************************************************
         subroutine OpenData(cm,flname,indx)
        !>------------------------------------------------------------------------------
        !>  Description: Open Units and Load the first block of climate data
        !>
        !>------------------------------------------------------------------------------
         implicit none

         !Input
         integer,intent(in)  :: indx
         character(len=*),intent(in):: flname

         type(clim_info) :: cm
         integer   :: ios
         character*80 :: end_of_r2c_header

         !>Open file depending on the format type of the climate data
         !> r2c format
         if     (cm%clin(indx)%flagRead .eq. 1) then

            print*, cm%clin(indx)%unitR ,trim(adjustl(flname))//'.r2c'

            OPEN(unit   = cm%clin(indx)%unitR           , &
                 file   = trim(adjustl(flname))//'.r2c' , &
                 STATUS = 'OLD'                         , &
                 IOSTAT = IOS                           )

         !> IOS would be 0 if the file opened successfully.

            IF(IOS/=0)THEN
              !>no basin longwave file exists
              PRINT *, trim(adjustl(flname))//'.r2c'//'not found'
              PRINT *, 'please adjust the mesh_input_run_options.ini file,'
              PRINT *, 'or put the r2c file in the correct location.'
              CLOSE(cm%clin(indx)%unitR)
              cm%clin(indx)%openFl = .false.
              STOP
            ELSE
              PRINT *, trim(adjustl(flname))//'.r2c'//' found'
               end_of_r2c_header = ""
              DO WHILE (end_of_r2c_header /= ":endHeader")
                READ (cm%clin(indx)%unitR, '(A10)') end_of_r2c_header
              ENDDO

              cm%clin(indx)%openFl = .true.

            ENDIF


         elseif (cm%clin(indx)%flagRead .eq. 2) then

            OPEN(unit   = cm%clin(indx)%unitR            , &
                 file   = trim(adjustl(flname))//'.csv'  , &
                 STATUS = 'OLD'                          , &
                 IOSTAT = IOS                            )

            !> IOS would be 0 if the file opened successfully.
            IF(IOS/=0)THEN
              !> no basin longwave file exists
              PRINT *, trim(adjustl(flname))//'.csv'//'not found'
              PRINT *, 'please adjust the mesh_input_run_options.ini file,'
              PRINT *, 'or put the csv file in the correct location.'
              CLOSE(cm%clin(indx)%unitR)
              cm%clin(indx)%openFl = .false.
              STOP
             ELSE
                PRINT *, trim(adjustl(flname))//'.csv'//' found'
                cm%clin(indx)%openFl = .true.
             ENDIF

         elseif (cm%clin(indx)%flagRead .eq. 3) then

            OPEN(UNIT   = cm%clin(indx)%unitR           , &
                 FILE   = trim(adjustl(flname))//'.seq' , &
                 STATUS = 'OLD'                         , &
                 FORM   = 'unformatted'                 , &
                 ACTION = 'read'                        , &
                 ACCESS = 'sequential'                  , &
                 IOSTAT = IOS                           )

            IF(IOS/=0)THEN
              !> no basin shortwave file exists
              PRINT *, trim(adjustl(flname))//'.seq'//'not found'
              PRINT *, 'please adjust the mesh_input_run_options.ini file'
              PRINT *, 'or put the seq file in the correct location.'
              CLOSE(cm%clin(indx)%unitR)
              cm%clin(indx)%openFl = .false.
              STOP
            ELSE

              PRINT *, trim(adjustl(flname))//'.seq'//' found'
              cm%clin(indx)%openFl = .true.
            ENDIF


         elseif (cm%clin(indx)%flagRead .eq. 4) then

            OPEN(UNIT   =  cm%clin(indx)%unitR          , &
                 FILE   = trim(adjustl(flname))//'.asc' , &
                 STATUS = 'OLD'                         , &
                 FORM   = 'formatted'                   , &
                 ACTION = 'read'                        , &
                 IOSTAT =  IOS                          )

            IF(IOS/=0)THEN
              !> no basin shortwave file exists
              PRINT *, trim(adjustl(flname))//'.asc'//'not found'
              PRINT *, 'please adjust the mesh_input_run_options.ini file'
              PRINT *, 'or put the seq file in the correct location.'
              CLOSE(cm%clin(indx)%unitR)
              cm%clin(indx)%openFl = .false.
              STOP
            ELSE

              PRINT *, trim(adjustl(flname))//'.asc'//' found'
              cm%clin(indx)%openFl = .true.
            ENDIF

         endif

         end subroutine OpenData

        !>******************************************************************************
         subroutine LoadData(cm,indx,xcount,ycount,xxx,yyy,na,enddata)

        !>------------------------------------------------------------------------------
        !>  Description: Load block of data
        !>
        !>------------------------------------------------------------------------------
         implicit none
         type(clim_info) :: cm


         !Inputs variables
         integer :: XCOUNT,YCOUNT,NA
         integer :: XXX(NA),YYY(NA)
         integer :: indx

         !output
         logical :: enddata

         !Internals variables
         integer :: tm(2),ntime
         integer :: i,j,ii,jj
         real    :: R4GRID2D(YCOUNT,XCOUNT)

         i = indx

            if (cm%clin(i)%flagId .eq. 5)then

                tm  = shape(cm%clin(i)%climv)
                !> r2c file format
                if     (cm%clin(i)%flagRead .eq. 1) then
                    do j = 1, tm(2)
                        READ(cm%clin(i)%unitR, *, END=999) !:Frame line
                        do ii = 1,YCOUNT
                            READ(cm%clin(i)%unitR, *, END=999) (R4GRID2D(ii,jj),jj=1,XCOUNT)
                        enddo
                        READ(cm%clin(i)%unitR, *, END=999) !:EndFrame line
                        do ii=1,NA
                           cm%clin(i)%climv(ii,j)=R4GRID2D(YYY(ii),XXX(ii))
                        enddo
                     enddo

                elseif (cm%clin(i)%flagRead .eq. 2) then

                    print*, 'NOT IMPLEMENTED YET'
                    STOP

                elseif (cm%clin(i)%flagRead .eq. 3) then

                    do j = 1, tm(2)

                        READ(unit=cm%clin(i)%unitR) NTIME

                        READ(unit=cm%clin(i)%unitR,END=999) cm%clin(i)%climv(:,j)

                    enddo

                elseif (cm%clin(i)%flagRead .eq. 4) then

                    do j = 1, tm(2)

                        READ(cm%clin(i)%unitR,*,END=999) (cm%clin(i)%climv(ii,j),ii=1,tm(1))

                    enddo

                endif
            endif
         return
999 ENDDATA = .TRUE.
         end subroutine LoadData


        !>******************************************************************************
         subroutine NeedUpdate_clim_data(cm,indx,timeC,xcount,ycount,xxx,yyy,na,enddata)

        !>------------------------------------------------------------------------------
        !>  Description: Check if we need to load data again if that, we deallocate
        !>  and allocate again and then we load data
        !>------------------------------------------------------------------------------
         implicit none
         type(clim_info) :: cm

         !Inputs variables
         integer :: XCOUNT,YCOUNT,NA
         integer :: XXX(NA),YYY(NA)
         integer :: timeC,indx

         !Ouput
         logical :: enddata

         integer :: i,tm(2),nstp,ss

         i = indx

         if (cm%clin(i)%flagId .eq. 5)then

            tm  = shape(cm%clin(i)%climv)

            if (timeC .eq. 1)then

                call LoadData(cm,i,xcount,ycount,xxx,yyy,na,enddata)

                nstp = 0

            !> Check if we need update
            elseif (cm%clin(i)%itime .eq. tm(2))then

                cm%clin(i)%readIndx = cm%clin(i)%readIndx + 1

                ss = size(cm%clin(i)%ntimes)

                if (cm%clin(i)%readIndx  .gt.  ss ) then

                    enddata = .true.

                    return

                else

                    deallocate(cm%clin(i)%climv)

                    allocate(cm%clin(i)%climv(cm%na,cm%clin(i)%ntimes(cm%clin(i)%readIndx)))

                    call LoadData(cm,i,xcount,ycount,xxx,yyy,na,enddata)

                    nstp = cm%clin(i)%ntimes(cm%clin(i)%readIndx-1)

                endif

            else

                nstp = cm%clin(i)%ntimes(cm%clin(i)%readIndx-1)

            endif

            cm%clin(i)%itime = timeC -  (cm%clin(i)%readIndx-1)*nstp

         endif



         end subroutine NeedUpdate_clim_data



         end module climate_forcing
