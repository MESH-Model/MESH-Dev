
module reservoir! Variable declarations

    use model_dates

    implicit none

    type reservoir_f
        integer :: id                              !# Reservoir ID
        integer :: rank                            !# Rank value to identify the grid cell as a reservoir
        integer :: modeltype                       !# Model type (reservoir storage zone release)
        real    :: lat                             !# Lat location of the reservoir (decimal degrees)
        real    :: long                            !# Lon location of the reservoir (decimal degrees)
        real    :: SMAX                            !# Max reservoir capacity (m3)
        real    :: flowO1                          !# Initial discharge (m3 s-1)
        real    :: Intstor1                        !# Initial storage (m3)
        real    :: qmaxmax                         !# Downstream channel capacity
        real    :: RXN                             !# Random term variance or range
        real    :: dsto(12)                        !# Monthly min storage 12 values one per month (m3)
        real    :: nsto(12)                        !# Monthly normal upper storage (m3)
        real    :: ndsto(12)                       !# Monthly upper storage (m3)
        real    :: Qmin(12)                        !# Monthly min release (m3 s-1)
        real    :: Qnor(12)                        !# Monthly normal upper release (m3 s-1)
        real    :: Qnd(12)                         !# Monthly upper release (m3 s-1)
        real    :: stoSIM(2)                       !# storage time series of the reservoir
        real    :: flowSIM(2)                      !# storage time series of the reservoir
    end type

    type reservoirs

        integer :: nreserv                         !# numer of the reservoirs
        type(reservoir_f),allocatable,dimension(:) :: rsvr

!        contains
!        procedure :: init => init_reservoirs

    end type reservoirs

    type(reservoirs), save  :: resrvs

    contains

    subroutine init_reservoirs(flIn)

        !> Derived-type variable.
!        class(reservoirs)         :: resrvs
!        integer, intent(in)       :: nr_timeStep
        character(len=*), intent(in) :: flIn
        !Internals
        integer :: i, j
        open(unit   = 75                    , &
             file   = trim(adjustl(flIn))   , &
             status = 'old'                 , &
             action = 'read'                )


        read(75,*) resrvs%nreserv
        allocate(resrvs%rsvr(resrvs%nreserv))

        do i = 1, resrvs%nreserv

            read(75,*) resrvs%rsvr(i)%id
            read(75,*) resrvs%rsvr(i)%rank
            read(75,*) resrvs%rsvr(i)%modeltype
            read(75,*) resrvs%rsvr(i)%lat, resrvs%rsvr(i)%long
            read(75,*) resrvs%rsvr(i)%SMAX
            read(75,*) resrvs%rsvr(i)%flowO1
            read(75,*) resrvs%rsvr(i)%Intstor1
            read(75,*) resrvs%rsvr(i)%qmaxmax
            read(75,*) resrvs%rsvr(i)%RXN

            read(75,*) (resrvs%rsvr(i)%dsto(j)   , j= 1,12)
            read(75,*) (resrvs%rsvr(i)%nsto(j)   , j= 1,12)
            read(75,*) (resrvs%rsvr(i)%ndsto(j)  , j= 1,12)
            read(75,*) (resrvs%rsvr(i)%Qmin(j)   , j= 1,12)
            read(75,*) (resrvs%rsvr(i)%Qnor(j)   , j= 1,12)
            read(75,*) (resrvs%rsvr(i)%Qnd(j)    , j= 1,12)

!            allocate(resrvs%rsvr(i)%stoSIM(2))
!            allocate(resrvs%rsvr(i)%flowSIM(2))

            ! We set the initial values
            resrvs%rsvr(i)%stoSIM(1)  = resrvs%rsvr(i)%Intstor1
            resrvs%rsvr(i)%flowSIM(1) = resrvs%rsvr(i)%flowO1

        enddo

        close(75)

    end subroutine init_reservoirs

    subroutine get_reservoir(rank,irsv)
    ! Get reservoir id from rank value
    !Iputs
!    class(reservoirs)        :: resrvs
    integer, intent(in)  :: rank
    !Outputs
    integer, intent(out) :: irsv
    !Internals
    integer    :: i

    ! identy reservoir using rank
        do i = 1, resrvs%nreserv
            if (rank .eq. resrvs%rsvr(i)%rank) then
                irsv = i
                exit
            endif
        enddo

    end subroutine get_reservoir

    subroutine compute_reservoir(resrv,flowIn,t,dt,mId)

        class(reservoir_f)     :: resrv
        real   , intent(in)  :: flowIn          !flow from downstream gridcell from routing
        integer, intent(in)  :: mId             !month to idenfy monthly variable parameters
        integer, intent(in)  :: t               !current time step
        real, intent(in)     :: dt

        !internals
        integer  :: irsv
        real     :: FU, LD, LC, LN, LF, RX
        real     :: rnd
        !> get random number
        call random_number(rnd)
        !rnd sould be between 0.0 and 1.0

        ! Water Balance computation S_t-S_t-1 = I - O
        resrv%stoSIM(t) = resrv%stoSIM(t-1) + &
                                      DT*(flowIn - resrv%flowSIM(t-1))

        FU = resrv%stoSIM(t)/resrv%SMAX
        LC = resrv%dsto(mId)/resrv%SMAX
        LN = resrv%nsto(mId)/resrv%SMAX
        LF = resrv%ndsto(mId)/resrv%SMAX
        RX = 2*resrv%RXN*rnd + resrv%RXN
        LD = 0.1

        if (FU <= LD) then

            resrv%flowSIM(t) = 0.0

        else if (FU > LD .and. FU <= LC) then

            resrv%flowSIM(t) = max((min(resrv%Qmin(mId),(FU*resrv%SMAX/DT))+RX),0.0)

        else if (FU > LC .and. FU <= LN) then

            resrv%flowSIM(t) = max((resrv%Qmin(mId) + &
                (resrv%Qnor(mId) - resrv%Qmin(mId))*((FU-LC)/(LN-LC)) + RX),0.0)

        else if (FU > LN .and. FU <= LF) then

            resrv%flowSIM(t) = max((resrv%Qnor(mId) + &
                                           ((FU-LN)/(LF-LN))*max((flowIn-resrv%Qnor(mId)), &
                                           (resrv%Qnd(mId)-resrv%Qnor(mId))) + RX),0.0)

        else

            resrv%flowSIM(t) = max((min(max(((FU-LF)*resrv%SMAX/DT),resrv%Qnd(mId)),resrv%qmaxmax) + RX),0.0)

        end if

        resrv%stoSIM(t-1)  = resrv%stoSIM(t)
        resrv%flowSIM(t-1) = resrv%flowSIM(t)

    end subroutine compute_reservoir

end module reservoir
