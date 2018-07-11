module irrigation_module

    implicit none

    !*  irflg: 1 for irrigated GRU; 0 otherwise (default: 0).
    !*  thlmin: Fraction of field capacity used to determine irrigation demand (default: 0.5). [--].
    !*  t1: Start hour in day for irrigation (default: 0 -- from beginning of day). [h].
    !*  t2: Stop hour in day for irrigation (default: 24 -- to end of day). [h].
    !*  ijday1: First day of cropping season (default: 0 -- by presence of ice in soil). [day of year].
    !*  ijday2: Last day of cropping season (default: 367 -- by presence of ice in soil). [day of year].
    !*  ignd: Last layer to include in demand calculation (default: min(3, NSL)). [--].
    type irrigation_parameters
        integer, dimension(:), allocatable :: irflg, t1, t2, ijday1, ijday2, ignd
        real, dimension(:), allocatable :: thlmin
    end type

    !*  dmnd: Calculated irrigation demand. [kg m-2 s-1].
    !*  avail: Water available for irrigation. [kg m-2 s-1].
    !*  oldpre: Diagnostic variable of precipitation before adding water for irrigation. [kg m-2 s-1].
    !*  newpre: Diagnostic variable of precipitation after adding water for irrigation. [kg m-2 s-1].
    type irrigation_variables
        real, dimension(:), allocatable :: dmnd, avail
    end type

    type irrigation_container
        type(irrigation_parameters) pm, pm_gru, pm_grid
        type(irrigation_variables) va
        logical :: PROCESS_ACTIVE = .false.
    end type

    real, dimension(:), allocatable, save :: IRDMND_TILE, IRDMND_GRID, AVAIL_GRID

    type(irrigation_container), save :: irrm

    contains

    subroutine irrigation_parameters_allocate(pm, n, ierr)

        type(irrigation_parameters) pm
        integer n, ierr

        allocate(pm%irflg(n), pm%t1(n), pm%t2(n), pm%ijday1(n), pm%ijday2(n), pm%ignd(n), pm%thlmin(n), stat = ierr)
        pm%irflg = 0; pm%t1 = 0; pm%t2 = 0; pm%ijday1 = 0; pm%ijday2 = 0; pm%ignd = 0; pm%thlmin = 0.0

    end subroutine

    subroutine irrigation_parameters_deallocate(pm, ierr)

        type(irrigation_parameters) pm
        integer ierr

        deallocate(pm%irflg, pm%t1, pm%t2, pm%ijday1, pm%ijday2, pm%ignd, pm%thlmin, stat = ierr)

    end subroutine

    subroutine irrigation_init(fls, shd, cm)

        use model_files_variabletypes
        use sa_mesh_common
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        integer ierr

        !> Return if the irrigation module is not active.
        if (.not. irrm%PROCESS_ACTIVE) then

            !> Deallocate NNML-based parameters allocated in other parts of the code.
            call irrigation_parameters_deallocate(irrm%pm, ierr)
            return
        end if

        !> Allocate and initialize variables.
        allocate(irrm%va%dmnd(shd%lc%NML), irrm%va%avail(shd%lc%NML))
        irrm%va%dmnd = 0.0; irrm%va%avail = 0.0
        allocate(IRDMND_TILE(shd%lc%NML), IRDMND_GRID(shd%NA), AVAIL_GRID(shd%NA))

        !> Deallocate non-NML based parameters allocated in other parts of the code.
        call irrigation_parameters_deallocate(irrm%pm_grid, ierr)
        call irrigation_parameters_deallocate(irrm%pm_gru, ierr)

        !> Assign default parameterization in the case of no parameterization.
        if (all(irrm%pm%thlmin == 0.0)) irrm%pm%thlmin = 0.5
        if (all(irrm%pm%t1 == 0)) irrm%pm%t1 = 0
        if (all(irrm%pm%t2 == 0)) irrm%pm%t2 = 24
        if (all(irrm%pm%ijday1 == 0)) irrm%pm%ijday1 = 0
        if (all(irrm%pm%ijday2 == 0)) irrm%pm%ijday2 = 367
        if (all(irrm%pm%ignd == 0)) irrm%pm%ignd = min(3, shd%lc%IGND)

    end subroutine

    subroutine irrigation_within_tile(fls, shd, cm)

        use mpi_module
        use model_files_variabletypes
        use sa_mesh_common
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        integer n, l, k, j
        real smin, fsmin, ir, lqsum, check
        logical iractive

        !*  MINSTG: Minimum storage to leave in the channel, not accessible for irrigation. [m3].
        !*  MINFSTG: Fraction of storage to leave in the channel, not accessible for irrigation. [--].
!        real :: MINSTG = 0.0
!        real :: MINFSTG = 0.05

        !> Return if the irrigation module is not active.
        if (.not. irrm%PROCESS_ACTIVE .or. ipid /= 0) return

        !> Calculate demand.
        IRDMND_GRID = 0.0
        do k = 1, shd%lc%NML !GRU -> loop for timestep

            !> Calculate demand for tile.
            IRDMND_TILE(k) = 0.0   !initialization for each time step
            if (irrm%pm%irflg(k) == 1 .and. sum(stas%sl%thic(k, :)) == 0.0 .and. &
                (ic%now%jday >= irrm%pm%ijday1(k) .and. ic%now%jday <= irrm%pm%ijday2(k))) then
                iractive = (ic%now%hour >= irrm%pm%t1(k) .and. ic%now%hour < irrm%pm%t2(k))
                if (iractive) then
                    if (irrm%pm%t1(k) == 0 .or. (ic%now%hour == irrm%pm%t1(k) .and. ic%ts_hourly == 1)) then ! calculate at beginning of irrigation period
                        do j = 1, irrm%pm%ignd(k) ! loop for each Soil layers
                            check = irrm%pm%thlmin(k)*pm%slp%thfc(k, j) ! calculate 50% of field capacity
                            lqsum =  stas%sl%thlq(k, j)
                            if (lqsum < check)then ! check if sum of soil moisture is less than 50% of FC
                                ir = (pm%slp%thfc(k, j) - lqsum)*stas%sl%delzw(k, j) ! calculate irrigation water to field capacity for each permeable soil depth
                            else
                                ir = 0.0
                            end if
                            IRDMND_TILE(k) = IRDMND_TILE(k) + ir ! sum of complete soil depth
                        end do !soil layer
                        IRDMND_TILE(k) = IRDMND_TILE(k)*(1000.0/ic%dts) ! convert into mm/sec
                        irrm%va%dmnd(k) = IRDMND_TILE(k)
                    end if
                    irrm%va%dmnd(k) = max(irrm%va%dmnd(k) - cm%dat(ck%RT)%GAT(k), 0.0) ! subtract current precipitation to calculate actual requirement if there is rain
                else
                    irrm%va%dmnd(k) = 0.0
                end if
            else
                irrm%va%dmnd(k) = 0.0
            end if

            !> Pool demand for irrigation districts.
            if (ro%RUNGRID .and. irrm%va%dmnd(k) > 0.0) then

                !> Determine abstraction point source.
                if (pm%tp%iabsp(k) > 0 .and. pm%tp%iabsp(k) <= fms%absp%n) then

                    !> Discrict, pulls from an abstraction point.
                    n = fms%absp%meta%rnk(pm%tp%iabsp(k))
                else

                    !> Grid, tile pulls from its own cell.
                    n = shd%lc%ILMOS(k)
                end if
                IRDMND_GRID(n) = IRDMND_GRID(n) + &
                    (irrm%va%dmnd(k)/1000.0*ic%dts)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*shd%AREA(shd%lc%ILMOS(k)) ! m3
            end if
        end do

        !> Determine the available water in each grid.
        AVAIL_GRID = 0.0
        if (ro%RUNGRID) then
            do n = 1, shd%NA

                !> Calculate available storage.
                if (IRDMND_GRID(n) > 0.0) then
                    if (any(fms%absp%meta%rnk == n)) then

                        !> Apply minimum storage conditions at abstraction points.
                        l = maxloc(fms%absp%meta%rnk, 1, fms%absp%meta%rnk <= n)
                        AVAIL_GRID(n) = &
                            min(max(stas_grid%chnl%stg(n) - fms%absp%smin(l), 0.0)*(1.0 - fms%absp%fsmin(l)), IRDMND_GRID(n))
                    else if (pm_grid%tp%iabsp(n) == 0) then

                        !> Minimum of available water and demand, with absolute minimum %5 storage preserved in channel.
                        AVAIL_GRID(n) = min(stas_grid%chnl%stg(n)*(1.0 - 0.05), IRDMND_GRID(n)) ! m3
                    end if

                    !> Update storage.
                    if (AVAIL_GRID(n) > 0.0) then
                        stas_grid%chnl%stg(n) = stas_grid%chnl%stg(n) - AVAIL_GRID(n)
                    end if
                end if
            end do
        end if

        !> Abstraction.
        irrm%va%avail = 0.0
        do k = 1, shd%lc%NML
            if (irrm%va%dmnd(k) > 0.0) then

                !> If only running over tiles, assume all demand is available.
                irrm%va%avail(k) = irrm%va%dmnd(k)

                !> If running over grids, check demand against available storage.
                if (ro%RUNGRID) then

                    !> Determine abstraction point source.
                    if (pm%tp%iabsp(k) > 0 .and. pm%tp%iabsp(k) <= fms%absp%n) then

                        !> Discrict, pulls from an abstraction point.
                        n = fms%absp%meta%rnk(pm%tp%iabsp(k))
                    else

                        !> Grid, tile pulls from its own cell.
                        n = shd%lc%ILMOS(k)
                    end if

                    !> Determine available storage in each grid.
                    irrm%va%avail(k) = irrm%va%dmnd(k)*(AVAIL_GRID(n)/IRDMND_GRID(n))
                end if

                !> Apply the abstraction to precipitation.
                cm%dat(ck%RT)%GAT(k) = cm%dat(ck%RT)%GAT(k) + irrm%va%avail(k)
                cm%dat(ck%RT)%GRD(shd%lc%ILMOS(k)) = &
                    cm%dat(ck%RT)%GRD(shd%lc%ILMOS(k)) + irrm%va%avail(k)*shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))

                !> Preserve demand gone unsatisfied.
                irrm%va%dmnd(k) = irrm%va%dmnd(k) - irrm%va%avail(k)
            end if
        end do

    end subroutine

    subroutine irrigation_open_output(fls, shd, cm)

        use mpi_module
        use model_files_variabletypes
        use sa_mesh_common
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        character(len = 200) fn
        character(len = 3) ffmti
        integer iun, l

        !> Return if the irrigation module is not active or if not the head node.
        if (.not. irrm%PROCESS_ACTIVE .or. ipid /= 0) return

        open(unit = 1981, file = './' // trim(fls%GENDIR_OUT) // '/irrigation.csv') ! open file for output
        write(1981, 1010) 'YEAR', 'DAY', 'HOUR', 'MINS', 'IRDMND', 'IRAVAI', 'IRTOT', 'OLDPRE', 'NEWPRE'
        if (fms%absp%n > 0) then
            do l = 1, fms%absp%n
                iun = 1981 + l
                write(ffmti, '(i3)') l
                fn = './' // trim(fls%GENDIR_OUT) // '/irrigation_' // trim(adjustl(ffmti)) // '.csv'
                open(unit = iun, file = fn)
                write(iun, 1010) 'YEAR', 'DAY', 'HOUR', 'MINS', 'IRDMND', 'IRAVAI', 'IRTOT', 'OLDPRE', 'NEWPRE'
            end do
        end if

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine irrigation_write_output(fls, shd, cm)

        use mpi_module
        use model_files_variabletypes
        use sa_mesh_common
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        real, dimension(:), allocatable :: SUMIRDMND, SUMIRAVAI, SUMOLDPRE, SUMNEWPRE, SUMDSCTAR
        real frac
        integer iun, l, k

        !> Return if the irrigation module is not active or if not the head node.
        if (.not. irrm%PROCESS_ACTIVE .or. ipid /= 0) return

        !> Aggregate outputs.
        !> Apply weight by contributing area (irrigated GRU).
        if (.not. allocated(SUMIRDMND)) allocate(SUMIRDMND(0:fms%absp%n))
        if (.not. allocated(SUMIRAVAI)) allocate(SUMIRAVAI(0:fms%absp%n))
        if (.not. allocated(SUMOLDPRE)) allocate(SUMOLDPRE(0:fms%absp%n))
        if (.not. allocated(SUMNEWPRE)) allocate(SUMNEWPRE(0:fms%absp%n))
        if (.not. allocated(SUMDSCTAR)) allocate(SUMDSCTAR(0:fms%absp%n))
        SUMIRDMND = 0.0; SUMIRAVAI = 0.0; SUMOLDPRE = 0.0; SUMNEWPRE = 0.0; SUMDSCTAR = 0.0
        do k = 1, shd%lc%NML
            frac = shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*shd%AREA(shd%lc%ILMOS(k))
            if (irrm%va%avail(k) > 0.0) then
                if (pm%tp%iabsp(k) > 0 .and. pm%tp%iabsp(k) <= fms%absp%n) then
                    SUMIRDMND(pm%tp%iabsp(k)) = SUMIRDMND(pm%tp%iabsp(k)) + (irrm%va%dmnd(k) + irrm%va%avail(k))*ic%dts*frac
                    SUMIRAVAI(pm%tp%iabsp(k)) = SUMIRAVAI(pm%tp%iabsp(k)) + irrm%va%avail(k)*ic%dts*frac
                    SUMOLDPRE(pm%tp%iabsp(k)) = SUMOLDPRE(pm%tp%iabsp(k)) + (cm%dat(ck%RT)%GAT(k) - irrm%va%avail(k))*ic%dts*frac
                    SUMNEWPRE(pm%tp%iabsp(k)) = SUMNEWPRE(pm%tp%iabsp(k)) + cm%dat(ck%RT)%GAT(k)*ic%dts*frac
                    SUMDSCTAR(pm%tp%iabsp(k)) = SUMDSCTAR(pm%tp%iabsp(k)) + frac
                end if
                SUMIRDMND(0) = SUMIRDMND(0) + (irrm%va%dmnd(k) + irrm%va%avail(k))*ic%dts*frac
                SUMIRAVAI(0) = SUMIRAVAI(0) + irrm%va%avail(k)*ic%dts*frac
                SUMOLDPRE(0) = SUMOLDPRE(0) + (cm%dat(ck%RT)%GAT(k) - irrm%va%avail(k))*ic%dts*frac
                SUMNEWPRE(0) = SUMNEWPRE(0) + cm%dat(ck%RT)%GAT(k)*ic%dts*frac
                SUMDSCTAR(0) = SUMDSCTAR(0) + frac
            end if
        end do

        !> Normalize to district area.
        where (SUMDSCTAR > 0.0)
            SUMIRDMND = SUMIRDMND/SUMDSCTAR
            SUMIRAVAI = SUMIRAVAI/SUMDSCTAR
            SUMOLDPRE = SUMOLDPRE/SUMDSCTAR
            SUMNEWPRE = SUMNEWPRE/SUMDSCTAR
        end where

        !> Write outputs.
        write(1981, 1010) &
            ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins, SUMIRDMND(0), SUMIRAVAI(0), SUMIRAVAI(0), SUMOLDPRE(0), SUMNEWPRE(0)
        do l = 1, fms%absp%n
            iun = 1981 + l
            write(iun, 1010) &
                ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins, &
                SUMIRDMND(l), SUMIRAVAI(l), SUMIRAVAI(l), SUMOLDPRE(l), SUMNEWPRE(l)
        end do

1010    format(9999(g15.7e2, ','))

    end subroutine

end module
