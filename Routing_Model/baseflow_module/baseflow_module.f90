module baseflow_module

    implicit none

    !>
    !> BASEFLOW module external parameter type
    !>
    !> Run options:
    !*  BASEFLOWFLAG: Flag that specifies baseflow routine.
    !>
    !> Initialization (states):
    !*  WrchrgIni: Initial constant recharge for cold start (mm/hr).
    !*  QbIni: Initial constant baseflow for cold start (mm/hr).
    !>
    !> Parameters (Hydrology):
    !> Routine 1: BASEFLOWFLAG 1
    !*  dgwsh: Delay time of the overlying soil layers in the  aquifer (hour).
    !*  agwsh: Recession constant of the aquifer.
    !>
    !> Routine 2: BASEFLOWFLAG 2
    !*  flz: lower zone function (mm).
    !*  pwr: exponent on the lower zone storage in the lower zone funnction.
    !>
    type baseflow_parameters
        real, dimension(:), allocatable :: dgw, agw
        real, dimension(:), allocatable :: pwr, flz
    end type

    type baseflow_variables
        real WrchrgIni, QbIni
    end type

    !>
    !> BASEFLOW internal variables
    !>

    !> BASEFLOWFLAG (1)
    real, dimension(:), allocatable :: Wseep, Wrchrg, Qb
    real :: Wrchrg_new, Qb_new

    !> BASEFLOWFLAG (2)
    real, dimension(:), allocatable :: dlz, lzs

    type baseflow_container
        type(baseflow_parameters) :: pm, pm_iak, pm_gru, pm_grid
        type(baseflow_variables) :: vs
        integer :: BASEFLOWFLAG = 0, BUCKETFLAG = 0, dts = 0
    end type

    type(baseflow_container), save :: bflm

    contains

    subroutine bflm_init(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        use FLAGS

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        integer NA, NML, NTYPE, NRVR, n, i, ierr
        integer :: iun = 58
        character(len = 200) BASEFLOWFLAG

        !> Return if BASEFLOWFLAG is not active
        if (bflm%BASEFLOWFLAG == 0) return

        NA = shd%NA
        NML = shd%lc%NML
        NTYPE = shd%lc%NTYPE
        NRVR = shd%NRVR

        !> Summarize current BASEFLOWFLAG configuration to file.
        if (ipid == 0 .and. MODELINFOOUTFLAG > 0) then
            write(BASEFLOWFLAG, '(i8)') bflm%dts/60
            BASEFLOWFLAG = 'hf=' // adjustl(BASEFLOWFLAG)
            select case (bflm%BUCKETFLAG)
                case (1)
                    BASEFLOWFLAG = 'grid ' // adjustl(BASEFLOWFLAG)
                case default
                    BASEFLOWFLAG = 'tile ' // adjustl(BASEFLOWFLAG)
            end select
            write(iun, 1100)
            select case (bflm%BASEFLOWFLAG)
                case (1)
                    BASEFLOWFLAG = 'BASEFLOWFLAG  luo_2012 ' // adjustl(BASEFLOWFLAG)
                    write(iun, 1130) BASEFLOWFLAG
                    if (ro%DIAGNOSEMODE > 0) then
                        write(iun, 1110) 'WRCHRG_INI', bflm%vs%WrchrgIni
                        write(iun, 1110) 'QB_INI', bflm%vs%QbIni
                        write(iun, 1110) 'DGWSH', (bflm%pm_gru%dgw(i), i = 1, NTYPE)
                        write(iun, 1110) 'AGWSH', (bflm%pm_gru%agw(i), i = 1, NTYPE)
                    end if
                case (2)
                    BASEFLOWFLAG = 'BASEFLOWFLAG  wf_lzs ' // adjustl(BASEFLOWFLAG)
                    write(iun, 1130) BASEFLOWFLAG
                    if (ro%DIAGNOSEMODE > 0) then
                        if (any(bflm%pm_gru%pwr /= 0.0)) then
                            write(iun, 1110) 'pwr_gru', (bflm%pm_gru%pwr(i), i = 1, NTYPE)
                        else if (any(bflm%pm_iak%pwr /= 0.0)) then
                            write(iun, 1110) 'pwr_iak', (bflm%pm_iak%pwr(i), i = 1, NRVR)
                        else if (any(bflm%pm_grid%pwr /= 0.0)) then
                            write(iun, 1110) 'pwr_grid (min., max.)', minval(bflm%pm_grid%pwr), maxval(bflm%pm_grid%pwr)
                        end if
                        if (any(bflm%pm_gru%pwr /= 0.0)) then
                            write(iun, 1110) 'flz_gru', (bflm%pm_gru%flz(i), i = 1, NTYPE)
                        else if (any(bflm%pm_iak%pwr /= 0.0)) then
                            write(iun, 1110) 'flz_iak', (bflm%pm_iak%flz(i), i = 1, NRVR)
                        else if (any(bflm%pm_grid%pwr /= 0.0)) then
                            write(iun, 1110) 'flz_grid (min., max.)', minval(bflm%pm_grid%flz), maxval(bflm%pm_grid%flz)
                        end if
                    end if
                case default
                    write(iun, 1120) bflm%BASEFLOWFLAG
            end select
            write(iun, *)
        end if

        !> Summarize current BASEFLOWFLAG configuration to screen.
        if (ro%VERBOSEMODE > 0) then
            print 1100
            print *
        end if

        !> Allocate and initialize local variables.
        stas%lzs%ws = bflm%vs%WrchrgIni
        stas_grid%lzs%ws = bflm%vs%WrchrgIni
        select case (bflm%BASEFLOWFLAG)
            case (1)
                allocate(Wseep(NML), Wrchrg(NML), Qb(NML))
                Wseep = 0.0
                Wrchrg = stas%lzs%ws
                Qb = bflm%vs%QbIni
            case (2)
                if (bflm%BUCKETFLAG == 1) then
                    allocate(dlz(NA), lzs(NA))
                    dlz = 0.0
                    lzs = stas_grid%lzs%ws
                    bflm%pm_grid%flz = 1.0 - (1.0 - bflm%pm_grid%flz)
                end if
            case default
                print 1120, bflm%BASEFLOWFLAG
                print *
        end select

        if (RESUMEFLAG == 4 .or. RESUMEFLAG == 5) then
            select case (bflm%BASEFLOWFLAG)
                case (1)
                    iun = fls%fl(mfk%f883)%iun
                    open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.luo_2012', status = 'old', action = 'read', &
                        form = 'unformatted', access = 'sequential', iostat = ierr)
                    read(iun) stas%lzs%ws
                    read(iun) Qb
                    close(iun)
                case (2)
                    iun = fls%fl(mfk%f883)%iun
                    open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.wfqlz', status = 'old', action = 'read', &
                        form = 'unformatted', access = 'sequential', iostat = ierr)
                    read(iun) stas%lzs%ws
                    close(iun)
            end select
        end if

1100    format(/1x, 'BASEFLOW component ACTIVATED')
1110    format(999(1x, g16.9))
1120    format(3x, 'WARNING: BASEFLOWFLAG ', i3, ' not supported.')
1130    format(3x, (a))

    end subroutine

    subroutine bflm_within_tile(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        integer k

        !> Return if BASEFLOWFLAG is not active
        if (bflm%BASEFLOWFLAG == 0) return

        !> Calculate contribution of baseflow to lower zone storage and redistribute runoff.
        select case (bflm%BASEFLOWFLAG)
            case (1)
                Wseep(il1:il2) = stas%lzs%rofb(il1:il2)*3600.0
                Wrchrg(il1:il2) = stas%lzs%ws(il1:il2)
                do k = il1, il2
                    call baseFlow_luo2012(Wseep(k), bflm%pm%dgw(k), Wrchrg(k), bflm%pm%agw(k), Qb(k), 1.0, Wrchrg_new, Qb_new)
                    stas%lzs%rofb(k) = Qb_new/3600.0
                    Qb(k) = Qb_new
                    stas%lzs%ws(k) = Wrchrg_new
                end do
            case (2)
                stas%lzs%ws(il1:il2) = stas%lzs%ws(il1:il2) + stas%lzs%rofb(il1:il2)*ic%dts
        end select

    end subroutine

    subroutine bflm_within_grid(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        integer k

        !> Return if BASEFLOWFLAG is not active
        if (bflm%BASEFLOWFLAG == 0) return

        !> Calculate contribution of baseflow to lower zone storage and redistribute runoff.
        select case (bflm%BASEFLOWFLAG)
            case (2)
                if ((bflm%dts - ic%dts*ic%ts_hourly) == 0) then
                    lzs(i1:i2) = stas_grid%lzs%ws(i1:i2)
                    call baseflow_wfqlz(bflm%pm_grid%flz, bflm%pm_grid%pwr, lzs, dlz, shd%NA, i1, i2)
                    dlz(i1:i2) = max(min(dlz(i1:i2), lzs(i1:i2)), 0.0)/real(bflm%dts/ic%dts)
                end if
                stas_grid%lzs%rofb(i1:i2) = dlz(i1:i2)/real(ic%dts)
                stas_grid%lzs%ws(i1:i2) = stas_grid%lzs%ws(i1:i2) - stas_grid%lzs%rofb(i1:i2)*ic%dts
                do k = il1, il2
                    stas%lzs%rofb(k) = stas_grid%lzs%rofb(shd%lc%ILMOS(k))
                    stas%lzs%ws(k) = stas_grid%lzs%ws(shd%lc%ILMOS(k))
                end do
        end select

    end subroutine

    subroutine bflm_finalize(fls, shd, cm)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing

        !> For: SAVERESUMEFLAG
        use FLAGS

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm

        !> Local variables.
        integer ierr, iun

        !> Return if not the head node.
        if (ipid /= 0) return

        !> Return if BASEFLOWFLAG is not active
        if (bflm%BASEFLOWFLAG == 0) return

        if (SAVERESUMEFLAG == 4 .or. SAVERESUMEFLAG == 5) then
            select case (bflm%BASEFLOWFLAG)
                case (1)
                    iun = fls%fl(mfk%f883)%iun
                    open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.luo_2012', status = 'replace', &
                        action = 'write', form = 'unformatted', access = 'sequential', iostat = ierr)
                    write(iun) stas%lzs%ws
                    write(iun) Qb
                    close(iun)
                case (2)
                    iun = fls%fl(mfk%f883)%iun
                    open(iun, file = trim(adjustl(fls%fl(mfk%f883)%fn)) // '.lzsp.wfqlz', status = 'replace', &
                        action = 'write', form = 'unformatted', access = 'sequential', iostat = ierr)
                    write(iun) stas%lzs%ws
                    close(iun)
            end select
        end if

    end subroutine

    subroutine bflm_parse_flag(line)

        use strings
        use model_dates

        !> Input variables.
        !*  line: BASEFLOWFLAG read from file to be parsed.
        character(len = *), intent(in) :: line

        !> Local variables.
        character(len = 200), dimension(20) :: args
        integer nargs, n, i, ierr

        !> Parse the line into a vector of options.
        call parse(line, ' ', args, nargs)

        !> Parse the options.
        bflm%BASEFLOWFLAG = 0
        do i = 2, nargs

            !> Old numeric option assigns presets.
            !> Word options override presets.
            if (args(i) == '1' .or. lowercase(args(i)) == 'luo_2012') then
                bflm%BASEFLOWFLAG = 1
                bflm%BUCKETFLAG = 0
                bflm%dts = ic%dts
            else if (args(i) == '2' .or. lowercase(args(i)) == 'wf_lzs') then
                bflm%BASEFLOWFLAG = 2
                bflm%BUCKETFLAG = 1
                bflm%dts = 3600
            else if (lowercase(args(i)) == 'grid') then
                bflm%BUCKETFLAG = 1
            else if (lowercase(args(i)) == 'tile') then
                bflm%BUCKETFLAG = 0
            else if (lowercase(args(i)(1:3)) == 'hf=') then
                call value(args(i)(4:), n, ierr)
                if (ierr == 0) bflm%dts = n*60
            end if

        end do

    end subroutine

end module
