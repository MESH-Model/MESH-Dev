module sa_mesh_run_between_grid

    !> 'model_files_variables' required for 'fls' object and file keys.
    !> 'sa_mesh_common' required for common SA_MESH variables and routines.
    !> 'climate_forcing' required for 'cm' variable.
    !> 'mpi_module' required for MPI variables, tile/grid parsing utility, barrier flag.
    use model_files_variables
    use sa_mesh_common
    use climate_forcing
    use mpi_module

!temp: Outputs.
    use model_files_variabletypes, only: fl_ids

    implicit none

    !> Variable type: WF_RTE_fout_stfl
    !>  Description: Internal file keys used for output files for streamflow.
    !>
    !> Variables:
    !*  KDLY: Daily output
    !*  KTS: Per time-step output
    !*  freq: Time intervals of the output (daily, ts).
    !*  fout_hyd: .true. to print observed and simulated values (default).
    !*  fout_bal: .true. to print channel storage terms (optional).
    !*  fout_acc: .true. to print accumulated (cumulative) observed and simulated values (optional).
    !*  fout_header: .true. to print header (default).
    !*  fls: Output file definitions.
    type WF_RTE_fout_stfl
        integer(kind = 4) :: KDLY = 0, KTS = 1
        integer :: kmin = 0, kmax = 1
        integer(kind = 4) :: freq = 1
        logical :: fout_hyd = .true., fout_bal = .false., fout_acc = .false.
        logical :: fout_header = .true.
        type(fl_ids) :: fls
    end type

    !> Variable type: WF_RTE_fout_rsvr
    !>  Description: Internal file keys used for output files for lakes and reservoirs.
    !>
    !> Variables:
    !*  KTS: Per time-step output
    !*  freq: Time intervals of the output (ts).
    !*  fout_header: .true. to print header (default).
    !*  fls: Output file definitions.
    type WF_RTE_fout_rsvr
        integer(kind = 4) :: KDLY = 0, KTS = 1, KHLY = 2
        integer :: kmin = 0, kmax = 2
        integer(kind = 4) :: freq = 0
        logical :: fout_header = .true.
        type(fl_ids) :: fls
    end type

    !> Output files
    type(WF_RTE_fout_stfl), save :: WF_RTE_fstflout
    type(WF_RTE_fout_rsvr), save :: WF_RTE_frsvrout

    real, dimension(:), allocatable :: WF_QHYD_CUM

!todo: Move to ro%?
    integer RTE_TS

    real, dimension(:), allocatable :: WF_QO2_ACC, WF_QO2_ACC_MM, WF_STORE2_ACC_MM

!>>>temp_diversion
    integer :: iun_div = 64, n_div, dt_div, iyear_div, ijday_div
    character(len = 13) :: fn_div = 'diversion.txt'
    real, dimension(:), allocatable :: x_src, y_src, x_snk, y_snk, in_div_m3, in_div_m3s, tnsfr_div
    integer, dimension(:), allocatable :: jx_src, iy_src, jx_snk, iy_snk, rnk_src, rnk_snk
    logical run_div
    real, dimension(:), allocatable :: qo_div_dly, qa_div_dly
!<<<temp_diversion

    contains

    subroutine run_between_grid_init(fls, shd, cm)

        !> Process modules.
        use SA_RTE_module
        use WF_ROUTE_config
        use reservoir
        use rte_module
        use cropland_irrigation_between_grid

!>>>temp_diversion
        use txt_io
!<<<temp_diversion
!temp: Outputs.
        use save_basin_output, only: STREAMFLOWOUTFLAG, REACHOUTFLAG
        use FLAGS
        use strings

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer, parameter :: MaxLenField = 20, MaxArgs = 20, MaxLenLine = 100
        integer NA
        integer NS, NR
        character(len = 4) ffmti
        character(len = 500) fn
        integer iun, ierr, l, j, i
        character(MaxLenField), dimension(MaxArgs) :: out_args
        integer nargs

!>>>temp_diversion
        integer n, isteps1, isteps2, iskip
        character(len = DEFAULT_LINE_LENGTH) line
!<<<temp_diversion

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        if (BASINSWEOUTFLAG > 0) then
            open(85, file = './' // trim(fls%GENDIR_OUT) // '/basin_SCA_alldays.csv')
            open(86, file = './' // trim(fls%GENDIR_OUT) // '/basin_SWE_alldays.csv')
        end if !(BASINSWEOUTFLAG > 0) then

        RTE_TS = ic%dts
        if (WF_RTE_flgs%PROCESS_ACTIVE) RTE_TS = WF_RTE_flgs%RTE_TS
        if (rteflg%PROCESS_ACTIVE) RTE_TS = rteflg%RTE_TS

        NA = shd%NA
        NR = fms%rsvr%n
        NS = fms%stmg%n

        !> For zone-based storage.
        if (RESERVOIRFLAG == 2) then

            !> Read configuration from file.
            call init_reservoirs('coeff_reserv.txt')

            !> Read initial storage and outflow states from file.
            if (RESUMEFLAG == 4 .or. RESUMEFLAG == 5) then
                iun = 100
                open(iun, file = 'zone_storage_states.txt', action = 'read', status = 'old')
                read(iun, *) (resrvs%rsvr(i)%stoSIM(1), i = 1, resrvs%nreserv)
                read(iun, *) (resrvs%rsvr(i)%flowSIM(1), i = 1, resrvs%nreserv)
                close(iun)
            end if
        end if

        !> Allocate file object.
        allocate( &
            WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%kmin:WF_RTE_fstflout%kmax), &
            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%kmin:WF_RTE_frsvrout%kmax))
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%fn = 'MESH_output_streamflow.csv'
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%iun = 70
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%fn = 'MESH_output_streamflow_ts.csv'
        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%iun = 71

        allocate(WF_QO2_ACC(NA), WF_QO2_ACC_MM(NA), WF_STORE2_ACC_MM(NA))
        WF_QO2_ACC = 0.0
        WF_QO2_ACC_MM = 0.0
        WF_STORE2_ACC_MM = 0.0

        if (NR > 0) then

            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KDLY)%fn = 'MESH_output_reach.csv'
            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KDLY)%iun = 708
            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%fn = 'MESH_output_reach_ts.csv'
            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%iun = 708+NR
!            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KHLY)%fn = 'MESH_output_reach_Hourly.csv'
!            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KHLY)%iun = 708+(NR*2)

            if (len_trim(REACHOUTFLAG) == 0) REACHOUTFLAG = 'REACHOUTFLAG default'
            call parse(REACHOUTFLAG, ' ', out_args, nargs)
            WF_RTE_frsvrout%freq = 0
            do j = 2, nargs
                select case (lowercase(out_args(j)))
                    case ('daily')
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KDLY)**WF_RTE_frsvrout%KDLY
                    case ('ts')
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KTS)**WF_RTE_frsvrout%KTS
                    case ('hourly')
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KHLY)**WF_RTE_frsvrout%KHLY
                    case ('default')
                        WF_RTE_frsvrout%freq = 0
                        exit
                    case ('no_header')
                        WF_RTE_frsvrout%fout_header = .false.
                    case ('all')
                        WF_RTE_frsvrout%freq = 0
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KDLY)**WF_RTE_frsvrout%KDLY
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KTS)**WF_RTE_frsvrout%KTS
                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KHLY)**WF_RTE_frsvrout%KHLY
                        exit
                    case ('none')
                        WF_RTE_frsvrout%freq = 0
                        exit
                end select
            end do

            !> Open output files for reaches.
            do j = WF_RTE_frsvrout%kmin, WF_RTE_frsvrout%kmax
!temp: Code missing to write hourly values
                if (j == WF_RTE_frsvrout%KHLY) cycle
                if (btest(WF_RTE_frsvrout%freq, j)) then
                    do i = 1, fms%rsvr%n
                        iun = WF_RTE_frsvrout%fls%fl(j)%iun + i
                        write(ffmti, '(i3)') i
                        fn = trim(adjustl(WF_RTE_frsvrout%fls%fl(j)%fn))
                        call insertstr(fn, trim(adjustl(ffmti)), index(fn, 'reach') + len_trim('reach'))
                        open(iun, &
                             file = './' // trim(fls%GENDIR_OUT) // '/' // fn, &
                             status = 'unknown', action = 'write', &
                             iostat = ierr)
                        if (WF_RTE_frsvrout%fout_header) then
                            write(iun, 1010, advance = 'no') 'YEAR', 'DAY'
                            if (j == WF_RTE_frsvrout%KTS .or. j == WF_RTE_frsvrout%KHLY) write(iun, 1010, advance = 'no') 'HOUR'
                            if (j == WF_RTE_frsvrout%KTS) write(iun, 1010, advance = 'no') 'MINS'
                            write(iun, 1010, advance = 'no') 'QISIM', 'STGCH', 'QOSIM'
                            write(iun, *)
                        end if
                    end do
                end if
            end do

            iun = 707
            open(iun, file = './' // trim(fls%GENDIR_OUT) // '/' // 'MESH_output_lake_level.csv', &
                 status = 'unknown', action = 'write')
            write(iun, 1010, advance = 'no') 'YEAR', 'DAY'
            do l = 1, fms%rsvr%n
                write(ffmti, '(i3)') l
                write(iun, 1010, advance = 'no') 'LVLSIM' // trim(adjustl(ffmti))
            end do
            write(iun, *)
        end if

        if (NS > 0) then
            allocate(WF_QHYD_CUM(NS))
            WF_QHYD_CUM = 0.0

            if (len_trim(STREAMFLOWOUTFLAG) == 0) STREAMFLOWOUTFLAG = 'STREAMFLOWOUTFLAG default'
            call parse(STREAMFLOWOUTFLAG, ' ', out_args, nargs)
            WF_RTE_fstflout%freq = 0
            do j = 2, nargs
                select case (lowercase(out_args(j)))
                    case ('daily')
                        WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
                    case ('ts')
                        WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KTS)**WF_RTE_fstflout%KTS
                    case ('bal')
                        WF_RTE_fstflout%fout_bal = .true.
                    case ('acc')
                        WF_RTE_fstflout%fout_acc = .true.
                    case ('default')
                        WF_RTE_fstflout%freq = radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
                        WF_RTE_fstflout%fout_hyd = .true.
                        WF_RTE_fstflout%fout_bal = .false.
                        WF_RTE_fstflout%fout_acc = .false.
                        WF_RTE_fstflout%fout_header = .true.
                        exit
                    case ('no_header')
                        WF_RTE_fstflout%fout_header = .false.
                    case ('all')
                        WF_RTE_fstflout%freq = radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
                        WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KTS)**WF_RTE_fstflout%KTS
                        WF_RTE_fstflout%fout_hyd = .true.
                        WF_RTE_fstflout%fout_bal = .true.
                        WF_RTE_fstflout%fout_acc = .true.
                        exit
                    case ('none')
                        WF_RTE_fstflout%freq = 0
                        exit
                end select
            end do

            !> Open output files for streamflow.
            do j = WF_RTE_fstflout%kmin, WF_RTE_fstflout%kmax
                if (btest(WF_RTE_fstflout%freq, j)) then
                    iun = WF_RTE_fstflout%fls%fl(j)%iun
                    open(iun, &
                         file = './' // trim(fls%GENDIR_OUT) // '/' // trim(adjustl(WF_RTE_fstflout%fls%fl(j)%fn)), &
                         status = 'unknown', action = 'write', &
                         iostat = ierr)
                    if (WF_RTE_fstflout%fout_header) then
                        write(iun, 1010, advance = 'no') 'YEAR', 'DAY'
                        if (j == WF_RTE_fstflout%KTS) write(iun, 1010, advance = 'no') 'HOUR', 'MINS'
                        do i = 1, fms%stmg%n
                            write(ffmti, '(i3)') i
                            if (WF_RTE_fstflout%fout_acc) then
                                write(iun, 1010, advance = 'no') 'QOMACC' // trim(adjustl(ffmti)), 'QOSACC' // trim(adjustl(ffmti))
                            end if
                            if (WF_RTE_fstflout%fout_hyd) then
                                write(iun, 1010, advance = 'no') 'QOMEAS' // trim(adjustl(ffmti)), 'QOSIM' // trim(adjustl(ffmti))
                            end if
                            if (WF_RTE_fstflout%fout_bal) then
                                write(iun, 1010, advance = 'no') 'RSIM' // trim(adjustl(ffmti)), 'STGCH' // trim(adjustl(ffmti))
                            end if
                        end do
                        write(iun, *)
                    end if
                end if
            end do
        end if

        !> Allocate output variables.
        call output_variables_allocate(out%d%grid%qi, shd%NA)
        call output_variables_allocate(out%d%grid%stgch, shd%NA)
        call output_variables_allocate(out%d%grid%qo, shd%NA)
        call output_variables_allocate(out%d%grid%zlvl, shd%NA)

        !> Call processes.
        call SA_RTE_init(shd)
        call WF_ROUTE_init(fls, shd)
        call run_rte_init(fls, shd)
        call runci_between_grid_init(shd, fls)

!>>>temp_diversion
        !> Enable diversion sets if the file exists.
        inquire(file = fn_div, exist = run_div)
        if (run_div) then

            !> Meta information.
            n_div = 0
            open(iun_div, file = fn_div, status = 'old', action = 'read')
            read(iun_div, *) n_div, dt_div, iyear_div, ijday_div !number of sets; dt (hours); start year; start jday
            if (n_div <= 0) then
                print *, "Bad number of diversions from '" // fn_div // "'."
                print *, 'To remove diversions rename or remove the file.'
                stop
            end if
            allocate( &
                x_src(n_div), y_src(n_div), x_snk(n_div), y_snk(n_div), in_div_m3(n_div), in_div_m3s(n_div), tnsfr_div(n_div), &
                jx_src(n_div), iy_src(n_div), jx_snk(n_div), iy_snk(n_div), rnk_src(n_div), rnk_snk(n_div))
            in_div_m3 = 0.0; in_div_m3s = 0.0
            do l = 1, n_div
                read(iun_div, *) x_src(l), y_src(l), x_snk(l), y_snk(l)
            end do

            !> Find the x-y cell coordinate of the locations.
            iy_src = int((y_src - shd%yOrigin)/shd%yDelta) + 1
            jx_src = int((x_src - shd%xOrigin)/shd%xDelta) + 1
            iy_snk = int((y_snk - shd%yOrigin)/shd%yDelta) + 1
            jx_snk = int((x_snk - shd%xOrigin)/shd%xDelta) + 1

            !> Find RANK at the locations.
            rnk_src = 0
            rnk_snk = 0
            do l = 1, n_div
                do n = 1, shd%NA
                    if (jx_src(l) == shd%xxx(n) .and. iy_src(l) == shd%yyy(n)) rnk_src(l) = n
                    if (jx_snk(l) == shd%xxx(n) .and. iy_snk(l) == shd%yyy(n)) rnk_snk(l) = n
                end do
            end do

            !> Skip records in the file to the simulation start date.
            !> Units of the records interval is hours.
            isteps1 = jday_to_tsteps(iyear_div, ijday_div, 0, 0, dt_div*60)
            isteps2 = jday_to_tsteps(ic%start%year, ic%start%jday, ic%start%hour, ic%start%mins, dt_div*60)
            if (isteps2 < isteps1) then
                call print_warning('The first record occurs after the simulation start date.', PAD_3)
                call print_message_detail('This may cause channels to initialize with no storage.')
                write(line, "(i5, i4)") iyear_div, ijday_div
                call print_message_detail('First record occurs on: ' // trim(line))
                write(line, "(i5, i4)") ic%start%year, ic%start%jday
                call print_message_detail('Simulation start date: ' // trim(line))
            end if
            iskip = (isteps2 - isteps1)
            if (iskip > 0) then
                write(line, FMT_GEN) iskip
                call print_message_detail('Skipping ' // trim(adjustl(line)) // ' records.')
                ierr = read_records_txt(iun_div, in_div_m3s, iskip)
                if (ierr /= 0) then
                    call print_warning('Reached end of file.', PAD_3)
                end if
            end if

            !> Print a summary of locations to file.
            write(line, FMT_GEN) n_div
            call print_message_detail('Number of diversion point sets: ' // trim(adjustl(line)))
            write(line, FMT_GEN) 'SET', 'SOURCE', 'IY', 'JX', 'RANK', 'SINK', 'IY', 'JX', 'RANK'
            call print_echo_txt(line, PAD_3)
            do l = 1, n_div
                write(line, FMT_GEN) &
                    l, '', iy_src(l), jx_src(l), rnk_src(l), '', iy_snk(l), jx_snk(l), rnk_snk(l)
                call print_echo_txt(line, PAD_3)
            end do
            call print_echo_txt('')
        end if
!<<<temp_diversion

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_between_grid(fls, shd, cm)

        !> Process modules.
        use SA_RTE_module
        use WF_ROUTE_module
        use rte_module
        use cropland_irrigation_between_grid

!temp: Outputs.
        use FLAGS
        use txt_io

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer k, ki, ierr

        !> Local variables.
        integer l, i, iun

!>>>temp_diversion
        character(len = 4) ffmti
        character(len = 200) fn
        integer n
!<<<temp_diversion

        !> SCA variables
        real TOTAL_AREA, FRAC, basin_SCA, basin_SWE

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Read in reservoir release values if such a type of reservoir has been defined.
        if (fms%rsvr%n > 0) then
            if (count(fms%rsvr%rls%b1 == 0.0) > 0) then

                !> The minimum time-stepping of the reservoir file is hourly.
                if (mod(ic%now%hour, fms%rsvr%rlsmeas%dts) == 0 .and. ic%now%mins == 0) then
                    ierr = read_records_txt(fms%rsvr%rlsmeas%fls%iun, fms%rsvr%rlsmeas%val)

                    !> Stop if no releases exist.
                    if (ierr /= 0) then
                        print "(3x, 'ERROR: End of file reached when reading from ', (a), '.')", &
                            trim(adjustl(fms%rsvr%rlsmeas%fls%fname))
                        stop
                    end if
                end if
            end if
        end if

        !> Read in observed streamflow from file for comparison and metrics.
        if (fms%stmg%n > 0) then

            !> The minimum time-stepping of the streamflow file is hourly.
            if (mod(ic%now%hour, fms%stmg%qomeas%dts) == 0 .and. ic%now%mins == 0) then
                ierr = read_records_txt(fms%stmg%qomeas%fls%iun, fms%stmg%qomeas%val)

                !> Assign a dummy value if no flow record exists.
                if (ierr /= 0) then
                    fms%stmg%qomeas%val = out%NO_DATA
                end if
            end if
        end if

!>>>temp_diversion
        if (run_div) then

            !> Read value.
            if (mod(ic%now%hour, dt_div) == 0 .and. ic%now%mins == 0) then
                ierr = read_records_txt(iun_div, in_div_m3s)
                if (ierr /= 0) then
                    if (ipid == 0) print 9990, trim(fn_div)
                    stop
                end if
                in_div_m3 = max(in_div_m3s*ic%dts, 0.0) !unit conversion: m3/s flow to m3 storage
            end if

9990    format(3x, 'ERROR: End of file reached when reading from ', (a), '.')

            !> Apply diversion.
            do l = 1, n_div

                !> Source.
                tnsfr_div(l) = 0.0
                n = rnk_src(l)
                if (n >= 1 .and. n <= shd%NAA) then
                    tnsfr_div(l) = min(max(stas_grid%chnl%stg(n) - 0.0, 0.0)*(1.0 - 0.05), in_div_m3(l)) !m3
                    stas_grid%chnl%stg(n) = stas_grid%chnl%stg(n) - tnsfr_div(l)
                end if

                !> Sink.
                n = rnk_snk(l)
                if (n >= 1 .and. n <= shd%NAA) then
                    stas_grid%chnl%stg(n) = stas_grid%chnl%stg(n) + tnsfr_div(l)
                end if

            end do
        end if
!<<<temp_diversion

!>>>temp_diversion
        if (run_div) then
            if (.not. allocated(qo_div_dly)) allocate(qo_div_dly(n_div))
            if (.not. allocated(qa_div_dly)) allocate(qa_div_dly(n_div))
            if (ic%ts_count == 1) then !first time-step
                qo_div_dly = 0.0; qa_div_dly = 0.0
                do l = 1, n_div
                    iun = 2080 + l
                    write(ffmti, '(i4)') l
                    fn = './' // trim(fls%GENDIR_OUT) // '/' // 'MESH_output_diversion' // trim(adjustl(ffmti)) // '.csv'
                    open(unit = iun, file = fn)
                    write(iun, 1010) 'YEAR', 'DAY', 'QODIV', 'QADIV'
                end do
            end if
            qo_div_dly = qo_div_dly + in_div_m3 !m3 storage
            qa_div_dly = qa_div_dly + tnsfr_div !m3 storage
            if (ic%ts_daily == 24*3600/ic%dts) then !daily
                qo_div_dly = qo_div_dly/(ic%dts*ic%ts_daily) !m3/s flow
                qa_div_dly = qa_div_dly/(ic%dts*ic%ts_daily) !m3/s flow
                do l = 1, n_div
                    iun = 2080 + l
                    write(iun, 1010) ic%now%year, ic%now%jday, qo_div_dly(l), qa_div_dly(l)
                end do
                qo_div_dly = 0.0
                qa_div_dly = 0.0
            end if
        end if
!<<<temp_diversion

        !> calculate and write the basin avg SCA similar to watclass3.0f5
        !> Same code than in wf_ensim.f subrutine of watclass3.0f8
        !> Especially for version MESH_Prototype 3.3.1.7b (not to be incorporated in future versions)
        !> calculate and write the basin avg SWE using the similar fudge factor!!!
        if (BASINSWEOUTFLAG > 0) then

            if (ic%now%hour == 12 .and. ic%now%mins == 0) then
                basin_SCA = 0.0
                basin_SWE = 0.0
                TOTAL_AREA = sum(shd%FRAC)
                do k = 1, shd%lc%NML
                    ki = shd%lc%ILMOS(k)
                    FRAC = shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*shd%FRAC(shd%lc%ILMOS(k))
                    basin_SCA = basin_SCA + stas%sno%fsno(k)*FRAC
                    basin_SWE = basin_SWE + stas%sno%sno(k)*FRAC
                end do
                basin_SCA = basin_SCA/TOTAL_AREA
                basin_SWE = basin_SWE/TOTAL_AREA
                if (BASINSWEOUTFLAG > 0) then
                    write(85, "(i5,',', f10.3)") ic%now%jday, basin_SCA
                    write(86, "(i5,',', f10.3)") ic%now%jday, basin_SWE
                end if
            end if

        end if !(ipid == 0) then

        !> Update variables.
        stas_grid%chnl%rff = (stas_grid%sfc%rofo + stas_grid%sl%rofs)*ic%dts
        stas_grid%chnl%rchg = (stas_grid%lzs%rofb + stas_grid%dzs%rofb)*ic%dts

        !> Call processes.
        call SA_RTE(shd)
        call WF_ROUTE_between_grid(fls, shd)
        call run_rte_between_grid(fls, shd)
        call runci_between_grid(shd, fls, cm)

        !> Update output variables.
!todo: remove this when code for output files has moved.
        call output_variables_update(shd, cm)

        if (mod(ic%ts_hourly*ic%dts, RTE_TS) == 0) then

            where (shd%DA > 0.0)
                WF_QO2_ACC_MM = WF_QO2_ACC_MM + stas_grid%chnl%qo/shd%DA/1000.0*RTE_TS
                WF_STORE2_ACC_MM = WF_STORE2_ACC_MM + stas_grid%chnl%stg/shd%DA/1000.0
            elsewhere
                WF_QO2_ACC_MM = out%NO_DATA
                WF_STORE2_ACC_MM = out%NO_DATA
            end where

            !> Write per time-step output for reaches.
            !> Divide by number of time-steps in routing time-step to resolve issues when RTE_TS > ic%dts.
            if (btest(WF_RTE_frsvrout%freq, WF_RTE_frsvrout%KTS)) then
                do l = 1, fms%rsvr%n
                    iun = WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%iun + l
                    write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins
                    write(iun, 1010, advance = 'no') &
                        out%ts%grid%qi(fms%rsvr%meta%rnk(l))/real(RTE_TS/ic%dts), &
                        out%ts%grid%stgch(fms%rsvr%meta%rnk(l))/real(RTE_TS/ic%dts), &
                        out%ts%grid%qo(fms%rsvr%meta%rnk(l))/real(RTE_TS/ic%dts)
                    write(iun, *)
                end do
            end if

            !> Write per time-step output for streamflow.
            !> Divide by number of time-steps in routing time-step to resolve issues when RTE_TS > ic%dts.
            if (btest(WF_RTE_fstflout%freq, WF_RTE_fstflout%KTS)) then
                iun = WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%iun
                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins
                do i = 1, fms%stmg%n
!todo
                    if (WF_RTE_fstflout%fout_acc) write(iun, 1010, advance = 'no') out%NO_DATA, out%NO_DATA
                    if (WF_RTE_fstflout%fout_hyd) then
                        write(iun, 1010, advance = 'no') &
                            fms%stmg%qomeas%val(i), &
                            out%ts%grid%qo(fms%stmg%meta%rnk(i))/real(RTE_TS/ic%dts)
                    end if
!todo
                    if (WF_RTE_fstflout%fout_bal) write(iun, 1010, advance = 'no') out%NO_DATA, out%NO_DATA
                end do
                write(iun, *)
            end if

        end if

        !> This occurs the last time-step of the day.
        if (ic%now%day /= ic%next%day) then

            if (fms%rsvr%n > 0) then
                where (out%d%grid%stgch(fms%rsvr%meta%rnk(:)) > 0.0 .and. fms%rsvr%rls%area > 0.0)
                    out%d%grid%zlvl(fms%rsvr%meta%rnk(:)) = out%d%grid%stgch(fms%rsvr%meta%rnk(:))/fms%rsvr%rls%area
                elsewhere
                    out%d%grid%zlvl(fms%rsvr%meta%rnk(:)) = out%NO_DATA
                end where
                iun = 707
                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
                write(iun, 1010, advance = 'no') (out%d%grid%zlvl(fms%rsvr%meta%rnk(l)), l = 1, fms%rsvr%n)
                write(iun, *)
                if (btest(WF_RTE_frsvrout%freq, WF_RTE_frsvrout%KDLY)) then
                    do l = 1, fms%rsvr%n
                        iun = WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KDLY)%iun + l
                        write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
                        write(iun, 1010, advance = 'no') &
                            out%d%grid%qi(fms%rsvr%meta%rnk(l)), &
                            out%d%grid%stgch(fms%rsvr%meta%rnk(l)), &
                            out%d%grid%qo(fms%rsvr%meta%rnk(l))
                        write(iun, *)
                    end do
                end if
            end if

            do i = 1, fms%stmg%n
                if (fms%stmg%qomeas%val(i) /= fms%stmg%qomeas%val(i)) then
                    WF_QHYD_CUM(i) = WF_QHYD_CUM(i) + fms%stmg%qomeas%val(i)
                else
                    WF_QHYD_CUM(i) = out%NO_DATA
                end if
            end do

            !> Write daily output for streamflow.
            if (btest(WF_RTE_fstflout%freq, WF_RTE_fstflout%KDLY)) then
                WF_QO2_ACC = WF_QO2_ACC + out%d%grid%qo
                where (WF_STORE2_ACC_MM /= out%NO_DATA) WF_STORE2_ACC_MM = WF_STORE2_ACC_MM/ic%ts_count
                iun = WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%iun
                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
                do i = 1, fms%stmg%n
                    if (WF_RTE_fstflout%fout_acc) write(iun, 1010, advance = 'no') &
                        WF_QHYD_CUM(i), WF_QO2_ACC(fms%stmg%meta%rnk(i))
                    if (WF_RTE_fstflout%fout_hyd) write(iun, 1010, advance = 'no') &
                        fms%stmg%qomeas%val(i), out%d%grid%qo(fms%stmg%meta%rnk(i))
                    if (WF_RTE_fstflout%fout_bal) write(iun, 1010, advance = 'no') &
                        WF_QO2_ACC_MM(fms%stmg%meta%rnk(i)), WF_STORE2_ACC_MM(fms%stmg%meta%rnk(i))
                end do
                write(iun, *)
            end if
        end if

1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_between_grid_finalize(fls, shd, cm)

        !> Process modules.
        use WF_ROUTE_config
        use rte_module
        use reservoir
        use FLAGS

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer iun, i

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Call processes.
        call WF_ROUTE_finalize(fls, shd)
        call run_rte_finalize(fls, shd)

        !> For zone-based storage.
        if (RESERVOIRFLAG == 2) then

            !> Save previous time-step storage and outflow states to file.
            if (SAVERESUMEFLAG == 4 .or. SAVERESUMEFLAG == 5) then
                iun = 100
                open(iun, file = 'zone_storage_states.txt', action = 'write', status = 'replace')
                write(iun, *) (resrvs%rsvr(i)%stoSIM(1), i = 1, resrvs%nreserv), '# Intstor1(1:NRESV)'
                write(iun, *) (resrvs%rsvr(i)%flowSIM(1), i = 1, resrvs%nreserv), '# flowO1(1:NRESV)'
                close(iun)
            end if
        end if

    end subroutine

end module
