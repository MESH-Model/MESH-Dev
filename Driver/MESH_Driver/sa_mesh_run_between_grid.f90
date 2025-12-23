module sa_mesh_run_between_grid

    !> 'model_files_variables' required for 'fls' object and file keys.
    !> 'sa_mesh_common' required for common SA_MESH variables and routines.
    !> 'model_dates' required for 'ic' counter.
    !> 'mpi_module' required for MPI variables, tile/grid parsing utility, barrier flag.
    use model_files_variables
    use sa_mesh_common
    use model_dates
    use mpi_module

!temp: Outputs.
!-    use model_files_variabletypes, only: fl_ids

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
!-    type WF_RTE_fout_stfl
!-        integer :: KDLY = 0, KTS = 1
!-        integer :: kmin = 0, kmax = 1
!-        integer :: freq = 1
!-        logical :: fout_hyd = .true., fout_bal = .false., fout_acc = .false.
!-        logical :: fout_header = .true.
!-        type(fl_ids) :: fls
!-    end type

    !> Variable type: WF_RTE_fout_rsvr
    !>  Description: Internal file keys used for output files for lakes and reservoirs.
    !>
    !> Variables:
    !*  KTS: Per time-step output
    !*  freq: Time intervals of the output (ts).
    !*  fout_header: .true. to print header (default).
    !*  fls: Output file definitions.
!-    type WF_RTE_fout_rsvr
!-        integer :: KDLY = 0, KTS = 1, KHLY = 2
!-        integer :: kmin = 0, kmax = 2
!-        integer :: freq = 0
!-        logical :: fout_header = .true.
!-        type(fl_ids) :: fls
!-    end type

    !> Output files
!-    type(WF_RTE_fout_stfl), save :: WF_RTE_fstflout
!-    type(WF_RTE_fout_rsvr), save :: WF_RTE_frsvrout

!-    real, dimension(:), allocatable :: WF_QHYD_CUM

!todo: Move to ro%?
!-    integer RTE_TS

!-    real, dimension(:), allocatable :: WF_QO2_ACC, WF_QO2_ACC_MM, WF_STORE2_ACC_MM

!>>>temp_diversion
    integer :: iun_div = 64, n_div, dt_div, iyear_div, ijday_div
    character(len = 13) :: fn_div = 'diversion.txt'
    real, dimension(:), allocatable :: x_src, y_src, x_snk, y_snk, in_div_m3, in_div_m3s, tnsfr_div
    integer, dimension(:), allocatable :: jx_src, iy_src, jx_snk, iy_snk, rnk_src, rnk_snk
    logical run_div
    real, dimension(:), allocatable :: qo_div_dly, qa_div_dly
!<<<temp_diversion

    contains

    subroutine run_between_grid_init(fls, shd)

        !> Process modules.
!-        use SA_RTE_module
        use WF_ROUTE_config
        use reservoir
        use rte_module
        use cropland_irrigation_between_grid

!>>>temp_diversion
        use txt_io
        use FLAGS
!<<<temp_diversion
!temp: Outputs.
!-        use save_basin_output, only: STREAMFLOWOUTFLAG, REACHOUTFLAG
!-        use FLAGS
!-        use strings

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
!-        integer, parameter :: MaxLenField = 20, MaxArgs = 20, MaxLenLine = 100
!-        integer NA
!-        integer NS, NR
!-        character(len = 4) ffmti
!-        character(len = 500) fn
!-        integer iun, ierr, l, j, i
!-        character(MaxLenField), dimension(MaxArgs) :: out_args
!-        integer nargs

!>>>temp_diversion
        integer l, n, isteps1, isteps2, iskip, ierr
        character(len = DEFAULT_LINE_LENGTH) line
!<<<temp_diversion

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

!-        if (BASINSWEOUTFLAG > 0) then
!-            open(85, file = './' // trim(fls%GENDIR_OUT) // '/basin_SCA_alldays.csv')
!-            open(86, file = './' // trim(fls%GENDIR_OUT) // '/basin_SWE_alldays.csv')
!-        end if !(BASINSWEOUTFLAG > 0) then

!-        RTE_TS = ic%dts
!-        if (WF_RTE_flgs%PROCESS_ACTIVE) RTE_TS = WF_RTE_flgs%RTE_TS
!-        if (rteflg%PROCESS_ACTIVE) RTE_TS = rteflg%RTE_TS

!-        NA = shd%NA
!-        NR = fms%rsvr%n
!-        NS = fms%stmg%n

        !> Read configuration for zone-based storage.
        if (RESERVOIRFLAG == 2) call init_reservoirs('coeff_reserv.txt')

        !> Allocate file object.
!-        allocate( &
!-            WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%kmin:WF_RTE_fstflout%kmax), &
!-            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%kmin:WF_RTE_frsvrout%kmax))
!-        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%fn = 'MESH_output_streamflow.csv'
!-        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%iun = 70
!-        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%fn = 'MESH_output_streamflow_ts.csv'
!-        WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%iun = 71

!-        allocate(WF_QO2_ACC(NA), WF_QO2_ACC_MM(NA), WF_STORE2_ACC_MM(NA))
!-        WF_QO2_ACC = 0.0
!-        WF_QO2_ACC_MM = 0.0
!-        WF_STORE2_ACC_MM = 0.0

!-        if (NR > 0) then

!-            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KDLY)%fn = 'MESH_output_reach.csv'
!-            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KDLY)%iun = 708
!-            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%fn = 'MESH_output_reach_ts.csv'
!-            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%iun = 708+NR
!-            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KHLY)%fn = 'MESH_output_reach_Hourly.csv'
!-            WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KHLY)%iun = 708+(NR*2)

!-            if (len_trim(REACHOUTFLAG) == 0) REACHOUTFLAG = 'REACHOUTFLAG default'
!-            call parse(REACHOUTFLAG, ' ', out_args, nargs)
!-            WF_RTE_frsvrout%freq = 0
!-            do j = 2, nargs
!-                select case (lowercase(out_args(j)))
!-                    case ('daily')
!-                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KDLY)**WF_RTE_frsvrout%KDLY
!-                    case ('ts')
!-                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KTS)**WF_RTE_frsvrout%KTS
!-                    case ('hourly')
!-                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KHLY)**WF_RTE_frsvrout%KHLY
!-                    case ('default')
!-                        WF_RTE_frsvrout%freq = 0
!-                        exit
!-                    case ('no_header')
!-                        WF_RTE_frsvrout%fout_header = .false.
!-                    case ('all')
!-                        WF_RTE_frsvrout%freq = 0
!-                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KDLY)**WF_RTE_frsvrout%KDLY
!-                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KTS)**WF_RTE_frsvrout%KTS
!-                        WF_RTE_frsvrout%freq = WF_RTE_frsvrout%freq + radix(WF_RTE_frsvrout%KHLY)**WF_RTE_frsvrout%KHLY
!-                        exit
!-                    case ('none')
!-                        WF_RTE_frsvrout%freq = 0
!-                        exit
!-                end select
!-            end do

            !> Open output files for reaches.
!-            do j = WF_RTE_frsvrout%kmin, WF_RTE_frsvrout%kmax
!temp: Code missing to write hourly values
!-                if (j == WF_RTE_frsvrout%KHLY) cycle
!-                if (btest(WF_RTE_frsvrout%freq, j)) then
!-                    do i = 1, fms%rsvr%n
!-                        iun = WF_RTE_frsvrout%fls%fl(j)%iun + i
!-                        write(ffmti, '(i3)') i
!-                        fn = trim(adjustl(WF_RTE_frsvrout%fls%fl(j)%fn))
!-                        call insertstr(fn, trim(adjustl(ffmti)), index(fn, 'reach') + len_trim('reach'))
!-                        open(iun, &
!-                             file = './' // trim(fls%GENDIR_OUT) // '/' // fn, &
!-                             status = 'unknown', action = 'write', &
!-                             iostat = ierr)
!-                        if (WF_RTE_frsvrout%fout_header) then
!-                            write(iun, 1010, advance = 'no') VN_YEAR, VN_JDAY
!-                            if (j == WF_RTE_frsvrout%KTS .or. j == WF_RTE_frsvrout%KHLY) write(iun, 1010, advance = 'no') VN_HOUR
!-                            if (j == WF_RTE_frsvrout%KTS) write(iun, 1010, advance = 'no') VN_MINS
!-                            write(iun, 1010, advance = 'no') VN_QI, VN_STGCH, VN_QO
!-                            write(iun, *)
!-                        end if
!-                    end do
!-                end if
!-            end do

!-            iun = 707
!-            open(iun, file = './' // trim(fls%GENDIR_OUT) // '/' // 'MESH_output_lake_level.csv', &
!-                 status = 'unknown', action = 'write')
!-            write(iun, 1010, advance = 'no') VN_YEAR, VN_JDAY
!-            do l = 1, fms%rsvr%n
!-                write(ffmti, '(i3)') l
!-                write(iun, 1010, advance = 'no') VN_ZLVL // trim(adjustl(ffmti))
!-            end do
!-            write(iun, *)
!-        end if

!-        if (NS > 0) then
!-            allocate(WF_QHYD_CUM(NS))
!-            WF_QHYD_CUM = 0.0

!-            if (len_trim(STREAMFLOWOUTFLAG) == 0) STREAMFLOWOUTFLAG = 'STREAMFLOWOUTFLAG default'
!-            call parse(STREAMFLOWOUTFLAG, ' ', out_args, nargs)
!-            WF_RTE_fstflout%freq = 0
!-            do j = 2, nargs
!-                select case (lowercase(out_args(j)))
!-                    case ('daily')
!-                        WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
!-                    case ('ts')
!-                        WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KTS)**WF_RTE_fstflout%KTS
!-                    case ('bal')
!-                        WF_RTE_fstflout%fout_bal = .true.
!-                    case ('acc')
!-                        WF_RTE_fstflout%fout_acc = .true.
!-                    case ('default')
!-                        WF_RTE_fstflout%freq = radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
!-                        WF_RTE_fstflout%fout_hyd = .true.
!-                        WF_RTE_fstflout%fout_bal = .false.
!-                        WF_RTE_fstflout%fout_acc = .false.
!-                        WF_RTE_fstflout%fout_header = .true.
!-                        exit
!-                    case ('no_header')
!-                        WF_RTE_fstflout%fout_header = .false.
!-                    case ('all')
!-                        WF_RTE_fstflout%freq = radix(WF_RTE_fstflout%KDLY)**WF_RTE_fstflout%KDLY
!-                        WF_RTE_fstflout%freq = WF_RTE_fstflout%freq + radix(WF_RTE_fstflout%KTS)**WF_RTE_fstflout%KTS
!-                        WF_RTE_fstflout%fout_hyd = .true.
!-                        WF_RTE_fstflout%fout_bal = .true.
!-                        WF_RTE_fstflout%fout_acc = .true.
!-                        exit
!-                    case ('none')
!-                        WF_RTE_fstflout%freq = 0
!-                        exit
!-                end select
!-            end do

            !> Open output files for streamflow.
!-            do j = WF_RTE_fstflout%kmin, WF_RTE_fstflout%kmax
!-                if (btest(WF_RTE_fstflout%freq, j)) then
!-                    iun = WF_RTE_fstflout%fls%fl(j)%iun
!-                    open(iun, &
!-                         file = './' // trim(fls%GENDIR_OUT) // '/' // trim(adjustl(WF_RTE_fstflout%fls%fl(j)%fn)), &
!-                         status = 'unknown', action = 'write', &
!-                         iostat = ierr)
!-                    if (WF_RTE_fstflout%fout_header) then
!-                        write(iun, 1010, advance = 'no') VN_YEAR, VN_JDAY
!-                        if (j == WF_RTE_fstflout%KTS) write(iun, 1010, advance = 'no') VN_HOUR, VN_MINS
!-                        do i = 1, fms%stmg%n
!-                            write(ffmti, '(i3)') i
!-                            if (WF_RTE_fstflout%fout_acc) then
!-                                write(iun, 1010, advance = 'no') &
!-                                    VN_QO // VN_MEAS // VN_ACC // trim(adjustl(ffmti)), &
!-                                    VN_QO // VN_SIM // VN_ACC // trim(adjustl(ffmti))
!-                            end if
!-                            if (WF_RTE_fstflout%fout_hyd) then
!-                                write(iun, 1010, advance = 'no') &
!-                                    VN_QO // VN_MEAS // trim(adjustl(ffmti)), VN_QO // VN_SIM // trim(adjustl(ffmti))
!-                            end if
!-                            if (WF_RTE_fstflout%fout_bal) then
!-                                write(iun, 1010, advance = 'no') &
!-                                    'RSIM' // trim(adjustl(ffmti)), VN_STGCH // trim(adjustl(ffmti))
!-                            end if
!-                        end do
!-                        write(iun, *)
!-                    end if
!-                end if
!-            end do
!-        end if

        !> Allocate output variables.
!-        call output_variables_activate(out%d%grid, (/ VN_DUMMY_LENGTH, VN_QI, VN_STGCH, VN_QO, VN_ZLVL /))

        !> Call processes.
!-        call SA_RTE_init(shd)
        call WF_ROUTE_init(fls, shd)
        call run_rte_init(fls, shd)
        call runci_between_grid_init(shd, fls)

!>>>temp_diversion
        !> Enable diversion sets if the file exists.
        inquire(file = fn_div, exist = run_div)
        if (run_div) then

            !> Meta information.
            call reset_tab()
            call print_message("READING: " // trim(fn_div))
            call increase_tab()
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
                call print_warning('The first record occurs after the simulation start date.')
                call print_message('This may cause channels to initialize with no storage.')
                write(line, "(i5, i4)") iyear_div, ijday_div
                call print_message('First record occurs on: ' // trim(line))
                write(line, "(i5, i4)") ic%start%year, ic%start%jday
                call print_message('Simulation start date: ' // trim(line))
            end if
            iskip = (isteps2 - isteps1)
            if (iskip > 0) then
                write(line, FMT_GEN) iskip
                call print_message('Skipping ' // trim(adjustl(line)) // ' records.')
                ierr = read_records_txt(iun_div, in_div_m3s, iskip)
                if (ierr /= 0) then
                    call print_warning('Reached end of file.')
                end if
            end if

            !> Print a summary of locations to file.
            write(line, FMT_GEN) n_div
            call print_message('Number of diversion point sets: ' // trim(adjustl(line)))
            write(line, FMT_GEN) 'SET', 'SOURCE', 'IY', 'JX', 'RANK', 'SINK', 'IY', 'JX', 'RANK'
            call print_echo_txt(trim(line))
            do l = 1, n_div
                write(line, FMT_GEN) &
                    l, '', iy_src(l), jx_src(l), rnk_src(l), '', iy_snk(l), jx_snk(l), rnk_snk(l)
                call print_echo_txt(trim(line))
            end do
            call print_echo_txt('')
        end if
!<<<temp_diversion

        !> Update basin variables.
        call run_within_grid_stas_basin_update(fls, shd)

!-1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_between_grid(fls, shd)

        !> Process modules.
!-        use SA_RTE_module
        use WF_ROUTE_module
        use rte_module
        use cropland_irrigation_between_grid

!temp: Outputs.
!-        use FLAGS
        use txt_io

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
!-        integer k, ki
        integer ierr

        !> Local variables.
!-        integer l, i, iun

!>>>temp_diversion
        character(len = 4) ffmti
        character(len = 200) fn
        integer iun, l, n
!<<<temp_diversion

        !> SCA variables
!-        real TOTAL_AREA, FRAC, basin_SCA, basin_SWE

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
                    tnsfr_div(l) = min(max(vs%grid%stgch(n) - 0.0, 0.0)*(1.0 - 0.05), in_div_m3(l)) !m3
                    vs%grid%stgch(n) = vs%grid%stgch(n) - tnsfr_div(l)
                end if

                !> Sink.
                n = rnk_snk(l)
                if (n >= 1 .and. n <= shd%NAA) then
                    vs%grid%stgch(n) = vs%grid%stgch(n) + tnsfr_div(l)
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
                    write(iun, FMT_GEN) 'YEAR', 'DAY', 'QODIV', 'QADIV'
                end do
            end if
            qo_div_dly = qo_div_dly + in_div_m3 !m3 storage
            qa_div_dly = qa_div_dly + tnsfr_div !m3 storage
            if (ic%ts_daily == 24*3600/ic%dts) then !daily
                qo_div_dly = qo_div_dly/(ic%dts*ic%ts_daily) !m3/s flow
                qa_div_dly = qa_div_dly/(ic%dts*ic%ts_daily) !m3/s flow
                do l = 1, n_div
                    iun = 2080 + l
                    write(iun, FMT_GEN) ic%now%year, ic%now%jday, qo_div_dly(l), qa_div_dly(l)
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
!-        if (BASINSWEOUTFLAG > 0) then

!-            if (ic%now%hour == 12 .and. ic%now%mins == 0) then
!-                basin_SCA = 0.0
!-                basin_SWE = 0.0
!-                TOTAL_AREA = sum(shd%FRAC)
!-                do k = 1, shd%lc%NML
!-                    ki = shd%lc%ILMOS(k)
!-                    FRAC = shd%lc%ACLASS(shd%lc%ILMOS(k), shd%lc%JLMOS(k))*shd%FRAC(shd%lc%ILMOS(k))
!-                    basin_SCA = basin_SCA + vs%tile%fsno(k)*FRAC
!-                    basin_SWE = basin_SWE + vs%tile%sno(k)*FRAC
!-                end do
!-                basin_SCA = basin_SCA/TOTAL_AREA
!-                basin_SWE = basin_SWE/TOTAL_AREA
!-                if (BASINSWEOUTFLAG > 0) then
!-                    write(85, "(i5,',', f10.3)") ic%now%jday, basin_SCA
!-                    write(86, "(i5,',', f10.3)") ic%now%jday, basin_SWE
!-                end if
!-            end if

!-        end if !(ipid == 0) then

        !> Update variables.
!-        if (ro%RUNLSS) then
!-            vs%grid%rff = (vs%grid%ovrflw + sum(vs%grid%latflw, 2))*ic%dts
!-            vs%grid%rchg = vs%grid%drainsol*ic%dts
!-        end if

        !> Call processes.
!-        call SA_RTE(shd)
        call WF_ROUTE_between_grid(fls, shd)
        call run_rte_between_grid(fls, shd)
        call runci_between_grid(shd, fls)

        !> Update basin variables.
        call run_within_grid_stas_basin_update(fls, shd)

        !> Update output variables.
!todo: remove this when code for output files has moved.
        call output_variables_update(shd)

!-        if (mod(ic%ts_hourly*ic%dts, RTE_TS) == 0 .and. ro%RUNCHNL) then

!-            where (shd%DA > 0.0)
!-                WF_QO2_ACC_MM = WF_QO2_ACC_MM + vs%grid%qo/shd%DA/1000.0*RTE_TS
!-                WF_STORE2_ACC_MM = WF_STORE2_ACC_MM + vs%grid%stgch/shd%DA/1000.0
!-            elsewhere
!-                WF_QO2_ACC_MM = out%NO_DATA
!-                WF_STORE2_ACC_MM = out%NO_DATA
!-            end where

            !> Write per time-step output for reaches.
            !> Divide by number of time-steps in routing time-step to resolve issues when RTE_TS > ic%dts.
!-            if (btest(WF_RTE_frsvrout%freq, WF_RTE_frsvrout%KTS)) then
!-                do l = 1, fms%rsvr%n
!-                    iun = WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%iun + l
!-                    write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins
!-                    write(iun, 1010, advance = 'no') &
!-                        out%ts%grid%qi(fms%rsvr%meta%rnk(l))/real(RTE_TS/ic%dts), &
!-                        out%ts%grid%stgch(fms%rsvr%meta%rnk(l))/real(RTE_TS/ic%dts), &
!-                        out%ts%grid%qo(fms%rsvr%meta%rnk(l))/real(RTE_TS/ic%dts)
!-                    write(iun, *)
!-                end do
!-            end if

            !> Write per time-step output for streamflow.
            !> Divide by number of time-steps in routing time-step to resolve issues when RTE_TS > ic%dts.
!-            if (btest(WF_RTE_fstflout%freq, WF_RTE_fstflout%KTS)) then
!-                iun = WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%iun
!-                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins
!-                do i = 1, fms%stmg%n
!todo
!-                    if (WF_RTE_fstflout%fout_acc) write(iun, 1010, advance = 'no') out%NO_DATA, out%NO_DATA
!-                    if (WF_RTE_fstflout%fout_hyd) then
!-                        write(iun, 1010, advance = 'no') &
!-                            fms%stmg%qomeas%val(i), &
!-                            out%ts%grid%qo(fms%stmg%meta%rnk(i))/real(RTE_TS/ic%dts)
!-                    end if
!todo
!-                    if (WF_RTE_fstflout%fout_bal) write(iun, 1010, advance = 'no') out%NO_DATA, out%NO_DATA
!-                end do
!-                write(iun, *)
!-            end if

!-        end if

        !> This occurs the last time-step of the day.
!-        if (ic%now%day /= ic%next%day .and. ro%RUNCHNL) then

!-            if (fms%rsvr%n > 0) then
!-                where (out%d%grid%stgch(fms%rsvr%meta%rnk(:)) > 0.0 .and. fms%rsvr%rls%area > 0.0)
!-                    out%d%grid%zlvl(fms%rsvr%meta%rnk(:)) = out%d%grid%stgch(fms%rsvr%meta%rnk(:))/fms%rsvr%rls%area
!-                elsewhere
!-                    out%d%grid%zlvl(fms%rsvr%meta%rnk(:)) = out%NO_DATA
!-                end where
!-                iun = 707
!-                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
!-                write(iun, 1010, advance = 'no') (out%d%grid%zlvl(fms%rsvr%meta%rnk(l)), l = 1, fms%rsvr%n)
!-                write(iun, *)
!-                if (btest(WF_RTE_frsvrout%freq, WF_RTE_frsvrout%KDLY)) then
!-                    do l = 1, fms%rsvr%n
!-                        iun = WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KDLY)%iun + l
!-                        write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
!-                        write(iun, 1010, advance = 'no') &
!-                            out%d%grid%qi(fms%rsvr%meta%rnk(l)), &
!-                            out%d%grid%stgch(fms%rsvr%meta%rnk(l)), &
!-                            out%d%grid%qo(fms%rsvr%meta%rnk(l))
!-                        write(iun, *)
!-                    end do
!-                end if
!-            end if

!-            do i = 1, fms%stmg%n
!-                if (fms%stmg%qomeas%val(i) /= fms%stmg%qomeas%val(i)) then
!-                    WF_QHYD_CUM(i) = WF_QHYD_CUM(i) + fms%stmg%qomeas%val(i)
!-                else
!-                    WF_QHYD_CUM(i) = out%NO_DATA
!-                end if
!-            end do

            !> Write daily output for streamflow.
!-            if (btest(WF_RTE_fstflout%freq, WF_RTE_fstflout%KDLY)) then
!-                WF_QO2_ACC = WF_QO2_ACC + out%d%grid%qo
!-                where (WF_STORE2_ACC_MM /= out%NO_DATA) WF_STORE2_ACC_MM = WF_STORE2_ACC_MM/ic%ts_count
!-                iun = WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%iun
!-                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
!-                do i = 1, fms%stmg%n
!-                    if (WF_RTE_fstflout%fout_acc) write(iun, 1010, advance = 'no') &
!-                        WF_QHYD_CUM(i), WF_QO2_ACC(fms%stmg%meta%rnk(i))
!-                    if (WF_RTE_fstflout%fout_hyd) write(iun, 1010, advance = 'no') &
!-                        fms%stmg%qomeas%val(i), out%d%grid%qo(fms%stmg%meta%rnk(i))
!-                    if (WF_RTE_fstflout%fout_bal) write(iun, 1010, advance = 'no') &
!-                        WF_QO2_ACC_MM(fms%stmg%meta%rnk(i)), WF_STORE2_ACC_MM(fms%stmg%meta%rnk(i))
!-                end do
!-                write(iun, *)
!-            end if
!-        end if

!-1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_within_grid_stas_basin_update(fls, shd)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Local variables.
        integer j, ii, i
        real albtfrac(shd%NA), tpndfrac(shd%NA), tsnofrac(shd%NA), tcanfrac(shd%NA), ticefrac(shd%NA), frac(shd%NA)

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Meteorology/climatology variables.
        if (allocated(vs%basin%fsin) .and. allocated(vs%grid%fsin)) then
            where (vs%grid%fsin /= huge(vs%grid%fsin))
                vs%basin%fsin = vs%grid%fsin*shd%FRAC
            elsewhere
                vs%basin%fsin = 0.0
            end where
        end if
        if (allocated(vs%basin%fsvs) .and. allocated(vs%grid%fsvs)) then
            where (vs%grid%fsvs /= huge(vs%grid%fsvs))
                vs%basin%fsvs = vs%grid%fsvs*shd%FRAC
            elsewhere
                vs%basin%fsvs = 0.0
            end where
        end if
        if (allocated(vs%basin%fsir) .and. allocated(vs%grid%fsir)) then
            where (vs%grid%fsir /= huge(vs%grid%fsir))
                vs%basin%fsir = vs%grid%fsir*shd%FRAC
            elsewhere
                vs%basin%fsir = 0.0
            end where
        end if
        if (allocated(vs%basin%fsdr) .and. allocated(vs%grid%fsdr)) then
            where (vs%grid%fsdr /= huge(vs%grid%fsdr))
                vs%basin%fsdr = vs%grid%fsdr*shd%FRAC
            elsewhere
                vs%basin%fsdr = 0.0
            end where
        end if
        if (allocated(vs%basin%fsdff) .and. allocated(vs%grid%fsdff)) then
            where (vs%grid%fsdff /= huge(vs%grid%fsdff))
                vs%basin%fsdff = vs%grid%fsdff*shd%FRAC
            elsewhere
                vs%basin%fsdff = 0.0
            end where
        end if
        if (allocated(vs%basin%flin) .and. allocated(vs%grid%flin)) then
            where (vs%grid%flin /= huge(vs%grid%flin))
                vs%basin%flin = vs%grid%flin*shd%FRAC
            elsewhere
                vs%basin%flin = 0.0
            end where
        end if
        if (allocated(vs%basin%ta) .and. allocated(vs%grid%ta)) then
            where (vs%grid%ta /= huge(vs%grid%ta))
                vs%basin%ta = vs%grid%ta*shd%FRAC
            elsewhere
                vs%basin%ta = 0.0
            end where
        end if
        if (allocated(vs%basin%qa) .and. allocated(vs%grid%qa)) then
            where (vs%grid%qa /= huge(vs%grid%qa))
                vs%basin%qa = vs%grid%qa*shd%FRAC
            elsewhere
                vs%basin%qa = 0.0
            end where
        end if
        if (allocated(vs%basin%pres) .and. allocated(vs%grid%pres)) then
            where (vs%grid%pres /= huge(vs%grid%pres))
                vs%basin%pres = vs%grid%pres*shd%FRAC
            elsewhere
                vs%basin%pres = 0.0
            end where
        end if
        if (allocated(vs%basin%uu) .and. allocated(vs%grid%uu)) then
            where (vs%grid%uu /= huge(vs%grid%uu))
                vs%basin%uu = vs%grid%uu*shd%FRAC
            elsewhere
                vs%basin%uu = 0.0
            end where
        end if
        if (allocated(vs%basin%vv) .and. allocated(vs%grid%vv)) then
            where (vs%grid%vv /= huge(vs%grid%vv))
                vs%basin%vv = vs%grid%vv*shd%FRAC
            elsewhere
                vs%basin%vv = 0.0
            end where
        end if
        if (allocated(vs%basin%uv) .and. allocated(vs%grid%uv)) then
            where (vs%grid%uv /= huge(vs%grid%uv))
                vs%basin%uv = vs%grid%uv*shd%FRAC
            elsewhere
                vs%basin%uv = 0.0
            end where
        end if
        if (allocated(vs%basin%wdir) .and. allocated(vs%grid%wdir)) then
            where (vs%grid%wdir /= huge(vs%grid%wdir))
                vs%basin%wdir = vs%grid%wdir*shd%FRAC
            elsewhere
                vs%basin%wdir = 0.0
            end where
        end if
        if (allocated(vs%basin%prern) .and. allocated(vs%grid%prern)) then
            where (vs%grid%prern /= huge(vs%grid%prern))
                vs%basin%prern = vs%grid%prern*shd%FRAC
            elsewhere
                vs%basin%prern = 0.0
            end where
        end if
        if (allocated(vs%basin%presno) .and. allocated(vs%grid%presno)) then
            where (vs%grid%presno /= huge(vs%grid%presno))
                vs%basin%presno = vs%grid%presno*shd%FRAC
            elsewhere
                vs%basin%presno = 0.0
            end where
        end if
        if (allocated(vs%basin%pre) .and. allocated(vs%grid%pre)) then
            where (vs%grid%pre /= huge(vs%grid%pre))
                vs%basin%pre = vs%grid%pre*shd%FRAC
            elsewhere
                vs%basin%pre = 0.0
            end where
        end if

        !> Canopy variables.
        if (allocated(vs%basin%lqwscan) .and. allocated(vs%grid%lqwscan)) then
            where (vs%grid%lqwscan /= huge(vs%grid%lqwscan))
                vs%basin%lqwscan = vs%grid%lqwscan*shd%FRAC
            elsewhere
                vs%basin%lqwscan = 0.0
            end where
        end if
        if (allocated(vs%basin%fzwscan) .and. allocated(vs%grid%fzwscan)) then
            where (vs%grid%fzwscan /= huge(vs%grid%fzwscan))
                vs%basin%fzwscan = vs%grid%fzwscan*shd%FRAC
            elsewhere
                vs%basin%fzwscan = 0.0
            end where
        end if
        if (allocated(vs%basin%cmai) .and. allocated(vs%grid%cmai)) then
            where (vs%grid%cmai /= huge(vs%grid%cmai))
                vs%basin%cmai = vs%grid%cmai*shd%FRAC
            elsewhere
                vs%basin%cmai = 0.0
            end where
        end if
        if (allocated(vs%basin%tacan) .and. allocated(vs%grid%tacan)) then
            where (vs%grid%tacan /= huge(vs%grid%tacan))
                vs%basin%tacan = vs%grid%tacan*shd%FRAC
            elsewhere
                vs%basin%tacan = 0.0
            end where
        end if
        if (allocated(vs%basin%qacan) .and. allocated(vs%grid%qacan)) then
            where (vs%grid%qacan /= huge(vs%grid%qacan))
                vs%basin%qacan = vs%grid%qacan*shd%FRAC
            elsewhere
                vs%basin%qacan = 0.0
            end where
        end if
        if (allocated(vs%basin%uvcan) .and. allocated(vs%grid%uvcan)) then
            where (vs%grid%uvcan /= huge(vs%grid%uvcan))
                vs%basin%uvcan = vs%grid%uvcan*shd%FRAC
            elsewhere
                vs%basin%uvcan = 0.0
            end where
        end if
        if (allocated(vs%basin%tcan) .and. allocated(vs%grid%tcan)) then
            where (vs%grid%tcan /= huge(vs%grid%tcan))
                vs%basin%tcan = vs%grid%tcan*shd%FRAC
                where (vs%basin%tcan > 0.0)
                    tcanfrac = shd%FRAC
                elsewhere
                    tcanfrac = 0.0
                end where
            elsewhere
                vs%basin%tcan = 0.0
            end where
        end if
        if (allocated(vs%basin%gro) .and. allocated(vs%grid%gro)) then
            where (vs%grid%gro /= huge(vs%grid%gro))
                vs%basin%gro = vs%grid%gro*shd%FRAC
            elsewhere
                vs%basin%gro = 0.0
            end where
        end if

        !> Snow variables.
        if (allocated(vs%basin%fsno) .and. allocated(vs%grid%fsno)) then
            where (vs%grid%fsno /= huge(vs%grid%fsno))
                vs%basin%fsno = vs%grid%fsno*shd%FRAC
            elsewhere
                vs%basin%fsno = 0.0
            end where
        end if
        if (allocated(vs%basin%sno) .and. allocated(vs%grid%sno)) then
            where (vs%grid%sno /= huge(vs%grid%sno))
                vs%basin%sno = vs%grid%sno*shd%FRAC
            elsewhere
                vs%basin%sno = 0.0
            end where
        end if
        if (allocated(vs%basin%rhosno) .and. allocated(vs%grid%rhosno)) then
            where (vs%grid%rhosno /= huge(vs%grid%rhosno))
                vs%basin%rhosno = vs%grid%rhosno*shd%FRAC
            elsewhere
                vs%basin%rhosno = 0.0
            end where
        end if
!-        if (allocated(vs%basin%zsno) .and. allocated(vs%grid%zsno)) then
!-            if (all(vs%grid%zsno /= huge(vs%grid%zsno))) then
!-                vs%basin%zsno = vs%grid%zsno*shd%FRAC
!-            else
!-                vs%basin%zsno = huge(vs%basin%zsno)
!-            end if
!-        end if
        if (allocated(vs%basin%lqwssno) .and. allocated(vs%grid%lqwssno)) then
            where (vs%grid%lqwssno /= huge(vs%grid%lqwssno))
                vs%basin%lqwssno = vs%grid%lqwssno*shd%FRAC
            elsewhere
                vs%basin%lqwssno = 0.0
            end where
        end if
        if (allocated(vs%basin%tsno) .and. allocated(vs%grid%tsno)) then
            where (vs%grid%tsno /= huge(vs%grid%tsno))
                vs%basin%tsno = vs%grid%tsno*shd%FRAC
                where (vs%basin%tsno > 0.0)
                    tsnofrac = shd%FRAC
                elsewhere
                    tsnofrac = 0.0
                end where
            elsewhere
                vs%basin%tsno = 0.0
            end where
        end if
        if (allocated(vs%basin%albsno) .and. allocated(vs%grid%albsno)) then
            where (vs%grid%albsno /= huge(vs%grid%albsno))
                vs%basin%albsno = vs%grid%albsno*shd%FRAC
            elsewhere
                vs%basin%albsno = 0.0
            end where
        end if
        if (allocated(vs%basin%drainsno) .and. allocated(vs%grid%drainsno)) then
            where (vs%grid%drainsno /= huge(vs%grid%drainsno))
                vs%basin%drainsno = vs%grid%drainsno*shd%FRAC
            elsewhere
                vs%basin%drainsno = 0.0
            end where
        end if

        !> Surface variables.
        if (allocated(vs%basin%albt) .and. allocated(vs%grid%albt)) then
            where (vs%grid%albt /= huge(vs%grid%albt))
                vs%basin%albt = vs%grid%albt*shd%FRAC
                where (vs%basin%albt > 0.0)
                    albtfrac = shd%FRAC
                elsewhere
                    albtfrac = 0.0
                end where
            elsewhere
                vs%basin%albt = 0.0
            end where
        end if
        if (allocated(vs%basin%alvs) .and. allocated(vs%grid%alvs)) then
            where (vs%grid%alvs /= huge(vs%grid%alvs))
                vs%basin%alvs = vs%grid%alvs*shd%FRAC
            elsewhere
                vs%basin%alvs = 0.0
            end where
        end if
        if (allocated(vs%basin%alir) .and. allocated(vs%grid%alir)) then
            where (vs%grid%alir /= huge(vs%grid%alir))
                vs%basin%alir = vs%grid%alir*shd%FRAC
            elsewhere
                vs%basin%alir = 0.0
            end where
        end if
        if (allocated(vs%basin%gte) .and. allocated(vs%grid%gte)) then
            where (vs%grid%gte /= huge(vs%grid%gte))
                vs%basin%gte = vs%grid%gte*shd%FRAC
            elsewhere
                vs%basin%gte = 0.0
            end where
        end if
        if (allocated(vs%basin%zpnd) .and. allocated(vs%grid%zpnd)) then
            where (vs%grid%zpnd /= huge(vs%grid%zpnd))
                vs%basin%zpnd = vs%grid%zpnd*shd%FRAC
            elsewhere
                vs%basin%zpnd = 0.0
            end where
        end if
!-        if (allocated(vs%basin%lqwspnd) .and. allocated(vs%grid%lqwspnd)) then
!-            if (all(vs%grid%lqwspnd /= huge(vs%grid%lqwspnd))) then
!-                vs%basin%lqwspnd = vs%grid%lqwspnd*shd%FRAC
!-            else
!-                vs%basin%lqwspnd = huge(vs%basin%lqwspnd)
!-            end if
!-        end if
        if (allocated(vs%basin%tpnd) .and. allocated(vs%grid%tpnd)) then
            where (vs%grid%tpnd /= huge(vs%grid%tpnd))
                vs%basin%tpnd = vs%grid%tpnd*shd%FRAC
                where (vs%basin%tpnd > 0.0)
                    tpndfrac = shd%FRAC
                elsewhere
                    tpndfrac = 0.0
                end where
            elsewhere
                vs%basin%tpnd = 0.0
            end where
        end if
        if (allocated(vs%basin%pndcaf) .and. allocated(vs%grid%pndcaf)) then
            where (vs%grid%pndcaf /= huge(vs%grid%pndcaf))
                vs%basin%pndcaf = vs%grid%pndcaf*shd%FRAC
            elsewhere
                vs%basin%pndcaf = 0.0
            end where
        end if
        if (allocated(vs%basin%potevp) .and. allocated(vs%grid%potevp)) then
            where (vs%grid%potevp /= huge(vs%grid%potevp))
                vs%basin%potevp = vs%grid%potevp*shd%FRAC
            elsewhere
                vs%basin%potevp = 0.0
            end where
        end if
        if (allocated(vs%basin%et) .and. allocated(vs%grid%et)) then
            where (vs%grid%et /= huge(vs%grid%et))
                vs%basin%et = vs%grid%et*shd%FRAC
            elsewhere
                vs%basin%et = 0.0
            end where
        end if
!-        if (allocated(vs%basin%evpb) .and. allocated(vs%grid%evpb)) then
!-            if (all(vs%grid%evpb /= huge(vs%grid%evpb))) then
!-                vs%basin%evpb = vs%grid%evpb*shd%FRAC
!-            else
!-                vs%basin%evpb = huge(vs%basin%evpb)
!-            end if
!-        end if
!-        if (allocated(vs%basin%arrd) .and. allocated(vs%grid%arrd)) then
!-            if (all(vs%grid%arrd /= huge(vs%grid%arrd))) then
!-                vs%basin%arrd = vs%grid%arrd*shd%FRAC
!-            else
!-                vs%basin%arrd = huge(vs%basin%arrd)
!-            end if
!-        end if
        if (allocated(vs%basin%ovrflw) .and. allocated(vs%grid%ovrflw)) then
            where (vs%grid%ovrflw /= huge(vs%grid%ovrflw))
                vs%basin%ovrflw = vs%grid%ovrflw*shd%FRAC
            elsewhere
                vs%basin%ovrflw = 0.0
            end where
        end if
        if (allocated(vs%basin%qevp) .and. allocated(vs%grid%qevp)) then
            where (vs%grid%qevp /= huge(vs%grid%qevp))
                vs%basin%qevp = vs%grid%qevp*shd%FRAC
            elsewhere
                vs%basin%qevp = 0.0
            end where
        end if
        if (allocated(vs%basin%qsens) .and. allocated(vs%grid%qsens)) then
            where (vs%grid%qsens /= huge(vs%grid%qsens))
                vs%basin%qsens = vs%grid%qsens*shd%FRAC
            elsewhere
                vs%basin%qsens = 0.0
            end where
        end if
        if (allocated(vs%basin%gzero) .and. allocated(vs%grid%gzero)) then
            where (vs%grid%gzero /= huge(vs%grid%gzero))
                vs%basin%gzero = vs%grid%gzero*shd%FRAC
            elsewhere
                vs%basin%gzero = 0.0
            end where
        end if
        if (allocated(vs%basin%tsfs) .and. allocated(vs%grid%tsfs)) then
            do j = 1, 4
                where (vs%grid%tsfs(:, j) /= huge(vs%grid%tsfs))
                    vs%basin%tsfs(:, j) = vs%grid%tsfs(:, j)*shd%FRAC
                elsewhere
                    vs%basin%tsfs(:, j) = 0.0
                end where
            end do
        end if
        if (allocated(vs%basin%tsurf) .and. allocated(vs%grid%tsurf)) then
            where (vs%grid%tsurf /= huge(vs%grid%tsurf))
                vs%basin%tsurf = vs%grid%tsurf*shd%FRAC
            elsewhere
                vs%basin%tsurf = 0.0
            end where
        end if

        !> Ice/glacier variables.
        if (allocated(vs%basin%lqwsice) .and. allocated(vs%grid%lqwsice)) then
            where (vs%grid%lqwsice /= huge(vs%grid%lqwsice))
                vs%basin%lqwsice = vs%grid%lqwsice*shd%FRAC
            elsewhere
                vs%basin%lqwsice = 0.0
            end where
        end if
        if (allocated(vs%basin%tice) .and. allocated(vs%grid%tice)) then
            where (vs%grid%tice /= huge(vs%grid%tice))
                vs%basin%tice = vs%grid%tice*shd%FRAC
                where (vs%basin%tice > 0.0)
                    ticefrac = shd%FRAC
                elsewhere
                    ticefrac = 0.0
                end where
            elsewhere
                vs%basin%tice = 0.0
            end where
        end if

        !> Subsurface/soil variables.
        do j = 1, shd%lc%IGND
            if (allocated(vs%basin%dzsol) .and. allocated(vs%grid%dzsol)) then
                where (vs%grid%dzsol(:, j) /= huge(vs%grid%dzsol))
                    vs%basin%dzsol(:, j) = vs%grid%dzsol(:, j)*shd%FRAC
                elsewhere
                    vs%basin%dzsol(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%dzsolhyd) .and. allocated(vs%grid%dzsolhyd)) then
                where (vs%grid%dzsolhyd(:, j) /= huge(vs%grid%dzsolhyd))
                    vs%basin%dzsolhyd(:, j) = vs%grid%dzsolhyd(:, j)*shd%FRAC
                elsewhere
                    vs%basin%dzsolhyd(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%thlqsol) .and. allocated(vs%grid%thlqsol)) then
                where (vs%grid%thlqsol(:, j) /= huge(vs%grid%thlqsol))
                    vs%basin%thlqsol(:, j) = vs%grid%thlqsol(:, j)*shd%FRAC
                elsewhere
                    vs%basin%thlqsol(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%thicsol) .and. allocated(vs%grid%thicsol)) then
                where (vs%grid%thicsol(:, j) /= huge(vs%grid%thicsol))
                    vs%basin%thicsol(:, j) = vs%grid%thicsol(:, j)*shd%FRAC
                elsewhere
                    vs%basin%thicsol(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%lqwssol) .and. allocated(vs%grid%lqwssol)) then
                where (vs%grid%lqwssol(:, j) /= huge(vs%grid%lqwssol))
                    vs%basin%lqwssol(:, j) = vs%grid%lqwssol(:, j)*shd%FRAC
                elsewhere
                    vs%basin%lqwssol(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%fzwssol) .and. allocated(vs%grid%fzwssol)) then
                where (vs%grid%fzwssol(:, j) /= huge(vs%grid%fzwssol))
                    vs%basin%fzwssol(:, j) = vs%grid%fzwssol(:, j)*shd%FRAC
                elsewhere
                    vs%basin%fzwssol(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%tsol) .and. allocated(vs%grid%tsol)) then
                where (vs%grid%tsol(:, j) /= huge(vs%grid%tsol))
                    vs%basin%tsol(:, j) = vs%grid%tsol(:, j)*shd%FRAC
                elsewhere
                    vs%basin%tsol(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%gflx) .and. allocated(vs%grid%gflx)) then
                where (vs%grid%gflx(:, j) /= huge(vs%grid%gflx))
                    vs%basin%gflx(:, j) = vs%grid%gflx(:, j)*shd%FRAC
                elsewhere
                    vs%basin%gflx(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%hcps) .and. allocated(vs%grid%hcps)) then
                where (vs%grid%hcps(:, j) /= huge(vs%grid%hcps))
                    vs%basin%hcps(:, j) = vs%grid%hcps(:, j)*shd%FRAC
                elsewhere
                    vs%basin%hcps(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%hcpc) .and. allocated(vs%grid%hcpc)) then
                where (vs%grid%hcpc(:, j) /= huge(vs%grid%hcpc))
                    vs%basin%hcpc(:, j) = vs%grid%hcpc(:, j)*shd%FRAC
                elsewhere
                    vs%basin%hcpc(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%hcpg) .and. allocated(vs%grid%hcpg)) then
                where (vs%grid%hcpg(:, j) /= huge(vs%grid%hcpg))
                    vs%basin%hcpg(:, j) = vs%grid%hcpg(:, j)*shd%FRAC
                elsewhere
                    vs%basin%hcpg(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%tctopc) .and. allocated(vs%grid%tctopc)) then
                where (vs%grid%tctopc(:, j) /= huge(vs%grid%tctopc))
                    vs%basin%tctopc(:, j) = vs%grid%tctopc(:, j)*shd%FRAC
                elsewhere
                    vs%basin%tctopc(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%tctopg) .and. allocated(vs%grid%tctopg)) then
                where (vs%grid%tctopg(:, j) /= huge(vs%grid%tctopg))
                    vs%basin%tctopg(:, j) = vs%grid%tctopg(:, j)*shd%FRAC
                elsewhere
                    vs%basin%tctopg(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%tcbotc) .and. allocated(vs%grid%tcbotc)) then
                where (vs%grid%tcbotc(:, j) /= huge(vs%grid%tcbotc))
                    vs%basin%tcbotc(:, j) = vs%grid%tcbotc(:, j)*shd%FRAC
                elsewhere
                    vs%basin%tcbotc(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%tcbotg) .and. allocated(vs%grid%tcbotg)) then
                where (vs%grid%tcbotg(:, j) /= huge(vs%grid%tcbotg))
                    vs%basin%tcbotg(:, j) = vs%grid%tcbotg(:, j)*shd%FRAC
                elsewhere
                    vs%basin%tcbotg(:, j) = 0.0
                end where
            end if
            if (allocated(vs%basin%latflw) .and. allocated(vs%grid%latflw)) then
                where (vs%grid%latflw(:, j) /= huge(vs%grid%latflw))
                    vs%basin%latflw(:, j) = vs%grid%latflw(:, j)*shd%FRAC
                elsewhere
                    vs%basin%latflw(:, j) = 0.0
                end where
            end if
        end do
        if (allocated(vs%basin%zsol) .and. allocated(vs%grid%zsol)) then
            where (vs%grid%zsol /= huge(vs%grid%zsol))
                vs%basin%zsol = vs%grid%zsol*shd%FRAC
            elsewhere
                vs%basin%zsol = 0.0
            end where
        end if
        if (allocated(vs%basin%zsolhyd) .and. allocated(vs%grid%zsolhyd)) then
            where (vs%grid%zsolhyd /= huge(vs%grid%zsolhyd))
                vs%basin%zsolhyd = vs%grid%zsolhyd*shd%FRAC
            elsewhere
                vs%basin%zsolhyd = 0.0
            end where
        end if
        if (allocated(vs%basin%zsolsat) .and. allocated(vs%grid%zsolsat)) then
            where (vs%grid%zsolsat /= huge(vs%grid%zsolsat))
                vs%basin%zsolsat = vs%grid%zsolsat*shd%FRAC
            elsewhere
                vs%basin%zsolsat = 0.0
            end where
        end if
!-        if (allocated(vs%basin%ggeo) .and. allocated(vs%grid%ggeo)) then
!-            where (vs%grid%ggeo /= huge(vs%grid%ggeo))
!-                vs%basin%ggeo = vs%grid%ggeo*shd%FRAC
!-            elsewhere
!-                vs%basin%ggeo = 0.0
!-            end where
!-        end if
        if (allocated(vs%basin%tbas) .and. allocated(vs%grid%tbas)) then
            where (vs%grid%tbas /= huge(vs%grid%tbas))
                vs%basin%tbas = vs%grid%tbas*shd%FRAC
            elsewhere
                vs%basin%tbas = 0.0
            end where
        end if
        if (allocated(vs%basin%drainsol) .and. allocated(vs%grid%drainsol)) then
            where (vs%grid%drainsol /= huge(vs%grid%drainsol))
                vs%basin%drainsol = vs%grid%drainsol*shd%FRAC
            elsewhere
                vs%basin%drainsol = 0.0
            end where
        end if

        !> Groundwater/lower zone storage variables.
        if (allocated(vs%basin%rchg) .and. allocated(vs%grid%rchg)) then
            where (vs%grid%rchg /= huge(vs%grid%rchg))
                vs%basin%rchg = vs%grid%rchg*shd%FRAC
            elsewhere
                vs%basin%rchg = 0.0
            end where
        end if
        if (allocated(vs%basin%stggw) .and. allocated(vs%grid%stggw)) then
            where (vs%grid%stggw /= huge(vs%grid%stggw))
                vs%basin%stggw = vs%grid%stggw*shd%FRAC
            elsewhere
                vs%basin%stggw = 0.0
            end where
        end if
        if (allocated(vs%basin%lkg) .and. allocated(vs%grid%lkg)) then
            where (vs%grid%lkg /= huge(vs%grid%lkg))
                vs%basin%lkg = vs%grid%lkg*shd%FRAC
            elsewhere
                vs%basin%lkg = 0.0
            end where
        end if
!-        if (allocated(vs%basin%dzs) .and. allocated(vs%grid%dzs)) then
!-            if (all(vs%grid%dzs /= huge(vs%grid%dzs))) then
!-                vs%basin%dzs = vs%grid%dzs*shd%FRAC
!-            else
!-                vs%basin%dzs = huge(vs%basin%dzs)
!-            end if
!-        end if

        !> Routing variables.
        if (allocated(vs%basin%rff) .and. allocated(vs%grid%rff)) then
            where (vs%grid%rff /= huge(vs%grid%rff))
                vs%basin%rff = vs%grid%rff*shd%FRAC
            elsewhere
                vs%basin%rff = 0.0
            end where
        end if
!        if (allocated(vs%basin%qi) .and. allocated(vs%grid%qi)) then
!            if (all(vs%grid%qi /= huge(vs%grid%qi))) then
!                vs%basin%qi = vs%grid%qi*shd%FRAC
!            else
!                vs%basin%qi = huge(vs%basin%qi)
!            end if
!        end if
!        if (allocated(vs%basin%qo) .and. allocated(vs%grid%qo)) then
!            if (all(vs%grid%qo /= huge(vs%grid%qo))) then
!                vs%basin%qo = vs%grid%qo*shd%FRAC
!            else
!                vs%basin%qo = huge(vs%basin%qo)
!            end if
!        end if
!        if (allocated(vs%basin%stgch) .and. allocated(vs%grid%stgch)) then
!            if (all(vs%grid%stgch /= huge(vs%grid%stgch))) then
!                vs%basin%stgch = vs%grid%stgch*shd%FRAC
!            else
!                vs%basin%stgch = huge(vs%basin%stgch)
!            end if
!        end if
!        if (allocated(vs%basin%zlvl) .and. allocated(vs%grid%zlvl)) then
!            if (all(vs%grid%zlvl /= huge(vs%grid%zlvl))) then
!                vs%basin%zlvl = vs%grid%zlvl*shd%FRAC
!            else
!                vs%basin%zlvl = huge(vs%basin%zlvl)
!            end if
!        end if
!        if (allocated(vs%basin%div) .and. allocated(vs%grid%div)) then
!            if (all(vs%grid%div /= huge(vs%grid%div))) then
!                vs%basin%div = vs%grid%div*shd%FRAC
!            else
!                vs%basin%div = huge(vs%basin%div)
!            end if
!        end if
!        if (allocated(vs%basin%abstr) .and. allocated(vs%grid%abstr)) then
!            if (all(vs%grid%abstr /= huge(vs%grid%abstr))) then
!                vs%basin%abstr = vs%grid%abstr*shd%FRAC
!            else
!                vs%basin%abstr = huge(vs%basin%abstr)
!            end if
!        end if

        !> Relative area fraction.
        frac = shd%FRAC

        !> Accumulate basin values.
        do i = 1, shd%NAA
            ii = shd%NEXT(i)
            if (ii > 0) then

                !> Meteorology/climatology variables.
                if (allocated(vs%basin%fsin)) then
                    if (vs%basin%fsin(i) /= huge(vs%basin%fsin)) vs%basin%fsin(ii) = vs%basin%fsin(ii) + vs%basin%fsin(i)
                end if
                if (allocated(vs%basin%fsvs)) then
                    if (vs%basin%fsvs(i) /= huge(vs%basin%fsvs)) vs%basin%fsvs(ii) = vs%basin%fsvs(ii) + vs%basin%fsvs(i)
                end if
                if (allocated(vs%basin%fsir)) then
                    if (vs%basin%fsir(i) /= huge(vs%basin%fsir)) vs%basin%fsir(ii) = vs%basin%fsir(ii) + vs%basin%fsir(i)
                end if
                if (allocated(vs%basin%fsdr)) then
                    if (vs%basin%fsdr(i) /= huge(vs%basin%fsdr)) vs%basin%fsdr(ii) = vs%basin%fsdr(ii) + vs%basin%fsdr(i)
                end if
                if (allocated(vs%basin%fsdff)) then
                    if (vs%basin%fsdff(i) /= huge(vs%basin%fsdff)) vs%basin%fsdff(ii) = vs%basin%fsdff(ii) + vs%basin%fsdff(i)
                end if
                if (allocated(vs%basin%flin)) then
                    if (vs%basin%flin(i) /= huge(vs%basin%flin)) vs%basin%flin(ii) = vs%basin%flin(ii) + vs%basin%flin(i)
                end if
                if (allocated(vs%basin%ta)) then
                    if (vs%basin%ta(i) /= huge(vs%basin%ta)) vs%basin%ta(ii) = vs%basin%ta(ii) + vs%basin%ta(i)
                end if
                if (allocated(vs%basin%qa)) then
                    if (vs%basin%qa(i) /= huge(vs%basin%qa)) vs%basin%qa(ii) = vs%basin%qa(ii) + vs%basin%qa(i)
                end if
                if (allocated(vs%basin%pres)) then
                    if (vs%basin%pres(i) /= huge(vs%basin%pres)) vs%basin%pres(ii) = vs%basin%pres(ii) + vs%basin%pres(i)
                end if
                if (allocated(vs%basin%uu)) then
                    if (vs%basin%uu(i) /= huge(vs%basin%uu)) vs%basin%uu(ii) = vs%basin%uu(ii) + vs%basin%uu(i)
                end if
                if (allocated(vs%basin%vv)) then
                    if (vs%basin%vv(i) /= huge(vs%basin%vv)) vs%basin%vv(ii) = vs%basin%vv(ii) + vs%basin%vv(i)
                end if
                if (allocated(vs%basin%uv)) then
                    if (vs%basin%uv(i) /= huge(vs%basin%uv)) vs%basin%uv(ii) = vs%basin%uv(ii) + vs%basin%uv(i)
                end if
                if (allocated(vs%basin%wdir)) then
                    if (vs%basin%wdir(i) /= huge(vs%basin%wdir)) vs%basin%wdir(ii) = vs%basin%wdir(ii) + vs%basin%wdir(i)
                end if
                if (allocated(vs%basin%prern)) then
                    if (vs%basin%prern(i) /= huge(vs%basin%prern)) vs%basin%prern(ii) = vs%basin%prern(ii) + vs%basin%prern(i)
                end if
                if (allocated(vs%basin%presno)) then
                    if (vs%basin%presno(i) /= huge(vs%basin%presno)) vs%basin%presno(ii) = vs%basin%presno(ii) + vs%basin%presno(i)
                end if
                if (allocated(vs%basin%pre)) then
                    if (vs%basin%pre(i) /= huge(vs%basin%pre)) vs%basin%pre(ii) = vs%basin%pre(ii) + vs%basin%pre(i)
                end if

                !> Canopy variables.
                if (allocated(vs%basin%lqwscan)) then
                    if (vs%basin%lqwscan(i) /= huge(vs%basin%lqwscan)) then
                        vs%basin%lqwscan(ii) = vs%basin%lqwscan(ii) + vs%basin%lqwscan(i)
                    end if
                end if
                if (allocated(vs%basin%fzwscan)) then
                    if (vs%basin%fzwscan(i) /= huge(vs%basin%fzwscan)) then
                        vs%basin%fzwscan(ii) = vs%basin%fzwscan(ii) + vs%basin%fzwscan(i)
                    end if
                end if
                if (allocated(vs%basin%cmai)) then
                    if (vs%basin%cmai(i) /= huge(vs%basin%cmai)) vs%basin%cmai(ii) = vs%basin%cmai(ii) + vs%basin%cmai(i)
                end if
                if (allocated(vs%basin%tacan)) then
                    if (vs%basin%tacan(i) /= huge(vs%basin%tacan)) vs%basin%tacan(ii) = vs%basin%tacan(ii) + vs%basin%tacan(i)
                end if
                if (allocated(vs%basin%qacan)) then
                    if (vs%basin%qacan(i) /= huge(vs%basin%qacan)) vs%basin%qacan(ii) = vs%basin%qacan(ii) + vs%basin%qacan(i)
                end if
                if (allocated(vs%basin%uvcan)) then
                    if (vs%basin%uvcan(i) /= huge(vs%basin%uvcan)) vs%basin%uvcan(ii) = vs%basin%uvcan(ii) + vs%basin%uvcan(i)
                end if
                if (allocated(vs%basin%tcan)) then
                    if (vs%basin%tcan(i) /= huge(vs%basin%tcan)) then
                        vs%basin%tcan(ii) = vs%basin%tcan(ii) + vs%basin%tcan(i)
                        if (vs%basin%tcan(i) > 0.0) then
                            tcanfrac(ii) = tcanfrac(ii) + tcanfrac(i)
                        end if
                    end if
                end if
                if (allocated(vs%basin%gro)) then
                    if (vs%basin%gro(i) /= huge(vs%basin%gro)) vs%basin%gro(ii) = vs%basin%gro(ii) + vs%basin%gro(i)
                end if

                !> Snow variables.
                if (allocated(vs%basin%fsno)) then
                    if (vs%basin%fsno(i) /= huge(vs%basin%fsno)) vs%basin%fsno(ii) = vs%basin%fsno(ii) + vs%basin%fsno(i)
                end if
                if (allocated(vs%basin%sno)) then
                    if (vs%basin%sno(i) /= huge(vs%basin%sno)) vs%basin%sno(ii) = vs%basin%sno(ii) + vs%basin%sno(i)
                end if
                if (allocated(vs%basin%rhosno)) then
                    if (vs%basin%rhosno(i) /= huge(vs%basin%rhosno)) vs%basin%rhosno(ii) = vs%basin%rhosno(ii) + vs%basin%rhosno(i)
                end if
!-                if (allocated(vs%basin%zsno)) then
!-                    if (vs%basin%zsno(i) /= huge(vs%basin%zsno)) vs%basin%zsno(ii) = vs%basin%zsno(ii) + vs%basin%zsno(i)
!-                end if
                if (allocated(vs%basin%lqwssno)) then
                    if (vs%basin%lqwssno(i) /= huge(vs%basin%lqwssno)) then
                        vs%basin%lqwssno(ii) = vs%basin%lqwssno(ii) + vs%basin%lqwssno(i)
                    end if
                end if
                if (allocated(vs%basin%tsno)) then
                    if (vs%basin%tsno(i) /= huge(vs%basin%tsno)) then
                        vs%basin%tsno(ii) = vs%basin%tsno(ii) + vs%basin%tsno(i)
                        if (vs%basin%tsno(i) > 0.0) then
                            tsnofrac(ii) = tsnofrac(ii) + tsnofrac(i)
                        end if
                    end if
                end if
                if (allocated(vs%basin%albsno)) then
                    if (vs%basin%albsno(i) /= huge(vs%basin%albsno)) vs%basin%albsno(ii) = vs%basin%albsno(ii) + vs%basin%albsno(i)
                end if
                if (allocated(vs%basin%drainsno)) then
                    if (vs%basin%drainsno(i) /= huge(vs%basin%drainsno)) then
                        vs%basin%drainsno(ii) = vs%basin%drainsno(ii) + vs%basin%drainsno(i)
                    end if
                end if

                !> Surface variables.
                if (allocated(vs%basin%albt)) then
                    if (vs%basin%albt(i) /= huge(vs%basin%albt)) then
                        vs%basin%albt(ii) = vs%basin%albt(ii) + vs%basin%albt(i)
                        if (vs%basin%albt(i) > 0.0) then
                            albtfrac(ii) = albtfrac(ii) + albtfrac(i)
                        end if
                    end if
                end if
                if (allocated(vs%basin%alvs)) then
                    if (vs%basin%alvs(i) /= huge(vs%basin%alvs)) vs%basin%alvs(ii) = vs%basin%alvs(ii) + vs%basin%alvs(i)
                end if
                if (allocated(vs%basin%alir)) then
                    if (vs%basin%alir(i) /= huge(vs%basin%alir)) vs%basin%alir(ii) = vs%basin%alir(ii) + vs%basin%alir(i)
                end if
                if (allocated(vs%basin%gte)) then
                    if (vs%basin%gte(i) /= huge(vs%basin%gte)) vs%basin%gte(ii) = vs%basin%gte(ii) + vs%basin%gte(i)
                end if
                if (allocated(vs%basin%zpnd)) then
                    if (vs%basin%zpnd(i) /= huge(vs%basin%zpnd)) vs%basin%zpnd(ii) = vs%basin%zpnd(ii) + vs%basin%zpnd(i)
                end if
!-                if (allocated(vs%basin%lqwspnd)) then
!-                    if (vs%basin%lqwspnd(i) /= huge(vs%basin%lqwspnd)) then
!-                        vs%basin%lqwspnd(ii) = vs%basin%lqwspnd(ii) + vs%basin%lqwspnd(i)
!-                    end if
!-                end if
                if (allocated(vs%basin%tpnd)) then
                    if (vs%basin%tpnd(i) /= huge(vs%basin%tpnd)) then
                        vs%basin%tpnd(ii) = vs%basin%tpnd(ii) + vs%basin%tpnd(i)
                        if (vs%basin%tpnd(i) > 0.0) then
                            tpndfrac(ii) = tpndfrac(ii) + tpndfrac(i)
                        end if
                    end if
                end if
                if (allocated(vs%basin%pndcaf)) then
                    if (vs%basin%pndcaf(i) /= huge(vs%basin%pndcaf)) vs%basin%pndcaf(ii) = vs%basin%pndcaf(ii) + vs%basin%pndcaf(i)
                end if
                if (allocated(vs%basin%potevp)) then
                    if (vs%basin%potevp(i) /= huge(vs%basin%potevp)) vs%basin%potevp(ii) = vs%basin%potevp(ii) + vs%basin%potevp(i)
                end if
                if (allocated(vs%basin%et)) then
                    if (vs%basin%et(i) /= huge(vs%basin%et)) vs%basin%et(ii) = vs%basin%et(ii) + vs%basin%et(i)
                end if
!-                if (allocated(vs%basin%evpb)) then
!-                    if (vs%basin%evpb(i) /= huge(vs%basin%evpb)) vs%basin%evpb(ii) = vs%basin%evpb(ii) + vs%basin%evpb(i)
!-                end if
!-                if (allocated(vs%basin%arrd)) then
!-                    if (vs%basin%arrd(i) /= huge(vs%basin%arrd)) vs%basin%arrd(ii) = vs%basin%arrd(ii) + vs%basin%arrd(i)
!-                end if
                if (allocated(vs%basin%ovrflw)) then
                    if (vs%basin%ovrflw(i) /= huge(vs%basin%ovrflw)) then
                        vs%basin%ovrflw(ii) = vs%basin%ovrflw(ii) + vs%basin%ovrflw(i)
                    end if
                end if
                if (allocated(vs%basin%qevp)) then
                    if (vs%basin%qevp(i) /= huge(vs%basin%qevp)) vs%basin%qevp(ii) = vs%basin%qevp(ii) + vs%basin%qevp(i)
                end if
                if (allocated(vs%basin%qsens)) then
                    if (vs%basin%qsens(i) /= huge(vs%basin%qsens)) vs%basin%qsens(ii) = vs%basin%qsens(ii) + vs%basin%qsens(i)
                end if
                if (allocated(vs%basin%gzero)) then
                    if (vs%basin%gzero(i) /= huge(vs%basin%gzero)) vs%basin%gzero(ii) = vs%basin%gzero(ii) + vs%basin%gzero(i)
                end if
                if (allocated(vs%basin%tsfs)) then
                    if (all(vs%basin%tsfs(i, :) /= huge(vs%basin%tsfs))) then
                        vs%basin%tsfs(ii, :) = vs%basin%tsfs(ii, :) + vs%basin%tsfs(i, :)
                    end if
                end if
                if (allocated(vs%basin%tsurf)) then
                    if (vs%basin%tsurf(i) /= huge(vs%basin%tsurf)) vs%basin%tsurf(ii) = vs%basin%tsurf(ii) + vs%basin%tsurf(i)
                end if

                !> Ice/glacier variables.
                if (allocated(vs%basin%lqwsice)) then
                    if (vs%basin%lqwsice(i) /= huge(vs%basin%lqwsice)) then
                        vs%basin%lqwsice(ii) = vs%basin%lqwsice(ii) + vs%basin%lqwsice(i)
                    end if
                end if
                if (allocated(vs%basin%tice)) then
                    if (vs%basin%tice(i) /= huge(vs%basin%tice)) then
                        vs%basin%tice(ii) = vs%basin%tice(ii) + vs%basin%tice(i)
                        if (vs%basin%tice(i) > 0.0) then
                            ticefrac(ii) = ticefrac(ii) + ticefrac(i)
                        end if
                    end if
                end if

                !> Subsurface/soil variables.
                if (allocated(vs%basin%dzsol)) then
                    if (all(vs%basin%dzsol(i, :) /= huge(vs%basin%dzsol))) then
                        vs%basin%dzsol(ii, :) = vs%basin%dzsol(ii, :) + vs%basin%dzsol(i, :)
                    end if
                end if
                if (allocated(vs%basin%dzsolhyd)) then
                    if (all(vs%basin%dzsolhyd(i, :) /= huge(vs%basin%dzsolhyd))) then
                        vs%basin%dzsolhyd(ii, :) = vs%basin%dzsolhyd(ii, :) + vs%basin%dzsolhyd(i, :)
                    end if
                end if
                if (allocated(vs%basin%thlqsol)) then
                    if (all(vs%basin%thlqsol(i, :) /= huge(vs%basin%thlqsol))) then
                        vs%basin%thlqsol(ii, :) = vs%basin%thlqsol(ii, :) + vs%basin%thlqsol(i, :)
                    end if
                end if
                if (allocated(vs%basin%thicsol)) then
                    if (all(vs%basin%thicsol(i, :) /= huge(vs%basin%thicsol))) then
                        vs%basin%thicsol(ii, :) = vs%basin%thicsol(ii, :) + vs%basin%thicsol(i, :)
                    end if
                end if
                if (allocated(vs%basin%lqwssol)) then
                    if (all(vs%basin%lqwssol(i, :) /= huge(vs%basin%lqwssol))) then
                        vs%basin%lqwssol(ii, :) = vs%basin%lqwssol(ii, :) + vs%basin%lqwssol(i, :)
                    end if
                end if
                if (allocated(vs%basin%fzwssol)) then
                    if (all(vs%basin%fzwssol(i, :) /= huge(vs%basin%fzwssol))) then
                        vs%basin%fzwssol(ii, :) = vs%basin%fzwssol(ii, :) + vs%basin%fzwssol(i, :)
                    end if
                end if
                if (allocated(vs%basin%tsol)) then
                    if (all(vs%basin%tsol(i, :) /= huge(vs%basin%tsol))) then
                        vs%basin%tsol(ii, :) = vs%basin%tsol(ii, :) + vs%basin%tsol(i, :)
                    end if
                end if
                if (allocated(vs%basin%gflx)) then
                    if (all(vs%basin%gflx(i, :) /= huge(vs%basin%gflx))) then
                        vs%basin%gflx(ii, :) = vs%basin%gflx(ii, :) + vs%basin%gflx(i, :)
                    end if
                end if
                if (allocated(vs%basin%hcps)) then
                    if (all(vs%basin%hcps(i, :) /= huge(vs%basin%hcps))) then
                        vs%basin%hcps(ii, :) = vs%basin%hcps(ii, :) + vs%basin%hcps(i, :)
                    end if
                end if
                if (allocated(vs%basin%hcpc)) then
                    if (all(vs%basin%hcpc(i, :) /= huge(vs%basin%hcpc))) then
                        vs%basin%hcpc(ii, :) = vs%basin%hcpc(ii, :) + vs%basin%hcpc(i, :)
                    end if
                end if
                if (allocated(vs%basin%hcpg)) then
                    if (all(vs%basin%hcpg(i, :) /= huge(vs%basin%hcpg))) then
                        vs%basin%hcpg(ii, :) = vs%basin%hcpg(ii, :) + vs%basin%hcpg(i, :)
                    end if
                end if
                if (allocated(vs%basin%tctopc)) then
                    if (all(vs%basin%tctopc(i, :) /= huge(vs%basin%tctopc))) then
                        vs%basin%tctopc(ii, :) = vs%basin%tctopc(ii, :) + vs%basin%tctopc(i, :)
                    end if
                end if
                if (allocated(vs%basin%tctopg)) then
                    if (all(vs%basin%tctopg(i, :) /= huge(vs%basin%tctopg))) then
                        vs%basin%tctopg(ii, :) = vs%basin%tctopg(ii, :) + vs%basin%tctopg(i, :)
                    end if
                end if
                if (allocated(vs%basin%tcbotc)) then
                    if (all(vs%basin%tcbotc(i, :) /= huge(vs%basin%tcbotc))) then
                        vs%basin%tcbotc(ii, :) = vs%basin%tcbotc(ii, :) + vs%basin%tcbotc(i, :)
                    end if
                end if
                if (allocated(vs%basin%tcbotg)) then
                    if (all(vs%basin%tcbotg(i, :) /= huge(vs%basin%tcbotg))) then
                        vs%basin%tcbotg(ii, :) = vs%basin%tcbotg(ii, :) + vs%basin%tcbotg(i, :)
                    end if
                end if
                if (allocated(vs%basin%latflw)) then
                    if (all(vs%basin%latflw(i, :) /= huge(vs%basin%latflw))) then
                        vs%basin%latflw(ii, :) = vs%basin%latflw(ii, :) + vs%basin%latflw(i, :)
                    end if
                end if
                if (allocated(vs%basin%zsol)) then
                    if (vs%basin%zsol(i) /= huge(vs%basin%zsol)) vs%basin%zsol(ii) = vs%basin%zsol(ii) + vs%basin%zsol(i)
                end if
                if (allocated(vs%basin%zsolhyd)) then
                    if (vs%basin%zsolhyd(i) /= huge(vs%basin%zsolhyd)) then
                        vs%basin%zsolhyd(ii) = vs%basin%zsolhyd(ii) + vs%basin%zsolhyd(i)
                    end if
                end if
                if (allocated(vs%basin%zsolsat)) then
                    if (vs%basin%zsolsat(i) /= huge(vs%basin%zsolsat)) then
                        vs%basin%zsolsat(ii) = vs%basin%zsolsat(ii) + vs%basin%zsolsat(i)
                    end if
                end if
!-                if (allocated(vs%basin%ggeo)) then
!-                    if (vs%basin%ggeo(i) /= huge(vs%basin%ggeo)) vs%basin%ggeo(ii) = vs%basin%ggeo(ii) + vs%basin%ggeo(i)
!-                end if
                if (allocated(vs%basin%tbas)) then
                    if (vs%basin%tbas(i) /= huge(vs%basin%tbas)) vs%basin%tbas(ii) = vs%basin%tbas(ii) + vs%basin%tbas(i)
                end if
                if (allocated(vs%basin%drainsol)) then
                    if (vs%basin%drainsol(i) /= huge(vs%basin%drainsol)) then
                        vs%basin%drainsol(ii) = vs%basin%drainsol(ii) + vs%basin%drainsol(i)
                    end if
                end if

                !> Groundwater/lower zone storage variables.
                if (allocated(vs%basin%rchg)) then
                    if (vs%basin%rchg(i) /= huge(vs%basin%rchg)) vs%basin%rchg(ii) = vs%basin%rchg(ii) + vs%basin%rchg(i)
                end if
                if (allocated(vs%basin%stggw)) then
                    if (vs%basin%stggw(i) /= huge(vs%basin%stggw)) vs%basin%stggw(ii) = vs%basin%stggw(ii) + vs%basin%stggw(i)
                end if
                if (allocated(vs%basin%lkg)) then
                    if (vs%basin%lkg(i) /= huge(vs%basin%lkg)) vs%basin%lkg(ii) = vs%basin%lkg(ii) + vs%basin%lkg(i)
                end if
!-                if (allocated(vs%basin%dzs)) then
!-                    if (vs%basin%dzs(i) /= huge(vs%basin%dzs)) vs%basin%dzs(ii) = vs%basin%dzs(ii) + vs%basin%dzs(i)
!-                end if

                !> Routing variables.
                if (allocated(vs%basin%rff)) then
                    if (vs%basin%rff(i) /= huge(vs%basin%rff)) vs%basin%rff(ii) = vs%basin%rff(ii) + vs%basin%rff(i)
                end if
!                if (allocated(vs%basin%qi)) then
!                    if (vs%basin%qi(i) /= huge(vs%basin%qi)) vs%basin%qi(ii) = vs%basin%qi(ii) + vs%basin%qi(i)
!                end if
!                if (allocated(vs%basin%qo)) then
!                    if (vs%basin%qo(i) /= huge(vs%basin%qo)) vs%basin%qo(ii) = vs%basin%qo(ii) + vs%basin%qo(i)
!                end if
!                if (allocated(vs%basin%stgch)) then
!                    if (vs%basin%stgch(i) /= huge(vs%basin%stgch)) vs%basin%stgch(ii) = vs%basin%stgch(ii) + vs%basin%stgch(i)
!                end if
!                if (allocated(vs%basin%zlvl)) then
!                    if (vs%basin%zlvl(i) /= huge(vs%basin%zlvl)) vs%basin%zlvl(ii) = vs%basin%zlvl(ii) + vs%basin%zlvl(i)
!                end if
!                if (allocated(vs%basin%div)) then
!                    if (vs%basin%div(i) /= huge(vs%basin%div)) vs%basin%div(ii) = vs%basin%div(ii) + vs%basin%div(i)
!                end if
!                if (allocated(vs%basin%abstr)) then
!                    if (vs%basin%abstr(i) /= huge(vs%basin%abstr)) vs%basin%abstr(ii) = vs%basin%abstr(ii) + vs%basin%abstr(i)
!                end if

                !> Relative area fraction.
                frac(ii) = frac(ii) + frac(i)
            end if
        end do

        !> Check for division by zero.
        where (frac == 0.0) frac = 1.0

        !> Meteorology/climatology variables.
        if (allocated(vs%basin%fsin)) then
            where (vs%basin%fsin /= huge(vs%basin%fsin)) vs%basin%fsin = vs%basin%fsin/frac
        end if
        if (allocated(vs%basin%fsvs)) then
            where (vs%basin%fsvs /= huge(vs%basin%fsvs)) vs%basin%fsvs = vs%basin%fsvs/frac
        end if
        if (allocated(vs%basin%fsir)) then
            where (vs%basin%fsir /= huge(vs%basin%fsir)) vs%basin%fsir = vs%basin%fsir/frac
        end if
        if (allocated(vs%basin%fsdr)) then
            where (vs%basin%fsdr /= huge(vs%basin%fsdr)) vs%basin%fsdr = vs%basin%fsdr/frac
        end if
        if (allocated(vs%basin%fsdff)) then
            where (vs%basin%fsdff /= huge(vs%basin%fsdff)) vs%basin%fsdff = vs%basin%fsdff/frac
        end if
        if (allocated(vs%basin%flin)) then
            where (vs%basin%flin /= huge(vs%basin%flin)) vs%basin%flin = vs%basin%flin/frac
        end if
        if (allocated(vs%basin%ta)) then
            where (vs%basin%ta /= huge(vs%basin%ta)) vs%basin%ta = vs%basin%ta/frac
        end if
        if (allocated(vs%basin%qa)) then
            where (vs%basin%qa /= huge(vs%basin%qa)) vs%basin%qa = vs%basin%qa/frac
        end if
        if (allocated(vs%basin%pres)) then
            where (vs%basin%pres /= huge(vs%basin%pres)) vs%basin%pres = vs%basin%pres/frac
        end if
        if (allocated(vs%basin%uu)) then
            where (vs%basin%uu /= huge(vs%basin%uu)) vs%basin%uu = vs%basin%uu/frac
        end if
        if (allocated(vs%basin%vv)) then
            where (vs%basin%vv /= huge(vs%basin%vv)) vs%basin%vv = vs%basin%vv/frac
        end if
        if (allocated(vs%basin%uv)) then
            where (vs%basin%uv /= huge(vs%basin%uv)) vs%basin%uv = vs%basin%uv/frac
        end if
        if (allocated(vs%basin%wdir)) then
            where (vs%basin%wdir /= huge(vs%basin%wdir)) vs%basin%wdir = vs%basin%wdir/frac
        end if
        if (allocated(vs%basin%prern)) then
            where (vs%basin%prern /= huge(vs%basin%prern)) vs%basin%prern = vs%basin%prern/frac
        end if
        if (allocated(vs%basin%presno)) then
            where (vs%basin%presno /= huge(vs%basin%presno)) vs%basin%presno = vs%basin%presno/frac
        end if
        if (allocated(vs%basin%pre)) then
            where (vs%basin%pre /= huge(vs%basin%pre)) vs%basin%pre = vs%basin%pre/frac
        end if

        !> Canopy variables.
        if (allocated(vs%basin%lqwscan)) then
            where (vs%basin%lqwscan /= huge(vs%basin%lqwscan)) vs%basin%lqwscan = vs%basin%lqwscan/frac
        end if
        if (allocated(vs%basin%fzwscan)) then
            where (vs%basin%fzwscan /= huge(vs%basin%fzwscan)) vs%basin%fzwscan = vs%basin%fzwscan/frac
        end if
        if (allocated(vs%basin%cmai)) then
            where (vs%basin%cmai /= huge(vs%basin%cmai) .and. tcanfrac > 0.0) vs%basin%cmai = vs%basin%cmai/tcanfrac
        end if
        if (allocated(vs%basin%tacan)) then
            where (vs%basin%tacan /= huge(vs%basin%tacan) .and. tcanfrac > 0.0) vs%basin%tacan = vs%basin%tacan/tcanfrac
        end if
        if (allocated(vs%basin%qacan)) then
            where (vs%basin%qacan /= huge(vs%basin%qacan) .and. tcanfrac > 0.0) vs%basin%qacan = vs%basin%qacan/tcanfrac
        end if
        if (allocated(vs%basin%uvcan)) then
            where (vs%basin%uvcan /= huge(vs%basin%uvcan) .and. tcanfrac > 0.0) vs%basin%uvcan = vs%basin%uvcan/tcanfrac
        end if
        if (allocated(vs%basin%tcan)) then
            where (vs%basin%tcan /= huge(vs%basin%tcan) .and. tcanfrac > 0.0) vs%basin%tcan = vs%basin%tcan/tcanfrac
        end if
        if (allocated(vs%basin%gro)) then
            where (vs%basin%gro /= huge(vs%basin%gro) .and. tcanfrac > 0.0) vs%basin%gro = vs%basin%gro/tcanfrac
        end if

        !> Snow variables.
        if (allocated(vs%basin%fsno)) then
            where (vs%basin%fsno /= huge(vs%basin%fsno)) vs%basin%fsno = vs%basin%fsno/frac
        end if
        if (allocated(vs%basin%sno)) then
            where (vs%basin%sno /= huge(vs%basin%sno)) vs%basin%sno = vs%basin%sno/frac
        end if
        if (allocated(vs%basin%rhosno)) then
            where (vs%basin%rhosno /= huge(vs%basin%rhosno) .and. tsnofrac > 0.0) vs%basin%rhosno = vs%basin%rhosno/tsnofrac
        end if
!-        if (allocated(vs%basin%zsno)) then
!-            if (all(vs%basin%zsno /= huge(vs%basin%zsno))) vs%basin%zsno = vs%basin%zsno/frac
!-        end if
        if (allocated(vs%basin%lqwssno)) then
            where (vs%basin%lqwssno /= huge(vs%basin%lqwssno)) vs%basin%lqwssno = vs%basin%lqwssno/frac
        end if
        if (allocated(vs%basin%tsno)) then
            where (vs%basin%tsno /= huge(vs%basin%tsno) .and. tsnofrac > 0.0) vs%basin%tsno = vs%basin%tsno/tsnofrac
        end if
        if (allocated(vs%basin%albsno)) then
            where (vs%basin%albsno /= huge(vs%basin%albsno) .and. tsnofrac > 0.0) vs%basin%albsno = vs%basin%albsno/tsnofrac
        end if
        if (allocated(vs%basin%drainsno)) then
            where (vs%basin%drainsno /= huge(vs%basin%drainsno)) vs%basin%drainsno = vs%basin%drainsno/frac
        end if

        !> Surface variables.
        if (allocated(vs%basin%albt)) then
            where (vs%basin%albt /= huge(vs%basin%albt) .and. albtfrac > 0.0) vs%basin%albt = vs%basin%albt/albtfrac
        end if
        if (allocated(vs%basin%alvs)) then
            where (vs%basin%alvs /= huge(vs%basin%alvs) .and. albtfrac > 0.0) vs%basin%alvs = vs%basin%alvs/albtfrac
        end if
        if (allocated(vs%basin%alir)) then
            where (vs%basin%alir /= huge(vs%basin%alir) .and. albtfrac > 0.0) vs%basin%alir = vs%basin%alir/albtfrac
        end if
        if (allocated(vs%basin%gte)) then
            where (vs%basin%gte /= huge(vs%basin%gte)) vs%basin%gte = vs%basin%gte/frac
        end if
        if (allocated(vs%basin%zpnd)) then
            where (vs%basin%zpnd /= huge(vs%basin%zpnd)) vs%basin%zpnd = vs%basin%zpnd/frac
        end if
!-        if (allocated(vs%basin%lqwspnd)) then
!-            if (all(vs%basin%lqwspnd /= huge(vs%basin%lqwspnd))) vs%basin%lqwspnd = vs%basin%lqwspnd/frac
!-        end if
        if (allocated(vs%basin%tpnd)) then
            where (vs%basin%tpnd /= huge(vs%basin%tpnd) .and. tpndfrac > 0.0) vs%basin%tpnd = vs%basin%tpnd/tpndfrac
        end if
        if (allocated(vs%basin%pndcaf)) then
            where (vs%basin%pndcaf /= huge(vs%basin%pndcaf) .and. tpndfrac > 0.0) vs%basin%pndcaf = vs%basin%pndcaf/tpndfrac
        end if
        if (allocated(vs%basin%potevp)) then
            where (vs%basin%potevp /= huge(vs%basin%potevp)) vs%basin%potevp = vs%basin%potevp/frac
        end if
        if (allocated(vs%basin%et)) then
            where (vs%basin%et /= huge(vs%basin%et)) vs%basin%et = vs%basin%et/frac
        end if
!-        if (allocated(vs%basin%evpb)) then
!-            if (all(vs%basin%evpb /= huge(vs%basin%evpb))) vs%basin%evpb = vs%basin%evpb/frac
!-        end if
!-        if (allocated(vs%basin%arrd)) then
!-            if (all(vs%basin%arrd /= huge(vs%basin%arrd))) vs%basin%arrd = vs%basin%arrd/frac
!-        end if
        if (allocated(vs%basin%ovrflw)) then
            where (vs%basin%ovrflw /= huge(vs%basin%ovrflw)) vs%basin%ovrflw = vs%basin%ovrflw/frac
        end if
        if (allocated(vs%basin%qevp)) then
            where (vs%basin%qevp /= huge(vs%basin%qevp)) vs%basin%qevp = vs%basin%qevp/frac
        end if
        if (allocated(vs%basin%qsens)) then
            where (vs%basin%qsens /= huge(vs%basin%qsens)) vs%basin%qsens = vs%basin%qsens/frac
        end if
        if (allocated(vs%basin%gzero)) then
            where (vs%basin%gzero /= huge(vs%basin%gzero)) vs%basin%gzero = vs%basin%gzero/frac
        end if
        if (allocated(vs%basin%tsfs)) then
            do j = 1, 4
                where (vs%basin%tsfs(:, j) /= huge(vs%basin%tsfs)) vs%basin%tsfs(:, j) = vs%basin%tsfs(:, j)/frac
            end do
        end if
        if (allocated(vs%basin%tsurf)) then
            where (vs%basin%tsurf /= huge(vs%basin%tsurf)) vs%basin%tsurf = vs%basin%tsurf/frac
        end if

        !> Ice/glacier variables.
        if (allocated(vs%basin%lqwsice)) then
            where (vs%basin%lqwsice /= huge(vs%basin%lqwsice)) vs%basin%lqwsice = vs%basin%lqwsice/frac
        end if
        if (allocated(vs%basin%tice)) then
            where (vs%basin%tice /= huge(vs%basin%tice) .and. ticefrac > 0.0) vs%basin%tice = vs%basin%tice/ticefrac
        end if

        !> Subsurface/soil variables.
        do j = 1, shd%lc%IGND
            if (allocated(vs%basin%dzsol)) then
                where(vs%basin%dzsol(:, j) /= huge(vs%basin%dzsol)) vs%basin%dzsol(:, j) = vs%basin%dzsol(:, j)/frac
            end if
            if (allocated(vs%basin%dzsolhyd)) then
                where (vs%basin%dzsolhyd(:, j) /= huge(vs%basin%dzsolhyd)) vs%basin%dzsolhyd(:, j) = vs%basin%dzsolhyd(:, j)/frac
            end if
            if (allocated(vs%basin%thlqsol)) then
                where (vs%basin%thlqsol(:, j) /= huge(vs%basin%thlqsol)) vs%basin%thlqsol(:, j) = vs%basin%thlqsol(:, j)/frac
            end if
            if (allocated(vs%basin%thicsol)) then
                where (vs%basin%thicsol(:, j) /= huge(vs%basin%thicsol)) vs%basin%thicsol(:, j) = vs%basin%thicsol(:, j)/frac
            end if
            if (allocated(vs%basin%lqwssol)) then
                where (vs%basin%lqwssol(:, j) /= huge(vs%basin%lqwssol)) vs%basin%lqwssol(:, j) = vs%basin%lqwssol(:, j)/frac
            end if
            if (allocated(vs%basin%fzwssol)) then
                where (vs%basin%fzwssol(:, j) /= huge(vs%basin%fzwssol)) vs%basin%fzwssol(:, j) = vs%basin%fzwssol(:, j)/frac
            end if
            if (allocated(vs%basin%tsol)) then
                where (vs%basin%tsol(:, j) /= huge(vs%basin%tsol)) vs%basin%tsol(:, j) = vs%basin%tsol(:, j)/frac
            end if
            if (allocated(vs%basin%gflx)) then
                where (vs%basin%gflx(:, j) /= huge(vs%basin%gflx)) vs%basin%gflx(:, j) = vs%basin%gflx(:, j)/frac
            end if
            if (allocated(vs%basin%hcps)) then
                where (vs%basin%hcps(:, j) /= huge(vs%basin%hcps)) vs%basin%hcps(:, j) = vs%basin%hcps(:, j)/frac
            end if
            if (allocated(vs%basin%hcpc)) then
                where (vs%basin%hcpc(:, j) /= huge(vs%basin%hcpc)) vs%basin%hcpc(:, j) = vs%basin%hcpc(:, j)/frac
            end if
            if (allocated(vs%basin%hcpg)) then
                where (vs%basin%hcpg(:, j) /= huge(vs%basin%hcpg)) vs%basin%hcpg(:, j) = vs%basin%hcpg(:, j)/frac
            end if
            if (allocated(vs%basin%tctopc)) then
                where (vs%basin%tctopc(:, j) /= huge(vs%basin%tctopc)) vs%basin%tctopc(:, j) = vs%basin%tctopc(:, j)/frac
            end if
            if (allocated(vs%basin%tctopg)) then
                where (vs%basin%tctopg(:, j) /= huge(vs%basin%tctopg)) vs%basin%tctopg(:, j) = vs%basin%tctopg(:, j)/frac
            end if
            if (allocated(vs%basin%tcbotc)) then
                where (vs%basin%tcbotc(:, j) /= huge(vs%basin%tcbotc)) vs%basin%tcbotc(:, j) = vs%basin%tcbotc(:, j)/frac
            end if
            if (allocated(vs%basin%tctopg)) then
                where (vs%basin%tctopg(:, j) /= huge(vs%basin%tctopg)) vs%basin%tctopg(:, j) = vs%basin%tctopg(:, j)/frac
            end if
            if (allocated(vs%basin%latflw)) then
                where (vs%basin%latflw(:, j) /= huge(vs%basin%latflw)) vs%basin%latflw(:, j) = vs%basin%latflw(:, j)/frac
            end if
        end do
        if (allocated(vs%basin%zsol)) then
            where (vs%basin%zsol /= huge(vs%basin%zsol)) vs%basin%zsol = vs%basin%zsol/frac
        end if
        if (allocated(vs%basin%zsolhyd)) then
            where (vs%basin%zsolhyd /= huge(vs%basin%zsolhyd)) vs%basin%zsolhyd = vs%basin%zsolhyd/frac
        end if
        if (allocated(vs%basin%zsolsat)) then
            where (vs%basin%zsolsat /= huge(vs%basin%zsolsat)) vs%basin%zsolsat = vs%basin%zsolsat/frac
        end if
!-        if (allocated(vs%basin%ggeo)) then
!-            where (vs%basin%ggeo /= huge(vs%basin%ggeo)) vs%basin%ggeo = vs%basin%ggeo/frac
!-        end if
        if (allocated(vs%basin%tbas)) then
            where (vs%basin%tbas /= huge(vs%basin%tbas)) vs%basin%tbas = vs%basin%tbas/frac
        end if
        if (allocated(vs%basin%drainsol)) then
            where (vs%basin%drainsol /= huge(vs%basin%drainsol)) vs%basin%drainsol = vs%basin%drainsol/frac
        end if

        !> Groundwater/lower zone storage variables.
        if (allocated(vs%basin%rchg)) then
            where (vs%basin%rchg /= huge(vs%basin%rchg)) vs%basin%rchg = vs%basin%rchg/frac
        end if
        if (allocated(vs%basin%stggw)) then
            where (vs%basin%stggw /= huge(vs%basin%stggw)) vs%basin%stggw = vs%basin%stggw/frac
        end if
        if (allocated(vs%basin%lkg)) then
            where (vs%basin%lkg /= huge(vs%basin%lkg)) vs%basin%lkg = vs%basin%lkg/frac
        end if
!-        if (allocated(vs%basin%dzs)) then
!-            if (all(vs%basin%dzs /= huge(vs%basin%dzs))) vs%basin%dzs = vs%basin%dzs/frac
!-        end if

        !> Routing variables.
        if (allocated(vs%basin%rff)) then
            where (vs%basin%rff /= huge(vs%basin%rff)) vs%basin%rff = vs%basin%rff/frac
        end if
!        if (allocated(vs%basin%qi)) then
!            if (all(vs%basin%qi /= huge(vs%basin%qi))) vs%basin%qi = vs%basin%qi/frac
!        end if
!        if (allocated(vs%basin%qo)) then
!            if (all(vs%basin%qo /= huge(vs%basin%qo))) vs%basin%qo = vs%basin%qo/frac
!        end if
!        if (allocated(vs%basin%stgch)) then
!            if (all(vs%basin%stgch /= huge(vs%basin%stgch))) vs%basin%stgch = vs%basin%stgch/frac
!        end if
!        if (allocated(vs%basin%zlvl)) then
!            if (all(vs%basin%zlvl /= huge(vs%basin%zlvl))) vs%basin%zlvl = vs%basin%zlvl/frac
!        end if
!        if (allocated(vs%basin%div)) then
!            if (all(vs%basin%div /= huge(vs%basin%div))) vs%basin%div = vs%basin%div/frac
!        end if
!        if (allocated(vs%basin%abstr)) then
!            if (all(vs%basin%abstr /= huge(vs%basin%abstr))) vs%basin%abstr = vs%basin%abstr/frac
!        end if

    end subroutine

    subroutine run_between_grid_finalize(fls, shd)

        !> Process modules.
        use WF_ROUTE_config
        use rte_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Call processes.
        call WF_ROUTE_finalize(fls, shd)
        call run_rte_finalize(fls, shd)

    end subroutine

end module
