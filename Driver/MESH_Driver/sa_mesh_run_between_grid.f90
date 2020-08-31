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
        integer :: KDLY = 0, KTS = 1
        integer :: kmin = 0, kmax = 1
        integer :: freq = 1
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
        integer :: KDLY = 0, KTS = 1, KHLY = 2
        integer :: kmin = 0, kmax = 2
        integer :: freq = 0
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

    contains

    subroutine run_between_grid_init(fls, shd, cm)

        !> Process modules.
        use SA_RTE_module
        use WF_ROUTE_config
        use rte_module
        use cropland_irrigation_between_grid

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
        call output_variables_activate(out%d%grid, (/ VN_QI, VN_STGCH, VN_QO, VN_ZLVL /))

        !> Call processes.
        call SA_RTE_init(shd)
        call WF_ROUTE_init(fls, shd)
        call run_rte_init(fls, shd)
        call runci_between_grid_init(shd, fls)

        !> Update basin variables.
        call run_within_grid_stas_basin_update(fls, shd, cm)

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
                    basin_SCA = basin_SCA + vs%tile%fsno(k)*FRAC
                    basin_SWE = basin_SWE + vs%tile%sno(k)*FRAC
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
        if (ro%RUNLSS) then
            vs%grid%rff = (vs%grid%rofo + sum(vs%grid%rofs, 2))*ic%dts
            vs%grid%rchg = vs%grid%rofb*ic%dts
        end if

        !> Call processes.
        call SA_RTE(shd)
        call WF_ROUTE_between_grid(fls, shd)
        call run_rte_between_grid(fls, shd)
        call runci_between_grid(shd, fls, cm)

        !> Update basin variables.
        call run_within_grid_stas_basin_update(fls, shd, cm)

        !> Update output variables.
!todo: remove this when code for output files has moved.
        call output_variables_update(shd)

        if (mod(ic%ts_hourly*ic%dts, RTE_TS) == 0 .and. ro%RUNCHNL) then

            where (shd%DA > 0.0)
                WF_QO2_ACC_MM = WF_QO2_ACC_MM + vs%grid%qo/shd%DA/1000.0*RTE_TS
                WF_STORE2_ACC_MM = WF_STORE2_ACC_MM + vs%grid%stgch/shd%DA/1000.0
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
        if (ic%now%day /= ic%next%day .and. ro%RUNCHNL) then

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

    subroutine run_within_grid_stas_basin_update(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer j, ii, i
        real albtfrac(shd%NA), tpndfrac(shd%NA), tsnofrac(shd%NA), tcanfrac(shd%NA), frac(shd%NA)

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Initialize variables.
        if (associated(vs%basin%fsin) .and. associated(vs%grid%fsin)) vs%basin%fsin = vs%grid%fsin*shd%FRAC
        if (associated(vs%basin%fsdr) .and. associated(vs%grid%fsdr)) vs%basin%fsdr = vs%grid%fsdr*shd%FRAC
        if (associated(vs%basin%fsdff) .and. associated(vs%grid%fsdff)) vs%basin%fsdff = vs%grid%fsdff*shd%FRAC
        if (associated(vs%basin%flin) .and. associated(vs%grid%flin)) vs%basin%flin = vs%grid%flin*shd%FRAC
        if (associated(vs%basin%ta) .and. associated(vs%grid%ta)) vs%basin%ta = vs%grid%ta*shd%FRAC
        if (associated(vs%basin%qa) .and. associated(vs%grid%qa)) vs%basin%qa = vs%grid%qa*shd%FRAC
        if (associated(vs%basin%pres) .and. associated(vs%grid%pres)) vs%basin%pres = vs%grid%pres*shd%FRAC
        if (associated(vs%basin%uv) .and. associated(vs%grid%uv)) vs%basin%uv = vs%grid%uv*shd%FRAC
        if (associated(vs%basin%wdir) .and. associated(vs%grid%wdir)) vs%basin%wdir = vs%grid%wdir*shd%FRAC
        if (associated(vs%basin%uu) .and. associated(vs%grid%uu)) vs%basin%uu = vs%grid%uu*shd%FRAC
        if (associated(vs%basin%vv) .and. associated(vs%grid%vv)) vs%basin%vv = vs%grid%vv*shd%FRAC
        if (associated(vs%basin%pre) .and. associated(vs%grid%pre)) vs%basin%pre = vs%grid%pre*shd%FRAC
        if (associated(vs%basin%prern) .and. associated(vs%grid%prern)) vs%basin%prern = vs%grid%prern*shd%FRAC
        if (associated(vs%basin%presno) .and. associated(vs%grid%presno)) vs%basin%presno = vs%grid%presno*shd%FRAC
        if (associated(vs%basin%rcan) .and. associated(vs%grid%rcan)) vs%basin%rcan = vs%grid%rcan*shd%FRAC
        if (associated(vs%basin%sncan) .and. associated(vs%grid%sncan)) vs%basin%sncan = vs%grid%sncan*shd%FRAC
        if (associated(vs%basin%cmas) .and. associated(vs%grid%cmas)) vs%basin%cmas = vs%grid%cmas*shd%FRAC
        if (associated(vs%basin%tac) .and. associated(vs%grid%tac)) vs%basin%tac = vs%grid%tac*shd%FRAC
        if (associated(vs%basin%qac) .and. associated(vs%grid%qac)) vs%basin%qac = vs%grid%qac*shd%FRAC
        if (associated(vs%basin%tcan) .and. associated(vs%grid%tcan)) then
            vs%basin%tcan = vs%grid%tcan*shd%FRAC
            where (vs%basin%tcan > 0.0)
                tcanfrac = shd%FRAC
            elsewhere
                tcanfrac = 0.0
            end where
        end if
        if (associated(vs%basin%gro) .and. associated(vs%grid%gro)) vs%basin%gro = vs%grid%gro*shd%FRAC
        if (associated(vs%basin%sno) .and. associated(vs%grid%sno)) vs%basin%sno = vs%grid%sno*shd%FRAC
        if (associated(vs%basin%rhos) .and. associated(vs%grid%rhos)) vs%basin%rhos = vs%grid%rhos*shd%FRAC
        if (associated(vs%basin%zsno) .and. associated(vs%grid%zsno)) vs%basin%zsno = vs%grid%zsno*shd%FRAC
        if (associated(vs%basin%fsno) .and. associated(vs%grid%fsno)) vs%basin%fsno = vs%grid%fsno*shd%FRAC
        if (associated(vs%basin%albs) .and. associated(vs%grid%albs)) vs%basin%albs = vs%grid%albs*shd%FRAC
        if (associated(vs%basin%wsno) .and. associated(vs%grid%wsno)) vs%basin%wsno = vs%grid%wsno*shd%FRAC
        if (associated(vs%basin%tsno) .and. associated(vs%grid%tsno)) then
            vs%basin%tsno = vs%grid%tsno*shd%FRAC
            where (vs%basin%tsno > 0.0)
                tsnofrac = shd%FRAC
            elsewhere
                tsnofrac = 0.0
            end where
        end if
        if (associated(vs%basin%rofsno) .and. associated(vs%grid%rofsno)) vs%basin%rofsno = vs%grid%rofsno*shd%FRAC
        if (associated(vs%basin%albt) .and. associated(vs%grid%albt)) then
            vs%basin%albt = vs%grid%albt*shd%FRAC
            where (vs%basin%albt > 0.0)
                albtfrac = shd%FRAC
            elsewhere
                albtfrac = 0.0
            end where
        end if
        if (associated(vs%basin%alvs) .and. associated(vs%grid%alvs)) vs%basin%alvs = vs%grid%alvs*shd%FRAC
        if (associated(vs%basin%alir) .and. associated(vs%grid%alir)) vs%basin%alir = vs%grid%alir*shd%FRAC
        if (associated(vs%basin%gte) .and. associated(vs%grid%gte)) vs%basin%gte = vs%grid%gte*shd%FRAC
        if (associated(vs%basin%zpnd) .and. associated(vs%grid%zpnd)) vs%basin%zpnd = vs%grid%zpnd*shd%FRAC
        if (associated(vs%basin%pndw) .and. associated(vs%grid%pndw)) vs%basin%pndw = vs%grid%pndw*shd%FRAC
        if (associated(vs%basin%tpnd) .and. associated(vs%grid%tpnd)) then
            vs%basin%tpnd = vs%grid%tpnd*shd%FRAC
            where (vs%basin%tpnd > 0.0)
                tpndfrac = shd%FRAC
            elsewhere
                tpndfrac = 0.0
            end where
        end if
        if (associated(vs%basin%fstr) .and. associated(vs%grid%fstr)) vs%basin%fstr = vs%grid%fstr*shd%FRAC
        if (associated(vs%basin%pevp) .and. associated(vs%grid%pevp)) vs%basin%pevp = vs%grid%pevp*shd%FRAC
        if (associated(vs%basin%evap) .and. associated(vs%grid%evap)) vs%basin%evap = vs%grid%evap*shd%FRAC
        if (associated(vs%basin%evpb) .and. associated(vs%grid%evpb)) vs%basin%evpb = vs%grid%evpb*shd%FRAC
        if (associated(vs%basin%arrd) .and. associated(vs%grid%arrd)) vs%basin%arrd = vs%grid%arrd*shd%FRAC
        if (associated(vs%basin%rofo) .and. associated(vs%grid%rofo)) vs%basin%rofo = vs%grid%rofo*shd%FRAC
        if (associated(vs%basin%qevp) .and. associated(vs%grid%qevp)) vs%basin%qevp = vs%grid%qevp*shd%FRAC
        if (associated(vs%basin%hfs) .and. associated(vs%grid%hfs)) vs%basin%hfs = vs%grid%hfs*shd%FRAC
        if (associated(vs%basin%gzero) .and. associated(vs%grid%gzero)) vs%basin%gzero = vs%grid%gzero*shd%FRAC
        do j = 1, 4
            if (associated(vs%basin%tsfs) .and. associated(vs%grid%tsfs)) vs%basin%tsfs(:, j) = vs%grid%tsfs(:, j)*shd%FRAC
        end do
        if (associated(vs%basin%ggeo) .and. associated(vs%grid%ggeo)) vs%basin%ggeo = vs%grid%ggeo*shd%FRAC
        if (associated(vs%basin%tbas) .and. associated(vs%grid%tbas)) vs%basin%tbas = vs%grid%tbas*shd%FRAC
        do j = 1, shd%lc%IGND
            if (associated(vs%basin%thlq) .and. associated(vs%grid%thlq)) vs%basin%thlq(:, j) = vs%grid%thlq(:, j)*shd%FRAC
            if (associated(vs%basin%thic) .and. associated(vs%grid%thic)) vs%basin%thic(:, j) = vs%grid%thic(:, j)*shd%FRAC
            if (associated(vs%basin%lqws) .and. associated(vs%grid%lqws)) vs%basin%lqws(:, j) = vs%grid%lqws(:, j)*shd%FRAC
            if (associated(vs%basin%fzws) .and. associated(vs%grid%fzws)) vs%basin%fzws(:, j) = vs%grid%fzws(:, j)*shd%FRAC
            if (associated(vs%basin%tbar) .and. associated(vs%grid%tbar)) vs%basin%tbar(:, j) = vs%grid%tbar(:, j)*shd%FRAC
            if (associated(vs%basin%gflx) .and. associated(vs%grid%gflx)) vs%basin%gflx(:, j) = vs%grid%gflx(:, j)*shd%FRAC
            if (associated(vs%basin%rofs) .and. associated(vs%grid%rofs)) vs%basin%rofs(:, j) = vs%grid%rofs(:, j)*shd%FRAC
            if (associated(vs%basin%delzw) .and. associated(vs%grid%delzw)) vs%basin%delzw(:, j) = vs%grid%delzw(:, j)*shd%FRAC
            if (associated(vs%basin%zbotw) .and. associated(vs%grid%zbotw)) vs%basin%zbotw(:, j) = vs%grid%zbotw(:, j)*shd%FRAC
        end do
        if (associated(vs%basin%rofb) .and. associated(vs%grid%rofb)) vs%basin%rofb = vs%grid%rofb*shd%FRAC
        if (associated(vs%basin%rchg) .and. associated(vs%grid%rchg)) vs%basin%rchg = vs%grid%rchg*shd%FRAC
        if (associated(vs%basin%lzs) .and. associated(vs%grid%lzs)) vs%basin%lzs = vs%grid%lzs*shd%FRAC
        if (associated(vs%basin%dzs) .and. associated(vs%grid%dzs)) vs%basin%dzs = vs%grid%dzs*shd%FRAC
        frac = shd%FRAC

        !> Update variables.
        do i = 1, shd%NAA
            ii = shd%NEXT(i)
            if (ii > 0) then
                if (associated(vs%basin%fsin)) vs%basin%fsin(ii) = vs%basin%fsin(ii) + vs%basin%fsin(i)
                if (associated(vs%basin%fsdr)) vs%basin%fsdr(ii) = vs%basin%fsdr(ii) + vs%basin%fsdr(i)
                if (associated(vs%basin%fsdff)) vs%basin%fsdff(ii) = vs%basin%fsdff(ii) + vs%basin%fsdff(i)
                if (associated(vs%basin%flin)) vs%basin%flin(ii) = vs%basin%flin(ii) + vs%basin%flin(i)
                if (associated(vs%basin%ta)) vs%basin%ta(ii) = vs%basin%ta(ii) + vs%basin%ta(i)
                if (associated(vs%basin%qa)) vs%basin%qa(ii) = vs%basin%qa(ii) + vs%basin%qa(i)
                if (associated(vs%basin%pres)) vs%basin%pres(ii) = vs%basin%pres(ii) + vs%basin%pres(i)
                if (associated(vs%basin%uv)) vs%basin%uv(ii) = vs%basin%uv(ii) + vs%basin%uv(i)
                if (associated(vs%basin%wdir)) vs%basin%wdir(ii) = vs%basin%wdir(ii) + vs%basin%wdir(i)
                if (associated(vs%basin%uu)) vs%basin%uu(ii) = vs%basin%uu(ii) + vs%basin%uu(i)
                if (associated(vs%basin%vv)) vs%basin%vv(ii) = vs%basin%vv(ii) + vs%basin%vv(i)
                if (associated(vs%basin%pre)) vs%basin%pre(ii) = vs%basin%pre(ii) + vs%basin%pre(i)
                if (associated(vs%basin%prern)) vs%basin%prern(ii) = vs%basin%prern(ii) + vs%basin%prern(i)
                if (associated(vs%basin%presno)) vs%basin%presno(ii) = vs%basin%presno(ii) + vs%basin%presno(i)
                if (associated(vs%basin%rcan)) vs%basin%rcan(ii) = vs%basin%rcan(ii) + vs%basin%rcan(i)
                if (associated(vs%basin%sncan)) vs%basin%sncan(ii) = vs%basin%sncan(ii) + vs%basin%sncan(i)
                if (associated(vs%basin%cmas)) vs%basin%cmas(ii) = vs%basin%cmas(ii) + vs%basin%cmas(i)
                if (associated(vs%basin%tac)) vs%basin%tac(ii) = vs%basin%tac(ii) + vs%basin%tac(i)
                if (associated(vs%basin%qac)) vs%basin%qac(ii) = vs%basin%qac(ii) + vs%basin%qac(i)
                if (associated(vs%basin%tcan)) then
                    vs%basin%tcan(ii) = vs%basin%tcan(ii) + vs%basin%tcan(i)
                    if (vs%basin%tcan(i) > 0.0) then
                        tcanfrac(ii) = tcanfrac(ii) + tcanfrac(i)
                    end if
                end if
                if (associated(vs%basin%gro)) vs%basin%gro(ii) = vs%basin%gro(ii) + vs%basin%gro(i)
                if (associated(vs%basin%sno)) vs%basin%sno(ii) = vs%basin%sno(ii) + vs%basin%sno(i)
                if (associated(vs%basin%rhos)) vs%basin%rhos(ii) = vs%basin%rhos(ii) + vs%basin%rhos(i)
                if (associated(vs%basin%zsno)) vs%basin%zsno(ii) = vs%basin%zsno(ii) + vs%basin%zsno(i)
                if (associated(vs%basin%fsno)) vs%basin%fsno(ii) = vs%basin%fsno(ii) + vs%basin%fsno(i)
                if (associated(vs%basin%albs)) vs%basin%albs(ii) = vs%basin%albs(ii) + vs%basin%albs(i)
                if (associated(vs%basin%wsno)) vs%basin%wsno(ii) = vs%basin%wsno(ii) + vs%basin%wsno(i)
                if (associated(vs%basin%tsno)) then
                    vs%basin%tsno(ii) = vs%basin%tsno(ii) + vs%basin%tsno(i)
                    if (vs%basin%tsno(i) > 0.0) then
                        tsnofrac(ii) = tsnofrac(ii) + tsnofrac(i)
                    end if
                end if
                if (associated(vs%basin%rofsno)) vs%basin%rofsno(ii) = vs%basin%rofsno(ii) + vs%basin%rofsno(i)
                if (associated(vs%basin%albt)) then
                    vs%basin%albt(ii) = vs%basin%albt(ii) + vs%basin%albt(i)
                    if (vs%basin%albt(i) > 0.0) then
                        albtfrac(ii) = albtfrac(ii) + albtfrac(i)
                    end if
                end if
                if (associated(vs%basin%alvs)) vs%basin%alvs(ii) = vs%basin%alvs(ii) + vs%basin%alvs(i)
                if (associated(vs%basin%alir)) vs%basin%alir(ii) = vs%basin%alir(ii) + vs%basin%alir(i)
                if (associated(vs%basin%gte)) vs%basin%gte(ii) = vs%basin%gte(ii) + vs%basin%gte(i)
                if (associated(vs%basin%zpnd)) vs%basin%zpnd(ii) = vs%basin%zpnd(ii) + vs%basin%zpnd(i)
                if (associated(vs%basin%pndw)) vs%basin%pndw(ii) = vs%basin%pndw(ii) + vs%basin%pndw(i)
                if (associated(vs%basin%tpnd)) then
                    vs%basin%tpnd(ii) = vs%basin%tpnd(ii) + vs%basin%tpnd(i)
                    if (vs%basin%tpnd(i) > 0.0) then
                        tpndfrac(ii) = tpndfrac(ii) + tpndfrac(i)
                    end if
                end if
                if (associated(vs%basin%fstr)) vs%basin%fstr(ii) = vs%basin%fstr(ii) + vs%basin%fstr(i)
                if (associated(vs%basin%pevp)) vs%basin%pevp(ii) = vs%basin%pevp(ii) + vs%basin%pevp(i)
                if (associated(vs%basin%evap)) vs%basin%evap(ii) = vs%basin%evap(ii) + vs%basin%evap(i)
                if (associated(vs%basin%evpb)) vs%basin%evpb(ii) = vs%basin%evpb(ii) + vs%basin%evpb(i)
                if (associated(vs%basin%arrd)) vs%basin%arrd(ii) = vs%basin%arrd(ii) + vs%basin%arrd(i)
                if (associated(vs%basin%rofo)) vs%basin%rofo(ii) = vs%basin%rofo(ii) + vs%basin%rofo(i)
                if (associated(vs%basin%qevp)) vs%basin%qevp(ii) = vs%basin%qevp(ii) + vs%basin%qevp(i)
                if (associated(vs%basin%hfs)) vs%basin%hfs(ii) = vs%basin%hfs(ii) + vs%basin%hfs(i)
                if (associated(vs%basin%gzero)) vs%basin%gzero(ii) = vs%basin%gzero(ii) + vs%basin%gzero(i)
                if (associated(vs%basin%tsfs)) vs%basin%tsfs(ii, :) = vs%basin%tsfs(ii, :) + vs%basin%tsfs(i, :)
                if (associated(vs%basin%ggeo)) vs%basin%ggeo(ii) = vs%basin%ggeo(ii) + vs%basin%ggeo(i)
                if (associated(vs%basin%tbas)) vs%basin%tbas(ii) = vs%basin%tbas(ii) + vs%basin%tbas(i)
                if (associated(vs%basin%thlq)) vs%basin%thlq(ii, :) = vs%basin%thlq(ii, :) + vs%basin%thlq(i, :)
                if (associated(vs%basin%thic)) vs%basin%thic(ii, :) = vs%basin%thic(ii, :) + vs%basin%thic(i, :)
                if (associated(vs%basin%lqws)) vs%basin%lqws(ii, :) = vs%basin%lqws(ii, :) + vs%basin%lqws(i, :)
                if (associated(vs%basin%fzws)) vs%basin%fzws(ii, :) = vs%basin%fzws(ii, :) + vs%basin%fzws(i, :)
                if (associated(vs%basin%tbar)) vs%basin%tbar(ii, :) = vs%basin%tbar(ii, :) + vs%basin%tbar(i, :)
                if (associated(vs%basin%gflx)) vs%basin%gflx(ii, :) = vs%basin%gflx(ii, :) + vs%basin%gflx(i, :)
                if (associated(vs%basin%rofs)) vs%basin%rofs(ii, :) = vs%basin%rofs(ii, :) + vs%basin%rofs(i, :)
                if (associated(vs%basin%delzw)) vs%basin%delzw(ii, :) = vs%basin%delzw(ii, :) + vs%basin%delzw(i, :)
                if (associated(vs%basin%zbotw)) vs%basin%zbotw(ii, :) = vs%basin%zbotw(ii, :) + vs%basin%zbotw(i, :)
                if (associated(vs%basin%rofb)) vs%basin%rofb(ii) = vs%basin%rofb(ii) + vs%basin%rofb(i)
                if (associated(vs%basin%rchg)) vs%basin%rchg(ii) = vs%basin%rchg(ii) + vs%basin%rchg(i)
                if (associated(vs%basin%lzs)) vs%basin%lzs(ii) = vs%basin%lzs(ii) + vs%basin%lzs(i)
                if (associated(vs%basin%dzs)) vs%basin%dzs(ii) = vs%basin%dzs(ii) + vs%basin%dzs(i)
                if (associated(vs%basin%stge)) vs%basin%stge(ii) = vs%basin%stge(ii) + vs%basin%stge(i)
                if (associated(vs%basin%stgw)) vs%basin%stgw(ii) = vs%basin%stgw(ii) + vs%basin%stgw(i)
                if (associated(vs%basin%rff)) vs%basin%rff(ii) = vs%basin%rff(ii) + vs%basin%rff(i)
                if (associated(vs%basin%qi)) vs%basin%qi(ii) = vs%basin%qi(ii) + vs%basin%qi(i)
                if (associated(vs%basin%qo)) vs%basin%qo(ii) = vs%basin%qo(ii) + vs%basin%qo(i)
                if (associated(vs%basin%stgch)) vs%basin%stgch(ii) = vs%basin%stgch(ii) + vs%basin%stgch(i)
                if (associated(vs%basin%zlvl)) vs%basin%zlvl(ii) = vs%basin%zlvl(ii) + vs%basin%zlvl(i)
                if (associated(vs%basin%div)) vs%basin%div(ii) = vs%basin%div(ii) + vs%basin%div(i)
                if (associated(vs%basin%ab)) vs%basin%ab(ii) = vs%basin%ab(ii) + vs%basin%ab(i)
                frac(ii) = frac(ii) + frac(i)
            end if
        end do

        !> Check for division by zero.
        where (frac == 0.0) frac = 1.0

        !> DA average.
        if (associated(vs%basin%fsin)) vs%basin%fsin = vs%basin%fsin/frac
        if (associated(vs%basin%fsdr)) vs%basin%fsdr = vs%basin%fsdr/frac
        if (associated(vs%basin%fsdff)) vs%basin%fsdff = vs%basin%fsdff/frac
        if (associated(vs%basin%flin)) vs%basin%flin = vs%basin%flin/frac
        if (associated(vs%basin%ta)) vs%basin%ta = vs%basin%ta/frac
        if (associated(vs%basin%qa)) vs%basin%qa = vs%basin%qa/frac
        if (associated(vs%basin%pres)) vs%basin%pres = vs%basin%pres/frac
        if (associated(vs%basin%uv)) vs%basin%uv = vs%basin%uv/frac
        if (associated(vs%basin%wdir)) vs%basin%wdir = vs%basin%wdir/frac
        if (associated(vs%basin%uu)) vs%basin%uu = vs%basin%uu/frac
        if (associated(vs%basin%vv)) vs%basin%vv = vs%basin%vv/frac
        if (associated(vs%basin%pre)) vs%basin%pre = vs%basin%pre/frac
        if (associated(vs%basin%prern)) vs%basin%prern = vs%basin%prern/frac
        if (associated(vs%basin%presno)) vs%basin%presno = vs%basin%presno/frac
        if (associated(vs%basin%rcan)) vs%basin%rcan = vs%basin%rcan/frac
        if (associated(vs%basin%sncan)) vs%basin%sncan = vs%basin%sncan/frac
        if (associated(vs%basin%cmas)) then
            where (tcanfrac > 0.0) vs%basin%cmas = vs%basin%cmas/tcanfrac
        end if
        if (associated(vs%basin%tac)) then
            where (tcanfrac > 0.0) vs%basin%tac = vs%basin%tac/tcanfrac
        end if
        if (associated(vs%basin%qac)) then
            where (tcanfrac > 0.0) vs%basin%qac = vs%basin%qac/tcanfrac
        end if
        if (associated(vs%basin%tcan)) then
            where (tcanfrac > 0.0) vs%basin%tcan = vs%basin%tcan/tcanfrac
        end if
        if (associated(vs%basin%gro)) then
            where (tcanfrac > 0.0) vs%basin%gro = vs%basin%gro/tcanfrac
        end if
        if (associated(vs%basin%sno)) vs%basin%sno = vs%basin%sno/frac
        if (associated(vs%basin%rhos)) then
            where (tsnofrac > 0.0) vs%basin%rhos = vs%basin%rhos/tsnofrac
        end if
        if (associated(vs%basin%zsno)) vs%basin%zsno = vs%basin%zsno/frac
        if (associated(vs%basin%fsno)) vs%basin%fsno = vs%basin%fsno/frac
        if (associated(vs%basin%albs)) then
            where (tsnofrac > 0.0) vs%basin%albs = vs%basin%albs/tsnofrac
        end if
        if (associated(vs%basin%wsno)) vs%basin%wsno = vs%basin%wsno/frac
        if (associated(vs%basin%tsno)) then
            where (tsnofrac > 0.0) vs%basin%tsno = vs%basin%tsno/tsnofrac
        end if
        if (associated(vs%basin%rofsno)) vs%basin%rofsno = vs%basin%rofsno/frac
        if (associated(vs%basin%albt)) then
            where (albtfrac > 0.0) vs%basin%albt = vs%basin%albt/albtfrac
        end if
        if (associated(vs%basin%alvs)) then
            where (albtfrac > 0.0) vs%basin%alvs = vs%basin%alvs/albtfrac
        end if
        if (associated(vs%basin%alir)) then
            where (albtfrac > 0.0) vs%basin%alir = vs%basin%alir/albtfrac
        end if
        if (associated(vs%basin%gte)) vs%basin%gte = vs%basin%gte/frac
        if (associated(vs%basin%zpnd)) vs%basin%zpnd = vs%basin%zpnd/frac
        if (associated(vs%basin%pndw)) vs%basin%pndw = vs%basin%pndw/frac
        if (associated(vs%basin%tpnd)) then
            where (tpndfrac > 0.0) vs%basin%tpnd = vs%basin%tpnd/tpndfrac
        end if
        if (associated(vs%basin%fstr)) then
            where (tpndfrac > 0.0) vs%basin%fstr = vs%basin%fstr/tpndfrac
        end if
        if (associated(vs%basin%pevp)) vs%basin%pevp = vs%basin%pevp/frac
        if (associated(vs%basin%evap)) vs%basin%evap = vs%basin%evap/frac
        if (associated(vs%basin%evpb)) vs%basin%evpb = vs%basin%evpb/frac
        if (associated(vs%basin%arrd)) vs%basin%arrd = vs%basin%arrd/frac
        if (associated(vs%basin%rofo)) vs%basin%rofo = vs%basin%rofo/frac
        if (associated(vs%basin%qevp)) vs%basin%qevp = vs%basin%qevp/frac
        if (associated(vs%basin%hfs)) vs%basin%hfs = vs%basin%hfs/frac
        if (associated(vs%basin%gzero)) vs%basin%gzero = vs%basin%gzero/frac
        do j = 1, 4
            if (associated(vs%basin%tsfs)) vs%basin%tsfs(:, j) = vs%basin%tsfs(:, j)/frac
        end do
        if (associated(vs%basin%ggeo)) vs%basin%ggeo = vs%basin%ggeo/frac
        if (associated(vs%basin%tbas)) vs%basin%tbas = vs%basin%tbas/frac
        do j = 1, shd%lc%IGND
            if (associated(vs%basin%thlq)) vs%basin%thlq(:, j) = vs%basin%thlq(:, j)/frac
            if (associated(vs%basin%thic)) vs%basin%thic(:, j) = vs%basin%thic(:, j)/frac
            if (associated(vs%basin%lqws)) vs%basin%lqws(:, j) = vs%basin%lqws(:, j)/frac
            if (associated(vs%basin%fzws)) vs%basin%fzws(:, j) = vs%basin%fzws(:, j)/frac
            if (associated(vs%basin%tbar)) vs%basin%tbar(:, j) = vs%basin%tbar(:, j)/frac
            if (associated(vs%basin%gflx)) vs%basin%gflx(:, j) = vs%basin%gflx(:, j)/frac
            if (associated(vs%basin%rofs)) vs%basin%rofs(:, j) = vs%basin%rofs(:, j)/frac
            if (associated(vs%basin%delzw)) vs%basin%delzw(:, j) = vs%basin%delzw(:, j)/frac
            if (associated(vs%basin%zbotw)) vs%basin%zbotw(:, j) = vs%basin%zbotw(:, j)/frac
        end do
        if (associated(vs%basin%rofb)) vs%basin%rofb = vs%basin%rofb/frac
        if (associated(vs%basin%rchg)) vs%basin%rchg = vs%basin%rchg/frac
        if (associated(vs%basin%lzs)) vs%basin%lzs = vs%basin%lzs/frac
        if (associated(vs%basin%dzs)) vs%basin%dzs = vs%basin%dzs/frac

    end subroutine

    subroutine run_between_grid_finalize(fls, shd, cm)

        !> Process modules.
        use WF_ROUTE_config
        use rte_module

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Call processes.
        call WF_ROUTE_finalize(fls, shd)
        call run_rte_finalize(fls, shd)

    end subroutine

end module
