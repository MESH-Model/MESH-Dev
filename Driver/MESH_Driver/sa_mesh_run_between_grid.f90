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
        vs%grid%rff = (vs%grid%rofo + vs%grid%rofs)*ic%dts
        vs%grid%rchg = vs%grid%rofb*ic%dts

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

        if (mod(ic%ts_hourly*ic%dts, RTE_TS) == 0) then

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

    subroutine run_within_grid_stas_basin_update(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer j, ii, i
        real frac(shd%NA), albtfrac(shd%NA), tpndfrac(shd%NA), tsnofrac(shd%NA), tcanfrac(shd%NA)

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Initialize variables.
        vs%basin%fsin = vs%grid%fsin*shd%FRAC
        vs%basin%fsdr = vs%grid%fsdr*shd%FRAC
        vs%basin%fsdff = vs%grid%fsdff*shd%FRAC
        vs%basin%flin = vs%grid%flin*shd%FRAC
        vs%basin%ta = vs%grid%ta*shd%FRAC
        vs%basin%qa = vs%grid%qa*shd%FRAC
        vs%basin%pres = vs%grid%pres*shd%FRAC
        vs%basin%uv = vs%grid%uv*shd%FRAC
        vs%basin%wdir = vs%grid%wdir*shd%FRAC
        vs%basin%uu = vs%grid%uu*shd%FRAC
        vs%basin%vv = vs%grid%vv*shd%FRAC
        vs%basin%pre = vs%grid%pre*shd%FRAC
        vs%basin%prern = vs%grid%prern*shd%FRAC
        vs%basin%presno = vs%grid%presno*shd%FRAC
        vs%basin%rcan = vs%grid%rcan*shd%FRAC
        vs%basin%sncan = vs%grid%sncan*shd%FRAC
        vs%basin%cmas = vs%grid%cmas*shd%FRAC
        vs%basin%tac = vs%grid%tac*shd%FRAC
        vs%basin%tcan = vs%grid%tcan*shd%FRAC
        vs%basin%qac = vs%grid%qac*shd%FRAC
        vs%basin%gro = vs%grid%gro*shd%FRAC
        vs%basin%sno = vs%grid%sno*shd%FRAC
        vs%basin%fsno = vs%grid%fsno*shd%FRAC
        vs%basin%rofsno = vs%grid%rofsno*shd%FRAC
        vs%basin%albs = vs%grid%albs*shd%FRAC
        vs%basin%rhos = vs%grid%rhos*shd%FRAC
        vs%basin%wsno = vs%grid%wsno*shd%FRAC
        vs%basin%tsno = vs%grid%tsno*shd%FRAC
        vs%basin%albt = vs%grid%albt*shd%FRAC
        vs%basin%alvs = vs%grid%alvs*shd%FRAC
        vs%basin%alir = vs%grid%alir*shd%FRAC
        vs%basin%gte = vs%grid%gte*shd%FRAC
        vs%basin%zpnd = vs%grid%zpnd*shd%FRAC
        vs%basin%pndw = vs%grid%pndw*shd%FRAC
        vs%basin%tpnd = vs%grid%tpnd*shd%FRAC
        vs%basin%fstr = vs%grid%fstr*shd%FRAC
        vs%basin%pevp = vs%grid%pevp*shd%FRAC
        vs%basin%evap = vs%grid%evap*shd%FRAC
        vs%basin%evpb = vs%grid%evpb*shd%FRAC
        vs%basin%arrd = vs%grid%arrd*shd%FRAC
        vs%basin%rofo = vs%grid%rofo*shd%FRAC
        vs%basin%qevp = vs%grid%qevp*shd%FRAC
        vs%basin%hfs = vs%grid%hfs*shd%FRAC
        vs%basin%gzero = vs%grid%gzero*shd%FRAC
        do j = 1, 4
            vs%basin%tsfs(:, j) = vs%grid%tsfs(:, j)*shd%FRAC
        end do
        vs%basin%ggeo = vs%grid%ggeo*shd%FRAC
        vs%basin%rofs = vs%grid%rofs*shd%FRAC
        vs%basin%tbas = vs%grid%tbas*shd%FRAC
        do j = 1, shd%lc%IGND
            vs%basin%thic(:, j) = vs%grid%thic(:, j)*shd%FRAC
            vs%basin%fzws(:, j) = vs%grid%fzws(:, j)*shd%FRAC
            vs%basin%thlq(:, j) = vs%grid%thlq(:, j)*shd%FRAC
            vs%basin%lqws(:, j) = vs%grid%lqws(:, j)*shd%FRAC
            vs%basin%tbar(:, j) = vs%grid%tbar(:, j)*shd%FRAC
            vs%basin%gflx(:, j) = vs%grid%gflx(:, j)*shd%FRAC
        end do
        vs%basin%lzs = vs%grid%lzs*shd%FRAC
        vs%basin%dzs = vs%grid%dzs*shd%FRAC
        vs%basin%rofb = vs%grid%rofb*shd%FRAC
        vs%basin%stgw = vs%grid%stgw*shd%FRAC
        vs%basin%stge = vs%grid%stge*shd%FRAC
        frac = shd%FRAC
        where (vs%basin%albt > 0.0)
            albtfrac = shd%FRAC
        elsewhere
            albtfrac = 0.0
        end where
        where (vs%basin%tpnd > 0.0)
            tpndfrac = shd%FRAC
        elsewhere
            tpndfrac = 0.0
        end where
        where (vs%basin%tsno > 0.0)
            tsnofrac = shd%FRAC
        elsewhere
            tsnofrac = 0.0
        end where
        where (vs%basin%tcan > 0.0)
            tcanfrac = shd%FRAC
        elsewhere
            tcanfrac = 0.0
        end where

        !> Update variables.
        do i = 1, shd%NAA
            ii = shd%NEXT(i)
            if (ii > 0) then
                vs%basin%fsin(ii) = vs%basin%fsin(ii) + vs%basin%fsin(i)
                vs%basin%fsdr(ii) = vs%basin%fsdr(ii) + vs%basin%fsdr(i)
                vs%basin%fsdff(ii) = vs%basin%fsdff(ii) + vs%basin%fsdff(i)
                vs%basin%flin(ii) = vs%basin%flin(ii) + vs%basin%flin(i)
                vs%basin%ta(ii) = vs%basin%ta(ii) + vs%basin%ta(i)
                vs%basin%qa(ii) = vs%basin%qa(ii) + vs%basin%qa(i)
                vs%basin%pres(ii) = vs%basin%pres(ii) + vs%basin%pres(i)
                vs%basin%uv(ii) = vs%basin%uv(ii) + vs%basin%uv(i)
                vs%basin%wdir(ii) = vs%basin%wdir(ii) + vs%basin%wdir(i)
                vs%basin%uu(ii) = vs%basin%uu(ii) + vs%basin%uu(i)
                vs%basin%vv(ii) = vs%basin%vv(ii) + vs%basin%vv(i)
                vs%basin%pre(ii) = vs%basin%pre(ii) + vs%basin%pre(i)
                vs%basin%prern(ii) = vs%basin%prern(ii) + vs%basin%prern(i)
                vs%basin%presno(ii) = vs%basin%presno(ii) + vs%basin%presno(i)
                vs%basin%rcan(ii) = vs%basin%rcan(ii) + vs%basin%rcan(i)
                vs%basin%sncan(ii) = vs%basin%sncan(ii) + vs%basin%sncan(i)
                vs%basin%cmas(ii) = vs%basin%cmas(ii) + vs%basin%cmas(i)
                vs%basin%tac(ii) = vs%basin%tac(ii) + vs%basin%tac(i)
                vs%basin%tcan(ii) = vs%basin%tcan(ii) + vs%basin%tcan(i)
                vs%basin%qac(ii) = vs%basin%qac(ii) + vs%basin%qac(i)
                vs%basin%gro(ii) = vs%basin%gro(ii) + vs%basin%gro(i)
                vs%basin%sno(ii) = vs%basin%sno(ii) + vs%basin%sno(i)
                vs%basin%fsno(ii) = vs%basin%fsno(ii) + vs%basin%fsno(i)
                vs%basin%rofsno(ii) = vs%basin%rofsno(ii) + vs%basin%rofsno(i)
                vs%basin%albs(ii) = vs%basin%albs(ii) + vs%basin%albs(i)
                vs%basin%rhos(ii) = vs%basin%rhos(ii) + vs%basin%rhos(i)
                vs%basin%wsno(ii) = vs%basin%wsno(ii) + vs%basin%wsno(i)
                vs%basin%tsno(ii) = vs%basin%tsno(ii) + vs%basin%tsno(i)
                vs%basin%albt(ii) = vs%basin%albt(ii) + vs%basin%albt(i)
                vs%basin%alvs(ii) = vs%basin%alvs(ii) + vs%basin%alvs(i)
                vs%basin%alir(ii) = vs%basin%alir(ii) + vs%basin%alir(i)
                vs%basin%gte(ii) = vs%basin%gte(ii) + vs%basin%gte(i)
                vs%basin%zpnd(ii) = vs%basin%zpnd(ii) + vs%basin%zpnd(i)
                vs%basin%pndw(ii) = vs%basin%pndw(ii) + vs%basin%pndw(i)
                vs%basin%tpnd(ii) = vs%basin%tpnd(ii) + vs%basin%tpnd(i)
                vs%basin%fstr(ii) = vs%basin%fstr(ii) + vs%basin%fstr(i)
                vs%basin%pevp(ii) = vs%basin%pevp(ii) + vs%basin%pevp(i)
                vs%basin%evap(ii) = vs%basin%evap(ii) + vs%basin%evap(i)
                vs%basin%evpb(ii) = vs%basin%evpb(ii) + vs%basin%evpb(i)
                vs%basin%arrd(ii) = vs%basin%arrd(ii) + vs%basin%arrd(i)
                vs%basin%rofo(ii) = vs%basin%rofo(ii) + vs%basin%rofo(i)
                vs%basin%qevp(ii) = vs%basin%qevp(ii) + vs%basin%qevp(i)
                vs%basin%hfs(ii) = vs%basin%hfs(ii) + vs%basin%hfs(i)
                vs%basin%gzero(ii) = vs%basin%gzero(ii) + vs%basin%gzero(i)
                vs%basin%tsfs(ii, :) = vs%basin%tsfs(ii, :) + vs%basin%tsfs(ii, :)
                vs%basin%ggeo(ii) = vs%basin%ggeo(ii) + vs%basin%ggeo(i)
                vs%basin%rofs(ii) = vs%basin%rofs(ii) + vs%basin%rofs(i)
                vs%basin%tbas(ii) = vs%basin%tbas(ii) + vs%basin%tbas(i)
                vs%basin%thic(ii, :) = vs%basin%thic(ii, :) + vs%basin%thic(i, :)
                vs%basin%fzws(ii, :) = vs%basin%fzws(ii, :) + vs%basin%fzws(i, :)
                vs%basin%thlq(ii, :) = vs%basin%thlq(ii, :) + vs%basin%thlq(i, :)
                vs%basin%lqws(ii, :) = vs%basin%lqws(ii, :) + vs%basin%lqws(i, :)
                vs%basin%tbar(ii, :) = vs%basin%tbar(ii, :) + vs%basin%tbar(i, :)
                vs%basin%gflx(ii, :) = vs%basin%gflx(ii, :) + vs%basin%gflx(i, :)
                vs%basin%lzs(ii) = vs%basin%lzs(ii) + vs%basin%lzs(i)
                vs%basin%dzs(ii) = vs%basin%dzs(ii) + vs%basin%dzs(i)
                vs%basin%rofb(ii) = vs%basin%rofb(ii) + vs%basin%rofb(i)
                vs%basin%stgw(ii) = vs%basin%stgw(ii) + vs%basin%stgw(i)
                vs%basin%stge(ii) = vs%basin%stge(ii) + vs%basin%stge(i)
                frac(ii) = frac(ii) + frac(i)
                if (vs%basin%albt(i) > 0.0) albtfrac(ii) = albtfrac(ii) + albtfrac(i)
                if (vs%basin%tpnd(i) > 0.0) tpndfrac(ii) = tpndfrac(ii) + tpndfrac(i)
                if (vs%basin%tsno(i) > 0.0) tsnofrac(ii) = tsnofrac(ii) + tsnofrac(i)
                if (vs%basin%tcan(i) > 0.0) tcanfrac(ii) = tcanfrac(ii) + tcanfrac(i)
            end if
        end do

        !> DA average.
        where (frac > 0.0)
            vs%basin%fsin = vs%basin%fsin/frac
            vs%basin%fsdr = vs%basin%fsdr/frac
            vs%basin%fsdff = vs%basin%fsdff/frac
            vs%basin%flin = vs%basin%flin/frac
            vs%basin%ta = vs%basin%ta/frac
            vs%basin%qa = vs%basin%qa/frac
            vs%basin%pres = vs%basin%pres/frac
            vs%basin%uv = vs%basin%uv/frac
            vs%basin%wdir = vs%basin%wdir/frac
            vs%basin%uu = vs%basin%uu/frac
            vs%basin%vv = vs%basin%vv/frac
            vs%basin%pre = vs%basin%pre/frac
            vs%basin%prern = vs%basin%prern/frac
            vs%basin%presno = vs%basin%presno/frac
            vs%basin%rcan = vs%basin%rcan/frac
            vs%basin%sncan = vs%basin%sncan/frac
            where (tcanfrac > 0.0)
                vs%basin%cmas = vs%basin%cmas/tcanfrac
                vs%basin%tac = vs%basin%tac/tcanfrac
                vs%basin%tcan = vs%basin%tcan/tcanfrac
                vs%basin%qac = vs%basin%qac/tcanfrac
                vs%basin%gro = vs%basin%gro/tcanfrac
            end where
            vs%basin%sno = vs%basin%sno/frac
            vs%basin%fsno = vs%basin%fsno/frac
            vs%basin%rofsno = vs%basin%rofsno/frac
            vs%basin%wsno = vs%basin%wsno/frac
            where (tsnofrac > 0.0)
                vs%basin%albs = vs%basin%albs/tsnofrac
                vs%basin%rhos = vs%basin%rhos/tsnofrac
                vs%basin%tsno = vs%basin%tsno/tsnofrac
            end where
            where (albtfrac > 0.0)
                vs%basin%albt = vs%basin%albt/albtfrac
                vs%basin%alvs = vs%basin%alvs/albtfrac
                vs%basin%alir = vs%basin%alir/albtfrac
            end where
            vs%basin%gte = vs%basin%gte/frac
            vs%basin%zpnd = vs%basin%zpnd/frac
            vs%basin%pndw = vs%basin%pndw/frac
            where (tpndfrac > 0.0)
                vs%basin%tpnd = vs%basin%tpnd/tpndfrac
                vs%basin%fstr = vs%basin%fstr/tpndfrac
            end where
            vs%basin%pevp = vs%basin%pevp/frac
            vs%basin%evap = vs%basin%evap/frac
            vs%basin%evpb = vs%basin%evpb/frac
            vs%basin%arrd = vs%basin%arrd/frac
            vs%basin%rofo = vs%basin%rofo/frac
            vs%basin%qevp = vs%basin%qevp/frac
            vs%basin%hfs = vs%basin%hfs/frac
            vs%basin%gzero = vs%basin%gzero/frac
            vs%basin%ggeo = vs%basin%ggeo/frac
            vs%basin%rofs = vs%basin%rofs/frac
            vs%basin%tbas = vs%basin%tbas/frac
            vs%basin%lzs = vs%basin%lzs/frac
            vs%basin%dzs = vs%basin%dzs/frac
            vs%basin%rofb = vs%basin%rofb/frac
            vs%basin%stgw = vs%basin%stgw/frac
            vs%basin%stge = vs%basin%stge/frac
        end where
        do j = 1, 4
            where (frac > 0.0)
                vs%basin%tsfs(:, j) = vs%basin%tsfs(:, j)/frac
            end where
        end do
        do j = 1, shd%lc%IGND
            where (frac > 0.0)
                vs%basin%thic(:, j) = vs%basin%thic(:, j)/frac
                vs%basin%fzws(:, j) = vs%basin%fzws(:, j)/frac
                vs%basin%thlq(:, j) = vs%basin%thlq(:, j)/frac
                vs%basin%lqws(:, j) = vs%basin%lqws(:, j)/frac
                vs%basin%tbar(:, j) = vs%basin%tbar(:, j)/frac
                vs%basin%gflx(:, j) = vs%basin%gflx(:, j)/frac
            end where
        end do

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
