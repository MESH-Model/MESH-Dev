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

    contains

    subroutine run_between_grid_init(fls, shd, cm)

        !> Process modules.
!-        use SA_RTE_module
        use WF_ROUTE_config
        use rte_module
        use cropland_irrigation_between_grid

!temp: Outputs.
!-        use save_basin_output, only: STREAMFLOWOUTFLAG, REACHOUTFLAG
!-        use FLAGS
!-        use strings

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
!-        integer, parameter :: MaxLenField = 20, MaxArgs = 20, MaxLenLine = 100
!-        integer NA
!-        integer NS, NR
!-        character(len = 4) ffmti
!-        character(len = 500) fn
!-        integer iun, ierr, l, j, i
!-        character(MaxLenField), dimension(MaxArgs) :: out_args
!-        integer nargs

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

        !> Update basin variables.
        call run_within_grid_stas_basin_update(fls, shd, cm)

!-1010    format(9999(g15.7e2, ','))

    end subroutine

    subroutine run_between_grid(fls, shd, cm)

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
        type(clim_info) cm

        !> Local variables.
!-        integer k, ki
        integer ierr

        !> Local variables.
!-        integer l, i, iun

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
        if (ro%RUNLSS) then
            vs%grid%rff = (vs%grid%ovrflw + sum(vs%grid%latflw, 2))*ic%dts
            vs%grid%rchg = vs%grid%drainsol*ic%dts
        end if

        !> Call processes.
!-        call SA_RTE(shd)
        call WF_ROUTE_between_grid(fls, shd)
        call run_rte_between_grid(fls, shd)
        call runci_between_grid(shd, fls, cm)

        !> Update basin variables.
        call run_within_grid_stas_basin_update(fls, shd, cm)

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

    subroutine run_within_grid_stas_basin_update(fls, shd, cm)

        !> Input/output variables.
        type(fl_ids) fls
        type(ShedGridParams) shd
        type(clim_info) cm

        !> Local variables.
        integer j, ii, i
        real albtfrac(shd%NA), tpndfrac(shd%NA), tsnofrac(shd%NA), tcanfrac(shd%NA), ticefrac(shd%NA), frac(shd%NA)

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Meteorology/climatology variables.
        if (associated(vs%basin%fsin) .and. associated(vs%grid%fsin)) vs%basin%fsin = vs%grid%fsin*shd%FRAC
        if (associated(vs%basin%fsvs) .and. associated(vs%grid%fsvs)) vs%basin%fsvs = vs%grid%fsvs*shd%FRAC
        if (associated(vs%basin%fsir) .and. associated(vs%grid%fsir)) vs%basin%fsir = vs%grid%fsir*shd%FRAC
        if (associated(vs%basin%fsdr) .and. associated(vs%grid%fsdr)) vs%basin%fsdr = vs%grid%fsdr*shd%FRAC
        if (associated(vs%basin%fsdff) .and. associated(vs%grid%fsdff)) vs%basin%fsdff = vs%grid%fsdff*shd%FRAC
        if (associated(vs%basin%flin) .and. associated(vs%grid%flin)) vs%basin%flin = vs%grid%flin*shd%FRAC
        if (associated(vs%basin%ta) .and. associated(vs%grid%ta)) vs%basin%ta = vs%grid%ta*shd%FRAC
        if (associated(vs%basin%qa) .and. associated(vs%grid%qa)) vs%basin%qa = vs%grid%qa*shd%FRAC
        if (associated(vs%basin%pres) .and. associated(vs%grid%pres)) vs%basin%pres = vs%grid%pres*shd%FRAC
        if (associated(vs%basin%uu) .and. associated(vs%grid%uu)) vs%basin%uu = vs%grid%uu*shd%FRAC
        if (associated(vs%basin%vv) .and. associated(vs%grid%vv)) vs%basin%vv = vs%grid%vv*shd%FRAC
        if (associated(vs%basin%uv) .and. associated(vs%grid%uv)) vs%basin%uv = vs%grid%uv*shd%FRAC
        if (associated(vs%basin%wdir) .and. associated(vs%grid%wdir)) vs%basin%wdir = vs%grid%wdir*shd%FRAC
        if (associated(vs%basin%prern) .and. associated(vs%grid%prern)) vs%basin%prern = vs%grid%prern*shd%FRAC
        if (associated(vs%basin%presno) .and. associated(vs%grid%presno)) vs%basin%presno = vs%grid%presno*shd%FRAC
        if (associated(vs%basin%pre) .and. associated(vs%grid%pre)) vs%basin%pre = vs%grid%pre*shd%FRAC

        !> Canopy variables.
        if (associated(vs%basin%lqwscan) .and. associated(vs%grid%lqwscan)) vs%basin%lqwscan = vs%grid%lqwscan*shd%FRAC
        if (associated(vs%basin%fzwscan) .and. associated(vs%grid%fzwscan)) vs%basin%fzwscan = vs%grid%fzwscan*shd%FRAC
        if (associated(vs%basin%cmas) .and. associated(vs%grid%cmas)) vs%basin%cmas = vs%grid%cmas*shd%FRAC
        if (associated(vs%basin%tacan) .and. associated(vs%grid%tacan)) vs%basin%tacan = vs%grid%tacan*shd%FRAC
        if (associated(vs%basin%qacan) .and. associated(vs%grid%qacan)) vs%basin%qacan = vs%grid%qacan*shd%FRAC
        if (associated(vs%basin%tcan) .and. associated(vs%grid%tcan)) then
            vs%basin%tcan = vs%grid%tcan*shd%FRAC
            where (vs%basin%tcan > 0.0)
                tcanfrac = shd%FRAC
            elsewhere
                tcanfrac = 0.0
            end where
        end if
        if (associated(vs%basin%gro) .and. associated(vs%grid%gro)) vs%basin%gro = vs%grid%gro*shd%FRAC

        !> Snow variables.
        if (associated(vs%basin%fsno) .and. associated(vs%grid%fsno)) vs%basin%fsno = vs%grid%fsno*shd%FRAC
        if (associated(vs%basin%sno) .and. associated(vs%grid%sno)) vs%basin%sno = vs%grid%sno*shd%FRAC
        if (associated(vs%basin%rhosno) .and. associated(vs%grid%rhosno)) vs%basin%rhosno = vs%grid%rhosno*shd%FRAC
!-        if (associated(vs%basin%zsno) .and. associated(vs%grid%zsno)) vs%basin%zsno = vs%grid%zsno*shd%FRAC
        if (associated(vs%basin%lqwssno) .and. associated(vs%grid%lqwssno)) vs%basin%lqwssno = vs%grid%lqwssno*shd%FRAC
        if (associated(vs%basin%tsno) .and. associated(vs%grid%tsno)) then
            vs%basin%tsno = vs%grid%tsno*shd%FRAC
            where (vs%basin%tsno > 0.0)
                tsnofrac = shd%FRAC
            elsewhere
                tsnofrac = 0.0
            end where
        end if
        if (associated(vs%basin%albsno) .and. associated(vs%grid%albsno)) vs%basin%albsno = vs%grid%albsno*shd%FRAC
        if (associated(vs%basin%drainsno) .and. associated(vs%grid%drainsno)) vs%basin%drainsno = vs%grid%drainsno*shd%FRAC

        !> Surface variables.
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
!-        if (associated(vs%basin%lqwspnd) .and. associated(vs%grid%lqwspnd)) vs%basin%lqwspnd = vs%grid%lqwspnd*shd%FRAC
        if (associated(vs%basin%tpnd) .and. associated(vs%grid%tpnd)) then
            vs%basin%tpnd = vs%grid%tpnd*shd%FRAC
            where (vs%basin%tpnd > 0.0)
                tpndfrac = shd%FRAC
            elsewhere
                tpndfrac = 0.0
            end where
        end if
        if (associated(vs%basin%pndcaf) .and. associated(vs%grid%pndcaf)) vs%basin%pndcaf = vs%grid%pndcaf*shd%FRAC
        if (associated(vs%basin%potevp) .and. associated(vs%grid%potevp)) vs%basin%potevp = vs%grid%potevp*shd%FRAC
        if (associated(vs%basin%et) .and. associated(vs%grid%et)) vs%basin%et = vs%grid%et*shd%FRAC
!-        if (associated(vs%basin%evpb) .and. associated(vs%grid%evpb)) vs%basin%evpb = vs%grid%evpb*shd%FRAC
!-        if (associated(vs%basin%arrd) .and. associated(vs%grid%arrd)) vs%basin%arrd = vs%grid%arrd*shd%FRAC
        if (associated(vs%basin%ovrflw) .and. associated(vs%grid%ovrflw)) vs%basin%ovrflw = vs%grid%ovrflw*shd%FRAC
        if (associated(vs%basin%qevp) .and. associated(vs%grid%qevp)) vs%basin%qevp = vs%grid%qevp*shd%FRAC
        if (associated(vs%basin%qsens) .and. associated(vs%grid%qsens)) vs%basin%qsens = vs%grid%qsens*shd%FRAC
        if (associated(vs%basin%gzero) .and. associated(vs%grid%gzero)) vs%basin%gzero = vs%grid%gzero*shd%FRAC
        do j = 1, 4
            if (associated(vs%basin%tsfs) .and. associated(vs%grid%tsfs)) vs%basin%tsfs(:, j) = vs%grid%tsfs(:, j)*shd%FRAC
        end do
        if (associated(vs%basin%tsurf) .and. associated(vs%grid%tsurf)) vs%basin%tsurf = vs%grid%tsurf*shd%FRAC

        !> Ice/glacier variables.
        if (associated(vs%basin%lqwsice) .and. associated(vs%grid%lqwsice)) vs%basin%lqwsice = vs%grid%lqwsice*shd%FRAC
        if (associated(vs%basin%tice) .and. associated(vs%grid%tice)) then
            vs%basin%tice = vs%grid%tice*shd%FRAC
            where (vs%basin%tice > 0.0)
                ticefrac = shd%FRAC
            elsewhere
                ticefrac = 0.0
            end where
        end if

        !> Subsurface/soil variables.
        do j = 1, shd%lc%IGND
            if (associated(vs%basin%dzsol) .and. associated(vs%grid%dzsol)) vs%basin%dzsol(:, j) = vs%grid%dzsol(:, j)*shd%FRAC
            if (associated(vs%basin%dzsolhyd) .and. associated(vs%grid%dzsolhyd)) then
                vs%basin%dzsolhyd(:, j) = vs%grid%dzsolhyd(:, j)*shd%FRAC
            end if
            if (associated(vs%basin%thlqsol) .and. associated(vs%grid%thlqsol)) then
                vs%basin%thlqsol(:, j) = vs%grid%thlqsol(:, j)*shd%FRAC
            end if
            if (associated(vs%basin%thicsol) .and. associated(vs%grid%thicsol)) then
                vs%basin%thicsol(:, j) = vs%grid%thicsol(:, j)*shd%FRAC
            end if
!-            if (associated(vs%basin%lqwssol) .and. associated(vs%grid%lqwssol)) then
!-                vs%basin%lqwssol(:, j) = vs%grid%lqwssol(:, j)*shd%FRAC
!-            end if
!-            if (associated(vs%basin%fzwssol) .and. associated(vs%grid%fzwssol)) then
!-                vs%basin%fzwssol(:, j) = vs%grid%fzwssol(:, j)*shd%FRAC
!-            end if
            if (associated(vs%basin%tsol) .and. associated(vs%grid%tsol)) vs%basin%tsol(:, j) = vs%grid%tsol(:, j)*shd%FRAC
            if (associated(vs%basin%gflx) .and. associated(vs%grid%gflx)) vs%basin%gflx(:, j) = vs%grid%gflx(:, j)*shd%FRAC
            if (associated(vs%basin%latflw) .and. associated(vs%grid%latflw)) vs%basin%latflw(:, j) = vs%grid%latflw(:, j)*shd%FRAC
        end do
        if (associated(vs%basin%zsol) .and. associated(vs%grid%zsol)) vs%basin%zsol = vs%grid%zsol*shd%FRAC
        if (associated(vs%basin%zsolhyd) .and. associated(vs%grid%zsolhyd)) vs%basin%zsolhyd = vs%grid%zsolhyd*shd%FRAC
        if (associated(vs%basin%zsolsat) .and. associated(vs%grid%zsolsat)) vs%basin%zsolsat = vs%grid%zsolsat*shd%FRAC
        if (associated(vs%basin%ggeo) .and. associated(vs%grid%ggeo)) vs%basin%ggeo = vs%grid%ggeo*shd%FRAC
        if (associated(vs%basin%tbas) .and. associated(vs%grid%tbas)) vs%basin%tbas = vs%grid%tbas*shd%FRAC
        if (associated(vs%basin%drainsol) .and. associated(vs%grid%drainsol)) vs%basin%drainsol = vs%grid%drainsol*shd%FRAC

        !> Groundwater/lower zone storage variables.
        if (associated(vs%basin%rchg) .and. associated(vs%grid%rchg)) vs%basin%rchg = vs%grid%rchg*shd%FRAC
        if (associated(vs%basin%stggw) .and. associated(vs%grid%stggw)) vs%basin%stggw = vs%grid%stggw*shd%FRAC
        if (associated(vs%basin%lkg) .and. associated(vs%grid%lkg)) vs%basin%lkg = vs%grid%lkg*shd%FRAC
!-        if (associated(vs%basin%dzs) .and. associated(vs%grid%dzs)) vs%basin%dzs = vs%grid%dzs*shd%FRAC

        !> Routing variables.
        if (associated(vs%basin%rff) .and. associated(vs%grid%rff)) vs%basin%rff = vs%grid%rff*shd%FRAC
!        if (associated(vs%basin%qi) .and. associated(vs%grid%qi)) vs%basin%qi = vs%grid%qi*shd%FRAC
!        if (associated(vs%basin%qo) .and. associated(vs%grid%qo)) vs%basin%qo = vs%grid%qo*shd%FRAC
!        if (associated(vs%basin%stgch) .and. associated(vs%grid%stgch)) vs%basin%stgch = vs%grid%stgch*shd%FRAC
!        if (associated(vs%basin%zlvl) .and. associated(vs%grid%zlvl)) vs%basin%zlvl = vs%grid%zlvl*shd%FRAC
!        if (associated(vs%basin%div) .and. associated(vs%grid%div)) vs%basin%div = vs%grid%div*shd%FRAC
!        if (associated(vs%basin%abstr) .and. associated(vs%grid%abstr)) vs%basin%abstr = vs%grid%abstr*shd%FRAC

        !> Relative area fraction.
        frac = shd%FRAC

        !> Accumulate basin values.
        do i = 1, shd%NAA
            ii = shd%NEXT(i)
            if (ii > 0) then

                !> Meteorology/climatology variables.
                if (associated(vs%basin%fsin)) vs%basin%fsin(ii) = vs%basin%fsin(ii) + vs%basin%fsin(i)
                if (associated(vs%basin%fsvs)) vs%basin%fsvs(ii) = vs%basin%fsvs(ii) + vs%basin%fsvs(i)
                if (associated(vs%basin%fsir)) vs%basin%fsir(ii) = vs%basin%fsir(ii) + vs%basin%fsir(i)
                if (associated(vs%basin%fsdr)) vs%basin%fsdr(ii) = vs%basin%fsdr(ii) + vs%basin%fsdr(i)
                if (associated(vs%basin%fsdff)) vs%basin%fsdff(ii) = vs%basin%fsdff(ii) + vs%basin%fsdff(i)
                if (associated(vs%basin%flin)) vs%basin%flin(ii) = vs%basin%flin(ii) + vs%basin%flin(i)
                if (associated(vs%basin%ta)) vs%basin%ta(ii) = vs%basin%ta(ii) + vs%basin%ta(i)
                if (associated(vs%basin%qa)) vs%basin%qa(ii) = vs%basin%qa(ii) + vs%basin%qa(i)
                if (associated(vs%basin%pres)) vs%basin%pres(ii) = vs%basin%pres(ii) + vs%basin%pres(i)
                if (associated(vs%basin%uu)) vs%basin%uu(ii) = vs%basin%uu(ii) + vs%basin%uu(i)
                if (associated(vs%basin%vv)) vs%basin%vv(ii) = vs%basin%vv(ii) + vs%basin%vv(i)
                if (associated(vs%basin%uv)) vs%basin%uv(ii) = vs%basin%uv(ii) + vs%basin%uv(i)
                if (associated(vs%basin%wdir)) vs%basin%wdir(ii) = vs%basin%wdir(ii) + vs%basin%wdir(i)
                if (associated(vs%basin%prern)) vs%basin%prern(ii) = vs%basin%prern(ii) + vs%basin%prern(i)
                if (associated(vs%basin%presno)) vs%basin%presno(ii) = vs%basin%presno(ii) + vs%basin%presno(i)
                if (associated(vs%basin%pre)) vs%basin%pre(ii) = vs%basin%pre(ii) + vs%basin%pre(i)

                !> Canopy variables.
                if (associated(vs%basin%lqwscan)) vs%basin%lqwscan(ii) = vs%basin%lqwscan(ii) + vs%basin%lqwscan(i)
                if (associated(vs%basin%fzwscan)) vs%basin%fzwscan(ii) = vs%basin%fzwscan(ii) + vs%basin%fzwscan(i)
                if (associated(vs%basin%cmas)) vs%basin%cmas(ii) = vs%basin%cmas(ii) + vs%basin%cmas(i)
                if (associated(vs%basin%tacan)) vs%basin%tacan(ii) = vs%basin%tacan(ii) + vs%basin%tacan(i)
                if (associated(vs%basin%qacan)) vs%basin%qacan(ii) = vs%basin%qacan(ii) + vs%basin%qacan(i)
                if (associated(vs%basin%tcan)) then
                    vs%basin%tcan(ii) = vs%basin%tcan(ii) + vs%basin%tcan(i)
                    if (vs%basin%tcan(i) > 0.0) then
                        tcanfrac(ii) = tcanfrac(ii) + tcanfrac(i)
                    end if
                end if
                if (associated(vs%basin%gro)) vs%basin%gro(ii) = vs%basin%gro(ii) + vs%basin%gro(i)

                !> Snow variables.
                if (associated(vs%basin%fsno)) vs%basin%fsno(ii) = vs%basin%fsno(ii) + vs%basin%fsno(i)
                if (associated(vs%basin%sno)) vs%basin%sno(ii) = vs%basin%sno(ii) + vs%basin%sno(i)
                if (associated(vs%basin%rhosno)) vs%basin%rhosno(ii) = vs%basin%rhosno(ii) + vs%basin%rhosno(i)
!-                if (associated(vs%basin%zsno)) vs%basin%zsno(ii) = vs%basin%zsno(ii) + vs%basin%zsno(i)
                if (associated(vs%basin%lqwssno)) vs%basin%lqwssno(ii) = vs%basin%lqwssno(ii) + vs%basin%lqwssno(i)
                if (associated(vs%basin%tsno)) then
                    vs%basin%tsno(ii) = vs%basin%tsno(ii) + vs%basin%tsno(i)
                    if (vs%basin%tsno(i) > 0.0) then
                        tsnofrac(ii) = tsnofrac(ii) + tsnofrac(i)
                    end if
                end if
                if (associated(vs%basin%albsno)) vs%basin%albsno(ii) = vs%basin%albsno(ii) + vs%basin%albsno(i)
                if (associated(vs%basin%drainsno)) vs%basin%drainsno(ii) = vs%basin%drainsno(ii) + vs%basin%drainsno(i)

                !> Surface variables.
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
!-                if (associated(vs%basin%lqwspnd)) vs%basin%lqwspnd(ii) = vs%basin%lqwspnd(ii) + vs%basin%lqwspnd(i)
                if (associated(vs%basin%tpnd)) then
                    vs%basin%tpnd(ii) = vs%basin%tpnd(ii) + vs%basin%tpnd(i)
                    if (vs%basin%tpnd(i) > 0.0) then
                        tpndfrac(ii) = tpndfrac(ii) + tpndfrac(i)
                    end if
                end if
                if (associated(vs%basin%pndcaf)) vs%basin%pndcaf(ii) = vs%basin%pndcaf(ii) + vs%basin%pndcaf(i)
                if (associated(vs%basin%potevp)) vs%basin%potevp(ii) = vs%basin%potevp(ii) + vs%basin%potevp(i)
                if (associated(vs%basin%et)) vs%basin%et(ii) = vs%basin%et(ii) + vs%basin%et(i)
!-                if (associated(vs%basin%evpb)) vs%basin%evpb(ii) = vs%basin%evpb(ii) + vs%basin%evpb(i)
!-                if (associated(vs%basin%arrd)) vs%basin%arrd(ii) = vs%basin%arrd(ii) + vs%basin%arrd(i)
                if (associated(vs%basin%ovrflw)) vs%basin%ovrflw(ii) = vs%basin%ovrflw(ii) + vs%basin%ovrflw(i)
                if (associated(vs%basin%qevp)) vs%basin%qevp(ii) = vs%basin%qevp(ii) + vs%basin%qevp(i)
                if (associated(vs%basin%qsens)) vs%basin%qsens(ii) = vs%basin%qsens(ii) + vs%basin%qsens(i)
                if (associated(vs%basin%gzero)) vs%basin%gzero(ii) = vs%basin%gzero(ii) + vs%basin%gzero(i)
                if (associated(vs%basin%tsfs)) vs%basin%tsfs(ii, :) = vs%basin%tsfs(ii, :) + vs%basin%tsfs(i, :)
                if (associated(vs%basin%tsurf)) vs%basin%tsurf(ii) = vs%basin%tsurf(ii) + vs%basin%tsurf(i)

                !> Ice/glacier variables.
                if (associated(vs%basin%lqwsice)) vs%basin%lqwsice(ii) = vs%basin%lqwsice(ii) + vs%basin%lqwsice(i)
                if (associated(vs%basin%tice)) then
                    vs%basin%tice(ii) = vs%basin%tice(ii) + vs%basin%tice(i)
                    if (vs%basin%tice(i) > 0.0) then
                        ticefrac(ii) = ticefrac(ii) + ticefrac(i)
                    end if
                end if

                !> Subsurface/soil variables.
                if (associated(vs%basin%dzsol)) vs%basin%dzsol(ii, :) = vs%basin%dzsol(ii, :) + vs%basin%dzsol(i, :)
                if (associated(vs%basin%dzsolhyd)) vs%basin%dzsolhyd(ii, :) = vs%basin%dzsolhyd(ii, :) + vs%basin%dzsolhyd(i, :)
                if (associated(vs%basin%thlqsol)) vs%basin%thlqsol(ii, :) = vs%basin%thlqsol(ii, :) + vs%basin%thlqsol(i, :)
                if (associated(vs%basin%thicsol)) vs%basin%thicsol(ii, :) = vs%basin%thicsol(ii, :) + vs%basin%thicsol(i, :)
!-                if (associated(vs%basin%lqwssol)) vs%basin%lqwssol(ii, :) = vs%basin%lqwssol(ii, :) + vs%basin%lqwssol(i, :)
!-                if (associated(vs%basin%fzwssol)) vs%basin%fzwssol(ii, :) = vs%basin%fzwssol(ii, :) + vs%basin%fzwssol(i, :)
                if (associated(vs%basin%tsol)) vs%basin%tsol(ii, :) = vs%basin%tsol(ii, :) + vs%basin%tsol(i, :)
                if (associated(vs%basin%gflx)) vs%basin%gflx(ii, :) = vs%basin%gflx(ii, :) + vs%basin%gflx(i, :)
                if (associated(vs%basin%latflw)) vs%basin%latflw(ii, :) = vs%basin%latflw(ii, :) + vs%basin%latflw(i, :)
                if (associated(vs%basin%zsol)) vs%basin%zsol(ii) = vs%basin%zsol(ii) + vs%basin%zsol(i)
                if (associated(vs%basin%zsolhyd)) vs%basin%zsolhyd(ii) = vs%basin%zsolhyd(ii) + vs%basin%zsolhyd(i)
                if (associated(vs%basin%zsolsat)) vs%basin%zsolsat(ii) = vs%basin%zsolsat(ii) + vs%basin%zsolsat(i)
                if (associated(vs%basin%ggeo)) vs%basin%ggeo(ii) = vs%basin%ggeo(ii) + vs%basin%ggeo(i)
                if (associated(vs%basin%tbas)) vs%basin%tbas(ii) = vs%basin%tbas(ii) + vs%basin%tbas(i)
                if (associated(vs%basin%drainsol)) vs%basin%drainsol(ii) = vs%basin%drainsol(ii) + vs%basin%drainsol(i)

                !> Groundwater/lower zone storage variables.
                if (associated(vs%basin%rchg)) vs%basin%rchg(ii) = vs%basin%rchg(ii) + vs%basin%rchg(i)
                if (associated(vs%basin%stggw)) vs%basin%stggw(ii) = vs%basin%stggw(ii) + vs%basin%stggw(i)
                if (associated(vs%basin%lkg)) vs%basin%lkg(ii) = vs%basin%lkg(ii) + vs%basin%lkg(i)
!-                if (associated(vs%basin%dzs)) vs%basin%dzs(ii) = vs%basin%dzs(ii) + vs%basin%dzs(i)

                !> Routing variables.
                if (associated(vs%basin%rff)) vs%basin%rff(ii) = vs%basin%rff(ii) + vs%basin%rff(i)
!                if (associated(vs%basin%qi)) vs%basin%qi(ii) = vs%basin%qi(ii) + vs%basin%qi(i)
!                if (associated(vs%basin%qo)) vs%basin%qo(ii) = vs%basin%qo(ii) + vs%basin%qo(i)
!                if (associated(vs%basin%stgch)) vs%basin%stgch(ii) = vs%basin%stgch(ii) + vs%basin%stgch(i)
!                if (associated(vs%basin%zlvl)) vs%basin%zlvl(ii) = vs%basin%zlvl(ii) + vs%basin%zlvl(i)
!                if (associated(vs%basin%div)) vs%basin%div(ii) = vs%basin%div(ii) + vs%basin%div(i)
!                if (associated(vs%basin%abstr)) vs%basin%abstr(ii) = vs%basin%abstr(ii) + vs%basin%abstr(i)

                !> Relative area fraction.
                frac(ii) = frac(ii) + frac(i)
            end if
        end do

        !> Check for division by zero.
        where (frac == 0.0) frac = 1.0

        !> Meteorology/climatology variables.
        if (associated(vs%basin%fsin)) vs%basin%fsin = vs%basin%fsin/frac
        if (associated(vs%basin%fsvs)) vs%basin%fsvs = vs%basin%fsvs/frac
        if (associated(vs%basin%fsir)) vs%basin%fsir = vs%basin%fsir/frac
        if (associated(vs%basin%fsdr)) vs%basin%fsdr = vs%basin%fsdr/frac
        if (associated(vs%basin%fsdff)) vs%basin%fsdff = vs%basin%fsdff/frac
        if (associated(vs%basin%flin)) vs%basin%flin = vs%basin%flin/frac
        if (associated(vs%basin%ta)) vs%basin%ta = vs%basin%ta/frac
        if (associated(vs%basin%qa)) vs%basin%qa = vs%basin%qa/frac
        if (associated(vs%basin%pres)) vs%basin%pres = vs%basin%pres/frac
        if (associated(vs%basin%uu)) vs%basin%uu = vs%basin%uu/frac
        if (associated(vs%basin%vv)) vs%basin%vv = vs%basin%vv/frac
        if (associated(vs%basin%uv)) vs%basin%uv = vs%basin%uv/frac
        if (associated(vs%basin%wdir)) vs%basin%wdir = vs%basin%wdir/frac
        if (associated(vs%basin%prern)) vs%basin%prern = vs%basin%prern/frac
        if (associated(vs%basin%presno)) vs%basin%presno = vs%basin%presno/frac
        if (associated(vs%basin%pre)) vs%basin%pre = vs%basin%pre/frac

        !> Canopy variables.
        if (associated(vs%basin%lqwscan)) vs%basin%lqwscan = vs%basin%lqwscan/frac
        if (associated(vs%basin%fzwscan)) vs%basin%fzwscan = vs%basin%fzwscan/frac
        if (associated(vs%basin%cmas)) then
            where (tcanfrac > 0.0) vs%basin%cmas = vs%basin%cmas/tcanfrac
        end if
        if (associated(vs%basin%tacan)) then
            where (tcanfrac > 0.0) vs%basin%tacan = vs%basin%tacan/tcanfrac
        end if
        if (associated(vs%basin%qacan)) then
            where (tcanfrac > 0.0) vs%basin%qacan = vs%basin%qacan/tcanfrac
        end if
        if (associated(vs%basin%tcan)) then
            where (tcanfrac > 0.0) vs%basin%tcan = vs%basin%tcan/tcanfrac
        end if
        if (associated(vs%basin%gro)) then
            where (tcanfrac > 0.0) vs%basin%gro = vs%basin%gro/tcanfrac
        end if

        !> Snow variables.
        if (associated(vs%basin%fsno)) vs%basin%fsno = vs%basin%fsno/frac
        if (associated(vs%basin%sno)) vs%basin%sno = vs%basin%sno/frac
        if (associated(vs%basin%rhosno)) then
            where (tsnofrac > 0.0) vs%basin%rhosno = vs%basin%rhosno/tsnofrac
        end if
!-        if (associated(vs%basin%zsno)) vs%basin%zsno = vs%basin%zsno/frac
        if (associated(vs%basin%lqwssno)) vs%basin%lqwssno = vs%basin%lqwssno/frac
        if (associated(vs%basin%tsno)) then
            where (tsnofrac > 0.0) vs%basin%tsno = vs%basin%tsno/tsnofrac
        end if
        if (associated(vs%basin%albsno)) then
            where (tsnofrac > 0.0) vs%basin%albsno = vs%basin%albsno/tsnofrac
        end if
        if (associated(vs%basin%drainsno)) vs%basin%drainsno = vs%basin%drainsno/frac

        !> Surface variables.
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
!-        if (associated(vs%basin%lqwspnd)) vs%basin%lqwspnd = vs%basin%lqwspnd/frac
        if (associated(vs%basin%tpnd)) then
            where (tpndfrac > 0.0) vs%basin%tpnd = vs%basin%tpnd/tpndfrac
        end if
        if (associated(vs%basin%pndcaf)) then
            where (tpndfrac > 0.0) vs%basin%pndcaf = vs%basin%pndcaf/tpndfrac
        end if
        if (associated(vs%basin%potevp)) vs%basin%potevp = vs%basin%potevp/frac
        if (associated(vs%basin%et)) vs%basin%et = vs%basin%et/frac
!-        if (associated(vs%basin%evpb)) vs%basin%evpb = vs%basin%evpb/frac
!-        if (associated(vs%basin%arrd)) vs%basin%arrd = vs%basin%arrd/frac
        if (associated(vs%basin%ovrflw)) vs%basin%ovrflw = vs%basin%ovrflw/frac
        if (associated(vs%basin%qevp)) vs%basin%qevp = vs%basin%qevp/frac
        if (associated(vs%basin%qsens)) vs%basin%qsens = vs%basin%qsens/frac
        if (associated(vs%basin%gzero)) vs%basin%gzero = vs%basin%gzero/frac
        do j = 1, 4
            if (associated(vs%basin%tsfs)) vs%basin%tsfs(:, j) = vs%basin%tsfs(:, j)/frac
        end do
        if (associated(vs%basin%tsurf)) vs%basin%tsurf = vs%basin%tsurf/frac

        !> Ice/glacier variables.
        if (associated(vs%basin%lqwsice)) vs%basin%lqwsice = vs%basin%lqwsice/frac
        if (associated(vs%basin%tice)) then
            where (ticefrac > 0.0) vs%basin%tice = vs%basin%tice/ticefrac
        end if

        !> Subsurface/soil variables.
        do j = 1, shd%lc%IGND
            if (associated(vs%basin%dzsol)) vs%basin%dzsol(:, j) = vs%basin%dzsol(:, j)/frac
            if (associated(vs%basin%dzsolhyd)) vs%basin%dzsolhyd(:, j) = vs%basin%dzsolhyd(:, j)/frac
            if (associated(vs%basin%thlqsol)) vs%basin%thlqsol(:, j) = vs%basin%thlqsol(:, j)/frac
            if (associated(vs%basin%thicsol)) vs%basin%thicsol(:, j) = vs%basin%thicsol(:, j)/frac
!-            if (associated(vs%basin%lqwssol)) vs%basin%lqwssol(:, j) = vs%basin%lqwssol(:, j)/frac
!-            if (associated(vs%basin%fzwssol)) vs%basin%fzwssol(:, j) = vs%basin%fzwssol(:, j)/frac
            if (associated(vs%basin%tsol)) vs%basin%tsol(:, j) = vs%basin%tsol(:, j)/frac
            if (associated(vs%basin%gflx)) vs%basin%gflx(:, j) = vs%basin%gflx(:, j)/frac
            if (associated(vs%basin%latflw)) vs%basin%latflw(:, j) = vs%basin%latflw(:, j)/frac
        end do
        if (associated(vs%basin%zsol)) vs%basin%zsol = vs%basin%zsol/frac
        if (associated(vs%basin%zsolhyd)) vs%basin%zsolhyd = vs%basin%zsolhyd/frac
        if (associated(vs%basin%zsolsat)) vs%basin%zsolsat = vs%basin%zsolsat/frac
        if (associated(vs%basin%ggeo)) vs%basin%ggeo = vs%basin%ggeo/frac
        if (associated(vs%basin%tbas)) vs%basin%tbas = vs%basin%tbas/frac
        if (associated(vs%basin%drainsol)) vs%basin%drainsol = vs%basin%drainsol/frac

        !> Groundwater/lower zone storage variables.
        if (associated(vs%basin%rchg)) vs%basin%rchg = vs%basin%rchg/frac
        if (associated(vs%basin%stggw)) vs%basin%stggw = vs%basin%stggw/frac
        if (associated(vs%basin%lkg)) vs%basin%lkg = vs%basin%lkg/frac
!-        if (associated(vs%basin%dzs)) vs%basin%dzs = vs%basin%dzs/frac

        !> Routing variables.
        if (associated(vs%basin%rff)) vs%basin%rff = vs%basin%rff/frac
!        if (associated(vs%basin%qi)) vs%basin%qi = vs%basin%qi/frac
!        if (associated(vs%basin%qo)) vs%basin%qo = vs%basin%qo/frac
!        if (associated(vs%basin%stgch)) vs%basin%stgch = vs%basin%stgch/frac
!        if (associated(vs%basin%zlvl)) vs%basin%zlvl = vs%basin%zlvl/frac
!        if (associated(vs%basin%div)) vs%basin%div = vs%basin%div/frac
!        if (associated(vs%basin%abstr)) vs%basin%abstr = vs%basin%abstr/frac

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
