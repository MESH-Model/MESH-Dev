module sa_mesh_run_between_grid

    implicit none

!>>>temp_diversion
    integer :: iun_div = 64, n_div, dt_div, iyear_div, ijday_div
    character(len = 13) :: fn_div = 'diversion.txt'
    real, dimension(:), allocatable :: x_src, y_src, x_snk, y_snk, in_div_m3, in_div_m3s, tnsfr_div
    integer, dimension(:), allocatable :: jx_src, iy_src, jx_snk, iy_snk, rnk_src, rnk_snk
    logical run_div
    real, dimension(:), allocatable :: qo_div_dly, qa_div_dly
!<<<temp_diversion

    contains

    subroutine run_between_grid_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use FLAGS
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use SA_RTE_module, only: SA_RTE_init
        use WF_ROUTE_config, only: WF_ROUTE_init
        use reservoir
        use rte_module
        use save_basin_output, only: run_save_basin_output_init

!>>>temp_diversion
        use txt_io
!<<<temp_diversion

        !> Cropland irrigation module.
        use cropland_irrigation_between_grid, only: runci_between_grid_init

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer NA

!>>>temp_diversion
        integer l, n, ijday1, ijday2, iskip, ierr
!<<<temp_diversion

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Initialize basin structures.
!-        call read_basin_structures(shd)

        if (BASINSWEOUTFLAG > 0) then
            open(85, file = './' // trim(fls%GENDIR_OUT) // '/basin_SCA_alldays.csv')
            open(86, file = './' // trim(fls%GENDIR_OUT) // '/basin_SWE_alldays.csv')
        end if !(BASINSWEOUTFLAG > 0) then

!todo: switch
        call SA_RTE_init(shd)

        !> Watflood, 1988.
        call WF_ROUTE_init(shd, fls, stfl, rrls)

        !> Fuad Reservoir Flag Active
        if (RESERVOIRFLAG == 2) then
            call init_reservoirs('coeff_reserv.txt')
        end if

        !> RPN RTE.
        call run_rte_init(fls, shd, stfl, rrls)

        call run_save_basin_output_init(fls, shd, cm)

        !> Cropland irrigation module (ICU).
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

            !> Skips records to present in file.
            call Julian_Day_ID(iyear_div, ijday_div, ijday1)
            call Julian_Day_ID(ic%start%year, ic%start%jday, ijday2)
            if (ijday2 < ijday1) then
                if (ipid == 0) then
                    print 9994, trim(fn_div), trim(fn_div), iyear_div, ijday_div, ic%start%year, ic%start%jday
                end if
            end if
            iskip = (ijday2 - ijday1)*24/dt_div
            if (iskip > 0) then
                if (ipid == 0) print 9993, iskip
                ierr = read_records_txt(iun_div, in_div_m3s, iskip)
                if (ierr /= 0) then
                    if (ipid == 0) print 9990, trim(fn_div)
                end if
            end if

9997    format(3x, 'Number of ', (a), ': ', i5)
9994    format( &
            /3x, 'WARNING: The start date in ', (a), ' occurs after the simulation start date.', &
            /8x, 'This may cause a no flow error if the channels are initialized using the observed value.', &
            /8x, (a), ' start date:', i5, i4, &
            /8x, 'Simulation start date:', i5, i4)
9993    format(3x, 'Skipping ', i8, ' registers in the file.')
9990    format(3x, 'ERROR: End of file reached when reading from ', (a), '.')

            !> Print a summary of locations to file.
            if (ipid == 0) then
                if (ro%VERBOSEMODE > 0) print 9997, 'diversion point sets', n_div
!                if (ro%DIAGNOSEMODE > 0) then
                    print 1020, 'SET', 'SOURCE', 'IY', 'JX', 'RANK', 'SINK', 'IY', 'JX', 'RANK'
                    do l = 1, n_div
                        print 1020, l, '', iy_src(l), jx_src(l), rnk_src(l), '', iy_snk(l), jx_snk(l), rnk_snk(l)
                    end do
!                end if
            end if

1020    format(3x, 9(g16.9, 1x))

        end if
!<<<temp_diversion

    end subroutine

    subroutine run_between_grid(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use FLAGS
        use model_dates
        use txt_io
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use SA_RTE_module, only: SA_RTE
        use WF_ROUTE_module, only: WF_ROUTE_between_grid
        use rte_module
        use save_basin_output, only: run_save_basin_output

        !> Cropland irrigation module.
        use cropland_irrigation_between_grid, only: runci_between_grid

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(dates_model) :: ts
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer k, ki, ierr

!>>>temp_diversion
        character(len = 4) ffmti
        character(len = 200) fn
        integer l, n, iun
!<<<temp_diversion

        !> SCA variables
        real TOTAL_AREA, FRAC, basin_SCA, basin_SWE

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Read in reservoir release values if such a type of reservoir has been defined.
        if (fms%rsvr%n > 0) then
            if (count(fms%rsvr%rls%b1 == 0.0) > 0) then

                !> The minimum time-stepping of the reservoir file is hourly.
                if (mod(ic%now%hour, fms%rsvr%qorls%dts) == 0 .and. ic%now%mins == 0) then
                    ierr = read_records_txt(fms%rsvr%qorls%fls%iun, fms%rsvr%qorls%val)

                    !> Stop if no releases exist.
                    if (ierr /= 0) then
                        print "(3x, 'ERROR: End of file reached when reading from ', (a), '.')", &
                            trim(adjustl(fms%rsvr%qorls%fls%fname))
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
                    fms%stmg%qomeas%val = -1.0
                end if
            end if
            stfl%qhyd = fms%stmg%qomeas%val
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
                    tnsfr_div(l) = min(max(stas_grid%chnl%s(n) - 0.0, 0.0)*(1.0 - 0.05), in_div_m3(l)) !m3
                    stas_grid%chnl%s(n) = stas_grid%chnl%s(n) - tnsfr_div(l)
                end if

                !> Sink.
                n = rnk_snk(l)
                if (n >= 1 .and. n <= shd%NAA) then
                    stas_grid%chnl%s(n) = stas_grid%chnl%s(n) + tnsfr_div(l)
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
                    fn = 'MESH_output_diversion' // trim(adjustl(ffmti)) // '.csv'
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
1010    format(9999(g15.7e2, ','))
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
                TOTAL_AREA = wb%basin_area
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

!todo: Switch
        call SA_RTE(shd, wb)

        !> Watflood, 1988.
        call WF_ROUTE_between_grid(shd, wb, stfl, rrls)

        !> RPN RTE.
        call run_rte_between_grid(fls, shd, wb, stfl, rrls)

        !> Cropland irrigation module (ICU).
        call runci_between_grid(shd, fls, cm)

        call run_save_basin_output(fls, shd, cm)

    end subroutine

    subroutine run_between_grid_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        use mpi_module
        use model_files_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT

        use WF_ROUTE_config, only: WF_ROUTE_finalize
        use save_basin_output, only: run_save_basin_output_finalize

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sv
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Watflood, 1988.
        call WF_ROUTE_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        call run_save_basin_output_finalize(fls, shd, cm)

    end subroutine

end module
