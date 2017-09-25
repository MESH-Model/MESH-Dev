module sa_mesh_run_between_grid

    implicit none

    contains

    subroutine run_between_grid_init(shd, fls, cm, stfl, rrls)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use FLAGS
        use climate_forcing
        use model_output_variabletypes

        use SA_RTE_module, only: SA_RTE_init
        use WF_ROUTE_config, only: WF_ROUTE_init
        use rte_module
        use save_basin_output, only: run_save_basin_output_init

        !> Cropland irrigation module.
        use cropland_irrigation_between_grid, only: runci_between_grid_init

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer NA

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Initialize basin structures.
        call read_basin_structures(shd)

        if (BASINSWEOUTFLAG > 0) then
            open(85, file = './' // trim(fls%GENDIR_OUT) // '/basin_SCA_alldays.csv')
            open(86, file = './' // trim(fls%GENDIR_OUT) // '/basin_SWE_alldays.csv')
        end if !(BASINSWEOUTFLAG > 0) then

!todo: switch
        call SA_RTE_init(shd)

        !> Watflood, 1988.
        call WF_ROUTE_init(shd, fls, stfl, rrls)

        !> RPN RTE.
        call run_rte_init(fls, shd, stfl, rrls)

        call run_save_basin_output_init(fls, shd, cm)

        !> Cropland irrigation module (ICU).
        call runci_between_grid_init(shd, fls)

    end subroutine

    subroutine run_between_grid(shd, fls, cm, stfl, rrls)

        use mpi_module
        use model_files_variables
        use sa_mesh_shared_variables
        use FLAGS
        use txt_io
        use climate_forcing
        use model_output_variabletypes

        use SA_RTE_module, only: SA_RTE
        use WF_ROUTE_module, only: WF_ROUTE_between_grid
        use rte_module
        use save_basin_output, only: run_save_basin_output

        !> Cropland irrigation module.
        use cropland_irrigation_between_grid, only: runci_between_grid

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer k, ki, ierr

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

!todo: Switch
        call SA_RTE(shd)

        !> Watflood, 1988.
        call WF_ROUTE_between_grid(shd, stfl, rrls)

        !> RPN RTE.
        call run_rte_between_grid(fls, shd, stfl, rrls)

        !> Cropland irrigation module (ICU).
        call runci_between_grid(shd, fls, cm)

        call run_save_basin_output(fls, shd, cm)

    end subroutine

    subroutine run_between_grid_finalize(fls, shd, cm, stfl, rrls)

        use mpi_module
        use model_files_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes

        use WF_ROUTE_config, only: WF_ROUTE_finalize
        use save_basin_output, only: run_save_basin_output_finalize

        type(fl_ids) :: fls
        type(ShedGridParams) :: shd
        type(clim_info) :: cm
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Return if not the head node or if grid processes are not active.
        if (ipid /= 0 .or. .not. ro%RUNGRID) return

        !> Watflood, 1988.
        call WF_ROUTE_finalize(fls, shd, stfl, rrls)

        call run_save_basin_output_finalize(fls, shd, cm)

    end subroutine

end module
