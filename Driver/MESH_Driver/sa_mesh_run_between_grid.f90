module sa_mesh_run_between_grid

    implicit none

    contains

    subroutine run_between_grid_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT
        use flags

        use SA_RTE_module, only: SA_RTE_init
        use WF_ROUTE_config, only: WF_ROUTE_init
        use save_basin_output, only: run_save_basin_output_init

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
        !* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
        !* WF_START_DAY OBSERVED STREAMFLOW START DAY
        !* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
!-        integer WF_START_YEAR, WF_START_DAY, WF_START_HOUR
!-        integer JDAY_IND_STRM, JDAY_IND1, JDAY_IND2
!-        real I_G, J_G
        integer NA

        !> Return if not the head node.
        if (ipid /= 0) return

        !> Initialiation of states.
        NA = shd%NA

        !> Stream channel.
        stas%chnl%n = NA
        allocate(stas%chnl%qi(1:NA), stas%chnl%qo(1:NA), stas%chnl%s(1:NA))
        stas%chnl%qi(1:NA) = 0.0
        stas%chnl%qo(1:NA) = 0.0
        stas%chnl%s(1:NA) = 0.0

        !> Lake.
!+        stas%lk%n = NLK
!+        allocate(stas%lk%ab(1:NLK), stas%lk%qi(1:NLK), stas%lk%qo(1:NLK), stas%lk%s(1:NLK))
!+        stas%lk%ab(1:NLK) = 0.0
!+        stas%lk%qi(1:NLK) = 0.0
!+        stas%lk%qo(1:NLK) = 0.0
!+        stas%lk%s(1:NLK) = 0.0

        !> Reservoir.
!+        stas%rsvr%n = NRSVR
!+        allocate(stas%rsvr%ab(1:NRSVR), stas%rsvr%qi(1:NRSVR), stas%rsvr%qo(1:NRSVR), stas%rsvr%s(1:NRSVR))
!+        stas%rsvr%ab(1:NRSVR) = 0.0
!+        stas%rsvr%qi(1:NRSVR) = 0.0
!+        stas%rsvr%qo(1:NRSVR) = 0.0
!+        stas%rsvr%s(1:NRSVR) = 0.0

        if (BASINSWEOUTFLAG > 0) then
            open(85, file = './' // trim(fls%GENDIR_OUT) // '/basin_SCA_alldays.csv')
            open(86, file = './' // trim(fls%GENDIR_OUT) // '/basin_SWE_alldays.csv')
        end if !(BASINSWEOUTFLAG > 0) then

!todo: switch
        call SA_RTE_init(shd)
        call WF_ROUTE_init(shd, fls, stfl, rrls)
        call run_save_basin_output_init(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        !> Cropland irrigation module (ICU).
        call runci_between_grid_init(shd, fls)

    end subroutine

    subroutine run_between_grid(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_variables
        use model_files_variables
        use model_dates
        use climate_forcing
        use model_output_variabletypes
        use MODEL_OUTPUT
        use flags

        use SA_RTE_module, only: SA_RTE
        use WF_ROUTE_module, only: WF_ROUTE_between_grid
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
        integer k, ki

        !> SCA variables
        real TOTAL_AREA, FRAC, basin_SCA, basin_SWE

        !> Return if not the head node.
        if (ipid /= 0) return

        !> calculate and write the basin avg SCA similar to watclass3.0f5
        !> Same code than in wf_ensim.f subrutine of watclass3.0f8
        !> Especially for version MESH_Prototype 3.3.1.7b (not to be incorporated in future versions)
        !> calculate and write the basin avg SWE using the similar fudge factor!!!
        if (BASINSWEOUTFLAG > 0) then

            !> BASIN_FRACTION is the basin snow cover
            !> (portions of the grids outside the basin are not included)
            !> for a given day - JDAY_NOW in the if statement
!            if (BASIN_FRACTION(1) == -1) then
!todo: FRAC is not actually the fraction of the grid square
!within the basin, we should be using some other value, but I'm
!not sure what.
!todo: calculate frac and write document to send to someone else.
!                do i = 1, NA ! NA = number of grid squares
!                    BASIN_FRACTION(i) = shd%FRAC(i)
!                end do
!            end if

            if (ic%now%hour == 12 .and. ic%now%mins == 0) then
                basin_SCA = 0.0
                basin_SWE = 0.0
!                do i = 1, NA
!                    if (BASIN_FRACTION(i) /= 0.0) then
!                        basin_SCA = basin_SCA + FSNOGRD(i)/BASIN_FRACTION(i)
!                        basin_SWE = basin_SWE + SNOGRD(i)/BASIN_FRACTION(i)
!                    end if
!                end do
!                basin_SCA = basin_SCA/NA
!                basin_SWE = basin_SWE/NA
                TOTAL_AREA = wb%basin_area

                !> BRUCE DAVISON - AUG 17, 2009 (see notes in my notebook for this day)
                !> Fixed calculation of basin averages. Needs documenting and testing.
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
        call WF_ROUTE_between_grid(shd, wb, stfl, rrls)

        !> Cropland irrigation module (ICU).
        call runci_between_grid(shd, fls, cm)

        call run_save_basin_output(shd, fls, ts, cm, wb, eb, sp, stfl, rrls)

    end subroutine

    subroutine run_between_grid_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

        use mpi_shared_variables
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

        !> Return if not the head node.
        if (ipid /= 0) return

        call WF_ROUTE_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)
        call run_save_basin_output_finalize(fls, shd, cm, wb, eb, sv, stfl, rrls)

    end subroutine

end module
