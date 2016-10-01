module cropland_irrigation_between_grid

    use cropland_irrigation_variables

    contains

    subroutine runci_between_grid(shd, fls, cm)

        use mpi_shared_variables
        use model_files_variabletypes
        use sa_mesh_shared_parameters
        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use model_files_variabletypes
        use climate_forcing

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        integer k, ik
        real ICU, FRAC, dna

        if (.not. cifg%PROCESS_ACTIVE .or. ipid /= 0) return

        !> Accumulate the weighted-area average ICU for the basin.
        ICU = 0.0
        dna = 0.0
        do k = il1, il2
            ik = shd%lc%ILMOS(k)
            FRAC = shd%lc%ACLASS(ik, shd%lc%JLMOS(k))*shd%FRAC(ik)
            dna = dna + FRAC
            ICU = ICU + civ%icu_mm(k)*FRAC
        end do

        !> Set a limit of zero on ICU in case the simulation starting date > sowing date.
        ICU = max(0.0, ICU/dna)

        !> Daily.
        if (btest(cifg%ts_flag, 0) .and. ic%ts_daily == (3600.0/ic%dts)*24) then
            write(950, "(i4, ',', i5, ',', 999(e14.6, ','))") ic%now%jday, ic%now%year, &
                ICU
        end if

        !> Hourly.
        if (btest(cifg%ts_flag, 2) .and. ic%ts_hourly == (3600.0/ic%dts)) then
            write(952, "(i4, ',', i5, ',', i3, ',', 999(e14.6, ','))") ic%now%jday, ic%now%year, ic%now%hour, &
                ICU
        end if

        !> Per time-step.
        if (btest(cifg%ts_flag, 3)) then
            write(953, "(i4, ',', i5, ',', 2(i3, ','), 999(e14.6, ','))") ic%now%jday, ic%now%year, ic%now%hour, ic%now%mins, &
                ICU
        end if

    end subroutine

end module
