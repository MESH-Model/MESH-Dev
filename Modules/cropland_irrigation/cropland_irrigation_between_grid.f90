module cropland_irrigation_between_grid

    use cropland_irrigation_variables

    implicit none

    contains

    subroutine runci_between_grid(shd, fls, cm)

        use mpi_shared_variables
        use sa_mesh_shared_variabletypes
        use model_files_variabletypes
        use climate_forcing
        use model_dates

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls
        type(clim_info) :: cm

        if (.not. cifg%PROCESS_ACTIVE .or. ipid /= 0) return

        !> Daily.
        if (btest(cifg%ts_flag, civ%fk%KDLY) .and. ic%ts_daily == (3600.0/ic%dts)*24) then
            write(950, "(i4, ',', i5, ',', 999(e14.6, ','))") ic%now%jday, ic%now%year, &
                runci_between_grid_process(shd, civ%fk%KDLY, il1, il2)
        end if

        !> Hourly.
        if (btest(cifg%ts_flag, civ%fk%KHLY) .and. ic%ts_hourly == (3600.0/ic%dts)) then
            write(952, "(i4, ',', i5, ',', i3, ',', 999(e14.6, ','))") ic%now%jday, ic%now%year, ic%now%hour, &
                runci_between_grid_process(shd, civ%fk%KHLY, il1, il2)
        end if

        !> Per time-step.
        if (btest(cifg%ts_flag, civ%fk%KTS)) then
            write(953, "(i4, ',', i5, ',', 2(i3, ','), 999(e14.6, ','))") ic%now%jday, ic%now%year, ic%now%hour, ic%now%mins, &
                runci_between_grid_process(shd, civ%fk%KTS, il1, il2)
        end if

    end subroutine

    function runci_between_grid_process(shd, fk, k1, k2)

        use sa_mesh_shared_variabletypes

        !> Returns ICU.
        real(kind = 4) runci_between_grid_process

        !> Input variables.
        type(ShedGridParams) :: shd
        integer :: fk, k1, k2

        !> Local variables.
        integer k, ik
        real ICU, FRAC, dna

        !> Accumulate the weighted-area average ICU for the basin.
        ICU = 0.0
        dna = 0.0
        do k = k1, k2
            ik = shd%lc%ILMOS(k)
            FRAC = shd%lc%ACLASS(ik, shd%lc%JLMOS(k))*shd%FRAC(ik)
            dna = dna + FRAC
            ICU = ICU + civ%vars(fk)%icu_mm(k)*FRAC
        end do

        !> Set a limit of zero on ICU in case the simulation starting date > sowing date.
        runci_between_grid_process = max(0.0, ICU/dna)

    end function

end module
