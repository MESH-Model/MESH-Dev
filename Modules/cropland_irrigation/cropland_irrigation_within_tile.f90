module cropland_irrigation_within_tile

    use cropland_irrigation_variables

    contains

    subroutine runci_within_tile(shd, fls, cm)

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
        real Kc

        if (.not. cifg%PROCESS_ACTIVE) return

        do k = il1, il2

            !> Grab the grid index (for ylat, xlng)
            ik = shd%lc%ILMOS(k)

            !> Calc 'calc_ET0' (PEVP).
            stas%cnpy%pevp(k) = calc_ET0( &
                cm%dat(ck%TT)%GAT(k), cm%dat(ck%UV)%GAT(k), cm%dat(ck%HU)%GAT(k), cm%dat(ck%P0)%GAT(k), cm%dat(ck%FB)%GAT(k), &
                shd%ylat(ik), shd%xlng(ik), shd%ELEV(ik), &
                pm%sfp%zrfm(k), &
                pm%cp%fcan(k, 1), pm%cp%fcan(k, 2), pm%cp%fcan(k, 3), pm%cp%fcan(k, 4), &
                ic%now%jday, ic%now%hour)

            !> Activate the new growing season.
            if (civ%jdini(k) == 0) then

                !> Starting day of the growing season.
                if (cip%jdsow(k) == 0 .and. stas%cnpy%gro(k) > 0.0) then
                    civ%jdini(k) = ic%now%jday
                else
                    civ%jdini(k) = cip%jdsow(k)
                end if

                !> Starting days of the growth stages.
                civ%jddev(k) = civ%jdini(k) + cip%ldini(k)
                civ%jdmid(k) = civ%jddev(k) + cip%lddev(k)
                civ%jdlate(k) = civ%jdmid(k) + cip%ldmid(k)
                civ%jdend(k) = civ%jdlate(k) + cip%ldlate(k)

            end if

            !> Cycle if outside the growing season or not a crop.
            if (civ%icrop(k) /= 1 .or. ic%now%jday < (civ%jdini(k) - 1) .or. ic%now%jday > (civ%jdend(k) + 1)) cycle

            !> Initial conditions for the beginning of the period.
            if (ic%now%jday == (civ%jdini(k) - 1)) then

                !> Initialize states.
                if (ic%ts_daily == 1) then
                    civ%lqws2_mm(k) = 0.0
                    civ%lqws1_mm(k) = 0.0
                    civ%pre_mm(k) = 0.0
                    civ%pevp_mm(k) = 0.0
                end if

                !> Daily.
                if (btest(cifg%ts_flag, 0)) then
                    civ%lqws2_mm(k) = civ%lqws2_mm(k) + (sum(stas%sl%lqws(k, :))*pm%cp%fcan(k, 3))/((3600.0/ic%dts)*24.0)
                end if

                !> Hourly.
                if (btest(cifg%ts_flag, 2) .and. ic%ts_daily > (3600.0/ic%dts)*23.0) then
                    civ%lqws2_mm(k) = civ%lqws2_mm(k) + (sum(stas%sl%lqws(k, :))*pm%cp%fcan(k, 3))/(3600.0/ic%dts)
                end if

                !> Per time-step.
                if (btest(cifg%ts_flag, 3) .and. ic%ts_daily == (3600.0/ic%dts)*24.0) then
                    civ%lqws2_mm(k) = sum(stas%sl%lqws(k, :))*pm%cp%fcan(k, 3)
                end if

            end if

            !> Outside the growing period.
            if (ic%now%jday == (civ%jdend(k) + 1)) then
                civ%jdini(k) = 0
                civ%icu_mm(k) = 0.0
                cycle
            end if

            !> Inside the growing period.
            if (ic%now%jday >= civ%jdini(k)) then

                !> Accumulate states for the present period.
                civ%pre_mm(k) = civ%pre_mm(k) + cm%dat(ck%RT)%GAT(k)*pm%cp%fcan(k, 3)*ic%dts
                civ%pevp_mm(k) = civ%pevp_mm(k) + stas%cnpy%pevp(k)*pm%cp%fcan(k, 3)*ic%dts
                civ%lqws1_mm(k) = civ%lqws1_mm(k) + sum(stas%sl%lqws(k, :))*pm%cp%fcan(k, 3)

                !> Determine Kc.
                if (ic%now%jday >= civ%jdlate(k)) then
                    Kc = cip%Kclate(k)
                else if (ic%now%jday >= civ%jdmid(k)) then
                    Kc = cip%Kcmid(k)
                else if (ic%now%jday >= civ%jddev(k)) then
                    Kc = cip%Kcdev(k)
                else
                    Kc = cip%Kcini(k)
                end if

                !> Daily.
                if (btest(cifg%ts_flag, 0) .and. ic%ts_daily == (3600.0/ic%dts)*24) then
                    civ%lqws1_mm(k) = civ%lqws1_mm(k)/ic%ts_daily
                    civ%icu_mm(k) = (105.0*Kc*civ%pevp_mm(k)) - civ%pre_mm(k) - (civ%lqws1_mm(k) - civ%lqws2_mm(k))
                    civ%pre_mm(k) = 0.0
                    civ%pevp_mm(k) = 0.0
                    civ%lqws2_mm(k) = civ%lqws1_mm(k)
                    civ%lqws1_mm(k) = 0.0
                end if

                !> Hourly.
                if (btest(cifg%ts_flag, 2) .and. ic%ts_hourly == (3600.0/ic%dts)) then
                    civ%lqws1_mm(k) = civ%lqws1_mm(k)/ic%ts_hourly
                    civ%icu_mm(k) = (105.0*Kc*civ%pevp_mm(k)) - civ%pre_mm(k) - (civ%lqws1_mm(k) - civ%lqws2_mm(k))
                    civ%pre_mm(k) = 0.0
                    civ%pevp_mm(k) = 0.0
                    civ%lqws2_mm(k) = civ%lqws1_mm(k)
                    civ%lqws1_mm(k) = 0.0
                end if

                !> Per time-step.
                if (btest(cifg%ts_flag, 3)) then
                    civ%pre_mm(k) = 0.0
                    civ%icu_mm(k) = (105.0*Kc*civ%pevp_mm(k)) - civ%pre_mm(k) - (civ%lqws1_mm(k) - civ%lqws2_mm(k))
                    civ%pevp_mm(k) = 0.0
                    civ%lqws2_mm(k) = civ%lqws1_mm(k)
                    civ%lqws1_mm(k) = 0.0
                end if

            end if

        end do

    end subroutine

end module
