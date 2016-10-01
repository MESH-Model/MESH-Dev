module cropland_irrigation_init

    use cropland_irrigation_variables

    contains

    subroutine runci_init(shd, fls)

        !> For 'il1,il2' indexing and ipid (current node).
        use mpi_shared_variables

        !> For canopy fractions.
        use sa_mesh_shared_parameters

        !> For 'shd' type (basin information), 'ro%' (run options) for print options, and 'FCAN' (canopy fraction, ROW indexing).
        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use sa_mesh_shared_output_variables

        !> For 'fls%GENDIR_OUT'.
        use model_files_variabletypes

        !> For current date and counter.
        use model_dates

        type(ShedGridParams) :: shd
        type(fl_ids) :: fls

        integer NML, k, m, ierr

        if (.not. cifg%PROCESS_ACTIVE) return

        if (ipid == 0 .and. ro%VERBOSEMODE > 0) print 9999

9999    format(/1x, '------------------------------------------------------', &
               /1x, '            CROPLAND IRRIGATION IS ACTIVE', &
               /1x, '------------------------------------------------------'/)
9998    format(/1x, 'Please correct these parameterization errors.'/)
9997    format( 3x, 'Bad parameter value: ', (a8), ' = ', i5)
9996    format( 3x, 'Bad parameter value: ', (a8), ' = ', f6.3)
9995    format( 1x, (a), " is zero. Sow date will be determined by the growth index 'GRO'.")
9994    format( 1x, 'GRU: ', i3, '    (no crops active)')
9993    format( 1x, 'GRU: ', i3, ' - Contributing fraction of crops: ', f5.3)

        !> Local variables.
        NML = shd%lc%NML

        !> Allocate and initialize internal variables.
        allocate(&
            civ%icrop(NML), civ%jdini(NML), civ%jddev(NML), civ%jdmid(NML), civ%jdlate(NML), civ%jdend(NML), &
            civ%lqws2_mm(NML), civ%lqws1_mm(NML), civ%pre_mm(NML), civ%pevp_mm(NML), civ%icu_mm(NML))
        civ%icrop = 0; civ%jdini = 0; civ%jddev = 0; civ%jdmid = 0; civ%jdlate = 0; civ%jdend = 0
        civ%lqws2_mm = 0.0; civ%lqws1_mm = 0.0; civ%pre_mm = 0.0; civ%pevp_mm = 0.0; civ%icu_mm = 0.0

        !> Check the parameter values.
        if (ipid == 0) then
            ierr = 0
            do m = 1, shd%lc%NTYPE
                if (pmrow%cp%fcan(m, 3) > 0.0) then
                    print 9993, m, pmrow%cp%fcan(m, 3)
                    if (ciprot%jdsow(m) <= 0) print 9995, 'jdsow'
                    if (ciprot%ldini(m) <= 0) then
                        print 9997, 'ldini', ciprot%ldini(m)
                        ierr = ierr + 1
                    end if
                    if (ciprot%lddev(m) <= 0) then
                        print 9997, 'lddev', ciprot%lddev(m)
                        ierr = ierr + 1
                    end if
                    if (ciprot%ldmid(m) <= 0) then
                        print 9997, 'ldmid', ciprot%ldmid(m)
                        ierr = ierr + 1
                    end if
                    if (ciprot%ldlate(m) <= 0) then
                        print 9997, 'ldlate', ciprot%ldlate(m)
                        ierr = ierr + 1
                    end if
                    if (ciprot%Kcini(m) <= 0) then
                        print 9996, 'Kcini', ciprot%Kcini(m)
                        ierr = ierr + 1
                    end if
                    if (ciprot%Kcdev(m) <= 0) then
                        print 9996, 'Kcdev', ciprot%Kcdev(m)
                        ierr = ierr + 1
                    end if
                    if (ciprot%Kcmid(m) <= 0) then
                        print 9996, 'Kcmid', ciprot%Kcmid(m)
                        ierr = ierr + 1
                    end if
                    if (ciprot%Kclate(m) <= 0) then
                        print 9996, 'Kclate', ciprot%Kclate(m)
                        ierr = ierr + 1
                    end if
                else
                    print 9994, m
                end if
            end do
            if (ierr > 0) then
                print 9998
                stop
            else
                print *
            end if
        end if

        !> Identify crop GRUs.
        civ%icrop(k) = 0
        do k = il1, il2

            !> FCAN(3) identifies as 'crop' in CLASS.ini.
            if (pm%cp%fcan(k, 3) > 0.0) then

                !> Identify the crop.
                civ%icrop(k) = 1

                !> Reset the starting day of the growing season.
                civ%jdini(k) = 0

            end if

        end do

        !> Daily.
        if (btest(cifg%ts_flag, 0)) then
            open(950, file = './' // trim(fls%GENDIR_OUT) // '/' // '/Basin_average_crop_irrigation.csv')
            write(950, '(a)') 'DAY,YEAR,' // 'ICU'
        end if

        !> Hourly.
        if (btest(cifg%ts_flag, 2)) then
            open(952, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_crop_irrigation_Hourly.csv')
            write(952, '(a)') 'DAY,YEAR,HOUR,' // 'ICU'
        end if

        !> Per time-step.
        if (btest(cifg%ts_flag, 3)) then
            open(953, file = './' // trim(fls%GENDIR_OUT) // '/Basin_average_crop_irrigation_ts.csv')
            write(953, '(a)') 'DAY,YEAR,HOUR,MINS,' // 'ICU'
        end if

    end subroutine

end module
