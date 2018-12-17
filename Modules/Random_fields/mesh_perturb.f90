module mesh_perturb

!>       MESH_Assimilate
!>=======================================================================
!> MESH_Assimilate
!>
!> Modified by: Ala Bahrami
!> I have applied these modification as a part of my Ph.D thesis in order to implement
!> the MESH Data Assimilation(MESH_DA) structure.
!>
!> Bahrami, 21/03/2017 - Adding using land_force_perturb, forcepert_types, file_variables modules
!>                     - Adding local variables tt, N_t, n2
!>                     - Adding three forcing data perturbation fields precip_pert, sw_pert, lw_pert
!>                     - Adding reading input variables of the Input2.ini file
!>                     - Adding ensemble loop
!>                     - Adding the section of Initialize random_fields variables
!>                     - Adding the section of generate random fields for initialization
!>                     - Adding the section of assigning variables related to perturbation fields
!>                     - Adding the section of perturbing input forcing data for every time step
!>                          by considering the spatial-temporal and cross correlation.
!>                     - Modifying the gridded data variables for output
!>                     - Adding closure of opened-forcing data for every ensemble loop
!>          03/05/2017 - Deallocate all variables which were been allocated before
!>          11/08/2017 - Adding perturbaiton of model states (SNO, THLQ)
!>          30/10/2017 - (DA) Reading perturbed GRACE observations
!>          09/01/2018 - Reading and writing the resume files in different folders for each ensemble member
!>          12/01/2018 - Allocating data assimilation variable
!>                     - Reading measurement operator H
!>          16/01/2018 - (DA)
!>                     - Constructing the model state matrix
!>                     - Calculating Ensemble Perturbations
!>                     - Calculating the background error covariance
!>                     - Calculating Kalman Gain Matrix
!>          18/01/2018 -
!>                     - Storing the random_fields variables at the beginning of the month
!>                     - (DA) Adding the capability of running the program in the assimilation mode (second run)
!>          19/01/2018 - (DA)
!>                     - Calculating the analysis increment
!>                     - Storing daily storage value
!>                     - Applying the analysis increment and update the TWS model states
!>          22/01/2018 - (DA)
!>                     - Updating the model states at the beginning of the month
!>          27/02/2018 - (DA)
!>                     - Adding the condition to calculate the Kalman Gain matrix
!>                     - and the Analysis increment and activate the assimilate mode from January 2009.
!>                     -
!>          26/03/2018 - (DA)
!>                     - Adding the capability to run the code month by month
!>                     - (DA)
!>          04/04/2018 - Updating the start time in run_option.ini file by adding number of the days from the previous
!>                     - month to the start time.
!>
!>          09/04/2018 - (DA)
!>                     - Constraining updating sno(i) when its value before update is zero
!>                     - or it value after update becomes less than 0.1.
!>                     -
!>          18/04/2018 -
!>                     - Modifying the code to update ic%start%jday when the OL/OLDA code runs for the
!>                     - the next month in order to read the appropriate input forcing.
!>                     - Adding a condition to update ic%start%jday and ic%start%year when
!>                     - ic%start%jday exceeds 366 or 367.
!>                     - (DA) Adding the variable month_n to avoid allocating GR_Pert varible manually
!>         24/07/2018  -
!>                     - Changing name of OLDA  to DA
!>                     - Changing name of the resume input folder
!>         24/09/2018  -
!>                     - Constraining the perturbation of longwave and shortwave radiation # 1389 to # 1396
!>         10/12/2018  - Moved MESH_Assimilate code to separate module mesh_perturb.f90.
!>=======================================================================

    !> Random fields.
    use land_force_perturb
    use forcepert_types
    use file_variables

    !> MESH variables.
    use mpi_module
    use model_files_variabletypes
    use sa_mesh_shared_variables
    use FLAGS
    use model_dates
    use climate_forcing
    use MODEL_OUTPUT

    implicit none

    private

    !> Forcing parameter for perturbation.
    type(forcepert_param_type), dimension(:), allocatable, public :: forcepert_param

    !> Data assimilation variables.
    !* RUN_DA: .true. for DA, .false. for OL.
    logical, public :: RUN_DA = .true.

    public :: read_random_fields_config, read_random_fields_resume
    public :: perturb_run_config, perturb_run_init, perturb_run_pre_ts, perturb_run_post_ts, perturb_run_finalize

    contains

    subroutine read_random_fields_config()

        integer iun, j, i

        !> Added by Ala Bahrami.
        !> Reading the random_fields input file.
        iun = 960
        open(iun, file = 'Inputs/Assimilation_parameters_perturbation.ini')
        read(iun, *) dx
        read(iun, *) dy
        read(iun, *) N_x
        read(iun, *) N_y
        read(iun, *) lambda_x
        read(iun, *) lambda_y
        read(iun, *) variance
        read(iun, *) RSEEDCONST
        read(iun, *) NRANDSEED2
        read(iun, *) N_forcepert
        read(iun, *) N_ens
        read(iun, *) dtstep
        read(iun, *) tcorr
        read(iun, *) GR_er
        read(iun, *) RESUMEFLAG_per
        read(iun, *) SAVERESUMEFLAG_per
        read(iun, *) month_n
        close(iun)

        !> Added by Ala Bahrami.
        if (RUN_DA) then

            !> Reading perturbed GRACE observations.
            if (allocated(GR_Pert)) deallocate(GR_Pert)
            allocate(GR_Pert(N_ens, month_n))
            open(10, file = 'Inputs/Assimilation_GRACETWS_perturbed.dat')
            do i = 1, N_ens
                read(10, *) (GR_Pert(i, j), j = 1, month_n)
            end do
            close (10)

            !> Reading Measurement Operator.
            if (allocated(H)) deallocate(H)
            if (allocated(HT)) deallocate(HT)
            allocate(H(1, 13*10173))
            allocate(HT(13*10173, 1))
            open(10, file = 'Inputs/Assimilation_Measurement_Operator.ini')
            read(10, *) (H(1, j), j = 1 , 13*10173)
            close(10)

            !> HT. Fortran cannot transpose of array of rank 1.
            HT = reshape(H, shape = (/13*10173 ,1/))

            !> Allocate model state matrix.
            if (allocated(X)) deallocate(X)
            allocate(X(13*10173, N_ens))

            !> Allocate model predicted TWS variables and analysis increments.
            if(allocated(HX)) deallocate(HX)
            allocate(HX(1, N_ens))
            if(allocated(D)) deallocate(D)
            allocate(D(1, N_ens))

            !> Allocate the analysis increment.
            if (allocated(AI)) deallocate(AI)
            allocate(AI(13*10173, N_ens))
            if(allocated(stg_accum)) deallocate(stg_accum)
            allocate(stg_accum(1, N_ens))

            !> Initialize.
            stg_accum(1, :) = 0

        end if

        !> Added by Ala Bahrami.
        !> Initialize random_fields variables.
        call assemble_forcepert_param(N_x, N_y, N_forcepert, forcepert_param)

        !> Deallocate the random_fields variables if they have been allocated before.
        if (allocated(ens_id)) deallocate(ens_id)
        if (allocated(Forcepert_rseed)) deallocate(Forcepert_rseed)
        if (allocated(rseed_store)) deallocate(rseed_store)
        if (allocated(Forcepert_ntrmdt)) deallocate(Forcepert_ntrmdt)
        if (allocated(ntrmdt_store)) deallocate(ntrmdt_store)
        if (allocated(Forcepert)) deallocate(Forcepert)

        !> Allocate Forcepert_ntrmdt_store if SAVERESUMEFLAG_per is active.
        if (SAVERESUMEFLAG_per == 1 .or. RESUMEFLAG_per == 1) then
            allocate(Forcepert_ntrmdt_store(N_forcepert, N_x*N_y*N_ens))
            allocate(Forcepert_rseed_store(NRANDSEED2*N_ens, 1))
        end if

        !> Allocate the the random_fields variables.
        allocate(ens_id(N_ens))
        allocate(Forcepert_rseed(NRANDSEED2, N_ens))
        allocate(rseed_store(NRANDSEED2, N_ens))
        allocate(Forcepert_ntrmdt(N_forcepert, N_x, N_y, N_ens))
        allocate(ntrmdt_store(N_forcepert, N_x, N_y, N_ens))
        allocate(Forcepert(N_forcepert, N_x, N_y, N_ens))

        it_counter = 1
!Note: Don't forget to bring it back to its original value when the code is crashed.
        month_counter = 1
        days_n = 0

    end subroutine

    subroutine read_random_fields_resume()

        integer iun, n

        !> Added by Ala Bahrami.
        select case (RESUMEFLAG_per)

            !> RESUMEFLAG_per 0
            case (0)
                write(*, *) ''
                write(*, *) 'Initializing Forcing Perturbations'
                write(*, *) ''

                !> Initialize.
                Forcepert_ntrmdt = 0.0
                Forcepert = 0.0
                initialize = .true.
                do n = 1, N_ens
                    ens_id(n) = n
                end do

                !> Generate random fields for initialization.
                call get_forcepert( &
                    N_forcepert, N_ens, N_x, N_y, &
                    dx, dy, dtstep, &
                    initialize, &
                    forcepert_param, &
                    ens_id, &
                    Forcepert_rseed, &
                    Forcepert_ntrmdt, &
                    Forcepert)

            !> RESUMEFLAG_per 1
            case (1)
                write(*, *) ''
                write(*, *) 'Reading Forcing Perturbations resume files'
                write(*, *) ''

                !> Initialize.
                do n = 1, N_ens
                    ens_id(n) = n
                end do

                !> Read and assign Forcepert_ntrmdt_store.
                iun = 963
                open(iun, file = 'Inputs/Forcepert_ntrmdt.seq', action = 'read', form = 'unformatted', access = 'sequential')
                read(iun) Forcepert_ntrmdt_store(1, :)
                read(iun) Forcepert_ntrmdt_store(2, :)
                read(iun) Forcepert_ntrmdt_store(3, :)
!todo: Reading the other field members.
!+                read(iun) Forcepert_ntrmdt_store(4, :)
!+                read(iun) Forcepert_ntrmdt_store(5, :)
!+                read(iun) Forcepert_ntrmdt_store(6, :)
!+                read(iun) Forcepert_ntrmdt_store(7, :)
!+                read(iun) Forcepert_ntrmdt_store(8, :)
                close(iun)
                Forcepert_ntrmdt = reshape(Forcepert_ntrmdt_store, (/N_forcepert, N_x, N_y, N_ens/))

                !> Read and assign Forcepert_rseed store.
                iun = 963
                open(iun, file = 'Inputs/Forcepert_rseed.seq', action = 'read', form = 'unformatted', access = 'sequential')
                read(iun) Forcepert_rseed_store
                close(iun)
                Forcepert_rseed = reshape(Forcepert_rseed_store, (/NRANDSEED2, N_ens/))

        end select

        !> Store random field variable to be used in ensemble loop iteration.
        rseed_store = Forcepert_rseed
        ntrmdt_store = Forcepert_ntrmdt

    end subroutine

    subroutine perturb_run_config(fls, shd, cm)

        type(fl_ids) fls
        type(ShedGridParams) shd
        type(CLIM_INFO) cm

        !> Added by Ala Bahrami.
        !> Starting main ensemble run.
        if (RUN_DA) then
            if (it_counter == 1) then
                write(*, *) ''
                write(*, *) 'The Data Assimilation (Forecast) Mode is Active'
                write(*, *) ''
            else if (it_counter == 2) then
                write(*, *) ''
                write(*, *) 'The Data Assimilation (Analysis) Mode is Active'
                write(*, *) ''
            end if
        else
            if (it_counter == 1) then
                write(*, *) ''
                write(*, *) 'The Open_Loop (Forecast) Mode is Active '
                write(*, *) ''
            end if
        end if

        !> Added by Ala Bahrami.
        !> Starting main ensemble loop.
!todo: Check the condition of the program for the second run for each ensemble to update model states
        tt = 1

    end subroutine

    subroutine perturb_run_init(fls, shd, cm)

        type(fl_ids) fls
        type(ShedGridParams) shd
        type(CLIM_INFO) cm

        integer NML, NA
        character(len = 5) strens

        NML = shd%lc%NML
        NA = shd%NA

        write(*, '(a)') ''
        write(*, "('Ensemble ', i5, ' of ', i5, ' (Pass ', i5, ')')") tt, N_ens, it_counter
        write(*, '(a)') ''

        !> Added by Ala Bahrami.
        !> Changing location of the input resume file.
        write(strens, '(i5)') tt
        if (.not. assim_mode .or. .not. RUN_DA) then
            fls%fl(mfk%f883)%fn = 'resume_ens/' // 'forecast/' // 'ens' // trim(adjustl(strens)) // '/' // fls%fl(mfk%f883)%fn
        else
            fls%fl(mfk%f883)%fn = 'resume_ens/' // 'analysis/' // 'ens' // trim(adjustl(strens)) // '/' // fls%fl(mfk%f883)%fn
        end if

        !> Added by Ala Bahrami.
        !> Updating the time counter starting date after one month simulation.
        if (month_counter > 1) then
            ic%start%jday = ic%start%jday + days_n
            if (ic%start%jday == 366 .or. ic%start%jday == 367) then
                ic%start%jday = 1
                ic%start%year = ic%start%year + 1
                days_n = 0
            end if
        end if

        !> Added by Ala Bahrami.
        !> Assigning variable related to perturbation fields.
        initialize = .false.
        n2 = 1

        if (allocated(Forcepert_vect)) deallocate(Forcepert_vect)
        if (allocated(precip_pert)) deallocate(precip_pert)
        if (allocated(sw_pert)) deallocate(sw_pert)
        if (allocated(lw_pert)) deallocate(lw_pert)
        if (allocated(swe_pert)) deallocate(swe_pert)
        if (allocated(thlq_pert)) deallocate(thlq_pert)
        allocate(Forcepert_vect(N_forcepert, NA, N_ens))
        allocate(precip_pert(NML, 1))
        allocate(sw_pert(NML, 1))
        allocate(lw_pert(NML, 1))
        allocate(swe_pert(NML, 1))
        allocate(thlq_pert(NML, 4))

        if (RUN_DA) then

            !> Variable to switch the program in forecast or assimilation mode.
            !> Note: If the program is in the assimilation mode this variable should be changed!
            if (it_counter >= 2) then
                it_counter = 3

                !> Reset the accumulated storage variable.
                stg_accum = 0
            else if (it_counter < 2) then
                assim_mode = .false.
            end if
        end if

        N_t = 0

    end subroutine

    subroutine perturb_run_pre_ts(fls, shd, cm)

        type(fl_ids) fls
        type(ShedGridParams) shd
        type(CLIM_INFO) cm

        integer NML, NA, k, i

        N_t = N_t + 1
        NML = shd%lc%NML
        NA = shd%NA

        !> Added by Ala Bahrami.
        if (RUN_DA) then

            !> Construct the model state matrix.
            if (.not. assim_mode) then
                call Julian2MonthDay(ic%now%jday, ic%now%year, nmth, ndy)
                if(ndy == 1) then
                    if (ic%now%hour .eq. 0 .and. ic%now%mins .eq. 0) then
                        X(1:NML, tt) = stas%cnpy%sncan
                        X((1*NML + 1):2*NML, tt) = stas%cnpy%rcan
                        X((2*NML + 1):3*NML, tt) = stas%sno%sno
                        X((3*NML + 1):4*NML, tt) = stas%sno%wsno
                        X((4*NML + 1):5*NML, tt) = stas%sfc%zpnd
!todo: Should consider the true number of soil layers (user-configurable).
                        X((5*NML + 1):6*NML, tt) = stas%sl%thlq(:, 1)
                        X((6*NML + 1):7*NML, tt) = stas%sl%thlq(:, 2)
                        X((7*NML + 1):8*NML, tt) = stas%sl%thlq(:, 3)
                        X((8*NML + 1):9*NML, tt) = stas%sl%thlq(:, 4)
                        X((9*NML + 1):10*NML, tt) = stas%sl%thic(:, 1)
                        X((10*NML + 1):11*NML, tt) = stas%sl%thic(:, 2)
                        X((11*NML + 1):12*NML, tt) = stas%sl%thic(:, 3)
                        X((12*NML + 1):13*NML, tt) = stas%sl%thic(:, 4)

                        !> Calculate the Kalman Gain Matrix.
                        if (tt .eq. N_ens) then
                            if (allocated(X2)) deallocate(X2)
                            if (allocated(X_ave)) deallocate(X_ave)
                            if (allocated(A)) deallocate(A)
                            if (allocated(X3)) deallocate(X3)
                            if (allocated(X4)) deallocate(X4)
                            if (allocated(KG)) deallocate(KG)
                            if (allocated(X5)) deallocate(X5)
                            allocate(X2(size(X, 1)))
                            allocate(X_ave(size(X, 1), N_ens))
                            allocate(A(size(X, 1), N_ens))
                            allocate(X3(N_ens, 1))
                            allocate(X4(size(X, 1), 1))
                            allocate(KG(size(X, 1), 1))
                            allocate(X5(1, 1))

                            !> Calculate ensemble average from ensemble members.
                            X2 = sum(X, 2)/N_ens
                            X_ave = spread(X2, 2, N_ens)

                            !> Calculate Ensemble Perturbations.
                            A = X - X_ave

                            !> Calculate the background error covariance indirectly.
                            !> Calculate local variable X3.
                            X3 = matmul(transpose(A), HT)

                            !> Calculate PHT which is assigned as X4.
                            X4 = matmul(A, X3)/(N_ens - 1)

                            !> Calculate X5.
                            X5 = 1.0/((matmul(H, X4) + GR_er**2.0))
!                            X6 = spread(X5, 1, size(X, 1))

                            !> Calculate Kalman Gain Matrix.
                            write(*, *) ''
                            write(*, *) 'Calculating Kalman Gain Matrix'
                            write(*, *) ''
                            KG = matmul(X4, X5)
                            open(10, file = 'Outputs/KG_20new.dat', action = 'write')
                            write(10, *) KG
                            close(10)
                            it_counter = it_counter + 1
!                            deallocate(X)
                        end if
                    end if
                end if
            end if

            !> Update the model states in the assimilation mode.
            if (assim_mode .and. ic%ts_daily == 48) then
!                write(*, *) ''
!                write(*, *) 'Model states are reinitialized in this stage'
!                write(*, *) ''
                stas%cnpy%sncan = stas%cnpy%sncan + AI(1:NML, tt)
                stas%cnpy%rcan = stas%cnpy%rcan + AI((1*NML + 1):2*NML, tt)

                !> Avoid updating SNO and WSNO for some problematic elements which create crashing problem in CLASSZ.
                do i = 1, NML
                    if (stas%sno%sno(i) <= 0.0) then
                        stas%sno%sno(i) = 0.0
                    else
                        stas%sno%sno(i) = stas%sno%sno(i) + AI((2*NML + i), tt)
                        if (stas%sno%sno(i) < 0.1) then
                            stas%sno%sno(i) = stas%sno%sno(i) - AI((2*NML + i), tt)
                        end if
                    end if
                    if (stas%sno%wsno(i) <= 0) then
                        stas%sno%wsno(i) = 0.0
                    else
                        stas%sno%wsno(i) = stas%sno%wsno(i) + AI((3*NML + i), tt)
                        if (stas%sno%wsno(i) < 0.0) then
                            stas%sno%wsno(i) = stas%sno%wsno(i) - AI((3*NML + i), tt)
                        end if
                    end if

                    !> Bring it back to the value before update.
!                    if (stas%sno%sno(i) < 0) then
!                        stas%sno%sno(i) = stas%sno%sno(i) - AI((2*NML + i), tt)
!                    end if
!                    if (stas%sno%wsno(i) < 0) then
!                        stas%sno%wsno(i) = stas%sno%wsno(i) - AI((3*NML + i), tt)
!                    end if
!todo: Should consider the true number of soil layers (user-configurable).
!                    if (stas%sl%thlq(i, 1) < 0) then
!                        stas%sl%thlq(i, 1) = stas%sl%thlq(i, 1) - AI((5*NML + i), tt)
!                    end if
!                    if (stas%sl%thlq(i, 2) < 0) then
!                        stas%sl%thlq(i, 2) = stas%sl%thlq(i, 2) - AI((6*NML + i), tt)
!                    end if
!                    if (stas%sl%thlq(i, 3) < 0) then
!                        stas%sl%thlq(i, 3) = stas%sl%thlq(i, 3) - AI((7*NML + i), tt)
!                    end if
!                    if (stas%sl%thlq(i, 4) < 0) then
!                        stas%sl%thlq(i, 4) = stas%sl%thlq(i, 4) - AI((8*NML + i), tt)
!                    end if
!                    if (stas%sl%thlc(i, 1) < 0) then
!                        stas%sl%thlc(i, 1) = stas%sl%thlc(i, 1) - AI((9*NML + i), tt)
!                    end if
!                    if (stas%sl%thlc(i, 2) < 0) then
!                        stas%sl%thlc(i, 2) = stas%sl%thlc(i, 2) - AI((10*NML + i), tt)
!                    end if
!                    if (stas%sl%thlc(i, 3) < 0) then
!                        stas%sl%thlc(i, 3) = stas%sl%thlc(i, 3) - AI((11*NML + i), tt)
!                    end if
!                    if (stas%sl%thlc(i, 4) < 0) then
!                        stas%sl%thlc(i, 4) = stas%sl%thlc(i, 4) - AI((12*NML + i), tt)
!                    end if
                end do
!                stas%sfc%zpnd = stas%sfc%zpnd + AI((4*NML + 1):5*NML, tt)
                stas%sl%thlq(:, 1) = stas%sl%thlq(:, 1) + AI((5*NML + 1):6*NML, tt)
                stas%sl%thlq(:, 2) = stas%sl%thlq(:, 2) + AI((6*NML + 1):7*NML, tt)
                stas%sl%thlq(:, 3) = stas%sl%thlq(:, 3) + AI((7*NML + 1):8*NML, tt)
                stas%sl%thlq(:, 4) = stas%sl%thlq(:, 4) + AI((8*NML + 1):9*NML, tt)
                stas%sl%thic(:, 1) = stas%sl%thic(:, 1) + AI((9*NML + 1):10*NML, tt)
                stas%sl%thic(:, 2) = stas%sl%thic(:, 2) + AI((10*NML + 1):11*NML, tt)
                stas%sl%thic(:, 3) = stas%sl%thic(:, 3) + AI((11*NML + 1):12*NML, tt)
                stas%sl%thic(:, 4) = stas%sl%thic(:, 4) + AI((12*NML + 1):13*NML, tt)
            end if
        end if

        !> Added by Ala Bahrami.
        !> Perturbing the input forcing data for every time step by considering
        !> the spatial-temporal and cross correlation.
        !> Note (Program speed): I see inside the sa_mesh_assimilate.f90 the get_forcepert()
        !> calculates Forcepert_rseed, Forcepert_ntrmdt, Forcepert for N_ens members for each time step,
        !> while sa_mesh_assimilate program use these variables for one only ensemble member.
        !> So, the number of calculation are multiplied by N_ens for each run of the program.
        !> This implementation slows down the process of running.

        !> Restoring the random field variables in the assimilation mode (analysis)
        !> similar perturbation fields of forecast.
        if (N_t == 1) then
            Forcepert_rseed = rseed_store
            Forcepert_ntrmdt = ntrmdt_store
        end if
        call get_forcepert( &
            N_forcepert, N_ens, N_x, N_y, &
            dx, dy, dtstep, &
            initialize, &
            Forcepert_param, &
            ens_id, &
            Forcepert_rseed, &
            Forcepert_ntrmdt, &
            Forcepert)

        !> Extracting Forcepert based on Rank.
        !> Note: I have modified the shd%yyy to be consistent with MATLAB code (the direction is considered up to down).
        do i = 1, NA
            Forcepert_vect(:, i, tt) = Forcepert(:, (shd%yCount - shd%yyy(i) + 1), shd%xxx(i), tt)
        end do

        !> Convert from the gridded value to tile.
        do k = 1, NML
!todo: Consider the cross correlation between different soil moisture layers. See Kumar article for more info.
!todo: Set up a condition to perturb model states based on temperature. See Forman et al. 2012 for more info.

            !> Forcing data perturbation fields.
!todo: Replace hard-coded indices with keys.
            precip_pert(k, 1) = Forcepert_vect(1, (shd%lc%ILMOS(k)), tt)
            sw_pert(k, 1) = Forcepert_vect(2, (shd%lc%ILMOS(k)), tt)
            lw_pert(k, 1) = Forcepert_vect(3, (shd%lc%ILMOS(k)), tt)

            !> Model states perturbation fields.
!+            swe_pert(k, 1) = Forcepert_vect(4, (shd%lc%ILMOS(k)), tt)
!todo: Should consider the true number of soil layers (user-configurable).
!+            thlq_pert(k, 1) = Forcepert_vect(5, (shd%lc%ILMOS(k)), tt)
!+            thlq_pert(k, 2) = Forcepert_vect(6, (shd%lc%ILMOS(k)), tt)
!+            thlq_pert(k, 3) = Forcepert_vect(7, (shd%lc%ILMOS(k)), tt)
!+            thlq_pert(k, 4) = Forcepert_vect(8, (shd%lc%ILMOS(k)), tt)

            !> Here the problematic days of any ensemble members are excluded from perturbation.
            !> A same methodology implemented to update model states should be applied.
!todo: Find out the reason of code crashing because of perturbation.
!-            if (ic%now%jday == 9 .or. ic%now%jday == 23 .or. ic%now%jday == 1) then
!-                if (tt == 6 .or. tt == 9 .or. tt == 15) then
!-                    cm%dat(3)%GAT(k) = cm%dat(3)%GAT(k)
!-                    cm%dat(1)%GAT(k) = cm%dat(1)%GAT(k)
!-                    cm%dat(2)%GAT(k) = cm%dat(2)%GAT(k)
!-                    stas%sno%sno(k) = stas%sno%sno(k)
!-                end if
!-            else
!todo: Replace hard-coded indices with keys.
                cm%dat(3)%GAT(k) = cm%dat(3)%GAT(k)*precip_pert(k, 1)
                cm%dat(1)%GAT(k) = cm%dat(1)%GAT(k)*sw_pert(k, 1)
                cm%dat(2)%GAT(k) = cm%dat(2)%GAT(k) + lw_pert(k, 1)
!+                stas%sno%sno(k) = stas%sno%sno(k)*swe_pert(k, 1)
!todo: Should consider the true number of soil layers (user-configurable).
!+                stas%sl%thlq(k, 1) = stas%sl%thlq(k, 1) + thlq_pert(k, 1)
!+                stas%sl%thlq(k, 2) = stas%sl%thlq(k, 2) + thlq_pert(k, 2)
!+                stas%sl%thlq(k, 3) = stas%sl%thlq(k, 3) + thlq_pert(k, 3)
!+                stas%sl%thlq(k, 4) = stas%sl%thlq(k, 4) + thlq_pert(k, 4)
!-            end if

            !> Constrain perturbed longwave radiation.
!todo: Replace hard-coded indices with keys.
            if (cm%dat(2)%GAT(k) < 100.0) then
                cm%dat(2)%GAT(k) = 100.0
            end if

            !> Constrain perturbed shortwave radiation.
!todo: Replace hard-coded indices with keys.
            if (cm%dat(1)%GAT(k) > 1500.0) then
                cm%dat(1)%GAT(k) = 1500.0
            end if
        end do

    end subroutine

    subroutine perturb_run_post_ts(fls, shd, cm)

        type(fl_ids) fls
        type(ShedGridParams) shd
        type(CLIM_INFO) cm

        integer NML, j, i

        NML = shd%lc%NML

        !> Added by Ala Bahrami.
        !> Calculate statistics of the run.
        if (RUN_DA) then

            !> Accumulate the daily storage value.
!todo: Check why the output storage does not correspond with saved CSV file.
            if (ic%ts_daily == 48 .and. .not. assim_mode) then
!                write(*, *) ''
!                write(*, *) 'ic', ic%now%jday, ic%now%hour, ic%now%mins
!                write(*, *) ''
                stg_accum(1, tt) =  stg_accum(1, tt) + sum( &
					(stas_grid%cnpy%rcan + stas_grid%cnpy%sncan + stas_grid%sno%sno + stas_grid%sno%wsno + stas_grid%sfc%pndw + &
					 sum(stas_grid%sl%lqws, 2) + sum(stas_grid%sl%fzws, 2) + &
					 stas_grid%lzs%ws + stas_grid%dzs%ws)*shd%FRAC)/sum(shd%FRAC)
!                write(*, *) ''
!                write(*, *) 'stg_accum(1,tt)', stg_accum(1, tt)
!                write(*, *) ''
            end if

            !> Calculate and save the ensemble innovation vector if the next day will be a new month.
            if (ic%ts_daily == 48 .and. .not. assim_mode) then
                call Julian2MonthDay((ic%now%jday + 1), ic%now%year, nmth, ndy)
                if (ndy == 1) then
                    call Julian2MonthDay(ic%now%jday, ic%now%year, nmth, ndy)
                    HX(1, tt) = stg_accum(1, tt)/ndy
!                    write(*, *) ''
!                    write(*, *) 'HX(1,tt)', HX(1, tt)
!                    write(*, *) ''
                    D(1, tt) = GR_Pert(tt, month_counter) - HX(1, tt)
                    open(11, file = 'Outputs/Innovation_20_Monthly_new.ini')
                    write(11, *) D(1, tt)
                    close(11)
                end if
            end if
        end if

        !> Added by Ala Bahrami.
        !> Calculate the analysis increment and activate the assimilation (analysis) mode.
        if (tt == N_ens .and. it_counter == 2) then

            !> Activate the assimilation mode if the next day will be a new month.
            call Julian2MonthDay((ic%now%jday + 1), ic%now%year, nmth, ndy)
            if (ndy == 1 .and. ic%ts_daily == 48) then

                !> Change to assimilation mode.
                assim_mode = .true.

                !> Calculate the analysis increment.
                write(*, *) ''
                write(*, *) 'Calculating Analysis Increment'
                write(*, *) ''
                AI = matmul(KG, D)

                !> Calculate and write the fraction of analysis increment for each day of the month.
                call Julian2MonthDay(ic%now%jday, ic%now%year, nmth, ndy)
                AI = AI/ndy
                open(13, file = 'Outputs/AI_20_new.ini')
                do i = 1, 13*NML
                    write(13, *) (AI(i, j), j = 1, N_ens)
                end do
                close(13)
            end if
        end if

    end subroutine

    subroutine perturb_run_finalize(fls, shd, cm)

        type(fl_ids) fls
        type(ShedGridParams) shd
        type(CLIM_INFO) cm

        integer iun

        if ((assim_mode .or. .not. RUN_DA) .and. tt .eq. N_ens) then

            !> Update counters.
            if (RUN_DA) then
                it_counter = 1
                assim_mode = .false.
            end if
            month_counter = month_counter + 1

            !> Extract the number of days of the previous month to update the start date.
            call Julian2MonthDay((ic%now%jday - 1), ic%now%year, nmth, ndy)

            !> Accumulate the number of days passed from the beginning of the program.
            days_n = days_n + ndy

            if (SAVERESUMEFLAG_per == 1) then

                !> Reshape and write Forcepert_ntrmdt.
                Forcepert_ntrmdt_store = reshape(Forcepert_ntrmdt, (/N_forcepert, N_x*N_y*N_ens/))
                iun = 963
                open(iun, file = 'Inputs/Forcepert_ntrmdt.seq', action = 'write', form = 'unformatted', access = 'sequential')
                write(iun) Forcepert_ntrmdt_store(1, :)
                write(iun) Forcepert_ntrmdt_store(2, :)
                write(iun) Forcepert_ntrmdt_store(3, :)
!todo: Writing the other field members.
!+                write(iun) Forcepert_ntrmdt_store(4, :)
!+                write(iun) Forcepert_ntrmdt_store(5, :)
!+                write(iun) Forcepert_ntrmdt_store(6, :)
!+                write(iun) Forcepert_ntrmdt_store(7, :)
!+                write(iun) Forcepert_ntrmdt_store(8, :)
                close(iun)

                !> Reshape and write Forcepert_rseed.
                Forcepert_rseed_store = reshape(Forcepert_rseed, (/NRANDSEED2*N_ens, 1/))
                iun = 963
                open(iun, file = 'Inputs/Forcepert_rseed.seq', action = 'write', form = 'unformatted', access = 'sequential')
                write(iun) Forcepert_rseed_store
                close(iun)
            end if

            if (.not. RUN_DA) then
                RESUMEFLAG_per = 1
            end if
        end if

    end subroutine

end module
