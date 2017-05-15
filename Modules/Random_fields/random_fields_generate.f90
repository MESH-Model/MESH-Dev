program main

    ! The purpose of this subroutine is to generate zero-mean, unit variance time series
    ! of N_forcepert 2d perturbation fields
    !
    ! Author : Ala Bahrami
    !
    ! Date of creation : 01/03/2017
    !
    ! Last modified : 21/03/2017
    ! Adding the output of the Forcepert into three output array of 'tmp_precip.dat' ,
    !  'tmp_swdn.dat' & 'tmp_lwdn.dat'. I also added the timing stamp
    ! I intend to read forcing data from MESH model

    use land_force_perturb
    use forcepert_types
    use file_variables

    implicit none

    ! ------------------------------------------------------------
    ! forcing parameters
    type(forcepert_param_type), dimension(:), allocatable :: forcepert_param

    ! ------------------------------------------------------------
    ! Calling the time stamp
    write (*, '(a)') ''
    write (*, '(a)') ' Testing the execution: Random field generating'
    write (*, '(a)') ''

    write (*, '(a)') ''
    call cpu_time(startprog)


    ! ------------------------------------------------------------
    ! Reading the input file
    open (10, file = 'Input2.ini')
        read (10 , *) dx
        read (10 , *) dy
        read (10 , *) N_x
        read (10 , *) N_y
        read (10 , *) lambda_x
        read (10 , *) lambda_y
        read (10 , *) variance
        read (10 , *) RSEEDCONST
        read (10 , *) NRANDSEED2
        read (10 , *) N_forcepert
        read (10 , *) N_ens
        read (10 , *) dtstep
        read (10 , *) tcorr
        read (10 , *) N_t

    close (10)

    ! ------------------------------------------------------------
    ! open files for output

    ! open(991, file='tmp_precip.dat', form='formatted', status='unknown')
    ! open(992, file='tmp_swdn.dat',   form='formatted', status='unknown')
    ! open(993, file='tmp_lwdn.dat',   form='formatted', status='unknown')

    ! ------------------------------------------------------------
    ! Initialize the variables
    call assemble_forcepert_param(N_x, N_y, N_forcepert, forcepert_param)

    allocate(ens_id(N_ens))

    allocate(Forcepert_rseed(NRANDSEED2 , N_ens))

    allocate(Forcepert_ntrmdt(N_forcepert , N_x , N_y , N_ens))
    allocate(Forcepert(       N_forcepert , N_x , N_y , N_ens))

    Forcepert_ntrmdt = 0.    ! initialize just in case (should not be needed)
    Forcepert        = 0.    ! initialize just in case (should not be needed)

    initialize = .true.

    do n = 1 , N_ens
            ens_id(n) = n
    end do

    ! ------------------------------------------------------------
    ! Generate random fields for initialization

    call get_forcepert(                                      &
       N_forcepert, N_ens, N_x, N_y,                       &
       dx, dy, dtstep,                                     &
       initialize,                                         &
       forcepert_param,                                    &
       ens_id,                                             &
       Forcepert_rseed,                                    &
       Forcepert_ntrmdt,                                   &
       Forcepert                )

    ! output of the initilized field


    ! do m = 1 , N_forcepert
        ! do n = 1 , N_ens
            ! do  i = 1 , N_x

                            ! !write (990 + m, '(3312(e13.5))') (Forcepert(m , i , j , n), j = ((n -1) * N_y + 1 ), (n * N_y))
                            ! write (990 + m, '(3312(e13.5))') (Forcepert(m , i , j , n), j =  1 ,  N_y)

            ! end do
        ! end do
    ! end do

    write (*,*) Forcepert(:,1,1,1)


    ! ------------------------------------------------------------
    ! loop through time

    initialize = .false.

    ! Applying for N_t times

    do tt = 1 , N_t

         call get_forcepert(                                      &
              N_forcepert, N_ens, N_x, N_y,                       &
              dx, dy, dtstep,                                     &
              initialize,                                         &
              forcepert_param,                                    &
              ens_id,                                             &
              Forcepert_rseed,                                    &
              Forcepert_ntrmdt,                                   &
              Forcepert                )

         ! output
         ! Todo: Convert the Forcepert from gridded output to vector based on the ranks

        ! do m = 1 , N_forcepert
            ! do n = 1 , N_ens
                ! do  i = 1 , N_x

                            ! !write (990 + m, '(3312(e13.5))') (Forcepert(m , i , j , n), j = ((n -1) * N_y + 1 ), (n * N_y))
                             ! write (990 + m, '(3312(e13.5))') (Forcepert(m , i , j , n), j = 1 ,  N_y)

                ! end do
            ! end do
        ! end do

         write (*,*) Forcepert(:,1,1,1)

    end do

    ! close (991)
    ! close (992)
    ! close (993)

    ! ------------------------------------------------------------
    ! Ending of the program
    write (*, '(a)') ''
    write (*, '(a)') 'Ending the execution: Random field generating'
    write (*, '(a)') ''

    call cpu_time(endprog)
    write(*, "('Time = ', e14.6, ' seconds.')") (endprog - startprog)

    ! --------------------------------------------------------------
    ! Format of data
    !58 format (2x, g14.6)

end
