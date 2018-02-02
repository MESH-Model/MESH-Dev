!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>  Calls "calc_rsrd_adjusted".
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!> Converted to Fortran: Feb 1, 2018 (exact)
!> Fortran code optimized/consolidated; subroutine isolated
!>  ('program' component replaced by 'solar_adjust_module'): Feb 2, 2018.
program Solar_Adjust

    implicit none

    !> File units.
    integer, parameter :: FSIN_IUN = 200, FSDIRECT_IUN = 500, FSDIFFUSE_IUN = 501

    !> Input variables.
    integer NVALS, NSTEPS
    real, dimension(:), allocatable :: ELEV, YLAT, SLOPE, ASPECT
    integer :: syyyy = 2004, sdd = 245, shh = 0 ! Start times

    !> Input/output variables.
    !* rsrd_dtmin: Time-step of radiation data. [minutes].
    !* rsrd: Incoming shortwave radiation (input).
    !* rsrd_adjusted: Adjusted incoming shortwave radiation.
    !* rsrd_direct: Direct component of adjusted radiation.
    !* rsrd_diffuse: Diffuse component of adjusted radiation.
    integer :: rsrd_dtmin = 60
    real, dimension(:), allocatable :: rsrd
    real, dimension(:), allocatable :: &
        rsrd_direct, rsrd_diffuse, rsrd_adjusted

    !> Local variables.
    integer k, m, n
    integer now_year, now_day, now_hour
    integer ierr

    !> Read indices from the 'index' file.
    !> 'nvals' is the product of the number of rows and colums read from the 'index' file.
    open(100, file = 'index.txt', status = 'old', action = 'read')
    read(100, *) m, n, nsteps
    close(100)
    nvals = m*n

    !> Allocate variables.
    allocate( &
        elev(nvals), ylat(nvals), slope(nvals), aspect(nvals), &
        rsrd(nvals), &
        rsrd_direct(nvals), rsrd_diffuse(nvals), rsrd_adjusted(nvals))

    !> Read elevation and latitude inputs.
    open(100, file = 'Grid_elev.txt', status = 'old', action = 'read')
    read(100, *) (elev(n), n = 1, nvals)
    close(100)
    open(100, file = 'Grid_lat.txt', status = 'old', action = 'read')
    read(100, *) (ylat(n), n = 1, nvals)
    close(100)

    !> Optional input variables.
    !> Set to zero for now.
    slope = 0.0
    aspect = 0.0

    !> Open persistent files to read/write input/output radiation.
    open(FSIN_IUN, file = 'rsrd.txt', status = 'old', action = 'read')
    open(FSDIRECT_IUN, file = 'rsrd_direct_new.txt', action = 'write')
    open(FSDIFFUSE_IUN, file = 'rsrd_diffuse_new.txt', action = 'write')

    !> Set current time.
    now_year = syyyy; now_day = sdd; now_hour = shh

    !> Loop until forced exist (e.g., by end of input data).
    do while (.true.)

        !> Read incoming shortwave radiation (input).
        read(FSIN_IUN, *, iostat = ierr) (rsrd(n), n = 1, nvals)
        if (ierr /= 0) exit

        !> Call routine to calculate adjusted radiation value.
        call calc_rsrd_adjusted( &
            elev, ylat, slope, aspect, nvals, &
            rsrd_dtmin, &
            rsrd, rsrd_direct, rsrd_diffuse, rsrd_adjusted, &
            now_year, now_day, now_hour)

        !> Write output.
        write(FSDIRECT_IUN, '(9999(f10.3, 1x))') (rsrd_direct(n), n = 1, nvals)
        write(FSDIFFUSE_IUN, '(9999(f10.3, 1x))') (rsrd_diffuse(n), n = 1, nvals)

        !> Increment the time.
        now_hour = now_hour + 1
        if (now_hour > 23) then
            print *, now_year, now_day
            now_day = now_day + 1; now_hour = 0
            if (now_day >= 366) then
                if (mod(now_year, 400) == 0) then !LEAP YEAR
                    if (now_day == 367) then
                        now_day = 1
                        now_year = now_year + 1
                    end if
                else if (mod(now_year, 100) == 0) then !NOT A LEAP YEAR
                    now_day = 1
                    now_year = now_year + 1
                else if (mod(now_year, 4) == 0) then !LEAP YEAR
                    if (now_day == 367) then
                        now_day = 1
                        now_year = now_year + 1
                    end if
                else !NOT A LEAP YEAR
                    now_day = 1
                    now_year = now_year + 1
                end if
            end if
        end if

    end do

end program
