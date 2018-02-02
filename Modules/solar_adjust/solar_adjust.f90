!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!> Converted to Fortran: Feb 1, 2018 (exact)
!> Fortran code optimized/consolidated: Feb 2, 2018.
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
        call calc_correction( &
            elev, ylat, slope, aspect, nvals, & !nsteps,
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

    contains

    subroutine calc_correction( &
        elev, ylat, slope, aspect, nvals, & !nsteps,
        rsrd_dtmin, &
        rsrd, rsrd_direct, rsrd_diffuse, rsrd_adjusted, &
        now_year, now_day, now_hour)

        !> Constants and conversion factors.
        real, parameter :: pi = 3.141592653589793
        real, parameter :: DEGtoRAD = pi/180.0
        real, parameter :: DEGtoRAD365 = 2.0*pi/365.0

        !> Parameters.
        !* Trans: Mean transmissivity of the atmosphere.
        !* Time_Offset: Solar time offset from local time.
        real :: Trans = 0.818
        real :: Time_Offset = 0.67
        integer :: CalcFreq = 288

        !> Input variables.
        real, dimension(:), intent(in) :: elev ! elevation grid (m)
        real, dimension(:), intent(in) :: ylat ! latitude of the elevation grid
        real, dimension(:), intent(in) :: slope
        real, dimension(:), intent(in) :: aspect

!temp
        integer, intent(in) :: nvals !, nsteps
!        integer k, m, n
!        integer :: syyyy = 2004, sdd = 245, shh = 0 ! Start times
        integer, intent(in) :: now_year, now_day, now_hour

        !> Input/output variables.
!temp: dimensions
        !* rsrd_dtmin: Time-step of radiation data. [minutes].
        !* rsrd: Incoming shortwave radiation (input).
        !* rsrd_adjusted: Adjusted incoming shortwave radiation.
        !* rsrd_direct: Direct component of adjusted radiation.
        !* rsrd_diffuse: Diffuse component of adjusted radiation.
        integer, intent(in) :: rsrd_dtmin
        real, dimension(nvals), intent(in) :: rsrd
        real, dimension(nvals), intent(out) :: &
            rsrd_direct, rsrd_diffuse, rsrd_adjusted

        !> Working variables.
!replaced with now_day        integer Day
        integer MINS_int
        real RADxxMIN, Hr_Ang, Dec, Rad_vec, Sol, Cdec, Sdec
        real, dimension(nvals) :: &
            Clat, Slat, x, y, z, t1, t2, t10, t20, &
            Czen, ACzen, oam, diff, &
            Iterr, cosxs0, cosxsL, Idir, Idiff, Sum_Idir, Sum_Diff, Sum_Flatd, Sum_Flatf
        real, dimension(nvals) :: Qsi, Qdirect, Qdiffuse, Qflat

        !> Local variables.
        integer kk

!temp
!        now_year = syyyy; now_day = sdd; now_hour = shh
!        rsrd_dtmin = 60

        !> Constant over time.
        Clat = cos(ylat*DEGtoRAD)
        Slat = sin(ylat*DEGtoRAD)
        x = -cos(aspect*DEGtoRAD)*sin(slope*DEGtoRAD)
        y = sin(aspect*DEGtoRAD)*sin(slope*DEGtoRAD) ! compute constant
        z = cos(slope*DEGtoRAD) ! components of cos(x^s)

        !> Time-stepping (for integral).
!        RADxxMIN = 2.0*pi/CalcFreq
        MINS_int = 24*60/CalcFreq

        !> Time-stepping (for inclination in day).
!replaced with calculation, if left to iterate then the minimal time-step for subroutine would have to be daily
!        Hr_Ang = -pi*(1.0 + Time_Offset/12.0)

!temp: loop
!        do k = 1, nsteps ! hourly loop over the study period

            !> Time-stepping (for inclination in day).
            Hr_Ang = -pi*(1.0 + Time_Offset/12.0) + (2.0*pi)*rsrd_dtmin/(24*60)*now_hour

            !> Calculate geometry.
!replaced with now_day            Day = dd ! day of the calendar year
            Dec = sin((now_day - 81)*DEGtoRAD365)*0.40928 ! declination
            Rad_vec = 0.01676*cos(pi - 0.017262*(now_day - 3)) + 1 ! radius vector of the earth'S orbit
            Sol = 0.0819/(Rad_vec*Rad_vec) ! solar constant  mj/m**2*min or 117.936 mj/m**2*day
            Cdec = cos(Dec)
            Sdec = sin(Dec)
            t1 = (x*Slat + z*Clat)*Cdec
            t2 = (-x*Clat + z*Slat)*Sdec
            t10 = Clat*Cdec
            t20 = Slat*Sdec

            !> Accumulate radiative fluxes.
            Sum_Idir = 0.0; Sum_Diff = 0.0; Sum_Flatd = 0.0; Sum_Flatf = 0.0
            do kk = 1, (rsrd_dtmin/MINS_int)
                Czen = Cdec*Clat*cos(Hr_Ang) + Sdec*Slat ! cos of zenith angle
                ACzen = acos(Czen)
                oam = abs(1.0/(Czen + (0.50572*(96.07995 - ACzen)**(-1.6364)))) ! oam by secant approximation from Kasten and Young (1989)

!                do m = 1, nrows
!                    do n = 1, ncols
                        where (oam < 2.9)
                            oam = oam
                        elsewhere (oam < 16.38) ! zenith < 86.5 deg
                            diff = 10**(2.247*log10(oam) - 2.104)
                            oam = oam - diff
                        elsewhere (oam <= 114.6) ! zenith < 89.5 deg
                            diff = 10**(1.576*log10(oam) - 1.279)
                            oam = oam - diff
                        elsewhere
                            oam = 30 ! computed
                        end where
!                    end do
!                end do
                oam = oam*(((CalcFreq - 0.0065*elev)/CalcFreq)**5.256) ! correction according to (List 1984)

!                do m = 1, nrows
!                    do n = 1, ncols
                        where (Czen > 0)
                            Iterr = MINS_int*Sol*Czen ! extra-ter. rad for MINS_int minute interval
                            cosxs0 = t10*cos(Hr_Ang)
                            cosxs0 = cosxs0 + t20

                            !> horizontal
                            where (cosxs0 > 0.0) ! not in shadow
                                Idir = MINS_int*Sol*cosxs0*(Trans**oam) ! direct rad. for MINS_int minute interval
                                Idiff = 0.5*(0.91*Iterr - Idir) ! diffuse radiation on horizontal
                                Sum_Flatd = Sum_Flatd + Idir
                                Sum_Flatf = Sum_Flatf + Idiff
                                cosxsL = -y*sin(Hr_Ang)*Cdec + t1*cos(Hr_Ang)
                                cosxsL = cosxsL + t2
                                where (cosxsL > 0.0) ! slope not in shadow
                                    Idir = MINS_int*Sol*cosxsL*(Trans**oam) ! direct rad. for MINS_int minute interval
                                    Sum_Idir = Sum_Idir + Idir
                                end where
                                Idiff = Idiff*(cos(slope/2.0))**2.0 ! on slope
                                Sum_Diff = Sum_Diff + Idiff
                            end where
                        end where
!                    end do
!                end do
                Hr_Ang = Hr_Ang + 2.0*pi/CalcFreq
            end do

            !> Convert units.
            Qdirect = (1000000.0/3600.0)*Sum_Idir ! clear-sky direct radiation on slope (MJ/m^2.int to W/m^2)
            Qdiffuse = (1000000.0/3600.0)*Sum_Diff ! clear-sky diffuse radiation on slope (MJ/m^2.int to W/m^2)
            Qflat = (1000000.0/3600.0)*(Sum_Flatd + Sum_Flatf) ! clear-sky 'Qdirect + Qdiffuse' on horizontal surface (MJ/m^2.int to W/m^2)

            !> Increment time-step information.
!            now_hour = now_hour + 1
!            if (now_hour > 23) then
!                print *, now_year, now_day
!                now_day = now_day + 1; now_hour = 0
!                if (now_day >= 366) then
!                    if (mod(now_year, 400) == 0) then !LEAP YEAR
!                        if (now_day == 367) then
!                            now_day = 1
!                            now_year = now_year + 1
!                        end if
!                    else if (mod(now_year, 100) == 0) then !NOT A LEAP YEAR
!                        now_day = 1
!                        now_year = now_year + 1
!                    else if (mod(now_year, 4) == 0) then !LEAP YEAR
!                        if (now_day == 367) then
!                            now_day = 1
!                            now_year = now_year + 1
!                        end if
!                    else !NOT A LEAP YEAR
!                        now_day = 1
!                        now_year = now_year + 1
!                    end if
!                end if

                !> Time-stepping (for inclination in day).
!replaced with calculation, if left to iterate then the minimal time-step for subroutine would have to be daily
!                Hr_Ang = -pi*(1.0 + Time_Offset/12.0)
!            end if
!        end do

!temp
!            read(FSIN_IUN, *) ((rsrd(m, n, k), n = 1, nvals)

        !> CRHM radiation correction for slope (the slope module of CHRM)
        Qsi = rsrd ! GEM sortwave radiation input for MESH
!        do k = 1, nsteps
!            do m = 1, nrows
!                do n = 1, ncols
                    where (Qflat > 1.0)
                        rsrd_direct = (Qsi/Qflat)*Qdirect
                        rsrd_diffuse = (Qsi/Qflat)*Qdiffuse
                    elsewhere
                        rsrd_direct = 0.0
                        rsrd_diffuse = 0.0
                    end where
!                end do
!            end do
        rsrd_adjusted = rsrd_direct + rsrd_diffuse

!temp
!            write(FSDIRECT_IUN, '(9999(f10.3, 1x))') ((rsrd_direct(m, n, k), n = 1, nvals)
!            write(FSDIFFUSE_IUN, '(9999(f10.3, 1x))') ((rsrd_diffuse(m, n, k), n = 1, nvals)

!        end do

    end subroutine

end program
