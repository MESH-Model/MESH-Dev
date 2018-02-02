!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!> Converted to Fortran: Feb 2, 2018 (exact)
program Solar_Adjust

    implicit none

    !> Constants.
    real, parameter :: pi = 3.141592653589793
    real, parameter :: DEGtoRAD = pi/180.0
    real, parameter :: DEGtoRAD365 = 2.0*pi/365.0
    real, parameter :: CalcFreq = 288.0
    real, parameter :: RADxxMIN = 2.0*pi/CalcFreq
    real, parameter :: MINS_int = 24.0*60.0/CalcFreq
    real, parameter :: Trans = 0.818 ! mean transmissivity of the atmosphere

    !> File units.
    integer, parameter :: FSIN_IUN = 200, FSDIRECT_IUN = 500, FSDIFFUSE_IUN = 501

    !> Input variables.
    integer NROWS, NCOLS, NSTEPS
    real, dimension(:, :), allocatable :: ELEV, LAT, SLOPE, ASPECT

    !> Local variables.
    integer m, n

    !> Input files.
    open(100, file = 'index.txt', status = 'old', action = 'read')
    read(100, *) nrows, ncols, nsteps
    close(100)
    allocate(elev(nrows, ncols), lat(nrows, ncols), slope(nrows, ncols), aspect(nrows, ncols))
    open(100, file = 'Grid_elev.txt', status = 'old', action = 'read')
    read(100, *) ((elev(m, n), n = 1, ncols), m = 1, nrows)
    close(100)
    open(100, file = 'Grid_lat.txt', status = 'old', action = 'read')
    read(100, *) ((lat(m, n), n = 1, ncols), m = 1, nrows)
    close(100)

    !> Optional input variables.
    slope = 0.0
    aspect = 0.0

    !> Input files (persistent).
    open(FSIN_IUN, file = 'rsrd.txt', status = 'old', action = 'read')

    !> Output files (persistent).
    open(FSDIRECT_IUN, file = 'rsrd_direct_new.txt', action = 'write')
    open(FSDIFFUSE_IUN, file = 'rsrd_diffuse_new.txt', action = 'write')

    call calc_correction(elev, lat, slope, aspect, nrows, ncols, nsteps)

    contains

    subroutine calc_correction(dem, Lat, slope, aspect, nrows, ncols, nsteps)

        !> Input variables.
        real, dimension(:, :), intent(in) :: dem ! elevation grid (m)
        real, dimension(:, :), intent(in) :: Lat ! latitude of the elevation grid
        real, dimension(:, :), intent(in) :: slope
        real, dimension(:, :), intent(in) :: aspect
        integer, intent(in) :: nrows, ncols, nsteps

        !> Local variables.
        integer syyyy, sdd, shh ! Start times
        integer k, kk, m, n, yyyy, dd, hh
        integer Day
        real Time_Offset
        real, dimension(nrows, ncols, nsteps) :: rsrd ! shortwave radiation input
        real, dimension(nrows, ncols, nsteps) :: rsrd_direct, rsrd_diffuse
        real, dimension(nrows, ncols) :: &
            Clat, Slat, x, y, z, t1, t2, t10, t20, &
            Czen, ACzen, oam, diff, &
            Sum_Id, Sum_Diff, Sum_Flatd, Sum_Flatf, &
            It, cosxs0, cosxsL, Id, diffuse
        real, dimension(nrows, ncols, nsteps) :: Qsi, Qdirect, Qdiffuse, Qflat
        real Hr_Ang, Dec, Rad_vec, Sol, Cdec, Sdec

        Time_Offset = 0.67 ! solar time offset from local time
        Clat = cos(Lat*DEGtoRAD)
        Slat = sin(Lat*DEGtoRAD)
        x = -cos(aspect*DEGtoRAD)*sin(slope*DEGtoRAD)
        y = sin(aspect*DEGtoRAD)*sin(slope*DEGtoRAD) ! compute constant
        z = cos(slope*DEGtoRAD) ! components of cos(x^s)

        syyyy=2004; sdd=245; shh=0
        yyyy=syyyy; dd=sdd; hh=shh
        Hr_Ang = -pi*(1.0 + Time_Offset/12.0)

        do k = 1, nsteps ! hourly loop over the study period

            read(FSIN_IUN, *) ((rsrd(m, n, k), n = 1, ncols), m = 1, nrows)

            Day = dd ! day of the calendar year
            Dec = sin((Day - 81)*DEGtoRAD365)*0.40928 ! declination
            Rad_vec = 0.01676*cos(pi - 0.017262*(Day - 3)) + 1 ! radius vector of the earth'S orbit
            Sol = 0.0819/(Rad_vec*Rad_vec) ! solar constant  mj/m**2*min or 117.936 mj/m**2*day
            Cdec = cos(Dec)
            Sdec = sin(Dec)
            t1 = (x*Slat + z*Clat)*Cdec
            t2 = (-x*Clat + z*Slat)*Sdec
            t10 = Clat*Cdec
            t20 = Slat*Sdec
            Sum_Id = 0.0
            Sum_Diff = 0.0
            Sum_Flatd = 0.0
            Sum_Flatf = 0.0

            do kk = 1, 12
                Czen = Cdec*Clat*cos(Hr_Ang) + Sdec*Slat ! cos of zenith angle
                ACzen = acos(Czen)
                oam = abs(1.0/(Czen + (0.50572*(96.07995 - ACzen)**(-1.6364)))) ! oam by secant approximation from Kasten and Young (1989)

                do m = 1, nrows
                    do n = 1, ncols
                        if (oam(m, n) < 2.9) then
                            oam(m, n) = oam(m, n)
                        else if (oam(m, n) < 16.38) then ! zenith < 86.5 deg
                            diff(m, n) = 10**(2.247*log10(oam(m, n)) - 2.104)
                            oam(m, n) = oam(m, n) - diff(m, n)
                        else if (oam(m, n) <= 114.6) then ! zenith < 89.5 deg
                            diff(m, n) = 10**(1.576*log10(oam(m, n)) - 1.279)
                            oam(m, n) = oam(m, n) - diff(m, n)
                        else
                            oam(m, n) = 30 ! computed
                        end if
                    end do
                end do
                oam = oam*(((CalcFreq - 0.0065*dem)/CalcFreq)**5.256) ! correction according to (List 1984)

                do m = 1, nrows
                    do n = 1, ncols
                        if (Czen(m, n) > 0) then
                            It(m, n) = MINS_int*Sol*Czen(m, n) ! extra-ter. rad for MINS_int minute interval
                            cosxs0 = t10*cos(Hr_Ang)
                            cosxs0 = cosxs0 + t20

                            !> horizontal
                            if (cosxs0(m, n) > 0.0) then ! not in shadow
                                Id(m, n) = MINS_int*Sol*cosxs0(m, n)*(Trans**oam(m, n)) ! direct rad. for MINS_int minute interval
                                diffuse(m, n) = 0.5*(0.91*It(m, n) - Id(m, n)) ! Diffuse radiation on horizontal
                                Sum_Flatd(m, n) = Sum_Flatd(m, n) + Id(m, n)
                                Sum_Flatf(m, n) = Sum_Flatf(m, n) + diffuse(m, n)
                                cosxsL = -y*sin(Hr_Ang)*Cdec + t1*cos(Hr_Ang)
                                cosxsL = cosxsL + t2
                                if (cosxsL(m, n) > 0.0) then ! slope not in shadow
                                    Id(m, n) = MINS_int*Sol*cosxsL(m, n)*(Trans**oam(m, n)) ! direct rad. for MINS_int minute interval
                                    Sum_Id(m, n) = Sum_Id(m, n) + Id(m, n)
                                end if
                                diffuse(m, n) = diffuse(m, n)*(cos(slope(m, n)/2.0))**2.0 ! on slope
                                Sum_Diff(m, n) = Sum_Diff(m, n) + diffuse(m, n)
                            end if
                        end if
                    end do
                end do
                Hr_Ang = Hr_Ang + RADxxMIN
            end do

            Qdirect(:, :, k) = (1000000.0/3600.0)*Sum_Id ! clear-sky direct radiation on slope (MJ/m^2.int to W/m^2)
            Qdiffuse(:, :, k) = (1000000.0/3600.0)*Sum_Diff ! clear-sky diffuse radiation on slope (MJ/m^2.int to W/m^2)
            Qflat(:, :, k)= (1000000.0/3600.0)*(Sum_Flatd + Sum_Flatf) ! clear-sky 'Qdirect + Qdiffuse' on horizontal surface (MJ/m^2.int to W/m^2)

            hh = hh + 1
            if (hh > 23) then
                print *, yyyy, dd
                dd = dd + 1; hh = 0
                Hr_Ang = -pi*(1.0 + Time_Offset/12.0)
                if (dd >= 366) then
                    if (mod(yyyy, 400) == 0) then !LEAP YEAR
                        if (dd == 367) then
                            dd = 1
                            yyyy = yyyy + 1
                        end if
                    else if (mod(yyyy, 100) == 0) then !NOT A LEAP YEAR
                        dd = 1
                        yyyy = yyyy + 1
                    else if (mod(yyyy, 4) == 0) then !LEAP YEAR
                        if (dd == 367) then
                            dd = 1
                            yyyy = yyyy + 1
                        end if
                    else !NOT A LEAP YEAR
                        dd = 1
                        yyyy = yyyy + 1
                    end if
                end if
            end if
!        end do

        !> CRHM radiation correction for slope (the slope module of CHRM)
        Qsi = rsrd ! GEM sortwave radiation input for MESH
!        do k = 1, nsteps
            do m = 1, nrows
                do n = 1, ncols
                    if (Qflat(m, n, k) > 1.0) then
                        rsrd_direct(m, n, k) = (Qsi(m, n, k)/Qflat(m, n, k))*Qdirect(m, n, k)
                        rsrd_diffuse(m, n, k) = (Qsi(m, n, k)/Qflat(m, n, k))*Qdiffuse(m, n, k)
                    else
                        rsrd_direct(m, n, k) = 0.0
                        rsrd_diffuse(m, n, k) = 0.0
                    end if
                end do
            end do

            write(FSDIRECT_IUN, '(9999(f10.3, 1x))') ((rsrd_direct(m, n, k), n = 1, ncols), m = 1, nrows)
            write(FSDIFFUSE_IUN, '(9999(f10.3, 1x))') ((rsrd_diffuse(m, n, k), n = 1, ncols), m = 1, nrows)

        end do

    end subroutine

end program
