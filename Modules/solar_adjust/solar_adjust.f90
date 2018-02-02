!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!> Converted to Fortran: Feb 1, 2018 (exact)
!> Fortran code optimized/consolidated; subroutine isolated
!>  ('program' component replaced by 'solar_adjust_module'): Feb 2, 2018.
!>
!> Input variables:
!*  nvals: Number of elements in the vector (e.g., 1:grids, 1:tiles, etc.). [--].
!*  elev: Elevation. [m].
!*  ylat: Latitude. [degrees].
!*  slope: Slope of surface. [--].
!*  aspect: Aspect of surface.
!*  rsrd_dtmin: Time-step of radiation data. [minutes].
!*  rsrd: Incoming shortwave radiation (input). [W m-2].
!*  now_year: Present year. [--].
!*  now_jday: Present day in year. [--].
!*  now_hour: Present hour in day (00-23). [--].
!>
!> Returns:
!*  rsrd_adjusted: Adjusted incoming shortwave radiation. [W m-2].
!*  rsrd_direct: Direct component of adjusted radiation. [W m-2].
!*  rsrd_diffuse: Diffuse component of adjusted radiation. [W m-2].
subroutine calc_rsrd_adjusted( &
    elev, ylat, slope, aspect, nvals, &
    rsrd_dtmin, &
    rsrd, rsrd_direct, rsrd_diffuse, rsrd_adjusted, &
    now_year, now_jday, now_hour)

    implicit none

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
    !*  nvals: Number of elements in the vector (e.g., 1:grids, 1:tiles, etc.). [--].
    !*  rsrd_dtmin: Time-step of radiation data. [minutes].
    !*  now_year: Present year. [--].
    !*  now_jday: Present day in year. [--].
    !*  now_hour: Present hour in day (00-23). [--].
    !*  rsrd: Incoming shortwave radiation (input). [W m-2].
    !*  elev: Elevation. [m].
    !*  ylat: Latitude. [degrees].
    !*  slope: Slope of surface. [--].
    !*  aspect: Aspect of surface.
    integer, intent(in) :: nvals, rsrd_dtmin, now_year, now_jday, now_hour
    real, dimension(nvals), intent(in) :: rsrd
    real, dimension(nvals), intent(in) :: elev, ylat, slope, aspect

    !> Output variables.
    !*  rsrd_adjusted: Adjusted incoming shortwave radiation. [W m-2].
    !*  rsrd_direct: Direct component of adjusted radiation. [W m-2].
    !*  rsrd_diffuse: Diffuse component of adjusted radiation. [W m-2].
    real, dimension(nvals), intent(out) :: rsrd_direct, rsrd_diffuse, rsrd_adjusted

    !> Working variables.
    integer MINS_int
    real RADxxMIN, Hr_Ang, Dec, Rad_vec, Sol, Cdec, Sdec
    real, dimension(nvals) :: &
        Clat, Slat, x, y, z, t1, t2, t10, t20, &
        Czen, ACzen, oam, diff, &
        Iterr, cosxs0, cosxsL, Idir, Idiff, Sum_Idir, Sum_Diff, Sum_Flatd, Sum_Flatf, &
        Qsi, Qdirect, Qdiffuse, Qflat

    !> Local variables.
    integer kk

    !> Constant over time.
    Clat = cos(ylat*DEGtoRAD)
    Slat = sin(ylat*DEGtoRAD)
    x = -cos(aspect*DEGtoRAD)*sin(slope*DEGtoRAD)
    y = sin(aspect*DEGtoRAD)*sin(slope*DEGtoRAD) ! compute constant
    z = cos(slope*DEGtoRAD) ! components of cos(x^s)

    !> Time-stepping (for integral).
    MINS_int = 24*60/CalcFreq

    !> Time-stepping (for inclination in day).
    Hr_Ang = -pi*(1.0 + Time_Offset/12.0) + (2.0*pi)*rsrd_dtmin/(24*60)*now_hour

    !> Calculate geometry.
    Dec = sin((now_jday - 81)*DEGtoRAD365)*0.40928 ! declination
    Rad_vec = 0.01676*cos(pi - 0.017262*(now_jday - 3)) + 1 ! radius vector of the earth'S orbit
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
        oam = oam*(((CalcFreq - 0.0065*elev)/CalcFreq)**5.256) ! correction according to (List 1984)
        where (Czen > 0)
            Iterr = MINS_int*Sol*Czen ! extra-ter. rad for MINS_int minute interval
            cosxs0 = t10*cos(Hr_Ang)
            cosxs0 = cosxs0 + t20

            !> Horizontal.
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
        Hr_Ang = Hr_Ang + 2.0*pi/CalcFreq
    end do

    !> Convert units.
    Qdirect = (1000000.0/3600.0)*Sum_Idir ! clear-sky direct radiation on slope (MJ/m^2.int to W/m^2)
    Qdiffuse = (1000000.0/3600.0)*Sum_Diff ! clear-sky diffuse radiation on slope (MJ/m^2.int to W/m^2)
    Qflat = (1000000.0/3600.0)*(Sum_Flatd + Sum_Flatf) ! clear-sky 'Qdirect + Qdiffuse' on horizontal surface (MJ/m^2.int to W/m^2)

    !> CRHM radiation correction for slope (the slope module of CHRM).
    Qsi = rsrd ! GEM sortwave radiation input for MESH
    where (Qflat > 1.0)
        rsrd_direct = (Qsi/Qflat)*Qdirect
        rsrd_diffuse = (Qsi/Qflat)*Qdiffuse
    elsewhere
        rsrd_direct = 0.0
        rsrd_diffuse = 0.0
    end where
    rsrd_adjusted = rsrd_direct + rsrd_diffuse

    return

end subroutine
