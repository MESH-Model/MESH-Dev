!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!> Converted to Fortran: Feb 1, 2018 (exact)
!> Fortran code optimized/consolidated; subroutine isolated
!>  ('program' component replaced by 'solar_adjust_module'): Feb 2, 2018.
!>
!> Parameters (options):
!*  Trans: Mean transmissivity of the atmosphere.
!*  Time_Zone: The time zone of the study area.
!*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
!>
!*  rsrd_adjusted: Adjusted incoming shortwave radiation. [W m-2].
!*  rlds_adjusted: Adjusted incoming longwave radiation. [W m-2].
!*  temp_adjusted: Adjusted air temperature. [K].
!*  pres_adjusted: Adjusted barometric pressure. [Pa].
!*  humd_adjusted: Adjusted specific humidity. [kg kg-1].
subroutine calc_rsrd_adjusted( &
    elev, xlng, ylat, slope, aspect, delta, &
    nvals, Trans, Time_Zone, CalcFreq, &
    rsrd_dtmin, rlds_dtmin, temp_dtmin, pres_dtmin, humd_dtmin, &
    rsrd, rlds, temp, pres, humd, rsrd_adjusted, rlds_adjusted, &
    temp_adjusted, pres_adjusted, humd_adjusted, &
    now_year, now_jday, now_hour, now_mins, dtmins)

    implicit none

    !> Constants and conversion factors.
    real, parameter :: GCons = 9.81                 !gravitational constant in m s−2
    real, parameter :: RCons = 287.05               !gas constant for dry air in J kg-1 K-1
    real, parameter :: pi = dacos(-1.d0)
    real, parameter :: DEGtoRAD = pi/180.0
    real, parameter :: DEGtoRAD365 = 2.0*pi/365.0
    real, parameter :: EQTCons = (pi/180.0)*(360.0/365.0) ! A constant that will be used for the calculation of the equation of time.

    !> Parameters.
    !*  Trans: Mean transmissivity of the atmosphere.
    !*  Time_Zone: The time zone of the study area (+ to East and - to West).
    !*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
    real, intent(in) :: Trans, Time_Zone
    integer, intent(in) :: CalcFreq

    !> Input variables.
    !*  nvals: Number of elements in the vector (e.g., 1:grids, 1:tiles, etc.). [--].
    !*  rsrd_dtmin: Time-step of incoming shortwave radiation data. [minutes].
    !*  rlds_dtmin: Time-step of incoming longwave radiation data. [minutes].
    !*  now_year: Present year. [--].
    !*  now_jday: Present day in year. [--].
    !*  now_hour: Present hour in day (00-23). [--].
    !*  now_mins: Present minutes in hour (00, 30). [--].
    !*  dtmins: Model time-step. [minutes].
    !*  rsrd: Incoming shortwave radiation (input). [W m-2].
    !*  rlds: Incoming longwave radiation (input). [W m-2].
    !*  temp: air temperature (input). [K].
    !*  pres: barometric pressure (input). [Pa].
    !*  humd: specific humidity (input). [kg kg-1].
    !*  xlng: Longitude. [degrees].
    !*  ylat: Latitude. [degrees].
    !*  gru_elev: Weighted average elevation of GRUs. [m].
    !*  gru_delta: Weighted average elevation difference between GEM and MESH. [m].
    !*  gru_slope: Weighted average slope of the surface. [--].
    !*  gru_aspect: Weighted average aspect of the surface.
    integer, intent(in) :: nvals, rsrd_dtmin, rlds_dtmin, temp_dtmin, &
                           pres_dtmin, humd_dtmin, now_year, now_jday, &
                           now_hour, now_mins, dtmins

    real, dimension(nvals), intent(in) :: rsrd, rlds, temp, pres, humd
    real, dimension(nvals), intent(in) :: elev, xlng, ylat, slope, &
                                          aspect, delta

    !> Output variables.
    !*  rsrd_adjusted, rlds_adjusted, temp_adjusted, pres_adjusted, humd_adjusted: Adjusted incoming shortwave radiation for slope and aspect. [W m-2].
    !*  Adjusted air temperature. [K],  Adjusted barometric pressure. [Pa] & Adjusted specific humidity. [kg kg-1] for elevation difference.
    real, dimension(nvals), intent(out) :: rsrd_adjusted
    real, dimension(nvals), intent(out) :: rlds_adjusted
    real, dimension(nvals), intent(out) :: temp_adjusted
    real, dimension(nvals), intent(out) :: pres_adjusted
    real, dimension(nvals), intent(out) :: humd_adjusted

    !> Working variables.
    integer MINS_int
    real EQTVar, EQT, Dec, Rad_vec, Sol, Cdec, Sdec
    real, dimension(nvals) :: &
        Clat, Slat, x, y, z, SHVar, SH, Hr_Ang, t1, t2, t10, t20, &
        Czen, ACzen, oam, diff, &
        Iterr, cosxs0, cosxsL, Idir, Idiff, Sum_Idir, Sum_Diff, &
        Sum_Flatd, Sum_Flatf, Qdirect, Qdiffuse, Qflat, &
        Tnew, Told, esnew, esold, eaold, eanew, rhold

    !> Local variables.
    integer kk

    !> Constant over time.
    Clat = cos(ylat*DEGtoRAD)
    Slat = sin(ylat*DEGtoRAD)
    x = -cos(aspect*DEGtoRAD)*sin(slope*DEGtoRAD)
    y = sin(aspect*DEGtoRAD)*sin(slope*DEGtoRAD)                        ! compute constant
    z = cos(slope*DEGtoRAD)                                             ! components of cos(x^s)

    !> Calculate geometry.
    Dec = sin(REAL(now_jday - 81)*DEGtoRAD365)*0.40928                  ! declination
    Rad_vec = 0.01676*cos(pi - 0.017262*REAL(now_jday - 3)) + 1.0       ! radius vector of the earth'S orbit
    Sol = 0.0819/(Rad_vec*Rad_vec)                                      ! solar constant  mj/m**2*min or 117.936 mj/m**2*day
    Cdec = cos(Dec)
    Sdec = sin(Dec)
    t1 = (x*Slat + z*Clat)*Cdec
    t2 = (-x*Clat + z*Slat)*Sdec
    t10 = Clat*Cdec
    t20 = Slat*Sdec

    !> Hour Angle calculation in now_jday day and the Hour Angle varies between -pi and pi and it is 0 at solar noon.
    EQTVar = EQTCons*REAL(now_jday - 81)
    EQT = 9.87*sin(2.0*EQTVar) - 7.53*cos(EQTVar) - 1.50*sin(EQTVar)    ! the equation of time from SunAngle, http://www.susdesign.com/sunangle/
!    SHVar = REAL(60.0*REAL(now_hour))                                   ! to run it on hourly bases
    SHVar = REAL(NINT(60.0*REAL(now_hour)) + now_mins)
    SHVar = SHVar + EQT + (4.0*xlng) - (60.0*Time_Zone)
    SH = MODULO(SHVar, 1440.0)
    SH = SH/4.0
    where (SH < 0.0)
        Hr_Ang = DEGtoRAD*(SH + 180.0)
    elsewhere
        Hr_Ang = DEGtoRAD*(SH - 180.0)
    end where

    !> Time-stepping (for integral).
    MINS_int = NINT(24.0*60.0/REAL(CalcFreq))

    !> Accumulate radiative fluxes.
    Sum_Idir = 0.0; Sum_Diff = 0.0; Sum_Flatd = 0.0; Sum_Flatf = 0.0
!    do kk = 1, NINT(REAL(rsrd_dtmin)/REAL(MINS_int))                    ! to run it on hourly bases
    do kk = 1, NINT(REAL(dtmins)/REAL(MINS_int))                        ! set to MESH run Time-stepping
        Czen = Cdec*Clat*cos(Hr_Ang) + Sdec*Slat                        ! cos of zenith angle
        ACzen = acos(Czen)/DEGtoRAD
        oam = abs(1.0/(Czen + (0.50572*(96.07995 - ACzen)**(-1.6364)))) ! oam by secant approximation from Kasten and Young (1989)
        where (oam <= 38.0868)                                          ! zenith <= 90 deg
            oam = oam
        elsewhere
            oam = 38.0868
        end where

        oam = oam*(((288.0 - 0.0065*elev)/288.0)**5.256)                ! correction according to (List 1984)
        where (Czen > 0.0)
            Iterr = REAL(MINS_int)*Sol*Czen                             ! extra-ter. rad for MINS_int minute interval
            cosxs0 = t10*cos(Hr_Ang)
            cosxs0 = cosxs0 + t20

            !> Horizontal.
            where (cosxs0 > 0.0)                                        ! not in shadow
                Idir = REAL(MINS_int)*Sol*cosxs0*(Trans**oam)           ! direct rad. for MINS_int minute interval
                Idiff = 0.5*(0.91*Iterr - Idir)                         ! diffuse radiation on horizontal
                Sum_Flatd = Sum_Flatd + Idir
                Sum_Flatf = Sum_Flatf + Idiff
                cosxsL = -y*sin(Hr_Ang)*Cdec + t1*cos(Hr_Ang)
                cosxsL = cosxsL + t2
             where (cosxsL > 0.0)                                       ! slope not in shadow
                Idir = REAL(MINS_int)*Sol*cosxsL*(Trans**oam)           ! direct rad. for MINS_int minute interval
                Sum_Idir = Sum_Idir + Idir
             end where
                Idiff = Idiff*((cos(slope/2.0))**2.0)                   ! on slope
                Sum_Diff = Sum_Diff + Idiff
             end where
        end where
        Hr_Ang = Hr_Ang + (2.0*pi/REAL(CalcFreq))
    end do

    !> Convert units.
    Qdirect = (1000000.0/(REAL(dtmins)*60.0))*Sum_Idir                  ! clear-sky direct radiation on slope (MJ/m^2.int to W/m^2)
    Qdiffuse = (1000000.0/(REAL(dtmins)*60.0))*Sum_Diff                 ! clear-sky diffuse radiation on slope (MJ/m^2.int to W/m^2)
    Qflat = (1000000.0/(REAL(dtmins)*60.0))*(Sum_Flatd + Sum_Flatf)     ! clear-sky 'Qdirect + Qdiffuse' on horizontal surface (MJ/m^2.int to W/m^2)

    !> CRHM radiation correction for slope (the slope module of CHRM).
    where (Qflat > 0.1)
        rsrd_adjusted = (rsrd/Qflat)*(Qdirect + Qdiffuse)
    elsewhere
        rsrd_adjusted = 0.0
    end where

    rlds_adjusted = rlds + (delta*-2.9/100.0)                           ! change 2.9 W/m^2 / 100 m is lapse rate (Marty et al. 2002) and should be moved into MESH run file for calibration.

    temp_adjusted = temp + (delta*-6.0/1000.0)                          ! change 6.0 K/km is lapse rate (Bernier et al. 2011) and should be moved into MESH run file for calibration.

    pres_adjusted = pres*exp(-(delta*GCons)/(RCons*temp_adjusted))

    Told = temp - 273.16
    Tnew = temp_adjusted - 273.16                                       ! Convert temperature from (Kelvin) to (degree C)

    eaold = humd*pres/(0.378*humd + 0.622)

    where (Told > 0.0)
        esold = 610.78*exp(17.27*Told / (Told + 237.3))                 ! Monteith and Unsworth (2008) provide Tetens' formula for temperatures above 0 ?C
    elsewhere
        esold = 610.78*exp(21.875*Told / (Told + 265.5))                ! Murray (1967) for temperature less than or equal to zero
    end where

    rhold = 100.0*eaold / esold

    where (Tnew > 0.0)
        esnew = 610.78*exp(17.27*Tnew / (Tnew + 237.3))
    elsewhere
        esnew = 610.78*exp(21.875*Tnew / (Tnew + 265.5))
    end where

    eanew = rhold*esnew / 100.0

    humd_adjusted = 0.622*eanew/(pres_adjusted - 0.378*eanew)

    return

end subroutine
