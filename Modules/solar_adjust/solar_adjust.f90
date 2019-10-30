!> Description:
!>  Calculate theoretical shortwave radiation using method proposed by
!>  Garnier and Ohmura (1970) to adjust for slope and aspect.
!>
!> Author: Zelalem Tesemma (of Solar_Adjust.m; last updated Jan 30, 2018)
!> Converted to Fortran: Feb 1, 2018 (exact)
!> Fortran code optimized/consolidated; subroutine isolated
!>  ('program' component replaced by 'solar_adjust_module'): Feb 2, 2018.
!>
!*  rsrd_adjusted: Adjusted incoming short wave radiation. [W m-2].
!*  rlds_adjusted: Adjusted incoming long wave radiation. [W m-2].
!*  temp_adjusted: Adjusted air temperature. [K].
!*  pres_adjusted: Adjusted barometric pressure. [Pa].
!*  humd_adjusted: Adjusted specific humidity. [kg kg-1].
!*  rain_adjusted: Adjusted precipitation. [mm s-1].
!*  wind_adjusted: Adjusted wind speed. [m s-1].

subroutine calc_rsrd_adjusted( &
    elev, xlng, ylat, slope, aspect, delta, &
    nvals, Time_Zone, CalcFreq, rsrd_dtmin, rlds_dtmin, &
    temp_dtmin, pres_dtmin, humd_dtmin, rain_dtmin, wind_dtmin, &
    rsrd, rlds, temp, pres, humd, rain, wind, rsrd_adjusted, &
    rlds_adjusted, temp_adjusted, pres_adjusted, humd_adjusted, &
    rain_adjusted, wind_adjusted, now_year, now_month, &
    now_jday, now_hour, now_mins, dtmins)

    implicit none

    !> Constants and conversion factors.
    real, parameter :: GCons = 9.80616                 !gravitational constant in m s-2 taken from CLASS V3.6 Mannual Page 31
    real, parameter :: RCons = 287.04                  !gas constant for dry air in J kg-1 K-1 taken from CLASS V3.6 Mannual Page 31
    real, parameter :: pi = 3.14159265
    real, parameter :: DEGtoRAD = pi/180.0
    real, parameter :: DEGtoRAD365 = 2.0*pi/365.0
    real, parameter :: EQTCons = (pi/180.0)*(360.0/365.0) ! A constant that will be used for the calculation of the equation of time.


! !* Option 1: Tables of temperature lapse rate derived from the high resolution (2.5km by 2.5km) GEM run for the period Oct, 2016 - Sept, 2019

      real tlapse(12), plapse(12), vapcoeff(12), lwlapse(12)

      DATA tlapse / 5.458, 5.783, 5.591, 6.939, 8.082, 7.907, &
                    6.632, 6.584, 6.094, 6.689, 7.060, 5.776 /

      DATA plapse / 0.35, 0.35, 0.35, 0.30, 0.25, 0.20, &
                    0.20, 0.20, 0.20, 0.25, 0.30, 0.35 /

      DATA vapcoeff / 0.41, 0.42, 0.40, 0.39, 0.38, 0.36, &
                      0.33, 0.33, 0.36, 0.37, 0.40, 0.40 /

      DATA lwlapse / 7.29, 8.85, 4.84, 18.37, 32.23, 28.99, &
                     36.34, 33.58, 25.99, 13.16, 0.54, 14.28 /

! ! !* Option 2: Tables of temperature lapse rate, vapor pressure coefficient (Kunkel et al., 1989),and precipitation–elevation adjustment factors(Thornton et al.1997) for each months from Northern Hemisphere.

      ! real tlapse(12), plapse(12), vapcoeff(12)
      ! DATA tlapse / 4.40, 5.90, 7.10, 7.80, 8.10, 8.20, &
                ! 8.10, 8.10, 7.70, 6.80, 5.50, 4.70 /
      ! DATA plapse / 0.35, 0.35, 0.35, 0.30, 0.25, 0.20, &
                ! 0.20, 0.20, 0.20, 0.25, 0.30, 0.35 /
      ! DATA vapcoeff / 0.41, 0.42, 0.40, 0.39, 0.38, 0.36, &
                  ! 0.33, 0.33, 0.36, 0.37, 0.40, 0.40 /

    !> Parameters.
    !*  Time_Zone: The time zone of the study area (+ to East and - to West).
    !*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
    real, intent(in) :: Time_Zone
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
    !*  rain: precipitation. [mm s-1].
    !*  wind: wind speed. [m s-1].
    !*  xlng: Longitude. [degrees].
    !*  ylat: Latitude. [degrees].
    !*  gru_elev: Weighted average elevation of GRUs. [m].
    !*  gru_delta: Weighted average elevation difference between GEM and MESH. [m].
    !*  gru_slope: Weighted average slope of the surface. [--].
    !*  gru_aspect: Weighted average aspect of the surface.
    integer, intent(in) :: nvals, rsrd_dtmin, rlds_dtmin, temp_dtmin, &
                           pres_dtmin, humd_dtmin, rain_dtmin, &
                           wind_dtmin, now_year, now_month, now_jday, &
                           now_hour, now_mins, dtmins

    real, dimension(nvals), intent(in) :: rsrd, rlds, temp, pres, &
                                          humd, rain, wind
    real, dimension(nvals), intent(in) :: elev, xlng, ylat, slope, &
                                          aspect, delta

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   integer, dimension(4) :: zkt = (/ 767, 768, 770, 773/)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !> Output variables.
    !*  rsrd_adjusted, rlds_adjusted, temp_adjusted, pres_adjusted, humd_adjusted: Adjusted incoming shortwave radiation for slope and aspect. [W m-2].
    !*  Adjusted air temperature. [K],  Adjusted barometric pressure. [Pa] & Adjusted specific humidity. [kg kg-1] for elevation difference.
        real, dimension(nvals), intent(out) :: rsrd_adjusted
        real, dimension(nvals), intent(out) :: rlds_adjusted
        real, dimension(nvals), intent(out) :: temp_adjusted
        real, dimension(nvals), intent(out) :: pres_adjusted
        real, dimension(nvals), intent(out) :: humd_adjusted
        real, dimension(nvals), intent(out) :: rain_adjusted
        real, dimension(nvals), intent(out) :: wind_adjusted

    !> Working variables.
    integer MINS_int
    real EQTVar, EQT, Dec, Rad_vec, Sol, Cdec, Sdec, Trans
    real, dimension(nvals) :: &
        Clat, Slat, x, y, z, SHVar, SH, Hr_Ang, t1, t2, t10, t20, &
        Czen, ACzen, oam, diff, Iterr, cosxs0, cosxsL, Idir, &
        Idiff, Sum_Idir, Sum_Diff, Sum_Flatd, Sum_Flatf, Qdirect, &
        Qdiffuse, Qflat, Tnew, Told, esold, esnew, eaold, eanew, &
        rhold, Tdew_old, Tdew_new, epsilon_corr, rain_frac, windcorr

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
    Trans = 0.818 - (0.064*sin(2.0*pi*REAL(now_jday - 90)/365.0))        !*  Trans: Mean transmissivity of the atmosphere.

    !> Hour Angle calculation in now_jday day and the Hour Angle varies between -pi and pi and it is 0 at solar noon.
    EQTVar = EQTCons*REAL(now_jday - 81)
    EQT = 9.87*sin(2.0*EQTVar) - 7.53*cos(EQTVar) - 1.50*sin(EQTVar)     ! the equation of time from SunAngle, http://www.susdesign.com/sunangle/
!   SHVar = REAL(60.0*REAL(now_hour))                                    ! to run it on hourly bases
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
!    do kk = 1, NINT(REAL(rsrd_dtmin)/REAL(MINS_int))                   ! to run it on hourly bases
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!! Other climate variables correction for elevation differences !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!* Option 1: Precipitation Conserved
    rain_adjusted = rain

!* Option 2: Precipitation correction using elevation based lapse  (Thornton, 1997)
   ! rain_frac = plapse(now_month)*delta/1000.0
   ! rain_adjusted = rain*(1.0 + rain_frac)/(1.0 - rain_frac)

!* Option 1: Linear lapse rates (measured, seasonal, constant, neutral stability) using the table provided above.
    temp_adjusted = temp - (delta*tlapse(now_month)/1000.0)

! !* Option 2: Based on uniform monthly lapse rate of 6.0 K/km (Bernier et al. 2011) and it can be moved into MESH run file for calibration.
   ! temp_adjusted = temp + (delta*(-6.0)/1000.0)

!* Pressure correction for elevation differences.
    pres_adjusted = pres*exp(-(delta*GCons)/(RCons*temp_adjusted))

!* Option 1: Specific humidity correction for elevation difference by conserving the relative humidity
   Told = temp - 273.16                                                ! Convert temperature from (Kelvin) to (degree C)
   Tnew = temp_adjusted - 273.16                                       ! Convert temperature from (Kelvin) to (degree C)
   eaold = humd*pres/(0.378*humd + 0.622)
   where (Told < 0.0)
       esold = 611.0*exp(21.874*Told/(Told + 265.5))                   ! Used from CLASS V3.6 Mannual, page 79
   elsewhere
       esold = 611.0*exp(17.269*Told/(Told + 237.3))                   ! Used from CLASS V3.6 Mannual, page 79
   end where

   rhold = 100.0*eaold / esold

   where (Tnew < 0.0)
       esnew = 611.0*exp(21.874*Tnew/(Tnew + 265.5))                   ! Used from CLASS V3.6 Mannual, page 79
   elsewhere
       esnew = 611.0*exp(17.269*Tnew/(Tnew + 237.3))                   ! Used from CLASS V3.6 Mannual, page 79
   end where
   eanew = rhold*esnew/100.0
   humd_adjusted = 0.622*eanew/(pres_adjusted - 0.378*eanew)

! !* Option 2: Specific humidity correction for elevation difference using dew point temperature (Kunkel, 1989)
   ! Told = temp - 273.16                                              !* Convert temperature from (Kelvin) to (degree C).
   ! Tnew = temp_adjusted - 273.16                                     !* Convert temperature from (Kelvin) to (degree C).
   ! eaold = humd*pres/(0.378*humd + 0.622)

   ! where (Told .LE. 0.0)
   ! Tdew_old = 272.55*LOG(eaold/611.15)/(22.452 - LOG(eaold/611.15))  ! Buck, 1981 for temperature less than or equal to zero.
   ! Tdew_new = Tdew_old - (delta*vapcoeff(now_month)*272.55/22.452)/1000.0
   ! eanew = 611.15*exp(22.452*Tdew_new/(Tdew_new + 272.55))
   ! elsewhere
   ! Tdew_old = 240.97*LOG(eaold/611.21)/(17.502 - LOG(eaold/611.21))  ! Buck, 1981 for temperatures above 0 °C.
   ! Tdew_new = Tdew_old - (delta*vapcoeff(now_month)*240.97/17.502)/1000.0
   ! eanew = 611.21*exp(17.502*Tdew_new/(Tdew_new + 240.97))
   ! end where
   ! humd_adjusted = 0.622*eanew/(pres_adjusted - 0.378*eanew)

!* Option 1: Incoming longwave  solar radiation correction for topography based on Stefan-Boltzmann law and Satterlund’s (1979) formula
   epsilon_corr = ((100*eanew/temp_adjusted)**(1/7))/((100*eaold/temp)**(1/7))
   rlds_adjusted = rlds*epsilon_corr*(temp_adjusted/temp)**4

! !* Option 2: Based on developed lapsrate using GEM 2.5 km or (Marty et al. 2002) value of 2.9 W/m^2 / 100 m can be used.
   ! rlds_adjusted = rlds - (delta*lwlapse(now_month)/1000.0)

!* Option 1: Precipitation Conserved
   wind_adjusted = wind

! !* Option 2: Using The wind weighting factor, used to modify the wind speed is given by Liston and Sturm (1998)
!  windcorr = (1.0 + ((2.0*delta/10000.0)*exp(-3.0*(elev - delta)/10000.0)))
!  wind_adjusted = wind * windcorr

    return

end subroutine
