!> Description:
!>  Module to calculate theoretical shortwave radiation using methods
!>  proposed by Garnier and Ohmura (1970) to adjust for elevation, slope
!>  and aspect. The module updates precipitation, temperature, specific
!>  humidity, pressure, wind speed, and incoming longwave radiation,
!>  accounting for elevation difference relative to NWP reference and
!>  topographic effect.
!>
!> Author:
!>  Zelalem Tesemma (initially based on Solar_Adjust.m; last updated Jan 30, 2018)
!>
!> Notes:
!>  - 2018/02/01: Converted to Fortran (exact copy)
!>  - 2018/02/02: Fortran code optimized/consolidated
!>      ('program' component replaced by 'solar_adjust_module')
!>  - 2019/10/10: Upgraded into Mountain MESH (renamed 'forcing_adjust')
!>  - 2020/04/16: Added phased precipitation adjustment.
!>
!> Input variables:
!*  elev: Weighted average elevation of GRUs. [m].
!*  xlng: Longitude. [degrees].
!*  ylat: Latitude. [degrees].
!*  slope: Weighted average slope of the surface. [--].
!*  aspect: Weighted average aspect of the surface.
!*  delta: Weighted average elevation difference between GEM and MESH. [m].
!*  curvature: Weighted average curvature of the surface.
!*  nvals: Number of elements in the vector (e.g., 1:grids, 1:tiles, etc.). [--].
!*  nml_grid_map: Lookup table of grid ID from tile ID. [--].
!*  i1: ID of first grid active on current processing node. [--].
!*  i2: ID of last grid active on current processing node. [--].
!*  Time_Zone: The time zone of the study area (+ to East and - to West).
!*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
!*  rsrd_dtmin: Time-step of incoming shortwave radiation data. [minutes].
!*  rlds_dtmin: Time-step of incoming longwave radiation data. [minutes].
!*  temp_dtmin: Time-step of air temperature. [minutes].
!*  pres_dtmin: Time-step of barometric pressure. [minutes].
!*  humd_dtmin: Time-step of specific humidity. [minutes].
!*  rain_dtmin: Time-step of precipitation. [minutes].
!*  wind_dtmin: Time-step of wind speed. [minutes].
!*  winddir_dtmin: Time-step of wind direction. [minutes].
!*  rsrd: Incoming shortwave radiation (input). [W m-2].
!*  rlds: Incoming longwave radiation (input). [W m-2].
!*  temp: Air temperature (input). [K].
!*  pres: Barometric pressure (input). [Pa].
!*  humd: Specific humidity (input). [kg kg-1].
!*  rain: Precipitation. [mm s-1].
!*  wind: Wind speed. [m s-1].
!*  winddir: Wind direction. [degree].
!*  now_year: Present year. [--].
!*  now_month: Present month. [--].
!*  now_jday: Present day in year. [--].
!*  now_hour: Present hour in day (00-23). [--].
!*  now_mins: Present minutes in hour (00, 30). [--].
!*  dtmins: Model time-step. [minutes].
!>
!> Output variables:
!*  rsrd_adjusted: Adjusted incoming short wave radiation. [W m-2].
!*  rlds_adjusted: Adjusted incoming long wave radiation. [W m-2].
!*  temp_adjusted: Adjusted air temperature. [K].
!*  pres_adjusted: Adjusted barometric pressure. [Pa].
!*  humd_adjusted: Adjusted specific humidity. [kg kg-1].
!*  rain_adjusted: Adjusted precipitation. [mm s-1].
!*  rain_phased_adjusted: Adjusted liquid component of precipitation. [mm s-1].
!*  snow_phased_adjusted: Adjusted solid component of precipitation. [mm s-1].
!*  wind_adjusted: Adjusted wind speed. [m s-1].
subroutine forcing_adjust( &
    elev, xlng, ylat, slope, aspect, delta, curvature, nvals, &
    nml_grid_map, i1, i2, &
    Time_Zone, CalcFreq, rsrd_dtmin, rlds_dtmin, temp_dtmin, &
    pres_dtmin, humd_dtmin, rain_dtmin, wind_dtmin, winddir_dtmin, &
    rsrd, rlds, temp, pres, humd, rain, wind, winddir, &
    rsrd_adjusted, rlds_adjusted, temp_adjusted, pres_adjusted, &
    humd_adjusted, rain_adjusted, rain_phased_adjusted, &
    snow_phased_adjusted, wind_adjusted, &
    now_year, now_month, now_jday, now_hour, now_mins, dtmins)

    implicit none

    !> Constants and conversion factors.
    real, parameter :: GCons = 9.80616                 ! Gravitational constant in m s-2 taken from CLASS V3.6 Mannual Page 31
    real, parameter :: RCons = 287.04                  ! Gas constant for dry air in J kg-1 K-1 taken from CLASS V3.6 Mannual Page 31
    real, parameter :: pi = 3.14159265
    real, parameter :: DEGtoRAD = pi/180.0
    real, parameter :: DEGtoRAD365 = 2.0*pi/365.0
    real, parameter :: gammaC = 0.5
    real, parameter :: gammaS = 0.5

    !> Input variables.
    real, intent(in) :: Time_Zone
    integer, intent(in) :: CalcFreq
    integer, dimension(nvals), intent(in) :: nml_grid_map
    integer, intent(in) :: i1, i2
    integer, intent(in) :: &
        nvals, rsrd_dtmin, rlds_dtmin, temp_dtmin, &
        pres_dtmin, humd_dtmin, rain_dtmin, wind_dtmin, &
        winddir_dtmin, now_year, now_month, now_jday, &
        now_hour, now_mins, dtmins
    real, dimension(nvals), intent(in) :: &
        rsrd, rlds, temp, pres, &
        humd, rain, wind, winddir
    real, dimension(nvals), intent(in) :: &
        elev, xlng, ylat, slope, &
        aspect, delta, curvature

!    integer, dimension(6) :: zkt = (/ 98, 99, 100, 101, 102, 103 /)

!    real, dimension(12) :: tlapse, plapse, dtlapse, lwlapse, wlapse
    real tlapse(12), plapse(12), dtlapse(12), lwlapse(12), wlapse(12)

    !> Option 1:
    !>  Tables of mean annual lapse rate derived from the high
    !>  resolution (2.5km by 2.5km) GEM run for the period Oct, 2016 to
    !>  Sept, 2019.
!    data plapse / 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30, 0.30 /
!    data tlapse / 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60, 6.60 /
!    data dtlapse / 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92, 2.92 /
!    data lwlapse / 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35, 18.35 /
!    data wlapse / 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21, 0.21 /

    !> Option 2:
    !>  Tables of monthly lapse rate derived from the high resolution
    !>  (2.5km by 2.5km) GEM run for the period Oct, 2016 to Sept, 2019.
    data plapse / 0.516, 0.306, 0.420, 0.263, 0.084, 0.164, 0.158, 0.219, 0.206, 0.461, 0.528, 0.342 /
    data tlapse / 5.479, 5.830, 5.683, 6.991, 8.107, 7.940, 6.700, 6.648, 6.177, 6.729, 7.080, 5.791 /
    data dtlapse / 1.986, 2.567, 1.941, 2.892, 2.747, 3.267, 4.834, 3.802, 3.257, 3.145, 2.505, 2.126 /
    data lwlapse / 6.832, 8.647, 4.803, 17.993, 31.842, 28.492, 36.013, 33.234, 25.388, 12.674, 0.288, 13.972 /
    data wlapse / 0.26, 0.32, 0.16, 0.22, 0.23, 0.26, 0.22, 0.12, 0.16, 0.13, 0.14, 0.20 /

    !> Option 3:
    !>  Tables of temperature lapse rate, vapor pressure coefficient
    !>  (Kunkel et al., 1989), and precipitation-elevation adjustment
    !>  factors (Thornton et al., 1997) for each month for the
    !>  Northern Hemisphere. Incoming long wave radiation lapse rate of
    !>  29 W/m^2/1000 m (Marty et al., 2002).
!    data plapse / 0.35, 0.35, 0.35, 0.30, 0.25, 0.20, 0.20, 0.20, 0.20, 0.25, 0.30, 0.35 /
!    data tlapse / 4.40, 5.90, 7.10, 7.80, 8.10, 8.20, 8.10, 8.10, 7.70, 6.80, 5.50, 4.70 /
!    data dtlapse / 5.64, 5.78, 5.51, 5.37, 5.23, 4.96, 4.54, 4.54, 4.96, 5.09, 5.51, 5.51 /
!    data lwlapse / 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0, 29.0 /

    !> Output variables.
    real, dimension(nvals), intent(out) :: rsrd_adjusted
    real, dimension(nvals), intent(out) :: rlds_adjusted
    real, dimension(nvals), intent(out) :: temp_adjusted
    real, dimension(nvals), intent(out) :: pres_adjusted
    real, dimension(nvals), intent(out) :: humd_adjusted
    real, dimension(nvals), intent(out) :: rain_adjusted
    real, dimension(nvals), intent(out) :: rain_phased_adjusted
    real, dimension(nvals), intent(out) :: snow_phased_adjusted
    real, dimension(nvals), intent(out) :: wind_adjusted

    !> Working variables.
    integer MINS_int, kk, k
    real EOTCons, EOT, Decl, Dcon, Rad_vec, Sol, Cdecl, Sdecl, Trans
    real, dimension(nvals) :: &
        Clat, Slat, Sslp, Cslp, Sasp, Casp, SHVar, SH, SM, tsolar, &
        Hr_Ang, t1, t2, Czen, ACzen, oam, diff, Iterr, cosxs0, cosxsL, &
        Idiff, Sum_Idir, Sum_Diff, Sum_Flatd, Sum_Flatf, Qdirect, &
        Idir, Qdiffuse, Qflat, es, es_adjusted, ea, ea_adjusted, &
        rh, Tdew, Tdew_adjusted, epsilon_corr, rain_frac, windcorr, &
        oam1, oam2, OmegaC, OmegaS, &
        D, lamda, pta, L, Ti1, Ti2, crit, crit1, &
        TT1, TT2, aa, bb, cc, ratio, gru_icebulb
    real, dimension(i1:i2) :: OmegaS_grid, curvature_grid

    !> Precipitation adjustment.
    !> Option 1:
    !>  Precipitation correction using elevation based lapse (Thornton,
    !>  1997). Precipitation lapse rate derived from the high resolution
    !>  (2.5km by 2.5km) GEM run for the period Oct, 2016 to Sept, 2019.
    rain_frac = plapse(now_month)*delta/1000.0
    where (abs(rain_frac) > 0.95)
        rain_adjusted = rain*(1.0 + 0.95)/(1.0 - 0.95)
    elsewhere
        rain_adjusted = rain*(1.0 + rain_frac)/(1.0 - rain_frac)
    end where

    !> Option 2:
    !>  Precipitation lapse rate derived from the high resolution (2.5km
    !>  by 2.5km) GEM run for the period Oct, 2016 to Sept, 2019.
    where ((1.0 + 0.001*delta*plapse(now_month)) < 0.5)     ! Limit the precipitation-elevation adjustment factor in the range 0.5 - 1.5.
        rain_adjusted = rain*0.5
    elsewhere ((1.0 + 0.001*delta*plapse(now_month)) > 1.5) ! Limit the precipitation-elevation adjustment factor in the range 0.5 - 1.5.
        rain_adjusted = rain*1.50
    elsewhere
        rain_adjusted = rain*(1.0 + 0.001*delta*plapse(now_month))
    end where

    !> Temperature adjustment.
    !>  Linear lapse rates (measured, seasonal, constant, neutral
    !>  stability) using the table provided above.
    temp_adjusted = temp - (tlapse(now_month)*(delta)/1000.0)

    !> Pressure adjustment.
    !>  Pressure correction for elevation differences.
    pres_adjusted = pres*exp(-(delta*GCons)/(RCons*temp_adjusted))

    !> Specific humidity adjustment.
    !> Option 1:
    !>  Specific humidity correction for elevation difference using dew
    !>  point temperature (Murray, 1967).
    ea = humd*pres/(0.378*humd + 0.622)
    where (temp >= 273.16)
        Tdew = 237.29*log(ea/610.78)/(17.269 - log(ea/610.78))  ! Murray (1967) as used in CLASS_v3.6 for temperature greater than or equal to zero.
    elsewhere
        Tdew = 265.49*log(ea/610.78)/(21.875 - log(ea/610.78))  ! Murray (1967) as used in CLASS_v3.6 for temperature less than zero.
    end where
    Tdew_adjusted = Tdew - (dtlapse(now_month)*delta/1000.0)
    where (temp_adjusted >= 273.16)
        ea_adjusted = 610.78*exp(17.269*Tdew_adjusted/(Tdew_adjusted + 237.29))
    elsewhere
        ea_adjusted = 610.78*exp(21.875*Tdew_adjusted/(Tdew_adjusted + 265.49))
    end where
    humd_adjusted = 0.622*ea_adjusted/(pres_adjusted - 0.378*ea_adjusted)

    !> Option 2:
    !>  Specific humidity correction for elevation difference using dew
    !>  point temperature (Kunkel, 1989).
    ea = humd*pres/(0.378*humd + 0.622)
    where (temp >= 273.16)
        Tdew = 240.97*log(ea/611.21)/(17.502 - log(ea/611.21))  ! Buck, 1981 for temperatures 0°C and above.
    elsewhere
        Tdew = 272.55*log(ea/611.15)/(22.452 - log(ea/611.15))  ! Buck, 1981 for temperature less than zero.
    end where
    Tdew_adjusted = Tdew - (dtlapse(now_month)*delta/1000.0)
    where (temp_adjusted >= 273.16)
        ea_adjusted = 611.21*exp(17.502*Tdew_adjusted/(Tdew_adjusted + 240.97))
    elsewhere
        ea_adjusted = 611.15*exp(22.452*Tdew_adjusted/(Tdew_adjusted + 272.55))
    end where
    humd_adjusted = 0.622*ea_adjusted/(pres_adjusted - 0.378*ea_adjusted)

    !> Longwave radiation adjustment.
    !> Option 1:
    !>  Based on elevation lapse rate values.
    rlds_adjusted = rlds - (lwlapse(now_month)*(delta)/1000.0)

    !> Option 2:
    !>  Incoming longwave solar radiation correction for topography
    !>  based on temperature and vapour pressure (Abramowitz et al.,
    !>  2012).
!    epsilon_corr = (0.031*ea_adjusted + 2.84*temp_adjusted - 522.50)/(0.031*ea + 2.84*temp - 522.50)
!    rlds_adjusted = rlds*epsilon_corr

    !> Wind speed adjustment.
    !> Option 1:
    !>  Using the wind weighting factor to modify the wind speed (Liston
    !>  and Sturm, 1998).
    OmegaS_grid = 0.0
    curvature_grid = 0.0
    OmegaS = DEGtoRAD*slope*cos((winddir - aspect)*DEGtoRAD)
    do k = 1, nvals
        if (abs(OmegaS(k)) > OmegaS_grid(nml_grid_map(k))) then
            OmegaS_grid(nml_grid_map(k)) = abs(OmegaS(k))
        end if
        if (abs(curvature(k)) > curvature_grid(nml_grid_map(k))) then
            curvature_grid(nml_grid_map(k)) = abs(curvature(k))
        end if
    end do
    do k = 1, nvals
        OmegaS(k) = OmegaS(k)/(OmegaS_grid(nml_grid_map(k))*2.0)
        OmegaC(k) = curvature(k)/(curvature_grid(nml_grid_map(k))*2.0)
    end do
    windcorr = 1.0 + GammaS*OmegaS + GammaC*OmegaC
    wind_adjusted = wind*windcorr

    !> Option 2:
    !>  Using wind speed lapse rate for elevations.
    windcorr = wlapse(now_month)*(delta)/1000.0
    where (abs(windcorr) > 0.95)
        wind_adjusted = wind*(1.0 + 0.95)/(1.0 - 0.95)
    elsewhere
        wind_adjusted = wind*(1.0 + windcorr)/(1.0 - windcorr)
    end where

    !> Precipitation phase adjustment.
    !>  Calculates precipitation phase via falling hydrometeor energy
    !>  balance (Harder and Pomeroy, 2013).
    D = 2.06*(10.0**-5.0)*(temp_adjusted/273.15)**1.75
    lamda = 0.000063*temp_adjusted + 0.00673
    where ((temp_adjusted - 273.16) < 0.0)
        L = 1000.0*(2834.1 - 0.29*(temp_adjusted - 273.16) - 0.004*(temp_adjusted - 273.16)**2)
    elsewhere
        L = 1000.0*(2501.0 - (2.361*(temp_adjusted - 273.16)))
    end where
    pta = 18.01528*(1000.0*ea_adjusted)/(0.00831441*temp_adjusted)/1000.0
    Ti1 = 250.0
    crit = 9999.0
    do while (any(crit > 0.0001))   ! Iteration solution optimised by using the Newton-Raphston method.
        TT1 = Ti1 + 0.001*Ti1
        TT2 = Ti1 - 0.001*Ti1
        aa = (-Ti1 + temp_adjusted + &
            (L*D/lamda)*(pta - (18.01528*(0.611*exp((17.3*(Ti1 - 273.16))/(237.3 + (Ti1 - 273.16))))/(0.00831441*Ti1)/1000.0)))
        bb = (-TT1 + temp_adjusted + &
            (L*D/lamda)*(pta - (18.01528*(0.611*exp((17.3*(TT1 - 273.16))/(237.3 + (TT1 - 273.16))))/(0.00831441*TT1)/1000.0)))
        cc = (-TT2 + temp_adjusted + &
            (L*D/lamda)*(pta - (18.01528*(0.611*exp((17.3*(TT2 - 273.16))/(237.3 + (TT2 - 273.16))))/(0.00831441*TT2)/1000.0)))
        Ti2 = Ti1 - (aa/((bb - cc)/(0.002*Ti1)))
        crit1 = Ti1 - Ti2
        where (crit1 < 0.0)
            crit = -crit1
        elsewhere
            crit = crit1
        end where 
        Ti1 = Ti2
    end do
    gru_icebulb = Ti1 - 273.16
    where (gru_icebulb < -10.0) ! Eoverflow if ratio calculated with icebulb < -39C.
        ratio = 0.0
    elsewhere
        ratio = 1.0/(1.0 + 2.50286*(0.125006**gru_icebulb))
    end where
    rain_phased_adjusted = rain_adjusted*ratio          ! if(rain_adjusted > 0.0) then the rain or snow determined by ice bulb ratio.
    snow_phased_adjusted = rain_adjusted*(1.0 - ratio)

    !> Shortwave radiation adjustment.
    !> Considers elevation, slope, and aspect.

    !> Constant over time.
    Clat = cos(ylat*DEGtoRAD)
    Slat = sin(ylat*DEGtoRAD)
    Sslp = sin(slope*DEGtoRAD)
    Cslp = cos(slope*DEGtoRAD)
    Sasp = sin(aspect*DEGtoRAD)
    Casp = cos(aspect*DEGtoRAD)

    !> Calculate geometry.
    !> Declination of the sun above the celestial equator in radians.
    Decl = 0.409*sin(((2.0*pi*real(now_jday))/365.0) - 1.39)    !365.0 can be replaced by (real(leap_year(year_now)))
    Rad_vec = 1.0 + 0.033*cos((2.0*pi*real(now_jday))/365.0)    !365.0 can be replaced by (real(leap_year(year_now)))
    Sol = 0.082*Rad_vec

    !> Declination calculation following Dingman, 2015 and Iqbal, 1983.
!    Dcon = DEGtoRAD365*(real(now_jday) - 1.0 + ((real(now_hour) - 12.0)/24.0))
!    Decl = &
!        0.006918 - 0.399912*cos(Dcon) + 0.070257*sin(Dcon) &
!        - 0.006758*cos(2.0*Dcon) + 0.000907*sin(2.0*Dcon) &
!        - 0.002697*cos(3.0*Dcon) + 0.00148*sin(3.0*Dcon)

    !> Radius vector of the Earth's orbit (Rad_vec*Rad_vec).
!    Rad_vec = &
!        1.000110 + 0.034221*cos(Dcon) + 0.001280*sin(Dcon) &
!        + 0.000719*cos(2.0*Dcon) + 0.00077*sin(2.0*Dcon)
!    Sol = 0.081833333*Rad_vec   ! solar constant 1364 W/m**2 or 117.8 MJ/m**2*day or 0.08183333 MJ/m**2*min
    Cdecl = cos(Decl)
    Sdecl = sin(Decl)
    t1 = (-Slat*Casp*Sslp + Clat*Cslp)*Cdecl
    t2 = (Clat*Casp*Sslp + Slat*Cslp)*Sdecl

    !> Seasonal transmissivity of the atmosphere. Mean transmissivity of
    !>  the atmosphere can also be used (Trans = 0.818).
    Trans = 0.818 - (0.064*sin(2.0*pi*real(now_jday - 90)/365.0))       ! Seasonal transmissivity of the atmosphere.

    !> Hour angle calculation in 'now_jday' day. The hour angle varies
    !>  between -pi and pi and it is 0.0 at solar noon.
    EOTCons = (2.0*pi/364.0)*real(now_jday - 81)
    EOT = 9.87*sin(2.0*EOTCons) - 7.53*cos(EOTCons) - 1.50*sin(EOTCons) ! The equation of time from SunAngle, http://www.susdesign.com/sunangle/

    !> Time-stepping (for integral).
    MINS_int = nint(24.0*60.0/real(CalcFreq))

    !> Accumulate radiative fluxes.
    Sum_Idir = 0.0
    Sum_Diff = 0.0
    Sum_Flatd = 0.0
    Sum_Flatf = 0.0
    do kk = 1, nint(real(dtmins)/real(MINS_int))    ! Set to MESH run time-stepping.

        !> Hour angle calculation in 'now_jday' day. The hour angle
        !>  varies between -pi and pi and it is 0.0 at solar noon.
        SHVar = 60.0*real(now_hour) + real(now_mins) + real(kk*MINS_int)
        SHVar = SHVar + EOT + (4.0*xlng) - (60.0*(ceiling(xlng/15.0)))  ! To use time zone use "(60.0*Time_Zone)" instead of "(60.0*(nint(xlng/15.0)))".
        SH = modulo(SHVar, 1440.0)
        SH = SH/4.0
        where (SH < 0.0)
            Hr_Ang = DEGtoRAD*(SH + 180.0)
        elsewhere
            Hr_Ang = DEGtoRAD*(SH - 180.0)
        end where
        Czen = Cdecl*Clat*cos(Hr_Ang) + Sdecl*Slat  ! cos of zenith angle
        Czen = sign(max(abs(Czen), 1.0e-3), Czen)   ! avoiding vanishingly small numbers

        !> Optical air mass, Young, A. T. 1994. Air mass and refraction.
        !>  Applied Optics. 33:1108–1110.
        oam1 = 1.002432*Czen**2.0 + 0.148386*Czen + 0.0096467
        oam2 = Czen**3.0 + 0.149864*Czen**2.0 + 0.0102963*Czen + 0.000303978
        oam = abs(oam1/oam2)
        where (oam < 39.7)
            oam = oam
        elsewhere
            oam = 39.7
        end where
        oam = oam *(pres_adjusted/101325.0)                     ! modified optical air mass for other pressures (Pa) from the standard pressure (sea surface (Pa)) (in Iqbal (1983), p.100)
        where (Czen > 0.0)
            Iterr = real(MINS_int)*Sol*Czen                     ! extrater. rad for MINS_int minute interval
            cosxs0 = Clat*Cdecl*cos(Hr_Ang) + Slat*Sdecl
            where (cosxs0 > 0.0)                                ! On horizontal surface not in shadow
                Idir = Sol*cosxs0*(Trans**oam)*real(MINS_int)   ! direct rad. for MINS_int minute interval
                Idiff = 0.5*(0.91*Iterr - Idir)                 ! diffuse radiation on horizontal
                Sum_Flatd = Sum_Flatd + Idir
                Sum_Flatf = Sum_Flatf + Idiff
                cosxsL = -sin(Hr_Ang)*Sasp*Sslp*Cdecl + t1*cos(Hr_Ang) + t2         ! components of cos(x^s)
                where (cosxsL > 0.0)                                                ! Slope not in shadow
                    Sum_Idir = Sum_Idir + Sol*cosxsL*(Trans**oam)*real(MINS_int)    ! direct rad. for MINS_int minute interval
                end where
                Sum_Diff = Sum_Diff + Idiff*((cos(DEGtoRAD*slope/2.0))**2.0)        ! On slope surface
            end where
        end where
    end do

    !> Convert units.
    Qdirect = (1000000.0/(real(dtmins)*60.0))*Sum_Idir              ! clear-sky direct radiation on slope (MJ/m^2.int to W/m^2)
    Qdiffuse = (1000000.0/(real(dtmins)*60.0))*Sum_Diff             ! clear-sky diffuse radiation on slope (MJ/m^2.int to W/m^2)
    Qflat = (1000000.0/(real(dtmins)*60.0))*(Sum_Flatd + Sum_Flatf) ! clear-sky 'Qdirect + Qdiffuse' on horizontal surface (MJ/m^2.int to W/m^2)

    !> Radiation correction for slope (the slope module of CHRM).
    where (Qflat > 1.0)
        rsrd_adjusted = (rsrd/Qflat)*(Qdirect + Qdiffuse)
    elsewhere
        rsrd_adjusted = 0.0
    end where

    return

end subroutine
