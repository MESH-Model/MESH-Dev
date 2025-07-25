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
!>  - 2021/01/31: Clean up and Optimised.
!> Flags to control MESH runs
!*  ipre: Flag to specify precipitation adjustment method. [--].
!>      0: None.
!>      1: Elevation Range with Maximum elevation Method (ERMM) (Zhang et al. (2018)) and (Tesfa et al. (2019))
!>      2: Adjustment based on Thornton, 1997 or derived lapse-rate for elevation.
!*  itemp: Flag to specify temperature adjustment method. [--].
!>      0: None.
!>      1: Lapse-rate adjustment.
!*  ipres: Flag to specify pressure adjustment method. [--].
!>      0: None.
!>      1: Elevation adjustment.
!*  ihumd: Flag to specify specific humidity adjustment method. [--].
!>      0: None.
!>      1: Adjustment based on Kunkel, 1989.
!>      2: Adjustment based on Murray, 1967.
!>      3: No adjustment
!*  irlds: Flag to specify longwave radiation adjustment method. [--].
!>      0: None.
!>      1: Correction based on temperature and vapour pressure (Sicart et al., 2005)
!>      2: Lapse-rate adjustment.
!*  iwind: Flag to specify wind speed adjustment method. [--].
!>      0: None.
!>      1: Adjustment based on Liston and Sturm, 1998 (requires wind direction, winddir).
!>      2: Lapse-rate adjustment.
!*  iphase: Flag to specify precipitation phase partitioning method. [--].
!>      0: Partioning to 0.0 degrees C.
!>      1: Partitioning based on Harder and Pomeroy, 2013.
!*  irsrd: Flag to specify shortwave radiation adjustment method. [--].
!>      0: None.
!>      1: Adjustment based on Garnier and Ohmura, 1970.
!>      2: Liston, Elder - 2006 - A Meteorological Distribution System for High-Resolution Terrestrial Modeling (MicroMet).
!*  iconsmm: Flag to conserve the forcing field in the grid or subbasin. [--].
!>      0: No conservation.
!>      1: Conservation of the forcing field.
!>
!> Input variables:
!*  elev: Weighted average elevation of GRUs. [m].
!*  xlng: Longitude. [degrees].
!*  ylat: Latitude. [degrees].
!*  slope: Weighted average slope of the surface. [--].
!*  aspect: Weighted average aspect of the surface.
!*  delta: Weighted average elevation difference between GEM and MESH. [m].
!*  curvature: Weighted average curvature of the surface for GRUs.
!*  skyviewfactor: Weighted average skyviewfactor of the surface for GRUs.
!*  gru_frac: Fraction GRUs in the modelling subbasin / grid.
!*  nvals: Number of elements in the vector (e.g., 1:grids, 1:tiles, etc.). [--].
!*  nml_grid_map: Lookup table of grid ID from tile ID. [--].
!*  i1: ID of first grid active on current processing node. [--].
!*  i2: ID of last grid active on current processing node. [--].
!*  tlapse: Table of lapse rate values for temperature. [--].
!*  plapse: Table of lapse rate values for precipitation. [--].
!*  dtlapse: Table of vapor pressure coefficient [km**-1].
!*  lwlapse: Table of lapse rate values for longwave radiation. [--].
!*  wlapse: Table of lapse rate values for wind speed. [--].
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
!*  Curveweight: wind model curvature weight
!*  CalcFreq: Iterations per day (must divide the day into equal increments of minutes). [--].
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
    elev, xlng, ylat, slope, aspect, delta, curvature, skyviewfactor, &
    nvals, nml_grid_map, i1, i2, &
    CurveWeight, CalcFreq, &
    ipre, itemp, ipres, ihumd, irlds, iwind, iphase, irsrd, iconsmm, &
    tlapse, plapse, dtlapse, lwlapse, wlapse, &
    rsrd_dtmin, rlds_dtmin, temp_dtmin, &
    pres_dtmin, humd_dtmin, rain_dtmin, wind_dtmin, winddir_dtmin, &
    rsrd, rlds, temp, pres, humd, rain, wind, winddir, &
    rsrd_adjusted, rlds_adjusted, temp_adjusted, pres_adjusted, &
    humd_adjusted, rain_adjusted, rain_phased_adjusted, &
    snow_phased_adjusted, wind_adjusted, &
    now_year, now_month, now_jday, now_hour, now_mins, dtmins, &
    gru_frac)

    implicit none

    !> Constants and conversion factors.
    real, parameter :: GCons = 9.80616                 ! Gravitational constant in m s-2 taken from CLASS V3.6 Mannual Page 31
    real, parameter :: RCons = 287.04                  ! Gas constant for dry air in J kg-1 K-1 taken from CLASS V3.6 Mannual Page 31
    real, parameter :: pi = 3.14159265
    real, parameter :: DEGtoRAD = pi/180.0
    real, parameter :: DEGtoRAD365 = 2.0*pi/365.0

    !> Input variables.
    integer, intent(in) :: nvals
    real, dimension(nvals), intent(in) :: &
        elev, xlng, ylat, slope, aspect, delta, curvature, skyviewfactor, gru_frac
    integer, dimension(nvals), intent(in) :: nml_grid_map
    integer, intent(in) :: i1, i2
    real, intent(in) :: CurveWeight
    integer, intent(in) :: CalcFreq
    integer, intent(in) :: ipre, itemp, ipres, ihumd, irlds, iwind, iphase, irsrd, iconsmm
    real, dimension(12), intent(in) :: plapse, lwlapse, wlapse
    real, dimension(1:12, 0:23), intent(in) :: dtlapse
    real, dimension(1:12, 0:119), intent(in) :: tlapse
    integer, intent(in) :: &
        rsrd_dtmin, rlds_dtmin, temp_dtmin, &
        pres_dtmin, humd_dtmin, rain_dtmin, wind_dtmin, &
        winddir_dtmin
    real, dimension(nvals), intent(in) :: &
        rsrd, rlds, temp, pres, &
        humd, rain, wind, winddir
    integer, intent(in) :: &
        now_year, now_month, now_jday, &
        now_hour, now_mins, dtmins

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
        rh, Tdew, Tdew_adjusted, F_factor, windcorr, &
        oam1, oam2, OmegaC, OmegaS, wind_frac, rain_frac, temp_frac, &
        pres_frac, humd_frac, ea_frac, rlds_frac, rsrd_frac, Ti1, Ti2, crit, &
        crit1, TT1, TT2, ratio, gru_icebulb, &
        Szen, cos_i, cloud_frac, sun_azimuth, aspect_S0, minhumd
    real(kind = 8), dimension(nvals) :: D, lamda, pta, L, aa, bb, cc
    real, dimension(i1:i2) :: &
        OmegaS_grid, curvature_grid, grid_max_elev, grid_sum_elev, num_grus, &
        grid_sum_wind, grid_sum_rain, grid_sum_temp, &
        grid_sum_pres, grid_sum_humd, grid_sum_ea, grid_sum_rlds, grid_sum_rsrd

    !> Wind speed adjustment.
    if (iwind == 1) then

        !> Option 1:
        !>  Using the wind weighting factor to modify the wind speed (Liston and Sturm, 1998).
        OmegaS_grid = 0.0
        curvature_grid = 0.0
        OmegaS = DEGtoRAD*slope*cos((winddir - aspect)*DEGtoRAD)
        do k = 1, nvals
            if (abs(OmegaS(k)) > OmegaS_grid(nml_grid_map(k))) then
                OmegaS_grid(nml_grid_map(k)) = abs(OmegaS(k))
            end if
            ! if (abs(curvature(k)) > curvature_grid(nml_grid_map(k))) then
                ! curvature_grid(nml_grid_map(k)) = abs(curvature(k))
            ! end if
        end do
        do k = 1, nvals
            OmegaS(k) = OmegaS(k)/(2.0 * max(0.001,OmegaS_grid(nml_grid_map(k))))
            ! OmegaC(k) = curvature(k)/(2.0 * max(0.001,curvature_grid(nml_grid_map(k))))
        end do
        ! windcorr = 1.0 + (1.0 - CurveWeight)*OmegaS + CurveWeight*OmegaC           !> (1.0 - CurveWeight) wind model slope weight
        windcorr = 1.0 + (1.0 - CurveWeight)*OmegaS + CurveWeight*curvature          !> (1.0 - CurveWeight) wind model slope weight
        wind_adjusted = wind*windcorr
    else

        !>  No adjustment.
        wind_adjusted = wind
    end if

    !> The following will be used for the case of conservative mountain MESH.
    !>  Grid and/or subbasin forcing are conserved.
    if (iconsmm == 1) then
        grid_sum_wind = 0.0
        do k = 1, nvals
            grid_sum_wind(nml_grid_map(k)) = grid_sum_wind(nml_grid_map(k)) + (gru_frac(k)*wind_adjusted(k))
        end do
        do k = 1, nvals
            wind_frac(k) = wind(k)/grid_sum_wind(nml_grid_map(k))
        end do
        if (iwind /= 0) then
            wind_adjusted = wind_adjusted*wind_frac
        end if
    end if

    !> Precipitation adjustment.
    if (ipre == 1) then

        !> Option 1:
        !>  Precipitation downscaling using Elevation Range with Maximum elevation Method (ERMM)
        !>  (Zhang et al. (2018)) and (Tesfa et al. (2020)).
        grid_sum_rain = 0.0
        grid_max_elev = 0.0
        grid_sum_elev = 0.0
        num_grus = 0.0
        do k = 1, nvals
            if (elev(k) > grid_max_elev(nml_grid_map(k))) then
                grid_max_elev(nml_grid_map(k)) = elev(k)
            end if
            grid_sum_elev(nml_grid_map(k)) = grid_sum_elev(nml_grid_map(k)) + elev(k)
            num_grus(nml_grid_map(k))= num_grus(nml_grid_map(k)) + 1.0
        end do
        do k = 1, nvals
            rain_frac(k) = (elev(k) - (grid_sum_elev(nml_grid_map(k))/num_grus(nml_grid_map(k))))/ &
            grid_max_elev(nml_grid_map(k))
            rain_frac(k) = rain_frac(k) + 1.0
            grid_sum_rain(nml_grid_map(k)) = grid_sum_rain(nml_grid_map(k)) + (gru_frac(k)*rain_frac(k))
        end do

        !> Accumulate the elevation correction factor multiplied by GRUs fraction to normalize it to 1 and
        !>  conserve the grid / subbasin precipitation while redistribution over GRUs based on elevation.
        do k = 1, nvals
            if (grid_sum_rain(nml_grid_map(k)) > 0.0) then
                rain_frac(k) = rain_frac(k)/grid_sum_rain(nml_grid_map(k))
            else
                rain_frac(k) = 0.0
            end if
        end do
        rain_adjusted = rain*rain_frac
    else if (ipre == 2) then

        !> Option 2:
        !>  Precipitation correction using elevation based lapse rate (Thornton, 1997) or
        !>  derived from the high resolution GEM (2.5km by 2.5km).
        rain_frac = plapse(now_month) * min(delta, 1200.0)/1000.0
        where (abs(rain_frac) > 0.95)
            rain_adjusted = rain*(1.0 + 0.95)/(1.0 - 0.95)
        elsewhere
            rain_adjusted = rain*(1.0 + rain_frac)/(1.0 - rain_frac)
        end where

        !> The following will be used for the case of conservative mountain MESH.
        !>  Grid and/or subbasin forcing are conserved.
        if (iconsmm == 1) then
            grid_sum_rain = 0.0
            do k = 1, nvals
                grid_sum_rain(nml_grid_map(k)) = grid_sum_rain(nml_grid_map(k)) + (gru_frac(k)*rain_adjusted(k))
            end do
            do k = 1, nvals
                rain_frac(k) = rain(k)/grid_sum_rain(nml_grid_map(k))
            end do
            rain_adjusted =  rain_adjusted*rain_frac
        end if
    else

        !>  No adjustment.
        rain_adjusted = rain
    end if

    !> Temperature adjustment.
    if (itemp == 1) then

        !> Option 1:
        !>  When the weighted average temperature of grid or subbasin is not conserved.
        !>  Elevation band linear lapse rates using the table provided.
        do k = 1, nvals
            if (elev(k) < 1000.0) then
                temp_adjusted(k) = temp(k) + (tlapse(now_month,now_hour)*delta(k)/1000.0)
            else if (elev(k) >= 1000.0 .and. elev(k) < 2000.0) then
                temp_adjusted(k) = temp(k) + (tlapse(now_month,(24 + now_hour))*delta(k)/1000.0)
            else if (elev(k) >= 2000.0 .and. elev(k) < 3000.0) then
                temp_adjusted(k) = temp(k) + (tlapse(now_month,(48 + now_hour))*delta(k)/1000.0)
            else if (elev(k) >= 3000.0 .and. elev(k) < 4000.0) then
                temp_adjusted(k) = temp(k) + (tlapse(now_month,(72 + now_hour))*delta(k)/1000.0)
            else
                temp_adjusted(k) = temp(k) + (tlapse(now_month,(96 + now_hour))*delta(k)/1000.0)
            end if
        end do
    else

        !>  No adjustment.
        temp_adjusted = temp
    end if

    !> The following will be used for the case of conservative mountain MESH.
    !>  Grid and/or subbasin forcing are conserved.
    if (iconsmm == 1) then
        grid_sum_temp = 0.0
        do k = 1, nvals
            grid_sum_temp(nml_grid_map(k)) = grid_sum_temp(nml_grid_map(k)) + (gru_frac(k)*temp_adjusted(k))
        end do
        do k = 1, nvals
            temp_frac(k) = temp(k)/grid_sum_temp(nml_grid_map(k))
        end do
        if (itemp .ne. 0) then
            temp_adjusted = temp_adjusted*temp_frac
        end if
    end if

    !> Pressure adjustment.
    if (ipres == 1) then

        !> Option 1:
        !>  Pressure correction for elevation differences.
        pres_adjusted = pres*exp(-(delta*GCons)/(RCons*temp_adjusted))
    else

        !>  No adjustment.
        pres_adjusted = pres
    end if

    !> The following will be used for the case of conservative mountain MESH.
    !>  Grid and/or subbasin forcing are conserved.
    if (iconsmm == 1) then
        grid_sum_pres = 0.0
        do k = 1, nvals
            grid_sum_pres(nml_grid_map(k)) = grid_sum_pres(nml_grid_map(k)) + (gru_frac(k)*pres_adjusted(k))
        end do
        do k = 1, nvals
            pres_frac(k) = pres(k)/grid_sum_pres(nml_grid_map(k))
        end do
        if (ipres /= 0) then
            pres_adjusted = pres_adjusted*pres_frac
        end if
    end if

    !> Specific humidity adjustment.
    minhumd = humd
    where (minhumd <= 0.0) minhumd = minval(humd, MASK = humd > 0.0)
    if (ihumd == 1) then

        !> Option 1:
        !>  Specific humidity correction for elevation difference using dew point temperature (Kunkel, 1989).
        !>  Variables required for specific humidity and precipitation phase change correction.
        ea = pres*minhumd/(0.378*minhumd + 0.622)
        where (temp >= 273.16)
            es = 611.21*exp(17.502*(temp - 273.16)/(temp - 273.16 + 240.97))
        elsewhere
            es = 611.15*exp(22.452*(temp - 273.16)/(temp - 273.16 + 272.55))
        end where
        where (temp_adjusted >= 273.16)
            es_adjusted = 611.21*exp(17.502*(temp_adjusted - 273.16)/(temp_adjusted - 273.16 + 240.97))
        elsewhere
            es_adjusted = 611.15*exp(22.452*(temp_adjusted - 273.16)/(temp_adjusted - 273.16 + 272.55))
        end where
        where (ea > es) ea = es
        ea_adjusted = (ea/es)*es_adjusted
        humd_adjusted = 0.622*ea_adjusted/(pres_adjusted - 0.378*ea_adjusted)
    else

        !>  No adjustment.
        ea = pres*minhumd/(0.378*minhumd + 0.622)
        where (temp >= 273.16)
            es = 611.21*exp(17.502*(temp - 273.16)/(temp - 273.16 + 240.97))
        elsewhere
            es = 611.15*exp(22.452*(temp - 273.16)/(temp - 273.16 + 272.55))
        end where
        where (ea > es) ea = es
        ea_adjusted = ea
        es_adjusted = es
        humd_adjusted = minhumd
    end if

    !> The following will be used for the case of conservative mountain MESH.
    !>  Grid and/or subbasin forcing are conserved.
    if (iconsmm == 1) then
        grid_sum_humd = 0.0
        grid_sum_ea = 0.0
        do k = 1, nvals
            grid_sum_humd(nml_grid_map(k)) = grid_sum_humd(nml_grid_map(k)) + (gru_frac(k)*humd_adjusted(k))
            grid_sum_ea(nml_grid_map(k)) = grid_sum_ea(nml_grid_map(k)) + (gru_frac(k)*ea_adjusted(k))
        end do
        do k = 1, nvals
            if (grid_sum_humd(nml_grid_map(k)) > 0.0) then
                humd_frac(k) = minhumd(k)/grid_sum_humd(nml_grid_map(k))
            else
                humd_frac(k) = 0.0
            end if
            if (grid_sum_ea(nml_grid_map(k)) > 0.0) then
                ea_frac(k) = ea(k)/grid_sum_ea(nml_grid_map(k))
            else
                ea_frac(k) = 0.0
            end if
        end do
        if (ihumd /= 0) then
            humd_adjusted = humd_adjusted*humd_frac
            ea_adjusted = ea_adjusted*ea_frac
        end if
    end if

    !> Longwave radiation adjustment.
    if (irlds == 1) then

        !> Option 1:
        !>  Incoming longwave radiation correction based on temperature and vapour pressure (Brutsaert (1975)).
        F_factor = rlds/(1.24*((0.01*ea/temp)**(1.0/7.0))*5.67e-8*temp**4.0)
        rlds_adjusted = F_factor*(1.24*(0.01*ea_adjusted/temp_adjusted)**(1.0/7.0)*5.67e-8*temp_adjusted**4.0)
        rlds_adjusted = skyviewfactor*rlds_adjusted
        rlds_adjusted = rlds_adjusted + ((1.0 - skyviewfactor)*0.98*5.67e-8*temp_adjusted**4.0)
    else if (irlds == 2) then

        !> Option 2: Based on elevation lapse rate values.
        !>  Incoming longwave solar radiation correction for topography.
        rlds_adjusted = rlds + (lwlapse(now_month)*delta/1000.0)
    else

        !>  No adjustment.
        rlds_adjusted = rlds
    end if

    !> The following will be used for the case of conservative mountain MESH.
    !>  Grid and/or subbasin forcing are conserved.
    if (iconsmm == 1) then
        grid_sum_rlds = 0.0
        do k = 1, nvals
            grid_sum_rlds(nml_grid_map(k)) = grid_sum_rlds(nml_grid_map(k)) + (gru_frac(k)*rlds_adjusted(k))
        end do
        do k = 1, nvals
            rlds_frac(k) = rlds(k)/grid_sum_rlds(nml_grid_map(k))
        end do
        if (irlds /= 0) then
            rlds_adjusted = rlds_adjusted*rlds_frac
        end if
    end if

    !> Precipitation phase adjustment.
    if (iphase == 1) then

        !> Option 1:
        !>  Calculates precipitation phase via falling hydrometeor energy balance (Harder and Pomeroy, 2013).
        D = 0.0000206*(temp_adjusted/273.16)**1.75
        lamda = (0.000063*temp_adjusted) + 0.00673
        pta = 18.01528*((ea/es)* &
            0.611*exp((17.3*(temp_adjusted - 273.16))/(237.3 + temp_adjusted - 273.16)))/(0.00831441*temp_adjusted)/1000.0
        where (temp_adjusted > 273.16)
            L = 1000.0*(2501.0 - (2.361*(temp_adjusted - 273.16)))
        elsewhere
            L = 1000.0*(2834.1 - 0.29*(temp_adjusted - 273.16) - 0.004*(temp_adjusted - 273.16)**2.0)
        end where
        Ti1 = 250.0
        crit = 9999.0
        do while (any(crit > 0.0001))                            ! Iteration solution optimised by using the Newton-Raphston method.
            TT1 = Ti1 + 0.001*Ti1
            TT2 = Ti1 - 0.001*Ti1
            aa = (-Ti1 + temp_adjusted + &
                (L*D/lamda)*(pta - (18.01528*(0.611*exp((17.3*(Ti1 - 273.16))/(237.3 + (Ti1 - 273.16))))/(0.00831441*Ti1)/1000.0)))
            bb = (-TT1 + temp_adjusted + &
                (L*D/lamda)*(pta - (18.01528*(0.611*exp((17.3*(TT1 - 273.16))/(237.3 + (TT1 - 273.16))))/(0.00831441*TT1)/1000.0)))
            cc = (-TT2 + temp_adjusted + &
                (L*D/lamda)*(pta - (18.01528*(0.611*exp((17.3*(TT2 - 273.16))/(237.3 + (TT2 - 273.16))))/(0.00831441*TT2)/1000.0)))
            Ti2 = Ti1 - real(aa/((bb - cc)/(0.002*Ti1)))
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

            !> 1-hour data.
            ratio = 1.0/(1.0 + 2.50286*(0.125006**gru_icebulb))

            !> 15-min data.
            ! ratio = 1.0/(1.0 + 2.630006*(0.09336**gru_icebulb))
        end where

        !> Round it to decimal places.
        ratio = nint(ratio*1000.0)*1.0E-3

        !> Determine by ice bulb ratio.
        rain_phased_adjusted = rain_adjusted*ratio
        snow_phased_adjusted = rain_adjusted*(1.0 - ratio)
    else

        !> Option 2:
        !>  Partioning to 0.0 degrees C.
        where (temp_adjusted > 273.16)
            rain_phased_adjusted = rain_adjusted
            snow_phased_adjusted = 0.0
        elsewhere
            rain_phased_adjusted = 0.0
            snow_phased_adjusted = rain_adjusted
        end where
    end if

    !> Shortwave radiation adjustment.
    !> Constant over time.
    Clat = cos(ylat*DEGtoRAD)
    Slat = sin(ylat*DEGtoRAD)
    Sslp = sin(slope*DEGtoRAD)
    Cslp = cos(slope*DEGtoRAD)
    Sasp = sin(aspect*DEGtoRAD)
    Casp = cos(aspect*DEGtoRAD)

    !> Adjustment of solar irradiance.
    if (irsrd == 1) then

        !> Option 1:
        !>  Shortwave radiation adjustment based on Garnier and Ohmura, 1970. (default).
        !>  Declination of the sun above the celestial equator in radians.
        Decl = sin(real(now_jday - 81)*DEGtoRAD365)*0.40928

        !> Radius vector.
        Rad_vec = 0.01676*cos(pi - 0.017262*(now_jday - 3)) + 1.0

        !> Solar constant: 1365 W/m**2 or 117.936 MJ/m**2*day or 0.0819 MJ/m**2*min
        Sol = 0.0819*Rad_vec*Rad_vec

        !> Constant within the hour
        Cdecl = cos(Decl)
        Sdecl = sin(Decl)
        t1 = (-Slat*Casp*Sslp + Clat*Cslp)*Cdecl
        t2 = (Clat*Casp*Sslp + Slat*Cslp)*Sdecl

        !> Seasonal transmissivity of the atmosphere (Granger and Gray (1990)).
        !>  Mean transmissivity of the atmosphere (Trans = 0.818) can be also used.
        Trans = 0.818 - (0.064*sin(2.0*pi*real(now_jday - 90)/365.0))

        !> Time-stepping (for integral).
        MINS_int = nint(24.0*60.0/real(CalcFreq))

        !> Accumulate radiative fluxes.
        Sum_Idir = 0.0
        Sum_Diff = 0.0
        Sum_Flatd = 0.0
        Sum_Flatf = 0.0

        !> Integration over the sub time step for Hour Angle.
        !>  (set based on MESH run time-stepping).
        do kk = 1, nint(real(dtmins)/real(MINS_int))

            !> Hour angle calculation in 'now_jday' day.
            !>  Hour angle varies between -pi and pi and is 0.0 at solar noon.
            Dcon = DEGtoRAD365*(real(now_jday) - 1.0 + ((real(now_hour) - 12.0)/24.0))

            !> Equation of time (EOT) using General Solar Position Calculations NOAA Global Monitoring Division.
            EOT = &
                229.18*(0.000075 + 0.001868*Cos(Dcon) - 0.032077*Sin(Dcon) - 0.014615*Cos(2.0*Dcon) - 0.040890*Sin(2.0*Dcon))
            SHVar = 60.0*real(now_hour) + real(now_mins) + real(kk*MINS_int)

            !> To use time zone replace "(60.0*nint(xlng/15.0))" instead of "(60.0*Time_Zone)" in the equation.
            SHVar = SHVar + EOT + (4.0*xlng) - (60.0*nint(xlng/15.0))
            SH = modulo(SHVar, 1440.0)
            SH = SH/4.0
            where (SH < 0.0)
                Hr_Ang = DEGtoRAD*(SH + 180.0)
            elsewhere
                Hr_Ang = DEGtoRAD*(SH - 180.0)
            end where

            !> Czen: cos of zenith angle.
            Czen = Cdecl*Clat*cos(Hr_Ang) + Sdecl*Slat
            where (Czen < 0.0) Czen = 0.0
            Iterr = 0.0
            Idir = 0.0
            Idiff = 0.0

            !> Horizontal not in shadow
            where (Czen > 0.0)

                !> Optical air mass, Young, A. T. 1994. Air mass and refraction. Applied Optics. 33:1108–1110.
                oam1 = 1.002432*Czen**2.0 + 0.148386*Czen + 0.0096467
                oam2 = Czen**3.0 + 0.149864*Czen**2.0 + 0.0102963*Czen + 0.000303978
                oam = abs(oam1/oam2)

                !> Extrater. rad for MINS_int minute interval.
                Iterr = Sol*real(MINS_int)*Czen

                !> Modified optical air mass for other pressures (Pa) from the standard pressure (sea surface (Pa)) (in Iqbal (1983), p.100).
                !>  Idir: Direct rad. for MINS_int minute interval.
                !>  Idiff: Diffuse radiation on horizontal.
                Idir = Sol*real(MINS_int)*Czen*(Trans**(oam*pres/101325.0))
                Idiff = 0.5*(0.91*Iterr - Idir)
                Sum_Flatd = Sum_Flatd + Idir
                Sum_Flatf = Sum_Flatf + Idiff

                !> Components of cos(x^s).
                cosxsL = (-sin(Hr_Ang)*Sasp*Sslp*Cdecl) + t1*cos(Hr_Ang) + t2

                !> Slope not in shadow.
                !>  Direct rad. for MINS_int minute interval.
                where (cosxsL > 0.0)
                    Idir = Sol*real(MINS_int)*cosxsL*(Trans**(oam*pres_adjusted/101325.0))
                end where

                !> On slope surface.
                Idiff = Idiff*skyviewfactor
                Sum_Idir = Sum_Idir + Idir
                Sum_Diff = Sum_Diff + Idiff
            end where
        end do

        !> Convert units.
        Qdirect = (1000000.0/real(dtmins)/60.0)*Sum_Idir                               ! clear-sky direct radiation on slope (MJ/m^2.int to W/m^2).
        Qdiffuse = (1000000.0/real(dtmins)/60.0)*Sum_Diff                              ! clear-sky diffuse radiation on slope (MJ/m^2.int to W/m^2).
        Qflat = (1000000.0/real(dtmins)/60.0)*(Sum_Flatd + Sum_Flatf)                  ! clear-sky 'Qdirect + Qdiffuse' on horizontal surface (MJ/m^2.int to W/m^2).
        where (Qflat > 1.0)
            rsrd_adjusted = (rsrd/Qflat)*(Qdirect + Qdiffuse)
        elsewhere
            rsrd_adjusted = 0.0
        end where
    else if (irsrd == 2) then

        !> Option 2: Recommended to test before use!
        !>  Shortwave radiation adjustment based Liston, Elder - 2006 - A Meteorological Distribution System
        !>  for High-Resolution Terrestrial Modeling (MicroMet).
        !>  Make north have zero azimuth if in the southern hemisphere.
        Decl = 0.409*cos(2.0*pi*(real(now_jday) - 173.0)/365.25)

        !> Constant within the hour.
        Cdecl = cos(Decl)
        Sdecl = sin(Decl)
        where (ylat >= 0.0)
            where (aspect >= 180.0)
                aspect_S0 = aspect - 180.0
            elsewhere
                aspect_S0 = aspect + 180.0
            end where
        elsewhere
            aspect_S0 = aspect
        end where

        !> Compute the sun's hour angle (radians).
        !>  Czen: cos of zenith angle.
        Hr_Ang = (real(now_hour + (now_mins/60))*15.0 - 180.0)*DEGtoRAD
        Czen = Cdecl*Clat*cos(Hr_Ang) + Sdecl*Slat
        where (Czen < 0.0) Czen = 0.0

        !> The sine of the solar zenith angle.
        Szen = sqrt(1.0 - Czen*Czen)

        !> Azimuth of the sun, with south having zero azimuth for the northern hemisphere.
        sun_azimuth = asin(max(-1.0, min(1.0, Cdecl*sin(Hr_Ang)/Szen)))

        !> Make the corrections so that the angles below the local horizon
        !>  are still measured from the normal to the slope.
        where (ylat < 0.0)
            sun_azimuth = -sun_azimuth
        elsewhere
            where (Hr_Ang < 0.0)
                where (Hr_Ang < sun_azimuth) sun_azimuth = -pi - sun_azimuth
            elsewhere
                where (Hr_Ang > sun_azimuth) sun_azimuth = pi - sun_azimuth
            end where
        end where

        !> Compute the angle between the normal to the slope and the angle at
        !>  which the direct solar radiation impinges on the sloping terrain (radians).
        cos_i = Cslp*Czen + Sslp*Szen*cos(sun_azimuth - (aspect_S0*DEGtoRAD))

        !> Adjust the topographic correction due to local slope so that
        !>  the correction is zero if the sun is below the local horizon
        !>  (i.e., the slope is in the shade) or if the sun is below the global horizon.
        where (cos_i < 0.0) cos_i = 0.0
        where (Czen <= 0.0) cos_i = 0.0

        !> Account for clouds, water vapor, pollution, etc.
        !>  Compute the solar radiation transmitted through the atmosphere.
        !>  solar_const = 1365.0
        where (Czen > 0)
            cloud_frac = (Czen*(0.6 + 0.2*Czen) - (rsrd/1365.0))/(Czen*(0.3 + 0.1*Czen))
        elsewhere
            cloud_frac = 0.0
        end where
        where (cloud_frac < 0.0) cloud_frac = 0.0
        where (cloud_frac > 1.0) cloud_frac = 1.0

        !> Adjust the solar radiation for slope, etc.
        Qdirect = cos_i*1365.0*(0.6 + 0.2*Czen)*(1.0 - cloud_frac)
        Qdiffuse = Czen*1365.0*(0.3 + 0.1*Czen)*cloud_frac
        where (Qdirect < 0.0) Qdirect = 0.0
        where (Qdiffuse < 0.0) Qdiffuse = 0.0

        !> Combine the direct and diffuse solar components.
        rsrd_adjusted = Qdirect + Qdiffuse
    else

        !>  No adjustment.
        rsrd_adjusted = rsrd
    end if

    !> The following will be used for the case of conservative mountain MESH.
    !>  Grid and/or subbasin forcing are conserved.
    if (iconsmm == 1) then
        grid_sum_rsrd = 0.0
        do k = 1, nvals
            grid_sum_rsrd(nml_grid_map(k)) = grid_sum_rsrd(nml_grid_map(k)) + (gru_frac(k)*rsrd_adjusted(k))
        end do
        do k = 1, nvals
            if (grid_sum_rsrd(nml_grid_map(k)) > 0.0) then
                rsrd_frac(k) = rsrd(k)/grid_sum_rsrd(nml_grid_map(k))
            else
                rsrd_frac(k) = 0.0
            end if
        end do
        rsrd_adjusted = rsrd_adjusted*rsrd_frac
    end if

    return

end subroutine
