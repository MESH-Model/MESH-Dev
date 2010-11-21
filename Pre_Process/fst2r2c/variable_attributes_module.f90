module variable_attributes_module

!   R2C
    integer, parameter :: nvr2c = 7
    character*100         varname (nvr2c)
    character*10          attrname(nvr2c)
    character*5           attrunit(nvr2c)
    character*50          FileName(nvr2c)

!   FST
    character*2          varNm(nvr2c+1) !2)

    data varname   /'shortwave_down',      &
                    'longwave_down',       &
                    'precipitation rate',  &
                    'air_temperature_2m',  &
                    'specific_humidity_2m',&
                    'surface_pressure',    &
                    'windspeed'/           
    
    data attrname  /'dswrf',               &
                    'dlwrf',               &
                    'prate',               &
                    'air',                 &
                    'shum',                &
                    'pres',                &
                    'uv'/
    
    data attrunit  /'W/m2',                 &
                    'W/m2',                 &
                    'mm/s',                 &
                    'K',                    &
                    'kg/kg',                &
                    'pa',                   &
                    'm/s'/
    
    data FileName /'basin_shortwave.r2c',   &
                   'basin_longwave.r2c',    &
                   'basin_rain.r2c',        &
                   'basin_temperature.r2c', &
                   'basin_humidity.r2c',    &
                   'basin_pres.r2c',        &
                   'basin_wind.r2c'/

!     FST - Surface Atmospheric Names - this needs to be 
!           updated based on changes in the FST file format

      Data varNm /'FB',  &  ! VISIBLE SHORTWAVE RADIATION 
                  'FI',  &  ! DOWNWELLING LONGWAVE RADIATION
!                  'G6',  &  ! PRECIPITATION RATE - LIQUID
!                  'G7',  &  ! PRECIPITATION RATE - SOLID 
                  'PR',  &  ! PRECIPITATION QUANTITY in [m]
                  'TT',  &  ! AMBIENT AIR TEMPERATURE
                  'HU',  &  ! SPECIFIC HUMIDITY AT REFERENCE HEIGHT
                  'UU',  &  ! WIND SPEED AT REFERENCE HEIGHT - x direction
                  'VV',  &  ! WIND SPEED AT REFERENCE HEIGHT - y direction
                  'P0'/     ! AIR PRESSURE AT SURFACE
end
