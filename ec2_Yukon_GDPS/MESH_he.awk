BEGIN { FS = "," }
{

dt = strftime("%Y/%m/%d %T", systime()-6*60*60)
if ( $2 == "humidity" ) { att = "specific_humidity_40m" ; atu = "kg/kg" }
if ( $2 == "longwave" ) { att = "longwave_down" ; atu = "W/m2" }
if ( $2 == "pres" ) { att = "surface_pressure" ; atu = "Pa" }
if ( $2 == "rain" ) { att = "precipitation_rate_at_40m" ; atu = "kg m-2 s-1" }
if ( $2 == "shortwave" ) { att = "shortwave_down" ; atu = "W/m2" }
if ( $2 == "temperature" ) { att = "surface_air_temperature" ; atu = "K" }
if ( $2 == "wind" ) { att = "wind_speed_40m" ; atu = "m/s" }

printf ("#######################################\n")
printf ("#\n")
printf (":FileType r2c  ASCII  EnSim 1.0\n")
printf ("#\n")
printf ("# DataType               2D Rect Cell\n")
printf ("#\n")
printf (":Application             FORTRAN\n")
printf (":Version                 1.0.0\n")
printf (":WrittenBy               ARP\n")
printf (":CreationDate            %s\n", dt)
printf ("#\n")
printf ("#--------------------------------------\n")
printf ("#\n")
printf (":Name                    %s_%s\n", $2, $1)
printf ("#\n")
printf (":Projection              LATLONG\n")
printf (":Ellipsoid               WGS84\n")
printf ("#\n")
printf (":xOrigin                 %s\n", $4)
printf (":yOrigin                 %s\n", $3)
printf ("#\n")
printf (":SourceFile              Datamart_grib2\n")
printf ("#\n")
printf (":AttributeName           %s\n", att )
printf (":AttributeUnit           %s\n", atu )
printf ("#\n")
printf (":xCount                  %i\n", $5)
printf (":yCount                  %i\n", $6)
printf (":xDelta                  %s\n", $7)
printf (":yDelta                  %s\n", $8)
printf ("#\n")
printf ("#\n")
printf (":endHeader\n")
}
