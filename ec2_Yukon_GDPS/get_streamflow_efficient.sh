#!/bin/bash
set -aex
#
# Dates
# Date to run is passed as argument $1
if [ -z "$1" ]; then
    echo 'MISSING argument $1 should equal date to run'
    exit 1
fi

# Provided the date 20170606 the script will retrieve data from 2017060516 to 2017060616 UTC-8
dt=$(date -d $1 -u +%Y%m%d)
date_today=$(date -d "${dt} + 1day" -u +%Y%m%d)

echo $dt

# DOWNLOAD Streamflow Observations csv files from remote_location_2 (2-3 days) and remote_location_3 (month)
    
    # Watershed characteristics
    stations[1]='09AB001'
    stations[2]='09BA001'
    stations[3]='09BC002'
    stations[4]='09DC006'
    stations[5]='10AA001'
    
    remote_location_2='http://dd.weather.gc.ca/hydrometric/csv/YT/hourly'
    remote_location_3='http://dd.weather.gc.ca/hydrometric/csv/YT/daily'
    home_dir='/home/ec2-user/Yukon_GDPS/'
    grib_file_path=$home_dir/GRIB    
    strm_file_path=$home_dir/Streamflow
    awk_file_path=$home_dir/scripts
    output_file_path=$home_dir/gem_forecasts    

    # Loop over the basins/watersheds
    for i in ${!stations[*]}
    do
       
       station=${stations[$i]}
       cd $strm_file_path/${station}/Archive/Gauged
       wget -r -l1 --no-parent -c -nd -A '*'$station'*' $remote_location_2
       wget -r -l1 --no-parent -c -nd -A '*'$station'*' $remote_location_3

       mv $'YT_'$station'_hourly_hydrometric.csv' $'YT_'$station'_hourly_hydrometric_'${date_today}'.csv'
       mv $'YT_'$station'_daily_hydrometric.csv' $'YT_'$station'_daily_hydrometric_'${date_today}'.csv'
    done

