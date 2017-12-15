#!/bin/bash

set -aex

# define paths and other variables
# use ABSOLUTE paths

home_dir='/fs/home/fs1/eccc/oth/nlab_west/aql000/my_proj_hall2/MESH_Forecast/'
remote_location='http://dd.weatheroffice.gc.ca/model_gem_regional/10km/grib2'
    run_time='18'
awk_file_path=$home_dir'scripts/'
grib_file_path=$home_dir'GRIB/'
temp_file_path=$home_dir'TempFiles/'
run_file_path=$home_dir'gem_forecasts/'
bin_wgrib2=/fs/ssm/eccc/cmo/cmoi/apps/wgrib2/2.05-ipolate/ubuntu-14.04-amd64-64/bin/wgrib2

    maxhours=54

namess[1]="humidity"
namess[2]="longwave"
namess[3]="pres"
namess[4]="rain"
namess[5]="shortwave"
namess[6]="temperature"
namess[7]="wind"

FILESS[1]="*SPFH_TGL_2*"  # kg/kg
FILESS[2]="*DLWRF_SFC*" # J/m2
FILESS[3]="*PRES_SFC*"  # Pa
#FILESS[4]="*PRATE_SFC*" # kg/m2 
FILESS[4]="*APCP_SFC*" # kg/m2 
FILESS[5]="*DSWRF_SFC*" # J/m2 accumulated
FILESS[6]="*TMP_TGL_2*"   # K
FILESS[7]="*WIND_TGL_40*"  # m/s at 40 m

runtimes[1]='18'
maxhours[1]=54

watersheds[1]='05KJ001'
stations[1]='05KJ001'
lats[1]=47.577
lons[1]=-118.361
xcounts[1]=166
ycounts[1]=72
xdelta=0.125
ydelta=0.125
a="*"


if [ -z "$1" ]; then
    echo 'MISSING argument $1 should equal date to run'
    exit 1
fi

# Provided the date 20170606 the script will retrieve data from 2017060516 to 2017060616 UTC-8

yest=$(date -d  "$1 -1day" -u +%Y%m%d)
endt=$(date -d  "$1 1day" -u +%Y%m%d)

echo $yest, $endt

    # DOWNLOAD the GEM grib2 files from remote_location
    cd $grib_file_path
    for hour in `seq -f %03.0f 5 1 $maxhours`
     do
         for variable in ${!FILESS[*]}
         do
            fvariable=${FILESS[$variable]}
            wget -r -l1 --no-parent -nd -A $fvariable$yest'*' $remote_location'/'$run_time'/'$hour'/'
        done
        echo "boo"
    done

# LOOP over the basins/watersheds
for basin in ${!lats[*]}
do

    # Remove temporary files if they exist
    rm $temp_file_path/*.* -f

    # ASSIGN paths for Stored Files and Working folder
    watershed=${watersheds[$basin]}        
    station=${stations[$basin]}

    # Create run directory
    output_file_path=$run_file_path$watershed'/'$yest'16_to_'$endt'16/'
    if [ ! -d "$output_file_path" ]; then
    mkdir $output_file_path
    fi
 
    lat=${lats[$basin]}
    lon=${lons[$basin]}
    ycount=${ycounts[$basin]}
    xcount=${xcounts[$basin]}

      # ------------------- GEM -------------------------------
    # LOOP over each variable on the saved grib2 files to create the GEM forcing files
    for index in ${!namess[*]}
    do

        # CREATE the forcing file header
        names=${namess[$index]}
        FILES=${FILESS[$index]}
      #  touch $awk_file_path$'header_info.txt'
        echo "$watershed,$names,$lat,$lon,$xcount,$ycount,$xdelta,$ydelta" > $awk_file_path$'header_info.txt' 
        gawk -f $awk_file_path'MESH_he.awk' $awk_file_path$'header_info.txt' > $awk_file_path$'basin_'$names'.r2c'

        # LOOP over each time-step on the grib2 files
        for hour in `seq -f %03.0f 5 1 $maxhours`
        do

           # CLIP the grib2 to a basin-size rectangle and CONVERT the clipped file into a csv file
           f=$grib_file_path$FILES$yest$run_time'_P'$hour'*.grib2'
       
          $bin_wgrib2 $f -new_grid_interpolation neighbor -new_grid_winds earth -new_grid latlon $lon:$xcount:$xdelta $lat:$ycount:$ydelta $temp_file_path$names$hour'.tmp'
           $bin_wgrib2 $temp_file_path$names$hour'.tmp' -csv $temp_file_path$names$hour'.csv'
    
           # Restructure the data in the csv files into a single-line-per-timestep file, and append all time-steps into another file
           gawk 'BEGIN { FS = "," }; { print $2, $3, $NF }' $temp_file_path$names$hour'.csv' > $temp_file_path$names$hour'_2.csv'
    
           gawk -f $awk_file_path'MESH_2.awk' $temp_file_path$names$hour'_2.csv' >> $temp_file_path$names'_3.csv'

        done

        # FORMAT the data into a matrix configuration as per r2c format
        gawk '{ print $0 }' $awk_file_path$'header_info.txt' > $temp_file_path$names'_4.csv'
        gawk '{ print $0 }' $temp_file_path$names'_3.csv' >> $temp_file_path$names'_4.csv'
        gawk -f $awk_file_path$'MESH_3.awk' $temp_file_path$names'_4.csv' > $temp_file_path$names'_5.csv'

        # APPEND the r2c body into the forcing file header
        gawk '{ print $0 }' $temp_file_path$names'_5.csv' >> $awk_file_path$'basin_'$names'.r2c'

        # MOVE the newly created forcing files into the run folder 
        mv $awk_file_path$'basin_'$names'.r2c' $output_file_path$'basin_'$names'.r2c'

     done

done

