#!/bin/bash

set -aex

# define paths and other variables
# use ABSOLUTE paths

#date=$(date --date='yesterday' -u +%Y%m%d)
date=$(date -u +%Y%m%d) #for download before 00:00:00 UTC

#dt=$(date -u +%Y%m%d)
#endt=$(date --date='1 day' -u +%Y%m%d)
#date=$(date --date='2 days ago' -u +%Y%m%d)
#dt=$(date --date='yesterday' -u +%Y%m%d)
#endt=$(date -u +%Y%m%d)


home_dir='/home/ec2-user/Yukon_GDPS/'
remote_location='http://dd.weatheroffice.gc.ca/model_gem_global/25km/grib2/lat_lon/'
    run_time='12'
awk_file_path=$home_dir'scripts/'
grib_file_path=$home_dir'GRIB/'
temp_file_path=$home_dir'TempFiles/'
run_file_path=$home_dir'gem_forecasts/'
    minhours=009
    maxhours=240
    #interval=3

namess[1]="humidity"
namess[2]="longwave"
namess[3]="pres"
namess[4]="rain"
namess[5]="shortwave"
namess[6]="temperature"
namess[7]="wind"

FILESS[1]="*SPFH_SFC*"        #TGL_2*"  # kg/kg
FILESS[2]="*DLWRF_SFC*" # J/m2
FILESS[3]="*PRES_SFC*"  # Pa
#FILESS[4]="*PRATE_SFC*" # kg/m2
FILESS[4]="*APCP_SFC*" # kg/m2
FILESS[5]="*DSWRF_SFC*" # J/m2 accumulated
FILESS[6]="*TMP_TGL_2*"   # K
FILESS[7]="*WIND_TGL_40*"  # m/s at 40 m


#Number of files to be downloaded. Used in a condition to check whether all necessary GDPS files have been downloaded.

counter=0
for hour in `seq -f %03.0f $minhours 3 $maxhours`
do 
   let counter=counter+1            
done

#counter=546

#runtimes[1]='12'
#maxhours[1]=006

watersheds[1]="09AB001"
stations[1]="09AB001"
lats[1]=58.7380
lons[1]=-135.8320
ycounts[1]=19
xcounts[1]=24
watersheds[2]="09BC002"
stations[2]="09BA001"
lats[2]=61.14
lons[2]=-132.75
xcounts[2]=30
ycounts[2]=17
watersheds[3]="09DC006"
stations[3]="09DC006"
lats[3]=62.75
lons[3]=-136.50
xcounts[3]=55
ycounts[3]=16
watersheds[4]="10AA001"
stations[4]="10AA001"
lats[4]=59.25
lons[4]=-132.10
xcounts[4]=31
ycounts[4]=25
xdelta=0.125
ydelta=0.125
a="*"

counter_temp=0
counter2=0
while [[ $counter_temp -lt counter && $counter2 -lt 5 ]] 
do
# DOWNLOAD the GEM grib2 files from remote_location
    cd $grib_file_path
    for hour in `seq -f %03.0f $minhours 3 $maxhours`
    do
        for variable in ${!FILESS[*]}
        do
            fvariable=${FILESS[$variable]}
            wget -r -l1 --no-parent -c -nd -A $fvariable$date'*' $remote_location'/'$run_time'/'$hour'/'
        done
        #echo "boo"
    done
    #Counting number of files downloaded and restarting download if files are missing. Pausing 1 minute before start of download.    
    counter_temp=$( (find . -type f -name "*glb*"$date"*" | wc -l) )
    let counter2=counter2+1
    #sleep 1m 
done 

# LOOP over the basins/watersheds
for basin in ${!lats[*]}
#for basin in `seq 3 4`
do

    # Remove temporary files if they exist
    rm $temp_file_path/*.* -f

    # ASSIGN paths for Stored Files and Working folder
    watershed=${watersheds[$basin]}
    station=${stations[$basin]}

    # Create run directory
    mkdir -p $run_file_path$watershed'/'$date'16/GDPS/'

  output_file_path=$run_file_path$watershed'/'$date'16/GDPS/'

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
        for hour in `seq -f %03.0f 9 3 $maxhours`
        do

           # CLIP the grib2 to a basin-size rectangle and CONVERT the clipped file into a csv file
           f=$grib_file_path$FILES'latlon*'$date$run_time'_P'$hour'*.grib2'

          /home/ec2-user/grib2/wgrib2/wgrib2 $f -new_grid_interpolation neighbor -new_grid_winds earth -new_grid latlon $lon:$xcount:$xdelta $lat:$ycount:$ydelta $temp_file_path$names$hour'.tmp'

           /home/ec2-user/grib2/wgrib2/wgrib2 $temp_file_path$names$hour'.tmp' -csv $temp_file_path$names$hour'.csv'

           # Restructure the data in the csv files into a single-line-per-timestep file, and append all time-steps into another file
           gawk 'BEGIN { FS = "," }; { print $2, $3, $NF }' $temp_file_path$names$hour'.csv' > $temp_file_path$names$hour'_2.csv'

           gawk -f $awk_file_path'MESH_2.awk' $temp_file_path$names$hour'_2.csv' >> $temp_file_path$names'_3.csv'

        done

        # FORMAT the data into a matrix configuration as per r2c format
        gawk '{ print $0 }' $awk_file_path$'header_info.txt' > $temp_file_path$names'_4.csv'
        gawk '{ print $0 }' $temp_file_path$names'_3.csv' >> $temp_file_path$names'_4.csv'
        gawk -f $awk_file_path$'MESH_3_3Hourly.awk' $temp_file_path$names'_4.csv' > $temp_file_path$names'_5.csv'

        # APPEND the r2c body into the forcing file header
        gawk '{ print $0 }' $temp_file_path$names'_5.csv' >> $awk_file_path$'basin_'$names'.r2c'


 # MOVE the newly created forcing files into the run folder
        mv $awk_file_path$'basin_'$names'.r2c' $output_file_path$'basin_'$names'.r2c'

     done

done






