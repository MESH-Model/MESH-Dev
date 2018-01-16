#!/bin/bash
set -aex
#
# Script to extract CaPA data from Datamart and convert it to R2C format
# Provided the date 20170606 the script will retrieve data from 2017060516 to 2017060616 UTC-8
#
#   ********** FOR CaPA **********
#
# The date to run is passed as argument $1

# Working directories
# The home directory is an absolute path; other directories build from this path
home_dir='/net/san/pnrscience1/data/GEM_MESH/MESH_Forecast2'
awk_file_path=$home_dir/scripts
grib_file_path=$home_dir/GRIB
temp_file_path=$home_dir/TempFiles
run_file_path=$home_dir/capa_hindcasts

# Remote paths
remote_location_CaPA=http://dd.weatheroffice.gc.ca/analysis/precip/rdpa/grib2/polar_stereographic/06

# Programs
bin_wgrib2=/apps/share/bin/wgrib2

# Variable names (to name output files)
namess[1]='rain' #(CaPA)

# Variables names (to match GRIB files)
FILESS[1]='APCP-006-0700'       # kg/m2 (6h period), (CaPA)

# Watershed characteristics
watersheds[1]='05KJ001'
stations[1]='05KJ001'
lats[1]=47.577
lons[1]=-118.361
xcounts[1]=166
ycounts[1]=72

xdelta=0.125
ydelta=0.125

# Dates
# Date to run is passed as argument $1
if [ -z "$1" ]; then
    echo 'MISSING argument $1 should equal date to run'
    exit 1
fi

# Provided the date 20170606 the script will retrieve data from 2017060516 to 2017060616 UTC-8
dt=$(date -d  "$1 -1day" -u +%Y%m%d)
echo $dt

# Create a list of CaPA files to download from the remote location
# CaPA example filename date: 2017060706_000
set +x
date_list_CaPA=
    for hour in 06 12 18 ; do date_list_CaPA="$date_list_CaPA ${dt}${hour}_000" ; done
    for hour in 00 ; do date_list_CaPA="$date_list_CaPA $(date -d "$dt + 1 day" -u +%Y%m%d)${hour}_000"; done
set -x
date_list_CaPA=$date_list_CaPA

# 'cd' to the GRIB directory and download the files
cd $grib_file_path
for date in $date_list_CaPA
do
    for index in ${!FILESS[*]}
    do
        wget -r -l1 --no-parent -nd -A *${FILESS[$index]}*${date}*.grib2 $remote_location_CaPA/
    done
done

# Loop over the basins/watersheds
for basin in ${!watersheds[*]}
do

    # Assign the output file path
    watershed=${watersheds[$basin]}
    station=${stations[$basin]}
    output_file_path="${run_file_path}/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16"
    output_file_path2="${run_file_path}/${watershed}/SaMESHRun"

    # Create run directory if it does not exist
    [[ -d $output_file_path ]] || mkdir -p $output_file_path

    lat=${lats[$basin]}
    lon=${lons[$basin]}
    ycount=${ycounts[$basin]}
    xcount=${xcounts[$basin]}

    # Remove temporary files if they exist
    rm $temp_file_path/*${watershed}*.* -f

    # Loop over each variable to create the R2C output file
    for index in ${!FILESS[*]}
    do

		# Name of output file
        fout_r2c=basin_${namess[$index]}.r2c
		# Temporary files used in converting from GRIB to R2C
		fheader_info=$temp_file_path/${watershed}_header_info.txt
        fconvert=$temp_file_path/${watershed}_${FILESS[$index]}

        # Create a new R2C file with header information
        echo "$watershed,${namess[$index]},$lat,$lon,$xcount,$ycount,$xdelta,$ydelta" > $fheader_info
        gawk -f $awk_file_path/MESH_he.awk $fheader_info > $temp_file_path/$fout_r2c

        # Loop over each GRIB input file
        for date in $date_list_CaPA
        do

            # Clip the GRIB file to a basin-size rectangle
            fdatamart=$grib_file_path/*${FILESS[$index]}*${date}*.grib2
            $bin_wgrib2 $fdatamart -new_grid_interpolation neighbor -new_grid_winds earth -new_grid latlon $lon:$xcount:$xdelta $lat:$ycount:$ydelta \
                ${fconvert}_clipped.grib2

            # Convert the clipped GRIB file to CSV format
            $bin_wgrib2 ${fconvert}_clipped.grib2 -csv ${fconvert}.csv
            # Restructure the data in the CSV file into a single-line-per-timestep file
            gawk 'BEGIN { FS = "," }; { print $2, $3, $NF }' ${fconvert}.csv > ${fconvert}_2.csv

            # Append the time-step into another file
            gawk -f $awk_file_path/MESH_2.awk ${fconvert}_2.csv >> ${fconvert}_3.csv
        done

        # Aggregate the data to a single file
        gawk '{ print $0 }' $fheader_info > ${fconvert}_4.csv
        gawk '{ print $0 }' ${fconvert}_3.csv >> ${fconvert}_4.csv

        # Convert the data to framed matrices compatible with the R2C format
         gawk -f $awk_file_path/MESH_7.awk ${fconvert}_4.csv > ${fconvert}_5.csv
        

        # Append the framed matrices to the R2C file
        gawk '{ print $0 }' ${fconvert}_5.csv >> $temp_file_path/$fout_r2c
        # Move the R2C to the run directory
        mv $temp_file_path/$fout_r2c $output_file_path/$fout_r2c

     done

done

