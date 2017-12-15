#!/bin/bash
# set -aex
 set -ae
#Forecast_DR.sh is an updated and modified version of the script Forecast.sh developed by Arcadio Rodriguez-Prado. Contact Dominique Richard for details at dominique.richard@usask.ca. 

#This script gets data, organizes the files to run Mesh, runs Mesh for 3 sub-basins of the Yukon river basin, which includes 4 stations: 09BC002, 09BA001, 09DC006 and 10AA001. The script to plot forecast graphs is called at the end.

# dt is today's date
# Provided the date 20170606 the script will retrieve CaPA data from 2017060516 to 2017060616 UTC-8 and GEM values from 20170606 to 20170608.

dt=$(date  -u +%Y%m%d)
capa_run_days=3

# option to assign dt to an early date; can be used for case study.
dt=$(date -d "2017-12-12" -u +%Y%m%d)

echo "CaPA simulation will start at 17 (UTC-7)  $(date -d  "$dt -${capa_run_days}days" -u +%F), and end at 17(UTC-7)  $(date -d  "$dt -1day" -u +%F)"
echo "GEM simulation will start at 17 (UTC-7) $(date -d  "$dt -1day" -u +%F), and end at 17(UTC-7) $(date -d  "$dt 1day" -u +%F)"
sleep 15
    

# Watersheds
watersheds[1]='05KJ001'

#Stations
stations[1]='05KJ001'



#Working directories
home_dir=/fs/home/fs1/eccc/oth/nlab_west/aql000/my_proj_hall2/MESH_Forecast
awk_file_path=$home_dir/scripts
output_file_path_capa=$home_dir/capa_hindcasts
output_file_path_gem=$home_dir/gem_forecasts
strm_file_path=$home_dir/Streamflow

echo $dt
dt2=$(date  -u +%Y%m%d)
echo $dt2

#Getting CaPA data (data that incorporate actual precip data with modelled values) from two days ago 16:00 until yesterday 16:00 local Yukon time.
# only download current data from the ECCC datamart 
 if [ "$dt" = "$dt2" ]; then 
  echo " Now download GRIB2 forecast data from the weather office datamart."
 $awk_file_path/get_capa.sh $dt

#Getting GEM (the files contain only modelled values) from yesterday 16:00 until tomorrow 16:00. That represents 48 hours. 54 hours are available but we do not use values from the first 6 hours of spinup.
 $awk_file_path/get_gem_forecast.sh $dt
 
 fi
#run capa_hindcast
 $awk_file_path/capa_hindcast.sh $capa_run_days $dt
 
#run 2-day RDPS forecast 
 $awk_file_path/gem_forecast.sh $dt 
 
#Getting the observed streamflow data from 16:00 yesterday.
# $awk_file_path/get_streamflow.sh $dt

#Plots*
#*********
matlab  -nodesktop -nosplash -r "streamflow_all"; quit
# The following script calls plotR_all_stations_png.R.
#$awk_file_path/Rplot_loop_2.sh

#The following command allows to email plots from sender to recipients specified in email.sh. 
#$awk_file_path/email.sh
