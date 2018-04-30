#!/bin/bash

#*********
#*R Plots*
#*********

#Stations
#stations[1]='09AB001'
stations[1]='09BC002'
stations[2]='09BA001'
stations[3]='09DC006'
stations[4]='10AA001'

#Creating streamflow observations file for each station, that can be used by R to produce a plot.
     for i in ${!stations[*]}
     
     do

     station=${stations[$i]}

     dt=$(date --date='yesterday' -u +%Y%m%d)
#     dt=$(date -d "-2 day" -u +%Y%m%d)
     date_today=$(date -d "${dt} +1 day" -u +%Y%m%d)

    #Working directories
     home_dir=/home/ec2-user/Yukon_GDPS
     awk_file_path=$home_dir/scripts
     output_file_path_gem=$home_dir/gem_forecasts
     strm_file_path=$home_dir/Streamflow

     gawk -f $awk_file_path/$'MESH_5_DR.awk' $strm_file_path/${station}/Archive/Gauged/$'YT_'$station'_hourly_hydrometric_'${date_today}'.csv' > $strm_file_path/${station}/$'Streamflow_Observations.csv'
       
       if [ ${station} != '09BA001' ]
       then
       {
       cp ${output_file_path_gem}/${station}/${dt}16/RDPS/MESH_output_streamflow_ts.csv $strm_file_path/${station}/Archive/RDPS/MESH_output_streamflow_ts_RDPS_${station}_${date_today}.csv #Copying model values to the streamflow directory.

       cp ${output_file_path_gem}/${station}/${dt}16/GDPS/MESH_output_streamflow_ts.csv $strm_file_path/${station}/Archive/GDPS/MESH_output_streamflow_ts_GDPS_${station}_${date_today}.csv #Copying model values to the streamflow directory.
       }
       else
       {
       echo "09BA001" #Copying model values from 09BC002 to 09BA001 streamflow folder
       cp ${output_file_path_gem}/09BC002/${dt}16/RDPS/MESH_output_streamflow_ts.csv $strm_file_path/${station}/Archive/RDPS/MESH_output_streamflow_ts_RDPS_${station}_${date_today}.csv
       cp ${output_file_path_gem}/09BC002/${dt}16/GDPS/MESH_output_streamflow_ts.csv $strm_file_path/${station}/Archive/GDPS/MESH_output_streamflow_ts_GDPS_${station}_${date_today}.csv
       }
       fi
     done

#Plot forecasts for all stations in stations.txt
cd $awk_file_path
#Rscript $'plotR_all_stations_png.R'
Rscript $'plotR_all_stations_RDPS_GDPS_png_efficient.R'      

