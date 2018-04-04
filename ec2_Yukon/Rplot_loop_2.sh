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

     #Working directories
     home_dir=/home/ec2-user/Yukon
     awk_file_path=$home_dir/scripts
     output_file_path_gem=$home_dir/gem_forecasts
     strm_file_path=$home_dir/Streamflow

     gawk -f $awk_file_path/$'MESH_5_DR.awk' $strm_file_path/${station}/$'YT_'$station*'.csv' > $strm_file_path/${station}/$'Streamflow_Observations.csv'
       
       if [ ${station} != '09BA001' ]
       then
       {
       cp ${output_file_path_gem}/${station}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/MESH_output_streamflow_ts.csv $strm_file_path/${station} #Copying model values to the streamflow directory.
       }
       else
       {
       echo "09BA001" #Copying model values from 09BC002 to 09BA001 streamflow folder
       cp ${output_file_path_gem}/09BC002/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/MESH_output_streamflow_ts.csv $strm_file_path/${station}
       }
       fi
     done

#Plot forecasts for all stations in stations.txt
cd $awk_file_path
Rscript $'plotR_all_stations_png.R'
      

