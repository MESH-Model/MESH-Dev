#!/bin/bash

set -aex

#Forecast_DR.sh is an updated and modified version of the script Forecast.sh developed by Arcadio Rodriguez-Prado. Contact Dominique Richard for details at dominique.richard@usask.ca. 
#This script's purpose is to produce a streamflow forecast for the Liard, Stewart, Pelly and Ross rivers for the time period yesterday 16:00 (UTC-8) to tomorrow 16:00 (UTC-8). A forecast for past dates is not possible due to the lack of online access to GEM values.

#This script gets data, organizes the files to run Mesh, runs Mesh for 3 sub-basins of the Yukon river basin, which includes 4 stations: 09BC002, 09BA001, 09DC006 and 10AA001. The script to plot forecast graphs is called at the end.

# Yesterday's date
# Provided the date 20170606 the script will retrieve CaPA data from 2017060516 to 2017060616 UTC-8 and GEM values from 20170606 to 20170608.

dt=$(date --date='yesterday' -u +%Y%m%d)
#dt=$(date -d "-6 day" -u +%Y%m%d)

# Watersheds
#watersheds[1]='09AB001'
watersheds[1]='09BC002'
watersheds[2]='09DC006'
watersheds[3]='10AA001'

#Stations
#stations[1]='09AB001'
stations[1]='09BC002'
stations[2]='09BA001'
stations[3]='09DC006'
stations[4]='10AA001'

#Working directories
home_dir=/home/ec2-user/Yukon
awk_file_path=$home_dir/scripts
output_file_path_capa=$home_dir/capa_hindcasts
output_file_path_gem=$home_dir/gem_forecasts
strm_file_path=$home_dir/Streamflow

#Getting CaPA data (data that incorporate actual precip data with modelled values) from two days ago 16:00 until yesterday 16:00 local Yukon time.

$awk_file_path/get_capa.sh $dt

#Getting GEM (the files contain only modelled values) from yesterday 16:00 until tomorrow 16:00. That represents 48 hours. 54 hours are available but we do not use values from the first 6 hours of spinup.

$awk_file_path/get_gem_forecast.sh

#Getting the observed streamflow data from 16:00 yesterday.

$awk_file_path/get_streamflow.sh $dt

#Setting up CaPA folders for all watersheds to produce state variable files. The saveresume and resume flags are set to 5 already.

$awk_file_path/setup_capa_hindcast_mod.sh $dt

###############
#CaPA Hindcast#
###############    

#for loop to run MESH for all watersheds specified above and create the state variable files with yesterday's date.

    for i in ${!watersheds[*]}
    do
    watershed=${watersheds[$i]}
    
    #Change date in MESH_input_run_options.ini file. ### need to change NR in awk files if file formats change ###
   
    gawk -f $awk_file_path/$'MESH_op_1_DR.awk' "$output_file_path_capa/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16/MESH_input_run_options.ini" > "$output_file_path_capa/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16/MESH_input_run_options_updated.ini"
    
    cp ${output_file_path_capa}/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16/MESH_input_run_options_updated.ini ${output_file_path_capa}/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16/MESH_input_run_options.ini
    
    #Run MESH (Hindcast using CaPA deterministic data)
    cd $output_file_path_capa/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16

   ./sa_mesh

    #Rename the newly created state variable file .seq with yesterday's date.
     cp ${output_file_path_capa}/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16/int_statVariables.seq.runclass36 ${output_file_path_capa}/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16/int_statVariables.seq.runclass36_${dt}16
     cp ${output_file_path_capa}/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16/int_statVariables.seq.wf_route ${output_file_path_capa}/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16/int_statVariables.seq.wf_route_${dt}16
   done


#Setting up GEM folders for the 4 stations to produce forecast. The saveresume flag is set to 0 and the resume flag, to 5 already.

$awk_file_path/setup_gem_forecast_mod.sh $dt

######################
#GEM 48-hour Forecast#
######################

    for i in ${!watersheds[*]}
    do
    watershed=${watersheds[$i]}
    
#Change start and/or end dates in MESH_input_run_options.ini, MESH_input_streamflow.txt and MESH_parameters_CLASS.ini files. **need to change NR in awk files if file formats change** 

    gawk -f ${awk_file_path}/'MESH_op_1_DR.awk' ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/'MESH_input_run_options.ini' > $output_file_path_gem/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/'MESH_input_run_options_updated.ini'

    cp ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/MESH_input_run_options_updated.ini ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/MESH_input_run_options.ini

    gawk -f ${awk_file_path}/'MESH_st.awk' ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/MESH_input_streamflow.txt > ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/'MESH_input_streamflow_updated.txt'

    cp ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/MESH_input_streamflow_updated.txt ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/MESH_input_streamflow.txt

    gawk -f $awk_file_path/'MESH_cl_DR.awk' ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/'MESH_parameters_CLASS.ini' > ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/'MESH_parameters_CLASS_updated.ini'

    cp ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/MESH_parameters_CLASS_updated.ini ${output_file_path_gem}/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/MESH_parameters_CLASS.ini


    #Run MESH (Forecast using GEM input model data)
    cd $output_file_path_gem/${watershed}/${dt}16_to_$(date -d "$dt 2 day" -u +%Y%m%d)16/

    ./sa_mesh

    cp MESH_output_streamflow_ts.csv MESH_output_streamflow_ts_$(date -d "$dt 1 day" -u +%Y%m%d).csv
   
done

#*********
#*R Plots*
#*********
# The following script calls plotR_all_stations_png.R.
$awk_file_path/Rplot_loop_2.sh

#The following command allows to email plots from sender to recipients specified in email.sh. 
#$awk_file_path/email.sh
