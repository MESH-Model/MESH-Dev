#!/bin/bash
set -aex
#
# Script to manage the files for the next gem forecast
# Provided the date 20170606 the script will retrieve data from 2017060516 to 2017060616 UTC-8
#
# The date to run is passed as argument $1

# Working directories
# The home directory is an absolute path; other directories build from this path
home_dir='/home/ec2-user/Yukon_GDPS'
#home_dir_tmp='/home/ec2-user/Yukon_streamline_scripts_tests'
awk_file_path=$home_dir/scripts
grib_file_path=$home_dir/GRIB
run_file_path_capa=$home_dir/capa_hindcasts
run_file_path_gem=$home_dir/gem_forecasts
run_file_path_capa_tmp=$home_dir/capa_hindcasts
run_file_path_gem_tmp=$home_dir/gem_forecasts

# Watershed characteristics
watersheds[1]='09BC002'
watersheds[2]='09DC006'
watersheds[3]='10AA001'

# Dates
# Date to run is passed as argument $1
if [ -z "$1" ]; then
    echo 'MISSING argument $1 should equal date to run'
    exit 1
fi

# Provided the date 20170606 the script will retrieve data from 2017060516 to 2017060616 UTC-8
dt=$(date -d $1 -u +%Y%m%d)

echo $dt

# Loop over the basins/watersheds
for basin in ${!watersheds[*]}
do

    # Assign the output file path
    watershed=${watersheds[$basin]}
    input_file_path_capa="${run_file_path_capa}/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16_to_${dt}16"
    input_file_path_gem="${run_file_path_gem}/${watershed}/$(date -d "$dt - 1 day" -u +%Y%m%d)16"
    output_file_path_gem="${run_file_path_gem_tmp}/${watershed}/${dt}16"

   # Obtain other mesh files that are unchanged from the previous gem forecast run
   cp ${input_file_path_gem}/MESH_drainage_database.r2c ${output_file_path_gem}
   [[ -f ${input_file_path_gem}/MESH_input_reservoir.txt ]] && cp ${input_file_path_gem}/MESH_input_reservoir.txt ${output_file_path_gem}
   [[ -f ${input_file_path_gem}/MESH_input_reservoir.tb0 ]] && cp ${input_file_path_gem}/MESH_input_reservoir.tb0 ${output_file_path_gem}
   cp ${input_file_path_gem}/MESH_input_soil_levels.txt ${output_file_path_gem}
   cp ${input_file_path_gem}/MESH_parameters_hydrology.ini ${output_file_path_gem}
   [[ -f ${input_file_path_gem}/MESH_parameters.r2c ]] && cp ${input_file_path_gem}/MESH_parameters.r2c ${output_file_path_gem}
   [[ -f ${input_file_path_gem}/minmax_parameters.txt ]] && cp ${input_file_path_gem}/minmax_parameters.txt ${output_file_path_gem}
   cp ${input_file_path_gem}/sa_mesh ${output_file_path_gem}

   # Obtain mesh files from the previous gem run that need changing
   # The dates in these files need changing
   cp ${input_file_path_gem}/RDPS/MESH_input_run_options.ini ${output_file_path_gem}
   cp ${input_file_path_gem}/MESH_input_run_options_180.ini ${output_file_path_gem}
   cp ${input_file_path_gem}/MESH_parameters_CLASS.ini ${output_file_path_gem}
   # The date and streamflow values in this file needs changing
   [[ -f ${input_file_path_gem}/MESH_input_streamflow.txt ]] && cp ${input_file_path_gem}/MESH_input_streamflow.txt ${output_file_path_gem} 
   [[ -f ${input_file_path_gem}/MESH_input_streamflow.tb0 ]] && cp ${input_file_path_gem}/MESH_input_streamflow.tb0 ${output_file_path_gem} 

   # Obtain mesh state files from the previous capa run
   if [ -f ${input_file_path_capa}/int_statVariables.seq.rte ]; then
      for name in rte runclass36 lzsp.wfqlz
	  do
         cp ${input_file_path_capa}/int_statVariables.seq.${name}_${dt}16 ${output_file_path_gem}
         cp ${output_file_path_gem}/int_statVariables.seq.${name}_${dt}16 ${output_file_path_gem}/int_statVariables.seq.${name}
	  done
   elif [ -f ${input_file_path_capa}/int_statVariables.seq.wf_route ]; then
      cp ${input_file_path_capa}/int_statVariables.seq.wf_route_${dt}16 ${output_file_path_gem}
      cp ${output_file_path_gem}/int_statVariables.seq.wf_route_${dt}16 ${output_file_path_gem}/int_statVariables.seq.wf_route
      cp ${input_file_path_capa}/int_statVariables.seq.runclass36_${dt}16 ${output_file_path_gem}
      cp ${output_file_path_gem}/int_statVariables.seq.runclass36_${dt}16 ${output_file_path_gem}/int_statVariables.seq.runclass36
   else
      cp ${input_file_path_capa}/int_statVariables_${dt}16.seq ${output_file_path_gem}
      cp ${output_file_path_gem}/int_statVariables_${dt}16.seq ${output_file_path_gem}/int_statVariables.seq
   fi

done

