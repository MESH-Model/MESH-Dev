#!/bin/sh
#copy_files.sh

#copies links and files needed by MESH and DDS

#directory for input files
DIR_FILES="$HOME/basins/ssrb"

DIR_SCRIPTS="$HOME/SCRIPTS"

#copy links of the drainage database file
cp -P $DIR_FILES/MESH_input_drainage_database.txt   MESH_input_drainage_database.txt
cp -P $DIR_FILES/new_shd.r2c                        new_shd.r2c
cp -P $DIR_FILES/MESH_drainage_database.r2c         MESH_drainage_database.r2c

#copy links of the forcing data
cp -P $DIR_FILES/basin_humidity.r2c                 basin_humidity.r2c
cp -P $DIR_FILES/basin_longwave.r2c                 basin_longwave.r2c
cp -P $DIR_FILES/basin_pres.r2c                     basin_pres.r2c
cp -P $DIR_FILES/basin_rain.r2c                     basin_rain.r2c
cp -P $DIR_FILES/basin_shortwave.r2c                basin_shortwave.r2c
cp -P $DIR_FILES/basin_temperature.r2c              basin_temperature.r2c
cp -P $DIR_FILES/basin_wind.r2c                     basin_wind.r2c

#copy link of soil level data
cp -P $DIR_FILES/MESH_input_soil_levels.txt         MESH_input_soil_levels.txt

#copy links of streamflow and reservoir files
cp -P $DIR_FILES/MESH_input_streamflow.txt          MESH_input_streamflow.txt
cp -P $DIR_FILES/MESH_input_reservoir.txt           MESH_input_reservoir.txt

#copy link to run options file
cp -P $DIR_FILES/MESH_input_run_options.ini         MESH_input_run_options.ini

#copy link for parameter ranges file
cp -P $DIR_FILES/minmax_parameters.txt              minmax_parameters.txt

#copy link to DDS_init file
cp -P $DIR_FILES/DDS_init.txt                       DDS_init.txt

#copy link to mesh batch file
cp -P $DIR_SCRIPTS/mesh_batch.bat                   mesh_batch.bat

#copy initialization files
cp $DIR_FILES/MESH_parameters_CLASS.ini             MESH_parameters_CLASS.ini
cp $DIR_FILES/MESH_parameters_hydrology.ini         MESH_parameters_hydrology.ini
