
# define paths and other variables
# use ABSOLUTE paths
#code used to create Sa_MESH forcing files using CaPA rainfall and RDPS (from T+6) forcing files
#run example:
#             ./gem_forecast.sh 20171205 ; xxxx-> assume process day is 2017 dec 05, and creat 7-day forecast forcing files
 


# define paths and other variables
# use ABSOLUTE paths
home_dir='/net/san/pnrscience1/data/GEM_MESH/MESH_Forecast2/'
remote_location='http://dd.weatheroffice.gc.ca/model_gem_regional/10km/grib2'
awk_file_path=$home_dir'scripts/'
grib_file_path=$home_dir'GRIB/'
temp_file_path=$home_dir'TempFiles/'
gem_file_path=$home_dir'gem_forecasts/'
capa_file_path=$home_dir'capa_hindcasts/'
bin_wgrib2=/apps/share/bin/wgrib

watersheds[1]='05KJ001'
stations[1]='05KJ001'
lats[1]=47.577
lons[1]=-118.361
xcounts[1]=166
ycounts[1]=72
xdelta=0.125
ydelta=0.125
a="*"

if [ -z ${1+x} ];then
dt=$(date  -u +%Y%m%d)
else
dt=$1 
fi
yest=$(date -d  "$dt -1day" -u +%Y%m%d)
endt=$(date -d  "$dt 1day" -u +%Y%m%d)
# LOOP over the basins/watersheds
for basin in ${!lats[*]}
do

    # ASSIGN paths for Stored Files and Working folder
    watershed=${watersheds[$basin]}        
    station=${stations[$basin]}

  model_file_path=$gem_file_path$watershed'/SaMESHRunShort'
  input_file_path_gem=$gem_file_path$watershed'/'$yest'16_to_'$endt'16/'
  input_file_path_capa=$capa_file_path$watershed'/'$(date -d "$yest" -u +%Y%m%d)'16_to_'$(date -d "$endt -1day" -u +%Y%m%d)'16/'
  
  
   echo $input_file_path_gem $model_file_path
   
      # ------------------- GEM -------------------------------
      cd $model_file_path
  for i in $(ls -d $input_file_path_capa/int*.*); do  ln -s -f $i; done
 ln -s -f $input_file_path_gem/basin*.r2c $model_file_path
 cp $model_file_path/MESH_input_run_options.ini $model_file_path/MESH_input_run_options.ini.orj
 cp $model_file_path/MESH_parameters_CLASS.ini $model_file_path/MESH_parameters_CLASS.ini.orj
 n=2
 echo $(date -d  $yest -u +%Y) $(date -d  $yest -u +%m) $(date -d  $yest -u +%d) 16 $n $model_file_path
 python -t  $awk_file_path/modify_runtime_gem.py  $(date -d  $yest -u +%Y) $(date -d  $yest -u +%m) $(date -d  $yest -u +%d)  16 $n $model_file_path
echo "this is great!" $model_file_path
done

if [ $# -gt 0 ]; then
cd $model_file_path
./sa_mesh_static 
fi
cd $model_file_path/BASINAVG1/
sed -n '2, $ p' MESH_output_streamflow_ts.csv > 1.output
cat  $capa_file_path$watershed/SaMESHRunShort/BASINAVG1/MESH_output_streamflow_ts.csv 1.output > MESH_output_streamflow_ts_final.csv                                                                                                
