
# define paths and other variables
# use ABSOLUTE paths
#code used to create Sa_MESH forcing files using CaPA rainfall and RDPS (from T+6) forcing files
#run example:
#            1) ./capa_hindcast.sh ; with no arguments --> add forcing data to existing longrun forcing files.
#            2) ./capa_hindcast.sh 7 ; --> creat last 7 days forecast forcing files for short run
#            3) ./capa_hindcast.sh 7 20171205 ; --> assume process day is 2017 dec 05, and creat 7-day forecast forcing files
 
option=$1 # if option =1, program will add foceing data to the longrun forcing fiel, otherwise  will create a new forcing file with headers. 

if [ $# -eq 0 ]; then 
daysback=1
writeheader=0
else
# daysback is used to set the simulation period in days
daysback=$1 # daysback is used to set the simulation period in days
writeheader=1 # writeheader =1, header section will be written into the forcing files. Set it to "0" if add data to existing files.
fi


# define paths and other variables
# use ABSOLUTE paths
home_dir='/fs/home/fs1/eccc/oth/nlab_west/aql000/my_proj_hall2/MESH_Forecast/'
remote_location='http://dd.weatheroffice.gc.ca/model_gem_regional/10km/grib2'
awk_file_path=$home_dir'scripts/'
grib_file_path=$home_dir'GRIB/'
temp_file_path=$home_dir'TempFiles/'
gem_file_path=$home_dir'gem_forecasts/'
capa_file_path=$home_dir'capa_hindcasts/'
bin_wgrib2=/fs/ssm/eccc/cmo/cmoi/apps/wgrib2/2.05-ipolate/ubuntu-14.04-amd64-64/bin/wgrib2


namess[1]="humidity"
namess[2]="longwave"
namess[3]="pres"
namess[4]="shortwave"
namess[5]="temperature"
namess[6]="wind"
namess[7]="rain"


FILESS[1]="*SPFH_TGL_2*"  # kg/kg
FILESS[2]="*DLWRF_SFC*" # J/m2
FILESS[3]="*PRES_SFC*"  # Pa
FILESS[4]="*DSWRF_SFC*" # J/m2 accumulated
FILESS[5]="*TMP_TGL_2*"   # K
FILESS[6]="*WIND_TGL_40*"  # m/s at 40 m
FILESS[7]="*APCP_SFC*" # kg/m2 



watersheds[1]='05KJ001'
stations[1]='05KJ001'
lats[1]=47.577
lons[1]=-118.361
xcounts[1]=166
ycounts[1]=72
xdelta=0.125
ydelta=0.125
a="*"

for n in $(seq $daysback -1 1)
do
if [ -z ${2+x} ];then
dtnow=$(date  -u +%Y%m%d)
else
dtnow=$2 
fi
echo "This is the day you are at  " $dtnow

dt=$(date -d  "$dtnow -${n}day" -u +%Y%m%d)
yest=$(date -d  "$dt -1day " -u +%Y%m%d)
endt=$(date -d  "$dt 1day" -u +%Y%m%d)
echo "n= " $n
    # DOWNLOAD the GEM grib2 files from remote_location
# LOOP over the basins/watersheds
for basin in ${!lats[*]}
do

    # ASSIGN paths for Stored Files and Working folder
    watershed=${watersheds[$basin]}        
    station=${stations[$basin]}

    # Create run directory
 if [ $# -eq 0 ]; then
  model_file_path=$capa_file_path$watershed'/SaMESHRunLong'
 else
  model_file_path=$capa_file_path$watershed'/SaMESHRunShort'
 fi
  input_file_path_gem=$gem_file_path$watershed'/'$yest'16_to_'$endt'16/'
  input_file_path_capa=$capa_file_path$watershed'/'$yest'16_to_'$(date -d "$endt -1day" -u +%Y%m%d)'16/'
  output_file_path_capa=$capa_file_path$watershed'/'$(date -d "$endt -1day" -u +%Y%m%d)'16_to_'$(date -d "$endt" -u +%Y%m%d)'16/'
   echo $input_file_path_capa $output_file_path_capa $model_file_path
   
      # ------------------- GEM -------------------------------
    # LOOP over each variable on the saved grib2 files to create the GEM forcing files
    for index in ${!namess[*]}
    do

        # CREATE the forcing file header
        names=${namess[$index]}
        FILES=${FILESS[$index]}
        if ((writeheader == 1)) && ((n==daysback));  then
      #  touch $awk_file_path$'header_info.txt'
        echo "$watershed,$names,$lat,$lon,$xcount,$ycount,$xdelta,$ydelta" > $awk_file_path$'header_info.txt' 
        gawk -f $awk_file_path'MESH_he.awk' $awk_file_path$'header_info.txt' > $model_file_path'/basin_'$names'.r2c'
       fi
       if ((index < 7)); then
       
        # APPEND the r2c body into the forcing file header
# remove forecast before 00Z to merge with the file that Dan produced which stop at 23 lcl (UT8) time, this code only use once.
#         cat $input_file_path_2'basin_'$names'.r2c' | sed -n '/19 00:00:00.000/,$p' >1.output

# append 24-hour forecast from GEM to the capa longrun forcing file  
                 file=$input_file_path_gem'basin_'$names'.r2c'
                 if [ -f $file ]; then
                   cat $input_file_path_gem'basin_'$names'.r2c' | sed -e '1,/endHeader/d' > 1.output
		   cat 1.output >> $model_file_path'/basin_'$names'.r2c'
		 else
                   echo $file "not exist; check if data on the datamart are ready."
	         sleep 12000 
		 fi
       else
              file=$input_file_path_capa'basin_'$names'.r2c'
                 if [ -f $file ]; then
	            cat $input_file_path_capa'/basin_'$names'.r2c' | sed -e '1,/endHeader/d' >> $model_file_path'/basin_'$names'.r2c'
		  
	         else
	         echo $file "not exist; check if data on the datamart are ready."
	         sleep 12000    
                 fi
        # MOVE the newly created forcing files into the run folder 
      fi
      done


done
 if ((n==$1)); then 
 cp $input_file_path_capa/int*.* $model_file_path
 cp $model_file_path/MESH_input_run_options.ini $model_file_path/MESH_input_run_options.ini.orj
 cp $model_file_path/MESH_parameters_CLASS.ini $model_file_path/MESH_parameters_CLASS.ini.orj
 python -t  $awk_file_path/modify_runtime.py  $(date -d  $yest -u +%Y) $(date -d  $yest -u +%m) $(date -d  $yest -u +%d)  16 $n
 fi
 
done

if [ $# -gt 0 ]; then
cd $model_file_path
./mpi_sa_mesh
if [ -d "$output_file_path_capa" ]; then
echo $output_file_path_capa " exists"
cp $model_file_path/int*.* $output_file_path_capa
else
echo $output_file_path_capa " does not exists"
mkdir  -p $output_file_path_capa
cp $model_file_path/int*.* $output_file_path_capa
fi
fi

