#$ -S  /bin/bash
#run_fst2r2c.sh

#Directory for fst files
DIR_FILE="/home/nfs2/GEM_forcing"

#directory for FST2R2C EXECUTABLE
DIR_FST2R2C=

#CURRENT=$(date -d"2004-05-19 00:00:00")
CURRENT=$(date -d"2004-06-01 00:00:00")
TIMESTEP=12
NFILES=3652

#run FST2R2C
for ((i=1;i<=$NFILES;i++))
   do 
   FileName=$(date -d"$CURRENT" "+%Y%m%d%H")
   echo 'i = '$i
   fst2r2c_point $i $NFILES $DIR_FILE/$FileName
   CURRENT=$(date -d"$CURRENT $TIMESTEP hours")
done

