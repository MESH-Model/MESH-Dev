#$ -S  /bin/bash
#run_fst2r2c_pr.sh

#Directory for fst files
DIR_FILE="/home/nfs2/CaPA/6h-20100224-disag-1hr"

#directory for FST2R2C EXECUTABLE
DIR_FST2R2C=

CURRENT=$(date -d"2004-06-01 00:00:00")
TIMESTEP=1
istart=2
iend=2
CURRENT=$(date -d"$CURRENT $istart hours")

#run FST2R2C
for ((i=$istart;i<=$iend;i++))
   do 
   FileName=pr_an_$(date -d"$CURRENT" "+%Y%m%d%H")_
   echo 'i = '$i
   fst2r2cCaPA $i $iend $DIR_FILE/$FileName
   CURRENT=$(date -d"$CURRENT $TIMESTEP hours")
done

