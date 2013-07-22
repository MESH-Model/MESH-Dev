#$ -S /bin/bash
#run_dds_node.sh

#runs dds on any of the available nodes
#to be used with the qsub command

#directory of batch files
DIR_SCRIPTS="$HOME/SCRIPTS"

#simulation directory
DIR_RUN="$HOME/cluster/$HOSTNAME"

#change the directory to simulation directory
cd $DIR_RUN

echo $HOSTNAME > output
/bin/date >> output

#run dds
$DIR_SCRIPTS/run_dds.sh > output_$HOSTNAME.txt
