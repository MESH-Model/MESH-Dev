#$ -S /bin/bash
#run_dds.sh

#creates directories needed for MESH runs
#copies for files needed by MESH and DDS
#sets environment variables needed by MESH and DDS
#runs dds

#directory for scripts
DIR_SCRIPTS="$HOME/SCRIPTS"

#directory for dds
DIR_DDS="$HOME/DDS"

#directory for output files
mkdir -p BASINAVG1
mkdir -p CLASSOUT1 CLASSOUT2 CLASSOUT3 CLASSOUT4 CLASSOUT5

#copy input files
$DIR_SCRIPTS/copy_files.sh

#run script to set environment variables needed by MESH and DDS
#$DIR_SCRIPTS/set_env.sh
export LD_LIBRARY_PATH=/home/nfs1/armnlib/ssm-002/PGI-CMC_6.1-1_linux24-i386/etch/linux86/6.1/PORTABLE/liblf-linux86-g232

#to use batch files in the current directory - mesh_batch.bat
export PATH=$PATH:/$HOME/cluster/$HOSTNAME

#run dds
$DIR_DDS/dds
