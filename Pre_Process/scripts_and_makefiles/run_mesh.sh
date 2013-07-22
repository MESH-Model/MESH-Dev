#$ -S /bin/sh
#run_mesh.sh

#creates directories needed by MESH
#runs a script to copy files needed by MESH
#runs MESH

#directory for mesh executable
DIR_MESH="$HOME/SA_MESH_1_3"

#directory of batch files
DIR_SCRIPTS="$HOME/SCRIPTS"

#make directories needed for MESH output
mkdir -p BASINAVG1 
mkdir -p CLASSOUT1 CLASSOUT2 CLASSOUT3 CLASSOUT4 CLASSOUT5

#run a script - copy links to input files, copy initialization files
$DIR_SCRIPTS/copy_files.sh

#set additional environment variables
$DIR_SCRIPTS/set_env.sh

#run mesh
$DIR_MESH/sa_mesh
