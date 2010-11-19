#$ -S /bin/bash
#set_env.sh

#sets environment variables needed by MESH and DDS runs

#additional library
export LD_LIBRARY_PATH=/home/nfs1/armnlib/ssm-002/PGI-CMC_6.1-1_linux24-i386/etch/linux86/6.1/PORTABLE/liblf-linux86-g232

#to use batch files in the current directory - mesh_batch.bat
export PATH=$PATH:/$HOME/cluster/$HOSTNAME
