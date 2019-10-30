#!/bin/bash
# create_list_of_bus_variables_used_by_svs.sh
# This script parses the physics and svs code to identify variables that are required by SVS in the physics bus
# It creates a files called list_of_bus_variables_used_by_svs.txt that is then used by create_runsvs_mod.sh
# to create the runsvs_mod module.

if [ "$1" == "" ]
then
	SVSDIR=svs_v1.1.3_cleanup
else
	SVSDIR=$1
fi
# Location of SVS source code
if [ "$1" == "" ]
then
        SVSDIR=svs_v1.1.3_cleanup
else
        SVSDIR=$1
fi
# Location of the runsvs source code
if [ "$2" == "" ]
then
        RUNSVSDIR=src
else
        RUNSVSDIR=$2
fi
# Location of the working directory containing the scripts and list of bus variables
if [ "$3" == "" ]
then
        WORKDIR=bin
else
        WORKDIR=$3
fi
NTILES=5
SVS=$SVSDIR/svs.ftn90
INISOILI=$SVSDIR/inisoili_svs.ftn90
INICOVER=$SVSDIR/inicover_svs.ftn90
PHYBUSINIT=$SVSDIR/phybusinit.ftn90
OUTFILE=$WORKDIR/list_of_bus_variables_used_by_svs.txt

# Parse the SVS source code to identify the physics and dynamics variables that need to be read in
# ------------------------------------------------------------------------------------------------

# Identify all physics variables used in SVS.FTN90, INISOILI_SVS.FTN90 and INICOVER_SVS.FTN90 
egrep -i "BUS\(X\([A-Z\ a-z,0-9_]*\)\)" -o $SVS | awk -F'(' '{print $3}' | awk -F',' '{print $1}' | awk '{print tolower($1)}' | sort -u > $WORKDIR/svs_var.out
grep -o -e "f([a-z0-9_]*" $INISOILI | sed -e 's/^f(//' -e 's/+$//' | sort -u > $WORKDIR/inisoili_var.out
grep -o -e "f([a-z0-9_]*" $INICOVER | sed -e 's/^f(//' -e 's/+$//' | sort -u > $WORKDIR/inicover_var.out

# Add dynamics variables (LOCBUS statements)
egrep "LOCBUS (.*,.*,.*)" $SVS | awk -F',' '{print $2}' | awk '{print tolower($1)}' | sort -u > $WORKDIR/dynamics_var.out

# Combine them in a single file
cat $WORKDIR/svs_var.out $WORKDIR/inisoili_var.out $WORKDIR/inicover_var.out $WORKDIR/dynamics_var.out | sort -u > $WORKDIR/all_var.out

# Find the definition for those variables in the physics dictionary
cat >$WORKDIR/phybusinit.out <<EOF
EOF
cat $WORKDIR/all_var.out | while read line
do
	awk -F',' '{if($1~"lsvs"||$1~"lisbasvs"||$1=="               Call gesdict (n"){if(gensub(/ /,"","g",$3)=="'$line'"){print $0}}}' $PHYBUSINIT >> $WORKDIR/phybusinit.out
done

# Determine the size of the variables for the column model
sed -e "s%'//%%g" -e "s%//'%%g" $WORKDIR/phybusinit.out | awk -F"'" '{split($1,s,",");print s[3],";",$2}' | sed -e "s/VN=//" -e "s/ON=//" -e "s/VD=//" -e "s/VS=//" -e "s/;VB=.*//" -e "s/@nm//" -e "s/;row\*/;/" -e "s/;row/;1/" -e "s/;slb/;1/" -e "s/;slc/;1/" -e "s/;nagg/;$NTILES/" > $OUTFILE

# Remove temporary files
rm -f $WORKDIR/*.out
