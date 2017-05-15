#!/bin/sh
#

set -ae
set -x

#Etienne Gaborit, september 2015
#script made to merge all r2c forcing files generated with the script "SVS_forcings.sh"
#into a unique met file that will be read by stand-alone SVS.
#the user should adjust the first and last year values,
#and may have to adjust the variables being processed.

first_year=2006
last_year=2011

repentree=../fst_converted/forcings
outputfile=output_file.met
cd $repentree

if [ -f $outputfile ] ; then
    rm -f $outputfile
fi
touch $outputfile

year=$first_year
#loop over the years
while [ $year -le $last_year ] ; do
    # concatenate all variables for a given year using a column-wise merging
    paste ${year}_FB.r2c ${year}_FI.r2c  ${year}_PR.r2c ${year}_TT.r2c \
    ${year}_HU.r2c  ${year}_UU.r2c  ${year}_VV.r2c  ${year}_P0.r2c \
    ${year}_VEGH.r2c  ${year}_VEGL.r2c | pr -t -> ${year}.r2c
    
    #concatenate all years into a unique met file; here we append at the end of the output met file.
    cat ${year}.r2c >> $outputfile 
 
    let year=year+1
done


#-------------------------------------------
echo "fusion des fichiers de forçage effectuée"
#-------------------------------------------
