#!/bin/bash
#run_fst2r2c.sh
#----------------------------------------------------------------------
# This script facilitates providing input arguments to the FST2R2C
# program. It also automates creating multi-frame time series R2C files
# from time series FST files.
#
#    Muluneh Admass Mekonnen - January 09/2012
#    Global Institute for Water Security - University of Saskatchewan
#    Hydrometeorology and Arctic Laboratory - Environment Canada
#----------------------------------------------------------------------

#Directory for fst files
DIR_FILE="/data/reynolds2/mesh/donnees/forcage/reg15op_capant_0618/reg15op"

#directory for FST2R2C EXECUTABLE
DIR_FST2R2C=

# Argument 1 - Constructing FST file name
# Date of the first FST file to be processed
first_date=$(date -d"2001-10-01 00:00:00")

# Date of the last FST file to be processed
last_date=$(date -d"2006-01-31 00:00")

# The time (in hours) gap between two consecutive FST files. This
# is used to automate constructing the FST file name
TIMESTEP=12

# Argument 2 - FST variable name for the record to be extracted
varName=PR0

# Argument 3 - Beginning of forecast hour in a given FST file
ip2start=7

# Argument 4 - End of forecast hour in a given FST file
ip2end=18

# Argument 5 - R2C file name (Output file)
r2cName='basin_rain.r2c' 

# Argument 6 - Multiplying factor for unit conversion when a
# different unit is required for the R2C file. 
# From accumulated precip in m (in 1 hour) to mm/s = 1000/3600
#cfactorm=0.2777777777777778 
# From accumulated precip in m (in 1 hour) to mm = 1000
cfactorm=1000 

# Argument 7 - Additive factor for unit conversion when a
# different unit is required for the R2C file
cfactora=0.0 

# Argument 8 - R2C variable attribute name to be written in the
# R2C header file
attrName='Precipitation_rate_at_40_m'

# Argument 9 - R2C variable unit to be written in the R2C header file
attrUnit='mm'

# Argument 10 - Format for the data (body) section of the R2C file
r2cFormat='999(F12.7,1X)'

#----------------------------------------------------------------------
#User may not need to modify the commands below
ifirst_date=$(date -d"$first_date" "+%Y%m%d%H")
ilast_date=$(date -d"$last_date" "+%Y%m%d%H")

#run FST2R2C
icount=1
CURRENT=$(date -d"${first_date} $((icount-1)) hours")
Counter=1
while [[ $(date -d"$CURRENT" "+%Y%m%d%H") -ge ${ifirst_date} && $(date -d"$CURRENT" "+%Y%m%d%H") -le ${ilast_date} ]]
do
   FileName=$(date -d"$CURRENT" "+%Y%m%d%H")
   fst2r2c  $DIR_FILE/$FileName ${varName} ${ip2start} ${ip2end} ${r2cName} ${cfactorm} ${cfactora} ${attrName} ${attrUnit} ${r2cFormat} ${Counter}
   CURRENT=$(date -d"$CURRENT $TIMESTEP hours")
   icount=$((icount + 1))
   let Counter="$Counter+12"
done
#----------------------END OF SCRIPT-----------------------------------
