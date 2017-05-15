#!/bin/sh

#this script is made to identify the pixels of the experiment's domain
#which are located inside of the studied watershed;
#each pixel is attributed a weight depending on the fraction of the pixel included in the watershed; 
#this is useful to compute watershed averages in stand-alone svs, 
#especially the total runoff to compute the catchment outflow.
#output is stored in the file 'Output_Idx.txt'

# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"

package require TclData
package require Logger
Log::Start [info script] 0.1

# erase previously created files
catch { eval file delete [glob 4interp/Output*] }

#----- Open the shapefile
set layer [ogrfile open SHPFILE append 4interp/shape/shed.shp]
eval ogrlayer read LAYER [lindex $layer 0]

#----- Read the data to be summed
fstdfile open DATAFILE read SPS_output_grid.fst
fstdfield read DATAFIELD DATAFILE -1 "" 0 -1 -1 "" "O1"

###----- Creer une nouvelle couche
ogrfile open SHPFILE2 write 4interp/Output.shp "ESRI Shapefile"
 
#----- Clear the file that will be used to sum the data
ogrlayer define LAYER -field SLOP_basin Real
ogrlayer clear LAYER SLOP_basin 0.0

#----- Open a file to save the index for future reuse for faster processing
#      if the file is empty, it will be filled with the index
#      otherwise, it will be used as an index
set f [open 4interp/Output_Idx.txt {RDWR CREAT}]
#puts "f is : $f"
#
#----- Do the sum in conservative mode splitting the grid cell in 1 segment
puts "   Interpolating field values into polygon layer"
ogrlayer interp  LAYER DATAFIELD SLOP_basin NORMALIZED_CONSERVATIVE 1 True $f
ogrlayer stats LAYER -table var
ogrlayer write LAYER SHPFILE2
ogrfile close SHPFILE2
ogrfile close SHPFILE

#catch { eval file delete [glob 4interp/Output.*] }


Log::End
