#!/bin/sh
#mesh_batch.bat

#batch file needed by DDS to run MESH and the connector,
#the connector updates the initialization files based on
#recent parameters from the DDS

#directory of the connector executable
DIR_CONNECTOR="$HOME/CONNECTOR_DDS_MESH"

#directory of MESH executable
DIR_MESH="$HOME/SA_MESH_1_3"

#run the connector
$DIR_CONNECTOR/connector_dds_mesh

#run MESH
$DIR_MESH/sa_mesh

#copy output files
cp -p BASINAVG1/MESH_output_echo_print.txt MESH_output_echo_print.txt
cp -p BASINAVG1/MESH_output_streamflow.csv MESH_output_streamflow.csv
