dds_mesh_connector.exe
cd ..
SA_MESH_1_3.exe
copy BASINAVG1\MESH_output_echo_print.txt dds
copy BASINAVG1\MESH_output_streamflow.csv dds
cd dds
nash_MESH.exe
copy nash_daily.csv function_out.txt
