
set(SA_MESH_routines_1
    # core routines
    shd_variables.f90
    print_routines.f90 
    program_end.f90 
    program_abort.f90
    module_files_variabletypes.f90 
    module_files_variables.f90
	module_dates.f90 
    FLAGS.f90
    control_variables.f90
    
    variable_names.f90 
    parameter_names.f90
    projection_variables.f90 
    fm_variables.f90 
    input_parameters.f90 
    model_variables.f90
    output_variables.f90 
    sa_mesh_common.f90
    resume_run.f90)

set(SA_MESH_routines_2 
    # Input routines
    read_initial_inputs.f90

    read_shed_r2c.f90 
    read_shed_nc.f90 
    read_shed_csv.f90 
    read_soil_levels.f90 
    read_shed_nc_subbasin.f90

    read_parameters.f90
    read_parameters_class.f90 
    read_parameters_hydrology.f90 
    read_parameters_r2c.f90 
    read_parameters_nc.f90 
    read_parameters_csv.f90

    check_parameters.f90

    read_basin_structures.f90

	read_streamflow_txt.f90 
    read_streamflow_tb0.f90 
    read_reservoir_txt.f90 
    read_reservoir_tb0.f90

	read_abstractionpoint_tb0.f90
    read_abstractionpoint_txt.f90

	read_soil_ini.f90

	read_initial_values_r2c.f90 
    read_initial_values_nc.f90
    
	module_files.f90

    #Output routines
    save_initial_values_nc.f90
	output_files.f90
    save_basin_output.f90

    # MESH driver
    read_run_options.f90 read_fews_runinfo_nc.f90
	sa_mesh_run_within_tile.f90 sa_mesh_run_within_grid.f90 sa_mesh_run_between_grid.f90
    
	resumerun_config.f90 resumerun_read.f90 resumerun_save.f90
	MESH_driver.f90
)



add_library(SA_MESH_ROUTINES_1 STATIC ${SA_MESH_routines_1})

add_library(SA_MESH_ROUTINES_2 STATIC ${SA_MESH_routines_2})

add_dependencies(SA_MESH_ROUTINES_1 MPI_MODULE IO_MODULE)
add_dependencies(SA_MESH_ROUTINES_2 
                    SA_MESH_ROUTINES_1 
                    SIM_STAT
                    WATROUTE_OLD
                    PERMAFROST
                    RESERVOIR_UPDATE
                    RPN_WATROUTE_MESH
                    IRRIGATION
                    BASEFLOW
                    CLASS_LIBRARY)

target_link_libraries(SA_MESH_ROUTINES_1 PUBLIC
                        MPI_MODULE    
                        IO_MODULE)
target_link_libraries(SA_MESH_ROUTINES_2 PUBLIC
                        SIM_STAT
                        WATROUTE_OLD
                        PERMAFROST
                        RESERVOIR_UPDATE
                        RPN_WATROUTE_MESH
                        IRRIGATION
                        BASEFLOW
                        CLASS_LIBRARY)

# target_include_directories(MPI_MODULE PUBLIC ${CMAKE_BINARY_DIR}/Modules/mpi_module/)
# # target_include_directories(IO_MODULE PUBLIC ${CMAKE_BINARY_DIR}/Modules/io_modules/)
# target_sources(${samesh_exe} PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/control_variables.f90)

# target_include_directories(${samesh_exe} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# target_link_libraries(${samesh_exe} PUBLIC
#                         MPI_OBJECTS 
#                         SIM_STAT 
#                         IRRIGATION
#                         IO_MODULE
#                         MOUNTAIN
#                         PERMAFROST
#                         STRING_MODULE
#                         LIBRMN
                        
#                         BLOWING_SNOW

#                         BASEFLOW
#                         RESERVOIR_UPDATE
#                         RPN_WATROUTE_MESH
#                         RPN_WATROUTE_CODE
#                         WATROUTE_OLD

#                         SVS_LIBRARY
#                         CLASS_LIBRARY
#                         )



# Optional: Add compiler flags
# target_compile_options(${samesh_exe} PRIVATE -O3)