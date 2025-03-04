
set(samesh_exe "sa_mesh" PARENT_SCOPE)
# Add the source files for this module
add_library(SIM_STAT
    SIMSTATS_config.f90
    KGE.f90 
    NSE.f90 
    SAE.f90 
    SORT.f90 
    SAESRT.f90 
    calc_drms.f90 
    calc_abserr.f90
    SIMSTATS.f90
)

target_link_libraries(SIM_STAT PUBLIC
                    SA_MESH_ROUTINES_1)   
# # Add these files to the main target
# target_sources(SIM_STAT PRIVATE 
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_driver/module_files_variables.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_driver/module_files_variabletypes.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_driver/FLAGS.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_driver/module_dates.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_driver/control_variables.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/shd_variables.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_driver/sa_mesh_common.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/fm_variables.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/input_parameters.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/model_variables.f90
#             ${CMAKE_SOURCE_DIR}/Modules/io_modules/mesh_io_constants.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/output_variables.f90
#             ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/print_routines.f90
#             )


target_include_directories(SIM_STAT PRIVATE 
        ${CMAKE_SOURCE_DIR}/Driver/MESH_driver
    )

# target_sources(sa_mesh PRIVATE ${SIM_STAT})
