add_library(IRRIGATION
            cropland_irrigation_variables.f90
            cropland_irrigation_init.f90
            cropland_irrigation_within_tile.f90 
            cropland_irrigation_between_grid.f90
            calc_ET0.f90
            irrigation_demand_module.f90
        )

target_link_libraries(IRRIGATION PUBLIC
                        MPI_MODULE
                        SA_MESH_routines_1
                        IO_MODULE)

add_dependencies(IRRIGATION SA_MESH_ROUTINES_1)

target_sources(IRRIGATION PUBLIC
                ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/module_files_variabletypes.f90
                ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/module_files_variables.f90
                ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/control_variables.f90
                ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/shd_variables.f90
                ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/fm_variables.f90
                ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/input_parameters.f90
                ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/model_variables.f90
                ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/output_variables.f90
                ${CMAKE_SOURCE_DIR}/Driver/MESH_Driver/sa_mesh_common.f90)

target_include_directories(IRRIGATION PRIVATE 
        ${CMAKE_SOURCE_DIR}/Driver/MESH_driver
    )


# target_sources(sa_mesh PRIVATE ${IRRIGATION})