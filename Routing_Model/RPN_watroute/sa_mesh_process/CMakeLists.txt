add_library(RPN_WATROUTE_MESH STATIC
            rte_module.f90
        )

target_include_directories(RPN_WATROUTE_MESH PRIVATE 
        ${CMAKE_SOURCE_DIR}/Modules/mpi_module
    )
target_sources(RPN_WATROUTE_MESH PRIVATE 
${CMAKE_SOURCE_DIR}/Routing_Model/RPN_watroute/code/area_watflood.f
${CMAKE_SOURCE_DIR}/Modules/mpi_module/mpi_module.f90
)
# target_sources(${samesh_exe} PRIVATE ${RPN_WATROUTE_MESH})
target_link_libraries(RPN_WATROUTE_MESH PUBLIC 
                        MPI_MODULE
                        STRING_MODULE)