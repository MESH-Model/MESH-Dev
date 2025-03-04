add_library(WATROUTE_OLD
            WF_ROUTE_config.f90 
            WF_ROUTE.f
            WF_ROUTE_module.f90
        )

target_link_libraries(WATROUTE_OLD PUBLIC
                        SA_MESH_ROUTINES_1)                                               

# target_sources(${samesh_exe} PRIVATE ${WATROUTE_OLD})