add_library(RESERVOIR_UPDATE
            module_reservoir.f90 
            zonebased_reservoir_release.f90
        )

target_link_libraries(RESERVOIR_UPDATE PUBLIC
        SA_MESH_ROUTINES_1)    