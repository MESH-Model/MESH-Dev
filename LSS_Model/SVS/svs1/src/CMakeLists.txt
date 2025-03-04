target_sources(SVS_LIBRARY PRIVATE
                runsvs_mod_sa_mesh.ftn90 
                runsvs_utils.ftn90
                runsvs_init_mesh.ftn90

                runsvs_mesh.F90

        )

target_include_directories(SVS_LIBRARY PRIVATE ${CMAKE_SOURCE_DIR}/LSS_Model/SVS/svs1/svs_GEM_v5.8.rc2)
target_include_directories(SVS_LIBRARY PRIVATE ${CMAKE_SOURCE_DIR}/LSS_Model/SVS/svs1/rpnphy_5.8.3_all)
target_include_directories(SVS_LIBRARY PRIVATE ${CMAKE_SOURCE_DIR}/Modules/mpi_module)
