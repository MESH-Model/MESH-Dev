set(samesh_exe "sa_mesh" PARENT_SCOPE)

add_library(MPI_MODULE STATIC
            mpi_flags.f90 
            mpi_shared_variables.f90 
            mpi_utilities.f90
            mpi_module.f90
            )

target_sources(MPI_MODULE PRIVATE 
        ${CMAKE_CURRENT_SOURCE_DIR}/mpi_stub.f90
    )

    
# target_sources(sa_mesh PRIVATE ${MPI_OBJECTS})