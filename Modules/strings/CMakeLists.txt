# CMake configuration for String Parsing module

set(samesh_exe "sa_mesh" PARENT_SCOPE)

# Add the source files for this module
add_library(STRING_MODULE STATIC
    precmod.f90
    stringmod.f90
)

target_link_libraries(STRING_MODULE PUBLIC IO_MODULE)
# target_link_libraries(STRING_MODULE PUBLIC 
#                         MPI_OBJECTS
#                     )

# Add these files to the main target
# target_sources(sa_mesh PRIVATE ${STRING_MODULE})
