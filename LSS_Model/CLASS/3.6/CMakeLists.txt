# Define the CLASS library
add_library(CLASS_LIBRARY STATIC)

# Add source files for the SVS library
add_subdirectory(sa_mesh_process)
add_subdirectory(src)


# Link dependencies
target_link_libraries(CLASS_LIBRARY PUBLIC 
MPI_MODULE
SA_MESH_ROUTINES_1
BLOWING_SNOW)



