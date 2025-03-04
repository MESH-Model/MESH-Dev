cmake_minimum_required(VERSION 3.14)

# Define output executable name (default)
set(samesh_exe "sa_mesh")

# Find NetCDF
FIND_PATH(NETCDFC_FOUND libnetcdf.a ${NETCDF_C_DIR}/lib)
FIND_PATH(NETCDFF_FOUND libnetcdff.a ${NETCDF_FORTRAN_DIR}/lib)
MESSAGE("NETCDFC_FOUND = ${NETCDFC_FOUND}")
MESSAGE("NETCDFF_FOUND = ${NETCDFF_FOUND}")

# Project configuration
project(MESH_cmake VERSION 1.0 LANGUAGES Fortran C)


# Define options
option(ENABLE_DEBUG "Enable debugging symbols" OFF)
option(ENABLE_DOUBLE "Enable double precision" OFF)
option(ENABLE_MPI "Enable MPI support" OFF)
option(USE_GFORTRAN "Use gfortran compiler" ON)

# Compiler selection
if(USE_GFORTRAN)
    set(CMAKE_Fortran_COMPILER gfortran)
    set(CMAKE_C_COMPILER gcc)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -cpp")   

else()
    set(CMAKE_Fortran_COMPILER ifort)
    set(CMAKE_C_COMPILER icc)
endif()



# Directory definitions
set(DIR_REPO ${CMAKE_CURRENT_SOURCE_DIR})

# Add MESH Driver directory
add_subdirectory(Driver/MESH_Driver)

# Include subdirectories
add_subdirectory(Modules/strings)
add_subdirectory(Modules/mpi_module)
add_subdirectory(Modules/simulation_statistics)
add_subdirectory(Modules/irrigation_demand)
add_subdirectory(Modules/permafrost_outputs)

add_subdirectory(Blowing_Snow)

add_subdirectory(Routing_Model/baseflow_module)

add_subdirectory(Modules/io_modules)
add_subdirectory(Modules/mountain_module)

add_subdirectory(LSS_Model/CLASS/3.6)
add_subdirectory(LSS_Model/SVS/svs1)

add_subdirectory(Modules/librmn/19.7.0)


add_subdirectory(Routing_Model/reservoir_update)
add_subdirectory(Routing_Model/WatRoute_old)
add_subdirectory(Routing_Model/RPN_watroute/sa_mesh_process)
add_subdirectory(Routing_Model/RPN_watroute/code)

add_executable(${samesh_exe} ${DIR_REPO}/Driver/MESH_Driver/MESH_driver.f90)

target_link_libraries(${samesh_exe} PUBLIC
                        MPI_MODULE 
                        STRING_MODULE
                        IO_MODULE
                        SA_MESH_ROUTINES_1
                                               

                        BASEFLOW
                        PERMAFROST
                        IRRIGATION
                        
                        SIM_STAT 
                        BLOWING_SNOW
                        CLASS_LIBRARY
                        

                        MOUNTAIN
                        
                        SVS_LIBRARY
                        LIBRMN
                                         
                        RESERVOIR_UPDATE
                        WATROUTE_OLD
                        RPN_WATROUTE_MESH
                        RPN_WATROUTE_CODE
                        
                        SA_MESH_ROUTINES_2
                        
                        )
                        
target_include_directories(MPI_MODULE PUBLIC ${CMAKE_BINARY_DIR}/Modules/mpi_module/)
target_include_directories(IO_MODULE PUBLIC ${CMAKE_BINARY_DIR}/Modules/io_modules/)
target_include_directories(SIM_STAT PUBLIC ${CMAKE_BINARY_DIR}/Modules/simulation_statistics/)
target_include_directories(STRING_MODULE PUBLIC ${CMAKE_BINARY_DIR}/Modules/strings/)
target_include_directories(IRRIGATION PUBLIC ${CMAKE_BINARY_DIR}/Modules/irrigation_demand/)

target_include_directories(BLOWING_SNOW PUBLIC ${CMAKE_BINARY_DIR}/Blowing_Snow)

target_include_directories(SA_MESH_ROUTINES_1 PUBLIC ${CMAKE_BINARY_DIR}/Driver/MESH_Driver/)
target_include_directories(SA_MESH_ROUTINES_2 PUBLIC ${CMAKE_BINARY_DIR}/Driver/MESH_Driver/)
target_include_directories(PERMAFROST PUBLIC ${CMAKE_BINARY_DIR}/Modules/permafrost_outputs/)

target_include_directories(WATROUTE_OLD PUBLIC ${CMAKE_BINARY_DIR}/Routing_Model/WatRoute_old/)
target_include_directories(RESERVOIR_UPDATE PUBLIC ${CMAKE_BINARY_DIR}/Routing_Model/reservoir_update/)
target_include_directories(RPN_WATROUTE_MESH PUBLIC ${CMAKE_BINARY_DIR}/Routing_Model/RPN_watroute/sa_mesh_process)
target_include_directories(BASEFLOW PUBLIC ${CMAKE_BINARY_DIR}/Routing_Model/baseflow_module/)

# Include directories to make sure headers or modules are visible
target_include_directories(CLASS_LIBRARY PUBLIC ${CMAKE_BINARY_DIR}/LSS_Model/CLASS/3.6/)

target_compile_options(${samesh_exe} PRIVATE -O3)
# NetCDF include directories
include_directories(${NETCDF_C_DIR}/include ${NETCDF_FORTRAN_DIR}/include)
link_directories(${NETCDF_C_DIR}/lib ${NETCDF_FORTRAN_DIR}/lib)

# Include current binary directory for mod files
include_directories(${CMAKE_CURRENT_BINARY_DIR})

# Link current binary directory for libraries
link_directories(${CMAKE_CURRENT_BINARY_DIR})
