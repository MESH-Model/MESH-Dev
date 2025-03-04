# SVS/CMakeLists.txt

# Define the SVS library
add_library(SVS_LIBRARY)

# Add source files for the SVS library
add_subdirectory(src)
add_subdirectory(svs_GEM_v5.8.rc2)
add_subdirectory(rpnphy_5.8.3_all)
add_subdirectory(changes_to_rpnphy)

# Include directories to make sure headers or modules are visible
target_include_directories(SVS_LIBRARY PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
