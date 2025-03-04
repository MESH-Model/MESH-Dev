add_library(LIBRMN)

# Add source files for the SVS library
add_subdirectory(base)
add_subdirectory(primitives)

# Include directories to make sure headers or modules are visible
target_include_directories(LIBRMN PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})