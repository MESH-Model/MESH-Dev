target_sources(SVS_LIBRARY PRIVATE
                sfclayer_mod.F90

                phy_options.F90

                aggcovernat.F90 
                juliand.F90 
                suncos1.F90 
                interpveg.F90
		compz0.F90 
                difuvd2.F90

                rmnlib_basics.hf
        )

target_include_directories(SVS_LIBRARY PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})

