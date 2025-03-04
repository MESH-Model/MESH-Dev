add_library(BLOWING_SNOW
            # PBSMrates.o 
            PBSMrun.f 
            ProbabilityThreshold.f
            REDISTRIB_SNOW.f 
            SUM.f
            PBSM_module.f90
        )

target_link_libraries(BLOWING_SNOW PUBLIC 
STRING_MODULE)
# target_sources(${samesh_exe} PRIVATE ${BLOWING_SNOW})