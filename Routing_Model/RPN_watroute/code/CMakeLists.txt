add_library(RPN_WATROUTE_CODE
            area_watflood.f
            rerout.f
            route.f
            flowinit.f
        )

# target_sources(${samesh_exe} PRIVATE ${RPN_WATROUTE_CODE})