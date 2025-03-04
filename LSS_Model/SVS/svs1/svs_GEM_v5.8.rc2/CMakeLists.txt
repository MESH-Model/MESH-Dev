target_sources(SVS_LIBRARY PRIVATE
                sfc_options.F90
                svs_configs.F90

                inicover_svs.F90
                
                svs.F90 
                soili_svs.F90 
                soil_fluxes.F90
                
                aggveghigh.F90 
                aggveglow.F90 
                veglowhigh.F90 
                vegi_svs.F90 
                drag_svs.F90
                
                snow_alone.F90 
                snow_veg.F90 
                ebudget_svs.F90

                runsvsdrn.F90

                hydro_svs.F90 
                phtsyn_svs.F90 
                update_svs.F90
                
                # For now, must manually specify one of following:
                inisoili_svs.F90
                
        )
