#
#                       Makefile for SA_MESH 
#                       Compile on Linux
#
FC=pgf95
#LFLAG=-fast
LFLAG=-c
OBJECTS= EF_Module.o EF_ParseUtilities.o area_watflood.o mesh_input_module.o\
	FLAGS.o APREP.o CANADD.o CANALB.o CANVAP.o  check_parameters.o\
	CGROW.o CHKWAT.o CLASSA.o  CLASSB.o CLASSBD.o CLASSBHYD.o \
	CLASSD.o CLASSG.o CLASSI.o CLASSS.o CLASST.o CLASSW.o \
	CLASSZ.o CWCALC.o DIASURF.o DIASURFZ.o DRCOEF.o \
	FLXSURFZ.o GATPREP.o \
	GRALB.o GRDRAN.o GRINFL.o ICEBAL.o Julian_Day_ID.o \
	KENUE.o MESH_driver.o \
	read_check_forcing_files.o  read_initial_inputs.o\
	read_parameters_class.o read_parameters_hydrology.o \
	read_r2c.o read_run_options.o read_s_moisture_txt.o \
	read_s_temperature_txt.o read_shed_ef.o read_soil_ini.o \
	read_soil_levels.o resume_state.o R2.o read_forcing_data.o \
	save_state.o SLDIAG.o \
	SNINFL.o SNOADD.o SNOALBA.o SNOALBW.o SNOVAP.o \
	SUBCAN.o TFREEZ.o tile_connector.o TMCALC.o TMELT.o \
	TNPOST.o TNPREP.o TPREP.o TSOLVC.o TSOLVE.o TSPOST.o \
	TSPREP.o TWCALC.o WATDRN.o WATROF.o WEND.o WF_ROUTE.o \
	WFILL.o WFLOW.o WPREP.o write_r2c.o XIT.o
sa_mesh: ${OBJECTS}
	$(FC) -o sa_mesh  $(OBJECTS)
	@echo "sa_mesh is now up to date."
mesh_input_module.mod: mesh_input_module.o mesh_input_module.for
	$(FC) $(LFLAG) mesh_input_module.for
area_watflood.mod: area_watflood.o area_watflood.for
	$(FC) $(LFLAG) area_watflood.for
EF_ParseUtilities.mod: EF_ParseUtilities.o EF_ParseUtilities.for
	$(FC) $(LFLAG) EF_ParseUtilities.for
EF_Module.mod: EF_ParseUtilities.mod EF_Module.o EF_Module.for
	$(FC) $(LFLAG) EF_Module.for
EF_Module.o : EF_ParseUtilities.o EF_Module.for
	$(FC) $(LFLAG) EF_Module.for
FLAG.mod: FLAG.o FLAG.F90
	$(FC) $(LFLAG) FLAG.F90
%.o: %.f
	$(FC) $(LFLAG) $<
%.o: %.F90
	$(FC) $(LFLAG) $<
%.o: %.f90
	$(FC) $(LFLAG) $<
%.o: %.for	
	$(FC) $(LFLAG) $<
#Cleaning everything
clean:
	rm *.mod *.o
#End of the makefile
