#
#                       Makefile for connector_dds_mesh 
#                       Compile on Linux
#
FC=pgf95
OBJECTS= connector_dds_mesh.o
connector_dds_mesh: ${OBJECTS}
	$(FC) -o connector_dds_mesh  $(OBJECTS)
	@echo "connector_dds_mesh is now up to date."
%.o: %.for	
	$(FC) -c $<
#Cleaning everything
clean:
	rm $(OBJECTS)
#End of the makefile
