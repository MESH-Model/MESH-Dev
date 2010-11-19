#
#                       Makefile for connector_dds_mesh 
#                       Compile on Linux
#
FC=pgf90
OBJECTS= main_dds.o mod_dds.o dds.o DDS_inp.o DDS_allocate.o \
	neigh_value.o more_output.o external_func.o 
dds: ${OBJECTS}
	$(FC) -o dds  $(OBJECTS)
	@echo "dds is now up to date."
mod_dds.mod: mod_dds.o mod_DDS.f
	$(FC) -c mod_dds.f
main_dds.o: mod_dds.o main_dds.f
	$(FC) -c main_dds.f
dds.o: mod_dds.o dds.f
	$(FC) -c dds.f
dds_allocate.o: mod_dds.o dds.f
	$(FC) -c dds_allocate.f
dds_inp.o: mod_dds.o dds_inp.f
	$(FC) -c dds_inp.f
external_func.o: mod_dds.o external_func.f
	$(FC) -c external_func.f
%.o: %.f	
	$(FC) -c $<
#Cleaning everything
clean:
	rm $(OBJECTS)
	rm *.mod
#End of the makefile
