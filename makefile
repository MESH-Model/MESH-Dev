#                      makefile for FST2R2C
#                       Compile on Linux
#
FC=r.compile
#LFLAG=-fast
LFLAG=-src
RMNLIB=-librmn rmn

OBJECTS=GEM_variable_attributes_module.o grid_parameters_module.o GEM_write_header.o read_grid_parameters.o main.o

fst2r2c: ${OBJECTS}
	$(FC) -o fst2r2c -obj $(OBJECTS) $(RMNLIB)
	@echo "fst2r2c is now up to date."

#Compiling rules
%.o: %.f90
	$(FC) $(LFLAG) $<

#Cleaning everything
clean:
	rm *.mod *.o
#End of the makefile
