#                      makefile for FST2R2C
#                       Compile on Linux
#
FC=r.compile
#LFLAG=-fast
LFLAG=-src
RMNLIB=-librmn rmn

OBJECTS=fst2r2c.o
SRCFILES=fst2r2c.f90
fst2r2c: ${OBJECTS}
	$(FC) -src $(SRCFILES) -o fst2r2c $(RMNLIB)
	@echo "fst2r2c is now up to date."

#Compiling rules
%.o: %.f90
	$(FC) $(LFLAG) $<

#Cleaning everything
clean:
	rm *.mod *.o
#End of the makefile
