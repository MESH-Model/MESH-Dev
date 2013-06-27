# ======================================================================
#           Makefile for SA_MESH
# ======================================================================
#
# ======================================================================
# Include definition file
# ======================================================================
include makefile.def


# ======================================================================
# Declaring variables
# ======================================================================

# The Compiler
#FC=gfortran
FC=gfortran #s.compile #pgf90

# Flag for compiling and debugging - comment as necessary
#LFLAG=-fast
LFLAG=-c -O3

# Flag for debugging
#LFLAG=-g

# ======================================================================
# Build SA_MESH executable and print message
# ======================================================================
SA_MESH_RELEASE: ${OBJECTS}
	$(FC) -o SA_MESH_RELEASE  $(OBJECTS)
	@echo ---*---
	@echo ---*---
	@echo sa_mesh is now up to date...
	@echo ---*---
	@echo ---*---
	@echo Cleaning object files...
	@echo ---*---
	@echo ---*---
	@rm *.mod *.o

# ======================================================================
# General rules
# ======================================================================
%.o: %.f
	$(FC) $(LFLAG) $<
%.o: %.F90
	$(FC) $(LFLAG) $<
%.o: %.f90
	$(FC) $(LFLAG) $<
%.o: %.for
	$(FC) $(LFLAG) $<

# ======================================================================
# Dependencies that can't be captured by the above general rules
# ======================================================================
EF_Module.o : EF_ParseUtilities.o

# ======================================================================
# Cleaning everything including the previously built executable
# ======================================================================
clean:
	rm *.mod *.o *.exe

