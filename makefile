# ======================================================================
#						Makefile for SA_MESH 
#           Compile on Windows using MinGW's gfortran compiler
#
#                           mingw32-make
# ======================================================================

# ======================================================================
# Include definition file
# ======================================================================
include makefile.def

# ======================================================================
# Declaring variables
# ======================================================================

# The Compiler
FC=gfortran

# Flag for compiling and debugging - comment as necessary
LFLAG=-c -O2

# Flag for debugging
#LFLAG=-g

# ======================================================================
# Build SA_MESH executable and print message
# ======================================================================
all: ${OBJECTS}
#	$(FC) -o sa_mesh  $(OBJECTS)
# JUL 11/11 - DGP: -static to remove gcc, gfortran dependency;
	$(FC) -static -o sa_mesh  $(OBJECTS)
	@echo ---*---
	@echo ---*---
	@echo sa_mesh is now up to date...
	@echo ---*---
	@echo ---*---

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
# Cleaning object files
# ======================================================================
clean:
	del *.mod *.o

# ======================================================================
# Cleaning everything including the previously built executable
# ======================================================================
veryclean:
	del *.mod *.o *.exe
