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
LFLAG=-c -O3

# Flag for debugging
#LFLAG=-g -O0 

# ======================================================================
# Build SA_MESH executable and print message
# ======================================================================
all: ${OBJECTS}
	$(FC) -o SA_MESH_CMEM  $(OBJECTS)
# JUL 11/11 - DGP: -static to remove gcc, gfortran dependency;
#	$(FC) -static -o SA_MESH_CMEM  $(OBJECTS)
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
