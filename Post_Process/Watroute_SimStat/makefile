# ======================================================================
# Include definition file
# ======================================================================
include makefile.def

# ======================================================================
# Declaring variables
# ======================================================================

# The Compiler
FC=gfortran

# Flag for compiling and debugging (-g) - comment as necessary
LFLAG=-c -O2

# Flag for debugging
#LFLAG=-g

# ======================================================================
# Build Watroute_SimStat executable and print message
# ======================================================================
all: ${OBJECTS}
	$(FC) -o Watroute_SimStat $(OBJECTS)

# For MinGW only (the Cygwin library cannot be statically linked to the binary):
#	$(FC) -o Watroute_SimStat_static -static-libgcc -static-libgfortran  $(OBJECTS)

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
EF_Module.mod : EF_ParseUtilities.mod

ef_parseutilities.mod : EF_ParseUtilities.mod

ef_module.mod : EF_Module.mod

# ======================================================================
# Cleaning object files
# ======================================================================
clean:
# 'rm' for Cygwin, 'del' for MinGW - comment as necessary
	rm *.mod *.o
#	del *.mod *.o

# ======================================================================
# Cleaning everything including the previously built executable
# ======================================================================
veryclean:
# 'rm' for Cygwin, 'del' for MinGW - comment as necessary
	rm *.mod *.o *.exe
#	del *.mod *.o *.exe
