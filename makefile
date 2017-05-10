# ======================================================================
#						Makefile for  
#           Compile on Windows 
#
#                           make (cygwin)
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
#LFLAG=-c -g
#LFLAG=-c -O2 -pg

# ======================================================================
# Build SA_MESH executable and print message
# ======================================================================
all: ${OBJECTS}
	$(FC) -o sa_mesh  $(OBJECTS)

# ======================================================================
# General rules
# ======================================================================
%.o: %.f
	$(FC) $(LFLAG) $<
%.o: %.F90
	$(FC) $(LFLAG) $<
%.o: %.f90
	$(FC) $(LFLAG) $<
%.o: %.f03
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
