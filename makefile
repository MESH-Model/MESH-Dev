# ======================================================================
#						Makefile for  
#           Compile 
#
#                           make 
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
# Build executable and print message
# ======================================================================
all: ${OBJECTS}
	$(FC) -o storage_eval  $(OBJECTS)

#static: ${OBJECTS}
# For MinGW only (the Cygwin library cannot be statically linked to the binary):
#	$(FC) -o storage_eval_static -static-libgcc -static-libgfortran  $(OBJECTS)

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
