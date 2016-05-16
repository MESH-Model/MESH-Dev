# ======================================================================
#
#						Makefile for SA_MESH 
#
# ======================================================================

# ======================================================================
# Include definition file
# ======================================================================
include makefile.def

# ======================================================================
# Declaring variables
# ======================================================================

# The Compiler
# Ensure to disable the MPI stub if using an MPI compiler.
FC=gfortran
#FC=mpifort

# Flags for compiling, profiling, and debugging - comment as necessary
# Flag for compiling
LFLAG=-c -O2
#LFLAG=-c -O3 -ffast-math

# Flag for debugging
#LFLAG=-c -g

# ======================================================================
# Build SA_MESH executable and print message
# ======================================================================
all: ${OBJECTS}
	$(FC) -o sa_mesh  $(OBJECTS)
#	$(FC) -o mpi_sa_mesh  $(OBJECTS)

#static: ${OBJECTS}
# For MinGW only (the Cygwin library cannot be statically linked to the binary):
#	$(FC) -o sa_mesh_static -static-libgcc -static-libgfortran  $(OBJECTS)

# ======================================================================
# Rules for MPI
# ======================================================================
# Enable the next two lines if using a regular compiler. Comment the 
# next two lines if using the MPI compiler.
module_mpi.o : module_mpi_stub.f90
	$(FC) $(LFLAG) $< -o module_mpi.o

# ======================================================================
# Rules for SVS
# ======================================================================

# Flag for defines
#DFLAG=-DRUNSVS -DNGRIDCELLS=$(NG)
DFLAG=-DRUNSVS

# Rules
%.o: %.ftn90
	gcc -x f95 -cpp $(LFLAG) -ffree-form -ffree-line-length-none -fcray-pointer -I$(CHANGES2PHY) -I$(PHY) -I$(SVS) $(DFLAG) $<

runsvs_mod.o: runsvs_mod_sa_mesh.ftn90
	gcc -x f95 -cpp $(LFLAG) -ffree-form -ffree-line-length-none -fcray-pointer -I$(CHANGES2PHY) -I$(PHY) -I$(SVS) $(DFLAG) -o runsvs_mod.o $<

process_SVS_variables.o: process_SVS_variables.f90
	$(FC) -cpp $(LFLAG) -I$(CHANGES2PHY) -I$(PHY) -I$(SVS) $<

process_SVS_config.o: process_SVS_config.f90
	$(FC) -cpp $(LFLAG) -I$(CHANGES2PHY) -I$(PHY) -I$(SVS) $<

process_SVS_module.o: process_SVS_module.f90
	$(FC) -cpp $(LFLAG) -I$(CHANGES2PHY) -I$(PHY) -I$(SVS) $<

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
# 'rm' for Cygwin, 'del' for MinGW - comment as necessary
	rm *.mod *.o
#	del *.mod *.o

# ======================================================================
# Cleaning everything including the previously built executable
# ======================================================================
veryclean:
# 'rm' for Cygwin, 'del' for MinGW - comment as necessary
	rm *.mod *.o sa_mesh
#	rm *.mod *.o mpi_sa_mesh
#	del *.mod *.o sa_mesh.exe
