# ======================================================================
# Makefile for SA_MESH

# ======================================================================
# Make targets (defined below).
.PHONY: default gfortran ifort mingw_static mpi_gcc mpi_intel debug all clean veryclean
default: all

# ======================================================================
# Options.
#   If the variable is not defined, an assumed value is assigned.
#   Options can be overwritten by user overrides or scripts.

# DIST: Compiler family/distribution.
#   - Blank/undefined to use GNU/GCC compiler (default).
#   - 'intel' to use Intel compiler.
#   - 'mingw' to use GNU/GCC compiler; overrides 'rm' with 'del' for MS-Windows/MS-DOS environment.

# MPI: Parallel/serial compilation.
#   - Blank/undefined to compile in serial (default).
#   - 'ompi' to compile using OMPI compiler.

# LSS: Land surface scheme (LSS).
#   - Blank/undefined to include default versions of CLASS+SVS (default).

# ROUTE: Routing scheme.
#   - Blank/undefined to include default versions of WF_ROUTE,SA_RTE+RTE (default).

# DEBUG: Debugging flags and options.
#   - Blank/undefined to compile with 'o2' optimization (default).
#   - 'yes' to include debug options and disable compiler optimization.

# ======================================================================
# File/object names.
include makefile.def

# ======================================================================
# Pre-configured compiler options.
ifeq ($(filter gfortran,$(MAKECMDGOALS)),gfortran)
DIST=
MPI=
else ifeq ($(filter ifort,$(MAKECMDGOALS)),ifort)
DIST=intel
MPI=
else ifeq ($(filter mingw_static,$(MAKECMDGOALS)),mingw_static)
DIST=mingw
MPI=
else ifeq ($(filter mpi_gcc,$(MAKECMDGOALS)),mpi_gcc)
DIST=
MPI=ompi
else ifeq ($(filter mpi_intel,$(MAKECMDGOALS)),mpi_intel)
DIST=intel
MPI=ompi
endif
ifeq ($(filter debug,$(MAKECMDGOALS)),debug)
DEBUG=yes
endif

gfortran: all
ifort: all
mingw_static: all
mpi_gcc: all
mpi_intel: all
debug: all

# ======================================================================
# Compiler and options.
# 'FTN90PP' and 'FTN90PPOPT' required to compile 'ftn90' files.
ifeq ($(DIST),intel)
FC=ifort
LFLAG=-c -g -traceback -check bounds -fpe0
FTN90PP=-fpp -free
FTN90PPOPT=-Tf
else
FC=gfortran
LFLAG=-c -g -fbacktrace -fbounds-check -ffpe-trap=invalid,zero,overflow -Wconversion -Winteger-division -Wsurprising -Wintrinsic-shadow -Wtarget-lifetime
FTN90PP=-x f95 -cpp -ffree-form -ffree-line-length-none -fcray-pointer
FTN90PPOPT=
endif

# Override debugging options if 'DEBUG' not enabled.
ifndef DEBUG
LFLAG=-c -O2
CLEANUP=@$(MAKE) -s clean DIST=$(DIST)
endif

# Output: sa_mesh.
OUT=sa_mesh

# If MPI is enabled, switch to OMPI compiler and rename output.
# Otherwise add MPI stub to 'OBJECTS'.
ifeq ($(MPI),ompi)
FC=mpifort
OUT=mpi_sa_mesh
else
OBJECTS:=	mpi_stub.o $(OBJECTS)
endif

# Override 'rm' with 'del' and add static option for MinGW.
ifeq ($(DIST),mingw)
BIN_DEL=del
LLINK=-static
FC=gfortran
OUT=sa_mesh_static
else
BIN_DEL=rm
endif

# ======================================================================
# General rules.
%.o: %.f
	$(FC) $(LFLAG) $<
%.o: %.F90
	$(FC) $(FTN90PP) $(LFLAG) $(INC_DIRS) $(DFLAG) $(FTN90PPOPT) $<
%.o: %.f90
	$(FC) $(LFLAG) $<
%.o: %.for
	$(FC) $(LFLAG) $<

# ======================================================================
# Special rules.
EF_Module.o: EF_ParseUtilities.o
%.o: %.ftn90
	$(FC) $(FTN90PP) $(LFLAG) $(INC_DIRS) $(DFLAG) $(FTN90PPOPT)$<

# ======================================================================
# Files renamed for SVS.
runsvs_mod.o: runsvs_mod_sa_mesh.ftn90
	$(FC) $(FTN90PP) $(LFLAG) $(INC_DIRS) $(DFLAG) -o runsvs_mod.o $(FTN90PPOPT)$<

# ======================================================================
# Make target: all
# Deletes object and modules files unless 'DEBUG' has a value.
all: ${OBJECTS}
	$(FC) $(OBJECTS) -o $(OUT) $(LLINK)
	$(CLEANUP)

# ======================================================================
# Make target: clean
# Remove object and module files.
clean:
	-$(BIN_DEL) *.mod *.o

# ======================================================================
# Make target: veryclean
# Remove object and module files, and output file.
veryclean: clean
	-$(BIN_DEL) $(OUT)
