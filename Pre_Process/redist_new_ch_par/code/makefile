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
#LFLAG = -O3 -g -ffixed-line-length-none -fd-lines-as-comments -fno-range-check
LFLAG = -O3 -static -ffixed-line-length-none -fd-lines-as-comments -fno-range-check

.SECONDARY:

# ======================================================================
# Build redist_new_ch_par executable and print message
# ======================================================================
all: ${MODULES} ${OBJECTS}
	$(FC) -o redist_new_ch_par $(OBJECTS)
	@echo ---*---
	@echo ---*---
	@echo redist_new_ch_par is now up to date...	@echo ---*---
	@echo ---*---

# ======================================================================
# General rules
# ======================================================================

.PHONY: EF_ParseUtilities.o area_watflood.o EF_Module.o
%.mod : %.f 
	$(FC) $(LFLAG) -c $< 

%.o : %.f $(MODULES) 
	$(FC) $(LFLAG) -c $<
%.o : %.F90 
	$(FC) $(LFLAG) -c $<
%.o : %.f90 
	$(FC) $(LFLAG) -c $<

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
	rm *.mod *.o watroute
#	del *.mod *.o *.exe
