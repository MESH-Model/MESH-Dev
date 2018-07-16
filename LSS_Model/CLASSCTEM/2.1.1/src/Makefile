#module load netcdf/intel/4.2

#The above is because of a bug so I would need to remove all of the original .o files until the problem is fixed

fcompiler = ifort
f90compiler = ifort

fflags = -traceback -fpe0 -r8 -g -O2 -nbs -cpp
MY_FLAGS = -extend-source -nofixed
#NCflag = -lnetcdff -lnetcdf 

LD = ifort

LDFLAGS = -g -assume byterecl -L/opt/sharcnet/netcdf/4.2/intel/lib -lnetcdff -lnetcdf

######## The list of object files for each target ########

NCLASSCTEM_o = ctem_params.o ctem_statevars.o APREP.o CLASSBD.o GRALB.o mvidx.o SNOW_ALBVAL.o \
SNOW_TRANVAL.o SNOALBA.o  TNPREP.o WFILL.o CANADD.o CLASSD.o GRDRAN.o SNOALBW.o TPREP.o WFLOW.o \
CANALB.o CLASSG.o GRINFL.o SNOVAP.o TSOLVC.o WPREP.o wetland_methane.o ctemg1.o bio2str.o ctems1.o \
ctemg2.o PHTSYN3.o CANVAP.o CLASSI.o CWCALC.o ICEBAL.o SUBCAN.o TSOLVE.o XIT.o CGROW.o CLASSS.o \
DIASURFZ.o SCREENRH.o TFREEZ.o TSPOST.o CHKWAT.o CLASST.o DRCOEF.o SLDIAG.o TMCALC.o TSPREP.o \
CLASSA.o CLASSW.o FLXSURFZ.o SNINFL.o TMELT.o TWCALC.o CLASSB.o CLASSZ.o GATPREP.o SNOADD.o \
TNPOST.o WEND.o WATROF.o WATDRN.o balcar.o GAUSSG.o ORDLEG.o TRIGL.o mainres.o hetresv.o hetresg.o \
allocate.o phenolgy.o turnover.o mortality.o disturb.o ctems2.o competition_map.o competition_unmap.o \
read_from_job_options.o soil_ch4uptake.o landuse_change_mod.o competition_mod.o ctem.o io_driver.o \
runclass36ctem.o


######## The targets ########

NCLASSCTEM: $(NCLASSCTEM_o)
	$(f90compiler) $(fflags) $(NCLASSCTEM_o) -o $@

######## Special rules to create object files ########

######## Generic rules to create object files ########

.SUFFIXES: .o .F90 .f90 .f77 .f .f95

.F90.o:
	$(f90compiler) $(fflags) -c $< -o $@
.f90.o:
	$(f90compiler) $(fflags) $(MY_FLAGS) -c $< -o $@
.f.o:
	$(fcompiler) $(fflags) -c $< -o $@
.f95.o:
	$(f95Compiler) $(fflags) -c $< -o $@
.mod.o:
	$(f95Compiler) $(fflags) -c $< -o $@

######## Special targets ########

clean:
	rm -rf *.o *.mod *~ core

clobber: clean
	rm -rf NCLASSCTEM

