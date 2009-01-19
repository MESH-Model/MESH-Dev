IC = ifort
IFLAGS = /traceback /F10000000 /check:uninit /nologo
ICOMPILE = /c
OPTIMISATION = /O2
GC = g95
GFLAGS = 
GCOMPILE = -c
SOURCE = APREP.f CANADD.f CANALB.f CANVAP.f CGROW.f CHKWAT.f CLASSA.f CLASSB.f CLASSBD.f CLASSD.f CLASSG.f CLASSI.f CLASSS.f CLASST.f CLASSW.f CLASSZ.f CWCALC.f DIASURF.f DRCOEF.f FLXSURFZ.f GATPREP.f GRALB.f GRDRAN.f GRINFL.f ICEBAL.f MESH_driver.f SLDIAG.f SNINFL.f SNOADD.f SNOALBA.f SNOALBW.f SNOVAP.f SUBCAN.f TFREEZ.f TMCALC.f TMELT.f TNPOST.f TNPREP.f TPREP.f TSOLVC.f TSOLVE.f TSPOST.f TSPREP.f TWCALC.f WATROF.f WAT_DRAIN.f WEND.f WFILL.f WFLOW.f wf_route.f WPREP.f XIT.f wat_ensim.f CLASSBHYD.f
OUTPUTG = $(SOURCE:.f=.o)
OUTPUTI = $(SOURCE:.f=.obj)

all: g95 ifort
	@echo To select a specific compiler, use either target "g95" or "ifort".

g95: mesh.g95.exe

ifort: mesh.ifort.exe

release: mesh.exe

mesh.ifort.exe: $(OUTPUTI)
	$(IC) $(IFLAGS) $(OUTPUTI) /exe:mesh.ifort.exe

mesh.g95.exe: $(OUTPUTG)
	$(GC) $(GFLAGS) $(OUTPUTG) -o mesh.g95.exe

mesh.exe:
	$(IC) $(IFLAGS) $(SOURCE) $(OPTIMISATION) /exe:mesh.exe

.SUFFIXES: .f .o .obj

.f.o:
	$(GC) $(GFLAGS) $(GCOMPILE) $*.f

.f.obj:
	$(IC) $(IFLAGS) $(ICOMPILE) $*.f

iclean:
	@del *.obj mesh.ifort.exe

gclean:
	@del *.o mesh.g95.exe

clean: iclean gclean
	@echo Done!
