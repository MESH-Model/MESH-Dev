#Include common variable definitions
include ..\..\makefile.def

FMESHEXE = -o mesh.exe
FRTEEXE = -o rte.exe
!IFDEF OPT
FOPT = -O2
!ENDIF
FC = g95
FFLAGS = -cpp -DG95
FCOMP = -c
!IFDEF DEBUG
FDEBUG =
!ENDIF

#Make modules
tmodules: $(MODULE_O)

#Make RTE.exe
RTE.exe: tmodules $(RTE_O)
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(RTE_O) $(MODULE_O) $(FRTEEXE)
	@copy RTE.exe ..\RTE.g95.exe

#Make the class files
tclass: $(CLASS_O)

#Make Mesh.exe
Mesh.exe: tmodules tclass $(MESH_O) $(MESHRTE_O)
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(MODULE_O) $(CLASS_O) $(MESH_O) $(MESHRTE_O) $(FMESHEXE)
	@copy Mesh.exe ..\Mesh.g95.exe

.SUFFIXES: .f .obj

.f.obj:
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(FCOMP) $*.f

#Clean targets
mclean:
	@del $(MODULE_O) $(MODULE_S) $(MODULE_S:.f=.mod) 2> nul

cclean:
	@del $(CLASS_O) $(CLASS_S) 2> nul

rclean:
	@del $(RTE_O) $(RTE_S) rte.exe 2> nul

meclean:
	@del $(MESH_O) $(MESH_S) $(MESHRTE_O) $(MESHRTE_S) mesh.exe 2> nul

#Automatic build rules
.SUFFIXES: .o .f

.f.o:
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(FCOMP) $*.f -o $*.o
