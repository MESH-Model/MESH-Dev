#Include common variable definitions
include ..\..\makefile.def

FMESHEXE = /exe:mesh.exe
FRTEEXE = /exe:rte.exe
SIEXE = /exe:SI.exe
!IFDEF OPT
FOPT = /O2
!ENDIF
FC = ifort
FFLAGS = /F30000000 /check:uninit /nologo /fpp /DIFORT
FCOMP = /c
!IFDEF DEBUG
FDEBUG = /traceback
!ENDIF

#Make modules
tmodules: $(MODULE_O)

#Make RTE.exe
RTE.exe: tmodules $(RTE_O)
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(RTE_O) $(MODULE_O) $(FRTEEXE)
	@copy RTE.exe ..\RTE.ifort.exe > nul

#Make the class files
tclass: $(CLASS_O)

#Make Mesh.exe
Mesh.exe: tmodules tclass $(MESH_O) $(MESHRTE_O)
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(MODULE_O) $(CLASS_O) $(MESH_O) $(MESHRTE_O) $(FMESHEXE)
	@copy Mesh.exe ..\Mesh.ifort.exe > nul

SI.exe: $(SI_O)
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(SI_O) $(SIEXE)
	@copy SI.exe ..\SI.ifort.exe > nul

.SUFFIXES: .f .obj .f90

.f.obj:
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(FCOMP) $*.f

.f90.obj:
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(FCOMP) $*.f90

#Clean targets
mclean:
	@del $(MODULE_O) $(MODULE_S) $(MODULE_S:.f=.mod) 2> nul

cclean:
	@del $(CLASS_O) $(CLASS_S) 2> nul

rclean:
	@del $(RTE_O) $(RTE_S) rte.exe 2> nul

meclean:
	@del $(MESH_O) $(MESH_S) $(MESHRTE_O) $(MESHRTE_S) mesh.exe 2> nul

sclean:
	@del $(SI_O) $(SI_S) SI.exe 2> nul

#Automatic build rules
.SUFFIXES: .obj .f

.f.obj:
	$(FC) $(FFLAGS) $(FOPT) $(FDEBUG) $(FCOMP) $*.f /object:$*.obj
