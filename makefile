#Include common variable definitions
include makefile.def

#Check for target
!IFNDEF target
ERROR_FLAG = 1
!ENDIF

#Check for optimisation flag
!IFDEF opt
SPECIAL=OPT
!ELSE
SPECIAL=DUMMY
!ENDIF

#Check for existence of Build directory
!IF !EXIST(Build)
ERROR_FLAG = 2
!ENDIF

#Check for debug flag
!IFDEF debug
SPECIAL=DEBUG
!ENDIF

check_errors:
	@if $(ERROR_FLAG)==1 @echo Usage: nmake COMPILERNAME target={mesh:rte} [opt=yes] [debug=yes] 1>&2 | exit 1
	@if $(ERROR_FLAG)==2 @echo Error: Build directory does not exist. 1>&2 | exit 2

g95:
	@nmake $(target) g95= target= /nologo

ifort:
	@nmake $(target) ifort= target= /nologo

mesh: check_errors mesh.$(SUBD).exe

rte: check_errors rte.$(SUBD).exe

#Make the modules
tmodules: $(MODULE_M)
	@for %i in ($(MODULE_M)) do @copy %i .\Build > nul
	@cd Build
	@nmake tmodules $(SUBD)= SPECIAL=$(SPECIAL) /nologo
	@del $(MODULE_S) 2> nul
	@cd..

#Make RTE.exe
RTE.$(SUBD).exe: tmodules $(RTE_M)
	@for %i in ($(RTE_M)) do @copy %i .\Build > nul
	@cd Build
	@nmake RTE.exe $(SUBD)= SPECIAL=$(SPECIAL) /nologo
	@del $(RTE_S) 2> nul
	@cd..

#Make the CLASS files
tclass: $(CLASS_M)
	@for %i in ($(CLASS_M)) do @copy %i .\Build > nul
	@cd Build
	@nmake tclass $(SUBD)= SPECIAL=$(SPECIAL) /nologo
	@del $(CLASS_S) 2> nul
	@cd..

#Make Mesh.exe
Mesh.$(SUBD).exe: tmodules tclass $(MESH_M) $(MESHRTE_M)
	@for %i in ($(MESH_M) $(MESHRTE_M)) do @copy %i .\Build > nul
	@cd Build
	@nmake Mesh.exe $(SUBD)= SPECIAL=$(SPECIAL) /nologo
	@del $(MESH_S) $(MESHRTE_S) 2> nul
	@cd..

mclean:
	@cd Build
	@nmake mclean /nologo
	@del $(MODULE_S) 2> nul
	@cd..

cclean:
	@cd Build
	@nmake cclean /nologo
	@del $(CLASS_S) 2> nul
	@cd..

rclean:
	@cd Build
	@nmake rclean /nologo
	@del $(RTE_S) 2> nul
	@cd..

meclean:
	@cd Build
	@nmake meclean /nologo
	@del $(MESH_S) $(MESHRTE_S) 2> nul
	@cd..

clean: mclean cclean rclean meclean
	@del mesh.*.exe rte.*.exe 2> nul
	@echo Done!
