#Include common variable definitions
include ..\makefile.def

#Make modules
tmodules:
	@for %i in ($(MODULE_S)) do @copy %i $(SUBD) > nul
	@cd $(SUBD)
	@nmake tmodules $(SUBD)= $(SPECIAL)= /nologo
	@del $(MODULE_S)
	@cd..

#Make RTE.exe
RTE.exe:
	@for %i in ($(RTE_S)) do @copy %i $(SUBD) > nul
	@cd $(SUBD)
	@nmake RTE.exe $(SUBD)= $(SPECIAL)= /nologo
	@del $(RTE_S)
	@cd..
	@copy RTE.$(SUBD).exe ..

#Make the class files
tclass:
	@for %i in ($(CLASS_S)) do @copy %i $(SUBD) > nul
	@cd $(SUBD)
	@nmake tclass $(SUBD)= $(SPECIAL)= /nologo
	@del $(CLASS_S)
	@cd..

#Make Mesh.exe
Mesh.exe:
	@for %i in ($(MESH_S) $(MESHRTE_S)) do @copy %i $(SUBD) > nul
	@cd $(SUBD)
	@nmake Mesh.exe $(SUBD)= $(SPECIAL)= /nologo
	@del $(MESH_S) $(MESHRTE_S)
	@cd..
	@copy Mesh.$(SUBD).exe ..

#Clean targets
mclean:
	@cd ifort
	@nmake ifort= mclean /nologo
	@cd..
	@cd g95
	@nmake g95= mclean /nologo
	@cd..

cclean:
	@cd ifort
	@nmake ifort= cclean /nologo
	@cd..
	@cd g95
	@nmake g95= cclean /nologo
	@cd..

rclean:
	@cd ifort
	@nmake ifort= rclean /nologo
	@cd..
	@cd g95
	@nmake g95= rclean /nologo
	@cd..
	@del rte.*.exe 2> nul

meclean:
	@cd ifort
	@nmake ifort= meclean /nologo
	@cd..
	@cd g95
	@nmake g95= meclean /nologo
	@cd..
	@del mesh.*.exe 2> nul
