# MESH-HDS
A repo for the HDS model implementation in MESH (version 1860).

HDS standalone repo: https://github.com/MIsmlAhmed/HDS

MESH repo: https://github.com/MESH-Model/MESH-Dev

# Changes to the MESH code

Changes were made in the following files to implement HDS into MESH
All changes related to HDS are commented with ! HDS

* makefile.def -> add HDS to the make process
* read_run_options.f90 -> enable HDSflag in the run_options file
* sa_mesh_run_within_grid.f90 -> call HDS after calculating all fluxes and before the routing step.

________

# Change log:
