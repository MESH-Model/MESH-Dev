# MESH-HDS
A repo for the HDS model implementation in MESH (version 1860).

HDS standalone repo: https://github.com/MIsmlAhmed/HDS

MESH repo: https://github.com/MESH-Model/MESH-Dev

# How to run HDS in MESH
1. In the `MESH_input_run_options.ini` file, add a new control flag `HDSFLAG` and set its value to `on`. The following is a snippet of the run options file with HDS activated:

        MESH input run options file
        ##### Control Flags #####
        ----#
        19 # Number of control flags
        HOURLYFLAG            60
        IWF                    1 # Selection of Runoff Generation Algorithms
        .
        .
        other control flags
        .
        .
        .
        RUNMODE           runrte
        HDSFLAG              on # HDS flag

2. create an `HDS_parameters.csv` file (in the same MESH inputs directory) that hold HDS parameters value for each grid/subbasin. Note that each line represents HDS parameters for a specific subbasin/grid. The rows should be in the same order as the subbasins (as indicated by `MESH_grid_no` column). The following is an example of the parameters file:

        MESH_grid_no, depressionDepth_m, depressionAreaFrac, deprCatchAreaFrac,    p,    b, oudinPETScaleK1,oudinPETTempThrK2
        1,                         0.33,               0.28,              0.97, 1.72,  1.5,            30.0,              3.0

The following is a description of the parameters:
| parameter name      | Description | Units |
| ------------------- | ----------- | ----- |
| MESH_grid_no | MESH grid or subbasin id (should be listed in the same order as in the shd file) | - |
| depressionDepth | average depression depth (depression volume/depression area) | m |
| depressionAreaFrac | depression area fraction (depression area/basin area) | - |
| deprCatchAreaFrac | catchment area (fraction of the land area = basin area-depression area) that drains to the depressions | -|
| p | shape of the slope profile  | - |
| b | shape of the fractional contributing area curve  | - |
| oudinPETScaleK1 | Oudin PET formula scaling factor | deg C|
| oudinPETTempThrK2 | Oudin PET formula temperature threshold | deg C|

Note that parameters `p`, `b`, `oudinPETScaleK1`, and `oudinPETTempThrK2` have a typical value of `1.72`, `1.5`, `30.0`, and `3.0`, respectively. The user can use these recommended values or try to calibrate them. The rest of the parameters can be obtained from terrain analysis or by calibration.

3. HDS outputs (states) are stored in the `results` directory of MESH in `HDS_balance.csv`. The altered streamflow values are written in the `MESH_output_streamflow.csv` file.
________
# Changes to the MESH code

Changes were made in the following files to implement HDS into MESH
All changes related to HDS are commented with ! HDS

* makefile.def -> add HDS to the make process
* read_run_options.f90 -> enable HDSflag in the run_options file
* sa_mesh_run_within_grid.f90 -> call HDS after calculating all fluxes and before the routing step.

________

# Change log:

* Code clean up to be transferred to ECCC
* Change HDS inputs to fractions and depth instead of actual areas and volume for easier calibration. Fix bugs when calculating land area and the amount of runoff that bypasses the depressions.