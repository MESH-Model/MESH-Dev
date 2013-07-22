#!/bin/sh
#
#   Author: Daniel Deacu (RPN) - August 2010 
#
###############################################################################

set -aex


# DATES

first_date=2004060100

#last_date=2004060100
#last_date=2004083100
last_date=2004060100
#last_date=2009053100


# FLAGS

rain_snow=0

run_watroute_only=0

# Set at most one of the streamflow flags below to 1

watroute_streamflow_insertion=0

watroute_streamflow_comparison=1

if [ ${watroute_streamflow_insertion}  -eq 1 -a \
     ${watroute_streamflow_comparison} -eq 1    ] ; then
   echo "ERROR: watroute_streamflow_insertion AND watroute_streamflow_comparison' CANNOT BE SET TO 1 AT THE SAME TIME!"
   exit 1
fi

# For start dates other than 1 June 2004, for which the lake levels are 
# hard-coded in WATROUTE, the lake levels have to be available in the 
# *_lvl.txt files. These files can be obained from a previous WATROUTE run 
# for the day preceding the start date.  

copy_initialconds_for_watroute=0


# PATHS
#
#
#   1. MODEL OUTPUT

#exper_path=/users/dor/armn/gri/arxt46/tests/cls_mixedfor
#exper_path=/users/dor/armn/gri/arxt46/tests/cls_wr_test
exper_path=/users/dor/armn/dav/hal/mec_mesh/MEC_repository/trunk/v333/exp/iugls/rundirs/long_run/output_long

#   2. BASE DIRECTORY OF THE SVN WORKING COPY

#basedir=/users/dor/armn/gri/arxt46/svn/v333
basedir=/users/dor/armn/dav/hal/mec_mesh/MEC_repository/trunk/v333


#   3. INITIAL CONDITIONS FOR WATROUTE FROM PREVIOUS RUN 

if [ ${copy_initialconds_for_watroute} -eq 1 ]; then
    iniconds_dir=/users/dor/armn/gri/arxt46/tests/cls_alb_new/BUDGET
fi


#****************** DO NOT CHANGE ANY OF THE PATHS BELOW ! ********************
#
#
#   4. ADD DIRECTORIES TO THE PATH ENVIRONMENT VARIABLE 

PATH=${basedir}/src/utils/fortran:${basedir}/src/utils/python:${basedir}/src/utils/scripts:$PATH
echo $PATH


#   5. MASK FILE
#
#      * 'maskfile' holds the absolute path of a symbolic link 
#      * change the link as required depending on the mask you want to use 
#        (see README file in same directory where the symbolic link is located)

maskfile=${basedir}/exp/iugls/masks/glake_masks.fst


#   6. WATROUTE 

watroute_config_dir=${basedir}/exp/iugls/watroute_config

if [ ${watroute_streamflow_insertion}  -eq 1 -o \
     ${watroute_streamflow_comparison} -eq 1    ] ; then
    watroute_event_path=${watroute_config_dir}/param_event_streamflows
    watroute_strfw_path=${watroute_config_dir}/strfw
else
    watroute_event_path=${watroute_config_dir}/param_event
fi

watroute_resrl_path=${watroute_config_dir}/resrl

watroute_run_dir=${exper_path}/BUDGET/watroute


###############################################################################


if [ ! -d ${exper_path}/BUDGET ] ; then
    mkdir -m 755 -p ${exper_path}/BUDGET
fi

if [ ${run_watroute_only} -eq 0 ] ; then

    extrait_AHFL
    extrait_PR
    extrait_TJ
    extrait_O1
    extrait_LATF
    extrait_TRAF

    if [ ${rain_snow} -eq 1 ] ; then

#_______Output names (ON)

	rainrate_on=U1
	snowrate_on=U3

	extract_rain_snow_rates
    fi

    yyyymm_list=`eval create_yyyymm_list.py ${first_date:0:8} ${last_date:0:8}`

    regroupe_enmois

    traitement_lacs_dd

    champs_pourwatroute

fi


#___WATROUTE

if [ ! -d ${exper_path}/BUDGET/watroute ] ; then
    mkdir -m 755 -p ${exper_path}/BUDGET/watroute
fi


rm -rf ${watroute_run_dir}/*

rsync -r --exclude=.svn ${watroute_config_dir}/exp/  ${watroute_run_dir}


if [ ${copy_initialconds_for_watroute} -eq 1 ]; then
    cp_iniconds_for_watroute
fi

if [ ${watroute_streamflow_comparison} -eq 1 ] ; then
    rm -f ${watroute_run_dir}/spl_rpn.csv
fi

watroute_enboucle

###############################################################################
