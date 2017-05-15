#!/bin/sh

set -ae
set -x

#Etienne Gaborit, Spetember 2015
#script made to convert a geophysical file in fst format to a r2c format.
#in theory, nothing has to be updated (the geophys. file is expected to be called 'Gem_geophy.fst'), but
#the user should check if all required variables are managed by the script, and 
#may have to adjust some variables' attributes.
#Also check that the file "header.r2c" in the same folder is up-to-date; this file specifies the domain boundaries.

repentree=./
repsortie1=../fst_converted

if [ ! -d ${repsortie1} ] ; then
    mkdir ${repsortie1}
fi

repsortie2=${repsortie1}/geophy
if [ ! -d ${repsortie2} ] ; then
    mkdir ${repsortie2}
fi

infile=${repentree}/Gem_geophy.fst
outfile=${repsortie2}/Gem_geophy.r2c
if [ -f ${outfile} ] ; then
    rm -f ${outfile}
fi

hournum=0
fstexec='fst2r2c_geo'

#loop over a prescribed set of variables to be extracted from the fst geophysical file and be added to the .r2c geophysical file 
#associate a set of attributes to each variable; some attributes are used to find the record in the fst file, such as ip1 and ip2

for var in ME VF GA VG MF LH Y7 Y8 Y9 Z0 ZP MGIN MG SLA0 SLOP DRND RSUM J1 J2 ; do
  if [ $var == ME ] ; then             
    lev=1200
    var_name='mean_topo_elev'
    unit='m'
    conv1=1.0
    conv2=0.0
    formatte='(999(f6.1,1x))'
${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == VF ] ; then
  # for vegetation, there are 26 classes over which to loop
    type=1
    for ip1 in 1199 1198 1197 1196 1195 1194 1193 1192 1191 1190 1189 1188 1187 1186 1185 1184 \
    1183 1182 1181 1180 1179 1178 1177 1176 1175 1174 ; do
        lev=$ip1
    #    exec=fst2r2c
        var_name='vegetation_of_type_'$type
        unit='fraction'
        conv1=1.0
        conv2=0.0
        formatte='(999(f5.3,1x))'
        let type=type+1
        ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
    done
  elif [ $var == GA ] ; then
    lev=0
    var_name='Glacier_mask'
    unit='fraction'
    conv1=1.0
    conv2=0.0
    formatte='(999(f5.3,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == VG ] ; then
    lev=0
    var_name='dominant_vegetation_type'
    unit='type'
    conv1=1.0
    conv2=0.0
    formatte='(999(f4.1,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == MF ] ; then
    lev=0
    var_name='filtered_topography'
    unit='m'
    conv1=1.0
    conv2=0.0
    formatte='(999(f6.1,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == LH ] ; then
    lev=0
    var_name='launching_height_(gravitational_waves)'
    unit='m'
    conv1=1.0
    conv2=0.0
    formatte='(999(f6.1,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == Y7 ] ; then
    lev=0
    var_name='correlation_of_subgrid_topographic_gradient_along_X_(nil)'
    unit='unknown'
    conv1=1.0 
    conv2=0.0
    formatte='(999(e10.3,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == Y8 ] ; then
    lev=0
    var_name='correlation_of_subgrid_topographic_gradient_along_Y_(nil)'
    unit='unknown'
    conv1=1.0 
    conv2=0.0
    formatte='(999(e10.3,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == Y9 ] ; then
    lev=0
    var_name='correlation_of_subgrid_topographic_gradient_along_X_and_Y_(nil)'
    unit='unknown'
    conv1=1.0 
    conv2=0.0
    formatte='(999(e10.3,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == Z0 ] ; then
    lev=0
    var_name='rugosity_length_(urban)'
    unit='m'
    conv1=1.0 
    conv2=0.0
    formatte='(999(f5.1,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == ZP ] ; then
    lev=0
    var_name='rugosity_length_(Cressman)'
    unit='m'
    conv1=1.0 
    conv2=0.0
    formatte='(999(f5.1,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == MGIN ] ; then
    lev=0
    var_name='water/land_mask'
    unit='fraction'
    conv1=1.0 
    conv2=0.0
    formatte='(999(f5.3,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == MG ] ; then
    lev=0
    var_name='water/land_mask_filtered_(subgrid_scale_lakes_removed)'
    unit='fraction'
    conv1=1.0 
    conv2=0.0
    formatte='(999(f5.3,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == SLA0 ] ; then
    lev=1200
    var_name='maximum_average_subgrid-scale_topographical_slope'
    unit='degrees'
    conv1=1.0 
    conv2=0.0
    formatte='(999(f5.2,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == SLOP ] ; then
    lev=1200
    var_name='slope'
    unit='m/m'
    conv1=1.0 
    conv2=0.0
    formatte='(999(f5.3,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == DRND ] ; then
    lev=1200
    var_name='drainage_density'
    unit='m/m2'
    conv1=1.0 
    conv2=0.0
    formatte='(999(e10.3,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == RSUM ] ; then
    lev=1200
    var_name='total_length_of_rivers_in_a_pixel'
    unit='m'
    conv1=1.0 
    conv2=0.0
    formatte='(999(e10.3,1x))'
    ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
  elif [ $var == J1 ] ; then
    type=1
    for ip1 in 1199 1198 1197 1196 1195 1194 1193 ; do
        lev=$ip1
        var_name='sand_content_for_soil_layer_number_'$type
        unit='percentage'
        conv1=1.0
        conv2=0.0
        formatte='(999(f6.2,1x))'
        ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
        let type=type+1
    done
  elif [ $var == J2 ] ; then
    type=1
    for ip1 in 1199 1198 1197 1196 1195 1194 1193 ; do
        lev=$ip1
        var_name='clay_content_for_soil_layer_number_'$type
        unit='percentage'
        conv1=1.0
        conv2=0.0
        formatte='(999(f6.2,1x))'
        ${fstexec} ${infile} $var $hournum $hournum $outfile $conv1 $conv2 \
$var_name $unit $formatte 'NEAREST' $lev
        let type=type+1
    done
  fi
  
done

#-------------------------------------------
echo "conversion du fichier geophysique effectuée"
#-------------------------------------------
