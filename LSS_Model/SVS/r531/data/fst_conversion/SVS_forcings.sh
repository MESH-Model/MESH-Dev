#!/bin/sh
#

set -ae
set -x

#Etienne Gaborit, september 2015
#script made to convert a set of fst forcing files created by SPS into a set of r2c files.
#at least the first three lines should be updated.
#the user should check if all required meteorological variables are managed by the script, and 
#may have to adjust some variables' attributes.
#Also check that the file "header.r2c" in the same folder is up-to-date; this file specifies the domain boundaries.

repentree=/users/dor/armn/ega/dataloc/10ySPSdata/SPS_10y/output_SPS_1.1_SVS_CaPA/output
first_date="20060601"
last_date="20110927"

#create output folders
repsortie1=./tempo
rm -rf ${repsortie1}
mkdir ${repsortie1}

repsortie2=../fst_converted
if [ ! -d ${repsortie2} ] ; then
    mkdir ${repsortie2}
fi
repsortie3=${repsortie2}/forcings
if [ ! -d ${repsortie3} ] ; then
    mkdir ${repsortie3}
fi

#identify last date of the period of interest
lastan=${last_date:0:4}
lastmo=${last_date:4:2}
lastda=${last_date:6:2}

fstexec='fst2r2c_met'

#create list of dates for the period of interest
yyyymmdd_list=`eval create_yyyymmdd_list.py ${first_date:0:8} ${last_date:0:8}`

#loop over the dates
for yyyymmdd in $yyyymmdd_list ; do

    annee=${yyyymmdd:0:4}
    mois=${yyyymmdd:4:2}
    jour=${yyyymmdd:6:2}
    
    #loop over the hours inside a given day
    hournum=1
    maxhour=25
    while [ ${hournum} -lt $maxhour ] ; do
    
        hourrep=10 
        if [ $hournum -lt $hourrep ] ; then hourstr=0$hournum; else hourstr=$hournum; fi
        infile=${repentree}/output_${annee}${mois}${jour}12/analysis/pm${annee}${mois}${jour}120000-00-00_0000${hourstr}h
        
        if [ -f ${infile} ] ; then
            cp ${infile} ${repsortie1}/current.fst
        else
            echo "missing SPS output file ", "$infile"
            exit
        fi
        
        # loop over the meteorological variables of interest to be converted to r2c format;
        # the user may wish to add new variables, for example those related to Leaf-area index.
        for var in FB FI T8 U8 V8 H8 P8 RT VEGH VEGL ; do
          if [ $var == FB ] ; then           
            lev=0
            filename='FB.r2c'
            var_name='downward_solar_flux'
            unit='W/m2'
            conv1=1.0
            conv2=0.0
            formatte='(999(f8.2,1x))'
          elif [ $var == FI ] ; then
            lev=0
            filename='FI.r2c'
            var_name='infrared_flux_reaching_surface'
            unit='W/m2'
            conv1=1.0
            conv2=0.0
            formatte='(999(f8.2,1x))'
          elif [ $var == T8 ] ; then
            lev=95366840
            filename='TT.r2c'
            var_name='40-m_air_temperature'
            unit='Kelvin'
            conv1=1.0
            conv2=0.0
            formatte='(999(f6.2,1x))'
          elif [ $var == U8 ] ; then
            lev=95366840
            filename='UU.r2c'
            var_name='40-m_X-axis_wind_component'
            unit='m/s'
            conv1=1.0
            conv2=0.0
            formatte='(999(f6.2,1x))'
          elif [ $var == V8 ] ; then
            lev=95366840
            filename='VV.r2c'
            var_name='40-m_Y-axis_wind_component'
            unit='m/s'
            conv1=1.0
            conv2=0.0
            formatte='(999(f6.2,1x))'
          elif [ $var == H8 ] ; then
            lev=95366840
            filename='HU.r2c'
            var_name='40-m_specific_humidity'
            unit='kg/kg'
            conv1=1.0
            conv2=0.0
            formatte='(999(e10.3,1x))'
          elif [ $var == P8 ] ; then
            lev=0
            filename='P0.r2c'
            var_name='surface_pressure'
            unit='PA'
            conv1=1.0 
            conv2=0.0
            formatte='(999(e10.3,1x))'
          elif [ $var == RT ] ; then
            lev=0
            filename='PR.r2c'
            var_name='precipitation_height_over_the_hourly_time-step'
            unit='m'
            conv1=1.0 
            conv2=0.0
            formatte='(999(e10.3,1x))'
          elif [ $var == VEGH ] ; then
            lev=0
            filename='VEGH.r2c'
            hournum2=24
            var_name='pixel_fraction_covered_by_high_vegetation'
            unit='fraction'
            conv1=1.0
            conv2=0.0
            formatte='(999(f5.3,1x))'
          elif [ $var == VEGL ] ; then
            lev=0
            filename='VEGL.r2c'
            hournum2=24
            var_name='pixel_fraction_covered_by_low_vegetation'
            unit='fraction'
            conv1=1.0
            conv2=0.0
            formatte='(999(f5.3,1x))'
          fi
          out_filename=${repsortie1}/${filename}
          
          # for VEGH and VEGL, values are available only for hournum = 24; we simply affect the same values for all hours in a day
          if [ \( $var == VEGH \) -o \( $var == VEGL \) ] ; then
            infile2=${repentree}/output_${annee}${mois}${jour}12/analysis/pm${annee}${mois}${jour}120000-00-00_000024h
            if [ -f ${infile2} ] ; then
                cp ${infile2} ${repsortie1}/current2.fst
            else
                echo "missing SPS output file ", "$infile2"
                exit
            fi
            fst2r2c_met ${repsortie1}/current.fst $var $hournum2 \
            $hournum2 $out_filename $conv1 $conv2 \
            $var_name $unit $formatte 'NEAREST' $lev
            rm -f ${repsortie1}/current2.fst
          else
                fst2r2c_met ${repsortie1}/current.fst $var $hournum \
                $hournum $out_filename $conv1 $conv2 \
                $var_name $unit $formatte 'NEAREST' $lev
          fi
        done
    
    let hournum=hournum+1
    rm -f ${repsortie1}/current.fst
    #end of loop on the hours
    done

    # if we are at the end of a year or at the end of the period of interest, copy the temporary output files into the final folder.
    if [ \( $mois == "12" -a $jour == "31" \) -o \( $lastan == $annee -a $lastmo == $mois -a $lastda == $jour \) ] ; then 
        for var in FB FI T8 U8 V8 H8 P8 RT VEGH VEGL; do
          if [ $var == FB ] ; then                # Set the output variable name and level
            filename='FB.r2c'
          elif [ $var == FI ] ; then
            filename='FI.r2c'
          elif [ $var == T8 ] ; then
            filename='TT.r2c'
          elif [ $var == U8 ] ; then
            filename='UU.r2c'
          elif [ $var == V8 ] ; then
            filename='VV.r2c'
          elif [ $var == H8 ] ; then
            filename='HU.r2c'
          elif [ $var == P8 ] ; then
            filename='P0.r2c'
          elif [ $var == RT ] ; then
            filename='PR.r2c'
          elif [ $var == VEGH ] ; then
            filename='VEGH.r2c'
          elif [ $var == VEGL ] ; then
            filename='VEGL.r2c'
          fi
          out_filename=${repsortie1}/${filename}
          mv ${out_filename} ${repsortie3}/${annee}_$filename
        done
    fi
    
#end of loop on the days
done

#-------------------------------------------
echo "conversion des fichiers de forçage effectuée"
#-------------------------------------------
