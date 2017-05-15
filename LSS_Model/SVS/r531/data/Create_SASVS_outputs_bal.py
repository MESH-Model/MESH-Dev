#!/usr/bin/python

# this script is made to create a text file containing the post-processed outputs from stand-alone svs (sasvs)
# some variables are processed inside this script and are not available as they stand after a sasvs simulation,
# for example the total liquid soil water content is recorded in the final text file, instead of the water content for each soil layer.
 
# this script assumes that sasvs original output file only contains outputs at a 24-h interval 

# arguments required: name of sasvs output file (outname: .out), name of catchment interpolation file (intrpfile: .intrp),
# number of points in the X-direction (NX), and number of points in the Y direction (NY) for the catchment under study 
 
import numpy as np, os

def build_fst(outname, intrpfile,NX,NY): 

    #enter here the depth (in m) of each soil layer (7 layers are assumed, see sasvs code)
    laydepths = {'L1':0.05, 'L2':0.05, 'L3':0.07, 'L4':0.11, 'L5':0.22, 'L6':0.3, 'L7':0.6}
    nlayers = len(laydepths.keys())
    #enter the level of each soil layer:
    laylevs = {'L1':'59868832', 'L2':'59968832', 'L3':'60068832', 'L4':'60168832', 'L5':'60268832', 'L6':'60368832', 'L7':'60468832'}
    
    # enter the thickness (m) of the soil layer which can contain ice, and its level
    icelaydepth = 0.18
    icelev = '0'
    
    # enter the list of the variables to be post-processed (and compute watershed averages):
    poprovar = ('SWEL','SWEH','SWE','SWAT','SICE')
    
    #enter the list of the variables (and their levels) not to be post-processed but for which we want to compute watershed averages:
    desout = {'DDIS':'0', 'AHFL':'0', 'TJ':'0',  'TRAF':'60268832', 'ALAT':'0', 'O1':'0', 'PR':'0', 'LZS':'0'}
    
    #enter the list of the order in which you want to have the final outputs to be stored
    # or switch status of lines 166 and 167 to output everything that's in the final dictionnary, in random order.
    finorder = ('SWEL','SWEH','SWE', 'AHFL_0', 'TJ_0','SWAT','SICE', 'TRAF_60268832', 'ALAT_0', 'O1_0', 'DDIS_0', 'PR_0')

    # check existence of output file
    if os.path.isfile(outname)==False:
        print 'the file ',  outname, 'was not found, aborting'
        return
        
    # read output file
    outfile=open(outname)
    cont = outfile.readlines()
    outfile.close()
    
    # initialze empty fulldico to store all variables' values over the whole simulation
    fulldico = dict()
    
    # fill the fulldico storage dictionnary
    for row in cont:
        
        line = row.split()
        date=line[0]
        var = line[1]
        level = line[2]
        fullvar = var+'_'+level
        
        if not fullvar in fulldico:
            fulldico[fullvar] = dict()
            
        if not date in fulldico[fullvar]:
            fulldico[fullvar][date] = np.zeros(NX*NY)
        
        for i in range(0, NX*NY):
            val = float(line[3+i])
            fulldico[fullvar][date][i] = val
    
    # delete useless variables
    del line, date, var,  level,  fullvar
    
    # post-process output variables to compute SWE(mm), SWAT (mm), etc.
    
    # SWE = Snow Water Equivalent or total water associated to snowpack (mm)
    # SWEL is for low veg., SWEH for high veg. and SWE is the pixel weighted average
    # SWAT = Soil liquid water content (mm)
    # SICE = Soil ice water content (mm)
    
    #copy created fulldico into new dictionnary fulldicopro
    fulldicopro = dict()
    for elem in desout.keys():
        fullel = elem + '_' + desout[elem]
        fulldicopro[fullel] = fulldico[fullel]
    
    # add to fulldicopro the room for incoming post-processed values 
    for provar in poprovar:
        fulldicopro[provar] = dict()
        for date in fulldico[fullel].keys():
            fulldicopro[provar][date] = np.zeros(NX*NY)
    
    del  elem,  provar,  date
    
    #post-process the values
    for provar in poprovar:
        if provar == 'SWEL':
            try:
                for date in fulldico['I5_0'].keys():
                    for i in range(0, NX*NY):
                        swelow = fulldico['I5_0'][date][i] + fulldico['I4_0'][date][i]
                        fulldicopro[provar][date][i] = swelow
            except:
                print 'the creation of ',provar,  ' failed; the variable is filled with zeros'  
                
        elif provar == 'SWEH':
            try:
                for date in fulldico['SVM_0'].keys():
                    for i in range(0, NX*NY):
                        swehigh = fulldico['SVM_0'][date][i] + fulldico['WSNV_0'][date][i]
                        fulldicopro[provar][date][i] = swehigh
            except:
                print 'the creation of ',provar,  ' failed; the variable is filled with zeros'  
        elif provar == 'SWE':
            try:
                for date in fulldico['I5_0'].keys():
                    for i in range(0, NX*NY):
                        swetot = (fulldico['SVM_0'][date][i] + fulldico['WSNV_0'][date][i]) * fulldico['VEGH_0'][date][i] + \
                            (fulldico['I5_0'][date][i] + fulldico['I4_0'][date][i]) * (1. - fulldico['VEGH_0'][date][i]) 
                        fulldicopro[provar][date][i] = swetot
            except:
                print 'the creation of ',provar,  ' failed; the variable is filled with zeros' 
        elif provar == 'SWAT':
            try:
                for date in fulldico['I1D_'+laylevs['L1']].keys():
                    swatliqu = np.zeros(NX*NY)
                    for layer in range(0, nlayers):
                        for i in range(0, NX*NY):
                            swatliqu[i] += fulldico[ 'I1D_'+laylevs['L'+str(layer+1)] ][date][i]  * laydepths['L'+str(layer+1)] * 1000 
                            
                    for i in range(0, NX*NY):
                        fulldicopro[provar][date][i] = swatliqu[i]
            except:
                print 'the creation of ',provar,  ' failed; the variable is filled with zeros' 
        elif provar == 'SICE':
            try:
                for date in fulldico['I2_'+icelev].keys():
                    for i in range(0, NX*NY):
                        sicewat = fulldico[ 'I2_' + icelev ] [date] [i]  * icelaydepth * 1000 
                        fulldicopro[provar][date][i] = sicewat
            except:
                print 'the creation of ',provar,  ' failed; the variable is filled with zeros'   
        
    # compute watershed averages of all desired variables
    
    
    #begin by reading the pixel weights contained in the interpolation file (2nd argument)
    interpfile = open(intrpfile); del intrpfile
    firstline = interpfile.readline() 
    nbpoints = int(firstline.split()[0])
    secline = interpfile.readline()
    shedarea = float(secline.split()[0])
    
    piwght = np.zeros(NX*NY)
    for numpoint in range(0, nbpoints):
        line = interpfile.readline()
        xpos = float(line.split()[0])
        line = interpfile.readline()
        ypos = float(line.split()[0])
        line = interpfile.readline()
        piwght[NX*(ypos-1)+xpos] = float(line.split()[1])
        
    sumwghts = np.sum(piwght)
        
    #
    # define lists of output elements and dates
#    outelemlist = sorted(fulldicopro.keys())
    outelemlist = finorder
    outdatelist = sorted(fulldicopro[poprovar[0]].keys())
    
    dicout = dict()
    for element in outelemlist:
        dicout[element] = dict()
        for date in outdatelist:
            totshedsum = 0.0
            for i in range(0, NX*NY):
                totshedsum += fulldicopro[element] [date] [i] * piwght[i]
            totshedavg = totshedsum / sumwghts
            dicout[element] [date] = totshedavg
    
    #try to compute the water balance using a defined set of required (post-processed) variables
    try:
        initwat = float(dicout['SWAT'] [outdatelist[0]]) + float(dicout['SICE'] [outdatelist[0]]) + float(dicout['SWE'] [outdatelist[0]]) 
        finalwat = float(dicout['SWAT'] [outdatelist[-1]]) + float(dicout['SICE'] [outdatelist[-1]]) + float(dicout['SWE'] [outdatelist[-1]]) 
        outdatelist2 = outdatelist[1:-1]
        
        dicum = dict()
        for element in ('PR_0', 'AHFL_0', 'TRAF_60268832', 'ALAT_0', 'O1_0'):
            cumul = 0
            for date in outdatelist2:
                cumul += float(dicout[element] [date])
            dicum[element] = cumul
        shedbal = (initwat + float(dicum['PR_0']) - float(dicum['AHFL_0']) - float(dicum['ALAT_0']) - float(dicum['O1_0']) - float(dicum['TRAF_60268832']) - finalwat) / \
            (initwat + float(dicum['PR_0'])) * 100
    except:
        print 'the water balance could not be computed; the variables required consist of : \r\n',  \
            'PR, AHFL, TRAF, ALAT, O1, SWAT, SICE, and SWE (canopy water is neglected)'
        shedbal = NaN
            
    output_header = ''
    for element in outelemlist:
        output_header+= element.ljust(10)[:10] + ' '
    output_header = "{:<22}".format('Date') + output_header + '\r\n'
    
    # create new output file to store post-processed variables
    outname2='./'+outname[0:-4]+'_outputs.txt'
    outfile2=open(outname2, 'w') ; del outname2
    firstline = 'for the watershed under study, the water balance is equal to ' + '%5.2f' % (shedbal) + '% over the simulation period of this file \r\n'
    outfile2.write(firstline)
    outfile2.write(output_header)
    
    for date in outdatelist:
        output_line = date + ' '
        for element in outelemlist:
            output_line += '%10.4f' % (float(dicout[element] [date])) + ' '
        output_line += '\r\n'
        outfile2.write(output_line)
    
    outfile2.close()
    return

build_fst('02HA006.out', '02HA006.intrp',5,2)
