#!/usr/bin/python

# this script is made to create SPS-like fst output files with stand-alone svs, in order to be able running Watroute from output files 
# produced with stand-alone svs
 
 
import numpy, os, rpnstd, os.path, datetime,  shutil, time,  math
from pylab import *

def build_fst(outname, UTCstart, X, Y): 

    if os.path.isfile(outname)==False:
        print 'the file ',  outname, 'was not found, aborting'
        return
        
    outfold='../outfst2'
    if os.path.exists(outfold):
        shutil.rmtree(outfold)
    if not os.path.exists(outfold):
        os.mkdir(outfold)
        
    outtemplate='../fst_conversion/outfst_template.fst'
    
    UTCstartnum=float(UTCstart)
    outfile=open(outname); del outname
    cont = outfile.readlines()
    outfile.close()
    outfolder='vide'
    
    daterep=datetime.datetime(1964, 05, 20)
    
    for row in cont:
        
        line = row.split()
        date=line[0]
        jour=date[0:8]
        heure=date[9:11]
        heurenum=float(heure)
        datenumcurr=datetime.datetime(int(jour[0:4]), int(jour[4:6]), int(jour[6:8]), int(heure))
        hourec=heurenum-UTCstartnum
        
        if (hourec>0):
            hourecord=hourec
            hourecord=int(hourecord)
        else:
            hourecord=heurenum+UTCstartnum
            hourecord=int(hourecord)
        if (hourecord<10):
            hourecstr='0'+str(hourecord)
        else:
            hourecstr=str(hourecord)
        
        if (heurenum==UTCstartnum+1):
            outfolder1=outfold+'/output'
            if not os.path.exists(outfolder1):
                os.mkdir(outfolder1)
            outfolder2=outfolder1+'/output_'+jour+UTCstart
            if not os.path.exists(outfolder2):
                os.mkdir(outfolder2)
            outfolder=outfolder2+'/analysis'
            if not os.path.exists(outfolder):
                os.mkdir(outfolder)
            oldjour=jour
        
        if os.path.exists(outfolder):
            outfilepath=outfolder+'/pm'+oldjour+UTCstart+'0000-00-00_0000'+hourecstr+'h'
            if not os.path.isfile(outfilepath):
                shutil.copy(outtemplate, outfilepath)
                
            towrite=rpnstd.RPNFile(outfilepath, 'RND+R/W+OLD')
            myrpnrec1 = towrite[rpnstd.RPNMeta(nom='TM')]
            var=line[1]
            niveau=line[2]
            niveau=int(niveau)
            varmat=zeros([X, Y], 'float')
            for j in range(0, Y):
                for i in range(0, X):
                    varmat[i, j]=float(line[3+j*X+i])
                
            datenum=datetime.datetime(int(oldjour[0:4]), int(oldjour[4:6]), int(oldjour[6:8]), int(UTCstartnum))
            numdate1=(datenum-daterep).days
            numdate=int(numdate1*21600+(UTCstartnum-7.111104)*900)
            
            myrpnrec2=myrpnrec1
            myrpnrec2.nom=var
            myrpnrec2.ip1=niveau
            myrpnrec2.dateo=numdate
            myrpnrec2.ip2=hourecord
            myrpnrec2.etiket='SVS'
            myrpnrec2.npas=int(hourecord*6)
            
            for j in range(0, Y):
                for i in range(0, X):
                    myrpnrec2.d[i, j]=varmat[i, j]
                
            towrite.write(myrpnrec2)
            del myrpnrec2, towrite, myrpnrec1, varmat, hourecord
            del hourec, date, jour, heure
            
            if ((heurenum==UTCstartnum)&((var=='TRAF')or(var=='ALAT')or(var=='O1')or(var=='PR')or(var=='AHFL'))):
                outfilepath1=outfolder+'/pm'+oldjour+UTCstart+'0000-00-00_000023h'
                outfilepath2=outfolder+'/pm'+oldjour+UTCstart+'0000-00-00_000000h'
                if not os.path.isfile(outfilepath2):
                    shutil.copy(outfilepath1, outfilepath2)
                    
                towrite=rpnstd.RPNFile(outfilepath2, 'RND+R/W+OLD')
                myrpnrec1 = towrite[rpnstd.RPNMeta(nom=var, ip1=niveau, ip2=23)]
                myrpnrec2=myrpnrec1
                towrite[rpnstd.RPNMeta(nom=var, ip1=niveau, ip2=23)]=None
                myrpnrec2.ip2=0
                myrpnrec2.npas=0
                for j in range(0, Y):
                    for i in range(0, X):
                        myrpnrec2.d[i, j]=0.0
                        
                towrite.write(myrpnrec2)
                del myrpnrec1, myrpnrec2, towrite, outfilepath1,  outfilepath2
                    
            del heurenum,  var,  niveau
                    

    
    return

build_fst('../02HL001.out', '12', 8, 6)
