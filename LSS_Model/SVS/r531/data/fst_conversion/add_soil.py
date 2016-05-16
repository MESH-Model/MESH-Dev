#!/usr/bin/python

import numpy, os, rpnstd, os.path, datetime

def add_soil():
    """this script allows to erase the soil data from the file 'GenPhysXout.fst' 
    and add to it the soil data from the file 'GenPhysXout_aux.fst', which contains the data for the 8 layers derived 
    from the BNU database; otherwise, 'GenPhysXout_aux.fst' only contains data for the first five layers. """
    
    file1 = rpnstd.RPNFile('GenPhysXout_aux.fst','RND+R/W+OLD')
    file2 = rpnstd.RPNFile('GenPhysXout.fst','RND+R/W+OLD')

    # erase
    for i in range(0, 5):
        file2[rpnstd.RPNMeta(nom='J1')] = None
        
    for i in range(0, 5):
        file2[rpnstd.RPNMeta(nom='J2')] = None
        
    #add
    myrpnrec1 = file1[rpnstd.RPNMeta(nom='J1',ip1=1199)]
    file2.write(myrpnrec1)
    
    myrpnrec2 = file1[rpnstd.RPNMeta(nom='J1',ip1=1198)]
    file2.write(myrpnrec2)
    
    myrpnrec3 = file1[rpnstd.RPNMeta(nom='J1',ip1=1197)]
    file2.write(myrpnrec3)
    
    myrpnrec4 = file1[rpnstd.RPNMeta(nom='J1',ip1=1196)]
    file2.write(myrpnrec4)
    
    myrpnrec5 = file1[rpnstd.RPNMeta(nom='J1',ip1=1195)]
    file2.write(myrpnrec5)
    
    myrpnrec6 = file1[rpnstd.RPNMeta(nom='J1',ip1=1194)]
    file2.write(myrpnrec6)
    
    myrpnrec7 = file1[rpnstd.RPNMeta(nom='J1',ip1=1193)]
    file2.write(myrpnrec7)
    
    myrpnrec8 = file1[rpnstd.RPNMeta(nom='J1',ip1=1192)]
    file2.write(myrpnrec8)
    
    myrpnrec1 = file1[rpnstd.RPNMeta(nom='J2',ip1=1199)]
    file2.write(myrpnrec1)
    
    myrpnrec2 = file1[rpnstd.RPNMeta(nom='J2',ip1=1198)]
    file2.write(myrpnrec2)
    
    myrpnrec3 = file1[rpnstd.RPNMeta(nom='J2',ip1=1197)]
    file2.write(myrpnrec3)
    
    myrpnrec4 = file1[rpnstd.RPNMeta(nom='J2',ip1=1196)]
    file2.write(myrpnrec4)
    
    myrpnrec5 = file1[rpnstd.RPNMeta(nom='J2',ip1=1195)]
    file2.write(myrpnrec5)
    
    myrpnrec6 = file1[rpnstd.RPNMeta(nom='J2',ip1=1194)]
    file2.write(myrpnrec6)
    
    myrpnrec7 = file1[rpnstd.RPNMeta(nom='J2',ip1=1193)]
    file2.write(myrpnrec7)
    
    myrpnrec8 = file1[rpnstd.RPNMeta(nom='J2',ip1=1192)]
    file2.write(myrpnrec8)
    
    del file1, file2
    
    return
    
add_soil()
