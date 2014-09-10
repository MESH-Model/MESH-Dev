# -*- coding: utf-8 -*-
"""
Created on Fri May 09 14:59:12 2014

@author: gos730
"""

import numpy as np
import os
import scikits.timeseries as ts
import pandas as pd
import netCDF4 as nt
import save_streamflow as sw
import mask_subBasins as bs

class InputStremflow():
    
    def __init__(self,**kwargs):
        
        """
        kwargs ={'baisnName':'Mackenzie',
                 'start_date':'2000-06-01',
                 'end_date':'2010-06-31',
                 'info_fl':r'C:\00_Work\02_Sim\00_Mackenzie\01_Data\01_Selected_RiverDischarge\RiverGages_description.csv',
                 'pthIn':r'C:\00_Work\02_Sim\00_Mackenzie\01_Data\01_Selected_RiverDischarge'}
        """        
        
        for key in kwargs:
            
            setattr(self, key, kwargs[key])        
        
        self._load_rivergages()
        
        self._idate = ts.Date('D',self.start_date)
        
        self._idateHStr = self._idate.strfmt('%Y/%m/%d')+' 00:00'
        
        self._fdate = ts.Date('D',self.end_date)
        
        self._dates = ts.date_array(start_date = self._idate , 
                                    end_date   = self._fdate ,
                                    freq='d'                 )
        
        self.nr_days = self._fdate-self._idate + 1
        
        self._get_matrix()
    
    def _load_rivergages(self):
         
         self._data = pd.read_csv(self.pthIn+'/'+self.info_fl)
         
         self._riverGages = dict()
         
         cnt = 0
         
         self.nr_riverGages = self._data['STATION_NU'].size
         
         for idr in self._data['STATION_NU']:
             
             latlong = [self._data['LAT'][cnt],
                        self._data['LONG'][cnt]]    

             flIn = self.pthIn+'/'+idr+'_Daily_Flow_ts.csv'  
             print idr
             self._riverGages[idr] = RiverGauge('ts', flIn,latlong)
             
             cnt = cnt + 1
             
    
    
    def _get_matrix(self):
        
        
        _matrix = ts.time_series(-999.9*np.ones((self._dates.size     ,
                                                      self.nr_riverGages)) ,
                                      dates = self._dates                  ,
                                      freq  = 'd'                          )
         
        for k in xrange(self.nr_riverGages):
            
            rg  = self._riverGages[self._data['STATION_NU'][k]].data
            
            if rg.start_date <= self._idate and \
               rg.end_date   >= self._fdate :
                  
                   idate = rg['Flow'].date_to_index(self._idate)
                   fdate = rg['Flow'].date_to_index(self._fdate)
                   data  = rg['Flow'][idate:fdate+1]
                   _matrix[:,k] = data
                   
            elif rg.start_date > self._idate and \
                 rg.end_date   < self._fdate :
                    
                  idate = self._dates.date_to_index(rg.start_date)
                  fdate = self._dates.date_to_index(rg.end_date)
                  data  = rg['Flow']
                  _matrix[idate:fdate+1,k] = data
                  
            elif rg.start_date < self._idate  and \
                 rg.end_date   < self._fdate  and \
                 rg.end_date   > self._idate  :
                                    
                  idate = rg['Flow'].date_to_index(self._idate)
                  fdate = self._dates.date_to_index(rg.end_date)
                  data  = rg['Flow'][idate:]
                  
                  _matrix[:fdate+1,k] = data
                  
                  
            elif rg.start_date > self._idate  and \
                 rg.start_date < self._fdate  and \
                 rg.end_date   > self._fdate  :
                 
                 idate = self._dates.date_to_index(rg.start_date)
                 fdate = rg['Flow'].date_to_index(self._fdate)
                 data  = rg['Flow'][:fdate+1]
                 _matrix[idate:,k] = data                  

        self._matrix = _matrix
                                      
             
    def Write_Stremflow_file_Watroure(self,flOut,info,nodata=-999.9,proj='LATONG'):
        
        salida = open(flOut, 'w')
        
        salida.write('%s\n'%('########################################'))
        salida.write('%s\n'%(':FileType tb0  ASCII  EnSim 1.0         '))         
        salida.write('%s\n'%('#                                       '))                                       
        salida.write('%s\n'%('# DataType               EnSim Table    '))
        salida.write('%s\n'%('#                                       '))
        salida.write('%s\n'%(':Application             EnSimHydrologic'))
        salida.write('%s\n'%(':Version                 2.1.23         '))
        salida.write('%s\n'%(':WrittenBy          '+ ' Gonzalo Sapriza'))
        salida.write('%s\n'%(':CreationDate       '+  datetime.datetime.today().strftime('%Y-%m-%d %H:%M')))     
        salida.write('%s\n'%('#                                       '))
        salida.write('%s\n'%('#---------------------------------------'))
        salida.write('%s\n'%('#                                       '))
        salida.write('%s\n'%(':SourceFile         TestFile            '))
        salida.write('%s\n'%('#                                       '))
        salida.write('%s\n'%(':Name               Streamflow          '))
        salida.write('%s\n'%('#                                       '))
        salida.write('%s\n'%(':Projection           '+ proj            ))
        salida.write('%s\n'%(':Ellipsoid          unknown             '))
        salida.write('%s\n'%('#                                       '))
        salida.write('%s\n'%(':StartTime          '+self._idateHStr    ))
        salida.write('%s\n'%(':DeltaT                        1'        ))
        salida.write('%s\n'%(':RoutingDeltaT                 0'        ))
        salida.write('%s\n'%('#                                       '))        
        salida.write('%s\n'%(':FillFlag           n'                   ))
        salida.write('%s\n'%('#                                       '))
        salida.write('%s\n'%(':ColumnMetaData                                        '))

        salida.write('%s'%('   :ColumnType       '))
        for i in xrange(self.nr_riverGages):
            salida.write('%s'%('float '))
        salida.write('\n')
        
        
        salida.write('%s'%('   :ColumnUnits        '))
        for i in xrange(self.nr_riverGages):
            salida.write('%s'%('m3/s '))
        salida.write('\n')
        
        salida.write('%s'%('   :ColumnName        '))
        for i in xrange(self.nr_riverGages):
            salida.write('%s %s'%(self._data['STATION_NU'][i] ,' '))
        salida.write('\n') 
        
        salida.write('%s'%('   :ColumnLocationX   '))
        for i in xrange(self.nr_riverGages):
            salida.write('%7.2f %s'%(self._riverGages[self._data['STATION_NU'][i]]._long,' '))
        salida.write('\n')

        salida.write('%s'%('   :ColumnLocationY   '))
        for i in xrange(self.nr_riverGages):
            salida.write('%6.3f %s'%(self._riverGages[self._data['STATION_NU'][i]]._lat,' '))
        salida.write('\n')

        salida.write('%s'%('   :Coeff1            '))
        for i in xrange(self.nr_riverGages):
            salida.write('%s'%('0 '))
        salida.write('\n')
        
        salida.write('%s'%('   :Coeff2            '))
        for i in xrange(self.nr_riverGages):
            salida.write('%s'%('0 '))
        salida.write('\n')
        
        salida.write('%s'%('   :Coeff3            '))
        for i in xrange(self.nr_riverGages):
            salida.write('%s'%('0 '))
        salida.write('\n')

        salida.write('%s'%('   :Coeff4            '))
        for i in xrange(self.nr_riverGages):
            salida.write('%s'%('0 '))
        salida.write('\n')

        salida.write('%s'%('   :Value1      '))
        for i in xrange(self.nr_riverGages):
            salida.write('%s'%('0 '))
        salida.write('\n')
        
        salida.write('%s\n'%(':EndColumnMetaData   '))                                                                                                                                                                                                                                                                                                                   
        salida.write('%s\n'%('#                                       '))  
        salida.write('%s\n'%(':EndHeader                              '))
                                
#        for  i in xrange(self.nr_days):
#            
#            for j in xrange(24):
#                
#                for k in xrange(self.nr_riverGages):
#                    
#                    val  = self._riverGages[self._data['STATION_NU'][k]]
#                    data = val.data['Flow']
#                    
#                    indx = data.date_to_index(self._idate)
#                    fndx = data.date_to_index(self._fdate)
#                    data = data[indx:fndx+1]  
#                
#                    if data[i]:
#                        
#                        val = data[i]/24.
#                    
#                    else:
#                        
#                        val = nodata
#                                                    
#                    salida.write('%10.3f'%(val))r'MESH_drainage_database.r2c'
#                
#                salida.write('\n')                      
        
        salida.close()
        
        sw.save_streamflow.tb0(flOut,self._matrix.data)
        
                                               
    def Write_Streamflow_file(self,info,nodata=-999.9):
        
        salida = open('MESH_input_streamflow.txt','w')

        salida.write('%s\n'%(info))
                        
        salida.write('%5d %5d %5d %5s %5d %5d %5s\n'%(self.nr_riverGages      ,
                                                      self.nr_days            ,
                                                      self.nr_days            ,
                                                      ' 24 '                  ,
                                                      self._idate.year        ,
                                                      self._idate.day_of_year ,
                                                      ' 00 '                 ))
        
        for i in xrange(self.nr_riverGages):            
            
            idrg = self._data['STATION_NU'][i]
            val = self._riverGages[idrg]
            salida.write('%5d %5d %5s \n'%(int(round(val._latMIN))  ,
                                           int(round(val._lonMIN))  ,
                                           self._data['STATION_NU'][i] ))        
                    
        salida.close()
                                
        sw.save_streamflow.meshfmt('MESH_input_streamflow.txt',self._matrix.data)
        
def GetRasterInt(flOut,vIn,xmin,ymin,xmax,ymax,xres,yres,ncols,nrows,epsg=4269):
    
    from osgeo import gdal
    #from osgeo import gdal_array
    from osgeo import osr
    
    geotransform=(xmin,xres,0,ymax,0, -yres)
    
    dataS = gdal.GetDriverByName('GTiff').Create(flOut,ncols, nrows, 1 ,gdal.GDT_Int32)
    dataS.SetGeoTransform(geotransform)
    srs = osr.SpatialReference()
    srs.ImportFromEPSG(epsg) 
    dataS.SetProjection( srs.ExportToWkt() )
    dataS.GetRasterBand(1).WriteArray(vIn)
    dataS = None
    
    
        
class SubBasin_mask():
    
    """
    kw = {'drainage_db' :{'flr2c':'r'MESH_drainage_database.r2c',
                          'onlyFields':True,
                          'ids':None,
                          'skiprows':72},
          'subBasin':{'flIn':'subBasin.csv'},
          'write':{'fldOut':''},
          'climate_forcing':{'pthIn':'',
                             'fmt':'seq',                                                         
                             'fls':{'pressure':'basin_pres.seq',
                                     'temp'
                             },
          'lrg_modelInfo':{'initprogClass':'',
                           'NTYPE':8,
                           'IGND':4}                             }}
                           
    Example:
        kw = {'drainage_db':{'flr2c':'MESH_drainage_database.r2c'},
              'subBasin':{'flIn':'Preselected_subBasins_2.csv'},
              'write':{'fldOut':'SubMask_p5'},
              'climate_forcing':{ 'pthIn':r'/home/gonzalo/00_Work/02_Sim/00_Mackenzie/03_ClimateData/01_WFDEI_02_12',
                                  'fmt':'seq',
                                  'fls':{'pressure':'basin_pres.seq',
                                        'rain':'basin_rain.seq',
                                        'temp':'basin_temperature.seq',
                                        'longwave':'basin_longwave.seq',
                                        'humidity':'basin_humidity.seq',
                                        'shortwave':'basin_shortwave.seq',
                                        'wind':'basin_wind.seq'}},
              'lrg_modelInfo':{'initprogClass':'/home/gonzalo/00_Work/02_Sim/00_Mackenzie/02_Model/int_statVariables.seq',
                               'NTYPE':8,
                               'IGND' :4}}                           
    """    
    def __init__(self,**kwargs):
        
        for key in kwargs:
            
            setattr(self, key, kwargs[key])      
                    
        self.drg_db = Mesh_drainage_database(self.drainage_db['flr2c'])   
            
        self.sbB_info = pd.read_csv(self.subBasin['flIn'])
        self.nr_sbB = self.sbB_info['STATION_NU'].shape[0]
        
        xi = np.arange(1,self.drg_db.xcount+1,1)
        yi = np.arange(1,self.drg_db.ycount+1,1)
        
        self._jj,self._ii = np.meshgrid(xi,yi)
        
        self._GetSubBasins()        
        
        if kwargs.has_key('write'):
            
            self.Write_drainage_db()
            
        if kwargs.has_key('climate_forcing'):
            
            self.Msk_ClimateForcings()
            
        if kwargs.has_key('lrg_modelInfo'):
            
            if kwargs['lrg_modelInfo'].has_key('initprogClass'):
                
                self.Msk_InitCond()
            
    
    def _GetSubBasins(self):
        
        bss = bs.mask_subbasins
        
        self.sbB = dict()
        rnk = np.asarray(self.drg_db.wmapFlds['Rank'],dtype=int)
        nxt = np.asarray(self.drg_db.wmapFlds['Next'],dtype=int)
        
        for i in xrange(self.nr_sbB):
            
            ids = self.sbB_info.STATION_NU.values[i]
            lat = self.sbB_info.LAT.values[i]
            lon = self.sbB_info.LONG.values[i]

            self.sbB[ids] = dict()
            self.sbB[ids]['msk'], self.sbB[ids]['ij_start'] =  bss.get_subbasin(lon               ,
                                                     lat               ,
                                                     self.drg_db.lon0  ,
                                                     self.drg_db.lat0  ,
                                                     self.drg_db.dylat ,
                                                     self.drg_db.dxlon ,
                                                     rnk,nxt           )
                                                                   
            msk = self.sbB[ids]['msk'] == 1    
            
            self.sbB[ids]['nr_grids'] = self.sbB[ids]['msk'].sum()
                                                         
            #xloc = np.ma.array(self._ii,mask=~msk)
            #yloc = np.ma.array(self._jj,mask=~msk)     
            
            xmin,xmax = self._jj[msk].min(),self._jj[msk].max()        
            ymin,ymax = self._ii[msk].min(),self._ii[msk].max()        
                                                
            self.sbB[ids]['j_rg'] =  [xmin,xmax]                                                              
            self.sbB[ids]['i_rg'] =  [ymin,ymax]   
            
            dx=float(self.drg_db._fl[':xDelta'])
            
            self.sbB[ids]['xOrigin'] = xmin*dx + \
                                       float(self.drg_db._fl[':xOrigin'])- dx
            
            dy=float(self.drg_db._fl[':yDelta'])                        
           
            self.sbB[ids]['yOrigin'] = ymin*dy+ \
                                       float(self.drg_db._fl[':yOrigin'])-dy
            
            self.sbB[ids]['msk_wnd'] = self.sbB[ids]['msk'][ymin-1:ymax,
                                                            xmin-1:xmax]
                                                            
            ir = self.sbB[ids]['ij_start'][2]-1
            jr = self.sbB[ids]['ij_start'][3]-1
            
            for key,val in self.drg_db.wmapFlds.iteritems():
                
                if key != 'Rank':
                    vals = val
                    vals[ir,jr] = 0.
                else:
                    vals = val
                    
                vals = vals[ymin-1:ymax,xmin-1:xmax]
                vals = np.where(self.sbB[ids]['msk_wnd']==0,0,vals)
                self.sbB[ids][key] =  vals     

            
            self.sbB[ids]['Rank_c'],self.sbB[ids]['Next_c']  = bss.renumerate(self.sbB[ids]['Rank'] ,
                                                      self.sbB[ids]['Next'] )     
            
            yxCount = self.sbB[ids]['Rank_c'].shape                                          
            self.sbB[ids]['xCount'] = yxCount[1]
            self.sbB[ids]['yCount'] = yxCount[0]
                
            self.sbB[ids]['classCount'] = 0
            self.sbB[ids]['lc_ids'] = []            
            self.sbB[ids]['msk_lc_indx'] = []            
            
            kk = 1
            for fld in self.drg_db._ids[12:]:
                
                if self.sbB[ids][fld].sum()>0:
                    
                    self.sbB[ids]['lc_ids'].append(fld)
                    self.sbB[ids]['classCount'] = self.sbB[ids]['classCount'] + 1        
                    self.sbB[ids]['msk_lc_indx'].append(kk)
                    
                kk = kk + 1 
                
            self.sbB[ids]['lc_ids'].append('Fake')          
                          
                                                      
    def Write_drainage_db(self):
        
        os.mkdir(self.write['fldOut'])
        
        for key, rg in self.sbB.iteritems():
            
            os.mkdir(self.write['fldOut']+'/'+key)
            
            pth = self.write['fldOut']+'/'+key + '/'
            rg['pthOut'] = pth
            
            self.WriteHeader(rg)
            
            self.WriteMatrix(rg)
            
        
    def WriteHeader(self,rg):
         
        r2c = open(rg['pthOut']+ 'MESH_drainage_database.r2c', 'w')
        
        for i in xrange(len(self.drg_db._fl['header'])):
            
            r2c.write(self.drg_db._fl['header'][i])
        
        r2c.write('%s\n'%(':SourceFile   '         + self.drg_db._fl[':SourceFile']        ))    
        r2c.write('%s\n'%(':NominalGridSize_AL   ' + self.drg_db._fl[':NominalGridSize_AL']))    
        r2c.write('%s\n'%(':ContourInterval   '    + self.drg_db._fl[':ContourInterval']   )) 
        r2c.write('%s\n'%(':ImperviousArea   '     + self.drg_db._fl[':ImperviousArea']   ))         
        r2c.write('%s\n'%(':ClassCount   '         + str(rg['classCount']+1)               ))    
        r2c.write('%s\n'%(':NumRiverClasses   '    + self.drg_db._fl[':NumRiverClasses']   ))    
        r2c.write('%s\n'%(':ElevConversion   '     + self.drg_db._fl[':ElevConversion']    ))    
        r2c.write('%s\n'%(':TotalNumOfGrids   '    + str(rg['nr_grids'])                   ))    
        r2c.write('%s\n'%(':NumGridsInBasin   '    + str(rg['nr_grids']-1)                 ))
        r2c.write('%s\n'%(':MinimumSlope   '       + self.drg_db._fl[':MinimumSlope']      ))

        r2c.write('%s\n'%('#'))
        r2c.write('%s\n'%('#'))
        
        r2c.write('%s\n'%(':Projection   ' + self.drg_db._fl[':Projection']))
        r2c.write('%s\n'%(':Ellipsoid   '  + self.drg_db._fl[':Ellipsoid'] ))

        r2c.write('%s\n'%('#'))
        
        r2c.write('%s\n'%(':xOrigin   '    + str(rg['xOrigin'])))
        r2c.write('%s\n'%(':yOrigin   '    + str(rg['yOrigin'])))        

        r2c.write('%s\n'%('#'))        
         
        for i in xrange(18):
            
            r2c.write('%s'%(self.drg_db._fl['header2'][i]))
            
        k = 13
        for i in xrange(len(rg['lc_ids'])):
            
            lin1 = ':AttributeName ' +str(k)+' '+rg['lc_ids'][i]+'('+str(i+1)+')'                       
            lin2 = ':AttributeUnits '+str(k)+' '+ ' 0-1'            
            r2c.write('%s\n'%(lin1))
            r2c.write('%s\n'%(lin2))
            k = k + 1                                                        
    
        r2c.write('%s\n'%('#')) 
        r2c.write('%s\n'%(':xCount   '  + str(rg['xCount'])))
        r2c.write('%s\n'%(':yCount   '  + str(rg['yCount'])))
        r2c.write('%s\n'%(':xDelta   '  + self.drg_db._fl[':xDelta']))
        r2c.write('%s\n'%(':yDelta   '  + self.drg_db._fl[':yDelta']))
        r2c.write('%s\n'%('#')) 
        r2c.write('%s\n'%(':EndHeader')) 
        
        r2c.close()
        
    
    def WriteMatrix(self,rg):
        
        fl = rg['pthOut']+ 'MESH_drainage_database.r2c'
        
        bs.mask_subbasins.writematrix(fl,rg['Rank_c'])
        bs.mask_subbasins.writematrix(fl,rg['Next_c'])
        
        for fld in self.drg_db._ids[2:12]:
            
            bs.mask_subbasins.writematrix(fl,rg[fld])
            
        for fld in rg['lc_ids']:
            
            if fld == 'Fake':
                v = np.zeros((rg['yCount'],rg['xCount']))                                
            else:                                
                v = rg[fld]
                
            bs.mask_subbasins.writematrix(fl,v)
        
    def Msk_ClimateForcings(self):
        """
        Description:
        Mask the climate forcing data. For the moment only for the seq format
        
        'climate_forcing':{'pthIn':'',
                             'fmt':'seq',                                                         
                             'fls':{'pressure':'basin_pres.seq',
                                     'temp':}}
        """
        
        ngrd = int(self.drg_db._fl[':TotalNumOfGrids'])
        
        for key, rg in self.sbB.iteritems():
            
            rnk = rg['Rank'][rg['Rank']>0.]
            rnk.sort()
            
            for key,val in self.climate_forcing['fls'].iteritems():
                
                flIn  = self.climate_forcing['pthIn']+'/'+ val
                flOut = rg['pthOut'] + '/' + val
                
                if self.climate_forcing['fmt'] == 'seq':
                    
                    bs.mask_subbasins.msk_climate_forcing(flIn,flOut,rnk, ngrd)
                
        
    def Msk_InitCond(self):
        
        
        ngrd = int(self.drg_db._fl[':TotalNumOfGrids'])
        
        flIn = self.lrg_modelInfo['initprogClass']
        
        ntype = self.lrg_modelInfo['NTYPE']        
        ignd  = self.lrg_modelInfo['IGND']    
        
        
        for key, rg in self.sbB.iteritems():
        
            rnk = rg['Rank'][rg['Rank']>0.]
            rnk.sort()
            
            msk_lc = np.asarray(rg['msk_lc_indx'],dtype=int)
            
            flOut =  rg['pthOut'] + '/' + 'int_statVariables.seq'
            
            bs.mask_subbasins.msk_initprogclass(flIn  , flOut ,
                                                rnk   , ngrd  ,
                                                ntype , ignd  , msk_lc)   
            
            
    def Copy_BasicFiles(self):
        """ Files that not change"""
        pass
    
    def ReWrite_ClassIni(self):
        
        pass
        

def Get_RadMeshForm(DEGLAT,DEGLON,AL,XCOUNT,YCOUNT):    

    PI = np.pi
    
    LATLENGTH = AL/1000./(111.136-0.5623*np.cos(2*(DEGLAT*PI/180.0))+ \
    0.0011*np.cos(4*(DEGLAT*PI/180.0)))
    
    LONGLENGTH=AL/1000./(111.4172*np.cos((DEGLAT*PI/180.0))- \
    0.094*np.cos(3*(DEGLAT*PI/180.0))+0.0002*np.cos(5*(DEGLAT*PI/180.0)))
    
    RADJGRD = np.zeros(YCOUNT)
    DLONGRD = np.zeros(XCOUNT)
    
    YYY = np.arange(1,YCOUNT+1,1)
    XXX = np.arange(1,XCOUNT+1,1)

    for I in xrange(YCOUNT):
        
        RADJGRD[I]=((DEGLAT-(YCOUNT/2.0)*LATLENGTH) +(YYY[I]-0.5)*LATLENGTH)*PI/180.

    for I in xrange(XCOUNT):
        
        DLONGRD[I]=(DEGLON-(XCOUNT/2.0)*LONGLENGTH) +(XXX[I]-0.5)*LONGLENGTH

    return RADJGRD,DLONGRD
    

            
class RiverGauge():
    
    def __init__(self,fmt,flIn,latlong,freq='d'):
        
        self.fileIn = flIn

        self._fmtIn = fmt
        
        if fmt is 'csv':
           
           self.data = pd.read_csv(flIn, 
                                   parse_dates={'Timestamp': ['Date']},
                                   index_col='Timestamp')                                                                     
                           
           
        
        if fmt is 'ts':
            
            self.data = ts.tsfromtxt(flIn            ,
                                     delimiter = ',' ,
                                     freq = freq     ,
                                     skiprows  =  1  ,
                                     datecols  =  2  ,
                                     names=['ID','PARAM','Flow','SYM'])
                
        self.idGage = self.data['ID'][0]
        
        self._lat  = latlong[0]        
        self._long = latlong[1]
        
        self._latMIN = self._lat*60. 
        self._lonMIN = self._long*60. 

        if self.data.freqstr == 'D':        
            self._nhours = 24
        elif self.data.freqstr == 'H':
            self._nhours = 1            
        
    def Write_MESH_tsi(self,start_date,end_date,pthOut,nodata=0.0):
        
        self._idate = ts.Date(self.data.freq, start_date)
        self._fdate = ts.Date(self.data.freq, end_date)
        

        data = self.data['Flow']
        indx = data.date_to_index(self._idate)
        fndx = data.date_to_index(self._fdate)
        data = data[indx:fndx+1]
        
        salida = open(pthOut+'\\MESH_input_streamflow.txt','w')

        salida.write('%s\n'%('Only one river gage'))
        
        salida.write('%5d %5d %5d %5d %5d %5d %5s \n'%(1,
                                      data.size               ,
                                      data.size               ,
                                      self._nhours            ,
                                      self._idate.year        ,
                                      self._idate.day_of_year ,
                                      ' 00 '                  ))
                                      
        salida.write('%5d %5d %5s \n'%(int(round(self._latMIN))  ,
                                       int(round(self._lonMIN))  ,
                                       self.idGage               ))
        for i in xrange(data.size):
            
            if data[i]:
                
                val = data[i]
            
            else:
                
                val = nodata
                        
            salida.write('%10.3f\n'%(val))
            
        salida.close()


class Mesh_drainage_database():
    
    
    def __init__(self,flIn):
        
        
        self.wmapFlds = dict()
        self.flIn = flIn
             
        self.ReadHeader()    
        
        self.xcount = int(self._fl[':xCount'])
        self.ycount = int(self._fl[':yCount'])
        self.lon0   = float(self._fl[':xOrigin'])
        self.lat0   = float(self._fl[':yOrigin'])
        self.dxlon  = float(self._fl[':xDelta'])
        self.dylat  = float(self._fl[':yDelta'])
        
        v = np.loadtxt(flIn, skiprows=self._skiprows) 
        
        rows = v.shape[0]/len(self._ids)

        for i in xrange(len(self._ids)):
            
            self.wmapFlds[self._ids[i]]=v[i*rows:i*rows+rows,:]
                
        
        self._shpFlds = self.wmapFlds[self._ids[i]].shape
        
        self._mask = self.wmapFlds['DA'] > 0
        
        
    def ReadHeader(self):
        
        r2c = open(self.flIn,'r')
        
        self._fl = dict()
        self._fl['header'] = []      
        
        cnt = 0

        for i in xrange(11):
            
            self._fl['header'].append(r2c.readline())
            cnt = cnt + 1
        
        for i in xrange(10):
            key,val = r2c.readline().split()
            self._fl[key] = val
            cnt = cnt + 1
        
        r2c.readline()
        r2c.readline()
        cnt = cnt + 2
        
        for i in xrange(2):
            key,val = r2c.readline().split()            
            self._fl[key] = val
            cnt = cnt + 1
            
        r2c.readline()
        cnt = cnt + 1
        
        for i in xrange(2):
            key,val = r2c.readline().split()            
            self._fl[key] = val  
            cnt = cnt + 1
        
        r2c.readline()
        cnt = cnt + 1
        
        self._nr_ldcv = int(self._fl[':ClassCount'])
        
        self._nr_att = self._nr_ldcv + 12
        
        self._ids = []
        self._fl['header2'] = []  
        
        for i in xrange(18+self._nr_ldcv*2):     
            
            line1 = r2c.readline()
            
            self._fl['header2'].append(line1)
            cnt = cnt + 1
            
            key = line1.split()    

            if key[0] == ':AttributeName':
                self._ids.append(key[2])
            
        r2c.readline()
        cnt = cnt + 1
         
        for i in xrange(4):
            key,val = r2c.readline().split()            
            self._fl[key] = val  
            cnt = cnt + 1
        
        cnt = cnt + 2
        
        self._skiprows = cnt
        r2c.close()     
        
        
    def _verfy_dirElev(self,rank=None,nextt=None):
        
        """ Pasarlo a fortran """
        
        self._locErrElev = []
        self._rankErr = []
        self._erro_Rank = []
        
        if rank is None:
            rank = self.wmapFlds['Rank']
        if nextt is None:
            nextt = self.wmapFlds['Next']
                
        for i in xrange(self._shpFlds[0]):
            
            for j in xrange(self._shpFlds[1]):
                
                if self._mask[i,j]:
                
                    elev1 = self.wmapFlds['Elev'][i,j]
                    
                    nxt = nextt[i,j]
                    rnk = rank[i,j]

                    
                    elev2 = self.wmapFlds['Elev'][np.where(rank== nxt)]
                    
                    
                    if nxt < rnk:
                        
                        self._erro_Rank.append([i,j])
                    
                    if elev2 > elev1:
                        
                        self._locErrElev.append([i,j])
                        self._rankErr.append(rank[i,j])
                        
                        
    def _get_NextDir(self,dirct,rank=None,elev=None,model='f90'):
        
        import drainage_verifyMESH as vf
        
        if elev is None:
            
            elev = self.wmapFlds['Elev']
        
        if rank is None:
            
            if model == 'scipy':

                import scipy.stats as st

                rks = st.rankdata(elev.flatten(),
                                  method='ordinal').reshape(elev.shape)
                                  
            elif model == 'f90':

                import mrgref as mr
                
                minElev = elev[elev>0.].min()
                n_zero = sum(elev<minElev)
                elev[elev<minElev] = 1e-3*np.random.rand(n_zero)
                elev_p = np.asarray(elev.flatten(),dtype=np.float64)
                sM = mr.m_mrgref(elev_p)
                smr = np.empty(sM.shape,dtype=int)
                
                for i in xrange(smr.size):
                    smr[sM[i]-1] = smr.size-i
                 
                rks = smr.reshape(elev.shape)

                rks[elev<minElev]=0                
                
            self._rks = rks                   

        else:
            
            rks = self.wmapFlds['Rank']
                               
                               
        self._nxt = vf.drainage_verify.rank_vrf(rks,dirct,elev)
        
        
    def _CorrectSameElev(self,elev,direction,factor=100.):    
        
        
        import drainage_verifyMESH as vf

        msk = elev == 0.
        mskG = ~msk
        
        self._elevCorr = vf.drainage_verify.corrct_elevsame(elev      ,
                                                            direction ,
                                                            mskG      ,
                                                            factor    )
        
        
    
    def _GetClasses(self,rstfl,nclass,ids):
        
        
        """
        ids = ['Forest', 'Grass', 'Wetland','Cropland',
               'Barrenland','Urban','Water', 'Glacier','NoData']
        rstfl = r'C:\00_Work\02_Sim\00_Mackenzie\03_Geo\04_LandCover\CCRS2005\NA_LandCover_2005V2_25haMMU_latlon_region_reclass.tif'
        
        """

        
        import get_distr as gt
        
        lc_a = np.load(rstfl)

        # Informacion del raster origen y resolucion tendriamos que obtnerlo
        # directament del raster cuando lo leemos
        # la info de el shed file como origen y resolucion tambien tendriamos
        # que obtenerla directamente del .r2c file en la lectura
        vs = gt.get_distr.ups_dstrb(lc_a,50.7,-141.,
                                    0.002851531243180,            
                                    51,-140.5,0.125000,
                                    308,156,nclass)
        j = 0                                    
        for i in ids:
            
            if ids == 'NoData':
                
                self.wmapFlds[i] = np.zeros((156,308))
        
            else:
            
                self._ids.append(i)
                
                rows = vs.shape[0]/len(ids)
    
                for i in xrange(len(self._ids)):
                    
                    self.wmapFlds[self._ids[i]]=vs[j*rows:j*rows+rows,:]
                    
                    j = j + 1
        
        
    
    def Write_shedTXT(self):
        
        pass
    
    
class Grl_ClimateForcing():
    
    def __init__(self,**kwargs):
        
        for key in kwargs:
            
            setattr(self, key, kwargs[key])
            
            
class ClimateForcing():
    
    def __init__(self,**kwargs):
        
        """
        kwargs = {'dataSource':'WFDEI',
                  'start_date':'2002-01-01',
                  'end_date':'2009-12-31',
                  'freq' :'3h',
                  'formatIn':'netcdf',                  
                  'pthIn':'',
                  
                  'mask':{'lat':[]   ,
                          'lon':[]   ,
                          'lat0':,'lon0':,
                          'dx':,'dy':,
                          'mask_array':r''}
                  }

        """        
        for key in kwargs:
            
            setattr(self, key, kwargs[key])
        
        self._lstfls = os.listdir(self.pthIn)
        self._unit   = np.random.randint(1,300)
        
        self.mask['msk'] = np.load(self.mask['mask_array'])
        
        self.mask['ntgt'] = self.mask['msk'].sum()
        
        if self.formatIn is 'netcdf':
            
            self.ntcdf2binMesh()
            
    
                          
    def ntcdf2binMesh(self):
        
        import ntcdf2BinMesh as wrt
        
        for i in xrange(len(self._lstfls)):
                    
            print i, self._lstfls[i]
            
            fli = self.pthIn + '\\' + self._lstfls[i]
            
            ncfl = nt.Dataset(fli,'r',format='NETCDF3_CLASSIC')
            
            if i == 0:
                
                wrt.ntcdf2binmesh.writefl(self._unit     ,
                                          False          ,
                                          'Rain'         ,
                                          True           ,
                                          np.ones((2,3)) )
                
                self._GetMaskNetCDF(ncfl)
 
            vls = ncfl.variables[self._varId][:,self._ilat,self._ilon]            
            print vls.shape
            valOut = wrt.ntcdf2binmesh.getvalloc(vls               ,
                                                 self._latMsk      ,
                                                 self._lonMsk      ,
                                                 self.mask['lat0'] ,
                                                 self.mask['lon0'] ,
                                                 self.mask['dx']   ,
                                                 self.mask['msk']  ,
                                                 self.mask['ntgt'] )
            
            
            print 'Ahora a escribir los valores'
            wrt.ntcdf2binmesh.writefl(self._unit  ,
                                      False       ,
                                      'Rain'      ,
                                      False       ,
                                      valOut      )
            
            ncfl.close()
        
        wrt.ntcdf2binmesh.writefl(self._unit     ,
                                  True           ,
                                  'Rain'         ,
                                  False          ,
                                  np.ones((2,3)) )
                                      
                                  
    
    def _GetMaskNetCDF(self,ncfl):
        
        import search_index as sch
        
        lat_c = ncfl.variables['lat']   

        v_lat = lat_c[:][(lat_c[:] <= self.mask['lat'][0]) & \
                         (lat_c[:] >= self.mask['lat'][1])]
        
        self._latMsk = v_lat
    
        lon_c = ncfl.variables['lon']

        v_lon = lon_c[:][(lon_c[:] <= self.mask['lon'][0]) & \
                         (lon_c[:] >= self.mask['lon'][1])]
        
        self._lonMsk = v_lon
    
        self._ilat = sch.search_index(ncfl.variables['lat'][:],v_lat)
        self._ilon = sch.search_index(ncfl.variables['lon'][:],v_lon)  
        
        
        
        
                          
        
def SaveInitProgClass():

    """
    Save the init conditions generated in the spin up under windows that are 
    saved as numpy array and save in  binary seq in linux format readed in
    Mesh
    """
    import load_progIniClass as ld
    pth = r'/media/gonzalo/OS/00_Work/02_Sim/00_Mackenzie/02_Model/01_SpinUp/InitCond'
    v=['ALBSROW'    , 
                        'CMAIROW'    , 
                        'GROROW'    , 
                        'QACROW'     , 
                        'RCANROW'    , 
                        'RHOSROW'    , 
                        'SCANROW'    , 
                        'SNOROW'     , 
                        'TACROW'     , 
                        'TBARROW'    , 
                        'TBASROW'    , 
                        'TCANROW'    , 
                        'THICROW'    , 
                        'THLQROW'    , 
                        'TPNDROW'    , 
                        'TSFSROW'    , 
                        'TSNOROW'    , 
                        'WSNOROW'    , 
                        'ZPNDROW']
                        
    data = dict()
    for i in xrange(len(v)):
        data[v[i]] = np.load(pth+'/'+v[i]+'.npy')        
        
    ld.load_proginiclass.save_initseq(r'/home/gonzalo/00_Work/02_Sim/00_Mackenzie/02_Model/00_InputFls/Init_cond.seq',
                                      data['ALBSROW'],data['CMAIROW'],data['GROROW'],
                                      data['QACROW'] ,data['RCANROW'],data['RHOSROW'],
                                      data['SCANROW'],data['SNOROW'] ,data['TACROW'],
                                      data['TBARROW'],data['TBASROW'],data['TCANROW'],
                                      data['THICROW'],data['THLQROW'],data['TPNDROW'],
                                      data['TSFSROW'],data['TSNOROW'],data['WSNOROW'],
                                      data['ZPNDROW'])        
        
        
        
        
        
        