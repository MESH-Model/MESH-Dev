#!/usr/bin/python


import numpy, os              #import useful packages
import datetime
from pylab import *
from matplotlib import pyplot
from matplotlib import gridspec
import rpnstd
import subprocess, string

def compare_strfw(date1,date2,output_file, obs_file, exper_case, gauge):
    
    date1 = date1[0:8]
    date2 = date2[0:8]
    
    start = datetime.datetime(int(date1[0:4]), int(date1[4:6]), int(date1[6:8]))
    startstr = start.strftime("%Y%m%d")
    end = datetime.datetime(int(date2[0:4]), int(date2[4:6]), int(date2[6:8]))
    endstr=end.strftime("%Y%m%d")
    length = (end-start).days + 1
    del date1, date2
    
    qall = zeros([length], 'float')
    oall = zeros([length], 'float')
    strdates = []
    dates=[]
    
    # get the parameter information so that we can print it onto the graph and keep track of tuning
    if os.path.isfile(obs_file)==True:
      obsfile = open(obs_file,'r')
      usls = obsfile.readline()
      obsdat = obsfile.readlines()
      del obsfile
      
      store = bool(0)
      index = 0
      for line in obsdat:
          date = line.split()[0]
          flow = float(line.split()[2])
          if date == startstr:
              store = bool(1)
          if store:
              index += 1
              oall[index-1] = flow
              dates.append(datetime.datetime(int(date[0:4]), int(date[4:6]), int(date[6:8])))
          if date == endstr:
              store = bool(0)
              
    else:
      print 'the file ',  obs_file, ' was not found, aborting'
      return
    
    if os.path.isfile(output_file)==True:
      outfile = open(output_file,'r')
      usls = outfile.readline(); usls = outfile.readline(); 
      outdat = outfile.readlines()
      del outfile
      
      store = bool(0)
      index = 0
      for line in outdat:
          date = line.split()[0][0:8]
          flow = float(line.split()[11])
          if date == startstr:
              store = bool(1)
          if store:
              index += 1
              qall[index-1] = flow
          if date == endstr:
              store = bool(0)
              
    else:
      print 'the file ',  output_file, ' was not found, aborting'
      return


    of = oall[numpy.where((oall>0) & (qall>0))]
    qf = qall[numpy.where((oall>0) & (qall>0))]
    
    perc_com = float(len(of))/float(len(oall))*float(100)
    
    ##      datesf = dates[numpy.where(o>0)]
    mynse = nse(qf,of)
    mylnse = nse(log10(qf),log10(of))
    mysqrnse = nse(sqrt(qf),sqrt(of))
    mymae = mae(qf,of)
    mybias = pbias(qf,of)
    mydmb = dmb(qf,of)
    
    try:
        mypearson = pow(pearson(qf,of),2)
    except:
        mypearson = 'NaN'
    
    fig = figure(figsize=(8,4))
    gs = gridspec.GridSpec(2, 1, height_ratios=[1, 5])
    gs.update(left=0.10, right=0.95, bottom=0.15)
    clf()
    
    ax2=pyplot.subplot(gs[1])
    maxim = max(max(qall), max(oall))
    ylabel('Streamflow (m3/s)')
    plot(dates,oall,'k-',label='Observed')
    plot(dates,qall,'c-',label='Simulated')
    
    if (gauge == '02HL001'):
        reg = 'regulated'
    #       ax2.set_ylim([0,400])
        rivname='Moira'
    elif (gauge == '02HM003'):
        reg = 'natural'
    #       ax2.set_ylim([0,120])
        rivname='Salmon'
    elif (gauge == '0423160'):
        reg = 'regulated'
        rivname='Genessee'
    elif (gauge== '0424900'):
        reg = 'regulated'
        rivname='Oswego'
    elif (gauge== '0425075'):
        reg = 'natural'
        rivname='Sandy'
    elif (gauge == '0426050'):
        reg = 'regulated'
    #       ax2.set_ylim([0,1400])
        rivname='Black'
    elif (gauge== '0425020'):
        reg = 'regulated'
    #       ax2.set_ylim([0,250])
        rivname='Salmon'
    elif (gauge== '02HA006'):
        reg = 'natural'
        rivname='20 mile'
    elif (gauge== '02HK003'):
        reg = 'regulated'
    #       ax2.set_ylim([0,350])
        rivname='Crowe'
    elif (gauge == '02HK002'):
        reg = 'regulated'
    #       ax2.set_ylim([-200,1200])
        rivname='Trent'
    elif (gauge== '02MB006'):
        reg = 'natural'
        rivname='Lyn'
    else:
        reg = 'unspecified regime'
        rivname='unknown'
    
    leg = legend(loc=[0.4,0.80],borderpad=0.1,markerscale=0.1,labelspacing=0,borderaxespad=0.1)
    for t in leg.get_texts():
        t.set_fontsize('small') 
    
    majlabels = [tick.label1 for tick in gca().xaxis.get_major_ticks()]
    majlines = gca().xaxis.get_gridlines()
    counter = 0
    for i in majlabels:
        i.set_horizontalalignment('right')
        i.set_verticalalignment('top')
        i.set_fontsize(10)
        i.set_rotation(30)
    
    ax1=pyplot.subplot(gs[0])
    ax1.set_xticklabels([], visible=False)
    ax1.set_yticklabels([], visible=False)
    ax1.patch.set_visible(False)
    ax1.set_title(exper_case+ ", gauge: " + gauge + ', '  + rivname+ ' river, '+ reg,  y=0.95, size=13)
    ax1.axis('off')
    
    nsestr  = 'NSE :  %4.2f' % mynse
    sqnse = 'SQRT_NSE : %4.2f' % mysqrnse
    lnsestr = 'LNSE: %4.2f' % mylnse
    if (mypearson <> 'NaN'):
        rstr    = 'R$^2$  :   %4.2f' % mypearson
    else:
        rstr = 'NaN'
    maestr  = 'MAE :  %4.2f' % mymae
    biasstr = 'PBIAS: %4.2f' % mybias
    dmbstr  = 'DMB :  %4.2f' % mydmb
    
    #print perfs
    ax1.annotate(nsestr  ,xy=(0.5,0.5),xytext=(.1,.60),textcoords='axes fraction',zorder=100, size=11)
    ax1.annotate(sqnse  ,xy=(0.5,0.5),xytext=(.35,.60),textcoords='axes fraction',zorder=100, size=11)
    ax1.annotate(lnsestr ,xy=(0.5,0.5),xytext=(.60,.60),textcoords='axes fraction',zorder=100,size=11 )
    ax1.annotate(rstr    ,xy=(0.5,0.5),xytext=(.85,.60),textcoords='axes fraction',zorder=100, size=11)
    ax1.annotate(maestr  ,xy=(0.5,0.5),xytext=(.1,.20),textcoords='axes fraction',zorder=100, size=11)
    ax1.annotate(biasstr ,xy=(0.5,0.5),xytext=(.35,.20),textcoords='axes fraction',zorder=100, size=11 )
    ax1.annotate(dmbstr  ,xy=(0.5,0.5),xytext=(.60,.20),textcoords='axes fraction',zorder=100, size=11)
    
    savefig(gauge+ '_'+exper_case+'.png', dpi=None, facecolor='w', edgecolor='w', format='png', transparent=False)
          
    print 'strfw_compare_daily.py DONE'
    return

def nse(sim,obs):
  nse = 1-(sum(pow((sim-obs),2))/sum(pow((obs-(sum(obs)/len(obs))),2)))
  return nse
def mae(sim,obs):
  return sum(fabs(sim-obs))/len(sim)
def rmse(sim,obs):
  return sqrt(sum(pow(sim-obs,2))/len(sim))
def pbias(sim,obs):
  return 100*sum(obs-sim)/sum(obs)
def dmb(sim,obs):
  return sum(sim)/sum(obs)
def pearson(sim,obs):
  avg_x = average(sim)
  avg_y = average(obs)
  diffprod = 0
  xdiff2 = 0
  ydiff2 = 0
  for idx in range(len(sim)):
      xdiff = sim[idx] - avg_x
      ydiff = obs[idx] - avg_y
      diffprod += xdiff * ydiff
      xdiff2 += xdiff * xdiff
      ydiff2 += ydiff * ydiff
  return diffprod / math.sqrt(xdiff2 * ydiff2)

#compare_strfw('20070602','20110927','Black_files/0426050_outputs.txt','Black_files/Black_Q.txt', 'EGnew_defpars2', '0426050')
compare_strfw('20070602','20110927','02HA006_outputs.txt','20mile_Q.txt', 'defpars', '02HA006')
#compare_strfw('20070602','20110927','Moira_files/02HL001_outputs.txt','Moira_files/Moira_Q.txt', 'EGnew_freeze_defpars', '02HL001')
#compare_strfw('20070602','20110927','Humber_files/02HC003_outputs.txt','Humber_files/Humber_Q.txt', 'EGnew_defpars', '02HC003')
#compare_strfw('20070602','20110927','Iron_files/0423205_outputs.txt','Iron_files/Irondequoit_Q.txt', 'test_defpars', '0423205')
