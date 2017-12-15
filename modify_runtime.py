#!/usr/bin/python
import  sys, datetime
fp1 = open('/fs/home/fs1/eccc/oth/nlab_west/aql000/my_proj_hall2/MESH_Forecast/capa_hindcasts/05KJ001/SaMESHRunShort/MESH_input_run_options.ini.orj','r')
fp2 = open('/fs/home/fs1/eccc/oth/nlab_west/aql000/my_proj_hall2/MESH_Forecast/capa_hindcasts/05KJ001/SaMESHRunShort/MESH_input_run_options.ini','w')
year = int(sys.argv[1])
mon  = int(sys.argv[2])
day  = int(sys.argv[3])
hour = int(sys.argv[4])
fmt = '%Y%m%d'
dt = datetime.datetime.strptime(str(year)+str(mon)+str(day),fmt)
tt =dt.timetuple()
jdate = int(tt.tm_yday)  
jyear = int(tt.tm_year)
dt2 = dt + datetime.timedelta(days=int(sys.argv[5]))
tt2 =dt2.timetuple()
jdate2 = int(tt2.tm_yday)                         
jyear2 = int(tt2.tm_year)
print jdate, jyear, jdate2, jyear2 

ft = fp1.readlines()
for i, line in enumerate(ft):
#   print i, year, line
   if i == 37:
      fp2.write(('{:4d}'*4+'\n').format(jyear, jdate, hour, 0))
   elif i == 38:
      fp2.write(('{:4d}'*4+'\n').format(jyear2, jdate2, hour, 0))
   else:
      fp2.write(line)
fp1.close()
fp2.close()
# modify Class configuration file
fp1 = open('/fs/home/fs1/eccc/oth/nlab_west/aql000/my_proj_hall2/MESH_Forecast/capa_hindcasts/05KJ001/SaMESHRunShort/MESH_parameters_CLASS.ini.orj','r')
fp2 = open('/fs/home/fs1/eccc/oth/nlab_west/aql000/my_proj_hall2/MESH_Forecast/capa_hindcasts/05KJ001/SaMESHRunShort/MESH_parameters_CLASS.ini','w')
ft = fp1.readlines()
for i, line in enumerate(ft):
#   print i, year, line
   if i == 126:
      fp2.write(('{:10d}'*4+'\n').format(hour, 0, jdate, jyear))
   else:
      fp2.write(line)
fp1.close()
fp2.close()

