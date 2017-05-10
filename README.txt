Program Main_netcdf2Meshbin.exe
--------------------------------
Author Gonzalo Sapriza
email g.sapriza@usask.ca
Objective:
----------
Convert netcdf files to bin sequential files formats used to run mesh
Given some information in the file (see Example_Grl.txt or Example_snow+Rain.txt)
Allows you from a list of netcdf files to convert in a unique bin sequentially format file.
Basically the program read the netcdf file and make a windows mask and then assign the values to the cells inside
the basin. If the watflood cell  are in the intersection of the grid netcdf file, the program calculate an weighted area average.

The produced binary file is write in the following way:
time 1 = 1
array of time 1
time 2 = 2
array of time 1
.
.
.
time n = n
array of time n

The position of values in the arrays are defined in base on the rank values. So you are only saving the values inside your basin.

There is an option to write in ascii but if you have big files inputs it can be a problem (up to you)

RUN the program:
----------------
Using Main_netcdf2Meshbin.exe file
You will need cygwin installed in your computer. When you install cgwin be sure to install the netcdf libraries, make, fortran. To do that when you are installing the program, there is an option to search libraries, put netcdf and fortran and the make and mark all the option that appear.
If you already have installed cygwin, you will need to install again if you don't have that libraries.

Compiling Main_netcdf2Meshbin.exe
--------------------------------
There is a makefile in the folder (makefile2). 
You will need to have installed the netcdf libraries. And then in the makefile give the proper location of this libraries.

In MESH:
--------
You have to change to 3 as follow. And of course have last version of mesh with that capability

BASINRAINFLAG          3                                #14 basin rain flag                             | A20, I4
BASINLONGWAVEFLAG      3                                #15 basin longwave flag                         | A20, I4
BASINSHORTWAVEFLAG     3                                #16 basin shortwave flag                        | A20, I4
BASINTEMPERATUREFLAG   3                                #17 basin temperature flag                      | A20, I4
BASINWINDFLAG          3                                #18 basin wind flag                             | A20, I4
BASINPRESFLAG          3                                #19 basin pressure flag                         | A20, I4
BASINHUMIDITYFLAG      3                                #20 basin humidity flag                         | A20, I4