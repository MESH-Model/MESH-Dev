      subroutine more_output(IDAY,MONTH,IYEAR,IHOUR,MINUTE,ISEC,
     &	out_dirname,allfile1,allfile2,trialfile,in_file,file3,
     &    all_initials,time_sec,time_hrs)

!	Purpose is to write detailed summary output file (file3) for DDS algorithm. 
!	Summary output file describes all output files produced by program.
!	Bryan Tolson, Nov. 2005.

	implicit none

	real time_sec, time_hrs  ! run timing variables
	integer IDAY, MONTH, IYEAR, IHOUR, MINUTE, ISEC ! run info variables
	CHARACTER(100) out_dirname,allfile1,allfile2,
     &				file3,in_file,trialfile,all_initials
			! above are for output file naming

      open(7700,file=file3,status='unknown')
	write(7700,90)

	if (IDAY.EQ.-1) THEN  ! time/date functions not used (eg Sharcnet run)
		write(7700,99) trim(out_dirname)
	ELSE
		write(7700,91) trim(out_dirname),IDAY,MONTH,IYEAR,IHOUR,MINUTE,ISEC
	ENDIF

	if (time_sec.gt.0.0) then
		write(7700,92) time_sec, time_hrs
	endif

	write(7700,*) '******** Detailed Output file descriptions:'
	write(7700,93)
	write(7700,94) allfile1 !  Fbests
	write(7700,95) allfile2 !  sbests
	write(7700,96) trialfile 
	write(7700,97) in_file ! input file
	write(7700,98) all_initials ! INI SOLUTION file

		 
90    format   ("Dynamically Dimensioned Search (DDS) Global",/,             
     &          "Optimization Algorithm - for WATmodels",/,                  
     &          "  - by Bryan Tolson. FORTRAN PC Version 1.1",//
     &"Reference: Tolson, B.A. & Shoemaker, C.A.  (2007). ",
     &"Dynamically dimensioned search algorithm for computationally",/,
     &"efficient watershed model calibration. Water Resources Res.",/,
     &"43, W01413, doi:10.1029/2005WR004723.",//,
     &"See README_WATmodel_1.1.pdf file for Disclaimer, Copyright, and ",
     &"detailed user instructions.",/, 
     &"--------------------------------------------------------------",
     &"---------------------"/)

91	format(/"This directory, e.g. ",a,
     &        " , contains all algorithm output ",
     &		"files for DDS main program executed on (day-mo-yr): ",/,
     &		"	",i2,"-",i2,"-",i4," at time (hr-min-sec): ", 
     &		i2,"-",i2,"-",i2,/)

99	format(/"This directory, e.g. ",a,
     &        " , contains all algorithm output ",
     &		"files for DDS main program."/)

92    format(/"Total program execution time was",f10.1," seconds",
     &" or ",f8.3," hours."//)

93    format(/"In the output files (and source code): ",/,
     &"- Decision variable called DV, also referred to by s",/,
     &"- DVs printed by column: col i = DV#i, col i+1 = DV#i+1, etc",/,
     &"- Objective function called function, func, also F etc.",/,
     &"- Fbest is best F value found so far, Ftest is F value for",
     &" the new test solution (stest) in current iteration (iter)",/,
     &"- the iteration# is also the # of F evaluations or Fevals",
     &" so far",/,
     &"- Ftest and Fbest printed by row."/)

94    format("File: ",a,/,
     &"   - contains Fbest values for all trials.",/,
     &"   - column 1 is trial 1, column 2 is trial 2 etc.",/,
     &"   - row 1 is for iteration 1, row 2 is for iteration 2 etc.",/,
     &"   - e.g. row 10 has best F value found after 10 objective",/,
     &"     function evaluations, for all optimization trials."/)

95    format("File: ",a,/,
     &"   - contains sbest values for all trials.",/,
     &"   - column 1 is DV#1, column 2 is DV#2 etc.",/,
     &"   - row 1 for optimization trial 1, row 2 for trial 2 etc.",/,
     &"   - e.g. row 10 has best solution found (sbest) by DDS in ",/,
     &"     optimization trial 10."/)
 
96    format("File: ",a,/,
     &"   - input flag controls whether this file is printed or not",/,
     &"   - contains ALL sbest, stest, Fbest, Ftest for all trials.",/,
     &"   - column headers and notes in file are self explanatory."/)

97    format("File: ",a,/,
     &"   - a copy of the DDS input file used for this run",/,
     &"   - enables easy replication of results."/)
      
98    format("File: ",a,/,
     &"   - contains the initial solution values for all trials.",/,
     &"   - column 1 is DV#1, column 2 is DV#2 etc.",/,
     &"   - row 1 for optimization trial 1, row 2 for trial 2 etc.",/,
     &"   - e.g. row 10 has initial solution for in trial 10",/,
     &"   - Copy & rename this file to *initials.txt* and it can be ",/,
     &"   - used to specify initial DDS solutions for another run."/)

	close(7700)

	return
	END

