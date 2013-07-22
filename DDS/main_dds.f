      program main_dds

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this is the main program that calls the DDS1 fortran version optimization 
!!	algorithm and saves all relevant algorithm output.

!!	Updated Nov 06 by BT to make this driver WATCLASS specific. (minor changes).  
!!	Search 'CLASS EDIT' to see changes.

!	Note that screen printing of each solution adds about 4 seconds per 10000 
!	function evaluations to the execution time.  (Otherwise 10000 long run for
!	Griewank require about 0.5 seconds on a ~2 Ghz laptop)
 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!	INCLUDE 'CXML_INCLUDE.F90' - do not use this if we can help it

	USE MOD_DDS

	implicit none
	real harvest  ! uniform random number variable
	real time_sec, time_hrs, time0, time1, runtime, CPSEC  ! timing variables
	integer j,i,k, ini_fevals, size
	integer IDAY, MONTH, IYEAR, IHOUR, MINUTE, ISEC ! run info variables

	INTEGER, ALLOCATABLE :: seeds (:) 

	LOGICAL(4) result1 ! dummy argument
	CHARACTER(100) allfile1,allfile2,allsols,
     &				file3,in_file,trialfile, all_initials, min_max
			! above are for output file naming
	CHARACTER(100) sys_command, sav_output ! Command line input string
	CHARACTER(100) save_best_name, num ! CLASS EDIT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! set timer to record program execution time
!	time_sec = SECNDS(0.0) ! requires DFLIB
!	time0 = CPSEC()  ! IMSL function
	! no timing anymore for Sharcnet

! Establish dates, time and system info:

! Sharcnet	CALL TDATE (IDAY, MONTH, IYEAR) ! num lib function
! Sharcnet	CALL TIMDY (IHOUR, MINUTE, ISEC) ! num lib function
	! call these below to calculate runtime

!   GET DDS INPUTS:
	call DDS_inp('DDS_init.txt',save_best_name) ! fixed filename, ! CLASS EDIT

! before run, prepare to manage algorithm outputs: AM added streamflow file
      out_dirname = trim('DDS_'//trim(runname))
      sys_command= 'mkdir '//out_dirname
	call system (sys_command)
	
	sav_output = trim(trim(out_dirname)//'\Streamflow')
	sys_command= 'mkdir '//sav_output
	call system (sys_command)

	if (save_best_name.NE.'') then ! CLASS EDIT
		call system ('mkdir best')  ! this must exist for batches to work
	endif


	! create proper output file locations
	allfile1=trim(trim(out_dirname)//'\all_Fbests.out')
	allfile2=trim(trim(out_dirname)//'\all_sbests.out')
	allsols=trim(trim(out_dirname)//'\summary.out')
	trialfile=trim(trim(out_dirname)//'\all_output.out')
	file3=trim(trim(out_dirname)//'\Run_info.txt')
	all_initials=trim(trim(out_dirname)//'\initials_out.txt')
	min_max=trim(trim(out_dirname)//'\minmax_info.txt')

	!AM Output min and max parameter limits for each parameter
	open(9100,file=min_max,status='unknown')
		do i=1,num_dec
			write(9100,101) i, s_min(i), s_max(i)
		enddo
	close(9100) 
	
101	format(2x,I4,2x,F10.5,2x,F10.5)		

! -----------------------------------------------------------------------------


! Echo USER inputs to output file: Simply copy input file to output directory
	in_file=trim(trim(out_dirname)//'\DDS_init.txt')
	sys_command='copy DDS_init.txt '//in_file  ! SHARCNET - must be 'cp' not 'copy' 
										! above is CLASS EDIT
	call system(sys_command)

! -------------------- INITIALIZATION OF RANDOM SEEDS	
	if (user_seed.EQ.0) then
		! set the seed based on the system clock
	    ! use this approach when submitting jobs to Sharcnet
		CALL RANDOM_SEED                        
	else ! set the seed so that trials easily replicated
		call random_seed(SIZE=size)  ! size is number elements in array, 2 usually
		ALLOCATE (seeds(size))
		seeds(1)=user_seed
		do i=1,size ! define initial set of seeds
			seeds(i)=max(1,user_seed-i)
		end do
		call random_seed(PUT=seeds(1:size)) ! set generator initially
! define seeds randomly based on user seed
		seeds(1)=user_seed
		do i=2,size
			call random_number(harvest) ! get one uniform random number
			seeds(i)=INT(harvest*2147483398.0) ! limit of seed 2 is constant
		end do
! Now have replicable, random set of seed arrays, set the generator:
		call random_seed(PUT=seeds(1:size))
	endif	
! -------------------- DONE random seed setting

      write (*,1000)
 1000 format(1x,"Dynamically Dimensioned Search (DDS) Algorithm",/,             
     &          "   MESH model version 1.1 - by Bryan Tolson",/,                  
     &          "               PC Version          ",/)

! DDS algorithm calling loop:
	do i=1,num_trials

		! CLASS EDIT:  FRANK - may want to run a batch here
		! to start with SAME initial class.ini file for multiple  opt. trials...

		 call DDS(i,trialfile,ini_fevals,save_best_name) ! CLASS EDIT

	! accumulate output across trials		
		initials(i,:)=ini_soln ! stests(ini_fevals,:)
		all_Fbests(i,:)=Fbests
		all_sbest(i,:)=sbest

	! For safely saving all results (avoiding losing during PC shutdown)
	! Put all output file writing statements below.  Output files just rewritten
	! after each new optimization trial completed.  Neglible impact on code efficiency.

	! Write initial solutions used by DDS for all trials:
		open(7700,file=all_initials,status='unknown')
		do j=1, i	! i is current number of opt trials completed
			do k=1, num_dec-1
				write(7700,111) initials(j,k)
			end do
			write(7700,112) initials(j,num_dec)
111			format(2x,e16.10,\) ! no carraige return
112			format(2x,e16.10)
		end do
		close(7700)

	! Write DDS output file for all trials (all_Fbests):
		open(7700,file=allfile1,status='unknown')
		do j=1,maxiter
			do k=1, i-1  ! i is current number of opt trials completed
						 ! not invoked after one trial
				write(7700,121) all_Fbests(k,j)
			end do
			write(7700,122) all_Fbests(i,j)
121			format(2x,e12.6,\) ! no carraige return
122			format(2x,e12.6)
		end do
		close(7700)

	! Write DDS output file for all trials (all_sbests):
		open(7700,file=allfile2,status='unknown')
		do j=1, i	! i is current number of opt trials completed
			do k=1, num_dec-1
				write(7700,121) all_sbest(j,k)
			end do
			write(7700,122) all_sbest(j,num_dec)
		end do
		close(7700)

	! Write DDS output file summarizing all trials solutions (summary):
		open(7700,file=allsols,status='unknown')
		do j=1, i	! i is current number of opt trials completed
			write(7700,121) all_Fbests(j,maxiter)
			do k=1, num_dec-1
				write(7700,121) all_sbest(j,k)
			end do
			write(7700,122) all_sbest(j,num_dec)
		end do
		close(7700)


	! CLASS EDIT, Nov07 BT.  Here to End trial loop.
	! save best CLASS specific files from each optimization run

	    if (save_best_name.NE.'') then ! CLASS EDIT
 !  Covert i from an interger to a charcter
            Open (UNIT = 101, STATUS = "SCRATCH")
			If (i.lt.10) THEN
				Write (101,51) i  
				REWIND(101)
				Read (101,*) num
			Else if (i.lt.100) THEN
				Write (101,52) i  
				REWIND(101)
				Read (101,*) num
			Else ! 100-1000 trials !
				Write (101,53) i  
				REWIND(101)
				Read (101,*) num
			End if
            Close (101)

			! make sure best directory exists:
		   sys_command='mkdir '//trim(out_dirname)//'\best'//trim(num) 
		   call system(sys_command)

		  ! copy current best folder to one named for opt trial within output dir
	sys_command='xcopy best '//trim(out_dirname)//'\best'//trim(num)
		  call system(sys_command)

	    endif

	end do  ! optimization trial loop


! write the run info file with detailed file descriptions:
!	time_sec = SECNDS(time_sec) ! stop run timer, SECNDS is not portable
!	time_hrs = time_sec/3600.0 ! also hrs
	time_sec=0.0
	time_hrs=0.0

!	time1 = CPSEC()  ! IMSL function
!	runtime = time1-time0
! NOTE THAT runtime with cpsec produces incorrect results...

! Sharcnet:
!	CALL more_output(IDAY,MONTH,IYEAR,IHOUR,MINUTE,ISEC,
!     &	out_dirname,allfile1,allfile2,trialfile,in_file,file3,
!     &    all_initials,time_sec,time_hrs)
	CALL more_output(-1,-1,-1,-1,-1,-1,   ! Sharcnet run
     &	out_dirname,allfile1,allfile2,trialfile,in_file,file3,
     &    all_initials,time_sec,time_hrs)


! Write final output messages to the screen:
	write(*,1001)
	write(*,1002) out_dirname
1001	Format(//,'DDS1 algorithm main program execution successful!')
1002	Format('  -> See algorithm output files in new subdirectory: ',a/)
! SHARCNET
!	PAUSE ! SO A double clicking .exe user sees above message 
		  ! & knows run successfully completed. 

	! CLASS EDIT:  Make sure to delete variables_in.txt and function_out.txt
	! CLASS parameter changing code uses the existence of these to signify initial
	! solution is extracted from class.ini
	sys_command='del variables_in.txt'
	call system(sys_command)
	sys_command='del function_out.txt'
	call system(sys_command)
	sys_command='del flow.bat'
	call system(sys_command)

	! CLASS edit Nov 07, BT
      sys_command='rmdir best /s /q'  ! class edit
      call system(sys_command)        ! class edit

51    Format(I1)
52	Format(I2)
53    Format(I3)

	stop
      end