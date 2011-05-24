      subroutine DDS(trial_num,file1,ini_fevals,save_best_name)

!	Dynamically dimensioned Search (DDS) version 1.1 algorithm by Bryan Tolson
!	Fortran version (original was coded in Matlab)
!	Coded in Nov 2005 by Bryan Tolson.  
!	Updated Nov 06 to allow user to run batch file when best solutinon is updated.
!	Updated in Dec06-Jan07 to be used for general simulation model
!	auto-calibration (such as Watflood or Watclass). Includes:
!		 - lots of printing statements moved (all_output after each function evaluation)
!		 

!	DDS is an n-dimensional continuous global optimization algorithm.  It is coded as a 
!	minimizer but built into the code is a hidden transformation to make it a 
!	maximizer.  In a maximization problem, the algorithm minimizes 
!	the negative of the objective function F (-1*F).  User specifies in inputs 
!	whether it is a max or a min problem.
! 	
!	Algorithm reference:  Tolson and Shoemaker WRR paper

!	DDS optimization algorithm - all inputs are global
!		see main_DDS.f for variable definitions

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units		|definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    trial_num		|none		| current optimization trial number 
!!	file1			|name		| name of optional algorithm output file
!!    ini_fevals		|none		| # of function evaluations to initialize DDS
!!    save_best_name	|none		| name of batch file to run when jbest is updated 
!!								| (in main DDS loop only) - CLASS EDIT. Nov 06.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!	all local variables are defined in code right after they first appear
!	global variables defined in MOD_DDS

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!	obj_func - user specified objective function.
!	random_number - intrinsic fortran uniform random # generator
!	neigh_value - 1-dimensional decision variable perturbation routine


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

	USE MOD_DDS

	implicit none

	REAL(8) ranval,zvalue,Ftest,fvalue,Pn,new_value
	INTEGER i, j, k, dvn_count, dv, ini_fevals, ind1, trial_num, ileft
	CHARACTER(100) name_p1, file1, sys_command ! output file naming variables
	character(100) save_best_name ! CLASS EDIT
	character(100) streamflow, trial, num, dummy !added by AM for output
	
!	AM need a scrach pad to convert num to string
	OPEN(2948, Status = "SCRATCH")
	WRITE(2948,*) trial_num
	REWIND(2948)
	READ(2948,*) dummy
	trial = trim(dummy)
	CLOSE(2948)

      write (*,999) trial_num
  999 format(1x,"DDS trial ",i3," executing....   ",/)

      IF (ini_name.NE.'') THEN ! user supplied fixed initial solution
		ini_fevals=1		 ! either from initials.txt or class.ini 
		stest=initials(trial_num,:)  ! from class.ini option sets stest to dummy values
	write (*,1000) 
 1000	format(5x,"Evaluating user supplied initial solution....  ",/)

	ELSE ! standard random initialization:
! Calculate # function evals to use for random sampling to initialize DDS 
	ini_fevals=max(5,IDNINT(0.005*dfloat(maxiter)))
	write (*,1001) 
 1001	format(5x,"Sampling for initial DDS solution....   ",/)
	ENDIF

	ileft=maxiter-ini_fevals ! reduce number of fevals in DDS loop
	if (ileft.LE.0) then
		stop 'Error! #Initialization samples >= Max # func evaluations.'
	endif

! for all_output file (writes after each iteration thru the code)
	if (out_print.EQ.0) then
		if (trial_num.EQ.1) then
			open(7700,file=file1,ACCESS='SEQUENTIAL',status='unknown')
			write(7700,116) 
116			Format ('Output from ALL optimization trials in this file.'/)
			close(7700)
		endif
	
		open(7700,file=file1,ACCESS = 'APPEND',status='unknown')
		write(7700,118) trial_num
118		FORMAT(//'DDS optimization trial ',i4,' outputs below:'/)
		write(7700,119)
119		FORMAT('#Fevals      Fbest         Ftest',5x,
     &'| -> Decision variable values 1, 2, etc. producing Ftest --> ')
		close(7700)
	endif

! Use random sampling (rs) algorithm to initialize DDS ------------------------------------
		! note:  The only way to start runs from same solution for a different
		! maxiter limit is to read in the initial solution matrix from an input file
	do i=1, ini_fevals
		if (ini_name.EQ.'') THEN
		! sample an initial solution candidate:
			do j=1, num_dec
				call random_number(ranval) ! get one uniform random number
 				!  selects next uniform random # in sequence
				!  intrinsic routine (no library)
				stest(j)=s_min(j)+ranval*(s_max(j)-s_min(j))
			end do
		endif 
		! else, stest already assigned 

		! Evaluate initial solution:
		call obj_func(num_dec,stest,fvalue)
		! when above called w stest=-999.9, it returns correct stest from variables_in.txt 
		Ftest=to_max*fvalue  ! to_max handles min and max problems
		if (i.EQ.1) then ! Fbest must be initialized
			Fbest=Ftest
			sbest = stest
		endif

	    if (Ftest.LE.Fbest) then ! update current (best) solution
			Fbest = Ftest
			sbest = stest
		endif

!	MAM
      open(100,file='pre_emption_value.txt', status='unknown')
      write(100,*)Fbest
      close(100)

	! accumulate DDS initialization outputs
		f_count(i)=i
		Ftests(i)=to_max*Ftest
		Fbests(i)=to_max*Fbest
		stests(i,:)=stest

	!	write(*,*) f_count(i), to_max*Fbest ! *** user uncomment if desired ***
!---------------
!	Write all output dds file line by line so nothing lost
!	BT used to write this at end of each trial (below)
!     outputs: Fbests, Ftests, stests (append)
!	AM copied the output files 
	if (out_print.EQ.0) then
		open(7700,file=file1,ACCESS = 'APPEND',status='unknown')
		write(7700,120) i, Fbests(i), Ftests(i)
		do k=1, num_dec-1
			write(7700,121) stests(i,k)
		end do
		write(7700,122) stests(i,num_dec)
		close(7700)

!		Added by AM, renames and moves the streamflow file based output name, trial and interation
c 		open(1994, file = "flow.bat", Status ='unknown')
c		write(1994,*) "move streamflow.csv "//trim(out_dirname)//
c     &		"\Streamflow\"//"Stream_out_T"//trim(trial)//'_1.csv'
c 		close(1994)
c		call system("flow.bat")
		

	endif
!---------------

	end do	! initialization rs sampling loop
	ini_soln=sbest
! -----------------------------------------------------------------------------

! start the main DDS algorithm loop for remaining allowble function evaluations
	write(*,1002)
 1002	format(5x,"Done DDS initialization. DDS running...",/)

	do i=1, ileft ! new maxiter
		! Determine variable selected as neighbour 
		Pn=1.0-DLOG(DFLOAT(i))/DLOG(DFLOAT(ileft)) ! prob selected
		dvn_count=0 ! counter for how many dec vars vary in neighbourhood
		stest=sbest  ! define stest initially as best current solution
		
		do j=1,num_dec
			call random_number(ranval) ! get one uniform random number
			if (ranval.LT.Pn) then ! jth DV selected to vary in neighbourhood
               dvn_count=dvn_count+1
				! call 1-D perturbation function to get new DV value (new_value)
               call neigh_value(sbest(j),s_min(j),
     &								s_max(j),r_val,new_value)
               stest(j)=new_value ! change relevant dec var value in stest
			endif
		end do

	    if (dvn_count.EQ.0) then ! no DVs selected at random, so select ONE
			call random_number(ranval) ! get one uniform random number
			dv=ceiling(DFLOAT(num_dec)*ranval) ! index for one DV   
			  ! call 1-D perturbation function to get new DV value (new_value)
			call neigh_value(sbest(dv),s_min(dv),s_max(dv),
     &											r_val,new_value)
			stest(dv)=new_value ! change relevant dec var value in stest
		endif

	! Evaluate obj function for stest:
		call obj_func(num_dec,stest,fvalue)
		Ftest=to_max*fvalue  ! to_max handles min (=1) and max (=-1) problems, 
		
	    if (Ftest.LE.Fbest) then ! update current (best) solution
			Fbest = Ftest
			sbest = stest
			if (save_best_name.NE.'') then ! CLASS EDIT
				call system(save_best_name)
			endif
			if (out_print.EQ.0) then ! update status file for long runs so
				! that intermediate results are never lost
				open(7999,file='status.out',status='unknown')
				write(7999,130) i, to_max*Fbest
				do k=1, num_dec-1
					write(7999,121) sbest(k)
				end do
				write(7999,122) sbest(num_dec)
				close(7999)
			endif
		endif

	! accumulate DDS outputs
		ind1=i+ini_fevals ! proper index for storage
		f_count(ind1)=ind1
		Ftests(ind1)=to_max*Ftest
		Fbests(ind1)=to_max*Fbest
		stests(ind1,:)=stest
		
!	MAM
      open(100,file='pre_emption_value.txt', status='unknown')
      write(100,*)Fbest
      close(100)

!	WRITE SCREEN OUTPUT - *** user uncomment if desired ***:
		write(*,*) f_count(ind1), to_max*Fbest

!	AM Write to scratch !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!Finish writing to scrach!!!!!! (num) 	
	OPEN(2948, Status = "SCRATCH")
	WRITE(2948,*) ind1
	REWIND(2948)
	READ(2948,*) dummy
	num = trim(dummy)
	CLOSE(2948)



!------------------
!	Write all output dds file line by line so nothing lost
!	BT used to write this at end of each trial (below)
!     outputs: Fbests, Ftests, stests (append)
	if (out_print.EQ.0) then
		open(7700,file=file1,ACCESS = 'APPEND',status='unknown')
		write(7700,120) ind1, Fbests(ind1), Ftests(ind1)
		do k=1, num_dec-1
			write(7700,121) stests(ind1,k)
		end do
		write(7700,122) stests(ind1,num_dec)
		close(7700)

!		Added by AM, renames and moves the streamflow file based output name, trial and interation
	 	open(1994, file = "flow.bat", Status ='unknown')
		write(1994,*) "move MESH_output_streamflow.csv "//trim(out_dirname)//
     &		"\Streamflow\"//"Stream_out_T"//trim(trial)//'_'//
     &		trim(num)//'.csv'
 		close(1994)
		call system("flow.bat")
	endif
		
!------------------

	end do ! main DDS loop

!	WRITE SCREEN OUTPUT - *** user uncomment if desired ***:
!		write(*,*) f_count(ind1), to_max*Fbest

! FORMAT statements used many times above:
         
120			format(i7,3x,e12.6,2x,e12.6\) ! no carraige return
121			format(2x,e12.6,\) ! no carraige return
122			format(2x,e12.6)


130	format('Fbest after ',i7,' function evaluations is ',e12.6,
     &' under the following decision variable values:'/)

	if (out_print.EQ.0) then
		! Delete the status file after results saved in standard output file above
		sys_command='del status.out'
		call system(sys_command)
	endif

	return
	END

