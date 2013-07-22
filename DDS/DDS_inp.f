      subroutine DDS_inp(filename1,save_best_name)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the DDS algorithm inputs from a text file.
!	Also checks that all inputs are valid.
!	Bryan Tolson, Nov. 2005

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    filename1   |none          | fixed DDS algorithm input file name 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    eof         |none          |end of file flag
!!    titldum     |NA            |title line for input file
!	see coding for additional comments
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!	DDS_allocate

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

	USE MOD_DDS

 
	character (len=100) :: titldum1,linedum,line3,line4, line9
	character (len=100) :: line15, save_best_name ! CLASS EDIT
	character (len=12) :: filename1  ! DDS input filename 
!	character (len=6) :: DVname 		! decision variable name.  CLASS EDIT
	real(8) realval_seed, real_num_dec, lower, upper, value 
	! 6 lines below are CLASS EDITS
	real(8) p_low1,p_up1	! temp arrays, parameter bounds. 
	real(8), dimension (:), allocatable :: dv_low1,dv_up1 ! temp arrays, 
							! DV bounds. 1 is for Land class INDEP params 
	real(8), dimension (:), allocatable :: p_low2,p_up2 ! temp arrays, 
							! parameter bounds. 
	real(8), dimension (:), allocatable :: dv_low2,dv_up2 ! temp arrays, 
								! DV bounds. 2 is for Land class DEP params 
	integer i, j, eof, ind1 
	integer nclass,num_INDEP_parms,num_DEP_parms,initial_option,maxL
	integer count_DVs, o_flag,num_DVs1 ! o_flag is for class INDEP params
	! Above is CLASS EDIT

	integer, dimension (:), allocatable :: opt_flag2 ! temp arrays, CLASS EDIT
	! ^ Flags to optimize or not. 2 = land class DEP parms
	open(110,file=filename1,status='unknown')

      read (110,5000) titldum
	read (110,5000) linedum
      read (110,5000) line3  
	runname=trim(line3(1:24)) ! short name used to create output files.
	if (runname.EQ.'') then
		runname='out'
	endif


      read (110,5000) line4  
		ind1=SCAN(line4(1:24),'!')
		if (ind1>0) then
			app_name=trim(ADJUSTL(line4(1:max(1,ind1-1))))
		else
			app_name=trim(ADJUSTL(line4(1:24)))
		endif			 
	if (app_name(1:1).EQ.'!') THEN	! assume this should be blank
		app_name(1:24)=''  ! means that no external application called, i.e.
						   ! obj. function compiled with DDS1 program.
	endif


      read (110,*) num_trials  ! number of optimization trials to run.
		if (num_trials.LT.1.OR.num_trials.GT.1000) then
			stop 'Error! Enter # opt. trials in range [1, 1000]!'
		endif

		
      read (110,*) maxiter   ! the max # of iterations or obj. function calls 
			! in each optimization trial.  DDS always runs to maximum iterations.
		if (maxiter.LT.5.OR.maxiter.GT.1000000) then
		stop 'Error! Enter max# of function evals in range 6-1000000!'
		endif

			 
	read (110,*) realval_seed  ! [1, 2147483562] 1st random seed range
					! to fix random # generator state so results can be replicated.
		user_seed=IDNINT(realval_seed)
		if (user_seed.LT.0.OR.user_seed.GT.2147483562) then
			stop 'Error! Enter random seed input in range [0, 2147483562]!'
		endif


      read (110,*) out_print  ! flag for minimizing number of output files. 
				 !  "0" to save all information or "1" to save only summary info.
		if (out_print.EQ.0.OR.out_print.EQ.1) then
			! nothing
		else
			stop 'Error! Enter "0" or "1" for output printing flag!'
		endif

	! CLASS EDIT here.  Used to read filename.  Now read option #
	read (110,*) initial_option ! line 9
!	see end of file for more initial solution coding

	read (110,5000) linedum ! nothing
	read (110,5000) user_comments ! line 10.  User Comments about specific run.


	read (110,*) to_max  ! line 12. -1 = maximization, 1 = minimization -> 
!       when algorithm called it acts as a minimizer.  This input seemlessly allows 
!	  users to max or min their original objective function.
		if (to_max.EQ.-1.0.OR.to_max.EQ.1.0) then
			! nothing
		else
			stop 'Error! Enter "-1" or "1" for to_max input!'
		endif


	read (110,*) r_val  ! line 13.  DDS perturbation parameter = std dev/DV  
!			range to determine variance of normal DV perturbation. Scales all DVs to be same 
!			perturbation size realtive to their DV range  
		if (r_val.LE.0.0.OR.r_val.GT.1.0) then
			stop 'Error! Enter DDS perturbation parameter (0.0, 1.0]!'
		endif


	read (110,5000) linedum ! line 14.  For a future DDS parameter.

      read (110,5000) line15  ! batch or exe name to run when best soln updated
		ind1=SCAN(line4(1:24),'!')
		if (ind1>0) then
			save_best_name=trim(ADJUSTL(line15(1:max(1,ind1-1))))
		else
			save_best_name=trim(ADJUSTL(line15(1:24))) ! can return empty, OK
		endif			 

	maxL=5000  ! use to allocate temporary arrays for possible DVs. CLASS EDIT

	read (110,*) num_INDEP_parms ! line 16  ! CLASS EDIT
		if (num_INDEP_parms.LT.0) then
		   stop 'Error! Enter line16 input >= 0!'
		endif


	read (110,*) num_DEP_parms ! line 17  ! CLASS EDIT
		if (num_DEP_parms.LT.0) then
			stop 'Error! Enter >= 0 on line 17 !'
		endif


	read (110,*) nclass ! line 18  ! CLASS EDIT
		if (nclass.LT.1) then
			stop 'Error! Enter >= 1 on line 18 !'
		endif

	if (nclass*num_DEP_parms.GT.maxL) then ! CLASS EDIT
		stop 'Error! Increase maxL variable in DDS_inp.f!'
	endif

	read (110,5000) linedum ! line 19

! Allocate new variables to hold all possible DV information.  ! CLASS EDIT
	allocate(dv_low1(maxL))
	allocate(dv_up1(maxL))
	allocate(p_low2(nclass))
	allocate(p_up2(nclass))
	allocate(opt_flag2(nclass))
	allocate(dv_low2(maxL))
	allocate(dv_up2(maxL))


! Get all possible decision variable info and determine true decision variables:
	eof=0
	count_DVs=0	! counter for number of parameters flagged for optimization

! Land class independent parameters first.  CLASS EDIT

	DO i=1, num_INDEP_parms

		read (110,*) lower
		read (110,*) upper 
		read (110,*) o_flag

		if (eof < 0) then
			stop 'Error! # of INDEP params listed not = line16 !?!?'
	    endif

		if (o_flag.EQ.1) then  ! parameter is optimized
			if (lower.GT.upper) then
			 stop 'Error! Mixed up order of 1 of the DV bounds.'
			endif
      		if (lower.EQ.upper) then
			 stop 'Error! Lower & Upper of 1 of DV bounds are equal!'
			endif
			count_DVs=count_DVs+1
			dv_low1(count_DVs)=lower
			dv_up1(count_DVs)=upper
		endif

	enddo
	
	num_DVs1=count_DVs ! use this to help assign DDS DV arrays properly
	
! Land class DEpendent parameters next.  CLASS EDIT

	DO i=1, num_DEP_parms
		 
		read (110,*) (p_low2(j),j=1,nclass) 
		read (110,*) (p_up2(j),j=1,nclass) 
		read (110,*) (opt_flag2(j),j=1,nclass) ! opt flag

		if (eof < 0) then
			stop 'Error! # DEP params/#classes not = line16 !?!?'
	    endif

		do j=1, nclass	! check which are optimized
			if (opt_flag2(j).EQ.1) then  ! parameter is optimized
			 if (p_low2(j).GT.p_up2(j)) then
			  write(*,*) i,j
			  stop 'Error! Optimized DEP param # above, min>max !?!?'
			 endif
      		 if (p_low2(j).EQ.p_up2(j)) then
			  write(*,*) i,j	
			  stop 'Error! Optimized DEP param bound # above same!'
			 endif
			 count_DVs=count_DVs+1
			 dv_low2(count_DVs-num_DVs1)=p_low2(j)
			 dv_up2(count_DVs-num_DVs1)=p_up2(j)
			endif
		enddo

	enddo

	num_DVs2=count_DVs-num_DVs1 ! # of land class dependent DVs. CLASS EDIT
	num_dec=count_DVs ! CLASS EDIT
	
	call DDS_allocate ! allocate arrays used by DDS algorithm.  
				! Moved later due to CLASS EDIT
				! Must do this variable allocation here.

! CLASS EDIT BELOW: assign DDS min and max arrays from temp arrays:
	do i=1, num_DVs1
		s_min(i)=dv_low1(i)
		s_max(i)=dv_up1(i)
	enddo
	
	do i=1, num_DVs2
		s_min(i+num_DVs1)=dv_low2(i)
		s_max(i+num_DVs1)=dv_up2(i)
	enddo

! code below for init soln is CLASS EDIT
! Initial solution handling:
	eof=0

	if (initial_option.EQ.1) then ! option 1 -  is random solutions.  Nothing.
		ini_name='' ! needed in dds.f code later
	elseif (initial_option.EQ.2) then ! option 2 - "initials.txt" file option
		ini_name='initials.txt'
		! must check bounds here
		open(120,file='initials.txt',status='unknown')
		DO i=1, num_trials
		  read(120,*,IOSTAT=eof) (initials(i,j), j=1,num_dec) 	
		   if (eof < 0.OR.eof > 0) then
		 	 write(*,*)'Wrong initial soln format in initials.txt.'
		 	 stop 'File must have: #rows=#trials, #colums=#DVs.'
	       endif
		  ! check that initial value within bounds:
		   do j=1, num_dec
		   value=initials(i,j) 
		    if (value.LT.s_min(j).OR.value.GT.s_max(j)) then
		     write(*,101) i, j
101		     Format(/'Initial value in row ',i3,' col ',i3,
     &		           ' is outside of DV bounds.')		
		     stop 'Error! Fix initial solutions to be within bounds.'
		    endif
		   enddo
	    END DO
		close (120)
	elseif (initial_option.EQ.3) then ! option 3 - initials from CLASS files
		! parameter changing exe handles this as it looks in variables_in.txt
		! for the specific value below in the first line.
		!  Print dummy flag signifying this option to edit_class to exe
		ini_name='dummy' ! to make dds.f work properly w this option & multiple opt trials
		DO i=1, num_trials
		   do j=1, num_dec
				initials(i,j)=-999.9
		   enddo
		enddo
	else
		stop 'Error! Enter 1,2 or 3 as initial soln option, line 9.'
	endif

5000  format (a)
      close (110)

	return
      end
