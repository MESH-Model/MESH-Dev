      subroutine obj_func(nopt,x_values,fvalue)
c
! general coding to call external or previously compiled function with 
! Fortran DDS program

! Bryan Tolson.  Nov 2005.

	USE MOD_DDS

	integer nopt
	real(8) fvalue
      real(8), dimension(nopt) :: x_values
	character(100) streamflow

! I/O Variable definitions: 
c	nopt		the number of decision variables
c	x_values	an array of decision variable values (size nopt)
c	fvalue		the value of the objective function with x_values as input
c	app_name	the name of the user supplied executable or batch file that
c				reads new DDS supplied decision variable values in STEP 1 below,
c				evaluates the objective function for the new decision variables (STEP 2),
c			    and then writes the objective function value to a file (STEP 3).
				
! STEP 1. Code to help transfer all decision variable values to proper simulation model 
!    (or other external program) input variable locations:
      open(unit=9, file='variables_in.txt',status='unknown')
      do ii=1, nopt
		write(9,100) x_values(ii)  
100		FORMAT(f22.15)  ! your code invoked by app_name must be able to  ! CLASS EDIT
					   ! read this format properly 					   
	end do
	close(9)

! STEP 2. Call USER SUPPLIED application (batch or exe file) to run the model under new inputs:
!	app_name='Hart6_exe'  ! THIS WORKS !
	call system(app_name)	! must be in same directory as DDS1.exe 
	! the above call needs to:
	!	a) transfer decision variable values in variables_in.txt to user model (or application)
	!	   inputs	 
	!	b) execute the model or application & compute the USER DEFINED objective function
	!	c) write the value of the objective function to 'function_out.txt'
!	! All of the above mentioned files should be located in the same directory as DDS1.exe.

! below is CLASS EDIT: makes sure correct initial solutions are used from WATCLASS files:
	if (x_values(1).EQ.-999.9) then
		open(unit=9, file='variables_in.txt',status='unknown')
		do ii=1, nopt
			read(9,100) x_values(ii)  
		end do
	! below loop makes sure each optimization trial initialized from same CLASS.ini sol'n
		do ii=1, num_trials 
			initials(ii,:)=x_values
		enddo 
	close(9)

	endif

! STEP 3. Get the value of objective function for the DDS algorithm:
      open(unit=9, file='function_out.txt',status='unknown')
      read(9,100) fvalue
	close(9)


	return
      end

