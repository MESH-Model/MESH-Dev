      subroutine neigh_value(x_cur,x_min,x_max,r,new_value)

!	Purpose is to generate a neighboring decision variable value for a single
!	decision variable value being perturbed by the DDS optimization algorithm.
!	New DV value respects the upper and lower DV bounds.
!	Coded by Bryan Tolson, Nov 2005.
 
! I/O variable definitions:
!	x_cur - current decision variable (DV) value
!	x_min - min DV value
!	x_max - max DV value
!	r  - the neighborhood perturbation factor
!	new_value - new DV variable value (within specified min and max)

	implicit none

	REAL(8) ranval,zvalue,x_cur,x_min,x_max,r,new_value,x_range
	integer i, j, k
	REAL(8) Work3, Work2, Work1 !	 for Box-Muller transform	
	
	x_range=x_max-x_min

	! perturb current value with normal random variable
!	CALL DRNNOA(1,zvalue)  ! generates a standard normal random deviate
						   ! ISML Stat Library 2 routine - Acceptance/rejection

!	returns a standard Gaussian random number based upon Numerical recipes gasdev and 
!	Marsagalia-Bray Algorithm
	Work3=2.0 
	Do While (Work3.ge.1.0.or.Work3.eq.0.0)
		call random_number(ranval) ! get one uniform random number
		Work1 = 2.0 * DBLE(ranval) - 1.0
		call random_number(ranval) ! get one uniform random number
		Work2 = 2.0 * DBLE(ranval) - 1.0
		Work3 = Work1 * Work1 + Work2 * Work2
	enddo  
	Work3 = ((-2.0 * dLog(Work3)) / Work3)**0.5  ! natural log
! pick one of two deviates at random: (don't worry about trying to use both)
	call random_number(ranval) ! get one uniform random number
	IF (ranval.LT.0.5) THEN
		zvalue = Work1 * Work3
	else
		zvalue = Work2 * Work3
	endif

				   	
	new_value=x_cur+zvalue*r*x_range

! need if statements to check within DV bounds.  If not, bounds are reflecting.
	if (new_value.LT.x_min) then
		new_value=x_min+(x_min-new_value)
		! if reflection goes past x_max then value should be x_min since 
		! without reflection the approach goes way past lower bound.  
		! This keeps x close to lower bound when x_cur is close to lower bound
		if (new_value.GT.x_max) then
			new_value=x_min
		endif
	elseif (new_value.GT.x_max) then
		new_value=x_max-(new_value-x_max)
		! if reflection goes past x_min then value should be x_max for same reasons as above
		if (new_value.LT.x_min) then
			new_value=x_max
	    endif
	endif

	return
	END

