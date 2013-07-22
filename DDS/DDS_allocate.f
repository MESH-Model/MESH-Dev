      subroutine DDS_allocate

!	variable allocations for DDS optimization algorithm 
!	Bryan Tolson, Nov, 2005

	USE MOD_DDS

	implicit none

! Allocate all DDS array variables:

	allocate(s_min(num_dec))
	allocate(s_max(num_dec))
	allocate(ini_soln(num_dec))
	allocate(sbest(num_dec))
	allocate(stest(num_dec))
	allocate(f_count(maxiter))
	allocate(stests(maxiter,num_dec))
	allocate(Ftests(maxiter))
	allocate(Fbests(maxiter))

	allocate(all_Fbests(num_trials,maxiter))
	allocate(all_sbest(num_trials,num_dec))
	allocate(initials(num_trials,num_dec))

	allocate(DVnames(num_dec))

	return
	END

