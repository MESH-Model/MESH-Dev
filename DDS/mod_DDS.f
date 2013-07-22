! Input module for the DDS algorithm 
! Coded by Bryan Tolson, Nov 2005.  

! See DDS_inp.f for all DDS input variable definitions not defined here.
! Also see DDS_main.f for other variable definitions.
! All arrays are allocated in DDS_allocate.f called from main_DDS.f


	MODULE MOD_DDS

! DDS user input values
	Integer out_print,user_seed
	Integer(8) num_trials, maxiter
	character*24 runname, ini_name, app_name
	character(len=100) user_comments 
	character(len=6), dimension (:), allocatable :: DVnames
	character(100) out_dirname !Added by AM, to move stream files

	real(8) r_val
	integer num_dec
      real(8), dimension (:), allocatable :: s_min, s_max, ini_soln 
      real(8), dimension (:,:), allocatable :: initials

!	DDS Output declarations
      real(8), dimension (:), allocatable :: Ftests,Fbests,sbest,stest 
      real(8), dimension (:,:), allocatable :: stests
	real(8) Fbest,to_max
	integer, dimension (:), allocatable :: f_count

!	Multiple DDS optimization trial outputs
      real(8), dimension (:,:), allocatable :: all_Fbests
      real(8), dimension (:,:), allocatable :: all_sbest
	
	END MODULE MOD_DDS