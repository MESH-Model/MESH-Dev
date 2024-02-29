! HDS module call in MESH
! The main HDS algorithm can be found in HDS.f90
! change log:
! Oct 2023 -> initial implementation of the HDS standalone code into MESH
! Feb 2024 -> Migrating the code the new MESH version (1860)

! To be Done
! 1. Add gatekeeping
! 2. Activate fractions

! The control volume is the depression and all calculations within the HDS is in L^3/T
module HDS_module


    use HDS
	use type_HDS
	
	use mpi_module
    use model_files_variables
    use sa_mesh_common
    use model_dates
	
	
	!!!!=================================================================
	!!! Exaplanation of the overall HDS and fractions concept
	! As of now, no fracs implementation in MESH
	! frac_up2rvr: fraction of the basin contributing directly to the river (basin area - small_dep_basin_area - big_dep_basin_area)
	
	!! Assumptions
	!! Frac_up2big  < 1.0
	!! Frac_sml2rvr < 1.0
	!! 
	!!               ┌───────────────┐
	!!               │ Upland Runoff │
	!!               │(grid/subbasin)│
	!!               └───────┬───────┘
	!!                       │
	!!                       ├───────────────────┬──────┐
	!!     1.0 - frac_up2big │                   │      │
	!!          - frac_up2rvr│                   │      │
	!!                 ┌─────▼─────┐             │      │
	!!                 │   Small   │             │      │
	!!                 │depressions│             │      │
	!!                 └─────┬─────┘             │      │
	!!                       │outflow(sd)        │      │
	!!   ┌───────────────────┤                   │      │
	!!   │                   │                   │      │
	!!   │ 1.0 - frac_sml2rvr│                   │      │
	!!   │             ┌─────▼────┐ frac_up2big  │      │
	!!   │             │    big   ◄──────────────┘      │
	!!   │             │depression│                     │
	!!   │             └─────┬────┘                     │
	!!   │                   │                          │
	!!   │                   │                          │
	!!   │                   │                          │
	!!   │                   │                          │
	!!   │ frac_sml2rvr  ┌───▼───┐  frac_up2rvr         │
	!!   └───────────────► River ◄──────────────────────┘
	!!                   │ Outlet│
	!!                   └───────┘
	!!!!=================================================================
	
    ! arguments for HDS (inputs, diagnostic variables, outputs)
	! global variables
	real(rkind), allocatable    :: catchmentArea(:,:)   	! catchment area of the depression [m^2], small depressions (index=1), big depression (index=2)
    real(rkind), allocatable    :: depressionArea(:,:)  	! depression area [m^2]
    real(rkind), allocatable    :: depressionVol(:,:)   	! depression volume [m^3]
	real(rkind), allocatable    :: p(:)   	          		! shape of the slope profile [-]
    real(rkind), allocatable    :: b(:)               		! shape of the fractional contributing area curve [-]
    real(rkind), allocatable    :: vMin(:)            		! minimum pond volume (for small depressions) below which contributing area is zero [m3]
    real(rkind), allocatable    :: conArea(:)         		! contributing area fraction per subbasin [-]
    real(rkind), allocatable    :: pondVol(:,:)         	! pond volume [m3]
    real(rkind), allocatable    :: pondArea(:,:)        	! pond area [m2] 
	real(rkind), allocatable 	:: runoff_fracs (:,:) 		! runoff fractions [-]: frac_up2big (index=1), frac_up2rvr (index=2), and frac_sml2rvr (index=3)

  	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!MESH variables
	type HDS_control_variables
        logical :: PROCESS_ACTIVE = .false.
    end type

    type(HDS_control_variables) HDS_MESH

    save

    private

    public HDS_init, HDS_within_grid, HDS_MESH

    contains
	
    subroutine HDS_init(fls, shd)

		! subroutine to initialize HDS module and its inputs

        type(fl_ids), intent(in) 			:: fls
        type(ShedGridParams), intent(in) 	:: shd 				! MESH grid/subbasin properties
        integer 							:: nmesh_grid 		! number of active grids/subbasins within the domain
		character(len=100)					:: fnam				! HDS output file name
		!internal vairables
		integer 							:: n, k				! grid and gru index
		! real 								:: sum_fracs		! sum of runoff fracs

        !> Return if the process is not active.
        if (.not. HDS_MESH%PROCESS_ACTIVE) then
            return
        else
            !> Print a message to screen noting HDS is active.
            call print_message('HDS IS ACTIVE.')
        end if

		! get the number of active grids/subbasins
		nmesh_grid=shd%NA

		! allocate HDS arrays
        allocate(depressionVol(nmesh_grid,2), catchmentArea(nmesh_grid,2),  p(nmesh_grid), b(nmesh_grid))! allocate variables for grids, area_mult(nmesh_grid),
		allocate(depressionArea(nmesh_grid, 2), pondArea(nmesh_grid, 2))
		allocate(runoff_fracs(nmesh_grid,3))! allocate variables for grids
		allocate(pondVol(nmesh_grid,2),conArea(nmesh_grid))! allocate variables for grids
		allocate(vMin(nmesh_grid))
        

		
		!loop by grid to get average grid canopy fraction to be used for PET calculations
		do n = i1, i2
			!loop by tile to average canopy fractions.
			pm%grid%fcan(n, :) = 0.0 !initialize average fcan values (it is zero from the original code)
			!pm%grid%zrfm(n) = 40.0 !Assume it as 40 because it is commonly used
			do k = il1, il2
				if (shd%lc%ILMOS(k) == n) then !if grid number matches n
					pm%grid%fcan(n, 1) = pm%grid%fcan(n, 1) + (pm%tile%fcan(k, 1)* shd%lc%ACLASS(n,k)) ! needleleaf fraction for the current tile * tile fraction per grid cell
					pm%grid%fcan(n, 2) = pm%grid%fcan(n, 2) + (pm%tile%fcan(k, 2)* shd%lc%ACLASS(n,k)) ! broadleaf fraction for the current tile * tile fraction per grid cell
					pm%grid%fcan(n, 3) = pm%grid%fcan(n, 3) + (pm%tile%fcan(k, 3)* shd%lc%ACLASS(n,k)) ! crop fraction for the current tile * tile fraction per grid cell
					pm%grid%fcan(n, 4) = pm%grid%fcan(n, 4) + (pm%tile%fcan(k, 4)* shd%lc%ACLASS(n,k)) ! grass fraction for the current tile * tile fraction per grid cell
					!That would cause troubles as the fluxes are estimated at certain depths
					!pm%grid%zrfm(n) = pm%grid%zrfm(n) + (pm%tile%zrfm(k) * shd%lc%ACLASS(n,k))
					pm%grid%zrfm(n) = pm%tile%zrfm(k) !just pick the zrfm from the last GRU id within the grid cell
				end if
			end do
		end do	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                      READ HDS model inputs                                 !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

		!define model parameters
		open(789, file='HDS_parameters.csv', status='old')
		read(789,*) !skip the header line
		! loop by grid to read grid values
		do n = i1, i2
						!MESH_grid_no    small_depressionVol       small_depressionArea small_catArea
			read(789,*) k, depressionVol(n,1), depressionArea(n,1), catchmentArea(n,1), &
						! big_depressionVol   big_depressionArea	big_catArea	
						 depressionVol(n,2), depressionArea(n,2), catchmentArea(n,2), &
						!  p, b, &
						 p(n), b(n), & 
						! frac_up2big, 		frac_sml2rvr			
						runoff_fracs(n,1), runoff_fracs(n,3)
			! calculate frac_up2rvr						
			runoff_fracs(n,2) = max((shd%DA(n) * 1e6) - catchmentArea(n,1) - catchmentArea(n,2), zero)/(shd%DA(n) * 1e6)
			!write(*,*) k, depressionVol(n,1), catchmentArea(n,1), depressionVol(n,2), catchmentArea(n,2), area_mult(n), p(n), &
			!		runoff_fracs(n,1), runoff_fracs(n,2), runoff_fracs(n,3)
			!read(*,*) k
			! check input parameters
			! set small/big ponds parameters to zero if zero or neg values detected in the inputs
			if (depressionVol(n,1) <= 0.0 .or. catchmentArea(n,1) <= 0.0) then
				!small depressions are not active
				depressionVol(n,1) = 0.0 
				catchmentArea(n,1) = 0.0
				pondVol(n,1) = 0.0
				pondArea(n,1) = zero
				conArea(n) = 1.0
			end if
			!Big pond check
			if (depressionVol(n,2) <= 0.0 .or. catchmentArea(n,2) <= 0.0) then
				!big depression is not active
				depressionVol(n,2) = 0.0 
				catchmentArea(n,2) = 0.0
				pondArea(n,2) = zero
			end if

			! fracs check
			! check if one component is active (small or big)
			if(depressionVol(n,1) <= 0.0 .or. depressionVol(n,2) <= 0)then
				!deactivate frac_up2big and frac_sml2rvr as they don't influence runoff
				!frac_up2big = frac_sml2rvr = 0.0
				runoff_fracs(n,1) = 0.0
				runoff_fracs(n,3) = 0.0
			end if

			! !! Frac_up2big + Frac_up2rvr < 1.0
			! if((runoff_fracs(n,1) + runoff_fracs(n,2)) > 1.0) then
			! 	! rescale the parameters based on their weight to make them equal 1
			! 	sum_fracs = runoff_fracs(n,1) + runoff_fracs(n,2)
			! 	runoff_fracs(n,1) = (runoff_fracs(n,1)/sum_fracs) * 1.0 !*1.0 because the sum needs to = 1
			! 	runoff_fracs(n,2) = (runoff_fracs(n,2)/sum_fracs) * 1.0
			! end if
			!! Frac_sml2rvr < 1.0
			runoff_fracs(n,1) = min(runoff_fracs(n,1), 1.0)
			runoff_fracs(n,3) = min(runoff_fracs(n,3), 1.0)

		end do

		!!initialize variables
		vMin(:) = zero       ! time varying model parameter, will be updated later

		! initialize state variables only if modules are active
		do n = i1, i2
			! small depressions
			if (depressionVol(n,1) .ne. 0.0 .and. catchmentArea(n,1) .ne. 0.0) then
				pondVol(n,1) = vs%grid%zpnd(n) * depressionArea(n,1) !m -> m^3 ! approximate vol calculation, will be updated later in the subroutine
				pondArea(n,1) = depressionArea(n,1)*((pondVol(n,1)/depressionVol(n,1))**(two/(p(n) + two)))
				conArea(n) = pondVol(n,1)/depressionVol(n,1) ! assume that contrib_frac = vol_frac_sml for initialization purposes
				vMin(n) = pondVol(n,1)
			end if
			!big depression
			if (depressionVol(n,2) .ne. 0.0 .and. catchmentArea(n,2) .ne. 0.0) then
				pondVol(n,2) = vs%grid%zpnd(n) * depressionArea(n,2) !m -> m^3 ! approximate vol calculation, will be updated later in the subroutine
				pondArea(n,2) = depressionArea(n,2)*((pondVol(n,2)/depressionVol(n,2))**(two/(p(n) + two)))
			end if

		end do
	
	!***********************
		!outputs (dummy for offline run only)
		if (ISHEADNODE) then
			! create the output file
			fnam= './' // trim(fls%GENDIR_OUT) // '/' // 'HDS_balance.csv' !to write HDS_blanace.csv in the results folder
			write(*,*)fnam
			open(99099,file=fnam,status='unknown')
			write(99099,1110)'year','day','hour','mins','grid_no','precip','pot_evap','runoff_depth','pondVol_sml','vol_frac_sml','pondArea_sml','ConArea','smlpond_outflow', &
								'pondVol_big','vol_frac_big','pondArea_big','bigpond_outflow','total_outflow'
				
			1110    format(9999(g15.7e2, ','))

		end if

    !***********************

    end subroutine
	
	
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


	subroutine HDS_within_grid(fls, shd)

		implicit none
		
		! HDS module within the grid cell 
		! subroutine arguments 
		type(fl_ids), intent(in) 			:: fls
        type(ShedGridParams) 				:: shd								! MESH grid/subbasin properties
		! local variables
		integer 							:: n, k								! grid and gru index
		integer 							:: year,day,hour,mins				! time variables: year, day, hour, minute
		real(rkind)							:: runoff_depth						! surface runoff and interflow from all soil layers [mm] (input to HDS)
		real(rkind), parameter    			:: dt  = 1.0_rkind          		! time step =1 [-] (no substep in the HDS calculation as MESH runs every 30 min so both I/O are at 30 min)
		real(rkind), parameter    			:: tau = zero 						! time constant linear reservoir to account for infiltration losses (currently deactivated)
		real(rkind)							:: precip, pot_evap					! precipitation and potential evaporation depths [mm]
		real(rkind)				 			:: rofo_grid			 			! overland runoff for the grid [mm]
		real(rkind), allocatable 			:: rofs_grid(:)						! interflow runoff for the grid [mm]
		real(rkind) 						:: total_outflow 					! total outflow [mm] from HDS component (small depressions+big depression+fractions)
		real(rkind) 						:: smallpond_outflow 				! outflow from small depressions [m3] ! hysteretic component
		real(rkind) 						:: bigpond_outflow 					! outflow from big depression [m3] ! gatekeeping component
		real(rkind) 						:: drain_area 						! total drainage area of the gridcell [m2]
		real(rkind), external 				:: calc_ET0							! Penman-Monteith function for PET calculations
		real(rkind) 						:: upslopeArea_small 				! upland area contributing to the small depressions [m2]
		real(rkind) 						:: upslopeArea_big 					! upland area contributing to the big deperssion [m2]
		real(rkind) 						:: vol_frac_small, vol_frac_big		! volume fraction for the small and big depressions [-]
		real(rkind) 						:: Q_det_adj, Q_dix_adj     		! adjusted evapotranspiration & infiltration fluxes [L3 T-1] for mass balance closure (i.e., when losses > pondVol). Zero values mean no adjustment needed.
		real(rkind)							:: smallpond_evap, bigpond_evap, total_evap ! evaporation volume from small and big ponds, and the entire basin
		integer								:: nsoillayer, isoillayer			! number of soil layers in MESH & index of soil layer
		
		!> Return if the process is not active.
        if (.not. HDS_MESH%PROCESS_ACTIVE) return
		! get current time
		year=ic%now%year
        day=ic%now%jday
        hour=ic%now%hour
        mins=ic%now%mins
		
		!loop by grid/subbasin
		do n = i1, i2

			!check to see if this cell/subbasin worth calculating (has HDS module active)
			if (depressionVol(n,1) .eq. 0.0 .and. depressionVol(n,2) .eq. 0.0) cycle !skip and iterate over next grid (n)
			
			! Initialize fluxes
			runoff_depth = zero
			rofo_grid = zero
			pot_evap = zero
			precip = zero
			total_outflow = zero
			!calculate upslope (upland area)
			upslopeArea_small = max(catchmentArea(n,1) - depressionArea(n,1), zero)
			upslopeArea_big = max(catchmentArea(n,2) - depressionArea(n,2), zero)
			!get drainage area
			drain_area = shd%DA(n) * 1e6 ! km2 -> m2
			pot_evap = calc_ET0( &
                   vs%grid%ta(n), vs%grid%uv(n), vs%grid%qa(n), vs%grid%pres(n), vs%grid%fsin(n), & 
                   shd%ylat(n), shd%xlng(n), shd%ELEV(n), &
                   pm%grid%zrfm(n), pm%grid%fcan(n, 1), pm%grid%fcan(n, 2), pm%grid%fcan(n, 3), pm%grid%fcan(n, 4), & 
                   ic%now%jday, ic%now%hour)*ic%dts !potential evap (penman) mm/s to mm from Penman-Monteith applied every time step
			
			! calculate inputs to the small & big depressions
			! overland runoff
			
			rofo_grid = vs%grid%ovrflw(n) * ic%dts !mm/s -> mm
			! interflow runoff from all layers within the soil column
			nsoillayer = size(vs%grid%latflw(n,:))
			allocate(rofs_grid(nsoillayer))
			rofs_grid = zero

			do isoillayer = 1, nsoillayer
				rofs_grid(isoillayer) = vs%grid%latflw(n, isoillayer) * ic%dts !mm/s -> mm !sum(vs%grid%rofs(n, :)) * ic%dts !mm/s -> mm
			end do
			
			!Total grid runoff 	! runoff_depth:  !surface runoff from the upland & depression (are tha's not wet)land area, q_usx [LT−1]
			runoff_depth =  rofo_grid + sum(rofs_grid) ! mm !surface runoff and interflow from all soil layers
			
			! precipitation depth
			precip = vs%grid%pre(n) * ic%dts !mm/s -> mm !vs%grid%prec(n) for newer versions of MESH

			!Initialize variables from pervious timestep
			!volume fraction
			vol_frac_small = zero
			vol_frac_big = zero
			if(depressionVol(n,1)>zero) vol_frac_small = pondVol(n,1) / depressionVol(n,1)
			if(depressionVol(n,2)>zero) vol_frac_big = pondVol(n,2) / depressionVol(n,2)
			!initialize outputs
			! area_frac_sml = 1.0 !updated inside the small_depressoins routine
			! area_frac_big = 1.0 !updated inside the big_depressoin routine
			smallpond_outflow = zero
			bigpond_outflow = zero
			! outflow volume from area with no depressions
			total_outflow = (runoff_depth/1000.0) * runoff_fracs(n,2) * drain_area ! mm -> m3
			!**************************************
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!!!		Run HDS on the grid scale	!!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!write(*,*)'calling HDS'
			!write(*,*)'current_pondVol_small',current_pondVol_small
			! run small depressions routine (hysteretic component) if active
			if (depressionVol(n,1) .ne. 0.0) then
				! call calculate_small_depressions_outflow(current_pondVol_small, runoff_depth, delta_depth, contrib_frac, &
				! 										depressionVol(n,1), catchmentArea(n,1), catchmentArea(n,2), &
				! 										upland_area_frac, p(n), vol_frac_sml, area_frac_sml, &
				! 										runoff_fracs(n,1), runoff_fracs(n,2), &
				! 										depth_sml, smallpond_outflow)

				call runDepression(pondVol(n,1), runoff_depth, precip, pot_evap, depressionArea(n,1), depressionVol(n,1), upslopeArea_small, &
                                	p(n), tau, b(n), vMin(n), dt, Q_det_adj, Q_dix_adj, vol_frac_small, conArea(n), pondArea(n,1), smallpond_outflow)
				
				! if(Q_det_adj>zero .or. Q_dix_adj>zero)then
					! assign the new evaporation and infiltration volumes as the Q_det_adj, Q_dix_adj in the LSM
					! to ensure mass balance closure
				! end if
				!add the fracs later
			end if										 
			
			total_outflow = total_outflow + smallpond_outflow ! m3
			! write(*,*)drain_area
			
			! ! run big depression routine (gatekeeping component)
			! if (depressionVol(n,2) .ne. 0.0) then
			! 	! if small depressions are inactive, assume smallpond_outflow as the runoff from upland area
			! 	! runoff_fracs(n,1) = 0 in this case, but is kept for consistency
			! 	if(depressionVol(n,1) .eq. 0.0) smallpond_outflow = (runoff_depth * upland_area_frac * (1.0 - runoff_fracs(n,1) - runoff_fracs(n,2))) !runoff from upland

			! 	call calculate_big_depression_outflow(current_pondVol_big, runoff_depth, delta_depth, smallpond_outflow, &
			! 										depressionVol(n,2), catchmentArea(n,2), &
			! 										p(n), vol_frac_big, area_frac_big, upland_area_frac, &
			! 										runoff_fracs(n,1), runoff_fracs(n,2), runoff_fracs(n,3), &
			! 										depth_big, bigpond_outflow, total_outflow)
			! end if
			! !assume total outflow as small depressions outflow if big depression is not active
			! if (depressionVol(n,2) .eq. 0.0) total_outflow = smallpond_outflow + &
			! 											 (runoff_depth * upland_area_frac * runoff_fracs(n,2))
			!1111111111111111111111111111111111111111111111111111111111111111111
			!1111111111111111111111111111111111111111111111111111111111111111111

			!override runoff and evap values of the grid 
			!!!!!!The model crashes here, sometime it runs and someother times it doesn't
			! This happens in debug mode, but the reguar compilation completes with no problem
			! redistribute total_outflow between ROFO and ROFS using their weights if runoff_depth>0
			if(runoff_depth>zero)then
				vs%grid%ovrflw(n) = (((total_outflow*(rofo_grid/runoff_depth))/drain_area)*1000.0) / ic%dts !m3 to mm/s ! to convert to grid average (to match MESH output)
				do isoillayer = 1, nsoillayer
					vs%grid%latflw(n,isoillayer) = (((total_outflow*(rofs_grid(isoillayer)/runoff_depth))/drain_area)*1000.0) / ic%dts !m3 to mm/s ! to convert to grid average (to match MESH output)
				end do
			else
				! assign total_outflow as rofo (this can happen when there's a precip, but no surface runoff)
				vs%grid%ovrflw(n) = ((total_outflow/drain_area)*1000.0) / ic%dts !m3 to mm/s ! to convert to grid average (to match MESH output)
			end if
			deallocate(rofs_grid)

			! override Evap values to include PET from wet areas
			! small pond evaporation volume
			smallpond_evap = (pot_evap/1000.0) * pondArea(n,1) !mm -> m3
			if(Q_det_adj > zero) smallpond_evap = Q_det_adj !m3
			! big pond evaporation volume
			bigpond_evap = (pot_evap/1000.0) * pondArea(n,2) ! mm-> m3
			! do the same Q_det_adj, but for big pond

			!grid evaporation
			total_evap = (vs%grid%et(n)* (ic%dts/1000.0) *  (drain_area - pondArea(n,1) - pondArea(n,2))) + & !mm/s -> m3 grid evap from unwet area
							smallpond_evap + bigpond_evap ! POT evap from ponds
			
			vs%grid%et(n) = ((total_evap/drain_area)*1000.0) / ic%dts !m3 to mm/s ! to convert to grid average (to match MESH output)

			if (ISHEADNODE) then

			write(99099,1110)year,day,hour,mins,n,precip,pot_evap,runoff_depth,pondVol(n,1),vol_frac_small,pondArea(n,1),ConArea(n),smallpond_outflow, &
							pondVol(n,2),vol_frac_big,pondArea(n,2), bigpond_outflow, total_outflow
							
								
			1110    format(9999(g15.7e2, ','))
			
			!write(*,1110)year,day,hour,mins,n,delta_depth,runoff_depth,depth,vol_frac_sml,water_area_frac,contrib_frac,smallpond_outflow
			
			end if
		
		end do
		
		!write(*,*)'finished'
		
	end subroutine
	
end module
