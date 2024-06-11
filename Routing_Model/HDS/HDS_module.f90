! HDS module call in MESH
! The main HDS algorithm can be found in HDS.f90
! change log:
! Oct 2023 -> initial implementation of the HDS standalone code into MESH
! Feb 2024 -> Migrating the code the new MESH version (1860)
! May 2024 -> code clean up for ECCC

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
	!! 
	!!               ┌───────────────┐
	!!               │ Upland Runoff │
	!!               │(grid/subbasin)│
	!!               └───────┬───────┘
	!!                       │
	!!                       ├──────────────────────────┐
	!!     depCatchAreaFrac  │                          │
	!!                       │                          │
	!!                 ┌─────▼─────┐                    │
	!!                 │   Small   │                    │
	!!                 │depressions│                    │
	!!                 └─────┬─────┘                    │
	!!                       │outflow(sd)               │
	!!                       │                          │
	!!                       │                          │
	!!                       │                          │
	!!                   ┌───▼───┐  1-depCatchAreaFrac  │
	!!                   | River ◄──────────────────────┘
	!!                   │ Outlet│
	!!                   └───────┘
	!!!!=================================================================
	
    ! arguments for HDS (inputs, diagnostic variables, outputs)
	! global variables
	real(rkind), allocatable    :: depCatchAreaFrac(:)      ! catchment area (fraction of the land area = basin area-depression area) that drains to the depressions [-]
    real(rkind), allocatable    :: depressionArea(:)  		! depression area [m^2]
    real(rkind), allocatable    :: depressionVol(:)   		! depression volume [m^3]
	real(rkind), allocatable    :: p(:)   	          		! shape of the slope profile [-]
    real(rkind), allocatable    :: b(:)               		! shape of the fractional contributing area curve [-]
    real(rkind), allocatable    :: vMin(:)            		! minimum pond volume (for small depressions) below which contributing area is zero [m3]
    real(rkind), allocatable    :: conArea(:)         		! contributing area fraction per subbasin [-]
    real(rkind), allocatable    :: pondVol(:)         		! pond volume [m3]
    real(rkind), allocatable    :: pondArea(:)        		! pond area [m2] 
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

        !> Return if the process is not active.
        if (.not. HDS_MESH%PROCESS_ACTIVE) then
            return
        else
            !> Print a message to screen noting HDS is active.
            call print_message('HDS IS ACTIVE.')
        end if

		! get the number of active grids/subbasins
		nmesh_grid=shd%NA-1

		! allocate HDS arrays (parameters, variables, or states for grids)
        allocate(depressionVol(nmesh_grid), depCatchAreaFrac(nmesh_grid),  p(nmesh_grid), b(nmesh_grid))
		allocate(depressionArea(nmesh_grid), pondArea(nmesh_grid))
		allocate(pondVol(nmesh_grid),conArea(nmesh_grid))
		allocate(vMin(nmesh_grid))
        

		
		!loop by grid to get average grid canopy fraction to be used for PET calculations
		do n = i1, i2
			!loop by tile to average canopy fractions.
			pm%grid%fcan(n, :) = 0.0 !initialize average fcan values (it is zero from the original code)
			do k = il1, il2
				if (shd%lc%ILMOS(k) == n) then !if grid number matches n
					pm%grid%fcan(n, 1) = pm%grid%fcan(n, 1) + (pm%tile%fcan(k, 1)* shd%lc%ACLASS(n,k)) ! needleleaf fraction for the current tile * tile fraction per grid cell
					pm%grid%fcan(n, 2) = pm%grid%fcan(n, 2) + (pm%tile%fcan(k, 2)* shd%lc%ACLASS(n,k)) ! broadleaf fraction for the current tile * tile fraction per grid cell
					pm%grid%fcan(n, 3) = pm%grid%fcan(n, 3) + (pm%tile%fcan(k, 3)* shd%lc%ACLASS(n,k)) ! crop fraction for the current tile * tile fraction per grid cell
					pm%grid%fcan(n, 4) = pm%grid%fcan(n, 4) + (pm%tile%fcan(k, 4)* shd%lc%ACLASS(n,k)) ! grass fraction for the current tile * tile fraction per grid cell
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
						!MESH_grid_no        depressionVol       depCatchAreaFrac
			read(789,*) k, depressionVol(n), depressionArea(n), depCatchAreaFrac(n), &
						!  p, b, &
						 p(n), b(n)

			! Note: depressionArea is being read as a fraction and below it is transformed to actual area
			! This was done to facilitate calibrating the parameters to commonly used ones by modellers
			! calculate depression Area
			depressionArea(n) =  depressionArea(n) * shd%DA(n) * 1e6 ! km2 -> m2
			! Note: depressionVol is being read as depth (m) and below it is transformed to actual volume
			depressionVol(n) = depressionVol(n) * depressionArea(n) 
			
			! set HDS parameters to zero if zero or neg values detected in the inputs
			if (depressionVol(n) <= 0.0 .or. depCatchAreaFrac(n) <= 0.0) then
				!HDS is not active
				depressionVol(n) = 0.0 
				depCatchAreaFrac(n) = 0.0
				pondVol(n) = 0.0
				pondArea(n) = 0.0
				conArea(n) = 1.0
			end if
		end do

		!!initialize variables
		vMin(:) = zero       ! time varying model parameter, will be updated later

		! initialize state variables only if modules are active
		! Below are simplified initialization. Actual values will be updated once HDS runs
		do n = i1, i2
			if (depressionVol(n) .ne. 0.0 .and. depCatchAreaFrac(n) .ne. 0.0) then
				pondVol(n) = vs%grid%zpnd(n) * depressionArea(n) !m -> m^3 ! approximate vol calculation, will be updated later in the subroutine
				pondArea(n) = depressionArea(n)*((pondVol(n)/depressionVol(n))**(two/(p(n) + two)))
				conArea(n) = pondVol(n)/depressionVol(n) ! assume that conArea = volFrac for initialization purposes
				vMin(n) = pondVol(n)
			end if
		end do
	
	!***********************
		!outputs (currently stored in a separate csv file)
		! This steo creates the csv file header
		if (ISHEADNODE) then
			! create the output file
			fnam= './' // trim(fls%GENDIR_OUT) // '/' // 'HDS_balance.csv' !to write HDS_blanace.csv in the results folder
			write(*,*)fnam
			open(99099,file=fnam,status='unknown')
			write(99099,1110)'year','day','hour','mins','grid_no','precip','pot_evap','runoff_depth','pondVol','volFrac','pondArea','ConArea','pondOutflow', &
								'totalOutflow', 'PET_CLASS'
				
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
		integer 							:: n, k								! grid and gru/tile index
		integer 							:: year,day,hour,mins				! time variables: year, day, hour, minute
		real(rkind)							:: runoff_depth						! surface runoff and interflow from all soil layers [mm] (input to HDS)
		real(rkind), parameter    			:: dt  = 1.0_rkind          		! time step =1 [-] (no substep in the HDS calculation as MESH runs every 30 min so both I/O are at 30 min)
		real(rkind), parameter    			:: tau = zero 						! time constant linear reservoir to account for infiltration losses (currently deactivated)
		real(rkind)							:: precip, pot_evap					! precipitation and potential evaporation depths [mm]
		real(rkind)							:: pot_class						! pot from class (not used in the subroutine)
		real(rkind)				 			:: rofo_grid			 			! overland runoff for the grid [mm]
		real(rkind), allocatable 			:: rofs_grid(:)						! interflow runoff for the grid for all soil layers [mm]
		real(rkind) 						:: total_outflow 					! total outflow [mm] from HDS component
		real(rkind) 						:: pond_outflow      				! outflow from the depressions [m3] ! hysteretic component
		real(rkind) 						:: drain_area 						! total drainage area of the gridcell [m2]
		real(rkind) 						:: land_area 						! area of land (basin area - depressions Area)
		real(rkind), external 				:: calc_ET0							! Penman-Monteith function for PET calculations
		real(rkind) 						:: upslopeArea      				! upland area contributing to the small depressions [m2]
		real(rkind) 						:: volFrac                  		! volume fraction for the depressions [-]
		real(rkind) 						:: Q_det_adj, Q_dix_adj     		! adjusted evapotranspiration & infiltration fluxes [L3 T-1] for mass balance closure (i.e., when losses > pondVol). Zero values mean no adjustment needed.
		real(rkind)							:: pond_evap, total_evap            ! evaporation volume from ponds and the entire basin
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
			if (depressionVol(n) .eq. 0.0) cycle !skip and iterate over next grid (n)
			
			! Initialize fluxes
			runoff_depth = zero
			rofo_grid = zero
			pot_evap = zero
			precip = zero
			total_outflow = zero
			! calculate variables
			!get drainage and land area
			drain_area = shd%DA(n) * 1e6 ! km2 -> m2
			land_area = drain_area - depressionArea(n)
			!calculate upslope (upland area) that drains to depressions
			upslopeArea = max(land_area * depCatchAreaFrac(n), zero)

			! calculations of PET based on grid values (using penman monteith -- generates Inf)
			! pot_evap = calc_ET0( &
            !        vs%grid%ta(n), vs%grid%uv(n), vs%grid%qa(n), vs%grid%pres(n), vs%grid%fsin(n), & 
            !        shd%ylat(n), shd%xlng(n), shd%ELEV(n), &
            !        pm%grid%zrfm(n), pm%grid%fcan(n, 1), pm%grid%fcan(n, 2), pm%grid%fcan(n, 3), pm%grid%fcan(n, 4), & 
            !        ic%now%jday, ic%now%hour)*ic%dts !potential evap (penman) mm/s to mm from Penman-Monteith applied every time step
			! pot_class = vs%grid%potevp(n) * ic%dts ! potential evap calculated by CLASS mm/s -> mm (not used)
			
			! estimate POT using Oudin's formula
			pot_evap = calcPotentialEvap_Oudin2005(vs%grid%fsin(n), vs%grid%ta(n)) * ic%dts ! potential evap calculated by Oudin's formula mm/s -> mm/timestep
			
			! calculate inputs to the depressions
			! overland runoff
			
			rofo_grid = vs%grid%ovrflw(n) * ic%dts !mm/s -> mm
			! interflow runoff from all layers within the soil column
			nsoillayer = size(vs%grid%latflw(n,:))
			allocate(rofs_grid(nsoillayer))
			rofs_grid = zero

			rofs_grid(:) = vs%grid%latflw(n, :) * ic%dts !mm/s -> mm !sum(vs%grid%rofs(n, :)) * ic%dts !mm/s -> mm

			
			!Total grid runoff
			runoff_depth =  rofo_grid + sum(rofs_grid) ! mm !surface runoff and interflow from all soil layers
			
			! precipitation depth
			precip = vs%grid%pre(n) * ic%dts !mm/s -> mm

			!Initialize variables from pervious timestep
			!volume fraction
			volFrac = zero
			if(depressionVol(n)>zero) volFrac = pondVol(n) / depressionVol(n)
			pond_outflow      = zero
			! add runoff from land area that drains directly to the river network
			total_outflow = (runoff_depth/1000.0) * land_area * (1.0-depCatchAreaFrac(n)) ! mm -> m3
			!**************************************
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			!!!		Run HDS on the grid scale	!!
			!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
			! run depressions routine
			if (depressionVol(n) .ne. 0.0) then

				call runDepression(pondVol(n), runoff_depth, precip, pot_evap, depressionArea(n), depressionVol(n), upslopeArea, &
                                	p(n), tau, b(n), vMin(n), dt, Q_det_adj, Q_dix_adj, volFrac, conArea(n), pondArea(n), pond_outflow)
			end if										 
			
			total_outflow = total_outflow + pond_outflow      ! m3
			
			!override runoff and evap values of the grid inside MESH
			! distribute the total_outflow between surface and interflow based on their original ratio
			if(runoff_depth>zero)then
				vs%grid%ovrflw(n) = (((total_outflow*(rofo_grid/runoff_depth))/drain_area)*1000.0) / ic%dts !m3 to mm/s ! convert to grid average (to match MESH output)
				do isoillayer = 1, nsoillayer
					vs%grid%latflw(n,isoillayer) = (((total_outflow*(rofs_grid(isoillayer)/runoff_depth))/drain_area)*1000.0) / ic%dts !m3 to mm/s ! to convert to grid average (to match MESH output)
				end do
			else
				! assign total_outflow as rofo (this can happen when there's a precip, but no surface runoff)
				vs%grid%ovrflw(n) = ((total_outflow/drain_area)*1000.0) / ic%dts !m3 to mm/s ! convert to grid average (to match MESH output)
			end if
			deallocate(rofs_grid)

			! override Evap values to include PET from wet areas
			! pond evaporation volume
			pond_evap = (pot_evap/1000.0) * pondArea(n) !mm -> m3
			if(Q_det_adj > zero) pond_evap = Q_det_adj !m3

			!override grid evaporation to account for pond evaporation
			total_evap = (vs%grid%et(n)* (ic%dts/1000.0) *  (drain_area - pondArea(n))) + & !mm/s -> m3 grid evap from unwet area
							pond_evap  ! POT evap from ponds
			
			vs%grid%et(n) = ((total_evap/drain_area)*1000.0) / ic%dts !m3 to mm/s ! convert to grid average (to match MESH output)

			if (ISHEADNODE) then
			
			! write output to file
			write(99099,1110)year,day,hour,mins,n,precip,pot_evap,runoff_depth,pondVol(n),volFrac,pondArea(n),ConArea(n),pond_outflow, &
							total_outflow, pot_class
							
								
			1110    format(9999(g15.7e2, ','))  
			
			end if
		end do
		
	end subroutine
	
end module
