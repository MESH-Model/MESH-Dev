module PRIMA_module


    use PRIMA_inputs
    use main_PRIMA

    use mpi_module
    use model_files_variables
    use sa_mesh_common
    use model_dates

    implicit none


    character(len=100)::trash,foutflow
    real ::exc_water_evap !in mm
    !integer:: t
    real::time_start,time_end
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !PRIMA model inputs
    character(len=100):: fnam, dir,rep_fnam,fin_wd
    character(len=200)::ascii_data(6)
    integer::rain_flag,ncol,nrow,ntot,inactive_cells,active_cells,n_rvr_cell,ras_out
    integer:: j,ini_dep_flag,lin_indj
    real*8:: xllcorner,yllcorner
    !real ::outflow_pr !outflow volume from PRIMA (either m3 or cms based on the module used)
    real:: frac_wet_area, avg_zpnd_pr,max_dep,ovrlnd_rof_pr !in mm outputs of PRIMA
!    real:: avg_zpnd_pr_pre
    real :: adj_avg_zpnd
    integer,allocatable::ind_node(:,:),rvr_cell_ind(:)
    !real,allocatable::mann(:)!,vel(:),dt(:)
    integer, allocatable::rnk_ind(:),ind_sur(:,:),ind_no_data(:)
    real*8, allocatable:: dem_lin(:),wl_lin(:)!,wdepth(:)
    real:: time_step,no_data,cell_size,mann1
    !dummy inputs
	integer::dum_niter
!mesh    integer:: ndays,get_nlines
!mesh    real:: ZPNDCLSPRE
!mesh    real, allocatable:: CLSWAT(:,:),ZPNDCLS(:),ROFOCLS(:) !ponded and overland runoff produced by class
    real, allocatable:: ZPNDCLSPRE(:),ZPNDCLS(:)!,ROFOCLS(:) !ponded and overland runoff produced by class
    common /dims/ncol,nrow,ntot

!mesh    call cpu_time(time_start)

    save

    private

    public PRIMA_init, PRIMA_within_grid

    contains

    subroutine PRIMA_init(fls, shd)

        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd
        integer nmesh_grid
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                 READ MESH water Balance data                                 !!
    !! ZPND and ROFO (ponded depth and overland runoff)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        allocate(ZPNDCLSPRE(shd%NA),ZPNDCLS(shd%NA))!,ROFOCLS(shd%NA))
        nmesh_grid=shd%NA


        ZPNDCLS = 0.0
        ZPNDCLSPRE = vs%grid%zpnd*1000.0 !m to mm


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                      READ PRIMA model inputs                                 !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call read_PRIMA_inputs(ascii_data,rain_flag,ncol,nrow,ntot,inactive_cells,&
                              & active_cells,n_rvr_cell,xllcorner,yllcorner,ind_node,&
                              & rvr_cell_ind,mann1,rnk_ind,ind_sur,ind_no_data,dem_lin,wl_lin,time_step,no_data&
                              & ,cell_size,rep_fnam,dir,ini_dep_flag,ras_out,nmesh_grid,ZPNDCLSPRE)
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                  END READ PRIMA model inputs                                 !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !***********************
    !outputs (dummy for offline run only)
    if (ISHEADNODE) then
    dir='./' // trim(fls%GENDIR_OUT)

    ! fnam=trim(dir)// '/' // 'PRIMA_balance.csv'
    fnam= './' // trim(fls%GENDIR_OUT) // '/' // 'PRIMA_balance.csv' !to write PRIMA_blanace.csv in the results folder
    write(*,*)fnam
    open(99099,file=fnam,status='unknown')
    write(99099,1110)'year','day','hour','mins','grid_no','exc_water','POT_CLASS','POT_pnmn', 'norm_exwtr','fwet_area',&
                'avg_zpnd','adj_avg_zpnd','max_dep','ovrlnd_rof','adj_rofo'
    
     1110    format(9999(g15.7e2, ','))

    end if

    !***********************

    end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine prima_within_grid(fls, shd)

        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        integer n, k!, nwet
        real adj_rofo, norm_exwtr, POT_CLASS, POT_pnmn, &
            error_thr,errord_thr,vol_thr
        real ::outflow_pr !outflow volume from PRIMA (either m3 or cms based on the module used)
        integer niter,freq_err,method
        real, external :: calc_ET0
        integer:: year,day,hour,mins
        real mann(ntot)
        
        !PRIMA inputs (constant)
        error_thr=1/1000.0 !depth change tolerance mm to m (when using add module)
        errord_thr=1/1000.0 !depth change tolerance  mm to m (when using add_drain module, common case)
        vol_thr=1 !volume change tolerance  m3
        niter = 100000 !max number of iterations
        freq_err = 1000 !frequency to calculate error (every 1000 (freq_err) iteration)
        mann=mann1
        method=4 !add and drain water at the same time
        !!!!!!!!!
        !initialize variables
        outflow_pr=0
        adj_rofo=0
        avg_zpnd_pr=0
		ovrlnd_rof_pr=0
        frac_wet_area=0
        avg_zpnd_pr=0
        max_dep=0
        ovrlnd_rof_pr=0
        adj_avg_zpnd = 0.0

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !v05.0
        vs%grid%zpnd(i1:i2) = vs%grid%zpnd(i1:i2) + (vs%grid%rofo(i1:i2)+sum(vs%grid%rofs(i1:i2, :), 2))*ic%dts/1000.0 !mm/s to m
        vs%grid%pndw(i1:i2) = vs%grid%pndw(i1:i2) + (vs%grid%rofo(i1:i2)+sum(vs%grid%rofs(i1:i2, :), 2))*ic%dts !mm/s to mm
        vs%grid%rofo(i1:i2) = 0.0
        vs%grid%rofs(i1:i2, :) = 0.0
        

		do k = il1, il2
            if (vs%grid%zpnd(shd%lc%ILMOS(k)) > 0.0 .and. vs%tile%zpnd(k) == 0.0) then
                vs%tile%tpnd(k) = 273.16
            end if
            vs%tile%zpnd(k) = vs%grid%zpnd(shd%lc%ILMOS(k)) !ILMOS is grid number; !JLMOS is the GRU number; ACLASS is GRU fraction first index is the grid and the second is the GRU
        end do


        year=ic%now%year
        day=ic%now%jday
        hour=ic%now%hour
        mins=ic%now%mins

        do n = i1, i2
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!!!!! RUNING PRIMA model in a daily, hourly, or sub-hourly time step     !!!!!!!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !! Note that exc_water_evap is the amount of water added to or removed from the old storage
        !! i.e., the difference between current storage and old storage
        !!example (adding water): if the ponded water was 10 mm, MESH calculated excess rainfall/snowmelt and was found to be 5mm,
        !! then, exc_water_evap equals 5 mm. the model will add 5 mm to the existing 10mm ponded water continue redistributing and qunatifying
        !! the amount of storage and surface runoff.
        !! example (removing water, evaporating): the old storage is 10mm, this was sent to MESH and the new storage became 7mm
        !! then, exc_water_evap= -3 mm, which means that 3 mm of water are going to evaporate.
        !add/subtract water


		ZPNDCLS(n) = vs%grid%zpnd(n)*1000.0 !m to mm
        !calculate excess water depth
        exc_water_evap=(ZPNDCLS(n)-ZPNDCLSPRE(n))!+ROFOCLS(n) !calculate the gains and losses of ponded depth + RORO(added water)
        !potential evaporation calculated by CLASS
        POT_CLASS = -1*vs%grid%pevp(n)*ic%dts !potential evap mm from calss
        !potential evaporation calculated by Penman-Monteith
        POT_pnmn= -1*calc_ET0( &
                    vs%grid%ta(n), vs%grid%uv(n), vs%grid%qa(n), vs%grid%pres(n), vs%grid%fsin(n), & 
                    shd%ylat(n), shd%xlng(n), shd%ELEV(n), &
                    pm%grid%zrfm(n), &
                    pm%grid%fcan(n, 1), pm%grid%fcan(n, 2), pm%grid%fcan(n, 3), pm%grid%fcan(n, 4), &
                    ic%now%jday, ic%now%hour)*ic%dts !potential evap (penman)mm/s to mm from Penman-Monteith applied every hour
        
        
        !if there is no gain or losses, subtract POT from the ponded water
        ! if(exc_water_evap .eq. 0.0) exc_water_evap = POT_CLASS !potential evap mm from CLASS
        if(exc_water_evap .eq. 0.0) exc_water_evap = POT_pnmn !potential evap mm from Penman-Monteith
        
    
		!write(*,*)'start PRIMA'
		if ((rain_flag == 1 .and. ic%now%day /= ic%next%day) &
            .or. (rain_flag == 2 .and. ic%now%hour /= ic%next%hour) &
            .or. rain_flag == 3) then !run in a daily or an hourly timestep (hourly is not working)
			!run actual PRIMA at the end of the day/hour
			!write(*,*)'******run real PRIMA*******'
            !!niter=1 !dummy test
			call Run_PRIMA(exc_water_evap,norm_exwtr,ascii_data,rain_flag,inactive_cells,active_cells,year,day,hour,mins,n_rvr_cell,niter,&
							& error_thr,cell_size,no_data,time_step,errord_thr,vol_thr,outflow_pr,ind_node,rvr_cell_ind,&
							& mann,rnk_ind,ind_sur,ind_no_data,dem_lin,wl_lin,rep_fnam,dir,xllcorner,yllcorner,ras_out,freq_err,&
							frac_wet_area, avg_zpnd_pr,max_dep,ovrlnd_rof_pr) !maybe the output that MESH needs
		
        else
			!run dummy PRIMA for only 1 iteration every 30 minutes
			!write(*,*)'run dummy PRIMA'
			dum_niter=1
			call Run_PRIMA(exc_water_evap,norm_exwtr,ascii_data,rain_flag,inactive_cells,active_cells,year,day,hour,mins,n_rvr_cell,dum_niter,&
							& error_thr,cell_size,no_data,time_step,errord_thr,vol_thr,outflow_pr,ind_node,rvr_cell_ind,&
							& mann,rnk_ind,ind_sur,ind_no_data,dem_lin,wl_lin,rep_fnam,dir,xllcorner,yllcorner,ras_out,freq_err,&
							frac_wet_area, avg_zpnd_pr,max_dep,ovrlnd_rof_pr) !maybe the output that MESH needs
		end if

        
        !--------------------------
        !bookkeeping and updating variables
        adj_rofo=min(ovrlnd_rof_pr,ZPNDCLS(n))
        adj_avg_zpnd=ZPNDCLS(n)-adj_rofo

        vs%grid%zpnd(n) = adj_avg_zpnd/1000.0 !avg_zpnd_pr/1000.0 !mm to m
        vs%grid%pndw(n) = adj_avg_zpnd !mm to mm
        vs%grid%rofo(n) = adj_rofo/ic%dts !mm to mm/s
		ZPNDCLSPRE(n)= adj_avg_zpnd !vs%grid%zpnd(n)*1000.0 !m to mm MIA This is the right place as it will take value after updating PRIMA

        !----------------------
        !write MB
        !MB=norm_exwtr-(avg_zpnd_pr-avg_zpnd_pr_previous)-ovrlnd_rof_pr


    end do


        if (ISHEADNODE) then
        ! write(*,*)'writting*******'
        ! write(*,1110)year,day,hour,mins,n,exc_water_evap,POT_CLASS,POT_pnmn,norm_exwtr,frac_wet_area, &
                    ! avg_zpnd_pr,adj_avg_zpnd, max_dep,ovrlnd_rof_pr,adj_rofo
        write(99099,1110)year,day,hour,mins,n,exc_water_evap,POT_CLASS,POT_pnmn,norm_exwtr,frac_wet_area, &
                    avg_zpnd_pr,adj_avg_zpnd, max_dep,ovrlnd_rof_pr,adj_rofo
         1110    format(9999(g15.7e2, ','))
         
        end if


!mesh
    do k = il1, il2
        if (vs%grid%zpnd(shd%lc%ILMOS(k)) > 0.0 .and. vs%tile%zpnd(k) == 0.0) then
            vs%tile%tpnd(k) = 273.16
        end if
        vs%tile%zpnd(k) = vs%grid%zpnd(shd%lc%ILMOS(k))
    end do

    end subroutine



end module

