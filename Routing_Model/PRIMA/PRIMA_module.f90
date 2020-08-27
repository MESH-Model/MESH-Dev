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
    integer:: t
    real::time_start,time_end
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !PRIMA model inputs
    character(len=100):: fnam, dir,rep_fnam,fin_wd
    character(len=200)::ascii_data(6)
    integer::rain_flag,ncol,nrow,ntot,inactive_cells,active_cells,n_rvr_cell,method,ras_out,freq_err
    integer:: j,niter,ini_dep_flag,lin_indj
    real*8:: xllcorner,yllcorner
    real ::outflow_pr !outflow volume from PRIMA (either m3 or cms based on the module used)
    real:: frac_wet_area, avg_zpnd_pr,max_dep,ovrlnd_rof_pr !in mm outputs of PRIMA
!    real:: avg_zpnd_pr_pre
    real :: adj_avg_zpnd
    integer,allocatable::ind_node(:,:),rvr_cell_ind(:)
    real,allocatable::mann(:)!,vel(:),dt(:)
    integer, allocatable::rnk_ind(:),ind_sur(:,:),ind_no_data(:)
    real*8, allocatable:: dem_lin(:),wl_lin(:),wdepth(:)
    real:: time_step,no_data,cell_size,error_thr,vol_thr,errord_thr
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
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                 READ MESH water Balance data                                 !!
    !! ZPND and ROFO (ponded depth and overland runoff)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    this is implemented as a test. The actual excess water will be calculated by MESH

    !(just for testing, the actual values will be passed from MESH to PRIMA)
    !get the number of days
!mesh    fnam='Basin_average_water_balance.csv'
!mesh    ndays=get_nlines(fnam)
!mesh    ndays=ndays-1
!mesh    allocate(CLSWAT(ndays,39),ZPNDCLS(ndays),ROFOCLS(ndays))
        allocate(ZPNDCLSPRE(shd%NA),ZPNDCLS(shd%NA))!,ROFOCLS(shd%NA))


!mesh    open(1, file=fnam,status='old')
!mesh    read(1,*)trash
!mesh    do j=1,ndays
!mesh        read(1,*)CLSWAT(j,:)  ! values in mm
!mesh    end do
!mesh    close(1)
    !write(*,*)CLSWAT(10,[12,20])

!mesh    ROFOCLS=CLSWAT(:,13)
!mesh    ZPNDCLS=CLSWAT(:,20)
!        ROFOCLS = 0.0
        ZPNDCLS = 0.0
    !initialize state variables
!mesh    ZPNDCLSPRE=0 !previous ponded depth by class
        ZPNDCLSPRE = vs%grid%zpnd*1000.0 !m to mm
!mesh    avg_zpnd_pr_pre=0 !previous ponded depth by PRIMA

!mesh    deallocate(CLSWAT)


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                      READ PRIMA model inputs                                 !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    call read_PRIMA_inputs(ascii_data,error_thr,errord_thr,vol_thr,rain_flag,ncol,nrow,ntot,inactive_cells,&
                              & active_cells,method,n_rvr_cell,niter,xllcorner,yllcorner,outflow_pr,ind_node,&
                              & rvr_cell_ind,mann,rnk_ind,ind_sur,ind_no_data,dem_lin,wl_lin,time_step,no_data&
                              & ,cell_size,rep_fnam,dir,ini_dep_flag,ras_out,freq_err)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                  END READ PRIMA model inputs                                 !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !***********************
    !outputs (dummy for offline run only)
    if (ISHEADNODE) then

    fnam=trim(dir)// '/' // 'PRIMA_balance.csv'
    write(*,*)fnam
    open(99099,file=fnam,status='unknown')
    write(99099,*)'time step,','exc_water_evap,', 'frac_wet_area_%,', 'avg_zpnd_pr_mm,',&
                &'adj_avg_zpnd_pr_mm,','max_dep_mm,','ovrlnd_rof_pr_mm,','outflow_pr_m3,','outflow cms'

    end if

    !***********************

    end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine prima_within_grid(fls, shd)

        type(fl_ids), intent(in) :: fls
        type(ShedGridParams), intent(in) :: shd

        integer n, k

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                     this section will be placed a                            !!
    !!  as a runoff generation algorithm wherein MESH applies it every time step    !!
    !!       it will be much faster if it can on a daily basis not hourly           !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!        if (rain_flag == 2 .and. ic%ts_hourly == 1) then
!            ROFOCLS = 0.0
!        else if (rain_flag == 1 .and. ic%ts_daily == 1) then
!            ROFOCLS = 0.0
!        else if (rain_flag == 0) then
!            ROFOCLS = 0.0
!        end if


!!!!!!!!!!!!!!!!!!!!!!!!!!
		!state variables are updated each time step in here
        !ROFOCLS = 0.0 !ROFOCLS + vs%grid%rofo*ic%dts !mm/s to mm
		!add ROFO calculated by CLASS to the ponded depth to get the excess water depth
        !write(*,*)vs%grid%zpnd(i1)*1000.0, vs%grid%rofo(i1)*ic%dts
        !this for tile (if we are going to 
        !vs%tile%zpnd(i1:i2) = vs%tile%zpnd(il1:il2) + vs%tile%rofo(il1:il2)*ic%dts/1000.0 !mm/s to m
        
        vs%grid%zpnd(i1:i2) = vs%grid%zpnd(i1:i2) + vs%grid%rofo(i1:i2)*ic%dts/1000.0 !mm/s to m
        vs%grid%pndw(i1:i2) = vs%grid%pndw(i1:i2) + vs%grid%rofo(i1:i2)*ic%dts !mm/s to mm
        vs%grid%rofo(i1:i2) = 0.0
        !write(*,*)vs%grid%pndw(1),vs%grid%zpnd(1)
        !this is for tiles
		do k = il1, il2
            if (vs%grid%zpnd(shd%lc%ILMOS(k)) > 0.0 .and. vs%tile%zpnd(k) == 0.0) then
                vs%tile%tpnd(k) = 273.16
            end if
            vs%tile%zpnd(k) = vs%grid%zpnd(shd%lc%ILMOS(k)) !ILMOS is grid number; !JLMOS is the GRU number; ACLASS is GRU fraction first index is the grid and the second is the GRU
        end do

		!Here, we can update the excess depth and then we can run the dummy PRIMA module
		        
		!***MIA_v03
		!check to control PRIMA to run the actual code every day or every hour
        !if (rain_flag == 2 .and. ic%now%hour == ic%next%hour) then
        !    return
        !else if (rain_flag == 1 .and. ic%now%jday == ic%next%jday) then
        !    return
        !end if



!!!!!!!!
        if (rain_flag == 2) then
            t = ic%iter%hour
        else if (rain_flag == 1) then
            t = ic%iter%day
        else if (rain_flag == 0) then
            t = ic%ts_count
        end if

!mesh    do t=1,ndays; !this refers to the time step (t day or hour) number. The actual number will be passed by MESH.
        do n = i1, i2
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!!!!!!!! RUNING PRIMA model in a daily time step     !!!!!!!!!
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !! please note that exc_water_evap is the amount of water added or removed to the old storage
        !! i.e., the difference between current storage and old storage
        !!example (adding water): if the ponded water was 10 mm, MESH calculated excess rainfall/snowmelt and was found to be 5mm,
        !! then, exc_water_evap equals 5 mm. the model will add 5 mm to the existing 10mm ponded water continue redistributing and qunatifying
        !! the amount of storage and surface runoff.
        !! example (removing water, evaporating): the old storage is 10mm, this was sent to MESH and the new storage became 7mm
        !! then, exc_water_evap= -3 mm, which means that 3 mm of water are going to evaporate.
        !add/subtract water

        !this will be replaced by actual ZPND and ROFO from CLASS
!mesh        exc_water_evap=(ZPNDCLS(t)-ZPNDCLSPRE)+ROFOCLS(t) !calculate the gains and losses of ponded depth + RORO(added water)
		ZPNDCLS(n) = vs%grid%zpnd(n)*1000.0 !m to mm
        exc_water_evap=(ZPNDCLS(n)-ZPNDCLSPRE(n))!+ROFOCLS(n) !calculate the gains and losses of ponded depth + RORO(added water)
        
        !debugging
		!write(*,*)t,ZPNDCLS(n)!,ZPNDCLSPRE(n)
		!read(*,*)dum_niter
        
        if (method .ne. 2)then
            !add water depth directly on top of the dem
            !apply the CA algorithm
            wl_lin=wl_lin+(exc_water_evap/1000.0); !apply the excess water depth (whether it is applied or removed) and convert to meters (/1000)
            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            !search for wl_lin<dem_lin and keep it to dem level (depth=0) (when evaporating water from potholes)
            !for active cells only
            if(exc_water_evap .lt. 0.0)then
                do j=1,active_cells !loop through active cells only to make sure that there is no cell with water level less than DEM
                    lin_indj=rnk_ind(j)
                    if(wl_lin(lin_indj)<dem_lin(lin_indj))  wl_lin(lin_indj)=dem_lin(lin_indj)
                end do
            end if
            wl_lin(ind_no_data)=no_data
        end if
        
        !!if (ic%now%day /= ic%next%day) then Run_PRIMA… else do something... end if !end fo day check
		
		!***MIA_v03
		!dumy variables
		avg_zpnd_pr=9.9
		ovrlnd_rof_pr=9.99
		
		if ((rain_flag == 1 .and. ic%now%day /= ic%next%day) .or. (rain_flag == 2 .and. ic%now%hour /= ic%next%hour)) then !run in a daily or an hourly timestep (hourly is not working)
			!run actual PRIMA at the end of the day/hour
			!write(*,*)'******run real PRIMA*******'
            !!niter=1 !dummy test
			call Run_PRIMA(ascii_data,rain_flag,inactive_cells,active_cells,t,n_rvr_cell,method,niter,&
							& error_thr,cell_size,no_data,time_step,errord_thr,vol_thr,outflow_pr,ind_node,rvr_cell_ind,&
							& mann,rnk_ind,ind_sur,ind_no_data,dem_lin,wl_lin,rep_fnam,dir,xllcorner,yllcorner,ras_out,freq_err,&
							frac_wet_area, avg_zpnd_pr,max_dep,ovrlnd_rof_pr) !maybe the output that MESH needs
		
        else
			!run dummy PRIMA for only 1 iteration every 30 minutes
			!write(*,*)'run dummy PRIMA'
			dum_niter=1
			call Run_PRIMA(ascii_data,rain_flag,inactive_cells,active_cells,t,n_rvr_cell,method,dum_niter,&
							& error_thr,cell_size,no_data,time_step,errord_thr,vol_thr,outflow_pr,ind_node,rvr_cell_ind,&
							& mann,rnk_ind,ind_sur,ind_no_data,dem_lin,wl_lin,rep_fnam,dir,xllcorner,yllcorner,ras_out,freq_err,&
							frac_wet_area, avg_zpnd_pr,max_dep,ovrlnd_rof_pr) !maybe the output that MESH needs
		end if


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !make sure that the water balance close (i.e., ponded+runoff depth= total applied depth)
        !this will be activated inside MESH
        ! I - O = ds
        !note that sometimes the balance will not close (especially when evaporating) because when evaporating the evaporative depth
        !is being deducted from wet cells not all cells and will lead to error in balance equation
        !check this block later
!        adj_avg_zpnd=avg_zpnd_pr+(exc_water_evap-ovrlnd_rof_pr)-(avg_zpnd_pr-avg_zpnd_pr_pre)
!        adj_avg_zpnd=maxval([adj_avg_zpnd, 0.0])
        adj_avg_zpnd = 0.0
        !this the output that makes the water balance close (i.e., any additional water is added or removed for ponded water)
        !the adj_avg_zpnd will be passed back to CLASS to make the water balance close
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !fractional water covered area, mean ponded depth (mm), max ponded depth (over all potholes) (mm), outflow depth (mm), outflow volume (m3)
        ! these are the values that might be needed to be sent back to MESH to be used for further calculations
        !write(*,*)'outputs', frac_wet_area,'%', avg_zpnd_pr,'mm',max_dep,'mm',ovrlnd_rof_pr,'mm',outflow_pr,'m3'
        !write(*,*)'outflow cms=',outflow_pr/time_step

        if (ISHEADNODE) then

        write(99099,*)t,',',exc_water_evap,',',frac_wet_area,',', avg_zpnd_pr,',',adj_avg_zpnd,',',&
                    & max_dep,',',ovrlnd_rof_pr,',',outflow_pr,',',outflow_pr/time_step

        end if




        !bookkeeping assign current ZPNDCLS as previous to estimate the amount of water add/subtracted from ponded water by CLASS
!mesh        ZPNDCLSPRE=ZPNDCLS(t)
        !ZPNDCLSPRE = ZPNDCLS(n) !placed by Dan but might be wrong as this value should be taken after updating the ZPND by PRIMA
		
        !!!!!!!avg_zpnd_pr=avg_zpnd_pr*frac_wet_area !test
        
        vs%grid%zpnd(n) = avg_zpnd_pr/1000.0 !avg_zpnd_pr/1000.0 !mm to m
        vs%grid%pndw(n) = avg_zpnd_pr !mm to mm
        vs%grid%rofo(n) = ovrlnd_rof_pr/ic%dts !mm to mm/s
!        avg_zpnd_pr_pre=avg_zpnd_pr !or adj_avg_zpnd_pr
        !! the updated value of the ZPND is (adj_avg_zpnd) and ROFO is ovrlnd_rof_pr
		ZPNDCLSPRE(n)= avg_zpnd_pr !vs%grid%zpnd(n)*1000.0 !m to mm MIA This is the right place as it will take value after updating PRIMA
		!write(*,*)'*****',ZPNDCLS(n),avg_zpnd_pr


    end do

!mesh
    do k = il1, il2
        if (vs%grid%zpnd(shd%lc%ILMOS(k)) > 0.0 .and. vs%tile%zpnd(k) == 0.0) then
            vs%tile%tpnd(k) = 273.16
        end if
        vs%tile%zpnd(k) = vs%grid%zpnd(shd%lc%ILMOS(k))
    end do

    end subroutine

!mesh    close(99)
    !!!writing output raster and outflow to files
!    allocate(wdepth(ntot))
!    wdepth=wl_lin-dem_lin
!    fin_wd='fin_wdepth.asc'
!    call write_ascii(wdepth,no_data,xllcorner,yllcorner,cell_size,ind_no_data,active_cells,rnk_ind,&
!                        & ascii_data,fin_wd,dir, inactive_cells)
!    deallocate(wdepth)
!    !write output
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !%write outflows
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    if(method .ne. 1 .and. rain_flag .ne. 0)then
!        foutflow=trim(dir)// '/' // 'PRIMA_outflow_cms.txt'
!        open(4,file=TRIM(ADJUSTL(foutflow)),status='unknown')
!        outflow_pr=outflow_pr/time_step
!        write(4,*) outflow_pr
!        close(4)
!    elseif(method .ne. 1 .and. rain_flag .eq. 0) then
!        foutflow=trim(dir)// '/' // 'PRIMA_outflow_m3.txt'
!        open(4,file=TRIM(ADJUSTL(foutflow)),status='unknown')
!        write(4,*) outflow_pr
!        close(4)
!    end if

!mesh    call cpu_time(time_end)

!mesh    write(*,*)'Elapsed time', (time_end-time_start)/60.0,'min'
!mesh    stop


end module

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!                              Subroutines                                     !!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!meshfunction get_nlines(fnam)
!mesh    implicit none
!mesh    character(len=100)::fnam,str
!mesh    integer::n,get_nlines,io

    !write(*,*)fnam
!mesh    open(1, file=fnam,status='old')

!mesh    n=0
!mesh    do
!mesh        READ(1,*,IOSTAT=io)  str
!mesh        IF (io > 0) THEN
!mesh          WRITE(*,*) 'Check input.  Something was wrong'
!mesh          EXIT
!mesh       ELSE IF (io < 0) THEN
!mesh          !WRITE(*,*)  'end of file ', n
!mesh          EXIT
!mesh       ELSE
!mesh          n = n + 1
!mesh       END IF
!mesh    end do
!mesh    close(1)


!mesh    get_nlines=n

!meshreturn
!meshend function get_nlines
