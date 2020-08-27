program main_PRIMA_model


    use PRIMA_inputs
    use main_PRIMA

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
    real:: avg_zpnd_pr_pre,adj_avg_zpnd
    integer,allocatable::ind_node(:,:),rvr_cell_ind(:)
    real,allocatable::mann(:)!,vel(:),dt(:)
    integer, allocatable::rnk_ind(:),ind_sur(:,:),ind_no_data(:)
    real*8, allocatable:: dem_lin(:),wl_lin(:),wdepth(:)
    real:: time_step,no_data,cell_size,error_thr,vol_thr,errord_thr
    !dummy inputs
    integer:: ndays,get_nlines
    real:: ZPNDCLSPRE
    real, allocatable:: CLSWAT(:,:),ZPNDCLS(:),ROFOCLS(:) !ponded and overland runoff produced by class
    common /dims/ncol,nrow,ntot

    call cpu_time(time_start)



    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                 READ MESH water Balance data                                 !!
    !! ZPND and ROFO (ponded depth and overland runoff)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    this is implemented as a test. The actual excess water will be calculated by MESH

    !(just for testing, the actual values will be passed from MESH to PRIMA)
    !get the number of days
    fnam='Basin_average_water_balance.csv'
    ndays=get_nlines(fnam)
    ndays=ndays-1
    allocate(CLSWAT(ndays,39),ZPNDCLS(ndays),ROFOCLS(ndays))


    open(1, file=fnam,status='old')
    read(1,*)trash
    do j=1,ndays
        read(1,*)CLSWAT(j,:)  ! values in mm
    end do
    close(1)
    !write(*,*)CLSWAT(10,[12,20])

    ROFOCLS=CLSWAT(:,13)
    ZPNDCLS=CLSWAT(:,20)
    !initialize state variables
    ZPNDCLSPRE=0 !previous ponded depth by class
    avg_zpnd_pr_pre=0 !previous ponded depth by PRIMA

    deallocate(CLSWAT)


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
    fnam=trim(dir)// '/' // 'PRIMA_balance.csv'
    write(*,*)fnam
    open(99,file=fnam,status='unknown')
    write(99,*)'time step,','exc_water_evap,', 'frac_wet_area_%,', 'avg_zpnd_pr_mm,',&
                &'adj_avg_zpnd_pr_mm,','max_dep_mm,','ovrlnd_rof_pr_mm,','outflow_pr_m3,','outflow cms'

    !***********************


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!                     this section will be placed a                            !!
    !!  as a runoff generation algorithm wherein MESH applies it every time step    !!
    !!       it will be much faster if it can on a daily basis not hourly           !!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




    do t=1,ndays; !this refers to the time step (t day or hour) number. The actual number will be passed by MESH.
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
        exc_water_evap=(ZPNDCLS(t)-ZPNDCLSPRE)+ROFOCLS(t) !calculate the gains and losses of ponded depth + RORO(added water)


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

        call Run_PRIMA(ascii_data,rain_flag,inactive_cells,active_cells,t,n_rvr_cell,method,niter,&
                        & error_thr,cell_size,no_data,time_step,errord_thr,vol_thr,outflow_pr,ind_node,rvr_cell_ind,&
                        & mann,rnk_ind,ind_sur,ind_no_data,dem_lin,wl_lin,rep_fnam,dir,xllcorner,yllcorner,ras_out,freq_err,&
                        frac_wet_area, avg_zpnd_pr,max_dep,ovrlnd_rof_pr) !maybe the output that MESH needs



        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !make sure that the water balance close (i.e., ponded+runoff depth= total applied depth)
        !this will be activated inside MESH
        ! I - O = ds
        !note that sometimes the balance will not close (especially when evaporating) because when evaporating the evaporative depth
        !is being deducted from wet cells not all cells and will lead to error in balance equation
        !check this block later
        adj_avg_zpnd=avg_zpnd_pr+(exc_water_evap-ovrlnd_rof_pr)-(avg_zpnd_pr-avg_zpnd_pr_pre)
        adj_avg_zpnd=maxval([adj_avg_zpnd, 0.0])
        !this the output that makes the water balance close (i.e., any additional water is added or removed for ponded water)
        !the adj_avg_zpnd will be passed back to CLASS to make the water balance close
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !fractional water covered area, mean ponded depth (mm), max ponded depth (over all potholes) (mm), outflow depth (mm), outflow volume (m3)
        ! these are the values that might be needed to be sent back to MESH to be used for further calculations
        !write(*,*)'outputs', frac_wet_area,'%', avg_zpnd_pr,'mm',max_dep,'mm',ovrlnd_rof_pr,'mm',outflow_pr,'m3'
        !write(*,*)'outflow cms=',outflow_pr/time_step

        write(99,*)t,',',exc_water_evap,',',frac_wet_area,',', avg_zpnd_pr,',',adj_avg_zpnd,',',&
                    & max_dep,',',ovrlnd_rof_pr,',',outflow_pr,',',outflow_pr/time_step




        !bookkeeping assign current ZPNDCLS as previous to estimate the amount of water add/subtracted from ponded water by CLASS
        ZPNDCLSPRE=ZPNDCLS(t)
        avg_zpnd_pr_pre=avg_zpnd_pr !or adj_avg_zpnd_pr
        !! the updated value of the ZPND is (adj_avg_zpnd) and ROFO is ovrlnd_rof_pr



    end do


    close(99)
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

    call cpu_time(time_end)

    write(*,*)'Elapsed time', (time_end-time_start)/60.0,'min'
    stop


end program main_PRIMA_model

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!                              Subroutines                                     !!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function get_nlines(fnam)
    implicit none
    character(len=100)::fnam,str
    integer::n,get_nlines,io

    !write(*,*)fnam
    open(1, file=fnam,status='old')

    n=0
    do
        READ(1,*,IOSTAT=io)  str
        IF (io > 0) THEN
          WRITE(*,*) 'Check input.  Something was wrong'
          EXIT
       ELSE IF (io < 0) THEN
          !WRITE(*,*)  'end of file ', n
          EXIT
       ELSE
          n = n + 1
       END IF
    end do
    close(1)


    get_nlines=n

return
end function get_nlines
