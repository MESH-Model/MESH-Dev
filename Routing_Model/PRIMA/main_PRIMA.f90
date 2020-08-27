module main_PRIMA

    contains

! Runs the PRIMA model
subroutine Run_PRIMA(exc_water_evap,norm_exwtr,ascii_data,rain_flag,inactive_cells,active_cells,year,day,hour,mins,n_rvr_cell,niter,&
                       & error_thr,cell_size,no_data,time_step,errord_thr,vol_thr,outflow,ind_node,rvr_cell_ind,&
                       & mann,rnk_ind,ind_sur,ind_no_data,dem_lin,wl_lin,rep_fnam,dir,xllcorner,yllcorner,ras_out,freq_err,&
                       & frac_wet_area, mean_dep,max_dep,drain_depth)


implicit none

    character(len=100):: dir,rep_fnam
    character(len=200)::ascii_data(6)
    integer::ncol,nrow,ntot,rain_flag,inactive_cells,active_cells,year,day,hour,mins,n_rvr_cell
    integer:: niter,ras_out,freq_err
    real:: error_thr, cell_size,no_data,time_step,vol_thr,time_end, time_start,outflow,errord_thr
    !double precision real number
    real*8:: xllcorner,yllcorner
    integer::ind_node(nrow,ncol),rvr_cell_ind(n_rvr_cell)
    real:: frac_wet_area, mean_dep,max_dep,drain_depth,norm_exwtr, zpnd1, zpnd2,exc_water_evap
    real::mann(ntot),vel(ntot),dt(ntot)

    integer::rnk_ind(ntot),ind_sur(ntot,10),ind_no_data(inactive_cells)


    real*8:: dem_lin(ntot),wl_lin(ntot)!,wdepth(ntot)

    common /dims/ncol,nrow,ntot



        vel=0;    !velocity for each cell (for each cell we will choose the min velocity among the surrounding cells)
        dt=time_step*100; !time step for each cell
        !wdepth=0;   !initial water depth

        outflow=0
        !write(*,*)mann(500)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!                    apply PRIMA model                    !!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    
        zpnd1 = (sum(wl_lin-dem_lin)/active_cells)*1000.0 !to convert from m to mm
        !write(*,*)'zpnd1',zpnd1
        !add water depth directly on top of the dem
        !apply the CA algorithm
        wl_lin=wl_lin+(exc_water_evap/1000.0); !apply the excess water depth (whether it is applied or removed) and convert to meters (/1000)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !search for wl_lin<dem_lin and keep it to dem level (depth=0) (when evaporating water from potholes)
        !for active cells only
        if(exc_water_evap .lt. 0.0)then
            where (wl_lin < dem_lin) wl_lin = dem_lin !to make sure that there is no cell with water level less than DEM
        end if
        wl_lin(ind_no_data)=no_data

        
        zpnd2 = (sum(wl_lin-dem_lin)/active_cells)*1000.0 !to convert from m to mm
        !write(*,*)'zpnd2',zpnd2
        !calculate normalized excess water (to account for the scale issue from unit area (depth units) to the basin area)
        norm_exwtr = zpnd2 - zpnd1
        


        ! if(method .eq. 1)then
            ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ! !!Add/subtract water to the dem without draining the water that reaches the outlet cells
            ! call add_water(time_step,error_thr,dt,vel,wl_lin,active_cells,rnk_ind,dem_lin,no_data,ind_sur,year,day,hour,mins,mann,&
                            ! & cell_size,dir,ascii_data,xllcorner,yllcorner,rep_fnam,ind_no_data,&
                            ! & inactive_cells,niter,ras_out,freq_err,frac_wet_area, mean_dep,max_dep,drain_depth)


            ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ! !**********************************************************************************
        ! elseif (method .eq. 2) then

            ! !!Drain water after adding it from river cells
            ! call drain_water(time_step,errord_thr,dt,vel,wl_lin,active_cells,rnk_ind,dem_lin,no_data,ind_sur,year,day,hour,mins,mann,&
                    ! & cell_size,dir,ascii_data,xllcorner,yllcorner,rep_fnam,ind_no_data,inactive_cells,outflow,&
                    ! & rvr_cell_ind,n_rvr_cell,vol_thr,niter,ras_out,freq_err,frac_wet_area, mean_dep,max_dep,drain_depth)

        ! elseif (method .eq. 3) then

            ! call add_water(time_step,error_thr,dt,vel,wl_lin,active_cells,rnk_ind,dem_lin,no_data,ind_sur,year,day,hour,mins,mann,&
                            ! & cell_size,dir,ascii_data,xllcorner,yllcorner,rep_fnam,ind_no_data,&
                            ! & inactive_cells,niter,ras_out,freq_err,frac_wet_area, mean_dep,max_dep,drain_depth)


            ! call drain_water(time_step,errord_thr,dt,vel,wl_lin,active_cells,rnk_ind,dem_lin,no_data,ind_sur,year,day,hour,mins,mann,&
                    ! & cell_size,dir,ascii_data,xllcorner,yllcorner,rep_fnam,ind_no_data,inactive_cells,outflow,&
                    ! & rvr_cell_ind,n_rvr_cell,vol_thr,niter,ras_out,freq_err,frac_wet_area, mean_dep,max_dep,drain_depth)
        ! else
            !add and drain the water at the same time
            call add_drain_water(time_step,errord_thr,dt,vel,wl_lin,active_cells,rnk_ind,dem_lin,no_data,ind_sur,year,day,hour,mins,mann,&
                    & cell_size,dir,ascii_data,xllcorner,yllcorner,rep_fnam,ind_no_data,inactive_cells,outflow,&
                    & rvr_cell_ind,n_rvr_cell,vol_thr,niter,ras_out,freq_err,frac_wet_area, mean_dep,max_dep,drain_depth)

            !**********************************************************************************
        ! end if


return
end subroutine Run_PRIMA

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!                              Subroutines                                     !!
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*******************************************
subroutine neighbor_cells8(ind_node,dem_lin,ind_sur,no_data)
    implicit none
    integer, intent(in):: ind_node(nrow,ncol)
    integer, intent(out):: ind_sur(ntot,10)
    integer:: dum(8),dum2(8)
    real*8,intent(in):: dem_lin(ntot)
    real,intent(in):: no_data
    integer:: c,active_sur_cells,s,se,e,ne,n,nw,w,sw,ele,nrow,ncol,ntot,i,j
    common /dims/ncol,nrow,ntot
    write(*,*) 'Getting neighboring 8 cells of all cells '
!% This technique is better than the 4 cells as it will be consistent with
!% the flow direction generated from arcGIS and also due to the following
!% case water will be retained at cell 18 as it can't drain diagonally
!% (hypothetical case but can occur)
!%        --------------
!%        | 40 | 50 | 43 |
!%        --------------
!%        | 30 | 10 | 10 |
!%        --------------
!%        | 18 | 20 | 25 |
!%        --------------
!%Cells orientation
!%        ----------
!%        | 6(nw) | 5(n ) | 4(ne) |
!%        -------------------------
!%        | 7(w ) | 0(c ) | 3(e ) |
!%        -------------------------
!%        | 8(sw) | 1(s ) | 2(se) |
!%        -------------------------

!array in_sur cell_linearid, no_of surrounding cells, surrounding cells linear_id as (1s,2se,3e,4ne,5n,6nw,7w,8sw)
ind_sur=0

    do j=1,ncol
    do i=1,nrow

        c=ind_node(i,j) !linear index of central cell

        if(dem_lin(c) .eq. no_data) then
            goto 55
        end if
        !linear index of all surrounding cells
        s=c+1
        se=s+nrow
        e=c+nrow
        ne=e-1
        n=c-1
        nw=n-nrow
        w=c-nrow
        sw=w+1

        !check if surrounding cells exist (cases of edge cells)
        !inactive_sur_cells=0
        if(i .eq. 1)then
            n=0; ne=0; nw=0
        end if
        if(i .eq. nrow) then
            s=0; se=0; sw=0;
        end if
        if(j .eq. 1)then
             w=0; nw=0; sw=0
        end if
        if( j .eq. ncol)then
            e=0; ne=0; se=0
        end if


        !get the number of elements not equal to zero (active surrounding cells) and ignore cells with no data also
        dum=[s,se,e,ne,n,nw,w,sw]
        dum2=0 !sorted final surrounding values (zeros comes last at this array)
        active_sur_cells=0
            do ele=1,8
                if(dum(ele) .gt. 0) then
                    if(dem_lin(dum(ele)) .ne. no_data)then
                        active_sur_cells=active_sur_cells+1
                        dum2(active_sur_cells)=dum(ele)
                    endif
                endif
            end do
        !active_sur_cells=9-inactive_sur_cells


        ind_sur(c,:)=[c,active_sur_cells,dum2(:)]

55  end do
end do


    return
end subroutine neighbor_cells8


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*******************************************
subroutine min_algorithm(wel0, wel, cur_ind_sur,n_sur_cells, wl_lin, dem_lin, mann, vel, cell_size,lin_ind,time_step)
    implicit none
!this subroutine is based on (Di Gregorio and Serra, 1999) in 8 surrounding cells
    integer,intent(in):: cur_ind_sur(10),lin_ind,n_sur_cells
    integer::cur_case,ncol,nrow,ntot
    real*8,intent(in):: wel(n_sur_cells), dem_lin(ntot)
    real,intent(in):: mann(ntot)
    real*8, intent(inout)::wl_lin(ntot)
    real, intent(inout):: vel(ntot)
    real,intent(in)::cell_size,time_step
    real*8,intent(in)::wel0
    real*8:: av,d1,cen_wl,sur_wl1,cur_wel
    real*8, allocatable:: wel_dum(:),wel_dum2(:) !flexibile
    integer:: ind(8),ne_new,ne,l,debug_cell,i
    integer, allocatable:: fin(:),ind_dum(:),ind_dum2(:)
    real*8, allocatable:: d(:),s(:),v(:),sur_wl(:)
    common /dims/ncol,nrow,ntot

    allocate(wel_dum(n_sur_cells),ind_dum(n_sur_cells))
    ind_dum=cur_ind_sur(3:3+n_sur_cells-1)
    wel_dum=wel
    ind=0
    ne=0; ne_new=0; av=0;

    do l=1,n_sur_cells+1 !loop through the surrounding cells and select cells with water elevation less than average water elevation

        ne=size(wel_dum,1)+1  !number of elements of wel (+1 for central cell)
        av=(wel0+sum(wel_dum))/ne !average water level

        !!!!!!find cells with water elevation < av !!!!!!
        ne_new=0
        ind=0
        do i=1,ne-1
            cur_wel=wel_dum(i) !for debugging
            if(wel_dum(i) .lt. av)then
                ne_new=ne_new+1 !new number of elements that have wl < av
                ind(ne_new)=i   !the index of these elements
            endif
        end do

        if (ne_new .eq. n_sur_cells .or. ne-1 .eq. ne_new .or. ne_new .eq. 0) then
            goto 11
        end if
        allocate(wel_dum2(ne-1),ind_dum2(ne-1)) !take the same old values
        wel_dum2=wel_dum
        ind_dum2=ind_dum

        deallocate(wel_dum,ind_dum)
        allocate(wel_dum(ne_new),ind_dum(ne_new)) !allocate new array with the values less than av only
        !write(*,*)ind(1:ne_new)
        wel_dum(:)=wel_dum2(ind(1:ne_new))
        ind_dum(:)=ind_dum2(ind(1:ne_new))
        deallocate(wel_dum2,ind_dum2)
    enddo !loop though ncells


11  if (ne .eq. 1 .or. ne_new .eq. 0)then  ! sometimes the central cell is lower than the surrounding cells this is to stop calculations
        vel(lin_ind)=0;
        goto 12
    endif

    allocate(fin(ne_new),d(ne_new),s(ne_new),v(ne_new))

    if (av>dem_lin(lin_ind))then
        cur_case=1
        fin=ind_dum; !final index with the surrounding cells that were less than av
!       %calculating velocity and time
        if(time_step<=1e20)then
            d=av-wl_lin(fin);   !depth distributed for each cell
!mesh: max of value or zero
            s=max((wl_lin(lin_ind) - wl_lin(fin)),0.0)/cell_size; !water surface slope
            v=((maxval(d)**(2.0/3.0))*(maxval(s)**0.5))/minval(mann(fin));
        end if
        cen_wl=av;
        sur_wl1=av;
    else
        cur_case=2
        allocate(sur_wl(ne_new))
        fin=ind_dum; !final index without of the surrounding cells that were less than av
        d1= wel0 - dem_lin(lin_ind);
        d1 = d1/(ne-1);
        if(time_step<=1e20)then
            s=(wl_lin(lin_ind) - wl_lin(fin))/cell_size;
            v=((d1**(2.0/3.0))*(maxval(s)**0.5))/minval(mann(fin));
        end if
        cen_wl=dem_lin(lin_ind);
        sur_wl=wl_lin(fin)+d1;
    endif


    if(time_step>=1e20) then
        vel(lin_ind)=0
    else
        vel(lin_ind)=maxval(v);    !choose max velocity for the surrounding cells which will result in min time
    end if
    wl_lin(lin_ind)=cen_wl;
    if(cur_case .eq. 1) wl_lin(fin)=sur_wl1;
    if(cur_case .eq. 2) wl_lin(fin)=sur_wl;

12  return
end subroutine min_algorithm

!******************************************************************************************************************!
!------------------------------------------------------------------------------------
!Add water subroutine
! subroutine add_water(time_step,error_thr,dt,vel,wl_lin,active_cells,rnk_ind,dem_lin,no_data,ind_sur,year,day,hour,mins,mann,&
                    ! & cell_size,dir,ascii_data,xllcorner,yllcorner,rep_fnam,ind_no_data,&
                    ! & inactive_cells,niter,ras_out,freq_err,frac_wet_area, mean_dep,max_dep,drain_depth)

    ! implicit none
    ! real, intent(inout)::dt(ntot),vel(ntot),frac_wet_area, mean_dep,max_dep,drain_depth
    ! real*8, intent(inout)::wl_lin(ntot)
    ! real*8, intent(in):: dem_lin(ntot)
    ! real, intent(in):: no_data, mann(ntot),cell_size,error_thr,time_step
    ! real*8::wl_old(ntot),wdepth(ntot)
    ! real*8::wel0
    ! real::cum_time,derr,time_end,time_start,outflow
    ! integer, intent(in):: active_cells,rnk_ind(ntot),ind_sur(ntot,10),year,day,hour,mins,ind_no_data(inactive_cells),&
                        ! & inactive_cells,niter,ras_out,freq_err
    ! integer::cur_ind_sur(10),iter,lin_ind,n_sur_cells,i,ncol,nrow,ntot
    ! real*8,allocatable::wel(:)
    ! character(len=100),intent(in)::dir,rep_fnam
    ! character(len=200), intent(in)::ascii_data(6)
    ! character(len=100):: fout,fiter
    ! real*8,intent(in):: xllcorner,yllcorner
    ! common /dims/ncol,nrow,ntot


    ! write(*,*) 'Adding water for day',year,day,hour,mins

    ! call cpu_time(time_start)

    ! cum_time=0; !cumulative time

    ! derr=1000; !depth error
    ! wdepth=wl_lin-dem_lin
    ! wl_old=wl_lin;

    ! do iter=1,niter

        ! dt=time_step*100;   !to initialize dt with a very high number
        ! vel=0;  !initialize velocity array
        ! !wl_old=wl_lin;

        ! do i=1,active_cells !loop through active cells only
            ! lin_ind=rnk_ind(i)  !linear indexing for current cell with higher
            ! if (wl_lin(lin_ind) .eq. dem_lin(lin_ind) .or. wl_lin(lin_ind) .eq. no_data)then
                ! goto 77                ! to continue to the next cell if the cell was dry or if it is -9999
            ! endif

            ! wel0=wl_lin(lin_ind);   !water surface elevation in the central cell (current cell)

            ! cur_ind_sur(:)= ind_sur(lin_ind,:);
            ! n_sur_cells=cur_ind_sur(2)

            ! allocate(wel(n_sur_cells))
            ! wel= wl_lin(cur_ind_sur(3:3+n_sur_cells-1));
            ! !min_algorithm call
            ! call min_algorithm(wel0, wel, cur_ind_sur,n_sur_cells, wl_lin, dem_lin, mann, vel, cell_size,lin_ind,time_step)
            ! deallocate(wel)

    ! 77  end do!for i=1,active cells

        ! !check if travel time calculation is active
        ! if(time_step>=1e20) then
            ! cum_time=0
        ! else
            ! dt=cell_size/vel
            ! cum_time=cum_time+minval(dt)
        ! end if



        ! if (cum_time>=time_step .and. niter > 1)then  !sec

            ! derr=maxval(abs(wl_lin-wl_old))

            ! write(*,111) year,day,hour,mins, iter, derr, cum_time
            ! 111 FORMAT (i6,i14,f17.4,f17.4)
            ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
            ! write(2,111) year,day,hour,mins, iter, derr, cum_time
            ! close(2)
            ! goto 66
        ! end if

        ! if (mod(iter,freq_err) .eq. 0)then !calculate error each freq_err iterations

            ! derr=maxval(abs(wl_lin-wl_old))
            ! wl_old=wl_lin

            ! !write output to screen and report
            ! write(*,'(i6,i14,f17.4,f17.4)')year,day,hour,mins,iter,derr,cum_time  !write every 100 iterations
            ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
            ! write(2,'(i6,i14,f17.4,f17.4)')year,day,hour,mins,iter,derr,cum_time  !write every 100 iterations
            ! close(2)
            ! !!!!!!!!!!!!!!!!!!!

            ! if (derr <= error_thr)  goto 66
        ! end if



    ! end do !for iter loop

    ! !write(*,*)'Max number of iterations reached'
    ! !write(*,*)derr,cum_time
    ! !write(*,111) year,day,hour,mins, niter, derr, cum_time
    ! if(niter > 1)then
        ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
        ! write(2,*)'Max number of iterations reached'
        ! write(2,111) year,day,hour,mins, niter, derr, cum_time
        ! close(2)
    ! end if
    
    ! 66 write(fout,222)'day_',year,day,hour,mins,'_add_wdepth.asc'		!water depth at each day
    ! 222 FORMAT (a4,i4.4,a15)
    ! !specifying the full name of the file
    ! if (ras_out .eq. 1 .and. niter > 1)then
        ! wdepth=wl_lin-dem_lin
        ! call write_ascii(wdepth,no_data,xllcorner,yllcorner,cell_size,ind_no_data,active_cells,rnk_ind,&
                         ! & ascii_data,fout,dir, inactive_cells)
    ! end if

    ! outflow=0 !no outflow in the add module
    ! call calc_stat(wdepth,no_data,cell_size,ind_no_data,active_cells,inactive_cells,rnk_ind,rep_fnam,outflow,&
                    ! & frac_wet_area, mean_dep,max_dep,drain_depth,niter)


    ! call cpu_time(time_end)
    ! !write(*,*)'PRIMA run time', (time_end-time_start)/60.0,'min'
    ! if(niter > 1) then
        ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append') !create report empty file
        ! write(2,*)'PRIMA run time', (time_end-time_start)/60.0,'min'
        ! close(2) !report file
    ! end if
    ! return

! end subroutine add_water

!------------------------------------------------------------------------------------
!Drain water subroutine
!same as add_water but here the river cells are drained
! subroutine drain_water(time_step,error_thr,dt,vel,wl_lin,active_cells,rnk_ind,dem_lin,no_data,ind_sur,year,day,hour,mins,mann,&
                    ! & cell_size,dir,ascii_data,xllcorner,yllcorner,rep_fnam,ind_no_data,inactive_cells,outflow,&
                    ! & rvr_cell_ind,n_rvr_cell,vol_thr,niter,ras_out,freq_err,frac_wet_area, mean_dep,max_dep,drain_depth)

    ! implicit none

    ! real, intent(inout)::dt(ntot),vel(ntot),outflow,frac_wet_area, mean_dep,max_dep,drain_depth
    ! real*8, intent(inout)::wl_lin(ntot)
    ! real, intent(in):: no_data, mann(ntot),cell_size,error_thr,time_step,vol_thr
    ! real*8, intent(in):: dem_lin(ntot)
    ! real*8::wl_old(ntot),wdepth(ntot),wel0     !,wdepth_out_2d(nrow,ncol)
    ! real::cum_time,derr,v_out,v_out_old,verr,time_start,time_end
    ! integer, intent(in):: active_cells,rnk_ind(ntot),ind_sur(ntot,10),year,day,hour,mins,ind_no_data(inactive_cells),inactive_cells
    ! integer, intent(in)::  n_rvr_cell,rvr_cell_ind(n_rvr_cell),niter,ras_out,freq_err!,ncol,nrow,ntot
    ! integer::cur_ind_sur(10),iter,lin_ind,i,n_sur_cells,ncol,nrow,ntot
    ! real*8,allocatable::wel(:)
    ! character(len=100),intent(in)::dir,rep_fnam
    ! character(len=200), intent(in)::ascii_data(6)
    ! character(len=100):: fout
    ! real*8,intent(in):: xllcorner,yllcorner

    ! common /dims/ncol,nrow,ntot


    ! write(*,*)'Draining water for day',year,day,hour,mins
    ! call cpu_time(time_start)
    ! cum_time=0; !cumulative time
    ! !step=0;
    ! derr=10000
    ! verr=1000000;   !volume error
    ! wdepth=wl_lin-dem_lin
    ! wl_old=wl_lin;
    ! v_out_old=0
    ! v_out=0

    ! do iter=1,niter

        ! dt=time_step*100;   !to initialize dt with a very high number
        ! vel=0;  !initialize velocity array


        ! do i=1,active_cells !loop through active cells only

            ! lin_ind=rnk_ind(i)  !linear indexing for current cell with higher

            ! if (wl_lin(lin_ind) .eq. dem_lin(lin_ind) .or. wl_lin(lin_ind) .eq. no_data)then
                ! goto 77                ! to continue to the next cell if the cell was dry or if it is -9999
            ! endif

            ! wel0=wl_lin(lin_ind);   !water surface elevation in the central cell (current cell)
            ! !cur_ind_sur(:)
            ! cur_ind_sur(:)= ind_sur(lin_ind,:);
            ! n_sur_cells=cur_ind_sur(2)

            ! allocate(wel(n_sur_cells))
            ! wel= wl_lin(cur_ind_sur(3:3+n_sur_cells-1));

            ! !min_algorithm call
            ! call min_algorithm(wel0, wel, cur_ind_sur,n_sur_cells, wl_lin, dem_lin, mann, vel, cell_size,lin_ind,time_step)

            ! deallocate(wel)

    ! 77  end do!for i=1,active cells


        ! if(time_step>=1e20) then
            ! cum_time=0
        ! else
            ! dt=cell_size/vel
            ! cum_time=cum_time+minval(dt)
        ! end if

        ! ! draining all water in the outflow cells and store them in outflow array
        ! !!!!--------------------------------------------------------------
        ! wdepth=wl_lin-dem_lin
        ! v_out=v_out+(sum(wdepth(rvr_cell_ind))*(cell_size**2.0)) !volume in m3
        ! wdepth(rvr_cell_ind)=0; wl_lin(rvr_cell_ind)=dem_lin(rvr_cell_ind)
        ! !!!!--------------------------------------------------------------


        ! if (cum_time>=time_step .and. niter > 1)then  !sec

            ! derr=maxval(abs(wl_lin-wl_old))
            ! verr=v_out-v_out_old

            ! write(*,111) year,day,hour,mins,iter,derr,verr,cum_time
            ! 111 FORMAT (i6,i14,f17.4,f17.4,f17.4)
            ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
            ! write(2,111) year,day,hour,mins,iter,derr,verr,cum_time
            ! close(2)
            ! goto 66
        ! end if

        ! if (mod(iter,freq_err) .eq. 0)then

            ! derr=maxval(abs(wl_lin-wl_old))
            ! wl_old=wl_lin
            ! verr=v_out-v_out_old
            ! v_out_old=v_out

            ! !write output to screen and report
            ! write(*,'(i6,i14,f17.4,f17.4,f17.4)')year,day,hour,mins,iter,derr,verr,cum_time  !write every 100 iterations
            ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
            ! write(2,'(i6,i14,f17.4,f17.4,f17.4)')year,day,hour,mins,iter,derr,verr,cum_time  !write every 100 iterations
            ! close(2)

            ! if (derr<=error_thr .or. verr <= vol_thr)  goto 66
        ! end if


    ! end do !for iter loop

    ! !write(*,*)'Max number of iterations reached'
    ! !write(*,111) year,day,hour,mins,niter,derr,verr,cum_time
    ! !write(*,*)'outflow',outflow
    ! if(niter > 1)then 
        ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
        ! write(2,*)'Max number of iterations reached'
        ! write(2,111) year,day,hour,mins,niter,derr,verr,cum_time
        ! close(2)
    ! end if

    ! 66 write(fout,222)'day_',year,day,hour,mins,'_drained_wdepth.asc'		!water depth at each day
    ! 222 FORMAT (a4,i4.4,a19)
    ! !specifying the full name of the file.
    ! if(ras_out .eq. 1 .and. niter > 1)then
        ! call write_ascii(wdepth,no_data,xllcorner,yllcorner,cell_size,ind_no_data,active_cells,rnk_ind,&
                            ! & ascii_data,fout,dir, inactive_cells)

    ! end if

    ! call calc_stat(wdepth,no_data,cell_size,ind_no_data,active_cells,inactive_cells,rnk_ind,rep_fnam,v_out,&
                    ! & frac_wet_area, mean_dep,max_dep,drain_depth,niter)

    ! outflow=v_out !outflow volume m3

    ! call cpu_time(time_end)
    ! !write(*,*)'PRIMA run time', (time_end-time_start)/60.0,'min'
    ! if(niter > 1) then
        ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append') !create report empty file
        ! write(2,*)'PRIMA run time', (time_end-time_start)/60.0,'min'
        ! close(2) !report file
    ! end if

    ! return

! end subroutine drain_water

!******************************************************************************************************************!


!Add and Drain water at the same time subroutine
!needs to be updated
!Copied from the drain subroutine but used to allow for both adding ad draining at the same time

subroutine add_drain_water(time_step,error_thr,dt,vel,wl_lin,active_cells,rnk_ind,dem_lin,no_data,ind_sur,year,day,hour,mins,mann,&
                    & cell_size,dir,ascii_data,xllcorner,yllcorner,rep_fnam,ind_no_data,inactive_cells,outflow,&
                    & rvr_cell_ind,n_rvr_cell,vol_thr,niter,ras_out,freq_err,frac_wet_area, mean_dep,max_dep,drain_depth)

    implicit none

    real, intent(inout)::dt(ntot),vel(ntot),outflow, frac_wet_area, mean_dep,max_dep,drain_depth
    real*8, intent(inout)::wl_lin(ntot)
    real, intent(in):: no_data, mann(ntot),cell_size,error_thr,time_step,vol_thr
    real*8, intent(in):: dem_lin(ntot)
    real*8::wl_old(ntot),wdepth(ntot),wel0     !,wdepth_out_2d(nrow,ncol)
    real::cum_time,derr,v_out,v_out_old,verr,time_start,time_end
    integer, intent(in):: active_cells,rnk_ind(ntot),ind_sur(ntot,10),year,day,hour,mins,ind_no_data(inactive_cells),inactive_cells
    integer, intent(in)::  n_rvr_cell,rvr_cell_ind(n_rvr_cell),niter,ras_out,freq_err!,ncol,nrow,ntot
    integer::cur_ind_sur(10),iter,lin_ind,i,n_sur_cells,ncol,nrow,ntot
    real*8,allocatable::wel(:)
    character(len=100),intent(in)::dir,rep_fnam
    character(len=200), intent(in)::ascii_data(6)
    character(len=100):: fout
    real*8,intent(in):: xllcorner,yllcorner

    common /dims/ncol,nrow,ntot

    !if(niter > 1) this was added to prevent PRIMA from printing on screen when running in dummy mode (i.e., when timestep is not the end of day)
    !if(niter > 1) write(*,*)'Adding and Draining water at the same time for day',year,day,hour,mins
    call cpu_time(time_start)
    cum_time=0; !cumulative time
    !step=0;
    derr=10000
    verr=1000000;   !volume error
    wdepth=wl_lin-dem_lin
    wl_old=wl_lin;
    v_out_old=0
    v_out=0

    do iter=1,niter
        dt=time_step*100;   !to initialize dt with a very high number
        vel=0;  !initialize velocity array


        do i=1,active_cells !loop through active cells only

            lin_ind=rnk_ind(i)  !linear indexing for current cell with higher

            if (wl_lin(lin_ind) .eq. dem_lin(lin_ind) .or. wl_lin(lin_ind) .eq. no_data)then
                goto 77                ! to continue to the next cell if the cell was dry or if it is -9999
            endif

            wel0=wl_lin(lin_ind);   !water surface elevation in the central cell (current cell)
            !cur_ind_sur(:)
            cur_ind_sur(:)= ind_sur(lin_ind,:);
            n_sur_cells=cur_ind_sur(2)

            allocate(wel(n_sur_cells))
            wel= wl_lin(cur_ind_sur(3:3+n_sur_cells-1));

            !min_algorithm call
            call min_algorithm(wel0, wel, cur_ind_sur,n_sur_cells, wl_lin, dem_lin, mann, vel, cell_size,lin_ind,time_step)

            deallocate(wel)

    77  end do!for i=1,active cells


        if(time_step>=1e20) then
            cum_time=0
        else
!mesh: wrapper in where to prevent divide by zero
            where(vel>0)
                dt=cell_size/vel
            elsewhere
                dt=0
            end where
!mesh: added mask
            cum_time=cum_time+minval(dt,dt>0)
        end if

        ! draining all water in the outflow cells and store them in outflow array
        !!!!--------------------------------------------------------------
        wdepth=wl_lin-dem_lin
        v_out=v_out+(sum(wdepth(rvr_cell_ind))*(cell_size**2.0)) !volume in m3
        wdepth(rvr_cell_ind)=0; wl_lin(rvr_cell_ind)=dem_lin(rvr_cell_ind)
        !!!!--------------------------------------------------------------


        if (cum_time>=time_step .and. niter > 1)then  !sec

            derr=maxval(abs(wl_lin-wl_old))
            verr=v_out-v_out_old

            write(*,111) 'PRIMA ',year,day,iter,derr,verr,cum_time
            111 FORMAT (a6,i4,i3,i14,f17.4,f17.4,f17.4)
            ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
            ! write(2,111) year,day,hour,mins,iter,derr,verr,cum_time
            ! close(2)
            goto 66
        end if

        if (mod(iter,freq_err) .eq. 0)then

            derr=maxval(abs(wl_lin-wl_old))
            wl_old=wl_lin
            verr=v_out-v_out_old
            v_out_old=v_out

            !write output to screen and report
            write(*,'(a6,i4,i3,i14,f17.4,f17.4,f17.4)')'PRIMA ',year,day,iter,derr,verr,cum_time  !write every 100 iterations
            ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
            ! write(2,'(i6,i14,f17.4,f17.4,f17.4)')year,day,hour,mins,iter,derr,verr,cum_time  !write every 100 iterations
            ! close(2)

            if (derr<=error_thr .or. verr <= vol_thr)  goto 66
        end if



    end do !for iter loop

    !write(*,*)'Max number of iterations reached'
    !write(*,111) year,day,hour,mins,niter,derr,verr,cum_time
    !write(*,*)'outflow',outflow
    ! if(niter > 1)then 
        ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
        ! write(2,*)'Max number of iterations reached'
        ! write(2,111) year,day,hour,mins,niter,derr,verr,cum_time
        ! close(2)
    ! end if

    66 write(fout,222)year,day,hour,mins,'_add_drain_wdepth.asc'		!water depth at each day
    222 FORMAT (i4.4,'_',i3.3,'_',i2.2,'_',i2.2,a21)
    !specifying the full name of the file.
    if(ras_out .eq. 1 .and. niter > 1)then !write raster file after convergence
        call write_ascii(wdepth,no_data,xllcorner,yllcorner,cell_size,ind_no_data,active_cells,rnk_ind,&
                            & ascii_data,fout,dir, inactive_cells)
    end if

    call calc_stat(wdepth,no_data,cell_size,ind_no_data,active_cells,inactive_cells,rnk_ind,rep_fnam,v_out,&
                    & frac_wet_area, mean_dep,max_dep,drain_depth,niter)

    outflow=v_out !outflow volume m3

    call cpu_time(time_end)
    !write(*,*)'PRIMA run time', (time_end-time_start)/60.0,'min'
    ! if(niter > 1) then
        ! open(2,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append') !create report empty file
        ! write(2,*)'PRIMA run time', (time_end-time_start)/60.0,'min'
        ! close(2) !report file
    ! endif

    return

end subroutine add_drain_water


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine write_ascii(wdepth,no_data,xllcorner,yllcorner,cell_size,ind_no_data,active_cells,rnk_ind,&
                        & ascii_data,fiter,dir, inactive_cells)

    implicit none
    !write the resulting water depth in *.asc raster format
    real*8,intent(in):: wdepth(ntot)
    real,intent(in):: no_data, cell_size
    real*8:: wdepth_out_2d(nrow,ncol),wdepth_dum(ntot)
    character(len=100),intent(in):: dir,fiter
    character(len=100):: full_fnam
    character(len=200),intent(in)::ascii_data(6)
    real*8,intent(in):: xllcorner,yllcorner
    integer, intent(in):: ind_no_data(inactive_cells),active_cells,inactive_cells,rnk_ind(ntot)
    integer::ncol,nrow,ntot,i,ii,lin_ind
    common /dims/ncol,nrow,ntot
    !write(*,*) 'Writing Ascii FIle'
    wdepth_dum=wdepth

    !specifying the full name of the file
    full_fnam=trim(dir)// '/' // fiter

    wdepth_dum(ind_no_data)=no_data
    !reshape wdepth_dum to be 2D array
    wdepth_out_2d=reshape(wdepth_dum,(/nrow,ncol/))

    open(1,file=TRIM(ADJUSTL(full_fnam)),status='unknown')
    write(1,'(a14,i4)')ascii_data(1),ncol
    write(1,'(a14,i4)')ascii_data(2),nrow
    write(1,'(a13,f16.7)')ascii_data(3),xllcorner
    write(1,'(a14,f16.7)')ascii_data(4),yllcorner
    write(1,'(a8,f16.7)')ascii_data(5),cell_size
    write(1,'(a15,f7.1)')ascii_data(6),no_data
    do ii=1,nrow
        write(1,'(*(f13.6,X))') wdepth_out_2d(ii,:)
        !write(1,*) wdepth_out_2d(ii,:)
    end do
    close(1)

    return
end subroutine write_ascii


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine calc_stat(wdepth,no_data,cell_size,ind_no_data,active_cells,inactive_cells,rnk_ind,rep_fnam,outflow,&
                        & frac_wet_area, mean_dep,max_dep,drain_depth,niter)

    !calculate statistics (e.g., mean ponded depth, max ponded depth, fractional water area, etc.)
    implicit none

    real*8,intent(in):: wdepth(ntot)
    real,intent(in):: no_data, cell_size,outflow
    real*8:: wdepth_out_2d(nrow,ncol),wdepth_dum(ntot)
    logical::mask(ntot)
    character(len=100),intent(in):: rep_fnam
    integer, intent(in):: ind_no_data(inactive_cells),active_cells,inactive_cells,rnk_ind(ntot),niter
    real::max_dep,mean_dep,frac_wet_area,drain_depth
    integer::ncol,nrow,ntot,i,ii,lin_ind,n_wet_cells
    common /dims/ncol,nrow,ntot

    wdepth_dum=wdepth
    mask=wdepth .gt. 0.001
    n_wet_cells = count(mask)


    !where(wdepth_dum <= 0.000) wdepth_dum=0 ! to remove possible cells with negative value
    max_dep=maxval(wdepth_dum)*1000.0 !to convert from m to mm
    mean_dep=(sum(wdepth_dum)/active_cells)*1000.0 !to convert from m to mm
    frac_wet_area=real(n_wet_cells)/real(active_cells)
    drain_depth=0
    if(outflow>0)then
        drain_depth=((outflow/(cell_size**2))/real(active_cells))*1000.0 !to convert from m to mm
    end if
    ! if(niter > 1) then
        ! open(1,file=TRIM(ADJUSTL(rep_fnam)),status='unknown',access = 'append')
        ! write(1,'(a32,f5.3)')'Fractional water covered area ',frac_wet_area
        ! write(1,'(a24,f9.3,a4)')'Mean ponded water depth ',mean_dep,' mm'
        ! write(1,'(a23,f9.3,a4)')'Max ponded water depth ',max_dep,' mm'
        ! if(outflow>0)then
            ! write(1,'(a38,f16.3,a3)')'Drained (runoff/outflow) water volume ',outflow,' m3'
            ! write(1,'(a38,f9.4,a4)')'Drained (runoff/outflow) water depth ',drain_depth,' mm'
        ! end if
        ! close(1)
    ! end if

end subroutine calc_stat

end module main_PRIMA
