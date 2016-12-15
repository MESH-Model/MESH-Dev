module WF_ROUTE_module

    use WF_ROUTE_config

    implicit none

    private

    public WF_ROUTE_within_tile, WF_ROUTE_between_grid

    contains

    function WF_ROUTE_within_tile(shd, stfl, rrls)

        use mpi_shared_variables
        use sa_mesh_shared_variables
        use model_dates
        use MODEL_OUTPUT
        use model_output_variabletypes

        character(100) WF_ROUTE_within_tile

        type(ShedGridParams), intent(in) :: shd
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Local variables.
        integer i, ierr

        WF_ROUTE_within_tile = ''

        !> WF_ROUTE only runs in serial. If ipid /= 0 then the model is
        !> likely running in parallel. This subroutine returns if ipid
        !> of the current process /= 0 or if the process has been marked
        !> inactive.
        if (.not. WF_RTE_flgs%PROCESS_ACTIVE .or. ipid /= 0) return

        !> *************************************************************
        !> Read in current reservoir release value
        !> *************************************************************

        !> only read in current value if we are on the correct time step
        !> however put in an exception if this is the first time through (ie. jan = 1),
        !> otherwise depending on the hour of the first time step
        !> there might not be any data in wf_qrel, wf_qhyd
        !> make sure we have a controlled reservoir (if not the mod(HOUR_NOW, wf_ktr)
        !> may give an error. Frank S Jun 2007
        if (WF_NORESV_CTRL > 0) then
            if (mod(ic%now%hour, WF_KTR) == 0 .and. ic%now%mins == 0) then
            !>        READ in current reservoir value
                read(21, '(100f10.3)', iostat = ierr) (WF_QREL(i), i = 1, WF_NORESV_CTRL)
                if (ierr /= 0) then
                    WF_ROUTE_within_tile = 'ran out of reservoir data before met data'
                    return
                end if
            else
                if (JAN == 1 .and. WF_NORESV_CTRL > 0) then
                    read(21, '(100f10.3)', iostat = ierr) (WF_QREL(i), i = 1, WF_NORESV_CTRL)
                    rewind 21
                    read(21, *)
                    do i = 1, fms%rsvr%n
                        read(21, *)
                    end do
                end if
            end if
        end if

        ! **************************************************************
        !> Read in current streamflow value
        !> *************************************************************

        !> only read in current value if we are on the correct time step
        !> also read in the first value if this is the first time through
        if (mod(ic%now%hour, WF_KT) == 0 .and. ic%now%mins == 0) then
            !>       read in current streamflow value
            read(22, *, iostat = ierr) (WF_QHYD(i), i = 1, fms%stmg%n)
            if (ierr /= 0) then
!-                WF_ROUTE_within_tile = 'ran out of streamflow data before met data'
!-                return
                WF_QHYD = WF_NODATA_VALUE
            end if
        end if

        return

    end function

    subroutine WF_ROUTE_between_grid(shd, wb, stfl, rrls)

        use sa_mesh_shared_variables
        use model_dates
        use MODEL_OUTPUT
        use model_output_variabletypes

        type(ShedGridParams), intent(in) :: shd
        type(water_balance), intent(in) :: wb
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Temporary variables.
!-        integer M_C
!-        real WF_R1(M_C), WF_R2(M_C)

        !> Local variables.
        integer l, i, iun
        logical writeout

        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

        if (ic%ts_daily == 1) then
            WF_QSYN_AVG = 0.0
        end if

        !> shd%NAA is the total number of grids.
        !> shd%NA is the total number of grids in the basin.
        !> WF_NAA is the number of outlets in the basin (e.g., shd%NA - shd%NAA).
        call WF_ROUTE(WF_ROUTETIMESTEP, wfp%r1, wfp%r2, &
                      shd%NA, WF_NAA, shd%lc%NTYPE, shd%yCount, shd%xCount, shd%iyMin, &
                      shd%iyMax, shd%jxMin, shd%jxMax, shd%yyy, shd%xxx, shd%IAK, shd%IROUGH, &
                      shd%ICHNL, shd%NEXT, shd%IREACH, shd%AL, shd%GRDN, shd%GRDE, &
                      shd%DA, shd%BNKFLL, shd%SLOPE_CHNL, shd%ELEV, shd%FRAC, &
                      shd%CHNL_LEN, &
                      WF_RTE_flgs%RLFLAG, WF_RTE_flgs%CAPFLAG, &
                      fms%stmg%n, WF_NL, WF_MHRD, WF_KT, fms%stmg%iy, fms%stmg%jx, &
                      WF_QHYD, WF_RES, WF_RESSTORE, WF_NORESV_CTRL, fms%rsvr%rnk, &
                      fms%rsvr%n, WF_NREL, WF_KTR, fms%rsvr%iy, fms%rsvr%jx, fms%rsvr%name, &
                      WF_B1, WF_B2, WF_B3, WF_B4, WF_B5, WF_QREL, WF_QR, &
                      WF_TIMECOUNT, WF_NHYD, WF_QBASE, stas%chnl%qi, WF_QI2, WF_QO1, stas%chnl%qo, &
                      wfp%aa1, wfp%aa2, wfp%aa3, wfp%aa4, &
                      WF_STORE1, stas%chnl%s, &
                      ic%dts, (wb%rof/ic%dts), shd%NA, shd%NRVR, fms%rsvr%n, fms%stmg%n, shd%NA, &
                      fms%stmg%rnk, JAN, ic%now%jday, ic%now%hour, ic%now%mins)
        do i = 1, fms%stmg%n
            WF_QSYN(i) = stas%chnl%qo(fms%stmg%rnk(i))
            WF_QSYN_AVG(i) = WF_QSYN_AVG(i) + stas%chnl%qo(fms%stmg%rnk(i))
            WF_QSYN_CUM(i) = WF_QSYN_CUM(i) + stas%chnl%qo(fms%stmg%rnk(i))
            WF_QHYD_AVG(i) = WF_QHYD(i) !(MAM)THIS SEEMS WORKING OKAY (AS IS THE CASE IN THE READING) FOR A DAILY STREAM FLOW DATA.
        end do
        where (shd%DA > 0.0)
            WF_QO2_ACC_MM = WF_QO2_ACC_MM + stas%chnl%qo/shd%DA/1000.0*ic%dts
            WF_STORE2_ACC_MM = WF_STORE2_ACC_MM + stas%chnl%s/shd%DA/1000.0
        end where

        !> Update state variables for the driver.
        do i = 1, fms%rsvr%n
            stas%rsvr%qi(i) = stas%chnl%qi(fms%rsvr%rnk(i))
            stas%rsvr%qo(i) = stas%chnl%qo(fms%rsvr%rnk(i))
            stas%rsvr%s(i) = stas%chnl%s(fms%rsvr%rnk(i))
        end do

        !> this is done so that INIT_STORE is not recalculated for
        !> each iteration when wf_route is not used
        if (JAN == 1) then
            JAN = 2
        end if

        !> *********************************************************************
        !> Write output to file.
        !> *********************************************************************

!-        do l = 1, fms%rsvr%n
!-            i = fms%rsvr%rnk(l)
!-            write(708+l,"(2(I6,','),7(G12.5,','))") l, i, &
!-                stas%chnl%qi(i), wf_store1(i), wf_qi2(i), stas%chnl%s(i), stas%chnl%qo(i)
!-        end do

        !> Write per time-step output for reaches.
        if (btest(WF_RTE_frsvrout%freq, WF_RTE_frsvrout%KTS)) then
            do l = 1, fms%rsvr%n
                iun = WF_RTE_frsvrout%fls%fl(WF_RTE_frsvrout%KTS)%iun + l
                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins
                i = fms%rsvr%rnk(l)
                write(iun, 1010, advance = 'no') l, i, stas%chnl%qi(i), wf_store1(i), wf_qi2(i), stas%chnl%s(i), stas%chnl%qo(i)
                write(iun, *)
            end do
        end if

        !> Write per time-step output for streamflow.
        if (btest(WF_RTE_fstflout%freq, WF_RTE_fstflout%KTS)) then
            iun = WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KTS)%iun
            write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday, ic%now%hour, ic%now%mins
            do i = 1, fms%stmg%n
!todo
                if (WF_RTE_fstflout%fout_acc) write(iun, 1010, advance = 'no') WF_NODATA_VALUE, WF_NODATA_VALUE
                if (WF_RTE_fstflout%fout_hyd) write(iun, 1010, advance = 'no') WF_QHYD(i), WF_QSYN(i)
!todo
                if (WF_RTE_fstflout%fout_bal) write(iun, 1010, advance = 'no') WF_NODATA_VALUE, WF_NODATA_VALUE
            end do
            write(iun, *)
        end if

        !> Determine if this is the last time-step of the hour.
        writeout = (mod(ic%ts_daily, 3600/ic%dts*24) == 0)
!        print *, ic%now%jday, ic%now%hour, ic%now%mins, writeout

        !> This occurs the last time-step of the day.
        if (writeout) then

            do i = 1, fms%stmg%n
                WF_QHYD_CUM(i) = WF_QHYD_CUM(i) + WF_QHYD_AVG(i)
            end do

            !> Write daily output for streamflow.
            if (btest(WF_RTE_fstflout%freq, WF_RTE_fstflout%KDLY)) then
                iun = WF_RTE_fstflout%fls%fl(WF_RTE_fstflout%KDLY)%iun
                write(iun, 1010, advance = 'no') ic%now%year, ic%now%jday
                do i = 1, fms%stmg%n
                    if (WF_RTE_fstflout%fout_acc) write(iun, 1010, advance = 'no') WF_QHYD_CUM(i), WF_QSYN_CUM(i)/ic%ts_daily
                    if (WF_RTE_fstflout%fout_hyd) write(iun, 1010, advance = 'no') WF_QHYD_AVG(i), WF_QSYN_AVG(i)/ic%ts_daily
                    if (WF_RTE_fstflout%fout_bal) write(iun, 1010, advance = 'no') &
                        WF_QO2_ACC_MM(fms%stmg%rnk(i)), WF_STORE2_ACC_MM(fms%stmg%rnk(i))/ic%ts_count
                end do
                write(iun, *)
            end if

            !> Write output for streamflow channel water balance output file.
!-            if (btest(WF_RTE_flgs%STREAMFLOWOUTFLAG, WF_RTE_fstfloutks%KDLYBAL)) then
!-                write(WF_RTE_fouts%fl(WF_RTE_fstfloutks%KDLYBAL)%iun, *) &
!-                    ic%now%jday, (WF_QO2_ACC_MM(fms%stmg%rnk(i)), &
!-                                  WF_STORE2_ACC_MM(fms%stmg%rnk(i))/ic%ts_count, i = 1, fms%stmg%n)
!-            end if

            !> Write output for cumulative daily streamflow output file.
!-            if (btest(WF_RTE_flgs%STREAMFLOWOUTFLAG, WF_RTE_fstfloutks%KDLYACC)) then
!-                write(WF_RTE_fouts%fl(WF_RTE_fstfloutks%KDLYACC)%iun, *) &
!-                    ic%now%jday, (WF_QHYD_CUM(i), WF_QSYN_CUM(i)/ic%ts_daily, i = 1, fms%stmg%n)
!-            end if

!-            WF_QSYN_AVG = 0.0

            !> Assign to the output variables.
            stfl%qhyd = WF_QHYD_AVG
            stfl%qsyn = WF_QSYN_AVG/ic%ts_daily

        end if !(writeout) then

1010    format(9999(g15.7e2, ','))

    end subroutine

end module
