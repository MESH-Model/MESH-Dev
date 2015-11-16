module process_WF_ROUTE

    use process_WF_ROUTE_config

    implicit none

    contains

    subroutine run_WF_ROUTE(shd, ic, wb, &
                            WF_ROUTETIMESTEP, WF_R1, WF_R2, &
                            WF_NO, WF_NL, WF_MHRD, WF_KT, WF_IY, WF_JX, &
                            WF_QHYD, WF_RES, WF_RESSTORE, WF_NORESV_CTRL, WF_R, &
                            WF_NORESV, WF_NREL, WF_KTR, WF_IRES, WF_JRES, WF_RESNAME, &
                            WF_B1, WF_B2, WF_QREL, WF_QR, &
                            WF_TIMECOUNT, WF_NHYD, WF_QBASE, WF_QI1, WF_QI2, WF_QO1, WF_QO2, &
                            WF_STORE1, WF_STORE2, &
                            M_C, M_R, M_S, &
                            WF_S, JAN, &
                            WF_QSYN, WF_QSYN_AVG, WF_QSYN_CUM, WF_QHYD_AVG, WF_QHYD_CUM)

        use sa_mesh_shared_variabletypes
        use model_dates
        use MODEL_OUTPUT

        type(ShedGridParams), intent(in) :: shd
        type(iter_counter), intent(in) :: ic
        type(water_balance), intent(in) :: wb

        integer M_S, M_R
        integer, intent(in) :: M_C
        integer WF_NO, WF_NL, WF_MHRD, WF_KT
        integer, dimension(:), allocatable :: WF_IY, WF_JX, WF_S
        real, dimension(:), allocatable :: WF_QHYD, WF_QHYD_AVG, WF_QHYD_CUM
        real, dimension(:), allocatable :: WF_QSYN, WF_QSYN_AVG, WF_QSYN_CUM
        character(8), dimension(:), allocatable :: WF_GAGE
        integer, dimension(:), allocatable :: WF_IRES, WF_JRES, WF_RES, WF_R
        real, dimension(:), allocatable :: WF_B1, WF_B2, WF_QREL, WF_RESSTORE
        character(8), dimension(:), allocatable :: WF_RESNAME
        integer JAN
        real WF_R1(M_C), WF_R2(M_C)
        real, dimension(:), allocatable :: WF_NHYD, WF_QBASE, WF_QI2, &
            WF_QO1, WF_QO2, WF_QR, WF_STORE1, WF_STORE2, WF_QI1
        integer WF_NORESV, WF_NREL, WF_KTR, WF_NORESV_CTRL
        integer WF_ROUTETIMESTEP, WF_TIMECOUNT, DRIVERTIMESTEP

        !> Local variables.
        integer i
        logical writeout

        if (.not. WF_RTE_flgs%PROCESS_ACTIVE) return

        call WF_ROUTE(WF_ROUTETIMESTEP, WF_R1, WF_R2, &
                      shd%NA, shd%NAA, shd%lc%NTYPE, shd%yCount, shd%xCount, shd%iyMin, &
                      shd%iyMax, shd%jxMin, shd%jxMax, shd%yyy, shd%xxx, shd%IAK, shd%IROUGH, &
                      shd%ICHNL, shd%NEXT, shd%IREACH, shd%AL, shd%GRDN, shd%GRDE, &
                      shd%DA, shd%BNKFLL, shd%SLOPE_CHNL, shd%ELEV, shd%FRAC, &
                      WF_NO, WF_NL, WF_MHRD, WF_KT, WF_IY, WF_JX, &
                      WF_QHYD, WF_RES, WF_RESSTORE, WF_NORESV_CTRL, WF_R, &
                      WF_NORESV, WF_NREL, WF_KTR, WF_IRES, WF_JRES, WF_RESNAME, &
                      WF_B1, WF_B2, WF_QREL, WF_QR, &
                      WF_TIMECOUNT, WF_NHYD, WF_QBASE, WF_QI1, WF_QI2, WF_QO1, WF_QO2, &
                      WF_STORE1, WF_STORE2, &
                      ic%dts, (wb%rof/ic%dts), shd%NA, M_C, M_R, M_S, shd%NA, &
                      WF_S, JAN, ic%now_jday, ic%now_hour, ic%now_mins)
        do i = 1, WF_NO
            WF_QSYN(i) = WF_QO2(WF_S(i))
            WF_QSYN_AVG(i) = WF_QSYN_AVG(i) + WF_QO2(WF_S(i))
            WF_QSYN_CUM(i) = WF_QSYN_CUM(i) + WF_QO2(WF_S(i))
            WF_QHYD_AVG(i) = WF_QHYD(i) !(MAM)THIS SEEMS WORKING OKAY (AS IS THE CASE IN THE READING) FOR A DAILY STREAM FLOW DATA.
        end do

        !> this is done so that INIT_STORE is not recalculated for
        !> each iteration when wf_route is not used
        if (JAN == 1) then
            JAN = 2
        end if

        !> *********************************************************************
        !> Write measured and simulated streamflow to file and screen
        !> Also write daily summary (pre, evap, rof)
        !> *********************************************************************

        !> Write output for hourly streamflow.
        if (STREAMFLOWFLAG == 1 .and. STREAMFLOWOUTFLAG >= 2) then
            write(WF_RTE_fls%fl(WF_RTE_flks%stfl_ts)%iun, 1002) &
                ic%now_jday, ic%now_hour, ic%now_mins, (WF_QHYD(i), WF_QSYN(i), i = 1, WF_NO)
        end if

        !> Determine if this is the last time-step of the hour.
        writeout = (mod(ic%ts_daily, 3600/ic%dts*24) == 0)
!        print *, ic%now_jday, ic%now_hour, ic%now_mins, writeout

        !> This occurs the last time-step of the day.
        if (writeout) then

            do i = 1, WF_NO
                WF_QHYD_CUM(i) = WF_QHYD_CUM(i) + WF_QHYD_AVG(i)
            end do

            !> Write output for daily and cumulative daily streamflow.
            if (STREAMFLOWOUTFLAG > 0) then
                write(WF_RTE_fls%fl(WF_RTE_flks%stfl_daily)%iun, 1001) &
                    ic%now_jday, (WF_QHYD_AVG(i), WF_QSYN_AVG(i)/ic%ts_daily, i = 1, WF_NO)
                if (STREAMFLOWOUTFLAG >= 2) then
                    write(WF_RTE_fls%fl(WF_RTE_flks%stfl_cumm)%iun, 1001) &
                        ic%now_jday, (WF_QHYD_CUM(i), WF_QSYN_CUM(i)/ic%ts_daily, i = 1, WF_NO)
                end if
            end if

            WF_QSYN_AVG = 0.0

        end if !(writeout) then

1001    format(1(i5, ','), f10.3, 9999(',', f10.3))
1002    format(3(i5, ','), f10.3, 9999(',', f10.3))

    end subroutine

end module
