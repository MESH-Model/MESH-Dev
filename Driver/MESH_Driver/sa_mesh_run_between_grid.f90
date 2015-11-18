module sa_mesh_run_between_grid

    implicit none

    contains

    subroutine run_between_grid_ini(shd, ts, ic, stfl, rrls, &
                                LOCATIONFLAG, STREAMFLOWOUTFLAG, &
                                M_R, M_S, &
                                WF_NHYD, &
                                WF_QBASE, WF_QI2, WF_QO1, WF_QO2, &
                                WF_STORE1, WF_STORE2, WF_QI1, WF_QR, &
                                WF_NORESV, WF_NREL, WF_KTR, &
                                WF_NORESV_CTRL, &
                                WF_IRES, WF_JRES, WF_RES, WF_R, WF_B1, WF_B2, &
                                WF_QREL, WF_RESSTORE, WF_RESNAME, &
                                I_G, J_G, &
                                WF_IY, &
                                WF_NO, WF_NL, WF_MHRD, WF_KT, &
                                WF_JX, WF_S, WF_QHYD, WF_QHYD_AVG, WF_QHYD_CUM, &
                                WF_QSYN, WF_QSYN_AVG, WF_QSYN_CUM, WF_GAGE, &
                                WF_ROUTETIMESTEP, WF_TIMECOUNT, &
                                JAN, &
                                WF_START_YEAR, WF_START_DAY, WF_START_HOUR, JDAY_IND1, &
                                JDAY_IND2, &
                                JDAY_IND_STRM, &
                                GENDIR_OUT)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use model_output_variabletypes

        use process_SA_RTE, only: configure_SA_RTE
        use process_WF_ROUTE_config, only: run_WF_ROUTE_ini

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

        !> Temporary variables.
        integer LOCATIONFLAG, STREAMFLOWOUTFLAG
        integer M_S, M_R
        integer WF_NO, WF_NL, WF_MHRD, WF_KT
        integer, dimension(:), allocatable :: WF_IY, WF_JX, WF_S
        real, dimension(:), allocatable :: WF_QHYD, WF_QHYD_AVG, WF_QHYD_CUM
        real, dimension(:), allocatable :: WF_QSYN, WF_QSYN_AVG, WF_QSYN_CUM
        character(8), dimension(:), allocatable :: WF_GAGE
        integer, dimension(:), allocatable :: WF_IRES, WF_JRES, WF_RES, WF_R
        real, dimension(:), allocatable :: WF_B1, WF_B2, WF_QREL, WF_RESSTORE
        character(8), dimension(:), allocatable :: WF_RESNAME
        integer JAN
        real, dimension(:), allocatable :: WF_NHYD, WF_QBASE, WF_QI2, &
            WF_QO1, WF_QO2, WF_QR, WF_STORE1, WF_STORE2, WF_QI1
        integer WF_NORESV, WF_NREL, WF_KTR, WF_NORESV_CTRL
        integer WF_ROUTETIMESTEP, WF_TIMECOUNT
        character(450) GENDIR_OUT

        !> Local variables.
        !* WF_START_YEAR OBSERVED STREAMFLOW START YEAR
        !* WF_START_DAY OBSERVED STREAMFLOW START DAY
        !* WF_START_HOUR OBSERVED STREAMFLOW START HOUR
        integer WF_START_YEAR, WF_START_DAY, WF_START_HOUR
        integer JDAY_IND_STRM, JDAY_IND1, JDAY_IND2
        real I_G, J_G

!todo: switch
        call configure_SA_RTE(shd, ic)
        call run_WF_ROUTE_ini(shd, ic, stfl, rrls, &
                              LOCATIONFLAG, STREAMFLOWOUTFLAG, &
                              M_R, M_S, &
                              WF_NHYD, &
                              WF_QBASE, WF_QI2, WF_QO1, WF_QO2, &
                              WF_STORE1, WF_STORE2, WF_QI1, WF_QR, &
                              WF_NORESV, WF_NREL, WF_KTR, &
                              WF_NORESV_CTRL, &
                              WF_IRES, WF_JRES, WF_RES, WF_R, WF_B1, WF_B2, &
                              WF_QREL, WF_RESSTORE, WF_RESNAME, &
                              I_G, J_G, &
                              WF_IY, &
                              WF_NO, WF_NL, WF_MHRD, WF_KT, &
                              WF_JX, WF_S, WF_QHYD, WF_QHYD_AVG, WF_QHYD_CUM, &
                              WF_QSYN, WF_QSYN_AVG, WF_QSYN_CUM, WF_GAGE, &
                              WF_ROUTETIMESTEP, WF_TIMECOUNT, &
                              JAN, &
                              WF_START_YEAR, WF_START_DAY, WF_START_HOUR, JDAY_IND1, &
                              JDAY_IND2, &
                              JDAY_IND_STRM, &
                              GENDIR_OUT)

    end subroutine

    subroutine run_between_grid(shd, ts, ic, cm, wb, eg, sp, stfl, rrls, &
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
        use sa_mesh_shared_variables
        use model_dates
        use climate_forcing
        use MODEL_OUTPUT
        use model_output_variabletypes

        use process_SA_RTE, only: run_SA_RTE
        use process_WF_ROUTE, only: run_WF_ROUTE_between_grid

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic
        type(clim_info), intent(in) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eg
        type(soil_statevars) :: sp
        type(streamflow_hydrograph) :: stfl
        type(reservoir_release) :: rrls

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

!todo: Switch
        call run_SA_RTE(shd, ic, wb)
        call run_WF_ROUTE_between_grid(shd, ic, wb, stfl, rrls, &
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

    end subroutine

end module
