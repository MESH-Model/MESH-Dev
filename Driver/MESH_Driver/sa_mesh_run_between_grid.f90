module sa_mesh_run_between_grid

    implicit none

    contains

    subroutine run_between_grid_config(shd, ts, ic, stfl)

        use sa_mesh_shared_variabletypes
        use sa_mesh_shared_variables
        use model_dates
        use model_output_variabletypes

        use process_SA_RTE, only: configure_SA_RTE
        use process_WF_ROUTE_config, only: config_WF_ROUTE

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic
        type(streamflow_hydrograph) :: stfl

!todo: switch
        call configure_SA_RTE(shd, ic)
        call config_WF_ROUTE(shd, ic, stfl)

    end subroutine

    subroutine run_between_grid(shd, ts, ic, cm, wb, eb, sov, stfl, &
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
        use process_WF_ROUTE, only: run_WF_ROUTE

        type(ShedGridParams), intent(in) :: shd
        type(dates_model) :: ts
        type(iter_counter), intent(in) :: ic
        type(clim_info), intent(in) :: cm
        type(water_balance) :: wb
        type(energy_balance) :: eb
        type(soil_statevars) :: sov
        type(streamflow_hydrograph) :: stfl

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
        call run_WF_ROUTE(shd, ic, wb, stfl, &
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
