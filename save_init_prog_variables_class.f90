subroutine save_init_prog_variables_class( CMAIROW  , QACROW  , TACROW   , &
                                           TBASROW  , TSFSROW , WSNOROW  , &
                                           ALBSROW  , GROROW  , rncanROW  , &
                                           RHOSROW  , sncanROW , SNOROW   , &
                                           TBARROW  , TCANROW , THICROW  , &
                                           THLQROW  , TPNDROW , TSNOROW  , &
                                           ZPNDROW                       , &
                                           NA      , NTYPE    , IGND     )
!-----------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------
! AUTHOR : GONZALO SAPRIZA
! DATE CREATION : 2014-07-14
! DATES MODIFICATIONS : -
! DESCRIPTION : Save only the prognostic variables needed by class as initial
!               condtions.
! The variables readed are:
!1)  ALBSROW  - Snow albedo [] (cp%ALBSROW)
!2)  CMAIROW  - Aggregated mass of vegetation canopy [kg m-2]
!3)  GROROW   - Vegetation growth index [] (cp%GROROW)
!4)  QACROW   - Spec. Humidity of air within veget canopy space [kg kg-1]
!5)  rncanROW  - Intercepted liquid water sotred on canopy [kg m-2] (cp%rncanROW)
!6)  RHOSROW  - Density of snow [kg m-3] (cp%RHOSROW)
!7)  sncanROW  - Intercepted frozen water stored on canopy [kg m-2] (cp%sncanROW)
!8)  SNOROW   - Mass of snow pack [kg m-2] (cp%SNOROW)
!9)  TACROW   - Temp of air within veget canopy [K]
!10) TBARROW  - Temp of soil layers [k] (cp%TBARROW)
!11) TBASROW  - Temp of bedrock in third soil layer [K]
!12) TCANROW  - Temp veget canopy [K] (cp%TCANROW)
!13) THICROW  - Vol frozen water conetent of soil layers [m3 m-3] (cp%THICROW)
!14) THLQROW  - Vol liquid water conetent of soil layers [m3 m-3] (cp%THLQROW)
!15) TPNDROW  - Temp of ponded water [k] (cp%TPNDROW)
!16) TSFSROW  - Ground surf temp over subarea [K]
!17) TSNOROW  - Snowpack temp [K] (cp%TSNOROW)
!18) WSNOROW  - Liquid water content of snow pack [kg m-2]
!19) ZPNDROW  - Depth of ponded water on surface [m] (cp%ZPNDROW)
!-----------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------


    implicit none

    !Inputs
    integer :: NA, NTYPE, IGND

    real,dimension(NA,NTYPE)  :: CMAIROW  , QACROW  , TACROW  , &
                                 TBASROW  , WSNOROW , ALBSROW , &
                                 GROROW   , rncanROW , RHOSROW , &
                                 sncanROW  , SNOROW  , TCANROW , &
                                 TPNDROW  , TSNOROW , ZPNDROW

    real,dimension(NA, NTYPE, 4)  :: TSFSROW

    real,dimension(NA, NTYPE, IGND)  :: TBARROW , THICROW , THLQROW


    !Internals
    integer :: IOS

!--------------Main Subtrouine start-----------------------------------------------

    OPEN(UNIT   = 883                     , &
         FILE   = 'int_statVariables.seq' , &
         STATUS = 'replace'               , &
         FORM   = 'unformatted'           , &
         ACTION = 'write'                 , &
         ACCESS = 'sequential'            , &
         IOSTAT = IOS                     )

    write(883) albsrow   !1
    write(883) cmairow   !2
    write(883) grorow    !3
    write(883) qacrow    !4
    write(883) rncanrow   !5
    write(883) rhosrow   !6
    write(883) sncanrow   !7
    write(883) snorow    !8
    write(883) tacrow    !9
    write(883) tbarrow   !10
    write(883) tbasrow   !11
    write(883) tcanrow   !12
    write(883) thicrow   !13
    write(883) thlqrow   !14
    write(883) tpndrow   !15
    write(883) tsfsrow   !16
    write(883) tsnorow   !17
    write(883) wsnorow   !18
    write(883) zpndrow   !19

    close(883)

end subroutine save_init_prog_variables_class
