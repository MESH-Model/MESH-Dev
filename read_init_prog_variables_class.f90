subroutine read_init_prog_variables_class( CMAIROW  , QACROW  , TACROW   , &
                                           TBASROW  , TSFSROW , WSNOROW  , &
                                           cp       , NA      , NTYPE    , &
                                           IGND                          )
!>***************************************************************************************
!>***************************************************************************************
!> AUTHOR : GONZALO SAPRIZA
!> DATE CREATION : 2014-07-14
!> DATES MODIFICATIONS : -
!> DESCRIPTION : Read only the prognostic variables needed by class as initial
!>               condtions.
!> The variables readed are:
!>1)  ALBSROW  - Snow albedo [] (cp%ALBSROW)
!>2)  CMAIROW  - Aggregated mass of vegetation canopy [kg m-2]
!>3)  GROROW   - Vegetation growth index [] (cp%GROROW)
!>4)  QACROW   - Spec. Humidity of air within veget canopy space [kg kg-1]
!>5)  rncanROW  - Intercepted liquid water sotred on canopy [kg m-2] (cp%rncanROW)
!>6)  RHOSROW  - Density of snow [kg m-3] (cp%RHOSROW)
!>7)  sncanROW  - Intercepted frozen water stored on canopy [kg m-2] (cp%sncanROW)
!>8)  SNOROW   - Mass of snow pack [kg m-2] (cp%SNOROW)
!>9)  TACROW   - Temp of air within veget canopy [K]
!>10) TBARROW  - Temp of soil layers [k] (cp%TBARROW)
!>11) TBASROW  - Temp of bedrock in third soil layer [K]
!>12) TCANROW  - Temp veget canopy [K] (cp%TCANROW)
!>13) THICROW  - Vol frozen water conetent of soil layers [m3 m-3] (cp%THICROW)
!>14) THLQROW  - Vol liquid water conetent of soil layers [m3 m-3] (cp%THLQROW)
!>15) TPNDROW  - Temp of ponded water [k] (cp%TPNDROW)
!>16) TSFSROW  - Ground surf temp over subarea [K]
!>17) TSNOROW  - Snowpack temp [K] (cp%TSNOROW)
!>18) WSNOROW  - Liquid water content of snow pack [kg m-2]
!>19) ZPNDROW  - Depth of ponded water on surface [m] (cp%ZPNDROW)
!>***************************************************************************************
!>***************************************************************************************

    use MESH_INPUT_MODULE

    implicit none

    !Inputs
    integer :: NA, NTYPE, IGND

    !Outputs
    real,dimension(NA,NTYPE)  :: CMAIROW , QACROW  , TACROW , &
                                 TBASROW  , WSNOROW

    real,dimension(NA, NTYPE, 4)  :: TSFSROW

    TYPE(ClassParameters) :: cp

    !Internals
    integer :: IOS


!--------------Main Subtrouine start-----------------------------------------------

    OPEN(UNIT   = 883                     , &
         FILE   = 'int_statVariables.seq' , &
         STATUS = 'OLD'                   , &
         FORM   = 'unformatted'           , &
         ACTION = 'read'                  , &
         ACCESS = 'sequential'            , &
         IOSTAT = IOS                     )

    read(883) cp%albsrow   !1
    read(883) cmairow      !2
    read(883) cp%grorow    !3
    read(883) qacrow       !4
    read(883) cp%rncanrow   !5
    read(883) cp%rhosrow   !6
    read(883) cp%sncanrow   !7
    read(883) cp%snorow    !8
    read(883) tacrow       !9
    read(883) cp%tbarrow   !10
    read(883) tbasrow      !11
    read(883) cp%tcanrow   !12
    read(883) cp%thicrow   !13
    read(883) cp%thlqrow   !14
    read(883) cp%tpndrow   !15
    read(883) tsfsrow      !16
    read(883) cp%tsnorow   !17
    read(883) wsnorow      !18
    read(883) cp%zpndrow   !19

    close(883)

end subroutine read_init_prog_variables_class
