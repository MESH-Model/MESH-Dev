subroutine read_init_prog_variables_class( CMAIROW  , QACROW  , TACROW   , &
                                           TBASROW  , TSFSROW , WSNOROW  , &
                                           cp       , NA      , NTYPE    , &
                                           IGND     , fls                 )
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
!>5)  RCANROW  - Intercepted liquid water sotred on canopy [kg m-2] (cp%RCANROW)
!>6)  RHOSROW  - Density of snow [kg m-3] (cp%RHOSROW)
!>7)  SCANROW  - Intercepted frozen water stored on canopy [kg m-2] (cp%SCANROW)
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
    use flags
    use model_files

    implicit none

    !Inputs
    integer NA, NTYPE, IGND
    type(fl_ids) :: fls

    !Outputs
    real,dimension(NA,NTYPE)  :: CMAIROW , QACROW  , TACROW , &
                                 TBASROW  , WSNOROW

    real,dimension(NA, NTYPE, 4)  :: TSFSROW

    TYPE(ClassParameters) :: cp

    !Internals
    integer IOS, unitfl

!--------------Main Subtrouine start-----------------------------------------------
    if ((VARIABLEFILESFLAG == 1) .and. (fls%fl(9)%isInit)) then
        open(unit   = fls%fl(9)%unit                , &
             file   = trim(adjustl(fls%fl(9)%name)) , &
             status = 'old'                   , &
             form   = 'unformatted'           , &
             action = 'read'                  , &
             access = 'sequential'            , &
             iostat = IOS                     )
        unitfl = fls%fl(9)%unit
    else
        open(unit   = 883                     , &
             file   = 'int_statVariables.seq' , &
             status = 'old'                   , &
             form   = 'unformatted'           , &
             action = 'read'                  , &
             access = 'sequential'            , &
             iostat = IOS                     )

        unitfl = 883
    end if

    read(unitfl) cp%albsrow   !1
    read(unitfl) cmairow      !2
    read(unitfl) cp%grorow    !3
    read(unitfl) qacrow       !4
    read(unitfl) cp%rcanrow   !5
    read(unitfl) cp%rhosrow   !6
    read(unitfl) cp%scanrow   !7
    read(unitfl) cp%snorow    !8
    read(unitfl) tacrow       !9
    read(unitfl) cp%tbarrow   !10
    read(unitfl) tbasrow      !11
    read(unitfl) cp%tcanrow   !12
    read(unitfl) cp%thicrow   !13
    read(unitfl) cp%thlqrow   !14
    read(unitfl) cp%tpndrow   !15
    read(unitfl) tsfsrow      !16
    read(unitfl) cp%tsnorow   !17
    read(unitfl) wsnorow      !18
    read(unitfl) cp%zpndrow   !19

    close(unitfl)

end subroutine read_init_prog_variables_class
