SUBROUTINE FROZEN(FI,SNOWMELT,TS,DELZ1,SWE, &
                  SOIL_MOIST_MAX,SOIL_MOIST, &
                  DELT,NCOUNT,NMELT,ILG,IL1,IL2,t0_ACC,&
                  SI,TSI,INFILTYPE,SNOWMELTD,SNOWMELTD_LAST, &
                  MELTRUNOFF,CUMMELTRUNOFF,SNOWINFIL,CUMSNOWINFIL)

!>=============================================================================
!>
!> December 20, 2010 - M.A. Mekonnen
!>=============================================================================
!>
!> DESCRIPTION: For a frozen soil SMR partitions snow melt runoff into  
!>              infiltration and runoff using Gray et al, 2001. The
!>              subroutine is adapted from the FROZEN module of CRHM.
!>
!> REFERENCE: 
!> 1. Pomeroy et al, 2007 - The cold regions hydrological model: a platform for 
!>                          basing process representation and model structure 
!>                          on physical evidence, Hydrological Processes 21: 
!>                          2650 - 2667 
!> 2. Gray et al, 2001 - Estimating areal snowmelt infiltration into frozen 
!>                       soils, Hydrological Processes 15: 3095 - 3111 
!> 3. Zhao and Gray, 1999 - Estimating areal snowmelt infiltration into frozen
!>                          soils, Hydrological Processes 13: 1827 - 1842 
!> 4. Zhao and Gray, 1997 - A parameteric expression for estimating 
!>                          infiltration into frozen soils, Hydrological 
!>                          Processes 11: 1761 - 1775
 
!>=============================================================================
!>
!> Incoming variables
!>
!> FROZENSOILINFILFLAG - Flag for frozen soil infiltration calculation choices.
!>                     - 0 - use class frozen soil infiltration. 
!>                         - this corresponds to unlimited infiltration.
!>                     - 1 - use frozen soil infiltration by Gray et al, 2001,
!>                         - with opportunity time based on Zhao and Gray, 1997.
!>                     - >1- use frozen soil infiltration by Gray et al, 2001,
!>                         - using the value of frozensoilinfilflag as opportunity time.
!>                         - as an approximate estimate, one can use class outputs 
!>                         - for snow accumulation time series. 
!> FI                 -  Fractional area
!> DELZ1              -  Top soil layer depth (m)
!> SNOWMELT           -  Melt rate at the bottom of snowpack (m/sec)
!> TS                 -  Temperature of top soil layer (°k)
!> SWE                -  Snow water equivalent (mm)
!> SOIL_MOIST         -  Frozen soil moisture (mm^3/mm^3)
!> SOIL_MOIST _MAX    -  Maximum frozen soil moisture (mm^3/mm^3)
!> DELT               -  Model time step (sec)
!> NCOUNT             -  Number of counts in a day [1 to 48]
!>                       TO DO #1: check that ncount = 1 corresponds to 
!>                                 half an hour after mid night.

!> Outgoing variables:
!>
!> t0_ACC             -  Accumulated opportunity time (hr)
!> TSI                -  Initial soil temperature (°C)
!> SI                 -  Initial soil moisture (mm^3/mm^3)
!> INFILTYPE          -  Infiltration type
!> SNOWINFIL          -  Melt infiltration (mm/sec)
!> CUMSNOWINFIL       -  Cumulative melt infiltration (mm)
!> MELTRUNOFF         -  Melt runoff (mm/sec)
!> CUMMELTRUNOFF      -  Cumulative melt runoff (mm)
!> SNOWMELTD          -  Daily snow melt (mm)
!> SNOWMELTD_LAST     -  Yesterday's snowmelt (mm)

!> Parameters:
!>
!> S0                 - Surface saturation during melting (mm^3/mm^3)
!> C                  - Coefficient for the frozen soil infiltration parameteric equation
!> T_ICE_LENS         - Overnight minimum temperature to cause ice lens after major melt (ºC)

!> Local temporary variables:
!>
!> t0                 -  Opportunity time (hr)
!> INF                -  Cumulative frozen soil infiltration (mm)
!> INF0               -  Frozen soil infiltration rate (mm/sec)
!> CAPACITY           -  Maximum top frozen soil water holding capacity (mm/sec)
!> NMELT              -  Counter to trace beginning of snow melt seasons
!> TFREZ              -  Freezing temperature (°C)

USE FLAGS
IMPLICIT NONE

!> INCOMING
INTEGER NCOUNT,ILG,IL1,IL2
REAL    DELT
REAL    FI(ILG),DELZ1(ILG),ZSNOW(ILG),SNOWMELT(ILG),TS(ILG),SWE(ILG), &
        SOIL_MOIST_MAX(ILG),SOIL_MOIST(ILG)

!> OUTGOING
INTEGER INFILTYPE(ILG),NMELT(ILG)
REAL    t0_ACC(ILG),SI(ILG),TSI(ILG),SNOWMELTD(ILG),SNOWMELTD_LAST(ILG), &
        SNOWINFIL(ILG),CUMSNOWINFIL(ILG),MELTRUNOFF(ILG),CUMMELTRUNOFF(ILG)
        

!> LOCAL
INTEGER, PARAMETER :: RESTRICTED = 1
INTEGER, PARAMETER :: UNLIMITED  = 2
INTEGER, PARAMETER :: LIMITED    = 3
REAL,    PARAMETER :: S0         = 0.75   ! Ranges vary from 0.75 to 1.0
REAL,    PARAMETER :: T_ICE_LENS = -10.0  ! Ranges vary from -50ºC to 0ºC
REAL,    PARAMETER :: C          = 2.1    ! Ranges vary from 1 to 3 -> Prairie value = 2.1, Boreal Forest value =1.14  
REAL,    PARAMETER :: TFREZ      = 0.0

INTEGER I
REAL    t0,INF,INF0,CAPACITY

!> Initialize snow infiltration and melt runoff.
SNOWINFIL  = SNOWMELT
MELTRUNOFF = 0.0

!> Loop through sub-basin GRUs.
DO I = IL1, IL2
    
    !> GRU exists.
    IF(FI(I) > 0.0)THEN
        
        !> Snow is melting.
        IF(SNOWMELT(I) > 0.0)THEN
           NMELT(I) = NMELT(I) + 1
       
           !> Snow melting just started.
           IF(NMELT(I) == 1)THEN

              !> Store initial soil moisture and initial soil temperature.
              SI(I)     = SOIL_MOIST(I)
              TSI(I)    = MIN(-5.0,TS(I))   !> TO DO #1 - Justify the limit -5.0
                                            !> Main reason is that TS is the average 
                                            !> temperature of the first layer and the ponded water

           ENDIF
     
          !> Accumulate opportunity time - for future revision
          !> to directly calculate the opportunity time in first calibration run
          !> and use the calculated values afterwards.
          !t0_ACC(I) = t0_ACC(I) + DELT

          !> Compute opportunity time [hours] based on Zhao and Gray, 1997.
          IF(FROZENSOILINFILFLAG == 1)t0 = MAX(DELT/3600.0, 0.65 * SWE(I) - 5) !>DELT is model time step

          !> User provides opportunity time as FROZENSOILINFILFLAG value.
          !> An approximate estimate can be found from class output files for snowdepth.
          IF(FROZENSOILINFILFLAG > 1)t0 = FROZENSOILINFILFLAG
          
         !> Beginning of day (just after mid night)
!         IF(NCOUNT == 1)THEN
           
           !> Check for ice lens formation at the beginning of the day.
           IF(TS(I) <= T_ICE_LENS .AND. INFILTYPE(I) == LIMITED .AND. &
              SNOWMELTD_LAST(I) > 5.0)THEN

              !> Restricted infiltration due to ice lens.
              INFILTYPE(I) = RESTRICTED

           ENDIF
!         ENDIF
    
            !> Compute daily snow melt.
            SNOWMELTD(I) = SNOWMELTD(I) + SNOWMELT(I)*DELT

            !> Store end of day's snow melt as yesterday's snow melt
            !> and reset daily snow melt to zero.
            IF(NCOUNT == 48)THEN
               SNOWMELTD_LAST(I) = SNOWMELTD(I)
               SNOWMELTD(I) = 0.0
            ENDIF
    
            !> Partition snow melt into infiltration and runoff.      
            SELECT CASE(INFILTYPE(I))
            
            !> For limited infiltration category,
            !> some part infiltrates and some part becomes runoff.
            CASE(LIMITED)
            
               !> Maximum water holding capacity of the frozen soil (m/sec).
               CAPACITY = (SOIL_MOIST_MAX(I) - SOIL_MOIST(I))*DELZ1(I)/DELT
               
               !> Compute cumulative infiltration using parameteric equation.
               !> For further details look into Reference #2 listed above.
               IF(CAPACITY > 0.0)THEN
                  INF  = C*(S0**2.92)*                                &
                         ((1.0 - SI(I))**1.64)*                       &
                         (((0.0 - TSI(I)) / 273.15)**-0.45)*          &
                         (t0**0.44)                                             
                  
                  !> Infiltration rate (m/sec) - Note the units: INF (mm), t0 (hr)       
                  INF0 = INF/(t0*3600.0*1000.0)
                  
                  !> All snow melt infiltrates if the frozen soil water holding 
                  !> capacity is sufficient enough and snow melt rate is less than
                  !> infiltration rate.
                  !> Order: SNOWMELT---INF0---CAPACITY or 
                  !>        SNOWMELT---CAPACITY---INF0
                  IF(SNOWMELT(I) <= INF0 .AND. SNOWMELT(I) <= CAPACITY)THEN
                     SNOWINFIL(I)  = SNOWMELT(I)
                     MELTRUNOFF(I) = 0.0
                  
                  !> Snow melt infilitration limited by infiltration rate.
                  !> Order: INF0---SNOWMELT---CAPACITY or 
                  !>        INF0---CAPACITY---SNOWMELT
                  ELSEIF(SNOWMELT(I) > INF0 .AND. INF0 <= CAPACITY)THEN !> Here there is deviation from CRHM
                     SNOWINFIL(I)  = INF0                               !>  TO DO #2
                     MELTRUNOFF(I) = SNOWMELT(I) - INF0

                  !> Snow melt infilitration limited by capacity.
                  !> Order: CAPACITY---INF0---SNOWMELT or 
                  !>        CAPACITY---SNOWMELT---INF0
                  ELSE
                     SNOWINFIL(I)  = CAPACITY
                     MELTRUNOFF(I) = SNOWMELT(I) - CAPACITY
                  ENDIF
                  
                  !> Accumulate infiltration and runoff.
                  CUMSNOWINFIL(I)  = CUMSNOWINFIL(I)  + SNOWINFIL(I)*DELT
                  CUMMELTRUNOFF(I) = CUMMELTRUNOFF(I) + MELTRUNOFF(I)*DELT
                  
                  !> Check mass conservation and adjust infiltration category.
                  !> This may address the above deviation (TO DO #2).
                  IF(CUMSNOWINFIL(I) > INF)THEN 
                     CUMMELTRUNOFF(I) = CUMMELTRUNOFF(I) + (CUMSNOWINFIL(I) - INF)
                     CUMSNOWINFIL(I)  = INF
                     INFILTYPE(I)     = RESTRICTED
                  ENDIF
               
               !> All snow melt becomes runoff if frozen soil water holding capacity
               !> is exhausted.
               ELSE
                  MELTRUNOFF(I)    = SNOWMELT(I)
                  CUMMELTRUNOFF(I) = CUMMELTRUNOFF(I) + MELTRUNOFF(I)*DELT
               ENDIF

            !> For unlimited infiltration category infiltrate all the snow melt.
            CASE(UNLIMITED)
               SNOWINFIL(I)    = SNOWMELT(I)
               CUMSNOWINFIL(I) = CUMSNOWINFIL(I) + SNOWINFIL(I)*DELT
          
            !> For limited infiltration category all the snow melt is directed to runoff.
            CASE(RESTRICTED)
               MELTRUNOFF(I)    = SNOWMELT(I)
               CUMMELTRUNOFF(I) = CUMMELTRUNOFF(I) + MELTRUNOFF(I)*DELT
            END SELECT
            
        ELSE
           
           !> Initialize counter for next melting.
           NMELT(I) = 0        

        ENDIF !> ENDIF SNOWMELT > 0.0

        !> Prepare for first entry into the next snow melting.
        IF(SWE(I) <= 0.0)THEN

           !> Reset infiltration categories.
           IF(SOIL_MOIST(I) >= 1.0)THEN
              INFILTYPE(I) = RESTRICTED
           ELSEIF(SOIL_MOIST(I) <= 0.0)THEN
              INFILTYPE(I) = UNLIMITED
           ELSE
              INFILTYPE(I) = LIMITED
           ENDIF
        ENDIF               
    
    ENDIF !> ENDIF FI > 0.0
ENDDO

RETURN

END