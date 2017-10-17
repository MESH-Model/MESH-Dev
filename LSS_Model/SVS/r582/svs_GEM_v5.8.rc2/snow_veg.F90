!copyright (C) 2001  MSC-RPN COMM  %%%RPNPHY%%%
      SUBROUTINE SNOW_VEG (TSNS,TSND,RHOSL,ALPHAS,WL, &  
                           SNODP,SM, &  
                           PS,VMOD,VDIR,RHOA,THETAA,RG,RAT, &  
                           HU,RR,SR,T,T2M, &  
                           U10M,V10M,SKYVIEW,VTR, &  
                           TVGD,WR,TAVG, &  
                           RNET,HFLUX,LE,EFLUX,RSNOW, &  
                           RHOSNO, &  
                           DT,Z0,Z0HSNOW,FCOR,LAT,ZUSL,ZTSL,PSNVHA, N)
!
      use sfclayer_mod, only: sl_sfclayer,SL_OK
      use sfc_options
      use svs_configs
      implicit none
#include <arch_specific.hf>
!
      INTEGER N
!
      REAL TSNS(N), TSND(N), RHOSL(N), ALPHAS(N), WL(N)
      REAL SNODP(N), SM(N), SKYVIEW(N), VTR(N)
      REAL TVGD(N)

      REAL PS(N), VMOD(N), VDIR(N), RHOA(N), THETAA(N), RG(N), RAT(N)
      REAL HU(N), RR(N), SR(N), T(N), T2M(N)
      REAL U10M(N), V10M(N)
      REAL RNET(N), HFLUX(N), LE(N), EFLUX(N), RSNOW(N)
      REAL RHOSNO(N), WR(N), TAVG(N)
      REAL DT, Z0HSNOW, Z0(N), FCOR(N), LAT(N), ZUSL(N), ZTSL(N),PSNVHA(N)

!
!
!Author
!             S. Belair & M. Abrahamowicz (May 2009)
!
!Revisions
!
! 001         M. Abrahamowicz (May 2009) -- remove HST as input of snow_alone subroutine,
!             as not needed, it is in fact a not-needed output of flxsurf3 subroutine... 
!             Replace HST by SORTI6 in flxsurf3 below
!             BUG FIX: LE was multiplied by snow fraction and then used in other computations...
!                      should NOT have been done ... 
! 002         removed the melt total accumulators from this subroutine...
! 003         S.Z. Husain (Feb 2012)
!              - BUG FIX: Update TSNST after snow melting calculations
!              - BUG FIX: Corrected checking if too much snow is melted
!                         Replace CT(I) by ZCS(I) and SM(I) by SMT(I)      
! 004         S. Zhang (March 2013)
!                Bug fix: replace ZU, ZT with ZUSL, ZTSL
! 005         N. Gauthier (March 2013)
!                Bug fix: correction of RHOMAX based on equation (21)(S. Belair et al. 2003, 
!                J. Hydrometeorology)   
! 006         M. Abrahamowicz (August 2013) -- switch to flxsurf4   
! 007         E. Gaborit (2015) --- bugfix water balance                     
!Object
!             Stand-alone snow model for snow under vegetation
!
!Arguments
!
!
!
!             - Input/Output (Prognostic variables of the snow-under-veg scheme) -
!
! TSNS        snow-under-veg temperature -- S for "skin"
! TSND        mean snow-under-veg temperature -- D for "deep"
! RHOSL       density of snow-under-veg (relative to that of liquid water)
! ALPHAS      albedo of snow-under-veg
! WL          liquid water in the snow-under-veg pack
! SNODP       snow-under-veg depth
! SM          snow-under-veg mass (equivalent water content of the snow reservoir)
! SKYVIEW     Sky view factor for tall/high vegetation
! VTR        (HIGH) Vegetation transmissivity
! 
!             - Input (Forcing) -
! PS          Surface pressure
! VMOD        wind speed at the model lowest level (ZUSL)
! VDIR        wind direction at the model lowest level (ZUSL)
! RHOA        density of air at the model lowest level
! THETAA      Potential temperature at the model lowest level
! RG          solar radiation incident at the surface (downward solar)
! RAT         longwave radiation incident at the surface (NIR)
! HU          Specific humidity of air at the model lowest level
! RR          liquid precipitation rate at the surface in [mm/s]
! SR          solid  precipitation rate at the surface in [mm/s]
! T           surface air temperature
! T2M         2-m air temperature
! U10M        U-component of wind at 10 m
! V10M        V-component of wind at 10 m
! SKYVIEW     Sky view factor for tall/high vegetation
! VTR         (HIGH) Vegetation transmissivity 
! TVGD        Mean/Deep Vegetation Temperature
! WR          Water retained by vegetation
!
!             - Other inputs -
! DT          time step
! Z0          momentum roughness length (no snow)
! Z0HSNOW     Constant for thermal roughness of snow
! FCOR        Coriolis factor
! LAT         latitude
! ZUSL          height of wind input(measured from model base at topo height + Z0)
! ZTSL          height of temperature and humidity input
!
!             - Output (Energy and water budgets of the snow pack) -
! TAVG        Average snow pack temperature use in melt/freez calc.
! RNET        net radiation at the snow surface
! HFLUX       sensible heat flux from the snow surface
! EFLUX       water vapor flux from the snow-unde-veg surface 
! LE          latent heat flux from the snow-unde-veg surface 
! RSNOW       liquid water out of the snow pack
! RHOSNO      density of snow-under-veg (kg/m3) for output only
!
include "thermoconsts.inc"
include "isbapar.cdk"

      INTEGER I
!
!
      REAL LAMI, CICE, DAY
      REAL EMISSN, RAIN1, RAIN2, MLTRAIN 
      REAL CRMIN, CRMAX, TAUHOUR, RHOE, MYOMEGA
      REAL RSNOW_DAY, RHOICE, ANSMIN, EMISONE
      REAL FWOOD, MAX_EFLUX
!
      REAL PETIT
      DATA PETIT/1.E-7/


      real, dimension(n) :: lams, zcs, zqs, ctu, zqsat, zdqsat, zqsatt, &
           rora, a, b, c, tsnst, tsndt, work, melts, freezs, rhomax, fmltrain, &
           smt, wlt, alphast, rhosfall, rhoslt, smx, rvrun, kdiffu, &
           dampd, z0h, bcoef, dmelt, dsnowdt, ftemp, wlmax, resa
include "dintern.inc"
include "fintern.inc"
!
!
!
!************************************************************************
!
!
!
!                                THE FOLLOWING SHOULD BE PUT IN 
!                                A COMMON COMDECK
!
!
      LAMI    = 2.22
      CICE    = 2.106E3 ! specific heat of ice 
      DAY     = 86400.
      EMISSN  = 0.97
      CRMIN   = 0.03
      CRMAX   = 0.10
      RHOE    = 0.20
      TAUHOUR = 3600.
      RHOICE  = 0.9
      ANSMIN  = 0.5
      MYOMEGA   = ( 2*PI )/ DAY
!
      RAIN1   = 2.8e-5  !mm/s
      RAIN2   = 2.8e-4  !mm/s



!             Emissivity of one for high vegetation
      EMISONE = 1.0
!             Here by definition assume that all veg is high so fwood=0.1
      FWOOD   = 0.1
!
!
!
!!            REFRESH ALL INPUT VARIABLES IF THERE IS NEGLIGIBLE SNOW
!                -----------------------------------------------------
!         CHECK THAT THIS INITIALIZATION MAKES SENSE
!
      DO I=1,N
        IF (SM(I).LT.CRITSNOWMASS) THEN
          ALPHAS(I)   = ANSMAX
          RHOSL(I)    = RHOSDEF
!                             For snow temperature, set it to AIR temperature
!                             capped at the triple point of water
          TSNS(I)     = MIN(T(I),TRPL)
          TSND(I)     = MIN(T(I),TRPL)
!                             Assume no liquid water in the snow pack
          WL(I)       = 0.
!                             Assume that snow depth and snow mass from previous
!                             timestep are zero
          SNODP(I)    = 0.
          SM(I)       = 0.       
        END IF
      END DO
!
!
!
!
!!       1.     THE HEAT CAPACITY AND THERMAL DIFFUSIVITY OF THE SNOW
!               -----------------------------
!
!
      DO I=1,N
!                          First calculate the conductivity of snow
        LAMS(I) = LAMI * RHOSL(I)**1.88
!
!                          Heat capacity
!
        ZCS(I) = 2.0 * SQRT( PI/(LAMS(I)* 1000. * RHOSL(I)*CICE*DAY ))
!                          
!                          Thermal diffusivity (You et al., 2014)
        KDIFFU(I) =  LAMS(I) / ( CICE * RAUW*RHOSL(I)) 
!
      END DO
!
!        2.     SPECIFIC HUMIDITY AT SNOW SURFACE
!               ----------------------------------------
!
!                       Calculate specific humidity at snow surface
!                       (For snow, specific & saturation humdity are
!                       the same as snow is always saturated)
!
      DO I=1,N
         ZQS(I) = FOQST( TSNS(I), PS(I) )
      END DO


!
!
!!!     3.     SURFACE TRANSFER COEFFICIENTS FOR HEAT AND MOMENTUM (CH and CD)
!!             ---------------------------------------------------------------
!
!  
!
!  
      DO I=1,N
        Z0H(I)      = Z0HSNOW
      END DO



      i = sl_sfclayer( THETAA, HU, VMOD, VDIR, ZUSL, ZTSL, &
                    TSNS, ZQS, Z0, Z0H, LAT, FCOR, &
                    coeft=CTU )

      if (i /= SL_OK) then
         print*, 'Aborting in snow_alone() because of error returned by sl_sfclayer()'
         stop
      endif

      DO I=1,N
!
        RESA(I) = 1. / CTU(I)
!
      END DO
!
!
!!       4.     TIME INTEGRATION of the SNOW SKIN TEMPERATURE (TSNS)
!               -------------------------------------------------
!
!                            Thermodynamic functions
!
      DO I=1,N
        ZQSAT(I)  = FOQST( TSNS(I),PS(I) )
        ZDQSAT(I) = FODQS( ZQSAT(I),TSNS(I) )
      END DO
!
!
!                            function zrsra
!
      DO I=1,N
        RORA(I) = RHOA(I) / RESA(I)
      END DO
!
!     
!
!                             terms za, zb, and zc for the
!                                    calculation of tsns(t)
!                             Note: Am using emissivity of one
!                             for vegetation 
!
      DO I=1,N
!
        A(I) = 1. / DT + ZCS(I) *  &
                (4. * EMISSN * STEFAN * (TSNS(I)**3) &  
                + RORA(I) * ZDQSAT(I) * (CHLC+CHLF) & 
                + RORA(I) * CPD)&  
                + 2. * PI / DAY
!
        B(I) = 1. / DT + ZCS(I) *  & 
                (3. * EMISSN * STEFAN * (TSNS(I)** 3)&   
                + RORA(I) * ZDQSAT(I) * (CHLC+CHLF) )
!
        C(I) = 2. * PI * TSND(I) / DAY &  
                + ZCS(I) * & 
                ( RORA(I) * CPD * THETAA(I)&   
                + VTR(I) * RG(I) * (1. - ALPHAS(I))&   
                + EMISSN * RAT(I) * SKYVIEW(I)&    
                + (1. - SKYVIEW(I))* EMISSN * STEFAN * (TVGD(I)**4)&  
                - RORA(I)  &
                * (CHLC+CHLF) * (ZQSAT(I)-HU(I)) )
!
      END DO
!
!
      DO I=1,N
         TSNST(I) = ( TSNS(I)*B(I) + C(I) ) / A(I)
      ENDDO

!
!!       5.     DEEP SNOW TEMPERATURE (TSND) AT TIME 'T+DT'
!               ------------------------------------------
!
      DO I=1,N
        TSNDT(I) = (TSND(I) + DT*TSNST(I)/DAY) / (1.+DT/DAY)
      END DO
!
!
!
!
!!       6.     MELTING AND FREEZING TENDENCIES OF SNOW
!               ---------------------------------------
!
!                             
!                             Calculate tendencies using T+ temperatures
!                             Apply energy related to melt/freez to T+ afterwards
!
!
      DO I=1,N
!                             Calculate average snow pack temperature
!                             assuming no time-depency or shift in phase
!                             of the sinusoidal thermal wave (of the force-restore) 
!                             penetrating the snow pack (roughly following You et al., 2014)
!
!                             
!                             Damping depth in m , assuming diurnal forcing dominates
!                           
         DAMPD(I) = SQRT(  2 * KDIFFU(I) / MYOMEGA  )
!                             
!                             Average snow pack temp. 
!        
         IF(SNODP(I).GT.0.0) THEN  
            DMELT(I) = MIN(DAMPD(I),SNODP(I))
            BCOEF(I) = ( 1. - exp( - DMELT(I)/DAMPD(I) ) ) * DAMPD(I) / DMELT(I)
            !BCOEF(I) = ( 1. - exp( - SNODP(I)/DAMPD(I) ) ) * DAMPD(I) / SNODP(I)
         ELSE
            BCOEF(I) = 0.0
         ENDIF

         TAVG(I) = BCOEF(I) * TSNST(I) + (1-BCOEF(I)) * TSNDT(I)
!      
!                         Common portion of the MELTS and FREEZS
!  
         WORK(I) = (TAVG(I)-TRPL) / ( ZCS(I)*CHLF*DT )
      END DO
!
!
!                             MELTS and FREEZS tendencies
!                             Also calculate the maximum snow density
!
      DO I=1,N
        IF (WORK(I).LT.0.) THEN
!                         have freezing
          MELTS(I)  = 0.0
          FREEZS(I) = MIN( -WORK(I), WL(I)/DT )
          RHOMAX(I) = 450. - 20.47 / (SNODP(I)+PETIT) * & 
                           ( 1.-EXP(-SNODP(I)/0.0673))
          RHOMAX(I) = 0.001 * RHOMAX(I)
        ELSE 
!                         have melting

           if(snodp(i).gt.0.0 )then
              MELTS(I)  = MIN( WORK(I) , SM(I)*(DMELT(I)/SNODP(I))/DT )
           else
              MELTS(I)  = 0.0
           endif



          FREEZS(I) = 0.0
          RHOMAX(I) = 600. - 20.47 / (SNODP(I)+PETIT) * & 
                           ( 1.-EXP(-SNODP(I)/0.0673))
          RHOMAX(I) = 0.001 * RHOMAX(I)
        END IF
      END DO

!
!
!
!        7.     EFFECT OF RAIN ON TEMPERATURE OF SNOW PACK
!               ------------------------------------------
!
!                                When rain is falling on snow,
!                                melting is accelerated due to heat
!                                transfers between the incident rain
!                                and the snow pack (since rain is
!                                usually warmer then the snow pack).
!
!                                It is hypothesized that the temperature
!                                of falling water is the same as that
!                                of air at the lowest atmospheric level.
!
      DO I=1,N
        IF (RR(I).LT.RAIN1) THEN
          FMLTRAIN(I) = 0.
        ELSE IF (RR(I).GT.RAIN2) THEN
          FMLTRAIN(I) = 1.
        ELSE
          FMLTRAIN(I) = ( RR(I) - RAIN1 ) / ( RAIN2 - RAIN1 )
        END IF
      END DO
!
      DO I=1,N
        IF (T(I).GT.TRPL.AND.SM(I).GT.0.0.AND.RR(I).GT.0.) THEN
          MLTRAIN = ( T(I)-TRPL ) / ( 2.*ZCS(I)*CHLF*DT )
          MELTS(I) = MELTS(I) + FMLTRAIN(I) * MLTRAIN
          ! Make sure do not melt more than ws/dt
          MELTS(I) = MIN( MELTS(I), SM(I)/DT )
       END IF
      END DO  

!
!
!
!
!                              Melting-Freezing tendency for the
!                              SM and WL reservoirs
!
      DO I=1,N
        DSNOWDT(I) = ( FREEZS(I)-MELTS(I) ) * DT
      END DO
!
!        8.     EFFECT OF MELT/FREEZE ON SNOWPACK TEMP.
!               ------------------------------------------
!
!
!                              new temperature Tsns(t) melting/freezing
!                              apply all the energy to TSNS because applying it to TSND seems to cause problems
!
      DO I=1,N

         TSNST(I) = TSNST(I) +   ZCS(I) * CHLF * (FREEZS(I)-MELTS(I)) * DT
!!$!                              new temperature Tsns(t) and Tsnd(t) after melting/freezing
!!$!                              here re-distribute energy using same weights as the ones used
!!$!                              to calc. avg snowpack temp.  -- TO BE TESTED ??
!!$
!!$        TSNST(I) = TSNST(I) +      BCOEF(I)  * ZCS(I) * CHLF * (FREEZS(I)-MELTS(I)) * DT
!!$        TSNDT(I) = TSNDT(I) + (1 - BCOEF(I)) * ZCS(I) * CHLF * (FREEZS(I)-MELTS(I)) * DT

!
!                              make sure don't exceed triple pt.
        TSNST(I) = MIN ( TSNST(I) , TRPL )
        TSNDT(I) = MIN ( TSNDT(I) , TRPL )
!        
      END DO      
!
!
!
!
!        9.     FLUX CALCULATIONS FOR SNOW COVERED SURFACE ONLY
!               ------------------------------------------------
!
!
      DO I=1,N
!                                            recalculate the qsat function
!
        ZQSATT(I) =  FOQST(  TSNST(I)  ,  PS(I)   )

!
!                                            net radiation
!
        RNET(I)  = VTR(I) * (1. - ALPHAS(I)) * RG(I)&   
                      + EMISSN * (SKYVIEW(I)* RAT(I) - STEFAN * (TSNST(I)** 4))&  
                      + (1. - SKYVIEW(I))* EMISONE * STEFAN * (TVGD(I)**4)

!
!                                            sensible heat flux
!
        HFLUX(I) = RHOA(I) * CPD * (TSNST(I) - THETAA(I)) / RESA(I)
        FTEMP(I) = ( TSNST(I) - THETAA(I) ) / RESA(I)
!
!                                            latent heat of evaporation from
!                                            the snow canopy
! 
        EFLUX(I) = (ZQSATT(I) - HU(I)) / RESA(I)                               
!                                            
!       IMPOSE MAXIMUM on EFLUX, based on available liquid water after melt/freez of reservoir... make sure latent heat consistent with this...
!
        if(PSNVHA(I).gt.0.0) then
           MAX_EFLUX=(  (SM(I) + DSNOWDT(I)) / DT + SR(I) )  / RHOA(I) / PSNVHA(I) 
        else
           MAX_EFLUX=0.0
        endif

        EFLUX(I)=  MIN( EFLUX(I),MAX_EFLUX )
        
        LE(I)    =  RHOA(I) * (CHLC+CHLF) * EFLUX(I)
   


!
      END DO
!
!
!
!!       10.     EVOLUTION OF THE SNOW EQUIVALENT WATER CONTENT (Wst)
!               --------------------------------------------
!
!                               evaporation over snow
!
      DO I=1,N

!
!
!                               evolution of Ws
!
!            ! DANGER HERE: We use SM as a check of snow presence,
!            ! In the case of no-snow this variable could be updated
!            ! anyhow due to the construct of the subroutine, so
!            ! we add an additional check here i.e. 
! 
!!           CALCULATE SMT VARIABLE IF AND ONLY IF:
!            A) there is snow i.e. ws >= critsnow
!         or B) the snow rate is non-zero
!              
        IF(SM(I).ge.CRITSNOWMASS.or.SR(I).gt.0.0) THEN
           SMT(I) = SM(I) - DT * (RHOA(I)*PSNVHA(I)*EFLUX(I) - SR(I)) + DSNOWDT(I)
           SMT(I) = MAX(SMT(I), 0.0)
        ELSE
           SMT(I) = 0.0
        ENDIF
     END DO
!
!
!       11.     EVOLUTION OF LIQUID WATER IN THE SNOW PACK
!               ------------------------------------------
!
!                               Calculate the contribution of rain
!                               and vegetation runnoff to WL
!                               WATCH OUT: Assume all rain goes directly
!                               to snowpack ... essential to close the water
!                               budget
!
       DO I=1,N
          RVRUN(I)= RR(I)
       END DO
!
!
!
!
!                               Calculate the maximum liquid water
!                               that can be retained in the snow pack
!
      DO I=1,N
        IF (RHOSL(I).LT.RHOE) THEN
          WLMAX(I) = ( CRMIN + (CRMAX-CRMIN)*(RHOE-RHOSL(I))/ RHOE)* SMT(I)
        ELSE
          WLMAX(I) = CRMIN * SMT(I)
        END IF
      END DO
!
!
!                               Calculate runoff of liquid water from 
!                               the snow pack
!
      DO I=1,N
        IF (WL(I).LE.WLMAX(I)) THEN
          RSNOW(I) = ( WL(I) / TAUHOUR ) * EXP( WL(I)-WLMAX(I) )
        ELSE
          RSNOW(I) = WLMAX(I) / TAUHOUR + (WL(I)-WLMAX(I)) / DT
        END IF
        RSNOW(I) = MAX( 0.      , RSNOW(I) )
        RSNOW(I) = MIN( (WL(I)-DSNOWDT(I))/DT+RVRUN(I) ,  RSNOW(I) )
      END DO
!
!
!                               Calculate the new values for WL and
!                               for the liquid water reaching the ground
!
      DO I=1,N
        WLT(I) = WL(I) + RVRUN(I) * DT - RSNOW(I)* DT - DSNOWDT(I)
        WLT(I) = MAX( 0.0, WLT(I) )
      END DO
!
!
!
!!      12.     EVOLUTION OF SNOW ALBEDO
!               ------------------------
!
!
!
      DO I=1,N
        RSNOW_DAY = MIN( RSNOW(I)*DAY, 10. )
      END DO
!
!                                       the evolution of the snow albedo differs
!                                       if there is melting or not
!
      DO I=1,N
!
!
        IF (SMT(I).GT.0.0.AND.DSNOWDT(I).LT.0.0) THEN
!
!                                       when there is freezing           
!
           ALPHAST(I) = (ALPHAS(I)-ANSMIN)*EXP(-0.01*DT/3600.) &  
                +  ANSMIN & 
                +  SR(I)*DT/WCRN*(ANSMAX-ANSMIN)
!
!
        ELSE IF (SMT(I).GT.0.0.AND.DSNOWDT(I).GE.0.0) THEN
!
!                                        when there is melting
!
           ALPHAST(I) = ALPHAS(I) - TODRY*DT/DAY &   
                + SR(I)*DT/WCRN*(ANSMAX-ANSMIN)
!
!
        ELSE
           ALPHAST(I) = ANSMAX
        ENDIF
!                                       limits of the albedo
!
        ALPHAST(I) = MAX( ANSMIN, ALPHAST(I) )       
        ALPHAST(I) = MIN( ANSMAX, ALPHAST(I) )
!

      END DO
!
!
!
!!       13.     EVOLUTION OF SNOW DENSITY
!                -------------------------
!
!
!                           Density of falling snow
!
      DO I=1,N
        IF (SMT(I).GT.0.0) THEN
           RHOSFALL(I) = 109. + 6.*(T2M(I)-TRPL) + &  
                              26.*(U10M(I)**2+V10M(I)**2)**0.25
           RHOSFALL(I) = MIN(MAX((RHOSFALL(I)*0.001),RHOMIN), 0.250)
        ELSE
           RHOSFALL(I) = RHOSDEF
	END IF
      END DO
!
!                           Evolution of the snow density depends
!                           on 3 factors:  
!    
!                           - decrease of density due to fresh new snow
!                           - increase of density due to aging
!                           - increase of density due to freezing of 
!                             liquid water in the snow pack
!
!
!                           A) decrease due to fresh new snow
!
      DO I=1,N
         SMX(I)   = MAX( SMT(I),SR(I)*DT)
         IF (SMT(I).GT.0.0) THEN
!          
            RHOSLT(I) = ( (SMX(I)-SR(I)*DT) * RHOSL(I)&  
                        + (SR(I)*DT) * RHOSFALL(I)) / SMX(I)
         ELSE
            ! default
            RHOSLT(I) = RHOSDEF
         ENDIF
      END DO
!
!
!                           B) increase due to aging
!
      DO I=1,N
        IF (SMT(I).GT.0.0.AND.RHOSLT(I).LT.RHOMAX(I)) THEN
          RHOSLT(I) = (RHOSLT(I)-RHOMAX(I))*EXP(-0.01*DT/3600.) &  
                          + RHOMAX(I)
        END IF
      END DO
!
!
!                           C) increase due to freezing
!
      DO I=1,N
        IF (SMT(I).GT.0.0) THEN
          RHOSLT(I) = ( SMT(I)*RHOSLT(I) + FREEZS(I)*DT*RHOICE ) & 
                        / ( SMT(I)+FREEZS(I)*DT )
!                          Make sure within bounds 
          RHOSLT(I) = MIN( RHOICE, RHOSLT(I) )
          RHOSLT(I) = MAX( RHOMIN, RHOSLT(I) )

        END IF
      END DO
!
!
!
!                          Calculate snow depth based on snow density
!
      DO I=1,N
         SNODP(I) = SMT(I)/(RHOSLT(I)*RAUW)
!
!
      END DO
!
!
!
!!       14.     UPDATE the PROGNOSTIC VARIABLES
!                -------------------------------
!
      DO I=1,N
        TSNS(I)     = TSNST(I)
        TSND(I)     = TSNDT(I)
        WL(I)       = WLT(I)
        SM(I)       = SMT(I)
        ALPHAS(I)   = ALPHAST(I)
        RHOSL(I)    = RHOSLT(I)
        RHOSNO(I)   = RHOSLT(I)*RAUW
      END DO
!
!
!
!                  If negligible amount of snow,
!                  set ALL OUTPUT variables 
!                  to default values and/or zero.
!                 
!
      DO I=1,N
        IF (SM(I).LT.CRITSNOWMASS) THEN
          ALPHAS(I)   = ANSMAX
          RHOSL(I)    = RHOSDEF
          RHOSNO(I)   = RHOSLT(I)*RAUW
          TSNS(I)     = 300.0
          TSND(I)     = 300.0
          TAVG(I)     = 300.0
          WL(I)       = 0.0
          SM(I)       = 0.0
          SNODP(I)    = 0.0
          RNET(I)     = 0.0
          HFLUX(I)    = 0.0
          LE(I)       = 0.0
          EFLUX(I)    = 0.0
        END IF
      END DO
!
      RETURN
      END
