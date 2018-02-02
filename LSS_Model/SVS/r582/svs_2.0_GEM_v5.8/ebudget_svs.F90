!copyright (C) 2001  MSC-RPN COMM  %%%RPNPHY%%%

      SUBROUTINE EBUDGET_SVS( TSA, TS, WD, WF , &
                   TGRS,TGRD,TVGS,TVGD, TP, TPV, TPERM, & 
                   PGRNDFLUX, PGRNDFLUXV, DT, VMOD, VDIR, LAT, & 
                   RG, ALVG, LAI, GAMVEG, & 
                   ALGR,EMGR, & 
                   RAT, THETAA, FCOR, ZUSL, ZTSL, HU, PS, &  
                   RHOA, Z0, Z0H,  & 
                   HRSURF, HV, DEL, RS, & 
                   CG,CVP, EMIS, PSNG, &  
                   RESAGR, RESAVG,  &
                   RNETSN, HFLUXSN, LESLNOFRAC,LESSNOFRAC, ESNOFRAC, & 
                   ALPHAS, &  
                   TSNS, & 
                   RNETSV, HFLUXSV, LESLVNOFRAC,LESSVNOFRAC, ESVNOFRAC,  &
                   ALPHASV, & 
                   TSVS, & 
                   VEGH, VEGL, VGHEIGHT,  &
                   PSNVH,PSNVHA,SKYVIEW,SKYVIEWA, & 
                   CONDSLD, CONDDRY, CAP, SOILCOND, &
                   RR,WR,SNDP,SVDP, &
                   VTRA,ALBT, & 
                   RNET, HFLUX, LE, LEG, LEV, LES,LESV,LEGV, & 
                   LER, LETR, EG, ER, ETR, GFLUX, EFLUX, & 
                   BM, FQ, BT, & 
                   LEFF, DWATERDT, & 
                   FTEMP, FVAP, ZQS, FRV, & 
                   ALFAT, ALFAQ, ILMO, HST, TRAD, N, &
                   QVEG, QGV, QGR, &
                   HFLUXGRV, TAF, QAF, VAF, & 
                   RGVG,RGGV,FIGV,FIVG,IRGV,IRVG, &
                   HGV,LGV, &
                   HGR,LGR, &
                   HVEG, LVEG, &
                   FGRV, TRNETVG, TFNETVG, RNGV, &
                   TNEIGE,TIRGV,TIRVG,TFIGV,TRGVG, RPP,grflux,Z0HA,RESAGRV )

      use sfclayer_mod, only: sl_sfclayer,SL_OK
      use sfc_options
      use svs_configs
      use modd_snowES_par,   only: XEMISSN,XSNOWDMIN
      implicit none
#include <arch_specific.hf>
!
!
      include "nsl_snowES.cdk"

      INTEGER N
      
      REAL TS(N,NL_SVS), TSA(N), DT, VMOD(N)
      REAL VDIR(N), LAT(N), PGRNDFLUX(N), PGRNDFLUXV(N)
      REAL TGRS(N), TGRD(N), TVGS(N), TVGD(N)
      REAL TSNS(N,NSL), TSVS(N,NSL)
      REAL DZ(NL_SVS), TP(N,NL_SVS), TPV(N,NL_SVS), TPERM(N)
      REAL WD(N,NL_SVS), WF(N,NL_SVS)
      REAL RG(N), ALVG(N), RAT(N), THETAA(N), FCOR(N)
      REAL ZUSL(N), ZTSL(N)
      REAL HU(N), PS(N), RHOA(N), Z0(N), Z0H(N)
      REAL HV(N), DEL(N), RS(N)
      REAL CG(N), CVP(N),  PSNG(N), EMIS(N)
      REAL LAI(N), GAMVEG(N), ALGR(N), EMGR(N)
      REAL RNET(N), HFLUX(N), LE(N), ALPHAS(N)
      REAL ALBT(N)
      REAL LEG(N), LEV(N), LER(N), LETR(N), GFLUX(N), LEGV(N)
      REAL EFLUX(N), BM(N), FQ(N), BT(N), LES(N)
      REAL FTEMP(N), FVAP(N), ER(N), ETR(N)
      REAL LEFF(N), DWATERDT(N), ZQS(N), FRV(N)
      REAL EG(N), HRSURF(N)
      REAL RESAGR(N), RESAVG(N)
      REAL RNETSN(N), HFLUXSN(N), LESLNOFRAC(N), LESSNOFRAC(N),ESNOFRAC(N)
      REAL RNETSV(N), HFLUXSV(N), LESLVNOFRAC(N), LESSVNOFRAC(N), ESVNOFRAC(N)
      REAL ALPHASV(N), ALFAT(N), ALFAQ(N), LESV(N)
      REAL VEGH(N), VEGL(N), PSNVH(N), PSNVHA(N)
      REAL ILMO(N), HST(N), TRAD(N)
      REAL SKYVIEW(N), SKYVIEWA(N), VTRA(N)
      REAL WR(N),RR(N),SVDP(N),SNDP(N)
      REAL VGHEIGHT(N), CAP(N,NL_SVS)
      REAL CONDSLD(N,NL_SVS), CONDDRY(N,NL_SVS), SOILCOND(N,NL_SVS)

!  ajout temporaire pour tests
      REAL VAF(N), TAF(N), QAF(N), QVEG(N), QGV(N), QGR(N), HVEG(N), LVEG(N), LGR(N), HGR(N)
      REAL RGVG(N), RGGV(N), FIGV(N), FIVG(N), IRGV(N), IRVG(N), HGV(N), LGV(N), FGRV(N), grflux(N)
      REAL TRNETVG(N),TFNETVG(N),RNGV(N), HFLUXGRV(N)
      REAL TNEIGE(N),TIRGV(N),TIRVG(N),TFIGV(N),TRGVG(N), RPP(N)
      REAL Z0HA(N), RESAGRV(N)
!
!Author
!          S. Belair (January 1997)
!Revisions
! 001      S. Belair (November 1998)
!             Use physics package thermodynamic functions
!
! 002      S. Belair (December 1998)
!             Correction to the latent heat coefficients due to
!             soil water freezing.
!             Calculation of the FREEZG and MELTG tendencies for
!             soil water freezing and melting.
!             Tendencies for the surface temperature TS.
!
! 003      S. Belair (January 1999)
!             Tendencies for melting and freezing of snow
!
! 009      S. Belair (May 2007)
!             Variable emissivity of the surface (depends on
!               the land use land cover)
!
!Object
!
!     Calculates the evolution of the surface and deep-soil temperature
!     (i.e., Ts and T2), as well as all the surface fluxes.
!
!
!!!!  METHOD
!!    ------
!
!     1- find the grid-averaged albedo, emissivity, and roughness length
!     2- compute the za, zb, and zc terms involved in the numerical
!        resolution of the equations for Ts and T2.
!     3- find Ts(t) and T2(t).
!     4- derive the surface fluxes.!
!
!Arguments
!
!
!          - Input/Output -
! TGRS      (bare) ground temperature -- S for "skin"
! TGRD      mean ground temperature -- D for "deep"
! TVGS      vegetation temperature -- S for "skin"
! TVGD      mean vegetation temperature -- D for "deep"
! TS        surface  temperature (new) as seen from ground
! TP(:,:)   soil temperature profile under bare ground
! TPV(:,:)  soil temperature profile under vegetation
!
!          - Input -
! VMOD      module of the low-level wind
! VDIR      wind direction at the lowest level
! LAT       latitude
! WD(:,1)   soil water content of the first soil layer 
! WF(:,:)   frozen soil water profile
! DT        timestep
! RG        global radiation (downward solar)
! ALVG      AVERAGED surface albedo associated with vegetation type
! LAI       AVERAGED vegetation leaf area index
! GAMVEG    AVERAGED parameter related to the vegetation height
! ALGR      albedo of bare ground (soil)
! EMGR      emissivity of bare ground (soil)   
! RAT       atmospheric radiation incident on the ground (NIR)
! THETAA    air potential temperature at the lowest level
! FCOR      Coriolis factor
! ZTSL      reference height for temperature and humidity input
! ZUSL      reference height for wind input
! HU        specific humidity of air at the lowest level
! PS        surface pressure
! RHOA      air density near the surface
! Z0H       agg. roughness length for heat transfer considering snow
! Z0        momentum roughness length (no snow)
! HRSURF    relative humidity of the bare ground surface (1st soil layer)
! HV        Halstead coefficient (relative humidity of veg. canopy)
! DEL       portion of the leaves covered by water
! RS        stomatal resistance
! CG        soil thermal coefficient
! CVP       AVERAGED vegetation thermal coefficient (with LAI effect)
! EMIS      AVERAGED surface emissivity when vegetation fully grown
! PSNG      fraction of bare ground or low veg. covered by snow
! RESAGR    aerodynamical surface resistance for bare ground
! RESAGRV   aerodynamical surface resistance for ground under vegetation
! RESAVG    aerodynamical surface resistance for vegetation
!
! RNETSN    net radiation over snow 
! HFLUXSN   sensible heat flux over snow 
! ESNOFRAC  water vapor flux from the snow surface (kg/m2/s)
! LESLNOFRAC latent heat flux of evaporation from the snow surface (W/m2)
! LESSNOFRAC latent heat flux of sublimation from the snow surface (W/m2)
! ALPHAS    albedo of snow
! TSNS      surface (skin) snow temperature at time t+dt (update in snow_alone.ftn)
!
! RNETSV    net radiation over snow-under-vegetation
! HFLUXSV   sensible heat flux over snow-under-vegetation
! ESVNOFRAC water vapor flux from the snow-under-vegetation  (kg/m2/s)
! LESLVNOFRAC latent heat flux of evaporation from the snow-under-vegetation (W/m2)
! LESSVNOFRAC latent heat flux of sublimation from the snow-under-vegetation (W/m2)
! ALPHASV   albedo of snow-under-veg
! TSVS      surface (skin) snow-under-veg temperature at time t+dt (update in snow_alone.ftn)
!
! VEGH     fraction of HIGH vegetation
! VEGL     fraction of LOW vegetation
! PSNVH    fraction of HIGH vegetation covered by snow
! PSNVHA   fraction of HIGH vegetation covered by snow as seen from
!          the ATMOSPHERE 
! SKYVIEW  Sky view factor for tall/high vegetation
! RR       Liquid precipitation rate at the surface in [mm/s]
! WR       Water retained by vegetation
! SVDP     snow depth for snow under high veg [m]
! SNDP     snow depth for snow under low veg and ground [m]
!
!           - Output -
! ALBT      total surface albedo (snow + vegetation + bare ground)
! RNET      net radiation
! HFLUX     sensible heat flux
! LE        latent heat flux
! LEG       latent heat flux over bare ground
! LEV       latent heat flux over vegetation
! LES       latent heat flux over snow
! LESV      latent heat flux over snow-under-veg
! LER       direct latent heat flux from vegetation leaves
! LETR      evapotranspiration latent heat flux
! EG        evaporation rate (no fraction) over bare ground (1st soil layer)
! ER        direct evaporation rate (no fraction) from veg. leaves
! ETR       evapotranspiration rate (no faction) 
!
! GFLUX     ground flux
! EFLUX     water vapor flux
! DWATERDT  net tendency of melting-freezing of soil water
! TS        surface  temperature (new) as seen from ground
! TSA       surface  temperature (new) as seen from space
! LEFF      effective latent heat
! FTEMP      land sfc avg turbulent surface flux of sensible heat
! FVAP       land sfc avg turbulent surface flux of vapor
! ILMO       land sfc avg (1/length of Monin-Obukov) 
! HST        land sfc avg height of the boundary layer 
! FRV        land sfc average friction velocity 
! BM         homogeneous boundary condition term in the
!            diffusion equation for U and V
! BT         homogeneous boundary condition term in the
!            diffusion equation for T and Q
! FQ         land sfc average momentum flux
! ALFAT      inhomogeneous boundary term in the diffusion equation for Theta
! ALFAQ      inhomogeneous boundary term in the diffusion equation for Q
! ZQS       area-averaged specific  humidity of a model tile
! TRAD      averaged radiative temperature

!
include "isbapar.cdk"
include "thermoconsts.inc"
include "dintern.inc"
include "fintern.inc"
!
      INTEGER I, K
!
!
      REAL EMISSN, EMSOIL, KCOEF
      REAL LOG_CONDI, LOG_CONDW, XF, XU, WORK1, WORK2, WORK3, CONDSAT, SATDEG
      REAL PRATE, AA, BB, CC, B2M4AC, M
      REAL BFREEZ, MLTRAIN, RAIN1, RAIN2
      REAL ABARK, KERSTEN
      REAL SOILCOND1, SOILCOND2
!
      REAL PETIT
      DATA PETIT/1.E-7/
      REAL BETAA 
      DATA BETAA/1.0/                           ! fully-implicit time scheme
!
!     a mettre dans automatic plus tard
      REAL, DIMENSION(NL_SVS)   :: DELZZ    ! (m) thickness of the soil between 2 temperature levels
      REAL, DIMENSION(N,NL_SVS) :: SOILCD   ! W/(m K) two layers averaged soil heat conductivity 
      REAL, DIMENSION(N,NL_SVS) :: A2, B2, C2, D2, A4, B4, C4, D4   

!
!     MULTIBUDGET VARIABLES 
!     GR:ground, SN:snow, VG:vegetation, AG: aggregated 
       real, dimension(n) :: a3, b3, c3, zhv, freezfrac, emvg, &
            alvglai, zqsatgr, zdqsatgr, zqsatvg, zdqsatvg, zqsatgrt, zqsatvgt, &
            zqsatgrv, zdqsatgrv, zqsatgrvt, zqsgrv, zqsvg, &
            rnetgr, rnetvg, hfluxgr, hfluxvg, roragr,roragrv, roravg, tnmt0, freezg, &
            meltg, zqsatsno, tgrst, tgrdt, tvgst, tvgdt, esf, esvf, evf, pnl, pnh, &
            egf, etrf, ev, zqsatsnv, levnofrac, legnofrac, legvnofrac, tsn4, &
            tsnvg4, tvg4, tgr4, cmu, cm, ctu, za, highveg, lowveg, tpvprim, tafprim, diftemp, &
            tgrvs, z0temp, ctugrv, z0hg, ta4flx, qa4flx, zu4flx, zt4flx, vit, z0m4flx, z0h4flx

!   Rename layer thicknesses for local purposes
       DO K=1,NL_SVS
          DZ(K) = DELZ(K)
       END DO

!************************************************************************
!
!
!
!                                THE FOLLOWING SHOULD BE PUT IN 
!                                A COMMON COMDECK
!
!      EMISSN = 0.97
      EMISSN = 0.99   !use XEMISSN instead
      EMSOIL = 0.94
      KCOEF  = 1.E-6
      BFREEZ = 4.
!                                Albedo of Bark (S. Wang, Ecological Modelling, 2005)
      ABARK  = 0.15
!
!
      RAIN1  = 2.8e-8
      RAIN2  = 2.8e-7
! 
!
!
!       1.     GRID-AVERAGED ALBEDO, EMISSIVITY, AND ROUGHNESS LENGTH
!              ------------------------------------------------------
!                          (considering snow surfaces)
!
      DO I=1,N
!
!
!                               Calculate grid-averaged albedo 
!
!                               Using PSNVHA
!        
         ALBT(I) = AG(VEGH(I),VEGL(I),PSNVHA(I),PSNG(I),ALGR(I),&  
                   ALVG(I),ALPHAS(I),ALPHASV(I))

! 
!                               Recalculate vegetation-only albedo to take LAI
!                               effect into account, can only consider it 
!                               if have high Vegetation (i.e. Gamveg greater
!                               or equal to 0.01). In the high vegetation case,
!                               weight albedo of leaves vs. bark according to LAI
!
         IF(GAMVEG(I).GE.0.01)THEN 
            ALVGLAI(I) = MIN( LAI(I)      , LAI0 )  * ALVG(I) / LAI0 &  
                       + MAX( LAI0-LAI(I) , 0.0  )  * ABARK  / LAI0
         ELSE
            ALVGLAI(I) = ALVG(I)
         ENDIF
!
!
!                            
!                               Calculate vegetation-only emissivity,
!                               the only bark emissivity i could find
!                               was for eucalyptus trees which is not
!                               very helpful as the bark is usually very
!                               bright
         EMVG(I)  = EMIS(I) 
!
      END DO
!        2.     LATENT HEAT COEFFICIENTS - CORRECTION DUE TO FREEZING
!               AND MELTING OF SOIL WATER
!               -----------------------------------------------------
!
!                               Using the fraction of frozen water
!                               in the soil, calculate the "effective"
!                               latent heat of evaporation/sublimation
!
        DO I=1,N
           FREEZFRAC(I) = WF(I,1) / (WD(I,1)+WF(I,1)+PETIT)
           LEFF(I)      = FREEZFRAC(I)      * (CHLC+CHLF)  &
                         + (1.-FREEZFRAC(I)) *  CHLC
        END DO
!
!
!
!
!
!       3A.     COEFFICIENTS FOR THE TIME INTEGRATION OF TP(I,K)
!               BARE SOIL TEMPERATURE PROFILE
!               --------------------------------------------
!
!                            Thermodynamic functions
!
       DO I=1,N
          ZQSATGR(I)  = FOQST( TP(I,1),PS(I) )
          ZDQSATGR(I) = FODQS( ZQSATGR(I),TP(I,1) )
       END DO

!
!                            Soil thermo parameters

!      Calculate thermal conductivity heat capacity of soil layer using PL98

       DO I=1,N

!         Thermal conductivity - Mean over two layers
          DO K=1,NL_SVS-1
             SOILCOND1 = SOILCOND(I,K)
             SOILCOND2 = SOILCOND(I,K+1)
             SOILCD(I,K) = (DZ(K)*SOILCOND1 + DZ(K+1)* SOILCOND2) / (DZ(K)+ DZ(K+1))
          END DO
!         special cases
          SOILCD(I,NL_SVS) = SOILCOND(I,NL_SVS)

!         Heat capacity
!!! maintenant passe en entree

       END DO

!
!                              function zrsra
!
       DO I=1,N
          RORAGR(I) = RHOA(I) / RESAGR(I)
       END DO
!
!                              coefficients A, B, C and term D for the
!                              matrix inversion for the calculation of TP(t) 
!
       DO K=1,NL_SVS-1
          DELZZ(K) = (DZ(K) + DZ(K+1)) / 2.
       END DO
       DELZZ(NL_SVS) = DZ(NL_SVS)
!
       DO K=2,NL_SVS-1

          DO I=1,N

             A2(I,K) = (-BETAA) * (DT * SOILCD(I,K-1)) / &
                    (CAP(I,K) * DZ(K) * DELZZ(K-1))

             C2(I,K) = (-BETAA) * (DT * SOILCD(I,K)) / &
                    (CAP(I,K) * DZ(K) * DELZZ(K))

             B2(I,K) = 1.0 - A2(I,K) - C2(I,K)
             
             D2(I,K) = ( (1.-BETAA)*DT / (CAP(I,K)*DZ(K)) ) * &  
                    (  SOILCD(I,K) / DELZZ(K) * (TP(I,K+1) - TP(I,K)) &
                    +  SOILCD(I,K-1) / DELZZ(K-1) * (TP(I,K-1) - TP(I,K)) ) &
                    +  TP(I,K)

          END DO
          
       END DO
!
!                               add the upper boundary condition
!
       DO I=1,N
       IF((1.0-VEGH(I)-VEGL(I)*(1.-PSNG(I))).gt.PETIT) then
          PNL(I) = (1.-VEGH(I))*PSNG(I)/(1.-VEGH(I)-VEGL(I)*(1.-PSNG(I)))
       else
          PNL(I) = 0.0
       endif
       END DO

       DO I=1,N
          B2(I,1) = DZ(1)*CAP(I,1)/DT + (1.-PNL(I)) * ( (4.*EMGR(I)*STEFAN*(TP(I,1)**3)) &
                  +  RORAGR(I)*CPD  &
                  +  RORAGR(I)*LEFF(I)*HRSURF(I)*ZDQSATGR(I) ) &
                  +  BETAA*SOILCD(I,1)/DELZZ(1)
          
          C2(I,1) = (-BETAA)*SOILCD(I,1)/DELZZ(1)
          A2(I,1) = 0.0
          
          D2(I,1) = (1.-PNL(I)) * ( (1.-ALGR(I))*RG(I) + EMGR(I)*RAT(I) &
                  + 3.*EMGR(I)*STEFAN*(TP(I,1)**4) &
                  + RORAGR(I)*CPD*THETAA(I) &
                  + RORAGR(I)*LEFF(I)*HRSURF(I)*ZDQSATGR(I)*TP(I,1) &
                  - RORAGR(I)*LEFF(I)*(HRSURF(I)*ZQSATGR(I)-HU(I)) ) &
                  + (1.-BETAA)*(SOILCD(I,1)/DELZZ(1))*(TP(I,2)-TP(I,1)) &
		  + ( DZ(1)*CAP(I,1)/DT )*TP(I,1) + PNL(I)*PGRNDFLUX(I)
       END DO
!
!
!                               add the lower boundary condition
!
       DO I=1,N

! flux-zero at bottom

!!$          A2(I,NL_SVS) = -(BETAA) * DT * SOILCD(I,NL_SVS-1) /  &
!!$                    (CAP(I,NL_SVS) * DZ(NL_SVS) * DELZZ(NL_SVS-1))
!!$
!!$          B2(I,NL_SVS) = 1. - A2(I,NL_SVS)
!!$          C2(I,NL_SVS) = 0.0
!!$
!!$          D2(I,NL_SVS) = TP(I,NL_SVS) - ( (1. - BETAA) * DT * SOILCD(I,NL_SVS-1) / &
!!$                    (CAP(I,NL_SVS) * DZ(NL_SVS)*DELZZ(NL_SVS-1)) ) * TP(I,NL_SVS) &
!!$                   +( (1. - BETAA) * DT * SOILCD(I,NL_SVS-1) / &
!!$                    (CAP(I,NL_SVS) * DZ(NL_SVS)*DELZZ(NL_SVS-1)) ) * TP(I,NL_SVS-1) 


!         Prescribed T at bottom (will become a option (flux=0 at the bottom is the other option)

          A2(I,NL_SVS) = -BETAA * DT * SOILCD(I,NL_SVS-1) / &
                    (CAP(I,NL_SVS) * DZ(NL_SVS)*DELZZ(NL_SVS-1))

!          B2(I,NL_SVS) = 1. + (BETAA*DT/(CAP(I,NL_SVS)*DZ(NL_SVS)))*(SOILCD(I,NL_SVS-1)/DELZZ(NL_SVS-1) + SOILCD(I,NL_SVS)/DELZZ(NL_SVS))
          B2(I,NL_SVS) = 1. + ( BETAA*DT*SOILCD(I,NL_SVS) ) / ( CAP(I,NL_SVS)*DZ(NL_SVS)*DELZZ(NL_SVS) ) + &
                     ( BETAA*DT*SOILCD(I,NL_SVS-1) ) / ( CAP(I,NL_SVS)*DZ(NL_SVS)*DELZZ(NL_SVS-1) )
          C2(I,NL_SVS) = 0.0

!!!$          D2(I,NL_SVS) = TP(I,NL_SVS) + ( DT * SOILCD(I,NL_SVS) / (CAP(I,NL_SVS) * DZ(NL_SVS)*DELZZ(NL_SVS)) ) * TPERM(I) - &
!!!$               ( (1. - BETAA) * DT * SOILCD(I,NL_SVS-1) / (CAP(I,NL_SVS) * DZ(NL_SVS)*DELZZ(NL_SVS-1)) ) * (TP(I,NL_SVS)-TP(I,NL_SVS-1)) - &
!!!$               ( (1. - BETAA) * DT * SOILCD(I,NL_SVS)   / (CAP(I,NL_SVS) * DZ(NL_SVS)*DELZZ(NL_SVS)) )  * TP(I,NL_SVS)

          D2(I,NL_SVS) = TP(I,NL_SVS) +  ( DT/(CAP(I,NL_SVS)*DZ(NL_SVS)) ) * ( ( SOILCD(I,NL_SVS)/DELZZ(NL_SVS) ) * TPERM(I) &
               - ( (1. - BETAA) * SOILCD(I,NL_SVS-1)/DELZZ(NL_SVS-1) ) * (TP(I,NL_SVS)-TP(I,NL_SVS-1)) &
               - ( (1. - BETAA) * SOILCD(I,NL_SVS)/DELZZ(NL_SVS) ) * TP(I,NL_SVS) )

       END DO
!
!                             matrix inversion
!
!
      CALL DIFUVD2(TP, A2, B2, C2, D2, D2, N, N, NL_SVS)
!
!
       DO I=1,N
          TGRST(I) = TP(I,1)
       ENDDO

!
!       3B.     COEFFICIENTS FOR THE TIME INTEGRATION OF  TVGS
!               (i.e. VEGETATION SKIN TEMPERATURE)
!               --------------------------------------------
!
!                            Thermodynamic functions
!
       DO I=1,N
         ZQSATVG(I)  = FOQST( TVGS(I),PS(I) )
         ZDQSATVG(I) = FODQS( ZQSATVG(I),TVGS(I) )
       END DO
!
!                              function zrsra
!
       DO I=1,N
         RORAVG(I) = RHOA(I) / RESAVG(I)
!
!                              Fraction of snow in high vegetation to total visible vegetation
!          
         IF((VEGH(I)+VEGL(I)*(1.-PSNG(I))).gt.PETIT) then
            PNH(I)   =  (VEGH(I)*PSNVH(I))/(VEGH(I)+VEGL(I)*(1.-PSNG(I)))
         ELSE
            PNH(I)   = 0.0
         ENDIF
!
         PNH(I)   =  MIN(PNH(I) , 1.0)
      END DO
!
!                          
!
!                                        terms za, zb, and zc for the
!                                        calculation of skin veg T --> tvgs(t)
!
      DO I=1,N
!
        A3(I) = 1. / DT + CVP(I) *  & 
                (4. * EMVG(I) * STEFAN * (TVGS(I)**3)  &  
                +  RORAVG(I) * ZDQSATVG(I) * CHLC * HV(I) &  
                +  RORAVG(I) * CPD )  & 
                + 2. * PI / 86400.
!
        B3(I) = 1. / DT + CVP(I) *   &
                (3. * EMVG(I) * STEFAN * (TVGS(I)** 3)  &   
                + RORAVG(I) * ZDQSATVG(I) * CHLC* HV(I) )
           
!
        C3(I) = 2. * PI * TVGD(I) / 86400. &   
                + CVP(I) *  & 
                ( RORAVG(I) * CPD * THETAA(I)  &  
                + RG(I) * (1. - ALVGLAI(I)) + EMVG(I)*RAT(I)  & 
                + PNH(I) * (1. - SKYVIEW(I))* EMISSN * STEFAN * (TSVS(I,1)**4)  &
                - RORAVG(I) * CHLC * HV(I) * (ZQSATVG(I)-HU(I)) )
!
     END DO

!
      DO I=1,N

!                   Note that as an added precaution,
!                   we set the vegetation temperature to
!                   that of the ground, when no vegetation is present
!
         IF(VEGH(I)+VEGL(I)*(1.-PSNG(I)).gt.PETIT) then
            TVGST(I) =  ( TVGS(I)*B3(I) + C3(I) ) / A3(I)
         ELSE  
            TVGST(I) = TGRST(I)
         ENDIF

      ENDDO
!
!
!!$!        4.     FREEZING AND MELTING TENDENCIES FOR SOIL WATER
!!$!               ----------------------------------------------


!     SET DWATERDT to zero, just to initialize array
      DWATERDT=0.0
!
!
!       5.A     TGRD AT TIME 'T+DT'
!               ------------------
!
      DO I=1,N
        TGRDT(I) = (TGRD(I) + DT*TGRST(I)/86400.) /   &  
                      (1.+DT/86400.)
!
!
!
!                            Include the effect of soil freeze/thaw
! note dwaterdt already multiplied by DT above, so have DT^2 ...below
! check if this is ok
! ** DO NOT INCLUDE THE EFFECT OF DWATERDT BECAUSE SET TO ZERO ABOVE !
!

        TGRDT(I) = TGRDT(I) 

      END DO
!
!       5.B     TVGD AT TIME 'T+DT'
!               -----------------
!
      DO I=1,N
!                   Note that as an added precaution,
!                   we set the vegetation temperature to
!                   that of the ground, when no vegetation is present
!
         IF(VEGH(I)+VEGL(I)*(1.-PSNG(I)).gt.PETIT)THEN
            TVGDT(I) = (TVGD(I) + DT*TVGST(I)/86400.) / (1.+DT/86400.)
         ELSE
            TVGDT(I) = TGRDT(I)
         ENDIF
            
      END DO

!       3C.     COEFFICIENTS FOR THE TIME INTEGRATION OF TPV(I,K)
!               SOIL UNDER VEGETATION SKIN TEMPERATURE
!               --------------------------------------------

!                         first calculate the saturation vapor
!                         pressure over ground under canopy
!
      DO I=1,N
         ZQSATGRV(I)  = FOQST( TPV(I,1), PS(I) )
         ZDQSATGRV(I) = FODQS( ZQSATGRV(I),TPV(I,1) )
         ZQSATVGT(I)  = FOQST( TVGST(I), PS(I) )
      END DO

!                         calculate the low veg and high veg fractions
!                         relative to total snow free vegetation 
!
      DO I=1,N
         IF((VEGH(I)*(1.-PSNVH(I))+VEGL(I)*(1.-PSNG(I))).gt.PETIT) then
            LOWVEG(I)  = VEGL(I)*(1.-PSNG(I))   / (VEGL(I)*(1.-PSNG(I))+VEGH(I)*(1.-PSNVH(I)))
            HIGHVEG(I) = VEGH(I)*( 1.-PSNVH(I)) / (VEGL(I)*(1.-PSNG(I))+VEGH(I)*(1.-PSNVH(I)))
         ELSE
            LOWVEG(I)  = 0.0
            HIGHVEG(I) = 0.0
         ENDIF
      END DO

      DO I=1,N
!!$        ZQSGRV(I) = HRSURF(I) * ZQSATGRV(I)
!!$        ZQSVG(I)  = HV(I)     * ZQSATVGT(I) + ( 1. - HV(I) ) * HU(I)

         IF(HIGHVEG(I).gt.LOWVEG(I))THEN
            ZQSGRV(I) = HRSURF(I) * ZQSATGRV(I)
            ZQSVG(I) = RPP(I) * ZQSATVGT(I) + ( 1. - RPP(I) ) * QAF(I)
         ELSE
            ZQSGRV(I) = HRSURF(I) * ZQSATGRV(I)
            ZQSVG(I) = HV(I) * ZQSATVGT(I) + ( 1. - HV(I) ) * HU(I)
         ENDIF
      END DO

      DO I=1,N 
!         VAF(I) = 0.7 * VMOD(I)   !low veg
         VAF(I) = 0.3 * VMOD(I)   !high veg
         TAF(I) = (0.3 * THETAA(I) + 0.6 *TVGST(I) + 0.1 * TPV(I,1))    ! Temp. inside canopy
         QAF(I) = (0.3 * HU(I) + 0.6 * ZQSVG(I) + 0.1 * ZQSGRV(I))      ! Hum.  inside canopy
      END DO
     
     DO I=1,N
        ZA(I) = 2. * VGHEIGHT(I) / 3.   ! reference height in canopy for flux
        ZA(I) = max(2.0,ZA(I))
!!$         Z0TEMP(I) = 0.05                ! bare ground local momentum roughness
!!$         Z0HG(I)=0.01                    ! bare ground local heat roughness
        Z0TEMP(I) = 1.0                 ! bare ground local momentum roughness
        Z0HG(I)=0.2                     ! bare ground local heat roughness
     END DO

     DO I=1,N
        TGRVS(I) = TPV(I,1) 
     END DO

     DO I=1,N
        IF(HIGHVEG(I).gt.LOWVEG(I))THEN
           TA4FLX(I)  = TAF(I)
           QA4FLX(I)  = QAF(I)
           ZU4FLX(I)  = ZA(I)
           ZT4FLX(I)  = ZA(I)
           VIT(I)     = VAF(I)
           Z0M4FLX(I) = Z0TEMP(I)
           Z0H4FLX(I) = Z0HG(I)
        ELSE
           TA4FLX(I)  = THETAA(I)
           QA4FLX(I)  = HU(I)
           ZU4FLX(I)  = ZUSL(I)
           ZT4FLX(I)  = ZTSL(I)
           VIT(I)     = VMOD(I)
           Z0M4FLX(I) = Z0(I)
           Z0H4FLX(I) = Z0HA(I)
        ENDIF
     END DO

!
   i = sl_sfclayer( TA4FLX, QA4FLX, VIT, VDIR, ZT4FLX, ZT4FLX, &
                    TGRVS, ZQSGRV, Z0M4FLX, Z0H4FLX, LAT, FCOR, &
                    coeft=CTUGRV )

   if (i /= SL_OK) then
      print*, 'Aborting in drag_svs() because of error returned by sl_sfclayer()'
      stop
   endif 


      DO I=1,N
        RESAGRV(I) = 1. / CTUGRV(I)
      END DO
!

!!$      DO I=1,N
!!$         CMUGRV(I) = CMUGRV(I) / UEGRV(I)
!!$         CHGRV(I) = CMUGRV(I) * CTUGRV(I) / UEGRV(I)
!!$
!!$         IF(HIGHVEG(I).gt.LOWVEG(I))THEN
!!$            RESAGRV(I) = 1. / CHGRV(I) / (VAF(I)+0.001) 
!!$         ELSE
!!$            RESAGRV(I) = 1. / CHGRV(I) / (VMOD(I)+0.001) 
!!$         ENDIF
!!$      END DO

!                              function zrsra ground under veg

      DO I=1,N
         RORAGRV(I) = RHOA(I) / RESAGRV(I)

!                              drag coef from CLASS for high veg only

         IF(HIGHVEG(I).gt.LOWVEG(I))THEN

            TPVPRIM(I) = TPV(I,1)*(1.0 + 0.61 * ZQSGRV(I) )
            TAFPRIM(I) = TAF(I)*(1.0 + 0.61 * QAF(I) )
            DIFTEMP(I) = TPVPRIM(I) - TAFPRIM(I)
          
            IF (DIFTEMP(I).GT.1.0 )THEN
               RORAGRV(I) = RHOA(I) * (0.0019 * (TPVPRIM(I) - TAFPRIM(I))**(1./3.) )
            ELSEIF (DIFTEMP(I).GT.0.001 .and. DIFTEMP(I).LE.1.0) THEN
               RORAGRV(I) = RHOA(I) * (0.0019 * (TPVPRIM(I) - TAFPRIM(I)) )
            ELSE
               RORAGRV(I) = 0.0
            ENDIF

         ENDIF

      END DO
   !
!
!                              coefficients A, B, C and term D for the
!                              matrix inversion for the calculation of TPV(t) 
!
       DO K=2,NL_SVS-1

          DO I=1,N

             A4(I,K) = -BETAA * (DT * SOILCD(I,K-1)) / &
                    (CAP(I,K) * DZ(K) * DELZZ(K-1))

             C4(I,K) = (-BETAA) * (DT * SOILCD(I,K)) / &
                    (CAP(I,K) * DZ(K) * DELZZ(K))

             B4(I,K) = 1.0 - A4(I,K) - C4(I,K)

             D4(I,K) = (1. - BETAA) * DT / (CAP(I,K) * DZ(K)) * &  
                    (  SOILCD(I,K) / DELZZ(K) * (TPV(I,K+1) - TPV(I,K)) &
                    +  SOILCD(I,K-1) / DELZZ(K-1) * (TPV(I,K-1) - TPV(I,K)) ) &
                    +  TPV(I,K)

          END DO

       END DO
!                   
!                               add the upper boundary condition
!
       DO I=1,N
          B4(I,1) = DZ(1)*CAP(I,1)/DT + (1.-PNH(I)) * ( 4.*EMGR(I)*STEFAN*(TPV(I,1)**3) &
                +  RORAGRV(I)*CPD &
                +  RORAGRV(I)*LEFF(I)*HRSURF(I)*ZDQSATGRV(I) ) &
                +  BETAA*SOILCD(I,1)/DELZZ(1)

          C4(I,1) = (-BETAA)*SOILCD(I,1)/DELZZ(1)
          A4(I,1) = 0.0

          IF(HIGHVEG(I).gt.LOWVEG(I))THEN

             D4(I,1) = (1.-PNH(I)) * ( VTRA(I)*(1.-ALGR(I))*RG(I) + SKYVIEWA(I)*EMGR(I)*RAT(I) &
                  + 3.*EMGR(I)*STEFAN*(TPV(I,1)**4) &
                  + (1.-SKYVIEWA(I))*EMGR(I)*EMVG(I)*STEFAN*(TVGDT(I)**4) &
                  + RORAGRV(I)*CPD*TAF(I) &
                  + RORAGRV(I)*LEFF(I)*HRSURF(I)*ZDQSATGRV(I)*TPV(I,1) &
                  - RORAGRV(I)*LEFF(I)*(HRSURF(I)*ZQSATGRV(I)-QAF(I)) )  &
                  + (1.-BETAA)*(SOILCD(I,1)/DELZZ(1))*(TPV(I,2)-TPV(I,1)) &
		  + DZ(1)*CAP(I,1)/DT*TPV(I,1) + PNH(I)*PGRNDFLUXV(I)
!            deboguage
             HGV(I) = RORAGRV(I) * CPD * (TPV(I,1) - TAF(I) )
             LGV(I) = RORAGRV(I) * LEFF(I) * (HRSURF(I)*ZQSATGRV(I) - QAF(I) )

          ELSE

             D4(I,1) = (1.-PNH(I)) * ( VTRA(I)*(1.-ALGR(I))*RG(I) + SKYVIEWA(I)*EMGR(I)*RAT(I) &
                  + 3.*EMGR(I)*STEFAN*(TPV(I,1)**4) &
                  + (1.-SKYVIEWA(I))*EMGR(I)*EMVG(I)*STEFAN*(TVGST(I)**4) &
                  + RORAGRV(I)*CPD*THETAA(I) &
                  + RORAGRV(I)*LEFF(I)*HRSURF(I)*ZDQSATGRV(I)*TPV(I,1) &
                  - RORAGRV(I)*LEFF(I)*(HRSURF(I)*ZQSATGRV(I)-HU(I)) ) &
                  + (1.-BETAA)*(SOILCD(I,1)/DELZZ(1))*(TPV(I,2)-TPV(I,1)) &
		  + DZ(1)*CAP(I,1)/DT*TPV(I,1) + PNH(I)*PGRNDFLUXV(I)
!            deboguage
             HGV(I) = RORAGRV(I) * CPD * (TPV(I,1) - THETAA(I) )
             LGV(I) = RORAGRV(I) * LEFF(I) * (HRSURF(I)*ZQSATGRV(I) - HU(I) )

          ENDIF

! pour debogage
          RGVG(I) = VTRA(I) * (1. - ALGR(I)) * RG(I)
          FIVG(I) = SKYVIEWA(I) * EMGR(I) * RAT(I)
          IRGV(I) = EMGR(I) * STEFAN * (TPV(I,1)**4)
          IRVG(I) = (1. - SKYVIEWA(I)) * EMGR(I) * EMVG(I) * STEFAN * (TVGST(I)**4)
!          IRVG(I) = (1. - SKYVIEWA(I)) * EMGR(I) * EMVG(I) * STEFAN * (TVGDT(I)**4)
!         specific humidity over ground under vegetation
          QGV(I) = HRSURF(I)*ZQSATGRV(I)
!         net radiative and energy balance over ground under vegetation
          RNGV(I) = RGVG(I) + FIVG(I) - IRGV(I) + IRVG(I)
          FGRV(I) = RGVG(I) + FIVG(I) - IRGV(I) + IRVG(I) - HGV(I) - LGV(I)
          grflux(I)=(TPV(I,2)-TPV(I,1))*SOILCD(I,1)/DELZZ(1)
! fin pour debogage

       END DO

!                               add the lower boundary condition

       DO I=1,N

!         Prescribed T at bottom (will become an option with flux=0 at the last level as another option)

          A4(I,NL_SVS) = -BETAA * DT * SOILCD(I,NL_SVS-1) / &
                    (CAP(I,NL_SVS) * DZ(NL_SVS)*DELZZ(NL_SVS-1))

          B4(I,NL_SVS) = 1. + (BETAA*DT/(CAP(I,NL_SVS)*DZ(NL_SVS)))*(SOILCD(I,NL_SVS-1)/DELZZ(NL_SVS-1) + SOILCD(I,NL_SVS)/DELZZ(NL_SVS))
          C4(I,NL_SVS) = 0.0

          D4(I,NL_SVS) = TPV(I,NL_SVS) +  ( DT/(CAP(I,NL_SVS)*DZ(NL_SVS)) ) * ( ( SOILCD(I,NL_SVS)/DELZZ(NL_SVS) ) * TPERM(I) &
               - ( (1. - BETAA) * SOILCD(I,NL_SVS-1)/DELZZ(NL_SVS-1) ) * (TPV(I,NL_SVS)-TPV(I,NL_SVS-1)) &
               - ( (1. - BETAA) * SOILCD(I,NL_SVS)/DELZZ(NL_SVS) ) * TPV(I,NL_SVS) )

       END DO

!
!                             matrix inversion
!
      CALL DIFUVD2(TPV, A4, B4, C4, D4, D4, N, N, NL_SVS)
!
!
!
!        6.     CALCULATE NEW SOIL TEMPERATURE (TS) PROFILE
!               BASED ON MULTIBUDGET TEMPERATURES
!               ----------------------------------------------
!
      DO I=1,N
!
         DO K=1,NL_SVS

            TS(I,K) = (1. - VEGH(I) - VEGL(I) ) * (1. - PSNG(I) )                 * TP(I,K) & 
                     + (VEGH(I) * (1. - PSNVH(I)) + VEGL(I) * (1. - PSNG(I)))     * TPV(I,K) &  
                     + (1. - VEGH(I)) * PSNG(I)                                   * TP(I,K)  & 
                     +       VEGH(I)  * PSNVH(I)                                  * TPV(I,K)
! or more simple because tsnow(2) doesn't exist anymore
!!$            TS(I,K) = (1. - VEGH(I) - VEGL(I)*(1. - PSNG(I)) )                 * TP(I,K) & 
!!$                     + (VEGH(I) + VEGL(I) * (1. - PSNG(I)))                    * TPV(I,K)

         END DO
!
      END DO
!
!
!        7.     FLUX CALCULATIONS
!               -----------------
!

      DO I=1,N
!                                            recalculate the qsat functions
!
        ZQSATGRT(I)  = FOQST(  TGRST(I)  ,  PS(I)   )
        ZQSATVGT(I) =  FOQST(  TVGST(I)  ,  PS(I)   )
        ZQSATGRVT(I) = FOQST(  TPV(I,1)  ,  PS(I)   )
!
      ENDDO
!                     
!
      DO I=1,N
!
!                                            NET RADIATION 
!                                            ---------------
!
!                                            Net radiation over bare ground
!
        RNETGR(I) = (1. - ALGR(I)) * RG(I) + EMGR(I) *&  
                 (RAT(I) - STEFAN * (TGRST(I)** 4))
!
!                                            Net radiation over vegetation
!
        RNETVG(I) = (1. - ALVGLAI(I)) * RG(I) + EMVG(I) *&  
                 (RAT(I) - STEFAN * (TVGST(I)** 4))
!
!                                            AGGREGATED net radiation (including snow)
!                                    
        RNET(I) = AG(VEGH(I),VEGL(I),PSNVHA(I),PSNG(I),RNETGR(I),&  
                  RNETVG(I),RNETSN(I),RNETSV(I))

!        
!
!                                            SENSIBLE HEAT FLUX 
!                                            ---------------
!
!
!                                            Sensible heat flux from the ground
!
        HFLUXGR(I) = RHOA(I) * CPD * (TGRST(I) - THETAA(I)) / RESAGR(I)
!
!                                            Sensible heat flux from the vegetation
!
        HFLUXVG(I) = RHOA(I) * CPD * (TVGST(I) - THETAA(I)) / RESAVG(I)
!
!                                             AGGREGATED sensible heat flux (including snow)
        HFLUX(I)   = AG(VEGH(I),VEGL(I),PSNVHA(I),PSNG(I),HFLUXGR(I),&  
                   HFLUXVG(I),HFLUXSN(I),HFLUXSV(I))
!
!
!
!                                             AGGREGATED turbulent surface flux of temperature
!
!        FTEMP(I) = HFLUX(I) / ( RHOA(I) * CPD )
!
!
!
!                                            LATENT HEAT FLUXES 
!                                            ---------------
!
!
!                                            Latent heat of evaporation from
!                                            the ground
!
        LEGNOFRAC(I) = RHOA(I) * LEFF(I) * (HRSURF(I)* ZQSATGRT(I) - HU(I)) / RESAGR(I)
        LEG(I) = (1.-VEGH(I)-VEGL(I))*(1.-PSNG(I)) * LEGNOFRAC(I)  

!
!                                            Latent heat of evaporation from
!                                            the ground under vegetation
!
        LEGVNOFRAC(I) = RORAGRV(I) * LEFF(I) * (HRSURF(I)* ZQSATGRVT(I) - HU(I))
        LEGV(I) = (1.-VEGH(I)-VEGL(I))*(1.-PSNG(I)) * LEGVNOFRAC(I)
!
!
!                                            Water vapor flux from ground
        EGF(I) = (1.-VEGH(I)-VEGL(I))*(1.-PSNG(I))  &
                     * (HRSURF(I)* ZQSATGRT(I) - HU(I)) / RESAGR(I)
!
!                                            Evaporation rate from ground (for hydro_svs.ftn)
!
        EG(I) = RHOA(I)*(HRSURF(I)* ZQSATGRT(I) - HU(I)) / RESAGR(I)
!
!                                            Latent heat of evaporation from
!                                            vegetation
!
        LEVNOFRAC(I) = RHOA(I) * CHLC * HV(I) * (ZQSATVGT(I) - HU(I)) / RESAVG(I)

!                                            Evaporation rate from vege. (for hydro_svs.ftn)
!
        EV(I) =  RHOA(I)*HV(I) * (ZQSATVGT(I) - HU(I)) / RESAVG(I)
!
!
!                                            Latent heat of Transpiration
!
!
        ZHV(I) = MAX(0.0 , SIGN(1.,ZQSATVGT(I) - HU(I)))
        LETR(I) = ZHV(I) * (1. - DEL(I)) * RHOA(I)  &
                      * CHLC * (VEGH(I) * (1. - PSNVHA(I)) + VEGL(I) * (1. - PSNG(I))) & 
                      * (ZQSATVGT(I)  - HU(I)) / (RESAVG(I) + RS(I))
!
!                                           Transpiration rate (for hydro_svs.ftn)
!
        ETR(I) = RHOA(I)*ZHV(I)*(1. - DEL(I))*(ZQSATVGT(I) - HU(I))/(RESAVG(I) + RS(I))
!

!  EV is limited to WR/DT+RR+ETR to avoid negative WR in hydro_svs when direct evaporation exceeds rainrate
!  When snow is present, rain falls through vegetation to snow bank... so is not considered in evaporation...
!  This is to conserve water budget.


        IF(VEGH(I)==0.)  THEN ! No high veg cover the grid
            IF(VEGL(I)>0.) THEN ! Presence of low veg
              IF (SNDP(I).GE.XSNOWDMIN)THEN
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)))
              ELSE
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)+RR(I)))
              ENDIF
            ENDIF
        ELSE IF(1-VEGH(I)>0.) THEN   ! High veg does not fully cover the grid
            IF(VEGL(I)>0.) THEN ! Presence of low veg
              IF (SVDP(I).GE.XSNOWDMIN) THEN
                  IF(SNDP(I).GE.XSNOWDMIN) THEN
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)))
                  ELSE 
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)+RR(I)*VEGL(I)/(VEGL(I)+VEGH(I))))
                  ENDIF
              ELSE
                  IF(SNDP(I).GE.XSNOWDMIN) THEN
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)+RR(I)*VEGH(I)/(VEGL(I)+VEGH(I))))
                  ELSE 
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)+RR(I)))
                  ENDIF
              ENDIF
            ELSE ! No low veg
              IF (SVDP(I).GE.XSNOWDMIN)THEN
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)))
              ELSE
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)+RR(I)))
              ENDIF
            ENDIF
        ELSE ! High veg fully covers the grid
              IF (SVDP(I).GE.XSNOWDMIN)THEN
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)))
              ELSE
                    EV(I) = MIN (EV(I),(WR(I)/DT+ETR(I)+RR(I)))
              ENDIF  
        ENDIF


!                                            Water vapor flux from vegetation

        EVF(I) =  (VEGH(I) * (1. - PSNVHA(I)) + VEGL(I) * (1. - PSNG(I))) * EV(I)/RHOA(I)


!                                            Latent heat of evaporation from vegetation
        LEV(I) = RHOA(I) * CHLC*EVF(I)
!
!                                            Direct evapo. rate from veg.(for hydro_svs.ftn)
!
        ER(I) = EV(I) - ETR(I)
!
!                                            Latent heat of direct evaporation
!
        LER(I)  = LEV(I) - LETR(I)
!
!                                            Calculate latent heat snow weighted
!                                            by grid-cell snow-coverage fraction
!       
        LES(I)  = (1. - VEGH(I)) * PSNG(I) *  (LESLNOFRAC(I) + LESSNOFRAC(I))
        ESF(I)  = (1. - VEGH(I)) * PSNG(I) *  ESNOFRAC(I)/RHOA(I)  ! ESF in m/s
!
!                                            Same for snow-under-vegetation
!
        LESV(I) =  VEGH(I) * PSNVHA(I)  *  (LESLVNOFRAC(I) + LESSVNOFRAC(I))
        ESVF(I)  =  VEGH(I) * PSNVHA(I) *  ESVNOFRAC(I)/RHOA(I)   ! ESVF in m/s
!
!                                            Total latent heat of evaporation
!                                            (Including snow contribution)
!
        LE(I) = LEG(I) + LEV(I) + LES(I) + LESV(I)
!
!                                            Total water vapor flux
!                                            (Including snow contribution)
        EFLUX(I) = EGF(I) + EVF(I) + ESF(I) + ESVF(I)
!        FVAP(I)  = EFLUX(I)
!
!                                            Heat flux into the ground
!
        GFLUX(I) = RNET(I) - HFLUX(I) - LE(I)
!
      ENDDO
!
!
!
!
!*       8.     NEW GRID-AVERAGED QUANTITIES  (FRV, ZQS, BM, FQ)
!               -----------------------------------------------  
!
!
!
      DO I=1,N
!                                             Re-calculate snow saturation humidity
!                                             instead of passing it from snow_alone.ftn
!                                             (N.B. snow always saturated 
!                                             i.e., specific=saturation humidity)
!
        ZQSATSNO(I)=FOQST( TSNS(I,1), PS(I) )
        ZQSATSNV(I)=FOQST( TSVS(I,1), PS(I) )
!
!                                             Calculate grid-averaged specific humidity
!
        ZQS(I) = (1.-VEGH(I)-VEGL(I))*(1.-PSNG(I))       * HRSURF(I)           * ZQSATGRT(I) & 
                   + (1.-VEGH(I))*PSNG(I)                                      * ZQSATSNO(I)&  
                   +  VEGH(I)*PSNVHA(I)                                        * ZQSATSNV(I)&  
                   + (VEGH(I)*(1.-PSNVHA(I))+VEGL(I)*(1.-PSNG(I))) *    HV(I)  * ZQSATVGT(I) & 
                   + (VEGH(I)*(1.-PSNVHA(I))+VEGL(I)*(1.-PSNG(I))) *(1.-HV(I)) * HU(I)
!
!                                             Calculate grid-averaged surface temperature
!                                             i.e., aggregate skin temperatures of diff.
!                                             surfaces 
!
        TSA(I) = AG(VEGH(I),VEGL(I),PSNVHA(I),PSNG(I),TGRST(I),TVGST(I),TSNS(I,1),TSVS(I,1))

!            Calculated averaged radiative Temperature

        TSN4    = TSNS(I,1)**4
        TSNVG4  = TSVS(I,1)**4
        TVG4    = TVGST(I)**4
        TGR4    = TGRST(I)**4
    
        TRAD(I)= AG(VEGH(I),VEGL(I),PSNVHA(I),PSNG(I),TGR4(I),TVG4(I),TSN4(I),TSNVG4(I))
        
        TRAD(I)=TRAD(I)**(1./4.)


      ENDDO
!
!
!                                             Calculate surface layer transfer coefficients
!                                             and fluxes using ZQS and TSA. 
!                                             Here want FRV (average friction velocity for 
!                                             grid-cell, used in diasurf2.ftn), CMU and 
!                                             CTU(Turbulent transfer coeff. for thermodynamics) 
!
!

! DO we still need FRV (UE), ILMO, HST ???? !!!! as output
!
!
      i = sl_sfclayer( THETAA, HU, VMOD, VDIR, ZUSL, ZTSL, &
                       TSA, ZQS, Z0, Z0H, LAT, FCOR, &
                       coefm =CMU, coeft=CTU, &
                       flux_t=FTEMP, flux_q=FVAP, &
                       ilmo=ILMO, ue=FRV, h=HST )

      if (i /= SL_OK) then
         print*, 'Aborting in ebudget_svs() because of error returned by sl_sfclayer()'
         stop
      endif


      DO I=1,N
!
!
!        TERMS SAME FOR IMPLICIT AND EXPLICIT FORMULATION
        
         CM(I)  = CMU(I) / FRV(I)

         bm(i) = vmod(i) * (cm(i)**2)  

         fq(i) = rhoa(i) * bm(i) * vmod(i)

      ENDDO

      
      ! -------  EXPLICIT FORMULATION
      IF (.NOT.IMPFLX) THEN
         DO I=1,N   
 
         !   inhomogeneous boundary term in the diffusion equation for Theta
         !   alfat = ctu * (ta -t_surf) = -ftemp
            ALFAT(I)   =  -FTEMP(I)
!            inhomogeneous boundary term in the diffusion equation for Q
         !   alfaq = ctu * (qa -q_surf) = -fvap
            ALFAQ(I)   =  -FVAP(I)

         ! homogeneous boundary condition term in the
         ! diffusion equation for T and Q          
            BT(I) = 0.0

         ENDDO
      ENDIF


      ! -------  IMPLICIT FORMULATION
      IF (IMPFLX) THEN
         DO I=1,N   
      
         !  inhomogeneous boundary term in the diffusion equation for Theta
            ALFAT(I)   =  -CTU(I)  * TSA(I)

         ! inhomogeneous boundary term in the diffusion equation for Q
            ALFAQ(I)   =  -CTU(I)  * ZQS(I)

         ! homogeneous boundary condition term in the
         ! diffusion equation for T and Q          
            BT(I) = CTU(I)

         ENDDO
      ENDIF

!
!*       9.     UPDATE TEMPERATURE VARIABLES
!              -----------------------------
!
      DO I=1,N
        TGRS(I)   = TGRST(I)
        TGRD(I)   = TGRDT(I)
        TVGS(I)   = TVGST(I)
        TVGD(I)   = TVGDT(I)
      ENDDO
!
!
      RETURN
      END
