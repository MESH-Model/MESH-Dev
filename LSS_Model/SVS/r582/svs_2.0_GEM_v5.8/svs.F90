
!-------------------------------------- LICENCE BEGIN ------------------------------------
!Environment Canada - Atmospheric Science and Technology License/Disclaimer,
!                     version 3; Last Modified: May 7, 2008.
!This is free but copyrighted software; you can use/redistribute/modify it under the terms
!of the Environment Canada - Atmospheric Science and Technology License/Disclaimer
!version 3 or (at your option) any later version that should be found at:
!http://collaboration.cmc.ec.gc.ca/science/rpn.comm/license.html
!
!This software is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
!without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!See the above mentioned License/Disclaimer for more details.
!You should have received a copy of the License/Disclaimer along with this software;
!if not, you can write to: EC-RPN COMM Group, 2121 TransCanada, suite 500, Dorval (Quebec),
!CANADA, H9P 1J3; or send e-mail to service.rpn@ec.gc.ca
!-------------------------------------- LICENCE END --------------------------------------
!** S/P SVS
!
subroutine svs(BUS, BUSSIZ, PTSURF, PTSURFSIZ, DT, KOUNT, TRNCH, N, M, NK)
   use sfclayer_mod, only: sl_prelim,sl_sfclayer,SL_OK

!VV DEBUT MODIFICATION POUR MESH
#ifdef RUNSVS
    use runsvs_mod
#endif
!   use sfcbus_mod
!VV FIN MODIFICATION POUR MESH
   use sfc_options
   use svs_configs
   implicit none
#include <arch_specific.hf>
!
!Author
!          S. Belair (January 1997), M. Abrahamowicz, S.Z. Husain (2012) 
!Revisions
!
! 001      Rewrite ISBA into SVS (multi-budget, multi-layer isba)
!              Add sea ice surface
!Object
!          Multitasking of the surface scheme SVS
!
!Arguments
!
!               - Input/Output -
! BUS           bus of surface variables
!
!               - Input -
! BUSSIZ        size of the surface bus
! PTSURF        surface pointers
! PTSURFSIZ     dimension of ptsurf
! KOUNT         number of timestep
! TRNCH         row number
! DT            timestep
! N             running length
! M             horizontal dimension
! NK            vertical dimension
!
!
!

   integer BUSSIZ, LONCLEF, VSIZ, N, NK, KOUNT, TRNCH
   real DT
   real,target :: bus(bussiz)
   integer PTSURFSIZ
   integer PTSURF(PTSURFSIZ)
   include "thermoconsts.inc"

   integer SURFLEN

!VV DEBUT MODIFICATION POUR MESH
integer ptr, x, j, k
#ifdef RUNSVS
#include "xptsurf.cdk"
#endif
! WARNING !!!! x in bus(x(varname,1,1)) is defined in the line below
! it is now case sensitive
!#define x(fptr,fj,fk) ptsurf(vd%fptr%i)+(fk-1)*surflen+fj-1
! so defined X() also to make it NOT case sensitive
!#define X(fptr,fj,fk) x(fptr,fj,fk)
!VV FIN MODIFICATION POUR MESH


   integer, parameter :: INDX_SFC = INDX_SOIL

   logical, parameter :: TDIAGLIM_FALSE = .false.



!     
! LOCAL ARRAYS defined for variables passed to 
! explicit interface in sl_prelim, sl_sfcmod ... need to pass arrays not address of first
! element, so use:
! bus(x(varname,i,k) :)        instead of 
! bus(x(varname,i,k)  )
! PASSING BUSES WILL NOT WORK FOR EXPLICIT INTERFACE... DIMENSION of VARIABLES
! DEFINED LOCALLY based on size of first variable... which in this case is WHOLE! BUS

   real,pointer,dimension(:) :: hum
   real,pointer,dimension(:) :: psm
   real,pointer,dimension(:) :: ttm
   real,pointer,dimension(:) :: uum
   real,pointer,dimension(:) :: vvm
   real,pointer,dimension(:) :: z0h
   real,pointer,dimension(:) :: z0m
   real,pointer,dimension(:) :: zdlat
   real,pointer,dimension(:) :: zfcor
   real,pointer,dimension(:) :: zqdiag
   real,pointer,dimension(:) :: zqsurf
!VV DEBUT MODIFICATION POUR MESH
!   real,pointer,dimension(:) :: zsnodp
!VV FIN MODIFICATION POUR MESH
   real,pointer,dimension(:) :: ztdiag
   real,pointer,dimension(:) :: zthetaa
   real,pointer,dimension(:) :: ztsa
   real,pointer,dimension(:) :: zudiag
   real,pointer,dimension(:) :: zvdiag
   real,pointer,dimension(:) :: zzusl
   real,pointer,dimension(:) :: zztsl

!
!

!******************************************************
!     LOCAL ARRAYS  --- ALPHABETICAL ORDER
!******************************************************
!
   include "nsl_snowES.cdk"
! to initiate number of layers in the snowpack

!VV DEBUT MODIFICATION POUR MESH
!   integer i,k,j,m, masklat50(n)
   integer i,m, masklat50(n)
!VV FIN MODIFICATION POUR MESH

   real,dimension(n) :: alva, cg, cvpa, del, dwaterdt
   real,dimension(n) :: esnofrac, esvnofrac, eva, gamva, hrsurf
   real,dimension(n) :: leff, lesnofrac, lesvnofrac, rainrate_mm
   real,dimension(n) :: rgla, rhoa, snowrate_mm, stom_rs, stomra, rpp
   real,dimension(n) :: suncosa, sunother1, sunother2, sunother3
   real,dimension(n) :: sunother4, trad, tva, vdir, vmod,  wrmax, wvegt

!  \\\\\------------------------------------\\\\\
!  for snowES only (to reorganise and clean some var)
   real,dimension(n) :: pct, pz0hnat, pz0
!   real,dimension(n) :: pthrufal, pgrndflux, prnsnow, phsnow, pgfluxsnow, phpsnow
   real,dimension(n) :: pgrndflux, prnsnow, phsnow, pgfluxsnow, phpsnow
   real,dimension(n) :: pleses, pleles, pevap
!   real,dimension(n) :: pthrufal_v, pgrndflux_v, prnsnow_v, phsnow_v, pgfluxsnow_v, phpsnow_v
   real,dimension(n) :: pgrndflux_v, prnsnow_v, phsnow_v, pgfluxsnow_v, phpsnow_v
   real,dimension(n) :: pleses_v, pleles_v, pevap_v

   real,dimension(n) :: psr_si, prr_si
   real,dimension(n) :: prg_veg    ! Surface incoming shortwave radiation under high vegetation
   real,dimension(n) :: prat_veg   ! Surface incoming longwave radiation under high vegetation

     ! NL_SVS VARIABLES
   real, dimension(n,nl_svs) :: ptg, psoilhcapz, psoilcondz, pd_g, pdzg
   real, dimension(n,nl_svs) :: ptg_v, psoilhcapz_v
!
     ! NSL VARIABLES
   real, dimension(n,nsl) :: pssa, pssav
!  fin snowES
!  \\\\\\------------------------------------\\\\\
! 
   real, dimension(n,nl_svs) :: d, isoilt, wsoilt, wsoiltt
   real, dimension(n,nl_svs) :: wft, wftv, wftg, wdttv, wdttg
   real, dimension(n,nl_svs) :: delwatgr, delwatvg, delicegr, delicevg
!
!******************************************************
!
      real,pointer,dimension(:) :: zfsolis
!      
      REAL HZ, HZ0, JULIEN, JULIAND    
!
!
!---------------------------------------------------------
!  ROUGNESS
!

!  GENERAL COMMENT:
!   want z0m *with orography* at resolution coarser than 3km... 
!  and local onl_svsy at higher resolution. Makes sure that the local z0 
!  calculated based on veg. height database, and NOT
!  on look-up table (values are quite weak...)
!
!     Conversion factor to convert from momemtum roughness to thermal roughness
      REAL, PARAMETER :: Z0M_TO_Z0H = 0.2
!
!     Thermal roughness for snow
      REAL, PARAMETER :: Z0HSNOW = 0.010
!
!     Momentum roughness for bare ground 
      REAL, PARAMETER :: Z0MBG = 0.05
!---------------------------------------------------------
!
!     In the offline mode the t-step 0 is (correctly) not performed
      IF (FLUVERT.EQ.'SURFACE'.AND.KOUNT.EQ.0) RETURN
!
      SURFLEN = M

! assign pointers
      hum      (1:n) => bus( x(humoins,1,nk)     : )
      psm      (1:n) => bus( x(pmoins,1,1)       : )
      ttm      (1:n) => bus( x(tmoins,1,nk)      : )
      uum      (1:n) => bus( x(umoins,1,nk)      : )
      vvm      (1:n) => bus( x(vmoins,1,nk)      : )
      z0h      (1:n) => bus( x(z0t,1,indx_sfc)   : )
      z0m      (1:n) => bus( x(z0,1,indx_sfc)    : )
      zdlat    (1:n) => bus( x(dlat,1,1)         : )
      zfcor    (1:n) => bus( x(fcor,1,1)         : )
      zqdiag   (1:n) => bus( x(qdiag,1,1)        : )
      zqsurf   (1:n) => bus( x(qsurf,1,indx_sfc) : )
!VV DEBUT MODIFICATION POUR MESH
!      zsnodp   (1:n) => bus( x(snodp,1,indx_sfc) : )
!VV FIN MODIFICATION POUR MESH
      ztdiag   (1:n) => bus( x(tdiag,1,1)        : )
      zthetaa  (1:n) => bus( x(thetaa,1,1)       : )
      ztsa     (1:n) => bus( x(tsa,1,1)          : )     
      zudiag   (1:n) => bus( x(udiag,1,1)        : )
      zvdiag   (1:n) => bus( x(vdiag,1,1)        : )
      zzusl    (1:n) => bus( x(zusl,1,1)         : )
      zztsl    (1:n) => bus( x(ztsl,1,1)         : )
!  
!
      IF (RADSLOPE) THEN
         zFSOLIS(1:n)   => bus( x(fluslop,1,1)      : )
      ELSE
         zFSOLIS(1:n)   => bus( x(flusolis,1,1)     : )
      ENDIF

     
      ! CONVERT RAINRATE AND SNOWRATE FROM M/S TO MM/S TO MATCH UNITS OF
      ! OTHER WATER FLUXES (EVAPORATION etc.)
      
      DO I=1,N
          rainrate_mm(i) = bus(x(rainrate,i,1)) * M_TO_MM
          snowrate_mm(i) = bus(x(snowrate,i,1)) * M_TO_MM
      ENDDO



!     CALCULATE GREENWICH HOUR
      HZ0 = DATE(5) + float(DATE(6))/360000.0 
!
      HZ = AMOD ( HZ0+(FLOAT(KOUNT)*DT)/3600. , 24. )
!
!                     Determine the current julian day
!
      JULIEN = JULIAND( DT, KOUNT, DATE )
!
!     Get cosinus of solar angle at LOCAL HOUR
! 
! 
      CALL SUNCOS1(SUNCOSA,SUNOTHER1,SUNOTHER2,SUNOTHER3,SUNOTHER4, N, & 
          bus(x (DLAT,1,1)), BUS(x(DLON,1,1)),  &
           HZ, JULIEN, DATE, .false.)

      ! Calculate mask for VF26 , |LAT|<=50 have masklat50=1, otherwise masklat50=0

      DO I=1,N

         if( abs (   bus(x(DLAT,I,1)) * 180./acos(-1.)  ) .le.50.) then
            masklat50(i)=1
         else
            masklat50(i)=0
         endif


      ENDDO
!






      call INI_CSTS_SNOWES
      
      if(KOUNT.eq.1) then

         ! at KOUNT eq. 0 have RETURN, so can't use kount=1 
         do I=1,N
!           Make sure rootdp does not exceed permeable depth
            bus(x(ROOTDP,I,1))=min( bus(x(ROOTDP,I,1)) , DL_SVS(KDP) )
         end do

       ! ---------------- FOR SNOW ES --------------------         
         call INI_CSTS_SNOWES
         do I=1,N
!            PTHRUFAL(I)=0.0
            bus(x(PTHRUFAL,I,1))=0.0
            PGRNDFLUX(I)=0.0
            PRNSNOW(I)=0.0
            PHSNOW(I)=0.0
            PGFLUXSNOW(I)=0.0
            PHPSNOW(I)=0.0
            PLESES(I)=0.0
            PLELES(I)=0.0
            PEVAP(I)=0.0
            
!            PTHRUFAL_V(I)=0.0
            bus(x(PTHRUFAL_V,I,1))=0.0
            PGRNDFLUX_V(I)=0.0
            PRNSNOW_V(I)=0.0
            PHSNOW_V(I)=0.0
            PGFLUXSNOW_V(I)=0.0
            PHPSNOW_V(I)=0.0
            PLESES_V(I)=0.0
            PLELES_V(I)=0.0
            PEVAP_V(I)=0.0
         enddo
         PSSA(:,:)=0.0
         PSSAV(:,:)=0.0

      endif

!        ---------------- FOR SNOW ES --------------------
         do I=1,N          

!            PSNOWALB(I)=0.75

            PSR_SI(I) = bus(x(SNOWRATE,I,1))*1000.
            PRR_SI(I) = bus(x(RAINRATE,I,1))*1000.

            ! TO BE CHECKED======================
            PCT(I)= 1.E-4
            PZ0HNAT(I)= Z0HSNOW
            PZ0(I)    = Z0HSNOW/Z0M_TO_Z0H
            do J=1,NL_SVS
               PTG(I,J)  = bus(x(TPSOIL,I,J))
               PTG_V(I,J)  = bus(x(TPSOILV,I,J))
               PD_G(I,J)=DL_SVS(J)
               if(J.eq.1) then
                  PDZG(i,j) = DL_SVS(J)
               else
                  PDZG(i,j) = DL_SVS(J) - DL_SVS(J-1) 
               endif
            enddo
         enddo
!
!******************************************************************
!                  SVS SUBROUTINES START HERE
!******************************************************************
!

      i = sl_prelim(ttm,hum,uum,vvm,psm,zzusl,VMOD,VDIR,TVA,RHOA, &
           min_wind_speed=2.5,min_wind_reduc='linear')


      if (i /= SL_OK) then
         print*, 'Aborting in SVS() because of error returned by sl_prelim()'
         stop
      endif

!
!
! Compute snow diagnostics for some inputs

!
      do I=1,N
!        total snow mass
         bus(x(SNOMA,I,1)) = 0.
         bus(x(SNVMA,I,1)) = 0.
!        total snow depths
         bus(x(SNODPL,I,1))  = 0.
         bus(x(SNVDP ,I,1))  = 0.

         do J=1,NSL
            bus(x(SNOMA ,I,1)) = bus(x(SNOMA ,I,1)) + bus(x(SNOMA_ES ,I,J))
            bus(x(SNVMA ,I,1)) = bus(x(SNVMA ,I,1)) + bus(x(SNOMA_ESV,I,J))
            bus(x(SNODPL,I,1)) = bus(x(SNODPL,I,1)) + bus(x(SNOMA_ES ,I,J))/bus(x(SNODEN_ES ,I,J))
            bus(x(SNVDP ,I,1)) = bus(x(SNVDP ,I,1)) + bus(x(SNOMA_ESV,I,J))/bus(x(SNODEN_ESV,I,J))
         enddo

!        mean snow densities (absolute and relative)
         if ( bus(x(SNODPL,I,1)) .gt.0.0) then
            bus(x(SNODEN,I,1)) = bus(x(SNOMA ,I,1))/bus(x(SNODPL, I,1))
            bus(x(SNORO ,I,1)) = bus(x(SNODEN,I,1))/1000.
         else
            bus(x(SNODEN,I,1)) = 50.0
            bus(x(SNORO ,I,1)) = 0.05
         endif
         if ( bus(x(SNVDP,I,1)) .gt.0.0) then
            bus(x(SNVDEN,I,1)) = bus(x(SNVMA ,I,1))/bus(x(SNVDP,I,1))
            bus(x(SNVRO ,I,1)) = bus(x(SNVDEN,I,1))/1000.
         else
            bus(x(SNVDEN,I,1)) = 50.0
            bus(x(SNVRO ,I,1)) = 0.05
         endif
      enddo


      CALL SOILI_SVS( BUS(x(WSOIL ,1,1)), &
           BUS(x(ISOIL  ,1,1)), &  
           BUS(x(SNOMA  ,1,1)), BUS(x(SNVMA  ,1,1)), &   
           BUS(x(SNORO  ,1,1)), BUS(x(SNVRO  ,1,1)), &  
           BUS(x(VEGH   ,1,1)), &  
           BUS(x(VEGL   ,1,1)), BUS(x(CGSAT  ,1,1)), &  
           BUS(x(WSAT   ,1,1)), BUS(x(WWILT  ,1,1)), &  
           BUS(x(BCOEF  ,1,1)), &  
           BUS(x(CVH    ,1,1)), BUS(x(CVL    ,1,1)), &  
           BUS(x(ALVH   ,1,1)), BUS(x(ALVL   ,1,1)), &   
           BUS(x(EMISVH ,1,1)), BUS(x(EMISVL ,1,1)), &              
           BUS(x(RGLVH  ,1,1)), BUS(x(RGLVL  ,1,1)), &  
           BUS(x(STOMRVH,1,1)), BUS(x(STOMRVL,1,1)), &  
           BUS(x(GAMVH  ,1,1)), BUS(x(GAMVL  ,1,1)), &           
           BUS(x(LAIVH  ,1,1)), BUS(x(LAIVL  ,1,1)),  & 
           BUS(x(Z0MVH  ,1,1)),   &
           BUS(x(Z0MVL  ,1,1)), BUS(x(z0,1,indx_soil)), &  
           BUS(x(CLAY   ,1,1)), BUS(x(SAND   ,1,1)), &  
           BUS(x(DECIDUOUS,1,1)),BUS(x(EVERGREEN,1,1)), &  
           BUS(x(LAIDECI,1,1)),   &
           Z0HSNOW, Z0MBG, Z0M_TO_Z0H, CG, PSOILHCAPZ, &
           PSOILCONDZ, BUS(x(PSNGRVL,1,1)),  &  
           BUS(x(Z0T  ,1,indx_soil)),  & 
           BUS(x(ALGR,1,1)),BUS(x(EMISGR,1,1)), &  
           BUS(x(PSNVH  ,1,1)), BUS(x(PSNVHA ,1,1)), &  
           ALVA, BUS(x(LAIVA  ,1,1)), CVPA, EVA,BUS(x(Z0HA ,1,1)), RGLA, STOMRA,   &
           GAMVA, BUS(X(VGHEIGHT ,1,1)), bus(x(CONDSLD    ,1,1)) , bus(x(CONDDRY   ,1,1)), N )
!
!     

      CALL VEGI_SVS ( zfsolis,   &
           bus(x(tmoins ,1,nk)), bus(x(TVEGE   ,1,1)),   &  
           bus(x(humoins,1,nk)), bus(x(pmoins  ,1,1)),   &  
           BUS(x(WSOIL ,1,1)),  &
           RGLA                ,  &   
           bus(x(LAIVA  ,1,1))     , bus(x(LAIVH   ,1,1)),   &  
           STOMRA,     &
           GAMVA, bus(x(WWILT   ,1,1)),      &
           bus(x(WFC     ,1,1)), SUNCOSA,     &
           bus(x(ROOTDP     ,1,1)),  bus(x(D50   ,1,1)),    &
           bus(x(D95   ,1,1)), bus(x(RST     ,1,1)),     &
           bus(x(SKYVIEW ,1,1)), bus(x(SKYVIEWA ,1,1)), bus(x(VEGTRANS,1,1)), bus(x(VEGTRANSA,1,1)),  &  
           bus(x(frootd   ,1,1)), bus(x(acroot ,1,1)), WRMAX, N)

      IF(KOUNT.EQ.1) then
         DO I=1,N
            STOM_RS(I) = bus(x(RST,I,1))
         ENDDO
         ! long-term ... define default value for rcctem in inisurf
      else
  
         IF( USE_PHOTO ) THEN

            DO I=1,N
               STOM_RS(I) =  bus(x(RCCTEM,I,1))
            END DO
         ELSE
            DO I=1,N
               STOM_RS(I) = bus(x(RST,I,1))
            END DO
         ENDIF
      
      endif

!


      CALL DRAG_SVS ( bus(x(TGROUND,1,1)), bus(x(TVEGE,1,1)),  &   
           bus(x(WSOIL ,1,1)) ,  &   
           bus(x(WVEG   ,1,1)), bus(x(thetaa,1,1)),  &   
           VMOD, VDIR,  bus(x(humoins,1,nk)),     &
           bus(x(pmoins ,1,1)), STOM_RS,   &  
           bus(x(z0    ,1,indx_soil)), bus(x(WFC,1,1)),      &
           bus(x(LAIVA,1,1)), bus(x(zusl,1,1)), bus(x(ztsl,1,1)),    & 
           bus(x (DLAT,1,1)), &
           bus(x(FCOR,1,1)),bus(x(Z0HA ,1,1)),Z0MBG,  Z0M_TO_Z0H,  &  
           bus(x(RESAGR,1,1)), bus(x(RESAVG,1,1)), &    
           bus(x(HUSURF,1,1)),   &  
           HRSURF,      &
           bus(x(HV,1,1)), DEL, RPP, bus(x(QAF,1,1)),    &
           N )  
!
!     Snow over bare/low ground
      CALL SNOWES_SVS(   DT,                                                      &
                         bus(x(SNOMA_ES,1,1)), bus(x(TSNOW,1,1)), bus(x(WSNOW_ES,1,1)),    &
                         bus(x(SNODEN_ES,1,1)), bus(x(SNOAL,1,1)),bus(x(SNOAGE_ES,1,1)),    &
                         bus(x(SNOGRAN1_ES,1,1)), bus(x(SNOGRAN2_ES,1,1)), PSSA,   &
                         PTG,  PCT, PSOILHCAPZ, PSOILCONDZ,                        &
                         bus(x(PMOINS    ,1,1)),bus(x(TMOINS  ,1,NK)),zfsolis,     &
                         BUS(X(HUMOINS,1,NK)), VMOD, bus(x(FDSI,1,1)),         &
                         PRR_SI, PSR_SI,                                       &
                         RHOA, bus(x(zusl,1,1)),  bus(x(ztsl,1,1)),             &
!                        bus(x(Z0,1,indx_soil)),bus(x(Z0,1,indx_soil)),&
                         PZ0,PZ0,&
                         PZ0HNAT, BUS(X(ALGR,1,1)), PD_G, PDZG,                          &
                         bus(x(PTHRUFAL,1,1)), PGRNDFLUX,PRNSNOW, PHSNOW, PGFLUXSNOW, PHPSNOW,       &
                         PLESES, PLELES, PEVAP,bus(x(PSNVHA ,1,1)) , N, NSL, NL_SVS)
!
!
!     Prepare radiation for snow under high veg --> Impact of vegetation on incoming SW and LW 
      DO I=1,N
           PRG_VEG(I)   = zfsolis(I) * bus(x(VEGTRANS,I,1))              ! Incoming SW under VEG

           PRAT_VEG(I)  = bus(x(SKYVIEW,I,1)) * bus(x(FDSI,I,1)) +    &  ! Incoming LW under veg
                 (1. - bus(x(SKYVIEW,I,1))) * EVA(I) * STEFAN * (bus(x(TVEGE,I,2)))**4.  ! add EVA--nathalie

      ENDDO
!
!  
!     Snow under high veg  
      CALL SNOWES_SVS(   DT,                                                      &
                         bus(x(SNOMA_ESV,1,1)), bus(x(TSNOWVEG,1,1)), bus(x(WSNOW_ESV,1,1)),               &
                         bus(x(SNODEN_ESV,1,1)), bus(x(SNVAL,1,1)),bus(x(SNOAGE_ESV,1,1)),    &
                         bus(x(SNOGRAN1_ESV,1,1)), bus(x(SNOGRAN2_ESV,1,1)), PSSAV,   &
                         PTG_V,  PCT, PSOILHCAPZ, PSOILCONDZ,                              &
                         bus(x(PMOINS    ,1,1)),bus(x(TMOINS  ,1,NK)),PRG_VEG,    &
                         BUS(X(HUMOINS,1,NK)), VMOD, PRAT_VEG,                     &
                         PRR_SI, PSR_SI,                                       &
                         RHOA, bus(x(zusl,1,1)),  bus(x(ztsl,1,1)),             &
!                        bus(x(Z0,1,indx_soil)),bus(x(Z0,1,indx_soil)),          &
                         PZ0,PZ0,&
                         PZ0HNAT, BUS(X(ALGR,1,1)), PD_G, PDZG,               &
                         bus(x(PTHRUFAL_V,1,1)), PGRNDFLUX_V,PRNSNOW_V, PHSNOW_V, PGFLUXSNOW_V, PHPSNOW_V,       &
                         PLESES_V, PLELES_V, PEVAP_V,BUS(X(PSNGRVL,1,1)) , N, NSL, NL_SVS)


!
! Compute snow diagnostics for hydro and outputs
!
      DO I=1,N
!        total snow mass
         bus(x(SNOMA,I,1))  = 0.
         bus(x(SNVMA,I,1)) = 0
!        total snow depths
         bus(x(SNODPL,I,1))  = 0.
         bus(x(SNVDP ,I,1))   = 0.

         DO J=1,NSL
            bus(x(SNOMA,I,1))   =  bus(x(SNOMA,I,1)) + bus(x(SNOMA_ES ,I,J))
            bus(x(SNVMA,I,1))   =  bus(x(SNVMA,I,1)) + bus(x(SNOMA_ESV,I,J))
            bus(x(SNODPL,I,1))  =  bus(x(SNODPL,I,1)) + bus(x(SNOMA_ES ,I,J))/bus(x(SNODEN_ES ,I,J))
            bus(x(SNVDP ,I,1))  =  bus(x(SNVDP ,I,1)) + bus(x(SNOMA_ESV,I,J))/bus(x(SNODEN_ESV,I,J))
            bus(x(SSA_ES,I,J))  =  PSSA(I,J)
            bus(x(SSA_ESV,I,J)) =  PSSAV(I,J)
         ENDDO
      ENDDO
!
!
      call EBUDGET_SVS(bus(x(TSA ,1,1)),  & 
                  bus(x(TSOIL     ,1,1)) , &
                  bus(x(WSOIL     ,1,1)) , bus(x(ISOIL,1,1)),      &   
                  bus(x(TGROUND   ,1,1)) , bus(x(TGROUND,1,2)),    & 
                  bus(x(TVEGE     ,1,1)) , bus(x(TVEGE,1,2)),      &   
                  bus(x(TPSOIL    ,1,1)) , bus(x(TPSOILV,1,1)),    & 
                  bus(x(TPERM     ,1,1)) , PGRNDFLUX, PGRNDFLUX_V, & 
                  DT, VMOD, VDIR         , bus(x(DLAT,1,1)),   &   
                  zfsolis ,ALVA ,bus(x(laiva,1,1)),GAMVA ,     & 
                  bus(x(ALGR      ,1,1)) , bus(x(EMISGR,1,1)),    & 
                  bus(x(FDSI      ,1,1)) , bus(x(thetaa,1,1)),    &   
                  bus(x(FCOR      ,1,1)) , bus(x(zusl,1,1)),    &  
                  bus(x(ztsl      ,1,1)) , bus(x(humoins    ,1,nk)), &
                  bus(x(pmoins,1,1)), RHOA,bus(x(Z0,1,indx_soil)) , bus(x(Z0T,1,indx_soil)),&
                  HRSURF,       & 
                  bus(x(HV         ,1,1)) , DEL, STOM_RS ,& 
                  CG,CVPA,EVA,bus(x(PSNGRVL    ,1,1)) ,    &    
                  bus(x(RESAGR,1,1)), bus(x(RESAVG,1,1)),   &        
                  PRNSNOW, PHSNOW,    &   
                  PLELES, PEVAP,      &
                  bus(x(SNOAL   ,1,1)) ,    &
                  bus(x(TSNOW      ,1,1)) ,    &  
                  PRNSNOW_V, PHSNOW_V, &
                  PLELES_V, PEVAP_V,   &
                  bus(x(SNVAL  ,1,1)) ,    &
                  bus(x(TSNOWVEG   ,1,1)) ,   &   
                  bus(x(VEGH       ,1,1)) , bus(x(VEGL   ,1,1)) , bus(x(VGHEIGHT   ,1,1)),  &   
                  bus(x(PSNVH      ,1,1)) , bus(X(PSNVHA     ,1,1)),   &
                  bus(x(SKYVIEW    ,1,1)) , bus(x(SKYVIEWA   ,1,1)),   & 
                  bus(x(CONDSLD    ,1,1)) , bus(x(CONDDRY   ,1,1)), PSOILHCAPZ, PSOILCONDZ, &   
                  rainrate_mm,bus(x(WVEG   ,1,1)),bus(x(snvma,1,1)),&
                  bus(x(VEGTRANSA  ,1,1)) , bus(x(ALVIS,1,indx_soil)),     & 
                  bus(x(RNET_S     ,1,1)),    &   
                  bus(x(FC  ,1,indx_soil)), bus(x(FV  ,1,indx_soil)),   &    
                  bus(x(LEG        ,1,1)) , bus(x(LEV  ,1,1)),    &   
                  bus(x(LES        ,1,1)) , bus(x(LESV   ,1,1)),    &  
                  bus(x(LEGV       ,1,1)) , bus(x(LER        ,1,1)) ,   &  
                  bus(x(LETR       ,1,1)) , bus(x(EG         ,1,1)) ,   &    
                  bus(x(ER         ,1,1)) , bus(x(ETR    ,1,1)),    &  
                  bus(x(FL         ,1,1)),  bus(x(EFLUX      ,1,1)) ,    &  
                  bus(x(BM         ,1,1)) , bus(x(FQ   ,1,1)),    &  
                  bus(x(bt, 1,indx_soil)) ,    &  
                  LEFF                    , DWATERDT,     & 
                  bus(x(FTEMP,1,indx_soil)), BUS(x(FVAP,1,indx_soil)),   &   
                  bus(x(qsurf,1,indx_soil)), bus(x(frv ,1,indx_soil)),   &   
                  bus(x(ALFAT      ,1,1)) , bus(x(ALFAQ      ,1,1)) ,    &  
                  bus(x(ilmo  ,1,indx_soil)), bus(x(hst  ,1,indx_soil)), &   
                  TRAD, N , &
                  bus(x(QVEG ,1,1)), bus(x(QGV   ,1,1)), bus(x(QGR   ,1,1)), & 
                  bus(x(FCGV ,1,1)), bus(x(TAF   ,1,1)), bus(x(QAF   ,1,1)), bus(x(VAF   ,1,1)), & 
                  bus(x(RGVG ,1,1)), bus(x(RGGV ,1,1)), bus(x(FIGV ,1,1)), &
                  bus(x(FIVG ,1,1)), bus(x(IRGV ,1,1)), bus(x(IRFVG ,1,1)), &
                  bus(x(HGV ,1,1)), bus(x(LGV ,1,1)), &
                  bus(x(HGR ,1,1)), bus(x(LGR ,1,1)), &
                  bus(x(HVEG ,1,1)), bus(x(LVEG ,1,1)), &
                  bus(x(FGRV ,1,1)),bus(x(TRNETVG ,1,1)),bus(x(TFNETVG ,1,1)),bus(x(TRNGV ,1,1)), &
                  bus(x(TNEIGE ,1,1)),bus(x(TIRGV ,1,1)),bus(x(TIRVG ,1,1)),bus(x(TFIGV ,1,1)),bus(x(TRGVG ,1,1)),RPP, &
                  bus(x(grflux ,1,1)),bus(x(Z0HA ,1,1)),bus(x(RESAGRV ,1,1)) )
!
!

      CALL HYDRO_SVS ( DT,      & 
           bus(x(eg      ,1,1)), bus(x(er      ,1,1)),&
           bus(x(etr     ,1,1)), rainrate_mm         ,&
           bus(x(PTHRUFAL,1,1)), bus(x(PTHRUFAL_V,1,1)),&
           bus(x(impervu ,1,1)), bus(x(vegl    ,1,1)),&
           bus(x(vegh    ,1,1)), bus(x(psngrvl ,1,1)),&
           bus(x(psnvha  ,1,1)), bus(x(acroot  ,1,1)),&
           wrmax,                bus(x(wsat    ,1,1)),&
           bus(x(ksat    ,1,1)), bus(x(psisat  ,1,1)),&
           bus(x(bcoef   ,1,1)), bus(x(fbcof   ,1,1)),&
           bus(x(wfcint  ,1,1)), bus(x(grkef   ,1,1)),&
           bus(x(snoma   ,1,1)), bus(x(snvma   ,1,1)),&
           bus(x(wveg    ,1,1)), wvegt               ,&
           bus(x(wsoil   ,1,1)), wsoilt              ,&
           bus(x(isoil   ,1,1))                      ,&
           bus(x(ksatc   ,1,1)), bus(x(khc     ,1,1)),&
           bus(x(psi     ,1,1)), bus(x(grksat  ,1,1)),&
           bus(x(wfcdp   ,1,1)), bus(x(watflow ,1,1)),&
           bus(x(latflw  ,1,1)), &
           bus(x(runofftot ,1,indx_soil)), N)



      IF( USE_PHOTO ) THEN


         CALL PHTSYN_SVS ( BUS(x(LAIVF26,1,1))  , BUS(x(VEGF   ,1,1)), &
                        BUS(x(TVEGE  ,1,1))  , BUS(x(PMOINS ,1,1)), &
                        BUS(x(RESAVG ,1,1))  , BUS(x(HUMOINS,1,1)), &
                        BUS(x(RNET_S ,1,1))  , BUS(x(WSOIL ,1,1)), &
                        BUS(x(FROOTD ,1,1))  , SUNCOSA            , &
                        BUS(x(WFC    ,1,1))  , BUS(x(WWILT  ,1,1)), &
                        MASKLAT50            , BUS(x(VGCTEM ,1,1))  , &
                        BUS(x(LAICTEM,1,1))  ,                      &
                        BUS(x(RCCTEM ,1,1))  , BUS(x(CO2I1  ,1,1)), &
                        BUS(x(AVG_GWSOL,1,1)), &
                        NCLASS, N)
       
      endif
!
!     phase change under veg
      CALL PHASE_CHANGES ( TRNCH, DT, bus(x(LAIVA  ,1,1)), PSOILHCAPZ , &
                      bus(x(WSAT    ,1,1)), bus(x(PSISAT  ,1,1)), bus(x(BCOEF  ,1,1)), &
                      bus(x(TPSOILV ,1,1)), bus(x(ISOIL  ,1,1)), &
                      wsoilt, WFTV, WDTTV, DELWATVG, DELICEVG      , &
                      bus(x(FROOTD  ,1,1)), N                   , &
                      bus(x(PHASEFV ,1,1)), bus(x(PHASEMV ,1,1)), &
                      bus(x(DELTATV ,1,1)), bus(x(APPHEATCAPV ,1,1)), bus(x(TMAXV ,1,1)) )
!     phase change bare ground
      CALL PHASE_CHANGES ( TRNCH, DT, bus(x(LAIVA  ,1,1)), PSOILHCAPZ , &
                      bus(x(WSAT   ,1,1)), bus(x(PSISAT  ,1,1)), bus(x(BCOEF  ,1,1)), &
                      bus(x(TPSOIL ,1,1)), bus(x(ISOIL  ,1,1)), &
                      wsoilt, WFTG, WDTTG, DELWATGR, DELICEGR     , &
                      bus(x(FROOTD ,1,1)), N                   , &
                      bus(x(PHASEF ,1,1)), bus(x(PHASEM ,1,1)) , &
                      bus(x(DELTAT ,1,1)), bus(x(APPHEATCAP ,1,1)), bus(x(TMAX ,1,1)) )
!
!     aggregate the phase changes into one
      CALL AGG_PHASES ( TRNCH, bus(x(VEGH  ,1,1)), bus(x(VEGL  ,1,1)), &
                       bus(x(PSNVH  ,1,1)), bus(x(PSNGRVL,  1,1))    , &
                       bus(x(ISOIL  ,1,1)),  wsoiltt, ISOILT, &
                       WDTTG, WDTTV, WFTG, WFTV       , &
                       bus(x(TPSOIL ,1,1)), bus(x(TPSOILV ,1,1)), PSOILHCAPZ, &
                       N )
!
!

      CALL UPDATE_SVS ( WSOILTT, ISOILT, WVEGT, &
                       bus(x(latflw  ,1,1)), bus(x(watflow ,1,1)),  &
                       bus(x(WSOIL   ,1,1)), bus(x(ISOIL   ,1,1)),  &
                       bus(x(WVEG    ,1,1)), bus(x(WSOILM  , 1,1)), &
                       bus(x(latflaf ,1,1)), bus(x(drainaf ,1,1)),  &
                       N )
!
  
 !# Compute values at the diagnostic level
     
      i = sl_sfclayer(zthetaa,hum,vmod,vdir,zzusl,zztsl,ztsa,zqsurf, &
           z0m,z0h,zdlat,zfcor,hghtm_diag=zu,hghtt_diag=zt,             &
           t_diag=ztdiag,q_diag=zqdiag,u_diag=zudiag,v_diag=zvdiag,&
           tdiaglim=TDIAGLIM_FALSE) 


      if (i /= SL_OK) then
         print*, 'Aborting in svs() because of error returned by sl_sfclayer()'
         stop
      endif


     

!VDIR NODEP

      do i=1,n
!
!
        bus(x(tsurf  ,i,1        )) = bus(x(tsa  ,i,1        ))
        bus(x(tsrad  ,i,1        )) = TRAD(i)
!
!       CALCULATE LAND-ATMOSPHERE OUTCOMING WATER FLUX
        BUS(x(WFLUX,I,1)) = RHOA(I)*BUS(x(EFLUX,I,1))
        BUS(x(ACCEVAP,I,1)) = BUS(x(ACCEVAP,I,1)) + BUS(x(WFLUX,I,1)) * DT
!
!       CALCULATE MEAN SNOW DEPTH FOR ESTHETIC PURPOSE ONLY
!VV DEBUT MODIFICATION POUR MESH
!       zsnodp(i) = bus(x(VEGH,i,1)) * bus(x(SNVDP,i,1)) + (1. -  bus(x(VEGH,i,1))) * bus(x(SNODPL,i,1))
!VV FIN MODIFICATION POUR MESH

      end do
!
!VV DEBUT MODIFICATION POUR MESH
#ifndef RUNSVS
!     FILL THE ARRAYS TO BE AGGREGATED LATER IN S/R AGREGE
      CALL FILLAGG ( BUS, BUSSIZ, PTSURF, PTSURFSIZ, INDX_SOIL,  &  
                    SURFLEN )
!VV FIN MODIFICATION POUR MESH
#endif
!



      RETURN
      END
