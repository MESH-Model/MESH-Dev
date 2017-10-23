!     #########
SUBROUTINE SNOWES_SVS(   PTSTEP,                                                         &
                         PSNOWSWE, PSNOWTEMP, PSNOWLIQ, PSNOWRHO, PSNOWALB,PSNOWAGE,     &
                         PSNOWGRAN1, PSNOWGRAN2, PSSA,                                   &
                         PTG,  PCT, PSOILHCAPZ, PSOILCONDZ,                              &
                         PPS, PTA, PSW_RAD, PQA, PVMOD, PLAT, PLON,                      &
                         PLW_RAD, PRR, PSR,                                              &
                         PRHOA, PUREF,                                                   &
                         PZREF, PZ0NAT, PZ0EFF, PZ0HNAT, PALB, PD_G, PDZG,               &
                         PTHRUFAL, PGRNDFLUX,PRNSNOW, PHSNOW, PGFLUXSNOW, PHPSNOW,       &
                         PLESES, PLELES, PEVAP,PPSN , N , NSL, NL)
!     ######################################################################################
!
!!****  *SNOWES_SVS*  
!!
!!    PURPOSE
!!    -------
!
!     3-Layer snow scheme option (Boone and Etchevers 1999)
!     This routine is NOT called as snow depth goes below
!     a critical threshold which is vanishingly small.
!     This routine acts as an interface between SNOWES and the Canadian model SVS (Soil Vegetation Snow)
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Boone and Etchevers (1999)
!!    Belair (1995)
!!    Noilhan and Planton (1989)
!!    Noilhan and Mahfouf (1996)
!!      
!!    AUTHOR
!!    ------
!!	A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        7/99  Boone
!!      Packing added   4/00  Masson & Boone
!!      z0h and snow    2/06  LeMoigne
!!
!!      Modified by B. Decharme  (03/2009): Consistency with Arpege permanent
!!                                          snow/ice treatment
!!      Modified by A. Boone     (04/2010): Implicit coupling with atmosphere permitted.
!!
!!      Modified by B. Decharme  (04/2010): check suspicious low temperature for ES and CROCUS
!!      Modified by B. Decharme  (08/2013): Qsat as argument (needed for coupling with atm)
!!      Modified by A. Boone     (10/2014): MEB: pass in fluxes when using MEB
!!
!-------------------------------------------------------------------------------
!
USE MODD_CSTS_SNOWES,       ONLY : XTT, XPI, XDAY, XLMTT, XLSTT, XCI,XP00, XRD, XCPD,  &
                                   XRHOLW
USE MODD_SNOWES_PAR,   ONLY : XRHOSMAX_ES, XSNOWDMIN, XRHOSMIN_ES, XEMISSN, XUNDEF

USE SNOWES_MOD , ONLY: SNOWES
!
!
!USE MODI_SNOWES
!USE MODI_SNOWCRO
!
!USE MODI_ABOR1_SFX
!
!
IMPLICIT NONE
!

!
!
INTEGER N, NSL, NL
!                                      N         = Size of "TRNCH" i.e.,of row passed to SVS
!                                      NSL       = Number of snow layers
!                                      NL        = Number of soil layers
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!                                      PTSTEP    = time step of the integration
!
REAL, DIMENSION(N,NL), INTENT(IN)    :: PSOILHCAPZ, PD_G, PDZG
REAL, DIMENSION(N),   INTENT(IN)    :: PCT, PSOILCONDZ  
!                                      PD_G      = Depth to bottom of each soil layer (m)
!                                      PDZG      = Soil layer thicknesses (m)
!                                      PCT       = area-averaged surface heat capacity [(K m2)/J]
!                                      PSOILCONDZ= soil thermal conductivity (W m-1 K-1)
!                                      PSOILHCAPZ= soil heat capacity (J m-3 K-1)
!
REAL, DIMENSION(N), INTENT(IN)      :: PPS, PTA, PSW_RAD, PQA,                       &
                                       PVMOD, PLW_RAD, PSR, PRR  
!                                      PSW_RAD = incoming solar radiation (W/m2)
!                                      PLW_RAD = atmospheric infrared radiation (W/m2)
!                                      PRR     = rain rate [kg/(m2 s)]
!                                      PSR     = snow rate (SWE) [kg/(m2 s)]
!                                      PTA     = atmospheric temperature at level za (K)
!                                      PVMOD   = modulus of the wind parallel to the orography (m/s)
!                                      PPS     = surface pressure
!                                      PQA     = atmospheric specific humidity
!                                                at level za
!
REAL, DIMENSION(N), INTENT(IN)      :: PZREF, PUREF,  PRHOA, PZ0NAT, PZ0EFF, PZ0HNAT, PALB
!                                      PZ0EFF    = roughness length for momentum 
!                                      PZ0NAT    = grid box average roughness length
!                                      PZ0HNAT   = grid box average roughness length
!                                      PZREF     = reference height of the first
!                                                  atmospheric level
!                                      PUREF     = reference height of the wind
!                                      PRHOA     = air density
!                                      PALB      = soil/vegetation albedo
!
REAL, DIMENSION(N), INTENT(IN)      :: PPSN, PLAT, PLON
!                                      PPSN     = Snow cover fraction (total) 
!
REAL, DIMENSION(N,NL), INTENT(INOUT) :: PTG
!                                      PTG       = Soil temperature profile (K)
!
REAL, DIMENSION(N), INTENT(INOUT)   :: PSNOWALB
!                                      PSNOWALB = Prognostic surface snow albedo
!                                                 (does not include anything but
!                                                 the actual snow cover)
!
REAL, DIMENSION(N,NSL), INTENT(INOUT) :: PSNOWTEMP, PSNOWRHO, PSNOWSWE,PSNOWLIQ
!                                      PSNOWTEMP = Snow layer(s) temperature (K)
!                                      PSNOWRHO  = Snow layer(s) averaged density (kg/m3)
!                                      PSNOWSWE  = Snow layer(s) liquid Water Equivalent (SWE:kg m-2)
!                                      PSNOWLIQ  = Snow layer(s) liquid water content (m)
!
REAL, DIMENSION(N,NSL), INTENT(INOUT) :: PSNOWAGE    ! Snow grain age
REAL, DIMENSION(N,NSL), INTENT(INOUT) :: PSNOWGRAN1  ! Snow grain characteristic
                                                     ! If metamo = C13, then PSNOWGRAN1 is
                                                     ! the optical diameter
REAL, DIMENSION(N,NSL), INTENT(INOUT) :: PSNOWGRAN2  ! Snow grain characteristic
                                                     ! If metamo = C13, then PSNOWGRAN2 is
                                                     ! the sphericity
!
REAL, DIMENSION(N,NSL), INTENT(OUT)   :: PSSA
!                                        PSSA = Specific surface area (SSA) (m2/kg)
!
REAL, DIMENSION(N), INTENT(INOUT)   :: PRNSNOW, PHSNOW, PLESES, PLELES,     &
                                       PHPSNOW, PEVAP, PGRNDFLUX 
!                                      PLELES        = evaporation heat flux from snow (W/m2)
!                                      PLESES        = sublimation (W/m2)
!                                      PHPSNOW       = heat release from rainfall (W/m2)
!                                      PRNSNOW       = net radiative flux from snow (W/m2)
!                                      PHSNOW        = sensible heat flux from snow (W/m2)
!                                      PEVAP         = total evaporative flux from snow (kg/m2/s)
!                                      PGRNDFLUX     = soil/snow interface heat flux (W/m2)
!
!
REAL, DIMENSION(N), INTENT(OUT)     :: PGFLUXSNOW
!                                      PGFLUXSNOW    = net heat flux from snow (W/m2)
!
!
REAL, DIMENSION(N), INTENT(OUT)     :: PTHRUFAL 
!                                      PTHRUFAL  = rate that liquid water leaves snow pack: 
!                                                  paritioned into soil infiltration/runoff 
!                                                  by ISBA [kg/(m2 s)]
!
CHARACTER(3)                        :: HSNOWMETAMO
                                         !-----------------------
                                         ! Metamorphism scheme
                                         ! HSNOWMETAMO=B92 Brun et al 1992
                                         ! HSNOWMETAMO=C13 Carmagnola et al 2014
                                         ! HSNOWMETAMO=T07 Taillandier et al 2007
                                         !-----------------------
!
!

!*      0.2    declarations of local variables
!
REAL, PARAMETER                     :: ZCHECK_TEMP = 100.0 
!                                      Limit to check suspicious low temperature (K)
!
INTEGER                             :: JWRK, JJ ! Loop control
!
INTEGER                             :: INLVLS   ! maximum number of snow layers
INTEGER                             :: INLVLG   ! number of ground layers
!
REAL, DIMENSION(SIZE(PTA))          :: ZRRSNOW, ZSOILCOND, ZSNOW, ZSNOWFALL,  &
                                       ZSNOWABLAT_DELTA, ZSNOWSWE_1D, ZSNOWD, & 
                                       ZSNOWH, ZSNOWH1, ZGRNDFLUXN, ZPSN,     &
                                       ZSOILCOR, ZSNOWSWE_OUT, ZTHRUFAL,      &
                                       ZSNOW_MASS_BUDGET
!                                      ZSOILCOND    = soil thermal conductivity [W/(m K)]
!                                      ZRRSNOW      = rain rate over snow [kg/(m2 s)]
!                                      ZSNOW        = snow depth (m) 
!                                      ZSNOWFALL    = minimum equivalent snow depth
!                                                     for snow falling during the
!                                                     current time step (m)
!                                      ZSNOWABLAT_DELTA = FLAG =1 if snow ablates completely
!                                                     during current time step, else=0
!                                      ZSNOWSWE_1D  = TOTAL snowpack SWE (kg m-2)
!                                      ZSNOWD       = snow depth
!                                      ZSNOWH       = snow total heat content (J m-2)
!                                      ZSNOWH1      = snow surface layer heat content (J m-2)
!                                      ZGRNDFLUXN   = corrected snow-ground flux (if snow fully ablated during timestep)
!                                      ZPSN         = snow fraction working array
!                                      ZSOILCOR = for vanishingy thin snow cover,
!                                                 allow any excess evaporation
!                                                 to be extracted from the soil
!                                                 to maintain an accurate water
!                                                 balance [kg/(m2 s)]
!                                      ZSNOW_MASS_BUDGET = snow water equivalent budget (kg/m2/s)
!
!###########################################################################################
!########       Temporary variables during phasing with SVS
!###########################################################################################

REAL, DIMENSION(SIZE(PTA))          :: ZEXNS,ZEXNA,ZDIRCOSZW 
!                                      PEXNS     = Exner function at surface
!                                      PEXNA     = Exner function at lowest atmos level
!                                      PDIRCOSZW = Cosinus of the angle between the 
!                                                  normal to the surface and the vertical
!
REAL, DIMENSION(SIZE(PTA))          :: ZSWNETSNOW, ZSWNETSNOWS ,ZLWNETSNOW
!                                      ZSWNETSNOW    = net shortwave radiation entering top of snowpack (W/m2)
!                                      ZSWNETSNOWS   = net shortwave radiation in uppermost layer of snowpack (W/m2)
!                                                      (for surface energy budget closure diagnostics)
!                                      ZLWNETSNOW    = net longwave radiation entering top of snowpack (W/m2)

REAL, DIMENSION(SIZE(PTA))     :: ZPEW_A_COEF, ZPEW_B_COEF,                   &
                                       ZPET_A_COEF, ZPEQ_A_COEF, ZPET_B_COEF,      &
                                       ZPEQ_B_COEF  
!                                      PPEW_A_COEF = wind coefficient
!                                      PPEW_B_COEF = wind coefficient
!                                      PPET_A_COEF = A-air temperature coefficient
!                                      PPET_B_COEF = B-air temperature coefficient
!                                      PPEQ_A_COEF = A-air specific humidity coefficient
!                                      PPEQ_B_COEF = B-air specific humidity coefficient       

REAL, DIMENSION(SIZE(PTA),SIZE(PSNOWSWE,2))     :: ZSNOWHEAT, ZSNOWDZ, ZSCAP
!                                      ZSNOWHEAT = Snow layer(s) heat content (J/m2)
!                                      ZSNOWDZ   = Snow layer(s) thickness (m)
!                                      ZSCAP      = Snow layer(s) heat capacity [J/(K m3)]
!
REAL, DIMENSION(SIZE(PTA))  :: ZFLSN_COR, ZEVAPCOR, ZSNOWHMASS, ZGSFCSNOW,               &
                                       ZDELHEATG, ZDELHEATG_SFC
!                                      PFLSN_COR = soil/snow correction heat flux (W/m2) (not MEB)

!                                      PEVAPCOR  = evaporation/sublimation correction term:
!                                                  extract any evaporation exceeding the
!                                                  actual snow cover (as snow vanishes)
!                                                  and apply it as a surface soil water
!                                                  sink. [kg/(m2 s)]
!                                      PSNOWHMASS = heat content change due to mass
!                                                   changes in snowpack (J/m2): for budget
!                                                   calculations only.
!                                      PGSFCSNOW  = heat flux between the surface and sub-surface 
!                                                   snow layers (for energy budget diagnostics) (W/m2)
!                                      PDELHEATG     = ground heat content change (diagnostic) (W/m2)
!                                                      note, modified if ground-snow flux adjusted
!                                      PDELHEATG_SFC = ground heat content change in sfc only (diagnostic) (W/m2)
!                                                      note, modified if ground-snow flux adjusted

REAL, DIMENSION(SIZE(PTA))        :: ZQS
!                                      ZQS = surface humidity (kg/kg)

!
REAL, DIMENSION(SIZE(PTA))       :: ZSNDRIFT
!                                      PSNDRIFT    = blowing snow sublimation (kg/m2/s)

REAL, DIMENSION(SIZE(PTA))  :: ZUSTARSNOW, ZCDSNOW, ZCHSNOW, ZRI,ZEMISNOW
!                                      PCDSNOW    = drag coefficient for momentum over snow (-)
!                                      PUSTARSNOW = friction velocity over snow (m/s)
!                                      PCHSNOW    = drag coefficient for heat over snow (-)
!                                      PRI        = Richardson number (-)
!                                      PEMISNOW      = snow surface emissivity

REAL, DIMENSION(SIZE(PTA))   :: ZSRSFC, ZRRSFC, ZSNOWSFCH, ZDELHEATN, ZDELHEATN_SFC
!                                      PSRSFC = snow rate on soil/veg surface when SNOWES in use
!                                      PRRSFC = rain rate on soil/veg surface when SNOWES in use

LOGICAL     :: LMEB
CHARACTER(LEN=3)    :: CSNOWRES,CIMPLICIT_WIND

LOGICAL     :: LSNOWDRIFT, LSNOWDRIFT_SUBLIM ! activate snowdrift, sublimation during drift


! ajout_EB pour prendre en compte angle zenithal du soleil dans LRAD
! puis plus tard dans LALB
REAL, DIMENSION(SIZE(PTA))      :: ZZENITH    ! solar zenith angle

!###########################################################################################
!########  END      Temporary variables during phasing with SVS
!###########################################################################################

!*      0.3    declarations of packed  variables
!
INTEGER                            :: ISIZE_SNOW ! number of points where computations are done
INTEGER, DIMENSION(SIZE(PTA))      :: NMASK      ! indices correspondance between arrays
!
LOGICAL, DIMENSION(SIZE(PTA))      :: LREMOVE_SNOW
!
!
! - - ---------------------------------------------------
!
!
!*       0.     Initialize variables:
!               ---------------------
!
PTHRUFAL(:)    = 0.0
ZSRSFC(:)      = PSR(:)         ! these are snow and rain rates passed to ISBA,
ZRRSFC(:)      = PRR(:)         ! so initialize here if SNOWES not used:
!
ZSNOW(:)       = 0.0
ZSNOWD(:)      = 0.0
ZGRNDFLUXN(:)  = 0.0
ZSNOWH(:)      = 0.0
ZSNOWH1(:)     = 0.0
ZSNOWSWE_1D(:) = 0.0
ZSNOWSWE_OUT(:)= 0.0
ZSOILCOND(:)   = 0.0
ZRRSNOW(:)     = 0.0
ZSNOWFALL(:)   = 0.0
ZSNOWABLAT_DELTA(:) = 0.0


!
INLVLS          = SIZE(PSNOWSWE(:,:),2)            ! on pourrait lire NSL             
INLVLG          = MIN(SIZE(PD_G(:,:),2),SIZE(PTG(:,:),2))  ! on pourrait lire NL         
!
!
!###########################################################################################
!########       Temporary variables during phasing with SVS
!###########################################################################################
CSNOWRES='RIL'
LSNOWDRIFT = .FALSE.
LSNOWDRIFT_SUBLIM = .FALSE.
LMEB=.FALSE.
CIMPLICIT_WIND = 'NEW'
ZPEW_A_COEF(:) = 0.
ZPEW_B_COEF(:) = PVMOD(:)
ZPET_A_COEF(:) = 0.
! We shoudl use PPA in this formulation. Currently we use PPS
ZPET_B_COEF(:) =  PTA / (PPS/XP00)**(XRD/XCPD) 
ZPEQ_A_COEF(:) = 0.
ZPEQ_B_COEF(:) = PQA(:)        

ZSNOWHEAT(:,:)  = 0.
ZSNOWDZ(:,:)  = 0.

HSNOWMETAMO = 'C13'

! Compute snow layer thickness
ZSNOWDZ(:,:) = PSNOWSWE(:,:)/PSNOWRHO(:,:)

! WARNING  : calcul a reprendre en fonction des valeurs par defauts dans SVS
ZSCAP(:,:)     = PSNOWRHO(:,:)*XCI

WHERE(PSNOWSWE(:,:)>0.) 
      ZSNOWHEAT(:,:) = ZSNOWDZ(:,:)*( ZSCAP(:,:)*(PSNOWTEMP(:,:)-XTT)        &
                   - XLMTT*PSNOWRHO(:,:) ) + XLMTT*XRHOLW*PSNOWLIQ(:,:)  
END WHERE


ZQS(:)         = XUNDEF
ZSNDRIFT(:)    = 0.

! Compute Exner funtion. So far we use the same atmopheric pressure. See if both pressure are available in SVS (as done in coupling_isban.f90)
ZEXNS(:)   = (PPS(:)/XP00)**(XRD/XCPD)
ZEXNA(:)   = (PPS(:)/XP00)**(XRD/XCPD)

! We assume so far a flat terrain.
ZDIRCOSZW(:) = 1.

! Correction flux
ZFLSN_COR(:)   = 0.0
ZEVAPCOR(:)    = 0.0

ZSNOWHMASS(:)  = 0.0

! Zenith angle is only useful for albedo computation in polar environment (high latitute)

ZZENITH(:) = XPI/2.

!
!###########################################################################################
!########   END    Temporary variables during phasing with SVS
!###########################################################################################

   PGRNDFLUX(:)   = 0.0
   PLESES(:)      = 0.0
   PLELES(:)      = 0.0
   PEVAP(:)       = 0.0
   PRNSNOW(:)     = 0.0
   PHSNOW(:)      = 0.0
   PGFLUXSNOW(:)  = 0.0
   PSSA(:,:)      = 0.0
   PHPSNOW(:)     = 0.0
   ZSWNETSNOW(:)  = 0.0
   ZSWNETSNOWS(:) = 0.0
   ZLWNETSNOW(:)  = 0.0
   ZEMISNOW(:)    = XEMISSN
   ZUSTARSNOW(:)  = 0.0
   ZCDSNOW(:)     = 0.0
   ZCHSNOW(:)     = 0.0
   ZRI(:)         = XUNDEF
!
!
! Use ISBA-SNOWES or NOT: NOTE that if explicit soil diffusion method in use,
! then *must* use explicit snow model:
!
! - Snow and rain falling onto the 3-L grid space:
!
   ZSRSFC(:)=0.0
!
   DO JJ=1,SIZE(PSR)
      ZRRSNOW(JJ)        = PPSN(JJ)*PRR(JJ)
      ZRRSFC(JJ)         = PRR(JJ) - ZRRSNOW(JJ)
      ZSNOWFALL(JJ)      = PSR(JJ)*PTSTEP/XRHOSMAX_ES    ! maximum possible snowfall depth (m)
   ENDDO
!
! Calculate preliminary snow depth (m)

   ZSNOW(:)=0.
   ZSNOWH(:)=0.
   ZSNOWSWE_1D(:)=0.
   ZSNOWH1(:)              = ZSNOWHEAT(:,1)*PSNOWSWE(:,1)/PSNOWRHO(:,1) ! sfc layer only
!
   DO JWRK=1,SIZE(PSNOWSWE,2)
      DO JJ=1,SIZE(PSNOWSWE,1)
         ZSNOWSWE_1D(JJ)     = ZSNOWSWE_1D(JJ) + PSNOWSWE(JJ,JWRK)
         ZSNOW(JJ)           = ZSNOW(JJ)       + PSNOWSWE(JJ,JWRK)/PSNOWRHO(JJ,JWRK)
         ZSNOWH(JJ)          = ZSNOWH(JJ)      + ZSNOWHEAT(JJ,JWRK)*PSNOWSWE(JJ,JWRK)/PSNOWRHO(JJ,JWRK)
      END DO
   ENDDO
!
   ZSOILCOND(:)   = PSOILCONDZ(:)
!
! ===============================================================
! === Packing: Only call snow model when there is snow on the surface
!              exceeding a minimum threshold OR if the equivalent
!              snow depth falling during the current time step exceeds 
!              this limit.
!
! counts the number of points where the computations will be made
!
!
   ISIZE_SNOW = 0
   NMASK(:) = 0
!
   DO JJ=1,SIZE(ZSNOW)
      IF (ZSNOW(JJ) >= XSNOWDMIN .OR. ZSNOWFALL(JJ) >= XSNOWDMIN) THEN
         ISIZE_SNOW = ISIZE_SNOW + 1
         NMASK(ISIZE_SNOW) = JJ
      ENDIF
   ENDDO
!  
   IF (ISIZE_SNOW>0) CALL CALL_MODEL(ISIZE_SNOW,INLVLS,INLVLG,NMASK)
!
! ===============================================================
!
! Remove trace amounts of snow and reinitialize snow prognostic variables
! if snow cover is ablated.
! If MEB used, soil T already computed, therefore correct heating/cooling
! effect of updated snow-soil flux
!
   ZSNOWD(:) = 0.
   ZSNOWSWE_OUT(:) = 0.
   DO JWRK=1,SIZE(PSNOWSWE,2)
      DO JJ=1,SIZE(PSNOWSWE,1)
         ZSNOWD      (JJ) = ZSNOWD      (JJ) + PSNOWSWE(JJ,JWRK)/PSNOWRHO(JJ,JWRK)
         ZSNOWSWE_OUT(JJ) = ZSNOWSWE_OUT(JJ) + PSNOWSWE(JJ,JWRK)
      ENDDO
   END DO
!
   LREMOVE_SNOW(:)=(ZSNOWD(:)<XSNOWDMIN*1.1)
!
!
!    To Conserve mass in ISBA without MEB, 
!    EVAP must be weignted by the snow fraction
!    in the calulation of THRUFAL
     ZPSN(:)=PPSN(:)
!
   ZSNOWABLAT_DELTA(:) = 0.0
   ZTHRUFAL        (:) = PTHRUFAL(:)
!
!
   WHERE(LREMOVE_SNOW(:))
      ZSNOWSWE_OUT(:)     = 0.0
      PLESES(:)           = MIN(PLESES(:), XLSTT*(ZSNOWSWE_1D(:)/PTSTEP + PSR(:)))
      PLELES(:)           = 0.0
      PEVAP(:)            = PLESES(:)/XLSTT
      PTHRUFAL(:)         = MAX(0.0, ZSNOWSWE_1D(:)/PTSTEP + PSR(:) - PEVAP(:)*ZPSN(:) + ZRRSNOW(:)) ! kg m-2 s-1
      ZTHRUFAL(:)         = MAX(0.0, ZSNOWSWE_1D(:)/PTSTEP + PSR(:) - PEVAP(:)         + ZRRSNOW(:)) ! kg m-2 s-1
      ZSRSFC(:)           = 0.0
      ZRRSFC(:)           = ZRRSFC(:)
      ZSNOWABLAT_DELTA(:) = 1.0
      !PSNOWALB(:)         = XUNDEF
      ! Change VV for SVS
      PSNOWALB(:)         = 0.1
      ZEVAPCOR(:)         = 0.0
      ZSOILCOR(:)         = 0.0
      PGFLUXSNOW(:)       = PRNSNOW(:) - PHSNOW(:) - PLESES(:) - PLELES(:)
      ZSNOWHMASS(:)       = -PSR(:)*(XLMTT*PTSTEP)
      ZGSFCSNOW(:)        = 0.0
      ZDELHEATN(:)        = -ZSNOWH(:) /PTSTEP
      ZDELHEATN_SFC(:)    = -ZSNOWH1(:)/PTSTEP
      ZSNOWSFCH(:)        = ZDELHEATN_SFC(:) - (ZSWNETSNOWS(:) + ZLWNETSNOW(:)    &
                          - PHSNOW(:) - PLESES(:) - PLELES(:)) + ZGSFCSNOW(:)     &
                          - ZSNOWHMASS(:)/PTSTEP 
      ZGRNDFLUXN(:)       = (ZSNOWH(:)+ZSNOWHMASS(:))/PTSTEP + PGFLUXSNOW(:)
      PTG(:,1)            = PTG(:,1) + PTSTEP*PCT(:)*ZPSN(:)*(ZGRNDFLUXN(:) - PGRNDFLUX(:) - ZFLSN_COR(:))
      ZDELHEATG(:)        = ZDELHEATG(:)     + ZPSN(:)*(ZGRNDFLUXN(:) - PGRNDFLUX(:) - ZFLSN_COR(:))
      ZDELHEATG_SFC(:)    = ZDELHEATG_SFC(:) + ZPSN(:)*(ZGRNDFLUXN(:) - PGRNDFLUX(:) - ZFLSN_COR(:))
      PGRNDFLUX(:)        = ZGRNDFLUXN(:)
      ZFLSN_COR(:)        = 0.0
   END WHERE
!
!
   DO JWRK=1,INLVLS
      DO JJ=1,SIZE(PSNOWSWE,1)
         PSNOWSWE (JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWSWE(JJ,JWRK)
         PSNOWRHO (JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWRHO(JJ,JWRK)  + &
                                    ZSNOWABLAT_DELTA(JJ)*XRHOSMIN_ES  
         PSNOWTEMP(JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWTEMP(JJ,JWRK) + &
                                    ZSNOWABLAT_DELTA(JJ)*XTT  
         PSNOWLIQ (JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWLIQ(JJ,JWRK)        
         PSNOWAGE (JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWAGE (JJ,JWRK)
      ENDDO
   ENDDO
! 
   DO JWRK=1,INLVLS
      DO JJ=1,SIZE(PSNOWSWE,1)
         PSNOWGRAN1(JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWGRAN1(JJ,JWRK) 
         PSNOWGRAN2(JJ,JWRK)  = (1.0-ZSNOWABLAT_DELTA(JJ))*PSNOWGRAN2(JJ,JWRK)
      ENDDO
   ENDDO

   DO JWRK=1,INLVLS
      DO JJ=1,SIZE(PSNOWSWE,1)
         IF(PSNOWSWE (JJ,JWRK)==0.) THEN
             PSNOWRHO (JJ,JWRK)  = XRHOSMIN_ES  
             PSNOWTEMP(JJ,JWRK)  = XTT  
             PSNOWLIQ (JJ,JWRK)  = 0.    
             PSNOWAGE (JJ,JWRK)  = 0.
             IF(JWRK==1.) THEN
               PSNOWALB(JJ) = 0.1
             ENDIF
         ENDIF
      ENDDO
    ENDDO

!
!  ===============================================================
!
!  Compute snow mass budget 
!
   ZSNOW_MASS_BUDGET(:) = (ZSNOWSWE_1D(:)-ZSNOWSWE_OUT(:))/PTSTEP + PSR     (:)+ZRRSNOW (:) &
                                                                  - PEVAP   (:)-ZTHRUFAL(:) &
                                                                  + ZEVAPCOR(:)+ZSOILCOR(:)
!
!
!  ===============================================================
!
!  To Conserve mass in ISBA, the latent heat flux part of 
!  the EVAPCOR term must be weignted by the snow fraction 
!
   ZEVAPCOR (:) = ZEVAPCOR(:)*ZPSN(:) + ZSOILCOR(:)
!
! ===============================================================
!
! check suspicious low temperature
!
   DO JWRK=1,INLVLS
      DO JJ=1,SIZE(PSNOWSWE,1)
         IF(PSNOWSWE(JJ,JWRK)>0.0.AND.PSNOWTEMP(JJ,JWRK)<ZCHECK_TEMP)THEN
            WRITE(*,*) 'Suspicious low temperature :',PSNOWTEMP(JJ,JWRK)
            WRITE(*,*) 'At point and location      :',JJ,'LAT=',PLAT(JJ),'LON=',PLON(JJ)
            WRITE(*,*) 'At snow level / total layer:',JWRK,'/',INLVLS
            WRITE(*,*) 'SNOW MASS BUDGET (kg/m2/s) :',ZSNOW_MASS_BUDGET(JJ)
            WRITE(*,*) 'SWE BY LAYER      (kg/m2)  :',PSNOWSWE (JJ,1:INLVLS)
            WRITE(*,*) 'DEPTH BY LAYER      (m)    :',ZSNOWDZ  (JJ,1:INLVLS)
            WRITE(*,*) 'DENSITY BY LAYER   (kg/m3) :',PSNOWRHO (JJ,1:INLVLS)
            WRITE(*,*) 'TEMPERATURE BY LAYER (K)   :',PSNOWTEMP(JJ,1:INLVLS)
!            CALL ABOR1_SFX('SNOWES_ISBA: Suspicious low temperature') 
            CALL ABORT               
         ENDIF
      ENDDO
   ENDDO
!
!
! ===============================================================
!
!
!
CONTAINS
!
!================================================================
SUBROUTINE CALL_MODEL(KSIZE1,KSIZE2,KSIZE3,KMASK)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSIZE1
INTEGER, INTENT(IN) :: KSIZE2
INTEGER, INTENT(IN) :: KSIZE3
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWSWE
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWDZ
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWRHO
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWHEAT
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWTEMP
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWLIQ
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWGRAN1
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWGRAN2
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SSA
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWHIST
REAL, DIMENSION(KSIZE1,KSIZE2) :: ZP_SNOWAGE
REAL, DIMENSION(KSIZE1)        :: ZP_SNOWALB
REAL, DIMENSION(KSIZE1)        :: ZP_SWNETSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_SWNETSNOWS
REAL, DIMENSION(KSIZE1)        :: ZP_LWNETSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_PS
REAL, DIMENSION(KSIZE1)        :: ZP_SRSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_RRSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_PSNES
REAL, DIMENSION(KSIZE1)        :: ZP_TA
REAL, DIMENSION(KSIZE1)        :: ZP_CT
REAL, DIMENSION(KSIZE1,KSIZE3) :: ZP_TG
REAL, DIMENSION(KSIZE1,KSIZE3) :: ZP_D_G
REAL, DIMENSION(KSIZE1,KSIZE3) :: ZP_DZG
REAL, DIMENSION(KSIZE1,KSIZE3) :: ZP_SOILHCAPZ
REAL, DIMENSION(KSIZE1)        :: ZP_SOILD
REAL, DIMENSION(KSIZE1)        :: ZP_DELHEATG
REAL, DIMENSION(KSIZE1)        :: ZP_DELHEATG_SFC
REAL, DIMENSION(KSIZE1)        :: ZP_SW_RAD
REAL, DIMENSION(KSIZE1)        :: ZP_QA
REAL, DIMENSION(KSIZE1)        :: ZP_VMOD
REAL, DIMENSION(KSIZE1)        :: ZP_LW_RAD
REAL, DIMENSION(KSIZE1)        :: ZP_RHOA
REAL, DIMENSION(KSIZE1)        :: ZP_UREF
REAL, DIMENSION(KSIZE1)        :: ZP_EXNS
REAL, DIMENSION(KSIZE1)        :: ZP_EXNA
REAL, DIMENSION(KSIZE1)        :: ZP_DIRCOSZW
REAL, DIMENSION(KSIZE1)        :: ZP_ZREF
REAL, DIMENSION(KSIZE1)        :: ZP_Z0NAT
REAL, DIMENSION(KSIZE1)        :: ZP_Z0HNAT
REAL, DIMENSION(KSIZE1)        :: ZP_Z0EFF
REAL, DIMENSION(KSIZE1)        :: ZP_ALB
REAL, DIMENSION(KSIZE1)        :: ZP_SOILCOND
REAL, DIMENSION(KSIZE1)        :: ZP_THRUFAL
REAL, DIMENSION(KSIZE1)        :: ZP_GRNDFLUX
REAL, DIMENSION(KSIZE1)        :: ZP_FLSN_COR
REAL, DIMENSION(KSIZE1)        :: ZP_GSFCSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_EVAPCOR
REAL, DIMENSION(KSIZE1)        :: ZP_SOILCOR
REAL, DIMENSION(KSIZE1)        :: ZP_GFLXCOR
REAL, DIMENSION(KSIZE1)        :: ZP_RNSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_HSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_GFLUXSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_DELHEATN
REAL, DIMENSION(KSIZE1)        :: ZP_DELHEATN_SFC
REAL, DIMENSION(KSIZE1)        :: ZP_SNOWSFCH
REAL, DIMENSION(KSIZE1)        :: ZP_HPSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_LESES
REAL, DIMENSION(KSIZE1)        :: ZP_LELES
REAL, DIMENSION(KSIZE1)        :: ZP_EVAP
REAL, DIMENSION(KSIZE1)        :: ZP_SNDRIFT
REAL, DIMENSION(KSIZE1)        :: ZP_RI
REAL, DIMENSION(KSIZE1)        :: ZP_QS
REAL, DIMENSION(KSIZE1)        :: ZP_EMISNOW
REAL, DIMENSION(KSIZE1)        :: ZP_CDSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_USTARSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_CHSNOW
REAL, DIMENSION(KSIZE1)        :: ZP_SNOWHMASS
REAL, DIMENSION(KSIZE1)        :: ZP_VEGTYPE
REAL, DIMENSION(KSIZE1)        :: ZP_PEW_A_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PEW_B_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PET_A_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PET_B_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PEQ_A_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_PEQ_B_COEF
REAL, DIMENSION(KSIZE1)        :: ZP_ZENITH
REAL, DIMENSION(KSIZE1)        :: ZP_LAT,ZP_LON
REAL, DIMENSION(KSIZE1)        :: ZP_PSN_INV
REAL, DIMENSION(KSIZE1)        :: ZP_PSN
REAL, DIMENSION(KSIZE1)        :: ZP_PSN_GFLXCOR
REAL, DIMENSION(KSIZE1)        :: ZP_WORK
!
REAL, PARAMETER :: ZDEPTHABS = 0.60 ! m
!
INTEGER :: JWRK, JJ, JI
!
!
! Initialize:
!
ZP_PSN_GFLXCOR(:)  = 0.
ZP_WORK(:)         = 0.
ZP_SOILD(:)        = 0.
!
! pack the variables
!
DO JWRK=1,KSIZE2
   DO JJ=1,KSIZE1
      JI = KMASK(JJ)
      ZP_SNOWSWE (JJ,JWRK) = PSNOWSWE (JI,JWRK)
      ZP_SNOWRHO (JJ,JWRK) = PSNOWRHO (JI,JWRK)
      ZP_SNOWHEAT(JJ,JWRK) = ZSNOWHEAT(JI,JWRK)
      ZP_SNOWTEMP(JJ,JWRK) = PSNOWTEMP(JI,JWRK)
      ZP_SNOWLIQ (JJ,JWRK) = PSNOWLIQ (JI,JWRK)
      ZP_SNOWDZ  (JJ,JWRK) = ZSNOWDZ  (JI,JWRK)
      ZP_SNOWAGE (JJ,JWRK) = PSNOWAGE (JI,JWRK)
      ZP_SNOWGRAN1(JJ,JWRK) = PSNOWGRAN1 (JI,JWRK)
      ZP_SNOWGRAN2(JJ,JWRK) = PSNOWGRAN2 (JI,JWRK)
      ZP_SNOWHIST (JJ,JWRK) = 1.0
   ENDDO
ENDDO
!
!  
DO JWRK=1,KSIZE3
   DO JJ=1,KSIZE1
      JI                    = KMASK           (JJ)
      ZP_TG       (JJ,JWRK) = PTG        (JI,JWRK)
      ZP_D_G      (JJ,JWRK) = PD_G       (JI,JWRK)
      ZP_SOILHCAPZ(JJ,JWRK) = PSOILHCAPZ (JI,JWRK)
   ENDDO
ENDDO
!
!
DO JJ=1,KSIZE1
   JI = KMASK(JJ)
   ZP_SNOWALB (JJ) = PSNOWALB (JI)    
   ZP_PS      (JJ) = PPS      (JI)
   ZP_SRSNOW  (JJ) = PSR      (JI)
   ZP_RRSNOW  (JJ) = ZRRSNOW  (JI)
   ZP_PSNES   (JJ) = PPSN     (JI)
   ZP_CT      (JJ) = PCT      (JI)
   ZP_TA      (JJ) = PTA      (JI)
   ZP_DELHEATG(JJ) = ZDELHEATG(JI)
   ZP_DELHEATG_SFC(JJ) = ZDELHEATG_SFC(JI)
   ZP_SW_RAD  (JJ) = PSW_RAD  (JI)
   ZP_QA      (JJ) = PQA      (JI)
   ZP_VMOD    (JJ) = PVMOD    (JI)
   ZP_LW_RAD  (JJ) = PLW_RAD  (JI)
   ZP_RHOA    (JJ) = PRHOA    (JI)
   ZP_UREF    (JJ) = PUREF    (JI)
   ZP_EXNS    (JJ) = ZEXNS    (JI)
   ZP_EXNA    (JJ) = ZEXNA    (JI)
   ZP_DIRCOSZW(JJ) = ZDIRCOSZW(JI)
   ZP_ZREF    (JJ) = PZREF    (JI)
   ZP_Z0NAT   (JJ) = PZ0NAT   (JI)
   ZP_Z0HNAT  (JJ) = PZ0HNAT  (JI)
   ZP_Z0EFF   (JJ) = PZ0EFF   (JI)
   ZP_ALB     (JJ) = PALB     (JI)
   ZP_SOILCOND(JJ) = ZSOILCOND(JI)
   !  
   ZP_PEW_A_COEF(JJ) = ZPEW_A_COEF(JI)
   ZP_PEW_B_COEF(JJ) = ZPEW_B_COEF(JI)
   ZP_PET_A_COEF(JJ) = ZPET_A_COEF(JI)
   ZP_PEQ_A_COEF(JJ) = ZPEQ_A_COEF(JI)      
   ZP_PET_B_COEF(JJ) = ZPET_B_COEF(JI)
   ZP_PEQ_B_COEF(JJ) = ZPEQ_B_COEF(JI)
   !
   ZP_LAT  (JJ)      = PLAT(JI)
   ZP_LON  (JJ)      = PLON(JI)
   ZP_ZENITH(JJ)     = ZZENITH  (JI)
!
   ZP_GRNDFLUX    (JJ) = PGRNDFLUX    (JI)
   ZP_RNSNOW      (JJ) = PRNSNOW      (JI)
   ZP_HSNOW       (JJ) = PHSNOW       (JI)
   ZP_DELHEATN    (JJ) = ZDELHEATN    (JI)
   ZP_DELHEATN_SFC(JJ) = ZDELHEATN_SFC(JI)
   ZP_SNOWSFCH    (JJ) = ZSNOWSFCH    (JI)
   ZP_HPSNOW      (JJ) = PHPSNOW      (JI)
   ZP_LESES       (JJ) = PLESES       (JI) 
   ZP_LELES       (JJ) = PLELES       (JI)  
   ZP_EVAP        (JJ) = PEVAP        (JI)
   ZP_EMISNOW     (JJ) = ZEMISNOW     (JI) 
   ZP_SWNETSNOW   (JJ) = ZSWNETSNOW   (JI) 
   ZP_SWNETSNOWS  (JJ) = ZSWNETSNOWS  (JI) 
   ZP_LWNETSNOW   (JJ) = ZLWNETSNOW   (JI) 
ENDDO

DO JJ=1,KSIZE1
   JI = KMASK(JJ)
   ZP_VEGTYPE (JJ) = 0.     ! FOR SVS : not applied over permanent snow
ENDDO
!

!
! ===============================================================
! conversion of snow heat from J/m3 into J/m2
!WHERE(ZP_SNOWSWE(:,:)>0.) &
!  ZP_SNOWHEAT(:,:) = ZP_SNOWHEAT(:,:) / ZP_SNOWRHO (:,:) * ZP_SNOWSWE (:,:)  
! ===============================================================
!
ZP_PSN_INV(:)       = 0.0
ZP_PSN(:)           = ZP_PSNES(:)
!
!
! Call ISBA-SNOWES model:  
!  
!IF (HSNOW_ISBA=='CRO') THEN 
!
!   CALL SNOWCRO(HSNOWRES, TPTIME, OGLACIER, HIMPLICIT_WIND,                &
!             ZP_PEW_A_COEF, ZP_PEW_B_COEF,                                 &
!             ZP_PET_A_COEF, ZP_PEQ_A_COEF, ZP_PET_B_COEF, ZP_PEQ_B_COEF,   &
!             ZP_SNOWSWE,ZP_SNOWRHO, ZP_SNOWHEAT, ZP_SNOWALB,               &
!             ZP_SNOWGRAN1, ZP_SNOWGRAN2, ZP_SNOWHIST, ZP_SNOWAGE, PTSTEP,  &
!             ZP_PS, ZP_SRSNOW, ZP_RRSNOW ,ZP_PSNES, ZP_TA, ZP_TG(:,1),     &
!             ZP_SW_RAD, ZP_QA, ZP_VMOD, ZP_LW_RAD, ZP_RHOA, ZP_UREF,       &
!             ZP_EXNS, ZP_EXNA, ZP_DIRCOSZW, ZP_ZREF, ZP_Z0NAT, ZP_Z0EFF,   &
!             ZP_Z0HNAT, ZP_ALB, ZP_SOILCOND, ZP_D_G(:,1), ZP_SNOWLIQ,      &
!             ZP_SNOWTEMP, ZP_SNOWDZ, ZP_THRUFAL, ZP_GRNDFLUX, ZP_EVAPCOR,  &
!             ZP_RNSNOW, ZP_HSNOW, ZP_GFLUXSNOW, ZP_HPSNOW, ZP_LESES,       &
!             ZP_LELES, ZP_EVAP, ZP_SNDRIFT, ZP_RI,                         &
!             ZP_EMISNOW, ZP_CDSNOW, ZP_USTARSNOW,                          &
!             ZP_CHSNOW, ZP_SNOWHMASS, ZP_QS, ZP_VEGTYPE, ZP_ZENITH,        &
!             ZP_LAT, ZP_LON, OSNOWDRIFT,OSNOWDRIFT_SUBLIM,                 &
!             OSNOW_ABS_ZENITH, HSNOWMETAMO,HSNOWRAD                        )
!!
!  ZP_GFLXCOR (:) = 0.0
!  ZP_FLSN_COR(:) = 0.0
!  ZP_SOILCOR (:) = 0.0
!!
!ELSE 
!

  CALL SNOWES(CSNOWRES,LMEB,CIMPLICIT_WIND,                                &
             ZP_PEW_A_COEF, ZP_PEW_B_COEF,                                 &
             ZP_PET_A_COEF, ZP_PEQ_A_COEF,ZP_PET_B_COEF, ZP_PEQ_B_COEF,    &
             ZP_SNOWSWE, ZP_SNOWRHO, ZP_SNOWHEAT, ZP_SNOWALB,              &
             ZP_SNOWGRAN1, ZP_SNOWGRAN2, ZP_SNOWHIST, ZP_SNOWAGE, PTSTEP,  &
             ZP_PS, ZP_SRSNOW, ZP_RRSNOW, ZP_PSNES, ZP_TA, ZP_TG(:,1),     &
             ZP_SW_RAD, ZP_QA, ZP_VMOD, ZP_LW_RAD, ZP_RHOA, ZP_UREF,       &
             ZP_EXNS, ZP_EXNA, ZP_DIRCOSZW, ZP_ZREF, ZP_Z0NAT, ZP_Z0EFF,   &
             ZP_Z0HNAT, ZP_ALB, ZP_SOILCOND, ZP_D_G(:,1), ZP_SNOWLIQ,      &
             ZP_SNOWTEMP, ZP_SNOWDZ, ZP_THRUFAL, ZP_GRNDFLUX ,             &
             ZP_EVAPCOR, ZP_SOILCOR, ZP_GFLXCOR, ZP_SNOWSFCH,              &
             ZP_DELHEATN, ZP_DELHEATN_SFC,                                 &
             ZP_SWNETSNOW, ZP_SWNETSNOWS, ZP_LWNETSNOW, ZP_GSFCSNOW,       &
             ZP_RNSNOW, ZP_HSNOW, ZP_GFLUXSNOW, ZP_HPSNOW, ZP_LESES,       &
             ZP_LELES, ZP_EVAP, ZP_SNDRIFT, ZP_RI,                         &
             ZP_EMISNOW, ZP_CDSNOW, ZP_USTARSNOW,                          &
             ZP_CHSNOW, ZP_SNOWHMASS, ZP_QS, ZP_VEGTYPE, ZP_ZENITH,        &
             ZP_LAT, ZP_LON, LSNOWDRIFT, LSNOWDRIFT_SUBLIM, HSNOWMETAMO,   &
             ZP_SSA    )
!
!
!    To conserve energy in ISBA, the correction flux must be distributed at least
!    over the first 60cm depth. This method prevent numerical oscillations
!    especially when explicit snow vanishes. Final Adjustments are done in ISBA_CEB
!
     ZP_FLSN_COR(:) = ZP_GFLXCOR(:) ! (W/m2)
  
!
!
!===============================================================
!conversion of snow heat from J/m2 into J/m3
!WHERE(ZP_SNOWSWE (:,:)>0.)
!      ZP_SNOWHEAT(:,:)=ZP_SNOWHEAT(:,:)*ZP_SNOWRHO(:,:)/ZP_SNOWSWE(:,:)  
!ENDWHERE
!===============================================================
!
! === Packing:
!
! unpack variables
!
DO JWRK=1,KSIZE2
  DO JJ=1,KSIZE1
    JI = KMASK(JJ)
    PSNOWSWE  (JI,JWRK) = ZP_SNOWSWE  (JJ,JWRK)
    PSNOWRHO  (JI,JWRK) = ZP_SNOWRHO  (JJ,JWRK)
    ZSNOWHEAT (JI,JWRK) = ZP_SNOWHEAT (JJ,JWRK)
    PSNOWTEMP (JI,JWRK) = ZP_SNOWTEMP (JJ,JWRK)
    PSNOWLIQ  (JI,JWRK) = ZP_SNOWLIQ  (JJ,JWRK)
    ZSNOWDZ   (JI,JWRK) = ZP_SNOWDZ   (JJ,JWRK)
    PSNOWAGE  (JI,JWRK) = ZP_SNOWAGE  (JJ,JWRK)
  ENDDO
ENDDO
!
DO JWRK=1,KSIZE2
  DO JJ=1,KSIZE1
    JI = KMASK(JJ)
    PSNOWGRAN1(JI,JWRK) = ZP_SNOWGRAN1(JJ,JWRK)
    PSNOWGRAN2(JI,JWRK) = ZP_SNOWGRAN2(JJ,JWRK)
    PSSA      (JI,JWRK) = ZP_SSA      (JJ,JWRK)
  ENDDO
ENDDO
!
DO JWRK=1,KSIZE3
   DO JJ=1,KSIZE1
      JI              = KMASK          (JJ)
      PTG    (JI,JWRK)= ZP_TG        (JJ,JWRK)
   ENDDO
ENDDO
!
DO JJ=1,KSIZE1
  JI                  = KMASK          (JJ)
  ZDELHEATG    (JI)   = ZP_DELHEATG    (JJ)
  ZDELHEATG_SFC(JI)   = ZP_DELHEATG_SFC(JJ)
  PSNOWALB     (JI)   = ZP_SNOWALB     (JJ)
  PTHRUFAL     (JI)   = ZP_THRUFAL     (JJ)
  ZEVAPCOR     (JI)   = ZP_EVAPCOR     (JJ)
  ZSOILCOR     (JI)   = ZP_SOILCOR     (JJ)
  ZRI          (JI)   = ZP_RI          (JJ)
  ZQS          (JI)   = ZP_QS          (JJ)
  ZCDSNOW      (JI)   = ZP_CDSNOW      (JJ)
  ZUSTARSNOW   (JI)   = ZP_USTARSNOW   (JJ)
  ZCHSNOW      (JI)   = ZP_CHSNOW      (JJ)
  ZSNOWHMASS   (JI)   = ZP_SNOWHMASS   (JJ)
  PGRNDFLUX    (JI)   = ZP_GRNDFLUX    (JJ)
  ZFLSN_COR    (JI)   = ZP_FLSN_COR    (JJ)
  PRNSNOW      (JI)   = ZP_RNSNOW      (JJ)
  PHSNOW       (JI)   = ZP_HSNOW       (JJ)
  PGFLUXSNOW   (JI)   = ZP_GFLUXSNOW   (JJ)
  ZDELHEATN    (JI)   = ZP_DELHEATN    (JJ)
  ZDELHEATN_SFC(JI)   = ZP_DELHEATN_SFC(JJ)
  ZSNOWSFCH    (JI)   = ZP_SNOWSFCH    (JJ)
  ZGSFCSNOW    (JI)   = ZP_GSFCSNOW    (JJ)
  PHPSNOW      (JI)   = ZP_HPSNOW      (JJ)
  PLESES       (JI)   = ZP_LESES       (JJ)
  PLELES       (JI)   = ZP_LELES       (JJ)
  PEVAP        (JI)   = ZP_EVAP        (JJ)
  ZEMISNOW     (JI)   = ZP_EMISNOW     (JJ)
  ZSWNETSNOW   (JI)   = ZP_SWNETSNOW   (JJ)
  ZSWNETSNOWS  (JI)   = ZP_SWNETSNOWS  (JJ)
  ZLWNETSNOW   (JI)   = ZP_LWNETSNOW   (JJ)
ENDDO
!
!
END SUBROUTINE CALL_MODEL
!
END SUBROUTINE SNOWES_SVS
