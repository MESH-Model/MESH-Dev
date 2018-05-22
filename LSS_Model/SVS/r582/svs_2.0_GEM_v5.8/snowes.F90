!     #########
!     NEED TO DECLARE WITHIN A MODULE CALLED BY SNOWES_SVS FOR THE assumed shape array to work i.e., arrays with : instead of hard-coded dimension
!     By using a module, both snowES_svs and snowES are compiled at the same time and the info about dimensions is passed
!
module snowES_mod

  private


  public :: snowES

contains

      SUBROUTINE SNOWES(HSNOWRES, OMEB, HIMPLICIT_WIND,           &
                PPEW_A_COEF, PPEW_B_COEF,                                 &
                PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,       &
                PSNOWSWE,PSNOWRHO,PSNOWHEAT,PSNOWALB,                     &
                PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,PSNOWAGE,                 &                
                PTSTEP,PPS,PSR,PRR,PPSNES,                                &
                PTA,PTG,PSW_RAD,PQA,PVMOD,PLW_RAD, PRHOA,                 &
                PUREF,PEXNS,PEXNA,PDIRCOSZW,                              &
                PZREF,PZ0,PZ0EFF,PZ0H,PALB,                               &
                PSOILCOND,PD_G,                                           &
                PSNOWLIQ,PSNOWTEMP,PSNOWDZ,                               &
                PTHRUFAL,PGRNDFLUX,PEVAPCOR,PSOILCOR,                     &
                PGFLXCOR,PSNOWSFCH, PDELHEATN, PDELHEATN_SFC,             &
                PSWNETSNOW,PSWNETSNOWS,PLWNETSNOW,PSNOWFLUX,              &
                PRNSNOW,PHSNOW,PGFLUXSNOW,                                &
                PHPSNOW,PLESES,PLELES,PEVAP,PSNDRIFT,PRI,                 &
                PEMISNOW,PCDSNOW,PUSTAR,PCHSNOW,PSNOWHMASS,PQS,           &
                PPERMSNOWFRAC,PZENITH,PXLAT,PXLON,                        &
                OSNOWDRIFT,OSNOWDRIFT_SUBLIM,HSNOWMETAMO,PSSA             )  
!     ##########################################################################
!
!!****  *SNOWES*
!!
!!    PURPOSE
!!    -------
!
!     Multi-Layer explicit snow scheme option (Boone and Etchevers, J Hydrometeor., 2000)
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
!!
!!    REFERENCE
!!    ---------
!!
!!    ISBA-ES: Boone and Etchevers (2001)
!!    ISBA: Belair (1995)
!!    ISBA: Noilhan and Planton (1989)
!!    ISBA: Noilhan and Mahfouf (1996)
!!
!!    AUTHOR
!!    ------
!!      A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    7/99
!!      Modified by A.Boone 05/02 (code, not physics)
!!      Modified by A.Boone 11/04 i) maximum density limit imposed (although
!!                                rarely if ever reached), ii) check to
!!                                see if upermost layer completely sublimates
!!                                during a timestep (as snowpack becomes vanishly
!!                                thin), iii) impose maximum grain size limit
!!                                in radiation transmission computation.
!!
!!      Modified by B. Decharme  (03/2009): Consistency with Arpege permanent
!!                                          snow/ice treatment (LGLACIER for alb)
!!      Modified by A. Boone     (04/2010): Implicit coupling and replace Qsat and DQsat
!!                                          by Qsati and DQsati, respectively.
!!      Modified by E. Brun      (08/2012): Mass conservation in SNOWESEVAPGONE
!!      Modified by B. Decharme  (08/2012): Loop optimization
!!      Modified by B. Decharme  (09/2012): New wind implicitation
!!      Modified by E. Brun      (10/2012): Bug in vertical snow redistribution
!!      Modified by B. Decharme  (08/2013): Qsat as argument (needed for coupling with atm)
!!                                          Add snow drift effect like in CROCUS
!!                                          Change snow albedo (CROCUS spectral albedo)
!!                                          Change snow viscosity like in CROCUS
!!      Modified by P. Samuelsson(10/2014): MEB added an optional snow albedo calculation
!!                                          for litter
!!      Modified by A. Boone     (10/2014): MEB modifs: removed above since spectral albedo 
!!                                          method from CROCUS added...can eventually add above
!!                                          back in a manner consistent with spectral bands...
!!      Modified by A. Boone     (10/2014): MEB modifs: permit option to impose fluxes at sfc  
!!      Modified by A. Boone     (10/2014): SNOWESREFRZ and SNOWESEVAPN edited to give
!!                                          better enthalpy conservation.
!!      Modified by C. Garnaud   (06/2016): Add specific surface area (SSA) calculation
!!      Modified by C. Garnaud   (07/2017): Update PSNOWSWE before metamorphism calculations
!!      Modified by C. Garnaud   (07/2017): Add a max on ZTELM (as done in SNOW3LHOLD) in wet 
!!                                          snow metamorphism in order to avoid instabilities 
!!                                          in SSA calculations
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS_SNOWES,     ONLY : XTT, XRHOLW, XLMTT, XCL, XDAY, XRHOLI
!
USE MODD_SNOWES_PAR,    ONLY : XSNOWDZMIN,XSNOWDMIN, NSPEC_BAND_SNOW,XUNDEF
!
USE MODE_SNOWES
USE MODE_THERMOS_SNOW,  ONLY : QSATI
!
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!                                      PTSTEP    = time step of the integration
!
CHARACTER(LEN=*),     INTENT(IN)    :: HSNOWRES
!                                      HSNOWRES  = ISBA-SNOWES turbulant exchange option
!                                      'DEF' = Default: Louis (ISBA: Noilhan and Mahfouf 1996)
!                                      'RIL' = Limit Richarson number under very stable
!                                              conditions (currently testing)
!
LOGICAL, INTENT(IN)                 :: OMEB       ! True = coupled to MEB. This means surface fluxes ae IMPOSED
!                                                 ! as an upper boundary condition to the explicit snow schemes. 
!                                                 ! If = False, then energy
!                                                 ! budget and fluxes are computed herein.
!
CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
!
REAL, DIMENSION(:), INTENT(IN)    :: PPS, PTA, PSW_RAD, PQA,                       &
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
REAL, DIMENSION(:), INTENT(IN)    :: PSOILCOND, PD_G, PPSNES
!                                      PSOILCOND = soil thermal conductivity [W/(m K)]
!                                      PD_G      = Assumed first soil layer thickness (m)
!                                                  Used to calculate ground/snow heat flux
!                                      PPSNES    = snow fraction
!
REAL, DIMENSION(:), INTENT(IN)    :: PZREF, PUREF, PEXNS, PEXNA, PDIRCOSZW, PRHOA, PZ0, PZ0EFF, &
                                       PALB, PZ0H, PPERMSNOWFRAC  
!                                      PZ0EFF    = roughness length for momentum
!                                      PZ0       = grid box average roughness length
!                                      PZ0H      = grid box average roughness length for heat
!                                      PZREF     = reference height of the first
!                                                  atmospheric level
!                                      PUREF     = reference height of the wind
!                                      PRHOA     = air density
!                                      PEXNS     = Exner function at surface
!                                      PEXNA     = Exner function at lowest atmos level
!                                      PDIRCOSZW = Cosinus of the angle between the
!                                                  normal to the surface and the vertical
!                                      PALB      = soil/vegetation albedo
!                                      PPERMSNOWFRAC  = fraction of permanet snow/ice
!
REAL, DIMENSION(:), INTENT(IN)      :: PPEW_A_COEF, PPEW_B_COEF,                   &
                                         PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,      &
                                         PPEQ_B_COEF  
!                                      PPEW_A_COEF = wind coefficient (m2s/kg)
!                                      PPEW_B_COEF = wind coefficient (m/s)
!                                      PPET_A_COEF = A-air temperature coefficient
!                                      PPET_B_COEF = B-air temperature coefficient
!                                      PPEQ_A_COEF = A-air specific humidity coefficient
!                                      PPEQ_B_COEF = B-air specific humidity coefficient
!
REAL, DIMENSION(:), INTENT(IN)    :: PTG
!                                      PTG       = Surface soil temperature (effective
!                                                  temperature the of layer lying below snow)
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOWALB
!                                      PSNOWALB = Prognostic surface snow albedo
!                                                 (does not include anything but
!                                                 the actual snow cover)
!
REAL, DIMENSION(:,:), INTENT(INOUT):: PSNOWHEAT, PSNOWRHO, PSNOWSWE
!                                      PSNOWHEAT = Snow layer(s) heat content (J/m2)
!                                      PSNOWRHO  = Snow layer(s) averaged density (kg/m3)
!                                      PSNOWSWE  = Snow layer(s) liquid Water Equivalent (SWE:kg m-2)
!
REAL, DIMENSION(:,:), INTENT(INOUT):: PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST
!                                      PSNOWGRAN1 = Snow layers grain feature 1
!                                      PSNOWGRAN2 = Snow layer grain feature 2
!                                      PSNOWHIST  = Snow layer grain historical
!                                                   parameter (only for non
!                                                   dendritic snow)
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWAGE  ! Snow grain age
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PRNSNOW, PHSNOW, PLESES, PLELES, &
                                       PHPSNOW, PEVAP,  PGRNDFLUX, PEMISNOW
!                                      PLESES      = evaporation heat flux from snow (W/m2)
!                                      PLELES      = sublimation (W/m2)
!                                      PHPSNOW     = heat release from rainfall (W/m2)
!                                      PRNSNOW     = net radiative flux from snow (W/m2)
!                                      PHSNOW      = sensible heat flux from snow (W/m2)
!                                      PEVAP       = total evaporative flux (kg/m2/s)
!                                      PGRNDFLUX   = soil/snow interface heat flux (W/m2)
!                                      PEMISNOW    = snow surface emissivity
!
REAL, DIMENSION(:), INTENT(OUT)     :: PGFLUXSNOW
!                                      PGFLUXSNOW  = net heat flux from snow (W/m2)
!
REAL, DIMENSION(:), INTENT(INOUT) :: PSWNETSNOW, PLWNETSNOW, PSWNETSNOWS
!                                      PSWNETSNOW = net shortwave radiation entering top of snowpack 
!                                                  (W m-2) Imposed if MEB=T, diagnosed herein if MEB=F
!                                      PSWNETSNOWS= net shortwave radiation in uppermost layer of snowpack 
!                                                  (W m-2) Imposed if MEB=T, diagnosed herein if MEB=F
!                                                   Used for surface energy budget diagnostics
!                                      PLWNETSNOW = net longwave radiation entering top of snowpack 
!                                                  (W m-2) Imposed if MEB=T, diagnosed herein if MEB=F
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PUSTAR, PCDSNOW, PCHSNOW, PRI
!                                      PCDSNOW    = drag coefficient for momentum over snow (-)
!                                      PUSTAR     = friction velocity over snow (m/s)
!                                      PCHSNOW    = drag coefficient for heat over snow (-)
!                                      PRI        = Richardson number (-)
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWTEMP
REAL, DIMENSION(:,:), INTENT(OUT)   :: PSNOWLIQ, PSNOWDZ
!                                      PSNOWLIQ  = Snow layer(s) liquid water content (m)
!                                      PSNOWTEMP = Snow layer(s) temperature (m)
!                                      PSNOWDZ   = Snow layer(s) thickness (m)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PTHRUFAL, PEVAPCOR, PSOILCOR, PGFLXCOR, &
                                       PSNOWFLUX, PSNOWSFCH, PDELHEATN, PDELHEATN_SFC
!                                      PTHRUFAL  = rate that liquid water leaves snow pack:
!                                                  paritioned into soil infiltration/runoff
!                                                  by ISBA [kg/(m2 s)]
!                                      PEVAPCOR  = evaporation/sublimation correction term:
!                                                  extract any evaporation exceeding the
!                                                  actual snow cover (as snow vanishes)
!                                                  and apply it as a surface soil water
!                                                  sink. [kg/(m2 s)]
!                                      PSOILCOR = for vanishingy thin snow cover,
!                                                 allow any excess evaporation
!                                                 to be extracted from the soil
!                                                 to maintain an accurate water
!                                                 balance [kg/(m2 s)]
!                                      PGFLXCOR  = flux correction to underlying soil for vanishing snowpack
!                                                  (to put any energy excess from snow to soil) (W/m2)
!                                      PSNOWFLUX = heat flux between the surface and sub-surface 
!                                                  snow layers (W/m2)
!                                      PSNOWSFCH = snow surface layer pseudo-heating term owing to
!                                                  changes in grid thickness            (W m-2)
!                                      PDELHEATN = total snow heat content change in the surface layer (W m-2)
!                                      PDELHEATN_SFC = total snow heat content change during the timestep (W m-2)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PSNDRIFT
!                                      PSNDRIFT    = blowing snow sublimation (kg/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)   ::   PSNOWHMASS
!                                      PSNOWHMASS  = heat content change due to mass
!                                                    changes in snowpack (J/m2): for budget
!                                                    calculations only.
!
REAL, DIMENSION(:), INTENT(OUT)   :: PQS
!                                    PQS = surface humidity
!
REAL, DIMENSION(:), INTENT(IN)    :: PZENITH ! solar zenith angle
REAL, DIMENSION(:), INTENT(IN)    :: PXLAT,PXLON ! LAT/LON after packing
!
LOGICAL, INTENT(IN)               :: OSNOWDRIFT, OSNOWDRIFT_SUBLIM ! activate snowdrift, sublimation during drift
!
CHARACTER(3), INTENT(IN)          :: HSNOWMETAMO
                                     !-----------------------
                                     ! Metamorphism scheme
                                     ! HSNOWMETAMO=B92 Brun et al 1992
                                     ! HSNOWMETAMO=C13 Carmagnola et al 2014
                                     ! HSNOWMETAMO=T07 Taillandier et al 2007
                                     !-----------------------
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PSSA
!                                        PSSA    = Specific Surface Area (m2/kg)
!
!*      0.2    declarations of local variables
!
INTEGER                            :: JJ, JI     ! Loop control
!
INTEGER                            :: INI        ! number of point
INTEGER                            :: INLVLS     ! number of snow layers
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWTEMP, ZSCAP, ZSNOWDZN, ZSCOND,    &
                                                      ZRADSINK, ZWORK2D, ZSNOWTEMPO  
!                                      ZSNOWTEMP  = Snow layer(s) averaged temperature (K)
!                                      ZSCAP      = Snow layer(s) heat capacity [J/(K m3)]
!                                      ZSNOWDZN   = Updated snow layer thicknesses (m)
!                                      ZSCOND     = Snow layer(s) thermal conducivity [W/(m K)]
!                                      ZRADSINK   = Snow solar Radiation source terms (W/m2)
!                                      ZWORK2D    = working variable (*)
!
REAL, DIMENSION(SIZE(PTA))          :: ZSNOW, ZSFCFRZ, ZTSTERM1, ZTSTERM2,                   &
                                       ZCT, ZRA, ZSNOWTEMPO1  
!                                      ZSNOW      = Total snow depth (m)
!                                      ZCT        = inverse of the product of snow heat capacity
!                                                   and layer thickness [(m2 K)/J]
!                                      ZRA        = Surface aerodynamic resistance
!                                      ZTSTERM1,ZTSTERM2 = Surface energy budget coefficients
!                                      ZSNOWTEMPO1= value of uppermost snow temperature
!                                                   before time integration (K)
!
LOGICAL, DIMENSION(SIZE(PTA))       :: GSFCMELT
!                                      GSFCMELT   = FLAG if surface melt is occurring, used
!                                                   for surface albedo calculation.
!
REAL, DIMENSION(SIZE(PTA))          :: ZRSRA, ZDQSAT, ZQSAT, ZRADXS, ZMELTXS, ZLIQHEATXS, &
                                       ZLWUPSNOW, ZGRNDFLUX, ZGRNDFLUXO, ZGRNDFLUXI, ZPSNES
!                                      ZRSRA    = air density over aerodynamic resistance
!                                      ZDQSAT   = derrivative of saturation specific humidity
!                                      ZQSAT    = saturation specific humidity
!                                      ZRADXS   = shortwave radiation absorbed by soil surface
!                                                 (for thin snow sover) (W m-2)
!                                      ZMELTXS  = excess energy for snowmelt for vanishingly
!                                                 thin snowpacks: used to heat underlying surface
!                                                 in order to maintain energy conservation (W m-2)
!                                      ZLIQHEATXS = excess snowpack heating for vanishingly thin
!                                                 snow cover: add energy to snow/ground heat
!                                                 flux (W m-2)
!                                      ZLWUPSNOW = upwelling longwave radiative flux (W m-2)
!                                      ZGRNDFLUXO= snow-ground flux before correction (W m-2)
!                                      ZGRNDFLUX = snow-ground flux after correction (W m-2)
!                                      ZGRNDFLUXI= for the case where the ground flux is imposed,
!                                                  this is the actual imposed value.
!                                      ZPSNES    = snow fraction: different use if MEB "on".
!                                                  In this case, it is only used for Tg update
!                                                  since only this variable has a sub-grid relevance.
!
REAL, DIMENSION(SIZE(PTA))          :: ZUSTAR2_IC, ZTA_IC, ZQA_IC, ZWORK, ZWORK2, ZWORK3,                 &
                                       ZPET_A_COEF_T, ZPEQ_A_COEF_T, ZPET_B_COEF_T, ZPEQ_B_COEF_T  
!                                      ZUSTAR2_IC    = implicit lowest atmospheric level friction (m2/s2)
!                                      ZTA_IC        = implicit lowest atmospheric level air temperature
!                                      ZQA_IC        = implicit lowest atmospheric level specific humidity
!                                      ZPET_A_COEF_T = transformed A-air temperature coefficient
!                                      ZPET_B_COEF_T = transformed B-air temperature coefficient
!                                      ZPEQ_A_COEF_T = transformed A-air specific humidity coefficient
!                                      ZPEQ_B_COEF_T = transformed B-air specific humidity coefficient
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),NSPEC_BAND_SNOW)  :: ZSPECTRALALBEDO, ZSPECTRALWORK
!                                                     ZSPECTRALALBEDO=spectral albedo 
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWHEAT0

CHARACTER(3)          :: CSNOWHOLD

!
!
! - - ---------------------------------------------------
!
!       0.     Initialization
!               --------------
! NOTE that snow layer thickness is used throughout this code: SWE
! is only used to diagnose the thickness at the beginning of this routine
! and it is updated at the end of this routine.
!
!
PSNOWDZ(:,:) = PSNOWSWE(:,:)/PSNOWRHO(:,:)
!
INI          = SIZE(PSNOWSWE(:,:),1)
INLVLS       = SIZE(PSNOWSWE(:,:),2)    ! total snow layers
!
ZUSTAR2_IC = 0.0
ZTA_IC     = 0.0
ZQA_IC     = 0.0
!
ZGRNDFLUXO = 0.0
ZGRNDFLUX  = PGRNDFLUX
!
ZSNOWHEAT0(:,:)  = PSNOWHEAT(:,:) ! save initial heat content
!
ZSNOWTEMPO(:,:)  = PSNOWTEMP(:,:) ! for MEB, this is the updated T profile
!
! Parameterization for maximal water holding capacity
CSNOWHOLD= 'B92'
!
!
!*       1.     Snow total depth
!               ----------------
!
ZSNOW(:) = 0.
DO JJ=1,INLVLS
   DO JI=1,INI
      ZSNOW(JI) = ZSNOW(JI) + PSNOWDZ(JI,JJ)  ! m
   ENDDO
END DO
!
ZWORK(:)=ZSNOW(:)
!
! Caluclate new snow albedo at time t
!
ZWORK2(:)=PSNOWALB(:)


!
CALL SNOW3LALB(ZWORK2,ZSPECTRALALBEDO,PSNOWRHO(:,1),PSNOWAGE(:,1),PPERMSNOWFRAC,PPS)
ZWORK3(:) = PSNOWALB(:)/ZWORK2(:)
DO JJ=1,SIZE(ZSPECTRALALBEDO,2)
   DO JI=1,INI
      ZSPECTRALALBEDO(JI,JJ)=ZSPECTRALALBEDO(JI,JJ)*ZWORK3(JI)
   ENDDO
ENDDO
!
!*       2.     Snowfall
!               --------
!
! Caluclate uppermost density and thickness changes due to snowfall,
! and add heat content of falling snow
!
 CALL SNOW3LFALL(PTSTEP,PSR,PTA,PVMOD,ZSNOW,PSNOWRHO,PSNOWDZ,          &
                 PSNOWHEAT,PSNOWHMASS,PSNOWAGE,PPERMSNOWFRAC,          &
                 PSNOWGRAN1,PSNOWGRAN2,HSNOWMETAMO,PUREF,PZ0EFF,       &
                 OSNOWDRIFT       )
!
! Caluclate new snow albedo at time t if snowfall
!
CALL SNOW3LALB(ZWORK2,ZSPECTRALWORK,PSNOWRHO(:,1),PSNOWAGE(:,1),PPERMSNOWFRAC,PPS)
!
DO JJ=1,SIZE(ZSPECTRALALBEDO,2)
   DO JI=1,INI
      IF(ZWORK(JI)==0.0.AND.PSR(JI)>0.0)THEN
         ZSPECTRALALBEDO(JI,JJ) = ZSPECTRALWORK(JI,JJ)
      ENDIF
   ENDDO
ENDDO
WHERE(ZWORK(:)==0.0.AND.PSR(:)>0.0)
     PSNOWALB(:) = ZWORK2(:)
ENDWHERE
!
!*       3.     Update grid/discretization
!               --------------------------
! Reset grid to conform to model specifications:
!
CALL SNOW3LGRID(ZSNOWDZN,ZSNOW,PSNOWDZ_OLD=PSNOWDZ)
!
! Mass/Heat redistribution:
!
CALL SNOW3LTRANSF(ZSNOW,PSNOWDZ,ZSNOWDZN,PSNOWRHO,PSNOWHEAT,PSNOWAGE,     &
                  PSNOWGRAN1,PSNOWGRAN2 )
!
!
!*       4.     Liquid water content and snow temperature
!               -----------------------------------------
!
! First diagnose snow temperatures and liquid
! water portion of the snow from snow heat content:
!
! 
PSNOWSWE(:,:) =  PSNOWDZ(:,:)*PSNOWRHO(:,:)
!
ZSCAP(:,:)     = SNOW3LSCAP(PSNOWRHO)
!


ZSNOWTEMP(:,:) = XTT + ( ((PSNOWHEAT(:,:)/PSNOWDZ(:,:))                   &
                   + XLMTT*PSNOWRHO(:,:))/ZSCAP(:,:) )  
!
PSNOWLIQ(:,:)  = MAX(0.0,ZSNOWTEMP(:,:)-XTT)*ZSCAP(:,:)*                  &
                   PSNOWDZ(:,:)/(XLMTT*XRHOLW)  
!
ZSNOWTEMP(:,:) = MIN(XTT,ZSNOWTEMP(:,:))
!
!
!
!        4.BIS   Snow metamorphism
!                ----------------- 
! 
CALL SNOWCROMETAMO(PSNOWDZ,PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,ZSNOWTEMP, &
                    PSNOWLIQ,PTSTEP,PSNOWSWE,PSNOWAGE,HSNOWMETAMO,PSNOWRHO)
!
!
!
!*       5.     Snow Compaction
!               ---------------
!
! Calculate snow density: compaction/aging: density increases
!
CALL SNOW3LCOMPACTN(PTSTEP,XSNOWDZMIN,PSNOWRHO,PSNOWDZ,ZSNOWTEMP,ZSNOW,PSNOWLIQ)
!
! Snow compaction and metamorphism due to drift
!
PSNDRIFT(:) = 0.0
IF (OSNOWDRIFT) THEN
!
! This routine needs to be modified to include snow metamorphism
! in case SNOWDRIFT is turned on
!
   CALL SNOW3LDRIFT(PTSTEP,PVMOD,PTA,PQA,PPS,PRHOA,PSNOWRHO,&
                    PSNOWDZ,ZSNOW,OSNOWDRIFT_SUBLIM,PSNDRIFT)
ENDIF
!
! Update snow heat content (J/m2):
!
ZSCAP(:,:)     = SNOW3LSCAP(PSNOWRHO)
PSNOWHEAT(:,:) = PSNOWDZ(:,:)*( ZSCAP(:,:)*(ZSNOWTEMP(:,:)-XTT)        &
                   - XLMTT*PSNOWRHO(:,:) ) + XLMTT*XRHOLW*PSNOWLIQ(:,:)  
!
!
!*       6.     Solar radiation transmission
!               -----------------------------
!
! Heat source (-sink) term due to shortwave
! radiation transmission within the snowpack:
!
CALL SNOW3LRAD(OMEB,XSNOWDZMIN,PSW_RAD,PSNOWALB,      &
               ZSPECTRALALBEDO,PSNOWDZ,PSNOWRHO,PALB, &
               PPERMSNOWFRAC,PZENITH,PSWNETSNOW,      &
               PSWNETSNOWS,ZRADSINK,ZRADXS,PSNOWAGE)  
!
!
!*       7.     Heat transfer and surface energy budget
!               ---------------------------------------
! Snow thermal conductivity:
!
CALL SNOW3LTHRM(PSNOWRHO,ZSCOND,ZSNOWTEMP,PPS)
!
! Precipitation heating term:
! Rainfall renders it's heat to the snow when it enters
! the snowpack:
! NOTE: for now set to zero because we'd need to remove this heat from
!       the atmosphere to conserve energy. This can be done, but should
!       be tested within an atmos model first probably...NOTE, this
!       could be actiavted for use in OFFLINE mode however.
!
!PHPSNOW(:)     = PRR(:)*XCL*(MAX(XTT,PTA(:))-XTT)    ! (W/m2)
PHPSNOW(:) = 0.0
!
! Surface Energy Budget calculations using ISBA linearized form
! and standard ISBA turbulent transfer formulation
!
IF(OMEB)THEN 

! - surface fluxes prescribed:

   CALL SNOW3LEBUDMEB(PTSTEP,XSNOWDZMIN,                                     &                
                ZSNOWTEMP(:,1),PSNOWDZ(:,1),PSNOWDZ(:,2),                    & 
                ZSCOND(:,1),ZSCOND(:,2),ZSCAP(:,1),                          &
                PSWNETSNOWS,PLWNETSNOW,                                      &
                PHSNOW,PLESES,PLELES,PHPSNOW,                                &   
                ZCT,ZTSTERM1,ZTSTERM2,PGFLUXSNOW)

! - for mass fluxes, snow fraction notion removed

   ZPSNES(:) = 1.0

ELSE

   ZPSNES(:) = PPSNES(:)

! - compute surface energy budget and fluxes:

   CALL SNOW3LEBUD(HSNOWRES, HIMPLICIT_WIND,                                   &
                  PPEW_A_COEF, PPEW_B_COEF,                                    &
                  PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,          &
                  XSNOWDZMIN,                                                  &
                  PZREF,ZSNOWTEMP(:,1),PSNOWRHO(:,1),PSNOWLIQ(:,1),ZSCAP(:,1), &
                  ZSCOND(:,1),ZSCOND(:,2),                                     &
                  PUREF,PEXNS,PEXNA,PDIRCOSZW,PVMOD,                           &
                  PLW_RAD,PSW_RAD,PTA,PQA,PPS,PTSTEP,                          &
                  PSNOWDZ(:,1),PSNOWDZ(:,2),PSNOWALB,PZ0,PZ0EFF,PZ0H,          &
                  ZSFCFRZ,ZRADSINK(:,1),PHPSNOW,                               &
                  ZCT,PEMISNOW,PRHOA,ZTSTERM1,ZTSTERM2,ZRA,PCDSNOW,PCHSNOW,    &
                  ZQSAT, ZDQSAT, ZRSRA, ZUSTAR2_IC, PRI,                       &
                  ZPET_A_COEF_T,ZPEQ_A_COEF_T,ZPET_B_COEF_T,ZPEQ_B_COEF_T      )  
!
ENDIF
!
! Heat transfer: simple diffusion along the thermal gradient
!
ZSNOWTEMPO1(:) = ZSNOWTEMP(:,1) ! save surface snow temperature before update
!
ZGRNDFLUXI(:)  = ZGRNDFLUX(:)


!
CALL SNOW3LSOLVT(OMEB,PTSTEP,XSNOWDZMIN,PSNOWDZ,ZSCOND,ZSCAP,PTG,              &
                   PSOILCOND,PD_G,ZRADSINK,ZCT,ZTSTERM1,ZTSTERM2,              &
                   ZPET_A_COEF_T,ZPEQ_A_COEF_T,ZPET_B_COEF_T,ZPEQ_B_COEF_T,    &
                   ZTA_IC,ZQA_IC,ZGRNDFLUX,ZGRNDFLUXO,ZSNOWTEMP,PSNOWFLUX      )  
!

!
!*       8.     Surface fluxes
!               --------------
! 
! Since surface fluxes already computed under MEB option (and already
! recomputed for the case when T>Tf), Only call if MEB not in use:
!
IF(.NOT.OMEB)THEN 

   CALL SNOW3LFLUX(ZSNOWTEMP(:,1),PSNOWDZ(:,1),PEXNS,PEXNA,           &
                  ZUSTAR2_IC,                                         &
                  PTSTEP,PSNOWALB,PSW_RAD,                            &
                  PEMISNOW,ZLWUPSNOW,PLW_RAD,PLWNETSNOW,              &
                  ZTA_IC,ZSFCFRZ,ZQA_IC,PHPSNOW,                      &
                  ZSNOWTEMPO1,PSNOWFLUX,ZCT,ZRADSINK(:,1),            &
                  ZQSAT,ZDQSAT,ZRSRA,                                 &
                  PRNSNOW,PHSNOW,PGFLUXSNOW,PLESES,PLELES,PEVAP,      &
                  PUSTAR,GSFCMELT                                     )
!
ENDIF                  
!
!
!*       9.     Snow melt
!               ---------
!
! First Test to see if snow pack vanishes during this time step:
!
CALL SNOW3LGONE(PTSTEP,PLELES,PLESES,PSNOWRHO,                            &
                PSNOWHEAT,ZRADSINK(:,INLVLS),PEVAPCOR,PTHRUFAL,ZGRNDFLUX, &
                PGFLUXSNOW,ZGRNDFLUXO,PSNOWDZ,PSNOWLIQ,ZSNOWTEMP,ZRADXS   )  
!
! For "normal" melt: transform excess heat content into snow liquid:
!
CALL SNOW3LMELT(PTSTEP,ZSCAP,ZSNOWTEMP,PSNOWDZ,PSNOWRHO,PSNOWLIQ,ZMELTXS)  
!
!
!*      10.     Snow water flow and refreezing
!               ------------------------------
! Liquid water vertical transfer and possible snowpack runoff
! And refreezing/freezing of meltwater/rainfall (ripening of the snow)
!
CALL SNOW3LREFRZ(PTSTEP,PRR,PSNOWRHO,ZSNOWTEMP,PSNOWDZ,PSNOWLIQ,PTHRUFAL)
!
ZSCAP(:,:)        = SNOW3LSCAP(PSNOWRHO)
PSNOWHEAT(:,:)    = PSNOWDZ(:,:)*( ZSCAP(:,:)*(ZSNOWTEMP(:,:)-XTT)        &
                      - XLMTT*PSNOWRHO(:,:) ) + XLMTT*XRHOLW*PSNOWLIQ(:,:) 
!
!
!*      11.     Snow Evaporation/Sublimation mass updates:
!               ------------------------------------------
!
CALL SNOW3LEVAPN(ZPSNES,PLESES,PLELES,PTSTEP,ZSNOWTEMP(:,1),PSNOWRHO(:,1), &
                   PSNOWDZ,PSNOWLIQ(:,1),PTA,PSNOWHEAT,PSOILCOR            )
!
! Update snow temperatures and liquid
! water portion of the snow from snow heat content
! since vertical distribution of enthalpy might have been
! modified in the previous routine:
!
ZSCAP(:,:)     = SNOW3LSCAP(PSNOWRHO)
ZWORK2D(:,:)   = MIN(1.0, PSNOWDZ(:,:)/XSNOWDMIN)
ZSNOWTEMP(:,:) = XTT + ZWORK2D(:,:)*( ((PSNOWHEAT(:,:)/MAX(XSNOWDMIN,PSNOWDZ(:,:)))  &
                   + XLMTT*PSNOWRHO(:,:))/ZSCAP(:,:) )  
PSNOWLIQ(:,:)  = MAX(0.0,ZSNOWTEMP(:,:)-XTT)*ZSCAP(:,:)*PSNOWDZ(:,:)/(XLMTT*XRHOLW)  
ZSNOWTEMP(:,:) = MIN(XTT,ZSNOWTEMP(:,:))
!
!
! If all snow in uppermost layer evaporates/sublimates, re-distribute
! grid (below could be evoked for vanishingly thin snowpacks):
!
CALL SNOW3LEVAPGONE(PSNOWHEAT,PSNOWDZ,PSNOWRHO,ZSNOWTEMP,PSNOWLIQ,   &
                          PSNOWGRAN1,PSNOWGRAN2,HSNOWMETAMO )
!
!
!*      12.     Update surface albedo:
!               ----------------------
! Snow clear sky albedo:
!
 CALL SNOW3LALB(PSNOWALB,ZSPECTRALALBEDO,PSNOWRHO(:,1),PSNOWAGE(:,1),PPERMSNOWFRAC,PPS)
!
!
!*      13.     Update snow heat content:
!               -------------------------
! Update the heat content (variable stored each time step)
! using current snow temperature and liquid water content:
!
! First, make check to make sure heat content not too large
! (this can result due to signifigant heating of thin snowpacks):
!
DO JJ=1,INLVLS
   DO JI=1,INI
      ZLIQHEATXS(JI) = MAX(0.0,PSNOWLIQ(JI,JJ)*XRHOLW-PSNOWDZ(JI,JJ)*PSNOWRHO(JI,JJ))*XLMTT/PTSTEP  
      PSNOWLIQ(JI,JJ)= PSNOWLIQ(JI,JJ) - ZLIQHEATXS(JI)*PTSTEP/(XRHOLW*XLMTT)
      PSNOWLIQ(JI,JJ)= MAX(0.0, PSNOWLIQ(JI,JJ))
   ENDDO
ENDDO
!
PSNOWTEMP(:,:)    = ZSNOWTEMP(:,:)
!
ZSCAP(:,:)        = SNOW3LSCAP(PSNOWRHO)
!
PSNOWHEAT(:,:)    = PSNOWDZ(:,:)*( ZSCAP(:,:)*(PSNOWTEMP(:,:)-XTT)        &
                      - XLMTT*PSNOWRHO(:,:) ) + XLMTT*XRHOLW*PSNOWLIQ(:,:)  
!
!
!*      14.     Snow/Ground heat flux:
!               ----------------------
!
!Add radiation not absorbed by snow to soil/vegetation interface flux
!
PGRNDFLUX(:) = ZGRNDFLUXO(:)+ZRADXS(:)
!
!Comput soil/snow correction heat flux to prevent TG1 numerical oscillations
!Add any excess heat for melting and liquid water :
!
PGFLXCOR(:) = (ZGRNDFLUX(:)-ZGRNDFLUXO(:))+ZMELTXS(:)+ZLIQHEATXS(:)
!
!
!*      15.     Update snow mass and age:
!               -------------------------
!
PSNOWSWE(:,:) = PSNOWDZ(:,:)*PSNOWRHO(:,:)
!
WHERE (PSNOWSWE(:,:)>0)
  PSNOWAGE(:,:)=PSNOWAGE(:,:)+(PTSTEP/XDAY)
ELSEWHERE
  PSNOWAGE(:,:)= XUNDEF
ENDWHERE

DO JI=1,INI
     IF(PSNOWSWE(JI,1)>0. .AND. PSNOWSWE(JI,4)==0.) THEN
         write(*,*) 'Souci de profil SWE 1 VS 4'
     ENDIF
     IF(PSNOWSWE(JI,6)>0. .AND. PSNOWSWE(JI,4)==0.) THEN
         write(*,*) 'Souci de profil SWE 6 VS 4'
     ENDIF
ENDDO


!
!
!*      16.     Update surface specific humidity:
!               ---------------------------------
!
IF(OMEB)THEN
   PQS(:) = QSATI(PSNOWTEMP(:,1),PPS) ! purely diagnostic
ELSE
   PQS(:) = ZQSAT(:)
ENDIF
!
!
!*      17.     MEB related checks
!               ------------------
!
IF(OMEB)THEN

! Final adjustment: If using MEB, then all excess heat (above initial guess ground flux)
! is used herein to adjust the ground temperature since for the MEB case, boundary
! fluxes are imposed (the energy budget of the ground has already been computed).
! This correction is needed since snow-soil flux here is implicit, while guess
! used in surface energy soil budget was semi-implicit. This forces the flux
! seen by the ground to be *the same* as that leaving the snow (energy conservation).
! Also, add any excessing cooling from sublimation as snowpack becomes vanishingly thin.
! This is added back to total heat content of the snowpack (and distributed among
! all snow layers while conserving total heat content plus correction)

   ZWORK(:)               = 0.
   DO JJ=1,INLVLS
      DO JI=1,INI
         ZWORK(JI)        = ZWORK(JI) + PSNOWHEAT(JI,JJ)
      ENDDO
   ENDDO
   ZWORK2(:)              = MIN(0.0, ZWORK(:) + PGRNDFLUX(:) - ZGRNDFLUXI(:))
   PGFLXCOR(:)            = PGFLXCOR(:) + MAX(0., ZWORK2(:)) ! add any possible (rare!) excess to soil

   WHERE(ZWORK(:) > -1.E-10)
      ZWORK(:) = 1. ! i.e. no modifs to H profile
   ELSEWHERE
      ZWORK(:) = ZWORK2(:)/ZWORK(:)
   END WHERE

   DO JJ=1,INLVLS
      DO JI=1,INI
         PSNOWHEAT(JI,JJ) = PSNOWHEAT(JI,JJ)*ZWORK(JI)
      ENDDO
   ENDDO

! these are just updated diagnostics at this point:

   ZWORK2D(:,:)   = MIN(1.0, PSNOWDZ(:,:)/XSNOWDMIN)/MAX(XSNOWDMIN,PSNOWDZ(:,:))
   PSNOWTEMP(:,:) = XTT + ZWORK2D(:,:)*( (PSNOWHEAT(:,:) + XLMTT*PSNOWSWE(:,:))/ZSCAP(:,:) )
   PSNOWLIQ(:,:)  = MAX(0.0,PSNOWTEMP(:,:)-XTT)*ZSCAP(:,:)*PSNOWDZ(:,:)/(XLMTT*XRHOLW)  
   PSNOWTEMP(:,:) = MIN(XTT,PSNOWTEMP(:,:))

ENDIF
!
!
!*      18.     Energy Budget Diagnostics:
!               --------------------------
!
! NOTE: To check enthalpy conservation, the error in W m-2 is defined HERE as
!
! error = (SUM(PSNOWHEAT(:,:),2)-SUM(ZSNOWHEAT0(:,:),2))/PTSTEP         &
!          - PSNOWHMASS(:)/PTSTEP - (PGFLUXSNOW(:) - PGRNDFLUX(:))
!
! where ZSNOWHEAT0 is the value of PSNOWHEAT that enters this routine before any adjustments/processes.
! Errors should be VERY small ( ~0) at each time step.
! The above is "snow relative"...the actual tile or grid box error is multiplied by PPSNES.
!
! Snow heat storage change over the time step (W m-2):

PDELHEATN(:) = 0.0
DO JJ=1,INLVLS
   DO JI=1,INI
      PDELHEATN(JI) = PDELHEATN(JI) + (PSNOWHEAT(JI,JJ)-ZSNOWHEAT0(JI,JJ))
   ENDDO
END DO
PDELHEATN(:)     =  PDELHEATN(:)                   /PTSTEP
PDELHEATN_SFC(:) = (PSNOWHEAT(:,1)-ZSNOWHEAT0(:,1))/PTSTEP

!
! The pseudo-heating (W m-2) is derrived as a diagnostic from the surface energy budget
! here...NOTE that the total snow energy budget is explicitly computed, and since
! it balances, then simply compute this term as a residue (which ensures sfc energy balance):
!
PSNOWSFCH(:)     = PDELHEATN_SFC(:) - (PSWNETSNOWS(:) +PLWNETSNOW(:) - PHSNOW(:) -PLESES(:)-PLELES(:))    &
                      + PSNOWFLUX(:) - PSNOWHMASS(:)/PTSTEP
!
!
!*      19.     Specific Surface Area (SSA) Diagnostics:
!               ----------------------------------------
!
!
PSSA(:,:) = 6./(XRHOLI*PSNOWGRAN1(:,:))
!
!
CONTAINS
!
!
!
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOW3LFALL(PTSTEP,PSR,PTA,PVMOD,PSNOW,PSNOWRHO,PSNOWDZ,        &
                      PSNOWHEAT,PSNOWHMASS,PSNOWAGE,PPERMSNOWFRAC,        &  
                      PSNOWGRAN1,PSNOWGRAN2,HSNOWMETAMO,PUREF,PZ0EFF,     &
                      OSNOWDRIFT  )
!
!!    PURPOSE
!!    -------
!     Calculate changes to snowpack resulting from snowfall.
!     Update mass and heat content of uppermost layer.
!
!
USE MODD_CSTS_SNOWES,     ONLY : XLMTT, XTT, XCI
USE MODD_SNOWES_PAR, ONLY : XRHOSMIN_ES, XSNOWDMIN, &
                          XSNOWFALL_A_SN,         &
                          XSNOWFALL_B_SN,         &
                          XSNOWFALL_C_SN
!                   
USE MODE_SNOWES
USE MODD_SNOW_METAMO
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PSR, PTA, PVMOD, PPERMSNOWFRAC
!
REAL, DIMENSION(:),INTENT(IN)       :: PZ0EFF,PUREF
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PSNOW
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWRHO, PSNOWDZ, PSNOWHEAT, PSNOWAGE
!
REAL, DIMENSION(:), INTENT(OUT)     :: PSNOWHMASS
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWGRAN1, PSNOWGRAN2
!
LOGICAL,INTENT(IN) :: OSNOWDRIFT ! if snowdrift then grain types are not modified by wind
!
CHARACTER(3), INTENT(IN)            :: HSNOWMETAMO ! metamorphism scheme
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ, JI
!
INTEGER                             :: INI
INTEGER                             :: INLVLS
!
REAL, DIMENSION(SIZE(PTA))          :: ZSNOWFALL, ZRHOSNEW,        &
                                       ZSNOW, ZSNOWTEMP,           &
                                       ZSNOWFALL_DELTA, ZSCAP,     &
                                       ZAGENEW,ZSNOWGRAN1NEW,      &
                                       ZSNOWGRAN2NEW
!
REAL :: ZZ0EFF,ZCOEF
!                               
! Coefficient to adjust wind speed at the height used in the parameterization
! for:
!   -  density of new snow
!   -  sphericity and dendricity of new snow
! Default values : 10 m for new snow (Pahaut, 1976) and 5 m for characteristics 
! of snow grains (Guyomarc'h et Merindol, 1998)
REAL, PARAMETER                    :: PPHREF_WIND_RHO   = 10.
REAL, PARAMETER                    :: PPHREF_WIND_GRAIN = 5.
REAL, PARAMETER                    :: PPHREF_WIND_MIN = MIN(PPHREF_WIND_RHO,PPHREF_WIND_GRAIN)*0.5
REAL, DIMENSION(SIZE(PTA))         :: ZWIND_RHO
REAL, DIMENSION(SIZE(PTA))         :: ZWIND_GRAIN
!
!-------------------------------------------------------------------------------
!
! 0. Initialize:
! ------------------
!
!
INI             = SIZE(PSNOWDZ(:,:),1)
INLVLS          = SIZE(PSNOWDZ(:,:),2)
!
ZRHOSNEW(:)     = XRHOSMIN_ES
ZAGENEW (:)     = 0.0
ZSNOWFALL(:)    = 0.0
ZSCAP(:)        = 0.0
ZSNOW(:)        = PSNOW(:)
!
PSNOWHMASS(:)     = 0.0
ZSNOWGRAN1NEW(:)  = 0.0
ZSNOWGRAN2NEW(:)  = 0.0
!
! 1. Incorporate snowfall into snowpack:
! --------------------------------------
!
!
! Heat content of newly fallen snow (J/m2):
! NOTE for now we assume the snowfall has
! the temperature of the snow surface upon reaching the snow.
! This is done as opposed to using the air temperature since
! this flux is quite small and has little to no impact
! on the time scales of interest. If we use the above assumption
! then, then the snowfall advective heat flux is zero.
!
ZSNOWTEMP(:)  = XTT
ZSCAP    (:)  = SNOW3LSCAP(PSNOWRHO(:,1))
!
WHERE (PSR(:) > 0.0 .AND. PSNOWDZ(:,1)>0.)
  ZSNOWTEMP(:)  = XTT + (PSNOWHEAT(:,1) +                              &
                    XLMTT*PSNOWRHO(:,1)*PSNOWDZ(:,1))/                   &
                    (ZSCAP(:)*MAX(XSNOWDMIN/INLVLS,PSNOWDZ(:,1)))  
  ZSNOWTEMP(:)  = MIN(XTT, ZSNOWTEMP(:))
END WHERE
!
DO JJ = 1,SIZE(PSNOW(:))
  !
  IF ( PSR(JJ)>0.0 ) THEN
!
    ! Wind speeds at reference heights for new snow density and charactristics of
    ! grains of new snow 
    ! Computed from PVMOD at PUREF (m) assuming a log profile in the SBL 
    ! and a roughness length equal to PZ0EFF
     ZZ0EFF=MIN(PZ0EFF(JJ),PUREF(JJ)*0.5,PPHREF_WIND_MIN)
     ZWIND_RHO(JJ)   = PVMOD(JJ)*LOG(PPHREF_WIND_RHO/ZZ0EFF)/          &
                               LOG(PUREF(JJ)/ZZ0EFF)
     ZWIND_GRAIN(JJ) = PVMOD(JJ)*LOG(PPHREF_WIND_GRAIN/ZZ0EFF)/        &
                               LOG(PUREF(JJ)/ZZ0EFF)

    PSNOWHMASS(JJ) = PSR(JJ)*(XCI*(ZSNOWTEMP(JJ)-XTT)-XLMTT)*PTSTEP
!
!    Snowfall density: Following CROCUS (Pahaut 1976)
!
     ZRHOSNEW(JJ)   = MAX(XRHOSMIN_ES, XSNOWFALL_A_SN + XSNOWFALL_B_SN*(PTA(JJ)-XTT)+         &
                     XSNOWFALL_C_SN*MIN( PVMOD(JJ), SQRT(ZWIND_RHO(JJ) )))  
!
!
! Fresh snowfall changes the snowpack age,
! decreasing in uppermost snow layer (mass weighted average):
!
     PSNOWAGE(JJ,1) = (PSNOWAGE(JJ,1)*PSNOWDZ(JJ,1)*PSNOWRHO(JJ,1)+ZAGENEW(JJ)*PSR(JJ)*PTSTEP) / &
                   (PSNOWDZ(JJ,1)*PSNOWRHO(JJ,1)+PSR(JJ)*PTSTEP)

! Initialise metamorphism variables (Dopt and sphericity)
! of fresh snow depending on wind and drift:

     IF ( OSNOWDRIFT ) THEN
        ZSNOWGRAN1NEW(JJ) = XVDIAM6
        ZSNOWGRAN2NEW(JJ) = XNSPH3/XGRAN
     ELSE
        ZSNOWGRAN2NEW(JJ) = MIN( MAX( XNSPH1*ZWIND_GRAIN(JJ)+XNSPH2, XNSPH3 ), XNSPH4 ) / XGRAN
        ZCOEF = MAX( MIN( XNDEN1*ZWIND_GRAIN(JJ)-XNDEN2, XNDEN3 ), -XGRAN ) / ( -XGRAN )
        ZSNOWGRAN1NEW(JJ) = XVDIAM6 * &
                        ( ZCOEF + ( 1.- ZCOEF ) * &
                                  ( 3.*ZSNOWGRAN2NEW(JJ) + 4.*(1.-ZSNOWGRAN2NEW(JJ)) ) )
     END IF

     PSNOWGRAN1(JJ,1) = (PSNOWGRAN1(JJ,1)*PSNOWDZ(JJ,1)*PSNOWRHO(JJ,1)+ZSNOWGRAN1NEW(JJ)*PSR(JJ)*PTSTEP) / &
                   (PSNOWDZ(JJ,1)*PSNOWRHO(JJ,1)+PSR(JJ)*PTSTEP)
     PSNOWGRAN2(JJ,1) = (PSNOWGRAN2(JJ,1)*PSNOWDZ(JJ,1)*PSNOWRHO(JJ,1)+ZSNOWGRAN2NEW(JJ)*PSR(JJ)*PTSTEP) / &
                   (PSNOWDZ(JJ,1)*PSNOWRHO(JJ,1)+PSR(JJ)*PTSTEP)
!
! Augment total pack depth:
!
     ZSNOWFALL(JJ)  = PSR(JJ)*PTSTEP/ZRHOSNEW(JJ)    ! snowfall thickness (m)
!
     PSNOW(JJ)      = PSNOW(JJ) + ZSNOWFALL(JJ)
!
! Fresh snowfall changes the snowpack
! density, increases the total liquid water
! equivalent: in uppermost snow layer:
!
     PSNOWRHO(JJ,1) = (PSNOWDZ(JJ,1)*PSNOWRHO(JJ,1) + ZSNOWFALL(JJ)*ZRHOSNEW(JJ))/     &
                   (PSNOWDZ(JJ,1)+ZSNOWFALL(JJ))  
!
     PSNOWDZ(JJ,1)  = PSNOWDZ(JJ,1) + ZSNOWFALL(JJ)
!
! Add energy of snowfall to snowpack:
! Update heat content (J/m2) (therefore the snow temperature
! and liquid content):
!
     PSNOWHEAT(JJ,1)  = PSNOWHEAT(JJ,1) + PSNOWHMASS(JJ)
!
!
  END IF

END DO
!
!
! 2. Case of new snowfall on a previously snow-free surface:
! ----------------------------------------------------------
!
! When snow first falls on a surface devoid of snow,
! redistribute the snow mass throughout the 3 layers:
! (temperature already set in the calling routine
! for this case)
!
ZSNOWFALL_DELTA(:)    = 0.0
WHERE(ZSNOW(:) == 0.0 .AND. PSR(:) > 0.0)
   ZSNOWFALL_DELTA(:) = 1.0
END WHERE
!
DO JJ=1,INLVLS
   DO JI=1,INI
!
      PSNOWDZ(JI,JJ)   = ZSNOWFALL_DELTA(JI)*(ZSNOWFALL(JI) /INLVLS) + &
                        (1.0-ZSNOWFALL_DELTA(JI))*PSNOWDZ(JI,JJ)  
!
      PSNOWHEAT(JI,JJ) = ZSNOWFALL_DELTA(JI)*(PSNOWHMASS(JI)/INLVLS) + &
                       (1.0-ZSNOWFALL_DELTA(JI))*PSNOWHEAT(JI,JJ)  
!
      PSNOWRHO(JI,JJ)  = ZSNOWFALL_DELTA(JI)*ZRHOSNEW(JI)            + &
                       (1.0-ZSNOWFALL_DELTA(JI))*PSNOWRHO(JI,JJ)
!
      PSNOWAGE(JI,JJ)  = ZSNOWFALL_DELTA(JI)*(ZAGENEW(JI))    + &
                       (1.0-ZSNOWFALL_DELTA(JI))*PSNOWAGE(JI,JJ)  
!
      PSNOWGRAN1(JI,JJ)= ZSNOWFALL_DELTA(JI)*(ZSNOWGRAN1NEW(JI))    + &
                       (1.0-ZSNOWFALL_DELTA(JI))*PSNOWGRAN1(JI,JJ)
!
      PSNOWGRAN2(JI,JJ)= ZSNOWFALL_DELTA(JI)*(ZSNOWGRAN2NEW(JI))    + &
                       (1.0-ZSNOWFALL_DELTA(JI))*PSNOWGRAN2(JI,JJ)
!      
   ENDDO
ENDDO
!
!
!
END SUBROUTINE SNOW3LFALL
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOW3LCOMPACTN(PTSTEP,PSNOWDZMIN,PSNOWRHO,PSNOWDZ,PSNOWTEMP,PSNOW,PSNOWLIQ)  
!
!!    PURPOSE
!!    -------
!     Snow compaction due to overburden and settling.
!     Mass is unchanged: layer thickness is reduced
!     in proportion to density increases. Method
!     of Brun et al (1989) and Vionnet et al. (2012)
!
!     
!
USE MODD_CSTS_SNOWES,     ONLY : XTT, XG
USE MODD_SNOWES_PAR, ONLY : XRHOSMAX_ES,XUNDEF,         &
                          XVVISC1,XVVISC3,XVVISC4,    &
                          XVVISC5,XVVISC6,XVRO11
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
REAL, INTENT(IN)                    :: PSNOWDZMIN
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWTEMP, PSNOWLIQ
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWRHO, PSNOWDZ
!
REAL, DIMENSION(:), INTENT(OUT)     :: PSNOW
!
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ, JI
!
INTEGER                             :: INI
INTEGER                             :: INLVLS
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHO2, ZVISCOCITY, ZF1, &
                                                      ZTEMP, ZSMASS, ZSNOWDZ,     &
                                                      ZWSNOWDZ, ZWHOLDMAX
!
!
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
!
INI             = SIZE(PSNOWDZ(:,:),1)
INLVLS          = SIZE(PSNOWDZ(:,:),2)
!
ZSNOWRHO2 (:,:) = PSNOWRHO(:,:)
ZSNOWDZ   (:,:) = MAX(PSNOWDZMIN,PSNOWDZ(:,:))
ZVISCOCITY(:,:) = 0.0
ZTEMP     (:,:) = 0.0
!
! 1. Cumulative snow mass (kg/m2):
! --------------------------------
!
ZSMASS(:,:) = 0.0
DO JJ=2,INLVLS
   DO JI=1,INI
      ZSMASS(JI,JJ) = ZSMASS(JI,JJ-1) + PSNOWDZ(JI,JJ-1)*PSNOWRHO(JI,JJ-1)
   ENDDO
ENDDO
! overburden of half the mass of the uppermost layer applied to itself
ZSMASS(:,1) = 0.5 * PSNOWDZ(:,1) * PSNOWRHO(:,1)
!
! 2. Compaction
! -------------
!
!Liquid water effect
!
!ZWHOLDMAX(:,:) = SNOW3LHOLD(PSNOWRHO,PSNOWDZ)
DO JJ=1,INLVLS
   DO JI=1,INI
    IF ( CSNOWHOLD == 'B02' ) THEN
       ZWHOLDMAX(JI,JJ) = SNOW3LHOLD(PSNOWRHO(JI,JJ),PSNOWDZ(JI,JJ))
    ELSE IF ( CSNOWHOLD == 'B92' ) THEN 
       ZWHOLDMAX(JI,JJ) = SNOWCROHOLD(PSNOWRHO(JI,JJ),PSNOWLIQ(JI,JJ),PSNOWDZ(JI,JJ))
    ELSE IF ( CSNOWHOLD == 'SPK' ) THEN
       ZWHOLDMAX(JI,JJ) = SNOWSPKHOLD(PSNOWRHO(JI,JJ),PSNOWLIQ(JI,JJ),PSNOWDZ(JI,JJ))
   ENDIF
ENDDO
ENDDO

!ZF1(:,:) = 1.0/(XVVISC5+10.*MIN(1.0,PSNOWLIQ(:,:)/ZWHOLDMAX(:,:)))
DO JJ=1,INLVLS
   DO JI=1,INI
   ZF1(JI,JJ) = 1.0/(XVVISC5+10.*MIN(1.0,PSNOWLIQ(JI,JJ)/ZWHOLDMAX(JI,JJ)))
 ! Same parameter as in Crocus
 !  ZF1(JI,JJ) = 1.0/( XVVISC5 + XVVISC6*PSNOWLIQ(JI,JJ)/PSNOWDZ(JI,JJ) ) 
ENDDO
ENDDO


!
!Snow viscocity, density and grid thicknesses
!
DO JJ=1,INLVLS
   DO JI=1,INI
!   
      IF(PSNOWRHO(JI,JJ) < XRHOSMAX_ES)THEN
!   
!       temperature dependence limited to 5K: Schleef et al. (2014)
        ZTEMP     (JI,JJ) = XVVISC4*MIN(5.0,ABS(XTT-PSNOWTEMP(JI,JJ)))
!        
!       Calculate snow viscocity: Brun et al. (1989), Vionnet et al. (2012)
        ZVISCOCITY(JI,JJ) = XVVISC1*ZF1(JI,JJ)*EXP(XVVISC3*PSNOWRHO(JI,JJ)+ZTEMP(JI,JJ))*PSNOWRHO(JI,JJ)/XVRO11
!
!       Calculate snow density:
        ZSNOWRHO2(JI,JJ) = PSNOWRHO(JI,JJ) + PSNOWRHO(JI,JJ)*PTSTEP &
                         * ( (XG*ZSMASS(JI,JJ)/ZVISCOCITY(JI,JJ)) )
!         
!       Conserve mass by decreasing grid thicknesses in response to density increases
        PSNOWDZ(JI,JJ) = PSNOWDZ(JI,JJ)*(PSNOWRHO(JI,JJ)/ZSNOWRHO2(JI,JJ))  
!        
      ENDIF
!
   ENDDO
ENDDO
!
! 3. Update total snow depth and density profile:
! -----------------------------------------------
!
! Compaction/augmentation of total snowpack depth
!
PSNOW(:) = 0.
DO JJ=1,INLVLS
   DO JI=1,INI
      PSNOW(JI) = PSNOW(JI) + PSNOWDZ(JI,JJ)
   ENDDO
ENDDO
!
! Update density (kg m-3):
!
PSNOWRHO(:,:)  = ZSNOWRHO2(:,:)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LCOMPACTN
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOW3LDRIFT(PTSTEP,PVMOD,PTA,PQA,PPS,PRHOA,PSNOWRHO,&
                       PSNOWDZ,PSNOW,OSNOWDRIFT_SUBLIM,PSNDRIFT)
!
!
USE MODD_SNOWES_PAR, ONLY : XVTIME, XVROMAX, XVROMIN, XVMOB1, &
                          XVDRIFT1, XVDRIFT2, XVDRIFT3,     &
                          XCOEF_FF, XCOEF_EFFECT, XQS_REF,   &
                          XUNDEF
!
USE MODE_THERMOS_SNOW
!
!
!!    PURPOSE
!!    -------
!     Snow compaction  and metamorphism due to drift
!     Mass is unchanged: layer thickness is reduced
!     in proportion to density increases. Method inspired from
!     Brun et al. (1997) and Guyomarch
!     
!     - computes a mobility index of each snow layer from its  density
!     - computes a drift index of each layer from its mobility and wind speed
!     - computes a transport index 
!     - increases density in case of transport
!
!     HISTORY:
!     Basic parameterization from Crocus/ARPEGE Coupling (1997) 
!     Adaptation from Crocus SNOWDRIFT subroutine
!
!!    PURPOSE
!!    -------
!     Snow compaction due to overburden and settling.
!     Mass is unchanged: layer thickness is reduced
!     in proportion to density increases. 
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PVMOD
REAL, DIMENSION(:), INTENT(IN)      :: PTA
REAL, DIMENSION(:), INTENT(IN)      :: PQA
REAL, DIMENSION(:), INTENT(IN)      :: PPS
REAL, DIMENSION(:), INTENT(IN)      :: PRHOA
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWRHO, PSNOWDZ
!
REAL, DIMENSION(:), INTENT(OUT)     :: PSNOW
!
LOGICAL,            INTENT(IN)      :: OSNOWDRIFT_SUBLIM
REAL, DIMENSION(:), INTENT(OUT)     :: PSNDRIFT !blowing snow sublimation (kg/m2/s)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHO2
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZRMOB
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZRDRIFT
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZRT
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZDRO
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZQS_EFFECT
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZDRIFT_EFFECT
REAL, DIMENSION(SIZE(PSNOWRHO,1)                 ) :: ZPROFEQU
REAL, DIMENSION(SIZE(PSNOWRHO,1)                 ) :: ZWIND
REAL, DIMENSION(SIZE(PSNOWRHO,1)                 ) :: ZQSATI
REAL, DIMENSION(SIZE(PSNOWRHO,1)                 ) :: ZVT
REAL, DIMENSION(SIZE(PSNOWRHO,1)                 ) :: ZQS
REAL, DIMENSION(SIZE(PSNOWRHO,1)                 ) :: ZW
REAL, DIMENSION(SIZE(PSNOWRHO,1)                 ) :: ZT
REAL, DIMENSION(SIZE(PSNOWRHO,1)                 ) :: ZSNOWDZ1
!
LOGICAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: GDRIFT
!
INTEGER                             :: JJ, JI
!
INTEGER                             :: INI
INTEGER                             :: INLVLS
!
!
!-------------------------------------------------------------------------------
!
!
! 0. Initialization:
! ------------------
!
INI    = SIZE(PSNOWDZ(:,:),1)
INLVLS = SIZE(PSNOWDZ(:,:),2)
!
ZSNOWRHO2    (:,:) = PSNOWRHO(:,:)
ZRDRIFT      (:,:) = XUNDEF
ZRT          (:,:) = XUNDEF
ZDRO         (:,:) = XUNDEF
ZQS_EFFECT   (:,:) = 0.0
ZDRIFT_EFFECT(:,:) = 0.0
GDRIFT       (:,:) = .FALSE.
!
ZSNOWDZ1(:) = PSNOWDZ(:,1)
!
! 1. Initialazation of drift and induced settling
! -----------------------------------------------
!
ZWIND    (:) = XCOEF_FF * PVMOD(:)
ZPROFEQU (:) = 0.0
!
DO JJ=1,INLVLS
   DO JI=1,INI
!      
!     mobility index computation of a layer as a function of its density 
      ZRMOB(JI,JJ)= 1.25-1.25E-3*(MAX(PSNOWRHO(JI,JJ),XVROMIN)-XVROMIN) / XVMOB1
!
!     computation of the drift index inclunding the decay by overburden snow 
      ZRDRIFT(JI,JJ) =  ZRMOB(JI,JJ)-(XVDRIFT1*EXP(-XVDRIFT2*ZWIND(JI))-1.0)
!
   ENDDO
ENDDO
!
!Compaction from the surface to layers beneath until 
!a snow layer having negative drift index
GDRIFT(:,:) = (ZRDRIFT(:,:)>0.0)
DO JI=1,INI
   DO JJ=1,INLVLS
      IF(.NOT.GDRIFT(JI,JJ))THEN
        GDRIFT(JI,JJ:INLVLS)=.FALSE.
        EXIT
      ENDIF
   ENDDO
ENDDO

! 2. Specific case for blowing snow sublimation
! ---------------------------------------------
!
IF(OSNOWDRIFT_SUBLIM)THEN
!
  ZQSATI(:)= QSATI(PTA(:),PPS(:))
!
! computation of wind speed threshold QSATI and RH withe respect to ice
!
  ZW (:)=MAX(-0.99,ZRMOB(:,1))
  ZVT(:)=LOG(XVDRIFT1/(1.0+ZW(:)))/XVDRIFT2
!
  WHERE(ZWIND(:)>0.0)
    ZW(:)=LOG(ZWIND(:)/ZVT(:))
    ZW(:)=EXP(3.6*ZW(:))
  ELSEWHERE
    ZW(:)=0.0
  ENDWHERE
!
! computation of sublimation rate according to Gordon's PhD
!
  ZT(:)=XTT/PTA(:)
  ZT(:)=0.0018*(ZT(:)**4)
!
  ZQS(:)=ZT(:)*ZVT(:)*PRHOA(:)*ZQSATI(:)*(1.-PQA(:)/ZQSATI(:))*ZW(:)
!
! surface depth decrease in case of blowing snow sublimation
!
  PSNOWDZ(:,1)=MAX(0.5*PSNOWDZ(:,1),PSNOWDZ(:,1)-MAX(0.,ZQS(:))*PTSTEP/(XCOEF_FF*PSNOWRHO(:,1)))
!
  PSNDRIFT(:) = (ZSNOWDZ1(:)-PSNOWDZ(:,1))*PSNOWRHO(:,1)/PTSTEP
!
  ZQS_EFFECT(:,1)=MIN(3.,MAX(0.,ZQS(:))/XQS_REF)
!
ENDIF
!
! 3. Computation of drift and induced settling
! --------------------------------------------
!
DO JJ=1,INLVLS
   DO JI=1,INI

!     update the decay coeff by half the current layer
      ZPROFEQU(JI) = ZPROFEQU(JI)  + 0.5 * PSNOWDZ(JI,JJ) * 0.1 * (XVDRIFT3-ZRDRIFT(JI,JJ))
!
      IF(GDRIFT(JI,JJ).AND.PSNOWRHO(JI,JJ)<XVROMAX)THEN
!      
!       computation of the drift index inclunding the decay by overburden snow 
        ZRT(JI,JJ) = MAX(0.0,ZRDRIFT(JI,JJ)*EXP(-ZPROFEQU(JI)*100.0))
!     
        ZDRIFT_EFFECT(JI,JJ) = (ZQS_EFFECT(JI,JJ)+XCOEF_EFFECT)*ZRT(JI,JJ)/(XVTIME*XCOEF_FF)
!
!       settling by wind transport only in case of not too dense snow
        ZDRO(JI,JJ) = (XVROMAX - PSNOWRHO(JI,JJ)) * ZDRIFT_EFFECT(JI,JJ) * PTSTEP
!          
!       Calculate new snow density:
        ZSNOWRHO2(JI,JJ) = MIN(XVROMAX,PSNOWRHO(JI,JJ)+ZDRO(JI,JJ))

!       Conserve mass by decreasing grid thicknesses in response to density increases
        PSNOWDZ(JI,JJ) = PSNOWDZ(JI,JJ)*(PSNOWRHO(JI,JJ)/ZSNOWRHO2(JI,JJ))    
!
      ENDIF
!       
!     update the decay coeff by half the current layer
      ZPROFEQU(JI) = ZPROFEQU(JI)  + 0.5 * PSNOWDZ(JI,JJ) * 0.1 * (XVDRIFT3-ZRDRIFT(JI,JJ))
!      
   ENDDO
ENDDO
!
! 4. Update total snow depth and density profile:
! -----------------------------------------------
!
! Compaction/augmentation of total snowpack depth
!
PSNOW(:) = 0.
DO JJ=1,INLVLS
   DO JI=1,INI
      PSNOW(JI) = PSNOW(JI) + PSNOWDZ(JI,JJ)
   ENDDO
ENDDO
!
! Update density (kg m-3):
!
PSNOWRHO(:,:)  = ZSNOWRHO2(:,:)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LDRIFT
!####################################################################
!####################################################################
!####################################################################
        SUBROUTINE SNOW3LTRANSF(PSNOW,PSNOWDZ,PSNOWDZN,    &
                                PSNOWRHO,PSNOWHEAT,PSNOWAGE, &
                                PSNOWGRAN1,PSNOWGRAN2 )  
!
!!    PURPOSE
!!    -------
!     Snow mass,heat and characteristics redistibution in case of
!     grid resizing. Total mass and heat content of the overall snowpack
!     unchanged/conserved within this routine.
!     Same method as in Crocus
!
USE MODD_SNOWES_PAR, ONLY : XSNOWCRITD
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:  ), INTENT(IN)    :: PSNOW
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWDZN  
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWHEAT
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWRHO
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWDZ
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWAGE
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWGRAN1
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWGRAN2
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JI, JL, JLO
!
INTEGER                             :: INI
INTEGER                             :: INLVLS
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHON
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWHEATN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWAGEN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWGRAN1N
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWGRAN2N
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWZTOP_NEW
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWZBOT_NEW                                       
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHOO
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWHEATO
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWAGEO
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWGRAN1O
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWGRAN2O
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWDZO
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWZTOP_OLD
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWZBOT_OLD  
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWHEAN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWAGN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWGRA1N
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWGRA2N
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZMASTOTN
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZMASSDZO
!
REAL, DIMENSION(SIZE(PSNOW)) :: ZPSNOW_OLD, ZPSNOW_NEW
REAL, DIMENSION(SIZE(PSNOW)) :: ZSUMHEAT, ZSUMSWE, ZSUMAGE, ZSNOWMIX_DELTA
REAL, DIMENSION(SIZE(PSNOW)) :: ZSUMGRAN1, ZSUMGRAN2
!
REAL :: ZPROPOR
!
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
!
!
INI        = SIZE(PSNOWRHO,1)
INLVLS     = SIZE(PSNOWRHO,2)
!
ZPSNOW_NEW(:) = 0.0
ZPSNOW_OLD(:) = PSNOW(:)
!
DO JL=1,INLVLS
   DO JI=1,INI
      ZPSNOW_NEW(JI)=ZPSNOW_NEW(JI)+PSNOWDZN(JI,JL)
   ENDDO
ENDDO
!
! initialization of variables describing the initial snowpack 
!
ZSNOWDZO  (:,:) = PSNOWDZ  (:,:)
ZSNOWRHOO (:,:) = PSNOWRHO (:,:)
ZSNOWHEATO(:,:) = PSNOWHEAT(:,:)
ZSNOWAGEO (:,:) = PSNOWAGE (:,:)
ZSNOWGRAN1O(:,:) = PSNOWGRAN1 (:,:)
ZSNOWGRAN2O(:,:) = PSNOWGRAN2 (:,:)
ZMASSDZO  (:,:) = XUNDEF
!
! 1. Calculate vertical grid limits (m):
! --------------------------------------
!
ZSNOWZTOP_OLD(:,1) = ZPSNOW_OLD(:)
ZSNOWZTOP_NEW(:,1) = ZPSNOW_NEW(:)
ZSNOWZBOT_OLD(:,1) = ZSNOWZTOP_OLD(:,1)-ZSNOWDZO(:,1)
ZSNOWZBOT_NEW(:,1) = ZSNOWZTOP_NEW(:,1)-PSNOWDZN(:,1)
!
DO JL=2,INLVLS
   DO JI=1,INI
      ZSNOWZTOP_OLD(JI,JL) = ZSNOWZBOT_OLD(JI,JL-1)
      ZSNOWZTOP_NEW(JI,JL) = ZSNOWZBOT_NEW(JI,JL-1)
      ZSNOWZBOT_OLD(JI,JL) = ZSNOWZTOP_OLD(JI,JL  )-ZSNOWDZO(JI,JL)
      ZSNOWZBOT_NEW(JI,JL) = ZSNOWZTOP_NEW(JI,JL  )-PSNOWDZN(JI,JL)
   ENDDO
ENDDO
ZSNOWZBOT_OLD(:,INLVLS)=0.0
ZSNOWZBOT_NEW(:,INLVLS)=0.0
!
! 3. Calculate mass, heat, charcateristics mixing due to vertical grid resizing:
! --------------------------------------------------------------------
!
! loop over the new snow layers
! Sum or avergage of the constituting quantities of the old snow layers
! which are totally or partially inserted in the new snow layer
! For snow age, mass weighted average is used.
!
ZSNOWHEAN(:,:)=0.0
ZMASTOTN (:,:)=0.0
ZSNOWAGN (:,:)=0.0
ZSNOWGRA1N (:,:)=0.0
ZSNOWGRA2N (:,:)=0.0
!
DO JL=1,INLVLS
   DO JLO=1, INLVLS   
      DO JI=1,INI
        IF((ZSNOWZTOP_OLD(JI,JLO)>ZSNOWZBOT_NEW(JI,JL)).AND.(ZSNOWZBOT_OLD(JI,JLO)<ZSNOWZTOP_NEW(JI,JL)))THEN
!                
          ZPROPOR = (MIN(ZSNOWZTOP_OLD(JI,JLO), ZSNOWZTOP_NEW(JI,JL)) &
                  -  MAX(ZSNOWZBOT_OLD(JI,JLO), ZSNOWZBOT_NEW(JI,JL)))&
                  / ZSNOWDZO(JI,JLO) 
!
          ZMASSDZO (JI,JLO)=ZSNOWRHOO(JI,JLO)*ZSNOWDZO(JI,JLO)*ZPROPOR
!
          ZMASTOTN (JI,JL)=ZMASTOTN (JI,JL)+ZMASSDZO  (JI,JLO)
!
          ZSNOWAGN (JI,JL)=ZSNOWAGN (JI,JL)+ZSNOWAGEO (JI,JLO)*ZMASSDZO(JI,JLO)
!
          ZSNOWHEAN(JI,JL)=ZSNOWHEAN(JI,JL)+ZSNOWHEATO(JI,JLO)*ZPROPOR

          ZSNOWGRA1N(JI,JL)=ZSNOWGRA1N(JI,JL)+ZSNOWGRAN1O(JI,JLO)*ZMASSDZO(JI,JLO)
!
          ZSNOWGRA2N(JI,JL)=ZSNOWGRA2N(JI,JL)+ZSNOWGRAN2O(JI,JLO)*ZMASSDZO(JI,JLO)
        ENDIF
      ENDDO 
    ENDDO 
ENDDO  
!
!
! the new layer inherits from the weighted average properties of the old ones
! heat and mass
!
ZSNOWHEATN(:,:)= ZSNOWHEAN(:,:)
ZSNOWAGEN (:,:)= ZSNOWAGN (:,:)/ZMASTOTN(:,:)
ZSNOWRHON (:,:)= ZMASTOTN (:,:)/PSNOWDZN(:,:)
ZSNOWGRAN1N(:,:)= ZSNOWGRA1N(:,:)/ZMASTOTN(:,:)
ZSNOWGRAN2N(:,:)= ZSNOWGRA2N(:,:)/ZMASTOTN(:,:)
!
! 4. Vanishing or very thin snowpack check:
! -----------------------------------------
!
! NOTE: ONLY for very shallow snowpacks, mix properties (homogeneous):
! this avoids problems related to heat and mass exchange for
! thin layers during heavy snowfall or signifigant melt: one
! new/old layer can exceed the thickness of several old/new layers.
! Therefore, mix (conservative):
!
ZSUMHEAT(:)       = 0.0
ZSUMSWE(:)        = 0.0
ZSUMAGE(:)        = 0.0
ZSUMGRAN1(:)      = 0.0
ZSUMGRAN2(:)      = 0.0
ZSNOWMIX_DELTA(:) = 0.0
!
DO JL=1,INLVLS
   DO JI=1,INI
      IF(PSNOW(JI) < XSNOWCRITD)THEN
         ZSUMHEAT      (JI) = ZSUMHEAT(JI) + PSNOWHEAT(JI,JL)
         ZSUMSWE       (JI) = ZSUMSWE (JI) + PSNOWRHO (JI,JL)*PSNOWDZ(JI,JL)
         ZSUMAGE       (JI) = ZSUMAGE (JI) + PSNOWAGE (JI,JL)
         ZSUMGRAN1     (JI) = ZSUMGRAN1 (JI) + PSNOWGRAN1(JI,JL)
         ZSUMGRAN2     (JI) = ZSUMGRAN2 (JI) + PSNOWGRAN2(JI,JL)
         ZSNOWMIX_DELTA(JI) = 1.0
      ENDIF
   ENDDO
ENDDO
!
! Heat and mass are evenly distributed vertically:
! heat and mass (density and thickness) are constant
! in profile:
!
DO JL=1,INLVLS
   DO JI=1,INI
!
      ZSNOWHEATN(JI,JL) = ZSNOWMIX_DELTA(JI)*(ZSUMHEAT(JI)/INLVLS)  + &
                         (1.0-ZSNOWMIX_DELTA(JI))*ZSNOWHEATN(JI,JL)  
!
      PSNOWDZN(JI,JL)   = ZSNOWMIX_DELTA(JI)*(PSNOW(JI)/INLVLS)     + &
                        (1.0-ZSNOWMIX_DELTA(JI))*PSNOWDZN(JI,JL)  
!
      ZSNOWRHON(JI,JL)  = ZSNOWMIX_DELTA(JI)*(ZSUMSWE(JI)/PSNOW(JI)) + &
                        (1.0-ZSNOWMIX_DELTA(JI))*ZSNOWRHON(JI,JL)
!
      ZSNOWAGEN(JI,JL)  = ZSNOWMIX_DELTA(JI)*(ZSUMAGE(JI)/INLVLS)  + &
                         (1.0-ZSNOWMIX_DELTA(JI))*ZSNOWAGEN(JI,JL)  
!
      ZSNOWGRAN1N(JI,JL)  = ZSNOWMIX_DELTA(JI)*(ZSUMGRAN1(JI)/INLVLS)  + &
                         (1.0-ZSNOWMIX_DELTA(JI))*ZSNOWGRAN1N(JI,JL)  
!
      ZSNOWGRAN2N(JI,JL)  = ZSNOWMIX_DELTA(JI)*(ZSUMGRAN2(JI)/INLVLS)  + &
                         (1.0-ZSNOWMIX_DELTA(JI))*ZSNOWGRAN2N(JI,JL)  
!
   ENDDO
ENDDO
!
! 5. Update mass (density and thickness), heat and snow grain properties:
! ------------------------------------------------
!
PSNOWDZ  (:,:) = PSNOWDZN  (:,:)
PSNOWRHO (:,:) = ZSNOWRHON (:,:)
PSNOWHEAT(:,:) = ZSNOWHEATN(:,:)
PSNOWAGE (:,:) = ZSNOWAGEN (:,:)
PSNOWGRAN1 (:,:) = ZSNOWGRAN1N (:,:)
PSNOWGRAN2 (:,:) = ZSNOWGRAN2N (:,:)
!
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LTRANSF
!####################################################################
!####################################################################
!####################################################################
      SUBROUTINE SNOW3LRAD(OMEB, PSNOWDZMIN, PSW_RAD, PSNOWALB,      &
                           PSPECTRALALBEDO, PSNOWDZ, PSNOWRHO, PALB, &
                           PPERMSNOWFRAC, PZENITH,  PSWNETSNOW,      &
                           PSWNETSNOWS, PRADSINK, PRADXS, PSNOWAGE   )  
!
!!    PURPOSE
!!    -------
!     Calculate the transmission of shortwave (solar) radiation
!     through the snowpack (using a form of Beer's Law: exponential
!     decay of radiation with increasing snow depth).
!
USE MODD_SNOWES_PAR, ONLY : XVSPEC1,XVSPEC2,XVSPEC3,XVBETA1,XVBETA2, &
                          XVBETA4,XVBETA3,XVBETA5, XMINCOSZEN
!USE MODD_MEB_PAR,  ONLY : XSW_WGHT_VIS, XSW_WGHT_NIR
!
USE MODE_SNOWES, ONLY : SNOW3LDOPT
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
LOGICAL,            INTENT(IN)      :: OMEB ! if=T, then uppermost abs is diagnosed
!                                           !       since fluxes known
!
REAL,               INTENT(IN)      :: PSNOWDZMIN
!
REAL, DIMENSION(:), INTENT(IN)      :: PSW_RAD
REAL, DIMENSION(:), INTENT(IN)      :: PSNOWALB
REAL, DIMENSION(:), INTENT(IN)      :: PALB
REAL, DIMENSION(:), INTENT(IN)      :: PPERMSNOWFRAC
REAL, DIMENSION(:), INTENT(IN)      :: PZENITH
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWRHO, PSNOWDZ, PSNOWAGE
REAL, DIMENSION(:,:), INTENT(IN)    :: PSPECTRALALBEDO
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PSWNETSNOW, PSWNETSNOWS
!
REAL, DIMENSION(:), INTENT(OUT)     :: PRADXS
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PRADSINK
!
!
!*      0.2    declarations of local variables
!
INTEGER                              :: JJ, JI
!
INTEGER                              :: INI
INTEGER                              :: INLVLS
!
REAL, DIMENSION(SIZE(PSNOWRHO,1))    :: ZRADTOT, ZPROJLAT, ZCOSZEN
REAL, DIMENSION(SIZE(PSNOWRHO,1))    :: ZOPTICALPATH1, ZOPTICALPATH2, ZOPTICALPATH3
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZDSGRAIN, ZCOEF, ZSNOWDZ, ZAGE
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZBETA1, ZBETA2, ZBETA3, ZWORK
REAL, DIMENSION(SIZE(PSPECTRALALBEDO,1),SIZE(PSPECTRALALBEDO,2)) :: ZSPECTRALALBEDO
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
!
INI    = SIZE(PSNOWDZ(:,:),1)
INLVLS = SIZE(PSNOWDZ(:,:),2)
!
ZSPECTRALALBEDO(:,:) = 0. ! Init
!
! 1. Vanishingly thin snowpack check:
! -----------------------------------
!    For vanishingly thin snowpacks, much of the radiation
!    can pass through snowpack into underlying soil, making
!    a large (albeit temporary) thermal gradient: by imposing
!    a minimum thickness, this increases the radiation absorbtion
!    for vanishingly thin snowpacks.
!
ZSNOWDZ(:,:) = MAX(PSNOWDZMIN, PSNOWDZ(:,:))
!
!
! 2. Extinction of net shortwave radiation
! ----------------------------------------
! Fn of snow depth and density (Loth and Graf 1993:
! SNOWCVEXT => from Bohren and Barkstrom 1974
! SNOWAGRAIN and SNOWBGRAIN=> from Jordan 1976)
!
! Coefficient for taking into account the increase of path length of rays
! in snow due to zenithal angle
!
ZCOSZEN(:)=MAX(XMINCOSZEN,COS(PZENITH(:)))
!
! This formulation is incorrect but it compensate partly the fact that 
! the albedo formulation does not account for zenithal angle.
! Only for polar or glacier regions
!
ZPROJLAT(:)=(1.0-PPERMSNOWFRAC(:))+PPERMSNOWFRAC(:)/ZCOSZEN(:)
!
! Snow optical grain diameter (no age dependency over polar regions):
!
DO JJ=1,INLVLS
   DO JI=1,INI
      ZAGE(JI,JJ) = (1.0-PPERMSNOWFRAC(JI))*PSNOWAGE(JI,JJ)
   ENDDO
ENDDO
!
ZDSGRAIN(:,:) = SNOW3LDOPT(PSNOWRHO,ZAGE)
!
! Extinction coefficient from Brun et al. (1989):
!
ZWORK(:,:)=SQRT(ZDSGRAIN(:,:))
!
ZBETA1(:,:)=MAX(XVBETA1*PSNOWRHO(:,:)/ZWORK(:,:),XVBETA2)
ZBETA2(:,:)=MAX(XVBETA3*PSNOWRHO(:,:)/ZWORK(:,:),XVBETA4)
ZBETA3(:,:)=XVBETA5
!
ZOPTICALPATH1(:) = 0.0
ZOPTICALPATH2(:) = 0.0
ZOPTICALPATH3(:) = 0.0
!
!IF(OMEB)THEN

! Only 2 bands currently considered (for vegetation and soil)
! thus eliminate a band and renormalize (as was done for the surface snow albedo
! for the MEB snow surface energy budget computations)

!   ZSPECTRALALBEDO(:,1) = PSPECTRALALBEDO(:,1)
!   ZSPECTRALALBEDO(:,2) = (PSNOWALB(:) - XSW_WGHT_VIS*ZSPECTRALALBEDO(:,1))/XSW_WGHT_NIR

!   DO JJ=1,INLVLS
!      DO JI=1,INI
!      !
!         ZOPTICALPATH1(JI) = ZOPTICALPATH1(JI) + ZBETA1(JI,JJ)*ZSNOWDZ(JI,JJ)
!         ZOPTICALPATH2(JI) = ZOPTICALPATH2(JI) + ZBETA2(JI,JJ)*ZSNOWDZ(JI,JJ)
!      !
!         ZCOEF (JI,JJ) = XSW_WGHT_VIS*(1.0-ZSPECTRALALBEDO(JI,1))*EXP(-ZOPTICALPATH1(JI)*ZPROJLAT(JI)) &
 !                      + XSW_WGHT_NIR*(1.0-ZSPECTRALALBEDO(JI,2))*EXP(-ZOPTICALPATH2(JI)*ZPROJLAT(JI)) 
      !
!      ENDDO
!   ENDDO

! diagnose surface layer coef (should be very close/identical to ZCOEF(:,1) computed above)

 !  ZCOEF(:,1)          = 1.0 - PSWNETSNOWS(:)/MAX(1.E-4,PSWNETSNOW(:))

!ELSE
!
   DO JJ=1,INLVLS
      DO JI=1,INI
      !
         ZOPTICALPATH1(JI) = ZOPTICALPATH1(JI) + ZBETA1(JI,JJ)*ZSNOWDZ(JI,JJ)
         ZOPTICALPATH2(JI) = ZOPTICALPATH2(JI) + ZBETA2(JI,JJ)*ZSNOWDZ(JI,JJ)
         ZOPTICALPATH3(JI) = ZOPTICALPATH3(JI) + ZBETA3(JI,JJ)*ZSNOWDZ(JI,JJ)
      !
         ZCOEF (JI,JJ) = XVSPEC1*(1.0-PSPECTRALALBEDO(JI,1))*EXP(-ZOPTICALPATH1(JI)*ZPROJLAT(JI)) &
                       + XVSPEC2*(1.0-PSPECTRALALBEDO(JI,2))*EXP(-ZOPTICALPATH2(JI)*ZPROJLAT(JI)) &
                       + XVSPEC3*(1.0-PSPECTRALALBEDO(JI,3))*EXP(-ZOPTICALPATH3(JI)*ZPROJLAT(JI))
      !
      ENDDO
   ENDDO

   PSWNETSNOW(:)       = PSW_RAD(:)*(1.-PSNOWALB(:))
   PSWNETSNOWS(:)      = PSWNETSNOW(:)*(1.0-ZCOEF(:,1)) 

!ENDIF
!
! 3. Radiation at each level: (W/m2)
! ----------------------------------
!
! NOTE: as this is a "sink" term for heat, it
! as assigned a negative value.
DO JJ=1,INLVLS
   DO JI=1,INI
      PRADSINK(JI,JJ)  = -PSW_RAD(JI)*ZCOEF(JI,JJ)
   ENDDO
ENDDO
!
! For thin snow packs, radiation might reach base of
! snowpack...so we influence this amount with sfc albedo
! and (outside of this routine) add any excess heat
! to underlying soil:
!
PRADSINK(:,INLVLS) = PRADSINK(:,INLVLS)*(1.0-PALB(:))
!
! 4. Excess radiation not absorbed by snowpack (W/m2):
! ----------------------------------------------------
!
ZRADTOT(:)    = PRADSINK(:,1) + (1.-PSNOWALB(:))*PSW_RAD(:)
DO JJ=2,INLVLS
   DO JI=1,INI
      ZRADTOT(JI) = ZRADTOT(JI) + PRADSINK(JI,JJ)-PRADSINK(JI,JJ-1)
   ENDDO
ENDDO
!
PRADXS(:)     = (1.-PSNOWALB(:))*PSW_RAD(:) - ZRADTOT(:)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LRAD
!####################################################################
!####################################################################
!####################################################################
      SUBROUTINE SNOW3LEBUD(HSNOWRES, HIMPLICIT_WIND,                                     &
                              PPEW_A_COEF, PPEW_B_COEF,                                   &
                              PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,         &
                              PSNOWDZMIN,                                                 &
                              PZREF,PTS,PSNOWRHO,PSNOWLIQ,PSCAP,PSCOND1,PSCOND2,          &
                              PUREF,PEXNS,PEXNA,PDIRCOSZW,PVMOD,                          &
                              PLW_RAD,PSW_RAD,PTA,PQA,PPS,PTSTEP,                         &
                              PSNOWDZ1,PSNOWDZ2,PALBT,PZ0,PZ0EFF,PZ0H,                    &
                              PSFCFRZ,PRADSINK,PHPSNOW,                                   &
                              PCT,PEMIST,PRHOA,PTSTERM1,PTSTERM2,PRA,PCDSNOW,PCHSNOW,     &
                              PQSAT,PDQSAT,PRSRA,PUSTAR2_IC,PRI,                          &
                              PPET_A_COEF_T,PPEQ_A_COEF_T,PPET_B_COEF_T,PPEQ_B_COEF_T     )  
!
!!    PURPOSE
!!    -------
!     Calculate surface energy budget linearization (terms) and turbulent
!     exchange coefficients/resistance between surface and atmosphere.
!     (Noilhan and Planton 1989; Giordani 1993; Noilhan and Mahfouf 1996)
!
USE MODD_CSTS_SNOWES,     ONLY : XCPD, XRHOLW, XSTEFAN, XLVTT, XLSTT
USE MODD_SNOWES_PAR, ONLY : X_RI_MAX, XEMISSN
!
USE MODE_THERMOS_SNOW 
!
!!USE MODI_SURFACE_RI
!USE MODI_SURFACE_AERO_COND
!USE MODI_SURFACE_CD
! REPLACE USE_MODI , BY :
USE surface_aero_cond_snowES_mod , ONLY: SURFACE_AERO_COND_SNOWES
USE surface_cd_snowES_mod, ONLY: SURFACE_CD_SNOWES
USE surface_ri_snowES_mod, ONLY: SURFACE_RI_SNOWES

!
IMPLICIT NONE
!
! EXTERNAL SURFACE_RI
! EXTERNAL SURFACE_AERO_COND
! EXTERNAL SURFACE_CD
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP, PSNOWDZMIN
!
 CHARACTER(LEN=*),     INTENT(IN)    :: HSNOWRES ! type of sfc resistance
!                                      DEFAULT=Louis (1979), standard ISBA
!                                      method. Option to limit Ri number
!                                      for very srtable conditions
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
!
REAL, DIMENSION(:), INTENT(IN)      :: PPEW_A_COEF, PPEW_B_COEF,                   &
                                         PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,      &
                                         PPEQ_B_COEF  
!                                      PPEW_A_COEF = wind coefficient (m2s/kg)
!                                      PPEW_B_COEF = wind coefficient (m/s)
!                                      PPET_A_COEF = A-air temperature coefficient
!                                      PPET_B_COEF = B-air temperature coefficient
!                                      PPEQ_A_COEF = A-air specific humidity coefficient
!                                      PPEQ_B_COEF = B-air specific humidity coefficient
!
REAL, DIMENSION(:), INTENT(IN)    :: PZREF, PTS, PSNOWDZ1, PSNOWDZ2,          &
                                         PRADSINK, PSNOWRHO, PSNOWLIQ, PSCAP,   &
                                         PSCOND1, PSCOND2,                      &
                                         PZ0, PHPSNOW,                          &
                                         PALBT, PZ0EFF, PZ0H  
!
REAL, DIMENSION(:), INTENT(IN)    :: PSW_RAD, PLW_RAD, PTA, PQA, PPS, PRHOA
!
REAL, DIMENSION(:), INTENT(IN)    :: PUREF, PEXNS, PEXNA, PDIRCOSZW, PVMOD
!
REAL, DIMENSION(:), INTENT(OUT)   :: PTSTERM1, PTSTERM2, PEMIST, PRA,         &
                                       PCT, PSFCFRZ, PCDSNOW, PCHSNOW,          &
                                       PQSAT, PDQSAT, PRSRA  
!
REAL, DIMENSION(:), INTENT(OUT)   :: PUSTAR2_IC,                        &
                                       PPET_A_COEF_T, PPEQ_A_COEF_T,    &
                                       PPET_B_COEF_T, PPEQ_B_COEF_T 
!
REAL, DIMENSION(:), INTENT(OUT)   :: PRI
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTS))        :: ZAC, ZRI, ZCOND1, ZCOND2,          &
                                       ZSCONDA, ZA, ZB, ZC,             &
                                       ZCDN, ZSNOWDZM1, ZSNOWDZM2,      &
                                       ZVMOD, ZUSTAR2, ZTS3, ZLVT,      &
                                       Z_CCOEF  
!
!-------------------------------------------------------------------------------
!
! 1. New saturated specific humidity and derrivative:
! ---------------------------------------------------
!
!
ZRI   (:) = XUNDEF
!
PQSAT (:) =  QSATI(PTS(:),PPS(:))
PDQSAT(:) = DQSATI(PTS(:),PPS(:),PQSAT(:))
!
!
! 2. Surface properties:
! ----------------------
! It might be of interest to use snow-specific roughness
! or a temperature dependence on emissivity:
! but for now, use ISBA defaults.
!
PEMIST(:) = XEMISSN
!
! 2. Computation of resistance and drag coefficient
! -------------------------------------------------
!
 CALL SURFACE_RI_SNOWES(PTS, PQSAT, PEXNS, PEXNA, PTA, PQA,                  &
                PZREF, PUREF, PDIRCOSZW, PVMOD, ZRI                  )  
!
! Simple adaptation of method by Martin and Lejeune (1998)
! to apply a lower limit to turbulent transfer coef
! by defining a maximum Richarson number for stable
! conditions:
!
IF(HSNOWRES=='RIL') THEN
  DO JJ = 1,SIZE(ZRI)
    ZRI(JJ) = MIN(X_RI_MAX,ZRI(JJ))
  ENDDO
ENDIF
!
PRI(:)=ZRI(:)
!
! Surface aerodynamic resistance for heat transfers
!
 CALL SURFACE_AERO_COND_SNOWES(ZRI, PZREF, PUREF, PVMOD, PZ0, PZ0H, ZAC, PRA, PCHSNOW)
!
! For atmospheric model coupling:
!
 CALL SURFACE_CD_SNOWES(ZRI, PZREF, PUREF, PZ0EFF, PZ0H, PCDSNOW, ZCDN)
!
PRSRA(:) = PRHOA(:) / PRA(:)
!
!
! Modify flux-form implicit coupling coefficients:
! - wind components:
!
IF(HIMPLICIT_WIND=='OLD')THEN
! old implicitation
  ZUSTAR2(:) = ( PCDSNOW(:)*PVMOD(:)*PPEW_B_COEF(:)) /        &
               (1.0-PRHOA(:)*PCDSNOW(:)*PVMOD(:)*PPEW_A_COEF(:))  
ELSE
! new implicitation
  ZUSTAR2(:) = (PCDSNOW(:)*PVMOD(:)*(2.*PPEW_B_COEF(:)-PVMOD(:)))  &
               / (1.0-2.0*PRHOA(:)*PCDSNOW(:)*PVMOD(:)*PPEW_A_COEF(:))
ENDIF               
!
ZVMOD(:)       = PRHOA(:)*PPEW_A_COEF(:)*ZUSTAR2(:) + PPEW_B_COEF(:)
ZVMOD(:)       = MAX(ZVMOD(:),0.)
!
WHERE(PPEW_A_COEF(:)/= 0.)
      ZUSTAR2(:) = MAX( ( ZVMOD(:) - PPEW_B_COEF(:) ) / (PRHOA(:)*PPEW_A_COEF(:)), 0.)
ENDWHERE
!
! implicit wind friction
ZUSTAR2(:) = MAX(ZUSTAR2(:),0.)
!
PUSTAR2_IC(:) =  ZUSTAR2(:)

!
! 3. Calculate linearized surface energy budget components:
! ---------------------------------------------------------
! To prevent numerical difficulties for very thin snow
! layers, limit the grid "thinness": this is important as
! layers become vanishing thin:
!
ZSNOWDZM1(:) = MAX(PSNOWDZ1(:), PSNOWDZMIN)
ZSNOWDZM2(:) = MAX(PSNOWDZ2(:), PSNOWDZMIN)
!
! Surface thermal inertia:
!
PCT(:)      = 1.0/(PSCAP(:)*ZSNOWDZM1(:))
!
! Fraction of surface frozen (sublimation) with the remaining
! fraction being liquid (evaporation):
! NOTE: currently, for simplicity, assume no liquid water flux
! OFF: PSFCFRZ(:)  = 1.0 - PSNOWLIPPEQ_A_COEFQ(:)*XRHOLW/(ZSNOWDZM1(:)*PSNOWRHO(:))
!
PSFCFRZ(:)  = 1.0 
!
! Thermal conductivity between uppermost and lower snow layers:
!
ZCOND1 (:) = ZSNOWDZM1(:)/((ZSNOWDZM1(:)+ZSNOWDZM2(:))*PSCOND1(:))
ZCOND2 (:) = ZSNOWDZM2(:)/((ZSNOWDZM1(:)+ZSNOWDZM2(:))*PSCOND2(:))
!
ZSCONDA(:) = 1.0/(ZCOND1(:)+ZCOND2(:))
!
! Transform implicit coupling coefficients: 
! Note, surface humidity is 100% over snow.
!
! - specific humidity:
!
Z_CCOEF(:)       = 1.0 - PPEQ_A_COEF(:)*PRSRA(:)
!
PPEQ_A_COEF_T(:) = - PPEQ_A_COEF(:)*PRSRA(:)*PDQSAT(:)/Z_CCOEF(:)
!
PPEQ_B_COEF_T(:) = ( PPEQ_B_COEF(:) - PPEQ_A_COEF(:)*PRSRA(:)*(PQSAT(:) - &
                       PDQSAT(:)*PTS(:)) )/Z_CCOEF(:)  
!
! - air temperature:
!   (assumes A and B correspond to potential T):
!
Z_CCOEF(:)       = (1.0 - PPET_A_COEF(:)*PRSRA(:))/PEXNA(:)
!
PPET_A_COEF_T(:) = - PPET_A_COEF(:)*PRSRA(:)/(PEXNS(:)*Z_CCOEF(:))
!
PPET_B_COEF_T(:) = PPET_B_COEF(:)/Z_CCOEF(:)
!
!
! Energy budget solution terms:
!
ZTS3(:) = PEMIST(:) * XSTEFAN * PTS(:)**3
ZLVT(:) = (1.-PSFCFRZ(:))*XLVTT + PSFCFRZ(:)*XLSTT
!
ZA(:)   = 1. / PTSTEP + PCT(:) * (4. * ZTS3(:) +                               &
            PRSRA(:) *  ZLVT(:) * (PDQSAT(:) - PPEQ_A_COEF_T(:))                 &
            + PRSRA(:) * XCPD * ( (1./PEXNS(:))-(PPET_A_COEF_T(:)/PEXNA(:)) )    &
            + (2.*ZSCONDA(:)/(ZSNOWDZM2(:)+ZSNOWDZM1(:))) )  
!
ZB(:)   = 1. / PTSTEP + PCT(:) * (3. * ZTS3(:) +                               &
            PRSRA(:) * PDQSAT(:) *  ZLVT(:) )  
!
ZC(:)   = PCT(:) * (PRSRA(:) * XCPD * PPET_B_COEF_T(:)/PEXNA(:) + PSW_RAD(:) * &
            (1. - PALBT(:)) + PEMIST(:)*PLW_RAD(:) - PRSRA(:) *                  &
            ZLVT(:) *  (PQSAT(:)-PPEQ_B_COEF_T(:))                               &
            + PHPSNOW(:) + PRADSINK(:) )  
!
!
! Coefficients needed for implicit solution
! of linearized surface energy budget:
!
PTSTERM2(:) = 2.*ZSCONDA(:)*PCT(:)/(ZA(:)*(ZSNOWDZM2(:)+ZSNOWDZM1(:)))
!
PTSTERM1(:) = (PTS(:)*ZB(:) + ZC(:))/ZA(:)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LEBUD
!####################################################################
!####################################################################
!####################################################################
      SUBROUTINE SNOW3LSOLVT(OMEB,PTSTEP,PSNOWDZMIN,                  &
                               PSNOWDZ,PSCOND,PSCAP,PTG,              &
                               PSOILCOND,PD_G,                        &
                               PRADSINK,PCT,PTERM1,PTERM2,            &
                               PPET_A_COEF_T,PPEQ_A_COEF_T,           &
                               PPET_B_COEF_T,PPEQ_B_COEF_T,           &
                               PTA_IC, PQA_IC,                        &
                               PGRNDFLUX,PGRNDFLUXO,PSNOWTEMP,        &
                               PSNOWFLUX                              )  
!
!!    PURPOSE
!!    -------
!     This subroutine solves the 1-d diffusion of 'ZSNOWTEMP' using a
!     layer averaged set of equations which are time differenced
!     using the backward-difference scheme (implicit).
!     The eqs are solved rapidly by taking advantage of the
!     fact that the matrix is tridiagonal. This is a very
!     general routine and can be used for the 1-d diffusion of any
!     quantity as long as the diffusity is not a function of the
!     quantity being diffused. Aaron Boone 8-98. Soln to the eq:
!
!                 c  dQ    d  k dQ    dS
!                    -- =  --   -- -  --
!                    dt    dx   dx    dx
!
!     where k = k(x) (thermal conductivity), c = c(x) (heat capacity)
!     as an eg. for temperature/heat eq. S is a sink (-source) term.
!     Diffusivity is k/c
!
!
USE MODD_CSTS_SNOWES,ONLY : XTT
!
!USE MODI_TRIDIAG_GROUND
USE tridiag_ground_snowES_mod , ONLY: TRIDIAG_GROUND_SNOWES
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
LOGICAL,            INTENT(IN)      :: OMEB
!
REAL,               INTENT(IN)      :: PTSTEP, PSNOWDZMIN
!
REAL, DIMENSION(:), INTENT(IN)      :: PTG, PSOILCOND, PD_G,         &
                                         PCT, PTERM1, PTERM2  

!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWDZ, PSCOND, PSCAP,       &
                                         PRADSINK
!
REAL, DIMENSION(:), INTENT(IN)      :: PPET_A_COEF_T, PPEQ_A_COEF_T, &
                                         PPET_B_COEF_T, PPEQ_B_COEF_T  
!                                       
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWTEMP
!
REAL, DIMENSION(:), INTENT(OUT)     :: PGRNDFLUX, PGRNDFLUXO, PSNOWFLUX,     &
                                         PTA_IC, PQA_IC   
!
!
!*      0.2    declarations of local variables
!
!
INTEGER                             :: JJ, JI
!
INTEGER                             :: INI
INTEGER                             :: INLVLS
!
REAL, DIMENSION(SIZE(PTG))                     :: ZSNOWTEMP_DELTA
!
REAL, DIMENSION(SIZE(PSNOWDZ,1),SIZE(PSNOWDZ,2)) :: ZSNOWTEMP, ZDTERM, ZCTERM, &
                                                      ZFRCV, ZAMTRX, ZBMTRX,     &
                                                      ZCMTRX  
!
REAL, DIMENSION(SIZE(PSNOWDZ,1),SIZE(PSNOWDZ,2)) :: ZWORK1, ZWORK2, ZDZDIF,    &
                                                      ZSNOWDZM  
!
REAL, DIMENSION(SIZE(PSNOWDZ,1),SIZE(PSNOWDZ,2)-1) :: ZSNOWTEMP_M,             &
                                                      ZFRCV_M, ZAMTRX_M,         &
                                                      ZBMTRX_M, ZCMTRX_M  
!-------------------------------------------------------------------------------
!
! 0. Initialize:
! ------------------
!
ZSNOWTEMP(:,:)  = PSNOWTEMP(:,:)
INI             = SIZE(PSNOWDZ(:,:),1)
INLVLS          = SIZE(PSNOWDZ(:,:),2)
!
!
! 1. Calculate tri-diagnoal matrix coefficients:
! ----------------------------------------------
! For heat transfer, assume a minimum grid
! thickness (to prevent numerical
! problems for very thin snow cover):
!
ZSNOWDZM(:,:)  = MAX(PSNOWDZ(:,:), PSNOWDZMIN)
!
DO JJ=1,INLVLS-1
   DO JI=1,INI
      ZDZDIF(JI,JJ)  = 0.5*(ZSNOWDZM(JI,JJ)+ZSNOWDZM(JI,JJ+1))
      ZWORK1(JI,JJ)  = ZSNOWDZM(JI,JJ  )/(2.0*ZDZDIF(JI,JJ)*PSCOND(JI,JJ  ))
      ZWORK2(JI,JJ)  = ZSNOWDZM(JI,JJ+1)/(2.0*ZDZDIF(JI,JJ)*PSCOND(JI,JJ+1))
   ENDDO
ENDDO
!
ZDZDIF(:,INLVLS) = 0.5*(ZSNOWDZM(:,INLVLS)+PD_G(:))
ZWORK1(:,INLVLS) = ZSNOWDZM(:,INLVLS)/(2.0*ZDZDIF(:,INLVLS)*PSCOND   (:,INLVLS))
ZWORK2(:,INLVLS) = PD_G    (:       )/(2.0*ZDZDIF(:,INLVLS)*PSOILCOND(:       ))
!
ZDTERM(:,:)      = 1.0/(ZDZDIF(:,:)*(ZWORK1(:,:)+ZWORK2(:,:)))
!
ZCTERM(:,:)      = PSCAP(:,:)*ZSNOWDZM(:,:)/PTSTEP
!
! 2. Set up tri-diagonal matrix
! -----------------------------
!
! Upper BC
!
ZAMTRX(:,1) =  0.0
ZBMTRX(:,1) =  1./(PCT(:)*PTSTEP)
ZCMTRX(:,1) = -PTERM2(:)*ZBMTRX(:,1)
ZFRCV(:,1)  =  PTERM1(:)*ZBMTRX(:,1)
!
!
! Interior Grid
!
DO JJ=2,INLVLS-1
   DO JI=1,INI
      ZAMTRX(JI,JJ) = -ZDTERM(JI,JJ-1)
      ZBMTRX(JI,JJ) =  ZCTERM(JI,JJ) + ZDTERM(JI,JJ-1) + ZDTERM(JI,JJ)
      ZCMTRX(JI,JJ) = -ZDTERM(JI,JJ)
      ZFRCV (JI,JJ) =  ZCTERM(JI,JJ)*PSNOWTEMP(JI,JJ) - (PRADSINK(JI,JJ-1)-PRADSINK(JI,JJ))  
   ENDDO
ENDDO
!
!Lower BC
!
ZAMTRX(:,INLVLS) = -ZDTERM(:,INLVLS-1)
ZBMTRX(:,INLVLS) =  ZCTERM(:,INLVLS) + ZDTERM(:,INLVLS-1) +                   &
                      ZDTERM(:,INLVLS)  
ZCMTRX(:,INLVLS) =  0.0
ZFRCV(:,INLVLS)  =  ZCTERM(:,INLVLS)*PSNOWTEMP(:,INLVLS) +                    &
                      ZDTERM(:,INLVLS)*PTG(:)                                   &
                      - (PRADSINK(:,INLVLS-1)-PRADSINK(:,INLVLS))  
!
! - - -------------------------------------------------
!
! 4. Compute solution vector
! --------------------------
!
 CALL TRIDIAG_GROUND_SNOWES(ZAMTRX,ZBMTRX,ZCMTRX,ZFRCV,ZSNOWTEMP)
!
! Heat flux between surface and 2nd snow layers: (W/m2)
!
PSNOWFLUX(:)      = ZDTERM(:,1)*(ZSNOWTEMP(:,1) - ZSNOWTEMP(:,2))
!
!
! 5. Snow melt case
! -----------------
! If melting in uppermost layer, assume surface layer
! temperature at freezing point and re-evaluate lower
! snowpack temperatures. This is done as it is most likely
! most signigant melting will occur within a time step in surface layer.
! Surface energy budget (and fluxes) will
! be re-calculated (outside of this routine).
!
! NOTE: if MEB is active, then surface fluxes have been defined outside
! of the snow routine and have been adjusted such that they are evaluated
! at a snow surface temperature no greater than Tf. Thus, the implicit surface temperature
! will likely never greatly exceed Tf (before melt computed and they are adjusted to Tf)
! so we can skip the next block of code when MEB is active.
!
IF(.NOT.OMEB)THEN
!
   ZAMTRX_M(:,1)  =  0.0
   ZBMTRX_M(:,1)  =  ZCTERM(:,2) + ZDTERM(:,1) + ZDTERM(:,2)
   ZCMTRX_M(:,1)  = -ZDTERM(:,2)
   ZFRCV_M(:,1)   =  ZCTERM(:,2)*PSNOWTEMP(:,2) + XTT*ZDTERM(:,1)  -  &
                    (PRADSINK(:,1)-PRADSINK(:,2))  
!
   DO JJ=2,INLVLS-1
      DO JI=1,INI
         ZAMTRX_M   (JI,JJ) = ZAMTRX   (JI,JJ+1)
         ZBMTRX_M   (JI,JJ) = ZBMTRX   (JI,JJ+1)
         ZCMTRX_M   (JI,JJ) = ZCMTRX   (JI,JJ+1)
         ZFRCV_M    (JI,JJ) = ZFRCV    (JI,JJ+1)
         ZSNOWTEMP_M(JI,JJ) = PSNOWTEMP(JI,JJ+1)
      ENDDO
   ENDDO
!
   CALL TRIDIAG_GROUND_SNOWES(ZAMTRX_M,ZBMTRX_M,ZCMTRX_M,ZFRCV_M,ZSNOWTEMP_M)
!
! If melting for 2 consecuative time steps, then replace current T-profile
! with one assuming T=Tf in surface layer:
!
   ZSNOWTEMP_DELTA(:)    = 0.0
!
   WHERE(ZSNOWTEMP(:,1) > XTT .AND. PSNOWTEMP(:,1) == XTT)
      PSNOWFLUX(:)       = ZDTERM(:,1)*(XTT - ZSNOWTEMP_M(:,1))
      ZSNOWTEMP_DELTA(:) = 1.0
   END WHERE
!
   DO JJ=2,INLVLS
      DO JI=1,INI
         ZSNOWTEMP(JI,JJ) = ZSNOWTEMP_DELTA(JI)*ZSNOWTEMP_M(JI,JJ-1)   &
              + (1.0-ZSNOWTEMP_DELTA(JI))*ZSNOWTEMP(JI,JJ)  
      ENDDO
   ENDDO
!
ENDIF
!
!
! 6. Lower boundary flux:
! -----------------------
! NOTE: evaluate this term assuming the snow layer
! can't exceed the freezing point as this adjustment
! is made in melting routine. Then must adjust temperature
! to conserve energy:
!
PGRNDFLUXO(:)       = ZDTERM(:,INLVLS)*(ZSNOWTEMP(:,INLVLS)         -PTG(:))
PGRNDFLUX(:)        = ZDTERM(:,INLVLS)*(MIN(XTT,ZSNOWTEMP(:,INLVLS))-PTG(:))
!
ZSNOWTEMP(:,INLVLS) = ZSNOWTEMP(:,INLVLS) + (PGRNDFLUXO(:)-PGRNDFLUX(:))/ZCTERM(:,INLVLS)
!
! 7. Update temperatute profile in time:
! --------------------------------------
!
PSNOWTEMP(:,:)      = ZSNOWTEMP(:,:)
!
!
! 8. Compute new (implicit) air T and specific humidity
! -----------------------------------------------------
!
IF(.NOT.OMEB)THEN
!
   PTA_IC(:) = PPET_B_COEF_T(:) + PPET_A_COEF_T(:)* PSNOWTEMP(:,1)
!
   PQA_IC(:) = PPEQ_B_COEF_T(:) + PPEQ_A_COEF_T(:)* PSNOWTEMP(:,1)
!
ENDIF
!
!
!
END SUBROUTINE SNOW3LSOLVT
!####################################################################
!####################################################################
!####################################################################
      SUBROUTINE SNOW3LMELT(PTSTEP,PSCAP,PSNOWTEMP,PSNOWDZ,         &
                              PSNOWRHO,PSNOWLIQ,PMELTXS               )  
!
!
!!    PURPOSE
!!    -------
!     Calculate snow melt (resulting from surface fluxes, ground fluxes,
!     or internal shortwave radiation absorbtion). It is used to
!     augment liquid water content, maintain temperatures
!     at or below freezing, and possibly reduce the mass
!     or compact the layer(s).
!
!
USE MODD_CSTS_SNOWES,ONLY : XTT, XLMTT, XRHOLW, XRHOLI
!
USE MODE_SNOWES
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSCAP
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWDZ, PSNOWTEMP, PSNOWRHO,   &
                                           PSNOWLIQ  
!
REAL, DIMENSION(:), INTENT(OUT)     :: PMELTXS
!
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZPHASE, ZCMPRSFACT,   &
                                                        ZSNOWLWE, ZWHOLDMAX,  &
                                                        ZSNOWMELT, ZSNOWTEMP, &
                                                        ZMELTXS  
!
INTEGER :: JWRK, JI ! loop counter
!-------------------------------------------------------------------------------
!
! 0. Initialize:
! ---------------------------
!
ZPHASE(:,:)     = 0.0
ZCMPRSFACT(:,:) = 0.0
ZSNOWLWE(:,:)   = 0.0
ZWHOLDMAX(:,:)  = 0.0
ZSNOWMELT(:,:)  = 0.0
ZSNOWTEMP(:,:)  = 0.0
ZMELTXS(:,:)    = 0.0
!
! 1. Determine amount of melt in each layer:
! ------------------------------------------
!
WHERE(PSNOWDZ > 0.0)
!
! Total Liquid equivalent water content of snow (m):
!
   ZSNOWLWE(:,:) = PSNOWRHO(:,:)*PSNOWDZ(:,:)/XRHOLW
!
! Melt snow if excess energy and snow available:
! Phase change (J/m2)
!
   ZPHASE(:,:)  = MIN(PSCAP(:,:)*MAX(0.0, PSNOWTEMP(:,:) - XTT)*      &
                    PSNOWDZ(:,:),                                       &
                    MAX(0.0,ZSNOWLWE(:,:)-PSNOWLIQ(:,:))*XLMTT*XRHOLW)  
!
!
! Update snow liq water content and temperature if melting:
! liquid water available for next layer from melting of snow
! which is assumed to be leaving the current layer (m):
!
   ZSNOWMELT(:,:) = ZPHASE(:,:)/(XLMTT*XRHOLW)
!
! Cool off snow layer temperature due to melt:
!
   ZSNOWTEMP(:,:) = PSNOWTEMP(:,:) - ZPHASE(:,:)/(PSCAP(:,:)*PSNOWDZ(:,:))
!
   PSNOWTEMP(:,:) = MIN(XTT, ZSNOWTEMP(:,:))
!
   ZMELTXS(:,:)   = (ZSNOWTEMP(:,:)-PSNOWTEMP(:,:))*PSCAP(:,:)*PSNOWDZ(:,:)
!
END WHERE
!
! Loss of snowpack depth: (m) and liquid equiv (m):
! Compression factor for melt loss: this decreases
! layer thickness and increases density thereby leaving
! total SWE constant. NOTE: All melt water
! in excess of the holding capacity is NOT used
! for compression, rather it decreases the layer
! thickness ONLY, causing a loss of SWE (outside
! of this routine).
!
!ZWHOLDMAX(:,:)  = SNOW3LHOLD(PSNOWRHO,PSNOWDZ)
DO JJ=1,INLVLS
   DO JI=1,INI
    IF ( CSNOWHOLD == 'B02' ) THEN
       ZWHOLDMAX(JI,JJ) = SNOW3LHOLD(PSNOWRHO(JI,JJ),PSNOWDZ(JI,JJ))
    ELSE IF ( CSNOWHOLD == 'B92' ) THEN 
       ZWHOLDMAX(JI,JJ) = SNOWCROHOLD(PSNOWRHO(JI,JJ),PSNOWLIQ(JI,JJ),PSNOWDZ(JI,JJ))
    ELSE IF ( CSNOWHOLD == 'SPK' ) THEN
      ZWHOLDMAX(JI,JJ) = SNOWSPKHOLD(PSNOWRHO(JI,JJ),PSNOWLIQ(JI,JJ),PSNOWDZ(JI,JJ))
   ENDIF
ENDDO
ENDDO

!
WHERE(PSNOWDZ > 0.0)
!
   ZCMPRSFACT(:,:) = (ZSNOWLWE(:,:)-MIN(PSNOWLIQ(:,:)+ZSNOWMELT(:,:),     &
                        ZWHOLDMAX(:,:)))/                                   &
                       (ZSNOWLWE(:,:)-MIN(PSNOWLIQ(:,:),ZWHOLDMAX(:,:)))  
!
   PSNOWDZ(:,:)    = PSNOWDZ(:,:)*ZCMPRSFACT(:,:)
   PSNOWRHO(:,:)   = ZSNOWLWE(:,:)*XRHOLW/PSNOWDZ(:,:)
!
! Make sure maximum density is not surpassed! If it is, lower the density
! and increase the snow thickness accordingly:

   ZCMPRSFACT(:,:) = MAX(XRHOLI, PSNOWRHO(:,:))/XRHOLI
   PSNOWDZ(:,:)    = PSNOWDZ(:,:)*ZCMPRSFACT(:,:)
   PSNOWRHO(:,:)   = ZSNOWLWE(:,:)*XRHOLW/PSNOWDZ(:,:)
!
!
! 2. Add snow melt to current snow liquid water content:
! ------------------------------------------------------
!
   PSNOWLIQ(:,:)   = PSNOWLIQ(:,:) + ZSNOWMELT(:,:)
!
END WHERE
!
! 3. Excess heat from melting
! ---------------------------
! use it to warm underlying ground/vegetation layer to conserve energy
!
PMELTXS(:) = 0.
DO JWRK = 1, SIZE(ZMELTXS,2)
   DO JI = 1, SIZE(ZMELTXS,1)
      PMELTXS(JI) = PMELTXS(JI) + ZMELTXS(JI,JWRK)
   ENDDO
ENDDO
PMELTXS(:) = PMELTXS(:) / PTSTEP   ! (W/m2)
!
!
!
!
END SUBROUTINE SNOW3LMELT
!####################################################################
!####################################################################
!####################################################################
      SUBROUTINE SNOW3LREFRZ(PTSTEP,PRR,                            &
                               PSNOWRHO,PSNOWTEMP,PSNOWDZ,PSNOWLIQ, &
                               PTHRUFAL                             )
!
!
!!    PURPOSE
!!    -------
!     Calculate any freezing/refreezing of liquid water in the snowpack.
!     Also, calculate liquid water transmission and snow runoff.
!     Water flow causes layer thickness reduction and can cause
!     rapid densification of a layer.
!
!
USE MODD_CSTS_SNOWES,     ONLY : XTT, XLMTT, XRHOLW
USE MODD_SNOWES_PAR, ONLY : XSNOWDMIN
!
USE MODE_SNOWES
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                      :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)        :: PRR
!
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWDZ, PSNOWTEMP, PSNOWLIQ, PSNOWRHO
!
REAL, DIMENSION(:), INTENT(INOUT)     :: PTHRUFAL
!
!
!
!*      0.2    declarations of local variables
!
INTEGER                               :: JJ, JI
!
INTEGER                               :: INI
INTEGER                               :: INLVLS
!
REAL, DIMENSION(SIZE(PRR))            :: ZPCPXS, ZTOTWCAP, ZRAINFALL
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) ::   ZFLOWLIQ, ZWORK,     &
                                                        ZSNOWLIQ, ZSNOWRHO,  &
                                                        ZWHOLDMAX, ZSNOWDZ,  &
                                                        ZSNOWTEMP, ZSCAP,    &  
                                                        ZSNOWHEAT
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),0:SIZE(PSNOWRHO,2)):: ZFLOWLIQT
!
!
!-------------------------------------------------------------------------------
!
! 0. Initialize:
! --------------
!
!
ZSNOWRHO(:,:)  = PSNOWRHO(:,:)
ZSNOWLIQ(:,:)  = PSNOWLIQ(:,:)
ZSNOWTEMP(:,:) = PSNOWTEMP(:,:)
INI            = SIZE(PSNOWDZ(:,:),1)
INLVLS         = SIZE(PSNOWDZ(:,:),2)
!
!
! 1. Refreeze due to heat transfer
!    -----------------------------
! Freeze liquid water in any layers which cooled due
! to heat transfer. First, update H and re-diagnose (update) T and Wl:
!
ZSCAP(:,:)     = SNOW3LSCAP(ZSNOWRHO)
!
ZSNOWHEAT(:,:) = PSNOWDZ(:,:)*( ZSCAP(:,:)*(ZSNOWTEMP(:,:)-XTT)        &
                        - XLMTT*ZSNOWRHO(:,:) ) + XLMTT*XRHOLW*ZSNOWLIQ(:,:)  
!
ZSNOWTEMP(:,:) = XTT + ( ((ZSNOWHEAT(:,:)/MAX(PSNOWDZ(:,:),XSNOWDMIN/INLVLS))    &
                   + XLMTT*ZSNOWRHO(:,:))/ZSCAP(:,:) )
!
ZSNOWLIQ(:,:)  = MAX(0.0,ZSNOWTEMP(:,:)-XTT)*ZSCAP(:,:)*PSNOWDZ(:,:)/(XLMTT*XRHOLW)  
!
ZSNOWTEMP(:,:) = MIN(XTT,ZSNOWTEMP(:,:))
!
!
! 2. Reduce thickness due to snowmelt in excess of holding capacity
!    --------------------------------------------------------------
! Any water in excess of the
! Maximum holding space for liquid water
! amount is drained into next layer down.
! Loss of water due to snowmelt causes a reduction
! in snow layer mass by a reduction in thickness. Owing to a consistent
! decrease in both liq and thickness (and the fact that T=Tf when liquid present), 
! enthalpy is conserved.
!
!ZWHOLDMAX(:,:) = SNOW3LHOLD(PSNOWRHO,PSNOWDZ)

DO JJ=1,INLVLS
   DO JI=1,INI
    IF ( CSNOWHOLD == 'B02' ) THEN
       ZWHOLDMAX(JI,JJ) = SNOW3LHOLD(PSNOWRHO(JI,JJ),PSNOWDZ(JI,JJ))
    ELSE IF ( CSNOWHOLD == 'B92' ) THEN 
       ZWHOLDMAX(JI,JJ) = SNOWCROHOLD(PSNOWRHO(JI,JJ),PSNOWLIQ(JI,JJ),PSNOWDZ(JI,JJ))
    ELSE IF ( CSNOWHOLD == 'SPK' ) THEN
      ZWHOLDMAX(JI,JJ) = SNOWSPKHOLD(PSNOWRHO(JI,JJ),PSNOWLIQ(JI,JJ),PSNOWDZ(JI,JJ))
   ENDIF
ENDDO
ENDDO

ZFLOWLIQ(:,:)  = MAX(0.,ZSNOWLIQ(:,:)-ZWHOLDMAX(:,:))
!
ZSNOWLIQ(:,:)  = ZSNOWLIQ(:,:) - ZFLOWLIQ(:,:)
!
ZSNOWDZ(:,:)   = PSNOWDZ(:,:) - ZFLOWLIQ(:,:)*XRHOLW/ZSNOWRHO(:,:)
!
ZSNOWDZ(:,:)   = MAX(0.0, ZSNOWDZ(:,:))  ! to prevent possible very small
!                                          negative values (machine prescision
!                                          as snow vanishes
!
!
! 3. Liquid water flow: liquid precipitation and meltwater
!    -----------------------------------------------------
!
! Rainfall flowing into uppermost snow layer:
! If rainfall is excessive enough (or layers thin enough)
! it is simply routed directly to runoff: First calculate
! the total snow pack available liquid water holding capacity:
!
ZTOTWCAP(:)   = 0.
DO JJ=1,INLVLS
   DO JI=1,INI
      ZTOTWCAP(JI) = ZTOTWCAP(JI) + ZWHOLDMAX(JI,JJ)
   ENDDO
ENDDO
!
! Rain entering snow (m):
!
ZRAINFALL(:)  = PRR(:)*PTSTEP/XRHOLW                ! rainfall (m)
!
ZFLOWLIQT(:,0)= MIN(ZRAINFALL(:),ZTOTWCAP(:))
!
! Rain assumed to directly pass through the pack to runoff (m):
!
ZPCPXS(:)     = ZRAINFALL(:) - ZFLOWLIQT(:,0)
!
DO JJ=1,INLVLS
   DO JI=1,INI
      ZFLOWLIQT(JI,JJ) = ZFLOWLIQ(JI,JJ)
   ENDDO
ENDDO
!
!
! Thickness is maintained during water through-flow,
! so that mass transfer is represented by
! density changes: NOTE a maximum density
! is assumed (XRHOSMAX_ES) so that all flow
! which would result in densities exceeding
! this limit are routed to next layer down.
! First test for saturation, then
! rout excess water down to next layer down
! and repeat calculation. Net gain in liquid (mass) is
! translated into a density increase:
!
ZFLOWLIQ(:,:)  = 0.0                ! clear this array for work
PSNOWLIQ(:,:)  = ZSNOWLIQ(:,:)      ! reset liquid water content
!
DO JJ=1,INLVLS
   DO JI=1,INI
      ZSNOWLIQ(JI,JJ)  = ZSNOWLIQ(JI,JJ) + ZFLOWLIQT(JI,JJ-1)
      ZFLOWLIQ(JI,JJ)  = MAX(0.0, ZSNOWLIQ(JI,JJ)-ZWHOLDMAX(JI,JJ))
      ZSNOWLIQ(JI,JJ)  = ZSNOWLIQ(JI,JJ) - ZFLOWLIQ(JI,JJ)
      ZFLOWLIQT(JI,JJ) = ZFLOWLIQT(JI,JJ) + ZFLOWLIQ(JI,JJ)
   ENDDO
ENDDO
!
ZWORK    (:,:) = MAX(XSNOWDMIN/INLVLS,ZSNOWDZ(:,:))
ZSNOWRHO (:,:) = ZSNOWRHO(:,:)+(ZSNOWLIQ(:,:)-PSNOWLIQ(:,:))*XRHOLW/ZWORK(:,:)  
ZSCAP    (:,:) = SNOW3LSCAP(ZSNOWRHO(:,:))
ZSNOWTEMP(:,:) = XTT +(((ZSNOWHEAT(:,:)/ZWORK(:,:))+XLMTT*ZSNOWRHO(:,:))/ZSCAP(:,:))
ZSNOWLIQ (:,:) = MAX(0.0,ZSNOWTEMP(:,:)-XTT)*ZSCAP(:,:)*ZSNOWDZ(:,:)/(XLMTT*XRHOLW)  
ZSNOWTEMP(:,:) = MIN(XTT,ZSNOWTEMP(:,:))
!
! Any remaining throughflow after freezing is available to
! the soil for infiltration or surface runoff (m).
! I.E. This is the amount of water leaving the snowpack:
! Rate water leaves the snowpack [kg/(m2 s)]:
!
PTHRUFAL(:)  = PTHRUFAL(:) + ZFLOWLIQT(:,INLVLS)
!
! Add excess rain (rain which flowed directly through the snow
! due to saturation):
!
PTHRUFAL(:)  = (PTHRUFAL(:) + ZPCPXS(:))*XRHOLW/PTSTEP
!
! 4. Update thickness and density and any freezing:
!    ----------------------------------------------
!
PSNOWTEMP(:,:)= ZSNOWTEMP(:,:)
PSNOWDZ(:,:)  = ZSNOWDZ(:,:)
PSNOWRHO(:,:) = ZSNOWRHO(:,:)
PSNOWLIQ(:,:) = ZSNOWLIQ(:,:)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LREFRZ
!####################################################################
!####################################################################
!####################################################################
      SUBROUTINE SNOW3LFLUX(PSNOWTEMP,PSNOWDZ,PEXNS,PEXNA,            &
                              PUSTAR2_IC,                             &
                              PTSTEP,PALBT,PSW_RAD,PEMIST,PLWUPSNOW,  &
                              PLW_RAD,PLWNETSNOW,                     &
                              PTA,PSFCFRZ,PQA,PHPSNOW,                &
                              PSNOWTEMPO1,PSNOWFLUX,PCT,PRADSINK,     &
                              PQSAT,PDQSAT,PRSRA,                     &
                              PRN,PH,PGFLUX,PLESES,PLELES,PEVAP,      &
                              PUSTAR,OSFCMELT                         )  
!
!
!!    PURPOSE
!!    -------
!     Calculate the surface fluxes (atmospheric/surface).
!     (Noilhan and Planton 1989; Noilhan and Mahfouf 1996)
!
!
USE MODD_CSTS_SNOWES,ONLY : XSTEFAN, XCPD, XLSTT, XLVTT, XTT
!
USE MODE_THERMOS_SNOW
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PSNOWDZ, PSNOWTEMPO1, PSNOWFLUX, PCT, &
                                         PRADSINK, PEXNS, PEXNA  
!
REAL, DIMENSION(:), INTENT(IN)      :: PALBT, PSW_RAD, PEMIST, PLW_RAD,        &
                                         PTA, PSFCFRZ, PQA,                    &
                                         PHPSNOW, PQSAT, PDQSAT, PRSRA,        &
                                         PUSTAR2_IC  
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PSNOWTEMP
!
REAL, DIMENSION(:), INTENT(OUT)     :: PRN, PH, PGFLUX, PLESES, PLELES,        &
                                         PEVAP, PLWUPSNOW, PUSTAR,             &  
                                         PLWNETSNOW
!
LOGICAL, DIMENSION(:), INTENT(OUT)  :: OSFCMELT
!
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWDZ))      :: ZEVAPC, ZLE, ZSNOWTEMP, ZSMSNOW, ZGFLUX,  &
                                         ZDELTAT, ZSNOWTO3  
!
!-------------------------------------------------------------------------------
!
! 0. Initialize:
! --------------
!
ZSNOWTEMP(:)  = PSNOWTEMP(:)
ZLE(:)        = 0.0
ZSMSNOW(:)    = 0.0
ZGFLUX(:)     = 0.0
!
OSFCMELT(:)   = .FALSE.
!
ZSNOWTO3(:)   = PSNOWTEMPO1(:) ** 3  ! to save some CPU time, store this
!
! 1. Flux calculations when melt not occuring at surface (W/m2):
! --------------------------------------------------------------
!
!
ZDELTAT(:)   = PSNOWTEMP(:) - PSNOWTEMPO1(:)   ! surface T time change:
!
PLWUPSNOW(:) = PEMIST(:) * XSTEFAN * ZSNOWTO3(:)*( PSNOWTEMPO1(:) + 4.* ZDELTAT(:) )
!
PLWNETSNOW(:)= PEMIST(:) * PLW_RAD(:) -  PLWUPSNOW(:)
!
PRN(:)       = (1. - PALBT(:)) * PSW_RAD(:) + PLWNETSNOW(:)
!
PH(:)        = PRSRA(:) * XCPD * (PSNOWTEMP(:)/PEXNS(:) - PTA(:)/PEXNA(:))
!
ZEVAPC(:)    = PRSRA(:) * ( (PQSAT(:) - PQA(:)) + PDQSAT(:)*ZDELTAT(:) )
!
PLESES(:)    = PSFCFRZ(:)     * XLSTT * ZEVAPC(:)
!
PLELES(:)    = (1.-PSFCFRZ(:))* XLVTT * ZEVAPC(:)
!
ZLE(:)       = PLESES(:) + PLELES(:)
!
PGFLUX(:)    = PRN(:) - PH(:) - ZLE(:) + PHPSNOW(:)
!
!
! 2. Initial melt adjustment
! --------------------------
! If energy avalabile to melt snow, then recalculate fluxes
! at the freezing point and add residual heat to layer
! average heat.
!
! A) If temperature change is > 0 and passes freezing point this timestep,
!    then recalculate fluxes at freezing point and excess energy
!    will be used outside of this routine to change snow heat content:
!
WHERE (PSNOWTEMP > XTT .AND. PSNOWTEMPO1 < XTT)
!
   OSFCMELT(:)= .TRUE.
!
   ZDELTAT(:)    = XTT - PSNOWTEMPO1(:)
!
   PLWUPSNOW(:) = PEMIST(:) * XSTEFAN * ZSNOWTO3(:)*( PSNOWTEMPO1(:) + 4.* ZDELTAT(:) ) 
!
   PLWNETSNOW(:)= PEMIST(:) * PLW_RAD(:) -  PLWUPSNOW(:)
!
   PRN(:)       = (1. - PALBT(:)) * PSW_RAD(:) + PLWNETSNOW(:)
!
   PH(:)        = PRSRA(:) * XCPD * (XTT/PEXNS(:) - PTA(:)/PEXNA(:))   
!
   ZEVAPC(:)    = PRSRA(:) * ( (PQSAT(:) - PQA(:)) + PDQSAT(:)*ZDELTAT(:) )
!
   PLESES(:)    = PSFCFRZ(:)     * XLSTT * ZEVAPC(:)
!
   PLELES(:)    = (1.-PSFCFRZ(:))* XLVTT * ZEVAPC(:)
!
   ZLE(:)       = PLESES(:) + PLELES(:)
!
   ZGFLUX(:)    = PRN(:) - PH(:) - ZLE(:) + PHPSNOW(:)
!
   ZSMSNOW(:)   = PGFLUX(:) - ZGFLUX(:)
!
   PGFLUX(:)  = ZGFLUX(:)
!
! This will be used to change heat content of snow:
!
   ZSNOWTEMP(:) = PSNOWTEMP(:) - ZSMSNOW(:)*PTSTEP*PCT(:)
!
END WHERE
!
! 3. Ongoing melt adjustment: explicit solution
! ---------------------------------------------
!    If temperature change is 0 and at freezing point this timestep,
!    then recalculate fluxes and surface temperature *explicitly*
!    as this is *exact* for snow at freezing point (Brun, Martin)
!
WHERE(PSNOWTEMP(:) > XTT .AND. PSNOWTEMPO1(:) >= XTT)
!
   OSFCMELT(:)  = .TRUE.   
!
   PLWUPSNOW(:) = PEMIST(:) * XSTEFAN * (XTT ** 4) 
!
   PLWNETSNOW(:)= PEMIST(:) * PLW_RAD(:) -  PLWUPSNOW(:)
!
   PRN(:)       = (1. - PALBT(:)) * PSW_RAD(:) + PLWNETSNOW(:)
!
   PH(:)        = PRSRA(:) * XCPD * (XTT/PEXNS(:) - PTA(:)/PEXNA(:))
!
   ZEVAPC(:)    = PRSRA(:) * (PQSAT(:) - PQA(:))
!
   PLESES(:)    = PSFCFRZ(:)     * XLSTT * ZEVAPC(:)
!
   PLELES(:)    = (1.-PSFCFRZ(:))* XLVTT * ZEVAPC(:)
!
   ZLE(:)       = PLESES(:) + PLELES(:)
!
   PGFLUX(:)    = PRN(:) - PH(:) - ZLE(:) + PHPSNOW(:)
!
   ZSNOWTEMP(:) = XTT + PTSTEP*PCT(:)*(PGFLUX(:) + PRADSINK(:) - PSNOWFLUX(:))
!
END WHERE
!
! 4. Update surface temperature:
! ------------------------------
!
PSNOWTEMP(:) = ZSNOWTEMP(:)
!
! 5. Final evaporative flux (kg/m2/s)
!
PEVAP(:)     = ZEVAPC(:)
!
! 6. Friction velocity
! --------------------
!
PUSTAR(:) = SQRT(PUSTAR2_IC(:)) 
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LFLUX
!####################################################################
!####################################################################
!####################################################################
      SUBROUTINE SNOW3LEVAPN(PPSNES,PLESES,PLELES,PTSTEP,PSNOWTEMP, &
                               PSNOWRHO,PSNOWDZ,PSNOWLIQ,PTA,       &
                               PSNOWHEAT,PSOILCOR                   )
!
!
!!    PURPOSE
!!    -------
!     Remove mass from uppermost snow layer in response to
!     evaporation (liquid) and sublimation.
!
!
USE MODD_CSTS_SNOWES,     ONLY : XLVTT, XRHOLW, XLSTT, XLMTT, XCI, XTT
USE MODD_SNOWES_PAR, ONLY : XRHOSMIN_ES, XSNOWDMIN
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PPSNES 
!
REAL, DIMENSION(:), INTENT(IN)      :: PLESES, PLELES   ! (W/m2)
!
REAL, DIMENSION(:), INTENT(IN)      :: PTA
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWHEAT, PSNOWDZ
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PSNOWRHO, PSNOWLIQ, &
                                       PSNOWTEMP
!
REAL, DIMENSION(:), INTENT(OUT)     :: PSOILCOR
!
!*      0.2    declarations of local variables
!
INTEGER                             :: INI, INLVLS, JJ, JI
!
REAL, DIMENSION(SIZE(PLESES))       :: ZSNOWEVAPS, ZSNOWEVAP, ZSNOWEVAPX,  &
                                       ZSNOWDZ, ZSNOWHEAT, ZSCAP, ZSNOWTEMP
!
REAL, DIMENSION(SIZE(PLESES))       :: ZXSE, ZISNOWD

!*      0.3    declarations of local parameters

REAL, PARAMETER                     :: ZSNOWDEMIN = 1.E-4 ! m
REAL, PARAMETER                     :: ZTDIF      = 15.   ! K To prevent a possible
                                                          !   decoupling of sfc snow T
                                                          !   when vanishingly thin, impose
                                                          !   this max T-diff based on obs...
                                                          !   between Ta-Ts
!
!
!
!-------------------------------------------------------------------------------
!
! 0. Initialize:
! --------------
!
!
PSOILCOR(:)    = 0.0
!
ZSNOWEVAPS(:)  = 0.0
ZSNOWEVAP(:)   = 0.0
ZSNOWEVAPX(:)  = 0.0
ZSNOWDZ(:)     = 0.0
ZSCAP(:)       = 0.0
ZSNOWHEAT(:)   = 0.0
ZSNOWTEMP(:)   = 0.0
ZXSE(:)        = 0.0
!
INI            = SIZE(PSNOWDZ(:,:),1)
INLVLS         = SIZE(PSNOWDZ(:,:),2)
!
!
! 1. Evaporation of snow liquid water
! -----------------------------------
! Evaporation reduces liq water equivalent content
! of snow pack either by reducing the snow density
! (evaporation) or the layer thickness (sublimation).
! Condensation does the reverse.
!
! Multiply evaporation components by snow fraction
! to be consistent with fluxes from the snow covered
! fraction of grid box
!
WHERE(PSNOWDZ(:,1) > 0.0)
!
! Evaporation:
! Reduce density and liquid water content:
!
   ZSNOWEVAP(:)   = PPSNES(:)*PLELES(:)*PTSTEP/(XLVTT*XRHOLW)
   ZSNOWEVAPX(:)  = MIN(ZSNOWEVAP(:),PSNOWLIQ(:))
!
!  This should not change enthalpy (since already accounted for 
!  in sfc e budget), so make density change insuring constant enthalpy:
!
   PSNOWLIQ(:)    = PSNOWLIQ(:) - ZSNOWEVAPX(:)
   PSNOWRHO(:) = (PSNOWHEAT(:,1)-XLMTT*XRHOLW*PSNOWLIQ(:))/ &
                 (PSNOWDZ(:,1)*(XCI*(PSNOWTEMP(:)-XTT)-XLMTT))
!
! Budget check: If density drops below minimum, then extract the
! corresponding water mass from soil (for vanishingly thin snow covers):
!
   PSOILCOR(:)    = MAX(0.0,XRHOSMIN_ES-PSNOWRHO(:))*PSNOWDZ(:,1)/PTSTEP
   PSNOWRHO(:)    = MAX(XRHOSMIN_ES,PSNOWRHO(:))
!
END WHERE
!
! 2. Update heat capacity:
! ---------------------------
!
ZSCAP(:) = SNOW3LSCAP(PSNOWRHO(:))
!
!
! 3. Sublimation of snow ice
! ---------------------------
!
WHERE(PSNOWDZ(:,1) > 0.0)

! Budget check: as last traces of liquid in snow evaporates, it is possible
! evaporation could exceed liquid present in snow. If this is the case,
! remove residual mass from solid snow in order to maintain high
! accuracy water balance:

   ZSNOWEVAPX(:)  = MAX(0.0, ZSNOWEVAP(:) - ZSNOWEVAPX(:))
   ZSNOWDZ(:)     = PSNOWDZ(:,1) - ZSNOWEVAPX(:)*XRHOLW/PSNOWRHO(:)
   PSNOWDZ(:,1)   = MAX(0.0, ZSNOWDZ(:))
   PSOILCOR(:)    = PSOILCOR(:) + MAX(0.0,-ZSNOWDZ(:))*PSNOWRHO(:)/PTSTEP
   
! Sublimation: Reduce layer thickness and total snow depth
! if sublimation: add to correction term if potential
! sublimation exceeds available snow cover.
!
   ZSNOWEVAPS(:)  = PPSNES(:)*PLESES(:)*PTSTEP/(XLSTT*PSNOWRHO(:))
   ZSNOWDZ(:)     = PSNOWDZ(:,1) - ZSNOWEVAPS(:)
   PSNOWDZ(:,1)   = MAX(0.0, ZSNOWDZ(:))
   PSOILCOR(:)    = PSOILCOR(:) + MAX(0.0,-ZSNOWDZ(:))*PSNOWRHO(:)/PTSTEP

! Note that the effect on enthalpy is already accounted for via the surface energy
! budget, so the change in enthalpy
! here owing to a mass loss must be offset by possible adjustments to T and Wl
! to conserve Enthalpy.

   PSNOWTEMP(:)   = XTT + ( ((PSNOWHEAT(:,1)/MAX(ZSNOWDEMIN,PSNOWDZ(:,1)))   &
                    + XLMTT*PSNOWRHO(:))/ZSCAP(:) )

   PSNOWLIQ(:)    = MAX(0.0,PSNOWTEMP(:)-XTT)*ZSCAP(:)*                  &
                    PSNOWDZ(:,1)/(XLMTT*XRHOLW)  

   PSNOWTEMP(:)   = MIN(XTT,PSNOWTEMP(:)) 

! For vanishingly thin snow, a decoupling can occur between forcing level T
! and snow sfc T (as snowpack vanishes owing to sublimation), so a simple limit
! imposed on this gradient:

   PSNOWTEMP(:)   = MAX(MIN(XTT,PTA(:)-ZTDIF), PSNOWTEMP(:))

! Update surface enthalpy:

   ZSNOWHEAT(:)   = PSNOWHEAT(:,1)
   PSNOWHEAT(:,1) = PSNOWDZ(:,1)*( ZSCAP(:)*(PSNOWTEMP(:)-XTT)             &
                    - XLMTT*PSNOWRHO(:) ) + XLMTT*XRHOLW*PSNOWLIQ(:) 

   ZXSE(:)        = PSNOWHEAT(:,1) - ZSNOWHEAT(:) ! excess cooling (removed from sfc)
 
END WHERE
!
! If the sfc T-Gradient was limited (and thus the enthalpy in the uppermost layer changed),
! distribute this energy correction (cooling) in the layers below (thickness-based weighting)
! to conserve total snowpack enthalpy. Normally this only occurs for this snowpacks,
! but it can rarely occur otherwise also.
!
ZISNOWD(:) = 0.
DO JJ=2,INLVLS
   DO JI=1,INI
      ZISNOWD(JI) = ZISNOWD(JI) + PSNOWDZ(JI,JJ)        ! m
   ENDDO
END DO
ZISNOWD(:)        = ZXSE(:)/MAX(ZISNOWD(:),ZSNOWDEMIN)  ! J kg-1 m-1 
!
DO JJ=2,INLVLS
   DO JI=1,INI
      PSNOWHEAT(JI,JJ) = PSNOWHEAT(JI,JJ) - PSNOWDZ(JI,JJ)*ZISNOWD(JI)
   ENDDO
ENDDO
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LEVAPN
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOW3LGONE(PTSTEP,PLELES,PLESES,PSNOWRHO,         &
                   PSNOWHEAT,PRADSINK,PEVAPCOR,PTHRUFAL,PGRNDFLUX,    &
                   PGFLUXSNOW,PGRNDFLUXO,PSNOWDZ,PSNOWLIQ,PSNOWTEMP,  &
                   PRADXS)  
!
!
!
!!    PURPOSE
!!    -------
!     Account for the case when the last trace of snow melts
!     during a time step: ensure mass and heat balance of
!     snow AND underlying surface.
!
!
USE MODD_CSTS_SNOWES,        ONLY : XTT, XLSTT, XLVTT
USE MODD_SNOWES_PAR, ONLY : XSNOWDZMIN
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PLELES, PLESES, PGFLUXSNOW, &
                                       PRADSINK, PGRNDFLUXO  
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWRHO, PSNOWHEAT
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PGRNDFLUX, PRADXS
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWDZ, PSNOWLIQ, PSNOWTEMP
!
REAL, DIMENSION(:), INTENT(OUT)     :: PTHRUFAL   ! melt water [kg/(m2 s)]
!
REAL, DIMENSION(:), INTENT(OUT)     :: PEVAPCOR   ! [kg/(m2 s)]
!                                      PEVAPCOR = for vanishingy thin snow cover,
!                                                 allow any excess evaporation
!                                                 to be extracted from the soil
!                                                 to maintain an accurate water
!                                                 balance.
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ, JI
!
INTEGER                             :: INI
INTEGER                             :: INLVLS
!
REAL, DIMENSION(SIZE(PLESES))       :: ZSNOWHEATC, ZSNOWGONE_DELTA, ZSNOW
!
!-------------------------------------------------------------------------------
!
! 0. Initialize:
! --------------
!
!
INI                   = SIZE(PSNOWDZ(:,:),1)
INLVLS                = SIZE(PSNOWDZ(:,:),2)
!
PEVAPCOR(:)           = 0.0
PTHRUFAL(:)           = 0.0
!
ZSNOWHEATC(:)         = 0.
ZSNOW(:)              = 0.
DO JJ=1,INLVLS
   DO JI=1,INI
      ZSNOWHEATC(JI) = ZSNOWHEATC(JI) + PSNOWHEAT(JI,JJ) ! total heat content (J m-2)
      ZSNOW(JI)      = ZSNOW(JI)      + PSNOWDZ(JI,JJ)   ! total snow depth (m)
   ENDDO
ENDDO
ZSNOWGONE_DELTA(:)    = 1.0
!
! 1. Simple test to see if snow vanishes:
! ---------------------------------------
! If so, set thicknesses (and therefore mass and heat) and liquid content
! to zero, and adjust fluxes of water, evaporation and heat into underlying
! surface. Note, test with flux computed *before* correction since this represents
! actual inflow of heat from below (as heat content correction owing to a corrected
! flux has not yet been done: here we compare to pre-corrected heat content).
!
WHERE(PGFLUXSNOW(:) + PRADSINK(:) >= (-ZSNOWHEATC(:)/PTSTEP) )
   PGRNDFLUX(:)       = PGFLUXSNOW(:) + (ZSNOWHEATC(:)/PTSTEP)
   PEVAPCOR(:)        = (PLELES(:)/XLVTT) + (PLESES(:)/XLSTT)
   PRADXS(:)          = 0.0
   ZSNOWGONE_DELTA(:) = 0.0          ! FLAG...if=0 then snow vanishes, else=1
END WHERE
!

IF(PEVAPCOR(1)>0.) write(*,*)  'EVAPCOR POS', PEVAPCOR(1)
IF(PEVAPCOR(1)<0.) write(*,*)  'EVAPCOR NEG', PEVAPCOR(1)

DO JJ=1,INLVLS
   DO JI=1,INI
      PTHRUFAL(JI) = PTHRUFAL(JI) + (1.0-ZSNOWGONE_DELTA(JI))*PSNOWRHO(JI,JJ)*PSNOWDZ(JI,JJ)/PTSTEP
   ENDDO
END DO
!
! 2. Final update of snow state
! -----------------------------
! (either still present or not):
!
DO JJ=1,INLVLS
   DO JI=1,INI
      PSNOWDZ  (JI,JJ) =                                 PSNOWDZ  (JI,JJ)*ZSNOWGONE_DELTA(JI)
      PSNOWLIQ (JI,JJ) =                                 PSNOWLIQ (JI,JJ)*ZSNOWGONE_DELTA(JI)
      PSNOWTEMP(JI,JJ) = (1.0-ZSNOWGONE_DELTA(JI))*XTT + PSNOWTEMP(JI,JJ)*ZSNOWGONE_DELTA(JI)
   ENDDO
ENDDO
!
!
END SUBROUTINE SNOW3LGONE
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOW3LEVAPGONE(PSNOWHEAT,PSNOWDZ,PSNOWRHO,PSNOWTEMP,PSNOWLIQ,   &
                          PSNOWGRAN1,PSNOWGRAN2,HSNOWMETAMO )
!
!!    PURPOSE
!!    -------
!
!     If all snow in uppermost layer evaporates/sublimates, re-distribute
!     grid (below assumes very thin snowpacks so layer-thicknesses are
!     constant).
!
!
USE MODD_CSTS_SNOWES,     ONLY : XTT, XRHOLW, XLMTT
USE MODD_SNOWES_PAR, ONLY : XRHOSMIN_ES, XSNOWDMIN, XRHOSMAX_ES
USE MODD_SNOW_METAMO
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWRHO   ! snow density profile                (kg/m3)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWDZ    ! snow layer thickness profile        (m)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWHEAT  ! snow heat content/enthalpy          (J/m2)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWTEMP  ! snow temperature profile            (K)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWLIQ   ! snow liquid water profile           (m)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWGRAN1 ! snow grain parameter 1              (-)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWGRAN2 ! snow grain parameter 2              (-)
CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO ! metamorphism scheme
!
!*      0.2    declarations of local variables
!
INTEGER                               :: JJ, JI
!
INTEGER                               :: INI
INTEGER                               :: INLVLS
!
REAL, DIMENSION(SIZE(PSNOWDZ,1))      :: ZSNOWHEAT_1D ! total heat content                (J/m2)
REAL, DIMENSION(SIZE(PSNOWDZ,1))      :: ZSNOW        ! total snow depth                  (m)
REAL, DIMENSION(SIZE(PSNOWDZ,1))      :: ZMASS        ! total mass
REAL, DIMENSION(SIZE(PSNOWDZ,1))      :: ZNDENT       ! Number of dendritic layers        (-)
REAL, DIMENSION(SIZE(PSNOWDZ,1))      :: ZNVIEU       ! Number of non dendritic layers    (-)
!
REAL, DIMENSION(SIZE(PSNOWDZ,1),SIZE(PSNOWDZ,2)) :: ZSCAP  ! Snow layer heat capacity          (J/K/m3)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWGRAN1N, ZSNOWGRAN2N
!
LOGICAL :: GDENDRITIC
!
!-------------------------------------------------------------------------------
!
! Initialize:
!
INI             = SIZE(PSNOWDZ,1)
INLVLS          = SIZE(PSNOWDZ,2)
!
! First, determine where uppermost snow layer has completely
! evaporated/sublimated (as it becomes thin):
!
ZSNOWHEAT_1D(:) = 0.
ZSNOW(:)        = 0.
ZMASS(:)        = 0.
ZNDENT(:)       = 0.
ZNVIEU(:)       = 0.
!
ZSCAP(:,:) = SNOW3LSCAP(PSNOWRHO(:,:))
!
DO JJ=2,INLVLS
   DO JI=1,INI
      IF(PSNOWDZ(JI,1) == 0.0)THEN
         ZSNOWHEAT_1D(JI) = ZSNOWHEAT_1D(JI) + XLMTT*XRHOLW*PSNOWLIQ(JI,JJ)  &
                          + PSNOWDZ(JI,JJ)*(ZSCAP(JI,JJ)*(PSNOWTEMP(JI,JJ)-XTT) &
                          - XLMTT*PSNOWRHO(JI,JJ) )           
         ZSNOW       (JI) = ZSNOW(JI) + PSNOWDZ(JI,JJ)
         ZMASS       (JI) = ZMASS(JI) + PSNOWDZ(JI,JJ)*PSNOWRHO(JI,JJ)    
         GDENDRITIC       = (PSNOWGRAN1(JI,JJ)<XVDIAM6*(4.-PSNOWGRAN2(JI,JJ))-XUEPSI )
         IF ( GDENDRITIC ) THEN   ! Dendritic snow
           ZNDENT(JI) = ZNDENT(JI) + 1.0
         ELSE                                    ! Non dendritic snow
           ZNVIEU(JI) = ZNVIEU(JI) + 1.0
         ENDIF
       ENDIF
    ENDDO
ENDDO
!
! Where uppermost snow layer has vanished, redistribute vertical
! snow mass and heat profiles (and associated quantities):
!
CALL SNOW3LAVGRAIN(PSNOWGRAN1,PSNOWGRAN2,                 &
                    ZSNOWGRAN1N,ZSNOWGRAN2N,ZNDENT,ZNVIEU,&
                    HSNOWMETAMO)

!
DO JJ=1,INLVLS
   DO JI=1,INI
      IF(ZSNOW(JI)/= 0.0)THEN
        ZSNOW    (JI)    = MAX(0.5*XSNOWDMIN,ZSNOW(JI))
        PSNOWDZ  (JI,JJ) = ZSNOW(JI)/REAL(INLVLS)
        PSNOWHEAT(JI,JJ) = ZSNOWHEAT_1D(JI)/REAL(INLVLS)
        PSNOWRHO (JI,JJ) = ZMASS (JI)/ZSNOW(JI)
      ENDIF
    ENDDO
ENDDO        
!
ZSCAP(:,:) = SNOW3LSCAP(PSNOWRHO(:,:))
!
DO JJ=1,INLVLS
   DO JI=1,INI
      IF(ZSNOW(JI)/= 0.0)THEN
        PSNOWTEMP(JI,JJ) = XTT + ( ((PSNOWHEAT(JI,JJ)/PSNOWDZ(JI,JJ))   &
                               + XLMTT*PSNOWRHO(JI,JJ))/ZSCAP(JI,JJ) )  
        PSNOWLIQ (JI,JJ) = MAX(0.0,PSNOWTEMP(JI,JJ)-XTT)*ZSCAP(JI,JJ)*  &
                                   PSNOWDZ(JI,JJ)/(XLMTT*XRHOLW)  
        PSNOWTEMP(JI,JJ) = MIN(XTT,PSNOWTEMP(JI,JJ))
      ENDIF
    ENDDO
ENDDO
!
END SUBROUTINE SNOW3LEVAPGONE
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOW3LEBUDMEB(PTSTEP, PSNOWDZMIN,                        &
           PTS, PSNOWDZ1, PSNOWDZ2, PSCOND1, PSCOND2, PSCAP,        &
           PSWNETSNOWS, PLWNETSNOW,                                 &
           PHSNOW, PLESES, PLELES, PHPSNOW,                         &
           PCT, PTSTERM1, PTSTERM2, PGFLUXSNOW                      )                          
!
!!    PURPOSE
!!    -------
!     Calculate surface energy budget with surface fluxes imposed.
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,    INTENT(IN)               :: PTSTEP, PSNOWDZMIN
!
REAL, DIMENSION(:), INTENT(IN)    :: PTS, PSNOWDZ1, PSNOWDZ2, PSCOND1, PSCOND2, PSCAP,  &
                                     PHSNOW, PLESES, PLELES, PHPSNOW,                   &
                                     PSWNETSNOWS, PLWNETSNOW
!
REAL, DIMENSION(:), INTENT(OUT)   :: PCT, PTSTERM1, PTSTERM2, PGFLUXSNOW
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTS))        :: ZSCONDA, ZA, ZB, ZC,             &
                                     ZSNOWDZM1, ZSNOWDZM2
!
!-------------------------------------------------------------------------------
!
!
! Calculate surface energy budget components:
! ---------------------------------------------------------
! To prevent numerical difficulties for very thin snow
! layers, limit the grid "thinness": this is important as
! layers become vanishing thin:
!
ZSNOWDZM1(:)  = MAX(PSNOWDZ1(:), PSNOWDZMIN)
ZSNOWDZM2(:)  = MAX(PSNOWDZ2(:), PSNOWDZMIN)
!
! Surface thermal inertia:
!
PCT(:)        = 1.0/(PSCAP(:)*ZSNOWDZM1(:))
!
! Surface fluxes entering the snowpack (radiative and turbulent):
!
PGFLUXSNOW(:) = PSWNETSNOWS(:) + PLWNETSNOW(:) - PHSNOW(:) - PLESES(:) - PLELES(:)
!
! Thermal conductivity between uppermost and lower snow layers:
!
ZSCONDA(:)    = (ZSNOWDZM1(:)+ZSNOWDZM2(:))/                           &
               ((ZSNOWDZM1(:)/PSCOND1(:)) + (ZSNOWDZM2(:)/PSCOND2(:)))
!
!
! Energy budget solution terms (with surface flux imposed):
!
ZB(:)         = 1./PTSTEP 
!
ZA(:)         = ZB(:) + PCT(:)*(2*ZSCONDA(:)/(ZSNOWDZM2(:)+ZSNOWDZM1(:)))
!
ZC(:)         = PCT(:)*( PGFLUXSNOW(:) + PHPSNOW(:) )
!
! Coefficients needed for implicit solution
! of linearized surface energy budget:
!
PTSTERM2(:)   = 2*ZSCONDA(:)*PCT(:)/(ZA(:)*(ZSNOWDZM2(:)+ZSNOWDZM1(:)))
!
PTSTERM1(:)   = (PTS(:)*ZB(:) + ZC(:))/ZA(:)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LEBUDMEB
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROMETAMO(PSNOWDZ,PSNOWGRAN1, PSNOWGRAN2,         &
                         PSNOWHIST, PSNOWTEMP, PSNOWLIQ, PTSTEP, &
                         PSNOWSWE, PSNOWAGE, HSNOWMETAMO,PSNOWRHO)
!                               

!**** *METAMO* - METAMORPHOSE DES GRAINS
!              - SNOW METAMORPHISM
!     OBJET.
!     ------
!     METAMORPHOSE DU MANTEAU NEIGEUX.
!     EVOLUTION DU TYPE DE GRAINS 
!     MISE A JOUR DES VARIABLES HISTORIQUES.
!     METAMORPHISM OF THE SNOW GRAINS,
!     HISTORICAL VARIABLES

!**   INTERFACE.
!     ----------
!     FORMALISME ADOPTE POUR LA REPRESENTATION DES GRAINS :
!     FORMALISM FOR THE REPRESENTATION OF GRAINS
!     -----------------------------------------------------


!                    1       - -1                 NEIGE FRAICHE
!                   / \      |                    -------------
!                  /   \     |  DENDRICITE        DECRITE EN TERME
!                 /     \    |  DENDRICITY        DE DENDRICITE ET
!                /       \   |                    SPHERICITE
!               2---------3  -  0                 DESCRIBED WITH
!                                                 SPHERICITY AND
!               |---------|                       DENDRICITY
!               0         1
!               SPHERICITE
!               SPHERICITY

!               4---------5  -
!               |         |  |
!               |         |  | DIAMETRE  (OU TAILLE)
!               |         |  | DIAMETER  (OR SIZE  )
!               |         |  |                 
!               |         |  |                   NEIGE NON DENDRITIQUE
!               6---------7  -                   ---------------------

!                                                SPHERICITE ET TAILLE
!                                                SPHERICITY AND SIZE 

!              LES VARIABLES DU MODELE : 
!              -------------------------
!              CAS DENDRITIQUE             CAS NON DENDRITIQUE
!  
!            SGRAN1(JST) : DENDRICITE      SGRAN1(JST) : SPHERICITE
!            SGRAN2(JST) : SPHERICITE      SGRAN2(JST) : TAILLE (EN METRE)
!                                                        SIZE

! 
!    CAS DENDRITIQUE/ DENDRITIC CASE
!    -------------------------------
!    SGRAN1(JST) VARIE DE -XVGRAN1 (-99 PAR DEFAUT) (ETOILE) A 0
!  (DENDRICITY)  >D OU LA DIVISION PAR -XVGRAN1 POUR OBTENIR DES VALEURS
!                 ENTRE 1 ET 0
!                 VARIES FROM -XVGRAN1 (DEFAULT -99) (FRESH SNOW) TO 0
!                 DIVISION BY -XVGRAN1 TO OBTAIN VALUES BETWEEN 0 AND 1

!    SGRAN2(JST) VARIE DE 0 (CAS COMPLETEMENT ANGULEUX) A XVGRAN1
!  (SPHERICITY)  (99 PAR DEFAUT)
!                >D OU LA DIVISION PAR XVGRAN1 POUR OBTENIR DES VALEURS
!                 ENTRE 0 ET 1
!                 VARIES FROM 0 (SPHERICITY=0) TO XVGRAN1


!    CAS NON DENDRITIQUE / NON DENDRITIC CASE
!    ---------------------------------------

!    SGRAN1(JST) VARIE DE 0 (CAS COMPLETEMENT ANGULEUX) A XVGRAN1
!  (SPHERICITY)  (99 PAR DEFAUT) (CAS SPHERIQUE)
!                >D OU LA DIVISION PAR XVGRAN1 POUR OBTENIR DES VALEURS
!                 ENTRE 0 ET 1
!                 VARIES FROM 0 TO 99

!    SGRAN2(JST) EST SUPERIEUR A XVDIAM1-SPHERICITE (3.E-4 M) ET NE FAIT QUE CROITRE
!     (SIZE)     IS GREATER THAN XVDIAM1-SPHERICITE (3.E-4 M) ALWAYS INCREASE


!    EXEMPLES : POINTS CARACTERISTIQUES DE LA FIGURE
!    --------

!                 SGRAN1     SGRAN2    DENDRICITE  SPHERICITE  TAILLE
!                                      DENDRICITY  SPHERICITY  SIZE
!      --------------------------------------------------------------
!                                                               (M)
!        1        -XVGRAN1    VNSPH3        1           0.5
!        2           0         0           0            0
!        3           0       XVGRAN1        0            1
!        4           0       XVDIAM1                     0        4.E-4 
!        5         XVGRAN1    XVDIAM1-XVSPHE1              1        3.E-4
!        6           0         --                       0        --
!        7         XVGRAN1      --                       1        --

!     PAR DEFAUT : XVGRAN1 =99   VNSPH3=50 XVSPHE1=1. XVDIAM1=4.E-4


!     METHODE.
!     --------
!     EVOLUTION DES TYPES DE GRAINS : SELON LES LOIS DECRITES 
!     DANS BRUN ET AL (1992)
!     PLUSIEURS CAS SONT A DISTINGUER
!      1.2 NEIGE HUMIDE
!      1.3 METAMORPHOSE NEIGE SECHE
!        1.3.1 FAIBLE GRADIENT
!        1.3.2 GRADIENT MOYEN
!        1.3.3 FORT GRADIENT
!     DANS CHAQUE CAS ON SEPARE NEIGE DENDRITIQUE ET NON DENDRITIQUE
!     LE PASSAGE DENDRITIQUE => NON DENDRITIQUE SE FAIT LORSQUE 
!     SGRAN1 DEVIENT > 0

!     TASSEMENT : LOIS DE VISCOSITE ADAPTEE SELON LE TYPE DE GRAINS

!     VARIABLES HISTORIQUES (CAS NON DENDRITIQUE SEULEMENT)

!     MSHIST DEFAUT
!        0           CAS NORMAL
!     NVHIS1   1     GRAINS ANGULEUX
!     NVHIS2   2     GRAINS AYANT ETE EN PRESENCE D EAU LIQUIDE 
!                    MAIS N'AYANT PAS EU DE CARATERE ANGULEUX 
!     NVHIS3   3     GRAINS AYANT ETE EN PRESENCE D EAU LIQUIDE
!                    AYANT EU AUPARAVANT UN CARACTERE ANGULEUX 

!     GRAIN METAMORPHISM ACCORDING TO BRUN ET AL (1992)
!     THE DIFFERENT CASES ARE :
!     1.2 WET SNOW
!     1.3 DRY SNOW
!       1.3.1. LOW      TEMPERATURE GRADIENT
!       1.3.2. MODERATE TEMPERATURE GRADIENT
!       1.3.3. HIGH     TEMPERATURE GRADIENTi
!     THE CASE OF DENTRITIC OR NON DENDRITIC SNOW IS TREATED SEPARATELY
!     THE LIMIT DENTRITIC ==> NON DENDRITIC IS REACHED WHEN SGRAN1>0

!     SNOW SETTLING : VISCOSITY DEPENDS ON THE GRAIN TYPES

!     HISTORICAL VARIABLES (NON DENDRITIC CASE)
!     MSHIST DEFAUT
!        0           CAS NORMAL
!     NVHIS1   1     FACETED CRISTAL
!     NVHIS2   2     LIQUID WATER AND NO FACETED CRISTALS BEFORE
!     NVHIS3   3     LIQUID WATER AND FACETED CRISTALS BEFORE

!     EXTERNES.
!     ---------

!     REFERENCES.
!     -----------

!     AUTEURS.
!     --------
!        ERIC BRUN ET AL. - JOURNAL OF GLACIOLOGY 1989/1992.

!     MODIFICATIONS.
!     --------------
!        08/95: YANNICK DANIELOU - CODAGE A LA NORME DOCTOR.
!        09/96: ERIC MARTIN      - CORRECTION COMMENTAIRES
!        03/06: JM Willemet      - F90 and SI units
!        08/06: JM Willemet      - new formulation for TEL (Mwat/(Mice+Mwat) instead of Mwat/Mice.
!                                  Threshold on the diameter increasing of the wet grains.
!        01/07 : JM Willemet     - CORRECTION DES COUCHES SATUREES SUBISSANT DU TASSEMENT
!                                  CORRECTION ON THE SATURATED LAYERS WHICH ARE SETTLED
!        12/12: CM Carmagnola    - Dendricity and size replaced by the optical diameter
!                                - Test of different evolution laws for the optical diameter
!        08/13: M Lafaysse       - Simplification of historical parameter computation (logicals GNONDENDRITIC, GFACETED, GSPHE_LW)
   !
USE MODD_SNOW_METAMO
USE MODD_CSTS_SNOWES, ONLY : XTT, XPI, XRHOLW, XRHOLI
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_SNOWES_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
USE MODE_SNOWES
!
IMPLICIT NONE
!
!     0.1 declarations of arguments  
!      
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWDZ, PSNOWTEMP, PSNOWLIQ, PSNOWSWE, PSNOWRHO
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST
!   
REAL, INTENT(IN)                    :: PTSTEP
! 
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWAGE
!
CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO ! metamorphism scheme
!
!     0.2 declaration of local variables      
!     
REAL :: ZGRADT, ZTELM, ZVDENT, ZDENT, ZSPHE, ZVAP, ZDANGL, &
        ZSIZE, ZSSA, ZSSA0, ZSSA_T, ZSSA_T_DT, ZA, ZB, ZC, &
        ZA2, ZB2, ZC2, ZOPTD, ZOPTR, ZOPTR0, ZDRDT
REAL :: ZVDENT1, ZVDENT2, ZVSPHE, ZCOEF_SPH
REAL :: ZDENOM1, ZDENOM2, ZFACT1, ZFACT2
INTEGER :: INLVLS,INI
INTEGER :: JST,JJ                                !Loop controls 
INTEGER :: IDRHO, IDGRAD, IDTEMP           !Indices for values from Flanner 2006
LOGICAL :: GNONDENDRITIC ,GFACETED, GSPHE_LW
LOGICAL :: GCOND_B92, GCOND_C13, GCOND_SPH
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHO
REAL :: ZHOLDMAXR
!
!
!     INITIALISATION
!     --------------
!
!
! Initialize:
!
INI             = SIZE(PSNOWDZ,1)
INLVLS          = SIZE(PSNOWDZ,2)
!INLVLS = SIZE(PSNOWGRAN1(:,:),2)  ! total snow layers
!
!*    1. METAMORPHOSES DANS LES STRATES. / METAMORPHISM
!        -----------------------------------------------
DO JJ = 1,INI
  !
  DO JST = 1,INLVLS
    !
    ! 1.1 INITIALISATION: GRADIENT DE TEMPERATURE / TEMPERATURE GRADIENT
    IF ( JST==INLVLS ) THEN
      ZGRADT = ABS(PSNOWTEMP(JJ,JST)   - PSNOWTEMP(JJ,JST-1))*2. / (PSNOWDZ(JJ,JST-1) + PSNOWDZ(JJ,JST))
    ELSEIF ( JST==1 ) THEN
      ZGRADT = ABS(PSNOWTEMP(JJ,JST+1) - PSNOWTEMP(JJ,JST)  )*2. / (PSNOWDZ(JJ,JST) + PSNOWDZ(JJ,JST+1))
    ELSE
      ZGRADT = ABS(PSNOWTEMP(JJ,JST+1) - PSNOWTEMP(JJ,JST-1))*2. / &
                                            (PSNOWDZ(JJ,JST-1) + PSNOWDZ(JJ,JST)*2. + PSNOWDZ(JJ,JST+1))
    ENDIF
    !
    IF ( PSNOWLIQ(JJ,JST)>XUEPSI ) THEN
      ! 1.2 METAMORPHOSE HUMIDE. / WET SNOW METAMORPHISM
      !
      ! TENEUR EN EAU LIQUIDE / LIQUID WATER CONTENT
      ZTELM  = XUPOURC * PSNOWLIQ(JJ,JST) * XRHOLW / PSNOWSWE(JJ,JST)
      !
      ! Evaluate capacity using upper density limit:
      !
      ZSNOWRHO(JJ,JST) = MIN(XRHOSMAX_ES, PSNOWRHO(JJ,JST))
      !
      ! Maximum ratio of liquid to SWE:
      !
      ZHOLDMAXR = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(JJ,JST))/XSNOWRHOHOLD
      ZTELM = MIN(100*ZHOLDMAXR,ZTELM)
      !
      ! VITESSES DE DIMINUTION DE LA DENDRICITE / RATE OF THE DENDRICITY DECREASE
      ZVDENT1 = MAX( XVDENT2 * ZTELM**NVDENT1, XVDENT1 * EXP(XVVAP1/XTT) )
      ZVDENT2 = ZVDENT1
      ! CONDITION POUR LE CAS NON DENDRITIQUE NON SPHERIQUE
      GCOND_B92  = ( PSNOWGRAN1(JJ,JST)<XVGRAN1-XUEPSI )
      GCOND_C13 = .TRUE. ! CONDITION POUR LE CALCUL DE SNOWGRAN1
      ! X COEF
      ZVSPHE = XVSPHE1
      ! FOR C13
      ZCOEF_SPH = 2.
      !
    ELSEIF ( ZGRADT<XVGRAT1 ) THEN
      ! 1.3.1 METAMORPHOSE SECHE FAIBLE/ DRY LOW GRADIENT (0-5 DEG/M).
      !
      ZVAP = EXP( XVVAP1/PSNOWTEMP(JJ,JST) )
      !
      ! VITESSES DE DIMINUTION DE LA DENDRICITE / RATE OF THE DENDRICITY DECREASE
      ZVDENT1 = XVDENT1 * ZVAP
      ZVDENT2 = XVSPHE2 * ZVAP
      ! CONDITION POUR LE CAS NON DENDRITIQUE SPHERICITE NON LIMITEE
      GCOND_B92  = ( PSNOWHIST(JJ,JST)/=NVHIS1 .OR. PSNOWGRAN2(JJ,JST)<XVDIAM2 )
      GCOND_C13 = ( HSNOWMETAMO=='C13' )  ! CONDITION POUR LE CALCUL DE SNOWGRAN1
      ! X COEF
      ZVSPHE = XVSPHE1
      ! FOR C13
      ZCOEF_SPH = 2.
      !
    ELSE
      ! 1.3.2 METAMORPHOSE SECHE GRADIENT MOYEN / DRY MODERATE (5-15).
      ! 1.3.3 METAMORPHOSE SECHE FORT / DRY HIGH GRADIENT
      !
      ZVAP = EXP( XVVAP1/PSNOWTEMP(JJ,JST) ) * (ZGRADT)**XVVAP2
      !
      ! VITESSES DE DIMINUTION DE LA DENDRICITE / RATE OF THE DENDRICITY DECREASE
      ZVDENT1 = XVDENT1 * ZVAP
      ZVDENT2 = - XVDENT1 * ZVAP
      ! CONDITION POUR LE CAS NON DENDRITIQUE NON COMPLETEMENT ANGULEUX
      GCOND_B92  = ( ZGRADT<XVGRAT2 .OR. PSNOWGRAN1(JJ,JST)>0. )
      GCOND_C13 = ( HSNOWMETAMO=='C13' ) ! CONDITION POUR LE CALCUL DE SNOWGRAN1
      ! X COEF
      ZVSPHE = XUNDEF
      ! FOR C13
      ZCOEF_SPH = 3.
      !
    ENDIF
    !
    IF ( HSNOWMETAMO=="B92" ) THEN
      !
      !------------------------------------------------
      !    BRUN et al. 1992 (B92)
      !
      ! -> Wet snow and dry snow 
      ! -> Evolution of dendricity, sphericity and size 
      !------------------------------------------------
      !
      IF ( PSNOWGRAN1(JJ,JST)<-XUEPSI ) THEN
        ! 1.2.1 CAS DENDRITIQUE/DENDRITIC CASE. 
        !
        ! / CALCUL NOUVELLE DENDRICITE ET SPHERICITE.
        ZDENT = - PSNOWGRAN1(JJ,JST)/XVGRAN1 - ZVDENT1 * PTSTEP
        ZSPHE =   PSNOWGRAN2(JJ,JST)/XVGRAN1 + ZVDENT2 * PTSTEP
        CALL SET_THRESH(ZGRADT,PSNOWLIQ(JJ,JST),ZSPHE)
        IF( ZDENT<=XUEPSI ) THEN
          ! EVOLUTION DE SGRAN1 ET SGRAN2 ET TEST PASSAGE DENDRITIQUE > NON DENDRITIQUE.
          PSNOWGRAN1(JJ,JST) =  ZSPHE * XVGRAN1
         PSNOWGRAN2(JJ,JST) = XVDIAM1 - XVDIAM5 * MIN( ZSPHE, ZVSPHE )
        ELSE
          PSNOWGRAN1(JJ,JST) = -ZDENT * XVGRAN1
          PSNOWGRAN2(JJ,JST) =  ZSPHE * XVGRAN1
        ENDIF
        !
      ELSEIF ( GCOND_B92 ) THEN
        ! 1.2.2 CAS NON DENDRITIQUE ET
        !             NON COMPLETEMENT SPHERIQUE / NON DENDRITIC AND NOT COMPLETELY SPHERIC CASE
        ! OU          SPHERICITE NON LIMITEE
        ! OU          NON COMPLETEMENT ANGULEUX
        !
        ! . EVOLUTION DE LA SPHERICITE SEULEMENT / EVOLUTION OF SPHERICITY ONLY (NOT SIZE)
        ZSPHE = PSNOWGRAN1(JJ,JST)/XVGRAN1 + ZVDENT2 * PTSTEP
        CALL SET_THRESH(ZGRADT,PSNOWLIQ(JJ,JST),ZSPHE)
        PSNOWGRAN1(JJ,JST) = ZSPHE * XVGRAN1
        !          
      ELSEIF ( PSNOWLIQ(JJ,JST)>XUEPSI ) THEN
        ! 1.2.3 CAS NON DENDRITIQUE ET SPHERIQUE/NON DENDRITIC AND SPHERIC EN METAMORPHOSE HUMIDE
        !
        ! EVOLUTION DE LA TAILLE SEULEMENT/EVOLUTION OF SIZE ONLY                  
        CALL GET_GRAN(PTSTEP,ZTELM,PSNOWGRAN2(JJ,JST))
        !
      ELSEIF ( ZGRADT<XVGRAT1 ) THEN
        ! 1.2.4. CAS HISTORIQUE=2 OU 3 ET GROS GRAINS SPHERICITE LIMITEE / CASE HISTORY=2 OR 3 AND BIG GRAINS LIMITED SPHERICITY 
        !
        ZSPHE =  PSNOWGRAN1(JJ,JST)/XVGRAN1 + &
                 ZVDENT2 * PTSTEP * EXP( MIN( 0., XVDIAM3-PSNOWGRAN2(JJ,JST) ) / XVDIAM6 )
        ZSPHE = MIN( ZSPHE, XVSPHE3 )
        CALL SET_THRESH(ZGRADT,PSNOWLIQ(JJ,JST),ZSPHE)
        PSNOWGRAN1(JJ,JST) = ZSPHE * XVGRAN1
        !
      ELSE
        ! 1.2.5. CAS NON DENDRITIQUE ET ANGULEUX/DENDRITIC AND SPERICITY=0.
        !
        ZDANGL = SNOW3L_MARBOUTY(PSNOWRHO(JJ,JST),PSNOWTEMP(JJ,JST),ZGRADT)
        PSNOWGRAN2(JJ,JST) = PSNOWGRAN2(JJ,JST) + ZDANGL * XVFI * PTSTEP
        !
      ENDIF
      !
    ELSE
      !
      !------------------------------------------------
      !    CARMAGNOLA et al. 2013 (C13)
      !
      ! -> Wet snow
      ! -> Evolution of optical diameter and sphericity
      !------------------------------------------------
      !
      ! SPHERICITY
      ZSPHE = PSNOWGRAN2(JJ,JST) + ZVDENT2 * PTSTEP
      CALL SET_THRESH(ZGRADT,PSNOWLIQ(JJ,JST),ZSPHE)
      IF ( PSNOWLIQ(JJ,JST)>XUEPSI .OR. ZGRADT<XVGRAT1 ) THEN
        GCOND_SPH = ( ZSPHE < 1.-XUEPSI )
      ELSE
        GCOND_SPH = ( ZSPHE > XUEPSI )
      ENDIF
      !      
      IF ( GCOND_C13 .AND. PSNOWGRAN1(JJ,JST)<XVDIAM6*(4.-ZSPHE)-XUEPSI ) THEN
        ! 1.1.1 CAS DENDRITIQUE/DENDRITIC CASE.
        !
        IF ( GCOND_SPH ) THEN
          PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST) + XVDIAM6 * PTSTEP * &
                               ( ZVDENT2*(PSNOWGRAN1(JJ,JST)/XVDIAM6-1.)/(ZSPHE-3.) - &
                                 ZVDENT1*(ZSPHE-3.) )
        ELSE
           PSNOWGRAN1(JJ,JST) =  PSNOWGRAN1(JJ,JST) + XVDIAM6 * PTSTEP * ZVDENT1 * ZCOEF_SPH
        ENDIF
        !
      ELSEIF ( GCOND_C13 .AND. GCOND_SPH ) THEN
        ! 1.2.2 CAS NON DENDRITIQUE ET
        !             NON COMPLETEMENT SPHERIQUE / NON DENDRITIC AND NOT COMPLETELY SPHERIC CASE
        ! OU          NON COMPLETEMENT ANGULEUX
        !        
        PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST) - XVDIAM6 * PTSTEP * ZVDENT2 * 2.* ZSPHE
        !
      ELSEIF ( PSNOWLIQ(JJ,JST)>XUEPSI ) THEN
        ! 1.2.3 CAS NON DENDRITIQUE ET SPHERIQUE/NON DENDRITIC AND SPHERIC EN METAMORPHOSE HUMIDE      
        !
        ! NON DENDRITIC AND SPHERIC: EVOLUTION OF SIZE ONLY 
        CALL GET_GRAN(PTSTEP,ZTELM,PSNOWGRAN1(JJ,JST))
        !
      ELSEIF ( GCOND_C13 .AND. ZGRADT>=XVGRAT2 ) THEN
        !
        ZDANGL = SNOW3L_MARBOUTY(PSNOWRHO(JJ,JST),PSNOWTEMP(JJ,JST),ZGRADT)
        PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST) + 0.5 * ZDANGL * XVFI * PTSTEP
        !
      ENDIF
      !
      PSNOWGRAN2(JJ,JST) = ZSPHE
      !
      !
    ENDIF
    !
  ENDDO
  !
ENDDO
!
!
END SUBROUTINE SNOWCROMETAMO
!
!####################################################################
!####################################################################
SUBROUTINE SET_THRESH(PGRADT,PSNOWLIQ,PSPHE)
!
USE MODD_SNOW_METAMO, ONLY : XUEPSI, XVGRAT1
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: PGRADT
REAL, INTENT(IN) :: PSNOWLIQ
REAL, INTENT(INOUT) :: PSPHE
!
IF ( PSNOWLIQ>XUEPSI .OR. PGRADT<XVGRAT1 ) THEN
  PSPHE = MIN(1.,PSPHE)
ELSE
  PSPHE = MAX(0.,PSPHE)
ENDIF
!
END SUBROUTINE SET_THRESH
!####################################################################
!####################################################################
SUBROUTINE GET_GRAN(PTSTEP,PTELM,PGRAN)
!
USE MODD_CSTS_SNOWES, ONLY : XPI
USE MODD_SNOW_METAMO, ONLY : XVTAIL1, XVTAIL2, NVDENT1
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: PTSTEP, PTELM
REAL, INTENT(INOUT) :: PGRAN
!
!
!
PGRAN = 2. * ( 3./(4.*XPI) * &
                  ( 4. * XPI/3. * (PGRAN/2.)**3 + &
                   ( XVTAIL1 + XVTAIL2 * PTELM**NVDENT1 ) * PTSTEP ) )**(1./3.)
!
!
END SUBROUTINE GET_GRAN
!  
!####################################################################
!####################################################################

!
!
!
END SUBROUTINE SNOWES

end module snowES_mod
