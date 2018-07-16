!>\file
!!The basic model structure of CTEM includes three live vegetation components
!!(leaf (L), stem (S) and root (R)) and two dead carbon pools (litter or
!!detritus (D) and soil carbon (H)). The amount of carbon in these pools
!!(\f$C_\mathrm{L}\f$, \f$C_\mathrm{S}\f$, \f$C_\mathrm{R}\f$, \f$C_\mathrm{D}\f$,
!!\f$C_\mathrm{H}\f$, \f$kgC m^{-2}\f$) is tracked prognostically through the
!!fluxes in and out of them. The rate change equations for carbon in these
!!pools are summarized in Sect. \ref{rate_change_eqns} after the processes
!!leading to the calculation of fluxes in and out of these pools are introduced
!!in the following sections.

      subroutine     ctem( fcancmx,    fsnow,     sand,      clay,  &
     &                      il1,       il2,      iday,      radj, &
     &                       tcano,    tcans,    tbarc,    tbarcs,    &
     &                       tbarg,   tbargs,       ta,     delzw,&
     &                     ancsveg,  ancgveg, rmlcsveg,  rmlcgveg,    &
     &                       zbotw,   thliqc,   thliqg,    deltat,&
     &                       uwind,    vwind,  lightng,  prbfrhuc, &
     &                    extnprob,   stdaln,     tbar,    popdon, &
     &                    nol2pfts, pfcancmx, nfcancmx,  lnduseon,&
     &                      thicec, thiceg,soildpth, spinfast,todfrac,&
     &                     compete,   netrad,   precip,  grclarea, &
     &                    popdin, dofire,  dowetlands,obswetf,isand,  &
     &                   faregat, onetile_perPFT, wetfrac, slopefrac,&
     &                   currlat,       THP,       BI,    PSIS, &
     &                   ch4conc,       GRAV,    RHOW,  RHOICE,&
     &                   leapnow, &
!
!    -------------- inputs used by ctem are above this line ---------
!
     &                    stemmass, rootmass, litrmass,  gleafmas,&
     &                    bleafmas, soilcmas,    ailcg,      ailc,&
     &                       zolnc, rmatctem,    rmatc,     ailcb,&
     &                    flhrloss,  pandays, lfstatus,  grwtheff,&
     &                    lystmmas, lyrotmas, tymaxlai,  vgbiomas,&
     &                    gavgltms, gavgscms, stmhrlos,      slai, &
     &                     bmasveg, cmasvegc, colddays,  rothrlos,&
     &                      fcanmx,   alvisc,   alnirc,   gavglai,&
!    ------- following 5 lines are competition related variables ----
     &                       tcurm, srpcuryr, dftcuryr,inibioclim,&
     &                      tmonth, anpcpcur,  anpecur,   gdd5cur,&
     &                    surmncur, defmncur, srplscur,  defctcur,&
     &                    geremort, intrmort,   lambda,  &
     &                    pftexist, twarmm,    tcoldm,       gdd5,&
     &                     aridity, srplsmon, defctmon,  anndefct,&
     &                    annsrpls,  annpcp,dry_season_length,&
     &                    burnvegf, pstemmass, pgleafmass,&
!
!    -------------- inputs updated by ctem are above this line ------
!
     &                        npp,       nep, hetrores,   autores,&
     &                   soilresp,        rm,       rg,       nbp,&
     &                     litres,    socres,      gpp, dstcemls1,&
     &                   litrfall,  humiftrs,  veghght,  rootdpth,&
     &                   litrfallveg,  humtrsvg, &
     &                        rml,       rms,      rmr,  tltrleaf,&
     &                   tltrstem,  tltrroot, leaflitr,  roottemp,&
     &                    afrleaf,   afrstem,  afrroot,  wtstatus,&
     &                   ltstatus,  burnfrac, smfunc_veg, lucemcom,&
     &                   lucltrin,  lucsocin,   nppveg,  &
     &                   dstcemls3, paicgat,  slaicgat,    &
     &                    emit_co2, emit_co,  emit_ch4, emit_nmhc,&
     &                    emit_h2,  emit_nox, emit_n2o, emit_pm25,&
     &                    emit_tpm, emit_tc,  emit_oc,    emit_bc,&
     &                  bterm_veg,    lterm, mterm_veg,        &
     &                         cc,       mm,&
     &                      rmlveg,  rmsveg,   rmrveg,    rgveg,&
     &                vgbiomas_veg,  gppveg,   nepveg,   nbpveg,&
     &                  hetrsveg,autoresveg, ltresveg, scresveg,&
     &                 nml,    ilmos, jlmos,  ch4wet1,  ch4wet2,  &
     &                 wetfdyn, ch4dyn1, ch4dyn2, ch4soills)
!
!    ---------------- outputs are listed above this line ------------ 
!
!             Canadian Terrestrial Ecosystem Model (CTEM) 
!             Main Ctem Subroutine Compatible With CLASS 

!     14  Mar 2016  - Remove grid cell area calculation to the driver. This will
!     J. Melton       harmonize this subroutine with the coupled code.
!
!     3   Feb 2016  - Bring in onetile_perPFT switch so now in mosaic mode the
!     J. Melton       tiles in a grid cell are treated independently by competition
!                     and LUC.
!
!     3   Jul 2014  - Bring in wetland and wetland methane code
!     R. Shrestha
!
!     12  Jun 2014  - Bring in a constant reproductive cost, remove expnbaln,
!     J. Melton       add a smoothing function for lambda calculation for competition,
!                     made it so NEP and NBP work with competition on.
!
!     17  Jan 2014  - Moved parameters to global file (ctem_params.f90)
!     J. Melton
!
!     Dec 6   2012   Make it so competition and luc can function in both
!     J. Melton      composite and mosaic modes.
!
!     sep 25  2012   Add competition_map and competition_unmap
!     Y. Peng   
!
!     Sep 12  2012
!     J. Melton     Add in bottom limit to bleafmas to ensure it does no
!                   slowly decay to infintesimaly small number.
!
!     Aug 23  2012
!     J. Melton     Pass in isand to ensure soil levels are properly
!                   labelled as bedrock if assigned so in classb
!
!     Jan 10  2012 
!     Yiran         Re-test bioclim and existence for competition
!
!     19  Sep. 2001 - This is the main terrestrial carbon model subrouti
!     V. Arora        
!                     all primary ctem subroutines are called from here,
!                     except phtsyn which is called from tsolvc
!
!     08 June  2001 - Add calls to three new subroutines (bioclim,
!     V. Arora        existence, and competition) to add dynamic
!                     competition between pfts. changes are also made 
!                     to land use change (luc) subroutine and the manner
!                     in which distrubance is handles. fire now creates
!                     bare ground which is subsequently available for
!                     colonization. other changes are also made to keep
!                     everything consistent with changing vegetation
!                     fractions. 
!    -----------------------------------------------------------------

use ctem_params,        only : kk, pi, zero,&
     &                         kn,iccp1, ican, ilg, nlat,&
     &                         ignd, icc, nmos, l2max, grescoef,&
     &                         humicfac,laimin,laimax,lambdamax,&
     &                         crop,repro_fraction

use landuse_change,     only : luc
use competition_scheme, only : bioclim, existence, competition
use disturbance_scheme, only : disturb

implicit none

!
!     inputs
!
logical, intent(in) :: lnduseon                         !<logical switch to run the land use change subroutine or not.
logical, intent(in) :: compete                          !<logical boolean telling if competition between pfts is on or not
logical, intent(in) :: dofire                           !<boolean, if true allow fire, if false no fire.
logical, intent(in) :: dowetlands                       !<if true allow wetland methane emission
logical, intent(in) :: obswetf                          !<if true, use read-in observed wetland fraction
logical, intent(in) :: onetile_perPFT                   !< if you are running with one tile per PFT in mosaic mode, set to true. Changes
                                                        !< how competition is run. Specifically it allows competition between tiles. This
                                                        !< is not recommended for any case where you don't have one PFT in each tile as it
                                                        !< has not been tested for that.
logical, intent(in) :: leapnow                          !< true if this year is a leap year. Only used if the switch 'leap' is true.
integer, intent(in) :: iday                             !<day of year
integer, intent(in) ::  spinfast                        !<spinup factor for soil carbon whose default value is 1. as this factor increases the
                                                        !<soil c pool will come into equilibrium faster. reasonable value for spinfast is
                                                        !<between 5 and 10. when spinfast.ne.1 then the balcar subroutine is not run.
integer, intent(in) :: il1                              !<il1=1
integer, intent(in) :: il2                              !<il2=ilg (no. of grid cells in latitude circle)
integer, dimension(ilg,ignd), intent(in) :: isand       !<
real, dimension(ilg), intent(in) :: fsnow               !< fraction of snow simulated by class
real, dimension(ilg,ignd), intent(in) :: sand           !< percentage sand
real, dimension(ilg,ignd), intent(in) :: clay           !< percentage clay
real, dimension(ilg), intent(in) :: radj                !< latitude in radians
real, dimension(ilg), intent(in) :: tcano               !< canopy temperature for canopy over ground subarea, K
real, dimension(ilg), intent(in) :: tcans               !< canopy temperature for canopy over snow subarea, K
real, dimension(ilg,ignd), intent(in) ::  tbar          !<soil temperature, k
real, dimension(ilg,ignd), intent(in) :: tbarc          !< soil temperature for canopy over ground subarea, K
real, dimension(ilg,ignd), intent(in) :: tbarcs         !< soil temperature for canopy over snow subarea
real, dimension(ilg,ignd), intent(in) :: tbarg          !< soil temperature for ground subarea
real, dimension(ilg,ignd), intent(in) :: tbargs         !< soil temperature for snow over ground subarea
real, dimension(ilg,ignd), intent(in) ::  thicec        !<frozen mois. content of 3 soil layers, for canopy over snow and canopy over ground subareas
real, dimension(ilg,ignd), intent(in) ::  thiceg        !<frozen mois. content of 3 soil layers, for bareground
real, dimension(ilg), intent(in) :: ta                  !< air temp, K
real, dimension(ilg,ignd), intent(in) :: delzw          !< thicknesses of the 3 soil layers
real, dimension(ilg,ignd), intent(in) :: zbotw          !< bottom of soil layers
real, dimension(ilg), intent(in) :: soildpth            !<soil depth (m)
real, dimension(ilg,ignd), intent(in) :: thliqc         !< liquid mois. content of 3 soil layers, for canopy
                                                        !<over snow and canopy over ground subareas
real, dimension(ilg,ignd), intent(in) :: thliqg         !< liquid mois. content of 3 soil layers, for ground
                                                        !<and snow over ground subareas
real, intent(in) :: deltat                              !< CTEM timestep in days
real, dimension(ilg), intent(in) ::  grclarea           !< area of the grid cell, \f$km^2\f$
real, dimension(ilg), intent(in) ::  currlat            !< centre latitude of grid cells in degrees
real, dimension(ilg), intent(in) :: uwind               !< u wind speed, m/s
real, dimension(ilg), intent(in) :: vwind               !< v wind speed, m/s
real, dimension(ilg), intent(in) ::  precip             !<daily precipitation (mm/day)
real, dimension(ilg), intent(in) ::  netrad             !<daily net radiation (w/m2)
real, dimension(ilg), intent(in) :: lightng             !< total lightning frequency, flashes/km2.year
real, dimension(ilg), intent(in) :: prbfrhuc            !< probability of fire due to human causes
real, dimension(ilg,ignd), intent(in) :: THP            !< Total porosity \f$(cm^3 cm^{-3})\f$ - daily average
real, dimension(ilg,ignd), intent(in) :: BI             !< Clapp and Hornberger b-term (-)
real, dimension(ilg,ignd), intent(in) :: PSIS           !< Soil moisture suction at saturation (m)
real, dimension(ilg,icc), intent(in) :: pfcancmx        !<previous year's fractional coverages of pfts
real, dimension(ilg,icc), intent(in) :: nfcancmx        !<next year's fractional coverages of pfts
real, dimension(ilg), intent(in) ::  faregat            !<
real, dimension(ilg,icc), intent(in) :: todfrac         !<max. fractional coverage of ctem's 9 pfts by the end of the day, for use by land use subroutine
real, dimension(ilg), intent(in) :: ch4conc             !< Atmospheric \f$CH_4\f$ concentration at the soil surface (ppmv)
integer, dimension(ilg), intent(in) :: stdaln           !< an integer telling if ctem is operated within gcm (=0)
                                                        !<or in stand alone mode (=1). this is used for fire
                                                        !<purposes. see comments just above where disturb subroutine is called.
real, dimension(ilg), intent(in) :: wetfrac             !<
real, dimension(ilg,8), intent(in) :: slopefrac         !<
real, intent(in) :: GRAV                                !<Acceleration due to gravity ($m s^{-1} ), (CLASS param) passed in to avoid the common block structure.
real, intent(in) :: RHOW                                !<Density of water ($kg m^{-3}), (CLASS param) passed in to avoid the common block structure.
real, intent(in) :: RHOICE                              !<Density of ice ($kg m^{-3}), (CLASS param) passed in to avoid the common block structure.
!
!     updates
!
logical, intent(inout) :: popdon                        !< if set true use population density data to calculate fire extinguishing
                                                        !< probability and probability of fire due to human causes,
                                                        !< or if false, read directly from .ctm file
logical, intent(inout) :: pftexist(ilg,icc)             !<
logical, intent(inout) :: inibioclim                    !<switch telling if bioclimatic parameters are being initialized from scratch (false)
                                                        !<or being initialized from some spun up values(true).
integer, dimension(ilg,icc), intent(inout) :: pandays   !<days with positive net photosynthesis (an) for use in the phenology subroutine
integer, dimension(ilg,2), intent(inout) :: colddays    !<cold days counter for tracking days below a certain temperature threshold for ndl dcd and crop pfts.
integer, dimension(ilg,icc), intent(inout) :: lfstatus  !<leaf phenology status
real, dimension(ilg,icc), intent(inout) :: ancsveg      !< net photosynthetic rate for ctems 9 pfts for canopy over snow subarea
real, dimension(ilg,icc), intent(inout) :: ancgveg      !< net photosynthetic rate for ctems 9 pfts for canopy over ground subarea
real, dimension(ilg,icc), intent(inout) :: rmlcsveg     !< leaf respiration rate for ctems 9 pfts forcanopy over snow subarea
real, dimension(ilg,icc), intent(inout) :: rmlcgveg     !< leaf respiration rate for ctems 9 pfts forcanopy over ground subarea
real, dimension(ilg), intent(inout) :: extnprob         !< fire extingusinging probability
real, dimension(ilg,icc), intent(inout) :: fcancmx      !< max. fractional coverage of ctem's 9 pfts, but this can be
                                                        !< modified by land-use change, and competition between pfts
real, dimension(ilg,ican,ignd), intent(inout) :: rmatc  !<fraction of roots for each of class' 4 pfts in each soil layer
real, dimension(ilg), intent(inout) :: surmncur         !<number of months with surplus water for current year
real, dimension(ilg), intent(inout) :: defmncur         !<number of months with water deficit for current year
real, dimension(ilg), intent(inout) :: tcurm            !<temperature of the current month (c)
real, dimension(ilg), intent(inout) :: annpcp           !<annual precipitation (mm)
real, dimension(ilg), intent(inout) :: dry_season_length!<length of the dry season (months)
real, dimension(ilg), intent(inout) :: twarmm           !<temperature of the warmest month (c)
real, dimension(ilg), intent(inout) :: tcoldm           !<temperature of the coldest month (c)
real, dimension(ilg), intent(inout) :: gdd5             !<growing degree days above 5 c
real, dimension(ilg), intent(inout) :: aridity          !<aridity index, ratio of potential evaporation to precipitation
real, dimension(ilg), intent(inout) :: srplsmon         !<number of months in a year with surplus water i.e. precipitation more than potential evaporation
real, dimension(ilg), intent(inout) :: defctmon         !<number of months in a year with water deficit i.e. precipitation less than potential evaporation
real, dimension(12,ilg), intent(inout) :: tmonth        !<monthly temperatures
real, dimension(ilg), intent(inout) :: anpcpcur         !<annual precipitation for current year (mm)
real, dimension(ilg), intent(inout) :: anpecur          !<annual potential evaporation for current year (mm)
real, dimension(ilg), intent(inout) :: gdd5cur          !<growing degree days above 5 c for current year
real, dimension(ilg), intent(inout) :: srplscur         !<water surplus for the current month
real, dimension(ilg), intent(inout) :: defctcur         !<water deficit for the current month
real, dimension(ilg), intent(inout) :: srpcuryr         !<water surplus for the current year
real, dimension(ilg), intent(inout) :: dftcuryr         !<water deficit for the current year
real, dimension(ilg), intent(inout) :: anndefct         !<annual water deficit (mm)
real, dimension(ilg), intent(inout) :: annsrpls         !<annual water surplus (mm)
real, dimension(ilg,icc), intent(inout) :: stemmass     !<stem mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real, dimension(ilg,icc), intent(inout) :: rootmass     !<root mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real, dimension(ilg,iccp1), intent(inout) :: litrmass   !<litter mass for each of the 9 ctem pfts + bare, \f$kg c/m^2\f$
real, dimension(ilg,icc), intent(inout) :: gleafmas     !<green leaf mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real, dimension(ilg,icc), intent(inout) :: bleafmas     !<brown leaf mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real, dimension(ilg,iccp1), intent(inout) :: soilcmas   !<soil carbon mass for each of the 9 ctem pfts + bare, \f$kg c/m^2\f$
real, dimension(ilg,icc), intent(inout) :: ailcg        !<green lai for ctem's 9 pfts
real, dimension(ilg,ican), intent(inout) :: ailc        !<lumped lai for class' 4 pfts
real, dimension(ilg,icc,ignd), intent(inout) :: rmatctem!<fraction of roots for each of ctem's 9 pfts in each soil layer
real, dimension(ilg,ican), intent(inout) :: zolnc       !<lumped log of roughness length for class' 4 pfts
real, dimension(ilg,icc), intent(inout) :: ailcb        !<brown lai for ctem's 9 pfts. for now we assume only grasses can have brown lai
real, dimension(ilg), intent(inout) :: vgbiomas         !<grid averaged vegetation biomass, \f$kg c/m^2\f$
real, dimension(ilg), intent(inout) :: gavgltms         !<grid averaged litter mass, \f$kg c/m^2\f$
real, dimension(ilg), intent(inout) :: gavgscms         !<grid averaged soil c mass, \f$kg c/m^2\f$
real, dimension(ilg), intent(inout) :: gavglai          !<grid averaged green leaf area index
real, dimension(ilg,icc), intent(inout) :: bmasveg      !<total (gleaf + stem + root) biomass for each ctem pft, \f$kg c/m^2\f$
real, dimension(ilg,ican), intent(inout) :: cmasvegc    !<total canopy mass for each of the 4 class pfts. recall that class requires canopy
                                                        !<mass as an input, and this is now provided by ctem. \f$kg/m^2\f$.
real, dimension(ilg,ican), intent(inout) :: fcanmx      !<fractional coverage of class' 4 pfts
real, dimension(ilg,ican), intent(inout) :: alvisc      !<visible albedo for class' 4 pfts
real, dimension(ilg,ican), intent(inout) :: alnirc      !<near ir albedo for class' 4 pfts
real, dimension(ilg,icc), intent(inout) :: pstemmass    !<stem mass from previous timestep, is value before fire. used by burntobare subroutine
real, dimension(ilg,icc), intent(inout) :: pgleafmass   !<root mass from previous timestep, is value before fire. used by burntobare subroutine
real, dimension(ilg,icc), intent(inout) :: flhrloss     !<fall or harvest loss for deciduous trees and crops, respectively, \f$kg c/m^2\f$
real, dimension(ilg,icc), intent(inout) :: stmhrlos     !<stem harvest loss for crops, \f$kg c/m^2\f$
real, dimension(ilg,icc), intent(inout) :: rothrlos     !<root death as crops are harvested, \f$kg c/m^2\f$
real, dimension(ilg,icc), intent(inout) :: grwtheff     !<growth efficiency. change in biomass per year per unit max. lai (\f$kg c/m^2\f$)/(m2/m2),
                                                        !<for use in mortality subroutine
real, dimension(ilg,icc), intent(inout) :: lystmmas     !<stem mass at the end of last year
real, dimension(ilg,icc), intent(inout) :: lyrotmas     !<root mass at the end of last year
real, dimension(ilg,icc), intent(inout) :: tymaxlai     !<this year's maximum lai
real, dimension(ilg,icc), intent(inout) ::  geremort    !<
real, dimension(ilg,icc), intent(inout) :: intrmort     !<
real, dimension(ilg,icc), intent(inout) ::  burnvegf    !<per PFT fraction burned of that PFT's area
real, dimension(ilg), intent(inout) :: popdin           !<population density \f$(people / km^2)\f$
!
!   outputs
!
real, dimension(ilg), intent(out) :: ch4soills          !< Methane uptake into the soil column \f$(mg CH_4 m^{-2} s^{-1})\f$
real, dimension(ilg), intent(out) :: rml                !<leaf maintenance respiration (u-mol co2/m2.sec)
real, dimension(ilg), intent(out) :: gpp                !<gross primary productivity
real, dimension(ilg,icc), intent(out) :: slai           !<storage/imaginary lai for phenology purposes
real, dimension(ilg,icc), intent(out) :: veghght        !<vegetation height (meters)
real, dimension(ilg,icc), intent(out) :: rootdpth       !<99% soil rooting depth (meters) both veghght & rootdpth can be used as diagnostics
                                                        !<to see how vegetation grows above and below ground, respectively
real, dimension(ilg), intent(out) :: npp                !<net primary productivity
real, dimension(ilg), intent(out) :: nep                !<net ecosystem productivity
real, dimension(ilg), intent(out) :: hetrores           !<heterotrophic respiration
real, dimension(ilg), intent(out) :: autores            !<autotrophic respiration
real, dimension(ilg), intent(out) :: soilresp           !<soil respiration. this includes root respiration and respiration from litter and soil
                                                        !<carbon pools. note that soilresp is different from socres, which is respiration from the soil c pool.
real, dimension(ilg), intent(out) :: rm                 !<maintenance respiration
real, dimension(ilg), intent(out) :: rg                 !<growth respiration
real, dimension(ilg), intent(out) :: nbp                !<net biome productivity
real, dimension(ilg), intent(out) :: dstcemls1          !<carbon emission losses due to disturbance (fire at present) from vegetation
real, dimension(ilg), intent(out) :: litrfall           !<total litter fall (from leaves, stem, and root) due to all causes (mortality, turnover, and disturbance)
real, dimension(ilg), intent(out) :: humiftrs           !<transfer of humidified litter from litter to soil c pool
real, dimension(ilg), intent(out) :: lucemcom           !<land use change (luc) related combustion emission losses, u-mol co2/m2.sec
real, dimension(ilg), intent(out) :: lucltrin           !<luc related inputs to litter pool, u-mol co2/m2.sec
real, dimension(ilg), intent(out) :: lucsocin           !<luc related inputs to soil c pool, u-mol co2/m2.sec
real, dimension(ilg), intent(out) :: dstcemls3          !<carbon emission losses due to disturbance (fire at present) from litter pool
real, dimension(ilg), intent(out) :: rms                !<stem maintenance respiration (u-mol co2/m2.sec)
real, dimension(ilg), intent(out) :: rmr                !<root maintenance respiration (u-mol co2/m2.sec)
real, dimension(ilg), intent(out) :: litres             !<litter respiration
real, dimension(ilg), intent(out) :: socres             !<soil carbon respiration
real, dimension(ilg,icc), intent(out) :: rmsveg         !<
real, dimension(ilg,icc), intent(out) :: rmrveg         !<
real, dimension(ilg,icc), intent(out) :: rmlveg         !<
real, dimension(ilg,icc), intent(out) :: gppveg         !<
real, dimension(ilg,icc), intent(out) :: nppveg         !<npp for individual pfts,  u-mol co2/m2.sec
real, dimension(ilg,icc), intent(out) :: rgveg          !<
real, dimension(ilg,iccp1), intent(out) :: nepveg       !<
real, dimension(ilg,iccp1), intent(out) :: nbpveg       !<
real, dimension(ilg,iccp1), intent(out) :: ltresveg     !<
real, dimension(ilg,iccp1), intent(out) :: scresveg     !<
real, dimension(ilg,iccp1), intent(out) :: hetrsveg     !<
real, dimension(ilg,iccp1), intent(out) :: humtrsvg     !<
real, dimension(ilg,icc), intent(out) :: autoresveg     !<
real, dimension(ilg,icc), intent(out) :: litrfallveg    !<
real, dimension(ilg,icc), intent(out) :: roottemp       !<root temperature, k
real, dimension(ilg,icc), intent(out) :: emit_co2       !<carbon dioxide emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_co        !<carbon monoxide emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_ch4       !<methane emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_nmhc      !<non-methane hydrocarbons emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_h2        !<hydrogen gas emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_nox       !<nitrogen oxides emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_n2o       !<nitrous oxide emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_pm25      !<particulate matter less than 2.5 um in diameter emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_tpm       !<total particulate matter emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_tc        !<total carbon emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_oc        !<organic carbon emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: emit_bc        !<black carbon emitted from biomass burning in g of compound
real, dimension(ilg,icc), intent(out) :: bterm_veg      !<biomass term for fire probabilty calc
real, dimension(ilg), intent(out) :: lterm              !<lightning term for fire probabilty calc
real, dimension(ilg,icc), intent(out) :: mterm_veg      !<moisture term for fire probabilty calc
real, dimension(ilg), intent(out) :: ch4wet1            !<
real, dimension(ilg), intent(out) :: ch4wet2            !<
real, dimension(ilg), intent(out) :: wetfdyn            !<
real, dimension(ilg), intent(out) :: ch4dyn1            !<
real, dimension(ilg), intent(out) :: ch4dyn2            !<
real, dimension(ilg,icc), intent(out) :: cc             !<
real, dimension(ilg,icc), intent(out) :: mm             !<
real, dimension(ilg,icc), intent(out) :: lambda         !<
real, dimension(ilg,icc), intent(out) :: afrleaf        !<allocation fraction for leaves
real, dimension(ilg,icc), intent(out) :: afrstem        !<allocation fraction for stem
real, dimension(ilg,icc), intent(out) :: afrroot        !<allocation fraction for root
real, dimension(ilg,icc), intent(out) :: wtstatus       !<soil water status used for calculating allocation fractions
real, dimension(ilg,icc), intent(out) :: ltstatus       !<light status used for calculating allocation fractions
real, dimension(ilg), intent(out) :: burnfrac           !<areal fraction burned due to fire for every grid cell (%)
real, dimension(ilg,icc), intent(out) :: leaflitr       !<leaf litter fall rate (u-mol co2/m2.sec). this leaf litter does not
                                                        !<include litter generated due to mortality/fire
real, dimension(ilg,icc), intent(out) :: smfunc_veg     !<soil moisture dependence on fire spread rate
real, dimension(ilg,icc), intent(out) :: tltrleaf       !<total leaf litter fall rate (u-mol co2/m2.sec)
real, dimension(ilg,icc), intent(out) :: tltrstem       !<total stem litter fall rate (u-mol co2/m2.sec)
real, dimension(ilg,icc), intent(out) :: tltrroot       !<total root litter fall rate (u-mol co2/m2.sec)
real, dimension(ilg,ican), intent(out) :: paicgat       !<
real, dimension(ilg,ican), intent(out) :: slaicgat      !<
real, dimension(ilg,icc), intent(out) :: vgbiomas_veg   !<

! ---------------------------------------------
! Local variables:

integer i
integer j
integer k
integer icount
integer n
integer m
integer sort(icc)
integer nol2pfts(ican) !<number of level 2 ctem pfts
integer k1
integer k2
integer nml
integer ilmos(ilg)
integer jlmos(ilg)
!
real fare_cmp(nlat,icc)      !<
real nppveg_cmp(nlat,icc)    !<
real geremort_cmp(nlat,icc)  !<
real intrmort_cmp(nlat,icc)  !<
real gleafmas_cmp(nlat,icc)  !<
real bleafmas_cmp(nlat,icc)  !<
real stemmass_cmp(nlat,icc)  !<
real rootmass_cmp(nlat,icc)  !<
real litrmass_cmp(nlat,iccp1)!<
real soilcmas_cmp(nlat,iccp1)!<
real lambda_cmp(nlat,icc)    !<
real bmasveg_cmp(nlat,icc)   !<
real burnvegf_cmp(nlat,icc)  !<
real add2allo_cmp(nlat,icc)  !<
real cc_cmp(nlat,icc)        !<
real mm_cmp(nlat,icc)        !<
real fcanmx_cmp(nlat,ican)   !<
real vgbiomas_cmp(nlat)      !<
real grclarea_cmp(nlat)      !<
real gavgltms_cmp(nlat)      !<
real gavgscms_cmp(nlat)      !<
real yesfrac_mos(nlat,icc)   !<
real todfrac_cmp(nlat)       !<
real pfcancmx_cmp(nlat,icc)  !<
real nfcancmx_cmp(nlat,icc)  !<
real pstemmass_cmp(nlat,icc) !<
real pgleafmass_cmp(nlat,icc)!<
real surmncur_cmp(nlat) !<
real defmncur_cmp(nlat) !<
logical pftexist_cmp(nlat,icc) !<
real netradrow(nlat,nmos)   !<
real ta_cmp(nlat)       !<
real precip_cmp(nlat)   !<
real netrad_cmp(nlat)   !<
real tcurm_cmp(nlat)    !<
real srpcuryr_cmp(nlat) !<
real dftcuryr_cmp(nlat) !<
real tmonth_cmp(12,nlat)!<
real anpcpcur_cmp(nlat) !<
real anpecur_cmp(nlat)  !<
real gdd5cur_cmp(nlat)  !<
real srplscur_cmp(nlat) !<
real defctcur_cmp(nlat) !<
real twarmm_cmp(nlat)   !<
real tcoldm_cmp(nlat)   !<
real gdd5_cmp(nlat)     !<
real aridity_cmp(nlat)  !<
real srplsmon_cmp(nlat) !<
real defctmon_cmp(nlat) !<
real anndefct_cmp(nlat) !<
real annsrpls_cmp(nlat) !<
real annpcp_cmp(nlat)   !<
real dry_season_length_cmp(nlat) !<
real lucemcom_cmp(nlat) !<
real lucltrin_cmp(nlat) !<
real lucsocin_cmp(nlat) !<
!
real gppcsveg(ilg,icc)   !<
real gppcgveg(ilg,icc)   !<
real yesfrac_comp(ilg,icc) !<
real galtcels(ilg) !<
real dstcemls2(ilg)!<
real fc(ilg)  !<
real fg(ilg)  !<
real fcs(ilg) !<
real fgs(ilg) !<
real fcans(ilg,ican) !<
real fcan(ilg,ican)  !<
real term        !<
real pglfmass(ilg,icc)  !<
real pblfmass(ilg,icc)  !<
real pstemass(ilg,icc)  !<
real protmass(ilg,icc)  !<
real plitmass(ilg,iccp1)!<
real psocmass(ilg,iccp1)!<
real pvgbioms(ilg)      !<
real pgavltms(ilg)      !<
real pgavscms(ilg)      !<
real fcancs(ilg,icc)   !<
real fcanc(ilg,icc)    !<
real rmscgveg(ilg,icc) !<
real rmscsveg(ilg,icc) !<
real rmrcgveg(ilg,icc) !<
real rmrcsveg(ilg,icc) !<
real anveg(ilg,icc)    !<
real rmveg(ilg,icc)    !<
real rttempcs(ilg,icc) !<
real rttempcg(ilg,icc) !<
real pheanveg(ilg,icc) !<
real pancsveg(ilg,icc) !<
real pancgveg(ilg,icc) !<
real ltrsvgcs(ilg,icc)   !<
real ltrsvgcg(ilg,icc)   !<
real scrsvgcs(ilg,icc)   !<
real scrsvgcg(ilg,icc)   !<
real ltrsbrg(ilg)        !<
real scrsbrg(ilg)        !<
real ltrsbrgs(ilg)       !<
real scrsbrgs(ilg)       !<
real soilrsvg(ilg,iccp1) !<
real ltrestep(ilg,iccp1) !<
real screstep(ilg,iccp1) !<
real hutrstep(ilg,iccp1) !<
!
real tbarccs(ilg,ignd) !<
real fieldsm(ilg,ignd) !<
real wiltsm(ilg,ignd)  !<
real rootlitr(ilg,icc) !<
real stemlitr(ilg,icc) !<
real nppvgstp(ilg,icc) !<
real rmlvgstp(ilg,icc) !<
real rmsvgstp(ilg,icc) !<
real rmrvgstp(ilg,icc) !<
real gppvgstp(ilg,icc) !<
real ntchlveg(ilg,icc) !<
real ntchsveg(ilg,icc) !<
real ntchrveg(ilg,icc) !<
real stemltrm(ilg,icc) !<
real rootltrm(ilg,icc) !<
real glealtrm(ilg,icc) !<
real stemltdt(ilg,icc)  !<
real rootltdt(ilg,icc)  !<
real glfltrdt(ilg,icc)  !<
real blfltrdt(ilg,icc)  !<
real glcaemls(ilg,icc)  !<
real blcaemls(ilg,icc)  !<
real rtcaemls(ilg,icc)  !<
real stcaemls(ilg,icc)  !<
real ltrcemls(ilg,icc)  !<
real dscemlv1(ilg,icc)  !<
real dscemlv2(ilg,icc)  !<
real add2allo(ilg,icc)  !<
real reprocost(ilg,icc) !<
real repro_cost_g(ilg)  !<
real lambdaalt !<

!>
!>     ---------------------------------------------------------------
!>     Constants and parameters are located in ctem_params.f90
!>     -----------------------------------------------------------------

!  ==========================================================================================




!> Begin calculations

!> Generate the sort index for correspondence between 9 pfts and the
!> 12 values in the parameter vectors

icount=0
do 95 j = 1, ican
  do 96 m = 1, nol2pfts(j)
    n = (j-1)*l2max + m
    icount = icount + 1
    sort(icount)=n
96    continue
95   continue

!     ---------------------------------------------------------------

if(compete .or. lnduseon)then

!> If you intend to have competition and LUC between tiles then set onetile_perPFT to true.
!> NOTE: Turning onetile_perPFT to true is usually not the behaviour desired unless you are
!> running with one PFT on each tile and want them to compete for space
!> across tiles. So in general keep this as False. JM Jan 2016.

  if (.not. onetile_perPFT) then !> this is composite/mosaic mode (in mosaic competition only occurs WITHIN a tile)

    if (compete) then

!>Calculate bioclimatic parameters for estimating pfts existence

        call  bioclim (iday,       ta,    precip,  netrad,&
     &                    1,     il2,    ilg, leapnow, &
     &                 tcurm, srpcuryr,  dftcuryr,  inibioclim,&
     &                 tmonth, anpcpcur,   anpecur, gdd5cur,&
     &                 surmncur, defmncur,  srplscur,defctcur,&
     &                 twarmm,   tcoldm,      gdd5, aridity,&
     &                 srplsmon, defctmon, anndefct, annsrpls,&
     &                 annpcp, dry_season_length )

        if (inibioclim) then
!>
!>If first day of year then based on updated bioclimatic parameters
!!find if pfts can exist or not.
!!If .not. inibioclim then it is the first year of a run that you do not have the
!!climatological means already in the CTM file. After one
!!year inibioclim is set to true and the climatological means
!!are used from the first year.
!!
            call existence(iday,            1,         il2, ilg,&
     &                     sort,     nol2pfts,        &
     &                   twarmm,   tcoldm,     gdd5,  aridity,&
     &                   srplsmon, defctmon, anndefct, annsrpls,&
     &                   annpcp, pftexist, dry_season_length )
!>
!!Call competition subroutine which on the basis of previous day's
!!npp estimates changes in fractional coverage of pfts
!!
            call competition (iday,     1,        il2,      ilg,&
     &                    nol2pfts, nppveg,   dofire, leapnow,&
     &                    pftexist, geremort, intrmort,&
     &                    gleafmas, bleafmas, stemmass, rootmass,&
     &                    litrmass, soilcmas, grclarea,   lambda,&
     &                    burnvegf, sort,  pstemmass, &
     &                    pgleafmass,&
!    ------------------- inputs above this line -------------------
     &                    fcancmx,   fcanmx, vgbiomas, gavgltms,&
     &                    gavgscms, bmasveg,  &
!    ------------------- updates above this line ------------------
     &                    add2allo,      cc,      mm)
!    ------------------- outputs above this line ------------------
!
        end if ! inibioclim

    endif  ! if (compete)



!>
!!If landuse is on, then implelement luc, change fractional coverages,
!!move biomasses around, and estimate luc related combustion emission losses.
!!
        if (lnduseon) then

         do j = 1, icc
           do i = il1, il2
             yesfrac_comp(i,j)=fcancmx(i,j)
           enddo
         enddo

         call luc(    il1,      il2,   ilg,  nol2pfts, &
     &                  grclarea, pfcancmx, nfcancmx,     iday,&
     &                   todfrac,yesfrac_comp,.true.,  compete, leapnow,&
     &                  gleafmas, bleafmas, stemmass, rootmass,&
     &                  litrmass, soilcmas, vgbiomas, gavgltms,&
     &                  gavgscms,  fcancmx,   fcanmx,&
     &                  lucemcom, lucltrin, lucsocin)
        endif !lnduseon

  else ! onetile_perPFT is True
!>
!!Land use change and competition for mosaics needs mapping and
!!unmapping of the pfts. Composite does not require these extra steps.
!!
!!Check if number of mosaics is equal to the number of pfts plus one bare, e.g., nmos=iccp1
!!
        if (nmos.ne.iccp1) then 
         write(*,2050) 'number of mosaics, nmos= ',nmos,&
     &                 ' is not equal to the number of pfts plus',&
     &                 ' one bare, iccp1= ',iccp1
         write(*,2051) 'competition works properly only when all pfts',&
     &                 ' and bare are considered. (onetile_perPFT is set to true!)'
         call xit ('ctem',-11)
        endif 
2050    format(a25,i2,a40,a18,i2,a1)
2051    format(a45,a40)
!>
!!check for fcancmx(i,j). this should be either 0 or 1 for competition to work.
!!
        do j=1, icc
         do i=il1, il2
          if(fcancmx(i,j).ne.1.0 .and. fcancmx(i,j).ne.0.0) then
           write(*,2100) &
     &                'mosaic ',i,' has pft fraction: ', &
     &                'fcancmx(',i,',',j,')=',fcancmx(i,j)
           write(*,2101) &
     &                'mosaic competition and luc work only when ',&
     &                'each mosaic is 100% occupied with one pft'
           call xit ('ctem',-12)
          endif
         enddo
        enddo
2100    format(a7,i2,a19,a8,i2,a1,i2,a2,f8.3)
2101    format(a40,a40)
!>
!!competition_map scatters and maps the array with indices of (ilg,icc) to (nlat,icc) for preparation for competition
!!
          call competition_map(    nml,    ilmos,   jlmos,   grclarea,&
     &                         faregat,   fcancmx,  nppveg,  geremort,&
     &                         intrmort, gleafmas, bleafmas, stemmass,&
     &                         rootmass, litrmass, soilcmas,&
     &                         pftexist,   lambda,  bmasveg,burnvegf,&
     &                         add2allo,       cc,       mm,   fcanmx,&
     &                         vgbiomas, gavgltms, gavgscms,&
     &                               ta,   precip,   netrad,    tcurm,&
     &                         srpcuryr, dftcuryr,   tmonth, anpcpcur, &
     &                          anpecur,  gdd5cur, surmncur, defmncur,&
     &                         srplscur, defctcur,   twarmm,   tcoldm,&
     &                             gdd5,  aridity, srplsmon, defctmon,&
     &                         anndefct, annsrpls,   annpcp,&
     &                      dry_season_length,&
     &                         lucemcom, lucltrin, lucsocin, pfcancmx,&
     &                         nfcancmx, pstemmass, pgleafmass,&
!    ------------------- inputs above this line ---------------------
     &                        netradrow,&
!    ------------------- intermediate and saved above this line -----
     &                         fare_cmp,    nppveg_cmp,  geremort_cmp,&
     &                     intrmort_cmp,  gleafmas_cmp,  bleafmas_cmp,&
     &                     stemmass_cmp,  rootmass_cmp,  litrmass_cmp,&
     &                     soilcmas_cmp,  pftexist_cmp,    lambda_cmp,&
     &                      bmasveg_cmp,burnvegf_cmp,  add2allo_cmp,&
     &                           cc_cmp,        mm_cmp,    fcanmx_cmp,&
     &                     vgbiomas_cmp,  grclarea_cmp,&
     &                     gavgltms_cmp,  gavgscms_cmp,&
     &                           ta_cmp,    precip_cmp,    netrad_cmp, &
     &                        tcurm_cmp,  srpcuryr_cmp,  dftcuryr_cmp,&
     &                       tmonth_cmp,  anpcpcur_cmp,   anpecur_cmp, &
     &                      gdd5cur_cmp,  surmncur_cmp,  defmncur_cmp,&
     &                     srplscur_cmp,  defctcur_cmp,    twarmm_cmp, &
     &                       tcoldm_cmp,      gdd5_cmp,   aridity_cmp,&
     &                     srplsmon_cmp,  defctmon_cmp,  anndefct_cmp,&
     &                     annsrpls_cmp,    annpcp_cmp,&
     &                     dry_season_length_cmp,&
     &                     lucemcom_cmp,  lucltrin_cmp,  lucsocin_cmp,&
     &                     pfcancmx_cmp,   nfcancmx_cmp, pstemmass_cmp,&
     &                     pgleafmass_cmp )
!    ------------------- outputs above this line --------------------

        if (compete) then

!>Calculate bioclimatic parameters for estimating pfts existence

         call  bioclim (iday,       ta_cmp,    precip_cmp,  netrad_cmp,&
     &                    1,         nlat,          nlat,   leapnow, &
     &            tcurm_cmp, srpcuryr_cmp,  dftcuryr_cmp,  inibioclim,&
     &           tmonth_cmp, anpcpcur_cmp,   anpecur_cmp, gdd5cur_cmp,&
     &         surmncur_cmp, defmncur_cmp,  srplscur_cmp,defctcur_cmp,&
     &           twarmm_cmp,   tcoldm_cmp,      gdd5_cmp, aridity_cmp,&
     &         srplsmon_cmp, defctmon_cmp, anndefct_cmp, annsrpls_cmp,&
     &           annpcp_cmp, dry_season_length_cmp)


       if (inibioclim) then
!>
!!If first day of year then based on updated bioclimatic parameters find if pfts can exist
!!or not. If .not. inibioclim then it is the first year of a run that you do not have the 
!!climatological means already in the CTM file. After one year inibioclim is set to true and 
!!the climatological means are used from the first year.
!!
          call existence(iday,            1,         nlat,         nlat,&
     &                   sort,     nol2pfts,      &
     &             twarmm_cmp,   tcoldm_cmp,     gdd5_cmp,  aridity_cmp,&
     &           srplsmon_cmp, defctmon_cmp, anndefct_cmp, annsrpls_cmp,&
     &             annpcp_cmp, pftexist_cmp,&
     &             dry_season_length_cmp)
!>
!!call competition subroutine which on the basis of previous day's npp estimates changes in fractional coverage of pfts
!!
         call competition (iday,          1,          nlat,        nlat,&
     &               nol2pfts,   nppveg_cmp, dofire, leapnow, &
     &           pftexist_cmp, geremort_cmp, intrmort_cmp,&
     &           gleafmas_cmp, bleafmas_cmp, stemmass_cmp, rootmass_cmp,&
     &           litrmass_cmp, soilcmas_cmp, grclarea_cmp,   lambda_cmp,&
     &             burnvegf_cmp,       sort, pstemmass_cmp,&
     &            pgleafmass_cmp,    &
!    ------------------- inputs above this line -------------------
     &               fare_cmp,   fcanmx_cmp, vgbiomas_cmp, gavgltms_cmp,&
     &           gavgscms_cmp,   bmasveg_cmp,&
!    ------------------- updates above this line ------------------
     &           add2allo_cmp,      cc_cmp,      mm_cmp)
!    ------------------- outputs above this line ------------------

         end if !inibioclim

        endif !compete check

        if(lnduseon)then

         do i = il1, nlat
           do j = 1, icc  
            yesfrac_mos(i,j)=fare_cmp(i,j)
           enddo
         enddo

         call luc(il1,      nlat,     nlat,     nol2pfts, &
     &           grclarea_cmp,    pfcancmx_cmp, nfcancmx_cmp,     iday,&
     &           todfrac_cmp,  yesfrac_mos,   .true., compete, leapnow,&
     &           gleafmas_cmp, bleafmas_cmp, stemmass_cmp, rootmass_cmp,&
     &           litrmass_cmp, soilcmas_cmp, vgbiomas_cmp, gavgltms_cmp,&
     &           gavgscms_cmp,     fare_cmp,   fcanmx_cmp,&
     &           lucemcom_cmp, lucltrin_cmp, lucsocin_cmp)

        endif !lnduseon check
!>
!!Competition_unmap unmaps and gathers the array with
!!indices (nlat,icc) back to (ilg,icc) after competition is done 
!!


        call competition_unmap( nml,      ilmos,    jlmos,   nol2pfts,&
     &                           fare_cmp,   nppveg_cmp, geremort_cmp,&
     &                       intrmort_cmp, gleafmas_cmp, bleafmas_cmp,&
     &                       stemmass_cmp, rootmass_cmp, litrmass_cmp,&
     &                       soilcmas_cmp, pftexist_cmp,   lambda_cmp,&
     &                        bmasveg_cmp,burnvegf_cmp, add2allo_cmp,&
     &                             cc_cmp,       mm_cmp,   fcanmx_cmp,&
     &                       vgbiomas_cmp, grclarea_cmp, &
     &                       gavgltms_cmp, gavgscms_cmp,&
     &                             ta_cmp,   precip_cmp,   netrad_cmp, &
     &                          tcurm_cmp, srpcuryr_cmp, dftcuryr_cmp,&
     &                         tmonth_cmp, anpcpcur_cmp,  anpecur_cmp, &
     &                        gdd5cur_cmp, surmncur_cmp, defmncur_cmp,&
     &                       srplscur_cmp, defctcur_cmp,   twarmm_cmp, &
     &                         tcoldm_cmp,     gdd5_cmp,  aridity_cmp,&
     &                       srplsmon_cmp, defctmon_cmp, anndefct_cmp,&
     &                       annsrpls_cmp,   annpcp_cmp,&
     &                     dry_season_length_cmp,&
     &                     lucemcom_cmp,  lucltrin_cmp,  lucsocin_cmp,&
     &                     pfcancmx_cmp,   nfcancmx_cmp, pstemmass_cmp,&
     &                      pgleafmass_cmp,&
!    ------------------- inputs above this line ---------------------
     &                            netradrow,&
!    ------------------- saved for intermediate above this line -----
     &                        faregat,  fcancmx,    nppveg, geremort,  &
     &                       intrmort, gleafmas,  bleafmas, stemmass,&
     &                       rootmass, litrmass,  soilcmas, grclarea,&
     &                        pftexist,  lambda,   bmasveg,burnvegf,&
     &                        add2allo,      cc,        mm,   fcanmx,&
     &                        vgbiomas, gavgltms, gavgscms,&
     &                     ta,  precip,   netrad,    tcurm, srpcuryr,&
     &                        dftcuryr ,  tmonth, anpcpcur,  anpecur, &
     &                         gdd5cur, surmncur, defmncur, srplscur,  &
     &                        defctcur,   twarmm,   tcoldm,     gdd5, &
     &                         aridity, srplsmon, defctmon, anndefct,&
     &                        annsrpls,   annpcp,&
     &                         dry_season_length,&
     &                         lucemcom, lucltrin, lucsocin, pfcancmx,&
     &                         nfcancmx, pstemmass, pgleafmass )
!    ------------------- updates above this line --------------------

  endif ! onetile_perPFT true/false

endif !compete/lnduseon

!     ---------------------------------------------------------------
!>
!>initialize required arrays to zero
!>
do 100 i = il1, il2
    rms(i) = 0.0         !<grid ave. stem maintenance respiration
    rmr(i) = 0.0         !<grid ave. root maintenance respiration
    rml(i) = 0.0         !<grid ave. leaf maintenance respiration
    rm(i) = 0.0          !<grid ave. total maintenance respiration
    rg(i) = 0.0          !<grid ave. growth respiration
    npp(i) = 0.0         !<grid ave. net primary productivity
    gpp(i) = 0.0         !<grid ave. gross primary productivity
    nep(i)=0.0           !<grid ave. net ecosystem productivity
    nbp(i)=0.0           !<grid ave. net biome productivity
    litres(i)=0.0        !<grid ave. litter respiration
    socres(i)=0.0        !<grid ave. soil carbon respiration
    hetrores(i)=0.0      !<grid ave. heterotrophic respiration
    autores(i)=0.0       !<grid ave. autotrophic respiration
    soilresp(i)=0.0      !<grid ave. soil respiration
    humiftrs(i)=0.0      !<grid ave. humification rate
    dstcemls1(i)=0.0     !<grid ave. carbon emission losses due to disturbance, vegetation
    dstcemls2(i)=0.0     !<grid ave. carbon emission losses due to disturbance, total
    dstcemls3(i)=0.0     !<grid ave. carbon emission losses due to disturbance, litter
    galtcels(i)=0.0      !<grid ave. litter fire emission losses (redundant, same as dstcemls3)
    fc(i)=0.0            !<fraction of canopy over ground subarea
    fcs(i)=0.0           !<fraction of canopy over snow subarea
    fg(i)=0.0            !<fraction of bare ground subarea
    fgs(i)=0.0           !<fraction of snow over ground subarea
    tbarccs(i,1)=0.0     !<avg. soil temperature over canopy over snow
    tbarccs(i,2)=0.0     !<and canopy over ground subareas.
    tbarccs(i,3)=0.0     !<over bare fraction of the grid cell
    screstep(i,iccp1)=0.0  !<soil c respiration in \f$kg c/m^2\f$ over the time step
    ltrestep(i,iccp1)=0.0  !<litter c respiration in \f$kg c/m^2\f$ over the time step
    soilrsvg(i,iccp1)=0.0  !<soil respiration over the bare fraction
    humtrsvg(i,iccp1)=0.0  !<humified rate the bare fraction
    ltresveg(i,iccp1)=0.0  !<litter respiration rate over bare fraction
    scresveg(i,iccp1)=0.0  !<soil c respiration rate over bare fraction
    hetrsveg(i,iccp1)=0.0  !<heterotrophic resp. rate over bare fraction
    nbpveg(i,iccp1) = 0.0  !<net biome productity for bare fraction
    nepveg(i,iccp1) = 0.0  !<net ecosystem productity for bare fraction
    !        expnbaln(i)=0.0        !amount of c related to spatial expansion !Not used JM Jun 2014
    repro_cost_g(i)=0.0    !<amount of C for production of reproductive tissues
100   continue 
!
do 110 j = 1,icc
    do 120 i = il1, il2
        fcanc(i,j) =0.0
        fcancs(i,j)=0.0
        rmsveg(i,j)=0.0    !<stem maintenance resp. rate for each pft
        rmrveg(i,j)=0.0    !<root maintenance resp. rate for each pft
        rmlveg(i,j)=0.0    !<leaf maintenance resp. rate for each pft
        rmveg(i,j)=0.0    !<total maintenance resp. rate for each pft
        rgveg(i,j)=0.0    !<growth resp. rate for each pft
        anveg(i,j)=0.0    !<net photosynthesis rate for each pft
        pheanveg(i,j)=0.0  !<net photosynthesis rate, for phenology purposes
        pancsveg(i,j)=0.0  !<net photosynthesis rate, canopy over snow subarea, for phenology purposes
        pancgveg(i,j)=0.0  !<net photosynthesis rate, canopy over ground subarea, for phenology purposes
        gppveg(i,j)=0.0    !<gross primary productity for each pft
        nppveg(i,j)=0.0    !<net primary productity for each pft
        nbpveg(i,j)=0.0    !<net biome productity for each pft
        nepveg(i,j)=0.0    !<net ecosystem productity for each pft
        ltresveg(i,j)=0.0  !<litter respiration rate for each pft
        scresveg(i,j)=0.0  !<soil c respiration rate for each pft
        hetrsveg(i,j)=0.0  !<heterotrophic resp. rate for each pft
        soilrsvg(i,j)=0.0  !<soil respiration rate for each pft
        humtrsvg(i,j)=0.0  !<humification rate for each pft
        litrfallveg(i,j)=0.0 !<litter fall in \f$kg c/m^2\f$ for each pft
        screstep(i,j)=0.0  !<soil c respiration in \f$kg c/m^2\f$ over the tim
        ltrestep(i,j)=0.0  !<litter c respiration in \f$kg c/m^2\f$ over the t
        hutrstep(i,j)=0.0  !<humification rate in \f$kg c/m^2\f$ over the time
        roottemp(i,j)=0.0  !<root temperature
        nppvgstp(i,j)=0.0  !<npp (\f$kg c/m^2\f$) sequestered over the model time step
        gppvgstp(i,j)=0.0  !<gpp (\f$kg c/m^2\f$) sequestered over the model time step
        rmlvgstp(i,j)=0.0  !<leaf maintenance resp. (\f$kg c/m^2\f$) respired over the model time step
        rmsvgstp(i,j)=0.0  !<stem maintenance resp. (\f$kg c/m^2\f$) respired  over the model time step
        rmrvgstp(i,j)=0.0  !<root maintenance resp. (\f$kg c/m^2\f$) respired over the model time step
        ntchlveg(i,j)=0.0  !<net change in gleaf biomass after auto. resp. & allocation
        ntchsveg(i,j)=0.0  !<net change in stem biomass after auto. resp. & allocation
        ntchrveg(i,j)=0.0  !<net change in root biomass after auto. resp. & allocation
        dscemlv1(i,j)=0.0  !<total carbon emission losses (\f$kg c/m^2\f$), mainly due to fire
        dscemlv2(i,j)=0.0  !<total carbon emission losses (\f$kg c/m^2\f$), mainly due to fire
        tltrleaf(i,j)=0.0  !<total leaf litter
        tltrstem(i,j)=0.0  !<total stem litter
        tltrroot(i,j)=0.0  !<total root litter
        vgbiomas_veg(i,j)=0.0 !<vegetation biomass for each pft
        lambda(i,j)=0.0    !< Used to determine the colonization rate
        reprocost(i,j) = 0.0 !< cost of producing reproductive tissues
    !          expbalvg(i,j)=0.0  !amount of c related to spatial expansion !Not used JM Jun 2014
120     continue
110   continue
!>
!!Store green and brown leaf, stem, and root biomass, and litter and
!!soil c pool mass in arrays. knowing initial sizes of all pools and
!!final sizes at the end of this subroutine, we check for conservation of mass.
!!
do 130 j = 1, icc
    do 140 i = il1, il2
        pglfmass(i,j)=gleafmas(i,j)    !<green leaf mass from last time step
        pblfmass(i,j)=bleafmas(i,j)    !<brown leaf mass from last time step
        pstemass(i,j)=stemmass(i,j)    !<stem mass from last time step
        protmass(i,j)=rootmass(i,j)    !<root mass from last time step
        plitmass(i,j)=litrmass(i,j)    !<litter mass from last time step
        psocmass(i,j)=soilcmas(i,j)    !<soil c mass from last time step
140     continue
130   continue
!
do 145 i = il1, il2
    pvgbioms(i)=vgbiomas(i)          !<vegetation biomass from last time step
    vgbiomas(i)= 0.0
    pgavltms(i)=gavgltms(i)          !<litter mass from last time step
    gavgltms(i)=0.0
    pgavscms(i)=gavgscms(i)          !<soil c mass from last time step
    gavgscms(i)=0.0
    litrfall(i)=0.0                  !<combined total litter fall rate
    gavglai (i)=0.0                  !<grid averaged green lai
    plitmass(i,iccp1)=litrmass(i,iccp1)  !<litter mass over bare fraction
    psocmass(i,iccp1)=soilcmas(i,iccp1)  !<soil c mass over bare fraction
145   continue

!     ------------------------------------------------------------------

!>Initialization ends

!>Find fc and fcs based on fcancmx

do 150 j = 1, icc
    do 160 i = il1, il2
        fcancs(i,j) = fcancmx(i,j)*fsnow(i)
        fcanc(i,j)  = fcancmx(i,j)*(1.-fsnow(i))
        fcs(i) = fcs(i) + fcancs(i,j)
        fc(i)  = fc(i)  + fcanc(i,j)
160     continue 
150   continue 

do 170 i = il1, il2
    fgs(i)=(1.0-fcs(i)-fc(i))*fsnow(i)
    fg(i)=(1.0-fcs(i)-fc(i))*(1.0-fsnow(i))
170   continue

!     ------------------------------------------------------------------


!>
!>Autotrophic respiration
!!
!!Leaf respiration is calculated in phtsyn subroutine, while stem
!!and root maintenance respiration are calculated here.
!!
!!We treat canopy over ground and canopy over snow subareas
!!separately because stem temperature (for which we use canopy
!!temperature as a surrogate) can be different for these two subareas.
!!
!!Find maintenance respiration for canopy over snow sub-area in umol co2/m2/sec
!!

call   mainres (fcancs,      fcs,     stemmass,   rootmass,       &
     &                  il1, il2, leapnow, &
     &                    ta,       tbarcs,   rmatctem,&
     &                  sort, nol2pfts,        isand,&
     &              rmscsveg, rmrcsveg,     rttempcs)




!>Find maintenance respiration for canopy over ground sub-area


call   mainres ( fcanc,       fc,     stemmass,   rootmass,       &
     &                   il1, il2, leapnow, &
     &                    ta,        tbarc,   rmatctem,&
     &                  sort, nol2pfts,        isand,&
     &              rmscgveg, rmrcgveg,     rttempcg)



!>If ailcg/gleafmas is zero, i.e. real leaves are not on, then
!>make maintenance respiration and gpp from storage/imaginary lai
!>equal to zero so that we don't use these numbers in carbon budget.

do 180 j = 1, icc
  do 190 i = il1, il2

    gppcsveg(i,j)=ancsveg(i,j)+rmlcsveg(i,j)
    gppcgveg(i,j)=ancgveg(i,j)+rmlcgveg(i,j)
!
    if (lfstatus(i,j).eq.4) then
        rmlcgveg(i,j)=0.0
        rmlcsveg(i,j)=0.0
        pancsveg(i,j)=ancsveg(i,j)   !< to be used for phenology
        pancgveg(i,j)=ancgveg(i,j)   !< purposes
        ancsveg(i,j)=0.0
        ancgveg(i,j)=0.0
    else
        pancsveg(i,j)=ancsveg(i,j)   !< to be used for phenology
        pancgveg(i,j)=ancgveg(i,j)   !< purposes
        if(slai(i,j).gt.ailcg(i,j))then
            term=((1.0/kn(sort(j)))*(1.0-exp(-kn(sort(j))*ailcg(i,j))) &
    &          /(1.0/kn(sort(j)))*(1.0-exp(-kn(sort(j))* slai(i,j))))
            rmlcgveg(i,j)=rmlcgveg(i,j)*term
            rmlcsveg(i,j)=rmlcsveg(i,j)*term
        endif
    endif
190     continue
180   continue
!>
!!Find vegetation averaged leaf, stem, and root respiration, and
!!gpp using values from canopy over ground and canopy over snow subareas
!!
do 270 j = 1, icc
    do 280 i = il1, il2
        if( (fcanc(i,j)+fcancs(i,j)).gt.zero) then


            rmsveg(i,j)= (fcanc(i,j)*rmscgveg(i,j) + &
        &        fcancs(i,j)*rmscsveg(i,j)) / ( fcanc(i,j) + fcancs(i,j))
            rmrveg(i,j)= (fcanc(i,j)*rmrcgveg(i,j) + &
        &        fcancs(i,j)*rmrcsveg(i,j)) / ( fcanc(i,j) + fcancs(i,j))
            rmlveg(i,j)= (fcanc(i,j)*rmlcgveg(i,j) + &
        &        fcancs(i,j)*rmlcsveg(i,j)) / ( fcanc(i,j) + fcancs(i,j))
            anveg(i,j)= (fcanc(i,j)*ancgveg(i,j) + &
        &        fcancs(i,j)*ancsveg(i,j)) / ( fcanc(i,j) + fcancs(i,j))
            gppveg(i,j)= (fcanc(i,j)*gppcgveg(i,j) + &
        &        fcancs(i,j)*gppcsveg(i,j)) / ( fcanc(i,j) + fcancs(i,j))
            pheanveg(i,j)= (fcanc(i,j)*pancgveg(i,j) + &
        &        fcancs(i,j)*pancsveg(i,j)) / ( fcanc(i,j) + fcancs(i,j))
        else
            rmsveg(i,j)= 0.0
            rmrveg(i,j)= 0.0
            rmlveg(i,j)= 0.0
            anveg(i,j)= 0.0
            gppveg(i,j)= 0.0
            pheanveg(i,j)= 0.0
        endif

        if(lfstatus(i,j).eq.4)then
            gppveg(i,j) = anveg(i,j) + rmlveg(i,j)
        endif

        rmveg(i,j)  = rmlveg(i,j) + rmrveg(i,j) + rmsveg(i,j)
        nppveg(i,j) = gppveg(i,j) - rmveg(i,j)

280     continue 
270   continue 
!>
!>Now that we know maintenance respiration from leaf, stem, and root
!>and gpp, we can find growth respiration for each vegetation
!>
do 300 j = 1, icc
  do 310 i = il1, il2
    if( nppveg(i,j).gt.zero ) then
        rgveg(i,j)=grescoef(sort(j))*nppveg(i,j)
    else
        rgveg(i,j)=0.0
    endif
    nppveg(i,j) = nppveg(i,j) - rgveg(i,j)

310     continue
300   continue

!>Calculate grid-averaged rates of rm, rg, npp, and gpp

do 320 j = 1,icc
  do 330 i = il1, il2
    rml(i)=rml(i)+fcancmx(i,j)*rmlveg(i,j)
    rms(i)=rms(i)+fcancmx(i,j)*rmsveg(i,j)
    rmr(i)=rmr(i)+fcancmx(i,j)*rmrveg(i,j)
    rm(i) =rm(i)+fcancmx(i,j)*rmveg(i,j)
    rg(i) =rg(i)+fcancmx(i,j)*rgveg(i,j)
    npp(i)=npp(i)+fcancmx(i,j)*nppveg(i,j)
    gpp(i)=gpp(i)+fcancmx(i,j)*gppveg(i,j)
    autores(i)=rg(i)+rm(i)
    autoresveg(i,j)=rmveg(i,j) + rgveg(i,j)
330     continue
320   continue

!     ------------------------------------------------------------------
!>
!>Heterotrophic respiration
!!
!!Find heterotrophic respiration rates (umol co2/m2/sec) for canopy over snow subarea

call    hetresv ( fcancs,      fcs, litrmass, soilcmas,&
     &                      il1,&
     &                      il2,   tbarcs,   thliqc,     sand,&
     &                     clay, rttempcs,    zbotw,     sort,&
     &                     isand,&
     &                 ltrsvgcs, scrsvgcs, thicec)
!>
!!Find heterotrophic respiration rates for canopy over ground subarea
!!
call    hetresv (  fcanc,       fc, litrmass, soilcmas,&
     &                      il1,&
     &                      il2,    tbarc,   thliqc,     sand,&
     &                     clay, rttempcg,    zbotw,     sort,&
     &                     isand,&
     &                 ltrsvgcg, scrsvgcg, thicec)
!>
!!Find heterotrophic respiration rates from bare ground subarea
!!
call  hetresg  (litrmass, soilcmas,            &
     &                      il1,      il2,     tbarg,   &
     &                   thliqg,     sand,      clay,   zbotw,   &
     &                       fg,        0,&
     &                     isand,&
     &                   ltrsbrg,  scrsbrg, thiceg)
!>
!!Find heterotrophic respiration rates from snow over ground subarea
!!
call  hetresg  (litrmass, soilcmas,            &
     &                      il1,      il2,    tbargs,   &
     &                   thliqg,     sand,      clay,   zbotw,   &
     &                      fgs,        1,&
     &                     isand,&
     &                   ltrsbrgs, scrsbrgs, thiceg)
!>
!!Find vegetation averaged litter and soil c respiration rates
!!using values from canopy over ground and canopy over snow subareas
!!


do 340 j = 1, icc
  do 350 i = il1, il2
    if( (fcanc(i,j)+fcancs(i,j)).gt.zero) then
        ltresveg(i,j)= (fcanc(i,j)*ltrsvgcg(i,j) + &
    &        fcancs(i,j)*ltrsvgcs(i,j)) / ( fcanc(i,j) + fcancs(i,j))
        scresveg(i,j)= (fcanc(i,j)*scrsvgcg(i,j) + &
    &        fcancs(i,j)*scrsvgcs(i,j)) / ( fcanc(i,j) + fcancs(i,j))
        hetrsveg(i,j) =  ltresveg(i,j) + scresveg(i,j)

    else
        ltresveg(i,j)= 0.0
        scresveg(i,j)= 0.0
        hetrsveg(i,j)= 0.0
    endif
    nepveg(i,j)=nppveg(i,j)-hetrsveg(i,j)

350     continue 
340   continue 
!>
!!Find litter and soil c respiration rates averaged over the bare
!!fraction of the grid cell using values from ground and snow over ground sub-areas.
!!
do 355 i = il1, il2
    if( (fg(i)+fgs(i)).gt.zero) then
            ltresveg(i,iccp1)= (fg(i)*ltrsbrg(i) + &
        &      fgs(i)*ltrsbrgs(i)) / ( fg(i) + fgs(i) )
            scresveg(i,iccp1)= (fg(i)*scrsbrg(i) + &
        &      fgs(i)*scrsbrgs(i)) / ( fg(i) + fgs(i) )
            hetrsveg(i,iccp1) =  ltresveg(i,iccp1) + scresveg(i,iccp1)
            nepveg(i,iccp1)=0.-hetrsveg(i,iccp1)
    else
            ltresveg(i,iccp1)= 0.0
            scresveg(i,iccp1)= 0.0
            hetrsveg(i,iccp1)= 0.0
    endif
355   continue
!>
!!Find grid averaged litter and soil c respiration rates
!!
do 360 j = 1,icc
  do 370 i = il1, il2
    litres(i)=litres(i)+fcancmx(i,j)*ltresveg(i,j)
    socres(i)=socres(i)+fcancmx(i,j)*scresveg(i,j)
370     continue
360   continue

do 380 i = il1, il2
    litres(i)=litres(i)+( (fg(i)+fgs(i))*ltresveg(i,iccp1))
    socres(i)=socres(i)+( (fg(i)+fgs(i))*scresveg(i,iccp1))
    hetrores(i)= litres(i)+socres(i)
    nep(i)=npp(i)-hetrores(i)
380   continue
!>
!!Update the litter and soil c pools based on litter and soil c respiration rates 
!!found above. also transfer humidified litter to the soil c pool.
!!
do 420 j = 1, iccp1
  do 430 i = il1, il2

!>Convert u mol co2/m2.sec -> \f$kg c/m^2\f$ respired over the model time step
    ltrestep(i,j)=ltresveg(i,j)*(1.0/963.62)*deltat
    screstep(i,j)=scresveg(i,j)*(1.0/963.62)*deltat

!>Update litter and soil c pools
    if (j .ne. iccp1) then
        litrmass(i,j)=litrmass(i,j)-(ltrestep(i,j)*&
    &                   (1.0+humicfac(sort(j))))
        hutrstep(i,j)=(humicfac(sort(j))* ltrestep(i,j))
    else
        litrmass(i,j)=litrmass(i,j)-(ltrestep(i,j)*(1.0+0.45))
        hutrstep(i,j)=(0.45 * ltrestep(i,j))
    endif

    humtrsvg(i,j)=hutrstep(i,j)*(963.62/deltat) ! u-mol co2/m2.sec
    soilcmas(i,j)=soilcmas(i,j) + &
&          real(spinfast) * (hutrstep(i,j) -  screstep(i,j))

    if(litrmass(i,j).lt.zero) litrmass(i,j)=0.0
    if(soilcmas(i,j).lt.zero) soilcmas(i,j)=0.0
430     continue
420   continue

!>Estimate soil respiration. this is sum of heterotrophic respiratio and root maintenance respiration.

do 440 j = 1, icc
  do 450 i = il1, il2
    soilrsvg(i,j)=ltresveg(i,j)+scresveg(i,j)+rmrveg(i,j)
450     continue
440   continue

!>But over the bare fraction there is no live root.

do 460 i = il1, il2
    soilrsvg(i,iccp1)=ltresveg(i,iccp1)+scresveg(i,iccp1)
460   continue

!>Find grid averaged humification and soil respiration rates

do 470 j = 1,icc
  do 480 i = il1, il2
    soilresp(i)=soilresp(i)+fcancmx(i,j)*soilrsvg(i,j)
    humiftrs(i)=humiftrs(i)+fcancmx(i,j)*humtrsvg(i,j)
480     continue
470   continue

do 490 i = il1, il2
    soilresp(i)=soilresp(i)+( (fg(i)+fgs(i))*soilrsvg(i,iccp1))
    humiftrs(i)=humiftrs(i)+( (fg(i)+fgs(i))*humtrsvg(i,iccp1))
490   continue

!     ------------------------------------------------------------------

!>Find CH4 wetland area (if not prescribed) and emissions:



if (dowetlands .or. obswetf) then
    call  wetland_methane (hetrores, il1, il2, ta, wetfrac,&
     &                        npp, tbar, thliqg, currlat,&  !FLAG consider making this thliqc? JM Aug 2016.
     &                     sand,  slopefrac, & !obswetf,&
     &                  ch4wet1,    ch4wet2,    wetfdyn,&
     &                  ch4dyn1,    ch4dyn2)
endif

!> Calculate the methane that is oxidized by the soil sink
if (dowetlands) then
    call soil_ch4uptake(il1,il2,tbar,THP,BI,thliqg, &
     &                     thicec,PSIS,GRAV,fcan,obswetf, &
     &                     wetfdyn,wetfrac,isand,RHOW, &
     &                     RHOICE,ch4conc,ch4soills)

endif
!    -------------------------------------------------------------------

!>Estimate allocation fractions for leaf, stem, and root components.

call allocate (lfstatus,   thliqc,    ailcg,     ailcb,&
     &                     il1, il2,     sand,     clay,  &
     &                    rmatctem,   gleafmas, stemmass, rootmass,     &
     &                       sort,    nol2pfts,  fcancmx, isand, &
     &                     afrleaf,  afrstem,  afrroot,    wiltsm,&
     &                     fieldsm, wtstatus, ltstatus)
!>
!!Note: fieldsm and wiltsm are calculated in allocate. They are called THFC and PSIWLT in the 
!!CLASS part of the model. They are recalculated here in CTEM to avoid passing them through the
!!coupler in the coupled model. The CTEM calculated versions of fieldsm and wiltsm are also used
!!in disturb and phenolgy. JM. Jan 14 2015.
!!
!!Estimate fraction of npp that is to be used for horizontal
!!expansion (lambda) during the next day (i.e. this will be determining
!!the colonization rate in competition).
!!
if (compete) then
    do 500 j = 1, icc
        if(.not. crop(j)) then   ! not for crops
            do 501 i = il1, il2

            n = sort(j)
            if(ailcg(i,j).le.laimin(n))then
                lambda(i,j)=0.0
            else if(ailcg(i,j).ge.laimax(n))then
                lambda(i,j)=lambdamax
            else
                lambda(i,j)=((ailcg(i,j)-laimin(n))*lambdamax)/&
        &                    (laimax(n)-laimin(n))
            endif

            !>We use the following new function to smooth the transition for lambda as
            !!an abrupt linear increase does not give good results. JM Jun 2014
            if (ailcg(i,j) .gt. laimin(n)*0.25) then
                lambdaalt = cosh((ailcg(i,j) - laimin(n)*0.25) * 0.115) - 1.
            else
                lambdaalt=0.
            end if
            lambda(i,j)=max(lambda(i,j),lambdaalt)

            lambda(i,j)=max(0.0, min(lambdamax, lambda(i,j)))

            !>If tree and leaves still coming out, or if npp is negative, then do not expand
            if((j.le.5.and.lfstatus(i,j).eq.1).or.nppveg(i,j).lt.0.0 &
            &.or..not.pftexist(i,j))then
                lambda(i,j)=0.0
            endif

        501      continue
        endif
    500    continue
endif !compete

!    ------------------------------------------------------------------
!>
!>Maintenance respiration also reduces leaf, stem, and root biomass.
!!when npp for a given pft is positive then this is taken care by
!!allocating +ve npp amongst the leaves, stem, and root component.
!!when npp for a given pft is negative then maintenance respiration
!!loss is explicitly deducted from each component.

      do 600 j = 1, icc
        do 610 i = il1, il2
!>
!!Convert npp and maintenance respiration from different components
!!from units of u mol co2/m2.sec -> \f$kg c/m^2\f$ sequestered or respired over the model time step (deltat)    
      
          gppvgstp(i,j)=gppveg(i,j)*(1.0/963.62)*deltat !+ add2allo(i,j)

!>Remove the cost of making reproductive tissues. This cost can only be removed when NPP is positive.
          !if (compete) then   !FLAG - set up now so only compete on has a reproductive cost. JM
            reprocost(i,j) =max(0.,nppveg(i,j)*repro_fraction)
          !else
          !  reprocost(i,j) = 0.
          !end if

!         Not in use. We now use a constant reproductive cost as the prior formulation
!         produces perturbations that do not allow closing of the C balance. JM Jun 2014.
!          nppvgstp(i,j)=nppveg(i,j)*(1.0/963.62)*deltat*(1.-lambda(i,j))
!     &                  + add2allo(i,j)
          nppvgstp(i,j)=(nppveg(i,j)-reprocost(i,j))*(1.0/963.62)*deltat
!
!         Amount of c related to horizontal expansion
!         Not in use. JM Jun 2014
!         expbalvg(i,j)=-1.0*nppveg(i,j)*deltat*lambda(i,j)+ add2allo(i,j)*(963.62/1.0)

       
          rmlvgstp(i,j)=rmlveg(i,j)*(1.0/963.62)*deltat
          rmsvgstp(i,j)=rmsveg(i,j)*(1.0/963.62)*deltat
          rmrvgstp(i,j)=rmrveg(i,j)*(1.0/963.62)*deltat
!
          if(lfstatus(i,j).ne.4)then
            if(nppvgstp(i,j).gt.0.0) then
              ntchlveg(i,j)=afrleaf(i,j)*nppvgstp(i,j)
              ntchsveg(i,j)=afrstem(i,j)*nppvgstp(i,j)
              ntchrveg(i,j)=afrroot(i,j)*nppvgstp(i,j)
            else
              ntchlveg(i,j)=-rmlvgstp(i,j)+afrleaf(i,j)*gppvgstp(i,j)
              ntchsveg(i,j)=-rmsvgstp(i,j)+afrstem(i,j)*gppvgstp(i,j)
              ntchrveg(i,j)=-rmrvgstp(i,j)+afrroot(i,j)*gppvgstp(i,j)
            endif
          else  !>i.e. if lfstatus.eq.4
!>and since we do not have any real leaves on then we do not take into account co2 uptake by imaginary leaves in carbon budget.
!!rmlvgstp(i,j) should be zero because we set maintenance respiration from storage/imaginary leaves equal to zero. in loop 180 
!!
            ntchlveg(i,j)=-rmlvgstp(i,j) 
            ntchsveg(i,j)=-rmsvgstp(i,j)
            ntchrveg(i,j)=-rmrvgstp(i,j)
!>
!>since no real leaves are on, make allocation fractions equal to zero.
!>
            afrleaf(i,j)=0.0
            afrstem(i,j)=0.0
            afrroot(i,j)=0.0
          endif
!
          gleafmas(i,j)=gleafmas(i,j)+ntchlveg(i,j)
          stemmass(i,j)=stemmass(i,j)+ntchsveg(i,j)
          rootmass(i,j)=rootmass(i,j)+ntchrveg(i,j)
!
!
          if(gleafmas(i,j).lt.0.0)then
            write(6,1900)'gleafmas lt zero at i=',i,' for pft=',j,''   
            write(6,1901)'gleafmas = ',gleafmas(i,j)
            write(6,1901)'ntchlveg = ',ntchlveg(i,j)
            write(6,1902)'lfstatus = ',lfstatus(i,j)
            write(6,1901)'ailcg    = ',ailcg(i,j)
            write(6,1901)'slai     = ',slai(i,j)
1900        format(a23,i4,a10,i2,a1)
1902        format(a11,i4)
            call xit ('ctem',-6)
          endif
!

          if(stemmass(i,j).lt.0.0)then
            write(6,1900)'stemmass lt zero at i=(',i,') for pft=',j,')' 
            write(6,1901)'stemmass = ',stemmass(i,j)
            write(6,1901)'ntchsveg = ',ntchsveg(i,j)
            write(6,1902)'lfstatus = ',lfstatus(i,j)
            write(6,1901)'rmsvgstp = ',rmsvgstp(i,j)
            write(6,1901)'afrstem  = ',afrstem(i,j)
            write(6,1901)'gppvgstp = ',gppvgstp(i,j)
            write(6,1901)'rmscsveg = ',rmscsveg(i,j)
            write(6,1901)'rmscgveg = ',rmscgveg(i,j)
1901        format(a11,f12.8)
            call xit ('ctem',-7)
          endif
!
          if(rootmass(i,j).lt.0.0)then
            write(6,1900)'rootmass lt zero at i=(',i,') for pft=',j,')' 
            write(6,1901)'rootmass = ',rootmass(i,j)
            call xit ('ctem',-8)
          endif
!>
!!convert net change in leaf, stem, and root biomass into 
!!u-mol co2/m2.sec for use in balcar subroutine
!!         
          ntchlveg(i,j)=ntchlveg(i,j)*(963.62/deltat)         
          ntchsveg(i,j)=ntchsveg(i,j)*(963.62/deltat)         
          ntchrveg(i,j)=ntchrveg(i,j)*(963.62/deltat)         
!>
!!to avoid over/underflow problems set gleafmas, stemmass, and
!!rootmass to zero if they get too small
!!
          if(bleafmas(i,j).lt.zero) bleafmas(i,j)=0.0
          if(gleafmas(i,j).lt.zero) gleafmas(i,j)=0.0
          if(stemmass(i,j).lt.zero) stemmass(i,j)=0.0
          if(rootmass(i,j).lt.zero) rootmass(i,j)=0.0
!
610     continue
600   continue
!>  
!>calculate grid averaged value of C related to spatial expansion
!>
      do 620 j = 1,icc
        do 621 i = il1, il2
         !if (compete .or. lnduseon) then
!           Not in use. We now use the constant reproductive cost below. JM Jun 2014
!           expnbaln(i)=expnbaln(i)+fcancmx(i,j)*expbalvg(i,j)
            repro_cost_g(i)=repro_cost_g(i)+fcancmx(i,j)*reprocost(i,j) 
         !endif
621     continue
620   continue
!
!    ------------------------------------------------------------------

!>Phenology part starts
!!
!!the phenology subroutine determines leaf status for each pft and calculates leaf litter.
!!the phenology subroutine uses soil temperature (tbar) and root temperature. however, 
!!since ctem doesn't make the distinction between canopy over ground, and canopy over 
!!snow sub-areas for phenology purposes (for  example, leaf onset is not assumed to occur 
!!at different times over these sub-areas) we use average soil and root temperature in the phenology subroutine.
!!
!!calculate average soil temperature and root temperature using values for canopy over ground and canopy over snow sub-areas, for each vegetation type.
!!
do 650 j = 1, icc
  do 660 i = il1, il2
    if( (fcanc(i,j)+fcancs(i,j)).gt.zero) then
        roottemp(i,j)= (fcanc(i,j)*rttempcg(i,j) + &
    &        fcancs(i,j)*rttempcs(i,j)) / ( fcanc(i,j) + fcancs(i,j))
    else
        roottemp(i,j)= rttempcg(i,j)
    endif
660     continue
650   continue

do 680 j = 1, ignd
  do 690 i = il1, il2
    if( (fc(i)+fcs(i)).gt.zero) then
        tbarccs(i,j)= (fc(i)*tbarc(i,j) + &
&        fcs(i)*tbarcs(i,j)) / ( fc(i) + fcs(i))
    else
        tbarccs(i,j)= tbar(i,j)
    endif
690     continue
680   continue
!>
!!Call the phenology subroutine, which determines the leaf growth
!!status, calculates leaf litter, and converts green grass into brown.
!!

call phenolgy(gleafmas, bleafmas, &
     &                         il1,      il2, leapnow,  tbarccs,&
     &                      thliqc,   wiltsm,  fieldsm,       ta,&
     &                    pheanveg,     iday,     radj, roottemp,&
     &                    rmatctem, stemmass, rootmass,     sort,&
     &                    nol2pfts,  fcancmx,&
     &                    flhrloss, leaflitr, lfstatus,  pandays,&
     &                    colddays)

!    -------------------------------------------------------------------
!>
!!while leaf litter is calculated in the phenology subroutine, stem
!!and root turnover is calculated in the turnover subroutine.
!!
call turnover (stemmass, rootmass,  lfstatus,    ailcg,&
     &                          il1,      il2,   leapnow,&
     &                         sort, nol2pfts,  fcancmx,&
     &                     stmhrlos, rothrlos,&
     &                     stemlitr, rootlitr)

!>Update green leaf biomass for trees and crops and brown leaf biomass for grasses

k1=0
do 700 j = 1, ican
    if(j.eq.1) then
        k1 = k1 + 1
    else
        k1 = k1 + nol2pfts(j-1)
    endif
    k2 = k1 + nol2pfts(j) - 1
    do 705 m = k1, k2
        do 710 i = il1, il2

            if(j.le.3)then    ! trees and crops
                gleafmas(i,m)=gleafmas(i,m)-leaflitr(i,m)
                if( gleafmas(i,m).lt.0.0) then
                    leaflitr(i,m)=leaflitr(i,m)+gleafmas(i,m)
                    gleafmas(i,m)=0.0
                endif
            else              ! grasses
                bleafmas(i,m)=bleafmas(i,m)-leaflitr(i,m)
                if( bleafmas(i,m).lt.0.0) then
                    leaflitr(i,m)=leaflitr(i,m)+bleafmas(i,m)
                    bleafmas(i,m)=0.0
                endif
            endif

710     continue
705    continue
700   continue

!>Update stem and root biomass for litter deductions

do 780 j = 1, icc
  do 790 i = il1, il2
    stemmass(i,j)=stemmass(i,j)-stemlitr(i,j)
    if( stemmass(i,j).lt.0.0) then
        stemlitr(i,j)=stemlitr(i,j)+stemmass(i,j)
        stemmass(i,j)=0.0
    endif

    rootmass(i,j)=rootmass(i,j)-rootlitr(i,j)
    if( rootmass(i,j).lt.0.0) then
        rootlitr(i,j)=rootlitr(i,j)+rootmass(i,j)
        rootmass(i,j)=0.0
    endif
790     continue
780   continue
!>
!>Update litter pool with leaf litter calculated in the phenology
!>subroutine and stem and root litter calculated in the turnover
!>subroutine. Also add the reproduction carbon directly to the litter pool
!>
do 800 j = 1, icc
  do 810 i = il1, il2
    litrmass(i,j)=litrmass(i,j) + leaflitr(i,j) + stemlitr(i,j) +&
&                  rootlitr(i,j) + reprocost(i,j)*(1.0/963.62)&
&                       *deltat
810     continue
800   continue

!    ------------------------------------------------------------------
!>
!>Call the mortaliy subroutine which calculates mortality due to reduced growth and aging. exogenous mortality due to fire and other
!!disturbances and the subsequent litter that is generated is calculated in the disturb subroutine.
!!
!!set maxage >0 in ctem_params.f90 to switch on mortality due to age and
!!reduced growth. Mortality is linked to the competition parameterization and generates bare fraction.
!!
Call       mortalty (stemmass, rootmass,        ailcg, gleafmas,&
     &                     bleafmas,      il1, il2, leapnow, &
     &                         iday,     sort,  fcancmx,  &
     &                     lystmmas,     lyrotmas, tymaxlai,&
     &                     grwtheff, stemltrm,     rootltrm, glealtrm,&
     &                     geremort, intrmort)
!>
!>Update leaf, stem, and root biomass pools to take into loss due to mortality, and put the 
!!litter into the litter pool. the mortality for green grasses doesn't generate litter, instead they turn brown.
!!
      k1=0
      do 830 j = 1, ican 
       if(j.eq.1) then
         k1 = k1 + 1
       else
         k1 = k1 + nol2pfts(j-1)
       endif
       k2 = k1 + nol2pfts(j) - 1
       do 835 m = k1, k2
        do 840 i = il1, il2
!
          stemmass(i,m)=stemmass(i,m)-stemltrm(i,m)
          rootmass(i,m)=rootmass(i,m)-rootltrm(i,m)
          litrmass(i,m)=litrmass(i,m)+stemltrm(i,m)+rootltrm(i,m)  
!
          if(j.eq.4)then    ! grasses
            gleafmas(i,m)=gleafmas(i,m)-glealtrm(i,m)
            bleafmas(i,m)=bleafmas(i,m)+glealtrm(i,m)
            glealtrm(i,m)=0.0
          else              ! trees and crops
            gleafmas(i,m)=gleafmas(i,m)-glealtrm(i,m)
          endif
          litrmass(i,m)=litrmass(i,m)+glealtrm(i,m)
!
840     continue
835    continue 
830   continue 

!    ------------------------------------------------------------------
!>
!>call the disturbance subroutine which calculates mortality due to fire and other disturbances.
!>the primary output from from disturbance subroutine is litter generated, c emissions due to fire
!!and area burned, which may be used to estimate change in fractional coverages.
!!
!!disturbance is spatial and requires area of gcm grid cell and areas of different pfts present in 
!!a given grid cell. however, when ctem is operated at a point scale then it is assumed that the 
!!spatial scale is 1 hectare = 10,000 m2. the disturbance subroutine may be stopped from simulating 
!!any fire by specifying fire extingushing probability equal to 1.
!!
call disturb (stemmass, rootmass, gleafmas, bleafmas,&
     &                      thliqc,   wiltsm,  fieldsm,    uwind,&
     &                       vwind,  lightng,  fcancmx, litrmass,&
     &                    prbfrhuc, rmatctem, extnprob, popdon,&
     &                         il1,      il2,     sort, nol2pfts,&
     &                    grclarea,   thicec,   popdin, lucemcom,&
     &                      dofire,  currlat,     iday, fsnow,&
!    in above, out below 
     &                    stemltdt, rootltdt, glfltrdt, blfltrdt,&
     &                    glcaemls, rtcaemls, stcaemls,&
     &                    blcaemls, ltrcemls, burnfrac,smfunc_veg,&
     &                    emit_co2, emit_co,  emit_ch4, emit_nmhc,&
     &                    emit_h2,  emit_nox, emit_n2o, emit_pm25,&
     &                    emit_tpm, emit_tc,  emit_oc,  emit_bc,&
     &                    burnvegf, bterm_veg,mterm_veg,  lterm,&
     &                    pstemmass, pgleafmass )  

!    ------------------------------------------------------------------
!>
!>Calculate nbp (net biome production) for each pft by taking into account
!!C emission losses. The disturbance routine produces emissions due to fire
!!and it also calculates emissions due to LUC. These LUC carbon emissions due to
!!combustion associated with LUC are first estimated in LUC. This flux is spread out over
!!the whole year and is therefore subtracted to get NBP of each pft
!!as well as the grid averaged value of NBP. Also LUC related combustion flux
!!is assumed to be spread uniformly over the grid cell and thus reduces NBP of each PFT
!!
      do 1000 i = il1, il2
        do 1010 j = 1, icc
          dscemlv1(i,j)=glcaemls(i,j) + blcaemls(i,j) + stcaemls(i,j) +&
     &                  rtcaemls(i,j) 
          dscemlv2(i,j)=glcaemls(i,j) + blcaemls(i,j) + stcaemls(i,j) +&
     &                  rtcaemls(i,j) + ltrcemls(i,j)

!         convert \f$kg c/m^2\f$ emitted in one day into u mol co2/m2.sec before
!         subtracting emission losses from nep. 
          nbpveg(i,j)  =nepveg(i,j)   - dscemlv2(i,j)*(963.62/deltat)

1010    continue

!       For accounting purposes, we also need to account for the bare fraction NBP
!       Since there is no fire on the bare, we use 0. 
          nbpveg(i,iccp1)  =nepveg(i,iccp1)   - 0. 

1000  continue
!>
!!Calculate grid. averaged rate of carbon emissions due to fire in u-mol co2/m2.sec. convert all emission losses from \f$kg c/m^2\f$
!!emitted in 1 day to u-mol co2/m2.sec. calculate grid averaged carbon emission losses from litter.
!!
      do 1030 j = 1,icc
        do 1040 i = il1, il2
          dstcemls1(i)=dstcemls1(i) +&
     &     fcancmx(i,j)*dscemlv1(i,j)*(963.62/deltat)
          dstcemls2(i)=dstcemls2(i) +&
     &     fcancmx(i,j)*dscemlv2(i,j)*(963.62/deltat)
          galtcels(i)=galtcels(i) +&
     &     fcancmx(i,j)*ltrcemls(i,j)*(963.62/deltat)
          glcaemls(i,j)=glcaemls(i,j)*(963.62/deltat)
          blcaemls(i,j)=blcaemls(i,j)*(963.62/deltat)
          stcaemls(i,j)=stcaemls(i,j)*(963.62/deltat)
          rtcaemls(i,j)=rtcaemls(i,j)*(963.62/deltat)
          ltrcemls(i,j)=ltrcemls(i,j)*(963.62/deltat)
1040    continue
1030  continue
!

      do 1041 i = il1, il2
        nbp(i)=nep(i)-dstcemls2(i)
        dstcemls3(i)=dstcemls2(i)-dstcemls1(i)
1041  continue
!>
!!calculate total litter fall from each component (leaves, stem, and root) from all causes (normal
!!turnover, drought and cold stress for leaves, mortality, and disturbance) for use in balcar subroutine
!!
      do 1050 j = 1,icc
        do 1060 i = il1, il2
!>    
!>units here are \f$kg c/m^2 .day\f$
         tltrleaf(i,j)=leaflitr(i,j)+glealtrm(i,j)+glfltrdt(i,j)+&
     &                 blfltrdt(i,j)
         tltrstem(i,j)=stemlitr(i,j)+stemltrm(i,j)+stemltdt(i,j)
         tltrroot(i,j)=rootlitr(i,j)+rootltrm(i,j)+rootltdt(i,j)
!>       
!>convert units to u-mol co2/m2.sec
         leaflitr(i,j)=leaflitr(i,j)*(963.62/deltat)
         tltrleaf(i,j)=tltrleaf(i,j)*(963.62/deltat)
         tltrstem(i,j)=tltrstem(i,j)*(963.62/deltat)
         tltrroot(i,j)=tltrroot(i,j)*(963.62/deltat)
1060    continue
1050  continue
!>
!>calculate grid-average vegetation biomass, litter mass, and soil carbon mass, and litter fall rate
!>
      do 1100 j = 1, icc
        do 1110 i = il1, il2
          vgbiomas(i)=vgbiomas(i)+fcancmx(i,j)*(gleafmas(i,j)+&
     &     bleafmas(i,j)+stemmass(i,j)+rootmass(i,j))
          litrfall(i)=litrfall(i)+fcancmx(i,j)*(tltrleaf(i,j)+&
     &     tltrstem(i,j)+tltrroot(i,j))
          ! store the per PFT litterfall for outputting.
          litrfallveg(i,j)=(tltrleaf(i,j)+tltrstem(i,j)+tltrroot(i,j))
          gavgltms(i)=gavgltms(i)+fcancmx(i,j)*litrmass(i,j)
          gavgscms(i)=gavgscms(i)+fcancmx(i,j)*soilcmas(i,j)
          vgbiomas_veg(i,j)=gleafmas(i,j)+&
     &     bleafmas(i,j)+stemmass(i,j)+rootmass(i,j) !vegetation biomass for each pft
1110    continue
1100  continue
!
      do 1020 i = il1, il2
        gavgltms(i)=gavgltms(i)+( (fg(i)+fgs(i))*litrmass(i,iccp1))
        gavgscms(i)=gavgscms(i)+( (fg(i)+fgs(i))*soilcmas(i,iccp1))

1020  continue

!     -----------------------------------------------------------------
!>
!>At this stage we have all required fluxes in u-mol co2/m2.sec and initial (loop 140 and 145) 
!!and updated sizes of all pools (in \f$kg c/m^2\f$). Now we call the balcar subroutine and make sure 
!!that C in leaves, stem, root, litter and soil C pool balances within a certain tolerance.
!!
if(spinfast.eq.1)then
        call  balcar(gleafmas, stemmass, rootmass,  bleafmas,&
&                    litrmass, soilcmas, ntchlveg,  ntchsveg,&
&                    ntchrveg, tltrleaf, tltrstem,  tltrroot,&
&                    glcaemls, blcaemls, stcaemls,  rtcaemls,&
&                    ltrcemls, ltresveg, scresveg,  humtrsvg,&
&                    pglfmass, pblfmass, pstemass,  protmass,&
&                    plitmass, psocmass, vgbiomas,  reprocost,&
&                    pvgbioms, gavgltms, pgavltms,  gavgscms,&
&                    pgavscms, galtcels, repro_cost_g,&
&                         npp,  autores, hetrores,       gpp,&
&                         nep,   litres,   socres, dstcemls1,&
&                         nbp, litrfall, humiftrs,&
&                         il1,       il2)
endif

!     -----------------------------------------------------------------
!>
!>Finally find vegetation structural attributes which can be passed to the land surface scheme using leaf, stem, and root biomass. 
!>
call bio2str( gleafmas, bleafmas, stemmass, rootmass,&
     &                            il1,      il2, fcancmx,    zbotw,&
     &                          delzw, nol2pfts,  soildpth,&
     &                          ailcg,    ailcb,     ailc,    zolnc,&
     &                          rmatc, rmatctem,     slai,  bmasveg,&
     &                       cmasvegc,  veghght, rootdpth,   alvisc,&
     &                         alnirc,  paicgat,  slaicgat  )

!>
!>Calculation of gavglai is moved from loop 1100 to here since ailcg is updated by bio2str
!>
do j = 1, icc
    do i = il1, il2
        gavglai (i)=gavglai (i)+fcancmx(i,j)*ailcg(i,j)
    enddo
enddo

return
end

