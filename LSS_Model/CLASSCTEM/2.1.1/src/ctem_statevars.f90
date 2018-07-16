!>\defgroup ctem_statevars

!>
!!this module contains the variable type structures:
!! 1. c_switch - switches for running CTEM, read from the joboptions file
!! 2. vrot - CTEM's 'rot' vars
!! 3. vgat - CTEM's 'gat' vars
!! 4. class_out - CLASS's monthly outputs
!! 5. ctem_grd - CTEM's grid average variables
!! 6. ctem_tile - CTEM's variables per tile
!! 7. ctem_mo - CTEM's variables monthly averaged (per pft)
!! 8. ctem_grd_mo - CTEM's grid average monthly values
!! 9. ctem_tile_mo - CTEM's variables per tile monthly values
!! 10. ctem_yr - CTEM's average annual values (per PFT)
!! 11. ctem_grd_yr - CTEM's grid average annual values
!! 12. ctem_tile_yr - CTEM's variables per tile annual values
!
!>\file
module ctem_statevars

! J. Melton Apr 2015

use ctem_params,  only : initpftpars, nlat, nmos, ilg, nmon,ican, ignd,icp1, icc, iccp1, &
                    monthend, mmday,modelpft, l2max,deltat, abszero, monthdays,seed, crop, NBS

implicit none

public :: initrowvars
public :: resetclassmon
public :: resetclassyr
public :: resetdaily
public :: resetmonthend
public :: resetyearend
public :: resetclassaccum
public :: resetgridavg
public :: finddaylength

!=================================================================================
!>switches for running CTEM, read from the joboptions file
type ctem_switches

    logical :: ctem_on     !<
    logical :: parallelrun !<set this to be true if model is run in parallel mode for 
                           !<multiple grid cells, output is limited to monthly & yearly 
                    	   !<grid-mean only. else the run is in stand alone mode, in which 
                     	   !<output includes half-hourly and daily and mosaic-mean as well.
    logical :: cyclemet    !<to cycle over only a fixed number of years 
                 	   !<(nummetcylyrs) starting at a certain year (metcylyrst)
                 	   !<if cyclemet, then put co2on = false and set an appopriate setco2conc, also
                 	   !<if popdon is true, it will choose the popn and luc data for year
                 	   !<metcylyrst and cycle on that.
    logical :: dofire      !<boolean, if true allow fire, if false no fire.
    logical :: run_model   !<
    logical :: met_rewound !<
    logical :: reach_eof   !<
    logical :: compete     !<logical boolean telling if competition between pfts is on or not
    logical :: start_bare  !<set this to true if competition is true, and if you wish to start from bare ground.
                           !<if this is set to false, the ini and ctm file info will be used to set up the run.
                           !<NOTE: This still keeps the crop fractions (while setting all pools to zero)
    logical :: rsfile      !<set this to true if restart files (.ini_rs and .ctm_rs) are written at the end of each
                           !<year. these files are necessary for checking whether the model reaches equilibrium after
                           !<running for a certain years. set this to false if restart files are not needed
                           !<(known how many years the model will run)
    logical :: lnduseon    !<logical switch to run the land use change subroutine or not.
    logical :: co2on       !<use \f$co_2\f$ time series, set to false if cyclemet is true
    logical :: ch4on       !<use \f$CH_4\f$ time series, set to false if cyclemet is true the \f$CO_2\f$ timeseries is in the 
                           !<same input file as the \f$CO_2\f$ one.
    logical :: popdon      !<if set true use population density data to calculate fire extinguishing probability and
                           !<probability of fire due to human causes, or if false, read directly from .ctm file
    logical :: inibioclim  !<switch telling if bioclimatic parameters are being initialized
                           !<from scratch (false) or being initialized from some spun up
                           !<values(true).
    logical :: start_from_rs!<if true, this option copies the _RS INI and CTM files to be the .INI and .CTM files and
                           !<then starts the run as per normal. it is handy when spinning up so you don't have to do a
                           !<complicated copying of the RS files to restart from them. NOTE! This will not work on
                           !<hadar or spica, instead you have to manually move the files and set this to .false.    
    logical :: leap        !< set to true if all/some leap years in the .MET file have data for 366 days
                           !< also accounts for leap years in .MET when cycling over meteorology (cyclemet)
    logical :: dowetlands   !<if true allow wetland methane emission
    logical :: obswetf      !<observed wetland fraction
    logical :: transient_run!<

    character(80) :: titlec1!<
    character(80) :: titlec2!<
    character(80) :: titlec3!<

end type ctem_switches

type (ctem_switches), save, target :: c_switch

!=================================================================================
!>CTEM's 'rot' vars
type veg_rot

    ! This is the basic data structure that contains the state variables
    ! for the Plant Functional Type (PFT). The dimensions are nlat,nmos,{icc,iccp1}

    real, dimension(nlat,nmos,icc) :: ailcmin      !<
    real, dimension(nlat,nmos,icc) :: ailcmax      !<
    real, dimension(nlat,nmos,icc) :: dvdfcan      !<
    real, dimension(nlat,nmos,icc) :: gleafmas     !<green leaf mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,icc) :: bleafmas     !<brown leaf mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,icc) :: stemmass     !<stem mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,icc) :: rootmass     !<root mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,icc) :: pstemmass    !<stem mass from previous timestep, is value before fire. used by burntobare subroutine
    real, dimension(nlat,nmos,icc) :: pgleafmass   !<root mass from previous timestep, is value before fire. used by burntobare subroutine
    real, dimension(nlat,nmos,icc) :: fcancmx      !<max. fractional coverage of ctem's 9 pfts, but this can be
                                                   !<modified by land-use change, and competition between pfts
    real, dimension(nlat,nmos) :: gavglai          !<grid averaged green leaf area index

    real, dimension(nlat,nmos,ican) :: zolnc       !<lumped log of roughness length for class' 4 pfts
    real, dimension(nlat,nmos,ican) :: ailc        !<lumped lai for class' 4 pfts

    real, dimension(nlat,nmos,icc) :: ailcg        !<green lai for ctem's 9 pfts
    real, dimension(nlat,nmos,icc) :: ailcgs       !<GREEN LAI FOR CANOPY OVER SNOW SUB-AREA
    real, dimension(nlat,nmos,icc) :: fcancs       !<FRACTION OF CANOPY OVER SNOW FOR CTEM's 9 PFTs
    real, dimension(nlat,nmos,icc) :: fcanc        !<FRACTIONAL COVERAGE OF 8 CARBON PFTs, CANOPY OVER SNOW

    real, dimension(nlat,nmos) :: co2conc          !<ATMOS. CO2 CONC. IN PPM
    real, dimension(nlat,nmos) :: ch4conc          !<

    real, dimension(nlat,nmos,icc) :: co2i1cg      !<INTERCELLULAR CO2 CONC FOR 8 PFTs FOR CANOPY OVER GROUND SUBAREA (Pa) - FOR SINGLE/SUNLIT LEAF
    real, dimension(nlat,nmos,icc) :: co2i1cs      !<SAME AS ABOVE BUT FOR SHADED LEAF (above being co2i1cg)
    real, dimension(nlat,nmos,icc) :: co2i2cg      !<INTERCELLULAR CO2 CONC FOR 8 PFTs FOR CANOPY OVER SNOWSUBAREA (Pa) - FOR SINGLE/SUNLIT LEAF
    real, dimension(nlat,nmos,icc) :: co2i2cs      !<SAME AS ABOVE BUT FOR SHADED LEAF (above being co2i2cg)
    real, dimension(nlat,nmos,icc) :: ancsveg      !<net photosynthetic rate for ctems 9 pfts for canopy over snow subarea
    real, dimension(nlat,nmos,icc) :: ancgveg      !<net photosynthetic rate for ctems 9 pfts for canopy over ground subarea
    real, dimension(nlat,nmos,icc) :: rmlcsveg     !<leaf respiration rate for ctems 9 pfts forcanopy over snow subarea
    real, dimension(nlat,nmos,icc) :: rmlcgveg     !<leaf respiration rate for ctems 9 pfts forcanopy over ground subarea
    real, dimension(nlat,nmos,icc) :: slai         !<storage/imaginary lai for phenology purposes
    real, dimension(nlat,nmos,icc) :: ailcb        !<brown lai for ctem's 9 pfts. for now we assume only grasses can have brown lai
    real, dimension(nlat,nmos)     :: canres       !<
    real, dimension(nlat,nmos,icc) :: flhrloss     !<fall or harvest loss for deciduous trees and crops, respectively, \f$kg c/m^2\f$il1
    real, dimension(nlat,nmos,icc) :: grwtheff     !<growth efficiency. change in biomass per year per unit max.
                                                   !<lai (\f$kg c/m^2\f$)/(m2/m2), for use in mortality subroutine
    real, dimension(nlat,nmos,icc) :: lystmmas     !<stem mass at the end of last year
    real, dimension(nlat,nmos,icc) :: lyrotmas     !<root mass at the end of last year
    real, dimension(nlat,nmos,icc) :: tymaxlai     !<this year's maximum lai
    real, dimension(nlat,nmos)     :: vgbiomas     !<grid averaged vegetation biomass, \f$kg c/m^2\f$
    real, dimension(nlat,nmos)     :: gavgltms     !<grid averaged litter mass, \f$kg c/m^2\f$
    real, dimension(nlat,nmos)     :: gavgscms     !<grid averaged soil c mass, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,icc) :: stmhrlos     !<stem harvest loss for crops, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,ican,ignd) :: rmatc  !<fraction of roots for each of class' 4 pfts in each soil layer
    real, dimension(nlat,nmos,icc,ignd) :: rmatctem!<fraction of roots for each of ctem's 9 pfts in each soil layer
    real, dimension(nlat,nmos,iccp1) :: litrmass   !<litter mass for each of the 9 ctem pfts + bare, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,iccp1) :: soilcmas   !<soil carbon mass for each of the 9 ctem pfts + bare, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,icc) :: vgbiomas_veg !<vegetation biomass for each pft

    ! c     Fire-related variables

    real, dimension(nlat,nmos,icc) :: emit_co2     !<carbon dioxide
    real, dimension(nlat,nmos,icc) :: emit_co      !<carbon monoxide
    real, dimension(nlat,nmos,icc) :: emit_ch4     !<methane
    real, dimension(nlat,nmos,icc) :: emit_nmhc    !<non-methane hydrocarbons
    real, dimension(nlat,nmos,icc) :: emit_h2      !<hydrogen gas
    real, dimension(nlat,nmos,icc) :: emit_nox     !<nitrogen oxides
    real, dimension(nlat,nmos,icc) :: emit_n2o     !<nitrous oxide
    real, dimension(nlat,nmos,icc) :: emit_pm25    !<particulate matter less than 2.5 um in diameter
    real, dimension(nlat,nmos,icc) :: emit_tpm     !<total particulate matter
    real, dimension(nlat,nmos,icc) :: emit_tc      !<total carbon
    real, dimension(nlat,nmos,icc) :: emit_oc      !<organic carbon
    real, dimension(nlat,nmos,icc) :: emit_bc      !<black carbon
    real, dimension(nlat,nmos)     :: burnfrac     !<areal fraction burned due to fire for every grid cell (%)
    real, dimension(nlat,nmos,icc) :: burnvegf     !<per PFT fraction burned of that PFT's area
    real, dimension(nlat,nmos,icc) :: smfuncveg    !<
    real, dimension(nlat,nmos)     :: popdin       !<population density \f$(people / km^2)\f$
    real, dimension(nlat,nmos,icc) :: bterm        !<biomass term for fire probabilty calc
    real, dimension(nlat,nmos)     :: lterm        !<lightning term for fire probabilty calc
    real, dimension(nlat,nmos,icc) :: mterm        !<moisture term for fire probabilty calc

    real, dimension(nlat,nmos)     :: extnprob     !<fire extingusinging probability
    real, dimension(nlat,nmos)     :: prbfrhuc     !<probability of fire due to human causes
    real, dimension(nlat,nmos,12)  :: mlightng     !<
    real, dimension(nlat) :: dayl_max !< maximum daylength for that location (hours)
    real, dimension(nlat) :: dayl     !< daylength for that location (hours)


    real, dimension(nlat,nmos,icc)  :: bmasveg  !<total (gleaf + stem + root) biomass for each ctem pft, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,ican) :: cmasvegc !<total canopy mass for each of the 4 class pfts. recall that
                                                !<class requires canopy mass as an input, and this is now provided by ctem. \f$kg/m^2\f$.
    real, dimension(nlat,nmos,icc)  :: veghght  !<vegetation height (meters)
    real, dimension(nlat,nmos,icc)  :: rootdpth !<99% soil rooting depth (meters)
                                                !<both veghght & rootdpth can be used as diagnostics to see
                                                !<how vegetation grows above and below ground, respectively
    real, dimension(nlat,nmos)      :: rml      !<leaf maintenance respiration (u-mol co2/m2.sec)
    real, dimension(nlat,nmos)      :: rms      !<stem maintenance respiration (u-mol co2/m2.sec)
    real, dimension(nlat,nmos,icc)  :: tltrleaf !<total leaf litter fall rate (u-mol co2/m2.sec)
    real, dimension(nlat,nmos,icc)  :: tltrstem !<total stem litter fall rate (u-mol co2/m2.sec)
    real, dimension(nlat,nmos,icc)  :: tltrroot !<total root litter fall rate (u-mol co2/m2.sec)
    real, dimension(nlat,nmos,icc)  :: leaflitr !<leaf litter fall rate (u-mol co2/m2.sec). this leaf litter 
                                                !<does not include litter generated due to mortality/fire
    real, dimension(nlat,nmos,icc)  :: roottemp !<root temperature, k
    real, dimension(nlat,nmos,icc)  :: afrleaf  !<allocation fraction for leaves
    real, dimension(nlat,nmos,icc)  :: afrstem  !<allocation fraction for stem
    real, dimension(nlat,nmos,icc)  :: afrroot  !<allocation fraction for root
    real, dimension(nlat,nmos,icc)  :: wtstatus !<soil water status used for calculating allocation fractions
    real, dimension(nlat,nmos,icc)  :: ltstatus !<light status used for calculating allocation fractions
    real, dimension(nlat,nmos)      :: rmr      !<root maintenance respiration (u-mol co2/m2.sec)

    real, dimension(nlat,nmos,8)  :: slopefrac  !<prescribed fraction of wetlands based on slope
                                                !<only(0.025, 0.05, 0.1, 0.15, 0.20, 0.25, 0.3 and 0.35 percent slope thresholds)
    real, dimension(nlat,nmos)    :: ch4wet1    !<methane flux from wetlands calculated using hetrores in umol ch4/m2.s
    real, dimension(nlat,nmos)    :: ch4wet2    !<methane flux from wetlands calculated using npp in umol ch4/m2.s
    real, dimension(nlat,nmos)    :: wetfdyn    !<dynamic wetland fraction
    real, dimension(nlat,nmos)    :: ch4dyn1    !<methane flux from wetlands calculated using hetrores 
                                                !<and wetfdyn, in umol ch4/m2.s
    real, dimension(nlat,nmos)    :: ch4dyn2    !<methane flux from wetlands calculated using npp and wetfdyn, 
                                                !<in umol ch4/m2.s
    real, dimension(nlat,nmos,12) :: wetfrac_mon!<
    real, dimension(nlat,nmos)    :: ch4_soills !<Methane uptake into the soil column ($mg CH_4 m^{-2} s^{-1}$)

    real, dimension(nlat,nmos) :: lucemcom !<land use change (luc) related combustion emission losses, u-mol co2/m2.sec
    real, dimension(nlat,nmos) :: lucltrin !<luc related inputs to litter pool, u-mol co2/m2.sec
    real, dimension(nlat,nmos) :: lucsocin !<luc related inputs to soil c pool, u-mol co2/m2.sec

    real, dimension(nlat,nmos) :: npp      !<net primary productivity
    real, dimension(nlat,nmos) :: nep      !<net ecosystem productivity
    real, dimension(nlat,nmos) :: nbp      !<net biome productivity
    real, dimension(nlat,nmos) :: gpp      !<gross primary productivity
    real, dimension(nlat,nmos) :: hetrores !<heterotrophic respiration
    real, dimension(nlat,nmos) :: autores  !<autotrophic respiration
    real, dimension(nlat,nmos) :: soilcresp!<
    real, dimension(nlat,nmos) :: rm       !<maintenance respiration
    real, dimension(nlat,nmos) :: rg       !<growth respiration
    real, dimension(nlat,nmos) :: litres   !<litter respiration
    real, dimension(nlat,nmos) :: socres   !<soil carbon respiration
    real, dimension(nlat,nmos) :: dstcemls !<carbon emission losses due to disturbance, mainly fire
    real, dimension(nlat,nmos) :: litrfall !<total litter fall (from leaves, stem, and root) due to 
                                           !<all causes (mortality, turnover, and disturbance)
    real, dimension(nlat,nmos) :: humiftrs !<transfer of humidified litter from litter to soil c pool

    real, dimension(nlat,nmos,icc)   :: gppveg      !<!gross primary productity for each pft
    real, dimension(nlat,nmos,iccp1) :: nepveg      !<net ecosystem productity for bare fraction expnbaln(i)=0.0 amount
                                                    !<of c related to spatial expansion Not used JM Jun 2014 
                                                    !<OR net ecosystem productity for each pft
    real, dimension(nlat,nmos,iccp1) :: nbpveg      !<net biome productity for bare fraction OR net biome productity for each pft
    real, dimension(nlat,nmos,icc)   :: nppveg      !<npp for individual pfts,  u-mol co2/m2.sec
    real, dimension(nlat,nmos,iccp1) :: hetroresveg !<
    real, dimension(nlat,nmos,icc)   :: autoresveg  !<
    real, dimension(nlat,nmos,iccp1) :: litresveg   !<
    real, dimension(nlat,nmos,iccp1) :: soilcresveg !<
    real, dimension(nlat,nmos,icc)   :: rmlvegacc   !<
    real, dimension(nlat,nmos,icc)   :: rmsveg      !<stem maintenance resp. rate for each pft
    real, dimension(nlat,nmos,icc)   :: rmrveg      !<root maintenance resp. rate for each pft
    real, dimension(nlat,nmos,icc)   :: rgveg       !<growth resp. rate for each pft
    real, dimension(nlat,nmos,icc)   :: litrfallveg !<litter fall in \f$kg c/m^2\f$ for each pft
    real, dimension(nlat,nmos,iccp1) :: humiftrsveg !<

    real, dimension(nlat,nmos,icc)  :: rothrlos !<root death as crops are harvested, \f$kg c/m^2\f$
    real, dimension(nlat,nmos,icc)  :: pfcancmx !<previous year's fractional coverages of pfts
    real, dimension(nlat,nmos,icc)  :: nfcancmx !<next year's fractional coverages of pfts
    real, dimension(nlat,nmos,ican) :: alvsctm  !<
    real, dimension(nlat,nmos,ican) :: paic     !<plant area index for class' 4 pfts. this is the sum of leaf
                                                !<area index and stem area index.
    real, dimension(nlat,nmos,ican) :: slaic    !<storage lai. this will be used as min. lai that class sees
                                                !<so that it doesn't blow up in its stomatal conductance calculations.
    real, dimension(nlat,nmos,ican) :: alirctm  !<
    real, dimension(nlat,nmos)      :: cfluxcg  !<
    real, dimension(nlat,nmos)      :: cfluxcs  !<
    real, dimension(nlat,nmos)      :: dstcemls3!<carbon emission losses due to disturbance (fire at present) from litter pool
    real, dimension(nlat,nmos,icc)  :: anveg    !<net photosynthesis rate for each pft
    real, dimension(nlat,nmos,icc)  :: rmlveg   !<leaf maintenance resp. rate for each pft

    logical, dimension(nlat,nmos,icc) :: pftexist !<logical array indicating pfts exist (t) or not (f)
    integer, dimension(nlat,nmos,2)   :: colddays !<cold days counter for tracking days below a certain
                                                  !<temperature threshold for ndl dcd and crop pfts.
    integer, dimension(nlat,nmos)     :: icount   !<
    integer, dimension(nlat,nmos,icc) :: lfstatus !<leaf phenology status
    integer, dimension(nlat,nmos,icc) :: pandays  !<days with positive net photosynthesis (an) for use in
                                                  !<the phenology subroutine
    integer, dimension(nlat,nmos)     :: stdaln   !<an integer telling if ctem is operated within gcm (=0) or in stand
                                                  !<alone mode (=1). this is used for fire purposes. see comments just
                                                  !<above where disturb subroutine is called.

    real, dimension(nlat,nmos) :: PREACC_M  !<
    real, dimension(nlat,nmos) :: GTACC_M   !<
    real, dimension(nlat,nmos) :: QEVPACC_M !<
    real, dimension(nlat,nmos) :: HFSACC_M  !<
    real, dimension(nlat,nmos) :: HMFNACC_M !<
    real, dimension(nlat,nmos) :: ROFACC_M  !<
    real, dimension(nlat,nmos) :: SNOACC_M  !<
    real, dimension(nlat,nmos) :: OVRACC_M  !<
    real, dimension(nlat,nmos) :: WTBLACC_M !<
    real, dimension(nlat,nmos,ignd) :: TBARACC_M !<
    real, dimension(nlat,nmos,ignd) :: THLQACC_M !<
    real, dimension(nlat,nmos,ignd) :: THICACC_M !<
    real, dimension(nlat,nmos,ignd) :: THALACC_M !<
    real, dimension(nlat,nmos) :: ALVSACC_M !<
    real, dimension(nlat,nmos) :: ALIRACC_M !<
    real, dimension(nlat,nmos) :: RHOSACC_M !<
    real, dimension(nlat,nmos) :: TSNOACC_M !<
    real, dimension(nlat,nmos) :: WSNOACC_M !<
    real, dimension(nlat,nmos) :: SNOARE_M  !<
    real, dimension(nlat,nmos) :: TCANACC_M !<
    real, dimension(nlat,nmos) :: RCANACC_M !<
    real, dimension(nlat,nmos) :: SCANACC_M !<
    real, dimension(nlat,nmos) :: ALTOTACC_M !<Daily broadband albedo
    integer, dimension(nlat) :: altotcntr_d !<Used to count the number of time steps with the sun above the horizon
    real, dimension(nlat,nmos) :: GROACC_M  !<
    real, dimension(nlat,nmos) :: FSINACC_M !<
    real, dimension(nlat,nmos) :: FLINACC_M !<
    real, dimension(nlat,nmos) :: TAACC_M   !<
    real, dimension(nlat,nmos) :: UVACC_M   !<
    real, dimension(nlat,nmos) :: PRESACC_M !<
    real, dimension(nlat,nmos) :: QAACC_M   !<
    real, dimension(nlat,nmos) :: EVAPACC_M !<
    real, dimension(nlat,nmos) :: FLUTACC_M !<

    real, dimension(nlat,nmos) :: tcanrs    !<
    real, dimension(nlat,nmos) :: tsnors    !<
    real, dimension(nlat,nmos) :: tpndrs    !<
    real, dimension(nlat,nmos,ican) :: csum        !<
    real, dimension(nlat,nmos,ignd) :: tbaraccrow_m!<
    real, dimension(nlat,nmos) :: tcanoaccrow_m    !<
    real, dimension(nlat,nmos) :: uvaccrow_m       !<
    real, dimension(nlat,nmos) :: vvaccrow_m       !<

    real, dimension(nlat,nmos) :: tcanoaccrow_out  !<
    real, dimension(nlat,nmos) :: qevpacc_m_save   !<

    real, dimension(nlat,nmos) :: twarmm    !< temperature of the warmest month (c)
    real, dimension(nlat,nmos) :: tcoldm    !< temperature of the coldest month (c)
    real, dimension(nlat,nmos) :: gdd5      !< growing degree days above 5 c
    real, dimension(nlat,nmos) :: aridity   !< aridity index, ratio of potential evaporation to precipitation
    real, dimension(nlat,nmos) :: srplsmon  !< number of months in a year with surplus water i.e. precipitation more than potential evaporation
    real, dimension(nlat,nmos) :: defctmon  !< number of months in a year with water deficit i.e. precipitation less than potential evaporation
    real, dimension(nlat,nmos) :: anndefct  !< annual water deficit (mm)
    real, dimension(nlat,nmos) :: annsrpls  !< annual water surplus (mm)
    real, dimension(nlat,nmos) :: annpcp    !< annual precipitation (mm)
    real, dimension(nlat,nmos) :: dry_season_length!< length of dry season (months)

end type veg_rot

type (veg_rot), save, target :: vrot

!=================================================================================
!>CTEM's 'gat' vars
type veg_gat

    ! This is the basic data structure that contains the state variables
    ! for the Plant Functional Type (PFT). The dimensions are ilg,{icc,iccp1}

    real, dimension(ilg,icc) :: ailcmin    !<
    real, dimension(ilg,icc) :: ailcmax    !<
    real, dimension(ilg,icc) :: dvdfcan    !<
    real, dimension(ilg,icc) :: gleafmas   !<green leaf mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
    real, dimension(ilg,icc) :: bleafmas   !<brown leaf mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
    real, dimension(ilg,icc) :: stemmass   !<stem mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
    real, dimension(ilg,icc) :: rootmass   !<root mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
    real, dimension(ilg,icc) :: pstemmass  !<stem mass from previous timestep, is value before fire. used by burntobare subroutine
    real, dimension(ilg,icc) :: pgleafmass !<root mass from previous timestep, is value before fire. used by burntobare subroutine
    real, dimension(ilg,icc) :: fcancmx    !<max. fractional coverage of ctem's 9 pfts, but this can be
                                           !<modified by land-use change, and competition between pfts
    real, dimension(ilg) :: gavglai        !<grid averaged green leaf area index

    real, dimension(ilg) :: lightng        !<total lightning frequency, flashes/km2.year
    real, dimension(ilg) :: tcanoaccgat_out!<

    real, dimension(ilg,ican) :: zolnc     !<lumped log of roughness length for class' 4 pfts
    real, dimension(ilg,ican) :: ailc      !<lumped lai for class' 4 pfts

    real, dimension(ilg,icc) :: ailcg      !<green lai for ctem's 9 pfts
    real, dimension(ilg,icc) :: ailcgs     !<GREEN LAI FOR CANOPY OVER SNOW SUB-AREA
    real, dimension(ilg,icc) :: fcancs     !<FRACTION OF CANOPY OVER SNOW FOR CTEM's 9 PFTs
    real, dimension(ilg,icc) :: fcanc      !<FRACTIONAL COVERAGE OF 8 CARBON PFTs, CANOPY OVER GROUND

    real, dimension(ilg)     :: co2conc    !<ATMOS. CO2 CONC. IN PPM
    real, dimension(ilg)     :: ch4conc    !<

    real, dimension(ilg,icc) :: co2i1cg    !<INTERCELLULAR CO2 CONC FOR 8 PFTs FOR CANOPY OVER GROUND SUBAREA (Pa) - FOR SINGLE/SUNLIT LEAF
    real, dimension(ilg,icc) :: co2i1cs    !<SAME AS ABOVE BUT FOR SHADED LEAF (above being co2i1cg)
    real, dimension(ilg,icc) :: co2i2cg    !<INTERCELLULAR CO2 CONC FOR 8 PFTs FOR CANOPY OVER SNOWSUBAREA (Pa) - FOR SINGLE/SUNLIT LEAF
    real, dimension(ilg,icc) :: co2i2cs    !<SAME AS ABOVE BUT FOR SHADED LEAF (above being co2i2cg)
    real, dimension(ilg,icc) :: ancsveg    !<net photosynthetic rate for ctems 9 pfts for canopy over snow subarea
    real, dimension(ilg,icc) :: ancgveg    !<net photosynthetic rate for ctems 9 pfts for canopy over ground subarea
    real, dimension(ilg,icc) :: rmlcsveg   !<leaf respiration rate for ctems 9 pfts forcanopy over snow subarea
    real, dimension(ilg,icc) :: rmlcgveg   !<leaf respiration rate for ctems 9 pfts forcanopy over ground subarea
    real, dimension(ilg,icc) :: slai       !<storage/imaginary lai for phenology purposes
    real, dimension(ilg,icc) :: ailcb      !<brown lai for ctem's 9 pfts. for now we assume only grasses can have brown lai
    real, dimension(ilg)     :: canres     !<
    real, dimension(ilg,icc) :: flhrloss   !<fall or harvest loss for deciduous trees and crops, respectively, \f$kg c/m^2\f$il1

    real, dimension(ilg,icc) :: grwtheff   !<growth efficiency. change in biomass per year per unit max.
                                           !<lai (\f$kg c/m^2\f$)/(m2/m2), for use in mortality subroutine
    real, dimension(ilg,icc) :: lystmmas   !<stem mass at the end of last year
    real, dimension(ilg,icc) :: lyrotmas   !<root mass at the end of last year
    real, dimension(ilg,icc) :: tymaxlai   !<this year's maximum lai
    real, dimension(ilg)     :: vgbiomas   !<grid averaged vegetation biomass, \f$kg c/m^2\f$
    real, dimension(ilg)     :: gavgltms   !<grid averaged litter mass, \f$kg c/m^2\f$
    real, dimension(ilg)     :: gavgscms   !<grid averaged soil c mass, \f$kg c/m^2\f$
    real, dimension(ilg,icc) :: stmhrlos   !<stem harvest loss for crops, \f$kg c/m^2\f$
    real, dimension(ilg,ican,ignd) :: rmatc!<fraction of roots for each of class' 4 pfts in each soil layer
    real, dimension(ilg,icc,ignd) :: rmatctem!<fraction of roots for each of ctem's 9 pfts in each soil layer
    real, dimension(ilg,iccp1) :: litrmass   !<litter mass for each of the 9 ctem pfts + bare, \f$kg c/m^2\f$
    real, dimension(ilg,iccp1) :: soilcmas   !<soil carbon mass for each of the 9 ctem pfts + bare, \f$kg c/m^2\f$
    real, dimension(ilg,icc) :: vgbiomas_veg !<vegetation biomass for each pft

    real, dimension(ilg,icc) :: emit_co2   !<carbon dioxide
    real, dimension(ilg,icc) :: emit_co    !<carbon monoxide
    real, dimension(ilg,icc) :: emit_ch4   !<methane
    real, dimension(ilg,icc) :: emit_nmhc  !<non-methane hydrocarbons
    real, dimension(ilg,icc) :: emit_h2    !<hydrogen gas
    real, dimension(ilg,icc) :: emit_nox   !<nitrogen oxides
    real, dimension(ilg,icc) :: emit_n2o   !<nitrous oxide
    real, dimension(ilg,icc) :: emit_pm25  !<particulate matter less than 2.5 um in diameter
    real, dimension(ilg,icc) :: emit_tpm   !<total particulate matter
    real, dimension(ilg,icc) :: emit_tc    !<total carbon
    real, dimension(ilg,icc) :: emit_oc    !<organic carbon
    real, dimension(ilg,icc) :: emit_bc    !<black carbon
    real, dimension(ilg)     :: burnfrac   !<areal fraction burned due to fire for every grid cell (%)
    real, dimension(ilg,icc) :: burnvegf   !<per PFT fraction burned of that PFT's area
    real, dimension(ilg,icc) :: smfuncveg  !<
    real, dimension(ilg)     :: popdin     !<population density (people / \f$km^2\f$)
    real, dimension(ilg,icc) :: bterm      !<biomass term for fire probabilty calc
    real, dimension(ilg)     :: lterm      !<lightning term for fire probabilty calc
    real, dimension(ilg,icc) :: mterm      !<moisture term for fire probabilty calc

    real, dimension(ilg)     :: extnprob   !<fire extingusinging probability
    real, dimension(ilg)     :: prbfrhuc   !<probability of fire due to human causes
    real, dimension(ilg,12)  :: mlightng   !<
    real, dimension(ilg)     :: dayl_max   !< maximum daylength for that location (hours)
    real, dimension(ilg)     :: dayl       !< daylength for that location (hours)

    real, dimension(ilg,icc) :: bmasveg    !<total (gleaf + stem + root) biomass for each ctem pft, \f$kg c/m^2\f$
    real, dimension(ilg,ican) :: cmasvegc  !<total canopy mass for each of the 4 class pfts. recall that
                                           !<class requires canopy mass as an input, and this is now provided by ctem. \f$kg/m^2\f$.
    real, dimension(ilg,icc) :: veghght    !<vegetation height (meters)
    real, dimension(ilg,icc) :: rootdpth   !<99% soil rooting depth (meters)
                                           !<both veghght & rootdpth can be used as diagnostics to see
                                           !<how vegetation grows above and below ground, respectively
    real, dimension(ilg)     :: rml        !<leaf maintenance respiration (u-mol co2/m2.sec)
    real, dimension(ilg)     :: rms        !<stem maintenance respiration (u-mol co2/m2.sec)
    real, dimension(ilg,icc) :: tltrleaf   !<total leaf litter fall rate (u-mol co2/m2.sec)
    real, dimension(ilg,icc) :: tltrstem   !<total stem litter fall rate (u-mol co2/m2.sec)
    real, dimension(ilg,icc) :: tltrroot   !<total root litter fall rate (u-mol co2/m2.sec)
    real, dimension(ilg,icc) :: leaflitr   !<leaf litter fall rate (u-mol co2/m2.sec). this leaf litter 
                                           !<does not include litter generated due to mortality/fire
    real, dimension(ilg,icc) :: roottemp   !<root temperature, k
    real, dimension(ilg,icc) :: afrleaf    !<allocation fraction for leaves
    real, dimension(ilg,icc) :: afrstem    !<allocation fraction for stem
    real, dimension(ilg,icc) :: afrroot    !<allocation fraction for root
    real, dimension(ilg,icc) :: wtstatus   !<soil water status used for calculating allocation fractions
    real, dimension(ilg,icc) :: ltstatus   !<light status used for calculating allocation fractions
    real, dimension(ilg)     :: rmr        !<root maintenance respiration (u-mol co2/m2.sec)

    real, dimension(ilg,8)  :: slopefrac   !<prescribed fraction of wetlands based on slope
                                           !<only(0.025, 0.05, 0.1, 0.15, 0.20, 0.25, 0.3 and 0.35 percent slope thresholds)
    real, dimension(ilg)    :: wetfrac_pres!<
    real, dimension(ilg,12) :: wetfrac_mon !<
    real, dimension(ilg)    :: ch4wet1    !<methane flux from wetlands calculated using hetrores in umol ch4/m2.s
    real, dimension(ilg)    :: ch4wet2    !<methane flux from wetlands calculated using npp in umol ch4/m2.s
    real, dimension(ilg)    :: wetfdyn    !<dynamic wetland fraction
    real, dimension(ilg)    :: ch4dyn1    !<methane flux from wetlands calculated using hetrores 
                                          !<and wetfdyn, in umol ch4/m2.s
    real, dimension(ilg)    :: ch4dyn2    !<methane flux from wetlands calculated using npp and wetfdyn, 
                                          !<in umol ch4/m2.s
    real, dimension(ilg)    :: ch4_soills !<Methane uptake into the soil column ($mg CH_4 m^{-2} s^{-1}$)

    real, dimension(ilg) :: lucemcom   !<land use change (luc) related combustion emission losses, u-mol co2/m2.sec
    real, dimension(ilg) :: lucltrin   !<luc related inputs to litter pool, u-mol co2/m2.sec
    real, dimension(ilg) :: lucsocin   !<luc related inputs to soil c pool, u-mol co2/m2.sec

    real, dimension(ilg) :: npp        !<net primary productivity
    real, dimension(ilg) :: nep        !<net ecosystem productivity
    real, dimension(ilg) :: nbp        !<net biome productivity
    real, dimension(ilg) :: gpp        !<gross primary productivity
    real, dimension(ilg) :: hetrores   !<heterotrophic respiration
    real, dimension(ilg) :: autores    !<autotrophic respiration
    real, dimension(ilg) :: soilcresp  !<
    real, dimension(ilg) :: rm         !<maintenance respiration
    real, dimension(ilg) :: rg         !<growth respiration
    real, dimension(ilg) :: litres     !<litter respiration
    real, dimension(ilg) :: socres     !<soil carbon respiration
    real, dimension(ilg) :: dstcemls   !<carbon emission losses due to disturbance, mainly fire
    real, dimension(ilg) :: litrfall   !<total litter fall (from leaves, stem, and root) due to 
                                       !<all causes (mortality, turnover, and disturbance)
    real, dimension(ilg) :: humiftrs   !<transfer of humidified litter from litter to soil c pool

    real, dimension(ilg,icc)   :: gppveg     !<gross primary productity for each pft
    real, dimension(ilg,iccp1) :: nepveg     !<net ecosystem productity for bare fraction expnbaln(i)=0.0 amount
                                             !<of c related to spatial expansion Not used JM Jun 2014 
                                             !<OR net ecosystem productity for each pft
    real, dimension(ilg,iccp1) :: nbpveg     !<net biome productity for bare fraction OR net biome productity for each pft
    real, dimension(ilg,icc)   :: nppveg     !<npp for individual pfts,  u-mol co2/m2.sec
    real, dimension(ilg,iccp1) :: hetroresveg!<
    real, dimension(ilg,icc)   :: autoresveg !<
    real, dimension(ilg,iccp1) :: litresveg  !<
    real, dimension(ilg,iccp1) :: soilcresveg!<
    real, dimension(ilg,icc)   :: rmlvegacc  !<
    real, dimension(ilg,icc)   :: rmsveg     !<stem maintenance resp. rate for each pft
    real, dimension(ilg,icc)   :: rmrveg     !<root maintenance resp. rate for each pft
    real, dimension(ilg,icc)   :: rgveg      !<growth resp. rate for each pft
    real, dimension(ilg,icc)   :: litrfallveg!<litter fall in \f$kg c/m^2\f$ for each pft
    real, dimension(ilg,iccp1) :: humiftrsveg!<

    real, dimension(ilg,icc)  :: rothrlos !<root death as crops are harvested, \f$kg c/m^2\f$
    real, dimension(ilg,icc)  :: pfcancmx !<previous year's fractional coverages of pfts
    real, dimension(ilg,icc)  :: nfcancmx !<next year's fractional coverages of pfts
    real, dimension(ilg,ican) :: alvsctm  !<
    real, dimension(ilg,ican) :: paic     !<plant area index for class' 4 pfts. this is the sum of leaf
                                          !<area index and stem area index.
    real, dimension(ilg,ican) :: slaic    !<storage lai. this will be used as min. lai that class sees
                                          !<so that it doesn't blow up in its stomatal conductance calculations.
    real, dimension(ilg,ican) :: alirctm  !<
    real, dimension(ilg)      :: cfluxcg  !<
    real, dimension(ilg)      :: cfluxcs  !<
    real, dimension(ilg)      :: dstcemls3!<carbon emission losses due to disturbance (fire at present) from litter pool
    real, dimension(ilg,icc)  :: anveg    !<net photosynthesis rate for each pft
    real, dimension(ilg,icc)  :: rmlveg   !<leaf maintenance resp. rate for each pft

    real, dimension(ilg) :: twarmm            !< temperature of the warmest month (c)
    real, dimension(ilg) :: tcoldm            !< temperature of the coldest month (c)
    real, dimension(ilg) :: gdd5              !< growing degree days above 5 c
    real, dimension(ilg) :: aridity           !< aridity index, ratio of potential evaporation to precipitation
    real, dimension(ilg) :: srplsmon          !< number of months in a year with surplus water i.e. precipitation more than potential evaporation
    real, dimension(ilg) :: defctmon          !< number of months in a year with water deficit i.e. precipitation less than potential evaporation
    real, dimension(ilg) :: anndefct          !< annual water deficit (mm)
    real, dimension(ilg) :: annsrpls          !< annual water surplus (mm)
    real, dimension(ilg) :: annpcp            !< annual precipitation (mm)
    real, dimension(ilg) :: dry_season_length !< length of dry season (months)

    ! These go into CTEM and are used to keep track of the bioclim limits.
    real, dimension(ilg) :: tcurm     !<temperature of the current month (c)
    real, dimension(ilg) :: srpcuryr  !<water surplus for the current year
    real, dimension(ilg) :: dftcuryr  !<water deficit for the current year
    real, dimension(12,ilg) :: tmonth !<monthly temperatures
    real, dimension(ilg) :: anpcpcur  !<annual precipitation for current year (mm)
    real, dimension(ilg) :: anpecur   !<annual potential evaporation for current year (mm)
    real, dimension(ilg) :: gdd5cur   !<growing degree days above 5 c for current year
    real, dimension(ilg) :: surmncur  !<number of months with surplus water for current year
    real, dimension(ilg) :: defmncur  !<number of months with water deficit for current year
    real, dimension(ilg) :: srplscur  !<water surplus for the current month
    real, dimension(ilg) :: defctcur  !<water deficit for the current month

    real, dimension(ilg,icc) :: geremort !<growth efficiency related mortality (1/day)
    real, dimension(ilg,icc) :: intrmort !<intrinsic (age related) mortality (1/day)
    real, dimension(ilg,icc) :: lambda   !<Used to determine the colonization rate
    real, dimension(ilg,icc) :: cc       !<colonization rate & mortality rate
    real, dimension(ilg,icc) :: mm       !<colonization rate & mortality rate

    logical, dimension(ilg,icc) :: pftexist !<logical array indicating pfts exist (t) or not (f)
    integer, dimension(ilg,2)   :: colddays !<cold days counter for tracking days below a certain
                                            !<temperature threshold for ndl dcd and crop pfts.
    integer, dimension(ilg)     :: icount   !<
    integer, dimension(ilg,icc) :: lfstatus !<leaf phenology status
    integer, dimension(ilg,icc) :: pandays  !<days with positive net photosynthesis (an) for use in
                                            !<the phenology subroutine
    integer, dimension(ilg)     :: stdaln   !<an integer telling if ctem is operated within gcm (=0) or in stand
                                            !<alone mode (=1). this is used for fire purposes. see comments just
                                            !<above where disturb subroutine is called.

end type veg_gat

type (veg_gat), save, target :: vgat
!=================================================================================
!>CLASS's monthly outputs
type class_moyr_output

!   MONTHLY OUTPUT FOR CLASS GRID-MEAN

    real, dimension(nlat) :: ALVSACC_MO   !<
    real, dimension(nlat) :: ALIRACC_MO   !<
    real, dimension(nlat) :: FLUTACC_MO   !<
    real, dimension(nlat) :: FSINACC_MO   !<
    real, dimension(nlat) :: FLINACC_MO   !<
    real, dimension(nlat) :: HFSACC_MO    !<
    real, dimension(nlat) :: QEVPACC_MO   !<
    real, dimension(nlat) :: SNOACC_MO    !<
    real, dimension(nlat) :: WSNOACC_MO   !<
    real, dimension(nlat) :: ROFACC_MO    !<
    real, dimension(nlat) :: PREACC_MO    !<
    real, dimension(nlat) :: EVAPACC_MO   !<
    real, dimension(nlat) :: TRANSPACC_MO !<
    real, dimension(nlat) :: TAACC_MO     !<
    real, dimension(nlat) :: ALTOTACC_MO  !< Broadband albedo
    real, dimension(nlat) :: GROUNDEVAP   !< evaporation and sublimation from the ground surface (formed from QFG and QFN), kg /m/mon
    real, dimension(nlat) :: CANOPYEVAP   !< evaporation and sublimation from the canopy (formed from QFCL and QFCF), kg /m/mon
    integer, dimension(nlat) :: altotcntr_m!< Used to count the number of time steps with the sun above the horizon

    real :: FSSTAR_MO !<
    real :: FLSTAR_MO !<
    real :: QH_MO     !<
    real :: QE_MO     !<

    real, dimension(nlat,ignd) :: TBARACC_MO !<
    real, dimension(nlat,ignd) :: THLQACC_MO !<
    real, dimension(nlat,ignd) :: THICACC_MO !<

!   YEARLY OUTPUT FOR CLASS GRID-MEAN

    real, dimension(nlat) :: ALVSACC_YR  !<
    real, dimension(nlat) :: ALIRACC_YR  !<
    real, dimension(nlat) :: FLUTACC_YR  !<
    real, dimension(nlat) :: FSINACC_YR  !<
    real, dimension(nlat) :: FLINACC_YR  !<
    real, dimension(nlat) :: HFSACC_YR   !<
    real, dimension(nlat) :: QEVPACC_YR  !<
    real, dimension(nlat) :: ROFACC_YR   !<
    real, dimension(nlat) :: PREACC_YR   !<
    real, dimension(nlat) :: EVAPACC_YR  !<
    real, dimension(nlat) :: TRANSPACC_YR!<
    real, dimension(nlat) :: TAACC_YR    !<
    real, dimension(nlat) :: ALTOTACC_YR !< Broadband albedo
    integer, dimension(nlat) :: altotcntr_yr !<Used to count the number of time steps with the sun above the horizon

    real :: FSSTAR_YR !<
    real :: FLSTAR_YR !<
    real :: QH_YR     !<
    real :: QE_YR     !<

end type class_moyr_output

type (class_moyr_output), save, target :: class_out

!=================================================================================
!>CTEM's grid average variables
type ctem_gridavg

! Grid-averaged variables (denoted with an ending of "_g")

      real, dimension(nlat) :: WSNOROT_g !<
      real, dimension(nlat) :: ROFSROT_g !<
      real, dimension(nlat) :: SNOROT_g  !<
      real, dimension(nlat) :: RHOSROT_g !<
      real, dimension(nlat) :: ROFROT_g  !<
      real, dimension(nlat) :: ZPNDROT_g !<
      real, dimension(nlat) :: RCANROT_g !<
      real, dimension(nlat) :: SCANROT_g !<
      real, dimension(nlat) :: TROFROT_g !<
      real, dimension(nlat) :: TROOROT_g !<
      real, dimension(nlat) :: TROBROT_g !<
      real, dimension(nlat) :: ROFOROT_g !<
      real, dimension(nlat) :: ROFBROT_g !<
      real, dimension(nlat) :: TROSROT_g !<
      real, dimension(nlat) :: FSGVROT_g !<
      real, dimension(nlat) :: FSGSROT_g !<
      real, dimension(nlat) :: FLGVROT_g !<
      real, dimension(nlat) :: FLGSROT_g !<
      real, dimension(nlat) :: HFSCROT_g !<
      real, dimension(nlat) :: HFSSROT_g !<
      real, dimension(nlat) :: HEVCROT_g !<
      real, dimension(nlat) :: HEVSROT_g !<
      real, dimension(nlat) :: HMFCROT_g !<
      real, dimension(nlat) :: HMFNROT_g !<
      real, dimension(nlat) :: HTCSROT_g !<
      real, dimension(nlat) :: HTCCROT_g !<
      real, dimension(nlat) :: FSGGROT_g !<
      real, dimension(nlat) :: FLGGROT_g !<
      real, dimension(nlat) :: HFSGROT_g !<
      real, dimension(nlat) :: HEVGROT_g !<
      real, dimension(nlat) :: CDHROT_g  !<
      real, dimension(nlat) :: CDMROT_g  !<
      real, dimension(nlat) :: SFCUROT_g !<
      real, dimension(nlat) :: SFCVROT_g !<
      real, dimension(nlat) :: fc_g      !<
      real, dimension(nlat) :: fg_g      !<
      real, dimension(nlat) :: fcs_g     !<
      real, dimension(nlat) :: fgs_g     !<
      real, dimension(nlat) :: PCFCROT_g !<
      real, dimension(nlat) :: PCLCROT_g !<
      real, dimension(nlat) :: PCPGROT_g !<
      real, dimension(nlat) :: QFCFROT_g !<
      real, dimension(nlat) :: QFGROT_g  !<
      real, dimension(nlat,ignd) :: QFCROT_g !<
      real, dimension(nlat) :: ROFCROT_g  !<
      real, dimension(nlat) :: ROFNROT_g  !<
      real, dimension(nlat) :: WTRSROT_g  !<
      real, dimension(nlat) :: WTRGROT_g  !<
      real, dimension(nlat) :: PCPNROT_g  !<
      real, dimension(nlat) :: QFCLROT_g  !<
      real, dimension(nlat) :: QFNROT_g   !<
      real, dimension(nlat) :: WTRCROT_g  !<
      real, dimension(nlat) :: gpp_g      !<
      real, dimension(nlat) :: npp_g      !<
      real, dimension(nlat) :: nbp_g      !<
      real, dimension(nlat) :: socres_g   !<
      real, dimension(nlat) :: autores_g  !<
      real, dimension(nlat) :: litres_g   !<
      real, dimension(nlat) :: dstcemls3_g!<
      real, dimension(nlat) :: litrfall_g !<
      real, dimension(nlat) :: rml_g      !<
      real, dimension(nlat) :: rms_g      !<
      real, dimension(nlat) :: rg_g       !<
      real, dimension(nlat) :: leaflitr_g !<
      real, dimension(nlat) :: tltrstem_g !<
      real, dimension(nlat) :: tltrroot_g !<
      real, dimension(nlat) :: nep_g      !<
      real, dimension(nlat) :: hetrores_g !<
      real, dimension(nlat) :: dstcemls_g !<
      real, dimension(nlat) :: humiftrs_g !<
      real, dimension(nlat) :: rmr_g      !<
      real, dimension(nlat) :: tltrleaf_g !<
      real, dimension(nlat) :: gavgltms_g !<

      real, dimension(nlat) :: vgbiomas_g !<
      real, dimension(nlat) :: gavglai_g  !<
      real, dimension(nlat) :: gavgscms_g !<
      real, dimension(nlat) :: gleafmas_g !<
      real, dimension(nlat) :: bleafmas_g !<
      real, dimension(nlat) :: stemmass_g !<
      real, dimension(nlat) :: rootmass_g !<
      real, dimension(nlat) :: litrmass_g !<
      real, dimension(nlat) :: soilcmas_g !<
      real, dimension(nlat) :: slai_g     !<
      real, dimension(nlat) :: ailcg_g    !<
      real, dimension(nlat) :: ailcb_g    !<
      real, dimension(nlat) :: veghght_g  !<
      real, dimension(nlat) :: rootdpth_g !<
      real, dimension(nlat) :: roottemp_g !<
      real, dimension(nlat) :: totcmass_g !<
      real, dimension(nlat) :: tcanoacc_out_g!<
      real, dimension(nlat) :: burnfrac_g !<
      real, dimension(nlat) :: smfuncveg_g!<
      real, dimension(nlat) :: lucemcom_g !<
      real, dimension(nlat) :: lucltrin_g !<
      real, dimension(nlat) :: lucsocin_g !<
      real, dimension(nlat) :: emit_co2_g !<
      real, dimension(nlat) :: emit_co_g  !<
      real, dimension(nlat) :: emit_ch4_g !<
      real, dimension(nlat) :: emit_nmhc_g!<
      real, dimension(nlat) :: emit_h2_g  !<
      real, dimension(nlat) :: emit_nox_g !<
      real, dimension(nlat) :: emit_n2o_g !<
      real, dimension(nlat) :: emit_pm25_g!<
      real, dimension(nlat) :: emit_tpm_g !<
      real, dimension(nlat) :: emit_tc_g  !<
      real, dimension(nlat) :: emit_oc_g  !<
      real, dimension(nlat) :: emit_bc_g  !<
      real, dimension(nlat) :: bterm_g    !<
      real, dimension(nlat) :: lterm_g    !<
      real, dimension(nlat) :: mterm_g    !<
      real, dimension(nlat) :: ch4wet1_g  !<
      real, dimension(nlat) :: ch4wet2_g  !<
      real, dimension(nlat) :: wetfdyn_g  !<
      real, dimension(nlat) :: ch4dyn1_g  !<
      real, dimension(nlat) :: ch4dyn2_g  !<
      real, dimension(nlat) :: ch4_soills_g   !<
      real, dimension(nlat,icc) :: afrleaf_g  !<
      real, dimension(nlat,icc) :: afrstem_g  !<
      real, dimension(nlat,icc) :: afrroot_g  !<
      real, dimension(nlat,icc) :: lfstatus_g !<
      real, dimension(nlat,icc) :: rmlvegrow_g!<
      real, dimension(nlat,icc) :: anvegrow_g !<
      real, dimension(nlat,ignd) :: rmatctem_g!<
      real, dimension(nlat,ignd) :: HMFGROT_g !<
      real, dimension(nlat,ignd) :: HTCROT_g  !<
      real, dimension(nlat,ignd) :: TBARROT_g !<
      real, dimension(nlat,ignd) :: THLQROT_g !<
      real, dimension(nlat,ignd) :: THICROT_g !<
      real, dimension(nlat,ignd) :: GFLXROT_g !<

      real, dimension(nlat) :: fsstar_g !<
      real, dimension(nlat) :: flstar_g !<
      real, dimension(nlat) :: qh_g     !<
      real, dimension(nlat) :: qe_g     !<
      real, dimension(nlat) :: snomlt_g !<
      real, dimension(nlat) :: beg_g    !<
      real, dimension(nlat) :: gtout_g  !<
      real, dimension(nlat) :: tpn_g    !<
      real, dimension(nlat) :: altot_g  !<
      real, dimension(nlat) :: tcn_g    !<
      real, dimension(nlat) :: tsn_g    !<
      real, dimension(nlat) :: zsn_g    !<

end type ctem_gridavg

type (ctem_gridavg), save, target :: ctem_grd

!=================================================================================
!>CTEM's variables per tile
type ctem_tile_level

!   Tile-level variables (denoted by an ending of "_t")

      real, dimension(nlat,nmos) :: leaflitr_t !<
      real, dimension(nlat,nmos) :: tltrleaf_t !<
      real, dimension(nlat,nmos) :: tltrstem_t !<
      real, dimension(nlat,nmos) :: tltrroot_t !<
      real, dimension(nlat,nmos) :: ailcg_t    !<
      real, dimension(nlat,nmos) :: ailcb_t    !<
      real, dimension(nlat,nmos,ignd) :: rmatctem_t !<
      real, dimension(nlat,nmos) :: veghght_t  !<
      real, dimension(nlat,nmos) :: rootdpth_t !<
      real, dimension(nlat,nmos) :: roottemp_t !<
      real, dimension(nlat,nmos) :: slai_t     !<
      real, dimension(nlat,nmos) :: afrroot_t  !<
      real, dimension(nlat,nmos) :: afrleaf_t  !<
      real, dimension(nlat,nmos) :: afrstem_t  !<
      real, dimension(nlat,nmos) :: laimaxg_t  !<
      real, dimension(nlat,nmos) :: stemmass_t !<
      real, dimension(nlat,nmos) :: rootmass_t !<
      real, dimension(nlat,nmos) :: litrmass_t !<
      real, dimension(nlat,nmos) :: gleafmas_t !<
      real, dimension(nlat,nmos) :: bleafmas_t !<
      real, dimension(nlat,nmos) :: soilcmas_t !<
      real, dimension(nlat,nmos) :: emit_co2_t !<
      real, dimension(nlat,nmos) :: emit_co_t  !<
      real, dimension(nlat,nmos) :: emit_ch4_t !<
      real, dimension(nlat,nmos) :: emit_nmhc_t!<
      real, dimension(nlat,nmos) :: emit_h2_t  !<
      real, dimension(nlat,nmos) :: emit_nox_t !<
      real, dimension(nlat,nmos) :: emit_n2o_t !<
      real, dimension(nlat,nmos) :: emit_pm25_t!<
      real, dimension(nlat,nmos) :: emit_tpm_t !<
      real, dimension(nlat,nmos) :: emit_tc_t  !<
      real, dimension(nlat,nmos) :: emit_oc_t  !<
      real, dimension(nlat,nmos) :: emit_bc_t  !<
      real, dimension(nlat,nmos) :: bterm_t    !<
      real, dimension(nlat,nmos) :: mterm_t    !<
      real, dimension(nlat,nmos) :: smfuncveg_t!<

      real, dimension(ilg) :: fsnowacc_t       !<
      real, dimension(ilg) :: tcansacc_t       !<
      real, dimension(ilg) :: tcanoaccgat_t    !<
      real, dimension(ilg) :: taaccgat_t       !<
      real, dimension(ilg) :: uvaccgat_t       !<
      real, dimension(ilg) :: vvaccgat_t       !<
      real, dimension(ilg,ignd) :: tbaraccgat_t!<
      real, dimension(ilg,ignd) :: tbarcacc_t  !<
      real, dimension(ilg,ignd) :: tbarcsacc_t !<
      real, dimension(ilg,ignd) :: tbargacc_t  !<
      real, dimension(ilg,ignd) :: tbargsacc_t !<
      real, dimension(ilg,ignd) :: thliqcacc_t !<
      real, dimension(ilg,ignd) :: thliqgacc_t !<
      real, dimension(ilg,ignd) :: thliqacc_t  !<
      real, dimension(ilg,ignd) :: thicecacc_t !<
      real, dimension(ilg,ignd) :: thicegacc_t !<
      real, dimension(ilg,icc)  :: ancsvgac_t  !<
      real, dimension(ilg,icc)  :: ancgvgac_t  !<
      real, dimension(ilg,icc)  :: rmlcsvga_t  !<
      real, dimension(ilg,icc)  :: rmlcgvga_t  !<

end type ctem_tile_level

type (ctem_tile_level), save, target :: ctem_tile

!=================================================================================
!>CTEM's variables monthly averaged (per pft)
type ctem_monthly

!     Tile-level monthly variables (denoted by name ending in "_mo_t")

      real, dimension(nlat,nmos,icc)   :: laimaxg_mo    !<
      real, dimension(nlat,nmos,icc)   :: stemmass_mo   !<
      real, dimension(nlat,nmos,icc)   :: rootmass_mo   !<
      real, dimension(nlat,nmos,icc)   :: litrfallveg_mo!<
      real, dimension(nlat,nmos,iccp1) :: humiftrsveg_mo!<
      real, dimension(nlat,nmos,icc)   :: npp_mo        !<
      real, dimension(nlat,nmos,icc)   :: gpp_mo        !<
      real, dimension(nlat,nmos,icc)   :: vgbiomas_mo   !<
      real, dimension(nlat,nmos,icc)   :: autores_mo    !<
      real, dimension(nlat,nmos,iccp1) :: totcmass_mo   !<
      real, dimension(nlat,nmos,iccp1) :: litrmass_mo   !<
      real, dimension(nlat,nmos,iccp1) :: soilcmas_mo   !<
      real, dimension(nlat,nmos,iccp1) :: nep_mo        !<
      real, dimension(nlat,nmos,iccp1) :: litres_mo     !<
      real, dimension(nlat,nmos,iccp1) :: soilcres_mo   !<
      real, dimension(nlat,nmos,iccp1) :: hetrores_mo   !<
      real, dimension(nlat,nmos,iccp1) :: nbp_mo        !<
      real, dimension(nlat,nmos,icc) :: emit_co2_mo  !<
      real, dimension(nlat,nmos,icc) :: emit_co_mo   !<
      real, dimension(nlat,nmos,icc) :: emit_ch4_mo  !<
      real, dimension(nlat,nmos,icc) :: emit_nmhc_mo !<
      real, dimension(nlat,nmos,icc) :: emit_h2_mo   !<
      real, dimension(nlat,nmos,icc) :: emit_nox_mo  !<
      real, dimension(nlat,nmos,icc) :: emit_n2o_mo  !<
      real, dimension(nlat,nmos,icc) :: emit_pm25_mo !<
      real, dimension(nlat,nmos,icc) :: emit_tpm_mo  !<
      real, dimension(nlat,nmos,icc) :: emit_tc_mo   !<
      real, dimension(nlat,nmos,icc) :: emit_oc_mo   !<
      real, dimension(nlat,nmos,icc) :: emit_bc_mo   !<
      real, dimension(nlat,nmos,icc) :: burnfrac_mo  !<
      real, dimension(nlat,nmos,icc) :: bterm_mo     !<
      real, dimension(nlat,nmos,icc) :: mterm_mo     !<
      real, dimension(nlat,nmos,icc) :: smfuncveg_mo !<

end type ctem_monthly

type (ctem_monthly), save, target :: ctem_mo

!=================================================================================
!>CTEM's grid average monthly values
type ctem_gridavg_monthly

!  Grid averaged monthly variables (denoted by name ending in "_mo_g")

    real, dimension(nlat) :: laimaxg_mo_g  !<
    real, dimension(nlat) :: stemmass_mo_g !<
    real, dimension(nlat) :: rootmass_mo_g !<
    real, dimension(nlat) :: litrmass_mo_g !<
    real, dimension(nlat) :: soilcmas_mo_g !<
    real, dimension(nlat) :: litrfall_mo_g !<
    real, dimension(nlat) :: humiftrs_mo_g !<
    real, dimension(nlat) :: npp_mo_g      !<
    real, dimension(nlat) :: gpp_mo_g      !<
    real, dimension(nlat) :: nep_mo_g      !<
    real, dimension(nlat) :: nbp_mo_g      !<
    real, dimension(nlat) :: hetrores_mo_g !<
    real, dimension(nlat) :: autores_mo_g  !<
    real, dimension(nlat) :: litres_mo_g   !<
    real, dimension(nlat) :: soilcres_mo_g !<
    real, dimension(nlat) :: vgbiomas_mo_g !<
    real, dimension(nlat) :: totcmass_mo_g !<
    real, dimension(nlat) :: emit_co2_mo_g !<
    real, dimension(nlat) :: emit_co_mo_g  !<
    real, dimension(nlat) :: emit_ch4_mo_g !<
    real, dimension(nlat) :: emit_nmhc_mo_g!<
    real, dimension(nlat) :: emit_h2_mo_g  !<
    real, dimension(nlat) :: emit_nox_mo_g !<
    real, dimension(nlat) :: emit_n2o_mo_g !<
    real, dimension(nlat) :: emit_pm25_mo_g!<
    real, dimension(nlat) :: emit_tpm_mo_g !<
    real, dimension(nlat) :: emit_tc_mo_g  !<
    real, dimension(nlat) :: emit_oc_mo_g  !<
    real, dimension(nlat) :: emit_bc_mo_g  !<
    real, dimension(nlat) :: smfuncveg_mo_g!<
    real, dimension(nlat) :: luc_emc_mo_g  !<
    real, dimension(nlat) :: lucltrin_mo_g !<
    real, dimension(nlat) :: lucsocin_mo_g !<
    real, dimension(nlat) :: burnfrac_mo_g !<
    real, dimension(nlat) :: bterm_mo_g    !<
    real, dimension(nlat) :: lterm_mo_g    !<
    real, dimension(nlat) :: mterm_mo_g    !<
    real, dimension(nlat) :: ch4wet1_mo_g  !<
    real, dimension(nlat) :: ch4wet2_mo_g  !<
    real, dimension(nlat) :: wetfdyn_mo_g  !<
    real, dimension(nlat) :: ch4dyn1_mo_g  !<
    real, dimension(nlat) :: ch4dyn2_mo_g  !<
    real, dimension(nlat) :: ch4soills_mo_g!<

end type ctem_gridavg_monthly

type (ctem_gridavg_monthly), save, target :: ctem_grd_mo

!=================================================================================
!>CTEM's variables per tile monthly values
type ctem_tileavg_monthly

!     Tile-level monthly variables (denoted by name ending in "_mo_t")

      real, dimension(nlat,nmos) :: laimaxg_mo_t  !<
      real, dimension(nlat,nmos) :: stemmass_mo_t !<
      real, dimension(nlat,nmos) :: rootmass_mo_t !<
      real, dimension(nlat,nmos) :: litrfall_mo_t !<
      real, dimension(nlat,nmos) :: humiftrs_mo_t !<
      real, dimension(nlat,nmos) :: npp_mo_t      !<
      real, dimension(nlat,nmos) :: gpp_mo_t      !<
      real, dimension(nlat,nmos) :: vgbiomas_mo_t !<
      real, dimension(nlat,nmos) :: autores_mo_t  !<
      real, dimension(nlat,nmos) :: totcmass_mo_t !<
      real, dimension(nlat,nmos) :: litrmass_mo_t !<
      real, dimension(nlat,nmos) :: soilcmas_mo_t !<
      real, dimension(nlat,nmos) :: nep_mo_t      !<
      real, dimension(nlat,nmos) :: litres_mo_t   !<
      real, dimension(nlat,nmos) :: soilcres_mo_t !<
      real, dimension(nlat,nmos) :: hetrores_mo_t !<
      real, dimension(nlat,nmos) :: nbp_mo_t      !<
      real, dimension(nlat,nmos) :: emit_co2_mo_t !<
      real, dimension(nlat,nmos) :: emit_co_mo_t  !<
      real, dimension(nlat,nmos) :: emit_ch4_mo_t !<
      real, dimension(nlat,nmos) :: emit_nmhc_mo_t!<
      real, dimension(nlat,nmos) :: emit_h2_mo_t  !<
      real, dimension(nlat,nmos) :: emit_nox_mo_t !<
      real, dimension(nlat,nmos) :: emit_n2o_mo_t !<
      real, dimension(nlat,nmos) :: emit_pm25_mo_t!<
      real, dimension(nlat,nmos) :: emit_tpm_mo_t !<
      real, dimension(nlat,nmos) :: emit_tc_mo_t  !<
      real, dimension(nlat,nmos) :: emit_oc_mo_t  !<
      real, dimension(nlat,nmos) :: emit_bc_mo_t  !<
      real, dimension(nlat,nmos) :: burnfrac_mo_t !<
      real, dimension(nlat,nmos) :: smfuncveg_mo_t!<
      real, dimension(nlat,nmos) :: bterm_mo_t    !<
      real, dimension(nlat,nmos) :: luc_emc_mo_t  !<
      real, dimension(nlat,nmos) :: lterm_mo_t    !<
      real, dimension(nlat,nmos) :: lucsocin_mo_t !<
      real, dimension(nlat,nmos) :: mterm_mo_t    !<
      real, dimension(nlat,nmos) :: lucltrin_mo_t !<
      real, dimension(nlat,nmos) :: ch4wet1_mo_t  !<
      real, dimension(nlat,nmos) :: ch4wet2_mo_t  !<
      real, dimension(nlat,nmos) :: wetfdyn_mo_t  !<
      real, dimension(nlat,nmos) :: ch4dyn1_mo_t  !<
      real, dimension(nlat,nmos) :: ch4dyn2_mo_t  !<
      real, dimension(nlat,nmos) :: ch4soills_mo_t!<
      real, dimension(nlat,nmos) :: wind_mo_t     !<

end type ctem_tileavg_monthly

type (ctem_tileavg_monthly), save, target :: ctem_tile_mo


!=================================================================================

!>CTEM's average annual values (per PFT)
type ctem_annual

! c      Annual output for CTEM mosaic variables:
! c      (denoted by name ending in "_yr_m")
!
      real, dimension(nlat,nmos,icc) :: laimaxg_yr   !<
      real, dimension(nlat,nmos,icc) :: stemmass_yr  !<
      real, dimension(nlat,nmos,icc) :: rootmass_yr  !<
      real, dimension(nlat,nmos,icc) :: npp_yr       !<
      real, dimension(nlat,nmos,icc) :: gpp_yr       !<
      real, dimension(nlat,nmos,icc) :: vgbiomas_yr  !<
      real, dimension(nlat,nmos,icc) :: autores_yr   !<
      real, dimension(nlat,nmos,iccp1) :: totcmass_yr!<
      real, dimension(nlat,nmos,iccp1) :: litrmass_yr!<
      real, dimension(nlat,nmos,iccp1) :: soilcmas_yr!<
      real, dimension(nlat,nmos,iccp1) :: nep_yr     !<
      real, dimension(nlat,nmos,iccp1) :: litres_yr  !<
      real, dimension(nlat,nmos,iccp1) :: soilcres_yr!<
      real, dimension(nlat,nmos,iccp1) :: hetrores_yr!<
      real, dimension(nlat,nmos,iccp1) :: nbp_yr     !<
      real, dimension(nlat,nmos,icc) :: emit_co2_yr  !<
      real, dimension(nlat,nmos,icc) :: emit_co_yr   !<
      real, dimension(nlat,nmos,icc) :: emit_ch4_yr  !<
      real, dimension(nlat,nmos,icc) :: emit_nmhc_yr !<
      real, dimension(nlat,nmos,icc) :: emit_h2_yr   !<
      real, dimension(nlat,nmos,icc) :: emit_nox_yr  !<
      real, dimension(nlat,nmos,icc) :: emit_n2o_yr  !<
      real, dimension(nlat,nmos,icc) :: emit_pm25_yr !<
      real, dimension(nlat,nmos,icc) :: emit_tpm_yr  !<
      real, dimension(nlat,nmos,icc) :: emit_tc_yr   !<
      real, dimension(nlat,nmos,icc) :: emit_oc_yr   !<
      real, dimension(nlat,nmos,icc) :: emit_bc_yr   !<
      real, dimension(nlat,nmos,icc) :: bterm_yr     !<
      real, dimension(nlat,nmos,icc) :: mterm_yr     !<
      real, dimension(nlat,nmos,icc) :: burnfrac_yr  !<
      real, dimension(nlat,nmos,icc) :: smfuncveg_yr !<
      real, dimension(nlat,nmos,icc) :: veghght_yr   !<

end type ctem_annual

type (ctem_annual), save, target :: ctem_yr

!=================================================================================

!>CTEM's grid average annual values
type ctem_gridavg_annual

! Annual output for CTEM grid-averaged variables:
! (denoted by name ending in "_yr_g")

    real, dimension(nlat) :: laimaxg_yr_g  !<
    real, dimension(nlat) :: stemmass_yr_g !<
    real, dimension(nlat) :: rootmass_yr_g !<
    real, dimension(nlat) :: litrmass_yr_g !<
    real, dimension(nlat) :: soilcmas_yr_g !<
    real, dimension(nlat) :: npp_yr_g      !<
    real, dimension(nlat) :: gpp_yr_g      !<
    real, dimension(nlat) :: nep_yr_g      !<
    real, dimension(nlat) :: nbp_yr_g      !<
    real, dimension(nlat) :: hetrores_yr_g !<
    real, dimension(nlat) :: autores_yr_g  !<
    real, dimension(nlat) :: litres_yr_g   !<
    real, dimension(nlat) :: soilcres_yr_g !<
    real, dimension(nlat) :: vgbiomas_yr_g !<
    real, dimension(nlat) :: totcmass_yr_g !<
    real, dimension(nlat) :: emit_co2_yr_g !<
    real, dimension(nlat) :: emit_co_yr_g  !<
    real, dimension(nlat) :: emit_ch4_yr_g !<
    real, dimension(nlat) :: emit_nmhc_yr_g!<
    real, dimension(nlat) :: emit_h2_yr_g  !<
    real, dimension(nlat) :: emit_nox_yr_g !<
    real, dimension(nlat) :: emit_n2o_yr_g !<
    real, dimension(nlat) :: emit_pm25_yr_g!<
    real, dimension(nlat) :: emit_tpm_yr_g !<
    real, dimension(nlat) :: emit_tc_yr_g  !<
    real, dimension(nlat) :: emit_oc_yr_g  !<
    real, dimension(nlat) :: emit_bc_yr_g  !<
    real, dimension(nlat) :: smfuncveg_yr_g!<
    real, dimension(nlat) :: luc_emc_yr_g  !<
    real, dimension(nlat) :: lucltrin_yr_g !<
    real, dimension(nlat) :: lucsocin_yr_g !<
    real, dimension(nlat) :: burnfrac_yr_g !<
    real, dimension(nlat) :: bterm_yr_g    !<
    real, dimension(nlat) :: lterm_yr_g    !<
    real, dimension(nlat) :: mterm_yr_g    !<
    real, dimension(nlat) :: ch4wet1_yr_g  !<
    real, dimension(nlat) :: ch4wet2_yr_g  !<
    real, dimension(nlat) :: wetfdyn_yr_g  !<
    real, dimension(nlat) :: ch4dyn1_yr_g  !<
    real, dimension(nlat) :: ch4dyn2_yr_g  !<
    real, dimension(nlat) :: ch4soills_yr_g!<
    real, dimension(nlat) :: veghght_yr_g  !<

end type ctem_gridavg_annual

type (ctem_gridavg_annual), save, target :: ctem_grd_yr

!=================================================================================
!>CTEM's variables per tile annual values
type ctem_tileavg_annual

! c      Annual output for CTEM mosaic variables:
! c      (denoted by name ending in "_yr_m")
!
      real, dimension(nlat,nmos) :: laimaxg_yr_t  !<
      real, dimension(nlat,nmos) :: stemmass_yr_t !<
      real, dimension(nlat,nmos) :: rootmass_yr_t !<
      real, dimension(nlat,nmos) :: npp_yr_t      !<
      real, dimension(nlat,nmos) :: gpp_yr_t      !<
      real, dimension(nlat,nmos) :: vgbiomas_yr_t !<
      real, dimension(nlat,nmos) :: autores_yr_t  !<
      real, dimension(nlat,nmos) :: totcmass_yr_t !<
      real, dimension(nlat,nmos) :: litrmass_yr_t !<
      real, dimension(nlat,nmos) :: soilcmas_yr_t !<
      real, dimension(nlat,nmos) :: nep_yr_t      !<
      real, dimension(nlat,nmos) :: litres_yr_t   !<
      real, dimension(nlat,nmos) :: soilcres_yr_t !<
      real, dimension(nlat,nmos) :: hetrores_yr_t !<
      real, dimension(nlat,nmos) :: nbp_yr_t      !<
      real, dimension(nlat,nmos) :: emit_co2_yr_t !<
      real, dimension(nlat,nmos) :: emit_co_yr_t  !<
      real, dimension(nlat,nmos) :: emit_ch4_yr_t !<
      real, dimension(nlat,nmos) :: emit_nmhc_yr_t!<
      real, dimension(nlat,nmos) :: emit_h2_yr_t  !<
      real, dimension(nlat,nmos) :: emit_nox_yr_t !<
      real, dimension(nlat,nmos) :: emit_n2o_yr_t !<
      real, dimension(nlat,nmos) :: emit_pm25_yr_t!<
      real, dimension(nlat,nmos) :: emit_tpm_yr_t !<
      real, dimension(nlat,nmos) :: emit_tc_yr_t  !<
      real, dimension(nlat,nmos) :: emit_oc_yr_t  !<
      real, dimension(nlat,nmos) :: emit_bc_yr_t  !<
      real, dimension(nlat,nmos) :: burnfrac_yr_t !<
      real, dimension(nlat,nmos) :: smfuncveg_yr_t!<
      real, dimension(nlat,nmos) :: bterm_yr_t    !<
      real, dimension(nlat,nmos) :: luc_emc_yr_t  !<
      real, dimension(nlat,nmos) :: lterm_yr_t    !<
      real, dimension(nlat,nmos) :: lucsocin_yr_t !<
      real, dimension(nlat,nmos) :: mterm_yr_t    !<
      real, dimension(nlat,nmos) :: lucltrin_yr_t !<
      real, dimension(nlat,nmos) :: ch4wet1_yr_t  !<
      real, dimension(nlat,nmos) :: ch4wet2_yr_t  !<
      real, dimension(nlat,nmos) :: wetfdyn_yr_t  !<
      real, dimension(nlat,nmos) :: ch4dyn1_yr_t  !<
      real, dimension(nlat,nmos) :: ch4dyn2_yr_t  !<
      real, dimension(nlat,nmos) :: ch4soills_yr_t!<
      real, dimension(nlat,nmos) :: veghght_yr_t  !<

end type ctem_tileavg_annual

type (ctem_tileavg_annual), save, target :: ctem_tile_yr


contains

!=================================================================================

subroutine initrowvars()

use ctem_params, only : nlat, nmos, ican, ignd ,icc, iccp1

implicit none

integer :: j,k,l,m

 do j = 1,nlat

   do k = 1,nmos

!         vrot%PREACC_M(j,k) = 0.
!         vrot%GTACC_M(j,k) = 0.
!         vrot%QEVPACC_M(j,k) = 0.
!         vrot%HFSACC_M(j,k) = 0.
!         vrot%HMFNACC_M(j,k) = 0.
!         vrot%ROFACC_M(j,k) = 0.
!         vrot%SNOACC_M(j,k) = 0.
!         vrot%OVRACC_M(j,k) = 0.
!         vrot%WTBLACC_M(j,k) = 0.
!
!         vrot%ALVSACC_M(j,k) = 0.
!         vrot%ALIRACC_M(j,k) = 0.
!         vrot%RHOSACC_M(j,k) = 0.
!         vrot%TSNOACC_M(j,k) = 0.
!         vrot%WSNOACC_M(j,k) = 0.
!         vrot%TCANACC_M(j,k) = 0.
!         vrot%RCANACC_M(j,k) = 0.
!         vrot%SCANACC_M(j,k) = 0.
!         vrot%GROACC_M(j,k) = 0.
!         vrot%FSINACC_M(j,k) = 0.
!         vrot%FLINACC_M(j,k) = 0.
!         vrot%TAACC_M(j,k) = 0.
!         vrot%UVACC_M(j,k) = 0.
!         vrot%PRESACC_M(j,k) = 0.
!         vrot%QAACC_M(j,k) = 0.
!         vrot%EVAPACC_M(j,k) = 0.
!         vrot%FLUTACC_M(j,k) = 0.
        vrot%icount(j,k)           = 0
        vrot%co2conc(j,k)          = 0.0
        vrot%npp(j,k)              = 0.0
        vrot%nep(j,k)              = 0.0
        vrot%hetrores(j,k)         = 0.0
        vrot%autores(j,k)          = 0.0
        vrot%soilcresp(j,k)        = 0.0
        vrot%rm(j,k)               = 0.0
        vrot%rg(j,k)               = 0.0
        vrot%nbp(j,k)              = 0.0
        vrot%litres(j,k)           = 0.0
        vrot%socres(j,k)           = 0.0
        vrot%gpp(j,k)              = 0.0
        vrot%dstcemls(j,k)         = 0.0
        vrot%dstcemls3(j,k)        = 0.0
        vrot%litrfall(j,k)         = 0.0
        vrot%humiftrs(j,k)         = 0.0
        vrot%canres(j,k)           = 0.0
        vrot%rml(j,k)              = 0.0
        vrot%rms(j,k)              = 0.0
        vrot%rmr(j,k)              = 0.0
        vrot%lucemcom(j,k)         = 0.0
        vrot%lucltrin(j,k)         = 0.0
        vrot%lucsocin(j,k)         = 0.0
        vrot%burnfrac(j,k)         = 0.0
        vrot%lterm(j,k)            = 0.0
        vrot%cfluxcg(j,k)          = 0.0
        vrot%cfluxcs(j,k)          = 0.0
        !vrot%TCANOACC_M(j,k)       = 0.0
        !vrot%UVACC_M(j,k)          = 0.0
        !vrot%VVACC_M(j,k)          = 0.0
        !vrot%TCANOACC_OUT(j,k)     = 0.0
        vrot%ch4wet1(j,k)          = 0.0
        vrot%ch4wet2(j,k)          = 0.0
        vrot%wetfdyn(j,k)          = 0.0
        vrot%ch4dyn1(j,k)          = 0.0
        vrot%ch4dyn2(j,k)          = 0.0
        vrot%ch4_soills(j,k)       = 0.0


        do l=1,ignd
            vrot%tbaraccrow_m(j,k,l)  = 0.0
!             vrot%TBARACC_M(j,k,l) = 0.
!             vrot%THLQACC_M(j,k,l) = 0.
!             vrot%THICACC_M(j,k,l) = 0.
!             vrot%THALACC_M(j,k,l) = 0.
        end do

        do l=1,ican
            vrot%ZOLNC(j,k,l)        = 0.0
            vrot%AILC(j,k,l)         = 0.0
            vrot%CMASVEGC(j,k,l)     = 0.0
            vrot%ALVSCTM(j,k,l)      = 0.0
            vrot%ALIRCTM(j,k,l)      = 0.0
            vrot%CSUM(j,k,l)            = 0.0
            vrot%PAIC(j,k,l)         = 0.0
            vrot%SLAIC(j,k,l)        = 0.0

            do m = 1, 3
                vrot%RMATC(j,k,l,m)    = 0.0
            end do

        end do

        do l = 1,icc

            vrot%smfuncveg(j,k,l)         = 0.0
            vrot%ailcmin(j,k,l) = 0.
            vrot%ailcmax(j,k,l) = 0.
            vrot%dvdfcan(j,k,l) = 0.
            vrot%gleafmas(j,k,l) = 0.
            vrot%bleafmas(j,k,l) = 0.
            vrot%stemmass(j,k,l) = 0.
            vrot%rootmass(j,k,l) = 0.
            vrot%pstemmass(j,k,l) = 0.
            vrot%pgleafmass(j,k,l) = 0.
            vrot%litrfallveg(j,k,l)=0.
            vrot%bterm(j,k,l)        = 0.0
            vrot%mterm(j,k,l)        = 0.0
            vrot%ailcg(j,k,l)        = 0.0
            vrot%ailcgs(j,k,l)       = 0.0
            vrot%fcancs(j,k,l)       = 0.0
            vrot%fcanc(j,k,l)        = 0.0
            vrot%fcancmx(j,k,l)      = 0.0
            vrot%co2i1cg(j,k,l)      = 0.0
            vrot%co2i1cs(j,k,l)      = 0.0
            vrot%co2i2cg(j,k,l)      = 0.0
            vrot%co2i2cs(j,k,l)      = 0.0
            vrot%ancsveg(j,k,l)      = 0.0
            vrot%ancgveg(j,k,l)      = 0.0
            vrot%rmlcsveg(j,k,l)     = 0.0
            vrot%rmlcgveg(j,k,l)     = 0.0
            vrot%stemmass(j,k,l)     = 0.0
            vrot%rootmass(j,k,l)     = 0.0
            vrot%ailcb(j,k,l)        = 0.0
            vrot%grwtheff(j,k,l)     = 0.0
            vrot%dvdfcan(j,k,l)      = 0.0
            vrot%bmasveg(j,k,l)      = 0.0
            vrot%tltrleaf(j,k,l)     = 0.0
            vrot%tltrstem(j,k,l)     = 0.0
            vrot%tltrroot(j,k,l)     = 0.0
            vrot%leaflitr(j,k,l)     = 0.0
            vrot%roottemp(j,k,l)     = 0.0
            vrot%afrleaf(j,k,l)      = 0.0
            vrot%afrstem(j,k,l)      = 0.0
            vrot%afrroot(j,k,l)      = 0.0
            vrot%wtstatus(j,k,l)     = 0.0
            vrot%ltstatus(j,k,l)     = 0.0
            vrot%ailcmin(j,k,l)      = 0.0
            vrot%ailcmax(j,k,l)      = 0.0
            vrot%pfcancmx(j,k,l)     = 0.0
            vrot%nfcancmx(j,k,l)     = 0.0
            vrot%nppveg(j,k,l)       = 0.0
            vrot%veghght(j,k,l)      = 0.0
            vrot%rootdpth(j,k,l)     = 0.0
            vrot%gleafmas(j,k,l)     = 0.0
            vrot%bleafmas(j,k,l)     = 0.0
            vrot%anveg(j,k,l)        = 0.0
            vrot%rmlveg(j,k,l)       = 0.0
            vrot%rmlvegacc(j,k,l)    = 0.0
            vrot%rmsveg(j,k,l)       = 0.0
            vrot%rmrveg(j,k,l)       = 0.0
            vrot%rgveg(j,k,l)        = 0.0
            vrot%vgbiomas_veg(j,k,l) = 0.0
            vrot%gppveg(j,k,l) = 0.0
            vrot%autoresveg(j,k,l) = 0.0
            vrot%emit_co2(j,k,l)         =0.0
            vrot%emit_co(j,k,l)          =0.0
            vrot%emit_ch4(j,k,l)         =0.0
            vrot%emit_nmhc(j,k,l)        =0.0
            vrot%emit_h2(j,k,l)          =0.0
            vrot%emit_nox(j,k,l)         =0.0
            vrot%emit_n2o(j,k,l)         =0.0
            vrot%emit_pm25(j,k,l)        =0.0
            vrot%emit_tpm(j,k,l)         =0.0
            vrot%emit_tc(j,k,l)          =0.0
            vrot%emit_oc(j,k,l)          =0.0
            vrot%emit_bc(j,k,l)          =0.0
            vrot%burnvegf(j,k,l)         =0.0

                do m = 1, ignd
                    vrot%rmatctem(j,k,l,m) = 0.0
                end do

            end do !icc
            !
            do l = 1, iccp1
                vrot%litrmass(j,k,l)    = 0.0
                vrot%soilcmas(j,k,l)    = 0.0
                vrot%hetroresveg(j,k,l) = 0.0
                vrot%litresveg(j,k,l) = 0.0
                vrot%soilcresveg(j,k,l) = 0.0
                vrot%nepveg(j,k,l) = 0.0
                vrot%nbpveg(j,k,l) = 0.0
                vrot%humiftrsveg(j,k,l)=0.
            end do !iccp1

   end do !nmos
 end do !nlat

end subroutine initrowvars

!==================================================

subroutine resetclassmon(nltest)

use ctem_params, only : ignd

implicit none

integer, intent(in) :: nltest

integer :: i,j

do i=1,nltest
    class_out%ALVSACC_MO(I)=0.
    class_out%ALIRACC_MO(I)=0.
    class_out%FLUTACC_MO(I)=0.
    class_out%FSINACC_MO(I)=0.
    class_out%FLINACC_MO(I)=0.
    class_out%HFSACC_MO(I) =0.
    class_out%QEVPACC_MO(I)=0.
    class_out%TRANSPACC_MO(I)=0.
    class_out%SNOACC_MO(I) =0.
    class_out%WSNOACC_MO(I)=0.
    class_out%ROFACC_MO(I) =0.
    class_out%PREACC_MO(I) =0.
    class_out%EVAPACC_MO(I)=0.
    class_out%TAACC_MO(I)=0.
    class_out%CANOPYEVAP(I)=0.
    class_out%GROUNDEVAP(I)=0.
    class_out%ALTOTACC_MO(I)=0.
    class_out%altotcntr_m(i)=0

    DO J=1,IGND
        class_out%TBARACC_MO(I,J)=0.
        class_out%THLQACC_MO(I,J)=0.
        class_out%THICACC_MO(I,J)=0.
    end do
end do

end subroutine resetclassmon

!==================================================

subroutine resetclassyr(nltest)

implicit none

integer, intent(in) :: nltest

integer :: i

do i=1,nltest
          class_out%ALVSACC_YR(I)=0.
          class_out%ALIRACC_YR(I)=0.
          class_out%FLUTACC_YR(I)=0.
          class_out%FSINACC_YR(I)=0.
          class_out%FLINACC_YR(I)=0.
          class_out%HFSACC_YR(I) =0.
          class_out%QEVPACC_YR(I)=0.
          class_out%ROFACC_YR(I) =0.
          class_out%PREACC_YR(I) =0.
          class_out%EVAPACC_YR(I)=0.
          class_out%TRANSPACC_YR(I)=0.
          class_out%TAACC_YR(I)=0.
          class_out%ALTOTACC_YR(I)=0.
          class_out%altotcntr_yr(i)=0
end do

end subroutine resetclassyr

!==================================================

subroutine resetdaily(nltest,nmtest)

use ctem_params, only : ignd,icc

implicit none

integer, intent(in) :: nltest
integer, intent(in) :: nmtest

integer :: i,j,k,m

! First reset the grid average
do i=1,nltest
    ctem_grd%gpp_g(i) =0.0
    ctem_grd%npp_g(i) =0.0
    ctem_grd%nep_g(i) =0.0
    ctem_grd%nbp_g(i) =0.0
    ctem_grd%autores_g(i) =0.0
    ctem_grd%hetrores_g(i)=0.0
    ctem_grd%litres_g(i) =0.0
    ctem_grd%socres_g(i) =0.0
    ctem_grd%dstcemls_g(i)=0.0
    ctem_grd%dstcemls3_g(i)=0.0
    ctem_grd%litrfall_g(i)=0.0
    ctem_grd%humiftrs_g(i)=0.0
    ctem_grd%rml_g(i) =0.0
    ctem_grd%rms_g(i) =0.0
    ctem_grd%rmr_g(i) =0.0
    ctem_grd%rg_g(i) =0.0
    ctem_grd%vgbiomas_g(i) =0.0
    ctem_grd%totcmass_g(i) =0.0
    ctem_grd%gavglai_g(i) =0.0
    ctem_grd%gavgltms_g(i) =0.0
    ctem_grd%gavgscms_g(i) =0.0
    ctem_grd%ailcg_g(i)=0.0
    ctem_grd%ailcb_g(i)=0.0
    ctem_grd%tcanoacc_out_g(i) =0.0
    ctem_grd%burnfrac_g(i) =0.0
    ctem_grd%smfuncveg_g(i) =0.0
    ctem_grd%lucemcom_g(i) =0.0
    ctem_grd%lucltrin_g(i) =0.0
    ctem_grd%lucsocin_g(i) =0.0
    ctem_grd%emit_co2_g(i) =0.0
    ctem_grd%emit_co_g(i)  =0.0
    ctem_grd%emit_ch4_g(i) =0.0
    ctem_grd%emit_nmhc_g(i) =0.0
    ctem_grd%emit_h2_g(i) =0.0
    ctem_grd%emit_nox_g(i) =0.0
    ctem_grd%emit_n2o_g(i) =0.0
    ctem_grd%emit_pm25_g(i) =0.0
    ctem_grd%emit_tpm_g(i) =0.0
    ctem_grd%emit_tc_g(i) =0.0
    ctem_grd%emit_oc_g(i) =0.0
    ctem_grd%emit_bc_g(i) =0.0
    ctem_grd%bterm_g(i)   =0.0
    ctem_grd%lterm_g(i)   =0.0
    ctem_grd%mterm_g(i)   =0.0
    ctem_grd%leaflitr_g(i)=0.0
    ctem_grd%tltrleaf_g(i)=0.0
    ctem_grd%tltrstem_g(i)=0.0
    ctem_grd%tltrroot_g(i)=0.0
    ctem_grd%gleafmas_g(i)=0.0
    ctem_grd%bleafmas_g(i)=0.0
    ctem_grd%stemmass_g(i)=0.0
    ctem_grd%rootmass_g(i)=0.0
    ctem_grd%litrmass_g(i)=0.0
    ctem_grd%soilcmas_g(i)=0.0
    ctem_grd%veghght_g(i)=0.0
    ctem_grd%rootdpth_g(i)=0.0
    ctem_grd%roottemp_g(i)=0.0
    ctem_grd%slai_g(i)=0.0
    ctem_grd%CH4WET1_G(i) = 0.0
    ctem_grd%CH4WET2_G(i) = 0.0
    ctem_grd%WETFDYN_G(i) = 0.0
    ctem_grd%CH4DYN1_G(i) = 0.0
    ctem_grd%CH4DYN2_G(i) = 0.0
    ctem_grd%ch4_soills_g(i) = 0.0

    do k=1,ignd
      ctem_grd%rmatctem_g(i,k)=0.0
    enddo

    do j=1,icc
      ctem_grd%afrleaf_g(i,j)=0.0
      ctem_grd%afrstem_g(i,j)=0.0
      ctem_grd%afrroot_g(i,j)=0.0
    enddo

    do m = 1, nmtest

        ctem_tile%leaflitr_t(i,m)=0.0
        ctem_tile%tltrleaf_t(i,m)=0.0
        ctem_tile%tltrstem_t(i,m)=0.0
        ctem_tile%tltrroot_t(i,m)=0.0
        ctem_tile%ailcg_t(i,m)=0.0
        ctem_tile%ailcb_t(i,m)=0.0
        ctem_tile%afrleaf_t(i,m)=0.0
        ctem_tile%afrstem_t(i,m)=0.0
        ctem_tile%afrroot_t(i,m)=0.0
        ctem_tile%veghght_t(i,m)=0.0
        ctem_tile%rootdpth_t(i,m)=0.0
        ctem_tile%roottemp_t(i,m)=0.0
        ctem_tile%slai_t(i,m)=0.0
        ctem_tile%gleafmas_t(i,m) = 0.0
        ctem_tile%bleafmas_t(i,m) = 0.0
        ctem_tile%stemmass_t(i,m) = 0.0
        ctem_tile%rootmass_t(i,m) = 0.0
        ctem_tile%litrmass_t(i,m) = 0.0
        ctem_tile%soilcmas_t(i,m) = 0.0
        ctem_tile%emit_co2_t(i,m) = 0.0
        ctem_tile%emit_co_t(i,m) = 0.0
        ctem_tile%emit_ch4_t(i,m) = 0.0
        ctem_tile%emit_nmhc_t(i,m) = 0.0
        ctem_tile%emit_h2_t(i,m) = 0.0
        ctem_tile%emit_nox_t(i,m) = 0.0
        ctem_tile%emit_n2o_t(i,m) = 0.0
        ctem_tile%emit_pm25_t(i,m) = 0.0
        ctem_tile%emit_tpm_t(i,m) = 0.0
        ctem_tile%emit_tc_t(i,m) = 0.0
        ctem_tile%emit_oc_t(i,m) = 0.0
        ctem_tile%emit_bc_t(i,m) = 0.0

        do k=1,ignd
            ctem_tile%rmatctem_t(i,m,k)=0.0
        enddo

    end do !nmtest
end do !nltest

end subroutine resetdaily

!==================================================
subroutine resetmonthend(nltest,nmtest)

use ctem_params, only : iccp1,icc

implicit none

integer, intent(in) :: nltest
integer, intent(in) :: nmtest

integer :: i,m,j

! These are assigned to mid-month, but are not accumulated so can be
! zeroed out at the same time as the other month-end vars.
do i=1,nltest
    ctem_grd_mo%stemmass_mo_g(i)=0.0
    ctem_grd_mo%rootmass_mo_g(i)=0.0
    ctem_grd_mo%litrmass_mo_g(i)=0.0
    ctem_grd_mo%soilcmas_mo_g(i)=0.0
    ctem_grd_mo%vgbiomas_mo_g(i)=0.0
    ctem_grd_mo%totcmass_mo_g(i)=0.0
  do m = 1, nmtest
        ctem_tile_mo%stemmass_mo_t(i,m)=0.0
        ctem_tile_mo%rootmass_mo_t(i,m)=0.0
        ctem_tile_mo%litrmass_mo_t(i,m)=0.0
        ctem_tile_mo%soilcmas_mo_t(i,m)=0.0
        ctem_tile_mo%vgbiomas_mo_t(i,m)=0.0
        ctem_tile_mo%totcmass_mo_t(i,m)=0.0
        do j = 1,icc
            ctem_mo%stemmass_mo(i,m,j)=0.0
            ctem_mo%rootmass_mo(i,m,j)=0.0
            ctem_mo%litrmass_mo(i,m,j)=0.0
            ctem_mo%soilcmas_mo(i,m,j)=0.0
            ctem_mo%vgbiomas_mo(i,m,j)=0.0
            ctem_mo%totcmass_mo(i,m,j)=0.0
        end do
            ctem_mo%litrmass_mo(i,m,iccp1)=0.0
            ctem_mo%soilcmas_mo(i,m,iccp1)=0.0
            ctem_mo%totcmass_mo(i,m,iccp1)=0.0
  end do
end do

! Now zero out the month end vars.
do i=1,nltest
    ! Grid avg
    ctem_grd_mo%laimaxg_mo_g(i)=0.0
    ctem_grd_mo%npp_mo_g(i)=0.0
    ctem_grd_mo%gpp_mo_g(i)=0.0
    ctem_grd_mo%nep_mo_g(i)=0.0
    ctem_grd_mo%nbp_mo_g(i)=0.0
    ctem_grd_mo%hetrores_mo_g(i)=0.0
    ctem_grd_mo%autores_mo_g(i)=0.0
    ctem_grd_mo%litres_mo_g(i)=0.0
    ctem_grd_mo%soilcres_mo_g(i)=0.0
    ctem_grd_mo%litrfall_mo_g(i)=0.0
    ctem_grd_mo%humiftrs_mo_g(i)=0.0
    ctem_grd_mo%emit_co2_mo_g(i)=0.0
    ctem_grd_mo%emit_co_mo_g(i) =0.0
    ctem_grd_mo%emit_ch4_mo_g(i) =0.0
    ctem_grd_mo%emit_nmhc_mo_g(i) =0.0
    ctem_grd_mo%emit_h2_mo_g(i) =0.0
    ctem_grd_mo%emit_nox_mo_g(i) =0.0
    ctem_grd_mo%emit_n2o_mo_g(i) =0.0
    ctem_grd_mo%emit_pm25_mo_g(i) =0.0
    ctem_grd_mo%emit_tpm_mo_g(i) =0.0
    ctem_grd_mo%emit_tc_mo_g(i) =0.0
    ctem_grd_mo%emit_oc_mo_g(i) =0.0
    ctem_grd_mo%emit_bc_mo_g(i) =0.0
    ctem_grd_mo%smfuncveg_mo_g(i) =0.0
    ctem_grd_mo%luc_emc_mo_g(i) =0.0
    ctem_grd_mo%lucsocin_mo_g(i) =0.0
    ctem_grd_mo%lucltrin_mo_g(i) =0.0
    ctem_grd_mo%burnfrac_mo_g(i) =0.0
    ctem_grd_mo%bterm_mo_g(i)    =0.0
    ctem_grd_mo%lterm_mo_g(i)    =0.0
    ctem_grd_mo%mterm_mo_g(i)    =0.0
    ctem_grd_mo%ch4wet1_mo_g(i)  =0.0
    ctem_grd_mo%ch4wet2_mo_g(i)  =0.0
    ctem_grd_mo%wetfdyn_mo_g(i)  =0.0
    ctem_grd_mo%ch4dyn1_mo_g(i)  =0.0
    ctem_grd_mo%ch4dyn2_mo_g(i)  =0.0
    ctem_grd_mo%ch4soills_mo_g(i)  =0.0

    do m = 1,nmtest
        ! Tile avg
        ctem_tile_mo%laimaxg_mo_t(i,m)=0.0
        ctem_tile_mo%npp_mo_t(i,m)=0.0
        ctem_tile_mo%gpp_mo_t(i,m)=0.0
        ctem_tile_mo%nep_mo_t(i,m)=0.0
        ctem_tile_mo%nbp_mo_t(i,m)=0.0
        ctem_tile_mo%hetrores_mo_t(i,m)=0.0
        ctem_tile_mo%autores_mo_t(i,m)=0.0
        ctem_tile_mo%litres_mo_t(i,m)=0.0
        ctem_tile_mo%soilcres_mo_t(i,m)=0.0
        ctem_tile_mo%litrfall_mo_t(i,m)=0.0
        ctem_tile_mo%humiftrs_mo_t(i,m)=0.0
        ctem_tile_mo%emit_co2_mo_t(i,m)=0.0
        ctem_tile_mo%emit_co_mo_t(i,m) =0.0
        ctem_tile_mo%emit_ch4_mo_t(i,m) =0.0
        ctem_tile_mo%emit_nmhc_mo_t(i,m) =0.0
        ctem_tile_mo%emit_h2_mo_t(i,m) =0.0
        ctem_tile_mo%emit_nox_mo_t(i,m) =0.0
        ctem_tile_mo%emit_n2o_mo_t(i,m) =0.0
        ctem_tile_mo%emit_pm25_mo_t(i,m) =0.0
        ctem_tile_mo%emit_tpm_mo_t(i,m) =0.0
        ctem_tile_mo%emit_tc_mo_t(i,m) =0.0
        ctem_tile_mo%emit_oc_mo_t(i,m) =0.0
        ctem_tile_mo%emit_bc_mo_t(i,m) =0.0
        ctem_tile_mo%smfuncveg_mo_t(i,m) =0.0
        ctem_tile_mo%luc_emc_mo_t(i,m) =0.0
        ctem_tile_mo%lucsocin_mo_t(i,m) =0.0
        ctem_tile_mo%lucltrin_mo_t(i,m) =0.0
        ctem_tile_mo%burnfrac_mo_t(i,m) =0.0
        ctem_tile_mo%bterm_mo_t(i,m)    =0.0
        ctem_tile_mo%lterm_mo_t(i,m)    =0.0
        ctem_tile_mo%mterm_mo_t(i,m)    =0.0
        ctem_tile_mo%ch4wet1_mo_t(i,m)  =0.0
        ctem_tile_mo%ch4wet2_mo_t(i,m)  =0.0
        ctem_tile_mo%wetfdyn_mo_t(i,m)  =0.0
        ctem_tile_mo%ch4dyn1_mo_t(i,m)  =0.0
        ctem_tile_mo%ch4dyn2_mo_t(i,m)  =0.0
        ctem_tile_mo%ch4soills_mo_t(i,m)  =0.0
        ctem_tile_mo%wind_mo_t(i,m) = 0.0

        do j=1,icc
            ! per pft
            ctem_mo%laimaxg_mo(i,m,j)=0.0
            ctem_mo%npp_mo(i,m,j)=0.0
            ctem_mo%gpp_mo(i,m,j)=0.0
            ctem_mo%nep_mo(i,m,j)=0.0
            ctem_mo%nbp_mo(i,m,j)=0.0
            ctem_mo%hetrores_mo(i,m,j)=0.0
            ctem_mo%autores_mo(i,m,j)=0.0
            ctem_mo%litres_mo(i,m,j)=0.0
            ctem_mo%soilcres_mo(i,m,j)=0.0
            ctem_mo%litrfallveg_mo(i,m,j)=0.0
            ctem_mo%humiftrsveg_mo(i,m,j)=0.0
            ctem_mo%emit_co2_mo(i,m,j)=0.0
            ctem_mo%emit_co_mo(i,m,j) =0.0
            ctem_mo%emit_ch4_mo(i,m,j) =0.0
            ctem_mo%emit_nmhc_mo(i,m,j) =0.0
            ctem_mo%emit_h2_mo(i,m,j) =0.0
            ctem_mo%emit_nox_mo(i,m,j) =0.0
            ctem_mo%emit_n2o_mo(i,m,j) =0.0
            ctem_mo%emit_pm25_mo(i,m,j) =0.0
            ctem_mo%emit_tpm_mo(i,m,j) =0.0
            ctem_mo%emit_tc_mo(i,m,j) =0.0
            ctem_mo%emit_oc_mo(i,m,j) =0.0
            ctem_mo%emit_bc_mo(i,m,j) =0.0
            ctem_mo%burnfrac_mo(i,m,j) =0.0
            ctem_mo%bterm_mo(i,m,j) =0.0
            ctem_mo%mterm_mo(i,m,j) =0.0
            ctem_mo%smfuncveg_mo(i,m,j) =0.0
        end do

        ctem_mo%nep_mo(i,m,iccp1)=0.0
        ctem_mo%nbp_mo(i,m,iccp1)=0.0
        ctem_mo%hetrores_mo(i,m,iccp1)=0.0
        ctem_mo%litres_mo(i,m,iccp1)=0.0
        ctem_mo%soilcres_mo(i,m,iccp1)=0.0
        ctem_mo%humiftrsveg_mo(i,m,iccp1)=0.0

    end do !nmtest
end do ! nltest

end subroutine resetmonthend

!==================================================

subroutine resetyearend(nltest,nmtest)

use ctem_params, only : iccp1,icc

implicit none

integer, intent(in) :: nltest
integer, intent(in) :: nmtest

integer :: i,m,j

WRITE (*,*) NLTEST

do i=1,nltest
    ! Grid avg
    ctem_grd_yr%laimaxg_yr_g(i)=0.0
    ctem_grd_yr%stemmass_yr_g(i)=0.0
    ctem_grd_yr%rootmass_yr_g(i)=0.0
    ctem_grd_yr%litrmass_yr_g(i)=0.0
    ctem_grd_yr%soilcmas_yr_g(i)=0.0
    ctem_grd_yr%vgbiomas_yr_g(i)=0.0
    ctem_grd_yr%totcmass_yr_g(i)=0.0
    ctem_grd_yr%veghght_yr_g(i)=0.0
    ctem_grd_yr%npp_yr_g(i)=0.0
    ctem_grd_yr%gpp_yr_g(i)=0.0
    ctem_grd_yr%nep_yr_g(i)=0.0
    ctem_grd_yr%nbp_yr_g(i)=0.0
    ctem_grd_yr%hetrores_yr_g(i)=0.0
    ctem_grd_yr%autores_yr_g(i)=0.0
    ctem_grd_yr%litres_yr_g(i)=0.0
    ctem_grd_yr%soilcres_yr_g(i)=0.0
    ctem_grd_yr%emit_co2_yr_g(i)=0.0
    ctem_grd_yr%emit_co_yr_g(i)=0.0
    ctem_grd_yr%emit_ch4_yr_g(i)=0.0
    ctem_grd_yr%emit_nmhc_yr_g(i)=0.0
    ctem_grd_yr%emit_h2_yr_g(i)=0.0
    ctem_grd_yr%emit_nox_yr_g(i)=0.0
    ctem_grd_yr%emit_n2o_yr_g(i)=0.0
    ctem_grd_yr%emit_pm25_yr_g(i)=0.0
    ctem_grd_yr%emit_tpm_yr_g(i)=0.0
    ctem_grd_yr%emit_tc_yr_g(i)=0.0
    ctem_grd_yr%emit_oc_yr_g(i)=0.0
    ctem_grd_yr%emit_bc_yr_g(i)=0.0
    ctem_grd_yr%smfuncveg_yr_g(i)=0.0
    ctem_grd_yr%luc_emc_yr_g(i)=0.0
    ctem_grd_yr%lucsocin_yr_g(i)=0.0
    ctem_grd_yr%lucltrin_yr_g(i)=0.0
    ctem_grd_yr%burnfrac_yr_g(i)=0.0
    ctem_grd_yr%bterm_yr_g(i)=0.0
    ctem_grd_yr%lterm_yr_g(i)=0.0
    ctem_grd_yr%mterm_yr_g(i)=0.0
    ctem_grd_yr%ch4wet1_yr_g(i)  =0.0
    ctem_grd_yr%ch4wet2_yr_g(i)  =0.0
    ctem_grd_yr%wetfdyn_yr_g(i)  =0.0
    ctem_grd_yr%ch4dyn1_yr_g(i)  =0.0
    ctem_grd_yr%ch4dyn2_yr_g(i)  =0.0
    ctem_grd_yr%ch4soills_yr_g(i)  =0.0

    do m = 1,nmtest
        ! Tile avg
        ctem_tile_yr%laimaxg_yr_t(i,m)=0.0
        ctem_tile_yr%stemmass_yr_t(i,m)=0.0
        ctem_tile_yr%rootmass_yr_t(i,m)=0.0
        ctem_tile_yr%litrmass_yr_t(i,m)=0.0
        ctem_tile_yr%soilcmas_yr_t(i,m)=0.0
        ctem_tile_yr%vgbiomas_yr_t(i,m)=0.0
        ctem_tile_yr%totcmass_yr_t(i,m)=0.0
        ctem_tile_yr%veghght_yr_t(i,m)=0.0
        ctem_tile_yr%npp_yr_t(i,m)=0.0
        ctem_tile_yr%gpp_yr_t(i,m)=0.0
        ctem_tile_yr%nep_yr_t(i,m)=0.0
        ctem_tile_yr%nbp_yr_t(i,m)=0.0
        ctem_tile_yr%hetrores_yr_t(i,m)=0.0
        ctem_tile_yr%autores_yr_t(i,m)=0.0
        ctem_tile_yr%litres_yr_t(i,m)=0.0
        ctem_tile_yr%soilcres_yr_t(i,m)=0.0
        ctem_tile_yr%emit_co2_yr_t(i,m)=0.0
        ctem_tile_yr%emit_co_yr_t(i,m)=0.0
        ctem_tile_yr%emit_ch4_yr_t(i,m)=0.0
        ctem_tile_yr%emit_nmhc_yr_t(i,m)=0.0
        ctem_tile_yr%emit_h2_yr_t(i,m)=0.0
        ctem_tile_yr%emit_nox_yr_t(i,m)=0.0
        ctem_tile_yr%emit_n2o_yr_t(i,m)=0.0
        ctem_tile_yr%emit_pm25_yr_t(i,m)=0.0
        ctem_tile_yr%emit_tpm_yr_t(i,m)=0.0
        ctem_tile_yr%emit_tc_yr_t(i,m)=0.0
        ctem_tile_yr%emit_oc_yr_t(i,m)=0.0
        ctem_tile_yr%emit_bc_yr_t(i,m)=0.0
        ctem_tile_yr%smfuncveg_yr_t(i,m)=0.0
        ctem_tile_yr%luc_emc_yr_t(i,m)=0.0
        ctem_tile_yr%lucsocin_yr_t(i,m)=0.0
        ctem_tile_yr%lucltrin_yr_t(i,m)=0.0
        ctem_tile_yr%burnfrac_yr_t(i,m)=0.0
        ctem_tile_yr%bterm_yr_t(i,m)=0.0
        ctem_tile_yr%lterm_yr_t(i,m)=0.0
        ctem_tile_yr%mterm_yr_t(i,m)=0.0
        ctem_tile_yr%ch4wet1_yr_t(i,m)  =0.0
        ctem_tile_yr%ch4wet2_yr_t(i,m)  =0.0
        ctem_tile_yr%wetfdyn_yr_t(i,m)  =0.0
        ctem_tile_yr%ch4dyn1_yr_t(i,m)  =0.0
        ctem_tile_yr%ch4dyn2_yr_t(i,m)  =0.0
        ctem_tile_yr%ch4soills_yr_t(i,m)  =0.0

        do j=1,icc
            ! per pft
            ctem_yr%laimaxg_yr(i,m,j)=0.0
            ctem_yr%stemmass_yr(i,m,j)=0.0
            ctem_yr%rootmass_yr(i,m,j)=0.0
            ctem_yr%litrmass_yr(i,m,j)=0.0
            ctem_yr%soilcmas_yr(i,m,j)=0.0
            ctem_yr%vgbiomas_yr(i,m,j)=0.0
            ctem_yr%totcmass_yr(i,m,j)=0.0
            ctem_yr%veghght_yr(i,m,j)=0.0
            ctem_yr%npp_yr(i,m,j)=0.0
            ctem_yr%gpp_yr(i,m,j)=0.0
            ctem_yr%nep_yr(i,m,j)=0.0
            ctem_yr%nbp_yr(i,m,j)=0.0
            ctem_yr%hetrores_yr(i,m,j)=0.0
            ctem_yr%autores_yr(i,m,j)=0.0
            ctem_yr%litres_yr(i,m,j)=0.0
            ctem_yr%soilcres_yr(i,m,j)=0.0
            ctem_yr%emit_co2_yr(i,m,j)=0.0
            ctem_yr%emit_co_yr(i,m,j)=0.0
            ctem_yr%emit_ch4_yr(i,m,j)=0.0
            ctem_yr%emit_nmhc_yr(i,m,j)=0.0
            ctem_yr%emit_h2_yr(i,m,j)=0.0
            ctem_yr%emit_nox_yr(i,m,j)=0.0
            ctem_yr%emit_n2o_yr(i,m,j)=0.0
            ctem_yr%emit_pm25_yr(i,m,j)=0.0
            ctem_yr%emit_tpm_yr(i,m,j)=0.0
            ctem_yr%emit_tc_yr(i,m,j)=0.0
            ctem_yr%emit_oc_yr(i,m,j)=0.0
            ctem_yr%emit_bc_yr(i,m,j)=0.0
            ctem_yr%bterm_yr(i,m,j)=0.0
            ctem_yr%mterm_yr(i,m,j)=0.0
            ctem_yr%burnfrac_yr(i,m,j)=0.0
            ctem_yr%smfuncveg_yr(i,m,j)=0.0
        end do

        ctem_yr%hetrores_yr(i,m,iccp1)=0.0
        ctem_yr%litres_yr(i,m,iccp1)=0.0
        ctem_yr%soilcres_yr(i,m,iccp1)=0.0
        ctem_yr%nep_yr(i,m,iccp1)=0.0
        ctem_yr%nbp_yr(i,m,iccp1)=0.0
        ctem_yr%litrmass_yr(i,m,iccp1)=0.0
        ctem_yr%soilcmas_yr(i,m,iccp1)=0.0
        ctem_yr%totcmass_yr(i,m,iccp1)=0.0

    end do !nmtest
end do ! nltest

end subroutine resetyearend

!==================================================
subroutine resetclassaccum(nltest,nmtest)

use ctem_params, only : ignd

implicit none

integer, intent(in) :: nltest
integer, intent(in) :: nmtest

integer :: i,m,j


!  Comment out for now as this is a very BIG Loop

!DO I=1,NLTEST

!    vrot%altotcntr_d(i) = 0

!  DO M=1,NMTEST

!        vrot%PREACC_M(i,m) = 0.
!        vrot%GTACC_M(i,m) = 0.
!        vrot%QEVPACC_M(i,m) = 0.
!        vrot%HFSACC_M(i,m) = 0.
!        vrot%HMFNACC_M(i,m) = 0.
!        vrot%ROFACC_M(i,m) = 0.
!        vrot%SNOACC_M(i,m) = 0.
!        vrot%OVRACC_M(i,m) = 0.
!        vrot%WTBLACC_M(i,m) = 0.
!        vrot%ALVSACC_M(i,m) = 0.
!        vrot%ALIRACC_M(i,m) = 0.
!        vrot%RHOSACC_M(i,m) = 0.
!        vrot%TSNOACC_M(i,m) = 0.
!        vrot%WSNOACC_M(i,m) = 0.
!        vrot%SNOARE_M(i,m) = 0.
!        vrot%TCANACC_M(i,m) = 0.
!        vrot%RCANACC_M(i,m) = 0.
!        vrot%SCANACC_M(i,m) = 0.
!        vrot%GROACC_M(i,m) = 0.
!        vrot%FSINACC_M(i,m) = 0.
!        vrot%FLINACC_M(i,m) = 0.
!        vrot%TAACC_M(i,m) = 0.
!        vrot%UVACC_M(i,m) = 0.
!        vrot%PRESACC_M(i,m) = 0.
!        vrot%QAACC_M(i,m) = 0.
!        vrot%ALTOTACC_M(i,m) = 0.
!        vrot%EVAPACC_M(i,m) = 0.
!        vrot%FLUTACC_M(i,m) = 0.

 !   DO J=1,IGND
!        vrot%TBARACC_M(I,M,J)=0.
!        vrot%THLQACC_M(I,M,J)=0.
!        vrot%THICACC_M(I,M,J)=0.
!        vrot%THALACC_M(I,M,J)=0.
!    end do
!  end do
!end do

end subroutine resetclassaccum


!==================================================

subroutine resetgridavg(nltest)

use ctem_params, only : ignd,icc

implicit none

integer, intent(in) :: nltest

integer :: i,j

        do i = 1, nltest

        ctem_grd%fsstar_g(i) =0.0
        ctem_grd%flstar_g(i) =0.0
        ctem_grd%qh_g(i)     =0.0
        ctem_grd%qe_g(i)     =0.0
        ctem_grd%snomlt_g(i) =0.0
        ctem_grd%beg_g(i)    =0.0
        ctem_grd%gtout_g(i)  =0.0
        ctem_grd%SNOROT_g(i) =0.0
        ctem_grd%RHOSROT_g(i)=0.0
        ctem_grd%WSNOROT_g(i)=0.0
        ctem_grd%altot_g(i)  =0.0
        ctem_grd%ROFROT_g(i) =0.0
        ctem_grd%tpn_g(i)    =0.0
        ctem_grd%ZPNDROT_g(i)=0.0
        ctem_grd%tcn_g(i)=0.0
        ctem_grd%tsn_g(i)=0.0
        ctem_grd%zsn_g(i)=0.0

        do j=1,ignd
         ctem_grd%TBARROT_g(i,j)=0.0
         ctem_grd%THLQROT_g(i,j)=0.0
         ctem_grd%THICROT_g(i,j)=0.0
         ctem_grd%GFLXROT_g(i,j)=0.0
         ctem_grd%HMFGROT_g(i,j)=0.0
         ctem_grd%HTCROT_g(i,j)=0.0
         ctem_grd%QFCROT_g(i,j)=0.0
        end do

        
        ctem_grd%RCANROT_g(i) =0.0
        ctem_grd%SCANROT_g(i) =0.0
        ctem_grd%TROFROT_g(i)=0.0
        ctem_grd%TROOROT_g(i)=0.0
        ctem_grd%TROBROT_g(i)=0.0
        ctem_grd%TROSROT_g(i)=0.0
        ctem_grd%ROFOROT_g(i)=0.0
        ctem_grd%ROFSROT_g(i)=0.0
        ctem_grd%ROFBROT_g(i)=0.0
        ctem_grd%FSGVROT_g(i)=0.0
        ctem_grd%FSGSROT_g(i)=0.0
        ctem_grd%FSGGROT_g(i)=0.0
        ctem_grd%FLGVROT_g(i)=0.0
        ctem_grd%FLGSROT_g(i)=0.0
        ctem_grd%FLGGROT_g(i)=0.0
        ctem_grd%HFSCROT_g(i)=0.0
        ctem_grd%HFSSROT_g(i)=0.0
        ctem_grd%HFSGROT_g(i)=0.0
        ctem_grd%HEVCROT_g(i)=0.0
        ctem_grd%HEVSROT_g(i)=0.0
        ctem_grd%HEVGROT_g(i)=0.0
        ctem_grd%HMFCROT_g(i)=0.0
        ctem_grd%HMFNROT_g(i)=0.0
        ctem_grd%HTCCROT_g(i)=0.0
        ctem_grd%HTCSROT_g(i)=0.0
        ctem_grd%PCFCROT_g(i)=0.0
        ctem_grd%PCLCROT_g(i)=0.0
        ctem_grd%PCPNROT_g(i)=0.0
        ctem_grd%PCPGROT_g(i)=0.0
        ctem_grd%QFCFROT_g(i)=0.0
        ctem_grd%QFCLROT_g(i)=0.0
        ctem_grd%QFNROT_g(i)=0.0
        ctem_grd%QFGROT_g(i)=0.0
        ctem_grd%ROFCROT_g(i)=0.0
        ctem_grd%ROFNROT_g(i)=0.0
        ctem_grd%WTRCROT_g(i)=0.0
        ctem_grd%WTRSROT_g(i)=0.0
        ctem_grd%WTRGROT_g(i)=0.0
        ctem_grd%CDHROT_g(i)=0.0
        ctem_grd%CDMROT_g(i)=0.0
        ctem_grd%SFCUROT_g(i)=0.0
        ctem_grd%SFCVROT_g(i)=0.0

       if (c_switch%ctem_on) then
          do j=1,icc
            ctem_grd%anvegrow_g(i,j)=0.0
            ctem_grd%rmlvegrow_g(i,j)=0.0
          end do
       end if

       end do

end subroutine resetgridavg



!==================================================

subroutine finddaylength(solday,radl,daylength)

! Calculate the daylength based on the latitude and day of year

! Joe Melton Dec 18 2015 (taken from phenlogy.f)

use ctem_params, only : pi

implicit none

real, intent(in) :: solday  !day of year
real, intent(in) :: radl    ! latitude
real, intent(out) :: daylength  ! calculated daylength
real :: theta               ! temp var
real :: decli               ! temp var
real :: term                ! temp var

    theta=0.2163108 + 2.0*atan(0.9671396*tan(0.0086*(solday-186.0)))
    decli=asin(0.39795*cos(theta))    !declination !note I see that CLASS does this also but with different formula...
    term=(sin(radl)*sin(decli))/(cos(radl)*cos(decli))
    term=max(-1.0,min(term,1.0))
    daylength=24.0-(24.0/pi)*acos(term)

end subroutine finddaylength

!==================================================


! separate one:
!
! c     reset mosaic accumulator arrays.
! c
!       do 655 i=1,nml
!          uvaccgat_t(i)=0.0
! 655   continue
! c
!       if (ctem_on) then
!         do 705 i = 1, nml
! c
! c         competitition related variables added by y. peng \\
!           fsinacc_gat(i)=0.
!           flinacc_gat(i)=0.
!           flutacc_gat(i)=0.
!           alswacc_gat(i)=0.
!           allwacc_gat(i)=0.
!           pregacc_gat(i)=0.
! c         competitition related variables added by y. peng //
! c
!           fsnowacc_t(i)=0.0
!           tcanoaccgat_out(i)=tcanoaccgat_t(i)
!           tcanoaccgat_t(i)=0.0
! c
!           tcansacc_t(i)=0.0
!           taaccgat_t(i)=0.0
!           vvaccgat_t(i)=0.0
! c
!           do 715 j=1,ignd
!              tbaraccgat_t(i,j)=0.0
!              tbarcacc_t(i,j)=0.0
!              tbarcsacc_t(i,j)=0.0
!              tbargacc_t(i,j)=0.0
!              tbargsacc_t(i,j)=0.0
!              thliqcacc_t(i,j)=0.0
!              thliqgacc_t(i,j)=0.0
!              thicecacc_t(i,j)=0.0
! 715       continue
! c
!           do 716 j = 1, icc
!             ancsvgac_t(i,j)=0.0
!             ancgvgac_t(i,j)=0.0
!             rmlcsvga_t(i,j)=0.0
!             rmlcgvga_t(i,j)=0.0
! 716       continue
! c
! 705     continue
!       endif  ! if(ctem_on)
!       endif  ! if(ncount.eq.nday)
!=================================================================================
!>@}
end module ctem_statevars
