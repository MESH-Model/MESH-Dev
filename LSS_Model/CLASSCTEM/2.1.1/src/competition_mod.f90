!>\defgroup competition_scheme_bioclim
!>
!!Canadian Terrestrial Ecosystem Model (CTEM)
!!Bioclimatic Parameters Estimation Subroutine
!!
!!
!!The mortality associated with bioclimatic criteria, \f$m_{bioclim}\f$, ensures
!! that PFTs do not venture outside their bioclimatic envelopes. The bioclimatic
!! criteria that determine PFT existence are listed in Table \ref{tab:pftparams}
!! for tree PFTs. Bioclimatic limits are not used for the \f$C_3\f$ and \f$C_4\f$
!! grass PFTs. The bioclimatic limits represent physiological limits to PFT survival
!! that are either not captured in the model or processes that are not sufficiently
!! described by empirical observations to allow their parametrization. Some examples
!! of the latter include a plant's resistance to frost damage and xylem cavitation
!! limits due to moisture stress. The bioclimatic criteria include the minimum 
!!coldest month air temperature (\f$T^{cold}_{min}\f$), the maximum coldest month
!! air temperature (\f$T^{cold}_{max}\f$), the maximum warmest month air temperature
!! (\f$T^{warm}_{max}\f$), the minimum number of annual growing degree days above
!! \f$5\,C\f$ (\f$GDD5_{min}\f$), the minimum annual aridity index (ratio of 
!!potential evapotranspiration to precipitation; \f$arid_{min}\f$) and the 
!!minimum dry season length in a year (\f$dryseason_{min}\f$), where the dry 
!!season length represents the number of consecutive months with precipitation 
!!less than potential evaporation. The bioclimatic indices are updated on a 25 year
!! timescale (\f$T=25\f$) such that the slowly changing value of a bioclimatic 
!!index \f$X(t+1)\f$ for time \f$t+1\f$ is updated using its previous year's value
!! \f$X(t)\f$ and its value \f$x(t)\f$ for the current year as
!!
!!\f[
!!\label{efold} X(t+1)=X(t)e^{-1/T} + x(t) (1 - e^{-1/T}).
!!\f]
!!
!!Equation (\ref{efold}) implies that \f$63\,{\%}\f$ of a sudden change in 
!!the value of a bioclimatic index \f$\Delta x\f$ is reflected in \f$X(t)\f$
!! in \f$T\f$ years \f$(1-e^{T(-1/T)}= 1-e^{-1} = 0.63)\f$, while \f$86\,{\%}\f$ 
!!of the change is reflected in \f$2T\f$ years \f$(1-e^{2T(-1/T)}= 1-e^{-2} = 0.86)
!!\f$.
!!

!------------------------------------------------------------------------------------

!>\defgroup competition_scheme_existence
!>
!>Canadian Terrestrial Ecosystem Model (CTEM)
!>PFT Existence Subroutine 

!------------------------------------------------------------------------------------

!>\defgroup competition_scheme_competition
!>               Canadian Terrestrial Ecosystem Model (CTEM) 
!>                          PFT Competition Subroutine 
!>

!------------------------------------------------------------------------------------

!>\file
!!
!! Central module for all competition scheme-related operations
!!
!!
!!Competition between PFTs in CTEM is based upon modified L--V equations \cite Arora2006-pp 
!!\cite Arora2006-ax. The L--V equations \cite lotka1925elements \cite Volterra1926-iz have
!! been adapted from their initial application for simulating predator--prey interactions
!! in ecosystem models as described below.
!!
!!Competition parametrization
!!
!!The change in fractional coverage (\f$f\f$) of a PFT \f$\alpha\f$ through time, 
!!\f$\frac{\mathrm{d}f_\alpha}{\mathrm{d}t}\f$, is expressed as the result of mortality, 
!!and competition and colonization (CC) interactions with the other PFTs present in a
!! grid cell and bare ground, collectively represented as \f$B\f$ where \f$\alpha \notin B\f$:
!!\f[
!!\label{concepteqn} \frac{\mathrm{d}f_\alpha}{\mathrm{d}t} = g(f_\alpha, f_B) - m_{\alpha} f_\alpha.
!!\f]
!!
!!The CC interactions are represented symbolically by the \f$g(f_\alpha, f_B)\f$ function.
!! Mortality is assumed to be proportional to the number density of plants and represented 
!!by the mortality term, \f$m_{\alpha} f_\alpha\f$. The PFT-dependent mortality rate 
!!(\f$m_{\alpha}\f$; \f$day^{-1}\f$) (described further in Sect. \ref{mort}) produces bare
!! ground via a number of processes, and that bare ground is subsequently available for colonization.
!! We consider the fractional coverage for \f$N\f$ PFTs plus bare ground (\f$f_{N+1}\f$ =
!! \f$f_{bare}\f$) where \f$\sum_{j=1}^{N+1} f_{j}=1\f$. For competition between unequal
!! competitors, the PFTs are ranked in terms of their dominance. If PFT \f$\alpha\f$ is the
!! most dominant, it will invade the area of other PFTs and the bare ground (\f$f_B\f$,
!! \f$\alpha \notin B\f$). Woody PFTs are all more dominant than grass PFTs since trees
!! can successfully invade grasses by overshading them \cite Siemann2003-jl and thus are
!! ranked higher. Within tree or grass PFTs the dominance rank of a PFT is calculated 
!!based upon its colonization rate (\f$c_\alpha\f$; \f$day^{-1}\f$) with higher colonization
!! rates giving a higher dominance ranking. For the general case of PFT \f$\alpha\f$ with a
!! dominance rank of \f$i\f$, we describe the ranking from most dominant to least as 1, 
!!2, \f${\ldots}\f$, \f$i-1\f$, \f$i\f$, \f$i+1\f$, \f${\ldots}\f$, \f$N\f$. 
!!Equation (\ref{concepteqn}) can then be reformulated following a phenomenological
!! approach as
!!
!!\f[
!!\frac{\mathrm{d}f_\alpha}{\mathrm{d}t} = f^b_\alpha(c_{\alpha, i+1}f_{i+1}
!! +c_{\alpha, i+2}f_{i+2} +\ldots+c_{\alpha,N}f_{N})\nonumber\\ 
!!- f_\alpha(c_{1,\alpha}f^b_1 + c_{2,\alpha}f^b_2 + \ldots + c_{(i-1),
!!\alpha}f^b_{i-1})\nonumber\\ - m_{\alpha} f_\alpha,\label{full}
!!\f]
!!
!!where the exponent \f$b\f$ is an empirical parameter, which controls the behaviour
!! of the L--V equations. In the original L--V formulation, \f$b\f$ is 1, but we
!! modify the L--V relations by using \f$b = 0\f$ following \cite Arora2006-pp 
!!\cite Arora2006-ax (implications of this choice are expanded upon below). The 
!!fractional cover of PFT \f$\alpha\f$ then changes depending on the gains it makes
!! into the area of less dominant PFTs and the losses it suffers due to mortality
!! and encroachment by more dominant PFTs. The rate of change of the bare fraction,
!! \f$f_{bare}\f$, is expressed as
!!
!!\f[
!!\label{barecol} \frac{\mathrm{d}f_{bare}}{\mathrm{d}t} = \sum_{\beta=1}^{N}
!!(m_\beta f_\beta - c_{\beta, {bare}}f^b_\beta f_{bare}).
!!\f]
!!
!!The rate at which PFT \f$\alpha\f$ invades another PFT \f$\beta\f$ is given by
!!
!!\f[
!!\label{coloniz} c_{\alpha,\beta}f^b_\alpha f_{\beta} = c_\alpha
!!\left(\frac{c_{\alpha,\beta}}{c_\alpha} \right)f^b_\alpha f_{\beta} 
!!= c_\alpha \delta_{\alpha,\beta} f^b_\alpha f_{\beta}.
!!\f]
!!
!!A PFT invading bare ground has an unimpeded \f$\textit{invasion}\f$ rate, \f$c_\alpha\f$.
!! The ratio of the invasion rate by PFT \f$\alpha\f$ into area covered by another PFT
!! \f$\beta\f$ and its unimpeded invasion rate (\f$\frac{c_{\alpha,\beta}}{c_\alpha}\f$)
!! gives the relative efficiency of colonization, termed \f$\delta_{\alpha,\beta}\f$,
!! which is a scalar between 0 and 1. \f$\delta\f$ is 1 for invasion of any PFT into
!! bare ground and 1 for tree PFT invasion into grass PFTs. If a PFT \f$\beta\f$ has
!! a lower dominance ranking than another PFT \f$\alpha\f$ then \f$\delta_{\beta,\alpha}
!!=0\f$ implying that sub-dominant PFTs do not invade dominant PFTs, but get invaded
!! by them, i.e. \f$\delta_{\alpha,\beta}=1\f$.  Equation (\ref{full}) can then 
!!be written more succinctly for each PFT as
!!
!!\f[
!!\label{compact} \frac{\mathrm{d}f_\alpha}{\mathrm{d}t} = \sum_{\beta=1}^{N+1}
!! (c_{\alpha} \delta_{\alpha,\beta}f^b_\alpha f_\beta - c_{\beta}
!! \delta_{\beta,\alpha} f_\alpha f^b_\beta) -  m_{\alpha} f_\alpha.
!!\f]
!!
!!The value of parameter \f$b\f$ is related to the manner in which two PFTs interact,
!! represented by \f$f_{\alpha}^b f_{\beta}\f$, in Eqs. (\ref{full})--(\ref{coloniz}).
!! As a result, the value of \f$b\f$ affects the equilibrium solution for fractional
!! coverage of PFTs as well as how \f$f_i\f$ evolves over time.
!!
!!For the usual form of the L--V equations with \f$b=\delta=1\f$, and for the case of
!! a grid cell with two PFTs, the competition--colonization equations are
!!
!!\f[
!!\frac{\mathrm{d}f_{1}} {\mathrm{d}t} = c_1 f_1 ( f_2 + f_{bare}) - m_1 f_1 
!!\nonumber \\ = c_1 f_1 ( 1 - f_1) - m_1 f_1, \\ \frac{\mathrm{d}f_{2}}
!! {\mathrm{d}t} = c_2 f_2 f_{bare} - c_1 f_1 f_2 - m_2 f_2 \nonumber \\ 
!!= c_2 f_2 (1 - f_1 - f_2) - c_1 f_1 f_2 - m_2 f_2\label{cc_eq_b_eq_1_2},
!!\f]
!!
!!where the dominant PFT 1 invades PFT 2 and the bare fraction, and PFT 2 invades
!! only the bare fraction. The equilibrium solutions for $f_1$ and $f_2$ in this case are
!!
!!\f[
!!f_1=max  \left[ \frac{c_1 - m_1}{c_1}, 0 \right], \vspace*{-4mm}
!!\f]
!!
!!\f[
!!f_2 = max \left[  \frac{c_2 - c_2 f_1 - c_1 f_1 - m_2}{c_2}, 0   \right]
!! \nonumber \\ = max \left[  \frac{(c_2 - m_2)-(1+\frac{c_2}{c_1})(c_1-m_1)}{c_2},0
!!   \right],\label{f2_eq_b_eq_1}
!!\f]
!!
!!In Eq. (\ref{f2_eq_b_eq_1}), as long as \f$(c_1 - m_1)\f$ > \f$(c_2 - m_2)\f$
!! the equilibrium solution for \f$f_2\f$ will always be zero and coexistence is not possible.
!!
!!For \f$b=0\f$ and \f$\delta=1\f$, the competition--colonization equations are
!!
!!\f[
!!\frac{\mathrm{d}f_{1}} {\mathrm{d}t} = c_1 ( f_2 + f_{bare}) - m_1 f_1 \nonumber \\ 
!!= c_1 ( 1 - f_1) - m_1 f_1, \\ \frac{\mathrm{d}f_{2}} {\mathrm{d}t} = c_2 f_{bare}
!! - c_1 f_2 - m_2 f_2 \nonumber \\ = c_2 (1 - f_1 - f_2) - c_1 f_2 - m_2 f_2,\label{cc_eq_b_eq_0_1}
!!\f]
!!
!!and the corresponding equilibrium fractions are
!!
!!\f[
!!\label{f_equil_b_eq_0_1} f_1 = \frac{c_1}{c_1 + m_1}, \vspace*{-4mm}
!!\f]
!!
!!\f[
!!\label{f_equil_b_eq_0_2}  f_2 = \frac{c_2(1 - f_1)}{(c_1 + c_2 + m_2)}.
!!\f]
!!
!!In Eqs. (\ref{f_equil_b_eq_0_1}) and (\ref{f_equil_b_eq_0_2}), as long as
!! \f$m_1> 0\f$ and \f$c_2 > 0\f$, then PFT 2 will always exist and
!! equilibrium coexistence is possible. Values of parameter \f$b\f$
!! between 1 and 0 yield equilibrium values of \f$f_2\f$ that vary 
!!between 0 (Eq. \ref{f2_eq_b_eq_1}) and those obtained using Eq. 
!!(\ref{f_equil_b_eq_0_2}). \f$b=0\f$ yields a maximum value of 
!!equilibrium \f$f_2\f$ allowing PFT 2 to coexist maximally.
!!
!!In the standard L--V equations for predator--prey interactions coexistence
!! is possible because the predator depends on prey for its food and so the
!! predator population suffers as the prey population declines. This is in
!! contrast to the application of the equations for competition between PFTs
!! where the dominant PFT does not depend on sub-dominant PFTs for its existence
!! and is thus able to exclude them completely. The PFTs interact with each other
!! through the invasion term \f$(-c_{\beta} \delta_{\beta,\alpha} 
!!f_\alpha f^b_\beta)\f$ in Eq. (\ref{compact}), where \f$\delta_{\alpha,\beta}
!! = 1\f$ or \f$0\f$ depending on whether PFT \f$\alpha\f$ can or cannot 
!!invade PFT \f$\beta\f$, respectively, as mentioned earlier. This interaction 
!!through invasion is represented by \f$-c_1 f_1 f_2\f$ in Eq.( \ref{cc_eq_b_eq_1_2})
!! (for \f$b=1\f$) and by \f$-c_1 f_2\f$ in Eq. (\ref{cc_eq_b_eq_0_1}) (for \f$b=0\f$).
!! The magnitude of this interaction thus depends on the value of parameter $b$.
!! When $b=1$ the interaction is proportional to the product of the fractional
!! coverage of the two PFTs (\f$f_1 f_2\f$). When \f$b=0\f$, the interaction is
!! proportional to the fractional coverage of the PFT being invaded (\f$f_2\f$).
!! The use of \f$b=0\f$ thus reduces the product term \f$f_{\alpha}^b f_{\beta}\f$
!! to \f$f_\beta\f$ and implies that the invasion of sub-dominant PFT \f$\beta\f$
!! does not depend on the current fractional coverage of the dominant PFT \f$\alpha\f$.
!! This case may be thought of as corresponding to the general availability of the
!! seeds of the dominant PFT \f$\alpha\f$ that may germinate and invade the coverage
!! of the sub-dominant PFT \f$\beta\f$ provided the climate is favourable, even if PFT
!! \f$\alpha\f$ does not exist in the grid cell, i.e. \f$f_\alpha = 0\f$ (in the case
!! where \f$f_\alpha = 0\f$, the PFT is always assumed to have a dormant seed bank
!! in the grid cell given the long lifetimes of seeds and their wide dispersion).
!! In contrast, in the standard version of the L--V equations, as implemented for
!! predator--prey interactions, \f$b\f$ always equals \f$1\f$ since the amount of
!! predation, and hence the reduction in the number of prey, depends on the product
!! of the number of predators and the number of prey. Using \f$b=0\f$ is thus 
!!consistent with invasion of the sub-dominant PFT \f$\beta\f$ being unaffected by
!! the fractional coverage of the dominant PFT \f$\alpha\f$.
!!
!!
!!Colonization rate
!!
!!
!!The PFT-dependent colonization rate (\f$c_\alpha\f$; \f$day^{-1}\f$) is calculated based
!! on the fraction (\f$\Lambda_\alpha\f$) of positive NPP (\f$kg\,C\,m^{-2}\,day^{-1}\f$) 
!!that is used for spatial expansion
!!
!!\f[
!!\label{c_a} c_\alpha = {\Lambda_\alpha\, NPP_\alpha\,\xi_{\alpha}},
!!\f]
!!
!!where \f$\xi_{\alpha}\f$ (\f$(kg\,C)^{-1}\,m^{2}\f$) is the inverse sapling density 
!!calculated as the reciprocal of vegetation biomass (\f$C_{veg,\alpha}\f$; \f$kg\,C\,m^{-2}\f$)
!! multiplied by a PFT-dependent constant (\f$S_{sap,\alpha}\f$; unitless; see also ctem_params.f90)
!!
!!\f[
!!\label{xi} \xi_{\alpha}=\frac{1}{S_{sap,\alpha}\,\max[0.25,\min(5.0, C_{veg,\alpha})]}.
!!\f]
!!
!!The fraction of NPP used for spatial expansion, \f$\Lambda_\alpha\f$, is calculated using the
!! leaf area index (\f${LAI}_\alpha\f$; \f$m^2\,leaf\,(m^{2}\,ground)^{-1}\f$) of a PFT
!!
!!\f[
!!\Lambda_{\alpha}=\min(\lambda_{max}, \max (\lambda_{1,\alpha}, \lambda_{2,\alpha})), \vspace*{-4mm}
!!\f]
!!
!!\f[
!!if   LAI_\alpha \leq LAI_{min,\alpha}:\nonumber \\ \quad \lambda_{1,\alpha} =0 \nonumber \\ 
!!if   LAI_{min,\alpha} < LAI_\alpha < LAI_{max,\alpha}:\nonumber \\ \quad  \lambda_{1,\alpha}
!! =\frac{LAI_\alpha - LAI_{min,\alpha}} {LAI_{max,\alpha} - LAI_{min,\alpha}} \lambda_{max}
!! \nonumber \\ if   LAI_\alpha \geq LAI_{max,\alpha}:\nonumber \\ \quad \lambda_{1,\alpha}
!! =\lambda_{max} \label{lam1} \vspace*{-4mm}
!!\f]
!!
!!\f[
!!if   LAI_\alpha > 0.25 LAI_{min,\alpha}:\nonumber \\ \quad \lambda_{2,\alpha} =\cosh(0.115(LAI_\alpha
!! - 0.25 LAI_{min,\alpha})) - 1 \nonumber \\ if   LAI_\alpha \leq 0.25 LAI_{min,\alpha}: \nonumber \\
!! \quad \lambda_{2,\alpha} = 0\label{lam2}
!!\f]
!!
!!The original formulation of \cite Arora2006-pp only considered \f$\lambda_{1,\alpha}\f$ but here
!! we adjust the parametrization with the addition of \f$\lambda_{2,\alpha}\f$, which ensures
!! that a small fraction of NPP is used for spatial expansion even at very low LAI values. 
!!This additional constraint allows for improved fractional coverage of grasses in arid 
!!regions. Similar to \f$S_{sap,\alpha}\f$, \f$LAI_{min,\alpha}\f$ and 
!!\f$LAI_{max,\alpha}\f$ are PFT-dependent parameters (see also ctem_params.f90).
!!
!!The value of \f$\lambda_{max}\f$ is set to 0.1 so that a maximum of 10\,{\%} of 
!!daily NPP can be used for spatial expansion. Finally, \f$\Lambda_\alpha\f$ is 
!!set to zero for tree PFTs when they are in a full leaf-out mode and all NPP is
!! being used for leaf expansion (see Appendix \ref{phenol}).
!!
!!
!!

! J. Melton. Jun 22, 2013

module competition_scheme

implicit none

! Subroutines contained in this module:
public  :: bioclim
public  :: existence
public  :: competition

contains

!-------------------------------------------------------------------------------------------------------------

subroutine  bioclim (   iday,        ta,   precip,   netrad, &
                              il1,       il2,      nilg,  leapnow, &
                            tcurm,  srpcuryr, dftcuryr,  inibioclim, &
                           tmonth,  anpcpcur,  anpecur,   gdd5cur, &
                         surmncur,  defmncur, srplscur,  defctcur, &
                           twarmm,    tcoldm,     gdd5,  aridity, &
                         srplsmon,  defctmon, anndefct, annsrpls, &
                           annpcp,  dry_season_length)    

!>\ingroup competition_scheme_bioclim
!!@{

!
!     10  Jun 2014  - Add in new dry_season_length variable
!     R. Shrestha
!
!     25  Jun 2013  - Convert to f90.
!     J. Melton
!
!     22  Nov 2012  - Calling this version 1.1 since a fair bit of ctem
!     V. Arora        subroutines were changed for compatibility with class
!                     version 3.6 including the capability to run ctem in
!                     mosaic/tile version along with class.
!
!     25  May 2004  - This subroutine calculates the bioclimatic
!     V. Arora        parameters that are required for determining
!                     existence of pfts. the bioclimatic parameters 
!                     are the mean monthly temperature of the warmest
!                     and the coldest months, growing degree days 
!                     above 5 c, annual precipitation and potential
!                     evaporation and some aridity parameters that are
!                     function of potential evaporation and precipitation.
!
!                     In addition, all these parameters are updated
!                     in an e-folding sense at some specified time
!                     scale.
!
!                     Note that this subroutine is only necessary when
!                     competition is switched on.


use ctem_params, only : zero, monthdays, monthend

implicit none

! arguments

integer, intent(in) :: iday      !>\var integer, intent(in) :: iday
!>day of the year
integer, intent(in) :: nilg      !< no. of grid cells in latitude circle (this is passed in 
                                 !< as either ilg or nlat depending on mos/comp)
integer, intent(in) :: il1       !< il1=1
integer, intent(in) :: il2       !< il2=nilg
real, dimension(nilg), intent(in)    :: ta        !< mean daily temperature, k
real, dimension(nilg), intent(in)    :: precip    !< daily precipitation (mm/day)
real, dimension(nilg), intent(in)    :: netrad    !< daily net radiation (w/m2)

logical, intent(inout) :: inibioclim !< switch telling if bioclimatic parameters are being
                                     !< initialized from scratch (false) or being initialized
                                     !< from some spun up values(true).
logical, intent(in) :: leapnow       !< true if this year is a leap year. Only used if the switch 'leap' is true.
real, dimension(nilg), intent(inout) :: tcurm     !< temperature of the current month (c)
real, dimension(nilg), intent(inout) :: srpcuryr  !< water surplus for the current year
real, dimension(nilg), intent(inout) :: dftcuryr  !< water deficit for the current year
real, dimension(12,nilg), intent(inout) :: tmonth !< monthly temperatures
real, dimension(nilg), intent(inout) :: anpcpcur  !< annual precipitation for current year (mm)
real, dimension(nilg), intent(inout) :: anpecur   !< annual potential evaporation for current year (mm)
real, dimension(nilg), intent(inout) :: gdd5cur   !< growing degree days above 5 c for current year
real, dimension(nilg), intent(inout) :: surmncur  !< number of months with surplus water for current year
real, dimension(nilg), intent(inout) :: defmncur  !< number of months with water deficit for current year
real, dimension(nilg), intent(inout) :: srplscur  !< water surplus for the current month
real, dimension(nilg), intent(inout) :: defctcur  !< water deficit for the current month

! the following are running averages in an e-folding sense

real, dimension(nilg), intent(inout) :: twarmm    !< temperature of the warmest month (c)
real, dimension(nilg), intent(inout) :: tcoldm    !< temperature of the coldest month (c)
real, dimension(nilg), intent(inout) :: gdd5      !< growing degree days above 5 c
real, dimension(nilg), intent(inout) :: aridity   !< aridity index, ratio of potential evaporation to precipitation
real, dimension(nilg), intent(inout) :: srplsmon  !< number of months in a year with surplus water i.e.
                                                  !< precipitation more than potential evaporation
real, dimension(nilg), intent(inout) :: defctmon  !< number of months in a year with water deficit i.e.
                                                  !< precipitation less than potential evaporation
real, dimension(nilg), intent(inout) :: anndefct  !< annual water deficit (mm) 
real, dimension(nilg), intent(inout) :: annsrpls  !< annual water surplus (mm)
real, dimension(nilg), intent(inout) :: annpcp    !< annual precipitation (mm)
real, dimension(nilg), intent(inout) :: dry_season_length !< annual maximum dry month length (months)
 
! local variables
real, dimension(nilg) :: tccuryr
real, dimension(nilg) :: twcuryr
real, dimension(nilg) :: aridcur
real :: wtrbal
integer :: month, atmonthend, temp, nmax, i, j, k, curmonth, m, n, l
integer, save, dimension(:,:), allocatable :: wet_dry_mon_index   
integer, save, dimension(:,:), allocatable :: wet_dry_mon_index2  
real, dimension(:), allocatable, save :: dry_season_length_curyr !<current year's maximum dry month length 

! local parameters
real, parameter :: eftime = 25.00 !< e-folding time scale for updating bioclimatic parameters (years)
real, parameter :: factor=exp(-1.0/eftime) !<faster to calculate this only at compile time.

!     ---------------------------------------------------------------

!     initializations

      if(iday.eq.1)then

        ! Allocate the arrays to find the length of dry season
          allocate(wet_dry_mon_index(nilg,12))
          allocate(wet_dry_mon_index2(nilg,24))
          allocate(dry_season_length_curyr(nilg))

        do 100 i = il1, il2
          gdd5cur(i)=0.0    !< gdd5 for the current year
          anpcpcur(i)=0.0   !< annual precip. for the current year
          anpecur(i)=0.0    !< annual potential evap for the current year
          aridcur(i)=100.0  !< aridity index for the current year
          surmncur(i)=0.    !< months with surplus water for current year
          defmncur(i)=0.    !< months with water deficit for current year
          srpcuryr(i)=0.0   !< current year's water surplus
          dftcuryr(i)=0.0   !< current year's water deficit
          tcurm(i)=0.0      !< temperature of current month
          srplscur(i)=0.0   !< current month's water surplus
          defctcur(i)=0.0   !< current month's water deficit
          dry_season_length_curyr(i) = 0.   !<current year's maximum dry month length  

         do month = 1,12
          tmonth(month,i)=0.0
         end do
100     continue      
      endif

!>Find current month

      curmonth=0
      do 220 k = 2, 13
        if(iday.ge.(monthend(k-1)+1).and.iday.le.monthend(k))then
          curmonth=k-1
        endif
220   continue
        
      if(curmonth.eq.0)then
        call xit('bioclim',-1)
      endif

!>Find if we are at end of month or not
      atmonthend=0
      if (iday.eq.monthend(curmonth+1)) then
        atmonthend=1
      endif
!>
!>Update monthly temperature for the current month, and other
!!variables. at the end of the month we will have average of 
!!all daily temperatures for the current month.
!!
      do 240 i = il1, il2
          tcurm(i)=tcurm(i)+(ta(i)-273.16)*(1.0/real(monthdays(curmonth)))
          gdd5cur(i)=gdd5cur(i)+max(0.0, (ta(i)-273.16-5.0))
          anpcpcur(i)=anpcpcur(i) + precip(i)
!         net radiation (W/m2) x 12.87 = potential evap (mm)
          if (leapnow) then 
            anpecur(i)=anpecur(i) + netrad(i)*12.87*(1.0/366.0)
            wtrbal=precip(i)-(netrad(i)*12.87*(1.0/366.0))
          else
            anpecur(i)=anpecur(i) + netrad(i)*12.87*(1.0/365.0)
            wtrbal=precip(i)-(netrad(i)*12.87*(1.0/365.0))
          endif 
          if(wtrbal.ge.0.0)then
            srplscur(i)=srplscur(i)+wtrbal
          else if(wtrbal.lt.0.0)then
            defctcur(i)=defctcur(i)+abs(wtrbal)
          endif
240   continue
!>
!!If its the end of the month then store the monthly temperature 
!!and set tcurm equal to zero. also check if this month had water
!!deficit or surplus
!!
      do 250 i = il1, il2
        if(atmonthend.eq.1)then
          tmonth(curmonth,i)=tcurm(i)
          if( srplscur(i).ge.defctcur(i) )then
            surmncur(i) = surmncur(i) + 1.
            wet_dry_mon_index(i,curmonth) = 1   
          else if(srplscur(i).lt.defctcur(i) )then
            defmncur(i) = defmncur(i) + 1.
            wet_dry_mon_index(i,curmonth) = -1  
          endif
          srpcuryr(i)=srpcuryr(i)+srplscur(i)
          dftcuryr(i)=dftcuryr(i)+defctcur(i)

          tcurm(i)=0.0    ! temperature of current month
          srplscur(i)=0.0 ! current month's water surplus
          defctcur(i)=0.0 ! current month's water deficit

        endif

        if ((.not. leapnow .and. iday.eq.365) .or. & 
            (leapnow .and. iday.eq.366)) then 
          twcuryr(i)=-9000.0
          tccuryr(i)=9000.0
          if(anpcpcur(i).gt.zero)then
            aridcur(i)=anpecur(i)/anpcpcur(i)
          else
            aridcur(i)=100.0
          endif
                             
         !>this loop doubles up the size of the "wet_dry_mon_index" matrix 
            do j = 1,12          
                do k = 1,2
                   m = (k-1)*12 + j
                   wet_dry_mon_index2(i,m) = wet_dry_mon_index(i,j)
                end do
            end do

            n = 0       !number of dry month
            nmax = 0    !maximum length of dry month
               do l = 1, 24
                   temp = wet_dry_mon_index2(i,l) 
                   if(temp.eq.-1)then
                      n = n+1
                      nmax = max(nmax, n) 
                   else 
                      n = 0
                   end if 
                end do 
            nmax = min(nmax, 12)
            dry_season_length_curyr(i) = real(nmax)

        endif     !iday=365/366

250   continue
!>
!!If its the end of year, then find the temperature of the warmest
!!and the coldest month

      if ((.not. leapnow .and. iday.eq.365) .or. & 
          (leapnow .and. iday.eq.366)) then 

          do 270 i = il1, il2
              twcuryr(i)=maxval(tmonth(:,i))
              tccuryr(i)=minval(tmonth(:,i))
270       continue
!>
!!Update long term moving average of bioclimatic parameters in an 
!!e-folding sense
          if(.not. inibioclim)then
            do i = il1, il2
                twarmm(i)=twcuryr(i)
                tcoldm(i)=tccuryr(i)
                gdd5(i)=gdd5cur(i)
                aridity(i)=aridcur(i)
                srplsmon(i)=surmncur(i)
                defctmon(i)=defmncur(i)
                annsrpls(i)=srpcuryr(i)
                anndefct(i)=dftcuryr(i)
                annpcp(i)=anpcpcur(i)
                dry_season_length(i)=dry_season_length_curyr(i)
            end do
            inibioclim=.true.
          else
            do 280 i = il1, il2
                twarmm(i)=twarmm(i)*factor + twcuryr(i)*(1.0-factor)
                tcoldm(i)=tcoldm(i)*factor + tccuryr(i)*(1.0-factor)
                gdd5(i)  =gdd5(i)*factor + gdd5cur(i)*(1.0-factor)
                aridity(i)=aridity(i)*factor + aridcur(i)*(1.0-factor)
                srplsmon(i)=srplsmon(i)*factor + surmncur(i)*(1.0-factor)
                defctmon(i)=defctmon(i)*factor + defmncur(i)*(1.0-factor)
                annsrpls(i)=annsrpls(i)*factor + srpcuryr(i)*(1.0-factor)
                anndefct(i)=anndefct(i)*factor + dftcuryr(i)*(1.0-factor)
                annpcp(i)=annpcp(i)*factor + anpcpcur(i)*(1.0-factor)
                dry_season_length(i)=dry_season_length(i)*factor + dry_season_length_curyr(i)*(1.0-factor)
280         continue
          endif

         ! Deallocate the arrays for dry season length
          deallocate(wet_dry_mon_index)
          deallocate(wet_dry_mon_index2)
          deallocate(dry_season_length_curyr)

      endif

      return
end subroutine bioclim
!>@}
!-------------------------------------------------------------------------------------------------------------
subroutine  existence(  iday,       il1,      il2,      nilg, &
                             sort,  nol2pfts,                 &
                           twarmm,    tcoldm,     gdd5,  aridity, &
                         srplsmon,  defctmon, anndefct, annsrpls, &
                           annpcp,pftexist,dry_season_length) 

!>\ingroup competition_scheme_existence
!!@{

!
!     12  Jun 2014  - Broadleaf cold deciduous now have a tcolmin constraint
!     J. Melton       and it and broadleaf drought/dry deciduous can not longer
!                     co-exist. Grasses are now able to coexist.
!
!     27  Jan 2014  - Moved parameters to global file (ctem_params.f90)
!     J. Melton
!
!     26  Nov 2013  - Update parameters for global off-line runs
!     J. Melton
!
!     25  Jun 2013  - Convert to f90, incorporate modules, and into larger module.
!     J. Melton         

!     27  May 2004  - This subroutine calculates the existence of
!     V. Arora        pfts in grid cells based on a set of bioclimatic
!                     parameters that are estimated in a running average
!                     sense using some specified timescale. 
!
!                     If long term averaged bioclimatic parameters
!                     indicate non-existence of a pft then an additional
!                     mortality term kicks in the competition eqns. Also
!                     while a pft may be able to exist in a grid cell it
!                     may be excluded by competition from other pfts.
!
!                -->  Note that since the fractional coverage of c3 and
!                -->  c4 crops is going to be prescribed, the model
!                -->  assumes that these pfts can always exist. But, of
!                     course the prescribed fractional coverage of these 
!                     pfts will decide if they are present in a grid cell or
!                     not.

use ctem_params, only : zero, kk, icc, ican, tcoldmin, tcoldmax, twarmmax, &
                        gdd5lmt, aridlmt, dryseasonlmt

implicit none

! arguments

integer, intent(in) :: iday      !< day of the year
integer, intent(in) :: nilg      !< no. of grid cells in latitude circle 
                                 !<(this is passed in as either ilg or nlat depending on mos/comp)
integer, intent(in) :: il1       !< il1=1
integer, intent(in) :: il2       !< il2=nilg

integer, dimension(icc), intent(in) :: sort !< index for correspondence between 9 ctem pfts and
                                            !< size 12 of parameter vectors
integer, dimension(ican), intent(in) :: nol2pfts !< number of level 2 ctem pfts
real, dimension(nilg), intent(in) :: twarmm    !< temperature of the warmest month (c)
real, dimension(nilg), intent(in) :: tcoldm    !< temperature of the coldest month (c)
real, dimension(nilg), intent(in) :: gdd5      !< growing degree days above 5 c
real, dimension(nilg), intent(in) :: aridity   !< aridity index, ratio of potential evaporation to precipitation
real, dimension(nilg), intent(in) :: srplsmon  !< number of months in a year with surplus water i.e.
                                               !< precipitation more than potential evaporation
real, dimension(nilg), intent(in) :: defctmon  !< number of months in a year with water deficit i.e.
                                               !< precipitation less than potential evaporation
real, dimension(nilg), intent(in) :: anndefct  !< annual water deficit (mm) 
real, dimension(nilg), intent(in) :: annsrpls  !< annual water surplus (mm)
real, dimension(nilg), intent(in) :: annpcp    !< annual precipitation (mm)
real, dimension(nilg), intent(in) :: dry_season_length !< length of dry season (months)

logical, dimension(nilg,icc), intent(out) :: pftexist(nilg,icc) !<binary array indicating pfts exist (=1) or not (=0)

! local variables
integer :: i,j

!> ----------------------------------------------------------------------
!>     Constants and parameters are located in ctem_params.f90
!> ----------------------------------------------------------------------

!>go through all grid cells and based on bioclimatic parameters
!>decide if a given pft should exist or not. 

      do 100 i = il1, il2

!>needleleaf evergreen
        j=1
        if(tcoldm(i).le.tcoldmax(sort(j)).and.gdd5(i).ge.gdd5lmt(sort(j)) ) then
           pftexist(i,j)=.true.
        else
           pftexist(i,j)=.false.
        endif

!>needleleaf deciduous
        j=2
        if(tcoldm(i).le.tcoldmax(sort(j)).and.twarmm(i).le.twarmmax(sort(j)).and. &
           gdd5(i).ge.gdd5lmt(sort(j)))then
           pftexist(i,j)=.true.
        else
           pftexist(i,j)=.false.
        endif

!>broadleaf evergreen
        j=3
        if(tcoldm(i).ge.tcoldmin(sort(j)).and. &
           gdd5(i).ge.gdd5lmt(sort(j)))then
           pftexist(i,j)=.true.
        else
           pftexist(i,j)=.false.
        endif

!>broadleaf deciduous cold  (see note below pft 5 too)
        j=4
        if(tcoldm(i).le.tcoldmax(sort(j)).and. &
           gdd5(i).ge.gdd5lmt(sort(j)).and. tcoldm(i).ge.tcoldmin &  
           (sort(j)))then
           pftexist(i,j)=.true.
        else
           pftexist(i,j)=.false.
        endif

!>broadleaf deciduous dry
        j=5
        if(tcoldm(i).ge.tcoldmin(sort(j)).and. aridity(i).ge.aridlmt(sort(j)) &
           .and.dry_season_length(i).ge.dryseasonlmt(sort(j)))then
           pftexist(i,j)=.true.
!>We don't want both broadleaf species co-existing so if it has PFT 5
!>remove PFT 4. 
           pftexist(i,j-1)=.false.
        else
           pftexist(i,j)=.false.
        endif

!>c3 and c4 crops
        pftexist(i,6)=.true.
        pftexist(i,7)=.true.

!>c3 grass
        j=8
!>if(tcoldm(i).le.tcoldmax(sort(j)))then
           pftexist(i,j)=.true.
!>else
!>pftexist(i,j)=.false.
!>endif

!>c4 grass
        j=9
!>if(tcoldm(i).ge.tcoldmin(sort(j)))then
           pftexist(i,j)=.true.
!>else
!>pftexist(i,j)=.false.
!>endif


100   continue
       
      return

end subroutine existence
!>@}

!-------------------------------------------------------------------------------------------------------------

subroutine competition(  iday,      il1,       il2,      nilg, &
                          nol2pfts,   nppveg,   dofire, leapnow, &
                          pftexist,  geremort, intrmort, &
                          gleafmas, bleafmas,  stemmass, rootmass, &
                          litrmass, soilcmas,  grclarea,   lambda, &
                           burnvegf,     sort, pstemmass, pgleafmass, &
                           fcancmx,   fcanmx,  vgbiomas, gavgltms, &
                          gavgscms,  bmasveg,   &
                          add2allo,        colrate,        mortrate)
!>\ingroup competition_scheme_competition
!!@{
!     12  Jun 2014  - Change how carbon used in horizontal expansion is dealt with. We 
!     J. Melton       now have a constant reproductive cost
!
!     26  Mar 2014  - Move disturbance adjustments back via a subroutine called
!     J. Melton       burntobare
!
!     20  Feb 2014  - Move adjustments due to disturbance out of here and into
!     J. Melton       disturbance subroutine.
!
!     27  Jan 2014  - Moved parameters to global file (ctem_params.f90)
!     J. Melton

!     25  Jun 2013  - Convert to f90, incorporate modules, and into larger module.
!     J. Melton
 
!     17  Oct 2012  - Adapt subroutine to any number of crops or grass 
!     J. Melton       pfts
!
!     1   Oct 2012  - Update subroutine and implement for running with
!     Y. Peng         mosaic version of class 3.6
!
!     27  May 2004  - This subroutine calculates the competition between
!     V. Arora        pFTs based on Lotka-Volterra eqns. ot its modified
!                     forms. either option may be used.
!
!                     PFTs that may exist are allowed to compete for
!                     available space in a grid cell. pfts that can't
!                     exist based on long term bioclimatic parameters
!                     are slowly killed by increasing their mortality.               
!

use ctem_params, only : zero, kk, numcrops, numgrass, numtreepfts, &
                        icc, ican, deltat, iccp1, seed, bio2sap, bioclimrt, &
                        tolrance, crop, grass, grass_ind

use disturbance_scheme, only : burntobare


implicit none

! arguments

integer, intent(in) :: iday                             !< day of the year
integer, intent(in) :: nilg                             !< no. of grid cells in latitude circle 
                                                        !<(this is passed in as either ilg or nlat depending on mos/comp)
integer, intent(in) :: il1                              !< il1=1
integer, intent(in) :: il2                              !< il2=nilg
logical ,intent(in) :: dofire                           !< if true then we have disturbance on.
real,  dimension(nilg),  intent(in) :: grclarea         !< grid cell area, km^2
integer, dimension(icc), intent(in) :: sort             !< index for correspondence between 9 ctem pfts and
                                                        !< size 12 of parameter vectors
integer, dimension(ican), intent(in) :: nol2pfts        !< number of level 2 ctem pfts
logical, dimension(nilg,icc), intent(in) :: pftexist    !< indicating pfts exist (T) or not (F)
logical, intent(in) :: leapnow                          !< true if this year is a leap year. Only used if the switch 'leap' is true.
real, dimension(nilg,icc), intent(in) :: geremort       !< growth related mortality (1/day)
real, dimension(nilg,icc), intent(in) :: intrmort       !< intrinsic (age related) mortality (1/day)
real, dimension(nilg,icc), intent(in) :: lambda         !< fraction of npp that is used for spatial expansion
real, dimension(nilg,icc), intent(in) :: burnvegf       !< fractional areas burned, for 9 ctem pfts
real, dimension(nilg,icc), intent(in) :: pstemmass      !< stem mass from previous timestep, is value before fire. used by burntobare subroutine
real, dimension(nilg,icc), intent(in) :: pgleafmass     !< root mass from previous timestep, is value before fire. used by burntobare subroutine
real, dimension(nilg,icc), intent(inout) :: nppveg      !< npp for each pft type /m2 of vegetated area u-mol co2-c/m2.sec
real, dimension(nilg,icc), intent(inout) :: bmasveg     !< total (gleaf + stem + root) biomass for each ctem pft, kg c/m2
real, dimension(nilg,icc), intent(inout) :: gleafmas    !< green leaf mass for each of the 9 ctem pfts, kg c/m2
real, dimension(nilg,icc), intent(inout) :: bleafmas    !< brown leaf mass for each of the 9 ctem pfts, kg c/m2
real, dimension(nilg,icc), intent(inout) :: stemmass    !< stem mass for each of the 9 ctem pfts, kg c/m2
real, dimension(nilg,icc), intent(inout) :: rootmass    !< root mass for each of the 9 ctem pfts, kg c/m2
real, dimension(nilg,iccp1), intent(inout) :: litrmass  !< litter mass for each of the 9 ctem pfts + bare, kg c/m2
real, dimension(nilg,iccp1), intent(inout) :: soilcmas  !< soil carbon mass for each of the 9 ctem pfts + bare, kg c/m2
real, dimension(nilg,icc), intent(inout) :: fcancmx     !< fractional coverage of ctem's 9 pfts
real, dimension(nilg,ican), intent(inout)  :: fcanmx    !< fractional coverage of class' 4 pfts
real, dimension(nilg),     intent(inout) :: vgbiomas    !< grid averaged vegetation biomass, kg c/m2
real, dimension(nilg),     intent(inout) :: gavgltms    !< grid averaged litter mass, kg c/m2
real, dimension(nilg),     intent(inout) :: gavgscms    !< grid averaged soil c mass, kg c/m2
real, dimension(nilg,icc), intent(out) :: add2allo      !< npp kg c/m2.day that is used for expansion and
                                                        !< subsequently allocated to leaves, stem, and root via 
                                                        !< the allocation part of the model.
real, dimension(nilg,icc), intent(out) :: colrate       !< colonization rate (1/day)    
real, dimension(nilg,icc), intent(out) :: mortrate      !< mortality rate

! local variables

integer :: i, j
integer :: n, k, k1, k2, l, a, b, g
integer :: sdfracin
integer, dimension(nilg) :: t1
integer, dimension(icc-numcrops) :: inirank
integer, dimension(nilg,icc-numcrops) :: rank
integer, dimension(nilg,icc-numcrops) :: exist1
integer, dimension(nilg,icc-numcrops) :: useexist
integer, dimension(nilg,icc) :: fraciord
integer, dimension(nilg) :: bareiord  

real :: befrmass, aftrmass
real :: sum1, sum2, sum3,term,sum4 
real :: colmult
real, dimension(nilg,icc) :: mrtboclm
real, dimension(nilg,icc) :: usenppvg
real, dimension(nilg) :: temp
real, dimension(nilg,icc-numcrops) :: usefrac, usec, usem
real, dimension(nilg,icc-numcrops) :: frac
real, dimension(nilg,icc-numcrops) :: c1
real, dimension(nilg,icc-numcrops) :: m1
real, dimension(nilg,icc-numcrops) :: term2, term3, term4, colterm, deathterm
real, dimension(nilg,icc-numcrops) :: delfrac
real, dimension(nilg) :: cropfrac, vegfrac    
real, dimension(nilg,icc) :: chngfrac
real, dimension(nilg,icc) :: expnterm, mortterm  
real, dimension(nilg,icc) :: pglfmass  
real, dimension(nilg,icc) :: pblfmass
real, dimension(nilg,icc) :: protmass
real, dimension(nilg,icc) :: pstmmass 
real, dimension(nilg,icc) :: pfcancmx
real, dimension(nilg) :: mincfrac  
real, dimension(nilg,icc) :: pbiomasvg, biomasvg
real, dimension(nilg,icc) :: putaside
real, dimension(nilg,icc) :: nppvegar 
real, dimension(nilg,iccp1) :: pltrmass
real, dimension(nilg,iccp1) :: psocmass
real, dimension(nilg,iccp1) :: deadmass 
real, dimension(nilg,iccp1) :: pdeadmas
real, dimension(nilg) :: barefrac   
real, dimension(nilg,icc) :: usebmsvg  
real, dimension(nilg,iccp1) ::ownsolc, ownlitr
real, dimension(nilg,icc) :: baresolc
real, dimension(nilg,icc) :: barelitr, baresoilc                
real, dimension(nilg,iccp1) :: incrlitr, incrsolc
real, dimension(nilg) :: pvgbioms
real, dimension(nilg) :: pgavltms
real, dimension(nilg) :: pgavscms
real, dimension(nilg) :: add2dead
real, dimension(nilg) :: gavgputa
real, dimension(nilg) :: gavgnpp       
real, dimension(nilg) :: pbarefra
real, dimension(nilg) :: grsumlit, grsumsoc

!     ---------------------------------------------------------------
!     Constants and parameters are located in ctem_params.f90
!     ---------------------------------------------------------------

! Model switches:

! set desired model to be used to .true. and all other to .false.
!                ** ONLY ONE (1) can be true!! **
logical, parameter :: lotvol=.false. !< original lotka-volterra eqns.
logical, parameter :: arora =.true.  !< modified form of lv eqns with f missing
logical, parameter :: boer  =.false. !< modified form of lv eqns with f missing and a modified self-thinning term

!     ---------------------------------------------------------------

      if(icc.ne.9)                      call xit('competition',-1)
      if(ican.ne.4)                       call xit('competition',-2)
!>
!>set competition parameters according to the model chosen
!>
      if (lotvol) then
        a=1 !< alpha. this is the b in the arora & boer (2006) paper
        b=1 !< beta
        g=0 !< gamma
        colmult=4.00  !< multiplier for colonization rate
      else if (arora) then
        a=0 !< alpha
        b=1 !< beta
        g=0 !< gamma
        colmult=1.00  !< multiplier for colonization rate
      else if (boer) then
        a=0 !< alpha
        b=1 !< beta
        g=1 !< gamma
        colmult=1.00 !< multiplier for colonization rate
      endif

!     ---------------------------------------------------------------

!>First, let's adjust the fractions if fire is turned on.

    if (dofire) then


        call burntobare(il1, il2, nilg, sort, vgbiomas, gavgltms, gavgscms,fcancmx, burnvegf, stemmass, &
                      rootmass, gleafmas, bleafmas, litrmass, soilcmas, pstemmass, pgleafmass, &
                      nppveg)

      !>Since the biomass pools could have changed, update bmasveg.
      do 190 i = il1, il2
        do 195 j = 1, icc
         if (fcancmx(i,j).gt.0.0) then
          bmasveg(i,j)=gleafmas(i,j)+stemmass(i,j)+rootmass(i,j)
         endif
195     continue
190   continue

    end if

!>Do our usual initialization

      do 150 i = il1, il2 
        do 160 j = 1, icc
          pglfmass(i,j)=gleafmas(i,j) ! save all biomasses before making
          pblfmass(i,j)=bleafmas(i,j) ! changes so that we can make sure
          protmass(i,j)=rootmass(i,j) ! mass balance is preserved.
          pstmmass(i,j)=stemmass(i,j)
          pltrmass(i,j)=litrmass(i,j)
          psocmass(i,j)=soilcmas(i,j)
          pfcancmx(i,j)=fcancmx(i,j)

          colrate(i,j)=0.0         ! colonization rate
          mortrate(i,j)=0.0        ! mortality rate
          mrtboclm(i,j)=0.0 ! mortality rate if long-term bioclimatic 
!                           ! conditions become unfavourable
          usenppvg(i,j)=0.0
          chngfrac(i,j)=0.0
          expnterm(i,j)=0.0
          mortterm(i,j)=0.0
          add2allo(i,j)=0.0
          biomasvg(i,j)=0.0
          pbiomasvg(i,j)=0.0
          nppvegar(i,j)=0.0
          deadmass(i,j)=0.0
          pdeadmas(i,j)=0.0
          barelitr(i,j)=0.0    ! kg c of litter added to bare fraction
          baresolc(i,j)=0.0    ! and same for soil c
          fraciord(i,j)=0
          incrlitr(i,j)=0.0
          incrsolc(i,j)=0.0
          ownlitr(i,j)=0.0
          ownsolc(i,j)=0.0
160     continue

      do 170 j = 1, icc-numcrops
          rank(i,j)=j
          frac(i,j)=0.0
          c1(i,j)=0.0
          m1(i,j)=0.0
          usefrac(i,j)=0.0
          usec(i,j)=0.0
          usem(i,j)=0.0
          colterm(i,j)=0.0
          deathterm(i,j)=0.0
          term2(i,j)=0.0
          term3(i,j)=0.0
          term4(i,j)=0.0
          delfrac(i,j)=0.0
          exist1(i,j)=0
          useexist(i,j)=0
170   continue

        cropfrac(i)=0.0
        vegfrac(i)=0.0
        temp(i)=0.0
        t1(i)=0
        mincfrac(i)=0.0
        barefrac(i)=1.0
        pbarefra(i)=1.0
        pvgbioms(i)=vgbiomas(i)  ! store grid average quantities in
        pgavltms(i)=gavgltms(i)  ! temporary arrays
        pgavscms(i)=gavgscms(i)
        vgbiomas(i)=0.0
        gavgltms(i)=0.0
        gavgscms(i)=0.0
        pltrmass(i,iccp1)=litrmass(i,iccp1)
        psocmass(i,iccp1)=soilcmas(i,iccp1)
        deadmass(i,iccp1)=0.0
        pdeadmas(i,iccp1)=0.0
        add2dead(i)=0.0
        gavgputa(i)=0.0 ! grid averaged value of c put aside for allocation
        gavgnpp(i)=0.0  ! grid averaged npp kg c/m2 for balance purposes
        bareiord(i)=0
        grsumlit(i)=0.0
        grsumsoc(i)=0.0
        ownlitr(i,iccp1)=0.0
        ownsolc(i,iccp1)=0.0
        incrlitr(i,iccp1)=0.0
        incrsolc(i,iccp1)=0.0

150   continue
!>
!!initial rank/superiority order for simulating competition. since crops 
!!are not in competition their rank doesn't matter and
!!therefore we only have icc-2 ranks corresponding to the remaining pfts. 
!!the first icc-4 are tree pfts and the last two are the c3 and c4 grasses.
      do j = 1,icc-numcrops
         inirank(j)=j
         do 180 i = il1, il2
           rank(i,j)=inirank(j)
180      continue
      end do 
!>
!!Estimate colonization and mortality rate for each pft, except for
!!crops whose fractional coverage is prescribed.
!!
      do 200 j = 1, icc
       if(.not. crop(j))then  ! do not run for crops
        do 210 i = il1, il2
!>
!!colonization rate (1/day). the factor (deltat/963.62) converts
!!npp from u-mol co2-c/m2.sec -> kg c/m2.day
!!
          usebmsvg(i,j)= min(5.0, max(0.25, bmasveg(i,j)))

          colrate(i,j)=lambda(i,j)*max(0.0,nppveg(i,j))*(deltat/963.62)* &
                colmult*(1.0/(bio2sap(sort(j))*usebmsvg(i,j)))
!>
!!mortality rate is the sum of growth related mortality, intrinsic mortality,
!! and an additional mortality that kicks in
!!when long term averaged bioclimatic conditions become unfavourable for a pft.
!!this last term is based on the binary array pftexist.
!!
          if(.not. pftexist(i,j))then

            if (leapnow) then 
              mrtboclm(i,j)=bioclimrt/366.0
            else 
              mrtboclm(i,j)=bioclimrt/365.0
            endif

          endif

          mortrate(i,j)=geremort(i,j)+intrmort(i,j)+mrtboclm(i,j)

210     continue
       endif
200   continue
!>
!!---> from here on we assume that we only have icc-numcrops pfts <----
!!since crops are not part of the competition.
!!
!!based on npp for each pft find the competition ranks / superiority 
!!order for simulating competition. note that crops
!!are not in competition, so the competition is between the
!!remaining pfts. in addition pfts which shouldn't exist in the
!!grid cell because of unfavourable values of long-term climatic
!!conditions are considered inferior.
!!
      do 220 j = 1, icc
        do 221 i = il1, il2

!        find crop fraction
         if (crop(j)) then
           cropfrac(i)=cropfrac(i)+fcancmx(i,j)
         endif

!        prepare to rank the tree pfts according to their colonization rates 
         if (pftexist(i,j)) then
           usenppvg(i,j)=colrate(i,j)
         end if

221     continue
220   continue
!>
!!bubble sort according to colonization rates NOTE - this only works if no tree species are 
!!indexed at positions > numtreepfts, i.e. the trees must be a contiguous 
!!unit at the start of the indexes. JM Jun 2014
      do 270 j = 1, numtreepfts
        do 280 n = 1, numtreepfts
          do 290 i = il1, il2
            if(usenppvg(i,n).lt.usenppvg(i,j))then
              temp(i)=usenppvg(i,n)
              usenppvg(i,n)=usenppvg(i,j)
              usenppvg(i,j)=temp(i)
              t1(i)=rank(i,n)
              rank(i,n)=rank(i,j)
              rank(i,j)=t1(i)
            endif
290       continue
280     continue
270   continue
!>
!!the rank of c3 and c4 grass is also determined on the basis of
!!their npp but grasses are always assumed to be inferior to tree pfts
!!
      do 310 i = il1, il2 
        if(usenppvg(i,grass_ind(1)).ge.usenppvg(i,grass_ind(2)))then 
          rank(i,grass_ind(1)-numcrops)=grass_ind(1)-numcrops
          rank(i,grass_ind(2)-numcrops)=grass_ind(2)-numcrops
        elseif(usenppvg(i,grass_ind(1)).lt.usenppvg(i,grass_ind(2)))then
          rank(i,grass_ind(1)-numcrops)=grass_ind(2)-numcrops
          rank(i,grass_ind(2)-numcrops)=grass_ind(1)-numcrops
        endif
310   continue
!>
!!with the ranks of all pfts in all grid cells we can now simulate
!!competition between them. for lotka-volterra eqns we need a
!!minimum seeding fraction otherwise the pfts will not expand at all.
!!
      do 330 j = 1, icc-numcrops   ! j now goes from 1 to icc-numcrops
        if(j.le.numtreepfts)then
          n=j
        else
          n=j+numcrops
        endif 
        do 340 i = il1, il2
          frac(i,j)=max(seed,fcancmx(i,n)) 
          if (pftexist(i,n)) then
           exist1(i,j)=1
           c1(i,j)=colrate(i,n)
          else
           exist1(i,j)=0
           c1(i,j)=0.0
          end if
          m1(i,j)=mortrate(i,n)
340     continue
330   continue

!>arrange colonization and mortality rates, and fractions, according to superiority ranks

      do 350 n = 1, icc-numcrops   ! n now goes from 1 to icc-numcrops
        do 360 i = il1, il2
          usefrac(i,n)=frac(i,rank(i,n))
          usec(i,n)=c1(i,rank(i,n))
          usem(i,n)=m1(i,rank(i,n))
          useexist(i,n)=exist1(i,rank(i,n))
360     continue
350   continue

      do 400 n = 1, icc-numcrops   ! n now goes from 1 to icc-numcrops
        do 410 i = il1, il2

          colterm(i,n)=usec(i,n)*(usefrac(i,n)**a) ! colonization term

          sum1 = cropfrac(i)+seed !minbare
          do 420 k = 1, n-1, 1
            sum1 = sum1 + usefrac(i,k)
420       continue          

          term2(i,n)=usec(i,n)*(usefrac(i,n)**a)*(sum1+(usefrac(i,n)**b)) ! self & expansion thinning
          term3(i,n)=usem(i,n)*usefrac(i,n) ! mortality term

          sum2 = 0.0
          do 430 j = 1, n-1, 1
            sum3 = cropfrac(i)
            do 440 k = 1, j-1, 1
              sum3 = sum3 + usefrac(i,k)
440         continue
            sum4 = cropfrac(i)
            do 450 k = 1, j, 1
              sum4 = sum4 + usefrac(i,k)
450         continue
            sum2 = sum2 + ( &
            ( ((1.-sum3)**g)*usec(i,j)*(usefrac(i,j)**a)*usefrac(i,n) )/ &
            ( (1.-sum4)**g )  )
430       continue
          term4(i,n)=sum2  ! invasion
          deathterm(i,n) = term2(i,n) + term3(i,n) + term4(i,n)
          delfrac(i,n)=colterm(i,n)-deathterm(i,n) ! delta fraction

410     continue
400   continue

!>update fractions and check if all fractions are +ve 

      do 500 n = 1, icc-numcrops
        do 510 i = il1, il2
          usefrac(i,n)=usefrac(i,n)+delfrac(i,n)
          if(usefrac(i,n).lt.0.0)then
            write(6,*)'fractional coverage -ve for cell ',i,' and pft',n
            call xit('competition',-5)
          endif
          usefrac(i,n)=max(seed,usefrac(i,n))
510     continue
500   continue
!>
!!with the minimum seeding fraction prescription, especially for
!!lotka volterra eqns the total veg fraction may exceed 1. to
!!prevent this we need to adjust fractional coverage of all non-crop
!!pfts that do not have the minimum fraction.
!!
      do 530 i = il1, il2
        vegfrac(i)=cropfrac(i)   !total vegetation fraction
        mincfrac(i)=cropfrac(i)  !sum of mininum prescribed & crop fractions
530   continue

      do 540 n = 1, icc-numcrops
        do 541 i = il1, il2
          vegfrac(i)=vegfrac(i)+usefrac(i,n)
          if(abs(usefrac(i,n)-seed).le.zero) then
            mincfrac(i)= mincfrac(i)+ usefrac(i,n)
          endif 
541     continue
540   continue

      do 550 n = 1, icc-numcrops
        do 551 i = il1, il2
          if(vegfrac(i).gt.1.0.and. &
          abs(usefrac(i,n)-seed).gt.zero) then
            term =(1.-mincfrac(i))/(vegfrac(i)-mincfrac(i)) 
            usefrac(i,n)=usefrac(i,n)*term
          endif
551     continue
550   continue

!>check again that total veg frac doesn't exceed 1.

      do 560 i = il1, il2
        vegfrac(i)=cropfrac(i) !total vegetation fraction
560   continue

      do 570 n = 1, icc-numcrops
        do 571 i = il1, il2
          vegfrac(i)=vegfrac(i)+usefrac(i,n)
571     continue
570   continue

      do 580 i = il1, il2
        if(vegfrac(i).gt.1.0+1e-5)then
          write(6,*)'vegetation fraction in cell ',i,' greater than'
          write(6,*)'1.0 and equal to ',vegfrac(i) 
          call xit('competition',-6)
        endif
580   continue
!>
!!map delfrac to chngfrac so that we get change in fraction
!!corresponding to the actual number of pfts
!!
      do 590 j = 1, icc-numcrops   ! j now goes from 1 to icc-numcrops
        do 591 i = il1, il2

          if(rank(i,j).le.numtreepfts)then
            k=rank(i,j)
          else
            k=rank(i,j)+2
          endif 
          expnterm(i,k)=colterm(i,j)
          mortterm(i,k)=deathterm(i,j)
          fcancmx(i,k)=usefrac(i,j)
          chngfrac(i,k)=fcancmx(i,k)-pfcancmx(i,k)

591     continue
590   continue

!>---> from here on we get back to our usual icc pfts <----

!>get bare fraction

      do 600 j = 1, icc
        do 601 i = il1, il2
          barefrac(i)=barefrac(i)-fcancmx(i,j)
          pbarefra(i)=pbarefra(i)-pfcancmx(i,j)
601     continue
600   continue

!>check if a pft's fractional cover is increasing or decreasing

      do 620 j = 1, icc
        do 621 i = il1, il2
          if( ( fcancmx(i,j).gt.pfcancmx(i,j)) .and. &
             (abs(pfcancmx(i,j)-fcancmx(i,j)).gt.zero) ) then
              fraciord(i,j)=1
          else if( ( fcancmx(i,j).lt.pfcancmx(i,j)) .and. &
                  (abs(pfcancmx(i,j)-fcancmx(i,j)).gt.zero) ) then
              fraciord(i,j)=-1
          endif
621     continue
620   continue

!>check if bare fraction increases or decreases

      do 640 i = il1, il2
        if( ( barefrac(i).gt.pbarefra(i)) .and. &
           (abs(pbarefra(i)-barefrac(i)).gt.zero) ) then
              bareiord(i)=1  ! increase in bare area
        else if ( ( barefrac(i).lt.pbarefra(i)) .and. &
                 (abs(pbarefra(i)-barefrac(i)).gt.zero) ) then
              bareiord(i)=-1 ! decrease in bare area
        endif
640   continue
!>
!!now that we know the change in fraction for every pft we use its
!!npp for spatial expansion and litter generation. we also spread
!!vegetation biomass uniformly over the new fractions, and generate
!!additional litter from mortality if the fractions decrease.
!!
!!three things can happen here
!!
!!1. fraciord = 0, which means all npp that was used for expansion 
!!becomes litter, due to self/expansion thinning and mortality.
!!
!!2. fraciord = 1, which means a part of or full npp is used for
!!expansion but some litter may also be generated. the part of 
!!npp that is used for expansion needs to be allocated to leaves,
!!stem, and root. rather than doing this here we will let the
!!allocation part handle this. so allocation module will allocate
!!not only the npp that is used for pure vertical expansion but 
!!also this npp. but we will do our part here and spread the
!!vegetation biomass over the new increased fraction.
!!
!!3. fraciord = -1, which means all of the npp is to be used for
!!litter generation but in addition some more litter will be
!!generated from mortality of the standing biomass.
!!
      do 660 j = 1, icc
       if(.not. crop(j))then  ! do not run for crops
        do 661 i = il1, il2

          if(fraciord(i,j).eq.1)then ! Expand

!           reduce biomass density by spreading over larger fraction

            term = (pfcancmx(i,j)/fcancmx(i,j))

            gleafmas(i,j) = gleafmas(i,j)*term
            bleafmas(i,j) = bleafmas(i,j)*term
            stemmass(i,j) = stemmass(i,j)*term
            rootmass(i,j) = rootmass(i,j)*term
            litrmass(i,j) = litrmass(i,j)*term
            soilcmas(i,j) = soilcmas(i,j)*term

!           only a fraction of npp becomes litter which for simplicity
!           and for now we spread over the whole grid cell

!            if(expnterm(i,j).le.zero.and.mortterm(i,j).gt.zero)then
!              write(6,*)'expansion term<= zero when fractional coverage'
!              write(6,*)'is increasing for pft',j,' in grid cell',i
!              write(*,*)'pfcancmx(',i,',',j,')=',pfcancmx(i,j)
!              write(*,*)'fcancmx(',i,',',j,')=',fcancmx(i,j)
!              write(*,*)'expnterm(',i,',',j,')=',expnterm(i,j)
!              call xit('competition',-7)
!            else if(expnterm(i,j).le.zero.and.mortterm(i,j).le.zero)then
!              term = 1.0
!            else
!              term = (mortterm(i,j)/expnterm(i,j))
!            endif

!            add2allo(i,j)=(1.-term)

!           the factor (deltat/963.62) converts npp from u-mol co2-c/m2.sec 
!           -> kg c/m2.deltat

            ! Not in use. JM Jun 2014.
            !incrlitr(i,j) = term*max(0.0,nppveg(i,j))*(deltat/963.62)*lambda(i,j)*pfcancmx(i,j) 
            incrlitr(i,j) = 0. 
            grsumlit(i)=grsumlit(i)+incrlitr(i,j)

!           ! Not in use. JM Jun 2014. -rest put aside for allocation
!           add2allo(i,j) = add2allo(i,j)* max(0.0,nppveg(i,j))*(deltat/963.62)*lambda(i,j)*&
!                          (pfcancmx(i,j)/fcancmx(i,j))
            add2allo(i,j) = 0.

          else if(fraciord(i,j).eq.-1)then ! Contract
!>
!!All npp used for expansion becomes litter plus there is additional mortality of the standing biomass. the npp that 
!!becomes litter is now spread over the whole grid cell. all biomass from fraction that dies due to mortality is 
!!also distributed over the litter pool of whole grid cell.
!!
            incrlitr(i,j) = abs(chngfrac(i,j))*(gleafmas(i,j)+ &  
               bleafmas(i,j)+stemmass(i,j)+rootmass(i,j)+litrmass(i,j))

            ! Not in use. JM Jun 2014.
            !incrlitr(i,j) = incrlitr(i,j)+max(0.0,nppveg(i,j))*(deltat/963.62)*lambda(i,j)*pfcancmx(i,j)  
            incrlitr(i,j) = incrlitr(i,j) 
            grsumlit(i)=grsumlit(i)+incrlitr(i,j)

!           Chop off soil c from the fraction that goes down and
!           spread it uniformly over the soil c pool of entire grid cell

            incrsolc(i,j)=abs(chngfrac(i,j))*soilcmas(i,j)  
            grsumsoc(i)=grsumsoc(i)+incrsolc(i,j)

          else if(fraciord(i,j).eq.0)then

            ! Not in use. JM Jun 2014.
!           all npp used for expansion becomes litter
            !incrlitr(i,j) =max(0.0,nppveg(i,j))*(deltat/963.62)*lambda(i,j)* pfcancmx(i,j) 
            incrlitr(i,j) =0. 
            grsumlit(i)=grsumlit(i)+incrlitr(i,j)

          endif

661     continue
       endif
660   continue
!>
!!if bare fraction decreases then chop off the litter and soil c
!!from the decreased fraction and add it to grsumlit & grsumsoc
!!for spreading over the whole grid cell. if bare fraction increases
!!then spread its litter and soil c uniformly over the increased fraction.
!!
      do 680 i = il1, il2
        if(bareiord(i).eq.-1)then !decrease in bare area

          incrlitr(i,iccp1) =(pbarefra(i)-barefrac(i))*litrmass(i,iccp1)
          grsumlit(i)=grsumlit(i)+incrlitr(i,iccp1)

          incrsolc(i,iccp1) = (pbarefra(i)-barefrac(i))*soilcmas(i,iccp1)
          grsumsoc(i)=grsumsoc(i)+incrsolc(i,iccp1)
 
        else if(bareiord(i).eq.1)then ! increase in bare area

          term = pbarefra(i)/barefrac(i)
          litrmass(i,iccp1)=litrmass(i,iccp1)*term
          soilcmas(i,iccp1)=soilcmas(i,iccp1)*term

        endif
680   continue
!>
!!if a pft is not supposed to exist as indicated by pftexist and its 
!!fractional coverage is really small then get rid of the pft all
!!together and spread its live and dead biomass over the grid cell.
!!
      do 690 j = 1, icc
        do 691 i = il1, il2
          if(.not. pftexist(i,j).and.fcancmx(i,j).lt.1.0e-05)then

            incrlitr(i,j)=incrlitr(i,j)+fcancmx(i,j)*(gleafmas(i,j)+bleafmas(i,j) &
              +stemmass(i,j)+rootmass(i,j)+litrmass(i,j))
            grsumlit(i) = grsumlit(i)+ incrlitr(i,j)

            incrsolc(i,j)=incrsolc(i,j)+fcancmx(i,j)*soilcmas(i,j)
            grsumsoc(i)=grsumsoc(i)+incrsolc(i,j)

            barefrac(i)=barefrac(i)+fcancmx(i,j)

!>adjust litter and soil c mass densities for increase in
!>barefrac over the bare fraction.

            term = (barefrac(i)-fcancmx(i,j))/barefrac(i)
            litrmass(i,iccp1) = litrmass(i,iccp1)*term
            soilcmas(i,iccp1) = soilcmas(i,iccp1)*term

            fcancmx(i,j)=0.0 !FLAG could this cause problems since it is 0 and not seed? JM May 27 

          endif
691     continue
690   continue

!>spread litter and soil c over all pfts and the barefrac

      do 700 j = 1, icc
        do 701 i = il1, il2
          if(fcancmx(i,j).gt.zero)then  
            litrmass(i,j)=litrmass(i,j)+grsumlit(i)
            soilcmas(i,j)=soilcmas(i,j)+grsumsoc(i)
          else
            gleafmas(i,j)=0.0
            bleafmas(i,j)=0.0
            stemmass(i,j)=0.0
            rootmass(i,j)=0.0
            litrmass(i,j)=0.0
            soilcmas(i,j)=0.0
          endif
701     continue
700   continue

      do 720 i = il1, il2
        if(barefrac(i).gt.zero)then
          litrmass(i,iccp1)=litrmass(i,iccp1)+grsumlit(i)
          soilcmas(i,iccp1)=soilcmas(i,iccp1)+grsumsoc(i)
        else
          litrmass(i,iccp1)=0.0
          soilcmas(i,iccp1)=0.0
        endif
720   continue

!>get fcanmxs for use by class based on the new fcancmxs

      do 740 j = 1, ican
        do 741 i = il1, il2
           fcanmx(i,j)=0.0 ! fractional coverage of class' pfts
741     continue
740   continue

      k1=0
      do 750 j = 1, ican
        if(j.eq.1) then
          k1 = k1 + 1
        else
          k1 = k1 + nol2pfts(j-1)
        endif
        k2 = k1 + nol2pfts(j) - 1
        do 751 l = k1, k2
          do 752 i = il1, il2
            fcanmx(i,j)=fcanmx(i,j)+fcancmx(i,l)
752       continue
751     continue
750   continue

!>update grid averaged vegetation biomass, and litter and soil c densities

      do 800 j = 1, icc
        do 801 i = il1, il2
          vgbiomas(i)=vgbiomas(i)+fcancmx(i,j)*(gleafmas(i,j)+ &
                     bleafmas(i,j)+stemmass(i,j)+rootmass(i,j))
          gavgltms(i)=gavgltms(i)+fcancmx(i,j)*litrmass(i,j)
          gavgscms(i)=gavgscms(i)+fcancmx(i,j)*soilcmas(i,j)
801     continue
800   continue

      do 810 i = il1, il2
        gavgltms(i)=gavgltms(i)+( barefrac(i)*litrmass(i,iccp1) )
        gavgscms(i)=gavgscms(i)+( barefrac(i)*soilcmas(i,iccp1) )
810   continue
!>
!>and finally we check the c balance. we were supposed to use a
!!fraction of npp for competition. some of it is used for expansion
!!(this is what we save for allocation), and the rest becomes litter. 
!!so for each pft the total c mass in vegetation and litter pools
!!must all add up to the same value as before competition.
!!
      do 830 j = 1, icc
       if (.not. crop(j)) then  
        do 831 i = il1, il2

          biomasvg(i,j)=fcancmx(i,j)* & 
           (gleafmas(i,j)+bleafmas(i,j)+stemmass(i,j)+rootmass(i,j)) 
          pbiomasvg(i,j)=pfcancmx(i,j)* & 
           (pglfmass(i,j)+pblfmass(i,j)+protmass(i,j)+pstmmass(i,j)) 

!         part of npp that we will use later for allocation
          putaside(i,j)=add2allo(i,j)*fcancmx(i,j) 

          gavgputa(i) = gavgputa(i) + putaside(i,j)

!         litter added to bare
          barelitr(i,j)=grsumlit(i)*fcancmx(i,j) 
          ownlitr(i,j)=incrlitr(i,j) 

!         soil c added to bare
          baresolc(i,j)=grsumsoc(i)*fcancmx(i,j) 
          ownsolc(i,j)=incrsolc(i,j) 

          add2dead(i) = add2dead(i) + barelitr(i,j) + baresolc(i,j)

!         Not in use. JM Jun 2014.
!         npp we had in first place to expand
!         nppvegar(i,j)=max(0.0,nppveg(i,j))*(deltat/963.62)*lambda(i,j)*pfcancmx(i,j)
          nppvegar(i,j)=0.

          gavgnpp(i) = gavgnpp(i) + nppvegar(i,j)

          deadmass(i,j)=fcancmx(i,j)*(litrmass(i,j)+soilcmas(i,j)) 
          pdeadmas(i,j)=pfcancmx(i,j)*(pltrmass(i,j)+psocmass(i,j)) 

!!total mass before competition
          befrmass=pbiomasvg(i,j)+nppvegar(i,j)+pdeadmas(i,j)

!!total mass after competition
          aftrmass=biomasvg(i,j)+putaside(i,j)+deadmass(i,j)- &
                  barelitr(i,j)-baresolc(i,j)+ownlitr(i,j)+ownsolc(i,j)

          if(abs(befrmass-aftrmass).gt.tolrance)then
            write(6,*)'total biomass for pft',j,', and grid cell =',i 
            write(6,*)'does not balance before and after competition'
            write(6,*)' '
            write(6,*)'chngfrac(',i,',',j,')=',chngfrac(i,j)
            write(6,*)'fraciord(',i,',',j,')=',fraciord(i,j)
            write(6,*)' '
            write(6,*)'pbiomasvg(',i,',',j,')=',pbiomasvg(i,j)
            write(6,*)'pdeadmas(',i,',',j,')=',pdeadmas(i,j)
            write(6,*)'nppvegar(',i,',',j,')=',nppvegar(i,j)
            write(6,*)' '
            write(6,*)' biomasvg(',i,',',j,')=',biomasvg(i,j)
            write(6,*)'deadmass(',i,',',j,')=',deadmass(i,j)
            write(6,*)'putaside(',i,',',j,')=',putaside(i,j)
            write(6,*)' '
            write(6,*)'before biomass density = ',gleafmas(i,j)+ &
                  bleafmas(i,j)+stemmass(i,j)+rootmass(i,j)
            write(6,*)'after  biomass density = ',pglfmass(i,j)+ &
                 pblfmass(i,j)+protmass(i,j)+pstmmass(i,j)
            write(6,*)' '
            write(6,*)'barelitr(',i,',',j,')=',barelitr(i,j)
            write(6,*)'baresolc(',i,',',j,')=',baresolc(i,j)
            write(6,*)'ownlitr(',i,',',j,')=',ownlitr(i,j)
            write(6,*)'ownsolc(',i,',',j,')=',ownsolc(i,j)
            write(6,*)' '
            write(6,*)'fcancmx(',i,',',j,')=',fcancmx(i,j)
            write(6,*)'pfcancmx(',i,',',j,')=',pfcancmx(i,j)
            write(6,*)' '
            write(6,*)'abs(befrmass-aftrmass)=',abs(befrmass-aftrmass)
            call xit('competition',-8)
          endif

831     continue
       endif
830   continue

!>check balance over the bare fraction

      j = iccp1
        do 851 i = il1, il2
          deadmass(i,j)=barefrac(i)*(litrmass(i,j)+soilcmas(i,j))
          pdeadmas(i,j)=pbarefra(i)*(pltrmass(i,j)+psocmass(i,j))

          add2dead(i)=(grsumlit(i)+grsumsoc(i))*barefrac(i) 

          ownlitr(i,j)=incrlitr(i,j) 
          ownsolc(i,j)=incrsolc(i,j) 

          befrmass=pdeadmas(i,j)+add2dead(i)
          aftrmass=deadmass(i,j)+ownlitr(i,j)+ownsolc(i,j)

          if(abs(befrmass-aftrmass).gt.tolrance)then
            write(6,*)'total dead mass for grid cell =',i,'does not balance over bare' 
            write(6,*)'pdeadmas(',i,',',j,')=',pdeadmas(i,j)
            write(6,*)'add2dead(',i,') term=',add2dead(i)
            write(6,*)'deadmass(',i,',',j,')=',deadmass(i,j)
            write(6,*)' '
            write(6,*)'ownlitr(',i,',',j,')=',ownlitr(i,j)
            write(6,*)'ownsolc(',i,',',j,')=',ownsolc(i,j)
            write(6,*)' '
            write(6,*)'pbarefra(',i,')=',pbarefra(i)
            write(6,*)'bareiord(',i,') =',bareiord(i)
            write(6,*)'barefrac(',i,')=',barefrac(i)
            write(6,*)' '
            write(6,*)'abs(befrmass-aftrmass)=',abs(befrmass-aftrmass)
            call xit('competition',-9)
          endif
851     continue

      return

end subroutine competition
!>@}
end module


