!>\defgroup disturbance_scheme_disturb
!!
!!Canadian Terrestrial Ecosystem Model (CTEM)
!!Disturbance Subroutine
!!
!!
!!
!!CTEM v. 2.0 represents disturbance as both natural and human-influenced fires.
!! The original fire parametrization corresponding to CTEM v. 1.0 is described
!! in \cite Arora20052ac. The parametrization has since been adapted and used
!! in several other DGVMs \cite Kloster2010-633 \cite Kloster2012-c79
!! \cite Migliavacca2013-eh \cite Li20121c2. CTEM v. 2.0 incorporates changes
!! suggested in these studies as well as several new improvements.
!!
!!Fire in CTEM is simulated using a process-based scheme of intermediate complexity
!! that accounts for all elements of the fire triangle: fuel load, combustibility
!! of fuel, and an ignition source. CTEM represents the probability of a fire 
!!occurrence (\f$P_\mathrm{f}\f$), for a representative area of \f$500\,km^2\f$
!! (\f$a_{rep}\f$), as
!!\f[ \label{fieya} P_\mathrm{f} = P_\mathrm{b}P_\mathrm{i}P_\mathrm{m},\f]
!!where the right hand side terms represent the fire probabilities that are 
!!conditioned on (i) the availability of biomass as a fuel source 
!!(\f$P_\mathrm{b}\f$), (ii) the combustibility of the fuel based on its moisture
!! content (\f$P_\mathrm{m}\f$), and (iii) the presence of an ignition source
!! (\f$P_\mathrm{i}\f$). The probability of fire and the subsequent calculations
!! are performed for each PFT present in a grid cell (but the PFT index
!! \f$\alpha\f$ is omitted for clarity in Eq. \ref{fieya}). Since the CTEM 
!! parametrization is based on one fire per day per representative area, the 
!! representative area has to be sufficiently small that the requirement of only
!! one fire per day is reasonable, yet sufficiently large such that it is not 
!! possible to burn the entire representative area in 1 day. Based on MODIS observed
!! fire counts in Fig. 1 of \cite Li20121c2, \f$500\,km^2\f$ is an appropriate size
!! to not have more than one fire per day and still be a large enough area to be
!! assumed representative of the grid cell as a whole.
!!
!!The \f$P_\mathrm{b}\f$ term depends on the aboveground biomass (\f$B_{ag}\f$)
!! available for sustaining a fire (which includes the green and brown leaf mass,
!! stem mass and litter mass, \f$B_{ag} = C_\mathrm{L} + C_\mathrm{S} + C_\mathrm{D}\f$).
!! Below a lower threshold of aboveground biomass (\f$B_{low}\f$; \f$0.2\, kg\,C\,m^{-2}\f$;
!! similar to \cite Moorcroft2001-co, and \cite Kucharik2000-xk), fire is not 
!!sustained and thus has a probability of 0. Above a biomass of 
!!\f$1.0\, kg\,C\,m^{-2}\f$ (\f$B_{high}\f$), \f$P_\mathrm{b}\f$ is set to 1 as the
!! amount of fuel available is assumed sufficient for fire. \f$P_\mathrm{b}\f$ is 
!! then calculated using the aboveground biomass, \f$B_{ag}\f$ (\f$kg\,C\,m^{-2}\f$)
!! with a linear variation between the upper and lower thresholds as
!!\f[ \label{eqn:Pb} P_\mathrm{b}=\max\left[0, \min\left(1,\frac{B_{ag}-B_{low}}
!!{B_{high} - B_{low}}\right)\right]. \f]
!!
!!The linear decrease of \f$P_\mathrm{b}\f$ from \f$B_{high}\f$ to \f$B_{low}\f$
!! reflects the fragmentation of fuel that occurs as biomass decreases. Fuel 
!!fragmentation impacts upon area burned as it impedes the fire spread rate 
!!\cite Guyette2002-rc.
!!
!!The probability of fire based on the presence of ignition sources (\f$P_\mathrm{i}\f$)
!! is influenced by both natural (lightning) and anthropogenic agents (either 
!!intentional or accidental). An initial lightning scalar, \f$\vartheta_F\f$,
!! that varies between 0 and 1 is found as
!!\f[ \vartheta_F = \max\left[0, \min \left(1,\frac{F_c2g - F_{low}}{F_{high} 
!!- F_{low}} \right)\right],\f]
!!where \f$F_{low}\f$ and \f$F_{high}\f$ represent lower and upper thresholds of
!! cloud-to-ground lightning strikes (\f$F_c2g\f$, \f$flashes\,km^{-2}\,month^{-1}\f$)
!!, respectively. Similar to Eq. (\ref{eqn:Pb}), below the lower threshold 
!!(\f$F_{low}\f$; \f$0.25\,flashes\,km^{-2}\,month^{-1}\f$), \f$\vartheta_F\f$
!! is 0 implying lightning strikes are not sufficient to cause fire ignition, 
!!above the upper threshold (\f$F_{high}\f$; \f$10.0\,flashes\,km^{-2}\,month^{-1}\f$)
!! \f$\vartheta_F\f$ is 1, as ignition sources now do not pose a constraint on fire.
!! The amount of cloud-to-ground lightning, \f$F_c2g\f$, is a fraction of the total
!! lightning based on the relationship derived by \cite Price1993-fm (approximation
!! of their Eqs. 1 and 2) as
!!\f[ F_c2g = 0.22 \exp (0.0059 \times \vert {\Phi}\vert) F_{tot},\f]
!!
!!where \f$\Phi\f$ is the grid cell latitude in degrees and \f$F_{tot}\f$ is the
!! total number of lightning \f$flashes\,km^{-2}\,month^{-1}\f$ (both cloud-to-cloud
!! and cloud-to-ground). The probability of fire due to natural ignition, 
!!\f$P_i,n\f$, depends on the lightning scalar, \f$\vartheta_F\f$, as
!!\f[ P_i,n = y(\vartheta_F) - y(0)(1 -  \vartheta_F) + \vartheta_F[1-y(1)] 
!!\nonumber\\ y(\vartheta_F) = \frac{1}{1 + \exp\left(\frac{0.8 - \vartheta_F}{0.1}\right)}. \f]
!!
!!Fire probability due to ignition caused by humans, \f$P_i,h\f$, is parametrized 
!! following \cite Kloster2010-633 with a dependence on population density,
!! \f$p_\mathrm{d}\f$ (\f$number of people\,km^{-2}\f$)
!!\f[ \label{eqn:Ph} P_i,h = \min\left[1,\left(\frac{p_\mathrm{d}}{p_{thres}}\right)^{0.43}\right], \f]
!!where \f$p_{thres}\f$ is a population threshold (\f$300\,people\,km^{-2}\f$) above which \f$P_{i,h}\f$
!! is 1. The probability of fire conditioned on ignition, \f$P_\mathrm{i}\f$, is then
!! the total contribution from both natural and human ignition sources
!!\f[ \label{eqn:Pi} P_\mathrm{i} = \max[0, \min\{1, P_{i,n} + (1 - P_{i,n})P_{i,h}\}]. \f]
!!
!!The population data used to calculate probability of fire ignition caused by humans
!! and anthropogenic fire suppression (discussed further down in this section) is
!! based on the HYDE 3.1 data set \cite Klein_Goldewijk2010-lh
!!
!!The probability of fire due to the combustibility of the fuel, \f$P_\mathrm{m}\f$,
!! is dependent on the soil moisture in vegetation's root zone and in the litter
!! layer. The root-zone soil wetness (\f$\phi_{root}\f$, Eq. \ref{degsoilsat})
!! is used as a surrogate for the vegetation moisture content and the soil wetness 
!! of the top soil layer as a surrogate for the litter moisture content. If a grid
!! cell is covered by snow, \f$P_\mathrm{m}\f$ is set to zero. The probability of
!! fire conditioned on soil wetness in vegetation's rooting zone, \f$P_{m,V}\f$,
!! is then
!!\f[ P_{m,V} = 1-\tanh
!!\left[\left( \frac{1.75\ \phi_{root}} {E_\mathrm{V}}\right )^2\right],\f]
!!where \f$E_\mathrm{V}\f$ is the extinction soil wetness above which \f$P_{f,V}\f$
!! is reduced to near zero and is set to 0.30.
!!
!!The probability of fire based on the moisture content in the \f$\textit{duff}\f$
!! layer, \f$P_{m,D}\f$, which includes the brown leaf mass (grasses only) and 
!!litter mass (\f$B_{duff} = C_{L,b} + C_\mathrm{D}\f$; \f$kg\,C\,m^{-2}\f$), is
!! calculated in a similar way but uses the soil wetness of the first soil layer,
!! (\f$\phi_1\f$, Eq. \ref{phitheta}), as a surrogate for the moisture in the duff
!! layer itself as
!!\f[ P_{m,D} = 1 -\tanh\left[\left(\frac{1.75 \phi_1}{E_{\mathrm{D}}}\right)^2\right], \f]
!!where the extinction soil wetness for the litter layer, \f$E_{\mathrm{D}}\f$,
!! is set to 0.50, which yields a higher probability of fire for the litter layer
!! than for the vegetation for the same soil wetness. \f$P_\mathrm{m}\f$ is then
!! the weighted average of \f$P_{m,V}\f$ and \f$P_{m,D}\f$ given by
!!\f[ \label{eqn:Pf} P_\mathrm{m} = P_{m,V} (1-f_{duff}) + P_{m,D} f_{duff}
!!\nonumber \\ f_{duff}=\frac{B_{duff}}{B_{ag}}\f]
!!where \f$f_{duff}\f$ is the duff fraction of aboveground combustible biomass.
!!
!!The area burned (\f$a\f$) is assumed to be elliptical in shape for fires based
!! upon the wind speed and properties of an ellipse
!!\f[ a(t)=\pi \frac{l}{2}\frac{w}{2}= \frac{\pi}{2} (v_\mathrm{d}+v_\mathrm{u})v_\mathrm{p}t^2,\f]
!!
!!where \f$l\f$ (\f$m\f$) and \f$w\f$ (\f$m\f$) are the lengths of major and minor
!! axes of the elliptical area burnt; \f$v_\mathrm{d}\f$ (\f$km\,h^{-1}\f$) and 
!! \f$v_\mathrm{u}\f$ (\f$km\,h^{-1}\f$) are the fire spread rates in the downwind
!! and upwind directions, respectively; \f$v_\mathrm{p}\f$ (\f$km\,h^{-1}\f$) is
!! the fire spread rate perpendicular to the wind direction and \f$t\f$ is the 
!!time (\f$h\f$).
!!
!!The fire spread rate in the downwind direction (\f$v_\mathrm{d}\f$) is represented
!! as
!!\f[ \label{firespreadrate} v_\mathrm{d} = v_{d,max}\,g(u)\,h(\phi_{r, d})\f]
!!
!!where \f$v_{d,max}\f$ (\f$km\,h^{-1}\f$) is the PFT-specific maximum fire spread
!! rate from \cite Li20121c2, which is set to zero for crop PFTs (see also 
!! ctem_params.f90). The functions \f$g(u)\f$ accounts for the effect of wind 
!! speed and \f$ h(\phi_{r, d})\f$ accounts for the effect of rooting zone and
!!  duff soil wetness on the fire spread rate, as discussed below.
!!
!!The wind speed (\f$u\f$; \f$km\,h^{-1}\f$) is used to determine the length 
!!(\f$l\f$) to breadth (\f$w\f$) ratio, \f$L_\mathrm{b}\f$, of the elliptical area
!! burned by fire
!!\f[ \label{lb} L_\mathrm{b}= \frac{l}{w} = \frac{v_\mathrm{d} + v_\mathrm{u}}{2v_\mathrm{p}}
!! = 1 + 10 [1 -\exp(-0.06 u)] \f]
!!and its head to back ratio, \f$H_\mathrm{b}\f$, following \cite Li20121c2, as
!!\f[ \label{hb} H_\mathrm{b} = \frac{v_\mathrm{d}}{v_\mathrm{u}} = 
!!\frac{L_\mathrm{b} + (L_\mathrm{b}^2 - 1)^{0.5}}{L_\mathrm{b} - 
!!(L_\mathrm{b}^2 - 1)^{0.5}},
!!\f]
!!which help determine the fire spread rate in the direction perpendicular to
!! the wind speed and in the downward direction. Equations (\ref{lb}) and 
!!(\ref{hb}) are combined to estimate the wind scalar \f$g(u)\f$ as
!!\f[ g(u)= g(0) \frac{2.0 L_\mathrm{b}}{(1 + 1/H_\mathrm{b})} \nonumber\\
!! \frac{g(u)}{g(0)}=\frac{v_\mathrm{d}}{v_\mathrm{p}} = \frac{2.0 L_\mathrm{b}}
!! {(1 + 1/H_\mathrm{b})},
!!\f]
!!
!!which varies between 0.05 and 1. The lower limit is imposed by the \f$g(0)\f$ term,
!! which has a value of 0.05 and represents the fire spread rate in the absence of
!! wind (\f$u = 0\f$); the upper limit is assigned a maximum value of 1. The fire
!! spread rate in the absence of wind is essentially the spread rate in the 
!!direction perpendicular to the wind speed (\f$v_\mathrm{p}\f$). The value of the
!! \f$g(0)\f$ term is derived by considering the case where the wind speed becomes
!! very large. As \f$u\f$ \f$\rightarrow \infty\f$ then \f$L_\mathrm{b}
!! \rightarrow 11\f$ and \f$H_\mathrm{b} \rightarrow 482\f$, while 
!!\f$g(\infty)=1\f$ due to its definition, which yields \f$g(0) = 0.0455
!! \approx 0.05\f$.
!!
!!The dependence of fire spread rate on the rooting zone and duff soil wetness,
!! \f$h(\phi_{r, d})\f$ is represented as
!!\f[ h(\phi_{r, d})= h(\phi_{root})(1-f_{duff}) + h(\phi_{1})f_{duff}\nonumber
!! \\ h(\phi_{root})= \left(1-min \left(1,\frac{\phi_{root}}{E_\mathrm{V}} \right)
!! \right)^2\nonumber \\ h(\phi_{1})= \left(1-min \left(1,\frac{\phi_{1}}{E_\mathrm{D}} \right) \right)^2.
!!\f]
!!
!!Both \f$h(\phi_{root})\f$ and \f$h(\phi_{1})\f$ gradually decrease from 1 
!!(when soil wetness is 0 and soil moisture does not constrain fire spread rate) 
!!to 0 when soil wetness exceeds the respective extinction wetness thresholds, 
!!\f$E_\mathrm{V}\f$ and \f$E_\mathrm{D}\f$.
!!
!!With fire spread rate determined, and the geometry of the burned area defined,
!! the area burned in 1 day, \f$a_{1{\mathrm{d}}}\f$ (\f$km^2\,day^{-1}\f$), 
!!following \cite Li20121c2, is calculated as
!!\f[ a_{1{\mathrm{d}}} = \frac{\pi v_\mathrm{d}^2 t^2}{4L_\mathrm{b}}\left(1 + 
!!\frac{1}{H_\mathrm{b}}\right)^2 \nonumber\\ = \frac{\pi v_\mathrm{d}^2 (24^2)}
!! {4L_\mathrm{b}}\left(1 + \frac{1}{H_\mathrm{b}}\right)^2\label{aburned}
!!\f]
!!by setting \f$t\f$ equal to \f$24\,h\f$.
!!
!!The fire extinguishing probability, \f$q\f$, is used to calculate the duration
!! (\f$\tau\f$, \f$days\f$) of the fire, which in turn is used to calculated the
!! area burned over the duration of the fire, \f${a_{\tau d}}\f$. \f$q\f$ is 
!! represented following \cite Kloster2010-633 as
!!\f[ q = 0.5 + \frac{\max\left[0,0.9 - \exp(-0.025\,p_\mathrm{d})\right]}{2},
!!\f]
!!which yields a value of \f$q\f$ that varies from 0.5 to 0.95 as population density, 
!!\f$p_\mathrm{d}\f$ (\f$number of people\,km^{-2}\f$), increases from zero to 
!! infinity. Higher population density thus implies a higher probability of fire
!! being extinguished. \f$q\f$ represents the probability that a fire will be
!! extinguished on the same day it initiated and the probability that it will 
!! continue to the next day is (\f$1-q\f$). Assuming individual days are independent,
!! the probability that the fire will still be burning on day \f$\tau\f$ is 
!! \f$(1-q)^\tau\f$. The probability that a fire will last exactly $\tau$ days,
!! $P(\tau)$, is the product of the probability that the fire still exists at day
!! \f$\tau\f$ and the probability it will be extinguished on that day hence
!! \f$P(\tau) = q(1-q)^\tau\f$. This yields an exponential distribution of fire 
!!duration whose expected value is
!!
!!\f[ \overline{\tau} = E(\tau) = \sum_{\tau=0}^\infty\,\tau\,q(1-q)^{\tau} 
!!= \frac{1-q}{q}.
!!\f]
!!Based on this fire duration and the area burned in 1 day (Eq. \ref{aburned}),
!! the area burned over the duration of the fire (\f$a_{\tau \mathrm{d}}\f$) 
!!(but still implemented in 1 day since the model does not track individual fires
!! over their duration, \f$km^2\,day^{-1}\f$) is calculated as
!!\f[ a_{\tau \mathrm{d}} =E(a_{1{\mathrm{d}}} \tau^2)=\sum_{\tau=0}^
!! \infty\,a_{1{\mathrm{d}}}\,\tau^2  q(1-q)^{\tau} \\ = a_{1{\mathrm{d}}}\,
!!\frac{(1-q) (2-q)}{q^2}.\nonumber
!!\f]
!!
!!Finally, and reintroducing the PFT index \f$\alpha\f$, the area burned 
!!is extrapolated for a PFT \f$\alpha\f$ (\f$A_{\mathrm{b},\alpha}\f$,\f$km^2\,
!! day^{-1}\f$) to the whole grid cell as
!!\f[A_{\mathrm{b},\alpha}=P_{f,\alpha}\,a_{\tau \mathrm{d},\alpha} 
!! \frac{A_\mathrm{g}f_\alpha}{a_{rep}}, \f]
!!where \f$A_\mathrm{g}\f$ is area of a grid cell (\f$km^2\f$), \f$f_\alpha\f$ 
!!the fractional coverage of PFT \f$\alpha\f$ and \f$a_{rep}\f$ the representative
!! area of \f$500\,km^2\f$, as mentioned earlier. Area burned over the whole grid
!! cell (\f$A_\mathrm{b}\f$, \f$km^2\,day^{-1}\f$) is then calculated as the sum
!! of area burned for individual PFTs,
!!\f[ A_\mathrm{b}=\sum_{\alpha=1}^{N}A_{\mathrm{b},\alpha}.\f]
!!
!!Fire emits \f$CO_2\f$, other trace gases, and aerosols as biomass is burned while
!! plant mortality and damage due to fire contribute to the litter pool. The
!! emissions of a trace gas/aerosol species \f$j\f$ from PFT \f$\alpha\f$, 
!!\f$E_{\alpha,j}\f$ (\f$g species (m^{-2} grid cell area) day^{-1}\f$) are 
!!obtained from a vector of carbon densities \f$\vec{C}_{\alpha} = (C_\mathrm{L},
!! C_\mathrm{S}, C_\mathrm{R}, C_\mathrm{D})_\alpha\f$ (\f$kg\,C\,m^{-2}\f$) for
!! its leaf, stem, root and litter components, multiplied by a vector of combustion
!! factors \f$mho_{\alpha} = (mho_\mathrm{L}, mho_\mathrm{S}, mho_\mathrm{R}, 
!! mho_\mathrm{D})_\alpha\f$, which determines what fraction of leaf, stem, root
!! and litter components gets burned, multiplied by a vector of emissions factors
!! \f$\Upsilon_{j} = (\Upsilon_\mathrm{L}, \Upsilon_\mathrm{S}, \Upsilon_\mathrm{R},
!! \Upsilon_\mathrm{D})_j\f$ (\f$g species (kg\,C\,dry organic matter)^{-1}\f$), and
!! by the area burned \f$A_{\mathrm{b},\alpha}\f$ for that PFT.
!!
!!The dot product of \f$\vec{C}_{\alpha}\f$, \f$\Upsilon_{j}\f$ and \f$mho_{\alpha}
!!\f$ thus yields emissions per unit grid cell area of species \f$j\f$ from PFT 
!!\f$\alpha\f$,
!!\f[ \label{emiss_combust_factor} {E_{\alpha,j}}= ((\vec{C}_\alpha\cdot mho_{\alpha}
!!)\cdot \Upsilon_{j}) \frac{A_{\mathrm{b},\alpha}}{A_\mathrm{g}}\frac{1000}{450},
!!\f]
!!
!!where the constant 1000 converts \f$\vec{C}_\alpha\f$ from \f$kg\,C\,m^{-2}\f$ 
!! to \f$g\,C\,m^{-2}\f$ and the constant 450 (\f$g\,C\,(kg dry organic matter)^{-1}
!!\f$) converts biomass from carbon units to dry organic matter \cite Li20121c2.
!! The corresponding loss of carbon (\f$kg\,C\,m^{-2}\,day^{-1}\f$) from the three
!! live vegetation components (L, S, R) and the litter pool (D) of PFT \f$\alpha\f$
!! is given by
!!\f[ \label{emiss_combust_loss} H_{\alpha, i}= C_{\alpha, i}
!! mho_i\left(\frac{A_{\mathrm{b},\alpha}}{A_\mathrm{g}}\right)\quad i={L, S, R, D}.
!!\f]
!!
!!The PFT-specific combustion factors for leaf (\f$mho_\mathrm{L}\f$), stem 
!!(\f$mho_{\mathrm{S}}\f$), root (\f$mho_{\mathrm{R}}\f$) and litter
!! (\f$mho_{\mathrm{D}}\f$) components are summarized in ctem_params.f90.
!! Emission factors for all species of trace gases and aerosols 
!!(\f$CO_2\f$, \f$CO\f$, \f$CH_4\f$, \f$H_2\f$, \f$NHMC\f$, \f$NO_x\f$,
!! \f$N_2O\f$, total particulate matter, particulate matter less than \f$2.5\,
!!\mu m\f$ in diameter, and black and organic carbon) are based on an updated 
!!set by \cite Andreae2001-e04 listed in Tables 3 and 4 of \cite Li20121c2.
!!
!!Litter generated by fire is based on similar mortality factors, which reflect
!! a PFT's susceptibility to damage due to fire \f$\vec{\Theta}_{\alpha} = 
!! (\Theta_\mathrm{L}, \Theta_\mathrm{S}, \Theta_\mathrm{R})_\alpha\f$ (fraction).
!! The contribution to litter pool of each PFT due to plant mortality associated 
!!with fire (\f$kg\,C\,m^{-2}\,day^{-1}\f$) is calculated as
!!\f[ \label{eqn_using_mort_factors} {M_{\alpha}}= (\vec{C}_\alpha \cdot 
!!\Theta_{\alpha} ) \frac{A_{\mathrm{b},\alpha}}{A_\mathrm{g}},
!!\f]
!!which is the sum of contribution from individual live vegetation pools
!!\f[ \label{eqn_using_mort_factors_individual} M_{\alpha, i}= C_{\alpha, i}
!! \Theta_{\alpha, i} \left(\frac{A_{\mathrm{b},\alpha}}{A_\mathrm{g}} \right)
!!\quad i={L, S, R}.
!!\f]
!!
!!The carbon loss terms associated with combustion of vegetation components and 
!! litter (\f$H_{\alpha, i}, i={L, S, R, D}\f$) and mortality of vegetation components
!! (\f$M_{\alpha, i}, i={L, S, R}\f$) due to fire are used in Eqs. (
!!\ref{rate_change_eqns_live_pools}) and (\ref{rate_change_eqns_dead_pools}), which
!! describe the rate of change of carbon in model's five pools (however, listed 
!! there without the PFT subscript \f$\alpha\f$). The PFT-specific mortality factors
!! for leaf (\f$\Theta_\mathrm{L}\f$), stem (\f$\Theta_{\mathrm{S}}\f$) and root
!! (\f$\Theta_\mathrm{R}\f$) components are listed in ctem_params.f90.
!!
!!When CTEM is run with prescribed PFT fractional cover, the area of PFTs does not
!! change and the fire-related emissions of \f$CO_2\f$, other trace gases and aerosols
!!, and generation of litter act to thin the remaining biomass. When competition
!! between PFTs for space is allowed, fire both thins the remaining biomass and
!! through plant mortality creates bare ground, which is subsequently available for
!! colonization. The creation of bare ground depends on the susceptibility of each
!! PFT to stand replacing fire (\f$\zeta_\mathrm{r}\f$, fraction) (see also 
!!ctem_params.f90) and the PFT area burned. The fire-related mortality rate, 
!!\f$m_{dist}\f$ (\f$day^{-1}\f$), used in Eq. (\ref{mortality}), is then
!!\f[ \label{m_dist} m_{dist,\alpha} = \zeta_{\mathrm{r},\alpha} 
!!\frac{A_{\mathrm{b},\alpha}}{f_\alpha A_\mathrm{g}}.
!!\f]
!!
!!After bare ground generation associated with fire, the thinned biomass
!! is spread uniformly over the remaining fraction of a PFT. However,
!! it is ensured that the carbon density of the remaining biomass does not 
!!increase to a value above what it was before the fire occurred.
!!
!!

!>\defgroup disturbance_scheme_burntobare

!>
!>Update fractional coverages of pfts to take into account the area
!>burnt by fire. Adjust all pools with new densities in their new
!>areas and increase bare fraction.
!>
!>And while we are doing this also run a small check to make sure
!>grid averaged quantities do not get messed up.


!>\file
!!Central module for all disturbance scheme-related operations
module disturbance_scheme

! J. Melton. Mar 26, 2014

implicit none

! Subroutines contained in this module:
public  :: disturb
public  :: burntobare

contains

! ------------------------------------------------------------------


!>\ingroup disturbance_scheme_disturb
!!@{

subroutine disturb (stemmass, rootmass, gleafmas, bleafmas, &
                            thliq,   wiltsm,  fieldsm,    uwind, &
                            vwind,  lightng,  fcancmx, litrmass, &    
                         prbfrhuc, rmatctem, extnprob, popdon,   &
                              il1,      il2,     sort, nol2pfts, &
                         grclarea,    thice,   popdin, lucemcom, &
                           dofire,   currlat,   iday,  fsnow,    &
!     ------------------ inputs above this line ----------------------          
                         stemltdt, rootltdt, glfltrdt, blfltrdt, &
                         glcaemls, rtcaemls, stcaemls, & 
                         blcaemls, ltrcemls, burnfrac, smfunc_veg, &
                         emit_co2, emit_co,  emit_ch4, emit_nmhc, &
                         emit_h2,  emit_nox, emit_n2o, emit_pm25, &
                         emit_tpm, emit_tc,  emit_oc,  emit_bc, &
                         burnvegf, bterm_veg, mterm_veg,    lterm, &
                         pstemmass, pgleafmass )  
!     ------------------outputs above this line ----------------------
!


!
!     11  Jun 2015  - Clean up to make all calculations PFT dependent and add in a
!     V. Arora        capability to do a fire count method for comparison with
!                     our usual parameterization
!
!     26  Mar 2014  - Split subroutine into two and create module. Move all fcancmx
!     J. Melton       adjustments into subroutine burntobare and call from competition
!
!     20  Feb 2014  - Adapt to deal with competition on. Bring in code that makes
!     J. Melton       bare fractions from competition module. Moved parameters to
!                     ctem_params.f90
!
!     4   Jan 2014  - Convert to f90 and include a saturation effect for lightning
!     J. Melton       strikes

!     25  Jul 2013  - Add in module for common params, cleaned up code 
!     J. Melton

!     24  Sep 2012  - add in checks to prevent calculation of non-present
!     J. Melton       pfts

!     09  May 2012  - addition of emission factors and revising of the
!     J. Melton       fire scheme

!     15  May 2003  - this subroutine calculates the litter generated
!     V. Arora        and c emissions from leaves, stem, and root 
!                     components due to fire. c emissions from burned
!                     litter are also estimated. at present no other 
!                     form of disturbance is modelled.


use ctem_params, only : ignd, icc, ilg, ican, zero,kk, pi, c2dom, crop, &
                        iccp1, standreplace, tolrance, bmasthrs_fire, &
                        lwrlthrs, hgrlthrs, parmlght, parblght, reparea, popdthrshld, & 
                        f0, maxsprd, frco2glf, frco2blf, &
                        frltrglf, frltrblf, frco2stm, frltrstm, frco2rt, frltrrt, &
                        frltrbrn, emif_co2, emif_co, emif_ch4, emif_nmhc, emif_h2, &
                        emif_nox, emif_n2o, emif_pm25, emif_tpm, emif_tc, emif_oc, emif_bc, &
                        grass, extnmois_veg, extnmois_duff, iccp1

implicit none

!     Outputs
!
!     emission factors for trace gases and aerosols. units are
!     g of compound emitted per kg of dry organic matter.
!     values are taken from li et al. 2012 biogeosci 
!     emif_co2  - carbon dioxide
!     emif_co   - carbon monoxide
!     emif_ch4  - methane
!     emif_nmhc - non-methane hydrocarbons
!     emif_h2   - hydrogen gas
!     emif_nox  - nitrogen oxides
!     emif_n2o  - nitrous oxide
!     emif_pm25 - particulate matter less than 2.5 um in diameter
!     emif_tpm  - total particulate matter
!     emif_tc   - total carbon
!     emif_oc   - organic carbon
!     emif_bc   - black carbon


real, dimension(ilg,icc), intent(out) :: pstemmass 
real, dimension(ilg,icc), intent(out) :: pgleafmass

logical, intent(in) :: popdon !<if set true use population density data to calculate fire extinguishing 
                              !<probability and probability of fire due to human causes, 
                              !<or if false, read directly from .ctm file

integer :: il1 !<il1=1
integer :: il2 !<il2=ilg
integer :: i,j,k,m,k1,k2,n

integer :: sort(icc) !<index for correspondence between 9 pfts and size 12 of parameters vectors
integer :: nol2pfts(ican) !<number of level 2 ctem pfts
integer :: iday

logical :: dofire !<boolean, if true allow fire, if false no fire.
logical :: fire(ilg) !<fire occuring

real :: stemmass(ilg,icc)  !<stem mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real :: rootmass(ilg,icc)  !<root mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real :: gleafmas(ilg,icc)  !<green leaf mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real :: bleafmas(ilg,icc)  !<brown leaf mass
real :: thliq(ilg,ignd)    !<liquid soil moisture content
real :: wiltsm(ilg,ignd)   !<wilting point soil moisture content
real :: fieldsm(ilg,ignd)  !<field capacity soil moisture content
real :: uwind(ilg)         !<wind speed, \f$m/s\f$
real :: vwind(ilg)         !<wind speed, \f$m/s\f$
real :: fcancmx(ilg,icc)   !<fractional coverages of ctem's 9 pfts
real :: lightng(ilg)       !<total \f$lightning, flashes/(km^2 . year)\f$ it is assumed that cloud
                           !<to ground lightning is some fixed fraction of total lightning.
real :: litrmass(ilg,iccp1)!<litter mass for each of the 9 pfts
real :: prbfrhuc(ilg)      !<probability of fire due to human causes
real :: extnprob(ilg)      !<fire extinguishing probability
real :: rmatctem(ilg,icc,ignd) !<fraction of roots in each soil layer for each pft
real :: thice(ilg,ignd)   !<frozen soil moisture content over canopy fraction
real :: popdin(ilg)       !<population density \f$(people / km^2)\f$
real :: lucemcom(ilg)     !<land use change (luc) related combustion emission losses, \f$u-mol co2/m2.sec\f$
real :: tmpprob(ilg)      !<
real :: currlat(ilg)      !<
real :: fsnow(ilg)        !<fraction of snow simulated by class

real :: stemltdt(ilg,icc) !<stem litter generated due to disturbance \f$(kg c/m^2)\f$
real :: rootltdt(ilg,icc) !<root litter generated due to disturbance \f$(kg c/m^2)\f$
real :: glfltrdt(ilg,icc) !<green leaf litter generated due to disturbance \f$(kg c/m^2)\f$
real :: burnarea(ilg)     !<total area burned, \f$km^2\f$

!     note the following c burned will be converted to a trace gas 
!     emission or aerosol on the basis of emission factors.
real :: glcaemls(ilg,icc) !<green leaf carbon emission losses, \f$kg c/m^2\f$
real :: rtcaemls(ilg,icc) !<root carbon emission losses, \f$kg c/m^2\f$
real :: stcaemls(ilg,icc) !<stem carbon emission losses, \f$kg c/m^2\f$
real :: ltrcemls(ilg,icc) !<litter carbon emission losses, \f$kg c/m^2\f$
real :: blfltrdt(ilg,icc) !<brown leaf litter generated due to disturbance \f$(kg c/m^2)\f$
real :: blcaemls(ilg,icc) !<brown leaf carbon emission losses, \f$kg c/m^2\f$
real :: burnfrac(ilg)     !<total areal fraction burned, (%)

!     emitted compounds from biomass burning in g of compound
real :: emit_co2(ilg,icc) !<carbon dioxide
real :: emit_co(ilg,icc)  !<carbon monoxide
real :: emit_ch4(ilg,icc) !<methane
real :: emit_nmhc(ilg,icc)!<non-methane hydrocarbons
real :: emit_h2(ilg,icc)  !<hydrogen gas
real :: emit_nox(ilg,icc) !<nitrogen oxides
real :: emit_n2o(ilg,icc) !<nitrous oxide
real :: emit_pm25(ilg,icc)!<particulate matter less than 2.5 um in diameter
real :: emit_tpm(ilg,icc) !<total particulate matter
real :: emit_tc(ilg,icc)  !<total carbon
real :: emit_oc(ilg,icc)  !<organic carbon
real :: emit_bc(ilg,icc)  !<black carbon

real :: biomass(ilg,icc)  !<total biomass for fire purposes
real :: drgtstrs(ilg,icc) !<soil dryness factor for pfts
real :: betadrgt(ilg,ignd)!<dryness term for soil layers
real :: avgdryns(ilg)     !<avg. dryness over the vegetated fraction
real :: fcsum(ilg)        !<total vegetated fraction
real :: avgbmass(ilg)     !<avg. veg. biomass over the veg. fraction of grid cell
real :: c2glgtng(ilg)     !<cloud-to-ground lightning
real :: betalght(ilg)     !<0-1 lightning term
real :: y(ilg)            !<logistic dist. for fire prob. due to lightning
real :: lterm(ilg)        !<lightning fire probability term
real :: temp(ilg)         !<
real :: betmsprd(ilg)     !<beta moisture for calculating fire spread rate
real :: smfunc(ilg)       !<soil moisture function used for fire spread rate
real :: wind(ilg)         !<wind speed in km/hr
real :: wndfunc(ilg)      !<wind function for fire spread rate
real :: sprdrate(ilg)     !<fire spread rate
real :: lbratio(ilg)      !<length to breadth ratio of fire
real :: arbn1day(ilg)     !<area burned in 1 day, \f$km^2\f$
real :: areamult(ilg)     !<multiplier to find area burned
real :: vegarea(ilg)      !<total vegetated area in a grid cell
real :: grclarea(ilg)     !<gcm grid cell area, \f$km^2\f$
real :: tot_emit          !<sum of all pools to be converted to emissions/aerosols \f$(g c/m^2)\f$
real :: tot_emit_dom      !<tot_emit converted to \f$kg dom / m^2\f$
real :: burnvegf(ilg,icc) !<per PFT fraction burned of that PFTs area

real :: natural_ignitions(ilg) !<
real :: anthro_ignitions(ilg)  !<
real :: fire_supp(ilg)         !<
real :: num_ignitions(ilg, icc)!<
real :: num_fires(ilg, icc)    !<

real :: hb_interm    !<interm calculation
real :: hbratio(ilg) !<head to back ratio of ellipse

real, dimension(ilg) :: surface_duff_f  !<fraction of biomass that is in the surface duff (grass brown leaves + litter) 
real, dimension(ilg,icc) :: pftareab    !<areas of different pfts in a grid cell, before fire, \f$km^2\f$
                                        !<pft area before fire \f$(km^2)\f$

real :: ymin, ymax, slope
real :: soilterm  !<temporary variable
real :: duffterm  !<temporary variable
real :: extn_par1 !<parameter used in calculation of fire extinguishing probability

real, dimension(ilg,icc) :: bterm_veg    !<biomass fire probability term, Vivek
real, dimension(ilg,icc) :: duff_frac_veg!<duff fraction for each PFT, Vivek
real, dimension(ilg,icc) :: mterm_veg    !<moisture fire probability term, Vivek
real, dimension(ilg,icc) :: probfire_veg !<PFT fire probability term, Vivek
real, dimension(ilg,icc) :: smfunc_veg   !<soil moisture dependence on fire spread rate, Vivek
real, dimension(ilg,icc) :: sprdrate_veg !<per PFT fire spread rate
real, dimension(ilg,icc) :: arbn1day_veg !<per PFT area burned in 1 day
real, dimension(ilg,icc) :: burnarea_veg !<per PFT area burned over fire duration
logical, dimension(ilg,icc) :: fire_veg  !<fire occuring logical, Vivek

real :: soilterm_veg, duffterm_veg, betmsprd_veg, betmsprd_duff      ! temporary variables

!     ------------------------------------------------------------------
!     Constants and parameters are located in ctem_params.f90
!     -----------------------------------------------------------------

!     * if icc /= 9 or ignd /= 3 this subroutine will need changes.
      IF(ICC.NE.9)      CALL XIT('DISTURB',-1)
      IF(IGND.NE.3)     CALL XIT('DISTURB',-2)

!>initialize required arrays to zero, or assign value

      do 140 j = 1,icc
        do 150 i = il1, il2
          stemltdt(i,j)=0.0
          rootltdt(i,j)=0.0     
          glfltrdt(i,j)=0.0
          blfltrdt(i,j)=0.0
          biomass(i,j)=0.0      
          drgtstrs(i,j)=0.0
          glcaemls(i,j)=0.0     
          blcaemls(i,j)=0.0
          stcaemls(i,j)=0.0
          rtcaemls(i,j)=0.0
          ltrcemls(i,j)=0.0

          emit_co2(i,j) = 0.0
          emit_co(i,j) = 0.0
          emit_ch4(i,j) = 0.0
          emit_nmhc(i,j) = 0.0
          emit_h2(i,j) = 0.0
          emit_nox(i,j) = 0.0
          emit_n2o(i,j) = 0.0
          emit_pm25(i,j) = 0.0
          emit_tpm(i,j) = 0.0
          emit_tc(i,j) = 0.0
          emit_oc(i,j) = 0.0
          emit_bc(i,j) = 0.0
          burnvegf(i,j)=0.0

          bterm_veg(i,j)=0.0
          mterm_veg(i,j)=0.0
          probfire_veg(i,j)=0.0
          fire_veg(i,j)=.false.
          duff_frac_veg(i,j)=0.0
          smfunc_veg(i,j)=0.0  
          sprdrate_veg(i,j)=0.0      
          arbn1day_veg(i,j)=0.0    
          burnarea_veg(i,j)=0.0     
150     continue                  
140   continue

      do 160 k = 1,ignd
        do 170 i = il1, il2
          betadrgt(i,k)=1.0     
170     continue                  
160   continue

      do 180 i = il1, il2
        
        avgbmass(i)=0.0         
        avgdryns(i)=0.0         
        fcsum(i)=0.0            
        c2glgtng(i)=0.0         
        betalght(i)=0.0         
        y(i)=0.0                
        lterm(i)=0.0            
        fire(i)=.false.         
        burnarea(i)=0.0         
        burnfrac(i)=0.0         
        betmsprd(i)=0.0         
        smfunc(i)=0.0           
        wind(i)=0.0             
        wndfunc(i)=0.0          
        sprdrate(i)=0.0         
        lbratio(i)=0.0          
        arbn1day(i)=0.0         
        areamult(i)=0.0         
        vegarea(i)=0.0          
        surface_duff_f(i)=0.0   

180   continue

!>if not simulating fire, leave the subroutine now.
      if (.not. dofire) goto 600


      do 190 i = il1, il2
        if(extnprob(i).le.zero) then
          write(6,*)'fire extinguishing prob. (',i,'= ',extnprob(i)
          write(6,*)'please use an appropriate value of this paramater'
          write(6,*)'else the whole grid cell will burn down leading to'
          write(6,*)'numerical problems.'
          call xit('disturb',-2)
        endif
190   continue

!>initialization ends    

!>Find pft areas before
        do 82 j = 1, icc
          do  83 i = il1, il2
            pftareab(i,j)=fcancmx(i,j)*grclarea(i)  !> area in \f$km^2\f$
83        continue
82      continue

!     ------------------------------------------------------------------

!>Find the probability of fire as a product of three functions
!>with dependence on total biomass, soil moisture, and lightning 

!>1. Dependence on total biomass

      do 200 j = 1, icc
        do 210 i = il1, il2

         if (.not. crop(j)) then !>don't allow it to bring in crops since they are not allowed to burn. 

!>Root biomass is not used to initiate fire. For example if
!>the last fire burned all grass leaves, and some of the roots
!>were left, its unlikely these roots could catch fire. 
           biomass(i,j)=gleafmas(i,j)+bleafmas(i,j)+stemmass(i,j)+ &
                      litrmass(i,j)

!>Find average biomass over the vegetated fraction
           avgbmass(i) = avgbmass(i)+biomass(i,j)*fcancmx(i,j)

!>Sum up the vegetated area
           fcsum(i)=fcsum(i) + fcancmx(i,j)

         endif

210     continue
200   continue

!>calculate bterm for individual PFTs as well

      do 251 j=1, icc
       do 252 i = il1, il2
          bterm_veg(i,j)=min(1.0,max(0.0,(biomass(i,j)-bmasthrs_fire(1))/(bmasthrs_fire(2)-bmasthrs_fire(1))))     
252    continue
251   continue


!>2. Dependence on soil moisture

!>This is calculated in a way such that the more dry the root zone
!>of a pft type is, and more fractional area is covered with that
!>pft, the more likely it is that fire will get started. that is
!>the dryness factor is weighted by fraction of roots in soil
!>layers, as well as according to the fractional coverage of 
!>different pfts. the assumption here is that if there is less 
!>moisture in root zone, then it is more likely the vegetation 
!>will be dry and thus the likeliness of fire is more.
!!
!!First find the dryness factor for each soil layer.
!!
!!If there is snow on the ground, do not allow fire so set betadrgt to
!!0 for all soil layers otherwise calculate as per normal.  
      do i = il1, il2
        if (fsnow(i) .eq. 0.) then
          do j = 1, ignd
           betadrgt(i,j)=min(1.0,max(0.0,(thliq(i,j)+thice(i,j)-wiltsm(i,j))/(fieldsm(i,j)-wiltsm(i,j))))   
          end do        
        end if
      end do

!>Now find weighted value of this dryness factor averaged over the rooting depth, for each pft

      do 320 j = 1, icc
        do 330 i = il1, il2
         if (.not. crop(j)) then
     
          drgtstrs(i,j) =  (betadrgt(i,1))*rmatctem(i,j,1) + (betadrgt(i,2))*rmatctem(i,j,2) + &
                         (betadrgt(i,3))*rmatctem(i,j,3)

          drgtstrs(i,j) = min(1.0,max(0.0,drgtstrs(i,j)/(rmatctem(i,j,1)+rmatctem(i,j,2)+rmatctem(i,j,3))))

!>Next find this dryness factor averaged over the vegetated fraction
!!\f$avgdryns(i) = avgdryns(i) + drgtstrs(i,j)*fcancmx(i,j)\f$
!! 
!! The litter and brown leaves are not affected by the soil water potential
!! therefore they will react only to the moisture conditions (our proxy here
!! is the upper soil moisture). If it is dry they increase the probability of 
!! fire corresponding to the proportion of total C they contribute. Only allow
!! if there is no snow. 
!           if (biomass(i,j) .gt. 0. .and. fsnow(i) .eq. 0.) then
!             ! The surface duff calculation ignores the litter on the bare fraction.
!             surface_duff_f(i) = surface_duff_f(i) + (bleafmas(i,j)+litrmass(i,j)) &
!                                                      /biomass(i,j) * fcancmx(i,j)
! 
!           end if

         endif

330     continue
320   continue

!>Use average root zone vegetation dryness to find likelihood of
!!fire due to moisture. 
!!
!!calculate mterm for each PFT

      do 381 j = 1, icc
        do 382 i = il1, il2
           !> duff fraction for each PFT, Vivek
            if (biomass(i,j) .gt. 0. .and. fsnow(i) .eq. 0.) then
               duff_frac_veg(i,j) = (bleafmas(i,j)+litrmass(i,j)) / biomass(i,j)
            end if

           !> \f$drgtstrs(i,j)\f$ is \f$\phi_{root}\f$ in Melton and Arora GMDD (2015) paper
           soilterm_veg = 1.0-tanh((1.75*drgtstrs(i,j)/extnmois_veg)**2) 
           duffterm_veg = 1.0-tanh((1.75*betadrgt(i,1)/extnmois_duff)**2)

           if(fcancmx(i,j) .gt. zero)then
             mterm_veg(i,j)=soilterm_veg*(1.-duff_frac_veg(i,j)) + duffterm_veg*duff_frac_veg(i,j)
           else
             mterm_veg(i,j)=0.0   !>no fire likelihood due to moisture if no vegetation
           endif

382     continue
381   continue


!>3. dependence on lightning
!!
!!Dependence on lightning is modelled in a simple way which implies that
!!a large no. of lightning flashes are more likely to cause fire than
!!few lightning flashes.

      do 400 i = il1, il2

!>New approximation of Price and Rind equation. It was developed from a more complete dataset than Prentice and Mackerras. 
!!Lightning comes in in units of \f$flashes/km^2/yr\f$ so divide by 12 to make per month.
        !c2glgtng(i)=lightng(i) !FLAG FireMIP lightning is already C2G! If using 'normal' lightning climatology use line below.
        c2glgtng(i)=0.219913*exp(0.0058899*abs(currlat(i)))*lightng(i)

        betalght(i)=min(1.0,max(0.0,(c2glgtng(i)-lwrlthrs)/(hgrlthrs-lwrlthrs)))
        y(i)=1.0/( 1.0+exp((parmlght-betalght(i))/parblght) )

!       No need to calculate each time, once settled on parameters, precalc and moved into a parameter. JM. Feb 19 2014.
        ymin=1.0/( 1.0+exp((parmlght-0.0)/parblght) )
        ymax=1.0/( 1.0+exp((parmlght-1.0)/parblght) )
        slope=abs(0.0-ymin)+abs(1.0-ymax)

        temp(i)=y(i)+(0.0-ymin)+betalght(i)*slope

!>Determine the probability of fire due to human causes
!!this is based upon the population density from the .popd read-in file
        if (popdon) then
            prbfrhuc(i)=min(1.0,(popdin(i)/popdthrshld)**0.43) !From Kloster et al. (2010)
        end if

        ! account for cultural ignitions in Savanna regions, see below for
        ! reduction in suppression too
!        if ( currlat(i).ge.-25.0.and.currlat(i).le.25.0 ) then
        ! i.e. between 25S and 25N, prbfrhuc is always a minimum
        ! of 0.7
!           prbfrhuc(i)=max(0.7, prbfrhuc(i))
!        endif

        lterm(i)=max(0.0, min(1.0, temp(i)+(1.0-temp(i))*prbfrhuc(i) ))

!>----------------------- Number of fire calculations ----------------------\\
!>
!>This is not used in CTEM in general but we keep the code here for testing purposes
!!
!!calculate natural and anthorpogenic ignitions/km2.day
!!the constant 0.25 assumes not all c2g lightning hits causes ignitions, only 0.25 of them do
!!the constant (1/30.4) converts c2g lightning from flashes/km2.month to flashes/km2.day
!!MAKE SURE LIGHTNING IS IN UNITS OF FLASHES/KM2.MONTH
!!
!!Eqs. (4) and (5) of Li et al. 2012 doi:10.5194/bg-9-2761-2012 + also see corrigendum

        !natural_ignitions(i)=c2glgtng(i) * 0.25 * (1/30.4)
        !anthro_ignitions(i)=9.72E-4 * (1/30.4) * popdin(i) * (6.8 * (popdin(i)**(-0.6)) )

!>calculate fire suppression also as a function of population density.
!!Li et al. (2012) formulation is quite similar to what we already use based
!!on Kloster et al. (2010, I think) but we use Kloster's formulation together
!!with our fire extingishing probability. Here, the fire suppression formulation
!!is just by itself

        !fire_supp(i)=0.99 - ( 0.98*exp(-0.025*popdin(i)*(i)) )

!>----------------------- Number of fire calculations ----------------------//
400   continue

!>calculate fire probability for each PFT. Recall that lightning term is still grid averaged

      do 421 j = 1, icc
        do 422 i = il1, il2
           probfire_veg(i,j)=bterm_veg(i,j)*mterm_veg(i,j)*lterm(i)
           if (probfire_veg(i,j) .gt. zero) fire_veg(i,j)=.true.

!>----------------------- Number of fire calculations ----------------------\\
!!
!!This is not used in CTEM in general but we keep the code here for testing purposes
!!
!!calculate total number of ignitions based natural and anthorpogenic ignitions
!!for the whole grid cell
    
           !num_ignitions(i,j) = ( natural_ignitions(i) + anthro_ignitions(i) ) * fcancmx(i,j)*grclarea(i)

!>finally calculate number of fire, noting that not all ignitions turn into fire
!!because moisture and biomass may now allow that to happen, and some of those
!!will be suppressed due to fire fighting efforts

           !num_fires(i,j) = num_ignitions(i,j)*(1-fire_supp(i))*bterm_veg(i,j)*mterm_veg(i,j)
!>
!>----------------------- Number of fire calculations ----------------------//

422     continue
421   continue

!>Calculate area burned for each PFT, make sure it's not greater than the
!!area available, then find area and fraction burned for the whole gridcell

      do 500 j = 1, icc
        do 501 i = il1, il2
          if (fire_veg(i,j) ) then

!>soil moisture dependence on fire spread rate
          betmsprd_veg = (1. - min(1., (drgtstrs(i,j)/extnmois_veg) ))**2
          betmsprd_duff = (1. - min(1., (betadrgt(i,1)/extnmois_duff) ))**2
          smfunc_veg(i,j)= betmsprd_veg*(1-duff_frac_veg(i,j)) + betmsprd_duff*duff_frac_veg(i,j)

!>wind speed, which is gridcell specific
          wind(i)=sqrt(uwind(i)**2.0 + vwind(i)**2.0)
          wind(i)=wind(i)*3.60     ! change m/s to km/hr

!>Length to breadth ratio from Li et al. (2012)
          lbratio(i)=1.0+10.0*(1.0-exp(-0.06*wind(i)))

!>head to back ratio from Li et al. (2012). 
          hb_interm = (lbratio(i)**2 - 1.0)**0.5
          hbratio(i) = (lbratio(i) + hb_interm)/(lbratio(i) - hb_interm)

!>dependence of spread rate on wind
          wndfunc(i)= (2.0 * lbratio(i)) / (1.0 + 1.0 / hbratio(i)) * f0 

!>fire spread rate per PFT
          n = sort(j)  
          sprdrate_veg(i,j)= maxsprd(n) * smfunc_veg(i,j) * wndfunc(i) 

!>area burned in 1 day for that PFT
          arbn1day_veg(i,j)=(pi*24.0*24.0*sprdrate_veg(i,j)**2)/(4.0 * lbratio(i))*(1.0 + 1.0 / hbratio(i))**2

!>fire extinguishing probability as a function of grid-cell averaged population density
          if (popdon) then

            !> account for low suppression in Savanna regions, see above for
            !> increase in ignition due to cultural practices
               extn_par1=-0.015 ! Value based on Vivek's testing. Jul 14 2016. old = -0.025

            ! change 0.9 to 1.0 in eqn. A78 of Melton and Arora, 2016, GMD competition paper
            extnprob(i)=max(0.0,1.0-exp(extn_par1*popdin(i)))

            extnprob(i)=0.5+extnprob(i)/2.0
          end if
          
!>area multipler to calculate area burned over the duration of the fire
          areamult(i)=((1.0-extnprob(i))*(2.0-extnprob(i)))/ extnprob(i)**2                              

!>per PFT area burned, \f$km^2\f$
          burnarea_veg(i,j)=arbn1day_veg(i,j)*areamult(i)*(grclarea(i)*fcancmx(i,j)*probfire_veg(i,j))/reparea


!          ------- Area burned based on number of fire calculations ----------------------\\

!       This is not used in CTEM in general but we keep the code here for testing purposes

!         the constant 4 is suppose to address the fact that Li et al. (2012) suggested to
!         double the fire spread rates. However, if we do that than our usual calculations
!         based on CTEM's original parameterization will be affected. Rather than do that
!         we just use a multiplier of 4, since doubling fire spread rates means 4 times the
!         area burned
!
!          burnarea_veg(i,j)=arbn1day_veg(i,j)*num_fires(i,j)*2.0  !flag test was 4!
 !
!          ------- Area burned based on number of fire calculations ----------------------//

!         if area burned greater than area of PFT, set it to area of PFT

          if ( burnarea_veg(i,j) .gt. grclarea(i)*fcancmx(i,j) ) then
             burnarea_veg(i,j)=grclarea(i)*fcancmx(i,j)
          endif

         endif
501     continue
500   continue

!>Calculate gridcell area burned and fraction

      do 510 j = 1, icc
        do 511 i = il1, il2
          burnarea(i)=burnarea(i)+burnarea_veg(i,j)
511     continue       
510   continue

      do 512 i = il1, il2
        burnfrac(i)=burnarea(i)/grclarea(i)
512   continue       

!>Finally estimate amount of litter generated from each pft, and
!>each vegetation component (leaves, stem, and root) based on their
!>resistance to combustion. Update the veg pools due to combustion.

      do 520 j = 1, icc
       n = sort(j)
        do 530 i = il1, il2
          if(pftareab(i,j) .gt. zero)then

            !>Set aside these pre-disturbance stem and root masses for use
            !>in burntobare subroutine.
            pstemmass(i,j)=stemmass(i,j)
            pgleafmass(i,j)=gleafmas(i,j)

            glfltrdt(i,j)= frltrglf(n) *gleafmas(i,j) *(burnarea_veg(i,j) /pftareab(i,j))
            blfltrdt(i,j)= frltrblf(n) *bleafmas(i,j) *(burnarea_veg(i,j) /pftareab(i,j))
            stemltdt(i,j)= frltrstm(n) *stemmass(i,j) *(burnarea_veg(i,j) /pftareab(i,j))
            rootltdt(i,j)= frltrrt(n)  *rootmass(i,j) *(burnarea_veg(i,j) /pftareab(i,j))
            glcaemls(i,j)= frco2glf(n) *gleafmas(i,j) *(burnarea_veg(i,j) /pftareab(i,j))
            blcaemls(i,j)= frco2blf(n) *bleafmas(i,j) *(burnarea_veg(i,j) /pftareab(i,j))
            stcaemls(i,j)= frco2stm(n) *stemmass(i,j) *(burnarea_veg(i,j) /pftareab(i,j))
            rtcaemls(i,j)= frco2rt(n)  *rootmass(i,j) *(burnarea_veg(i,j) /pftareab(i,j))
            ltrcemls(i,j)= frltrbrn(n) *litrmass(i,j) *(burnarea_veg(i,j) /pftareab(i,j))

!>Update the pools:
            gleafmas(i,j)=gleafmas(i,j) - glfltrdt(i,j) - glcaemls(i,j)
            bleafmas(i,j)=bleafmas(i,j) - blfltrdt(i,j) - blcaemls(i,j)
            stemmass(i,j)=stemmass(i,j) - stemltdt(i,j) - stcaemls(i,j)
            rootmass(i,j)=rootmass(i,j) - rootltdt(i,j) - rtcaemls(i,j)
            litrmass(i,j)=litrmass(i,j) + glfltrdt(i,j) + blfltrdt(i,j) + stemltdt(i,j) + rootltdt(i,j) - ltrcemls(i,j)

!>Output the burned area per PFT (the units here are burned fraction of each PFTs area. So
!>if a PFT has 50% gridcell cover and 50% of that burns it will have a burnvegf of 0.5 (which
!>then translates into a gridcell fraction of 0.25). This units is for consistency outside of this subroutine.
            burnvegf(i,j)=burnarea_veg(i,j) /  pftareab(i,j)

          endif
530     continue
520   continue


600   continue !> If .not. dofire then we enter here and perform the calculations for the emissions
               !> since we might have some from luc. 
!>
!>We also estimate \f$CO_2\f$ emissions from each
!>of these components. Note that the litter which is generated due 
!!to disturbance is uniformly distributed over the entire area of 
!!a given pft, and this essentially thins the vegetation biomass. 
!!If compete is not on, this does not change the vegetation fractions,
!!if competition is on a fraction will become bare. That is handled in
!!burntobare subroutine called from competition subroutine.
!!
      do 620 j = 1, icc
       n = sort(j)
        do 630 i = il1, il2

!>Calculate the emissions of trace gases and aerosols based upon how much plant matter was burnt

!>Sum all pools that will be converted to emissions/aerosols \f$(g c/m^2)\f$
           tot_emit = (glcaemls(i,j) + blcaemls(i,j) + rtcaemls(i,j)+ stcaemls(i,j) + ltrcemls(i,j)) * 1000.0

!>Add in the emissions due to luc fires (deforestation)
!>the luc emissions are converted from \f$umol co_2 m-2 s-1 to g c m-2\f$ (day-1) before adding to tot_emit         
           tot_emit = tot_emit + (lucemcom(i) / 963.62 * 1000.0)

!>Convert burnt plant matter from carbon to dry organic matter using 
!>a conversion factor, assume all parts of the plant has the same
!>ratio of carbon to dry organic matter. units: \f$kg dom / m^2\f$
           tot_emit_dom = tot_emit / c2dom

!>Convert the dom to emissions/aerosols using emissions factors units: \f$g species / m^2\f$

           emit_co2(i,j)  = emif_co2(n) * tot_emit_dom
           emit_co(i,j)   = emif_co(n)  * tot_emit_dom
           emit_ch4(i,j)  = emif_ch4(n) * tot_emit_dom
           emit_nmhc(i,j) = emif_nmhc(n) * tot_emit_dom
           emit_h2(i,j)   = emif_h2(n) * tot_emit_dom
           emit_nox(i,j)  = emif_nox(n) * tot_emit_dom
           emit_n2o(i,j)  = emif_n2o(n) * tot_emit_dom
           emit_pm25(i,j) = emif_pm25(n) * tot_emit_dom
           emit_tpm(i,j)  = emif_tpm(n) * tot_emit_dom
           emit_tc(i,j)   = emif_tc(n) * tot_emit_dom
           emit_oc(i,j)   = emif_oc(n) * tot_emit_dom
           emit_bc(i,j)   = emif_bc(n) * tot_emit_dom

630     continue
620   continue
!>
!>FLAG for the optimization of popd effect on fire I am taking the lterm out as the 'temp' var. So I have made tmp be dimension
!>ilg and I overwrite the lterm (which is written to an output file) here in its place.

lterm = temp

end subroutine disturb
!>@}
! ------------------------------------------------------------------------------------

!>\ingroup disturbance_scheme_burntobare
!!@{
subroutine burntobare(il1, il2, nilg, sort,pvgbioms,pgavltms,pgavscms,fcancmx, burnvegf, stemmass, &
                      rootmass, gleafmas, bleafmas, litrmass, soilcmas, pstemmass, pgleafmass,&
                      nppveg)

!     J. Melton. Mar 26 2014  - Create subroutine


use ctem_params, only : crop, icc, seed, standreplace, grass, zero, &
                        iccp1, tolrance, numcrops

implicit none

integer, intent(in) :: il1
integer, intent(in) :: il2
integer, intent(in) :: nilg       !< no. of grid cells in latitude circle (this is passed in as either ilg or nlat depending on mos/comp)
integer, dimension(icc), intent(in) :: sort             !< index for correspondence between 9 ctem pfts and
                                                        !< size 12 of parameter vectors
real, dimension(nilg), intent(in) :: pvgbioms           !< initial veg biomass
real, dimension(nilg), intent(in) :: pgavltms           !< initial litter mass
real, dimension(nilg), intent(in) :: pgavscms           !< initial soil c mass
real, dimension(nilg,icc), intent(inout) :: fcancmx     !< initial fractions of the ctem pfts
real, dimension(nilg,icc), intent(in) :: burnvegf       !< per PFT fraction burned of that PFTs area
real, dimension(nilg,icc), intent(inout) :: gleafmas    !< green leaf carbon mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real, dimension(nilg,icc), intent(inout) :: bleafmas    !< brown leaf carbon mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real, dimension(nilg,icc), intent(inout) :: stemmass    !< stem carbon mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real, dimension(nilg,icc), intent(inout) :: rootmass    !< roots carbon mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
real, dimension(nilg,icc), intent(inout) :: nppveg      !< npp for individual pfts,  \f$u-mol co_2/m^2.sec\f$
real, dimension(nilg,iccp1), intent(inout) :: soilcmas  !< soil carbon mass for each of the 9 ctem pfts + bare, \f$kg c/m^2\f$
real, dimension(nilg,iccp1), intent(inout) :: litrmass  !< litter carbon mass for each of the 9 ctem pfts + bare, \f$kg c/m^2\f$
real, dimension(nilg,icc), intent(in)    :: pstemmass   !< grid averaged stemmass prior to disturbance, \f$kg c/m^2\f$
real, dimension(nilg,icc), intent(in)    :: pgleafmass  !< grid averaged rootmass prior to disturbance, \f$kg c/m^2\f$

logical, dimension(nilg) :: shifts_occur      !< true if any fractions changed
integer :: i, j, n, k
real :: pftfraca_old
real :: term                                  !< temp variable for change in fraction due to fire
real, dimension(nilg) :: pbarefra             !< bare fraction prior to fire              
real, dimension(nilg) :: barefrac             !< bare fraction of grid cell
real, dimension(nilg) :: litr_lost            !< litter that is transferred to bare 
real, dimension(nilg) :: soilc_lost           !< soilc that is transferred to bare
real, dimension(nilg) :: vgbiomas_temp        !< grid averaged vegetation biomass for internal checks, \f$kg c/m^2\f$
real, dimension(nilg) :: gavgltms_temp        !< grid averaged litter mass for internal checks, \f$kg c/m^2\f$
real, dimension(nilg) :: gavgscms_temp        !< grid averaged soil c mass for internal checks, \f$kg c/m^2\f$
real, dimension(nilg,icc) :: pftfracb         !< pft fractions before accounting for creation of bare ground
real, dimension(nilg,icc) :: pftfraca         !< pft fractions after accounting for creation of bare ground
real :: frac_chang                            !< pftfracb - pftfraca

! -----------------------------------------

!> Do some initializations
do 10 i = il1, il2
        shifts_occur(i) = .false.
        pbarefra(i)=1.0
        barefrac(i)=1.0
        vgbiomas_temp(i)=0.0
        gavgltms_temp(i)=0.0
        gavgscms_temp(i)=0.0
        litr_lost(i)=0.0
        soilc_lost(i)=0.0
10  continue
!>
!>  Account for disturbance creation of bare ground. This occurs with relatively low
!>  frequency and is PFT dependent. We must adjust the amount of bare ground created
!>  to ensure that we do not increase the density of the remaining vegetation. 

       do 20 i = il1, il2
          do 25 j = 1, icc
            if(.not. crop(j))then  

              n = sort(j)

              pbarefra(i)=pbarefra(i)-fcancmx(i,j)
              pftfracb(i,j)=fcancmx(i,j)

              pftfraca(i,j) = max(seed,fcancmx(i,j) - burnvegf(i,j) * standreplace(n))

              fcancmx(i,j) = pftfraca(i,j)

              barefrac(i)=barefrac(i)-fcancmx(i,j)

            else  !crops

              pbarefra(i)=pbarefra(i)-fcancmx(i,j)
              barefrac(i)=barefrac(i)-fcancmx(i,j)

            endif

25      continue
20   continue

      do 40 i = il1, il2
       do 50 j = 1, icc
        if(.not. crop(j))then 
 
          !> Test the pftfraca to ensure it does not cause densification of the exisiting biomass  
          !> Trees compare the stemmass while grass compares the root mass. 
          if (pftfraca(i,j) .ne. pftfracb(i,j)) then

            shifts_occur(i) = .true.

            term = pftfracb(i,j)/pftfraca(i,j)

            if (.not. grass(j)) then
               if (stemmass(i,j)*term .gt. pstemmass(i,j) .and. pstemmass(i,j) .gt. 0.) then  !>the pstemmass is from before the fire occurred, i.e. no thinning!

                 pftfraca_old = pftfraca(i,j)
                 pftfraca(i,j) = max(seed,stemmass(i,j) * pftfracb(i,j) / pstemmass(i,j))
                 fcancmx(i,j) = pftfraca(i,j)

                 !> adjust the bare frac to accomodate for the changes 
                 barefrac(i) = barefrac(i) + pftfraca_old - pftfraca(i,j)

               end if
            else !grasses

               if (gleafmas(i,j)*term .gt. pgleafmass(i,j) .and. pgleafmass(i,j) .gt. 0.) then !>the pgleafmass is from before the fire occurred, i.e. no thinning!

                 pftfraca_old = pftfraca(i,j)
                 pftfraca(i,j) = max(seed,gleafmas(i,j) * pftfracb(i,j) / pgleafmass(i,j))
                 fcancmx(i,j) = pftfraca(i,j)

                 !> adjust the bare frac to accomodate for the changes 
                 barefrac(i) = barefrac(i) + pftfraca_old - pftfraca(i,j)

               end if
            end if

            term = pftfracb(i,j)/pftfraca(i,j)
            gleafmas(i,j)=gleafmas(i,j)*term
            bleafmas(i,j)=bleafmas(i,j)*term
            stemmass(i,j)=stemmass(i,j)*term
            rootmass(i,j)=rootmass(i,j)*term
            nppveg(i,j)  =nppveg(i,j)*term
!>
!>Soil and litter carbon are treated such that we actually transfer the carbon
!>to the bare fraction since it would remain in place as a location was devegetated
!>In doing so we do not adjust the litter or soilc density on the 
!>remaining vegetated fraction. But we do adjust it on the bare fraction to ensure
!>our carbon balance works out.
            frac_chang = pftfracb(i,j) - pftfraca(i,j)
            litr_lost(i)= litr_lost(i) + litrmass(i,j) * frac_chang
            soilc_lost(i)= soilc_lost(i) + soilcmas(i,j) * frac_chang

        ! else  

        !    no changes

          end if

        endif  !crop
50     continue
40  continue

      do 100 i = il1, il2
       if (shifts_occur(i)) then !>only do checks if we actually shifted fractions here. 

        if(barefrac(i).ge.zero .and. barefrac(i) .gt. pbarefra(i))then
          litrmass(i,iccp1) = (litrmass(i,iccp1)*pbarefra(i) + litr_lost(i)) / barefrac(i) 
          soilcmas(i,iccp1) = (soilcmas(i,iccp1)*pbarefra(i) + soilc_lost(i)) / barefrac(i) 
        else if (barefrac(i) .lt. 0.) then  
          
          write(6,*)' In burntobare you have negative bare area, which should be impossible...'
          write(6,*)' bare is',barefrac(i),' original was',pbarefra(i)
          call xit('disturb-burntobare',-6)

        endif
       end if
100  continue

!>check if total grid average biomass density is same before and after adjusting fractions

    do 200 i = il1, il2

      if (shifts_occur(i)) then !>only do checks if we actually shifted fractions here. 

       do 250 j = 1, icc

          vgbiomas_temp(i)=vgbiomas_temp(i)+fcancmx(i,j)*(gleafmas(i,j)+&
          bleafmas(i,j)+stemmass(i,j)+rootmass(i,j))
          gavgltms_temp(i)=gavgltms_temp(i)+fcancmx(i,j)*litrmass(i,j)
          gavgscms_temp(i)=gavgscms_temp(i)+fcancmx(i,j)*soilcmas(i,j)

250    continue

        !>then add the bare ground in.
        gavgltms_temp(i)=gavgltms_temp(i)+ barefrac(i)*litrmass(i,iccp1)
        gavgscms_temp(i)=gavgscms_temp(i)+ barefrac(i)*soilcmas(i,iccp1)

        if(abs(vgbiomas_temp(i)-pvgbioms(i)).gt.tolrance)then
          write(6,*)'grid averaged biomass densities do not balance'
          write(6,*)'after fractional coverages are changed to take'
          write(6,*)'into account burn area'
          write(6,*)'vgbiomas_temp(',i,')=',vgbiomas_temp(i)
          write(6,*)'pvgbioms(',i,')=',pvgbioms(i)
          call xit('disturb',-7)
        endif
        if(abs(gavgltms_temp(i)-pgavltms(i)).gt.tolrance)then
          write(6,*)'grid averaged litter densities do not balance'
          write(6,*)'after fractional coverages are changed to take'
          write(6,*)'into account burn area'
          write(6,*)'gavgltms_temp(',i,')=',gavgltms_temp(i)
          write(6,*)'pgavltms(',i,')=',pgavltms(i)
          call xit('disturb',-8)
        endif
        if(abs(gavgscms_temp(i)-pgavscms(i)).gt.tolrance)then
          write(6,*)'grid averaged soilc densities do not balance'
          write(6,*)'after fractional coverages are changed to take'
          write(6,*)'into account burn area'
          write(6,*)'gavgscms_temp(',i,')=',gavgscms_temp(i)
          write(6,*)'pgavscms(',i,')=',pgavscms(i)
          call xit('disturb',-9)
        endif

      end if

200  continue

      return

end subroutine burntobare
!!@}
end module

