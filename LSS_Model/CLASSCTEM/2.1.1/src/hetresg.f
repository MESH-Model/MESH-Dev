!>\file
!!Canadian Terrestrial Ecosystem Model (CTEM) 
!!Heterotrophic Respiration Subroutine For Bare Fraction
!!
!!
!!Heterotrophic respiration, \f$R_\mathrm{h}\f$ (\f$mol\,CO_2\,m^{-2}\,s^{-1}\f$), in CTEM is based on respiration from the litter (which includes contributions from the stem, leaf and root components), \f$R_{h,D}\f$, and soil carbon, \f$R_{h,H}\f$, pools,
!!\f[ \label{hetres_all} R_\mathrm{h}=R_{h,D}+R_{h,H}. \f]
!!
!!Heterotrophic respiration is regulated by soil temperature and moisture and is calculated on a daily time step. The original heterotrophic respiration scheme is described in \cite Arora2003-3b7 while the modified parametrization used in CTEM v. 2.0 is detailed in \cite Melton2014-xy and is briefly described here. Respiration from the litter and soil carbon pools takes the following basic form
!!\f[ R_{\mathrm{h},i} = 2.64 \times 10^{-6}\,\varsigma_i C_i f_{15}(Q_{10}) f(\Psi)_i,\nonumber \\ i = \mathrm{D}, \mathrm{H}.\label{lithet} \f]
!!
!!The soil carbon and litter respiration depends on the amount of carbon in these components (\f$C_\mathrm{H}\f$ and \f$C_\mathrm{D}\f$; \f$kg\,C\,m^{-2}\f$) and on a PFT-dependent respiration rate specified at \f$15\,{C}\f$ (\f$\varsigma_\mathrm{H}\f$ and \f$\varsigma_\mathrm{D}\f$; \f$kg\,C\,(kg\,C)^{-1}\,yr^{-1}\f$; see also ctem_params.f90). The constant \f$2.64 \times 10^{-6}\f$ converts units from \f$kg\,C\,m^{-2}\,yr^{-1}\f$ to \f$mol\,CO_2\,m^{-2}\,s^{-1}\f$.
!!
!!The effect of soil moisture is accounted for via dependence on soil matric potential (\f$f(\Psi)\f$), described later. The temperature dependency of microbial soil respiration rates has been estimated by several different formulations, ranging from simple \f$Q_{10}\f$ (exponential) to Arrhenius-type formulations (see review by \cite Lloyd1994-ct). In CTEM, soil temperature influences heterotrophic respiration through a temperature-dependent \f$Q_{10}\f$ function (\f$f_{15}(Q_{10})\f$). The value of \f$Q_{10}\f$ itself is assumed to be a function of temperature following a hyperbolic tan function:
!!\f[ Q_{10} = 1.44 + 0.56\,\tanh[0.075 (46.0 - T_i)], \nonumber\\ i = \mathrm{D}, \mathrm{H}\label{hyper}, \f]
!!where \f$T_{\{D,H\}}\f$ is the temperature of either the litter or soil carbon pool (\f$C\f$), respectively. The parametrization is a compromise between the temperature-independent \f$Q_{10}\f$ commonly found in many terrestrial ecosystem models \cite Cox2001-am and the temperature-dependent \f$Q_{10}\f$ of \cite Kirschbaum1995-db. While a constant \f$Q_{10}\f$ yields an indefinitely increasing respiration rate with increasing temperature, the formulation of \cite Kirschbaum1995-db gives a continuously increasing \f$Q_{10}\f$ under decreasing temperature, which leads to unreasonably high soil and litter carbon pools at high latitudes in CTEM. The CTEM parametrization avoids these issues with a $Q_{10}$ value of about 2.0 for temperatures less than \f$20\,C\f$, while a decreasing value of \f$Q_{10}\f$ at temperatures above \f$20\,C\f$ ensures that the respiration rate does not increase indefinitely.
!!
!!The temperature of the litter pool is a weighted average of the temperature of the top soil layer (\f$T_1\f$) and the root temperature (\f$T_\mathrm{R}\f$) as litter consists of leaf, stem, and root litter (\f$T_\mathrm{D} = 0.7 T_1 + 0.3T_\mathrm{R}\f$). The temperature of the soil carbon pool is calculated as the mean soil temperature in the rooting zone based upon the fraction of roots in each soil layer and their temperature. The carbon in each soil layer is not explicitly tracked but assumed to adopt an exponential distribution \cite Jobbagy2000-pa.
!!
!!The response of heterotrophic respiration to soil moisture is formulated through soil matric potential (\f$\Psi\f$; \f$MPa\f$). While soil matric potential values are usually negative, the formulation uses absolute values to allow its logarithm to be taken. Absolute values of soil matric potential are high when soil is dry and low when it is wet. The primary premise of soil moisture control on heterotrophic respiration is that heterotrophic respiration is constrained both when the soils are dry (due to reduced microbial activity) and when they are wet (due to impeded oxygen supply to microbes) with optimum conditions in-between. The exception is the respiration from the litter component, which is assumed to be continually exposed to air, and thus never oxygen deprived, even when soil moisture content is high (\f$0.04 > \vert \Psi \vert \geq \vert \Psi_{sat} \vert\f$, where \f$\Psi_{sat}\f$ is the soil matric potential at saturation). The soil moisture dependence thus varies between 0 and 1 with matric potential as follows:
!!
!!for \f$0.04 > \vert\Psi\vert \geq \vert\Psi_{sat}\vert\f$
!!\f[ f(\Psi)_\mathrm{H} = 1 - 0.5  \frac{\log(0.04)-\log\vert\Psi\vert}{\log(0.04)-\log\vert\Psi_{sat}\vert}\\ f(\Psi)_D = 1\nonumber; \f]
!!for \f$0.06 \geq \vert\Psi\vert \geq 0.04\f$
!!\f[ f(\Psi)_\{D,H\} = 1; \f]
!!for \f$100.0 \geq \vert\Psi\vert > 0.06\f$
!!\f[ f(\Psi)_\{D,H\} = 1 - 0.8\frac{\log\vert\Psi\vert-\log(0.06)}{\log(100)-\log(0.06)}; \f]
!!for \f$\vert\Psi\vert > 100.0\f$
!!\f[ \label{lastpsi} f(\Psi)_\{D,H\}=0.2. \f]
!!
!!Heterotrophic respiration for bare ground is treated separately in CTEM. The carbon contributions to the bare ground litter and soil carbon pools come via processes such as creation of bare ground due to fire, competition between PFTs and land use change. The heterotrophic respiration is sensitive to temperature and moisture in the same manner as vegetated areas using Eqs. (\ref{lithet})--(\ref{lastpsi}). The base respiration rates of \f$\varsigma_{D,bare}\f$ and \f$\varsigma_{H,bare}\f$ are set to \f$0.5605\f$ and \f$0.02258\,kg\,C\,(kg\,C)^{-1}\,yr^{-1}\f$, respectively.
!!
!!The amount of humidified litter, which is transferred from the litter to the soil carbon pool (\f$C_{\mathrm{D} \rightarrow \mathrm{H}}\f$) is modelled as a fraction of litter respiration (\f$R_{h,D}\f$) as
!!\f[ \label{cdtoh} C_{\mathrm{D} \rightarrow \mathrm{H}} = \chi\,R_{h,D} \f]
!!where \f$\chi\f$ (see also ctem_params.f90) is the PFT-dependent humification factor and varies between 0.4 and 0.5. For crops, \f$\chi\f$ is set to 0.1 to account for reduced transfer of humidified litter to the soil carbon pool which leads to loss in soil carbon when natural vegetation is converted to croplands. Over the bare ground fraction \f$\chi\f$ is set to 0.45.
!!
!!With heterotrophic respiration known, net ecosystem productivity (\f$NEP\f$) is calculated as
!!\f[ NEP = G_{canopy} - R_\mathrm{m} - R_\mathrm{g} - R_\mathrm{h}. \f]
!!
      subroutine hetresg (litrmass, soilcmas,         
     1                         il1,      il2,     tbar,    
     2                       thliq,     sand,      clay,   zbotw,   
     3                        frac,    isnow,      isand,
c    -------------- inputs above this line, outputs below -------------
     4                      litres,   socres, thiceg)
c
c     11  Apr. 2003 - this subroutine calculates heterotrophic respiration
c     V. Arora        over the bare subarea of a grid cell (i.e. ground only
c                     and snow over ground subareas).
c
c     change history:
c
c     17  Jan 2014  - Moved parameters to global file (ctem_params.f90)
c     J. Melton
c
c     23  Jul 2013  - add in module for parameters
c     J. Melton
c     J. Melton and V.Arora - changed tanhq10 parameters, they were switched
c               25 Sep 2012
c     J. Melton 31 Aug 2012 - remove isnow, it is not used.
c     J. Melton 23 Aug 2012 - bring in isand, converting sand to
c                             int was missing some gridcells assigned
c                             to bedrock in classb

c     ------
c     inputs 
c
c     icc       - no. of vegetation types (currently 8)
c     ignd      - no. of soil layers (currently 3)
c     ilg       - no. of grid cells in latitude circle
c

      use ctem_params,        only : icc, ilg, ignd, zero, tanhq10, a,
     1                               bsratelt_g, bsratesc_g

      implicit none
c
c     
c
      integer il1   !<il1=1
      integer il2   !<il2=ilg
      integer i,j,k
      integer isnow !<integer telling if bare fraction is fg (0) or fgs (1), isnow 
                    !<is changed to isnow(ilg) in classt of class version higher than 3.4 for coupling with ctem
      integer isand(ilg,ignd) !<

      real litrmass(ilg,icc+1)!<litter mass for the 8 pfts + bare in \f$kg c/m^2\f$
      real soilcmas(ilg,icc+1)!<soil carbon mass for the 8 pfts + bare in \f$kg c/m^2\f$
      real tbar(ilg,ignd)     !<soil temperature, k
      real thliq(ilg,ignd)    !<liquid soil moisture content in 3 soil layers
      real sand(ilg,ignd)     !<percentage sand
      real zbotw(ilg,ignd)    !<bottom of soil layers
      real litres(ilg)        !<litter respiration over the given unvegetated sub-area in umol co2/m2.s
      real socres(ilg)        !<soil c respiration over the given unvegetated sub-area in umol co2/m2.s
      real clay(ilg,ignd)     !<percentage clay
      real frac(ilg)          !<fraction of ground (fg) or snow over ground (fgs)
      real thiceg(ilg,ignd)   !<liquid soil moisture content in 3 soil layers for bare ground
      real litrq10          !<
      real soilcq10         !<
      real litrtemp(ilg)    !<litter temperature
      real solctemp(ilg)    !<soil carbon pool temperature
      real q10func          !<
      real psisat(ilg,ignd) !<saturation matric potential
      real grksat(ilg,ignd) !<saturation hyd. conductivity
      real b(ilg,ignd)      !<parameter b of clapp and hornberger
      real thpor(ilg,ignd)  !<porosity
      real beta             !<
      real fracarb(ilg,ignd)!<fraction of carbon in each soil layer
      real zcarbon          !<
      real tempq10l(ilg)    !<
      real socmoscl(ilg)    !<soil moisture scalar for soil carbon decomposition
      real scmotrm(ilg,ignd)!<
      real ltrmoscl(ilg)    !<soil moisture scalar for litter decomposition
      real psi(ilg,ignd)    !<
      real tempq10s(ilg)    !<
      real fcoeff           !<

      real thporg(3)          !<porosity for peat soils
      real psisorg(3)         !<saturation matric potential for peat soils
      real borg(3)            !<parameter b of clapp and hornberger for peat soils

      COMMON /CLASS5/ THPORG,BORG,PSISORG

!>
!>------------------------------------------------------------------
!!Constants and parameters are located in ctem_params.f90
!!---------------------------------------------------------------
!!
!!initialize required arrays to zero
!!
      do 100 k = 1, ignd
        do 100 i = il1, il2
          fracarb(i,k)=0.0  
100   continue
c
      do 110 i = il1, il2
        litrtemp(i)=0.0     
        solctemp(i)=0.0      
        socmoscl(i)=0.0      
        ltrmoscl(i)=0.0      
        litres(i)=0.0       
        tempq10l(i)=0.0     
        socres(i)=0.0       
        tempq10s(i)=0.0    
110   continue
c
      do 120 j = 1, ignd
        do 130 i = il1, il2
          psisat(i,j) = 0.0        
          grksat(i,j) = 0.0        
          thpor(i,j) = 0.0         
          b(i,j) = 0.0             
130     continue
120   continue
!>
!!initialization ends    
!!
c     ------------------------------------------------------------------
!>
!!estimate temperature of the litter and soil carbon pools. 
!!
!!over the bare fraction there is no live root. so we make the
!!simplest assumption that litter temperature is same as temperature of the top soil layer.
!!    
      do 210 i = il1, il2
        litrtemp(i)=tbar(i,1)
210   continue
!>
!!we estimate the temperature of the soil c pool assuming that soil carbon over the bare fraction is distributed exponentially. note
!!that bare fraction may contain dead roots from different pfts all of which may be distributed differently. for simplicity we do not
!!track each pft's dead root biomass and assume that distribution of soil carbon over the bare fraction can be described by a single parameter.
!!
      do 240 i = il1, il2
c
        zcarbon=3.0/a                 ! 95% depth
        if(zcarbon.le.zbotw(i,1)) then
            fracarb(i,1)=1.0             ! fraction of carbon in
            fracarb(i,2)=0.0             ! soil layers
            fracarb(i,3)=0.0
        else
            fcoeff=exp(-a*zcarbon)
            fracarb(i,1)=
     &        1.0-(exp(-a*zbotw(i,1))-fcoeff)/(1.0-fcoeff)
            if(zcarbon.le.zbotw(i,2)) then
                fracarb(i,2)=1.0-fracarb(i,1)
                fracarb(i,3)=0.0
            else
                fracarb(i,3)=
     &            (exp(-a*zbotw(i,2))-fcoeff)/(1.0-fcoeff)
                fracarb(i,2)=1.0-fracarb(i,1)-fracarb(i,3)
            endif
        endif
c
        solctemp(i)=tbar(i,1)*fracarb(i,1) +
     &     tbar(i,2)*fracarb(i,2) +
     &     tbar(i,3)*fracarb(i,3)
        solctemp(i)=solctemp(i) /
     &     (fracarb(i,1)+fracarb(i,2)+fracarb(i,3))
!>
!!make sure we don't use temperatures of 2nd and 3rd soil layers if they are specified bedrock via sand -3 flag
!!
        if(isand(i,3).eq.-3)then 
!>third layer bed rock
          solctemp(i)=tbar(i,1)*fracarb(i,1) +
     &       tbar(i,2)*fracarb(i,2) 
          solctemp(i)=solctemp(i) /
     &       (fracarb(i,1)+fracarb(i,2))
        endif
        if(isand(i,2).eq.-3)then 
!>second layer bed rock
          solctemp(i)=tbar(i,1)
        endif
c
240   continue     
!>
!>find moisture scalar for soil c decomposition
!!
!!this is modelled as function of logarithm of matric potential. 
!!we find values for all soil layers, and then find an average value 
!!based on fraction of carbon present in each layer. 
!!
      do 260 j = 1, ignd
        do 270 i = il1, il2
c
          if(isand(i,j).eq.-3.or.isand(i,j).eq.-4)then
            !>set to large number so that ltrmoscl becomes 0.2
            scmotrm (i,j)=0.2
            psi (i,j) = 10000.0 

          else 
           if (isand(i,j).eq.-2) then ! peat soils!
            thpor(i,j)=thporg(min(j,3))
            b(i,j)=borg(min(j,3))
            psisat(i,j)=psisorg(min(j,3))
           else ! mineral soils
            psisat(i,j)= (10.0**(-0.0131*sand(i,j)+1.88))/100.0
            b(i,j)     = 0.159*clay(i,j)+2.91
            thpor(i,j) = (-0.126*sand(i,j)+48.9)/100.0
           end if
            psi(i,j)   = psisat(i,j)*(thliq(i,j)/(thpor(i,j)+0.005 !>the 0.005 prevents a divide by 0 situation.
     1                   -thiceg(i,j)))**(-b(i,j)) 
c   
            if(psi(i,j).ge.10000.0) then
              scmotrm(i,j)=0.2
            else if( psi(i,j).lt.10000.0 .and.  psi(i,j).gt.6.0 ) then
              scmotrm(i,j)=1.0 - 0.8*
     &   ( (log10(psi(i,j)) - log10(6.0))/(log10(10000.0)-log10(6.0)) )         
            else if( psi(i,j).le.6.0 .and.  psi(i,j).ge.4.0 ) then
              scmotrm(i,j)=1.0
            else if( psi(i,j).lt.4.0.and.psi(i,j).gt.psisat(i,j) )then 
              scmotrm(i,j)=1.0 - 
     &          0.5*( (log10(4.0) - log10(psi(i,j))) / 
     &         (log10(4.0)-log10(psisat(i,j))) )
            else if( psi(i,j).le.psisat(i,j) ) then
              scmotrm(i,j)=0.5
            endif
          endif 
!> if sand.eq.-3 or -4
          scmotrm(i,j)=max(0.0,min(scmotrm(i,j),1.0))
270     continue     
260   continue     
c
      do 290 i = il1, il2
        socmoscl(i) = scmotrm(i,1)*fracarb(i,1) + 
     &     scmotrm(i,2)*fracarb(i,2) +
     &     scmotrm(i,3)*fracarb(i,3)
        socmoscl(i) = socmoscl(i) /
     &     (fracarb(i,1)+fracarb(i,2)+fracarb(i,3))    
!>
!!make sure we don't use scmotrm of 2nd and 3rd soil layers if they are specified bedrock via sand -3 flag
!!
        if(isand(i,3).eq.-3)then ! third layer bed rock
          socmoscl(i) = scmotrm(i,1)*fracarb(i,1) +
     &                    scmotrm(i,2)*fracarb(i,2)
          socmoscl(i) = socmoscl(i) /
     &     (fracarb(i,1)+fracarb(i,2))
        endif
        if(isand(i,2).eq.-3)then ! second layer bed rock
          socmoscl(i) = scmotrm(i,1)
        endif
c
        socmoscl(i)=max(0.2,min(socmoscl(i),1.0))
290   continue     
!>
!>find moisture scalar for litter decomposition
!!
!!the difference between moisture scalar for litter and soil c is that the litter decomposition is not constrained by high
!!soil moisture (assuming that litter is always exposed to air). in addition, we use moisture content of the top soil layer
!!as a surrogate for litter moisture content. so we use only psi(i,1) calculated in loops 260 and 270 above.
!!
      do 300 i = il1, il2
        if(psi(i,1).gt.10000.0) then
          ltrmoscl(i)=0.2
        else if( psi(i,1).le.10000.0 .and.  psi(i,1).gt.6.0 ) then
          ltrmoscl(i)=1.0 - 0.8*
     &    ( (log10(psi(i,1)) - log10(6.0))/(log10(10000.0)-log10(6.0)) )
        else if( psi(i,1).le.6.0 ) then
          ltrmoscl(i)=1.0 
        endif
        ltrmoscl(i)=max(0.2,min(ltrmoscl(i),1.0))
300   continue
!>
!!use temperature of the litter and soil c pools, and their soil
!!moisture scalars to find respiration rates from these pools
!!
      do 330 i = il1, il2
      if(frac(i).gt.zero)then
!>
!!first find the q10 response function to scale base respiration
!!rate from 15 c to current temperature, we do litter first
!!
        tempq10l(i)=litrtemp(i)-273.16
        litrq10 = tanhq10(1) + tanhq10(2)*
     &            ( tanh( tanhq10(3)*(tanhq10(4)-tempq10l(i))  ) )
c
        q10func = litrq10**(0.1*(litrtemp(i)-273.16-15.0))
        litres(i)= ltrmoscl(i) * litrmass(i,icc+1)*
     &    bsratelt_g*2.64*q10func 
!>2.64 converts bsratelt_g from kg c/kg c.year to u-mol co2/kg c.s
!>respiration from soil c pool
c
        tempq10s(i)=solctemp(i)-273.16
        soilcq10= tanhq10(1) + tanhq10(2)*
     &            ( tanh( tanhq10(3)*(tanhq10(4)-tempq10s(i))  ) )
c
        q10func = soilcq10**(0.1*(solctemp(i)-273.16-15.0))
        socres(i)= socmoscl(i)* soilcmas(i,icc+1)*
     &    bsratesc_g*2.64*q10func 
!> 2.64 converts bsratesc_g from kg c/kg c.year to u-mol co2/kg c.s
c
      endif
330   continue
c
      return
      end

