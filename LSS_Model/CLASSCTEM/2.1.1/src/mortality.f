!>\file
!!Canadian Terrestrial Ecosystem Model (CTEM) 
!!Mortality Subroutine
!!
!!Mortality
!!
!!The PFT-dependent mortality rate (\f$day^{-1}\f$),
!!
!!\f[ \label{mortality} m_{\alpha} = m_{intr,\alpha} + m_{ge,\alpha} + m_{bioclim,\alpha} + m_{dist,\alpha}, \f]
!!reflects the net effect of four different processes: (1) intrinsic- or age-related mortality, \f$m_{intr}\f$, (2) growth or stress-related mortality, \f$m_{ge}\f$, (3) mortality associated with bioclimatic criteria, \f$m_{bioclim}\f$ and (4) mortality associated with disturbances, \f$m_{dist}\f$.
!!
!!Intrinsic- or age-related mortality uses a PFT-specific maximum age, \f$A_{max}\f$ (see also ctem_params.f90), to calculate an annual mortality rate such that only \f$1\,{\%}\f$ of tree PFTs exceed \f$A_{max},\alpha\f$. Intrinsic mortality accounts for processes, whose effect is not explicitly captured in the model including insect damage, hail, wind throw, etc.,
!!
!!\f[ \label{intrmort} m_{intr,\alpha} = 1 - \exp(-4.605/A_{max,\alpha}). \f]
!!
!!Grasses and crops have \f$m_{intr} = 0\f$. The annual growth-related mortality \f$m_{ge}\f$ is calculated using growth efficiency of a PFT over the course of the previous year following \cite Prentice1993-xn and \cite Sitch2003-847 as
!!
!!\f[ \label{mgrow} m_{ge,\alpha} = \frac{m_{{ge},max,\alpha}}{1 + k_{m} g_{\mathrm{e},\alpha}}, \f]
!!
!!where \f$m_{{ge},max}\f$ represents the PFT-specific maximum mortality rate when no growth occurs (see also ctem_params.f90). \f$k_{m}\f$ is a parameter set to \f$0.3\,m^{2}\,(g\,C)^{-1}\f$. \f$g_\mathrm{e}\f$ is the growth efficiency of the PFT (\f$g\,C\,m^{-2}\f$) calculated based on the maximum LAI (\f$L_{\alpha,max}\f$; \f$m^{2}\,m^{-2}\f$) and the increment in stem and root mass over the course of the previous year (\f$\Delta C_\mathrm{S}\f$ and \f$\Delta C_\mathrm{R}\f$; \f$kg\,C\,m^{-2}\f$, respectively) \cite Waring1983-wc
!!\f[ g_{\mathrm{e},\alpha} = 1000\frac{\max(0,(\Delta C_{\mathrm{S},\alpha}+\Delta C_{\mathrm{R},\alpha}))}{L_{\alpha,max}}. \f]
!!
!!Mortality associated with bioclimatic criteria, \f$m_{bioclim}\f$ (\f$0.25\,yr^{-1}\f$), is applied when climatic conditions in a grid cell become unfavourable for a PFT to exist and ensures that PFTs do not exist outside their bioclimatic envelopes, as explained in the next section.
!!
!!The annual mortality rates for \f$m_{intr}\f$, \f$m_{ge}\f$ and \f$m_{bioclim}\f$ are converted to daily rates and applied at the daily time step of the model, while \f$m_{dist}\f$ is calculated by the fire module of the model based on daily area burned for each PFT as summarized in Appendix \ref{fire}. In practice, the \f$\frac{\mathrm{d}f_\alpha}{\mathrm{d}t}=-m_{dist,\alpha}f_\alpha\f$ term of Eq. (\ref{compact}) is implemented right after area burnt is calculated.
!!
!!
      subroutine mortalty (stemmass, rootmass,    ailcg, gleafmas,
     1                     bleafmas,     il1,  il2, leapnow,
     2                     iday, sort,  fcancmx,
c    + ------------------ inputs above this line ----------------------   
     4                     lystmmas, lyrotmas, tymaxlai, grwtheff,
c    + -------------- inputs updated above this line ------------------
     5                     stemltrm, rootltrm, glealtrm, geremort,
     6                     intrmort)
c    + ------------------outputs above this line ----------------------
c
c     17  Jan 2014  - Moved parameters to global file (ctem_params.f90)
c     J. Melton
c
c     22  Jul 2013  - Add in module for parameters
C     J. Melton
c
c     24  sep 2012  - add in checks to prevent calculation of non-present
c     J. Melton       pfts
c
c     07  may 2003  - this subroutine calculates the litter generated
c     V. Arora        from leaves, stem, and root components after
c                     vegetation dies due to reduced growth efficiency
c                     or due to aging (the intrinsic mortality)  
c
c     inputs 
c
c     icc       - no. of ctem plant function types, currently 8
c     ilg       - no. of grid cells in latitude circle
c
      use ctem_params,        only : icc, ilg, kk, zero, mxmortge, 
     1                               kmort1, maxage
c
      implicit none
c
      integer il1    !<il1=1
      integer il2    !<il2=ilg
      integer i, j, k
      integer iday   !<day of the year
      integer n      !<
c
      logical leapnow   !< true if this year is a leap year. Only used if the switch 'leap' is true.
      integer sort(icc) !<index for correspondence between ctem 9 pfts and size 12 of parameters vectors
c
      real stemmass(ilg,icc) !<stem mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
      real rootmass(ilg,icc) !<root mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
      real gleafmas(ilg,icc) !<green leaf mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
      real ailcg(ilg,icc)    !<green or live lai
      real grwtheff(ilg,icc) !<growth efficiency. change in biomass per year per unit max. lai (g c/m2)/(m2/m2)
      real lystmmas(ilg,icc) !<stem mass at the end of last year
      real lyrotmas(ilg,icc) !<root mass at the end of last year
      real tymaxlai(ilg,icc) !<this year's maximum lai
      real bleafmas(ilg,icc) !<brown leaf mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
c
      real stemltrm(ilg,icc) !<stem litter generated due to mortality \f$(kg c/m^2)\f$
      real rootltrm(ilg,icc) !<root litter generated due to mortality \f$(kg c/m^2)\f$
      real glealtrm(ilg,icc) !<green leaf litter generated due to mortality \f$(kg c/m^2)\f$
      real geremort(ilg,icc) !<growth efficiency related mortality (1/day)
      real intrmort(ilg,icc) !<intrinsic mortality (1/day)
      real fcancmx(ilg,icc)  !<
!>
!!------------------------------------------------------------------
!!Constants and parameters are located in ctem_params.f90
!!---------------------------------------------------------------
!!
!!initialize required arrays to zero
!!
      do 140 j = 1,icc
        do 150 i = il1, il2
          stemltrm(i,j)=0.0    
          rootltrm(i,j)=0.0   
          glealtrm(i,j)=0.0   
          geremort(i,j)=0.0    
          intrmort(i,j)=0.0  
150     continue                  
140   continue
!>
!>initialization ends    
!!
!!------------------------------------------------------------------
!!
!!at the end of every year, i.e. when iday equals 365, we calculate
!!growth related mortality. rather than using this number to kill
!!plants at the end of every year, this mortality rate is applied
!!gradually over the next year.
!!
      do 200 j = 1, icc
        n = sort(j)
        do 210 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then
          if(iday.eq.1)then
            tymaxlai(i,j) =0.0
          endif
c
          if(ailcg(i,j).gt.tymaxlai(i,j))then
            tymaxlai(i,j)=ailcg(i,j)
          endif
c
          if ((.not. leapnow.and.iday.eq.365) .or.
     &        (leapnow.and.iday.eq.366)) then
            if(tymaxlai(i,j).gt.zero)then
              grwtheff(i,j)= ( (stemmass(i,j)+rootmass(i,j))-
     &         (lystmmas(i,j)+lyrotmas(i,j)) )/tymaxlai(i,j) 
            else
              grwtheff(i,j)= 0.0
            endif
            grwtheff(i,j)=max(0.0,grwtheff(i,j))*1000.0
            lystmmas(i,j)=stemmass(i,j)
            lyrotmas(i,j)=rootmass(i,j)
          endif
!>
!!calculate growth related mortality using last year's growth
!!efficiency or the new growth efficiency if day is 365 and
!!growth efficiency estimate has been updated above.
!!
          geremort(i,j)=mxmortge(n)/(1.0+kmort1*grwtheff(i,j))
c
!>convert (1/year) rate into (1/day) rate 
          if (leapnow) then 
            geremort(i,j)=geremort(i,j)/366.0
          else 
            geremort(i,j)=geremort(i,j)/365.0
          endif 
         endif
210     continue
200   continue
!>
!>calculate intrinsic mortality rate due to aging which implicity includes effects of frost, 
!!hail, wind throw etc. it is assumed that only 1% of the plants exceed maximum age (which is 
!!a pft-dependent parameter). to achieve this some fraction of the plants need to be killed every year. 
!!
      do 250 j = 1, icc
        n = sort(j)
        do 260 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then

           if(maxage(n).gt.zero)then
              intrmort(i,j)=1.0-exp(-4.605/maxage(n))
           else
              intrmort(i,j)=0.0
           endif

!>convert (1/year) rate into (1/day) rate   
          if (leapnow) then 
            intrmort(i,j)=intrmort(i,j)/366.0
          else 
            intrmort(i,j)=intrmort(i,j)/365.0
          endif
         endif
260     continue
250   continue 
!>
!!now that we have both growth related and intrinsic mortality rates,
!!lets combine these rates for every pft and estimate litter generated
!!
      do 300 j = 1, icc
        do 310 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then
          stemltrm(i,j)=stemmass(i,j)*
     &    ( 1.0-exp(-1.0*(geremort(i,j)+intrmort(i,j))) )
          rootltrm(i,j)=rootmass(i,j)*
     &    ( 1.0-exp(-1.0*(geremort(i,j)+intrmort(i,j))) )
          glealtrm(i,j)=gleafmas(i,j)*
     &    ( 1.0-exp(-1.0*(geremort(i,j)+intrmort(i,j))) )
         endif
310     continue
300   continue
c
      return
      end

