!>\file
!>Canadian Terrestrial Ecosystem Model (CTEM) 
!!Stem And Root Turnover Subroutine
!!
!!The turnover of stem and root components is modelled via their PFT-dependent specified lifetimes. The litter generation (\f$kg\,C\,m^{-2}\f$ \f$day^{-1}\f$) associated with turnover of stem (\f$D_\mathrm{S}\f$) and root (\f$D_\mathrm{R}\f$) components is calculated based on the amount of biomass in the respective components (\f$C_\mathrm{S}, C_\mathrm{R}\f$; \f$kg\,C\,m^{-2}\f$) and their respective turnover timescales (\f$\tau_\mathrm{S}\f$ and \f$\tau_\mathrm{R}\f$; \f$yr\f$; see also ctem_params.f90) as
!!\f[ \label{citod} D_{i} = C_{i}\left[1 - \exp\left(-\frac{1}{365\,\tau_{i}}\right)\right],\quad i = S, R.\f]
!!
      subroutine turnover (stemmass, rootmass,  lfstatus,    ailcg,
     1                          il1,      il2,   leapnow,
     2                         sort, nol2pfts,  fcancmx,
c    3 ------------------ inputs above this line ----------------------   
     4                     stmhrlos, rothrlos,
c    5 ----------- inputs which are updated above this line -----------
     6                     stemlitr, rootlitr)
c    7 ------------------outputs above this line ----------------------
c
c               
c     17  Jan 2014  - Moved parameters to global file (ctem_params.f90)
c     J. Melton
c
c     22  Jul 2013  - Add in module for parameters
C     J. Melton
c
c     24  Sep 2012  - add in checks to prevent calculation of non-present
c     J. Melton       pfts
c
c     07  May 2003  - this subroutine calculates the litter generated
c     V. Arora        from stem and root turnover
c
c     inputs 
c     icc       - no. of ctem plant function types, currently 9
c     ilg       - no. of grid cells in latitude circle
c     ican      - number of class pfts

      use ctem_params,        only : icc, ilg, ican, kk, zero, stemlife,
     1                               rootlife, stmhrspn
c
      implicit none
c
      integer il1 !<il1=1
      integer il2 !<il2=ilg
      integer i, j, k
      integer lfstatus(ilg,icc) !<leaf status. an integer indicating if leaves are in "max. growth", 
                                !<"normal growth", "fall/harvest", or "no leaves" mode. see phenolgy subroutine for more details.
      integer n
      integer m
      integer k1,  k2
      logical :: leapnow     !< true if this year is a leap year. Only used if the switch 'leap' is true.
c
      integer sort(icc)      !<index for correspondence between 9 ctem pfts and size 12 of parameter vectors
      integer nol2pfts(ican) !<number of level 2 ctem pfts
c
      real stemmass(ilg,icc) !<stem mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
      real rootmass(ilg,icc) !<root mass for each of the 9 ctem pfts, \f$kg c/m^2\f$
      real ailcg(ilg,icc)    !<green or live lai
      real fcancmx(ilg,icc)  !<max. fractional coverage of ctem's 9 pfts, but this can be modified by 
                             !<land-use change, and competition between pfts
c
      real stemlitr(ilg,icc) !<stem litter \f$(kg c/m^2)\f$
      real rootlitr(ilg,icc) !<root litter \f$(kg c/m^2)\f$
      real nrmlsmlr(ilg,icc) !<stem litter from normal turnover
      real nrmlrtlr(ilg,icc) !<root litter from normal turnover
      real rothrlos(ilg,icc) !<root death for crops. when in "harvest" mode for crops, root is assumed
                             !<to die in a similar way as stem is harvested.
      real stmhrlos(ilg,icc) !<stem harvest loss for crops. when in "harvest" mode for crops, stem is 
                             !<also assumed to be harvested and this generates litter.
!>
!!------------------------------------------------------------------
!!Constants and parameters are located in ctem_params.f90
!!---------------------------------------------------------------
!!
!!initialize required arrays to zero
!!
      do 140 j = 1,icc
        do 150 i = il1, il2
          stemlitr(i,j)=0.0
          rootlitr(i,j)=0.0
          nrmlsmlr(i,j)=0.0          
          nrmlrtlr(i,j)=0.0          
150     continue                  
140   continue
!>
!>initialization ends    
!!
!!------------------------------------------------------------------
!!
!!calculate normal stem and root litter using the amount of stem and
!!root biomass and their turnover time scales.
!!
      do 200 j = 1, icc
       n = sort(j)
       do 210 i = il1, il2
       if (fcancmx(i,j).gt.0.0) then
        if(stemlife(n).gt.zero)then

         if (leapnow) then 
          nrmlsmlr(i,j)=stemmass(i,j)*
     &                  (1.0-exp(-1.0/(366.0*stemlife(n))))  
         else 
          nrmlsmlr(i,j)=stemmass(i,j)*
     &                  (1.0-exp(-1.0/(365.0*stemlife(n))))  
         endif 

        endif
        if(rootlife(n).gt.zero)then
         if (leapnow) then 
          nrmlrtlr(i,j)=rootmass(i,j)*
     &                  (1.0-exp(-1.0/(366.0*rootlife(n))))  
         else 
          nrmlrtlr(i,j)=rootmass(i,j)*
     &                  (1.0-exp(-1.0/(365.0*rootlife(n))))  
         endif 
        endif
       endif
210    continue
200   continue
!>
!>if crops are in harvest mode then we start harvesting stem as well.
!!if stem has already been harvested then we set the stem harvest
!!loss equal to zero. the roots of the crop die in a similar way.
!!     
      k1=0
      do 250 j = 1, ican
       if(j.eq.1) then
         k1 = k1 + 1
       else
         k1 = k1 + nol2pfts(j-1)
       endif
       k2 = k1 + nol2pfts(j) - 1
       do 255 m = k1, k2
        do 260 i = il1, il2
         if (fcancmx(i,m).gt.0.0) then
          if(j.eq.3)then     !>stem/root harvest/death for crops
c
            if(lfstatus(i,m).eq.3.and.stmhrlos(i,m).le.zero.and.
     &      stemmass(i,m).gt.zero)then          
              stmhrlos(i,m)=stemmass(i,m)*(1.0/stmhrspn)
            endif
c
            if(lfstatus(i,m).eq.3.and.rothrlos(i,m).le.zero.and.
     &      rootmass(i,m).gt.zero)then          
              rothrlos(i,m)=rootmass(i,m)*(1.0/stmhrspn)   
            endif
c
            if(stemmass(i,m).le.zero.or.lfstatus(i,m).eq.1.or.
     &      lfstatus(i,m).eq.2)then
              stmhrlos(i,m)=0.0
            endif
c
            if(rootmass(i,m).le.zero.or.lfstatus(i,m).eq.1.or.
     &      lfstatus(i,m).eq.2)then
              rothrlos(i,m)=0.0
            endif
c
          else
            stmhrlos(i,m)=0.0
            rothrlos(i,m)=0.0
          endif
         endif
260     continue
255    continue   
250   continue   
!>
!>add stem and root litter from all sources
!>
      do 350 j = 1, icc
        do 360 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then
          stemlitr(i,j)=nrmlsmlr(i,j)+stmhrlos(i,j)
          rootlitr(i,j)=nrmlrtlr(i,j)+rothrlos(i,j)
         endif
360     continue
350   continue
c
      return
      end

