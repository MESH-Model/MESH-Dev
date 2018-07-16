!>\file
!!               Canadian Terrestrial Ecosystem Model (CTEM)
!!                            Allocation Subroutine
!!
!!
!!
!!Positive NPP is allocated daily to the leaf, stem and root components, which generally causes their respective biomass to increase, although the biomass may also decrease depending on the autotrophic respiration flux of a component. Negative NPP generally causes net carbon loss from the components. While CTEM offers the ability to use both specified constant or dynamically calculated allocation fractions for leaves, stems and roots, in practice the dynamic allocation fractions are primarily used. The formulation used in CTEM v. 2.0 differs from that for CTEM v. 1.0 as described in \cite Arora2005-6b1 only in the parameter values.
!!
!!The dynamic allocation to the live plant tissues is based on the light, water and leaf phenological status of vegetation. The preferential allocation of carbon to the different tissue pools is based on three assumptions: (i) if soil moisture is limiting, carbon should be preferentially allocated to roots for greater access to water, (ii) if LAI is low, carbon should be allocated to leaves for enhanced photosynthesis and finally (iii) carbon is allocated to the stem to increase vegetation height and lateral spread of vegetation when the increase in LAI results in a decrease in light penetration.
!!
!!The vegetation water status, \f$W\f$, is determined as a linear scalar quantity that varies between 0 and 1 for each PFT and calculated by weighting the degree of soil saturation ($\phi_{i}(\theta_{i})$, Eq. \ref{phitheta}) with the fraction of roots in each soil layer
!!
!!\f[ \label{degsoilsat} W = \phi_{root} = \sum_{i=1}^g \phi_{i}(\theta_{i})  r_{i}. \f]
!!
!!The light status, \f$L\f$, is parametrized as a function of LAI and nitrogen extinction coefficient, \f$k_\mathrm{n}\f$ (PFT-dependent; see also ctem_params.f90), as
!!\f[ L = \begin{cases} \exp(-k_\mathrm{n} LAI),  \quad trees and crops \\ \max\left(0,1-\frac{LAI}{4.5}\right),\quad grasses. \end{cases} \f]
!!
!!For PFTs with a stem component (i.e. tree and crop PFTs), the fractions of positive NPP allocated to stem (\f$a_{fS}\f$), leaf (\f$a_{fL}\f$) and root (\f$a_{fR}\f$) components are calculated as
!!\f[ \label{As} a_{fS}=\frac{\epsilon_\mathrm{S}+\omega_\mathrm{a}(1-L)}{1+\omega_\mathrm{a}(2-L-W)} \vspace*{-4mm} \f]
!!
!!\f[ \label{Ar} a_{fR}=\frac{\epsilon_\mathrm{R}+ \omega_\mathrm{a}(1-W)}{1+\omega_\mathrm{a}(2-L-W)}, \vspace*{-4mm}\f]
!!
!!\f[ \label{Al} a_{fL}=\frac{\epsilon_\mathrm{L}}{1+\omega_\mathrm{a}(2-L-W)}= 1-a_{fS}-a_{fR}. \f]
!!The base allocation fractions for each component (leaves -- \f$\epsilon_\mathrm{L}\f$, stem -- \f$\epsilon_\mathrm{S}\f$, and roots -- \f$\epsilon_\mathrm{R}\f$) are PFT-dependent (see also ctem_params.f90) and sum to 1, i.e. \f$\epsilon_\mathrm{L} + \epsilon_\mathrm{S} + \epsilon_\mathrm{R} = 1\f$. The parameter \f$\omega_\mathrm{a}\f$, which varies by PFT (see also ctem_params.f90), determines the sensitivity of the allocation scheme to changes in \f$W\f$ and \f$L\f$. Larger values of \f$\omega_\mathrm{a}\f$ yield higher sensitivity to changes in \f$L\f$ and \f$W\f$.
!!
!!Grasses do not have a stem component (i.e. \f$a_{fS}=0\f$) and the allocation fractions for leaf and root components are given by
!!\f[ a_{fL}=\frac{\epsilon_\mathrm{L}+\omega_\mathrm{a} L}{1+\omega_\mathrm{a}(1+L-W)},\\ a_{fR}=\frac{\epsilon_\mathrm{R}+\omega_\mathrm{a}(1-W)}{1+\omega_\mathrm{a}(1+L-W)}.\f]
!!
!!The above equations ensure that the allocation fractions add up to one (\f$a_{fL} + a_{fR} + a_{fS} = 1\f$).
!!
!!The dynamic allocation fractions are superseded under three conditions. First, during the leaf onset for crops and deciduous trees, all carbon must be allocated to leaves (\f$a_{fL} = 1\f$, \f$a_{fS} = a_{fR} = 0\f$). Second, the proportion of stem plus root biomasses to leaf biomass must satisfy the relationship:
!!\f[ \label{propwoody} C_\mathrm{S} + C_\mathrm{R} = \eta C_\mathrm{L}^{\kappa},\f]
!!
!!where \f$C_\mathrm{S}\f$, \f$C_\mathrm{R}\f$ and \f$C_\mathrm{L}\f$ are the carbon in the stem, root and leaves, respectively. The parameter \f$\eta\f$ is PFT-specific (see also ctem_params.f90) and parameter \f$\kappa\f$ has a value of 1.6 for trees and crops and 1.2 for grasses. Both parameters are based on the Frankfurt Biosphere Model (FBM) \cite Ludeke1994-px. This constraint (Eq. \ref{propwoody}) is based on the physical requirement of sufficient stem and root tissues to support a given leaf biomass. As grasses have no stem component, Eq. (\ref{propwoody}) determines their root to shoot ratio (i.e. the ratio of belowground to aboveground biomass). The final condition ensures that a minimum realistic root to shoot ratio is maintained for all PFTs (\f${lr}_{min}\f$, see also ctem_params.f90). Root mass is required for nutrient and water uptake and support for the aboveground biomass. If the minimum root to shoot ratio is not being maintained, carbon is allocated preferentially to roots.
!!
!!

      subroutine allocate(lfstatus,    thliq,    ailcg,     ailcb, 
     1                         il1,     il2,     sand,     clay,  
     2                    rmatctem, gleafmas, stemmass, rootmass,
     4                        sort, nol2pfts, fcancmx, isand,
c    5 ------------------ inputs above this line ----------------------   
     6                     afrleaf,  afrstem,  afrroot,  wiltsm,
     7                     fieldsm, wtstatus, ltstatus)
c    8 ------------------outputs  above this line ---------------------
c
C     22  Jul 2015  - The code with rmatctem was not set up for >3 soil layers.
C     J. Melton       Fixed that and also brought in isand so that the layers of
C                     bedrock won't have their rmat used.
c
c     17  Jan 2014  - Moved parameters to global file (ctem_params.f90)
c     J. Melton
c   
c     5   Jul 2013  - Fixed bug with initializing the variables. Brought in
c     J. Melton       the modules for global parameters
c
c     22  Nov 2012  - Calling this version 1.1 since a fair bit of ctem
c     V. Arora        subroutines were changed for compatibility with class
c                     version 3.6 including the capability to run ctem in
c                     mosaic/tile version along with class.
c
c     24  Sep 2012  - Add in checks to prevent calculation of non-present
c     J. Melton       pfts
c
c     05  May 2003  - This subroutine calculates the allocation fractions
c     V. Arora        for leaf, stem, and root components for ctem's pfts 
c
c     inputs 
c     icc       - no. of ctem plant function types, currently 9
c     ignd        - no. of soil layers (currently 3)
c     ilg       - no. of grid cells in latitude circle
c     ican        - number of class pfts
c

      use ctem_params,        only : eta, kappa, kn, abszero, icc, ilg,
     1                               ignd, kk, ican, omega, epsilonl,
     2                               epsilons, epsilonr, caleaf, castem,
     3                               caroot, consallo, rtsrmin, aldrlfon
c
      implicit none

      integer il1 !<input: il1=1
      integer il2 !<input: il2=ilg
      integer i, j, k
      integer lfstatus(ilg,icc) !<input: leaf status. an integer indicating if leaves are  
                                !<in "max. growth", "normal growth", "fall/harvest",
                                !<or "no leaves" mode. see phenolgy subroutine for more details.
      integer n, k1,  k2,   m
c
      integer sort(icc) !<input: index for correspondence between 9 pfts and the
                        !<12 values in parameters vectors
      integer nol2pfts(ican) !<input: number of level 2 ctem pfts
      integer isand(ilg,ignd)
c
      real   ailcg(ilg,icc) !<input: green or live leaf area index
      real   ailcb(ilg,icc) !<input: brown or dead leaf area index
      real   thliq(ilg,ignd) !<input: liquid soil moisture content in 3 soil layers
      real   wiltsm(ilg,ignd) !<output: wilting point soil moisture content (called PSIWLT in CLASS, but
                              !<calculated again here to avoid passing through coupler)
      real   fieldsm(ilg,ignd) !<output: field capacity soil moisture content (called THFC in CLASS, but
                               !<calculated again here to avoid passing through coupler)
      real   rootmass(ilg,icc) !<input: root mass for each of the 9 ctem pfts, kg c/m2
      real   rmatctem(ilg,icc,ignd) !<input: fraction of roots in each soil layer for each pft
      real   gleafmas(ilg,icc) !<input: green or live leaf mass in kg c/m2, for the 9 pfts
      real   stemmass(ilg,icc) !<input: stem mass for each of the 9 ctem pfts, kg c/m2

      real   sand(ilg,ignd) !<input: percentage sand
      real   clay(ilg,ignd) !<input: percentage clay
      real   thpor(ilg,ignd)
      real   psisat(ilg,ignd)
      real   b(ilg,ignd)
      real   grksat(ilg,ignd)
c
      real   afrleaf(ilg,icc) !<output: allocation fraction for leaves
      real   afrstem(ilg,icc) !<output: allocation fraction for stem
      real   afrroot(ilg,icc) !<output: allocation fraction for root
      real   fcancmx(ilg,icc) !<input: max. fractional coverage of ctem's 9 pfts, but this can be
                              !<modified by land-use change, and competition between pfts
c
      real  avwiltsm(ilg,icc),  afieldsm(ilg,icc),    avthliq(ilg,icc)
      real  wtstatus(ilg,icc) !<output: soil water status (0 dry -> 1 wet)
      real  ltstatus(ilg,icc) !<output: light status
      real  nstatus(ilg,icc)
      real  wnstatus(ilg,icc),              denom,   mnstrtms(ilg,icc),
     1                   diff,              term1,               term2,
     2         aleaf(ilg,icc),     astem(ilg,icc),      aroot(ilg,icc),
     3      tot_rmat_ctem(ilg,icc)
c
c
c     ------------------------------------------------------------------
c     Constants and parameters are located in ctem_params.f90
c     ---------------------------------------------------------------
c
c     initialize required arrays to 0
c
      do 140 j = 1,icc
        do 150 i = il1, il2
          afrleaf(i,j)=0.0    !<allocation fraction for leaves
          afrstem(i,j)=0.0    !<allocation fraction for stem
          afrroot(i,j)=0.0    !<allocation fraction for root
c
            aleaf(i,j)=0.0    !<temporary variable
            astem(i,j)=0.0    !<temporary variable
            aroot(i,j)=0.0    !<temporary variable
                              !<averaged over the root zone
          avwiltsm(i,j)=0.0   !<wilting point soil moisture
          afieldsm(i,j)=0.0   !<field capacity soil moisture
          avthliq(i,j)=0.0    !<liquid soil moisture content
          tot_rmat_ctem(i,j)= 0.0 !<temp var.
c
          wtstatus(i,j)=0.0   !<water status
          ltstatus(i,j)=0.0   !<light status
           nstatus(i,j)=0.0   !<nitrogen status, if and when we
c                             !<will have n cycle in the model
          wnstatus(i,j)=0.0   !<min. of water & n status
c
          mnstrtms(i,j)=0.0   !<min. (stem+root) biomass needed to
c                             !<support leaves
150     continue                  
140   continue
c
c     initialization ends    
c
c     ------------------------------------------------------------------
!>Estimate field capacity and wilting point soil moisture contents
!!
!!Wilting point corresponds to matric potential of 150 m
!!field capacity corresponds to hydarulic conductivity of
!!0.10 mm/day -> 1.157x1e-09 m/s
!!
      do 160 j = 1, ignd
        do 170 i = il1, il2
c
          psisat(i,j)= (10.0**(-0.0131*sand(i,j)+1.88))/100.0
          grksat(i,j)= (10.0**(0.0153*sand(i,j)-0.884))*7.0556e-6
          thpor(i,j) = (-0.126*sand(i,j)+48.9)/100.0
          b(i,j)     = 0.159*clay(i,j)+2.91
c
          wiltsm(i,j) = (150./psisat(i,j))**(-1.0/b(i,j))
          wiltsm(i,j) = thpor(i,j) * wiltsm(i,j)
c
          fieldsm(i,j) = (1.157e-09/grksat(i,j))**
     &      (1./(2.*b(i,j)+3.))
          fieldsm(i,j) = thpor(i,j) *  fieldsm(i,j)
c
170     continue
160   continue

!>
!!Calculate liquid soil moisture content, and wilting and field capacity 
!!soil moisture contents averaged over the root zone. note that while
!!the soil moisture content is same under the entire gcm grid cell,
!!soil moisture averaged over the rooting depth is different for each
!!pft because of different fraction of roots present in each soil layer.
!!
      do 200 j = 1, icc
        do 210 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then 
          do 215 n = 1, ignd
           if (isand(i,n) .ne. -3) then !Only for non-bedrock
            avwiltsm(i,j) = avwiltsm(i,j) + wiltsm(i,n)*rmatctem(i,j,n)
            afieldsm(i,j) = afieldsm(i,j) + fieldsm(i,n)*rmatctem(i,j,n)
            avthliq(i,j)  = avthliq(i,j) + thliq(i,n)*rmatctem(i,j,n)
            tot_rmat_ctem(i,j) = tot_rmat_ctem(i,j) + rmatctem(i,j,n)
           end if
215       continue
          avwiltsm(i,j) = avwiltsm(i,j) / tot_rmat_ctem(i,j)
          afieldsm(i,j) = afieldsm(i,j) / tot_rmat_ctem(i,j)
          avthliq(i,j)  = avthliq(i,j) / tot_rmat_ctem(i,j)
         end if
210     continue
200   continue
!>
!!Using liquid soil moisture content together with wilting and field 
!!capacity soil moisture contents averaged over the root zone, find
!!soil water status.
!!
      do 230 j = 1, icc
        do 240 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then 
          if(avthliq(i,j).le.avwiltsm(i,j))then
            wtstatus(i,j)=0.0
          else if(avthliq(i,j).gt.avwiltsm(i,j).and.
     &    avthliq(i,j).lt.afieldsm(i,j))then
            wtstatus(i,j)=(avthliq(i,j)-avwiltsm(i,j))/
     &      (afieldsm(i,j)-avwiltsm(i,j))
          else
            wtstatus(i,j)=1.0
          endif
         endif
240     continue
230   continue
!>
!!Calculate light status as a function of lai and light extinction
!!parameter. for now set nitrogen status equal to 1, which means 
!!nitrogen is non-limiting.
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
          if(j.eq.4) then  ! grasses
            ltstatus(i,m)=max(0.0, (1.0-(ailcg(i,m)/4.0)) )
          else             ! trees and crops
            ltstatus(i,m)=exp(-kn(sort(m))*ailcg(i,m))
          endif
          nstatus(i,m) =1.0
260     continue 
255    continue
250   continue
!>
!!allocation to roots is determined by min. of water and nitrogen
!!status
!!
      do 380 j = 1,icc
        do 390 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then 
          wnstatus(i,j)=min(nstatus(i,j),wtstatus(i,j))
         endif
390     continue
380   continue
!>
!!now that we know water, light, and nitrogen status we can find
!!allocation fractions for leaves, stem, and root components. note
!!that allocation formulae for grasses are different from those
!!for trees and crops, since there is no stem component in grasses. 
!!
      k1=0
      do 400 j = 1, ican
       if(j.eq.1) then
         k1 = k1 + 1
       else
         k1 = k1 + nol2pfts(j-1)
       endif
       k2 = k1 + nol2pfts(j) - 1
       do 405 m = k1, k2
        do 410 i = il1, il2
          n = sort(m)
          if(j.le.3)then           !trees and crops
            denom = 1.0 + (omega(n)*( 2.0-ltstatus(i,m)-wnstatus(i,m) ))    
            afrstem(i,m)=( epsilons(n)+omega(n)*(1.0-ltstatus(i,m)) )/
     &                     denom  
            afrroot(i,m)=( epsilonr(n)+omega(n)*(1.0-wnstatus(i,m)) )/
     &                     denom  
            afrleaf(i,m)=  epsilonl(n)/denom 
          else if (j.eq.4) then     !grasses
            denom = 1.0 + (omega(n)*( 1.0+ltstatus(i,m)-wnstatus(i,m) ))
            afrleaf(i,m)=( epsilonl(n) + omega(n)*ltstatus(i,m) ) /denom  
            afrroot(i,m)=( epsilonr(n)+omega(n)*(1.0-wnstatus(i,m)) )/
     &                     denom  
            afrstem(i,m)= 0.0
          endif
410     continue
405    continue
400   continue
!>
!!if using constant allocation factors then replace the dynamically
!!calculated allocation fractions.
!!
      if(consallo)then
        do 420 j = 1, icc
          do 421 i = il1, il2
           if (fcancmx(i,j).gt.0.0) then 
            afrleaf(i,j)=caleaf(sort(j))
            afrstem(i,j)=castem(sort(j))
            afrroot(i,j)=caroot(sort(j))
           endif
421       continue
420     continue
      endif
!>
!!make sure allocation fractions add to one

!I made a change here where it does not use the adsolute zero number, that is very sensitive to numerical error
!So instead I change it to 0.0001, for both loop DO 440 and DO 590
!!
      do 430 j = 1, icc
        do 440 i = il1, il2 
         if (fcancmx(i,j).gt.0.0) then 
          if(abs(afrstem(i,j)+afrroot(i,j)+afrleaf(i,j)-1.0).gt.0.001) 
     &    then  
           write(6,2000) i,j,(afrstem(i,j)+afrroot(i,j)+afrleaf(i,j))
2000       format(' at (i) = (',i3,'), pft=',i2,'  allocation fractions
     &not adding to one. sum  = ',e12.7)
       write(*,*)abs(afrstem(i,j)+afrroot(i,j)+afrleaf(i,j)-1.0)-abszero
          call xit('allocate',-2)
          endif
         endif
440     continue
430   continue
!>
!!the allocation fractions calculated above are overridden by two
!!rules. 
!!
!!rule 1 which states that at the time of leaf onset which corresponds 
!!to leaf status equal to 1, more c is allocated to leaves so 
!!that they can grow asap. in addition when leaf status is 
!!"fall/harvest" then nothing is allocated to leaves.
!!
      k1=0
      do 500 j = 1, ican
       if(j.eq.1) then
         k1 = k1 + 1
       else
         k1 = k1 + nol2pfts(j-1)
       endif
       k2 = k1 + nol2pfts(j) - 1
       do 505 m = k1, k2
        do 510 i = il1, il2
         if (fcancmx(i,m).gt.0.0) then 
          if(lfstatus(i,m).eq.1) then
            aleaf(i,m)=aldrlfon(sort(m))
!>
!!for grasses we use the usual allocation even at leaf onset
!!
            if(j.eq.4)then
              aleaf(i,m)=afrleaf(i,m)
            endif
c
            diff  = afrleaf(i,m)-aleaf(i,m)
            if((afrstem(i,m)+afrroot(i,m)).gt.abszero)then 
              term1 = afrstem(i,m)/(afrstem(i,m)+afrroot(i,m))
              term2 = afrroot(i,m)/(afrstem(i,m)+afrroot(i,m))
            else
              term1 = 0.0
              term2 = 0.0
            endif 
            astem(i,m) = afrstem(i,m) + diff*term1
            aroot(i,m) = afrroot(i,m) + diff*term2
            afrleaf(i,m)=aleaf(i,m)
            afrstem(i,m)=max(0.0,astem(i,m))
            afrroot(i,m)=max(0.0,aroot(i,m))
          else if(lfstatus(i,m).eq.3)then
            aleaf(i,m)=0.0
            diff  = afrleaf(i,m)-aleaf(i,m)
            if((afrstem(i,m)+afrroot(i,m)).gt.abszero)then 
              term1 = afrstem(i,m)/(afrstem(i,m)+afrroot(i,m))
              term2 = afrroot(i,m)/(afrstem(i,m)+afrroot(i,m))
            else
              term1 = 0.0
              term2 = 0.0
            endif 
            astem(i,m) = afrstem(i,m) + diff*term1
            aroot(i,m) = afrroot(i,m) + diff*term2
            afrleaf(i,m)=aleaf(i,m)
            afrstem(i,m)=astem(i,m)
            afrroot(i,m)=aroot(i,m)
          endif
         endif
510     continue
505    continue
500   continue

!>
!!rule 2 overrides rule 1 above and makes sure that we do not allow the 
!!amount of leaves on trees and crops (i.e. pfts 1 to 7) to exceed 
!!an amount such that the remaining woody biomass cannot support. 
!!if this happens, allocation to leaves is reduced and most npp 
!!is allocated to stem and roots, in a proportion based on calculated 
!!afrstem and afrroot. for grasses this rule essentially constrains 
!!the root:shoot ratio, meaning that the model grasses can't have 
!!lots of leaves without having a reasonable amount of roots.
!!
      do 530 j = 1, icc
        n=sort(j)
        do 540 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then 
c         find min. stem+root biomass needed to support the green leaf 
c         biomass.
          mnstrtms(i,j)=eta(n)*(gleafmas(i,j)**kappa(n))
c
          if( (stemmass(i,j)+rootmass(i,j)).lt.mnstrtms(i,j)) then   
            if( (afrstem(i,j)+afrroot(i,j)).gt.abszero ) then
              aleaf(i,j)=min(0.05,afrleaf(i,j))
              diff  = afrleaf(i,j)-aleaf(i,j)
              term1 = afrstem(i,j)/(afrstem(i,j)+afrroot(i,j))
              term2 = afrroot(i,j)/(afrstem(i,j)+afrroot(i,j))
              astem(i,j) = afrstem(i,j) + diff*term1
              aroot(i,j) = afrroot(i,j) + diff*term2
              afrleaf(i,j)=aleaf(i,j)
              afrstem(i,j)=astem(i,j)
              afrroot(i,j)=aroot(i,j)
            else
              aleaf(i,j)=min(0.05,afrleaf(i,j))
              diff  = afrleaf(i,j)-aleaf(i,j)
              afrleaf(i,j)=aleaf(i,j)
              afrstem(i,j)=diff*0.5 + afrstem(i,j)
              afrroot(i,j)=diff*0.5 + afrroot(i,j)
            endif
          endif
         endif
540     continue
530   continue
!>
!!make sure that root:shoot ratio is at least equal to rtsrmin. if not
!!allocate more to root and decrease allocation to stem.
!!
      do 541 j = 1, icc
        n=sort(j)
        do 542 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then 
          if( (stemmass(i,j)+gleafmas(i,j)).gt.0.05)then
            if( (rootmass(i,j)/(stemmass(i,j)+gleafmas(i,j))).
     &      lt.rtsrmin(n) ) then  
              astem(i,j)=min(0.05,afrstem(i,j))
              diff = afrstem(i,j)-astem(i,j)
              afrstem(i,j)=afrstem(i,j)-diff
              afrroot(i,j)=afrroot(i,j)+diff
            endif
          endif
         endif
542     continue
541   continue
!>
!!finally check if all allocation fractions are positive and check
!!again they all add to one.
!!
      do 550 j = 1, icc
        do 560 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then 
          if( (afrleaf(i,j).lt.0.0).or.(afrstem(i,j).lt.0.0).or.
     &    (afrroot(i,j).lt.0.0))then
           write(6,2200) i,j
2200       format(' at (i) = (',i3,'), pft=',i2,'  allocation fractions 
     & negative') 
           write(6,2100)afrleaf(i,j),afrstem(i,j),afrroot(i,j)
2100       format(' aleaf = ',f12.9,' astem = ',f12.9,' aroot = ',f12.9)
           call xit('allocate',-3)
          endif
         endif
560     continue
550   continue
c
      do 580 j = 1, icc
        do 590 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then 
          if(abs(afrstem(i,j)+afrroot(i,j)+afrleaf(i,j)-1.0).gt.0.0001) 
     &    then  
           write(6,2300) i,j,(afrstem(i,j)+afrroot(i,j)+afrleaf(i,j))
2300       format(' at (i) = (',i3,'), pft=',i2,'  allocation fractions
     &not adding to one. sum  = ',f12.7)
           call xit('allocate',-4)
          endif
         endif
590     continue
580   continue
c
      return
      end

