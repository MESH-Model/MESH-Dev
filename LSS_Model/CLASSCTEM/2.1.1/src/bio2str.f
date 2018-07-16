!>\file
C!           Canadian Terrestrial Ecosystem Model (CTEM)
C!        Biomass To Structural Attributes Conversion Subroutine 
!!
!!
!!The time-varying biomass in the leaves (\f$C_\mathrm{L}\f$), stem (\f$C_\mathrm{S}\f$) and root (\f$C_\mathrm{R}\f$) components is used to calculate the structural attributes of vegetation for the energy and water balance calculations by CLASS.
!!
!!Leaf biomass is converted to LAI using specific leaf area (\f${SLA}\f$, \f$m^2\,(kg\,C)^{-1}\f$), which itself is assumed to be a function of leaf lifespan (\f$\tau_\mathrm{L}\f$; see also ctem_params.f90)
!!
!!\f[ \label{sla} SLA= \gamma_\mathrm{L}\tau_\mathrm{L}^{-0.5}\\ LAI = C_\mathrm{L}SLA\nonumber \f]
!!
!!where \f$\gamma_\mathrm{L}\f$ is a constant with value equal to \f$25\,m^2\,(kg\,C)^{-1}\,yr^{0.5}\f$.
!!
!!The vegetation height (\f$H\f$; \f$m\f$) is calculated for tree, crop and grass PFTs as
!!\f[\label{height} H = \begin{cases} \min\left(10.0C_\mathrm{S}^{0.385},45\right) trees\\ (C_\mathrm{S} + C_\mathrm{L})^{0.385} crops\\ 3.5 (C_{L,g} + 0.55\,C_{L,b})^{0.5} grasses,\\ \end{cases} \f]
!!where \f$C_{L,g}\f$ is the green leaf biomass and \f$C_{L,b}\f$ is the brown leaf biomass that is scaled by 0.55 to reduce its contribution to the plant height. CTEM explicitly tracks brown leaf mass for grass PFTs. The turnover of green grass leaves, due to normal aging or stress from drought and/or cold, does not contribute to litter pool directly as the leaves first turn brown. The brown leaves themselves turnover to litter relatively rapidly \f$(\tau_{L,b} = 0.1\,\tau_\mathrm{L}\f$).
!!
!!CTEM dynamically simulates root distribution and depth in soil following \cite Arora2003838. The root distribution takes an exponential form and roots grow and deepen with increasing root biomass. The cumulative root fraction at depth \f$z\f$ is given by
!!\f[ \label{fracroo} f_\mathrm{R}(z) = 1 - \exp(-\iota z). \f]
!!
!!Rooting depth (\f$d_\mathrm{R}\f$; \f$m\f$), which is defined to be the depth containing \f$99\,{\%}\f$ of the root mass, is found by setting \f$z\f$ equal to \f$d_\mathrm{R}\f$ and \f$f_\mathrm{R} = 0.99\f$, which yields
!!\f[ \label{rootterm1} d_\mathrm{R} = \frac{-\ln(1-f_\mathrm{R})}{\iota} = \frac{-\ln(1 - 0.99)}{\iota} = \frac{4.605}{\iota}. \f]
!!
!!The parameter \f$\iota\f$ that describes the exponential root distribution is calculated as
!!\f[ \label{iota} \iota = \overline{\iota} \left(\frac{\overline{C_\mathrm{R}}}{C_\mathrm{R}} \right)^{0.8}, \f]
!!
!!where \f$\overline{\iota}\f$ represents the PFT-specific mean root distribution profile parameter and \f$\overline{C_\mathrm{R}}\f$ the average root biomass derived from \cite Jackson1996-va (see also ctem_params.f90). Equation (\ref{iota}) yields a lower (higher) value of \f$\iota\f$ than \f$\overline{\iota}\f$ when root biomass \f$C_\mathrm{R}\f$ is higher (lower) than the PFT-specific mean root biomass \f$\overline{C_\mathrm{R}}\f$, resulting in a deeper (shallower) root profile than the mean root profile.
!!
!!The rooting depth \f$d_\mathrm{R}\f$ is checked to ensure it does not exceed the soil depth. If so, \f$d_\mathrm{R}\f$ is set to the soil depth and \f$\iota\f$ is recalculated as \f$\iota = 4.605/d_\mathrm{R}\f$ (see Eq. \ref{rootterm1} for derivation of 4.605 term). The new value of \f$\iota\f$ is used to determine the root distribution profile adjusted to the shallower depth. Finally, the root distribution profile is used to calculate fraction of roots in each of the model's soil layers.
!!
!!
      subroutine    bio2str( gleafmas, bleafmas, stemmass, rootmass,                        
     1                            il1,      il2,  fcancmx,    zbotw,
     2                          delzw, nol2pfts,  soildpth,
c    4--------------- inputs above this line, outputs below --------
     5                          ailcg,    ailcb,     ailc,    zolnc,
     6                          rmatc, rmatctem,     slai,  bmasveg,
     7                       cmasvegc,  veghght, rootdpth,   alvisc,
     8                         alnirc,     paic,    slaic )
c
c     ----------------------------------------------------------------
c
c     2   Jul 2013  - Integreated ctem_params module
c     J. Melton       
c
c     22  Nov 2012  - calling this version 1.1 since a fair bit of ctem
c     V. Arora        subroutines were changed for compatibility with class
c                     version 3.6 including the capability to run ctem in
c                     mosaic/tile version along with class.
c
c     24  Sep 2012  - add in checks to prevent calculation of non-present
c     J. Melton       pfts. cleaned up initialization
c
c     18  May  2012 - fix bug for soils with the depth close to the
c     J. Melton       boundary between layers, was resulting in roots 
c                     being placed incorrectly.
c
c     28  Nov. 2011 - make changes for coupling with class 3.5
c     Y. Peng
c
c     31  Aug. 2009 - make changes for coupling with class 3.4. include
c     V. Arora        new output variable called plant area index
c                     (paic) and storage leaf area index (slaic) 
c
c     14  Mar. 2003 - this subroutine converts leaf, stem, and root biomass 
c     V. Arora        into lai, vegetation height, and fraction of roots
c                     in each soil layer. storage lai is also calculated.
c                     
c                     note that while ctem keeps track of 9 pfts, class 2.7
c                     keeps track of 4 pfts, so all these vegetation 
c                     structural attributes are calculated for 9 pfts
c                     sepatarely but then lumped into 4 pfts for use in
c                     energy & water balance calculations by class
c               
c                     also, this subroutine does not estimate zolnc(i,5)
c                     the log of roughness length over the bare fraction
c                     of the grid cell. only roughness lengths over the
c                     vegetated fraction are updated
c

      use ctem_params,        only : ignd, icc, ilg, ican, abszero,
     1                               l2max,kk, eta, kappa, kn, lfespany, 
     2                               fracbofg, specsla, abar, avertmas,
     3                               alpha, prcnslai, minslai, mxrtdpth,
     4                               albvis, albnir           

      implicit none

      integer il1 !<input: il1=1
      integer il2 !<input: il2=ilg
      integer i, j, k, m, n, k1c, k2c
      integer nol2pfts(ican) !<input: number of level 2 pfts
      integer icount
      integer sort(icc)
      integer kend

      logical deeproots
        
      real gleafmas(ilg,icc)     !<input: green or live leaf mass in kg c/m2, for the 9 pfts
      real bleafmas(ilg,icc)     !<input: brown or dead leaf mass in kg c/m2, for the 9 pfts
      real stemmass(ilg,icc)     !<input: stem biomass in kg c/m2, for the 9 pfts
      real rootmass(ilg,icc)     !<input: root biomass in kg c/m2, for the 9 pfts
      real ailcg(ilg,icc)        !<output: green lai for ctem's 9 pfts
      real ailcb(ilg,icc)        !<output: brown lai for ctem's 9 pfts. for now we assume only grasses can have brown leaves.
      real ailc(ilg,ican)        !<output: lai to be used by class
      real zolnc(ilg,ican)       !<output: log of roughness length to be used by class
      real paic(ilg, ican)       !<output: plant area index for class' 4 pfts. this is the sum of leaf area index and stem area index.
      real rmatc(ilg,ican,ignd)  !<output: fraction of live roots in each soil layer for each of the class' 4 pfts
      real fcancmx(ilg,icc)      !<input: max. fractional coverages of ctem's 9 pfts. this is different from fcanc and fcancs
                                 !<(which may vary with snow depth). fcancmx doesn't change, unless of course its changed by
                                 !<land use change or dynamic vegetation.
      real delzw(ilg,ignd)       !<input: thicknesses of the 3 soil layers
      real zbotw(ilg,ignd)       !<input: bottom of soil layers
      real rmatctem(ilg,icc,ignd)!<output: fraction of live roots in each soil layer for each of ctem's 9 pfts
      real slai(ilg,icc)         !<output: storage or imaginary lai for phenology purposes
      real bmasveg(ilg,icc)      !<output: total (gleaf + stem + root) biomass for each ctem pft, kg c/m2
      real cmasvegc(ilg,ican)    !<output: total canopy mass for each of the 4 class pfts. recall that class requires canopy
                                 !<mass as an input, and this is now provided by ctem. kg/m2.
      real sai(ilg,icc)          !< 
      real saic(ilg,ican)        !< 
      real sfcancmx(ilg,ican)    !< 
      real alvisc(ilg,ican)      !<output: albedo for 4 class pfts simulated by ctem, visible 
      real alnirc(ilg,ican)      !<output: albedo for 4 class pfts simulated by ctem, near ir
      real pai(ilg,icc)          !< 
      real slaic(ilg,ican)       !<output: storage lai. this will be used as min. lai that class sees
                                 !<so that it doesn't blow up in its stomatal conductance calculations.
      real sla(icc)              !< 
      real veghght(ilg,icc)      !<output: vegetation height (meters)
      real fcoeff                !< 
      real lnrghlth(ilg,icc)     !< 
      real averough(ilg,ican)    !< 
      real b(icc)                !< 
      real rootdpth(ilg,icc)     !<output: 99% soil rooting depth (meters) both veghght & rootdpth can be used as diagnostics
                                 !<to see how vegetation grows above and below ground, respectively.
      real usealpha(ilg,icc)     !< 
      real a(ilg,icc)            !< 
      real useb(ilg,icc)         !< 
      real zroot                 !< 
      real soildpth(ilg)         !<input: soil depth (m)
      real etmp(ilg,icc,ignd)    !<
      real totala(ilg,icc)       !<
      real rmat_sum              !< 
c
c     ---------------------------------------------------------------
!>     Constants and parameters are located in ctem_params.f90
!!
!!    class' original root parameterization has deeper roots than ctem's
!!    default values based on literature. in the coupled model this leads
!!    to lower evapotranspiration (et) values. an option is provided here to
!!    deepen roots, but this will also increase photosynthesis and vegetation
!!    biomass slightly, due to more access to soil water. so while use of
!!    deeper roots is desirable in the coupled global model, one may decide
!!    to use ctem's default parameterizarion for stand alone simulations, and
!!    set deeproots to .false. 
!!
      data deeproots/.false./
c     ---------------------------------------------------------------
c
c     initialization
c
      do 30 j = 1,icc
        do 40 k = 1,ignd
          do 50 i = il1,il2
            rmatctem(i,j,k)=0.0
            etmp(i,j,k)    =0.0
50        continue
40      continue
30    continue
c
      do 31 j = 1,ican
        do 41 k = 1,ignd
          do 51 i = il1,il2
            rmatc(i,j,k)=0.0
51        continue
41      continue
31    continue
c
      icount=0
      do 52 j = 1, ican
        do 53 m = 1, nol2pfts(j)
          n = (j-1)*l2max + m
          icount = icount + 1
          sort(icount)=n
53      continue
52    continue
c
      do 60 j = 1,ican
        do 70 i =  il1, il2
          ailc(i,j)=0.0
          saic(i,j)=0.0
          paic(i,j)=0.0
          slaic(i,j)=0.0
          zolnc(i,j)=0.0
          averough(i,j)=0.0
          alvisc(i,j)=0.0
          alnirc(i,j)=0.0
          cmasvegc(i,j)=0.0
          sfcancmx(i,j)=0.0    ! sum of fcancmxs

70      continue
60    continue
c
      do 80 j = 1,icc
        sla(j)=0.0
        do 90 i =  il1, il2
          usealpha(i,j)=alpha(sort(j))
          useb(i,j)=0.0
          ailcg(i,j)=0.0
          ailcb(i,j)=0.0
          veghght(i,j)=0.0
          lnrghlth(i,j)=0.0
          a(i,j)=0.0
          slai(i,j)=0.0
          sai(i,j)=0.0
          bmasveg(i,j)=0.0
          pai(i,j)=0.0
90      continue
80    continue
!>
!!------ 1. conversion of leaf biomass into leaf area index -------
!!
!!find specific leaf area (sla, m2/kg) using leaf life span
!!
      icount=0
      do 100 j = 1, ican
        do 101 m = 1, nol2pfts(j)
          n = (j-1)*l2max + m
          icount = icount + 1
          sla(icount) = 25.0*(lfespany(n)**(-0.50))
          if(specsla(n).gt.abszero) sla(icount)=specsla(n)  
101     continue
100   continue
!>
!!convert leaf biomass into lai. brown leaves could have less
!!lai than the green leaves for the same leaf mass. for now we
!!assume sla of brown leaves is fracbofg times that of green 
!!leaves. 
!!
!!also find stem area index as a function of stem biomass
!!
      do 150 j = 1,icc
        do 160 i = il1,il2
         if (fcancmx(i,j).gt.0.0) then
          ailcg(i,j)=sla(j)*gleafmas(i,j)
          ailcb(i,j)=sla(j)*bleafmas(i,j)*fracbofg
          sai(i,j)=0.55*(1.0-exp(-0.175*stemmass(i,j))) !stem area index
!>plant area index is sum of green and brown leaf area indices
!!and stem area index
          pai(i,j)=ailcg(i,j)+ailcb(i,j)+sai(i,j)

!>make class see some minimum pai, otherwise it runs into numerical
!!problems
            pai(i,j)=max(0.3,pai(i,j))

         endif
160     continue
150   continue
!>
!!get fcancmx weighted leaf area index for use by class
!!needle leaf evg + dcd = total needle leaf    
!!broad leaf evg + dcd cld + dcd dry = total broad leaf    
!!crop c3 + c4 = total crop
!!grass c3 + c4 = total grass
!!also add brown lai. note that although green + brown
!!lai is to be used by class for energy and water balance
!!calculations, stomatal conductance estimated by the 
!!photosynthesis subroutine is only based on green lai.
!!that is although both green+brown leaves intercept
!!water and light, only the green portion photosynthesizes.
!!also lump stem and plant area indices for class' 4 pfts
!!
      k1c=0
      do 200 j = 1, ican
        if(j.eq.1) then
          k1c = k1c + 1
        else
          k1c = k1c + nol2pfts(j-1)
        endif
        k2c = k1c + nol2pfts(j) - 1
        do 210 m = k1c, k2c
          do 220 i = il1, il2
            sfcancmx(i,j)=sfcancmx(i,j)+fcancmx(i,m)
            ailc(i,j)=ailc(i,j)+(fcancmx(i,m)*(ailcg(i,m)+ailcb(i,m)))
            saic(i,j)=saic(i,j)+(fcancmx(i,m)*sai(i,m))
            paic(i,j)=paic(i,j)+(fcancmx(i,m)*pai(i,m))
            slaic(i,j)=slaic(i,j)+(fcancmx(i,m)*slai(i,m))
220       continue
210     continue
200   continue
c
      do 230 j = 1, ican
        do 240 i = il1, il2
c
          if(sfcancmx(i,j).gt.abszero)then
             ailc(i,j)=ailc(i,j)/sfcancmx(i,j)
             saic(i,j)=saic(i,j)/sfcancmx(i,j)
             paic(i,j)=paic(i,j)/sfcancmx(i,j)
             slaic(i,j)=slaic(i,j)/sfcancmx(i,j)
          else
             ailc(i,j)=0.0
             saic(i,j)=0.0
             paic(i,j)=0.0
             slaic(i,j)=0.0
          endif
!>for crops and grasses set the minimum lai to a small number, other
!!wise class will never run tsolvc and thus phtsyn and ctem will not
!!be able to grow crops or grasses.
          if(j.eq.3.or.j.eq.4) ailc(i,j)=max(ailc(i,j),0.1)
c
240     continue
230   continue
!>
!!------ 2. conversion of stem biomass into roughness length -------
!!
!!class uses log of roughness length (zoln) as an input parameter. when 
!!vegetation grows and dies as per ctem, then zoln is provided by ctem.
!!
!!1. convert stem biomass into vegetation height for trees and crops,
!!and convert leaf biomass into vegetation height for grass
!!
!!2. convert vegetation height into roughness length & take its log
!!
!!3. lump this for ctem's 9 pfts into class' 4 pfts
!!
      k1c=0
      do 250 j = 1, ican
        if(j.eq.1) then
          k1c = k1c + 1
        else
          k1c = k1c + nol2pfts(j-1)
        endif
        k2c = k1c + nol2pfts(j) - 1
        do 260 m = k1c, k2c
          do 270 i = il1, il2
c          
          if (j.le.2) then                            ! trees
           veghght(i,m)=10.0*stemmass(i,m)**0.385
           veghght(i,m)=min(veghght(i,m),45.0)
          else if (j.eq.3) then                       ! crops
           veghght(i,m)=1.0*(stemmass(i,m)+gleafmas(i,m))**0.385
          else if (j.eq.4) then                       ! grasses
           veghght(i,m)=3.5*(gleafmas(i,m)+fracbofg*bleafmas(i,m))**0.50   
          endif
          lnrghlth(i,m)= log(0.10 * max(veghght(i,m),0.10))
c
270       continue
260     continue
250   continue
c
      k1c=0
      do 300 j = 1, ican
        if(j.eq.1) then
          k1c = k1c + 1
        else
          k1c = k1c + nol2pfts(j-1)
        endif
        k2c = k1c + nol2pfts(j) - 1
        do 310 m = k1c, k2c
          do 320 i = il1, il2
            averough(i,j)=averough(i,j)+(fcancmx(i,m)*lnrghlth(i,m))
320       continue
310     continue
300   continue
c
      do 330 j = 1, ican
        do 340 i = il1, il2
c
          if(sfcancmx(i,j).gt.abszero)then
             averough(i,j)=averough(i,j)/sfcancmx(i,j)
          else
            averough(i,j)=-4.605
          endif
          zolnc(i,j)=averough(i,j)
c
340     continue
330   continue
!>
!!------ 3. estimating fraction of roots in each soil layer for -----
!!------      ctem's each vegetation type, using root biomass   -----
!!
!!estimate parameter b of variable root profile parameterization
!!
      icount=0
      do 350 j = 1, ican
        do 360 m = 1, nol2pfts(j)
          n = (j-1)*l2max + m
          icount = icount + 1
c         use decreased abar for all pfts to deepen roots to account
c         for hydraulic redistrubution
          if(deeproots) then
              b(icount) = (abar(n)-1.5) * (avertmas(n)**alpha(n))
          else
              b(icount) = abar(n) * (avertmas(n)**alpha(n))
          endif
360     continue
350   continue
!>
!!use b to estimate 99% rooting depth
!!
      k1c=0
      do 370 j = 1,ican
        if(j.eq.1) then
          k1c = k1c + 1
        else
          k1c = k1c + nol2pfts(j-1)
        endif
        k2c = k1c + nol2pfts(j) - 1
        do 380 m = k1c, k2c
          do 390 i = il1, il2
c
            useb(i,m)=b(m)
            usealpha(i,m)=alpha(sort(m))
            rootdpth(i,m) = (4.605*(rootmass(i,m)**alpha(sort(m))))/b(m)

!>
!!if estimated rooting depth is greater than soil depth, or
!!the maximum rooting depth then adjust rooting depth and
!!parameter alpha
!!
!!also find "a" (parameter determining root profile). this is 
!!the "a" which depends on time varying root biomass 
!!
            if(rootdpth(i,m).gt.min(soildpth(i),zbotw(i,ignd),
     1                                mxrtdpth(sort(m))))then
              rootdpth(i,m) = min(soildpth(i),zbotw(i,ignd),
     1                                  mxrtdpth(sort(m)))
              if(rootdpth(i,m).le.abszero)then
                a(i,m)=100.0
              else
                a(i,m)=4.605/rootdpth(i,m)
              endif
            else
              if(rootmass(i,m).le.abszero)then
                a(i,m)=100.0
              else
                a(i,m)=useb(i,m)/(rootmass(i,m)**usealpha(i,m))
              endif
            endif
c
390       continue
380     continue
370   continue
c
      do 400 j = 1,icc
        do 410 i = il1, il2

           kend=9999  ! initialize with a dummy value
!>
!!using parameter "a" we can find fraction of roots in each soil layer just like class
!!

           totala(i,j) = 1.0-exp(-a(i,j)*zroot)

           if(zroot.le.zbotw(i,1))then
!!if rootdepth is shallower than the bottom of the first layer
            rmatctem(i,j,1)=1.0
            do 414 k=2,ignd
             rmatctem(i,j,k)=0.0
414         continue
            kend=1
           else
c
            do 415 k=2,ignd
             if(zroot.le.zbotw(i,k).and.zroot.gt.zbotw(i,k-1))then
!>if rootdepth is shallower than the bottom of current layer and
!!is deeper than bottom of the previous top layer
              kend=k ! kend = soil layer number in which the roots end  
             endif


415         continue

            if (kend .eq. 9999) then
              write(6,2100) i,j,k,kend
2100          format(' at (i) = (',i3,'), pft=',i2,', depth=',i2,' kend 
     & is not assigned. kend  = ',i5)
              call xit('bio2str',-3)
            end if

            etmp(i,j,1)=exp(-a(i,j)*zbotw(i,1))
            rmatctem(i,j,1)=(1.0-etmp(i,j,1))/totala(i,j)
            if (kend .eq. 2) then
!>if rootdepth is shallower than the bottom of 2nd layer
               etmp(i,j,kend)=exp(-a(i,j)*zroot)
               rmatctem(i,j,kend)=(etmp(i,j,kend-1)-etmp(i,j,kend))
     1                          /totala(i,j)
            elseif (kend .gt. 2) then
!>if rootdepth is shallower than the bottom of 3rd layer 
!!or even the deeper layer (ignd>3)

              do 416 k=2,kend-1
                etmp(i,j,k)=exp(-a(i,j)*zbotw(i,k))
                rmatctem(i,j,k)=(etmp(i,j,k-1)-etmp(i,j,k))/totala(i,j)
416           continue

              etmp(i,j,kend)=exp(-a(i,j)*zroot)
              rmatctem(i,j,kend)=(etmp(i,j,kend-1)-etmp(i,j,kend))
     1                          /totala(i,j)
            endif
           endif
c
410     continue
400   continue
!>
!!make sure all fractions (of roots in each layer) add to one.
!!
      do 411 j = 1, icc
        do 412 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then
          rmat_sum = 0.0
          do 413 k = 1, ignd
            rmat_sum = rmat_sum + rmatctem(i,j,k)
413       continue
c
          if( abs(rmat_sum-1.0).gt.1e-10) then
           write(6,2300) i,j,rmat_sum
2300       format(' at (i) = (',i3,'), pft=',i2,' fractions of roots
     &not adding to one. sum  = ',f12.7)
           call xit('bio2str',-3)
          endif
         endif
412     continue
411   continue
!>
!!lump rmatctem(i,9,ignd)  into rmatc(i,4,ignd) for use by class
!!
      k1c=0
      do 420 j = 1, ican
        if(j.eq.1) then
          k1c = k1c + 1
        else
          k1c = k1c + nol2pfts(j-1)
        endif
        k2c = k1c + nol2pfts(j) - 1
        do 430 m = k1c, k2c
          do 440 i = il1, il2
c   
            do 441 k = 1, ignd
              rmatc(i,j,k)=rmatc(i,j,k)+(fcancmx(i,m)*rmatctem(i,m,k))  
441         continue
c
440       continue
430     continue
420   continue
c
      do 450 j = 1, ican
        do 460 i = il1, il2
c
          if(sfcancmx(i,j).gt.abszero)then
             do 461 k = 1, ignd
               rmatc(i,j,k)=rmatc(i,j,k)/sfcancmx(i,j)
461          continue
          else
             rmatc(i,j,1)=1.0
             do 462 k = 2, ignd
               rmatc(i,j,k)=0.0
462          continue
          endif
c
460     continue
450   continue
!>
!>-------------------  4. calculate storage lai  --------------------
!>
      do 500 j = 1, icc
        do 510 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then
          slai(i,j)=((stemmass(i,j)+rootmass(i,j))/eta(sort(j)))
     &     **(1./kappa(sort(j)))
          slai(i,j)=(prcnslai(sort(j))/100.0)*sla(j)*slai(i,j)
!>
!>need a minimum slai to be able to grow from scratch. consider this as model seeds.
          slai(i,j)=max(slai(i,j),minslai(sort(j)))
         endif
510     continue
500   continue
!>
!!--- 5. calculate total vegetation biomass for each ctem pft, and --
!!---------------- canopy mass for each class pft ------------------
!!
      do 550 j = 1, icc
        do 560 i = il1, il2
         if (fcancmx(i,j).gt.0.0) then
          bmasveg(i,j)=gleafmas(i,j)+stemmass(i,j)+rootmass(i,j)
         endif
560     continue
550   continue
!>
!!since class uses canopy mass and not total vegetation biomass as an
!!input, we find canopy mass as a sum of stem and leaf mass, for each
!!class pft, i.e. only above ground biomass. 
!!
      k1c=0
      do 600 j = 1, ican
        if(j.eq.1) then
          k1c = k1c + 1
        else
          k1c = k1c + nol2pfts(j-1)
        endif
        k2c = k1c + nol2pfts(j) - 1
        do 610 m = k1c, k2c
          do 620 i = il1, il2
            cmasvegc(i,j)= cmasvegc(i,j) +
     &      (fcancmx(i,m)*(bleafmas(i,m)+gleafmas(i,m)+stemmass(i,m)))  
620       continue
610     continue
600   continue
c
      do 630 j = 1, ican
        do 640 i = il1, il2
c
          if(sfcancmx(i,j).gt.abszero)then
            cmasvegc(i,j)=cmasvegc(i,j)/sfcancmx(i,j)
            cmasvegc(i,j)=cmasvegc(i,j)*(1.0/0.50) !assuming biomass is 50% c
          else
            cmasvegc(i,j)=0.0
          endif
!>      
!!if there is no vegetation canopy mass will be abszero. this should 
!!essentially mean more bare ground, but since we are not changing
!!fractional coverages at present, we pass a minimum canopy mass
!!to class so that it doesn't run into numerical problems.
!!
          cmasvegc(i,j)=max(cmasvegc(i,j),3.0)
c
640     continue
630   continue
!>
!!--- 6. calculate albedo for class' 4 pfts based on specified ----
!!------ albedos of ctem 9 pfts and their fractional coveraes -----
!!
      k1c=0
      do 700 j = 1, ican
        if(j.eq.1) then
          k1c = k1c + 1
        else
          k1c = k1c + nol2pfts(j-1)
        endif
        k2c = k1c + nol2pfts(j) - 1
        do 710 m = k1c, k2c
          do 720 i = il1, il2
            alvisc(i,j)= alvisc(i,j) + (fcancmx(i,m)*albvis(sort(m)))  
            alnirc(i,j)= alnirc(i,j) + (fcancmx(i,m)*albnir(sort(m)))  
720       continue
710     continue
700   continue
c
      do 730 j = 1, ican
        do 740 i = il1, il2

          if(sfcancmx(i,j).gt.abszero)then
            alvisc(i,j)=(alvisc(i,j)/sfcancmx(i,j))/100.0
            alnirc(i,j)=(alnirc(i,j)/sfcancmx(i,j))/100.0
          else
            alvisc(i,j)=0.0
            alnirc(i,j)=0.0
          endif

740     continue
730   continue

      return
      end


