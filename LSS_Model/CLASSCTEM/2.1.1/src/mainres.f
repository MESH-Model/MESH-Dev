!>\file
!!Canadian Terrestrial Ecosystem Model (CTEM)
!!Maintenance Respiration Subroutine
!!
!!
!!Autotrophic respiration (\f$mol\,CO_2\,m^{-2}\,s^{-1}\f$) is composed of maintenance, \f$R_\mathrm{m}\f$, and growth respirations, \f$R_\mathrm{g}\f$,
!!\f[
!!R_\mathrm{a} =R_\mathrm{m} + R_\mathrm{g}.
!!\f]
!!Maintenance respiration accounts for carbon consumed by processes that keep existing plant tissues alive and is a function of environmental stresses. Maintenance respiration is calculated on a half-hourly time step (with photosynthesis) for the leaves, \f$R_{mL}\f$, and at a daily time step for the stem, \f$R_{mS}\f$, and root, \f$R_{mR}\f$, components
!!\f[
!!\label{mainres_all} R_\mathrm{m} = R_{mL} + R_{mS} + R_{mR}.
!!\f]
!!
!!Maintenance respiration is generally strongly correlated with nitrogen content \cite Reich1998-zr \cite Ryan1991-ai. The current version of CTEM does not explicitly track nitrogen in its vegetation components. Therefore, we adopt the approach of \cite Collatz1991-5bc \cite Collatz1992-jf in which the close relation between maximum catalytic capacity of Rubisco, \f$V_\mathrm{m}\f$, and leaf nitrogen content is used as a proxy to estimate leaf maintenance respiration,
!!\f[
!!R_{mL} = \varsigma_\mathrm{L}V_\mathrm{m}\,f_{25}(Q_10d,n)f_{PAR},
!!\f]
!!where \f$\varsigma_\mathrm{L}\f$ is set to 0.015 and 0.025 for \f$C_3\f$ and \f$C_4\f$ plants, respectively, \f$f_{PAR}\f$ scales respiration from the leaf to the canopy level, similar to Eq. (\ref{G_canopy}), and the \f$f_{25}(Q_10d,n)\f$ function accounts for different temperature sensitivities of leaf respiration during day (\f$d\f$) and night (\f$n\f$). \cite Pons2003-f26 and \cite Xu2003-d75 suggest lower temperature sensitivity for leaf respiration during the day compared to night, and therefore we use values of \f$Q_10d=1.3\f$ and \f$Q_10n=2.0\f$ for day and night, respectively.
!!
!!Maintenance respiration from the stem and root components is estimated based on PFT-specific base respiration rates (\f$\varsigma_\mathrm{S}\f$ and \f$\varsigma_\mathrm{R}\f$ specified at \f$15\,C\f$, \f$kg\,C\,(kg\,C)^{-1}\,yr^{-1}\f$; see also ctem_params.f90) that are modified to account for temperature response following a \f$Q_{10}\f$ function. Maintenance respiration from stem and root components, \f$R_{m\{S,R\}}\f$, is calculated as
!!\f[
!!\label{r_msr} R_{\mathrm{m},i} = 2.64 \times 10^{-6}\varsigma_{i}l_{\mathrm{v}, i}C_{i}f_{15}(Q_{10}),\quad i = \mathrm{S}, \mathrm{R},
!!\f]
!!where \f$l_{v,i}\f$ is the live fraction of stem or root component, i.e. the sapwood, and \f$C_i\f$ is the stem or root carbon mass (\f$kg\,C\,m^{-2}\f$). The constant \f$2.64 \times 10^{-6}\f$ converts units from \f$kg\,C\,m^{-2}\,yr^{-1}\f$ to \f$mol\,CO_2\,m^{-2}\,s^{-1}\f$. The live sapwood fraction, \f$l_{\mathrm{v},i}\f$, for stem or root component is calculated following the CENTURY model \cite Parton1996-zv as
!!\f[
!!l_{\mathrm{v},i} = \max(0.05, \min[1.0, \exp^{-0.2835 C_i} ]),\quad i = \mathrm{S}, \mathrm{R}.
!!\f]
!!
!!The \f$Q_{10}\f$ value used in Eq. (\ref{r_msr}) is not assumed to be constant but modelled as a function of temperature following \cite Tjoelker2001-uz as
!!\f[
!!Q_{10} = 3.22 - 0.046 \left(\frac{15.0 + T_{\{S,R\}}}{1.9}\right),
!!\f]
!!where \f$T_{\{S,R\}}\f$ is stem or root temperature (\f$C\f$). Stem temperature is assumed to be the same as air temperature while root temperature is based on the soil temperature weighted by the fraction of roots present in each soil layer \cite Arora2003838. The calculated \f$Q_{10}\f$ value is additionally constrained to be between 1.5 and 4.0.
!!
!!Growth respiration, \f$R_\mathrm{g}\f$ (\f$mol\,CO_2\,m^{-2}\,s^{-1}\f$), is estimated as a fraction (\f$\epsilon_\mathrm{g}=0.15\f$) of the positive gross canopy photosynthetic rate after maintenance respiration has been accounted for
!!\f[
!!\label{growth_res} R_\mathrm{g}=\epsilon_\mathrm{g}\max[0,(G_{canopy} - R_\mathrm{m})].
!!\f]
!!Finally, net primary productivity (\f$NPP\f$) is calculated as
!!\f[
!!NPP = G_{canopy} - R_\mathrm{m} - R_\mathrm{g}.
!!\f]
!!


      subroutine mainres (  fcan,      fct,     stemmass,   rootmass, 
     1                       il1, il2, leapnow,
     2                       tcan,         tbar,   rmatctem,
     3                      sort, nol2pfts,        isand,
c    -------------- inputs above this line, outputs below ----------
     4                      rmsveg, rmrveg,     roottemp,
     5                 AILCG,     NRUB,     RMLVEG, CTEMN)
c
c     20  sep. 2001 - this subroutine calculates maintenance respiration,
c     V. Arora        over a given sub-area, for stem and root components.
c                     leaf respiration is estimated within the phtsyn
c                     subroutine.

c     change history:

!     J. Melton 22  Jul 2015 - Loops 180 and 190 were not set up for > 3 soil layers. Fixed.
!
c     J. Melton 17  Jan 2014 - Moved parameters to global file (ctem_params.f90)
c
c     J. Melton 22  Jul 2013 - Add in module for parameters
C     
c     J. Melton 20 sep 2012 - made it so does not do calcs for pfts with
c                             fcan = 0.
c     J. Melton 23 aug 2012 - change sand to isand, converting sand to
c                             int was missing some gridcells assigned
c                             to bedrock in classb. isand is now passed
c                             in.
c
c
c     inputs 
c
c     icc       - no. of ctem pfts (currently 9)
c     ignd      - no. of soil layers
c     ilg       - no. of grid cells in latitude circle
c     ican      - number of class pfts, currently 4
c
      use ctem_params,        only : icc, ilg, ignd, ican, kk, zero, 
     1                               bsrtstem, bsrtroot, bsrtleaf,
     2                               minlvfr

      implicit none
c
      integer il1 !<il1=1
      integer il2 !<il2=ilg
      integer i, j, k
      integer sort(icc) !<index for correspondence between 9 pfts and 12 values in the parameter vectors
      integer n
      integer nol2pfts(ican) !<number of level 2 ctem pfts
      integer k1,   k2,  m
      integer isand(ilg,ignd) !<flag for bedrock or ice in a soil layer
      logical leapnow        !< true if this year is a leap year. Only used if the switch 'leap' is true.
c
      real fcan(ilg,icc)     !<fractional coverage of ctem's 9 pfts over the given sub-area
      real fct(ilg)          !<sum of all fcan fcan & fct are not used at this time but could be used at some later stage
      real stemmass(ilg,icc) !<stem biomass for the 9 pfts in \f$kg c/m^2\f$
      real tcan(ilg)         !<canopy temperature, k
      real tbar(ilg,ignd)    !<soil temperature, k
      real rootmass(ilg,icc) !<root biomass for the 9 pfts in \f$kg c/m^2\f$
      real rmsveg(ilg,icc)   !<maintenance respiration for stem for the 9 pfts
      real rmrveg(ilg,icc)   !<maintenance respiration for root for the 9 pfts both in u mol co2/m2. sec
      real rmatctem(ilg,icc,ignd) !<fraction of roots in each layer for each pft
c
      real tempq10r(ilg,icc) !<
      real tempq10s(ilg)     !<
      real roottemp(ilg,icc) !<root temperature (k)
      real q10               !<
      real q10func           !<
      real livstmfr(ilg,icc) !<
      real livrotfr(ilg,icc) !<
      real tot_rmat(ilg,icc) !<
c
      logical consq10 !<

c     Nitrogen components for mainres
      REAL  NRUB(ILG,ICC), ailcg(ilg,icc),      
     1      RMLVEG(ILG,ICC)
      LOGICAL CTEMN
!>
!>---------------------------------------------------
!!Constants and parameters are located in ctem_params.f90
!!
!!set the following switch to .true. for using constant temperature
!!indepedent q10 specified below
      data consq10 /.false./
!>
!>q10 - if using a constant temperature independent value, i.e.
!>if consq10 is set to true
      data q10/2.00/
!>
!!---------------------------------------------------
!!
!!initialize required arrays to zero
!!
      do 100 j = 1, icc
        do 110 i = il1, il2
          roottemp(i,j) = 0.0        ! root temperature
          tot_rmat(i,j) = 0.0
          rmsveg(i,j) = 0.0          ! stem maintenance respiration
          rmrveg(i,j) = 0.0          ! root maintenance respiration
          livstmfr(i,j)= 0.0         ! live stem fraction
          livrotfr(i,j)= 0.0         ! live root fraction
110     continue 
100   continue 
!>
!>initialization ends
!!
!!based on root and stem biomass, find fraction which is live.
!!for stem this would be the sapwood to total wood ratio.
!!
      k1=0
      do 120 j = 1, ican
        if(j.eq.1) then
          k1 = k1 + 1
        else
          k1 = k1 + nol2pfts(j-1)
        endif
        k2 = k1 + nol2pfts(j) - 1
        do 125 m = k1, k2
         do 130 i = il1, il2
          if(j.le.2)then     ! trees
            livstmfr(i,m) = exp(-0.2835*stemmass(i,m))  !following century model              
            livstmfr(i,m) = max(minlvfr,min(livstmfr(i,m),1.0))
            livrotfr(i,m) = exp(-0.2835*rootmass(i,m))               
            livrotfr(i,m) = max(minlvfr,min(livrotfr(i,m),1.0))
          else                 ! crop and grass are all live
            livstmfr(i,m) = 1.0
            livrotfr(i,m) = 1.0
          endif
130     continue 
125    continue 
120   continue 
!>
!>fraction of roots for each vegetation type, for each soil layer, 
!!in each grid cell is given by rmatctem (grid cell, veg type, soil layer) 
!!which bio2str subroutine calculates. rmatctem can thus be used 
!!to find average root temperature for each plant functional type 
!!
      do 180 j = 1, icc
        do 190 i = il1, il2
         if (fcan(i,j) .gt. 0.) then
          do 195 n = 1, ignd
           if (isand(i,n) .ne. -3) then !Only for non-bedrock
            roottemp(i,j)= roottemp(i,j)+ tbar(i,n)*rmatctem(i,j,n)
            tot_rmat(i,j)=tot_rmat(i,j) + rmatctem(i,j,n)
           end if
195       continue
          roottemp(i,j)=roottemp(i,j) / tot_rmat(i,j)
         endif !fcan check.     
190     continue 
180   continue 
!>
!!we assume that stem temperature is same as canopy temperature tcan.
!!using stem and root temperatures we can find their maintenance respirations rates
!!
      do 200 i = il1, il2
!>
!!first find the q10 response function to scale base respiration
!!rate from 15 c to current temperature, we do the stem first.
!!
          if (.not.consq10) then
!>when finding temperature dependent q10, use temperature which
!>is close to average of actual temperature and the temperature at which base rate is specified
            tempq10s(i)=(15.0+273.16+tcan(i))/1.9
            q10 = 3.22 - 0.046*(tempq10s(i)-273.16)       
            q10 = min(4.0, max(1.5, q10))
          endif
c
          q10func = q10**(0.1*(tcan(i)-288.16))
c
        do 210 j = 1, icc
         if (fcan(i,j) .gt. 0.) then
!>
!!This q10 value is then used with the base rate of respiration
!!(commonly taken at some reference temperature (15 deg c), see Tjoelker et
!!al. 2009 New Phytologist or Atkin et al. 2000 New Phyto for 
!!an example.). Long-term acclimation to temperature could be occuring 
!!see King et al. 2006 Nature SOM for a possible approach. JM.


          if (leapnow) then
            rmsveg(i,j)=stemmass(i,j)* livstmfr(i,j)* q10func*
     &       (bsrtstem(sort(j))/366.0)
          else 
            rmsveg(i,j)=stemmass(i,j)* livstmfr(i,j)* q10func*
     &       (bsrtstem(sort(j))/365.0)
          endif 
!>
!>convert kg c/m2.day -> u mol co2/m2.sec
          rmsveg(i,j)= rmsveg(i,j) * 963.62
!>
!>root respiration
!>   
          if (.not.consq10) then
            tempq10r(i,j)=(15.0+273.16+roottemp(i,j))/1.9
            q10 = 3.22 - 0.046*(tempq10r(i,j)-273.16)       
            q10 = min(4.0, max(1.5, q10))
          endif
c
          q10func = q10**(0.1*(roottemp(i,j)-288.16))
          if (leapnow) then 
            rmrveg(i,j)=rootmass(i,j)* livrotfr(i,j)* q10func*
     &       (bsrtroot(sort(j))/366.0)
          else 
            rmrveg(i,j)=rootmass(i,j)* livrotfr(i,j)* q10func*
     &        (bsrtroot(sort(j))/365.0)
          endif 
c
!>convert kg c/m2.day -> u mol co2/m2.sec
          rmrveg(i,j)= rmrveg(i,j) * 963.62 
c
         endif !fcan check.   
210     continue 
200   continue 


!!     Nitrogen green leaf maintanence resp  BW 7/2015

      if (CTEMN) then !turned off for mosaic (causes overheating in
                           !some pfts)
      DO 220 J = 1, ICC
        DO 230 I = IL1, IL2

!!         FIRST FIND THE Q10 RESPONSE FUNCTION TO SCALE BASE RESPIRATION
!!         RATE FROM 15 C TO CURRENT TEMPERATURE, WE DO THE STEM FIRST.

          IF (.NOT.CONSQ10) THEN
!!           WHEN FINDING TEMPERATURE DEPENDENT Q10, USE TEMPERATURE WHICH
!!           IS CLOSE TO AVERAGE OF ACTUAL TEMPERATURE AND THE TEMPERATURE
!!           AT WHICH BASE RATE IS SPECIFIED
            TEMPQ10S(I)=(15.0+273.16+TCAN(I))/1.9
            Q10 = 3.22 - 0.046*(TEMPQ10S(I)-273.16)       
            Q10 = MIN(4.0, MAX(1.5, Q10))
          ENDIF

          Q10FUNC = Q10**(0.1*(TCAN(I)-288.16))

        IF (AILCG(I,J).NE.0.) THEN
          RMLVEG(I,J) = NRUB(I,J)/AILCG(I,J)     !CONVERT NRUB from ground-based to LAI-based
     1      *Q10FUNC*(BSRTLEAF(SORT(J))/365.0)
     2                  * 963.62                  !CONVERT Kg/M2.DAY -> u MOL CO2/M2.SEC
        ELSE
          RMLVEG(I,J) = 0.
        ENDIF

230     CONTINUE 
220   CONTINUE  
      endif    

c     
      return
      end

