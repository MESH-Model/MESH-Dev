!>\file
!!Canadian Terrestrial Ecosystem Model (CTEM) 
!!Heterotrophic Respiration Subtoutine For Vegetated Fraction
!!
      subroutine hetresv ( fcan,      fct, litrmass, soilcmas,   
     1                      il1,
     2                      il2,     tbar,    thliq,     sand,     
     3                     clay, roottemp,    zbotw,     sort,
     4                     isand,
c    -------------- inputs above this line, outputs below -------------
     5                 ltresveg, scresveg,thicec)
c
c     16  oct. 2001 - this subroutine calculates heterotrophic respiration
c     v. arora        for a given sub-area, from litter and soil carbon
c                     pools. 
c
c     change history:
c
C     1 Dec 2016 -    Make it so it works with the organic soils parameterization
C     J. Melton
C
C     30  Jul 2015  - Based on work by Yuanqiao Wu, respiration was found to
c                     behave incorrectly if the soil froze as it thought the water
c                     was leaving the soil. This is now fixed.
c     17  Jan 2014  - Moved parameters to global file (ctem_params.f90)
c     J. Melton
c
c     22  Jul 2013  - Add in module for parameters
C     J. Melton
c
c     j. melton and v.arora - changed tanhq10 parameters, they were switched
c               25 sep 2012
c     j. melton 23 aug 2012 - bring in isand, converting sand to
c                             int was missing some gridcells assigned
c                             to bedrock in classb
c     ------
c     inputs 
c
c     icc       - no. of vegetation types (currently 9)
c     ignd      - no. of soil layers (currently 3)
c     ilg       - no. of grid cells in latitude circle
c
      use ctem_params,        only : icc, ilg, ignd, kk, zero, bsratelt, 
     1                               bsratesc, abar, tanhq10, 
     2                               alpha_hetres

      implicit none

      integer il1       !<il1=1
      integer il2       !<il2=ilg
      integer i, j, k
      integer sort(icc) !<index for correspondence between 9 pfts and 12 values in the parameters vectors
      integer isand(ilg,ignd) !<
c
      real fcan(ilg,icc)      !<fractional coverage of ctem's 9 pfts
      real fct(ilg)           !<sum of all fcan, fcan & fct are not used at this time but could be used at some later stage
      real litrmass(ilg,icc+1)!<litter mass for the 9 pfts + bare in \f$kg c/m^2\f$
      real tbar(ilg,ignd)     !<soil temperature, k
      real soilcmas(ilg,icc+1)!<soil carbon mass for the 9 pfts + bare in \f$kg c/m^2\f$
      real thliq(ilg,ignd)    !<liquid soil moisture content in 3 soil layers
      real sand(ilg,ignd)     !<percentage sand
      real clay(ilg,ignd)     !<percentage clay
      real roottemp(ilg,icc)  !<root temperature as estimated in mainres subroutine
      real zbotw(ilg,ignd)    !<bottom of soil layers
      real ltresveg(ilg,icc)  !<litter respiration for the given sub-area in umol co2/m2.s, for ctem's 9 pfts
      real scresveg(ilg,icc)  !<soil carbon respiration for the given sub-area in umol co2/m2.s, for ctem's 9 pfts
      real thicec(ilg,ignd)   !<liquid soil moisture content in 3 soil layers in canopy covered subarea

      real litrq10            !<
      real soilcq10           !<
      real litrtemp(ilg,icc)  !<litter temperature
      real solctemp(ilg,icc)  !<soil carbon pool temperature
      real q10func            !<
      real psisat(ilg,ignd)   !<saturation matric potential
      real grksat(ilg,ignd)   !<saturation hyd. conductivity
      real b(ilg,ignd)        !<parameter b of clapp and hornberger
      real thpor(ilg,ignd)    !<porosity
      real fracarb(ilg,icc,ignd) !<fraction of carbon in each soil layer for each vegetation
      real zcarbon            !<
      real tempq10l(ilg,icc)  !<
      real socmoscl(ilg,icc)  !<soil moisture scalar for soil carbon decomposition
      real scmotrm(ilg,ignd)  !<soil carbon moisture term
      real ltrmoscl(ilg)      !<soil moisture scalar for litter decomposition
      real psi(ilg,ignd)      !<
      real tempq10s(ilg,icc)  !<
      real fcoeff             !<

      real thporg(3)          !<porosity for peat soils
      real psisorg(3)         !<saturation matric potential for peat soils
      real borg(3)            !<parameter b of clapp and hornberger for peat soils

      COMMON /CLASS5/ THPORG,BORG,PSISORG
!>
!>------------------------------------------------------------------
!!Constants and parameters are located in ctem_params.f90
!!
!!parameters of the hyperbolic tan q10 formulation
!!
!!---------------------------------------------------------------
!!
!!initialize required arrays to zero

      do 100 j = 1, icc
        do 110 i = il1, il2
          litrtemp(i,j)=0.0 
          tempq10l(i,j)=0.0
          solctemp(i,j)=0.0    
          tempq10s(i,j)=0.0
          socmoscl(i,j)=0.0    
          ltresveg(i,j)=0.0
          scresveg(i,j)=0.0
110     continue
100   continue
c
      do 120 j = 1, ignd
        do 130 i = il1, il2
          psisat(i,j) = 0.0        
          grksat(i,j) = 0.0        
          thpor(i,j) = 0.0         
          b(i,j) = 0.0             
          scmotrm(i,j)=0.0         
130     continue
120   continue

      do 140 i = il1, il2
        ltrmoscl(i)=0.0          
140   continue
c
      do 150 k = 1, ignd
        do 150 j = 1, icc
          do 150 i = il1, il2
            fracarb(i,j,k)=0.0
150   continue
!>
!!initialization ends    
!!
!!------------------------------------------------------------------
!!
!!estimate temperature of the litter and soil carbon pools. litter
!!temperature is weighted average of temperatue of top soil layer
!!(where the stem and leaf litter sits) and root temperature, because
!!litter pool is made of leaf, stem, and root litter.
!!     
      do 200 j = 1,icc
        do 210 i = il1, il2
         if (fcan(i,j) .gt. 0.) then
          litrtemp(i,j)=alpha_hetres*tbar(i,1)+roottemp(i,j)*
     1                                    (1.0-alpha_hetres)
         endif
210     continue
200   continue
!>
!!estimation of soil carbon pool temperature is not straight forward.
!!ideally soil c pool temperature should be set same as root temperature,
!!since soil c profiles are similar to root profiles. but in the event
!!when the roots die then we may run into trouble. so we find the 
!!temperature of the soil c pool assuming that soil carbon is
!!exponentially distributed, just like roots. but rather than using 
!!the parameter of this exponential profile from our variable root 
!!distribution we use fixed vegetation-dependent parameters.
!!
      do 230 j = 1, icc
        do 240 i = il1, il2
         if (fcan(i,j) .gt. 0.) then
c
          zcarbon=3.0/abar(sort(j))                ! 95% depth
          if(zcarbon.le.zbotw(i,1)) then
              fracarb(i,j,1)=1.0             !> fraction of carbon in
              fracarb(i,j,2)=0.0             !> soil layers
              fracarb(i,j,3)=0.0
          else
              fcoeff=exp(-abar(sort(j))*zcarbon)
              fracarb(i,j,1)=
     &          1.0-(exp(-abar(sort(j))*zbotw(i,1))-fcoeff)/(1.0-fcoeff) 
              if(zcarbon.le.zbotw(i,2)) then
                  fracarb(i,j,2)=1.0-fracarb(i,j,1)
                  fracarb(i,j,3)=0.0
              else
                  fracarb(i,j,3)=
     &             (exp(-abar(sort(j))*zbotw(i,2))-fcoeff)/(1.0-fcoeff)    
                  fracarb(i,j,2)=1.0-fracarb(i,j,1)-fracarb(i,j,3)
              endif
          endif
c
          solctemp(i,j)=tbar(i,1)*fracarb(i,j,1) +
     &                  tbar(i,2)*fracarb(i,j,2) +
     &                  tbar(i,3)*fracarb(i,j,3)
          solctemp(i,j)=solctemp(i,j) /
     &       (fracarb(i,j,1)+fracarb(i,j,2)+fracarb(i,j,3))
!>
!>make sure we don't use temperatures of 2nd and 3rd soil layers if they are specified bedrock via sand -3 flag
!>
          if(isand(i,3).eq.-3)then !>third layer bed rock
            solctemp(i,j)=tbar(i,1)*fracarb(i,j,1) +
     &                    tbar(i,2)*fracarb(i,j,2) 
            solctemp(i,j)=solctemp(i,j) /
     &         (fracarb(i,j,1)+fracarb(i,j,2))
          endif 
          if(isand(i,2).eq.-3)then !>second layer bed rock
            solctemp(i,j)=tbar(i,1)
          endif
        endif
240     continue     
230   continue     
!>
!>find moisture scalar for soil c decomposition
!!
!!this is modelled as function of logarithm of matric potential. we find values for all soil layers, and then find an average value 
!!based on fraction of carbon present in each layer. this makes moisture scalar a function of vegetation type.
!!
      do 260 j = 1, ignd
        do 270 i = il1, il2
c
          if(isand(i,j).eq.-3.or.isand(i,j).eq.-4)then
            scmotrm (i,j)=0.2
            !>set to large number so that ltrmoscl becomes 0.2
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
     1                   -thicec(i,j)))**(-b(i,j)) 
c   
            if(psi(i,j).ge.10000.0) then
              scmotrm(i,j)=0.2
            else if( psi(i,j).lt.10000.0 .and.  psi(i,j).gt.6.0 ) then
              scmotrm(i,j)=1.0 - 0.8*
     &    ( (log10(psi(i,j)) - log10(6.0))/(log10(10000.0)-log10(6.0)) )               
            else if( psi(i,j).le.6.0 .and. psi(i,j).ge.4.0 ) then
              scmotrm(i,j)=1.0
            else if( psi(i,j).lt.4.0 .and. psi(i,j).gt.psisat(i,j) )then 
              scmotrm(i,j)=1.0 - 
     &          0.5*( (log10(4.0) - log10(psi(i,j))) / 
     &         (log10(4.0)-log10(psisat(i,j))) )
            else if( psi(i,j).le.psisat(i,j) ) then
              scmotrm(i,j)=0.5
            endif
            scmotrm(i,j)=max(0.2,min(1.0,scmotrm(i,j)))
          endif !> sand.eq.-3 or -4
c
270     continue     
260   continue     
c
      do 280 j = 1, icc
        do 290 i = il1, il2
         if (fcan(i,j) .gt. 0.) then
          socmoscl(i,j) = scmotrm(i,1)*fracarb(i,j,1) + 
     &                    scmotrm(i,2)*fracarb(i,j,2) +
     &                    scmotrm(i,3)*fracarb(i,j,3)
          socmoscl(i,j) = socmoscl(i,j) /
     &       (fracarb(i,j,1)+fracarb(i,j,2)+fracarb(i,j,3))    
!>
!>make sure we don't use scmotrm of 2nd and 3rd soil layers if they are specified bedrock via sand -3 flag
!>
          if(isand(i,3).eq.-3)then !> third layer bed rock
            socmoscl(i,j) = scmotrm(i,1)*fracarb(i,j,1) + 
     &                      scmotrm(i,2)*fracarb(i,j,2) 
            socmoscl(i,j) = socmoscl(i,j) /
     &       (fracarb(i,j,1)+fracarb(i,j,2))
          endif
          if(isand(i,2).eq.-3)then !> second layer bed rock
            socmoscl(i,j) = scmotrm(i,1)
          endif
          socmoscl(i,j)=max(0.2,min(1.0,socmoscl(i,j)))
         endif
290     continue     
280   continue     
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
        else if( psi(i,1).le.10000.0 .and. psi(i,1).gt.6.0 ) then
          ltrmoscl(i)=1.0 -  0.8*
     &    ( (log10(psi(i,1)) - log10(6.0))/(log10(10000.0)-log10(6.0)) )
        else if( psi(i,1).le.6.0 ) then
          ltrmoscl(i)=1.0 
        endif
        ltrmoscl(i)=max(0.2,min(1.0,ltrmoscl(i)))
300   continue
!>
!!use temperature of the litter and soil c pools, and their soil moisture scalars to find respiration rates from these pools
!!
      do 320 j = 1, icc
        do 330 i = il1, il2
         if (fcan(i,j) .gt. 0.) then
!>
!!first find the q10 response function to scale base respiration rate from 15 c to current temperature, we do litter first
!!
          tempq10l(i,j)=litrtemp(i,j)-273.16
          litrq10 = tanhq10(1) + tanhq10(2)*
     &              ( tanh( tanhq10(3)*(tanhq10(4)-tempq10l(i,j))  ) )
c
          q10func = litrq10**(0.1*(litrtemp(i,j)-273.16-15.0))
          ltresveg(i,j)= ltrmoscl(i) * litrmass(i,j)*
     &      bsratelt(sort(j))*2.64*q10func 
!>2.64 converts bsratelt from kg c/kg c.year to u-mol co2/kg c.s
!>respiration from soil c pool
          tempq10s(i,j)=solctemp(i,j)-273.16
          soilcq10= tanhq10(1) + tanhq10(2)*
     &              ( tanh( tanhq10(3)*(tanhq10(4)-tempq10s(i,j))  ) )
c
          q10func = soilcq10**(0.1*(solctemp(i,j)-273.16-15.0))
          scresveg(i,j)= socmoscl(i,j)* soilcmas(i,j)*
     &      bsratesc(sort(j))*2.64*q10func  
!>2.64 converts bsratesc from kg c/kg c.year to u-mol co2/kg c.s
c
         endif
330     continue
320   continue
c
      return
      end

