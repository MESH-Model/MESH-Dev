      SUBROUTINE WATDRN2(i,j,ASAT1,ASAT2,KSAT,GRKEFF,ISAND,
     1        runoff,ovrflw,SUBFLW,BASFLW,
     2        trunof,tovrfl,tsubfl,tbasfl,      
     2        IG,NA,NTYPE,ILG,IL1,IL2,
     3        bi,fi,satfc,thliq,thpor)
c     3        BCAP,DCOEFF,BFCAP,BFCOEFF,bi,satfc)
c
c     * December 4, 2009, Vincent Fortin
c
c       Initial WATDRN code derived from WAT_DRAIN by Ric Soulis
c       Code entirely rewritten for three main reasons:
c       - simplify the parameterization by working with average
c         hydraulic conductivity within a layer (runs faster, but
c         more importantly is easier to understand, for users)
c       - meet CCC and RPN soufrce code standards
c       - improve readability
c
c     * December 11, 2009, Vincent Fortin
c
c       - Define grkeff as ksat * (tile slope / tile length) (1/s)
c         (take out thpor, as thpor changes with ice content)
c       - Modify computation of critical time accordingly:
c         tc = thpor / (c*grkeff)
c
c     * March 22, 2010, Vincent Fortin
c
c       - Stop computing t0 and t1 to avoid overflow, compute tc/t
c         directly when t>tc and t/tc when t<tc (where t=t0 or t1)
c       - Avoid dividing by tc to make the code more robust
c         when tc is small
c       - Simplify calculation of baseflow rate
c       - Bugfix: move satsfc calculation after computation of tc/t0
c       - Change variable name from thpor to thpora
c
c     Summary:
c
c     This routine calculates the outflow from a tilted landscape
c     element (tile). Rather than using average saturation for the 
c     element(asat0), it finds flow using the saturation at the
c     seepage face (for interflow) at at the bottom of the layer
c     (for baseflow). The routine returns:
c     1) the saturation at the end of the time step (asat1)
c     2) the interflow (subflw) and baseflow (basflw) integrated 
c        over the duration of the time step (in m)
c     3) the fraction of the surface which is saturated at the
c        surface according to the model (satsfc)
c
c     Interflow amount during the time step in m is determined from:
c     subflw = (asat0 - asat1) * thpor * delzw
c     (implicit estimation)
c
c     Baseflow amount during the time step in m is determined from:
c     basflw = ksat * asatb0**(2*bcoef+3) * delt
c     where asatb0 is the average saturation across the bottom of the
c     element at initial time (explicit estimation, 1st order)
c
c     The fraction of the surface that is saturated at the initial
c     time (satsfc) is estimated as MIN(1-t0/tc,0) where t0 is the 
c     (theoretical) time elapsed since saturation and tc is the
c     critical time at which the saturation front reaches the
c     seepage face.
c
c     Background info on WATDRN:
c
c     WATDRN is a parameterization for sub-grid scale interflow,
c     i.e. subsurface flow, which is thought to be an important flow
c     generation process following a rain event at the basin scale
c
c     The underlying principles behind WATDRN are described in
c     a paper by Soulis et al. (2000), Atmosphere-Ocean.
c     However, this code presents a simpler solution to the problem
c     which assumes that hydraulic conductivity is constant within
c     a soil layer. This is a departure from previous versions of
c     WATDRN aimed at making the code easier to understand
c     and faster to run.
c
c     Here is the basic idea: interflow is especially important
c     after a rain event which leaves the soil close to saturation.
c     Following such a rain event, water flows from the hillslope
c     to the river which creates a gradient in water content along
c     the hillslope. But land-surface models have generally as their
c     prognostic variable the mean water content of the grid box.
c     Under the hypothesis of Darcian flow along the hillslope
c     following a rain event which left the soil saturated, WATDRN
c     recovers the saturation distribution along the hillslope
c     from the bulk saturation, estimates from this the interflow
c     rate at the seepage face and integrates this rate over the
c     duration of the time step.
c
c     Given a bulk saturation value asat_t0 at the start of the
c     time step, and assuming that this bulk saturation is the
c     result of Darcian flow in the direction of the hillslope
c     starting from a saturated soil (with no rain after that),
c     there is a one-to-one relationship between bulk saturation
c     and the time elapsed since the rain stopped. So we can figure
c     out:
c
c     1. t0, the (theoretical) time elapsed since the soil was
c        saturated, knowing the bulk saturation asat0.
c     2. t1, the (theoretical) time at the end of the time step
c        (that's just t0+delt)
c     3. asat1, the bulk saturation at the end of the time step
c
c     Then interflow is proportional to the difference between
c     asat0 and asat1.
c
c     From bulk saturation, it is also possible to figure out
c     average saturation across the bottom of the element, which
c     is used to estimate baseflow, and the fraction of the surface
c     that is saturated, which can be used to separate runoff and
c     infiltration.
c
      IMPLICIT NONE  
c*
c     Input parameters
      INTEGER ilg         ! Size of arrays
      INTEGER il1         ! index of first grid point to process
      INTEGER il2         ! index of last grid point to process
      REAL    delt        ! duration of the time step (s)
c*
c     Input arrays
      REAL    dden(ilg)     ! drainage density (m/m)
      REAL    thpora(ilg,ig)  ! porosity
      REAL    thpor(ilg,ig)  ! porosity
      REAL    xslope(ilg) ! valley slope
      REAL    fi(ilg) ! valley slope
      REAL    delzw(ilg,ig)  ! layer thickness (m)
c      REAL    b(ilg,ig)     ! slope of retention curve, related to
                          ! Clapp-Hornberger connextivity index by c=2b+3 
      REAL    thporaj(ilg) ! Available porosity of the soil layer
                          ! (total porosity - ice content)
      REAL    psisat(ilg,ig) ! soil section TODO - NEED UNITS
      REAL    ksat(ilg,ig) ! Vertical hydraulic conductivity at saturation
                          ! at the bottom of the layer (m/s)
      REAL    grkeff(ilg,ig) ! average value of the parameter
                          ! controlling the time scale of interflow process
                          ! ksat * (tile slope / tile length) (1/s)
      REAL    asat1  ! bulk saturation at initial time
      real    bsat0(ilg)
      REAL    TC
      REAL    C(ilg,IG)
      REAL    RATIOT(ILG)
      REAL    ratiotb(ILG)
      REAL    ratios(ilg) 
      integer isand(ILG,IG) !SAND PERCENT
      real    sandfrac(ig)

c     Output arrays
      REAL    asat2  ! bulk saturation at the end of the time step
      real    bsat1
      real    bsat2
      real    bsatc

      real    runoff(ilg)
      real    ovrflw(ilg)
      REAL    subflw(ilg) ! interflow amount during the time step (m)
      REAL    basflw(ilg) ! baseflow rate during the time step (m)
      REAL    satsfc(ilg) ! saturatbed fraction of the surface (0 to 1)
      real    satfc(ilg,ig)  ! bulk saturation
*
c     Work 
      REAL    cm1(ilg,ig)    ! c-1
      REAL    c2m1(ilg,ig)   ! 2*c-1
      REAL    asatc  ! bulk saturation at the critical time tc
      REAL    bcof   ! kinematic recession constant
      REAL    dcof   ! kinematic recession constant
      REAL    btc   ! kinematic recession constant
      REAL    bfc   ! kinematic recession constant
      real    sslope,tcp,tp1,xbase,xheight,xlength
      real    t,t0,t1,t2

c      real    BCAP,DCOEFF,BFCAP,BFCOEFF, BFMIN,BQMAX
      REAL, DIMENSION(ilg) :: tovrfl,trunof,tbasfl,tsubfl        
      LOGICAL satspf(ilg,ig) ! indicates if seepage face is saturated
                          ! equivalent to knowing if t<=tc
      REAL, DIMENSION(ilg,ig) ::bi,thliq
      integer  IG,NA,NTYPE,N

c
c     Local variables
c      REAL    bfc         ! Bulk Field Capacity
c      REAL    btc         ! Bulk Saturation When Unsaturated Flow Begins
      real dels,bij
      real t0p
      real t1p
      real t2p
      real deltp
      INTEGER i,j           ! Array index

      real dover(Ilg),didrn(ilg,ig),dodrn(ilg)

c      print*,"HELLO"
*
c**********************************************************************
c     STEP 0: Initialize a few things before we start
c             - output variables
c             - functions of the input parameters
c**********************************************************************
*                                                 
c      DO i=il1,il2
      
          sandfrac(j) =isand(i,j)/100.0
c         sandfrac = sandfrac
c        c and c factors
cccc     
         bij = bi(i,j)
         c(i,j)    = 2.*bij+3.
         cm1(i,j)  = c(i,j)-1.
         c2m1(i,j) = 2.*c(i,j)-1.
c        RIC 26/03/14 - kinematic recession constant
         PSISAT(i,j)= EXP(-0.0302*sandfrac(j)+4.33)
         KSAT(i,j) = 7.0556E-6*(EXP(0.0352*sandfrac(j)-2.035))
c         print*, "sandfrac,psisat(i,j),ksat(i,j)"
c         print*, sandfrac,psisat(i,j),ksat(i,j)
c         pause  "102"
         xlength = 1.0/(2.0*dden(i))
         sslope = atan(xslope(i))
         xbase  = xlength * cos(sslope)
         xheight= xlength * sin(sslope)
         grkeff(i,j)=ksat(i,j)*sslope*delzw(i,j)/(2.0*dden(i))
         bij=(c(i,j)-3.0)/2.0
         btc = 1.0 - 1.0/c(i,j)
         bcof=(bij+2)/2
         dcof=(bij+2)/2
         IF(XSLOPE(i).le.0.0)then
             asat2=0.04
         else    
 
           bfc= (1.0/(bij-1.0))*
     +       ((-psisat(i,j)*bij/xheight)**(1.0/bij))*
     +       ((3.0*bij+2.0)**((bij-1.0)/bij)-
     +       (2.0*bij+2.0)**((bij-1.0)/bij))
c         bfc = max(0.0,min(bfc,1.0-2.0*(1.0-btc)))
            bfc = max(0.04,min(bfc,1.0-2.0/c(i,j)))
      print*, btc,bfc,psisat(i,j),grkeff(i,j),bcof            
      if (grkeff(i,j).lt.1.0e-12)then
           asat2=0.04
       else
c          pause
c        bulk saturation at critical time
c        (just before the seepage face becomes unsaturated)
c         asatc = 1.-1./c(i)
c        layer average saturation asat0 may be greater than 1
c        e.g. frost heave but it is not possible for wat_drain
         asat1 = MIN(1.,asat1)
c        assess if seepage face is saturated at initial time
         satspf(i,j) = asat1 .GE. satfc(i,j)
 
c**********************************************************************
c     STEP 1: Find theoretical time t0 elapsed since element was last
c             saturated and estimate baseflow rate at initial time
c             Also estimate fraction of surface that is saturated
c**********************************************************************
*
c        determine time at which seepage face becomes unsaturated
c         tc(i) = thporaj(i)/(c(i)*grkeff(i))
         tc = 1.0/(c(i,j)/grkeff(i,j))
         bsatc = 1-1/c(i,j)
         t0p=0.0
         tcp=1.0
         deltp=delt/tc

c        find theoretical start of recession (t0=0) from bulk saturation
ciZZZ
         bsat1 = asat1
          t1p=(asat1-bsatc)/(1-bsatc)
          t2p=t1p+deltp

c         saturated seepage face at initial time
          if (t1p .lt. 1) then
        
c           saturated seepage face at end time
            if (t2p .lt.1) then
              asat2  = 1-t2p/c(i,j)
c           unsaturated seepage face at  end time
            else
              tp1 = 1/(1.-bcof)/(btc-bfc)/grkeff(i,j)/c(i,j)
              bsat2  = ((tp1-1)/(tp1 - 1+deltp))**(1/(1-bcof))
              asat2 = bfc+(btc-bfc)*bsat2
            endif

c         unsaturated seepage face at initial time:
          else
      tp1 = (1 - bsat1**(1-bcof))/(1.-bcof)/(btc-bfc)/grkeff(i,j)/c(i,j)
            bsat2  = bsat1*((tp1)/(tp1+deltp))**(1/(1-bcof))
            asat2 = bfc+(btc-bfc)*bsat2

          endif
c      IF (j) THEN
c         print *, "i,j,asat1(i,j),asat2,asat1-asat2j)"
c         print *,  i,asat1,asat2(i,j),asat1- asat2
c         pause
c      ENDIF   
      endif
      endif
c       sanity check: bulk saturation should not increase with time
c        obtain interflow from the difference in bulk saturation
        subflw(i)=subflw(i)+
     1 (asat2-asat1)*thpor(i,j)*delzw(i,j)
         asat2=max(0.04,min(asat1,asat2) )
c         print*, "subflw(i)", subflw(i)
c      ENDDO
      

      RETURN
      END