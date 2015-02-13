      subroutine watrof(isfc,THLIQ,THICE,ZPOND,TPOND,OVRFLW,TOVRFL,
     1                  SUBFLW,TSUBFL,BASFLW,TBASFL,RUNOFF,TRUNOF,FI,
     2                  ZPLIM,XSLOPE,XDRAIN,MANNING_N,DDEN,GRKSAT,TBARW,
     3                  DELZW,THPOR,THLMIN,BI,DODRN,DOVER,DIDRN,
     4                  ISAND,ICLAY,IWF,IWD,IG,ILG,IL1,IL2,BULKFC,
     5                  NA,NTYPE,ILMOS,JLMOS,RATIO)
      USE FLAGS
      IMPLICIT NONE                         
      
************************************************************************************************************
*     WATROF2 *** VERSION 1.2 *** FEBRUARY 13, 2015 *** Ric Soulis *** Mateusz Tinel                       *
************************************************************************************************************
c
c     This routine calculates the outflow from a tilted landscape element (tile). This code is a replacement
c     for WATDRN and WATROF, in contrast the recession curve is a consequence of pore sized distribution, 
c     rather than continuity with depth. Starting from saturation the flow continues at maximum value Qmax 
c     untill the largest pores stop flowing due to suction. Flow continues at near saturation rate, while 
c     the saturation curvature changes from negative curvature to positive. This produces a recession curve 
c     pattern shown below. The curve cab be conveniently represented by Q = min(Qmax, Seffective^Qcof). 
c     Basflow is based on the average saturation of the element, represented by Seffective^Bcof. 

c                    SATURATION A FUNCTION OF TIME
      
c     |*                                                          | - asat saturation 1.0
c     |+*                                                         |
c     |+ *                                                        |
c     |+  *                                                       |
c     | +  *                                                      |
c  S  | +   *                                                     |
c  A  | +    *                                                    |
c  T  |  +    *                                                   |
c  U  |  +     *                                                  | - asat saturation stc, s saturation 1.0
c  R  |   +     **                                                |
c  A  |    +      **                                              |
c  T  |     +       **                                            |
c  I  |      +        **                                          |
c  O  |       +         **                                        |
c  N  |        ++         ***                                     |
c     |          ++          ***                                  |
c     |            +++          ***                               |
c     |               +++++        *****                          |
c     |                    ++++++       *****                     |
c     |                          +++++++++++ *******              | - asat saturation sfc, s saturatoin 0.0
c     |                                                           |
c     |__________________________________________________________ |
c      |               |                                           
c      time 0.0        time tc, s curve time 0.0                                           
c                                TIME 
      
c     Saturated flow is calculated using by a straight line: 
c         asat(t) = 1 - ((1 - stc)/tc)*t 
c     Time t1 at begining of time step can be calculated using this equation. When saturated flow ends at 
c     time = tc, unsaturated flow begins, that is calculated using by: 
c         s(t) = ((B - 1) * (Bmax * t + (1/(B - 1))))**(1/(1 - B))
c     For the curve, at time t = 0 saturation s = 1 everywhere, therefore the time must be corrected for 
c     the time spent on saturated flow which lasted for length of time = tc (so t = t2 - tc), if initial 
c     saturation asat1 happens during saturated flow. If asat1 happens during unsaturated flow then t1 can 
c     be determined using the initial saturation s1. asat can be connected to s using:
c         s = (asat - sfc)/(stc - sfc) since; 
c     if time = tc then asat = stc and the equation gives s = 1, when flow ends then asat = sfc and the 
c     equation gives s = 0. 
      
      
***** COMMON BLOCK PARAMETERS ******************************************************************************
*
      REAL DELT,TFREZ,HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,SPHW,
     1     SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,TCGLAC,CLHMLT,CLHVAP
*
      COMMON /CLASS1/ DELT,TFREZ
      COMMON /CLASS4/ HCPW,HCPICE,HCPSOL,HCPOM,HCPSND,HCPCLY,
     1                SPHW,SPHICE,SPHVEG,SPHAIR,RHOW,RHOICE,
     2                TCGLAC,CLHMLT,CLHVAP
*
***** INTERNAL SCALARS AND VECTORS *************************************************************************
      REAL*8  qflowij , bflowij            !interflow and base flow accumulator
      REAL*8  asat1ij , asat2ij            !initial, and final interflow soil saturation
      REAL*8  asat3ij , asat4ij            !initial, and final baseflow soil saturation
      REAL*8  stc                          !soil saturation at end of pure and apparent saturated flow
      REAL*8  sfc                          !lowes soil saturation possible
      REAL*8  satmin  , satice             !minimal soil saturation and soil saturation with ice
      REAL*8  t1      , t2      , t3  ,t4  !initial, and final interflow and baseflow time
      REAL*8  tc                           !time at end of pure saturated and apparent saturated flow
      REAL*8  s1      , s2      , s3  , s4 !initial, and final interflow and baseflow soil saturation
      REAL*8  qmax    , bmax    , smax     !max flow for interflow, baseflow and overland flow
      REAL*8  qcof    , bcof    , scof     !coefficient for interflow, base flow and overland flow
      REAL*8  xlengthi, xbasei  , xheighti !geometric properties of the tile
      REAL*8  xanglei , xreliefi           !more geometric preperties of tile
      REAL*8  h       , sslopei            !soil depth and slope
      REAL*8  avlspc  , avlflw
      REAL*8  potflw  , actadj             !available and potential flow, and actual flow correction
      real*8  xsatice , vsatice            !mass and volmetric degree of saruationc fo ice
      REAL*8  dover1  , dover2             !initial and final overland flow level
      real*8  dexcess , asatexcess
      REAL*8  asat_extra, avail_depth, basflw_now
*
***** INTERNAL AND INPUT INTEGERS **************************************************************************
      INTEGER IWF, IWD, IG, ILG, IL1, IL2, NA, NTYPE,jj
      INTEGER ILMOS (ILG), JLMOS (ILG)
      INTEGER ISAND (ILG, IG), isandij     !SAND PERCENT 0-100
      integer ICLAY (ILG, IG), iclayij     !CLAY PERCENT 0-100
      INTEGER i                            !counters: i = 1,ilg number of gru elemennts; 
      INTEGER j                            !          j = 1,ig  number of soil layers, 
      integer isfc, iwfstep/0/             !isfc - canopy state,  iwfstep - internal watrof counter  
*
***** INPUT ARRAS AND VECTORS ******************************************************************************
      REAL    BI     (ILG, IG), biij
      REAL    BULKFC (ILG, IG), bulkfcij
      REAL    DELZW  (ILG, IG), delzwij
      REAL    DIDRN  (ILG, IG)
      REAL    GRKSAT (ILG, IG), grksatij   !is mean cross-sectional velocity (m/s) 
      REAL    PSISAT (ILG, IG), psisatij   !soil section TODO - NEED UNITS
      REAL    THICE  (ILG, IG), thiceij
      REAL    THLIQ  (ILG, IG), thliqij
      REAL    THLMIN (ILG, IG), thlminij
      REAL    THPOR  (ILG, IG), thporij    !porosity
      REAL    DDEN       (ILG), ddeni      !drainage density (m/m^2)
      REAL    DODRN      (ILG)
      REAL    DOVER      (ILG)  
      REAL    FI         (ILG), fii  
      REAL    MANNING_N  (ILG), manning_ni
      REAL    TPOND      (ILG), tpondi
      REAL    XDRAIN     (ILG), xdraini    !vertical lateral flow ratio
      REAL    XSLOPE     (ILG), xslopei    !valley slope
      REAL    ZPLIM      (ILG), zplimi
      REAL    ZPOND      (ILG), zpondi
      REAL    RATIO      (ILG), ratioij    
      
***** OUTPUT ARRAYS ****************************************************************************************
      REAL    OVRFLW (ILG), ovrflwi        !overland flow
      REAL    SUBFLW (ILG), subflwi        !interflow
      REAL    BASFLW (ILG), basflwi        !baseflow
      REAL    RUNOFF (ILG)                 !runoff
      
***** UNUSED VARIABLES *************************************************************************************
      REAL    TBARW    (ILG, IG)
      REAL    TOVRFL       (ILG)
      REAL    TSUBFL       (ILG)
      REAL    TBASFL       (ILG)
      REAL    TRUNOF       (ILG)

C----------------------------------------------------------------------C
C     USE FLAGS IWF AND IWD                                            C
C----------------------------------------------------------------------C
      if (IWF.ne.2) then
        print *, "warning: iwf has changed from" , iwf, "to 2"
        iwf = 2
      endif  

c************************************************************************************************************
      do i = 1,ilg
      
C----------------------------------------------------------------------C
C     RESETING THE FLOWS FROM PREVIOUS CALCULATION                     C
C----------------------------------------------------------------------C
      ovrflwi = 0.0
      subflwi = 0.0
      basflwi = 0.0  
      fii = FI(i)
      
      if (fii .gt. 0.0) then
      
C----------------------------------------------------------------------C
C     GEOMETRY OF TILE                                                 C
C----------------------------------------------------------------------C
      ddeni    = DDEN(i)
      xslopei  = xslope(i)    
      sslopei  = atan(xslopei)
      xbasei   = 1.0/2.0/dden(i)
      xanglei  = atan(xslope(i))
      xreliefi = xbasei*(tan(xanglei))
      xlengthi = (xbasei**2+xreliefi**2)**0.5
      
C----------------------------------------------------------------------C
C     HYDRAULICS OF TILE                                               C
C----------------------------------------------------------------------C
      xdraini = XDRAIN(I)
      qcof    = 5.0
      bcof    = 5.0

C----------------------------------------------------------------------C
C     OVERLAND FLOW PARAMETERS                                         C
C----------------------------------------------------------------------C
      zpondi     = ZPOND(i)      
      zplimi     = ZPLIM(i)
      manning_ni = MANNING_N(i)
      tpondi     = tpond(i)
      
************************************************************************************************************
      do j = 1,IG
        
C----------------------------------------------------------------------C
C     RATIO FOR QMAX                                                   C
C----------------------------------------------------------------------C
c     Ratio is split into the three layers, however the average tile  
c     ratio remains the same.
        if     (J.eq.1) then
        ratioij = RATIO(i)
        elseif (J.eq.2) then
        ratioij = RATIO(i)
        else  !(J.eq.3)
        ratioij = RATIO(i)
        endif

C----------------------------------------------------------------------C
C     STATE VARIABLES - VALUSE AT BEGINNING OF TIME STEP               C
C----------------------------------------------------------------------C
        thporij    = THPOR(i,j)
        thliqij    = thliq(i,j)
        thiceij    = THICE(i,j)
        thlminij   = THLMIN(i,j)
        isandij    = isand(i,j)  
            
C----------------------------------------------------------------------C
C     GET NEW LAYER THICKNESS                                          C
C----------------------------------------------------------------------C
        h = DELZW(i,j)
        biij = bi(i,j)
            
C----------------------------------------------------------------------C
C     soil texture and pedotransfer functions                          C
C----------------------------------------------------------------------C
      iclayij  = iclay(i,j)
      bulkfcij = bulkfc(i,j)
      psisatij = 0.01*(10.0**(-0.0131*isandij+1.88))
      grksatij = (1/3600./39.37)*(10.0**(0.0153*isandij-0.884))
************************************************************************************************************
      
C----------------------------------------------------------------------C
C     CLEAR ACCUMULATORS                                               C
C----------------------------------------------------------------------C
        qflowij = 0.0
        bflowij = 0.0
        
C----------------------------------------------------------------------------------------------------------C
C     FIND POTENTIAL LATERAL FLOW                                                                          C
C----------------------------------------------------------------------------------------------------------C
      if(xslopei>0.0.and.isand(i,j)>=0)then
      if (thliqij.gt.bulkfcij .and. biij.gt.1.0) then
          
c         SECONDARY TILE PREPERTIES
         
c         qcof/bcof exponent for recession curve (B), (dimensionless, 4 for 0% clay, 6 for 30% clay)
c         qmax/bmax maximum flux (Bmax)
c         asat      is the actual saturation of the soil (asat1ij, asat2ij)
c         s         is the saturation of soil used for the curve (s1, s2)
c         t1        is the time at begining of the time step
c         t2        is the time at the end of time step
c         tc        end time of pure saturation flow, beginng of a mixture unsaturated flow
c         stc       is bulk saturation when pure saturation flow ceases
c         sfc/bfc   is the lowest possible bulk saturation, as time goes to infinity
c         satice    is the saturation with ice
c         satmin    is the minimum saturation

          xsatice = (rhoice/thiceij)/(rhow*thporij)
          vsatice = thiceij/thporij
          satice  = xsatice
          satmin  = thlminij/thporij
 
          qmax = ratioij*grksatij*xslopei*h/xbasei
c     1           *max(0.0,min(1.0,(1.0-max(vsatice,satmin))))
          stc  = (1.0 - 1.0/(2.0*biij+3.0))
          tc   = (1.0 - stc)/(qmax)
c     1           *(1.0-max(xsatice,satmin))
          sfc  = bulkfcij/thporij

C----------------------------------------------------------------------C
C     beginning of step - find starting storage
C     determine how much flux can exit this time step- units (kg/m**2/s)==mm/s
C----------------------------------------------------------------------C
          asat1ij = max(0.0,min(1.0,thliqij/thporij))
          s1 = (asat1ij - sfc)/(stc-sfc)

C----------------------------------------------------------------------C
C     CASE (SAT - *) - PRIMARY SATURATED FLOW AT THE END OF TIME STEP  C
C----------------------------------------------------------------------C
          if (asat1ij .gt. stc) then
            t1 = tc*(1.0-asat1ij)/(1.0-stc)
            t2 = t1 + delt
            
C----------------------------------------------------------------------C
C     case (sat-sat) - saturated flow at start and end of time step    C
C----------------------------------------------------------------------C
            if (t2 .lt. tc) then
              asat2ij = 1.0-((1.0-stc)/tc)*t2
              
C----------------------------------------------------------------------C
C     case (sat-unsat) - unsaturated flow at end of time step          C
C----------------------------------------------------------------------C
            else
              s2 = ((qcof-1.0)*qmax*(t2-tc)+1.0)**(1.0/(1.0-qcof))
              asat2ij = s2*(stc-sfc)+sfc
            endif

C----------------------------------------------------------------------C
C    case (unsat-unsat) - unsaturated flow at both ends of the timestep
C----------------------------------------------------------------------C
          else 
            t1 = (s1**(1.0-qcof)-1.0)/((qcof-1.0)*(qmax)) + tc
            t2 = t1 + delt
            s2 = ((qcof-1.0)*qmax*(t2-tc)+1.0)**(1.0/(1.0-qcof))
            asat2ij = s2*(stc-sfc)+sfc
          endif
          
          asat2ij  = min(asat1ij, asat2ij)
      
C----------------------------------------------------------------------C
C     FIND POTENTIAL BASEFLOW (draw from bottom layer only)            C
C                                                                      C
C     only case is: (u-u) - unsaturated at start and end of time step  C
C----------------------------------------------------------------------C
              
          bmax = grksatij*xlengthi/xbasei
          s3 = s1
          t3 = (s3**(1.0-bcof)-1.0)/((bcof-1.0)*(bmax))
          s3 = ((bcof-1.0)*bmax*t3+1.0)**(1.0/(1.0-bcof))
          asat3ij = s3*(1.0-sfc)+sfc
          t4 = t3 + delt
          s4 = ((bcof-1.0)*bmax*t4+1.0)**(1.0/(1.0-bcof))
          asat4ij = s4*(1.0-sfc)+sfc
          asat4ij = min(asat3ij,asat4ij)

C----------------------------------------------------------------------C
C     determine how much flux can enter this time step                 C
C     - units (kg/m**2/s)==mm/s                                        C
C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C         available liquid in current layer                            C
C----------------------------------------------------------------------C
          avlflw = asat1ij-max(sfc,satice+satmin)
          avlflw = max(0.0,avlflw)
              
C----------------------------------------------------------------------C
C         max possible outflow given physics of viscous flow           C
C----------------------------------------------------------------------C
          qflowij = (asat1ij-asat2ij)
          bflowij = (asat3ij-asat4ij)        
          potflw  = qflowij + bflowij
          
         if (avlflw .le. 0.0 .or. potflw .le. 0.0) then
           qflowij = 0.0
           bflowij = 0.0
           asat2ij = asat1ij
           asat4ij = asat3ij
         elseif (avlflw .lt.  potflw) then
           actadj  = avlflw/potflw
           qflowij = actadj*qflowij
           bflowij = actadj*bflowij
           asat2ij = asat1ij - qflowij - bflowij
           asat4ij = asat2ij
         else
           asat2ij = asat1ij - qflowij - bflowij
           asat4ij = asat2ij
         endif
         
************************************************************************
C         BASEFLOW FILLING UP NEXT LAYER                                
c         This function takes the available space in the next layer and
c         fills it with baseflow from the first layer. It is limited by
c         the amount of unoccupied space in the soil layer. 

          if ( j.lt.ig ) then
c             This only happens if current layer is not the bottom:
              
              !available sapce for water to flow into in mm
              avail_depth =
     1            max(0.0,thpor(i,j+1)-thice(i,j+1)-thlmin(i,j+1)
     1           -thliq(i,j+1))*delzw(i,j+1)
              !available baseflow in current layer in mm
              basflw_now = (bflowij)*thporij*h
      
              if (basflw_now.gt.avail_depth) then
c                 If the amount of baseflow in the current layer is geater
c                 than the amount of depth available in the next layer:
                  
                  !liquid for the next layer is filled up completly
                  thliq(i,j+1) = thliq(i,j+1) + avail_depth/delzw(i,j+1)
                  
                  !baseflow for current layer is NOT drained completly:
                      basflw_now = basflw_now - avail_depth
                      bflowij    = basflw_now/thporij/h
                      !the rest of baseflow stays in bflowij layer
                      asat4ij = asat3ij - bflowij
              
              else !(basflw_now.le.avail_depth)
c                 If the amount of baseflow in the current layer is less
c                 than or equal to the amaount of depth available in the
c                 next layer:
                  
                  !all current baseflow fills up next layer
                  thliq(i,j+1) = thliq(i,j+1) + basflw_now/delzw(i,j+1)
                  !all baseflow for current layer drained down
                  basflw_now = 0.0
                  bflowij    = 0.0
              
              endif
          endif
      
************************************************************************
         
C----------------------------------------------------------------------C
C        COLLECT TOTALS TO MASTER SOILS                                C
C        all outflow is transferred to baseflow
C----------------------------------------------------------------------C
         
          subflwi = subflwi + qflowij * thporij *h
          basflwi = basflwi + bflowij * thporij *h
          thliq(i,j) = asat2ij * thporij
          
      endif
      endif
      
************************************************************************************************************
      enddo
************************************************************************************************************
      
C----------------------------------------------------------------------C
C     calculate the depth of overland flow                             C
C----------------------------------------------------------------------C
      if (zpondi.gt.zplimi .and. zplimi.ge. 0.0  
     1    .and. manning_ni .gt. 0.0) then
          
        dover1    = zpondi-zplimi
        scof      = -2.0/3.0
        smax      = (2.0*ddeni/manning_ni)*(xslopei**0.5)
        dover2    = (dover1**scof - scof*smax*delt)**(1.0/scof)
        dover2    = max(0.0, min(dover1, dover2))

        zpond(i)  = dover2 + zplimi
        ovrflwi   = dover1 - dover2
        
      endif
      
C----------------------------------------------------------------------C
C     ADD TOTAL AT THE END                                             C
C----------------------------------------------------------------------C
      
      OVRFLW(i) = OVRFLW(i) + fii * ovrflwi
      SUBFLW(i) = SUBFLW(i) + fii * subflwi
      BASFLW(i) = BASFLW(i) + fii * basflwi
      RUNOFF(i) = RUNOFF(i) + ovrflwi + subflwi + basflwi
      
      trunof(i) = RUNOFF(i)*trunof(i) + ovrflwi*tpondi 
     1            + subflwi*tsubfl(i) + basflwi*tbasfl(i)
      trunof(i) = trunof(i)/runoff(i)
      
      endif
      enddo
************************************************************************************************************
      RETURN
      end
      