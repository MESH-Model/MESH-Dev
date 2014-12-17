      subroutine watrof(isfc,THLIQ,THICE,ZPOND,TPOND,OVRFLW,TOVRFL,
     1                  SUBFLW,TSUBFL,BASFLW,TBASFL,RUNOFF,TRUNOF,FI,
     2                  ZPLIM,XSLOPE,XDRAIN,MANNING_N,DDEN,GRKSAT,TBARW,
     3                  DELZW,THPOR,THLMIN,BI,DODRN,DOVER,DIDRN,
     4                  ISAND,IWF,IG,ILG,IL1,IL2,BULKFC,
     5                  NA,NTYPE,ILMOS,JLMOS,SDEPTH,RATIO)
      USE FLAGS
      IMPLICIT NONE                         
      
************************************************************************************************************
*     WATROFTEST *** DRAFT VERSION 1.0 *** DECEMBER 16, 2014 *** Ric Soulis *** Mateusz Tinel              *
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
c  U  |  +     *                                                  | - asat saturation sta, s saturation 1.0
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
c      time 0.0        time ta, s curve time 0.0                                           
c                                TIME 
      
c     Saturated flow is calculated using by a straight line: 
c         asat(t) = 1 - ((1 - sta)/ta)*t 
c     Time t1 at begining of time step can be calculated using this equation. When saturated flow ends at 
c     time = ta, unsaturated flow begins, that is calculated using by: 
c         s(t) = ((B - 1) * (Bmax * t + (1/(B - 1))))**(1/(1 - B))
c     For the curve, at time t = 0 saturation s = 1 everywhere, therefore the time must be corrected for 
c     the time spent on saturated flow which lasted for length of time = ta (so t = t2 - ta), if initial 
c     saturation asat1 happens during saturated flow. If asat1 happens during unsaturated flow then t1 can 
c     be determined using the initial saturation s1. asat can be connected to s using:
c         s = (asat - sfc)/(sta - sfc) since; 
c     if time = ta then asat = sta and the equation gives s = 1, when flow ends then asat = sfc and the 
c     equation gives s = 0. 
      
      
***** COMMON BLOCK PARAMETERS ******************************************************************************
      
      REAL DELT,TFREZ
      
      COMMON /CLASS1/ DELT,TFREZ 
      
***** INTERNAL SCALARS AND VECTORS *************************************************************************
      REAL*8  qflowij , bflowij            !interflow and base flow accumulator
      REAL*8  asat1ij , asat2ij , asat3ij  !initial, and final interflow and baseflow soil saturation
      REAL*8  stc     , sta                !soil saturation at end of pure and apparent saturated flow
      REAL*8  sfc                          !lowes soil saturation possible
      REAL*8  satmin  , satice             !minimal soil saturation and soil saturation with ice
      REAL*8  t1      , t2      , t3       !initial, and final interflow and baseflow time
      REAL*8  tc      , ta                 !time at end of pure saturated and apparent saturated flow
      REAL*8  s1      , s2      , s3       !initial, and final interflow and baseflow soil saturation
      REAL*8  qmax    , bmax    , smax     !max flow for interflow, baseflow and overland flow
      REAL*8  qcof    , bcof    , scof     !coefficient for interflow, base flow and overland flow
      REAL*8  xlengthi, xbasei  , xheighti !geometric properties of the tile
      REAL*8  h       , sslopei            !soil depth and slope
      REAL*8  avlflw  , potflw  , actadj   !available and potential flow, and actual flow correction
      REAL*8  dover1  , dover2             !initial and final overlanf flow level
      
***** INTERNAL AND INPUT INTEGERS **************************************************************************
      INTEGER IWF, IG, ILG, IL1, IL2, NA, NTYPE 
      INTEGER ILMOS (ILG), JLMOS (ILG)
      INTEGER ISAND  (ILG, IG), isandij    !SAND PERCENT 0-100
      INTEGER CLAY   (ILG, IG), clayij     !CLAY PERCENT 0-100
      INTEGER i, j, isfc
      
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
      REAL    RATIO      (ILG), ratioi
      REAL    SDEPTH (NTYPE, IG)
      
***** OUTPUT ARRAYS ****************************************************************************************
      REAL    OVRFLW (ILG), ovrflwi        !overland flow
      REAL    SUBFLW (ILG), subflwi        !interflow
      REAL    BASFLW (ILG), basflwi        !baseflow
      REAL    RUNOFF (ILG)                 !runoff
      
***** UNUSED VARIABLES *************************************************************************************
      REAL    TBARW (ILG, IG)
      REAL    TOVRFL    (ILG)
      REAL    TSUBFL    (ILG)
      REAL    TBASFL    (ILG)
      REAL    TRUNOF    (ILG)
      
C----------------------------------------------------------------------C
C     SKIP IF USING CLASS ONLY                                         C
C----------------------------------------------------------------------C
      if (IWF.eq.0) return
      
************************************************************************************************************
      do i = il1,il2
      
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
      xlengthi = 1.0/(2*ddeni)
      xslopei  = xslope(i)
      sslopei  = atan(xslopei)
      xbasei   = xlengthi * cos(sslopei)
      xheighti = xlengthi * sin(sslopei)
      xdraini  = xdrain(i)
      
C----------------------------------------------------------------------C
C     RATIO FOR Q MAX                                                  C
C----------------------------------------------------------------------C
      ratioi   = RATIO(i)
      
      zpondi     = ZPOND(i)      
      zplimi     = ZPLIM(i)
      manning_ni = MANNING_N(i)
       
************************************************************************************************************
        do j = 1,IG
            
C----------------------------------------------------------------------C
C     CLEAR ACCUMULATORS                                               C
C----------------------------------------------------------------------C
        qflowij = 0.0
        bflowij = 0.0

C----------------------------------------------------------------------C
C     GET LAYER THICKNESS                                              C
C----------------------------------------------------------------------C
        h = DELZW(i,j)
        biij = bi(i,j)

C----------------------------------------------------------------------C
C     STATE VARIABLES - VALUSE AT BEGINNING OF TIME STEP               C
C----------------------------------------------------------------------C
        thlminij   = THLMIN(i,j)
        thliqij    = THLIQ(i,j)
        thporij    = THPOR(i,j)
        thiceij    = THICE(i,j)
        isandij    = isand(i,j)
      
        if(xslopei>0.0.and.isand(i,j)>=0.and.biij>0.0)then

C----------------------------------------------------------------------C
C     soil texture and pedotransfer functions                          C
C     ( clay : 0.0-1.0,isand : 0..100)                                 C
C     (this needs to be confirmed)                                     C
C----------------------------------------------------------------------C
      clayij  = clay(i,j)
      bulkfcij = bulkfc(i,j)
      psisatij = 0.01*(10.0**(-0.0131*isandij+1.88))
      grksatij = (1/3600./39.37)*(10.0**(0.0153*isandij-0.884))
              
************************************************************************************************************
C----------------------------------------------------------------------------------------------------------C
C     FIND POTENTIAL LATERAL FLOW                                                                          C
C----------------------------------------------------------------------------------------------------------C
      if (thliqij.gt.bulkfcij.and.biij.gt.1.0) then
         
c         SECONDARY TILE PREPERTIES
         
c         qcof/bcof exponent for recession curve (B), (dimensionless, 4 for 0% clay, 6 for 30% clay)
c         qmax/bmax maximum flux (Bmax)
c         asat      is the actual saturation of the soil (asat1ij, asat2ij)
c         s         is the saturation of soil used for the curve (s1, s2)
c         t1        is the time at begining of the time step
c         t2        is the time at the end of time step
c         tc        end time of pure saturation flow, beginng of a mixture unsaturated flow
c         ta        end of apparent saturation flow, beginning of sustantial unsaturated flow
c         stc       is bulk saturation when pure saturation flow ceases
c         sta       is bulk saturation when apparent saturation flow ceases
c         sfc/bfc   is the lowest possible bulk saturation, as time goes to infinity
c         satice    is the saturation with ice
c         satmin    is the minimum saturation
 
          qmax    = ratioi*grksatij*xslopei/xlengthi
          qcof    = (biij+2.0)/2.0
          stc     = (1.0 - 1.0/(2.0*biij+3.0))
          tc      = (1.0 - stc)/qmax
          ta      = 2.0*tc
          sta     = 2.0*stc - 1.0
          sfc     = bulkfcij/thporij
          asat1ij = thliqij/thporij
          satice  = thiceij/thporij
          satmin  = thlminij/thporij
          s1 = (asat1ij - sfc)/(sta-sfc)

C----------------------------------------------------------------------C
C     beginning of step - find starting storage
C     determine how much flux can exit this time step- units (kg/m**2/s)==mm/s
C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C     CASE (SAT - *) - PRIMARY SATURATED FLOW AT THE END OF TIME STEP  C
C----------------------------------------------------------------------C
          if (asat1ij .ge. sta) then
            t1 = ta*(1.0-asat1ij)/(1.0-sta)
            t2 = t1 + delt
            t3 = t2
            
C----------------------------------------------------------------------C
C     case (sat-sat) - saturated flow at start and end of time step    C
C----------------------------------------------------------------------C
            if (t2 .le. ta) then
              asat2ij = 1.0-((1.0-sta)/ta)*t2
              
C----------------------------------------------------------------------C
C     case (sat-unsat) - unsaturated flow at end of time step          C
C----------------------------------------------------------------------C
            else
              s2 = ((qcof-1.0)*qmax*(t2-ta)+1.0)**(1.0/(1.0-qcof))
              asat2ij = s2*(sta-sfc)+sfc
            endif

C----------------------------------------------------------------------C
C    case (unsat-unsat) - unsaturated flow at both ends of the timestep
C----------------------------------------------------------------------C
          else 
            t1 = (s1**(1.0-qcof)-1.0)/((qcof-1.0)*(qmax))
            t2 = t1 + delt
            t3 = t2 + ta
            s2 = ((qcof-1.0)*qmax*t2+1.0)**(1.0/(1.0-qcof))
            asat2ij = s2*(sta-sfc)+sfc
          endif
          
          asat2ij  = min(asat1ij, max(asat2ij, satmin))
          
          
C----------------------------------------------------------------------C
C     FIND POTENTIAL BASEFLOW (draw from bottom layer only)            C
C----------------------------------------------------------------------C
         if (j.lt.ig) then
             
            asat3ij = asat1ij
            
C----------------------------------------------------------------------C
C     only case is: (u-u) - unsaturated at start and end of time step  C
C----------------------------------------------------------------------C
         else 
            bmax    = grksatij
            bcof    = (biij+2.0)
            bcof = 6.0 !TESTING
            s3      = ((bcof-1.0)*bmax*t2+1.0)**(1.0/(1.0-bcof))
            asat3ij = s3*(1.0-sfc)+sfc
            asat3ij  = min(asat1ij, max(asat3ij, satmin))
         endif
         
C----------------------------------------------------------------------C
C     determine how much flux can exit this time step                  C
C     - units (kg/m**2/s)==mm/s                                        C
C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C         available liquid water                                       C
C----------------------------------------------------------------------C
          avlflw = asat1ij - max(satmin,sfc,satice)
          avlflw = max(0.0,min(1.0,avlflw))
      
C----------------------------------------------------------------------C
C         max possible outflow given physics of viscous flow           C
C----------------------------------------------------------------------C
          qflowij = (asat1ij-asat2ij) 
          bflowij = (asat1ij-asat3ij)
          potflw  = qflowij + bflowij
      
         if (avlflw .le. 0.0) then
           qflowij = 0.0
           bflowij = 0.0
           asat2ij = asat1ij
         elseif (avlflw .le.  potflw) then
           actadj  = avlflw/potflw 
           qflowij = actadj*qflowij
           bflowij = actadj*bflowij*(1.0-xdraini)
           asat2ij = asat1ij - qflowij - bflowij  
         else
           asat2ij = asat1ij - qflowij - bflowij  
         endif
         
C----------------------------------------------------------------------C
C        collect and add totals to master silos                        C
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

      if(zpondi.gt.zplimi .and. zplimi.ge. 0.0  
     1    .and. manning_ni .gt. 0.0) then
        dover1    = zpondi-zplimi
        scof      = -2.0/3.0     
        smax      = (1.0/manning_ni)*(xslopei**0.5)
        dover2    = (dover1**scof+ smax*delt)**(1.0/scof)
        dover2    = max(0.0, min(dover1, dover2))

        zpond(i)  = dover2 + zplimi
        ovrflwi   = dover1 - dover2
      endif

      endif
      OVRFLW(i) = OVRFLW(i) + fii*ovrflwi
      SUBFLW(i) = SUBFLW(i) + fii*subflwi
      BASFLW(i) = BASFLW(i) + fii*basflwi
      RUNOFF(i) = RUNOFF(i) + ovrflwi + subflwi + basflwi

      enddo
************************************************************************************************************
      
      RETURN
      end