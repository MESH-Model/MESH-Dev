      SUBROUTINE WATROF(THLIQ,THICE,ZPOND,TPOND,OVRFLW,TOVRFL,
     1                  SUBFLW,TSUBFL,basflw,tbasfl,RUNOFF,TRUNOF, 
     1                  FI,ZPLIM,igrd,igrn,
     2                  XSLOPE,xdrain,MANNING_N,DDen,KSAT,TBARW,
     3                  DELZW,THPOR,THLMIN,BI,DODRN,DOVER,DIDRN,
     4                  ISAND,IWF,IG,ILG,IL1,IL2,BULKFC,satfc,
c*ADDED FOR WATDRN3
     6                  NA,NTYPE,ILMOS,JLMOS,
c     7                  BTC,BCAP,DCOEFF,BFCAP,BFCOEFF,grkeff,
     7                  grkeff,
c*added to be consistent with grdra
     8                  THLMAX,
     3                  PSISAT,THFC,LZF,
     4                  IGP1,IGP2,JL,N)
 
                      

C     * JUN 03/11 - D.PRINCZ. FOR RIC'S WATDRN3. ADDED USE FLAGS.
C     * MAR 03/10 - M.A.MEKONNEN/B.DAVISON/M.MACDONALD
C     *             RE-WRITTEN FOR TWO REASONS:
C     *             -TO USE VINCENT'S VERSION OF WATDRN; 
C     *             -TO INCLUDE MORE COMMENTS.
C     * SEP 16/06 - R.SOULIS/F.SEGLENIEKS/A.PIETRONIRO/B.DAVISON.
C     *             MODIFICATIONS TO OVERLAND FLOW.
C     * SEP 15/05 - D.VERSEGHY. REMOVE HARD CODING OF IG=3.
C     * MAR 30/05 - D.VERSEGHY. ADDITIONAL FIELDS.
C     * NOV 03/04 - D.VERSEGHY. ADD "IMPLICIT NONE" COMMAND.
C     * AUG 02/02 - R.SOULIS/D.VERSEGHY. UPDATES DEVELOPED AT   e
C     *             WATERLOO.
C     * DEC 10/01 - R.SOULIS/K.SNELGROVE/T.WHIDDEN/D.VERSEGHY 
C     *             WATFLOOD ROUTINE TO CALCULATE OVERLAND FLOW AND
C     *             INTERFLOW COMPONENTS OF SURFACE RUNOFF.
C

C------------------ADDITIONAL CLARIFICATION-----------------------------------------------
C     
C     FOR A SLOPING ELEMENT, WATROF COMPUTES:
C         1. OVERLAND FLOW AT THE SURFACE BASED ON THE AVAILABLE PONDING DEPTH;
C         2. LATERAL FLOW FROM EACH LAYER BASED ON THE AVAILABLE WATER IN EACH SOIL LAYER. 
C          
C     THE BASE FLOW FROM EACH LAYER IS ALSO COMPUTED BUT IT IS NOT USED AT PRESENT. SO, 
C     THE TOTAL BASEFLOW REMAINS THE SAME AS THE ONE CALCULATED IN CLASS.
C
C-----------------------------------------------------------------------------------------
C     DEFINITIONS
C     IWF         - FLAG GOVERNING OVERLAND AND LATERAL FLOW CALCULATIONS
C                   0 REPRESENTS FLAT ELEMENT - CLASS CALCULATES OVERLAND AND LATERAL FLOW
C                   NE 0 REPRESENTS SLOPING ELEMENT - WATROF CALCULATES OVERLAND AND LATERAL FLOW
C     IG          - TOTAL NUMBER OF SOIL LAYERS
C     ILG         - TOTAL NUMBER OF ELEMENTS
C     IL1         - STARTING INDEX OF ACTIVE ELEMENT
C     IL2         - FINAL INDEX OF ACTIVE ELEMENT
C     THLIQ       - VOLUMETRIC LIQUID WATER CONTENT OF SOIL LAYERS
C     THICE       - VOLUMETRIC FROZEN WATER CONTENT OF SOIL LAYERS
C     FI          - FRACTIONAL COVERAGE OF SUBAREA IN QUESTION ON MODELLED AREA
C     ZPLIM       - SUBAREA MAXIMUM PONDING DEPTH
C     XSLOPE      - SURFACE SLOPE
C     GRKFAC      - WATROF PARAMETER USED WHEN RUNNING MESH CODE ? NEEDS MORE CLARIFICATION
C     WFCINT      - WATROF PARAMETER USED WHEN RUNNING MESH CODE ? NEEDS MORE CLARIFICATION
C     TBARW       - TEMPERATURE OF WATER IN SOIL LAYER
C     ZPOND       - DEPTH OF PONDED WATER ON SURFACE
C     TPOND       - SUBAREA TEMPERATURE OF SURFACE PONDED WATER
C     OVRFLW      - OVERLAND FLOW FROM TOP OF SOIL COLUMN
C     TOVRFL      - TEMPERATURE OF OVERLAND FLOW
C     SUBFLW      - INTERFLOW FROM SIDES OF SOIL COLUMN
C     TSUBFL      - TEMPERATURE OF INTERFLOW FROM SIDES OF SOIL COLUMN
C     RUNOFF      - TOTAL RUNOFF
C     TRUNOF      - TEMPERATURE OF TOTAL RUNOFF
C     DELZW       - PERMEABLE THICKNESS OF SOIL LAYER
C     THPOR       - PORE VOLUME IN SOIL LAYER
C     THLMIN      - RESIDUAL SOIL LIQUID WATER CONTENT REMAINING AFTER FREEZING OR EVAPORATION
C     PSISAT      - SOIL MOISTURE SUCTION AT SATURATION
C     BI          - CLAPP AND HORNBERGER EMPIRICAL “B” PARAMETER
C     ISAND       - SAND CONTENT FLAG ? NEEDS MORE CLARIFICATION FOR THE VALUES
C     bulkfc     - BULK FIELD CAPACITY
C     DELT        - TIME STEP
C     TFREZ       - FREEZING POINT OF WATER
C     MANNING_N   - MANNING'S ROUGHNESS COEFFICIENT
C     DDen          - DRAINAGE DENSITY
C     ASAT1     - BULK SATURATION AT INITIAL TIME
c     ASAT2     - BULK SATURATION AT FINAL TIME
c     ASATC     - BULK SATRUATION WHEN FLOW EFFECTIVELY STOPS (i.e. FIELD CAPACITY)
      USE FLAGS
C                              
      IMPLICIT NONE
      
c      * COMMON BLOCK PARAMETERS.
C
      REAL DELT,TFREZ
C
      COMMON /CLASS1/ DELT,TFREZ
      
C     * INTERNAL SCALARS AND VECTORS
C
      INTEGER  NC,NA,NTYPE,ig,il1,il2,ilg,i,j,igp1,igp2,iwf,jl,n
      integer lzf(ilg)
      real    fi(ilg)
      REAL    dden(ilg)     ! drainage density (m/m)
      REAL    thpor(ilg,ig)  ! porosity
      REAL    thpora! porosity
      REAL    xslope(ilg) ! valley slope
      REAL    delzw(ilg,ig)  ! layer thickness (m c      REAL    bcoef(ilg)  ! slope of retention curve, related to
                          ! Clapp-Hornberger connextivity index by c=2b+3 
      REAL    psisat(ilg,ig) ! soil section TODO - NEED UNITS
      REAL    ksat(ilg,ig)   ! Vertical hydraulic conductivity at saturation
                          ! at the bottom of the layer (m/s)
      REAL    grkeff(ilg,ig) ! average value of the parameter
                          ! controlling the time scale of interflow process
                          ! ksat * (tile slope / tile length) (1/s)
      REAL    asat1  ! bulk saturation at initial time
      REAL    tc(ILG)
      REAL    c(ilg,ig)
      REAL    bi(ILG,ig)

      REAL     RATIOT(ILG)
      REAL     ratiotb(ILG)
      integer  isand(ILG,IG) !SAND PERCENT 0-100
      
c     Output arrays
      REAL    asat2  ! bulk saturation at the end of the time step
      real    runoff(ilg)
      real    ovrflw(ilg)
      REAL    subflw(ilg) ! interflow amount during the time step (m)
      real    zpond(ilg) 
c     Work arrays
      REAL    cm1(ilg,ig)    ! c-1
      REAL    c2m1(ilg,ig)   ! 2*c-1
      REAL    bcof(ilg)   ! kinematic recession constant

      LOGICAL satspf(ilg) ! indicates if seepage face is saturated
                          ! equivalent to knowing if t<=tc

c     Local variables                     
      REAL    bfc         ! Bulk Field Capacity
      REAL    btc         ! Bulk Saturation When Unsaturated Flow Begins
      REAL    asatc
      
C     * WATDRN3  
      INTEGER, DIMENSION(ILG) :: ILMOS,JLMOS,igrd,igrn
      real, dimension(ilg) :: tovrfl,tsubfl,basflw,tbasfl,tpond,trunof
      real  DCOEFF,BFCAP,H,fkl,exav
       REAL, DIMENSION(ilg,IG) :: thliq,thice,bulkfc,
     1    satfc,tbarw,thfc,thlmax,thlmin
      REAL VEL_T0(ILG),NUC_DOVER(ILG),MANNING_N(ILG),
     1    zplim(ilg),thliq_avail,bfcoeff,
     2    SATSFC(ILG),thplim(ilg,ig),
     3     DAVAIL,DTOT,bcap,
     4     XLAMBDA,ktop,kl,h0,c1,c2,rk1,
     +     ztop(ilg,ig),dover(ilg),
     =     didrn(ilg,ig),dodrn(ilg),xlength,xdrain(ilg)
        
c       integer n
c      return
C-----------------------------------------------------------------------------------------
C     coefficients
      c1 = 2.0/3.0
      c2 = 1.5 !3.0/2.0

C-----------------------------------------------------------------------------------------
C     parameter - will be used to compute xdrain (the fractional change in horizontal 
C     conductivity in a depth change h0) in Vincent's new formula.
      H = 2.0

C-----------------------------------------------------------------------------------------
C     skip if using flat class 
c      if(iwf.eq.0)return
      
C-----------------------------------------------------------------------------------------
C     loop through each element
 

        do i = il1,il2
c              PRINT* ,"watrof",i

            xlength=1.0/(2*dden(i))
C        ---------------------------------------------------------------------------------
C        compute overland flow and add to 
c and to the overall overland flow
C        ---------------------------------------------------------------------------------
c          if(.false. .and. igrd(i).gt.0) then              
           if(igrd(i).gt.0) then              

           if(fi(i) .gt. 0.0) then
              
           if(zpond(i) .gt. zplim(i))then

C           ------------------------------------------------------------------------------
C           calculate the depth of water available for overland flow
C           ------------------------------------------------------------------------------
            dover(i) = zpond(i)-zplim(i)

C           ------------------------------------------------------------------------------
C           calculate the flow velocity at the beginning of the timestep 
C           (based on kinematic wave velocity) - eqn (1) in notes on overland flow
C           ------------------------------------------------------------------------------
c            vel_t0(i) = max(0.0,vel_t0(i)*delt*2*dden(i))
            vel_t0(i) = (dover(i)**c1)*sqrt(xslope(i))/(manning_n(i))
            
C           ------------------------------------------------------------------------------
C           calculate a normalized unconstrained overland flow to avoid numerical 
C           problems with a division of small dover(i) values. 
c           eqn (29) in notes on overland flow
C           ------------------------------------------------------------------------------
            nuc_dover(i) = -2.0*dden(i)*vel_t0(i)*delt

C           ------------------------------------------------------------------------------
C           constrained overland flow - limited by physically possible flow.
C           eqn (30) in notes on overland flow
C           ------------------------------------------------------------------------------
            dodrn(i) = dover(i)*(1.0-1./((1.0-c1*nuc_dover(i))**c2))
C           ------------------------------------------------------------------------------
C           add overland flow to runoff and to the overall overland flow
C           ------------------------------------------------------------------------------
            if(dodrn(i) .gt. .0) then
c              trunof(i) = (trunof(i)*runoff(i)+(tpond(i)+tfrez)*
c    1                      dodrn(i))/(runoff(i)+dodrn(i))
             runoff(i)    = dodrn(i)/xlength
            
c               tovrfl(i) = (tovrfl(i)*ovrflw(i)+(tpond(i)+tfrez)*
c    1              fi(i)*dodrn(i))/(ovrflw(i)+fi(i)*dodrn(j))
               ovrflw(i) = fi(i)*dodrn(i)/xlength

C           ---------------------------------------------------------------------------
C           subtract overland flow depth from the ponding depth
C           ---------------------------------------------------------------------------
          
c            zpond(i)  = max(zplim(i),zpond(i)- dodrn(i))
            zpond(i)  = max(zplim(i),zpond(i)- dodrn(i))

          endif
        endif
        endif
       
C-----------------------------------------------------------------------------------------
C     compute interflow flow from each layer   
C-----------------------------------------------------------------------------------------
C-----------------------------------------------------------------------------------------
C     loop through each soil layer 
c-------------------------------------------------------------------------------------

C        ---------------------------------------------------------------------------------
C        loop through each element  
             
      do j = 1,ig
c          print*,i,j


        if( isand(i,j) .ge. -2)then
          if (delzw(i,j) .gt. 0.0)then 

C        ---------------------------------------------------------------------------
C        determine available liquidwater in layer
C         ---------------------------------------------------------------------------

         thliq_avail = min(thliq(i,j)
     1    ,max(0.0, thliq(i,j)-satfc(i,j)*thpor(i,j)))

C          ---------------------------------------------------------------------------
C          determine available porosity
C          ---------------------------------------------------------------------------

        thpora = min(thpor(i,j)*(1.0-2/c(i,j)),
     1     thpor(i,j)-satfc(i,j)*thpor(i,j))

C      ---------------------------------------------------------------------------
C       saturation defined as liquid water content over porosity
C       ---------------------------------------------------------------------------
             
            asat1 = thliq(i,j)/thpor(i,j)
            asat2 = min(asat1,1.0)
            asatc = thpora/thpor(i,j)
            
C           ------------------------------------------------------------------------------
C           grkeff - average value of the parameter controlling the time scale of 
C                    interflow process - kl * (tile slope / tile length) (1/s)
C           Note: this formula is not the same as the one in Fhydro2_VF_20100226.f
C                 and needs to be confirmed by Vincent.
C           ------------------------------------------------------------------------------
c*       Integration of k across the layer -> kl
            xlambda   = -log(xdrain(i))/H
c            xlambda   = -log(0.1)/H
            ktop      = ksat(i,j)*exp(xlambda*ztop(i,j))
            fkl        = ktop * exav(xlambda*delzw(i,j))

c         frk1        = ksat(i,j)
            
            
            xlength=1/(2*dden(i))
            grkeff(i,j)=
     1        ksat(i,j)*xslope(i)/(1+xslope(i)**2.0)
     2        *delzw(i,j)*delt/xlength
                  

C        ---------------------------------------------------------------------------------
C        compute interflow from the layer (subflwj). Baseflow from the layer (basflwj) is
C        also computed but is not used at present.
C        ---------------------------------------------------------------------------------


       wd3=2
       if (wd3== 0)then
          thliq(i,j) = max(satfc(i,j)*thpor(i,j)
     2     ,0.99*thliq(i,j))
       endif          
       IF (wd3 == 2) THEN
           call watdrn2 (i,j,ASAT1,ASAT2,KSAT,GRKEFF,ISAND,
     1      runoff,ovrflw,SUBFLW,BASFLW,                             
     2      trunof,tovrfl,tsubfl,tbasfl,
     1      IG,NA,NTYPE,ILG,IL1,IL2,
     3      bi,fi,satfc,thliq,thpor)
       ENDIF

       IF (wd3 == 3) then
         CALL WATDRN3 (i,j,ASAT1,ASAT2,KSAT,GRKEFF,ISAND,
     1     runoff,ovrflw,SUBFLW,BASFLW,      
     2     trunof,tovrfl,tsubfl,tbasfl,
     1     IG,NA,NTYPE,ILG,IL1,IL2,
     3     bi,fi,satfc,thliq,thpor)
       endif
      
       endif
      endif
             
      enddo      
      endif
          

      enddo
c       pause                  
      return
      end
c      
c**********************************************************************
c
c     function exav(x)
c     finds average of an exponential function exp(-x)
c     over the interval [0,x], which is (1-exp(-x))/x
c     deals with limit values of 1-x/2 when x->0 and 1/x when x->inf
c
c**********************************************************************
*                                                                        
      function exav(x) 
      implicit none
*
      real exphuge,expbig,expsmall,x,exav
      data exphuge/1.0e+9/,expbig/1.0e+8/,expsmall/1.0e-8/
*
      if (x .gt. exphuge) then
         exav = 0.0
      elseif (x .gt. expbig) then
         exav = 1.0/x
      elseif (x .gt. expsmall) then
         exav = (1.0-exp(-x))/x
      else
         exav = 1.0-x/2.0
      endif
      return
      end