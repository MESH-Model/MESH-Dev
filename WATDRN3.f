C
CDGP  2011-05-05: FOR WARDRN3 (RIC)
C
C     *****************************************************************
C     SUB WATDRN3
C     *****************************************************************
C     IMPLEMENTS RIC'S WATDRN METHOD.  VARIABLES ARE COLLECTED BY 
C     WATDRNB.
C     *****************************************************************
C     JUN 03/11 - DGP      ADDED
C     JUL 10/11 - DGP      ADDED USE FLAGS FOR WD3FLOW.
C
      SUBROUTINE WATDRN3 (i,j,ASAT1,ASAT2,KSAT,GRKEFF,ISAND,
     1     runoff,ovrflw,SUBFLW,BASFLW,      
     2     trunof,tovrfl,tsubfl,tbasfl,
     1     IG,NA,NTYPE,ILG,IL1,IL2,
     3     bi,fi,satfc,thliq,thpor)
c     3     BCAP,DCOEFF,BFCAP,BFCOEFF,b,satfc)
C
      USE FLAGS
C
      IMPLICIT NONE
C
      INTEGER IGND,NA,NTYPE,ILG,IL1,IL2,K,M,ig
C
      INTEGER, DIMENSION(ILG) :: ILMOS,JLMOS
CC
      REAL    dden(ilg)     ! drainage density (m/m)
      REAL    thpor(ilg,ig)  ! porosity
      REAL    xslope(ilg) ! valley slope
      REAL    delzw(ilg,ig)  ! layer thickness (m)
      REAL    bfcoef  ! slope of retention curve, related to
                            ! Clapp-Hornberger connextivity index by c=2b+3 
c      REAL    thporaj(ilg) ! Available porosity of the soil layer
c      real    thicej(ilg)  ! (total porosity - ice content)
c      REAL    psisatj(ilg) ! soil section TODO - NEED UNITS
      REAL    ksat(ilg,ig)   ! Vertical hydraulic conductivity at saturation
                          ! at the bottom of the layer (m/s)
      REAL    grkeff(ilg,ig) ! average value of the parameter
                          ! controlling the time scale of interflow process
                          ! ksat * (tile slope / tile length) (1/s)
      REAL    asat1        ! bulk saturation at initial time
      REAL    TC(ILG),qflow,dpotent
      REAL    C(ILG,ig)
      real    bi(ilg,ig)
      real    fi(ilg)
      REAL    RATIOT(ILG)
      REAL    ratiotb(ILG)
      integer isand(ILG,IG) !SAND PERCENT
      
c     Output arrays
      REAL    asat2 ! bulk saturation at the end of the time step
      real    runoff(ilg)
      REAL    subflw(ilg) ! interflow amount during the time step (m)
      REAL    basflw(ilg) ! baseflow rate during the time step (m)
      REAL    satsfc(ilg) ! saturated fraction of the surface (0 to 1)
      real    zpond(ilg) 
c     Work arrays
      REAL    cm1(ilg)    ! c-1
      REAL    c2m1(ilg)   ! 2*c-1
      REAL    asatc  ! bulk saturation at the critical time tc

      LOGICAL satspf(ilg) ! indicates if seepage face is saturated
                          ! equivalent to knowing if t<=tc
      REAL, DIMENSION(ilg,ig) ::  satfc,tsubfl
      REAL, DIMENSION(ilg) :: tovrfl,trunof,tbasfl
      REAL  DELT,BFLOW,
     1      DAVAIL,QHAT,BFHAT,
     1       qtotal,btc

C     * INTERNAL SCALARS AND VECTORS
      REAL VEL_T0(ILG),NUC_DOVERj(ILG),MANNING_N(ILG),
     1    ovrflw(ilg),tol,
     2    BULKFC(ILG,IG),
     4    qmax,XLAMBDA,ktop,kl,h0,c1,c2,
     +    ztop(ilg),thliq(ilg,ig)
      
      integer i,j

C     *****************************************************************
      TOL=1.E-6
C
C     *****************************************************************
C     CALCULATE SUBFLOW AND BASEFLOW.  WE HAVE TO TRANSLATE FROM GAT-
C     TO ROW-INDEXING.
C
      
c      DO i=IL1,IL2
        
        davail=max(0.0,asat1-satfc(i,j))

*FOR SUBFLOW
        btc=1.0-1.0/(2.0*c(i,j)-3.0)
        IF (asat1 .le. (satfc(i,j))) THEN
            QHAT=0.
        ELSEIF (asat1 .ge. btc) THEN
            QHAT=1.
        ELSE
            QHAT=
     1 ((asat1-satfc(i,j))/(btc-satfc(i,j)))**5.0

        endif
           qflow=grkeff(i,j)*QHAT

           qflow = min(davail,qflow)
           asat2=max(satfc(i,j),asat1-qflow)
           asat2=min(asat2,asat1)
           
           qflow=asat1-asat2
           bflow=0.0
       
*FOR BASEFLOW
        if(j==ig)then
         IF (asat1 .LE. satfc(i,j)) THEN
            bfHAT=0.
        ELSE IF (asat1 .GE. 1.0) THEN
            bfHAT=1.
        ELSE
             bfHAT=
     1  ((asat1-satfc(i,j))/(1.0-satfc(i,j)))**7.0
        END IF


        bflow=ksat(i,j)*delt*bfhat*2*dden(i)

        endif

        qflow=min(davail,qflow+bflow)
        asat2=max(satfc(i,j),asat1-qflow)
        asat2=min(asat2,asat1)
        qflow=asat1-asat2


             
*CORRECTION/CONFIGURATION FOR NOT USED BASFLW
*WD3FLOW=0 THEN SUBFLW=SUBFLW,BASFLW=BASFLW
      runoff(i)=runoff(i)+qflow
      SUBFLW(i)=subflw(i)+fi(i)*qflow
      BASFLW(i)=0.0
*WD3FLwOW=1 THEN SUBFLW=SUBFLW+BASFLW,BASFLW=0
c        IF (WD3FLOW .EQ. 1) THEN
c            SUBFLW(K)=SUBFLW(K)+BASFLW(K)+qflow+bflow
c            basflw(k)=0.0
c        END IF
C
C     ****************************************************************C     RETURN
      
c             enddo
      RETURN
C
      END SUBROUTINE WATDRN3
