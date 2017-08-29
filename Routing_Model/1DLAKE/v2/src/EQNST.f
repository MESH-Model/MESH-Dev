      SUBROUTINE EQNST(EXPW,RHO,TCEL,H)
C======================================================================
C     * AUG  2/16 - M.MACKAY.  	Equation of state moved to subroutine
C
      IMPLICIT NONE
C
C ----* INPUT FIELDS *------------------------------------------------
C
      REAL EXPW,RHO,TCEL
      REAL GRAV,H,C0,ALPHA,P,BETA,RHO0,T0,T,Z,XI,KAPPA,S
C
C SALINITY AND PRESSURE EFFECTS NOT INCLUDED 
c
      Z=0.0
      S=0.0
Cmdm  S=0.3
Cmdm  S=0.014
Cmdm  S=0.03
Cmdm  Z=MIN(10.0,H)
C-------------------------------------------------------------------------
      GRAV=9.80616
      C0=4.9388E-5
      ALPHA=3.3039E-7
      BETA=8.2545E-6
      XI=8.0477E-1
      KAPPA=0.2151
Cmdm  T0=3.9816
Cmdm  T0=3.9839
      T0=3.98275
      RHO0=999.975 + XI*S
      P=RHO0*GRAV*Z*1.0E-5
      T=TCEL-T0-kappa*S
C======================================================================
C Farmer and Carmack, 1981
C
       RHO= RHO0*(1. + P*(C0-ALPHA*T) - BETA*T*T )
       EXPW = (2.*RHO0*BETA*T + RHO0*P*ALPHA )/RHO

      RETURN
      END
