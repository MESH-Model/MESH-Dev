      SUBROUTINE SCREENRH(SRH,ST,SQ,PRESSG,FMASK,ILG,IL1,IL2,q)
C
C     * APR 30, 2009 - M.LAZARE.
C
C     * CALCULATES SCREEN RELATIVE HUMIDITY BASED ON INPUT SCREEN
C     * TEMPERATURE, SCREEN SPECIFIC HUMIDITY AND SURFACE PRESSURE.
C     * THE FORMULAE USED HERE ARE CONSISTENT WITH THAT USED ELSEWHERE
C     * IN THE GCM PHYSICS.
C
      use MODELS, only : Nmod
      IMPLICIT NONE
C
C     * OUTPUT FIELD:
C
      REAL,   DIMENSION(ILG)              :: SRH
C
C     * INPUT FIELDS.
C
      REAL,   DIMENSION(ILG)              :: ST,SQ,PRESSG
      real, dimension (ILG,Nmod) :: FMASK
C
      REAL FACTE,EPSLIM,FRACW,ETMP,ESTREF,ESAT,QSW
      REAL A,B,EPS1,EPS2,T1S,T2S,AI,BI,AW,BW,SLP,CP
      REAL RW1,RW2,RW3,RI1,RI2,RI3                 
      REAL ESW,ESI,ESTEFF,TTT,UUU
C
      INTEGER ILG,IL,IL1,IL2,q
C
C     * COMMON BLOCKS FOR THERMODYNAMIC CONSTANTS.
C
      COMMON /EPS /  A,B,EPS1,EPS2    
      COMMON /HTCP/  T1S,T2S,AI,BI,AW,BW,SLP,CP
C
C     * PARAMETERS USED IN NEW SATURATION VAPOUR PRESSURE FORMULATION.
C
      COMMON /ESTWI/ RW1,RW2,RW3,RI1,RI2,RI3                 
C
C     * COMPUTES THE SATURATION VAPOUR PRESSURE OVER WATER OR ICE.
C
      ESW(TTT)        = EXP(RW1+RW2/TTT)*TTT**RW3
      ESI(TTT)        = EXP(RI1+RI2/TTT)*TTT**RI3
      ESTEFF(TTT,UUU) = UUU*ESW(TTT) + (1.-UUU)*ESI(TTT)   
C========================================================================
!For common block EPS:

      A      =21.656
      B      =5418.
      EPS1   =0.622
      EPS2   =0.378

!For HTCP:

      CP     =1004.5
      T1S    =273.16
      T2S    =233.16
      AI     =2.88053E+6/CP
      BI     =0.167E+3/CP
      AW     =3.15213E+6/CP
      BW     =2.38E+3/CP
      SLP    =1./(T1S-T2S)

!And for ESTWI:

      RW1 = 53.67957
      RW2 = -6743.769
      RW3 = -4.8451
      RI1 = 23.33086
      RI2 = -6111.72784
      RI3 = 0.15215
      
      
      EPSLIM=0.001
      FACTE=1./EPS1-1.
      DO IL=IL1,IL2
       IF(FMASK(IL,q).GT.0.)                                        THEN    
C
C       * COMPUTE THE FRACTIONAL PROBABILITY OF WATER PHASE      
C       * EXISTING AS A FUNCTION OF TEMPERATURE (FROM ROCKEL,     
C       * RASCHKE AND WEYRES, BEITR. PHYS. ATMOSPH., 1991.)       
C
        FRACW = MERGE( 1.,    
     1                 0.0059+0.9941*EXP(-0.003102*(T1S-ST(IL)+T1S)**2),
     2                 (ST(IL)+T1S).GE.T1S )  
C
        ETMP=ESTEFF(ST(IL)+T1S,FRACW)
!       ETMP=FRACW*(EXP(RW1+RW2/ST(IL))*ST(IL)**RW3) + 
!    +        (1.-FRACW)*EXP(RI1+RI2/ST(IL))*ST(IL)**RI3
        ESTREF=0.01*PRESSG(IL)*(1.-EPSLIM)/(1.-EPSLIM*EPS2)
        IF ( ETMP.LT.ESTREF ) THEN
          ESAT=ETMP
        ELSE
          ESAT=ESTREF
        ENDIF
C
        QSW=EPS1*ESAT/(0.01*PRESSG(IL)-EPS2*ESAT)
        SRH(IL)=MIN(MAX((SQ(IL)*(1.+QSW*FACTE))
     1         /(QSW*(1.+SQ(IL)*FACTE)),0.),1.)
       ENDIF
      ENDDO
C
      RETURN
      END
