      BLOCK DATA 
C
C     * SEPT/10 - M.MACDONALD.ADDED PBSM PARAMETERS
C
      COMMON /PBSM/   rhoo, Qstar, MMM, RR, LATH, DICE, ZD, XD, gg,
     1                Betaa, C1, C2, C3, M1KAARMAN, M2KAARMAN, M_PI,
     2                DegToRad
C
      DATA     rhoo,  Qstar, MMM,     RR,      LATH,    DICE, ZD,  XD
     1       / 1.23, 120.0, 18.01, 8313.,  2.838E6, 900., 0.3, 300./

      DATA     gg,    Betaa, C1,  C2,  C3,  M1KAARMAN, M2KAARMAN 
     1       / 9.80, 170.0, 2.8, 1.6, 4.2,     0.4,   0.16/
 
      DATA     M_PI,            DegToRad
     1       / 3.1415926535898, 0.017453292/
C
      END
