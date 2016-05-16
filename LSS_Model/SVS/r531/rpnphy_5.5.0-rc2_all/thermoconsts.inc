!Module module_thermoconsts
!Implicit None
  Real ::  ai     = 0.2864887713087e+04  ! pour fn htvocp
  Real ::  aw     = 0.3135012829948e+04  ! pour fn htvocp
  Real ::  bi     = 0.1660931315020e+00  ! pour fn htvocp
  Real ::  bw     = 0.2367075766316e+01  ! pour fn htvocp
  Real ::  cappa  = 0.2854912179500e+00  ! rgasd/cpd
  Real ::  chlf   = 0.3340000000000e+06  ! ch. lat. fusion       J kg-1
  Real ::  chlc   = 0.2501000000000e+07  ! ch. lat. condens.(0C) J kg-1
  Real ::  consol = 0.1367000000000e+04  ! constante solaire     W m-2
  Real ::  cpd    = 0.1005460000000e+04  ! chal. spec. air sec   J kg-1 K-1
  Real ::  cpv    = 0.1869460000000e+04  ! chal. spec. vap eau   J kg-1 K-1
  Real ::  cpi    = 0.2115300000000e+04  ! chal. spec. glace     J kg-1 K-1
  Real ::  delta  = 0.6077686814144e+00  ! 1/eps1 - 1
  Real ::  eps1   = 0.6219800221014e+00  ! rgasd/rgasv
  Real ::  eps2   = 0.3780199778986e+00  ! 1 - eps1
  Real ::  grav   = 0.9806160000000e+01  ! acc. de gravite       m s-2
  Real ::  karman = 0.4000000000000e+00  ! cte de von karman
  Real ::  knams  = 0.5147910000000e+00  ! passage kt a m/s
  Real ::  omega  = 0.7292000000000e-04  ! rotation terre        s-1
  Real ::  pi     = 0.3141592653590e+01  ! cte pi=acos(-1)
  Real ::  rauw   = 0.1000000000000e+04  ! densite eau liq       kg m-3
  Real ::  rayt   = 0.6371220000000e+07  ! rayon moy. terre      m
  Real ::  rgasd  = 0.2870500000000e+03  ! cte gaz - air sec     J kg-1 K-1
  Real ::  rgasv  = 0.4615100000000e+03  ! cte gaz - vap eau     J kg-1 K-1
  Real ::  ric    = 0.2000000000000e+00  ! cte richardson crit.
  Real ::  slp    = 0.6666666666667e-01  ! pour fn htvocp
  Real ::  stefan = 0.5669800000000e-07  ! cte stefan-boltzmann  J m-2 s-1 K-4
  Real ::  stlo   = 0.6628486583943e-03  ! schuman-newell l.r.   K s2 m-2
  Real ::  t1s    = 0.2731600000000e+03  ! pour fn htvocp        K
  Real ::  t2s    = 0.2581600000000e+03  ! pour fn htvocp        K
  Real ::  tcdk   = 0.2731500000000e+03  ! passage k a c         C
  Real ::  tgl    = 0.2731600000000e+03  ! temp glace dans atm   K
  Real ::  trpl   = 0.2731600000000e+03  ! point triple - eau    K
!
  Real*8, Parameter :: ttns1  = 610.78D0
  Real*8, Parameter :: ttns3w = 17.269D0
  Real*8, Parameter :: ttns3i = 21.875D0
  Real*8, Parameter :: ttns4w = 35.86D0
  Real*8, Parameter :: ttns4i =  7.66D0
!
!  Logical :: bit = .True.
!End Module module_thermoconsts
