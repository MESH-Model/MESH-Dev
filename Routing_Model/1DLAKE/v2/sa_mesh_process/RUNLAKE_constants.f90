module RUNLAKE_constants

    implicit none

    real :: TKECN = 1.33
    real :: TKECF = 0.25
    real :: TKECE = 1.15
    real :: TKECS = 0.2
    real :: HDPTHMIN = 0.5
    real :: DUMAX = 0.1
    real :: TKEMIN = 1.0E-12
    real :: DELMAX = 5.0
    real :: DELMIN = 0.5
    real :: EMSW = 0.97
    real :: DELZLK = 0.5
    real :: DELSKIN = 0.05
    real :: DHMAX = 2.0
    real :: TKECL = 0.235

    !> Data block accessible to CLASSL, etc.
    common /LAKECON/ &
        TKECN, TKECF, TKECE, TKECS, HDPTHMIN, TKEMIN, DELMAX, &
        DELMIN, EMSW, DELZLK, DELSKIN, DHMAX, TKECL, DUMAX

end module
