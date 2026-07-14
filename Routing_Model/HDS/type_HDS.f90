MODULE type_HDS
    IMPLICIT NONE
    SAVE
    ! data types
    INTEGER,      PARAMETER  :: I4B = SELECTED_INT_KIND(9)
    INTEGER,      PARAMETER  :: I2B = SELECTED_INT_KIND(4)
    INTEGER,      PARAMETER  :: I1B = SELECTED_INT_KIND(2)
    INTEGER,      PARAMETER  :: SP  = KIND(1.0)
    INTEGER,      PARAMETER  :: DP  = KIND(1.0D0)
    INTEGER,      PARAMETER  :: LGT = KIND(.true.)
    ! specify precision
    INTEGER,      PARAMETER  :: rkind = SP
    ! useful shortcuts
    real(rkind),  parameter  :: zero      = 0.0_rkind
    real(rkind),  parameter  :: half      = 0.5_rkind
    real(rkind),  parameter  :: one       = 1.0_rkind
    real(rkind),  parameter  :: two       = 2.0_rkind
    real(rkind),  parameter  :: verySmall = 1.0e-12_rkind
    ! physical constants
    real(rkind),  parameter  :: rho_w     = 1000._rkind  ! density of water (kg m-3)
    ! data types
    type, public :: hStruct
        real(rkind),allocatable :: dat(:)
    endtype hStruct
END MODULE type_HDS
