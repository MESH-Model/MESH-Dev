MODULE YOMCMEMPAR

! Module containing the model *SETUP* parameters,
!  derived parameters and universal constants

! Model Parameter options :

!   Physical Parameters :
!     fghz : microwave frequency (GHz)
!     theta : incidence angle (degrees)

!   Default Model Configuration :
!     CIDIEL    dielectric mixing model : 
!               1 : 'Wang' Wang and Schmugge, 1980
!               2 : 'Dobson' Dobson et al., 1985
!               3 : 'Mironov' Mironov et al., 2004
!     CITEFF    effective temperature parametrization  
!               0 : 'Tsoil' teff = tsoil1
!               1 : 'Choudhury' et al., 1982
!               2 : 'Wigneron' et al., 2001
!               3 : 'Holmes' et al., 2006
!     CISMR     model for Smooth Surface Emissivity
!               1 : 'Fresnel' (Njoku and Kong, 1977)
!               2 : 'Wilheit', 1978
!     CIRGHR    surface roughness model
!               0 : 'No' Use Smooth Surface Emissivity
!               1 : 'Choudhury'  79
!               2 : 'Wsimple' Wigneron et al., 2001
!               3 : 'Wegmueller' and Maetzler, 1999
!               4 : 'Wtexture' Wigneron et al, fn of texture 
!               5 : 'Wigneron'  with moisture dependance, test phase
!     CIVEG     vegetation opacity model
!               0 : 'No' no vegetation
!               1 : 'Kirdyashev' et al., 1979
!               2 : 'Wegmueller' et al., 1995
!               3 : 'Wigneron'   et al., 2007
!               4 : 'Jackson'    and Schmugge, 1991
!     CIATM     atmospheric radiative transfer model
!               0 : 'No' no atmosphere and no cosmic background radiation
!               1 : 'Pellarin' and Calvet
!               2 : 'Liebe', 1989
!               3 : 'Ulaby', 1986
!    CITDIEL : Temperature for dielectric model
!               1 : 'Teff'   tdiel=teff   (lmeb)
!               2 : 'Tsurf'  tdiel=tsurf  (lsmem and for iteff=3) 
!    CITVEG : Temperature of vegetation
!               1 : 'Tsurf'   tveg=tsurf   (lmeb)
!               2 : 'Tair'    tveg=tair    (lsmem)
!               3 : 'Tir'     tveg=tskin           
!    CIDVEG : vegetation cover input data
!               1 : 'Ecoclimap'  ecoclimap
!               2 : 'Tessel' ECMWF (mars)
!               3 : 'HTessel' ECMWF climate data base

!   Aditional soil model parameters :
!    nlay_soil_mw : number of soil layers in the MW emission model
!    nlay_soil_ls : number of soil layers in the Land surface model
!    nobs_atm : number of atmospheric observations
!   ip_rgh_surf : soil surface roughness [cm]
!              (Attention: surface roughness in Choudhury et al. (~0.35)
!              is not the same as in Wegmueller and Maetzler (~0-5cm))
              ! *QUESTION* make different names to keep them apart?   
!  ip_Q : Cross polarization parameter


! Derived Parameters :
!   f : microwave frequency (Hz)
!   costheta : cosine of incedence angle theta
!   sintheta : sine of incedence angle theta
!   lam : wavelength (m)
!   k : wavenumber (1/m)
!   lamcm : wavelength (cm)
!   kcm : wavenumber (1/cm)
!   omega : radian frequency (omega = 2. * pi * f), with f in Herz 

! Universal constants
!   c : speed of light (m/s)
!   tfreeze : freezing point of water (K)
!   rhowat : water density (kg/m3)
!   pi
!---------------------------------------------------------------------------
!    NAME       TYPE      PURPOSE
!    ----    :  ----   : ----------------------------------------------

! NAMOPT:
! CIDIEL   C : CHARACTER: IDENTIFIES THE DIELCTRIC MODEL
! CITEFF   C : CHARACTER: IDENTIFIES THE EFFECTIVE TEMPRATURE MODEL 
! CISMR    C : CHARACTER: IDENTIFIES THE SOIL MODEL 
! CIRGHR   C : CHARACTER: IDENTIFIES THE ROUGHNESS MODEL
! CIVEG    C : CHARACTER: IDENTIFIES THE VEGETATION OPACITY MODEL
! CIATM    C : CHARACTER: IDENTIFIES THE ATMOSPHERIC OPACITY MODEL
! CITVEG   C : CHARACTER: IDENTIFIES THE VEGETAION TEMPERATURE
! CIDVEG   C : CHARACTER: IDENTIFIES THE INPUT VEGETATION DATA SET
! CITDIEL  C : CHARACTER: IDENTIFIES THE TEMPERATURE FOR DIELCTRIC MODEL 
! CNAMEID  C : CHARACTER: NAME OF SIMUALTION, SUMMARIZES THE 8 FIRST OPTIONS CHOOSEN
! CFREQ    C : CHARACTER: Frequency of observation (GHz)
! CANGLE   C : CHARACTER: Angle of observation (degrees)

! NAMDEF:
! LOFFCMEM L : LOGICAL: False when offline
! LOMASK_OCEAN L : LOGICAL: False when global 
! LOMASK_AUTO  L : LOGICAL: True to remove wrong points (example TSKIN <=0K) 
! LOFIELDEXP L : LOGICAL: False when grid, true for 1pt -  only with ascii forcing 

! LGPRINT  L : LOGICAL: TRUE IF DEBUG INFO IS REQUIRED
! JPHISTLEV I : Number of output files
!             : Level1 (TB, Teff)
!             : Level2 (TB, Teff, VWC, frbare)
!             : Level3 (TB, Teff, VWC, frbare,salwat,Bpa, WP, falpha, fh, ffraci)
! CFINOUT   C : File format for input/output files: 'netcdf','grib,'ascii'

!NAMERAD:
! THETA    R : REAL: Incidence angle (in degres)
! FGHZ     R : REAL: Microwave frequency in Ghz

! NAMELEV:
! NLAY_SOIL_MW I: INTEGER: Number of soil layer in the MW model in input data
! NLAY_SOIL_LS I: INTEGER: Number of soil layer in the Land Surface Model
! NOBS_ATM   I: INTEGER: Number of atmospheric levels in input data (for CIATM='Liebe')




USE PARKIND1  ,ONLY : JPIM,JPRM 
IMPLICIT NONE


INTEGER(KIND=JPIM), PARAMETER::JPCHARLEN=100             !! Length of CNAMEID
CHARACTER(LEN=JPCHARLEN) ::  CIDIEL, CITEFF, CISMR, CIRGHR, CIVEG, CIATM, CITVEG, CIDVEG, CITDIEL
INTEGER(KIND=JPIM), PARAMETER::JPNAMEIDLEN=16             !! Length of CNAMEID
CHARACTER(LEN=JPNAMEIDLEN) :: CNAMEID
CHARACTER(LEN=3) CFREQ
CHARACTER(LEN=2) CANGLE

INTEGER(KIND=JPIM), PARAMETER::JPCMEMTILE=7             !! number of tiles in CMEM




INTEGER(KIND=JPIM)             :: JPHISTLEV      !! Defines levels of outputs
LOGICAL                        :: LGPRINT        !! Print debug info
LOGICAL                   :: LOFFCMEM       !! Offline mode
LOGICAL                   :: LOFIELDEXP       !! For field experiment application (1pixel)
LOGICAL                   :: LOMASK_OCEAN       !! For ocean masking in case no input data 
LOGICAL                   :: LOMASK_AUTO       !! For masking in case of wrong input data 
CHARACTER(LEN=JPCHARLEN) ::  CFINOUT        !! Input/output file format

REAL(KIND = JPRM) :: fghz        
REAL(KIND = JPRM) :: theta      

INTEGER(KIND=JPIM) :: nlay_soil_mw
INTEGER(KIND=JPIM) :: nlay_soil_ls
INTEGER(KIND=JPIM) :: nobs_atm 



REAL(KIND = JPRM) :: ip_sal_soil
REAL(KIND = JPRM) :: ip_rgh_surf
REAL(KIND = JPRM) :: ip_wcveg(2)
REAL(KIND = JPRM) :: ip_Q

REAL(KIND = JPRM) :: f
REAL(KIND = JPRM) :: costheta
REAL(KIND = JPRM) :: sintheta
REAL(KIND = JPRM) :: lam
REAL(KIND = JPRM) :: k_cmem
REAL(KIND = JPRM) :: lamcm
REAL(KIND = JPRM) :: kcm
REAL(KIND = JPRM) :: omega

REAL(KIND = JPRM) :: c
REAL(KIND = JPRM) :: tfreeze
REAL(KIND = JPRM) :: rhowat
REAL(KIND = JPRM) :: pi_cmem
REAL(KIND = JPRM) :: PPG 


 
END MODULE YOMCMEMPAR
