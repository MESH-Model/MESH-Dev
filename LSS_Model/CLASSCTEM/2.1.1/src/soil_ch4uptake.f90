!>\file
!!Canadian Terrestrial Ecosystem Model (CTEM)
!!Soil Methane Oxidation Subroutine
!!
!! Coded up based on \cite Curry2007-du.

subroutine soil_ch4uptake(IL1,IL2,tbar,THP,BI,THLQ, &
     &                     THIC,PSIS,GRAV,FCAN,obswetf, &
     &                     wetfdyn,wetfracgrd,isand,RHOW,RHOICE, &
     &                     atm_CH4,CH4_soills)

!History:

! J. Melton. Dec 22 2015 - Coded up based on C. Curry (2007) Modelling the
! the soil consumption of atmospheric methane at the global scale. Global
! Biogeo. Cycl. v. 21 GB4012 doi: 10.1029/2006GB002818.

use ctem_params,  only : ilg,ignd,ican,nlat,wtCH4

implicit none

! Arguments:
real, dimension(ilg,ignd), intent(in) :: tbar     !< Temperature of soil layers (K) - daily average
real, dimension(ilg,ignd), intent(in) :: THP      !< Total porosity \f$(cm^3 cm^{-3})\f$ - daily average
real, dimension(ilg,ignd), intent(in) :: BI       !< Clapp and Hornberger b-term (-)
real, dimension(ilg,ignd), intent(in) :: THLQ     !< Fractional water content (-) - daily average
real, dimension(ilg,ignd), intent(in) :: THIC     !< Fractional ice content (-) - daily average
real, dimension(ilg,ignd), intent(in) :: PSIS     !< Soil moisture suction at saturation (m)
real, dimension(ilg,ican), intent(in) :: FCAN     !< Fractional coverage of vegetation (-)
real, dimension(nlat), intent(in) :: wetfracgrd   !< Prescribed fraction of wetlands in a grid cell
real, dimension(ilg), intent(in) :: wetfdyn       !< Dynamic gridcell wetland fraction determined using slope and soil moisture
real, dimension(ilg), intent(in) :: atm_CH4       !< Atmospheric \f$CH_4\f$ concentration at the soil surface (ppmv)
real, intent(in) :: GRAV                          !< Acceleration due to gravity \f$(m s^{-1})\f$
logical, intent(in) :: obswetf                    !< Switch, if true then use the prescribed wetland cover
real, intent(in) :: RHOW                          !< Density of water \f$(kg m^{-3})\f$
real, intent(in) :: RHOICE                        !< Density of ice \f$(kg m^{-3})\f$
integer, dimension(ilg,ignd), intent(in) :: isand !< flag for soil/bedrock/ice/glacier
integer, intent(in) :: IL1
integer, intent(in) :: IL2
real, dimension(ilg), intent(out) :: CH4_soills   !< Methane uptake into the soil column \f$(mg CH_4 m^{-2} s^{-1})\f$

! GRAV, RHOW, and RHOICE are the same as in the commonblocks:
! CLASS2 : GRAV and CLASS4 : RHOW, RHOICE. I am passing as arguments
! to avoid the common block format.

! Local variables:
real :: Tsoil                           !< Temperature of soil layers \f$(\circ C)\f$
real :: D_soil                          !< Diffusivity of CH4 in soil \f$(cm^2 s^{-1})\f$
real :: G_T                             !< Temperature factor used in determining D_soil (-)
real :: G_soil                          !< Soil moisture factor used in determining D_soil (-)
real :: THP_air                         !< Air-filled porosity \f$(cm^3 cm^{-3})\f$
real :: k_oxidr                         !< First-order oxidation rate constant \f$(s^-1)\f$
real :: r_T                             !< Temperature factor used in determination of rate constant (-)
real :: r_SM                            !< Soil moisture factor used in determination of rate constant (-)
integer :: i,j,layer                    !< Counters
real :: psi                             !< Soil moisture suction / matric potential (m)
real :: r_C                             !< Factor to account for croplands
real :: r_W                             !< Factor to account for wetlands
real :: THP_tot                         !< temp variable for total porosity \f$(cm^3 cm^{-3})\f$

! Local parameters:
real, parameter :: D_air = 0.196        !< Diffusivity of CH4 in air (cm^2 s^-1) @ STP
real, parameter :: g_0 = 586.7 / 86400. !< Scaling factor takes CH4_soills to mg CH4 m^-2 s^-1 (units: \f$mg CH_4 ppmv^{-1} s s^{-1} m^{-2} cm{-1}\f$)
real, parameter :: betaCH4 = 0.8        !< Constant derived in Curry (2007) from comparison against measurements (-)
real, parameter :: k_o = 5.03E-5        !< Base oxidation rate derived in Curry (2007) from comparison against measurements \f$(s^{-1})\f$

!>---------------------------------------------------------------------
!> Begin
!!
!! The soil oxidation methane sink is assumed to only operate in the first model
!! soil layer, thus we only consider that layer here.
layer = 1

do 10 i = IL1, IL2

    if (isand(i,layer) <= -1) goto 10 !> not soil so move on.

    !> Convert tbar to Tsoil (from K to deg C)
    Tsoil = tbar(i,layer) - 273.16

    !> Find the diffusion coefficient in soil (D_soil)

    !> First the temperature factor, G_T:
    G_T = 1.0 + 0.0055 * Tsoil

    !> Find the air filled porosity, THP_air:
    THP_air = THP(i,layer) - (THLQ(i,layer) + THIC(i,layer)*RHOICE/RHOW)
    THP_tot = THP(i,layer)

    !> Note: THP_air can fall to < 0 after snow melt
    if (THP_air  < 0.) then
        THP_air = 0.0
        THP_tot = (THLQ(i,layer) + THIC(i,layer)*RHOICE/RHOW)
    end if

    !> The BI  (Clapp and Hornberger b-term) is already calculated by CLASS as:
    !>BI = 15.9 * f_clay + 2.91, thus we use that value.

    !> G_soil is the influence of the soil texture, moisture, and porosity:
    G_soil = THP_tot**(4./3.) * (THP_air / THP_tot)**(1.5 + 3. / BI(i,layer))

    !> The diffusion coefficient of CH4 in soil is then:
    D_soil = D_air * G_T * G_soil

    !> Determine the first-order oxidation rate constant (k_oxidr)

    !> First find the temperature term, r_T (FLAG note that Charles' original code does not have the high temp limit!)

    if (Tsoil < 0.0 .and. Tsoil >= -10.0) then
        r_T = (0.1 * Tsoil + 1.0)**2
    else if (Tsoil >= 0.0 .and. Tsoil < 43.3) then
        r_T = exp(0.0693 * Tsoil - 8.56E-7 * Tsoil**4)
    else !>all other temps (<-10 and >=43.3)
        r_T = 0.
    end if

    !> Next find the term based on soil moisture (suction)

    !> Find the soil water potential for the uppermost layer
    !> need the absolute value.
    psi = abs(PSIS(i,layer) * (THLQ(i,layer)/THP_tot)**(-BI(i,layer)))

    !> Convert units from m to kPa
    psi = psi * GRAV

    if ( psi < 200.) then !>0.2 MPa in paper (NOTE: In Charles's code this is \f$\leq\f$, but is < in paper)
        r_SM = 1.0
    else if (psi >= 200. .and. psi <= 1.E5) then !>0.2 and 100 Mpa in paper
        r_SM = (1. - (log10(psi) - log10(200.)) / (log10(1.E5) - log10(200.)))**betaCH4
    else !> psi > 100 MPa.
        r_SM = 0.
    end if

    k_oxidr = k_o * r_T * r_SM

    !> Find the flux correction for croplands

    r_C = 1.0 - (0.75 * FCAN(i, 3))

    !> Find the flux correction due to wetlands

    if (obswetf) then  !> Use the prescribed wetland fractions
        r_W = 1.0 - wetfracgrd(i)
    else !> use the dynamically determined wetland area
        r_W = 1.0 - wetfdyn(i)
    end if

    !> Find the surface flux (CH4_soills) for each tile, then for each gridcell

    CH4_soills(i) =  atm_CH4(i) * r_C * r_W * g_0 * sqrt(D_soil * k_oxidr)

    !> Convert from mg CH4 m^-2 s^-1 to umol CH4 m^-2 s^-1

    CH4_soills(i) = CH4_soills(i) * 1.E3 / wtCH4


10 continue

end subroutine soil_ch4uptake

