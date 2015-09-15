module climate_forcing_variables

    implicit none

    !> *****************************************************************
    !> These are relative indices of the forcing files used throughout
    !> the I/O code for climate forcing.
    !> *****************************************************************
    type climate_forcing_file_keys

        !* FCLO: Fractional cloud cover [-]
        !integer :: FCLO = -9999

        !* FI: Downwelling longwave sky radiation [W m-2]
        integer :: FI = 2

        !* FB: Incoming solar radiation [W m-2]
        !>       CLASS ordinarily requires that the forcing incoming
        !>       shortwave radiation be partitioned into
        !>       the visible and near-infrared components. If these
        !>       are not available, however, they can each
        !>       be roughly estimated as approximately half of the
        !>       total incoming solar radiation.
        !* FSIH: Near infrared shortwave radiation incident on a
        !*       horizontal surface [W m-2]
        !* FSVH: Visible shortwave radiation incident on a horizontal
        !*       surface [W m-2]
        integer :: FB = 1
        !integer :: FSIH = -9999
        !integer :: FSVH = -9999

        !* PR: Surface precipitation rate [kg m-2 s-1]
        integer :: PR = 3

        !* P0: Surface air pressure [Pa]
        integer :: P0 = 6

        !* HU: Specific humidity at reference height [kg kg-1]
        integer :: HU = 7

        !* TT: Air temperature at reference height [K]
        integer :: TT = 4

        !* UU: Zonal component of wind velocity [m s-1]
        !* VV: Meridional component of wind velocity [m s-1]
        !>       CLASS does not actually require information on wind
        !>       direction. Thus, if only the scalar wind
        !>       speed is available, either ULGRD or VLGRD can be set
        !>       to it, and the other to zero.
        !* UV: Wind speed [m s-1]
        !integer :: UU = -9999
        !integer :: VV = -9999
        integer :: UV = 5

    end type

    type(climate_forcing_file_keys), save :: cfk

end module
