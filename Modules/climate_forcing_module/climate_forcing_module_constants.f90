module climate_forcing_constants

    implicit none

    !>
    !> Description: These are the indices/keys of the forcing variables
    !> stored in the climate forcing variable.
    !>
    !* ck: Instance of the climate forcing variable keys.
    !>

    type climate_forcing_file_keys

        !* nn: Count of available climate forcing variable keys.
!        integer :: nn = 14
        integer :: nn = 7

        !* FB: Incoming solar radiation [W m-2].
        !>       CLASS ordinarily requires that the forcing incoming
        !>       shortwave radiation be partitioned into
        !>       the visible and near-infrared components. If these
        !>       are not available, however, they can each
        !>       be roughly estimated as approximately half of the
        !>       total incoming solar radiation.
        !* FSI: Near infrared shortwave radiation incident on a
        !*      horizontal surface (FSIH) [W m-2].
        !* FSV: Visible shortwave radiation incident on a horizontal
        !*      surface (FSVH) [W m-2].
        integer :: FB = 1
!        integer :: FSI = 10
!        integer :: FSV = 11

        !* FI: Downwelling longwave sky radiation [W m-2].
        integer :: FI = 2

        !* FN: Fractional cloud cover (FCLO) [-].
!        integer :: FN = 12

        !* PR: Surface precipitation rate [m].
        !* RT: Total precipitation rate [kg m-2 s-1]
!        integer :: PR = 14
        integer :: RT = 3

        !* TT: Air temperature at reference height [K]
        integer :: TT = 4

        !* UU: Zonal component of wind velocity [m s-1]
        !* VV: Meridional component of wind velocity [m s-1]
        !>       CLASS does not actually require information on wind
        !>       direction. Thus, if only the scalar wind
        !>       speed is available, either ULGRD or VLGRD can be set
        !>       to it, and the other to zero.
        !* UV: Wind speed [m s-1]
        !* WD: Wind direction [deg].
!        integer :: UU = 8
!        integer :: VV = 9
        integer :: UV = 5
!        integer :: WD = 13

        !* P0: Surface air pressure [Pa].
        integer :: P0 = 6

        !* HU: Specific humidity at reference height [kg kg-1]
        integer :: HU = 7

    end type

    type(climate_forcing_file_keys), save :: ck

    type climate_forcing_block_types
        integer :: GRD = 1
        integer :: GRU = 2
        integer :: GAT = 3
    end type

    type(climate_forcing_block_types), save :: cbk

end module
