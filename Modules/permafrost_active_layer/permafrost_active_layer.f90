module permafrost_active_layer

    implicit none

    contains

    subroutine zero_oscillation_depth( &
        TMAX, TMIN, ZBOT, TTOL, &
        ZOD, &
        NSL, ILG, IL1, IL2)

        !> Input variables.
        !*  NSL: Number of soil layers. [--].
        !*  ILG: Number of elements in the arrays. [--].
        !*  IL1, IL2: Start and stop indices to use from elemental arrays. [--; IL2 >= IL1].
        !*  ID1, ID2: Start and stop indices to use, corresponding to a period of days. [--; ID2 >= ID1].
        integer, intent(in) :: NSL, ILG, IL1, IL2
        !*  TMAX: Maximum soil temperature of each layer (1: ILG; 2: NSL). [deg C or K].
        !*  TMIN: Minimum soil temperature of each layer (1: ILG; 2: NSL). [deg C or K].
        !*  ZBOT: Bottom of soil layer to surface (1: NSL). [m below surface].
        !*  TTOL: Tolerance for temperature comparison. [deg C or K].
        real, intent(in) :: TMAX(ILG, NSL), TMIN(ILG, NSL), ZBOT(NSL), TTOL

        !> Local variables.
        integer j, i
        real TRANGE(ILG, NSL)

        !> Output variables.
        !*  ZOD: Zero oscillation depth, where TMAX ~= TMIN. [m below surface].
        real, intent(out) :: ZOD(ILG)

        !> Initialize output variables.
        ZOD = -1.0

        !> Get maximum and minimum values of temperature.
        !> ZOD is determined using TTOL.
        do i = il1, il2
            TRANGE(i, :) = TMAX(i, :) - TMIN(i, :)
            do j = 2, NSL
                if (sign(1.0, TRANGE(i, j) - TTOL) /= sign(1.0, TRANGE(i, j - 1) - TTOL)) then
                    ZOD(i) = (ZBOT(j) - ZBOT(j - 1))/(TRANGE(i, j - 1) - TRANGE(i, j))*(TRANGE(i, j - 1) - TTOL) + ZBOT(j - 1)
                end if
            end do
        end do

    end subroutine

    subroutine active_layer_depth( &
        TBAR, ZBOT, &
        ALD, &
        NSL, ILG, IL1, IL2)

        !> Input variables.
        !*  NSL: Number of soil layers. [--].
        !*  ILG: Number of elements in the arrays. [--].
        !*  IL1, IL2: Start and stop indices to use from elemental arrays. [--; IL2 >= IL1].
        integer, intent(in) :: NSL, ILG, IL1, IL2
        !*  TBAR: Soil temperature (1: ILG; 2: NSL). [K].
        !*  ZBOT: Bottom of soil layer to surface (1: NSL). [m below surface].
        real, intent(in) :: TBAR(ILG, NSL), ZBOT(NSL)

        !> Local variables.
        real, parameter :: TFREZ = 273.16
        integer j, i

        !> Output variables.
        !*  ALD: Active layer depth. [m below surface].
        real, intent(out) :: ALD(ILG)

        !> Initialize output variables.
        ALD = -1.0

        !> Searching for zero temperature, where temperature in the soil transitions from positive to below zero.
        !> ALD is interpolated by TBAR to a depth inside the layer.
        do i = il1, il2
            do j = 2, NSL
                if (TBAR(i, j) == TFREZ) then
                    ALD(i) = ZBOT(j)
                    exit
                else if (TBAR(i, j - 1) > TFREZ .and. TBAR(i, j) <= TFREZ) then
                    ALD(i) = (ZBOT(j) - ZBOT(j - 1))/(TBAR(i, j - 1) - TBAR(i, j))*(TBAR(i, j - 1) - 0.0) + ZBOT(j - 1)
                    exit
                end if
            end do
        end do

    end subroutine

end module
