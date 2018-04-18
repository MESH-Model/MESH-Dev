!> Description:
!>  Subroutine to calculate active layer depth given temperatures for a
!>  soil profile.
!>
!> Input variables:
!*  tbar: Soil temperature (1: Grid or tile index; 2: Soil layer). [K].
!*  zbot: Bottom of soil layer to surface (1: Soil layer). [m below surface].
!*  nsl: Number of soil layers. [--].
!*  ilen: Number of elements in the inputs. [--].
!*  i1: First grid or tile index to process. [--].
!*  i2: Last grid of tile index to process. [--].
!>
!> Output variables:
!*  ald: Active layer depth. [m below surface].
subroutine permafrost_ald(tbar, zbot, ald, ilen, nsl, i1, i2)

    implicit none

    !> Input variables.
    integer nsl, ilen, i1, i2
    real tbar(ilen, nsl), zbot(nsl)

    !> Output variables.
    real ald(ilen)

    !> Local variables.
    real, parameter :: TFREZ = 273.16
    integer i, j

    !> Calculate ALD, where temperature in the soil transitions from above to below freezing.
    do i = i1, i2

        !> Set ALD = -1.0 in case no ALD is found.
        ald(i) = -1.0
        do j = 2, nsl
            if (tbar(i, j - 1) < TFREZ) then

                !> Ignore and cycle if the layer above is frozen.
                ald(i) = -2.0
                exit
            else if (tbar(i, j) == TFREZ) then

                !> TBAR is TFREZ at the layer.
                ald(i) = zbot(j)
                exit
            else if (sign(1.0, tbar(i, j) - TFREZ) < sign(1.0, tbar(i, j - 1) - TFREZ)) then

                !> Transition occurs inside the layer; ALD is interpolated.
                ald(i) = (zbot(j) - zbot(j - 1))/(tbar(i, j - 1) - tbar(i, j))*(tbar(i, j - 1) - TFREZ) + zbot(j - 1)
                exit
            end if
        end do
    end do

end subroutine
