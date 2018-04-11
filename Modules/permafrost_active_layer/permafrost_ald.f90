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
    integer, intent(in) :: nsl, ilen, i1, i2
    real, intent(in) :: tbar(ilen, nsl), zbot(nsl)

    !> Local variables.
    real, parameter :: TFREZ = 273.16
    integer i, j

    !> Output variables.
    real, intent(out) :: ald(ilen)

    !> Searching for active layer depth, where temperature in the soil transitions from above to below freezing.
    !> ALD is interpolated by TBAR to a depth inside the layer.
    ald = 0.0
    do i = i1, i2
        do j = 2, nsl
            if (sign(1.0, tbar(i, j) - TFREZ) /= sign(1.0, tbar(i, j - 1) - TFREZ)) then
                ald(i) = (zbot(j) - zbot(j - 1))/(tbar(i, j - 1) - tbar(i, j))*(tbar(i, j - 1) - TFREZ) + zbot(j - 1)
                exit
            end if
        end do
    end do

end subroutine
