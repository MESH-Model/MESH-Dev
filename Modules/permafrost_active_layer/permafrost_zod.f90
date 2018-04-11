!> Description:
!>  Subroutine to calculate zero oscillation depth given minimum and
!>  maximum temperatures and a temperature tolerance for a soil profile.
!>
!> Input variables:
!*  tmax: Maximum soil temperature of each layer (1: Grid or tile index; 2: Soil layer). [deg C or K].
!*  tmin: Minimum soil temperature of each layer (1: Grid or tile index; 2: Soil layer). [deg C or K].
!*  zbot: Bottom of soil layer to surface (1: Soil layer). [m below surface].
!*  ttol: Tolerance for temperature comparison. [deg C or K].
!*  nsl: Number of soil layers. [--].
!*  ilen: Number of elements in the inputs. [--].
!*  i1: First grid or tile index to process. [--].
!*  i2: Last grid of tile index to process. [--].
!>
!> Output variables:
!*  zod: Zero oscillation depth where tmax ~= tmin. [m below surface].
subroutine permafrost_zod(tmax, tmin, zbot, ttol, zod, ilen, nsl, i1, i2)

    implicit none

    !> Input variables.
    integer, intent(in) :: nsl, ilen, i1, i2
    real, intent(in) :: tmax(ilen, nsl), tmin(ilen, nsl), zbot(nsl), ttol

    !> Output variables.
    real, intent(out) :: zod(ilen)

    !> Local variables.
    integer i, j
    real trng(ilen, nsl)

    !> Calculate the depth where the range of maximum to minimum temperature is within 'TTOL'.
    !> ZOD is interpolated to a depth inside the layer.
    trng(i1:i2, :) = tmax(i1:i2, :) - tmin(i1:i2, :)
    zod = 0.0
    do i = i1, i2
        do j = 2, nsl
            if (sign(1.0, trng(i, j) - ttol) /= sign(1.0, trng(i, j - 1) - ttol)) then
                zod(i) = (zbot(j) - zbot(j - 1))/(trng(i, j - 1) - trng(i, j))*(trng(i, j - 1) - ttol) + zbot(j - 1)
                exit
            end if
        end do
    end do

end subroutine
