!>
!> Module for calculating Mean Absolute Error (ABSERR)
!>
!> Contains the model_output_abserr variable type which stores
!> the value at each streamflow gauge, as well as an average value
!> representative of all gauges.
!>
!> Contains the calc_abserr_value subroutine to calculate ABSERR
!> given observed and simulated daily streamflow.
!>
!> References:
!>      Gupta et al. (1998) -   Toward improved calibration of
!>                              hydrologic models, Water Resources
!>                              Research 34: 751-763
!>
!> Updates:
!>
!> D. G. Princz - 2014-11-19.
!>
!> Fuad Yassin and Uchechukwu Udenze - 2024-06-07.
!>
!> Changes made:
!> - The original calculation did not remove negative values from the observed streamflow (`qobs`).
!> - Updated the `calc_abserr_value` function to:
!>   - Filter out negative values from `qobs`.
!>   - Adjust the indexing accordingly to handle valid data points only.
!>   - Calculate the MAE (Mean Absolute Error) using only the valid data points.
!>
module calc_abserr

    implicit none

    private

    !> The model_output_abserr type and calc_abserr_value function are accessible outside the module.
    public :: model_output_abserr, calc_abserr_value

    !> Type: model_output_abserr
    !* value_gauge: ABSERR for the streamflow gauge (1: streamflow gauge)
    !* value_gauge_avg: Average ABSERR of all gauges
    type model_output_abserr
        real, dimension(:), allocatable :: value_gauge
        real :: value_gauge_avg
    end type

    !>
    !> Begin module.
    !>

    contains

    type(model_output_abserr) function calc_abserr_value(ncal_day_min, ncal_day, qobs, qsim)

        !> Variable declarations
        !* ncal_day_min: Minimum number of days for spin-up
        !* ncal_day: Number of days of daily streamflow values
        !* qobs: Observed values (1: daily flow value; 2: streamflow gauge)
        !* qsim: Simulated values (1: daily flow value; 2: streamflow gauge)
        integer, intent(in) :: ncal_day_min, ncal_day
        real, dimension(:, :), intent(in) :: qobs, qsim

        !> Local variables
        !* j: Counter (streamflow gauge)
        !* i: Counter (day)
        !* mask: Mask of valid values (non-negative numbers)
        !* valid_qobs: List of valid observed values
        !* valid_qsim: List of valid simulated values
        integer j, i, n_valid_days
        logical, dimension(:), allocatable :: mask
        real, dimension(:), allocatable :: valid_qobs, valid_qsim

        !> Allocate the function variable.
        allocate(calc_abserr_value%value_gauge(size(qsim, 2)))
        calc_abserr_value%value_gauge = 0.0
        calc_abserr_value%value_gauge_avg = 0.0

        !> Only calculate if the current day is greater than the spin-up period.
        if (ncal_day > ncal_day_min) then

            !> Calculate the per gauge value.
            do j = 1, size(qsim, 2)

                !> Create a mask of valid values in the accounting for a spin-up period.
                allocate(mask(ncal_day - ncal_day_min + 1))
                do i = ncal_day_min, ncal_day
                    mask(i - ncal_day_min + 1) = (qobs(i, j) >= 0.0)
                end do
                n_valid_days = count(mask)

                !> Transfer and calculate the metric of only the valid period.
                if (n_valid_days > 0) then
                    allocate(valid_qobs(n_valid_days))
                    allocate(valid_qsim(n_valid_days))
                    valid_qobs = pack(qobs(ncal_day_min:ncal_day, j), mask)
                    valid_qsim = pack(qsim(ncal_day_min:ncal_day, j), mask)

                    !> Calculate the metric.
                    calc_abserr_value%value_gauge(j) = &
                        sum(abs(valid_qobs - valid_qsim))/n_valid_days
                    deallocate(valid_qobs)
                    deallocate(valid_qsim)
                end if
                deallocate(mask)
            end do

            !> Calculate the average value.
            calc_abserr_value%value_gauge_avg = &
                sum(calc_abserr_value%value_gauge)/size(calc_abserr_value%value_gauge)
        end if

    end function

end module calc_abserr
