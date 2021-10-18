!> Description:
!>  Module for date/time types and related routines.
module datetime_types

    implicit none

    !> Description:
    !>  Structure for storing date components.
    !>
    !> Variables:
    !*  year: Standard four-digit year.
    !*  month: Month of year (1-12).
    !*  day: Day of month.
    !*  jday: Day of year.
    !*  hour: Hour of day (0-23).
    !*  minutes: Minutes in hour (0-59).
    type io_datetime
        integer :: year = 0
        integer :: month = 0
        integer :: day = 0
        integer :: jday = 0
        integer :: hour = 0
        integer :: minutes = 0
    end type

end module
