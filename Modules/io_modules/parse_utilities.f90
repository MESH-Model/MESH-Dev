!> Description:
!>  Module that contains subroutines and functions for parsing lines
!>  read from simple text and/or CSV format files.
module parse_utilities

    !> 'strings': For 'compact', 'parse' and 'value' functions.
    use strings

    implicit none

    private assign_line_args_fval, assign_line_args_ival

    !> Description:
    !>  Assign the arguments provided from a string to the given field.
    !>
    !> Input/output variables:
    !*  field: Field of size 'nfield' (allocated), assigned values from 'args'.
    !*  nfield: Size of 'field'.
    !*  args: Values to assign to 'field'.
    !*  nargs: Size of 'args'.
    !*  ierr: Conversion/error status.
    interface assign_line_args_vector
        module procedure assign_line_args_fval
        module procedure assign_line_args_ival
    end interface

    !> Description:
    !>  Type for error keys (to be interpreted by called routines).
    !>
    !> Variables:
    !*  COUNT_MISMATCH: When the number of values in 'args' does not match the expected number.
    !*  BAD_ASSIGN: When an error occured converting the type of the variable to assign to the value.
    type error_keys
        integer :: COUNT_MISMATCH = 1
        integer :: BAD_ASSIGN = 2
        integer :: BAD_FORMAT = 3
        integer :: BAD_DATE_FORMAT = 4
        integer :: BAD_TIME_FORMAT = 5
    end type

    !* pserr: Instance of error keys.
    type(error_keys), save :: pserr

    contains

    subroutine assign_line_args_fval(field, nfield, args, nargs, ierr)

        !> strings: For 'value' function.
        use strings

        !> Input/output variables.
        integer, intent(in) :: nfield, nargs
        character(len = *), dimension(nargs), intent(in) :: args
        real, dimension(nfield) :: field
        integer ierr

        !> Local variables.
        integer i
        real fval

        !> Check dimensions.
        if (nargs < nfield) then
            ierr = pserr%COUNT_MISMATCH
            return
        end if

        !> Extract the fields.
        ierr = 0
        do i = 1, nfield
            call value(args(i), field(i), ierr)
            if (ierr /= 0) exit
        end do

        !> Check error status.
        if (ierr /= 0) ierr = pserr%BAD_ASSIGN

        return

    end subroutine

    subroutine assign_line_args_ival(field, nfield, args, nargs, ierr)

        !> Input/output variables.
        integer, intent(in) :: nfield, nargs
        character(len = *), dimension(nargs), intent(in) :: args
        integer, dimension(nfield) :: field
        integer ierr

        !> Local variables.
        real, dimension(nfield) :: fval

        !> Call subroutine for type real.
        call assign_line_args_fval(fval, nfield, args, nargs, ierr)

        !> Assign field.
        if (ierr == 0) field = int(fval)

        return

    end subroutine

    !> Description:
    !>  Parse the start time in the file header into individual components.
    !>
    !> Input:
    !*  datetime: Date/time structure to parse.
    !>
    !> Output:
    !*  year: Year component of date.
    !*  month: Month component of date.
    !*  day: Day in month component of date.
    !*  hour: Hour in day component of date.
    !*  minutes: Minutes in hour component of date.
    !*  seconds: Seconds in minutes component of date.
    !>
    !> Error return:
    !*  ierr: Return status (of 'error_keys' type value).
    subroutine parse_datetime(datetime, year, month, day, hour, minutes, seconds, ierr)

        !> strings: For 'compact', 'parse' and 'value' functions.
        !> print_routines: For 'DEFAULT_LINE_LENGTH' and 'DEFAULT_FIELD_LENGTH' constants.
        !> module_dates: For 'get_jday' and 'get_jdate' functions.
        use strings
        use print_routines
        use model_dates

        !> Input variables.
        character(len = *), intent(in) :: datetime

        !> Output variables.
        integer, intent(out) :: year, month, day, hour, minutes, seconds, ierr

        !> Local variables.
        integer nargs, j, h, m, n, i, z
        real(kind = 8) dt
        character(len = DEFAULT_FIELD_LENGTH), dimension(DEFAULT_LINE_LENGTH/DEFAULT_FIELD_LENGTH) :: args
        character(len = DEFAULT_LINE_LENGTH) line
        character(len = DEFAULT_FIELD_LENGTH) ctmp

        !> Initially set the values to zero.
        year = 0; month = 0; day = 0; hour = 0; minutes = 0; seconds = 0

        !> Initialize the return status.
        ierr = 0

        !> Replace forward-slash with dash.
        line = adjustl(datetime)
        call compact(line)
        do i = 1, len_trim(line)
            if (line(i:i) == '/') line(i:i) = '-'
        end do

        !> Parse date/time text.
        call parse(line, ' ', args, nargs)
        if (.not. nargs > 0) then
            ierr = pserr%BAD_FORMAT
            return
        end if

        !> Find start time in the list of attributes.
        do n = 1, nargs

            !> Scan for a date signature (e.g., 2003-01-31).
            ctmp = adjustl(args(n))
            z = 0
            if (index(ctmp, '-') > 1 .and. index(ctmp, '-') /= index(ctmp, '-', back = .true.)) then
                call value(ctmp(1:(index(ctmp, '-') - 1)), year, z)
                if (z == 0) call value(ctmp((index(ctmp, '-') + 1):(index(ctmp, '-', back = .true.) - 1)), month, z)
                if (z == 0) call value(ctmp((index(ctmp, '-', back = .true.) + 1):len(ctmp)), day, z)
            end if
            if (z /= 0) then
                ierr = pserr%BAD_DATE_FORMAT
                return
            end if

            !> Scan for a time signature (e.g., 24:00:00.000; 24:00:00; 24:00).
            ctmp = adjustl(args(n))
            z = 0
            if (index(ctmp, ':') > 1) then
                call value(ctmp(1:(index(ctmp, ':') - 1)), hour, z)
                if (index(ctmp, ':') /= index(ctmp, ':', back = .true.)) then
                    if (z == 0) call value(ctmp((index(ctmp, ':') + 1):(index(ctmp, ':', back = .true.) - 1)), minutes, z)
                    if (z == 0) call value(ctmp((index(ctmp, ':', back = .true.) + 1):len(ctmp)), seconds, z)
                else
                    if (z == 0) call value(ctmp((index(ctmp, ':') + 1):len(ctmp)), minutes, z)
                end if

                !> Check for time-zone signature (e.g., -6, -06, -530, -0530, -6:00, -06:00).
                if ((n + 1) <= nargs) then
                    ctmp = adjustl(args(n + 1))
                    h = 0
                    m = 0
                    if (index(ctmp, ':') > 1) then

                        !> Format contains ':'.
                        if (z == 0) call value(ctmp(1:(index(ctmp, ':') - 1)), h, z)
                        if (z == 0) call value(ctmp((index(ctmp, ':') + 1):len(ctmp)), m, z)
                    else
                        if (z == 0) call value(ctmp, h, z)
                        if (z == 0) then

                            !> Format is '-530' or '-0530' (or format is '-6' or '-06' and value is used as-is).
                            if (abs(h) > 99) then
                                m = h - floor(h/100.0)*100
                                h = floor(h/100.0)
                            end if
                        end if
                    end if

                    !> Calculate the adjustment using 'h' and 'm' to convert to UTC+00.
                    if (z == 0) then
                        j = get_jday(month, day, year)
                        dt = get_jdate(year, j)*24.0 + hour + minutes/60.0 + seconds/60.0/60.0
                        dt = dt - (h + m/60.0)
                        year = floor(dt/24.0/365.25) + 1601
                        j = floor(dt/24.0) - floor((year - 1601)*365.25)
                        call Julian2MonthDay(j, year, month, day)
                        hour = floor(dt) - get_jdate(year, j)*24
                        minutes = floor((dt - floor(dt))*60.0)
                        seconds = floor((dt - floor(dt))*60.0*60.0 - minutes*60.0)
                    end if
                end if
            end if
            if (z /= 0) then
                ierr = pserr%BAD_TIME_FORMAT
                return
            end if
        end do

        return

    end subroutine

end module
