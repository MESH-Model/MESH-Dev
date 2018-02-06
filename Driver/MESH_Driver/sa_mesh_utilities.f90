module sa_mesh_utilities

    implicit none

    !> Flags.
    !*  VERBOSEMODE: .true. to print messages to screen; .false. otherwise (default: .true.).
    !*  DIAGNOSEMODE: .true. to print diagnostic and excess information to screen; .false. otherwise (default: .false.).
    !*  ECHOTXTMODE: .true. to print message to summary file; .false. otherwise (default: .true.).
    logical :: VERBOSEMODE = .true.
    logical :: DIAGNOSEMODE = .false.
    logical :: ECHOTXTMODE = .true.

    !> Constants.
    !*  ECHO_SCN_IUN: Unit of screen (for print).
    !*  ECHO_TXT_IUN: Unit of summary file (for write).
    integer, parameter :: ECHO_SCN_IUN = 6, ECHO_TXT_IUN = 58

    !> String constants.
    !*  DEFAULT_LINE_LENGTH: Default length of a single line.
    !*  DEFAULT_FIELD_LENGTH: Default length of a field (e.g., in a line).
    integer, parameter :: DEFAULT_LINE_LENGTH = 1000
    integer, parameter :: DEFAULT_FIELD_LENGTH = 20

    contains

    !> Description:
    !>  Print the provided message to screen and to the summary file.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  fmt: Format for output.
    subroutine print_message(message, fmt)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Input variables (optional).
        character(len = *), intent(in), optional :: fmt

        !> Local variables.
        character(len = DEFAULT_LINE_LENGTH) f

        !> Format statement.
        if (present(fmt)) then
            f = adjustl(fmt)
        else
            f = '(1x, (a))'
        end if

        !> Print to screen.
        if (VERBOSEMODE) write(ECHO_SCN_IUN, f) message

        !> Print to summary file.
        if (ECHOTXTMODE) write(ECHO_TXT_IUN, f) message

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file.
    !>  Lead the message with "WARNING :".
    subroutine print_warning(message)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Flush the message.
        call print_message(message, "(1x, 'WARNING: ', (a))")

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  only if 'DIAGNOSEMODE' is enabled.
    subroutine print_diagnostic_info(message)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Flush the message.
        if (DIAGNOSEMODE) call print_message(message)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file.
    !>  Lead the message with "ERROR: ".
    subroutine print_error(message)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Flush the message.
        call print_message(message, "(1x, 'ERROR: ', (a))")

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  with an extra indentation.
    subroutine print_message_detail(message)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Flush the message.
        call print_message(message, "(3x, (a))")

    end subroutine

end module
