module sa_mesh_utilities

    implicit none

    !> Flags.
    !* VERBOSEMODE: .true. to print messages to screen; .false. otherwise (default: .true.).
    !* DIAGNOSEMODE: .true. to print diagnostic and excess information to screen; .false. otherwise (default: .false.).
    !* ECHOTXTMODE: .true. to print message to summary file; .false. otherwise (default: .true.).
    logical :: VERBOSEMODE = .true.
    logical :: DIAGNOSEMODE = .false.
    logical :: ECHOTXTMODE = .true.

    !> File units.
    !* ECHO_SCN_IUN: Unit of screen (for print).
    !* ECHO_TXT_IUN: Unit of summary file (for write).
    integer, parameter :: ECHO_SCN_IUN = 6, ECHO_TXT_IUN = 58

    !> String constants.
    !* DEFAULT_LINE_LENGTH: Default length of a single line.
    !* DEFAULT_FIELD_LENGTH: Default length of a field (e.g., in a line).
    !* PAD_1: Padding of '1x'.
    !* PAD_3: Padding of '3x'.
    !* PAD_NONE: No padding.
    integer, parameter :: DEFAULT_LINE_LENGTH = 1000
    integer, parameter :: DEFAULT_FIELD_LENGTH = 20
    integer, parameter :: PAD_1 = 1
    integer, parameter :: PAD_3 = 3
    integer, parameter :: PAD_NONE = 0

    contains

    !> Description:
    !>  Returns a default character format statement provided a level.
    !>
    !> Variables:
    !*  level: Offset from the leading edge of the line (optional; default: 1x).
    !>
    !> Returns:
    !*  f: Character format statement.
    function get_format(level) result(f)

        !> Input variables (optional).
        integer, intent(in), optional :: level

        !> Output variables.
        character(len = DEFAULT_LINE_LENGTH) f

        !> Format statement based on 'level'.
        if (present(level)) then
            if (level == 0) then
                f = '((a))'
            else
                write(f, '(i4)') level
                f = '(' // trim(adjustl(f)) // 'x, (a))'
            end if
        else
            f = '(1x, (a))'
        end if

    end function

    !> Description:
    !>  Print the provided message to screen only.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line.
    subroutine print_screen(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Print to screen.
        if (VERBOSEMODE) write(ECHO_SCN_IUN, get_format(level)) trim(message)

    end subroutine

    !> Description:
    !>  Print the provided message to the summary file only.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line.
    subroutine print_echo_txt(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Print to the summary file.
        if (VERBOSEMODE .and. ECHOTXTMODE) write(ECHO_TXT_IUN, get_format(level)) trim(message)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file.
    !>  Level specifies an offset in spacing relative to the leading
    !>  edge of the line.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line.
    subroutine print_message(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Input variables (optional).
        integer, intent(in), optional :: level

        !> Print to screen.
        call print_screen(message, level)

        !> Print to the summary file.
        call print_echo_txt(message, level)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file.
    !>  Lead the message with "WARNING:".
    subroutine print_warning(message)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Flush the message.
        call print_message('WARNING: ' // trim(adjustl(message)))

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file.
    !>  Lead the message with "REMARK:".
    subroutine print_remark(message)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Flush the message.
        call print_message('REMARK: ' // trim(adjustl(message)))

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
    !>  Write an extra line before the message.
    subroutine print_error(message)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Flush the message.
        call print_message('')
        call print_message('ERROR: ' // trim(adjustl(message)))

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file
    !>  with an extra indentation.
    subroutine print_message_detail(message)

        !> Input variables.
        character(len = *), intent(in) :: message

        !> Flush the message.
        call print_message(message, PAD_3)

    end subroutine

    !> Description:
    !>  Open the summary file.
    !>
    !> Variables:
    !*  path: Full path to the file.
    subroutine open_summary_file(path)

        !> Input variables.
        character(len = *), intent(in) :: path

        !> Local variables.
        integer ierr

        !> Return if writing output to the file is disabled.
        !> Return if 'path' is empty.
        !> Return if 'VERBOSEMODE' is disabled.
        if (.not. ECHOTXTMODE .or. len_trim(path) == 0 .or. .not. VERBOSEMODE) return

        !> Open the file and print an error if unsuccessful.
        open(ECHO_TXT_IUN, file = path, status = 'replace', action = 'write', iostat = ierr)
        if (ierr /= 0) then
            call print_error('Unable to open file: ' // trim(adjustl(path)))
            call print_message('Check that the path exists, that the file it is not read-protected or open in another application.')
            call stop_program()
        end if

    end subroutine

end module
