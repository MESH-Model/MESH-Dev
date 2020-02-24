module print_routines

    implicit none

    !> Control flag constants.
    character(len = *), parameter :: DIAGNOSEMODE_NAME = 'DIAGNOSEMODE'
    character(len = *), parameter :: ECHOTXTMODE_NAME = 'ECHOTXTMODE'
    character(len = *), parameter :: PRINTSIMSTATUS_NAME = 'PRINTSIMSTATUS'

    !> Control flags.
    !* ISHEADNODE: .true. if node passes messages to output (default: .true.; .false. for worker nodes).
    !* DIAGNOSEMODE: .true. to print diagnostic and excess information (default: .false.).
    !* ECHOTXTMODE: .true. to echo screen output to the output file (default: .true.).
    logical :: ISHEADNODE = .true.
    logical :: DIAGNOSEMODE = .false.
    logical :: ECHOTXTMODE = .true.

    !> Flag constants.
    integer, parameter :: OUT_NONE = 0
    integer, parameter :: OUT_JDATE_DLY = 1
    integer, parameter :: OUT_JDATE_MLY = 2
    integer, parameter :: OUT_DATE_DLY = 3
    integer, parameter :: OUT_DATE_MLY = 4

    !> Options.
    !* PRINTSIMSTATUS: Flag to control how frequently the model prints simulation status messages to screen (default: OUT_DLY).
    integer :: PRINTSIMSTATUS = OUT_JDATE_DLY

    !> File units.
    !* ECHO_SCN_IUN: Unit of screen (for print).
    !* ECHO_TXT_IUN: Unit of summary file (for write).
    integer, parameter :: ECHO_SCN_IUN = 6, ECHO_TXT_IUN = 58

    !> Padding constants.
    !* PAD_1: Padding of '1x'.
    !* PAD_3: Padding of '3x'.
    !* PAD_NONE: No padding.
    integer, parameter :: PAD_1 = 1
    integer, parameter :: PAD_3 = 3
    integer, parameter :: PAD_NONE = 0

    !> Internal pad position.
    integer, private :: PAD_NOW = PAD_1

    !> Line and field length constants.
    !* DEFAULT_LINE_LENGTH: Default length of a single line.
    !* DEFAULT_FIELD_LENGTH: Default length of a field (e.g., in a line).
    integer, parameter :: DEFAULT_LINE_LENGTH = 1000
    integer, parameter :: DEFAULT_FIELD_LENGTH = 20

    !> Format constants.
    character(len = *), parameter :: FMT_GEN = '(99999(g15.6e2, 1x))'
    character(len = *), parameter :: FMT_CSV = "(99999(g15.6e2, ','))"

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
            if (level == PAD_NONE) then
                f = '((a))'
            else
                write(f, '(i4)') level
                f = '(' // trim(adjustl(f)) // 'x, (a))'
            end if
        else
            write(f, '(i4)') PAD_NOW
            f = '(' // trim(adjustl(f)) // 'x, (a))'
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
        if (ISHEADNODE) write(ECHO_SCN_IUN, get_format(level)) trim(message)

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
        if (ISHEADNODE .and. ECHOTXTMODE) write(ECHO_TXT_IUN, get_format(level)) trim(message)

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
        integer, intent(in), optional :: level

        !> Print to screen.
        call print_screen(message, level)

        !> Print to the summary file.
        call print_echo_txt(message, level)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file.
    !>  Lead the message with "WARNING:".
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line.
    subroutine print_warning(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Flush the message.
        call print_message('WARNING: ' // trim(adjustl(message)), level)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file.
    !>  Lead the message with "REMARK:".
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line.
    subroutine print_remark(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Flush the message.
        call print_message('REMARK: ' // trim(adjustl(message)), level)

    end subroutine

    !> Description:
    !>  Print the provided message to screen and to the summary file.
    !>  Lead the message with "ERROR: ".
    !>  Write an extra line before the message.
    !>
    !> Variables:
    !>  message: Message to output.
    !>  level: Offset from the leading edge of the line.
    subroutine print_error(message, level)

        !> Input variables.
        character(len = *), intent(in) :: message
        integer, intent(in), optional :: level

        !> Print a leading line if the indent level is not present.
        if (.not. present(level) .and. PAD_NOW == PAD_1) call print_message('')

        !> Flush the message.
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
    !>  Reset the starting position of message output.
    subroutine reset_tab()
        PAD_NOW = PAD_1
    end subroutine

    !> Description:
    !>  Increase the starting position of message output.
    subroutine increase_tab()
        PAD_NOW = PAD_3
    end subroutine

    !> Description:
    !>  Decrease the starting position of message output.
    subroutine decrease_tab()
        PAD_NOW = PAD_1
    end subroutine

    !> Description:
    !>  Open the summary file.
    !>
    !> Variables:
    !*  path: Full path to the file.
    subroutine open_echo_txt(path, ierr)

        !> Input variables.
        character(len = *), intent(in) :: path

        !> Output variables.
        integer, intent(out) :: ierr

        !> Initialize the return status.
        ierr = 0

        !> Return if writing output to the file is disabled.
        !> Return if 'path' is empty.
        !> Return if 'VERBOSEMODE' is disabled.
        if (.not. ECHOTXTMODE .or. len_trim(path) == 0 .or. .not. ISHEADNODE) return

        !> Open the file and print an error if unsuccessful.
        open(ECHO_TXT_IUN, file = path, status = 'replace', action = 'write', iostat = ierr)
        if (ierr /= 0) then

            !> Disable output to the file.
            ECHOTXTMODE = .false.

            !> Print an error (to screen).
            call print_error('Unable to open file: ' // trim(adjustl(path)))
            call print_message('Check that the path exists, that the file it is not read-protected or open in another application.')
            return
        end if

    end subroutine

    !> Description:
    !>  Update options for printing messages.
    !>
    !> Variables:
    !*  option: Option to update.
    !*  values: Vector of configuration options.
    subroutine parse_options(option, values)

        use strings

        !> Input variables.
        character(len = *), intent(in) :: option, values(:)

        !> Local variables.
        integer i

        !> Update options.
        select case(option)
            case (DIAGNOSEMODE_NAME)
                do i = 1, size(values)
                    DIAGNOSEMODE = (values(i) == '1' .or. lowercase(values(i)) == 'on')
                end do
            case (ECHOTXTMODE_NAME)
                do i = 1, size(values)
                    ECHOTXTMODE = (values(i) == '1' .or. lowercase(values(i)) == 'on')
                end do
            case (PRINTSIMSTATUS_NAME)
                do i = 1, size(values)
                    select case (lowercase(values(i)))
                        case ('monthly')
                            PRINTSIMSTATUS = OUT_JDATE_MLY
                        case ('date_daily')
                            PRINTSIMSTATUS = OUT_DATE_DLY
                        case ('date_monthly')
                            PRINTSIMSTATUS = OUT_DATE_MLY
                        case ('1', 'on', 'default')
                            PRINTSIMSTATUS = OUT_JDATE_DLY
                        case ('0', 'off')
                            PRINTSIMSTATUS = OUT_NONE
                    end select
                end do
        end select

    end subroutine

end module
